-- | Production EORC server lane. Semantic admission remains in the Oracle
-- owner; this worker owns framing, physical Herald binding, and reply routing.
module Eclips.Oracle.Runtime.Internal.TCP.Oracle
  ( handleOracleTcpConnection,
  )
where

import Control.Concurrent.MVar
  ( MVar,
    newMVar,
    withMVar,
  )
import Control.Exception
  ( IOException,
    finally,
    mask,
    try,
  )
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGenerationId,
    HeraldMembershipHistory,
  )
import Eclips.Oracle.Admission
  ( HeraldAdmissionManifest,
  )
import Eclips.Oracle.Canonical (canonicalOracleEnvelopeValue)
import Eclips.Oracle.Command
  ( OracleEnvelope,
    oracleCommandProgress,
    oracleEnvelopeCommand,
    oracleEnvelopeHomeHeraldEpoch,
    oracleEnvelopeRequestId,
  )
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    checkedOracleCatalogueDigest,
    checkedOracleConfigurationDigest,
    checkedOracleInitialProjectionDigest,
    checkedOracleSystemId,
  )
import Eclips.Oracle.Identity (oracleClientRequestHome)
import Eclips.Oracle.Runtime.Internal.ConnectedWait
  ( ConnectedWaitOutcome (..),
    awaitWhilePeerConnected,
  )
import Eclips.Oracle.Runtime.Internal.Connection
  ( awaitOracleRuntimeHelloStatus,
    queryOracleRuntimeHealth,
    readOracleRuntimeStatus,
  )
import Eclips.Oracle.Runtime.Internal.Coordination
  ( queryPublishedOracleRequest,
    submitCanonicalOracle,
    watchPublishedOracleEntries,
  )
import Eclips.Oracle.Runtime.Internal.Hello
  ( heraldLaneGenerationAuthorized,
    heraldLaneIdentityAuthorized,
  )
import Eclips.Oracle.Runtime.Internal.Scope (signalRuntimeFailure)
import Eclips.Oracle.Runtime.Internal.TCP.Retirement
  ( retireSocketQuietly,
  )
import Eclips.Oracle.Runtime.Internal.TCP.Socket
  ( receiveRawFrame,
    sendSocketBytes,
  )
import Eclips.Oracle.Runtime.Internal.Types
  ( OracleRequestResult (..),
    OracleRuntime (..),
    OracleRuntimeConfiguration (configuredOracleGenesis),
    OracleRuntimeFailure (RuntimeWatchEncodingInvariantFault),
    OracleRuntimeHealth (..),
    OracleRuntimeStatus (..),
    OracleSubmitResult (..),
    OracleWatchResult (..),
  )
import Eclips.Oracle.Runtime.Internal.WatchServe
  ( OracleWatchTermination (OracleWatchRuntimeUnavailable),
    prepareOracleWatchEntries,
    terminateOracleWatchLane,
  )
import Eclips.Oracle.Runtime.Internal.WatchWorker
  ( WatchWorkerOwner,
    newWatchWorkerOwnerIO,
    startWatchWorkerOrReject,
    stopWatchWorkerOwner,
  )
import Eclips.Protocol.Oracle.Codec
  ( canonicalOracleEnvelopeDtoToCore,
    canonicalOracleReceiptDtoFromValue,
    catalogueDigestClaimFromDomain,
    configurationDigestClaimFromDomain,
    controlIndexDtoFromDomain,
    controlIndexDtoToDomain,
    heraldEpochClaimToDomain,
    heraldIdClaimToDomain,
    heraldMembershipGenerationClaimToDomain,
    initialProjectionDigestClaimFromDomain,
    labelDecisionIdClaimFromDomain,
    oracleClientRequestIdDtoFromCore,
    oracleClientRequestIdDtoToCore,
    oracleProgressDtoFromCore,
    oracleRequestRetirementDtoFromCore,
    raftNodeIdClaimFromCore,
    raftTermDtoFromCore,
    systemIdClaimFromDomain,
  )
import Eclips.Protocol.Oracle.Frame
  ( OracleIngressContinuation (..),
    OracleIngressDecoder,
    OracleIngressFeedResult (..),
    encodeOracleFrame,
    feedOracleIngress,
    initialOracleIngressDecoder,
    oracleServerIngressContext,
  )
import Eclips.Protocol.Oracle.Types
  ( OracleClientMessage (..),
    OracleHelloAcceptedDto,
    OracleHelloContext,
    OracleHelloDto,
    OracleProtocolEnvelope (..),
    OracleServerMessage,
    oracleHelloAcceptedDto,
    oracleHelloContext,
    oracleHelloHeraldEpoch,
    oracleHelloHeraldId,
    oracleHelloMembershipGeneration,
    oracleRedirectDto,
  )
import Eclips.Protocol.Oracle.Types qualified as Protocol
import Eclips.Raft.Identity
  ( RaftNodeId,
    RaftTerm,
  )
import Eclips.Raft.State (RaftRole (RaftLeader))
import Network.Socket (Socket)

data OracleTcpConnection = OracleTcpConnection
  { socket :: Socket,
    writeLock :: MVar (),
    watchWorkerOwner :: WatchWorkerOwner
  }

handleOracleTcpConnection :: OracleRuntime -> Socket -> IO ()
handleOracleTcpConnection runtime socket = mask $ \restore -> do
  writeLock <- newMVar ()
  watchWorkerOwner <- newWatchWorkerOwnerIO writeLock
  let connection = OracleTcpConnection socket writeLock watchWorkerOwner
  restore (runConnection connection)
    `finally` stopWatchWorkerOwner connection.watchWorkerOwner
  where
    runConnection connection = do
      let genesis = runtime.runtimeConfiguration.configuredOracleGenesis
          decoder = initialOracleIngressDecoder (oracleServerIngressContext (helloContext genesis))
      first <- receiveRawFrame connection.socket
      case admitOne decoder first of
        Just (OracleClientEnvelope (OracleHealthQuery roundNumber hello), _) -> do
          status <- readOracleRuntimeStatus runtime
          observed <-
            if healthSourceAuthorized status hello
              then queryOracleRuntimeHealth runtime
              else pure Nothing
          case observed of
            Nothing -> pure ()
            Just (OracleRuntimeHealth node term generation reference configuration) ->
              sendServer
                connection
                ( Protocol.OracleHealthReply
                    ( Protocol.oracleHealthReplyDto
                        roundNumber
                        (raftNodeIdClaimFromCore node)
                        (raftTermDtoFromCore term)
                        generation
                        reference
                        configuration
                    )
                )
        Just (OracleClientEnvelope (OracleHello hello), established) ->
          do
            outcome <-
              awaitWhilePeerConnected
                (awaitOracleRuntimeHelloStatus runtime)
                connection.socket
            case outcome of
              ConnectedStateChanged (Just status) ->
                case authorizeHerald status.statusHeraldCatalogue status.statusMembershipHistory hello of
                  Nothing -> pure ()
                  Just (acceptedEpoch, acceptedGeneration) -> do
                    let accepted = helloAccepted status
                    sendServer connection (Protocol.OracleHelloAccepted accepted)
                    establishedLoop
                      connection
                      acceptedEpoch
                      acceptedGeneration
                      accepted
                      established
              ConnectedStateChanged Nothing -> pure ()
              ConnectedPeerEnded -> pure ()
        _ -> pure ()

    establishedLoop connection acceptedEpoch acceptedGeneration acceptedHello decoder = do
      raw <- receiveRawFrame connection.socket
      case admitOne decoder raw of
        Just (OracleClientEnvelope message, successor) ->
          handleMessage
            connection
            acceptedEpoch
            acceptedGeneration
            acceptedHello
            successor
            message
        _ -> pure ()

    handleMessage connection acceptedEpoch acceptedGeneration acceptedHello successor = \case
      OracleHello _ -> pure ()
      OracleHealthQuery {} -> pure ()
      SubmitOracleCommand dto ->
        case canonicalOracleEnvelopeDtoToCore dto of
          Left _ -> pure ()
          Right canonical -> do
            let envelopeAuthorized =
                  envelopeMatches acceptedEpoch (canonicalOracleEnvelopeValue canonical)
            if not envelopeAuthorized
              then pure ()
              else do
                current <- readOracleRuntimeStatus runtime
                let laneAuthorized =
                      laneStillAuthorized acceptedEpoch acceptedGeneration current
                if laneAuthorized
                  then do
                    result <- submitCanonicalOracle runtime canonical
                    case result of
                      OracleSubmitReceipt receipt -> do
                        sendServer connection (Protocol.OracleReceipt (canonicalOracleReceiptDtoFromValue receipt))
                        establishedLoop connection acceptedEpoch acceptedGeneration acceptedHello successor
                      OracleSubmitProgressRetired _ frontier -> do
                        sendServer
                          connection
                          ( Protocol.OracleProgressRetired
                              (oracleClientRequestIdDtoFromCore (oracleEnvelopeRequestId (canonicalOracleEnvelopeValue canonical)))
                              (oracleProgressDtoFromCore frontier)
                          )
                        establishedLoop connection acceptedEpoch acceptedGeneration acceptedHello successor
                      OracleSubmitRetired reason -> do
                        sendServer
                          connection
                          ( Protocol.OracleRequestRetired
                              (oracleClientRequestIdDtoFromCore (oracleEnvelopeRequestId (canonicalOracleEnvelopeValue canonical)))
                              (oracleRequestRetirementDtoFromCore reason)
                          )
                        establishedLoop connection acceptedEpoch acceptedGeneration acceptedHello successor
                      OracleSubmitDeferred deferredRequest decision observedIndex -> do
                        sendServer
                          connection
                          ( Protocol.OracleSubmissionDeferred
                              (oracleClientRequestIdDtoFromCore deferredRequest)
                              (labelDecisionIdClaimFromDomain decision)
                              (controlIndexDtoFromDomain observedIndex)
                          )
                        establishedLoop connection acceptedEpoch acceptedGeneration acceptedHello successor
                      OracleSubmitRedirect hint term -> sendRedirect connection hint term
                      OracleSubmitNotReady term -> do
                        let request = oracleClientRequestIdDtoFromCore (oracleEnvelopeRequestId (canonicalOracleEnvelopeValue canonical))
                            response =
                              case oracleCommandProgress (oracleEnvelopeCommand (canonicalOracleEnvelopeValue canonical)) of
                                Just progress -> Protocol.OracleProgressNotReady request (oracleProgressDtoFromCore progress) (raftTermDtoFromCore term)
                                Nothing -> Protocol.OracleSubmissionNotReady request (raftTermDtoFromCore term)
                        sendServer
                          connection
                          response
                        establishedLoop connection acceptedEpoch acceptedGeneration acceptedHello successor
                      OracleSubmitConflict _ -> pure ()
                      OracleSubmitUnavailable -> pure ()
                  else pure ()
      WatchOracle cursorDto -> do
        current <- readOracleRuntimeStatus runtime
        if laneStillAuthorized acceptedEpoch acceptedGeneration current
          then do
            let cursor = controlIndexDtoToDomain cursorDto
            started <-
              startWatchWorkerOrReject
                connection.watchWorkerOwner
                (retireSocketQuietly connection.socket)
                (runWatchWorker connection cursor)
            case started of
              Just _ -> establishedLoop connection acceptedEpoch acceptedGeneration acceptedHello successor
              Nothing -> pure ()
          else pure ()
      GetOracleRequestResult requestDto ->
        case oracleClientRequestIdDtoToCore requestDto of
          Left _ -> pure ()
          Right request
            | oracleClientRequestHome request /= acceptedEpoch -> pure ()
            | otherwise -> do
                current <- readOracleRuntimeStatus runtime
                if laneStillAuthorized acceptedEpoch acceptedGeneration current
                  then do
                    result <- queryPublishedOracleRequest runtime request
                    case result of
                      OracleRequestFound receipt -> do
                        sendServer connection (Protocol.OracleReceipt (canonicalOracleReceiptDtoFromValue receipt))
                        establishedLoop connection acceptedEpoch acceptedGeneration acceptedHello successor
                      OracleRequestRetired reason -> do
                        sendServer connection (Protocol.OracleRequestRetired requestDto (oracleRequestRetirementDtoFromCore reason))
                        establishedLoop connection acceptedEpoch acceptedGeneration acceptedHello successor
                      OracleRequestAbsent prefix -> do
                        latest <- readOracleRuntimeStatus runtime
                        if laneStillAuthorized acceptedEpoch acceptedGeneration latest
                          && absenceStillAuthoritative acceptedHello latest prefix
                          then do
                            sendServer
                              connection
                              (Protocol.OracleRequestAbsent requestDto (controlIndexDtoFromDomain prefix))
                            establishedLoop connection acceptedEpoch acceptedGeneration acceptedHello successor
                          else sendRedirect connection latest.statusLeaderHint latest.statusTerm
                      OracleRequestRedirect hint term -> sendRedirect connection hint term
                      OracleRequestNotReady term -> do
                        latest <- readOracleRuntimeStatus runtime
                        sendRedirect connection latest.statusLeaderHint term
                      OracleRequestUnavailable -> pure ()
                  else pure ()

    serveWatch connection cursor responsePublished = do
      result <- watchPublishedOracleEntries runtime cursor
      case result of
        OracleWatchEntries entries ->
          case prepareOracleWatchEntries cursor entries of
            Left termination -> terminate termination
            Right dto ->
              sendWatchServer connection responsePublished (Protocol.CommittedOracleEntries dto)
        OracleWatchRedirect hint term -> sendWatchRedirect connection responsePublished hint term
        OracleWatchNotReady term -> do
          current <- readOracleRuntimeStatus runtime
          sendWatchRedirect connection responsePublished current.statusLeaderHint term
        OracleWatchCursorAhead _ -> do
          current <- readOracleRuntimeStatus runtime
          sendWatchRedirect connection responsePublished current.statusLeaderHint current.statusTerm
        OracleWatchUnavailable -> terminate OracleWatchRuntimeUnavailable
      where
        terminate =
          terminateOracleWatchLane
            ( \problem ->
                signalRuntimeFailure
                  runtime.runtimeScope
                  (RuntimeWatchEncodingInvariantFault cursor problem)
            )
            connection.socket

    runWatchWorker connection cursor responsePublished = do
      outcome <- try @IOException (serveWatch connection cursor responsePublished)
      case outcome of
        Left _ -> retireSocketQuietly connection.socket
        Right () -> pure ()

helloContext :: CheckedOracleGenesis -> OracleHelloContext
helloContext genesis =
  oracleHelloContext
    (systemIdClaimFromDomain (checkedOracleSystemId genesis))
    (catalogueDigestClaimFromDomain (checkedOracleCatalogueDigest genesis))
    (configurationDigestClaimFromDomain (checkedOracleConfigurationDigest genesis))
    ( initialProjectionDigestClaimFromDomain
        (checkedOracleInitialProjectionDigest genesis)
    )

authorizeHerald ::
  [HeraldAdmissionManifest] ->
  HeraldMembershipHistory ->
  OracleHelloDto ->
  Maybe (HeraldEpoch, HeraldMembershipGenerationId)
authorizeHerald catalogue membership hello = do
  heraldId <- either (const Nothing) Just (heraldIdClaimToDomain (oracleHelloHeraldId hello))
  epoch <- either (const Nothing) Just (heraldEpochClaimToDomain (oracleHelloHeraldEpoch hello))
  generation <-
    either
      (const Nothing)
      Just
      (heraldMembershipGenerationClaimToDomain (oracleHelloMembershipGeneration hello))
  guard (heraldLaneIdentityAuthorized heraldId epoch generation catalogue membership)
  pure (epoch, generation)

-- Health bypasses ordinary leader/readiness and caller-history gates, but a
-- committed retired epoch must not renew its isolation grace after missing its
-- final watch entry. These are Oracle-owner-published membership facts; this
-- worker never reads the kernel state. A racing retirement can permit at most
-- one already-authorized bounded round before later queries are refused.
healthSourceAuthorized :: OracleRuntimeStatus -> OracleHelloDto -> Bool
healthSourceAuthorized status hello = case identity of
  Nothing -> False
  Just (herald, epoch) ->
    status.statusAccepting && Map.lookup epoch status.statusActiveHeralds == Just herald
  where
    identity = (,) <$> either (const Nothing) Just (heraldIdClaimToDomain (oracleHelloHeraldId hello)) <*> either (const Nothing) Just (heraldEpochClaimToDomain (oracleHelloHeraldEpoch hello))

laneStillAuthorized ::
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  OracleRuntimeStatus ->
  Bool
laneStillAuthorized acceptedEpoch acceptedGeneration status =
  status.statusAccepting
    && heraldLaneGenerationAuthorized
      acceptedEpoch
      acceptedGeneration
      status.statusMembershipHistory

envelopeMatches :: HeraldEpoch -> OracleEnvelope -> Bool
envelopeMatches acceptedEpoch envelope =
  oracleClientRequestHome (oracleEnvelopeRequestId envelope) == acceptedEpoch
    && oracleEnvelopeHomeHeraldEpoch envelope == acceptedEpoch

helloAccepted :: OracleRuntimeStatus -> OracleHelloAcceptedDto
helloAccepted status =
  oracleHelloAcceptedDto
    (raftNodeIdClaimFromCore status.statusNode)
    (raftTermDtoFromCore status.statusTerm)
    (controlIndexDtoFromDomain status.statusAppliedControlIndex)
    (fmap raftNodeIdClaimFromCore status.statusLeaderHint)
    status.statusServiceReady

absenceStillAuthoritative ::
  OracleHelloAcceptedDto ->
  OracleRuntimeStatus ->
  ControlIndex ->
  Bool
absenceStillAuthoritative accepted current prefix =
  current.statusAccepting
    && current.statusRole == RaftLeader
    && current.statusServiceReady
    && helloAccepted current == accepted
    && current.statusAppliedControlIndex == prefix

sendRedirect :: OracleTcpConnection -> Maybe RaftNodeId -> RaftTerm -> IO ()
sendRedirect connection hint term =
  sendServer
    connection
    ( Protocol.OracleRedirect
        (oracleRedirectDto (fmap raftNodeIdClaimFromCore hint) (raftTermDtoFromCore term))
    )

sendWatchRedirect :: OracleTcpConnection -> IO () -> Maybe RaftNodeId -> RaftTerm -> IO ()
sendWatchRedirect connection responsePublished hint term =
  sendWatchServer
    connection
    responsePublished
    ( Protocol.OracleRedirect
        (oracleRedirectDto (fmap raftNodeIdClaimFromCore hint) (raftTermDtoFromCore term))
    )

sendServer :: OracleTcpConnection -> OracleServerMessage -> IO ()
sendServer connection message =
  withMVar connection.writeLock $ \() ->
    sendSocketBytes
      connection.socket
      (encodeOracleFrame (OracleServerEnvelope message))

-- The completion marker shares the write lock with publication. A subsequent
-- Watch cannot observe the old worker as outstanding after its response has
-- become visible on the wire merely because the worker's finalizer has not yet
-- been scheduled.
sendWatchServer :: OracleTcpConnection -> IO () -> OracleServerMessage -> IO ()
sendWatchServer connection responsePublished message =
  withMVar connection.writeLock $ \() -> do
    sendSocketBytes
      connection.socket
      (encodeOracleFrame (OracleServerEnvelope message))
    responsePublished

admitOne ::
  OracleIngressDecoder ->
  ByteString ->
  Maybe (OracleProtocolEnvelope, OracleIngressDecoder)
admitOne decoder raw =
  case feedOracleIngress decoder raw of
    OracleIngressFeedResult [envelope] (NeedOracleIngressBytes successor) ->
      Just (envelope, successor)
    _ -> Nothing
