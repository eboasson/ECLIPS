-- | Authorized current-command EORC conformance client. Every operation uses
-- the production framed TCP lane; the cluster handle supplies only the
-- independently observed authority required to admit terminal absence.
module Eclips.Oracle.Runtime.ConformanceClient
  ( ConformanceClientFault (..),
    ConformanceClient,
    newConformanceClient,
    addConformanceReplicaCluster,
    ConformanceRequest,
    prepareConformanceRequest,
    prepareConflictingConformanceRequest,
    conformanceRequestId,
    conformanceRequestFromCanonical,
    withConformanceProgress,
    withConformanceReceiptRetirement,
    withConformanceReceiptRetirementProgress,
    ConformanceRequestResult (..),
    ConformanceSubmissionDisposition (..),
    submitConformanceRequest,
    submitConformanceRequestVia,
    submitConformanceRequestDispositionVia,
    submitConformanceRequestWithoutReply,
    retireConformanceProgress,
    retireConformanceReceipts,
    retireConformanceReceiptProgress,
    queryConformanceRequest,
    ConformanceConflictResult (..),
    submitConflictingConformanceRequest,
    watchConformanceEntries,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    newTVarIO,
    stateTVar,
  )
import Control.Exception
  ( IOException,
    finally,
    onException,
    try,
  )
import Control.Monad
  ( guard,
  )
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    HeraldId,
    LabelDecisionId,
    controlIndex,
    controlIndexWord64,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Oracle.Canonical
  ( CanonicalOracleEnvelope,
    canonicalOracleEnvelopeDigest,
    canonicalOracleEnvelopeValue,
    canonicalizeOracleEnvelope,
  )
import Eclips.Oracle.Command
  ( OracleCommand,
    oracleEnvelope,
    oracleEnvelopeCommand,
    oracleEnvelopeExpectedControlIndex,
    oracleEnvelopeHomeHeraldEpoch,
    oracleEnvelopeRequestId,
    oracleEnvelopeWithProgress,
    oracleEnvelopeWithReceiptRetirementProgress,
    retireOracleProgressCommand,
  )
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    checkedOracleActiveHeralds,
    checkedOracleCatalogueDigest,
    checkedOracleConfigurationDigest,
    checkedOracleInitialProjectionDigest,
    checkedOracleRaftVoterBindings,
    checkedOracleSystemId,
    raftVoterBindingNode,
  )
import Eclips.Oracle.Identity
  ( OracleClientRequestId,
    oracleClientRequestId,
  )
import Eclips.Oracle.Progress (OracleProgress, oracleProgress, oracleProgressCovers, oracleProgressReceipts)
import Eclips.Oracle.Projection
  ( AppliedOracleEntry,
    appliedEntryControlIndex,
  )
import Eclips.Oracle.Receipt
  ( OracleReceipt,
    OracleRequestRetirement,
    oracleReceiptCommandDigest,
    oracleReceiptRequestId,
  )
import Eclips.Oracle.Runtime
  ( OracleRuntimeStatus,
    oracleRuntimeAppliedControlIndex,
    oracleRuntimeMembershipGeneration,
    oracleRuntimeNode,
    oracleRuntimeReplicaRegistrations,
    oracleRuntimeRole,
    oracleRuntimeServiceReady,
    oracleRuntimeTerm,
  )
import Eclips.Oracle.Runtime.Internal.TCP.Socket
  ( ResolvedTcpEndpoint (..),
    TcpClientConnection,
    closeTcpClientConnection,
    connectTcpClientEndpoint,
    receiveTcpClientRawFrame,
    sendTcpClientBytes,
  )
import Eclips.Oracle.Runtime.TCP
  ( OracleTcpCluster,
    OracleTcpEndpoint,
    oracleTcpEndpointHost,
    oracleTcpEndpointPort,
    oracleTcpNodeStatuses,
    oracleTcpOracleContacts,
  )
import Eclips.Oracle.Voter (replicaRegistrationNode)
import Eclips.Protocol.Oracle.Codec
  ( canonicalOracleEnvelopeDtoFromCore,
    canonicalOracleReceiptDtoValue,
    catalogueDigestClaimFromDomain,
    committedOracleEntriesDtoValues,
    configurationDigestClaimFromDomain,
    controlIndexDtoFromDomain,
    heraldEpochClaimFromDomain,
    heraldIdClaimFromDomain,
    heraldMembershipGenerationClaimFromDomain,
    initialProjectionDigestClaimFromDomain,
    labelDecisionIdClaimToDomain,
    oracleClientRequestIdDtoFromCore,
    oracleProgressDtoToCore,
    oracleRequestRetirementDtoToCore,
    raftNodeIdClaimFromCore,
    raftNodeIdClaimToCore,
    raftTermDtoToCore,
    systemIdClaimFromDomain,
  )
import Eclips.Protocol.Oracle.Frame
  ( OracleIngressAuthorityUpdateResult (..),
    OracleIngressContinuation (..),
    OracleIngressDecoder,
    OracleIngressFeedResult (..),
    encodeOracleFrame,
    feedOracleIngress,
    initialOracleIngressDecoder,
    oracleClientIngressContext,
    updateOracleClientIngressAuthority,
  )
import Eclips.Protocol.Oracle.Types
  ( OracleClientMessage (..),
    OracleHelloAcceptedDto,
    OracleProtocolEnvelope (..),
    OracleRedirectDto,
    OracleServerMessage (..),
    controlIndexDtoWord64,
    oracleHelloAcceptedCurrentTerm,
    oracleHelloAcceptedLocalAppliedControlIndex,
    oracleHelloAcceptedRaftNodeId,
    oracleHelloAcceptedServiceReady,
    oracleHelloDto,
    oracleRedirectLeaderHint,
    oracleServiceReadyLeaderContext,
  )
import Eclips.Protocol.Oracle.Types qualified as Protocol
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import Eclips.Raft.Identity
  ( RaftNodeId,
    RaftTerm,
  )
import Eclips.Raft.State (RaftRole (RaftLeader))

data ConformanceClientFault
  = ConformanceClientHeraldNotActive
  | ConformanceClientContactSetMismatch [RaftNodeId] [RaftNodeId]
  deriving stock (Eq, Show)

data ConformanceClient = ConformanceClient
  { clientGenesis :: CheckedOracleGenesis,
    clientHeraldId :: HeraldId,
    clientHeraldEpoch :: HeraldEpoch,
    clientContacts :: Map RaftNodeId OracleTcpEndpoint,
    clientClusters :: [OracleTcpCluster],
    clientNextRequest :: TVar Word64
  }

data ConformanceRequest = ConformanceRequest
  { requestId :: OracleClientRequestId,
    requestCanonical :: CanonicalOracleEnvelope
  }

-- | Add a separately supervised learner only after an existing replica has
-- applied its registration. The client still uses production EORC; handles
-- supply independent owner observations solely for terminal-absence admission.
addConformanceReplicaCluster :: ConformanceClient -> OracleTcpCluster -> IO (Either ConformanceClientFault ConformanceClient)
addConformanceReplicaCluster client cluster = do
  statuses <- concat <$> traverse oracleTcpNodeStatuses client.clientClusters
  let added = Map.fromList (oracleTcpOracleContacts cluster)
      known = [replicaRegistrationNode registration | (_, status) <- statuses, registration <- oracleRuntimeReplicaRegistrations status]
  pure
    $ if all (`elem` known) (Map.keys added)
      then Right client {clientContacts = client.clientContacts <> added, clientClusters = client.clientClusters <> [cluster]}
      else Left (ConformanceClientContactSetMismatch known (Map.keys added))

conformanceRequestId :: ConformanceRequest -> OracleClientRequestId
conformanceRequestId = requestId

-- | Replay or query an owner-authored request captured by a runtime fixture.
-- This does not allocate a request or change the supplied canonical envelope.
conformanceRequestFromCanonical :: CanonicalOracleEnvelope -> ConformanceRequest
conformanceRequestFromCanonical canonical =
  ConformanceRequest (oracleEnvelopeRequestId (canonicalOracleEnvelopeValue canonical)) canonical

-- | Add a release promise while preserving the request's semantic identity.
withConformanceProgress :: OracleProgress -> ConformanceRequest -> ConformanceRequest
withConformanceProgress progress request =
  request
    { requestCanonical = canonicalizeOracleEnvelope (oracleEnvelopeWithProgress progress (canonicalOracleEnvelopeValue request.requestCanonical))
    }

withConformanceReceiptRetirement :: Word64 -> ConformanceRequest -> ConformanceRequest
withConformanceReceiptRetirement = withConformanceReceiptRetirementProgress . Lifetime.receiptRetirementPrefix . Just

withConformanceReceiptRetirementProgress :: Lifetime.ReceiptRetirement -> ConformanceRequest -> ConformanceRequest
withConformanceReceiptRetirementProgress progress request =
  request
    { requestCanonical = canonicalizeOracleEnvelope (oracleEnvelopeWithReceiptRetirementProgress progress (canonicalOracleEnvelopeValue request.requestCanonical))
    }

data ConformanceRequestResult
  = ConformanceRequestFound OracleReceipt
  | ConformanceRequestAbsent ControlIndex
  | ConformanceRequestRetired OracleRequestRetirement
  deriving stock (Eq, Show)

data ConformanceSubmissionDisposition
  = ConformanceSubmissionReceipt OracleReceipt
  | ConformanceSubmissionDeferred LabelDecisionId ControlIndex
  | ConformanceSubmissionRetired OracleRequestRetirement
  deriving stock (Eq, Show)

data ConformanceConflictResult
  = ConformanceConflictConnectionClosed
  | ConformanceConflictSendUnknown
  deriving stock (Eq, Show)

data Lane = Lane
  { laneNode :: RaftNodeId,
    laneSocket :: TcpClientConnection,
    laneDecoder :: OracleIngressDecoder,
    laneHello :: OracleHelloAcceptedDto
  }

data LaneOpening
  = LaneOpened Lane
  | LaneRedirected (Maybe RaftNodeId)
  | LaneUnavailable

data ServerFrame
  = ServerMessage OracleServerMessage OracleIngressDecoder
  | ServerRedirect OracleRedirectDto
  | ServerFrameRejected

newConformanceClient ::
  CheckedOracleGenesis ->
  HeraldId ->
  HeraldEpoch ->
  OracleTcpCluster ->
  IO (Either ConformanceClientFault ConformanceClient)
newConformanceClient genesis heraldId epoch cluster = do
  let active =
        any
          (\member -> member.heraldMemberId == heraldId && member.heraldMemberEpoch == epoch)
          (checkedOracleActiveHeralds genesis)
      expected = fmap raftVoterBindingNode (checkedOracleRaftVoterBindings genesis)
      contacts = Map.fromList (oracleTcpOracleContacts cluster)
      actual = Map.keys contacts
  if not active
    then pure (Left ConformanceClientHeraldNotActive)
    else
      if actual /= expected
        then pure (Left (ConformanceClientContactSetMismatch expected actual))
        else do
          nextRequest <- newTVarIO 1
          pure
            ( Right
                ConformanceClient
                  { clientGenesis = genesis,
                    clientHeraldId = heraldId,
                    clientHeraldEpoch = epoch,
                    clientContacts = contacts,
                    clientClusters = [cluster],
                    clientNextRequest = nextRequest
                  }
            )

prepareConformanceRequest ::
  ConformanceClient ->
  Maybe ControlIndex ->
  OracleCommand ->
  IO ConformanceRequest
prepareConformanceRequest client expectedIndex command = do
  sequenceNumber <-
    atomically
      (stateTVar client.clientNextRequest (\current -> (current, current + 1)))
  let request = oracleClientRequestId client.clientHeraldEpoch sequenceNumber
      envelope =
        oracleEnvelope
          request
          expectedIndex
          client.clientHeraldEpoch
          command
  pure
    ConformanceRequest
      { requestId = request,
        requestCanonical = canonicalizeOracleEnvelope envelope
      }

prepareConflictingConformanceRequest ::
  ConformanceRequest ->
  OracleCommand ->
  Maybe ConformanceRequest
prepareConflictingConformanceRequest original replacement = do
  let envelope = canonicalOracleEnvelopeValue original.requestCanonical
  guard (oracleEnvelopeCommand envelope /= replacement)
  pure
    ConformanceRequest
      { requestId = original.requestId,
        requestCanonical =
          canonicalizeOracleEnvelope
            ( oracleEnvelope
                (oracleEnvelopeRequestId envelope)
                (oracleEnvelopeExpectedControlIndex envelope)
                (oracleEnvelopeHomeHeraldEpoch envelope)
                replacement
            )
      }

submitConformanceRequest :: ConformanceClient -> ConformanceRequest -> IO OracleReceipt
submitConformanceRequest client = submitStartingAt client Nothing

submitConformanceRequestVia ::
  ConformanceClient ->
  RaftNodeId ->
  ConformanceRequest ->
  IO (Maybe OracleReceipt)
submitConformanceRequestVia client firstContact request
  | Map.member firstContact client.clientContacts =
      Just <$> submitStartingAt client (Just firstContact) request
  | otherwise = pure Nothing

submitConformanceRequestDispositionVia ::
  ConformanceClient ->
  RaftNodeId ->
  ConformanceRequest ->
  IO (Maybe ConformanceSubmissionDisposition)
submitConformanceRequestDispositionVia client firstContact request
  | not (Map.member firstContact client.clientContacts) = pure Nothing
  | otherwise =
      Just <$> tryContacts client (Just firstContact) submitAt
  where
    submitAt node = do
      opening <- openLane client node zeroCursor
      case opening of
        LaneRedirected hint -> pure (RetryAt hint)
        LaneUnavailable -> pure RetryNext
        LaneOpened lane -> do
          attempted <- try @IOException (useLane lane (submitOnLane lane))
          pure (either (const RetryNext) id attempted)

    submitOnLane lane = do
      sendClient
        lane.laneSocket
        ( SubmitOracleCommand
            (canonicalOracleEnvelopeDtoFromCore request.requestCanonical)
        )
      awaitSubmission lane lane.laneDecoder

    awaitSubmission lane decoder = do
      response <- receiveServer decoder lane.laneSocket
      case response of
        ServerMessage (OracleReceipt dto) _ ->
          case canonicalOracleReceiptDtoValue dto of
            Right receipt
              | receiptMatches request receipt ->
                  pure (Finished (ConformanceSubmissionReceipt receipt))
            _ -> pure RetryNext
        ServerMessage (OracleRequestRetired requestDto reason) _
          | requestDto == oracleClientRequestIdDtoFromCore request.requestId ->
              pure (Finished (ConformanceSubmissionRetired (oracleRequestRetirementDtoToCore reason)))
        ServerMessage (OracleSubmissionDeferred requestDto decisionDto prefix) _
          | requestDto == oracleClientRequestIdDtoFromCore request.requestId ->
              case labelDecisionIdClaimToDomain decisionDto of
                Right decision ->
                  pure
                    ( Finished
                        ( ConformanceSubmissionDeferred
                            decision
                            (controlIndexFromDto prefix)
                        )
                    )
                Left _ -> pure RetryNext
        ServerMessage (OracleSubmissionNotReady requestDto _) successor
          | requestDto == oracleClientRequestIdDtoFromCore request.requestId -> do
              threadDelay 10_000
              sendClient
                lane.laneSocket
                ( SubmitOracleCommand
                    (canonicalOracleEnvelopeDtoFromCore request.requestCanonical)
                )
              awaitSubmission lane successor
        ServerRedirect redirect -> pure (RetryAt (redirectHint client redirect))
        _ -> pure RetryNext

submitStartingAt ::
  ConformanceClient ->
  Maybe RaftNodeId ->
  ConformanceRequest ->
  IO OracleReceipt
submitStartingAt client firstContact request =
  tryContacts client firstContact $ \node -> do
    opening <- openLane client node zeroCursor
    case opening of
      LaneRedirected hint -> pure (RetryAt hint)
      LaneUnavailable -> pure RetryNext
      LaneOpened lane -> do
        attempted <- try @IOException (useLane lane (submitOnLane lane))
        case attempted of
          Right outcome -> pure outcome
          Left _ -> recover
  where
    submitOnLane lane = do
      sendClient
        lane.laneSocket
        ( SubmitOracleCommand
            (canonicalOracleEnvelopeDtoFromCore request.requestCanonical)
        )
      awaitSubmission lane lane.laneDecoder

    awaitSubmission lane decoder = do
      response <- receiveServer decoder lane.laneSocket
      case response of
        ServerMessage (OracleReceipt dto) _ ->
          case canonicalOracleReceiptDtoValue dto of
            Right receipt
              | receiptMatches request receipt -> pure (Finished receipt)
            _ -> pure RetryNext
        ServerMessage (OracleSubmissionNotReady requestDto _) successor
          | requestDto == oracleClientRequestIdDtoFromCore request.requestId -> do
              threadDelay 10_000
              sendClient
                lane.laneSocket
                ( SubmitOracleCommand
                    (canonicalOracleEnvelopeDtoFromCore request.requestCanonical)
                )
              awaitSubmission lane successor
        ServerRedirect redirect -> pure (RetryAt (redirectHint client redirect))
        _ -> recover

    recover = do
      recovered <- queryConformanceRequest client request
      case recovered of
        ConformanceRequestFound receipt -> pure (Finished receipt)
        ConformanceRequestAbsent _ -> pure RetryNext
        ConformanceRequestRetired _ -> fail "the conformance caller retried a request after retiring its receipt"

submitConformanceRequestWithoutReply ::
  ConformanceClient ->
  RaftNodeId ->
  ConformanceRequest ->
  IO Bool
submitConformanceRequestWithoutReply client firstContact request
  | not (Map.member firstContact client.clientContacts) = pure False
  | otherwise =
      tryContacts client (Just firstContact) $ \node -> do
        opening <- openLane client node zeroCursor
        case opening of
          LaneRedirected hint -> pure (RetryAt hint)
          LaneUnavailable -> pure RetryNext
          LaneOpened lane -> do
            if oracleHelloAcceptedServiceReady lane.laneHello
              then do
                handed <-
                  try @IOException
                    ( ( sendClient
                          lane.laneSocket
                          ( SubmitOracleCommand
                              (canonicalOracleEnvelopeDtoFromCore request.requestCanonical)
                          )
                      )
                        `finally` closeTcpClientConnection lane.laneSocket
                    )
                pure (Finished (either (const False) (const True) handed))
              else do
                response <-
                  try @IOException $ useLane lane $ do
                    sendClient
                      lane.laneSocket
                      ( SubmitOracleCommand
                          (canonicalOracleEnvelopeDtoFromCore request.requestCanonical)
                      )
                    receiveServer lane.laneDecoder lane.laneSocket
                case response of
                  Right (ServerRedirect redirect) ->
                    pure (RetryAt (redirectHint client redirect))
                  Right (ServerMessage (OracleReceipt dto) _) ->
                    case canonicalOracleReceiptDtoValue dto of
                      Right receipt
                        | receiptMatches request receipt -> pure (Finished True)
                      _ -> pure (Finished False)
                  _ -> pure RetryNext

queryConformanceRequest ::
  ConformanceClient ->
  ConformanceRequest ->
  IO ConformanceRequestResult
queryConformanceRequest client request =
  tryContacts client Nothing $ \node -> do
    opening <- openLane client node zeroCursor
    case opening of
      LaneRedirected hint -> pure (RetryAt hint)
      LaneUnavailable -> pure RetryNext
      LaneOpened lane -> do
        attempted <- try @IOException $ useLane lane $ do
          sendClient
            lane.laneSocket
            (GetOracleRequestResult (oracleClientRequestIdDtoFromCore request.requestId))
          authoritative <- installLaneAuthority client lane
          response <- receiveServer authoritative lane.laneSocket
          case response of
            ServerMessage (OracleReceipt dto) _ ->
              case canonicalOracleReceiptDtoValue dto of
                Right receipt
                  | receiptMatches request receipt ->
                      pure (Finished (ConformanceRequestFound receipt))
                _ -> pure RetryNext
            ServerMessage (OracleRequestAbsent requestDto prefix) _
              | requestDto == oracleClientRequestIdDtoFromCore request.requestId ->
                  pure (Finished (ConformanceRequestAbsent (controlIndexFromDto prefix)))
            ServerMessage (OracleRequestRetired requestDto reason) _
              | requestDto == oracleClientRequestIdDtoFromCore request.requestId ->
                  pure (Finished (ConformanceRequestRetired (oracleRequestRetirementDtoToCore reason)))
            ServerRedirect redirect -> pure (RetryAt (redirectHint client redirect))
            _ -> pure RetryNext
        pure (either (const RetryNext) id attempted)

-- | Exercise the production maintenance lane without allocating another
-- ordinary request whose receipt would itself need acknowledgement. Callers
-- establish that their contiguous prefix no longer needs Oracle retry/query.
retireConformanceReceipts :: ConformanceClient -> Word64 -> IO Word64
retireConformanceReceipts client frontier =
  maybe 0 id . Lifetime.receiptRetirementHighWater <$> retireConformanceReceiptProgress client (Lifetime.receiptRetirementPrefix (Just frontier))

-- | Release every allocated identity outside the unresolved exception set.
retireConformanceReceiptProgress :: ConformanceClient -> Lifetime.ReceiptRetirement -> IO Lifetime.ReceiptRetirement
retireConformanceReceiptProgress client receipts =
  oracleProgressReceipts <$> retireConformanceProgress client (oracleProgress receipts zeroCursor)

-- | Announce both lifetime promises without allocating an ordinary request.
-- NotReady correlates the complete offer: its receipt high water alone can be
-- shared by multiple snapshots with different label or exception progress.
retireConformanceProgress :: ConformanceClient -> OracleProgress -> IO OracleProgress
retireConformanceProgress client progress =
  tryContacts client Nothing $ \node -> do
    opening <- openLane client node zeroCursor
    case opening of
      LaneRedirected hint -> pure (RetryAt hint)
      LaneUnavailable -> pure RetryNext
      LaneOpened lane -> do
        attempted <- try @IOException (useLane lane (submitOnLane lane))
        pure (either (const RetryNext) id attempted)
  where
    request = oracleClientRequestId client.clientHeraldEpoch (maybe 0 id (Lifetime.receiptRetirementHighWater (oracleProgressReceipts progress)))
    canonical = canonicalizeOracleEnvelope (oracleEnvelope request Nothing client.clientHeraldEpoch (retireOracleProgressCommand progress))
    submitOnLane lane = do
      sendClient lane.laneSocket (SubmitOracleCommand (canonicalOracleEnvelopeDtoFromCore canonical))
      awaitRetirement lane lane.laneDecoder
    awaitRetirement lane decoder = do
      response <- receiveServer decoder lane.laneSocket
      case response of
        ServerMessage (OracleProgressRetired requestDto effectiveDto) _
          | requestDto == oracleClientRequestIdDtoFromCore request,
            let effective = oracleProgressDtoToCore effectiveDto,
            oracleProgressCovers effective progress ->
              pure (Finished effective)
        ServerMessage (OracleProgressNotReady requestDto offered _) successor
          | requestDto == oracleClientRequestIdDtoFromCore request,
            oracleProgressDtoToCore offered == progress -> do
              threadDelay 10_000
              sendClient lane.laneSocket (SubmitOracleCommand (canonicalOracleEnvelopeDtoFromCore canonical))
              awaitRetirement lane successor
        ServerRedirect redirect -> pure (RetryAt (redirectHint client redirect))
        _ -> pure RetryNext

submitConflictingConformanceRequest ::
  ConformanceClient ->
  ConformanceRequest ->
  IO ConformanceConflictResult
submitConflictingConformanceRequest client request =
  tryContacts client Nothing $ \node -> do
    opening <- openLane client node zeroCursor
    case opening of
      LaneRedirected hint -> pure (RetryAt hint)
      LaneUnavailable -> pure RetryNext
      LaneOpened lane -> do
        response <-
          useLane lane $ do
            sent <-
              try @IOException
                ( sendClient
                    lane.laneSocket
                    ( SubmitOracleCommand
                        (canonicalOracleEnvelopeDtoFromCore request.requestCanonical)
                    )
                )
            case sent of
              Left _ -> pure (Left False)
              Right () -> do
                received <- try @IOException (receiveServer lane.laneDecoder lane.laneSocket)
                pure (either (const (Left True)) Right received)
        case response of
          Left False -> pure (Finished ConformanceConflictSendUnknown)
          Left True -> pure (Finished ConformanceConflictConnectionClosed)
          Right (ServerRedirect redirect) -> pure (RetryAt (redirectHint client redirect))
          Right _ -> pure (Finished ConformanceConflictSendUnknown)

watchConformanceEntries ::
  ConformanceClient ->
  ControlIndex ->
  IO (NonEmpty AppliedOracleEntry)
watchConformanceEntries client cursor =
  tryContacts client Nothing $ \node -> do
    opening <- openLane client node cursor
    case opening of
      LaneRedirected hint -> pure (RetryAt hint)
      LaneUnavailable -> pure RetryNext
      LaneOpened lane -> do
        attempted <- try @IOException $ useLane lane $ do
          sendClient lane.laneSocket (WatchOracle (controlIndexDtoFromDomain cursor))
          response <- receiveServer lane.laneDecoder lane.laneSocket
          case response of
            ServerMessage (CommittedOracleEntries dto) _ ->
              case committedOracleEntriesDtoValues dto of
                Right entries
                  | appliedEntryControlIndex (NonEmpty.head entries)
                      == controlIndex (controlIndexWord64 cursor + 1) ->
                      pure (Finished entries)
                Left _ -> pure RetryNext
                Right _ -> pure RetryNext
            ServerRedirect redirect -> pure (RetryAt (redirectHint client redirect))
            _ -> pure RetryNext
        pure (either (const RetryNext) id attempted)

data Attempt result
  = Finished result
  | RetryAt (Maybe RaftNodeId)
  | RetryNext

tryContacts ::
  ConformanceClient ->
  Maybe RaftNodeId ->
  (RaftNodeId -> IO (Attempt result)) ->
  IO result
tryContacts client firstHint attempt = loop (orderedContacts client firstHint)
  where
    loop [] = threadDelay 10000 >> loop (Map.keys client.clientContacts)
    loop (node : remaining) = do
      outcome <- attempt node
      case outcome of
        Finished result -> pure result
        RetryAt (Just hint) -> loop (orderedContacts client (Just hint))
        RetryAt Nothing -> loop remaining
        RetryNext -> loop remaining

orderedContacts :: ConformanceClient -> Maybe RaftNodeId -> [RaftNodeId]
orderedContacts client hint =
  case hint of
    Just node
      | Map.member node client.clientContacts ->
          node : filter (/= node) (Map.keys client.clientContacts)
    _ -> Map.keys client.clientContacts

openLane :: ConformanceClient -> RaftNodeId -> ControlIndex -> IO LaneOpening
openLane client node cursor = do
  statuses <- concat <$> traverse oracleTcpNodeStatuses client.clientClusters
  case (Map.lookup node client.clientContacts, lookup node statuses) of
    (Just endpoint, Just status) -> do
      connected <- try @IOException (connectTcpClientEndpoint (resolved endpoint))
      case connected of
        Left _ -> pure LaneUnavailable
        Right connection -> do
          opened <-
            ( try @IOException $ do
                sendClient
                  connection
                  ( OracleHello
                      ( helloDto
                          client
                          (oracleRuntimeMembershipGeneration status)
                          cursor
                      )
                  )
                response <- receiveServer (initialOracleIngressDecoder oracleClientIngressContext) connection
                case response of
                  ServerMessage (OracleHelloAccepted accepted) decoder
                    | raftNodeIdClaimFromCore node == oracleHelloAcceptedRaftNodeId accepted ->
                        pure
                          ( LaneOpened
                              Lane
                                { laneNode = node,
                                  laneSocket = connection,
                                  laneDecoder = decoder,
                                  laneHello = accepted
                                }
                          )
                  ServerRedirect redirect ->
                    pure (LaneRedirected (redirectHint client redirect))
                  _ -> pure LaneUnavailable
            )
              `onException` closeTcpClientConnection connection
          case opened of
            Left _ -> closeTcpClientConnection connection >> pure LaneUnavailable
            Right result -> case result of
              LaneOpened _ -> pure result
              _ -> closeTcpClientConnection connection >> pure result
    _ -> pure LaneUnavailable

useLane :: Lane -> IO result -> IO result
useLane lane action = action `finally` closeTcpClientConnection lane.laneSocket

installLaneAuthority :: ConformanceClient -> Lane -> IO OracleIngressDecoder
installLaneAuthority client lane = do
  statuses <- concat <$> traverse oracleTcpNodeStatuses client.clientClusters
  let exact = lookup lane.laneNode statuses >>= authorityStatus lane.laneHello
  pure $ case exact of
    Nothing -> lane.laneDecoder
    Just status ->
      case updateOracleClientIngressAuthority
        ( oracleServiceReadyLeaderContext
            (raftNodeIdClaimFromCore (oracleRuntimeNode status))
            (oracleHelloAcceptedCurrentTerm lane.laneHello)
            (controlIndexDtoFromDomain (oracleRuntimeAppliedControlIndex status))
        )
        lane.laneDecoder of
        OracleIngressAuthorityUpdated decoder -> decoder
        OracleIngressAuthorityUpdateRejected _ decoder -> decoder

authorityStatus :: OracleHelloAcceptedDto -> OracleRuntimeStatus -> Maybe OracleRuntimeStatus
authorityStatus accepted status = do
  guard (oracleRuntimeRole status == RaftLeader)
  guard (oracleRuntimeServiceReady status)
  guard (raftNodeIdClaimFromCore (oracleRuntimeNode status) == oracleHelloAcceptedRaftNodeId accepted)
  guard (oracleRuntimeTerm status == termFromHello accepted)
  guard
    ( controlIndexDtoFromDomain (oracleRuntimeAppliedControlIndex status)
        == oracleHelloAcceptedLocalAppliedControlIndex accepted
    )
  pure status

receiveServer :: OracleIngressDecoder -> TcpClientConnection -> IO ServerFrame
receiveServer decoder connection = do
  raw <- receiveTcpClientRawFrame connection
  pure $ case feedOracleIngress decoder raw of
    OracleIngressFeedResult [OracleServerEnvelope message] (NeedOracleIngressBytes successor) ->
      ServerMessage message successor
    OracleIngressFeedResult [OracleServerEnvelope (OracleRedirect redirect)] (OracleIngressRedirected _) ->
      ServerRedirect redirect
    _ -> ServerFrameRejected

sendClient :: TcpClientConnection -> OracleClientMessage -> IO ()
sendClient connection message =
  sendTcpClientBytes connection (encodeOracleFrame (OracleClientEnvelope message))

helloDto ::
  ConformanceClient ->
  HeraldMembershipGeneration ->
  ControlIndex ->
  Protocol.OracleHelloDto
helloDto client membership cursor =
  oracleHelloDto
    (systemIdClaimFromDomain (checkedOracleSystemId client.clientGenesis))
    (catalogueDigestClaimFromDomain (checkedOracleCatalogueDigest client.clientGenesis))
    (configurationDigestClaimFromDomain (checkedOracleConfigurationDigest client.clientGenesis))
    ( initialProjectionDigestClaimFromDomain
        (checkedOracleInitialProjectionDigest client.clientGenesis)
    )
    (heraldIdClaimFromDomain client.clientHeraldId)
    (heraldEpochClaimFromDomain client.clientHeraldEpoch)
    (controlIndexDtoFromDomain cursor)
    ( heraldMembershipGenerationClaimFromDomain
        (heraldMembershipGenerationId membership)
    )

redirectHint :: ConformanceClient -> OracleRedirectDto -> Maybe RaftNodeId
redirectHint client redirect = do
  claim <- oracleRedirectLeaderHint redirect
  node <- either (const Nothing) Just (raftNodeIdClaimToCore claim)
  guard (Map.member node client.clientContacts)
  pure node

receiptMatches :: ConformanceRequest -> OracleReceipt -> Bool
receiptMatches request receipt =
  oracleReceiptRequestId receipt == request.requestId
    && oracleReceiptCommandDigest receipt
      == canonicalOracleEnvelopeDigest request.requestCanonical

resolved :: OracleTcpEndpoint -> ResolvedTcpEndpoint
resolved endpoint =
  ResolvedTcpEndpoint
    (oracleTcpEndpointHost endpoint)
    (oracleTcpEndpointPort endpoint)

zeroCursor :: ControlIndex
zeroCursor = controlIndexFromWord64 0

controlIndexFromWord64 :: Word64 -> ControlIndex
controlIndexFromWord64 = controlIndex

controlIndexFromDto :: Protocol.ControlIndexDto -> ControlIndex
controlIndexFromDto = controlIndexFromWord64 . controlIndexDtoWord64

termFromHello :: OracleHelloAcceptedDto -> RaftTerm
termFromHello = raftTermDtoToCore . oracleHelloAcceptedCurrentTerm
