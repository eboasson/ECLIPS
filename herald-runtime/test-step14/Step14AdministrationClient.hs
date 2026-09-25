-- | Manual EADM client used by the Step-14 real-transport schedules.
--
-- Fixture identities deliberately stay with the schedule. This module owns
-- only the physical lane, the authorized handshake, incremental server-frame
-- decoding, and causal correlation checks.
module Step14AdministrationClient
  ( Step14AdministrationClient,
    Step14AdministrationReceiveFailure (..),
    Step14AdministrationReceive (..),
    withStep14AdministrationClient,
    connectStep14AdministrationClient,
    closeStep14AdministrationClient,
    sendStep14AdministrationRequest,
    sendStep14AdministrationRequestAndClose,
    receiveStep14AdministrationWithin,
    receiveStep14AdministrationForWithin,
  )
where

import Control.Concurrent.MVar
  ( MVar,
    modifyMVar,
    modifyMVar_,
    newMVar,
    withMVar,
  )
import Control.Exception
  ( IOException,
    bracket,
    finally,
    onException,
    try,
  )
import Data.ByteString qualified as ByteString
import Eclips.Herald.Runtime.TCP (ResolvedTcpEndpoint)
import Eclips.Herald.Runtime.TCP.Internal.Socket (connectTcpEndpoint)
import Eclips.Protocol.Admin.Frame
  ( AdminFrameContinuation (..),
    AdminFrameDecoder,
    AdminFrameDirection (AdminServerFrames),
    AdminFrameError,
    AdminFrameFeedResult (..),
    encodeAdminFrame,
    feedAdminFrame,
    finishAdminFrameDecoder,
    initialAdminFrameDecoder,
  )
import Eclips.Protocol.Admin.Types
  ( AdminClientDto (..),
    AdminCorrelationIdClaim,
    AdminDeploymentIdClaim,
    AdminEnvelope (..),
    AdminRoleClaim (ProcessAdministrator),
    AdminServerDto (..),
    adminReceiptRetirementWork,
  )
import Network.Socket (Socket)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBytes
import System.Timeout (timeout)

data Step14AdministrationClient = Step14AdministrationClient
  { administrationSocket :: Socket,
    administrationClosed :: MVar Bool,
    administrationReceiveState :: MVar AdministrationReceiveState
  }

data AdministrationReceiveState
  = AdministrationReceiveState
      [AdminServerDto]
      AdministrationStreamState

data AdministrationStreamState
  = AdministrationReceiving AdminFrameDecoder
  | AdministrationFrameRejected AdminFrameError
  | AdministrationStreamClosed

data Step14AdministrationReceiveFailure
  = Step14AdministrationLaneClosed
  | Step14AdministrationFrameFailed AdminFrameError
  | Step14AdministrationCorrelationMismatch
      AdminCorrelationIdClaim
      AdminCorrelationIdClaim
  deriving stock (Eq, Show)

data Step14AdministrationReceive
  = Step14AdministrationReceived AdminServerDto
  | Step14AdministrationReceiveFailed Step14AdministrationReceiveFailure
  | Step14AdministrationReceiveTimedOut
  deriving stock (Eq, Show)

-- | Connect and send the sole accepted administration role handshake. The
-- deployment claim remains schedule-owned so this helper carries no fixture
-- identity.
connectStep14AdministrationClient ::
  ResolvedTcpEndpoint ->
  AdminDeploymentIdClaim ->
  IO Step14AdministrationClient
connectStep14AdministrationClient endpoint deployment = do
  socketHandle <- connectTcpEndpoint endpoint
  closed <- newMVar False
  receiveState <-
    newMVar
      ( AdministrationReceiveState
          []
          (AdministrationReceiving (initialAdminFrameDecoder AdminServerFrames))
      )
  let client = Step14AdministrationClient socketHandle closed receiveState
  sendClientDto
    client
    (AdminHello deployment ProcessAdministrator)
    `onException` closeStep14AdministrationClient client
  pure client

withStep14AdministrationClient ::
  ResolvedTcpEndpoint ->
  AdminDeploymentIdClaim ->
  (Step14AdministrationClient -> IO result) ->
  IO result
withStep14AdministrationClient endpoint deployment =
  bracket
    (connectStep14AdministrationClient endpoint deployment)
    closeStep14AdministrationClient

closeStep14AdministrationClient :: Step14AdministrationClient -> IO ()
closeStep14AdministrationClient client =
  modifyMVar_ (administrationClosed client) $ \closed ->
    if closed
      then pure True
      else Socket.close (administrationSocket client) >> pure True

-- | Send one post-handshake request supplied by the schedule.
-- A second Hello is helper misuse rather than protocol traffic required by any
-- Step-14 schedule.
sendStep14AdministrationRequest ::
  Step14AdministrationClient ->
  AdminClientDto ->
  IO ()
sendStep14AdministrationRequest client request =
  case request of
    AdminHello {} ->
      ioError (userError "the Step-14 administration client already sent AdminHello")
    StartProcessEpoch {} -> sendClientDto client request
    EndProcessEpoch {} -> sendClientDto client request
    CancelChildPreparation {} -> sendClientDto client request
    GetAdminResult {} -> sendClientDto client request
    GetHeraldStatus {} -> sendClientDto client request
    ListChildPreparations {} -> sendClientDto client request
    DrainHerald {} -> sendClientDto client request
    PrepareOracleReplica {} -> sendClientDto client request
    BeginVoterChange {} -> sendClientDto client request
    CancelVoterChange {} -> sendClientDto client request
    GetOracleConfiguration {} -> sendClientDto client request
    GetVoterChangeStatus {} -> sendClientDto client request
    AdminWithRetirement _ work
      | adminReceiptRetirementWork work -> sendClientDto client request
      | otherwise -> ioError (userError "retirement must carry one established administration request")
    RetireAdminReceipts {} -> sendClientDto client request

-- | Hand the complete request frame to TCP and close the lane without reading
-- its result. This closes its correlation namespace while admitted semantic
-- work continues; a new connection receives a fresh result lifetime.
sendStep14AdministrationRequestAndClose ::
  Step14AdministrationClient ->
  AdminClientDto ->
  IO ()
sendStep14AdministrationRequestAndClose client request =
  sendStep14AdministrationRequest client request
    `finally` closeStep14AdministrationClient client

-- | Receive the next causally ordered server DTO. The caller owns the outer
-- timeout budget; no polling delay or sleep participates in the proof.
receiveStep14AdministrationWithin ::
  Int ->
  Step14AdministrationClient ->
  IO Step14AdministrationReceive
receiveStep14AdministrationWithin timeoutMicros client = do
  observed <- timeout timeoutMicros (receiveNext client)
  pure $ case observed of
    Nothing -> Step14AdministrationReceiveTimedOut
    Just (Left failure) -> Step14AdministrationReceiveFailed failure
    Just (Right dto) -> Step14AdministrationReceived dto

-- | Receive the next server DTO and prove that it belongs to the expected
-- request correlation within this physical lifetime. Retirement acknowledgments
-- are control frames; standalone callers inspect them with the unfiltered reader.
receiveStep14AdministrationForWithin ::
  Int ->
  AdminCorrelationIdClaim ->
  Step14AdministrationClient ->
  IO Step14AdministrationReceive
receiveStep14AdministrationForWithin timeoutMicros expected client = do
  observed <- timeout timeoutMicros receiveCorrelated
  pure $ case observed of
    Nothing -> Step14AdministrationReceiveTimedOut
    Just result -> result
  where
    receiveCorrelated =
      receiveNext client >>= \case
        Left failure -> pure (Step14AdministrationReceiveFailed failure)
        -- A piggyback is acknowledged before its attached semantic result. Keep
        -- one outer deadline while crossing these uncorrelated control frames.
        Right (AdminReceiptsRetired _) -> receiveCorrelated
        Right dto -> pure $ case serverCorrelation dto of
          Just actual
            | actual /= expected ->
                Step14AdministrationReceiveFailed
                  (Step14AdministrationCorrelationMismatch expected actual)
          _ -> Step14AdministrationReceived dto

sendClientDto :: Step14AdministrationClient -> AdminClientDto -> IO ()
sendClientDto client dto =
  withMVar (administrationClosed client) $ \closed ->
    if closed
      then ioError (userError "the Step-14 administration lane is closed")
      else
        SocketBytes.sendAll
          (administrationSocket client)
          (encodeAdminFrame (AdminClientEnvelope dto))

receiveNext ::
  Step14AdministrationClient ->
  IO (Either Step14AdministrationReceiveFailure AdminServerDto)
receiveNext client =
  modifyMVar
    (administrationReceiveState client)
    (advanceReceive (administrationSocket client))

advanceReceive ::
  Socket ->
  AdministrationReceiveState ->
  IO
    ( AdministrationReceiveState,
      Either Step14AdministrationReceiveFailure AdminServerDto
    )
advanceReceive socketHandle state@(AdministrationReceiveState pending stream) =
  case pending of
    dto : remaining ->
      pure (AdministrationReceiveState remaining stream, Right dto)
    [] -> case stream of
      AdministrationFrameRejected problem ->
        pure (state, Left (Step14AdministrationFrameFailed problem))
      AdministrationStreamClosed ->
        pure (state, Left Step14AdministrationLaneClosed)
      AdministrationReceiving decoder -> do
        received <- try @IOException (SocketBytes.recv socketHandle 32_768)
        case received of
          Left _ -> closed
          Right chunk
            | ByteString.null chunk -> endOfStream decoder
            | otherwise ->
                let AdminFrameFeedResult envelopes continuation =
                      feedAdminFrame decoder chunk
                    successor =
                      AdministrationReceiveState
                        (fmap serverDto envelopes)
                        (streamContinuation continuation)
                 in advanceReceive socketHandle successor
  where
    closed = do
      let successor = AdministrationReceiveState [] AdministrationStreamClosed
      pure (successor, Left Step14AdministrationLaneClosed)

    endOfStream decoder =
      case finishAdminFrameDecoder decoder of
        Left problem -> do
          let successor =
                AdministrationReceiveState [] (AdministrationFrameRejected problem)
          pure (successor, Left (Step14AdministrationFrameFailed problem))
        Right () -> closed

streamContinuation :: AdminFrameContinuation -> AdministrationStreamState
streamContinuation = \case
  NeedAdminFrameBytes decoder -> AdministrationReceiving decoder
  AdminFrameFailed problem -> AdministrationFrameRejected problem

serverDto :: AdminEnvelope -> AdminServerDto
serverDto = \case
  AdminServerEnvelope dto -> dto
  AdminClientEnvelope _ ->
    error "AdminServerFrames decoder admitted a client envelope"

serverCorrelation :: AdminServerDto -> Maybe AdminCorrelationIdClaim
serverCorrelation = \case
  AdminResult correlation _ -> Just correlation
  AdminAbsent correlation -> Just correlation
  AdminConflict correlation -> Just correlation
  HeraldStatus correlation _ -> Just correlation
  ChildPreparations correlation _ -> Just correlation
  HeraldDrainAccepted correlation -> Just correlation
  HeraldDrained correlation -> Just correlation
  OracleConfiguration correlation _ -> Just correlation
  VoterChangeStatus correlation _ -> Just correlation
  AdminResultRetired correlation _ -> Just correlation
  AdminReceiptsRetired _ -> Nothing
  AdminRetirementNotReady -> Nothing
