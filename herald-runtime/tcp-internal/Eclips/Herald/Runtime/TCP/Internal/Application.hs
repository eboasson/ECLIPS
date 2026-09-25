{-# LANGUAGE OverloadedRecordDot #-}

-- | Application listener and one-reader socket ownership.
module Eclips.Herald.Runtime.TCP.Internal.Application
  ( ApplicationSocketPhase (..),
    runApplicationListener,
    claimApplicationWrite,
    closeApplicationSocketUnlessTerminating,
    sendApplicationPong,
    applicationHeartbeatPhase,
    applicationOutboundDto,
  )
where

import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    newTVarIO,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Exception
  ( IOException,
    finally,
    mask_,
    try,
  )
import Control.Monad (foldM, when)
import Data.ByteString qualified as ByteString
import Eclips.Application.Types.Lifecycle (HeraldLocator, heraldLocator)
import Eclips.Herald.Application.RPC
  ( ApplicationOutbound (..),
  )
import Eclips.Herald.Runtime
  ( HeraldRuntime,
  )
import Eclips.Herald.Runtime.Connection
  ( ApplicationPlane,
    ConnectionRef,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeLaneOffer (..),
    runtimeApplicationConnectionHandlersAt,
  )
import Eclips.Herald.Runtime.Ingress
  ( RuntimeRegistration (..),
    RuntimeSubmission (..),
    closeRuntimeConnection,
    registerApplicationConnection,
    submitApplicationDto,
  )
import Eclips.Herald.Runtime.TCP.Internal.Heartbeat
  ( HeartbeatPhase (..),
    HeartbeatState,
    discardHeartbeatAttribution,
    newHeartbeatState,
    noteHeartbeatActivity,
    runEstablishmentDeadline,
    runPassiveHeartbeat,
  )
import Eclips.Herald.Runtime.TCP.Internal.Scope (claimTcpSocket, newTcpSocketKey, releaseTcpSocket)
import Eclips.Herald.Runtime.TCP.Internal.Socket
  ( TcpSocketWriter,
    closeSocketOnce,
    newTcpSocketWriter,
    prepareConnectedTcpSocket,
    receiveTcpSocketBytes,
    sendTcpSocketBytes,
    sendTcpSocketBytesClaimed,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( HeartbeatConfiguration,
    HeartbeatLane (ApplicationHeartbeatLane),
    ResolvedTcpEndpoint (..),
    TcpContext (..),
  )
import Eclips.Protocol.Application.Frame
  ( ApplicationFrameContinuation (..),
    ApplicationFrameDirection (..),
    ApplicationFrameFeedResult (..),
    encodeApplicationFrame,
    feedApplicationFrame,
    initialApplicationFrameDecoder,
  )
import Eclips.Protocol.Application.Types
  ( ApplicationClientDto (..),
    ApplicationEnvelope (..),
    ApplicationServerDto (..),
  )
import Network.Socket
  ( Socket,
    accept,
  )

data ApplicationSocketPhase
  = CandidateApplicationSocket
  | ClaimPendingApplicationSocket
  | EstablishingApplicationSocket
  | EstablishedApplicationSocket
  | TerminatingApplicationSocket
  | ClosingApplicationSocket
  deriving stock (Eq, Show)

-- | Accept and install application leases until the listener is closed.
runApplicationListener ::
  (IO () -> IO ()) ->
  (IO () -> IO ()) ->
  TcpContext ->
  HeraldRuntime ->
  HeartbeatConfiguration ->
  ResolvedTcpEndpoint ->
  Socket ->
  IO ()
runApplicationListener spawnSocket spawnFinite context runtime heartbeatConfiguration (ResolvedTcpEndpoint host port) listener = do
  locator <- either (ioError . userError . show) pure (heraldLocator host port)
  loop locator
  where
    loop locator = do
      (connection, _) <- accept listener
      prepared <- try (prepareConnectedTcpSocket connection) :: IO (Either IOException ())
      case prepared of
        Left _ -> pure ()
        Right () ->
          installApplicationSocket
            spawnSocket
            spawnFinite
            context
            runtime
            heartbeatConfiguration
            locator
            connection
      loop locator

installApplicationSocket ::
  (IO () -> IO ()) ->
  (IO () -> IO ()) ->
  TcpContext ->
  HeraldRuntime ->
  HeartbeatConfiguration ->
  HeraldLocator ->
  Socket ->
  IO ()
installApplicationSocket spawnSocket spawnFinite context runtime heartbeatConfiguration locator socketHandle = do
  closed <- newTVarIO False
  socketKey <- newTcpSocketKey
  phase <- newTVarIO CandidateApplicationSocket
  heartbeat <- newHeartbeatState
  writer <- newTcpSocketWriter
  let closePhysical = mask_ (closeSocketOnce closed socketHandle >> releaseTcpSocket context socketKey)
      closeAction = do
        atomically (writeTVar phase ClosingApplicationSocket)
        closePhysical
      closeUnlessTerminating =
        closeApplicationSocketUnlessTerminating phase closePhysical
      handlers =
        runtimeApplicationConnectionHandlersAt
          locator
          (writeApplicationOutbound phase heartbeat writer socketHandle)
          closeAction
  claimed <- claimTcpSocket context socketKey closeAction
  if not claimed
    then closeAction
    else do
      registration <- registerApplicationConnection runtime handlers
      case registration of
        RegistrationStopped -> closeAction
        ConnectionRegistered connectionRef -> do
          spawnSocket
            ( runApplicationReader
                runtime
                connectionRef
                phase
                heartbeat
                writer
                closeUnlessTerminating
                socketHandle
                `finally` do
                  _ <- closeRuntimeConnection runtime connectionRef
                  closeUnlessTerminating
            )
          spawnFinite
            ( runEstablishmentDeadline
                context
                heartbeatConfiguration
                ApplicationHeartbeatLane
                (applicationHeartbeatPhase phase)
                closeUnlessTerminating
            )
          spawnFinite
            ( runPassiveHeartbeat
                context
                heartbeatConfiguration
                ApplicationHeartbeatLane
                (applicationHeartbeatPhase phase)
                heartbeat
                closeUnlessTerminating
                `finally` closeUnlessTerminating
            )

-- | Let a physical close race terminal delivery at the same STM-owned phase
-- boundary. Once terminal output has claimed the lane, a heartbeat decision or
-- reader finalizer cannot interrupt it; CloseWriter remains the sole action
-- that advances Terminating to Closing and closes the descriptor.
closeApplicationSocketUnlessTerminating ::
  TVar ApplicationSocketPhase ->
  IO () ->
  IO ()
closeApplicationSocketUnlessTerminating phase closePhysical = do
  claimed <-
    atomically $ do
      observed <- readTVar phase
      case observed of
        TerminatingApplicationSocket -> pure False
        _ -> writeTVar phase ClosingApplicationSocket >> pure True
  when claimed closePhysical

writeApplicationOutbound ::
  TVar ApplicationSocketPhase ->
  HeartbeatState ->
  TcpSocketWriter ->
  Socket ->
  ApplicationOutbound ->
  IO RuntimeLaneOffer
writeApplicationOutbound phase heartbeat writer socketHandle outbound = do
  case outbound of
    DisposeEstablishedApplicationSession {} -> writeTerminal
    _ -> writePhaseChecked
  where
    bytes =
      encodeApplicationFrame
        (ApplicationServerEnvelope (applicationOutboundDto outbound))
    writeTerminal = do
      permitted <- atomically (claimApplicationWrite phase outbound)
      if permitted
        then finishWrite (sendTcpSocketBytes writer socketHandle bytes)
        else pure LaneLost
    writePhaseChecked =
      finishClaimedWrite
        ( sendTcpSocketBytesClaimed
            (claimApplicationWrite phase outbound)
            (completeApplicationWrite phase heartbeat outbound)
            writer
            socketHandle
            bytes
        )
    finishClaimedWrite action = do
      result <-
        try action :: IO (Either IOException Bool)
      case result of
        Left _ -> do
          atomically (writeTVar phase ClosingApplicationSocket)
          pure LaneLost
        Right False -> pure LaneLost
        Right True -> pure LaneOffered
    finishWrite action = do
      result <- try action :: IO (Either IOException ())
      case result of
        Left _ -> do
          atomically (writeTVar phase ClosingApplicationSocket)
          pure LaneLost
        Right () -> pure LaneOffered

claimApplicationWrite ::
  TVar ApplicationSocketPhase ->
  ApplicationOutbound ->
  STM Bool
claimApplicationWrite phase outbound = do
  observed <- readTVar phase
  case (outbound, observed) of
    (SendPendingApplicationCandidate {}, CandidateApplicationSocket) -> do
      writeTVar phase EstablishingApplicationSocket
      pure True
    (SendPendingApplicationCandidate {}, ClaimPendingApplicationSocket) -> do
      writeTVar phase EstablishingApplicationSocket
      pure True
    (AcceptApplicationCandidate {}, ClaimPendingApplicationSocket) -> do
      writeTVar phase EstablishingApplicationSocket
      pure True
    (AcceptApplicationCandidate {}, CandidateApplicationSocket) -> do
      writeTVar phase EstablishingApplicationSocket
      pure True
    (DisposeEstablishedApplicationSession {}, EstablishedApplicationSocket) -> do
      -- Claim the terminal frame and quiesce reader/heartbeat work in the same
      -- transaction. Keep physical close distinct: both worker finalizers close
      -- their socket, so publishing 'ClosingApplicationSocket' here would race
      -- the terminal send rather than leave it one best-effort opportunity.
      writeTVar phase TerminatingApplicationSocket
      pure True
    (DisposeEstablishedApplicationSession {}, _) -> pure False
    (_, TerminatingApplicationSocket) -> pure False
    (_, ClosingApplicationSocket) -> pure False
    _ -> pure True

completeApplicationWrite ::
  TVar ApplicationSocketPhase ->
  HeartbeatState ->
  ApplicationOutbound ->
  STM ()
completeApplicationWrite phase heartbeat outbound = case outbound of
  SendPendingApplicationCandidate {} -> do
    observed <- readTVar phase
    case observed of
      EstablishingApplicationSocket -> do
        noteHeartbeatActivity heartbeat
        writeTVar phase ClaimPendingApplicationSocket
      _ -> pure ()
  AcceptApplicationCandidate {} -> do
    observed <- readTVar phase
    case observed of
      EstablishingApplicationSocket -> do
        noteHeartbeatActivity heartbeat
        writeTVar phase EstablishedApplicationSocket
      _ -> pure ()
  _ -> pure ()

applicationHeartbeatPhase ::
  TVar ApplicationSocketPhase ->
  STM HeartbeatPhase
applicationHeartbeatPhase phase = do
  observed <- readTVar phase
  pure $ case observed of
    CandidateApplicationSocket -> HeartbeatNotEstablished
    ClaimPendingApplicationSocket -> HeartbeatEstablished
    EstablishingApplicationSocket -> HeartbeatNotEstablished
    EstablishedApplicationSocket -> HeartbeatEstablished
    -- Quiescing defeats the establishment deadline while parking the passive
    -- worker until CloseWriter publishes Closed. Neither worker may run its
    -- close finalizer ahead of the terminal frame.
    TerminatingApplicationSocket -> HeartbeatQuiescing
    ClosingApplicationSocket -> HeartbeatClosed

sendApplicationPong ::
  TVar ApplicationSocketPhase ->
  TcpSocketWriter ->
  IO () ->
  Socket ->
  ApplicationServerDto ->
  IO Bool
sendApplicationPong phase writer closeAction socketHandle pong = do
  result <-
    try
      ( sendTcpSocketBytesClaimed
          ((`elem` [ClaimPendingApplicationSocket, EstablishedApplicationSocket]) <$> readTVar phase)
          (pure ())
          writer
          socketHandle
          ( encodeApplicationFrame
              (ApplicationServerEnvelope pong)
          )
      ) ::
      IO (Either IOException Bool)
  case result of
    Left _ -> closeAction >> pure False
    Right sent -> pure sent

applicationOutboundDto :: ApplicationOutbound -> ApplicationServerDto
applicationOutboundDto = \case
  AcceptApplicationCandidate _ _ dto -> dto
  SendPendingApplicationCandidate _ dto -> dto
  RejectApplicationCandidate _ dto -> dto
  RejectEstablishedApplication _ dto -> dto
  SendEstablishedApplication _ dto -> dto
  DisposeEstablishedApplicationSession _ dto -> dto

runApplicationReader ::
  HeraldRuntime ->
  ConnectionRef ApplicationPlane ->
  TVar ApplicationSocketPhase ->
  HeartbeatState ->
  TcpSocketWriter ->
  IO () ->
  Socket ->
  IO ()
runApplicationReader runtime connectionRef phase heartbeat writer closeAction socketHandle =
  loop (initialApplicationFrameDecoder ApplicationClientFrames)
  where
    loop decoder = do
      chunk <- receiveTcpSocketBytes socketHandle 32768
      if ByteString.null chunk
        then pure ()
        else do
          atomically (noteApplicationChunkActivity phase heartbeat)
          case feedApplicationFrame decoder chunk of
            ApplicationFrameFeedResult envelopes continuation -> do
              continue <- foldM submitEnvelope True envelopes
              if continue
                then case continuation of
                  NeedApplicationFrameBytes successor -> loop successor
                  ApplicationFrameFailed _ -> pure ()
                else pure ()
    submitEnvelope False _ = pure False
    submitEnvelope True envelope = do
      atomically (discardEstablishedAttribution phase heartbeat)
      case envelope of
        ApplicationClientEnvelope dto -> do
          observed <- atomically (awaitApplicationReadablePhase phase)
          case (observed, dto) of
            (CandidateApplicationSocket, Ping _) -> pure False
            (current, Ping nonce) | current `elem` [ClaimPendingApplicationSocket, EstablishedApplicationSocket] -> do
              sent <-
                sendApplicationPong
                  phase
                  writer
                  closeAction
                  socketHandle
                  (Pong nonce)
              if sent
                then pure True
                else atomically (awaitApplicationReadablePhase phase) >> pure False
            (CandidateApplicationSocket, _) -> submitDto dto
            (ClaimPendingApplicationSocket, _) -> submitDto dto
            (EstablishedApplicationSocket, _) -> submitDto dto
            (TerminatingApplicationSocket, _) -> pure False
            (ClosingApplicationSocket, _) -> pure False
            (EstablishingApplicationSocket, _) -> pure False
        ApplicationServerEnvelope _ -> pure False
    submitDto :: ApplicationClientDto -> IO Bool
    submitDto dto = do
      result <- submitApplicationDto runtime connectionRef dto
      pure (result == Queued)

awaitApplicationReadablePhase ::
  TVar ApplicationSocketPhase ->
  STM ApplicationSocketPhase
awaitApplicationReadablePhase phase = do
  observed <- readTVar phase
  case observed of
    EstablishingApplicationSocket -> retry
    -- Do not let the reader finalizer close the socket ahead of the terminal
    -- writer. CloseWriter changes this to Closing and wakes the parked reader.
    TerminatingApplicationSocket -> retry
    _ -> pure observed

noteApplicationChunkActivity ::
  TVar ApplicationSocketPhase ->
  HeartbeatState ->
  STM ()
noteApplicationChunkActivity phase heartbeat = do
  observed <- readTVar phase
  case observed of
    ClaimPendingApplicationSocket -> noteHeartbeatActivity heartbeat
    EstablishedApplicationSocket -> noteHeartbeatActivity heartbeat
    _ -> pure ()

discardEstablishedAttribution ::
  TVar ApplicationSocketPhase ->
  HeartbeatState ->
  STM ()
discardEstablishedAttribution phase heartbeat = do
  observed <- readTVar phase
  case observed of
    ClaimPendingApplicationSocket -> discardHeartbeatAttribution heartbeat
    EstablishedApplicationSocket -> discardHeartbeatAttribution heartbeat
    _ -> pure ()
