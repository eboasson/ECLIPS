-- | One-attempt TCP mechanics and bounded physical EAPP heartbeat.
module Eclips.Application.Runtime.Internal.Connection
  ( connectApplicationSocket,
    newClientConnection,
    clientConnectionGeneration,
    offerApplicationDto,
    writeFinalApplicationDto,
    noteServerDispositionQueued,
    clientHandshakeTimeoutEligible,
    establishClientConnection,
    closeClientConnection,
    runApplicationReader,
    runApplicationWriter,
    runApplicationHeartbeat,
  )
where

import Control.Concurrent.STM
  ( STM,
    TMVar,
    TVar,
    atomically,
    isEmptyTMVar,
    modifyTVar',
    newEmptyTMVarIO,
    newTQueueIO,
    newTVarIO,
    readTQueue,
    readTVar,
    registerDelay,
    retry,
    tryPutTMVar,
    writeTQueue,
    writeTVar,
  )
import Control.Exception
  ( IOException,
    mask,
    onException,
    try,
  )
import Control.Monad (forM_, unless, void)
import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Data.Word (Word64)
import Eclips.Application.Client (ApplicationTransportAttemptGeneration)
import Eclips.Application.Runtime.Internal.Physical
  ( ApplicationEstablishmentStatus (..),
    ApplicationHeartbeatActivity,
    ApplicationHeartbeatWriteStatus (..),
    applicationEstablishmentStatus,
    applicationHandshakeTimeoutEligible,
    applicationHeartbeatBidirectionalProgress,
    applicationHeartbeatReceived,
    applicationHeartbeatWriteStatus,
    applicationHeartbeatWritten,
    applicationPhysicalEstablished,
    applicationServerDispositionQueued,
    initialApplicationHeartbeatActivity,
  )
import Eclips.Application.Runtime.Internal.Socket
  ( receiveApplicationSocketBytes,
    sendApplicationSocketBytes,
  )
import Eclips.Application.Runtime.Internal.Types
  ( ApplicationEndpoint (..),
    ApplicationPhysicalPhase (..),
    ClientConnection (..),
    ClientWrite (..),
  )
import Eclips.Protocol.Application.Frame
  ( ApplicationFrameContinuation (..),
    ApplicationFrameDirection (..),
    ApplicationFrameFeedResult (..),
    feedApplicationFrame,
    initialApplicationFrameDecoder,
  )
import Eclips.Protocol.Application.Frame qualified as ApplicationFrame
import Eclips.Protocol.Application.Types
  ( ApplicationClientDto (..),
    ApplicationEnvelope (..),
    ApplicationHeartbeatNonce,
    ApplicationServerDto (..),
    applicationHeartbeatNonce,
  )
import Network.Socket
  ( AddrInfo (..),
    AddrInfoFlag (..),
    ShutdownCmd (ShutdownBoth),
    Socket,
    SocketOption (NoDelay),
    SocketType (..),
    addrAddress,
    addrFamily,
    addrProtocol,
    addrSocketType,
    close,
    connect,
    defaultHints,
    getAddrInfo,
    setSocketOption,
    shutdown,
    socket,
  )

data EstablishmentOutcome
  = EstablishmentReady
  | EstablishmentTimedOut
  | EstablishmentStopped

data IdleOutcome
  = IdleRestart ApplicationHeartbeatActivity
  | IdleElapsed
  | IdleStopped

data PingClaim
  = PingClaimed ApplicationHeartbeatNonce (TMVar ())
  | PingRestart ApplicationHeartbeatActivity
  | PingStopped

data ReplyOutcome
  = ReplyProgress
  | ReplyTimedOut
  | ReplyStopped

-- | Resolve the checked numeric endpoint and connect one fresh socket.
connectApplicationSocket :: ApplicationEndpoint -> IO Socket
connectApplicationSocket (ApplicationEndpoint host port) = do
  addresses <-
    getAddrInfo
      ( Just
          defaultHints
            { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV],
              addrSocketType = Stream
            }
      )
      (Just (Text.unpack host))
      (Just (show port))
  case addresses of
    [] -> ioError (userError "numeric application endpoint resolved to no address")
    address : _ -> do
      connected <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
      ( do
          connect connected (addrAddress address)
          setSocketOption connected NoDelay 1
        )
        `onException` close connected
      pure connected

-- | Allocate one connection record around an already-connected socket.
newClientConnection ::
  ApplicationTransportAttemptGeneration ->
  Socket ->
  IO ClientConnection
newClientConnection generation connected = do
  writer <- newTQueueIO
  closed <- newTVarIO False
  phase <- newTVarIO ApplicationPhysicalHandshaking
  activity <- newTVarIO initialApplicationHeartbeatActivity
  outstanding <- newTVarIO Nothing
  nextNonce <- newTVarIO 0
  pure
    ( ClientConnection
        generation
        connected
        writer
        closed
        phase
        activity
        outstanding
        nextNonce
    )

-- | Observe the pure-owner transport-attempt correlation.
clientConnectionGeneration ::
  ClientConnection ->
  ApplicationTransportAttemptGeneration
clientConnectionGeneration (ClientConnection generation _ _ _ _ _ _ _) = generation

-- | Offer one exact DTO to this attempt while it remains open.
offerApplicationDto :: ClientConnection -> ApplicationClientDto -> STM Bool
offerApplicationDto connection dto = offerApplicationWrite connection (ClientWrite dto Nothing)

offerApplicationWrite :: ClientConnection -> ClientWrite -> STM Bool
offerApplicationWrite (ClientConnection _ _ writer closed phase _ _ _) message = do
  isClosed <- readTVar closed
  currentPhase <- readTVar phase
  if isClosed || currentPhase == ApplicationPhysicalClosed
    then pure False
    else do
      writeTQueue writer (Just message)
      pure True

-- | Keep the sole writer alive until the final complete frame is written.
-- The existing physical reply timeout bounds a writer that cannot make progress;
-- a concurrent physical close also releases the waiter immediately.
writeFinalApplicationDto :: Word64 -> ClientConnection -> ApplicationClientDto -> IO Bool
writeFinalApplicationDto replyMicroseconds connection dto = do
  written <- newEmptyTMVarIO
  offered <- atomically (offerApplicationWrite connection (ClientWrite dto (Just written)))
  if not offered
    then pure False
    else do
      elapsed <- delayFlag replyMicroseconds
      atomically $ do
        completed <- not <$> isEmptyTMVar written
        phase <- readPhysicalPhase connection
        timedOut <- readTVar elapsed
        if completed
          then pure True
          else if phase == ApplicationPhysicalClosed || timedOut then pure False else retry

-- | Promote one exact handshake only after the pure owner validates Open or
-- Resume. TCP connect and receipt of arbitrary bytes do not establish it.
establishClientConnection :: ClientConnection -> STM Bool
establishClientConnection (ClientConnection _ _ _ closed phase _ _ _) = do
  isClosed <- readTVar closed
  currentPhase <- readTVar phase
  if isClosed || currentPhase == ApplicationPhysicalClosed
    then pure False
    else do
      writeTVar phase (applicationPhysicalEstablished currentPhase)
      pure True

-- | Stop the physical handshake clock once one complete server disposition is
-- admitted to the current connection's ordered runtime queue.
noteServerDispositionQueued :: ClientConnection -> STM Bool
noteServerDispositionQueued (ClientConnection _ _ _ closed phase _ _ _) = do
  isClosed <- readTVar closed
  currentPhase <- readTVar phase
  if isClosed || currentPhase == ApplicationPhysicalClosed
    then pure False
    else do
      writeTVar phase (applicationServerDispositionQueued currentPhase)
      pure True

-- | Check whether the physical handshake timer can still claim this binding.
-- The caller must combine this with its current-binding check and loss enqueue
-- in the same STM transaction.
clientHandshakeTimeoutEligible :: ClientConnection -> STM Bool
clientHandshakeTimeoutEligible (ClientConnection _ _ _ closed phase _ _ _) = do
  isClosed <- readTVar closed
  currentPhase <- readTVar phase
  pure (not isClosed && applicationHandshakeTimeoutEligible currentPhase)

-- | Idempotently wake all physical workers and close the descriptor.
closeClientConnection :: ClientConnection -> IO ()
closeClientConnection
  (ClientConnection _ connected writer closed phase _ outstanding _) =
    mask $ \restore -> do
      alreadyClosed <- atomically (readTVar closed)
      unless alreadyClosed $ do
        atomically $ do
          currentPhase <- readTVar phase
          unless (currentPhase == ApplicationPhysicalClosed) $ do
            writeTVar phase ApplicationPhysicalClosed
            writeTVar outstanding Nothing
            writeTQueue writer Nothing
        restore $ do
          void (try (shutdown connected ShutdownBoth) :: IO (Either IOException ()))
          close connected
        atomically (writeTVar closed True)

-- | Receive complete server-direction EAPP frames. Every nonempty inbound byte
-- chunk is physical progress; Pong is consumed here and never reaches the pure
-- session owner.
runApplicationReader :: ClientConnection -> (ApplicationServerDto -> IO ()) -> IO ()
runApplicationReader connection@(ClientConnection _ connected _ _ _ _ _ _) submit =
  loop (initialApplicationFrameDecoder ApplicationServerFrames)
  where
    loop decoder = do
      chunk <- receiveApplicationSocketBytes connected 32768
      if ByteString.null chunk
        then pure ()
        else do
          atomically (noteInboundActivity connection)
          case feedApplicationFrame decoder chunk of
            ApplicationFrameFeedResult envelopes continuation -> do
              forM_ envelopes $ \case
                ApplicationServerEnvelope (Pong _) -> pure ()
                ApplicationServerEnvelope dto -> submit dto
                ApplicationClientEnvelope _ -> pure ()
              case continuation of
                NeedApplicationFrameBytes successor -> loop successor
                ApplicationFrameFailed _ -> pure ()

-- | Serialize semantic and heartbeat DTOs through one complete-frame FIFO.
runApplicationWriter :: ClientConnection -> IO ()
runApplicationWriter (ClientConnection _ connected writer _ _ activity _ _) = loop
  where
    loop = do
      next <- atomically (readTQueue writer)
      case next of
        Nothing -> pure ()
        Just (ClientWrite dto acknowledgement) -> do
          sendApplicationSocketBytes connected (encode dto)
          atomically $ do
            modifyTVar' activity applicationHeartbeatWritten
            forM_ acknowledgement (\written -> void (tryPutTMVar written ()))
          loop
    encode =
      ApplicationFrame.encodeApplicationFrame
        . ApplicationClientEnvelope

-- | Require establishment within the configured application reply interval, then send at most one
-- heartbeat when either direction is idle. Any inbound-byte progress clears the
-- outstanding heartbeat. A missed handshake or reply closes this attempt.
runApplicationHeartbeat ::
  Word64 ->
  Word64 ->
  ClientConnection ->
  IO () ->
  IO Bool ->
  IO ()
runApplicationHeartbeat idleMicroseconds replyMicroseconds connection closeAction handshakeTimeoutAction = do
  establishment <- awaitEstablishment replyMicroseconds connection
  case establishment of
    EstablishmentReady -> do
      generation <- atomically (readActivityGeneration connection)
      loop generation
    EstablishmentTimedOut -> do
      shouldStop <- handshakeTimeoutAction
      if shouldStop
        then pure ()
        else runApplicationHeartbeat idleMicroseconds replyMicroseconds connection closeAction handshakeTimeoutAction
    EstablishmentStopped -> pure ()
  where
    loop generation = do
      idle <- awaitIdle idleMicroseconds connection generation
      case idle of
        IdleStopped -> pure ()
        IdleRestart successor -> loop successor
        IdleElapsed -> do
          written <- newEmptyTMVarIO
          claim <- atomically (claimHeartbeat connection generation written)
          case claim of
            PingStopped -> pure ()
            PingRestart successor -> loop successor
            PingClaimed nonce acknowledgement -> do
              writeStatus <- atomically (awaitHeartbeatWrite connection acknowledgement)
              case writeStatus of
                ApplicationHeartbeatWriteStopped -> pure ()
                ApplicationHeartbeatWritten -> do
                  reply <- awaitReply replyMicroseconds connection nonce
                  case reply of
                    ReplyStopped -> pure ()
                    ReplyProgress -> do
                      successor <- atomically (readActivityGeneration connection)
                      loop successor
                    ReplyTimedOut -> closeAction
                ApplicationHeartbeatWriteWaiting -> retryImpossible

awaitEstablishment :: Word64 -> ClientConnection -> IO EstablishmentOutcome
awaitEstablishment microseconds connection = do
  elapsed <- delayFlag microseconds
  atomically $ do
    phase <- readPhysicalPhase connection
    timedOut <- readTVar elapsed
    case applicationEstablishmentStatus phase timedOut of
      ApplicationEstablishmentReady -> pure EstablishmentReady
      ApplicationEstablishmentTimedOut -> pure EstablishmentTimedOut
      ApplicationEstablishmentStopped -> pure EstablishmentStopped
      ApplicationEstablishmentWaiting -> retry

awaitIdle :: Word64 -> ClientConnection -> ApplicationHeartbeatActivity -> IO IdleOutcome
awaitIdle microseconds connection generation = do
  elapsed <- delayFlag microseconds
  atomically $ do
    phase <- readPhysicalPhase connection
    case phase of
      ApplicationPhysicalClosed -> pure IdleStopped
      ApplicationPhysicalHandshaking -> retry
      ApplicationPhysicalDispositionQueued -> retry
      ApplicationPhysicalEstablished -> do
        -- Keep this one registration until its window matures. Sampling
        -- activity first would wake and abandon a timer on every inbound chunk.
        timedOut <- readTVar elapsed
        if not timedOut
          then retry
          else do
            current <- readActivityGeneration connection
            if applicationHeartbeatBidirectionalProgress generation current
              then pure (IdleRestart current)
              else pure IdleElapsed

claimHeartbeat :: ClientConnection -> ApplicationHeartbeatActivity -> TMVar () -> STM PingClaim
claimHeartbeat
  (ClientConnection _ _ writer closed phase activity outstanding nextNonce)
  generation
  written = do
    isClosed <- readTVar closed
    currentPhase <- readTVar phase
    currentActivity <- readTVar activity
    pending <- readTVar outstanding
    if isClosed || currentPhase == ApplicationPhysicalClosed
      then pure PingStopped
      else
        if currentPhase /= ApplicationPhysicalEstablished
          || applicationHeartbeatBidirectionalProgress generation currentActivity
          || pending /= Nothing
          then pure (PingRestart currentActivity)
          else do
            nonceWord <- readTVar nextNonce
            let nonce = applicationHeartbeatNonce nonceWord
            writeTVar nextNonce (nonceWord + 1)
            writeTVar outstanding (Just nonce)
            writeTQueue writer (Just (ClientWrite (Ping nonce) (Just written)))
            pure (PingClaimed nonce written)

awaitHeartbeatWrite ::
  ClientConnection ->
  TMVar () ->
  STM ApplicationHeartbeatWriteStatus
awaitHeartbeatWrite connection written = do
  phase <- readPhysicalPhase connection
  wasWritten <- not <$> isEmptyTMVar written
  case applicationHeartbeatWriteStatus phase wasWritten of
    ApplicationHeartbeatWriteWaiting -> retry
    status -> pure status

awaitReply :: Word64 -> ClientConnection -> ApplicationHeartbeatNonce -> IO ReplyOutcome
awaitReply microseconds connection nonce = do
  elapsed <- delayFlag microseconds
  atomically $ do
    phase <- readPhysicalPhase connection
    case phase of
      ApplicationPhysicalClosed -> pure ReplyStopped
      ApplicationPhysicalHandshaking -> retry
      ApplicationPhysicalDispositionQueued -> retry
      ApplicationPhysicalEstablished -> do
        outstanding <- readOutstandingHeartbeat connection
        if outstanding /= Just nonce
          then pure ReplyProgress
          else do
            timedOut <- readTVar elapsed
            if timedOut then pure ReplyTimedOut else retry

noteInboundActivity :: ClientConnection -> STM ()
noteInboundActivity (ClientConnection _ _ _ _ _ activity outstanding _) = do
  modifyTVar' activity applicationHeartbeatReceived
  writeTVar outstanding Nothing

readPhysicalPhase :: ClientConnection -> STM ApplicationPhysicalPhase
readPhysicalPhase (ClientConnection _ _ _ _ phase _ _ _) = readTVar phase

readActivityGeneration :: ClientConnection -> STM ApplicationHeartbeatActivity
readActivityGeneration (ClientConnection _ _ _ _ _ activity _ _) = readTVar activity

readOutstandingHeartbeat :: ClientConnection -> STM (Maybe ApplicationHeartbeatNonce)
readOutstandingHeartbeat (ClientConnection _ _ _ _ _ _ outstanding _) =
  readTVar outstanding

delayFlag :: Word64 -> IO (TVar Bool)
delayFlag = registerDelay . fromIntegral

retryImpossible :: IO ()
retryImpossible = ioError (userError "unreachable heartbeat write wait")
