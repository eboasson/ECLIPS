{-# LANGUAGE OverloadedRecordDot #-}

-- | Exclusive pure-client ownership and its scoped transport interpreter.
module Eclips.Application.Runtime.Internal.Owner
  ( withApplicationRuntime,
    submitApplicationCall,
    submitApplicationLifecycleCall,
    recoverEndProcessRuntime,
    awaitApplicationIsolationRuntime,
    awaitApplicationUnavailableRuntime,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkFinally,
    killThread,
    myThreadId,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    withMVar,
  )
import Control.Concurrent.STM
  ( STM,
    TMVar,
    TVar,
    atomically,
    flushTQueue,
    modifyTVar',
    newEmptyTMVarIO,
    newTQueueIO,
    newTVarIO,
    orElse,
    readTMVar,
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
    SomeException,
    finally,
    fromException,
    mask,
    mask_,
    onException,
    throwIO,
    try,
  )
import Control.Monad (forM_, unless, void, when)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Eclips.Application.Client
  ( ApplicationCallOutcome,
    ApplicationClientEffect (..),
    ApplicationClientEffectBatch,
    ApplicationClientInput (..),
    ApplicationClientMonotonicInstant,
    ApplicationClientState,
    ApplicationClientTransition (..),
    ApplicationInvocationId,
    ApplicationTransportAttemptGeneration,
    ApplicationUnavailableReason,
    applicationClientEffects,
    applicationClientMonotonicInstant,
    applicationClientMonotonicInstantMicroseconds,
    applicationClientRecoveryDeadlineMicroseconds,
    initialApplicationClient,
    stepApplicationClient,
  )
import Eclips.Application.Client qualified as Client
import Eclips.Application.Runtime.Internal.Connection
  ( clientConnectionGeneration,
    clientHandshakeTimeoutEligible,
    closeClientConnection,
    connectApplicationSocket,
    establishClientConnection,
    newClientConnection,
    noteServerDispositionQueued,
    offerApplicationDto,
    runApplicationHeartbeat,
    runApplicationReader,
    runApplicationWriter,
    writeFinalApplicationDto,
  )
import Eclips.Application.Runtime.Internal.Types
  ( Application (..),
    ApplicationCall (..),
    ApplicationCallCompletion (..),
    ApplicationConfiguration (..),
    ApplicationLifecycleCall (..),
    ApplicationLifecycleCompletion (..),
    ApplicationLivenessConfiguration (..),
    ApplicationRuntimeFailure (..),
    ClientConnection,
    EffectEnvelope (..),
    RuntimeContext (..),
    RuntimeTimerKey (..),
    applicationConfigurationAttachment,
    applicationConfigurationEndpoint,
    applicationConfigurationLiveness,
  )
import Eclips.Application.Types.Access (ApplicationStartupAccess)
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Protocol.Application.Types
  ( ApplicationClientDto (EndSession),
    ApplicationIsolationOverlayDto,
    ApplicationServerDto,
  )
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket qualified as Socket

-- | Run one pure client and every physical child within the callback scope.
withApplicationRuntime ::
  ApplicationConfiguration ->
  (Application -> ApplicationStartupAccess -> IO result) ->
  IO (Either ApplicationRuntimeFailure result)
withApplicationRuntime configuration use =
  mask $ \restore -> do
    context <- newRuntimeContext
    ( do
        startRuntimeWorkers configuration context
        atomically (writeTQueue context.runtimeInput Start)
        startup <-
          restore (atomically (awaitStartup context))
            `onException` stopRuntime context
        case startup of
          Left failure -> do
            stopRuntime context
            pure (Left failure)
          Right access -> do
            callbackResult <- newEmptyTMVarIO
            callbackThread <-
              forkFinally
                (restore (use (Application context) access))
                (atomically . void . tryPutTMVar callbackResult)
            first <-
              restore
                ( atomically
                    ( (Left <$> readTMVar callbackResult)
                        `orElse` (Right <$> readTMVar context.runtimeFailure)
                    )
                )
                `onException` cancelCallbackAndStop callbackThread callbackResult context
            case first of
              Right failure -> do
                stopRuntime context
                killThread callbackThread
                void (atomically (readTMVar callbackResult))
                pure (Left failure)
              Left callback -> do
                shutdownResult <- orderlyClose context
                stopRuntime context
                case callback of
                  Left exception -> throwIO exception
                  Right result -> case shutdownResult of
                    Left failure -> pure (Left failure)
                    Right () -> pure (Right result)
      )
      `onException` stopRuntime context

awaitApplicationIsolationRuntime ::
  Application -> IO ApplicationIsolationOverlayDto
awaitApplicationIsolationRuntime (Application context) =
  atomically (readTMVar context.runtimeIsolation)

awaitApplicationUnavailableRuntime ::
  Application -> IO ApplicationUnavailableReason
awaitApplicationUnavailableRuntime (Application context) =
  atomically (readTMVar context.runtimeUnavailable)

cancelCallbackAndStop ::
  ThreadId ->
  TMVar (Either SomeException result) ->
  RuntimeContext ->
  IO ()
cancelCallbackAndStop callbackThread callbackResult context = do
  stopRuntime context
  killThread callbackThread
  void (atomically (readTMVar callbackResult))

awaitStartup :: RuntimeContext -> STM (Either ApplicationRuntimeFailure ApplicationStartupAccess)
awaitStartup context =
  readTMVar context.runtimeStartup
    `orElse` (Left <$> readTMVar context.runtimeFailure)
    `orElse` (Left . ApplicationSessionUnavailableBeforeStartup <$> readTMVar context.runtimeUnavailable)

newRuntimeContext :: IO RuntimeContext
newRuntimeContext =
  RuntimeContext
    <$> newTQueueIO
    <*> newTQueueIO
    <*> newEmptyTMVarIO
    <*> newEmptyTMVarIO
    <*> newEmptyTMVarIO
    <*> newEmptyTMVarIO
    <*> newEmptyTMVarIO
    <*> newTVarIO False
    <*> newMVar ()
    <*> newTVarIO Nothing
    <*> newTVarIO Nothing
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO 0
    <*> newTVarIO Map.empty

startRuntimeWorkers :: ApplicationConfiguration -> RuntimeContext -> IO ()
startRuntimeWorkers configuration context =
  startRuntimeWorkersFrom configuration context initial
  where
    ApplicationLivenessConfiguration recovery _ _ = applicationConfigurationLiveness configuration
    initial = case configuration of
      ApplicationConfiguration _ attachment nonce _ -> initialApplicationClient recovery attachment nonce
      PreparedApplicationConfiguration descriptor claim _ -> Client.initialPreparedApplicationClient recovery descriptor claim

startRuntimeWorkersFrom :: ApplicationConfiguration -> RuntimeContext -> ApplicationClientState -> IO ()
startRuntimeWorkersFrom configuration context initial = do
  void (spawnEssential context (ownerLoop context initial))
  void (spawnEssential context (effectLoop configuration context))

ownerLoop :: RuntimeContext -> ApplicationClientState -> IO ()
ownerLoop context = loop
  where
    loop state = do
      input <- atomically (readTQueue context.runtimeInput)
      case stepApplicationClient input state of
        Left _ -> signalFailure context ApplicationClientInvariantFailure
        Right transition -> case transition of
          ApplicationClientCommandRejected _ -> do
            settleRejectedInput context input
            loop state
          ApplicationClientAdvanced successor batch -> do
            handoffEffects context batch
            loop successor

handoffEffects :: RuntimeContext -> ApplicationClientEffectBatch -> IO ()
handoffEffects context batch =
  atomically $ do
    captured <- readTVar context.runtimeCurrent
    writeTQueue
      context.runtimeEffects
      (EffectEnvelope captured (applicationClientEffects batch))

settleRejectedInput :: RuntimeContext -> ApplicationClientInput -> IO ()
settleRejectedInput context = \case
  Submit invocation _ -> settleCall context invocation ApplicationCallClosed
  SubmitLifecycle invocation _ -> settleLifecycle context invocation ApplicationLifecycleClosed
  _ -> pure ()

effectLoop :: ApplicationConfiguration -> RuntimeContext -> IO ()
effectLoop configuration context = loop
  where
    loop = do
      EffectEnvelope captured effects <- atomically (readTQueue context.runtimeEffects)
      forM_ effects (interpretEffect configuration context captured)
      loop

interpretEffect ::
  ApplicationConfiguration ->
  RuntimeContext ->
  Maybe ClientConnection ->
  ApplicationClientEffect ->
  IO ()
interpretEffect configuration context captured = \case
  RequestTransport attempt -> requestTransport configuration context attempt
  ArmTransportRetry scope attempt retryAt ->
    armRuntimeTimer
      context
      (RuntimeTransportRetryTimer scope attempt)
      (applicationClientMonotonicInstantMicroseconds retryAt)
      (TransportRetryElapsed scope attempt)
  CancelTransportRetry scope attempt ->
    cancelRuntimeTimer context (RuntimeTransportRetryTimer scope attempt)
  ArmRecoveryDeadline generation deadline ->
    armRuntimeTimer
      context
      (RuntimeRecoveryDeadlineTimer generation deadline)
      (applicationClientRecoveryDeadlineMicroseconds deadline)
      (RecoveryDeadlineElapsed generation deadline)
  CancelRecoveryDeadline generation deadline ->
    cancelRuntimeTimer context (RuntimeRecoveryDeadlineTimer generation deadline)
  SessionEstablishedOnTransport attempt ->
    forM_ captured $ \connection ->
      atomically $ do
        current <- readTVar context.runtimeCurrent
        when
          (clientConnectionGeneration connection == attempt && sameConnection connection current)
          (void (establishClientConnection connection))
  ScheduleReceiptRetirement attempt delay -> do
    now <- monotonicNow
    armRuntimeTimer
      context
      (RuntimeReceiptRetirementTimer attempt)
      (applicationClientMonotonicInstantMicroseconds now + delay)
      (const (ReceiptRetirementFlush attempt))
  SendApplicationDto dto ->
    forM_ captured $ \connection -> do
      offered <- case dto of
        EndSession _ -> do
          let ApplicationLivenessConfiguration _ _ reply = applicationConfigurationLiveness configuration
          writeFinalApplicationDto reply connection dto
        _ -> atomically (offerApplicationDto connection dto)
      unless offered (loseConnection context connection)
  SessionReady access ->
    atomically (void (tryPutTMVar context.runtimeStartup (Right access)))
  HeraldIsolationObserved overlay ->
    atomically (void (tryPutTMVar context.runtimeIsolation overlay))
  HeraldPermanentlyUnavailableObserved reason ->
    atomically (void (tryPutTMVar context.runtimeUnavailable reason))
  CallFinished invocation outcome ->
    settleCall context invocation (completionFromOutcome outcome)
  CallCancelled invocation -> settleCall context invocation ApplicationCallCancelled
  CallUnknown invocation reason -> settleCall context invocation (ApplicationCallUnknown reason)
  CallClosed invocation -> settleCall context invocation ApplicationCallClosed
  SessionOpenFailed rejection -> do
    let failure = ApplicationSessionOpenRejected rejection
    atomically $ do
      void (tryPutTMVar context.runtimeStartup (Left failure))
      void (tryPutTMVar context.runtimeFailure failure)
  InitialClaimFailed rejection -> do
    let failure = ApplicationInitialClaimRejected rejection
    atomically $ do
      void (tryPutTMVar context.runtimeStartup (Left failure))
      void (tryPutTMVar context.runtimeFailure failure)
  LifecycleAssigned invocation request -> atomically $ do
    cells <- readTVar context.runtimeLifecycleCalls
    forM_ (Map.lookup invocation cells) $ \(_, _, assigned) -> void (tryPutTMVar assigned (Just request))
  LifecycleChanged invocation status -> do
    atomically $ do
      cells <- readTVar context.runtimeLifecycleCalls
      forM_ (Map.lookup invocation cells) $ \(_, progress, _) -> writeTVar progress (Just status)
    case status of
      Lifecycle.LifecyclePending _ -> pure ()
      Lifecycle.LifecycleCompleted result -> settleLifecycle context invocation (ApplicationLifecycleSucceeded result)
      Lifecycle.LifecycleRejected problem -> settleLifecycle context invocation (ApplicationLifecycleRejected problem)
  LifecycleUnknown invocation reason -> settleLifecycle context invocation (ApplicationLifecycleUnknown reason)
  LifecycleClosed invocation -> settleLifecycle context invocation ApplicationLifecycleClosed
  ResetTransport -> closeAllTransport context captured
  CloseTransport -> do
    closeAllTransport context captured
    atomically (void (tryPutTMVar context.runtimeClosed ()))

completionFromOutcome :: ApplicationCallOutcome -> ApplicationCallCompletion
completionFromOutcome = \case
  Client.ApplicationCallSucceeded result -> ApplicationCallSucceeded result
  Client.ApplicationCallRejected rejection -> ApplicationCallRejected rejection

requestTransport ::
  ApplicationConfiguration ->
  RuntimeContext ->
  ApplicationTransportAttemptGeneration ->
  IO ()
requestTransport configuration context attempt = do
  accepted <-
    atomically $ do
      stopping <- readTVar context.runtimeStopping
      requested <- readTVar context.runtimeRequestedAttempt
      current <- readTVar context.runtimeCurrent
      if stopping || requested /= Nothing || hasCurrent current
        then pure False
        else do
          writeTVar context.runtimeRequestedAttempt (Just attempt)
          pure True
  when accepted
    $ void
      ( spawnRequired
          context
          (atomically (clearRequestedAttempt context attempt))
          (dialTransport configuration context attempt)
      )
  where
    hasCurrent Nothing = False
    hasCurrent (Just _) = True

dialTransport ::
  ApplicationConfiguration ->
  RuntimeContext ->
  ApplicationTransportAttemptGeneration ->
  IO ()
dialTransport
  configuration
  context
  attempt = do
    let endpoint = applicationConfigurationEndpoint configuration
    connected <- try (connectApplicationSocket endpoint) :: IO (Either IOException Socket.Socket)
    case connected of
      Left _ -> reportTransportAttemptFailure context attempt
      Right socket -> do
        connection <-
          newClientConnection attempt socket
            `onException` Socket.close socket
        ( do
            installed <-
              observeRuntimeEvent context $ \observedAt ->
                atomically $ do
                  stopping <- readTVar context.runtimeStopping
                  requested <- readTVar context.runtimeRequestedAttempt
                  current <- readTVar context.runtimeCurrent
                  if stopping || requested /= Just attempt || hasCurrent current
                    then pure False
                    else do
                      writeTVar context.runtimeRequestedAttempt Nothing
                      writeTVar context.runtimeCurrent (Just connection)
                      writeTQueue context.runtimeInput (TransportAvailable attempt observedAt)
                      pure True
            if installed
              then startConnectionWorkers configuration context connection
              else closeClientConnection connection
          )
          `onException` closeClientConnection connection
    where
      hasCurrent Nothing = False
      hasCurrent (Just _) = True

reportTransportAttemptFailure ::
  RuntimeContext ->
  ApplicationTransportAttemptGeneration ->
  IO ()
reportTransportAttemptFailure context attempt = do
  observeRuntimeEvent context $ \observedAt ->
    atomically $ do
      stopping <- readTVar context.runtimeStopping
      requested <- readTVar context.runtimeRequestedAttempt
      when (not stopping && requested == Just attempt) $ do
        writeTVar context.runtimeRequestedAttempt Nothing
        writeTQueue context.runtimeInput (TransportAttemptFailed attempt observedAt)

clearRequestedAttempt ::
  RuntimeContext ->
  ApplicationTransportAttemptGeneration ->
  STM ()
clearRequestedAttempt context attempt = do
  requested <- readTVar context.runtimeRequestedAttempt
  when (requested == Just attempt) $ writeTVar context.runtimeRequestedAttempt Nothing

startConnectionWorkers ::
  ApplicationConfiguration ->
  RuntimeContext ->
  ClientConnection ->
  IO ()
startConnectionWorkers
  configuration
  context
  connection = do
    let ApplicationLivenessConfiguration _ idle reply = applicationConfigurationLiveness configuration
    void
      ( spawnTracked
          context
          ( runConnectionLaneWorker
              context
              connection
              (runApplicationReader connection (submitServerDto context connection))
          )
      )
    void
      ( spawnTracked
          context
          (runConnectionLaneWorker context connection (runApplicationWriter connection))
      )
    void
      ( spawnTracked
          context
          (runHeartbeatWorker context idle reply connection)
      )

runHeartbeatWorker ::
  RuntimeContext ->
  Word64 ->
  Word64 ->
  ClientConnection ->
  IO ()
runHeartbeatWorker context idle reply connection =
  runConnectionLaneWorker
    context
    connection
    ( runApplicationHeartbeat
        idle
        reply
        connection
        (loseConnection context connection)
        (loseHandshakingConnection context connection)
    )

-- | Socket failures and EOF are lane loss. A non-I/O fault in framing, the
-- sole writer, or heartbeat machinery is an owner failure; cancellation after
-- teardown begins is deliberately silent.
runConnectionLaneWorker :: RuntimeContext -> ClientConnection -> IO () -> IO ()
runConnectionLaneWorker context connection action = do
  result <-
    try (action `finally` loseConnection context connection) ::
      IO (Either SomeException ())
  case result of
    Right () -> pure ()
    Left exception ->
      case tryIOException exception of
        Just _ -> pure ()
        Nothing -> do
          stopping <- atomically (readTVar context.runtimeStopping)
          unless stopping (signalFailure context ApplicationTransportOwnerFailure)
  where
    tryIOException :: SomeException -> Maybe IOException
    tryIOException = fromException

submitServerDto :: RuntimeContext -> ClientConnection -> ApplicationServerDto -> IO ()
submitServerDto context connection dto =
  observeRuntimeEvent context $ \observedAt ->
    atomically $ do
      current <- readTVar context.runtimeCurrent
      when (sameConnection connection current) $ do
        queued <- noteServerDispositionQueued connection
        when queued
          $ writeTQueue
            context.runtimeInput
            (ServerDtoReceived (clientConnectionGeneration connection) observedAt dto)

loseConnection :: RuntimeContext -> ClientConnection -> IO ()
loseConnection context connection = do
  _ <-
    observeRuntimeEvent context $ \observedAt ->
      atomically $ do
        current <- readTVar context.runtimeCurrent
        if sameConnection connection current
          then do
            writeTVar context.runtimeCurrent Nothing
            writeTQueue
              context.runtimeInput
              (TransportLost (clientConnectionGeneration connection) observedAt)
            pure True
          else pure False
  closeClientConnection connection

-- | Let a physical handshake timeout claim the binding only if no complete
-- server disposition reached the ordered owner queue first. A false result
-- tells the heartbeat worker to continue waiting for local pure admission.
loseHandshakingConnection :: RuntimeContext -> ClientConnection -> IO Bool
loseHandshakingConnection context connection = do
  (shouldStop, won) <-
    observeRuntimeEvent context $ \observedAt ->
      atomically $ do
        current <- readTVar context.runtimeCurrent
        if not (sameConnection connection current)
          then pure (True, False)
          else do
            eligible <- clientHandshakeTimeoutEligible connection
            if not eligible
              then pure (False, False)
              else do
                writeTVar context.runtimeCurrent Nothing
                writeTQueue
                  context.runtimeInput
                  (TransportLost (clientConnectionGeneration connection) observedAt)
                pure (True, True)
  when won (closeClientConnection connection)
  pure shouldStop

sameConnection :: ClientConnection -> Maybe ClientConnection -> Bool
sameConnection _ Nothing = False
sameConnection expected (Just current) =
  clientConnectionGeneration expected == clientConnectionGeneration current

armRuntimeTimer ::
  RuntimeContext ->
  RuntimeTimerKey ->
  Word64 ->
  (ApplicationClientMonotonicInstant -> ApplicationClientInput) ->
  IO ()
armRuntimeTimer context key dueAt makeInput = do
  cancelled <- newTVarIO False
  accepted <-
    atomically $ do
      stopping <- readTVar context.runtimeStopping
      if stopping
        then pure False
        else do
          timers <- readTVar context.runtimeTimers
          forM_ (Map.lookup key timers) (\prior -> writeTVar prior True)
          writeTVar context.runtimeTimers (Map.insert key cancelled timers)
          pure True
  when accepted $ do
    void
      ( spawnRequired
          context
          (cancelRuntimeTimer context key)
          $ do
            elapsed <- deadlineFlag dueAt
            shouldFinish <-
              atomically $ do
                isCancelled <- readTVar cancelled
                isElapsed <- readTVar elapsed
                if isCancelled
                  then pure False
                  else if isElapsed then pure True else retry
            when shouldFinish $ do
              observeRuntimeEvent context $ \observedAt ->
                atomically $ do
                  isCancelled <- readTVar cancelled
                  stopping <- readTVar context.runtimeStopping
                  unless (isCancelled || stopping) $ do
                    modifyTVar' context.runtimeTimers (Map.delete key)
                    writeTQueue context.runtimeInput (makeInput observedAt)
      )

cancelRuntimeTimer :: RuntimeContext -> RuntimeTimerKey -> IO ()
cancelRuntimeTimer context key =
  atomically $ do
    timers <- readTVar context.runtimeTimers
    forM_ (Map.lookup key timers) (\cancelled -> writeTVar cancelled True)
    writeTVar context.runtimeTimers (Map.delete key timers)

deadlineFlag :: Word64 -> IO (TVar Bool)
deadlineFlag dueAt = do
  now <- monotonicNow
  let nowWord = applicationClientMonotonicInstantMicroseconds now
  if nowWord >= dueAt
    then newTVarIO True
    else registerDelay (fromIntegral (dueAt - nowWord))

monotonicNow :: IO ApplicationClientMonotonicInstant
monotonicNow =
  applicationClientMonotonicInstant
    . (`div` 1000)
    <$> getMonotonicTimeNSec

-- | Serialize the physical observation point itself, not merely the later STM
-- write. Consequently every timestamped input reaches the owner in the same
-- order in which its monotonic instant was sampled.
observeRuntimeEvent ::
  RuntimeContext ->
  (ApplicationClientMonotonicInstant -> IO result) ->
  IO result
observeRuntimeEvent context action =
  withMVar context.runtimeObservationGate $ \() ->
    monotonicNow >>= action

closeAllTransport :: RuntimeContext -> Maybe ClientConnection -> IO ()
closeAllTransport context captured = do
  current <-
    atomically $ do
      writeTVar context.runtimeRequestedAttempt Nothing
      current <- readTVar context.runtimeCurrent
      writeTVar context.runtimeCurrent Nothing
      timers <- readTVar context.runtimeTimers
      forM_ timers (\cancelled -> writeTVar cancelled True)
      writeTVar context.runtimeTimers Map.empty
      pure current
  closeDistinctConnections captured current

closeDistinctConnections :: Maybe ClientConnection -> Maybe ClientConnection -> IO ()
closeDistinctConnections captured current = do
  forM_ captured closeClientConnection
  case (captured, current) of
    (Just old, Just live)
      | clientConnectionGeneration old == clientConnectionGeneration live -> pure ()
    (_, Just live) -> closeClientConnection live
    _ -> pure ()

settleCall :: RuntimeContext -> ApplicationInvocationId -> ApplicationCallCompletion -> IO ()
settleCall context invocation completion =
  atomically $ do
    cells <- readTVar context.runtimeCalls
    forM_ (Map.lookup invocation cells) $ \cell ->
      void (tryPutTMVar cell completion)
    writeTVar context.runtimeCalls (Map.delete invocation cells)

signalFailure :: RuntimeContext -> ApplicationRuntimeFailure -> IO ()
signalFailure context failure =
  atomically (void (tryPutTMVar context.runtimeFailure failure))

orderlyClose :: RuntimeContext -> IO (Either ApplicationRuntimeFailure ())
orderlyClose context = do
  atomically (writeTQueue context.runtimeInput End)
  atomically
    ( (Right <$> readTMVar context.runtimeClosed)
        `orElse` (Left <$> readTMVar context.runtimeFailure)
    )

stopRuntime :: RuntimeContext -> IO ()
stopRuntime context = mask_ $ do
  (current, tracked) <-
    atomically $ do
      writeTVar context.runtimeStopping True
      writeTVar context.runtimeRequestedAttempt Nothing
      current <- readTVar context.runtimeCurrent
      writeTVar context.runtimeCurrent Nothing
      timers <- readTVar context.runtimeTimers
      forM_ timers (\cancelled -> writeTVar cancelled True)
      writeTVar context.runtimeTimers Map.empty
      lifecycleCalls <- readTVar context.runtimeLifecycleCalls
      forM_ lifecycleCalls $ \(cell, _, assigned) -> do
        void (tryPutTMVar cell ApplicationLifecycleClosed)
        void (tryPutTMVar assigned Nothing)
      writeTVar context.runtimeLifecycleCalls Map.empty
      calls <- readTVar context.runtimeCalls
      forM_ calls $ \cell ->
        void (tryPutTMVar cell ApplicationCallClosed)
      writeTVar context.runtimeCalls Map.empty
      tracked <- readTVar context.runtimeThreads
      pure (current, Map.toList tracked)
  forM_ current closeClientConnection
    `finally` do
      forM_ tracked $ \(thread, _) -> killThread thread
      forM_ tracked $ \(_, done) -> void (readMVar done)
      atomically $ do
        void (flushTQueue context.runtimeInput)
        void (flushTQueue context.runtimeEffects)

spawnEssential :: RuntimeContext -> IO () -> IO (Maybe ThreadId)
spawnEssential context action =
  spawnTracked context actionWithTerminal
  where
    actionWithTerminal = do
      result <- try action :: IO (Either SomeException ())
      stopping <- atomically (readTVar context.runtimeStopping)
      unless stopping $ case result of
        Left _ -> signalFailure context ApplicationTransportOwnerFailure
        Right () -> signalFailure context ApplicationTransportOwnerFailure

-- | Run a finite required child. Normal return is expected; an unexpected
-- exception first releases its owned correlation and then terminates the scope.
-- Async cancellation during scope teardown is intentionally silent.
spawnRequired :: RuntimeContext -> IO () -> IO () -> IO (Maybe ThreadId)
spawnRequired context release action =
  spawnTracked context $ do
    result <- try action :: IO (Either SomeException ())
    case result of
      Right () -> pure ()
      Left _ -> do
        stopping <- atomically (readTVar context.runtimeStopping)
        unless stopping $ do
          release
          signalFailure context ApplicationTransportOwnerFailure

spawnTracked :: RuntimeContext -> IO () -> IO (Maybe ThreadId)
spawnTracked context action = mask_ $ do
  start <- newEmptyMVar
  done <- newEmptyMVar
  thread <-
    forkFinally
      (takeMVar start >> action)
      ( \_ -> do
          ownThread <- myThreadId
          putMVar done ()
          atomically (modifyTVar' context.runtimeThreads (Map.delete ownThread))
      )
  accepted <-
    atomically $ do
      stopping <- readTVar context.runtimeStopping
      if stopping
        then pure False
        else do
          modifyTVar' context.runtimeThreads (Map.insert thread done)
          pure True
  putMVar start ()
  if accepted
    then pure (Just thread)
    else do
      killThread thread
      void (readMVar done)
      pure Nothing

-- | Allocate a never-reused finite-run invocation and hand its command to the
-- sole owner.
submitApplicationCall ::
  Application ->
  (ApplicationInvocationId -> ApplicationClientInput) ->
  IO ApplicationCall
submitApplicationCall (Application context) makeInput = do
  completion <- newEmptyTMVarIO
  invocation <-
    atomically $ do
      next <- readTVar context.runtimeNextInvocation
      writeTVar context.runtimeNextInvocation (next + 1)
      let invocation = Client.applicationInvocationId next
      stopping <- readTVar context.runtimeStopping
      if stopping
        then void (tryPutTMVar completion ApplicationCallClosed)
        else do
          modifyTVar' context.runtimeCalls (Map.insert invocation completion)
          writeTQueue context.runtimeInput (makeInput invocation)
      pure invocation
  pure (ApplicationCall invocation completion context.runtimeInput)

settleLifecycle :: RuntimeContext -> ApplicationInvocationId -> ApplicationLifecycleCompletion -> IO ()
settleLifecycle context invocation completion = atomically $ do
  cells <- readTVar context.runtimeLifecycleCalls
  forM_ (Map.lookup invocation cells) $ \(cell, _, assigned) -> do
    void (tryPutTMVar cell completion)
    void (tryPutTMVar assigned Nothing)
  writeTVar context.runtimeLifecycleCalls (Map.delete invocation cells)

submitApplicationLifecycleCall :: Application -> Lifecycle.LifecycleCommand -> IO ApplicationLifecycleCall
submitApplicationLifecycleCall (Application context) command = do
  completion <- newEmptyTMVarIO
  progress <- newTVarIO Nothing
  assigned <- newEmptyTMVarIO
  atomically $ do
    next <- readTVar context.runtimeNextInvocation
    writeTVar context.runtimeNextInvocation (next + 1)
    let invocation = Client.applicationInvocationId next
    stopping <- readTVar context.runtimeStopping
    if stopping
      then do
        void (tryPutTMVar completion ApplicationLifecycleClosed)
        void (tryPutTMVar assigned Nothing)
      else do
        modifyTVar' context.runtimeLifecycleCalls (Map.insert invocation (completion, progress, assigned))
        writeTQueue context.runtimeInput (SubmitLifecycle invocation command)
  request <- atomically (readTMVar assigned)
  pure (ApplicationLifecycleCall request completion progress)

recoverEndProcessRuntime :: ApplicationConfiguration -> Lifecycle.LifecycleRequestId -> IO (Either ApplicationRuntimeFailure ApplicationLifecycleCompletion)
recoverEndProcessRuntime configuration request = mask $ \restore -> do
  context <- newRuntimeContext
  completion <- newEmptyTMVarIO
  progress <- newTVarIO Nothing
  assigned <- newEmptyTMVarIO
  atomically (modifyTVar' context.runtimeLifecycleCalls (Map.insert (Client.applicationInvocationId 0) (completion, progress, assigned)))
  let ApplicationLivenessConfiguration recovery _ _ = applicationConfigurationLiveness configuration
      initial = Client.initialLifecycleRecoveryClient recovery (applicationConfigurationAttachment configuration) request
  ( do
      startRuntimeWorkersFrom configuration context initial
      atomically (writeTQueue context.runtimeInput Start)
      restore (atomically ((Right <$> readTMVar completion) `orElse` (Left <$> readTMVar context.runtimeFailure)))
    )
    `finally` stopRuntime context
