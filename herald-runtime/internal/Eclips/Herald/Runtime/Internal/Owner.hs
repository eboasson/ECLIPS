{-# LANGUAGE NumericUnderscores #-}

-- | Composition root for the exclusive owner, dispatcher, and lane workers.
module Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    RuntimeRetentionCounts (..),
    defaultRuntimeHooks,
    startHeraldRuntime,
    startHeraldRuntimeWithHooks,
    withHeraldRuntimeWithoutTraceHistory,
    withHeraldRuntimeWithHooks,
    deduplicateDiscardedWorkTickets,
    partitionPeerDialCancellation,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkFinally,
    forkIOWithUnmask,
    killThread,
    myThreadId,
    newMVar,
    threadDelay,
    withMVar,
  )
import Control.Concurrent.STM
  ( STM,
    TMVar,
    TQueue,
    TVar,
    atomically,
    check,
    flushTQueue,
    modifyTVar',
    newEmptyTMVar,
    newTQueue,
    newTVar,
    orElse,
    peekTQueue,
    putTMVar,
    readTMVar,
    readTQueue,
    readTVar,
    registerDelay,
    retry,
    stateTVar,
    tryPutTMVar,
    tryReadTMVar,
    writeTQueue,
    writeTVar,
  )
import Control.Exception
  ( AsyncException (..),
    SomeAsyncException,
    SomeException,
    evaluate,
    finally,
    fromException,
    mask,
    mask_,
    onException,
    throwIO,
    try,
    uninterruptibleMask_,
  )
import Control.Monad
  ( forM,
    forM_,
    forever,
    unless,
    void,
    when,
  )
import Data.ByteString (ByteString)
import Data.IORef
  ( IORef,
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.List (find, partition)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Unique (newUnique)
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle (HeraldLocator)
import Eclips.Domain.Disappearance (DisappearanceProbeId)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.Topology (MemberSetDigest)
import Eclips.Herald.Administration
  ( AdminCorrelationId,
    AdministrationBinding,
    DrainId,
    FinalAdminReply (..),
    checkedInitialAdministrationBinding,
  )
import Eclips.Herald.Administration.RPC
  ( AdministrationIngressContext (..),
    AdministrationOutbound (..),
    admitAdministrationClientDto,
    projectAdministrationEffect,
    projectFinalAdministrationReply,
  )
import Eclips.Herald.Application.RPC
  ( ApplicationIngressContext (..),
    ApplicationOutbound (..),
    admitApplicationClientDto,
    projectApplicationEffect,
  )
import Eclips.Herald.Application.Recovery (ApplicationRecoveryConfiguration)
import Eclips.Herald.Application.Session
  ( ApplicationSessionBinding,
    sessionAcceptanceBinding,
    sessionBindingSessionId,
  )
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks)
import Eclips.Herald.Diagnostics (heraldAlignmentInventory, heraldLabelWorkCounts, heraldPublicationGroupInventory, heraldStructuralReportCounts)
import Eclips.Herald.Discovery
  ( PeerAddress,
    PeerBinding,
    PeerCandidate,
    PeerDialIntent,
    PeerHello,
    PeerHelloDisposition (..),
    peerBindingRemoteHeraldEpoch,
    peerCandidateConnectionNonce,
    peerCandidateOpened,
    peerDialIntentCancellationKey,
    peerHelloConnectionNonce,
  )
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    PeerProtocolDisposition,
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.IdGenerator
  ( GeneratorSeed,
    mkGeneratorSeed,
  )
import Eclips.Herald.Initialization
  ( HeraldState,
    configureHeraldDiagnosticChecks,
    initialHerald,
    initialHeraldWithIsolation,
    initialHeraldWithOracleVoters,
    initialHeraldWithTimingAndIsolation,
  )
import Eclips.Herald.Input
  ( AdministrationIngress (..),
    CandidateAdministrationLane,
    CandidateApplicationLane,
    DisappearanceIngress (AbortDisappearanceProbe),
    HeraldInputBody (..),
    PeerControl,
    PeerIngress (..),
    RuntimeObservation (..),
    heraldInput,
    peerPublicationReceived,
  )
import Eclips.Herald.Isolation (IsolationConfiguration)
import Eclips.Herald.OracleClient
  ( OracleClientAction (..),
    OracleClientIngress,
    OracleContact,
    OracleContactSet,
    OracleHelloClaims,
  )
import Eclips.Herald.OracleHealth (OracleHealthObservation)
import Eclips.Herald.Peer.RPC
  ( PeerRpcProjectionFault,
    projectPeerControl,
    projectPeerControlWithProgress,
    projectPeerHello,
    projectPeerLogicalAttempt,
    projectPeerLogicalAttemptWithProgress,
  )
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome (..),
    PeerDispatchTicket,
    PeerLogicalAttempt,
    PeerLogicalItem,
    peerDispatchTicketDestinationHeraldEpoch,
  )
import Eclips.Herald.PeerLiveness (PeerRecoveryConfiguration)
import Eclips.Herald.Runtime.Internal.Arbiter
  ( Arbiter,
    ArbiterSelection (..),
    SourceFamily (..),
    SourceId (..),
    enqueueArbiter,
    newArbiter,
    pauseArbiterSource,
    removeArbiterSource,
    resumeArbiterSource,
    selectArbiter,
  )
import Eclips.Herald.Runtime.Internal.Connection
  ( ApplicationPhase (..),
    ConfiguredAdministrationPhase (..),
    ConnectionSlot (..),
    PeerPhase (..),
    Registry,
    WriterCommand (..),
    allocateAdministration,
    allocateApplication,
    allocateConfiguredAdministration,
    allocatePeer,
    claimApplicationSessionDisposal,
    findApplicationBinding,
    findConfiguredAdministrationBinding,
    findPeerBinding,
    lookupCurrentSlot,
    newRegistry,
    registrySlots,
    removeCurrentSlot,
    replaceSlot,
    slotCloseAction,
    slotRetirementAction,
    slotSource,
    slotWriterQueue,
  )
import Eclips.Herald.Runtime.Internal.Coordination
  ( BatchEnvelope (..),
    BatchOrdinal (..),
    CompletionCorrelation (..),
    CompletionOrdinal (..),
    DispositionExpectation (..),
    DispositionRoute (..),
    DrainLedger,
    EffectMemberOrdinal (..),
    IngressEnvelope (..),
    ObligationOrigin (..),
    OutcomeCellState (..),
    RuntimePhase (..),
    StepOrdinal (..),
    TransferBinding (..),
    advanceRoutedThrough,
    awaitPhysicalOutcome,
    claimOutcomeCell,
    dispositionExpectationFor,
    drainBarrierReady,
    emptyDrainLedger,
    markObligationKernelStepped,
    queueObligationObservation,
    registerObligation,
    sealDrainLedger,
    settleObligationByOffer,
    validateDispositionBatch,
  )
import Eclips.Herald.Runtime.Internal.Recording (RuntimeTraceFinalEvidence (..))
import Eclips.Herald.Runtime.Internal.Timer
  ( RuntimeTimerClock (..),
    RuntimeTimerCommand (..),
    RuntimeTimerScheduler,
    runRuntimeTimerManager,
    systemRuntimeTimerScheduler,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( ConnectionPromotion (..),
    KernelTraceStep (..),
    PhysicalConnectionName (..),
    PhysicalConnectionRef (..),
    PhysicalLaneClass (..),
    RuntimeWorkerClass (..),
    ShellTraceEvent (..),
    TraceCaptureMode (..),
    TraceLedger,
    addTraceWorkCounts,
    appendKernelTrace,
    appendShellTrace,
    newTraceLedgerWithMode,
    newTraceLedgerWithWorkCounts,
    snapshotTraceWorkCounts,
    traceLedgerCapturesHistory,
  )
import Eclips.Herald.Runtime.Internal.Types
  ( AdministrationPlane,
    ApplicationPlane,
    ConfiguredAdministrationPlane,
    ConnectionRef (..),
    HeraldRuntime (..),
    HeraldRuntimeConfiguration (..),
    HeraldRuntimeExit (..),
    HeraldRuntimeFailure (..),
    HeraldRuntimeFailureClass (..),
    HeraldRuntimeHandlers (..),
    PeerPlane,
    RuntimeAdministrationConnectionHandlers (..),
    RuntimeAdministrationRegistration (..),
    RuntimeApplicationConnectionHandlers (..),
    RuntimeConfiguredAdministrationConnectionHandlers (..),
    RuntimeDiagnostic (..),
    RuntimeDiagnosticClass (..),
    RuntimeGeneratorSeedSource (..),
    RuntimeLaneOffer (..),
    RuntimeMonotonicClock (..),
    RuntimePeerConnectionHandlers (..),
    RuntimePeerWorkDelay (..),
    RuntimeRegistration (..),
    RuntimeSubmission (..),
    RuntimeToken (..),
    connectionRefOrdinal,
    connectionRefToken,
  )
import Eclips.Herald.Time qualified as Time
import Eclips.Herald.Timer (TimerAttempt, TimerOutcome)
import Eclips.Herald.Transition (heraldOracleReplicaProjection, stepHerald)
import Eclips.Oracle.Voter (OracleReplicaRegistration, VoterConfiguration, replicaRegistrationNode)
import Eclips.Protocol.Admin.Types (AdminClientDto)
import Eclips.Protocol.Application.Types (ApplicationClientDto)
import Eclips.Protocol.Peer.Types (PeerEnvelope)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import Eclips.Public.Types.Timing (TakeoverTarget)
import Eclips.Raft.Identity (RaftNodeId)

data RuntimeContext = RuntimeContext
  { contextClock :: IO Time.MonotonicInstant,
    contextHooks :: RuntimeHooks,
    contextAdministrationBinding :: AdministrationBinding,
    contextDelayMicroseconds :: Word64,
    contextArbiter :: Arbiter IngressEnvelope,
    contextRegistry :: Registry,
    contextPubliclyClosed :: TVar (Set PhysicalConnectionName),
    contextBatchQueue :: TQueue BatchEnvelope,
    contextTimerCommands :: TQueue RuntimeTimerCommand,
    contextPeerDialQueue :: TQueue PeerDialIntent,
    contextPeerDialSink :: Maybe (PeerDialIntent -> IO ()),
    contextPeerDialCancelSink :: Maybe (PeerDialIntent -> IO ()),
    contextOracleActionSink :: Maybe (OracleClientAction -> IO ()),
    contextOracleHealthSink :: Maybe (Word64 -> Word64 -> OracleHelloClaims -> [OracleContact] -> IO ()),
    contextPeerDialHanded :: TVar (Maybe PeerDialIntent),
    contextDiagnosticQueue :: TQueue RuntimeDiagnostic,
    contextDiagnosticEnabled :: TVar Bool,
    contextDiagnosticHandler :: RuntimeDiagnostic -> IO (),
    contextTrace :: TraceLedger,
    contextWorkCountsSink :: Maybe (Bool -> [(String, Word64)] -> IO ()),
    contextPhase :: TVar RuntimePhase,
    contextTerminalRequest :: TMVar (Either HeraldRuntimeFailure HeraldRuntimeExit),
    contextExit :: TMVar (Either HeraldRuntimeFailure HeraldRuntimeExit),
    contextSupervisorDone :: TMVar (),
    contextThreads :: TVar [RuntimeWorker],
    contextSpawnsInFlight :: TVar Word64,
    contextNextBatch :: TVar Word64,
    contextNextStep :: TVar Word64,
    contextNextCompletion :: TVar Word64,
    contextCompletionSteppedThrough :: TVar (Maybe CompletionOrdinal),
    contextResolvedCompletions :: TVar (Set CompletionOrdinal),
    contextDrainPending :: TVar Bool,
    contextDrainLedger :: TVar DrainLedger,
    contextDrainCut :: TVar (Maybe BatchOrdinal),
    contextCompletionSealed :: TVar Bool,
    contextDrainBarrier :: TVar (Maybe (DrainId, BatchOrdinal, Bool)),
    contextRemoteBindings :: TVar (Map.Map HeraldEpoch PeerBinding),
    contextDormantWork :: TVar (Map.Map HeraldEpoch [PeerDispatchTicket]),
    contextDelayedQueue :: TQueue (Word64, HeraldEpoch, PeerDispatchTicket),
    contextDelayedSlots :: TVar (Map.Map HeraldEpoch [(Word64, PeerDispatchTicket)]),
    contextNextDelay :: TVar Word64,
    contextOutcomeCells :: TVar [(PhysicalConnectionRef, SourceId, ObligationOrigin, PeerLogicalAttempt, TVar OutcomeCellState)],
    contextTransferReservations :: TVar (Map.Map (TransferBinding, BatchOrdinal) DispositionRoute),
    contextBindingLosses :: TVar (Map.Map TransferBinding BindingLossState),
    contextHeldBindingLossOrigins :: TVar (Map.Map TransferBinding HeldBindingLoss),
    contextCancelledDestinations :: TVar (Set HeraldEpoch),
    contextNextJoinCorrelation :: TVar Word64,
    contextJoinReplies :: TVar (Map.Map Word64 (TMVar ByteString)),
    contextOracleReplicas :: TVar (Map.Map RaftNodeId OracleReplicaRegistration)
  }

data RuntimeWorker = RuntimeWorker RuntimeWorkerClass (Maybe PhysicalConnectionRef) ThreadId (TMVar ()) (TVar RuntimeWorkerPhase)

data RuntimeWorkerPhase
  = WorkerRunning
  | WorkerStopRequested
  | WorkerClosing
  | WorkerFinished
  | WorkerJoined
  deriving stock (Eq)

data SelectedIngress = SelectedIngress
  { selectedBody :: HeraldInputBody,
    selectedDispositionRoute :: Maybe DispositionRoute,
    selectedCompletion :: Maybe (CompletionOrdinal, CompletionCorrelation)
  }

data BindingLossState
  = BindingLossPending SourceId CompletionOrdinal [ObligationOrigin]
  | BindingLossSelected CompletionOrdinal [ObligationOrigin]
  | BindingLossStepped BatchOrdinal

newtype HeldBindingLoss = HeldBindingLoss [ObligationOrigin]

-- The writer worker returns the optional drain acknowledgement to its wrapper.
-- The wrapper first invalidates the registry entry and only then runs the
-- potentially blocking close callback, so no caller can observe a dead current
-- route and a drain acknowledgement cannot overtake physical close.
newtype WriterTermination = WriterTermination (Maybe (TMVar ()))

data WriterLoopResult
  = WriterLoopStopped WriterTermination
  | WriterCommandFailed (Maybe (TMVar ())) SomeException

slotPhysicalRef :: ConnectionSlot -> PhysicalConnectionRef
slotPhysicalRef = \case
  ApplicationSlot reference _ _ _ -> PhysicalApplicationRef reference
  AdministrationSlot reference _ _ -> PhysicalAdministrationRef reference
  ConfiguredAdministrationSlot reference _ _ _ ->
    PhysicalConfiguredAdministrationRef reference
  PeerSlot reference _ _ _ -> PhysicalPeerRef reference

-- | Package-private observations of shell ownership, never kernel state.
data RuntimeRetentionCounts = RuntimeRetentionCounts
  { retainedWorkers :: Int,
    retainedBindingLosses :: Int,
    retainedOutcomeCells :: Int,
    retainedPublicCloses :: Int
  }
  deriving stock (Eq, Show)

data RuntimeHooks = RuntimeHooks
  { hookRuntimeInitialized :: Time.MonotonicInstant -> OracleContactSet -> GeneratorSeed -> ApplicationRecoveryConfiguration -> PeerRecoveryConfiguration -> EffectBatch -> TraceLedger -> IO (),
    hookRuntimeEffectInjector :: (EffectBatch -> IO ()) -> IO (),
    hookBeforeDispatchBatch :: BatchEnvelope -> IO (),
    hookAfterDispatchBatch :: BatchEnvelope -> IO (),
    hookBeforePeerDialSink :: PeerDialIntent -> IO (),
    hookDelayPeerWork :: PeerDispatchTicket -> Word64 -> IO (),
    hookPublishFinalEvidence :: RuntimeTraceFinalEvidence -> IO (),
    hookBeforeConnectionWriterBody :: PhysicalConnectionRef -> IO (),
    hookAfterWriterCommandDequeued :: PhysicalConnectionRef -> IO (),
    hookRuntimeRetentionReader :: IO RuntimeRetentionCounts -> IO (),
    hookTimerScheduler :: RuntimeTimerScheduler
  }

defaultRuntimeHooks :: RuntimeHooks
defaultRuntimeHooks =
  RuntimeHooks
    { hookRuntimeInitialized = \_ _ _ _ _ _ _ -> pure (),
      hookRuntimeEffectInjector = const (pure ()),
      hookBeforeDispatchBatch = const (pure ()),
      hookAfterDispatchBatch = const (pure ()),
      hookBeforePeerDialSink = const (pure ()),
      hookDelayPeerWork = \_ -> threadDelay . fromIntegral,
      hookPublishFinalEvidence = const (pure ()),
      hookBeforeConnectionWriterBody = const (pure ()),
      hookAfterWriterCommandDequeued = const (pure ()),
      hookRuntimeRetentionReader = const (pure ()),
      hookTimerScheduler = systemRuntimeTimerScheduler
    }

-- | Run one Herald runtime with package-private lifecycle hooks. The public
-- runtime and TCP facades delegate to this same structured scope so hook-based
-- tests retain ordinary startup, terminal arbitration, and join semantics.
withHeraldRuntimeWithHooks ::
  RuntimeHooks ->
  HeraldRuntimeConfiguration ->
  (HeraldRuntime -> IO result) ->
  IO (Either HeraldRuntimeFailure (result, HeraldRuntimeExit))
withHeraldRuntimeWithHooks = withHeraldRuntimeWithTraceMode CaptureExactHistory

-- | Ordinary public scope: allocate causal ordinals without retaining the
-- package-private conformance transcript. The hook runner keeps full capture.
withHeraldRuntimeWithoutTraceHistory ::
  HeraldRuntimeConfiguration ->
  (HeraldRuntime -> IO result) ->
  IO (Either HeraldRuntimeFailure (result, HeraldRuntimeExit))
withHeraldRuntimeWithoutTraceHistory =
  withHeraldRuntimeWithTraceMode OrdinalsOnly defaultRuntimeHooks

withHeraldRuntimeWithTraceMode ::
  TraceCaptureMode ->
  RuntimeHooks ->
  HeraldRuntimeConfiguration ->
  (HeraldRuntime -> IO result) ->
  IO (Either HeraldRuntimeFailure (result, HeraldRuntimeExit))
withHeraldRuntimeWithTraceMode capture hooks configuration use = mask $ \restore -> do
  started <- startHeraldRuntimeWithTraceMode capture hooks configuration
  case started of
    Left failure -> pure (Left failure)
    Right runtime -> do
      callbackResult <- atomically newEmptyTMVar
      callbackThread <-
        forkFinally
          (restore (use runtime))
          (atomically . putTMVar callbackResult)
      runtimeResult <- atomically newEmptyTMVar
      _runtimeWaiter <-
        forkFinally
          (runtimeAwaitExit runtime)
          (atomically . putTMVar runtimeResult)
      onException
        ( do
            first <-
              atomically
                ( (Left <$> readTMVar callbackResult)
                    `orElse` (Right <$> readTMVar runtimeResult)
                )
            case first of
              Left callback -> finishRuntimeScopeCallback runtime runtimeResult callback
              Right (Left waiterException) -> do
                killThread callbackThread
                void (atomically (readTMVar callbackResult))
                runtimeCloseScope runtime
                void (atomically (readTMVar runtimeResult))
                throwIO waiterException
              Right (Right (Left failure)) -> do
                killThread callbackThread
                void (atomically (readTMVar callbackResult))
                void (atomically (readTMVar runtimeResult))
                runtimeCloseScope runtime
                pure (Left failure)
              Right (Right (Right runtimeExit)) -> do
                void (atomically (readTMVar runtimeResult))
                callback <- atomically (readTMVar callbackResult)
                case callback of
                  Left callbackException -> do
                    runtimeCloseScope runtime
                    throwIO callbackException
                  Right result -> do
                    runtimeCloseScope runtime
                    pure (Right (result, runtimeExit))
        )
        ( cancelAndJoinRuntimeScope
            runtime
            callbackThread
            callbackResult
            runtimeResult
        )

cancelAndJoinRuntimeScope ::
  HeraldRuntime ->
  ThreadId ->
  TMVar (Either SomeException result) ->
  TMVar (Either SomeException (Either HeraldRuntimeFailure HeraldRuntimeExit)) ->
  IO ()
cancelAndJoinRuntimeScope runtime callbackThread callbackResult runtimeResult = do
  killThread callbackThread
  void (atomically (readTMVar callbackResult))
  runtimeCloseScope runtime
  void (atomically (readTMVar runtimeResult))

finishRuntimeScopeCallback ::
  HeraldRuntime ->
  TMVar (Either SomeException (Either HeraldRuntimeFailure HeraldRuntimeExit)) ->
  Either SomeException result ->
  IO (Either HeraldRuntimeFailure (result, HeraldRuntimeExit))
finishRuntimeScopeCallback runtime runtimeResult = \case
  Left callbackException -> do
    runtimeCloseScope runtime
    void (atomically (readTMVar runtimeResult))
    throwIO callbackException
  Right result -> do
    runtimeCloseScope runtime
    observed <- atomically (readTMVar runtimeResult)
    case observed of
      Left waiterException -> throwIO waiterException
      Right observedExit -> case observedExit of
        Left failure -> pure (Left failure)
        Right runtimeExit -> pure (Right (result, runtimeExit))

startHeraldRuntime ::
  HeraldRuntimeConfiguration ->
  IO (Either HeraldRuntimeFailure HeraldRuntime)
startHeraldRuntime = startHeraldRuntimeWithTraceMode OrdinalsOnly defaultRuntimeHooks

startHeraldRuntimeWithHooks ::
  RuntimeHooks ->
  HeraldRuntimeConfiguration ->
  IO (Either HeraldRuntimeFailure HeraldRuntime)
startHeraldRuntimeWithHooks = startHeraldRuntimeWithTraceMode CaptureExactHistory

startHeraldRuntimeWithTraceMode ::
  TraceCaptureMode ->
  RuntimeHooks ->
  HeraldRuntimeConfiguration ->
  IO (Either HeraldRuntimeFailure HeraldRuntime)
startHeraldRuntimeWithTraceMode
  capture
  hooks
  ( HeraldRuntimeConfiguration
      genesis
      bootstraps
      contacts
      (RuntimeGeneratorSeedSource seedSource)
      (RuntimeMonotonicClock clock)
      delayConfiguration
      applicationRecoveryConfiguration
      peerRecoveryConfiguration
      isolationConfiguration
      (HeraldRuntimeHandlers diagnostic)
      peerDialSink
      peerDialCancelSink
      oracleActionSink
      oracleVoters
      oracleHealthSink
      timingTarget
      workCountsSink
      diagnosticChecks
    ) = mask $ \restore -> do
    acquired <- try (restore seedSource)
    case acquired of
      Left (exception :: SomeException)
        | Just (_ :: SomeAsyncException) <- fromException exception -> throwIO exception
        | otherwise -> pure (Left initializationFailure)
      Right seedBytes -> case mkGeneratorSeed seedBytes of
        Left _ -> pure (Left initializationFailure)
        Right seed -> startWithSeed restore seed
    where
      initializationFailure = HeraldRuntimeFailure HeraldRuntimeInitializationFailure

      startWithSeed restore seed = do
        token <- RuntimeToken <$> newUnique
        clockGate <- newMVar ()
        let serializedClock = withMVar clockGate (const clock)
        let administrationBinding = checkedInitialAdministrationBinding genesis
        context <-
          atomically
            ( newRuntimeContext
                capture
                workCountsSink
                token
                administrationBinding
                serializedClock
                hooks
                (runtimePeerWorkDelayMicros delayConfiguration)
                diagnostic
                peerDialSink
                peerDialCancelSink
                oracleActionSink
                oracleHealthSink
            )
        startupGate <- atomically newEmptyTMVar
        startupResult <- atomically newEmptyTMVar
        _ <-
          spawnEssential context OwnerWorker $ do
            atomically (readTMVar startupGate)
            initializeOwner
              context
              genesis
              bootstraps
              contacts
              seed
              applicationRecoveryConfiguration
              peerRecoveryConfiguration
              isolationConfiguration
              oracleVoters
              timingTarget
              diagnosticChecks
              startupResult
        _ <- spawnEssential context DispatcherWorker (dispatcherLoop context)
        _ <-
          spawnEssential
            context
            TimerWorker
            ( runRuntimeTimerManager
                (hookTimerScheduler hooks)
                (RuntimeTimerClock serializedClock)
                (contextTimerCommands context)
                (queueTimerOutcome context)
            )
        _ <- spawnEssential context DelayedWorkWorker (delayedWorkLoop context)
        _ <- spawnEssential context PeerDialSinkWorker (peerDialSinkLoop context)
        _ <- spawnTracked context DiagnosticWorker (diagnosticLoop context)
        _ <- spawnSupervisor context
        atomically (putTMVar startupGate ())
        result <-
          restore (atomically (readTMVar startupResult))
            `onException` closeScope context
        case result of
          Left failure -> do
            void (awaitPublicExit context)
            pure (Left failure)
          Right () -> pure (Right (runtimeHandle context))

initializeOwner ::
  RuntimeContext ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  OracleContactSet ->
  GeneratorSeed ->
  ApplicationRecoveryConfiguration ->
  PeerRecoveryConfiguration ->
  Maybe (Set HeraldEpoch, IsolationConfiguration) ->
  Maybe (VoterConfiguration, [OracleReplicaRegistration]) ->
  Maybe TakeoverTarget ->
  DiagnosticChecks ->
  TMVar (Either HeraldRuntimeFailure ()) ->
  IO ()
initializeOwner context genesis bootstraps contacts seed applicationRecoveryConfiguration peerRecoveryConfiguration isolationConfiguration oracleVoters timingTarget diagnosticChecks startupResult = do
  initialized <-
    try $ do
      observedAt <- contextClock context
      result <-
        evaluate
          ( case (timingTarget, oracleVoters, isolationConfiguration) of
              (Just target, _, _) ->
                initialHeraldWithTimingAndIsolation isolationConfiguration target oracleVoters observedAt genesis bootstraps contacts seed
              (Nothing, Just (configuration, registrations), _) ->
                initialHeraldWithOracleVoters configuration registrations observedAt genesis bootstraps contacts seed applicationRecoveryConfiguration peerRecoveryConfiguration (snd <$> isolationConfiguration)
              (Nothing, Nothing, Nothing) ->
                initialHerald observedAt genesis bootstraps contacts seed applicationRecoveryConfiguration peerRecoveryConfiguration
              (Nothing, Nothing, Just (voterHosts, isolation)) ->
                initialHeraldWithIsolation observedAt genesis bootstraps contacts seed applicationRecoveryConfiguration peerRecoveryConfiguration voterHosts isolation
          )
      case result of
        Left fault -> pure (Left fault)
        Right (checkedInitialState, initialBatch) -> do
          let initialState = configureHeraldDiagnosticChecks diagnosticChecks checkedInitialState
          atomically $ do
            publishOracleRegistrations context initialState
            void (appendKernelTrace (contextTrace context) (KernelTraceInitialized (BatchOrdinal 0) initialBatch))
            void (appendShellTrace (contextTrace context) (ShellBatchHanded (BatchOrdinal 0)))
            writeTQueue (contextBatchQueue context) (BatchEnvelope (BatchOrdinal 0) initialBatch Nothing Nothing)
            writeTVar (contextNextBatch context) 1
            writeTVar (contextPhase context) RuntimeRunning
          hookRuntimeInitialized
            (contextHooks context)
            observedAt
            contacts
            seed
            applicationRecoveryConfiguration
            peerRecoveryConfiguration
            initialBatch
            (contextTrace context)
          hookRuntimeEffectInjector (contextHooks context) (injectEffectBatch context)
          hookRuntimeRetentionReader (contextHooks context) (atomically (runtimeRetentionCounts context))
          pure (Right initialState)
  case initialized of
    Left (exception :: SomeException)
      | Just ThreadKilled <- fromException exception -> throwIO exception
      | otherwise -> initializationFailed
    Right (Left fault) -> do
      _ <- try (hookPublishFinalEvidence (contextHooks context) (RuntimeTraceFinalInvariantFault fault)) :: IO (Either SomeException ())
      initializationFailed
    Right (Right initialState) -> do
      atomically (putTMVar startupResult (Right ()))
      ownerLoop context initialState
        `onException` hookPublishFinalEvidence (contextHooks context) (RuntimeTraceFinalState initialState)
  where
    initializationFailed = do
      let failure = HeraldRuntimeFailure HeraldRuntimeInitializationFailure
      atomically $ do
        failRuntime context HeraldRuntimeInitializationFailure
        void (tryPutTMVar startupResult (Left failure))
      -- The supervisor owns cancellation and join. Remaining blocked here keeps
      -- an expected startup failure from looking like an unexpected normal
      -- return of the essential owner.
      atomically retry

-- Package-private deterministic effect injection used only by runtime tests.
-- The injected batch enters the ordinary serialized dispatcher and therefore
-- exercises exactly the production effect interpreters.
injectEffectBatch :: RuntimeContext -> EffectBatch -> IO ()
injectEffectBatch context batch = atomically $ do
  phase <- readTVar (contextPhase context)
  case phase of
    RuntimeRunning -> do
      ordinal <- nextBatchOrdinal context
      void (appendShellTrace (contextTrace context) (ShellBatchHanded ordinal))
      writeTQueue (contextBatchQueue context) (BatchEnvelope ordinal batch Nothing Nothing)
    _ -> pure ()

newRuntimeContext ::
  TraceCaptureMode ->
  Maybe (Bool -> [(String, Word64)] -> IO ()) ->
  RuntimeToken ->
  AdministrationBinding ->
  IO Time.MonotonicInstant ->
  RuntimeHooks ->
  Word64 ->
  (RuntimeDiagnostic -> IO ()) ->
  Maybe (PeerDialIntent -> IO ()) ->
  Maybe (PeerDialIntent -> IO ()) ->
  Maybe (OracleClientAction -> IO ()) ->
  Maybe (Word64 -> Word64 -> OracleHelloClaims -> [OracleContact] -> IO ()) ->
  STM RuntimeContext
newRuntimeContext capture workCountsSink token administrationBinding clock hooks delay diagnostic peerDialSink peerDialCancelSink oracleActionSink oracleHealthSink = do
  arbiter <- newArbiter
  registry <- newRegistry token (HeraldRuntimeHandlers diagnostic)
  publiclyClosed <- newTVar Set.empty
  RuntimeContext clock hooks administrationBinding delay arbiter registry publiclyClosed
    <$> newTQueue
    <*> newTQueue
    <*> newTQueue
    <*> pure peerDialSink
    <*> pure peerDialCancelSink
    <*> pure oracleActionSink
    <*> pure oracleHealthSink
    <*> newTVar Nothing
    <*> newTQueue
    <*> newTVar True
    <*> pure diagnostic
    <*> (case workCountsSink of Nothing -> newTraceLedgerWithMode capture; Just _ -> newTraceLedgerWithWorkCounts capture)
    <*> pure workCountsSink
    <*> newTVar RuntimeStarting
    <*> newEmptyTMVar
    <*> newEmptyTMVar
    <*> newEmptyTMVar
    <*> newTVar []
    <*> newTVar 0
    <*> newTVar 1
    <*> newTVar 0
    <*> newTVar 1
    <*> newTVar (Just (CompletionOrdinal 0))
    <*> newTVar Set.empty
    <*> newTVar False
    <*> newTVar emptyDrainLedger
    <*> newTVar Nothing
    <*> newTVar False
    <*> newTVar Nothing
    <*> newTVar Map.empty
    <*> newTVar Map.empty
    <*> newTQueue
    <*> newTVar Map.empty
    <*> newTVar 0
    <*> newTVar []
    <*> newTVar Map.empty
    <*> newTVar Map.empty
    <*> newTVar Map.empty
    <*> newTVar Set.empty
    <*> newTVar 0
    <*> newTVar Map.empty
    <*> newTVar Map.empty

runtimeHandle :: RuntimeContext -> HeraldRuntime
runtimeHandle context =
  HeraldRuntime
    { runtimeCapturesTraceHistory = traceLedgerCapturesHistory (contextTrace context),
      runtimeRegisterApplication = registerApplication context,
      runtimeRegisterAdministration = registerAdministration context,
      runtimeRegisterConfiguredAdministration = registerConfiguredAdministration context,
      runtimeRegisterPeer = registerPeer context,
      runtimeCloseConnection = closeConnection context,
      runtimeSubmitApplication = submitApplication context,
      runtimeSubmitConfiguredAdministration = submitConfiguredAdministration context,
      runtimeOpenPeer = submitPeerOpen context,
      runtimeSubmitPeerHello = submitPeerHelloIngress context,
      runtimeSubmitPeerControlWithProgress = submitPeerControlWithProgressIngress context,
      runtimeSubmitPeerPublicationWithProgress = submitPeerPublicationWithProgressIngress context,
      runtimeSubmitPeerControl = submitPeerControlIngress context,
      runtimeSubmitPeerPublication = submitPeerPublicationIngress context,
      runtimeSubmitOracle = submitOracleIngress context,
      runtimeSubmitOracleHealthRound = submitOracleHealthRound context,
      runtimeExchangeJoin = exchangeJoin context,
      runtimeAwaitOracleReplicaRegistration = awaitOracleReplicaRegistration context,
      runtimeSubmitDisappearanceAbort = submitDisappearanceAbort context,
      runtimeReportRetiredEpochRejection = submitRetiredEpochRejection context,
      runtimeRequestDrain = submitDrain context (contextAdministrationBinding context),
      runtimeAwaitExit = awaitPublicExit context,
      runtimeTryExit = tryPublicExit context,
      runtimeCloseScope = closeScope context
    }

runtimeRetentionCounts :: RuntimeContext -> STM RuntimeRetentionCounts
runtimeRetentionCounts context =
  RuntimeRetentionCounts
    <$> (length <$> readTVar (contextThreads context))
    <*> (Map.size <$> readTVar (contextBindingLosses context))
    <*> (length <$> readTVar (contextOutcomeCells context))
    <*> (Set.size <$> readTVar (contextPubliclyClosed context))

ownerLoop :: RuntimeContext -> HeraldState -> IO ()
ownerLoop context initialState = mask $ \restore -> go restore initialState
  where
    go :: (forall value. IO value -> IO value) -> HeraldState -> IO ()
    go restore predecessor = do
      successor <- iteration restore predecessor `onException` publishState predecessor
      forM_ successor (go restore)

    iteration ::
      (forall value. IO value -> IO value) ->
      HeraldState ->
      IO (Maybe HeraldState)
    iteration restore predecessor = do
      (source, selected) <-
        restore
          ( atomically $ do
              ArbiterSelection selectedSource selectedEnvelope <- selectArbiter (contextArbiter context)
              admitted <- admitSelected context selectedSource selectedEnvelope
              pure (selectedSource, admitted)
          )
      case selected of
        Nothing -> pure (Just predecessor)
        Just selection -> do
          let body = selectedBody selection
          observedAt <- restore (contextClock context)
          let input = heraldInput observedAt body
          case stepHerald input predecessor of
            Left fault -> do
              atomically $ do
                stepOrdinal <- nextStepOrdinal context
                void (appendKernelTrace (contextTrace context) (KernelTraceStepped stepOrdinal source input (Left fault)))
                failRuntime context HeraldRuntimeKernelInvariantFailure
              hookPublishFinalEvidence (contextHooks context) (RuntimeTraceFinalInvariantFault fault)
              pure Nothing
            Right (successor, batch) -> do
              workCounts <- prepareOwnerWorkCounts context (heraldStructuralReportCounts predecessor batch)
              case dispositionExpectationFor body of
                Just expectation
                  | not (validateDispositionBatch expectation batch)
                      || not (routeMatchesExpectation expectation (selectedDispositionRoute selection)) ->
                      do
                        atomically $ do
                          stepOrdinal <- nextStepOrdinal context
                          void (appendKernelTrace (contextTrace context) (KernelTraceStepped stepOrdinal source input (Right batch)))
                          addTraceWorkCounts (contextTrace context) workCounts
                          failRuntime context HeraldRuntimeCoordinationFailure
                        publishState successor
                        pure Nothing
                expectation -> do
                  atomically $ do
                    when (fst (heraldOracleReplicaProjection predecessor) /= fst (heraldOracleReplicaProjection successor))
                      $ publishOracleRegistrations context successor
                    stepOrdinal <- nextStepOrdinal context
                    batchOrdinal <- nextBatchOrdinal context
                    void (appendKernelTrace (contextTrace context) (KernelTraceStepped stepOrdinal source input (Right batch)))
                    addTraceWorkCounts (contextTrace context) workCounts
                    settleSelectedCompletion context batchOrdinal selection
                    case batchDrainId batch of
                      Just drainId -> do
                        writeTVar (contextPhase context) (RuntimeDraining drainId)
                        writeTVar (contextDrainBarrier context) (Just (drainId, batchOrdinal, False))
                        writeTVar (contextDrainCut context) (Just batchOrdinal)
                        discardExternalIngress context
                        void (appendShellTrace (contextTrace context) (ShellDrainCut drainId batchOrdinal))
                      Nothing -> pure ()
                    installTransferReservation
                      context
                      batchOrdinal
                      (selectedDispositionRoute selection)
                      batch
                    writeTQueue
                      (contextBatchQueue context)
                      (BatchEnvelope batchOrdinal batch expectation (selectedDispositionRoute selection))
                    void (appendShellTrace (contextTrace context) (ShellBatchHanded batchOrdinal))
                    tryQueueBarrier context
                  pure (Just successor)
    publishState state = do
      counts <- prepareOwnerWorkCounts context (("alignment.inventory.snapshots", 1) : heraldAlignmentInventory state <> heraldPublicationGroupInventory state <> heraldLabelWorkCounts state)
      atomically (addTraceWorkCounts (contextTrace context) counts)
      hookPublishFinalEvidence (contextHooks context) (RuntimeTraceFinalState state)

-- Diagnostic traversals run only on the owner and outside STM. Force each
-- finite key/count completely before handoff so the ledger cannot retain a
-- closure over a kernel state. Ordinary operation does not inspect the input.
prepareOwnerWorkCounts :: RuntimeContext -> [(String, Word64)] -> IO [(String, Word64)]
prepareOwnerWorkCounts context counts = case contextWorkCountsSink context of
  Nothing -> pure []
  Just _ -> do
    evaluate (foldl' forceCount () counts)
    pure counts
  where
    forceCount () (key, count) = foldl' (\() character -> character `seq` ()) () key `seq` count `seq` ()

-- The sole kernel owner publishes current facts only after installing the
-- corresponding control prefix. Ordinary graph work does not traverse them,
-- and an unchanged registration map does not wake blocked role supervisors.
publishOracleRegistrations :: RuntimeContext -> HeraldState -> STM ()
publishOracleRegistrations context state = do
  let registrations = Map.fromList [(replicaRegistrationNode registration, registration) | registration <- snd (heraldOracleReplicaProjection state)]
  previous <- readTVar (contextOracleReplicas context)
  when (registrations /= previous) (writeTVar (contextOracleReplicas context) registrations)

awaitOracleReplicaRegistration :: RuntimeContext -> RaftNodeId -> IO (Either RuntimeSubmission OracleReplicaRegistration)
awaitOracleReplicaRegistration context node = atomically $ do
  phase <- readTVar (contextPhase context)
  case phase of
    RuntimeRunning -> do
      registrations <- readTVar (contextOracleReplicas context)
      maybe retry (pure . Right) (Map.lookup node registrations)
    RuntimeDraining _ -> pure (Left IngressClosedForDrain)
    RuntimeStoppedPhase _ -> pure (Left RuntimeStopped)
    RuntimeStarting -> retry

admitSelected :: RuntimeContext -> SourceId -> IngressEnvelope -> STM (Maybe SelectedIngress)
admitSelected context source envelope = do
  phase <- readTVar (contextPhase context)
  if externalEnvelope envelope && isDraining phase
    then do
      void (appendShellTrace (contextTrace context) (staleEnvelopeEvent envelope source))
      pure Nothing
    else admit envelope
  where
    admit = \case
      ApplicationEnvelope reference dto -> do
        current <- lookupCurrentSlot (contextRegistry context) reference
        case current of
          Just slot@(ApplicationSlot _ applicationPhase _ _) -> case applicationPhase of
            ApplicationCandidate candidate -> admitApplicationCandidate reference dto slot candidate
            ApplicationClaimPending candidate -> admitApplicationCandidate reference dto slot candidate
            ApplicationAdmissionDeferred _ -> stale
            ApplicationDispositionPending _ -> stale
            ApplicationEstablished binding -> admitApplicationEstablished reference dto slot binding
            ApplicationClosing _ -> stale
          _ -> stale
      ConfiguredAdministrationEnvelope reference dto -> do
        current <- lookupCurrentSlot (contextRegistry context) reference
        case current of
          Just slot@(ConfiguredAdministrationSlot _ administrationPhase _ _) ->
            case administrationPhase of
              ConfiguredAdministrationCandidate candidate ->
                admitConfiguredAdministrationCandidate reference dto slot candidate
              ConfiguredAdministrationDispositionPending _ -> stale
              ConfiguredAdministrationEstablished binding ->
                admitConfiguredAdministrationEstablished reference dto binding
              ConfiguredAdministrationClosing _ -> stale
          _ -> stale
      PeerOpenEnvelope reference addresses locator -> do
        current <- lookupCurrentSlot (contextRegistry context) reference
        case current of
          Just slot@(PeerSlot _ (PeerFresh nonce) _ _) -> do
            replaceSlot (contextRegistry context) (setPeerPhase (PeerCandidateRoutePending nonce) slot)
            pauseArbiterSource (contextArbiter context) source
            pure
              ( Just
                  ( SelectedIngress
                      (PeerInput (PeerCandidateOpened (peerCandidateOpened nonce addresses locator)))
                      (Just (LocalPeerCandidateRoute reference source nonce))
                      Nothing
                  )
              )
          Just (PeerSlot {}) -> rejectPeerProtocol reference
          _ -> stale
      PeerHelloEnvelope reference candidate addresses hello generation members locator -> do
        current <- lookupCurrentSlot (contextRegistry context) reference
        case current of
          Just slot@(PeerSlot _ peerPhase _ _) | helloMatchesPhase candidate hello peerPhase -> do
            replaceSlot (contextRegistry context) (setPeerPhase (PeerDispositionPending candidate) slot)
            pauseArbiterSource (contextArbiter context) source
            pure
              ( Just
                  ( SelectedIngress
                      (PeerInput (PeerHelloReceived candidate addresses hello generation members locator))
                      (Just (PeerDispositionRoute reference source candidate))
                      Nothing
                  )
              )
          Just (PeerSlot {}) -> rejectPeerProtocol reference
          _ -> stale
      PeerControlEnvelope reference binding control ->
        currentPeerBinding context source reference binding (PeerInput (PeerControlReceived binding control))
      PeerPublicationEnvelope reference binding item ->
        currentPeerBinding context source reference binding (PeerInput (peerPublicationReceived binding item))
      PeerControlWithProgressEnvelope reference binding progress control ->
        currentPeerBinding context source reference binding (PeerInput (PeerControlReceivedWithProgress binding progress control))
      PeerPublicationWithProgressEnvelope reference binding progress item ->
        currentPeerBinding context source reference binding (PeerInput (PeerPublicationReceivedWithProgress binding progress item))
      OracleEnvelope ingress ->
        pure (Just (SelectedIngress (OracleInput ingress) Nothing Nothing))
      DisappearanceAbortEnvelope probe -> pure (Just (SelectedIngress (DisappearanceInput (AbortDisappearanceProbe probe)) Nothing Nothing))
      CompletionEnvelope ordinal correlation observation ->
        admitCompletion ordinal correlation observation
      AdmittedEnvelope body -> pure (Just (SelectedIngress body Nothing Nothing))
    candidateContext slot candidate = case slot of
      ApplicationSlot _ _ (RuntimeApplicationConnectionHandlers (Just locator) _ _) _ -> CandidateApplicationLifecycleIngress candidate locator
      _ -> CandidateApplicationIngress candidate
    admitApplicationCandidate reference dto slot candidate =
      case admitApplicationClientDto (candidateContext slot candidate) dto of
        Left _ -> rejectProtocol context reference
        Right body -> do
          replaceSlot (contextRegistry context) (setApplicationPhase (ApplicationDispositionPending candidate) slot)
          pauseArbiterSource (contextArbiter context) source
          pure
            ( Just
                ( SelectedIngress
                    body
                    (Just (ApplicationDispositionRoute reference source candidate))
                    Nothing
                )
            )
    admitApplicationEstablished reference dto slot binding =
      let ingress = case slot of
            ApplicationSlot _ _ (RuntimeApplicationConnectionHandlers (Just locator) _ _) _ -> EstablishedApplicationLifecycleIngress binding locator
            _ -> EstablishedApplicationIngress binding
       in case admitApplicationClientDto ingress dto of
            Left _ -> rejectProtocol context reference
            Right body -> pure (Just (SelectedIngress body Nothing Nothing))
    admitConfiguredAdministrationCandidate reference dto slot candidate =
      case admitAdministrationClientDto
        (CandidateAdministrationIngress candidate)
        dto of
        Left _ -> rejectConfiguredAdministrationProtocol context reference
        Right body -> do
          replaceSlot
            (contextRegistry context)
            ( setConfiguredAdministrationPhase
                (ConfiguredAdministrationDispositionPending candidate)
                slot
            )
          pauseArbiterSource (contextArbiter context) source
          pure
            ( Just
                ( SelectedIngress
                    body
                    ( Just
                        ( ConfiguredAdministrationDispositionRoute
                            reference
                            source
                            candidate
                        )
                    )
                    Nothing
                )
            )
    admitConfiguredAdministrationEstablished reference dto binding =
      case admitAdministrationClientDto
        (EstablishedAdministrationIngress binding)
        dto of
        Left _ -> rejectConfiguredAdministrationProtocol context reference
        Right body -> pure (Just (SelectedIngress body Nothing Nothing))
    admitCompletion ordinal correlation observation = case correlation of
      BindingLossCompletion binding -> do
        losses <- readTVar (contextBindingLosses context)
        case Map.lookup binding losses of
          Just (BindingLossPending pendingSource pendingOrdinal origins)
            | pendingSource == source && pendingOrdinal == ordinal -> do
                writeTVar
                  (contextBindingLosses context)
                  (Map.insert binding (BindingLossSelected ordinal origins) losses)
                pure
                  ( Just
                      ( SelectedIngress
                          (RuntimeObserved observation)
                          Nothing
                          (Just (ordinal, correlation))
                      )
                  )
          _ -> stale
      _ ->
        pure
          ( Just
              ( SelectedIngress
                  (RuntimeObserved observation)
                  Nothing
                  (Just (ordinal, correlation))
              )
          )
    stale = do
      void (appendShellTrace (contextTrace context) (staleEnvelopeEvent envelope source))
      pure Nothing
    rejectPeerProtocol reference = do
      void (closeConnectionSTM context reference True)
      pure Nothing

-- Administration ingress is stamped with the checked startup binding at
-- submission, using this private wrapper to avoid storing it in public state.
submitDrain ::
  RuntimeContext ->
  AdministrationBinding ->
  ConnectionRef AdministrationPlane ->
  AdminCorrelationId ->
  IO RuntimeSubmission
submitDrain context binding reference correlation = atomically $ do
  phase <- readTVar (contextPhase context)
  pending <- readTVar (contextDrainPending context)
  current <- lookupCurrentSlot (contextRegistry context) reference
  case current of
    Nothing -> do
      let physical = PhysicalAdministrationRef reference
      void (appendShellTrace (contextTrace context) (ShellStaleSuppressed (Just (physicalName physical)) (Just (physicalRefSource physical))))
      pure StalePhysicalGeneration
    Just slot@(AdministrationSlot {}) ->
      case phase of
        RuntimeStoppedPhase _ -> pure RuntimeStopped
        RuntimeDraining _ -> pure DrainRequestAlreadyPending
        RuntimeStarting -> pure RuntimeStopped
        RuntimeRunning
          | pending -> pure DrainRequestAlreadyPending
          | otherwise -> do
              writeTVar (contextDrainPending context) True
              enqueueArbiter
                (contextArbiter context)
                (slotSource slot)
                (AdmittedEnvelope (AdministrationInput (OrderlyHeraldShutdown binding correlation)))
              pure Queued
    Just _ -> pure StalePhysicalGeneration

registerApplication :: RuntimeContext -> RuntimeApplicationConnectionHandlers -> IO (RuntimeRegistration ApplicationPlane)
registerApplication context handlers = mask $ \restore -> do
  allocated <- atomically $ do
    phase <- readTVar (contextPhase context)
    case phase of
      RuntimeRunning -> do
        allocated <- allocateApplication (contextRegistry context) handlers
        reserveRegisteredWorker context (snd allocated)
        pure (Just allocated)
      _ -> pure Nothing
  case allocated of
    Nothing -> pure RegistrationStopped
    Just (reference, slot) -> finishRegistration context restore slot (ConnectionRegistered reference)

registerConfiguredAdministration ::
  RuntimeContext ->
  RuntimeConfiguredAdministrationConnectionHandlers ->
  IO (RuntimeRegistration ConfiguredAdministrationPlane)
registerConfiguredAdministration context handlers = mask $ \restore -> do
  allocated <- atomically $ do
    phase <- readTVar (contextPhase context)
    case phase of
      RuntimeRunning -> do
        allocated <-
          allocateConfiguredAdministration (contextRegistry context) handlers
        reserveRegisteredWorker context (snd allocated)
        pure (Just allocated)
      _ -> pure Nothing
  case allocated of
    Nothing -> pure RegistrationStopped
    Just (reference, slot) ->
      finishRegistration context restore slot (ConnectionRegistered reference)

registerPeer :: RuntimeContext -> RuntimePeerConnectionHandlers -> IO (RuntimeRegistration PeerPlane)
registerPeer context handlers = mask $ \restore -> do
  allocated <- atomically $ do
    phase <- readTVar (contextPhase context)
    case phase of
      RuntimeRunning -> do
        allocated <- allocatePeer (contextRegistry context) handlers
        reserveRegisteredWorker context (snd allocated)
        pure (Just allocated)
      _ -> pure Nothing
  case allocated of
    Nothing -> pure RegistrationStopped
    Just (reference, slot) -> finishRegistration context restore slot (ConnectionRegistered reference)

finishRegistration :: RuntimeContext -> (forall value. IO value -> IO value) -> ConnectionSlot -> result -> IO result
finishRegistration context restore slot result = do
  ( do
      thread <- spawnConnection context slot
      atomically (emitDiagnostic context RuntimeConnectionDiagnostic)
      restore (pure result) `onException` rollbackRegistration context slot thread
    )
    `finally` atomically (modifyTVar' (contextSpawnsInFlight context) (\count -> count - 1))

registerAdministration :: RuntimeContext -> RuntimeAdministrationConnectionHandlers -> IO RuntimeAdministrationRegistration
registerAdministration context handlers = mask $ \restore -> do
  allocated <- atomically $ do
    phase <- readTVar (contextPhase context)
    case phase of
      RuntimeRunning -> do
        allocated <- allocateAdministration (contextRegistry context) handlers
        forM_ allocated (reserveRegisteredWorker context . snd)
        pure (Right allocated)
      _ -> pure (Left ())
  case allocated of
    Left () -> pure AdministrationRegistrationStopped
    Right Nothing -> pure AdministrationRegistrationUnavailable
    Right (Just (reference, slot)) ->
      finishRegistration context restore slot (AdministrationConnectionRegistered reference)

rollbackRegistration :: RuntimeContext -> ConnectionSlot -> ThreadId -> IO ()
rollbackRegistration context slot thread = uninterruptibleMask_ $ do
  atomically $ case slot of
    ApplicationSlot reference _ _ _ -> void (closeConnectionSTM context reference True)
    AdministrationSlot reference _ _ -> void (closeConnectionSTM context reference True)
    ConfiguredAdministrationSlot reference _ _ _ ->
      void (closeConnectionSTM context reference True)
    PeerSlot reference _ _ _ -> void (closeConnectionSTM context reference True)
  workers <- atomically (readTVar (contextThreads context))
  forM_ (workerCompletion thread workers) (atomically . readTMVar)

workerCompletion :: ThreadId -> [RuntimeWorker] -> Maybe (TMVar ())
workerCompletion _ [] = Nothing
workerCompletion sought (RuntimeWorker _ _ thread done _ : retained)
  | sought == thread = Just done
  | otherwise = workerCompletion sought retained

reserveRegisteredWorker :: RuntimeContext -> ConnectionSlot -> STM ()
reserveRegisteredWorker context slot = do
  modifyTVar' (contextSpawnsInFlight context) (+ 1)
  void
    ( appendShellTrace
        (contextTrace context)
        (ShellConnectionRegistered (slotPhysicalRef slot) (slotSource slot))
    )

closeConnection :: RuntimeContext -> ConnectionRef plane -> IO RuntimeSubmission
closeConnection context reference = atomically $ do
  let physical = PhysicalConnectionName (connectionRefToken reference) (connectionRefOrdinal reference)
  publiclyClosed <- Set.member physical <$> readTVar (contextPubliclyClosed context)
  if publiclyClosed
    then stale physical
    else do
      removed <- closeConnectionSTM context reference True
      case removed of
        Just slot -> do
          when (slotOwnsFinalClose slot)
            $ modifyTVar' (contextPubliclyClosed context) (Set.insert physical)
          pure ConnectionClosed
        Nothing -> stale physical
  where
    stale physical = do
      void (appendShellTrace (contextTrace context) (ShellStaleSuppressed (Just physical) Nothing))
      pure StalePhysicalGeneration

submitApplication :: RuntimeContext -> ConnectionRef ApplicationPlane -> ApplicationClientDto -> IO RuntimeSubmission
submitApplication context reference dto = enqueueExternal context reference (ApplicationEnvelope reference dto)

submitConfiguredAdministration ::
  RuntimeContext ->
  ConnectionRef ConfiguredAdministrationPlane ->
  AdminClientDto ->
  IO RuntimeSubmission
submitConfiguredAdministration context reference dto =
  enqueueExternal
    context
    reference
    (ConfiguredAdministrationEnvelope reference dto)

submitPeerOpen :: RuntimeContext -> ConnectionRef PeerPlane -> Set PeerAddress -> Maybe HeraldLocator -> IO RuntimeSubmission
submitPeerOpen context reference addresses locator = enqueueExternal context reference (PeerOpenEnvelope reference addresses locator)

submitPeerHelloIngress :: RuntimeContext -> ConnectionRef PeerPlane -> PeerCandidate -> Set PeerAddress -> PeerHello -> HeraldMembershipGenerationId -> MemberSetDigest -> Maybe HeraldLocator -> IO RuntimeSubmission
submitPeerHelloIngress context reference candidate addresses hello generation members locator =
  enqueueExternal context reference (PeerHelloEnvelope reference candidate addresses hello generation members locator)

submitPeerControlWithProgressIngress :: RuntimeContext -> ConnectionRef PeerPlane -> PeerBinding -> ReceiptRetirement -> PeerControl -> IO RuntimeSubmission
submitPeerControlWithProgressIngress context reference binding progress control =
  enqueueExternal context reference (PeerControlWithProgressEnvelope reference binding progress control)

submitPeerPublicationWithProgressIngress :: RuntimeContext -> ConnectionRef PeerPlane -> PeerBinding -> ReceiptRetirement -> PeerLogicalItem -> IO RuntimeSubmission
submitPeerPublicationWithProgressIngress context reference binding progress item =
  enqueueExternal context reference (PeerPublicationWithProgressEnvelope reference binding progress item)

submitPeerControlIngress :: RuntimeContext -> ConnectionRef PeerPlane -> PeerBinding -> PeerControl -> IO RuntimeSubmission
submitPeerControlIngress context reference binding control =
  enqueueExternal context reference (PeerControlEnvelope reference binding control)

submitPeerPublicationIngress :: RuntimeContext -> ConnectionRef PeerPlane -> PeerBinding -> PeerLogicalItem -> IO RuntimeSubmission
submitPeerPublicationIngress context reference binding item =
  enqueueExternal context reference (PeerPublicationEnvelope reference binding item)

submitOracleIngress :: RuntimeContext -> OracleClientIngress -> IO RuntimeSubmission
submitOracleIngress context ingress = atomically $ do
  phase <- readTVar (contextPhase context)
  case phase of
    RuntimeRunning -> do
      enqueueArbiter (contextArbiter context) oracleSource (OracleEnvelope ingress)
      pure Queued
    RuntimeDraining _ -> pure IngressClosedForDrain
    RuntimeStoppedPhase _ -> pure RuntimeStopped
    RuntimeStarting -> pure RuntimeStopped

submitOracleHealthRound :: RuntimeContext -> Word64 -> [OracleHealthObservation] -> IO RuntimeSubmission
submitOracleHealthRound context roundNumber observations = atomically $ do
  phase <- readTVar (contextPhase context)
  case phase of
    RuntimeRunning -> do
      void (queueCompletionFrom context UncorrelatedCompletion (OracleHealthRoundObserved roundNumber observations))
      pure Queued
    RuntimeDraining _ -> pure IngressClosedForDrain
    RuntimeStoppedPhase _ -> pure RuntimeStopped
    RuntimeStarting -> pure RuntimeStopped

-- | One local physical waiter for a pure restricted request. The numeric
-- correlation is delivery routing only; retries retain the caller's exact
-- logical payload and obtain a fresh routing correlation. Cancellation removes
-- the waiter without retracting an already selected semantic request.
exchangeJoin :: RuntimeContext -> ByteString -> IO (Either RuntimeSubmission ByteString)
exchangeJoin context payload = mask $ \restore -> do
  admitted <- atomically $ do
    phase <- readTVar (contextPhase context)
    case phase of
      RuntimeRunning -> do
        correlation <- stateTVar (contextNextJoinCorrelation context) (\next -> (next, next + 1))
        reply <- newEmptyTMVar
        modifyTVar' (contextJoinReplies context) (Map.insert correlation reply)
        enqueueArbiter
          (contextArbiter context)
          (SourceId OracleSources 1)
          (AdmittedEnvelope (JoinInput correlation payload))
        pure (Right (correlation, reply))
      RuntimeDraining _ -> pure (Left IngressClosedForDrain)
      RuntimeStoppedPhase _ -> pure (Left RuntimeStopped)
      RuntimeStarting -> pure (Left RuntimeStopped)
  case admitted of
    Left disposition -> pure (Left disposition)
    Right (correlation, reply) ->
      restore
        ( atomically
            ( (Right <$> readTMVar reply)
                `orElse` (readTMVar (contextTerminalRequest context) >> pure (Left RuntimeStopped))
            )
        )
        `finally` atomically (modifyTVar' (contextJoinReplies context) (Map.delete correlation))

submitDisappearanceAbort :: RuntimeContext -> DisappearanceProbeId -> IO RuntimeSubmission
submitDisappearanceAbort context probe = atomically $ do
  phase <- readTVar (contextPhase context)
  case phase of
    RuntimeRunning -> do
      enqueueArbiter
        (contextArbiter context)
        oracleSource
        (DisappearanceAbortEnvelope probe)
      pure Queued
    RuntimeDraining _ -> pure IngressClosedForDrain
    RuntimeStoppedPhase _ -> pure RuntimeStopped
    RuntimeStarting -> pure RuntimeStopped

submitRetiredEpochRejection ::
  RuntimeContext ->
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  IO RuntimeSubmission
submitRetiredEpochRejection context reporter generation = atomically $ do
  phase <- readTVar (contextPhase context)
  case phase of
    RuntimeRunning -> do
      void
        ( queueCompletionFrom
            context
            UncorrelatedCompletion
            (RetiredLocalHeraldEpochRejected reporter generation)
        )
      pure Queued
    RuntimeDraining _ -> pure IngressClosedForDrain
    RuntimeStoppedPhase _ -> pure RuntimeStopped
    RuntimeStarting -> pure RuntimeStopped

enqueueExternal :: RuntimeContext -> ConnectionRef plane -> IngressEnvelope -> IO RuntimeSubmission
enqueueExternal context reference envelope = atomically $ do
  phase <- readTVar (contextPhase context)
  current <- lookupCurrentSlot (contextRegistry context) reference
  publiclyClosed <-
    Set.member
      (PhysicalConnectionName (connectionRefToken reference) (connectionRefOrdinal reference))
      <$> readTVar (contextPubliclyClosed context)
  case if publiclyClosed then Nothing else current of
    Nothing -> do
      forM_ (envelopePhysicalRef envelope) $ \physical ->
        void (appendShellTrace (contextTrace context) (ShellStaleSuppressed (Just (physicalName physical)) (Just (physicalRefSource physical))))
      pure StalePhysicalGeneration
    Just slot -> case phase of
      RuntimeRunning -> do
        enqueueArbiter (contextArbiter context) (slotSource slot) envelope
        pure Queued
      RuntimeDraining _ -> pure IngressClosedForDrain
      RuntimeStoppedPhase _ -> pure RuntimeStopped
      RuntimeStarting -> pure RuntimeStopped

dispatcherLoop :: RuntimeContext -> IO ()
dispatcherLoop context = mask $ \restore -> go restore
  where
    go restore = do
      pending <- atomically (peekTQueue (contextBatchQueue context))
      restore (hookBeforeDispatchBatch (contextHooks context) pending)
      envelope <- atomically (readTQueue (contextBatchQueue context))
      continue <- routeBatch context envelope
      restore (hookAfterDispatchBatch (contextHooks context) envelope)
      when continue (go restore)

routeBatch :: RuntimeContext -> BatchEnvelope -> IO Bool
routeBatch context (BatchEnvelope batchOrdinal batch _ route) = do
  completed <- routeMembers (zip [0 ..] (effectBatchMembers batch))
  when completed $ atomically $ do
    -- The owner has already consumed each recorded loss. Once its own batch
    -- has crossed the FIFO dispatcher, every earlier offer naming that dead
    -- binding has also crossed it. Later writers cannot create a new loss:
    -- the physical registry permits only one removal of their exact slot.
    modifyTVar' (contextBindingLosses context) (Map.filter (lossStillNeeded batchOrdinal))
    cut <- readTVar (contextDrainCut context)
    when (maybe True (batchOrdinal <=) cut) $ do
      modifyTVar' (contextDrainLedger context) (advanceRoutedThrough batchOrdinal)
      sealAtCut context batchOrdinal
      tryQueueBarrier context
  pure completed
  where
    lossStillNeeded routed (BindingLossStepped cut) = routed < cut
    lossStillNeeded _ _ = True

    routeMembers [] = pure True
    routeMembers ((memberOrdinal, effect) : retained) = do
      let ordinal = EffectMemberOrdinal memberOrdinal
          origin = ObligationOrigin batchOrdinal ordinal
      atomically $ do
        cut <- readTVar (contextDrainCut context)
        when (maybe True (batchOrdinal <=) cut)
          $ modifyTVar' (contextDrainLedger context) (registerObligation origin)
      continue <- routeEffect context batchOrdinal route origin effect
      if continue then routeMembers retained else pure False

routeEffect :: RuntimeContext -> BatchOrdinal -> Maybe DispositionRoute -> ObligationOrigin -> HeraldEffect -> IO Bool
routeEffect context batchOrdinal route origin effect = case projectApplicationEffect effect of
  Left _ -> do
    atomically (failRuntime context HeraldRuntimeCoordinationFailure)
    pure False
  Right (Just outbound) -> do
    atomically $ do
      routeApplication context batchOrdinal route origin outbound
      appendRouted context origin effect
    pure True
  Right Nothing -> case projectAdministrationEffect effect of
    Left _ -> do
      atomically (failRuntime context HeraldRuntimeCoordinationFailure)
      pure False
    Right (Just outbound) -> do
      atomically $ do
        routeConfiguredAdministration context route origin outbound
        appendRouted context origin effect
      pure True
    Right Nothing -> case projectPeerWireEffect effect of
      Left _ -> do
        atomically (failRuntime context HeraldRuntimeCoordinationFailure)
        pure False
      Right projected ->
        routeNonApplication context batchOrdinal route origin projected effect

projectPeerWireEffect ::
  HeraldEffect ->
  Either PeerRpcProjectionFault (Maybe PeerEnvelope)
projectPeerWireEffect = \case
  SendPeerCandidate _ generation members hello ->
    Just <$> projectPeerHello generation members hello
  SendPeerControl _ control -> Just <$> projectPeerControl control
  SendPeerControlWithProgress _ progress control -> Just <$> projectPeerControlWithProgress progress control
  SendPeerItem _ attempt -> Just <$> projectPeerLogicalAttempt attempt
  SendPeerItemWithProgress _ progress attempt -> Just <$> projectPeerLogicalAttemptWithProgress progress attempt
  _ -> Right Nothing

appendRouted :: RuntimeContext -> ObligationOrigin -> HeraldEffect -> STM ()
appendRouted context (ObligationOrigin batch member) effect =
  void (appendShellTrace (contextTrace context) (ShellEffectRouted batch member effect))

routeApplication :: RuntimeContext -> BatchOrdinal -> Maybe DispositionRoute -> ObligationOrigin -> ApplicationOutbound -> STM ()
routeApplication context batchOrdinal route origin outbound = case outbound of
  AcceptApplicationCandidate candidate binding _ -> do
    target <- exactApplicationRoute context route candidate
    case target of
      Just slot@(ApplicationSlot _ _ _ queue) -> do
        rehomeApplicationBinding context slot binding
        replaceSlot (contextRegistry context) (setApplicationPhase (ApplicationEstablished binding) slot)
        void
          ( appendShellTrace
              (contextTrace context)
              (ShellConnectionPromoted (slotPhysicalRef slot) (slotSource slot) (ApplicationConnectionPromoted binding))
          )
        resumeArbiterSource (contextArbiter context) (slotSource slot)
        writeTQueue queue (WriteApplication outbound)
        resolveTransferReservation context batchOrdinal (TransferApplicationBinding binding) True
        settleOffer context origin
      _ -> do
        resolveTransferReservation context batchOrdinal (TransferApplicationBinding binding) False
        queueBindingLoss context (TransferApplicationBinding binding) (Just origin)
  SendPendingApplicationCandidate candidate _ -> do
    target <- exactApplicationRoute context route candidate
    forM_ target $ \slot -> do
      replaceSlot (contextRegistry context) (setApplicationPhase (ApplicationClaimPending candidate) slot)
      resumeArbiterSource (contextArbiter context) (slotSource slot)
      writeTQueue (slotWriterQueue slot) (WriteApplication outbound)
    settleOffer context origin
  RejectApplicationCandidate candidate _ -> do
    target <- exactApplicationRoute context route candidate
    forM_ target $ \slot -> do
      pauseArbiterSource (contextArbiter context) (slotSource slot)
      replaceSlot (contextRegistry context) (setApplicationPhase (ApplicationClosing Nothing) slot)
      writeTQueue (slotWriterQueue slot) (WriteApplication outbound)
      writeTQueue (slotWriterQueue slot) (CloseWriter Nothing)
    settleOffer context origin
  RejectEstablishedApplication binding _ -> routeApplicationBinding context origin binding outbound True
  SendEstablishedApplication binding _ -> routeApplicationBinding context origin binding outbound False
  DisposeEstablishedApplicationSession session _ -> do
    target <- claimApplicationSessionDisposal (contextRegistry context) session
    forM_ target $ \slot -> do
      -- A terminal disposition belongs to the session, not to the binding
      -- generation which happened to trigger it.  Once claimed, make the
      -- current route ingress-inert and retain FIFO ownership of its one final
      -- write followed by physical close.  'Nothing' deliberately suppresses
      -- a fabricated binding-loss observation when the writer tears down.
      pauseArbiterSource (contextArbiter context) (slotSource slot)
      writeTQueue (slotWriterQueue slot) (WriteApplication outbound)
      writeTQueue (slotWriterQueue slot) (CloseWriter Nothing)
    -- A missing or already-closing physical route is an idempotently settled
    -- best-effort terminal notification, not evidence of another loss.
    settleOffer context origin

routeConfiguredAdministration ::
  RuntimeContext ->
  Maybe DispositionRoute ->
  ObligationOrigin ->
  AdministrationOutbound ->
  STM ()
routeConfiguredAdministration context route origin outbound = case outbound of
  AcceptAdministrationCandidate candidate binding -> do
    target <- exactConfiguredAdministrationRoute context route candidate
    case target of
      Just slot -> do
        rehomeConfiguredAdministration context slot
        replaceSlot
          (contextRegistry context)
          ( setConfiguredAdministrationPhase
              (ConfiguredAdministrationEstablished binding)
              slot
          )
        void
          ( appendShellTrace
              (contextTrace context)
              ( ShellConnectionPromoted
                  (slotPhysicalRef slot)
                  (slotSource slot)
                  (ConfiguredAdministrationConnectionPromoted binding)
              )
          )
        resumeArbiterSource (contextArbiter context) (slotSource slot)
        writeTQueue
          (slotWriterQueue slot)
          (WriteConfiguredAdministration outbound)
        settleOffer context origin
      Nothing ->
        queueBindingLoss
          context
          (TransferAdministrationBinding binding)
          (Just origin)
  RejectAdministrationCandidate candidate -> do
    target <- exactConfiguredAdministrationRoute context route candidate
    forM_ target $ \slot -> do
      pauseArbiterSource (contextArbiter context) (slotSource slot)
      replaceSlot
        (contextRegistry context)
        (setConfiguredAdministrationPhase (ConfiguredAdministrationClosing Nothing) slot)
      writeTQueue
        (slotWriterQueue slot)
        (WriteConfiguredAdministration outbound)
      writeTQueue (slotWriterQueue slot) (CloseWriter Nothing)
    settleOffer context origin
  RejectEstablishedAdministration binding -> do
    target <- findConfiguredAdministrationBinding (contextRegistry context) binding
    forM_ target $ \slot -> do
      pauseArbiterSource (contextArbiter context) (slotSource slot)
      replaceSlot
        (contextRegistry context)
        ( setConfiguredAdministrationPhase
            (ConfiguredAdministrationClosing (Just binding))
            slot
        )
      writeTQueue
        (slotWriterQueue slot)
        (WriteConfiguredAdministration outbound)
      writeTQueue (slotWriterQueue slot) (CloseWriter Nothing)
    settleOffer context origin
  SendEstablishedAdministration binding _ -> do
    target <- findConfiguredAdministrationBinding (contextRegistry context) binding
    case target of
      Nothing ->
        queueBindingLoss
          context
          (TransferAdministrationBinding binding)
          (Just origin)
      Just slot -> do
        writeTQueue
          (slotWriterQueue slot)
          (WriteConfiguredAdministration outbound)
        settleOffer context origin

exactConfiguredAdministrationRoute ::
  RuntimeContext ->
  Maybe DispositionRoute ->
  CandidateAdministrationLane ->
  STM (Maybe ConnectionSlot)
exactConfiguredAdministrationRoute context route candidate = case route of
  Just (ConfiguredAdministrationDispositionRoute reference source expected)
    | expected == candidate -> do
        current <- lookupCurrentSlot (contextRegistry context) reference
        pure $ case current of
          Just slot@(ConfiguredAdministrationSlot _ (ConfiguredAdministrationDispositionPending pending) _ _)
            | pending == candidate && slotSource slot == source -> Just slot
          _ -> Nothing
  _ -> pure Nothing

exactApplicationRoute ::
  RuntimeContext ->
  Maybe DispositionRoute ->
  CandidateApplicationLane ->
  STM (Maybe ConnectionSlot)
exactApplicationRoute context route candidate = case route of
  Just (ApplicationDispositionRoute reference source expected)
    | expected == candidate -> do
        current <- lookupCurrentSlot (contextRegistry context) reference
        pure $ case current of
          Just slot@(ApplicationSlot _ (ApplicationDispositionPending pending) _ _)
            | pending == candidate && slotSource slot == source -> Just slot
          _ -> Nothing
  _ -> do
    slots <- registrySlots (contextRegistry context)
    pure (find pendingClaim slots)
  where
    pendingClaim (ApplicationSlot _ (ApplicationClaimPending current) _ _) = current == candidate
    pendingClaim (ApplicationSlot _ (ApplicationAdmissionDeferred current) _ _) = current == candidate
    pendingClaim _ = False

exactPeerCandidateRoute ::
  RuntimeContext ->
  Maybe DispositionRoute ->
  PeerCandidate ->
  PeerHello ->
  STM (Maybe ConnectionSlot)
exactPeerCandidateRoute context route candidate hello = case route of
  Just (LocalPeerCandidateRoute reference source nonce)
    | peerCandidateConnectionNonce candidate == nonce
        && peerHelloConnectionNonce hello == nonce ->
        exactPeerSlot reference source $ \case
          PeerCandidateRoutePending pending -> pending == nonce
          _ -> False
  Just (PeerDispositionRoute reference source expected)
    | expected == candidate ->
        exactPeerSlot reference source $ \case
          PeerDispositionPending pending -> pending == candidate
          _ -> False
  _ -> pure Nothing
  where
    exactPeerSlot reference source matches = do
      current <- lookupCurrentSlot (contextRegistry context) reference
      pure $ case current of
        Just slot@(PeerSlot _ phase _ _)
          | matches phase && slotSource slot == source -> Just slot
        _ -> Nothing

exactPeerDispositionRoute ::
  RuntimeContext ->
  Maybe DispositionRoute ->
  PeerCandidate ->
  STM (Maybe ConnectionSlot)
exactPeerDispositionRoute context route candidate = case route of
  Just (PeerDispositionRoute reference source expected)
    | expected == candidate -> do
        current <- lookupCurrentSlot (contextRegistry context) reference
        pure $ case current of
          Just slot@(PeerSlot _ (PeerDispositionPending pending) _ _)
            | pending == candidate && slotSource slot == source -> Just slot
          _ -> Nothing
  _ -> pure Nothing

routeMatchesExpectation :: DispositionExpectation -> Maybe DispositionRoute -> Bool
routeMatchesExpectation expectation route = case (expectation, route) of
  (ExpectApplicationDisposition expected, Just (ApplicationDispositionRoute _ _ actual)) -> expected == actual
  ( ExpectConfiguredAdministrationDisposition expected,
    Just (ConfiguredAdministrationDispositionRoute _ _ actual)
    ) -> expected == actual
  (ExpectPeerCandidateRoute expected, Just (LocalPeerCandidateRoute _ _ actual)) -> expected == actual
  (ExpectPeerDisposition expected, Just (PeerDispositionRoute _ _ actual)) -> expected == actual
  _ -> False

installTransferReservation ::
  RuntimeContext ->
  BatchOrdinal ->
  Maybe DispositionRoute ->
  EffectBatch ->
  STM ()
installTransferReservation context batchOrdinal route batch =
  forM_ (acceptedTransfer route batch) $ \binding -> do
    losses <- readTVar (contextBindingLosses context)
    forM_ route $ \exactRoute -> do
      modifyTVar'
        (contextTransferReservations context)
        (Map.insert (binding, batchOrdinal) exactRoute)
      case Map.lookup binding losses of
        Just (BindingLossPending lossSource completion origins) -> do
          void (removeArbiterSource (contextArbiter context) lossSource)
          modifyTVar' (contextBindingLosses context) (Map.delete binding)
          modifyTVar'
            (contextHeldBindingLossOrigins context)
            (Map.insert binding (HeldBindingLoss origins))
          markCompletionResolved context completion
          void (appendShellTrace (contextTrace context) (ShellStaleSuppressed Nothing (Just lossSource)))
        _ -> pure ()
      tryQueueBarrier context

acceptedTransfer :: Maybe DispositionRoute -> EffectBatch -> Maybe TransferBinding
acceptedTransfer route = foldr match Nothing . effectBatchMembers
  where
    match effect retained = case (route, effect) of
      ( Just (ApplicationDispositionRoute _ _ expected),
        SetApplicationConnectionDisposition candidate acceptance
        )
          | candidate == expected ->
              Just (TransferApplicationBinding (sessionAcceptanceBinding acceptance))
      ( Just (PeerDispositionRoute _ _ expected),
        SetPeerCandidateDisposition candidate (PeerHelloAccepted _ binding)
        )
          | candidate == expected -> Just (TransferPeerBinding binding)
      _ -> retained

currentTransferRoute :: RuntimeContext -> TransferBinding -> STM (Maybe ConnectionSlot)
currentTransferRoute context binding = do
  slots <- registrySlots (contextRegistry context)
  pure (foldr retainMatching Nothing slots)
  where
    retainMatching slot retained
      | slotOwnsTransferBinding binding slot = Just slot
      | otherwise = retained

slotOwnsTransferBinding :: TransferBinding -> ConnectionSlot -> Bool
slotOwnsTransferBinding binding slot = case (binding, slot) of
  (TransferApplicationBinding expected, ApplicationSlot _ phase _ _) -> case phase of
    ApplicationEstablished actual -> actual == expected
    ApplicationClosing (Just actual) -> actual == expected
    _ -> False
  (TransferAdministrationBinding expected, ConfiguredAdministrationSlot _ phase _ _) ->
    case phase of
      ConfiguredAdministrationEstablished actual -> actual == expected
      ConfiguredAdministrationClosing (Just actual) -> actual == expected
      _ -> False
  (TransferPeerBinding expected, PeerSlot _ phase _ _) -> case phase of
    PeerEstablished actual -> actual == expected
    PeerClosing (Just actual) -> actual == expected
    _ -> False
  _ -> False

resolveTransferReservation ::
  RuntimeContext ->
  BatchOrdinal ->
  TransferBinding ->
  Bool ->
  STM ()
resolveTransferReservation context batchOrdinal binding succeeded = do
  reservations <- readTVar (contextTransferReservations context)
  held <- readTVar (contextHeldBindingLossOrigins context)
  let wasReserved = Map.member (binding, batchOrdinal) reservations
      retainedOrigins = case Map.lookup binding held of
        Just (HeldBindingLoss origins) -> origins
        Nothing -> []
  writeTVar
    (contextTransferReservations context)
    (Map.delete (binding, batchOrdinal) reservations)
  modifyTVar' (contextHeldBindingLossOrigins context) (Map.delete binding)
  if succeeded
    then do
      modifyTVar' (contextBindingLosses context) (Map.delete binding)
      forM_ retainedOrigins (settleOffer context)
    else when wasReserved $ do
      oldRoute <- currentTransferRoute context binding
      forM_ oldRoute (detachSlotWithoutLoss context)
      forM_ retainedOrigins (queueBindingLoss context binding . Just)

rehomeApplicationBinding :: RuntimeContext -> ConnectionSlot -> ApplicationSessionBinding -> STM ()
rehomeApplicationBinding context target binding = do
  slots <- registrySlots (contextRegistry context)
  forM_ slots $ \slot ->
    when
      ( slotSource slot /= slotSource target
          && applicationSlotSession slot == Just (sessionBindingSessionId binding)
      )
      (detachSlotWithoutLoss context slot)
  where
    applicationSlotSession = \case
      ApplicationSlot _ phase _ _ -> case phase of
        ApplicationEstablished current -> Just (sessionBindingSessionId current)
        ApplicationClosing (Just current) -> Just (sessionBindingSessionId current)
        _ -> Nothing
      _ -> Nothing

rehomeConfiguredAdministration ::
  RuntimeContext ->
  ConnectionSlot ->
  STM ()
rehomeConfiguredAdministration context target = do
  slots <- registrySlots (contextRegistry context)
  forM_ slots $ \slot -> case slot of
    ConfiguredAdministrationSlot _ phase _ _
      | slotSource slot /= slotSource target && establishedOrClosing phase ->
          detachSlotWithoutLoss context slot
    _ -> pure ()
  where
    establishedOrClosing = \case
      ConfiguredAdministrationEstablished _ -> True
      ConfiguredAdministrationClosing _ -> True
      _ -> False

rehomePeerBinding :: RuntimeContext -> ConnectionSlot -> PeerBinding -> STM ()
rehomePeerBinding context target binding = do
  slots <- registrySlots (contextRegistry context)
  forM_ slots $ \slot ->
    when
      ( slotSource slot /= slotSource target
          && peerSlotRemoteHerald slot == Just (peerBindingRemoteHeraldEpoch binding)
      )
      (detachSlotWithoutLoss context slot)
  where
    peerSlotRemoteHerald = \case
      PeerSlot _ phase _ _ -> case phase of
        PeerEstablished current -> Just (peerBindingRemoteHeraldEpoch current)
        PeerClosing (Just current) -> Just (peerBindingRemoteHeraldEpoch current)
        _ -> Nothing
      _ -> Nothing

-- | Retire every physical projection superseded by a logical peer acceptance
-- whose selected candidate disappeared before its disposition could be
-- routed. The pure owner has already replaced the old binding at this point,
-- so retaining an established slot or the runtime's old epoch lookup would
-- leave a route which can never again be current.
retireFailedPeerAcceptance :: RuntimeContext -> PeerBinding -> STM ()
retireFailedPeerAcceptance context binding = do
  slots <- registrySlots (contextRegistry context)
  forM_ slots $ \slot -> case slot of
    PeerSlot _ phase _ _
      | establishedRemote phase == Just remote ->
          detachSlotWithoutLoss context slot
    _ -> pure ()
  modifyTVar' (contextRemoteBindings context) (Map.delete remote)
  where
    remote = peerBindingRemoteHeraldEpoch binding
    establishedRemote = \case
      PeerEstablished current -> Just (peerBindingRemoteHeraldEpoch current)
      PeerClosing (Just current) -> Just (peerBindingRemoteHeraldEpoch current)
      _ -> Nothing

detachSlotWithoutLoss :: RuntimeContext -> ConnectionSlot -> STM ()
detachSlotWithoutLoss context slot = do
  removed <- case slot of
    ApplicationSlot reference _ _ _ -> removeCurrentSlot (contextRegistry context) reference
    AdministrationSlot reference _ _ -> removeCurrentSlot (contextRegistry context) reference
    ConfiguredAdministrationSlot reference _ _ _ ->
      removeCurrentSlot (contextRegistry context) reference
    PeerSlot reference _ _ _ -> removeCurrentSlot (contextRegistry context) reference
  forM_ removed $ \current -> do
    modifyTVar' (contextPubliclyClosed context) (Set.delete (physicalName (slotPhysicalRef current)))
    slotRetirementAction current
    void (removeArbiterSource (contextArbiter context) (slotSource current))
    when (not (slotOwnsFinalClose current))
      $ writeTQueue (slotWriterQueue current) (CloseWriter Nothing)
    claimSlotOutcomes context current
    removeSlotRemoteBinding context current
    void
      ( appendShellTrace
          (contextTrace context)
          (ShellConnectionClosed (slotPhysicalRef current) (slotSource current))
      )
    emitDiagnostic context RuntimeConnectionDiagnostic

routeApplicationBinding :: RuntimeContext -> ObligationOrigin -> ApplicationSessionBinding -> ApplicationOutbound -> Bool -> STM ()
routeApplicationBinding context origin binding outbound closes = do
  target <- findApplicationBinding (contextRegistry context) binding
  case target of
    Nothing -> queueBindingLoss context (TransferApplicationBinding binding) (Just origin)
    Just slot -> do
      when closes $ do
        pauseArbiterSource (contextArbiter context) (slotSource slot)
        replaceSlot (contextRegistry context) (setApplicationPhase (ApplicationClosing (Just binding)) slot)
      writeTQueue (slotWriterQueue slot) (WriteApplication outbound)
      when closes (writeTQueue (slotWriterQueue slot) (CloseWriter Nothing))
      settleOffer context origin

settleOffer :: RuntimeContext -> ObligationOrigin -> STM ()
settleOffer context origin =
  updateOriginLedger context origin (settleObligationByOffer origin)

batchDrainId :: EffectBatch -> Maybe DrainId
batchDrainId = foldr findDrain Nothing . effectBatchMembers
  where
    findDrain (BeginDrain drainId) _ = Just drainId
    findDrain _ retained = retained

externalEnvelope :: IngressEnvelope -> Bool
externalEnvelope = \case
  ApplicationEnvelope {} -> True
  ConfiguredAdministrationEnvelope {} -> True
  PeerOpenEnvelope {} -> True
  PeerHelloEnvelope {} -> True
  PeerControlEnvelope {} -> True
  PeerControlWithProgressEnvelope {} -> True
  PeerPublicationWithProgressEnvelope {} -> True
  PeerPublicationEnvelope {} -> True
  OracleEnvelope _ -> True
  DisappearanceAbortEnvelope {} -> True
  CompletionEnvelope {} -> False
  AdmittedEnvelope {} -> False

envelopePhysicalRef :: IngressEnvelope -> Maybe PhysicalConnectionRef
envelopePhysicalRef = \case
  ApplicationEnvelope reference _ -> Just (PhysicalApplicationRef reference)
  ConfiguredAdministrationEnvelope reference _ ->
    Just (PhysicalConfiguredAdministrationRef reference)
  PeerOpenEnvelope reference _ _ -> Just (PhysicalPeerRef reference)
  PeerHelloEnvelope reference _ _ _ _ _ _ -> Just (PhysicalPeerRef reference)
  PeerControlEnvelope reference _ _ -> Just (PhysicalPeerRef reference)
  PeerControlWithProgressEnvelope reference _ _ _ -> Just (PhysicalPeerRef reference)
  PeerPublicationWithProgressEnvelope reference _ _ _ -> Just (PhysicalPeerRef reference)
  PeerPublicationEnvelope reference _ _ -> Just (PhysicalPeerRef reference)
  OracleEnvelope _ -> Nothing
  DisappearanceAbortEnvelope {} -> Nothing
  CompletionEnvelope {} -> Nothing
  AdmittedEnvelope {} -> Nothing

physicalName :: PhysicalConnectionRef -> PhysicalConnectionName
physicalName = \case
  PhysicalApplicationRef reference -> PhysicalConnectionName (connectionRefToken reference) (connectionRefOrdinal reference)
  PhysicalAdministrationRef reference -> PhysicalConnectionName (connectionRefToken reference) (connectionRefOrdinal reference)
  PhysicalConfiguredAdministrationRef reference ->
    PhysicalConnectionName (connectionRefToken reference) (connectionRefOrdinal reference)
  PhysicalPeerRef reference -> PhysicalConnectionName (connectionRefToken reference) (connectionRefOrdinal reference)
  PhysicalOpaqueRef token ordinal -> PhysicalConnectionName token ordinal

staleEnvelopeEvent :: IngressEnvelope -> SourceId -> ShellTraceEvent
staleEnvelopeEvent envelope source =
  ShellStaleSuppressed (physicalName <$> envelopePhysicalRef envelope) (Just source)

physicalRefSource :: PhysicalConnectionRef -> SourceId
physicalRefSource = \case
  PhysicalApplicationRef (ConnectionRef _ ordinal) -> SourceId ApplicationSources ordinal
  PhysicalAdministrationRef (ConnectionRef _ ordinal) -> SourceId AdministrationSources ordinal
  PhysicalConfiguredAdministrationRef (ConnectionRef _ ordinal) ->
    SourceId ConfiguredAdministrationSources ordinal
  PhysicalPeerRef (ConnectionRef _ ordinal) -> SourceId PeerSources ordinal
  PhysicalOpaqueRef _ ordinal -> SourceId RuntimeCompletionSources ordinal

isDraining :: RuntimePhase -> Bool
isDraining = \case
  RuntimeDraining _ -> True
  _ -> False

discardExternalIngress :: RuntimeContext -> STM ()
discardExternalIngress context = do
  slots <- registrySlots (contextRegistry context)
  forM_ slots $ \slot -> case slot of
    AdministrationSlot {} -> pure ()
    _ -> void (removeArbiterSource (contextArbiter context) (slotSource slot))
  discardedWork <- removeArbiterSource (contextArbiter context) workSource
  void (removeArbiterSource (contextArbiter context) oracleSource)
  discardedDials <- flushTQueue (contextPeerDialQueue context)
  handedDial <- readTVar (contextPeerDialHanded context)
  writeTVar (contextPeerDialHanded context) Nothing
  delayed <- readTVar (contextDelayedSlots context)
  dormant <- readTVar (contextDormantWork context)
  let delayedTickets = concatMap (fmap snd) (Map.elems delayed)
      dormantTickets = concat (Map.elems dormant)
      queuedTickets = [ticket | AdmittedEnvelope (PeerInput (PeerDispatchSelected ticket _)) <- discardedWork]
      suppressed = deduplicateDiscardedWorkTickets [delayedTickets, dormantTickets, queuedTickets]
  writeTVar (contextDelayedSlots context) Map.empty
  writeTVar (contextDormantWork context) Map.empty
  forM_ suppressed $ \ticket ->
    void (appendShellTrace (contextTrace context) (ShellDelayedWorkSuppressed ticket))
  forM_ discardedDials $ \intent ->
    void (appendShellTrace (contextTrace context) (ShellPeerDialSuppressed intent))
  forM_ handedDial $ \intent ->
    void (appendShellTrace (contextTrace context) (ShellPeerDialSuppressed intent))

deduplicateDiscardedWorkTickets :: (Ord ticket) => [[ticket]] -> [ticket]
deduplicateDiscardedWorkTickets = Set.toList . Set.fromList . concat

partitionPeerDialCancellation ::
  PeerDialIntent ->
  [PeerDialIntent] ->
  Maybe PeerDialIntent ->
  ([PeerDialIntent], [PeerDialIntent], Maybe PeerDialIntent)
partitionPeerDialCancellation cancellation queued handed =
  (cancelled, retained, handed >>= \current -> if matches current then Just current else Nothing)
  where
    key = peerDialIntentCancellationKey cancellation
    matches = (== key) . peerDialIntentCancellationKey
    (cancelled, retained) = partition matches queued

routePeerControl :: RuntimeContext -> ObligationOrigin -> PeerBinding -> WriterCommand -> STM ()
routePeerControl context origin binding command = do
  target <- findPeerBinding (contextRegistry context) binding
  case target of
    Nothing -> queueBindingLoss context (TransferPeerBinding binding) (Just origin)
    Just slot -> do
      writeTQueue (slotWriterQueue slot) command
      settleOffer context origin

routeNonApplication :: RuntimeContext -> BatchOrdinal -> Maybe DispositionRoute -> ObligationOrigin -> Maybe PeerEnvelope -> HeraldEffect -> IO Bool
routeNonApplication context batchOrdinal route origin projected effect = case effect of
  DeferApplicationSession candidate -> do
    routed $ do
      target <- exactApplicationRoute context route candidate
      forM_ target $ \slot ->
        replaceSlot (contextRegistry context) (setApplicationPhase (ApplicationAdmissionDeferred candidate) slot)
      -- The request is retained by the pure owner. Its exact physical source
      -- remains paused until a later batch disposes this candidate lane.
      settleOffer context origin
    pure True
  RejectPeerCandidateOpened nonce -> do
    routed $ do
      case route of
        Just (LocalPeerCandidateRoute reference source expected) | expected == nonce -> do
          current <- lookupCurrentSlot (contextRegistry context) reference
          case current of
            Just slot@(PeerSlot _ (PeerCandidateRoutePending pending) _ _)
              | pending == nonce && slotSource slot == source -> do
                  replaceSlot (contextRegistry context) (setPeerPhase (PeerClosing Nothing) slot)
                  writeTQueue (slotWriterQueue slot) (CloseWriter Nothing)
            _ -> pure ()
        _ -> pure ()
      settleOffer context origin
    pure True
  SendPeerCandidate candidate _ _ hello -> withPeerEnvelope projected $ \envelope -> routed $ do
    target <- exactPeerCandidateRoute context route candidate hello
    case target of
      Just slot -> do
        case (route, slot) of
          (Just (LocalPeerCandidateRoute {}), PeerSlot _ (PeerCandidateRoutePending _) _ _) -> do
            replaceSlot (contextRegistry context) (setPeerPhase (PeerCandidateCurrent candidate) slot)
            resumeArbiterSource (contextArbiter context) (slotSource slot)
          _ -> pure ()
        writeTQueue (slotWriterQueue slot) (WritePeerCandidate candidate envelope)
        settleOffer context origin
      Nothing -> settleOffer context origin
  SetPeerCandidateDisposition candidate disposition -> do
    routed $ do
      target <- exactPeerDispositionRoute context route candidate
      case (target, disposition) of
        (Just slot, PeerHelloAccepted _ binding) -> do
          rehomePeerBinding context slot binding
          replaceSlot (contextRegistry context) (setPeerPhase (PeerEstablished binding) slot)
          void
            ( appendShellTrace
                (contextTrace context)
                (ShellConnectionPromoted (slotPhysicalRef slot) (slotSource slot) (PeerConnectionPromoted binding))
            )
          modifyTVar' (contextRemoteBindings context) (Map.insert (peerBindingRemoteHeraldEpoch binding) binding)
          resumeArbiterSource (contextArbiter context) (slotSource slot)
          writeTQueue (slotWriterQueue slot) (WritePeerDisposition candidate disposition)
          wakeDormant context (peerBindingRemoteHeraldEpoch binding)
          resolveTransferReservation context batchOrdinal (TransferPeerBinding binding) True
          settleOffer context origin
        (Just slot, PeerHelloRejected _) -> do
          pauseArbiterSource (contextArbiter context) (slotSource slot)
          replaceSlot (contextRegistry context) (setPeerPhase (PeerClosing Nothing) slot)
          writeTQueue (slotWriterQueue slot) (WritePeerDisposition candidate disposition)
          writeTQueue (slotWriterQueue slot) (CloseWriter Nothing)
          settleOffer context origin
        (_, PeerHelloAccepted _ binding) -> do
          retireFailedPeerAcceptance context binding
          resolveTransferReservation context batchOrdinal (TransferPeerBinding binding) False
          queueBindingLoss context (TransferPeerBinding binding) (Just origin)
        _ -> settleOffer context origin
    pure True
  QueuePeerAlignmentEvidence {} -> do
    atomically (failRuntime context HeraldRuntimeCoordinationFailure)
    pure False
  SendPeerControl binding _ -> sendControl binding
  SendPeerControlWithProgress binding _ _ -> sendControl binding
  DialPeer intent -> do
    routed $ do
      phase <- readTVar (contextPhase context)
      case phase of
        RuntimeRunning -> do
          writeTQueue (contextPeerDialQueue context) intent
          void (appendShellTrace (contextTrace context) (ShellPeerDialQueued intent))
        _ ->
          void (appendShellTrace (contextTrace context) (ShellPeerDialSuppressed intent))
      settleOffer context origin
    pure True
  ClosePeerBinding binding -> do
    routed $ do
      target <- findPeerBinding (contextRegistry context) binding
      forM_ target (detachSlotWithoutLoss context)
      settleOffer context origin
    pure True
  CancelPeerDial intent -> do
    atomically $ do
      queued <- flushTQueue (contextPeerDialQueue context)
      handed <- readTVar (contextPeerDialHanded context)
      let (cancelled, retained, handedCancelled) =
            partitionPeerDialCancellation intent queued handed
      forM_ retained (writeTQueue (contextPeerDialQueue context))
      when (handedCancelled /= Nothing) (writeTVar (contextPeerDialHanded context) Nothing)
      forM_ (cancelled <> maybe [] pure handedCancelled) $ \current ->
        void (appendShellTrace (contextTrace context) (ShellPeerDialSuppressed current))
      appendRouted context origin effect
      settleOffer context origin
    forM_ (contextPeerDialCancelSink context) ($ intent)
    pure True
  CancelPeerDestination destination -> do
    routed $ do
      cancelPeerDestination context destination
      settleOffer context origin
    pure True
  SchedulePeerDispatch ticket -> do
    schedulePeerTicket context origin effect ticket
    pure True
  SendPeerItem binding attempt -> sendItem binding attempt
  SendPeerItemWithProgress binding _ attempt -> sendItem binding attempt
  RejectPeerConnection binding disposition -> do
    routed $ do
      target <- findPeerBinding (contextRegistry context) binding
      forM_ target $ \slot -> do
        pauseArbiterSource (contextArbiter context) (slotSource slot)
        replaceSlot (contextRegistry context) (setPeerPhase (PeerClosing (Just binding)) slot)
        writeTQueue (slotWriterQueue slot) (WritePeerRejection binding disposition)
        writeTQueue (slotWriterQueue slot) (CloseWriter Nothing)
      case target of
        Nothing -> queueBindingLoss context (TransferPeerBinding binding) (Just origin)
        Just _ -> settleOffer context origin
    pure True
  ArmTimer attempt spec -> do
    routed $ do
      phase <- readTVar (contextPhase context)
      case phase of
        RuntimeRunning -> do
          writeTQueue
            (contextTimerCommands context)
            (ArmRuntimeTimer attempt spec)
          void
            ( appendShellTrace
                (contextTrace context)
                (ShellTimerArmed attempt spec)
            )
        _ ->
          void
            ( appendShellTrace
                (contextTrace context)
                (ShellTimerCancelled attempt)
            )
      settleOffer context origin
    pure True
  CancelTimer attempt -> do
    routed $ do
      writeTQueue
        (contextTimerCommands context)
        (CancelRuntimeTimer attempt)
      void
        ( appendShellTrace
            (contextTrace context)
            (ShellTimerCancelled attempt)
        )
      settleOffer context origin
    pure True
  SendJoinReply correlation payload -> do
    routed $ do
      replies <- readTVar (contextJoinReplies context)
      forM_ (Map.lookup correlation replies) (\reply -> void (tryPutTMVar reply payload))
      settleOffer context origin
    pure True
  RunOracleClientAction action -> do
    forM_ (contextOracleActionSink context) ($ action)
    atomically $ do
      case action of
        ReportOracleClientDiagnostic {} ->
          emitDiagnostic context RuntimeSuppressionDiagnostic
        _ -> pure ()
      settleOffer context origin
      appendRouted context origin effect
    pure True
  RunOracleHealthRound roundNumber cadence claims contacts -> do
    forM_ (contextOracleHealthSink context) (\sink -> sink roundNumber cadence claims contacts)
    routed (settleOffer context origin)
    pure True
  BeginDrain _ -> do
    routed $ do
      settleOffer context origin
      emitDiagnostic context RuntimeDrainDiagnostic
    pure True
  FinishDrain drainId reply -> do
    let finalAdministration slot finalReply = do
          acknowledged <- newEmptyTMVar
          writeTQueue (slotWriterQueue slot) (WriteFinalAdministration finalReply acknowledged)
          pure [acknowledged]
        privateReply = \case
          Just final@OrderlyHeraldShutdownCompleted {} -> Just final
          _ -> Nothing
    administrationAcks <- atomically $ do
      slots <- registrySlots (contextRegistry context)
      acknowledgements <- fmap concat $ forM slots $ \slot -> do
        case slot of
          AdministrationSlot {} -> finalAdministration slot (privateReply reply)
          ConfiguredAdministrationSlot _ (ConfiguredAdministrationEstablished binding) _ _
            | Just final@(ConfiguredHeraldShutdownCompleted target _) <- reply,
              binding == target ->
                finalAdministration slot (Just final)
          _ -> do
            writeTQueue (slotWriterQueue slot) (CloseWriter Nothing)
            pure []
      appendRouted context origin effect
      pure acknowledgements
    forM_ administrationAcks (atomically . readTMVar)
    atomically $ do
      settleOffer context origin
      void (appendShellTrace (contextTrace context) (ShellDrainFinalized drainId))
      finishRuntime context HeraldRuntimeDrained
    pure True
  FinishIsolation _ -> do
    -- Session terminal dispositions precede this effect in the same kernel
    -- batch. Let their sole writers finish the best-effort final frame and
    -- close before cancelling only the remaining application writers. The
    -- control shell and independently scoped Oracle/Raft role remain alive.
    dispositionDeadline <- registerDelay isolationTerminalWriteGraceMicroseconds
    atomically
      ( ( do
            slots <- registrySlots (contextRegistry context)
            check (not (any applicationDispositionPending slots))
        )
          `orElse` do
            elapsed <- readTVar dispositionDeadline
            check elapsed
      )
    applicationWorkers <- atomically $ do
      workers <- readTVar (contextThreads context)
      pure [worker | worker@(RuntimeWorker _ (Just PhysicalApplicationRef {}) _ _ _) <- workers]
    cancelAndJoinSelectedWorkers context applicationWorkers
    atomically $ do
      appendRouted context origin effect
      settleOffer context origin
    pure True
  applicationEffect@SendInitialClaimPending {} -> routeEffect context batchOrdinal route origin applicationEffect
  applicationEffect@RejectInitialClaim {} -> routeEffect context batchOrdinal route origin applicationEffect
  applicationEffect@SendApplicationLifecycleReply {} -> routeEffect context batchOrdinal route origin applicationEffect
  applicationEffect@SetApplicationConnectionDisposition {} -> routeEffect context batchOrdinal route origin applicationEffect
  applicationEffect@RejectApplicationConnection {} -> routeEffect context batchOrdinal route origin applicationEffect
  applicationEffect@SendApplicationReply {} -> routeEffect context batchOrdinal route origin applicationEffect
  applicationEffect@SendApplicationWaitWake {} -> routeEffect context batchOrdinal route origin applicationEffect
  applicationEffect@SendApplicationIsolationBegun {} -> routeEffect context batchOrdinal route origin applicationEffect
  applicationEffect@DisposeApplicationSession {} -> routeEffect context batchOrdinal route origin applicationEffect
  administrationEffect@SetAdministrationConnectionDisposition {} ->
    routeEffect context batchOrdinal route origin administrationEffect
  administrationEffect@RejectAdministrationConnection {} ->
    routeEffect context batchOrdinal route origin administrationEffect
  administrationEffect@SendAdministrationReply {} ->
    routeEffect context batchOrdinal route origin administrationEffect
  where
    sendControl binding = withPeerEnvelope projected $ \envelope ->
      routed $ routePeerControl context origin binding (WritePeerControl binding envelope)
    sendItem binding attempt = withPeerEnvelope projected $ \envelope -> routed $ do
      target <- findPeerBinding (contextRegistry context) binding
      case target of
        Nothing -> do
          -- A logical binding without its physical route is already lost.  A
          -- Deferred outcome would immediately reschedule the same ticket while
          -- Discovery still selected this binding, creating an unbounded
          -- select/defer loop that can starve the loss observation.  Binding
          -- loss clears the in-flight attempt and retains its payload for the
          -- replacement binding, while correlating this effect's obligation
          -- with the one coalesced loss completion.
          queueBindingLoss context (TransferPeerBinding binding) (Just origin)
        Just slot -> do
          outcomeCell <- newTVar OutcomeCellEmpty
          modifyTVar'
            (contextOutcomeCells context)
            ((slotPhysicalRef slot, slotSource slot, origin, attempt, outcomeCell) :)
          writeTQueue
            (slotWriterQueue slot)
            (WritePeerItem binding attempt envelope origin outcomeCell)
          updateOriginLedger context origin (awaitPhysicalOutcome origin)
    routed action = atomically $ do
      action
      appendRouted context origin effect
    withPeerEnvelope maybeEnvelope action = case maybeEnvelope of
      Nothing -> do
        atomically (failRuntime context HeraldRuntimeCoordinationFailure)
        pure False
      Just envelope -> action envelope >> pure True

    applicationDispositionPending = \case
      ApplicationSlot _ (ApplicationClosing Nothing) _ _ -> True
      _ -> False

-- A terminal disposition is best-effort protocol delivery, not a liveness
-- prerequisite. A blocked socket writer therefore gets one short opportunity
-- to finish before its writer is cancelled. The control shell stays alive.
isolationTerminalWriteGraceMicroseconds :: Int
isolationTerminalWriteGraceMicroseconds = 250_000

schedulePeerTicket :: RuntimeContext -> ObligationOrigin -> HeraldEffect -> PeerDispatchTicket -> IO ()
schedulePeerTicket context origin effect ticket = atomically $ do
  phase <- readTVar (contextPhase context)
  let destination = peerDispatchTicketDestinationHeraldEpoch ticket
  cancelled <- Set.member destination <$> readTVar (contextCancelledDestinations context)
  case (phase, cancelled) of
    (RuntimeRunning, False) -> do
      binding <- Map.lookup destination <$> readTVar (contextRemoteBindings context)
      case binding of
        Nothing -> do
          dormant <- readTVar (contextDormantWork context)
          let retained = Map.findWithDefault [] destination dormant
          when (ticket `notElem` retained) $ writeTVar (contextDormantWork context) (Map.insert destination (retained <> [ticket]) dormant)
        Just _ -> scheduleDelayed context destination ticket
    -- A pre-cut batch can reach the dispatcher after the owner has installed
    -- the cut.  Such a ticket was not present for the cut transaction to
    -- clear, so its route transaction supplies the one exact suppression.
    _ -> void (appendShellTrace (contextTrace context) (ShellDelayedWorkSuppressed ticket))
  settleOffer context origin
  appendRouted context origin effect

cancelPeerDestination :: RuntimeContext -> HeraldEpoch -> STM ()
cancelPeerDestination context destination = do
  modifyTVar' (contextCancelledDestinations context) (Set.insert destination)
  dormant <- readTVar (contextDormantWork context)
  delayed <- readTVar (contextDelayedSlots context)
  queued <- removeArbiterSource (contextArbiter context) workSource
  let dormantTickets = Map.findWithDefault [] destination dormant
      delayedTickets = fmap snd (Map.findWithDefault [] destination delayed)
      (discarded, retained) = partition (targets destination) queued
      queuedTickets = [ticket | AdmittedEnvelope (PeerInput (PeerDispatchSelected ticket _)) <- discarded]
  writeTVar (contextDormantWork context) (Map.delete destination dormant)
  writeTVar (contextDelayedSlots context) (Map.delete destination delayed)
  forM_ retained (enqueueArbiter (contextArbiter context) workSource)
  forM_ (deduplicateDiscardedWorkTickets [dormantTickets, delayedTickets, queuedTickets]) $ \ticket ->
    void (appendShellTrace (contextTrace context) (ShellDelayedWorkSuppressed ticket))
  where
    targets expected (AdmittedEnvelope (PeerInput (PeerDispatchSelected ticket _))) =
      peerDispatchTicketDestinationHeraldEpoch ticket == expected
    targets _ _ = False

scheduleDelayed :: RuntimeContext -> HeraldEpoch -> PeerDispatchTicket -> STM ()
scheduleDelayed context destination ticket = do
  slots <- readTVar (contextDelayedSlots context)
  let retained = Map.findWithDefault [] destination slots
  when (ticket `notElem` fmap snd retained) $ do
    generation <- stateTVar (contextNextDelay context) (\value -> (value, value + 1))
    writeTVar
      (contextDelayedSlots context)
      (Map.insert destination (retained <> [(generation, ticket)]) slots)
    writeTQueue (contextDelayedQueue context) (generation, destination, ticket)
    void (appendShellTrace (contextTrace context) (ShellDelayedWorkEligible ticket))

delayedWorkLoop :: RuntimeContext -> IO ()
delayedWorkLoop context = forever $ do
  (generation, destination, ticket) <- atomically (readTQueue (contextDelayedQueue context))
  hookDelayPeerWork (contextHooks context) ticket (contextDelayMicroseconds context)
  atomically $ do
    slots <- readTVar (contextDelayedSlots context)
    let retained = Map.findWithDefault [] destination slots
        isCurrent = (generation, ticket) `elem` retained
        remaining = filter (/= (generation, ticket)) retained
        successor =
          if null remaining
            then Map.delete destination slots
            else Map.insert destination remaining slots
    if isCurrent
      then do
        writeTVar (contextDelayedSlots context) successor
        phase <- readTVar (contextPhase context)
        binding <- Map.lookup destination <$> readTVar (contextRemoteBindings context)
        case (phase, binding) of
          (RuntimeRunning, Just currentBinding) -> do
            void (appendShellTrace (contextTrace context) (ShellDelayedWorkReleased ticket))
            enqueueArbiter
              (contextArbiter context)
              workSource
              (AdmittedEnvelope (PeerInput (PeerDispatchSelected ticket currentBinding)))
          (RuntimeRunning, Nothing) -> do
            modifyTVar' (contextDormantWork context) (Map.insertWith (<>) destination [ticket])
          _ -> do
            void (appendShellTrace (contextTrace context) (ShellDelayedWorkSuppressed ticket))
            emitDiagnostic context RuntimeSuppressionDiagnostic
      -- The only way a queued generation ceases to be current is the atomic
      -- drain cut, which already records its one suppression while clearing
      -- the slot.  A late worker wake must not duplicate that evidence.
      else pure ()

peerDialSinkLoop :: RuntimeContext -> IO ()
peerDialSinkLoop context = forever $ do
  intent <- atomically $ do
    current <- readTQueue (contextPeerDialQueue context)
    writeTVar (contextPeerDialHanded context) (Just current)
    void (appendShellTrace (contextTrace context) (ShellPeerDialHanded current))
    pure current
  hookBeforePeerDialSink (contextHooks context) intent
  shouldDeliver <- atomically $ do
    phase <- readTVar (contextPhase context)
    handed <- readTVar (contextPeerDialHanded context)
    if phase == RuntimeRunning && handed == Just intent
      then do
        writeTVar (contextPeerDialHanded context) Nothing
        pure True
      else pure False
  when shouldDeliver $ do
    case contextPeerDialSink context of
      Nothing -> ioError (userError "Herald peer dial sink is not configured")
      Just sink -> sink intent
    atomically
      (void (appendShellTrace (contextTrace context) (ShellPeerDialDelivered intent)))

writerLoop :: RuntimeContext -> ConnectionSlot -> IORef (Maybe (TMVar ())) -> IO WriterLoopResult
writerLoop context slot currentAcknowledgement = go
  where
    go = mask $ \restore -> do
      command <- atomically (readTQueue (slotWriterQueue slot))
      writeIORef currentAcknowledgement (writerCommandAcknowledgement command)
      hookAfterWriterCommandDequeued (contextHooks context) (slotPhysicalRef slot)
      outcome <- try (restore (executeWriterCommand context slot command))
      case outcome of
        Left exception ->
          pure (WriterCommandFailed (writerCommandAcknowledgement command) exception)
        Right Nothing -> do
          writeIORef currentAcknowledgement Nothing
          restore go
        Right (Just termination) -> pure (WriterLoopStopped termination)

writerCommandAcknowledgement :: WriterCommand -> Maybe (TMVar ())
writerCommandAcknowledgement = \case
  WriteFinalAdministration _ acknowledged -> Just acknowledged
  CloseWriter acknowledged -> acknowledged
  _ -> Nothing

executeWriterCommand :: RuntimeContext -> ConnectionSlot -> WriterCommand -> IO (Maybe WriterTermination)
executeWriterCommand context slot = \case
  WriteApplication outbound -> case slot of
    ApplicationSlot {} -> offered ApplicationLane (applicationOffer slot outbound)
    _ -> pure (Just (WriterTermination Nothing))
  WriteConfiguredAdministration outbound -> case slot of
    ConfiguredAdministrationSlot {} ->
      offered
        ConfiguredAdministrationLane
        (configuredAdministrationOffer slot outbound)
    _ -> pure (Just (WriterTermination Nothing))
  WriteAdministration reply -> case slot of
    AdministrationSlot {} -> offered AdministrationLane (administrationOffer slot reply)
    _ -> pure (Just (WriterTermination Nothing))
  WriteFinalAdministration reply acknowledged -> case slot of
    AdministrationSlot {} -> do
      void (offered AdministrationLane (administrationOffer slot reply))
      pure (Just (WriterTermination (Just acknowledged)))
    ConfiguredAdministrationSlot {} -> do
      forM_ (reply >>= projectFinalAdministrationReply) $ \outbound ->
        void (offered ConfiguredAdministrationLane (configuredAdministrationOffer slot outbound))
      pure (Just (WriterTermination (Just acknowledged)))
    _ -> pure (Just (WriterTermination (Just acknowledged)))
  WritePeerCandidate candidate envelope -> case slot of
    PeerSlot {} -> offered PeerCandidateLane (peerCandidateOffer slot candidate envelope)
    _ -> pure (Just (WriterTermination Nothing))
  WritePeerDisposition candidate disposition -> case slot of
    PeerSlot {} -> offered PeerDispositionLane (peerDispositionOffer slot candidate disposition)
    _ -> pure (Just (WriterTermination Nothing))
  WritePeerControl binding envelope -> case slot of
    PeerSlot {} -> offered PeerControlLane (peerControlOffer slot binding envelope)
    _ -> pure (Just (WriterTermination Nothing))
  WritePeerRejection binding disposition -> case slot of
    PeerSlot {} -> offered PeerRejectionLane (peerRejectionOffer slot binding disposition)
    _ -> pure (Just (WriterTermination Nothing))
  WritePeerItem binding attempt envelope _ cell -> case slot of
    PeerSlot _ _ (RuntimePeerConnectionHandlers _ _ _ _ offer _ _) _ -> do
      result <- offer binding attempt envelope
      atomically (claimPeerOutcome context (slotPhysicalRef slot) attempt cell result)
      pure Nothing
    _ -> pure (Just (WriterTermination Nothing))
  CloseWriter acknowledged -> pure (Just (WriterTermination acknowledged))
  where
    offered lane action = do
      result <- action
      atomically
        ( void
            ( appendShellTrace
                (contextTrace context)
                (ShellLaneOffered (slotPhysicalRef slot) lane result)
            )
        )
      pure (laneResult result)
    laneResult LaneOffered = Nothing
    laneResult LaneLost = Just (WriterTermination Nothing)

applicationOffer :: ConnectionSlot -> ApplicationOutbound -> IO RuntimeLaneOffer
applicationOffer (ApplicationSlot _ _ (RuntimeApplicationConnectionHandlers _ offer _) _) = offer
applicationOffer _ = const (pure LaneLost)

administrationOffer :: ConnectionSlot -> Maybe FinalAdminReply -> IO RuntimeLaneOffer
administrationOffer (AdministrationSlot _ (RuntimeAdministrationConnectionHandlers offer _) _) = offer
administrationOffer _ = const (pure LaneLost)

configuredAdministrationOffer ::
  ConnectionSlot ->
  AdministrationOutbound ->
  IO RuntimeLaneOffer
configuredAdministrationOffer
  (ConfiguredAdministrationSlot _ _ (RuntimeConfiguredAdministrationConnectionHandlers offer _) _) =
    offer
configuredAdministrationOffer _ = const (pure LaneLost)

peerCandidateOffer :: ConnectionSlot -> PeerCandidate -> PeerEnvelope -> IO RuntimeLaneOffer
peerCandidateOffer (PeerSlot _ _ (RuntimePeerConnectionHandlers offer _ _ _ _ _ _) _) = offer
peerCandidateOffer _ = \_ _ -> pure LaneLost

peerDispositionOffer :: ConnectionSlot -> PeerCandidate -> PeerHelloDisposition -> IO RuntimeLaneOffer
peerDispositionOffer (PeerSlot _ _ (RuntimePeerConnectionHandlers _ offer _ _ _ _ _) _) = offer
peerDispositionOffer _ = \_ _ -> pure LaneLost

peerControlOffer :: ConnectionSlot -> PeerBinding -> PeerEnvelope -> IO RuntimeLaneOffer
peerControlOffer (PeerSlot _ _ (RuntimePeerConnectionHandlers _ _ offer _ _ _ _) _) = offer
peerControlOffer _ = \_ _ -> pure LaneLost

peerRejectionOffer :: ConnectionSlot -> PeerBinding -> PeerProtocolDisposition -> IO RuntimeLaneOffer
peerRejectionOffer (PeerSlot _ _ (RuntimePeerConnectionHandlers _ _ _ offer _ _ _) _) = offer
peerRejectionOffer _ = \_ _ -> pure LaneLost

diagnosticLoop :: RuntimeContext -> IO ()
diagnosticLoop context = do
  diagnostic <- atomically (readTQueue (contextDiagnosticQueue context))
  delivered <- try (contextDiagnosticHandler context diagnostic)
  case delivered of
    Right () -> diagnosticLoop context
    Left (_ :: SomeException) -> atomically $ do
      writeTVar (contextDiagnosticEnabled context) False
      void (appendShellTrace (contextTrace context) ShellDiagnosticDisabled)

emitDiagnostic :: RuntimeContext -> RuntimeDiagnosticClass -> STM ()
emitDiagnostic context diagnosticClass = do
  enabled <- readTVar (contextDiagnosticEnabled context)
  when enabled $ do
    writeTQueue (contextDiagnosticQueue context) (RuntimeDiagnostic diagnosticClass)
    void (appendShellTrace (contextTrace context) (ShellDiagnosticEmitted diagnosticClass))

spawnEssential :: RuntimeContext -> RuntimeWorkerClass -> IO () -> IO ThreadId
spawnEssential context workerClass action = spawnTracked context workerClass $ do
  outcome <- try action
  case outcome of
    Left (exception :: SomeException)
      | Just ThreadKilled <- fromException exception -> pure ()
      | otherwise -> reportFailure
    Right () -> reportFailure
  where
    reportFailure = do
      current <- myThreadId
      atomically $ do
        void (appendShellTrace (contextTrace context) (ShellChildFailed workerClass current))
        failRuntime context HeraldRuntimeEssentialWorkerFailure

spawnConnection :: RuntimeContext -> ConnectionSlot -> IO ThreadId
spawnConnection context slot = spawnTrackedWithUnmask context ConnectionWriterWorker (Just (slotPhysicalRef slot)) $ \restore workerPhase -> do
  currentAcknowledgement <- newIORef Nothing
  -- Enter the connection action masked. Its exception handler must exist
  -- before cancellation can be delivered, including before the writer first
  -- runs; otherwise a supervised child could finish without closing its slot.
  outcome <-
    try
      ( restore
          ( hookBeforeConnectionWriterBody (contextHooks context) (slotPhysicalRef slot)
              >> writerLoop context slot currentAcknowledgement
          )
      )
  cancelled <- atomically $ do
    phase <- readTVar workerPhase
    writeTVar workerPhase WorkerClosing
    pure (phase == WorkerStopRequested)
  case outcome of
    Left (exception :: SomeException) -> do
      unless (cancelled && isThreadKilled exception)
        $ atomically (recordCurrentWriterFailure context slot)
      finishWriter =<< readIORef currentAcknowledgement
    Right (WriterCommandFailed acknowledged exception) -> do
      unless (cancelled && isThreadKilled exception)
        $ atomically (recordCurrentWriterFailure context slot)
      finishWriter acknowledged
    Right (WriterLoopStopped (WriterTermination acknowledged)) -> finishWriter acknowledged
  where
    finishWriter acknowledged = do
      void (closeSlotIfCurrent context slot)
      uninterruptibleMask_ (safelyCloseSlot slot)
      -- Removing the slot fences every producer of this writer FIFO. A final
      -- drain acknowledgement may already be queued behind the command which
      -- failed, so settle both the current command and those abandoned queued
      -- commands only after the physical close has completed.
      atomically $ do
        abandoned <- flushTQueue (slotWriterQueue slot)
        forM_ acknowledged (void . (`tryPutTMVar` ()))
        forM_ abandoned $ \command ->
          forM_ (writerCommandAcknowledgement command) (void . (`tryPutTMVar` ()))

    isThreadKilled exception = case fromException exception of
      Just ThreadKilled -> True
      _ -> False

recordCurrentWriterFailure :: RuntimeContext -> ConnectionSlot -> STM ()
recordCurrentWriterFailure context slot = do
  current <- case slot of
    ApplicationSlot reference _ _ _ -> lookupCurrentSlot (contextRegistry context) reference
    AdministrationSlot reference _ _ -> lookupCurrentSlot (contextRegistry context) reference
    ConfiguredAdministrationSlot reference _ _ _ ->
      lookupCurrentSlot (contextRegistry context) reference
    PeerSlot reference _ _ _ -> lookupCurrentSlot (contextRegistry context) reference
  when (maybe False ((== slotPhysicalRef slot) . slotPhysicalRef) current)
    $ void (appendShellTrace (contextTrace context) (ShellConnectionWriterFailed (slotPhysicalRef slot)))

spawnTracked :: RuntimeContext -> RuntimeWorkerClass -> IO () -> IO ThreadId
spawnTracked context workerClass action =
  spawnTrackedWithPhase context workerClass (const action)

spawnTrackedWithPhase :: RuntimeContext -> RuntimeWorkerClass -> (TVar RuntimeWorkerPhase -> IO ()) -> IO ThreadId
spawnTrackedWithPhase context workerClass action =
  spawnTrackedWithUnmask context workerClass Nothing (\unmask phase -> unmask (action phase))

spawnTrackedWithUnmask :: RuntimeContext -> RuntimeWorkerClass -> Maybe PhysicalConnectionRef -> ((forall value. IO value -> IO value) -> TVar RuntimeWorkerPhase -> IO ()) -> IO ThreadId
spawnTrackedWithUnmask context workerClass physical action = mask_ $ do
  done <- atomically newEmptyTMVar
  workerPhase <- atomically (newTVar WorkerRunning)
  started <- atomically newEmptyTMVar
  thread <-
    forkIOWithUnmask $ \unmask ->
      (atomically (readTMVar started) >> action unmask workerPhase)
        `finally` atomically
          ( do
              writeTVar workerPhase WorkerFinished
              void (tryPutTMVar done ())
          )
  atomically $ do
    modifyTVar' (contextThreads context) (RuntimeWorker workerClass physical thread done workerPhase :)
    void (appendShellTrace (contextTrace context) (ShellChildStarted workerClass thread))
    putTMVar started ()
  pure thread

spawnSupervisor :: RuntimeContext -> IO ThreadId
spawnSupervisor context = mask_ $ do
  thread <-
    forkIOWithUnmask $ \unmask ->
      ( do
          outcome <- try (unmask (supervisorLoop context))
          case outcome of
            Right () -> pure ()
            Left (_ :: SomeException) -> supervisorFailed context
      )
        `finally` ( flushRuntimeWorkCounts context
                      `finally` atomically (void (tryPutTMVar (contextSupervisorDone context) ()))
                  )
  pure thread

-- Aggregate instrumentation owns no worker and performs no per-event IO. Both
-- normal shutdown and supervisor failure join the tracked workers first. The
-- public completion barrier includes this single final handoff; a failed cleanup
-- is visibly marked incomplete rather than presented as a complete census.
flushRuntimeWorkCounts :: RuntimeContext -> IO ()
flushRuntimeWorkCounts context = forM_ (contextWorkCountsSink context) $ \sink -> do
  (counts, complete) <- atomically $ do
    counts <- snapshotTraceWorkCounts (contextTrace context)
    workers <- readTVar (contextThreads context)
    inFlight <- readTVar (contextSpawnsInFlight context)
    pure (counts, null workers && inFlight == 0)
  forM_ counts (sink complete . Map.toAscList)

supervisorFailed :: RuntimeContext -> IO ()
supervisorFailed context = mask_ $ do
  publicTerminal <- atomically $ do
    requested <- tryReadTMVar (contextTerminalRequest context)
    case requested of
      Just failure@(Left _) -> pure failure
      Just (Right _) -> do
        let failure = HeraldRuntimeFailure HeraldRuntimeEssentialWorkerFailure
        void (appendShellTrace (contextTrace context) (ShellRuntimeFailed HeraldRuntimeEssentialWorkerFailure))
        pure (Left failure)
      Nothing -> do
        let failure = HeraldRuntimeFailure HeraldRuntimeEssentialWorkerFailure
        putTMVar (contextTerminalRequest context) (Left failure)
        writeTVar (contextPhase context) (RuntimeStoppedPhase HeraldRuntimeScopeClosed)
        void (appendShellTrace (contextTrace context) (ShellRuntimeFailed HeraldRuntimeEssentialWorkerFailure))
        pure (Left failure)
  -- This coordinator is deliberately outside the tracked child set.  Its
  -- fallback therefore cannot self-join, and the cleanup operations below do
  -- not depend on the failed path through 'supervisorLoop'.
  cancelAndJoinWorkers context
  atomically $ do
    void (tryPutTMVar (contextExit context) publicTerminal)

supervisorLoop :: RuntimeContext -> IO ()
supervisorLoop context = do
  next <-
    atomically
      $ ( do
            requested <- readTMVar (contextTerminalRequest context)
            inFlight <- readTVar (contextSpawnsInFlight context)
            check (inFlight == 0)
            pure (Left requested)
        )
        `orElse` do
          workers <- readTVar (contextThreads context)
          finished <- forM workers $ \worker@(RuntimeWorker _ _ _ done _) -> do
            completed <- tryReadTMVar done
            pure [worker | completed == Just ()]
          let ready = concat finished
          check (not (null ready))
          pure (Right ready)
  case next of
    Right finished -> do
      cancelAndJoinSelectedWorkers context finished
      supervisorLoop context
    Left terminal -> do
      cancelAndJoinWorkers context
      atomically (void (tryPutTMVar (contextExit context) terminal))

closeSlotIfCurrent :: RuntimeContext -> ConnectionSlot -> IO RuntimeSubmission
closeSlotIfCurrent context slot = atomically $ do
  removed <- case slot of
    ApplicationSlot reference _ _ _ -> closeConnectionSTM context reference False
    AdministrationSlot reference _ _ -> closeConnectionSTM context reference False
    ConfiguredAdministrationSlot reference _ _ _ ->
      closeConnectionSTM context reference False
    PeerSlot reference _ _ _ -> closeConnectionSTM context reference False
  pure (maybe StalePhysicalGeneration (const ConnectionClosed) removed)

closeConnectionSTM ::
  RuntimeContext ->
  ConnectionRef plane ->
  Bool ->
  STM (Maybe ConnectionSlot)
closeConnectionSTM context reference sendCloseMarker = do
  current <- lookupCurrentSlot (contextRegistry context) reference
  case current of
    Just slot
      | sendCloseMarker && slotOwnsFinalClose slot -> do
          -- The already queued final offer and marker retain ownership.  A
          -- generic close cannot invalidate the route or insert its loss ahead
          -- of that FIFO; the writer wrapper calls back with sendCloseMarker
          -- False after the marker, LaneLost, or an exception.
          pure (Just slot)
    _ -> do
      removed <- removeCurrentSlot (contextRegistry context) reference
      forM_ removed $ \slot -> do
        modifyTVar' (contextPubliclyClosed context) (Set.delete (physicalName (slotPhysicalRef slot)))
        slotRetirementAction slot
        void (removeArbiterSource (contextArbiter context) (slotSource slot))
        when sendCloseMarker (writeTQueue (slotWriterQueue slot) (CloseWriter Nothing))
        claimSlotOutcomes context slot
        removeSlotRemoteBinding context slot
        queueSlotLoss context slot
        void
          ( appendShellTrace
              (contextTrace context)
              (ShellConnectionClosed (slotPhysicalRef slot) (slotSource slot))
          )
        emitDiagnostic context RuntimeConnectionDiagnostic
      pure removed

slotOwnsFinalClose :: ConnectionSlot -> Bool
slotOwnsFinalClose = \case
  ApplicationSlot _ ApplicationClosing {} _ _ -> True
  ConfiguredAdministrationSlot _ ConfiguredAdministrationClosing {} _ _ -> True
  PeerSlot _ PeerClosing {} _ _ -> True
  _ -> False

removeSlotRemoteBinding :: RuntimeContext -> ConnectionSlot -> STM ()
removeSlotRemoteBinding context = \case
  PeerSlot _ phase _ _ -> case phase of
    PeerEstablished binding -> removeBinding binding
    PeerClosing (Just binding) -> removeBinding binding
    _ -> pure ()
  _ -> pure ()
  where
    removeBinding binding =
      modifyTVar'
        (contextRemoteBindings context)
        ( Map.update
            (\current -> if current == binding then Nothing else Just current)
            (peerBindingRemoteHeraldEpoch binding)
        )

safelyCloseSlot :: ConnectionSlot -> IO ()
safelyCloseSlot slot = do
  _ <- try (slotCloseAction slot) :: IO (Either SomeException ())
  pure ()

claimPeerOutcome ::
  RuntimeContext ->
  PhysicalConnectionRef ->
  PeerLogicalAttempt ->
  TVar OutcomeCellState ->
  PeerDispatchOutcome ->
  STM ()
claimPeerOutcome context reference attempt cell outcome = do
  sealed <- readTVar (contextCompletionSealed context)
  retained <- readTVar cell
  let (won, successor) = claimOutcomeCell outcome retained
  if won
    then do
      writeTVar cell successor
      origin <- outcomeOrigin context cell
      forgetOutcomeCell context cell
      void (appendShellTrace (contextTrace context) (ShellPeerOutcomeWon reference attempt outcome))
      if sealed
        then emitDiagnostic context RuntimeSuppressionDiagnostic
        else do
          completion <- queueCompletionFrom context (maybe UncorrelatedCompletion OriginCompletion origin) (PeerDispatchObserved attempt outcome)
          forM_ origin $ \obligation ->
            updateOriginLedger context obligation (queueObligationObservation obligation completion)
    else do
      void (appendShellTrace (contextTrace context) (ShellPeerOutcomeSuppressed reference attempt outcome))
      emitDiagnostic context RuntimeSuppressionDiagnostic

claimSlotOutcomes :: RuntimeContext -> ConnectionSlot -> STM ()
claimSlotOutcomes context slot = do
  outcomes <- readTVar (contextOutcomeCells context)
  forM_ outcomes $ \(_, source, _, attempt, cell) ->
    when (source == slotSource slot) $ do
      retained <- readTVar cell
      let (won, successor) = claimOutcomeCell PeerDispatchDeferred retained
      if won
        then do
          writeTVar cell successor
          void (appendShellTrace (contextTrace context) (ShellPeerOutcomeWon (slotPhysicalRef slot) attempt PeerDispatchDeferred))
          sealed <- readTVar (contextCompletionSealed context)
          if sealed
            then emitDiagnostic context RuntimeSuppressionDiagnostic
            else do
              let origin = lookupOutcomeOrigin outcomes cell
              completion <- queueCompletionFrom context (maybe UncorrelatedCompletion OriginCompletion origin) (PeerDispatchObserved attempt PeerDispatchDeferred)
              forM_ origin $ \obligation ->
                updateOriginLedger context obligation (queueObligationObservation obligation completion)
        else do
          void (appendShellTrace (contextTrace context) (ShellPeerOutcomeSuppressed (slotPhysicalRef slot) attempt PeerDispatchDeferred))
          emitDiagnostic context RuntimeSuppressionDiagnostic
  -- Writer commands retain their own one-shot cell. The registry is needed
  -- only while socket loss or drain can still win its first outcome.
  modifyTVar' (contextOutcomeCells context) (strictFilter (\(_, source, _, _, _) -> source /= slotSource slot))

forgetOutcomeCell :: RuntimeContext -> TVar OutcomeCellState -> STM ()
forgetOutcomeCell context cell =
  modifyTVar' (contextOutcomeCells context) (strictFilter (\(_, _, _, _, retained) -> retained /= cell))

strictFilter :: (value -> Bool) -> [value] -> [value]
strictFilter keep values = let retained = filter keep values in length retained `seq` retained

outcomeOrigin :: RuntimeContext -> TVar OutcomeCellState -> STM (Maybe ObligationOrigin)
outcomeOrigin context cell = do
  outcomes <- readTVar (contextOutcomeCells context)
  pure (lookupOutcomeOrigin outcomes cell)

lookupOutcomeOrigin ::
  [(PhysicalConnectionRef, SourceId, ObligationOrigin, PeerLogicalAttempt, TVar OutcomeCellState)] ->
  TVar OutcomeCellState ->
  Maybe ObligationOrigin
lookupOutcomeOrigin outcomes sought =
  foldr
    (\(_, _, origin, _, cell) retained -> if cell == sought then Just origin else retained)
    Nothing
    outcomes

closeScope :: RuntimeContext -> IO ()
closeScope context = do
  atomically (finishRuntime context HeraldRuntimeScopeClosed)
  void (awaitPublicExit context)

awaitPublicExit :: RuntimeContext -> IO (Either HeraldRuntimeFailure HeraldRuntimeExit)
awaitPublicExit context = atomically $ do
  terminal <- readTMVar (contextExit context)
  readTMVar (contextSupervisorDone context)
  pure terminal

tryPublicExit :: RuntimeContext -> IO (Maybe (Either HeraldRuntimeFailure HeraldRuntimeExit))
tryPublicExit context = atomically $ do
  terminal <- tryReadTMVar (contextExit context)
  done <- tryReadTMVar (contextSupervisorDone context)
  pure $ case (terminal, done) of
    (Just result, Just ()) -> Just result
    _ -> Nothing

cancelAndJoinWorkers :: RuntimeContext -> IO ()
cancelAndJoinWorkers context = do
  workers <- atomically (readTVar (contextThreads context))
  cancelAndJoinSelectedWorkers context workers

cancelAndJoinSelectedWorkers :: RuntimeContext -> [RuntimeWorker] -> IO ()
cancelAndJoinSelectedWorkers context workers = do
  current <- myThreadId
  let siblings = [(workerClass, thread, done, workerPhase) | RuntimeWorker workerClass _ thread done workerPhase <- workers, thread /= current]
  forM_ siblings $ \(workerClass, thread, _, workerPhase) -> do
    shouldCancel <- atomically $ do
      phase <- readTVar workerPhase
      case phase of
        WorkerRunning -> do
          writeTVar workerPhase WorkerStopRequested
          void (appendShellTrace (contextTrace context) (ShellChildCancellationRequested workerClass thread))
          pure True
        WorkerStopRequested -> pure False
        WorkerClosing -> pure False
        WorkerFinished -> pure False
        WorkerJoined -> pure False
    when shouldCancel (killThread thread)
  forM_ siblings $ \(workerClass, thread, done, workerPhase) -> atomically $ do
    readTMVar done
    phase <- readTVar workerPhase
    unless (phase == WorkerJoined) $ do
      writeTVar workerPhase WorkerJoined
      void (appendShellTrace (contextTrace context) (ShellChildJoined workerClass thread))
    modifyTVar' (contextThreads context) (strictFilter (\(RuntimeWorker _ _ candidate _ _) -> candidate /= thread))

failRuntime :: RuntimeContext -> HeraldRuntimeFailureClass -> STM ()
failRuntime context failureClass = do
  won <- tryPutTMVar (contextTerminalRequest context) (Left (HeraldRuntimeFailure failureClass))
  when won $ do
    writeTVar (contextPhase context) (RuntimeStoppedPhase HeraldRuntimeScopeClosed)
    void (appendShellTrace (contextTrace context) (ShellRuntimeFailed failureClass))

finishRuntime :: RuntimeContext -> HeraldRuntimeExit -> STM ()
finishRuntime context runtimeExit = do
  won <- tryPutTMVar (contextTerminalRequest context) (Right runtimeExit)
  when won $ do
    writeTVar (contextPhase context) (RuntimeStoppedPhase runtimeExit)
    void (appendShellTrace (contextTrace context) (ShellRuntimeExited runtimeExit))

queueCompletionFrom :: RuntimeContext -> CompletionCorrelation -> RuntimeObservation -> STM CompletionOrdinal
queueCompletionFrom context correlation observation = do
  ordinal <- stateTVar (contextNextCompletion context) (\value -> (value, value + 1))
  let completion = CompletionOrdinal ordinal
  enqueueArbiter
    (contextArbiter context)
    (SourceId RuntimeCompletionSources ordinal)
    (CompletionEnvelope completion correlation observation)
  pure completion

queueTimerOutcome :: RuntimeContext -> TimerAttempt -> TimerOutcome -> IO ()
queueTimerOutcome context attempt outcome = atomically $ do
  phase <- readTVar (contextPhase context)
  case phase of
    RuntimeStoppedPhase _ ->
      void
        ( appendShellTrace
            (contextTrace context)
            (ShellTimerOutcomeSuppressed attempt outcome)
        )
    _ -> do
      void
        ( queueCompletionFrom
            context
            UncorrelatedCompletion
            (TimerObserved attempt outcome)
        )
      void
        ( appendShellTrace
            (contextTrace context)
            (ShellTimerOutcomeQueued attempt outcome)
        )

queueBindingLoss :: RuntimeContext -> TransferBinding -> Maybe ObligationOrigin -> STM ()
queueBindingLoss context binding origin = do
  reservations <- readTVar (contextTransferReservations context)
  if any ((== binding) . fst) (Map.keys reservations)
    then do
      modifyTVar'
        (contextHeldBindingLossOrigins context)
        (Map.alter (Just . appendHeld origin) binding)
    else do
      losses <- readTVar (contextBindingLosses context)
      case Map.lookup binding losses of
        Nothing -> do
          next <- readTVar (contextNextCompletion context)
          let source = SourceId RuntimeCompletionSources next
          completion <-
            queueCompletionFrom
              context
              (BindingLossCompletion binding)
              (bindingLossObservation binding)
          writeTVar
            (contextBindingLosses context)
            (Map.insert binding (BindingLossPending source completion (maybe [] pure origin)) losses)
          forM_ origin $ \obligation ->
            updateOriginLedger context obligation (queueObligationObservation obligation completion)
        Just (BindingLossPending source completion origins) -> do
          let retained = maybe origins (appendUnique origins) origin
          writeTVar
            (contextBindingLosses context)
            (Map.insert binding (BindingLossPending source completion retained) losses)
          forM_ origin $ \obligation ->
            updateOriginLedger context obligation (queueObligationObservation obligation completion)
        Just (BindingLossSelected completion origins) -> do
          let retained = maybe origins (appendUnique origins) origin
          writeTVar
            (contextBindingLosses context)
            (Map.insert binding (BindingLossSelected completion retained) losses)
          forM_ origin $ \obligation ->
            updateOriginLedger context obligation (queueObligationObservation obligation completion)
        Just (BindingLossStepped _) ->
          forM_ origin $ \obligation ->
            updateOriginLedger context obligation (markObligationKernelStepped obligation)
  where
    appendHeld Nothing Nothing = HeldBindingLoss []
    appendHeld Nothing (Just retained) = retained
    appendHeld (Just obligation) Nothing = HeldBindingLoss [obligation]
    appendHeld (Just obligation) (Just (HeldBindingLoss retained)) =
      HeldBindingLoss (appendUnique retained obligation)

bindingLossObservation :: TransferBinding -> RuntimeObservation
bindingLossObservation = \case
  TransferApplicationBinding binding -> ApplicationBindingLost binding
  TransferAdministrationBinding binding -> AdministrationBindingLost binding
  TransferPeerBinding binding -> PeerBindingLost binding

appendUnique :: (Eq value) => [value] -> value -> [value]
appendUnique retained value
  | value `elem` retained = retained
  | otherwise = retained <> [value]

settleSelectedCompletion :: RuntimeContext -> BatchOrdinal -> SelectedIngress -> STM ()
settleSelectedCompletion context batchOrdinal selection =
  forM_ (selectedCompletion selection) $ \(completion, correlation) -> do
    case correlation of
      OriginCompletion origin ->
        updateOriginLedger context origin (markObligationKernelStepped origin)
      BindingLossCompletion binding -> do
        losses <- readTVar (contextBindingLosses context)
        case Map.lookup binding losses of
          Just (BindingLossSelected retainedCompletion origins)
            | retainedCompletion == completion -> do
                forM_ origins $ \origin ->
                  updateOriginLedger context origin (markObligationKernelStepped origin)
                writeTVar
                  (contextBindingLosses context)
                  (Map.insert binding (BindingLossStepped batchOrdinal) losses)
          _ -> pure ()
      UncorrelatedCompletion -> pure ()
    markCompletionResolved context completion

markCompletionResolved :: RuntimeContext -> CompletionOrdinal -> STM ()
markCompletionResolved context completion = do
  modifyTVar' (contextResolvedCompletions context) (Set.insert completion)
  frontier <- readTVar (contextCompletionSteppedThrough context)
  resolved <- readTVar (contextResolvedCompletions context)
  let (successorFrontier, retained) = advanceCompletionFrontier frontier resolved
  writeTVar (contextCompletionSteppedThrough context) successorFrontier
  writeTVar (contextResolvedCompletions context) retained

advanceCompletionFrontier ::
  Maybe CompletionOrdinal ->
  Set CompletionOrdinal ->
  (Maybe CompletionOrdinal, Set CompletionOrdinal)
advanceCompletionFrontier frontier resolved = case frontier of
  Nothing -> (Nothing, resolved)
  Just current ->
    let next = successorCompletion current
     in if next `Set.member` resolved
          then advanceCompletionFrontier (Just next) (Set.delete next resolved)
          else (Just current, resolved)

successorCompletion :: CompletionOrdinal -> CompletionOrdinal
successorCompletion (CompletionOrdinal ordinal) = CompletionOrdinal (ordinal + 1)

updateOriginLedger ::
  RuntimeContext ->
  ObligationOrigin ->
  (DrainLedger -> DrainLedger) ->
  STM ()
updateOriginLedger context (ObligationOrigin batch _) update = do
  cut <- readTVar (contextDrainCut context)
  when (maybe True (batch <=) cut)
    $ modifyTVar' (contextDrainLedger context) update

queueSlotLoss :: RuntimeContext -> ConnectionSlot -> STM ()
queueSlotLoss context slot = do
  phase <- readTVar (contextPhase context)
  sealed <- readTVar (contextCompletionSealed context)
  case phase of
    RuntimeStoppedPhase _ -> pure ()
    _ | sealed -> emitDiagnostic context RuntimeSuppressionDiagnostic
    _ -> case slot of
      ApplicationSlot _ (ApplicationDispositionPending candidate) _ _ -> candidateLost candidate
      ApplicationSlot _ (ApplicationClaimPending candidate) _ _ -> candidateLost candidate
      ApplicationSlot _ (ApplicationAdmissionDeferred candidate) _ _ -> candidateLost candidate
      ApplicationSlot _ (ApplicationEstablished binding) _ _ -> queueBindingLoss context (TransferApplicationBinding binding) Nothing
      ApplicationSlot _ (ApplicationClosing (Just binding)) _ _ -> queueBindingLoss context (TransferApplicationBinding binding) Nothing
      AdministrationSlot {} -> void (queueCompletionFrom context UncorrelatedCompletion (AdministrationBindingLost (contextAdministrationBinding context)))
      ConfiguredAdministrationSlot _ (ConfiguredAdministrationEstablished binding) _ _ ->
        queueBindingLoss context (TransferAdministrationBinding binding) Nothing
      ConfiguredAdministrationSlot _ (ConfiguredAdministrationClosing (Just binding)) _ _ ->
        queueBindingLoss context (TransferAdministrationBinding binding) Nothing
      PeerSlot _ (PeerEstablished binding) _ _ -> queueBindingLoss context (TransferPeerBinding binding) Nothing
      PeerSlot _ (PeerClosing (Just binding)) _ _ -> queueBindingLoss context (TransferPeerBinding binding) Nothing
      _ -> pure ()
  where
    candidateLost candidate = void (queueCompletionFrom context UncorrelatedCompletion (ApplicationCandidateLost candidate))

sealAtCut :: RuntimeContext -> BatchOrdinal -> STM ()
sealAtCut context routedBatch = do
  barrier <- readTVar (contextDrainBarrier context)
  case barrier of
    Just (drainId, cut, False) | routedBatch >= cut -> do
      outcomes <- readTVar (contextOutcomeCells context)
      forM_ outcomes $ \(reference, _, origin@(ObligationOrigin batch _), attempt, cell) ->
        when (batch <= cut) $ do
          retained <- readTVar cell
          let (won, successor) = claimOutcomeCell PeerDispatchDeferred retained
          when won $ do
            writeTVar cell successor
            void (appendShellTrace (contextTrace context) (ShellPeerOutcomeWon reference attempt PeerDispatchDeferred))
            completion <- queueCompletionFrom context (OriginCompletion origin) (PeerDispatchObserved attempt PeerDispatchDeferred)
            updateOriginLedger context origin (queueObligationObservation origin completion)
      modifyTVar' (contextOutcomeCells context) (strictFilter (\(_, _, ObligationOrigin batch _, _, _) -> batch > cut))
      nextCompletion <- readTVar (contextNextCompletion context)
      let completionCut = CompletionOrdinal (nextCompletion - 1)
      modifyTVar' (contextDrainLedger context) (sealDrainLedger cut completionCut)
      writeTVar (contextCompletionSealed context) True
      writeTVar (contextDrainBarrier context) (Just (drainId, cut, True))
    _ -> pure ()

tryQueueBarrier :: RuntimeContext -> STM ()
tryQueueBarrier context = do
  barrier <- readTVar (contextDrainBarrier context)
  ledger <- readTVar (contextDrainLedger context)
  stepped <- readTVar (contextCompletionSteppedThrough context)
  case barrier of
    Just (drainId, _, True)
      | maybe False (`drainBarrierReady` ledger) stepped -> do
          writeTVar (contextDrainBarrier context) Nothing
          void (queueCompletionFrom context UncorrelatedCompletion (DrainBarrierObserved drainId))
          void (appendShellTrace (contextTrace context) (ShellDrainBarrierQueued drainId))
    _ -> pure ()

nextStepOrdinal :: RuntimeContext -> STM StepOrdinal
nextStepOrdinal context = StepOrdinal <$> stateTVar (contextNextStep context) (\value -> (value, value + 1))

nextBatchOrdinal :: RuntimeContext -> STM BatchOrdinal
nextBatchOrdinal context = BatchOrdinal <$> stateTVar (contextNextBatch context) (\value -> (value, value + 1))

rejectProtocol :: RuntimeContext -> ConnectionRef ApplicationPlane -> STM (Maybe SelectedIngress)
rejectProtocol context reference = do
  void (closeConnectionSTM context reference True)
  pure Nothing

rejectConfiguredAdministrationProtocol ::
  RuntimeContext ->
  ConnectionRef ConfiguredAdministrationPlane ->
  STM (Maybe SelectedIngress)
rejectConfiguredAdministrationProtocol context reference = do
  void (closeConnectionSTM context reference True)
  pure Nothing

currentPeerBinding :: RuntimeContext -> SourceId -> ConnectionRef PeerPlane -> PeerBinding -> HeraldInputBody -> STM (Maybe SelectedIngress)
currentPeerBinding context source reference binding body = do
  current <- lookupCurrentSlot (contextRegistry context) reference
  case current of
    Just (PeerSlot _ (PeerEstablished currentBinding) _ _)
      | currentBinding == binding ->
          pure (Just (SelectedIngress body Nothing Nothing))
    Just (PeerSlot {}) -> do
      void (closeConnectionSTM context reference True)
      pure Nothing
    _ -> do
      void (appendShellTrace (contextTrace context) (ShellStaleSuppressed (Just (physicalName (PhysicalPeerRef reference))) (Just source)))
      pure Nothing

helloMatchesPhase :: PeerCandidate -> PeerHello -> PeerPhase -> Bool
helloMatchesPhase candidate hello = \case
  PeerFresh _ -> peerCandidateConnectionNonce candidate == peerHelloConnectionNonce hello
  PeerCandidateRoutePending nonce ->
    peerCandidateConnectionNonce candidate == nonce
      && peerHelloConnectionNonce hello == nonce
  PeerCandidateCurrent current -> current == candidate
  PeerDispositionPending current -> current == candidate
  PeerEstablished _ -> False
  PeerClosing _ -> False

setApplicationPhase :: ApplicationPhase -> ConnectionSlot -> ConnectionSlot
setApplicationPhase phase (ApplicationSlot reference _ handlers queue) = ApplicationSlot reference phase handlers queue
setApplicationPhase _ slot = slot

setConfiguredAdministrationPhase ::
  ConfiguredAdministrationPhase ->
  ConnectionSlot ->
  ConnectionSlot
setConfiguredAdministrationPhase
  phase
  (ConfiguredAdministrationSlot reference _ handlers queue) =
    ConfiguredAdministrationSlot reference phase handlers queue
setConfiguredAdministrationPhase _ slot = slot

setPeerPhase :: PeerPhase -> ConnectionSlot -> ConnectionSlot
setPeerPhase phase (PeerSlot reference _ handlers queue) = PeerSlot reference phase handlers queue
setPeerPhase _ slot = slot

wakeDormant :: RuntimeContext -> HeraldEpoch -> STM ()
wakeDormant context destination = do
  dormant <- readTVar (contextDormantWork context)
  let tickets = Map.findWithDefault [] destination dormant
  writeTVar (contextDormantWork context) (Map.delete destination dormant)
  binding <- Map.lookup destination <$> readTVar (contextRemoteBindings context)
  forM_ binding $ \_ ->
    forM_ tickets (scheduleDelayed context destination)

workSource :: SourceId
workSource = SourceId LocalPeerWorkSources 0

oracleSource :: SourceId
oracleSource = SourceId OracleSources 0
