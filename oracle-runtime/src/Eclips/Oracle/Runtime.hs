-- | Scoped ownership and lifecycle of one current-build Oracle/Raft replica.
module Eclips.Oracle.Runtime
  ( OracleRuntime,
    OracleRuntimeConfiguration,
    OracleRuntimeStartupFault (..),
    OracleRuntimeFailure (..),
    OracleRuntimeExit (..),
    OracleRuntimeStatus,
    OracleRuntimeHealth,
    oracleRuntimeHealth,
    oracleHealthNode,
    oracleHealthTerm,
    oracleHealthGeneration,
    oracleHealthEffectiveConfiguration,
    RuntimeElectionTimeoutSource,
    RuntimeDelay,
    systemRuntimeElectionTimeoutSource,
    runtimeElectionTimeoutSource,
    runtimeDelay,
    oracleRuntimeConfiguration,
    oracleRuntimeLearnerConfiguration,
    configureOracleRuntimeDiagnostics,
    OracleRuntimeRecordingMode (..),
    configureOracleRuntimeRecording,
    OracleStateDigestMode (..),
    configureOracleRuntimeStateDigest,
    configureOracleRuntimeReplicaResolver,
    OracleVoterCheckpoint (..),
    configureOracleRuntimeVoterCheckpoint,
    configureOracleRuntimeSubmissionCheckpoint,
    configureOracleRuntimeWatchAuthorityDelay,
    startOracleRuntime,
    withOracleRuntime,
    stopOracleRuntime,
    awaitOracleRuntimeExit,
    oracleRuntimeStatus,
    oracleRuntimeNode,
    oracleRuntimeRole,
    oracleRuntimeTerm,
    oracleRuntimeLeaderHint,
    oracleRuntimeServiceReady,
    oracleRuntimeAppliedControlIndex,
    oracleRuntimeLabelRetiredThrough,
    oracleRuntimeCompletedWorkflowCount,
    oracleRuntimeAppliedRaftIndex,
    oracleRuntimeConfigurationFrontier,
    oracleRuntimeVoterConfiguration,
    oracleRuntimeReplicaRegistrations,
    oracleRuntimePendingVoterChange,
    oracleRuntimeVoterChange,
    oracleRuntimeMembershipGeneration,
    OracleRuntimeEvent (..),
    oracleRuntimeRecording,
  )
where

import Control.Concurrent
  ( forkFinally,
    forkIOWithUnmask,
    killThread,
  )
import Control.Concurrent.MVar
  ( newEmptyMVar,
    putMVar,
    readMVar,
  )
import Control.Concurrent.STM
  ( TMVar,
    TQueue,
    atomically,
    check,
    newEmptyTMVar,
    newTQueue,
    newTVar,
    orElse,
    putTMVar,
    readTMVar,
    readTVar,
    tryPutTMVar,
    tryReadTMVar,
    writeTQueue,
    writeTVar,
  )
import Control.Exception
  ( SomeException,
    catch,
    displayException,
    finally,
    mask,
    onException,
    throwIO,
    try,
  )
import Control.Monad
  ( void,
    when,
  )
import Data.List (find)
import Data.Word (Word64)
import Eclips.Domain.Identity (ControlIndex, HeraldEpoch, controlIndex)
import Eclips.Domain.Membership (HeraldMembershipGeneration, heraldMembershipHistoryCurrent)
import Eclips.Oracle.Command (OracleCommand)
import Eclips.Oracle.Genesis
  ( OracleGenesis,
    checkOracleGenesis,
    checkedOracleRaftNativeConfiguration,
    checkedOracleRaftVoterBindings,
    raftVoterBindingHeraldEpoch,
    raftVoterBindingNode,
  )
import Eclips.Oracle.Identity (OracleClientRequestId)
import Eclips.Oracle.Runtime.Internal.Adapter (runCommittedEntryAdapter)
import Eclips.Oracle.Runtime.Internal.Connection (queryOracleRuntimeHealth)
import Eclips.Oracle.Runtime.Internal.Coordination
  ( abandonRuntimeProposals,
    newRuntimeCoordination,
  )
import Eclips.Oracle.Runtime.Internal.Dispatcher (runRaftDispatcher)
import Eclips.Oracle.Runtime.Internal.Entropy
  ( runtimeElectionTimeoutSource,
    systemRuntimeElectionTimeoutSource,
  )
import Eclips.Oracle.Runtime.Internal.Hello (activeHeraldIdentities)
import Eclips.Oracle.Runtime.Internal.OracleOwner (runOracleOwner)
import Eclips.Oracle.Runtime.Internal.RaftOwner (runRaftOwner)
import Eclips.Oracle.Runtime.Internal.Recording
  ( recordRuntimeEvent,
    runRecordingOwner,
  )
import Eclips.Oracle.Runtime.Internal.Scope
  ( abortRuntimeScope,
    claimRuntimeScopeClose,
    forkRuntimeChild,
    joinRuntimeScope,
    newRuntimeScope,
    runtimeScopeObservedFailure,
  )
import Eclips.Oracle.Runtime.Internal.Timer
  ( runRuntimeTimer,
    systemRuntimeDelay,
  )
import Eclips.Oracle.Runtime.Internal.Types
  ( AdapterCommand (..),
    OracleOwnerCommand (..),
    OracleRuntime (..),
    OracleRuntimeConfiguration (..),
    OracleRuntimeEvent (..),
    OracleRuntimeExit (..),
    OracleRuntimeFailure (..),
    OracleRuntimeHealth (..),
    OracleRuntimeRecordingMode (..),
    OracleRuntimeStartupFault (..),
    OracleRuntimeStatus (..),
    OracleVoterCheckpoint (..),
    RaftDispatcherCommand (..),
    RaftOwnerCommand (..),
    RecordingCommand (..),
    RuntimeCoordination (..),
    RuntimeDiagnosticSink (..),
    RuntimeElectionTimeoutSource,
    RuntimeRaftTransport (..),
    RuntimeReplicaRegistry (..),
    RuntimeSubmissionCheckpoint (..),
    RuntimeTimerCommand (BarrierRuntimeTimer, StopRuntimeTimer),
    RuntimeTimers (..),
    WatchLedgerCommand (..),
  )
import Eclips.Oracle.Runtime.Internal.WatchAvailability
  ( RuntimeDelay,
    runtimeDelay,
  )
import Eclips.Oracle.Runtime.Internal.WatchLedgerOwner (runWatchLedgerOwner)
import Eclips.Oracle.State (OracleStateDigestMode (..), oracleCheckedMembershipHistory, oracleCompletedWorkflowCount, oracleHeraldCatalogue, oracleLabelRetiredThrough)
import Eclips.Oracle.Transition (initialOracleWithStateDigestMode)
import Eclips.Oracle.Voter
import Eclips.Raft.Configuration (RaftCatchUpFrontier, RaftConfigurationRef, RaftVotingConfiguration, genesisRaftConfigurationRef, raftVoterSet, stableRaftConfiguration)
import Eclips.Raft.Genesis
  ( RaftGenesis,
    checkRaftGenesis,
    checkRaftLearnerGenesis,
    checkedRaftLocalNode,
    checkedRaftNativeConfiguration,
    raftNativeVoters,
  )
import Eclips.Raft.Identity
  ( RaftLogIndex,
    RaftNodeId,
    RaftTerm,
    raftLogIndex,
    raftTerm,
  )
import Eclips.Raft.Input
  ( fireElectionTimer,
    fireHeartbeatTimer,
    fireRecentLeaderTimer,
  )
import Eclips.Raft.State
  ( RaftRole (RaftFollower),
  )
import Eclips.Raft.State qualified as Raft

oracleRuntimeConfiguration ::
  RaftGenesis ->
  OracleGenesis ->
  HeraldEpoch ->
  RuntimeElectionTimeoutSource ->
  Either OracleRuntimeStartupFault OracleRuntimeConfiguration
oracleRuntimeConfiguration raftGenesis oracleGenesis localCohost timeoutSource = do
  checkedRaft <-
    either (Left . RuntimeRaftGenesisRejected) Right (checkRaftGenesis raftGenesis)
  checkedOracle <-
    either (Left . RuntimeOracleGenesisRejected) Right (checkOracleGenesis oracleGenesis)
  let native = checkedRaftNativeConfiguration checkedRaft
      oracleNative = checkedOracleRaftNativeConfiguration checkedOracle
      localNode = checkedRaftLocalNode checkedRaft
      bindings = checkedOracleRaftVoterBindings checkedOracle
  if native == oracleNative
    then Right ()
    else Left RuntimeRaftOracleConfigurationMismatch
  if fmap raftVoterBindingNode bindings == raftNativeVoters native
    then Right ()
    else Left RuntimeRaftVoterBindingMismatch
  binding <-
    maybe
      (Left (RuntimeLocalCohostBindingMissing localNode))
      Right
      (find ((== localNode) . raftVoterBindingNode) bindings)
  let expectedCohost = raftVoterBindingHeraldEpoch binding
  if localCohost == expectedCohost
    then Right ()
    else Left (RuntimeLocalCohostMismatch expectedCohost localCohost)
  Right
    OracleRuntimeConfiguration
      { configuredRaftGenesis = checkedRaft,
        configuredOracleGenesis = checkedOracle,
        configuredStateDigestMode = OracleStateDigestDisabled,
        configuredLocalCohost = localCohost,
        configuredElectionTimeoutSource = timeoutSource,
        configuredWatchAuthorityDelay = systemRuntimeDelay,
        configuredRaftTransport = RuntimeRaftTransport (\_ _ _ _ -> pure ()),
        configuredReplicaRegistry = RuntimeReplicaRegistry (\_ _ -> pure ()),
        configuredReplicaResolver = const (pure Nothing),
        configuredReplicaBootstrap = Nothing,
        configuredVoterCheckpoint = const (pure ()),
        configuredDiagnosticSink = RuntimeDiagnosticSink (const (pure ())),
        configuredRecordingMode = OracleDiagnosticsOnly,
        configuredSubmissionCheckpoint = RuntimeSubmissionCheckpoint (\_ _ _ -> pure ())
      }

-- | Start a registered non-genesis node with an empty native log and the same
-- immutable Oracle genesis. Committed history must reconstruct its registration;
-- the supplied registration permits transport bootstrap, never voting.
oracleRuntimeLearnerConfiguration ::
  RaftGenesis ->
  OracleGenesis ->
  OracleReplicaRegistration ->
  RuntimeElectionTimeoutSource ->
  Either OracleRuntimeStartupFault OracleRuntimeConfiguration
oracleRuntimeLearnerConfiguration raftGenesis oracleGenesis registration timeoutSource = do
  checkedRaft <- either (Left . RuntimeRaftGenesisRejected) Right (checkRaftLearnerGenesis raftGenesis)
  checkedOracle <- either (Left . RuntimeOracleGenesisRejected) Right (checkOracleGenesis oracleGenesis)
  let local = checkedRaftLocalNode checkedRaft
  if local == replicaRegistrationNode registration
    then Right ()
    else Left (RuntimeLocalReplicaRegistrationMismatch local)
  if checkedRaftNativeConfiguration checkedRaft == checkedOracleRaftNativeConfiguration checkedOracle
    then Right ()
    else Left RuntimeRaftOracleConfigurationMismatch
  Right
    OracleRuntimeConfiguration
      { configuredRaftGenesis = checkedRaft,
        configuredOracleGenesis = checkedOracle,
        configuredStateDigestMode = OracleStateDigestDisabled,
        configuredLocalCohost = replicaRegistrationHost registration,
        configuredElectionTimeoutSource = timeoutSource,
        configuredWatchAuthorityDelay = systemRuntimeDelay,
        configuredRaftTransport = RuntimeRaftTransport (\_ _ _ _ -> pure ()),
        configuredReplicaRegistry = RuntimeReplicaRegistry (\_ _ -> pure ()),
        configuredReplicaResolver = const (pure Nothing),
        configuredReplicaBootstrap = Just registration,
        configuredVoterCheckpoint = const (pure ()),
        configuredDiagnosticSink = RuntimeDiagnosticSink (const (pure ())),
        configuredRecordingMode = OracleDiagnosticsOnly,
        configuredSubmissionCheckpoint = RuntimeSubmissionCheckpoint (\_ _ _ -> pure ())
      }

oracleRuntimeConfigurationFrontier :: OracleRuntimeStatus -> Maybe RaftCatchUpFrontier
oracleRuntimeConfigurationFrontier = statusConfigurationFrontier

oracleRuntimeAppliedRaftIndex :: OracleRuntimeStatus -> RaftLogIndex
oracleRuntimeAppliedRaftIndex = statusAppliedRaftIndex
oracleRuntimeVoterConfiguration :: OracleRuntimeStatus -> VoterConfiguration
oracleRuntimeVoterConfiguration = statusVoterConfiguration
oracleRuntimeReplicaRegistrations :: OracleRuntimeStatus -> [OracleReplicaRegistration]
oracleRuntimeReplicaRegistrations = statusReplicaRegistrations
oracleRuntimePendingVoterChange :: OracleRuntimeStatus -> Maybe VoterChange
oracleRuntimePendingVoterChange = statusPendingVoterChange

-- | Query retained workflow progress at this owner's applied prefix. Callers
-- display the accompanying runtime status when freshness matters.
oracleRuntimeVoterChange :: OracleRuntime -> VoterChangeId -> IO (Maybe VoterChange)
oracleRuntimeVoterChange runtime changeId = do
  result <- atomically newEmptyTMVar
  offered <- atomically $ do
    status <- readTVar runtime.runtimeStatusCell
    if not status.statusAccepting
      then pure False
      else do
        writeTQueue runtime.runtimeOracleCommands (QueryOracleVoterChangeOwner changeId result)
        pure True
  if not offered
    then pure Nothing
    else
      atomically
        $ readTMVar result
          `orElse` (runtimeScopeObservedFailure runtime.runtimeScope >> pure Nothing)
          `orElse` (readTVar runtime.runtimeStatusCell >>= check . not . statusAccepting >> pure Nothing)

-- | Select the diagnostic witness before startup. Every replica in a run must
-- use the same mode; it cannot change when a checkpoint is installed.
configureOracleRuntimeStateDigest :: OracleStateDigestMode -> OracleRuntimeConfiguration -> OracleRuntimeConfiguration
configureOracleRuntimeStateDigest mode configuration = configuration {configuredStateDigestMode = mode}

-- | Restricted same-run discovery resolves a registration independently of
-- the Raft lane. The result grants only provisional transport reachability;
-- ordinary committed replay must establish the identical registration.
configureOracleRuntimeReplicaResolver ::
  (RaftNodeId -> IO (Maybe OracleReplicaRegistration)) -> OracleRuntimeConfiguration -> OracleRuntimeConfiguration
configureOracleRuntimeReplicaResolver resolve configuration = configuration {configuredReplicaResolver = resolve}

configureOracleRuntimeVoterCheckpoint ::
  (OracleVoterCheckpoint -> IO ()) -> OracleRuntimeConfiguration -> OracleRuntimeConfiguration
configureOracleRuntimeVoterCheckpoint checkpoint configuration = configuration {configuredVoterCheckpoint = checkpoint}

configureOracleRuntimeDiagnostics ::
  (OracleRuntimeEvent -> IO ()) ->
  OracleRuntimeConfiguration ->
  OracleRuntimeConfiguration
configureOracleRuntimeDiagnostics sink configuration =
  configuration {configuredDiagnosticSink = RuntimeDiagnosticSink sink}

-- | Opt in to retaining exact finite-run evidence. Ordinary configurations
-- stream diagnostics without retaining a lifetime transcript.
configureOracleRuntimeRecording :: OracleRuntimeRecordingMode -> OracleRuntimeConfiguration -> OracleRuntimeConfiguration
configureOracleRuntimeRecording mode configuration =
  configuration {configuredRecordingMode = mode}

-- | Install a deterministic observation point after semantic preflight and
-- before the reserved request reaches Raft. The default returns immediately.
-- A callback exception is propagated after the still-pre-Raft semantic
-- reservation has been released.
configureOracleRuntimeSubmissionCheckpoint ::
  (OracleClientRequestId -> OracleCommand -> ControlIndex -> IO ()) ->
  OracleRuntimeConfiguration ->
  OracleRuntimeConfiguration
configureOracleRuntimeSubmissionCheckpoint checkpoint configuration =
  configuration
    { configuredSubmissionCheckpoint = RuntimeSubmissionCheckpoint checkpoint
    }

-- | Replace the physical delay used to bound an already-admitted watch while
-- local Raft authority is unresolved. This is an explicit deterministic test
-- seam; production configurations use the runtime timer boundary.
configureOracleRuntimeWatchAuthorityDelay ::
  RuntimeDelay ->
  OracleRuntimeConfiguration ->
  OracleRuntimeConfiguration
configureOracleRuntimeWatchAuthorityDelay delay configuration =
  configuration
    { configuredWatchAuthorityDelay = delay
    }

startOracleRuntime :: OracleRuntimeConfiguration -> IO (Either OracleRuntimeFailure OracleRuntime)
startOracleRuntime configuration = mask $ \_ -> do
  supervisorDone <- newEmptyMVar
  let initialOracleState =
        case initialOracleWithStateDigestMode configuration.configuredStateDigestMode configuration.configuredOracleGenesis of
          Left fault -> error ("checked Oracle genesis failed initialization: " <> show fault)
          Right state -> state
  runtime <- atomically $ do
    raftCommands <- newTQueue
    raftBatches <- newTQueue
    oracleCommands <- newTQueue
    adapterCommands <- newTQueue
    watchCommands <- newTQueue
    recordingCommands <- newTQueue
    finalRecording <- newEmptyTMVar
    coordination <- newRuntimeCoordination
    scope <- newRuntimeScope
    electionCommands <- newTQueue
    heartbeatCommands <- newTQueue
    recentLeaderCommands <- newTQueue
    exitCell <- newEmptyTMVar
    let node = checkedRaftLocalNode configuration.configuredRaftGenesis
    status <-
      newTVar
        OracleRuntimeStatus
          { statusNode = node,
            statusRole = RaftFollower,
            statusTerm = raftTerm 0,
            statusLeaderHint = Nothing,
            statusServiceReady = False,
            statusAppliedControlIndex = controlIndex 0,
            statusLabelRetiredThrough = oracleLabelRetiredThrough initialOracleState,
            statusCompletedWorkflowCount = oracleCompletedWorkflowCount initialOracleState,
            statusAppliedRaftIndex = raftLogIndex 0,
            statusVoterConfiguration = oracleVoterConfiguration initialOracleState,
            statusReplicaRegistrations = oracleReplicaRegistrations initialOracleState,
            statusPendingVoterChange = Nothing,
            statusConfigurationFrontier = Nothing,
            statusEffectiveConfiguration =
              (genesisRaftConfigurationRef, stableRaftConfiguration (either (error . show) id (raftVoterSet (raftNativeVoters (checkedRaftNativeConfiguration configuration.configuredRaftGenesis))))),
            statusConfigurationGeneration = 0,
            statusMembershipHistory = oracleCheckedMembershipHistory initialOracleState,
            statusHeraldCatalogue = oracleHeraldCatalogue initialOracleState,
            statusActiveHeralds = activeHeraldIdentities (oracleHeraldCatalogue initialOracleState) (oracleCheckedMembershipHistory initialOracleState),
            statusAccepting = True
          }
    pure
      OracleRuntime
        { runtimeConfiguration = configuration,
          runtimeRaftCommands = raftCommands,
          runtimeRaftBatches = raftBatches,
          runtimeOracleCommands = oracleCommands,
          runtimeAdapterCommands = adapterCommands,
          runtimeWatchCommands = watchCommands,
          runtimeRecordingCommands = recordingCommands,
          runtimeFinalRecording = finalRecording,
          runtimeStatusCell = status,
          runtimeCoordination = coordination,
          runtimeScope = scope,
          runtimeTimers = RuntimeTimers electionCommands heartbeatCommands recentLeaderCommands,
          runtimeExitCell = exitCell,
          runtimeSupervisorDone = supervisorDone
        }
  started <- try @SomeException (startChildren runtime >> startupBarrier runtime)
  startup <- case started of
    Left exception ->
      pure
        ( Left
            (RuntimeEssentialChildFailed "runtime-startup" (displayException exception))
        )
    Right outcome -> pure outcome
  case startup of
    Left failure -> do
      abortStartedRuntime runtime failure
      pure (Left failure)
    Right () -> do
      startLifecycleSupervisor runtime
      pure (Right runtime)

startChildren :: OracleRuntime -> IO ()
startChildren runtime = do
  let scope = runtime.runtimeScope
      recording = runtime.runtimeRecordingCommands
      sink = runRuntimeDiagnosticSink runtime.runtimeConfiguration.configuredDiagnosticSink
  forkRuntimeChild
    scope
    "recording-owner"
    (runRecordingOwner runtime.runtimeConfiguration.configuredRecordingMode sink recording runtime.runtimeFinalRecording)
  forkRuntimeChild
    scope
    "oracle-owner"
    ( runOracleOwner
        runtime.runtimeConfiguration.configuredOracleGenesis
        runtime.runtimeConfiguration.configuredStateDigestMode
        runtime.runtimeOracleCommands
        runtime.runtimeRaftCommands
        runtime.runtimeCoordination
        runtime.runtimeStatusCell
        recording
        scope
    )
  forkRuntimeChild
    scope
    "watch-ledger-owner"
    (runWatchLedgerOwner runtime.runtimeWatchCommands runtime.runtimeStatusCell recording scope)
  forkRuntimeChild
    scope
    "committed-entry-adapter"
    ( runCommittedEntryAdapter
        runtime.runtimeConfiguration.configuredVoterCheckpoint
        runtime.runtimeAdapterCommands
        runtime.runtimeOracleCommands
        runtime.runtimeWatchCommands
        runtime.runtimeRaftCommands
        runtime.runtimeStatusCell
        runtime.runtimeCoordination
        scope
    )
  forkRuntimeChild
    scope
    "raft-owner"
    ( runRaftOwner
        runtime.runtimeConfiguration.configuredRaftGenesis
        runtime.runtimeRaftCommands
        runtime.runtimeRaftBatches
        runtime.runtimeStatusCell
        runtime.runtimeCoordination
        runtime.runtimeConfiguration.configuredVoterCheckpoint
        recording
        scope
    )
  forkRuntimeChild scope "raft-dispatcher" (runRaftDispatcher runtime)
  forkRuntimeChild
    scope
    "election-timer"
    ( runRuntimeTimer
        runtime.runtimeTimers.electionTimerCommands
        (\generation -> offerWhileAccepting runtime (StepRaftOwner (fireElectionTimer generation) Nothing))
    )
  forkRuntimeChild
    scope
    "heartbeat-timer"
    ( runRuntimeTimer
        runtime.runtimeTimers.heartbeatTimerCommands
        (\generation -> offerWhileAccepting runtime (StepRaftOwner (fireHeartbeatTimer generation) Nothing))
    )

  forkRuntimeChild
    scope
    "recent-leader-timer"
    ( runRuntimeTimer
        runtime.runtimeTimers.recentLeaderTimerCommands
        (\generation -> offerWhileAccepting runtime (StepRaftOwner (fireRecentLeaderTimer generation) Nothing))
    )

startupBarrier :: OracleRuntime -> IO (Either OracleRuntimeFailure ())
startupBarrier runtime = do
  sequenceRuntimeChecks
    [ recordingBarrier runtime,
      barrierQueueHealthy runtime runtime.runtimeOracleCommands BarrierOracleOwner,
      barrierQueueHealthy runtime runtime.runtimeWatchCommands BarrierWatchLedger,
      barrierQueueHealthy runtime runtime.runtimeAdapterCommands BarrierAdapter,
      -- The initial Raft batch is already ahead of this dispatcher barrier.
      -- D -> R -> D reaches the fixed point through timeout selection and arm.
      barrierQueueHealthy runtime runtime.runtimeRaftBatches BarrierRaftDispatcher,
      barrierQueueHealthy runtime runtime.runtimeRaftCommands BarrierRaftOwner,
      barrierQueueHealthy runtime runtime.runtimeRaftBatches BarrierRaftDispatcher,
      barrierQueueHealthy runtime runtime.runtimeTimers.electionTimerCommands BarrierRuntimeTimer,
      barrierQueueHealthy runtime runtime.runtimeTimers.heartbeatTimerCommands BarrierRuntimeTimer,
      barrierQueueHealthy runtime runtime.runtimeTimers.recentLeaderTimerCommands BarrierRuntimeTimer,
      recordingBarrier runtime
    ]

withOracleRuntime ::
  OracleRuntimeConfiguration ->
  (OracleRuntime -> IO result) ->
  IO (Either OracleRuntimeFailure (result, OracleRuntimeExit))
withOracleRuntime configuration use = mask $ \restore -> do
  started <- startOracleRuntime configuration
  case started of
    Left failure -> pure (Left failure)
    Right runtime -> do
      callbackResult <- atomically newEmptyTMVar
      callbackThread <- forkFinally (restore (use runtime)) (atomically . putTMVar callbackResult)
      first <-
        atomically
          ( (Left <$> runtimeScopeObservedFailure runtime.runtimeScope)
              `orElse` (Right <$> readTMVar callbackResult)
          )
          `onException` (killThread callbackThread >> stopOracleRuntime runtime)
      case first of
        Left failure -> do
          killThread callbackThread
          void (atomically (readTMVar callbackResult))
          shutdownOracleRuntime runtime (OracleRuntimeFailed failure)
          pure (Left failure)
        Right (Left exception) -> do
          stopOracleRuntime runtime
          throwIO exception
        Right (Right result) -> do
          stopOracleRuntime runtime
          exit <- awaitOracleRuntimeExit runtime
          case exit of
            OracleRuntimeFailed failure -> pure (Left failure)
            stopped -> pure (Right (result, stopped))

stopOracleRuntime :: OracleRuntime -> IO ()
stopOracleRuntime runtime = shutdownOracleRuntime runtime OracleRuntimeStopped

shutdownOracleRuntime :: OracleRuntime -> OracleRuntimeExit -> IO ()
shutdownOracleRuntime runtime requestedExit = mask $ \_ -> do
  requestRuntimeShutdown runtime requestedExit
  void (atomically (readTMVar runtime.runtimeExitCell))
  void (readMVar runtime.runtimeSupervisorDone)

requestRuntimeShutdown :: OracleRuntime -> OracleRuntimeExit -> IO ()
requestRuntimeShutdown runtime requestedExit = mask $ \_ -> do
  owner <- atomically (claimRuntimeScopeClose runtime.runtimeScope)
  when owner $ do
    _ <-
      forkIOWithUnmask $ \_ ->
        shutdownOwned runtime requestedExit
          `catch` \exception ->
            abortAndPublish
              runtime
              ( RuntimeEssentialChildFailed
                  "runtime-shutdown"
                  (displayException (exception :: SomeException))
              )
    pure ()

startLifecycleSupervisor :: OracleRuntime -> IO ()
startLifecycleSupervisor runtime = do
  _ <-
    forkIOWithUnmask $ \unmask ->
      unmask supervise
        `finally` putMVar runtime.runtimeSupervisorDone ()
  pure ()
  where
    supervise = do
      observed <-
        atomically
          ( (Left <$> runtimeScopeObservedFailure runtime.runtimeScope)
              `orElse` (Right <$> readTMVar runtime.runtimeExitCell)
          )
      case observed of
        Left failure -> do
          requestRuntimeShutdown runtime (OracleRuntimeFailed failure)
          void (atomically (readTMVar runtime.runtimeExitCell))
        Right _ -> pure ()

shutdownOwned :: OracleRuntime -> OracleRuntimeExit -> IO ()
shutdownOwned runtime requestedExit = mask $ \_ -> do
  atomically $ do
    status <- readTVar runtime.runtimeStatusCell
    writeTVar runtime.runtimeStatusCell status {statusAccepting = False}
    writeTQueue runtime.runtimeTimers.electionTimerCommands StopRuntimeTimer
    writeTQueue runtime.runtimeTimers.heartbeatTimerCommands StopRuntimeTimer
    writeTQueue runtime.runtimeTimers.recentLeaderTimerCommands StopRuntimeTimer
    abandonRuntimeProposals runtime.runtimeCoordination
  case requestedExit of
    OracleRuntimeFailed failure -> abortAndPublish runtime failure
    OracleRuntimeStopped -> do
      drained <- drainRuntime runtime
      case drained of
        Left failure -> abortAndPublish runtime failure
        Right () -> gracefulStopAndPublish runtime

awaitOracleRuntimeExit :: OracleRuntime -> IO OracleRuntimeExit
awaitOracleRuntimeExit runtime = do
  exit <- atomically (readTMVar runtime.runtimeExitCell)
  void (readMVar runtime.runtimeSupervisorDone)
  pure exit

oracleRuntimeStatus :: OracleRuntime -> IO OracleRuntimeStatus
oracleRuntimeStatus runtime = atomically $ do
  status <- readTVar runtime.runtimeStatusCell
  semanticReady <- readTVar runtime.runtimeCoordination.coordinationSemanticReady
  pure
    status
      { statusServiceReady =
          status.statusAccepting && status.statusServiceReady && semanticReady
      }

-- | Request a fresh observation from the live native owner. This remains
-- available without quorum or a service-ready leader and returns Nothing once
-- that owner stops.
oracleRuntimeHealth :: OracleRuntime -> IO (Maybe OracleRuntimeHealth)
oracleRuntimeHealth = queryOracleRuntimeHealth

oracleHealthNode :: OracleRuntimeHealth -> RaftNodeId
oracleHealthNode (OracleRuntimeHealth node _ _ _ _) = node

oracleHealthTerm :: OracleRuntimeHealth -> RaftTerm
oracleHealthTerm (OracleRuntimeHealth _ term _ _ _) = term

oracleHealthGeneration :: OracleRuntimeHealth -> Word64
oracleHealthGeneration (OracleRuntimeHealth _ _ generation _ _) = generation

oracleHealthEffectiveConfiguration :: OracleRuntimeHealth -> (RaftConfigurationRef, RaftVotingConfiguration)
oracleHealthEffectiveConfiguration (OracleRuntimeHealth _ _ _ reference configuration) = (reference, configuration)

oracleRuntimeNode :: OracleRuntimeStatus -> RaftNodeId
oracleRuntimeNode status = status.statusNode

oracleRuntimeRole :: OracleRuntimeStatus -> Raft.RaftRole
oracleRuntimeRole status = status.statusRole

oracleRuntimeTerm :: OracleRuntimeStatus -> RaftTerm
oracleRuntimeTerm status = status.statusTerm

oracleRuntimeLeaderHint :: OracleRuntimeStatus -> Maybe RaftNodeId
oracleRuntimeLeaderHint status = status.statusLeaderHint

oracleRuntimeServiceReady :: OracleRuntimeStatus -> Bool
oracleRuntimeServiceReady status = status.statusServiceReady

oracleRuntimeAppliedControlIndex :: OracleRuntimeStatus -> ControlIndex
oracleRuntimeAppliedControlIndex status = status.statusAppliedControlIndex

-- | Immutable scalar last published by the sole Oracle owner. Its publication
-- can precede the watch owner's applied-control projection.
oracleRuntimeLabelRetiredThrough :: OracleRuntimeStatus -> ControlIndex
oracleRuntimeLabelRetiredThrough status = status.statusLabelRetiredThrough

oracleRuntimeCompletedWorkflowCount :: OracleRuntimeStatus -> Int
oracleRuntimeCompletedWorkflowCount status = status.statusCompletedWorkflowCount

oracleRuntimeMembershipGeneration :: OracleRuntimeStatus -> HeraldMembershipGeneration
oracleRuntimeMembershipGeneration status = heraldMembershipHistoryCurrent status.statusMembershipHistory

-- | Read captured evidence, or an empty list in diagnostics-only mode. Both
-- live and post-stop snapshots retain their ordinary synchronization behavior.
oracleRuntimeRecording :: OracleRuntime -> IO [OracleRuntimeEvent]
oracleRuntimeRecording runtime = do
  live <- atomically $ do
    final <- tryReadTMVar runtime.runtimeFinalRecording
    case final of
      Just recording -> pure (Left recording)
      Nothing -> do
        status <- readTVar runtime.runtimeStatusCell
        if status.statusAccepting
          then do
            result <- newEmptyTMVar
            writeTQueue runtime.runtimeRecordingCommands (SnapshotRuntimeRecording result)
            pure (Right result)
          else pure (Right runtime.runtimeFinalRecording)
  case live of
    Left recording -> pure recording
    Right result ->
      atomically
        ( readTMVar result
            `orElse` readTMVar runtime.runtimeFinalRecording
        )

barrierQueueHealthy ::
  OracleRuntime ->
  TQueue command ->
  (TMVar () -> command) ->
  IO (Either OracleRuntimeFailure ())
barrierQueueHealthy runtime queue constructor = do
  done <- atomically newEmptyTMVar
  atomically (writeTQueue queue (constructor done))
  atomically
    ( (readTMVar done >> pure (Right ()))
        `orElse` (Left <$> runtimeScopeObservedFailure runtime.runtimeScope)
    )

recordingBarrier :: OracleRuntime -> IO (Either OracleRuntimeFailure ())
recordingBarrier runtime = do
  done <- atomically newEmptyTMVar
  atomically (writeTQueue runtime.runtimeRecordingCommands (SnapshotRuntimeRecording done))
  atomically
    ( (readTMVar done >> pure (Right ()))
        `orElse` (Left <$> runtimeScopeObservedFailure runtime.runtimeScope)
    )

sequenceRuntimeChecks ::
  [IO (Either OracleRuntimeFailure ())] ->
  IO (Either OracleRuntimeFailure ())
sequenceRuntimeChecks [] = pure (Right ())
sequenceRuntimeChecks (checkOne : rest) = do
  outcome <- checkOne
  case outcome of
    Left failure -> pure (Left failure)
    Right () -> sequenceRuntimeChecks rest

drainRuntime :: OracleRuntime -> IO (Either OracleRuntimeFailure ())
drainRuntime runtime =
  sequenceRuntimeChecks
    [ barrierQueueHealthy runtime runtime.runtimeRaftCommands BarrierRaftOwner,
      barrierQueueHealthy runtime runtime.runtimeRaftBatches BarrierRaftDispatcher,
      barrierQueueHealthy runtime runtime.runtimeRaftCommands BarrierRaftOwner,
      barrierQueueHealthy runtime runtime.runtimeRaftBatches BarrierRaftDispatcher,
      barrierQueueHealthy runtime runtime.runtimeAdapterCommands BarrierAdapter,
      barrierQueueHealthy runtime runtime.runtimeOracleCommands BarrierOracleOwner,
      barrierQueueHealthy runtime runtime.runtimeWatchCommands BarrierWatchLedger
    ]

gracefulStopAndPublish :: OracleRuntime -> IO ()
gracefulStopAndPublish runtime = do
  recordRuntimeEvent runtime.runtimeRecordingCommands RuntimeReplicaStopped
  atomically $ do
    writeTQueue runtime.runtimeRaftCommands StopRaftOwner
    writeTQueue runtime.runtimeRaftBatches StopRaftDispatcher
    writeTQueue runtime.runtimeAdapterCommands StopAdapter
    writeTQueue runtime.runtimeOracleCommands StopOracleOwner
    writeTQueue runtime.runtimeWatchCommands StopWatchLedger
    writeTQueue runtime.runtimeRecordingCommands StopRuntimeRecording
  observedFailure <- joinRuntimeScope runtime.runtimeScope
  let exit = maybe OracleRuntimeStopped OracleRuntimeFailed observedFailure
  atomically (void (tryPutTMVar runtime.runtimeExitCell exit))

abortStartedRuntime :: OracleRuntime -> OracleRuntimeFailure -> IO ()
abortStartedRuntime runtime failure = do
  atomically $ do
    status <- readTVar runtime.runtimeStatusCell
    writeTVar runtime.runtimeStatusCell status {statusAccepting = False}
    abandonRuntimeProposals runtime.runtimeCoordination
  abortAndPublish runtime failure

abortAndPublish :: OracleRuntime -> OracleRuntimeFailure -> IO ()
abortAndPublish runtime failure = do
  observed <- abortRuntimeScope runtime.runtimeScope
  let terminal = maybe failure id observed
  atomically $ do
    _ <- tryPutTMVar runtime.runtimeFinalRecording []
    void (tryPutTMVar runtime.runtimeExitCell (OracleRuntimeFailed terminal))

offerWhileAccepting :: OracleRuntime -> RaftOwnerCommand -> IO ()
offerWhileAccepting runtime command = atomically $ do
  status <- readTVar runtime.runtimeStatusCell
  if status.statusAccepting
    then writeTQueue runtime.runtimeRaftCommands command
    else pure ()
