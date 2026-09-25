-- | Package-private live recording and scheduling controls. Production-shaped
-- scheduling and deterministically gated scheduling both start the ordinary
-- owner, dispatcher, writers, and public ingress facade; only the latter
-- pauses the existing dispatcher/delay hooks.
module Eclips.Herald.Runtime.Internal.Conformance
  ( RecordingSchedule (..),
    RecordedHeraldRuntime,
    recordedHeraldRuntime,
    injectRecordedEffectBatch,
    startRecordedHeraldRuntime,
    awaitRecordedBatch,
    releaseRecordedBatch,
    advanceRecordedBatch,
    advanceRecordedBatchesUntilPeerOutcome,
    awaitRecordedDrainCut,
    awaitRecordedPeerControlAfter,
    awaitRecordedPeerBindingLoss,
    awaitRecordedPeerPublicationAfter,
    snapshotRecordedRuntimeEvents,
    pauseRecordedBatchPump,
    resumeRecordedBatchPump,
    awaitRecordedDelay,
    releaseRecordedDelay,
    snapshotRecordedHeraldRuntime,
    ApplicationConformanceScript (..),
    ConformanceRunFailure (..),
    runApplicationConformanceScript,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
  ( Chan,
    newChan,
    readChan,
    writeChan,
  )
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    readMVar,
    tryPutMVar,
  )
import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    check,
    newTQueue,
    newTVar,
    readTQueue,
    readTVar,
    writeTQueue,
    writeTVar,
  )
import Control.Exception (finally)
import Control.Monad (void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Set (Set)
import Data.Word (Word64)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Herald.Application.RPC (ApplicationOutbound)
import Eclips.Herald.Application.Recovery (ApplicationRecoveryConfiguration)
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks)
import Eclips.Herald.Discovery (PeerBinding)
import Eclips.Herald.EffectBatch (EffectBatch)
import Eclips.Herald.IdGenerator (GeneratorSeed)
import Eclips.Herald.Input
  ( HeraldInputBody (..),
    PeerControl,
    PeerIngress (..),
    RuntimeObservation (PeerBindingLost),
    inputBody,
  )
import Eclips.Herald.Isolation (IsolationConfiguration)
import Eclips.Herald.OracleClient (OracleContactSet)
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome,
    PeerDispatchTicket,
    PeerLogicalAttempt,
    PeerLogicalItem,
  )
import Eclips.Herald.PeerLiveness (PeerRecoveryConfiguration)
import Eclips.Herald.Runtime.Connection
  ( ApplicationPlane,
    ConnectionRef,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeLaneOffer (..),
    runtimeApplicationConnectionHandlers,
  )
import Eclips.Herald.Runtime.Ingress
  ( RuntimeRegistration (..),
    RuntimeSubmission (..),
    closeRuntimeConnection,
    registerApplicationConnection,
    submitApplicationDto,
  )
import Eclips.Herald.Runtime.Internal.Arbiter (SourceId)
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    defaultRuntimeHooks,
    startHeraldRuntimeWithHooks,
  )
import Eclips.Herald.Runtime.Internal.Recording
  ( RuntimeTrace,
    RuntimeTraceFinalEvidence,
    RuntimeTraceTerminal (..),
    runtimeTraceSnapshot,
    runtimeTraceWithDiagnosticChecks,
    runtimeTraceWithTiming,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (..),
    RuntimeEventOrdinal,
    RuntimeTraceEvent (..),
    ShellTraceEvent (..),
    TraceDriverEvent (..),
    TraceLedger,
    appendShellTrace,
    snapshotTraceLedger,
  )
import Eclips.Herald.Runtime.Internal.Types
  ( HeraldRuntime (..),
    HeraldRuntimeConfiguration (..),
    HeraldRuntimeFailure,
  )
import Eclips.Herald.Time (MonotonicInstant)
import Eclips.Oracle.Voter (OracleReplicaRegistration, VoterConfiguration)
import Eclips.Protocol.Application.Types (ApplicationClientDto)
import Eclips.Public.Types.Timing (TakeoverTarget)

data RecordingSchedule
  = ProductionShapedRecording
  | DeterministicallyGatedRecording
  deriving stock (Eq, Show)

data RuntimeInitialization
  = RuntimeInitialization
      MonotonicInstant
      OracleContactSet
      GeneratorSeed
      ApplicationRecoveryConfiguration
      PeerRecoveryConfiguration
      EffectBatch
      TraceLedger
      (IORef (EffectBatch -> IO ()))
      (Maybe (TakeoverTarget, Maybe (VoterConfiguration, [OracleReplicaRegistration]), Maybe (Set HeraldEpoch, IsolationConfiguration)))
      DiagnosticChecks

data RecordedBatchControl
  = RecordedBatchControl
      (TVar Bool)
      (TVar Bool)

data RecordedHeraldRuntime
  = RecordedHeraldRuntime
      HeraldRuntime
      RecordingSchedule
      (TQueue ())
      (Chan ())
      (Chan ())
      RecordedBatchControl
      (Chan (PeerDispatchTicket, Word64))
      (Chan ())
      (MVar RuntimeInitialization)
      (MVar RuntimeTraceFinalEvidence)

recordedHeraldRuntime :: RecordedHeraldRuntime -> HeraldRuntime
recordedHeraldRuntime (RecordedHeraldRuntime runtime _ _ _ _ _ _ _ _ _) = runtime

startRecordedHeraldRuntime ::
  RecordingSchedule ->
  HeraldRuntimeConfiguration ->
  IO (Either HeraldRuntimeFailure RecordedHeraldRuntime)
startRecordedHeraldRuntime schedule configuration = do
  let HeraldRuntimeConfiguration _ _ _ _ _ _ _ _ isolation _ _ _ _ voters _ target _ checks = configuration
      timing = fmap (\selected -> (selected, voters, isolation)) target
  batchArrived <- atomically newTQueue
  batchRelease <- newChan
  batchCompleted <- newChan
  batchEnabled <- atomically (newTVar True)
  batchActive <- atomically (newTVar False)
  let batchControl = RecordedBatchControl batchEnabled batchActive
  delayArrived <- newChan
  delayRelease <- newChan
  initialization <- newEmptyMVar
  finalEvidence <- newEmptyMVar
  effectInjector <- newIORef (const (pure ()))
  let hooks =
        RuntimeHooks
          { hookRuntimeInitialized = \observedAt contacts seed applicationRecovery peerRecovery batch ledger ->
              void
                ( tryPutMVar
                    initialization
                    (RuntimeInitialization observedAt contacts seed applicationRecovery peerRecovery batch ledger effectInjector timing checks)
                ),
            hookRuntimeEffectInjector = writeIORef effectInjector,
            hookBeforeDispatchBatch = \_ -> do
              atomically (writeTQueue batchArrived ())
              when (schedule == DeterministicallyGatedRecording) (readChan batchRelease),
            hookAfterDispatchBatch = \_ -> writeChan batchCompleted (),
            hookBeforePeerDialSink = const (pure ()),
            hookDelayPeerWork = \ticket microseconds -> do
              writeChan delayArrived (ticket, microseconds)
              case schedule of
                ProductionShapedRecording ->
                  threadDelay (fromIntegral microseconds)
                DeterministicallyGatedRecording -> readChan delayRelease,
            hookPublishFinalEvidence = \evidence ->
              void (tryPutMVar finalEvidence evidence),
            hookBeforeConnectionWriterBody = const (pure ()),
            hookAfterWriterCommandDequeued = const (pure ()),
            hookRuntimeRetentionReader = const (pure ()),
            hookTimerScheduler = hookTimerScheduler defaultRuntimeHooks
          }
  started <- startHeraldRuntimeWithHooks hooks configuration
  pure
    $ fmap
      ( \runtime ->
          RecordedHeraldRuntime
            runtime
            schedule
            batchArrived
            batchRelease
            batchCompleted
            batchControl
            delayArrived
            delayRelease
            initialization
            finalEvidence
      )
      started

injectRecordedEffectBatch :: RecordedHeraldRuntime -> EffectBatch -> IO ()
injectRecordedEffectBatch (RecordedHeraldRuntime _ _ _ _ _ _ _ _ initialization _) batch = do
  RuntimeInitialization _ _ _ _ _ _ _ injector _ _ <- readMVar initialization
  readIORef injector >>= ($ batch)

-- | Observe the ordinary dispatcher immediately before it routes a selected
-- whole batch.  Production-shaped recording observes without interposing;
-- deterministically gated scheduling leaves the dispatcher waiting for release.
awaitRecordedBatch :: RecordedHeraldRuntime -> IO ()
awaitRecordedBatch (RecordedHeraldRuntime _ _ arrived _ _ _ _ _ _ _) = atomically (readTQueue arrived)

-- | Release the observed batch in gated mode and wait until the same ordinary
-- dispatcher has routed every member.
releaseRecordedBatch :: RecordedHeraldRuntime -> IO ()
releaseRecordedBatch (RecordedHeraldRuntime _ schedule _ release completed _ _ _ _ _) = do
  when (schedule == DeterministicallyGatedRecording) (writeChan release ())
  readChan completed

-- | Observe, release, and complete one whole batch.
advanceRecordedBatch :: RecordedHeraldRuntime -> IO ()
advanceRecordedBatch recorded@(RecordedHeraldRuntime _ _ arrived _ _ (RecordedBatchControl enabled active) _ _ _ _) = do
  atomically $ do
    automatic <- readTVar enabled
    check automatic
    readTQueue arrived
    writeTVar active True
  releaseRecordedBatch recorded
    `finally` atomically (writeTVar active False)

-- | Under deterministically gated recording, with the sole automatic batch
-- observer paused, route whole batches until the exact first-winner attempt outcome
-- is committed.  Each released batch was already pending at one atomic cut
-- where the outcome was absent; after it completes, the ledger is checked
-- again before another pending arrival is consumed.
advanceRecordedBatchesUntilPeerOutcome ::
  RecordedHeraldRuntime ->
  PeerLogicalAttempt ->
  PeerDispatchOutcome ->
  IO ()
advanceRecordedBatchesUntilPeerOutcome recorded@(RecordedHeraldRuntime _ _ arrived _ _ _ _ _ initialization _) expectedAttempt expectedOutcome = do
  RuntimeInitialization _ _ _ _ _ _ ledger _ _ _ <- readMVar initialization
  let go = do
        outcomeCommitted <- atomically $ do
          events <- snapshotTraceLedger ledger
          if any isExpectedOutcome events
            then pure True
            else readTQueue arrived >> pure False
        if outcomeCommitted
          then pure ()
          else releaseRecordedBatch recorded >> go
  go
  where
    isExpectedOutcome = \case
      ShellEvent _ (ShellPeerOutcomeWon _ attempt outcome) ->
        attempt == expectedAttempt && outcome == expectedOutcome
      _ -> False

pauseRecordedBatchPump :: RecordedHeraldRuntime -> IO ()
pauseRecordedBatchPump (RecordedHeraldRuntime _ _ _ _ _ (RecordedBatchControl enabled active) _ _ _ _) =
  atomically $ do
    writeTVar enabled False
    routing <- readTVar active
    check (not routing)

resumeRecordedBatchPump :: RecordedHeraldRuntime -> IO ()
resumeRecordedBatchPump (RecordedHeraldRuntime _ _ _ _ _ (RecordedBatchControl enabled _) _ _ _ _) =
  atomically (writeTVar enabled True)

-- | Wait until the owner has atomically installed the drain admission cut.
-- This observes the exact ledger entry; it does not add a second lifecycle
-- hook or expose mutable runtime state.
awaitRecordedDrainCut :: RecordedHeraldRuntime -> IO ()
awaitRecordedDrainCut (RecordedHeraldRuntime _ _ _ _ _ _ _ _ initialization _) = do
  RuntimeInitialization _ _ _ _ _ _ ledger _ _ _ <- readMVar initialization
  atomically $ do
    events <- snapshotTraceLedger ledger
    check (any isDrainCut events)
  where
    isDrainCut (ShellEvent _ ShellDrainCut {}) = True
    isDrainCut _ = False

-- | Wait for the terminal resume control of one exact peer generation after a
-- causal trace watermark.  The accepted response is handed through the same
-- physical source FIFO after all earlier handshake controls, so this one exact
-- input is a stronger fence than a cumulative submission count that can retain
-- queued work belonging to a removed generation.
awaitRecordedPeerControlAfter ::
  RuntimeEventOrdinal ->
  SourceId ->
  PeerBinding ->
  PeerControl ->
  RecordedHeraldRuntime ->
  IO ()
awaitRecordedPeerControlAfter watermark expectedSource expectedBinding expectedControl (RecordedHeraldRuntime _ _ _ _ _ _ _ _ initialization _) = do
  RuntimeInitialization _ _ _ _ _ _ ledger _ _ _ <- readMVar initialization
  atomically $ do
    events <- snapshotTraceLedger ledger
    check (any isExpectedPeerControl events)
  where
    isExpectedPeerControl (KernelEvent ordinal (KernelTraceStepped _ source input _))
      | ordinal > watermark,
        source == expectedSource =
          case inputBody input of
            PeerInput (PeerControlReceived binding control) -> binding == expectedBinding && control == expectedControl
            PeerInput (PeerControlReceivedWithProgress binding _ control) -> binding == expectedBinding && control == expectedControl
            _ -> False
    isExpectedPeerControl _ = False

-- | Wait until the ordinary owner has selected and stepped the exact binding
-- loss.  Tests use this as a causal terminal alternative to a physical writer
-- callback; it does not infer connection loss from elapsed wall-clock time.
awaitRecordedPeerBindingLoss :: RecordedHeraldRuntime -> PeerBinding -> IO ()
awaitRecordedPeerBindingLoss (RecordedHeraldRuntime _ _ _ _ _ _ _ _ initialization _) expectedBinding = do
  RuntimeInitialization _ _ _ _ _ _ ledger _ _ _ <- readMVar initialization
  atomically $ do
    events <- snapshotTraceLedger ledger
    check (any isExpectedBindingLoss events)
  where
    isExpectedBindingLoss (KernelEvent _ (KernelTraceStepped _ _ input _)) =
      inputBody input == RuntimeObserved (PeerBindingLost expectedBinding)
    isExpectedBindingLoss _ = False

-- | Wait for the exact opaque peer publication strictly after a causal trace
-- watermark. Retransmission deliberately repeats the same immutable item, so
-- equality alone cannot distinguish the current delivery from an earlier one.
awaitRecordedPeerPublicationAfter :: RuntimeEventOrdinal -> RecordedHeraldRuntime -> PeerLogicalItem -> IO ()
awaitRecordedPeerPublicationAfter watermark (RecordedHeraldRuntime _ _ _ _ _ _ _ _ initialization _) expectedItem = do
  RuntimeInitialization _ _ _ _ _ _ ledger _ _ _ <- readMVar initialization
  atomically $ do
    events <- snapshotTraceLedger ledger
    check (any isExpectedPublication events)
  where
    isExpectedPublication (KernelEvent ordinal (KernelTraceStepped _ _ input _))
      | ordinal > watermark = case inputBody input of
          PeerInput (PeerPublicationReceived _ item) -> item == expectedItem
          PeerInput (PeerPublicationReceivedWithProgress _ _ item) -> item == expectedItem
          _ -> False
    isExpectedPublication _ = False

snapshotRecordedRuntimeEvents :: RecordedHeraldRuntime -> IO [RuntimeTraceEvent]
snapshotRecordedRuntimeEvents (RecordedHeraldRuntime _ _ _ _ _ _ _ _ initialization _) = do
  RuntimeInitialization _ _ _ _ _ _ ledger _ _ _ <- readMVar initialization
  atomically (snapshotTraceLedger ledger)

-- | Observe the real delayed-work worker reaching its physical wait boundary.
-- The caller may then release that exact boundary in gated mode.
awaitRecordedDelay :: RecordedHeraldRuntime -> IO (PeerDispatchTicket, Word64)
awaitRecordedDelay (RecordedHeraldRuntime _ _ _ _ _ _ arrived _ _ _) = readChan arrived

releaseRecordedDelay :: RecordedHeraldRuntime -> IO ()
releaseRecordedDelay (RecordedHeraldRuntime _ schedule _ _ _ _ _ release _ _) =
  when (schedule == DeterministicallyGatedRecording) (writeChan release ())

appendRecordedDriverEvent :: RecordedHeraldRuntime -> TraceDriverEvent -> IO ()
appendRecordedDriverEvent (RecordedHeraldRuntime _ _ _ _ _ _ _ _ initialization _) driverEvent = do
  RuntimeInitialization _ _ _ _ _ _ ledger _ _ _ <- readMVar initialization
  atomically (void (appendShellTrace ledger (ShellDriverEvent driverEvent)))

closeRecordedHeraldRuntime :: RecordedHeraldRuntime -> IO ()
closeRecordedHeraldRuntime = runtimeCloseScope . recordedHeraldRuntime

-- | Take a snapshot only after the public terminal capability is observable.
-- At that point the supervisor has cancelled and joined the owner, so the
-- MVar-held final witness is immutable post-mortem evidence rather than shared
-- live kernel state.
snapshotRecordedHeraldRuntime :: RecordedHeraldRuntime -> IO RuntimeTrace
snapshotRecordedHeraldRuntime recorded@(RecordedHeraldRuntime _ _ _ _ _ _ _ _ initialization finalEvidence) = do
  terminalResult <- runtimeAwaitExit (recordedHeraldRuntime recorded)
  RuntimeInitialization observedAt contacts seed applicationRecovery peerRecovery initialBatch ledger _ timing checks <-
    readMVar initialization
  evidence <- readMVar finalEvidence
  events <- atomically (snapshotTraceLedger ledger)
  let terminal = either RuntimeTraceFailed RuntimeTraceExited terminalResult
  pure
    ( runtimeTraceWithDiagnosticChecks checks
        . maybe id (\(target, voters, hosts) -> runtimeTraceWithTiming target voters hosts) timing
        $ runtimeTraceSnapshot
          observedAt
          contacts
          seed
          applicationRecovery
          peerRecovery
          initialBatch
          events
          terminal
          evidence
    )

data ApplicationConformanceScript = ApplicationConformanceScript
  { closedCandidateLanes :: Word64,
    staleCloseProbes :: Word64
  }
  deriving stock (Eq, Show)

data ConformanceRunFailure
  = ConformanceRuntimeStartFailed HeraldRuntimeFailure
  | ConformanceRegistrationStopped
  | ConformanceSubmissionMismatch RuntimeSubmission
  deriving stock (Eq, Show)

-- | A small typed stimulus runner used by focused and generated parity tests.
-- It deliberately calls the public registration/submission/close facade and
-- observes a real writer callback; only batch release differs between modes.
runApplicationConformanceScript ::
  RecordingSchedule ->
  HeraldRuntimeConfiguration ->
  ApplicationClientDto ->
  ApplicationConformanceScript ->
  IO (Either ConformanceRunFailure RuntimeTrace)
runApplicationConformanceScript schedule configuration dto script = do
  started <- startRecordedHeraldRuntime schedule configuration
  case started of
    Left failure -> pure (Left (ConformanceRuntimeStartFailed failure))
    Right recorded -> do
      advanceRecordedBatch recorded
      closed <- newChan
      outbound <- newChan
      previousResult <- closeCandidates recorded closed outbound (closedCandidateLanes script) []
      case previousResult of
        Left failure -> abort recorded failure
        Right previous -> do
          currentResult <- registerLane recorded closed outbound
          case currentResult of
            Left failure -> abort recorded failure
            Right current -> do
              repeatStaleProbes (staleCloseProbes script) (mapM_ (probeStale recorded) previous)
              appendRecordedDriverEvent recorded DriverApplicationSubmitted
              submitted <- submitApplicationDto (recordedHeraldRuntime recorded) current dto
              if submitted /= Queued
                then abort recorded (ConformanceSubmissionMismatch submitted)
                else do
                  advanceRecordedBatch recorded
                  _ <- readChan outbound
                  awaitLaneOffer recorded
                  appendRecordedDriverEvent recorded DriverScopeClosed
                  closeRecordedHeraldRuntime recorded
                  Right <$> snapshotRecordedHeraldRuntime recorded
  where
    abort recorded failure = do
      closeRecordedHeraldRuntime recorded
      pure (Left failure)

closeCandidates ::
  RecordedHeraldRuntime ->
  Chan () ->
  Chan ApplicationOutbound ->
  Word64 ->
  [ConnectionRef ApplicationPlane] ->
  IO (Either ConformanceRunFailure [ConnectionRef ApplicationPlane])
closeCandidates _ _ _ 0 retained = pure (Right retained)
closeCandidates recorded closed outbound count retained = do
  registered <- registerLane recorded closed outbound
  case registered of
    Left failure -> pure (Left failure)
    Right reference -> do
      appendRecordedDriverEvent recorded DriverApplicationClosed
      result <- closeRuntimeConnection (recordedHeraldRuntime recorded) reference
      if result /= ConnectionClosed
        then pure (Left (ConformanceSubmissionMismatch result))
        else do
          _ <- readChan closed
          closeCandidates recorded closed outbound (count - 1) (reference : retained)

registerLane ::
  RecordedHeraldRuntime ->
  Chan () ->
  Chan ApplicationOutbound ->
  IO (Either ConformanceRunFailure (ConnectionRef ApplicationPlane))
registerLane recorded closed outbound = do
  appendRecordedDriverEvent recorded DriverApplicationRegistered
  registration <-
    registerApplicationConnection
      (recordedHeraldRuntime recorded)
      (runtimeApplicationConnectionHandlers (\message -> writeChan outbound message >> pure LaneOffered) (writeChan closed ()))
  pure $ case registration of
    ConnectionRegistered reference -> Right reference
    RegistrationStopped -> Left ConformanceRegistrationStopped

probeStale :: RecordedHeraldRuntime -> ConnectionRef ApplicationPlane -> IO ()
probeStale recorded reference = do
  appendRecordedDriverEvent recorded DriverStaleCloseProbed
  void (closeRuntimeConnection (recordedHeraldRuntime recorded) reference)

repeatStaleProbes :: Word64 -> IO () -> IO ()
repeatStaleProbes 0 _ = pure ()
repeatStaleProbes count action = action >> repeatStaleProbes (count - 1) action

awaitLaneOffer :: RecordedHeraldRuntime -> IO ()
awaitLaneOffer (RecordedHeraldRuntime _ _ _ _ _ _ _ _ initialization _) = do
  RuntimeInitialization _ _ _ _ _ _ ledger _ _ _ <- readMVar initialization
  atomically $ do
    events <- snapshotTraceLedger ledger
    check (any isLaneOffer events)
  where
    isLaneOffer (ShellEvent _ ShellLaneOffered {}) = True
    isLaneOffer _ = False
