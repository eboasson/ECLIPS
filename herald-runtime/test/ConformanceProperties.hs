module ConformanceProperties
  ( tests,
  )
where

import Control.Concurrent
  ( forkFinally,
    killThread,
    myThreadId,
    newEmptyMVar,
    putMVar,
    readMVar,
    threadDelay,
  )
import Control.Concurrent.Chan
  ( Chan,
    newChan,
    readChan,
    writeChan,
  )
import Control.Concurrent.STM
  ( atomically,
    newEmptyTMVarIO,
    putTMVar,
    readTMVar,
    tryPutTMVar,
  )
import Control.Exception (finally)
import Control.Monad (forM_, forever, void)
import Data.List (findIndex)
import Data.Set qualified as Set
import Eclips.Application.Types.NewId (NewIdTarget (BareNewId))
import Eclips.Application.Types.Operation (ApplicationOperation (NewIdApplication))
import Eclips.Application.Types.Result (RegularCallResult (NewIdCompleted))
import Eclips.Herald.Administration (adminCorrelationId)
import Eclips.Herald.Application.RPC
  ( ApplicationOutbound (..),
  )
import Eclips.Herald.Discovery (connectionNonce, peerHelloHeraldEpoch)
import Eclips.Herald.EffectBatch (HeraldEffect (ArmTimer, DisposeApplicationSession), effectBatchMembers)
import Eclips.Herald.Input
  ( HeraldInputBody (RuntimeObserved),
    RuntimeObservation (ApplicationBindingLost),
    inputBody,
  )
import Eclips.Herald.Isolation (checkIsolationConfiguration)
import Eclips.Herald.Runtime
  ( DiagnosticChecks (..),
    HeraldRuntimeConfiguration,
    configureHeraldRuntimeDiagnosticChecks,
    configureHeraldRuntimeIsolation,
    configureHeraldRuntimeTiming,
    heraldRuntimeConfiguration,
    heraldRuntimeDiagnosticChecks,
    requestHeraldRuntimeDrain,
    runtimeMonotonicClock,
    runtimePeerWorkDelayMicroseconds,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeLaneOffer (..),
    heraldRuntimeHandlers,
    runtimeAdministrationConnectionHandlers,
    runtimeApplicationConnectionHandlers,
  )
import Eclips.Herald.Runtime.Ingress
  ( RuntimeAdministrationRegistration (..),
    RuntimeRegistration (..),
    RuntimeSubmission (..),
    closeRuntimeConnection,
    registerAdministrationConnection,
    registerApplicationConnection,
    submitApplicationDto,
  )
import Eclips.Herald.Runtime.Internal.Conformance
  ( ApplicationConformanceScript (..),
    ConformanceRunFailure,
    RecordedHeraldRuntime,
    RecordingSchedule (..),
    advanceRecordedBatch,
    recordedHeraldRuntime,
    runApplicationConformanceScript,
    snapshotRecordedHeraldRuntime,
    snapshotRecordedRuntimeEvents,
    startRecordedHeraldRuntime,
  )
import Eclips.Herald.Runtime.Internal.Recording
  ( RuntimeTrace,
    RuntimeTraceFinalEvidence (..),
    RuntimeTraceTerminal (..),
    TypedReplayMismatch (..),
    TypedReplayResult,
    normalizeRuntimeTrace,
    replayRuntimeTrace,
    runtimeTraceApplicationRecoveryConfiguration,
    runtimeTraceDiagnosticChecks,
    runtimeTraceEvents,
    runtimeTraceFinalEvidence,
    runtimeTraceInitialBatch,
    runtimeTraceInitialGeneratorSeed,
    runtimeTraceInitialObservation,
    runtimeTraceInitialOracleContacts,
    runtimeTracePeerRecoveryConfiguration,
    runtimeTraceSnapshot,
    runtimeTraceTerminal,
    runtimeTraceWithDiagnosticChecks,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    PhysicalConnectionRef (PhysicalAdministrationRef),
    RuntimeTraceEvent (..),
    RuntimeWorkerClass (OwnerWorker),
    ShellTraceEvent (..),
  )
import Eclips.Herald.Runtime.Internal.Types qualified as RuntimeInternal
import Eclips.Herald.Runtime.Trace
  ( HeraldRuntimeExit (HeraldRuntimeDrained),
    HeraldRuntimeFailureClass (HeraldRuntimeEssentialWorkerFailure),
    heraldRuntimeFailureClass,
  )
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Timer (timerSpecAbsoluteDeadline)
import Eclips.Protocol.Application.Types qualified as Protocol
import Eclips.Public.Types.Timing (takeoverTarget)
import RuntimeFixtures
  ( fixtureAlternateGeneratorSeed,
    fixtureApplicationRecoveryConfiguration,
    fixtureCheckedBootstraps,
    fixtureCheckedGenesis,
    fixtureGeneratorSeed,
    fixtureGeneratorSeedSource,
    fixtureOpenDto,
    fixtureOracleContacts,
    fixturePeerHello,
    fixturePeerRecoveryConfiguration,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( NonNegative (..),
    Property,
    ioProperty,
    testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "live production/gated-schedule conformance"
    [ testCase "disabled diagnostic audits survive runtime configuration, recording and exact replay" caseDiagnosticChecksReplay,
      testCase "nondefault timing survives runtime recording and pure replay" caseTimingReplay,
      testCase "later isolation and drain overrides survive timing recording and replay" caseTimingIsolationOverrideReplay,
      testCase "a captured live trace normalizes equally and typed-replays to its final state" caseLiveParity,
      testCase "a live bare newid replay is exact and rejects an alternate header seed" caseLiveNewIdReplay,
      testCase "a returned live snapshot has joined every recorded child exactly once" caseLiveChildBalance,
      testCase "an unexpected owner exception still publishes a replayable final snapshot" caseOwnerExceptionSnapshot,
      testProperty "100 generated public-seam scripts preserve normalized parity and typed replay" propGeneratedParity
    ]

caseDiagnosticChecksReplay :: Assertion
caseDiagnosticChecksReplay = do
  assertEqual "runtime defaults to audits enabled" DiagnosticChecksEnabled (heraldRuntimeDiagnosticChecks fixtureConfiguration)
  target <- either (assertFailure . show) pure (takeoverTarget 8_000_000)
  let configuration = configureHeraldRuntimeTiming target (configureHeraldRuntimeDiagnosticChecks DiagnosticChecksDisabled fixtureConfiguration)
  assertEqual "timing configuration preserves audit mode" DiagnosticChecksDisabled (heraldRuntimeDiagnosticChecks configuration)
  trace <- requireTrace =<< runApplicationConformanceScript ProductionShapedRecording configuration fixtureOpenDto (ApplicationConformanceScript 1 1)
  assertEqual "trace records disabled audits" DiagnosticChecksDisabled (runtimeTraceDiagnosticChecks trace)
  assertReplay "disabled audit execution replays to the exact final owner state" trace
  case replayRuntimeTrace fixtureCheckedGenesis fixtureCheckedBootstraps (runtimeTraceWithDiagnosticChecks DiagnosticChecksEnabled trace) of
    Left _ -> pure ()
    Right _ -> assertFailure "changing the policy must differ from the exact recorded owner state"

caseTimingReplay :: Assertion
caseTimingReplay = do
  target <- either (assertFailure . show) pure (takeoverTarget 8_000_000)
  trace <- requireTrace =<< runApplicationConformanceScript ProductionShapedRecording (configureHeraldRuntimeTiming target fixtureConfiguration) fixtureOpenDto (ApplicationConformanceScript 1 1)
  assertReplay "derived timing replays exactly" trace

caseTimingIsolationOverrideReplay :: Assertion
caseTimingIsolationOverrideReplay = do
  target <- either (assertFailure . show) pure (takeoverTarget 8_000_000)
  isolation <- either (assertFailure . show) pure (checkIsolationConfiguration 6_000_000 750_000)
  let hosts = Set.singleton (peerHelloHeraldEpoch (fixturePeerHello (connectionNonce 77)))
      configuration = configureHeraldRuntimeIsolation hosts isolation (configureHeraldRuntimeTiming target fixtureConfiguration)
  trace <- requireTrace =<< runApplicationConformanceScript ProductionShapedRecording configuration fixtureOpenDto (ApplicationConformanceScript 1 1)
  assertEqual "explicit grace overrides the target-derived grace" [monotonicInstant 6_000_100] [timerSpecAbsoluteDeadline specification | ArmTimer _ specification <- effectBatchMembers (runtimeTraceInitialBatch trace)]
  assertReplay "explicit grace and drain replay to the exact retained owner state" trace

caseLiveParity :: Assertion
caseLiveParity = do
  let script = ApplicationConformanceScript 2 2
  production <- requireTrace =<< run ProductionShapedRecording script
  gated <- requireTrace =<< run DeterministicallyGatedRecording script
  assertEqual
    "the same typed public script has one closed normalized projection"
    (normalizeRuntimeTrace production)
    (normalizeRuntimeTrace gated)
  assertReplay "production-shaped" production
  assertReplay "gated recording" gated

caseLiveNewIdReplay :: Assertion
caseLiveNewIdReplay = do
  production <- runLiveNewIdTrace ProductionShapedRecording
  gated <- runLiveNewIdTrace DeterministicallyGatedRecording
  assertEqual
    "the causally serialized Open and bare-newid script has one normalized trace"
    (normalizeRuntimeTrace production)
    (normalizeRuntimeTrace gated)
  assertEqual
    "the production trace header retains the exact admitted seed"
    fixtureGeneratorSeed
    (runtimeTraceInitialGeneratorSeed production)
  assertEqual
    "the gated trace header retains the exact admitted seed"
    fixtureGeneratorSeed
    (runtimeTraceInitialGeneratorSeed gated)
  assertReplay "production-shaped live bare-newid snapshot" production
  assertReplay "gated live bare-newid snapshot" gated
  let alternateSeedTrace =
        runtimeTraceSnapshot
          (runtimeTraceInitialObservation production)
          (runtimeTraceInitialOracleContacts production)
          fixtureAlternateGeneratorSeed
          (runtimeTraceApplicationRecoveryConfiguration production)
          (runtimeTracePeerRecoveryConfiguration production)
          (runtimeTraceInitialBatch production)
          (runtimeTraceEvents production)
          (runtimeTraceTerminal production)
          (runtimeTraceFinalEvidence production)
  assertEqual
    "replay obtains seed authority only from the header and detects replacement"
    (Left TypedReplayFinalStateMismatch)
    (replay alternateSeedTrace)

runLiveNewIdTrace :: RecordingSchedule -> IO RuntimeTrace
runLiveNewIdTrace schedule = do
  started <- startRecordedHeraldRuntime schedule fixtureConfiguration
  recorded <- case started of
    Left failure -> assertFailure ("recorded runtime initialization failed: " <> show failure)
    Right running -> pure running
  advanceRecordedBatch recorded
  pumpDone <- newEmptyMVar
  pump <-
    forkFinally
      (forever (advanceRecordedBatch recorded))
      (putMVar pumpDone)
  runScript recorded
    `finally` do
      RuntimeInternal.runtimeCloseScope (recordedHeraldRuntime recorded)
      killThread pump
      void (readMVar pumpDone)
  where
    runScript recorded = do
      outbound <- newChan
      registration <-
        registerApplicationConnection
          (recordedHeraldRuntime recorded)
          (runtimeApplicationConnectionHandlers (\message -> writeChan outbound message >> pure LaneOffered) (pure ()))
      reference <- case registration of
        ConnectionRegistered current -> pure current
        RegistrationStopped -> assertFailure "running recorded runtime rejected registration"
      administrationRegistration <-
        registerAdministrationConnection
          (recordedHeraldRuntime recorded)
          (runtimeAdministrationConnectionHandlers (const (pure LaneOffered)) (pure ()))
      administration <- case administrationRegistration of
        AdministrationConnectionRegistered current -> pure current
        AdministrationRegistrationUnavailable -> assertFailure "recorded runtime administration registration unavailable"
        AdministrationRegistrationStopped -> assertFailure "recorded runtime administration registration stopped"
      assertEqual "the live Open enters the runtime" Queued
        =<< submitApplicationDto (recordedHeraldRuntime recorded) reference fixtureOpenDto
      session <- do
        opening <- awaitOutbound "live SessionOpened" outbound
        case opening of
          AcceptApplicationCandidate _ _ (Protocol.SessionOpened _ accepted _ _) -> pure accepted
          other -> assertFailure ("expected SessionOpened, got " <> show other)
      assertEqual "the live bare newid enters the runtime" Queued
        =<< submitApplicationDto
          (recordedHeraldRuntime recorded)
          reference
          ( Protocol.Call
              session
              (Protocol.applicationRequestIdClaim 0)
              (NewIdApplication BareNewId)
          )
      completion <- awaitOutbound "live bare newid completion" outbound
      case completion of
        SendEstablishedApplication
          _
          (Protocol.RequestRetained _ _ _ (Protocol.Completed NewIdCompleted {})) -> pure ()
        other -> assertFailure ("expected retained bare newid completion, got " <> show other)
      applicationCloseWatermark <- length <$> snapshotRecordedRuntimeEvents recorded
      assertEqual "the application lease closes before the administration drain" ConnectionClosed
        =<< closeRuntimeConnection (recordedHeraldRuntime recorded) reference
      awaitApplicationLossDisposalRoute recorded applicationCloseWatermark
      assertEqual "the semantic drain enters the runtime" Queued
        =<< requestHeraldRuntimeDrain
          (recordedHeraldRuntime recorded)
          administration
          (adminCorrelationId 301)
      trace <- snapshotRecordedHeraldRuntime recorded
      assertEqual
        "the live newid trace snapshots only after semantic drain and joined exit"
        (RuntimeTraceExited HeraldRuntimeDrained)
        (runtimeTraceTerminal trace)
      let shellEvents = [event | ShellEvent _ event <- runtimeTraceEvents trace]
          administrationCloseIndices =
            [ index
            | (index, ShellConnectionClosed (PhysicalAdministrationRef current) _) <- zip [(0 :: Int) ..] shellEvents,
              current == administration
            ]
          drainFinalizationIndices =
            [index | (index, ShellDrainFinalized {}) <- zip [(0 :: Int) ..] shellEvents]
      assertEqual "the live administration route has one exact cleanup" 1 (length administrationCloseIndices)
      assertEqual "the live trace has one exact drain finalization" 1 (length drainFinalizationIndices)
      assertBool
        "the final administration cleanup causally precedes drain finalization"
        (administrationCloseIndices < drainFinalizationIndices)
      pure trace

awaitApplicationLossDisposalRoute :: RecordedHeraldRuntime -> Int -> Assertion
awaitApplicationLossDisposalRoute recorded watermark = do
  observed <- timeout 2000000 loop
  case observed of
    Nothing -> assertFailure "application-close immediate disposal route timed out"
    Just () -> pure ()
  where
    loop = do
      events <- drop watermark <$> snapshotRecordedRuntimeEvents recorded
      if applicationLossPrecedesDisposalRoute events
        then pure ()
        else threadDelay 1000 >> loop

applicationLossPrecedesDisposalRoute :: [RuntimeTraceEvent] -> Bool
applicationLossPrecedesDisposalRoute events =
  case dropWhile (not . isApplicationLoss) events of
    [] -> False
    _ : afterLoss -> any isDisposalRoute afterLoss
  where
    isApplicationLoss (KernelEvent _ (KernelTraceStepped _ _ input _)) =
      case inputBody input of
        RuntimeObserved ApplicationBindingLost {} -> True
        _ -> False
    isApplicationLoss _ = False

    isDisposalRoute (ShellEvent _ (ShellEffectRouted _ _ DisposeApplicationSession {})) = True
    isDisposalRoute _ = False

awaitOutbound :: String -> Chan ApplicationOutbound -> IO ApplicationOutbound
awaitOutbound context outbound = do
  observed <- timeout 2000000 (readChan outbound)
  case observed of
    Nothing -> assertFailure (context <> " timed out")
    Just value -> pure value

caseLiveChildBalance :: Assertion
caseLiveChildBalance = do
  trace <- requireTrace =<< run ProductionShapedRecording (ApplicationConformanceScript 2 2)
  let shellEvents = [event | ShellEvent _ event <- runtimeTraceEvents trace]
      starts = [(workerClass, thread) | ShellChildStarted workerClass thread <- shellEvents]
      joins = [(workerClass, thread) | ShellChildJoined workerClass thread <- shellEvents]
      indexed = zip [(0 :: Int) ..] shellEvents
  assertBool "the live recorder observed the supervised child tree" (not (null starts))
  forM_ starts $ \started ->
    assertEqual
      ("recorded child has exactly one join: " <> show started)
      1
      (length (filter (== started) joins))
  forM_ joins $ \joined ->
    assertEqual
      ("join belongs to exactly one recorded start: " <> show joined)
      1
      (length (filter (== joined) starts))
  forM_ indexed $ \(activityIndex, event) ->
    forM_ (childActivity event) $ \key ->
      assertBool
        ("child activity follows its committed start: " <> show key)
        ( maybe
            False
            (< activityIndex)
            (findIndex (isChildStart key) shellEvents)
        )
  terminalIndex <-
    case findIndex isTerminal shellEvents of
      Nothing -> assertFailure "the returned trace omitted its terminal request"
      Just index -> pure index
  assertBool
    "the terminal request precedes cancellation of still-live children"
    (all (\(index, event) -> not (isCancellation event) || index > terminalIndex) indexed)
  where
    isTerminal ShellRuntimeExited {} = True
    isTerminal ShellRuntimeFailed {} = True
    isTerminal _ = False

    isCancellation ShellChildCancellationRequested {} = True
    isCancellation _ = False

    childActivity (ShellChildFailed workerClass thread) = Just (workerClass, thread)
    childActivity (ShellChildCancellationRequested workerClass thread) = Just (workerClass, thread)
    childActivity (ShellChildJoined workerClass thread) = Just (workerClass, thread)
    childActivity _ = Nothing

    isChildStart expected (ShellChildStarted workerClass thread) = expected == (workerClass, thread)
    isChildStart _ _ = False

caseOwnerExceptionSnapshot :: Assertion
caseOwnerExceptionSnapshot = do
  ownerThread <- newEmptyTMVarIO
  releaseFailure <- newEmptyTMVarIO
  let failingClock = do
        caller <- myThreadId
        initializing <- atomically (tryPutTMVar ownerThread caller)
        owner <- atomically (readTMVar ownerThread)
        -- Initialization reads first, before timers exist. The timer worker
        -- shares this clock, so its later reads must not inject an owner fault.
        if initializing || caller /= owner
          then pure (monotonicInstant 100)
          else do
            atomically (readTMVar releaseFailure)
            ioError (userError "intentional owner clock failure")
      configuration =
        heraldRuntimeConfiguration
          fixtureCheckedGenesis
          fixtureCheckedBootstraps
          fixtureOracleContacts
          fixtureGeneratorSeedSource
          (runtimeMonotonicClock failingClock)
          (runtimePeerWorkDelayMicroseconds 0)
          (heraldRuntimeHandlers (const (pure ())))
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
  started <- startRecordedHeraldRuntime ProductionShapedRecording configuration
  recorded <- case started of
    Left failure -> assertFailure ("recorded runtime initialization failed: " <> show failure)
    Right running -> pure running
  ( do
      registration <-
        registerApplicationConnection
          (recordedHeraldRuntime recorded)
          (runtimeApplicationConnectionHandlers (const (pure LaneOffered)) (pure ()))
      reference <- case registration of
        ConnectionRegistered current -> pure current
        RegistrationStopped -> assertFailure "running recorded runtime rejected registration"
      assertEqual "the failing owner first accepts the public ingress" Queued
        =<< submitApplicationDto (recordedHeraldRuntime recorded) reference fixtureOpenDto
      atomically (putTMVar releaseFailure ())
      observed <- timeout 2000000 (snapshotRecordedHeraldRuntime recorded)
      trace <- case observed of
        Nothing -> assertFailure "owner failure left the final snapshot waiting"
        Just captured -> pure captured
      case runtimeTraceTerminal trace of
        RuntimeTraceFailed failure ->
          assertEqual
            "the unexpected owner exception is an essential-worker failure"
            HeraldRuntimeEssentialWorkerFailure
            (heraldRuntimeFailureClass failure)
        terminal -> assertFailure ("owner exception produced normal terminal evidence: " <> show terminal)
      assertEqual
        "the injected exception belongs to the owner, not the shared-clock timer worker"
        [OwnerWorker]
        [worker | ShellEvent _ (ShellChildFailed worker _) <- runtimeTraceEvents trace]
      case runtimeTraceFinalEvidence trace of
        RuntimeTraceFinalState _ -> assertReplay "owner-exception snapshot" trace
        RuntimeTraceFinalInvariantFault fault ->
          assertFailure ("ordinary owner exception was misclassified as a kernel fault: " <> show fault)
    )
    `finally` RuntimeInternal.runtimeCloseScope (recordedHeraldRuntime recorded)

propGeneratedParity :: NonNegative Int -> NonNegative Int -> Property
propGeneratedParity (NonNegative rawClosed) (NonNegative rawStale) =
  ioProperty $ do
    let script =
          ApplicationConformanceScript
            (fromIntegral (rawClosed `mod` 4))
            (fromIntegral (rawStale `mod` 4))
    production <- run ProductionShapedRecording script
    gated <- run DeterministicallyGatedRecording script
    pure $ case (production, gated) of
      (Right productionTrace, Right gatedTrace) ->
        normalizeRuntimeTrace productionTrace == normalizeRuntimeTrace gatedTrace
          && replaySucceeded productionTrace
          && replaySucceeded gatedTrace
      _ -> False

run ::
  RecordingSchedule ->
  ApplicationConformanceScript ->
  IO (Either ConformanceRunFailure RuntimeTrace)
run schedule =
  runApplicationConformanceScript schedule fixtureConfiguration fixtureOpenDto

requireTrace :: Either ConformanceRunFailure RuntimeTrace -> IO RuntimeTrace
requireTrace = \case
  Left failure -> assertFailure ("conformance run failed: " <> show failure)
  Right trace -> pure trace

assertReplay :: String -> RuntimeTrace -> Assertion
assertReplay label trace =
  case replay trace of
    Left mismatch -> assertFailure (label <> " typed replay failed: " <> show mismatch)
    Right _ -> pure ()

replaySucceeded :: RuntimeTrace -> Bool
replaySucceeded = either (const False) (const True) . replay

replay :: RuntimeTrace -> Either TypedReplayMismatch TypedReplayResult
replay trace = replayRuntimeTrace fixtureCheckedGenesis fixtureCheckedBootstraps trace

fixtureConfiguration :: HeraldRuntimeConfiguration
fixtureConfiguration =
  heraldRuntimeConfiguration
    fixtureCheckedGenesis
    fixtureCheckedBootstraps
    fixtureOracleContacts
    fixtureGeneratorSeedSource
    (runtimeMonotonicClock (pure (monotonicInstant 100)))
    (runtimePeerWorkDelayMicroseconds 0)
    (heraldRuntimeHandlers (const (pure ())))
    fixtureApplicationRecoveryConfiguration
    fixturePeerRecoveryConfiguration
