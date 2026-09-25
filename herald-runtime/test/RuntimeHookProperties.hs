module RuntimeHookProperties
  ( tests,
  )
where

import Control.Concurrent
  ( forkFinally,
    killThread,
    myThreadId,
  )
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
    tryTakeMVar,
  )
import Control.Concurrent.STM (atomically, check)
import Control.Exception
  ( AsyncException (ThreadKilled),
    catch,
    finally,
    throwIO,
  )
import Control.Monad (forM_, when)
import Data.ByteString qualified as ByteString
import Data.IORef
  ( IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
  )
import Eclips.Herald.Administration
  ( adminCorrelationId,
  )
import Eclips.Herald.Application.RPC (ApplicationOutbound (..))
import Eclips.Herald.EffectBatch
  ( HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Input
  ( HeraldInputBody (RuntimeObserved),
    RuntimeObservation (ApplicationCandidateLost),
    candidateApplicationLane,
    inputBody,
  )
import Eclips.Herald.Runtime
  ( HeraldRuntime,
    HeraldRuntimeConfiguration,
    RuntimeGeneratorSeedSource,
    RuntimeMonotonicClock,
    awaitHeraldRuntimeExit,
    configureHeraldRuntimePeerDialSink,
    exchangeHeraldJoin,
    heraldRuntimeConfiguration,
    requestHeraldRuntimeDrain,
    runtimeGeneratorSeedSource,
    runtimeMonotonicClock,
    runtimePeerWorkDelayMicroseconds,
    systemRuntimeGeneratorSeedSource,
    withHeraldRuntime,
  )
import Eclips.Herald.Runtime.Connection
  ( AdministrationPlane,
    ApplicationPlane,
    ConnectionRef,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeAdministrationConnectionHandlers,
    RuntimeApplicationConnectionHandlers,
    RuntimeLaneOffer (..),
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
  ( RecordingSchedule (..),
    recordedHeraldRuntime,
    snapshotRecordedRuntimeEvents,
    startRecordedHeraldRuntime,
  )
import Eclips.Herald.Runtime.Internal.Coordination
  ( BatchEnvelope (..),
  )
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    defaultRuntimeHooks,
    startHeraldRuntime,
    startHeraldRuntimeWithHooks,
    withHeraldRuntimeWithHooks,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (..),
    PhysicalConnectionRef (..),
    RuntimeTraceEvent (..),
    ShellTraceEvent (..),
    snapshotTraceLedger,
    traceLedgerCapturesHistory,
  )
import Eclips.Herald.Runtime.Internal.Types qualified as RuntimeInternal
import Eclips.Herald.Runtime.Trace
  ( HeraldRuntimeExit (..),
    HeraldRuntimeFailureClass
      ( HeraldRuntimeEssentialWorkerFailure,
        HeraldRuntimeInitializationFailure
      ),
    heraldRuntimeFailureClass,
  )
import Eclips.Herald.Time (monotonicInstant)
import RuntimeFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedBootstraps,
    fixtureCheckedGenesis,
    fixtureGeneratorSeedSource,
    fixtureOpenDto,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "deterministic runtime conformance gates"
    [ testCase "ordinary public and private startup retain ordinals without exact history" caseOrdinaryTraceModes,
      testCase "private hook startup explicitly retains exact conformance history" caseHookTraceModes,
      testCase "both actual conformance runners retain exact initialization history" caseConformanceTraceModes,
      testCase "BeginDrain owner handoff closes later external admission" caseDrainHandoffCut,
      testCase "a paused application disposition never retargets after close" casePausedDispositionClose,
      testCase "physical close before paused FinishDrain leaves no final offer or hang" casePausedFinishDrainClose,
      testCase "FinishDrain route evidence precedes visibility of its final writer offer" caseFinishDrainRouteLinearization,
      testCase "final administration cleanup precedes drain finalization on every writer exit" caseFinalAdministrationCleanupOrdering,
      testCase "post-dequeue writer exit retains the final administration acknowledgement" caseFinalAdministrationPostDequeueExit,
      testCase "a current writer exception is recorded before its exact cleanup" caseCurrentWriterFailureEvidence,
      testCase "a stale writer exception emits no current-connection failure evidence" caseStaleWriterFailureSuppressed,
      testCase "essential dispatcher failure cancels and joins a blocked connection child" caseEssentialFailureJoinsChild,
      testCase "supervised cancellation before the writer body still closes and joins its slot" caseCancellationBeforeWriterBody,
      testCase "a successful start acquires its generator seed exactly once" caseGeneratorSeedAcquiredOnce,
      testCase "a seed-source exception fails before initialization or clock sampling" caseGeneratorSeedSourceFailure,
      testCase "a wrong-width seed fails before initialization or clock sampling" caseGeneratorSeedWrongWidth,
      testCase "the system generator seed source starts through checked admission" caseSystemGeneratorSeedSource,
      testCase "initial clock failure is retained as typed startup failure" caseInitialClockFailure,
      testCase "caught dispatcher-hook cancellation leaves the exact batch queued for one route" caseDispatcherHookCancellationRetainsBatch,
      testCase "canceled join waiter cannot consume or retarget the next correlated reply" caseJoinWaiterCancellation,
      testCase "runtime shutdown releases a pending restricted join waiter" caseJoinWaiterShutdown
    ]

caseOrdinaryTraceModes :: Assertion
caseOrdinaryTraceModes = do
  scoped <- withHeraldRuntime fixtureConfiguration (pure . RuntimeInternal.runtimeCapturesTraceHistory)
  assertEqual
    "the public scope selects ordinal-only recording"
    (Right (False, HeraldRuntimeScopeClosed))
    scoped
  started <- startHeraldRuntime fixtureConfiguration
  case started of
    Left failure -> assertFailure ("ordinary startup failed: " <> show failure)
    Right runtime ->
      assertEqual
        "ordinary private startup also omits exact history"
        False
        (RuntimeInternal.runtimeCapturesTraceHistory runtime)
        `finally` RuntimeInternal.runtimeCloseScope runtime

caseHookTraceModes :: Assertion
caseHookTraceModes = do
  ledgerCell <- newEmptyMVar
  let hooks =
        defaultRuntimeHooks
          { hookRuntimeInitialized = \_ _ _ _ _ _ -> putOnce ledgerCell
          }
  scoped <- withHeraldRuntimeWithHooks hooks fixtureConfiguration (pure . RuntimeInternal.runtimeCapturesTraceHistory)
  assertEqual
    "the private hook scope selects exact recording"
    (Right (True, HeraldRuntimeScopeClosed))
    scoped
  ledger <- awaitMVar "exact trace ledger" ledgerCell
  assertEqual "the initialized hook receives a capturing ledger" True (traceLedgerCapturesHistory ledger)
  events <- atomically (snapshotTraceLedger ledger)
  assertEqual
    "the conformance history retains the exact initialization"
    True
    (any (\case KernelEvent _ KernelTraceInitialized {} -> True; _ -> False) events)
  assertEqual
    "the closed scope retains its final exit witness"
    True
    (any (\case ShellEvent _ (ShellRuntimeExited HeraldRuntimeScopeClosed) -> True; _ -> False) events)
  withHookedRuntime defaultRuntimeHooks $ \runtime ->
    assertEqual
      "direct hook startup also captures exact history"
      True
      (RuntimeInternal.runtimeCapturesTraceHistory runtime)

caseConformanceTraceModes :: Assertion
caseConformanceTraceModes =
  forM_ [ProductionShapedRecording, DeterministicallyGatedRecording] $ \schedule -> do
    started <- startRecordedHeraldRuntime schedule fixtureConfiguration
    case started of
      Left failure -> assertFailure ("conformance startup failed: " <> show failure)
      Right recorded ->
        ( do
            assertEqual
              "the actual conformance runner captures exact history"
              True
              (RuntimeInternal.runtimeCapturesTraceHistory (recordedHeraldRuntime recorded))
            events <- snapshotRecordedRuntimeEvents recorded
            assertEqual
              "initialization is available to the conformance recorder"
              True
              (any (\case KernelEvent _ KernelTraceInitialized {} -> True; _ -> False) events)
        )
          `finally` RuntimeInternal.runtimeCloseScope (recordedHeraldRuntime recorded)

caseDrainHandoffCut :: Assertion
caseDrainHandoffCut = do
  drainHanded <- newEmptyMVar
  releaseDrain <- newEmptyMVar
  applicationClosed <- newEmptyMVar
  let hooks =
        gateFirstMatchingBatch
          hasBeginDrain
          drainHanded
          releaseDrain
  withHookedRuntime hooks $ \runtime -> do
    application <-
      registerApplication
        runtime
        (runtimeApplicationConnectionHandlers (const (pure LaneOffered)) (putOnce applicationClosed ()))
    administration <- registerAdministration runtime quietAdministrationHandlers
    assertEqual "the first drain request is queued" Queued
      =<< requestHeraldRuntimeDrain runtime administration (adminCorrelationId 201)
    _ <- awaitMVar "BeginDrain dispatcher gate" drainHanded
    assertEqual
      "the owner handoff, not later dispatcher progress, defines the admission cut"
      IngressClosedForDrain
      =<< submitApplicationDto runtime application fixtureOpenDto
    putMVar releaseDrain ()
    assertEqual "the released drain reaches its semantic exit" (Right HeraldRuntimeDrained)
      =<< awaitRuntimeExit "released BeginDrain" runtime
    assertEqual
      "the joined runtime scope has completed application close"
      (Just ())
      =<< tryTakeMVar applicationClosed

casePausedDispositionClose :: Assertion
casePausedDispositionClose = do
  ledgerCell <- newEmptyMVar
  dispositionHanded <- newEmptyMVar
  releaseDisposition <- newEmptyMVar
  oldOutput <- newIORef []
  newOutput <- newIORef []
  oldClosed <- newEmptyMVar
  newClosed <- newEmptyMVar
  newOutputSeen <- newEmptyMVar
  let hooks =
        ( gateFirstMatchingBatch
            hasApplicationDisposition
            dispositionHanded
            releaseDisposition
        )
          { hookRuntimeInitialized = \_ _ _ _ _ _ -> putOnce ledgerCell
          }
      oldHandlers = recordingApplicationHandlers oldOutput Nothing oldClosed
      newHandlers = recordingApplicationHandlers newOutput (Just newOutputSeen) newClosed
  withHookedRuntime hooks $ \runtime -> do
    oldReference <- registerApplication runtime oldHandlers
    assertEqual "the old Open enters its source FIFO" Queued
      =<< submitApplicationDto runtime oldReference fixtureOpenDto
    _ <- awaitMVar "application disposition dispatcher gate" dispositionHanded
    assertEqual "the old route closes while its exact batch is paused" ConnectionClosed
      =<< closeRuntimeConnection runtime oldReference
    _ <- awaitMVar "old application close" oldClosed
    ledger <- readMVar ledgerCell
    let oldCandidate = candidateApplicationLane (RuntimeInternal.connectionRefOrdinal oldReference)
    candidateLoss <- timeout 2_000_000 $ atomically $ do
      events <- snapshotTraceLedger ledger
      check
        ( any
            ( \case
                KernelEvent _ (KernelTraceStepped _ _ input (Right _)) ->
                  inputBody input == RuntimeObserved (ApplicationCandidateLost oldCandidate)
                _ -> False
            )
            events
        )
    assertEqual "physical close retires the exact candidate while its disposition is paused" (Just ()) candidateLoss

    newReference <- registerApplication runtime newHandlers
    putMVar releaseDisposition ()
    assertEqual "the new route admits its own Open" Queued
      =<< submitApplicationDto runtime newReference fixtureOpenDto
    outboundResult <- timeout 2000000 (takeMVar newOutputSeen)
    outbound <- case outboundResult of
      Just observed -> pure observed
      Nothing -> do
        terminal <- RuntimeInternal.runtimeTryExit runtime
        assertFailure ("new application disposition timed out; runtime terminal = " <> show terminal)
    case outbound of
      AcceptApplicationCandidate {} -> pure ()
      _ -> assertFailure ("expected application acceptance, got " <> show outbound)
    assertEqual "the new route closes after draining its writer FIFO" ConnectionClosed
      =<< closeRuntimeConnection runtime newReference
    _ <- awaitMVar "new application close" newClosed
    pure ()
  assertEqual "the stale exact target receives no disposition" [] =<< readIORef oldOutput
  recorded <- readIORef newOutput
  assertEqual "the replacement receives only its own disposition" 1 (length recorded)

casePausedFinishDrainClose :: Assertion
casePausedFinishDrainClose = do
  finishHanded <- newEmptyMVar
  releaseFinish <- newEmptyMVar
  offers <- newIORef []
  closed <- newEmptyMVar
  let hooks =
        gateFirstMatchingBatch
          hasFinishDrain
          finishHanded
          releaseFinish
      handlers =
        runtimeAdministrationConnectionHandlers
          (\reply -> append offers reply >> pure LaneOffered)
          (putOnce closed ())
  withHookedRuntime hooks $ \runtime -> do
    administration <- registerAdministration runtime handlers
    assertEqual "the drain request is queued" Queued
      =<< requestHeraldRuntimeDrain runtime administration (adminCorrelationId 202)
    _ <- awaitMVar "FinishDrain dispatcher gate" finishHanded
    assertEqual "physical close wins before final routing" ConnectionClosed
      =<< closeRuntimeConnection runtime administration
    _ <- awaitMVar "administration close before FinishDrain" closed
    assertEqual "the paused final batch has not published terminal state" Nothing
      =<< RuntimeInternal.runtimeTryExit runtime
    putMVar releaseFinish ()
    assertEqual "a missing optional administration lane cannot hang drain" (Right HeraldRuntimeDrained)
      =<< awaitRuntimeExit "released FinishDrain" runtime
  assertEqual "no final reply is offered after its physical lane was closed" [] =<< readIORef offers

caseFinishDrainRouteLinearization :: Assertion
caseFinishDrainRouteLinearization = do
  ledgerCell <- newEmptyMVar
  routedBeforeOffer <- newEmptyMVar
  let hooks =
        defaultRuntimeHooks
          { hookRuntimeInitialized = \_ _ _ _ _ _ -> putOnce ledgerCell
          }
      handlers =
        runtimeAdministrationConnectionHandlers
          ( \_ -> do
              ledger <- readMVar ledgerCell
              events <- atomically (snapshotTraceLedger ledger)
              putOnce routedBeforeOffer (any isFinishDrainRoute events)
              pure LaneOffered
          )
          (pure ())
  withHookedRuntime hooks $ \runtime -> do
    administration <- registerAdministration runtime handlers
    assertEqual "the drain request is queued" Queued
      =<< requestHeraldRuntimeDrain runtime administration (adminCorrelationId 203)
    assertEqual
      "the writer cannot observe its final offer until exact route evidence commits"
      True
      =<< awaitMVar "FinishDrain route before final offer" routedBeforeOffer
    assertEqual "the evidenced final route drains normally" (Right HeraldRuntimeDrained)
      =<< awaitRuntimeExit "FinishDrain route evidence" runtime

caseFinalAdministrationCleanupOrdering :: Assertion
caseFinalAdministrationCleanupOrdering =
  forM_
    [ ("LaneOffered", pure LaneOffered, 0),
      ("LaneLost", pure LaneLost, 0),
      ("exception", ioError (userError "intentional final administration failure"), 1)
    ]
    $ \(label, offerResult, expectedFailures) -> do
      ledgerCell <- newEmptyMVar
      let hooks =
            defaultRuntimeHooks
              { hookRuntimeInitialized = \_ _ _ _ _ _ -> putOnce ledgerCell
              }
          handlers =
            runtimeAdministrationConnectionHandlers
              (const offerResult)
              (pure ())
      withHookedRuntime hooks $ \runtime -> do
        reference <- registerAdministration runtime handlers
        assertEqual (label <> " drain request is queued") Queued
          =<< requestHeraldRuntimeDrain runtime reference (adminCorrelationId 204)
        assertEqual (label <> " final writer exit retains semantic drain") (Right HeraldRuntimeDrained)
          =<< awaitRuntimeExit (label <> " final writer exit") runtime
        ledger <- readMVar ledgerCell
        events <- atomically (snapshotTraceLedger ledger)
        let physical = PhysicalAdministrationRef reference
            failureIndices = eventIndices (isWriterFailure physical) events
            closeIndices = eventIndices (isPhysicalClose physical) events
            finalizedIndices = eventIndices isDrainFinalized events
        assertEqual (label <> " has the expected writer failure evidence") expectedFailures (length failureIndices)
        assertEqual (label <> " has one exact physical cleanup") 1 (length closeIndices)
        assertEqual (label <> " has one exact drain finalization") 1 (length finalizedIndices)
        assertEqual
          (label <> " cleanup acknowledgement cannot overtake physical invalidation")
          True
          (closeIndices < finalizedIndices)

caseFinalAdministrationPostDequeueExit :: Assertion
caseFinalAdministrationPostDequeueExit = do
  ledgerCell <- newEmptyMVar
  targetCell <- newEmptyMVar
  commandDequeued <- newEmptyMVar
  closeObserved <- newEmptyMVar
  offers <- newIORef ([] :: [()])
  let hooks =
        defaultRuntimeHooks
          { hookRuntimeInitialized = \_ _ _ _ _ _ -> putOnce ledgerCell,
            hookAfterWriterCommandDequeued = \physical -> do
              target <- readMVar targetCell
              when (physical == target) $ do
                putOnce commandDequeued ()
                throwIO ThreadKilled
          }
      handlers =
        runtimeAdministrationConnectionHandlers
          (\_ -> append offers () >> pure LaneOffered)
          (putOnce closeObserved ())
  withHookedRuntime hooks $ \runtime -> do
    reference <- registerAdministration runtime handlers
    let physical = PhysicalAdministrationRef reference
    putMVar targetCell physical
    assertEqual "the drain request is queued" Queued
      =<< requestHeraldRuntimeDrain runtime reference (adminCorrelationId 205)
    _ <- awaitMVar "final administration command dequeue" commandDequeued
    _ <- awaitMVar "post-dequeue writer cleanup" closeObserved
    assertEqual
      "the outer writer exception path retains the terminal acknowledgement"
      (Right HeraldRuntimeDrained)
      =<< awaitRuntimeExit "post-dequeue final writer exit" runtime
    ledger <- readMVar ledgerCell
    events <- atomically (snapshotTraceLedger ledger)
    let failureIndices = eventIndices (isWriterFailure physical) events
        closeIndices = eventIndices (isPhysicalClose physical) events
        finalizedIndices = eventIndices isDrainFinalized events
    assertEqual "the interrupted command records one current-writer failure" 1 (length failureIndices)
    assertEqual "the interrupted command has one exact physical cleanup" 1 (length closeIndices)
    assertEqual "the interrupted command has one exact drain finalization" 1 (length finalizedIndices)
    assertEqual
      "post-dequeue cleanup still precedes drain finalization"
      True
      (closeIndices < finalizedIndices)
  assertEqual "the interrupted final command never reaches the lane callback" [] =<< readIORef offers

caseCurrentWriterFailureEvidence :: Assertion
caseCurrentWriterFailureEvidence = do
  ledgerCell <- newEmptyMVar
  offerEntered <- newEmptyMVar
  closed <- newEmptyMVar
  let hooks =
        defaultRuntimeHooks
          { hookRuntimeInitialized = \_ _ _ _ _ _ -> putOnce ledgerCell
          }
      handlers =
        runtimeApplicationConnectionHandlers
          (\_ -> putOnce offerEntered () >> ioError (userError "intentional current writer failure"))
          (putOnce closed ())
  withHookedRuntime hooks $ \runtime -> do
    reference <- registerApplication runtime handlers
    assertEqual "the Open reaches the current writer" Queued
      =<< submitApplicationDto runtime reference fixtureOpenDto
    _ <- awaitMVar "current failing writer callback" offerEntered
    _ <- awaitMVar "current failing writer cleanup" closed
    ledger <- readMVar ledgerCell
    events <- atomically (snapshotTraceLedger ledger)
    let physical = PhysicalApplicationRef reference
        failureIndices = eventIndices (isWriterFailure physical) events
        closeIndices = eventIndices (isPhysicalClose physical) events
    assertEqual "the current callback has one exact private failure witness" 1 (length failureIndices)
    assertEqual "the current callback has one exact physical cleanup" 1 (length closeIndices)
    assertEqual
      "failure evidence precedes physical cleanup"
      True
      (failureIndices < closeIndices)

caseStaleWriterFailureSuppressed :: Assertion
caseStaleWriterFailureSuppressed = do
  ledgerCell <- newEmptyMVar
  oldEntered <- newEmptyMVar
  releaseOld <- newEmptyMVar
  oldClosed <- newEmptyMVar
  replacementOffered <- newEmptyMVar
  replacementClosed <- newEmptyMVar
  let hooks =
        defaultRuntimeHooks
          { hookRuntimeInitialized = \_ _ _ _ _ _ -> putOnce ledgerCell
          }
      oldHandlers =
        runtimeApplicationConnectionHandlers
          (\_ -> putOnce oldEntered () >> takeMVar releaseOld >> ioError (userError "intentional stale writer failure"))
          (putOnce oldClosed ())
      replacementHandlers =
        runtimeApplicationConnectionHandlers
          (\_ -> putOnce replacementOffered () >> pure LaneOffered)
          (putOnce replacementClosed ())
  withHookedRuntime hooks $ \runtime -> do
    oldReference <- registerApplication runtime oldHandlers
    assertEqual "the old Open reaches its writer" Queued
      =<< submitApplicationDto runtime oldReference fixtureOpenDto
    _ <- awaitMVar "old writer callback" oldEntered
    replacement <- registerApplication runtime replacementHandlers
    assertEqual "the exact retry reaches the replacement source" Queued
      =<< submitApplicationDto runtime replacement fixtureOpenDto
    _ <- awaitMVar "replacement writer callback" replacementOffered
    putMVar releaseOld ()
    _ <- awaitMVar "stale writer cleanup" oldClosed
    ledger <- readMVar ledgerCell
    events <- atomically (snapshotTraceLedger ledger)
    assertEqual
      "the late stale exception cannot masquerade as the current writer's failure"
      []
      (eventIndices (isWriterFailure (PhysicalApplicationRef oldReference)) events)
    assertEqual "the replacement remains current" ConnectionClosed
      =<< closeRuntimeConnection runtime replacement
    _ <- awaitMVar "replacement cleanup" replacementClosed
    pure ()

caseEssentialFailureJoinsChild :: Assertion
caseEssentialFailureJoinsChild = do
  closeEntered <- newEmptyMVar
  releaseClose <- newEmptyMVar
  let hooks =
        defaultRuntimeHooks
          { hookBeforeDispatchBatch = \batch ->
              when (hasApplicationDisposition batch) $ ioError (userError "intentional dispatcher failure")
          }
      handlers =
        runtimeApplicationConnectionHandlers
          (const (pure LaneOffered))
          (putOnce closeEntered () >> takeMVar releaseClose)
  withHookedRuntime hooks $ \runtime -> do
    application <- registerApplication runtime handlers
    assertEqual "the Open is accepted before the injected dispatcher failure" Queued
      =<< submitApplicationDto runtime application fixtureOpenDto
    _ <- awaitMVar "cancelled connection close callback" closeEntered
    assertEqual "terminal publication waits for the connection child join" Nothing
      =<< RuntimeInternal.runtimeTryExit runtime
    putMVar releaseClose ()
    outcome <- awaitRuntimeExit "essential dispatcher failure" runtime
    case outcome of
      Left failure ->
        assertEqual
          "the first essential failure class is retained"
          HeraldRuntimeEssentialWorkerFailure
          (heraldRuntimeFailureClass failure)
      Right runtimeExit -> assertFailure ("essential failure produced a normal exit: " <> show runtimeExit)

caseCancellationBeforeWriterBody :: Assertion
caseCancellationBeforeWriterBody = do
  entered <- newEmptyMVar
  neverStart <- newEmptyMVar
  closed <- newEmptyMVar
  closeRelease <- newEmptyMVar
  let hooks =
        defaultRuntimeHooks
          { hookBeforeConnectionWriterBody = \_ -> putOnce entered () >> readMVar neverStart,
            hookBeforeDispatchBatch = \batch ->
              when (hasApplicationDisposition batch) (ioError (userError "fail before connection writer starts"))
          }
      handlers =
        runtimeApplicationConnectionHandlers
          (const (assertFailure "writer body started after its pre-start cancellation"))
          (putOnce closed () >> readMVar closeRelease)
  withHookedRuntime hooks $ \runtime -> flip finally (putOnce closeRelease ()) $ do
    application <- registerApplication runtime handlers
    _ <- awaitMVar "writer parked before its first command" entered
    assertEqual "Open reaches the owner while its writer has never run" Queued
      =<< submitApplicationDto runtime application fixtureOpenDto
    _ <- awaitMVar "pre-start cancellation invoked exact slot cleanup" closed
    assertEqual "terminal result waits for the pre-start child's close" Nothing
      =<< RuntimeInternal.runtimeTryExit runtime
    putMVar closeRelease ()
    outcome <- awaitRuntimeExit "pre-start connection cancellation" runtime
    case outcome of
      Left failure ->
        assertEqual
          "dispatcher failure remains the terminal cause"
          HeraldRuntimeEssentialWorkerFailure
          (heraldRuntimeFailureClass failure)
      Right exit -> assertFailure ("pre-start cancellation unexpectedly succeeded: " <> show exit)

caseGeneratorSeedAcquiredOnce :: Assertion
caseGeneratorSeedAcquiredOnce = do
  sourceCalls <- newIORef (0 :: Int)
  let source =
        runtimeGeneratorSeedSource $ do
          atomicModifyIORef' sourceCalls (\count -> (count + 1, ()))
          pure (ByteString.pack [0 .. 31])
      configuration =
        configureHeraldRuntimePeerDialSink
          (const (pure ()))
          (configurationWithSeedSource source (runtimeMonotonicClock (pure (monotonicInstant 100))))
  started <- startHeraldRuntimeWithHooks defaultRuntimeHooks configuration
  runtime <- case started of
    Left failure -> assertFailure ("fixed seed source failed to start: " <> show failure)
    Right running -> pure running
  assertEqual "immutable peer-dial refinement preserves and invokes the source once" 1 =<< readIORef sourceCalls
  RuntimeInternal.runtimeCloseScope runtime
  assertEqual "physical scope close does not reacquire a seed" 1 =<< readIORef sourceCalls

caseGeneratorSeedSourceFailure :: Assertion
caseGeneratorSeedSourceFailure = do
  sourceCalls <- newIORef (0 :: Int)
  clockCalls <- newIORef (0 :: Int)
  initializationCalls <- newIORef (0 :: Int)
  let source =
        runtimeGeneratorSeedSource $ do
          atomicModifyIORef' sourceCalls (\count -> (count + 1, ()))
          ioError (userError "intentional seed-source failure")
      clock = runtimeMonotonicClock $ do
        atomicModifyIORef' clockCalls (\count -> (count + 1, ()))
        pure (monotonicInstant 100)
      hooks =
        defaultRuntimeHooks
          { hookRuntimeInitialized = \_ _ _ _ _ _ _ ->
              atomicModifyIORef' initializationCalls (\count -> (count + 1, ()))
          }
  assertSeedStartFailure hooks (configurationWithSeedSource source clock)
  assertEqual "the failing source is not retried" 1 =<< readIORef sourceCalls
  assertEqual "the owner clock is not sampled" 0 =<< readIORef clockCalls
  assertEqual "the initialization hook is not called" 0 =<< readIORef initializationCalls

caseGeneratorSeedWrongWidth :: Assertion
caseGeneratorSeedWrongWidth = do
  clockCalls <- newIORef (0 :: Int)
  initializationCalls <- newIORef (0 :: Int)
  let source = runtimeGeneratorSeedSource (pure (ByteString.replicate 31 0x5a))
      clock = runtimeMonotonicClock $ do
        atomicModifyIORef' clockCalls (\count -> (count + 1, ()))
        pure (monotonicInstant 100)
      hooks =
        defaultRuntimeHooks
          { hookRuntimeInitialized = \_ _ _ _ _ _ _ ->
              atomicModifyIORef' initializationCalls (\count -> (count + 1, ()))
          }
  assertSeedStartFailure hooks (configurationWithSeedSource source clock)
  assertEqual "the owner clock is not sampled" 0 =<< readIORef clockCalls
  assertEqual "the initialization hook is not called" 0 =<< readIORef initializationCalls

caseSystemGeneratorSeedSource :: Assertion
caseSystemGeneratorSeedSource = do
  started <-
    startHeraldRuntimeWithHooks
      defaultRuntimeHooks
      (configurationWithSeedSource systemRuntimeGeneratorSeedSource (runtimeMonotonicClock (pure (monotonicInstant 100))))
  runtime <- case started of
    Left failure -> assertFailure ("system seed source failed checked startup: " <> show failure)
    Right running -> pure running
  RuntimeInternal.runtimeCloseScope runtime

assertSeedStartFailure :: RuntimeHooks -> HeraldRuntimeConfiguration -> Assertion
assertSeedStartFailure hooks configuration = do
  started <- startHeraldRuntimeWithHooks hooks configuration
  case started of
    Left failure ->
      assertEqual
        "seed acquisition failure has the redacted initialization class"
        HeraldRuntimeInitializationFailure
        (heraldRuntimeFailureClass failure)
    Right runtime -> do
      RuntimeInternal.runtimeCloseScope runtime
      assertFailure "invalid seed acquisition published a runtime handle"

caseInitialClockFailure :: Assertion
caseInitialClockFailure = do
  let configuration =
        heraldRuntimeConfiguration
          fixtureCheckedGenesis
          fixtureCheckedBootstraps
          fixtureOracleContacts
          fixtureGeneratorSeedSource
          (runtimeMonotonicClock (ioError (userError "intentional initial clock failure")))
          (runtimePeerWorkDelayMicroseconds 0)
          (heraldRuntimeHandlers (const (pure ())))
          fixtureApplicationRecoveryConfiguration
          fixturePeerRecoveryConfiguration
  started <- startHeraldRuntimeWithHooks defaultRuntimeHooks configuration
  case started of
    Left failure ->
      assertEqual
        "raw initial clock exceptions map to the startup failure class"
        HeraldRuntimeInitializationFailure
        (heraldRuntimeFailureClass failure)
    Right runtime -> do
      RuntimeInternal.runtimeCloseScope runtime
      assertFailure "an initial clock exception published a usable runtime handle"

caseDispatcherHookCancellationRetainsBatch :: Assertion
caseDispatcherHookCancellationRetainsBatch = do
  hookEntered <- newEmptyMVar
  cancelHook <- newEmptyMVar
  applicationOffered <- newEmptyMVar
  let hooks =
        defaultRuntimeHooks
          { hookBeforeDispatchBatch = \batch ->
              when (hasApplicationDisposition batch) $ do
                thread <- myThreadId
                first <- tryPutMVar hookEntered thread
                when first $ do
                  takeMVar cancelHook
                    `catch` \exception -> case exception of
                      ThreadKilled -> pure ()
                      _ -> throwIO exception
          }
      handlers =
        runtimeApplicationConnectionHandlers
          (\_ -> putOnce applicationOffered () >> pure LaneOffered)
          (pure ())
  withHookedRuntime hooks $ \runtime -> do
    application <- registerApplication runtime handlers
    assertEqual "the cancellation-regression Open enters its source" Queued
      =<< submitApplicationDto runtime application fixtureOpenDto
    dispatcher <- awaitMVar "dispatcher hook entry" hookEntered
    killThread dispatcher
    _ <- awaitMVar "the retained batch routes after caught hook cancellation" applicationOffered
    assertEqual "the runtime remains live after hook-local caught cancellation" Nothing
      =<< RuntimeInternal.runtimeTryExit runtime

caseJoinWaiterCancellation :: Assertion
caseJoinWaiterCancellation = do
  firstEntered <- newEmptyMVar
  firstRelease <- newEmptyMVar
  secondEntered <- newEmptyMVar
  secondRelease <- newEmptyMVar
  firstDone <- newEmptyMVar
  secondDone <- newEmptyMVar
  let hooks =
        defaultRuntimeHooks
          { hookBeforeDispatchBatch = \batch -> forM_ (effectBatchMembers (batchEnvelopeEffects batch)) $ \case
              SendJoinReply 0 _ -> putOnce firstEntered () >> readMVar firstRelease
              SendJoinReply 1 _ -> putOnce secondEntered () >> readMVar secondRelease
              _ -> pure ()
          }
  withHookedRuntime hooks $ \runtime -> do
    -- Unknown payload tags still receive a correlated pure-owner rejection.
    first <- forkFinally (exchangeHeraldJoin runtime (ByteString.singleton 254)) (putMVar firstDone)
    _ <- awaitMVar "first restricted reply selected" firstEntered
    killThread first
    _ <- awaitMVar "canceled restricted caller joined" firstDone
    second <- forkFinally (exchangeHeraldJoin runtime (ByteString.singleton 255)) (putMVar secondDone)
    ( do
        putMVar firstRelease ()
        _ <- awaitMVar "second restricted reply selected" secondEntered
        premature <- tryTakeMVar secondDone
        case premature of
          Nothing -> pure ()
          Just _ -> assertFailure "the stale first reply completed the replacement waiter"
        putMVar secondRelease ()
        result <- awaitMVar "second restricted reply delivered" secondDone
        case result of
          Right (Right _) -> pure ()
          other -> assertFailure ("second join exchange failed: " <> show other)
        assertEqual "cancellation leaves the owner live" Nothing =<< RuntimeInternal.runtimeTryExit runtime
      )
      `finally` killThread second

caseJoinWaiterShutdown :: Assertion
caseJoinWaiterShutdown = do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  done <- newEmptyMVar
  let hooks =
        gateFirstMatchingBatch
          (any (\case SendJoinReply {} -> True; _ -> False) . effectBatchMembers . batchEnvelopeEffects)
          entered
          release
  withHookedRuntime hooks $ \runtime -> do
    caller <- forkFinally (exchangeHeraldJoin runtime (ByteString.singleton 254)) (putMVar done)
    ( do
        _ <- awaitMVar "restricted reply held during shutdown" entered
        RuntimeInternal.runtimeCloseScope runtime
        result <- awaitMVar "shutdown released restricted waiter" done
        case result of
          Right (Left RuntimeStopped) -> pure ()
          other -> assertFailure ("join waiter did not observe terminal runtime: " <> show other)
      )
      `finally` killThread caller

gateFirstMatchingBatch ::
  (BatchEnvelope -> Bool) ->
  MVar () ->
  MVar () ->
  RuntimeHooks
gateFirstMatchingBatch matches entered release =
  defaultRuntimeHooks
    { hookBeforeDispatchBatch = \batch ->
        when (matches batch) $ do
          first <- tryPutMVar entered ()
          when first (readMVar release)
    }

hasBeginDrain :: BatchEnvelope -> Bool
hasBeginDrain = any isBegin . effectBatchMembers . batchEnvelopeEffects
  where
    isBegin BeginDrain {} = True
    isBegin _ = False

hasFinishDrain :: BatchEnvelope -> Bool
hasFinishDrain = any isFinish . effectBatchMembers . batchEnvelopeEffects
  where
    isFinish FinishDrain {} = True
    isFinish _ = False

hasApplicationDisposition :: BatchEnvelope -> Bool
hasApplicationDisposition = any isDisposition . effectBatchMembers . batchEnvelopeEffects
  where
    isDisposition SetApplicationConnectionDisposition {} = True
    isDisposition RejectApplicationConnection {} = True
    isDisposition _ = False

isFinishDrainRoute :: RuntimeTraceEvent -> Bool
isFinishDrainRoute (ShellEvent _ (ShellEffectRouted _ _ FinishDrain {})) = True
isFinishDrainRoute _ = False

isWriterFailure :: PhysicalConnectionRef -> RuntimeTraceEvent -> Bool
isWriterFailure expected (ShellEvent _ (ShellConnectionWriterFailed actual)) = actual == expected
isWriterFailure _ _ = False

isPhysicalClose :: PhysicalConnectionRef -> RuntimeTraceEvent -> Bool
isPhysicalClose expected (ShellEvent _ (ShellConnectionClosed actual _)) = actual == expected
isPhysicalClose _ _ = False

isDrainFinalized :: RuntimeTraceEvent -> Bool
isDrainFinalized (ShellEvent _ ShellDrainFinalized {}) = True
isDrainFinalized _ = False

eventIndices :: (RuntimeTraceEvent -> Bool) -> [RuntimeTraceEvent] -> [Int]
eventIndices matches events =
  [index | (index, event) <- zip [0 ..] events, matches event]

withHookedRuntime :: RuntimeHooks -> (HeraldRuntime -> IO ()) -> Assertion
withHookedRuntime hooks use = do
  started <- startHeraldRuntimeWithHooks hooks fixtureConfiguration
  case started of
    Left failure -> assertFailure ("runtime initialization failed: " <> show failure)
    Right runtime -> use runtime `finally` RuntimeInternal.runtimeCloseScope runtime

fixtureConfiguration :: HeraldRuntimeConfiguration
fixtureConfiguration =
  configurationWithSeedSource
    fixtureGeneratorSeedSource
    (runtimeMonotonicClock (pure (monotonicInstant 100)))

configurationWithSeedSource ::
  RuntimeGeneratorSeedSource ->
  RuntimeMonotonicClock ->
  HeraldRuntimeConfiguration
configurationWithSeedSource seedSource clock =
  heraldRuntimeConfiguration
    fixtureCheckedGenesis
    fixtureCheckedBootstraps
    fixtureOracleContacts
    seedSource
    clock
    (runtimePeerWorkDelayMicroseconds 0)
    (heraldRuntimeHandlers (const (pure ())))
    fixtureApplicationRecoveryConfiguration
    fixturePeerRecoveryConfiguration

registerApplication ::
  HeraldRuntime ->
  RuntimeApplicationConnectionHandlers ->
  IO (ConnectionRef ApplicationPlane)
registerApplication runtime handlers = do
  registration <- registerApplicationConnection runtime handlers
  case registration of
    ConnectionRegistered reference -> pure reference
    RegistrationStopped -> assertFailure "running runtime rejected application registration"

registerAdministration ::
  HeraldRuntime ->
  RuntimeAdministrationConnectionHandlers ->
  IO (ConnectionRef AdministrationPlane)
registerAdministration runtime handlers = do
  registration <- registerAdministrationConnection runtime handlers
  case registration of
    AdministrationConnectionRegistered reference -> pure reference
    AdministrationRegistrationUnavailable -> assertFailure "first administration registration was unavailable"
    AdministrationRegistrationStopped -> assertFailure "running runtime rejected administration registration"

quietAdministrationHandlers :: RuntimeAdministrationConnectionHandlers
quietAdministrationHandlers =
  runtimeAdministrationConnectionHandlers (const (pure LaneOffered)) (pure ())

recordingApplicationHandlers ::
  IORef [ApplicationOutbound] ->
  Maybe (MVar ApplicationOutbound) ->
  MVar () ->
  RuntimeApplicationConnectionHandlers
recordingApplicationHandlers outputs observed closed =
  runtimeApplicationConnectionHandlers
    ( \outbound -> do
        append outputs outbound
        maybe (pure ()) (\cell -> putOnce cell outbound) observed
        pure LaneOffered
    )
    (putOnce closed ())

append :: IORef [value] -> value -> IO ()
append events event = atomicModifyIORef' events (\retained -> (retained <> [event], ()))

putOnce :: MVar value -> value -> IO ()
putOnce cell value = do
  _ <- tryPutMVar cell value
  pure ()

awaitMVar :: String -> MVar value -> IO value
awaitMVar context cell = do
  result <- timeout 2000000 (takeMVar cell)
  case result of
    Just value -> pure value
    Nothing -> assertFailure (context <> " timed out")

awaitRuntimeExit ::
  String ->
  HeraldRuntime ->
  IO (Either RuntimeInternal.HeraldRuntimeFailure HeraldRuntimeExit)
awaitRuntimeExit context runtime = do
  result <- timeout 2000000 (awaitHeraldRuntimeExit runtime)
  case result of
    Just observed -> pure observed
    Nothing -> assertFailure (context <> " timed out")
