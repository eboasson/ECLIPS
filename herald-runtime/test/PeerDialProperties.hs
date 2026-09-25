{-# LANGUAGE OverloadedStrings #-}

module PeerDialProperties
  ( tests,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
  )
import Control.Concurrent.STM
  ( atomically,
    newEmptyTMVar,
    newTVar,
    putTMVar,
    readTMVar,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Exception (finally)
import Control.Monad (forM_, when)
import Data.IORef
  ( IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
  )
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Herald.Administration (adminCorrelationId)
import Eclips.Herald.Discovery
  ( PeerAddress,
    PeerBinding,
    PeerDialIntent,
    PeerHelloDisposition (PeerHelloAccepted),
    peerAddress,
    peerCandidateConnectionNonce,
    peerDialIntentAddresses,
    peerDialIntentHeraldEpoch,
    peerDialIntentHeraldId,
    peerHello,
    peerHelloAppliedControlIndex,
    peerHelloCatalogueDigest,
    peerHelloHeraldEpoch,
    peerHelloHeraldId,
    peerHelloInitialProjectionDigest,
    peerHelloSystemId,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (..),
    effectBatchMembers,
    orderedEffectBatch,
  )
import Eclips.Herald.Input
  ( HeraldInputBody (RuntimeObserved),
    RuntimeObservation (TimerObserved),
    inputBody,
  )
import Eclips.Herald.PeerDispatch (PeerDispatchOutcome (PeerDispatchWritten))
import Eclips.Herald.PeerLiveness
  ( PeerRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
  )
import Eclips.Herald.Runtime
  ( HeraldRuntime,
    HeraldRuntimeConfiguration,
    awaitHeraldRuntimeExit,
    configureHeraldRuntimePeerDialCancellationSink,
    configureHeraldRuntimePeerDialSink,
    heraldRuntimeConfiguration,
    requestHeraldRuntimeDrain,
    runtimeMonotonicClock,
    runtimePeerWorkDelayMicroseconds,
  )
import Eclips.Herald.Runtime.Connection
  ( AdministrationPlane,
    ConnectionRef,
    PeerPlane,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeLaneOffer (LaneOffered),
    heraldRuntimeHandlers,
    runtimeAdministrationConnectionHandlers,
    runtimePeerConnectionHandlers,
  )
import Eclips.Herald.Runtime.Ingress
  ( RuntimeAdministrationRegistration (..),
    RuntimeRegistration (..),
    RuntimeSubmission (ConnectionClosed, Queued, StalePhysicalGeneration),
    closeRuntimeConnection,
    registerAdministrationConnection,
    registerPeerConnection,
    submitPeerHello,
  )
import Eclips.Herald.Runtime.Internal.Conformance
  ( RecordedHeraldRuntime,
    RecordingSchedule (ProductionShapedRecording),
    recordedHeraldRuntime,
    snapshotRecordedHeraldRuntime,
    snapshotRecordedRuntimeEvents,
    startRecordedHeraldRuntime,
  )
import Eclips.Herald.Runtime.Internal.Coordination (batchEnvelopeEffects)
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    RuntimeRetentionCounts,
    defaultRuntimeHooks,
    partitionPeerDialCancellation,
    startHeraldRuntime,
    startHeraldRuntimeWithHooks,
  )
import Eclips.Herald.Runtime.Internal.Recording
  ( RuntimeTraceTerminal (RuntimeTraceExited),
    replayRuntimeTrace,
    runtimeTraceEvents,
    runtimeTraceSnapshot,
  )
import Eclips.Herald.Runtime.Internal.Timer (RuntimeTimerScheduler (..))
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    RuntimeTraceEvent (..),
    ShellTraceEvent (..),
    TraceLedger,
    snapshotTraceLedger,
  )
import Eclips.Herald.Runtime.Internal.Types qualified as RuntimeInternal
import Eclips.Herald.Runtime.Trace
  ( HeraldRuntimeExit (HeraldRuntimeDrained),
    HeraldRuntimeFailureClass (HeraldRuntimeEssentialWorkerFailure),
    heraldRuntimeFailureClass,
  )
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Timer (TimerAttempt, TimerOutcome (TimerFired), timerSpecAbsoluteDeadline)
import RuntimeFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedBootstraps,
    fixtureCheckedGenesis,
    fixtureGeneratorSeedSource,
    fixtureMemberSetDigest,
    fixtureMembershipGenerationId,
    fixtureOracleContacts,
    fixturePeerCandidate,
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

tests :: TestTree
tests =
  testGroup
    "runtime peer-dial sink"
    [ testCase "the recording sink receives the exact intent and typed replay reproduces it" caseRecordedExactReplay,
      testCase "an explicit binding retirement detaches silently without ordinary recovery" caseSilentBindingRetirement,
      testCase "an explicit dial cancellation terminates already-handed learned work" caseHandedDialRetirement,
      testCase "the independent sink worker preserves repeated level-intent FIFO order" caseSinkFifo,
      testCase "repeated peer turnover releases workers and loss receipts while stale closes stay inert" caseTurnoverRetention,
      testCase "a missing sink is an essential runtime failure" caseMissingSinkFailure,
      testCase "a throwing sink is an essential runtime failure" caseThrowingSinkFailure,
      testCase "the drain cut suppresses a handed but not-yet-started dial" caseDrainSuppressesHandedDial,
      testCase "drain cancels and joins an outstanding peer-recovery timer" caseDrainCancelsOutstandingRecoveryTimer,
      testCase "one absolute peer-recovery timer is recorded and typed-replays" caseRecoveryTimerRecordedReplay,
      testCase "host timer allocation failure terminates the supervised runtime" caseTimerSchedulerFailure
    ]

caseSilentBindingRetirement :: Assertion
caseSilentBindingRetirement = do
  injectorCell <- newEmptyMVar
  routed <- newEmptyMVar
  delivered <- newIORef []
  let hooks =
        defaultRuntimeHooks
          { hookRuntimeEffectInjector = putOnce injectorCell,
            hookAfterDispatchBatch = \batch ->
              when
                (any isCloseBinding (effectBatchMembers (batchEnvelopeEffects batch)))
                (putOnce routed ())
          }
      sink intent = atomicModifyIORef' delivered (\seen -> (seen <> [intent], ()))
  started <- startHeraldRuntimeWithHooks hooks (recordingConfiguration sink)
  runtime <- either (assertFailure . ("binding-retirement runtime start: " <>) . show) pure started
  (reference, binding) <- establishPeer runtime (peerAddress "tcp://127.0.0.1:5802")
  inject <- awaitMVar "runtime effect injector" injectorCell
  inject (orderedEffectBatch [ClosePeerBinding binding])
  _ <- awaitMVar "silent binding retirement route" routed
  assertEqual "the detached physical lease is no longer current" StalePhysicalGeneration
    =<< closeRuntimeConnection runtime reference
  threadDelay 20_000
  assertEqual "silent retirement emits no ordinary reconnect dial" [] =<< readIORef delivered
  RuntimeInternal.runtimeCloseScope runtime
  where
    isCloseBinding ClosePeerBinding {} = True
    isCloseBinding _ = False

caseHandedDialRetirement :: Assertion
caseHandedDialRetirement = do
  injectorCell <- newEmptyMVar
  handed <- newEmptyMVar
  release <- newEmptyMVar
  routed <- newEmptyMVar
  cancelled <- newEmptyMVar
  delivered <- newIORef []
  let hooks =
        defaultRuntimeHooks
          { hookRuntimeEffectInjector = putOnce injectorCell,
            hookBeforePeerDialSink = \intent -> putOnce handed intent >> takeMVar release,
            hookAfterDispatchBatch = \batch ->
              when
                (any isCancelDial (effectBatchMembers (batchEnvelopeEffects batch)))
                (putOnce routed ())
          }
      sink intent = atomicModifyIORef' delivered (\seen -> (seen <> [intent], ()))
      configuration =
        configureHeraldRuntimePeerDialCancellationSink
          (putOnce cancelled)
          (recordingConfiguration sink)
  started <- startHeraldRuntimeWithHooks hooks configuration
  runtime <- either (assertFailure . ("dial-retirement runtime start: " <>) . show) pure started
  (reference, _) <- establishPeer runtime (peerAddress "tcp://127.0.0.1:5902")
  assertEqual "peer loss creates learned dial work" ConnectionClosed
    =<< closeRuntimeConnection runtime reference
  intent <- awaitMVar "handed learned dial" handed
  let (cancelledQueued, retainedQueued, cancelledHanded) =
        partitionPeerDialCancellation intent [intent] (Just intent)
  assertEqual "exact owner intent cancels its queued work" [intent] cancelledQueued
  assertEqual "exact-key cancellation retains no target work" [] retainedQueued
  assertEqual "exact owner intent cancels its handed work" (Just intent) cancelledHanded
  inject <- awaitMVar "runtime effect injector" injectorCell
  inject (orderedEffectBatch [CancelPeerDial intent])
  _ <- awaitMVar "dial cancellation route" routed
  assertEqual "TCP cancellation receives the exact owner intent" intent
    =<< awaitMVar "TCP dial cancellation" cancelled
  _ <- tryPutMVar release ()
  threadDelay 20_000
  assertEqual "a cancelled handed dial never reaches the dial sink" [] =<< readIORef delivered
  RuntimeInternal.runtimeCloseScope runtime
  where
    isCancelDial CancelPeerDial {} = True
    isCancelDial _ = False

caseRecordedExactReplay :: Assertion
caseRecordedExactReplay = do
  received <- newEmptyMVar
  started <-
    startRecordedHeraldRuntime
      ProductionShapedRecording
      (recordingConfiguration (putOnce received))
  recorded <- either (assertFailure . ("recorded runtime start: " <>) . show) pure started
  let runtime = recordedHeraldRuntime recorded
      address = peerAddress "tcp://127.0.0.1:5102"
  (reference, _) <- establishPeer runtime address
  assertEqual "the current physical peer closes" ConnectionClosed
    =<< closeRuntimeConnection runtime reference
  intent <- awaitMVar "recorded peer dial" received
  assertFixtureIntent "recorded peer dial" (Set.singleton address) intent
  _ <- awaitRecordedTrace "delivered peer dial trace" recorded (isDelivered intent)
  RuntimeInternal.runtimeCloseScope runtime
  trace <- snapshotRecordedHeraldRuntime recorded
  case replayRuntimeTrace fixtureCheckedGenesis fixtureCheckedBootstraps trace of
    Left mismatch -> assertFailure ("typed dial trace replay failed: " <> show mismatch)
    Right _ -> pure ()
  assertBool
    "the exact replayed kernel batch retains the owner-minted intent"
    ( any
        ( \case
            KernelEvent _ (KernelTraceStepped _ _ _ (Right batch)) ->
              DialPeer intent `elem` effectBatchMembers batch
            _ -> False
        )
        (runtimeTraceEvents trace)
    )

caseSinkFifo :: Assertion
caseSinkFifo = do
  delivered <- newIORef []
  available <- newEmptyMVar
  let sink intent = do
        atomicModifyIORef' delivered (\retained -> (retained <> [intent], ()))
        putOnce available ()
      firstAddress = peerAddress "tcp://127.0.0.1:5202"
      secondAddress = peerAddress "tcp://127.0.0.1:5302"
  started <- startHeraldRuntime (recordingConfiguration sink)
  runtime <- either (assertFailure . ("runtime start: " <>) . show) pure started
  (firstReference, _) <- establishPeer runtime firstAddress
  assertEqual "first peer close" ConnectionClosed
    =<< closeRuntimeConnection runtime firstReference
  _ <- awaitMVar "first dial" available

  (secondReference, _) <- establishPeer runtime secondAddress
  assertEqual "second peer close" ConnectionClosed
    =<< closeRuntimeConnection runtime secondReference
  awaitLength "second dial" delivered 2
  intents <- readIORef delivered
  case intents of
    [first, second] -> do
      assertFixtureIntent "first FIFO dial" (Set.singleton firstAddress) first
      assertFixtureIntent "second FIFO dial" (Set.fromList [firstAddress, secondAddress]) second
    _ -> assertFailure ("expected two FIFO dials, observed " <> show intents)
  RuntimeInternal.runtimeCloseScope runtime

caseTurnoverRetention :: Assertion
caseTurnoverRetention = do
  readerCell <- newEmptyMVar :: IO (MVar (IO RuntimeRetentionCounts))
  delivered <- newEmptyMVar
  let hooks = defaultRuntimeHooks {hookRuntimeRetentionReader = putOnce readerCell}
      sink intent = putOnce delivered intent
      address = peerAddress "tcp://127.0.0.1:5303"
  started <- startHeraldRuntimeWithHooks hooks (recordingConfiguration sink)
  runtime <- either (assertFailure . ("turnover runtime start: " <>) . show) pure started
  flip finally (RuntimeInternal.runtimeCloseScope runtime) $ do
    readCounts <- awaitMVar "runtime retention reader" readerCell
    baseline <- readCounts
    forM_ [1 .. 80 :: Int] $ \iteration -> do
      (reference, _) <- establishPeer runtime address
      assertEqual "current peer closes once" ConnectionClosed =<< closeRuntimeConnection runtime reference
      observed <- timeout 2_000_000 (takeMVar delivered)
      case observed of
        Nothing -> assertFailure ("turnover lost its owner-minted dial at " <> show iteration)
        Just _ -> pure ()
      settled <- timeout 2_000_000 (awaitCounts readCounts baseline)
      assertEqual ("only current shell owners remain at turnover " <> show iteration) (Just baseline) settled
      assertEqual "old close stays stale after its loss receipt was released" StalePhysicalGeneration =<< closeRuntimeConnection runtime reference
      assertEqual "stale close cannot recreate retained coordination" baseline =<< readCounts
  where
    awaitCounts readCounts wanted = do
      current <- readCounts
      if current == wanted then pure current else threadDelay 1_000 >> awaitCounts readCounts wanted

caseMissingSinkFailure :: Assertion
caseMissingSinkFailure = assertDialSinkFailure fixtureConfiguration

caseThrowingSinkFailure :: Assertion
caseThrowingSinkFailure =
  assertDialSinkFailure
    ( configureHeraldRuntimePeerDialSink
        (const (ioError (userError "intentional peer dial sink failure")))
        fixtureConfiguration
    )

caseDrainSuppressesHandedDial :: Assertion
caseDrainSuppressesHandedDial = do
  handed <- newEmptyMVar
  release <- newEmptyMVar
  ledgerCell <- newEmptyMVar
  delivered <- newIORef []
  let hooks =
        defaultRuntimeHooks
          { hookRuntimeInitialized = \_ _ _ _ _ _ -> putOnce ledgerCell,
            hookBeforePeerDialSink = \intent -> putOnce handed intent >> takeMVar release
          }
      sink intent = atomicModifyIORef' delivered (\retained -> (retained <> [intent], ()))
  started <- startHeraldRuntimeWithHooks hooks (recordingConfiguration sink)
  runtime <- either (assertFailure . ("gated runtime start: " <>) . show) pure started
  administration <- registerAdministration runtime
  let address = peerAddress "tcp://127.0.0.1:5402"
  (reference, _) <- establishPeer runtime address
  assertEqual "peer close before drain" ConnectionClosed
    =<< closeRuntimeConnection runtime reference
  intent <- awaitMVar "handed dial" handed
  ledger <- awaitMVar "runtime trace ledger" ledgerCell
  assertEqual "drain request enters its owner FIFO" Queued
    =<< requestHeraldRuntimeDrain runtime administration (adminCorrelationId 71)
  _ <- awaitLedgerTrace "semantic drain cut" ledger isDrainCut
  _ <- tryPutMVar release ()
  assertEqual "the runtime drains after suppressing the handed dial" (Right HeraldRuntimeDrained)
    =<< awaitExit "handed dial drain" runtime
  events <- atomically (snapshotTraceLedger ledger)
  assertBool "the cut records exact dial suppression" (any (isSuppressed intent) events)
  assertBool "the suppressed dial is never delivered" (not (any (isDelivered intent) events))
  assertEqual "the configured sink was not invoked" [] =<< readIORef delivered

caseDrainCancelsOutstandingRecoveryTimer :: Assertion
caseDrainCancelsOutstandingRecoveryTimer = do
  now <- atomically (newTVar (monotonicInstant 100))
  elapsed <- atomically newEmptyTMVar
  scheduled <- newEmptyMVar
  ledgerCell <- newEmptyMVar
  let scheduler =
        RuntimeTimerScheduler $ \microseconds ->
          if microseconds == 10
            then putOnce scheduled microseconds >> pure (readTMVar elapsed)
            else pure retry
      hooks =
        defaultRuntimeHooks
          { hookRuntimeInitialized = \_ _ _ _ _ _ -> putOnce ledgerCell,
            hookTimerScheduler = scheduler
          }
      configuration =
        configureHeraldRuntimePeerDialSink
          (const (pure ()))
          ( heraldRuntimeConfiguration
              fixtureCheckedGenesis
              fixtureCheckedBootstraps
              fixtureOracleContacts
              fixtureGeneratorSeedSource
              (runtimeMonotonicClock (atomically (readTVar now)))
              (runtimePeerWorkDelayMicroseconds 0)
              (heraldRuntimeHandlers (const (pure ())))
              fixtureApplicationRecoveryConfiguration
              (checkedRecoveryConfiguration 10)
          )
  started <- startHeraldRuntimeWithHooks hooks configuration
  runtime <- either (assertFailure . ("timer-drain runtime start: " <>) . show) pure started
  administration <- registerAdministration runtime
  (reference, _) <- establishPeer runtime (peerAddress "tcp://127.0.0.1:5552")
  assertEqual "peer loss starts the outstanding timer" ConnectionClosed
    =<< closeRuntimeConnection runtime reference
  assertEqual "the unreleased timer waits for the complete grace" 10
    =<< awaitMVar "outstanding peer-recovery timer schedule" scheduled
  ledger <- awaitMVar "timer-drain trace ledger" ledgerCell
  _ <- awaitLedgerTrace "outstanding peer-recovery timer arm" ledger isTimerArm

  assertEqual "timer-drain request enters the owner FIFO" Queued
    =<< requestHeraldRuntimeDrain runtime administration (adminCorrelationId 73)
  assertEqual "drain joins despite the unreleased timer" (Right HeraldRuntimeDrained)
    =<< awaitExit "outstanding timer drain" runtime

  eventsBeforeRelease <- atomically (snapshotTraceLedger ledger)
  case peerRecoveryTimerAttempts eventsBeforeRelease of
    [attempt] ->
      assertEqual
        "drain records cancellation of the exact armed attempt"
        [attempt]
        [cancelled | ShellEvent _ (ShellTimerCancelled cancelled) <- eventsBeforeRelease, cancelled == attempt]
    attempts -> assertFailure ("expected one armed peer-recovery timer, observed " <> show attempts)
  assertBool
    "the unreleased timer produced no semantic input before drain"
    (not (any isAnyTimerObservation eventsBeforeRelease))

  atomically (putTMVar elapsed ())
  threadDelay 10_000
  eventsAfterRelease <- atomically (snapshotTraceLedger ledger)
  assertEqual "a post-join scheduler release cannot append trace events" eventsBeforeRelease eventsAfterRelease
  assertBool
    "a post-join scheduler release cannot produce a late semantic input"
    (not (any isAnyTimerObservation eventsAfterRelease))

caseRecoveryTimerRecordedReplay :: Assertion
caseRecoveryTimerRecordedReplay = do
  now <- atomically (newTVar (monotonicInstant 100))
  elapsed <- atomically newEmptyTMVar
  scheduled <- newEmptyMVar
  scheduleCalls <- newIORef []
  initialization <- newEmptyMVar
  finalEvidence <- newEmptyMVar
  let recovery = checkedRecoveryConfiguration 10
      scheduler =
        RuntimeTimerScheduler $ \microseconds ->
          if microseconds == 10
            then do
              atomicModifyIORef' scheduleCalls (\calls -> (calls <> [microseconds], ()))
              putOnce scheduled microseconds
              pure (readTMVar elapsed)
            else pure retry
      hooks =
        defaultRuntimeHooks
          { hookRuntimeInitialized = \observed contacts seed applicationRecovery configuredRecovery initialBatch ledger ->
              putOnce
                initialization
                (observed, contacts, seed, applicationRecovery, configuredRecovery, initialBatch, ledger),
            hookPublishFinalEvidence = putOnce finalEvidence,
            hookTimerScheduler = scheduler
          }
      configuration =
        configureHeraldRuntimePeerDialSink
          (const (pure ()))
          ( heraldRuntimeConfiguration
              fixtureCheckedGenesis
              fixtureCheckedBootstraps
              fixtureOracleContacts
              fixtureGeneratorSeedSource
              (runtimeMonotonicClock (atomically (readTVar now)))
              (runtimePeerWorkDelayMicroseconds 0)
              (heraldRuntimeHandlers (const (pure ())))
              fixtureApplicationRecoveryConfiguration
              recovery
          )
  started <- startHeraldRuntimeWithHooks hooks configuration
  runtime <- either (assertFailure . ("timer runtime start: " <>) . show) pure started
  administration <- registerAdministration runtime
  (reference, _) <- establishPeer runtime (peerAddress "tcp://127.0.0.1:5602")
  assertEqual "peer loss starts recovery" ConnectionClosed
    =<< closeRuntimeConnection runtime reference
  assertEqual "the absolute deadline schedules one ten-microsecond wait" 10
    =<< awaitMVar "peer-recovery timer schedule" scheduled
  (_, _, _, _, _, _, ledger) <- awaitMVar "timer runtime initialization" initialization
  armed <- awaitLedgerTrace "peer-recovery timer arm" ledger isTimerArm
  attempt <- case peerRecoveryTimerAttempts armed of
    [value] -> pure value
    values -> assertFailure ("expected one armed peer-recovery timer, observed " <> show values)

  atomically $ do
    writeTVar now (monotonicInstant 110)
    putTMVar elapsed ()
  _ <- awaitLedgerTrace "peer-recovery timer observation" ledger (isTimerObservation attempt)
  assertEqual "idle suspicion does not amplify timer work" [10] =<< readIORef scheduleCalls

  assertEqual "timer runtime drain request" Queued
    =<< requestHeraldRuntimeDrain runtime administration (adminCorrelationId 72)
  assertEqual "timer runtime drains" (Right HeraldRuntimeDrained)
    =<< awaitExit "timer runtime drain" runtime
  (observed, contacts, seed, applicationRecovery, configuredRecovery, initialBatch, _) <-
    awaitMVar "timer runtime initialization" initialization
  evidence <- awaitMVar "timer runtime final evidence" finalEvidence
  events <- atomically (snapshotTraceLedger ledger)
  let trace =
        runtimeTraceSnapshot
          observed
          contacts
          seed
          applicationRecovery
          configuredRecovery
          initialBatch
          events
          (RuntimeTraceExited HeraldRuntimeDrained)
          evidence
  case replayRuntimeTrace fixtureCheckedGenesis fixtureCheckedBootstraps trace of
    Left mismatch -> assertFailure ("typed timer trace replay failed: " <> show mismatch)
    Right _ -> pure ()

checkedRecoveryConfiguration :: Word64 -> PeerRecoveryConfiguration
checkedRecoveryConfiguration microseconds =
  case checkPeerRecoveryConfiguration microseconds of
    Right configuration -> configuration
    Left problem -> error ("invalid peer recovery fixture: " <> show problem)

caseTimerSchedulerFailure :: Assertion
caseTimerSchedulerFailure = do
  let hooks =
        defaultRuntimeHooks
          { hookTimerScheduler =
              RuntimeTimerScheduler
                ( \microseconds ->
                    if microseconds == 10
                      then ioError (userError "intentional peer-recovery host timer failure")
                      else pure retry
                )
          }
      configuration =
        configureHeraldRuntimePeerDialSink (const (pure ())) (fixtureConfigurationWithPeerRecovery (checkedRecoveryConfiguration 10))
  started <- startHeraldRuntimeWithHooks hooks configuration
  runtime <- either (assertFailure . ("timer-failure runtime start: " <>) . show) pure started
  (reference, _) <- establishPeer runtime (peerAddress "tcp://127.0.0.1:5702")
  assertEqual "peer loss requests the failed host timer" ConnectionClosed
    =<< closeRuntimeConnection runtime reference
  outcome <- awaitExit "host timer allocation failure" runtime
  case outcome of
    Left failure ->
      assertEqual
        "host timer failure class"
        HeraldRuntimeEssentialWorkerFailure
        (heraldRuntimeFailureClass failure)
    Right runtimeExit ->
      assertFailure ("host timer allocation failure exited normally: " <> show runtimeExit)

assertDialSinkFailure :: HeraldRuntimeConfiguration -> Assertion
assertDialSinkFailure configuration = do
  started <- startHeraldRuntime configuration
  runtime <- either (assertFailure . ("runtime start: " <>) . show) pure started
  (reference, _) <- establishPeer runtime (peerAddress "tcp://127.0.0.1:5502")
  assertEqual "peer close creates the owner dial" ConnectionClosed
    =<< closeRuntimeConnection runtime reference
  outcome <- awaitExit "dial sink failure" runtime
  case outcome of
    Left failure ->
      assertEqual
        "dial sink failure class"
        HeraldRuntimeEssentialWorkerFailure
        (heraldRuntimeFailureClass failure)
    Right runtimeExit -> assertFailure ("dial sink failure exited normally: " <> show runtimeExit)

establishPeer :: HeraldRuntime -> PeerAddress -> IO (ConnectionRef PeerPlane, PeerBinding)
establishPeer runtime address = do
  accepted <- newEmptyMVar
  registration <-
    registerPeerConnection
      runtime
      ( runtimePeerConnectionHandlers
          (\_ _ -> pure LaneOffered)
          ( \_ disposition -> do
              case disposition of
                PeerHelloAccepted _ binding -> putOnce accepted binding
                _ -> pure ()
              pure LaneOffered
          )
          (\_ _ -> pure LaneOffered)
          (\_ _ -> pure LaneOffered)
          (\_ _ _ -> pure PeerDispatchWritten)
          (pure ())
      )
  reference <- case registration of
    ConnectionRegistered current -> pure current
    RegistrationStopped -> assertFailure "running runtime rejected peer registration"
  let nonce = peerCandidateConnectionNonce fixturePeerCandidate
      base = fixturePeerHello nonce
      hello =
        peerHello
          (peerHelloSystemId base)
          (peerHelloHeraldId base)
          (peerHelloHeraldEpoch base)
          nonce
          (Set.singleton address)
          (peerHelloAppliedControlIndex base)
          (peerHelloCatalogueDigest base)
          (peerHelloInitialProjectionDigest base)
          Nothing
  assertEqual "remote Hello enters the peer FIFO" Queued
    =<< submitPeerHello
      runtime
      reference
      fixturePeerCandidate
      Set.empty
      hello
      fixtureMembershipGenerationId
      fixtureMemberSetDigest
      Nothing
  binding <- awaitMVar "accepted peer binding" accepted
  pure (reference, binding)

registerAdministration :: HeraldRuntime -> IO (ConnectionRef AdministrationPlane)
registerAdministration runtime = do
  registration <-
    registerAdministrationConnection
      runtime
      (runtimeAdministrationConnectionHandlers (const (pure LaneOffered)) (pure ()))
  case registration of
    AdministrationConnectionRegistered reference -> pure reference
    AdministrationRegistrationUnavailable -> assertFailure "administration registration unavailable"
    AdministrationRegistrationStopped -> assertFailure "running runtime rejected administration registration"

fixtureConfiguration :: HeraldRuntimeConfiguration
fixtureConfiguration = fixtureConfigurationWithPeerRecovery fixturePeerRecoveryConfiguration

fixtureConfigurationWithPeerRecovery :: PeerRecoveryConfiguration -> HeraldRuntimeConfiguration
fixtureConfigurationWithPeerRecovery recovery =
  heraldRuntimeConfiguration
    fixtureCheckedGenesis
    fixtureCheckedBootstraps
    fixtureOracleContacts
    fixtureGeneratorSeedSource
    (runtimeMonotonicClock (pure (monotonicInstant 100)))
    (runtimePeerWorkDelayMicroseconds 0)
    (heraldRuntimeHandlers (const (pure ())))
    fixtureApplicationRecoveryConfiguration
    recovery

recordingConfiguration ::
  (PeerDialIntent -> IO ()) ->
  HeraldRuntimeConfiguration
recordingConfiguration sink = configureHeraldRuntimePeerDialSink sink fixtureConfiguration

assertFixtureIntent :: String -> Set.Set PeerAddress -> PeerDialIntent -> Assertion
assertFixtureIntent context addresses intent = do
  let hello = fixturePeerHello (peerCandidateConnectionNonce fixturePeerCandidate)
  assertEqual (context <> " HeraldId") (peerHelloHeraldId hello) (peerDialIntentHeraldId intent)
  assertEqual (context <> " HeraldEpoch") (peerHelloHeraldEpoch hello) (peerDialIntentHeraldEpoch intent)
  assertEqual (context <> " addresses") addresses (peerDialIntentAddresses intent)

awaitMVar :: String -> MVar value -> IO value
awaitMVar context variable = do
  observed <- timeout 2000000 (readMVar variable)
  maybe (assertFailure (context <> " timed out")) pure observed

awaitExit ::
  String ->
  HeraldRuntime ->
  IO (Either RuntimeInternal.HeraldRuntimeFailure RuntimeInternal.HeraldRuntimeExit)
awaitExit context runtime = do
  observed <- timeout 2000000 (awaitHeraldRuntimeExit runtime)
  maybe (assertFailure (context <> " timed out")) pure observed

awaitLength :: String -> IORef [value] -> Int -> Assertion
awaitLength context retained expected = do
  observed <- timeout 2000000 loop
  case observed of
    Nothing -> assertFailure (context <> " timed out")
    Just () -> pure ()
  where
    loop = do
      values <- readIORef retained
      if length values >= expected
        then pure ()
        else threadDelay 1000 >> loop

awaitRecordedTrace ::
  String ->
  RecordedHeraldRuntime ->
  (RuntimeTraceEvent -> Bool) ->
  IO [RuntimeTraceEvent]
awaitRecordedTrace context recorded predicate =
  awaitTrace context (snapshotRecordedRuntimeEvents recorded) predicate

awaitLedgerTrace ::
  String ->
  TraceLedger ->
  (RuntimeTraceEvent -> Bool) ->
  IO [RuntimeTraceEvent]
awaitLedgerTrace context ledger predicate =
  awaitTrace context (atomically (snapshotTraceLedger ledger)) predicate

awaitTrace ::
  String ->
  IO [RuntimeTraceEvent] ->
  (RuntimeTraceEvent -> Bool) ->
  IO [RuntimeTraceEvent]
awaitTrace context snapshot predicate = do
  observed <- timeout 2000000 loop
  maybe (assertFailure (context <> " timed out")) pure observed
  where
    loop = do
      events <- snapshot
      if any predicate events
        then pure events
        else threadDelay 1000 >> loop

isDrainCut :: RuntimeTraceEvent -> Bool
isDrainCut (ShellEvent _ ShellDrainCut {}) = True
isDrainCut _ = False

isDelivered :: PeerDialIntent -> RuntimeTraceEvent -> Bool
isDelivered expected (ShellEvent _ (ShellPeerDialDelivered actual)) = expected == actual
isDelivered _ _ = False

isSuppressed :: PeerDialIntent -> RuntimeTraceEvent -> Bool
isSuppressed expected (ShellEvent _ (ShellPeerDialSuppressed actual)) = expected == actual
isSuppressed _ _ = False

isTimerArm :: RuntimeTraceEvent -> Bool
isTimerArm event = not (null (peerRecoveryTimerAttempts [event]))

-- These fixtures start at 100 and choose a ten-microsecond peer grace. Select
-- that owner-issued absolute specification; startup isolation owns a distinct,
-- longer timer whose presence and cancellation are valid independent work.
peerRecoveryTimerAttempts :: [RuntimeTraceEvent] -> [TimerAttempt]
peerRecoveryTimerAttempts events =
  [ attempt
  | ShellEvent _ (ShellTimerArmed attempt spec) <- events,
    timerSpecAbsoluteDeadline spec == monotonicInstant 110
  ]

isTimerObservation :: TimerAttempt -> RuntimeTraceEvent -> Bool
isTimerObservation expected (KernelEvent _ (KernelTraceStepped _ _ input (Right _))) =
  case inputBody input of
    RuntimeObserved (TimerObserved actual (TimerFired firedAt)) ->
      actual == expected && firedAt == monotonicInstant 110
    _ -> False
isTimerObservation _ _ = False

isAnyTimerObservation :: RuntimeTraceEvent -> Bool
isAnyTimerObservation (KernelEvent _ (KernelTraceStepped _ _ input _)) =
  case inputBody input of
    RuntimeObserved (TimerObserved _ _) -> True
    _ -> False
isAnyTimerObservation _ = False

putOnce :: MVar value -> value -> IO ()
putOnce variable value = do
  _ <- tryPutMVar variable value
  pure ()
