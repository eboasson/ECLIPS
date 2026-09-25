{-# LANGUAGE OverloadedRecordDot #-}

module PeerLivenessProperties
  ( tests,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Word (Word32, Word64)
import Eclips.Domain.Identity (HeraldEpoch, controlIndex)
import Eclips.Domain.Membership
  ( FailureProbeResolution (DismissFailureProbe),
    HeraldFailureProbeId,
    HeraldMembershipGenerationId,
    deriveFailureProbeResolutionId,
    deriveHeraldFailureProbeId,
    heraldMembershipGenerationId,
  )
import Eclips.Herald.Administration
  ( adminCorrelationId,
    checkedInitialAdministrationBinding,
  )
import Eclips.Herald.Discovery
  ( PeerBinding,
    PeerHelloDisposition (PeerHelloAccepted),
    connectionNonce,
    peerCandidate,
    peerHello,
  )
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.FailureDetection.State qualified as FailureDetection
import Eclips.Herald.Genesis
  ( HeraldMember (..),
    checkedInitialProjectionDigest,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedCatalogueDigest,
    checkedSystemId,
  )
import Eclips.Herald.Initialization
  ( HeraldState,
    initialHerald,
    initialHeraldWithIsolation,
    initialHeraldWithOracleVoters,
  )
import Eclips.Herald.Input
  ( AdministrationIngress (OrderlyHeraldShutdown),
    HeraldInputBody (..),
    PeerControl (PeerDirectFailureProbeRequested),
    RuntimeObservation (PeerBindingLost, TimerObserved),
    heraldInput,
  )
import Eclips.Herald.Isolation
  ( checkIsolationConfiguration,
  )
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.OracleClient qualified as Client
import Eclips.Herald.OracleClient.Request qualified as Request
import Eclips.Herald.OracleClient.State qualified as ClientState
import Eclips.Herald.Peer.Step15
  ( DirectFailureProbeResult (DirectFailureProbeReachable),
    directFailureProbeResponse,
  )
import Eclips.Herald.PeerLiveness
  ( PeerRecoveryConfiguration,
    PeerRecoveryConfigurationError (PeerRecoveryGraceZero),
    checkPeerRecoveryConfiguration,
    peerRecoveryGraceMicroseconds,
  )
import Eclips.Herald.PeerLiveness.State qualified as PeerLiveness
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (HeraldStartupInvariant, HeraldTransitionInvariant),
    HeraldTransitionInvariantViolation (HeraldTimerObservationContradiction),
    StartupInvariantSubject (StartupStatic),
    StartupInvariantViolation (PeerLivenessOwnerInvariant),
    validateHeraldState,
  )
import Eclips.Herald.Startup.State
  ( replaceStartupPeerLivenessState,
    startupFailureDetectionState,
    startupIsolationState,
    startupOracleClientState,
    startupPeerLivenessState,
  )
import Eclips.Herald.Time
  ( MonotonicInstant,
    monotonicInstant,
  )
import Eclips.Herald.Timer
  ( TimerAttempt,
    TimerOutcome (TimerFired),
    TimerSpec,
    timerAttemptGeneration,
    timerAttemptGenerationWord64,
    timerSpecAbsoluteDeadline,
  )
import Eclips.Herald.Transition (stepHerald)
import Eclips.Herald.UseCase.FailureDetection qualified as FailureUseCase
import Eclips.Oracle.Canonical (canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Command (oracleEnvelope)
import Eclips.Oracle.Effect (OracleEffect (EmitAppliedOracleEntry), OracleStepOutcome (OracleCommitted), oracleEffects)
import Eclips.Oracle.Genesis (raftVoterBindingHeraldEpoch)
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Projection
  ( OracleProjectionEventView
      ( OracleFailureProbeDismissedView,
        OracleFailureProbeOpenedView
      ),
  )
import Eclips.Oracle.State (OracleState, oracleCurrentMembership)
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import Eclips.Oracle.Voter qualified as Voter
import GenesisFixtures
  ( currentPeerHelloReceived,
    fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureCheckedInitialBootstraps,
    fixtureGeneratorSeed,
    fixtureOracleContacts,
    fixtureRemoteMember,
  )
import GenesisFixtures qualified as Fixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( NonNegative (NonNegative),
    Positive (Positive),
    Property,
    counterexample,
    testProperty,
  )
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "peer recovery liveness"
    [ testProperty "recovery grace is checked, positive, and explicitly microsecond based" propCheckedPositiveGrace,
      testProperty "a repeated loss never renews a current absolute deadline" propNoDeadlineRenewal,
      testCase "before-deadline timer results rearm the same deadline; at and after it suspect" caseDeadlineBoundary,
      testCase "an explicit paced restart replaces recovering and suspected episodes" caseExplicitRecoveryRestart,
      testCase "stale, duplicate, and wrong-generation timer results are exact no-ops" caseStaleTimerResults,
      testCase "current binding loss arms once and exact Hello cancels the episode" caseBindingRecoveryLifecycle,
      testCase "a stale loss from a replaced binding cannot remove voter reachability" caseReplacedBindingLossIsIsolationStale,
      testCase "deadline expiry retains only local suspicion and emits no Oracle action" caseLocalSuspicionOnly,
      testCase "a newly Preparing voter change reconsiders an already suspected old voter" casePreparingReconsidersSuspicion,
      testCase "delayed Dismiss restarts an already suspected post-Open loss episode" caseDelayedDismissRestartsSuspected,
      testCase "Dismiss cancels and replaces a still-recovering post-Open loss episode" caseDismissRestartsRecovering,
      testCase "draining cuts and cancels current peer recovery timers" caseDrainCutsRecovery,
      testCase "cross-owner validation rejects recovery for a currently bound peer" caseBoundRecoveryInvariant,
      testCase "a timer firing cannot postdate its enclosing observation" caseTimerObservationConsistency
    ]

propCheckedPositiveGrace :: Positive Word64 -> Property
propCheckedPositiveGrace (Positive grace) =
  counterexample ("positive grace: " <> show grace)
    $ checkPeerRecoveryConfiguration 0 == Left PeerRecoveryGraceZero
      && fmap peerRecoveryGraceMicroseconds (checkPeerRecoveryConfiguration grace)
        == Right grace

propNoDeadlineRenewal :: Positive Word32 -> NonNegative Word32 -> Property
propNoDeadlineRenewal (Positive rawGrace) (NonNegative rawDelay) =
  counterexample
    ("grace/delay: " <> show (grace, delay))
    ( case PeerLiveness.beginRecovery secondObservation target afterFirst of
        (afterSecond, PeerLiveness.RecoveryAlreadyActive) ->
          afterSecond == afterFirst
            && timerSpecAbsoluteDeadline firstSpec == deadline
        _ -> False
    )
  where
    grace = fromIntegral rawGrace
    delay = fromIntegral rawDelay
    firstObservation = monotonicInstant 100
    secondObservation = monotonicInstant (100 + delay)
    deadline = monotonicInstant (100 + grace)
    initial = PeerLiveness.initialState (checkedRecovery grace)
    (afterFirst, _, firstSpec) = startedRecovery firstObservation initial

caseDeadlineBoundary :: Assertion
caseDeadlineBoundary = do
  let initial = PeerLiveness.initialState (checkedRecovery 10)
      (recovering, firstAttempt, firstSpec) = startedRecovery (monotonicInstant 100) initial
      expectedDeadline = monotonicInstant 110
  assertEqual "the timer spec carries the absolute deadline" expectedDeadline (timerSpecAbsoluteDeadline firstSpec)

  let (afterEarly, earlyDisposition) =
        PeerLiveness.observeTimer firstAttempt (TimerFired (monotonicInstant 109)) recovering
  (secondAttempt, secondSpec) <- case earlyDisposition of
    PeerLiveness.TimerObservationRearmed attempt spec -> pure (attempt, spec)
    other -> assertFailure ("early result did not rearm: " <> show other)
  assertEqual "early firing retains the original absolute deadline" firstSpec secondSpec
  assertEqual "early firing advances the physical attempt generation" 2 (attemptGeneration secondAttempt)
  assertRecovering "early firing" expectedDeadline secondAttempt afterEarly

  let (atState, atAttempt, _) = startedRecovery (monotonicInstant 100) initial
      (afterAt, atDisposition) =
        PeerLiveness.observeTimer atAttempt (TimerFired expectedDeadline) atState
  assertEqual "firing at the deadline expires" PeerLiveness.TimerObservationExpired atDisposition
  assertSuspected "at deadline" expectedDeadline afterAt

  let (afterState, afterAttempt, _) = startedRecovery (monotonicInstant 100) initial
      (afterAfter, afterDisposition) =
        PeerLiveness.observeTimer afterAttempt (TimerFired (monotonicInstant 111)) afterState
  assertEqual "firing after the deadline expires" PeerLiveness.TimerObservationExpired afterDisposition
  assertSuspected "after deadline" expectedDeadline afterAfter

caseExplicitRecoveryRestart :: Assertion
caseExplicitRecoveryRestart = do
  let initial = PeerLiveness.initialState (checkedRecovery 10)
      (firstEpisode, firstAttempt, _) = startedRecovery (monotonicInstant 100) initial
      (secondEpisode, secondDisposition) =
        PeerLiveness.restartRecovery (monotonicInstant 101) target firstEpisode
  (secondAttempt, secondSpec) <- case secondDisposition of
    PeerLiveness.RecoveryRestarted (Just cancelled) attempt specification -> do
      assertEqual "a recovering restart cancels the exact old attempt" firstAttempt cancelled
      pure (attempt, specification)
    other -> assertFailure ("recovering restart did not replace the episode: " <> show other)
  assertBool "a restart allocates a fresh recovery generation" (secondAttempt /= firstAttempt)
  assertEqual
    "the replacement uses a fresh absolute deadline"
    (monotonicInstant 111)
    (timerSpecAbsoluteDeadline secondSpec)
  assertRecovering "restarted recovering episode" (monotonicInstant 111) secondAttempt secondEpisode

  let (suspected, expiryDisposition) =
        PeerLiveness.observeTimer
          secondAttempt
          (TimerFired (monotonicInstant 111))
          secondEpisode
      (thirdEpisode, thirdDisposition) =
        PeerLiveness.restartRecovery (monotonicInstant 112) target suspected
  assertEqual "the replacement attempt first reaches suspicion" PeerLiveness.TimerObservationExpired expiryDisposition
  (thirdAttempt, thirdSpec) <- case thirdDisposition of
    PeerLiveness.RecoveryRestarted Nothing attempt specification -> pure (attempt, specification)
    other -> assertFailure ("suspected restart retained a timer cancellation: " <> show other)
  assertBool "a suspected restart also allocates a fresh generation" (thirdAttempt /= secondAttempt)
  assertEqual
    "the suspected replacement uses the new observation deadline"
    (monotonicInstant 122)
    (timerSpecAbsoluteDeadline thirdSpec)
  assertRecovering "restarted suspected episode" (monotonicInstant 122) thirdAttempt thirdEpisode

caseStaleTimerResults :: Assertion
caseStaleTimerResults = do
  let initial = PeerLiveness.initialState (checkedRecovery 10)
      (firstEpisode, oldAttempt, _) = startedRecovery (monotonicInstant 100) initial
      (cleared, cancellation) = PeerLiveness.clearRecovery target firstEpisode
  assertEqual "clearing returns the current physical attempt" (Just oldAttempt) cancellation
  let (secondEpisode, currentAttempt, _) = startedRecovery (monotonicInstant 200) cleared
      (afterOld, oldDisposition) =
        PeerLiveness.observeTimer oldAttempt (TimerFired (monotonicInstant 210)) secondEpisode
  assertEqual "old generation is stale" PeerLiveness.TimerObservationStale oldDisposition
  assertEqual "old generation cannot mutate the replacement episode" secondEpisode afterOld

  let (suspected, currentDisposition) =
        PeerLiveness.observeTimer currentAttempt (TimerFired (monotonicInstant 210)) secondEpisode
  assertEqual "current generation expires" PeerLiveness.TimerObservationExpired currentDisposition
  let (afterDuplicate, duplicateDisposition) =
        PeerLiveness.observeTimer currentAttempt (TimerFired (monotonicInstant 211)) suspected
  assertEqual "duplicate result is stale" PeerLiveness.TimerObservationStale duplicateDisposition
  assertEqual "duplicate result is an exact no-op" suspected afterDuplicate

caseBindingRecoveryLifecycle :: Assertion
caseBindingRecoveryLifecycle = do
  let initial = initialFixture 10
      (bound, binding, _) = bindRemote 11 11 initial
      (afterLoss, lossEffects) =
        stepBody 20 (RuntimeObserved (PeerBindingLost binding)) bound
  (attempt, spec) <- soleArm "current binding loss" lossEffects
  assertEqual "loss arms the configured absolute deadline" (monotonicInstant 30) (timerSpecAbsoluteDeadline spec)
  assertRecoveringHerald "loss" (monotonicInstant 30) attempt afterLoss

  let (afterRepeated, repeatedEffects) =
        stepBody 21 (RuntimeObserved (PeerBindingLost binding)) afterLoss
  assertEqual "repeated stale binding loss emits no effects" [] (effectBatchMembers repeatedEffects)
  assertEqual
    "repeated loss does not renew or replace the episode"
    (PeerLiveness.stateWitness (startupPeerLivenessState afterLoss))
    (PeerLiveness.stateWitness (startupPeerLivenessState afterRepeated))

  let (reconnected, _, helloEffects) = bindRemote 22 22 afterRepeated
  assertEqual "accepted exact Hello cancels the current timer first" (Just (CancelTimer attempt)) (firstEffect helloEffects)
  assertEqual "accepted exact Hello clears recovery" [] (recoveryWitnesses reconnected)

  let (_, lateEffects) =
        stepBody 30 (RuntimeObserved (TimerObserved attempt (TimerFired (monotonicInstant 30)))) reconnected
  assertEqual "late result from cancelled timer is silent" [] (effectBatchMembers lateEffects)

caseReplacedBindingLossIsIsolationStale :: Assertion
caseReplacedBindingLossIsIsolationStale = do
  let initial = initialIsolationFixture
      (firstBound, replacedBinding, _) = bindRemote 52 11 initial
      (replacementBound, currentBinding, _) = bindRemote 51 12 firstBound
  assertBool "replacement advances the exact logical binding" (replacedBinding /= currentBinding)
  let (afterStaleLoss, effects) =
        stepBody
          20
          (RuntimeObserved (PeerBindingLost replacedBinding))
          replacementBound
      witness = Isolation.stateWitness (startupIsolationState afterStaleLoss)
  assertEqual "the stale physical close is effect-free" [] (effectBatchMembers effects)
  assertEqual
    "even the current EPRP binding supplies no native health evidence"
    Set.empty
    (Isolation.isolationWitnessReachableVoterHosts witness)
  assertEqual
    "stale EPRP loss cannot alter the existing Oracle-health grace"
    Isolation.IsolationVoterQuorumGraceView
    (Isolation.isolationWitnessPhase witness)

caseLocalSuspicionOnly :: Assertion
caseLocalSuspicionOnly = do
  let initial = initialFixture 10
      (bound, binding, _) = bindRemote 25 11 initial
      (recovering, lossEffects) =
        stepBody 20 (RuntimeObserved (PeerBindingLost binding)) bound
  (attempt, _) <- soleArm "loss before local suspicion" lossEffects
  let (suspected, expiryEffects) =
        stepBody
          30
          (RuntimeObserved (TimerObserved attempt (TimerFired (monotonicInstant 30))))
          recovering
  assertEqual "local suspicion emits no runtime or Oracle action" [] (effectBatchMembers expiryEffects)
  assertSuspected "top-level deadline" (monotonicInstant 30) (startupPeerLivenessState suspected)

casePreparingReconsidersSuspicion :: Assertion
casePreparingReconsidersSuspicion = do
  let initial = initialFailureDetectionFixture 10
      attempt = case ClientState.oracleClientActions (startupOracleClientState initial) of
        [Client.ConnectAndHelloOracle value _] -> value
        actions -> error ("expected initial Oracle connection: " <> show actions)
      node = Client.oracleContactNode (Client.oracleConnectAttemptContact attempt)
      (connected, _) = stepBody 1 (OracleInput (Client.OracleHelloReceived attempt (Client.oracleHelloAcceptance node (Client.oracleObservedTerm 1) (controlIndex 0) (Just node) True))) initial
      oracleBinding = case ClientState.oracleClientCurrentBinding (startupOracleClientState connected) of
        Just value -> value
        Nothing -> error "fixture Oracle binding missing"
      (bound, peerBinding, _) = bindRemote 81 11 connected
      (recovering, lossEffects) = stepBody 20 (RuntimeObserved (PeerBindingLost peerBinding)) bound
  (timer, _) <- soleArm "old-voter recovery" lossEffects
  let (suspected, _) = stepBody 30 (RuntimeObserved (TimerObserved timer (TimerFired (monotonicInstant 30)))) recovering
      oldBindings = Voter.voterConfigurationBindings failureVoterConfiguration
      survivors = checked "surviving voter bindings" (Voter.oracleVoterBindings (filter ((/= target) . raftVoterBindingHeraldEpoch) oldBindings))
      local = (FailureDetection.stateWitness (startupFailureDetectionState initial)).witnessLocal
      command = Voter.beginVoterChangeCommand (Voter.voterConfigurationId failureVoterConfiguration) Voter.ExplicitDemotion survivors
      envelope = oracleEnvelope (oracleClientRequestId local 900001) Nothing local command
      (change, canonical) = case checked "begin explicit voter change" (stepOracle envelope failureOracle) of
        (successor, OracleCommitted _, effects) | [entry] <- [entry | EmitAppliedOracleEntry entry <- oracleEffects effects], Just value <- Voter.oraclePendingVoterChange successor -> (value, canonicalizeAppliedOracleEntry entry)
        other -> error ("expected accepted voter preparation: " <> show other)
      ingress = OracleInput (Client.OracleEntriesReceived oracleBinding (canonical NonEmpty.:| []))
      (preparing, _) = stepBody 31 ingress suspected
      cancellations state = [(reference, witness) | (reference, witness) <- ClientState.oracleClientRequestEntries (startupOracleClientState state), Request.FailureVoterCancellationIntent identifier <- [Request.oracleRequestWitnessIntent witness], identifier == Voter.voterChangeId change]
      (replayed, _) = stepBody 32 ingress preparing
  assertSuspected "already terminal recovery remains available" (monotonicInstant 30) (startupPeerLivenessState preparing)
  assertEqual "new preparation retains one cancellation without waiting for another timer" 1 (length (cancellations preparing))
  assertEqual "duplicate preparation preserves exact cancellation identity" (cancellations preparing) (cancellations replayed)
  assertEqual "ordinary failure Open is suspended while cancellation is pending" [] (FailureDetection.stateWitness (startupFailureDetectionState preparing)).witnessOpenIntentions

caseDelayedDismissRestartsSuspected :: Assertion
caseDelayedDismissRestartsSuspected = do
  let initial = initialFailureDetectionFixture 10
      (bound, binding, _) = bindRemote 61 11 initial
  reported <- openAndReportReachable binding bound
  let (recovering, lossEffects) =
        stepBody 20 (RuntimeObserved (PeerBindingLost binding)) reported
  (oldAttempt, _) <- soleArm "post-Open binding loss" lossEffects
  let (suspected, expiryEffects) =
        stepBody
          30
          (RuntimeObserved (TimerObserved oldAttempt (TimerFired (monotonicInstant 30))))
          recovering
  assertEqual
    "the already-open probe consumes the recovery expiry without opening another"
    []
    (effectBatchMembers expiryEffects)
  assertSuspected "pre-Dismiss delayed projection" (monotonicInstant 30) (startupPeerLivenessState suspected)

  let (dismissed, dismissEffects) = projectFixtureDismiss suspected
  (replacementAttempt, replacementSpec) <- soleArmFromMembers "delayed Dismiss" dismissEffects
  assertEqual
    "a terminal suspicion has no physical timer to cancel"
    []
    [attempt | CancelTimer attempt <- dismissEffects]
  assertEqual
    "Dismiss starts a fresh paced episode from its latest observed time"
    (monotonicInstant 40)
    (timerSpecAbsoluteDeadline replacementSpec)
  assertRecoveringHerald "post-Dismiss suspected restart" (monotonicInstant 40) replacementAttempt dismissed
  replacementRecovery <- case recoveryWitnesses dismissed of
    [ PeerLiveness.PeerRecoveryWitness
        observedTarget
        recovery
        _
        (PeerLiveness.PeerRecoveringWitness observedAttempt)
      ]
        | observedTarget == target,
          observedAttempt == replacementAttempt ->
            pure recovery
    other ->
      assertFailure
        ("post-Dismiss recovery generation was not retained: " <> show other)

  let (reopened, _) =
        stepBody
          40
          ( RuntimeObserved
              (TimerObserved replacementAttempt (TimerFired (monotonicInstant 40)))
          )
          dismissed
  assertBool
    "continued loss after Dismiss retains the exact replacement-generation Open"
    ( (target, fixtureMembershipGeneration, replacementRecovery, Voter.voterConfigurationId failureVoterConfiguration)
        `elem` ( FailureDetection.stateWitness
                   (startupFailureDetectionState reopened)
               ).witnessOpenIntentions
    )

caseDismissRestartsRecovering :: Assertion
caseDismissRestartsRecovering = do
  let initial = initialFailureDetectionFixture 10
      (bound, binding, _) = bindRemote 71 11 initial
  reported <- openAndReportReachable binding bound
  let (recovering, lossEffects) =
        stepBody 20 (RuntimeObserved (PeerBindingLost binding)) reported
  (oldAttempt, _) <- soleArm "pre-Dismiss active recovery" lossEffects
  let (dismissed, dismissEffects) = projectFixtureDismiss recovering
  assertEqual
    "Dismiss cancels the still-live old recovery attempt"
    [oldAttempt]
    [attempt | CancelTimer attempt <- dismissEffects]
  (replacementAttempt, replacementSpec) <- soleArmFromMembers "active-recovery Dismiss" dismissEffects
  assertBool "Dismiss replaces rather than renews the old attempt" (replacementAttempt /= oldAttempt)
  assertEqual
    "the replacement is paced from the latest serialized observation"
    (monotonicInstant 30)
    (timerSpecAbsoluteDeadline replacementSpec)
  assertRecoveringHerald "post-Dismiss active restart" (monotonicInstant 30) replacementAttempt dismissed

  let (afterOld, oldEffects) =
        stepBody
          30
          (RuntimeObserved (TimerObserved oldAttempt (TimerFired (monotonicInstant 30))))
          dismissed
  assertEqual "the cancelled predecessor attempt is stale" [] (effectBatchMembers oldEffects)
  assertRecoveringHerald "late predecessor attempt" (monotonicInstant 30) replacementAttempt afterOld

caseDrainCutsRecovery :: Assertion
caseDrainCutsRecovery = do
  let initial = initialFixture 10
      (bound, binding, _) = bindRemote 31 11 initial
      (recovering, lossEffects) =
        stepBody 20 (RuntimeObserved (PeerBindingLost binding)) bound
  (attempt, _) <- soleArm "loss before drain" lossEffects
  let (draining, drainEffects) =
        stepBody
          21
          ( AdministrationInput
              ( OrderlyHeraldShutdown
                  (checkedInitialAdministrationBinding fixtureCheckedGenesis)
                  (adminCorrelationId 1)
              )
          )
          recovering
      members = effectBatchMembers drainEffects
  assertEqual "drain cancels the recovery timer before its barrier effect" (Just (CancelTimer attempt)) (safeHead members)
  assertEqual "drain clears the recovery owner" [] (recoveryWitnesses draining)
  let (afterLate, lateEffects) =
        stepBody 30 (RuntimeObserved (TimerObserved attempt (TimerFired (monotonicInstant 30)))) draining
  assertEqual
    "timer results cannot restore liveness while draining"
    (PeerLiveness.stateWitness (startupPeerLivenessState draining))
    (PeerLiveness.stateWitness (startupPeerLivenessState afterLate))
  assertEqual "timer results emit nothing while draining" [] (effectBatchMembers lateEffects)

caseBoundRecoveryInvariant :: Assertion
caseBoundRecoveryInvariant = do
  let initial = initialFixture 10
      (bound, _, _) = bindRemote 35 11 initial
      (invalidLiveness, _, _) =
        startedRecovery (monotonicInstant 12) (startupPeerLivenessState bound)
      invalid = replaceStartupPeerLivenessState invalidLiveness bound
  assertEqual
    "a bound peer cannot simultaneously own a recovery episode"
    (Left (HeraldStartupInvariant StartupStatic PeerLivenessOwnerInvariant))
    (validateHeraldState invalid)

caseTimerObservationConsistency :: Assertion
caseTimerObservationConsistency = do
  let initial = initialFixture 10
      (bound, binding, _) = bindRemote 41 11 initial
      (recovering, lossEffects) =
        stepBody 20 (RuntimeObserved (PeerBindingLost binding)) bound
  (attempt, _) <- soleArm "loss before inconsistent timer result" lossEffects
  case stepHerald
    ( heraldInput
        (monotonicInstant 29)
        (RuntimeObserved (TimerObserved attempt (TimerFired (monotonicInstant 30))))
    )
    recovering of
    Left (HeraldTransitionInvariant HeraldTimerObservationContradiction) -> pure ()
    Left other -> assertFailure ("unexpected future-firing fault: " <> show other)
    Right _ -> assertFailure "future physical firing was admitted"

initialFixture :: Word64 -> HeraldState
initialFixture grace = case initialHerald
  (monotonicInstant 0)
  fixtureCheckedGenesis
  fixtureCheckedInitialBootstraps
  fixtureOracleContacts
  fixtureGeneratorSeed
  fixtureApplicationRecoveryConfiguration
  (checkedRecovery grace) of
  Left problem -> error ("peer liveness fixture initialization: " <> show problem)
  Right (state, _) -> state

initialIsolationFixture :: HeraldState
initialIsolationFixture =
  case initialHeraldWithIsolation
    (monotonicInstant 0)
    fixtureCheckedGenesis
    fixtureCheckedInitialBootstraps
    fixtureOracleContacts
    fixtureGeneratorSeed
    fixtureApplicationRecoveryConfiguration
    (checkedRecovery 10)
    (Set.singleton (heraldMemberEpoch fixtureRemoteMember))
    (checked "isolation configuration" (checkIsolationConfiguration 10 5)) of
    Left problem -> error ("isolation fixture initialization: " <> show problem)
    Right (state, _) -> state

initialFailureDetectionFixture :: Word64 -> HeraldState
initialFailureDetectionFixture grace =
  case initialHeraldWithOracleVoters
    failureVoterConfiguration
    (Voter.oracleReplicaRegistrations failureOracle)
    (monotonicInstant 0)
    Fixtures.fixtureStep14CheckedGenesis
    fixtureCheckedInitialBootstraps
    fixtureOracleContacts
    fixtureGeneratorSeed
    fixtureApplicationRecoveryConfiguration
    (checkedRecovery grace)
    (Just (checked "failure-detection isolation configuration" (checkIsolationConfiguration 10 5))) of
    Left problem -> error ("failure-detection fixture initialization: " <> show problem)
    Right (state, _) -> state

bindRemote :: Word64 -> Word64 -> HeraldState -> (HeraldState, PeerBinding, EffectBatch)
bindRemote nonceValue observed predecessor =
  case [ binding
       | SetPeerCandidateDisposition _ (PeerHelloAccepted _ binding) <- effectBatchMembers effects
       ] of
    [binding] -> (successor, binding, effects)
    bindings -> error ("expected one admitted remote binding, observed " <> show bindings)
  where
    remote = fixtureRemoteMember
    nonce = connectionNonce nonceValue
    candidate = peerCandidate (heraldMemberId remote) (heraldMemberEpoch remote) nonce
    hello =
      peerHello
        (checkedSystemId fixtureCheckedGenesis)
        (heraldMemberId remote)
        (heraldMemberEpoch remote)
        nonce
        Set.empty
        (controlIndex 0)
        (checkedCatalogueDigest fixtureCheckedGenesis)
        (checkedInitialProjectionDigest fixtureCheckedInitialBootstraps)
        Nothing
    (successor, effects) =
      stepBody
        observed
        (PeerInput (currentPeerHelloReceived predecessor candidate Set.empty hello))
        predecessor

checkedRecovery :: Word64 -> PeerRecoveryConfiguration
checkedRecovery grace = case checkPeerRecoveryConfiguration grace of
  Left problem -> error ("checked peer recovery grace: " <> show problem)
  Right configuration -> configuration

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

openAndReportReachable :: PeerBinding -> HeraldState -> IO HeraldState
openAndReportReachable binding predecessor = do
  let (opened, openEffects) =
        checked
          "project fixture failure-probe Open"
          ( FailureUseCase.applyFailureProjectionEvent
              ( OracleFailureProbeOpenedView
                  fixtureFailureProbe
                  target
                  fixtureMembershipGeneration
                  failureVoterConfiguration
              )
              predecessor
          )
  request <- case [ observedRequest
                  | SendPeerControl observedBinding (PeerDirectFailureProbeRequested observedRequest) <- openEffects,
                    observedBinding == binding
                  ] of
    [observed] -> pure observed
    other -> assertFailure ("expected one direct Reachable request, observed " <> show other)
  let (reported, responseEffects) =
        checked
          "apply fixture direct Reachable response"
          ( FailureUseCase.applyDirectFailureProbeResponse
              binding
              (directFailureProbeResponse request DirectFailureProbeReachable)
              opened
          )
  assertBool
    "the Reachable result consumes its bounded direct-probe timer"
    (any isTimerCancellation (effectBatchMembers responseEffects))
  pure reported

projectFixtureDismiss :: HeraldState -> (HeraldState, [HeraldEffect])
projectFixtureDismiss =
  checked "project fixture failure-probe Dismiss"
    . FailureUseCase.applyFailureProjectionEvent
      ( OracleFailureProbeDismissedView
          fixtureFailureProbe
          (deriveFailureProbeResolutionId fixtureFailureProbe DismissFailureProbe)
      )

fixtureMembershipGeneration :: HeraldMembershipGenerationId
fixtureMembershipGeneration =
  heraldMembershipGenerationId (oracleCurrentMembership failureOracle)

fixtureFailureProbe :: HeraldFailureProbeId
fixtureFailureProbe =
  checked "fixture failure probe" (deriveHeraldFailureProbeId (controlIndex 1))

isTimerCancellation :: HeraldEffect -> Bool
isTimerCancellation (CancelTimer _) = True
isTimerCancellation _ = False

target :: HeraldEpoch
target = heraldMemberEpoch fixtureRemoteMember

startedRecovery ::
  MonotonicInstant ->
  PeerLiveness.State ->
  (PeerLiveness.State, TimerAttempt, TimerSpec)
startedRecovery observed state = case PeerLiveness.beginRecovery observed target state of
  (successor, PeerLiveness.RecoveryStarted attempt spec) -> (successor, attempt, spec)
  (_, disposition) -> error ("expected a fresh recovery, observed " <> show disposition)

assertRecovering :: String -> MonotonicInstant -> TimerAttempt -> PeerLiveness.State -> Assertion
assertRecovering context deadline attempt state =
  case recoveryWitnessesIn state of
    [PeerLiveness.PeerRecoveryWitness observedTarget _ observedDeadline (PeerLiveness.PeerRecoveringWitness observedAttempt)] -> do
      assertEqual (context <> ": target") target observedTarget
      assertEqual (context <> ": deadline") deadline observedDeadline
      assertEqual (context <> ": timer attempt") attempt observedAttempt
    witnesses -> assertFailure (context <> ": unexpected recovery witness: " <> show witnesses)

assertSuspected :: String -> MonotonicInstant -> PeerLiveness.State -> Assertion
assertSuspected context deadline state =
  case recoveryWitnessesIn state of
    [PeerLiveness.PeerRecoveryWitness observedTarget _ observedDeadline PeerLiveness.PeerSuspectedWitness] -> do
      assertEqual (context <> ": target") target observedTarget
      assertEqual (context <> ": deadline") deadline observedDeadline
    witnesses -> assertFailure (context <> ": unexpected suspicion witness: " <> show witnesses)

assertRecoveringHerald :: String -> MonotonicInstant -> TimerAttempt -> HeraldState -> Assertion
assertRecoveringHerald context deadline attempt =
  assertRecovering context deadline attempt . startupPeerLivenessState

recoveryWitnesses :: HeraldState -> [PeerLiveness.PeerRecoveryWitness]
recoveryWitnesses = recoveryWitnessesIn . startupPeerLivenessState

recoveryWitnessesIn :: PeerLiveness.State -> [PeerLiveness.PeerRecoveryWitness]
recoveryWitnessesIn state = case PeerLiveness.stateWitness state of
  PeerLiveness.StateWitness _ witnesses -> witnesses

soleArm :: String -> EffectBatch -> IO (TimerAttempt, TimerSpec)
soleArm context effects = case [(attempt, spec) | ArmTimer attempt spec <- effectBatchMembers effects] of
  [armed] -> pure armed
  observed -> assertFailure (context <> ": expected one timer arm, observed " <> show observed)

soleArmFromMembers :: String -> [HeraldEffect] -> IO (TimerAttempt, TimerSpec)
soleArmFromMembers context effects = case [(attempt, spec) | ArmTimer attempt spec <- effects] of
  [armed] -> pure armed
  observed -> assertFailure (context <> ": expected one timer arm, observed " <> show observed)

firstEffect :: EffectBatch -> Maybe HeraldEffect
firstEffect = safeHead . effectBatchMembers

safeHead :: [value] -> Maybe value
safeHead [] = Nothing
safeHead (value : _) = Just value

attemptGeneration :: TimerAttempt -> Word64
attemptGeneration = timerAttemptGenerationWord64 . timerAttemptGeneration

stepBody :: Word64 -> HeraldInputBody -> HeraldState -> (HeraldState, EffectBatch)
stepBody observed body predecessor = case verifiedStepHerald (heraldInput (monotonicInstant observed) body) predecessor of
  Left problem -> error ("peer liveness fixture step: " <> show problem)
  Right result -> result

failureOracle :: OracleState
failureOracle = checked "failure Oracle genesis" (initialOracle Fixtures.fixtureCheckedOracleGenesis)
failureVoterConfiguration :: Voter.VoterConfiguration
failureVoterConfiguration = Voter.oracleVoterConfiguration failureOracle
