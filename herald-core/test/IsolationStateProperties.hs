{-# LANGUAGE OverloadedRecordDot #-}

module IsolationStateProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( HeraldEpoch,
    ProcessEpochId,
    mkHeraldEpoch,
    mkProcessEpochId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGenerationId,
    mkHeraldMembershipGenerationId,
  )
import Eclips.Herald.Isolation
  ( IsolationConfiguration,
    IsolationConfigurationError (..),
    IsolationDrainId,
    checkIsolationConfiguration,
    isolationDrainIdWord64,
    isolationDrainMicroseconds,
    isolationGraceGenerationWord64,
    isolationGraceMicroseconds,
  )
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.Time (MonotonicInstant, monotonicInstant)
import Eclips.Herald.Timer
  ( TimerAttempt,
    TimerOutcome (TimerFired),
    TimerSpec,
    timerAttemptGeneration,
    timerAttemptGenerationWord64,
    timerSpecAbsoluteDeadline,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Positive (Positive),
    Property,
    counterexample,
    forAll,
    property,
    sublistOf,
    testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "irreversible local Herald isolation"
    [ testProperty "isolation grace and drain durations are checked positive microseconds" propCheckedConfiguration,
      testCase "a non-voter below voter quorum arms immediately at startup" caseInitialNonVoterGrace,
      testCase "a voter-host Herald arms and self-fences without fresh native quorum" caseVoterHostDisabled,
      testProperty "joint reachability requires both majorities for every reached subset" propJointReachability,
      testCase "role changes preserve an existing absolute non-voter grace" caseRoleGracePreserved,
      testCase "promotion cancels grace and demotion cannot reuse its timer" casePromotionDemotion,
      testCase "demotion uses current bindings without interrupting a healthy Herald" caseHealthyDemotion,
      testCase "later role projection cannot reopen a fenced epoch" caseFencedRoleProjection,
      testProperty "the first two of three voter bindings reach quorum independent of order" propBindingOrderIndependent,
      testCase "quorum recovery cancels and later quorum loss starts one non-renewable grace" caseReachabilityLifecycle,
      testCase "early, stale, and terminal grace observations retain exact timer identity" caseGraceTimerLifecycle,
      testCase "one checked current-generation retired rejection prepares an immediate fence" caseExplicitRetiredRejection,
      testCase "locally learned retirement supersedes the active grace through the same fence" caseLocallyLearnedRetirement,
      testCase "the bounded read-only drain preserves its overlay and ends irreversibly" caseReadOnlyDrainLifecycle,
      testCase "self-fence without a connected application lane is terminal immediately" caseImmediateTerminalFence
    ]

propCheckedConfiguration :: Positive Word64 -> Positive Word64 -> Property
propCheckedConfiguration (Positive grace) (Positive drain) =
  counterexample ("durations: " <> show (grace, drain))
    $ checkIsolationConfiguration 0 drain == Left IsolationGraceZero
      && checkIsolationConfiguration grace 0 == Left IsolationDrainZero
      && fmap isolationGraceMicroseconds checkedConfiguration == Right grace
      && fmap isolationDrainMicroseconds checkedConfiguration == Right drain
  where
    checkedConfiguration = checkIsolationConfiguration grace drain

caseInitialNonVoterGrace :: Assertion
caseInitialNonVoterGrace = do
  assertEqual
    "an empty voter-host set is rejected"
    (Left Isolation.IsolationVoterHostSetEmpty)
    ( Isolation.initialState
        (monotonicInstant 100)
        h4
        Set.empty
        membership
        configuration
    )
  (state, attempt, spec) <- initialNonVoter (monotonicInstant 100)
  let witness = Isolation.stateWitness state
  assertEqual "three voters require a strict majority of two" 2 (Isolation.isolationWitnessRequiredVoterCount witness)
  assertEqual "H4 starts with no established voter" Set.empty (Isolation.isolationWitnessReachableVoterHosts witness)
  assertEqual "startup begins the absolute grace" Isolation.IsolationVoterQuorumGraceView (Isolation.isolationWitnessPhase witness)
  assertEqual "the first grace starts at generation one" (Just 1) (isolationGraceGenerationWord64 <$> Isolation.isolationWitnessLastGraceGeneration witness)
  assertEqual "the absolute deadline is fixed from startup" (monotonicInstant 110) (timerSpecAbsoluteDeadline spec)
  assertEqual "the witness retains the exact timer" (Just attempt) (Isolation.isolationWitnessCurrentTimer witness)
  assertValidated "initial non-voter" state

caseVoterHostDisabled :: Assertion
caseVoterHostDisabled = do
  (state, attempt) <- case Isolation.initialState (monotonicInstant 100) h1 voterHosts membership configuration of
    Right (successor, Isolation.InitialIsolationGraceArmed timer _) -> pure (successor, timer)
    other -> assertFailure ("voter initialization did not arm isolation: " <> show other)
  case snd (Isolation.observeGraceTimer attempt (TimerFired (monotonicInstant 110)) state) of
    Isolation.IsolationGraceTimerSelfFenceReady _ -> pure ()
    other -> assertFailure ("voter-host grace did not prepare its semantic fence: " <> show other)
  assertBool "a current checked rejection can fence a voter-host Herald" (Isolation.prepareRetiredEpochRejection (monotonicInstant 101) h2 membership state /= Nothing)
  assertValidated "voter-host awaiting fresh quorum" state

propBindingOrderIndependent :: Bool -> Property
propBindingOrderIndependent reverseOrder =
  counterexample ("reverse order: " <> show reverseOrder) $ case initialFixture of
    Left problem -> counterexample (show problem) False
    Right (initial, Isolation.InitialIsolationGraceArmed initialAttempt _) ->
      let (first, second) = if reverseOrder then (h2, h1) else (h1, h2)
          (afterFirst, firstDisposition) = Isolation.observeVoterBound first initial
          (afterSecond, secondDisposition) = Isolation.observeVoterBound second afterFirst
          witness = Isolation.stateWitness afterSecond
       in property
            ( firstDisposition == Isolation.IsolationReachabilityUnchanged
                && secondDisposition == Isolation.IsolationGraceCancelled initialAttempt
                && Isolation.isolationWitnessPhase witness == Isolation.IsolationVoterQuorumHealthyView
                && Isolation.isolationWitnessReachableVoterHosts witness == Set.fromList [h1, h2]
                && Isolation.validateState afterSecond == Right ()
            )

caseReachabilityLifecycle :: Assertion
caseReachabilityLifecycle = do
  (initial, initialAttempt, _) <- initialNonVoter (monotonicInstant 0)
  let (oneBound, firstDisposition) = Isolation.observeVoterBound h1 initial
      (healthy, secondDisposition) = Isolation.observeVoterBound h2 oneBound
  assertEqual "one voter is below quorum" Isolation.IsolationReachabilityUnchanged firstDisposition
  assertEqual "the second voter cancels the original grace" (Isolation.IsolationGraceCancelled initialAttempt) secondDisposition
  assertEqual "the branch is healthy at two of three" Isolation.IsolationVoterQuorumHealthyView (phase healthy)

  let (recovering, lossDisposition) = Isolation.observeVoterLost (monotonicInstant 50) h1 healthy
  (recoveryAttempt, recoverySpec) <- case lossDisposition of
    Isolation.IsolationGraceStarted attempt spec -> pure (attempt, spec)
    other -> assertFailure ("quorum loss did not start recovery: " <> show other)
  assertEqual "later quorum loss allocates generation two" (Just 2) (lastGrace recovering)
  assertEqual "later grace has one absolute deadline" (monotonicInstant 60) (timerSpecAbsoluteDeadline recoverySpec)

  let (duplicateLoss, duplicateDisposition) = Isolation.observeVoterLost (monotonicInstant 59) h1 recovering
      (furtherLoss, furtherDisposition) = Isolation.observeVoterLost (monotonicInstant 59) h2 duplicateLoss
  assertEqual "duplicate loss is inert" Isolation.IsolationReachabilityUnchanged duplicateDisposition
  assertEqual "another loss cannot renew the current episode" Isolation.IsolationReachabilityUnchanged furtherDisposition
  assertEqual "the same exact timer remains current" (Just recoveryAttempt) (Isolation.isolationWitnessCurrentTimer (Isolation.stateWitness furtherLoss))
  assertEqual "the deadline did not move" (Just (monotonicInstant 60)) (Isolation.isolationWitnessCurrentDeadline (Isolation.stateWitness furtherLoss))
  assertValidated "non-renewed grace" furtherLoss

caseGraceTimerLifecycle :: Assertion
caseGraceTimerLifecycle = do
  (initial, firstAttempt, firstSpec) <- initialNonVoter (monotonicInstant 100)
  let (afterEarly, earlyDisposition) =
        Isolation.observeGraceTimer firstAttempt (TimerFired (monotonicInstant 109)) initial
  (secondAttempt, secondSpec) <- case earlyDisposition of
    Isolation.IsolationGraceTimerRearmed attempt spec -> pure (attempt, spec)
    other -> assertFailure ("early grace timer did not rearm: " <> show other)
  assertEqual "early delivery retains the deadline" firstSpec secondSpec
  assertEqual "early delivery advances only the physical attempt" 2 (attemptGeneration secondAttempt)

  let (afterOld, oldDisposition) =
        Isolation.observeGraceTimer firstAttempt (TimerFired (monotonicInstant 110)) afterEarly
  assertEqual "the consumed attempt is stale" Isolation.IsolationGraceTimerStale oldDisposition
  assertEqual "a stale timer cannot mutate state" afterEarly afterOld

  let (readyState, readyDisposition) =
        Isolation.observeGraceTimer secondAttempt (TimerFired (monotonicInstant 110)) afterEarly
  prepared <- case readyDisposition of
    Isolation.IsolationGraceTimerSelfFenceReady value -> pure value
    other -> assertFailure ("deadline did not prepare a self-fence: " <> show other)
  assertEqual "preparation alone does not expose a half-cut" afterEarly readyState
  assertEqual "timer expiry needs no cancellation" Nothing (Isolation.preparedSelfFenceTimerCancellation prepared)
  assertEqual "expiry has the closed reason" Isolation.IsolationQuorumGraceExpired (Isolation.preparedSelfFenceReason prepared)

  let residents = Set.fromList [process1, process2]
      (draining, disposition) =
        Isolation.commitSelfFence residents Isolation.IsolationDrainRequired prepared
  (drainAttempt, drainSpec) <- case disposition of
    Isolation.IsolationReadOnlyDrainStarted drain attempt spec cancellation reason -> do
      assertEqual "the first drain identity is one" 1 (isolationDrainIdWord64 drain)
      assertEqual "the consumed grace timer is not canceled" Nothing cancellation
      assertEqual "the fence reason survives" Isolation.IsolationQuorumGraceExpired reason
      pure (attempt, spec)
    other -> assertFailure ("self-fence did not start a drain: " <> show other)
  assertEqual "drain deadline uses its independent duration" (monotonicInstant 115) (timerSpecAbsoluteDeadline drainSpec)
  assertEqual "drain attempt starts at physical generation one" 1 (attemptGeneration drainAttempt)
  assertEqual "resident overlay is retained exactly" residents (Isolation.isolationWitnessResidentProcessOverlay (Isolation.stateWitness draining))
  assertValidated "committed self-fence" draining

caseExplicitRetiredRejection :: Assertion
caseExplicitRetiredRejection = do
  (initial, graceAttempt, _) <- initialNonVoter (monotonicInstant 0)
  assertEqual
    "an unrelated reporter is ignored"
    Nothing
    (Isolation.prepareRetiredEpochRejection (monotonicInstant 2) h4 membership initial)
  assertEqual
    "a stale membership generation is ignored"
    Nothing
    (Isolation.prepareRetiredEpochRejection (monotonicInstant 2) h1 otherMembership initial)
  prepared <- case Isolation.prepareRetiredEpochRejection (monotonicInstant 2) h1 membership initial of
    Nothing -> assertFailure "checked rejection did not prepare the self-fence"
    Just value -> pure value
  assertEqual "the exact active grace is canceled" (Just graceAttempt) (Isolation.preparedSelfFenceTimerCancellation prepared)
  assertEqual
    "the reporter and rejected generation are retained"
    (Isolation.IsolationRetiredEpochRejected h1 membership)
    (Isolation.preparedSelfFenceReason prepared)
  let (terminal, disposition) =
        Isolation.commitSelfFence
          (Set.singleton process1)
          Isolation.IsolationDrainNotRequired
          prepared
  case disposition of
    Isolation.IsolationTerminatedWithoutDrain drain cancellation reason -> do
      assertEqual "the terminal fence still has a correlation" 1 (isolationDrainIdWord64 drain)
      assertEqual "the coordinator receives the grace cancellation" (Just graceAttempt) cancellation
      assertEqual "the terminal reason is exact" (Isolation.IsolationRetiredEpochRejected h1 membership) reason
    other -> assertFailure ("no-lane fence did not become terminal: " <> show other)
  assertEqual "the explicit fence is terminal" Isolation.IsolationTerminalView (phase terminal)
  assertValidated "explicit terminal fence" terminal

caseLocallyLearnedRetirement :: Assertion
caseLocallyLearnedRetirement = do
  (initial, graceAttempt, _) <- initialNonVoter (monotonicInstant 0)
  prepared <- case Isolation.prepareLocallyLearnedRetirement
    (monotonicInstant 2)
    otherMembership
    initial of
    Nothing -> assertFailure "local retirement did not prepare the self-fence"
    Just value -> pure value
  assertEqual
    "local retirement cancels the exact active isolation grace"
    (Just graceAttempt)
    (Isolation.preparedSelfFenceTimerCancellation prepared)
  assertEqual
    "the successor membership is the terminal reason"
    (Isolation.IsolationRetirementLearned otherMembership)
    (Isolation.preparedSelfFenceReason prepared)
  let (draining, disposition) =
        Isolation.commitSelfFence
          (Set.singleton process1)
          Isolation.IsolationDrainRequired
          prepared
  case disposition of
    Isolation.IsolationReadOnlyDrainStarted _ _ _ (Just cancelled) reason -> do
      assertEqual "the drain owns cancellation of the old grace" graceAttempt cancelled
      assertEqual "the drain retains the retirement reason" (Isolation.IsolationRetirementLearned otherMembership) reason
    other -> assertFailure ("local retirement did not start the shared drain: " <> show other)
  let (afterLateGrace, lateGraceDisposition) =
        Isolation.observeGraceTimer
          graceAttempt
          (TimerFired (monotonicInstant 10))
          draining
  assertEqual "the displaced grace timer is stale" Isolation.IsolationGraceTimerStale lateGraceDisposition
  assertEqual "the old timer cannot revive the locally retired Herald" draining afterLateGrace
  assertEqual
    "the isolation owner advances to the learned successor generation"
    otherMembership
    ( Isolation.isolationWitnessMembershipGeneration
        (Isolation.stateWitness draining)
    )
  assertValidated "locally learned retirement drain" draining

caseReadOnlyDrainLifecycle :: Assertion
caseReadOnlyDrainLifecycle = do
  (initial, graceAttempt, _) <- initialNonVoter (monotonicInstant 100)
  let (_, readyDisposition) =
        Isolation.observeGraceTimer graceAttempt (TimerFired (monotonicInstant 110)) initial
  prepared <- readyFence readyDisposition
  let residents = Set.fromList [process1, process2]
      (draining, startDisposition) =
        Isolation.commitSelfFence residents Isolation.IsolationDrainRequired prepared
  (drain, firstAttempt, firstSpec) <- startedDrain startDisposition

  let (graceFamilyState, graceFamilyDisposition) =
        Isolation.observeGraceTimer firstAttempt (TimerFired (monotonicInstant 110)) initial
      (drainFamilyState, drainFamilyDisposition) =
        Isolation.observeDrainTimer graceAttempt (TimerFired (monotonicInstant 115)) draining
  assertEqual "a drain attempt cannot enter the grace family" Isolation.IsolationGraceTimerStale graceFamilyDisposition
  assertEqual "cross-family grace observation is inert" initial graceFamilyState
  assertEqual "a grace attempt cannot enter the drain family" Isolation.IsolationDrainTimerStale drainFamilyDisposition
  assertEqual "cross-family drain observation is inert" draining drainFamilyState

  let (afterReconnect, reconnectDisposition) = Isolation.observeVoterBound h1 draining
  assertEqual "post-fence connectivity is inert" Isolation.IsolationReachabilityUnchanged reconnectDisposition
  assertEqual "post-fence connectivity cannot reactivate" draining afterReconnect

  let (afterEarly, earlyDisposition) =
        Isolation.observeDrainTimer firstAttempt (TimerFired (monotonicInstant 114)) draining
  (secondAttempt, secondSpec) <- case earlyDisposition of
    Isolation.IsolationDrainTimerRearmed attempt spec -> pure (attempt, spec)
    other -> assertFailure ("early drain timer did not rearm: " <> show other)
  assertEqual "drain rearm keeps its absolute deadline" firstSpec secondSpec
  assertEqual "drain rearm advances physical generation" 2 (attemptGeneration secondAttempt)

  let (expired, expiryDisposition) =
        Isolation.observeDrainTimer secondAttempt (TimerFired (monotonicInstant 115)) afterEarly
  assertEqual "the exact deadline terminally expires the drain" (Isolation.IsolationDrainTimerExpired drain) expiryDisposition
  assertEqual "timer expiry is terminal" Isolation.IsolationTerminalView (phase expired)
  assertEqual "timer expiry retains the zombie overlay" residents (Isolation.isolationWitnessResidentProcessOverlay (Isolation.stateWitness expired))
  assertValidated "expired read-only drain" expired

  let (terminal, completionDisposition) = Isolation.completeReadOnlyDrain drain afterEarly
  assertEqual "early client close returns the exact current timer" (Isolation.IsolationDrainCompleted drain secondAttempt) completionDisposition
  assertEqual "the drain becomes terminal" Isolation.IsolationTerminalView (phase terminal)
  assertEqual "the terminal state retains the zombie overlay" residents (Isolation.isolationWitnessResidentProcessOverlay (Isolation.stateWitness terminal))

  let (afterLate, lateDisposition) =
        Isolation.observeDrainTimer secondAttempt (TimerFired (monotonicInstant 115)) terminal
  assertEqual "the canceled drain timer is stale" Isolation.IsolationDrainTimerStale lateDisposition
  assertEqual "terminal isolation is irreversible" terminal afterLate
  assertValidated "completed read-only drain" terminal

caseImmediateTerminalFence :: Assertion
caseImmediateTerminalFence = do
  (initial, attempt, _) <- initialNonVoter (monotonicInstant 20)
  let (_, readyDisposition) =
        Isolation.observeGraceTimer attempt (TimerFired (monotonicInstant 30)) initial
  prepared <- readyFence readyDisposition
  let (terminal, disposition) =
        Isolation.commitSelfFence Set.empty Isolation.IsolationDrainNotRequired prepared
  case disposition of
    Isolation.IsolationTerminatedWithoutDrain drain Nothing Isolation.IsolationQuorumGraceExpired ->
      assertEqual "the first terminal cut has drain identity one" 1 (isolationDrainIdWord64 drain)
    other -> assertFailure ("unexpected immediate-terminal disposition: " <> show other)
  assertEqual "no timer survives immediate terminalization" Nothing (Isolation.isolationWitnessCurrentTimer (Isolation.stateWitness terminal))
  assertEqual "terminal reconnect stays inert" (terminal, Isolation.IsolationReachabilityUnchanged) (Isolation.observeVoterBound h1 terminal)
  assertValidated "immediate terminal fence" terminal

propJointReachability :: Property
propJointReachability =
  forAll (sublistOf [h1, h2, h3, h5, h6]) $ \reachableList ->
    let oldHosts = Set.fromList [h1, h2, h3]
        newHosts = Set.fromList [h1, h5, h6]
        reachable = Set.fromList reachableList
        expectedHealthy =
          Set.size (Set.intersection reachable oldHosts) >= 2
            && Set.size (Set.intersection reachable newHosts) >= 2
        expectedPhase =
          if expectedHealthy
            then Isolation.IsolationVoterQuorumHealthyView
            else Isolation.IsolationVoterQuorumGraceView
        (initial, _) = checked "initial isolation" initialFixture
        (updated, _) =
          checked
            "joint roles"
            (Isolation.observeVoterConfiguration (monotonicInstant 3) oldHosts (Just newHosts) reachable initial)
        (unbound, _) =
          checked
            "joint roles without bindings"
            (Isolation.observeVoterConfiguration (monotonicInstant 3) oldHosts (Just newHosts) Set.empty initial)
        incrementallyBound = foldl (\state voter -> fst (Isolation.observeVoterBound voter state)) unbound reachableList
        allBound = foldl (\state voter -> fst (Isolation.observeVoterBound voter state)) unbound [h1, h2, h3, h5, h6]
        incrementallyLost =
          foldl
            (\state voter -> fst (Isolation.observeVoterLost (monotonicInstant 4) voter state))
            allBound
            (Set.toAscList (Set.difference (Set.union oldHosts newHosts) reachable))
     in counterexample
          (show (reachableList, Isolation.stateWitness updated))
          ( phase updated == expectedPhase
              && phase incrementallyBound == expectedPhase
              && phase incrementallyLost == expectedPhase
              && all ((== Right ()) . Isolation.validateState) [updated, incrementallyBound, incrementallyLost]
              && Isolation.isolationWitnessVoterQuorums (Isolation.stateWitness updated) == (oldHosts, Just newHosts)
          )

caseRoleGracePreserved :: Assertion
caseRoleGracePreserved = do
  (initial, timer, spec) <- initialNonVoter (monotonicInstant 100)
  let newHosts = Set.fromList [h1, h5, h6]
      (joint, disposition) =
        checked
          "joint roles"
          (Isolation.observeVoterConfiguration (monotonicInstant 109) voterHosts (Just newHosts) voterHosts initial)
      witness = Isolation.stateWitness joint
  assertEqual "a union majority alone cannot cancel grace" Isolation.IsolationReachabilityUnchanged disposition
  assertEqual "the existing grace remains exact" (Just timer) (Isolation.isolationWitnessCurrentTimer witness)
  assertEqual "configuration learning cannot extend its deadline" (Just (timerSpecAbsoluteDeadline spec)) (Isolation.isolationWitnessCurrentDeadline witness)
  let (stable, _) =
        checked
          "new stable roles"
          (Isolation.observeVoterConfiguration (monotonicInstant 109) newHosts Nothing (Set.singleton h1) joint)
  assertEqual "final commitment still cannot extend grace" (Just (monotonicInstant 110)) (Isolation.isolationWitnessCurrentDeadline (Isolation.stateWitness stable))
  assertValidated "joint non-voter grace" joint
  assertValidated "stable non-voter grace" stable

casePromotionDemotion :: Assertion
casePromotionDemotion = do
  (initial, oldTimer, _) <- initialNonVoter (monotonicInstant 0)
  let promotedHosts = Set.insert h4 voterHosts
      (promoted, promotion) =
        checked
          "promotion"
          (Isolation.observeVoterConfiguration (monotonicInstant 3) voterHosts (Just promotedHosts) voterHosts initial)
      (demoted, demotion) =
        checked
          "demotion"
          (Isolation.observeVoterConfiguration (monotonicInstant 20) voterHosts Nothing Set.empty promoted)
  assertEqual "promotion cancels the exact non-voter grace" (Isolation.IsolationGraceCancelled oldTimer) promotion
  assertEqual "both fresh joint majorities confirm a promoted host" Isolation.IsolationVoterQuorumHealthyView (phase promoted)
  (newTimer, spec) <- case demotion of
    Isolation.IsolationGraceStarted timer spec -> pure (timer, spec)
    other -> assertFailure ("demotion did not arm ordinary grace: " <> show other)
  assertEqual "demotion keeps a fresh full absolute grace" (monotonicInstant 30) (timerSpecAbsoluteDeadline spec)
  assertEqual "promotion must not reset consumed grace generations" (Just 2) (lastGrace demoted)
  assertEqual "the new timer is not the old identity" False (oldTimer == newTimer)
  assertEqual
    "a late canceled timer cannot fence the demoted Herald"
    (demoted, Isolation.IsolationGraceTimerStale)
    (Isolation.observeGraceTimer oldTimer (TimerFired (monotonicInstant 31)) demoted)
  assertValidated "promoted voter" promoted
  assertValidated "demoted non-voter" demoted

caseHealthyDemotion :: Assertion
caseHealthyDemotion = do
  let (initialFounder, _) =
        checked
          "founder isolation"
          (Isolation.initialState (monotonicInstant 0) h1 voterHosts membership configuration)
      founder = fst (Isolation.observeOracleQuorum (monotonicInstant 1) True initialFounder)
      newHosts = Set.fromList [h2, h3]
      (demoted, disposition) =
        checked
          "healthy demotion"
          (Isolation.observeVoterConfiguration (monotonicInstant 20) newHosts Nothing (Set.fromList [h2, h3, h4]) founder)
  assertEqual "existing sufficient bindings need no new timer" Isolation.IsolationReachabilityUnchanged disposition
  assertEqual "demotion preserves healthy semantic participation" Isolation.IsolationVoterQuorumHealthyView (phase demoted)
  assertEqual "only current voter bindings contribute" newHosts (Isolation.isolationWitnessReachableVoterHosts (Isolation.stateWitness demoted))
  assertValidated "healthy founder demotion" demoted

caseFencedRoleProjection :: Assertion
caseFencedRoleProjection = do
  (initial, timer, _) <- initialNonVoter (monotonicInstant 0)
  prepared <- readyFence (snd (Isolation.observeGraceTimer timer (TimerFired (monotonicInstant 10)) initial))
  let residents = Set.singleton process1
      (terminal, _) = Isolation.commitSelfFence residents Isolation.IsolationDrainNotRequired prepared
      (draining, _) = Isolation.commitSelfFence residents Isolation.IsolationDrainRequired prepared
      promotedHosts = Set.insert h4 voterHosts
  mapM_
    ( \fenced -> do
        let (updated, disposition) =
              checked
                "post-fence role projection"
                (Isolation.observeVoterConfiguration (monotonicInstant 12) promotedHosts Nothing voterHosts fenced)
        assertEqual "configuration does not revive a fenced phase" (phase fenced) (phase updated)
        assertEqual "configuration leaves the exact drain timer alone" (Isolation.isolationWitnessCurrentTimer (Isolation.stateWitness fenced)) (Isolation.isolationWitnessCurrentTimer (Isolation.stateWitness updated))
        assertEqual "configuration cannot change the resident overlay" residents (Isolation.isolationWitnessResidentProcessOverlay (Isolation.stateWitness updated))
        assertEqual "role projection emits no reachability timer" Isolation.IsolationReachabilityUnchanged disposition
        assertValidated "fenced role update" updated
    )
    [terminal, draining]

initialNonVoter ::
  MonotonicInstant ->
  IO (Isolation.State, TimerAttempt, TimerSpec)
initialNonVoter observedAt = case Isolation.initialState observedAt h4 voterHosts membership configuration of
  Right (state, Isolation.InitialIsolationGraceArmed attempt spec) -> pure (state, attempt, spec)
  other -> assertFailure ("non-voter initialization failed: " <> show other)

initialFixture ::
  Either
    Isolation.IsolationInitializationProblem
    (Isolation.State, Isolation.IsolationInitializationDisposition)
initialFixture =
  Isolation.initialState
    (monotonicInstant 0)
    h4
    voterHosts
    membership
    configuration

readyFence ::
  Isolation.IsolationGraceTimerDisposition ->
  IO Isolation.PreparedSelfFence
readyFence = \case
  Isolation.IsolationGraceTimerSelfFenceReady prepared -> pure prepared
  other -> assertFailure ("expected a ready fence, got: " <> show other)

startedDrain ::
  Isolation.IsolationSelfFenceDisposition ->
  IO (IsolationDrainId, TimerAttempt, TimerSpec)
startedDrain = \case
  Isolation.IsolationReadOnlyDrainStarted drain attempt spec _ _ ->
    pure (drain, attempt, spec)
  other -> assertFailure ("expected a read-only drain, got: " <> show other)

phase :: Isolation.State -> Isolation.IsolationPhaseView
phase = Isolation.isolationWitnessPhase . Isolation.stateWitness

lastGrace :: Isolation.State -> Maybe Word64
lastGrace =
  fmap isolationGraceGenerationWord64
    . Isolation.isolationWitnessLastGraceGeneration
    . Isolation.stateWitness

attemptGeneration :: TimerAttempt -> Word64
attemptGeneration =
  timerAttemptGenerationWord64 . timerAttemptGeneration

assertValidated :: String -> Isolation.State -> Assertion
assertValidated context state =
  assertEqual (context <> " remains owner-valid") (Right ()) (Isolation.validateState state)

configuration :: IsolationConfiguration
configuration = checked "isolation configuration" (checkIsolationConfiguration 10 5)

voterHosts :: Set HeraldEpoch
voterHosts = Set.fromList [h1, h2, h3]

h1, h2, h3, h4, h5, h6 :: HeraldEpoch
h1 = epoch 0x11
h2 = epoch 0x22
h3 = epoch 0x33
h4 = epoch 0x44
h5 = epoch 0x45
h6 = epoch 0x46

membership, otherMembership :: HeraldMembershipGenerationId
membership = membershipId 0x51
otherMembership = membershipId 0x52

process1, process2 :: ProcessEpochId
process1 = processEpoch 0x61
process2 = processEpoch 0x62

epoch :: Word8 -> HeraldEpoch
epoch byte = checked "Herald epoch" (mkHeraldEpoch (ByteString.replicate 32 byte))

membershipId :: Word8 -> HeraldMembershipGenerationId
membershipId byte =
  checked
    "membership generation"
    (mkHeraldMembershipGenerationId (ByteString.replicate 32 byte))

processEpoch :: Word8 -> ProcessEpochId
processEpoch byte =
  checked "process epoch" (mkProcessEpochId (ByteString.replicate 32 byte))

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (error . ((label <> ": ") <>) . show) id
