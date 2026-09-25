{-# LANGUAGE OverloadedRecordDot #-}

module FailureDetectionProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word8)
import Eclips.Domain.Identity
  ( HeraldEpoch,
    HeraldId,
    SystemId,
    controlIndex,
    mkHeraldEpoch,
    mkHeraldId,
    mkSystemId,
  )
import Eclips.Domain.Membership
  ( FailureProbeResolution (..),
    HeraldFailureProbeId,
    HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    deriveFailureProbeResolutionId,
    deriveHeraldFailureProbeId,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationId,
  )
import Eclips.Herald.Discovery.Internal
  ( ConnectionNonce (..),
    PeerBinding (..),
    PeerCandidate (..),
    firstPeerBindingGeneration,
  )
import Eclips.Herald.FailureDetection.State qualified as FailureDetection
import Eclips.Herald.Genesis.Internal
  ( checkedCatalogueDigest,
    checkedConfigurationDigest,
    checkedInitialProjectionDigest,
    checkedLocalHeraldEpoch,
    checkedLocalHeraldId,
    checkedOracleControlIndex,
    checkedSystemId,
  )
import Eclips.Herald.OracleClient (oracleHelloClaims)
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.Peer.Step15
  ( DirectFailureProbeRequest,
    DirectFailureProbeResult (..),
    directFailureProbeRequest,
    directFailureProbeResponse,
  )
import Eclips.Herald.PeerLiveness
  ( PeerRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
  )
import Eclips.Herald.PeerLiveness.Internal
  ( PeerRecoveryGeneration,
    firstPeerRecoveryGeneration,
    nextPeerRecoveryGeneration,
  )
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Timer
  ( TimerAttempt,
    TimerOutcome (TimerFired),
    TimerSpec,
    timerSpecAbsoluteDeadline,
  )
import Eclips.Oracle.Canonical (canonicalOracleEnvelopeValue)
import Eclips.Oracle.Command
  ( OracleCommand,
    ProbeResult (..),
    openHeraldFailureProbeCommand,
    oracleEnvelopeCommand,
  )
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Public.Types.Timing
import GenesisFixtures
  ( fixtureCheckedGenesis,
    fixtureCheckedInitialBootstraps,
    fixtureOracleContacts,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck (Property, counterexample, ioProperty, testProperty)

tests :: TestTree
tests =
  testGroup
    "live Herald failure detection"
    [ testProperty "derived fresh probes expire independently of the longer reconnect grace" propDerivedProbeDeadline,
      testCase "only a voter retains one recovery-qualified Open intention" caseVoterOpenGate,
      testCase "a live voter target contributes one fresh self-reachability report" caseLocalVoterTarget,
      testCase "a stopped H4 produces one bounded Unreachable report" caseStoppedH4,
      testCase "a reachable H4 answers only a fresh open direct probe" caseReachableH4,
      testCase "duplicate and stale direct observations are inert" caseDuplicateAndStale,
      testCase "strict voter majorities retain one deterministic terminal intention" caseMajorityIntent,
      testCase "Dismiss requires a new paced recovery generation before reopening" caseDismissRearm,
      testCase "recovery qualification changes only the stable local Open identity" caseOpenRequestIdentity,
      testCase "one probe has one retry-stable local Report result" caseReportRequestIdentity,
      testCase "promoted targets require fresh evidence against the entire voter denominator" casePromotedTarget,
      testCase "demotion cancels probes without reusing their identities after promotion" caseDemotedReporter,
      testCase "pending and joint changes suspend fresh probe work" caseVoterChangeGate,
      testProperty "configuration changes cannot rebase either report majority" propNoEvidenceRebase
    ]

propDerivedProbeDeadline :: Word8 -> Property
propDerivedProbeDeadline scale = ioProperty $ do
  let target = either (error . show) id (takeoverTarget (500_000 * (1 + fromIntegral scale)))
      policy = deriveTimingPolicy target
      duration = timingFailureProbeMicroseconds policy
      initial = FailureDetection.initialStateWithProbeDuration duration h1 voterHosts (Just configuration1)
      (opened, actions) = openAt Nothing probe1 initial
  (attempt, specification, _) <- openedAttempt actions
  assertEqual "deadline starts at committed Open observation" (monotonicInstant (100 + duration)) (timerSpecAbsoluteDeadline specification)
  let (_, expired) = FailureDetection.observeProbeTimer attempt (TimerFired (monotonicInstant (100 + duration))) opened
  assertEqual "probe reports before reconnect-sized wait could expire" [FailureDetection.RetainFailureProbeReport probe1 configuration1 ProbeUnreachable] expired
  pure (duration < timingPeerRecoveryGraceMicroseconds policy)

caseLocalVoterTarget :: Assertion
caseLocalVoterTarget = do
  let (opened, actions) = FailureDetection.observeProbeOpened (monotonicInstant 10) membership Nothing probe1 h1 generation configuration1 voterHosts (initialAt h1)
      (duplicate, repeated) = FailureDetection.observeProbeOpened (monotonicInstant 11) membership Nothing probe1 h1 generation configuration1 voterHosts opened
  assertEqual "the executing target owner supplies fresh Reachable without a self socket or timeout" [FailureDetection.RetainFailureProbeReport probe1 configuration1 ProbeReachable] actions
  assertEqual "replayed Open does not report twice" [] repeated
  assertEqual "replayed Open preserves state" opened duplicate
  assertValidated "local target report" opened

caseVoterOpenGate :: Assertion
caseVoterOpenGate = do
  let voterInitial = initialAt h1
      nonVoterInitial = initialAt h4
      (retained, firstActions) =
        FailureDetection.observeRecoveryExpired
          h4
          membership
          firstRecovery
          voterInitial
      (duplicate, duplicateActions) =
        FailureDetection.observeRecoveryExpired
          h4
          membership
          firstRecovery
          retained
      (nonVoter, nonVoterActions) =
        FailureDetection.observeRecoveryExpired
          h1
          membership
          firstRecovery
          nonVoterInitial
  assertEqual
    "the current voter retains the exact episode-qualified intention"
    [ FailureDetection.RetainOpenFailureProbe
        h4
        generation
        firstRecovery
        configuration1
    ]
    firstActions
  assertEqual "an equal observation emits nothing" [] duplicateActions
  assertEqual "an equal observation does not mutate state" retained duplicate
  assertEqual "H4 relies on its isolation owner" [] nonVoterActions
  assertEqual "the non-voter owner remains unchanged" nonVoterInitial nonVoter
  assertValidated "voter Open gate" retained
  assertValidated "non-voter Open gate" nonVoter

caseStoppedH4 :: Assertion
caseStoppedH4 = do
  let (opened, openActions) = openAt Nothing probe1 (initialAt h1)
  (firstAttempt, firstSpec, _) <- openedAttempt openActions
  assertEqual
    "the direct attempt has one absolute deadline"
    (monotonicInstant 110)
    (timerSpecAbsoluteDeadline firstSpec)

  let (afterEarly, earlyActions) =
        FailureDetection.observeProbeTimer
          firstAttempt
          (TimerFired (monotonicInstant 109))
          opened
  (secondAttempt, secondSpec) <- soleArm earlyActions
  assertEqual "an early callback cannot widen the deadline" firstSpec secondSpec

  let (afterOld, oldActions) =
        FailureDetection.observeProbeTimer
          firstAttempt
          (TimerFired (monotonicInstant 110))
          afterEarly
      (reported, expiryActions) =
        FailureDetection.observeProbeTimer
          secondAttempt
          (TimerFired (monotonicInstant 110))
          afterOld
      (duplicate, duplicateActions) =
        FailureDetection.observeProbeTimer
          secondAttempt
          (TimerFired (monotonicInstant 111))
          reported
  assertEqual "the replaced timer is stale" [] oldActions
  assertEqual "a stale timer cannot mutate state" afterEarly afterOld
  assertEqual
    "the fresh fixed deadline retains one Unreachable report"
    [FailureDetection.RetainFailureProbeReport probe1 configuration1 ProbeUnreachable]
    expiryActions
  assertEqual "the consumed timeout cannot report again" [] duplicateActions
  assertEqual "the consumed timeout cannot mutate state" reported duplicate
  assertValidated "stopped H4" reported

caseReachableH4 :: Assertion
caseReachableH4 = do
  let reporterBinding = bindingTo h4 1
      targetBinding = bindingTo h1 1
      (reporter, openActions) =
        openAt (Just reporterBinding) probe1 (initialAt h1)
  (timer, _, request) <- openedAttempt openActions

  let (targetBeforeOpen, beforeOpenActions) =
        FailureDetection.observeDirectProbeRequest
          targetBinding
          membership
          False
          voterHosts
          request
          (initialAt h4)
      (target, responseActions) =
        FailureDetection.observeDirectProbeRequest
          targetBinding
          membership
          True
          voterHosts
          request
          targetBeforeOpen
  assertEqual "pre-Open reachability is not evidence" [] beforeOpenActions
  assertEqual "the request does not allocate target-side probe state" targetBeforeOpen target
  response <- case responseActions of
    [FailureDetection.SendDirectFailureProbeResponse binding response]
      | binding == targetBinding -> pure response
    other -> assertFailure ("expected one reachable H4 response, got " <> show other)

  let (reported, reportActions) =
        FailureDetection.observeDirectProbeResponse
          reporterBinding
          response
          reporter
  assertEqual
    "the exact response cancels the timer and retains one Reachable report"
    [ FailureDetection.CancelDirectFailureProbeTimer timer,
      FailureDetection.RetainFailureProbeReport probe1 configuration1 ProbeReachable
    ]
    reportActions
  assertValidated "reachable H4 reporter" reported
  assertValidated "reachable H4 target" target

caseDuplicateAndStale :: Assertion
caseDuplicateAndStale = do
  let currentBinding = bindingTo h4 1
      replacementBinding = bindingTo h4 2
      (opened, openActions) =
        openAt (Just currentBinding) probe1 (initialAt h1)
  (timer, _, request) <- openedAttempt openActions
  let (sameBinding, sameActions) =
        FailureDetection.observeBindingAccepted currentBinding opened
      (replacementOffered, replacementActions) =
        FailureDetection.observeBindingAccepted replacementBinding sameBinding
      (replacementDuplicate, replacementDuplicateActions) =
        FailureDetection.observeBindingAccepted replacementBinding replacementOffered
  assertEqual "the already offered binding is inert" [] sameActions
  assertEqual
    "one later accepted logical binding gets exactly one offer"
    [FailureDetection.SendDirectFailureProbe replacementBinding request]
    replacementActions
  assertEqual "the replacement cannot be reoffered" [] replacementDuplicateActions
  assertEqual "a duplicate binding cannot mutate state" replacementOffered replacementDuplicate

  let staleRequest = directFailureProbeRequest probe1 h4 otherGeneration configuration1
      (_, staleRequestActions) =
        FailureDetection.observeDirectProbeRequest
          (bindingTo h1 1)
          membership
          True
          voterHosts
          staleRequest
          (initialAt h4)
      staleResponse =
        directFailureProbeResponse request DirectFailureProbeUnreachable
      (afterStaleResponse, staleResponseActions) =
        FailureDetection.observeDirectProbeResponse
          currentBinding
          staleResponse
          replacementOffered
      wrongConfiguration = directFailureProbeRequest probe1 h4 generation configuration2
      (afterWrongConfiguration, wrongConfigurationActions) =
        FailureDetection.observeDirectProbeResponse
          replacementBinding
          (directFailureProbeResponse wrongConfiguration DirectFailureProbeReachable)
          replacementOffered
  assertEqual "a stale generation request is inert" [] staleRequestActions
  assertEqual "a peer cannot author Unreachable" [] staleResponseActions
  assertEqual "the invalid response cannot mutate state" replacementOffered afterStaleResponse
  assertEqual "a different captured configuration cannot report reachability" [] wrongConfigurationActions
  assertEqual "the wrong-configuration response is inert" replacementOffered afterWrongConfiguration

  let (terminal, terminalActions) =
        FailureDetection.observeProbeTerminal probe1 replacementOffered
      (afterLateTimer, lateTimerActions) =
        FailureDetection.observeProbeTimer
          timer
          (TimerFired (monotonicInstant 110))
          terminal
      (afterLateReports, lateReportActions) =
        FailureDetection.observeProjectedReports
          probe1
          h4
          [(h1, ProbeReachable), (h2, ProbeReachable)]
          afterLateTimer
  assertEqual
    "terminal projection cancels the exact active timer"
    [FailureDetection.CancelDirectFailureProbeTimer timer]
    terminalActions
  assertEqual "a terminal probe cannot reuse a late timeout" [] lateTimerActions
  assertEqual "a late timeout cannot mutate terminal evidence" terminal afterLateTimer
  assertEqual "a terminal probe cannot reuse a stale majority" [] lateReportActions
  assertEqual "a stale majority cannot mutate terminal evidence" terminal afterLateReports
  assertValidated "duplicate and stale observations" afterLateReports

caseMajorityIntent :: Assertion
caseMajorityIntent = do
  let (reachableProbe, _) = openAt Nothing probe1 (initialAt h1)
      (dismissReady, dismissActions) =
        FailureDetection.observeProjectedReports
          probe1
          h4
          [(h1, ProbeReachable), (h2, ProbeReachable)]
          reachableProbe
      (dismissDuplicate, dismissDuplicateActions) =
        FailureDetection.observeProjectedReports
          probe1
          h4
          [(h1, ProbeReachable), (h2, ProbeReachable), (h3, ProbeReachable)]
          dismissReady
      (unreachableProbe, _) = openAt Nothing probe2 (initialAt h1)
      (retireReady, retireActions) =
        FailureDetection.observeProjectedReports
          probe2
          h4
          [(h1, ProbeUnreachable), (h2, ProbeUnreachable)]
          unreachableProbe
      (_, minorityActions) =
        FailureDetection.observeProjectedReports
          probe2
          h4
          [(h1, ProbeUnreachable)]
          unreachableProbe
  assertEqual
    "a Reachable strict majority derives the one Dismiss identity"
    [ FailureDetection.RetainFailureProbeDismissal
        (deriveFailureProbeResolutionId probe1 DismissFailureProbe)
    ]
    dismissActions
  assertEqual "later majority projection is idempotent" [] dismissDuplicateActions
  assertEqual "idempotence preserves the retained intention" dismissReady dismissDuplicate
  assertEqual
    "an Unreachable strict majority derives the one retirement identity"
    [ FailureDetection.RetainHeraldRetirement
        (deriveFailureProbeResolutionId probe2 RetireFailureProbeTarget)
        h4
    ]
    retireActions
  assertEqual "one reporter cannot retire H4" [] minorityActions
  assertValidated "Dismiss majority" dismissReady
  assertValidated "Retire majority" retireReady

caseDismissRearm :: Assertion
caseDismissRearm = do
  let (firstSuspected, firstActions) =
        FailureDetection.observeRecoveryExpired
          h4
          membership
          firstRecovery
          (initialAt h1)
      (opened, _) = openAt Nothing probe1 firstSuspected
      (dismissed, _) = FailureDetection.observeProbeTerminal probe1 opened
      (sameEpisode, sameActions) =
        FailureDetection.observeRecoveryExpired
          h4
          membership
          firstRecovery
          dismissed
      secondRecovery = nextPeerRecoveryGeneration firstRecovery
      (nextEpisode, nextActions) =
        FailureDetection.observeRecoveryExpired
          h4
          membership
          secondRecovery
          sameEpisode
      (duplicate, duplicateActions) =
        FailureDetection.observeRecoveryExpired
          h4
          membership
          secondRecovery
          nextEpisode
  assertEqual
    "the first paced episode retains one stable Open"
    [ FailureDetection.RetainOpenFailureProbe
        h4
        generation
        firstRecovery
        configuration1
    ]
    firstActions
  assertEqual "Dismiss cannot reopen within the consumed episode" [] sameActions
  assertEqual "the consumed episode remains inert" dismissed sameEpisode
  assertEqual
    "the next paced recovery generation retains one fresh Open"
    [ FailureDetection.RetainOpenFailureProbe
        h4
        generation
        secondRecovery
        configuration1
    ]
    nextActions
  assertEqual "the fresh episode is itself stable" [] duplicateActions
  assertEqual "an equal fresh observation cannot mutate state" nextEpisode duplicate
  assertValidated "post-Dismiss rearm" nextEpisode

caseOpenRequestIdentity :: Assertion
caseOpenRequestIdentity = do
  firstPrepared <-
    checkedIO
      "first recovery-qualified Open"
      ( OracleClient.prepareOpenHeraldFailureProbeRequest
          h4
          generation
          firstRecovery
          configuration1
          initialOracleClient
      )
  let (firstClient, firstClassification, firstReference) =
        OracleClient.commitOracleRequest firstPrepared
  duplicatePrepared <-
    checkedIO
      "equal recovery-qualified Open"
      ( OracleClient.prepareOpenHeraldFailureProbeRequest
          h4
          generation
          firstRecovery
          configuration1
          firstClient
      )
  let (duplicateClient, duplicateClassification, duplicateReference) =
        OracleClient.commitOracleRequest duplicatePrepared
      secondRecovery = nextPeerRecoveryGeneration firstRecovery
  secondPrepared <-
    checkedIO
      "next recovery-qualified Open"
      ( OracleClient.prepareOpenHeraldFailureProbeRequest
          h4
          generation
          secondRecovery
          configuration1
          duplicateClient
      )
  let (secondClient, secondClassification, secondReference) =
        OracleClient.commitOracleRequest secondPrepared
  assertEqual
    "the first episode allocates one stable request"
    OracleClient.OracleRequestFirstPrepared
    firstClassification
  assertEqual
    "an equal episode is the exact retained request"
    OracleClient.OracleRequestExactRetry
    duplicateClassification
  assertEqual "the equal episode retains its reference" firstReference duplicateReference
  assertEqual "the exact retry cannot mutate client state" firstClient duplicateClient
  assertEqual
    "a later paced episode allocates a fresh local request"
    OracleClient.OracleRequestFirstPrepared
    secondClassification
  assertEqual
    "the later episode has a distinct request identity"
    False
    (secondReference == firstReference)
  firstCommand <- retainedCommand firstReference secondClient
  secondCommand <- retainedCommand secondReference secondClient
  let expected = openHeraldFailureProbeCommand h4 generation configuration1
  assertEqual "the first wire command remains the configuration-qualified Open" expected firstCommand
  assertEqual "the next wire command remains the same configuration-qualified Open" expected secondCommand

caseReportRequestIdentity :: Assertion
caseReportRequestIdentity = do
  reachablePrepared <-
    checkedIO
      "first Reachable report"
      ( OracleClient.prepareReportHeraldFailureProbeRequest
          probe1
          configuration1
          ProbeReachable
          initialOracleClient
      )
  let (reachableClient, firstClassification, firstReference) =
        OracleClient.commitOracleRequest reachablePrepared
  duplicatePrepared <-
    checkedIO
      "equal Reachable report"
      ( OracleClient.prepareReportHeraldFailureProbeRequest
          probe1
          configuration1
          ProbeReachable
          reachableClient
      )
  let (duplicateClient, duplicateClassification, duplicateReference) =
        OracleClient.commitOracleRequest duplicatePrepared
  assertEqual "the first result allocates its request" OracleClient.OracleRequestFirstPrepared firstClassification
  assertEqual "the equal result is an exact retry" OracleClient.OracleRequestExactRetry duplicateClassification
  assertEqual "the equal result retains its reference" firstReference duplicateReference
  assertEqual "the equal result cannot mutate client state" reachableClient duplicateClient
  case OracleClient.prepareReportHeraldFailureProbeRequest
    probe1
    configuration1
    ProbeUnreachable
    duplicateClient of
    Left (OracleClient.OracleRequestConflict _) -> pure ()
    other -> assertFailure ("conflicting local report did not conflict: " <> showPrepared other)

showPrepared :: Either OracleClient.OracleClientProblem OracleClient.PreparedOracleRequest -> String
showPrepared result = case result of
  Left problem -> show problem
  Right _ -> "unexpected prepared Oracle request"

casePromotedTarget :: Assertion
casePromotedTarget = do
  let (opened, openActions) = openAt (Just (bindingTo h4 1)) probe1 (initialAt h1)
  (timer, _, request) <- openedAttempt openActions
  let promotedHosts = Set.insert h4 voterHosts
      (updated, actions) =
        checked
          "promoted target roles"
          (FailureDetection.observeVoterConfiguration voterHosts (Just promotedHosts) (Just configuration2) False opened)
      (afterLateResponse, responseActions) =
        FailureDetection.observeDirectProbeResponse
          (bindingTo h4 1)
          (directFailureProbeResponse request DirectFailureProbeReachable)
          updated
      (_, recoveryActions) = FailureDetection.observeRecoveryExpired h4 membership firstRecovery updated
      (stable, _) =
        checked
          "promoted target stable roles"
          (FailureDetection.observeVoterConfiguration promotedHosts Nothing (Just configuration2) False updated)
      (_, stableRecoveryActions) = FailureDetection.observeRecoveryExpired h4 membership firstRecovery stable
  assertEqual "promotion cancels the old direct attempt" [FailureDetection.CancelDirectFailureProbeTimer timer] actions
  assertEqual "joint union supplies the suspended reporter roles" promotedHosts (FailureDetection.stateWitness updated).witnessVoterHosts
  assertEqual "a pending old response cannot become evidence" [] responseActions
  assertEqual "the response is a state no-op" updated afterLateResponse
  assertEqual "joint voter configurations suspend probes" [] recoveryActions
  assertEqual
    "the promoted target can open only a new configuration-qualified probe"
    [FailureDetection.RetainOpenFailureProbe h4 generation firstRecovery configuration2]
    stableRecoveryActions
  let (fresh, _) = openAt Nothing probe2 stable
      (minority, minorityActions) = FailureDetection.observeProjectedReports probe2 h4 [(h1, ProbeUnreachable), (h2, ProbeUnreachable)] fresh
      (accepted, majorityActions) = FailureDetection.observeProjectedReports probe2 h4 [(h1, ProbeUnreachable), (h2, ProbeUnreachable), (h3, ProbeUnreachable)] minority
  assertEqual "the target remains in the captured four-voter denominator" promotedHosts (maybe Set.empty (.witnessReporters) (FailureDetection.failureProbeWitness probe2 fresh))
  assertEqual "two of four reporters cannot exclude their target" [] minorityActions
  assertEqual "three of four reporters retain native exclusion before semantic retirement" [FailureDetection.RetainVoterHostFailureAcceptance (deriveFailureProbeResolutionId probe2 RetireFailureProbeTarget)] majorityActions
  assertEqual
    "historical probe identity remains terminal"
    (Just FailureDetection.FailureProbeTerminalWitness)
    ((\witness -> witness.witnessPhase) <$> FailureDetection.failureProbeWitness probe1 updated)
  assertValidated "joint promoted target" updated
  assertValidated "stable promoted target" stable
  assertValidated "accepted voter-host failure" accepted

caseDemotedReporter :: Assertion
caseDemotedReporter = do
  let (suspected, _) = FailureDetection.observeRecoveryExpired h4 membership firstRecovery (initialAt h1)
      (opened, openActions) = openAt Nothing probe1 suspected
  (timer, _, _) <- openedAttempt openActions
  let (demoted, actions) =
        checked
          "demoted reporter roles"
          (FailureDetection.observeVoterConfiguration (Set.fromList [h2, h3]) Nothing (Just configuration2) False opened)
      (promoted, _) =
        checked
          "restored reporter roles"
          (FailureDetection.observeVoterConfiguration voterHosts Nothing (Just configuration1) False demoted)
      (afterOldTimer, timerActions) = FailureDetection.observeProbeTimer timer (TimerFired (monotonicInstant 200)) promoted
      (afterOldOpen, oldOpenActions) = openAt Nothing probe1 promoted
      (afterOldRecovery, oldRecoveryActions) = FailureDetection.observeRecoveryExpired h4 membership firstRecovery promoted
      (_, freshRecoveryActions) = FailureDetection.observeRecoveryExpired h4 membership (nextPeerRecoveryGeneration firstRecovery) promoted
  assertEqual "demotion cancels exact active timers" [FailureDetection.CancelDirectFailureProbeTimer timer] actions
  assertEqual "demotion clears active Open intentions" [] (FailureDetection.stateWitness demoted).witnessOpenIntentions
  assertEqual "later promotion cannot revive a timeout" [] timerActions
  assertEqual "the late timer is inert" promoted afterOldTimer
  assertEqual "old Open projection cannot allocate a fresh attempt" [] oldOpenActions
  assertEqual "old Open projection is inert" promoted afterOldOpen
  assertEqual "the consumed recovery cannot reuse its Open request" [] oldRecoveryActions
  assertEqual "the old recovery is inert" promoted afterOldRecovery
  assertEqual
    "fresh recovery remains possible after a complete role change"
    [FailureDetection.RetainOpenFailureProbe h4 generation (nextPeerRecoveryGeneration firstRecovery) configuration1]
    freshRecoveryActions
  assertValidated "demoted reporter with terminal history" demoted
  assertValidated "promoted reporter with terminal history" promoted

caseVoterChangeGate :: Assertion
caseVoterChangeGate = do
  let (opened, openActions) = openAt Nothing probe1 (initialAt h1)
  (timer, _, _) <- openedAttempt openActions
  mapM_
    ( \(newHosts, pending) -> do
        let (updated, actions) =
              checked
                "voter change gate"
                (FailureDetection.observeVoterConfiguration voterHosts newHosts (Just configuration1) pending opened)
            (_, openActions') = openAt Nothing probe2 updated
            (_, recoveryActions) = FailureDetection.observeRecoveryExpired h4 membership firstRecovery updated
            (duplicate, duplicateActions) =
              checked
                "duplicate voter change gate"
                (FailureDetection.observeVoterConfiguration voterHosts newHosts (Just configuration1) pending updated)
        assertEqual "changing coordination invalidates the old timer" [FailureDetection.CancelDirectFailureProbeTimer timer] actions
        assertEqual "pending/joint Open cannot start a probe" [] openActions'
        assertEqual "pending/joint recovery cannot retain Open" [] recoveryActions
        assertEqual "identical role facts do not recancel timers" [] duplicateActions
        assertEqual "identical role facts are a state no-op" updated duplicate
        assertValidated "suspended failure detection" updated
    )
    [(Nothing, True), (Just (Set.fromList [h1, h2]), False)]

propNoEvidenceRebase :: Bool -> Bool -> Property
propNoEvidenceRebase reachable shrink =
  let result = if reachable then ProbeReachable else ProbeUnreachable
      (opened, _) = openAt Nothing probe1 (initialAt h1)
      reports = [(h1, result)]
      (beforeChange, oldActions) = FailureDetection.observeProjectedReports probe1 h4 reports opened
      newReporters = if shrink then Set.singleton h1 else voterHosts
      (afterChange, _) =
        checked
          "new singleton roles"
          (FailureDetection.observeVoterConfiguration newReporters Nothing (Just configuration2) False beforeChange)
      (afterReports, actions) = FailureDetection.observeProjectedReports probe1 h4 reports afterChange
      (freshProbe, _) = openAt Nothing probe2 afterChange
      (_, freshActions) = FailureDetection.observeProjectedReports probe2 h4 reports freshProbe
      resolution = if reachable then DismissFailureProbe else RetireFailureProbeTarget
      expected =
        if reachable
          then FailureDetection.RetainFailureProbeDismissal (deriveFailureProbeResolutionId probe2 resolution)
          else FailureDetection.RetainHeraldRetirement (deriveFailureProbeResolutionId probe2 resolution) h4
   in counterexample
        (show (result, FailureDetection.stateWitness afterReports, actions, freshActions))
        ( null oldActions
            && null actions
            && afterChange == afterReports
            && freshActions == [expected | shrink]
            && FailureDetection.validateState afterReports == Right ()
        )

retainedCommand :: OracleClient.OracleRequestRef -> OracleClient.State -> IO OracleCommand
retainedCommand reference client = case OracleClient.lookupOracleRequest reference client of
  Nothing -> assertFailure ("missing retained Oracle request " <> show reference)
  Just witness ->
    pure
      ( oracleEnvelopeCommand
          ( canonicalOracleEnvelopeValue
              (OracleClient.oracleRequestWitnessEnvelope witness)
          )
      )

initialOracleClient :: OracleClient.State
initialOracleClient =
  OracleClient.initialState
    ( oracleHelloClaims
        (checkedSystemId fixtureCheckedGenesis)
        (checkedCatalogueDigest fixtureCheckedGenesis)
        (checkedConfigurationDigest fixtureCheckedGenesis)
        (checkedInitialProjectionDigest fixtureCheckedInitialBootstraps)
        (checkedLocalHeraldId fixtureCheckedGenesis)
        (checkedLocalHeraldEpoch fixtureCheckedGenesis)
        generation
    )
    fixtureOracleContacts
    (checkedOracleControlIndex fixtureCheckedGenesis)

configuration1, configuration2 :: Voter.VoterConfigurationId
configuration1 = checked "first voter configuration" (Voter.mkVoterConfigurationId (ByteString.replicate 32 231))
configuration2 = checked "second voter configuration" (Voter.mkVoterConfigurationId (ByteString.replicate 32 232))

initialAt :: HeraldEpoch -> FailureDetection.State
initialAt local =
  FailureDetection.initialState local voterHosts (Just configuration1) recoveryConfiguration

openAt ::
  Maybe PeerBinding ->
  HeraldFailureProbeId ->
  FailureDetection.State ->
  (FailureDetection.State, [FailureDetection.FailureDetectionAction])
openAt targetBinding identifier state =
  FailureDetection.observeProbeOpened
    (monotonicInstant 100)
    membership
    targetBinding
    identifier
    h4
    generation
    (maybe configuration1 id (FailureDetection.stateWitness state).witnessVoterConfiguration)
    (FailureDetection.stateWitness state).witnessVoterHosts
    state

openedAttempt ::
  [FailureDetection.FailureDetectionAction] ->
  IO (TimerAttempt, TimerSpec, DirectFailureProbeRequest)
openedAttempt actions = case actions of
  [FailureDetection.ArmDirectFailureProbeTimer attempt specification] ->
    pure (attempt, specification, directFailureProbeRequest probe1 h4 generation configuration1)
  [ FailureDetection.ArmDirectFailureProbeTimer attempt specification,
    FailureDetection.SendDirectFailureProbe _ request
    ] -> pure (attempt, specification, request)
  other -> assertFailure ("expected one opened direct attempt, got " <> show other)

soleArm ::
  [FailureDetection.FailureDetectionAction] ->
  IO (TimerAttempt, TimerSpec)
soleArm actions = case actions of
  [FailureDetection.ArmDirectFailureProbeTimer attempt specification] ->
    pure (attempt, specification)
  other -> assertFailure ("expected one rearmed direct attempt, got " <> show other)

assertValidated :: String -> FailureDetection.State -> Assertion
assertValidated context state =
  assertEqual
    (context <> " remains owner-valid")
    (Right ())
    (FailureDetection.validateState state)

bindingTo :: HeraldEpoch -> Word8 -> PeerBinding
bindingTo remote nonce =
  PeerBinding
    (heraldId nonce)
    remote
    firstPeerBindingGeneration
    (PeerCandidate (heraldId nonce) remote (ConnectionNonce (fromIntegral nonce)))

recoveryConfiguration :: PeerRecoveryConfiguration
recoveryConfiguration =
  checked "peer recovery configuration" (checkPeerRecoveryConfiguration 10)

firstRecovery :: PeerRecoveryGeneration
firstRecovery = firstPeerRecoveryGeneration

voterHosts :: Set HeraldEpoch
voterHosts = Set.fromList [h1, h2, h3]

membership :: HeraldMembershipGeneration
membership =
  checked
    "Herald membership"
    (genesisHeraldMembershipGeneration system (h1 :| [h2, h3, h4]))

generation :: HeraldMembershipGenerationId
generation = heraldMembershipGenerationId membership

otherGeneration :: HeraldMembershipGenerationId
otherGeneration =
  heraldMembershipGenerationId
    ( checked
        "other Herald membership"
        (genesisHeraldMembershipGeneration (checkedIdentifier mkSystemId 0x55) (h1 :| [h2, h3, h4]))
    )

probe1, probe2 :: HeraldFailureProbeId
probe1 = checked "failure probe 1" (deriveHeraldFailureProbeId (controlIndex 1))
probe2 = checked "failure probe 2" (deriveHeraldFailureProbeId (controlIndex 2))

h1, h2, h3, h4 :: HeraldEpoch
h1 = epoch 0x11
h2 = epoch 0x22
h3 = epoch 0x33
h4 = epoch 0x44

system :: SystemId
system = checkedIdentifier mkSystemId 0x50

epoch :: Word8 -> HeraldEpoch
epoch = checkedIdentifier mkHeraldEpoch

heraldId :: Word8 -> HeraldId
heraldId = checkedIdentifier mkHeraldId

checkedIdentifier ::
  (ByteString.ByteString -> Either problem value) ->
  Word8 ->
  value
checkedIdentifier constructor byte =
  either
    (const (error "32-byte test identifier rejected"))
    id
    (constructor (ByteString.replicate 32 byte))

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (error . ((label <> ": ") <>) . show) id

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO label = either (assertFailure . ((label <> ": ") <>) . show) pure
