{-# LANGUAGE OverloadedStrings #-}

module Step15FailureVerticalProperties (tests) where

import Control.Monad (foldM)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( HeraldEpoch,
    ProcessEpochId,
    controlIndex,
    mkHeraldEpoch,
    mkHeraldId,
    mkLabelDecisionId,
  )
import Eclips.Domain.Membership
  ( HeraldFailureProbeId,
    HeraldMembershipGenerationId,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.ProcessStart (processStart)
import Eclips.Domain.Startup
  ( ConfiguredProcessBootstrap,
    HeraldMember (..),
    InitialTopologyManifest (..),
    appliedBootstrapManifestId,
    appliedProcessEpochId,
    checkInitialTopologyProjection,
    configuredProcessBootstrapManifestId,
    configuredProcessBootstrapProcessEpochId,
    configuredProcessBootstrapProcessId,
    configuredProcessBootstrapResidence,
    configuredProcessBootstrapRoots,
    deriveInitialProjectionDigest,
    genesisPredefinedOccurrenceSet,
    materializeConfiguredProcessBootstrap,
    mkConfiguredProcessBootstrap,
  )
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.EffectBatch (effectBatchMembers)
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    ConfiguredProcessManifest (..),
    DeploymentManifest (..),
    OracleGenesisManifest (..),
    PrimordialProcessManifest (..),
    checkHeraldGenesis,
    checkInitialBootstraps,
  )
import Eclips.Herald.Genesis.Internal qualified as HeraldGenesis
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.OracleClient
  ( OracleClientAction (ConnectAndHelloOracle),
    OracleClientIngress (OracleHelloReceived),
    oracleConnectAttemptContact,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleObservedTerm,
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.OracleProjection.Step15 qualified as Projection
import Eclips.Herald.Peer.Step15 qualified as PeerStep15
import Eclips.Herald.PeerLiveness
  ( PeerRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
  )
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupOracleClientState,
    replaceStartupOracleProjectionState,
    replaceStartupStructuralProgressState,
    startupOracleClientState,
    startupOracleProjectionState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Timer.Internal (TimerAttempt, TimerOutcome (TimerFired))
import Eclips.Herald.UseCase.Step15FailureVertical qualified as Vertical
import Eclips.Herald.UseCase.Step15MembershipAdvance qualified as MembershipAdvance
import Eclips.Herald.UseCase.Step15RetirementClosure qualified as RetirementClosure
import Eclips.Herald.UseCase.Step15StructuralBase qualified as StructuralBase
import Eclips.Oracle.Canonical
  ( CanonicalAppliedOracleEntry,
    canonicalAppliedOracleEntryValue,
    canonicalizeAppliedOracleEntry,
  )
import Eclips.Oracle.Command (oracleEnvelope, startProcessEpochCommand)
import Eclips.Oracle.Effect
  ( OracleEffect (EmitAppliedOracleEntry),
    OracleStepOutcome (OracleCommitted),
    oracleEffects,
  )
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    checkOracleGenesis,
    checkedOracleActiveHeralds,
    checkedOracleAppliedBootstraps,
    checkedOracleCatalogueDigest,
    checkedOracleConfigurationDigest,
    checkedOracleInitialProjectionDigest,
    checkedOraclePredefinedDescriptors,
    checkedOracleRaftConfigurationDigest,
    checkedOracleRaftNativeConfiguration,
    checkedOracleRaftVoterBindings,
    checkedOracleSystemId,
    oracleGenesis,
    raftVoterBindingHeraldEpoch,
  )
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Projection (appliedEntryControlIndex)
import Eclips.Oracle.Step15.Reference
  ( ReferenceAppliedOracleEntry,
    ReferenceCommandResult (..),
    ReferenceFailureProbeView (..),
    ReferenceOracleCommand,
    ReferenceOracleReceiptResult (..),
    ReferenceOracleState,
    ReferenceProbeResult (..),
    ReferenceSubmissionOutcome (..),
    ReferenceTerminalIntention,
    initialReferenceOracle,
    referenceOpenHeraldFailureProbeCommand,
    referenceOracleCurrentMembership,
    referenceOracleEnvelope,
    referenceOracleFailureProbe,
    referenceOracleReceiptResult,
    referenceOracleStateCanonicalBytes,
    referenceOracleStateDigest,
    referenceOracleTerminalIntention,
    referenceReportHeraldFailureProbeCommand,
    referenceTerminalIntentionCommand,
    submitReferenceOracle,
  )
import Eclips.Oracle.Step15.WorkflowReference qualified as Workflow
import Eclips.Oracle.Transition (initialOracle, stepOracle)
import Eclips.Oracle.Voter qualified as Voter
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedOracleGenesis,
    fixtureDeploymentManifest,
    fixtureGeneratorSeedH1,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
    fixtureStep14CheckedGenesis,
  )
import Step15RetirementClosureProperties (establishEmptyStructuralBase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "detached Step-15 Herald/Oracle vertical"
    [ testCase "simultaneous Open aliases and reports require projected Open" caseOpenAliasing,
      testCase "direct probes and Hello retain their exact membership generation" caseMembershipQualifiedPeerForms,
      testCase "one retirement entry drives workflow consequences through the checked adapter" caseRetirementEntryAdapter,
      testCase "reachable majority dismisses and disconnected peers cool down before rearming" caseDismissCooldown,
      testCase "unreachable majority retires a non-voter" caseRetire,
      testCase "physical stop and a still-running partition have identical Oracle meaning" casePhysicalEquivalence,
      testCase "minority, stale, non-voter reporter, and voter target remain safe" caseSafety,
      testCase "retirement supersedes another genesis probe" caseSupersedes
    ]

caseMembershipQualifiedPeerForms :: Assertion
caseMembershipQualifiedPeerForms = do
  let oracle = initialReferenceOracle genesis4
      generation = heraldMembershipGenerationId (referenceOracleCurrentMembership oracle)
      otherGeneration = heraldMembershipGenerationId (referenceOracleCurrentMembership (initialReferenceOracle genesis5))
      (_, _, openResult) = accepted h1 1 (referenceOpenHeraldFailureProbeCommand h4 generation) oracle
      probe = expectOpened openResult
      configuration = checkedEither "captured voter configuration" (Voter.mkVoterConfigurationId (ByteString.replicate 32 231))
      request = PeerStep15.directFailureProbeRequest probe h4 generation configuration
      reachable = PeerStep15.directFailureProbeResponse request PeerStep15.DirectFailureProbeReachable
      unreachable = PeerStep15.directFailureProbeResponse request PeerStep15.DirectFailureProbeUnreachable
      remote = member 0x44
      hello =
        Discovery.peerHello
          (checkedOracleSystemId genesis4)
          (heraldMemberId remote)
          (heraldMemberEpoch remote)
          (Discovery.connectionNonce 27)
          Set.empty
          (controlIndex 3)
          (checkedOracleCatalogueDigest genesis4)
          (checkedOracleInitialProjectionDigest genesis4)
          Nothing
      qualifiedHello = PeerStep15.membershipPeerHello hello generation
  assertBool "fixture generations differ" (generation /= otherGeneration)
  assertEqual "request retains probe" probe (PeerStep15.directFailureProbeRequestProbe request)
  assertEqual "request retains target" h4 (PeerStep15.directFailureProbeRequestTarget request)
  assertEqual "request retains generation" generation (PeerStep15.directFailureProbeRequestGeneration request)
  assertEqual "request retains captured voter configuration" configuration (PeerStep15.directFailureProbeRequestVoterConfiguration request)
  assertEqual "response retains exact request" request (PeerStep15.directFailureProbeResponseRequest reachable)
  assertEqual "reachable response retains result" PeerStep15.DirectFailureProbeReachable (PeerStep15.directFailureProbeResponseResult reachable)
  assertEqual "result does not alter request identity" request (PeerStep15.directFailureProbeResponseRequest unreachable)
  assertEqual "Hello wrapper retains live Hello" hello (PeerStep15.membershipPeerHelloValue qualifiedHello)
  assertEqual "Hello wrapper retains generation" generation (PeerStep15.membershipPeerHelloGeneration qualifiedHello)
  assertBool
    "a different generation produces a distinct qualified Hello"
    (qualifiedHello /= PeerStep15.membershipPeerHello hello otherGeneration)

caseRetirementEntryAdapter :: Assertion
caseRetirementEntryAdapter = do
  let oracle0 = initialReferenceOracle genesis4
      generation = heraldMembershipGenerationId (referenceOracleCurrentMembership oracle0)
      (oracle1, openEntry, openResult) = accepted h1 1 (referenceOpenHeraldFailureProbeCommand h4 generation) oracle0
      probe = expectOpened openResult
      (oracle2, _, _) = accepted h1 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeUnreachable) oracle1
      (oracle3, _, reportResult) = accepted h2 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeUnreachable) oracle2
      intention = expectIntention reportResult
      (retiredOracle, retirementEntry, _) = accepted h1 3 (referenceTerminalIntentionCommand intention) oracle3
      successor = referenceOracleCurrentMembership retiredOracle
      checkedAdvance = checkedEither "checked retirement entry" (Vertical.checkReferenceMembershipAdvance retirementEntry)
      decision = checkedEither "workflow decision" (mkLabelDecisionId (ByteString.replicate 32 0xa5))
      initialWorkflow = checkedEither "initial workflow" (Workflow.initialReferenceWorkflowState (referenceOracleCurrentMembership oracle0))
      applied = checkedEither "atomic Applied decision" (Workflow.decideReferenceWorkflow decision generation (controlIndex 2) (Right "value") initialWorkflow)
      notApplied = checkedEither "atomic NotApplied decision" (Workflow.decideReferenceWorkflow decision generation (controlIndex 2) (Left "comparison-changed") initialWorkflow)
      retiredApplied = checkedEither "apply retirement after Applied" (Vertical.applyReferenceWorkflowMembershipAdvance checkedAdvance applied)
      retiredNotApplied = checkedEither "apply retirement after NotApplied" (Vertical.applyReferenceWorkflowMembershipAdvance checkedAdvance notApplied)
      replayedApplied = checkedEither "replay retirement after Applied" (Vertical.applyReferenceWorkflowMembershipAdvance checkedAdvance retiredApplied)
      retiredBeforeDecision = checkedEither "apply retirement before decision" (Vertical.applyReferenceWorkflowMembershipAdvance checkedAdvance initialWorkflow)
      decidedAfterRetirement = checkedEither "decide in successor membership" (Workflow.decideReferenceWorkflow decision (heraldMembershipGenerationId successor) (controlIndex 5) (Right "value") retiredBeforeDecision)
      survivors = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successor)
  assertEqual "an entry without a membership successor is rejected" (Left Vertical.ReferenceMembershipAdvanceAbsent) (Vertical.checkReferenceMembershipAdvance openEntry)
  assertEqual "NotApplied remains complete without installation collection" (Just Workflow.ReferenceWorkflowCompletedNotAppliedView) (Workflow.referenceWorkflowPhase retiredNotApplied)
  assertEqual "Applied retains its pending installation collection" (Just Workflow.ReferenceWorkflowReleasedView) (Workflow.referenceWorkflowPhase retiredApplied)
  assertEqual "retirement never rewrites Applied" (Workflow.referenceWorkflowTerminal applied) (Workflow.referenceWorkflowTerminal retiredApplied)
  assertEqual "retirement never rewrites NotApplied" (Workflow.referenceWorkflowTerminal notApplied) (Workflow.referenceWorkflowTerminal retiredNotApplied)
  assertEqual "Applied contracts to successor reporters" survivors (Workflow.referenceWorkflowApplicableReporters retiredApplied)
  assertEqual "workflow receives the exact Oracle successor" successor (Workflow.referenceWorkflowCurrentMembership retiredApplied)
  assertEqual "the checked adapter preserves exact replay identity" retiredApplied replayedApplied
  assertEqual "a later decision captures only successor members" survivors (Workflow.referenceWorkflowCapturedMembers decidedAfterRetirement)
  initialHeraldState <- ordinaryHeraldState4
  boundHeraldState <- bindOrdinaryOracle initialHeraldState
  positionedHeraldState <- positionOrdinaryPrefix ordinaryPrefixEntries boundHeraldState
  assertEqual
    "ordinary Herald prefix scaffold remains whole-state valid"
    (Right ())
    (validateHeraldState positionedHeraldState)
  preparedHerald <-
    checkedIO
      "apply reference retirement to ordinary Herald state"
      (Vertical.prepareReferenceHeraldMembershipAdvance checkedAdvance positionedHeraldState)
  assertEqual
    "reference retirement drives the ordinary survivor coordinator"
    (MembershipAdvance.MembershipAdvanceApplied h4 False)
    (MembershipAdvance.preparedMembershipAdvanceDisposition preparedHerald)
  let retiredHeraldState = MembershipAdvance.commitMembershipAdvance preparedHerald
      retiredHeraldProjection = startupOracleProjectionState retiredHeraldState
  assertEqual
    "ordinary Herald receives the reference entry's exact successor membership"
    successor
    ( OracleProjection.oracleViewCurrentHeraldMembership
        (OracleProjection.oracleView retiredHeraldProjection)
    )
  assertBool
    "ordinary Herald retains the reference entry's resident process End"
    (h4ProcessEpoch `elem` fmap fst (OracleProjection.projectedEndedProcesses retiredHeraldProjection))
  assertEqual
    "reference retirement leaves the ordinary Herald whole-state valid"
    (Right ())
    (validateHeraldState retiredHeraldState)
  initialStructuralBase <-
    checkedIO
      "begin structural closure from the same checked retirement entry"
      ( StructuralBase.beginStructuralBaseAfterAppliedMembershipAdvance
          []
          positionedHeraldState
          retiredHeraldState
      )
  establishedStructuralBase <-
    establishEmptyStructuralBase
      positionedHeraldState
      preparedHerald
      initialStructuralBase
  preparedClosure <-
    checkedIO
      "complete structural closure from the same checked retirement entry"
      ( RetirementClosure.prepareRetirementClosure
          positionedHeraldState
          retiredHeraldState
          preparedHerald
          establishedStructuralBase
      )
  let closedHeraldState =
        RetirementClosure.commitRetirementClosure preparedClosure
  assertEqual
    "the joined reference-entry schedule installs the successor base"
    RetirementClosure.RetirementClosureApplied
    ( RetirementClosure.retirementClosureDisposition
        (RetirementClosure.preparedRetirementClosureReceipt preparedClosure)
    )
  assertEqual
    "the joined reference-entry successor remains whole-state valid"
    (Right ())
    (validateHeraldState closedHeraldState)
  replayedHerald <-
    checkedIO
      "replay reference retirement against ordinary Herald state"
      (Vertical.prepareReferenceHeraldMembershipAdvance checkedAdvance closedHeraldState)
  assertEqual
    "ordinary Herald recognizes exact reference-entry replay"
    MembershipAdvance.MembershipAdvanceExactDuplicate
    (MembershipAdvance.preparedMembershipAdvanceDisposition replayedHerald)
  assertEqual
    "exact reference-entry replay emits no effects"
    []
    (effectBatchMembers (MembershipAdvance.preparedMembershipAdvanceEffects replayedHerald))
  assertBool
    "exact reference-entry replay preserves the whole ordinary state"
    (MembershipAdvance.commitMembershipAdvance replayedHerald == closedHeraldState)
  replayedClosure <-
    checkedIO
      "replay the joined retirement closure"
      ( RetirementClosure.prepareRetirementClosure
          positionedHeraldState
          closedHeraldState
          replayedHerald
          establishedStructuralBase
      )
  assertEqual
    "the joined closure recognizes exact replay"
    RetirementClosure.RetirementClosureExactReplay
    ( RetirementClosure.retirementClosureDisposition
        (RetirementClosure.preparedRetirementClosureReceipt replayedClosure)
    )
  assertBool
    "the joined closure replay is state-identical"
    (RetirementClosure.commitRetirementClosure replayedClosure == closedHeraldState)
  assertEqual
    "the joined closure replay emits no effects"
    []
    ( effectBatchMembers
        (RetirementClosure.preparedRetirementClosureEffects replayedClosure)
    )

ordinaryHeraldState4 :: IO HeraldState
ordinaryHeraldState4 =
  fmap
    fst
    ( checkedIO
        "initialize matching four-Herald ordinary state"
        ( initialHerald
            (monotonicInstant 0)
            ordinaryHeraldGenesis4
            ordinaryHeraldBootstraps4
            fixtureOracleContacts
            fixtureGeneratorSeedH1
            fixtureApplicationRecoveryConfiguration
            fixturePeerRecoveryConfiguration
        )
    )

ordinaryHeraldGenesis4 :: CheckedHeraldGenesis
ordinaryHeraldGenesis4 =
  checkedEither
    "matching four-Herald genesis"
    ( checkHeraldGenesis
        base
          { deploymentActiveHeralds = members,
            deploymentConfiguredProcesses = relocated : drop 1 configured,
            deploymentOracleGenesis =
              (deploymentOracleGenesis base)
                { oracleGenesisActiveHeralds = members
                }
          }
    )
  where
    base = fixtureDeploymentManifest
    members = checkedOracleActiveHeralds genesis4
    configured = deploymentConfiguredProcesses base
    relocated = case configured of
      first : _ -> first {configuredProcessResidence = h4}
      [] -> error "matching four-Herald genesis: configured catalogue is empty"

ordinaryHeraldBootstraps4 :: CheckedInitialBootstraps
ordinaryHeraldBootstraps4 =
  checkedEither
    "matching four-Herald initial bootstraps"
    ( checkInitialBootstraps
        ordinaryHeraldGenesis4
        ( PrimordialProcessManifest
            (appliedBootstrapManifestId <$> checkedOracleAppliedBootstraps genesis4)
        )
    )

matchingOrdinaryOracleGenesis4 :: CheckedOracleGenesis
matchingOrdinaryOracleGenesis4 =
  checkedEither "matching four-Herald ordinary Oracle genesis" (checkOracleGenesis raw)
  where
    raw =
      oracleGenesis
        (HeraldGenesis.checkedSystemId ordinaryHeraldGenesis4)
        (HeraldGenesis.checkedActiveHeralds ordinaryHeraldGenesis4)
        (HeraldGenesis.checkedCatalogueDigest ordinaryHeraldGenesis4)
        (checkedOraclePredefinedDescriptors genesis4)
        (HeraldGenesis.checkedInitialBootstraps ordinaryHeraldBootstraps4)
        (HeraldGenesis.checkedConfigurationDigest ordinaryHeraldGenesis4)
        (HeraldGenesis.checkedInitialTopologyProjection ordinaryHeraldBootstraps4)
        (HeraldGenesis.checkedInitialProjectionDigest ordinaryHeraldBootstraps4)
        (checkedOracleRaftVoterBindings genesis4)
        (checkedOracleRaftNativeConfiguration genesis4)
        (checkedOracleRaftConfigurationDigest genesis4)

-- The independent failure-reference entries at control indices 1-3 are not
-- reused as live Oracle values. Three real committed rejected entries therefore
-- position the ordinary cursor at the same prefix without projecting unrelated
-- semantic work.
ordinaryPrefixEntries :: [CanonicalAppliedOracleEntry]
ordinaryPrefixEntries = snd (foldl commitRejected (oracle0, []) [1 .. 3])
  where
    oracle0 = checkedEither "initialize matching ordinary Oracle" (initialOracle matchingOrdinaryOracleGenesis4)
    bootstrap = case drop 1 (HeraldGenesis.checkedConfiguredProcessBootstraps ordinaryHeraldGenesis4) of
      candidate : _ -> candidate
      [] -> error "matching four-Herald ordinary Oracle: missing configured bootstrap"

    commitRejected (predecessor, retained) sequenceNumber =
      case checkedEither
        "commit ordinary rejected prefix entry"
        ( stepOracle
            ( oracleEnvelope
                (oracleClientRequestId h1 sequenceNumber)
                (Just (controlIndex 99))
                h1
                (startProcessEpochCommand (processStart (configuredProcessBootstrapProcessId bootstrap) (configuredProcessBootstrapProcessEpochId bootstrap) (configuredProcessBootstrapResidence bootstrap)))
            )
            predecessor
        ) of
        (successorState, OracleCommitted _, effects) -> case oracleEffects effects of
          [EmitAppliedOracleEntry entry] ->
            (successorState, retained <> [canonicalizeAppliedOracleEntry entry])
          other -> error ("ordinary rejected prefix entry emitted unexpected effects: " <> show other)
        (_, other, _) ->
          error ("ordinary rejected prefix entry did not commit: " <> show other)

positionOrdinaryPrefix ::
  [CanonicalAppliedOracleEntry] ->
  HeraldState ->
  IO HeraldState
positionOrdinaryPrefix entries initial = foldM positionOne initial entries
  where
    positionOne predecessor entry = do
      preparedProjection <-
        checkedIO
          "project ordinary prefix entry"
          (OracleProjection.prepareAppliedEntry entry (startupOracleProjectionState predecessor))
      preparedClient <-
        checkedIO
          "advance ordinary Oracle-client prefix"
          (OracleClient.prepareCursorAdvance entry (startupOracleClientState predecessor))
      preparedProgress <-
        checkedIO
          "advance ordinary structural-control prefix"
          ( GraphProgress.prepareStructuralControlProgress
              (appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry))
              (startupStructuralProgressState predecessor)
          )
      let (progress, _) = GraphProgress.commitStructuralControlProgress preparedProgress
      pure
        ( replaceStartupStructuralProgressState progress
            . replaceStartupOracleClientState (OracleClient.commitCursorAdvance preparedClient)
            . replaceStartupOracleProjectionState (OracleProjection.commitAppliedEntry preparedProjection)
            $ predecessor
        )

bindOrdinaryOracle :: HeraldState -> IO HeraldState
bindOrdinaryOracle state = do
  let client = startupOracleClientState state
  attempt <- case OracleClient.oracleClientActions client of
    [ConnectAndHelloOracle current _] -> pure current
    actions -> assertFailure ("expected one initial Oracle connection attempt, got " <> show actions)
  let nodeId = oracleContactNode (oracleConnectAttemptContact attempt)
      acceptance =
        oracleHelloAcceptance
          nodeId
          (oracleObservedTerm 1)
          (controlIndex 0)
          (Just nodeId)
          True
  prepared <-
    checkedIO
      "bind matching ordinary Oracle client"
      (OracleClient.prepareClientIngress (OracleHelloReceived attempt acceptance) client)
  pure (replaceStartupOracleClientState (OracleClient.commitClientIngress prepared) state)

caseOpenAliasing :: Assertion
caseOpenAliasing = do
  let oracle0 = initialReferenceOracle genesis4
      generation = heraldMembershipGenerationId (referenceOracleCurrentMembership oracle0)
      (oracle1, entry1, result1) = accepted h1 1 (referenceOpenHeraldFailureProbeCommand h4 generation) oracle0
      probe1 = expectOpened result1
      (_, aliasEntry, result2) = accepted h2 1 (referenceOpenHeraldFailureProbeCommand h4 generation) oracle1
      probe2 = expectOpened result2
      (_, conflictingEntry, _) = accepted h2 1 (referenceOpenHeraldFailureProbeCommand h4 generation) oracle0
      node0 = node h1 oracle0
  assertEqual "simultaneous opens alias" probe1 probe2
  let (node1, openEffects) = checkedEither "project Open" (Vertical.projectOracleEntry (monotonicInstant 0) entry1 node0)
      attempt = expectDirectProbe openEffects
      (node2, _) = checkedEither "project alias receipt" (Vertical.projectOracleEntry (monotonicInstant 1) aliasEntry node1)
      (reported, firstReport) = Vertical.freshReportCommand attempt ReferenceProbeReachable node1
  assertEqual "direct attempt retains probe" probe1 (Vertical.directProbeAttemptProbe attempt)
  assertEqual "direct attempt retains target" h4 (Vertical.directProbeAttemptTarget attempt)
  assertEqual "direct attempt retains membership generation" generation (Vertical.directProbeAttemptMembershipGeneration attempt)
  assertEqual "pre-Open reachability cannot be reused" Vertical.FreshReportNotProjected (snd (Vertical.freshReportCommand attempt ReferenceProbeReachable node0))
  assertBool "projected Open admits a fresh report" (isFreshCommand firstReport)
  assertEqual "equal local repeat deduplicates" Vertical.FreshReportDuplicate (snd (Vertical.freshReportCommand attempt ReferenceProbeReachable reported))
  assertEqual "conflicting local repeat is inert" (Vertical.FreshReportConflict ReferenceProbeReachable ReferenceProbeUnreachable) (snd (Vertical.freshReportCommand attempt ReferenceProbeUnreachable reported))
  assertEqual "old exact entry duplicate is inert" (Right (node2, [])) (Vertical.projectOracleEntry (monotonicInstant 2) entry1 node2)
  assertEqual "contradictory old-index replay is explicit" (Left (Vertical.VerticalProjectionConflict (controlIndex 1))) (Vertical.projectOracleEntry (monotonicInstant 2) conflictingEntry node2)

caseDismissCooldown :: Assertion
caseDismissCooldown = do
  let oracle0 = initialReferenceOracle genesis4
      generation = heraldMembershipGenerationId (referenceOracleCurrentMembership oracle0)
      (oracle1, openEntry, openResult) = accepted h1 1 (referenceOpenHeraldFailureProbeCommand h4 generation) oracle0
      probe = expectOpened openResult
      (oracle2, report1, _) = accepted h1 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeReachable) oracle1
      (oracle3, report2, secondReportResult) = accepted h2 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeReachable) oracle2
      intention = expectIntention secondReportResult
      (_, dismissEntry, _) = accepted h1 3 (referenceTerminalIntentionCommand intention) oracle3
      (disconnected, disconnectEffects) = Vertical.observeDisconnected (monotonicInstant 0) h4 (node h1 oracle0)
      (p1, _) = checkedEither "project Open" (Vertical.projectOracleEntry (monotonicInstant 1) openEntry disconnected)
      (p2, _) = checkedEither "project first report" (Vertical.projectOracleEntry (monotonicInstant 2) report1 p1)
      (p3, _) = checkedEither "project second report" (Vertical.projectOracleEntry (monotonicInstant 3) report2 p2)
      (dismissed, effects) = checkedEither "project Dismiss" (Vertical.projectOracleEntry (monotonicInstant 10) dismissEntry p3)
  token <- case [token | Vertical.CooldownStarted token _ <- effects] of
    [found] -> pure found
    other -> assertFailure ("expected one cooldown, got " <> show other) >> error "unreachable"
  oldAttempt <- case [attempt | Vertical.RecoveryTimerStarted attempt _ <- disconnectEffects] of
    [found] -> pure found
    other -> assertFailure ("expected initial recovery timer, got " <> show other) >> error "unreachable"
  let (_, oldTimerEffects) = Vertical.observeRecoveryTimer oldAttempt (TimerFired (monotonicInstant 100)) dismissed
      (_, repeatedDisconnectEffects) = Vertical.observeDisconnected (monotonicInstant 11) h4 dismissed
  assertEqual "old recovery timer is inert after Dismiss" [] oldTimerEffects
  assertEqual "disconnect cannot bypass active cooldown" [] repeatedDisconnectEffects
  let (staleState, staleEffects) = Vertical.observeCooldown token (monotonicInstant 19) dismissed
      (rearmed, rearmEffects) = Vertical.observeCooldown token (monotonicInstant 20) staleState
      (_, duplicateEffects) = Vertical.observeCooldown token (monotonicInstant 21) rearmed
  assertBool "early absolute observation does not begin recovery" (any isEarly staleEffects)
  assertBool "current cooldown expiry alone begins recovery" (any isRecovery rearmEffects)
  assertEqual "old cooldown token is inert" [Vertical.CooldownObservationStale] duplicateEffects

caseRetire :: Assertion
caseRetire = do
  let (oracle, probe) = majority ReferenceProbeUnreachable genesis4
      intention = checkedMaybe "retirement intention" (referenceOracleTerminalIntention probe oracle)
      (retiredOracle, _, retiredResult) = accepted h1 4 (referenceTerminalIntentionCommand intention) oracle
      successor = expectRetired retiredResult
  assertBool "membership generation advances" (successor /= heraldMembershipGenerationId (referenceOracleCurrentMembership (initialReferenceOracle genesis4)))
  assertEqual "target is removed" [h1, h2, h3] (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (referenceOracleCurrentMembership retiredOracle)))

casePhysicalEquivalence :: Assertion
casePhysicalEquivalence = do
  let stoppedTrace = PhysicalStopped
      partitionTrace = PhysicalStillRunningPartition
      (stopped, stoppedEvidence, stoppedNodes) = runPhysicalRetirement stoppedTrace
      (isolatedButRunning, partitionEvidence, partitionNodes) = runPhysicalRetirement partitionTrace
      stoppedMembership = referenceOracleCurrentMembership stopped
      partitionMembership = referenceOracleCurrentMembership isolatedButRunning
  assertBool "outer physical traces remain distinct" (stoppedTrace /= partitionTrace)
  assertEqual "physical schedules derive equal Oracle probe evidence" stoppedEvidence partitionEvidence
  assertEqual
    "erasing physical detail yields the same canonical retired state"
    (referenceOracleStateCanonicalBytes stopped)
    (referenceOracleStateCanonicalBytes isolatedButRunning)
  assertEqual "canonical state digest agrees" (referenceOracleStateDigest stopped) (referenceOracleStateDigest isolatedButRunning)
  mapM_
    ( \projected -> do
        assertEqual "stopped schedule projects the retired membership" stoppedMembership (Projection.projectedMembership (Vertical.projectionState projected))
        assertBool "stopped schedule projects the resident process End" (Projection.projectedProcessEnd h4ProcessEpoch (Vertical.projectionState projected) /= Nothing)
    )
    stoppedNodes
  mapM_
    ( \projected -> do
        assertEqual "partition schedule projects the retired membership" partitionMembership (Projection.projectedMembership (Vertical.projectionState projected))
        assertBool "partition schedule projects the resident process End" (Projection.projectedProcessEnd h4ProcessEpoch (Vertical.projectionState projected) /= Nothing)
    )
    partitionNodes

caseSafety :: Assertion
caseSafety = do
  let oracle0 = initialReferenceOracle genesis4
      generation = heraldMembershipGenerationId (referenceOracleCurrentMembership oracle0)
      (oracle1, openEntry, openResult) = accepted h1 1 (referenceOpenHeraldFailureProbeCommand h4 generation) oracle0
      probe = expectOpened openResult
      (oracle2, _, _) = accepted h1 2 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeUnreachable) oracle1
      node0 = node h2 oracle0
      nonVoter0 = node h4 oracle0
  assertEqual "minority has no terminal intention" Nothing (referenceOracleTerminalIntention probe oracle2)
  let (projected, openEffects) = checkedEither "project Open" (Vertical.projectOracleEntry (monotonicInstant 0) openEntry node0)
      attempt = expectDirectProbe openEffects
      (_, nonVoterEffects) = checkedEither "project Open at non-voter" (Vertical.projectOracleEntry (monotonicInstant 0) openEntry nonVoter0)
  assertEqual "stale local projection cannot report" Vertical.FreshReportNotProjected (snd (Vertical.freshReportCommand attempt ReferenceProbeUnreachable node0))
  assertBool "fresh projection can report" (isFreshCommand (snd (Vertical.freshReportCommand attempt ReferenceProbeUnreachable projected)))
  assertEqual "non-voter host does not begin a direct probe" [] [candidate | candidate@Vertical.BeginFreshDirectProbe {} <- nonVoterEffects]
  assertRejected "non-voter reporter" h4 1 (referenceReportHeraldFailureProbeCommand probe ReferenceProbeUnreachable) oracle1
  assertRejected "voter target" h1 9 (referenceOpenHeraldFailureProbeCommand h1 generation) oracle1

caseSupersedes :: Assertion
caseSupersedes = do
  let oracle0 = initialReferenceOracle genesis5
      generation = heraldMembershipGenerationId (referenceOracleCurrentMembership oracle0)
      (oracle1, open4, openResult4) = accepted h1 1 (referenceOpenHeraldFailureProbeCommand h4 generation) oracle0
      probe4 = expectOpened openResult4
      (oracle2, open5, openResult5) = accepted h2 1 (referenceOpenHeraldFailureProbeCommand h5 generation) oracle1
      probe5 = expectOpened openResult5
      (oracle3, report1, _) = accepted h1 2 (referenceReportHeraldFailureProbeCommand probe4 ReferenceProbeUnreachable) oracle2
      (oracle4, report2, secondReportResult) = accepted h2 2 (referenceReportHeraldFailureProbeCommand probe4 ReferenceProbeUnreachable) oracle3
      intention = expectIntention secondReportResult
      (oracle5, retirement, _) = accepted h1 3 (referenceTerminalIntentionCommand intention) oracle4
  case referenceOracleFailureProbe probe5 oracle5 of
    Just (ReferenceFailureProbeView _ _ _ _ _ (Just _)) -> pure ()
    other -> assertFailure ("other genesis probe was not terminally superseded: " <> show other)
  let (disconnected5, _) = Vertical.observeDisconnected (monotonicInstant 0) h5 (node h1 oracle0)
      (p1, _) = checkedEither "project first Open" (Vertical.projectOracleEntry (monotonicInstant 1) open4 disconnected5)
      (p2, open5Effects) = checkedEither "project second Open" (Vertical.projectOracleEntry (monotonicInstant 2) open5 p1)
      attempt5 = expectDirectProbe open5Effects
      (p3, _) = checkedEither "project first report" (Vertical.projectOracleEntry (monotonicInstant 3) report1 p2)
      (p4, _) = checkedEither "project second report" (Vertical.projectOracleEntry (monotonicInstant 4) report2 p3)
      (retired, effects) = checkedEither "project retirement" (Vertical.projectOracleEntry (monotonicInstant 5) retirement p4)
      (_, retiredReconnectEffects) = Vertical.observeDisconnected (monotonicInstant 6) h4 retired
  assertBool "superseded disconnected target starts fresh ordinary recovery" (any isRecovery effects)
  assertEqual "retired target cannot restart recovery" [] retiredReconnectEffects
  assertEqual "old superseded probe is no longer reportable" Vertical.FreshReportNotProjected (snd (Vertical.freshReportCommand attempt5 ReferenceProbeUnreachable retired))
  assertEqual "successor membership is projected" (referenceOracleCurrentMembership oracle5) (Projection.projectedMembership (Vertical.projectionState retired))
  assertBool "target-resident process End is retained" (Projection.projectedProcessEnd h4ProcessEpoch (Vertical.projectionState retired) /= Nothing)

majority :: ReferenceProbeResult -> CheckedOracleGenesis -> (ReferenceOracleState, HeraldFailureProbeId)
majority result genesis =
  let oracle0 = initialReferenceOracle genesis
      generation = heraldMembershipGenerationId (referenceOracleCurrentMembership oracle0)
      (oracle1, _, openResult) = accepted h1 1 (referenceOpenHeraldFailureProbeCommand h4 generation) oracle0
      probe = expectOpened openResult
      (oracle2, _, _) = accepted h1 2 (referenceReportHeraldFailureProbeCommand probe result) oracle1
      (oracle3, _, _) = accepted h2 2 (referenceReportHeraldFailureProbeCommand probe result) oracle2
   in (oracle3, probe)

data PhysicalTrace = PhysicalStopped | PhysicalStillRunningPartition
  deriving stock (Eq, Show)

runPhysicalRetirement ::
  PhysicalTrace ->
  (ReferenceOracleState, [ReferenceOracleCommand], [Vertical.State])
runPhysicalRetirement trace =
  let oracle0 = initialReferenceOracle genesis4
      (h1BeforeOpen, h1Open) = drivePhysicalSuspicion trace h1 oracle0
      (h2BeforeOpen, h2Open) = drivePhysicalSuspicion trace h2 oracle0
      (oracle1, openEntry, openResult) = accepted h1 1 h1Open oracle0
      probe = expectOpened openResult
      (oracle2, aliasEntry, aliasResult) = accepted h2 1 h2Open oracle1
      aliasProbe = expectOpened aliasResult
      (h1Opened, h1OpenEffects) = projectTwo "H1 Open" openEntry aliasEntry h1BeforeOpen
      (h2Opened, h2OpenEffects) = projectTwo "H2 Open" openEntry aliasEntry h2BeforeOpen
      h1Attempt = expectDirectProbe h1OpenEffects
      h2Attempt = expectDirectProbe h2OpenEffects
      (h1Reported, h1ReportDisposition) = Vertical.freshReportCommand h1Attempt ReferenceProbeUnreachable h1Opened
      (h2Reported, h2ReportDisposition) = Vertical.freshReportCommand h2Attempt ReferenceProbeUnreachable h2Opened
      h1Report = expectFreshReport h1ReportDisposition
      h2Report = expectFreshReport h2ReportDisposition
      (oracle3, reportEntry1, _) = accepted h1 2 h1Report oracle2
      (oracle4, reportEntry2, secondReportResult) = accepted h2 2 h2Report oracle3
      intention = expectIntention secondReportResult
      terminal = referenceTerminalIntentionCommand intention
      (retired, retirementEntry, _) = accepted h1 3 terminal oracle4
      h1Projected = projectThree "H1 reports and retirement" reportEntry1 reportEntry2 retirementEntry h1Reported
      h2Projected = projectThree "H2 reports and retirement" reportEntry1 reportEntry2 retirementEntry h2Reported
   in if probe == aliasProbe
        then (retired, [h1Open, h2Open, h1Report, h2Report, terminal], [h1Projected, h2Projected])
        else error "physical schedule Opens did not alias"

drivePhysicalSuspicion ::
  PhysicalTrace ->
  HeraldEpoch ->
  ReferenceOracleState ->
  (Vertical.State, ReferenceOracleCommand)
drivePhysicalSuspicion trace local oracle = case trace of
  PhysicalStopped ->
    let (disconnected, effects) = Vertical.observeDisconnected (monotonicInstant 0) h4 (node local oracle)
        attempt = expectRecoveryTimer effects
        (suspected, expiryEffects) = Vertical.observeRecoveryTimer attempt (TimerFired (monotonicInstant 10)) disconnected
     in (suspected, expectOpenProbe expiryEffects)
  PhysicalStillRunningPartition ->
    let (connected, _) = Vertical.observeConnected h4 (node local oracle)
        (transientLoss, firstLossEffects) = Vertical.observeDisconnected (monotonicInstant 1) h4 connected
        firstAttempt = expectRecoveryTimer firstLossEffects
        (reconnected, cancellationEffects) = Vertical.observeConnected h4 transientLoss
        (partitioned, finalLossEffects) = Vertical.observeDisconnected (monotonicInstant 3) h4 reconnected
        finalAttempt = expectRecoveryTimer finalLossEffects
        (suspected, expiryEffects) = Vertical.observeRecoveryTimer finalAttempt (TimerFired (monotonicInstant 13)) partitioned
     in if any (== Vertical.RecoveryTimerCancelled firstAttempt) cancellationEffects
          then (suspected, expectOpenProbe expiryEffects)
          else error "partition schedule did not cancel its transient recovery"

projectTwo ::
  String ->
  ReferenceAppliedOracleEntry ->
  ReferenceAppliedOracleEntry ->
  Vertical.State ->
  (Vertical.State, [Vertical.VerticalEffect])
projectTwo label first second initial =
  let (afterFirst, firstEffects) = checkedEither (label <> " first entry") (Vertical.projectOracleEntry (monotonicInstant 20) first initial)
      (afterSecond, secondEffects) = checkedEither (label <> " second entry") (Vertical.projectOracleEntry (monotonicInstant 21) second afterFirst)
   in (afterSecond, firstEffects <> secondEffects)

projectThree ::
  String ->
  ReferenceAppliedOracleEntry ->
  ReferenceAppliedOracleEntry ->
  ReferenceAppliedOracleEntry ->
  Vertical.State ->
  Vertical.State
projectThree label first second third initial =
  let (afterFirst, _) = checkedEither (label <> " first entry") (Vertical.projectOracleEntry (monotonicInstant 22) first initial)
      (afterSecond, _) = checkedEither (label <> " second entry") (Vertical.projectOracleEntry (monotonicInstant 23) second afterFirst)
      (afterThird, _) = checkedEither (label <> " third entry") (Vertical.projectOracleEntry (monotonicInstant 24) third afterSecond)
   in afterThird

accepted home sequenceNumber command oracle =
  case submitReferenceOracle (referenceOracleEnvelope (oracleClientRequestId home sequenceNumber) Nothing home command) oracle of
    (successor, ReferenceSubmissionCommitted receipt entry) -> case referenceOracleReceiptResult receipt of
      ReferenceOracleAccepted result -> (successor, entry, result)
      ReferenceOracleRejected problem -> error ("unexpected rejection: " <> show problem)
    (_, other) -> error ("unexpected submission: " <> show other)

accepted ::
  HeraldEpoch ->
  Word64 ->
  ReferenceOracleCommand ->
  ReferenceOracleState ->
  (ReferenceOracleState, ReferenceAppliedOracleEntry, ReferenceCommandResult)

assertRejected label home sequenceNumber command oracle =
  case submitReferenceOracle (referenceOracleEnvelope (oracleClientRequestId home sequenceNumber) Nothing home command) oracle of
    (_, ReferenceSubmissionCommitted receipt _) -> case referenceOracleReceiptResult receipt of
      ReferenceOracleRejected _ -> pure ()
      other -> assertFailure (label <> ": expected rejection, got " <> show other)
    (_, other) -> assertFailure (label <> ": expected committed rejection, got " <> show other)

assertRejected ::
  String ->
  HeraldEpoch ->
  Word64 ->
  ReferenceOracleCommand ->
  ReferenceOracleState ->
  Assertion

node local oracle =
  Vertical.initialState
    local
    (Set.fromList (raftVoterBindingHeraldEpoch <$> checkedOracleRaftVoterBindings genesis4))
    (referenceOracleCurrentMembership oracle)
    recoveryConfiguration
    cooldownConfiguration

node :: HeraldEpoch -> ReferenceOracleState -> Vertical.State

recoveryConfiguration = checkedEither "peer recovery configuration" (checkPeerRecoveryConfiguration 10)
cooldownConfiguration = checkedEither "cooldown configuration" (Vertical.checkCooldownConfiguration 10)

recoveryConfiguration :: PeerRecoveryConfiguration
cooldownConfiguration :: Vertical.CooldownConfiguration
genesis4, genesis5 :: CheckedOracleGenesis
genesis4 = extendGenesis [member 0x44]
genesis5 = extendGenesis [member 0x44, member 0x55]

extendGenesis extras = checkedEither "extended non-voter genesis" (checkOracleGenesis raw)
  where
    base = fixtureCheckedOracleGenesis
    members = checkedOracleActiveHeralds base <> extras
    configured = relocatedConfigured h4 (headChecked "configured bootstrap" (HeraldGenesis.checkedConfiguredProcessBootstraps fixtureStep14CheckedGenesis))
    applied =
      checkedEither
        "materialized relocated bootstrap"
        (materializeConfiguredProcessBootstrap (controlIndex 0) (genesisPredefinedOccurrenceSet (checkedOracleSystemId base)) configured)
    appliedBootstraps = applied : drop 1 (checkedOracleAppliedBootstraps base)
    topology =
      checkedEither
        "extended initial topology"
        ( checkInitialTopologyProjection
            (checkedOracleSystemId base)
            members
            appliedBootstraps
            (InitialTopologyManifest [])
        )
    raw =
      oracleGenesis
        (checkedOracleSystemId base)
        members
        (checkedOracleCatalogueDigest base)
        (checkedOraclePredefinedDescriptors base)
        appliedBootstraps
        (checkedOracleConfigurationDigest base)
        topology
        (deriveInitialProjectionDigest appliedBootstraps topology)
        (checkedOracleRaftVoterBindings base)
        (checkedOracleRaftNativeConfiguration base)
        (checkedOracleRaftConfigurationDigest base)

extendGenesis :: [HeraldMember] -> CheckedOracleGenesis

member byte = HeraldMember (checkedEither "Herald ID" (mkHeraldId bytes)) (checkedEither "Herald epoch" (mkHeraldEpoch bytes))
  where
    bytes = ByteString.replicate 32 byte

member :: Word8 -> HeraldMember
h1, h2, h3, h4, h5 :: HeraldEpoch
h1 = voterEpoch 0
h2 = voterEpoch 1
h3 = voterEpoch 2
h4 = heraldMemberEpoch (member 0x44)
h5 = heraldMemberEpoch (member 0x55)

h4ProcessEpoch =
  appliedProcessEpochId
    (headChecked "H4 applied bootstrap" (checkedOracleAppliedBootstraps genesis4))

h4ProcessEpoch :: ProcessEpochId

relocatedConfigured residence configured =
  checkedMaybe
    "relocated configured bootstrap"
    ( mkConfiguredProcessBootstrap
        (configuredProcessBootstrapManifestId configured)
        (configuredProcessBootstrapProcessId configured)
        (configuredProcessBootstrapProcessEpochId configured)
        residence
        (configuredProcessBootstrapRoots configured)
    )

relocatedConfigured ::
  HeraldEpoch -> ConfiguredProcessBootstrap -> ConfiguredProcessBootstrap

headChecked label values = case values of
  value : _ -> value
  [] -> error (label <> ": absent")

headChecked :: String -> [value] -> value

voterEpoch offset = case drop offset (checkedOracleRaftVoterBindings fixtureCheckedOracleGenesis) of
  binding : _ -> raftVoterBindingHeraldEpoch binding
  [] -> error "fixture is missing a voter binding"

voterEpoch :: Int -> HeraldEpoch

isEarly (Vertical.CooldownObservationEarly _ _) = True
isEarly _ = False

isEarly :: Vertical.VerticalEffect -> Bool

isRecovery (Vertical.RecoveryTimerStarted _ _) = True
isRecovery _ = False

isRecovery :: Vertical.VerticalEffect -> Bool

isFreshCommand (Vertical.FreshReportCommand _) = True
isFreshCommand _ = False

isFreshCommand :: Vertical.FreshReportDisposition -> Bool

expectDirectProbe effects = case [attempt | Vertical.BeginFreshDirectProbe attempt <- effects] of
  [attempt] -> attempt
  other -> error ("expected one direct-probe attempt, got " <> show other)

expectDirectProbe :: [Vertical.VerticalEffect] -> Vertical.DirectProbeAttempt
expectRecoveryTimer :: [Vertical.VerticalEffect] -> TimerAttempt
expectRecoveryTimer effects = case [attempt | Vertical.RecoveryTimerStarted attempt _ <- effects] of
  [attempt] -> attempt
  other -> error ("expected one recovery timer, got " <> show other)

expectOpenProbe :: [Vertical.VerticalEffect] -> ReferenceOracleCommand
expectOpenProbe effects = case [command | Vertical.OpenProbeRequested command <- effects] of
  [command] -> command
  other -> error ("expected one Open-probe command, got " <> show other)

expectFreshReport :: Vertical.FreshReportDisposition -> ReferenceOracleCommand
expectFreshReport disposition = case disposition of
  Vertical.FreshReportCommand command -> command
  other -> error ("expected one fresh Report command, got " <> show other)

checkedEither label = either (error . ((label <> ": ") <>) . show) id
checkedMaybe label = maybe (error (label <> ": absent")) id
checkedIO label = either (assertFailure . ((label <> ": ") <>) . show) pure

checkedEither :: (Show problem) => String -> Either problem value -> value
checkedMaybe :: String -> Maybe value -> value
checkedIO :: (Show problem) => String -> Either problem value -> IO value

expectOpened result = case result of
  ReferenceProbeOpened probe -> probe
  other -> error ("expected ProbeOpened, got " <> show other)

expectOpened :: ReferenceCommandResult -> HeraldFailureProbeId

expectIntention result = case result of
  ReferenceProbeReportRecorded _ (Just intention) -> intention
  other -> error ("expected report with intention, got " <> show other)

expectIntention :: ReferenceCommandResult -> ReferenceTerminalIntention

expectRetired result = case result of
  ReferenceHeraldRetiredResult _ successor -> successor
  other -> error ("expected Retired, got " <> show other)

expectRetired :: ReferenceCommandResult -> HeraldMembershipGenerationId
