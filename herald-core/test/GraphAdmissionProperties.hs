{-# LANGUAGE OverloadedStrings #-}

module GraphAdmissionProperties (tests) where

import Data.ByteString qualified as BS
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Eclips.Domain.Graph (VertexId (DeltaVertex))
import Eclips.Domain.Identity
import Eclips.Domain.Membership
import Eclips.Domain.Startup (InitialProjectionDigest, mkInitialProjectionDigest)
import Eclips.Domain.Structural
import Eclips.Domain.StructuralConsequence (processEndCause)
import Eclips.Domain.Topology
import Eclips.Herald.Graph.Progress
import Eclips.Herald.Graph.Protocol
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Join.Bootstrap qualified as Bootstrap
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Oracle.Admission
import Eclips.Oracle.Command
import Eclips.Oracle.Effect (OracleStepOutcome (..))
import Eclips.Oracle.Genesis (checkedOracleSystemId)
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Receipt
import Eclips.Oracle.State
import Eclips.Oracle.Transition
import GenesisFixtures qualified as Fixtures
import StructuralProgressProperties (assertIndependentProgressEvidence, assertInstalledCoverageAgainstReference, assertPortableProgressImport)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (conjoin, forAll, shuffle, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "Herald admission structural bases"
    [ testCase "idle founder establishes its first real seal through its old-member ceremony" caseIdleSeal,
      testCase "old member installs a certificate without a second cut barrier" (caseInstall False),
      testCase "applicant imports old cuts without own evidence and becomes active at its base" (caseInstall True),
      testCase "a later applicant imports an earlier activation without gaining membership" caseHistoricalAdmission,
      testCase "admission-base acknowledgements from active members are exact stale evidence" caseAdmissionAcknowledgement,
      testProperty "report order preserves the exact recipe and installed admission cut"
        $ forAll (shuffle oldMembers)
        $ \order ->
          let (_, leftProgress, leftCut) = installedFixture False oldMembers
              (_, rightProgress, rightCut) = installedFixture False order
           in conjoin [leftCut === rightCut, structuralAppliedVector leftProgress === structuralAppliedVector rightProgress]
    ]

caseIdleSeal :: Assertion
caseIdleSeal = do
  let local = NE.head (heraldMembershipGenerationActiveHeraldEpochs (oracleCurrentMembership initialOracleState))
      generation = checked (genesisHeraldMembershipGeneration system (local :| []))
      vector = emptyStructuralVersionVector generation
      initial = checked (initialStructuralProgressState system local generation initialDigest (Reconciliation.emptyStructuralAppliedState vector))
      state = fst (commitStructuralControlProgress (checked (prepareStructuralControlProgress (controlIndex 1) initial)))
      admission = checked (deriveHeraldAdmissionId (controlIndex 1))
  case checked (prepareTopologyCutProposal (views local) (topologyControlCheckpoint (controlIndex 1) "ordinary") state) of
    TopologyCutProposalUnavailable -> pure ()
    _ -> assertFailure "ordinary unchanged control prefix unexpectedly proposed a cut"
  case checked (prepareTopologyCutProposal (views local) (topologyJoinSealCheckpoint admission (controlIndex 1) "join") state) of
    TopologyCutProposalReady prepared -> do
      let proposed = commitTopologyCutProposal prepared
          established = checked (prepareTopologyCutEstablishment proposed)
          installed = commitTopologyCutEstablishment established
      assertEqual "single old member's real acceptance establishes the seal" 1 (length (structuralInstalledCutIds installed))
      assertEqual "sealed state invariant" (Right ()) (validateStructuralProgressState installed)
    _ -> assertFailure "join checkpoint failed to establish a finite idle seal"

caseInstall :: Bool -> Assertion
caseInstall observer = do
  let (readyRecord, certificate, recipe, contribution, before, local, oldCut) = fixture observer oldMembers
      prepared = checked (prepareHeraldJoinBaseCandidate readyRecord recipe contribution (views local) Graph.emptyState before)
      (graph, progress, cut) = installedFixture observer oldMembers
      snapshot = checked (structuralProjectionAtInstalledCut (views local) (deriveTopologyCutId cut) progress)
      oldSnapshot = checked (structuralProjectionAtInstalledCut (views local) (deriveTopologyCutId oldCut) progress)
      expectedVertices = [DeltaVertex delta | (_, delta, _) <- Bootstrap.contributionViews contribution]
      replay = checked (prepareMembershipAdmissionBaseInstallation certificate recipe contribution (views local) graph progress)
      (replayedGraph, replayedProgress) = commitMembershipAdmissionBaseInstallation replay
      process = checked (mkProcessEpochId (BS.replicate 32 0x73))
      oldCause = checked (processEndCause process (controlIndex 1))
      activationCause = checked (processEndCause process (admissionRecordChangedIndex certificate))
  assertEqual "candidate contains the seal's exact recipe" (heraldJoinBaseRecipeDigestBytes recipe) (preparedHeraldJoinBaseCandidateDigest prepared)
  assertEqual "contribution creates no edge" [] (Graph.graphEdges graph)
  assertEqual "admission never mutates the genesis baseline" [] (Graph.graphBaselineVertices graph)
  assertEqual "exactly six private destinations appear" (Set.fromList expectedVertices) (Set.fromList (Graph.graphVertices graph))
  assertEqual "old cut has no admission destinations" [] (Reconciliation.structuralProjectionSnapshotAdmissionVertices oldSnapshot)
  assertEqual "activation cut includes the exact contribution" (Set.fromList expectedVertices) (Set.fromList (Reconciliation.structuralProjectionSnapshotAdmissionVertices snapshot))
  assertEqual "new source is initially empty" (Just emptyStructuralPrefix) (structuralVersionVectorComponent applicant (structuralAppliedVector progress))
  assertEqual "activation opens the observer role" False (structuralProgressIsJoiningObserver progress)
  assertEqual "complete installed owner invariant" (Right ()) (validateStructuralProgressState progress)
  assertBool "exact certificate retry preserves graph and progress" (graph == replayedGraph && progress == replayedProgress)
  mapM_
    ( \state -> do
        assertEqual "admission and replay retain earliest coverage in the old seal" (Just (deriveTopologyCutId oldCut)) (installedCutCoveringCause oldCause state)
        assertEqual "activation first covers its new control prefix" (Just (deriveTopologyCutId cut)) (installedCutCoveringCause activationCause state)
        assertEqual "the admission base is the current covering cut" (Just (deriveTopologyCutId cut)) (currentInstalledCutCoveringCause oldCause state)
    )
    [progress, replayedProgress]
  assertBool "a pending seal is insufficient for activation" (case prepareMembershipAdmissionBaseInstallation readyRecord recipe contribution (views local) Graph.emptyState before of Left StructuralProgressAdmissionCertificateMismatch -> True; _ -> False)
  mapM_ (assertInstalledCoverageAgainstReference [] []) [before, progress, replayedProgress]
  if observer
    then do
      assertEqual "applicant has no old-member report" [] (structuralReportEntries before)
      let retained = fromJust (lookupInstalledTopologyCut (deriveTopologyCutId oldCut) before)
      assertEqual "applicant has no old acceptance" Nothing (installedTopologyCutOwnAcceptance retained)
      assertEqual "applicant has no old acknowledgement" Nothing (installedTopologyCutOwnAcknowledgement retained)
    else pure ()

caseAdmissionAcknowledgement :: Assertion
caseAdmissionAcknowledgement = do
  let (_, progress, cut) = installedFixture False oldMembers
      generation = structuralProgressMembershipGenerationId progress
      cutId = deriveTopologyCutId cut
  mapM_
    ( \reporter -> do
        let acknowledgement = topologyCutEstablishedAck cutId reporter generation
            prepared = checked (prepareTopologyCutEstablishedAck acknowledgement progress)
        assertEqual "installed admission base needs no acknowledgement collection" TopologyCutAckStale (preparedTopologyCutAckClassification prepared)
        assertEqual "exact acknowledgement is inert" progress (commitTopologyCutEstablishedAck prepared)
    )
    (applicant : oldMembers)

caseHistoricalAdmission :: Assertion
caseHistoricalAdmission = do
  let future = checked (mkHeraldEpoch (BS.replicate 32 0xf1))
      (_, certificate, recipe, contribution, before, _, _) = fixtureAt future True oldMembers
      ahead = fst (commitStructuralControlProgress (checked (prepareStructuralControlProgress (controlIndex 100) before)))
      prepared = checked (prepareMembershipAdmissionBaseInstallation certificate recipe contribution (views future) Graph.emptyState ahead)
      (_, imported) = commitMembershipAdmissionBaseInstallation prepared
      installed = fromJust (lookupInstalledTopologyCut (deriveTopologyCutId (preparedMembershipAdmissionBaseCut prepared)) imported)
      process = checked (mkProcessEpochId (BS.replicate 32 0x73))
      activationCause = checked (processEndCause process (admissionRecordChangedIndex certificate))
      unappliedToCutCause = checked (processEndCause process (controlIndex 100))
  assertEqual "unadmitted observer stays restricted" True (structuralProgressIsJoiningObserver imported)
  assertEqual "observer has no report in the intermediate generation" [] (structuralReportEntries imported)
  assertEqual "observer has no historical admission acceptance" Nothing (installedTopologyCutOwnAcceptance installed)
  assertEqual "observer has no historical admission acknowledgement" Nothing (installedTopologyCutOwnAcknowledgement installed)
  assertEqual "already replayed control prefix does not regress" (controlIndex 100) (structuralAppliedControlPrefix imported)
  assertInstalledCoverageAgainstReference [] [] imported
  assertEqual "observer lineage is valid" (Right ()) (validateStructuralProgressState imported)
  assertEqual "historical activation first covers its control prefix" (Just (deriveTopologyCutId (preparedMembershipAdmissionBaseCut prepared))) (installedCutCoveringCause activationCause imported)
  assertEqual "observer control replay ahead of the cut does not create coverage" Nothing (installedCutCoveringCause unappliedToCutCause imported)
  let generation = oracleCurrentMembership initialOracleState
      observer = checked (initialJoiningStructuralProgressState system future generation initialDigest (Reconciliation.emptyStructuralAppliedState (emptyStructuralVersionVector generation)))
  assertPortableProgressImport (views future) observer imported
  let successor = case admissionRecordPhase certificate of AdmissionActivated _ value -> value; _ -> error "historical admission fixture is not activated"
      history = checked (heraldMembershipHistory (generation :| [successor]))
  assertIndependentProgressEvidence history [certificate] (controlIndex 100) (structuralProgressReconciliation imported) observer imported
  assertIndependentProgressEvidence history [certificate] (controlIndex 100) (structuralProgressReconciliation before) observer before

installedFixture :: Bool -> [HeraldEpoch] -> (Graph.State, StructuralProgressState, TopologyCut)
installedFixture observer order =
  let (_, certificate, recipe, contribution, before, local, _) = fixture observer order
      prepared = checked (prepareMembershipAdmissionBaseInstallation certificate recipe contribution (views local) Graph.emptyState before)
      (graph, progress) = commitMembershipAdmissionBaseInstallation prepared
   in (graph, progress, preparedMembershipAdmissionBaseCut prepared)

fixture :: Bool -> [HeraldEpoch] -> (HeraldAdmissionRecord, HeraldAdmissionRecord, HeraldJoinBaseRecipe, Bootstrap.HeraldBootstrapContribution, StructuralProgressState, HeraldEpoch, TopologyCut)
fixture observer = fixtureAt (if observer then applicant else oldMembers !! 1) observer

fixtureAt :: HeraldEpoch -> Bool -> [HeraldEpoch] -> (HeraldAdmissionRecord, HeraldAdmissionRecord, HeraldJoinBaseRecipe, Bootstrap.HeraldBootstrapContribution, StructuralProgressState, HeraldEpoch, TopologyCut)
fixtureAt local observer order =
  let generation = oracleCurrentMembership initialOracleState
      vector = emptyStructuralVersionVector generation
      constructor = if observer then initialJoiningStructuralProgressState else initialStructuralProgressState
      initial = checked (constructor system local generation initialDigest (Reconciliation.emptyStructuralAppliedState vector))
      anchor = checked (structuralAdmissionAnchorClaim (views local) initial)
      begun = submit (NE.head (heraldMembershipGenerationActiveHeraldEpochs (oracleCurrentMembership initialOracleState))) (beginHeraldAdmissionCommand manifest anchor) initialOracleState
      admission = admissionRecordId (fromJust (oraclePendingHeraldAdmission begun))
      contribution = checked (Bootstrap.heraldBootstrapContribution system admission applicant Set.empty Set.empty)
      controlled = fst (commitStructuralControlProgress (checked (prepareStructuralControlProgress (controlIndex 1) initial)))
      occurrence = either (error . show) id (checked (structuralTopologyOccurrenceDigestAt (views local) vector (controlIndex 1) controlled))
      oldCut = checked (topologyCut (sameGenerationPredecessor (structuralGenesisCutId controlled)) (topologyFrontier vector (controlIndex 1)) occurrence)
      cutId = deriveTopologyCutId oldCut
      oldCertificate = checked (topologyCutEstablished cutId oldCut [topologyCutAcceptance cutId (structuralAppliedReport h vector (controlIndex 1)) | h <- oldMembers])
      imported = checked (prepareTopologyCutEstablished (views local) (topologyJoinSealCheckpoint admission (controlIndex 1) "join") oldCertificate controlled)
      before = commitTopologyCutEstablished imported
      recipe = checked (heraldJoinBaseRecipe admission applicant generation oldCut (Bootstrap.contributionDigest contribution))
      digest = checked (mkHeraldJoinDigest (topologyOccurrenceDigestBytes (Bootstrap.contributionDigest contribution)))
      recipeDigest = checked (mkHeraldJoinDigest (heraldJoinBaseRecipeDigestBytes recipe))
      seal = checked (heraldJoinSeal admission 1 oldCut (controlIndex 1) [(h, heraldJoinMemberCut 0 0 digest) | h <- oldMembers] digest recipeDigest)
      sealed = submit (NE.head (heraldMembershipGenerationActiveHeraldEpochs (oracleCurrentMembership initialOracleState))) (sealHeraldAdmissionCommand seal) begun
      accepted = foldl (\state h -> submit h (acceptHeraldJoinSealCommand admission 1 (joinSealDigest seal)) state) sealed order
      report h = heraldJoinReadyReport admission 1 h (joinSealDigest seal) (controlIndex 1) recipeDigest
      oldReady = foldl (\state h -> submit h (reportHeraldJoinBaseReadyCommand (report h)) state) accepted order
      ready = submit (NE.head (heraldMembershipGenerationActiveHeraldEpochs (oracleCurrentMembership initialOracleState))) (reportHeraldJoinReadyCommand (report applicant)) oldReady
      activated = submit (NE.head (heraldMembershipGenerationActiveHeraldEpochs (oracleCurrentMembership initialOracleState))) (activateHeraldCommand admission) ready
   in (fromJust (oraclePendingHeraldAdmission ready), fromJust (oracleHeraldAdmission admission activated), recipe, contribution, before, local, oldCut)

submit :: HeraldEpoch -> OracleCommand -> OracleState -> OracleState
submit home command state =
  let request = oracleClientRequestId home (controlIndexWord64 (oracleGreatestControlIndex state) + 1)
      envelope = oracleEnvelope request Nothing home command
   in case checked (stepOracle envelope state) of
        (next, OracleCommitted receipt, _) | oracleReceiptResult receipt == OracleAccepted -> next
        (_, result, _) -> error ("admission fixture rejected: " <> show result)

initialOracleState :: OracleState
initialOracleState = checked (initialOracle Fixtures.fixtureCheckedOracleGenesis)
oldMembers :: [HeraldEpoch]
oldMembers = NE.toList (heraldMembershipGenerationActiveHeraldEpochs (oracleCurrentMembership initialOracleState))
system :: SystemId
system = checkedOracleSystemId Fixtures.fixtureCheckedOracleGenesis
applicant :: HeraldEpoch
applicant = checked (mkHeraldEpoch (BS.replicate 32 0xf0))
manifest :: HeraldAdmissionManifest
manifest = heraldAdmissionManifest system (checked (mkHeraldId (BS.replicate 32 0xef))) applicant
initialDigest :: InitialProjectionDigest
initialDigest = checked (mkInitialProjectionDigest (BS.replicate 32 0x42))
views :: HeraldEpoch -> Reconciliation.ReconciliationViews
views local = Reconciliation.reconciliationViews local Map.empty Set.empty Map.empty Set.empty
checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
