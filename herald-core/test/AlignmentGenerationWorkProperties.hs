{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module AlignmentGenerationWorkProperties (tests) where

import AlignmentCoordinatorProperties qualified as Fixture
import Control.Monad (foldM, forM_)
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Word (Word8)
import Eclips.Domain.Alignment
import Eclips.Domain.Context (deriveContextGraph)
import Eclips.Domain.Graph (VertexId (DeltaVertex))
import Eclips.Domain.Identity
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Domain.StructuralConsequence (processEndCause)
import Eclips.Domain.Topology (deriveTopologyCutId)
import Eclips.Herald.Alignment.CutQueries qualified as CutQueries
import Eclips.Herald.Alignment.Generation
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.EffectBatch
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Startup.State
import Eclips.Herald.Structural.Debt
import Eclips.Herald.UseCase.Alignment qualified as Coordinator
import Eclips.Herald.UseCase.OracleAdvance (structuralReconciliationViews)
import GenesisFixtures (fixtureIdentifierBytes, fixtureLocalMember, fixtureRemoteMember)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "selective alignment generation work"
    [ QC.testProperty "pending evidence lifetime, replay and consumption match an independent model" propEvidenceLifetime,
      testCase "historical generation insertion and member readiness wake exact evidence" caseHistoricalWake,
      QC.testProperty "sort grouping retains every live cause and replay stays quiet" propSortGroups,
      testCase "promoting one sort cannot enable or wake another parked sort" caseIndependentSorts,
      QC.testProperty "parked certificates and reordered readiness match exhaustive traces" propEvidenceTrace,
      testCase "invalid evidence still closes the current binding in the original order" caseInvalidEvidence,
      testCase "retired announcement queries are trimmed while unrelated evidence stays parked" caseRetiredAnnouncementQueryScope
    ]

propEvidenceLifetime :: [Word8] -> QC.Property
propEvidenceLifetime choices = QC.ioProperty $ do
  plan <- Fixture.generationPlan
  let generation = only (alignmentGenerationPlanGenerations plan)
      controls = generationControls generation
      schedule = [0 .. 15] <> take 60 choices
      observe (state, model) choice = do
        let (remote, control) = fixtureAt (fromIntegral choice `mod` length controls) controls
            key = checked (Alignment.pendingGenerationEvidenceKey control)
            action = fromIntegral choice `div` length controls `mod` 4 :: Int
            (next, expected) = case action of
              0 -> (retain remote control state, Map.insertWith (\_ old -> old) key (remote, True) model)
              1 -> (Alignment.consumeGenerationEvidenceWork key state, Map.adjust (\(peer, _) -> (peer, False)) key model)
              2 -> (fst (Alignment.commitPendingGenerationEvidence (checked (Alignment.preparePendingGenerationEvidenceRemoval remote control state))), Map.delete key model)
              _ -> (fst (Alignment.commitPendingGenerationEvidenceRetirement (Alignment.preparePendingGenerationEvidenceRetirement remote state)), Map.filter ((/= remote) . fst) model)
        assertEqual "registration follows exact retained payload lifetime" (Map.keysSet expected) (Alignment.generationEvidenceWorkIds next)
        assertEqual "replay cannot dirty a parked payload" (Map.keysSet (Map.filter snd expected)) (Alignment.pendingGenerationEvidenceWork next)
        assertBool "all reverse buckets agree with independent payload reconstruction" (Alignment.generationSchedulingValid next)
        pure (next, expected)
  _ <- foldM observe (Alignment.emptyState, Map.empty) schedule
  pure True

caseHistoricalWake :: Assertion
caseHistoricalWake = do
  plan <- Fixture.generationPlan
  let generation = only (alignmentGenerationPlanGenerations plan)
      generationId = alignmentGenerationId generation
      controls = generationControls generation
      retained = foldl (\state (peer, control) -> retain peer control state) Alignment.emptyState controls
      parked = consumeEvidence retained
      observer = checked (mkHeraldEpoch (fixtureIdentifierBytes 0xee))
      foreignId = checked (mkContextClassGenerationId (fixtureIdentifierBytes 0xed))
      member = NonEmpty.head (alignmentCutExactMembers (alignmentGenerationCut generation))
      foreignControl = Protocol.AlignmentMemberReadyAdvertised (Protocol.classMemberReady Protocol.firstAlignmentEvidenceSequence foreignId (alignmentMemberStoreIncarnation member) (deriveMemberReadyEvidenceDigest "foreign-ready") initialStoreRevision)
      withForeign = consumeEvidence (retain (alignmentMemberHerald member) foreignControl parked)
      knownKeys = Set.fromList [checked (Alignment.pendingGenerationEvidenceKey control) | (_, control) <- controls]
      installed = checked (Alignment.retainHistoricalAlignmentPlan observer plan withForeign)
  assertEqual "only waiters on the installed immutable generation wake" knownKeys (Alignment.pendingGenerationEvidenceWork installed)
  assertEqual "new generation notifies route heads independently" (Set.singleton generationId) (fst (Alignment.takeRouteMarkerChanges installed))
  let (markerIds, withoutMarkers) = Alignment.takeRouteMarkerChanges installed
      replay = checked (Alignment.retainHistoricalAlignmentPlan observer plan (consumeEvidence withoutMarkers))
  assertEqual "marker drain does not consume generation evidence work" knownKeys (Alignment.pendingGenerationEvidenceWork withoutMarkers)
  assertEqual "historical replay does not recreate evidence work" Set.empty (Alignment.pendingGenerationEvidenceWork replay)
  assertEqual "historical replay does not recreate marker work" Set.empty (fst (Alignment.takeRouteMarkerChanges replay))
  assertEqual "historical insertion has one route marker coordinate" (Set.singleton generationId) markerIds
  let readiness = [observedReady | (_, Protocol.AlignmentMemberReadyAdvertised observedReady) <- controls]
      ready = only readiness
      certificateKeys = Set.fromList [checked (Alignment.pendingGenerationEvidenceKey control) | (_, control@Protocol.AlignmentHistoricalCertificateAdvertised {}) <- controls]
      withReady = fst (Alignment.commitClassMemberReady (checked (Alignment.prepareClassMemberReady ready replay)))
      readyReplay = fst (Alignment.commitClassMemberReady (checked (Alignment.prepareClassMemberReady ready (consumeEvidence withReady))))
  assertEqual "member readiness wakes only same-generation certificates" certificateKeys (Alignment.pendingGenerationEvidenceWork withReady)
  assertEqual "exact readiness replay is silent" Set.empty (Alignment.pendingGenerationEvidenceWork readyReplay)
  assertEqual "owner admission and scheduling invariants agree" (Right ()) (Alignment.validateAlignmentState readyReplay)

propSortGroups :: [Word8] -> QC.Property
propSortGroups choices = QC.ioProperty $ do
  (promoted, generation) <- Fixture.promotedFixture
  let cause = Alignment.alignmentPromotionKeyCause (fst (only (Alignment.alignmentPromotionEntries promoted)))
      cut = alignmentGenerationCut generation
      firstSort = sortOccurrence (alignmentCutSortId cut) (alignmentCutSortDefinitionOccurrenceId cut)
      secondSort = sortOccurrence (checked (mkSortId (fixtureIdentifierBytes 0xe5))) (checked (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xe6)))
      sorts = [firstSort, secondSort]
      kinds = [TopologyAlignmentDebt, CutoverDebt]
      causes = [cause, checked (processEndCause (checked (mkProcessEpochId (fixtureIdentifierBytes 0xea))) (controlIndex 1))]
      observe state choice = do
        let affected = fixtureAt (fromIntegral choice `mod` 2) sorts
            kind = fixtureAt (fromIntegral choice `div` 2 `mod` 2) kinds
            selectedCause = fixtureAt (fromIntegral choice `div` 4 `mod` 2) causes
            key = structuralDebtKey selectedCause kind affected Nothing
            debt = structuralConsequenceDebt key (structuralDebtEvidence Set.empty Set.empty Set.empty)
            existed = key `elem` map structuralConsequenceDebtKey (Alignment.liveAlignmentStructuralDebtEntries state)
            next = fst (Alignment.commitStructuralDebtRetention (checked (Alignment.prepareStructuralDebtRetention selectedCause (normalizeStructuralDebts [debt]) state)))
            expected = Set.fromList [(structuralDebtKeyCause k, structuralDebtKeySort k) | retained <- Alignment.liveAlignmentStructuralDebtEntries next, let k = structuralConsequenceDebtKey retained]
        assertEqual "query inventory preserves old global cause/sort order" (Set.toAscList expected) (Alignment.liveAlignmentDebtCoordinatesAtSorts (Set.fromList sorts) next)
        assertEqual "only new live work wakes its exact sort" (if existed then Set.empty else Set.singleton affected) (Alignment.pendingGenerationPlanWork next)
        assertEqual "ordinary equality retains scheduling metadata" existed (next == state)
        assertBool "sort and reverse scheduling indexes reconstruct" (Alignment.generationSchedulingValid next)
        pure (Alignment.consumeGenerationPlanWork (Set.fromList sorts) next)
  _ <- foldM observe Alignment.emptyState ([0 .. 7] <> take 40 choices)
  pure True

caseIndependentSorts :: Assertion
caseIndependentSorts = do
  firstPlan <- Fixture.generationPlan
  (promoted, firstGeneration) <- Fixture.promotedFixture
  let cause = Alignment.alignmentPromotionKeyCause (fst (only (Alignment.alignmentPromotionEntries promoted)))
      cut = alignmentGenerationCut firstGeneration
      firstSort = sortOccurrence (alignmentCutSortId cut) (alignmentCutSortDefinitionOccurrenceId cut)
      secondSort = sortOccurrence (checked (mkSortId (fixtureIdentifierBytes 0xe5))) (checked (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xe6)))
      delta = checked (mkDeltaId (fixtureIdentifierBytes 0xe7))
      store = checked (mkStoreIncarnationId (fixtureIdentifierBytes 0xe8))
      herald = alignmentGenerationAnnouncer firstGeneration
      context = checked (deriveContextGraph [DeltaVertex delta] [] [delta])
      input = generationMemberInput delta store herald initialStoreRevision
      topology = alignmentCutTopologyCut cut
      topologyId = deriveTopologyCutId topology
      vector = alignmentCutPhysicalPlacementRevisionVector cut
      addDebt affected state = fst (Alignment.commitStructuralDebtRetention (checked (Alignment.prepareStructuralDebtRetention cause (normalizeStructuralDebts [structuralConsequenceDebt (structuralDebtKey cause TopologyAlignmentDebt affected Nothing) (structuralDebtEvidence Set.empty Set.empty Set.empty)]) state)))
      parked = Alignment.consumeGenerationPlanWork (Set.fromList [firstSort, secondSort]) (addDebt secondSort (addDebt firstSort Alignment.emptyState))
      eligibility state = Coordinator.alignmentCausePromotionDisposition cause secondSort topologyId vector state
      first = fst (Alignment.commitAlignmentPromotion (checked (Alignment.prepareAlignmentPromotion herald cause firstSort topologyId vector firstPlan parked)))
  secondPlan <- case prepareAlignmentGenerationPlan secondSort topology vector context [input] [] [] Set.empty of
    Right (AlignmentGenerationReady ready) -> pure ready
    other -> assertFailure ("second independent sort plan: " <> show other)
  assertEqual "the second sort is due before the first promotion" Coordinator.AlignmentCausePromotionDue (eligibility parked)
  assertEqual "first-sort promotion preserves the other sort's blocker facts" (eligibility parked) (eligibility first)
  assertEqual "no cross-sort eligibility notification is manufactured" Set.empty (Alignment.pendingGenerationPlanWork first)
  assertEqual "only still-live sort work stays registered" (Set.singleton secondSort) (Alignment.generationPlanWorkIds first)
  let completed = fst (Alignment.commitAlignmentPromotion (checked (Alignment.prepareAlignmentPromotion herald cause secondSort topologyId vector secondPlan first)))
  assertEqual "both completed sort lifetimes release their scheduling roots" Set.empty (Alignment.generationPlanWorkIds completed)
  assertEqual "completed independent promotions validate" (Right ()) (Alignment.validateAlignmentState completed)

propEvidenceTrace :: [Word8] -> QC.Property
propEvidenceTrace choices = QC.ioProperty $ do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (initialOwner, generation) <- Fixture.readyAlignmentFixture local remote
  (_, _, accepted, remoteReady, certificate) <- Fixture.generationFastPathFixture local remote
  let certificateControl = Protocol.AlignmentHistoricalCertificateAdvertised certificate
      readyControl = Protocol.AlignmentMemberReadyAdvertised remoteReady
      acceptanceControl = Protocol.AlignmentCutAcceptanceAdvertised accepted
      pending = retain remote certificateControl initialOwner
      key = checked (Alignment.pendingGenerationEvidenceKey certificateControl)
  (initial, _) <- Fixture.connectedWithAlignment pending
  (parked, effects, disposition) <- differential "incomplete certificate" initial
  assertEqual "a blocked certificate produces no effects" [] (effectBatchMembers effects)
  assertEqual "parking does not mean semantic progress" Coordinator.AlignmentGenerationWorkUnchanged disposition
  assertEqual "certificate remains registered but is no longer selected" (Set.singleton key) (Alignment.generationEvidenceWorkIds (startupAlignmentState parked))
  assertEqual "certificate queue is empty until a dependency changes" Set.empty (Alignment.pendingGenerationEvidenceWork (startupAlignmentState parked))
  let schedules = take 20 choices <> [0, 1, 2, 3]
      operation state choice = do
        let owner = startupAlignmentState state
            altered = case choice `mod` 4 of
              0 -> retain remote certificateControl owner
              1 -> retain remote acceptanceControl owner
              2 -> Alignment.wakeGenerationPlanSorts Set.empty owner
              _ -> retain remote readyControl owner
            staged = replaceStartupAlignmentState altered state
        (next, _, _) <- differential "reordered queued evidence" staged
        pure next
  final <- foldM operation parked schedules
  assertEqual "same-pass readiness wakes the previously parked later certificate" (Just certificate) (Alignment.lookupAlignmentHistoricalCertificate (alignmentGenerationId generation) (Protocol.historicalCertificateSourceStoreIncarnation certificate) (startupAlignmentState final))
  assertEqual "all completed payload lifetimes are released" Set.empty (Alignment.generationEvidenceWorkIds (startupAlignmentState final))
  assertEqual "final complete owner invariant holds" (Right ()) (Alignment.validateAlignmentState (startupAlignmentState final))
  pure True

caseInvalidEvidence :: Assertion
caseInvalidEvidence = do
  let HeraldMember _ local = fixtureLocalMember
      HeraldMember _ remote = fixtureRemoteMember
  (owner, generation, _, _, certificate) <- Fixture.generationFastPathFixture local remote
  let cut = alignmentGenerationCut generation
      localMember = required (find ((== local) . alignmentMemberHerald) (NonEmpty.toList (alignmentCutExactMembers cut)))
      invalid = Protocol.AlignmentMemberReadyAdvertised (Protocol.classMemberReady Protocol.firstAlignmentEvidenceSequence (alignmentGenerationId generation) (alignmentMemberStoreIncarnation localMember) (deriveMemberReadyEvidenceDigest "wrong-owner") initialStoreRevision)
      pending = retain remote invalid (retain remote (Protocol.AlignmentHistoricalCertificateAdvertised certificate) owner)
  (initial, binding) <- Fixture.connectedWithAlignment pending
  (final, effects, _) <- differential "invalid earlier readiness and valid later certificate" initial
  assertEqual "exact protocol rejection is retained" [RejectPeerConnection binding ClosePeerControlProtocol] [effect | effect@RejectPeerConnection {} <- effectBatchMembers effects]
  assertEqual "the later valid certificate is still admitted" (Just certificate) (Alignment.lookupAlignmentHistoricalCertificate (alignmentGenerationId generation) (Protocol.historicalCertificateSourceStoreIncarnation certificate) (startupAlignmentState final))
  assertEqual "invalid payload is removed without poisoning the index" Set.empty (Alignment.generationEvidenceWorkIds (startupAlignmentState final))

caseRetiredAnnouncementQueryScope :: Assertion
caseRetiredAnnouncementQueryScope = do
  plan <- Fixture.generationPlan
  (baseline, _, _) <- differential "query-scope fixture assembly" Fixture.retainedCutQueryFixture
  let generation = only (alignmentGenerationPlanGenerations plan)
      cut = alignmentGenerationCut generation
      retired = alignmentGenerationAnnouncer generation
      announce = Protocol.AlignmentCutAnnounced (alignmentGenerationCutAnnounce generation)
      announcedVector = alignmentCutPhysicalPlacementRevisionVector cut
      currentVector = checked (Placement.currentPhysicalPlacementRevisionVector (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState baseline))) (startupPlacementState baseline))
      survivor = checked (mkHeraldEpoch (fixtureIdentifierBytes 0xef))
      absentGeneration = checked (mkContextClassGenerationId (fixtureIdentifierBytes 0xee))
      member = NonEmpty.head (alignmentCutExactMembers cut)
      certificate = Protocol.AlignmentHistoricalCertificateAdvertised (Protocol.historicalCertificate absentGeneration (alignmentMemberStoreIncarnation member) (deriveMemberReadyEvidenceDigest "scope-pending-ready") (deriveBootstrapEvidenceDigest "scope-pending-prefix") initialStoreRevision)
      certificateKey = checked (Alignment.pendingGenerationEvidenceKey certificate)
      affectedSort = sortOccurrence (alignmentCutSortId cut) (alignmentCutSortDefinitionOccurrenceId cut)
      uninstalledCause = checked (processEndCause (checked (mkProcessEpochId (fixtureIdentifierBytes 0xed))) (controlIndex 999))
      debt = structuralConsequenceDebt (structuralDebtKey uninstalledCause TopologyAlignmentDebt affectedSort Nothing) (structuralDebtEvidence Set.empty Set.empty Set.empty)
      scope = (Set.singleton uninstalledCause, Set.singleton affectedSort, Set.fromList [currentVector, announcedVector])
      cached = CutQueries.prepareCutQueryCache (structuralReconciliationViews baseline) (startupStructuralProgressState baseline) (startupPlacementState baseline) (Set.singleton uninstalledCause) (Set.singleton affectedSort) (Set.fromList [currentVector, announcedVector]) CutQueries.emptyCutQueryCache
      certificateOnly = retain survivor certificate (startupAlignmentState baseline)
      removedCertificate = fst (Alignment.commitPendingGenerationEvidence (checked (Alignment.preparePendingGenerationEvidenceRemoval survivor certificate certificateOnly)))
  assertBool "the obsolete announced vector is distinct from the current scope" (announcedVector /= currentVector)
  assertBool "the remaining evidence belongs to another peer" (retired /= survivor)
  assertBool "a certificate removal has no placement-query scope event" (not (fst (Alignment.takeGenerationQueryScopeChange removedCertificate)))
  assertBool "fixture has an actual installed structural query index" (not (Map.null (CutQueries.cutQueryPositions cached)))
  forM_ [False, True] $ \keepPlan -> do
    let withDebt
          | keepPlan = fst (Alignment.commitStructuralDebtRetention (checked (Alignment.prepareStructuralDebtRetention uninstalledCause (normalizeStructuralDebts [debt]) certificateOnly)))
          | otherwise = certificateOnly
        owner = consumeEvidence (Alignment.consumeGenerationPlanWork (Set.singleton affectedSort) (retain retired announce withDebt))
        warm = replaceStartupAlignmentCutQueryCache cached (replaceStartupAlignmentState owner baseline)
    assertBool "an announcement remains a query consumer after its work parks" (Alignment.hasPendingGenerationAnnouncements owner)
    (parked, parkedEffects, parkedWork) <- differential "unchanged parked query scope" warm
    assertEqual "unchanged parked query scope retains every prepared entry" scope (CutQueries.cutQueryRetainedKeys (startupAlignmentCutQueryCache parked))
    assertEqual "parking emits no effects" [] (effectBatchMembers parkedEffects)
    assertEqual "parking is not semantic work" Coordinator.AlignmentGenerationWorkUnchanged parkedWork
    let (retiredOwner, removed) = Alignment.commitPendingGenerationEvidenceRetirement (Alignment.preparePendingGenerationEvidenceRetirement retired (startupAlignmentState parked))
        (replayedRetirement, duplicateRemoved) = Alignment.commitPendingGenerationEvidenceRetirement (Alignment.preparePendingGenerationEvidenceRetirement retired retiredOwner)
    assertEqual "retirement removes exactly its announcement" [announce] (map Alignment.pendingGenerationEvidenceControl removed)
    assertEqual "duplicate retirement removes no extra payload" [] duplicateRemoved
    assertBool "retirement coalesces the placement-query scope notification" (fst (Alignment.takeGenerationQueryScopeChange replayedRetirement))
    assertBool "retirement releases its announcement registration" (not (Alignment.hasPendingGenerationAnnouncements retiredOwner))
    assertEqual "unrelated missing-generation certificate stays registered" (Set.singleton certificateKey) (Alignment.generationEvidenceWorkIds retiredOwner)
    assertEqual "retirement does not wake blocked evidence" Set.empty (Alignment.pendingGenerationEvidenceWork retiredOwner)
    assertEqual "retirement does not wake blocked plans" Set.empty (Alignment.pendingGenerationPlanWork retiredOwner)
    (released, effects, work) <- differential "retired parked query scope" (replaceStartupAlignmentState replayedRetirement parked)
    let retained = startupAlignmentCutQueryCache released
        (_, retainedSorts, retainedVectors) = CutQueries.cutQueryRetainedKeys retained
    assertEqual "only the live plan retains the current vector" (if keepPlan then Set.singleton currentVector else Set.empty) retainedVectors
    assertEqual "no query consumer retains its per-sort suffixes" (if keepPlan then Set.singleton affectedSort else Set.empty) retainedSorts
    assertEqual "scope retirement preserves structural preparation" (CutQueries.cutQueryPositions cached) (CutQueries.cutQueryPositions retained)
    assertBool "scope retirement consumes its one-shot notification" (not (fst (Alignment.takeGenerationQueryScopeChange (startupAlignmentState released))))
    assertEqual "scope retirement emits no effects" [] (effectBatchMembers effects)
    assertEqual "scope retirement is not semantic work" Coordinator.AlignmentGenerationWorkUnchanged work
    (again, againEffects, againWork) <- differential "repeat after query-scope retirement" released
    assertBool "another idle poll preserves every owner" (again == released)
    assertEqual "another idle poll keeps the compact cache scope" (CutQueries.cutQueryRetainedKeys retained) (CutQueries.cutQueryRetainedKeys (startupAlignmentCutQueryCache again))
    assertEqual "another idle poll emits no effects" [] (effectBatchMembers againEffects)
    assertEqual "another idle poll is not semantic work" Coordinator.AlignmentGenerationWorkUnchanged againWork

-- Reference inventory selection is the original whole pending map and all live
-- causes. Scheduling/cache normalization is explicit and only for comparison.
differential :: String -> HeraldState -> IO (HeraldState, EffectBatch, Coordinator.AlignmentGenerationWorkDisposition)
differential label predecessor = do
  result@(successor, effects, work) <- either (assertFailure . ((label <> ": ") <>) . show) pure (Coordinator.advanceAlignmentGenerationsWithDisposition predecessor)
  (reference, referenceEffects, referenceWork) <- either (assertFailure . ((label <> " exhaustive: ") <>) . show) pure (Coordinator.advanceAlignmentGenerationsWithDispositionExhaustive predecessor)
  assertBool (label <> ": semantic owners match independent enumeration") (clearStartupCoordinationScheduling successor == clearStartupCoordinationScheduling reference)
  assertEqual (label <> ": exact ordered effects match") (effectBatchMembers referenceEffects) (effectBatchMembers effects)
  assertEqual (label <> ": semantic Changed matches") referenceWork work
  assertBool (label <> ": scheduling ownership is valid") (Alignment.generationSchedulingValid (startupAlignmentState successor) && Alignment.transferSchedulingValid (startupAlignmentState successor))
  pure result

generationControls :: AlignmentGeneration -> [(HeraldEpoch, Protocol.AlignmentControl)]
generationControls generation =
  [ (alignmentGenerationAnnouncer generation, Protocol.AlignmentCutAnnounced (alignmentGenerationCutAnnounce generation)),
    (remote, Protocol.AlignmentCutAcceptanceAdvertised (Protocol.alignmentCutAccepted identifier remote (deriveTopologyCutId (alignmentCutTopologyCut cut)) (alignmentCutPhysicalPlacementRevisionVector cut) EmptyHeraldPublicationPrefix)),
    (remote, Protocol.AlignmentMemberReadyAdvertised ready),
    (remote, Protocol.AlignmentHistoricalCertificateAdvertised (Protocol.historicalCertificate identifier store (deriveMemberReadyEvidenceDigest "pending-set") (deriveBootstrapEvidenceDigest "pending-prefix") initialStoreRevision))
  ]
  where
    cut = alignmentGenerationCut generation
    identifier = alignmentGenerationId generation
    member = NonEmpty.last (alignmentCutExactMembers cut)
    remote = alignmentMemberHerald member
    store = alignmentMemberStoreIncarnation member
    ready = Protocol.classMemberReady Protocol.firstAlignmentEvidenceSequence identifier store (deriveMemberReadyEvidenceDigest "pending-ready") initialStoreRevision

retain :: HeraldEpoch -> Protocol.AlignmentControl -> Alignment.State -> Alignment.State
retain remote control state = fst (Alignment.commitPendingGenerationEvidence (checked (Alignment.preparePendingGenerationEvidence remote control state)))

consumeEvidence :: Alignment.State -> Alignment.State
consumeEvidence state = Set.foldl' (flip Alignment.consumeGenerationEvidenceWork) state (Alignment.generationEvidenceWorkIds state)

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

only :: [value] -> value
only [value] = value
only _ = error "expected one fixture value"

required :: Maybe value -> value
required = maybe (error "missing fixture value") id

fixtureAt :: Int -> [value] -> value
fixtureAt index = required . listToMaybe . drop index
