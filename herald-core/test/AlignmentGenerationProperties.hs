{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module AlignmentGenerationProperties
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.ByteString.Char8 qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Alignment
  ( ContextClassGenerationId,
    HeraldPublicationPrefix (EmptyHeraldPublicationPrefix),
    PhysicalPlacementRevisionVector,
    alignmentCutFreshMemberBaseEvidence,
    alignmentCutPredecessorGenerationIds,
    alignmentCutTopologyCut,
    deriveBootstrapEvidenceDigest,
    deriveMemberReadyEvidenceDigest,
    firstPlacementRevision,
    initialStoreRevision,
    memberReadyEvidenceDigestBytes,
    physicalPlacementRevisionVector,
  )
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Context (ContextGraph, deriveContextGraph)
import Eclips.Domain.Graph
  ( EdgeStrength (Preserve),
    VertexId (DeltaVertex),
    edgePayload,
  )
import Eclips.Domain.Identity
  ( DeltaId,
    HeraldEpoch,
    ProcessEpochId,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
    StructuralOccurrenceId,
    SystemId,
    TopologyCutId,
    controlIndex,
    mkDeltaId,
    mkHeraldEpoch,
    mkProcessEpochId,
    mkSortDefinitionOccurrenceId,
    mkSortId,
    mkStoreIncarnationId,
    mkStructuralSequence,
    mkSystemId,
    mkTopologyCutId,
    structuralOccurrenceId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    genesisHeraldMembershipGeneration,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Structural
  ( StructuralVersionVector,
    emptyStructuralVersionVector,
  )
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    processEndCause,
    structuralOccurrenceCause,
  )
import Eclips.Domain.Topology
  ( TopologyCut,
    deriveTopologyCutId,
    deriveTopologyOccurrenceDigest,
    sameGenerationPredecessor,
    topologyCut,
    topologyFrontier,
  )
import Eclips.Domain.Topology qualified as Topology
import Eclips.Herald.Alignment.Generation
  ( AlignmentGeneration,
    AlignmentGenerationPlan,
    AlignmentGenerationPreparation (..),
    AlignmentGenerationProblem,
    AlignmentGenerationRelation,
    alignmentGenerationCut,
    alignmentGenerationCutAnnounce,
    alignmentGenerationId,
    alignmentGenerationPlanGenerations,
    alignmentGenerationPlanRelations,
    alignmentGenerationRelationDestination,
    alignmentGenerationRelationSource,
    alignmentGenerationTopologyCutId,
    generationMemberInput,
    prepareAlignmentGenerationPlan,
  )
import Eclips.Herald.Alignment.History qualified as History
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.Protocol
  ( alignmentCutAccepted,
    alignmentCutAnnounceGeneration,
    alignmentObligationCause,
    checkAlignmentCutAnnounce,
    classMemberReady,
    classMemberReadyPredecessorAndBasePrefixDigest,
    classMemberReadySetDigest,
    classMemberReadyStoreIncarnation,
    firstAlignmentEvidenceSequence,
    historicalCertificate,
  )
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Alignment.State
  ( AlignmentAttemptProblem
      ( AlignmentAttemptDestinationAcceptanceUnavailable,
        AlignmentAttemptSourceEvidenceUnavailable
      ),
    AlignmentPromotionDisposition (..),
    AlignmentPromotionProblem (AlignmentPromotionConflict),
    alignmentGenerationEntries,
    alignmentObligationEntries,
    alignmentPromotionEntries,
    alignmentPromotionKey,
    alignmentPromotionKeyCause,
    commitAlignmentAttempt,
    commitAlignmentCutAcceptance,
    commitAlignmentPromotion,
    commitClassMemberReady,
    commitHistoricalCertificate,
    commitStructuralDebtRetention,
    emptyState,
    prepareAlignmentAttempt,
    prepareAlignmentCutAcceptance,
    prepareAlignmentPromotion,
    prepareClassMemberReady,
    prepareHistoricalCertificate,
    prepareStructuralDebtRetention,
    preparedAlignmentPromotionDisposition,
  )
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Internal.Diagnostics qualified as Diagnostics
import Eclips.Herald.Join.History qualified as JoinHistory
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    StructuralDebtKind (TopologyAlignmentDebt),
    normalizeStructuralDebts,
    sortOccurrence,
    structuralConsequenceDebt,
    structuralDebtEvidence,
    structuralDebtKey,
  )
import Eclips.Herald.Structural.Debt qualified as Debt
import GenesisFixtures (fixtureIdentifierBytes, fixtureRetirementResolution)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    chooseInt,
    conjoin,
    counterexample,
    elements,
    forAll,
    shuffle,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "alignment generations"
    [ testCase "deterministic split/merge ancestry and certificate gate" caseDeterministicAncestry,
      testCase "retained generations expose their checked cut announcement" caseCheckedCutAnnouncement,
      testProperty "retained topology digests match generated canonical cuts" propCachedTopologyCut,
      testProperty "ancestry and readiness are invariant under evidence arrival permutations" propEvidenceArrivalPermutation,
      testCase "debt promotion is exact-once and generation-retaining" casePromotion,
      testCase "control cause survives debt, promotion, and obligation refinement" caseControlCausePromotion,
      testCase "supplier attempt waits for exact acceptance and certificate" caseAttemptGate,
      testCase "historical catalogue import creates no local alignment authority or work" caseHistoricalCatalogue,
      testCase "historical frontier adoption requires a complete old plan and cannot rewind" caseHistoricalFrontier,
      testProperty "duplicate passive imports preserve the canonical adopted frontier" propHistoricalCatalogueReplay,
      testCase "historical evidence recipients retain only their current attempt without local authority" caseHistoricalRecipientScope,
      testProperty "historical recipient scope converges under duplicate and reordered captures" propHistoricalRecipientReplay,
      testCase "passive cause coverage preserves local ownership and exact historical coordinates" caseHistoricalCoverage,
      testProperty "history preserves nonzero attempts after exact promotion and invalidation" propHistoricalAttemptCoverage,
      testProperty "fresh attempts atomically reset current and closing owners with loopback cancellation" propResetPlanOwners,
      testProperty "obsolete reports freeze once and schedule retries without live debts" propObsoleteReportScheduling,
      testProperty "reports overtaking a reset remain pending without preventing its admission" propObsoleteReportBeforeReset,
      testCase "a new membership reset supersedes the old announcer's larger ordinal" caseResetAfterAnnouncerMembershipChange,
      testProperty "passive coverage and receiver-derived debts commute under duplicate arrival" propHistoricalCoverageArrival,
      testProperty "invalidation reopens passive coverage independently of evidence arrival order" propHistoricalCoverageInvalidation,
      testProperty "coverage range queries equal exhaustive history filtering under admitted replay" propCoverageRangeQueries,
      testCase "removal-only historical coverage survives capture without a frontier or generation" caseHistoricalEmptyCoverage,
      testCase "frontier admission proof requires exactly the surviving original fixed members" caseHistoricalAdmissionProof
    ]

caseHistoricalCoverage :: Assertion
caseHistoricalCoverage = do
  let plan = coveragePlan
      key = coverageKey
      donorDebt = coverageDonorDebt
      donor =
        fst
          ( commitAlignmentPromotion
              (checked "ordinary donor promotion" (Alignment.prepareAlignmentPlanPromotion heraldB coverageCause plan (retainCoverageDebt donorDebt emptyState)))
          )
      identifiers = Set.toAscList (Set.fromList (map alignmentGenerationId (routingGenerations plan)))
      retained = foldl (flip retainCoverageDebt) emptyState coverageDebts
      imported = checked "import source completion" (Alignment.retainHistoricalAlignmentPlanCoverage joiningHerald key plan retained)
      captured = History.captureAlignmentHistory imported
      beforeCoverage = checked "adopt frontier before coverage" (Alignment.adoptHistoricalAlignmentPlanFrontier joiningHerald [(typedSort, Plan.alignmentPlanId plan)] (checked "retain passive plan before coverage" (Alignment.retainHistoricalAlignmentRoutingPlan joiningHerald plan retained)))
      afterCoverage = checked "cover debt without replacing frontier" (Alignment.retainHistoricalAlignmentPlanCoverage joiningHerald key plan beforeCoverage)
  assertEqual "ordinary promotion exports semantic completion without owner IDs" [(key, identifiers)] (Alignment.alignmentCompletedCauseCoverage donor)
  assertEqual "the recipient retains the source's exact completion coordinate" (Alignment.alignmentCompletedCauseCoverage donor) (Alignment.alignmentCompletedCauseCoverage imported)
  assertEqual "receiver-specific destination debts are covered while other causes and sorts remain live" (uncoveredDebtKeys retained) (liveDebtKeys imported)
  assertEqual "all receiver debt and evidence remain immutable derivation history" (Alignment.alignmentStructuralDebts retained) (Alignment.alignmentStructuralDebts imported)
  assertBool "coverage creates no local work, cut votes, readiness or frontier" (passiveCoverageNoWork imported)
  assertEqual "coverage also preserves an already adopted frontier" (Alignment.historicalAlignmentFrontier beforeCoverage) (Alignment.historicalAlignmentFrontier afterCoverage)
  assertEqual "coverage replay is owner-idempotent" (Right imported) (Alignment.retainHistoricalAlignmentPlanCoverage joiningHerald key plan imported)
  assertEqual "coverage cannot stand in for an original member's promotion" (Left (Alignment.AlignmentHistoricalPlanLocalMember heraldA)) (Alignment.retainHistoricalAlignmentPlanCoverage heraldA key plan imported)
  assertBool "coverage rejects a different exact topology" (isRejected (Alignment.retainHistoricalAlignmentPlanCoverage joiningHerald (alignmentPromotionKey coverageCause typedSort predecessorCut placement) plan retained))
  let conflicting = routingPlan topology reverseContext Nothing Set.empty
  assertBool "same generation IDs cannot change the covered plan's relations" (isRejected (Alignment.retainHistoricalAlignmentPlanCoverage joiningHerald key conflicting imported))
  assertEqual "coverage preserves the checked alignment invariant" (Right ()) (Alignment.validateAlignmentState imported)
  assertEqual "coverage is captured even without an adopted latest frontier" [] (History.alignmentHistoryFrontier captured)
  assertEqual "capture includes completed cause coverage, independently of the frontier" [(key, identifiers)] (History.alignmentHistoryCoverage captured)
  assertEqual "coverage capture retains the complete checked sibling plan" (Set.fromList identifiers) (Set.fromList [alignmentCutAnnounceGeneration announce | siblings <- History.alignmentHistoryPlans captured, announce <- Protocol.alignmentPlanAnnounceCreatedCuts siblings])
  assertEqual "the canonical codec preserves completed coverage" (Right captured) (History.decodeAlignmentHistory (History.encodeAlignmentHistory captured))

-- Exercise the actual owner boundary before capture. A new attempt at exactly
-- the old physical/topology coordinate must survive its predecessor's tombstone;
-- the next ordinary plan carries the new birth generations without renaming them.
propHistoricalAttemptCoverage :: Property
propHistoricalAttemptCoverage =
  forAll (chooseInt (1, 5)) $ \ordinal ->
    let attempt = iterate DomainAlignment.nextAlignmentPlanAttempt DomainAlignment.initialAlignmentPlanAttempt !! ordinal
        original = coveragePlan
        recovered = prepare attempt topology (Just (original, Plan.AlignmentPredecessorInvalidated))
        carried = prepare attempt historySuccessorTopology (Just (recovered, Plan.AlignmentPredecessorUsable))
        prepare selectedAttempt selectedTopology prior = case Plan.prepareAlignmentPlanAtAttempt selectedAttempt typedSort selectedTopology placement firstContext members prior Set.empty of
          Right (Plan.AlignmentPlanReady plan) -> plan
          result -> error ("historical attempt fixture: " <> show result)
        members = [generationMemberInput deltaA storeA heraldA initialStoreRevision, generationMemberInput deltaB storeB heraldB initialStoreRevision]
        promote cause plan = fst . commitAlignmentPromotion . checked "promote historical attempt" . Alignment.prepareAlignmentPlanPromotion heraldA cause plan
        originalOwner = promote coverageCause original (retainCoverageDebt coverageDonorDebt emptyState)
        lost = case [generation | generation <- routingGenerations original, any ((== deltaA) . DomainAlignment.alignmentMemberDelta) (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut generation))] of
          [generation] -> generation
          _ -> error "historical attempt fixture must have one class containing the lost Store"
        loss = Alignment.AlignmentPlanDestinationStoreLost (alignmentGenerationId lost) (Protocol.destinationStore deltaA storeA)
        invalidated = fst (Alignment.commitAlignmentPlanInvalidation (checked "invalidate initial attempt" (Alignment.prepareAlignmentPlanInvalidation loss originalOwner)))
        recoveredOwner = promote coverageCause recovered invalidated
        carriedCause = checked "carried plan cause" (processEndCause endedProcess (controlIndex 7))
        carriedDebt = structuralConsequenceDebt (structuralDebtKey carriedCause TopologyAlignmentDebt typedSort Nothing) (structuralDebtEvidence Set.empty Set.empty Set.empty)
        donor = promote carriedCause carried (retainCoverageDebt carriedDebt recoveredOwner)
        captured = History.captureAlignmentHistory donor
        decoded = checked "decode attempted historical plans" (History.decodeAlignmentHistory (History.encodeAlignmentHistory captured))
        announcements = Map.fromList [(Protocol.alignmentPlanAnnounceId announce, announce) | announce <- History.alignmentHistoryPlans decoded]
        identifiers = map Plan.alignmentPlanId [original, recovered, carried]
        reconstruct state identifier = do
          announce <- maybe (Left "captured plan is absent") Right (Map.lookup identifier announcements)
          prior <- traverse (\parent -> maybe (Left "reconstructed parent is absent") Right (Alignment.lookupAlignmentPlan parent state)) (Protocol.alignmentPlanAnnouncePredecessor announce)
          prepared <- shape (Plan.prepareAlignmentPlanAtAttempt (Plan.alignmentPlanIdAttempt identifier) (Plan.alignmentPlanIdSort identifier) (Protocol.alignmentPlanAnnounceTopology announce) (Plan.alignmentPlanIdPlacement identifier) firstContext members ((,Protocol.alignmentPlanAnnouncePredecessorStatus announce) <$> prior) Set.empty)
          plan <- case prepared of
            Plan.AlignmentPlanReady value -> shape (Plan.checkAlignmentPlanAnnouncement value announce)
            Plan.AlignmentPlanHeld missing -> Left ("unexpected historical certificates: " <> show missing)
          shape (Alignment.retainHistoricalAlignmentRoutingPlan joiningHerald plan state)
        reconstructed = checked "reconstruct historical attempt chain" (foldM reconstruct emptyState identifiers)
        importCoverage state (key, _) = do
          plan <- maybe (Left "coverage lost its exact plan attempt") Right (Alignment.lookupAlignmentPlan (Alignment.alignmentPromotionKeyPlanId key) state)
          shape (Alignment.retainHistoricalAlignmentPlanCoverage joiningHerald key plan state)
        covered = checked "import exact attempted cause coverage" (foldM importCoverage (foldl (flip retainCoverageDebt) reconstructed [coverageDonorDebt, carriedDebt]) (History.alignmentHistoryCoverage decoded))
        adopted = checked "adopt attempted historical frontier" (Alignment.adoptHistoricalAlignmentPlanFrontier joiningHerald (History.alignmentHistoryFrontier decoded) covered)
        oldIds = Set.fromList (map alignmentGenerationId (routingGenerations original))
        newIds = Set.fromList (map alignmentGenerationId (routingGenerations recovered))
        inventory = Map.fromList (Diagnostics.alignmentInventoryCounts (map snd (Alignment.alignmentGenerationEntries donor)) (Alignment.alignmentGenerationRelationEntries donor))
     in counterexample ("history attempt " <> show ordinal)
          $ conjoin
            [ Alignment.alignmentPlanInvalidated (Plan.alignmentPlanId original) donor === True,
              Alignment.alignmentPlanInvalidated (Plan.alignmentPlanId recovered) donor === False,
              Set.null (Set.intersection oldIds newIds) === True,
              routingGenerations carried === routingGenerations recovered,
              all ((== attempt) . DomainAlignment.alignmentCutAttempt . alignmentGenerationCut) (routingGenerations carried) === True,
              Set.fromList (map (Alignment.alignmentPromotionKeyPlanId . fst) (History.alignmentHistoryCoverage decoded)) === Set.fromList (map Plan.alignmentPlanId [recovered, carried]),
              History.decodeAlignmentHistory (History.encodeAlignmentHistory captured) === Right captured,
              History.captureAlignmentHistory adopted === captured,
              Alignment.historicalAlignmentPlanFrontier adopted === [(typedSort, Plan.alignmentPlanId carried)],
              Map.lookup "alignment.inventory.plan_coordinates.total" inventory === Just 2,
              Alignment.validateAlignmentState donor === Right (),
              Alignment.validateAlignmentState adopted === Right ()
            ]
  where
    shape result = either (Left . show) Right result

-- Keep two independently owned relations alive across an ordinary routing
-- change, including a closing relation and all fresh-base bootstrap work.
-- Reset must cancel their concrete loopback transfers as one owner transition.
propResetPlanOwners :: Property
propResetPlanOwners = forAll (chooseInt (1, 5)) $ \ordinal ->
  let attempt = iterate DomainAlignment.nextAlignmentPlanAttempt DomainAlignment.initialAlignmentPlanAttempt !! ordinal
      members = [generationMemberInput deltaA storeA heraldB initialStoreRevision, generationMemberInput deltaB storeB heraldB initialStoreRevision]
      preparedPlan selectedTopology context prior = ready (Plan.prepareAlignmentPlan typedSort selectedTopology placement context members ((,Plan.AlignmentPredecessorUsable) <$> prior) (maybe Set.empty (Set.fromList . map alignmentGenerationId . routingGenerations) prior))
      original = preparedPlan topology firstContext Nothing
      successor = preparedPlan historySuccessorTopology reverseContext (Just original)
      reset = ready (Plan.prepareAlignmentResetPlan attempt typedSort historySuccessorTopology placement reverseContext members)
      laterCause = checked "reset successor cause" (processEndCause endedProcess (controlIndex 7))
      laterDebt = structuralConsequenceDebt (structuralDebtKey laterCause TopologyAlignmentDebt typedSort Nothing) (structuralDebtEvidence Set.empty Set.empty Set.empty)
      promote cause plan = fst . commitAlignmentPromotion . checked "reset fixture promotion" . Alignment.prepareAlignmentPlanPromotion heraldB cause plan
      accept plan = fst . checked "reset fixture plan acceptance" . Alignment.retainAlignmentPlanAcceptance (checked "reset fixture accepted" (Protocol.alignmentPlanAccepted (Plan.alignmentPlanId plan) heraldB EmptyHeraldPublicationPrefix))
      firstOwner = promote coverageCause original (retainCoverageDebt coverageDonorDebt emptyState)
      firstReady = foldl certify (accept original firstOwner) (routingGenerations original)
      firstActive = startAttempts firstReady
      secondOwner = promote laterCause successor (retainCoverageDebt laterDebt firstActive)
      secondReady = foldl certify (accept successor secondOwner) (routingGenerations successor)
      before = startLoopbacks (startAttempts secondReady)
      report = checked "reset fixture obsolete report" (Protocol.alignmentPlanObsolete (Plan.alignmentPlanId successor) heraldA (Protocol.alignmentLostStore heraldB deltaA obsoleteStore firstPlacementRevision :| []))
      reported = fst (checked "retain reset report" (Alignment.retainAlignmentPlanObsolete report before))
      preparation = checked "prepare fresh owner reset" (Alignment.prepareAlignmentPlanResetPromotion heraldB (coverageCause :| [laterCause]) reset reported)
      (after, disposition) = Alignment.commitAlignmentPlanResetPromotion preparation
      cancellations = Alignment.preparedAlignmentPlanResetPromotionCancellations preparation
      oldGenerationIds = Set.fromList (map fst (alignmentGenerationEntries before))
      newGenerationIds = Set.fromList (map alignmentGenerationId (routingGenerations reset))
      oldSubscriptions = Set.fromList (map fst (Transfer.destinationSubscriptionEntries (Alignment.alignmentTransferState before)))
      cancelledDestinations = Transfer.destinationSubscriptionEntries (Alignment.alignmentTransferState after)
      cancelledSources = Transfer.sourceSubscriptionEntries (Alignment.alignmentTransferState after)
      frozenEvidence :: (Eq evidence) => (Alignment.State -> evidence) -> Bool
      frozenEvidence project = project before == project after
      replay = checked "replay exact owner reset" (Alignment.prepareAlignmentPlanResetPromotion heraldB (coverageCause :| [laterCause]) reset after)
   in counterexample ("reset attempt " <> show ordinal)
        $ conjoin
          [ length (alignmentObligationEntries before) === 2,
            counterexample "ordinary owner attempts are installed" (length (Alignment.alignmentAttemptEntries before) === 2),
            length (Alignment.alignmentClosingObligationIds before) === 1,
            -- The original two fresh bases remain; Q imports A into A,
            -- and both B and its replaced incoming source A into B.
            counterexample "old and successor bootstrap owners" (length (Alignment.bootstrapImportEntries before) === 5),
            counterexample "ordinary and bootstrap subscriptions" (Set.size oldSubscriptions === 7),
            disposition === AlignmentPromotionCommitted,
            Alignment.preparedAlignmentPlanResetPromotionDisposition replay === AlignmentPromotionUnchanged,
            fst (Alignment.commitAlignmentPlanResetPromotion replay) === after,
            Set.fromList (map Protocol.alignmentCancelSubscriptionId cancellations) === oldSubscriptions,
            all ((== Protocol.AlignmentRelationRemoved) . Protocol.alignmentCancelReason) cancellations === True,
            all ((/= Nothing) . Transfer.destinationSubscriptionCancellation . snd) cancelledDestinations === True,
            all ((/= Nothing) . Transfer.sourceSubscriptionCancellation . snd) cancelledSources === True,
            Set.fromList (map fst cancelledSources) === oldSubscriptions,
            Set.null (Set.intersection oldGenerationIds newGenerationIds) === True,
            all ((== Plan.alignmentPlanId reset) . Alignment.generationBirthPlanId . snd) (filter ((`Set.member` newGenerationIds) . fst) (alignmentGenerationEntries after)) === True,
            all (\plan -> Alignment.alignmentPlanInvalidated (Plan.alignmentPlanId plan) after) [original, successor] === True,
            map (Plan.alignmentPlanId . snd) (filter ((== Plan.alignmentPlanId reset) . fst) (Alignment.alignmentPlanEntries after)) === [Plan.alignmentPlanId reset],
            all (\(_, obligation) -> Set.member (Protocol.alignmentObligationDestinationGeneration obligation) newGenerationIds) (alignmentObligationEntries after) === True,
            all (\(key, _) -> Set.member (Alignment.bootstrapImportKeyGeneration key) newGenerationIds) (Alignment.bootstrapImportEntries after) === True,
            Alignment.alignmentAttemptEntries after === [],
            Alignment.bootstrapImportAttemptEntries after === [],
            length (Alignment.alignmentAttemptInvalidationEntries after) === 2,
            counterexample "every bootstrap subscription receives a terminal receipt" (length (Alignment.bootstrapImportAttemptInvalidationEntries after) === 5),
            liveDebtKeys after === Set.empty,
            Alignment.alignmentPlanObsoleteEntries after === [],
            Alignment.generationPlanWorkIds after === Set.empty,
            Alignment.alignmentUnavailableSources after === Alignment.alignmentUnavailableSources before,
            frozenEvidence Alignment.alignmentCutAcceptanceEntries === True,
            frozenEvidence Alignment.alignmentMemberReadinessEntries === True,
            frozenEvidence Alignment.alignmentHistoricalCertificateEntries === True,
            all (\(identifier, generation) -> lookup identifier (alignmentGenerationEntries after) == Just generation) (alignmentGenerationEntries before) === True,
            fst (checked "late obsolete report" (Alignment.retainAlignmentPlanObsolete report after)) === after,
            Alignment.validateAlignmentState before === Right (),
            Alignment.validateAlignmentState after === Right ()
          ]
  where
    ready result = case checked "reset routing preparation" result of
      Plan.AlignmentPlanReady plan -> plan
      Plan.AlignmentPlanHeld missing -> error ("unexpected reset fixture prerequisites: " <> show missing)
    obsoleteStore = checked "older Store incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 0xbb))
    certify state generation =
      let identifier = alignmentGenerationId generation
          source = case DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut generation) of
            member :| [] -> DomainAlignment.alignmentMemberStoreIncarnation member
            _ -> error "reset fixture classes must be singletons"
          memberReady = classMemberReady firstAlignmentEvidenceSequence identifier source (deriveMemberReadyEvidenceDigest "reset fixture base") initialStoreRevision
          withReady = fst (commitClassMemberReady (checked "reset readiness" (prepareClassMemberReady memberReady state)))
          certificate = historicalCertificate identifier source (classMemberReadySetDigest [memberReady]) (deriveBootstrapEvidenceDigest (memberReadyEvidenceDigestBytes (classMemberReadyPredecessorAndBasePrefixDigest memberReady))) initialStoreRevision
       in fst (commitHistoricalCertificate (checked "reset historical certificate" (prepareHistoricalCertificate certificate withReady)))
    startAttempts state =
      let ordinary = foldl startOrdinary state (alignmentObligationEntries state)
       in foldl (\owner (key, _) -> fst (Alignment.commitBootstrapImportAttempt (checked "reset bootstrap attempt" (Alignment.prepareBootstrapImportAttempt key owner)))) ordinary (Alignment.bootstrapImportEntries ordinary)
    -- Ordinary attempts and their destination transfer are separate owners;
    -- compose the same two checked preparations as the runtime coordinator.
    startOrdinary state (identifier, _) =
      let prepared = checked "reset ordinary attempt" (prepareAlignmentAttempt identifier state)
          owner = fst (commitAlignmentAttempt prepared)
          destination = checked "reset ordinary destination" (Transfer.prepareDestinationAttempt (Alignment.preparedAlignmentAttempt prepared) (Alignment.alignmentTransferState owner))
          transfer = fst (Transfer.commitDestinationAttempt destination)
       in Alignment.replaceAlignmentTransferState transfer owner
    startLoopbacks state =
      let destinations = Transfer.destinationSubscriptionEntries (Alignment.alignmentTransferState state)
          transfer = foldl (\owner (_, destination) -> fst (Transfer.commitSourceSubscription (checked "reset loopback source" (Transfer.prepareSourceSubscription (Transfer.destinationSubscriptionSubscribe destination) initialStoreRevision [] Transfer.SourceLiveImmediate owner)))) (Alignment.alignmentTransferState state) destinations
       in Alignment.replaceAlignmentTransferState transfer state

propObsoleteReportScheduling :: Property
propObsoleteReportScheduling = forAll (chooseInt (0, 8)) $ \duplicates ->
  let identifier = Plan.alignmentPlanId coveragePlan
      first = checked "first frozen report" (Protocol.alignmentPlanObsolete identifier heraldA (Protocol.alignmentLostStore heraldA deltaA storeA firstPlacementRevision :| []))
      later = checked "later loss report" (Protocol.alignmentPlanObsolete identifier heraldA (Protocol.alignmentLostStore heraldB deltaB storeB firstPlacementRevision :| []))
      retain report = fst . checked "retain retry-only report" . Alignment.retainAlignmentPlanObsolete report
      retained = retain first emptyState
      consumed = Alignment.consumeGenerationPlanWork (Set.singleton typedSort) retained
      repeated = iterate (retain later) consumed !! duplicates
      woken = Alignment.wakeGenerationPlanInputs repeated
   in conjoin
        [ Alignment.alignmentPlanObsoleteEntries repeated === [first],
          Alignment.lookupAlignmentPlan identifier repeated === Nothing,
          liveDebtKeys repeated === Set.empty,
          Alignment.generationPlanWorkIds repeated === Set.singleton typedSort,
          Alignment.alignmentResetRequiredSorts repeated === Set.singleton typedSort,
          Alignment.pendingGenerationPlanWork repeated === Set.empty,
          Alignment.pendingGenerationPlanWork woken === Set.singleton typedSort,
          repeated === consumed,
          Alignment.validateAlignmentState repeated === Right ()
        ]

-- Report delivery can overtake the author's plan on a different peer stream.
-- A report is evidence for later recovery, not an adopted generation frontier.
propObsoleteReportBeforeReset :: Property
propObsoleteReportBeforeReset = forAll (chooseInt (0, 2)) $ \reportLead ->
  let attempt = DomainAlignment.nextAlignmentPlanAttempt DomainAlignment.initialAlignmentPlanAttempt
      reportedAttempt = iterate DomainAlignment.nextAlignmentPlanAttempt attempt !! reportLead
      members = [generationMemberInput deltaA storeA heraldA initialStoreRevision, generationMemberInput deltaB storeB heraldB initialStoreRevision]
      reset = case checked "prepare overtaken reset" (Plan.prepareAlignmentResetPlan attempt typedSort topology placement firstContext members) of
        Plan.AlignmentPlanReady plan -> plan
        Plan.AlignmentPlanHeld missing -> error ("unexpected overtaken reset prerequisites: " <> show missing)
      owner = fst (commitAlignmentPromotion (checked "promote original before overtaking" (Alignment.prepareAlignmentPlanPromotion heraldA coverageCause coveragePlan (retainCoverageDebt coverageDonorDebt emptyState))))
      reportedId = Plan.alignmentPlanIdFromClaimedCoordinatesAtAttempt reportedAttempt typedSort topologyId placement
      report = checked "future reset report" (Protocol.alignmentPlanObsolete reportedId heraldB (Protocol.alignmentLostStore heraldA deltaA storeA firstPlacementRevision :| []))
      reported = fst (checked "report overtakes reset" (Alignment.retainAlignmentPlanObsolete report owner))
      prepared = checked "admit reset despite overtaking report" (Alignment.prepareAlignmentPlanResetPromotion heraldA (coverageCause :| []) reset reported)
      (after, disposition) = Alignment.commitAlignmentPlanResetPromotion prepared
   in conjoin
        [ disposition === AlignmentPromotionCommitted,
          fmap Plan.alignmentPlanId (Alignment.currentAlignmentPlanForSort typedSort after) === Just (Plan.alignmentPlanId reset),
          Alignment.alignmentPlanObsoleteEntries after === [report],
          liveDebtKeys after === Set.empty,
          Alignment.alignmentResetRequiredSorts after === Set.singleton typedSort,
          Alignment.generationPlanWorkIds after === Set.singleton typedSort,
          Alignment.pendingGenerationPlanWork after === Set.singleton typedSort,
          Alignment.validateAlignmentState after === Right ()
        ]

-- A surviving receiver may know a reset which the next announcer never saw.
-- Membership authority must order that new announcer's first reset after every
-- ordinal from the retired announcer, including delayed reports for unknown
-- old plans. Exercise the checked owner against an actual contraction cut.
caseResetAfterAnnouncerMembershipChange :: Assertion
caseResetAfterAnnouncerMembershipChange = do
  let change = controlIndex 2
      oldAttempt = DomainAlignment.mkAlignmentPlanAttempt 99
      oldMembers = [generationMemberInput deltaA storeA heraldA initialStoreRevision, generationMemberInput deltaB storeB heraldB initialStoreRevision]
      oldPlan = ready (Plan.prepareAlignmentResetPlan oldAttempt typedSort topology placement firstContext oldMembers)
      before = fst (Alignment.commitAlignmentPlanResetPromotion (checked "install old announcer reset" (Alignment.prepareAlignmentPlanResetPromotion heraldB (coverageCause :| []) oldPlan (retainCoverageDebt coverageDonorDebt emptyState))))
      nextMembership = checked "retire the previous announcer" (Membership.retireHeraldMembershipGeneration change (fixtureRetirementResolution change) heraldA membership)
      history = checked "reset membership history" (Membership.heraldMembershipHistory (membership :| [nextMembership]))
      lineage = checked "reset membership lineage" (Membership.heraldMembershipLineage (Membership.heraldMembershipGenerationId membership) (Membership.heraldMembershipGenerationId nextMembership) history)
      successorVector = emptyStructuralVersionVector nextMembership
      unionDigest = checked "reset terminal union digest" (Topology.mkTerminalSourceUnionDigest (fixtureIdentifierBytes 0xe7))
      predecessor = checked "reset contraction predecessor" (Topology.membershipSuccessorPredecessor lineage topologyId emptyVector successorVector unionDigest)
      successorTopology = checked "reset contraction topology" (topologyCut predecessor (topologyFrontier successorVector change) (deriveTopologyOccurrenceDigest "alignment reset after announcer retirement"))
      successorPlacement = checked "survivor placement" (physicalPlacementRevisionVector nextMembership [(heraldB, firstPlacementRevision)])
      successorContext = checked "surviving class" (deriveContextGraph [DeltaVertex deltaB] [] [deltaB])
      attempt = DomainAlignment.mkAlignmentPlanAttemptAtMembership change 1
      replacement = ready (Plan.prepareAlignmentResetPlan attempt typedSort successorTopology successorPlacement successorContext [generationMemberInput deltaB storeB heraldB initialStoreRevision])
      (after, disposition) = Alignment.commitAlignmentPlanResetPromotion (checked "replace unseen old-author counter" (Alignment.prepareAlignmentPlanResetPromotion heraldB (coverageCause :| []) replacement before))
      lateIdentifier = Plan.alignmentPlanIdFromClaimedCoordinatesAtAttempt (DomainAlignment.mkAlignmentPlanAttempt 100) typedSort topologyId placement
      lateReport = checked "delayed old-author report" (Protocol.alignmentPlanObsolete lateIdentifier heraldA (Protocol.alignmentLostStore heraldA deltaA storeA firstPlacementRevision :| []))
      (afterLateReport, reportDisposition) = checked "ignore report from the prior membership epoch" (Alignment.retainAlignmentPlanObsolete lateReport after)
      oldGenerations = Set.fromList (map alignmentGenerationId (routingGenerations oldPlan))
      newGenerations = Set.fromList (map alignmentGenerationId (routingGenerations replacement))
  assertEqual "the retired member was the original fixed announcer" (Just heraldA) (Plan.alignmentPlanAnnouncer oldPlan)
  assertEqual "the survivor owns the replacement announcement" (Just heraldB) (Plan.alignmentPlanAnnouncer replacement)
  assertBool "later membership authority dominates the old ordinal" (oldAttempt < attempt)
  assertEqual "the new epoch commits its first reset" AlignmentPromotionCommitted disposition
  assertEqual "the receiver selects the new announcer's exact plan" (Just (Plan.alignmentPlanId replacement)) (Plan.alignmentPlanId <$> Alignment.currentAlignmentPlanForSort typedSort after)
  assertBool "the old plan stays terminal" (Alignment.alignmentPlanInvalidated (Plan.alignmentPlanId oldPlan) after)
  assertBool "the replacement never reuses old generation identities" (Set.disjoint oldGenerations newGenerations)
  assertEqual "a larger ordinal from the retired epoch is stale" Alignment.AlignmentCutEvidenceUnchanged reportDisposition
  assertEqual "stale reports cannot create another reset" after afterLateReport
  assertEqual "the old exact plan remains in retained history" (Just oldPlan) (Alignment.lookupAlignmentPlan (Plan.alignmentPlanId oldPlan) after)
  assertEqual "the old receiver owner validates" (Right ()) (Alignment.validateAlignmentState before)
  assertEqual "the replacement owner validates" (Right ()) (Alignment.validateAlignmentState after)
  where
    ready result = case checked "membership reset plan" result of
      Plan.AlignmentPlanReady plan -> plan
      Plan.AlignmentPlanHeld missing -> error ("unexpected membership reset prerequisites: " <> show missing)

data HistoricalCoverageEvent
  = ImportCoverage
  | RetainCoverageDebt Debt.StructuralConsequenceDebt
  | InvalidateCoverage
  deriving stock (Eq, Show)

propHistoricalCoverageArrival :: Property
propHistoricalCoverageArrival =
  forAll (shuffle events) $ \arrivals ->
    let actual = foldl applyCoverageEvent emptyState arrivals
        retained = foldl (flip retainCoverageDebt) emptyState coverageDebts
        expected = checked "coverage after complete receiver replay" (Alignment.retainHistoricalAlignmentPlanCoverage joiningHerald coverageKey coveragePlan retained)
     in counterexample ("coverage/debt schedule: " <> show arrivals)
          $ conjoin
            [ actual === expected,
              liveDebtKeys actual === uncoveredDebtKeys retained,
              passiveCoverageNoWork actual === True,
              Alignment.validateAlignmentState actual === Right ()
            ]
  where
    events = [ImportCoverage, ImportCoverage] <> concatMap (\debt -> [RetainCoverageDebt debt, RetainCoverageDebt debt]) coverageDebts

propHistoricalCoverageInvalidation :: Property
propHistoricalCoverageInvalidation =
  forAll (shuffle events) $ \arrivals ->
    let initial = checked "passive invalidation fixture" (Alignment.retainHistoricalAlignmentRoutingPlan joiningHerald coveragePlan emptyState)
        states = scanl applyCoverageEvent initial arrivals
        actual = last states
        retained = foldl (flip retainCoverageDebt) emptyState coverageDebts
        replayed = checked "replay tombstoned historical coverage" (Alignment.retainHistoricalAlignmentPlanCoverage joiningHerald coverageKey coveragePlan actual)
        retry = Alignment.prepareAlignmentPlanPromotion joiningHerald coverageCause coveragePlan actual
        exactTombstone = case retry of
          Left (Alignment.AlignmentPromotionPlanInvalidated coordinate) ->
            Alignment.alignmentPlanCoordinateSort coordinate == typedSort
              && Alignment.alignmentPlanCoordinateTopologyCut coordinate == topologyId
              && Alignment.alignmentPlanCoordinatePlacementVector coordinate == placement
          _ -> False
     in counterexample ("coverage/loss/debt schedule: " <> show arrivals)
          $ conjoin
            [ liveDebtKeys actual === liveDebtKeys retained,
              Alignment.alignmentCompletedCauseCoverage actual === [],
              Alignment.alignmentCauseCoverageKeys actual === [coverageKey],
              Alignment.alignmentCauseCoverageKeysFor coverageCause typedSort actual === [coverageKey],
              replayed === actual,
              exactTombstone === True,
              passiveCoverageNoWork actual === True,
              History.alignmentHistoryCoverage (History.captureAlignmentHistory actual) === [],
              conjoin
                [ conjoin
                    [ Alignment.validateAlignmentState state === Right (),
                      Alignment.alignmentCauseCoverageKeysFor coverageCause typedSort state === referenceCoverageKeysFor coverageCause typedSort state,
                      Alignment.alignmentCauseCoverageKeysFor coverageCause missingSort state === [],
                      Alignment.alignmentPlanIsInvalidated typedSort topologyId placement state === referencePlanInvalidated typedSort topologyId placement state,
                      Alignment.alignmentPlanIsInvalidated typedSort predecessorCut placement state === False,
                      Alignment.alignmentPlanIsInvalidated missingSort topologyId placement state === False
                    ]
                | state <- states
                ]
            ]
  where
    events = [ImportCoverage, InvalidateCoverage, ImportCoverage, InvalidateCoverage] <> map RetainCoverageDebt coverageDebts
    missingSort = sortOccurrence sortId (checked "missing coverage sort" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xfe)))

data CoverageQueryEvent
  = PromoteQueryCoverage Alignment.AlignmentPromotionKey
  | ImportQueryCoverage Alignment.AlignmentPromotionKey
  deriving stock (Eq, Show)

-- The removal-only plans deliberately have no generations: their ordinary
-- receipts and passive historical receipts are both real admitted completion
-- facts, including an exact key retained by both maps. Nonempty invalidated
-- historical plans are exercised separately by the property above.
propCoverageRangeQueries :: Property
propCoverageRangeQueries =
  forAll (chooseInt (1, 3)) $ \repetitions ->
    forAll (shuffle (concatMap (replicate repetitions) events)) $ \arrivals ->
      let states = scanl applyQueryEvent emptyState arrivals
          actual = last states
          replayed = foldl applyQueryEvent actual events
          overlapKey = case keys of
            key : _ -> key
            [] -> error "coverage query fixture has no keys"
          promoted = applyQueryEvent emptyState (PromoteQueryCoverage overlapKey)
          overlapping = applyQueryEvent promoted (ImportQueryCoverage overlapKey)
       in counterexample ("coverage query schedule: " <> show arrivals)
            $ conjoin
              [ Alignment.alignmentCauseCoverageKeys actual === keys,
                map fst (Alignment.alignmentPromotionEntries actual) === promotionKeys,
                replayed === actual,
                (overlapping /= promoted) === True,
                Alignment.alignmentCauseCoverageKeys overlapping === [overlapKey],
                conjoin
                  [ conjoin
                      ( (Alignment.validateAlignmentState state === Right ())
                          : [ Alignment.alignmentCauseCoverageKeysFor retainedCause affectedSort state === referenceCoverageKeysFor retainedCause affectedSort state
                            | retainedCause <- causes <> [missingCause],
                              affectedSort <- sorts <> [missingSort]
                            ]
                      )
                  | state <- states
                  ]
              ]
  where
    causes = [structuralOccurrenceCause (structuralOccurrenceId heraldA (checked "coverage query sequence" (mkStructuralSequence sequenceNumber))) | sequenceNumber <- [1, 2, 3]]
    sorts = [sortOccurrence sortId (checked "coverage query sort" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes identifier))) | identifier <- [0x91, 0xa2, 0xb3]]
    missingCause = structuralOccurrenceCause (structuralOccurrenceId heraldA (checked "absent coverage query sequence" (mkStructuralSequence 4)))
    missingSort = sortOccurrence sortId (checked "absent coverage query sort" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xaf)))
    otherTopology = checked "coverage query successor topology" (topologyCut (sameGenerationPredecessor topologyId) (topologyFrontier emptyVector (controlIndex 0)) (deriveTopologyOccurrenceDigest "coverage-query-successor"))
    topologies = Map.fromList [(topologyId, topology), (deriveTopologyCutId otherTopology, otherTopology)]
    keys = Set.toAscList (Set.fromList [alignmentPromotionKey retainedCause affectedSort selectedTopology placement | retainedCause <- causes, affectedSort <- sorts, selectedTopology <- Map.keys topologies])
    indexedKeys = zip [0 :: Int ..] keys
    promotionKeys = [key | (index, key) <- indexedKeys, index `mod` 3 /= 2]
    events = concatMap eventsFor indexedKeys
    eventsFor (index, key) = case index `mod` 3 of
      0 -> [PromoteQueryCoverage key, ImportQueryCoverage key]
      1 -> [PromoteQueryCoverage key]
      _ -> [ImportQueryCoverage key]
    planFor key =
      let affectedSort = Alignment.alignmentPromotionKeySort key
          selectedTopology = topologies Map.! Alignment.alignmentPromotionKeyTopologyCut key
          emptyContext = checked "coverage query removal context" (deriveContextGraph [] [] [])
       in case prepareAlignmentGenerationPlan affectedSort selectedTopology placement emptyContext [] [] [] Set.empty of
            Right (AlignmentGenerationReady plan) -> plan
            result -> error ("coverage query removal plan: " <> show result)
    applyQueryEvent state event = case event of
      ImportQueryCoverage key -> checked "import query coverage" (Alignment.retainHistoricalAlignmentCoverage joiningHerald key (planFor key) state)
      PromoteQueryCoverage key ->
        let retainedCause = Alignment.alignmentPromotionKeyCause key
            affectedSort = Alignment.alignmentPromotionKeySort key
            debt = structuralConsequenceDebt (structuralDebtKey retainedCause TopologyAlignmentDebt affectedSort Nothing) (structuralDebtEvidence Set.empty Set.empty Set.empty)
         in fst (commitAlignmentPromotion (checked "promote query coverage" (prepareAlignmentPromotion joiningHerald retainedCause affectedSort (Alignment.alignmentPromotionKeyTopologyCut key) placement (planFor key) (retainCoverageDebt debt state))))

-- Frozen exhaustive coordinator queries. These deliberately retain the full
-- coverage-key union and receipt scan rather than using either keyed query.
referenceCoverageKeysFor :: StructuralConsequenceCause -> SortOccurrence -> Alignment.State -> [Alignment.AlignmentPromotionKey]
referenceCoverageKeysFor retainedCause affectedSort state =
  [ key
  | key <- Alignment.alignmentCauseCoverageKeys state,
    Alignment.alignmentPromotionKeyCause key == retainedCause,
    Alignment.alignmentPromotionKeySort key == affectedSort
  ]

referencePlanInvalidated :: SortOccurrence -> TopologyCutId -> PhysicalPlacementRevisionVector -> Alignment.State -> Bool
referencePlanInvalidated affectedSort selectedTopology selectedPlacement state =
  any
    ( \(_, receipt) ->
        let coordinate = Alignment.alignmentPlanInvalidationReceiptCoordinate receipt
         in Alignment.alignmentPlanCoordinateSort coordinate == affectedSort
              && Alignment.alignmentPlanCoordinateTopologyCut coordinate == selectedTopology
              && Alignment.alignmentPlanCoordinatePlacementVector coordinate == selectedPlacement
    )
    (Alignment.alignmentPlanInvalidationEntries state)

caseHistoricalEmptyCoverage :: Assertion
caseHistoricalEmptyCoverage = do
  let plan = emptyRoutingPlan topology Nothing
      retained = retainCoverageDebt coverageDonorDebt emptyState
      donor = fst (commitAlignmentPromotion (checked "ordinary removal-only promotion" (Alignment.prepareAlignmentPlanPromotion heraldB coverageCause plan retained)))
      imported = checked "passive removal-only coverage" (Alignment.retainHistoricalAlignmentPlanCoverage joiningHerald coverageKey plan retained)
      captured = History.captureAlignmentHistory imported
  assertEqual "the real removal-only plan has no generations" [] (routingGenerations plan)
  assertEqual "ordinary removal completion is explicit" [(coverageKey, [])] (Alignment.alignmentCompletedCauseCoverage donor)
  assertEqual "passive removal completion preserves that exact fact" (Alignment.alignmentCompletedCauseCoverage donor) (Alignment.alignmentCompletedCauseCoverage imported)
  assertEqual "removal-only completion covers receiver debt" Set.empty (liveDebtKeys imported)
  assertBool "empty coverage creates no owner work or frontier" (passiveCoverageNoWork imported)
  assertEqual "capture retains the exact empty plan without fake generations" [Plan.alignmentPlanAnnouncement plan] (History.alignmentHistoryPlans captured)
  assertEqual "removal-only capture preserves cause" [(coverageKey, [])] (History.alignmentHistoryCoverage captured)
  assertEqual "empty plan catalogue roundtrips" (Right captured) (History.decodeAlignmentHistory (History.encodeAlignmentHistory captured))
  assertEqual "empty coverage replay remains unchanged" (Right imported) (Alignment.retainHistoricalAlignmentPlanCoverage joiningHerald coverageKey plan imported)
  assertEqual "empty coverage satisfies owner invariants" (Right ()) (Alignment.validateAlignmentState imported)

caseHistoricalAdmissionProof :: Assertion
caseHistoricalAdmissionProof = do
  let plan = coveragePlan
      identifier = Plan.alignmentPlanId plan
      imported = checked "passive admission plan" (Alignment.retainHistoricalAlignmentRoutingPlan joiningHerald plan emptyState)
      adopted = checked "adopt proof fixture frontier" (Alignment.adoptHistoricalAlignmentPlanFrontier joiningHerald [(typedSort, identifier)] imported)
      history = History.captureAlignmentHistory adopted
      proofs members = [checked "plan admission proof" (Protocol.alignmentPlanAccepted identifier member EmptyHeraldPublicationPrefix) | member <- members]
      oneOwner = checked "one owner's inherited proof" (History.withAlignmentHistoryPlanAcceptances (proofs [heraldA]) history)
      complete = checked "all original owners' inherited proof" (History.withAlignmentHistoryPlanAcceptances (proofs [heraldA, heraldB]) history)
      emptyPlan = emptyRoutingPlan historySuccessorTopology (Just plan)
      emptyImported = checked "empty plan import" (Alignment.retainHistoricalAlignmentRoutingPlan joiningHerald emptyPlan imported)
      empty = History.captureAlignmentHistory (checked "explicit empty frontier" (Alignment.adoptHistoricalAlignmentPlanFrontier joiningHerald [(typedSort, Plan.alignmentPlanId emptyPlan)] emptyImported))
      emptyProofs = [checked "empty plan admission proof" (Protocol.alignmentPlanAccepted (Plan.alignmentPlanId emptyPlan) member EmptyHeraldPublicationPrefix) | member <- [heraldA, heraldB]]
      emptyAdmitted = checked "empty plan inherited admission" (History.withAlignmentHistoryPlanAcceptances emptyProofs empty)
      emptyWrongProofs = checked "old plan inherited admission" (History.withAlignmentHistoryPlanAcceptances (proofs [heraldA, heraldB]) empty)
  assertBool "frontier without admission cannot be handed off" (not (JoinHistory.alignmentFrontierAdmitted [heraldA, heraldB] history))
  assertBool "one proof cannot speak for another surviving owner" (not (JoinHistory.alignmentFrontierAdmitted [heraldA, heraldB] oneOwner))
  assertBool "retired original member does not block handoff" (JoinHistory.alignmentFrontierAdmitted [heraldA] oneOwner)
  assertBool "all surviving fixed members admit the frontier" (JoinHistory.alignmentFrontierAdmitted [heraldA, heraldB] complete)
  assertEqual "admission does not demand readiness" [] (History.alignmentHistoryReadiness complete)
  assertEqual "admission does not demand certificates" [] (History.alignmentHistoryCertificates complete)
  assertBool "empty current plan still requires exact frontier acceptance" (not (JoinHistory.alignmentFrontierAdmitted [heraldA, heraldB] empty))
  assertBool "predecessor plan acceptance cannot admit an empty successor" (not (JoinHistory.alignmentFrontierAdmitted [heraldA, heraldB] emptyWrongProofs))
  assertBool "empty current plan admits with its own fixed-member acceptance" (JoinHistory.alignmentFrontierAdmitted [heraldA, heraldB] emptyAdmitted)
  assertEqual "empty plan admission needs no fictional class readiness" [] (History.alignmentHistoryReadiness emptyAdmitted)
  assertEqual "empty plan admission needs no fictional class certificates" [] (History.alignmentHistoryCertificates emptyAdmitted)
  assertEqual "empty frontier acceptance survives canonical history encoding" (Right emptyAdmitted) (History.decodeAlignmentHistory (History.encodeAlignmentHistory emptyAdmitted))
  assertEqual "inherited proof creates no observer votes" [] (Alignment.alignmentPlanAcceptanceEntries adopted)
  assertEqual "proof enrichment is idempotent" (Right complete) (History.withAlignmentHistoryPlanAcceptances (History.alignmentHistoryPlanAcceptances complete) (History.captureAlignmentHistory adopted))
  assertBool "foreign acceptor is rejected at protocol admission" (isRejected (Protocol.alignmentPlanAccepted identifier joiningHerald EmptyHeraldPublicationPrefix))
  let unrelated = checked "other-plan proof" (Protocol.alignmentPlanAccepted (Plan.alignmentPlanId emptyPlan) heraldA EmptyHeraldPublicationPrefix)
  assertEqual "another plan's proof cannot admit this frontier" (Right history) (History.withAlignmentHistoryPlanAcceptances [unrelated] history)
  assertEqual "codec preserves exact admission evidence" (Right complete) (History.decodeAlignmentHistory (History.encodeAlignmentHistory complete))

coveragePlan :: Plan.AlignmentPlan
coveragePlan = routingPlan topology firstContext Nothing Set.empty

routingGenerations :: Plan.AlignmentPlan -> [AlignmentGeneration]
routingGenerations = map (Plan.alignmentPlanBindingGeneration . snd) . Plan.alignmentPlanBindings

routingPlan :: TopologyCut -> ContextGraph -> Maybe Plan.AlignmentPlan -> Set.Set ContextClassGenerationId -> Plan.AlignmentPlan
routingPlan selectedTopology context prior certificates = case Plan.prepareAlignmentPlan typedSort selectedTopology placement context [generationMemberInput deltaA storeA heraldA initialStoreRevision, generationMemberInput deltaB storeB heraldB initialStoreRevision] ((,Plan.AlignmentPredecessorUsable) <$> prior) certificates of
  Right (Plan.AlignmentPlanReady plan) -> plan
  other -> error ("historical routing plan fixture: " <> show other)

emptyRoutingPlan :: TopologyCut -> Maybe Plan.AlignmentPlan -> Plan.AlignmentPlan
emptyRoutingPlan selectedTopology prior = case Plan.prepareAlignmentPlan typedSort selectedTopology placement (checked "empty context" (deriveContextGraph [] [] [])) [] ((,Plan.AlignmentPredecessorUsable) <$> prior) Set.empty of
  Right (Plan.AlignmentPlanReady plan) -> plan
  other -> error ("historical empty routing plan fixture: " <> show other)

historySuccessorTopology :: TopologyCut
historySuccessorTopology = checked "historical successor topology" (topologyCut (sameGenerationPredecessor topologyId) (topologyFrontier emptyVector (controlIndex 1)) (deriveTopologyOccurrenceDigest "historical successor"))

coverageCause :: StructuralConsequenceCause
coverageCause = structuralOccurrenceCause occurrence

coverageKey :: Alignment.AlignmentPromotionKey
coverageKey = alignmentPromotionKey coverageCause typedSort topologyId placement

coverageDebts :: [Debt.StructuralConsequenceDebt]
coverageDebts =
  [ coverageDonorDebt,
    structuralConsequenceDebt
      (structuralDebtKey coverageCause Debt.DestinationAlignmentDebt typedSort (Just (Debt.qualifiedStoreIncarnation joiningHerald deltaA typedSort storeA)))
      (structuralDebtEvidence Set.empty Set.empty Set.empty),
    make coverageCause anotherSort Nothing,
    make anotherCause typedSort Nothing
  ]
  where
    make cause affected destination = structuralConsequenceDebt (structuralDebtKey cause TopologyAlignmentDebt affected destination) (structuralDebtEvidence Set.empty Set.empty Set.empty)
    anotherSort = sortOccurrence sortId (checked "another definition occurrence" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xba)))
    anotherCause = checked "unrelated End cause" (processEndCause endedProcess (controlIndex 7))

coverageDonorDebt :: Debt.StructuralConsequenceDebt
coverageDonorDebt = structuralConsequenceDebt (structuralDebtKey coverageCause TopologyAlignmentDebt typedSort Nothing) (structuralDebtEvidence Set.empty Set.empty Set.empty)

retainCoverageDebt :: Debt.StructuralConsequenceDebt -> Alignment.State -> Alignment.State
retainCoverageDebt debt state = fst (commitStructuralDebtRetention (checked "retain receiver debt" (prepareStructuralDebtRetention (Debt.structuralDebtKeyCause (Debt.structuralConsequenceDebtKey debt)) (normalizeStructuralDebts [debt]) state)))

applyCoverageEvent :: Alignment.State -> HistoricalCoverageEvent -> Alignment.State
applyCoverageEvent state event = case event of
  ImportCoverage -> checked "import historical coverage event" (Alignment.retainHistoricalAlignmentPlanCoverage joiningHerald coverageKey coveragePlan state)
  RetainCoverageDebt debt -> retainCoverageDebt debt state
  InvalidateCoverage -> fst (Alignment.commitAlignmentPlanInvalidation (checked "invalidate historical coverage event" (Alignment.prepareAlignmentPlanInvalidation cause state)))
    where
      generation = case [value | value <- routingGenerations coveragePlan, any ((== deltaA) . DomainAlignment.alignmentMemberDelta) (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut value))] of
        [value] -> value
        _ -> error "coverage plan must have exactly one class containing delta A"
      cause = Alignment.AlignmentPlanDestinationStoreLost (alignmentGenerationId generation) (Protocol.destinationStore deltaA storeA)

liveDebtKeys :: Alignment.State -> Set.Set Debt.StructuralDebtKey
liveDebtKeys = Set.fromList . map Debt.structuralConsequenceDebtKey . Alignment.liveAlignmentStructuralDebtEntries

uncoveredDebtKeys :: Alignment.State -> Set.Set Debt.StructuralDebtKey
uncoveredDebtKeys = Set.filter (\key -> Debt.structuralDebtKeyCause key /= coverageCause || Debt.structuralDebtKeySort key /= typedSort) . liveDebtKeys

passiveCoverageNoWork :: Alignment.State -> Bool
passiveCoverageNoWork state =
  null (Alignment.alignmentPromotionEntries state)
    && null (Alignment.historicalAlignmentFrontier state)
    && null (Alignment.alignmentObligationEntries state)
    && null (Alignment.bootstrapImportEntries state)
    && null (Alignment.alignmentAttemptEntries state)
    && null (Alignment.bootstrapImportAttemptEntries state)
    && null (Alignment.alignmentCutAcceptanceEntries state)
    && null (Alignment.alignmentLocalMemberReadinessEntries state)
    && null (Alignment.pendingAlignmentMemberProgress joiningHerald state)
    && null (Alignment.pendingAlignmentRouteCutoverEntries joiningHerald state)
    && Alignment.alignmentTransferState state == Alignment.alignmentTransferState emptyState

caseHistoricalRecipientScope :: Assertion
caseHistoricalRecipientScope = do
  plan <- readyPlan firstContext [] [] Set.empty
  let identifiers = map alignmentGenerationId (alignmentGenerationPlanGenerations plan)
      first = Set.fromList (take 1 identifiers)
      second = Set.fromList (drop 1 identifiers)
      retain = Alignment.retainHistoricalAlignmentRecipient joiningHerald
      initial = retain 1 first emptyState
      expanded = retain 1 second initial
      replacement = retain 2 second expanded
      cleared = retain 3 Set.empty replacement
      isolated = Alignment.retainHistoricalAlignmentRecipient heraldB 1 first replacement
      recipientIds = Alignment.historicalAlignmentRecipientGenerations joiningHerald
      noAuthority state =
        null (Alignment.alignmentGenerationEntries state)
          && null (Alignment.alignmentPromotionEntries state)
          && null (Alignment.historicalAlignmentFrontier state)
          && null (Alignment.alignmentObligationEntries state)
          && null (Alignment.bootstrapImportEntries state)
          && null (Alignment.alignmentAttemptEntries state)
          && null (Alignment.bootstrapImportAttemptEntries state)
          && null (Alignment.alignmentCutAcceptanceEntries state)
          && null (Alignment.alignmentMemberReadinessEntries state)
          && null (Alignment.alignmentHistoricalCertificateEntries state)
          && null (Alignment.pendingGenerationEvidenceEntries state)
          && null (Alignment.pendingAlignmentMemberProgress joiningHerald state)
          && null (Alignment.pendingAlignmentRouteCutoverEntries joiningHerald state)
  assertEqual "same-attempt sources accumulate their exact catalogues" (first <> second) (recipientIds expanded)
  assertEqual "same-attempt replay is idempotent" expanded (retain 1 first expanded)
  assertEqual "a recapture replaces the abandoned catalogue" second (recipientIds replacement)
  assertEqual "a stale capture cannot restore abandoned IDs" replacement (retain 1 first replacement)
  assertEqual "empty recapture still retains its attempt witness" cleared (retain 2 first cleared)
  assertEqual "empty replacement removes all prior delivery interest" Set.empty (recipientIds cleared)
  assertEqual "another recipient has an independent scope" second (recipientIds isolated)
  assertEqual "another recipient retains its own catalogue" first (Alignment.historicalAlignmentRecipientGenerations heraldB isolated)
  assertBool "interest may precede plan retention without creating owner authority" (all noAuthority [initial, expanded, replacement, cleared, isolated])
  assertEqual "unknown generation interest preserves owner invariants" (Right ()) (Alignment.validateAlignmentState isolated)
  assertEqual "control projection carries only each exact delivery scope" (Map.fromList [(joiningHerald, second), (heraldB, first)]) (Alignment.alignmentControlEvidence Nothing isolated).historicalRecipientGenerations
  assertBool "membership query is scoped to the named recipient" (all (\identifier -> Alignment.historicalAlignmentRecipient heraldB identifier isolated && not (Alignment.historicalAlignmentRecipient joiningHerald identifier isolated)) (Set.toList first))
  assertEqual "changed control generations include added and removed interest" (first <> second) (Alignment.alignmentChangedControlGenerations expanded replacement)
  assertEqual "plan delta includes scope-only changes" (first <> second) (Alignment.alignmentControlPlanGenerations Set.empty expanded replacement)
  assertEqual "scope removal participates in differential control selection" second (Alignment.alignmentChangedControlGenerations replacement cleared)
  assertEqual "scope changes are symmetric across admitted snapshots" (Alignment.alignmentControlPlanGenerations Set.empty replacement cleared) (Alignment.alignmentControlPlanGenerations Set.empty cleared replacement)

propHistoricalRecipientReplay :: Property
propHistoricalRecipientReplay =
  let plan = case preparePlan firstContext [] [] Set.empty of
        Right (AlignmentGenerationReady value) -> value
        other -> error ("historical recipient plan preparation: " <> show other)
      identifiers = map alignmentGenerationId (alignmentGenerationPlanGenerations plan)
      first = Set.fromList (take 1 identifiers)
      second = Set.fromList (drop 1 identifiers)
      captures = [(1, first), (2, first <> second), (3, second), (3, Set.empty), (3, second), (1, first)]
      retain state (attempt, scope) = Alignment.retainHistoricalAlignmentRecipient joiningHerald attempt scope state
      expected = Alignment.retainHistoricalAlignmentRecipient joiningHerald 3 second emptyState
   in forAll (shuffle captures) $ \arrivalOrder ->
        let retained = foldl retain emptyState arrivalOrder
         in conjoin
              [ retained === expected,
                Alignment.validateAlignmentState retained === Right ()
              ]

caseHistoricalCatalogue :: Assertion
caseHistoricalCatalogue = do
  let plan = coveragePlan
      conflicting = routingPlan topology reverseContext Nothing Set.empty
  let imported = checked "passive plan import" (Alignment.retainHistoricalAlignmentRoutingPlan joiningHerald plan emptyState)
      noWork state =
        null (Alignment.alignmentPromotionEntries state)
          && null (Alignment.alignmentObligationEntries state)
          && null (Alignment.bootstrapImportEntries state)
          && null (Alignment.alignmentAttemptEntries state)
          && null (Alignment.bootstrapImportAttemptEntries state)
          && null (Alignment.alignmentCutAcceptanceEntries state)
          && null (Alignment.alignmentLocalMemberReadinessEntries state)
          && null (Alignment.pendingAlignmentMemberProgress joiningHerald state)
          && null (Alignment.pendingAlignmentRouteCutoverEntries joiningHerald state)
  assertEqual "complete historical classes retained" 2 (length (alignmentGenerationEntries imported))
  assertEqual "catalogue import does not choose latest" [] (Alignment.historicalAlignmentFrontier imported)
  assertBool "no votes, owner receipts, subscriptions or local candidates" (noWork imported)
  assertEqual "passive catalogue satisfies owner invariants" (Right ()) (Alignment.validateAlignmentState imported)
  assertEqual "exact catalogue replay is idempotent" (Right imported) (Alignment.retainHistoricalAlignmentRoutingPlan joiningHerald plan imported)
  assertEqual "actual old member must use ordinary promotion" (Left (Alignment.AlignmentHistoricalPlanLocalMember heraldA)) (Alignment.retainHistoricalAlignmentRoutingPlan heraldA plan emptyState)
  assertEqual "actual member can corroborate exactly retained facts" (Right imported) (Alignment.retainHistoricalAlignmentRoutingPlan heraldA plan imported)
  assertBool "same class IDs cannot overwrite their historical relations" (isRejected (Alignment.retainHistoricalAlignmentRoutingPlan joiningHerald conflicting imported))
  let prior = routingGenerations plan
      priorIds = Set.fromList (map alignmentGenerationId prior)
      successorTopology =
        checked
          "successor topology"
          (topologyCut (sameGenerationPredecessor topologyId) (topologyFrontier emptyVector (controlIndex 1)) (deriveTopologyOccurrenceDigest "historical successor"))
      successor = routingPlan successorTopology mergedContext (Just plan) priorIds
  assertBool "missing predecessor catalogue is rejected" (isRejected (Alignment.retainHistoricalAlignmentRoutingPlan joiningHerald successor emptyState))
  case Alignment.retainHistoricalAlignmentRoutingPlan joiningHerald successor imported of
    Left (Alignment.AlignmentHistoricalCertificateMissing identifier) ->
      assertBool "uncertified named predecessor blocks import" (Set.member identifier priorIds)
    other -> assertFailure ("expected historical certificate hold, got " <> show other)
  let certified = foldl certify imported prior
      complete = checked "import certified successor" (Alignment.retainHistoricalAlignmentRoutingPlan joiningHerald successor certified)
  assertEqual "certified predecessor closure admits its successor" 3 (length (alignmentGenerationEntries complete))
  assertEqual "successor catalogue preserves owner invariants" (Right ()) (Alignment.validateAlignmentState complete)
  assertBool "remote certificates do not confer local owner work" (noWork complete)
  let withFrontier = checked "adopt successor frontier" (Alignment.adoptHistoricalAlignmentPlanFrontier joiningHerald [(typedSort, Plan.alignmentPlanId successor)] complete)
      history = History.captureAlignmentHistory withFrontier
      bytes = History.encodeAlignmentHistory history
      capturedIds = Set.fromList [alignmentCutAnnounceGeneration announce | siblingSet <- History.alignmentHistoryPlans history, announce <- Protocol.alignmentPlanAnnounceCreatedCuts siblingSet]
  assertEqual
    "capture retains complete predecessor and sibling closure"
    (Set.fromList (map fst (alignmentGenerationEntries complete)))
    capturedIds
  assertEqual
    "capture retains exact prerequisite readiness"
    (map snd (Alignment.alignmentMemberReadinessEntries complete))
    (History.alignmentHistoryReadiness history)
  assertEqual
    "capture retains exact prerequisite certificates"
    (map snd (Alignment.alignmentHistoricalCertificateEntries complete))
    (History.alignmentHistoryCertificates history)
  assertEqual "certified ancestry catalogue has a canonical roundtrip" (Right history) (History.decodeAlignmentHistory bytes)
  assertBool "alignment history rejects trailing bytes" (isRejected (History.decodeAlignmentHistory (bytes <> "x")))
  let successorAcceptances = [checked "successor acceptance" (Protocol.alignmentPlanAccepted (Plan.alignmentPlanId successor) owner EmptyHeraldPublicationPrefix) | owner <- [heraldA, heraldB]]
      withSuccessorProof = checked "inherit current-frontier acceptances" (History.withAlignmentHistoryPlanAcceptances successorAcceptances history)
  assertBool "only current plan requires admission proof" (JoinHistory.alignmentFrontierAdmitted [heraldA, heraldB] withSuccessorProof)
  assertEqual "historical predecessor acceptances are not fabricated" (Set.singleton (Plan.alignmentPlanId successor)) (Set.fromList (map Protocol.alignmentPlanAcceptedId (History.alignmentHistoryPlanAcceptances withSuccessorProof)))
  assertEqual "successor admission proof survives ancestry roundtrip" (Right withSuccessorProof) (History.decodeAlignmentHistory (History.encodeAlignmentHistory withSuccessorProof))
  where
    certify state generation =
      let identifier = alignmentGenerationId generation
          readies =
            fmap
              (\member -> classMemberReady firstAlignmentEvidenceSequence identifier (DomainAlignment.alignmentMemberStoreIncarnation member) (deriveMemberReadyEvidenceDigest "historical predecessor prefix") initialStoreRevision)
              (DomainAlignment.alignmentCutExactMembers (alignmentGenerationCut generation))
          sourceReady :| _ = readies
          readyList = foldr (:) [] readies
          withReady =
            foldl
              (\owner ready -> fst (commitClassMemberReady (checked "retain historical readiness" (prepareClassMemberReady ready owner))))
              state
              readyList
          certificate =
            historicalCertificate
              identifier
              (classMemberReadyStoreIncarnation sourceReady)
              (classMemberReadySetDigest readyList)
              (deriveBootstrapEvidenceDigest (memberReadyEvidenceDigestBytes (classMemberReadyPredecessorAndBasePrefixDigest sourceReady)))
              initialStoreRevision
       in fst (commitHistoricalCertificate (checked "retain historical certificate" (prepareHistoricalCertificate certificate withReady)))

caseHistoricalFrontier :: Assertion
caseHistoricalFrontier = do
  let plan = coveragePlan
      imported = checked "passive plan import" (Alignment.retainHistoricalAlignmentRoutingPlan joiningHerald plan emptyState)
      frontier = [(typedSort, Plan.alignmentPlanId plan)]
      emptyPlan = emptyRoutingPlan historySuccessorTopology (Just plan)
      withEmpty = checked "passive empty plan" (Alignment.retainHistoricalAlignmentRoutingPlan joiningHerald emptyPlan imported)
      emptyFrontier = [(typedSort, Plan.alignmentPlanId emptyPlan)]
      adopted = checked "adopt original frontier" (Alignment.adoptHistoricalAlignmentPlanFrontier joiningHerald frontier withEmpty)
      empty = checked "adopt cleared frontier" (Alignment.adoptHistoricalAlignmentPlanFrontier joiningHerald emptyFrontier withEmpty)
  assertEqual "canonical plan frontier adopted" frontier (Alignment.historicalAlignmentPlanFrontier adopted)
  assertEqual "empty plan remains an explicit sort fact" emptyFrontier (Alignment.historicalAlignmentPlanFrontier empty)
  assertEqual "frontier preserves invariants" (Right ()) (Alignment.validateAlignmentState adopted)
  assertEqual "empty frontier preserves history" (alignmentGenerationEntries imported) (alignmentGenerationEntries empty)
  assertEqual "exact frontier replay is inert" (Right adopted) (Alignment.adoptHistoricalAlignmentPlanFrontier joiningHerald frontier adopted)
  assertEqual "a later different frontier cannot rewind sealed adoption" (Left (Alignment.AlignmentHistoricalFrontierConflict typedSort)) (Alignment.adoptHistoricalAlignmentPlanFrontier joiningHerald emptyFrontier adopted)
  assertBool "missing complete plan rejects" (isRejected (Alignment.adoptHistoricalAlignmentPlanFrontier joiningHerald emptyFrontier imported))
  assertBool "duplicate sort entries reject" (isRejected (Alignment.adoptHistoricalAlignmentPlanFrontier joiningHerald (frontier <> frontier) imported))
  assertBool "frontier cannot confer old local authority" (isRejected (Alignment.adoptHistoricalAlignmentPlanFrontier heraldA frontier imported))
  let nonemptyHistory = History.captureAlignmentHistory adopted
      emptyHistory = History.captureAlignmentHistory empty
  assertEqual "captured nonempty frontier is exact" frontier (History.alignmentHistoryFrontier nonemptyHistory)
  assertEqual "nonempty catalogue roundtrips" (Right nonemptyHistory) (History.decodeAlignmentHistory (History.encodeAlignmentHistory nonemptyHistory))
  assertEqual "empty current plan retains explicit predecessor-plan closure" 2 (length (History.alignmentHistoryPlans emptyHistory))
  assertEqual "empty plan frontier is retained" emptyFrontier (History.alignmentHistoryFrontier emptyHistory)
  assertEqual "empty catalogue roundtrips" (Right emptyHistory) (History.decodeAlignmentHistory (History.encodeAlignmentHistory emptyHistory))

propHistoricalCatalogueReplay :: Property
propHistoricalCatalogueReplay = forAll (chooseInt (0, 8)) $ \copies ->
  let plan = coveragePlan
      install = checked "passive plan replay" . Alignment.retainHistoricalAlignmentRoutingPlan joiningHerald plan
      imported = install emptyState
      frontier = [(typedSort, Plan.alignmentPlanId plan)]
      adopted = checked "canonical frontier" (Alignment.adoptHistoricalAlignmentPlanFrontier joiningHerald frontier imported)
      replayed = iterate install adopted !! copies
   in conjoin [replayed === adopted, Alignment.validateAlignmentState replayed === Right ()]

joiningHerald :: HeraldEpoch
joiningHerald = checked "joining Herald" (mkHeraldEpoch (fixtureIdentifierBytes 0x03))

isRejected :: Either problem value -> Bool
isRejected (Left _) = True
isRejected (Right _) = False

caseDeterministicAncestry :: Assertion
caseDeterministicAncestry = do
  firstPlan <- readyPlan firstContext [] [] Set.empty
  assertEqual "one generation per state-bearing SCC" 2 (length (alignmentGenerationPlanGenerations firstPlan))
  assertEqual "one immediate context relation" 1 (length (alignmentGenerationPlanRelations firstPlan))
  let prior = alignmentGenerationPlanGenerations firstPlan
      priorIds = Set.fromList (fmap alignmentGenerationId prior)
      priorRelations = alignmentGenerationPlanRelations firstPlan
  case preparePlan mergedContext prior priorRelations Set.empty of
    Right (AlignmentGenerationHeld missing) ->
      assertEqual "both intersecting predecessors gate the merge" priorIds missing
    other -> assertFailure ("expected missing predecessor certificates, got " <> show other)
  merged <- readyPlan mergedContext prior priorRelations priorIds
  case alignmentGenerationPlanGenerations merged of
    [generation] -> do
      assertEqual
        "predecessor set is canonical and complete"
        (Set.toAscList priorIds)
        (alignmentCutPredecessorGenerationIds (alignmentGenerationCut generation))
      assertEqual
        "no member represented by a predecessor receives a fresh base"
        []
        (alignmentCutFreshMemberBaseEvidence (alignmentGenerationCut generation))
    other -> assertFailure ("expected one merged generation, got " <> show (length other))

caseCheckedCutAnnouncement :: Assertion
caseCheckedCutAnnouncement = do
  plan <- readyPlan firstContext [] [] Set.empty
  mapM_ assertChecked (alignmentGenerationPlanGenerations plan)
  where
    assertChecked generation =
      assertEqual
        "the retained announcement carries an admitted generation/cut pair"
        (Right (alignmentGenerationCutAnnounce generation))
        ( checkAlignmentCutAnnounce
            (alignmentGenerationId generation)
            (alignmentGenerationCut generation)
        )

propCachedTopologyCut :: Property
propCachedTopologyCut =
  forAll (chooseInt (0, 1000)) $ \prefix ->
    forAll (elements [firstContext, reverseContext, mergedContext]) $ \context ->
      forAll (shuffle members) $ \presentedMembers ->
        let selectedTopology =
              checked
                "generated topology cut"
                ( topologyCut
                    (sameGenerationPredecessor predecessorCut)
                    (topologyFrontier emptyVector (controlIndex (fromIntegral prefix)))
                    (deriveTopologyOccurrenceDigest (ByteString.pack (show prefix)))
                )
         in case prepareAlignmentGenerationPlan typedSort selectedTopology placement context presentedMembers [] [] Set.empty of
              Left problem -> counterexample (show problem) False
              Right (AlignmentGenerationHeld missing) -> counterexample (show missing) False
              Right (AlignmentGenerationReady plan) ->
                conjoin
                  [ conjoin
                      [ alignmentGenerationTopologyCutId generation
                          === deriveTopologyCutId (alignmentCutTopologyCut (alignmentGenerationCut generation)),
                        checkAlignmentCutAnnounce (alignmentGenerationId generation) (alignmentGenerationCut generation)
                          === Right (alignmentGenerationCutAnnounce generation)
                      ]
                  | generation <- alignmentGenerationPlanGenerations plan
                  ]
  where
    members =
      [ generationMemberInput deltaA storeA heraldA initialStoreRevision,
        generationMemberInput deltaB storeB heraldB initialStoreRevision
      ]

propEvidenceArrivalPermutation :: Property
propEvidenceArrivalPermutation =
  forAll (shuffle prior) $ \presentedPrior ->
    forAll (shuffle priorRelations) $ \presentedRelations ->
      forAll (shuffle duplicateArrivals) $ \arrivals ->
        let prefixes = scanl (flip Set.insert) Set.empty arrivals
            observations =
              fmap
                (preparePlan mergedContext presentedPrior presentedRelations)
                prefixes
            expectedFor certificates
              | certificates == priorIds = Right (AlignmentGenerationReady expected)
              | otherwise =
                  Right
                    ( AlignmentGenerationHeld
                        (priorIds `Set.difference` certificates)
                    )
         in counterexample ("certificate arrival order: " <> show arrivals)
              . conjoin
              $ zipWith (===) observations (fmap expectedFor prefixes)
  where
    initial =
      case preparePlan firstContext [] [] Set.empty of
        Right (AlignmentGenerationReady plan) -> plan
        other -> error ("initial generated plan was not ready: " <> show other)
    prior = alignmentGenerationPlanGenerations initial
    priorRelations = alignmentGenerationPlanRelations initial
    priorIds = Set.fromList (fmap alignmentGenerationId prior)
    duplicateArrivals = concatMap (\identifier -> [identifier, identifier]) (Set.toAscList priorIds)
    expected =
      case preparePlan mergedContext prior priorRelations priorIds of
        Right (AlignmentGenerationReady plan) -> plan
        other -> error ("completed generated plan was not ready: " <> show other)

casePromotion :: Assertion
casePromotion = do
  plan <- readyPlan firstContext [] [] Set.empty
  relationMutant <- readyPlan reverseContext [] [] Set.empty
  let cause = structuralOccurrenceCause occurrence
      debts =
        normalizeStructuralDebts
          [ structuralConsequenceDebt
              (structuralDebtKey cause TopologyAlignmentDebt typedSort Nothing)
              (structuralDebtEvidence Set.empty Set.empty Set.empty)
          ]
  let retained =
        checked
          "retain structural debt"
          (prepareStructuralDebtRetention cause debts emptyState)
      (withDebt, _) = commitStructuralDebtRetention retained
      prepared =
        checked
          "prepare promotion"
          (prepareAlignmentPromotion heraldB cause typedSort topologyId placement plan withDebt)
  assertEqual "first promotion commits" AlignmentPromotionCommitted (preparedAlignmentPromotionDisposition prepared)
  let (promoted, _) = commitAlignmentPromotion prepared
  let replay =
        checked
          "prepare exact promotion replay"
          (prepareAlignmentPromotion heraldB cause typedSort topologyId placement plan promoted)
  assertEqual "exact promotion replay is unchanged" AlignmentPromotionUnchanged (preparedAlignmentPromotionDisposition replay)
  let (replayed, _) = commitAlignmentPromotion replay
  assertEqual "one promotion receipt retained" 1 (length (alignmentPromotionEntries replayed))
  assertEqual "one destination-owned unsourced obligation retained" 1 (length (alignmentObligationEntries replayed))
  assertEqual
    "each generation is retained exactly once"
    (length (alignmentGenerationPlanGenerations plan))
    (length (alignmentGenerationEntries replayed))
  assertEqual
    "the mutant deliberately retains the exact generation identities"
    (fmap alignmentGenerationId (alignmentGenerationPlanGenerations plan))
    (fmap alignmentGenerationId (alignmentGenerationPlanGenerations relationMutant))
  assertBool
    "the mutant changes only the normalized relation set"
    ( alignmentGenerationPlanRelations plan
        /= alignmentGenerationPlanRelations relationMutant
    )
  case prepareAlignmentPromotion
    heraldB
    cause
    typedSort
    topologyId
    placement
    relationMutant
    promoted of
    Left problem ->
      assertEqual
        "same-key replay with changed relations is a conflict"
        ( AlignmentPromotionConflict
            (alignmentPromotionKey cause typedSort topologyId placement)
        )
        problem
    Right _ ->
      assertFailure "same-key replay with changed relations unexpectedly prepared"

caseControlCausePromotion :: Assertion
caseControlCausePromotion = do
  plan <- readyPlan firstContext [] [] Set.empty
  let cause =
        checked
          "process End cause"
          (processEndCause endedProcess (controlIndex 7))
      debts =
        normalizeStructuralDebts
          [ structuralConsequenceDebt
              (structuralDebtKey cause TopologyAlignmentDebt typedSort Nothing)
              (structuralDebtEvidence Set.empty Set.empty Set.empty)
          ]
      retained =
        checked
          "retain control-caused debt"
          (prepareStructuralDebtRetention cause debts emptyState)
      (withDebt, _) = commitStructuralDebtRetention retained
      prepared =
        checked
          "promote control-caused debt"
          (prepareAlignmentPromotion heraldB cause typedSort topologyId placement plan withDebt)
      (promoted, _) = commitAlignmentPromotion prepared
  assertEqual
    "the promotion key retains the exact control cause"
    [cause]
    (fmap (alignmentPromotionKeyCause . fst) (alignmentPromotionEntries promoted))
  assertEqual
    "every derived obligation retains the exact control cause"
    [cause]
    (fmap (alignmentObligationCause . snd) (alignmentObligationEntries promoted))

caseAttemptGate :: Assertion
caseAttemptGate = do
  plan <- readyPlan firstContext [] [] Set.empty
  let cause = structuralOccurrenceCause occurrence
      debts =
        normalizeStructuralDebts
          [ structuralConsequenceDebt
              (structuralDebtKey cause TopologyAlignmentDebt typedSort Nothing)
              (structuralDebtEvidence Set.empty Set.empty Set.empty)
          ]
      retained =
        checked
          "retain attempt debt"
          (prepareStructuralDebtRetention cause debts emptyState)
      (withDebt, _) = commitStructuralDebtRetention retained
      promotedPreparation =
        checked
          "promote attempt debt"
          (prepareAlignmentPromotion heraldB cause typedSort topologyId placement plan withDebt)
      (promoted, _) = commitAlignmentPromotion promotedPreparation
      obligationId = case alignmentObligationEntries promoted of
        [(identifier, _)] -> identifier
        other -> error ("expected one obligation, got " <> show (length other))
      relation = case alignmentGenerationPlanRelations plan of
        [onlyRelation] -> onlyRelation
        other -> error ("expected one relation, got " <> show (length other))
      sourceGeneration = alignmentGenerationRelationSource relation
      destinationGeneration = alignmentGenerationRelationDestination relation
      destinationAcceptance =
        alignmentCutAccepted
          destinationGeneration
          heraldB
          topologyId
          placement
          EmptyHeraldPublicationPrefix
      destinationAcceptedOnlyPreparation =
        checked
          "retain destination-only cut acceptance"
          (prepareAlignmentCutAcceptance destinationAcceptance promoted)
      (destinationAcceptedOnly, _) =
        commitAlignmentCutAcceptance destinationAcceptedOnlyPreparation
  assertEqual
    "destination acceptance alone cannot replace source evidence"
    (Left (AlignmentAttemptSourceEvidenceUnavailable obligationId))
    (fmap (const ()) (prepareAlignmentAttempt obligationId destinationAcceptedOnly))

  let acceptance =
        alignmentCutAccepted
          sourceGeneration
          heraldA
          topologyId
          placement
          EmptyHeraldPublicationPrefix
      acceptedPreparation =
        checked
          "retain source cut acceptance"
          (prepareAlignmentCutAcceptance acceptance promoted)
      (withAcceptance, _) =
        commitAlignmentCutAcceptance acceptedPreparation
      ready =
        classMemberReady
          firstAlignmentEvidenceSequence
          sourceGeneration
          storeA
          (deriveMemberReadyEvidenceDigest "predecessor-and-base")
          initialStoreRevision
      readyPreparation =
        checked
          "retain source member readiness"
          (prepareClassMemberReady ready withAcceptance)
      (withReady, _) = commitClassMemberReady readyPreparation
      certificate =
        historicalCertificate
          sourceGeneration
          storeA
          (classMemberReadySetDigest [ready])
          ( deriveBootstrapEvidenceDigest
              ( memberReadyEvidenceDigestBytes
                  (classMemberReadyPredecessorAndBasePrefixDigest ready)
              )
          )
          initialStoreRevision
      certificatePreparation =
        checked
          "retain exact historical certificate"
          (prepareHistoricalCertificate certificate withReady)
      (withCertificate, _) =
        commitHistoricalCertificate certificatePreparation
  assertEqual
    "source evidence cannot bypass destination-generation acceptance"
    (Left (AlignmentAttemptDestinationAcceptanceUnavailable obligationId))
    (fmap (const ()) (prepareAlignmentAttempt obligationId withCertificate))
  let destinationAcceptedPreparation =
        checked
          "retain destination cut acceptance"
          (prepareAlignmentCutAcceptance destinationAcceptance withCertificate)
      (withDestinationAcceptance, _) =
        commitAlignmentCutAcceptance destinationAcceptedPreparation
      sourceDestinationAccepted = fst (commitAlignmentCutAcceptance (checked "source accepts destination routing cut" (prepareAlignmentCutAcceptance (alignmentCutAccepted destinationGeneration heraldA topologyId placement EmptyHeraldPublicationPrefix) withDestinationAcceptance)))
      attemptPreparation =
        checked
          "prepare supplier-qualified attempt"
          (prepareAlignmentAttempt obligationId sourceDestinationAccepted)
      (withAttempt, _) = commitAlignmentAttempt attemptPreparation
      replay =
        checked
          "prepare exact attempt retry"
          (prepareAlignmentAttempt obligationId withAttempt)
      (afterReplay, _) = commitAlignmentAttempt replay
  assertEqual "attempt retry is owner-idempotent" withAttempt afterReplay

readyPlan ::
  ContextGraph ->
  [AlignmentGeneration] ->
  [AlignmentGenerationRelation] ->
  Set.Set ContextClassGenerationId ->
  IO AlignmentGenerationPlan
readyPlan context prior relations certificates =
  case preparePlan context prior relations certificates of
    Right (AlignmentGenerationReady plan) -> pure plan
    other -> assertFailure ("expected ready generation plan, got " <> show other)

preparePlan ::
  ContextGraph ->
  [AlignmentGeneration] ->
  [AlignmentGenerationRelation] ->
  Set.Set ContextClassGenerationId ->
  Either AlignmentGenerationProblem AlignmentGenerationPreparation
preparePlan context prior relations certificates =
  prepareAlignmentGenerationPlan
    typedSort
    topology
    placement
    context
    [ generationMemberInput deltaA storeA heraldA initialStoreRevision,
      generationMemberInput deltaB storeB heraldB initialStoreRevision
    ]
    prior
    relations
    certificates

firstContext :: ContextGraph
firstContext =
  checked
    "first context"
    ( deriveContextGraph
        vertices
        [edgePayload (DeltaVertex deltaA) (DeltaVertex deltaB) Preserve]
        [deltaA, deltaB]
    )

reverseContext :: ContextGraph
reverseContext =
  checked
    "reverse relation context"
    ( deriveContextGraph
        vertices
        [edgePayload (DeltaVertex deltaB) (DeltaVertex deltaA) Preserve]
        [deltaA, deltaB]
    )

mergedContext :: ContextGraph
mergedContext =
  checked
    "merged context"
    ( deriveContextGraph
        vertices
        [ edgePayload (DeltaVertex deltaA) (DeltaVertex deltaB) Preserve,
          edgePayload (DeltaVertex deltaB) (DeltaVertex deltaA) Preserve
        ]
        [deltaA, deltaB]
    )

vertices :: [Eclips.Domain.Graph.VertexId]
vertices = [DeltaVertex deltaA, DeltaVertex deltaB]

typedSort :: SortOccurrence
typedSort = sortOccurrence sortId occurrenceId

placement :: PhysicalPlacementRevisionVector
placement =
  checked
    "placement vector"
    ( physicalPlacementRevisionVector
        membership
        [(heraldB, firstPlacementRevision), (heraldA, firstPlacementRevision)]
    )

topology :: TopologyCut
topology =
  checked
    "topology cut"
    ( topologyCut
        (sameGenerationPredecessor predecessorCut)
        (topologyFrontier emptyVector (controlIndex 0))
        (deriveTopologyOccurrenceDigest "alignment-generation-topology")
    )

topologyId :: TopologyCutId
topologyId = deriveTopologyCutId topology

emptyVector :: StructuralVersionVector
emptyVector = emptyStructuralVersionVector membership

membership :: HeraldMembershipGeneration
membership =
  checked
    "membership generation"
    (genesisHeraldMembershipGeneration system (heraldA :| [heraldB]))

system :: SystemId
system = checked "system" (mkSystemId (fixtureIdentifierBytes 0xaa))

occurrence :: StructuralOccurrenceId
occurrence = structuralOccurrenceId heraldA (checked "sequence" (mkStructuralSequence 1))

endedProcess :: ProcessEpochId
endedProcess =
  checked "ended process" (mkProcessEpochId (fixtureIdentifierBytes 0xaa))

sortId :: SortId
sortId = checked "sort" (mkSortId (fixtureIdentifierBytes 0xa1))

occurrenceId :: SortDefinitionOccurrenceId
occurrenceId = checked "sort occurrence" (mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 0xa2))

deltaA, deltaB :: DeltaId
deltaA = checked "delta A" (mkDeltaId (fixtureIdentifierBytes 0xa3))
deltaB = checked "delta B" (mkDeltaId (fixtureIdentifierBytes 0xa4))

storeA, storeB :: StoreIncarnationId
storeA = checked "store A" (mkStoreIncarnationId (fixtureIdentifierBytes 0xa5))
storeB = checked "store B" (mkStoreIncarnationId (fixtureIdentifierBytes 0xa6))

heraldA, heraldB :: HeraldEpoch
heraldA = checked "herald A" (mkHeraldEpoch (fixtureIdentifierBytes 0xa7))
heraldB = checked "herald B" (mkHeraldEpoch (fixtureIdentifierBytes 0xa8))

predecessorCut :: TopologyCutId
predecessorCut = checked "predecessor cut" (mkTopologyCutId (fixtureIdentifierBytes 0xa9))

checked :: (Show problem) => String -> Either problem value -> value
checked _ (Right value) = value
checked label (Left problem) = error (label <> ": " <> show problem)
