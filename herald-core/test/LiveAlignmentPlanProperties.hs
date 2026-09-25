{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module LiveAlignmentPlanProperties (tests) where

import Data.ByteString.Char8 qualified as Bytes
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment qualified as Domain
import Eclips.Domain.Context qualified as Context
import Eclips.Domain.Graph qualified as Graph
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Route qualified as Route
import Eclips.Domain.Structural qualified as Structural
import Eclips.Domain.StructuralConsequence qualified as Consequence
import Eclips.Domain.Topology qualified as Topology
import Eclips.Domain.Value qualified as Value
import Eclips.Herald.Alignment.Generation qualified as Generation
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Structural.Debt qualified as Debt
import GenesisFixtures (fixtureIdentifierBytes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "live alignment plan carry"
    [ QC.testProperty "unchanged plans preserve exact owner and stream state across arbitrary Store revisions" propCarryOwners,
      QC.testProperty "generation evidence does not revisit the plans that carried it" propExactPlanEvidence,
      testCase "new incoming relation needs source acceptance of its creation plan" caseNewDestinationAcceptance,
      testCase "mixed-age invalidation targets the current plan and preserves birth history" caseMixedAgeInvalidation,
      testCase "superseded Store loss preserves unrelated current carried bindings" caseSupersededIncarnationLoss,
      testCase "historical replay and added cause coverage cannot rewind current work" caseHistoricalReplay,
      testCase "removed then re-added relations allocate distinct owners and subscriptions" caseRemovedRelation,
      testCase "same-plan cause coverage cannot duplicate bootstrap or relation work" caseAdditionalCause,
      testCase "a carried input keeps its sealed child prefix while continuing to advance" caseContinuingInputClosure
    ]

propExactPlanEvidence :: QC.Property
propExactPlanEvidence = QC.forAll (QC.choose (1, 12 :: Word64)) $ \depth ->
  let first = planAt 0 baseEdges Nothing 0
      advance (previous, owner) index =
        let next = planAt index baseEdges (Just previous) (index + 7)
         in (next, accept heraldA next (accept heraldB next (promote (index + 1) next owner)))
      (current, retained) = foldl advance (first, readyOwner first) [1 .. depth]
      generation = generationIdFor deltaA current
      generationOnly = Alignment.alignmentControlEvidenceForPlans (Just (Set.singleton generation)) (Just Set.empty) retained
      planOnly = Alignment.alignmentControlEvidenceForPlans (Just Set.empty) (Just (Set.singleton (Plan.alignmentPlanId current))) retained
   in QC.conjoin
        [ generationOnly.currentPlanEntries QC.=== [],
          generationOnly.currentPlanAcceptances QC.=== [],
          generationOnly.explicitBirthPlans QC.=== Set.singleton (Plan.alignmentPlanId first),
          map fst generationOnly.generationEntries QC.=== [generation],
          map fst planOnly.currentPlanEntries QC.=== [Plan.alignmentPlanId current],
          map (fst . fst) planOnly.currentPlanAcceptances QC.=== replicate 2 (Plan.alignmentPlanId current),
          planOnly.generationEntries QC.=== []
        ]

propCarryOwners :: QC.Property
propCarryOwners = QC.forAll (QC.choose (8, 100000 :: Word64)) $ \currentRevision ->
  let first = planAt 0 baseEdges Nothing 0
      started = startAttempt (obligationFor deltaA first) (readyOwner first)
      next = planAt 1 baseEdges (Just first) currentRevision
      carried = promote 2 next started
      accepted = accept heraldB next carried
   in QC.conjoin
        [ QC.counterexample "every binding carries" (all ((== Plan.AlignmentCarried) . Plan.alignmentPlanBindingDisposition . snd) (Plan.alignmentPlanBindings next)),
          Alignment.alignmentGenerationEntries carried QC.=== Alignment.alignmentGenerationEntries started,
          Alignment.alignmentObligationEntries carried QC.=== Alignment.alignmentObligationEntries started,
          Alignment.alignmentAttemptEntries carried QC.=== Alignment.alignmentAttemptEntries started,
          Alignment.bootstrapImportEntries carried QC.=== Alignment.bootstrapImportEntries started,
          Alignment.alignmentHistoricalCertificateEntries carried QC.=== Alignment.alignmentHistoricalCertificateEntries started,
          Alignment.alignmentCutAcceptanceEntries accepted QC.=== Alignment.alignmentCutAcceptanceEntries started,
          Alignment.alignmentTransferState accepted QC.=== Alignment.alignmentTransferState started,
          Alignment.alignmentClosingObligationIds accepted QC.=== [],
          Alignment.currentAlignmentPlanForSort occurrence accepted QC.=== Just next,
          Alignment.validateAlignmentState accepted QC.=== Right ()
        ]

caseNewDestinationAcceptance :: Assertion
caseNewDestinationAcceptance = do
  let first = planAt 0 baseEdges Nothing 0
      second = planAt 1 changedEdges (Just first) 99
      withPlan = promote 2 second (readyOwner first)
      destinationAccepted = accept heraldB second withPlan
      obligation = findObligation deltaA second destinationAccepted
  assertEqual "source A retains its exact birth generation" (generationFor deltaA first) (generationFor deltaA second)
  assertBool "new incoming C history creates a new destination B" (generationFor deltaB first /= generationFor deltaB second)
  assertEqual
    "old source certificate and birth acceptance cannot authorize the new destination plan"
    (Left (Alignment.AlignmentAttemptSourceEvidenceUnavailable obligation))
    (fmap (const ()) (Alignment.prepareAlignmentAttempt obligation destinationAccepted))
  let sourceAccepted = accept heraldA second destinationAccepted
      started = startAttempt obligation sourceAccepted
  assertEqual "new plan source acceptance admits the relation" 1 (length (Alignment.alignmentAttemptEntries started))
  assertEqual "mixed-age relation owner invariant" (Right ()) (Alignment.validateAlignmentState started)

caseMixedAgeInvalidation :: Assertion
caseMixedAgeInvalidation = do
  let first = planAt 0 baseEdges Nothing 0
      second = planAt 1 changedEdges (Just first) 99
      predecessor = readyOwner first
      current = accept heraldA second (accept heraldB second (promote 2 second predecessor))
      target = generationIdFor deltaA second
      invalidation = checked "invalidate carried member" (Alignment.prepareAlignmentPlanInvalidation (Alignment.AlignmentPlanDestinationStoreLost target (Protocol.destinationStore deltaA storeA)) current)
      invalidated = fst (Alignment.commitAlignmentPlanInvalidation invalidation)
  assertBool "current plan containing the carried member is invalidated" (Alignment.alignmentPlanInvalidated (Plan.alignmentPlanId second) invalidated)
  assertBool "immutable creation plan is not invalidated by current-plan loss" (not (Alignment.alignmentPlanInvalidated (Plan.alignmentPlanId first) invalidated))
  assertEqual "current execution owners are removed" [] [entry | entry@(_, obligation) <- Alignment.activeAlignmentObligationEntries invalidated, Protocol.alignmentObligationDestinationGeneration obligation `elem` map (Generation.alignmentGenerationId . Plan.alignmentPlanBindingGeneration . snd) (Plan.alignmentPlanBindings second)]
  assertEqual "old birth generations remain retained" (Alignment.alignmentGenerationEntries current) (Alignment.alignmentGenerationEntries invalidated)
  assertEqual "historical certificates remain retained" (Alignment.alignmentHistoricalCertificateEntries current) (Alignment.alignmentHistoricalCertificateEntries invalidated)
  assertEqual "birth cut acceptances remain retained" (Alignment.alignmentCutAcceptanceEntries current) (Alignment.alignmentCutAcceptanceEntries invalidated)
  assertEqual "invalidated mixed-age owner invariant" (Right ()) (Alignment.validateAlignmentState invalidated)

caseHistoricalReplay :: Assertion
caseHistoricalReplay = do
  let first = planAt 0 baseEdges Nothing 0
      second = planAt 1 changedEdges (Just first) 8
      current = promote 2 second (readyOwner first)
      exactReplay = promote 1 first current
      extraCoverage = promote 3 first exactReplay
  assertEqual "exact old-plan replay changes no owner state" current exactReplay
  assertEqual "new old-plan cause does not rewind current plan" (Just second) (Alignment.currentAlignmentPlanForSort occurrence extraCoverage)
  assertEqual "new old-plan cause does not rewind latest bindings" (Alignment.latestAlignmentGenerationEntries current) (Alignment.latestAlignmentGenerationEntries extraCoverage)
  assertEqual "new old-plan cause does not recreate owners" (Alignment.alignmentObligationEntries current) (Alignment.alignmentObligationEntries extraCoverage)
  assertEqual "new old-plan cause does not recreate bootstrap imports" (Alignment.bootstrapImportEntries current) (Alignment.bootstrapImportEntries extraCoverage)
  assertEqual "new old-plan cause leaves closing ownership unchanged" (Alignment.alignmentClosingObligationIds current) (Alignment.alignmentClosingObligationIds extraCoverage)
  assertEqual "each cause retains independent coverage" 3 (length (Alignment.alignmentPromotionEntries extraCoverage))
  assertEqual "historical cause replay is idempotent" extraCoverage (promote 3 first extraCoverage)
  assertEqual "historical coverage owner invariant" (Right ()) (Alignment.validateAlignmentState extraCoverage)

caseSupersededIncarnationLoss :: Assertion
caseSupersededIncarnationLoss = do
  let first = planAt 0 baseEdges Nothing 0
      replacementStore = checked "replacement B Store" (Identity.mkStoreIncarnationId (fixtureIdentifierBytes 121))
      replacementMembers currentRevision = [Generation.generationMemberInput delta store herald (Domain.storeRevisionFromWord64 currentRevision) | (delta, store, herald) <- [(deltaA, storeA, heraldA), (deltaB, replacementStore, heraldB), (deltaC, storeC, heraldA)]]
      second =
        planWithMembersAt
          1
          baseEdges
          (Just first)
          (replacementMembers 8)
      current = promote 2 second (readyOwner first)
      invalidation = checked "invalidate superseded B incarnation" (Alignment.prepareAlignmentPlanInvalidation (Alignment.AlignmentPlanDestinationStoreLost (generationIdFor deltaB first) (Protocol.destinationStore deltaB storeB)) current)
      invalidated = fst (Alignment.commitAlignmentPlanInvalidation invalidation)
      currentIds = map (Generation.alignmentGenerationId . Plan.alignmentPlanBindingGeneration . snd) (Plan.alignmentPlanBindings second)
      currentOwners state = [entry | entry@(_, obligation) <- Alignment.alignmentObligationEntries state, Protocol.alignmentObligationDestinationGeneration obligation `elem` currentIds]
      third = planWithMembersAt 2 baseEdges (Just second) (replacementMembers 9)
      later = promote 3 third invalidated
  assertBool "old bootstrap still names the superseded Store" (any ((== Protocol.destinationStore deltaB storeB) . Alignment.bootstrapImportKeyDestination . fst) (Alignment.bootstrapImportEntries current))
  assertEqual "independent A is carried across B replacement" (generationFor deltaA first) (generationFor deltaA second)
  assertBool "the old plan is invalidated" (Alignment.alignmentPlanInvalidated (Plan.alignmentPlanId first) invalidated)
  assertBool "unrelated current plan remains valid" (not (Alignment.alignmentPlanInvalidated (Plan.alignmentPlanId second) invalidated))
  assertEqual "current relation ownership survives old incarnation loss" (currentOwners current) (currentOwners invalidated)
  assertEqual "current plan frontier is preserved" (Just second) (Alignment.currentAlignmentPlanForSort occurrence invalidated)
  assertEqual "carried member scheduling survives the old-plan tombstone" (Right ()) (Alignment.validateAlignmentState invalidated)
  assertBool "later frontier advance still carries unrelated A" (generationFor deltaA first == generationFor deltaA third)
  assertBool "frozen old invalidation never retroactively invalidates A" (not (Alignment.generationCurrentPlanInvalidated (generationFor deltaA third) later))
  assertEqual "later frontier preserves exact current owner identities" (currentOwners invalidated) (currentOwners later)
  assertEqual "frozen affected generation set remains valid at later frontiers" (Right ()) (Alignment.validateAlignmentState later)

caseRemovedRelation :: Assertion
caseRemovedRelation = do
  let first = planAt 0 baseEdges Nothing 0
      initial = readyOwner first
      oldId = findObligation deltaA first initial
      started = startAttempt oldId initial
      removedPlan = planAt 1 [] (Just first) 8
      removed = promote 2 removedPlan started
      removedReady = certifyPlan removedPlan removed
      restoredPlan = planAt 2 baseEdges (Just removedPlan) 9
      restored = accept heraldA restoredPlan (accept heraldB restoredPlan (promote 3 restoredPlan removedReady))
      newId = findObligation deltaA restoredPlan restored
      restarted = startAttempt newId restored
  assertBool "removed owner waits in its exact old closing scope" (Alignment.alignmentObligationIsClosing oldId removed)
  assertBool "re-add cannot reactivate the old owner" (Alignment.alignmentObligationIsClosing oldId restarted)
  assertBool "re-added relation has a fresh obligation ID" (oldId /= newId)
  let subscriptions = map (Protocol.alignmentAttemptSubscriptionId . snd) (Alignment.alignmentAttemptEntries restarted)
  assertEqual "old closing and new active attempts have distinct subscription IDs" 2 (Set.size (Set.fromList subscriptions))
  assertEqual "re-added relation has one non-closing owner" [newId] [identifier | (identifier, _) <- Alignment.activeAlignmentObligationEntries restarted, not (Alignment.alignmentObligationIsClosing identifier restarted)]
  assertEqual "old and new stream transcripts coexist" 2 (length (Transfer.destinationSubscriptionEntries (Alignment.alignmentTransferState restarted)))
  assertEqual "remove and re-add owner invariant" (Right ()) (Alignment.validateAlignmentState restarted)

caseAdditionalCause :: Assertion
caseAdditionalCause = do
  let first = planAt 0 baseEdges Nothing 0
      initial = readyOwner first
      repeated = promote 4 first initial
  assertEqual "new cause creates no ordinary work" (Alignment.alignmentObligationEntries initial) (Alignment.alignmentObligationEntries repeated)
  assertEqual "new cause creates no bootstrap work" (Alignment.bootstrapImportEntries initial) (Alignment.bootstrapImportEntries repeated)
  assertEqual "cause replay is idempotent" repeated (promote 4 first repeated)
  assertEqual "covered causes leave no live debt" [] (Alignment.liveAlignmentStructuralDebtEntries repeated)
  assertEqual "same-plan coverage invariant" (Right ()) (Alignment.validateAlignmentState repeated)

caseContinuingInputClosure :: Assertion
caseContinuingInputClosure = do
  let first = planAt 0 baseEdges Nothing 0
      childPlan = planAt 1 (baseEdges <> [Graph.edgePayload (Graph.DeltaVertex deltaB) (Graph.DeltaVertex deltaC) Graph.Preserve]) (Just first) 8
      owner = startAttempt (obligationFor deltaA first) (readyOwner first)
      attempt = case Alignment.alignmentAttemptEntries owner of
        [(_, retained)] -> retained
        retained -> error ("expected one continuing input attempt, got " <> show (length retained))
      subscription = Protocol.alignmentAttemptSubscriptionId attempt
      generation = generationIdFor deltaC childPlan
      childState = promote 2 childPlan owner
      stream = Alignment.alignmentTransferState childState
      barrier = fst (Transfer.commitPredecessorInputClosures (checked "prepare child input barrier" (Transfer.preparePredecessorInputClosures generation (Set.singleton subscription) stream)))
      witnessed = advanceLive revision barrier
      localSettled = fst (Transfer.commitLocalRoutePrefixSettlement (checked "settle child's local route prefix" (Transfer.prepareLocalRoutePrefixSettlement generation heraldB witnessed)))
      prefix = Transfer.memberReadinessPrefix subscription revision revision revision
      sealed = fst (Transfer.commitPredecessorInputClosureReceipt (checked "seal child input prefix" (Transfer.preparePredecessorInputClosureReceipt generation heraldB storeB revision [prefix] localSettled)))
      nextRevision = Domain.nextStoreRevision revision
      appliedChange = fst (Transfer.commitDestinationWork (checked "advance continuing input data" (Transfer.prepareDestinationChange (Protocol.alignmentChange subscription nextRevision evidence) sealed)))
      withChange = fst (Transfer.commitAppliedDestinationEvidence (checked "retain applied continuing evidence" (Transfer.prepareAppliedDestinationEvidence [Transfer.appliedDestinationEvidence subscription (Protocol.destinationStore deltaB storeB) evidence Route.Normal nextRevision Transfer.AppliedContinuingChange Transfer.StoreAppliedOrAlreadyApplied] appliedChange)))
      advanced = advanceLive nextRevision withChange
      receiptEntries = Transfer.predecessorInputClosureReceiptEntries sealed
      current = maybe (error "continuing destination disappeared") id (Transfer.lookupDestinationSubscription subscription advanced)
      evidence = Protocol.primordialRetainedPublicationEvidence publication (Debt.sortOccurrenceSortId occurrence) (Debt.sortOccurrenceDefinition occurrence) (Value.canonicalValueBytes (Value.boolValue True)) Route.Normal
      publication = Identity.publicationId (checked "publication writer" (Identity.mkNablaId (fixtureIdentifierBytes 40))) Identity.genesisAuthorityEpoch heraldA (Identity.nablaSequence 1)
      advanceLive through = fst . Transfer.commitDestinationWork . checked "advance continuing input Live" . Transfer.prepareDestinationLive (Protocol.alignmentLive subscription through)
      bootstrapSubscription = Protocol.alignmentSubscriptionId heraldA (Protocol.nextAlignmentSubscriptionSequence Protocol.firstAlignmentSubscriptionSequence)
      bootstrapObligation = checked "late bootstrap obligation" (Protocol.alignmentObligation (Protocol.alignmentObligationId heraldA Protocol.firstAlignmentObligationSequence) (cause 2) (Debt.sortOccurrenceSortId occurrence) (Debt.sortOccurrenceDefinition occurrence) generation (Protocol.destinationStore deltaC storeC :| []) (generationIdFor deltaB first) Route.Normal)
      bootstrapSubscribe = checked "late bootstrap subscribe" (Protocol.alignmentSubscribe bootstrapObligation bootstrapSubscription storeB (Protocol.BootstrapProof generation (Domain.deriveBootstrapEvidenceDigest "late-child-prefix")))
      lateSource = fst (Transfer.commitSourceSubscription (checked "retain late bootstrap source" (Transfer.prepareSourceSubscription bootstrapSubscribe nextRevision [] Transfer.SourceLiveWithheld advanced)))
      released = fst (Transfer.commitSourceLiveRelease (checked "reuse immutable closure for late source" (Transfer.prepareSourceLiveRelease generation storeB bootstrapSubscription lateSource)))
  assertEqual "A->B relation remains carried while C is created" (generationFor deltaB first) (generationFor deltaB childPlan)
  assertEqual "receipt stays immutable while ordinary data advances" receiptEntries (Transfer.predecessorInputClosureReceiptEntries advanced)
  assertEqual "sealed barrier retains the exact receipt lower bound" (Transfer.predecessorInputClosureEntries sealed) (Transfer.predecessorInputClosureEntries advanced)
  assertEqual "continuing stream is not cancelled" Nothing (Transfer.destinationSubscriptionCancellation current)
  assertEqual "continuing stream applies subsequent data" (Just nextRevision) (Transfer.destinationSubscriptionAppliedThrough current)
  assertEqual "continuing stream retains subsequent Live" (Just nextRevision) (Transfer.destinationSubscriptionLiveThrough current)
  assertEqual "continuing stream acknowledges subsequent data" (Just nextRevision) (Transfer.destinationSubscriptionAcknowledgedThrough current)
  assertEqual "continued stream satisfies immutable receipt invariants" (Right ()) (Transfer.validateAlignmentTransferState advanced)
  assertEqual "later bootstrap source reuses the original exact receipt" receiptEntries (Transfer.predecessorInputClosureReceiptEntries released)
  assertBool "later source releases Live above the retained lower bound" (maybe False Transfer.sourceSubscriptionLiveReleased (Transfer.lookupSourceSubscription bootstrapSubscription released))
  assertEqual "later release retains valid immutable source closure" (Right ()) (Transfer.validateAlignmentTransferState released)

readyOwner :: Plan.AlignmentPlan -> Alignment.State
readyOwner plan = certifyPlan plan (accept heraldA plan (accept heraldB plan (promote 1 plan Alignment.emptyState)))

promote :: Word64 -> Plan.AlignmentPlan -> Alignment.State -> Alignment.State
promote index plan owner = fst (Alignment.commitAlignmentPromotion prepared)
  where
    event = cause index
    debt = Debt.structuralConsequenceDebt (Debt.structuralDebtKey event Debt.TopologyAlignmentDebt occurrence Nothing) (Debt.structuralDebtEvidence Set.empty Set.empty Set.empty)
    withDebt = fst (Alignment.commitStructuralDebtRetention (checked "retain cause debt" (Alignment.prepareStructuralDebtRetention event (Debt.normalizeStructuralDebts [debt]) owner)))
    prepared = checked "promote explicit plan" (Alignment.prepareAlignmentPlanPromotion heraldB event plan withDebt)

accept :: Identity.HeraldEpoch -> Plan.AlignmentPlan -> Alignment.State -> Alignment.State
accept herald plan = fst . checked "retain current plan acceptance" . Alignment.retainAlignmentPlanAcceptance accepted
  where
    accepted = checked "plan acceptance" (Protocol.alignmentPlanAccepted (Plan.alignmentPlanId plan) herald Domain.EmptyHeraldPublicationPrefix)

certifyPlan :: Plan.AlignmentPlan -> Alignment.State -> Alignment.State
certifyPlan plan owner = foldl certify owner (map (Plan.alignmentPlanBindingGeneration . snd) (Plan.alignmentPlanBindings plan))
  where
    certify previous generation
      | any ((== Generation.alignmentGenerationId generation) . fst . fst) (Alignment.alignmentHistoricalCertificateEntries previous) = previous
      | otherwise =
          let identifier = Generation.alignmentGenerationId generation
              stores = map Domain.alignmentMemberStoreIncarnation (NonEmpty.toList (Domain.alignmentCutExactMembers (Generation.alignmentGenerationCut generation)))
              prefix = Domain.deriveMemberReadyEvidenceDigest "live-carry-history"
              readiness = [Protocol.classMemberReady Protocol.firstAlignmentEvidenceSequence identifier store prefix revision | store <- stores]
              withReady = foldl (\retained ready -> fst (Alignment.commitClassMemberReady (checked "retain history readiness" (Alignment.prepareClassMemberReady ready retained)))) previous readiness
              certificates = [Protocol.historicalCertificate identifier store (Protocol.classMemberReadySetDigest readiness) (Domain.deriveBootstrapEvidenceDigest (Domain.memberReadyEvidenceDigestBytes prefix)) revision | store <- stores]
           in foldl (\retained certificate -> fst (Alignment.commitHistoricalCertificate (checked "retain history certificate" (Alignment.prepareHistoricalCertificate certificate retained)))) withReady certificates

startAttempt :: Protocol.AlignmentObligationId -> Alignment.State -> Alignment.State
startAttempt identifier owner = Alignment.replaceAlignmentTransferState complete withAttempt
  where
    prepared = checked "prepare ordinary attempt" (Alignment.prepareAlignmentAttempt identifier owner)
    attempt = Alignment.preparedAlignmentAttempt prepared
    withAttempt = fst (Alignment.commitAlignmentAttempt prepared)
    transfer = fst (Transfer.commitDestinationAttempt (checked "retain destination" (Transfer.prepareDestinationAttempt attempt (Alignment.alignmentTransferState withAttempt))))
    subscription = Protocol.alignmentAttemptSubscriptionId attempt
    certificate = Protocol.alignmentAttemptHistoricalCertificate attempt
    digest = Protocol.alignmentSemanticSnapshotDigest (Protocol.historicalCertificateClassGeneration certificate) (Protocol.historicalCertificateSourceStoreIncarnation certificate) revision []
    complete =
      foldl
        apply
        transfer
        [ Protocol.AlignmentSnapshotStarted (Protocol.alignmentSnapshotStart subscription revision digest 1),
          Protocol.AlignmentSnapshotChunkTransferred (Protocol.alignmentSnapshotChunk subscription 0 []),
          Protocol.AlignmentSnapshotEnded (Protocol.alignmentSnapshotEnd subscription revision digest),
          Protocol.AlignmentLiveAdvertised (Protocol.alignmentLive subscription revision)
        ]
    apply retained control = fst . Transfer.commitDestinationWork . checked "apply empty retained snapshot" $ case control of
      Protocol.AlignmentSnapshotStarted value -> Transfer.prepareDestinationSnapshotStart value retained
      Protocol.AlignmentSnapshotChunkTransferred value -> Transfer.prepareDestinationSnapshotChunk value retained
      Protocol.AlignmentSnapshotEnded value -> Transfer.prepareDestinationSnapshotEnd value retained
      Protocol.AlignmentLiveAdvertised value -> Transfer.prepareDestinationLive value retained
      _ -> error "fixture control"

obligationFor :: Identity.DeltaId -> Plan.AlignmentPlan -> Protocol.AlignmentObligationId
obligationFor delta plan = findObligation delta plan (readyOwner plan)

findObligation :: Identity.DeltaId -> Plan.AlignmentPlan -> Alignment.State -> Protocol.AlignmentObligationId
findObligation delta plan owner = case [ identifier
                                       | (identifier, obligation) <- Alignment.activeAlignmentObligationEntries owner,
                                         Protocol.alignmentObligationSourceGeneration obligation == generationIdFor delta plan,
                                         Protocol.alignmentObligationDestinationGeneration obligation == generationIdFor deltaB plan
                                       ] of
  [identifier] -> identifier
  values -> error ("expected one relation owner, got " <> show values)

planAt :: Word64 -> [Graph.EdgePayload] -> Maybe Plan.AlignmentPlan -> Word64 -> Plan.AlignmentPlan
planAt index edges previous currentRevision = planWithMembersAt index edges previous inputs
  where
    inputs = [Generation.generationMemberInput delta store herald (Domain.storeRevisionFromWord64 currentRevision) | (delta, store, herald) <- [(deltaA, storeA, heraldA), (deltaB, storeB, heraldB), (deltaC, storeC, heraldA)]]

planWithMembersAt :: Word64 -> [Graph.EdgePayload] -> Maybe Plan.AlignmentPlan -> [Generation.GenerationMemberInput] -> Plan.AlignmentPlan
planWithMembersAt index edges previous inputs = case checked "prepare plan" prepared of
  Plan.AlignmentPlanReady plan -> plan
  Plan.AlignmentPlanHeld missing -> error ("fixture plan held on " <> show missing)
  where
    graph = checked "context" (Context.deriveContextGraph (map Graph.DeltaVertex [deltaA, deltaB, deltaC]) edges [deltaA, deltaB, deltaC])
    available = maybe Set.empty (Set.fromList . map (Generation.alignmentGenerationId . Plan.alignmentPlanBindingGeneration . snd) . Plan.alignmentPlanBindings) previous
    prepared = Plan.prepareAlignmentPlan occurrence (topology index) (placement index) graph inputs ((,Plan.AlignmentPredecessorUsable) <$> previous) available

generationFor :: Identity.DeltaId -> Plan.AlignmentPlan -> Generation.AlignmentGeneration
generationFor delta plan = case [Plan.alignmentPlanBindingGeneration binding | (contextClass, binding) <- Plan.alignmentPlanBindings plan, delta `elem` Context.contextClassMembers contextClass] of
  [generation] -> generation
  _ -> error "fixture generation"

generationIdFor :: Identity.DeltaId -> Plan.AlignmentPlan -> Domain.ContextClassGenerationId
generationIdFor delta = Generation.alignmentGenerationId . generationFor delta

baseEdges, changedEdges :: [Graph.EdgePayload]
baseEdges = [Graph.edgePayload (Graph.DeltaVertex deltaA) (Graph.DeltaVertex deltaB) Graph.Preserve]
changedEdges = baseEdges <> [Graph.edgePayload (Graph.DeltaVertex deltaC) (Graph.DeltaVertex deltaB) Graph.Preserve]

topology :: Word64 -> Topology.TopologyCut
topology index = checked "topology" (Topology.topologyCut (Topology.sameGenerationPredecessor predecessor) (Topology.topologyFrontier (Structural.emptyStructuralVersionVector membership) (Identity.controlIndex index)) (Topology.deriveTopologyOccurrenceDigest (Bytes.pack (show index))))
  where
    predecessor = checked "predecessor" (Identity.mkTopologyCutId (fixtureIdentifierBytes 90))

placement :: Word64 -> Domain.PhysicalPlacementRevisionVector
placement index = checked "placement" (Domain.physicalPlacementRevisionVector membership [(herald, checked "placement revision" (Domain.mkPlacementRevision (index + 1))) | herald <- [heraldA, heraldB]])

membership :: Membership.HeraldMembershipGeneration
membership = checked "membership" (Membership.genesisHeraldMembershipGeneration (checked "system" (Identity.mkSystemId (fixtureIdentifierBytes 1))) (heraldA :| [heraldB]))

occurrence :: Debt.SortOccurrence
occurrence = Debt.sortOccurrence (checked "sort" (Identity.mkSortId (fixtureIdentifierBytes 2))) (checked "occurrence" (Identity.mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 3)))

cause :: Word64 -> Consequence.StructuralConsequenceCause
cause index = Consequence.structuralOccurrenceCause (Identity.structuralOccurrenceId heraldB (checked "structural sequence" (Identity.mkStructuralSequence index)))

revision :: Domain.StoreRevision
revision = Domain.storeRevisionFromWord64 7

deltaA, deltaB, deltaC :: Identity.DeltaId
deltaA = checked "delta A" (Identity.mkDeltaId (fixtureIdentifierBytes 5))
deltaB = checked "delta B" (Identity.mkDeltaId (fixtureIdentifierBytes 6))
deltaC = checked "delta C" (Identity.mkDeltaId (fixtureIdentifierBytes 7))

storeA, storeB, storeC :: Identity.StoreIncarnationId
storeA = checked "store A" (Identity.mkStoreIncarnationId (fixtureIdentifierBytes 20))
storeB = checked "store B" (Identity.mkStoreIncarnationId (fixtureIdentifierBytes 21))
storeC = checked "store C" (Identity.mkStoreIncarnationId (fixtureIdentifierBytes 22))

heraldA, heraldB :: Identity.HeraldEpoch
heraldA = checked "Herald A" (Identity.mkHeraldEpoch (fixtureIdentifierBytes 10))
heraldB = checked "Herald B" (Identity.mkHeraldEpoch (fixtureIdentifierBytes 11))

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (error . ((label <> ": ") <>) . show) id
