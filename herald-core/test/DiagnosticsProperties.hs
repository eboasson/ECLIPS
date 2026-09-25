{-# LANGUAGE OverloadedStrings #-}

module DiagnosticsProperties (tests) where

import Data.ByteString.Char8 qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment qualified as Alignment
import Eclips.Domain.Context qualified as Context
import Eclips.Domain.Graph qualified as Graph
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Sort.Profile qualified as Profile
import Eclips.Domain.Structural qualified as Structural
import Eclips.Domain.Topology qualified as Topology
import Eclips.Herald.Alignment.Generation qualified as Generation
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks (..), runDiagnosticCheck)
import Eclips.Herald.Diagnostics (heraldStructuralReportCounts)
import Eclips.Herald.Discovery.Internal qualified as Discovery
import Eclips.Herald.EffectBatch (HeraldEffect (..), orderedEffectBatch)
import Eclips.Herald.Graph.Protocol qualified as Protocol
import Eclips.Herald.Input (PeerControl (PeerStructuralAppliedReported))
import Eclips.Herald.Internal.Diagnostics (alignmentInventoryCounts, alignmentPlanInventoryCounts, structuralReportCounts)
import Eclips.Herald.Structural.Debt qualified as Debt
import Eclips.Herald.UseCase.LabelPatch qualified as LabelPatch
import Eclips.Public.Types.ReceiptRetirement qualified as Receipt
import GenesisFixtures (fixtureIdentifierBytes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck (Gen, Property, chooseInt, conjoin, counterexample, elements, forAll, shuffle, testProperty, vectorOf, (===))

tests :: TestTree
tests =
  testGroup
    "opt-in Herald diagnostics"
    [ testCase "disabled audits do not construct or evaluate their evidence" caseDisabledAuditDemand,
      testProperty "label work summaries form an additive monoid" propLabelWorkMonoid,
      testProperty "label work snapshots preserve each cumulative category and no semantic work" propLabelWorkAccumulation,
      testProperty "explicit plan tables count created and carried bindings independently of birth history" propPlanInventory,
      testProperty "explicit plan inventory deduplicates only repeated plan presentation" propPlanInventoryOrder,
      testCase "empty explicit plans remain visible in the inventory" caseEmptyPlanInventory,
      testProperty "retained generation partitions reconcile across generated topology-only history" propInventoryHistory,
      testProperty "inventory order and duplicate presentation do not affect counts" propInventoryOrder,
      testProperty "dependency closure agrees with recursive DAG reference under generated source changes" propDependencyReference,
      testCase "changed source ancestry blocks downstream local-shape matches" caseDependencyCascade,
      testCase "a missing retained predecessor cannot produce a carry alias" caseMissingPredecessor,
      testCase "member incarnation and host changes affect the member and downstream relation" caseMemberChanges,
      testCase "membership and sort occurrence boundaries remain separate" caseBoundaries,
      testCase "relation direction and strength changes are distinguished from unchanged classes" caseRelations,
      testCase "placement-only progress is counted separately from class changes" casePlacement,
      testCase "immutable topology, process, and sort-definition inventories remain distinguishable" caseSortFamilies,
      testCase "structural reports partition initial, membership, vector, control and identical offers" caseReports,
      testProperty "progress headers count once and repeated offers use the within-batch baseline" propReportSequence,
      testCase "non-report effects do not demand predecessor state" caseReportDemand
    ]

caseDisabledAuditDemand :: Assertion
caseDisabledAuditDemand = do
  assertEqual
    "off does not demand the whole-state audit"
    (Right () :: Either String ())
    (runDiagnosticCheck DiagnosticChecksDisabled (error "audit must remain unevaluated"))
  assertEqual
    "on retains a reported contradiction"
    (Left "contradiction" :: Either String ())
    (runDiagnosticCheck DiagnosticChecksEnabled (Left "contradiction"))

labelWork :: Gen LabelPatch.LabelStructuralWork
labelWork = LabelPatch.LabelStructuralWork <$> count <*> count <*> count <*> count <*> count <*> count <*> count <*> count <*> count <*> count <*> count
  where
    count = fromIntegral <$> chooseInt (0, 128)

propLabelWorkMonoid :: Property
propLabelWorkMonoid =
  forAll labelWork $ \first ->
    forAll labelWork $ \second ->
      forAll labelWork $ \third ->
        conjoin
          [ mempty <> first === first,
            first <> mempty === first,
            (first <> second) <> third === first <> (second <> third)
          ]

propLabelWorkAccumulation :: Property
propLabelWorkAccumulation =
  forAll (chooseInt (0, 30)) $ \count ->
    forAll (vectorOf count labelWork) $ \observations ->
      let snapshot = Map.fromList . LabelPatch.labelStructuralWorkCounts
          initial = LabelPatch.initialState
          final = foldl' (flip LabelPatch.recordLabelStructuralWork) initial observations
          expected = foldl' (Map.unionWith (+)) (snapshot initial) [snapshot (LabelPatch.recordLabelStructuralWork observation initial) | observation <- observations]
       in conjoin
            [ snapshot final === expected,
              Map.size (snapshot final) === 11,
              LabelPatch.retainedLabelPatches final === LabelPatch.retainedLabelPatches initial,
              LabelPatch.heldTargetWork final === LabelPatch.heldTargetWork initial
            ]

propPlanInventory :: Property
propPlanInventory =
  forAll (chooseInt (1, 8)) $ \count ->
    let plans = explicitPlanHistory count
        counts = Map.fromList (alignmentPlanInventoryCounts "retained" plans)
        expected = fromIntegral count
     in conjoin
          [ lookupCount "alignment.plans.retained.total" counts === expected,
            lookupCount "alignment.plans.retained.empty" counts === 0,
            lookupCount "alignment.plans.retained.bindings.total" counts === 2 * expected,
            lookupCount "alignment.plans.retained.bindings.created" counts === 2,
            lookupCount "alignment.plans.retained.bindings.carried" counts === 2 * (expected - 1),
            lookupCount "alignment.plans.retained.generations.unique" counts === 2
          ]

propPlanInventoryOrder :: Property
propPlanInventoryOrder =
  let plans = explicitPlanHistory 4
   in forAll (shuffle (plans <> plans)) $ \shuffled ->
        alignmentPlanInventoryCounts "retained" shuffled === alignmentPlanInventoryCounts "retained" plans

caseEmptyPlanInventory :: Assertion
caseEmptyPlanInventory = do
  let counts = Map.fromList (alignmentPlanInventoryCounts "retained" [makeExplicitPlan 0 [] Nothing])
  assertEqual "the empty plan is retained" 1 (lookupCount "alignment.plans.retained.total" counts)
  assertEqual "the empty plan has no generation bindings" 1 (lookupCount "alignment.plans.retained.empty" counts)
  assertEqual "no invented binding" 0 (lookupCount "alignment.plans.retained.bindings.total" counts)

explicitPlanHistory :: Int -> [Plan.AlignmentPlan]
explicitPlanHistory count = reverse (foldl' next [] [0 .. count - 1])
  where
    next prior prefix = makeExplicitPlan (fromIntegral prefix) members (case prior of [] -> Nothing; latest : _ -> Just latest) : prior

makeExplicitPlan :: Word64 -> [Generation.GenerationMemberInput] -> Maybe Plan.AlignmentPlan -> Plan.AlignmentPlan
makeExplicitPlan prefix selectedMembers prior =
  case checked "explicit plan" (Plan.prepareAlignmentPlan (Debt.sortOccurrence sortId occurrence) topology placement context selectedMembers ((,Plan.AlignmentPredecessorUsable) <$> prior) Set.empty) of
    Plan.AlignmentPlanReady plan -> plan
    Plan.AlignmentPlanHeld missing -> error ("unchanged fixture unexpectedly held: " <> show missing)
  where
    context = checked "plan context" (Context.deriveContextGraph (map (Graph.DeltaVertex . Generation.generationMemberDelta) selectedMembers) (if null selectedMembers then [] else forward) (map Generation.generationMemberDelta selectedMembers))
    topology = checked "plan topology" (Topology.topologyCut (Topology.sameGenerationPredecessor predecessorCut) (Topology.topologyFrontier (Structural.emptyStructuralVersionVector membership) (Identity.controlIndex prefix)) (Topology.deriveTopologyOccurrenceDigest (ByteString.pack (show prefix))))
    placement = checked "plan placement" (Alignment.physicalPlacementRevisionVector membership [(member, checked "plan revision" (Alignment.mkPlacementRevision 1)) | member <- NonEmpty.toList (Membership.heraldMembershipGenerationActiveHeraldEpochs membership)])

propInventoryHistory :: Property
propInventoryHistory =
  forAll (chooseInt (1, 8)) $ \count ->
    let plans = history count
        counts = inventory plans
        expected = fromIntegral count
     in conjoin
          [ lookupCount "alignment.inventory.generation.total" counts === 2 * expected,
            categorySum counts === 2 * expected,
            lookupCount (generationKey "no_exact_prior_member_class") counts === 2,
            lookupCount (generationKey "unchanged_class") counts === 2 * (expected - 1),
            lookupCount (generationKey "unchanged_class.incoming_generation_ids_unchanged") counts === expected - 1,
            lookupCount (generationKey "dependency_equivalent") counts === 2 * (expected - 1),
            lookupCount "alignment.inventory.coordinate.comparable" counts === 2 * (expected - 1),
            lookupCount "alignment.inventory.coordinate.topology.changed" counts === 2 * (expected - 1),
            lookupCount "alignment.inventory.coordinate.placement.unchanged" counts === 2 * (expected - 1),
            lookupCount "alignment.inventory.sort.other.total" counts === 2 * expected,
            lookupCount "alignment.inventory.plan_coordinates.total" counts === expected,
            lookupCount "alignment.inventory.sort_occurrences.total" counts === 1
          ]

propInventoryOrder :: Property
propInventoryOrder =
  let plans = history 4
      generations = concatMap Generation.alignmentGenerationPlanGenerations plans
      relations = concatMap Generation.alignmentGenerationPlanRelations plans
   in forAll (shuffle (generations <> generations)) $ \shuffledGenerations ->
        forAll (shuffle (relations <> relations)) $ \shuffledRelations ->
          alignmentInventoryCounts shuffledGenerations shuffledRelations
            === alignmentInventoryCounts generations relations

propDependencyReference :: Property
propDependencyReference =
  forAll (chooseInt (1, 5)) $ \count ->
    forAll (vectorOf count (elements [forwardChain, weakFirstChain, forward, backward, merged, []])) $ \graphs ->
      forAll (vectorOf count (elements [storeA, storeC])) $ \stores ->
        let plans = variedHistory graphs stores
            counts = inventory plans
            actual = lookupCount (generationKey "dependency_equivalent") counts
            expected = referenceAliasCount plans
         in counterexample (show counts)
              $ conjoin
                [ actual === expected,
                  (actual <= lookupCount (generationKey "unchanged_class") counts) === True,
                  (lookupCount (generationKey "unchanged_class.incoming_generation_ids_unchanged") counts <= actual) === True,
                  categorySum counts === lookupCount "alignment.inventory.generation.total" counts
                ]

caseDependencyCascade :: Assertion
caseDependencyCascade = do
  let plans = variedHistory [forwardChain, weakFirstChain, weakFirstChain] [storeA, storeA, storeA]
      firstChange = inventory (take 2 plans)
      complete = inventory plans
  assertEqual "source A and downstream C retain immediate member/strength shapes" 2 (lookupCount (generationKey "unchanged_class") firstChange)
  assertEqual "C cannot inherit through B after B's upstream strength changes" 1 (lookupCount (generationKey "dependency_equivalent") firstChange)
  assertEqual "the later unchanged plan carries all three dependency-closed classes" 4 (lookupCount (generationKey "dependency_equivalent") complete)
  assertEqual "recursive reference observes the same blocked lineage" (referenceAliasCount plans) (lookupCount (generationKey "dependency_equivalent") complete)

caseMissingPredecessor :: Assertion
caseMissingPredecessor = do
  let plans = history 2
      allGenerations = concatMap Generation.alignmentGenerationPlanGenerations plans
      firstIds = Set.fromList (map Generation.alignmentGenerationId (Generation.alignmentGenerationPlanGenerations (headFixture plans)))
      retained = [generation | generation <- allGenerations, not (Set.member (Generation.alignmentGenerationId generation) firstIds && any ((== deltaA) . Alignment.alignmentMemberDelta) (NonEmpty.toList (Alignment.alignmentCutExactMembers (Generation.alignmentGenerationCut generation))))]
      counts = Map.fromList (alignmentInventoryCounts retained (concatMap Generation.alignmentGenerationPlanRelations plans))
  assertEqual "no aliases can cross the missing source predecessor" 0 (lookupCount (generationKey "dependency_equivalent") counts)
  assertEqual "all remaining generations are initial or conservatively unclassified" (lookupCount "alignment.inventory.generation.total" counts) (lookupCount (generationKey "no_exact_prior_member_class") counts)
  where
    headFixture (first : _) = first
    headFixture [] = error "two-plan fixture is empty"

-- Independent reference: recursively unfold the admitted predecessor/source
-- DAG, without a representative table, fixed-point pass, or production helper.
-- Generated inventories are small, so deliberately repeated reference work is
-- useful for checking the memo-free semantic rule against aggregate closure.
referenceAliasCount :: [Generation.AlignmentGenerationPlan] -> Word64
referenceAliasCount plans =
  fromIntegral (length [generation | generation <- generations, representative generation /= Generation.alignmentGenerationId generation])
  where
    generations = concatMap Generation.alignmentGenerationPlanGenerations plans
    relations = concatMap Generation.alignmentGenerationPlanRelations plans
    byId = Map.fromList [(Generation.alignmentGenerationId generation, generation) | generation <- generations]
    representative generation = case priorClasses generation of
      [prior]
        | exactMembers prior == exactMembers generation,
          membershipId prior == membershipId generation,
          incomingRepresentatives prior == incomingRepresentatives generation ->
            representative prior
      _ -> Generation.alignmentGenerationId generation
    priorClasses generation =
      [ prior
      | identifier <- Alignment.alignmentCutPredecessorGenerationIds (Generation.alignmentGenerationCut generation),
        Just prior <- [Map.lookup identifier byId],
        classKey prior == classKey generation
      ]
    incomingRepresentatives generation =
      Set.fromList
        [ (representative (byId Map.! Generation.alignmentGenerationRelationSource relation), Generation.alignmentGenerationRelationStrength relation)
        | relation <- relations,
          Generation.alignmentGenerationRelationDestination relation == Generation.alignmentGenerationId generation
        ]
    classKey generation =
      let cut = Generation.alignmentGenerationCut generation
       in (Alignment.alignmentCutSortId cut, Alignment.alignmentCutSortDefinitionOccurrenceId cut, Set.fromList (map Alignment.alignmentMemberDelta (NonEmpty.toList (Alignment.alignmentCutExactMembers cut))))
    exactMembers = Alignment.alignmentCutExactMembers . Generation.alignmentGenerationCut
    membershipId = Alignment.physicalPlacementRevisionMembershipGenerationId . Alignment.alignmentCutPhysicalPlacementRevisionVector . Generation.alignmentGenerationCut

variedHistory :: [[Graph.EdgePayload]] -> [Identity.StoreIncarnationId] -> [Generation.AlignmentGenerationPlan]
variedHistory graphs stores = go 0 Nothing (zip graphs stores)
  where
    go _ _ [] = []
    go index prior ((edges, store) : rest) =
      let selectedMembers = [Generation.generationMemberInput deltaA store heraldA Alignment.initialStoreRevision, memberB, memberC]
          plan = makePlan sortId occurrence membership index 1 selectedMembers edges prior
       in plan : go (index + 1) (Just plan) rest

caseMemberChanges :: Assertion
caseMemberChanges = do
  let storeChanged = [Generation.generationMemberInput deltaA storeC heraldA Alignment.initialStoreRevision, memberB]
      hostChanged = [Generation.generationMemberInput deltaA storeA heraldB Alignment.initialStoreRevision, memberB]
  mapM_
    (\changedMembers -> assertCategories "exact member triples include incarnation and host" (twoPlans membership occurrence changedMembers forward 1) [("no_exact_prior_member_class", 2), ("member_identity_changed", 1), ("incoming_relation_changed", 1)])
    [storeChanged, hostChanged]

caseBoundaries :: Assertion
caseBoundaries = do
  assertCategories "a membership boundary is not a reusable class" (twoPlans widerMembership occurrence members forward 1) [("no_exact_prior_member_class", 2), ("membership_changed", 2)]
  assertCategories "a new sort occurrence has no same-occurrence predecessor" (twoPlans membership otherOccurrence members forward 1) [("no_exact_prior_member_class", 4)]

caseRelations :: Assertion
caseRelations = do
  assertCategories "reversing direction changes both incoming signatures" (twoPlans membership occurrence members backward 1) [("no_exact_prior_member_class", 2), ("incoming_relation_changed", 2)]
  assertCategories "weakening changes the destination's relation" (twoPlans membership occurrence members weakForward 1) [("no_exact_prior_member_class", 2), ("incoming_relation_changed", 1), ("unchanged_class", 1)]
  assertCategories "merging two member classes has no exact prior class" (twoPlans membership occurrence members merged 1) [("no_exact_prior_member_class", 3)]

casePlacement :: Assertion
casePlacement = do
  let counts = inventory (twoPlans membership occurrence members forward 2)
  assertEqual "both classes have unchanged semantic shape" 2 (lookupCount (generationKey "unchanged_class") counts)
  assertEqual "both placement coordinates changed" 2 (lookupCount "alignment.inventory.coordinate.placement.changed" counts)
  assertEqual "placement coordinates partition comparable pairs" (lookupCount "alignment.inventory.coordinate.comparable" counts) (lookupCount "alignment.inventory.coordinate.placement.changed" counts + lookupCount "alignment.inventory.coordinate.placement.unchanged" counts)

caseSortFamilies :: Assertion
caseSortFamilies = do
  let selected = [makePlan (Profile.profileSortFor Profile.EdgeRole) occurrence membership 0 1 members forward Nothing, makePlan (Profile.profileSortFor Profile.SortDefinitionRole) occurrence membership 0 1 members forward Nothing, makePlan (Profile.profileSortFor Profile.ProcessEpochRole) occurrence membership 0 1 members forward Nothing]
      counts = inventory selected
  assertEqual "structural carrier sort is explicit" 2 (lookupCount "alignment.inventory.sort.immutable_topology.total" counts)
  assertEqual "sort-definition carrier is explicit" 2 (lookupCount "alignment.inventory.sort.predefined_sort_definition.total" counts)
  assertEqual "mutable process carrier sort is explicit" 2 (lookupCount "alignment.inventory.sort.process_epoch.total" counts)
  assertEqual "all initial generations are partitioned" 6 (categorySum counts)
  assertEqual "empty inventory reports an explicit zero" (Just 0) (Map.lookup "alignment.inventory.generation.total" (inventory []))

assertCategories :: String -> [Generation.AlignmentGenerationPlan] -> [(String, Word64)] -> Assertion
assertCategories message plans expected = do
  let counts = inventory plans
  mapM_ (\category -> assertEqual (message <> ": " <> category) (Map.findWithDefault 0 category (Map.fromList expected)) (lookupCount (generationKey category) counts)) generationCategories
  assertEqual (message <> ": partition sum") (lookupCount "alignment.inventory.generation.total" counts) (categorySum counts)

generationCategories :: [String]
generationCategories = ["no_exact_prior_member_class", "member_identity_changed", "membership_changed", "incoming_relation_changed", "unchanged_class"]

categorySum :: Map.Map String Word64 -> Word64
categorySum counts = sum [lookupCount (generationKey category) counts | category <- generationCategories]

generationKey :: String -> String
generationKey = ("alignment.inventory.generation." <>)

lookupCount :: String -> Map.Map String Word64 -> Word64
lookupCount = Map.findWithDefault 0

inventory :: [Generation.AlignmentGenerationPlan] -> Map.Map String Word64
inventory plans = Map.fromList (alignmentInventoryCounts (concatMap Generation.alignmentGenerationPlanGenerations plans) (concatMap Generation.alignmentGenerationPlanRelations plans))

history :: Int -> [Generation.AlignmentGenerationPlan]
history count = go 0 Nothing
  where
    go index prior
      | index >= count = []
      | otherwise = let plan = makePlan sortId occurrence membership (fromIntegral index) 1 members forward prior in plan : go (index + 1) (Just plan)

twoPlans :: Membership.HeraldMembershipGeneration -> Identity.SortDefinitionOccurrenceId -> [Generation.GenerationMemberInput] -> [Graph.EdgePayload] -> Word64 -> [Generation.AlignmentGenerationPlan]
twoPlans selectedMembership selectedOccurrence selectedMembers edges revision =
  let first = makePlan sortId occurrence membership 0 1 members forward Nothing
   in [first, makePlan sortId selectedOccurrence selectedMembership 1 revision selectedMembers edges (Just first)]

makePlan :: Identity.SortId -> Identity.SortDefinitionOccurrenceId -> Membership.HeraldMembershipGeneration -> Word64 -> Word64 -> [Generation.GenerationMemberInput] -> [Graph.EdgePayload] -> Maybe Generation.AlignmentGenerationPlan -> Generation.AlignmentGenerationPlan
makePlan selectedSort selectedOccurrence selectedMembership prefix revision selectedMembers edges prior =
  case checked "generation preparation" (Generation.prepareAlignmentGenerationPlan (Debt.sortOccurrence selectedSort selectedOccurrence) topology placement context selectedMembers priorGenerations priorRelations (Set.fromList (map Generation.alignmentGenerationId priorGenerations))) of
    Generation.AlignmentGenerationReady plan -> plan
    Generation.AlignmentGenerationHeld missing -> error ("fixture held: " <> show missing)
  where
    priorGenerations = maybe [] Generation.alignmentGenerationPlanGenerations prior
    priorRelations = maybe [] Generation.alignmentGenerationPlanRelations prior
    context = checked "context" (Context.deriveContextGraph (map (Graph.DeltaVertex . Generation.generationMemberDelta) selectedMembers) edges (map Generation.generationMemberDelta selectedMembers))
    topology = checked "topology" (Topology.topologyCut (Topology.sameGenerationPredecessor predecessorCut) (Topology.topologyFrontier (Structural.emptyStructuralVersionVector selectedMembership) (Identity.controlIndex prefix)) (Topology.deriveTopologyOccurrenceDigest (ByteString.pack (show prefix))))
    placement = checked "placement" (Alignment.physicalPlacementRevisionVector selectedMembership [(member, checked "revision" (Alignment.mkPlacementRevision revision)) | member <- NonEmpty.toList (Membership.heraldMembershipGenerationActiveHeraldEpochs selectedMembership)])

caseReports :: Assertion
caseReports = do
  let initial = report membership 0
      changedControl = report membership 1
      changedVector = Protocol.structuralAppliedReport heraldA advancedVector (Identity.controlIndex 1)
      changedMembership = report widerMembership 1
      effects = orderedEffectBatch [send binding initial, send binding changedControl, send binding changedVector, send binding changedMembership, send otherBinding changedMembership, withProgress otherBinding changedMembership]
      counts = Map.fromList (structuralReportCounts (Map.singleton binding initial) effects)
  assertEqual "one count per semantic report" 6 (lookupCount "structural_report.emitted.total" counts)
  mapM_ (\(category, expected) -> assertEqual category expected (lookupCount ("structural_report.emitted." <> category) counts)) [("initial", 1), ("membership_changed", 1), ("vector_changed", 1), ("control_prefix_only", 1), ("identical", 2)]

propReportSequence :: Property
propReportSequence =
  forAll (chooseInt (0, 25)) $ \count ->
    let effects = orderedEffectBatch (concat [[send binding (report membership prefix), withProgress binding (report membership prefix)] | prefix <- [1 .. fromIntegral count]])
        counts = Map.fromList (structuralReportCounts (Map.singleton binding (report membership 0)) effects)
     in counterexample (show counts) $ conjoin [lookupCount "structural_report.emitted.total" counts === 2 * fromIntegral count, lookupCount "structural_report.emitted.control_prefix_only" counts === fromIntegral count, lookupCount "structural_report.emitted.identical" counts === fromIntegral count]

caseReportDemand :: Assertion
caseReportDemand =
  assertEqual "irrelevant effects inspect no state or payload" [] (heraldStructuralReportCounts (error "diagnostic forced unrelated predecessor") (orderedEffectBatch [SendJoinReply 1 (error "diagnostic forced unrelated reply")]))

send :: Discovery.PeerBinding -> Protocol.StructuralAppliedReport -> HeraldEffect
send target = SendPeerControl target . PeerStructuralAppliedReported

withProgress :: Discovery.PeerBinding -> Protocol.StructuralAppliedReport -> HeraldEffect
withProgress target = SendPeerControlWithProgress target (Receipt.receiptRetirementPrefix Nothing) . PeerStructuralAppliedReported

report :: Membership.HeraldMembershipGeneration -> Word64 -> Protocol.StructuralAppliedReport
report selectedMembership prefix = Protocol.structuralAppliedReport heraldA (Structural.emptyStructuralVersionVector selectedMembership) (Identity.controlIndex prefix)

advancedVector :: Structural.StructuralVersionVector
advancedVector = checked "advanced vector" (Structural.mkStructuralVersionVector membership [(heraldA, Structural.structuralPrefixThrough (checked "sequence" (Identity.mkStructuralSequence 1))), (heraldB, Structural.emptyStructuralPrefix)])

binding, otherBinding :: Discovery.PeerBinding
binding = Discovery.PeerBinding heraldId heraldB Discovery.firstPeerBindingGeneration candidate
otherBinding = Discovery.PeerBinding heraldId heraldB (Discovery.nextPeerBindingGeneration Discovery.firstPeerBindingGeneration) candidate

candidate :: Discovery.PeerCandidate
candidate = Discovery.PeerCandidate heraldId heraldA (Discovery.ConnectionNonce 1)

members :: [Generation.GenerationMemberInput]
members = [Generation.generationMemberInput deltaA storeA heraldA Alignment.initialStoreRevision, memberB]

memberB, memberC :: Generation.GenerationMemberInput
memberB = Generation.generationMemberInput deltaB storeB heraldB Alignment.initialStoreRevision
memberC = Generation.generationMemberInput deltaC storeD heraldB Alignment.initialStoreRevision

forward, backward, weakForward, merged, forwardChain, weakFirstChain :: [Graph.EdgePayload]
-- These are context projections at distinct cuts, not object-update commands.
-- Changing endpoints or strength models deleting an immutable edge and creating
-- a replacement with a fresh identity; this projection retains only payloads.
forward = [Graph.edgePayload (Graph.DeltaVertex deltaA) (Graph.DeltaVertex deltaB) Graph.Preserve]
backward = [Graph.edgePayload (Graph.DeltaVertex deltaB) (Graph.DeltaVertex deltaA) Graph.Preserve]
weakForward = [Graph.edgePayload (Graph.DeltaVertex deltaA) (Graph.DeltaVertex deltaB) Graph.Weaken]
merged = forward <> backward
forwardChain = forward <> [Graph.edgePayload (Graph.DeltaVertex deltaB) (Graph.DeltaVertex deltaC) Graph.Preserve]
weakFirstChain = weakForward <> [Graph.edgePayload (Graph.DeltaVertex deltaB) (Graph.DeltaVertex deltaC) Graph.Preserve]

membership, widerMembership :: Membership.HeraldMembershipGeneration
membership = checked "membership" (Membership.genesisHeraldMembershipGeneration system (heraldA :| [heraldB]))
widerMembership = checked "wider membership" (Membership.genesisHeraldMembershipGeneration system (heraldA :| [heraldB, heraldC]))

system :: Identity.SystemId
system = checked "system" (Identity.mkSystemId (fixtureIdentifierBytes 1))

sortId :: Identity.SortId
sortId = checked "sort" (Identity.mkSortId (fixtureIdentifierBytes 2))

occurrence, otherOccurrence :: Identity.SortDefinitionOccurrenceId
occurrence = checked "occurrence" (Identity.mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 3))
otherOccurrence = checked "other occurrence" (Identity.mkSortDefinitionOccurrenceId (fixtureIdentifierBytes 4))

deltaA, deltaB, deltaC :: Identity.DeltaId
deltaA = checked "delta A" (Identity.mkDeltaId (fixtureIdentifierBytes 5))
deltaB = checked "delta B" (Identity.mkDeltaId (fixtureIdentifierBytes 6))
deltaC = checked "delta C" (Identity.mkDeltaId (fixtureIdentifierBytes 15))

storeA, storeB, storeC, storeD :: Identity.StoreIncarnationId
storeA = checked "store A" (Identity.mkStoreIncarnationId (fixtureIdentifierBytes 7))
storeB = checked "store B" (Identity.mkStoreIncarnationId (fixtureIdentifierBytes 8))
storeC = checked "store C" (Identity.mkStoreIncarnationId (fixtureIdentifierBytes 9))
storeD = checked "store D" (Identity.mkStoreIncarnationId (fixtureIdentifierBytes 16))

heraldA, heraldB, heraldC :: Identity.HeraldEpoch
heraldA = checked "Herald A" (Identity.mkHeraldEpoch (fixtureIdentifierBytes 10))
heraldB = checked "Herald B" (Identity.mkHeraldEpoch (fixtureIdentifierBytes 11))
heraldC = checked "Herald C" (Identity.mkHeraldEpoch (fixtureIdentifierBytes 12))

heraldId :: Identity.HeraldId
heraldId = checked "Herald ID" (Identity.mkHeraldId (fixtureIdentifierBytes 13))

predecessorCut :: Identity.TopologyCutId
predecessorCut = checked "predecessor cut" (Identity.mkTopologyCutId (fixtureIdentifierBytes 14))

checked :: (Show problem) => String -> Either problem value -> value
checked _ (Right value) = value
checked name (Left problem) = error (name <> ": " <> show problem)
