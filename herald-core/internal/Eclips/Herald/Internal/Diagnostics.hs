-- | Read-only aggregate diagnostics. The runtime demands these only when an
-- explicit work-count sink is installed. No derived inventory is retained.
module Eclips.Herald.Internal.Diagnostics
  ( alignmentInventoryCounts,
    alignmentPlanInventoryCounts,
    structuralReportCounts,
  )
where

import Data.List (sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment
  ( alignmentCutAttempt,
    alignmentCutExactMembers,
    alignmentCutPhysicalPlacementRevisionVector,
    alignmentCutPredecessorGenerationIds,
    alignmentCutSortDefinitionOccurrenceId,
    alignmentCutSortId,
    alignmentMemberDelta,
    alignmentMemberHerald,
    alignmentMemberStoreIncarnation,
    physicalPlacementRevisionMembershipGenerationId,
  )
import Eclips.Domain.Sort.Profile (PredefinedSortRole (..), profileSortFor)
import Eclips.Herald.Alignment.Generation
  ( AlignmentGeneration,
    AlignmentGenerationRelation,
    alignmentGenerationCut,
    alignmentGenerationId,
    alignmentGenerationRelationDestination,
    alignmentGenerationRelationSource,
    alignmentGenerationRelationStrength,
    alignmentGenerationTopologyCutId,
  )
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Discovery (PeerBinding, PeerHelloDisposition (PeerHelloAccepted))
import Eclips.Herald.EffectBatch (EffectBatch, HeraldEffect (..), effectBatchMembers)
import Eclips.Herald.Graph.Protocol
  ( StructuralAppliedReport,
    structuralAppliedReportControlPrefix,
    structuralAppliedReportMembershipGenerationId,
    structuralAppliedReportVersionVector,
  )
import Eclips.Herald.Input (PeerControl (PeerStructuralAppliedReported))

-- | Classify each retained canonical generation once. An exact prior class
-- means the same sort occurrence and delta set among the cut's selected
-- predecessors; incoming source predecessors are not automatically a prior
-- version of the destination. Incoming relation signatures deliberately erase
-- generation IDs but retain source sort, exact member triples and strength.
-- Thus @unchanged_class@ is a necessary-condition opportunity, not proof that
-- recursive history or subscriptions can safely be reused. The additional
-- incoming-ID-stable subset identifies the stricter direct-source condition.
-- @dependency_equivalent@ closes the unchanged-class candidates under source
-- dependencies: a generation can alias its unique prior class only when every
-- incoming source has the same representative and strength on both sides.
-- This still makes no claim about invalidated plans or whether certificates
-- and obligations were ready when a generation was created.
--
-- This is a birth-generation inventory, not a count of current plans,
-- preparation attempts or promotions. After live carry, an unchanged current
-- plan need not create any generation; these historical comparisons therefore
-- do not measure all carry opportunities. @plan_coordinates@ counts distinct
-- birth coordinates only. Explicit plan tables are counted separately below.
-- Different Heralds can retain the same generation. Missing or ambiguous prior
-- classes conservatively enter the no-exact-prior bucket.
alignmentInventoryCounts ::
  [AlignmentGeneration] -> [AlignmentGenerationRelation] -> [(String, Word64)]
alignmentInventoryCounts supplied relations =
  Map.toAscList
    ( Map.insert "alignment.inventory.plan_coordinates.total" (fromIntegral (Set.size planCoordinates))
        . Map.insert "alignment.inventory.sort_occurrences.total" (fromIntegral (Set.size sortOccurrences))
        $ foldl' countGeneration emptyCounts (Map.elems generations)
    )
  where
    categories = ["no_exact_prior_member_class", "member_identity_changed", "membership_changed", "incoming_relation_changed", "unchanged_class"]
    families = ["immutable_topology", "process_epoch", "predefined_sort_definition", "other"]
    emptyCounts =
      Map.fromList
        [ (key, 0)
        | key <-
            [generationKey "total", generationKey "unchanged_class.incoming_generation_ids_unchanged", generationKey "dependency_equivalent", "alignment.inventory.coordinate.comparable"]
              <> map generationKey categories
              <> [coordinateKey coordinate changed | coordinate <- ["topology", "placement", "attempt"], changed <- [False, True]]
              <> ["alignment.inventory.sort." <> family <> "." <> category | family <- families, category <- "total" : categories]
              <> ["alignment.inventory.sort." <> family <> ".dependency_equivalent" | family <- families]
        ]
    generations = Map.fromList [(alignmentGenerationId generation, generation) | generation <- supplied]
    sortOccurrences = Set.fromList (map sortCoordinate (Map.elems generations))
    planCoordinates = Set.fromList [(sortCoordinate generation, alignmentGenerationTopologyCutId generation, placement generation, attempt generation) | generation <- Map.elems generations]
    incoming =
      Map.fromListWith
        Set.union
        [ (alignmentGenerationRelationDestination relation, Set.singleton (alignmentGenerationRelationSource relation, alignmentGenerationRelationStrength relation))
        | relation <- relations
        ]
    incomingIds generation = Map.findWithDefault Set.empty (alignmentGenerationId generation) incoming
    incomingShape generation =
      traverse
        (\(source, strength) -> (,strength) . memberIdentity <$> Map.lookup source generations)
        (Set.toAscList (incomingIds generation))
    exactPrior generation
      | any (`Map.notMember` generations) predecessorIds = []
      | otherwise =
          [ prior
          | identifier <- predecessorIds,
            Just prior <- [Map.lookup identifier generations],
            memberClass prior == memberClass generation
          ]
      where
        predecessorIds = alignmentCutPredecessorGenerationIds (alignmentGenerationCut generation)
    -- Every redirect follows the retained predecessor DAG. An enabled equality
    -- remains equal as representatives merge, so this finite fixed point only
    -- moves representatives toward older ancestors; it never retracts an alias.
    representatives = closeDependencies (Map.mapWithKey (\identifier _ -> identifier) generations)
    candidates =
      [ (generation, prior)
      | generation <- Map.elems generations,
        [prior] <- [exactPrior generation],
        changeCategory prior generation == "unchanged_class"
      ]
    closeDependencies current =
      let next = foldl' inheritRepresentative current candidates
       in if current == next then current else closeDependencies next
    inheritRepresentative current (generation, prior)
      | normalizedIncoming current generation == normalizedIncoming current prior =
          Map.insert (alignmentGenerationId generation) (current Map.! alignmentGenerationId prior) current
      | otherwise = current
    normalizedIncoming current generation =
      Set.map (\(source, strength) -> (current Map.! source, strength)) (incomingIds generation)
    countGeneration counts generation =
      addCounts
        ( ["alignment.inventory.generation.total", "alignment.inventory.sort." <> sortFamily generation <> ".total"]
            <> [key | representatives Map.! alignmentGenerationId generation /= alignmentGenerationId generation, key <- [generationKey "dependency_equivalent", "alignment.inventory.sort." <> sortFamily generation <> ".dependency_equivalent"]]
            <> case exactPrior generation of
              [prior] -> comparableCounts prior generation
              _ -> [generationKey "no_exact_prior_member_class", "alignment.inventory.sort." <> sortFamily generation <> ".no_exact_prior_member_class"]
        )
        counts
    changeCategory prior generation
      | memberIdentity prior /= memberIdentity generation = "member_identity_changed"
      | membership prior /= membership generation = "membership_changed"
      | incomingShape prior == Nothing || fmap sort (incomingShape prior) /= fmap sort (incomingShape generation) = "incoming_relation_changed"
      | otherwise = "unchanged_class"
    comparableCounts prior generation =
      let category = changeCategory prior generation
       in [ generationKey category,
            "alignment.inventory.sort." <> sortFamily generation <> "." <> category,
            "alignment.inventory.coordinate.comparable",
            coordinateKey "topology" (alignmentGenerationTopologyCutId prior /= alignmentGenerationTopologyCutId generation),
            coordinateKey "placement" (placement prior /= placement generation),
            coordinateKey "attempt" (attempt prior /= attempt generation)
          ]
            <> [ generationKey "unchanged_class.incoming_generation_ids_unchanged"
               | category == "unchanged_class",
                 incomingIds prior == incomingIds generation
               ]
    memberClass generation =
      let cut = alignmentGenerationCut generation
       in (alignmentCutSortId cut, alignmentCutSortDefinitionOccurrenceId cut, sort (map alignmentMemberDelta (NonEmpty.toList (alignmentCutExactMembers cut))))
    memberIdentity generation =
      let cut = alignmentGenerationCut generation
       in ( alignmentCutSortId cut,
            alignmentCutSortDefinitionOccurrenceId cut,
            sort
              [ (alignmentMemberDelta member, alignmentMemberStoreIncarnation member, alignmentMemberHerald member)
              | member <- NonEmpty.toList (alignmentCutExactMembers cut)
              ]
          )
    placement = alignmentCutPhysicalPlacementRevisionVector . alignmentGenerationCut
    attempt = alignmentCutAttempt . alignmentGenerationCut
    membership = physicalPlacementRevisionMembershipGenerationId . placement
    sortCoordinate generation = let cut = alignmentGenerationCut generation in (alignmentCutSortId cut, alignmentCutSortDefinitionOccurrenceId cut)
    sortFamily generation
      | identifier == profileSortFor SortDefinitionRole = "predefined_sort_definition"
      | identifier == profileSortFor ProcessEpochRole = "process_epoch"
      | identifier `elem` map profileSortFor [NeutralVertexRole, EdgeRole, NablaRole, DeltaRole] = "immutable_topology"
      | otherwise = "other"
      where
        identifier = alignmentCutSortId (alignmentGenerationCut generation)
    generationKey = ("alignment.inventory.generation." <>)
    coordinateKey coordinate changed = "alignment.inventory.coordinate." <> coordinate <> if changed then ".changed" else ".unchanged"

-- | Count the actual binding dispositions in each distinct checked plan table.
-- Scope is one of the caller's finite retained/current/live inventory labels.
-- A generation carried through several plans contributes several bindings but
-- only one unique generation. These are retained observations per owner, not
-- lifetime construction events or counts deduplicated across replicas.
alignmentPlanInventoryCounts :: String -> [Plan.AlignmentPlan] -> [(String, Word64)]
alignmentPlanInventoryCounts scope supplied =
  [ count "total" plans,
    count "empty" (filter (null . Plan.alignmentPlanBindings) plans),
    count "bindings.total" bindings,
    count "bindings.created" (filter ((== Plan.AlignmentCreated) . Plan.alignmentPlanBindingDisposition) bindings),
    count "bindings.carried" (filter ((== Plan.AlignmentCarried) . Plan.alignmentPlanBindingDisposition) bindings),
    (prefix <> "generations.unique", fromIntegral (Set.size generations))
  ]
  where
    prefix = "alignment.plans." <> scope <> "."
    plans = Map.elems (Map.fromList [(Plan.alignmentPlanId plan, plan) | plan <- supplied])
    bindings = concatMap (map snd . Plan.alignmentPlanBindings) plans
    generations = Set.fromList (map (alignmentGenerationId . Plan.alignmentPlanBindingGeneration) bindings)
    count :: String -> [a] -> (String, Word64)
    count key values = (prefix <> key, fromIntegral (length values))

-- | Partition emitted reports relative to the last offered report in this
-- batch. Initial baselines come from the predecessor's current bindings and
-- local report. Binding acceptance resets its baseline; repair can legitimately
-- emit an identical report. Progress-bearing controls carry one semantic report
-- and are counted once. Non-report effects never demand their payloads.
structuralReportCounts ::
  Map PeerBinding StructuralAppliedReport -> EffectBatch -> [(String, Word64)]
structuralReportCounts initial = Map.toAscList . snd . foldl' countEffect (initial, Map.empty) . effectBatchMembers
  where
    countEffect (reports, counts) effect = case effect of
      SetPeerCandidateDisposition _ (PeerHelloAccepted _ binding) -> (Map.delete binding reports, counts)
      SendPeerControl binding (PeerStructuralAppliedReported report) -> countReport reports counts binding report
      SendPeerControlWithProgress binding _ (PeerStructuralAppliedReported report) -> countReport reports counts binding report
      _ -> (reports, counts)
    countReport reports counts binding report =
      let category = case Map.lookup binding reports of
            Nothing -> "initial"
            Just prior
              | structuralAppliedReportMembershipGenerationId prior /= structuralAppliedReportMembershipGenerationId report -> "membership_changed"
              | structuralAppliedReportVersionVector prior /= structuralAppliedReportVersionVector report -> "vector_changed"
              | structuralAppliedReportControlPrefix prior /= structuralAppliedReportControlPrefix report -> "control_prefix_only"
              | otherwise -> "identical"
       in (Map.insert binding report reports, addCounts ["structural_report.emitted.total", "structural_report.emitted." <> category] counts)

addCounts :: [String] -> Map String Word64 -> Map String Word64
addCounts keys counts = foldl' (\retained key -> Map.insertWith (+) key 1 retained) counts keys
