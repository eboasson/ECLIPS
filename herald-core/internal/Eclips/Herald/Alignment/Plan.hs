-- | Checked current routing plans, separate from immutable generation birth
-- evidence. This pure planner is not yet installed in the live alignment owner.
-- A carry witness proves historical equivalence, not current routing settlement.
module Eclips.Herald.Alignment.Plan
  ( AlignmentPlanId,
    alignmentPlanIdFromClaimedCoordinates,
    alignmentPlanIdFromClaimedCoordinatesAtAttempt,
    alignmentPlanIdAtTopology,
    alignmentPlanIdAtTopologyAtAttempt,
    alignmentPlanIdAttempt,
    alignmentPlanIdSort,
    alignmentPlanIdTopology,
    alignmentPlanIdPlacement,
    AlignmentPlan,
    alignmentPlanId,
    alignmentPlanPredecessor,
    alignmentPlanPredecessorStatus,
    alignmentPlanTopologyCut,
    alignmentPlanBindings,
    alignmentPlanRelations,
    alignmentPlanAnnouncer,
    alignmentGenerationBirthPlanId,
    AlignmentPlanBinding,
    alignmentPlanBindingGeneration,
    alignmentPlanBindingDisposition,
    AlignmentBindingDisposition (..),
    AlignmentPredecessorStatus (..),
    AlignmentPlanProblem (..),
    AlignmentPlanPreparation (..),
    prepareAlignmentPlan,
    prepareAlignmentPlanAtAttempt,
    prepareAlignmentResetPlan,
    AlignmentPlanClaim (..),
    alignmentPlanClaim,
    checkAlignmentPlanClaim,
    alignmentPlanAnnouncement,
    checkAlignmentPlanAnnouncement,
    AlignmentPlanLedger,
    emptyAlignmentPlanLedger,
    alignmentPlanEntries,
    lookupAlignmentPlan,
    alignmentPlanCoverage,
    AlignmentPlanRetention (..),
    retainAlignmentPlan,
    retainHistoricalAlignmentPlan,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Alignment
  ( AlignmentPlanAttempt,
    AlignmentShapeError (..),
    ContextClassGenerationId,
    PhysicalPlacementRevisionVector,
    alignmentCutAttempt,
    alignmentCutExactMembers,
    alignmentCutPhysicalPlacementRevisionVector,
    alignmentCutSortDefinitionOccurrenceId,
    alignmentCutSortId,
    alignmentMemberDelta,
    alignmentMemberHerald,
    alignmentMemberStoreIncarnation,
    initialAlignmentPlanAttempt,
    physicalPlacementRevisionEntries,
    physicalPlacementRevisionMemberSetDigest,
    physicalPlacementRevisionMembershipGenerationId,
  )
import Eclips.Domain.Context
  ( ContextClass,
    ContextGraph,
    contextClassMembers,
    contextGraphRelations,
    contextRelationDestination,
    contextRelationSource,
    contextRelationStrength,
  )
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Domain.StructuralConsequence (StructuralConsequenceCause)
import Eclips.Domain.Topology
  ( TopologyCut,
    deriveTopologyCutId,
    topologyCutFrontier,
    topologyFrontierMemberSetDigest,
    topologyFrontierMembershipGenerationId,
  )
import Eclips.Herald.Alignment.Generation
  ( AlignmentGeneration,
    AlignmentGenerationProblem,
    AlignmentGenerationRelation,
    GenerationMemberInput,
    alignmentGenerationCut,
    alignmentGenerationCutAnnounce,
    alignmentGenerationDraftClass,
    alignmentGenerationDraftPredecessors,
    alignmentGenerationDraftTopologyCutId,
    alignmentGenerationId,
    alignmentGenerationRelation,
    alignmentGenerationRelationDestination,
    alignmentGenerationRelationSource,
    alignmentGenerationRelationStrength,
    alignmentGenerationTopologyCutId,
    generationMemberDelta,
    generationMemberHerald,
    generationMemberStoreIncarnation,
    materializeAlignmentGenerationDraftAtAttempt,
    prepareAlignmentGenerationDrafts,
  )
import Eclips.Herald.Alignment.Plan.Identity
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Structural.Debt (SortOccurrence, sortOccurrence)

data AlignmentPlanBinding = AlignmentPlanBinding !AlignmentBindingDisposition !AlignmentGeneration
  deriving stock (Eq, Show)

alignmentPlanBindingGeneration :: AlignmentPlanBinding -> AlignmentGeneration
alignmentPlanBindingGeneration (AlignmentPlanBinding _ generation) = generation

alignmentPlanBindingDisposition :: AlignmentPlanBinding -> AlignmentBindingDisposition
alignmentPlanBindingDisposition (AlignmentPlanBinding disposition _) = disposition

data AlignmentPlan = AlignmentPlan !AlignmentPlanId !TopologyCut !(Maybe AlignmentPlanClaim) !AlignmentPredecessorStatus !(Map ContextClass AlignmentPlanBinding) !(Set AlignmentGenerationRelation) !Protocol.AlignmentPlanAnnounce
  deriving stock (Eq, Show)

alignmentPlanId :: AlignmentPlan -> AlignmentPlanId
alignmentPlanId (AlignmentPlan identifier _ _ _ _ _ _) = identifier

alignmentPlanPredecessor :: AlignmentPlan -> Maybe AlignmentPlanId
alignmentPlanPredecessor (AlignmentPlan _ _ predecessor _ _ _ _) = claimIdentifier <$> predecessor

-- Current topology evidence must survive even if every generation was born at
-- an older cut. Join/reconstruction must not recover it from a carried birth cut.
alignmentPlanTopologyCut :: AlignmentPlan -> TopologyCut
alignmentPlanTopologyCut (AlignmentPlan _ topology _ _ _ _ _) = topology

alignmentPlanBindings :: AlignmentPlan -> [(ContextClass, AlignmentPlanBinding)]
alignmentPlanBindings (AlignmentPlan _ _ _ _ bindings _ _) = Map.toAscList bindings

alignmentPlanRelations :: AlignmentPlan -> [AlignmentGenerationRelation]
alignmentPlanRelations (AlignmentPlan _ _ _ _ _ relations _) = Set.toAscList relations

-- Every routing coordinate, including an empty removal-only plan, has the
-- same fixed authority. Empty plans still name the predecessor of later work.
-- A carried class's birth announcer does not select current plan authority.
alignmentPlanAnnouncer :: AlignmentPlan -> Maybe HeraldEpoch
alignmentPlanAnnouncer plan = Just (minimum (fmap fst (physicalPlacementRevisionEntries (alignmentPlanIdPlacement (alignmentPlanId plan)))))

-- | The coordinate of immutable class birth evidence; current-plan consumers
-- must use their explicit plan binding instead of treating this as latest.
alignmentGenerationBirthPlanId :: AlignmentGeneration -> AlignmentPlanId
alignmentGenerationBirthPlanId generation = alignmentPlanIdFromClaimedCoordinatesAtAttempt (alignmentCutAttempt cut) (sortOccurrence (alignmentCutSortId cut) (alignmentCutSortDefinitionOccurrenceId cut)) (alignmentGenerationTopologyCutId generation) (alignmentCutPhysicalPlacementRevisionVector cut)
  where
    cut = alignmentGenerationCut generation

alignmentPlanPredecessorStatus :: AlignmentPlan -> AlignmentPredecessorStatus
alignmentPlanPredecessorStatus (AlignmentPlan _ _ _ status _ _ _) = status

data AlignmentPlanProblem
  = AlignmentPlanGenerationProblem !AlignmentGenerationProblem
  | AlignmentPlanShapeProblem !AlignmentShapeError
  | AlignmentPlanDifferentSortOccurrence !SortOccurrence !SortOccurrence
  | AlignmentPlanSameCoordinate !AlignmentPlanId
  | AlignmentPlanClaimMismatch
  | AlignmentPlanIdentityConflict !AlignmentPlanId
  | AlignmentPlanMissingPredecessor !AlignmentPlanId
  | AlignmentPlanPredecessorConflict !AlignmentPlanId
  | AlignmentPlanResetShapeMismatch
  deriving stock (Eq, Show)

data AlignmentPlanPreparation
  = AlignmentPlanHeld !(Set ContextClassGenerationId)
  | AlignmentPlanReady !AlignmentPlan
  deriving stock (Eq, Show)

-- | Construct the complete binding/relation table. Certificate availability
-- gates only the existing ancestry of created classes; it cannot select carry
-- versus create. Carried generations keep their exact frozen cuts and bases.
-- The caller supplies one checked graph/placement projection for this coordinate.
-- A new sort occurrence starts with Nothing, rather than inheriting old ancestry.
prepareAlignmentPlan ::
  SortOccurrence ->
  TopologyCut ->
  PhysicalPlacementRevisionVector ->
  ContextGraph ->
  [GenerationMemberInput] ->
  Maybe (AlignmentPlan, AlignmentPredecessorStatus) ->
  Set ContextClassGenerationId ->
  Either AlignmentPlanProblem AlignmentPlanPreparation
prepareAlignmentPlan occurrence topology placement context suppliedMembers predecessor =
  prepareAlignmentPlanAtAttempt
    (maybe initialAlignmentPlanAttempt (alignmentPlanIdAttempt . alignmentPlanId . fst) predecessor)
    occurrence
    topology
    placement
    context
    suppliedMembers
    predecessor

-- | Construct an explicitly identified attempt. The ordinary entry point
-- inherits its predecessor's attempt and still rejects an identical coordinate.
prepareAlignmentPlanAtAttempt ::
  AlignmentPlanAttempt ->
  SortOccurrence ->
  TopologyCut ->
  PhysicalPlacementRevisionVector ->
  ContextGraph ->
  [GenerationMemberInput] ->
  Maybe (AlignmentPlan, AlignmentPredecessorStatus) ->
  Set ContextClassGenerationId ->
  Either AlignmentPlanProblem AlignmentPlanPreparation
prepareAlignmentPlanAtAttempt attempt occurrence topology placement context suppliedMembers predecessor =
  prepareAlignmentPlanWithStatus (maybe AlignmentPredecessorUsable snd predecessor) attempt occurrence topology placement context suppliedMembers predecessor

-- | Reconstruct fresh bases after an obsolete attempt. No predecessor claim,
-- generation ancestry or certificate barrier crosses this boundary.
prepareAlignmentResetPlan ::
  AlignmentPlanAttempt ->
  SortOccurrence ->
  TopologyCut ->
  PhysicalPlacementRevisionVector ->
  ContextGraph ->
  [GenerationMemberInput] ->
  Either AlignmentPlanProblem AlignmentPlanPreparation
prepareAlignmentResetPlan attempt occurrence topology placement context suppliedMembers =
  prepareAlignmentPlanWithStatus AlignmentPredecessorReset attempt occurrence topology placement context suppliedMembers Nothing Set.empty

prepareAlignmentPlanWithStatus :: AlignmentPredecessorStatus -> AlignmentPlanAttempt -> SortOccurrence -> TopologyCut -> PhysicalPlacementRevisionVector -> ContextGraph -> [GenerationMemberInput] -> Maybe (AlignmentPlan, AlignmentPredecessorStatus) -> Set ContextClassGenerationId -> Either AlignmentPlanProblem AlignmentPlanPreparation
prepareAlignmentPlanWithStatus predecessorStatus attempt occurrence topology placement context suppliedMembers predecessor certificates = do
  if predecessorStatus == AlignmentPredecessorReset && (attempt == initialAlignmentPlanAttempt || maybe False (const True) predecessor)
    then Left AlignmentPlanResetShapeMismatch
    else Right ()
  validateCoordinate topology placement
  case predecessor of
    Just (prior, _)
      | alignmentPlanIdSort (alignmentPlanId prior) /= occurrence ->
          Left (AlignmentPlanDifferentSortOccurrence (alignmentPlanIdSort (alignmentPlanId prior)) occurrence)
    _ -> Right ()
  -- Draft admission validates the complete active member projection and the
  -- existing ancestry inputs, without deriving any new generation digest.
  drafts <- mapLeft AlignmentPlanGenerationProblem (prepareAlignmentGenerationDrafts occurrence topology placement context suppliedMembers priorGenerations priorRelations)
  let identifier = alignmentPlanIdFromClaimedCoordinatesAtAttempt attempt occurrence retainedTopologyId placement
      retainedTopologyId = case drafts of
        first : _ -> alignmentGenerationDraftTopologyCutId first
        [] -> deriveTopologyCutId topology
  case predecessor of
    Just (prior, _) | alignmentPlanId prior == identifier -> Left (AlignmentPlanSameCoordinate identifier)
    _ -> Right ()
  let initial =
        Map.fromList
          [ (contextClass, generation)
          | carryBoundary,
            draft <- drafts,
            let contextClass = alignmentGenerationDraftClass draft,
            Just binding <- [Map.lookup contextClass priorBindings],
            let generation = alignmentPlanBindingGeneration binding,
            currentMembers contextClass == priorMembers generation
          ]
      carried = closeCarry initial
      createdDrafts = filter ((`Map.notMember` carried) . alignmentGenerationDraftClass) drafts
      required = Set.unions (map alignmentGenerationDraftPredecessors createdDrafts)
      missing = required `Set.difference` certificates
  if Set.null missing
    then do
      bindings <-
        Map.fromList
          <$> traverse
            ( \draft -> do
                let contextClass = alignmentGenerationDraftClass draft
                binding <- case Map.lookup contextClass carried of
                  Just generation -> Right (AlignmentPlanBinding AlignmentCarried generation)
                  Nothing -> AlignmentPlanBinding AlignmentCreated <$> mapLeft AlignmentPlanGenerationProblem (materializeAlignmentGenerationDraftAtAttempt attempt draft)
                Right (contextClass, binding)
            )
            drafts
      let relations =
            Set.fromList
              [ alignmentGenerationRelation
                  (alignmentGenerationId (alignmentPlanBindingGeneration (bindings Map.! contextRelationSource relation)))
                  (alignmentGenerationId (alignmentPlanBindingGeneration (bindings Map.! contextRelationDestination relation)))
                  (contextRelationStrength relation)
              | relation <- contextGraphRelations context
              ]
      let priorClaim = alignmentPlanClaim . fst <$> predecessor
          announcement = buildAlignmentPlanAnnouncement identifier topology (claimIdentifier <$> priorClaim) predecessorStatus (Map.toAscList bindings) (Set.toAscList relations)
      Right (AlignmentPlanReady (AlignmentPlan identifier topology priorClaim predecessorStatus bindings relations announcement))
    else Right (AlignmentPlanHeld missing)
  where
    priorBindings = case predecessor of
      Just (prior, AlignmentPredecessorUsable) -> Map.fromList (alignmentPlanBindings prior)
      _ -> Map.empty
    priorGenerations = map alignmentPlanBindingGeneration (Map.elems priorBindings)
    priorRelations = case predecessor of
      Just (prior, AlignmentPredecessorUsable) -> alignmentPlanRelations prior
      _ -> []
    members = Map.fromList [(generationMemberDelta member, member) | member <- suppliedMembers]
    currentMembers contextClass =
      [ (delta, generationMemberStoreIncarnation member, generationMemberHerald member)
      | delta <- NonEmpty.toList (contextClassMembers contextClass),
        let member = members Map.! delta
      ]
    priorMembers generation =
      [ (alignmentMemberDelta member, alignmentMemberStoreIncarnation member, alignmentMemberHerald member)
      | member <- NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation))
      ]
    carryBoundary = case predecessor of
      Just (prior, AlignmentPredecessorUsable) ->
        let previousPlacement = alignmentPlanIdPlacement (alignmentPlanId prior)
         in physicalPlacementRevisionMembershipGenerationId placement == physicalPlacementRevisionMembershipGenerationId previousPlacement
              && physicalPlacementRevisionMemberSetDigest placement == physicalPlacementRevisionMemberSetDigest previousPlacement
      _ -> False
    incoming =
      Map.fromListWith
        Set.union
        [ (contextRelationDestination relation, Set.singleton (contextRelationSource relation, contextRelationStrength relation))
        | relation <- contextGraphRelations context
        ]
    oldIncoming =
      Map.fromListWith
        Set.union
        [ (alignmentGenerationRelationDestination relation, Set.singleton (alignmentGenerationRelationSource relation, alignmentGenerationRelationStrength relation))
        | relation <- priorRelations
        ]
    -- Greatest dependency-closed subset. Every pass removes candidates only;
    -- canonical maps make the result independent of presentation/arrival order.
    closeCarry candidates =
      let retained = Map.filterWithKey (sameIncoming candidates) candidates
       in if Map.size retained == Map.size candidates then retained else closeCarry retained
    sameIncoming candidates contextClass generation =
      fmap
        Set.fromList
        ( traverse
            (\(source, strength) -> (,strength) . alignmentGenerationId <$> Map.lookup source candidates)
            (Set.toAscList (Map.findWithDefault Set.empty contextClass incoming))
        )
        == Just (Map.findWithDefault Set.empty (alignmentGenerationId generation) oldIncoming)

-- This check must also run when there are no newly created classes: alignmentCut
-- alone cannot validate the coordinate of an empty or entirely carried plan.
validateCoordinate :: TopologyCut -> PhysicalPlacementRevisionVector -> Either AlignmentPlanProblem ()
validateCoordinate topology placement
  | topologyGeneration /= placementGeneration = Left (AlignmentPlanShapeProblem (AlignmentTopologyPlacementGenerationMismatch topologyGeneration placementGeneration))
  | topologyMembers /= placementMembers = Left (AlignmentPlanShapeProblem (AlignmentTopologyPlacementMemberSetMismatch topologyMembers placementMembers))
  | otherwise = Right ()
  where
    frontier = topologyCutFrontier topology
    topologyGeneration = topologyFrontierMembershipGenerationId frontier
    placementGeneration = physicalPlacementRevisionMembershipGenerationId placement
    topologyMembers = topologyFrontierMemberSetDigest frontier
    placementMembers = physicalPlacementRevisionMemberSetDigest placement

-- | An untrusted complete-table claim, for reconstruction/replay properties.
-- This is not a wire format. Consumers first derive the expected checked plan
-- from its authoritative inputs, then compare every binding and relation.
data AlignmentPlanClaim
  = AlignmentPlanClaim
      !AlignmentPlanId
      !(Maybe AlignmentPlanId)
      !AlignmentPredecessorStatus
      ![(ContextClass, AlignmentBindingDisposition, ContextClassGenerationId)]
      ![AlignmentGenerationRelation]
  deriving stock (Eq, Show)

claimIdentifier :: AlignmentPlanClaim -> AlignmentPlanId
claimIdentifier (AlignmentPlanClaim identifier _ _ _ _) = identifier

alignmentPlanClaim :: AlignmentPlan -> AlignmentPlanClaim
alignmentPlanClaim plan =
  AlignmentPlanClaim
    (alignmentPlanId plan)
    (alignmentPlanPredecessor plan)
    (alignmentPlanPredecessorStatus plan)
    [(contextClass, alignmentPlanBindingDisposition binding, alignmentGenerationId (alignmentPlanBindingGeneration binding)) | (contextClass, binding) <- alignmentPlanBindings plan]
    (alignmentPlanRelations plan)

checkAlignmentPlanClaim :: AlignmentPlan -> AlignmentPlanClaim -> Either AlignmentPlanProblem AlignmentPlan
checkAlignmentPlanClaim expected (AlignmentPlanClaim identifier predecessor status bindings relations)
  | identifier == alignmentPlanId expected,
    predecessor == alignmentPlanPredecessor expected,
    status == alignmentPlanPredecessorStatus expected,
    length bindings == Map.size suppliedBindings,
    length relations == Set.size suppliedRelations,
    suppliedBindings == expectedBindings,
    suppliedRelations == Set.fromList (alignmentPlanRelations expected) =
      Right expected
  | otherwise = Left AlignmentPlanClaimMismatch
  where
    suppliedBindings = Map.fromList [(contextClass, (disposition, generation)) | (contextClass, disposition, generation) <- bindings]
    suppliedRelations = Set.fromList relations
    expectedBindings = Map.fromList [(contextClass, (alignmentPlanBindingDisposition binding, alignmentGenerationId (alignmentPlanBindingGeneration binding))) | (contextClass, binding) <- alignmentPlanBindings expected]

-- | Return the checked immutable protocol projection retained at construction.
-- Repeated dissemination must not hash the topology or rebuild complete tables.
alignmentPlanAnnouncement :: AlignmentPlan -> Protocol.AlignmentPlanAnnounce
alignmentPlanAnnouncement (AlignmentPlan _ _ _ _ _ _ announcement) = announcement

buildAlignmentPlanAnnouncement :: AlignmentPlanId -> TopologyCut -> Maybe AlignmentPlanId -> AlignmentPredecessorStatus -> [(ContextClass, AlignmentPlanBinding)] -> [AlignmentGenerationRelation] -> Protocol.AlignmentPlanAnnounce
buildAlignmentPlanAnnouncement identifier topology predecessor status suppliedBindings suppliedRelations = either (error . ("checked alignment plan announcement: " <>) . show) id $ do
  bindings <- traverse (\(contextClass, binding) -> Protocol.alignmentPlanBindingClaim (contextClassMembers contextClass) (alignmentPlanBindingDisposition binding) (alignmentGenerationId (alignmentPlanBindingGeneration binding))) suppliedBindings
  Protocol.alignmentPlanAnnounce identifier topology predecessor status bindings relations created
  where
    relations = [Protocol.alignmentPlanRelationClaim (alignmentGenerationRelationSource relation) (alignmentGenerationRelationDestination relation) (alignmentGenerationRelationStrength relation) | relation <- suppliedRelations]
    created = [alignmentGenerationCutAnnounce (alignmentPlanBindingGeneration binding) | (_, binding) <- suppliedBindings, alignmentPlanBindingDisposition binding == AlignmentCreated]

checkAlignmentPlanAnnouncement :: AlignmentPlan -> Protocol.AlignmentPlanAnnounce -> Either AlignmentPlanProblem AlignmentPlan
checkAlignmentPlanAnnouncement expected supplied
  | alignmentPlanAnnouncement expected == supplied = Right expected
  | otherwise = Left AlignmentPlanClaimMismatch

-- | Immutable plan retention and cause coverage; no active frontier, readiness,
-- obligations or effects live here. Only the first retention owns new plan work.
-- Adding another cause must not replay the table as newly created semantic work.
data AlignmentPlanLedger = AlignmentPlanLedger !(Map AlignmentPlanId AlignmentPlan) !(Set (StructuralConsequenceCause, AlignmentPlanId))
  deriving stock (Eq, Show)

emptyAlignmentPlanLedger :: AlignmentPlanLedger
emptyAlignmentPlanLedger = AlignmentPlanLedger Map.empty Set.empty

alignmentPlanEntries :: AlignmentPlanLedger -> [(AlignmentPlanId, AlignmentPlan)]
alignmentPlanEntries (AlignmentPlanLedger plans _) = Map.toAscList plans

lookupAlignmentPlan :: AlignmentPlanId -> AlignmentPlanLedger -> Maybe AlignmentPlan
lookupAlignmentPlan identifier (AlignmentPlanLedger plans _) = Map.lookup identifier plans

alignmentPlanCoverage :: AlignmentPlanLedger -> [(StructuralConsequenceCause, AlignmentPlanId)]
alignmentPlanCoverage (AlignmentPlanLedger _ coverage) = Set.toAscList coverage

data AlignmentPlanRetention = AlignmentPlanInserted | AlignmentPlanCauseAdded | AlignmentPlanReplay
  deriving stock (Eq, Show)

-- | Passive catalogue admission. Retain immutable plan identity and exact
-- predecessor evidence without inventing cause coverage or any owner work.
retainHistoricalAlignmentPlan :: AlignmentPlan -> AlignmentPlanLedger -> Either AlignmentPlanProblem AlignmentPlanLedger
retainHistoricalAlignmentPlan plan@(AlignmentPlan identifier _ priorClaim _ _ _ _) ledger@(AlignmentPlanLedger plans coverage) = do
  case Map.lookup identifier plans of
    Just retained | retained /= plan -> Left (AlignmentPlanIdentityConflict identifier)
    _ -> Right ()
  case priorClaim of
    Just expected -> case Map.lookup (claimIdentifier expected) plans of
      Nothing -> Left (AlignmentPlanMissingPredecessor (claimIdentifier expected))
      Just retained | alignmentPlanClaim retained /= expected -> Left (AlignmentPlanPredecessorConflict (claimIdentifier expected))
      _ -> Right ()
    Nothing -> Right ()
  if Map.member identifier plans then Right ledger else Right (AlignmentPlanLedger (Map.insert identifier plan plans) coverage)

retainAlignmentPlan ::
  StructuralConsequenceCause ->
  AlignmentPlan ->
  AlignmentPlanLedger ->
  Either AlignmentPlanProblem (AlignmentPlanLedger, AlignmentPlanRetention)
retainAlignmentPlan cause plan ledger@(AlignmentPlanLedger plans coverage) = do
  AlignmentPlanLedger retainedPlans _ <- retainHistoricalAlignmentPlan plan ledger
  let disposition
        | Map.notMember identifier plans = AlignmentPlanInserted
        | Set.notMember (cause, identifier) coverage = AlignmentPlanCauseAdded
        | otherwise = AlignmentPlanReplay
      retained = AlignmentPlanLedger retainedPlans (Set.insert (cause, identifier) coverage)
  Right (retained, disposition)
  where
    identifier = alignmentPlanId plan

mapLeft :: (problem -> mapped) -> Either problem value -> Either mapped value
mapLeft mapping = either (Left . mapping) Right
