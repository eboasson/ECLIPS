{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Deterministic construction of per-sort context-class generations.
--
-- The caller supplies exact projections for one installed topology cut and one
-- complete physical-placement cut.  Message arrival, provider selection, and
-- certificates cannot alter ancestry: certificates only decide whether the
-- already-determined plan is ready to be committed.
module Eclips.Herald.Alignment.Generation
  ( GenerationMemberInput,
    generationMemberInput,
    generationMemberDelta,
    generationMemberStoreIncarnation,
    generationMemberHerald,
    generationMemberBaseRevision,
    AlignmentGeneration,
    alignmentGenerationId,
    alignmentGenerationCut,
    alignmentGenerationTopologyCutId,
    alignmentGenerationCutAnnounce,
    alignmentGenerationContextClass,
    alignmentGenerationAnnouncer,
    AlignmentGenerationRelation,
    alignmentGenerationRelation,
    alignmentGenerationRelationSource,
    alignmentGenerationRelationDestination,
    alignmentGenerationRelationStrength,
    AlignmentGenerationDraft,
    prepareAlignmentGenerationDrafts,
    alignmentGenerationDraftClass,
    alignmentGenerationDraftTopologyCutId,
    alignmentGenerationDraftPredecessors,
    materializeAlignmentGenerationDraft,
    materializeAlignmentGenerationDraftAtAttempt,
    AlignmentGenerationPlan,
    alignmentGenerationPlanGenerations,
    alignmentGenerationPlanRelations,
    AlignmentGenerationProblem (..),
    AlignmentGenerationPreparation (..),
    prepareAlignmentGenerationPlan,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Alignment
  ( AlignmentCut,
    AlignmentMember,
    AlignmentPlanAttempt,
    AlignmentShapeError,
    ContextClassGenerationId,
    FreshMemberBaseEvidence,
    PhysicalPlacementRevisionVector,
    StoreRevision,
    alignmentCutAtAttempt,
    alignmentCutExactMembers,
    alignmentMember,
    alignmentMemberDelta,
    alignmentMemberHerald,
    freshMemberBaseEvidence,
    initialAlignmentPlanAttempt,
  )
import Eclips.Domain.Context
  ( ContextClass,
    ContextGraph,
    contextClassMembers,
    contextGraphClasses,
    contextGraphRelations,
    contextRelationDestination,
    contextRelationSource,
    contextRelationStrength,
  )
import Eclips.Domain.Identity
  ( DeltaId,
    HeraldEpoch,
    StoreIncarnationId,
    TopologyCutId,
  )
import Eclips.Domain.Route (ReplicaStrength)
import Eclips.Domain.Topology (TopologyCut, deriveTopologyCutId)
import Eclips.Herald.Alignment.Protocol
  ( AlignmentCutAnnounce,
    alignmentCutAnnounce,
    alignmentCutAnnounceCut,
    alignmentCutAnnounceGeneration,
  )
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    sortOccurrenceDefinition,
    sortOccurrenceSortId,
  )

-- | Exact active member and captured fresh-store base at the selected physical
-- placement cut.
data GenerationMemberInput = GenerationMemberInput
  { delta :: DeltaId,
    storeIncarnation :: StoreIncarnationId,
    herald :: HeraldEpoch,
    baseRevision :: StoreRevision
  }
  deriving stock (Eq, Ord, Show)

generationMemberInput ::
  DeltaId ->
  StoreIncarnationId ->
  HeraldEpoch ->
  StoreRevision ->
  GenerationMemberInput
generationMemberInput = GenerationMemberInput

generationMemberDelta :: GenerationMemberInput -> DeltaId
generationMemberDelta input = input.delta

generationMemberStoreIncarnation ::
  GenerationMemberInput -> StoreIncarnationId
generationMemberStoreIncarnation input = input.storeIncarnation

generationMemberHerald :: GenerationMemberInput -> HeraldEpoch
generationMemberHerald input = input.herald

generationMemberBaseRevision :: GenerationMemberInput -> StoreRevision
generationMemberBaseRevision input = input.baseRevision

-- | One immutable class generation.  The context class is retained beside its
-- canonical cut so later transitions can select ancestry without reverse
-- engineering the member list.
data AlignmentGeneration = AlignmentGeneration
  { cutAnnounce :: AlignmentCutAnnounce,
    topologyCutId :: !TopologyCutId,
    contextClass :: ContextClass,
    announcer :: HeraldEpoch
  }
  deriving stock (Eq, Show)

alignmentGenerationId :: AlignmentGeneration -> ContextClassGenerationId
alignmentGenerationId generation =
  alignmentCutAnnounceGeneration generation.cutAnnounce

alignmentGenerationCut :: AlignmentGeneration -> AlignmentCut
alignmentGenerationCut generation =
  alignmentCutAnnounceCut generation.cutAnnounce

-- | The canonical topology digest retained when the immutable generation was
-- checked. Readiness and plan lookups reuse it without hashing the cut again.
alignmentGenerationTopologyCutId :: AlignmentGeneration -> TopologyCutId
alignmentGenerationTopologyCutId generation = generation.topologyCutId

-- | Reuse the checked generation/cut pair admitted when this opaque generation
-- was constructed.  In particular, retry and reconnect projection must not
-- re-derive the generation digest from an already-retained cut.
alignmentGenerationCutAnnounce :: AlignmentGeneration -> AlignmentCutAnnounce
alignmentGenerationCutAnnounce generation = generation.cutAnnounce

alignmentGenerationContextClass :: AlignmentGeneration -> ContextClass
alignmentGenerationContextClass generation = generation.contextClass

alignmentGenerationAnnouncer :: AlignmentGeneration -> HeraldEpoch
alignmentGenerationAnnouncer generation = generation.announcer

-- | One frozen source-to-destination class relation at the same alignment cut.
data AlignmentGenerationRelation = AlignmentGenerationRelation
  { source :: ContextClassGenerationId,
    destination :: ContextClassGenerationId,
    strength :: ReplicaStrength
  }
  deriving stock (Eq, Ord, Show)

alignmentGenerationRelation ::
  ContextClassGenerationId ->
  ContextClassGenerationId ->
  ReplicaStrength ->
  AlignmentGenerationRelation
alignmentGenerationRelation = AlignmentGenerationRelation

alignmentGenerationRelationSource ::
  AlignmentGenerationRelation -> ContextClassGenerationId
alignmentGenerationRelationSource relation = relation.source

alignmentGenerationRelationDestination ::
  AlignmentGenerationRelation -> ContextClassGenerationId
alignmentGenerationRelationDestination relation = relation.destination

alignmentGenerationRelationStrength ::
  AlignmentGenerationRelation -> ReplicaStrength
alignmentGenerationRelationStrength relation = relation.strength

data AlignmentGenerationPlan = AlignmentGenerationPlan
  { generations :: [AlignmentGeneration],
    relations :: [AlignmentGenerationRelation]
  }
  deriving stock (Eq, Show)

alignmentGenerationPlanGenerations ::
  AlignmentGenerationPlan -> [AlignmentGeneration]
alignmentGenerationPlanGenerations plan = plan.generations

alignmentGenerationPlanRelations ::
  AlignmentGenerationPlan -> [AlignmentGenerationRelation]
alignmentGenerationPlanRelations plan = plan.relations

data AlignmentGenerationProblem
  = AlignmentGenerationMemberMissing DeltaId
  | AlignmentGenerationUnexpectedMember DeltaId
  | AlignmentGenerationDuplicateMember DeltaId
  | AlignmentGenerationPriorIdConflict ContextClassGenerationId
  | AlignmentGenerationPriorRelationUnknownSource ContextClassGenerationId
  | AlignmentGenerationPriorRelationUnknownDestination ContextClassGenerationId
  | AlignmentGenerationShapeProblem AlignmentShapeError
  deriving stock (Eq, Show)

data AlignmentGenerationPreparation
  = AlignmentGenerationHeld (Set ContextClassGenerationId)
  | AlignmentGenerationReady AlignmentGenerationPlan
  deriving stock (Eq, Show)

-- | Construct all class generations and relations for one exact cut.
--
-- The supplied prior generations are the prior latest generation set for this
-- sort occurrence.  Same-class ancestry is selected by intersecting member
-- delta IDs.  Prior source generations are additionally selected whenever
-- their old destination intersects the new destination class; this is the
-- deterministic replaced-relation rule.  A new member gets a fresh base iff no
-- selected predecessor represents its delta.
prepareAlignmentGenerationPlan ::
  SortOccurrence ->
  TopologyCut ->
  PhysicalPlacementRevisionVector ->
  ContextGraph ->
  [GenerationMemberInput] ->
  [AlignmentGeneration] ->
  [AlignmentGenerationRelation] ->
  Set ContextClassGenerationId ->
  Either AlignmentGenerationProblem AlignmentGenerationPreparation
prepareAlignmentGenerationPlan sortOccurrence topologyCut placement context suppliedMembers prior suppliedPriorRelations availableCertificates = do
  prepared <-
    prepareAlignmentGenerationDrafts
      sortOccurrence
      topologyCut
      placement
      context
      suppliedMembers
      prior
      suppliedPriorRelations
  let selectedPredecessors =
        Set.unions (fmap alignmentGenerationDraftPredecessors prepared)
      missingCertificates =
        selectedPredecessors `Set.difference` availableCertificates
  if Set.null missingCertificates
    then do
      generations <- traverse materializeAlignmentGenerationDraft prepared
      let generationByClass =
            Map.fromList
              [ (generation.contextClass, alignmentGenerationId generation)
              | generation <- generations
              ]
          relations =
            Set.toAscList
              ( Set.fromList
                  (fmap (buildRelation generationByClass) (contextGraphRelations context))
              )
      Right
        ( AlignmentGenerationReady
            AlignmentGenerationPlan
              { generations,
                relations
              }
        )
    else Right (AlignmentGenerationHeld missingCertificates)
  where
    buildRelation generationByClass relation =
      AlignmentGenerationRelation
        { source = generationByClass Map.! contextRelationSource relation,
          destination = generationByClass Map.! contextRelationDestination relation,
          strength = contextRelationStrength relation
        }

-- | An exact prospective birth cut, before its checked cut or generation digest
-- is constructed. The topology digest is a shared lazy cache, so discarding a
-- draft or waiting for predecessor certificates does not compute it either.
-- The final ancestry derivation is deliberately lazy: inspecting the class to
-- decide whether to carry its prior generation must not select discarded
-- predecessors or fresh bases.
data AlignmentGenerationDraft
  = AlignmentGenerationDraft
      !SortOccurrence
      !TopologyCut
      !PhysicalPlacementRevisionVector
      TopologyCutId
      !ContextClass
      PreparedClass
  deriving stock (Eq, Show)

alignmentGenerationDraftClass :: AlignmentGenerationDraft -> ContextClass
alignmentGenerationDraftClass (AlignmentGenerationDraft _ _ _ _ contextClass _) =
  contextClass

alignmentGenerationDraftTopologyCutId :: AlignmentGenerationDraft -> TopologyCutId
alignmentGenerationDraftTopologyCutId (AlignmentGenerationDraft _ _ _ identifier _ _) =
  identifier

alignmentGenerationDraftPredecessors ::
  AlignmentGenerationDraft -> Set ContextClassGenerationId
alignmentGenerationDraftPredecessors (AlignmentGenerationDraft _ _ _ _ _ prepared) =
  prepared.predecessors

-- | Select ancestry and fresh bases independently of certificates and generation
-- construction. A caller may discard an unchanged class's draft and retain its
-- existing generation without hashing a replacement birth cut.
prepareAlignmentGenerationDrafts ::
  SortOccurrence ->
  TopologyCut ->
  PhysicalPlacementRevisionVector ->
  ContextGraph ->
  [GenerationMemberInput] ->
  [AlignmentGeneration] ->
  [AlignmentGenerationRelation] ->
  Either AlignmentGenerationProblem [AlignmentGenerationDraft]
prepareAlignmentGenerationDrafts sortOccurrence topologyCut placement context suppliedMembers prior suppliedPriorRelations = do
  members <- uniqueMembers suppliedMembers
  requireExactContextMembers context members
  priorById <- uniquePrior prior
  mapM_ (validatePriorRelation priorById) suppliedPriorRelations
  Right
    [ AlignmentGenerationDraft
        sortOccurrence
        topologyCut
        placement
        retainedTopologyCutId
        contextClass
        (prepareClass members priorById suppliedPriorRelations contextClass)
    | contextClass <- contextGraphClasses context
    ]
  where
    retainedTopologyCutId = deriveTopologyCutId topologyCut

    prepareClass members priorById priorRelations contextClass =
      let deltas = Set.fromList (NonEmpty.toList (contextClassMembers contextClass))
          direct =
            Set.fromList
              [ alignmentGenerationId generation
              | generation <- Map.elems priorById,
                not
                  ( Set.disjoint
                      deltas
                      (generationMemberDeltas generation)
                  )
              ]
          replacedSources =
            Set.fromList
              [ relation.source
              | relation <- priorRelations,
                Just destination <- [Map.lookup relation.destination priorById],
                not
                  ( Set.disjoint
                      deltas
                      (generationMemberDeltas destination)
                  )
              ]
          predecessors = Set.union direct replacedSources
          represented =
            Set.unions
              [ generationMemberDeltas generation
              | predecessor <- Set.toAscList predecessors,
                Just generation <- [Map.lookup predecessor priorById]
              ]
          classMembers = fmap (members Map.!) (Set.toAscList deltas)
          fresh = filter ((`Set.notMember` represented) . (.delta)) classMembers
       in PreparedClass contextClass classMembers predecessors fresh

materializeAlignmentGenerationDraft ::
  AlignmentGenerationDraft ->
  Either AlignmentGenerationProblem AlignmentGeneration
materializeAlignmentGenerationDraft = materializeAlignmentGenerationDraftAtAttempt initialAlignmentPlanAttempt

materializeAlignmentGenerationDraftAtAttempt ::
  AlignmentPlanAttempt ->
  AlignmentGenerationDraft ->
  Either AlignmentGenerationProblem AlignmentGeneration
materializeAlignmentGenerationDraftAtAttempt attempt (AlignmentGenerationDraft sortOccurrence topologyCut placement retainedTopologyCutId _ prepared) = do
  cut <-
    mapLeft AlignmentGenerationShapeProblem
      $ alignmentCutAtAttempt
        attempt
        (sortOccurrenceSortId sortOccurrence)
        (sortOccurrenceDefinition sortOccurrence)
        topologyCut
        placement
        (toAlignmentMembers prepared.members)
        (Set.toAscList prepared.predecessors)
        (fmap toFreshBase prepared.freshMembers)
  let cutAnnounce = alignmentCutAnnounce cut
      host =
        alignmentMemberHerald
          (minimum (NonEmpty.toList (alignmentCutExactMembers cut)))
  Right
    AlignmentGeneration
      { cutAnnounce,
        topologyCutId = retainedTopologyCutId,
        contextClass = prepared.contextClass,
        announcer = host
      }

data PreparedClass = PreparedClass
  { contextClass :: !ContextClass,
    members :: ![GenerationMemberInput],
    predecessors :: !(Set ContextClassGenerationId),
    freshMembers :: ![GenerationMemberInput]
  }
  deriving stock (Eq, Show)

uniqueMembers ::
  [GenerationMemberInput] ->
  Either AlignmentGenerationProblem (Map DeltaId GenerationMemberInput)
uniqueMembers = foldl insertMember (Right Map.empty)
  where
    insertMember accumulated member = do
      current <- accumulated
      if Map.member member.delta current
        then Left (AlignmentGenerationDuplicateMember member.delta)
        else Right (Map.insert member.delta member current)

requireExactContextMembers ::
  ContextGraph ->
  Map DeltaId GenerationMemberInput ->
  Either AlignmentGenerationProblem ()
requireExactContextMembers context members = do
  let expected =
        Set.fromList
          [ delta
          | contextClass <- contextGraphClasses context,
            delta <- NonEmpty.toList (contextClassMembers contextClass)
          ]
      actual = Map.keysSet members
  case Set.lookupMin (expected `Set.difference` actual) of
    Just missing -> Left (AlignmentGenerationMemberMissing missing)
    Nothing -> case Set.lookupMin (actual `Set.difference` expected) of
      Just unexpected -> Left (AlignmentGenerationUnexpectedMember unexpected)
      Nothing -> Right ()

uniquePrior ::
  [AlignmentGeneration] ->
  Either
    AlignmentGenerationProblem
    (Map ContextClassGenerationId AlignmentGeneration)
uniquePrior = foldl insertGeneration (Right Map.empty)
  where
    insertGeneration accumulated generation = do
      current <- accumulated
      let identifier = alignmentGenerationId generation
       in if Map.member identifier current
            then Left (AlignmentGenerationPriorIdConflict identifier)
            else Right (Map.insert identifier generation current)

validatePriorRelation ::
  Map ContextClassGenerationId AlignmentGeneration ->
  AlignmentGenerationRelation ->
  Either AlignmentGenerationProblem ()
validatePriorRelation generations relation
  | Map.notMember relation.source generations =
      Left (AlignmentGenerationPriorRelationUnknownSource relation.source)
  | Map.notMember relation.destination generations =
      Left
        (AlignmentGenerationPriorRelationUnknownDestination relation.destination)
  | otherwise = Right ()

generationMemberDeltas :: AlignmentGeneration -> Set DeltaId
generationMemberDeltas =
  Set.fromList
    . fmap alignmentMemberDelta
    . NonEmpty.toList
    . alignmentCutExactMembers
    . alignmentGenerationCut

toAlignmentMembers :: NonEmptyList GenerationMemberInput -> NonEmpty AlignmentMember
toAlignmentMembers members =
  case fmap toAlignmentMember members of
    first : remaining -> first :| remaining
    [] -> error "a context class cannot have no members"

type NonEmptyList value = [value]

toAlignmentMember :: GenerationMemberInput -> AlignmentMember
toAlignmentMember member =
  alignmentMember member.delta member.storeIncarnation member.herald

toFreshBase :: GenerationMemberInput -> FreshMemberBaseEvidence
toFreshBase member =
  freshMemberBaseEvidence member.delta member.storeIncarnation member.baseRevision

mapLeft :: (left -> mapped) -> Either left value -> Either mapped value
mapLeft mapping = either (Left . mapping) Right
