{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Finite, occurrence-qualified consequences of structural reconciliation.
--
-- A debt deliberately names only facts which exist at the local structural
-- application boundary.  In particular it cannot name a topology cut, an
-- alignment cut, a context generation, a certificate, or an obligation.  A
-- later owner may refine one of these debts after the required common topology
-- and placement evidence exists.
module Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    sortOccurrence,
    sortOccurrenceSortId,
    sortOccurrenceDefinition,
    StructuralIdentity (..),
    QualifiedStoreIncarnation,
    qualifiedStoreIncarnation,
    qualifiedStoreHerald,
    qualifiedStoreDelta,
    qualifiedStoreSort,
    qualifiedStoreIncarnationId,
    QualifiedPlacementFact,
    qualifiedPlacementFact,
    qualifiedPlacementHerald,
    qualifiedPlacementDelta,
    qualifiedPlacementSort,
    qualifiedPlacementIncarnation,
    StructuralDebtKind (..),
    StructuralDebtKey,
    structuralDebtKey,
    structuralDebtKeyCause,
    structuralDebtKeyKind,
    structuralDebtKeySort,
    structuralDebtKeyDestination,
    StructuralDebtEvidence,
    structuralDebtEvidence,
    structuralDebtAffectedIdentities,
    structuralDebtKnownPlacements,
    structuralDebtPredecessorResources,
    StructuralConsequenceDebt,
    structuralConsequenceDebt,
    structuralConsequenceDebtKey,
    structuralConsequenceDebtEvidence,
    StructuralDebtSet,
    emptyStructuralDebtSet,
    normalizeStructuralDebts,
    structuralDebtSetEntries,
    structuralDebtSetNull,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Graph (VertexId)
import Eclips.Domain.Identity
  ( DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
  )
import Eclips.Domain.StructuralConsequence (StructuralConsequenceCause)

-- | One currently effective interpretation of a content-addressed sort.
data SortOccurrence = SortOccurrence
  { sortId :: SortId,
    definition :: SortDefinitionOccurrenceId
  }
  deriving stock (Eq, Ord, Show)

sortOccurrence :: SortId -> SortDefinitionOccurrenceId -> SortOccurrence
sortOccurrence = SortOccurrence

sortOccurrenceSortId :: SortOccurrence -> SortId
sortOccurrenceSortId occurrence = occurrence.sortId

sortOccurrenceDefinition :: SortOccurrence -> SortDefinitionOccurrenceId
sortOccurrenceDefinition occurrence = occurrence.definition

-- | The structural subject whose applied projection caused a consequence.
-- Edge identity is the controlled edge-object identity, not its endpoint
-- payload.  That distinction is required when two objects induce equal edges.
data StructuralIdentity
  = StructuralVertexIdentity VertexId
  | StructuralEdgeIdentity GlobalObjectId
  deriving stock (Eq, Ord, Show)

-- | An exact, already-known physical store.  This is not a promise that a
-- future store with these logical delta bits will have the same incarnation.
data QualifiedStoreIncarnation = QualifiedStoreIncarnation
  { herald :: HeraldEpoch,
    delta :: DeltaId,
    sort :: SortOccurrence,
    incarnation :: StoreIncarnationId
  }
  deriving stock (Eq, Ord, Show)

qualifiedStoreIncarnation ::
  HeraldEpoch ->
  DeltaId ->
  SortOccurrence ->
  StoreIncarnationId ->
  QualifiedStoreIncarnation
qualifiedStoreIncarnation = QualifiedStoreIncarnation

qualifiedStoreHerald :: QualifiedStoreIncarnation -> HeraldEpoch
qualifiedStoreHerald store = store.herald

qualifiedStoreDelta :: QualifiedStoreIncarnation -> DeltaId
qualifiedStoreDelta store = store.delta

qualifiedStoreSort :: QualifiedStoreIncarnation -> SortOccurrence
qualifiedStoreSort store = store.sort

qualifiedStoreIncarnationId :: QualifiedStoreIncarnation -> StoreIncarnationId
qualifiedStoreIncarnationId store = store.incarnation

-- | A placement fact known while preparing the structural occurrence.  A
-- placement revision is intentionally absent: the matching placement cut is a
-- later input to debt promotion, not something reconciliation may invent.
data QualifiedPlacementFact = QualifiedPlacementFact
  { herald :: HeraldEpoch,
    delta :: DeltaId,
    sort :: SortOccurrence,
    incarnation :: StoreIncarnationId
  }
  deriving stock (Eq, Ord, Show)

qualifiedPlacementFact ::
  HeraldEpoch ->
  DeltaId ->
  SortOccurrence ->
  StoreIncarnationId ->
  QualifiedPlacementFact
qualifiedPlacementFact = QualifiedPlacementFact

qualifiedPlacementHerald :: QualifiedPlacementFact -> HeraldEpoch
qualifiedPlacementHerald placement = placement.herald

qualifiedPlacementDelta :: QualifiedPlacementFact -> DeltaId
qualifiedPlacementDelta placement = placement.delta

qualifiedPlacementSort :: QualifiedPlacementFact -> SortOccurrence
qualifiedPlacementSort placement = placement.sort

qualifiedPlacementIncarnation :: QualifiedPlacementFact -> StoreIncarnationId
qualifiedPlacementIncarnation placement = placement.incarnation

-- | Finite work represented at local structural completion.  Fulfilment of
-- any of these facts is explicitly outside that completion boundary.
data StructuralDebtKind
  = TopologyAlignmentDebt
  | CutoverDebt
  | DestinationAlignmentDebt
  | PlacementActivationDebt
  | StoreActivationDebt
  | StoreReplacementDebt
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Canonical logical debt identity. Equal derivations for one cause coalesce;
-- equal consequences of two causes remain distinct.
data StructuralDebtKey = StructuralDebtKey
  { cause :: StructuralConsequenceCause,
    kind :: StructuralDebtKind,
    sort :: SortOccurrence,
    destination :: Maybe QualifiedStoreIncarnation
  }
  deriving stock (Eq, Ord, Show)

structuralDebtKey ::
  StructuralConsequenceCause ->
  StructuralDebtKind ->
  SortOccurrence ->
  Maybe QualifiedStoreIncarnation ->
  StructuralDebtKey
structuralDebtKey = StructuralDebtKey

structuralDebtKeyCause :: StructuralDebtKey -> StructuralConsequenceCause
structuralDebtKeyCause key = key.cause

structuralDebtKeyKind :: StructuralDebtKey -> StructuralDebtKind
structuralDebtKeyKind key = key.kind

structuralDebtKeySort :: StructuralDebtKey -> SortOccurrence
structuralDebtKeySort key = key.sort

structuralDebtKeyDestination ::
  StructuralDebtKey -> Maybe QualifiedStoreIncarnation
structuralDebtKeyDestination key = key.destination

-- | Already-known inputs which later promotion must preserve.  Set union is
-- the lawful merge for repeated derivations of the same logical debt.
data StructuralDebtEvidence = StructuralDebtEvidence
  { affectedIdentities :: Set StructuralIdentity,
    knownPlacements :: Set QualifiedPlacementFact,
    predecessorResources :: Set QualifiedStoreIncarnation
  }
  deriving stock (Eq, Show)

structuralDebtEvidence ::
  Set StructuralIdentity ->
  Set QualifiedPlacementFact ->
  Set QualifiedStoreIncarnation ->
  StructuralDebtEvidence
structuralDebtEvidence = StructuralDebtEvidence

structuralDebtAffectedIdentities ::
  StructuralDebtEvidence -> Set StructuralIdentity
structuralDebtAffectedIdentities evidence = evidence.affectedIdentities

structuralDebtKnownPlacements ::
  StructuralDebtEvidence -> Set QualifiedPlacementFact
structuralDebtKnownPlacements evidence = evidence.knownPlacements

structuralDebtPredecessorResources ::
  StructuralDebtEvidence -> Set QualifiedStoreIncarnation
structuralDebtPredecessorResources evidence = evidence.predecessorResources

data StructuralConsequenceDebt = StructuralConsequenceDebt
  { key :: StructuralDebtKey,
    evidence :: StructuralDebtEvidence
  }
  deriving stock (Eq, Show)

structuralConsequenceDebt ::
  StructuralDebtKey ->
  StructuralDebtEvidence ->
  StructuralConsequenceDebt
structuralConsequenceDebt = StructuralConsequenceDebt

structuralConsequenceDebtKey :: StructuralConsequenceDebt -> StructuralDebtKey
structuralConsequenceDebtKey debt = debt.key

structuralConsequenceDebtEvidence ::
  StructuralConsequenceDebt -> StructuralDebtEvidence
structuralConsequenceDebtEvidence debt = debt.evidence

newtype StructuralDebtSet
  = StructuralDebtSet
      (Map StructuralDebtKey StructuralDebtEvidence)
  deriving stock (Eq, Show)

emptyStructuralDebtSet :: StructuralDebtSet
emptyStructuralDebtSet = StructuralDebtSet Map.empty

-- | Normalize presentation and coalesce repeated derivations without erasing
-- occurrence identity.
normalizeStructuralDebts :: [StructuralConsequenceDebt] -> StructuralDebtSet
normalizeStructuralDebts debts =
  StructuralDebtSet
    ( Map.fromListWith
        mergeEvidence
        [ (debt.key, debt.evidence)
        | debt <- debts
        ]
    )

structuralDebtSetEntries :: StructuralDebtSet -> [StructuralConsequenceDebt]
structuralDebtSetEntries (StructuralDebtSet debts) =
  [ StructuralConsequenceDebt key evidence
  | (key, evidence) <- Map.toAscList debts
  ]

structuralDebtSetNull :: StructuralDebtSet -> Bool
structuralDebtSetNull (StructuralDebtSet debts) = Map.null debts

mergeEvidence :: StructuralDebtEvidence -> StructuralDebtEvidence -> StructuralDebtEvidence
mergeEvidence left right =
  StructuralDebtEvidence
    { affectedIdentities = Set.union left.affectedIdentities right.affectedIdentities,
      knownPlacements = Set.union left.knownPlacements right.knownPlacements,
      predecessorResources =
        Set.union left.predecessorResources right.predecessorResources
    }
