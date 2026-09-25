{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Live Slice-4 owner for declarative label patches and their
-- complete role-qualified hold categories.
--
-- Local preparation retains canonical decision facts and explicit target holds.
-- Installation constructs its transient recipe from the current owner cut and
-- allocates any Store identities atomically with installation. Neither pending
-- nor historical proof retains an executable recipe or Store allocation map.
module Eclips.Herald.UseCase.LabelPatch
  ( State,
    initialState,
    CheckedLabelPatchBase,
    captureCheckedLabelPatchBase,
    labelPatchBaseFromProjection,
    installCheckedLabelPatchBase,
    checkedLabelPatchBaseEntries,
    ImportedLabelPatch,
    importedLabelPatchDecision,
    importedLabelPatchObject,
    importedLabelPatchPreparedFacts,
    importedLabelPatchPreparedDigest,
    importedLabelPatchReleaseIndex,
    importedLabelPatches,
    lookupImportedLabelPatch,
    importedLabelBaseControlIndex,
    LabelStructuralWork (..),
    noLabelStructuralWork,
    structuralLabelPreparationWork,
    recordLabelStructuralWork,
    labelStructuralWorkCounts,
    TargetRole,
    absentOrdinaryTarget,
    ordinaryControlledTarget,
    StructuralTargetProvenance,
    genesisStructuralTarget,
    dynamicStructuralTarget,
    StructuralTargetProvenanceView (..),
    structuralTargetProvenanceView,
    neutralTarget,
    genesisNeutralTarget,
    nablaTarget,
    genesisNablaTarget,
    deltaTarget,
    genesisDeltaTarget,
    edgeTarget,
    genesisEdgeTarget,
    TargetRoleView (..),
    targetRoleView,
    StoreActivation,
    storeActivation,
    storeActivationDelta,
    storeActivationSort,
    TargetWorkKey (..),
    CheckedEffectiveSortCut,
    effectiveSortPatchCut,
    CheckedAffectedSortCut,
    checkedAffectedSortCut,
    CheckedDeltaSortCut,
    checkedDeltaSortCut,
    CheckedLocalProcessCut,
    checkedLocalProcessCut,
    PatchContextProblem (..),
    PatchContext,
    absentOrdinaryPatchContext,
    ordinaryControlledPatchContext,
    neutralPatchContext,
    nablaPatchContext,
    DeltaSortDisposition (..),
    deltaPatchContext,
    edgePatchContext,
    checkedPatchSpecification,
    PatchSpecification,
    patchSpecification,
    patchSpecificationPreparedFacts,
    patchSpecificationDecision,
    patchSpecificationObject,
    patchSpecificationRole,
    patchSpecificationExpectedPriorState,
    patchSpecificationExpectedPriorRevision,
    patchSpecificationProposedState,
    patchSpecificationAuthorityDisposition,
    patchSpecificationPreparedDigest,
    patchSpecificationAffectedSorts,
    patchSpecificationStoreActivations,
    patchSpecificationHeldWork,
    PatchForm (..),
    LabelPatchRecipe,
    labelPatchRecipeDecision,
    labelPatchRecipePreparedFacts,
    labelPatchRecipeObject,
    labelPatchRecipeRole,
    labelPatchRecipeForm,
    labelPatchRecipeExpectedPriorState,
    labelPatchRecipeExpectedPriorRevision,
    labelPatchRecipeProposedState,
    labelPatchRecipeAuthorityDisposition,
    labelPatchRecipePreparedDigest,
    labelPatchRecipeAffectedSorts,
    labelPatchRecipePreallocatedStores,
    labelPatchRecipeHeldWork,
    PatchPhaseView (..),
    InstalledLabel,
    installedLabelDecision,
    installedLabelObject,
    installedLabelPreparedFacts,
    installedLabelPreparedDigest,
    installedLabelRole,
    installedLabelPreparationRole,
    installedLabelReleaseIndex,
    installedLabelMatchesRecipe,
    PendingLabelPreparation,
    pendingLabelPreparationDecision,
    pendingLabelPreparationObject,
    pendingLabelPreparationRole,
    pendingLabelPreparationHeldWork,
    RetainedPatchView,
    retainedPatchViewObject,
    retainedPatchViewPreparedFacts,
    retainedPatchViewPreparedDigest,
    retainedPatchViewInstallation,
    retainedPatchViewPreparation,
    retainedPatchViewPhase,
    retainedLabelPatches,
    lookupRetainedLabelPatch,
    heldTargetWork,
    targetWorkIsHeld,
    PatchPreparationDisposition (..),
    PatchProblem (..),
    PreparedLabelPatch,
    prepareLabelPatch,
    preparedLabelPatchPreparation,
    preparedLabelPatchDisposition,
    commitLabelPatch,
    PreparedPatchRelease,
    preparePatchRelease,
    preparedReleasedPatchRecipe,
    preparedReleasedPatchInstallation,
    commitPatchRelease,
  )
where

import Control.Monad (foldM, unless, when)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    NablaId,
    ProcessEpochId,
    PublicationId,
    StoreIncarnationId,
    StructuralOccurrenceId,
    globalObjectIdFromDeltaId,
    globalObjectIdFromNablaId,
    globalObjectIdFromProcessEpochId,
    globalUniqueIdBytes,
    mkStoreIncarnationId,
  )
import Eclips.Domain.Label
  ( LabelRevision,
    PreparedAuthorityDisposition,
    PreparedLabelDigest,
    PreparedLabelFacts,
    ReleasedLabelState,
    ReleasedLabelStateView (..),
    derivePreparedLabelDigest,
    mkLabelRevision,
    preparedLabelFactsAuthorityDisposition,
    preparedLabelFactsDecisionId,
    preparedLabelFactsExpectedPriorRevision,
    preparedLabelFactsExpectedPriorState,
    preparedLabelFactsObjectId,
    preparedLabelFactsProposedOutcome,
    preparedLabelFactsResolveIndex,
    releasedLabelStateView,
  )
import Eclips.Domain.Sort.Descriptor (StructuralCarrierRole (..))
import Eclips.Domain.Value (LabelOwner (..))
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.IdGenerator.State qualified as IdGenerator
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    sortOccurrence,
    sortOccurrenceDefinition,
    sortOccurrenceSortId,
    structuralDebtSetEntries,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation

-- | Provenance of the structural carrier selected from the canonical target
-- catalogue.  Genesis material deliberately has no positive structural
-- occurrence; keeping that as a real sum arm prevents label preparation from
-- fabricating one merely to fit an occurrence-shaped hold key.
data StructuralTargetProvenance
  = GenesisStructuralTarget
  | DynamicStructuralTarget StructuralOccurrenceId
  deriving stock (Eq, Ord, Show)

data StructuralTargetProvenanceView
  = GenesisStructuralTargetView
  | DynamicStructuralTargetView StructuralOccurrenceId
  deriving stock (Eq, Ord, Show)

genesisStructuralTarget :: StructuralTargetProvenance
genesisStructuralTarget = GenesisStructuralTarget

dynamicStructuralTarget ::
  StructuralOccurrenceId -> StructuralTargetProvenance
dynamicStructuralTarget = DynamicStructuralTarget

structuralTargetProvenanceView ::
  StructuralTargetProvenance -> StructuralTargetProvenanceView
structuralTargetProvenanceView provenance = case provenance of
  GenesisStructuralTarget -> GenesisStructuralTargetView
  DynamicStructuralTarget occurrence -> DynamicStructuralTargetView occurrence

-- | The locally known semantic role of the target.  Structural roles retain
-- their real genesis-or-dynamic provenance separately from object identity; an
-- absent target is representable only for an ordinary object.
data TargetRole
  = AbsentOrdinaryTarget
  | OrdinaryControlledTarget
  | NeutralTarget StructuralTargetProvenance
  | NablaTarget NablaId StructuralTargetProvenance
  | DeltaTarget DeltaId StructuralTargetProvenance
  | EdgeTarget StructuralTargetProvenance
  deriving stock (Eq, Ord, Show)

data TargetRoleView
  = AbsentOrdinaryTargetView
  | OrdinaryControlledTargetView
  | NeutralTargetView StructuralTargetProvenanceView
  | NablaTargetView NablaId StructuralTargetProvenanceView
  | DeltaTargetView DeltaId StructuralTargetProvenanceView
  | EdgeTargetView StructuralTargetProvenanceView
  deriving stock (Eq, Ord, Show)

absentOrdinaryTarget :: TargetRole
absentOrdinaryTarget = AbsentOrdinaryTarget

ordinaryControlledTarget :: TargetRole
ordinaryControlledTarget = OrdinaryControlledTarget

neutralTarget :: StructuralOccurrenceId -> TargetRole
neutralTarget = NeutralTarget . DynamicStructuralTarget

genesisNeutralTarget :: TargetRole
genesisNeutralTarget = NeutralTarget GenesisStructuralTarget

nablaTarget :: NablaId -> StructuralOccurrenceId -> TargetRole
nablaTarget nabla = NablaTarget nabla . DynamicStructuralTarget

genesisNablaTarget :: NablaId -> TargetRole
genesisNablaTarget nabla = NablaTarget nabla GenesisStructuralTarget

deltaTarget :: DeltaId -> StructuralOccurrenceId -> TargetRole
deltaTarget delta = DeltaTarget delta . DynamicStructuralTarget

genesisDeltaTarget :: DeltaId -> TargetRole
genesisDeltaTarget delta = DeltaTarget delta GenesisStructuralTarget

edgeTarget :: StructuralOccurrenceId -> TargetRole
edgeTarget = EdgeTarget . DynamicStructuralTarget

genesisEdgeTarget :: TargetRole
genesisEdgeTarget = EdgeTarget GenesisStructuralTarget

targetRoleView :: TargetRole -> TargetRoleView
targetRoleView role = case role of
  AbsentOrdinaryTarget -> AbsentOrdinaryTargetView
  OrdinaryControlledTarget -> OrdinaryControlledTargetView
  NeutralTarget provenance ->
    NeutralTargetView (structuralTargetProvenanceView provenance)
  NablaTarget nabla provenance ->
    NablaTargetView nabla (structuralTargetProvenanceView provenance)
  DeltaTarget delta provenance ->
    DeltaTargetView delta (structuralTargetProvenanceView provenance)
  EdgeTarget provenance ->
    EdgeTargetView (structuralTargetProvenanceView provenance)

-- | One Store incarnation allocated atomically with local installation. Only a delta
-- patch assigning a process proved live and local may request one.
data StoreActivation = StoreActivation
  { delta :: DeltaId,
    sort :: SortOccurrence
  }
  deriving stock (Eq, Ord, Show)

storeActivation :: DeltaId -> SortOccurrence -> StoreActivation
storeActivation = StoreActivation

storeActivationDelta :: StoreActivation -> DeltaId
storeActivationDelta activation = activation.delta

storeActivationSort :: StoreActivation -> SortOccurrence
storeActivationSort activation = activation.sort

-- | Stable keys retained by their existing semantic owners.  The patch stores
-- no payload and no successor owner map.  More detailed live stream and
-- alignment coordinates remain private to their owners until Increment 6.
data TargetWorkKey
  = HeldApplicationWork GlobalObjectId ProcessEpochId Word64
  | HeldPeerPublication GlobalObjectId HeraldEpoch PublicationId
  | HeldStructuralWork GlobalObjectId StructuralTargetProvenance
  | HeldStructuralDependentWork
      StructuralTargetProvenance
      StructuralTargetProvenance
  | HeldStoreWork DeltaId StoreIncarnationId
  | HeldAlignmentWork DeltaId StoreIncarnationId
  | HeldEffectiveSortWork SortOccurrence
  | HeldProcessLifecycleWork ProcessEpochId
  | HeldObjectControlWork GlobalObjectId
  | HeldAllApplicationWorkForObject GlobalObjectId
  | HeldAllPeerWorkForObject GlobalObjectId
  | HeldAllStructuralDependents StructuralTargetProvenance
  | HeldAllStoreWorkForDelta DeltaId
  | HeldAllAlignmentWorkForDelta DeltaId
  | HeldAllEffectiveSortWork
  deriving stock (Eq, Ord, Show)

-- | A live role-qualified preparation context.  Its sort/resource cuts
-- are complete projections of the supplied registry, placement, and Store
-- states; downstream preparation cannot narrow them or attach a Store
-- activation to another delta/sort.  The checked constructor binds those
-- supplied states into one atomic Herald-owner cut and derives the target role
-- from the live structural/Controlled owner catalogue.
--
-- The checked cut supplied for an edge or neutral is the complete supplied
-- SortRegistry projection.  For a nabla this live derivation conservatively
-- uses the same complete effective-sort projection.  A delta cut gives one
-- explicit disposition for every sort; its keys therefore define the affected
-- set and activation subset together.
data PatchContext = PatchContext
  { role :: TargetRole,
    contextualSorts :: Set SortOccurrence,
    deltaSortDispositions :: Map SortOccurrence DeltaSortDisposition,
    locallyLiveProcesses :: Set ProcessEpochId
  }
  deriving stock (Eq, Show)

-- | Whether the complete delta context already has the required Store for a
-- sort or needs one fresh incarnation allocated at installation.
data DeltaSortDisposition
  = DeltaStoreRetained
  | DeltaStoreActivationRequired
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A complete affected-sort projection, keyed by the semantic sort identity
-- which selected each effective occurrence.  Opaqueness prevents a prepared
-- context from later being narrowed to a caller-chosen subset.
newtype CheckedAffectedSortCut
  = CheckedAffectedSortCut (Set SortOccurrence)
  deriving stock (Eq, Show)

-- | The complete supplied effective-sort registry projection at preparation.
-- It is built from opaque 'SortRegistry.State', so an edge/neutral caller
-- cannot claim that an arbitrary subset is the all-sort cut.
newtype CheckedEffectiveSortCut
  = CheckedEffectiveSortCut (Set SortOccurrence)
  deriving stock (Eq, Show)

-- | The corresponding complete delta projection.  Every contextual sort has
-- exactly one Store disposition, rather than activations being supplied as an
-- independent and potentially divergent set.
newtype CheckedDeltaSortCut
  = CheckedDeltaSortCut (Map SortOccurrence DeltaSortDisposition)
  deriving stock (Eq, Show)

-- | Exact locally live process projection from the supplied bounded Oracle
-- projection. A delta Store identity may be allocated only for a proposed
-- controller present in this cut; void, ended, unknown, and remotely resident
-- processes cannot cause local allocation.
newtype CheckedLocalProcessCut
  = CheckedLocalProcessCut (Set ProcessEpochId)
  deriving stock (Eq, Show)

checkedLocalProcessCut ::
  HeraldEpoch ->
  OracleProjection.State ->
  CheckedLocalProcessCut
checkedLocalProcessCut local projection =
  CheckedLocalProcessCut
    ( Set.fromList
        [ process
        | process <- OracleProjection.projectedProcessEpochs projection,
          OracleProjection.oracleViewProcessResidence process view == Just local,
          OracleProjection.oracleViewProcessIsLive process view
        ]
    )
  where
    view = OracleProjection.oracleView projection

data PatchContextProblem
  = PatchContextDeltaPlacementMissing DeltaId
  | PatchContextPlacementStoreMismatch DeltaId
  | PatchContextPlacementSortNotEffective DeltaId SortOccurrence
  | PatchContextStructuralCatalogueMismatch
      GlobalObjectId
      StructuralCarrierRole
      (Maybe StructuralCarrierRole)
  | PatchContextStructuralTargetDeleted GlobalObjectId
  | PatchContextProcessTargetNotLabelable GlobalObjectId ProcessEpochId
  | PatchContextCarriedSortNotEffective GlobalObjectId SortOccurrence
  | PatchContextDeltaPlacementSortMismatch
      DeltaId
      SortOccurrence
      SortOccurrence
  | PatchContextPreparedDigestMismatch
  deriving stock (Eq, Show)

effectiveSortPatchCut :: SortRegistry.State -> CheckedEffectiveSortCut
effectiveSortPatchCut registry =
  CheckedEffectiveSortCut
    ( Set.fromList
        [ sortOccurrence
            (SortRegistry.registryEntrySortId entry)
            (SortRegistry.registryEntryOccurrenceId entry)
        | entry <- SortRegistry.registryEntries registry
        ]
    )

checkedAffectedSortCut ::
  SortRegistry.State -> CheckedAffectedSortCut
checkedAffectedSortCut registry =
  let CheckedEffectiveSortCut sorts = effectiveSortPatchCut registry
   in CheckedAffectedSortCut sorts

checkedDeltaSortCut ::
  DeltaId ->
  SortRegistry.State ->
  Placement.State ->
  Store.State ->
  Either PatchContextProblem CheckedDeltaSortCut
checkedDeltaSortCut delta registry placements stores = do
  (targetSort, targetDisposition) <-
    deltaStoreDisposition delta placements stores
  let effectiveSorts =
        Set.fromList
          [ sortOccurrence
              (SortRegistry.registryEntrySortId entry)
              (SortRegistry.registryEntryOccurrenceId entry)
          | entry <- SortRegistry.registryEntries registry
          ]
  unless
    (Set.member targetSort effectiveSorts)
    (Left (PatchContextPlacementSortNotEffective delta targetSort))
  Right
    ( CheckedDeltaSortCut
        ( Map.fromList
            [ ( occurrence,
                if occurrence == targetSort
                  then targetDisposition
                  else DeltaStoreRetained
              )
            | occurrence <- Set.toAscList effectiveSorts
            ]
        )
    )

deltaStoreDisposition ::
  DeltaId ->
  Placement.State ->
  Store.State ->
  Either PatchContextProblem (SortOccurrence, DeltaSortDisposition)
deltaStoreDisposition delta placements stores =
  case (Placement.lookupLocalPlacement delta placements, Store.lookupStoreSlot delta stores) of
    (Nothing, _) -> Left (PatchContextDeltaPlacementMissing delta)
    (Just placement, Nothing) ->
      Right
        ( sortOccurrence
            (Placement.localPlacementSortId placement)
            (Placement.localPlacementOccurrenceId placement),
          DeltaStoreActivationRequired
        )
    (Just placement, Just slot)
      | Placement.localPlacementStoreIncarnation placement
          == Store.storeSlotIncarnation slot,
        Placement.localPlacementSortId placement == Store.storeSlotSortId slot,
        Placement.localPlacementOccurrenceId placement
          == Store.storeSlotOccurrenceId slot ->
          Right
            ( sortOccurrence
                (Placement.localPlacementSortId placement)
                (Placement.localPlacementOccurrenceId placement),
              DeltaStoreRetained
            )
    _ -> Left (PatchContextPlacementStoreMismatch delta)

absentOrdinaryPatchContext :: PatchContext
absentOrdinaryPatchContext = contextWithoutSorts AbsentOrdinaryTarget

ordinaryControlledPatchContext :: PatchContext
ordinaryControlledPatchContext = contextWithoutSorts OrdinaryControlledTarget

neutralPatchContext ::
  StructuralOccurrenceId ->
  CheckedEffectiveSortCut ->
  PatchContext
neutralPatchContext occurrence (CheckedEffectiveSortCut sorts) =
  contextWithSorts
    (NeutralTarget (DynamicStructuralTarget occurrence))
    sorts

nablaPatchContext ::
  NablaId ->
  StructuralOccurrenceId ->
  CheckedAffectedSortCut ->
  PatchContext
nablaPatchContext nabla occurrence (CheckedAffectedSortCut sorts) =
  contextWithSorts
    (NablaTarget nabla (DynamicStructuralTarget occurrence))
    sorts

deltaPatchContext ::
  DeltaId ->
  StructuralOccurrenceId ->
  CheckedDeltaSortCut ->
  CheckedLocalProcessCut ->
  PatchContext
deltaPatchContext
  delta
  occurrence
  (CheckedDeltaSortCut dispositions)
  (CheckedLocalProcessCut liveProcesses) =
    PatchContext
      { role = DeltaTarget delta (DynamicStructuralTarget occurrence),
        contextualSorts = Map.keysSet dispositions,
        deltaSortDispositions = dispositions,
        locallyLiveProcesses = liveProcesses
      }

edgePatchContext ::
  StructuralOccurrenceId ->
  CheckedEffectiveSortCut ->
  PatchContext
edgePatchContext occurrence (CheckedEffectiveSortCut sorts) =
  contextWithSorts
    (EdgeTarget (DynamicStructuralTarget occurrence))
    sorts

contextWithoutSorts :: TargetRole -> PatchContext
contextWithoutSorts role = contextWithSorts role Set.empty

contextWithSorts ::
  TargetRole ->
  Set SortOccurrence ->
  PatchContext
contextWithSorts role sorts =
  PatchContext
    { role,
      contextualSorts = sorts,
      deltaSortDispositions = Map.empty,
      locallyLiveProcesses = Set.empty
    }

-- | Derive the complete patch specification from one immutable owner cut.
-- The caller supplies no role or sort classification: structural provenance
-- comes from the replayable canonical target catalogue, ordinary presence and
-- Normal possession come from Controlled, and delta resource disposition is
-- checked jointly against Placement and Store.
checkedPatchSpecification ::
  HeraldEpoch ->
  PreparedLabelFacts ->
  PreparedLabelDigest ->
  GraphProgress.StructuralProgressState ->
  Controlled.State ->
  SortRegistry.State ->
  Placement.State ->
  Store.State ->
  OracleProjection.State ->
  Either PatchContextProblem PatchSpecification
checkedPatchSpecification local facts suppliedDigest progress controlled registry placements stores oracle = do
  unless
    (derivePreparedLabelDigest facts == suppliedDigest)
    (Left PatchContextPreparedDigestMismatch)
  context <-
    checkedPatchContext
      local
      (preparedLabelFactsObjectId facts)
      (preparedLabelFactsExpectedPriorState facts)
      (preparedLabelFactsProposedOutcome facts)
      (GraphProgress.structuralProgressReconciliation progress)
      controlled
      registry
      placements
      stores
      oracle
  Right (patchSpecification facts suppliedDigest context)

checkedPatchContext ::
  HeraldEpoch ->
  GlobalObjectId ->
  ReleasedLabelState ->
  ReleasedLabelState ->
  Reconciliation.StructuralAppliedState ->
  Controlled.State ->
  SortRegistry.State ->
  Placement.State ->
  Store.State ->
  OracleProjection.State ->
  Either PatchContextProblem PatchContext
checkedPatchContext local object prior proposed topology controlled registry placements stores oracle =
  case liveStructuralTarget object topology controlled of
    Nothing -> checkedOrdinaryContext object controlled
    Just (expectedRole, role, carriedSort) -> do
      let observedRole = controlledKnownStructuralRole object controlled
      unless
        (observedRole == Just expectedRole)
        ( Left
            ( PatchContextStructuralCatalogueMismatch
                object
                expectedRole
                observedRole
            )
        )
      case role of
        NeutralTarget provenance ->
          Right
            ( contextWithSorts
                (NeutralTarget provenance)
                deletionSorts
            )
        EdgeTarget provenance ->
          Right
            ( contextWithSorts
                (EdgeTarget provenance)
                deletionSorts
            )
        NablaTarget nabla provenance -> do
          requiredSort <- maybe structuralCatalogueFault Right carriedSort
          requireEffectiveCarriedSort object requiredSort registry
          Right
            ( contextWithSorts
                (NablaTarget nabla provenance)
                (Set.singleton requiredSort)
            )
        DeltaTarget delta provenance -> do
          requiredSort <- maybe structuralCatalogueFault Right carriedSort
          requireEffectiveCarriedSort object requiredSort registry
          exactStore <-
            checkedExistingDeltaStore delta requiredSort placements stores
          let activeProcess =
                checkedActiveLocalProcess local object proposed controlled oracle
              controllerChanged = not (sameProcessController prior proposed)
              disposition =
                case activeProcess of
                  Just _
                    | controllerChanged || not exactStore ->
                        DeltaStoreActivationRequired
                  _ -> DeltaStoreRetained
          Right
            PatchContext
              { role = DeltaTarget delta provenance,
                contextualSorts = Set.singleton requiredSort,
                deltaSortDispositions = Map.singleton requiredSort disposition,
                locallyLiveProcesses = maybe Set.empty Set.singleton activeProcess
              }
        AbsentOrdinaryTarget -> structuralCatalogueFault
        OrdinaryControlledTarget -> structuralCatalogueFault
  where
    deletionSorts
      | isDeleted proposed = effectiveSortSet registry
      | otherwise = Set.empty
    structuralCatalogueFault :: Either PatchContextProblem a
    structuralCatalogueFault =
      Left
        ( PatchContextStructuralCatalogueMismatch
            object
            ProcessEpochCarrier
            (controlledKnownStructuralRole object controlled)
        )

liveStructuralTarget ::
  GlobalObjectId ->
  Reconciliation.StructuralAppliedState ->
  Controlled.State ->
  Maybe (StructuralCarrierRole, TargetRole, Maybe SortOccurrence)
liveStructuralTarget object topology controlled =
  case Reconciliation.structuralAppliedVertexProjectionAt object topology of
    Just projection -> Just (vertexTarget projection)
    Nothing -> case Reconciliation.structuralAppliedEdgeProjectionAt object topology of
      Just edge ->
        Just
          ( EdgeCarrier,
            EdgeTarget
              (projectionProvenance (Reconciliation.ownedEdgeProjectionProvenance edge)),
            Nothing
          )
      Nothing -> genesisTarget object controlled

vertexTarget ::
  Reconciliation.StructuralVertexProjection ->
  (StructuralCarrierRole, TargetRole, Maybe SortOccurrence)
vertexTarget projection =
  case projection of
    Reconciliation.NeutralVertexProjection provenance _ ->
      (NeutralVertexCarrier, NeutralTarget (projectionProvenance provenance), Nothing)
    Reconciliation.NablaVertexProjection provenance nabla carriedSort _ _ ->
      ( NablaCarrier,
        NablaTarget nabla (projectionProvenance provenance),
        Just carriedSort
      )
    Reconciliation.DeltaVertexProjection provenance delta carriedSort _ _ ->
      ( DeltaCarrier,
        DeltaTarget delta (projectionProvenance provenance),
        Just carriedSort
      )

projectionProvenance ::
  Reconciliation.StructuralProjectionProvenance ->
  StructuralTargetProvenance
projectionProvenance provenance =
  maybe
    GenesisStructuralTarget
    DynamicStructuralTarget
    (Reconciliation.structuralProjectionProvenanceOccurrence provenance)

genesisTarget ::
  GlobalObjectId ->
  Controlled.State ->
  Maybe (StructuralCarrierRole, TargetRole, Maybe SortOccurrence)
genesisTarget object controlled =
  case [ root
       | root <- Controlled.controlledRootFacts controlled,
         rootObject root == object
       ] of
    [root] -> case Controlled.rootFactRole root of
      Controlled.ControlledWriter nabla _ ->
        Just
          ( NablaCarrier,
            genesisNablaTarget nabla,
            Just (rootSort root)
          )
      Controlled.ControlledReader delta ->
        Just
          ( DeltaCarrier,
            genesisDeltaTarget delta,
            Just (rootSort root)
          )
    _ -> case [ process
              | process <- Controlled.controlledProcessFacts controlled,
                globalObjectIdFromProcessEpochId
                  (Controlled.processFactProcessEpoch process)
                  == object
              ] of
      [_] ->
        Just
          ( ProcessEpochCarrier,
            OrdinaryControlledTarget,
            Nothing
          )
      _ -> Nothing
  where
    rootObject root = case Controlled.rootFactRole root of
      Controlled.ControlledWriter nabla _ -> globalObjectIdFromNablaId nabla
      Controlled.ControlledReader delta -> globalObjectIdFromDeltaId delta
    rootSort root =
      sortOccurrence
        (Controlled.rootFactSortId root)
        (Controlled.rootFactOccurrenceId root)

checkedOrdinaryContext ::
  GlobalObjectId ->
  Controlled.State ->
  Either PatchContextProblem PatchContext
checkedOrdinaryContext object controlled =
  case Controlled.controlledLocalRecord object controlled of
    Just record -> case Controlled.controlledRecordStructuralRole record of
      Just role ->
        Left (PatchContextStructuralCatalogueMismatch object role Nothing)
      Nothing -> Right ordinaryControlledPatchContext
    Nothing -> case Controlled.controlledCurrentStructuralRole object controlled of
      Just role ->
        Left (PatchContextStructuralCatalogueMismatch object role Nothing)
      Nothing -> Right absentOrdinaryPatchContext

controlledKnownStructuralRole ::
  GlobalObjectId -> Controlled.State -> Maybe StructuralCarrierRole
controlledKnownStructuralRole object controlled =
  case Controlled.controlledCurrentStructuralRole object controlled of
    Just role -> Just role
    Nothing ->
      Controlled.controlledLocalRecord object controlled
        >>= Controlled.controlledRecordStructuralRole

effectiveSortSet :: SortRegistry.State -> Set SortOccurrence
effectiveSortSet registry =
  Set.fromList
    [ sortOccurrence
        (SortRegistry.registryEntrySortId entry)
        (SortRegistry.registryEntryOccurrenceId entry)
    | entry <- SortRegistry.registryEntries registry
    ]

requireEffectiveCarriedSort ::
  GlobalObjectId ->
  SortOccurrence ->
  SortRegistry.State ->
  Either PatchContextProblem ()
requireEffectiveCarriedSort object carriedSort registry =
  unless
    ( maybe
        False
        ((== sortOccurrenceDefinition carriedSort) . SortRegistry.registryEntryOccurrenceId)
        (SortRegistry.lookupEffectiveSort (sortOccurrenceSortId carriedSort) registry)
    )
    (Left (PatchContextCarriedSortNotEffective object carriedSort))

checkedActiveLocalProcess ::
  HeraldEpoch ->
  GlobalObjectId ->
  ReleasedLabelState ->
  Controlled.State ->
  OracleProjection.State ->
  Maybe ProcessEpochId
checkedActiveLocalProcess local object proposed controlled oracle =
  case releasedLabelStateView proposed of
    ReleasedLabelView (ProcessLabel process, _)
      | OracleProjection.oracleViewProcessIsLive process view,
        OracleProjection.oracleViewProcessResidence process view == Just local,
        Controlled.controlledHasNormalPossession process object controlled ->
          Just process
    _ -> Nothing
  where
    view = OracleProjection.oracleView oracle

sameProcessController :: ReleasedLabelState -> ReleasedLabelState -> Bool
sameProcessController prior proposed =
  case (releasedLabelStateView prior, releasedLabelStateView proposed) of
    (ReleasedLabelView (ProcessLabel old, _), ReleasedLabelView (ProcessLabel new, _)) ->
      old == new
    _ -> False

checkedExistingDeltaStore ::
  DeltaId ->
  SortOccurrence ->
  Placement.State ->
  Store.State ->
  Either PatchContextProblem Bool
checkedExistingDeltaStore delta carriedSort placements stores =
  case (Placement.lookupLocalPlacement delta placements, Store.lookupStoreSlot delta stores) of
    (Nothing, Nothing) -> Right False
    (Nothing, Just _) -> Left (PatchContextPlacementStoreMismatch delta)
    (Just placement, maybeSlot) -> do
      let placedSort =
            sortOccurrence
              (Placement.localPlacementSortId placement)
              (Placement.localPlacementOccurrenceId placement)
      unless
        (placedSort == carriedSort)
        ( Left
            ( PatchContextDeltaPlacementSortMismatch
                delta
                carriedSort
                placedSort
            )
        )
      case maybeSlot of
        Nothing -> Right False
        Just slot
          | Placement.localPlacementStoreIncarnation placement
              == Store.storeSlotIncarnation slot,
            Placement.localPlacementSortId placement == Store.storeSlotSortId slot,
            Placement.localPlacementOccurrenceId placement
              == Store.storeSlotOccurrenceId slot ->
              Right True
          | otherwise -> Left (PatchContextPlacementStoreMismatch delta)

-- | Transient normalized input derived from one current owner cut. Local
-- preparation retains canonical decision facts and target holds; installation
-- derives a fresh specification and allocates Store identities atomically.
data PatchSpecification = PatchSpecification
  { preparedFacts :: PreparedLabelFacts,
    decision :: LabelDecisionId,
    object :: GlobalObjectId,
    role :: TargetRole,
    expectedPriorState :: ReleasedLabelState,
    expectedPriorRevision :: Maybe LabelRevision,
    proposedState :: ReleasedLabelState,
    authorityDisposition :: PreparedAuthorityDisposition,
    preparedDigest :: PreparedLabelDigest,
    affectedSorts :: Set SortOccurrence,
    storeActivations :: Set StoreActivation,
    heldWork :: Set TargetWorkKey
  }
  deriving stock (Eq, Show)

patchSpecification ::
  PreparedLabelFacts ->
  PreparedLabelDigest ->
  PatchContext ->
  PatchSpecification
patchSpecification facts preparedDigest context =
  PatchSpecification
    { preparedFacts = facts,
      decision = preparedLabelFactsDecisionId facts,
      object = preparedLabelFactsObjectId facts,
      role = context.role,
      expectedPriorState = preparedLabelFactsExpectedPriorState facts,
      expectedPriorRevision =
        preparedLabelFactsExpectedPriorRevision facts,
      proposedState = preparedLabelFactsProposedOutcome facts,
      authorityDisposition = preparedLabelFactsAuthorityDisposition facts,
      preparedDigest,
      affectedSorts,
      storeActivations,
      heldWork =
        mandatoryHeldWork
          (preparedLabelFactsObjectId facts)
          context.role
          (preparedLabelFactsExpectedPriorState facts)
          (preparedLabelFactsProposedOutcome facts)
          affectedSorts
    }
  where
    affectedSorts =
      contextAffectedSorts
        context
        (preparedLabelFactsProposedOutcome facts)
    storeActivations =
      contextStoreActivations
        context
        (preparedLabelFactsProposedOutcome facts)

contextAffectedSorts ::
  PatchContext -> ReleasedLabelState -> Set SortOccurrence
contextAffectedSorts context proposedState = case context.role of
  AbsentOrdinaryTarget -> Set.empty
  OrdinaryControlledTarget -> Set.empty
  NeutralTarget {}
    | isDeleted proposedState -> context.contextualSorts
    | otherwise -> Set.empty
  NablaTarget {} -> context.contextualSorts
  DeltaTarget {} -> context.contextualSorts
  EdgeTarget {}
    | isDeleted proposedState -> context.contextualSorts
    | otherwise -> Set.empty

contextStoreActivations ::
  PatchContext -> ReleasedLabelState -> Set StoreActivation
contextStoreActivations context proposedState =
  case releasedLabelStateView proposedState of
    ReleasedLabelView (ProcessLabel process, _)
      | Set.member process context.locallyLiveProcesses -> case context.role of
          DeltaTarget delta _ ->
            Set.fromList
              [ StoreActivation delta sort
              | (sort, disposition) <-
                  Map.toAscList context.deltaSortDispositions,
                disposition == DeltaStoreActivationRequired
              ]
          _ -> Set.empty
      | otherwise -> Set.empty
    ReleasedLabelView (VoidLabel, _) -> Set.empty
    ReleasedLabelView (ZombieLabel _, _) -> Set.empty
    ReleasedDeletedView {} -> Set.empty

mandatoryHeldWork ::
  GlobalObjectId ->
  TargetRole ->
  ReleasedLabelState ->
  ReleasedLabelState ->
  Set SortOccurrence ->
  Set TargetWorkKey
mandatoryHeldWork object role prior proposed affectedSorts =
  Set.fromList
    ( [ HeldObjectControlWork object,
        HeldAllApplicationWorkForObject object,
        HeldAllPeerWorkForObject object
      ]
        <> fmap (HeldStructuralWork object) structuralProvenances
        <> fmap HeldAllStructuralDependents structuralProvenances
        <> fmap HeldEffectiveSortWork (Set.toAscList affectedSorts)
        <> roleOwnerHolds role
        <> [ HeldAllEffectiveSortWork
           | EdgeTarget {} <- [role],
             isDeleted proposed
           ]
        <> fmap
          HeldProcessLifecycleWork
          ( Set.toAscList
              ( processesInReleasedState prior
                  <> processesInReleasedState proposed
              )
          )
    )
  where
    structuralProvenances = targetStructuralProvenance role

roleOwnerHolds :: TargetRole -> [TargetWorkKey]
roleOwnerHolds role = case role of
  DeltaTarget delta _ ->
    [ HeldAllStoreWorkForDelta delta,
      HeldAllAlignmentWorkForDelta delta
    ]
  _ -> []

patchSpecificationDecision :: PatchSpecification -> LabelDecisionId
patchSpecificationDecision specification = specification.decision

patchSpecificationPreparedFacts :: PatchSpecification -> PreparedLabelFacts
patchSpecificationPreparedFacts specification = specification.preparedFacts

patchSpecificationObject :: PatchSpecification -> GlobalObjectId
patchSpecificationObject specification = specification.object

patchSpecificationRole :: PatchSpecification -> TargetRole
patchSpecificationRole specification = specification.role

patchSpecificationExpectedPriorState ::
  PatchSpecification -> ReleasedLabelState
patchSpecificationExpectedPriorState specification =
  specification.expectedPriorState

patchSpecificationExpectedPriorRevision ::
  PatchSpecification -> Maybe LabelRevision
patchSpecificationExpectedPriorRevision specification =
  specification.expectedPriorRevision

patchSpecificationProposedState :: PatchSpecification -> ReleasedLabelState
patchSpecificationProposedState specification = specification.proposedState

patchSpecificationAuthorityDisposition ::
  PatchSpecification -> PreparedAuthorityDisposition
patchSpecificationAuthorityDisposition specification =
  specification.authorityDisposition

patchSpecificationPreparedDigest :: PatchSpecification -> PreparedLabelDigest
patchSpecificationPreparedDigest specification = specification.preparedDigest

patchSpecificationAffectedSorts :: PatchSpecification -> Set SortOccurrence
patchSpecificationAffectedSorts specification = specification.affectedSorts

patchSpecificationStoreActivations :: PatchSpecification -> Set StoreActivation
patchSpecificationStoreActivations specification = specification.storeActivations

patchSpecificationHeldWork :: PatchSpecification -> Set TargetWorkKey
patchSpecificationHeldWork specification = specification.heldWork

-- | Narrow declarative transform selected entirely during preparation.
data PatchForm
  = InstallFutureReleasedOverlay
  | InstallFutureTerminalSuppression
  | OverlayOrdinaryControlledObject
  | DeleteOrdinaryControlledObject
  | OverlayNeutralVertex
  | DeleteNeutralVertex
  | RelabelNablaProjection
  | DeleteNablaProjection
  | RelabelDeltaProjection
  | DeleteDeltaProjection
  | OverlayEdge
  | DeleteEdge
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A recipe contains only stable semantic coordinates.  In particular, it
-- contains no Store, Graph, Placement, Alignment, Controlled, Visibility, or
-- Herald successor state.
data LabelPatchRecipe = LabelPatchRecipe
  { specification :: PatchSpecification,
    form :: PatchForm,
    preallocatedStores :: Map StoreActivation StoreIncarnationId
  }
  deriving stock (Eq, Show)

labelPatchRecipeDecision :: LabelPatchRecipe -> LabelDecisionId
labelPatchRecipeDecision recipe = recipe.specification.decision

labelPatchRecipePreparedFacts :: LabelPatchRecipe -> PreparedLabelFacts
labelPatchRecipePreparedFacts recipe = recipe.specification.preparedFacts

labelPatchRecipeObject :: LabelPatchRecipe -> GlobalObjectId
labelPatchRecipeObject recipe = recipe.specification.object

labelPatchRecipeRole :: LabelPatchRecipe -> TargetRole
labelPatchRecipeRole recipe = recipe.specification.role

labelPatchRecipeForm :: LabelPatchRecipe -> PatchForm
labelPatchRecipeForm recipe = recipe.form

labelPatchRecipeExpectedPriorState ::
  LabelPatchRecipe -> ReleasedLabelState
labelPatchRecipeExpectedPriorState recipe =
  recipe.specification.expectedPriorState

labelPatchRecipeExpectedPriorRevision ::
  LabelPatchRecipe -> Maybe LabelRevision
labelPatchRecipeExpectedPriorRevision recipe =
  recipe.specification.expectedPriorRevision

labelPatchRecipeProposedState :: LabelPatchRecipe -> ReleasedLabelState
labelPatchRecipeProposedState recipe = recipe.specification.proposedState

labelPatchRecipeAuthorityDisposition ::
  LabelPatchRecipe -> PreparedAuthorityDisposition
labelPatchRecipeAuthorityDisposition recipe =
  recipe.specification.authorityDisposition

labelPatchRecipePreparedDigest :: LabelPatchRecipe -> PreparedLabelDigest
labelPatchRecipePreparedDigest recipe = recipe.specification.preparedDigest

labelPatchRecipeAffectedSorts :: LabelPatchRecipe -> Set SortOccurrence
labelPatchRecipeAffectedSorts recipe = recipe.specification.affectedSorts

labelPatchRecipePreallocatedStores ::
  LabelPatchRecipe -> Map StoreActivation StoreIncarnationId
labelPatchRecipePreallocatedStores recipe = recipe.preallocatedStores

labelPatchRecipeHeldWork :: LabelPatchRecipe -> Set TargetWorkKey
labelPatchRecipeHeldWork recipe = recipe.specification.heldWork

data PatchPhaseView
  = PatchPreparedView
  | PatchReleasedView ControlIndex
  deriving stock (Eq, Show)

-- | Historical installation proof. The canonical facts are fixed-size and
-- retain the outcome needed for the next decision's prior-state check. The
-- installation role records what was actually installed, including absence.
-- A separate preparation role authenticates replay independently of later
-- target materialization. Neither fact is rewritten later.
-- Force the compact fields so selector thunks cannot retain the old recipe.
data InstalledLabel = InstalledLabel
  { preparedFacts :: !PreparedLabelFacts,
    preparedDigest :: !PreparedLabelDigest,
    role :: !TargetRole,
    preparationRole :: !TargetRole,
    releaseIndex :: !ControlIndex
  }
  deriving stock (Eq, Show)

installedLabelDecision :: InstalledLabel -> LabelDecisionId
installedLabelDecision = preparedLabelFactsDecisionId . (.preparedFacts)

installedLabelObject :: InstalledLabel -> GlobalObjectId
installedLabelObject = preparedLabelFactsObjectId . (.preparedFacts)

installedLabelPreparedFacts :: InstalledLabel -> PreparedLabelFacts
installedLabelPreparedFacts = (.preparedFacts)

installedLabelPreparedDigest :: InstalledLabel -> PreparedLabelDigest
installedLabelPreparedDigest = (.preparedDigest)

installedLabelRole :: InstalledLabel -> TargetRole
installedLabelRole = (.role)

installedLabelPreparationRole :: InstalledLabel -> TargetRole
installedLabelPreparationRole = (.preparationRole)

installedLabelReleaseIndex :: InstalledLabel -> ControlIndex
installedLabelReleaseIndex = (.releaseIndex)

installedLabelMatchesRecipe :: InstalledLabel -> LabelPatchRecipe -> Bool
installedLabelMatchesRecipe installed recipe =
  installedMatchesSpecification installed recipe.specification

installedMatchesSpecification :: InstalledLabel -> PatchSpecification -> Bool
installedMatchesSpecification installed specification =
  installed.preparedFacts == specification.preparedFacts
    && installed.preparedDigest == specification.preparedDigest
    && installed.role == specification.role

installedPreparationMatchesSpecification :: InstalledLabel -> PatchSpecification -> Bool
installedPreparationMatchesSpecification installed specification =
  installed.preparedFacts == specification.preparedFacts
    && installed.preparedDigest == specification.preparedDigest
    && installed.preparationRole == specification.role

-- | Canonical decision evidence and holds during local installation.
-- The local role records preparation-time bootstrap provenance only. It does
-- not prescribe the installation role or resource consequences.
data PendingLabelPreparation = PendingLabelPreparation
  { preparedFacts :: !PreparedLabelFacts,
    preparedDigest :: !PreparedLabelDigest,
    role :: !TargetRole,
    heldWork :: !(Set TargetWorkKey)
  }
  deriving stock (Eq, Show)

pendingLabelPreparationDecision :: PendingLabelPreparation -> LabelDecisionId
pendingLabelPreparationDecision = preparedLabelFactsDecisionId . (.preparedFacts)

pendingLabelPreparationObject :: PendingLabelPreparation -> GlobalObjectId
pendingLabelPreparationObject = preparedLabelFactsObjectId . (.preparedFacts)

pendingLabelPreparationRole :: PendingLabelPreparation -> TargetRole
pendingLabelPreparationRole = (.role)

pendingLabelPreparationHeldWork :: PendingLabelPreparation -> Set TargetWorkKey
pendingLabelPreparationHeldWork = (.heldWork)

pendingMatchesSpecification :: PendingLabelPreparation -> PatchSpecification -> Bool
pendingMatchesSpecification pending specification =
  pending.preparedFacts == specification.preparedFacts
    && pending.preparedDigest == specification.preparedDigest
    && pending.role == specification.role
    && pending.heldWork == specification.heldWork

data RetainedPatch
  = PreparedPatch !PendingLabelPreparation !ByteString !Word64
  | InstalledPatch !InstalledLabel !ByteString !Word64
  deriving stock (Eq, Show)

data RetainedPatchView
  = PreparedPatchView PendingLabelPreparation
  | InstalledPatchView InstalledLabel
  deriving stock (Eq, Show)

retainedPatchViewPreparation :: RetainedPatchView -> Maybe PendingLabelPreparation
retainedPatchViewPreparation (PreparedPatchView pending) = Just pending
retainedPatchViewPreparation (InstalledPatchView _) = Nothing

retainedPatchViewInstallation :: RetainedPatchView -> Maybe InstalledLabel
retainedPatchViewInstallation (PreparedPatchView _) = Nothing
retainedPatchViewInstallation (InstalledPatchView installed) = Just installed

retainedPatchViewObject :: RetainedPatchView -> GlobalObjectId
retainedPatchViewObject (PreparedPatchView pending) = pendingLabelPreparationObject pending
retainedPatchViewObject (InstalledPatchView installed) = installedLabelObject installed

retainedPatchViewPreparedFacts :: RetainedPatchView -> PreparedLabelFacts
retainedPatchViewPreparedFacts (PreparedPatchView pending) = pending.preparedFacts
retainedPatchViewPreparedFacts (InstalledPatchView installed) = installedLabelPreparedFacts installed

retainedPatchViewPreparedDigest :: RetainedPatchView -> PreparedLabelDigest
retainedPatchViewPreparedDigest (PreparedPatchView pending) = pending.preparedDigest
retainedPatchViewPreparedDigest (InstalledPatchView installed) = installedLabelPreparedDigest installed

retainedPatchViewPhase :: RetainedPatchView -> PatchPhaseView
retainedPatchViewPhase (PreparedPatchView _) = PatchPreparedView
retainedPatchViewPhase (InstalledPatchView installed) = PatchReleasedView installed.releaseIndex

data State = State
  { byDecision :: Map LabelDecisionId RetainedPatch,
    currentByObject :: Map GlobalObjectId LabelDecisionId,
    importedByDecision :: !(Map LabelDecisionId ImportedLabelPatch),
    importedCurrentByObject :: !(Map GlobalObjectId LabelDecisionId),
    importedControlIndex :: !(Maybe ControlIndex),
    work :: !LabelStructuralWork
  }
  deriving stock (Eq, Show)

initialState :: State
initialState = State Map.empty Map.empty Map.empty Map.empty Nothing noLabelStructuralWork

-- | Portable predecessor evidence, distinct from proof of a receiver-local
-- installation. It contains neither a local target role nor generator lineage.
data ImportedLabelPatch = ImportedLabelPatch !PreparedLabelFacts !PreparedLabelDigest !ControlIndex
  deriving stock (Eq, Show)

newtype CheckedLabelPatchBase = CheckedLabelPatchBase (Map GlobalObjectId ImportedLabelPatch)
  deriving stock (Eq, Show)

importedLabelPatchDecision :: ImportedLabelPatch -> LabelDecisionId
importedLabelPatchDecision = preparedLabelFactsDecisionId . importedLabelPatchPreparedFacts

importedLabelPatchObject :: ImportedLabelPatch -> GlobalObjectId
importedLabelPatchObject = preparedLabelFactsObjectId . importedLabelPatchPreparedFacts

importedLabelPatchPreparedFacts :: ImportedLabelPatch -> PreparedLabelFacts
importedLabelPatchPreparedFacts (ImportedLabelPatch facts _ _) = facts

importedLabelPatchPreparedDigest :: ImportedLabelPatch -> PreparedLabelDigest
importedLabelPatchPreparedDigest (ImportedLabelPatch _ digest _) = digest

importedLabelPatchReleaseIndex :: ImportedLabelPatch -> ControlIndex
importedLabelPatchReleaseIndex (ImportedLabelPatch _ _ index) = index

checkedLabelPatchBaseEntries :: CheckedLabelPatchBase -> [ImportedLabelPatch]
checkedLabelPatchBaseEntries (CheckedLabelPatchBase entries) = Map.elems entries

importedLabelPatches :: State -> Map LabelDecisionId ImportedLabelPatch
importedLabelPatches = (.importedByDecision)

lookupImportedLabelPatch :: LabelDecisionId -> State -> Maybe ImportedLabelPatch
lookupImportedLabelPatch decision = Map.lookup decision . (.importedByDecision)

-- | The immutable adopted cut, independent of later local patch installation
-- or projection replay-base maintenance. Historical structural causes below it
-- are authenticated by the composed base's retained canonical projection.
importedLabelBaseControlIndex :: State -> Maybe ControlIndex
importedLabelBaseControlIndex = (.importedControlIndex)

-- | Capture only the latest completed predecessor per object. Earlier local
-- installation archives and imported baselines remain with their own owner.
captureCheckedLabelPatchBase :: State -> Either PatchProblem CheckedLabelPatchBase
captureCheckedLabelPatchBase state = do
  current <- Map.traverseWithKey captureCurrent state.currentByObject
  let inherited = Map.fromList [(importedLabelPatchObject entry, entry) | entry <- Map.elems state.importedByDecision]
      !entries = current `Map.union` inherited
  Right (CheckedLabelPatchBase entries)
  where
    captureCurrent object decision = case Map.lookup decision state.byDecision of
      Just (InstalledPatch installed _ _) ->
        let !entry = ImportedLabelPatch installed.preparedFacts installed.preparedDigest installed.releaseIndex
         in Right entry
      _ -> Left (PatchObjectBusy object decision)

-- | Reconstruct portable predecessors from the admitted semantic projection.
-- The facts, their digest and release coordinate already belong to that base;
-- transferring another copy of local patch history adds no authority. Ignore
-- unsuccessful decisions and retain the latest actual release for each object.
-- End changes effective labels in Controlled, not these immutable CAS facts.
labelPatchBaseFromProjection :: OracleProjection.State -> CheckedLabelPatchBase
labelPatchBaseFromProjection projection = CheckedLabelPatchBase entries
  where
    !entries =
      Map.fromListWith
        later
        [ (preparedLabelFactsObjectId facts, ImportedLabelPatch facts digest index)
        | (_, workflow) <- OracleProjection.projectedLabelWorkflows projection,
          Just facts <- [OracleProjection.projectedLabelWorkflowPreparedFacts workflow],
          Just digest <- [OracleProjection.projectedLabelWorkflowPreparedDigest workflow],
          Just (OracleProjection.ProjectedLabelReleased index _ _) <- [OracleProjection.projectedLabelWorkflowTerminal workflow]
        ]
    later left right
      | importedLabelPatchReleaseIndex left > importedLabelPatchReleaseIndex right = left
      | otherwise = right

-- | Install a checked semantic baseline into a pristine receiver-local owner.
-- No source holds, installation roles, Store allocations or work counters cross
-- this seam. The composed transaction checks the matching control/label base.
installCheckedLabelPatchBase :: ControlIndex -> CheckedLabelPatchBase -> State -> Either PatchProblem State
installCheckedLabelPatchBase cut (CheckedLabelPatchBase entries) state = do
  unless
    (Map.null state.byDecision && Map.null state.currentByObject && Map.null state.importedByDecision && Map.null state.importedCurrentByObject && state.importedControlIndex == Nothing)
    (Left PatchBaseTargetNotPristine)
  mapM_
    (\entry -> unless (importedLabelPatchReleaseIndex entry <= cut) (Left (PatchBaseReleaseBeyondCut (importedLabelPatchReleaseIndex entry) cut)))
    entries
  let !proofs = Map.fromList [(importedLabelPatchDecision entry, entry) | entry <- Map.elems entries]
      !current = Map.map importedLabelPatchDecision entries
      !successor = state {importedByDecision = proofs, importedCurrentByObject = current, importedControlIndex = Just cut}
  Right successor

-- | Cumulative actual label work at this owner. Every field is a strict scalar;
-- recording a patch retains neither that patch nor any predecessor state. These
-- counters describe the selected preparation path and applied leaf work, not
-- structural reports, retained plan inventories, or work on other transitions.
data LabelStructuralWork = LabelStructuralWork
  { labelWorkProjectionReuses :: !Word64,
    labelWorkFullPreparations :: !Word64,
    labelWorkGraphPreparations :: !Word64,
    labelWorkDebtPreparations :: !Word64,
    labelWorkGraphChanges :: !Word64,
    labelWorkPlacementChanges :: !Word64,
    labelWorkStoreActivations :: !Word64,
    labelWorkStorePassivations :: !Word64,
    labelWorkStoreReplacements :: !Word64,
    labelWorkStoreDeletions :: !Word64,
    labelWorkDebts :: !Word64
  }
  deriving stock (Eq, Show)

instance Semigroup LabelStructuralWork where
  left <> right =
    LabelStructuralWork
      { labelWorkProjectionReuses = left.labelWorkProjectionReuses + right.labelWorkProjectionReuses,
        labelWorkFullPreparations = left.labelWorkFullPreparations + right.labelWorkFullPreparations,
        labelWorkGraphPreparations = left.labelWorkGraphPreparations + right.labelWorkGraphPreparations,
        labelWorkDebtPreparations = left.labelWorkDebtPreparations + right.labelWorkDebtPreparations,
        labelWorkGraphChanges = left.labelWorkGraphChanges + right.labelWorkGraphChanges,
        labelWorkPlacementChanges = left.labelWorkPlacementChanges + right.labelWorkPlacementChanges,
        labelWorkStoreActivations = left.labelWorkStoreActivations + right.labelWorkStoreActivations,
        labelWorkStorePassivations = left.labelWorkStorePassivations + right.labelWorkStorePassivations,
        labelWorkStoreReplacements = left.labelWorkStoreReplacements + right.labelWorkStoreReplacements,
        labelWorkStoreDeletions = left.labelWorkStoreDeletions + right.labelWorkStoreDeletions,
        labelWorkDebts = left.labelWorkDebts + right.labelWorkDebts
      }

instance Monoid LabelStructuralWork where
  mempty = LabelStructuralWork 0 0 0 0 0 0 0 0 0 0 0

noLabelStructuralWork :: LabelStructuralWork
noLabelStructuralWork = mempty

-- | Count the checked per-object patches, never reconstruct their inputs or
-- compare complete owner maps. A deletion retires the active Store; other
-- transitions to no active Store passivate it. A replacement counts once rather
-- than pretending that one label activated and passivated independent Stores.
structuralLabelPreparationWork :: Bool -> Reconciliation.PreparedStructuralReconciliation -> LabelStructuralWork
structuralLabelPreparationWork deletion prepared =
  foldl' countStore base (Map.elems (Reconciliation.storePatchChanges (Reconciliation.preparedStructuralStorePatch prepared)))
  where
    graph = Reconciliation.preparedStructuralGraphPatch prepared
    changed = if Reconciliation.preparedStructuralChangesState prepared then 1 else 0
    base =
      noLabelStructuralWork
        { labelWorkFullPreparations = 1,
          labelWorkGraphPreparations = changed,
          labelWorkDebtPreparations = changed,
          labelWorkGraphChanges = fromIntegral (Map.size (Reconciliation.structuralGraphVertexChanges graph) + Map.size (Reconciliation.structuralGraphEdgeChanges graph)),
          labelWorkPlacementChanges = fromIntegral (Map.size (Reconciliation.placementPatchChanges (Reconciliation.preparedStructuralPlacementPatch prepared))),
          labelWorkDebts = fromIntegral (length (structuralDebtSetEntries (Reconciliation.preparedStructuralDebts prepared)))
        }
    countStore counts change = case (Reconciliation.exactChangeBefore change, Reconciliation.exactChangeAfter change) of
      (Nothing, Just _) -> counts {labelWorkStoreActivations = counts.labelWorkStoreActivations + 1}
      (Just _, Nothing)
        | deletion -> counts {labelWorkStoreDeletions = counts.labelWorkStoreDeletions + 1}
        | otherwise -> counts {labelWorkStorePassivations = counts.labelWorkStorePassivations + 1}
      (Just before, Just after)
        | Reconciliation.dynamicStoreIncarnation before /= Reconciliation.dynamicStoreIncarnation after -> counts {labelWorkStoreReplacements = counts.labelWorkStoreReplacements + 1}
      _ -> counts

recordLabelStructuralWork :: LabelStructuralWork -> State -> State
recordLabelStructuralWork observed state = state {work = state.work <> observed}

-- | A finite shutdown snapshot, independent of the number of retained labels.
-- The runtime requests this view only when its optional work-count sink exists.
labelStructuralWorkCounts :: State -> [(String, Word64)]
labelStructuralWorkCounts state =
  [ ("label.structural.projection_reuses", state.work.labelWorkProjectionReuses),
    ("label.structural.full_preparations", state.work.labelWorkFullPreparations),
    ("label.structural.graph_preparations", state.work.labelWorkGraphPreparations),
    ("label.structural.debt_preparations", state.work.labelWorkDebtPreparations),
    ("label.structural.graph_changes", state.work.labelWorkGraphChanges),
    ("label.structural.placement_changes", state.work.labelWorkPlacementChanges),
    ("label.structural.store_activations", state.work.labelWorkStoreActivations),
    ("label.structural.store_passivations", state.work.labelWorkStorePassivations),
    ("label.structural.store_replacements", state.work.labelWorkStoreReplacements),
    ("label.structural.store_deletions", state.work.labelWorkStoreDeletions),
    ("label.structural.debts", state.work.labelWorkDebts)
  ]

retainedLabelPatches :: State -> Map LabelDecisionId RetainedPatchView
retainedLabelPatches state = fmap retainedPatchView state.byDecision

lookupRetainedLabelPatch ::
  LabelDecisionId -> State -> Maybe RetainedPatchView
lookupRetainedLabelPatch decision state =
  retainedPatchView <$> Map.lookup decision state.byDecision

retainedPatchView :: RetainedPatch -> RetainedPatchView
retainedPatchView (PreparedPatch pending _ _) = PreparedPatchView pending
retainedPatchView (InstalledPatch installed _ _) = InstalledPatchView installed

heldTargetWork :: State -> Set TargetWorkKey
heldTargetWork state =
  Set.unions
    [ pending.heldWork
    | PreparedPatch pending _ _ <- Map.elems state.byDecision
    ]

targetWorkIsHeld :: TargetWorkKey -> State -> Bool
targetWorkIsHeld target state =
  Set.member target held
    || case target of
      HeldApplicationWork object _ _ ->
        Set.member (HeldAllApplicationWorkForObject object) held
      HeldPeerPublication object _ _ ->
        Set.member (HeldAllPeerWorkForObject object) held
      HeldStructuralDependentWork subject _ ->
        Set.member (HeldAllStructuralDependents subject) held
      HeldStoreWork delta _ -> Set.member (HeldAllStoreWorkForDelta delta) held
      HeldAlignmentWork delta _ ->
        Set.member (HeldAllAlignmentWorkForDelta delta) held
      HeldEffectiveSortWork _ -> Set.member HeldAllEffectiveSortWork held
      _ -> False
  where
    held = heldTargetWork state

data PatchPreparationDisposition
  = PatchPreparedFresh
  | PatchPreparationExactReplay
  deriving stock (Eq, Show)

data PatchProblem
  = PatchDecisionConflict LabelDecisionId
  | PatchBaseTargetNotPristine
  | PatchBaseReleaseBeyondCut ControlIndex ControlIndex
  | PatchObjectBusy GlobalObjectId LabelDecisionId
  | PatchObjectTerminallyDeleted GlobalObjectId
  | PatchPriorStateMismatch
      ReleasedLabelState
      ReleasedLabelState
  | PatchPriorRevisionMismatch
      (Maybe LabelRevision)
      (Maybe LabelRevision)
  | PatchNablaObjectMismatch GlobalObjectId NablaId
  | PatchDeltaObjectMismatch GlobalObjectId DeltaId
  | PatchStoreActivationRequiresLiveDelta
  | PatchStoreActivationDeltaMismatch DeltaId DeltaId
  | PatchStoreActivationSortUnaffected SortOccurrence
  | PatchMissingObjectControlHold GlobalObjectId
  | PatchMissingStructuralHold StructuralTargetProvenance
  | PatchMissingEffectiveSortHold SortOccurrence
  | PatchMissingProcessLifecycleHold ProcessEpochId
  | PatchStoredZombieNotPermitted ProcessEpochId
  | PatchFirstPriorDeleted GlobalObjectId
  | PatchFirstPriorRevisionPresent LabelRevision
  | PatchGeneratedStoreIdentityInvariant
  | PatchGeneratorLineageMismatch
  | PatchGeneratorPrecedesRetainedAllocation Word64 Word64
  | PatchReleaseMissing LabelDecisionId
  | PatchReleaseDigestMismatch
  | PatchReleaseIndexMustBePositive ControlIndex
  | PatchDecisionIndexMismatch ControlIndex ControlIndex
  | PatchReleaseRevisionDidNotAdvance LabelRevision LabelRevision
  | PatchReleaseConflict ControlIndex ControlIndex
  deriving stock (Eq, Show)

data PreparedLabelPatch = PreparedLabelPatch
  { successorPatchState :: State,
    successorGeneratorState :: IdGenerator.State,
    preparation :: Maybe PendingLabelPreparation,
    disposition :: PatchPreparationDisposition
  }

prepareLabelPatch ::
  PatchSpecification ->
  IdGenerator.State ->
  State ->
  Either PatchProblem PreparedLabelPatch
prepareLabelPatch specification generator state = do
  validateSpecification specification
  case Map.lookup specification.decision state.byDecision of
    Just (PreparedPatch pending generatorPrefix generatorFloor)
      | pendingMatchesSpecification pending specification -> do
          validateReplayGenerator generatorPrefix generatorFloor generator
          Right
            PreparedLabelPatch
              { successorPatchState = state,
                successorGeneratorState = generator,
                preparation = Just pending,
                disposition = PatchPreparationExactReplay
              }
      | otherwise -> Left (PatchDecisionConflict specification.decision)
    Just (InstalledPatch installed generatorPrefix generatorFloor)
      | installedPreparationMatchesSpecification installed specification -> do
          validateReplayGenerator generatorPrefix generatorFloor generator
          Right
            PreparedLabelPatch
              { successorPatchState = state,
                successorGeneratorState = generator,
                preparation = Nothing,
                disposition = PatchPreparationExactReplay
              }
      | otherwise -> Left (PatchDecisionConflict specification.decision)
    Nothing -> prepareFreshPatch specification generator state

-- Generator lineage is fixed-size replay evidence, independent of the discarded
-- per-Store allocation map. An inert retry must still not rewind this owner.
validateReplayGenerator :: ByteString -> Word64 -> IdGenerator.State -> Either PatchProblem ()
validateReplayGenerator generatorPrefix generatorFloor generator = do
  unless
    (IdGenerator.idGeneratorInitialPrefix generator == generatorPrefix)
    (Left PatchGeneratorLineageMismatch)
  let suppliedCounter = generatorCounter generator
  unless
    (suppliedCounter >= generatorFloor)
    (Left (PatchGeneratorPrecedesRetainedAllocation generatorFloor suppliedCounter))

prepareFreshPatch ::
  PatchSpecification ->
  IdGenerator.State ->
  State ->
  Either PatchProblem PreparedLabelPatch
prepareFreshPatch specification generator state = do
  validateObjectPredecessor specification state
  let pending =
        PendingLabelPreparation
          specification.preparedFacts
          specification.preparedDigest
          specification.role
          specification.heldWork
      retained =
        PreparedPatch
          pending
          (IdGenerator.idGeneratorInitialPrefix generator)
          (generatorCounter generator)
      successor =
        state
          { byDecision = Map.insert specification.decision retained state.byDecision,
            currentByObject =
              Map.insert specification.object specification.decision state.currentByObject
          }
  Right
    PreparedLabelPatch
      { successorPatchState = successor,
        successorGeneratorState = generator,
        preparation = Just pending,
        disposition = PatchPreparedFresh
      }

validateSpecification :: PatchSpecification -> Either PatchProblem ()
validateSpecification specification = do
  validateStoredState specification.proposedState
  case specification.role of
    NablaTarget nabla _ ->
      unless
        (globalObjectIdFromNablaId nabla == specification.object)
        (Left (PatchNablaObjectMismatch specification.object nabla))
    DeltaTarget delta _ ->
      unless
        (globalObjectIdFromDeltaId delta == specification.object)
        (Left (PatchDeltaObjectMismatch specification.object delta))
    _ -> Right ()
  unless
    ( HeldObjectControlWork specification.object
        `Set.member` specification.heldWork
    )
    (Left (PatchMissingObjectControlHold specification.object))
  mapM_
    (requireHeld specification.heldWork . HeldStructuralWork specification.object)
    (targetStructuralProvenance specification.role)
  mapM_
    (requireHeld specification.heldWork . HeldEffectiveSortWork)
    (Set.toAscList specification.affectedSorts)
  mapM_
    (requireHeld specification.heldWork . HeldProcessLifecycleWork)
    (requiredProcessHolds specification)
  case Set.toAscList specification.storeActivations of
    [] -> Right ()
    activations -> case ( specification.role,
                          releasedLabelStateView specification.proposedState
                        ) of
      (DeltaTarget targetDelta _, ReleasedLabelView (ProcessLabel _, _)) ->
        mapM_
          (validateActivation targetDelta specification.affectedSorts)
          activations
      _ -> Left PatchStoreActivationRequiresLiveDelta

validateActivation ::
  DeltaId -> Set SortOccurrence -> StoreActivation -> Either PatchProblem ()
validateActivation targetDelta affectedSorts activation = do
  unless
    (activation.delta == targetDelta)
    (Left (PatchStoreActivationDeltaMismatch targetDelta activation.delta))
  unless
    (Set.member activation.sort affectedSorts)
    (Left (PatchStoreActivationSortUnaffected activation.sort))

validateStoredState :: ReleasedLabelState -> Either PatchProblem ()
validateStoredState state = case releasedLabelStateView state of
  ReleasedLabelView (ZombieLabel process, _) ->
    Left (PatchStoredZombieNotPermitted process)
  ReleasedLabelView _ -> Right ()
  ReleasedDeletedView {} -> Right ()

targetStructuralProvenance :: TargetRole -> [StructuralTargetProvenance]
targetStructuralProvenance role = case role of
  AbsentOrdinaryTarget -> []
  OrdinaryControlledTarget -> []
  NeutralTarget provenance -> [provenance]
  NablaTarget _ provenance -> [provenance]
  DeltaTarget _ provenance -> [provenance]
  EdgeTarget provenance -> [provenance]

requiredProcessHolds :: PatchSpecification -> [ProcessEpochId]
requiredProcessHolds specification =
  Set.toAscList
    ( processesInReleasedState specification.expectedPriorState
        <> processesInReleasedState specification.proposedState
    )

processesInReleasedState :: ReleasedLabelState -> Set ProcessEpochId
processesInReleasedState state = case releasedLabelStateView state of
  ReleasedLabelView (ProcessLabel process, _) -> Set.singleton process
  ReleasedLabelView _ -> Set.empty
  ReleasedDeletedView {} -> Set.empty

requireHeld :: Set TargetWorkKey -> TargetWorkKey -> Either PatchProblem ()
requireHeld held key = unless (Set.member key held) (Left (missingHoldProblem key))

missingHoldProblem :: TargetWorkKey -> PatchProblem
missingHoldProblem key = case key of
  HeldStructuralWork _ occurrence -> PatchMissingStructuralHold occurrence
  HeldEffectiveSortWork sort -> PatchMissingEffectiveSortHold sort
  HeldProcessLifecycleWork process -> PatchMissingProcessLifecycleHold process
  HeldObjectControlWork object -> PatchMissingObjectControlHold object
  _ -> error "Eclips.Herald.UseCase.LabelPatch: unsupported mandatory hold kind"

validateObjectPredecessor ::
  PatchSpecification -> State -> Either PatchProblem ()
validateObjectPredecessor specification state =
  case Map.lookup specification.object state.currentByObject of
    Nothing -> case Map.lookup specification.object state.importedCurrentByObject >>= (`Map.lookup` state.importedByDecision) of
      Just imported ->
        validatePrior
          (importedLabelPatchPreparedFacts imported)
          (importedLabelPatchReleaseIndex imported)
      Nothing -> do
        when
          (isDeleted specification.expectedPriorState)
          (Left (PatchFirstPriorDeleted specification.object))
        case specification.expectedPriorRevision of
          Nothing -> Right ()
          Just revision -> Left (PatchFirstPriorRevisionPresent revision)
    Just previousDecision -> case Map.lookup previousDecision state.byDecision of
      Nothing -> Left (PatchObjectBusy specification.object previousDecision)
      Just (PreparedPatch _ _ _) -> Left (PatchObjectBusy specification.object previousDecision)
      Just (InstalledPatch installed _ _) -> validatePrior installed.preparedFacts installed.releaseIndex
  where
    validatePrior facts releaseIndex = do
      let priorState = preparedLabelFactsProposedOutcome facts
          priorRevision = either (const Nothing) Just (mkLabelRevision releaseIndex)
      when
        (isDeleted priorState)
        (Left (PatchObjectTerminallyDeleted specification.object))
      unless
        (matchesEffectivePrior priorState specification.expectedPriorState)
        (Left (PatchPriorStateMismatch priorState specification.expectedPriorState))
      unless
        (specification.expectedPriorRevision == priorRevision)
        (Left (PatchPriorRevisionMismatch priorRevision specification.expectedPriorRevision))

    -- End changes the effective label without changing the stored tenure or its
    -- revision. Controlled authenticates that End and the complete effective
    -- prior in the enclosing atomic release before any successor is exposed.
    matchesEffectivePrior :: ReleasedLabelState -> ReleasedLabelState -> Bool
    matchesEffectivePrior stored expected
      | stored == expected = True
      | otherwise = case (releasedLabelStateView stored, releasedLabelStateView expected) of
          (ReleasedLabelView (ProcessLabel process, generation), ReleasedLabelView (ZombieLabel ended, expectedGeneration)) ->
            process == ended && generation == expectedGeneration
          _ -> False

preallocateStores ::
  IdGenerator.State ->
  [StoreActivation] ->
  Either
    PatchProblem
    (IdGenerator.State, Map StoreActivation StoreIncarnationId)
preallocateStores = foldM allocateOne . (,Map.empty)
  where
    allocateOne (generator, stores) activation = do
      prepared <-
        either
          (const (Left PatchGeneratedStoreIdentityInvariant))
          Right
          (IdGenerator.prepareGeneratedId generator)
      incarnation <-
        either
          (const (Left PatchGeneratedStoreIdentityInvariant))
          Right
          ( mkStoreIncarnationId
              (globalUniqueIdBytes (IdGenerator.preparedGlobalUniqueId prepared))
          )
      Right
        ( IdGenerator.commitGeneratedId prepared,
          Map.insert activation incarnation stores
        )

preparedLabelPatchPreparation :: PreparedLabelPatch -> Maybe PendingLabelPreparation
preparedLabelPatchPreparation prepared = prepared.preparation

preparedLabelPatchDisposition ::
  PreparedLabelPatch -> PatchPreparationDisposition
preparedLabelPatchDisposition prepared = prepared.disposition

commitLabelPatch :: PreparedLabelPatch -> (State, IdGenerator.State)
commitLabelPatch prepared =
  (prepared.successorPatchState, prepared.successorGeneratorState)

-- | One atomic owner transition. The recipe is transient output consumed by
-- the composed direct-effect applicator; it is never retained in State.
data PreparedPatchRelease = PreparedPatchRelease
  { successor :: State,
    successorGenerator :: IdGenerator.State,
    recipe :: Maybe LabelPatchRecipe,
    installation :: InstalledLabel
  }

preparePatchRelease ::
  PatchSpecification ->
  ControlIndex ->
  IdGenerator.State ->
  State ->
  Either PatchProblem PreparedPatchRelease
preparePatchRelease specification releaseIndex generator state = do
  let decision = specification.decision
      digest = specification.preparedDigest
  retained <-
    maybe
      (Left (PatchReleaseMissing decision))
      Right
      (Map.lookup decision state.byDecision)
  let (facts, retainedDigest, generatorPrefix, generatorFloor) = case retained of
        PreparedPatch pending prefix floorCounter -> (pending.preparedFacts, pending.preparedDigest, prefix, floorCounter)
        InstalledPatch installed prefix floorCounter -> (installed.preparedFacts, installed.preparedDigest, prefix, floorCounter)
  unless (retainedDigest == digest) (Left PatchReleaseDigestMismatch)
  unless (facts == specification.preparedFacts) (Left (PatchDecisionConflict decision))
  validateReplayGenerator generatorPrefix generatorFloor generator
  releaseRevision <-
    either
      (const (Left (PatchReleaseIndexMustBePositive releaseIndex)))
      Right
      (mkLabelRevision releaseIndex)
  let resolveIndex = preparedLabelFactsResolveIndex facts
  unless
    (releaseIndex == resolveIndex)
    (Left (PatchDecisionIndexMismatch resolveIndex releaseIndex))
  case preparedLabelFactsExpectedPriorRevision facts of
    Just priorRevision ->
      unless
        (releaseRevision > priorRevision)
        (Left (PatchReleaseRevisionDidNotAdvance priorRevision releaseRevision))
    Nothing -> Right ()
  case retained of
    PreparedPatch pending _ _ -> do
      validateSpecification specification
      (successorGenerator, stores) <-
        preallocateStores generator (Set.toAscList specification.storeActivations)
      let recipe = LabelPatchRecipe specification (patchForm specification.role specification.proposedState) stores
          installed = InstalledLabel facts digest specification.role pending.role releaseIndex
          successor = state {byDecision = Map.insert decision (InstalledPatch installed generatorPrefix (generatorCounter successorGenerator)) state.byDecision}
      Right PreparedPatchRelease {successor, successorGenerator, recipe = Just recipe, installation = installed}
    InstalledPatch installed _ _
      | installed.releaseIndex == releaseIndex ->
          Right PreparedPatchRelease {successor = state, successorGenerator = generator, recipe = Nothing, installation = installed}
      | otherwise -> Left (PatchReleaseConflict installed.releaseIndex releaseIndex)

-- | Present only on the transition that installs the current local patch.
-- Exact installation replay retains proof without recreating work or allocating IDs.
preparedReleasedPatchRecipe :: PreparedPatchRelease -> Maybe LabelPatchRecipe
preparedReleasedPatchRecipe prepared = prepared.recipe

preparedReleasedPatchInstallation :: PreparedPatchRelease -> InstalledLabel
preparedReleasedPatchInstallation prepared = prepared.installation

commitPatchRelease :: PreparedPatchRelease -> (State, IdGenerator.State)
commitPatchRelease prepared = (prepared.successor, prepared.successorGenerator)

generatorCounter :: IdGenerator.State -> Word64
generatorCounter =
  IdGenerator.witnessedNextGeneratorCounter
    . IdGenerator.idGeneratorStateWitness

patchForm :: TargetRole -> ReleasedLabelState -> PatchForm
patchForm role proposed = case (role, isDeleted proposed) of
  (AbsentOrdinaryTarget, False) -> InstallFutureReleasedOverlay
  (AbsentOrdinaryTarget, True) -> InstallFutureTerminalSuppression
  (OrdinaryControlledTarget, False) -> OverlayOrdinaryControlledObject
  (OrdinaryControlledTarget, True) -> DeleteOrdinaryControlledObject
  (NeutralTarget _, False) -> OverlayNeutralVertex
  (NeutralTarget _, True) -> DeleteNeutralVertex
  (NablaTarget _ _, False) -> RelabelNablaProjection
  (NablaTarget _ _, True) -> DeleteNablaProjection
  (DeltaTarget _ _, False) -> RelabelDeltaProjection
  (DeltaTarget _ _, True) -> DeleteDeltaProjection
  (EdgeTarget _, False) -> OverlayEdge
  (EdgeTarget _, True) -> DeleteEdge

isDeleted :: ReleasedLabelState -> Bool
isDeleted state = case releasedLabelStateView state of
  ReleasedDeletedView {} -> True
  ReleasedLabelView _ -> False
