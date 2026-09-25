{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Non-committing preparation of the four direct structural carriers.
--
-- This module deliberately owns no live Herald state.  It computes a checked,
-- exact patch from an occurrence-qualified applied-history projection.  The
-- potentially ahead application-side 'Controlled' winner is not an input.
-- The Increment-4 coordinator authenticates an occurrence and atomically
-- interprets this prepared product with its vector advance and stream completion.
module Eclips.Herald.Structural.Reconciliation
  ( StructuralDependency (..),
    FreshStoreIncarnationSource (..),
    FreshStoreIncarnationEvidence,
    freshStoreIncarnationEvidence,
    freshStoreIncarnationSource,
    freshStoreIncarnationDelta,
    freshStoreIncarnationId,
    ReconciliationViews,
    reconciliationViews,
    reconciliationViewsWithEndedProcesses,
    withReconciliationDiagnosticChecks,
    reconciliationDiagnosticChecks,
    reconciliationLocalHerald,
    StructuralApplication,
    structuralApplication,
    structuralApplicationOccurrence,
    structuralApplicationPredecessor,
    structuralApplicationCarrierSort,
    structuralApplicationPublication,
    structuralApplicationControlPrerequisite,
    StructuralProjectionProvenance (..),
    structuralProjectionProvenanceOccurrence,
    structuralProjectionProvenancePublication,
    BaselineStructuralWinner,
    baselineStructuralWinner,
    DynamicLocalStoreSpec,
    dynamicLocalStoreSpec,
    dynamicStoreProcess,
    dynamicStoreController,
    dynamicStoreHerald,
    dynamicStoreDelta,
    dynamicStoreSort,
    dynamicStoreIncarnation,
    dynamicStoreIncarnationSource,
    StructuralAppliedState,
    emptyStructuralAppliedState,
    structuralAppliedStateWithGenesis,
    structuralAppliedStateWithStores,
    structuralAppliedStateWithBaseline,
    structuralAppliedMembershipGenerationId,
    structuralAppliedMemberSetDigest,
    structuralAppliedFixedMembership,
    rebaseStructuralAppliedMembership,
    establishStructuralAppliedMembership,
    establishStructuralAdmissionMembership,
    structuralAppliedHasOccurrence,
    StructuralFrontierSummary,
    prepareStructuralFrontierSummary,
    structuralFrontierPredecessors,
    structuralFrontierControlPrerequisite,
    validateStructuralFrontier,
    validateStructuralFrontierExhaustive,
    StructuralRetirementSuppression (..),
    structuralRetirementSuppressionControlIndex,
    structuralRetirementSuppressionControlPrerequisite,
    structuralAppliedRetirementSuppressionAt,
    structuralAppliedRetirementSuppressions,
    structuralAppliedControlledProjectionAtOccurrence,
    structuralAppliedHistoryOccurrences,
    structuralAppliedRetainedObjectRoles,
    structuralAppliedLocalStores,
    restoreStructuralLocalStoreForInvariantTest,
    structuralAppliedRetainedStoreResources,
    structuralAppliedOccurrenceStoreResources,
    structuralAppliedBaselineStoreMatches,
    AppliedDynamicDeltaCoordinate,
    appliedDynamicDeltaOccurrence,
    appliedDynamicDeltaObject,
    appliedDynamicDeltaDelta,
    appliedDynamicDeltaSort,
    appliedDynamicDeltaController,
    appliedDynamicDeltaControlPrerequisite,
    structuralAppliedDynamicDeltaCoordinates,
    AppliedDeltaRouteCoordinate,
    appliedDeltaRouteProvenance,
    appliedDeltaRouteObject,
    appliedDeltaRouteDelta,
    appliedDeltaRouteSort,
    appliedDeltaRouteController,
    appliedDeltaRouteControlPrerequisite,
    structuralAppliedDeltaRouteCoordinates,
    structuralAppliedObjectHasControlHistory,
    PreparedDeltaRouteAdmission,
    prepareDeltaRouteAdmission,
    preparedDeltaRouteIsValid,
    preparedDeltaRouteLatestPrerequisite,
    preparedDeltaRouteObjectHasControlHistory,
    structuralAppliedUsedStoreIncarnations,
    StructuralControlOverlayView (..),
    structuralAppliedControlOverlays,
    structuralAppliedControlHistory,
    StructuralCarrierBase,
    captureStructuralCarrierBase,
    StructuralCarrierBaseContext,
    StructuralCarrierBaseCodecProblem (..),
    structuralCarrierBaseContext,
    encodeStructuralCarrierBase,
    decodeStructuralCarrierBase,
    structuralCarrierBaseControlBase,
    structuralCarrierBasePublications,
    structuralCarrierBaseApplications,
    PreparedStructuralCarrierBaseImport,
    prepareStructuralCarrierBaseImport,
    preparedStructuralCarrierBasePredecessor,
    preparedStructuralCarrierBaseSuccessor,
    preparedStructuralCarrierBaseGraphPatch,
    StructuralControlBase,
    captureStructuralControlBase,
    restrictStructuralControlBaseToObjects,
    StructuralControlBasePreparation (..),
    StructuralControlBaseStep,
    structuralControlBaseStepCause,
    structuralControlBaseStepPreparation,
    prepareStructuralControlBaseImport,
    structuralControlBaseOverlays,
    structuralControlBaseHistory,
    structuralControlBaseObjects,
    structuralControlBaseControlIndices,
    structuralControlBaseGreatestControlIndex,
    StructuralAppliedInvariantProblem (..),
    validateStructuralAppliedState,
    relocateStructuralControlHistoryForInvariantTest,
    dropStructuralControlOverlayForInvariantTest,
    structuralControlCheckpointAt,
    HistoricalNablaProjection,
    historicalNablaProjectionProvenance,
    historicalNablaProjectionController,
    historicalNablaProjectionSort,
    historicalNablaProjectionControlPrerequisite,
    structuralNablaProjectionAt,
    PreparedNablaProjections,
    prepareNablaProjectionsAt,
    lookupPreparedNablaProjection,
    structuralAppliedVertexProjections,
    structuralAppliedEdgeProjections,
    structuralAppliedVertexProjectionAt,
    structuralAppliedEdgeProjectionAt,
    StructuralProjectionSnapshot,
    structuralProjectionSnapshotVertices,
    structuralProjectionSnapshotAdmissionVertices,
    structuralProjectionSnapshotEdges,
    StructuralProjectionDigestEntry,
    structuralProjectionDigestEntryControlled,
    structuralProjectionDigestEntryCarrierSort,
    structuralProjectionDigestEntryControlPrerequisite,
    structuralProjectionDigestEntryControlCause,
    structuralProjectionDigestEntryVertex,
    structuralProjectionDigestEntryEdge,
    structuralProjectionSnapshotDigestEntries,
    structuralProjectionSnapshotDeletedControls,
    structuralProjectionSnapshotOccurrencesOnly,
    structuralProjectionSnapshotCanonicalBytes,
    prepareStructuralProjectionAtVector,
    prepareStructuralProjectionAtAdmittedVector,
    StructuralProjectionInputs,
    prepareStructuralProjectionInputs,
    structuralProjectionControlCoordinate,
    ControllerProjection (..),
    RootActivityProjection (..),
    StructuralVertexProjection (..),
    structuralVertexProjectionProvenance,
    structuralVertexProjectionOccurrence,
    structuralVertexProjectionPublication,
    structuralVertexProjectionVertex,
    OwnedEdgeProjection,
    ownedEdgeProjectionProvenance,
    ownedEdgeProjectionOccurrence,
    ownedEdgeProjectionPublication,
    ownedEdgeProjectionObject,
    ownedEdgeProjectionPayload,
    AppliedControlledProjection,
    appliedControlledProjectionProvenance,
    appliedControlledProjectionObject,
    appliedControlledProjectionOccurrence,
    appliedControlledProjectionPublication,
    appliedControlledProjectionRole,
    appliedControlledProjectionLifecycle,
    appliedControlledProjectionSuppression,
    ExactChange,
    exactChangeBefore,
    exactChangeAfter,
    ControlledPatch,
    controlledPatchObservation,
    controlledPatchWinnerChange,
    StructuralGraphPatch,
    structuralGraphVertexChanges,
    structuralGraphEdgeChanges,
    structuralGraphPatchNull,
    PlacementPatch,
    placementPatchChanges,
    StorePatch,
    storePatchChanges,
    storePatchRetainedResourceChanges,
    StructuralApplicationClassification (..),
    PreparedStructuralReconciliation,
    preparedStructuralClassification,
    preparedStructuralChangesState,
    preparedStructuralApplication,
    preparedStructuralControlledPatch,
    preparedStructuralGraphPatch,
    preparedStructuralPlacementPatch,
    preparedStructuralStorePatch,
    preparedStructuralDebts,
    preparedStructuralPredecessor,
    preparedStructuralSuccessor,
    preparedStructuralObservedCarrierRole,
    preparedStructuralSatisfiedDependencies,
    StructuralPreparation (..),
    StructuralReconciliationProblem (..),
    prepareStructuralReconciliation,
    prepareStructuralReconciliationAtAdmittedPredecessor,
    prepareRetirementSuppressedStructuralReconciliation,
    prepareTerminallySuppressedStructuralReconciliation,
    prepareCarriedStructuralReconciliation,
    prepareRetirementSuppressedCarriedStructuralReconciliation,
    prepareTerminallySuppressedCarriedStructuralReconciliation,
    prepareStructuralProjectionRefresh,
    prepareUnchangedStructuralLabelControlRefresh,
    prepareStructuralLabelControlRefresh,
    StructuralControlTarget (..),
    prepareStructuralProcessEndRefreshes,
  )
where

import Control.Monad (foldM, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Serialize qualified as Codec
import Data.Serialize.Put qualified as Serialize
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word64, Word8)
import Eclips.Domain.Disappearance qualified as Disappearance
import Eclips.Domain.Graph
  ( EdgePayload,
    EdgeStrength,
    VertexId (..),
    edgeDestination,
    edgePayload,
    edgeSource,
    edgeStrength,
    edgeStrengthFromSymbol,
    edgeStrengthTranscriptTag,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    NablaId,
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
    StructuralOccurrenceId,
    StructuralSequence,
    SystemId,
    authorityEpochCanonicalBytes,
    controlIndex,
    controlIndexWord64,
    deltaIdBytes,
    deltaIdFromGlobalObjectId,
    globalObjectIdBytes,
    globalObjectIdFromGlobalUniqueId,
    globalObjectIdFromNablaId,
    heraldEpochBytes,
    mkSortId,
    mkStructuralSequence,
    nablaIdBytes,
    nablaIdFromGlobalObjectId,
    nablaSequenceWord64,
    processEpochIdBytes,
    publicationAuthorityEpoch,
    publicationNabla,
    publicationNablaSequence,
    publicationSourceHeraldEpoch,
    sortDefinitionOccurrenceIdBytes,
    sortIdBytes,
    structuralOccurrenceId,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
    structuralSequenceWord64,
  )
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Label
  ( ReleasedLabelState,
    ReleasedLabelStateView (..),
    releasedLabelStateView,
  )
import Eclips.Domain.MemberSet (MemberSetDigest)
import Eclips.Domain.Membership
  ( HeraldMembershipGenerationId,
    HeraldMembershipLineage,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    heraldMembershipLineageFrom,
    heraldMembershipLineageOrigin,
    heraldMembershipLineageTarget,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Publication
  ( CheckedPublication,
    PublicationWinnerKey,
    checkedPublicationId,
    checkedPublicationLifecycle,
    checkedPublicationSort,
    checkedPublicationValue,
    checkedPublicationWinnerKey,
  )
import Eclips.Domain.Publication qualified as Publication
import Eclips.Domain.Sort.Descriptor
  ( Lifecycle (..),
    StructuralCarrierRole (..),
    structuralCarrierRoleTag,
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (..),
    predefinedCatalogueSortId,
    profileEntryFor,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    AppliedRootRole (..),
    appliedProcessEnvironmentPublications,
    appliedProcessEpochId,
    appliedProcessRoots,
    appliedRootObjectId,
    appliedRootOccurrenceId,
    appliedRootPlacement,
    appliedRootRole,
    appliedRootSortId,
  )
import Eclips.Domain.Structural
  ( StructuralVersionVector,
    projectStructuralVersionVector,
    structuralPredecessorPrefix,
    structuralPrefixSequence,
    structuralPrefixThrough,
    structuralVersionVectorComponent,
    structuralVersionVectorCovers,
    structuralVersionVectorEntries,
    structuralVersionVectorMemberSetDigest,
    structuralVersionVectorMembershipGenerationId,
  )
import Eclips.Domain.Structural qualified as Structural
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    processEndCause,
    structuralConsequenceCauseCanonicalBytes,
    structuralConsequenceCauseControlIndex,
    structuralOccurrenceCause,
  )
import Eclips.Domain.StructuralConsequence qualified as Consequence
import Eclips.Domain.Topology qualified as Topology
import Eclips.Domain.Value
  ( FieldName,
    Label,
    LabelOwner (..),
    Value,
    ValueView (..),
    directProjection,
    mkFieldName,
    valueAt,
    viewValue,
  )
import Eclips.Domain.Value qualified as Value
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks (..), runDiagnosticCheck)
import Eclips.Herald.Graph.TerminalSource
  ( CarriedStampedSurvivor,
    SuccessorStructuralBase,
    carriedStampedSurvivorPredecessor,
    carriedStampedSurvivorStamp,
    successorStructuralBaseEstablishedUnion,
    successorStructuralBaseGenerationId,
    successorStructuralBaseLineage,
    terminalSourceUnionEstablishedUnion,
    terminalSourceUnionSuccessorInitialVector,
    terminalSourceUnionTerminalPredecessorVector,
  )
import Eclips.Herald.Graph.TerminalSource qualified as Terminal
import Eclips.Herald.Join.Bootstrap qualified as JoinBootstrap
import Eclips.Herald.PeerPublication
  ( structuralOccurrenceStampOccurrence,
    structuralOccurrenceStampPredecessor,
    structuralOccurrenceStampPublication,
  )
import Eclips.Herald.PeerPublication qualified as Peer
import Eclips.Herald.SortRegistry.State qualified as Registry
import Eclips.Herald.Structural.Debt
  ( QualifiedPlacementFact,
    QualifiedStoreIncarnation,
    SortOccurrence,
    StructuralDebtKind (..),
    StructuralDebtSet,
    StructuralIdentity (..),
    normalizeStructuralDebts,
    qualifiedPlacementFact,
    qualifiedStoreIncarnation,
    sortOccurrence,
    sortOccurrenceDefinition,
    sortOccurrenceSortId,
    structuralConsequenceDebt,
    structuralDebtEvidence,
    structuralDebtKey,
  )

-- | A semantic prerequisite which can make the same retained occurrence ready
-- later.  Held preparation produces no partial owner mutation.
data StructuralDependency
  = EffectiveSortDependency SortId
  | StructuralEndpointDependency GlobalObjectId
  | StructuralControlCarrierDependency GlobalObjectId
  | StructuralProcessDependency ProcessEpochId
  | FreshStoreIncarnationDependency DeltaId
  | StructuralPredecessorDependency StructuralOccurrenceId
  deriving stock (Eq, Ord, Show)

data FreshStoreIncarnationSource
  = ConfiguredManifestStoreIncarnation
  | GeneratedStoreIncarnation
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Explicit evidence supplied by the manifest or generated-ID owner.  The
-- reconciliation code never derives an incarnation from a delta identity.
data FreshStoreIncarnationEvidence
  = FreshStoreIncarnationEvidence
      FreshStoreIncarnationSource
      DeltaId
      StoreIncarnationId
  deriving stock (Eq, Show)

freshStoreIncarnationEvidence ::
  FreshStoreIncarnationSource ->
  DeltaId ->
  StoreIncarnationId ->
  FreshStoreIncarnationEvidence
freshStoreIncarnationEvidence = FreshStoreIncarnationEvidence

freshStoreIncarnationSource ::
  FreshStoreIncarnationEvidence -> FreshStoreIncarnationSource
freshStoreIncarnationSource (FreshStoreIncarnationEvidence source _ _) = source

freshStoreIncarnationDelta :: FreshStoreIncarnationEvidence -> DeltaId
freshStoreIncarnationDelta (FreshStoreIncarnationEvidence _ delta _) = delta

freshStoreIncarnationId :: FreshStoreIncarnationEvidence -> StoreIncarnationId
freshStoreIncarnationId (FreshStoreIncarnationEvidence _ _ incarnation) = incarnation

-- | Read-only applied facts.  Process residence and possession are kept
-- separate so activity is derived rather than accepted as a caller Boolean.
data ReconciliationViews = ReconciliationViews
  { localHerald :: HeraldEpoch,
    effectiveSorts :: Map SortId SortOccurrence,
    baselineVertices :: Set VertexId,
    liveProcessResidences :: Map ProcessEpochId HeraldEpoch,
    endedProcesses :: Map ProcessEpochId (HeraldEpoch, ControlIndex),
    normalPossessions :: Set (ProcessEpochId, GlobalObjectId),
    diagnosticChecks :: !DiagnosticChecks
  }
  deriving stock (Eq, Show)

reconciliationViews ::
  HeraldEpoch ->
  Map SortId SortOccurrence ->
  Set VertexId ->
  Map ProcessEpochId HeraldEpoch ->
  Set (ProcessEpochId, GlobalObjectId) ->
  ReconciliationViews
reconciliationViews local sorts vertices processes possessions =
  ReconciliationViews
    { localHerald = local,
      effectiveSorts = sorts,
      baselineVertices = vertices,
      liveProcessResidences = processes,
      endedProcesses = Map.empty,
      normalPossessions = possessions,
      diagnosticChecks = DiagnosticChecksEnabled
    }

-- | Retain immutable residence and the canonical End position even after a
-- process leaves the live projection.  A delayed carrier still describes its
-- original controller; End is a separately ordered projection overlay.
reconciliationViewsWithEndedProcesses ::
  Map ProcessEpochId (HeraldEpoch, ControlIndex) ->
  ReconciliationViews ->
  ReconciliationViews
reconciliationViewsWithEndedProcesses ended views = views {endedProcesses = ended}

-- | Choose whether projections at already admitted frontiers repeat their
-- coverage and closure checks. Admission of new work remains fully checked.
withReconciliationDiagnosticChecks :: DiagnosticChecks -> ReconciliationViews -> ReconciliationViews
withReconciliationDiagnosticChecks checks views = views {diagnosticChecks = checks}

reconciliationDiagnosticChecks :: ReconciliationViews -> DiagnosticChecks
reconciliationDiagnosticChecks views = views.diagnosticChecks

reconciliationLocalHerald :: ReconciliationViews -> HeraldEpoch
reconciliationLocalHerald views = views.localHerald

-- | One authenticated occurrence's already reconstructed publication.  The
-- carrier sort occurrence comes from the effective carrier definition, not
-- from the carried nabla/delta data-sort reference.
data StructuralApplication = StructuralApplication
  { occurrence :: StructuralOccurrenceId,
    predecessor :: StructuralVersionVector,
    carrierSort :: SortOccurrence,
    publication :: CheckedPublication,
    controlPrerequisite :: ControlIndex
  }
  deriving stock (Eq, Show)

structuralApplication ::
  StructuralOccurrenceId ->
  StructuralVersionVector ->
  SortOccurrence ->
  CheckedPublication ->
  ControlIndex ->
  StructuralApplication
structuralApplication = StructuralApplication

structuralApplicationOccurrence :: StructuralApplication -> StructuralOccurrenceId
structuralApplicationOccurrence application = application.occurrence

structuralApplicationPredecessor ::
  StructuralApplication -> StructuralVersionVector
structuralApplicationPredecessor application = application.predecessor

structuralApplicationCarrierSort :: StructuralApplication -> SortOccurrence
structuralApplicationCarrierSort application = application.carrierSort

structuralApplicationPublication :: StructuralApplication -> CheckedPublication
structuralApplicationPublication application = application.publication

structuralApplicationControlPrerequisite ::
  StructuralApplication -> ControlIndex
structuralApplicationControlPrerequisite application =
  application.controlPrerequisite

-- | Provenance of a current structural projection. Checked genesis material
-- has a canonical object identity and publication but no vector dot; an
-- explicitly imported baseline is identified by its publication; only the
-- third arm carries a dynamic occurrence. Control refreshes retain the
-- original provenance instead of fabricating an index-zero vector dot.
data StructuralProjectionProvenance
  = GenesisStructuralProvenance GlobalObjectId
  | BaselineStructuralProvenance PublicationId
  | OccurrenceStructuralProvenance StructuralOccurrenceId PublicationId
  deriving stock (Eq, Ord, Show)

structuralProjectionProvenanceOccurrence ::
  StructuralProjectionProvenance -> Maybe StructuralOccurrenceId
structuralProjectionProvenanceOccurrence = \case
  GenesisStructuralProvenance _ -> Nothing
  BaselineStructuralProvenance _ -> Nothing
  OccurrenceStructuralProvenance occurrence _ -> Just occurrence

structuralProjectionProvenancePublication ::
  StructuralProjectionProvenance -> Maybe PublicationId
structuralProjectionProvenancePublication = \case
  GenesisStructuralProvenance _ -> Nothing
  BaselineStructuralProvenance publication -> Just publication
  OccurrenceStructuralProvenance _ publication -> Just publication

-- | Exact local destination semantics which existing Store provenance cannot
-- yet express for an arbitrary dynamically published delta.
data DynamicLocalStoreSpec = DynamicLocalStoreSpec
  { process :: ProcessEpochId,
    controller :: GlobalObjectId,
    herald :: HeraldEpoch,
    delta :: DeltaId,
    sort :: SortOccurrence,
    incarnation :: StoreIncarnationId,
    incarnationSource :: FreshStoreIncarnationSource
  }
  deriving stock (Eq, Ord, Show)

dynamicLocalStoreSpec ::
  ProcessEpochId ->
  GlobalObjectId ->
  HeraldEpoch ->
  DeltaId ->
  SortOccurrence ->
  StoreIncarnationId ->
  FreshStoreIncarnationSource ->
  DynamicLocalStoreSpec
dynamicLocalStoreSpec = DynamicLocalStoreSpec

dynamicStoreProcess :: DynamicLocalStoreSpec -> ProcessEpochId
dynamicStoreProcess store = store.process

dynamicStoreController :: DynamicLocalStoreSpec -> GlobalObjectId
dynamicStoreController store = store.controller

dynamicStoreHerald :: DynamicLocalStoreSpec -> HeraldEpoch
dynamicStoreHerald store = store.herald

dynamicStoreDelta :: DynamicLocalStoreSpec -> DeltaId
dynamicStoreDelta store = store.delta

dynamicStoreSort :: DynamicLocalStoreSpec -> SortOccurrence
dynamicStoreSort store = store.sort

dynamicStoreIncarnation :: DynamicLocalStoreSpec -> StoreIncarnationId
dynamicStoreIncarnation store = store.incarnation

dynamicStoreIncarnationSource ::
  DynamicLocalStoreSpec -> FreshStoreIncarnationSource
dynamicStoreIncarnationSource store = store.incarnationSource

data ControllerProjection
  = VoidController
  | LiveProcessController ProcessEpochId HeraldEpoch
  | ZombieProcessController ProcessEpochId
  deriving stock (Eq, Ord, Show)

-- | Host-local activity.  A remote live controller is not claimed active from
-- local possession; its host's exact placement advertisement is authoritative.
data RootActivityProjection
  = PassiveRoot
  | ActiveRootHere ProcessEpochId
  | RemoteController ProcessEpochId HeraldEpoch
  deriving stock (Eq, Ord, Show)

data StructuralVertexProjection
  = NeutralVertexProjection
      StructuralProjectionProvenance
      GlobalObjectId
  | NablaVertexProjection
      StructuralProjectionProvenance
      NablaId
      SortOccurrence
      ControllerProjection
      RootActivityProjection
  | DeltaVertexProjection
      StructuralProjectionProvenance
      DeltaId
      SortOccurrence
      ControllerProjection
      RootActivityProjection
  deriving stock (Eq, Ord, Show)

structuralVertexProjectionProvenance ::
  StructuralVertexProjection -> StructuralProjectionProvenance
structuralVertexProjectionProvenance = \case
  NeutralVertexProjection provenance _ -> provenance
  NablaVertexProjection provenance _ _ _ _ -> provenance
  DeltaVertexProjection provenance _ _ _ _ -> provenance

structuralVertexProjectionOccurrence ::
  StructuralVertexProjection -> Maybe StructuralOccurrenceId
structuralVertexProjectionOccurrence = \case
  NeutralVertexProjection provenance _ -> structuralProjectionProvenanceOccurrence provenance
  NablaVertexProjection provenance _ _ _ _ -> structuralProjectionProvenanceOccurrence provenance
  DeltaVertexProjection provenance _ _ _ _ -> structuralProjectionProvenanceOccurrence provenance

structuralVertexProjectionPublication ::
  StructuralVertexProjection -> Maybe PublicationId
structuralVertexProjectionPublication = \case
  NeutralVertexProjection provenance _ -> structuralProjectionProvenancePublication provenance
  NablaVertexProjection provenance _ _ _ _ -> structuralProjectionProvenancePublication provenance
  DeltaVertexProjection provenance _ _ _ _ -> structuralProjectionProvenancePublication provenance

structuralVertexProjectionVertex :: StructuralVertexProjection -> VertexId
structuralVertexProjectionVertex = \case
  NeutralVertexProjection _ object -> NeutralVertex object
  NablaVertexProjection _ nabla _ _ _ -> NablaVertex nabla
  DeltaVertexProjection _ delta _ _ _ -> DeltaVertex delta

data OwnedEdgeProjection = OwnedEdgeProjection
  { provenance :: StructuralProjectionProvenance,
    object :: GlobalObjectId,
    payload :: EdgePayload
  }
  deriving stock (Eq, Ord, Show)

ownedEdgeProjectionProvenance ::
  OwnedEdgeProjection -> StructuralProjectionProvenance
ownedEdgeProjectionProvenance edge = edge.provenance

ownedEdgeProjectionOccurrence ::
  OwnedEdgeProjection -> Maybe StructuralOccurrenceId
ownedEdgeProjectionOccurrence edge =
  structuralProjectionProvenanceOccurrence edge.provenance

ownedEdgeProjectionPublication :: OwnedEdgeProjection -> Maybe PublicationId
ownedEdgeProjectionPublication edge =
  structuralProjectionProvenancePublication edge.provenance

ownedEdgeProjectionObject :: OwnedEdgeProjection -> GlobalObjectId
ownedEdgeProjectionObject edge = edge.object

ownedEdgeProjectionPayload :: OwnedEdgeProjection -> EdgePayload
ownedEdgeProjectionPayload edge = edge.payload

data AppliedControlledProjection = AppliedControlledProjection
  { object :: GlobalObjectId,
    provenance :: StructuralProjectionProvenance,
    role :: StructuralCarrierRole,
    lifecycle :: Lifecycle,
    suppression :: Maybe PublicationWinnerKey
  }
  deriving stock (Eq, Show)

appliedControlledProjectionObject :: AppliedControlledProjection -> GlobalObjectId
appliedControlledProjectionObject projection = projection.object

appliedControlledProjectionProvenance ::
  AppliedControlledProjection -> StructuralProjectionProvenance
appliedControlledProjectionProvenance projection = projection.provenance

appliedControlledProjectionOccurrence ::
  AppliedControlledProjection -> Maybe StructuralOccurrenceId
appliedControlledProjectionOccurrence projection =
  structuralProjectionProvenanceOccurrence projection.provenance

appliedControlledProjectionPublication :: AppliedControlledProjection -> Maybe PublicationId
appliedControlledProjectionPublication projection =
  structuralProjectionProvenancePublication projection.provenance

appliedControlledProjectionRole :: AppliedControlledProjection -> StructuralCarrierRole
appliedControlledProjectionRole projection = projection.role

appliedControlledProjectionLifecycle :: AppliedControlledProjection -> Lifecycle
appliedControlledProjectionLifecycle projection = projection.lifecycle

appliedControlledProjectionSuppression ::
  AppliedControlledProjection -> Maybe PublicationWinnerKey
appliedControlledProjectionSuppression projection = projection.suppression

data ExactChange value = ExactChange
  { before :: Maybe value,
    after :: Maybe value
  }
  deriving stock (Eq, Show)

exactChangeBefore :: ExactChange value -> Maybe value
exactChangeBefore change = change.before

exactChangeAfter :: ExactChange value -> Maybe value
exactChangeAfter change = change.after

data ControlledPatch = ControlledPatch
  { observation :: Maybe StructuralApplication,
    winnerChange :: Maybe (ExactChange AppliedControlledProjection)
  }
  deriving stock (Eq, Show)

controlledPatchObservation :: ControlledPatch -> Maybe StructuralApplication
controlledPatchObservation patch = patch.observation

controlledPatchWinnerChange ::
  ControlledPatch -> Maybe (ExactChange AppliedControlledProjection)
controlledPatchWinnerChange patch = patch.winnerChange

data StructuralGraphPatch = StructuralGraphPatch
  { vertexChanges :: Map GlobalObjectId (ExactChange StructuralVertexProjection),
    edgeChanges :: Map GlobalObjectId (ExactChange OwnedEdgeProjection)
  }
  deriving stock (Eq, Show)

structuralGraphVertexChanges ::
  StructuralGraphPatch -> Map GlobalObjectId (ExactChange StructuralVertexProjection)
structuralGraphVertexChanges patch = patch.vertexChanges

structuralGraphEdgeChanges ::
  StructuralGraphPatch -> Map GlobalObjectId (ExactChange OwnedEdgeProjection)
structuralGraphEdgeChanges patch = patch.edgeChanges

structuralGraphPatchNull :: StructuralGraphPatch -> Bool
structuralGraphPatchNull patch = Map.null patch.vertexChanges && Map.null patch.edgeChanges

newtype PlacementPatch
  = PlacementPatch
      (Map DeltaId (ExactChange DynamicLocalStoreSpec))
  deriving stock (Eq, Show)

placementPatchChanges :: PlacementPatch -> Map DeltaId (ExactChange DynamicLocalStoreSpec)
placementPatchChanges (PlacementPatch changes) = changes

data StorePatch = StorePatch
  { activeChanges :: Map DeltaId (ExactChange DynamicLocalStoreSpec),
    retainedResourceChanges ::
      Map StoreIncarnationId (ExactChange DynamicLocalStoreSpec)
  }
  deriving stock (Eq, Show)

storePatchChanges :: StorePatch -> Map DeltaId (ExactChange DynamicLocalStoreSpec)
storePatchChanges patch = patch.activeChanges

-- | Exact resource-reservation mutations, including resources retained for a
-- causally winning occurrence which is already hidden at the full-current
-- cut.  These are not active Placement changes.
storePatchRetainedResourceChanges ::
  StorePatch -> Map StoreIncarnationId (ExactChange DynamicLocalStoreSpec)
storePatchRetainedResourceChanges patch = patch.retainedResourceChanges

data CheckedStructuralMeaning
  = CheckedNeutral GlobalObjectId
  | CheckedEdge GlobalObjectId EdgePayload
  | CheckedNabla
      GlobalObjectId
      NablaId
      SortOccurrence
      ControllerProjection
      RootActivityProjection
  | CheckedDelta
      GlobalObjectId
      DeltaId
      SortOccurrence
      ControllerProjection
      RootActivityProjection
  deriving stock (Eq, Show)

data AppliedEntrySource
  = GenesisEntrySource
      GlobalObjectId
      SortOccurrence
      ControlIndex
      CheckedPublication
  | BaselineEntrySource SortOccurrence CheckedPublication
  | OccurrenceEntrySource StructuralApplication
  deriving stock (Eq, Show)

data AppliedEntry = AppliedEntry
  { source :: AppliedEntrySource,
    intrinsic :: DecodedStructuralCarrier
  }
  deriving stock (Eq, Show)

data BaselineStructuralWinner
  = BaselineStructuralWinner SortOccurrence CheckedPublication
  deriving stock (Eq, Show)

baselineStructuralWinner ::
  SortOccurrence -> CheckedPublication -> BaselineStructuralWinner
baselineStructuralWinner = BaselineStructuralWinner

-- | Carrier-intrinsic facts checked independently of mutable controller,
-- possession, effective-sort, and endpoint projections.  Every admitted dot
-- retains this complete closed meaning so an older causal cut can be resolved
-- again even after a concurrent winner has become current.
data DecodedStructuralCarrier
  = DecodedNeutral GlobalObjectId Label
  | DecodedEdge GlobalObjectId Label GlobalObjectId GlobalObjectId EdgeStrength
  | DecodedNabla GlobalObjectId Label SortId
  | DecodedDelta GlobalObjectId Label SortId
  deriving stock (Eq, Show)

data StructuralAppliedState = StructuralAppliedState
  { membershipGenerationId :: HeraldMembershipGenerationId,
    memberSetDigest :: MemberSetDigest,
    membership :: Set HeraldEpoch,
    membershipBases :: Map HeraldMembershipGenerationId SuccessorStructuralBase,
    admissionBases :: Map HeraldMembershipGenerationId HeraldMembershipLineage,
    admissionContributions :: Map JoinBootstrap.MembershipAdmissionOrigin (ControlIndex, JoinBootstrap.HeraldBootstrapContribution),
    baselineWinners :: Map GlobalObjectId AppliedEntry,
    history :: Map StructuralOccurrenceId AppliedEntry,
    sourceHistory :: !(Map HeraldEpoch StructuralSourceHistory),
    retirementSuppressions :: Map StructuralOccurrenceId StructuralRetirementSuppression,
    currentWinners :: !(Map GlobalObjectId AppliedEntry),
    projectionMeanings :: Map StructuralProjectionProvenance CheckedStructuralMeaning,
    localStores :: Map DeltaId DynamicLocalStoreSpec,
    retainedStoreResources :: Map StoreIncarnationId DynamicLocalStoreSpec,
    projectionStoreResources :: Map StructuralProjectionProvenance DynamicLocalStoreSpec,
    usedStoreIncarnations :: Map StoreIncarnationId FreshStoreIncarnationSource,
    controlOverlays :: Map GlobalObjectId StructuralControlOverlay,
    controlHistory :: Map (ControlIndex, GlobalObjectId) StructuralControlOverlay
  }
  deriving stock (Eq, Show)

-- | A source prefix is contiguous even across membership changes. Original
-- predecessors need not be monotone after carried/imported work, so retain the
-- endpoint before every non-dominating boundary. These anchors contain no
-- copied vectors: queries read the immutable applications already in history.
-- Control prerequisites have their own sparse running-maximum breakpoints.
data StructuralSourceHistory = StructuralSourceHistory
  { through :: !StructuralSequence,
    closureAnchors :: !(Set StructuralSequence),
    controlMaxima :: !(Map StructuralSequence ControlIndex)
  }
  deriving stock (Eq, Show)

extendStructuralSourceHistory ::
  StructuralApplication ->
  Maybe (StructuralSourceHistory, StructuralApplication) ->
  StructuralSourceHistory
extendStructuralSourceHistory application previous =
  StructuralSourceHistory
    { through = sequenceNumber,
      closureAnchors = case previous of
        Nothing -> Set.empty
        Just (summary, predecessor)
          | structuralVersionVectorCovers application.predecessor predecessor.predecessor -> summary.closureAnchors
          | otherwise -> Set.insert summary.through summary.closureAnchors,
      controlMaxima =
        if application.controlPrerequisite > greatest
          then Map.insert sequenceNumber application.controlPrerequisite maxima
          else maxima
    }
  where
    sequenceNumber = structuralOccurrenceSourceSequence application.occurrence
    maxima = maybe Map.empty ((.controlMaxima) . fst) previous
    greatest = maybe (controlIndex 0) snd (Map.lookupMax maxima)

advanceStructuralSourceHistory ::
  StructuralApplication -> StructuralAppliedState -> Map HeraldEpoch StructuralSourceHistory
advanceStructuralSourceHistory application state =
  Map.insert source (extendStructuralSourceHistory application previous) state.sourceHistory
  where
    source = structuralOccurrenceSourceHeraldEpoch application.occurrence
    previous = do
      summary <- Map.lookup source state.sourceHistory
      predecessor <- entryApplication (state.history Map.! structuralOccurrenceId source summary.through)
      pure (summary, predecessor)

-- Imported rows are already ordered by source/sequence. Prove contiguity once
-- while deriving the same small indexes used by ordinary admission. The fold
-- holds only the immediately preceding original application, not another owner.
-- This preserves the old import condition: every checked original stamp names
-- its own n-1, and the carrier decoder checks predecessor presence for every
-- retained row, including retired origins. Thus accepted imports have no gaps.
rebuildStructuralSourceHistory ::
  Map StructuralOccurrenceId AppliedEntry ->
  Either StructuralReconciliationProblem (Map HeraldEpoch StructuralSourceHistory)
rebuildStructuralSourceHistory history = fst <$> foldM step (Map.empty, Nothing) (Map.toAscList history)
  where
    step (summaries, prior) (occurrence, entry) = do
      let source = structuralOccurrenceSourceHeraldEpoch occurrence
          sequenceNumber = structuralOccurrenceSourceSequence occurrence
          previous = do
            summary <- Map.lookup source summaries
            predecessor <- prior
            pure (summary, predecessor)
          expected = Structural.nextAfterStructuralPrefix (maybe Structural.emptyStructuralPrefix (structuralPrefixThrough . (.through) . fst) previous)
      unless (sequenceNumber == expected) (Left (StructuralPredecessorDotMissing (structuralOccurrenceId source expected)))
      case entryApplication entry of
        Nothing -> error "structural occurrence history contains a baseline entry"
        Just application ->
          let summary = extendStructuralSourceHistory application previous
           in Right (Map.insert source summary summaries, Just application)

-- | The finite set of retained predecessor vectors which together dominate
-- every included occurrence, plus its greatest control prerequisite. Admission
-- proves the requested prefixes exist before exposing the summary.
data StructuralFrontierSummary = StructuralFrontierSummary
  { predecessors :: [(StructuralOccurrenceId, StructuralVersionVector)],
    controlPrerequisite :: !ControlIndex
  }

structuralFrontierPredecessors :: StructuralFrontierSummary -> [(StructuralOccurrenceId, StructuralVersionVector)]
structuralFrontierPredecessors summary = summary.predecessors

structuralFrontierControlPrerequisite :: StructuralFrontierSummary -> ControlIndex
structuralFrontierControlPrerequisite summary = summary.controlPrerequisite

prepareStructuralFrontierSummary ::
  StructuralVersionVector -> StructuralAppliedState -> Either StructuralReconciliationProblem StructuralFrontierSummary
prepareStructuralFrontierSummary frontier state = do
  validateCoveredDots frontier state
  let selected =
        [ (source, endpoint, state.sourceHistory Map.! source)
        | (source, prefix) <- structuralVersionVectorEntries frontier,
          Just endpoint <- [structuralPrefixSequence prefix]
        ]
      requirements =
        [ (occurrence, application.predecessor)
        | (source, endpoint, summary) <- selected,
          sequenceNumber <- Set.toAscList (Set.insert endpoint (fst (Set.split endpoint summary.closureAnchors))),
          let occurrence = structuralOccurrenceId source sequenceNumber,
          Just application <- [entryApplication (state.history Map.! occurrence)]
        ]
      greatest = foldl' max (controlIndex 0) [index | (_, endpoint, summary) <- selected, Just (_, index) <- [Map.lookupLE endpoint summary.controlMaxima]]
  Right (StructuralFrontierSummary requirements greatest)

-- | Optimized admission and its independent exhaustive reference. The latter
-- remains available for differential properties and explicit diagnostics.
validateStructuralFrontier :: StructuralVersionVector -> StructuralAppliedState -> Either StructuralReconciliationProblem ()
validateStructuralFrontier frontier state = validateCoveredDots frontier state >> validateProjectionClosure frontier state

validateStructuralFrontierExhaustive :: StructuralVersionVector -> StructuralAppliedState -> Either StructuralReconciliationProblem ()
validateStructuralFrontierExhaustive frontier state = validateCoveredDotsExhaustive frontier state >> validateProjectionClosureExhaustive frontier state

data StructuralControlOverlay
  = StructuralControllerOverlay
      StructuralConsequenceCause
      ControllerProjection
      RootActivityProjection
  | StructuralDeletedOverlay StructuralConsequenceCause
  deriving stock (Eq, Show)

data StructuralControlOverlayView
  = StructuralControllerOverlayView
      StructuralConsequenceCause
      ControllerProjection
  | StructuralDeletedOverlayView StructuralConsequenceCause
  deriving stock (Eq, Show)

data StructuralControlTarget
  = GenesisStructuralControlTarget GlobalObjectId
  | PublishedBaselineStructuralControlTarget GlobalObjectId PublicationId
  | OccurrenceStructuralControlTarget StructuralOccurrenceId
  deriving stock (Eq, Ord, Show)

-- | The authority-relevant part of one Nabla at an exact retained
-- structural/control coordinate.  This deliberately excludes current
-- placement and possession: those are applicability facts checked by the
-- caller, not historical authority evidence.
data HistoricalNablaProjection = HistoricalNablaProjection
  { provenance :: StructuralProjectionProvenance,
    controller :: ControllerProjection,
    sort :: SortOccurrence,
    controlPrerequisite :: ControlIndex
  }
  deriving stock (Eq, Show)

historicalNablaProjectionProvenance ::
  HistoricalNablaProjection -> StructuralProjectionProvenance
historicalNablaProjectionProvenance projection = projection.provenance

historicalNablaProjectionController ::
  HistoricalNablaProjection -> ControllerProjection
historicalNablaProjectionController projection = projection.controller

historicalNablaProjectionSort :: HistoricalNablaProjection -> SortOccurrence
historicalNablaProjectionSort projection = projection.sort

historicalNablaProjectionControlPrerequisite ::
  HistoricalNablaProjection -> ControlIndex
historicalNablaProjectionControlPrerequisite projection =
  projection.controlPrerequisite

emptyStructuralAppliedState :: StructuralVersionVector -> StructuralAppliedState
emptyStructuralAppliedState membershipWitness =
  StructuralAppliedState
    { membershipGenerationId =
        structuralVersionVectorMembershipGenerationId membershipWitness,
      memberSetDigest =
        structuralVersionVectorMemberSetDigest membershipWitness,
      membership = membershipOf membershipWitness,
      membershipBases = Map.empty,
      admissionBases = Map.empty,
      admissionContributions = Map.empty,
      baselineWinners = Map.empty,
      history = Map.empty,
      sourceHistory = Map.empty,
      retirementSuppressions = Map.empty,
      currentWinners = Map.empty,
      projectionMeanings = Map.empty,
      localStores = Map.empty,
      retainedStoreResources = Map.empty,
      projectionStoreResources = Map.empty,
      usedStoreIncarnations = Map.empty,
      controlOverlays = Map.empty,
      controlHistory = Map.empty
    }

-- | Seed every checked index-zero environment description as explicit genesis
-- material. Each entry retains its canonical publication, without inventing a
-- structural occurrence. Local reader resources use their exact checked
-- placements; remote resources remain absent from this Herald-owned projection.
structuralAppliedStateWithGenesis ::
  SystemId ->
  ReconciliationViews ->
  StructuralVersionVector ->
  [AppliedProcessBootstrap] ->
  Either
    StructuralReconciliationProblem
    (Either (Set StructuralDependency) StructuralAppliedState)
structuralAppliedStateWithGenesis system views membership bootstraps = do
  validateEffectiveSorts views
  entries <- traverse genesisEntry [entry | bootstrap <- bootstraps, entry <- appliedProcessEnvironmentPublications system bootstraps bootstrap]
  let duplicateObjects = duplicateKeys (fmap entryObject entries)
  case duplicateObjects of
    object : _ -> Left (StructuralGenesisObjectConflict object)
    [] -> Right ()
  let baselines = Map.fromList [(entryObject entry, entry) | entry <- entries]
      stores = Map.fromList (catMaybes (fmap genesisStore suppliedRoots))
      unresolved = seededState views membership baselines Map.empty stores
  validateLocalStores views unresolved
  resolved <- resolveSeededEntries views unresolved baselines
  let dependencies = Set.unions [held | Left held <- Map.elems resolved]
  if Set.null dependencies
    then finishResolvedEntries views stores unresolved baselines resolved
    else Right (Left dependencies)
  where
    suppliedRoots =
      [ (bootstrap, root)
      | bootstrap <- bootstraps,
        root <- appliedProcessRoots bootstrap
      ]

    genesisEntry (_, occurrence, publication) = do
      intrinsic <- decodeCarrierPublication publication
      let object = decodedCarrierObject intrinsic
          carrierSort = sortOccurrence (checkedPublicationSort publication) occurrence
      Right (AppliedEntry (GenesisEntrySource object carrierSort (controlIndex 0) publication) intrinsic)

    genesisStore (bootstrap, root) =
      case (appliedRootRole root, appliedRootPlacement root) of
        (ReaderRoot delta, Just (herald, incarnation))
          | herald == views.localHerald ->
              Just
                ( delta,
                  dynamicLocalStoreSpec
                    (appliedProcessEpochId bootstrap)
                    (appliedRootObjectId root)
                    herald
                    delta
                    (sortOccurrence (appliedRootSortId root) (appliedRootOccurrenceId root))
                    incarnation
                    ConfiguredManifestStoreIncarnation
                )
        _ -> Nothing

    duplicateKeys values =
      Map.keys
        (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(value, 1) | value <- values]))

structuralAppliedStateWithStores ::
  ReconciliationViews ->
  StructuralVersionVector ->
  Map DeltaId DynamicLocalStoreSpec ->
  Either StructuralReconciliationProblem StructuralAppliedState
structuralAppliedStateWithStores views membership =
  \stores -> do
    let seeded = seededState views membership Map.empty Map.empty stores
    validateLocalStores views seeded
    Right seeded

-- | Seed exact already-published baseline winners without inventing structural
-- dots.
-- The complete set is decoded before resolution, so a controlled baseline edge
-- may depend on a controlled baseline vertex.  Active local delta projections
-- additionally require their exact already-existing Store resource.  These
-- baseline projections retain publication provenance: later ordered controls
-- revise them without fabricating a structural dot. Checked startup objects
-- use 'structuralAppliedStateWithGenesis' so their canonical publications also
-- retain the distinct object-qualified genesis provenance.
structuralAppliedStateWithBaseline ::
  ReconciliationViews ->
  StructuralVersionVector ->
  Map GlobalObjectId BaselineStructuralWinner ->
  Map DeltaId DynamicLocalStoreSpec ->
  Either
    StructuralReconciliationProblem
    (Either (Set StructuralDependency) StructuralAppliedState)
structuralAppliedStateWithBaseline views membership supplied stores = do
  validateEffectiveSorts views
  baselines <- Map.traverseWithKey prepareEntry supplied
  let unresolved = seededState views membership baselines Map.empty stores
  rejectDuplicateBaselinePublications baselines
  validateLocalStores views unresolved
  resolved <- resolveSeededEntries views unresolved baselines
  let dependencies = Set.unions [held | Left held <- Map.elems resolved]
  if Set.null dependencies
    then finishResolvedEntries views stores unresolved baselines resolved
    else Right (Left dependencies)
  where
    prepareEntry key (BaselineStructuralWinner carrierSort publication) = do
      validateCarrierDefinition views carrierSort publication
      intrinsic <- decodeCarrierPublication publication
      let entry = AppliedEntry (BaselineEntrySource carrierSort publication) intrinsic
      if key == entryObject entry
        then Right entry
        else Left (StructuralBaselineObjectKeyMismatch key (entryObject entry))

-- Edges must resolve against the admitted vertex meanings, including when the
-- complete connected environment is supplied by checked genesis.
resolveSeededEntries ::
  ReconciliationViews ->
  StructuralAppliedState ->
  Map GlobalObjectId AppliedEntry ->
  Either StructuralReconciliationProblem (Map GlobalObjectId (Either (Set StructuralDependency) CheckedStructuralMeaning))
resolveSeededEntries views unresolved baselines = do
  let vertexEntries = Map.filter ((/= EdgeCarrier) . entryRole) baselines
      edgeEntries = Map.filter ((== EdgeCarrier) . entryRole) baselines
  resolvedVertices <- traverse (resolveBaseline unresolved) vertexEntries
  let availableVertexMeanings =
        Map.fromList
          [ (entryProvenance entry, meaning)
          | (object, entry) <- Map.toAscList vertexEntries,
            Right meaning <- [resolvedVertices Map.! object]
          ]
      withResolvedVertices =
        unresolved {projectionMeanings = availableVertexMeanings}
  resolvedEdges <- traverse (resolveBaseline withResolvedVertices) edgeEntries
  Right (Map.union resolvedVertices resolvedEdges)
  where
    resolveBaseline state entry =
      resolveMeaning views state Nothing entry.intrinsic

finishResolvedEntries ::
  ReconciliationViews ->
  Map DeltaId DynamicLocalStoreSpec ->
  StructuralAppliedState ->
  Map GlobalObjectId AppliedEntry ->
  Map GlobalObjectId (Either (Set StructuralDependency) CheckedStructuralMeaning) ->
  Either
    StructuralReconciliationProblem
    (Either (Set StructuralDependency) StructuralAppliedState)
finishResolvedEntries views stores state entries resolved = do
  let projections =
        Map.fromList
          [ (entryProvenance entry, meaning)
          | (object, entry) <- Map.toAscList entries,
            Right meaning <- [resolved Map.! object]
          ]
  resourceResults <-
    traverse
      (\(object, entry) -> baselineResource entry (resolved Map.! object))
      (Map.toAscList entries)
  let resourceDependencies = Set.unions [held | Left held <- resourceResults]
  if Set.null resourceDependencies
    then
      let resources = catMaybes [resource | Right resource <- resourceResults]
          projectionResources = Map.fromList resources
          matchedDeltas = Set.fromList [store.delta | (_, store) <- resources]
       in case Set.lookupMin (Map.keysSet stores `Set.difference` matchedDeltas) of
            Just unmatched -> Left (StructuralBaselineStoreWithoutActiveProjection unmatched)
            Nothing ->
              Right
                ( Right
                    state
                      { projectionMeanings = projections,
                        projectionStoreResources = projectionResources
                      }
                )
    else Right (Left resourceDependencies)
  where
    baselineResource entry = \case
      Right
        ( CheckedDelta
            rootObject
            delta
            typedSort
            _
            (ActiveRootHere process)
          ) ->
          case Map.lookup delta stores of
            Nothing ->
              Right (Left (Set.singleton (FreshStoreIncarnationDependency delta)))
            Just store
              | destinationLogicalKey store
                  == (process, rootObject, views.localHerald, delta, typedSort) ->
                  Right (Right (Just (entryProvenance entry, store)))
              | otherwise -> Left (StructuralBaselineStoreMismatch delta)
      Right _ -> Right (Right Nothing)
      Left _ -> error "baseline dependency aggregate omitted a held projection"

seededState ::
  ReconciliationViews ->
  StructuralVersionVector ->
  Map GlobalObjectId AppliedEntry ->
  Map StructuralProjectionProvenance CheckedStructuralMeaning ->
  Map DeltaId DynamicLocalStoreSpec ->
  StructuralAppliedState
seededState _views membership baselines projections stores =
  StructuralAppliedState
    { membershipGenerationId =
        structuralVersionVectorMembershipGenerationId membership,
      memberSetDigest = structuralVersionVectorMemberSetDigest membership,
      membership = membershipOf membership,
      membershipBases = Map.empty,
      admissionBases = Map.empty,
      admissionContributions = Map.empty,
      baselineWinners = baselines,
      history = Map.empty,
      sourceHistory = Map.empty,
      retirementSuppressions = Map.empty,
      currentWinners = baselines,
      projectionMeanings = projections,
      localStores = stores,
      retainedStoreResources =
        Map.fromList [(store.incarnation, store) | store <- Map.elems stores],
      projectionStoreResources = Map.empty,
      usedStoreIncarnations =
        Map.fromList
          [ (store.incarnation, store.incarnationSource)
          | store <- Map.elems stores
          ],
      controlOverlays = Map.empty,
      controlHistory = Map.empty
    }

structuralAppliedHistoryOccurrences ::
  StructuralAppliedState -> [StructuralOccurrenceId]
structuralAppliedHistoryOccurrences state = Map.keys state.history

-- | Every structurally admitted object and its immutable carrier role,
-- including genesis/baseline entries and occurrences which are no longer
-- current. Object-role admission guarantees that duplicates agree.
structuralAppliedRetainedObjectRoles ::
  StructuralAppliedState -> [(GlobalObjectId, StructuralCarrierRole)]
structuralAppliedRetainedObjectRoles state =
  Map.toAscList
    ( Map.fromList
        [ (entryObject entry, entryRole entry)
        | entry <- Map.elems state.baselineWinners <> Map.elems state.history
        ]
    )

-- | Whether one exact causal occurrence has committed to retained history.
-- Peer-input dependency release uses this predicate without exposing the
-- history map or accepting a caller-supplied progress claim.
structuralAppliedHasOccurrence ::
  StructuralOccurrenceId -> StructuralAppliedState -> Bool
structuralAppliedHasOccurrence occurrence state =
  Map.member occurrence state.history

-- | Exact, immutable reason why one structural occurrence was consumed only
-- as a causal dot. A direct cause names the retired regular SortId and Resolve
-- coordinate. An inherited cause names the predecessor-covered suppressed
-- endpoint occurrence from which an Edge inherits the same floor.
data StructuralRetirementSuppression
  = DirectRegularSortRetirementSuppression SortId ControlIndex
  | InheritedEndpointRetirementSuppression StructuralOccurrenceId ControlIndex
  deriving stock (Eq, Ord, Show)

structuralRetirementSuppressionControlIndex ::
  StructuralRetirementSuppression -> ControlIndex
structuralRetirementSuppressionControlIndex = \case
  DirectRegularSortRetirementSuppression _ floorIndex -> floorIndex
  InheritedEndpointRetirementSuppression _ floorIndex -> floorIndex

-- | Derive the latest regular-sort retirement floor inherited through
-- occurrence-local structural dependencies. A suppressed Edge endpoint is
-- retained as intrinsic history but cannot satisfy the live endpoint
-- projection; lower-control downstream work must therefore settle as another
-- suppressed causal dot instead of waiting forever.
structuralRetirementSuppressionControlPrerequisite ::
  StructuralVersionVector ->
  CheckedPublication ->
  StructuralAppliedState ->
  Either
    StructuralReconciliationProblem
    (Maybe StructuralRetirementSuppression)
structuralRetirementSuppressionControlPrerequisite predecessor publication state = do
  carrier <- decodeCarrierPublication publication
  pure
    ( foldl'
        laterSuppression
        Nothing
        [ InheritedEndpointRetirementSuppression occurrence floorIndex
        | endpoint <- carrierDependencyObjects carrier,
          winnerForObjectAt (Just predecessor) endpoint state == Nothing,
          (occurrence, suppression) <- Map.toAscList state.retirementSuppressions,
          coversOccurrence predecessor occurrence,
          Just entry <- [Map.lookup occurrence state.history],
          entryObject entry == endpoint,
          let floorIndex = structuralRetirementSuppressionControlIndex suppression
        ]
    )
  where
    laterSuppression Nothing candidate = Just candidate
    laterSuppression incumbent@(Just retained) candidate
      | suppressionKey candidate > suppressionKey retained = Just candidate
      | otherwise = incumbent
    suppressionKey suppression =
      (structuralRetirementSuppressionControlIndex suppression, suppression)

    carrierDependencyObjects = \case
      DecodedEdge _ _ source destination _ -> [source, destination]
      DecodedNeutral {} -> []
      DecodedNabla {} -> []
      DecodedDelta {} -> []

-- | Complete retained suppression transcript for composed invariant checking.
-- The occurrence, causal predecessor, publication and suppression evidence are
-- returned together so the caller can authenticate direct Registry history;
-- inherited endpoint evidence is already checked recursively by this owner.
structuralAppliedRetirementSuppressions ::
  StructuralAppliedState ->
  [ ( StructuralOccurrenceId,
      StructuralVersionVector,
      CheckedPublication,
      StructuralRetirementSuppression
    )
  ]
structuralAppliedRetirementSuppressions state =
  [ (occurrence, application.predecessor, application.publication, suppression)
  | (occurrence, suppression) <- Map.toAscList state.retirementSuppressions,
    Just entry <- [Map.lookup occurrence state.history],
    Just application <- [entryApplication entry]
  ]

structuralAppliedRetirementSuppressionAt ::
  StructuralOccurrenceId ->
  StructuralAppliedState ->
  Maybe StructuralRetirementSuppression
structuralAppliedRetirementSuppressionAt occurrence state =
  Map.lookup occurrence state.retirementSuppressions

-- | Recover the immutable controlled carrier facts for one exact dynamic
-- occurrence.  This is intentionally independent of the current winner and
-- control overlay: an Oracle Resolve may win before a later publication is
-- observed locally, in which case the captured occurrence still authenticates
-- the subject while terminal deletion applies to the then-current same-object
-- projection.
structuralAppliedControlledProjectionAtOccurrence ::
  StructuralOccurrenceId ->
  StructuralAppliedState ->
  Maybe AppliedControlledProjection
structuralAppliedControlledProjectionAtOccurrence occurrence state =
  appliedControlledProjection <$> Map.lookup occurrence state.history

structuralAppliedMembershipGenerationId ::
  StructuralAppliedState -> HeraldMembershipGenerationId
structuralAppliedMembershipGenerationId state = state.membershipGenerationId

structuralAppliedMemberSetDigest :: StructuralAppliedState -> MemberSetDigest
structuralAppliedMemberSetDigest state = state.memberSetDigest

structuralAppliedFixedMembership :: StructuralAppliedState -> [HeraldEpoch]
structuralAppliedFixedMembership state = Set.toAscList state.membership

-- | Select an already-retained coordinate for historical reconstruction.
-- This changes neither original stamps nor established-base evidence. New
-- descendant authority enters through 'establishStructuralAppliedMembership';
-- selecting a vector alone cannot authorize cross-generation causal coverage.
rebaseStructuralAppliedMembership ::
  StructuralVersionVector ->
  StructuralAppliedState ->
  StructuralAppliedState
rebaseStructuralAppliedMembership successorVector state =
  state
    { membershipGenerationId =
        structuralVersionVectorMembershipGenerationId successorVector,
      memberSetDigest = structuralVersionVectorMemberSetDigest successorVector,
      membership = membershipOf successorVector
    }

-- | Establish checked descendant authority without changing any occurrence's
-- original stamp. The retained base is also the proof used when a historical
-- predecessor vector is interpreted at a later membership coordinate.
establishStructuralAppliedMembership ::
  SuccessorStructuralBase ->
  StructuralVersionVector ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralAppliedState
establishStructuralAppliedMembership base successorVector state = do
  suffix <-
    either
      (const (Left StructuralMembershipBaseMismatch))
      Right
      (heraldMembershipLineageFrom state.membershipGenerationId (successorStructuralBaseLineage base))
  unless
    ( heraldMembershipGenerationActiveMemberSetDigest (heraldMembershipLineageOrigin suffix) == state.memberSetDigest
        && structuralVersionVectorCovers successorVector initialVector
        && (state.membershipGenerationId /= target || Map.lookup target state.membershipBases == Just base)
    )
    (Left StructuralMembershipBaseMismatch)
  case Map.lookup target state.membershipBases of
    Just retained | retained /= base -> Left StructuralMembershipBaseMismatch
    _ ->
      Right
        (rebaseStructuralAppliedMembership successorVector state)
          { membershipBases = Map.insert target base state.membershipBases
          }
  where
    target = successorStructuralBaseGenerationId base
    initialVector =
      terminalSourceUnionSuccessorInitialVector
        (terminalSourceUnionEstablishedUnion (successorStructuralBaseEstablishedUnion base))

-- | Retain the exact admission lineage and nominal private-view contribution.
-- These facts authorize historical vector interpretation and topology presence,
-- without introducing a publication, controlled root, or application writer.
establishStructuralAdmissionMembership :: HeraldMembershipLineage -> Topology.HeraldJoinBaseRecipe -> JoinBootstrap.HeraldBootstrapContribution -> Topology.TopologyCut -> StructuralVersionVector -> StructuralAppliedState -> Either StructuralReconciliationProblem StructuralAppliedState
establishStructuralAdmissionMembership lineage recipe contribution cut vector state = do
  let origin = heraldMembershipLineageOrigin lineage
      target = heraldMembershipLineageTarget lineage
      targetId = heraldMembershipGenerationId target
      index = Topology.topologyFrontierAppliedControlPrefix (Topology.topologyCutFrontier cut)
  expected <- either (const (Left StructuralMembershipBaseMismatch)) Right (Topology.activateHeraldJoinBase index (Topology.topologyCutOccurrenceDigest cut) recipe)
  unless
    ( expected == (target, cut)
        && origin == Topology.heraldJoinBaseRecipeGeneration recipe
        && heraldMembershipGenerationId origin == state.membershipGenerationId
        && heraldMembershipGenerationActiveMemberSetDigest origin == state.memberSetDigest
        && JoinBootstrap.contributionAdmission contribution == Topology.heraldJoinBaseRecipeAdmissionId recipe
        && JoinBootstrap.contributionHerald contribution == Topology.heraldJoinBaseRecipeApplicant recipe
        && JoinBootstrap.contributionDigest contribution == Topology.heraldJoinBaseRecipeContributionDigest recipe
        && structuralVersionVectorCovers vector (Topology.topologyFrontierStructuralVersionVector (Topology.topologyCutFrontier cut))
        && Map.notMember targetId state.admissionBases
        && Map.notMember (JoinBootstrap.contributionOrigin contribution) state.admissionContributions
    )
    (Left StructuralMembershipBaseMismatch)
  pure
    (rebaseStructuralAppliedMembership vector state)
      { admissionBases = Map.insert targetId lineage state.admissionBases,
        admissionContributions = Map.insert (JoinBootstrap.contributionOrigin contribution) (index, contribution) state.admissionContributions
      }

structuralAppliedLocalStores ::
  StructuralAppliedState -> [DynamicLocalStoreSpec]
structuralAppliedLocalStores state = Map.elems state.localStores

-- | Reintroduce one retained live resource without changing its structural
-- projection.  This deliberately constructs the cross-owner contradiction
-- used by whole-Herald invariant properties; production transitions must use
-- a prepared reconciliation instead.
restoreStructuralLocalStoreForInvariantTest ::
  DynamicLocalStoreSpec ->
  StructuralAppliedState ->
  StructuralAppliedState
restoreStructuralLocalStoreForInvariantTest resource state =
  state
    { localStores =
        Map.insert (dynamicStoreDelta resource) resource state.localStores
    }

-- | Exact resource metadata retained even when another concurrent occurrence
-- owns the active placement.  This is deliberately distinct from
-- 'structuralAppliedLocalStores'.
structuralAppliedRetainedStoreResources ::
  StructuralAppliedState -> Map StoreIncarnationId DynamicLocalStoreSpec
structuralAppliedRetainedStoreResources state = state.retainedStoreResources

-- | The currently selected retained resource for each causally capable delta
-- occurrence.  Replaced resources remain in the incarnation ledger above.
structuralAppliedOccurrenceStoreResources ::
  StructuralAppliedState -> Map StructuralOccurrenceId DynamicLocalStoreSpec
structuralAppliedOccurrenceStoreResources state =
  Map.fromList
    [ (occurrence, store)
    | (provenance, store) <- Map.toAscList state.projectionStoreResources,
      Just occurrence <- [structuralProjectionProvenanceOccurrence provenance]
    ]

-- | Verify that a retained local resource is the exact Store belonging to an
-- imported publication-backed baseline Delta.  The publication is deliberately
-- kept out of 'DynamicLocalStoreSpec': it is projection provenance, while the
-- Store slot retains it explicitly as 'StructuralBaselineReader'.
structuralAppliedBaselineStoreMatches ::
  PublicationId ->
  DynamicLocalStoreSpec ->
  StructuralAppliedState ->
  Bool
structuralAppliedBaselineStoreMatches publication resource state =
  any matchesBaseline (Map.elems state.baselineWinners)
  where
    matchesBaseline entry =
      entryProvenance entry == BaselineStructuralProvenance publication
        && case Map.lookup (entryProvenance entry) state.projectionMeanings of
          Just
            ( CheckedDelta
                object
                delta
                typedSort
                (LiveProcessController process herald)
                (ActiveRootHere activeProcess)
              ) ->
              object == resource.controller
                && delta == resource.delta
                && typedSort == resource.sort
                && process == resource.process
                && activeProcess == resource.process
                && herald == resource.herald
          _ -> False

-- | One occurrence-exact Delta meaning retained independently of the current
-- structural winner.  Placement advertisements are qualified by an installed
-- topology cut, so a receiver which has already advanced to a later winner
-- must still be able to authenticate the older route named by that cut.
data AppliedDynamicDeltaCoordinate = AppliedDynamicDeltaCoordinate
  { occurrence :: StructuralOccurrenceId,
    object :: GlobalObjectId,
    delta :: DeltaId,
    sort :: SortOccurrence,
    controller :: ControllerProjection,
    controlPrerequisite :: ControlIndex
  }
  deriving stock (Eq, Show)

appliedDynamicDeltaOccurrence ::
  AppliedDynamicDeltaCoordinate -> StructuralOccurrenceId
appliedDynamicDeltaOccurrence coordinate = coordinate.occurrence

appliedDynamicDeltaObject ::
  AppliedDynamicDeltaCoordinate -> GlobalObjectId
appliedDynamicDeltaObject coordinate = coordinate.object

appliedDynamicDeltaDelta :: AppliedDynamicDeltaCoordinate -> DeltaId
appliedDynamicDeltaDelta coordinate = coordinate.delta

appliedDynamicDeltaSort :: AppliedDynamicDeltaCoordinate -> SortOccurrence
appliedDynamicDeltaSort coordinate = coordinate.sort

appliedDynamicDeltaController ::
  AppliedDynamicDeltaCoordinate -> ControllerProjection
appliedDynamicDeltaController coordinate = coordinate.controller

appliedDynamicDeltaControlPrerequisite ::
  AppliedDynamicDeltaCoordinate -> ControlIndex
appliedDynamicDeltaControlPrerequisite coordinate = coordinate.controlPrerequisite

structuralAppliedDynamicDeltaCoordinates ::
  StructuralAppliedState -> [AppliedDynamicDeltaCoordinate]
structuralAppliedDynamicDeltaCoordinates state =
  [ AppliedDynamicDeltaCoordinate
      { occurrence,
        object,
        delta,
        sort = typedSort,
        controller,
        controlPrerequisite = application.controlPrerequisite
      }
  | (occurrence, entry) <- Map.toAscList state.history,
    Just application <- [entryApplication entry],
    Just (ProjectedEntry _ (CheckedDelta object delta typedSort controller _)) <-
      [effectiveProjectedEntry state entry]
  ]

-- | Every retained Delta route coordinate which was induced either by its
-- immutable carrier meaning or by a topology-changing ordered control. Unlike
-- the compatibility occurrence-only view above, this includes checked genesis
-- and imported baseline provenance and never rewrites old coordinates through
-- the latest overlay.
data AppliedDeltaRouteCoordinate = AppliedDeltaRouteCoordinate
  { provenance :: StructuralProjectionProvenance,
    object :: GlobalObjectId,
    delta :: DeltaId,
    sort :: SortOccurrence,
    controller :: ControllerProjection,
    controlPrerequisite :: ControlIndex
  }
  deriving stock (Eq, Show)

appliedDeltaRouteProvenance ::
  AppliedDeltaRouteCoordinate -> StructuralProjectionProvenance
appliedDeltaRouteProvenance coordinate = coordinate.provenance

appliedDeltaRouteObject :: AppliedDeltaRouteCoordinate -> GlobalObjectId
appliedDeltaRouteObject coordinate = coordinate.object

appliedDeltaRouteDelta :: AppliedDeltaRouteCoordinate -> DeltaId
appliedDeltaRouteDelta coordinate = coordinate.delta

appliedDeltaRouteSort :: AppliedDeltaRouteCoordinate -> SortOccurrence
appliedDeltaRouteSort coordinate = coordinate.sort

appliedDeltaRouteController :: AppliedDeltaRouteCoordinate -> ControllerProjection
appliedDeltaRouteController coordinate = coordinate.controller

appliedDeltaRouteControlPrerequisite ::
  AppliedDeltaRouteCoordinate -> ControlIndex
appliedDeltaRouteControlPrerequisite coordinate = coordinate.controlPrerequisite

structuralAppliedDeltaRouteCoordinates ::
  StructuralAppliedState -> [AppliedDeltaRouteCoordinate]
structuralAppliedDeltaRouteCoordinates state =
  originalCoordinates <> controlCoordinates
  where
    entries = Map.elems state.baselineWinners <> Map.elems state.history
    originalCoordinates = mapMaybe originalCoordinate entries
    originalCoordinate entry = do
      CheckedDelta object delta typedSort controller _ <-
        Map.lookup (entryProvenance entry) state.projectionMeanings
      LiveProcessController {} <- Just controller
      pure
        AppliedDeltaRouteCoordinate
          { provenance = entryProvenance entry,
            object,
            delta,
            sort = typedSort,
            controller,
            controlPrerequisite = entryControlPrerequisite entry
          }
    controlCoordinates =
      [ AppliedDeltaRouteCoordinate
          { provenance = entryProvenance entry,
            object,
            delta,
            sort = typedSort,
            controller,
            controlPrerequisite = max (entryControlPrerequisite entry) index
          }
      | ((index, controlledObject), overlay) <- Map.toAscList state.controlHistory,
        StructuralControllerOverlay _ controller _ <- [overlay],
        LiveProcessController {} <- [controller],
        entry <- entries,
        entryObject entry == controlledObject,
        Just (CheckedDelta object delta typedSort _ _) <-
          [Map.lookup (entryProvenance entry) state.projectionMeanings]
      ]

structuralAppliedObjectHasControlHistory ::
  GlobalObjectId -> StructuralAppliedState -> Bool
structuralAppliedObjectHasControlHistory object state =
  any ((== object) . snd . fst) (Map.toAscList state.controlHistory)

-- | Snapshot-local historical authority. Structural meanings and ordered
-- controller facts remain separate: preparing this index never materializes
-- their Cartesian product. Neither current placement nor Store incarnation is
-- part of historical route authority.
data PreparedDeltaRouteAdmission
  = PreparedDeltaRouteAdmission
      !(Map (DeltaId, GlobalObjectId, SortOccurrence, ControllerProjection) (Set ControlIndex))
      !(Map (DeltaId, GlobalObjectId, SortOccurrence) (Set ControlIndex))
      !(Map (GlobalObjectId, ControllerProjection) (Set ControlIndex))
      !(Set GlobalObjectId)

prepareDeltaRouteAdmission ::
  StructuralAppliedState -> PreparedDeltaRouteAdmission
prepareDeltaRouteAdmission state =
  PreparedDeltaRouteAdmission originals structuralPrerequisites controlIndices controlledObjects
  where
    -- Admission has already checked that a meaning's object is the entry's
    -- object, including retained nonwinning and imported baseline entries.
    deltaFacts =
      [ (delta, object, typedSort, controller, entryControlPrerequisite entry)
      | entry <- Map.elems state.baselineWinners <> Map.elems state.history,
        Just (CheckedDelta object delta typedSort controller _) <-
          [Map.lookup (entryProvenance entry) state.projectionMeanings]
      ]
    originals =
      Map.fromListWith
        Set.union
        [ ((delta, object, typedSort, controller), Set.singleton prerequisite)
        | (delta, object, typedSort, controller@LiveProcessController {}, prerequisite) <- deltaFacts
        ]
    structuralPrerequisites =
      Map.fromListWith
        Set.union
        [ ((delta, object, typedSort), Set.singleton prerequisite)
        | (delta, object, typedSort, _, prerequisite) <- deltaFacts
        ]
    controlIndices =
      Map.fromListWith
        Set.union
        [ ((object, controller), Set.singleton index)
        | ((index, object), StructuralControllerOverlay _ controller _) <-
            Map.toAscList state.controlHistory,
          LiveProcessController {} <- [controller]
        ]
    -- Even deletion and non-live controller history disables the configured
    -- genesis shortcut; only the retained exact coordinates may then admit it.
    controlledObjects = Set.fromList [object | (_, object) <- Map.keys state.controlHistory]

preparedDeltaRouteIsValid ::
  DeltaId ->
  GlobalObjectId ->
  SortOccurrence ->
  ControllerProjection ->
  ControlIndex ->
  PreparedDeltaRouteAdmission ->
  Bool
preparedDeltaRouteIsValid delta object typedSort controller prerequisite (PreparedDeltaRouteAdmission originals structuralPrerequisites controlIndices _) =
  maybe False (Set.member prerequisite) (Map.lookup (delta, object, typedSort, controller) originals)
    || case (Map.lookup (delta, object, typedSort) structuralPrerequisites, Map.lookup (object, controller) controlIndices) of
      (Just structural, Just controls) ->
        -- max(b, i) = p exactly when one input equals p and the other is
        -- at most p. The sets' minima answer existence without enumerating
        -- any structural/control pair, including controls predating a carrier.
        (Set.member prerequisite structural && minimumAtMost controls)
          || (Set.member prerequisite controls && minimumAtMost structural)
      _ -> False
  where
    minimumAtMost = maybe False (<= prerequisite) . Set.lookupMin

-- | Greatest exact prerequisite for the local Store audit, with the same
-- absence result as the exhaustive route-coordinate projection.
preparedDeltaRouteLatestPrerequisite ::
  DeltaId ->
  GlobalObjectId ->
  SortOccurrence ->
  ControllerProjection ->
  PreparedDeltaRouteAdmission ->
  Maybe ControlIndex
preparedDeltaRouteLatestPrerequisite delta object typedSort controller (PreparedDeltaRouteAdmission originals structuralPrerequisites controlIndices _) =
  max original controlled
  where
    original = Map.lookup (delta, object, typedSort, controller) originals >>= Set.lookupMax
    controlled = do
      structural <- Map.lookup (delta, object, typedSort) structuralPrerequisites >>= Set.lookupMax
      control <- Map.lookup (object, controller) controlIndices >>= Set.lookupMax
      pure (max structural control)

preparedDeltaRouteObjectHasControlHistory ::
  GlobalObjectId -> PreparedDeltaRouteAdmission -> Bool
preparedDeltaRouteObjectHasControlHistory object (PreparedDeltaRouteAdmission _ _ _ controlledObjects) =
  Set.member object controlledObjects

-- | Every incarnation admitted by this preparation state, including retired
-- incarnations.  Keeping this hidden history prevents passivate/reactivate
-- from accepting an old exact store identity as fresh evidence.
structuralAppliedUsedStoreIncarnations ::
  StructuralAppliedState -> Map StoreIncarnationId FreshStoreIncarnationSource
structuralAppliedUsedStoreIncarnations state = state.usedStoreIncarnations

structuralAppliedControlOverlays ::
  StructuralAppliedState -> [(GlobalObjectId, StructuralControlOverlayView)]
structuralAppliedControlOverlays state =
  [ (object, controlOverlayView overlay)
  | (object, overlay) <- Map.toAscList state.controlOverlays
  ]

controlOverlayView :: StructuralControlOverlay -> StructuralControlOverlayView
controlOverlayView overlay = case overlay of
  StructuralControllerOverlay cause controller _ ->
    StructuralControllerOverlayView cause controller
  StructuralDeletedOverlay cause -> StructuralDeletedOverlayView cause

-- | Immutable ordered control evidence.  The key index is repeated in the
-- view deliberately: whole-state validation must be able to authenticate
-- every historical cause, not only the currently effective overlay.
structuralAppliedControlHistory ::
  StructuralAppliedState -> [(ControlIndex, GlobalObjectId, StructuralControlOverlayView)]
structuralAppliedControlHistory state =
  [ (index, object, controlOverlayView overlay)
  | ((index, object), overlay) <- Map.toAscList state.controlHistory
  ]

-- | Original resolved meaning, independent of donor possession or local Store
-- ownership. A carrier's intrinsic SortId alone cannot reconstruct its historical
-- meaning after that sort has been retired and introduced again.
data PortableStructuralMeaning
  = PortableNeutral !GlobalObjectId
  | PortableEdge !GlobalObjectId !EdgePayload
  | PortableNabla !GlobalObjectId !NablaId !SortOccurrence !ControllerProjection
  | PortableDelta !GlobalObjectId !DeltaId !SortOccurrence !ControllerProjection
  deriving stock (Eq, Show)

-- | Portable immutable carrier facts paired with the exact captured control
-- history. No donor resource, local activity, debt, or applied owner is retained.
-- Missing resolved-meaning keys are significant for dominated/suppressed entries
-- and remain missing when the base is installed.
data StructuralCarrierBase
  = StructuralCarrierBase
      !HeraldMembershipGenerationId
      !MemberSetDigest
      !(Set HeraldEpoch)
      !(Map HeraldMembershipGenerationId SuccessorStructuralBase)
      !(Map HeraldMembershipGenerationId HeraldMembershipLineage)
      !(Map JoinBootstrap.MembershipAdmissionOrigin (ControlIndex, JoinBootstrap.HeraldBootstrapContribution))
      !(Map GlobalObjectId AppliedEntry)
      !(Map StructuralOccurrenceId AppliedEntry)
      !(Map StructuralOccurrenceId StructuralRetirementSuppression)
      !(Map StructuralProjectionProvenance PortableStructuralMeaning)
      !StructuralControlBase
  deriving stock (Eq, Show)

captureStructuralCarrierBase :: StructuralAppliedState -> StructuralCarrierBase
captureStructuralCarrierBase state =
  StructuralCarrierBase
    state.membershipGenerationId
    state.memberSetDigest
    state.membership
    state.membershipBases
    state.admissionBases
    state.admissionContributions
    state.baselineWinners
    state.history
    state.retirementSuppressions
    (Map.map portableMeaning state.projectionMeanings)
    (captureStructuralControlBase state)
  where
    portableMeaning = \case
      CheckedNeutral object -> PortableNeutral object
      CheckedEdge object payload -> PortableEdge object payload
      CheckedNabla object nabla typedSort controller _ -> PortableNabla object nabla typedSort controller
      CheckedDelta object delta typedSort controller _ -> PortableDelta object delta typedSort controller

structuralCarrierBaseControlBase :: StructuralCarrierBase -> StructuralControlBase
structuralCarrierBaseControlBase (StructuralCarrierBase _ _ _ _ _ _ _ _ _ _ controls) = controls

-- | Checked publications with their original carrier-sort occurrence, including
-- genesis/baseline and dominated/suppressed history. Composition restores the
-- receiver's Controlled observations from these immutable inputs.
structuralCarrierBasePublications :: StructuralCarrierBase -> [(SortOccurrence, CheckedPublication)]
structuralCarrierBasePublications (StructuralCarrierBase _ _ _ _ _ _ baselines occurrences _ _ _) =
  map
    (\entry -> (entryCarrierSort entry, entryCheckedPublication entry))
    (Map.elems baselines <> Map.elems occurrences)
  where
    entryCheckedPublication entry = case entry.source of
      GenesisEntrySource _ _ _ publication -> publication
      BaselineEntrySource _ publication -> publication
      OccurrenceEntrySource application -> application.publication

-- | Original applied occurrence facts in ascending occurrence order, including
-- dominated and retirement-suppressed entries. These retain the predecessor,
-- carrier sort, checked publication and control prerequisite without changing
-- any resolved meaning. The carrier owner does not retain a complete occurrence
-- stamp or source topology: this view cannot authenticate their digest or the
-- original source process and strength carried in the separate semantic payload.
structuralCarrierBaseApplications :: StructuralCarrierBase -> [StructuralApplication]
structuralCarrierBaseApplications (StructuralCarrierBase _ _ _ _ _ _ _ occurrences _ _ _) =
  mapMaybe entryApplication (Map.elems occurrences)

-- | Concrete admitted context, detached from the Projection/Registry owners.
-- Original occurrence evidence is ephemeral input to decoding, not an invented
-- field of the historical carrier owner. Extra retained originals are allowed.
data StructuralCarrierBaseContext
  = StructuralCarrierBaseContext
      !SystemId
      !ControlIndex
      !Membership.HeraldMembershipHistory
      !(Map SortOccurrence Registry.RegistryEntry)
      ![Registry.RegularSortRetirementView]
      !(Map HeraldMembershipGenerationId SuccessorStructuralBase)
      !(Map HeraldMembershipGenerationId HeraldMembershipLineage)
      !(Map JoinBootstrap.MembershipAdmissionOrigin (ControlIndex, JoinBootstrap.HeraldBootstrapContribution))
      !(Map StructuralOccurrenceId Terminal.TerminalStructuralOccurrence)

data StructuralCarrierBaseCodecProblem
  = StructuralCarrierBaseMalformed String
  | StructuralCarrierBaseInconsistent String
  | StructuralCarrierBaseImportProblem StructuralReconciliationProblem
  deriving stock (Eq, Show)

-- | The enclosing Progress owner has admitted these membership certificates.
-- Recheck their keys and shared nominal coordinates without replaying old
-- commands. Admission contributions are derived from the bound system/recipe.
structuralCarrierBaseContext ::
  SystemId ->
  ControlIndex ->
  Membership.HeraldMembershipHistory ->
  Registry.State ->
  [SuccessorStructuralBase] ->
  [(HeraldMembershipLineage, Topology.HeraldJoinBaseRecipe)] ->
  Map StructuralOccurrenceId Terminal.TerminalStructuralOccurrence ->
  Either StructuralCarrierBaseCodecProblem StructuralCarrierBaseContext
structuralCarrierBaseContext system control history registry bases admissions originals = do
  let generations = Membership.heraldMembershipHistoryGenerations history
      known = Registry.registryEntries registry <> mapMaybe Registry.regularSortRetirementEntry (Registry.regularSortRetirements registry)
      entries = Map.fromList [(sortOccurrence (Registry.registryEntrySortId entry) (Registry.registryEntryOccurrenceId entry), entry) | entry <- known]
      retirementBases = Map.fromList [(successorStructuralBaseGenerationId base, base) | base <- bases]
      admissionBases = Map.fromList [(heraldMembershipGenerationId (heraldMembershipLineageTarget lineage), lineage) | (lineage, _) <- admissions]
  carrierBaseRequire
    ( Membership.genesisHeraldMembershipGeneration system (Membership.heraldMembershipGenerationActiveHeraldEpochs (NonEmpty.head generations)) == Right (NonEmpty.head generations)
        && all (maybe True (<= control) . Membership.heraldMembershipGenerationChangeControlIndex) generations
        && Map.size retirementBases == length bases
        && Map.size admissionBases == length admissions
        && Set.null (Map.keysSet retirementBases `Set.intersection` Map.keysSet admissionBases)
        && all (\entry -> Map.lookup (sortOccurrence (Registry.registryEntrySortId entry) (Registry.registryEntryOccurrenceId entry)) entries == Just entry) known
    )
    "carrier context membership/definition coordinates disagree"
  mapM_ (checkLineage . successorStructuralBaseLineage) bases
  contributions <- traverse admitAdmission admissions
  let byOrigin = Map.fromList [(JoinBootstrap.contributionOrigin contribution, (index, contribution)) | (index, contribution) <- contributions]
  carrierBaseRequire (Map.size byOrigin == length contributions) "duplicate carrier admission contribution"
  copiedOriginals <- Map.traverseWithKey admitOriginal originals
  pure (StructuralCarrierBaseContext system control history entries (Registry.regularSortRetirements registry) retirementBases admissionBases byOrigin copiedOriginals)
  where
    checkLineage lineage =
      carrierBaseRequire
        (Membership.heraldMembershipLineage (heraldMembershipGenerationId (heraldMembershipLineageOrigin lineage)) (heraldMembershipGenerationId (heraldMembershipLineageTarget lineage)) history == Right lineage)
        "carrier certificate lineage differs from admitted history"
    admitAdmission (lineage, recipe) = do
      checkLineage lineage
      let target = heraldMembershipLineageTarget lineage
      index <- maybe (Left (StructuralCarrierBaseInconsistent "admission target lacks activation")) Right (Membership.heraldMembershipGenerationChangeControlIndex target)
      contribution <- carrierBaseClaim (JoinBootstrap.heraldBootstrapContribution system (Topology.heraldJoinBaseRecipeAdmissionId recipe) (Topology.heraldJoinBaseRecipeApplicant recipe) Set.empty Set.empty)
      carrierBaseRequire
        ( heraldMembershipLineageOrigin lineage == Topology.heraldJoinBaseRecipeGeneration recipe
            && index <= control
            && fmap fst (Topology.activateHeraldJoinBase index (Topology.topologyCutOccurrenceDigest (Topology.heraldJoinBaseRecipeEstablishedCut recipe)) recipe) == Right target
            && JoinBootstrap.contributionDigest contribution == Topology.heraldJoinBaseRecipeContributionDigest recipe
        )
        "carrier admission recipe differs from checked generation/contribution"
      pure (index, contribution)
    admitOriginal key original = do
      checked <- carrierBaseClaim (Terminal.decodeTerminalStructuralOccurrenceCanonicalBytes (ByteString.copy (Terminal.terminalStructuralOccurrenceCanonicalBytes original)))
      carrierBaseRequire (Terminal.terminalStructuralOccurrenceId checked == key) "original occurrence map key mismatch"
      pure checked

type RawCarrierSort = (ByteString, ByteString)
type RawCarrierPublication = (ByteString, ByteString, ByteString, Word64, ByteString)
type RawCarrierController = (Word8, [ByteString])
type RawCarrierVertex = (Word8, ByteString)
type RawCarrierMeaning = (Word8, Maybe RawCarrierSort, Maybe RawCarrierController, Maybe (RawCarrierVertex, RawCarrierVertex))
type RawCarrierSuppression = (Word8, ByteString, Word64, Word64)
type RawCarrierBaseline = (Word8, RawCarrierSort, Word64, RawCarrierPublication, Maybe RawCarrierMeaning)
type RawCarrierOccurrence = ((ByteString, Word64), Maybe RawCarrierMeaning, Maybe RawCarrierSuppression)
type RawCarrierCause = (Word8, ByteString, Word64, Word64)
type RawCarrierControl = (ByteString, RawCarrierCause, Maybe RawCarrierController)
type RawCarrierBase =
  (ByteString, ByteString, ([ByteString], [ByteString], [(Word64, ByteString)]), [RawCarrierBaseline], [RawCarrierOccurrence], [RawCarrierControl])

-- | Explicit immutable facts and references to the separately supplied exact
-- original occurrence archive. No local activity, resources, debts or private
-- owner state are encoded. Current overlays and intrinsic caches are derived.
encodeStructuralCarrierBase :: StructuralCarrierBase -> ByteString
encodeStructuralCarrierBase (StructuralCarrierBase generation _ _ bases admissions contributions baselines occurrences suppressions meanings (StructuralControlBase _ controls)) = Codec.encode transcript
  where
    transcript :: RawCarrierBase
    transcript =
      ( "ECLIPS-STRUCTURAL-CARRIER-BASE",
        Membership.heraldMembershipGenerationIdBytes generation,
        (map Membership.heraldMembershipGenerationIdBytes (Map.keys bases), map Membership.heraldMembershipGenerationIdBytes (Map.keys admissions), [(controlIndexWord64 index, JoinBootstrap.contributionCanonicalBytes contribution) | (index, contribution) <- Map.elems contributions]),
        map rawBaseline (Map.elems baselines),
        [(rawOccurrenceId ident, rawMeaning <$> Map.lookup (entryProvenance entry) meanings, rawSuppression <$> Map.lookup ident suppressions) | (ident, entry) <- Map.toAscList occurrences],
        [(globalObjectIdBytes object, rawCause (overlayCause overlay), overlayController overlay) | ((_, object), overlay) <- Map.toAscList controls]
      )
    rawBaseline entry =
      let (tag, index, publication) = case entry.source of
            GenesisEntrySource _ _ at value -> (0, at, value)
            BaselineEntrySource _ value -> (1, controlIndex 0, value)
            OccurrenceEntrySource _ -> error "checked carrier baseline contains occurrence source"
       in (tag, rawSort (entryCarrierSort entry), controlIndexWord64 index, rawPublication publication, rawMeaning <$> Map.lookup (entryProvenance entry) meanings)
    rawPublication publication =
      let ident = checkedPublicationId publication
       in (nablaIdBytes (publicationNabla ident), authorityEpochCanonicalBytes (publicationAuthorityEpoch ident), heraldEpochBytes (publicationSourceHeraldEpoch ident), nablaSequenceWord64 (publicationNablaSequence ident), Value.canonicalValueByteString (Publication.checkedPublicationCanonicalValue publication))
    rawMeaning = \case
      PortableNeutral _ -> (0, Nothing, Nothing, Nothing)
      PortableEdge _ payload -> (1, Nothing, Nothing, Just (rawVertex (edgeSource payload), rawVertex (edgeDestination payload)))
      PortableNabla _ _ typed controller -> (2, Just (rawSort typed), Just (rawController controller), Nothing)
      PortableDelta _ _ typed controller -> (3, Just (rawSort typed), Just (rawController controller), Nothing)
    rawVertex = \case
      NeutralVertex object -> (0, globalObjectIdBytes object)
      NablaVertex nabla -> (1, nablaIdBytes nabla)
      DeltaVertex delta -> (2, deltaIdBytes delta)
    rawSuppression = \case
      DirectRegularSortRetirementSuppression sortId index -> (0, sortIdBytes sortId, controlIndexWord64 index, 0)
      InheritedEndpointRetirementSuppression ident index -> (1, heraldEpochBytes (structuralOccurrenceSourceHeraldEpoch ident), structuralSequenceWord64 (structuralOccurrenceSourceSequence ident), controlIndexWord64 index)
    rawCause cause = case Consequence.structuralConsequenceCauseView cause of
      Consequence.LabelReleaseCauseView ident index -> (1, Identity.labelDecisionIdBytes ident, 0, controlIndexWord64 index)
      Consequence.ProcessEndCauseView process index -> (2, processEpochIdBytes process, 0, controlIndexWord64 index)
      Consequence.PredefinedDisappearanceCauseView probe index -> (3, Disappearance.disappearanceProbeIdBytes probe, controlIndexWord64 (Disappearance.disappearanceProbeOpenControlIndex probe), controlIndexWord64 index)
      Consequence.StructuralOccurrenceCauseView _ -> error "checked structural control lacks Oracle coordinate"
    overlayCause = \case
      StructuralDeletedOverlayView cause -> cause
      StructuralControllerOverlayView cause _ -> cause
    overlayController = \case
      StructuralDeletedOverlayView _ -> Nothing
      StructuralControllerOverlayView _ controller -> Just (rawController controller)
    rawController = \case
      VoidController -> (0, [])
      LiveProcessController process herald -> (1, [processEpochIdBytes process, heraldEpochBytes herald])
      ZombieProcessController process -> (2, [processEpochIdBytes process])
    rawSort typed = (sortIdBytes (sortOccurrenceSortId typed), sortDefinitionOccurrenceIdBytes (sortOccurrenceDefinition typed))
    rawOccurrenceId ident = (heraldEpochBytes (structuralOccurrenceSourceHeraldEpoch ident), structuralSequenceWord64 (structuralOccurrenceSourceSequence ident))

-- | Admit supplied facts before invoking the existing fresh-observer importer.
-- Original controller meaning is checked against immutable process residence,
-- including ended processes. End/handover remain separately ordered overlays;
-- decoding neither resolves through current labels nor synthesizes missed Ends.
decodeStructuralCarrierBase ::
  StructuralCarrierBaseContext ->
  ReconciliationViews ->
  StructuralAppliedState ->
  ByteString ->
  Either StructuralCarrierBaseCodecProblem StructuralCarrierBase
decodeStructuralCarrierBase
  (StructuralCarrierBaseContext _ control history definitions retirements bases admissions contributions originals)
  views
  receiver
  bytes = do
    (domain, rawGeneration, rawCertificates, rawBaselines, rawOccurrences, rawControls) <- carrierBaseClaim (Codec.decode bytes :: Either String RawCarrierBase)
    let membership = Membership.heraldMembershipHistoryCurrent history
        generation = heraldMembershipGenerationId membership
        expectedCertificates = (map Membership.heraldMembershipGenerationIdBytes (Map.keys bases), map Membership.heraldMembershipGenerationIdBytes (Map.keys admissions), [(controlIndexWord64 index, JoinBootstrap.contributionCanonicalBytes contribution) | (index, contribution) <- Map.elems contributions])
    carrierBaseRequire
      (domain == "ECLIPS-STRUCTURAL-CARRIER-BASE" && rawGeneration == Membership.heraldMembershipGenerationIdBytes generation && rawCertificates == expectedCertificates)
      "carrier domain/membership/certificate references differ"
    baselineRows <- traverse decodeBaseline rawBaselines
    occurrenceRows <- traverse decodeOccurrence rawOccurrences
    controlRows <- traverse decodeControl rawControls
    let baselines = Map.fromList [(entryObject entry, entry) | (entry, _) <- baselineRows]
        occurrences = Map.fromList [(structuralApplicationOccurrence application, entry) | (entry, _, _) <- occurrenceRows, OccurrenceEntrySource application <- [entry.source]]
        allEntries = Map.elems baselines <> Map.elems occurrences
        publications = mapMaybe (fmap checkedPublicationId . entryPublication) allEntries
        roleClaims = Map.fromListWith Set.union [(entryObject entry, Set.singleton (entryRole entry)) | entry <- allEntries]
    carrierBaseRequire (Set.size (Set.fromList publications) == length publications) "retained carrier publication identity is reused"
    carrierBaseRequire (all ((== 1) . Set.size) (Map.elems roleClaims)) "retained object changes structural role"
    baselineMeanings <- traverse (admitMeaning roleClaims) baselineRows
    occurrenceMeanings <- traverse (\(entry, meaning, _) -> admitMeaning roleClaims (entry, meaning)) occurrenceRows
    let meanings = Map.fromList (catMaybes (baselineMeanings <> occurrenceMeanings))
        suppressions = Map.fromList [(structuralApplicationOccurrence application, suppression) | (entry, _, Just suppression) <- occurrenceRows, OccurrenceEntrySource application <- [entry.source]]
        controls = Map.fromList controlRows
        overlays = Map.foldlWithKey' (\retained (_, object) overlay -> Map.insert object overlay retained) Map.empty controls
        base = StructuralCarrierBase generation (heraldMembershipGenerationActiveMemberSetDigest membership) (Set.fromList (NonEmpty.toList (Membership.heraldMembershipGenerationActiveHeraldEpochs membership))) bases admissions contributions baselines occurrences suppressions meanings (StructuralControlBase overlays controls)
    carrierBaseRequire (encodeStructuralCarrierBase base == bytes) "noncanonical carrier base"
    let current = winningEntriesFrom (Map.elems baselines <> [entry | (ident, entry) <- Map.toAscList occurrences, Map.notMember ident suppressions])
    carrierBaseRequire
      (all (\entry -> Map.member (entryProvenance entry) meanings) (Map.elems baselines <> Map.elems current))
      "baseline/current carrier lacks its retained meaning"
    prepared <- carrierBaseImport (prepareStructuralCarrierBaseImport views base receiver)
    carrierBaseClaim (validateStructuralAppliedState (preparedStructuralCarrierBaseSuccessor prepared))
    mapM_ (checkSuppression occurrences) (Map.toAscList suppressions)
    mapM_ (checkPredecessor occurrences) (Map.elems occurrences)
    pure base
    where
      decodeBaseline (tag, rawSort, rawControl, rawPublication, rawMeaning) = do
        typed <- decodeSort rawSort
        publication <- decodePublication typed rawPublication
        intrinsic <- carrierBaseImport (decodeCarrierPublication publication)
        let object = decodedCarrierObject intrinsic
            at = controlIndex rawControl
        source <- case tag of
          0 -> do
            carrierBaseRequire (at <= control) "genesis carrier exceeds control cut"
            pure (GenesisEntrySource object typed at publication)
          1 -> do
            carrierBaseRequire (at == controlIndex 0) "published baseline has a fabricated control coordinate"
            pure (BaselineEntrySource typed publication)
          _ -> Left (StructuralCarrierBaseMalformed "unknown carrier baseline tag")
        pure (AppliedEntry source intrinsic, rawMeaning)
      decodePublication typed (rawNabla, rawAuthority, rawHerald, rawSequence, rawValue) = do
        definition <- requireDefinition typed
        nabla <- carrierBaseClaim (Identity.mkNablaId (ByteString.copy rawNabla))
        authority <- carrierBaseClaim (Identity.decodeAuthorityEpochCanonicalBytes (ByteString.copy rawAuthority))
        herald <- carrierBaseClaim (Identity.mkHeraldEpoch (ByteString.copy rawHerald))
        value <- carrierBaseClaim (Value.decodeCanonicalValue (ByteString.copy rawValue))
        carrierBaseClaim (Publication.mkCheckedPublication (Registry.registryEntryDescriptor definition) (Identity.publicationId nabla authority herald (Identity.nablaSequence rawSequence)) value)
      decodeOccurrence (rawIdent, rawMeaning, rawSuppression) = do
        ident <- decodeOccurrenceId rawIdent
        original <- maybe (Left (StructuralCarrierBaseInconsistent "missing original structural payload")) Right (Map.lookup ident originals)
        semantics <- carrierBaseClaim (Peer.decodeStructuralPublicationCanonicalBytes (ByteString.copy (Terminal.terminalStructuralOccurrencePayloadBytes original)))
        let stamp = Terminal.terminalStructuralOccurrenceStamp original
            predecessor = structuralOccurrenceStampPredecessor stamp
            publicationId' = Peer.structuralPublicationSemanticsPublicationId semantics
            typed = sortOccurrence (Peer.structuralPublicationSemanticsSortId semantics) (Peer.structuralPublicationSemanticsOccurrenceId semantics)
            prerequisite = Peer.structuralPublicationSemanticsControlPrerequisite semantics
        membership <- maybe (Left (StructuralCarrierBaseInconsistent "original predecessor membership unknown")) Right (Membership.lookupHeraldMembershipGeneration (structuralVersionVectorMembershipGenerationId predecessor) history)
        carrierBaseRequire
          ( Structural.mkStructuralVersionVector membership (structuralVersionVectorEntries predecessor) == Right predecessor
              && structuralOccurrenceStampPublication stamp == publicationId'
              && Peer.structuralOccurrenceStampCarrierRole stamp == Peer.structuralPublicationSemanticsCarrierRole semantics
              && prerequisite <= control
          )
          "original stamp and semantic payload disagree"
        definition <- requireDefinition typed
        value <- carrierBaseClaim (Value.decodeCanonicalValue (ByteString.copy (Value.canonicalValueByteString (Peer.structuralPublicationSemanticsCanonicalValue semantics))))
        publication <- carrierBaseClaim (Publication.mkCheckedPublication (Registry.registryEntryDescriptor definition) publicationId' value)
        intrinsic <- carrierBaseImport (decodeCarrierPublication publication)
        carrierBaseRequire (decodedCarrierRole intrinsic == Peer.structuralOccurrenceStampCarrierRole stamp) "original stamp role differs from carrier"
        suppression <- traverse decodeSuppression rawSuppression
        pure (AppliedEntry (OccurrenceEntrySource (StructuralApplication ident predecessor typed publication prerequisite)) intrinsic, rawMeaning, suppression)
      decodeSort (rawSort, rawOccurrence) = sortOccurrence <$> carrierBaseClaim (Identity.mkSortId (ByteString.copy rawSort)) <*> carrierBaseClaim (Identity.mkSortDefinitionOccurrenceId (ByteString.copy rawOccurrence))
      requireDefinition typed = maybe (Left (StructuralCarrierBaseInconsistent "carrier/typed sort definition was never observed")) Right (Map.lookup typed definitions)
      decodeOccurrenceId (rawHerald, rawSequence) = structuralOccurrenceId <$> carrierBaseClaim (Identity.mkHeraldEpoch (ByteString.copy rawHerald)) <*> carrierBaseClaim (Identity.mkStructuralSequence rawSequence)
      decodeController (tag, fields) = case (tag, fields) of
        (0, []) -> pure VoidController
        (1, [rawProcess, rawHerald]) -> do
          process <- carrierBaseClaim (Identity.mkProcessEpochId (ByteString.copy rawProcess))
          herald <- carrierBaseClaim (Identity.mkHeraldEpoch (ByteString.copy rawHerald))
          carrierBaseRequire (residence process == Just herald) "controller residence differs from admitted process"
          pure (LiveProcessController process herald)
        (2, [rawProcess]) -> ZombieProcessController <$> carrierBaseClaim (Identity.mkProcessEpochId (ByteString.copy rawProcess))
        _ -> Left (StructuralCarrierBaseMalformed "unknown portable controller shape")
      residence process = case Map.lookup process views.liveProcessResidences of
        Just herald -> Just herald
        Nothing -> fst <$> Map.lookup process views.endedProcesses
      expectedController (owner, _) = case owner of
        VoidLabel -> pure VoidController
        ZombieLabel process -> pure (ZombieProcessController process)
        ProcessLabel process -> maybe (Left (StructuralCarrierBaseInconsistent "original controller has no process residence")) (Right . LiveProcessController process) (residence process)
      admitMeaning _ (_, Nothing) = pure Nothing
      admitMeaning roles (entry, Just (tag, typed, controller, endpoints)) = do
        meaning <- case (tag, entry.intrinsic, typed, controller, endpoints) of
          (0, DecodedNeutral object _, Nothing, Nothing, Nothing) -> pure (PortableNeutral object)
          (1, DecodedEdge object _ source destination strength, Nothing, Nothing, Just (rawSource, rawDestination)) -> do
            sourceVertex <- decodeVertex rawSource
            destinationVertex <- decodeVertex rawDestination
            carrierBaseRequire (vertexObject sourceVertex == source && vertexObject destinationVertex == destination && vertexKnown roles sourceVertex && vertexKnown roles destinationVertex) "original edge endpoint role mismatch"
            pure (PortableEdge object (edgePayload sourceVertex destinationVertex strength))
          (2, DecodedNabla object label sortId, Just rawTyped, Just rawController, Nothing) -> PortableNabla object (nablaIdFromGlobalObjectId object) <$> checkRoot sortId rawTyped <*> checkController label rawController
          (3, DecodedDelta object label sortId, Just rawTyped, Just rawController, Nothing) -> PortableDelta object (deltaIdFromGlobalObjectId object) <$> checkRoot sortId rawTyped <*> checkController label rawController
          _ -> Left (StructuralCarrierBaseInconsistent "portable meaning differs from intrinsic carrier")
        pure (Just (entryProvenance entry, meaning))
      checkRoot sortId raw = do
        typed <- decodeSort raw
        _ <- requireDefinition typed
        carrierBaseRequire (sortOccurrenceSortId typed == sortId) "original typed sort differs from carrier"
        pure typed
      checkController label raw = do
        controller <- decodeController raw
        expected <- expectedController label
        carrierBaseRequire (controller == expected) "original meaning changed controller"
        pure controller
      decodeVertex (tag, raw) = case tag of
        0 -> NeutralVertex <$> carrierBaseClaim (Identity.mkGlobalObjectId (ByteString.copy raw))
        1 -> NablaVertex <$> carrierBaseClaim (Identity.mkNablaId (ByteString.copy raw))
        2 -> DeltaVertex <$> carrierBaseClaim (Identity.mkDeltaId (ByteString.copy raw))
        _ -> Left (StructuralCarrierBaseMalformed "unknown edge endpoint kind")
      vertexObject = \case
        NeutralVertex object -> object
        NablaVertex nabla -> Identity.globalObjectIdFromNablaId nabla
        DeltaVertex delta -> Identity.globalObjectIdFromDeltaId delta
      admissionVertices = Set.fromList [DeltaVertex delta | (_, contribution) <- Map.elems contributions, (_, delta, _) <- JoinBootstrap.contributionViews contribution]
      vertexKnown roles vertex = Set.member vertex views.baselineVertices || Set.member vertex admissionVertices || Map.lookup (vertexObject vertex) roles == Just (Set.singleton (case vertex of NeutralVertex _ -> NeutralVertexCarrier; NablaVertex _ -> NablaCarrier; DeltaVertex _ -> DeltaCarrier))
      decodeSuppression (tag, raw, first, second) = case tag of
        0 -> do
          sortId <- carrierBaseClaim (Identity.mkSortId (ByteString.copy raw))
          carrierBaseRequire (first > 0 && second == 0 && controlIndex first <= control) "direct retirement suppression coordinate invalid"
          pure (DirectRegularSortRetirementSuppression sortId (controlIndex first))
        1 -> do
          ident <- decodeOccurrenceId (raw, first)
          carrierBaseRequire (second > 0 && controlIndex second <= control) "inherited retirement suppression coordinate invalid"
          pure (InheritedEndpointRetirementSuppression ident (controlIndex second))
        _ -> Left (StructuralCarrierBaseMalformed "unknown retirement suppression")
      decodeControl (rawObject, (tag, rawIdentity, rawOpened, rawIndex), rawController) = do
        object <- carrierBaseClaim (Identity.mkGlobalObjectId (ByteString.copy rawObject))
        let index = controlIndex rawIndex
        carrierBaseRequire (index <= control) "structural control exceeds admitted cut"
        cause <- case tag of
          1 -> do
            carrierBaseRequire (rawOpened == 0) "label cause carries probe coordinate"
            decision <- carrierBaseClaim (Identity.mkLabelDecisionId (ByteString.copy rawIdentity))
            carrierBaseClaim (Consequence.labelReleaseCause decision index)
          2 -> do
            carrierBaseRequire (rawOpened == 0) "End cause carries probe coordinate"
            process <- carrierBaseClaim (Identity.mkProcessEpochId (ByteString.copy rawIdentity))
            carrierBaseRequire (maybe False ((== index) . snd) (Map.lookup process views.endedProcesses)) "End cause differs from admitted process End"
            carrierBaseClaim (Consequence.processEndCause process index)
          3 -> do
            probe <- carrierBaseClaim (Disappearance.deriveDisappearanceProbeId (controlIndex rawOpened))
            carrierBaseRequire (Disappearance.disappearanceProbeIdBytes probe == rawIdentity && rawOpened < rawIndex) "disappearance cause probe/Resolve mismatch"
            carrierBaseClaim (Consequence.predefinedDisappearanceCause probe index)
          _ -> Left (StructuralCarrierBaseMalformed "unknown ordered structural cause")
        controller <- traverse decodeController rawController
        case (Consequence.structuralConsequenceCauseView cause, controller) of
          (Consequence.ProcessEndCauseView process _, Just (ZombieProcessController actual)) -> carrierBaseRequire (process == actual) "End controls wrong process"
          (Consequence.ProcessEndCauseView {}, _) -> Left (StructuralCarrierBaseInconsistent "End control is not Zombie")
          (Consequence.PredefinedDisappearanceCauseView {}, Nothing) -> pure ()
          (Consequence.PredefinedDisappearanceCauseView {}, _) -> Left (StructuralCarrierBaseInconsistent "disappearance control is not deletion")
          _ -> pure ()
        pure ((index, object), maybe (StructuralDeletedOverlayView cause) (StructuralControllerOverlayView cause) controller)
      checkSuppression occurrences (ident, suppression) = case suppression of
        InheritedEndpointRetirementSuppression {} -> pure () -- paired-owner audit checks the exact covered suppressed endpoint
        DirectRegularSortRetirementSuppression sortId index -> do
          let matches entry = case entry.intrinsic of DecodedNabla _ _ typed -> typed == sortId; DecodedDelta _ _ typed -> typed == sortId; _ -> False
          carrierBaseRequire
            ( maybe False matches (Map.lookup ident occurrences)
                && any (\retirement -> Registry.regularSortRetirementSortId retirement == sortId && Registry.regularSortRetirementResolveIndex retirement == index) retirements
            )
            "direct suppression lacks matching Registry retirement"
      checkPredecessor occurrences entry = case entry.source of
        OccurrenceEntrySource application -> do
          let covers (source, prefix) = maybe True (\sequenceNumber -> Map.member (structuralOccurrenceId source sequenceNumber) occurrences) (structuralPrefixSequence prefix)
          carrierBaseRequire (all covers (structuralVersionVectorEntries application.predecessor)) "carrier predecessor history is incomplete"
        _ -> pure ()

carrierBaseImport :: Either StructuralReconciliationProblem value -> Either StructuralCarrierBaseCodecProblem value
carrierBaseImport = either (Left . StructuralCarrierBaseImportProblem) Right

carrierBaseClaim :: (Show problem) => Either problem value -> Either StructuralCarrierBaseCodecProblem value
carrierBaseClaim = either (Left . StructuralCarrierBaseMalformed . show) Right

carrierBaseRequire :: Bool -> String -> Either StructuralCarrierBaseCodecProblem ()
carrierBaseRequire condition detail = unless condition (Left (StructuralCarrierBaseInconsistent detail))

data PreparedStructuralCarrierBaseImport
  = PreparedStructuralCarrierBaseImport
      !StructuralAppliedState
      !StructuralAppliedState
      !StructuralGraphPatch
  deriving stock (Eq, Show)

preparedStructuralCarrierBasePredecessor :: PreparedStructuralCarrierBaseImport -> StructuralAppliedState
preparedStructuralCarrierBasePredecessor (PreparedStructuralCarrierBaseImport predecessor _ _) = predecessor

preparedStructuralCarrierBaseSuccessor :: PreparedStructuralCarrierBaseImport -> StructuralAppliedState
preparedStructuralCarrierBaseSuccessor (PreparedStructuralCarrierBaseImport _ successor _) = successor

preparedStructuralCarrierBaseGraphPatch :: PreparedStructuralCarrierBaseImport -> StructuralGraphPatch
preparedStructuralCarrierBaseGraphPatch (PreparedStructuralCarrierBaseImport _ _ patch) = patch

-- | Install a paired carrier/control history into a fresh observer's private
-- reconstruction. Original sorts, process residences and End/handover ordering
-- are copied as already admitted facts, never reinterpreted through current
-- views. Only receiver activity is derived anew. Fresh observers own no Normal
-- possession or Store; admission therefore creates no historical resource/debt.
-- Composition checks the common control cut and commits the aggregate Graph
-- patch together with its progress/certificate and other imported owners.
prepareStructuralCarrierBaseImport ::
  ReconciliationViews ->
  StructuralCarrierBase ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem PreparedStructuralCarrierBaseImport
prepareStructuralCarrierBaseImport
  views
  (StructuralCarrierBase generation digest members bases admissions contributions baselines occurrences suppressions meanings (StructuralControlBase overlays history))
  predecessor = do
    unless fresh (Left StructuralCarrierBaseReceiverNotFresh)
    unless
      (predecessor.baselineWinners == Map.filter isGenesis baselines)
      (Left StructuralCarrierBaseGenesisMismatch)
    summaries <- rebuildStructuralSourceHistory occurrences
    let receiverEpoch = views.localHerald
        restoredMeanings = Map.map (restoreMeaning receiverEpoch) meanings
        restoredOverlays = Map.map (restoreOverlay receiverEpoch) overlays
        restoredHistory = Map.map (restoreOverlay receiverEpoch) history
        staged =
          StructuralAppliedState
            { membershipGenerationId = generation,
              memberSetDigest = digest,
              membership = members,
              membershipBases = bases,
              admissionBases = admissions,
              admissionContributions = contributions,
              baselineWinners = baselines,
              history = occurrences,
              sourceHistory = summaries,
              retirementSuppressions = suppressions,
              currentWinners = Map.empty,
              projectionMeanings = restoredMeanings,
              localStores = Map.empty,
              retainedStoreResources = Map.empty,
              projectionStoreResources = Map.empty,
              usedStoreIncarnations = Map.empty,
              controlOverlays = restoredOverlays,
              controlHistory = restoredHistory
            }
        successor = staged {currentWinners = winningEntriesAt Nothing staged}
        patch =
          StructuralGraphPatch
            (exactChanges (structuralAppliedVertexProjections predecessor) (structuralAppliedVertexProjections successor))
            (exactChanges (structuralAppliedEdgeProjections predecessor) (structuralAppliedEdgeProjections successor))
    -- Historical fields must not keep the temporary receiver views alive.
    -- Strict map construction also releases the portable-meaning conversion
    -- closures before the imported owner escapes this preparation boundary.
    receiverEpoch
      `seq` restoredMeanings
      `seq` restoredOverlays
      `seq` restoredHistory
      `seq` Right (PreparedStructuralCarrierBaseImport predecessor successor patch)
    where
      fresh =
        Set.null views.normalPossessions
          && Map.null predecessor.history
          && Map.null predecessor.retirementSuppressions
          && Map.null predecessor.membershipBases
          && Map.null predecessor.admissionBases
          && Map.null predecessor.admissionContributions
          && Map.null predecessor.localStores
          && Map.null predecessor.retainedStoreResources
          && Map.null predecessor.projectionStoreResources
          && Map.null predecessor.usedStoreIncarnations
          && Map.null predecessor.controlOverlays
          && Map.null predecessor.controlHistory
          && all isGenesis (Map.elems predecessor.baselineWinners)
      isGenesis entry = case entry.source of
        GenesisEntrySource {} -> True
        _ -> False
      restoreOverlay receiverEpoch = \case
        StructuralDeletedOverlayView cause -> StructuralDeletedOverlay cause
        StructuralControllerOverlayView cause controller ->
          StructuralControllerOverlay cause controller (observerControllerActivity receiverEpoch controller)
      restoreMeaning receiverEpoch = \case
        PortableNeutral object -> CheckedNeutral object
        PortableEdge object payload -> CheckedEdge object payload
        PortableNabla object nabla typedSort controller ->
          CheckedNabla object nabla typedSort controller (observerControllerActivity receiverEpoch controller)
        PortableDelta object delta typedSort controller ->
          CheckedDelta object delta typedSort controller (observerControllerActivity receiverEpoch controller)
      exactChanges before after =
        Map.fromAscList
          [ (key, ExactChange old new)
          | key <- Set.toAscList (Set.union (Map.keysSet before) (Map.keysSet after)),
            let old = Map.lookup key before,
            let new = Map.lookup key after,
            old /= new
          ]

-- | Portable ordered control facts from an admitted structural owner. Capture
-- omits every donor-local activity, Store resource, debt and carrier owner.
-- These facts are staged evidence, not an installed structural projection:
-- composition authenticates each cause and supplies the named carrier before
-- any receiver can realize its own controller activity.
data StructuralControlBase
  = StructuralControlBase
      !(Map GlobalObjectId StructuralControlOverlayView)
      !(Map (ControlIndex, GlobalObjectId) StructuralControlOverlayView)
  deriving stock (Eq, Show)

captureStructuralControlBase :: StructuralAppliedState -> StructuralControlBase
captureStructuralControlBase state =
  StructuralControlBase
    (Map.map controlOverlayView state.controlOverlays)
    (Map.map controlOverlayView state.controlHistory)

-- | Restrict staged evidence to carriers already admitted by a private Join
-- reconstruction. The complete base must still be admitted before publishing
-- that reconstruction; a restricted base cannot prove the omitted dependencies.
restrictStructuralControlBaseToObjects ::
  Set GlobalObjectId -> StructuralControlBase -> StructuralControlBase
restrictStructuralControlBaseToObjects objects (StructuralControlBase overlays history) =
  StructuralControlBase
    (Map.restrictKeys overlays objects)
    (Map.filterWithKey (\(_, object) _ -> Set.member object objects) history)

data StructuralControlBasePreparation
  = StructuralControlBaseHeld !(Set StructuralDependency)
  | StructuralControlBaseReady ![StructuralControlBaseStep]
  deriving stock (Eq, Show)

-- | A current consequence carries its exact Oracle cause for Store admission
-- and debt retention. Historical attachment has no new semantic cause.
data StructuralControlBaseStep
  = StructuralControlBaseStep
      !(Maybe StructuralConsequenceCause)
      !PreparedStructuralReconciliation
  deriving stock (Eq, Show)

structuralControlBaseStepCause ::
  StructuralControlBaseStep -> Maybe StructuralConsequenceCause
structuralControlBaseStepCause (StructuralControlBaseStep cause _) = cause

structuralControlBaseStepPreparation ::
  StructuralControlBaseStep -> PreparedStructuralReconciliation
structuralControlBaseStepPreparation (StructuralControlBaseStep _ preparation) = preparation

-- | Prepare the receiver's present consequences and then attach exact historical
-- control evidence. Past handovers never allocate Stores or recreate debts. The
-- returned transactions form one private predecessor/successor chain; composition
-- must apply their Graph, Store and Placement patches before exposing an owner.
-- A held result exposes no partial preparation, even if an earlier object was ready.
prepareStructuralControlBaseImport ::
  ReconciliationViews ->
  StructuralControlBase ->
  Map GlobalObjectId FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralControlBasePreparation
prepareStructuralControlBaseImport views base@(StructuralControlBase overlays history) fresh initial = do
  validateEffectiveSorts views
  validateLocalStores views initial
  mapM_ checkExistingHistory (Map.toAscList initial.controlHistory)
  mapM_ (rejectUnexpectedFresh . Just) (Map.elems (Map.withoutKeys fresh objects))
  if Set.null missing
    then prepareCurrent initial (Map.toAscList overlays) []
    else Right (StructuralControlBaseHeld (Set.map StructuralControlCarrierDependency missing))
  where
    objects = structuralControlBaseObjects base
    retainedObjects =
      Set.fromList
        (map entryObject (Map.elems initial.baselineWinners <> Map.elems initial.history))
    missing = Set.difference objects retainedObjects
    checkExistingHistory (coordinate@(index, object), overlay)
      | Set.notMember object objects = Right ()
      | Map.lookup coordinate history == Just (controlOverlayView overlay) = Right ()
      | otherwise = Left (StructuralControlBaseHistoryConflict index object)
    importedHistory = Map.mapWithKey (\(_, object) -> receiverOverlay object) history
    importedOverlays = Map.mapWithKey receiverOverlay overlays
    receiverOverlay = receiverControlOverlay views
    prepareCurrent predecessor [] prepared =
      let successorHistory = Map.union importedHistory predecessor.controlHistory
          successorOverlays = Map.union importedOverlays predecessor.controlOverlays
          changed = successorHistory /= predecessor.controlHistory || successorOverlays /= predecessor.controlOverlays
          historyPreparation =
            PreparedStructuralReconciliation
              { classification = StructuralProjectionRefresh,
                changesState = True,
                application = Nothing,
                controlledPatch = ControlledPatch Nothing Nothing,
                graphPatch = emptyGraphPatch,
                placementPatch = PlacementPatch Map.empty,
                storePatch = StorePatch Map.empty Map.empty,
                debts = normalizeStructuralDebts [],
                predecessorState = predecessor,
                successor =
                  predecessor
                    { controlHistory = successorHistory,
                      controlOverlays = successorOverlays
                    },
                satisfiedDependencies = Set.empty
              }
       in Right
            ( StructuralControlBaseReady
                (reverse (if changed then StructuralControlBaseStep Nothing historyPreparation : prepared else prepared))
            )
    prepareCurrent predecessor ((object, overlay) : remaining) prepared =
      case winnerForObject object predecessor of
        -- A retained carrier suppressed by regular-sort retirement has no current
        -- projection. Its control history remains queryable without resurrection.
        Nothing -> unchanged
        Just entry -> case effectiveProjectedEntry predecessor entry of
          -- Exact terminal replay has no present consequence either.
          Nothing -> unchanged
          Just _ -> do
            let (cause, replacement) = case receiverOverlay object overlay of
                  StructuralDeletedOverlay cause' -> (cause', Nothing)
                  StructuralControllerOverlay cause' controller activity ->
                    (cause', Just (controller, activity))
            result <-
              prepareStructuralControlRefresh
                views
                cause
                object
                entry
                replacement
                (Map.lookup object fresh)
                predecessor
            case result of
              StructuralHeld dependencies -> Right (StructuralControlBaseHeld dependencies)
              StructuralReady ready ->
                prepareCurrent
                  (preparedStructuralSuccessor ready)
                  remaining
                  (if preparedStructuralChangesState ready then StructuralControlBaseStep (Just cause) ready : prepared else prepared)
      where
        unchanged = do
          rejectUnexpectedFresh (Map.lookup object fresh)
          prepareCurrent predecessor remaining prepared

receiverControlOverlay :: ReconciliationViews -> GlobalObjectId -> StructuralControlOverlayView -> StructuralControlOverlay
receiverControlOverlay views object = \case
  StructuralDeletedOverlayView cause -> StructuralDeletedOverlay cause
  StructuralControllerOverlayView cause controller ->
    StructuralControllerOverlay cause controller (receiverControllerActivity views object controller)

receiverControllerActivity :: ReconciliationViews -> GlobalObjectId -> ControllerProjection -> RootActivityProjection
receiverControllerActivity views object controller =
  localActivity views object controller (observerControllerActivity views.localHerald controller)

observerControllerActivity :: HeraldEpoch -> ControllerProjection -> RootActivityProjection
observerControllerActivity receiverEpoch = \case
  LiveProcessController process residence
    | residence /= receiverEpoch -> RemoteController process residence
  _ -> PassiveRoot

structuralControlBaseOverlays ::
  StructuralControlBase -> [(GlobalObjectId, StructuralControlOverlayView)]
structuralControlBaseOverlays (StructuralControlBase overlays _) = Map.toAscList overlays

structuralControlBaseHistory ::
  StructuralControlBase -> [(ControlIndex, GlobalObjectId, StructuralControlOverlayView)]
structuralControlBaseHistory (StructuralControlBase _ history) =
  [(index, object, overlay) | ((index, object), overlay) <- Map.toAscList history]

-- | Exact immutable carrier dependencies; an unknown object remains a
-- dependency rather than acquiring a fabricated structural overlay.
structuralControlBaseObjects :: StructuralControlBase -> Set GlobalObjectId
structuralControlBaseObjects (StructuralControlBase overlays history) =
  Set.union (Map.keysSet overlays) (Set.fromList [object | (_, object) <- Map.keys history])

structuralControlBaseControlIndices :: StructuralControlBase -> Set ControlIndex
structuralControlBaseControlIndices (StructuralControlBase _ history) =
  Set.fromList [index | (index, _) <- Map.keys history]

structuralControlBaseGreatestControlIndex :: StructuralControlBase -> Maybe ControlIndex
structuralControlBaseGreatestControlIndex (StructuralControlBase _ history) =
  fst . fst <$> Map.lookupMax history

data StructuralAppliedInvariantProblem
  = StructuralAppliedControlHistoryCauseIndexMismatch
      ControlIndex
      GlobalObjectId
      (Maybe ControlIndex)
  | StructuralAppliedControlHistoryObjectMissing GlobalObjectId
  | StructuralAppliedCurrentControlOverlayMismatch
  | StructuralAppliedRetirementSuppressionMismatch StructuralOccurrenceId
  | StructuralAppliedCurrentWinnersMismatch
  | StructuralAppliedSourceHistoryMismatch
  deriving stock (Eq, Show)

-- | Validate the authority-neutral shape of immutable structural control
-- history.  Cross-owner authentication of each cause is performed by the
-- composed Herald invariant, which owns the LabelPatch, Oracle and terminal
-- deletion witnesses.
validateStructuralAppliedState ::
  StructuralAppliedState -> Either StructuralAppliedInvariantProblem ()
validateStructuralAppliedState state = do
  unless
    (rebuildStructuralSourceHistory state.history == Right state.sourceHistory)
    (Left StructuralAppliedSourceHistoryMismatch)
  unless
    (state.currentWinners == winningEntriesAt Nothing state)
    (Left StructuralAppliedCurrentWinnersMismatch)
  mapM_ validateRetirementSuppression (Map.toAscList state.retirementSuppressions)
  mapM_ validateEntry (Map.toAscList state.controlHistory)
  if foldedHistory == state.controlOverlays
    then Right ()
    else Left StructuralAppliedCurrentControlOverlayMismatch
  where
    validateRetirementSuppression (occurrence, suppression) =
      case Map.lookup occurrence state.history >>= entryApplication of
        Just application
          | suppressionControlCompatible application suppression,
            suppressionEvidenceMatches application suppression ->
              Right ()
        _ -> Left (StructuralAppliedRetirementSuppressionMismatch occurrence)
    suppressionEvidenceMatches application = \case
      DirectRegularSortRetirementSuppression {} -> True
      InheritedEndpointRetirementSuppression inherited floorIndex ->
        coversOccurrence application.predecessor inherited
          && maybe
            False
            ( (== floorIndex)
                . structuralRetirementSuppressionControlIndex
            )
            (Map.lookup inherited state.retirementSuppressions)
          && case ( Map.lookup inherited state.history,
                    decodeCarrier application
                  ) of
            (Just inheritedEntry, Right (DecodedEdge _ _ source destination _)) ->
              ( entryObject inheritedEntry == source
                  && winnerForObjectAt (Just application.predecessor) source state == Nothing
              )
                || ( entryObject inheritedEntry == destination
                       && winnerForObjectAt (Just application.predecessor) destination state == Nothing
                   )
            _ -> False

    suppressionControlCompatible application = \case
      DirectRegularSortRetirementSuppression _ floorIndex ->
        application.controlPrerequisite < floorIndex
      InheritedEndpointRetirementSuppression {} -> True

    retainedObjects =
      Set.fromList
        ( fmap
            entryObject
            (Map.elems state.baselineWinners <> Map.elems state.history)
        )
    validateEntry ((index, object), overlay) = do
      let causeIndex = structuralConsequenceCauseControlIndex (controlOverlayCause overlay)
      if causeIndex == Just index
        then Right ()
        else
          Left
            ( StructuralAppliedControlHistoryCauseIndexMismatch
                index
                object
                causeIndex
            )
      if Set.member object retainedObjects
        then Right ()
        else Left (StructuralAppliedControlHistoryObjectMissing object)
    foldedHistory =
      foldl'
        (\overlays ((_, object), overlay) -> Map.insert object overlay overlays)
        Map.empty
        (Map.toAscList state.controlHistory)

-- | Narrow corruption hooks used only to prove that owner validation catches
-- history contradictions which cannot be constructed through admission.
relocateStructuralControlHistoryForInvariantTest ::
  ControlIndex ->
  ControlIndex ->
  GlobalObjectId ->
  StructuralAppliedState ->
  StructuralAppliedState
relocateStructuralControlHistoryForInvariantTest oldIndex newIndex object state =
  case Map.lookup (oldIndex, object) state.controlHistory of
    Nothing -> state
    Just overlay ->
      state
        { controlHistory =
            Map.insert
              (newIndex, object)
              overlay
              (Map.delete (oldIndex, object) state.controlHistory)
        }

dropStructuralControlOverlayForInvariantTest ::
  GlobalObjectId -> StructuralAppliedState -> StructuralAppliedState
dropStructuralControlOverlayForInvariantTest object state =
  state {controlOverlays = Map.delete object state.controlOverlays}

-- | Canonical, prefix-reconstructable history of only genuine topology
-- changes. Same-controller releases never enter this ledger, so advancing the
-- Oracle cursor for an authority-only change leaves these bytes identical.
structuralControlCheckpointAt ::
  ControlIndex -> StructuralAppliedState -> (Bool, ByteString)
structuralControlCheckpointAt requested state =
  (hasConsequence, Serialize.runPut (putCheckpoint normalized))
  where
    -- Normalization inserts every covered history entry, so its map is nonempty
    -- exactly when the earliest retained consequence is covered. A caller which
    -- only needs this fact must not reconstruct or encode the entire history.
    hasConsequence = maybe False ((<= requested) . fst . fst) (Map.lookupMin state.controlHistory)
    normalized = normalizedControlOverlaysAt requested state

    putCheckpoint overlays = do
      putSizedBytes (ByteString.Char8.pack "ECLIPS-STRUCTURAL-CONTROL")
      putWord64Count (Map.size overlays)
      mapM_ putEntry (Map.toAscList overlays)

    putEntry (object, overlay) = do
      let cause = controlOverlayCause overlay
      putSizedBytes (structuralConsequenceCauseCanonicalBytes cause)
      Serialize.putByteString (globalObjectIdBytes object)
      case overlay of
        StructuralControllerOverlay _ controller _ -> do
          Serialize.putWord8 0
          putController controller
        StructuralDeletedOverlay _ -> Serialize.putWord8 1

controlOverlayCause :: StructuralControlOverlay -> StructuralConsequenceCause
controlOverlayCause = \case
  StructuralControllerOverlay cause _ _ -> cause
  StructuralDeletedOverlay cause -> cause

normalizedControlOverlaysAt ::
  ControlIndex -> StructuralAppliedState -> Map GlobalObjectId StructuralControlOverlay
normalizedControlOverlaysAt requested state =
  foldl'
    ( \overlays ((index, object), overlay) ->
        if index <= requested
          then Map.insert object overlay overlays
          else overlays
    )
    Map.empty
    (Map.toAscList state.controlHistory)

-- | Reconstruct only the authority-bearing Nabla facts at one exact retained
-- vector/control coordinate.  Unlike the full topology projection this does
-- not consult current liveness, possession, effective-sort, or endpoint
-- views, so later local state cannot reinterpret delayed work.
structuralNablaProjectionAt ::
  NablaId ->
  StructuralVersionVector ->
  ControlIndex ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem (Maybe HistoricalNablaProjection)
structuralNablaProjectionAt nabla frontier controlPrefix state = do
  if vectorMatchesMembership frontier state
    then Right ()
    else Left StructuralProjectionMembershipMismatch
  validateCoveredDots frontier state
  validateProjectionClosure frontier state
  case Map.lookup object (winningEntriesAt (Just frontier) state) of
    Nothing -> Right Nothing
    Just entry -> do
      meaning <-
        maybe
          (Left (StructuralProjectionMeaningMissing (entryProvenance entry)))
          Right
          (Map.lookup (entryProvenance entry) state.projectionMeanings)
      case Map.lookup object (normalizedControlOverlaysAt controlPrefix state) of
        Just (StructuralDeletedOverlay _) -> Right Nothing
        overlay ->
          case applyController overlay meaning of
            CheckedNabla _ observed typedSort controller _
              | observed == nabla ->
                  Right
                    ( Just
                        HistoricalNablaProjection
                          { provenance = entryProvenance entry,
                            controller,
                            sort = typedSort,
                            controlPrerequisite = entryControlPrerequisite entry
                          }
                    )
            _ -> Right Nothing
  where
    object = globalObjectIdFromNablaId nabla

    applyController Nothing meaning = meaning
    applyController (Just (StructuralControllerOverlay _ controller activity)) meaning =
      reviseMeaningController controller activity meaning
    applyController (Just (StructuralDeletedOverlay _)) meaning = meaning

-- | One checked coordinate shared by a batch of writer questions. Meaning
-- lookup remains per object: preparing a batch does not expose a missing
-- meaning belonging to an object which the caller never queries.
data PreparedNablaProjections
  = PreparedNablaProjections
      (Map GlobalObjectId AppliedEntry)
      (Map StructuralProjectionProvenance CheckedStructuralMeaning)
      (Map GlobalObjectId StructuralControlOverlay)

prepareNablaProjectionsAt ::
  StructuralVersionVector ->
  ControlIndex ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem PreparedNablaProjections
prepareNablaProjectionsAt frontier controlPrefix state = do
  if vectorMatchesMembership frontier state
    then Right ()
    else Left StructuralProjectionMembershipMismatch
  validateCoveredDots frontier state
  validateProjectionClosure frontier state
  Right
    ( PreparedNablaProjections
        (winningEntriesAt (Just frontier) state)
        state.projectionMeanings
        (normalizedControlOverlaysAt controlPrefix state)
    )

lookupPreparedNablaProjection ::
  NablaId ->
  PreparedNablaProjections ->
  Either StructuralReconciliationProblem (Maybe HistoricalNablaProjection)
lookupPreparedNablaProjection nabla (PreparedNablaProjections winners meanings overlays) =
  case Map.lookup object winners of
    Nothing -> Right Nothing
    Just entry -> do
      meaning <-
        maybe
          (Left (StructuralProjectionMeaningMissing (entryProvenance entry)))
          Right
          (Map.lookup (entryProvenance entry) meanings)
      case Map.lookup object overlays of
        Just (StructuralDeletedOverlay _) -> Right Nothing
        overlay ->
          case applyController overlay meaning of
            CheckedNabla _ observed typedSort controller _
              | observed == nabla ->
                  Right
                    ( Just
                        HistoricalNablaProjection
                          { provenance = entryProvenance entry,
                            controller,
                            sort = typedSort,
                            controlPrerequisite = entryControlPrerequisite entry
                          }
                    )
            _ -> Right Nothing
  where
    object = globalObjectIdFromNablaId nabla
    applyController Nothing meaning = meaning
    applyController (Just (StructuralControllerOverlay _ controller activity)) meaning =
      reviseMeaningController controller activity meaning
    applyController (Just (StructuralDeletedOverlay _)) meaning = meaning

structuralAppliedVertexProjections ::
  StructuralAppliedState -> Map GlobalObjectId StructuralVertexProjection
structuralAppliedVertexProjections state =
  Map.mapMaybe
    (\entry -> effectiveProjectedEntry state entry >>= entryVertexProjection)
    (winningEntries state)

structuralAppliedEdgeProjections ::
  StructuralAppliedState -> Map GlobalObjectId OwnedEdgeProjection
structuralAppliedEdgeProjections state =
  Map.mapMaybe
    (\entry -> effectiveProjectedEntry state entry >>= entryEdgeProjection)
    (winningEntries state)

-- | Read one current projection without reconstructing the complete history
-- or the projections of unrelated objects. The current carrier index is
-- maintained when an unsuppressed occurrence is admitted; control overlays are
-- still applied at lookup, so deletion and controller changes remain visible.
structuralAppliedVertexProjectionAt ::
  GlobalObjectId -> StructuralAppliedState -> Maybe StructuralVertexProjection
structuralAppliedVertexProjectionAt object state = do
  entry <- winnerForObject object state
  effectiveProjectedEntry state entry >>= entryVertexProjection

structuralAppliedEdgeProjectionAt ::
  GlobalObjectId -> StructuralAppliedState -> Maybe OwnedEdgeProjection
structuralAppliedEdgeProjectionAt object state = do
  entry <- winnerForObject object state
  effectiveProjectedEntry state entry >>= entryEdgeProjection

-- | Recomputed graph projection at one exact causal frontier.  It is derived
-- from baseline winners plus only the intrinsic dots covered by the supplied
-- vector; the state's ahead current projection cache is intentionally ignored.
data StructuralProjectionSnapshot = StructuralProjectionSnapshot
  { vertices :: Map GlobalObjectId StructuralVertexProjection,
    edges :: Map GlobalObjectId OwnedEdgeProjection,
    digestEntries :: Map GlobalObjectId StructuralProjectionDigestEntry,
    deletedControls ::
      Map GlobalObjectId (StructuralProjectionProvenance, StructuralConsequenceCause),
    admissionContributions :: Map JoinBootstrap.MembershipAdmissionOrigin (ControlIndex, JoinBootstrap.HeraldBootstrapContribution)
  }
  deriving stock (Eq, Show)

-- | Complete canonical material for one winning structural object at a
-- requested frontier. Each present entry carries the last control cause which
-- changed its controller, while 'StructuralProjectionSnapshot' retains a
-- separate diagnostic record of known deletion provenance. Absence has no
-- deletion provenance in the canonical projection. In particular, the
-- carrier definition occurrence and genuine prerequisite remain available
-- after an occurrence loses the current winner comparison.
data StructuralProjectionDigestEntry = StructuralProjectionDigestEntry
  { controlled :: AppliedControlledProjection,
    carrierSort :: SortOccurrence,
    controlPrerequisite :: ControlIndex,
    controlCause :: Maybe StructuralConsequenceCause,
    vertex :: Maybe StructuralVertexProjection,
    edge :: Maybe OwnedEdgeProjection
  }
  deriving stock (Eq, Show)

structuralProjectionDigestEntryControlled ::
  StructuralProjectionDigestEntry -> AppliedControlledProjection
structuralProjectionDigestEntryControlled entry = entry.controlled

structuralProjectionDigestEntryCarrierSort ::
  StructuralProjectionDigestEntry -> SortOccurrence
structuralProjectionDigestEntryCarrierSort entry = entry.carrierSort

structuralProjectionDigestEntryControlPrerequisite ::
  StructuralProjectionDigestEntry -> ControlIndex
structuralProjectionDigestEntryControlPrerequisite entry =
  entry.controlPrerequisite

structuralProjectionDigestEntryControlCause ::
  StructuralProjectionDigestEntry -> Maybe StructuralConsequenceCause
structuralProjectionDigestEntryControlCause entry = entry.controlCause

structuralProjectionDigestEntryVertex ::
  StructuralProjectionDigestEntry -> Maybe StructuralVertexProjection
structuralProjectionDigestEntryVertex entry = entry.vertex

structuralProjectionDigestEntryEdge ::
  StructuralProjectionDigestEntry -> Maybe OwnedEdgeProjection
structuralProjectionDigestEntryEdge entry = entry.edge

structuralProjectionSnapshotVertices ::
  StructuralProjectionSnapshot -> Map GlobalObjectId StructuralVertexProjection
structuralProjectionSnapshotVertices snapshot = snapshot.vertices

structuralProjectionSnapshotAdmissionVertices :: StructuralProjectionSnapshot -> [VertexId]
structuralProjectionSnapshotAdmissionVertices snapshot =
  [ DeltaVertex delta
  | (_, contribution) <- Map.elems snapshot.admissionContributions,
    (_, delta, _) <- JoinBootstrap.contributionViews contribution
  ]

structuralProjectionSnapshotEdges ::
  StructuralProjectionSnapshot -> Map GlobalObjectId OwnedEdgeProjection
structuralProjectionSnapshotEdges snapshot = snapshot.edges

structuralProjectionSnapshotDigestEntries ::
  StructuralProjectionSnapshot -> Map GlobalObjectId StructuralProjectionDigestEntry
structuralProjectionSnapshotDigestEntries snapshot = snapshot.digestEntries

-- | Diagnostic provenance for deleted objects whose meaning was admitted.
-- A receiver which first admits an already retired occurrence can have the
-- same absent topology without this record. Operational terminal suppression
-- is retained separately in the applied state's controls and retirements.
structuralProjectionSnapshotDeletedControls ::
  StructuralProjectionSnapshot ->
  Map GlobalObjectId (StructuralProjectionProvenance, StructuralConsequenceCause)
structuralProjectionSnapshotDeletedControls snapshot = snapshot.deletedControls

-- | Remove already-published baseline winners from a historical snapshot.
--
-- Step 14 combines the checked startup/genesis projection with only genuine
-- occurrence-provenance dynamic winners. Keeping this normalization beside the
-- private snapshot constructor lets its existing canonical encoder remain the
-- single implementation of dynamic structural bytes.
structuralProjectionSnapshotOccurrencesOnly ::
  StructuralProjectionSnapshot -> StructuralProjectionSnapshot
structuralProjectionSnapshotOccurrencesOnly snapshot =
  StructuralProjectionSnapshot
    { vertices =
        Map.filter
          ((/= Nothing) . structuralVertexProjectionOccurrence)
          snapshot.vertices,
      edges =
        Map.filter
          ((/= Nothing) . ownedEdgeProjectionOccurrence)
          snapshot.edges,
      digestEntries =
        Map.filter
          ( (/= Nothing)
              . structuralProjectionProvenanceOccurrence
              . appliedControlledProjectionProvenance
              . structuralProjectionDigestEntryControlled
          )
          snapshot.digestEntries,
      deletedControls =
        Map.filter
          ((/= Nothing) . structuralProjectionProvenanceOccurrence . fst)
          snapshot.deletedControls,
      admissionContributions = snapshot.admissionContributions
    }

-- | Canonical semantic transcript for the exact requested structural frontier.
--
-- The transcript deliberately contains no prospective topology-cut identity or
-- derived structural authority. Each ascending object entry instead binds its
-- winning publication provenance, genuine carrier definition occurrence,
-- genuine control prerequisite, lifecycle/role, and complete induced structural
-- graph meaning. Herald-local activity, placement, and Store state are
-- deliberately excluded: those remain local reconciliation outputs and later
-- belong to an Alignment cut. Present entries include the normalized control
-- cause which produced their controller projection. Deletion provenance is
-- excluded: equal absence must not depend on whether an owner admitted the
-- object's old meaning before learning that it was deleted or retired.
-- Terminal controls and retirement suppressions remain independent operational
-- facts which prevent later carriers from restoring the object.
structuralProjectionSnapshotCanonicalBytes ::
  StructuralProjectionSnapshot -> ByteString
structuralProjectionSnapshotCanonicalBytes snapshot =
  Serialize.runPut $ do
    putSizedBytes (ByteString.Char8.pack "ECLIPS-STRUCTURAL-PROJECTION")
    putWord64Count (Map.size snapshot.digestEntries)
    mapM_ putDigestEntry (Map.toAscList snapshot.digestEntries)
    unless (Map.null snapshot.admissionContributions) $ do
      putSizedBytes "ECLIPS-MEMBERSHIP-ADMISSION-ORIGINS"
      putWord64Count (Map.size snapshot.admissionContributions)
      mapM_ putAdmissionContribution (Map.elems snapshot.admissionContributions)
  where
    putAdmissionContribution (index, contribution) = do
      Serialize.putWord64be (controlIndexWord64 index)
      putSizedBytes (JoinBootstrap.contributionCanonicalBytes contribution)

putDigestEntry ::
  (GlobalObjectId, StructuralProjectionDigestEntry) -> Serialize.Put
putDigestEntry (object, entry) = do
  Serialize.putByteString (globalObjectIdBytes object)
  putControlled entry.controlled
  putSortOccurrence entry.carrierSort
  Serialize.putWord64be (controlIndexWord64 entry.controlPrerequisite)
  putMaybe (putSizedBytes . structuralConsequenceCauseCanonicalBytes) entry.controlCause
  putMaybe putVertexProjection entry.vertex
  putMaybe putEdgeProjection entry.edge

putControlled :: AppliedControlledProjection -> Serialize.Put
putControlled projection = do
  Serialize.putByteString (globalObjectIdBytes projection.object)
  putProvenance projection.provenance
  Serialize.putWord8 (structuralCarrierRoleTag projection.role)
  Serialize.putWord8 $ case projection.lifecycle of
    Current -> 0
    Obsolete -> 1

putProvenance :: StructuralProjectionProvenance -> Serialize.Put
putProvenance provenance = case provenance of
  GenesisStructuralProvenance object -> do
    Serialize.putWord8 0
    Serialize.putByteString (globalObjectIdBytes object)
  BaselineStructuralProvenance publication -> do
    Serialize.putWord8 1
    putPublicationId publication
  OccurrenceStructuralProvenance occurrence publication -> do
    Serialize.putWord8 2
    putOccurrence occurrence
    putPublicationId publication

putPublicationId :: PublicationId -> Serialize.Put
putPublicationId publication = do
  Serialize.putByteString (nablaIdBytes (publicationNabla publication))
  putAuthorityEpoch (publicationAuthorityEpoch publication)
  Serialize.putByteString
    (heraldEpochBytes (publicationSourceHeraldEpoch publication))
  Serialize.putWord64be
    (nablaSequenceWord64 (publicationNablaSequence publication))

putAuthorityEpoch :: AuthorityEpoch -> Serialize.Put
putAuthorityEpoch = putSizedBytes . authorityEpochCanonicalBytes

putOccurrence :: StructuralOccurrenceId -> Serialize.Put
putOccurrence occurrence = do
  Serialize.putByteString
    (heraldEpochBytes (structuralOccurrenceSourceHeraldEpoch occurrence))
  Serialize.putWord64be
    ( structuralSequenceWord64
        (structuralOccurrenceSourceSequence occurrence)
    )

putSortOccurrence :: SortOccurrence -> Serialize.Put
putSortOccurrence occurrence = do
  Serialize.putByteString (sortIdBytes (sortOccurrenceSortId occurrence))
  Serialize.putByteString
    (sortDefinitionOccurrenceIdBytes (sortOccurrenceDefinition occurrence))

putVertexProjection :: StructuralVertexProjection -> Serialize.Put
putVertexProjection projection = case projection of
  NeutralVertexProjection provenance object -> do
    Serialize.putWord8 0
    putProvenance provenance
    Serialize.putByteString (globalObjectIdBytes object)
  NablaVertexProjection provenance nabla typedSort controller _activity -> do
    Serialize.putWord8 1
    putProvenance provenance
    Serialize.putByteString (nablaIdBytes nabla)
    putSortOccurrence typedSort
    putController controller
  DeltaVertexProjection provenance delta typedSort controller _activity -> do
    Serialize.putWord8 2
    putProvenance provenance
    Serialize.putByteString (deltaIdBytes delta)
    putSortOccurrence typedSort
    putController controller

putController :: ControllerProjection -> Serialize.Put
putController controller = case controller of
  VoidController -> Serialize.putWord8 0
  LiveProcessController process herald -> do
    Serialize.putWord8 1
    Serialize.putByteString (processEpochIdBytes process)
    Serialize.putByteString (heraldEpochBytes herald)
  ZombieProcessController process -> do
    Serialize.putWord8 2
    Serialize.putByteString (processEpochIdBytes process)

putEdgeProjection :: OwnedEdgeProjection -> Serialize.Put
putEdgeProjection projection = do
  putProvenance projection.provenance
  Serialize.putByteString (globalObjectIdBytes projection.object)
  putVertexId (edgeSource projection.payload)
  putVertexId (edgeDestination projection.payload)
  Serialize.putInt64be (edgeStrengthTranscriptTag (edgeStrength projection.payload))

putVertexId :: VertexId -> Serialize.Put
putVertexId vertex = case vertex of
  NablaVertex nabla -> do
    Serialize.putWord8 0
    Serialize.putByteString (nablaIdBytes nabla)
  DeltaVertex delta -> do
    Serialize.putWord8 1
    Serialize.putByteString (deltaIdBytes delta)
  NeutralVertex object -> do
    Serialize.putWord8 2
    Serialize.putByteString (globalObjectIdBytes object)

putMaybe :: (value -> Serialize.Put) -> Maybe value -> Serialize.Put
putMaybe putValue value = case value of
  Nothing -> Serialize.putWord8 0
  Just present -> Serialize.putWord8 1 >> putValue present

putSizedBytes :: ByteString -> Serialize.Put
putSizedBytes bytes = do
  Serialize.putWord64be (fromIntegral (ByteString.length bytes))
  Serialize.putByteString bytes

putWord64Count :: Int -> Serialize.Put
putWord64Count = Serialize.putWord64be . fromIntegral

-- | Only the external facts read by historical topology projection. Sort
-- definitions are checked before admitting this key; their immutable meanings
-- have already been retained by reconciliation. Residence, possession and live
-- processes do not reinterpret those meanings.
data StructuralProjectionInputs = StructuralProjectionInputs !(Set VertexId)
  deriving stock (Eq, Show)

prepareStructuralProjectionInputs :: ReconciliationViews -> Either StructuralReconciliationProblem StructuralProjectionInputs
prepareStructuralProjectionInputs views = do
  validateEffectiveSorts views
  pure (StructuralProjectionInputs views.baselineVertices)

-- | Moving an Oracle cursor between retained consequences cannot alter a
-- historical projection. Select the last relevant coordinate without rebuilding
-- its normalized overlay map or serializing a checkpoint. Admission entries are
-- rare, separately retained contributions and must participate in this boundary.
structuralProjectionControlCoordinate :: ControlIndex -> StructuralAppliedState -> ControlIndex
structuralProjectionControlCoordinate requested state =
  foldl'
    max
    overlayCoordinate
    [ index
    | (index, _) <- Map.elems state.admissionContributions,
      index <= requested
    ]
  where
    overlayCoordinate = maybe (controlIndex 0) (fst . fst) (Map.lookupMax covered)
    covered = Map.takeWhileAntitone ((<= requested) . fst) state.controlHistory

-- | Resolve one historical frontier without changing applied history,
-- current placement, or retained resource metadata. The structural coordinator
-- calls preparation only after vector gaps are held; the same checked guard
-- is retained here so this pure seam cannot silently manufacture a cut.
prepareStructuralProjectionAtVector ::
  ReconciliationViews ->
  StructuralVersionVector ->
  ControlIndex ->
  StructuralAppliedState ->
  Either
    StructuralReconciliationProblem
    (Either (Set StructuralDependency) StructuralProjectionSnapshot)
prepareStructuralProjectionAtVector views frontier controlPrefix state =
  prepareStructuralProjectionAtVectorWithChecks DiagnosticChecksEnabled views frontier controlPrefix state

-- | Reconstruct a frontier whose coverage and causal closure the owning
-- progress transition has already admitted. Only that repeated proof is
-- diagnostic; view shape, membership and retained meanings remain checked.
prepareStructuralProjectionAtAdmittedVector ::
  ReconciliationViews ->
  StructuralVersionVector ->
  ControlIndex ->
  StructuralAppliedState ->
  Either
    StructuralReconciliationProblem
    (Either (Set StructuralDependency) StructuralProjectionSnapshot)
prepareStructuralProjectionAtAdmittedVector views =
  prepareStructuralProjectionAtVectorWithChecks views.diagnosticChecks views

prepareStructuralProjectionAtVectorWithChecks ::
  DiagnosticChecks ->
  ReconciliationViews ->
  StructuralVersionVector ->
  ControlIndex ->
  StructuralAppliedState ->
  Either
    StructuralReconciliationProblem
    (Either (Set StructuralDependency) StructuralProjectionSnapshot)
prepareStructuralProjectionAtVectorWithChecks checks views frontier controlPrefix state = do
  validateEffectiveSorts views
  if vectorMatchesMembership frontier state
    then Right ()
    else Left StructuralProjectionMembershipMismatch
  runDiagnosticCheck checks $ do
    validateCoveredDots frontier state
    validateProjectionClosure frontier state
  let winners = winningEntriesAt (Just frontier) state
      overlays = normalizedControlOverlaysAt controlPrefix state
  meanings <- traverse retainedMeaning winners
  let overlaid = Map.mapWithKey (projectReady overlays winners) meanings
      structuralBaselineVertices =
        Set.fromList
          [ structuralVertexProjectionVertex vertex
          | entry <- Map.elems state.baselineWinners,
            Just meaning <- [Map.lookup (entryProvenance entry) state.projectionMeanings],
            Just vertex <- [entryVertexProjection (ProjectedEntry entry meaning)]
          ]
      availableVertices =
        (views.baselineVertices `Set.difference` structuralBaselineVertices)
          `Set.union` Set.fromList
            [ DeltaVertex delta
            | (index, contribution) <- Map.elems state.admissionContributions,
              index <= controlPrefix,
              (_, delta, _) <- JoinBootstrap.contributionViews contribution
            ]
          `Set.union` Set.fromList
            ( Map.elems
                ( Map.mapMaybe
                    (fmap structuralVertexProjectionVertex . projectedVertex)
                    overlaid
                )
            )
      projected = Map.filter (retainsInducedProjection availableVertices) overlaid
  Right
    ( Right
        StructuralProjectionSnapshot
          { vertices = Map.mapMaybe projectedVertex projected,
            edges = Map.mapMaybe projectedEdge projected,
            digestEntries = Map.mapMaybe projectedDigestEntry projected,
            deletedControls = Map.mapMaybe projectedDeletion projected,
            admissionContributions = Map.filter ((<= controlPrefix) . fst) state.admissionContributions
          }
    )
  where
    retainedMeaning entry =
      maybe
        (Left (StructuralProjectionMeaningMissing (entryProvenance entry)))
        Right
        (Map.lookup (entryProvenance entry) state.projectionMeanings)

    projectReady overlays winners object meaning =
      let entry = winners Map.! object
       in case Map.lookup object overlays of
            Nothing -> HistoricalPresent (ProjectedEntry entry meaning) Nothing
            Just (StructuralDeletedOverlay cause) ->
              HistoricalDeleted (entryProvenance entry) cause
            Just overlay@(StructuralControllerOverlay _ controller activity) ->
              HistoricalPresent
                ( ProjectedEntry
                    entry
                    (reviseMeaningController controller activity meaning)
                )
                (Just (controlOverlayCause overlay))

    projectedVertex = \case
      HistoricalPresent projected _ -> entryVertexProjection projected
      HistoricalDeleted _ _ -> Nothing

    projectedEdge = \case
      HistoricalPresent projected _ -> entryEdgeProjection projected
      HistoricalDeleted _ _ -> Nothing

    projectedDigestEntry = \case
      HistoricalPresent projected cause ->
        Just (structuralProjectionDigestEntry projected cause)
      HistoricalDeleted _ _ -> Nothing

    projectedDeletion = \case
      HistoricalPresent {} -> Nothing
      HistoricalDeleted provenance cause -> Just (provenance, cause)

    retainsInducedProjection available = \case
      HistoricalPresent projected _ ->
        case entryEdgeProjection projected of
          Nothing -> True
          Just edge ->
            Set.member (edgeSource edge.payload) available
              && Set.member (edgeDestination edge.payload) available
      HistoricalDeleted {} -> True

data HistoricalProjectedEntry
  = HistoricalPresent ProjectedEntry (Maybe StructuralConsequenceCause)
  | HistoricalDeleted StructuralProjectionProvenance StructuralConsequenceCause

structuralProjectionDigestEntry ::
  ProjectedEntry -> Maybe StructuralConsequenceCause -> StructuralProjectionDigestEntry
structuralProjectionDigestEntry projected@(ProjectedEntry applied _) cause =
  StructuralProjectionDigestEntry
    { controlled = appliedControlledProjection applied,
      carrierSort = entryCarrierSort applied,
      controlPrerequisite = entryControlPrerequisite applied,
      controlCause = cause,
      vertex = entryVertexProjection projected,
      edge = entryEdgeProjection projected
    }

entryCarrierSort :: AppliedEntry -> SortOccurrence
entryCarrierSort entry = case entry.source of
  GenesisEntrySource _ carrierSort _ _ -> carrierSort
  BaselineEntrySource carrierSort _ -> carrierSort
  OccurrenceEntrySource application -> application.carrierSort

entryControlPrerequisite :: AppliedEntry -> ControlIndex
entryControlPrerequisite entry = case entry.source of
  GenesisEntrySource _ _ prerequisite _ -> prerequisite
  BaselineEntrySource {} -> controlIndex 0
  OccurrenceEntrySource application -> application.controlPrerequisite

validateProjectionClosure ::
  StructuralVersionVector ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem ()
validateProjectionClosure frontier state = do
  summary <- prepareStructuralFrontierSummary frontier state
  case validateProjectionRequirements validateCoveredDots frontier state summary.predecessors of
    Right () -> Right ()
    -- Failed admission is rare. Retain the exhaustive ordering so callers see
    -- precisely the original first missing/invalid dependency, not an endpoint.
    Left _ -> validateProjectionClosureExhaustive frontier state

validateProjectionClosureExhaustive ::
  StructuralVersionVector -> StructuralAppliedState -> Either StructuralReconciliationProblem ()
validateProjectionClosureExhaustive frontier state =
  validateProjectionRequirements
    validateCoveredDotsExhaustive
    frontier
    state
    [ (occurrence, application.predecessor)
    | (occurrence, entry) <- Map.toAscList state.history,
      coversOccurrence frontier occurrence,
      Just application <- [entryApplication entry]
    ]

validateProjectionRequirements ::
  (StructuralVersionVector -> StructuralAppliedState -> Either StructuralReconciliationProblem ()) ->
  StructuralVersionVector ->
  StructuralAppliedState ->
  [(StructuralOccurrenceId, StructuralVersionVector)] ->
  Either StructuralReconciliationProblem ()
validateProjectionRequirements validateCoverage frontier state included =
  mapM_ requirePredecessorClosure included
  where
    requirePredecessorClosure (occurrence, predecessor)
      | structuralVersionVectorCovers frontier predecessor = Right ()
      | coveredThroughEstablishedBases predecessor = validateCoverage predecessor state
      | otherwise = Left (StructuralProjectionPredecessorNotCovered occurrence)
    coveredThroughEstablishedBases required
      | structuralVersionVectorMembershipGenerationId frontier == structuralVersionVectorMembershipGenerationId required =
          structuralVersionVectorCovers frontier required
      | otherwise = any (throughBase required) (Map.elems state.membershipBases) || any (throughAdmission required) (Map.elems state.admissionBases)
    throughAdmission required lineage =
      case heraldMembershipLineageFrom (structuralVersionVectorMembershipGenerationId required) lineage of
        Left _ -> False
        Right suffix -> case projectStructuralVersionVector suffix required of
          Left _ -> False
          Right projected ->
            structuralVersionVectorMembershipGenerationId projected /= structuralVersionVectorMembershipGenerationId required
              && coveredThroughEstablishedBases projected
    throughBase required base =
      case heraldMembershipLineageFrom (structuralVersionVectorMembershipGenerationId required) (successorStructuralBaseLineage base) of
        Left _ -> False
        Right suffix -> case projectStructuralVersionVector suffix required of
          Left _ -> False
          Right projected ->
            structuralVersionVectorMembershipGenerationId projected /= structuralVersionVectorMembershipGenerationId required
              && all (removedCovered projected terminal) (structuralVersionVectorEntries required)
              && coveredThroughEstablishedBases projected
      where
        terminal =
          terminalSourceUnionTerminalPredecessorVector
            (terminalSourceUnionEstablishedUnion (successorStructuralBaseEstablishedUnion base))
    removedCovered projected terminal (origin, prefix) =
      case structuralVersionVectorComponent origin projected of
        Just _ -> True
        Nothing -> maybe False (>= prefix) (structuralVersionVectorComponent origin terminal)

data StructuralApplicationClassification
  = StructuralApplicationAppliedWinner
  | StructuralApplicationAppliedNonWinner
  | StructuralApplicationRetirementSuppressed
  | StructuralApplicationReplay
  | StructuralProjectionRefresh
  deriving stock (Eq, Ord, Show)

data PreparedStructuralReconciliation = PreparedStructuralReconciliation
  { classification :: StructuralApplicationClassification,
    changesState :: !Bool,
    application :: Maybe StructuralApplication,
    controlledPatch :: ControlledPatch,
    graphPatch :: StructuralGraphPatch,
    placementPatch :: PlacementPatch,
    storePatch :: StorePatch,
    debts :: StructuralDebtSet,
    predecessorState :: StructuralAppliedState,
    successor :: StructuralAppliedState,
    satisfiedDependencies :: Set StructuralDependency
  }
  deriving stock (Eq, Show)

preparedStructuralClassification ::
  PreparedStructuralReconciliation -> StructuralApplicationClassification
preparedStructuralClassification prepared = prepared.classification

-- | Whether the admitted preparation changes reconciliation facts. This is
-- computed at the authoritative mutation branch, including hidden historical
-- facts and resources; empty visible patches alone do not prove a no-op.
preparedStructuralChangesState :: PreparedStructuralReconciliation -> Bool
preparedStructuralChangesState prepared = prepared.changesState

-- | The exact checked occurrence carried by an application preparation.
-- Projection refreshes have no new structural occurrence.  Keeping this fact
-- separate from 'ControlledPatch' lets an exact replay remain a complete
-- mutation no-op while still giving the progress owner the authenticated dot
-- it must compare with the retained history and stamp.
preparedStructuralApplication ::
  PreparedStructuralReconciliation -> Maybe StructuralApplication
preparedStructuralApplication prepared = prepared.application

preparedStructuralControlledPatch ::
  PreparedStructuralReconciliation -> ControlledPatch
preparedStructuralControlledPatch prepared = prepared.controlledPatch

preparedStructuralGraphPatch ::
  PreparedStructuralReconciliation -> StructuralGraphPatch
preparedStructuralGraphPatch prepared = prepared.graphPatch

preparedStructuralPlacementPatch ::
  PreparedStructuralReconciliation -> PlacementPatch
preparedStructuralPlacementPatch prepared = prepared.placementPatch

preparedStructuralStorePatch :: PreparedStructuralReconciliation -> StorePatch
preparedStructuralStorePatch prepared = prepared.storePatch

preparedStructuralDebts :: PreparedStructuralReconciliation -> StructuralDebtSet
preparedStructuralDebts prepared = prepared.debts

preparedStructuralPredecessor ::
  PreparedStructuralReconciliation -> StructuralAppliedState
preparedStructuralPredecessor prepared = prepared.predecessorState

preparedStructuralSuccessor ::
  PreparedStructuralReconciliation -> StructuralAppliedState
preparedStructuralSuccessor prepared = prepared.successor

preparedStructuralObservedCarrierRole ::
  PreparedStructuralReconciliation -> Maybe StructuralCarrierRole
preparedStructuralObservedCarrierRole prepared = do
  application <- preparedStructuralApplication prepared
  entry <- Map.lookup application.occurrence prepared.successor.history
  pure (entryRole entry)

preparedStructuralSatisfiedDependencies ::
  PreparedStructuralReconciliation -> Set StructuralDependency
preparedStructuralSatisfiedDependencies prepared = prepared.satisfiedDependencies

data StructuralPreparation
  = StructuralHeld (Set StructuralDependency)
  | StructuralReady PreparedStructuralReconciliation
  deriving stock (Eq, Show)

data StructuralReconciliationProblem
  = StructuralCarrierSortUnavailable SortId
  | StructuralCarrierOccurrenceMismatch
      SortId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | StructuralCarrierSortMismatch SortId SortId
  | StructuralEffectiveSortKeyMismatch SortId SortId
  | StructuralCarrierUnsupported SortId
  | StructuralProcessCarrierUnsupported
  | StructuralValueShapeContradiction StructuralCarrierRole
  | StructuralOccurrenceConflict StructuralOccurrenceId
  | StructuralOccurrencePublicationSourceMismatch HeraldEpoch HeraldEpoch
  | StructuralPredecessorMembershipMismatch
  | StructuralMembershipBaseMismatch
  | StructuralProjectionMembershipMismatch
  | StructuralProjectionPredecessorNotCovered StructuralOccurrenceId
  | StructuralProjectionMeaningMissing StructuralProjectionProvenance
  | StructuralPredecessorSourceMissing HeraldEpoch
  | StructuralPredecessorSourceMismatch HeraldEpoch
  | StructuralPredecessorDotMissing StructuralOccurrenceId
  | StructuralCarriedStampMismatch StructuralOccurrenceId
  | StructuralRefreshOccurrenceMissing StructuralOccurrenceId
  | StructuralRefreshOccurrenceNotProjectable StructuralOccurrenceId
  | StructuralPublicationOccurrenceConflict
      PublicationId
      StructuralOccurrenceId
      StructuralOccurrenceId
  | StructuralPublicationBaselineConflict PublicationId
  | StructuralObjectRoleConflict
      GlobalObjectId
      StructuralCarrierRole
      StructuralCarrierRole
  | StructuralBaselineObjectKeyMismatch GlobalObjectId GlobalObjectId
  | StructuralBaselinePublicationConflict PublicationId
  | StructuralGenesisObjectConflict GlobalObjectId
  | StructuralGenesisControlPrerequisiteNotZero GlobalObjectId
  | StructuralUnrepresentedBaselineObject GlobalObjectId
  | StructuralBaselineStoreMismatch DeltaId
  | StructuralBaselineStoreWithoutActiveProjection DeltaId
  | StructuralEndpointRoleConflict GlobalObjectId
  | StructuralFreshIncarnationWrongDelta DeltaId DeltaId
  | StructuralFreshIncarnationUnexpected DeltaId
  | StructuralFreshIncarnationReused DeltaId StoreIncarnationId
  | StructuralCarrierBaseReceiverNotFresh
  | StructuralCarrierBaseGenesisMismatch
  | StructuralControlTargetMissing GlobalObjectId
  | StructuralControlBaseHistoryConflict ControlIndex GlobalObjectId
  | StructuralControlCauseNotOracleOrdered StructuralConsequenceCause
  | StructuralControlRestoresDeletedObject GlobalObjectId
  | StructuralTerminalSuppressionCauseConflict
      GlobalObjectId
      StructuralConsequenceCause
      StructuralConsequenceCause
  | StructuralTerminalSuppressionProjectedObject GlobalObjectId
  | StructuralRetirementSuppressionNotStale ControlIndex ControlIndex
  | StructuralProcessEndControlHeld ProcessEpochId
  | StructuralLocalStoreHeraldMismatch HeraldEpoch HeraldEpoch
  | StructuralLocalStoreKeyMismatch DeltaId DeltaId
  | StructuralLocalStoreIncarnationConflict
      StoreIncarnationId
      DeltaId
      DeltaId
  deriving stock (Eq, Show)

-- | Prepare one occurrence without mutating a live owner.  Dependency holding
-- is a complete no-op.  A ready result advances only this private applied
-- projection; callers cannot derive its winner from a later application-side
-- Controlled record.
prepareStructuralReconciliation ::
  ReconciliationViews ->
  StructuralApplication ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareStructuralReconciliation views application suppliedFresh predecessor =
  prepareStructuralReconciliationAt
    Nothing
    views
    application.predecessor
    application
    suppliedFresh
    predecessor

-- | Mint source-local work against the exact applied vector owned alongside
-- this reconciliation state. That vector already proves causal coverage and
-- closure; diagnostics may repeat those proofs. All carrier admission, source
-- sequence, membership, meaning and resource checks remain unconditional.
-- Incoming, carried and terminal work must use their fully checked seams.
prepareStructuralReconciliationAtAdmittedPredecessor ::
  ReconciliationViews ->
  StructuralApplication ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareStructuralReconciliationAtAdmittedPredecessor views application =
  prepareStructuralReconciliationRawAtWithChecks
    views.diagnosticChecks
    views
    application.predecessor
    application

-- | Causally consume a structural occurrence whose embedded regular-sort
-- reference predates a committed retirement. The immutable application and
-- decoded carrier remain in history so later source dots can prove predecessor
-- closure, but the occurrence is excluded from every live winner/projection
-- and produces no Graph, Placement, Store, debt, or Controlled consequence.
prepareRetirementSuppressedStructuralReconciliation ::
  StructuralRetirementSuppression ->
  ReconciliationViews ->
  StructuralApplication ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareRetirementSuppressedStructuralReconciliation suppression views application predecessor =
  prepareRetirementSuppressedStructuralReconciliationAt
    suppression
    views
    application.predecessor
    application
    predecessor

-- | Retain an authority-checked occurrence after its controlled identity has
-- already been terminally deleted.  The intrinsic occurrence and vector
-- history remain ordinary structural history, while the terminal control
-- overlay prevents the delayed carrier from recreating Graph, Placement, or
-- Store topology.  The cause is retained even when this Herald had not yet
-- observed any structural incarnation of the object at deletion time.
prepareTerminallySuppressedStructuralReconciliation ::
  StructuralConsequenceCause ->
  ReconciliationViews ->
  StructuralApplication ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareTerminallySuppressedStructuralReconciliation
  cause
  views
  application
  suppliedFresh
  predecessor =
    prepareStructuralReconciliationAt
      (Just cause)
      views
      application.predecessor
      application
      suppliedFresh
      predecessor

-- | Prepare a predecessor-generation survivor occurrence against the
-- successor-generation causal cursor authenticated by terminal lineage. The
-- retained application remains unchanged: its historical predecessor is the
-- exact vector carried by the immutable old stamp.
prepareCarriedStructuralReconciliation ::
  ReconciliationViews ->
  CarriedStampedSurvivor ->
  StructuralApplication ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareCarriedStructuralReconciliation views carried application suppliedFresh predecessor =
  prepareCarriedStructuralReconciliationAt
    Nothing
    views
    carried
    application
    suppliedFresh
    predecessor

-- | Membership-successor counterpart of
-- 'prepareRetirementSuppressedStructuralReconciliation'. The old stamp remains
-- byte-for-byte immutable while terminal lineage supplies the live-ahead
-- predecessor used by causal admission.
prepareRetirementSuppressedCarriedStructuralReconciliation ::
  StructuralRetirementSuppression ->
  ReconciliationViews ->
  CarriedStampedSurvivor ->
  StructuralApplication ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareRetirementSuppressedCarriedStructuralReconciliation
  suppression
  views
  carried
  application
  predecessor = do
    let stamp = carriedStampedSurvivorStamp carried
    if structuralOccurrenceStampOccurrence stamp == application.occurrence
      && structuralOccurrenceStampPredecessor stamp == application.predecessor
      && structuralOccurrenceStampPublication stamp
        == checkedPublicationId application.publication
      then Right ()
      else Left (StructuralCarriedStampMismatch application.occurrence)
    prepareRetirementSuppressedStructuralReconciliationAt
      suppression
      views
      (carriedStampedSurvivorPredecessor carried)
      application
      predecessor

prepareRetirementSuppressedStructuralReconciliationAt ::
  StructuralRetirementSuppression ->
  ReconciliationViews ->
  StructuralVersionVector ->
  StructuralApplication ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareRetirementSuppressedStructuralReconciliationAt
  suppression
  views
  causalPredecessor
  application
  predecessor = do
    validateEffectiveSorts views
    validateCarrierOccurrence views application
    validateLocalStores views predecessor
    if suppressionControlCompatible
      then Right ()
      else
        Left
          ( StructuralRetirementSuppressionNotStale
              application.controlPrerequisite
              floorIndex
          )
    case Map.lookup application.occurrence predecessor.history of
      Just incumbent
        | entryApplication incumbent == Just application,
          Just _ <-
            Map.lookup application.occurrence predecessor.retirementSuppressions ->
            Right (StructuralReady (suppressedPrepared False predecessor))
        | otherwise -> Left (StructuralOccurrenceConflict application.occurrence)
      Nothing -> do
        validateCausalShapeAt causalPredecessor application predecessor
        rejectPublicationReuse application predecessor
        carrier <- decodeCarrier application
        validateBaselineObjectRepresentation views predecessor (decodedCarrierObject carrier)
        validateObjectRoleClaim predecessor (decodedCarrierObject carrier) (decodedCarrierRole carrier)
        let entry = AppliedEntry (OccurrenceEntrySource application) carrier
            successor =
              predecessor
                { history = Map.insert application.occurrence entry predecessor.history,
                  sourceHistory = advanceStructuralSourceHistory application predecessor,
                  retirementSuppressions =
                    Map.insert
                      application.occurrence
                      suppression
                      predecessor.retirementSuppressions
                }
        Right (StructuralReady (suppressedPrepared True successor))
    where
      floorIndex = structuralRetirementSuppressionControlIndex suppression
      suppressionControlCompatible = case suppression of
        DirectRegularSortRetirementSuppression {} ->
          application.controlPrerequisite < floorIndex
        InheritedEndpointRetirementSuppression {} -> True

      suppressedPrepared changesState successor =
        PreparedStructuralReconciliation
          { classification = StructuralApplicationRetirementSuppressed,
            changesState,
            application = Just application,
            controlledPatch = ControlledPatch Nothing Nothing,
            graphPatch = emptyGraphPatch,
            placementPatch = PlacementPatch Map.empty,
            storePatch = StorePatch Map.empty Map.empty,
            debts = normalizeStructuralDebts [],
            predecessorState = predecessor,
            successor,
            satisfiedDependencies =
              Set.singleton (StructuralPredecessorDependency application.occurrence)
          }

-- | Terminally suppressed counterpart of
-- 'prepareCarriedStructuralReconciliation'.  Survivor authentication remains
-- unchanged; only the local live projection is kept terminally absent.
prepareTerminallySuppressedCarriedStructuralReconciliation ::
  StructuralConsequenceCause ->
  ReconciliationViews ->
  CarriedStampedSurvivor ->
  StructuralApplication ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareTerminallySuppressedCarriedStructuralReconciliation
  cause
  views
  carried
  application
  suppliedFresh
  predecessor =
    prepareCarriedStructuralReconciliationAt
      (Just cause)
      views
      carried
      application
      suppliedFresh
      predecessor

prepareCarriedStructuralReconciliationAt ::
  Maybe StructuralConsequenceCause ->
  ReconciliationViews ->
  CarriedStampedSurvivor ->
  StructuralApplication ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareCarriedStructuralReconciliationAt terminalCause views carried application suppliedFresh predecessor = do
  let stamp = carriedStampedSurvivorStamp carried
      occurrence = application.occurrence
  if structuralOccurrenceStampOccurrence stamp == occurrence
    && structuralOccurrenceStampPredecessor stamp == application.predecessor
    && structuralOccurrenceStampPublication stamp
      == checkedPublicationId application.publication
    then Right ()
    else Left (StructuralCarriedStampMismatch occurrence)
  prepareStructuralReconciliationAt
    terminalCause
    views
    (carriedStampedSurvivorPredecessor carried)
    application
    suppliedFresh
    predecessor

prepareStructuralReconciliationAt ::
  Maybe StructuralConsequenceCause ->
  ReconciliationViews ->
  StructuralVersionVector ->
  StructuralApplication ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareStructuralReconciliationAt terminalCause views causalPredecessor application suppliedFresh predecessor =
  case terminalCause of
    Nothing ->
      prepareStructuralReconciliationRawAt
        views
        causalPredecessor
        application
        suppliedFresh
        predecessor
    Just cause ->
      prepareTerminallySuppressedStructuralReconciliationAt
        cause
        views
        causalPredecessor
        application
        suppliedFresh
        predecessor

prepareStructuralReconciliationRawAt ::
  ReconciliationViews ->
  StructuralVersionVector ->
  StructuralApplication ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareStructuralReconciliationRawAt =
  prepareStructuralReconciliationRawAtWithChecks DiagnosticChecksEnabled

prepareStructuralReconciliationRawAtWithChecks ::
  DiagnosticChecks ->
  ReconciliationViews ->
  StructuralVersionVector ->
  StructuralApplication ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareStructuralReconciliationRawAtWithChecks checks views causalPredecessor application suppliedFresh predecessor = do
  validateEffectiveSorts views
  validateCarrierOccurrence views application
  validateLocalStores views predecessor
  case Map.lookup application.occurrence predecessor.history of
    Just incumbent
      | entryApplication incumbent == Just application -> do
          rejectUnexpectedFresh suppliedFresh
          Right (StructuralReady (replayPrepared application predecessor))
      | otherwise -> Left (StructuralOccurrenceConflict application.occurrence)
    Nothing -> do
      validateCausalShapeAtWithChecks checks causalPredecessor application predecessor
      rejectPublicationReuse application predecessor
      prepareFirst
  where
    prepareFirst = do
      carrier <- decodeCarrier application
      let role = decodedCarrierRole carrier
          object = decodedCarrierObject carrier
          entry = AppliedEntry (OccurrenceEntrySource application) carrier
          causalIncumbent =
            winnerForObjectAt
              (Just causalPredecessor)
              object
              predecessor
          causalWins = maybe True (entryWins entry) causalIncumbent
      validateBaselineObjectRepresentation views predecessor object
      validateObjectRoleClaim predecessor object role
      if causalWins
        then do
          decoded <-
            resolveMeaning views predecessor (Just causalPredecessor) carrier
          case decoded of
            Left dependencies -> Right (StructuralHeld dependencies)
            Right meaning -> prepareDecoded entry object causalIncumbent meaning
        else prepareDominated entry

    prepareDominated entry = do
      rejectUnexpectedFresh suppliedFresh
      let successor =
            predecessor
              { history = Map.insert application.occurrence entry predecessor.history,
                sourceHistory = advanceStructuralSourceHistory application predecessor,
                currentWinners = insertCurrentWinner entry predecessor.currentWinners
              }
      Right
        ( StructuralReady
            PreparedStructuralReconciliation
              { classification = StructuralApplicationAppliedNonWinner,
                changesState = True,
                application = Just application,
                controlledPatch = ControlledPatch (Just application) Nothing,
                graphPatch = emptyGraphPatch,
                placementPatch = PlacementPatch Map.empty,
                storePatch = StorePatch Map.empty Map.empty,
                debts = normalizeStructuralDebts [],
                predecessorState = predecessor,
                successor,
                satisfiedDependencies =
                  Set.singleton
                    (StructuralPredecessorDependency application.occurrence)
              }
        )

    prepareDecoded entry object causalIncumbent meaning = do
      let
        currentBefore = winnerForObject object predecessor
        historySuccessor =
          retainDelayedProcessEnd
            views
            object
            meaning
            predecessor
              { history = Map.insert application.occurrence entry predecessor.history,
                sourceHistory = advanceStructuralSourceHistory application predecessor,
                currentWinners = insertCurrentWinner entry predecessor.currentWinners,
                projectionMeanings =
                  Map.insert
                    (entryProvenance entry)
                    meaning
                    predecessor.projectionMeanings
              }
        resourceMeaning = delayedResourceMeaning views object historySuccessor meaning
        currentAfter = winnerForObject object historySuccessor
        currentWins =
          maybe
            False
            ((== Just application.occurrence) . entryOccurrence)
            currentAfter
      resolvedIncumbent <- resolveCausalIncumbent causalIncumbent
      case resolvedIncumbent of
        Left dependencies -> Right (StructuralHeld dependencies)
        Right causalIncumbentMeaning -> do
          resourcePlan <-
            prepareCausalResource
              views
              suppliedFresh
              True
              (entryProvenance entry)
              (causalIncumbent >>= resourceForEntry predecessor)
              predecessor
              resourceMeaning
          case resourcePlan of
            Left dependencies -> Right (StructuralHeld dependencies)
            Right resources ->
              finish
                currentBefore
                historySuccessor
                currentAfter
                currentWins
                causalIncumbentMeaning
                resourceMeaning
                resources
      where
        resolveCausalIncumbent Nothing = Right (Right Nothing)
        resolveCausalIncumbent (Just incumbent) =
          fmap
            (fmap Just)
            ( resolveMeaning
                views
                predecessor
                (Just causalPredecessor)
                incumbent.intrinsic
            )

        finish
          currentBefore
          historySuccessor
          currentAfter
          currentWins
          causalIncumbentMeaning
          resourceMeaning
          resources = do
            let plan =
                  prepareLocalDestination
                    currentWins
                    predecessor
                    resources.resource
                    resourceMeaning
            let successor =
                  historySuccessor
                    { localStores = plan.successorStores,
                      retainedStoreResources = resources.successorRetainedStoreResources,
                      projectionStoreResources =
                        resources.successorProjectionStoreResources,
                      usedStoreIncarnations = resources.successorUsedStoreIncarnations
                    }
                winnerChanged =
                  (entryProvenance <$> currentBefore)
                    /= (entryProvenance <$> currentAfter)
                controlledChange =
                  if winnerChanged
                    then
                      Just
                        ( ExactChange
                            (appliedControlledProjection <$> currentBefore)
                            (appliedControlledProjection <$> currentAfter)
                        )
                    else Nothing
                projectedBefore =
                  currentBefore >>= effectiveProjectedEntry predecessor
                projectedAfter =
                  currentAfter >>= effectiveProjectedEntry successor
                graphChanged =
                  not (sameMaybeProjectedGraph projectedBefore projectedAfter)
                graphPatch =
                  if graphChanged
                    then
                      graphChangeBetween
                        object
                        projectedBefore
                        projectedAfter
                    else emptyGraphPatch
                debts =
                  prepareDebts
                    views
                    (structuralOccurrenceCause application.occurrence)
                    causalIncumbentMeaning
                    (Just meaning)
                    predecessor.retainedStoreResources
                    plan.successorStores
                    plan.change
                    resources.resource
                    resources.newlyRetainedResource
                    Nothing
            Right
              ( StructuralReady
                  PreparedStructuralReconciliation
                    { classification =
                        if currentWins
                          then StructuralApplicationAppliedWinner
                          else StructuralApplicationAppliedNonWinner,
                      changesState = True,
                      application = Just application,
                      controlledPatch = ControlledPatch (Just application) controlledChange,
                      graphPatch,
                      placementPatch = PlacementPatch plan.change,
                      storePatch =
                        StorePatch
                          plan.change
                          (retainedResourceCreation resources.newlyRetainedResource),
                      debts,
                      predecessorState = predecessor,
                      successor,
                      satisfiedDependencies =
                        Set.insert
                          (StructuralPredecessorDependency application.occurrence)
                          (satisfiedBy meaning)
                    }
              )

-- | Install (or retain) the terminal overlay before interpreting a delayed
-- structural carrier.  A live projection must already have been removed by
-- the shared terminal applicator, which also installs this exact overlay.  The
-- only missing-overlay case is therefore an object with no structural winner
-- at deletion time; retaining its authority-only overlay creates no fallout.
-- Keeping this prelude patch-free is important: the enclosing incoming-item
-- transaction is occurrence-qualified, whereas terminal removal fallout is
-- qualified by the deletion cause and its control index.
prepareTerminallySuppressedStructuralReconciliationAt ::
  StructuralConsequenceCause ->
  ReconciliationViews ->
  StructuralVersionVector ->
  StructuralApplication ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareTerminallySuppressedStructuralReconciliationAt
  cause
  views
  causalPredecessor
  application
  suppliedFresh
  predecessor = do
    carrier <- decodeCarrier application
    let object = decodedCarrierObject carrier
        terminalViews =
          views
            { normalPossessions =
                Set.filter ((/= object) . snd) views.normalPossessions
            }
    prelude <- prepareTerminalSuppressionPrelude cause object predecessor
    case prelude of
      StructuralHeld dependencies -> Right (StructuralHeld dependencies)
      StructuralReady terminalPrepared -> do
        -- Terminal suppression removes only live realization.  We still
        -- resolve the complete historical meaning: an earlier control-prefix
        -- snapshot must know an Edge endpoint's vertex role and a root's exact
        -- sort/controller.  Filtering possession below rules out the one
        -- realization-only prerequisite, a fresh local Store incarnation.
        applicationResult <-
          prepareStructuralReconciliationRawAt
            terminalViews
            causalPredecessor
            application
            suppliedFresh
            terminalPrepared.successor
        case applicationResult of
          StructuralHeld dependencies -> Right (StructuralHeld dependencies)
          StructuralReady applicationPrepared -> do
            validateTerminalApplicationPatch object applicationPrepared
            Right
              ( StructuralReady
                  applicationPrepared
                    { changesState = terminalPrepared.changesState || applicationPrepared.changesState,
                      graphPatch = terminalPrepared.graphPatch,
                      placementPatch = terminalPrepared.placementPatch,
                      storePatch = terminalPrepared.storePatch,
                      debts = terminalPrepared.debts,
                      predecessorState = predecessor
                    }
              )

prepareTerminalSuppressionPrelude ::
  StructuralConsequenceCause ->
  GlobalObjectId ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareTerminalSuppressionPrelude cause object predecessor = do
  controlIndex' <-
    maybe
      (Left (StructuralControlCauseNotOracleOrdered cause))
      Right
      (structuralConsequenceCauseControlIndex cause)
  case Map.lookup object predecessor.controlOverlays of
    Just (StructuralDeletedOverlay retained)
      | retained == cause -> Right (StructuralReady (unchangedPrelude predecessor))
      | otherwise ->
          Left (StructuralTerminalSuppressionCauseConflict object retained cause)
    Just (StructuralControllerOverlay _ _ _) ->
      Left (StructuralTerminalSuppressionProjectedObject object)
    Nothing -> case winnerForObject object predecessor of
      Just _ -> Left (StructuralTerminalSuppressionProjectedObject object)
      Nothing ->
        let overlay = StructuralDeletedOverlay cause
            successor =
              predecessor
                { controlOverlays =
                    Map.insert object overlay predecessor.controlOverlays,
                  controlHistory =
                    Map.insert
                      (controlIndex', object)
                      overlay
                      predecessor.controlHistory
                }
         in Right
              ( StructuralReady
                  ( (unchangedPrelude predecessor)
                      { successor = successor,
                        changesState = True
                      }
                  )
              )
  where
    unchangedPrelude state =
      PreparedStructuralReconciliation
        { classification = StructuralProjectionRefresh,
          changesState = False,
          application = Nothing,
          controlledPatch = ControlledPatch Nothing Nothing,
          graphPatch = emptyGraphPatch,
          placementPatch = PlacementPatch Map.empty,
          storePatch = StorePatch Map.empty Map.empty,
          debts = normalizeStructuralDebts [],
          predecessorState = state,
          successor = state,
          satisfiedDependencies = Set.empty
        }

validateTerminalApplicationPatch ::
  GlobalObjectId ->
  PreparedStructuralReconciliation ->
  Either StructuralReconciliationProblem ()
validateTerminalApplicationPatch object prepared
  | not (structuralGraphPatchNull prepared.graphPatch) = fault
  | not (Map.null (placementPatchChanges prepared.placementPatch)) = fault
  | not (Map.null (storePatchChanges prepared.storePatch)) = fault
  | not (Map.null (storePatchRetainedResourceChanges prepared.storePatch)) = fault
  | maybe
      False
      (const True)
      (winnerForObject object prepared.successor >>= effectiveProjectedEntry prepared.successor) =
      fault
  | any ((== object) . dynamicStoreController) (Map.elems prepared.successor.localStores) = fault
  | otherwise = Right ()
  where
    fault = Left (StructuralTerminalSuppressionProjectedObject object)

-- | Refresh receiver-local activity and Store resources for one retained
-- projectable occurrence. Its admitted sort occurrence, endpoints and original
-- controller residence remain immutable: current dependency views must not
-- reinterpret the same structural dot or change an older installed cut.
-- This is not another structural application: it neither inserts a history
-- occurrence nor reports a Controlled observation. The structural coordinator
-- interprets its exact projection/store/debt patch atomically without advancing a
-- structural vector or completing a peer-stream item. Ordered label and End
-- changes use the separate control-refresh APIs. Baseline winners have no
-- structural occurrence and remain outside this API.
prepareStructuralProjectionRefresh ::
  ReconciliationViews ->
  StructuralOccurrenceId ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareStructuralProjectionRefresh views occurrence suppliedFresh predecessor = do
  validateEffectiveSorts views
  validateLocalStores views predecessor
  incumbent <-
    maybe
      (Left (StructuralRefreshOccurrenceMissing occurrence))
      Right
      (Map.lookup occurrence predecessor.history)
  application <-
    maybe
      (Left (StructuralRefreshOccurrenceMissing occurrence))
      Right
      (entryApplication incumbent)
  validateCarrierOccurrence views application
  let carrier = incumbent.intrinsic
      object = decodedCarrierObject carrier
      provenance = entryProvenance incumbent
      currentWins =
        maybe
          False
          ((== Just occurrence) . entryOccurrence)
          (winnerForObject object predecessor)
  incumbentMeaning <-
    maybe
      (Left (StructuralRefreshOccurrenceNotProjectable occurrence))
      Right
      (Map.lookup provenance predecessor.projectionMeanings)
  prepareOccurrence
    object
    currentWins
    incumbent
    incumbentMeaning
  where
    prepareOccurrence object currentWins incumbent incumbentMeaning = do
      let provenance = entryProvenance incumbent
          previousResource = resourceForEntry predecessor incumbent
          meaning = case meaningController incumbentMeaning of
            Nothing -> incumbentMeaning
            Just controller ->
              reviseMeaningController
                controller
                (receiverControllerActivity views object controller)
                incumbentMeaning
      let resourceMeaning = delayedResourceMeaning views object predecessor meaning
      resourcePlan <-
        prepareCausalResource
          views
          suppliedFresh
          True
          provenance
          previousResource
          predecessor
          resourceMeaning
      case resourcePlan of
        Left dependencies -> Right (StructuralHeld dependencies)
        Right resources ->
          let plan =
                prepareLocalDestination
                  currentWins
                  predecessor
                  resources.resource
                  resourceMeaning
              meaningChanged = meaning /= incumbentMeaning
              storesChanged = not (Map.null plan.change)
              resourceStateChanged =
                predecessor.retainedStoreResources
                  /= resources.successorRetainedStoreResources
                  || predecessor.projectionStoreResources
                    /= resources.successorProjectionStoreResources
                  || predecessor.usedStoreIncarnations
                    /= resources.successorUsedStoreIncarnations
              changed = meaningChanged || storesChanged || resourceStateChanged
              successorResource =
                Map.lookup provenance resources.successorProjectionStoreResources
              releasedResource =
                previousResource >>= \old ->
                  if Just old == successorResource
                    then Nothing
                    else Just old
              successor =
                if changed
                  then
                    predecessor
                      { projectionMeanings =
                          Map.insert
                            (entryProvenance incumbent)
                            meaning
                            predecessor.projectionMeanings,
                        localStores = plan.successorStores,
                        retainedStoreResources =
                          resources.successorRetainedStoreResources,
                        projectionStoreResources =
                          resources.successorProjectionStoreResources,
                        usedStoreIncarnations =
                          resources.successorUsedStoreIncarnations
                      }
                  else predecessor
              graphPatch =
                if currentWins && meaningChanged
                  then
                    graphChangeBetween
                      object
                      (effectiveProjectedEntry predecessor incumbent)
                      (effectiveProjectedEntry successor incumbent)
                  else emptyGraphPatch
              debts =
                if changed
                  then
                    prepareDebts
                      views
                      (structuralOccurrenceCause occurrence)
                      (Just incumbentMeaning)
                      (Just meaning)
                      predecessor.retainedStoreResources
                      plan.successorStores
                      plan.change
                      resources.resource
                      resources.newlyRetainedResource
                      releasedResource
                  else normalizeStructuralDebts []
           in Right
                ( StructuralReady
                    PreparedStructuralReconciliation
                      { classification = StructuralProjectionRefresh,
                        changesState = changed,
                        application = Nothing,
                        controlledPatch = ControlledPatch Nothing Nothing,
                        graphPatch,
                        placementPatch = PlacementPatch plan.change,
                        storePatch =
                          StorePatch
                            plan.change
                            (retainedResourceCreation resources.newlyRetainedResource),
                        debts,
                        predecessorState = predecessor,
                        successor,
                        satisfiedDependencies = Set.empty
                      }
                )

-- | Prove that one released label leaves the current structural projection and
-- all of its Store resources unchanged. The supplied views need only contain
-- the released controller's residence and possession of this object. Sort and
-- baseline views are never inspected: the immutable target meaning has already
-- been admitted, and the maintained carrier index selects it directly.
--
-- A failed proof asks the caller to use the full refresh below. In particular,
-- controller/activity equality alone does not prove a Delta no-op: its exact
-- retained resource and current Store destination must agree as well.
prepareUnchangedStructuralLabelControlRefresh ::
  ReconciliationViews ->
  StructuralConsequenceCause ->
  GlobalObjectId ->
  ReleasedLabelState ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem (Maybe PreparedStructuralReconciliation)
prepareUnchangedStructuralLabelControlRefresh views cause object released suppliedFresh predecessor =
  case releasedLabelStateView released of
    ReleasedDeletedView {} -> Right Nothing
    ReleasedLabelView label -> do
      _ <-
        maybe
          (Left (StructuralControlCauseNotOracleOrdered cause))
          Right
          (structuralConsequenceCauseControlIndex cause)
      case Map.lookup object predecessor.controlOverlays of
        Just StructuralDeletedOverlay {} ->
          Left (StructuralControlRestoresDeletedObject object)
        _ -> Right ()
      entry <-
        maybe
          (Left (StructuralControlTargetMissing object))
          Right
          (winnerForObject object predecessor)
      before <-
        maybe
          (Left (StructuralControlTargetMissing object))
          Right
          (effectiveProjectedEntry predecessor entry)
      projected <- controllerProjection views label
      case projected of
        Left _ -> Right Nothing
        Right (controller, initialActivity) -> do
          let meaning = projectedEntryMeaning before
              activity = localActivity views object controller initialActivity
              unchangedMeaning = reviseMeaningController controller activity meaning == meaning
          if unchangedMeaning && resourcesUnchanged entry meaning
            then do
              rejectUnexpectedFresh suppliedFresh
              Right
                ( Just
                    PreparedStructuralReconciliation
                      { classification = StructuralProjectionRefresh,
                        changesState = False,
                        application = Nothing,
                        controlledPatch = ControlledPatch Nothing Nothing,
                        graphPatch = emptyGraphPatch,
                        placementPatch = PlacementPatch Map.empty,
                        storePatch = StorePatch Map.empty Map.empty,
                        debts = normalizeStructuralDebts [],
                        predecessorState = predecessor,
                        successor = predecessor,
                        satisfiedDependencies = Set.empty
                      }
                )
            else Right Nothing
  where
    resourcesUnchanged entry meaning = case meaning of
      CheckedDelta target delta typedSort _ (ActiveRootHere process) ->
        case resourceForEntry predecessor entry of
          Nothing -> False
          Just store ->
            destinationLogicalKey store
              == (process, target, views.localHerald, delta, typedSort)
              && Map.lookup delta predecessor.localStores == Just store
              && Map.lookup store.incarnation predecessor.retainedStoreResources == Just store
              && Map.lookup store.incarnation predecessor.usedStoreIncarnations == Just store.incarnationSource
      CheckedDelta _ delta _ _ _ ->
        resourceForEntry predecessor entry == Nothing
          && Map.notMember delta predecessor.localStores
      _ -> True

-- | Apply one released label to the current structural winner. The Oracle
-- control is retained as an object-qualified overlay while the checked
-- genesis, imported-publication, or occurrence carrier provenance remains
-- immutable. Genesis Store and Graph owners are refreshed through the same
-- prepared transaction without fabricating a publication or occurrence.
prepareStructuralLabelControlRefresh ::
  ReconciliationViews ->
  StructuralConsequenceCause ->
  GlobalObjectId ->
  ReleasedLabelState ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareStructuralLabelControlRefresh views cause object released suppliedFresh predecessor = do
  validateEffectiveSorts views
  validateLocalStores views predecessor
  entry <-
    maybe
      (Left (StructuralControlTargetMissing object))
      Right
      (winnerForObject object predecessor)
  case releasedLabelStateView released of
    ReleasedDeletedView {} ->
      prepareStructuralControlRefresh
        views
        cause
        object
        entry
        Nothing
        suppliedFresh
        predecessor
    ReleasedLabelView label -> do
      projected <- controllerProjection views label
      case projected of
        Left dependencies -> Right (StructuralHeld dependencies)
        Right (controller, initialActivity) ->
          prepareStructuralControlRefresh
            views
            cause
            object
            entry
            (Just (controller, localActivity views object controller initialActivity))
            suppliedFresh
            predecessor

-- | Prepare every controller revision caused by one ended process, including
-- checked genesis roots and imported publication baselines which have no
-- structural occurrence. The explicit target sum prevents callers from
-- fabricating an occurrence merely to name one of those owners.
-- Preparations are chained against one another but remain non-committing; a
-- caller can prepare all affected live owners before exposing any successor.
prepareStructuralProcessEndRefreshes ::
  ReconciliationViews ->
  StructuralConsequenceCause ->
  ProcessEpochId ->
  StructuralAppliedState ->
  Either
    StructuralReconciliationProblem
    [(StructuralControlTarget, PreparedStructuralReconciliation)]
prepareStructuralProcessEndRefreshes views cause process initial =
  go initial affected []
  where
    affected =
      [ (object, controlTargetForEntry entry, entry)
      | (object, entry) <- Map.toAscList (winningEntries initial),
        Just projected <- [effectiveProjectedEntry initial entry],
        projectedEntryHasLiveController process projected
      ]

    go _ [] prepared = Right (reverse prepared)
    go predecessor ((object, target, entry) : remaining) prepared = do
      result <-
        prepareStructuralControlRefresh
          views
          cause
          object
          entry
          (Just (ZombieProcessController process, PassiveRoot))
          Nothing
          predecessor
      case result of
        StructuralHeld _ -> Left (StructuralProcessEndControlHeld process)
        StructuralReady ready ->
          go
            (preparedStructuralSuccessor ready)
            remaining
            ((target, ready) : prepared)

controlTargetForEntry :: AppliedEntry -> StructuralControlTarget
controlTargetForEntry entry =
  case entryProvenance entry of
    GenesisStructuralProvenance object -> GenesisStructuralControlTarget object
    BaselineStructuralProvenance publication ->
      PublishedBaselineStructuralControlTarget (entryObject entry) publication
    OccurrenceStructuralProvenance occurrence _ ->
      OccurrenceStructuralControlTarget occurrence

projectedEntryHasLiveController ::
  ProcessEpochId -> ProjectedEntry -> Bool
projectedEntryHasLiveController process (ProjectedEntry _ meaning) =
  case meaning of
    CheckedNabla _ _ _ (LiveProcessController observed _) _ -> observed == process
    CheckedDelta _ _ _ (LiveProcessController observed _) _ -> observed == process
    _ -> False

prepareStructuralControlRefresh ::
  ReconciliationViews ->
  StructuralConsequenceCause ->
  GlobalObjectId ->
  AppliedEntry ->
  Maybe (ControllerProjection, RootActivityProjection) ->
  Maybe FreshStoreIncarnationEvidence ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem StructuralPreparation
prepareStructuralControlRefresh
  views
  cause
  object
  entry
  replacement
  suppliedFresh
  predecessor = do
    controlIndex' <-
      maybe
        (Left (StructuralControlCauseNotOracleOrdered cause))
        Right
        (structuralConsequenceCauseControlIndex cause)
    case (Map.lookup object predecessor.controlOverlays, replacement) of
      (Just StructuralDeletedOverlay {}, Just _) ->
        Left (StructuralControlRestoresDeletedObject object)
      _ -> Right ()
    before <-
      maybe
        (Left (StructuralControlTargetMissing object))
        Right
        (effectiveProjectedEntry predecessor entry)
    let beforeMeaning = projectedEntryMeaning before
        afterMeaning =
          fmap
            (\(controller, activity) -> reviseMeaningController controller activity beforeMeaning)
            replacement
        provenance = entryProvenance entry
        previousResource = resourceForEntry predecessor entry
    resourceResult <- case afterMeaning of
      Nothing -> do
        rejectUnexpectedFresh suppliedFresh
        Right
          ( Right
              ( (unchangedCausalResourcePlan predecessor)
                  { successorProjectionStoreResources =
                      Map.delete provenance predecessor.projectionStoreResources
                  }
              )
          )
      Just meaning ->
        prepareCausalResource
          views
          suppliedFresh
          True
          provenance
          previousResource
          predecessor
          meaning
    case resourceResult of
      Left dependencies -> Right (StructuralHeld dependencies)
      Right resources -> do
        let plan = case afterMeaning of
              Nothing -> prepareDeletedLocalDestination predecessor beforeMeaning
              Just meaning ->
                prepareLocalDestination True predecessor resources.resource meaning
            successorResource =
              Map.lookup provenance resources.successorProjectionStoreResources
            releasedResource =
              previousResource >>= \old ->
                if Just old == successorResource then Nothing else Just old
            candidateOverlay = case replacement of
              Nothing -> StructuralDeletedOverlay cause
              Just (controller, activity) ->
                StructuralControllerOverlay cause controller activity
            after = ProjectedEntry entry <$> afterMeaning
            graphChanged = not (sameProjectedGraph before after)
            storesChanged = not (Map.null plan.change)
            resourceStateChanged =
              predecessor.retainedStoreResources
                /= resources.successorRetainedStoreResources
                || predecessor.projectionStoreResources
                  /= resources.successorProjectionStoreResources
                || predecessor.usedStoreIncarnations
                  /= resources.successorUsedStoreIncarnations
            changed = graphChanged || storesChanged || resourceStateChanged
            successor =
              if changed
                then
                  predecessor
                    { localStores = plan.successorStores,
                      retainedStoreResources = resources.successorRetainedStoreResources,
                      projectionStoreResources = resources.successorProjectionStoreResources,
                      usedStoreIncarnations = resources.successorUsedStoreIncarnations,
                      controlOverlays =
                        if graphChanged
                          then Map.insert object candidateOverlay predecessor.controlOverlays
                          else predecessor.controlOverlays,
                      controlHistory =
                        if graphChanged
                          then
                            Map.insert
                              (controlIndex', object)
                              candidateOverlay
                              predecessor.controlHistory
                          else predecessor.controlHistory
                    }
                else predecessor
            graphPatch =
              if graphChanged
                then graphChangeBetween object (Just before) after
                else emptyGraphPatch
            debts =
              if changed
                then
                  prepareDebts
                    views
                    cause
                    (Just beforeMeaning)
                    afterMeaning
                    predecessor.retainedStoreResources
                    plan.successorStores
                    plan.change
                    resources.resource
                    resources.newlyRetainedResource
                    releasedResource
                else normalizeStructuralDebts []
        Right
          ( StructuralReady
              PreparedStructuralReconciliation
                { classification = StructuralProjectionRefresh,
                  changesState = changed,
                  application = Nothing,
                  controlledPatch = ControlledPatch Nothing Nothing,
                  graphPatch,
                  placementPatch = PlacementPatch plan.change,
                  storePatch =
                    StorePatch
                      plan.change
                      (retainedResourceCreation resources.newlyRetainedResource),
                  debts,
                  predecessorState = predecessor,
                  successor,
                  satisfiedDependencies = Set.empty
                }
          )

projectedEntryMeaning :: ProjectedEntry -> CheckedStructuralMeaning
projectedEntryMeaning (ProjectedEntry _ meaning) = meaning

sameProjectedGraph :: ProjectedEntry -> Maybe ProjectedEntry -> Bool
sameProjectedGraph before after =
  entryVertexProjection before == (after >>= entryVertexProjection)
    && entryEdgeProjection before == (after >>= entryEdgeProjection)

sameMaybeProjectedGraph :: Maybe ProjectedEntry -> Maybe ProjectedEntry -> Bool
sameMaybeProjectedGraph before after =
  (before >>= entryVertexProjection) == (after >>= entryVertexProjection)
    && (before >>= entryEdgeProjection) == (after >>= entryEdgeProjection)

prepareDeletedLocalDestination ::
  StructuralAppliedState -> CheckedStructuralMeaning -> LocalDestinationPlan
prepareDeletedLocalDestination predecessor meaning = case meaning of
  CheckedDelta _ delta _ _ _ ->
    let current = Map.lookup delta predecessor.localStores
     in LocalDestinationPlan
          (maybe Map.empty (\store -> Map.singleton delta (ExactChange (Just store) Nothing)) current)
          (Map.delete delta predecessor.localStores)
  _ -> unchangedLocalDestinationPlan predecessor

replayPrepared ::
  StructuralApplication ->
  StructuralAppliedState ->
  PreparedStructuralReconciliation
replayPrepared application predecessor =
  PreparedStructuralReconciliation
    { classification = StructuralApplicationReplay,
      changesState = False,
      application = Just application,
      controlledPatch = ControlledPatch Nothing Nothing,
      graphPatch = emptyGraphPatch,
      placementPatch = PlacementPatch Map.empty,
      storePatch = StorePatch Map.empty Map.empty,
      debts = normalizeStructuralDebts [],
      predecessorState = predecessor,
      successor = predecessor,
      satisfiedDependencies = Set.empty
    }

validateCarrierOccurrence ::
  ReconciliationViews ->
  StructuralApplication ->
  Either StructuralReconciliationProblem ()
validateCarrierOccurrence views application = do
  validateCarrierDefinition views application.carrierSort application.publication

validateCarrierDefinition ::
  ReconciliationViews ->
  SortOccurrence ->
  CheckedPublication ->
  Either StructuralReconciliationProblem ()
validateCarrierDefinition views carrierSort publication = do
  let actualSort = checkedPublicationSort publication
      claimedSort = sortOccurrenceSortId carrierSort
  if actualSort == claimedSort
    then Right ()
    else Left (StructuralCarrierSortMismatch claimedSort actualSort)
  effective <-
    maybe
      (Left (StructuralCarrierSortUnavailable claimedSort))
      Right
      (Map.lookup claimedSort views.effectiveSorts)
  if effective == carrierSort
    then Right ()
    else
      Left
        ( StructuralCarrierOccurrenceMismatch
            claimedSort
            (sortOccurrenceDefinition effective)
            (sortOccurrenceDefinition carrierSort)
        )
  case carrierRoleForSort claimedSort of
    Nothing -> Left (StructuralCarrierUnsupported claimedSort)
    Just ProcessEpochCarrier -> Left StructuralProcessCarrierUnsupported
    Just _ -> Right ()

validateEffectiveSorts ::
  ReconciliationViews ->
  Either StructuralReconciliationProblem ()
validateEffectiveSorts views =
  mapM_ validate (Map.toAscList views.effectiveSorts)
  where
    validate (key, occurrence)
      | key == sortOccurrenceSortId occurrence = Right ()
      | otherwise =
          Left
            ( StructuralEffectiveSortKeyMismatch
                key
                (sortOccurrenceSortId occurrence)
            )

validateLocalStores ::
  ReconciliationViews ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem ()
validateLocalStores views state =
  do
    mapM_ validate (Map.toAscList state.localStores)
    validateUniqueIncarnations (Map.elems state.localStores)
  where
    validate (key, store)
      | key /= store.delta = Left (StructuralLocalStoreKeyMismatch key store.delta)
      | store.herald /= views.localHerald =
          Left (StructuralLocalStoreHeraldMismatch views.localHerald store.herald)
      | otherwise = Right ()

validateUniqueIncarnations ::
  [DynamicLocalStoreSpec] ->
  Either StructuralReconciliationProblem ()
validateUniqueIncarnations = go Map.empty
  where
    go _ [] = Right ()
    go observed (store : remaining) =
      case Map.lookup store.incarnation observed of
        Nothing -> go (Map.insert store.incarnation store.delta observed) remaining
        Just incumbentDelta ->
          Left
            ( StructuralLocalStoreIncarnationConflict
                store.incarnation
                incumbentDelta
                store.delta
            )

membershipOf :: StructuralVersionVector -> Set HeraldEpoch
membershipOf = Set.fromList . fmap fst . structuralVersionVectorEntries

vectorMatchesMembership ::
  StructuralVersionVector -> StructuralAppliedState -> Bool
vectorMatchesMembership vector state =
  structuralVersionVectorMembershipGenerationId vector
    == state.membershipGenerationId
    && structuralVersionVectorMemberSetDigest vector == state.memberSetDigest
    && membershipOf vector == state.membership

validateCausalShapeAt ::
  StructuralVersionVector ->
  StructuralApplication ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem ()
validateCausalShapeAt = validateCausalShapeAtWithChecks DiagnosticChecksEnabled

validateCausalShapeAtWithChecks ::
  DiagnosticChecks ->
  StructuralVersionVector ->
  StructuralApplication ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem ()
validateCausalShapeAtWithChecks checks causalPredecessor application state = do
  let occurrence = application.occurrence
      occurrenceSource = structuralOccurrenceSourceHeraldEpoch occurrence
      publicationSource = publicationSourceHeraldEpoch (checkedPublicationId application.publication)
  if occurrenceSource == publicationSource
    then Right ()
    else
      Left
        ( StructuralOccurrencePublicationSourceMismatch
            occurrenceSource
            publicationSource
        )
  if vectorMatchesMembership causalPredecessor state
    then Right ()
    else Left StructuralPredecessorMembershipMismatch
  observedSourcePrefix <-
    maybe
      (Left (StructuralPredecessorSourceMissing occurrenceSource))
      Right
      (structuralVersionVectorComponent occurrenceSource causalPredecessor)
  if observedSourcePrefix
    == structuralPredecessorPrefix (structuralOccurrenceSourceSequence occurrence)
    then Right ()
    else Left (StructuralPredecessorSourceMismatch occurrenceSource)
  runDiagnosticCheck checks $ do
    validateCoveredDots causalPredecessor state
    validateProjectionClosure causalPredecessor state

validateCoveredDots ::
  StructuralVersionVector ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem ()
validateCoveredDots frontier state =
  mapM_ requireCoveredComponent (structuralVersionVectorEntries frontier)
  where
    requireCoveredComponent (source, requested) =
      let retained = maybe Structural.emptyStructuralPrefix (structuralPrefixThrough . (.through)) (Map.lookup source state.sourceHistory)
       in unless
            (retained >= requested)
            (Left (StructuralPredecessorDotMissing (structuralOccurrenceId source (Structural.nextAfterStructuralPrefix retained))))

validateCoveredDotsExhaustive ::
  StructuralVersionVector -> StructuralAppliedState -> Either StructuralReconciliationProblem ()
validateCoveredDotsExhaustive frontier state =
  mapM_ requireCoveredComponent (structuralVersionVectorEntries frontier)
  where
    requireCoveredComponent (herald, prefix) =
      case structuralPrefixSequence prefix of
        Nothing -> Right ()
        Just through ->
          mapM_
            (requireDot herald)
            [1 .. structuralSequenceWord64 through]
    requireDot herald sequenceNumber =
      let sequenceValue =
            either
              (const (error "positive structural sequence rejected"))
              id
              (mkStructuralSequence sequenceNumber)
          required = structuralOccurrenceId herald sequenceValue
       in if Map.member required state.history
            then Right ()
            else Left (StructuralPredecessorDotMissing required)

rejectDuplicateBaselinePublications ::
  Map GlobalObjectId AppliedEntry ->
  Either StructuralReconciliationProblem ()
rejectDuplicateBaselinePublications = go Set.empty . Map.elems
  where
    go _ [] = Right ()
    go observed (entry : remaining) =
      case checkedPublicationId <$> entryPublication entry of
        Nothing -> go observed remaining
        Just publication ->
          if Set.member publication observed
            then Left (StructuralBaselinePublicationConflict publication)
            else go (Set.insert publication observed) remaining

rejectPublicationReuse ::
  StructuralApplication ->
  StructuralAppliedState ->
  Either StructuralReconciliationProblem ()
rejectPublicationReuse application state =
  if any ((== Just publication) . fmap checkedPublicationId . entryPublication) (Map.elems state.baselineWinners)
    then Left (StructuralPublicationBaselineConflict publication)
    else case [ occurrence
              | (occurrence, entry) <- Map.toAscList state.history,
                fmap checkedPublicationId (entryPublication entry) == Just publication
              ] of
      [] -> Right ()
      incumbent : _ ->
        Left
          ( StructuralPublicationOccurrenceConflict
              publication
              incumbent
              application.occurrence
          )
  where
    publication = checkedPublicationId application.publication

decodeCarrier ::
  StructuralApplication ->
  Either StructuralReconciliationProblem DecodedStructuralCarrier
decodeCarrier = decodeCarrierPublication . structuralApplicationPublication

decodeCarrierPublication ::
  CheckedPublication ->
  Either StructuralReconciliationProblem DecodedStructuralCarrier
decodeCarrierPublication publication = do
  role <-
    maybe
      (Left (StructuralCarrierUnsupported (checkedPublicationSort publication)))
      Right
      (carrierRoleForSort (checkedPublicationSort publication))
  case role of
    ProcessEpochCarrier -> Left StructuralProcessCarrierUnsupported
    NeutralVertexCarrier -> do
      object <- fieldObject role value
      label <- fieldLabel role =<< fieldValue role labelField value
      Right (DecodedNeutral object label)
    EdgeCarrier -> do
      object <- fieldObject role value
      label <- fieldLabel role =<< fieldValue role labelField value
      source <- fieldObject role =<< fieldValue role edgeSourceField value
      destination <- fieldObject role =<< fieldValue role edgeDestinationField value
      strengthValue <- fieldValue role edgeStrengthField value
      strength <- case viewValue strengthValue of
        EnumValue symbol ->
          maybe
            (Left (StructuralValueShapeContradiction role))
            Right
            (edgeStrengthFromSymbol symbol)
        _ -> Left (StructuralValueShapeContradiction role)
      Right (DecodedEdge object label source destination strength)
    NablaCarrier -> decodeRoot role DecodedNabla
    DeltaCarrier -> decodeRoot role DecodedDelta
  where
    value = checkedPublicationValue publication
    decodeRoot role build = do
      object <- fieldObject role value
      label <- fieldLabel role =<< fieldValue role labelField value
      carriedSortValue <- fieldValue role sortIdField value
      carriedSort <- case viewValue carriedSortValue of
        BytesValue bytes ->
          case mkSortId bytes of
            Right identifier -> Right identifier
            Left _ -> Left (StructuralValueShapeContradiction role)
        _ -> Left (StructuralValueShapeContradiction role)
      Right (build object label carriedSort)

decodedCarrierRole :: DecodedStructuralCarrier -> StructuralCarrierRole
decodedCarrierRole = \case
  DecodedNeutral {} -> NeutralVertexCarrier
  DecodedEdge {} -> EdgeCarrier
  DecodedNabla {} -> NablaCarrier
  DecodedDelta {} -> DeltaCarrier

decodedCarrierObject :: DecodedStructuralCarrier -> GlobalObjectId
decodedCarrierObject = \case
  DecodedNeutral object _ -> object
  DecodedEdge object _ _ _ _ -> object
  DecodedNabla object _ _ -> object
  DecodedDelta object _ _ -> object

resolveMeaning ::
  ReconciliationViews ->
  StructuralAppliedState ->
  Maybe StructuralVersionVector ->
  DecodedStructuralCarrier ->
  Either
    StructuralReconciliationProblem
    (Either (Set StructuralDependency) CheckedStructuralMeaning)
resolveMeaning views state frontier carrier =
  case carrier of
    DecodedNeutral object _ -> Right (Right (CheckedNeutral object))
    DecodedEdge object _ sourceObject destinationObject strength ->
      resolveEdge object sourceObject destinationObject strength
    DecodedNabla object label carriedSort -> resolveRoot True object label carriedSort
    DecodedDelta object label carriedSort -> resolveRoot False object label carriedSort
  where
    resolveEdge object sourceObject destinationObject strength = do
      let source = resolveEndpoint views state frontier sourceObject
          destination = resolveEndpoint views state frontier destinationObject
      case (source, destination) of
        (EndpointConflict, _) -> Left (StructuralEndpointRoleConflict sourceObject)
        (_, EndpointConflict) -> Left (StructuralEndpointRoleConflict destinationObject)
        _ ->
          let dependencies =
                Set.fromList
                  ( catMaybes
                      [ missingEndpoint sourceObject source,
                        missingEndpoint destinationObject destination
                      ]
                  )
           in if Set.null dependencies
                then case (source, destination) of
                  (EndpointReady sourceVertex, EndpointReady destinationVertex) ->
                    Right
                      ( Right
                          (CheckedEdge object (edgePayload sourceVertex destinationVertex strength))
                      )
                  _ -> Right (Left dependencies)
                else Right (Left dependencies)

    resolveRoot isNabla object label carriedSort = do
      case Map.lookup carriedSort views.effectiveSorts of
        Nothing -> Right (Left (Set.singleton (EffectiveSortDependency carriedSort)))
        Just typedSort -> do
          controller <- controllerProjection views label
          case controller of
            Left dependencies -> Right (Left dependencies)
            Right (projection, activity) ->
              if isNabla
                then
                  Right
                    ( Right
                        ( CheckedNabla
                            object
                            (nablaIdFromGlobalObjectId object)
                            typedSort
                            projection
                            (localActivity views object projection activity)
                        )
                    )
                else
                  Right
                    ( Right
                        ( CheckedDelta
                            object
                            (deltaIdFromGlobalObjectId object)
                            typedSort
                            projection
                            (localActivity views object projection activity)
                        )
                    )

fieldLabel ::
  StructuralCarrierRole ->
  Value ->
  Either StructuralReconciliationProblem Label
fieldLabel role supplied = case viewValue supplied of
  LabelValue label -> Right label
  _ -> Left (StructuralValueShapeContradiction role)

controllerProjection ::
  ReconciliationViews ->
  Label ->
  Either
    StructuralReconciliationProblem
    (Either (Set StructuralDependency) (ControllerProjection, RootActivityProjection))
controllerProjection views (owner, _) = case owner of
  VoidLabel -> Right (Right (VoidController, PassiveRoot))
  ZombieLabel process ->
    Right (Right (ZombieProcessController process, PassiveRoot))
  ProcessLabel process -> case processResidence process of
    Nothing -> Right (Left (Set.singleton (StructuralProcessDependency process)))
    Just residence ->
      Right
        ( Right
            ( LiveProcessController process residence,
              if residence == views.localHerald
                then PassiveRoot
                else RemoteController process residence
            )
        )
  where
    processResidence process = case Map.lookup process views.liveProcessResidences of
      Just residence -> Just residence
      Nothing -> fst <$> Map.lookup process views.endedProcesses

-- A previously unavailable root did not participate in the End refresh. Keep
-- its original meaning for historical cuts, and insert the missed ordered
-- overlay only if the effective controller is the ended process. An existing
-- handoff to Q, Void/Zombie label, or deletion is therefore unaffected.
retainDelayedProcessEnd ::
  ReconciliationViews ->
  GlobalObjectId ->
  CheckedStructuralMeaning ->
  StructuralAppliedState ->
  StructuralAppliedState
retainDelayedProcessEnd views object meaning initial =
  foldl' retainEnd initial endedControllers
  where
    endedControllers =
      Set.toAscList
        ( Set.fromList
            [ (index, process)
            | Just (LiveProcessController process _) <-
                [meaningController meaning, overlayController =<< Map.lookup object initial.controlOverlays],
              Just (_, index) <- [Map.lookup process views.endedProcesses]
            ]
        )
    retainEnd state (index, process)
      | Map.member (index, object) state.controlHistory = state
      | controllerAtEnd state index /= Just (LiveProcessController process residence) = state
      | otherwise =
          let cause = case processEndCause process index of
                Right checkedCause -> checkedCause
                Left _ -> error "canonical process End has a zero control index"
              overlay = StructuralControllerOverlay cause (ZombieProcessController process) PassiveRoot
              current = Map.lookup object state.controlOverlays
              currentOverlays =
                if maybe True ((< Just index) . structuralConsequenceCauseControlIndex . controlOverlayCause) current
                  then Map.insert object overlay state.controlOverlays
                  else state.controlOverlays
           in state
                { controlOverlays = currentOverlays,
                  controlHistory = Map.insert (index, object) overlay state.controlHistory
                }
      where
        residence = fst (views.endedProcesses Map.! process)

    controllerAtEnd state index = case Map.lookup object state.controlOverlays of
      Nothing -> meaningController meaning
      Just overlay
        | structuralConsequenceCauseControlIndex (controlOverlayCause overlay) <= Just index ->
            overlayController overlay
        | otherwise ->
            -- Existing exact End overlays take the indexed fast path above.
            -- Only a late carrier predating a newer handoff needs an older
            -- object control; the history is ordered primarily by index.
            case [ older
                 | ((_, candidate), older) <-
                     Map.toDescList (Map.takeWhileAntitone ((<= index) . fst) state.controlHistory),
                   candidate == object
                 ] of
              older : _ -> overlayController older
              [] -> meaningController meaning

    overlayController (StructuralDeletedOverlay _) = Nothing
    overlayController (StructuralControllerOverlay _ controller _) = Just controller

-- Delayed materialization must not allocate a Store for an ended controller.
-- Preserve intrinsic meaning above; resource realization uses the current
-- ordered overlay whenever End affects that intrinsic controller.
delayedResourceMeaning ::
  ReconciliationViews ->
  GlobalObjectId ->
  StructuralAppliedState ->
  CheckedStructuralMeaning ->
  CheckedStructuralMeaning
delayedResourceMeaning views object state meaning
  | ended (meaningController meaning) || ended currentController =
      case currentOverlay of
        Just (StructuralControllerOverlay _ controller activity) ->
          reviseMeaningController controller activity meaning
        Just (StructuralDeletedOverlay _) -> reviseMeaningController VoidController PassiveRoot meaning
        Nothing -> meaning
  | otherwise = meaning
  where
    currentOverlay = Map.lookup object state.controlOverlays
    currentController = case currentOverlay of
      Just (StructuralControllerOverlay _ controller _) -> Just controller
      _ -> Nothing
    ended (Just (LiveProcessController process _)) = Map.member process views.endedProcesses
    ended (Just (ZombieProcessController process)) = Map.member process views.endedProcesses
    ended _ = False

meaningController :: CheckedStructuralMeaning -> Maybe ControllerProjection
meaningController = \case
  CheckedNabla _ _ _ controller _ -> Just controller
  CheckedDelta _ _ _ controller _ -> Just controller
  _ -> Nothing

localActivity ::
  ReconciliationViews ->
  GlobalObjectId ->
  ControllerProjection ->
  RootActivityProjection ->
  RootActivityProjection
localActivity views object controller initial = case controller of
  LiveProcessController process residence
    | residence == views.localHerald,
      Set.member (process, object) views.normalPossessions ->
        ActiveRootHere process
  _ -> initial

fieldObject ::
  StructuralCarrierRole ->
  Value ->
  Either StructuralReconciliationProblem GlobalObjectId
fieldObject role supplied = case viewValue supplied of
  GlobalUniqueIdValue identifier -> Right (globalObjectIdFromGlobalUniqueId identifier)
  RecordValue _ -> do
    projected <- fieldValue role objectIdField supplied
    case viewValue projected of
      GlobalUniqueIdValue identifier -> Right (globalObjectIdFromGlobalUniqueId identifier)
      _ -> Left (StructuralValueShapeContradiction role)
  _ -> Left (StructuralValueShapeContradiction role)

fieldValue ::
  StructuralCarrierRole ->
  FieldName ->
  Value ->
  Either StructuralReconciliationProblem Value
fieldValue role field value =
  either
    (const (Left (StructuralValueShapeContradiction role)))
    Right
    (valueAt (directProjection field) value)

data EndpointResolution
  = EndpointMissing
  | EndpointConflict
  | EndpointReady VertexId

resolveEndpoint ::
  ReconciliationViews ->
  StructuralAppliedState ->
  Maybe StructuralVersionVector ->
  GlobalObjectId ->
  EndpointResolution
resolveEndpoint views state frontier object =
  case Set.toAscList candidates of
    [] -> EndpointMissing
    [vertex] -> EndpointReady vertex
    _ -> EndpointConflict
  where
    candidates =
      Set.fromList
        ( baselineCandidates
            <> maybe
              []
              ( maybe
                  []
                  ( maybe [] (pure . structuralVertexProjectionVertex)
                      . entryVertexProjection
                  )
                  . effectiveProjectedEntry state
              )
              (winnerForObjectAt frontier object state)
        )
    baselineCandidates = case Map.lookup object state.controlOverlays of
      Just StructuralDeletedOverlay {} -> []
      _ -> baselineVerticesForObject views object

baselineVerticesForObject :: ReconciliationViews -> GlobalObjectId -> [VertexId]
baselineVerticesForObject views object =
  [ vertex
  | vertex <-
      [ NeutralVertex object,
        NablaVertex (nablaIdFromGlobalObjectId object),
        DeltaVertex (deltaIdFromGlobalObjectId object)
      ],
    Set.member vertex views.baselineVertices
  ]

missingEndpoint ::
  GlobalObjectId ->
  EndpointResolution ->
  Maybe StructuralDependency
missingEndpoint object = \case
  EndpointMissing -> Just (StructuralEndpointDependency object)
  EndpointConflict -> Nothing
  EndpointReady _ -> Nothing

validateObjectRoleClaim ::
  StructuralAppliedState ->
  GlobalObjectId ->
  StructuralCarrierRole ->
  Either StructuralReconciliationProblem ()
validateObjectRoleClaim state object actual =
  case [entryRole entry | entry <- eligibleEntries Nothing state, entryObject entry == object] of
    [] -> Right ()
    incumbent : _
      | incumbent == actual -> Right ()
      | otherwise -> Left (StructuralObjectRoleConflict object incumbent actual)

-- | A graph baseline alone does not identify an ordinary structural winner.
-- A later occurrence may use that vertex as an endpoint, but may update the
-- same object only when its description is represented by a retained baseline
-- or occurrence. Checked application environments supply those descriptions;
-- Herald-private infrastructure may remain graph-only.
validateBaselineObjectRepresentation ::
  ReconciliationViews ->
  StructuralAppliedState ->
  GlobalObjectId ->
  Either StructuralReconciliationProblem ()
validateBaselineObjectRepresentation views state object
  | null (baselineVerticesForObject views object) = Right ()
  | any ((== object) . entryObject) (eligibleEntries Nothing state) = Right ()
  | otherwise = Left (StructuralUnrepresentedBaselineObject object)

data CausalResourcePlan = CausalResourcePlan
  { resource :: Maybe DynamicLocalStoreSpec,
    newlyRetainedResource :: Maybe DynamicLocalStoreSpec,
    successorRetainedStoreResources :: Map StoreIncarnationId DynamicLocalStoreSpec,
    successorProjectionStoreResources ::
      Map StructuralProjectionProvenance DynamicLocalStoreSpec,
    successorUsedStoreIncarnations :: Map StoreIncarnationId FreshStoreIncarnationSource
  }

prepareCausalResource ::
  ReconciliationViews ->
  Maybe FreshStoreIncarnationEvidence ->
  Bool ->
  StructuralProjectionProvenance ->
  Maybe DynamicLocalStoreSpec ->
  StructuralAppliedState ->
  CheckedStructuralMeaning ->
  Either
    StructuralReconciliationProblem
    (Either (Set StructuralDependency) CausalResourcePlan)
prepareCausalResource
  views
  suppliedFresh
  causalWins
  candidateProvenance
  reusableResource
  predecessor
  meaning
    | not causalWins = do
        rejectUnexpectedFresh suppliedFresh
        Right (Right (unchangedCausalResourcePlan predecessor))
    | otherwise = case meaning of
        CheckedDelta object delta typedSort _ (ActiveRootHere process) ->
          let logicalKey = (process, object, views.localHerald, delta, typedSort)
              reusable =
                reusableResource >>= \store ->
                  if destinationLogicalKey store == logicalKey
                    then Just store
                    else Nothing
           in case reusable of
                Just store -> do
                  rejectUnexpectedFresh suppliedFresh
                  Right
                    ( Right
                        ( (unchangedCausalResourcePlan predecessor)
                            { resource = Just store,
                              successorProjectionStoreResources =
                                Map.insert
                                  candidateProvenance
                                  store
                                  predecessor.projectionStoreResources
                            }
                        )
                    )
                Nothing -> case suppliedFresh of
                  Nothing ->
                    Right (Left (Set.singleton (FreshStoreIncarnationDependency delta)))
                  Just evidence -> do
                    let evidenceDelta = freshStoreIncarnationDelta evidence
                        incarnation = freshStoreIncarnationId evidence
                        incarnationSource = freshStoreIncarnationSource evidence
                    if evidenceDelta /= delta
                      then Left (StructuralFreshIncarnationWrongDelta delta evidenceDelta)
                      else Right ()
                    if Map.member incarnation predecessor.usedStoreIncarnations
                      then Left (StructuralFreshIncarnationReused delta incarnation)
                      else Right ()
                    let retained =
                          DynamicLocalStoreSpec
                            process
                            object
                            views.localHerald
                            delta
                            typedSort
                            incarnation
                            incarnationSource
                    Right
                      ( Right
                          CausalResourcePlan
                            { resource = Just retained,
                              newlyRetainedResource = Just retained,
                              successorRetainedStoreResources =
                                Map.insert
                                  incarnation
                                  retained
                                  predecessor.retainedStoreResources,
                              successorProjectionStoreResources =
                                Map.insert
                                  candidateProvenance
                                  retained
                                  predecessor.projectionStoreResources,
                              successorUsedStoreIncarnations =
                                Map.insert
                                  incarnation
                                  incarnationSource
                                  predecessor.usedStoreIncarnations
                            }
                      )
        CheckedDelta {} -> do
          rejectUnexpectedFresh suppliedFresh
          Right
            ( Right
                ( (unchangedCausalResourcePlan predecessor)
                    { successorProjectionStoreResources =
                        Map.delete
                          candidateProvenance
                          predecessor.projectionStoreResources
                    }
                )
            )
        _ -> do
          rejectUnexpectedFresh suppliedFresh
          Right (Right (unchangedCausalResourcePlan predecessor))

unchangedCausalResourcePlan :: StructuralAppliedState -> CausalResourcePlan
unchangedCausalResourcePlan state =
  CausalResourcePlan
    { resource = Nothing,
      newlyRetainedResource = Nothing,
      successorRetainedStoreResources = state.retainedStoreResources,
      successorProjectionStoreResources = state.projectionStoreResources,
      successorUsedStoreIncarnations = state.usedStoreIncarnations
    }

retainedResourceCreation ::
  Maybe DynamicLocalStoreSpec ->
  Map StoreIncarnationId (ExactChange DynamicLocalStoreSpec)
retainedResourceCreation = \case
  Nothing -> Map.empty
  Just store -> Map.singleton store.incarnation (ExactChange Nothing (Just store))

data LocalDestinationPlan = LocalDestinationPlan
  { change :: Map DeltaId (ExactChange DynamicLocalStoreSpec),
    successorStores :: Map DeltaId DynamicLocalStoreSpec
  }

prepareLocalDestination ::
  Bool ->
  StructuralAppliedState ->
  Maybe DynamicLocalStoreSpec ->
  CheckedStructuralMeaning ->
  LocalDestinationPlan
prepareLocalDestination currentWins predecessor causalResource meaning
  | not currentWins = unchangedLocalDestinationPlan predecessor
  | otherwise = case meaning of
      CheckedDelta _ delta _ _ (ActiveRootHere _) ->
        case causalResource of
          Nothing -> error "active current delta lacks retained resource"
          Just destination ->
            let current = Map.lookup delta predecessor.localStores
             in if current == Just destination
                  then unchangedLocalDestinationPlan predecessor
                  else
                    LocalDestinationPlan
                      (Map.singleton delta (ExactChange current (Just destination)))
                      (Map.insert delta destination predecessor.localStores)
      CheckedDelta _ delta _ _ PassiveRoot -> closeCurrent delta
      CheckedDelta _ delta _ _ (RemoteController _ _) -> closeCurrent delta
      _ -> unchangedLocalDestinationPlan predecessor
  where
    closeCurrent delta =
      let current = Map.lookup delta predecessor.localStores
       in let changes = maybe Map.empty (const (Map.singleton delta (ExactChange current Nothing))) current
              successorStores = Map.delete delta predecessor.localStores
           in LocalDestinationPlan changes successorStores

unchangedLocalDestinationPlan :: StructuralAppliedState -> LocalDestinationPlan
unchangedLocalDestinationPlan state =
  LocalDestinationPlan Map.empty state.localStores

destinationLogicalKey ::
  DynamicLocalStoreSpec ->
  (ProcessEpochId, GlobalObjectId, HeraldEpoch, DeltaId, SortOccurrence)
destinationLogicalKey store =
  (store.process, store.controller, store.herald, store.delta, store.sort)

rejectUnexpectedFresh ::
  Maybe FreshStoreIncarnationEvidence ->
  Either StructuralReconciliationProblem ()
rejectUnexpectedFresh supplied = case supplied of
  Nothing -> Right ()
  Just evidence ->
    Left
      ( StructuralFreshIncarnationUnexpected
          (freshStoreIncarnationDelta evidence)
      )

data ProjectedEntry = ProjectedEntry AppliedEntry CheckedStructuralMeaning

projectedEntry :: StructuralAppliedState -> AppliedEntry -> ProjectedEntry
projectedEntry state entry =
  ProjectedEntry
    entry
    ( case Map.lookup (entryProvenance entry) state.projectionMeanings of
        Just meaning -> meaning
        Nothing -> error "applied structural entry lacks its projection"
    )

effectiveProjectedEntry ::
  StructuralAppliedState -> AppliedEntry -> Maybe ProjectedEntry
effectiveProjectedEntry state entry =
  case entryOccurrence entry >>= (`Map.lookup` state.retirementSuppressions) of
    Just _ -> Nothing
    Nothing ->
      case Map.lookup (entryObject entry) state.controlOverlays of
        Nothing -> Just (projectedEntry state entry)
        Just (StructuralDeletedOverlay _) -> Nothing
        Just (StructuralControllerOverlay _ controller activity) ->
          Just
            ( ProjectedEntry
                entry
                ( reviseMeaningController
                    controller
                    activity
                    (case projectedEntry state entry of ProjectedEntry _ meaning -> meaning)
                )
            )

reviseMeaningController ::
  ControllerProjection ->
  RootActivityProjection ->
  CheckedStructuralMeaning ->
  CheckedStructuralMeaning
reviseMeaningController controller activity meaning = case meaning of
  CheckedNabla object nabla typedSort _ _ ->
    CheckedNabla object nabla typedSort controller activity
  CheckedDelta object delta typedSort _ _ ->
    CheckedDelta object delta typedSort controller activity
  other -> other

graphChangeBetween ::
  GlobalObjectId ->
  Maybe ProjectedEntry ->
  Maybe ProjectedEntry ->
  StructuralGraphPatch
graphChangeBetween object before after =
  case projectedRole before after of
    EdgeCarrier ->
      StructuralGraphPatch
        Map.empty
        ( Map.singleton
            object
            (ExactChange (before >>= entryEdgeProjection) (after >>= entryEdgeProjection))
        )
    _ ->
      StructuralGraphPatch
        ( Map.singleton
            object
            ( ExactChange
                (before >>= entryVertexProjection)
                (after >>= entryVertexProjection)
            )
        )
        Map.empty
  where
    projectedRole (Just (ProjectedEntry entry _)) _ = entryRole entry
    projectedRole Nothing (Just (ProjectedEntry entry _)) = entryRole entry
    projectedRole Nothing Nothing = error "empty structural graph change"

emptyGraphPatch :: StructuralGraphPatch
emptyGraphPatch = StructuralGraphPatch Map.empty Map.empty

entryVertex :: ProjectedEntry -> StructuralVertexProjection
entryVertex (ProjectedEntry entry meaning) = case meaning of
  CheckedNeutral object ->
    NeutralVertexProjection provenance object
  CheckedNabla _ nabla typedSort controller activity ->
    NablaVertexProjection provenance nabla typedSort controller activity
  CheckedDelta _ delta typedSort controller activity ->
    DeltaVertexProjection provenance delta typedSort controller activity
  CheckedEdge {} -> error "edge occurrence has no vertex projection"
  where
    provenance = entryProvenance entry

entryVertexProjection :: ProjectedEntry -> Maybe StructuralVertexProjection
entryVertexProjection projected@(ProjectedEntry _ meaning) = case meaning of
  CheckedEdge {} -> Nothing
  _ -> Just (entryVertex projected)

entryEdge :: ProjectedEntry -> OwnedEdgeProjection
entryEdge (ProjectedEntry entry meaning) = case meaning of
  CheckedEdge object payload ->
    OwnedEdgeProjection
      (entryProvenance entry)
      object
      payload
  _ -> error "non-edge occurrence has no edge projection"

entryEdgeProjection :: ProjectedEntry -> Maybe OwnedEdgeProjection
entryEdgeProjection projected@(ProjectedEntry _ meaning) = case meaning of
  CheckedEdge {} -> Just (entryEdge projected)
  _ -> Nothing

appliedControlledProjection :: AppliedEntry -> AppliedControlledProjection
appliedControlledProjection entry =
  AppliedControlledProjection
    { object = entryObject entry,
      provenance = entryProvenance entry,
      role = entryRole entry,
      lifecycle = maybe Current checkedPublicationLifecycle publication,
      suppression = case publication of
        Nothing -> Nothing
        Just retained ->
          case checkedPublicationLifecycle retained of
            Current -> Nothing
            Obsolete -> Just (checkedPublicationWinnerKey retained)
    }
  where
    publication = entryPublication entry

entryApplication :: AppliedEntry -> Maybe StructuralApplication
entryApplication entry = case entry.source of
  GenesisEntrySource {} -> Nothing
  BaselineEntrySource {} -> Nothing
  OccurrenceEntrySource application -> Just application

entryPublication :: AppliedEntry -> Maybe CheckedPublication
entryPublication entry = case entry.source of
  GenesisEntrySource _ _ _ publication -> Just publication
  BaselineEntrySource _ publication -> Just publication
  OccurrenceEntrySource application -> Just application.publication

entryProvenance :: AppliedEntry -> StructuralProjectionProvenance
entryProvenance entry = case entry.source of
  GenesisEntrySource object _ _ _ -> GenesisStructuralProvenance object
  BaselineEntrySource _ publication ->
    BaselineStructuralProvenance (checkedPublicationId publication)
  OccurrenceEntrySource application ->
    OccurrenceStructuralProvenance
      application.occurrence
      (checkedPublicationId application.publication)

entryOccurrence :: AppliedEntry -> Maybe StructuralOccurrenceId
entryOccurrence = structuralProjectionProvenanceOccurrence . entryProvenance

resourceForEntry ::
  StructuralAppliedState -> AppliedEntry -> Maybe DynamicLocalStoreSpec
resourceForEntry state entry =
  Map.lookup (entryProvenance entry) state.projectionStoreResources

entryObject :: AppliedEntry -> GlobalObjectId
entryObject = decodedCarrierObject . (.intrinsic)

entryRole :: AppliedEntry -> StructuralCarrierRole
entryRole = decodedCarrierRole . (.intrinsic)

entryWins :: AppliedEntry -> AppliedEntry -> Bool
entryWins candidate incumbent =
  case (entryPublication candidate, entryPublication incumbent) of
    (Nothing, Nothing) -> False
    (Just _, Nothing) -> True
    (Nothing, Just _) -> False
    (Just candidatePublication, Just incumbentPublication) ->
      checkedPublicationWinnerKey candidatePublication
        > checkedPublicationWinnerKey incumbentPublication

winnerForObject ::
  GlobalObjectId ->
  StructuralAppliedState ->
  Maybe AppliedEntry
winnerForObject object state = Map.lookup object state.currentWinners

-- Only unsuppressed admissions enter this index. A later control overlay can
-- hide or revise a winner but cannot change which immutable carrier wins.
-- Historical cuts continue to use the independent history fold below.
insertCurrentWinner ::
  AppliedEntry -> Map GlobalObjectId AppliedEntry -> Map GlobalObjectId AppliedEntry
insertCurrentWinner candidate = Map.insertWith choose (entryObject candidate) candidate
  where
    choose incoming retained
      | entryWins incoming retained = incoming
      | otherwise = retained

winnerForObjectAt ::
  Maybe StructuralVersionVector ->
  GlobalObjectId ->
  StructuralAppliedState ->
  Maybe AppliedEntry
winnerForObjectAt frontier object state =
  foldl'
    choose
    Nothing
    (filter ((== object) . entryObject) (projectableEntries frontier state))
  where
    choose Nothing candidate = Just candidate
    choose incumbent@(Just retained) candidate
      | entryWins candidate retained = Just candidate
      | otherwise = incumbent

winningEntries :: StructuralAppliedState -> Map GlobalObjectId AppliedEntry
winningEntries state = state.currentWinners

winningEntriesAt ::
  Maybe StructuralVersionVector ->
  StructuralAppliedState ->
  Map GlobalObjectId AppliedEntry
winningEntriesAt frontier state = winningEntriesFrom (projectableEntries frontier state)

winningEntriesFrom :: [AppliedEntry] -> Map GlobalObjectId AppliedEntry
winningEntriesFrom entries =
  Map.fromListWith choose [(entryObject entry, entry) | entry <- entries]
  where
    choose left right
      | entryWins left right = left
      | otherwise = right

eligibleEntries ::
  Maybe StructuralVersionVector ->
  StructuralAppliedState ->
  [AppliedEntry]
eligibleEntries frontier state =
  Map.elems state.baselineWinners
    <> [ entry
       | (occurrence, entry) <- Map.toAscList state.history,
         maybe True (`coversOccurrence` occurrence) frontier
       ]

projectableEntries ::
  Maybe StructuralVersionVector ->
  StructuralAppliedState ->
  [AppliedEntry]
projectableEntries frontier state =
  Map.elems state.baselineWinners
    <> [ entry
       | (occurrence, entry) <- Map.toAscList state.history,
         Map.notMember occurrence state.retirementSuppressions,
         maybe True (`coversOccurrence` occurrence) frontier
       ]

coversOccurrence ::
  StructuralVersionVector ->
  StructuralOccurrenceId ->
  Bool
coversOccurrence vector occurrence =
  maybe
    False
    (>= structuralPrefixThrough (structuralOccurrenceSourceSequence occurrence))
    ( structuralVersionVectorComponent
        (structuralOccurrenceSourceHeraldEpoch occurrence)
        vector
    )

satisfiedBy :: CheckedStructuralMeaning -> Set StructuralDependency
satisfiedBy meaning = case meaning of
  CheckedNeutral object -> Set.singleton (StructuralEndpointDependency object)
  CheckedNabla object _ _ _ _ ->
    Set.singleton (StructuralEndpointDependency object)
  CheckedDelta object _ _ _ _ ->
    Set.singleton (StructuralEndpointDependency object)
  CheckedEdge {} -> Set.empty

prepareDebts ::
  ReconciliationViews ->
  StructuralConsequenceCause ->
  Maybe CheckedStructuralMeaning ->
  Maybe CheckedStructuralMeaning ->
  Map StoreIncarnationId DynamicLocalStoreSpec ->
  Map DeltaId DynamicLocalStoreSpec ->
  Map DeltaId (ExactChange DynamicLocalStoreSpec) ->
  Maybe DynamicLocalStoreSpec ->
  Maybe DynamicLocalStoreSpec ->
  Maybe DynamicLocalStoreSpec ->
  StructuralDebtSet
prepareDebts
  views
  cause
  incumbent
  successor
  predecessorResources
  successorStores
  storeChanges
  causalResource
  newlyRetainedResource
  releasedResource =
    normalizeStructuralDebts
      ( topologyDebts
          <> destinationDebts
          <> transitionDebts
          <> resourceDebts
          <> releasedResourceDebts
      )
    where
      affectedSorts = sortsAffected views incumbent successor
      identities = identitiesAffected incumbent successor
      evidenceFor sort =
        structuralDebtEvidence
          identities
          ( Set.fromList
              [ placementFact store
              | store <- Map.elems successorStores,
                store.sort == sort
              ]
          )
          ( Set.fromList
              [ qualifiedStore store
              | store <- Map.elems predecessorResources,
                store.sort == sort
              ]
          )
      topologyDebts =
        [ debt kind sort Nothing (evidenceFor sort)
        | sort <- Set.toAscList affectedSorts,
          kind <- [TopologyAlignmentDebt, CutoverDebt]
        ]
      destinationDebts =
        [ debt
            DestinationAlignmentDebt
            sort
            (Just destination)
            (evidenceFor sort)
        | sort <- Set.toAscList affectedSorts,
          destination <-
            Set.toAscList
              ( Set.fromList
                  [ qualifiedStore store
                  | store <- Map.elems successorStores <> maybe [] pure causalResource,
                    store.sort == sort
                  ]
              )
        ]
      transitionDebts = concatMap transitionDebt (Map.elems storeChanges)
      transitionDebt change =
        let old = exactChangeBefore change
            new = exactChangeAfter change
         in catMaybes
              [ fmap
                  ( \store ->
                      debt
                        StoreReplacementDebt
                        store.sort
                        (Just (qualifiedStore store))
                        (evidenceFor store.sort)
                  )
                  old,
                fmap
                  ( \store ->
                      debt
                        PlacementActivationDebt
                        store.sort
                        (Just (qualifiedStore store))
                        (evidenceFor store.sort)
                  )
                  new
              ]
      resourceDebts =
        maybe
          []
          ( \store ->
              [ debt
                  StoreActivationDebt
                  store.sort
                  (Just (qualifiedStore store))
                  (evidenceFor store.sort)
              ]
          )
          newlyRetainedResource
      releasedResourceDebts =
        maybe
          []
          ( \store ->
              [ debt
                  StoreReplacementDebt
                  store.sort
                  (Just (qualifiedStore store))
                  (evidenceFor store.sort)
              ]
          )
          releasedResource
      debt kind sort destination evidenceValue =
        structuralConsequenceDebt
          (structuralDebtKey cause kind sort destination)
          evidenceValue

sortsAffected ::
  ReconciliationViews ->
  Maybe CheckedStructuralMeaning ->
  Maybe CheckedStructuralMeaning ->
  Set SortOccurrence
sortsAffected views incumbent successor = case successor of
  Just CheckedNeutral {} -> allSorts
  Just CheckedEdge {} -> allSorts
  Just (CheckedNabla _ _ typedSort _ _) ->
    Set.fromList (typedSort : maybe [] meaningSorts incumbent)
  Just (CheckedDelta _ _ typedSort _ _) ->
    Set.fromList (typedSort : maybe [] meaningSorts incumbent)
  Nothing -> case incumbent of
    Just CheckedNeutral {} -> allSorts
    Just CheckedEdge {} -> allSorts
    Just meaning -> Set.fromList (meaningSorts meaning)
    Nothing -> Set.empty
  where
    allSorts = Set.fromList (Map.elems views.effectiveSorts)

meaningSorts :: CheckedStructuralMeaning -> [SortOccurrence]
meaningSorts = \case
  CheckedNabla _ _ sort _ _ -> [sort]
  CheckedDelta _ _ sort _ _ -> [sort]
  _ -> []

identitiesAffected ::
  Maybe CheckedStructuralMeaning ->
  Maybe CheckedStructuralMeaning ->
  Set StructuralIdentity
identitiesAffected incumbent successor =
  Set.fromList
    ( maybe [] meaningIdentities successor
        <> maybe [] meaningIdentities incumbent
    )

meaningIdentities :: CheckedStructuralMeaning -> [StructuralIdentity]
meaningIdentities = \case
  CheckedNeutral object -> [StructuralVertexIdentity (NeutralVertex object)]
  CheckedNabla _ nabla _ _ _ -> [StructuralVertexIdentity (NablaVertex nabla)]
  CheckedDelta _ delta _ _ _ -> [StructuralVertexIdentity (DeltaVertex delta)]
  CheckedEdge object payload ->
    [ StructuralEdgeIdentity object,
      StructuralVertexIdentity (edgeSource payload),
      StructuralVertexIdentity (edgeDestination payload)
    ]

qualifiedStore :: DynamicLocalStoreSpec -> QualifiedStoreIncarnation
qualifiedStore store =
  qualifiedStoreIncarnation
    store.herald
    store.delta
    store.sort
    store.incarnation

placementFact :: DynamicLocalStoreSpec -> QualifiedPlacementFact
placementFact store =
  qualifiedPlacementFact
    store.herald
    store.delta
    store.sort
    store.incarnation

carrierRoleForSort :: SortId -> Maybe StructuralCarrierRole
carrierRoleForSort sortId
  | sortId == predefinedCatalogueSortId (profileEntryFor NeutralVertexRole) =
      Just NeutralVertexCarrier
  | sortId == predefinedCatalogueSortId (profileEntryFor EdgeRole) = Just EdgeCarrier
  | sortId == predefinedCatalogueSortId (profileEntryFor NablaRole) = Just NablaCarrier
  | sortId == predefinedCatalogueSortId (profileEntryFor DeltaRole) = Just DeltaCarrier
  | sortId == predefinedCatalogueSortId (profileEntryFor ProcessEpochRole) =
      Just ProcessEpochCarrier
  | otherwise = Nothing

objectIdField, labelField, sortIdField :: FieldName
objectIdField = checkedField "object_id"
labelField = checkedField "label"
sortIdField = checkedField "sort_id"

edgeSourceField, edgeDestinationField, edgeStrengthField :: FieldName
edgeSourceField = checkedField "source_vertex"
edgeDestinationField = checkedField "destination_vertex"
edgeStrengthField = checkedField "strength"

checkedField :: Text -> FieldName
checkedField name = case mkFieldName name of
  Right field -> field
  Left _ -> error "invalid closed structural carrier field name"
