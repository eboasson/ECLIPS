{-# LANGUAGE OverloadedRecordDot #-}

-- | Whole-state validation for the composed Herald.
--
-- This validator diagnoses contradictions between independently owned pure
-- projections.  Its finite public categories intentionally discard leaf error
-- payloads: an invariant fault stops the Herald role and is not a recovery or
-- protocol outcome.
module Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (..),
    BootstrapInvariantCategory (..),
    StartupInvariantSubject (..),
    StartupInvariantViolation (..),
    HeraldTransitionInvariantViolation (..),
    bootstrapInvariantFault,
    validateHeraldState,
  )
where

import Control.Monad (unless)
import Data.List (find, sort, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing, maybeToList)
import Data.Set qualified as Set
import Eclips.Application.Types.Access
  ( ApplicationStartupAccess,
    allApplicationPredefinedSortRoles,
    environmentAccessEdges,
    environmentAccessHub,
    environmentAccessPredefined,
    predefinedAccessRole,
    predefinedReader,
    predefinedWriter,
  )
import Eclips.Application.Types.Access qualified as StartupAccess
import Eclips.Application.Types.Forward (ForwardResult (ForwardAccepted))
import Eclips.Application.Types.Identity
  ( PrivateNablaId,
    PrivateUniqueId,
    privateDeltaUniqueId,
    privateNablaUniqueId,
    privateObjectUniqueId,
    privateProcessUniqueId,
    privateUniqueIdWord64,
  )
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Application.Types.NewId (NewIdTarget (..))
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Result
  ( OperationPendingReason (EnvironmentStabilizationPending, LabelSettlementPending),
    RegularCallResult (..),
  )
import Eclips.Application.Types.Value (ApplicationValue (SortDefinitionValue))
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (..),
    WriteResult (..),
  )
import Eclips.Domain.Alignment qualified as DomainAlignment
import Eclips.Domain.Disappearance
  ( DisappearanceSubjectView (..),
    deriveCanonicalDescriptorDigest,
    disappearanceResolutionControlIndex,
    disappearanceResolutionSubject,
    disappearanceSubjectView,
  )
import Eclips.Domain.Environment (EnvironmentRootClaimView (..), environmentRootClaimView, environmentRootSlotClaim, environmentRootSlotStructuralCarrierRole)
import Eclips.Domain.Graph
  ( VertexId (..),
    edgeDestination,
    edgeSource,
  )
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    ControlIndex,
    DeltaId,
    GlobalObjectId,
    GlobalUniqueId,
    LabelDecisionId,
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
    StructuralOccurrenceId,
    controlIndex,
    deltaIdFromGlobalObjectId,
    globalObjectIdFromDeltaId,
    globalObjectIdFromGlobalUniqueId,
    globalObjectIdFromNablaId,
    globalObjectIdFromProcessEpochId,
    globalUniqueIdFromGlobalObjectId,
    nablaIdFromGlobalObjectId,
    nablaSequenceWord64,
    processEpochIdFromGlobalObjectId,
    publicationAuthorityEpoch,
    publicationNabla,
    publicationNablaSequence,
    publicationSourceHeraldEpoch,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
  )
import Eclips.Domain.Identity qualified as DomainIdentity
import Eclips.Domain.Label
  ( homeLabelAcceptanceCutProcessPosition,
    labelRecordReleasedState,
    labelRecordRevision,
    labelRevisionControlIndex,
    preparedLabelFactsObjectId,
    releasedLabelStateIsDeleted,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationChangeControlIndex,
    heraldMembershipGenerationId,
    heraldMembershipGenerationPredecessor,
    heraldMembershipGenerationRetirementControlIndex,
    heraldMembershipHistoryGenesis,
    heraldMembershipLineageOrigin,
    heraldMembershipLineageTarget,
  )
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (ApplicationPermanentlyLost, ExplicitAdministrativeEnd),
  )
import Eclips.Domain.ProcessStart (processStartProcessEpochId, processStartProcessId, processStartResidence)
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationCanonicalValue,
    checkedPublicationId,
    checkedPublicationKey,
    checkedPublicationLifecycle,
    checkedPublicationLogicalState,
    checkedPublicationSort,
    checkedPublicationValue,
    mkCheckedPublication,
  )
import Eclips.Domain.Route
  ( FrozenRoute,
    ReplicaStrength (..),
    attenuateFrozenRoute,
    destinationDelta,
    destinationHerald,
    destinationStoreIncarnation,
    destinationStrength,
    freezeRoute,
    routeDestination,
    routeDestinations,
  )
import Eclips.Domain.Sort.Canonical
  ( canonicalCheckedDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( Lifecycle (Current),
    ObjectKey,
    SortKind (ControlledSort),
    StructuralCarrierRole (..),
    controlledObjectKey,
    descriptorKind,
    descriptorStructuralCarrierRole,
  )
import Eclips.Domain.Sort.Profile
  ( decodeSortDefinitionValue,
    profileSortFor,
    structuralCarrierSortReferences,
  )
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
    resolvedRetirementOccurrenceBase,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    AppliedRoot,
    AppliedRootRole (..),
    HeraldMember (..),
    PredefinedSortRole (DeltaRole, EdgeRole, NablaRole, NeutralVertexRole, ProcessEpochRole, SortDefinitionRole),
    allPredefinedSortRoles,
    appliedBootstrapManifestId,
    appliedProcessAuthority,
    appliedProcessEnvironmentEdges,
    appliedProcessEnvironmentHub,
    appliedProcessEnvironmentPublications,
    appliedProcessEpochId,
    appliedProcessId,
    appliedProcessResidence,
    appliedProcessRoots,
    appliedRootAuthority,
    appliedRootCatalogueRole,
    appliedRootControlPrerequisite,
    appliedRootObjectId,
    appliedRootOccurrenceId,
    appliedRootPlacement,
    appliedRootRole,
    appliedRootSortId,
    checkedInitialTopologyEdges,
    checkedInitialTopologyVertices,
    deriveInitialProjectionDigest,
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
    materializeConfiguredProcessBootstrap,
    primordialReplicaDescriptor,
    primordialReplicaOccurrenceId,
    primordialReplicaPublication,
    primordialReplicaPublicationId,
    primordialReplicaRole,
    primordialReplicaSortId,
  )
import Eclips.Domain.Store
  ( deltaStoreSort,
    logicalStateStrength,
    lookupRetained,
    lookupVisible,
    retainedInstances,
    storedPublication,
    storedStrength,
    visibleInstances,
  )
import Eclips.Domain.Structural
  ( structuralPrefixSequence,
    structuralVersionVectorComponent,
    structuralVersionVectorMembershipGenerationId,
  )
import Eclips.Domain.StructuralConsequence qualified as StructuralConsequence
import Eclips.Domain.Topology
  ( deriveGenesisTopologyCutId,
    deriveTopologyCutId,
    topologyCutFrontier,
    topologyFrontierAppliedControlPrefix,
  )
import Eclips.Domain.Value
  ( LabelOwner (ProcessLabel),
    canonicalValueByteString,
    canonicalValueBytes,
    decodeCanonicalValue,
  )
import Eclips.Herald.Administration
  ( checkedInitialAdministrationBinding,
    drainIdWord64,
  )
import Eclips.Herald.Administration.Internal qualified as AdministrationInternal
import Eclips.Herald.Administration.State qualified as Administration
import Eclips.Herald.Alignment.Generation qualified as AlignmentGeneration
import Eclips.Herald.Alignment.Plan qualified as AlignmentPlan
import Eclips.Herald.Alignment.Protocol
  ( alignmentRouteCutoverMarkerGeneration,
    alignmentRouteCutoverMarkerPlan,
    alignmentRouteCutoverMarkerPredecessorHerald,
    alignmentRouteCutoverMarkerSourceHerald,
  )
import Eclips.Herald.Alignment.Protocol qualified as AlignmentProtocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as AlignmentTransfer
import Eclips.Herald.Application.Environment qualified as Environment
import Eclips.Herald.Application.Primordial qualified as Primordial
import Eclips.Herald.Application.Primordial.Transfer qualified as StartupTransfer
import Eclips.Herald.Application.PrivateIdentity
  ( ProcessIdentityWitness,
    witnessedBindings,
    witnessedNextPrivateUniqueId,
    witnessedProcessEpoch,
    witnessedReverseBindings,
  )
import Eclips.Herald.Application.Publication qualified as ApplicationPublication
import Eclips.Herald.Application.Request.Internal qualified as ApplicationRequest
import Eclips.Herald.Application.Session.Internal qualified as ApplicationSession
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Authority qualified as Authority
import Eclips.Herald.Bootstrap (BootstrapInvariant (..))
import Eclips.Herald.Bootstrap qualified as Bootstrap
import Eclips.Herald.ConfiguredProcess.State qualified as Configured
import Eclips.Herald.Controlled.Operate qualified as ControlledOperate
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Disappearance.Evidence qualified as DisappearanceEvidence
import Eclips.Herald.Disappearance.Gate qualified as DisappearanceGate
import Eclips.Herald.Disappearance.OwnerEvidence qualified as OwnerEvidence
import Eclips.Herald.Disappearance.Protocol
  ( matchingPublicationPosition,
  )
import Eclips.Herald.Disappearance.Protocol qualified as DisappearanceProtocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.Discovery
  ( peerBindingGeneration,
    peerBindingGenerationWord64,
    peerBindingRemoteHeraldEpoch,
  )
import Eclips.Herald.Discovery.Internal (DiscoveryStatic (..))
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.FailureDetection.State qualified as FailureDetection
import Eclips.Herald.Genesis.Internal
  ( checkedActiveHeraldEpochs,
    checkedActiveHeralds,
    checkedCatalogueDigest,
    checkedConfigurationDigest,
    checkedInitialProjectionDigest,
    checkedInitialTopologyProjection,
    checkedLocalHeraldEpoch,
    checkedLocalHeraldId,
    checkedOracleControlIndex,
    checkedPredefinedOccurrenceSet,
    checkedPrimordialReplicas,
    checkedStartupAdmission,
    checkedSystemId,
    genesisAuthorityEpoch,
    lookupConfiguredProcessBootstrap,
    lookupSystemBootstrapWriter,
    systemBootstrapWriterAuthority,
    systemBootstrapWriterNablaId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource qualified as TerminalSource
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.Join.State qualified as Join
import Eclips.Herald.LabelBarrier.State qualified as LabelBarrier
import Eclips.Herald.OracleClient (oracleHelloClaims)
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerLiveness.State qualified as PeerLiveness
import Eclips.Herald.PeerPayload
  ( PeerLogicalPayload (..),
    peerLogicalPayloadDigest,
  )
import Eclips.Herald.PeerPublication
  ( PeerPublicationKind (StructuralPublicationKind),
    StructuralOccurrenceStamp,
    authenticatePeerPublication,
    decodeStructuralPublicationCanonicalBytes,
    peerPublicationBatch,
    peerPublicationDigest,
    peerPublicationKind,
    peerPublicationStructuralStamp,
    publicationBatchCanonicalValue,
    publicationBatchControlPrerequisite,
    publicationBatchDestinations,
    publicationBatchHasFenceDependency,
    publicationBatchId,
    publicationBatchOccurrenceId,
    publicationBatchSortId,
    publicationBatchSourceProcess,
    publicationBatchSourceStrength,
    publicationBatchSourceTopologyPrerequisite,
    publicationDestinationDelta,
    publicationDestinationStoreIncarnation,
    publicationDestinationStrength,
    structuralOccurrenceStampCarrierRole,
    structuralOccurrenceStampOccurrence,
    structuralOccurrenceStampPredecessor,
    structuralOccurrenceStampPublication,
    structuralOccurrenceStampPublicationDigest,
    structuralPublicationDigestForSemantics,
    structuralPublicationSemanticsCanonicalValue,
    structuralPublicationSemanticsCarrierRole,
    structuralPublicationSemanticsControlPrerequisite,
    structuralPublicationSemanticsOccurrenceId,
    structuralPublicationSemanticsPublicationId,
    structuralPublicationSemanticsSortId,
    structuralPublicationSemanticsSourceProcess,
    structuralPublicationSemanticsSourceStrength,
    structuralPublicationSemanticsSourceTopologyPrerequisite,
  )
import Eclips.Herald.PeerStream
  ( assignmentReceipt,
    assignmentReceiptDigest,
    assignmentReceiptDirection,
    assignmentReceiptSequence,
    peerDispatchBindingGenerationWord64,
    sequencedItemDigest,
    sequencedItemDirection,
    sequencedItemPayload,
    sequencedItemSequence,
    streamDirectionDestination,
    streamDirectionSource,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement qualified as PlacementRoute
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.ProcessPreparation.Protocol qualified as PreparationProtocol
import Eclips.Herald.ProcessPreparation.State qualified as ProcessPreparation
import Eclips.Herald.Publication.Disappearance qualified as PublicationDisappearance
import Eclips.Herald.Publication.Groups qualified as PublicationGroups
import Eclips.Herald.Publication.Route qualified as PublicationRoute
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.State
  ( HeraldPhase (..),
    HeraldState,
    StartupStateWitness (..),
    heraldDrainWitnessId,
    heraldDrainWitnessRequest,
    startupAdministrationState,
    startupAlignmentState,
    startupApplicationState,
    startupControlOracleProjectionState,
    startupControlledState,
    startupDisappearanceState,
    startupDiscoveryState,
    startupFailureDetectionState,
    startupGenesis,
    startupGraphState,
    startupInitialBootstraps,
    startupIsolationState,
    startupJoinState,
    startupLabelBarrierState,
    startupLabelPatchState,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPeerLivenessState,
    startupPeerStreamState,
    startupPlacementState,
    startupProcessPreparationState,
    startupPublicationState,
    startupSemanticControlIsFrozen,
    startupSortRegistryState,
    startupStateWitness,
    startupStoreState,
    startupStructuralBaseCoordinator,
    startupStructuralProgressState,
    startupTerminalSourceHoldState,
    startupVisibilityState,
    startupWaitState,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt qualified as StructuralDebt
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.TerminalSourceHold.State qualified as TerminalSourceHold
import Eclips.Herald.UseCase.Alignment qualified as AlignmentCoordinator
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransferCoordinator
import Eclips.Herald.UseCase.LabelPatch qualified as LabelPatch
import Eclips.Herald.UseCase.PeerInput qualified as PeerInput
import Eclips.Herald.UseCase.PeerPlacement (remotePlacementRouteIsValid)
import Eclips.Herald.UseCase.Step15StructuralBase qualified as StructuralBase
import Eclips.Herald.UseCase.StructuralSettlement qualified as StructuralSettlement
import Eclips.Herald.Visibility.State qualified as Visibility
import Eclips.Herald.Wait.State qualified as Wait
import Eclips.Oracle.Admission qualified as Admission
import Eclips.Oracle.Canonical
  ( canonicalAppliedOracleEntryBytes,
    canonicalAppliedOracleEntryValue,
    canonicalizeOracleReceipt,
  )
import Eclips.Oracle.Genesis (raftVoterBindingHeraldEpoch)
import Eclips.Oracle.Label qualified as OracleLabel
import Eclips.Oracle.Projection
  ( OracleProjectionEventView (..),
    ProcessOriginView (DynamicStartView),
    appliedEntryCommand,
    appliedEntryControlIndex,
    appliedEntryProjectionEvents,
    appliedEntryReceipt,
    oracleProjectionEventView,
    processRecordOrigin,
    processRecordProcessEpoch,
  )
import Eclips.Oracle.Receipt
  ( OracleReceiptResult (OracleAccepted, OracleRejected),
    oracleReceiptResult,
  )
import Eclips.Oracle.Voter qualified as Voter

-- | Redacted origin of a contradiction observed while composing one already
-- checked bootstrap.
data BootstrapInvariantCategory
  = BootstrapOracleProjectionInvariant
  | BootstrapResidenceInvariant
  | BootstrapSortMissingInvariant
  | BootstrapSortIdentityInvariant
  | BootstrapOccurrenceInvariant
  | BootstrapControlPrerequisiteInvariant
  | BootstrapApplicationInvariant
  | BootstrapControlledInvariant
  | BootstrapGraphInvariant
  | BootstrapPlacementInvariant
  | BootstrapStoreOwnerInvariant
  | BootstrapStoreContentsInvariant
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Finite typed context for one startup contradiction. Nominal IDs are retained
-- where they identify the affected checked object without retaining state or
-- application data.
data StartupInvariantSubject
  = StartupStatic
  | StartupProcess ProcessEpochId
  | StartupRoot ProcessEpochId PredefinedSortRole
  | StartupSort PredefinedSortRole
  | StartupDelta DeltaId
  deriving stock (Eq, Ord, Show)

-- | Finite cross-owner relationship category.  It is deliberately not a copy
-- of any leaf's internal error vocabulary.
data StartupInvariantViolation
  = OracleControlIndexInvariant
  | OracleClientOwnerInvariant
  | OracleClientClaimsInvariant
  | OracleClientCursorInvariant
  | OracleClientEvidenceInvariant
  | OracleProjectionOwnerInvariant
  | OracleSystemIdentityInvariant
  | OracleLocalIdentityInvariant
  | OracleMembershipInvariant
  | OracleAppliedProcessInvariant
  | OracleInitialProjectionDigestInvariant
  | JoinControlTailInvariant
  | IdGeneratorInitializationInvariant
  | PrimordialPublicationInvariant
  | SortRegistrySetInvariant
  | SortRegistryDescriptorInvariant
  | SortRegistryIdentityInvariant
  | ResidentApplicationSetInvariant
  | ResidentControlledSetInvariant
  | ResidentRootSetInvariant
  | GraphVertexSetInvariant
  | GraphEdgeSetInvariant
  | StructuralProgressInvariant
  | AlignmentStateInvariant
  | AlignmentTopologyRelationshipInvariant
  | AlignmentPlacementRelationshipInvariant
  | AlignmentStoreRelationshipInvariant
  | AlignmentTransferRelationshipInvariant
  | DiscoveryOwnerInvariant
  | PeerLivenessOwnerInvariant
  | DisappearanceOwnerInvariant
  | FailureDetectionOwnerInvariant
  | IsolationOwnerInvariant
  | TerminalSourceHoldOwnerInvariant
  | PeerStreamOwnerInvariant
  | PlacementSetInvariant
  | StoreSetInvariant
  | SystemViewStoreSetInvariant
  | SystemViewGraphInvariant
  | SystemViewPlacementInvariant
  | ApplicationAccessInvariant
  | ApplicationAttachmentInvariant
  | ApplicationSessionSetInvariant
  | ApplicationSessionAccessInvariant
  | ApplicationSessionRetryIndexInvariant
  | ApplicationSessionIdentityInvariant
  | ApplicationSessionAllocationInvariant
  | ApplicationSessionRecoveryInvariant
  | ApplicationProcessRecoveryInvariant
  | ApplicationRequestOwnerInvariant
  | ApplicationRequestIdentityInvariant
  | ApplicationRequestProcessInvariant
  | ApplicationRequestStatusInvariant
  | ApplicationRequestPositionInvariant
  | ApplicationDetachedRequestInvariant
  | ApplicationFenceHeldInvariant
  | ApplicationActiveLabelInvariant
  | ApplicationReplyCursorInvariant
  | ApplicationAcceptanceAllocationInvariant
  | ApplicationGeneratedIdentityInvariant
  | EnvironmentOwnerInvariant
  | ControlledReservationInvariant
  | ConfiguredScaffoldInvariant
  | ConfiguredStartAdministrationInvariant
  | ConfiguredStartOracleRequestInvariant
  | ConfiguredStartOracleProjectionInvariant
  | ConfiguredStartReadyInvariant
  | ProcessPreparationOwnerInvariant
  | ApplicationWaitRegistrationInvariant
  | PublicationOwnerInvariant
  | PublicationAllocationInvariant
  | PublicationRecordInvariant
  | PublicationRouteReceiptInvariant
  | PublicationRemoteAssignmentInvariant
  | PublicationIncomingInvariant
  | DynamicSortRegistryInvariant
  | PrivateIdentityBijectionInvariant
  | PrivateIdentityAllocationInvariant
  | ControlledProcessInvariant
  | ControlledRootInvariant
  | ControlledTerminalDeletionInvariant
  | NormalPossessionInvariant
  | ControlledStorePossessionInvariant
  | PlacementFactInvariant
  | StoreFactInvariant
  | StoreContentsInvariant
  | StoreHistoryInvariant
  | LabelBarrierOwnerInvariant
  | LabelImportedBaseInvariant
  | LabelOracleRequestInvariant
  | VisibilityOwnerInvariant
  | SystemViewStoreFactInvariant
  | SystemViewStoreContentsInvariant
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data HeraldInvariantFault
  = HeraldBootstrapInvariant
      BootstrapManifestId
      BootstrapInvariantCategory
  | HeraldStartupInvariant
      StartupInvariantSubject
      StartupInvariantViolation
  | HeraldTransitionInvariant HeraldTransitionInvariantViolation
  deriving stock (Eq, Show)

-- | Closed contradictions diagnosed by the current transition coordinator.
data HeraldTransitionInvariantViolation
  = HeraldObservationTimeRegressed
  | HeraldTimerObservationContradiction
  | HeraldAdministrationTransitionContradiction
  | HeraldPhaseTransitionContradiction
  | HeraldDrainOwnerContradiction
  | HeraldControlledTransitionContradiction
  | HeraldApplicationTransitionContradiction
  | HeraldProcessPreparationTransitionContradiction
  | HeraldPeerTransitionContradiction
  | HeraldOracleTransitionContradiction
  deriving stock (Eq, Ord, Show, Enum, Bounded)

bootstrapInvariantFault ::
  BootstrapManifestId ->
  BootstrapInvariant ->
  HeraldInvariantFault
bootstrapInvariantFault manifest problem =
  HeraldBootstrapInvariant manifest $ case problem of
    BootstrapOracleProjectionContradiction _ -> BootstrapOracleProjectionInvariant
    BootstrapResidenceContradiction {} -> BootstrapResidenceInvariant
    BootstrapSortMissing _ -> BootstrapSortMissingInvariant
    BootstrapSortContradiction _ -> BootstrapSortIdentityInvariant
    BootstrapOccurrenceContradiction _ -> BootstrapOccurrenceInvariant
    BootstrapControlPrerequisiteContradiction _ ->
      BootstrapControlPrerequisiteInvariant
    BootstrapApplicationContradiction _ -> BootstrapApplicationInvariant
    BootstrapControlledContradiction _ -> BootstrapControlledInvariant
    BootstrapControlledObservationContradiction _ -> BootstrapControlledInvariant
    BootstrapGraphContradiction _ -> BootstrapGraphInvariant
    BootstrapPlacementContradiction _ -> BootstrapPlacementInvariant
    BootstrapStoreContradiction _ -> BootstrapStoreOwnerInvariant
    Eclips.Herald.Bootstrap.BootstrapStoreInvariant _ ->
      BootstrapStoreContentsInvariant
    BootstrapStoreObservationContradiction _ -> BootstrapStoreContentsInvariant
    BootstrapStoreSeedContradiction _ -> BootstrapStoreContentsInvariant

-- | Exhaustively verify the closed, cross-owner Herald invariant.
--
-- Initialization and explicit test/debug boundaries use this composition
-- check. Ordinary transitions instead preserve the invariant through their
-- owner-local admission and preparation checks, avoiding a full state replay
-- for every input.
validateHeraldState :: HeraldState -> Either HeraldInvariantFault ()
validateHeraldState state = do
  validateHeraldPhase state
  allApplied <- validateOracleProjection state
  validateOracleClient state
  validateJoinControlTails state
  require
    StartupStatic
    DisappearanceOwnerInvariant
    (case Disappearance.validateState (startupDisappearanceState state) of Right () -> True; Left _ -> False)
  validateSortRegistry state
  validateSystemViewStores state
  validateAlignmentOwner state
  let localEpoch = checkedLocalHeraldEpoch genesis
      oracleView = OracleProjection.oracleView (startupOracleProjectionState state)
      isolationWitness = Isolation.stateWitness (startupIsolationState state)
      isolationPhase = Isolation.isolationWitnessPhase isolationWitness
      isolationOverlay =
        Isolation.isolationWitnessResidentProcessOverlay isolationWitness
      resident =
        [ bootstrap
        | bootstrap <- allApplied,
          appliedProcessResidence bootstrap == localEpoch,
          OracleProjection.oracleViewProcessIsLive
            (appliedProcessEpochId bootstrap)
            oracleView
            || ( isolationPhase == Isolation.IsolationReadOnlyDrainView
                   && appliedProcessEpochId bootstrap `Set.member` isolationOverlay
               )
        ]
      projectedApplicationResidents = residentApplicationAliases state resident
      liveApplicationResidents =
        [ alias
        | alias@(_, process) <- projectedApplicationResidents,
          isolationPhase /= Isolation.IsolationTerminalView
            || process `Set.notMember` isolationOverlay
        ]
      liveApplicationProcesses = Set.fromList (fmap snd liveApplicationResidents)
      applicationResidents =
        liveApplicationResidents
          <> [ alias
             | alias@(_, process) <- isolationApplicationAliases state,
               process `Set.notMember` liveApplicationProcesses
             ]
      applicationResidentProcesses = Set.fromList (fmap snd applicationResidents)
  validateResidentSets state resident applicationResidents
  validateControlledStorePossessions state
  validateControlledTerminalDeletions state
  validatePeerOwners state
  validateApplicationSessions state applicationResidents
  validateApplicationRequests state applicationResidents
  validateApplicationLabelWork state
  validatePrivateEnvironments state
  validateGeneratedIdentities state
  validateWaitRegistrations state
  validateConfiguredProcesses state
  validateProcessPreparations state
  validatePublications state applicationResidents
  traverse_
    (validateResidentProcess state)
    [ bootstrap
    | bootstrap <- resident,
      appliedProcessEpochId bootstrap `Set.member` applicationResidentProcesses
    ]
  validateGraph state resident
  validatePlacementsAndStores state resident
  validateStructuralOwners state
  validateAlignmentRelationships state
  validateStoreHistory state
  validateSlice4Owners state
  where
    genesis = startupGenesis state

-- Outstanding onboarding promises retain bytes independently of the semantic
-- base. Donors keep the Begin floor; only the applicant advances its local
-- reconstruction floor after importing a complete source set.
validateJoinControlTails :: HeraldState -> Either HeraldInvariantFault ()
validateJoinControlTails state = require StartupStatic JoinControlTailInvariant valid
  where
    projection = startupOracleProjectionState state
    view = OracleProjection.oracleView projection
    local = checkedLocalHeraldEpoch (startupGenesis state)
    join = startupJoinState state
    valid =
      all validTail (Join.admissionControlTails join)
        && maybe True (isJust . (`OracleProjection.retainedControlSuffixAfter` projection)) (Join.admissionControlTailFloor join)
    validTail promise =
      case OracleProjection.oracleViewHeraldAdmission (Join.admissionControlTailAdmission promise) view of
        Nothing -> False
        Just record ->
          let applicant = Join.admissionControlTailApplicant promise
              begin = Join.admissionControlTailBegin promise
              floorIndex = Join.admissionControlTailExclusiveFloor promise
           in Admission.admissionManifestHeraldEpoch (Admission.admissionRecordManifest record) == applicant
                && Admission.admissionRecordBeginIndex record == begin
                && begin <= floorIndex
                && floorIndex <= OracleProjection.oracleViewControlIndex view
                && (local == applicant || local `elem` heraldMembershipGenerationActiveHeraldEpochs (Admission.admissionRecordPredecessor record))
                && (local == applicant || floorIndex == begin)

-- The imported cut remains fixed when local replay origins advance. Below it,
-- canonical release facts authenticate historical structural causes without
-- claiming that this receiver performed the donor's local patch installation.
validateImportedLabelBase :: HeraldState -> Either HeraldInvariantFault ()
validateImportedLabelBase state = require StartupStatic LabelImportedBaseInvariant valid
  where
    patches = startupLabelPatchState state
    projection = startupOracleProjectionState state
    view = OracleProjection.oracleView projection
    imported = LabelPatch.importedLabelPatches patches
    valid = case LabelPatch.importedLabelBaseControlIndex patches of
      Nothing -> Map.null imported
      Just cut ->
        cut <= OracleProjection.oracleViewControlIndex view
          && isJust (checkedStartupAdmission (startupGenesis state))
          && all (matchesProjection cut) (Map.elems imported)
          && importedLatest == projectedLatest cut
    importedLatest =
      Map.fromList
        [ (LabelPatch.importedLabelPatchObject proof, (LabelPatch.importedLabelPatchReleaseIndex proof, LabelPatch.importedLabelPatchDecision proof))
        | proof <- Map.elems imported
        ]
    projectedLatest cut =
      Map.fromListWith
        max
        [ (preparedLabelFactsObjectId facts, (index, decision))
        | (decision, workflow) <- OracleProjection.projectedLabelWorkflows projection,
          Just facts <- [OracleProjection.projectedLabelWorkflowPreparedFacts workflow],
          Just (OracleProjection.ProjectedLabelReleased index _ _) <- [OracleProjection.projectedLabelWorkflowTerminal workflow],
          index <= cut
        ]
    matchesProjection cut proof =
      LabelPatch.importedLabelPatchReleaseIndex proof <= cut
        && case OracleProjection.oracleViewLabelWorkflow (LabelPatch.importedLabelPatchDecision proof) view of
          Nothing -> False
          Just workflow ->
            OracleProjection.projectedLabelWorkflowPreparedFacts workflow == Just (LabelPatch.importedLabelPatchPreparedFacts proof)
              && OracleProjection.projectedLabelWorkflowPreparedDigest workflow == Just (LabelPatch.importedLabelPatchPreparedDigest proof)
              && case OracleProjection.projectedLabelWorkflowTerminal workflow of
                Just (OracleProjection.ProjectedLabelReleased index _ _) -> index == LabelPatch.importedLabelPatchReleaseIndex proof
                _ -> False

importedHistoricalLabelReleaseMatches :: HeraldState -> GlobalObjectId -> LabelDecisionId -> ControlIndex -> Bool
importedHistoricalLabelReleaseMatches state object decision index =
  case LabelPatch.importedLabelBaseControlIndex (startupLabelPatchState state) of
    Nothing -> False
    Just cut ->
      index <= cut
        && case OracleProjection.oracleViewLabelWorkflow decision (OracleProjection.oracleView (startupOracleProjectionState state)) of
          Nothing -> False
          Just workflow ->
            maybe False ((== object) . preparedLabelFactsObjectId) (OracleProjection.projectedLabelWorkflowPreparedFacts workflow)
              && case OracleProjection.projectedLabelWorkflowTerminal workflow of
                Just (OracleProjection.ProjectedLabelReleased releasedAt _ _) -> releasedAt == index
                _ -> False

validateSlice4Owners :: HeraldState -> Either HeraldInvariantFault ()
validateSlice4Owners state = do
  validateImportedLabelBase state
  require
    StartupStatic
    LabelBarrierOwnerInvariant
    ( case LabelBarrier.validateBarrierState (startupLabelBarrierState state) of
        Right () ->
          LabelBarrier.barrierLocalHerald (startupLabelBarrierState state)
            == checkedLocalHeraldEpoch (startupGenesis state)
        Left _ -> False
    )
  let visibility = startupVisibilityState state
      patchState = startupLabelPatchState state
      retainedWork = LabelPatch.heldTargetWork patchState
      pendingPreparations =
        [ pending
        | retained <- Map.elems (LabelPatch.retainedLabelPatches patchState),
          Just pending <- [LabelPatch.retainedPatchViewPreparation retained]
        ]
      expectedRelations =
        Map.fromList
          [ ( key,
              NonEmpty.singleton
                ( Visibility.TargetDependency
                    (LabelPatch.pendingLabelPreparationDecision pending)
                    (LabelPatch.pendingLabelPreparationObject pending)
                )
            )
          | pending <- pendingPreparations,
            key <- Set.toAscList (LabelPatch.pendingLabelPreparationHeldWork pending)
          ]
      actualRelations = Visibility.visibilityDependencyRelations visibility
  require
    StartupStatic
    VisibilityOwnerInvariant
    ( case Visibility.validateVisibilityState
        retainedWork
        visibility of
        Right () ->
          Visibility.heldOwnerKeys visibility == retainedWork
            && actualRelations == expectedRelations
        Left _ -> False
    )
  require
    StartupStatic
    LabelOracleRequestInvariant
    (all labelRequestConsistent labelRequests)
  where
    local = checkedLocalHeraldEpoch (startupGenesis state)
    projection = startupOracleProjectionState state
    projectionView = OracleProjection.oracleView projection
    barrier = startupLabelBarrierState state
    application = startupApplicationState state
    labelRequests =
      [ witness
      | (_, witness) <-
          OracleClient.oracleClientRequestEntries (startupOracleClientState state),
        isLabelIntent (OracleClient.oracleRequestWitnessIntent witness)
      ]

    isLabelIntent = \case
      OracleClient.DecideLabelIntent {} -> True
      OracleClient.CompleteLabelDecisionIntent {} -> True
      _ -> False

    labelRequestConsistent witness =
      case OracleClient.oracleRequestWitnessStatus witness of
        OracleClient.OracleRequestAwaitingSourceDrain ->
          case OracleClient.oracleRequestWitnessIntent witness of
            OracleClient.DecideLabelIntent decision process object _ _ _ _ _ cut ->
              localInvocationMatches witness decision process object cut
                && Set.notMember decision (LabelBarrier.barrierActiveDecisions barrier)
                && OracleProjection.oracleViewLabelWorkflow decision projectionView == Nothing
            _ -> False
        OracleClient.OracleRequestEndedBeforeSubmission index ->
          case OracleClient.oracleRequestWitnessIntent witness of
            OracleClient.DecideLabelIntent decision process _ _ _ _ _ _ _ ->
              canonicalEndMatches process index
                && Application.applicationActiveLabelForDecision decision application == Nothing
                && Set.notMember decision (LabelBarrier.barrierActiveDecisions barrier)
                && OracleProjection.oracleViewLabelWorkflow decision projectionView == Nothing
            _ -> False
        OracleClient.OracleRequestProjected index ->
          case OracleClient.oracleClientRequestEvidence (OracleClient.oracleRequestWitnessRef witness) (startupOracleClientState state) of
            Just entry -> appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry) == index
            Nothing -> False
        OracleClient.OracleRequestDeferred {} -> awaitingIntentConsistent witness
        OracleClient.OracleRequestAwaitingProjection ->
          awaitingIntentConsistent witness

    -- A draining invocation owns only its selected application boundary. A
    -- different canonical workflow may use the distributed barrier meanwhile.
    localInvocationMatches witness decision process object cut =
      case Application.applicationActiveLabelForDecision decision application of
        Just active ->
          Application.activeLabelApplicationDecision active == decision
            && Application.applicationRequestKeyCanonicalBytes (Application.activeLabelApplicationKey active)
              == OracleClient.oracleRequestWitnessSemanticKey witness
            && Application.activeLabelApplicationPosition active
              == homeLabelAcceptanceCutProcessPosition cut
            && Application.activeLabelApplicationSelection active
              == Application.sequencingSelection process object
            && Application.activeLabelApplicationTerminalResult active == Nothing
        Nothing -> False

    -- This terminal status has lifecycle evidence, not a label receipt. The
    -- Oracle projection authenticates the End independently of the client.
    canonicalEndMatches process index =
      case OracleProjection.oracleViewEndedProcess process projectionView of
        Just ended ->
          OracleProjection.projectedEndedProcessControlIndex ended == index
            && OracleProjection.projectedEndedProcessEpoch ended == process
        Nothing -> False

    awaitingIntentConsistent witness = case OracleClient.oracleRequestWitnessIntent witness of
      OracleClient.DecideLabelIntent decision process object _ _ _ _ _ cut ->
        localInvocationMatches witness decision process object cut
      OracleClient.CompleteLabelDecisionIntent report ->
        OracleLabel.labelCompletionCollector report == local
          && case OracleProjection.oracleViewLabelWorkflow (OracleLabel.labelCompletionDecisionId report) projectionView of
            Just workflow -> case OracleProjection.projectedLabelWorkflowTerminal workflow of
              Just (OracleProjection.ProjectedLabelReleased _ _ digest) -> digest == OracleLabel.labelCompletionOutcomeDigest report
              _ -> False
            Nothing -> False
      _ -> False

-- | The Store owner proves its contiguous retained-history projection only
-- after the publication/receipt relations above have had the opportunity to
-- report their more precise cross-owner category.
validateStoreHistory :: HeraldState -> Either HeraldInvariantFault ()
validateStoreHistory state =
  require
    StartupStatic
    StoreHistoryInvariant
    ( case Store.validateStoreHistoryState (startupStoreState state) of
        Right () -> True
        Left _ -> False
    )

validateAlignmentOwner :: HeraldState -> Either HeraldInvariantFault ()
validateAlignmentOwner state =
  require
    StartupStatic
    AlignmentStateInvariant
    ( case Alignment.validateAlignmentState (startupAlignmentState state) of
        Right () -> True
        Left _ -> False
    )

-- | Cross-owner alignment facts are intentionally checked outside the pure
-- Alignment owner.  That owner can prove the shape of each transcript, but it
-- cannot by itself prove that a generation names an installed topology and a
-- replayable placement cut, or that its transfer work still names the exact
-- retained Store incarnations in the composed Herald.
validateAlignmentRelationships :: HeraldState -> Either HeraldInvariantFault ()
validateAlignmentRelationships state = do
  traverse_ validateExactAlignmentPlan (Alignment.alignmentPlanEntries alignment)
  traverse_ validateGeneration generationEntries
  traverse_ (validateObligation . snd) obligationEntries
  traverse_ (validateBootstrapImport . snd) bootstrapEntries
  validateDestinationBijection
  traverse_ (validateSourceTranscript . snd) sourceEntries
  traverse_ (validateSourceEnvelope . AlignmentTransfer.sourceRejectionSubscribe . snd) sourceRejectionEntries
  traverse_ (validateRouteCutover . snd) routeCutoverEntries
  either
    (const (fault StartupStatic AlignmentTransferRelationshipInvariant))
    Right
    (AlignmentTransferCoordinator.validateAlignmentTransferCoordinatorRelationships state)
  where
    alignment = startupAlignmentState state
    transfer = Alignment.alignmentTransferState alignment
    store = startupStoreState state
    local = checkedLocalHeraldEpoch (startupGenesis state)
    generationEntries = Alignment.alignmentGenerationEntries alignment
    obligationEntries = Alignment.alignmentObligationEntries alignment
    bootstrapEntries = Alignment.bootstrapImportEntries alignment
    ordinaryAttempts = fmap snd (Alignment.alignmentAttemptEntries alignment)
    bootstrapAttempts = fmap snd (Alignment.bootstrapImportAttemptEntries alignment)
    ordinaryInvalidations = fmap snd (Alignment.alignmentAttemptInvalidationEntries alignment)
    bootstrapInvalidations =
      fmap snd (Alignment.bootstrapImportAttemptInvalidationEntries alignment)
    sourceEntries = AlignmentTransfer.sourceSubscriptionEntries transfer
    sourceRejectionEntries = AlignmentTransfer.sourceRejectionEntries transfer
    destinationEntries = AlignmentTransfer.destinationSubscriptionEntries transfer
    routeCutoverEntries = AlignmentTransfer.routeCutoverEntries transfer
    -- A plan's current coordinate may bind generations born at several older
    -- coordinates, or contain no classes at all. Replay its explicit table;
    -- grouping birth cuts cannot reconstruct either case.
    validateExactAlignmentPlan (identifier, retained) = do
      plan <-
        either
          (const (fault StartupStatic AlignmentTopologyRelationshipInvariant))
          Right
          (AlignmentCoordinator.replayAlignmentPlanAt state identifier)
      require
        StartupStatic
        AlignmentTopologyRelationshipInvariant
        ( AlignmentPlan.alignmentPlanId retained == identifier
            && retained == plan
            && all
              ( \(_, binding) ->
                  let generation = AlignmentPlan.alignmentPlanBindingGeneration binding
                   in Alignment.lookupAlignmentGeneration (AlignmentGeneration.alignmentGenerationId generation) alignment
                        == Just generation
              )
              (AlignmentPlan.alignmentPlanBindings retained)
        )

    validateGeneration (_, generation) = do
      let cut = AlignmentGeneration.alignmentGenerationCut generation
          topology = DomainAlignment.alignmentCutTopologyCut cut
          topologyIdentifier = deriveTopologyCutId topology
          vector = DomainAlignment.alignmentCutPhysicalPlacementRevisionVector cut
          members = NonEmpty.toList (DomainAlignment.alignmentCutExactMembers cut)
      require
        StartupStatic
        AlignmentTopologyRelationshipInvariant
        ( maybe
            False
            ((== topology) . GraphProgress.installedTopologyCutCut)
            ( GraphProgress.lookupInstalledTopologyCut
                topologyIdentifier
                (startupStructuralProgressState state)
            )
        )
      routes <-
        either
          (const (fault StartupStatic AlignmentPlacementRelationshipInvariant))
          Right
          (Placement.placementRoutesAtVector vector (startupPlacementState state))
      require
        StartupStatic
        AlignmentPlacementRelationshipInvariant
        (all (memberMatchesRoute cut routes) members)
      traverse_ (validateRetainedMember cut) members

    memberMatchesRoute cut routes member =
      any
        ( \(owner, ownerRoutes) ->
            owner == DomainAlignment.alignmentMemberHerald member
              && any
                ( \route ->
                    PlacementRoute.deltaRouteDelta route
                      == DomainAlignment.alignmentMemberDelta member
                      && PlacementRoute.deltaRouteStoreIncarnation route
                        == DomainAlignment.alignmentMemberStoreIncarnation member
                      && PlacementRoute.deltaRouteSortId route
                        == DomainAlignment.alignmentCutSortId cut
                      && PlacementRoute.deltaRouteOccurrenceId route
                        == DomainAlignment.alignmentCutSortDefinitionOccurrenceId cut
                )
                ownerRoutes
        )
        routes

    validateRetainedMember cut member
      | DomainAlignment.alignmentMemberHerald member /= local = Right ()
      | otherwise =
          require
            (StartupDelta (DomainAlignment.alignmentMemberDelta member))
            AlignmentStoreRelationshipInvariant
            ( retainedStoreMatches
                (DomainAlignment.alignmentMemberDelta member)
                (DomainAlignment.alignmentMemberStoreIncarnation member)
                (DomainAlignment.alignmentCutSortId cut)
                (DomainAlignment.alignmentCutSortDefinitionOccurrenceId cut)
            )

    validateObligation obligation = do
      generation <- requireGeneration (AlignmentProtocol.alignmentObligationDestinationGeneration obligation)
      let cut = AlignmentGeneration.alignmentGenerationCut generation
      require
        StartupStatic
        AlignmentStoreRelationshipInvariant
        ( all
            (destinationBelongsToLocalGeneration cut)
            (NonEmpty.toList (AlignmentProtocol.alignmentObligationDestinationStores obligation))
        )

    validateBootstrapImport bootstrapImport = do
      let key = Alignment.bootstrapImportKeyValue bootstrapImport
          destination = Alignment.bootstrapImportKeyDestination key
      generation <- requireGeneration (Alignment.bootstrapImportKeyGeneration key)
      require
        StartupStatic
        AlignmentStoreRelationshipInvariant
        (destinationBelongsToLocalGeneration (AlignmentGeneration.alignmentGenerationCut generation) destination)

    requireGeneration identifier =
      maybe
        (fault StartupStatic AlignmentTopologyRelationshipInvariant)
        Right
        (Alignment.lookupAlignmentGeneration identifier alignment)

    destinationBelongsToLocalGeneration cut destination =
      any
        ( \member ->
            DomainAlignment.alignmentMemberHerald member == local
              && DomainAlignment.alignmentMemberDelta member
                == AlignmentProtocol.destinationStoreDelta destination
              && DomainAlignment.alignmentMemberStoreIncarnation member
                == AlignmentProtocol.destinationStoreIncarnation destination
        )
        (NonEmpty.toList (DomainAlignment.alignmentCutExactMembers cut))
        && retainedStoreMatches
          (AlignmentProtocol.destinationStoreDelta destination)
          (AlignmentProtocol.destinationStoreIncarnation destination)
          (DomainAlignment.alignmentCutSortId cut)
          (DomainAlignment.alignmentCutSortDefinitionOccurrenceId cut)

    retainedStoreMatches delta incarnation sortId occurrence =
      case Store.lookupRetainedStoreSlot incarnation store of
        Nothing -> False
        Just slot ->
          Store.storeSlotDelta slot == delta
            && Store.storeSlotIncarnation slot == incarnation
            && Store.storeSlotSortId slot == sortId
            && Store.storeSlotOccurrenceId slot == occurrence

    validateDestinationBijection =
      require
        StartupStatic
        AlignmentTransferRelationshipInvariant
        ( Map.keysSet destinationsBySubscription == Map.keysSet expectedDestinations
            && all destinationMatches (Map.toAscList destinationsBySubscription)
        )
      where
        destinationsBySubscription = Map.fromList destinationEntries
        ordinaryBySubscription =
          Map.fromList
            [ ( AlignmentProtocol.alignmentAttemptSubscriptionId attempt,
                (Left attempt, Nothing)
              )
            | attempt <- ordinaryAttempts
            ]
        bootstrapBySubscription =
          Map.fromList
            [ ( AlignmentProtocol.alignmentSubscribeSubscriptionId
                  (Alignment.bootstrapImportAttemptSubscribe attempt),
                (Right attempt, Nothing)
              )
            | attempt <- bootstrapAttempts
            ]
        retiredOrdinaryBySubscription =
          Map.fromList
            [ ( AlignmentProtocol.alignmentAttemptSubscriptionId attempt,
                ( Left attempt,
                  Just (Alignment.alignmentAttemptInvalidationReason receipt)
                )
              )
            | receipt <- ordinaryInvalidations,
              let attempt = Alignment.alignmentAttemptInvalidationAttempt receipt
            ]
        retiredBootstrapBySubscription =
          Map.fromList
            [ ( AlignmentProtocol.alignmentSubscribeSubscriptionId subscribe,
                ( Right attempt,
                  Just (Alignment.bootstrapImportAttemptInvalidationReason receipt)
                )
              )
            | receipt <- bootstrapInvalidations,
              let attempt = Alignment.bootstrapImportAttemptInvalidationAttempt receipt,
              let subscribe = Alignment.bootstrapImportAttemptSubscribe attempt
            ]
        expectedDestinations =
          Map.unions
            [ ordinaryBySubscription,
              bootstrapBySubscription,
              retiredOrdinaryBySubscription,
              retiredBootstrapBySubscription
            ]
        destinationMatches (identifier, destination) =
          case Map.lookup identifier expectedDestinations of
            Just (Left attempt, cancellationReason) ->
              AlignmentTransfer.destinationSubscriptionAttempt destination == Just attempt
                && expectedOrdinarySubscribe attempt
                  == Just (AlignmentTransfer.destinationSubscriptionSubscribe destination)
                && AlignmentTransfer.destinationSubscriptionBootstrapSourceHerald destination
                  == Nothing
                && cancellationMatches identifier cancellationReason destination
            Just (Right attempt, cancellationReason) ->
              AlignmentTransfer.destinationSubscriptionAttempt destination == Nothing
                && AlignmentTransfer.destinationSubscriptionSubscribe destination
                  == Alignment.bootstrapImportAttemptSubscribe attempt
                && AlignmentTransfer.destinationSubscriptionBootstrapSourceHerald destination
                  == Just (Alignment.bootstrapImportAttemptSourceHerald attempt)
                && cancellationMatches identifier cancellationReason destination
            Nothing -> False

        cancellationMatches identifier expected destination =
          AlignmentTransfer.destinationSubscriptionCancellation destination
            == (AlignmentProtocol.alignmentCancel identifier <$> expected)

    expectedOrdinarySubscribe attempt =
      let certificate = AlignmentProtocol.alignmentAttemptHistoricalCertificate attempt
       in either
            (const Nothing)
            Just
            ( AlignmentProtocol.alignmentSubscribe
                (AlignmentProtocol.alignmentAttemptObligation attempt)
                (AlignmentProtocol.alignmentAttemptSubscriptionId attempt)
                (AlignmentProtocol.historicalCertificateSourceStoreIncarnation certificate)
                ( AlignmentProtocol.FinalCertificate
                    (AlignmentProtocol.historicalCertificateClassGeneration certificate)
                    (AlignmentProtocol.alignmentAttemptHistoricalCertificateDigest attempt)
                )
            )

    validateSourceTranscript source = do
      let subscribe = AlignmentTransfer.sourceSubscriptionSubscribe source
          sourceStore = AlignmentProtocol.alignmentSubscribeSourceStoreIncarnation subscribe
          base = AlignmentTransfer.sourceSubscriptionBaseRevision source
          sentThrough = AlignmentTransfer.sourceSubscriptionSentThrough source
          obligation = AlignmentProtocol.alignmentSubscribeObligation subscribe
      validateSourceEnvelope subscribe
      generation <- requireGeneration (AlignmentProtocol.alignmentObligationSourceGeneration obligation)
      require
        StartupStatic
        AlignmentStoreRelationshipInvariant
        (sourceStoreBelongsToLocalGeneration sourceStore generation)
      case Store.lookupRetainedStoreSlot sourceStore store of
        Nothing ->
          require
            StartupStatic
            AlignmentTransferRelationshipInvariant
            (AlignmentTransfer.sourceSubscriptionCancellation source /= Nothing)
        Just slot -> do
          require
            StartupStatic
            AlignmentStoreRelationshipInvariant
            (Store.storeSlotRevision slot >= sentThrough)
          snapshot <-
            either
              (const (fault StartupStatic AlignmentTransferRelationshipInvariant))
              Right
              (Store.retainedStoreAlignmentSnapshotAt sourceStore base store)
          projectedFacts <-
            either
              (const (fault StartupStatic AlignmentTransferRelationshipInvariant))
              Right
              (traverse retainedSnapshotFactEvidence (Store.retainedStoreSnapshotFacts snapshot))
          changes <-
            either
              (const (fault StartupStatic AlignmentTransferRelationshipInvariant))
              Right
              (Store.retainedStoreChangesAfter sourceStore base store)
          projectedChanges <-
            either
              (const (fault StartupStatic AlignmentTransferRelationshipInvariant))
              Right
              ( traverse
                  (retainedStoreChangeMessage (AlignmentProtocol.alignmentSubscribeSubscriptionId subscribe))
                  (filter ((<= sentThrough) . Store.retainedStoreChangeRevision) changes)
              )
          let canonicalFacts =
                sortOn AlignmentProtocol.retainedStateEvidenceCanonicalBytes projectedFacts
          require
            StartupStatic
            AlignmentTransferRelationshipInvariant
            ( canonicalFacts == AlignmentTransfer.sourceSubscriptionSnapshotFacts source
                && projectedChanges == AlignmentTransfer.sourceSubscriptionSentChanges source
            )
    validateSourceEnvelope subscribe =
      either
        (const (fault StartupStatic AlignmentTransferRelationshipInvariant))
        (const (Right ()))
        ( AlignmentTransferCoordinator.validateRetainedSourceSubscribe
            ( AlignmentProtocol.alignmentSubscriptionIdDestinationHerald
                (AlignmentProtocol.alignmentSubscribeSubscriptionId subscribe)
            )
            subscribe
            state
        )

    sourceStoreBelongsToLocalGeneration sourceStore generation =
      any
        ( \member ->
            DomainAlignment.alignmentMemberHerald member == local
              && DomainAlignment.alignmentMemberStoreIncarnation member == sourceStore
        )
        ( NonEmpty.toList
            ( DomainAlignment.alignmentCutExactMembers
                (AlignmentGeneration.alignmentGenerationCut generation)
            )
        )

    validateRouteCutover marker = do
      generation <- requireGeneration (alignmentRouteCutoverMarkerGeneration marker)
      plan <-
        maybe
          (fault StartupStatic AlignmentTransferRelationshipInvariant)
          Right
          (Alignment.lookupAlignmentPlan (alignmentRouteCutoverMarkerPlan marker) alignment)
      let cut = AlignmentGeneration.alignmentGenerationCut generation
          fixedHeralds =
            Set.fromList
              ( fmap
                  fst
                  ( NonEmpty.toList
                      ( DomainAlignment.physicalPlacementRevisionEntries
                          (AlignmentPlan.alignmentPlanIdPlacement (AlignmentPlan.alignmentPlanId plan))
                      )
                  )
              )
          predecessor = alignmentRouteCutoverMarkerPredecessorHerald marker
      require
        StartupStatic
        AlignmentTransferRelationshipInvariant
        ( Set.member (alignmentRouteCutoverMarkerSourceHerald marker) fixedHeralds
            && Set.member predecessor fixedHeralds
            && any
              ( \(_, binding) ->
                  AlignmentPlan.alignmentPlanBindingDisposition binding == AlignmentPlan.AlignmentCreated
                    && AlignmentPlan.alignmentPlanBindingGeneration binding == generation
              )
              (AlignmentPlan.alignmentPlanBindings plan)
            && Alignment.lookupAlignmentPlanAcceptance
              (alignmentRouteCutoverMarkerPlan marker)
              (alignmentRouteCutoverMarkerSourceHerald marker)
              alignment
              /= Nothing
            && any
              (predecessorGenerationHostedBy predecessor)
              (DomainAlignment.alignmentCutPredecessorGenerationIds cut)
            && routeCutoverDeliveryValid marker
        )

    routeCutoverDeliveryValid marker
      | source == local,
        predecessor /= local =
          maybe
            False
            routeAssignmentRetained
            (lookup key (AlignmentTransfer.routeCutoverAssignmentEntries transfer))
      | source /= local,
        predecessor == local =
          lookup key (AlignmentTransfer.routeCutoverAssignmentEntries transfer) == Nothing
      | otherwise = False
      where
        source = alignmentRouteCutoverMarkerSourceHerald marker
        predecessor = alignmentRouteCutoverMarkerPredecessorHerald marker
        key =
          ( alignmentRouteCutoverMarkerGeneration marker,
            source,
            predecessor
          )

    -- Incoming route-cutover retention is itself the checked semantic receipt:
    -- the Alignment coordinator admits source, generation and predecessor before
    -- installing it atomically with stream completion. Its stream payload can go.
    routeAssignmentRetained assignment =
      let direction = assignmentReceiptDirection assignment
          stream = startupPeerStreamState state
          active =
            either
              (const [])
              ( fmap
                  ( \item ->
                      assignmentReceipt
                        (sequencedItemDirection item)
                        (sequencedItemSequence item)
                        (sequencedItemDigest item)
                  )
              )
              (PeerStream.activeOutgoingItems direction stream)
       in assignment `elem` active
            || PeerStream.assignmentCompletionKnown assignment stream == Right True
            || PeerStream.assignmentRetirementKnown assignment stream == Right True

    predecessorGenerationHostedBy herald identifier =
      maybe
        False
        ( any ((== herald) . DomainAlignment.alignmentMemberHerald)
            . NonEmpty.toList
            . DomainAlignment.alignmentCutExactMembers
            . AlignmentGeneration.alignmentGenerationCut
        )
        (Alignment.lookupAlignmentGeneration identifier alignment)

retainedSnapshotFactEvidence ::
  Store.RetainedStoreSnapshotFact ->
  Either AlignmentProtocol.AlignmentProtocolProblem AlignmentProtocol.RetainedStateEvidence
retainedSnapshotFactEvidence fact = do
  representative <-
    retainedStoreObservationEvidence
      (Store.retainedStoreSnapshotFactRepresentative fact)
  witness <-
    retainedStoreObservationEvidence
      (Store.retainedStoreSnapshotFactStrengthWitness fact)
  AlignmentProtocol.retainedStateEvidence
    representative
    witness
    (Store.retainedStoreSnapshotFactStrength fact)

retainedStoreChangeMessage ::
  AlignmentProtocol.AlignmentSubscriptionId ->
  Store.RetainedStoreChange ->
  Either AlignmentProtocol.AlignmentProtocolProblem AlignmentProtocol.AlignmentChange
retainedStoreChangeMessage identifier change =
  AlignmentProtocol.alignmentChange
    identifier
    (Store.retainedStoreChangeRevision change)
    <$> retainedStoreObservationEvidence
      (Store.retainedStoreChangeObservation change)

retainedStoreObservationEvidence ::
  Store.RetainedStoreObservation ->
  Either
    AlignmentProtocol.AlignmentProtocolProblem
    AlignmentProtocol.RetainedPublicationEvidence
retainedStoreObservationEvidence observation =
  case Store.retainedStoreObservationOrigin observation of
    Store.PrimordialStoreObservation ->
      Right
        ( AlignmentProtocol.primordialRetainedPublicationEvidence
            identifier
            (checkedPublicationSort publication)
            occurrence
            (checkedPublicationCanonicalValue publication)
            strength
        )
    Store.RoutedStoreObservation envelope ->
      AlignmentProtocol.retainedPublicationEvidence
        identifier
        (Store.storeObservationEnvelopeSourceProcess envelope)
        (checkedPublicationSort publication)
        occurrence
        (checkedPublicationCanonicalValue publication)
        strength
        (Store.storeObservationEnvelopeSourceTopology envelope)
        (Store.storeObservationEnvelopeControlPrerequisite envelope)
        (Store.storeObservationEnvelopeStructuralStamp envelope)
  where
    publication = Store.retainedStoreObservationPublication observation
    identifier = checkedPublicationId publication
    occurrence = Store.retainedStoreObservationSortOccurrence observation
    strength = Store.retainedStoreObservationIncomingStrength observation

type ResidentApplicationAlias =
  (ApplicationSession.ApplicationAttachment, ProcessEpochId)

-- | The isolation coordinator deliberately retains the application-side owner
-- after an Oracle retirement has removed the process from the live projection.
-- These aliases are admissible only for the exact losing-branch overlay; once
-- the bounded drain reaches its terminal point the application owner removes
-- them again.
isolationApplicationAliases :: HeraldState -> [ResidentApplicationAlias]
isolationApplicationAliases state =
  [ alias
  | alias@(_, process) <- attachments,
    process `Set.member` overlay
  ]
  where
    Application.ApplicationStateWitness attachments _ _ _ =
      Application.applicationStateWitness (startupApplicationState state)
    overlay =
      Isolation.isolationWitnessResidentProcessOverlay
        (Isolation.stateWitness (startupIsolationState state))

-- | Index-zero bootstraps and ready dynamic Starts own application aliases.
-- Dynamic Starts carry selected access and a normal process fact; only the
-- immutable genesis set contributes bootstrap root facts.
residentApplicationAliases ::
  HeraldState ->
  [AppliedProcessBootstrap] ->
  [ResidentApplicationAlias]
residentApplicationAliases state resident =
  [ ( ApplicationSession.applicationAttachmentForBootstrap
        (appliedBootstrapManifestId bootstrap),
      appliedProcessEpochId bootstrap
    )
  | bootstrap <- resident
  ]
    <> [ (attachment, process)
       | (attachment, process) <- Administration.configuredStartApplicationAliases (startupAdministrationState state),
         dynamicApplicationResident state process
       ]
    <> [ (ApplicationSession.applicationAttachmentForProcess process, process)
       | (_, preparation) <- ProcessPreparation.preparationEntries (startupProcessPreparationState state),
         ProcessPreparation.preparationWasInstalled preparation,
         let process = processStartProcessEpochId (ProcessPreparation.preparationStart preparation),
         dynamicApplicationResident state process
       ]

-- | End removes live access while keeping the immutable administration result.
-- Isolation retains access only during its explicitly owned read-only drain.
dynamicApplicationResident :: HeraldState -> ProcessEpochId -> Bool
dynamicApplicationResident state process =
  not (phase == Isolation.IsolationTerminalView && isolated)
    && ( OracleProjection.oracleViewProcessIsLive
           process
           (OracleProjection.oracleView (startupOracleProjectionState state))
           || (phase == Isolation.IsolationReadOnlyDrainView && isolated)
       )
  where
    isolation = Isolation.stateWitness (startupIsolationState state)
    phase = Isolation.isolationWitnessPhase isolation
    isolated = process `Set.member` Isolation.isolationWitnessResidentProcessOverlay isolation

-- | Relate the self-validating structural-progress owner to immutable genesis
-- and to the two physical owners updated in the same structural transaction.
-- Alignment may legitimately retain no debt for an applied occurrence, so its
-- exact relationship is a typed subset of the applied occurrence history.
data StructuralMembershipPhase
  = StructuralMembershipSynchronized HeraldMembershipGeneration
  | StructuralMembershipAwaitingSuccessorBase
      HeraldMembershipGeneration
      HeraldMembershipGeneration

structuralMembershipPhase ::
  OracleProjection.View ->
  GraphProgress.StructuralProgressState ->
  Maybe StructuralMembershipPhase
structuralMembershipPhase oracleView progress = do
  graphMembership <-
    OracleProjection.oracleViewHeraldMembershipById graphMembershipId oracleView
  if graphMembershipId == heraldMembershipGenerationId currentMembership
    then Just (StructuralMembershipSynchronized graphMembership)
    else do
      _ <-
        OracleProjection.oracleViewHeraldMembershipLineage
          graphMembershipId
          (heraldMembershipGenerationId currentMembership)
          oracleView
      Just
        ( StructuralMembershipAwaitingSuccessorBase
            graphMembership
            currentMembership
        )
  where
    graphMembershipId =
      GraphProgress.structuralProgressMembershipGenerationId progress
    currentMembership =
      OracleProjection.oracleViewCurrentHeraldMembership oracleView

structuralPhaseGraphMembership ::
  StructuralMembershipPhase -> HeraldMembershipGeneration
structuralPhaseGraphMembership phase = case phase of
  StructuralMembershipSynchronized membership -> membership
  StructuralMembershipAwaitingSuccessorBase predecessor _ -> predecessor

validateStructuralOwners :: HeraldState -> Either HeraldInvariantFault ()
validateStructuralOwners state = do
  require
    StartupStatic
    StructuralProgressInvariant
    ( case GraphProgress.validateStructuralProgressState progress of
        Right () -> True
        Left _ -> False
    )
  membershipPhase <-
    maybe
      (fault StartupStatic StructuralProgressInvariant)
      Right
      (structuralMembershipPhase oracleView progress)
  genesisMembership <-
    case filter
      ((== Nothing) . heraldMembershipGenerationPredecessor)
      (OracleProjection.oracleViewHeraldMembershipHistory oracleView) of
      [membership] -> Right membership
      _ -> fault StartupStatic StructuralProgressInvariant
  require StartupStatic StructuralProgressInvariant membershipClosureMatchesHistory
  let graphMembership = structuralPhaseGraphMembership membershipPhase
      expectedMembers =
        heraldMembershipGenerationActiveHeraldEpochs graphMembership
  expectedGenesisCut <-
    either
      (const (fault StartupStatic StructuralProgressInvariant))
      Right
      ( deriveGenesisTopologyCutId
          (checkedSystemId genesis)
          genesisMembership
          (checkedInitialProjectionDigest (startupInitialBootstraps state))
      )
  require
    StartupStatic
    StructuralProgressInvariant
    ( GraphProgress.structuralProgressLocalHerald progress
        == checkedLocalHeraldEpoch genesis
        && NonEmpty.toList (GraphProgress.structuralProgressMembers progress)
          == NonEmpty.toList expectedMembers
        && GraphProgress.structuralGenesisCutId progress == expectedGenesisCut
        && all
          (isJust . (`GraphProgress.lookupAppliedStructuralOccurrence` progress))
          reconciliationOccurrences
        && all alignmentDebtMatchesHistory alignmentDebts
        && all
          controlOverlayCauseRetained
          (Reconciliation.structuralAppliedControlOverlays reconciliation)
        && all
          (\(_, object, overlay) -> controlOverlayCauseRetained (object, overlay))
          (Reconciliation.structuralAppliedControlHistory reconciliation)
        && all
          retirementSuppressionAuthenticated
          (Reconciliation.structuralAppliedRetirementSuppressions reconciliation)
    )
  require
    StartupStatic
    StructuralProgressInvariant
    ( Graph.graphStructuralVertexProjections (startupGraphState state)
        == Reconciliation.structuralAppliedVertexProjections reconciliation
        && Graph.graphStructuralEdgeProjections (startupGraphState state)
          == Reconciliation.structuralAppliedEdgeProjections reconciliation
    )
  require
    StartupStatic
    StoreFactInvariant
    ( Map.keysSet activeStructuralSlots == Map.keysSet activeResources
        && Map.keysSet retainedStructuralSlots == Map.keysSet retainedResources
        && and
          [ structuralStoreSlotMatches slot resource
          | (delta, slot) <- Map.toAscList activeStructuralSlots,
            Just resource <- [Map.lookup delta activeResources]
          ]
        && and
          [ structuralStoreSlotMatches slot resource
          | (incarnation, slot) <- Map.toAscList retainedStructuralSlots,
            Just resource <- [Map.lookup incarnation retainedResources]
          ]
    )
  where
    genesis = startupGenesis state
    progress = startupStructuralProgressState state
    membershipClosureMatchesHistory = case startupStructuralBaseCoordinator state of
      Nothing -> True
      Just closure ->
        let target = heraldMembershipLineageTarget closure.lineage
            targetId = heraldMembershipGenerationId target
            anchor = heraldMembershipLineageOrigin closure.lineage
            anchorId = heraldMembershipGenerationId anchor
            genesisId = heraldMembershipGenerationId (heraldMembershipHistoryGenesis (OracleProjection.oracleViewHeraldMembershipHistoryChecked oracleView))
            anchorCut = TerminalSource.terminalSourcePredecessorBaseCutId closure.predecessorBase
            anchorMatches
              | anchorCut == GraphProgress.structuralGenesisCutId progress = TerminalSource.terminalSourceGenesisPredecessorBase anchor anchorCut == Right closure.predecessorBase
              | otherwise = case GraphProgress.lookupInstalledTopologyCut anchorCut progress of
                  Nothing -> False
                  Just installed -> TerminalSource.terminalSourceInstalledPredecessorBase anchor (GraphProgress.installedTopologyCutCut installed) == Right closure.predecessorBase
         in closure.localHerald == checkedLocalHeraldEpoch genesis
              && OracleProjection.oracleViewHeraldMembershipLineage genesisId targetId oracleView == Just closure.historyLineage
              && OracleProjection.oracleViewHeraldMembershipLineage anchorId targetId oracleView == Just closure.lineage
              && ( not (OracleProjection.oracleViewLocalHeraldIsCurrent oracleView)
                     || target == OracleProjection.oracleViewCurrentHeraldMembership oracleView
                     || ( isJust closure.installedBase
                            && isJust (OracleProjection.oracleViewHeraldMembershipLineage targetId (OracleProjection.oracleViewCurrentHeraldMembershipId oracleView) oracleView)
                        )
                 )
              && anchorMatches
              && case closure.installedBase of
                Nothing -> True
                Just installed ->
                  TerminalSource.successorStructuralBaseLineage installed == closure.lineage
                    && closure.established == Just (TerminalSource.successorStructuralBaseEstablishedUnion installed)
                    && GraphProgress.structuralSuccessorBaseInstalled installed progress
    reconciliation = GraphProgress.structuralProgressReconciliation progress
    reconciliationOccurrences =
      Reconciliation.structuralAppliedHistoryOccurrences reconciliation
    reconciliationOccurrenceSet = Set.fromList reconciliationOccurrences
    registry = startupSortRegistryState state
    alignmentDebts =
      StructuralDebt.structuralDebtSetEntries
        (Alignment.alignmentStructuralDebts (startupAlignmentState state))
    alignmentDebtMatchesHistory debt =
      let key = StructuralDebt.structuralConsequenceDebtKey debt
          cause = StructuralDebt.structuralDebtKeyCause key
       in case StructuralConsequence.structuralConsequenceCauseView cause of
            StructuralConsequence.StructuralOccurrenceCauseView occurrence ->
              Set.member occurrence reconciliationOccurrenceSet
                && isJust
                  (GraphProgress.lookupAppliedStructuralOccurrence occurrence progress)
            StructuralConsequence.LabelReleaseCauseView _ index ->
              GraphProgress.structuralAppliedControlPrefix progress >= index
            StructuralConsequence.ProcessEndCauseView _ index ->
              GraphProgress.structuralAppliedControlPrefix progress >= index
            StructuralConsequence.PredefinedDisappearanceCauseView _ index ->
              GraphProgress.structuralAppliedControlPrefix progress >= index
    retirementSuppressionAuthenticated (_, _, publication, suppression) =
      case suppression of
        Reconciliation.DirectRegularSortRetirementSuppression referencedSort floorIndex ->
          publicationReferencesSort referencedSort publication
            && any
              ( \retirement ->
                  SortRegistry.regularSortRetirementSortId retirement
                    == referencedSort
                    && SortRegistry.regularSortRetirementResolveIndex retirement
                      == floorIndex
              )
              (SortRegistry.regularSortRetirements registry)
        Reconciliation.InheritedEndpointRetirementSuppression {} -> True
    publicationReferencesSort referencedSort publication =
      case SortRegistry.lookupEffectiveSort (checkedPublicationSort publication) registry of
        Nothing -> False
        Just carrierEntry ->
          case structuralCarrierSortReferences
            (SortRegistry.registryEntryDescriptor carrierEntry)
            (checkedPublicationValue publication) of
            Left _ -> False
            Right references -> referencedSort `elem` references
    controlOverlayCauseRetained (object, overlay) =
      case overlay of
        Reconciliation.StructuralControllerOverlayView cause controller ->
          case StructuralConsequence.structuralConsequenceCauseView cause of
            StructuralConsequence.LabelReleaseCauseView decision index ->
              releasedPatchMatches object decision index
            StructuralConsequence.ProcessEndCauseView process index ->
              controller == Reconciliation.ZombieProcessController process
                && projectedEndMatches process index
            StructuralConsequence.StructuralOccurrenceCauseView {} -> False
            StructuralConsequence.PredefinedDisappearanceCauseView {} -> False
        Reconciliation.StructuralDeletedOverlayView cause ->
          case StructuralConsequence.structuralConsequenceCauseView cause of
            StructuralConsequence.LabelReleaseCauseView decision index ->
              releasedPatchMatches object decision index
                && terminalDeletionMatches object cause
            StructuralConsequence.StructuralOccurrenceCauseView {} -> False
            StructuralConsequence.ProcessEndCauseView {} -> False
            StructuralConsequence.PredefinedDisappearanceCauseView _ index ->
              GraphProgress.structuralAppliedControlPrefix progress >= index
                && terminalDeletionMatches object cause
    terminalDeletionMatches object cause =
      case Controlled.controlledTerminalDeletion
        object
        (startupControlledState state) of
        Just deletion ->
          Controlled.controlledTerminalDeletionObject deletion == object
            && Controlled.controlledTerminalDeletionCause deletion == cause
        Nothing -> False
    releasedPatchMatches object decision index =
      case LabelPatch.lookupRetainedLabelPatch decision (startupLabelPatchState state) of
        Just retained ->
          case LabelPatch.retainedPatchViewInstallation retained of
            Just installed ->
              LabelPatch.installedLabelObject installed == object
                && LabelPatch.installedLabelReleaseIndex installed == index
            Nothing -> False
        Nothing -> importedHistoricalLabelReleaseMatches state object decision index
    projectedEndMatches process index =
      case OracleProjection.oracleViewEndedProcess process oracleView of
        Just ended ->
          OracleProjection.projectedEndedProcessControlIndex ended == index
        Nothing -> False
    oracleView = OracleProjection.oracleView (startupOracleProjectionState state)
    activeResources =
      Map.fromList
        [ (Reconciliation.dynamicStoreDelta resource, resource)
        | resource <- Reconciliation.structuralAppliedLocalStores reconciliation
        ]
    retainedResources =
      Reconciliation.structuralAppliedRetainedStoreResources reconciliation
    activeStructuralSlots =
      Map.fromList
        [ (Store.storeSlotDelta slot, slot)
        | slot <- Store.storeSlots store,
          structuralStoreSlot slot
        ]
    retainedStructuralSlots =
      Map.fromList
        [ (Store.storeSlotIncarnation slot, slot)
        | slot <- Store.retainedStoreSlots store,
          structuralStoreSlot slot
        ]
    store = startupStoreState state
    structuralStoreSlot slot = case Store.storeSlotProvenance slot of
      Store.StructuralBaselineReader {} -> True
      Store.StructuralApplicationReader {} -> True
      Store.StructuralControlReader {} -> True
      Store.ApplicationReader {} -> True
      _ -> False
    structuralStoreSlotMatches slot resource =
      Reconciliation.dynamicStoreHerald resource == checkedLocalHeraldEpoch genesis
        && Store.storeSlotDelta slot == Reconciliation.dynamicStoreDelta resource
        && Store.storeSlotSortId slot
          == StructuralDebt.sortOccurrenceSortId
            (Reconciliation.dynamicStoreSort resource)
        && Store.storeSlotOccurrenceId slot
          == StructuralDebt.sortOccurrenceDefinition
            (Reconciliation.dynamicStoreSort resource)
        && Store.storeSlotIncarnation slot
          == Reconciliation.dynamicStoreIncarnation resource
        && case Store.storeSlotProvenance slot of
          Store.StructuralBaselineReader publication process controller source ->
            Reconciliation.structuralAppliedBaselineStoreMatches
              publication
              resource
              reconciliation
              && process == Reconciliation.dynamicStoreProcess resource
              && controller == Reconciliation.dynamicStoreController resource
              && source == Reconciliation.dynamicStoreIncarnationSource resource
          Store.StructuralApplicationReader occurrence process controller source ->
            Set.member occurrence reconciliationOccurrenceSet
              && process == Reconciliation.dynamicStoreProcess resource
              && controller == Reconciliation.dynamicStoreController resource
              && source == Reconciliation.dynamicStoreIncarnationSource resource
          Store.StructuralControlReader cause process controller source ->
            controlStoreCauseRetained cause resource
              && process == Reconciliation.dynamicStoreProcess resource
              && controller == Reconciliation.dynamicStoreController resource
              && source == Reconciliation.dynamicStoreIncarnationSource resource
          Store.ApplicationReader process _ ->
            process == Reconciliation.dynamicStoreProcess resource
          _ -> False
    controlStoreCauseRetained cause resource =
      case StructuralConsequence.structuralConsequenceCauseView cause of
        StructuralConsequence.LabelReleaseCauseView decision index ->
          releasedPatchMatches
            (Reconciliation.dynamicStoreController resource)
            decision
            index
        StructuralConsequence.ProcessEndCauseView process index ->
          process == Reconciliation.dynamicStoreProcess resource
            && projectedEndMatches process index
        StructuralConsequence.StructuralOccurrenceCauseView {} -> False
        StructuralConsequence.PredefinedDisappearanceCauseView {} -> False

validatePeerOwners :: HeraldState -> Either HeraldInvariantFault ()
validatePeerOwners state = do
  let genesis = startupGenesis state
      localEpoch = checkedLocalHeraldEpoch genesis
      expectedCatalogue =
        Map.fromList
          [ (heraldMemberEpoch member, heraldMemberId member)
          | member <- checkedActiveHeralds genesis
          ]
      liveCatalogue = Discovery.liveHeraldCatalogue (startupDiscoveryState state)
      projectionView =
        OracleProjection.oracleView (startupOracleProjectionState state)
      currentMembership =
        OracleProjection.oracleViewCurrentHeraldMembership projectionView
      currentMembers =
        Map.fromList
          [ (heraldMemberEpoch member, heraldMemberId member)
          | member <- OracleProjection.oracleViewActiveHeralds projectionView
          ]
      peerStream = startupPeerStreamState state
      placement = startupPlacementState state
      peerDirectionOwnerValid peer =
        Map.member peer liveCatalogue
          && ( Map.member peer currentMembers
                 /= PeerStream.peerStreamPeerRetired peer peerStream
             )
      DiscoveryStatic
        observedSystem
        observedLocalId
        observedLocalEpoch
        observedMembers
        observedInitialMembership
        observedCatalogue
        observedProjection =
          Discovery.discoveryStaticView (startupDiscoveryState state)
  require
    StartupStatic
    DiscoveryOwnerInvariant
    ( observedSystem == checkedSystemId genesis
        && observedLocalId == checkedLocalHeraldId genesis
        && observedLocalEpoch == localEpoch
        && observedMembers == expectedCatalogue
        && sort
          (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs observedInitialMembership))
          == sort (checkedActiveHeraldEpochs genesis)
        && observedCatalogue == checkedCatalogueDigest genesis
        && observedProjection
          == checkedInitialProjectionDigest (startupInitialBootstraps state)
        && Discovery.currentMembership (startupDiscoveryState state)
          == currentMembership
        && Map.isSubmapOf currentMembers liveCatalogue
        && case Discovery.validateDiscoveryState (startupDiscoveryState state) of
          Right () -> True
          Left _ -> False
    )
  let peerLiveness = startupPeerLivenessState state
      failureDetection = startupFailureDetectionState state
      failureWitness = FailureDetection.stateWitness failureDetection
      isolation = startupIsolationState state
      isolationWitness = Isolation.stateWitness isolation
      activeEpochs =
        Set.fromList (OracleProjection.oracleViewActiveHeraldEpochs projectionView)
      boundEpochs =
        Set.fromList
          ( fmap
              peerBindingRemoteHeraldEpoch
              (Discovery.currentPeerBindings (startupDiscoveryState state))
          )
      noRecoveriesDuringDrain =
        startupWitnessPhase (startupStateWitness state) == HeraldServing
          || Set.null (PeerLiveness.recoveryTargets peerLiveness)
  require
    StartupStatic
    PeerLivenessOwnerInvariant
    ( case PeerLiveness.validateState localEpoch activeEpochs peerLiveness of
        Left _ -> False
        Right () ->
          Set.null (Set.intersection boundEpochs (PeerLiveness.recoveryTargets peerLiveness))
            && noRecoveriesDuringDrain
    )
  -- Recovery grace and fresh-probe duration are independent policies; each
  -- owner validates its own positive duration. Shared authority must still agree.
  require
    StartupStatic
    FailureDetectionOwnerInvariant
    ( case FailureDetection.validateState failureDetection of
        Left _ -> False
        Right () ->
          failureWitness.witnessLocal == localEpoch
            && failureWitness.witnessVoterHosts
              == Isolation.isolationWitnessVoterHosts isolationWitness
            && failureWitness.witnessVoterQuorums
              == Isolation.isolationWitnessVoterQuorums isolationWitness
            && failureWitness.witnessVoterChangePending
              == (OracleProjection.oracleViewPendingVoterChange projectionView /= Nothing)
    )
  require
    StartupStatic
    IsolationOwnerInvariant
    ( case Isolation.validateState isolation of
        Left _ -> False
        Right () ->
          Isolation.isolationWitnessLocalHerald isolationWitness == localEpoch
            && case OracleProjection.oracleViewVoterConfiguration projectionView of
              Nothing -> True
              Just configuration ->
                let hosts = Set.fromList . map raftVoterBindingHeraldEpoch . Voter.oracleVoterBindingList
                    expected = case Voter.voterConfigurationView configuration of
                      Voter.StableVoterConfigurationView bindings -> (hosts bindings, Nothing)
                      Voter.JointVoterConfigurationView old new -> (hosts old, Just (hosts new))
                 in Isolation.isolationWitnessVoterQuorums isolationWitness == expected
    )
  require
    StartupStatic
    TerminalSourceHoldOwnerInvariant
    ( case TerminalSourceHold.validateState
        (OracleProjection.oracleViewHeraldMembershipHistoryChecked (OracleProjection.oracleView (startupOracleProjectionState state)))
        (startupTerminalSourceHoldState state) of
        Right () -> True
        Left _ -> False
    )
  witness <-
    case PeerStream.peerStreamStateWitness peerStream of
      Left _ -> Left (HeraldStartupInvariant StartupStatic PeerStreamOwnerInvariant)
      Right value -> Right value
  require
    StartupStatic
    PeerStreamOwnerInvariant
    ( PeerStream.peerStreamLocalEpoch peerStream == localEpoch
        && case PeerStream.validatePeerStreamState peerStream of
          Right () -> True
          Left _ -> False
        && all
          ( \direction ->
              streamDirectionSource direction == localEpoch
                && peerDirectionOwnerValid
                  (streamDirectionDestination direction)
          )
          (PeerStream.peerStreamWitnessOutgoingDirections witness)
        && all
          ( \direction ->
              streamDirectionDestination direction == localEpoch
                && peerDirectionOwnerValid
                  (streamDirectionSource direction)
          )
          (PeerStream.peerStreamWitnessIncomingDirections witness)
        && dispatchBindingsAgree state witness
    )
  require
    StartupStatic
    PlacementFactInvariant
    ( case Placement.validatePlacementState placement of
        Left _ -> False
        Right () ->
          all
            ( \peer ->
                Placement.remotePlacementOwnerRetired peer placement
                  == Map.notMember peer currentMembers
            )
            (filter (/= localEpoch) (Map.keys expectedCatalogue))
            && all
              ( \owner ->
                  Map.member owner currentMembers
                    && all
                      ( remotePlacementRouteIsValid
                          (checkedSystemId genesis)
                          owner
                          (OracleProjection.projectedBootstraps (startupOracleProjectionState state))
                          (startupGraphState state)
                          (startupStructuralProgressState state)
                      )
                      (Placement.remotePlacementRoutes owner placement)
              )
              (Placement.remotePlacementOwners placement)
    )

dispatchBindingsAgree ::
  HeraldState ->
  PeerStream.PeerStreamStateWitness ->
  Bool
dispatchBindingsAgree state witness =
  case (traverse observedBinding outgoingDirections, traverse observedAttempt outgoingDirections) of
    (Left _, _) -> False
    (_, Left _) -> False
    (Right observedMaybes, Right attempts) ->
      let observed = catMaybes observedMaybes
       in case startupWitnessPhase (startupStateWitness state) of
            HeraldServing -> Map.fromList observed == expected
            HeraldDraining -> null observed
            HeraldStopped -> null observed && all isNothing attempts
  where
    stream = startupPeerStreamState state
    outgoingDirections = PeerStream.peerStreamWitnessOutgoingDirections witness
    expected =
      Map.fromList
        [ ( peerBindingRemoteHeraldEpoch binding,
            peerBindingGenerationWord64 (peerBindingGeneration binding)
          )
        | binding <- Discovery.currentPeerBindings (startupDiscoveryState state)
        ]
    observedBinding direction = do
      binding <- PeerStream.currentDispatchBinding direction stream
      Right
        ( case binding of
            Nothing -> Nothing
            Just generation ->
              Just
                ( streamDirectionDestination direction,
                  peerDispatchBindingGenerationWord64 generation
                )
        )
    observedAttempt direction = PeerStream.currentDispatchAttempt direction stream

validateApplicationSessions ::
  HeraldState ->
  [ResidentApplicationAlias] ->
  Either HeraldInvariantFault ()
validateApplicationSessions state residents = do
  require
    StartupStatic
    ApplicationAttachmentInvariant
    (sort attachments == sort residents)
  traverse_ validateSession sessions
  require
    StartupStatic
    ApplicationSessionRetryIndexInvariant
    (sort retryIndex == sort expectedRetryIndex)
  require
    StartupStatic
    ApplicationSessionIdentityInvariant
    ( unique sessionIds
        && unique resumeTokens
        && unique bindings
    )
  require
    StartupStatic
    ApplicationSessionAllocationInvariant
    ( nextOrdinal >= 1
        && all
          ((>= 1) . ApplicationSession.applicationSessionIdOrdinal)
          sessionIds
        && all ((< nextOrdinal) . ApplicationSession.applicationSessionIdOrdinal) sessionIds
    )
  require
    StartupStatic
    ApplicationSessionRecoveryInvariant
    ( all validSessionRecovery sessionRecoveries
        && all
          (\session -> sessionWitnessProcess session `Set.member` recoveryProcesses)
          sessions
    )
  traverse_ validateProcessRecovery processRecoveries
  require
    StartupStatic
    ApplicationProcessRecoveryInvariant
    ( all
        (\process -> process `Set.notMember` residentProcesses || process `Set.member` sealedProcesses)
        automaticLossProcesses
    )
  where
    applicationState = startupApplicationState state
    Application.ApplicationStateWitness
      attachments
      sessions
      retryIndex
      nextOrdinal = Application.applicationStateWitness applicationState
    localEpoch = checkedLocalHeraldEpoch (startupGenesis state)
    residentProcesses = Set.fromList (fmap snd residents)
    sessionIds = fmap sessionWitnessId sessions
    resumeTokens = fmap sessionWitnessResumeToken sessions
    bindings = fmap sessionWitnessBinding sessions
    expectedRetryIndex =
      [ ((sessionWitnessProcess session, sessionWitnessNonce session), sessionWitnessId session)
      | session <- sessions
      ]
    sessionRecoveries = Application.applicationSessionRecoveryEntries applicationState
    processRecoveries = Application.applicationProcessRecoveryEntries applicationState
    recoveryProcesses =
      Set.fromList
        [ process
        | Application.ApplicationProcessRecoveryWitness process _ _ <- processRecoveries
        ]
    sealedProcesses =
      Set.fromList
        [ process
        | Application.ApplicationProcessRecoveryWitness
            process
            _
            (Application.ApplicationProcessSealedWitness _) <-
            processRecoveries
        ]
    automaticLossProcesses =
      [ process
      | (_, request) <-
          OracleClient.oracleClientRequestEntries (startupOracleClientState state),
        OracleClient.ProcessEndIntent process ApplicationPermanentlyLost <-
          [OracleClient.oracleRequestWitnessIntent request]
      ]
    validSessionRecovery
      (Application.ApplicationSessionRecoveryWitness session _ _ _) =
        case find ((== session) . sessionWitnessId) sessions of
          Just witness -> not (Application.applicationSessionWitnessDeliveryLive witness)
          Nothing -> False
    validateProcessRecovery
      (Application.ApplicationProcessRecoveryWitness process lastGeneration status) = do
        let ownedSessions = filter ((== process) . sessionWitnessProcess) sessions
            attached = any Application.applicationSessionWitnessDeliveryLive ownedSessions
            recovering =
              any
                (\(Application.ApplicationSessionRecoveryWitness _ _ _ _) -> True)
                [ recovery
                | recovery@(Application.ApplicationSessionRecoveryWitness session _ _ _) <-
                    sessionRecoveries,
                  any
                    (\witness -> sessionWitnessId witness == session && sessionWitnessProcess witness == process)
                    sessions
                ]
            automaticEndCount = length (filter (== process) automaticLossProcesses)
            coherent = case status of
              Application.ApplicationProcessAvailableWitness ->
                attached || not recovering
              Application.ApplicationProcessRecoveringWitness generation ->
                lastGeneration == Just generation
                  && not attached
                  && recovering
              Application.ApplicationProcessSealedWitness generation ->
                lastGeneration == Just generation
                  && null ownedSessions
                  && automaticEndCount == 1
        require
          (StartupProcess process)
          ApplicationProcessRecoveryInvariant
          ( process `Set.member` residentProcesses
              && coherent
          )
    validateSession session = do
      let process = sessionWitnessProcess session
          subject = StartupProcess process
      require
        subject
        ApplicationSessionSetInvariant
        ( process `Set.member` residentProcesses
            && lookup (sessionWitnessAttachment session) attachments == Just process
        )
      require
        subject
        ApplicationSessionIdentityInvariant
        ( ApplicationSession.applicationSessionIdHeraldEpoch
            (sessionWitnessId session)
            == localEpoch
            && ApplicationSession.applicationResumeTokenHeraldEpoch
              (sessionWitnessResumeToken session)
              == localEpoch
            && ApplicationSession.applicationSessionIdOrdinal
              (sessionWitnessId session)
              == ApplicationSession.applicationResumeTokenOrdinal
                (sessionWitnessResumeToken session)
            && ApplicationSession.sessionBindingSessionId
              (sessionWitnessBinding session)
              == sessionWitnessId session
        )
      expectedAccess <-
        maybe
          (fault subject ApplicationSessionAccessInvariant)
          ( either
              (const (fault subject ApplicationSessionAccessInvariant))
              Right
              . Application.applicationStartupAccessForBootstrap
          )
          (Application.applicationBootstrapAccess process applicationState)
      require
        subject
        ApplicationSessionAccessInvariant
        (sessionWitnessStartupAccess session == expectedAccess)

sessionWitnessId :: Application.ApplicationSessionWitness -> ApplicationSession.ApplicationSessionId
sessionWitnessId (Application.ApplicationSessionWitness session _ _ _ _ _ _ _) = session

sessionWitnessProcess :: Application.ApplicationSessionWitness -> ProcessEpochId
sessionWitnessProcess (Application.ApplicationSessionWitness _ process _ _ _ _ _ _) = process

sessionWitnessAttachment ::
  Application.ApplicationSessionWitness ->
  ApplicationSession.ApplicationAttachment
sessionWitnessAttachment (Application.ApplicationSessionWitness _ _ attachment _ _ _ _ _) = attachment

sessionWitnessNonce :: Application.ApplicationSessionWitness -> ApplicationSession.ClientNonce
sessionWitnessNonce (Application.ApplicationSessionWitness _ _ _ nonce _ _ _ _) = nonce

sessionWitnessResumeToken ::
  Application.ApplicationSessionWitness ->
  ApplicationSession.ApplicationResumeToken
sessionWitnessResumeToken (Application.ApplicationSessionWitness _ _ _ _ token _ _ _) = token

sessionWitnessBinding ::
  Application.ApplicationSessionWitness ->
  ApplicationSession.ApplicationSessionBinding
sessionWitnessBinding (Application.ApplicationSessionWitness _ _ _ _ _ binding _ _) = binding

sessionWitnessStartupAccess ::
  Application.ApplicationSessionWitness ->
  ApplicationStartupAccess
sessionWitnessStartupAccess (Application.ApplicationSessionWitness _ _ _ _ _ _ _ access) = access

unique :: (Ord value) => [value] -> Bool
unique values = length values == Set.size (Set.fromList values)

consistentCheckedPublications :: [CheckedPublication] -> Bool
consistentCheckedPublications publications =
  all
    consistent
    ( Map.elems
        ( Map.fromListWith
            (<>)
            [ (checkedPublicationId publication, [publication])
            | publication <- publications
            ]
        )
    )
  where
    consistent [] = True
    consistent (publication : rest) = all (== publication) rest

type RetainedRequestEntry =
  ( ApplicationSession.ApplicationSessionId,
    ApplicationRequest.RequestId,
    Application.ApplicationRequestWitness
  )

retainedRequestEntries :: HeraldState -> [RetainedRequestEntry]
retainedRequestEntries state =
  [ (session, request, witness)
  | (session, _) <- Application.applicationRequestOwnerEntries applicationState,
    (request, witness) <- Application.applicationRequestEntries session applicationState
  ]
  where
    applicationState = startupApplicationState state

type RetainedPublicationRequestEntry =
  ( ApplicationSession.ApplicationSessionId,
    ApplicationRequest.RequestId,
    Application.ApplicationPublicationRequestWitness
  )

retainedPublicationRequestEntries :: HeraldState -> [RetainedPublicationRequestEntry]
retainedPublicationRequestEntries state =
  [ (session, request, witness)
  | (session, _) <- Application.applicationRequestOwnerEntries applicationState,
    (request, witness) <-
      Application.applicationPublicationRequestEntries session applicationState
  ]
  where
    applicationState = startupApplicationState state

validateApplicationRequests ::
  HeraldState ->
  [ResidentApplicationAlias] ->
  Either HeraldInvariantFault ()
validateApplicationRequests state residents = do
  require
    StartupStatic
    ApplicationRequestOwnerInvariant
    ( unique ownerKeys
        && Set.fromList ownerKeys == Set.fromList sessionKeys
        && all (\(key, owner) -> key == Application.applicationRequestOwnerSession owner) owners
    )
  require
    StartupStatic
    ApplicationAcceptanceAllocationInvariant
    ( unique (fmap fst nextAcceptanceEntries)
        && Map.keysSet nextAcceptanceByProcess == residentProcesses
        && all (>= 1) (Map.elems nextAcceptanceByProcess)
    )
  traverse_ validateOwner owners
  require
    StartupStatic
    ApplicationRequestPositionInvariant
    ( unique
        [ (process, ApplicationRequest.processAcceptancePositionOrdinal position)
        | (process, position) <-
            [ ( Application.applicationRequestWitnessProcess request,
                position
              )
            | (_, _, request) <- requests,
              Just position <- [Application.applicationRequestWitnessPosition request]
            ]
              <> [ ( Application.applicationPublicationRequestWitnessProcess request,
                     Application.applicationPublicationRequestWitnessPosition request
                   )
                 | (_, _, request) <- publicationRequests
                 ]
              <> [ ( ApplicationRequest.processAcceptancePositionProcess position,
                     position
                   )
                 | held <- Application.applicationFenceHeldEntries applicationState,
                   let position = Application.applicationFenceHeldPosition held
                 ]
        ]
    )
  where
    applicationState = startupApplicationState state
    sessionWitnesses =
      Application.applicationWitnessSessions
        (Application.applicationStateWitness applicationState)
    sessionKeys = fmap Application.applicationSessionWitnessId sessionWitnesses
    sessionsById =
      Map.fromList
        [ (Application.applicationSessionWitnessId session, session)
        | session <- sessionWitnesses
        ]
    owners = Application.applicationRequestOwnerEntries applicationState
    ownerKeys = fmap fst owners
    requests = retainedRequestEntries state
    publicationRequests = retainedPublicationRequestEntries state
    nextAcceptanceEntries =
      Application.applicationRequestNextAcceptancePositions
        (Application.applicationRequestStateWitness applicationState)
    nextAcceptanceByProcess = Map.fromList nextAcceptanceEntries
    residentProcesses = Set.fromList (fmap snd residents)
    isolationWitness = Isolation.stateWitness (startupIsolationState state)

    validateOwner (ownerKey, owner) = do
      session <-
        maybe
          (fault StartupStatic ApplicationRequestOwnerInvariant)
          Right
          (Map.lookup ownerKey sessionsById)
      let process = Application.applicationSessionWitnessProcess session
          subject = StartupProcess process
          entries = Application.applicationRequestEntries ownerKey applicationState
          publicationEntries =
            Application.applicationPublicationRequestEntries ownerKey applicationState
          bodyEntries =
            Application.applicationRequestReplyBodyEntries ownerKey applicationState
          bodiesByRequest = Map.fromList bodyEntries
          requestCursors =
            fmap
              ( ApplicationRequest.applicationReplyCursorWord64
                  . Application.applicationRequestWitnessCursor
                  . snd
              )
              entries
          bindingCursor =
            ApplicationRequest.applicationReplyCursorWord64
              (Application.applicationRequestOwnerBindingEstablishedBy owner)
          lastCursor =
            ApplicationRequest.applicationReplyCursorWord64
              (Application.applicationRequestOwnerLastIssuedReplyCursor owner)
          retiredCursors =
            fmap
              ApplicationRequest.applicationReplyCursorWord64
              (maybe [] pure (Application.applicationRequestOwnerRetiredReplyCursor owner))
          requestCursorMaximum = maximum (bindingCursor : requestCursors <> retiredCursors)
          isolationNoticeMayOwnLastCursor =
            Isolation.isolationWitnessPhase isolationWitness
              == Isolation.IsolationReadOnlyDrainView
              && process
                `Set.member` Isolation.isolationWitnessResidentProcessOverlay isolationWitness
      require
        subject
        ApplicationRequestIdentityInvariant
        ( unique (fmap fst entries)
            && unique (fmap fst publicationEntries)
            && Set.disjoint
              (Set.fromList (fmap fst entries))
              (Set.fromList (fmap fst publicationEntries))
            && fmap fst entries == fmap fst bodyEntries
            && all (\(request, _) -> Application.applicationRequestRetirement ownerKey request applicationState == Nothing) publicationEntries
            && all
              (\(key, request) -> key == Application.applicationRequestWitnessId request)
              entries
            && all
              ( \(key, request) ->
                  key == Application.applicationPublicationRequestWitnessId request
              )
              publicationEntries
        )
      require
        subject
        ApplicationReplyCursorInvariant
        ( bindingCursor >= 1
            && bindingCursor <= lastCursor
            && all (\cursor -> cursor >= 1 && cursor <= lastCursor) (requestCursors <> retiredCursors)
            && all (\(request, _) -> Application.applicationRequestRetirement ownerKey request applicationState == Nothing) entries
            && unique (bindingCursor : requestCursors)
            && ( lastCursor == requestCursorMaximum
                   || ( isolationNoticeMayOwnLastCursor
                          && lastCursor == requestCursorMaximum + 1
                      )
               )
        )
      traverse_ (validateRequest ownerKey process bodiesByRequest) entries
      traverse_ (validatePublicationRequest process) publicationEntries

    validateRequest session process bodiesByRequest (requestKey, request) = do
      let subject = StartupProcess process
          witnessedProcess = Application.applicationRequestWitnessProcess request
          status = Application.applicationRequestWitnessStatus request
          call = Application.applicationRequestWitnessCall request
          position = Application.applicationRequestWitnessPosition request
      body <-
        maybe
          (fault subject ApplicationRequestIdentityInvariant)
          Right
          (Map.lookup requestKey bodiesByRequest)
      require
        subject
        ApplicationRequestProcessInvariant
        (witnessedProcess == process)
      require
        subject
        ApplicationRequestIdentityInvariant
        ( ApplicationRequest.retainedReplyBodyRequestId body == requestKey
            && ApplicationRequest.retainedReplyBodyStatus body == status
            && retainedWaitIdentityMatches session requestKey body
        )
      require
        subject
        ApplicationRequestStatusInvariant
        ( requestStatusMatchesCall call status
            && requestStatusPositionConsistent status position
        )
      case position of
        Nothing -> Right ()
        Just accepted -> do
          next <-
            maybe
              (fault subject ApplicationAcceptanceAllocationInvariant)
              Right
              (Map.lookup process nextAcceptanceByProcess)
          require
            subject
            ApplicationRequestPositionInvariant
            ( ApplicationRequest.processAcceptancePositionProcess accepted == process
                && ApplicationRequest.processAcceptancePositionOrdinal accepted >= 1
                && ApplicationRequest.processAcceptancePositionOrdinal accepted < next
            )
      case status of
        ApplicationRequest.ApplicationRequestPending wait ->
          require
            subject
            ApplicationRequestIdentityInvariant
            ( ApplicationRequest.waitIdHeraldEpoch wait
                == ApplicationSession.applicationSessionIdHeraldEpoch session
                && ApplicationRequest.waitIdSessionOrdinal wait
                  == ApplicationSession.applicationSessionIdOrdinal session
                && ApplicationRequest.waitIdRequestId wait == requestKey
            )
        _ -> Right ()

    validatePublicationRequest process (requestKey, request) = do
      let subject = StartupProcess process
          witnessedProcess =
            Application.applicationPublicationRequestWitnessProcess request
          position =
            Application.applicationPublicationRequestWitnessPosition request
      next <-
        maybe
          (fault subject ApplicationAcceptanceAllocationInvariant)
          Right
          (Map.lookup process nextAcceptanceByProcess)
      record <-
        maybe
          (fault subject ApplicationRequestIdentityInvariant)
          Right
          ( Publication.lookupApplicationPublication
              position
              (startupPublicationState state)
          )
      effective <-
        maybe
          (fault subject PublicationRecordInvariant)
          Right
          ( lookupRetainedRegistryEntryAt
              (checkedPublicationSort (Publication.applicationPublicationChecked record))
              (Publication.applicationPublicationSortOccurrenceId record)
              (startupSortRegistryState state)
          )
      let roles =
            Application.processRoleView
              ( OracleProjection.projectedProcessEpochs
                  (startupOracleProjectionState state)
              )
      require
        subject
        ApplicationRequestProcessInvariant
        (witnessedProcess == process)
      require
        subject
        ApplicationRequestPositionInvariant
        ( ApplicationRequest.processAcceptancePositionProcess position == process
            && ApplicationRequest.processAcceptancePositionOrdinal position >= 1
            && ApplicationRequest.processAcceptancePositionOrdinal position < next
            && Publication.applicationPublicationSourceProcess record == process
            && Publication.applicationPublicationAcceptancePosition record == position
        )
      require
        subject
        ApplicationRequestIdentityInvariant
        ( requestKey
            == Application.applicationPublicationRequestWitnessId request
            && ApplicationPublication.applicationPublicationRequestMatchesRecord
              roles
              request
              (SortRegistry.registryEntryDescriptor effective)
              applicationState
              record
        )

-- | Cross-check the package-private environment request owner, Publication's
-- immutable range indexes, and Controlled's accepted target observations.
-- Accepted manifests remain valid after their session or process owner is
-- retired; in that case only the optional reply key and effective possession
-- are absent.
-- | Resolve the descriptor retained at one exact public sort occurrence.
-- Completed transcripts may legitimately name a retired occurrence after the
-- Registry has advanced to its successor; active old-occurrence work is still
-- rejected independently by the regular-retirement residual invariant.
lookupRetainedRegistryEntryAt ::
  SortId ->
  SortDefinitionOccurrenceId ->
  SortRegistry.State ->
  Maybe SortRegistry.RegistryEntry
lookupRetainedRegistryEntryAt sortId occurrence registry =
  case SortRegistry.lookupEffectiveSort sortId registry of
    Just entry
      | SortRegistry.registryEntryOccurrenceId entry == occurrence -> Just entry
    _ ->
      SortRegistry.lookupRegularSortRetirement sortId occurrence registry
        >>= SortRegistry.regularSortRetirementEntry

validatePrivateEnvironments :: HeraldState -> Either HeraldInvariantFault ()
validatePrivateEnvironments state = do
  require
    StartupStatic
    EnvironmentOwnerInvariant
    ( Publication.publicationEnvironmentRangesWellFormed publication
        && unique (fmap fst pendingEntries)
        && unique (fmap fst completedEntries)
        && Map.keysSet pendingManifests `Set.disjoint` Map.keysSet completedManifests
        && all validPendingEntry pendingEntries
        && all validCompletedEntry completedEntries
        && retainedManifests == publicationManifests
        && Set.disjoint environmentCandidateRequestKeys otherRequestKeys
        && Set.disjoint environmentPositions otherAcceptedPositions
        && all validEnvironmentRequest environmentRequests
        && all validPendingReply pendingEntries
        && all validCompletedReply completedEntries
        && all validCompletedAccess completedEntries
    )
  traverse_ validateManifest (Map.elems retainedManifests)
  where
    application = startupApplicationState state
    publication = startupPublicationState state
    controlled = startupControlledState state
    oracleView = OracleProjection.oracleView (startupOracleProjectionState state)
    owners = Application.applicationRequestOwnerEntries application
    sessionProcesses =
      Map.fromList
        [ ( Application.applicationSessionWitnessId session,
            Application.applicationSessionWitnessProcess session
          )
        | session <-
            Application.applicationWitnessSessions
              (Application.applicationStateWitness application)
        ]
    environmentRequests =
      [ (session, request, witness)
      | (session, _) <- owners,
        (request, witness) <-
          Application.applicationEnvironmentRequestEntries session application
      ]
    environmentCandidateRequestKeys =
      Set.fromList
        [ (session, request)
        | (session, request, witness) <- environmentRequests,
          Application.environmentRequestWitnessStage witness
            == Application.EnvironmentRequestCandidateStage
        ]
    ordinaryRequestKeys =
      Set.fromList [(session, request) | (session, request, _) <- retainedRequestEntries state]
    publicationRequestKeys =
      Set.fromList
        [ (session, request)
        | (session, request, _) <- retainedPublicationRequestEntries state
        ]
    detachedRequestKeys =
      Set.fromList
        ( [ ( Application.applicationRequestKeySession key,
              Application.applicationRequestKeyRequest key
            )
          | candidate <- Application.applicationLabelCandidateEntries application,
            let key = Application.applicationLabelCandidateKey candidate
          ]
            <> [ ( Application.applicationRequestKeySession key,
                   Application.applicationRequestKeyRequest key
                 )
               | gated <- Application.applicationGatedIngressEntries application,
                 let key = Application.applicationGatedIngressKey gated
               ]
            <> [ ( Application.applicationRequestKeySession key,
                   Application.applicationRequestKeyRequest key
                 )
               | held <- Application.applicationFenceHeldEntries application,
                 Just key <- [Application.applicationFenceHeldReplyKey held]
               ]
        )
    otherRequestKeys =
      ordinaryRequestKeys
        `Set.union` publicationRequestKeys
        `Set.union` detachedRequestKeys
    pendingEntries = Application.applicationPendingEnvironmentEntries application
    pendingManifests =
      Map.fromList
        [ (position, Environment.pendingEnvironmentManifest pending)
        | (position, pending) <- pendingEntries
        ]
    completedEntries = Application.applicationCompletedEnvironmentEntries application
    completedManifests =
      Map.fromList
        [ (position, Environment.completedEnvironmentManifest completed)
        | (position, completed) <- completedEntries
        ]
    retainedManifests = pendingManifests `Map.union` completedManifests
    publicationManifests =
      Map.fromList
        [ (Environment.environmentManifestPosition manifest, manifest)
        | (_, manifest) <- Publication.environmentManifestEntries publication
        ]
    environmentPositions = Map.keysSet retainedManifests
    otherAcceptedPositions =
      Set.fromList
        ( [ position
          | (_, _, request) <- retainedRequestEntries state,
            Application.applicationRequestWitnessCall request
              /= NewEnvironmentApplication,
            Just position <- [Application.applicationRequestWitnessPosition request]
          ]
            <> [ Application.applicationPublicationRequestWitnessPosition request
               | (_, _, request) <- retainedPublicationRequestEntries state
               ]
            <> [ Application.applicationFenceHeldPosition held
               | held <- Application.applicationFenceHeldEntries application
               ]
        )
    nextByProcess =
      Map.fromList
        ( Application.applicationRequestNextAcceptancePositions
            (Application.applicationRequestStateWitness application)
        )

    validEnvironmentRequest (session, request, witness) =
      Application.applicationRequestRetirement session request application == Nothing
        && Map.lookup session sessionProcesses
          == Just (Application.environmentRequestWitnessProcess witness)
        && Application.environmentRequestWitnessId witness == request
        && case Application.environmentRequestWitnessStage witness of
          Application.EnvironmentRequestCandidateStage -> True
          Application.EnvironmentRequestAcceptedStage position ->
            let process = Application.environmentRequestWitnessProcess witness
                reply = Environment.environmentReplyKey session request
             in ApplicationRequest.processAcceptancePositionProcess position == process
                  && maybe
                    False
                    (\next -> ApplicationRequest.processAcceptancePositionOrdinal position < next)
                    (Map.lookup process nextByProcess)
                  && case Map.lookup position pendingManifests of
                    Nothing -> False
                    Just manifest ->
                      Environment.environmentManifestProcess manifest == process
                        && any
                          ( \(retainedPosition, pending) ->
                              retainedPosition == position
                                && Environment.pendingEnvironmentReplyKey pending == Just reply
                          )
                          pendingEntries
          Application.EnvironmentRequestCompletedStage position ->
            let process = Application.environmentRequestWitnessProcess witness
                reply = Environment.environmentReplyKey session request
             in ApplicationRequest.processAcceptancePositionProcess position == process
                  && maybe
                    False
                    (\next -> ApplicationRequest.processAcceptancePositionOrdinal position < next)
                    (Map.lookup process nextByProcess)
                  && case Map.lookup position completedManifests of
                    Nothing -> False
                    Just manifest ->
                      Environment.environmentManifestProcess manifest == process
                        && any
                          ( \(retainedPosition, completed) ->
                              retainedPosition == position
                                && Environment.completedEnvironmentReplyKey completed == Just reply
                          )
                          completedEntries

    validPendingReply (_, pending) =
      case Environment.pendingEnvironmentReplyKey pending of
        Nothing -> True
        Just reply ->
          any
            ( \(session, request, witness) ->
                session == Environment.environmentReplyKeySession reply
                  && request == Environment.environmentReplyKeyRequest reply
                  && case Application.environmentRequestWitnessStage witness of
                    Application.EnvironmentRequestAcceptedStage position ->
                      position
                        == Environment.environmentManifestPosition
                          (Environment.pendingEnvironmentManifest pending)
                    Application.EnvironmentRequestCandidateStage -> False
                    Application.EnvironmentRequestCompletedStage {} -> False
            )
            environmentRequests

    validPendingEntry (position, pending) =
      Environment.environmentManifestPosition
        (Environment.pendingEnvironmentManifest pending)
        == position

    validCompletedReply (_, completed) =
      case Environment.completedEnvironmentReplyKey completed of
        Nothing -> True
        Just reply ->
          any
            ( \(session, request, witness) ->
                session == Environment.environmentReplyKeySession reply
                  && request == Environment.environmentReplyKeyRequest reply
                  && case Application.environmentRequestWitnessStage witness of
                    Application.EnvironmentRequestCompletedStage position ->
                      position
                        == Environment.environmentManifestPosition
                          (Environment.completedEnvironmentManifest completed)
                    Application.EnvironmentRequestCandidateStage -> False
                    Application.EnvironmentRequestAcceptedStage {} -> False
            )
            environmentRequests

    validCompletedEntry (position, completed) =
      let manifest = Environment.completedEnvironmentManifest completed
       in Environment.environmentManifestPosition manifest == position
            && ( Environment.environmentManifestComplete manifest
                   || ( Controlled.controlledProcessEnded
                          (Environment.environmentManifestProcess manifest)
                          controlled
                          && Environment.completedEnvironmentAccess completed == Nothing
                          && Environment.completedEnvironmentReplyKey completed == Nothing
                      )
               )
            && StructuralSettlement.environmentManifestSettlementEvidence
              completed
              state
              == Right True

    validCompletedAccess (_, completed) =
      case Environment.completedEnvironmentAccess completed of
        Nothing ->
          Environment.completedEnvironmentReplyKey completed == Nothing
            && Application.applicationBootstrapAccess process application == Nothing
        Just localized ->
          let accesses = environmentAccessPredefined localized
              privateIds =
                concatMap
                  ( \access ->
                      [ privateNablaUniqueId (predefinedWriter access),
                        privateDeltaUniqueId (predefinedReader access)
                      ]
                  )
                  accesses
                  <> fmap privateObjectUniqueId (environmentAccessHub localized : environmentAccessEdges localized)
              resolvedGlobals =
                traverse
                  ( \privateIdentity ->
                      either
                        (const Nothing)
                        Just
                        ( Application.resolveApplicationPrivateUniqueId
                            process
                            privateIdentity
                            application
                        )
                  )
                  privateIds
           in isJust (Application.applicationBootstrapAccess process application)
                && fmap predefinedAccessRole accesses == allApplicationPredefinedSortRoles
                && resolvedGlobals == Just expectedGlobals
      where
        manifest = Environment.completedEnvironmentManifest completed
        process = Environment.environmentManifestProcess manifest
        expectedGlobals =
          [ globalUniqueIdFromGlobalObjectId
              ( Environment.environmentRootPlanTargetObject
                  (Environment.positionedEnvironmentRootPlan root)
              )
          | root <- NonEmpty.toList (Environment.environmentManifestRoots manifest)
          ]

    validateManifest manifest = do
      let process = Environment.environmentManifestProcess manifest
          subject = StartupProcess process
      membership <-
        maybe
          (fault subject EnvironmentOwnerInvariant)
          Right
          ( OracleProjection.oracleViewHeraldMembershipById
              (Environment.environmentManifestMembershipGenerationId manifest)
              oracleView
          )
      require
        subject
        EnvironmentOwnerInvariant
        ( heraldMembershipGenerationActiveMemberSetDigest membership
            == Environment.environmentManifestActiveMemberSetDigest manifest
        )
      traverse_
        ( validateRoot
            subject
            process
            (Set.fromList (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership)))
        )
        (NonEmpty.toList (Environment.environmentManifestRoots manifest))

    validateRoot subject process activeMembers root = do
      let plan = Environment.positionedEnvironmentRootPlan root
          checked = Environment.positionedEnvironmentRootChecked root
          identifier = Environment.positionedEnvironmentRootPublicationId root
          target = Environment.environmentRootPlanTargetObject plan
          destinations = routeDestinations (Environment.environmentRootPlanRoute plan)
          routedMembers = Set.fromList (destinationHerald <$> destinations)
          normallyRoutedMembers =
            Set.fromList
              [ destinationHerald destination
              | destination <- destinations,
                destinationStrength destination == Normal
              ]
          role =
            environmentRootSlotStructuralCarrierRole
              (Environment.environmentRootPlanSlot plan)
      require
        subject
        EnvironmentOwnerInvariant
        ( Publication.lookupPositionedEnvironmentRoot identifier publication == Just root
            && Environment.environmentRootPlanSourceStrength plan == Normal
            && normallyRoutedMembers == routedMembers
            && routedMembers `Set.isSubsetOf` activeMembers
            && case Controlled.controlledLocalRecord target controlled of
              Just record ->
                Controlled.controlledTerminalDeletion target controlled == Nothing
                  && Controlled.controlledRecordFirstPublication record == checked
                  && Controlled.controlledRecordObservation identifier record == Just checked
                  && Controlled.controlledRecordSortId record == checkedPublicationSort checked
                  && Controlled.controlledRecordOccurrenceId record
                    == Environment.environmentRootPlanOccurrenceId plan
                  && Controlled.controlledRecordStructuralRole record == Just role
                  && Controlled.controlledRecordObservedLabel record == Just (ProcessLabel process, 0)
                  && if Controlled.controlledProcessEnded process controlled
                    then controlledPossessionEntriesAbsent process target controlled
                    else Controlled.controlledHasNormalPossession process target controlled
              Nothing ->
                isJust (Controlled.controlledTerminalDeletion target controlled)
                  && not (Controlled.controlledHasAnyNormalPossession target controlled)
        )

-- | Cross-check the process-wide application request classes introduced by
-- the live label fence. Detached candidates and gated ingress have not yet
-- consumed a process position; fence-held work has consumed one but has no
-- ordinary reply owner until release. The four key classes must therefore be
-- disjoint, while an attached held entry must still name its exact live
-- session/process owner.
validateApplicationLabelWork ::
  HeraldState ->
  Either HeraldInvariantFault ()
validateApplicationLabelWork state = do
  require
    StartupStatic
    ApplicationDetachedRequestInvariant
    ( unique candidateKeys
        && unique gatedKeys
        && unique attachedHeldKeys
        && unique ordinaryKeys
        && pairwiseDisjoint
          [ Set.fromList candidateKeys,
            Set.fromList gatedKeys,
            Set.fromList attachedHeldKeys,
            Set.fromList ordinaryKeys
          ]
        && all candidateNamesLiveOwner candidates
        && all gatedNamesLiveOwner gated
    )
  traverse_ validateHeld held
  validateActive
  where
    application = startupApplicationState state
    projection = OracleProjection.oracleView (startupOracleProjectionState state)
    candidates = Application.applicationLabelCandidateEntries application
    gated = Application.applicationGatedIngressEntries application
    held = Application.applicationFenceHeldEntries application
    candidateKeys = fmap Application.applicationLabelCandidateKey candidates
    gatedKeys = fmap Application.applicationGatedIngressKey gated
    attachedHeldKeys =
      catMaybes (fmap Application.applicationFenceHeldReplyKey held)
    ordinaryKeys =
      [ Application.applicationRequestKey session request
      | (session, request, _) <- retainedRequestEntries state
      ]
        <> [ Application.applicationRequestKey session request
           | (session, request, _) <- retainedPublicationRequestEntries state
           ]
    nextAcceptance =
      Map.fromList
        ( Application.applicationRequestNextAcceptancePositions
            (Application.applicationRequestStateWitness application)
        )

    candidateNamesLiveOwner entry =
      keyNamesLiveOwner
        (Application.applicationLabelCandidateKey entry)
        (Application.applicationLabelCandidateProcess entry)

    gatedNamesLiveOwner entry =
      keyNamesLiveOwner
        (Application.applicationGatedIngressKey entry)
        (Application.applicationGatedIngressProcess entry)

    keyNamesLiveOwner key process =
      Application.applicationSessionProcess
        (Application.applicationRequestKeySession key)
        application
        == Just process
        && Application.applicationRequestRetirement
          (Application.applicationRequestKeySession key)
          (Application.applicationRequestKeyRequest key)
          application
          == Nothing
        && OracleProjection.oracleViewProcessIsLive process projection

    validateHeld entry = do
      let position = Application.applicationFenceHeldPosition entry
          process = ApplicationRequest.processAcceptancePositionProcess position
          ordinal = ApplicationRequest.processAcceptancePositionOrdinal position
          subject = StartupProcess process
          attached = Application.applicationFenceHeldReplyKey entry
          groupMembership =
            Application.applicationFenceHeldWorkGroupMembership
              (Application.applicationFenceHeldSemanticWork entry)
      require
        subject
        ApplicationFenceHeldInvariant
        ( ordinal >= 1
            && heldPositionWasAllocated process ordinal
            && heldOperationMatchesOrigin entry
            && all ((== process) . PublicationGroups.groupProcess) (PublicationGroups.membershipKeys groupMembership)
            && all (heldGateMatches entry) (Set.toList (Application.applicationFenceHeldDisappearanceProbes entry))
            && maybe True (`keyNamesLiveOwner` process) attached
        )

    heldGateMatches entry probe =
      case Disappearance.probeWitness probe (startupDisappearanceState state) of
        Nothing -> False
        Just witness ->
          let work = Application.applicationFenceHeldSemanticWork entry
           in OwnerEvidence.canonicalPublicationIsSubjectRelevant
                (DisappearanceProtocol.projectedProbeSubject witness.witnessProjectedProbe)
                (Application.applicationFenceHeldWorkSortId work)
                (Application.applicationFenceHeldWorkSortOccurrenceId work)
                (canonicalValueBytes (Publication.admittedApplicationPublicationValue (Application.applicationFenceHeldWorkAdmittedValue work)))
                && (witness.witnessInvalidationIntention /= Nothing || witness.witnessPhase /= Disappearance.ProbeCollectingView)

    heldPositionWasAllocated process ordinal =
      case Map.lookup process nextAcceptance of
        Just next -> ordinal < next
        Nothing ->
          isJust (OracleProjection.oracleViewEndedProcess process projection)

    heldOperationMatchesOrigin entry =
      case ( Application.applicationFenceHeldOperation entry,
             Application.applicationFenceHeldWorkOrigin
               (Application.applicationFenceHeldSemanticWork entry)
           ) of
        (ForwardApplication {}, Publication.ForwardApplicationPublicationOrigin {}) -> True
        (WriteApplication _ (PublishValue _), Publication.ForwardApplicationPublicationOrigin {}) -> False
        (WriteApplication _ (PublishValue _), _) -> True
        _ -> False

    validateActive = do
      require StartupStatic ApplicationActiveLabelInvariant (Application.applicationLabelIndexesValid application)
      mapM_ validateOneActive (Application.applicationActiveLabels application)
    validateOneActive active =
      let key = Application.activeLabelApplicationKey active
          session = Application.applicationRequestKeySession key
          request = Application.applicationRequestKeyRequest key
          position = Application.activeLabelApplicationPosition active
       in case Application.applicationSessionProcess session application of
            Nothing -> Right ()
            Just process ->
              require
                (StartupProcess process)
                ApplicationActiveLabelInvariant
                ( ApplicationRequest.processAcceptancePositionProcess position
                    == process
                    && case lookup request (Application.applicationRequestEntries session application) of
                      Nothing -> False
                      Just witness ->
                        Application.applicationRequestWitnessProcess witness == process
                          && Application.applicationRequestWitnessCall witness
                            == Application.labelApplicationOperation
                              (Application.activeLabelApplicationCall active)
                          && Application.applicationRequestWitnessPosition witness
                            == Just position
                )

pairwiseDisjoint :: (Ord value) => [Set.Set value] -> Bool
pairwiseDisjoint [] = True
pairwiseDisjoint (values : rest) =
  all (Set.disjoint values) rest && pairwiseDisjoint rest

retainedWaitIdentityMatches ::
  ApplicationSession.ApplicationSessionId ->
  ApplicationRequest.RequestId ->
  ApplicationRequest.RetainedRequestReplyBody ->
  Bool
retainedWaitIdentityMatches session request body = case body of
  ApplicationRequest.WaitAccepted _ wait -> matches wait
  ApplicationRequest.Cancelled _ wait -> matches wait
  ApplicationRequest.OperationAccepted {} -> True
  ApplicationRequest.Completed {} -> True
  ApplicationRequest.Rejected {} -> True
  where
    matches wait =
      ApplicationRequest.waitIdHeraldEpoch wait
        == ApplicationSession.applicationSessionIdHeraldEpoch session
        && ApplicationRequest.waitIdSessionOrdinal wait
          == ApplicationSession.applicationSessionIdOrdinal session
        && ApplicationRequest.waitIdRequestId wait == request

requestStatusPositionConsistent ::
  ApplicationRequest.ApplicationRequestStatus ->
  Maybe ApplicationRequest.ProcessAcceptancePosition ->
  Bool
requestStatusPositionConsistent status position = case status of
  -- A rejection before semantic admission has no position. Revalidation of
  -- already positioned fence-held work may instead reject while retaining its
  -- original acceptance position.
  ApplicationRequest.ApplicationRequestRejected _ -> True
  ApplicationRequest.ApplicationRequestPending _ -> isJust position
  ApplicationRequest.ApplicationOperationAccepted _ -> isJust position
  ApplicationRequest.ApplicationRequestCompleted _ -> isJust position
  ApplicationRequest.ApplicationRequestCancelled -> isJust position

requestStatusMatchesCall ::
  ApplicationOperation ->
  ApplicationRequest.ApplicationRequestStatus ->
  Bool
requestStatusMatchesCall call status = case (call, status) of
  (_, ApplicationRequest.ApplicationRequestRejected _) -> True
  (WaitApplication _, ApplicationRequest.ApplicationRequestPending _) -> True
  (WaitApplication _, ApplicationRequest.ApplicationRequestCancelled) -> True
  ( WaitApplication _,
    ApplicationRequest.ApplicationRequestCompleted (WaitCompleted _)
    ) -> True
  ( NewIdApplication _,
    ApplicationRequest.ApplicationRequestCompleted (NewIdCompleted _)
    ) -> True
  ( WriteApplication _ (PublishValue (SortDefinitionValue _)),
    ApplicationRequest.ApplicationRequestCompleted
      (WriteCompleted (SortDefinitionWritten _))
    ) -> True
  ( WriteApplication _ (PublishValue value),
    ApplicationRequest.ApplicationRequestCompleted
      (WriteCompleted WriteAccepted)
    ) -> case value of
      SortDefinitionValue _ -> False
      _ -> True
  ( WriteApplication _ (PublishValue _),
    ApplicationRequest.ApplicationOperationAccepted _
    ) -> True
  ( WriteApplication _ (DeleteReserved _),
    ApplicationRequest.ApplicationRequestCompleted
      (WriteCompleted WriteAccepted)
    ) -> True
  ( ForwardApplication _ _,
    ApplicationRequest.ApplicationRequestCompleted
      (ForwardCompleted ForwardAccepted)
    ) -> True
  ( ForwardApplication _ _,
    ApplicationRequest.ApplicationOperationAccepted _
    ) -> True
  ( ReadApplication _,
    ApplicationRequest.ApplicationRequestCompleted (ReadCompleted _)
    ) -> True
  ( LocalTakeApplication _,
    ApplicationRequest.ApplicationRequestCompleted (LocalTakeCompleted _)
    ) -> True
  ( LabelApplication {},
    ApplicationRequest.ApplicationOperationAccepted LabelSettlementPending
    ) -> True
  ( LabelApplication {},
    ApplicationRequest.ApplicationRequestCompleted (LabelCompleted _)
    ) -> True
  ( NewEnvironmentApplication,
    ApplicationRequest.ApplicationOperationAccepted EnvironmentStabilizationPending
    ) -> True
  ( NewEnvironmentApplication,
    ApplicationRequest.ApplicationRequestCompleted (NewEnvironmentCompleted _)
    ) -> True
  _ -> False

-- End masks effective possession immediately; inspect the retained ownership
-- entries themselves when checking that its revocation also removed them.
controlledPossessionEntriesAbsent :: ProcessEpochId -> GlobalObjectId -> Controlled.State -> Bool
controlledPossessionEntriesAbsent process object controlled =
  not (Controlled.controlledHasDirectPossession process object controlled)
    && all
      ( \witness ->
          Controlled.controlledStorePossessionWitnessProcess witness /= process
            || Controlled.controlledStorePossessionWitnessObject witness /= object
      )
      (Controlled.controlledStorePossessionWitnesses controlled)

validateGeneratedIdentities :: HeraldState -> Either HeraldInvariantFault ()
validateGeneratedIdentities state = do
  traverse_ validateCompletedNewId requests
  traverse_ validateReservation reservations
  traverse_ validateCompletedDelete requests
  where
    applicationState = startupApplicationState state
    controlledState = startupControlledState state
    structuralProgress = startupStructuralProgressState state
    reservations = Controlled.controlledReservationWitnesses controlledState
    requests = retainedRequestEntries state

    validateCompletedNewId (_, _, request) =
      case ( Application.applicationRequestWitnessCall request,
             Application.applicationRequestWitnessStatus request
           ) of
        ( NewIdApplication target,
          ApplicationRequest.ApplicationRequestCompleted
            (NewIdCompleted privateIdentity)
          ) -> do
            let process = Application.applicationRequestWitnessProcess request
                subject = StartupProcess process
            generated <- resolvePrivate subject process privateIdentity
            case target of
              BareNewId ->
                require
                  subject
                  ControlledReservationInvariant
                  (Controlled.controlledReservationWitness generated controlledState == Nothing)
              ControlledNewId privateNabla -> do
                nablaGlobal <-
                  resolvePrivate subject process (privateNablaUniqueId privateNabla)
                let nabla =
                      nablaIdFromGlobalObjectId
                        (globalObjectIdFromGlobalUniqueId nablaGlobal)
                case Controlled.controlledReservationWitness generated controlledState of
                  Just reservation ->
                    require
                      subject
                      ControlledReservationInvariant
                      ( Controlled.reservationWitnessProcess reservation == process
                          && Controlled.reservationWitnessNabla reservation == nabla
                      )
                  Nothing ->
                    require
                      subject
                      ControlledReservationInvariant
                      ( isJust
                          ( Controlled.controlledTerminalDeletion
                              (globalObjectIdFromNablaId nabla)
                              controlledState
                          )
                          && Controlled.controlledLocalRecord
                            (globalObjectIdFromGlobalUniqueId generated)
                            controlledState
                            == Nothing
                          && not
                            ( Controlled.controlledHasAnyNormalPossession
                                (globalObjectIdFromGlobalUniqueId generated)
                                controlledState
                            )
                      )
        _ -> Right ()

    validateReservation reservation = do
      let process = Controlled.reservationWitnessProcess reservation
          subject = StartupProcess process
          generated = Controlled.reservationWitnessGlobalUniqueId reservation
          generatedObject = globalObjectIdFromGlobalUniqueId generated
          nabla = Controlled.reservationWitnessNabla reservation
          reservedSort = Controlled.reservationWitnessSortId reservation
          phase = Controlled.reservationWitnessPhase reservation
          writerObject = globalObjectIdFromNablaId nabla
      let operate =
            either
              (const Nothing)
              Just
              ( ControlledOperate.checkControlledOperate
                  process
                  nabla
                  controlledState
                  structuralProgress
              )
      require
        subject
        ControlledReservationInvariant
        ( reservationIdentityMatches process generated
            && sourceRelationMatches
              process
              writerObject
              reservedSort
              phase
              reservation
              operate
            && not (Controlled.controlledHasAnyRole generatedObject controlledState)
            && case phase of
              Controlled.Reserved ->
                Controlled.reservationWitnessFirstPublicationId reservation == Nothing
                  && Controlled.controlledLocalRecord generatedObject controlledState == Nothing
                  && not
                    (Controlled.controlledHasAnyNormalPossession generatedObject controlledState)
              Controlled.Published ->
                case ( Controlled.reservationWitnessFirstPublicationId reservation,
                       Controlled.controlledLocalRecord generatedObject controlledState
                     ) of
                  (Just firstPublication, Just record) ->
                    Controlled.controlledTerminalDeletion generatedObject controlledState
                      == Nothing
                      && checkedPublicationId
                        (Controlled.controlledRecordFirstPublication record)
                        == firstPublication
                      && Controlled.controlledRecordSortId record
                        == reservedSort
                      && maybe
                        True
                        ( (== Controlled.controlledRecordOccurrenceId record)
                            . ControlledOperate.controlledOperateOccurrenceId
                        )
                        operate
                      && publishedPossessionMatches
                        process
                        generatedObject
                  (Just firstPublication, Nothing) ->
                    isJust
                      ( Controlled.controlledTerminalDeletion
                          generatedObject
                          controlledState
                      )
                      && not
                        ( Controlled.controlledHasAnyNormalPossession
                            generatedObject
                            controlledState
                        )
                      && terminalFirstPublicationRetained
                        generated
                        firstPublication
                        reservedSort
                  _ -> False
              Controlled.ConsumedByDelete ->
                Controlled.reservationWitnessFirstPublicationId reservation == Nothing
                  && Controlled.controlledLocalRecord generatedObject controlledState == Nothing
                  && not
                    (Controlled.controlledHasAnyNormalPossession generatedObject controlledState)
        )

    reservationIdentityMatches process generated =
      case find
        ((== process) . witnessedProcessEpoch)
        (Application.applicationProcessWitnesses applicationState) of
        Just witness ->
          not (Controlled.controlledProcessEnded process controlledState)
            && any ((== generated) . fst) (witnessedBindings witness)
        Nothing -> Controlled.controlledProcessEnded process controlledState

    sourceRelationMatches process writerObject expectedSort phase reservation operate =
      case operate of
        Just available ->
          ControlledOperate.controlledOperateProcess available == process
            && ControlledOperate.controlledOperateSortId available == expectedSort
            && Controlled.controlledHasNormalPossession
              process
              writerObject
              controlledState
            && Controlled.reservationWitnessAuthority reservation
              == ControlledOperate.controlledOperateAuthority available
        Nothing ->
          phase /= Controlled.Reserved
            && not
              ( Controlled.controlledHasNormalPossession
                  process
                  writerObject
                  controlledState
              )

    publishedPossessionMatches process object =
      case Controlled.controlledEffectiveLabelState object controlledState of
        Just released
          | releasedLabelStateIsDeleted released ->
              not
                (Controlled.controlledHasAnyNormalPossession object controlledState)
        _
          | Controlled.controlledProcessEnded process controlledState ->
              controlledPossessionEntriesAbsent process object controlledState
        _ ->
          Controlled.controlledHasNormalPossession
            process
            object
            controlledState

    terminalFirstPublicationRetained generated identifier expectedSort =
      any
        ( \(_, retained) ->
            Publication.applicationPublicationId retained == identifier
              && Publication.applicationPublicationOrigin retained
                == Publication.ControlledFirstUseApplicationPublicationOrigin generated
              && checkedPublicationSort
                (Publication.applicationPublicationChecked retained)
                == expectedSort
        )
        ( Publication.applicationPublicationEntries
            (startupPublicationState state)
        )

    validateCompletedDelete (_, _, request) =
      case ( Application.applicationRequestWitnessCall request,
             Application.applicationRequestWitnessStatus request
           ) of
        ( WriteApplication privateNabla (DeleteReserved privateObject),
          ApplicationRequest.ApplicationRequestCompleted
            (WriteCompleted WriteAccepted)
          ) -> do
            let process = Application.applicationRequestWitnessProcess request
                subject = StartupProcess process
            nablaGlobal <-
              resolvePrivate subject process (privateNablaUniqueId privateNabla)
            generated <-
              resolvePrivate subject process (privateObjectUniqueId privateObject)
            reservation <-
              maybe
                (fault subject ControlledReservationInvariant)
                Right
                (Controlled.controlledReservationWitness generated controlledState)
            require
              subject
              ControlledReservationInvariant
              ( Controlled.reservationWitnessProcess reservation == process
                  && Controlled.reservationWitnessNabla reservation
                    == nablaIdFromGlobalObjectId
                      (globalObjectIdFromGlobalUniqueId nablaGlobal)
                  && Controlled.reservationWitnessPhase reservation
                    == Controlled.ConsumedByDelete
              )
        _ -> Right ()

    resolvePrivate subject process privateIdentity =
      case Application.resolveApplicationPrivateUniqueId
        process
        privateIdentity
        applicationState of
        Right generated -> Right generated
        Left _ -> fault subject ApplicationGeneratedIdentityInvariant

validateWaitRegistrations :: HeraldState -> Either HeraldInvariantFault ()
validateWaitRegistrations state = do
  require
    StartupStatic
    ApplicationWaitRegistrationInvariant
    ( unique registrationIds
        && Set.fromList registrationIds == Set.fromList pendingIds
    )
  traverse_ validatePending pending
  where
    registrations = Wait.waitRegistrations (startupWaitState state)
    registrationsById =
      Map.fromList [(Wait.waitRegistrationId registration, registration) | registration <- registrations]
    registrationIds = fmap Wait.waitRegistrationId registrations
    pending =
      [ (session, request, witness, wait)
      | (session, request, witness) <- retainedRequestEntries state,
        ApplicationRequest.ApplicationRequestPending wait <-
          [Application.applicationRequestWitnessStatus witness]
      ]
    pendingIds = [wait | (_, _, _, wait) <- pending]
    validatePending (session, request, witness, wait) = do
      let process = Application.applicationRequestWitnessProcess witness
          subject = StartupProcess process
      registration <-
        maybe
          (fault subject ApplicationWaitRegistrationInvariant)
          Right
          (Map.lookup wait registrationsById)
      position <-
        maybe
          (fault subject ApplicationWaitRegistrationInvariant)
          Right
          (Application.applicationRequestWitnessPosition witness)
      require
        subject
        ApplicationWaitRegistrationInvariant
        ( Wait.waitRegistrationId registration == wait
            && Wait.waitRegistrationSessionId registration == session
            && Wait.waitRegistrationRequestId registration == request
            && Wait.waitRegistrationProcessEpoch registration == process
            && Wait.waitRegistrationProcessPosition registration == position
        )

-- | A preparation can outlive its parent session and the child's live access.
-- Its checked grant is immutable; current possession follows ordinary removal
-- and End, while exact private localization is retained only for live residents.
validateProcessPreparations :: HeraldState -> Either HeraldInvariantFault ()
validateProcessPreparations state = do
  require
    StartupStatic
    ProcessPreparationOwnerInvariant
    (ProcessPreparation.validateState owner == Right ())
  traverse_ validatePreparation entries
  traverse_ validateObservedGenesis (observedGenesisSelections state)
  traverse_ validateOutgoing (ProcessPreparation.outgoingEntries owner)
  traverse_ validateIncoming (ProcessPreparation.incomingEntries owner)
  traverse_ validateLifecycleRequest (ProcessPreparation.requestEntries owner)
  traverse_ validateInitialClaim (Application.applicationInitialClaimEntries application)
  require
    StartupStatic
    ProcessPreparationOwnerInvariant
    ( Set.fromList [(attachment, process) | (attachment, process, _) <- Application.applicationInitialClaimEntries application]
        == Set.union
          ( Set.fromList
              [ (ApplicationSession.applicationAttachmentForProcess process, process)
              | (_, preparation) <- entries,
                ProcessPreparation.preparationWasInstalled preparation,
                let process = processStartProcessEpochId (ProcessPreparation.preparationStart preparation)
              ]
          )
          (Set.intersection genesisClaims (Set.fromList [(attachment, process) | (attachment, process, _) <- Application.applicationInitialClaimEntries application]))
    )
  where
    owner = startupProcessPreparationState state
    entries = ProcessPreparation.preparationEntries owner
    application = startupApplicationState state
    controlled = startupControlledState state
    oracleProjection = startupOracleProjectionState state
    oracleView = OracleProjection.oracleView oracleProjection
    oracleRequests = Map.fromList (OracleClient.oracleClientRequestEntries (startupOracleClientState state))
    sessions = Map.fromList [(sessionWitnessId session, session) | session <- Application.applicationWitnessSessions (Application.applicationStateWitness application)]
    genesisClaims =
      Set.fromList
        [ (ApplicationSession.applicationAttachmentForBootstrap (appliedBootstrapManifestId bootstrap), appliedProcessEpochId bootstrap)
        | (_, bootstrap) <- OracleProjection.projectedBootstraps oracleProjection,
          appliedProcessResidence bootstrap == checkedLocalHeraldEpoch (startupGenesis state)
        ]
    preparationByProcess = Map.fromList [(processStartProcessEpochId (ProcessPreparation.preparationStart preparation), preparation) | (_, preparation) <- entries]
    requirePreparationFact subject = require subject ProcessPreparationOwnerInvariant
    requirePreparationValue subject = maybe (fault subject ProcessPreparationOwnerInvariant) Right

    -- The retained initial result never changes into a Resume result. A later
    -- binding belongs to the same session; expiry/End may remove that session
    -- while both owners retain the historical claim winner.
    validateInitialClaim (attachment, process, status) = do
      let subject = StartupProcess process
          check = requirePreparationFact subject
      let preparation = Map.lookup process preparationByProcess
      check (isJust preparation || Set.member (attachment, process) genesisClaims)
      case status of
        Application.ApplicationInitialClaimUnclaimed _ ->
          check
            ( maybe False ((`elem` [ProcessPreparation.Prepared, ProcessPreparation.Cancelling, ProcessPreparation.Terminal]) . ProcessPreparation.preparationPhase) preparation
                && all ((/= process) . sessionWitnessProcess) (Map.elems sessions)
            )
        Application.ApplicationInitialClaimClaimed _ acceptance deadline confirmed -> do
          check (maybe True ((`elem` [ProcessPreparation.Attached, ProcessPreparation.Terminal]) . ProcessPreparation.preparationPhase) preparation)
          let binding = ApplicationSession.sessionAcceptanceBinding acceptance
              session = ApplicationSession.sessionBindingSessionId binding
              localEpoch = checkedLocalHeraldEpoch (startupGenesis state)
              ordinal = ApplicationSession.applicationSessionIdOrdinal session
              (initialSession, initialToken, initialBinding) = ApplicationSession.initialSessionAllocation localEpoch ordinal
          check
            ( session == initialSession
                && binding == initialBinding
                && ordinal > 0
                && ordinal < Application.applicationWitnessNextSessionOrdinal (Application.applicationStateWitness application)
                && ApplicationSession.sessionAcceptanceCursor acceptance == ApplicationRequest.firstApplicationReplyCursor
            )
          case ApplicationSession.sessionAcceptanceReply acceptance of
            ApplicationSession.SessionResumed {} -> fault subject ProcessPreparationOwnerInvariant
            ApplicationSession.SessionOpened opened token startup -> do
              check (opened == session && token == initialToken)
              traverse_
                (\access -> check (Application.applicationStartupAccessForBootstrap access == Right startup))
                (maybe [] pure (Application.applicationBootstrapAccess process application))
              case Map.lookup session sessions of
                Nothing -> pure ()
                Just current -> do
                  check
                    ( sessionWitnessProcess current == process
                        && sessionWitnessAttachment current == attachment
                        && sessionWitnessNonce current == ApplicationSession.clientNonce 0
                        && sessionWitnessResumeToken current == token
                        && sessionWitnessStartupAccess current == startup
                        && if confirmed
                          then ApplicationSession.sessionBindingGeneration (sessionWitnessBinding current) >= ApplicationSession.sessionBindingGeneration binding
                          else sessionWitnessBinding current == binding
                    )
                  traverse_
                    ( \(Application.ApplicationSessionRecoveryWitness recovering _ expires _) ->
                        check (recovering /= session || confirmed || deadline == Just expires)
                    )
                    (Application.applicationSessionRecoveryEntries application)

    retainedTransfer subject bytes =
      either (const (fault subject ProcessPreparationOwnerInvariant)) Right (StartupTransfer.decodeStartupTransfer bytes)

    validateObservedGenesis (bootstrap, roots) = do
      let process = appliedProcessEpochId bootstrap
          check = requirePreparationFact (StartupProcess process)
      check (Controlled.controlledProcessFact process controlled == Just (Bootstrap.processControlledFact bootstrap))
      traverse_ (\root -> check (lookup (appliedRootObjectId root) [(controlledRootObject fact, fact) | fact <- Controlled.controlledRootFacts controlled] == Just (Bootstrap.controlledRootFact process root))) roots

    validateIncoming (reference, incoming) = do
      let subject = StartupStatic
      case ProcessPreparation.incomingOffer incoming of
        Nothing -> requirePreparationFact subject (ProcessPreparation.incomingCancelled incoming)
        Just (_, bytes) -> do
          transfer <- retainedTransfer subject bytes
          requirePreparationFact subject (StartupTransfer.transferSourceHerald transfer == ProcessPreparation.incomingSource incoming)
          traverse_
            ( \preparation ->
                requirePreparationFact
                  subject
                  ( ProcessPreparation.preparationSource preparation == Just (ProcessPreparation.incomingSource incoming)
                      && ProcessPreparation.preparationParent preparation == StartupTransfer.transferParent transfer
                  )
            )
            (maybe [] pure (ProcessPreparation.lookupPreparation reference owner))

    validateOutgoing (reference, outgoing) = do
      let parent = ProcessPreparation.outgoingParent outgoing
          subject = StartupProcess parent
          check = requirePreparationFact subject
      transfer <- retainedTransfer subject (ProcessPreparation.outgoingTransfer outgoing)
      check
        ( StartupTransfer.transferParent transfer == parent
            && StartupTransfer.transferSourceHerald transfer == checkedLocalHeraldEpoch (startupGenesis state)
            && ProcessPreparation.outgoingTarget outgoing /= checkedLocalHeraldEpoch (startupGenesis state)
        )
      case ProcessPreparation.outgoingResult outgoing of
        Nothing -> pure ()
        Just result -> do
          PreparationProtocol.RemoteChild child index descriptor <- requirePreparationValue subject (ProcessPreparation.outgoingChild outgoing)
          started <- requirePreparationValue subject (OracleProjection.oracleViewStartedProcess child oracleView)
          check
            ( OracleProjection.projectedStartedProcessControlIndex started == index
                && processStartResidence (OracleProjection.projectedStartedProcessBootstrap started) == ProcessPreparation.outgoingTarget outgoing
                && Lifecycle.preparedChildPreparation result == reference
                && Lifecycle.preparedChildConnection result == descriptor
                && Lifecycle.connectionDescriptorEpochBytes descriptor == DomainIdentity.heraldEpochBytes (ProcessPreparation.outgoingTarget outgoing)
                && Lifecycle.connectionDescriptorLineageBytes descriptor == DomainIdentity.systemIdBytes (checkedSystemId (startupGenesis state))
                && Lifecycle.connectionDescriptorAttachmentBytes descriptor == DomainIdentity.processEpochIdBytes child
            )
          case Application.applicationBootstrapAccess parent application of
            Nothing -> pure ()
            Just _ -> do
              check
                ( Application.resolveApplicationPrivateUniqueId parent (privateProcessUniqueId (Lifecycle.preparedChildProcess result)) application
                    == Right (globalUniqueIdFromGlobalObjectId (globalObjectIdFromProcessEpochId child))
                )
              check
                ( not (Controlled.controlledObjectCurrent (globalObjectIdFromProcessEpochId child) controlled)
                    || Controlled.controlledHasNormalPossession parent (globalObjectIdFromProcessEpochId child) controlled
                )

    validateEndReference subject process reference = do
      witness <- requirePreparationValue subject (Map.lookup reference oracleRequests)
      requirePreparationFact
        subject
        ( OracleClient.oracleRequestWitnessRef witness == reference
            && OracleClient.oracleRequestWitnessIntent witness == OracleClient.ProcessEndIntent process ExplicitAdministrativeEnd
        )

    -- Reuse the Oracle client's checked exact-request interpreter, then relate
    -- the accepted End to the canonical process projection as well.
    projectedLifecycleResult subject reference = do
      witness <- requirePreparationValue subject (Map.lookup reference oracleRequests)
      index <- case OracleClient.oracleRequestWitnessStatus witness of
        OracleClient.OracleRequestProjected index -> Right index
        _ -> fault subject ProcessPreparationOwnerInvariant
      entry <- requirePreparationValue subject (OracleClient.oracleClientRequestEvidence reference (startupOracleClientState state))
      requirePreparationFact
        subject
        (appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry) == index)
      prepared <- either (const (fault subject ProcessPreparationOwnerInvariant)) Right (OracleClient.prepareOracleResult reference entry (startupOracleClientState state))
      pure (OracleClient.preparedOracleResult prepared)

    validateCanonicalEnd subject process = do
      ended <- requirePreparationValue subject (OracleProjection.oracleViewEndedProcess process oracleView)
      let index = OracleProjection.projectedEndedProcessControlIndex ended
          reason = OracleProjection.projectedEndedProcessReason ended
      requirePreparationFact
        subject
        (OracleProjection.projectedEndedProcessEpoch ended == process)
      pure (index, reason)

    validateLifecycleRequest (key, record) = do
      let process = ProcessPreparation.lifecycleKeyProcess key
          subject = StartupProcess process
          check = requirePreparationFact subject
      case (ProcessPreparation.requestCommand record, ProcessPreparation.requestEndReference record) of
        (Lifecycle.EndOwnProcess, Just reference) -> do
          validateEndReference subject process reference
          case ProcessPreparation.requestStatus record of
            Lifecycle.LifecyclePending _ -> pure ()
            Lifecycle.LifecycleCompleted Lifecycle.ProcessEnded -> do
              result <- projectedLifecycleResult subject reference
              (index, reason) <- validateCanonicalEnd subject process
              check (result == OracleClient.OracleRequestEnded process index reason)
            Lifecycle.LifecycleRejected Lifecycle.LifecycleOracleRejected -> do
              result <- projectedLifecycleResult subject reference
              check (case result of OracleClient.OracleRequestRejected _ -> True; _ -> False)
            _ -> fault subject ProcessPreparationOwnerInvariant
        (Lifecycle.EndOwnProcess, Nothing) ->
          case ProcessPreparation.requestStatus record of
            -- The local lifecycle owner retains End before dispatch while
            -- an accepted environment finishes using its fresh writers.
            Lifecycle.LifecyclePending _ -> pure ()
            -- Another accepted End can terminate the process first.
            Lifecycle.LifecycleCompleted Lifecycle.ProcessEnded ->
              () <$ validateCanonicalEnd subject process
            Lifecycle.LifecycleRejected problem ->
              check (problem /= Lifecycle.LifecycleOracleRejected)
            _ -> fault subject ProcessPreparationOwnerInvariant
        (_, reference) -> check (isNothing reference)

    validateCancellationTerminal subject process preparation =
      case ProcessPreparation.preparationStartControl preparation of
        Just _ -> () <$ validateCanonicalEnd subject process
        Nothing -> case ProcessPreparation.preparationStartReference preparation of
          Nothing -> pure ()
          Just reference -> do
            result <- projectedLifecycleResult subject reference
            case result of
              OracleClient.OracleRequestRejected _ -> pure ()
              OracleClient.OracleRequestStarted start _ -> do
                requirePreparationFact subject (start == ProcessPreparation.preparationStart preparation)
                () <$ validateCanonicalEnd subject process
              _ -> fault subject ProcessPreparationOwnerInvariant

    validatePreparation (_, preparation) = do
      let start = ProcessPreparation.preparationStart preparation
          process = processStartProcessEpochId start
          subject = StartupProcess process
          check = require subject ProcessPreparationOwnerInvariant
          attachment = ApplicationSession.applicationAttachmentForProcess process
          grant = ProcessPreparation.preparationGrant preparation
      check (processStartResidence start == checkedLocalHeraldEpoch (startupGenesis state))
      case ProcessPreparation.preparationSource preparation of
        Nothing -> check (ProcessPreparation.lookupIncoming (ProcessPreparation.preparationReference preparation) owner == Nothing)
        Just source -> do
          incoming <- requirePreparationValue subject (ProcessPreparation.lookupIncoming (ProcessPreparation.preparationReference preparation) owner)
          check (ProcessPreparation.incomingSource incoming == source && isJust (ProcessPreparation.incomingOffer incoming))
      traverse_
        (\reference -> check (maybe False ((== Just start) . OracleClient.oracleRequestWitnessBootstrap) (Map.lookup reference oracleRequests)))
        (maybe [] pure (ProcessPreparation.preparationStartReference preparation))
      traverse_
        (validateEndReference subject process)
        (maybe [] pure (ProcessPreparation.preparationEndReference preparation))
      unless
        ( ProcessPreparation.preparationPhase preparation /= ProcessPreparation.Terminal
            || not (ProcessPreparation.preparationCancelled preparation)
            || isJust (ProcessPreparation.preparationFailure preparation)
        )
        (validateCancellationTerminal subject process preparation)
      let hasAccess = ProcessPreparation.preparationWasInstalled preparation && dynamicApplicationResident state process
      if not hasAccess
        then check (Application.applicationBootstrapAccess process application == Nothing)
        else do
          access <- maybe (fault subject ProcessPreparationOwnerInvariant) Right (Application.applicationBootstrapAccess process application)
          let selected = Application.bootstrapAccessPrimordial access
          check
            ( Application.applicationAttachmentProcess attachment application == Just process
                && StartupAccess.accessRequiredEndpoints selected == Primordial.primordialGrantRequiredEndpoints grant
                && StartupAccess.accessEnvironmentSources selected == Primordial.primordialGrantEnvironmentSources grant
                && Map.keysSet (StartupAccess.accessEntries selected) == Map.keysSet (Primordial.primordialGrantEntries grant)
            )
          check
            ( Application.resolveApplicationPrivateUniqueId process (privateProcessUniqueId (Application.bootstrapAccessProcess access)) application
                == Right (globalUniqueIdFromGlobalObjectId (globalObjectIdFromProcessEpochId process))
            )
          traverse_
            ( \(key, global) -> do
                private <- maybe (fault subject ProcessPreparationOwnerInvariant) Right (Map.lookup key (StartupAccess.accessEntries selected))
                check (Application.resolveApplicationPrivateUniqueId process (StartupAccess.primordialEntryIdentity private) application == Right (Primordial.globalPrimordialIdentity global))
            )
            (Map.toList (Primordial.primordialGrantEntries grant))
          traverse_
            ( \possession ->
                let object = Controlled.controlledGrantObject possession
                 in check
                      ( not (Controlled.controlledObjectCurrent object controlled)
                          || Controlled.controlledHasNormalPossession process object controlled
                      )
            )
            (Primordial.primordialGrantPossessions grant)
      case ProcessPreparation.preparationResult preparation of
        Just result -> do
          let parent = ProcessPreparation.preparationParent preparation
          check (Lifecycle.connectionDescriptorAttachmentBytes (Lifecycle.preparedChildConnection result) == DomainIdentity.processEpochIdBytes process)
          case Application.applicationBootstrapAccess parent application of
            Nothing -> pure ()
            Just _ ->
              check
                ( Application.resolveApplicationPrivateUniqueId parent (privateProcessUniqueId (Lifecycle.preparedChildProcess result)) application
                    == Right (globalUniqueIdFromGlobalObjectId (globalObjectIdFromProcessEpochId process))
                )
        Nothing -> pure ()

validateConfiguredProcesses ::
  HeraldState ->
  Either HeraldInvariantFault ()
validateConfiguredProcesses state = do
  require
    StartupStatic
    ConfiguredStartAdministrationInvariant
    ( unique commandCorrelations
        && Set.fromList commandCorrelations
          == Set.fromList
            ( [correlation | (correlation, _, _, _) <- configuredStartEntries]
                <> [correlation | (correlation, _, _) <- endProcessEntries]
                <> [correlation | (correlation, _, _) <- Administration.preparationCancellationEntries administration]
                <> [correlation | (correlation, _, _) <- Administration.voterAdministrationEntries administration]
                <> map fst (Administration.voterPreparationRejectionEntries administration)
            )
        && unique acceptancePositions
        && nextAcceptance >= 1
        && all (\position -> position >= 1 && position < nextAcceptance) acceptancePositions
        && fromIntegral (length acceptancePositions) + Administration.retiredAcceptanceCount administration == nextAcceptance - 1
        && Set.fromList oracleReferences
          `Set.isSubsetOf` Map.keysSet oracleRequests
        && unique preparedProcesses
        && Set.fromList preparedProcesses == Map.keysSet scaffoldsByProcess
    )
  traverse_ validateConfiguredStart configuredStartEntries
  traverse_ validateRetiredStart (Administration.configuredStartRetiredOrigins administration)
  traverse_ validateEndProcess endProcessEntries
  traverse_ validateVoterAdministration (Administration.voterAdministrationEntries administration)
  require
    StartupStatic
    ConfiguredScaffoldInvariant
    (unique (fmap fst scaffoldEntries))
  traverse_ validateScaffold scaffoldEntries
  traverse_ validateDynamicProcess (OracleProjection.projectedStartedProcesses oracleProjection)
  where
    genesis = startupGenesis state
    localEpoch = checkedLocalHeraldEpoch genesis
    administration = startupAdministrationState state
    nextAcceptance = Administration.configuredStartNextAcceptanceOrdinal administration
    oracleClient = startupOracleClientState state
    oracleProjection = startupOracleProjectionState state
    configuredStartEntries = Administration.configuredStartEntries administration
    endProcessEntries = Administration.endProcessEpochEntries administration
    commandCorrelations =
      [ correlation
      | (correlation, _, _) <-
          Administration.administrationWitnessCommands
            (Administration.administrationStateWitness administration)
      ]
    oracleRequests =
      Map.fromList (OracleClient.oracleClientRequestEntries oracleClient)
    oracleReferences =
      [ Administration.startProcessIntentionOracleRequest intention
      | (_, _, _, status) <- configuredStartEntries,
        intention <- case status of
          Administration.ConfiguredStartGated -> []
          Administration.ConfiguredStartAwaitingOracle value -> [value]
          Administration.ConfiguredStartOracleRejected value _ _ -> [value]
          Administration.ConfiguredStartProcessInstalled value _ _ -> [value]
          Administration.ConfiguredStartReady value _ _ _ -> [value]
      ]
        <> [ Administration.endProcessIntentionOracleRequest intention
           | (_, _, status) <- endProcessEntries,
             intention <- maybeToList (endProcessStatusIntention status)
           ]
    validateVoterAdministration (_, reference, receipt) = do
      require StartupStatic ConfiguredStartAdministrationInvariant (Map.member reference oracleRequests)
      let witness = oracleRequests Map.! reference
      require StartupStatic ConfiguredStartAdministrationInvariant (case OracleClient.oracleRequestWitnessIntent witness of OracleClient.VoterAdministrationIntent {} -> True; _ -> False)
      require StartupStatic ConfiguredStartAdministrationInvariant $ case (OracleClient.oracleRequestWitnessStatus witness, receipt) of
        (OracleClient.OracleRequestAwaitingProjection, Nothing) -> True
        (OracleClient.OracleRequestProjected index, Just retained) ->
          case OracleClient.oracleClientRequestEvidence reference oracleClient of
            Just entry ->
              appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry) == index
                && (canonicalizeOracleReceipt . appliedEntryReceipt <$> appliedEntryCommand (canonicalAppliedOracleEntryValue entry)) == Just retained
            Nothing -> False
        _ -> False
    acceptancePositions =
      [ AdministrationInternal.configuredStartAcceptancePositionWord64
          (Administration.startProcessIntentionAcceptancePosition intention)
      | (_, _, _, status) <- configuredStartEntries,
        intention <- case status of
          Administration.ConfiguredStartGated -> []
          Administration.ConfiguredStartAwaitingOracle value -> [value]
          Administration.ConfiguredStartOracleRejected value _ _ -> [value]
          Administration.ConfiguredStartProcessInstalled value _ _ -> [value]
          Administration.ConfiguredStartReady value _ _ _ -> [value]
      ]
        <> [ AdministrationInternal.configuredStartAcceptancePositionWord64
               (Administration.endProcessIntentionAcceptancePosition intention)
           | (_, _, status) <- endProcessEntries,
             intention <- maybeToList (endProcessStatusIntention status)
           ]
    preparedProcesses =
      [ Configured.liveProcessScaffoldRefProcessEpoch reference
      | (_, _, _, Administration.ConfiguredStartProcessInstalled _ _ reference) <-
          configuredStartEntries
      ]
        <> [ Configured.liveProcessScaffoldRefProcessEpoch reference
           | (_, _, _, Administration.ConfiguredStartReady _ _ reference _) <-
               configuredStartEntries
           ]
        <> [process | (process, _, _, _, _) <- Administration.configuredStartRetiredOrigins administration]
    scaffoldEntries =
      startupWitnessConfiguredScaffolds (startupStateWitness state)
    scaffoldsByProcess = Map.fromList scaffoldEntries
    validateRetiredStart (process, index, request, reference, attachment) = do
      let subject = StartupProcess process
      scaffold <- maybe (fault subject ConfiguredScaffoldInvariant) Right (Map.lookup process scaffoldsByProcess)
      require
        subject
        ConfiguredScaffoldInvariant
        ( Configured.liveProcessScaffoldRefProcessEpoch reference == process
            && Configured.liveProcessScaffoldControlIndex scaffold == index
            && case OracleProjection.oracleViewStartedProcess process (OracleProjection.oracleView oracleProjection) of
              Just started ->
                OracleProjection.projectedStartedProcessControlIndex started == index
                  && OracleProjection.projectedStartedProcessRequest started == OracleClient.oracleRequestRefRequestId request
              Nothing -> False
        )
      maybe (pure ()) (validateReadyConfiguredStart scaffold) attachment

    validateConfiguredStart (correlation, request, digest, status) = do
      require
        StartupStatic
        ConfiguredStartAdministrationInvariant
        ( correlation
            == AdministrationInternal.startProcessCorrelation request
            && digest
              == AdministrationInternal.startProcessRequestDigest request
        )
      case status of
        Administration.ConfiguredStartGated -> Right ()
        Administration.ConfiguredStartAwaitingOracle intention -> do
          (_, _, witness) <- validateConfiguredIntention request digest intention
          require
            StartupStatic
            ConfiguredStartOracleRequestInvariant
            ( case OracleClient.oracleRequestWitnessStatus witness of
                OracleClient.OracleRequestAwaitingSourceDrain -> False
                OracleClient.OracleRequestEndedBeforeSubmission {} -> False
                OracleClient.OracleRequestAwaitingProjection -> True
                OracleClient.OracleRequestDeferred {} -> False
                OracleClient.OracleRequestProjected index ->
                  projectedRequestEvidenceMatches intention index
            )
        Administration.ConfiguredStartOracleRejected intention index rejection -> do
          (_, subject, witness) <- validateConfiguredIntention request digest intention
          entry <- requireProjectedRequest subject intention witness index
          let applied = canonicalAppliedOracleEntryValue entry
          require
            subject
            ConfiguredStartOracleProjectionInvariant
            ( case ( fmap (oracleReceiptResult . appliedEntryReceipt) (appliedEntryCommand applied),
                     appliedEntryProjectionEvents applied
                   ) of
                (Just (OracleRejected observed), []) -> observed == rejection
                _ -> False
            )
        Administration.ConfiguredStartProcessInstalled intention index reference -> do
          _ <- validatePreparedConfiguredStart request digest intention index reference
          Right ()
        Administration.ConfiguredStartReady intention index reference attachment -> do
          (_, retainedScaffold) <-
            validatePreparedConfiguredStart request digest intention index reference
          validateReadyConfiguredStart retainedScaffold attachment

    endProcessStatusIntention status = case status of
      Administration.EndProcessAwaitingOracle intention -> Just intention
      Administration.EndProcessOracleRejected intention _ _ -> Just intention
      Administration.EndProcessAwaitingRetirement intention _ _ _ -> Just intention
      Administration.EndProcessCompleted intention _ _ _ -> Just intention

    validateEndProcess (correlation, request, status) = do
      let process = AdministrationInternal.endProcessEpochProcess request
          reason = AdministrationInternal.endProcessEpochReason request
          subject = StartupProcess process
      require
        subject
        ConfiguredStartAdministrationInvariant
        (correlation == AdministrationInternal.endProcessEpochCorrelation request)
      intention <-
        maybe
          (fault subject ConfiguredStartAdministrationInvariant)
          Right
          (endProcessStatusIntention status)
      let reference = Administration.endProcessIntentionOracleRequest intention
      witness <-
        maybe
          (fault subject ConfiguredStartOracleRequestInvariant)
          Right
          (Map.lookup reference oracleRequests)
      require
        subject
        ConfiguredStartOracleRequestInvariant
        ( OracleClient.oracleRequestWitnessRef witness == reference
            && OracleClient.oracleRequestWitnessSemanticKey witness
              == AdministrationInternal.endProcessRequestDigestBytes
                (AdministrationInternal.endProcessEpochRequestDigest request)
            && OracleClient.oracleRequestWitnessIntent witness
              == OracleClient.ProcessEndIntent process reason
        )
      case status of
        Administration.EndProcessAwaitingOracle _ ->
          require
            subject
            ConfiguredStartOracleRequestInvariant
            ( case OracleClient.oracleRequestWitnessStatus witness of
                OracleClient.OracleRequestAwaitingSourceDrain -> False
                OracleClient.OracleRequestEndedBeforeSubmission {} -> False
                OracleClient.OracleRequestAwaitingProjection -> True
                OracleClient.OracleRequestDeferred decision observedIndex ->
                  observedIndex
                    <= OracleProjection.oracleViewControlIndex
                      (OracleProjection.oracleView oracleProjection)
                    && not
                      ( OracleProjection.oracleViewLabelWorkflowCompleted
                          decision
                          (OracleProjection.oracleView oracleProjection)
                      )
                OracleClient.OracleRequestProjected index ->
                  projectedEndRequestEvidenceMatches reference index
            )
        Administration.EndProcessOracleRejected _ index rejection -> do
          entry <- requireEndProjectedRequest subject reference witness index
          let applied = canonicalAppliedOracleEntryValue entry
          require
            subject
            ConfiguredStartOracleProjectionInvariant
            ( fmap (oracleReceiptResult . appliedEntryReceipt) (appliedEntryCommand applied)
                == Just (OracleRejected rejection)
                && null (appliedEntryProjectionEvents applied)
            )
        Administration.EndProcessAwaitingRetirement _ index retainedProcess retainedReason ->
          validateAcceptedEnd subject reference witness index retainedProcess retainedReason
        Administration.EndProcessCompleted _ index retainedProcess retainedReason ->
          validateAcceptedEnd subject reference witness index retainedProcess retainedReason

    validateAcceptedEnd subject reference witness index process reason = do
      entry <- requireEndProjectedRequest subject reference witness index
      let applied = canonicalAppliedOracleEntryValue entry
      ended <- case ( fmap (oracleReceiptResult . appliedEntryReceipt) (appliedEntryCommand applied),
                      fmap oracleProjectionEventView (appliedEntryProjectionEvents applied)
                    ) of
        (Just OracleAccepted, [ProcessEpochEndedEventView observed observedIndex observedReason]) ->
          Right (observed, observedIndex, observedReason)
        _ -> fault subject ConfiguredStartOracleProjectionInvariant
      require
        subject
        ConfiguredStartOracleProjectionInvariant
        ( ended == (process, index, reason)
            && case OracleProjection.oracleViewEndedProcess
              process
              (OracleProjection.oracleView oracleProjection) of
              Just projected ->
                OracleProjection.projectedEndedProcessEpoch projected == process
                  && OracleProjection.projectedEndedProcessControlIndex projected == index
                  && OracleProjection.projectedEndedProcessReason projected == reason
              Nothing -> False
        )

    requireEndProjectedRequest subject reference witness index = do
      entry <-
        maybe
          (fault subject ConfiguredStartOracleProjectionInvariant)
          Right
          (OracleClient.oracleClientRequestEvidence reference oracleClient)
      require
        subject
        ConfiguredStartOracleProjectionInvariant
        ( appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry) == index
            && OracleClient.oracleRequestWitnessStatus witness
              == OracleClient.OracleRequestProjected index
        )
      Right entry

    projectedEndRequestEvidenceMatches reference index =
      case OracleClient.oracleClientRequestEvidence reference oracleClient of
        Just entry -> appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry) == index
        Nothing -> False

    validatePreparedConfiguredStart request digest intention index reference = do
      (bootstrap, subject, witness) <-
        validateConfiguredIntention request digest intention
      entry <- requireProjectedRequest subject intention witness index
      let applied = canonicalAppliedOracleEntryValue entry
      startedRecord <-
        case ( fmap (oracleReceiptResult . appliedEntryReceipt) (appliedEntryCommand applied),
               fmap oracleProjectionEventView (appliedEntryProjectionEvents applied)
             ) of
          (Just OracleAccepted, [ProcessStartedView value]) -> Right value
          _ -> fault subject ConfiguredStartOracleProjectionInvariant
      require
        subject
        ConfiguredStartOracleProjectionInvariant
        ( Configured.liveProcessScaffoldRefProcessEpoch reference
            == processStartProcessEpochId bootstrap
            && processRecordProcessEpoch startedRecord
              == processStartProcessEpochId bootstrap
            && processRecordOrigin startedRecord
              == DynamicStartView index (OracleClient.oracleRequestRefRequestId (Administration.startProcessIntentionOracleRequest intention))
            && appliedEntryControlIndex applied == index
        )
      expectedPrepared <-
        either
          (const (fault subject ConfiguredScaffoldInvariant))
          Right
          (Configured.prepareLiveProcessScaffold genesis bootstrap index Configured.initialState)
      let process = processStartProcessEpochId bootstrap
          expectedScaffold = Configured.preparedLiveProcessScaffold expectedPrepared
      retainedScaffold <-
        maybe
          (fault subject ConfiguredScaffoldInvariant)
          Right
          (Map.lookup process scaffoldsByProcess)
      require subject ConfiguredScaffoldInvariant (retainedScaffold == expectedScaffold)
      Right (bootstrap, retainedScaffold)

    validateReadyConfiguredStart scaffold attachment = do
      let process = processStartProcessEpochId (Configured.liveProcessScaffoldBootstrap scaffold)
          subject = StartupProcess process
          application = startupApplicationState state
      if not (dynamicApplicationResident state process)
        then
          require
            subject
            ConfiguredStartReadyInvariant
            ( Application.applicationBootstrapAccess process application == Nothing
                && Application.applicationAttachmentProcess attachment application == Nothing
            )
        else do
          access <-
            maybe
              (fault subject ConfiguredStartReadyInvariant)
              Right
              (Application.applicationBootstrapAccess process application)
          require
            subject
            ConfiguredStartReadyInvariant
            ( attachment == ApplicationSession.applicationAttachmentForProcess process
                && Application.applicationAttachmentProcess attachment application == Just process
                && Application.bootstrapAccessProcessEpoch access == process
            )
          processBinding <-
            either
              (const (fault subject ConfiguredStartReadyInvariant))
              Right
              ( Application.resolveApplicationPrivateUniqueId
                  process
                  (privateProcessUniqueId (Application.bootstrapAccessProcess access))
                  application
              )
          require
            subject
            ConfiguredStartReadyInvariant
            ( processBinding
                == globalUniqueIdFromGlobalObjectId
                  (globalObjectIdFromProcessEpochId process)
            )

    validateConfiguredIntention _request digest intention = do
      let reference = Administration.startProcessIntentionOracleRequest intention
      witness <-
        maybe
          (fault StartupStatic ConfiguredStartOracleRequestInvariant)
          Right
          (Map.lookup reference oracleRequests)
      bootstrap <-
        maybe
          (fault StartupStatic ConfiguredStartOracleRequestInvariant)
          Right
          (OracleClient.oracleRequestWitnessBootstrap witness)
      let subject = StartupProcess (processStartProcessEpochId bootstrap)
      require
        subject
        ConfiguredStartOracleRequestInvariant
        ( processStartResidence bootstrap == localEpoch
            && OracleClient.oracleRequestWitnessRef witness == reference
            && OracleClient.oracleRequestWitnessSemanticKey witness
              == AdministrationInternal.processStartRequestDigestBytes digest
            && OracleClient.oracleRequestWitnessIntent witness
              == OracleClient.DynamicStartIntent bootstrap
        )
      Right (bootstrap, subject, witness)

    requireProjectedRequest subject intention witness index = do
      let reference = Administration.startProcessIntentionOracleRequest intention
      entry <-
        maybe
          (fault subject ConfiguredStartOracleProjectionInvariant)
          Right
          (OracleClient.oracleClientRequestEvidence reference oracleClient)
      require
        subject
        ConfiguredStartOracleProjectionInvariant
        ( appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry) == index
            && OracleClient.oracleRequestWitnessStatus witness
              == OracleClient.OracleRequestProjected index
        )
      Right entry

    projectedRequestEvidenceMatches intention index =
      let reference = Administration.startProcessIntentionOracleRequest intention
       in case OracleClient.oracleClientRequestEvidence reference oracleClient of
            Just entry -> appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry) == index
            Nothing -> False

    validateDynamicProcess (process, started) = do
      let bootstrap = OracleProjection.projectedStartedProcessBootstrap started
          index = OracleProjection.projectedStartedProcessControlIndex started
          controlled = startupControlledState state
      fact <-
        maybe
          (fault (StartupProcess process) ControlledProcessInvariant)
          Right
          (Controlled.controlledProcessFact process controlled)
      require
        (StartupProcess process)
        ControlledProcessInvariant
        ( Controlled.processFactProcessId fact == processStartProcessId bootstrap
            && Controlled.processFactProcessEpoch fact == process
            && Controlled.processFactResidence fact == processStartResidence bootstrap
            && Controlled.processFactAuthority fact == DomainIdentity.labelAuthorityEpoch index
        )

    validateScaffold (process, scaffold) = do
      let bootstrap = Configured.liveProcessScaffoldBootstrap scaffold
      require
        (StartupProcess process)
        ConfiguredScaffoldInvariant
        ( processStartProcessEpochId bootstrap == process
            && processStartResidence bootstrap == localEpoch
        )

validatePublications ::
  HeraldState ->
  [ResidentApplicationAlias] ->
  Either HeraldInvariantFault ()
validatePublications state residents = do
  traverse_ (fmap (const ()) . checkedIncoming) authenticatedHeldIncomingRecords
  incomingChecked <- traverse checkedIncoming completedIncomingRecords
  alignmentChecked <- traverse checkedAlignmentEvidence alignmentEvidence
  joinChecked <- traverse checkedJoinEvidence joinEvidence
  terminalChecked <- traverse checkedTerminalOccurrence terminalOccurrences
  let checkedByPublication =
        Map.fromList
          ( [(checkedPublicationId publication, publication) | publication <- primordial <> genesisDescriptions]
              <> [ ( Publication.outgoingPublicationId record,
                     Publication.outgoingPublicationChecked record
                   )
                 | record <- records
                 ]
              <> [ ( Publication.applicationPublicationId record,
                     Publication.applicationPublicationChecked record
                   )
                 | record <- structuralApplicationRecords
                 ]
              <> [ ( identifier,
                     Environment.positionedEnvironmentRootChecked positioned
                   )
                 | (identifier, positioned) <- environmentEntries
                 ]
              <> incomingChecked
              <> alignmentChecked
              <> joinChecked
              <> [ (identifier, publication)
                 | (identifier, publication, _) <- terminalChecked
                 ]
          )
      stampedStructuralRoutes =
        retainedStampedStructuralRoutes
          `Map.union` Map.fromList
            [ (identifier, route)
            | (identifier, _, route) <- terminalChecked
            ]
      primordialIds = Set.fromList (fmap checkedPublicationId primordial)
      terminalPurgeEntries =
        [ (checkedPublicationId publication, coordinate, cause)
        | publication <- Map.elems checkedByPublication,
          Just (coordinate, cause) <- [terminalPurgeCoordinate publication]
        ]
      bootstrapTerminalPurgeEntries =
        [ (coordinate, Controlled.controlledTerminalDeletionCause deletion)
        | (object, deletion) <- Controlled.controlledTerminalDeletionEntries controlled,
          Just coordinate <- [bootstrapTerminalPurgeCoordinate object]
        ]
      terminalPurgeCauseSets =
        Map.fromListWith
          Set.union
          ( [ (coordinate, Set.singleton cause)
            | (_, coordinate, cause) <- terminalPurgeEntries
            ]
              <> [ (coordinate, Set.singleton cause)
                 | (coordinate, cause) <- bootstrapTerminalPurgeEntries
                 ]
          )
      terminalPurgeCauses = Set.findMin <$> terminalPurgeCauseSets
      receiptedPublications =
        Set.fromList
          [ identifier
          | slot <- storeSlots,
            (identifier, _) <- Store.storeSlotApplicationReceipts slot
          ]
      requiredTerminalPurgeCauseSets =
        Map.fromListWith
          Set.union
          ( [ (coordinate, Set.singleton cause)
            | (identifier, coordinate, cause) <- terminalPurgeEntries,
              Set.member identifier receiptedPublications
            ]
              <> [ (coordinate, Set.singleton cause)
                 | (coordinate, cause) <- bootstrapTerminalPurgeEntries
                 ]
          )
      requiredTerminalPurges = Set.findMin <$> requiredTerminalPurgeCauseSets
  require
    StartupStatic
    ControlledTerminalDeletionInvariant
    ( all ((== 1) . Set.size) (Map.elems terminalPurgeCauseSets)
        && all ((== 1) . Set.size) (Map.elems requiredTerminalPurgeCauseSets)
        && Store.storeTerminalObjectPurgeEntries storeState
          == Map.toAscList terminalPurgeCauses
    )
  require
    StartupStatic
    PublicationOwnerInvariant
    ( Publication.publicationWitnessHeraldEpoch witness == localEpoch
        && unique entryKeys
        && all (\(key, record) -> key == Publication.outgoingPublicationId record) entries
        && unique (fmap fst applicationEntries)
        && all
          ( \(key, record) ->
              key == Publication.applicationPublicationAcceptancePosition record
          )
          applicationEntries
        && unique incomingEntryKeys
        && all
          ( \(key, record) ->
              key
                == publicationBatchId
                  (peerPublicationBatch (Publication.incomingPeerPublication record))
          )
          incomingEntries
        && all
          ( \(key, positioned) ->
              key
                == Environment.positionedEnvironmentRootPublicationId positioned
          )
          environmentEntries
        && unique allPublicationIds
        && consistentCheckedPublications
          ( fmap snd incomingChecked
              <> fmap snd alignmentChecked
              <> fmap snd joinChecked
              <> primordial
              <> fmap Publication.outgoingPublicationChecked records
              <> fmap Publication.applicationPublicationChecked structuralApplicationRecords
              <> fmap Environment.positionedEnvironmentRootChecked environmentPositioned
              <> [publication | (_, publication, _) <- terminalChecked]
          )
        && unique
          [ ( Publication.outgoingPublicationSourceProcess record,
              ApplicationRequest.processAcceptancePositionOrdinal
                (Publication.outgoingPublicationAcceptancePosition record)
            )
          | record <- records
          ]
    )
  require
    StartupStatic
    PublicationAllocationInvariant
    ( unique localAllocatedIds
        && unique heraldPositions
        && sort heraldPositions == [1 .. fromIntegral (length heraldPositions)]
        && nextHeraldPosition == fromIntegral (length heraldPositions + 1)
        && unique (fmap fst nextSequenceEntries)
        && Map.keysSet nextSequences == expectedTenures
        && and
          [ let sequences = Map.findWithDefault [] tenure sequencesByTenure
                initialSequence =
                  if tenure `Set.member` environmentWriterTenures
                    && not (0 `elem` sequences)
                    then 1
                    else 0
             in sort sequences
                  == take (length sequences) [initialSequence ..]
                  && maybe
                    False
                    ( (== initialSequence + fromIntegral (length sequences))
                        . nablaSequenceWord64
                    )
                    (Map.lookup tenure nextSequences)
          | tenure <- Set.toAscList expectedTenures
          ]
    )
  require
    StartupStatic
    PublicationRemoteAssignmentInvariant
    ( all Publication.outgoingPublicationDispatchCertified records
        && all Publication.stampedStructuralDispatchCertified stampedStructuralStages
    )
  traverse_ validateRecord entries
  traverse_ validateStampedStructuralStage stampedStructuralEntries
  traverse_ validateApplicationLink applicationRecords
  validateIncomingPublications state
  traverse_ validateSuccessfulWrite successfulWrites
  traverse_ validateRequestPublicationOverlap acceptedRequests
  traverse_
    ( validateStoreSlotReceipts
        checkedByPublication
        storeState
        terminalPurgeCauses
        requiredTerminalPurges
        primordialIds
        (Map.unionWith Set.union genesisEnvironmentSeeds dynamicEnvironmentSeeds)
        outgoingById
        stampedStructuralRoutes
        incomingById
        (Map.unionWith (<>) alignmentStrengths joinStrengths)
    )
    storeSlots
  where
    localEpoch = checkedLocalHeraldEpoch (startupGenesis state)
    oracleView = OracleProjection.oracleView (startupOracleProjectionState state)
    publicationState = startupPublicationState state
    witness = Publication.publicationStateWitness publicationState
    entries = Publication.outgoingPublicationEntries publicationState
    entryKeys = fmap fst entries
    records = fmap snd entries
    outgoingById = Map.fromList entries
    applicationEntries =
      Publication.applicationPublicationEntries publicationState
    applicationRecords = fmap snd applicationEntries
    structuralApplicationRecords =
      filter
        (isNothing . Publication.applicationPublicationOutgoingRecord)
        applicationRecords
    stampedStructuralEntries =
      Publication.stampedStructuralStageEntries publicationState
    stampedStructuralStages = fmap snd stampedStructuralEntries
    retainedStampedStructuralRoutes =
      Map.fromList
        [ ( checkedPublicationId (Publication.stampedStructuralChecked stage),
            Publication.stampedStructuralRoute stage
          )
        | stage <- stampedStructuralStages
        ]
    environmentEntries =
      Publication.environmentRootStageEntries publicationState
    environmentPositioned = fmap snd environmentEntries
    incomingEntries = Publication.incomingPublicationEntries publicationState
    incomingEntryKeys = fmap fst incomingEntries
    incomingById = Map.fromList incomingEntries
    alignmentTransfer =
      Alignment.alignmentTransferState (startupAlignmentState state)
    alignmentEvidence =
      AlignmentTransfer.appliedDestinationEvidenceEntries alignmentTransfer
    joinEvidence = Join.importedSystemViewObservations (startupJoinState state)
    joinStrengths =
      Map.fromListWith
        (<>)
        [ ( ( deriveSystemViewDeltaId system localEpoch role,
              deriveSystemViewStoreIncarnationId system localEpoch role,
              AlignmentProtocol.retainedPublicationEvidencePublicationId evidence
            ),
            [AlignmentProtocol.retainedPublicationEvidenceSourceStrength evidence]
          )
        | (role, evidence) <- joinEvidence,
          let system = checkedSystemId (startupGenesis state)
        ]
    terminalOccurrences =
      maybe
        []
        (fmap snd . StructuralBase.structuralBasePayloadEntries)
        (startupStructuralBaseCoordinator state)
    alignmentStrengths =
      Map.fromListWith
        (<>)
        [ ( ( AlignmentProtocol.destinationStoreDelta destination,
              AlignmentProtocol.destinationStoreIncarnation destination,
              AlignmentProtocol.retainedPublicationEvidencePublicationId publication
            ),
            [AlignmentTransfer.appliedDestinationEvidenceStrength evidence]
          )
        | evidence <- alignmentEvidence,
          AlignmentTransfer.appliedDestinationEvidenceDisposition evidence
            == AlignmentTransfer.StoreAppliedOrAlreadyApplied,
          let destination =
                AlignmentTransfer.appliedDestinationEvidenceDestination evidence,
          let publication =
                AlignmentTransfer.appliedDestinationEvidencePublication evidence
        ]
    primordial = Store.primordialSeedPublications (startupStoreState state)
    genesisBootstraps = fmap snd (OracleProjection.projectedBootstraps (startupOracleProjectionState state))
    genesisDescriptionBundles =
      [ (bootstrap, [publication | (_, _, publication) <- appliedProcessEnvironmentPublications (checkedSystemId (startupGenesis state)) genesisBootstraps bootstrap])
      | bootstrap <- genesisBootstraps
      ]
    genesisDescriptions = concatMap snd genesisDescriptionBundles
    genesisEnvironmentSeeds =
      Map.fromList
        [ ( (delta, incarnation),
            Set.fromList [checkedPublicationId publication | publication <- publications, checkedPublicationSort publication == appliedRootSortId root]
          )
        | (bootstrap, publications) <- genesisDescriptionBundles,
          appliedProcessResidence bootstrap == localEpoch,
          root <- appliedProcessRoots bootstrap,
          ReaderRoot delta <- [appliedRootRole root],
          Just (_, incarnation) <- [appliedRootPlacement root]
        ]
    -- Construction seeding belongs to the original reader incarnation only.
    -- A later label-created store receives data through ordinary alignment.
    dynamicEnvironmentSeeds =
      Map.fromListWith
        Set.union
        [ ((delta, Store.storeSlotIncarnation slot), Set.fromList seededIds)
        | (_, manifest) <- Publication.environmentManifestEntries publicationState,
          let process = Environment.environmentManifestProcess manifest,
          let roots = NonEmpty.toList (Environment.environmentManifestRoots manifest),
          reader <- roots,
          let readerPlan = Environment.positionedEnvironmentRootPlan reader,
          EnvironmentReaderRootClaimView role <- [environmentRootClaimView (environmentRootSlotClaim (Environment.environmentRootPlanSlot readerPlan))],
          let target = Environment.environmentRootPlanTargetObject readerPlan,
          let delta = deltaIdFromGlobalObjectId target,
          Just readerStage <- [Map.lookup (Environment.positionedEnvironmentRootPublicationId reader) environmentStagesByPublication],
          let creation = structuralOccurrenceStampOccurrence (Publication.stampedStructuralStamp readerStage),
          slot <- storeSlots,
          Store.storeSlotDelta slot == delta,
          Store.StructuralApplicationReader origin owner controller _ <- [Store.storeSlotProvenance slot],
          origin == creation,
          owner == process,
          controller == target,
          let seededIds =
                [ Environment.positionedEnvironmentRootPublicationId root
                | root <- roots,
                  checkedPublicationSort (Environment.positionedEnvironmentRootChecked root) == profileSortFor role,
                  Just stage <- [Map.lookup (Environment.positionedEnvironmentRootPublicationId root) environmentStagesByPublication],
                  isJust
                    ( Publication.lookupStructuralStabilization
                        (structuralOccurrenceStampOccurrence (Publication.stampedStructuralStamp stage))
                        publicationState
                    )
                ]
        ]
    environmentStagesByPublication =
      Map.fromList
        [ (checkedPublicationId (Publication.stampedStructuralChecked stage), stage)
        | stage <- stampedStructuralStages,
          isJust (Publication.stampedStructuralEnvironmentRoot stage)
        ]
    allPublicationIds =
      fmap checkedPublicationId primordial
        <> localAllocatedIds
        <> incomingEntryKeys
    localAllocatedIds =
      entryKeys
        <> fmap Publication.applicationPublicationId structuralApplicationRecords
        <> fmap fst environmentEntries
    completedIncomingRecords =
      [ (identifier, record)
      | (identifier, record) <- incomingEntries,
        Publication.incomingPublicationDisposition record
          `elem` [Publication.Applied, Publication.TerminallyIgnored]
      ]
    authenticatedHeldIncomingRecords =
      [ (identifier, record)
      | (identifier, record) <- incomingEntries,
        Publication.incomingPublicationDisposition record
          == Publication.DependencyHeld,
        Publication.incomingPublicationSemanticallyAuthenticated record
      ]
    residentProcesses = Set.fromList (fmap snd residents)
    controlled = startupControlledState state
    structuralProgress = startupStructuralProgressState state
    storeState = startupStoreState state
    storeSlots = Store.retainedStoreSlots storeState

    terminalPurgeCoordinate publication = do
      entry <-
        SortRegistry.lookupEffectiveSort
          (checkedPublicationSort publication)
          (startupSortRegistryState state)
      let descriptor = SortRegistry.registryEntryDescriptor entry
      if descriptorKind (canonicalCheckedDescriptor descriptor) /= ControlledSort
        then Nothing
        else do
          observation <-
            either
              (const Nothing)
              Just
              ( Controlled.checkControlledObservation
                  descriptor
                  (SortRegistry.registryEntryOccurrenceId entry)
                  publication
              )
          deletion <-
            Controlled.controlledTerminalDeletion
              (Controlled.checkedControlledObservationObject observation)
              controlled
          Just
            ( (checkedPublicationSort publication, checkedPublicationKey publication),
              Controlled.controlledTerminalDeletionCause deletion
            )

    bootstrapTerminalPurgeCoordinate object =
      let process = processEpochIdFromGlobalObjectId object
          nabla = nablaIdFromGlobalObjectId object
          delta = deltaIdFromGlobalObjectId object
          objectKey = controlledObjectKey (globalUniqueIdFromGlobalObjectId object)
       in case ( Controlled.controlledProcessFact process controlled,
                 Controlled.controlledWriterFact nabla controlled,
                 Controlled.controlledReaderFact delta controlled
               ) of
            (Just _, Nothing, Nothing) ->
              Just (profileSortFor ProcessEpochRole, objectKey)
            (Nothing, Just _, Nothing) ->
              Just (profileSortFor NablaRole, objectKey)
            (Nothing, Nothing, Just _) ->
              Just (profileSortFor DeltaRole, objectKey)
            _ -> Nothing
    nextAcceptanceByProcess =
      Map.fromList
        ( Application.applicationRequestNextAcceptancePositions
            (Application.applicationRequestStateWitness (startupApplicationState state))
        )
    nextSequenceEntries = Publication.publicationWitnessNextSequences witness
    nextSequences = Map.fromList nextSequenceEntries
    sequencesByTenure =
      Map.fromListWith
        (<>)
        [ ( ( publicationNabla identifier,
              publicationAuthorityEpoch identifier
            ),
            [nablaSequenceWord64 (publicationNablaSequence identifier)]
          )
        | identifier <- localAllocatedIds
        ]
    environmentWriterTenures =
      Set.fromList
        [ ( publicationNabla identifier,
            publicationAuthorityEpoch identifier
          )
        | (identifier, _) <- environmentEntries
        ]
    expectedTenures =
      Map.keysSet sequencesByTenure
    heraldPositions =
      fmap
        ( Publication.heraldPublicationPositionWord64
            . Publication.outgoingPublicationHeraldPosition
        )
        records
        <> fmap
          ( Publication.heraldPublicationPositionWord64
              . Publication.applicationPublicationHeraldPosition
          )
          structuralApplicationRecords
        <> fmap
          ( Publication.heraldPublicationPositionWord64
              . Environment.positionedEnvironmentRootHeraldPosition
          )
          environmentPositioned
    nextHeraldPosition =
      Publication.heraldPublicationPositionWord64
        (Publication.publicationWitnessNextHeraldPosition witness)
    requests = retainedRequestEntries state
    successfulWrites =
      [ (process, position, privateNabla, resultSort)
      | (_, _, request) <- requests,
        let process = Application.applicationRequestWitnessProcess request,
        Just position <- [Application.applicationRequestWitnessPosition request],
        Just (privateNabla, resultSort) <- [successfulSortDefinitionWrite request]
      ]
    acceptedRequests =
      [ request
      | (_, _, request) <- requests,
        Just _ <- [Application.applicationRequestWitnessPosition request]
      ]

    checkedIncoming (identifier, record) = do
      let peerPublication = Publication.incomingPeerPublication record
          batch = peerPublicationBatch peerPublication
          subject = StartupProcess (publicationBatchSourceProcess batch)
      registryEntry <-
        maybe
          (fault subject PublicationIncomingInvariant)
          Right
          ( lookupRetainedRegistryEntryAt
              (publicationBatchSortId batch)
              (publicationBatchOccurrenceId batch)
              (startupSortRegistryState state)
          )
      value <-
        either
          (const (fault subject PublicationIncomingInvariant))
          Right
          ( decodeCanonicalValue
              (canonicalValueByteString (publicationBatchCanonicalValue batch))
          )
      publication <-
        either
          (const (fault subject PublicationIncomingInvariant))
          Right
          ( mkCheckedPublication
              (SortRegistry.registryEntryDescriptor registryEntry)
              identifier
              value
          )
      authenticated <-
        either
          (const (fault subject PublicationIncomingInvariant))
          Right
          ( authenticatePeerPublication
              (SortRegistry.registryEntryDescriptor registryEntry)
              (SortRegistry.registryEntryOccurrenceId registryEntry)
              publication
              peerPublication
          )
      require
        subject
        PublicationIncomingInvariant
        ( SortRegistry.registryEntryOccurrenceId registryEntry
            == publicationBatchOccurrenceId batch
            && checkedPublicationCanonicalValue publication
              == publicationBatchCanonicalValue batch
            && authenticated == peerPublication
            && Publication.incomingPublicationDigest record
              == peerPublicationDigest peerPublication
        )
      Right (identifier, publication)

    -- Onboarding uses semantic Store admission without fabricating an ordinary
    -- incoming stream or alignment transfer. Its retained checked receipt
    -- justifies only this receiver's exact mandatory system-view destination.
    checkedJoinEvidence (role, retained) = do
      let identifier = AlignmentProtocol.retainedPublicationEvidencePublicationId retained
          system = checkedSystemId (startupGenesis state)
          delta = deriveSystemViewDeltaId system localEpoch role
          incarnation = deriveSystemViewStoreIncarnationId system localEpoch role
          subject = StartupDelta delta
      registryEntry <-
        maybe
          (fault subject PublicationRouteReceiptInvariant)
          Right
          (SortRegistry.lookupPredefinedRole role (startupSortRegistryState state))
      slot <- maybe (fault subject PublicationRouteReceiptInvariant) Right (Store.lookupStoreSlot delta storeState)
      require
        subject
        PublicationRouteReceiptInvariant
        ( Store.storeSlotProvenance slot == Store.HeraldSystemView localEpoch role
            && Store.storeSlotIncarnation slot == incarnation
            && SortRegistry.registryEntrySortId registryEntry == AlignmentProtocol.retainedPublicationEvidenceSortId retained
            && SortRegistry.registryEntryOccurrenceId registryEntry == AlignmentProtocol.retainedPublicationEvidenceSortDefinitionOccurrenceId retained
        )
      value <-
        either
          (const (fault subject PublicationRouteReceiptInvariant))
          Right
          (decodeCanonicalValue (canonicalValueByteString (AlignmentProtocol.retainedPublicationEvidenceCanonicalValue retained)))
      publication <-
        either
          (const (fault subject PublicationRouteReceiptInvariant))
          Right
          (mkCheckedPublication (SortRegistry.registryEntryDescriptor registryEntry) identifier value)
      require
        subject
        PublicationRouteReceiptInvariant
        (checkedPublicationCanonicalValue publication == AlignmentProtocol.retainedPublicationEvidenceCanonicalValue retained)
      validateAlignmentEvidenceOrigin subject registryEntry publication retained
      Right (identifier, publication)

    checkedAlignmentEvidence evidence = do
      let retained =
            AlignmentTransfer.appliedDestinationEvidencePublication evidence
          identifier =
            AlignmentProtocol.retainedPublicationEvidencePublicationId retained
          subject =
            StartupDelta
              ( AlignmentProtocol.destinationStoreDelta
                  (AlignmentTransfer.appliedDestinationEvidenceDestination evidence)
              )
      registryEntry <-
        maybe
          (fault subject PublicationRouteReceiptInvariant)
          Right
          ( lookupRetainedRegistryEntryAt
              (AlignmentProtocol.retainedPublicationEvidenceSortId retained)
              (AlignmentProtocol.retainedPublicationEvidenceSortDefinitionOccurrenceId retained)
              (startupSortRegistryState state)
          )
      require
        subject
        PublicationRouteReceiptInvariant
        ( SortRegistry.registryEntryOccurrenceId registryEntry
            == AlignmentProtocol.retainedPublicationEvidenceSortDefinitionOccurrenceId retained
        )
      value <-
        either
          (const (fault subject PublicationRouteReceiptInvariant))
          Right
          ( decodeCanonicalValue
              ( canonicalValueByteString
                  (AlignmentProtocol.retainedPublicationEvidenceCanonicalValue retained)
              )
          )
      publication <-
        either
          (const (fault subject PublicationRouteReceiptInvariant))
          Right
          ( mkCheckedPublication
              (SortRegistry.registryEntryDescriptor registryEntry)
              identifier
              value
          )
      require
        subject
        PublicationRouteReceiptInvariant
        ( checkedPublicationCanonicalValue publication
            == AlignmentProtocol.retainedPublicationEvidenceCanonicalValue retained
        )
      validateAlignmentEvidenceOrigin subject registryEntry publication retained
      validateAlignmentEvidenceDisposition
        subject
        (SortRegistry.registryEntryDescriptor registryEntry)
        publication
        retained
        evidence
      Right (identifier, publication)

    -- Terminal-source repair deliberately bypasses Publication's ordinary
    -- incoming stream owner: the coordinator archive is the retained exact
    -- provenance for the publication materialized into the receiver's
    -- mandatory private system view. Re-admit its exact checked value and
    -- structural digest even while readiness is held; once a Store receipt
    -- exists, also re-check historical source authority. Expose only its one
    -- receiver-local frozen route to Store validation.
    checkedTerminalOccurrence occurrence = do
      semantics <-
        either
          (const (fault StartupStatic PublicationRouteReceiptInvariant))
          Right
          ( decodeStructuralPublicationCanonicalBytes
              (TerminalSource.terminalStructuralOccurrencePayloadBytes occurrence)
          )
      let stamp = TerminalSource.terminalStructuralOccurrenceStamp occurrence
          identifier = structuralPublicationSemanticsPublicationId semantics
          role = structuralPublicationSemanticsCarrierRole semantics
          catalogueRole = structuralCarrierCatalogueRole role
          system = checkedSystemId (startupGenesis state)
          delta = deriveSystemViewDeltaId system localEpoch catalogueRole
          incarnation =
            deriveSystemViewStoreIncarnationId system localEpoch catalogueRole
          subject = StartupDelta delta
      registryEntry <-
        maybe
          (fault subject PublicationRouteReceiptInvariant)
          Right
          ( lookupRetainedRegistryEntryAt
              (structuralPublicationSemanticsSortId semantics)
              (structuralPublicationSemanticsOccurrenceId semantics)
              (startupSortRegistryState state)
          )
      value <-
        either
          (const (fault subject PublicationRouteReceiptInvariant))
          Right
          ( decodeCanonicalValue
              ( canonicalValueByteString
                  (structuralPublicationSemanticsCanonicalValue semantics)
              )
          )
      publication <-
        either
          (const (fault subject PublicationRouteReceiptInvariant))
          Right
          ( mkCheckedPublication
              (SortRegistry.registryEntryDescriptor registryEntry)
              identifier
              value
          )
      expectedDigest <-
        either
          (const (fault subject PublicationRouteReceiptInvariant))
          Right
          ( structuralPublicationDigestForSemantics
              (SortRegistry.registryEntryDescriptor registryEntry)
              (structuralPublicationSemanticsOccurrenceId semantics)
              publication
              (structuralPublicationSemanticsSourceProcess semantics)
              (structuralPublicationSemanticsSourceStrength semantics)
              (structuralPublicationSemanticsSourceTopologyPrerequisite semantics)
              (structuralPublicationSemanticsControlPrerequisite semantics)
          )
      require
        subject
        PublicationRouteReceiptInvariant
        ( SortRegistry.registryEntryOccurrenceId registryEntry
            == structuralPublicationSemanticsOccurrenceId semantics
            && checkedPublicationCanonicalValue publication
              == structuralPublicationSemanticsCanonicalValue semantics
            && structuralOccurrenceStampPublication stamp == identifier
            && structuralOccurrenceStampCarrierRole stamp == role
            && structuralOccurrenceStampPublicationDigest stamp == expectedDigest
        )
      if terminalPublicationHasReceipt delta incarnation identifier
        then
          either
            (const (fault subject PublicationRouteReceiptInvariant))
            (const (Right ()))
            ( PeerInput.validateRoutedPublicationAuthorityHistory
                ( PeerInput.peerInputContext
                    (startupGenesis state)
                    (startupOracleProjectionState state)
                    (startupDiscoveryState state)
                    (startupPlacementState state)
                )
                structuralProgress
                controlled
                ( PeerInput.routedPublicationAuthorityClaim
                    (SortRegistry.registryEntryDescriptor registryEntry)
                    (structuralPublicationSemanticsOccurrenceId semantics)
                    publication
                    (structuralPublicationSemanticsSourceProcess semantics)
                    (structuralPublicationSemanticsSourceStrength semantics)
                    (structuralPublicationSemanticsSourceTopologyPrerequisite semantics)
                    (structuralPublicationSemanticsControlPrerequisite semantics)
                    (Just stamp)
                )
            )
        else Right ()
      route <-
        either
          (const (fault subject PublicationRouteReceiptInvariant))
          Right
          ( freezeRoute
              [ routeDestination
                  delta
                  incarnation
                  localEpoch
                  (structuralPublicationSemanticsSourceStrength semantics)
              ]
          )
      Right (identifier, publication, route)

    terminalPublicationHasReceipt delta incarnation identifier =
      case Store.lookupStoreSlot delta storeState of
        Nothing -> False
        Just slot ->
          Store.storeSlotIncarnation slot == incarnation
            && any ((== identifier) . fst) (Store.storeSlotApplicationReceipts slot)

    validateAlignmentEvidenceOrigin subject registryEntry publication retained =
      case AlignmentProtocol.retainedPublicationEvidenceOrigin retained of
        AlignmentProtocol.PrimordialRetainedObservation ->
          require
            subject
            PublicationRouteReceiptInvariant
            ( publication `elem` genesisDescriptions
                || ( publication `elem` primordial
                       && AlignmentProtocol.retainedPublicationEvidenceSourceStrength retained == Normal
                   )
            )
        AlignmentProtocol.RoutedRetainedObservation source topology control stamp ->
          either
            (const (fault subject PublicationRouteReceiptInvariant))
            (const (Right ()))
            ( PeerInput.validateRetainedStorePublication
                ( PeerInput.routedPublicationAuthorityClaim
                    (SortRegistry.registryEntryDescriptor registryEntry)
                    (AlignmentProtocol.retainedPublicationEvidenceSortDefinitionOccurrenceId retained)
                    publication
                    source
                    (AlignmentProtocol.retainedPublicationEvidenceSourceStrength retained)
                    topology
                    control
                    stamp
                )
            )

    validateAlignmentEvidenceDisposition
      subject
      descriptor
      publication
      retained
      evidence =
        case AlignmentTransfer.appliedDestinationEvidenceDisposition evidence of
          AlignmentTransfer.StoreAppliedOrAlreadyApplied ->
            require
              subject
              PublicationRouteReceiptInvariant
              ( case lookupPublicationReceiptStoreSlot evidenceDelta evidenceIncarnation storeState of
                  Just slot ->
                    Store.storeSlotIncarnation slot == evidenceIncarnation
                      && Store.storeSlotSortId slot
                        == AlignmentProtocol.retainedPublicationEvidenceSortId retained
                      && Store.storeSlotOccurrenceId slot
                        == AlignmentProtocol.retainedPublicationEvidenceSortDefinitionOccurrenceId retained
                      && maybe
                        False
                        (>= AlignmentTransfer.appliedDestinationEvidenceStrength evidence)
                        ( lookup
                            (checkedPublicationId publication)
                            (Store.storeSlotApplicationReceipts slot)
                        )
                  Nothing -> False
              )
          AlignmentTransfer.StoreTerminallyIgnored -> do
            observation <-
              either
                (const (fault subject PublicationRouteReceiptInvariant))
                Right
                ( Store.retainedStoreObservation
                    publication
                    (AlignmentTransfer.appliedDestinationEvidenceStrength evidence)
                    (AlignmentProtocol.retainedPublicationEvidenceSortDefinitionOccurrenceId retained)
                    (alignmentStoreObservationOrigin retained)
                )
            prepared <-
              either
                (const (fault subject PublicationRouteReceiptInvariant))
                Right
                ( Store.prepareAlignmentStoreApplication
                    ( NonEmpty.singleton
                        ( Store.peerStoreDestination
                            evidenceDelta
                            evidenceIncarnation
                            (AlignmentTransfer.appliedDestinationEvidenceStrength evidence)
                        )
                    )
                    observation
                    storeState
                )
            let (successor, outcomes) = Store.commitPeerStoreApplication prepared
            require
              subject
              PublicationRouteReceiptInvariant
              ( successor == storeState
                  && fmap Store.peerStoreOutcomeDisposition (NonEmpty.toList outcomes)
                    == [Store.PeerStoreTerminallyIgnored]
              )
          AlignmentTransfer.ControlledSuppressed -> do
            observation <-
              either
                (const (fault subject PublicationRouteReceiptInvariant))
                Right
                ( Controlled.checkControlledObservation
                    descriptor
                    (AlignmentProtocol.retainedPublicationEvidenceSortDefinitionOccurrenceId retained)
                    publication
                )
            prepared <-
              either
                (const (fault subject PublicationRouteReceiptInvariant))
                Right
                ( Controlled.prepareControlledPeerObservation
                    observation
                    controlled
                )
            let (successor, disposition) =
                  Controlled.commitControlledPeerObservation prepared
            require
              subject
              PublicationRouteReceiptInvariant
              ( successor == controlled
                  && disposition == Controlled.ControlledPeerCurrentSuppressed
              )
        where
          destination = AlignmentTransfer.appliedDestinationEvidenceDestination evidence
          evidenceDelta = AlignmentProtocol.destinationStoreDelta destination
          evidenceIncarnation =
            AlignmentProtocol.destinationStoreIncarnation destination

    alignmentStoreObservationOrigin retained =
      case AlignmentProtocol.retainedPublicationEvidenceOrigin retained of
        AlignmentProtocol.PrimordialRetainedObservation ->
          Store.PrimordialStoreObservation
        AlignmentProtocol.RoutedRetainedObservation source topology control stamp ->
          Store.RoutedStoreObservation
            ( Store.storeObservationEnvelope
                source
                (AlignmentProtocol.retainedPublicationEvidenceSortDefinitionOccurrenceId retained)
                topology
                control
                stamp
            )

    validateRecord (key, record) = do
      let publication = Publication.outgoingPublicationChecked record
          identifier = Publication.outgoingPublicationId record
          process = Publication.outgoingPublicationSourceProcess record
          position = Publication.outgoingPublicationAcceptancePosition record
          subject = StartupProcess process
          nabla = publicationNabla identifier
          applicationRecord =
            Publication.lookupApplicationPublication position publicationState
      -- End retires the live application allocator, while immutable outgoing
      -- records continue to prove earlier publications and delivery work.
      require
        subject
        PublicationAllocationInvariant
        ( case Map.lookup process nextAcceptanceByProcess of
            Just nextAcceptance ->
              process `Set.member` residentProcesses
                && ApplicationRequest.processAcceptancePositionOrdinal position < nextAcceptance
            Nothing -> isJust (OracleProjection.oracleViewEndedProcess process oracleView)
        )
      authority <-
        retainedApplicationPublicationAuthority
          subject
          nabla
          (publicationAuthorityEpoch identifier)
          (Publication.outgoingPublicationSourceTopologyPrerequisite record)
          (Publication.outgoingPublicationControlPrerequisite record)
      effective <-
        maybe
          (fault subject PublicationRecordInvariant)
          Right
          ( lookupRetainedRegistryEntryAt
              (checkedPublicationSort publication)
              (Publication.outgoingPublicationOccurrenceId record)
              (startupSortRegistryState state)
          )
      validateRetainedApplicationPublicationControl
        subject
        publication
        (SortRegistry.registryEntryDescriptor effective)
        (Publication.outgoingPublicationSourceTopologyPrerequisite record)
        (Publication.outgoingPublicationControlPrerequisite record)
        authority
      case applicationRecord of
        Just generic
          | Publication.applicationPublicationOutgoingRecord generic == Just record ->
              Right ()
        _ -> do
          writer <-
            maybe
              (fault subject PublicationRecordInvariant)
              Right
              (Controlled.controlledWriterFact nabla controlled)
          carrier <-
            maybe
              (fault subject PublicationRecordInvariant)
              Right
              ( SortRegistry.lookupPredefinedRole
                  SortDefinitionRole
                  (startupSortRegistryState state)
              )
          _ <-
            either
              (const (fault subject PublicationRecordInvariant))
              Right
              (decodeSortDefinitionValue (checkedPublicationValue publication))
          require
            subject
            PublicationRecordInvariant
            ( Controlled.rootFactCatalogueRole writer == SortDefinitionRole
                && checkedPublicationSort publication
                  == SortRegistry.registryEntrySortId carrier
            )
      require
        subject
        PublicationRecordInvariant
        ( key == identifier
            && checkedPublicationId publication == identifier
            && publicationSourceHeraldEpoch identifier == localEpoch
            && ApplicationRequest.processAcceptancePositionProcess position == process
            && ApplicationRequest.processAcceptancePositionOrdinal position >= 1
            && Authority.nablaAuthorityResolutionController authority == process
            && Authority.nablaAuthorityResolutionResidence authority == localEpoch
            && Authority.nablaAuthorityResolutionAuthority authority
              == publicationAuthorityEpoch identifier
            && StructuralDebt.sortOccurrenceDefinition
              (Authority.nablaAuthorityResolutionSort authority)
              == Publication.outgoingPublicationOccurrenceId record
            && Authority.nablaAuthorityResolutionMinimumControlPrerequisite authority
              <= Publication.outgoingPublicationControlPrerequisite record
            && StructuralDebt.sortOccurrenceSortId
              (Authority.nablaAuthorityResolutionSort authority)
              == checkedPublicationSort publication
            && checkedPublicationSort publication
              == SortRegistry.registryEntrySortId effective
            && not (Publication.outgoingPublicationHasFenceDependency record)
        )
      traverse_
        (validateRouteReceipt record identifier publication)
        (routeDestinations (Publication.outgoingPublicationRoute record))
      traverse_
        (validateRemoteBatch subject record)
        (Map.toAscList (Publication.outgoingPublicationRemoteBatches record))

    validateApplicationLink record = do
      let publication = Publication.applicationPublicationChecked record
          identifier = Publication.applicationPublicationId record
          process = Publication.applicationPublicationSourceProcess record
          nabla = publicationNabla identifier
          subject = StartupProcess process
      authority <-
        retainedApplicationPublicationAuthority
          subject
          nabla
          (publicationAuthorityEpoch identifier)
          (Publication.applicationPublicationSourceTopologyPrerequisite record)
          (Publication.applicationPublicationControlPrerequisite record)
      effective <-
        maybe
          (fault subject PublicationRecordInvariant)
          Right
          ( lookupRetainedRegistryEntryAt
              (checkedPublicationSort publication)
              (Publication.applicationPublicationSortOccurrenceId record)
              (startupSortRegistryState state)
          )
      let descriptor = SortRegistry.registryEntryDescriptor effective
          retention = Publication.applicationPublicationRetention record
      validateRetainedApplicationPublicationControl
        subject
        publication
        descriptor
        (Publication.applicationPublicationSourceTopologyPrerequisite record)
        (Publication.applicationPublicationControlPrerequisite record)
        authority
      admittedMembership <-
        maybe
          (fault subject PublicationRecordInvariant)
          Right
          ( OracleProjection.oracleViewHeraldMembershipById
              (Publication.applicationPublicationMembershipGenerationId record)
              ( OracleProjection.oracleView
                  (startupOracleProjectionState state)
              )
          )
      require
        subject
        PublicationRecordInvariant
        ( checkedPublicationValue publication
            == Publication.admittedApplicationPublicationValue
              (Publication.applicationPublicationAdmittedValue record)
            && Authority.nablaAuthorityResolutionController authority == process
            && Authority.nablaAuthorityResolutionResidence authority == localEpoch
            && Authority.nablaAuthorityResolutionAuthority authority
              == publicationAuthorityEpoch identifier
            && StructuralDebt.sortOccurrenceSortId
              (Authority.nablaAuthorityResolutionSort authority)
              == checkedPublicationSort publication
            && StructuralDebt.sortOccurrenceDefinition
              (Authority.nablaAuthorityResolutionSort authority)
              == Publication.applicationPublicationSortOccurrenceId record
            && Authority.nablaAuthorityResolutionMinimumControlPrerequisite authority
              <= Publication.applicationPublicationControlPrerequisite record
            && SortRegistry.registryEntryOccurrenceId effective
              == Publication.applicationPublicationSortOccurrenceId record
        )
      case descriptorStructuralCarrierRole (canonicalCheckedDescriptor descriptor) of
        Nothing -> Right ()
        Just role -> do
          mandatoryRoute <-
            either
              (const (fault subject PublicationRecordInvariant))
              Right
              ( attenuateFrozenRoute
                  (Publication.applicationPublicationSourceStrength record)
                  <$> structuralApplicationRoute admittedMembership role
              )
          require
            subject
            PublicationRecordInvariant
            ( structuralApplicationRouteValid
                admittedMembership
                (Publication.applicationPublicationSourceStrength record)
                (checkedPublicationSort publication)
                (Publication.applicationPublicationSortOccurrenceId record)
                mandatoryRoute
                (Publication.applicationPublicationRoute record)
            )
      if descriptorKind (canonicalCheckedDescriptor descriptor) /= ControlledSort
        then
          require
            subject
            PublicationRecordInvariant
            ( Publication.applicationPublicationOrigin record
                == Publication.RegularApplicationPublicationOrigin
                && Publication.applicationPublicationSourceStrength record == Normal
                && Publication.applicationPublicationPossession process retention == Nothing
                && Publication.applicationPublicationSuppression retention == Nothing
            )
        else do
          observation <-
            either
              (const (fault subject PublicationRecordInvariant))
              Right
              ( Controlled.checkControlledObservation
                  descriptor
                  (Publication.applicationPublicationSortOccurrenceId record)
                  publication
              )
          let object = Controlled.checkedControlledObservationObject observation
              controlledRecord = Controlled.controlledLocalRecord object controlled
              terminalDeletion = Controlled.controlledTerminalDeletion object controlled
              retainedSort =
                maybe
                  (checkedPublicationSort publication)
                  Controlled.controlledRecordSortId
                  controlledRecord
          case (controlledRecord, terminalDeletion) of
            (Just retained, Nothing) ->
              require
                subject
                PublicationRecordInvariant
                ( Controlled.controlledRecordSortId retained
                    == checkedPublicationSort publication
                    && Controlled.controlledRecordOccurrenceId retained
                      == Publication.applicationPublicationSortOccurrenceId record
                    && Controlled.controlledRecordObservationLabelCompatible
                      retained
                      observation
                      controlled
                    && Controlled.controlledRecordStructuralRole retained
                      == Controlled.checkedControlledObservationStructuralRole observation
                    && Controlled.controlledRecordObservation identifier retained
                      == Just publication
                )
            (Nothing, Just deletion) ->
              require
                subject
                PublicationRecordInvariant
                ( Controlled.controlledTerminalDeletionObject deletion == object
                    && not (Controlled.controlledHasAnyNormalPossession object controlled)
                )
            _ -> fault subject PublicationRecordInvariant
          case Publication.applicationPublicationSuppression retention of
            Nothing -> Right ()
            Just suppressed ->
              require
                subject
                PublicationRecordInvariant
                ( suppressed == object
                    && case controlledRecord of
                      Just retained ->
                        Controlled.controlledRecordSuppression retained
                          /= Controlled.ControlledUnsuppressed
                      Nothing -> isJust terminalDeletion
                )
          case Publication.applicationPublicationOrigin record of
            Publication.ControlledFirstUseApplicationPublicationOrigin generated -> do
              reservation <-
                maybe
                  (fault subject PublicationRecordInvariant)
                  Right
                  (Controlled.controlledReservationWitness generated controlled)
              require
                subject
                PublicationRecordInvariant
                ( globalObjectIdFromGlobalUniqueId generated == object
                    && Publication.applicationPublicationSourceStrength record == Normal
                    && Publication.applicationPublicationPossession process retention
                      == Just (process, object)
                    && retainedPossessionMatches process object
                    && Controlled.reservationWitnessPhase reservation == Controlled.Published
                    && Controlled.reservationWitnessFirstPublicationId reservation
                      == Just identifier
                    && Controlled.reservationWitnessSortId reservation
                      == retainedSort
                )
            Publication.ControlledUpdateApplicationPublicationOrigin retainedObject ->
              require
                subject
                PublicationRecordInvariant
                ( retainedObject == object
                    && Publication.applicationPublicationSourceStrength record == Normal
                    && Publication.applicationPublicationPossession process retention
                      == Just (process, object)
                    && retainedPossessionMatches process object
                )
            Publication.ForwardApplicationPublicationOrigin retainedObject sourceStrength ->
              require
                subject
                PublicationRecordInvariant
                ( retainedObject == object
                    && sourceStrength
                      == Publication.applicationPublicationSourceStrength record
                    && Publication.applicationPublicationPossession process retention
                      == Just (process, object)
                )
            Publication.RegularApplicationPublicationOrigin ->
              fault subject PublicationRecordInvariant

    -- Increment 8 can advance StructuralProgress through a detached regular
    -- Resolve while the live Oracle projection deliberately remains unchanged
    -- until Increment 9.  A structural carrier joins that Resolve floor into
    -- its one retained control prerequisite, so source-authority authentication
    -- must use the highest Oracle coordinate actually present and check the
    -- joined floor separately.  Refuse the fallback if a locally retained label
    -- release lies in the unprojected interval: clamping is sound only for the
    -- detached regular-retirement controls, which introduce no label facts.
    retainedApplicationPublicationAuthority
      subject
      nabla
      claimedAuthority
      sourceTopology
      retainedControl = do
        let projectedControl = OracleProjection.oracleViewControlIndex oracleView
            authorityControl = min retainedControl projectedControl
            unprojectedRelease = case Controlled.controlledReleasedLabelRecord
              (globalObjectIdFromNablaId nabla)
              controlled of
              Nothing -> False
              Just record ->
                let releaseControl =
                      labelRevisionControlIndex (labelRecordRevision record)
                 in releaseControl > authorityControl
                      && releaseControl <= retainedControl
        require
          subject
          PublicationRecordInvariant
          ( retainedControl
              <= GraphProgress.structuralAppliedControlPrefix structuralProgress
              && not unprojectedRelease
          )
        resolution <-
          either
            (const (fault subject PublicationRecordInvariant))
            Right
            ( Authority.resolveNablaAuthorityAt
                nabla
                claimedAuthority
                sourceTopology
                authorityControl
                structuralProgress
                (startupOracleProjectionState state)
            )
        require
          subject
          PublicationRecordInvariant
          ( Authority.nablaAuthorityResolutionMinimumControlPrerequisite resolution
              <= retainedControl
          )
        Right resolution

    -- The retained control coordinate is the exact join produced by local
    -- admission, not merely an arbitrary later applied prefix.  Use the
    -- retirement history at-or-before that coordinate so immutable old
    -- carriers continue to validate after another retirement advances the
    -- current epoch.
    validateRetainedApplicationPublicationControl
      subject
      publication
      descriptor
      sourceTopology
      retainedControl
      authority = do
        structuralReferences <-
          either
            (const (fault subject PublicationRecordInvariant))
            Right
            ( structuralCarrierSortReferences
                descriptor
                (checkedPublicationValue publication)
            )
        definitionReferences <-
          if checkedPublicationSort publication == profileSortFor SortDefinitionRole
            then
              either
                (const (fault subject PublicationRecordInvariant))
                (Right . pure . descriptorSortId)
                (decodeSortDefinitionValue (checkedPublicationValue publication))
            else Right []
        topologyControl <- sourceTopologyControlPrerequisite subject sourceTopology
        let writerControl =
              max
                topologyControl
                (Authority.nablaAuthorityResolutionMinimumControlPrerequisite authority)
            referenceControls =
              fmap
                (regularRetirementFloorAt retainedControl)
                (structuralReferences <> definitionReferences)
            expectedControl = foldl max writerControl referenceControls
        require
          subject
          PublicationRecordInvariant
          (retainedControl == expectedControl)

    sourceTopologyControlPrerequisite subject sourceTopology
      | sourceTopology == GraphProgress.structuralGenesisCutId structuralProgress =
          Right (controlIndex 0)
      | otherwise = do
          installed <-
            maybe
              (fault subject PublicationRecordInvariant)
              Right
              ( GraphProgress.lookupInstalledTopologyCut
                  sourceTopology
                  structuralProgress
              )
          Right
            ( topologyFrontierAppliedControlPrefix
                (topologyCutFrontier (GraphProgress.installedTopologyCutCut installed))
            )

    regularRetirementFloorAt :: ControlIndex -> SortId -> ControlIndex
    regularRetirementFloorAt retainedControl sortId =
      foldl
        max
        (controlIndex 0)
        [ resolveIndex
        | retirement <-
            SortRegistry.regularSortRetirements (startupSortRegistryState state),
          SortRegistry.regularSortRetirementSortId retirement
            == sortId,
          let resolveIndex = SortRegistry.regularSortRetirementResolveIndex retirement,
          resolveIndex <= retainedControl
        ]

    retainedPossessionMatches process object =
      case Controlled.controlledEffectiveLabelState object controlled of
        Just released
          | releasedLabelStateIsDeleted released ->
              not (Controlled.controlledHasAnyNormalPossession object controlled)
        _
          | Controlled.controlledProcessEnded process controlled ->
              controlledPossessionEntriesAbsent process object controlled
        _ -> Controlled.controlledHasNormalPossession process object controlled

    structuralApplicationRoute membership role =
      freezeRoute
        [ routeDestination
            (deriveSystemViewDeltaId system herald catalogueRole)
            (deriveSystemViewStoreIncarnationId system herald catalogueRole)
            herald
            Normal
        | herald <- membershipHeralds membership
        ]
      where
        system = checkedSystemId (startupGenesis state)
        catalogueRole = structuralCarrierCatalogueRole role

    structuralApplicationRouteValid membership sourceStrength sortId occurrence mandatory observed =
      all (`elem` observedDestinations) mandatoryDestinations
        && all permitted observedDestinations
      where
        mandatoryDestinations = routeDestinations mandatory
        observedDestinations = routeDestinations observed
        fixedHeralds = Set.fromList (membershipHeralds membership)
        allPrivateSystemViewDeltas =
          Set.fromList
            [ deriveSystemViewDeltaId
                (checkedSystemId (startupGenesis state))
                herald
                role
            | herald <- Set.toAscList fixedHeralds,
              role <- allPredefinedSortRoles
            ]
        mandatoryDeltas = Set.fromList (fmap destinationDelta mandatoryDestinations)
        permitted destination =
          destinationStrength destination <= sourceStrength
            && Set.member (destinationHerald destination) fixedHeralds
            && if Set.member
              (destinationDelta destination)
              allPrivateSystemViewDeltas
              then Set.member (destinationDelta destination) mandatoryDeltas
              else
                Placement.retainedPlacementRouteMatches
                  (destinationHerald destination)
                  (destinationDelta destination)
                  (destinationStoreIncarnation destination)
                  sortId
                  occurrence
                  (startupPlacementState state)

    structuralCarrierCatalogueRole = \case
      NeutralVertexCarrier -> NeutralVertexRole
      EdgeCarrier -> EdgeRole
      NablaCarrier -> NablaRole
      DeltaCarrier -> DeltaRole
      ProcessEpochCarrier -> ProcessEpochRole

    membershipHeralds =
      NonEmpty.toList . heraldMembershipGenerationActiveHeraldEpochs

    validateRouteReceipt record identifier publication destination = do
      let delta = destinationDelta destination
          incarnation = destinationStoreIncarnation destination
          subject = StartupDelta delta
      if destinationHerald destination == localEpoch
        then do
          slot <-
            maybe
              (fault subject PublicationRouteReceiptInvariant)
              Right
              (lookupPublicationReceiptStoreSlot delta incarnation storeState)
          require
            subject
            PublicationRouteReceiptInvariant
            ( incarnation == Store.storeSlotIncarnation slot
                && checkedPublicationSort publication == Store.storeSlotSortId slot
                && lookup identifier (Store.storeSlotApplicationReceipts slot)
                  == Just (destinationStrength destination)
            )
        else
          require
            subject
            PublicationRemoteAssignmentInvariant
            ( Store.lookupStoreSlot delta storeState == Nothing
                && case Map.lookup
                  (destinationHerald destination)
                  (Publication.outgoingPublicationRemoteBatches record) of
                  Nothing -> False
                  Just batch ->
                    any
                      ( \batchDestination ->
                          publicationDestinationDelta batchDestination == delta
                            && publicationDestinationStoreIncarnation batchDestination
                              == destinationStoreIncarnation destination
                            && publicationDestinationStrength batchDestination
                              == destinationStrength destination
                      )
                      (publicationBatchDestinations batch)
            )

    validateRemoteBatch subject record (peer, batch) = do
      let expectedDestinations =
            sort
              [ ( destinationDelta destination,
                  destinationStoreIncarnation destination,
                  destinationStrength destination
                )
              | destination <- routeDestinations (Publication.outgoingPublicationRoute record),
                destinationHerald destination == peer
              ]
          observedDestinations =
            fmap
              ( \destination ->
                  ( publicationDestinationDelta destination,
                    publicationDestinationStoreIncarnation destination,
                    publicationDestinationStrength destination
                  )
              )
              (NonEmpty.toList (publicationBatchDestinations batch))
      require
        subject
        PublicationRemoteAssignmentInvariant
        ( publicationBatchId batch == Publication.outgoingPublicationId record
            && publicationBatchSourceProcess batch
              == Publication.outgoingPublicationSourceProcess record
            && publicationBatchSortId batch
              == checkedPublicationSort (Publication.outgoingPublicationChecked record)
            && publicationBatchOccurrenceId batch
              == Publication.outgoingPublicationOccurrenceId record
            && publicationBatchCanonicalValue batch
              == checkedPublicationCanonicalValue
                (Publication.outgoingPublicationChecked record)
            && publicationBatchSourceStrength batch
              == Publication.outgoingPublicationSourceStrength record
            && publicationBatchSourceTopologyPrerequisite batch
              == Publication.outgoingPublicationSourceTopologyPrerequisite record
            && publicationBatchControlPrerequisite batch
              == Publication.outgoingPublicationControlPrerequisite record
            && not (publicationBatchHasFenceDependency batch)
            && observedDestinations == expectedDestinations
            && Publication.outgoingPublicationDispatchCertified record
        )

    validateStampedStructuralStage (occurrence, stage) = do
      let stamp = Publication.stampedStructuralStamp stage
          checked = Publication.stampedStructuralChecked stage
          sourceProcess = Publication.stampedStructuralSourceProcess stage
          sourceIdentifier = checkedPublicationId checked
          subject =
            StartupProcess
              sourceProcess
          sourceRetained =
            case Publication.stampedStructuralApplicationRecordMaybe stage of
              Just application ->
                Publication.lookupApplicationPublication
                  (Publication.applicationPublicationAcceptancePosition application)
                  publicationState
                  == Just application
              Nothing ->
                case Publication.stampedStructuralEnvironmentRoot stage of
                  Just (manifest, root) ->
                    maybe
                      False
                      (`Environment.environmentManifestExtends` manifest)
                      ( Publication.lookupEnvironmentManifest
                          (Environment.environmentManifestKeyOf manifest)
                          publicationState
                      )
                      && Publication.lookupPositionedEnvironmentRoot
                        sourceIdentifier
                        publicationState
                        == Just root
                      && StructuralSettlement.environmentStructuralStageLineageEvidence
                        manifest
                        stage
                        state
                  Nothing -> False
          expectedPeers = do
            membership <-
              OracleProjection.oracleViewHeraldMembershipById
                ( structuralVersionVectorMembershipGenerationId
                    (structuralOccurrenceStampPredecessor stamp)
                )
                oracleView
            pure
              ( Set.delete
                  localEpoch
                  ( Set.fromList
                      ( NonEmpty.toList
                          (heraldMembershipGenerationActiveHeraldEpochs membership)
                      )
                  )
              )
          peerSetMatches =
            expectedPeers
              == Just
                (Map.keysSet (Publication.stampedStructuralPeerPublications stage))
      require
        subject
        PublicationRecordInvariant
        ( occurrence == structuralOccurrenceStampOccurrence stamp
            && structuralOccurrenceStampPublication stamp
              == sourceIdentifier
            && sourceRetained
            && peerSetMatches
        )
      traverse_
        (validateStampedStructuralPeer subject stage stamp)
        (Map.toAscList (Publication.stampedStructuralPeerPublications stage))

    validateStampedStructuralPeer subject stage stamp (peer, peerPublication) = do
      let checked = Publication.stampedStructuralChecked stage
          batch = peerPublicationBatch peerPublication
          expectedDestinations = do
            membership <-
              OracleProjection.oracleViewHeraldMembershipById
                (structuralVersionVectorMembershipGenerationId (structuralOccurrenceStampPredecessor stamp))
                oracleView
            rawDeliveryRoute <-
              either
                (const Nothing)
                Just
                ( PublicationRoute.extendStructuralSystemViews
                    (checkedSystemId (startupGenesis state))
                    membership
                    (structuralOccurrenceStampCarrierRole stamp)
                    (Publication.stampedStructuralRoute stage)
                )
            let deliveryRoute = attenuateFrozenRoute (Publication.stampedStructuralSourceStrength stage) rawDeliveryRoute
            pure
              ( Set.fromList
                  [ (destinationDelta destination, destinationStoreIncarnation destination, destinationStrength destination)
                  | destination <- routeDestinations deliveryRoute,
                    destinationHerald destination == peer
                  ]
              )
          observedDestinations =
            Set.fromList
              [ (publicationDestinationDelta destination, publicationDestinationStoreIncarnation destination, publicationDestinationStrength destination)
              | destination <- NonEmpty.toList (publicationBatchDestinations batch)
              ]
      require
        subject
        PublicationRemoteAssignmentInvariant
        ( peerPublicationKind peerPublication == StructuralPublicationKind
            && exactlyOneStructuralSourceKind stage
            && publicationBatchId batch
              == checkedPublicationId checked
            && publicationBatchSourceProcess batch
              == Publication.stampedStructuralSourceProcess stage
            && publicationBatchSortId batch == checkedPublicationSort checked
            && publicationBatchOccurrenceId batch
              == Publication.stampedStructuralSortOccurrenceId stage
            && publicationBatchCanonicalValue batch
              == checkedPublicationCanonicalValue checked
            && publicationBatchSourceStrength batch
              == Publication.stampedStructuralSourceStrength stage
            && publicationBatchSourceTopologyPrerequisite batch
              == Publication.stampedStructuralSourceTopologyPrerequisite stage
            && publicationBatchControlPrerequisite batch
              == Publication.stampedStructuralControlPrerequisite stage
            && structuralOccurrenceStampPublication stamp == publicationBatchId batch
            && expectedDestinations == Just observedDestinations
            && Publication.stampedStructuralDispatchCertified stage
        )

    exactlyOneStructuralSourceKind stage =
      case ( isJust (Publication.stampedStructuralApplicationRecordMaybe stage),
             isJust (Publication.stampedStructuralEnvironmentRoot stage)
           ) of
        (True, False) -> True
        (False, True) -> True
        _ -> False

    validateSuccessfulWrite (process, position, privateNabla, resultSort) = do
      let subject = StartupProcess process
          matching =
            [ record
            | record <- records,
              Publication.outgoingPublicationSourceProcess record == process,
              Publication.outgoingPublicationAcceptancePosition record == position
            ]
      record <- case matching of
        [one] -> Right one
        _ -> fault subject PublicationRecordInvariant
      descriptor <-
        either
          (const (fault subject PublicationRecordInvariant))
          Right
          ( decodeSortDefinitionValue
              (checkedPublicationValue (Publication.outgoingPublicationChecked record))
          )
      globalNabla <-
        either
          (const (fault subject PublicationRecordInvariant))
          Right
          ( Application.resolveApplicationPrivateUniqueId
              process
              (privateNablaUniqueId privateNabla)
              (startupApplicationState state)
          )
      require
        subject
        PublicationRecordInvariant
        ( descriptorSortId descriptor == resultSort
            && publicationNabla (Publication.outgoingPublicationId record)
              == nablaIdFromGlobalObjectId
                (globalObjectIdFromGlobalUniqueId globalNabla)
        )

    validateRequestPublicationOverlap request = do
      let process = Application.applicationRequestWitnessProcess request
          subject = StartupProcess process
          position = Application.applicationRequestWitnessPosition request
          overlaps =
            [ record
            | record <- records,
              Publication.outgoingPublicationSourceProcess record == process,
              Just (Publication.outgoingPublicationAcceptancePosition record) == position
            ]
          application =
            position
              >>= (`Publication.lookupApplicationPublication` publicationState)
      case application of
        Nothing ->
          require
            subject
            PublicationRecordInvariant
            (null overlaps || successfulSortDefinitionWrite request /= Nothing)
        Just record -> do
          require
            subject
            PublicationRecordInvariant
            ( Publication.applicationPublicationSourceProcess record == process
                && Just (Publication.applicationPublicationAcceptancePosition record)
                  == position
                && case Publication.applicationPublicationOutgoingRecord record of
                  Nothing -> null overlaps
                  Just outgoing -> overlaps == [outgoing]
            )
          validateApplicationRequestSource subject process request record

    validateApplicationRequestSource subject process request record =
      case Application.applicationRequestWitnessCall request of
        WriteApplication privateNabla (PublishValue _) -> do
          requireApplicationWriter subject process privateNabla record
          case Publication.applicationPublicationOrigin record of
            Publication.ForwardApplicationPublicationOrigin {} ->
              fault subject PublicationRecordInvariant
            _ -> Right ()
        ForwardApplication privateNabla privateObject -> do
          requireApplicationWriter subject process privateNabla record
          target <-
            either
              (const (fault subject PublicationRecordInvariant))
              (Right . globalObjectIdFromGlobalUniqueId)
              ( Application.resolveApplicationPrivateUniqueId
                  process
                  (privateObjectUniqueId privateObject)
                  (startupApplicationState state)
              )
          require
            subject
            PublicationRecordInvariant
            ( case Publication.applicationPublicationOrigin record of
                Publication.ForwardApplicationPublicationOrigin object _ ->
                  object == target
                _ -> False
            )
        _ -> fault subject PublicationRecordInvariant

    requireApplicationWriter subject process privateNabla record = do
      writer <-
        either
          (const (fault subject PublicationRecordInvariant))
          Right
          ( Application.resolveApplicationPrivateUniqueId
              process
              (privateNablaUniqueId privateNabla)
              (startupApplicationState state)
          )
      require
        subject
        PublicationRecordInvariant
        ( publicationNabla (Publication.applicationPublicationId record)
            == nablaIdFromGlobalObjectId
              (globalObjectIdFromGlobalUniqueId writer)
        )

-- Terminal publication receipts name an immutable Store incarnation, which
-- remains retained after passivation or replacement. Prefer the active exact
-- incarnation so live receipt checks still inspect its current owner product.
lookupPublicationReceiptStoreSlot ::
  DeltaId -> StoreIncarnationId -> Store.State -> Maybe Store.StoreSlot
lookupPublicationReceiptStoreSlot delta incarnation store =
  case Store.lookupStoreSlot delta store of
    Just slot
      | Store.storeSlotIncarnation slot == incarnation -> Just slot
    _ -> do
      slot <- Store.lookupRetainedStoreSlot incarnation store
      if Store.storeSlotDelta slot == delta
        then Just slot
        else Nothing

validateIncomingPublications ::
  HeraldState ->
  Either HeraldInvariantFault ()
validateIncomingPublications state = do
  streamWitness <-
    either
      (const (fault StartupStatic PublicationIncomingInvariant))
      Right
      (PeerStream.peerStreamStateWitness peerStreamState)
  retainedByDirection <-
    traverse
      retainedForDirection
      (PeerStream.peerStreamWitnessIncomingDirections streamWitness)
  let retained = concat retainedByDirection
      retainedKeys = fmap retainedKey retained
      admittedRetained =
        [ ( retainedKey retainedItem,
            publicationBatchId (peerPublicationBatch peerPublication)
          )
        | retainedItem@(_, item, status) <- retained,
          status /= PeerStream.GapRetained,
          PeerLogicalPublication peerPublication <- [sequencedItemPayload item]
        ]
  require
    StartupStatic
    PublicationIncomingInvariant
    ( unique incomingKeys
        && unique assignmentClaimKeys
        && unique retainedKeys
        && Map.fromList assignmentOwnerEntries == expectedAssignmentOwners
        && Map.fromList admittedRetained == expectedAssignmentOwners
        && Map.fromList dependencyWaiterEntries == expectedDependencyWaiters
        && all (not . Set.null . snd) dependencyWaiterEntries
    )
  traverse_ validateRecord incomingEntries
  traverse_ validateRetained retained
  where
    publicationState = startupPublicationState state
    peerStreamState = startupPeerStreamState state
    storeState = startupStoreState state
    localEpoch = checkedLocalHeraldEpoch (startupGenesis state)
    oracleView =
      OracleProjection.oracleView (startupOracleProjectionState state)
    currentControl = OracleProjection.oracleViewControlIndex oracleView
    structuralProgress = startupStructuralProgressState state
    incomingEntries = Publication.incomingPublicationEntries publicationState
    incomingKeys = fmap fst incomingEntries
    incomingById = Map.fromList incomingEntries
    assignmentOwnerEntries =
      Publication.incomingAssignmentEntries publicationState
    dependencyWaiterEntries =
      Publication.incomingDependencyWaiterEntries publicationState
    assignmentClaims =
      [ ( ( assignmentReceiptDirection receipt,
            assignmentReceiptSequence receipt
          ),
          identifier
        )
      | (identifier, record) <- incomingEntries,
        receipt <- Set.toAscList (Publication.incomingPublicationAssignments record)
      ]
    assignmentClaimKeys = fmap fst assignmentClaims
    expectedAssignmentOwners = Map.fromList assignmentClaims
    expectedDependencyWaiters =
      Map.fromListWith
        Set.union
        [ (dependency, Set.singleton identifier)
        | (identifier, record) <- incomingEntries,
          dependency <- Set.toAscList (Publication.incomingPublicationDependencies record)
        ]

    retainedForDirection direction =
      either
        (const (fault StartupStatic PublicationIncomingInvariant))
        (Right . fmap (\(item, status) -> (direction, item, status)))
        (PeerStream.incomingRetainedItems direction peerStreamState)

    retainedKey (_, item, _) =
      (sequencedItemDirection item, sequencedItemSequence item)

    validateRetained (_, item, status) = case sequencedItemPayload item of
      PeerLogicalPublication peerPublication -> case status of
        PeerStream.GapRetained ->
          require
            StartupStatic
            PublicationIncomingInvariant
            (Map.notMember key expectedAssignmentOwners)
        PeerStream.IncomingReceivedPending ->
          validateAdmittedRetained
            item
            peerPublication
            key
            (== Publication.DependencyHeld)
        PeerStream.IncomingCompleted ->
          validateAdmittedRetained
            item
            peerPublication
            key
            ( \disposition ->
                disposition == Publication.Applied
                  || disposition == Publication.TerminallyIgnored
                  || disposition == Publication.ProtocolRejected
            )
      PeerLogicalRouteCutover marker -> case status of
        PeerStream.GapRetained ->
          require
            StartupStatic
            PublicationIncomingInvariant
            (Map.notMember key expectedAssignmentOwners)
        PeerStream.IncomingReceivedPending ->
          require
            StartupStatic
            PublicationIncomingInvariant
            ( markerClaimsMatch item marker
                && marker
                  `notElem` retainedRouteCutoverMarkers
            )
        PeerStream.IncomingCompleted ->
          require
            StartupStatic
            PublicationIncomingInvariant
            ( markerClaimsMatch item marker
                && ( retainedItemSourceRetired item
                       || ( markerHasSourceAcceptance marker
                              && marker `elem` retainedRouteCutoverMarkers
                          )
                       || routeCutoverMarkerTerminallyRejected item marker
                   )
            )
      PeerLogicalDisappearanceProbeMarker marker ->
        require
          StartupStatic
          PublicationIncomingInvariant
          ( DisappearanceProtocol.disappearancePublicationMarkerDirection marker == sequencedItemDirection item
              && DisappearanceProtocol.disappearancePublicationMarkerSequence marker == sequencedItemSequence item
              && DisappearanceProtocol.disappearancePublicationMarkerDigest marker == sequencedItemDigest item
              && case status of
                PeerStream.GapRetained -> Map.notMember key expectedAssignmentOwners
                PeerStream.IncomingReceivedPending -> True
                PeerStream.IncomingCompleted -> retainedItemSourceRetired item || disappearanceMarkerCompleted marker
          )
      where
        key = (sequencedItemDirection item, sequencedItemSequence item)

    disappearanceMarkerCompleted marker =
      case Disappearance.probeWitness (DisappearanceProtocol.disappearancePublicationMarkerProbe marker) (startupDisappearanceState state) of
        Nothing -> False
        Just witness -> case witness.witnessPhase of
          Disappearance.ProbeCollectingView ->
            any
              (\(_, cell) -> cell.markerCellMarker == marker && cell.markerCellCompleted)
              (witness.witnessIncomingPublicationMarkers)
          _ -> True

    markerClaimsMatch item marker =
      alignmentRouteCutoverMarkerSourceHerald marker
        == streamDirectionSource (sequencedItemDirection item)
        && alignmentRouteCutoverMarkerPredecessorHerald marker
          == streamDirectionDestination (sequencedItemDirection item)
        && sequencedItemDigest item
          == peerLogicalPayloadDigest (PeerLogicalRouteCutover marker)

    -- Retirement locally settles an already-admitted stream position whose
    -- source-qualified semantic evidence can no longer arrive. The retained
    -- stream item remains the exact audit witness, but it deliberately has no
    -- corresponding live alignment or disappearance owner.
    retainedItemSourceRetired item =
      PeerStream.peerStreamPeerRetired
        (streamDirectionSource (sequencedItemDirection item))
        peerStreamState

    -- A retained control item may become semantically decidable only after an
    -- unordered prerequisite arrives. Remote protocol failure then terminally
    -- completes that exact stream position and closes its binding without
    -- mutating the semantic owner. Re-evaluate only enough to distinguish that
    -- specified terminal outcome from a marker which is still waiting or one
    -- which would be valid but is missing from its owner.
    routeCutoverMarkerTerminallyRejected item marker =
      case AlignmentCoordinator.retainAlignmentRouteCutoverMarker
        (streamDirectionSource (sequencedItemDirection item))
        (streamDirectionDestination (sequencedItemDirection item))
        marker
        (startupAlignmentState state) of
        Left AlignmentCoordinator.AlignmentRouteCutoverGenerationMissing {} -> False
        Left AlignmentCoordinator.AlignmentRouteCutoverSourceAcceptanceMissing {} -> False
        Left _ -> True
        Right _ -> False

    retainedRouteCutoverMarkers =
      fmap
        snd
        ( AlignmentTransfer.routeCutoverEntries
            ( Alignment.alignmentTransferState
                (startupAlignmentState state)
            )
        )

    markerHasSourceAcceptance marker =
      any
        ( ( ==
              ( alignmentRouteCutoverMarkerGeneration marker,
                alignmentRouteCutoverMarkerSourceHerald marker
              )
          )
            . fst
        )
        (Alignment.alignmentCutAcceptanceEntries (startupAlignmentState state))

    validateAdmittedRetained item peerPublication key validDisposition = do
      identifier <-
        maybe
          (fault StartupStatic PublicationIncomingInvariant)
          Right
          (Map.lookup key expectedAssignmentOwners)
      record <-
        maybe
          (fault StartupStatic PublicationIncomingInvariant)
          Right
          (Map.lookup identifier incomingById)
      let receipt =
            assignmentReceipt
              (sequencedItemDirection item)
              (sequencedItemSequence item)
              (sequencedItemDigest item)
          batch = peerPublicationBatch peerPublication
      require
        (StartupProcess (publicationBatchSourceProcess batch))
        PublicationIncomingInvariant
        ( publicationBatchId batch == identifier
            && peerPublication == Publication.incomingPeerPublication record
            && batch == Publication.incomingPublicationBatch record
            && sequencedItemDigest item == peerPublicationDigest peerPublication
            && sequencedItemDigest item == Publication.incomingPublicationDigest record
            && receipt `Set.member` Publication.incomingPublicationAssignments record
            && validDisposition (Publication.incomingPublicationDisposition record)
        )

    validateRecord (identifier, record) = do
      let peerPublication = Publication.incomingPeerPublication record
          batch = peerPublicationBatch peerPublication
          direction = Publication.incomingPublicationDirection record
          sourceProcess = publicationBatchSourceProcess batch
          subject = StartupProcess sourceProcess
          destinations = publicationBatchDestinations batch
          outcomes = Publication.incomingPublicationDestinationOutcomes record
          dependencies = Publication.incomingPublicationDependencies record
          disposition = Publication.incomingPublicationDisposition record
          protocolRejected = disposition == Publication.ProtocolRejected
          allPending = all (== Publication.DestinationPending) (Map.elems outcomes)
          allIgnored =
            all (== Publication.DestinationTerminallyIgnored) (Map.elems outcomes)
          anyApplied = Publication.DestinationApplied `elem` Map.elems outcomes
          anyPending = Publication.DestinationPending `elem` Map.elems outcomes
          retainedMembership =
            OracleProjection.oracleViewHeraldMembershipById
              (Publication.incomingPublicationMembershipGenerationId record)
              oracleView
          structuralDestinationMatches =
            case peerPublicationStructuralStamp peerPublication of
              Nothing -> True
              Just stamp -> case retainedMembership of
                Nothing -> False
                Just membership ->
                  PeerInput.structuralPublicationDestinationsValidAt
                    ( PeerInput.peerInputContext
                        (startupGenesis state)
                        (startupOracleProjectionState state)
                        (startupDiscoveryState state)
                        (startupPlacementState state)
                    )
                    membership
                    (startupPlacementState state)
                    stamp
                    batch
          sortDependency =
            Publication.EffectiveSortDependency
              (publicationBatchSortId batch)
              (publicationBatchOccurrenceId batch)
          controlDependency =
            Publication.ControlIndexDependency
              (publicationBatchControlPrerequisite batch)
          topologyDependency =
            Publication.SourceTopologyDependency
              (publicationBatchSourceTopologyPrerequisite batch)
          (sortProjectionMatches, expectedSortDependencies) =
            case lookupRetainedRegistryEntryAt
              (publicationBatchSortId batch)
              (publicationBatchOccurrenceId batch)
              (startupSortRegistryState state) of
              Nothing -> (True, Set.singleton sortDependency)
              Just entry ->
                ( SortRegistry.registryEntryOccurrenceId entry
                    == publicationBatchOccurrenceId batch,
                  Set.empty
                )
          expectedControlDependencies =
            if currentControl < publicationBatchControlPrerequisite batch
              then Set.singleton controlDependency
              else Set.empty
          expectedTopologyDependencies =
            if sourceTopologyInstalled (publicationBatchSourceTopologyPrerequisite batch)
              then Set.empty
              else Set.singleton topologyDependency
          expectedGateDependencies =
            Set.fromList
              [ Publication.DisappearanceProbeDependency probe
              | receipt <- Set.toList (Publication.incomingPublicationAssignments record),
                probe <-
                  Set.toList
                    ( DisappearanceGate.peerPublicationGateProbes
                        (streamDirectionSource (assignmentReceiptDirection receipt))
                        (assignmentReceiptSequence receipt)
                        peerPublication
                        (startupDisappearanceState state)
                    )
              ]
          expectedDependencies =
            expectedGateDependencies
              `Set.union` expectedSortDependencies
              `Set.union` expectedControlDependencies
              `Set.union` expectedTopologyDependencies
          sourceIsActive =
            OracleProjection.oracleViewIsActiveHerald
              (publicationSourceHeraldEpoch identifier)
              oracleView
          localIsActive =
            OracleProjection.oracleViewIsActiveHerald localEpoch oracleView
          currentMembership =
            OracleProjection.oracleViewCurrentHeraldMembership oracleView
          currentMembershipId =
            heraldMembershipGenerationId currentMembership
          successorStructuralBaseDependency =
            Publication.SuccessorStructuralBaseDependency currentMembershipId
          successorStructuralBaseRequired =
            incomingPublicationRequiresSuccessorBase peerPublication record
              && isJust
                (heraldMembershipGenerationPredecessor currentMembership)
              && isJust
                (heraldMembershipGenerationRetirementControlIndex currentMembership)
          controlledRelation = incomingControlledRelation state record
          controlledCanApply = case controlledRelation of
            IncomingControlledMismatch -> False
            IncomingControlledSuppressedCurrent {} -> False
            IncomingControlledRetirementSuppressed {} -> False
            _ -> True
          controlledCanFinish = case controlledRelation of
            IncomingControlledMismatch -> regularRetirementSuppressed
            _ -> True
          terminallyIgnoredJustified =
            controlledRelationIsSuppressed controlledRelation
              || regularRetirementSuppressed
          regularRetirementSuppressed =
            incomingRegularRetirementSuppressed state record
          heldDependencyPhaseValid =
            disposition /= Publication.DependencyHeld
              || ( not (Set.null dependencies)
                     && ( all preAuthenticationDependency (Set.toList dependencies)
                            || ( Set.null expectedDependencies
                                   && all postAuthenticationDependency (Set.toList dependencies)
                               )
                        )
                 )
      require
        subject
        PublicationIncomingInvariant
        ( identifier == publicationBatchId batch
            && Publication.incomingPublicationDigest record
              == peerPublicationDigest peerPublication
            && streamDirectionSource direction
              == publicationSourceHeraldEpoch identifier
            && streamDirectionDestination direction == localEpoch
            && isJust retainedMembership
            && not (publicationBatchHasFenceDependency batch)
            && ( protocolRejected
                   || not (Set.null expectedDependencies)
                   || incomingSourceAuthorityMatches direction peerPublication
               )
            && (disposition /= Publication.DependencyHeld || not (Set.null (Publication.incomingPublicationAssignments record)))
            && all
              (assignmentMatches identifier record)
              (Set.toAscList (Publication.incomingPublicationAssignments record))
            && Map.keysSet outcomes == Set.fromList (NonEmpty.toList destinations)
            && structuralDestinationMatches
            && all
              (dependencyMatches peerPublication)
              (Set.toAscList dependencies)
            && heldDependencyPhaseValid
            && (protocolRejected || sortProjectionMatches)
            && ( protocolRejected
                   || if sourceIsActive && localIsActive
                     then
                       if successorStructuralBaseRequired
                         && Set.null expectedDependencies
                         then
                           dependencies
                             == Set.singleton successorStructuralBaseDependency
                         else
                           if Set.null expectedDependencies
                             then all (structuralDependencyPermitted peerPublication) dependencies
                             else dependencies == expectedDependencies
                     else
                       expectedDependencies `Set.isSubsetOf` dependencies
                         && ( dependencies == expectedDependencies
                                || Publication.incomingPublicationMembershipGenerationId record
                                  /= OracleProjection.oracleViewCurrentHeraldMembershipId oracleView
                            )
               )
            && case disposition of
              Publication.DependencyHeld ->
                not (Set.null dependencies) && allPending
              Publication.Applied ->
                Set.null dependencies
                  && not anyPending
                  && anyApplied
                  && publicationBatchControlPrerequisite batch <= currentControl
                  && controlledCanApply
              Publication.TerminallyIgnored ->
                Set.null dependencies
                  && allIgnored
                  && publicationBatchControlPrerequisite batch <= currentControl
                  && controlledCanFinish
                  && terminallyIgnoredJustified
              Publication.ProtocolRejected ->
                Set.null dependencies
                  && allPending
                  && publicationBatchControlPrerequisite batch <= currentControl
        )
      traverse_
        ( validateDestination
            subject
            identifier
            batch
            controlledRelation
            regularRetirementSuppressed
        )
        (Map.toAscList outcomes)

    assignmentMatches identifier record receipt =
      assignmentReceiptDirection receipt
        == Publication.incomingPublicationDirection record
        && assignmentReceiptDigest receipt
          == Publication.incomingPublicationDigest record
        && Map.lookup
          ( assignmentReceiptDirection receipt,
            assignmentReceiptSequence receipt
          )
          expectedAssignmentOwners
          == Just identifier

    preAuthenticationDependency = \case
      Publication.EffectiveSortDependency {} -> True
      Publication.ControlIndexDependency {} -> True
      Publication.DisappearanceProbeDependency {} -> True
      Publication.SourceTopologyDependency {} -> True
      Publication.SuccessorStructuralBaseDependency {} -> False
      Publication.StructuralApplicationDependency {} -> False

    postAuthenticationDependency = \case
      Publication.EffectiveSortDependency {} -> False
      Publication.ControlIndexDependency {} -> False
      Publication.DisappearanceProbeDependency {} -> False
      Publication.SourceTopologyDependency {} -> False
      Publication.SuccessorStructuralBaseDependency {} -> True
      Publication.StructuralApplicationDependency {} -> True

    dependencyMatches peerPublication dependency = case dependency of
      Publication.EffectiveSortDependency sortId occurrence ->
        sortId == publicationBatchSortId batch
          && occurrence == publicationBatchOccurrenceId batch
      Publication.DisappearanceProbeDependency probe ->
        any
          ( \receipt ->
              probe
                `Set.member` DisappearanceGate.peerPublicationGateProbes
                  (streamDirectionSource (assignmentReceiptDirection receipt))
                  (assignmentReceiptSequence receipt)
                  peerPublication
                  (startupDisappearanceState state)
          )
          [ receipt
          | (_, record) <- Publication.incomingPublicationEntries (startupPublicationState state),
            Publication.incomingPeerPublication record == peerPublication,
            receipt <- Set.toList (Publication.incomingPublicationAssignments record)
          ]
      Publication.ControlIndexDependency prerequisite ->
        prerequisite == publicationBatchControlPrerequisite batch
      Publication.SourceTopologyDependency cut ->
        cut == publicationBatchSourceTopologyPrerequisite batch
      Publication.SuccessorStructuralBaseDependency membership ->
        successorStructuralBaseDependencyMatches peerPublication membership
      Publication.StructuralApplicationDependency structural ->
        structuralDependencyMatches peerPublication structural
      where
        batch = peerPublicationBatch peerPublication

    structuralDependencyPermitted peerPublication dependency = case dependency of
      Publication.SuccessorStructuralBaseDependency membership ->
        successorStructuralBaseDependencyMatches peerPublication membership
      Publication.StructuralApplicationDependency structural ->
        structuralDependencyMatches peerPublication structural
      _ -> False

    incomingGenerationRequiresSuccessorBase origin =
      let applied = GraphProgress.structuralProgressMembershipGenerationId structuralProgress
          current = OracleProjection.oracleViewCurrentHeraldMembershipId oracleView
       in applied /= origin
            && isJust (OracleProjection.oracleViewHeraldMembershipLineage applied origin oracleView)
            && isJust (OracleProjection.oracleViewHeraldMembershipLineage origin current oracleView)

    incomingPublicationRequiresSuccessorBase peerPublication record =
      case peerPublicationStructuralStamp peerPublication of
        Nothing -> False
        Just stamp ->
          any
            incomingGenerationRequiresSuccessorBase
            [ Publication.incomingPublicationMembershipGenerationId record,
              structuralVersionVectorMembershipGenerationId
                (structuralOccurrenceStampPredecessor stamp)
            ]

    successorStructuralBaseDependencyMatches peerPublication membership =
      isJust (peerPublicationStructuralStamp peerPublication)
        && membership == OracleProjection.oracleViewCurrentHeraldMembershipId oracleView
        && any
          ( \(_, record) ->
              Publication.incomingPeerPublication record == peerPublication
                && incomingPublicationRequiresSuccessorBase peerPublication record
          )
          (Publication.incomingPublicationEntries (startupPublicationState state))

    structuralDependencyMatches peerPublication dependency =
      case dependency of
        Reconciliation.StructuralPredecessorDependency occurrence ->
          case peerPublicationStructuralStamp peerPublication of
            Nothing -> False
            Just stamp ->
              predecessorCovers occurrence stamp
                && not
                  ( Reconciliation.structuralAppliedHasOccurrence
                      occurrence
                      (GraphProgress.structuralProgressReconciliation structuralProgress)
                  )
        _ -> isJust (peerPublicationStructuralStamp peerPublication)

    predecessorCovers ::
      StructuralOccurrenceId ->
      StructuralOccurrenceStamp ->
      Bool
    predecessorCovers occurrence stamp =
      maybe
        False
        ( maybe
            False
            (>= structuralOccurrenceSourceSequence occurrence)
            . structuralPrefixSequence
        )
        ( structuralVersionVectorComponent
            (structuralOccurrenceSourceHeraldEpoch occurrence)
            (structuralOccurrenceStampPredecessor stamp)
        )

    sourceTopologyInstalled cut =
      cut == GraphProgress.structuralGenesisCutId structuralProgress
        || isJust (GraphProgress.lookupInstalledTopologyCut cut structuralProgress)

    incomingSourceAuthorityMatches direction peerPublication =
      hiddenWriterMatches source process nabla observedAuthority peerPublication
        || resolvedWriterMatches
      where
        batch = peerPublicationBatch peerPublication
        process = publicationBatchSourceProcess batch
        identifier = publicationBatchId batch
        nabla = publicationNabla identifier
        observedAuthority = publicationAuthorityEpoch identifier
        source = streamDirectionSource direction
        resolvedWriterMatches =
          case Authority.resolveNablaAuthorityAt
            nabla
            observedAuthority
            (publicationBatchSourceTopologyPrerequisite batch)
            (publicationBatchControlPrerequisite batch)
            structuralProgress
            (startupOracleProjectionState state) of
            Left _ -> False
            Right resolution ->
              Authority.nablaAuthorityResolutionController resolution == process
                && Authority.nablaAuthorityResolutionResidence resolution == source
                && StructuralDebt.sortOccurrenceSortId
                  (Authority.nablaAuthorityResolutionSort resolution)
                  == publicationBatchSortId batch
                && StructuralDebt.sortOccurrenceDefinition
                  (Authority.nablaAuthorityResolutionSort resolution)
                  == publicationBatchOccurrenceId batch

    hiddenWriterMatches source process nabla observedAuthority peerPublication =
      case peerPublicationStructuralStamp peerPublication of
        Nothing -> False
        Just stamp ->
          case ( lookupSystemBootstrapWriter
                   source
                   (structuralOccurrenceStampCarrierRole stamp)
                   (startupGenesis state),
                 OracleProjection.oracleViewStartedProcess process oracleView,
                 OracleProjection.oracleViewProcessResidence process oracleView
               ) of
            (Just writer, Just started, Just residence) ->
              systemBootstrapWriterNablaId writer == nabla
                && residence == source
                && publicationBatchControlPrerequisite
                  (peerPublicationBatch peerPublication)
                  == OracleProjection.projectedStartedProcessControlIndex started
                && observedAuthority
                  == genesisAuthorityEpoch (systemBootstrapWriterAuthority writer)
            _ -> False

    validateDestination
      subject
      identifier
      batch
      controlledRelation
      regularRetirementSuppressed
      (destination, outcome) =
        case outcome of
          Publication.DestinationPending ->
            require
              subject
              PublicationIncomingInvariant
              (not (slotRetainsPublication identifier destination))
          Publication.DestinationApplied -> do
            slot <-
              maybe
                (fault subject PublicationIncomingInvariant)
                Right
                ( lookupPublicationReceiptStoreSlot
                    (publicationDestinationDelta destination)
                    (publicationDestinationStoreIncarnation destination)
                    storeState
                )
            require
              subject
              PublicationIncomingInvariant
              ( Store.storeSlotIncarnation slot
                  == publicationDestinationStoreIncarnation destination
                  && Store.storeSlotSortId slot == publicationBatchSortId batch
                  && Store.storeSlotOccurrenceId slot == publicationBatchOccurrenceId batch
                  && lookup identifier (Store.storeSlotApplicationReceipts slot)
                    == Just (publicationDestinationStrength destination)
              )
          Publication.DestinationTerminallyIgnored ->
            require
              subject
              PublicationIncomingInvariant
              ( case Store.lookupStoreSlot
                  (publicationDestinationDelta destination)
                  storeState of
                  Nothing -> True
                  Just slot ->
                    lookup identifier (Store.storeSlotApplicationReceipts slot)
                      == Nothing
                      && ( Store.storeSlotIncarnation slot
                             /= publicationDestinationStoreIncarnation destination
                             || ( ( controlledRelationIsSuppressed controlledRelation
                                      || regularRetirementSuppressed
                                  )
                                    && Store.storeSlotSortId slot
                                      == publicationBatchSortId batch
                                    && Store.storeSlotOccurrenceId slot
                                      == publicationBatchOccurrenceId batch
                                )
                         )
              )

    slotRetainsPublication identifier destination =
      case Store.lookupStoreSlot (publicationDestinationDelta destination) storeState of
        Nothing -> False
        Just slot ->
          lookup identifier (Store.storeSlotApplicationReceipts slot) /= Nothing

data IncomingControlledRelation
  = IncomingUncontrolled
  | IncomingControlledMismatch
  | IncomingControlledMatch GlobalObjectId Lifecycle (Maybe StructuralCarrierRole)
  | IncomingControlledSuppressedCurrent GlobalObjectId (Maybe StructuralCarrierRole)
  | IncomingControlledRetirementSuppressed GlobalObjectId (Maybe StructuralCarrierRole)
  | IncomingControlledTerminallyDeleted GlobalObjectId (Maybe StructuralCarrierRole)
  deriving stock (Eq)

controlledRelationIsSuppressed :: IncomingControlledRelation -> Bool
controlledRelationIsSuppressed relation = case relation of
  IncomingControlledSuppressedCurrent {} -> True
  IncomingControlledRetirementSuppressed {} -> True
  IncomingControlledTerminallyDeleted {} -> True
  _ -> False

incomingRegularRetirementSuppressed ::
  HeraldState ->
  Publication.IncomingPublicationRecord ->
  Bool
incomingRegularRetirementSuppressed state incoming =
  case lookupRetainedRegistryEntryAt
    (publicationBatchSortId batch)
    (publicationBatchOccurrenceId batch)
    registry of
    Nothing -> False
    Just entry
      | SortRegistry.registryEntryOccurrenceId entry
          /= publicationBatchOccurrenceId batch ->
          False
      | otherwise ->
          any
            ( \suppression ->
                OwnerEvidence.peerPublicationIsRegularRetirementResidual
                  (Store.regularRetirementSuppressionSubject suppression)
                  (Store.regularRetirementSuppressionResolveIndex suppression)
                  (Publication.incomingPeerPublication incoming)
            )
            (Store.regularRetirementSuppressions store)
  where
    batch = Publication.incomingPublicationBatch incoming
    registry = startupSortRegistryState state
    store = startupStoreState state

-- | Recompute the controlled observation from the effective checked descriptor
-- and publication, then require the exact fact in the Herald-local Controlled
-- owner. Oracle target-object state is deliberately absent.
incomingControlledRelation ::
  HeraldState ->
  Publication.IncomingPublicationRecord ->
  IncomingControlledRelation
incomingControlledRelation state incoming =
  case lookupRetainedRegistryEntryAt
    (publicationBatchSortId batch)
    (publicationBatchOccurrenceId batch)
    registry of
    Nothing -> IncomingControlledMismatch
    Just entry ->
      let descriptor = checkedDescriptorFor entry
       in if descriptorKind descriptor /= ControlledSort
            then IncomingUncontrolled
            else
              if SortRegistry.registryEntryOccurrenceId entry
                /= publicationBatchOccurrenceId batch
                then IncomingControlledMismatch
                else
                  maybe IncomingControlledMismatch controlledRelation (controlledFacts entry)
  where
    batch = Publication.incomingPublicationBatch incoming
    registry = startupSortRegistryState state
    checkedDescriptorFor entry =
      canonicalCheckedDescriptor (SortRegistry.registryEntryDescriptor entry)

    controlledFacts entry = do
      value <-
        either
          (const Nothing)
          Just
          (decodeCanonicalValue (canonicalValueByteString (publicationBatchCanonicalValue batch)))
      publication <-
        either
          (const Nothing)
          Just
          (mkCheckedPublication (SortRegistry.registryEntryDescriptor entry) (publicationBatchId batch) value)
      observation <-
        either
          (const Nothing)
          Just
          ( Controlled.checkControlledObservation
              (SortRegistry.registryEntryDescriptor entry)
              (publicationBatchOccurrenceId batch)
              publication
          )
      if checkedPublicationCanonicalValue publication
        == publicationBatchCanonicalValue batch
        then
          Just (observation, publication)
        else Nothing

    controlledRelation (observation, publication) =
      let object = Controlled.checkedControlledObservationObject observation
          carrier = Controlled.checkedControlledObservationStructuralRole observation
          lifecycle =
            checkedPublicationLifecycle
              (Controlled.checkedControlledObservationPublication observation)
          controlled = startupControlledState state
       in if exactRetirementSuppression publication
            then IncomingControlledRetirementSuppressed object carrier
            else case ( Controlled.controlledLocalRecord object controlled,
                        Controlled.controlledTerminalDeletion object controlled
                      ) of
              (Nothing, Just _) ->
                IncomingControlledTerminallyDeleted object carrier
              (Just record, Nothing)
                | Controlled.controlledRecordSortId record
                    == publicationBatchSortId batch,
                  Controlled.controlledRecordOccurrenceId record
                    == publicationBatchOccurrenceId batch,
                  Controlled.controlledRecordObservationLabelCompatible
                    record
                    observation
                    controlled,
                  Controlled.controlledRecordStructuralRole record == carrier,
                  Controlled.controlledRecordObservation (publicationBatchId batch) record
                    == Just publication ->
                    case (lifecycle, Controlled.controlledRecordLifecycle record) of
                      (Current, Controlled.ControlledObsolete) ->
                        IncomingControlledSuppressedCurrent object carrier
                      (observed, retained)
                        | lifecycleMatches observed retained ->
                            IncomingControlledMatch object observed carrier
                      _ -> IncomingControlledMismatch
              _ -> IncomingControlledMismatch

    exactRetirementSuppression publication =
      case peerPublicationStructuralStamp (Publication.incomingPeerPublication incoming) of
        Nothing -> False
        Just stamp ->
          let occurrence = structuralOccurrenceStampOccurrence stamp
              progress = startupStructuralProgressState state
              reconciliation = GraphProgress.structuralProgressReconciliation progress
           in checkedPublicationId publication == publicationBatchId batch
                && isJust
                  ( Reconciliation.structuralAppliedRetirementSuppressionAt
                      occurrence
                      reconciliation
                  )
                && case GraphProgress.lookupAppliedStructuralOccurrence occurrence progress of
                  Nothing -> False
                  Just applied ->
                    GraphProgress.appliedStructuralOccurrenceStamp applied == stamp

    lifecycleMatches lifecycle retained = case (lifecycle, retained) of
      (Current, Controlled.ControlledCurrent) -> True
      (_, Controlled.ControlledObsolete) -> lifecycle /= Current
      _ -> False

validateStoreSlotReceipts ::
  Map.Map PublicationId CheckedPublication ->
  Store.State ->
  Map.Map (SortId, ObjectKey) StructuralConsequence.StructuralConsequenceCause ->
  Map.Map (SortId, ObjectKey) StructuralConsequence.StructuralConsequenceCause ->
  Set.Set PublicationId ->
  Map.Map (DeltaId, StoreIncarnationId) (Set.Set PublicationId) ->
  Map.Map PublicationId Publication.OutgoingPublicationRecord ->
  Map.Map PublicationId FrozenRoute ->
  Map.Map PublicationId Publication.IncomingPublicationRecord ->
  Map.Map (DeltaId, StoreIncarnationId, PublicationId) [ReplicaStrength] ->
  Store.StoreSlot ->
  Either HeraldInvariantFault ()
validateStoreSlotReceipts
  checkedByPublication
  storeState
  terminalPurgeCauses
  requiredTerminalPurges
  primordialIds
  genesisEnvironmentSeeds
  outgoingById
  stampedStructuralRoutes
  incomingById
  alignmentStrengths
  slot = do
    let retainedObservations = Store.storeSlotApplicationObservations slot
    require
      subject
      StoreContentsInvariant
      ( deltaStoreSort contents == Store.storeSlotSortId slot
          && actualPurges `Map.isSubmapOf` justifiedPurges
          && requiredPurges `Map.isSubmapOf` actualPurges
          && all purgePayloadAbsent (Map.keys actualPurges)
          && all retainedObservationIsAuthenticated retainedObservations
      )
    traverse_ (validateReceipt retainedObservations) receipts
    traverse_ (validateSeed retainedObservations) matchingPrimordial
    traverse_ validateStored (visibleInstances contents)
    traverse_ validateStored (retainedInstances contents)
    where
      subject = StartupDelta (Store.storeSlotDelta slot)
      contents = Store.storeSlotContents slot
      receipts = Store.storeSlotApplicationReceipts slot
      actualPurges = Map.fromList (Store.storeSlotTerminalObjectPurges slot)
      justifiedPurges =
        Map.fromList
          [ (key, cause)
          | ((sortId, key), cause) <- Map.toAscList terminalPurgeCauses,
            sortId == Store.storeSlotSortId slot
          ]
      requiredPurges =
        Map.fromList
          [ (key, cause)
          | ((sortId, key), cause) <- Map.toAscList requiredTerminalPurges,
            sortId == Store.storeSlotSortId slot
          ]
      matchingPrimordial = case Store.storeSlotProvenance slot of
        -- Reconciliation-created resources are born empty at their structural
        -- occurrence. They never replay the index-zero seed set; their exact
        -- resource/provenance relation is validated by
        -- 'validateStructuralOwners', and subsequent data is justified only
        -- by retained application receipts below.
        Store.StructuralApplicationReader {} -> []
        Store.StructuralBaselineReader {} -> []
        Store.StructuralControlReader {} -> []
        Store.HeraldSystemView {} -> matchingBootstrapSeeds primordialIds
        Store.ApplicationReader {} -> matchingBootstrapSeeds (primordialIds `Set.union` ownGenesisSeeds)
      ownGenesisSeeds = Map.findWithDefault Set.empty (Store.storeSlotDelta slot, Store.storeSlotIncarnation slot) genesisEnvironmentSeeds
      matchingBootstrapSeeds seedIds =
        [ publication
        | publication <- Map.elems checkedByPublication,
          checkedPublicationId publication `Set.member` seedIds,
          checkedPublicationSort publication == Store.storeSlotSortId slot
        ]
      receiptMap = Map.fromList receipts

      retainedObservationIsAuthenticated observation =
        let publication = Store.retainedStoreObservationPublication observation
         in Map.lookup (checkedPublicationId publication) checkedByPublication
              == Just publication

      validateReceipt retainedObservations (identifier, strength) = do
        publication <-
          maybe
            (fault subject StoreContentsInvariant)
            Right
            (Map.lookup identifier checkedByPublication)
        require
          subject
          StoreContentsInvariant
          ( checkedPublicationSort publication == Store.storeSlotSortId slot
              && retainedStrengthMatches
                retainedObservations
                publication
                strength
          )
        let provenanceStrengths =
              ( [Normal | identifier `Set.member` primordialIds || identifier `Set.member` ownGenesisSeeds]
                  <> maybe
                    []
                    ( routeReceiptStrengths
                        (Store.storeSlotDelta slot)
                        (Store.storeSlotIncarnation slot)
                        . Publication.outgoingPublicationRoute
                    )
                    (Map.lookup identifier outgoingById)
                  <> maybe
                    []
                    ( routeReceiptStrengths
                        (Store.storeSlotDelta slot)
                        (Store.storeSlotIncarnation slot)
                    )
                    (Map.lookup identifier stampedStructuralRoutes)
                  <> maybe
                    []
                    ( incomingRecordReceiptStrengths
                        (Store.storeSlotDelta slot)
                        (Store.storeSlotIncarnation slot)
                    )
                    (Map.lookup identifier incomingById)
                  <> Map.findWithDefault
                    []
                    ( Store.storeSlotDelta slot,
                      Store.storeSlotIncarnation slot,
                      identifier
                    )
                    alignmentStrengths
              )
        require
          subject
          PublicationRouteReceiptInvariant
          (not (null provenanceStrengths) && maximum provenanceStrengths == strength)

      validateSeed retainedObservations publication =
        require
          subject
          StoreContentsInvariant
          ( Map.lookup (checkedPublicationId publication) receiptMap == Just Normal
              && retainedStrengthMatches retainedObservations publication Normal
          )

      validateStored (key, stored) =
        require
          subject
          StoreContentsInvariant
          ( checkedPublicationId (storedPublication stored) `Map.member` receiptMap
              && key == checkedPublicationKey (storedPublication stored)
          )

      retainedStrengthMatches retainedObservations publication strength =
        case Map.lookup (checkedPublicationKey publication) actualPurges of
          Just _ ->
            logicalStateStrength
              (checkedPublicationLogicalState publication)
              contents
              == Nothing
          Nothing ->
            receiptSuppressedByRegularRetirement
              publication
              retainedObservations
              || maybe
                False
                (>= strength)
                ( logicalStateStrength
                    (checkedPublicationLogicalState publication)
                    contents
                )

      receiptSuppressedByRegularRetirement publication retainedObservations =
        case filter
          ((== publication) . Store.retainedStoreObservationPublication)
          retainedObservations of
          [] -> False
          matching -> all observationIsSuppressed matching
        where
          observationIsSuppressed observation =
            any
              (suppressesObservation observation)
              (Store.regularRetirementSuppressions storeState)

          suppressesObservation observation suppression =
            OwnerEvidence.checkedPublicationIsRegularRetirementResidual
              (Store.retainedStoreObservationSortOccurrence observation)
              (Store.regularRetirementSuppressionSubject suppression)
              (Store.regularRetirementSuppressionResolveIndex suppression)
              (controlPrerequisite observation)
              publication

          controlPrerequisite observation =
            case Store.retainedStoreObservationOrigin observation of
              Store.PrimordialStoreObservation -> Nothing
              Store.RoutedStoreObservation envelope ->
                Just (Store.storeObservationEnvelopeControlPrerequisite envelope)

      purgePayloadAbsent key =
        lookupVisible key contents == Nothing
          && lookupRetained key contents == Nothing

routeReceiptStrengths ::
  DeltaId ->
  StoreIncarnationId ->
  FrozenRoute ->
  [ReplicaStrength]
routeReceiptStrengths delta incarnation =
  fmap destinationStrength
    . filter
      ( \destination ->
          destinationDelta destination == delta
            && destinationStoreIncarnation destination == incarnation
      )
    . routeDestinations

incomingRecordReceiptStrengths ::
  DeltaId ->
  StoreIncarnationId ->
  Publication.IncomingPublicationRecord ->
  [ReplicaStrength]
incomingRecordReceiptStrengths delta incarnation record =
  [ publicationDestinationStrength destination
  | (destination, outcome) <-
      (Map.toAscList (Publication.incomingPublicationDestinationOutcomes record)),
    outcome == Publication.DestinationApplied,
    publicationDestinationDelta destination == delta,
    publicationDestinationStoreIncarnation destination == incarnation
  ]

successfulSortDefinitionWrite ::
  Application.ApplicationRequestWitness ->
  Maybe (PrivateNablaId, SortId)
successfulSortDefinitionWrite request =
  case ( Application.applicationRequestWitnessCall request,
         Application.applicationRequestWitnessStatus request
       ) of
    ( WriteApplication privateNabla (PublishValue (SortDefinitionValue _)),
      ApplicationRequest.ApplicationRequestCompleted
        (WriteCompleted (SortDefinitionWritten sortId))
      ) -> Just (privateNabla, sortId)
    _ -> Nothing

validateHeraldPhase :: HeraldState -> Either HeraldInvariantFault ()
validateHeraldPhase state =
  if Administration.administrationWitnessBinding administration
    /= checkedInitialAdministrationBinding (startupGenesis state)
    then phaseFault
    else case ( startupWitnessPhase witness,
                startupWitnessDrain witness,
                Administration.administrationWitnessShutdown administration
              ) of
      (HeraldServing, Nothing, Nothing)
        | Administration.administrationWitnessNextDrainOrdinal administration == 1 ->
            Right ()
      ( HeraldDraining,
        Just drain,
        Just (_, ownerDrain, Administration.AdministrationShutdownPending)
        ) -> requireMatchingDrain drain ownerDrain
      ( HeraldStopped,
        Just drain,
        Just (_, ownerDrain, Administration.AdministrationShutdownCompleted)
        ) -> requireMatchingDrain drain ownerDrain
      _ -> phaseFault
  where
    witness = startupStateWitness state
    administration = startupWitnessAdministration witness
    requireMatchingDrain drain ownerDrain
      | heraldDrainWitnessId drain == ownerDrain
          && Administration.drainRequestRefDrainId
            (heraldDrainWitnessRequest drain)
            == ownerDrain
          && Administration.administrationWitnessNextDrainOrdinal administration
            == drainIdWord64 ownerDrain + 1 =
          Right ()
      | otherwise = phaseFault
    phaseFault =
      Left (HeraldTransitionInvariant HeraldDrainOwnerContradiction)

validateOracleProjection ::
  HeraldState ->
  Either HeraldInvariantFault [AppliedProcessBootstrap]
validateOracleProjection state = do
  require
    StartupStatic
    OracleProjectionOwnerInvariant
    (case OracleProjection.validateState oracleState of Right _ -> True; Left _ -> False)
  require
    StartupStatic
    OracleControlIndexInvariant
    (OracleProjection.oracleViewControlIndex view >= checkedOracleControlIndex genesis)
  require
    StartupStatic
    OracleSystemIdentityInvariant
    (OracleProjection.oracleViewSystemId view == checkedSystemId genesis)
  require
    StartupStatic
    OracleLocalIdentityInvariant
    (OracleProjection.oracleViewLocalHeraldEpoch view == checkedLocalHeraldEpoch genesis)
  require
    StartupStatic
    OracleMembershipInvariant
    ( initialMembershipMatchesGenesis
        && all currentMemberMatchesGenesis (OracleProjection.oracleViewActiveHeralds view)
    )
  applied <- traverse reconstruct (OracleProjection.projectedBootstraps oracleState)
  traverse_ validateStarted (OracleProjection.projectedStartedProcesses oracleState)
  require
    StartupStatic
    OracleInitialProjectionDigestInvariant
    ( OracleProjection.oracleViewInitialProjectionDigest view
        == deriveInitialProjectionDigest
          applied
          ( checkedInitialTopologyProjection
              (startupInitialBootstraps state)
          )
    )
  pure applied
  where
    genesis = startupGenesis state
    oracleState = startupOracleProjectionState state
    view = OracleProjection.oracleView oracleState
    initialMembershipMatchesGenesis =
      case OracleProjection.oracleViewHeraldMembershipHistory view of
        initialMembership : _ ->
          sort
            ( NonEmpty.toList
                (heraldMembershipGenerationActiveHeraldEpochs initialMembership)
            )
            == sort (checkedActiveHeraldEpochs genesis)
        [] -> False
    currentMemberMatchesGenesis member =
      member `elem` checkedActiveHeralds genesis
        || any
          ( \record ->
              let manifest = Admission.admissionRecordManifest record
               in Admission.admissionManifestHeraldId manifest == heraldMemberId member
                    && Admission.admissionManifestHeraldEpoch manifest == heraldMemberEpoch member
          )
          (OracleProjection.projectedHeraldAdmissions oracleState)
    -- Step 3's complete applied set is the index-zero fixture.  A later
    -- multi-index projection must validate each retained applied record at its
    -- own admission index, rather than rematerialising old records at the
    -- projection's latest control index.
    reconstruct (process, projected) = do
      let manifest = appliedBootstrapManifestId projected
      configured <-
        maybe
          (fault (StartupProcess process) OracleAppliedProcessInvariant)
          Right
          (lookupConfiguredProcessBootstrap manifest genesis)
      applied <-
        either
          (const (fault (StartupProcess process) OracleAppliedProcessInvariant))
          Right
          ( materializeConfiguredProcessBootstrap
              (checkedOracleControlIndex genesis)
              (checkedPredefinedOccurrenceSet genesis)
              configured
          )
      require
        (StartupProcess process)
        OracleAppliedProcessInvariant
        (applied == projected)
      pure applied
    validateStarted (process, started) =
      let start = OracleProjection.projectedStartedProcessBootstrap started
          index = OracleProjection.projectedStartedProcessControlIndex started
       in require
            (StartupProcess process)
            OracleAppliedProcessInvariant
            ( process == processStartProcessEpochId start
                && index > controlIndex 0
                && index <= OracleProjection.oracleViewControlIndex view
                && maybe
                  False
                  (elem (processStartResidence start) . NonEmpty.toList . heraldMembershipGenerationActiveHeraldEpochs)
                  (OracleProjection.oracleViewHeraldMembershipAt index view)
            )

validateOracleClient :: HeraldState -> Either HeraldInvariantFault ()
validateOracleClient state = do
  let controlProjection = startupControlOracleProjectionState state
      controlView = OracleProjection.oracleView controlProjection
      semanticView = OracleProjection.oracleView (startupOracleProjectionState state)
  require
    StartupStatic
    OracleProjectionOwnerInvariant
    (not (startupSemanticControlIsFrozen state) || case OracleProjection.validateState controlProjection of Right _ -> True; Left _ -> False)
  require
    StartupStatic
    OracleControlIndexInvariant
    (OracleProjection.oracleViewControlIndex controlView >= OracleProjection.oracleViewControlIndex semanticView)
  require
    StartupStatic
    OracleSystemIdentityInvariant
    (OracleProjection.oracleViewSystemId controlView == checkedSystemId (startupGenesis state))
  require
    StartupStatic
    OracleLocalIdentityInvariant
    (OracleProjection.oracleViewLocalHeraldEpoch controlView == checkedLocalHeraldEpoch (startupGenesis state))
  require
    StartupStatic
    OracleClientOwnerInvariant
    (case OracleClient.validateState client of Right () -> True; Left _ -> False)
  require
    StartupStatic
    OracleClientOwnerInvariant
    ( localHeraldIsActive
        || ( checkedStartupAdmission genesis /= Nothing
               && GraphProgress.structuralProgressIsJoiningObserver (startupStructuralProgressState state)
               && OracleClient.oracleClientCurrentBinding client == Nothing
           )
        || OracleClient.oracleClientWitnessRetired
          (OracleClient.oracleClientStateWitness client)
    )
  require
    StartupStatic
    OracleClientClaimsInvariant
    (OracleClient.oracleClientHelloClaims client == expectedClaims)
  require
    StartupStatic
    OracleClientCursorInvariant
    ( OracleClient.oracleClientAppliedCursor client
        == OracleProjection.oracleViewControlIndex
          (OracleProjection.oracleView (startupControlOracleProjectionState state))
        && (OracleClient.oracleClientStateWitness client).oracleClientWitnessLastSemanticCursor
          == OracleProjection.projectionLastSemanticControlIndex (startupControlOracleProjectionState state)
    )
  require
    StartupStatic
    OracleClientEvidenceInvariant
    ( clientEvidence == projectionEvidence
        && clientMembershipEvidence == projectionMembershipEvidence
        && OracleClient.oracleClientCanonicalCoveredThrough client
          == OracleProjection.projectionCanonicalCoveredThrough (startupControlOracleProjectionState state)
    )
  where
    genesis = startupGenesis state
    client = startupOracleClientState state
    localHeraldIsActive =
      checkedLocalHeraldEpoch genesis
        `elem` heraldMembershipGenerationActiveHeraldEpochs
          ( OracleProjection.oracleViewCurrentHeraldMembership
              (OracleProjection.oracleView (startupControlOracleProjectionState state))
          )
    expectedClaims =
      oracleHelloClaims
        (checkedSystemId genesis)
        (checkedCatalogueDigest genesis)
        (checkedConfigurationDigest genesis)
        (checkedInitialProjectionDigest (startupInitialBootstraps state))
        (checkedLocalHeraldId genesis)
        (checkedLocalHeraldEpoch genesis)
        ( OracleProjection.oracleViewCurrentHeraldMembershipId
            (OracleProjection.oracleView (startupControlOracleProjectionState state))
        )
    clientEvidence =
      OracleClient.oracleClientWitnessRetainedEntryBytes
        (OracleClient.oracleClientStateWitness client)
    clientMembershipEvidence =
      OracleClient.oracleClientWitnessMembershipAdvances
        (OracleClient.oracleClientStateWitness client)
    projectionEvidence = case OracleProjection.validateState (startupControlOracleProjectionState state) of
      Left _ -> []
      Right witness ->
        [ (index, canonicalAppliedOracleEntryBytes entry)
        | (index, entry) <- OracleProjection.projectionWitnessAppliedEntries witness,
          OracleClient.oracleClientRetainsEntry entry client
        ]
    projectionMembershipEvidence =
      [ (index, heraldMembershipGenerationId membership)
      | membership <-
          OracleProjection.oracleViewHeraldMembershipHistory
            (OracleProjection.oracleView (startupControlOracleProjectionState state)),
        Just index <- [heraldMembershipGenerationChangeControlIndex membership]
      ]

validateSortRegistry :: HeraldState -> Either HeraldInvariantFault ()
validateSortRegistry state = do
  require
    StartupStatic
    PrimordialPublicationInvariant
    ( Store.primordialSeedPublications storeState
        == fmap primordialReplicaPublication replicas
    )
  require
    StartupStatic
    SortRegistrySetInvariant
    ( Map.keysSet replicaByRole == Map.keysSet entryByRole
        && length predefinedEntries == length replicas
        && length retainedPredefinedEntries == length replicas
        && Set.fromList (fmap SortRegistry.registryEntrySortId retainedPredefinedEntries)
          == Set.fromList (fmap primordialReplicaSortId replicas)
        && unique (fmap SortRegistry.registryEntrySortId entries)
    )
  traverse_ validateReplica replicas
  -- Regular retirement is one cross-owner transaction: the registry chain and
  -- Store suppression ledger must describe the same exact occurrences.
  traverse_ validateRetirementChain (Map.toAscList retirementsBySort)
  traverse_ validateSuppression suppressions
  require
    StartupStatic
    DynamicSortRegistryInvariant
    (length retirements == length suppressions)
  traverse_ validateDynamic dynamicEntries
  where
    genesis = startupGenesis state
    replicas = checkedPrimordialReplicas genesis
    storeState = startupStoreState state
    registryState = startupSortRegistryState state
    replicaByRole = Map.fromList [(primordialReplicaRole replica, replica) | replica <- replicas]
    entries = SortRegistry.registryEntries registryState
    predefinedEntries = SortRegistry.predefinedRegistryEntries registryState
    retainedPredefinedEntries =
      [ entry
      | entry <- entries,
        SortRegistry.registryEntryPredefinedRole entry /= Nothing
      ]
    dynamicEntries =
      [ entry
      | entry <- entries,
        SortRegistry.registryEntryPredefinedRole entry == Nothing
      ]
    retirements = SortRegistry.regularSortRetirements registryState
    retirementsBySort =
      Map.fromListWith
        (<>)
        [ ( SortRegistry.regularSortRetirementSortId retirement,
            [retirement]
          )
        | retirement <- retirements
        ]
    suppressions = Store.regularRetirementSuppressions storeState
    entryByRole =
      Map.fromList
        [ (role, entry)
        | entry <- predefinedEntries,
          Just role <- [SortRegistry.registryEntryPredefinedRole entry]
        ]
    validateReplica replica = do
      let role = primordialReplicaRole replica
          subject = StartupSort role
      entry <-
        maybe
          (fault subject SortRegistrySetInvariant)
          Right
          (SortRegistry.lookupPredefinedRole role registryState)
      require
        subject
        SortRegistryDescriptorInvariant
        ( SortRegistry.registryEntryDescriptor entry
            == primordialReplicaDescriptor replica
        )
      require
        subject
        SortRegistryIdentityInvariant
        ( SortRegistry.registryEntrySortId entry == primordialReplicaSortId replica
            && SortRegistry.registryEntryOccurrenceId entry
              == primordialReplicaOccurrenceId replica
            && SortRegistry.registryEntryPublicationId entry
              == primordialReplicaPublicationId replica
        )
    validateDynamic entry = do
      let subject = StartupStatic
          sortId = SortRegistry.registryEntrySortId entry
          system = checkedSystemId genesis
      (descriptor, controlPrerequisite) <- definitionEvidence entry
      plan <-
        either
          (const (fault subject DynamicSortRegistryInvariant))
          Right
          (SortRegistry.planSortInduction system descriptor registryState)
      require
        subject
        DynamicSortRegistryInvariant
        ( descriptor == SortRegistry.registryEntryDescriptor entry
            && descriptorSortId descriptor == sortId
            && SortRegistry.registryEntryOccurrenceId entry
              == SortRegistry.sortInductionPlanOccurrenceId plan
            && SortRegistry.sortInductionPlanSortId plan == sortId
            && controlPrerequisite
              >= SortRegistry.sortInductionPlanControlPrerequisite plan
        )

    -- Sorting by Resolve index exposes the only legal chain: Genesis is
    -- retired first and each later retired occurrence is the predecessor
    -- derived by the preceding Resolve.
    validateRetirementChain (sortId, unorderedRetirements) = do
      (retiredDigest, latestIndex, latestSuccessor) <-
        validateChain
          Nothing
          Genesis
          Nothing
          Set.empty
          (sortOn SortRegistry.regularSortRetirementResolveIndex unorderedRetirements)
      -- A Herald which never learned this descriptor retains its checked
      -- Oracle retirement floor without fabricating a Registry publication.
      case SortRegistry.lookupEffectiveSort sortId registryState of
        Nothing -> Right ()
        Just effective -> do
          plan <-
            either
              (const (fault StartupStatic DynamicSortRegistryInvariant))
              Right
              (SortRegistry.planSortInduction (checkedSystemId genesis) (SortRegistry.registryEntryDescriptor effective) registryState)
          require
            StartupStatic
            DynamicSortRegistryInvariant
            ( SortRegistry.registryEntryPredefinedRole effective == Nothing
                && deriveCanonicalDescriptorDigest (SortRegistry.registryEntryDescriptor effective) == retiredDigest
                && SortRegistry.registryEntryOccurrenceId effective == latestSuccessor
                && SortRegistry.sortInductionPlanOccurrenceId plan == latestSuccessor
                && SortRegistry.sortInductionPlanControlPrerequisite plan == latestIndex
            )
      where
        validateChain _ _ _ _ [] = fault StartupStatic DynamicSortRegistryInvariant
        validateChain expectedDigest expectedBase previousIndex seenOccurrences (retirement : remaining) = do
          let digest = SortRegistry.regularSortRetirementDescriptorDigest retirement
              occurrence = SortRegistry.regularSortRetirementOccurrenceId retirement
              resolveIndex = SortRegistry.regularSortRetirementResolveIndex retirement
              expectedOccurrence = deriveSortDefinitionOccurrenceId (checkedSystemId genesis) sortId expectedBase
              retainedSuccessor = SortRegistry.regularSortRetirementSuccessorOccurrenceId retirement
          successorBase <-
            either
              (const (fault StartupStatic DynamicSortRegistryInvariant))
              Right
              (resolvedRetirementOccurrenceBase resolveIndex)
          let expectedSuccessor = deriveSortDefinitionOccurrenceId (checkedSystemId genesis) sortId successorBase
              matchingSuppressions =
                [ suppression
                | suppression <- suppressions,
                  RegularSortDefinitionSubjectView retainedSort retainedDigest retainedOccurrence <-
                    [disappearanceSubjectView (Store.regularRetirementSuppressionSubject suppression)],
                  (retainedSort, retainedDigest, retainedOccurrence) == (sortId, digest, occurrence)
                ]
          suppression <- case matchingSuppressions of
            [retained] -> Right retained
            _ -> fault StartupStatic DynamicSortRegistryInvariant
          let retirementSubject = Store.regularRetirementSuppressionSubject suppression
          case SortRegistry.regularSortRetirementEntry retirement of
            Just retired -> do
              (publicationDescriptor, publicationControlPrerequisite) <- definitionEvidence retired
              require
                StartupStatic
                DynamicSortRegistryInvariant
                ( SortRegistry.registryEntryPredefinedRole retired == Nothing
                    && SortRegistry.registryEntrySortId retired == sortId
                    && SortRegistry.registryEntryOccurrenceId retired == occurrence
                    && SortRegistry.registryEntryDescriptor retired == publicationDescriptor
                    && deriveCanonicalDescriptorDigest publicationDescriptor == digest
                    && maybe True (<= publicationControlPrerequisite) previousIndex
                )
            Nothing ->
              require
                StartupStatic
                DynamicSortRegistryInvariant
                ( any
                    ( \(_, projected) -> case OracleProjection.projectedDisappearanceTerminal projected of
                        Just (OracleProjection.ProjectedDisappearanceResolved outcome index) ->
                          index == resolveIndex
                            && disappearanceResolutionSubject outcome == retirementSubject
                            && disappearanceResolutionControlIndex outcome == resolveIndex
                        _ -> False
                    )
                    (OracleProjection.projectedDisappearanceProbes (startupOracleProjectionState state))
                )
          currentBlockers <-
            either
              (const (fault StartupStatic DynamicSortRegistryInvariant))
              Right
              (DisappearanceEvidence.regularRetirementResidualBlockersForHerald retirementSubject resolveIndex state)
          let publicationView = PublicationDisappearance.publicationRegularRetirementDisappearanceView retirementSubject resolveIndex (startupPublicationState state)
              suppressionCut = Store.regularRetirementSuppressionLocalPublicationCut suppression
              matchingPositionsCovered =
                all
                  (publicationPositionCoveredBy suppressionCut . matchingPublicationPosition)
                  (PublicationDisappearance.publicationDisappearanceMatchingObservations publicationView)
          require
            StartupStatic
            DynamicSortRegistryInvariant
            ( SortRegistry.regularSortRetirementSortId retirement == sortId
                && maybe True (== digest) expectedDigest
                && occurrence == expectedOccurrence
                && Set.notMember occurrence seenOccurrences
                && retainedSuccessor == expectedSuccessor
                && retainedSuccessor /= occurrence
                && Set.notMember retainedSuccessor seenOccurrences
                && maybe True (< resolveIndex) previousIndex
                && Store.regularRetirementSuppressionResolveIndex suppression == resolveIndex
                && GraphProgress.structuralAppliedControlPrefix (startupStructuralProgressState state) >= resolveIndex
                && null currentBlockers
                && matchingPositionsCovered
                && suppressionCut <= Publication.publicationCurrentHeraldPrefix (startupPublicationState state)
            )
          case remaining of
            [] -> Right (digest, resolveIndex, retainedSuccessor)
            _ -> validateChain (Just digest) successorBase (Just resolveIndex) (Set.insert occurrence seenOccurrences) remaining

        publicationPositionCoveredBy
          DomainAlignment.EmptyHeraldPublicationPrefix
          _ = False
        publicationPositionCoveredBy
          (DomainAlignment.HeraldPublicationPrefixThrough through)
          position = position <= through

    validateSuppression suppression =
      case disappearanceSubjectView
        (Store.regularRetirementSuppressionSubject suppression) of
        ControlledPredefinedSubjectView {} ->
          fault StartupStatic DynamicSortRegistryInvariant
        RegularSortDefinitionSubjectView sortId descriptorDigest occurrence -> do
          retirement <-
            maybe
              (fault StartupStatic DynamicSortRegistryInvariant)
              Right
              ( SortRegistry.lookupRegularSortRetirement
                  sortId
                  occurrence
                  registryState
              )
          require
            StartupStatic
            DynamicSortRegistryInvariant
            ( descriptorDigest
                == SortRegistry.regularSortRetirementDescriptorDigest retirement
                && Store.regularRetirementSuppressionResolveIndex suppression
                  == SortRegistry.regularSortRetirementResolveIndex retirement
                && Store.regularRetirementSuppressionLocalPublicationCut suppression
                  <= Publication.publicationCurrentHeraldPrefix
                    (startupPublicationState state)
            )

    definitionEvidence entry = do
      let subject = StartupStatic
          identifier = SortRegistry.registryEntryPublicationId entry
          herald = checkedLocalHeraldEpoch genesis
          system = checkedSystemId genesis
          systemViewDelta = deriveSystemViewDeltaId system herald SortDefinitionRole
      carrier <-
        maybe
          (fault subject DynamicSortRegistryInvariant)
          Right
          (SortRegistry.lookupPredefinedRole SortDefinitionRole registryState)
      slot <-
        maybe
          (fault subject DynamicSortRegistryInvariant)
          Right
          (Store.lookupStoreSlot systemViewDelta storeState)
      (descriptor, carrierSort, strength, controlPrerequisite) <-
        case ( Publication.lookupOutgoingPublication
                 identifier
                 (startupPublicationState state),
               Publication.lookupIncomingPublication
                 identifier
                 (startupPublicationState state)
             ) of
          (Just outgoing, Nothing) -> outgoingEvidence slot outgoing
          (Nothing, Just incoming) -> incomingEvidence entry slot incoming
          (Nothing, Nothing) -> importedDefinitionEvidence identifier
          _ -> fault subject DynamicSortRegistryInvariant
      require
        subject
        DynamicSortRegistryInvariant
        ( carrierSort == SortRegistry.registryEntrySortId carrier
            && Store.storeSlotProvenance slot
              == Store.HeraldSystemView herald SortDefinitionRole
            && lookup identifier (Store.storeSlotApplicationReceipts slot)
              == Just strength
        )
      Right (descriptor, controlPrerequisite)

    importedDefinitionEvidence identifier = do
      let observations =
            [ evidence
            | (SortDefinitionRole, evidence) <- Join.importedSystemViewObservations (startupJoinState state),
              AlignmentProtocol.retainedPublicationEvidencePublicationId evidence == identifier
            ]
      retained <- case observations of
        first : _ -> Right first
        [] -> fault StartupStatic DynamicSortRegistryInvariant
      value <-
        either
          (const (fault StartupStatic DynamicSortRegistryInvariant))
          Right
          (decodeCanonicalValue (canonicalValueByteString (AlignmentProtocol.retainedPublicationEvidenceCanonicalValue retained)))
      descriptor <- either (const (fault StartupStatic DynamicSortRegistryInvariant)) Right (decodeSortDefinitionValue value)
      let control = maybe (controlIndex 0) id (AlignmentProtocol.retainedPublicationEvidenceControlPrerequisite retained)
      require
        StartupStatic
        DynamicSortRegistryInvariant
        ( all
            ( \evidence ->
                AlignmentProtocol.retainedPublicationEvidenceCanonicalValue evidence == AlignmentProtocol.retainedPublicationEvidenceCanonicalValue retained
                  && maybe (controlIndex 0) id (AlignmentProtocol.retainedPublicationEvidenceControlPrerequisite evidence) == control
            )
            observations
        )
      Right
        ( descriptor,
          AlignmentProtocol.retainedPublicationEvidenceSortId retained,
          maximum (map AlignmentProtocol.retainedPublicationEvidenceSourceStrength observations),
          control
        )

    outgoingEvidence slot record = do
      descriptor <-
        either
          (const (fault StartupStatic DynamicSortRegistryInvariant))
          Right
          ( decodeSortDefinitionValue
              (checkedPublicationValue (Publication.outgoingPublicationChecked record))
          )
      let matchingDestinations =
            [ destinationStrength destination
            | destination <- routeDestinations (Publication.outgoingPublicationRoute record),
              destinationDelta destination == Store.storeSlotDelta slot,
              destinationStoreIncarnation destination == Store.storeSlotIncarnation slot,
              destinationHerald destination == checkedLocalHeraldEpoch genesis
            ]
      strength <- case matchingDestinations of
        [one] -> Right one
        _ -> fault StartupStatic DynamicSortRegistryInvariant
      Right
        ( descriptor,
          checkedPublicationSort (Publication.outgoingPublicationChecked record),
          strength,
          Publication.outgoingPublicationControlPrerequisite record
        )

    incomingEvidence entry slot record = do
      let batch = Publication.incomingPublicationBatch record
          matchingDestinations =
            [ publicationDestinationStrength destination
            | (destination, outcome) <-
                Map.toAscList
                  (Publication.incomingPublicationDestinationOutcomes record),
              outcome == Publication.DestinationApplied,
              publicationDestinationDelta destination == Store.storeSlotDelta slot,
              publicationDestinationStoreIncarnation destination
                == Store.storeSlotIncarnation slot
            ]
      value <-
        either
          (const (fault StartupStatic DynamicSortRegistryInvariant))
          Right
          ( decodeCanonicalValue
              (canonicalValueByteString (publicationBatchCanonicalValue batch))
          )
      descriptor <-
        either
          (const (fault StartupStatic DynamicSortRegistryInvariant))
          Right
          (decodeSortDefinitionValue value)
      strength <- case matchingDestinations of
        [one] -> Right one
        _ -> fault StartupStatic DynamicSortRegistryInvariant
      require
        StartupStatic
        DynamicSortRegistryInvariant
        ( publicationBatchId batch
            == SortRegistry.registryEntryPublicationId entry
            && Publication.incomingPublicationDisposition record
              == Publication.Applied
        )
      Right
        ( descriptor,
          publicationBatchSortId batch,
          strength,
          publicationBatchControlPrerequisite batch
        )

validateSystemViewStores :: HeraldState -> Either HeraldInvariantFault ()
validateSystemViewStores state = do
  require
    StartupStatic
    SystemViewStoreSetInvariant
    (actualSystemViewDeltas == expectedSystemViewDeltas)
  require
    StartupStatic
    SystemViewPlacementInvariant
    (actualSystemViewPlacementDeltas == expectedSystemViewDeltas)
  traverse_ validateReplica replicas
  where
    genesis = startupGenesis state
    system = checkedSystemId genesis
    herald = checkedLocalHeraldEpoch genesis
    replicas = checkedPrimordialReplicas genesis
    storeState = startupStoreState state
    placementState = startupPlacementState state
    systemViewSlots =
      [ slot
      | slot <- Store.storeSlots storeState,
        Store.HeraldSystemView {} <- [Store.storeSlotProvenance slot]
      ]
    actualSystemViewDeltas = Set.fromList (fmap Store.storeSlotDelta systemViewSlots)
    actualSystemViewPlacementDeltas =
      Set.fromList
        ( fmap
            Placement.systemViewPlacementDelta
            (Placement.systemViewPlacements placementState)
        )
    expectedSystemViewDeltas =
      Set.fromList
        [ deriveSystemViewDeltaId system herald (primordialReplicaRole replica)
        | replica <- replicas
        ]
    validateReplica replica = do
      let role = primordialReplicaRole replica
          subject = StartupSort role
          delta = deriveSystemViewDeltaId system herald role
          incarnation = deriveSystemViewStoreIncarnationId system herald role
      slot <-
        maybe
          (fault subject SystemViewStoreFactInvariant)
          Right
          (Store.lookupStoreSlot delta storeState)
      require
        subject
        SystemViewStoreFactInvariant
        ( Store.storeSlotProvenance slot == Store.HeraldSystemView herald role
            && Store.storeSlotDelta slot == delta
            && Store.storeSlotSortId slot == primordialReplicaSortId replica
            && Store.storeSlotOccurrenceId slot
              == primordialReplicaOccurrenceId replica
            && Store.storeSlotIncarnation slot == incarnation
        )
      placement <-
        maybe
          (fault subject SystemViewPlacementInvariant)
          Right
          (Placement.lookupSystemViewPlacement delta placementState)
      require
        subject
        SystemViewPlacementInvariant
        ( Placement.systemViewPlacementRole placement == role
            && Placement.systemViewPlacementDelta placement == delta
            && Placement.systemViewPlacementSortId placement
              == primordialReplicaSortId replica
            && Placement.systemViewPlacementOccurrenceId placement
              == primordialReplicaOccurrenceId replica
            && Placement.systemViewPlacementHeraldEpoch placement == herald
            && Placement.systemViewPlacementStoreIncarnation placement == incarnation
        )
      require
        subject
        SystemViewStoreContentsInvariant
        ( deltaStoreSort (Store.storeSlotContents slot)
            == primordialReplicaSortId replica
            && all
              (\publication -> lookup (checkedPublicationId publication) (Store.storeSlotApplicationReceipts slot) == Just Normal)
              (matchingSeedPublications replica)
        )
    matchingSeedPublications replica =
      [ publication
      | publication <- Store.primordialSeedPublications storeState,
        checkedPublicationSort publication == primordialReplicaSortId replica
      ]

-- Metadata for a selected immutable genesis object is distinct from the local
-- bootstrap capability. Remote observations never install the full root set or
-- a parent namespace; the retained checked grant identifies the exact subset.
observedGenesisSelections :: HeraldState -> [(AppliedProcessBootstrap, [AppliedRoot])]
observedGenesisSelections state =
  [ (bootstrap, roots)
  | (_, bootstrap) <- OracleProjection.projectedBootstraps (startupOracleProjectionState state),
    let roots = filter ((`Set.member` selected) . appliedRootObjectId) (appliedProcessRoots bootstrap),
    not (null roots) || globalObjectIdFromProcessEpochId (appliedProcessEpochId bootstrap) `Set.member` selected
  ]
  where
    selected =
      Set.fromList [LabelPatch.retainedPatchViewObject patch | patch <- Map.elems (LabelPatch.retainedLabelPatches (startupLabelPatchState state))]
        `Set.union` Set.fromList [LabelPatch.importedLabelPatchObject proof | proof <- Map.elems (LabelPatch.importedLabelPatches (startupLabelPatchState state))]
        `Set.union` Set.fromList
          [ globalObjectIdFromGlobalUniqueId (Primordial.globalPrimordialIdentity entry)
          | (_, preparation) <- ProcessPreparation.preparationEntries (startupProcessPreparationState state),
            isJust (ProcessPreparation.preparationSource preparation),
            entry <- Map.elems (Primordial.primordialGrantEntries (ProcessPreparation.preparationGrant preparation)),
            case entry of Primordial.GlobalIdentity _ -> False; _ -> True
          ]

validateResidentSets ::
  HeraldState ->
  [AppliedProcessBootstrap] ->
  [ResidentApplicationAlias] ->
  Either HeraldInvariantFault ()
validateResidentSets state resident applicationResidents = do
  require
    StartupStatic
    ResidentApplicationSetInvariant
    (expectedApplicationProcesses == applicationProcesses)
  require
    StartupStatic
    ResidentControlledSetInvariant
    (expectedProcesses == controlledProcesses)
  require
    StartupStatic
    ResidentRootSetInvariant
    (expectedRoots == controlledRoots)
  require
    StartupStatic
    PlacementSetInvariant
    ( expectedStructuralDeltas == applicationPlacementDeltas
        && expectedSystemViewDeltas == systemViewPlacementDeltas
    )
  require
    StartupStatic
    StoreSetInvariant
    ( expectedStructuralDeltas
        `Set.union` expectedSystemViewDeltas
        == storeDeltas
    )
  where
    genesis = startupGenesis state
    expectedProcesses =
      Set.fromList (fmap appliedProcessEpochId resident)
        `Set.union` Set.fromList
          [ appliedProcessEpochId bootstrap
          | (bootstrap, _) <- observedGenesisSelections state,
            not (Controlled.controlledProcessEnded (appliedProcessEpochId bootstrap) controlled)
          ]
        `Set.union` Set.fromList
          [ process
          | (process, _) <- OracleProjection.projectedStartedProcesses (startupOracleProjectionState state),
            not (Controlled.controlledProcessEnded process controlled)
          ]
    expectedApplicationProcesses = Set.fromList (fmap snd applicationResidents)
    expectedRoots =
      Set.fromList (concatMap bootstrapRootObjects resident)
        `Set.union` Set.fromList
          [ appliedRootObjectId root
          | (bootstrap, roots) <- observedGenesisSelections state,
            not (Controlled.controlledProcessEnded (appliedProcessEpochId bootstrap) controlled),
            root <- roots
          ]
    expectedSystemViewDeltas =
      Set.fromList
        [ deriveSystemViewDeltaId
            (checkedSystemId genesis)
            (checkedLocalHeraldEpoch genesis)
            (primordialReplicaRole replica)
        | replica <- checkedPrimordialReplicas genesis
        ]
    expectedStructuralDeltas =
      Set.fromList
        ( fmap
            Reconciliation.dynamicStoreDelta
            ( Reconciliation.structuralAppliedLocalStores
                ( GraphProgress.structuralProgressReconciliation
                    (startupStructuralProgressState state)
                )
            )
        )
    applicationProcesses =
      Set.fromList
        ( fmap
            witnessedProcessEpoch
            (Application.applicationProcessWitnesses (startupApplicationState state))
        )
    -- Controlled retains immutable identity facts after End; only facts whose
    -- owner epoch is still effective participate in the live resident sets.
    controlledProcesses =
      Set.fromList
        [ process
        | fact <- Controlled.controlledProcessFacts controlled,
          let process = Controlled.processFactProcessEpoch fact,
          not (Controlled.controlledProcessEnded process controlled)
        ]
    controlledRoots =
      Set.fromList
        [ controlledRootObject fact
        | fact <- Controlled.controlledRootFacts controlled,
          not
            ( Controlled.controlledProcessEnded
                (Controlled.rootFactProcessEpoch fact)
                controlled
            )
        ]
    applicationPlacementDeltas =
      Set.fromList
        (fmap Placement.localPlacementDelta (Placement.localPlacements (startupPlacementState state)))
    systemViewPlacementDeltas =
      Set.fromList
        ( fmap
            Placement.systemViewPlacementDelta
            (Placement.systemViewPlacements (startupPlacementState state))
        )
    storeDeltas =
      Set.fromList
        (fmap Store.storeSlotDelta (Store.storeSlots (startupStoreState state)))
    controlled = startupControlledState state

-- | Every Store-derived possession must still name its exact owned delta and a
-- visible checked controlled object at at least the strength observed by the
-- application.  Retained bootstrap/publisher evidence is deliberately outside
-- this relation.
validateControlledStorePossessions :: HeraldState -> Either HeraldInvariantFault ()
validateControlledStorePossessions state =
  traverse_ validateSource (Controlled.controlledStorePossessionWitnesses controlled)
  where
    controlled = startupControlledState state
    store = startupStoreState state
    registry = startupSortRegistryState state

    validateSource source = do
      let process = Controlled.controlledStorePossessionWitnessProcess source
          delta = Controlled.controlledStorePossessionWitnessDelta source
          object = Controlled.controlledStorePossessionWitnessObject source
          subject = StartupDelta delta
      reader <-
        maybe
          (fault subject ControlledStorePossessionInvariant)
          Right
          (Controlled.controlledReaderFact delta controlled)
      slot <-
        maybe
          (fault subject ControlledStorePossessionInvariant)
          Right
          (Store.lookupStoreSlot delta store)
      entry <-
        maybe
          (fault subject ControlledStorePossessionInvariant)
          Right
          (SortRegistry.lookupEffectiveSort (Controlled.rootFactSortId reader) registry)
      record <-
        maybe
          (fault subject ControlledStorePossessionInvariant)
          Right
          (Controlled.controlledLocalRecord object controlled)
      require
        subject
        ControlledStorePossessionInvariant
        ( Controlled.rootFactProcessEpoch reader == process
            && Store.storeSlotDelta slot == delta
            && Store.storeSlotSortId slot == Controlled.rootFactSortId reader
            && Store.storeSlotOccurrenceId slot == Controlled.rootFactOccurrenceId reader
            && SortRegistry.registryEntrySortId entry == Controlled.rootFactSortId reader
            && SortRegistry.registryEntryOccurrenceId entry == Controlled.rootFactOccurrenceId reader
            && Controlled.controlledRecordSortId record == Controlled.rootFactSortId reader
            && Controlled.controlledRecordOccurrenceId record == Controlled.rootFactOccurrenceId reader
            && any (visibleSourceMatches source entry record) (visibleInstances (Store.storeSlotContents slot))
        )

    visibleSourceMatches source entry record (_, stored) =
      let publication = storedPublication stored
       in Controlled.controlledStorePossessionWitnessStrength source <= storedStrength stored
            && case Controlled.checkControlledObservation
              (SortRegistry.registryEntryDescriptor entry)
              (SortRegistry.registryEntryOccurrenceId entry)
              publication of
              Left _ -> False
              Right observation ->
                Controlled.checkedControlledObservationObject observation
                  == Controlled.controlledStorePossessionWitnessObject source
                  && Controlled.controlledRecordObservationLabelCompatible
                    record
                    observation
                    controlled
                  && Controlled.controlledRecordStructuralRole record
                    == Controlled.checkedControlledObservationStructuralRole observation
                  && Controlled.controlledRecordObservation
                    (checkedPublicationId publication)
                    record
                    == Just publication

-- | Terminal controlled deletion is a payload-free authority fact.  The
-- immutable publication and structural histories remain with their owners,
-- while the live Controlled projection must retain neither an observation nor
-- any direct/Store-derived possession.  A structural target additionally has
-- the exact deleted graph overlay installed by the same ordered cause.  The
-- tombstone also dominates every live structural owner: neither topology nor
-- a controller-qualified local resource, placement, or active Store may
-- survive even when the deletion was originally classified as ordinary.
validateControlledTerminalDeletions ::
  HeraldState -> Either HeraldInvariantFault ()
validateControlledTerminalDeletions state =
  traverse_
    validateDeletion
    (Controlled.controlledTerminalDeletionEntries controlled)
  where
    controlled = startupControlledState state
    progress = startupStructuralProgressState state
    reconciliation = GraphProgress.structuralProgressReconciliation progress
    overlays =
      Map.fromList (Reconciliation.structuralAppliedControlOverlays reconciliation)
    graph = startupGraphState state
    placement = startupPlacementState state
    store = startupStoreState state
    liveResources = Reconciliation.structuralAppliedLocalStores reconciliation
    retainedResources =
      Map.elems
        (Reconciliation.structuralAppliedRetainedStoreResources reconciliation)
    retainedDeltaObjects =
      Set.fromList
        [ object
        | (object, DeltaCarrier) <-
            Reconciliation.structuralAppliedRetainedObjectRoles reconciliation
        ]
    storePossessions = Controlled.controlledStorePossessionWitnesses controlled
    processFacts = Controlled.controlledProcessFacts controlled

    validateDeletion (object, deletion) =
      require
        StartupStatic
        ControlledTerminalDeletionInvariant
        ( Controlled.controlledTerminalDeletionObject deletion == object
            && Controlled.controlledLocalRecord object controlled == Nothing
            && all
              ( \fact ->
                  not
                    ( Controlled.controlledHasDirectPossession
                        (Controlled.processFactProcessEpoch fact)
                        object
                        controlled
                    )
              )
              processFacts
            && all
              ( (/= object)
                  . Controlled.controlledStorePossessionWitnessObject
              )
              storePossessions
            && Map.notMember
              object
              (Graph.graphStructuralVertexProjections graph)
            && Map.notMember
              object
              (Graph.graphStructuralEdgeProjections graph)
            && all
              ((/= object) . Reconciliation.dynamicStoreController)
              liveResources
            && all
              ((/= object) . Placement.localPlacementControllerObject)
              (Placement.localPlacements placement)
            && ( Set.notMember object retainedDeltaObjects
                   || isNothing
                     ( Store.lookupStoreSlot
                         (deltaIdFromGlobalObjectId object)
                         store
                     )
               )
            && all
              ( \resource ->
                  Reconciliation.dynamicStoreController resource /= object
                    || isNothing
                      ( Store.lookupStoreSlot
                          (Reconciliation.dynamicStoreDelta resource)
                          store
                      )
              )
              retainedResources
            && maybe
              False
              releasedLabelStateIsDeleted
              (Controlled.controlledEffectiveLabelState object controlled)
            && terminalCauseRetained
              object
              (Controlled.controlledTerminalDeletionCause deletion)
        )

    terminalCauseRetained object cause =
      case StructuralConsequence.structuralConsequenceCauseView cause of
        StructuralConsequence.LabelReleaseCauseView decision index ->
          GraphProgress.structuralAppliedControlPrefix progress >= index
            && releasedRecordMatches object index
            && case LabelPatch.lookupRetainedLabelPatch
              decision
              (startupLabelPatchState state) of
              Just retained ->
                case LabelPatch.retainedPatchViewInstallation retained of
                  Just installed ->
                    LabelPatch.installedLabelObject installed == object
                      && LabelPatch.installedLabelReleaseIndex installed == index
                      && targetRoleDeletionMatches
                        object
                        cause
                        (LabelPatch.targetRoleView (LabelPatch.installedLabelRole installed))
                  Nothing -> False
              Nothing ->
                importedHistoricalLabelReleaseMatches state object decision index
                  && if object `elem` map fst (Reconciliation.structuralAppliedRetainedObjectRoles reconciliation)
                    then deletedOverlayMatches object cause
                    else absentOrDeletedOverlayMatches object cause
        StructuralConsequence.PredefinedDisappearanceCauseView _ index ->
          GraphProgress.structuralAppliedControlPrefix progress >= index
            && disappearanceLabelCompatible object
            && deletedOverlayMatches object cause
        StructuralConsequence.StructuralOccurrenceCauseView {} -> False
        StructuralConsequence.ProcessEndCauseView {} -> False

    releasedRecordMatches object index =
      case Controlled.controlledReleasedLabelRecord object controlled of
        Just record ->
          releasedLabelStateIsDeleted (labelRecordReleasedState record)
            && Controlled.controlledEffectiveLabelState object controlled == Just (labelRecordReleasedState record)
            && labelRevisionControlIndex (labelRecordRevision record) == index
        Nothing -> False

    disappearanceLabelCompatible object =
      case Controlled.controlledReleasedLabelRecord object controlled of
        Nothing -> True
        Just record ->
          not (releasedLabelStateIsDeleted (labelRecordReleasedState record))

    targetRoleDeletionMatches object cause role = case role of
      -- An ordinary/absent installation has no structural target to reconcile at
      -- release time.  A delayed structural carrier for that same globally
      -- unique identity may subsequently retain an authority-only deleted
      -- overlay so its occurrence/vector history can advance without reviving
      -- topology.
      LabelPatch.AbsentOrdinaryTargetView -> absentOrDeletedOverlayMatches object cause
      LabelPatch.OrdinaryControlledTargetView -> absentOrDeletedOverlayMatches object cause
      LabelPatch.NeutralTargetView {} -> deletedOverlayMatches object cause
      LabelPatch.EdgeTargetView {} -> deletedOverlayMatches object cause
      LabelPatch.NablaTargetView {} -> deletedOverlayMatches object cause
      LabelPatch.DeltaTargetView {} -> deletedOverlayMatches object cause

    deletedOverlayMatches object cause =
      case Map.lookup object overlays of
        Just (Reconciliation.StructuralDeletedOverlayView retained) ->
          retained == cause
        _ -> False

    absentOrDeletedOverlayMatches object cause =
      case Map.lookup object overlays of
        Nothing -> True
        Just (Reconciliation.StructuralDeletedOverlayView retained) ->
          retained == cause
        Just (Reconciliation.StructuralControllerOverlayView _ _) -> False

validateResidentProcess ::
  HeraldState ->
  AppliedProcessBootstrap ->
  Either HeraldInvariantFault ()
validateResidentProcess state bootstrap = do
  witness <-
    maybe
      (fault subject ResidentApplicationSetInvariant)
      Right
      (Map.lookup process identityByProcess)
  access <-
    maybe
      (fault subject ApplicationAccessInvariant)
      Right
      (Application.applicationBootstrapAccess process applicationState)
  validatePrivateIdentity bootstrap witness access
  processFact <-
    maybe
      (fault subject ControlledProcessInvariant)
      Right
      (Controlled.controlledProcessFact process controlledState)
  require
    subject
    ControlledProcessInvariant
    ( Controlled.processFactProcessId processFact == appliedProcessId bootstrap
        && Controlled.processFactProcessEpoch processFact == process
        && Controlled.processFactResidence processFact == appliedProcessResidence bootstrap
        && Controlled.processFactAuthority processFact == appliedProcessAuthority bootstrap
    )
  require
    subject
    NormalPossessionInvariant
    ( Controlled.controlledHasNormalPossession
        process
        (globalObjectIdFromProcessEpochId process)
        controlledState
    )
  traverse_ (validateControlledRoot controlledState process) (appliedProcessRoots bootstrap)
  where
    process = appliedProcessEpochId bootstrap
    subject = StartupProcess process
    applicationState = startupApplicationState state
    identityByProcess =
      Map.fromList
        [ (witnessedProcessEpoch witness, witness)
        | witness <- Application.applicationProcessWitnesses applicationState
        ]
    controlledState = startupControlledState state

validatePrivateIdentity ::
  AppliedProcessBootstrap ->
  ProcessIdentityWitness ->
  Application.BootstrapAccess ->
  Either HeraldInvariantFault ()
validatePrivateIdentity bootstrap witness access = do
  require
    subject
    ApplicationAccessInvariant
    ( Application.bootstrapAccessProcessEpoch access == process
        && witnessedProcessEpoch witness == process
    )
  rootBindings <- accessRootBindings subject (appliedProcessRoots bootstrap) (Application.bootstrapAccessRoots access)
  wiringBindings <- traverse wiringBinding (zip wiringKeys wiringObjects)
  let bindings =
        ( globalUniqueIdFromGlobalObjectId (globalObjectIdFromProcessEpochId process),
          privateProcessUniqueId (Application.bootstrapAccessProcess access)
        )
          : rootBindings <> wiringBindings
      expectedForward = Map.fromList bindings
      actualForward = Map.fromList (witnessedBindings witness)
      actualReverse = Map.fromList (witnessedReverseBindings witness)
      invertedForward =
        Map.fromList
          [ (privateIdentity, globalIdentity)
          | (globalIdentity, privateIdentity) <- Map.toAscList actualForward
          ]
      bootstrapWords = fmap (privateUniqueIdWord64 . snd) bindings
      expectedBootstrapWords = [1 .. fromIntegral (length bindings)]
      allocatedWords =
        sort (fmap privateUniqueIdWord64 (Map.elems actualForward))
      nextWord = privateUniqueIdWord64 (witnessedNextPrivateUniqueId witness)
      expectedAllocatedWords = [1 .. fromIntegral (Map.size actualForward)]
  require
    subject
    PrivateIdentityBijectionInvariant
    ( Map.size actualForward == Map.size actualReverse
        && actualReverse == invertedForward
    )
  require
    subject
    ApplicationAccessInvariant
    (Map.isSubmapOfBy (==) expectedForward actualForward)
  require
    subject
    PrivateIdentityAllocationInvariant
    ( bootstrapWords == expectedBootstrapWords
        && all (\allocated -> allocated > 0 && allocated < nextWord) allocatedWords
        && allocatedWords == expectedAllocatedWords
        && nextWord == fromIntegral (Map.size actualForward + 1)
    )
  where
    process = appliedProcessEpochId bootstrap
    subject = StartupProcess process
    wiringObjects = appliedProcessEnvironmentHub bootstrap : map fst (appliedProcessEnvironmentEdges bootstrap)
    wiringKeys = StartupAccess.environmentHubKey : [StartupAccess.environmentEdgeKey role direction | role <- allApplicationPredefinedSortRoles, direction <- StartupAccess.allEnvironmentEdgeRoles]
    wiringBinding (key, object) = case Map.lookup key (StartupAccess.accessEntries (Application.bootstrapAccessPrimordial access)) of
      Just (StartupAccess.Object private) -> Right (globalUniqueIdFromGlobalObjectId object, privateObjectUniqueId private)
      _ -> fault subject ApplicationAccessInvariant

accessRootBindings ::
  StartupInvariantSubject ->
  [AppliedRoot] ->
  [Application.RootAccess] ->
  Either HeraldInvariantFault [(GlobalUniqueId, PrivateUniqueId)]
accessRootBindings subject roots accesses
  | length roots /= length accesses =
      fault subject ApplicationAccessInvariant
  | otherwise = traverse one (zip roots accesses)
  where
    one (root, access) = case (appliedRootRole root, access) of
      (WriterRoot nabla _, Application.WriterAccess role privateNabla _)
        | role == appliedRootCatalogueRole root ->
            Right
              ( globalUniqueIdFromGlobalObjectId (globalObjectIdFromNablaId nabla),
                privateNablaUniqueId privateNabla
              )
      (ReaderRoot delta, Application.ReaderAccess role privateDelta)
        | role == appliedRootCatalogueRole root ->
            Right
              ( globalUniqueIdFromGlobalObjectId (globalObjectIdFromDeltaId delta),
                privateDeltaUniqueId privateDelta
              )
      _ -> fault subject ApplicationAccessInvariant

validateControlledRoot ::
  Controlled.State ->
  ProcessEpochId ->
  AppliedRoot ->
  Either HeraldInvariantFault ()
validateControlledRoot controlledState process root = do
  fact <- case appliedRootRole root of
    WriterRoot nabla _ ->
      maybe
        (fault subject ControlledRootInvariant)
        Right
        (Controlled.controlledWriterFact nabla controlledState)
    ReaderRoot delta ->
      maybe
        (fault subject ControlledRootInvariant)
        Right
        (Controlled.controlledReaderFact delta controlledState)
  require
    subject
    ControlledRootInvariant
    ( Controlled.rootFactProcessEpoch fact == process
        && Controlled.rootFactCatalogueRole fact == appliedRootCatalogueRole root
        && Controlled.rootFactSortId fact == appliedRootSortId root
        && Controlled.rootFactOccurrenceId fact == appliedRootOccurrenceId root
        && Controlled.rootFactAuthority fact == appliedRootAuthority root
        && Controlled.rootFactControlPrerequisite fact
          == appliedRootControlPrerequisite root
        && controlledRoleMatches root (Controlled.rootFactRole fact)
    )
  require
    subject
    NormalPossessionInvariant
    (rootPossessionMatches (appliedRootObjectId root))
  where
    subject = StartupRoot process (appliedRootCatalogueRole root)
    rootPossessionMatches object =
      case Controlled.controlledEffectiveLabelState object controlledState of
        Just released
          | releasedLabelStateIsDeleted released ->
              not
                ( Controlled.controlledHasAnyNormalPossession
                    object
                    controlledState
                )
        _ ->
          Controlled.controlledHasNormalPossession
            process
            object
            controlledState

validateGraph ::
  HeraldState ->
  [AppliedProcessBootstrap] ->
  Either HeraldInvariantFault ()
validateGraph state _resident = do
  require
    StartupStatic
    GraphVertexSetInvariant
    (Set.fromList (Graph.graphVertices graphState) == expectedVertices)
  require
    StartupStatic
    GraphEdgeSetInvariant
    (Set.fromList (Graph.graphEdges graphState) == expectedEdges)
  where
    graphState = startupGraphState state
    topology =
      checkedInitialTopologyProjection (startupInitialBootstraps state)
    expectedVertices =
      ( Set.fromList (checkedInitialTopologyVertices topology)
          `Set.difference` genesisRootVertices
      )
        `Set.union` inducedNeutralVertices
        `Set.union` structuralVertices
        `Set.union` admissionVertices
    expectedEdges =
      Set.filter
        ( \edge ->
            Set.member (edgeSource edge) expectedVertices
              && Set.member (edgeDestination edge) expectedVertices
        )
        ( (Set.fromList (checkedInitialTopologyEdges topology) `Set.difference` genesisEnvironmentEdges)
            `Set.union` structuralEdges
        )
    reconciliation =
      GraphProgress.structuralProgressReconciliation
        (startupStructuralProgressState state)
    admissionVertices =
      Set.fromList
        [ DeltaVertex (deriveSystemViewDeltaId (checkedSystemId (startupGenesis state)) (Admission.admissionManifestHeraldEpoch (Admission.admissionRecordManifest record)) role)
        | record <- OracleProjection.projectedHeraldAdmissions (startupOracleProjectionState state),
          Admission.AdmissionActivated index _ <- [Admission.admissionRecordPhase record],
          index <= GraphProgress.structuralAppliedControlPrefix (startupStructuralProgressState state),
          role <- allPredefinedSortRoles
        ]
    structuralVertices =
      Set.fromList
        ( fmap
            Reconciliation.structuralVertexProjectionVertex
            (Map.elems (Reconciliation.structuralAppliedVertexProjections reconciliation))
        )
    structuralEdges =
      Set.fromList
        ( fmap
            Reconciliation.ownedEdgeProjectionPayload
            (Map.elems (Reconciliation.structuralAppliedEdgeProjections reconciliation))
        )
    genesisRootVertices =
      Set.fromList
        ( [ case appliedRootRole root of
              WriterRoot nabla _ -> NablaVertex nabla
              ReaderRoot delta -> DeltaVertex delta
          | (_, bootstrap) <-
              OracleProjection.projectedBootstraps
                (startupOracleProjectionState state),
            root <- appliedProcessRoots bootstrap
          ]
            <> [NeutralVertex (appliedProcessEnvironmentHub bootstrap) | (_, bootstrap) <- OracleProjection.projectedBootstraps (startupOracleProjectionState state)]
        )
    genesisEnvironmentEdges = Set.fromList [edge | (_, bootstrap) <- OracleProjection.projectedBootstraps (startupOracleProjectionState state), (_, edge) <- appliedProcessEnvironmentEdges bootstrap]
    inducedNeutralVertices =
      Set.fromList
        [ NeutralVertex object
        | (identifier, incoming) <-
            Publication.incomingPublicationEntries (startupPublicationState state),
          Publication.incomingPublicationDisposition incoming == Publication.Applied,
          IncomingControlledMatch object Current (Just NeutralVertexCarrier) <-
            [incomingControlledRelation state incoming],
          any
            (destinationActuallyApplied identifier (Publication.incomingPublicationBatch incoming))
            (Map.toAscList (Publication.incomingPublicationDestinationOutcomes incoming))
        ]
    destinationActuallyApplied identifier batch (destination, outcome)
      | outcome /= Publication.DestinationApplied = False
      | otherwise =
          case Store.lookupStoreSlot
            (publicationDestinationDelta destination)
            (startupStoreState state) of
            Nothing -> False
            Just slot ->
              Store.storeSlotIncarnation slot
                == publicationDestinationStoreIncarnation destination
                && Store.storeSlotSortId slot == publicationBatchSortId batch
                && Store.storeSlotOccurrenceId slot == publicationBatchOccurrenceId batch
                && lookup identifier (Store.storeSlotApplicationReceipts slot)
                  == Just (publicationDestinationStrength destination)

validatePlacementsAndStores ::
  HeraldState ->
  [AppliedProcessBootstrap] ->
  Either HeraldInvariantFault ()
validatePlacementsAndStores state _resident =
  traverse_ validateResource activeResources
  where
    placementState = startupPlacementState state
    storeState = startupStoreState state
    reconciliation =
      GraphProgress.structuralProgressReconciliation
        (startupStructuralProgressState state)
    activeResources =
      Reconciliation.structuralAppliedLocalStores reconciliation
    retainedRoutes =
      Reconciliation.prepareDeltaRouteAdmission reconciliation

    validateResource resource = do
      let delta = Reconciliation.dynamicStoreDelta resource
          typedSort = Reconciliation.dynamicStoreSort resource
          process = Reconciliation.dynamicStoreProcess resource
          herald = Reconciliation.dynamicStoreHerald resource
          incarnation = Reconciliation.dynamicStoreIncarnation resource
          controller = Reconciliation.dynamicStoreController resource
          subject = StartupDelta delta
      prerequisite <-
        maybe
          (fault subject PlacementFactInvariant)
          Right
          ( Reconciliation.preparedDeltaRouteLatestPrerequisite
              delta
              controller
              typedSort
              (Reconciliation.LiveProcessController process herald)
              retainedRoutes
          )
      placement <-
        maybe
          (fault subject PlacementFactInvariant)
          Right
          (Placement.lookupLocalPlacement delta placementState)
      require
        subject
        PlacementFactInvariant
        ( Placement.localPlacementDelta placement == delta
            && Placement.localPlacementSortId placement
              == StructuralDebt.sortOccurrenceSortId typedSort
            && Placement.localPlacementOccurrenceId placement
              == StructuralDebt.sortOccurrenceDefinition typedSort
            && Placement.localPlacementControllerObject placement == controller
            && Placement.localPlacementProcessEpoch placement == process
            && Placement.localPlacementHeraldEpoch placement == herald
            && Placement.localPlacementStoreIncarnation placement == incarnation
            && Placement.localPlacementControlPrerequisite placement == prerequisite
        )
      slot <-
        maybe
          (fault subject StoreFactInvariant)
          Right
          (Store.lookupStoreSlot delta storeState)
      require
        subject
        StoreFactInvariant
        ( Store.storeSlotDelta slot == delta
            && Store.storeSlotSortId slot
              == StructuralDebt.sortOccurrenceSortId typedSort
            && Store.storeSlotOccurrenceId slot
              == StructuralDebt.sortOccurrenceDefinition typedSort
            && Store.storeSlotIncarnation slot == incarnation
        )
      -- This cross-owner check binds the live resource to the physical Store
      -- shape only. 'validateStoreSlotReceipts' separately checks every
      -- retained Store with provenance-sensitive seed semantics: bootstrap
      -- readers replay matching primordial definitions, whereas structural
      -- application/control replacement Stores are intentionally born empty.
      require
        subject
        StoreContentsInvariant
        ( deltaStoreSort (Store.storeSlotContents slot)
            == StructuralDebt.sortOccurrenceSortId typedSort
        )

bootstrapRootObjects :: AppliedProcessBootstrap -> [GlobalObjectId]
bootstrapRootObjects = fmap appliedRootObjectId . appliedProcessRoots

controlledRootObject :: Controlled.RootFact -> GlobalObjectId
controlledRootObject fact = case Controlled.rootFactRole fact of
  Controlled.ControlledWriter nabla _ -> globalObjectIdFromNablaId nabla
  Controlled.ControlledReader delta -> globalObjectIdFromDeltaId delta

controlledRoleMatches :: AppliedRoot -> Controlled.ControlledRootRole -> Bool
controlledRoleMatches root controlled = case (appliedRootRole root, controlled) of
  ( WriterRoot expected expectedSequencing,
    Controlled.ControlledWriter actual actualSequencing
    ) ->
      expected == actual && expectedSequencing == actualSequencing
  (ReaderRoot expected, Controlled.ControlledReader actual) -> expected == actual
  _ -> False

require ::
  StartupInvariantSubject ->
  StartupInvariantViolation ->
  Bool ->
  Either HeraldInvariantFault ()
require subject violation condition =
  unless condition (fault subject violation)

fault ::
  StartupInvariantSubject ->
  StartupInvariantViolation ->
  Either HeraldInvariantFault value
fault subject violation = Left (HeraldStartupInvariant subject violation)

traverse_ :: (value -> Either error ()) -> [value] -> Either error ()
traverse_ action = foldr (\value rest -> action value >> rest) (Right ())
