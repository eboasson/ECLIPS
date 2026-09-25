{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Atomic admission of established, sequenced peer publication input.
--
-- This coordinator owns no durable state. It prepares one successor cut across
-- the eight semantic owners changed by incoming publication work. Discovery,
-- Placement, checked genesis, and the applied Oracle projection are read-only
-- authority context.
module Eclips.Herald.UseCase.PeerInput
  ( PeerInputContext,
    peerInputContext,
    RoutedPublicationAuthorityClaim,
    routedPublicationAuthorityClaim,
    validateRoutedPublicationAuthority,
    validateRoutedPublicationAuthorityHistory,
    validateRetainedStorePublication,
    structuralPublicationDestinationsValid,
    structuralPublicationDestinationsValidAt,
    PeerInputState,
    peerInputState,
    peerInputPublicationState,
    peerInputPeerStreamState,
    peerInputStoreState,
    peerInputGraphState,
    peerInputIdGeneratorState,
    peerInputStructuralProgressState,
    peerInputAlignmentState,
    peerInputPlacementState,
    peerInputControlledState,
    peerInputSortRegistryState,
    peerInputApplicationState,
    peerInputWaitState,
    peerInputLabelBarrierState,
    peerInputDisappearanceState,
    PeerInputDisposition (..),
    PeerAcknowledgementPlan,
    peerAcknowledgementDirection,
    peerAcknowledgementReceivedPrefix,
    peerAcknowledgementCompletedPrefix,
    peerAcknowledgementCompletion,
    peerAcknowledgementGapSummary,
    peerAcknowledgementRepairOffer,
    DeferredPeerRejection,
    deferredPeerRejectionDirection,
    deferredPeerRejectionPublicationId,
    PeerInputResult,
    peerInputDisposition,
    peerInputAcknowledgements,
    peerInputPublicationIds,
    peerInputPlacementSnapshots,
    peerInputWakes,
    peerInputDeferredRejections,
    PeerDependencyReleaseDisposition (..),
    PeerDependencyReleaseResult,
    peerDependencyReleaseDisposition,
    peerDependencyReleasedPublications,
    peerDependencyAcknowledgements,
    peerDependencyPlacementSnapshots,
    peerDependencyWakes,
    peerDependencyDeferredRejections,
    PeerControlReleaseResult,
    peerControlReleasedPublications,
    peerControlAcknowledgements,
    peerControlPlacementSnapshots,
    peerControlWakes,
    peerControlDeferredRejections,
    PeerInputProtocolViolation (..),
    PeerInputInvariantViolation (..),
    PeerInputProblemDisposition (..),
    PeerInputProblem (..),
    peerInputProblemDisposition,
    TerminalStructuralMaterializationDisposition (..),
    TerminalStructuralMaterializationResult,
    terminalStructuralMaterializationDisposition,
    terminalStructuralMaterializationPlacementSnapshots,
    terminalStructuralMaterializationWakes,
    materializeTerminalStructuralOccurrence,
    replayHistoricalStructuralOccurrence,
    applyPeerLogicalItem,
    applyPeerPublication,
    releasePeerDependency,
    releasePeerSuccessorStructuralBase,
    releasePeerTopology,
    releasePeerControl,
    releasePendingAlignmentRouteCutovers,
    releasePendingAlignmentRouteCutoversExhaustive,
    releasePendingDisappearanceProbeMarkers,
    releasePendingDisappearanceProbeMarkersExhaustive,
    samePeerInputMarkerWorkState,
  )
where

import Control.Monad (foldM, unless)
import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Disappearance (DisappearanceProbeId, disappearanceProbeOpenControlIndex)
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    IdentityError,
    NablaId,
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    StructuralOccurrenceId,
    TopologyCutId,
    globalUniqueIdBytes,
    mkStoreIncarnationId,
    publicationAuthorityEpoch,
    publicationNabla,
    publicationSourceHeraldEpoch,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    heraldMembershipGenerationActiveHeraldEpochs,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    PublicationError,
    checkedPublicationCanonicalValue,
    checkedPublicationId,
    checkedPublicationLifecycle,
    checkedPublicationSort,
    checkedPublicationValue,
    mkCheckedPublication,
  )
import Eclips.Domain.Route (ReplicaStrength)
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalCheckedDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( Lifecycle (Current),
    SortKind (ControlledSort),
    StructuralCarrierRole (..),
    descriptorKind,
    descriptorStructuralCarrierRole,
  )
import Eclips.Domain.Sort.Profile
  ( SortDefinitionIntrinsicError,
    StructuralCarrierReferenceError,
    decodeSortDefinitionValue,
    structuralCarrierSortReferences,
  )
import Eclips.Domain.Startup
  ( PredefinedSortRole (..),
    allPredefinedSortRoles,
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
  )
import Eclips.Domain.Structural
  ( StructuralVersionVector,
    structuralVersionVectorMembershipGenerationId,
  )
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    structuralOccurrenceCause,
  )
import Eclips.Domain.Value
  ( ValueError,
    canonicalValueByteString,
    decodeCanonicalValue,
  )
import Eclips.Herald.Alignment.Protocol
  ( alignmentRouteCutoverMarkerGeneration,
    alignmentRouteCutoverMarkerPredecessorHerald,
    alignmentRouteCutoverMarkerSourceHerald,
  )
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Authority qualified as Authority
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Disappearance.Gate qualified as DisappearanceGate
import Eclips.Herald.Disappearance.Protocol qualified as DisappearanceProtocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.Discovery
  ( DiscoveryInvariantFault,
    PeerBinding,
    peerBindingRemoteHeraldEpoch,
  )
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectivePublication
  ( EffectivePublicationProblem,
    PublicationHiddenReason (PublicationObsoleteSuppressed),
    checkEffectivePublicationContext,
    effectivePublicationHiddenReason,
    interpretEffectivePublication,
  )
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    checkedLocalHeraldEpoch,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource
  ( CarriedStampedSurvivor,
    SuccessorStructuralBase,
    TerminalStructuralOccurrence,
    successorStructuralBaseCutId,
    successorStructuralBaseGenerationId,
    successorStructuralBaseReleases,
    terminalStructuralOccurrencePayloadBytes,
    terminalStructuralOccurrenceStamp,
  )
import Eclips.Herald.IdGenerator.State qualified as IdGenerator
import Eclips.Herald.LabelBarrier.State qualified as LabelBarrier
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerPayload
  ( PeerLogicalPayload (..),
    peerLogicalPayloadDigest,
  )
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    PeerPublicationProblem (..),
    PublicationBatch,
    PublicationBatchProblem,
    PublicationDestination,
    StructuralOccurrenceStamp,
    StructuralPublicationSemantics,
    authenticatePeerPublication,
    decodeStructuralPublicationCanonicalBytes,
    mkPublicationBatch,
    mkStructuralPeerPublication,
    peerPublicationBatch,
    peerPublicationStructuralStamp,
    publicationBatchCanonicalValue,
    publicationBatchControlPrerequisite,
    publicationBatchDestinations,
    publicationBatchId,
    publicationBatchOccurrenceId,
    publicationBatchSortId,
    publicationBatchSourceProcess,
    publicationBatchSourceStrength,
    publicationBatchSourceTopologyPrerequisite,
    publicationDestination,
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
  ( AssignmentReceipt,
    GapSummary,
    ReceiveDisposition,
    ReceiveProgress (..),
    ResumeOffer,
    SequencedItem,
    StreamDirection,
    StreamPrefix,
    StreamSequence,
    assignmentReceiptDirection,
    assignmentReceiptSequence,
    gapSummaryEntries,
    nextAfterStreamPrefix,
    nextStreamSequence,
    receiveResultDisposition,
    resumeOfferGapSummary,
    sequencedItem,
    sequencedItemDigest,
    sequencedItemDirection,
    sequencedItemPayload,
    sequencedItemSequence,
    streamCompletedPrefix,
    streamCompletionPrefix,
    streamCompletionProgress,
    streamDirectionDestination,
    streamDirectionSource,
    streamReceivedPrefix,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement (PlacementSnapshot)
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Query (ResolvedQuery)
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Store.Observation qualified as StoreObservation
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt
  ( sortOccurrence,
    sortOccurrenceDefinition,
    sortOccurrenceSortId,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.UseCase.Alignment qualified as AlignmentUseCase
import Eclips.Herald.Wait.State qualified as Wait
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)

-- | Read-only facts needed to admit one incoming transaction.
data PeerInputContext = PeerInputContext
  { genesis :: CheckedHeraldGenesis,
    oracle :: OracleProjection.State,
    discovery :: Discovery.State,
    placement :: Placement.State
  }

peerInputContext ::
  CheckedHeraldGenesis ->
  OracleProjection.State ->
  Discovery.State ->
  Placement.State ->
  PeerInputContext
peerInputContext genesis oracle discovery placement =
  PeerInputContext {genesis, oracle, discovery, placement}

-- | Destination-independent source-authority facts for one routed retained
-- publication.  The constructor is hidden so callers cannot omit the exact
-- descriptor/value semantics needed to authenticate a structural stamp.
data RoutedPublicationAuthorityClaim = RoutedPublicationAuthorityClaim
  { descriptor :: CanonicalDescriptor,
    sortOccurrence :: SortDefinitionOccurrenceId,
    publication :: CheckedPublication,
    sourceProcess :: ProcessEpochId,
    sourceStrength :: ReplicaStrength,
    sourceTopologyPrerequisite :: TopologyCutId,
    controlPrerequisite :: ControlIndex,
    structuralStamp :: Maybe StructuralOccurrenceStamp
  }

routedPublicationAuthorityClaim ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  ProcessEpochId ->
  ReplicaStrength ->
  TopologyCutId ->
  ControlIndex ->
  Maybe StructuralOccurrenceStamp ->
  RoutedPublicationAuthorityClaim
routedPublicationAuthorityClaim
  descriptor
  definitionOccurrence
  publication
  sourceProcess
  sourceStrength
  sourceTopologyPrerequisite
  controlPrerequisite
  structuralStamp =
    RoutedPublicationAuthorityClaim
      { descriptor,
        sortOccurrence = definitionOccurrence,
        publication,
        sourceProcess,
        sourceStrength,
        sourceTopologyPrerequisite,
        controlPrerequisite,
        structuralStamp
      }

-- | Authenticate a retained routed observation against the same immutable
-- writer law used by ordinary peer ingress.  No destination fact participates:
-- the check binds source Herald/process, topology and control prerequisites,
-- authority, sort occurrence, canonical checked semantics, and (when present)
-- the recomputed destination-independent structural publication digest.
validateRoutedPublicationAuthority ::
  PeerInputContext ->
  GraphProgress.StructuralProgressState ->
  Controlled.State ->
  RoutedPublicationAuthorityClaim ->
  Either PeerInputProblem ()
validateRoutedPublicationAuthority context structuralProgress controlled claim =
  ()
    <$ validateRoutedPublicationAuthorityFacts
      RequireCurrentApplicability
      context
      structuralProgress
      controlled
      claim

-- | Authenticate immutable retained evidence at its exact historical
-- coordinate without requiring that its source remains currently operable.
-- Current deletion/End applicability belongs to live driving paths, not to
-- whole-state validation of accepted history.
validateRoutedPublicationAuthorityHistory ::
  PeerInputContext ->
  GraphProgress.StructuralProgressState ->
  Controlled.State ->
  RoutedPublicationAuthorityClaim ->
  Either PeerInputProblem ()
validateRoutedPublicationAuthorityHistory context structuralProgress controlled claim =
  ()
    <$ validateRoutedPublicationAuthorityFacts
      RetainedHistoryOnly
      context
      structuralProgress
      controlled
      claim

data AuthorityApplicability
  = RequireCurrentApplicability
  | RetainedHistoryOnly

-- | Check the immutable publication representation of previously admitted
-- Store state. The caller must have admitted its alignment or Join source and
-- exact transfer obligation before using this check. The original writer does
-- not exercise its authority again when a Store transfers an old value: its
-- topology/control coordinates remain provenance, not history dependencies.
-- Keep carrier, stamp and digest consistency independent of that history.
validateRetainedStorePublication ::
  RoutedPublicationAuthorityClaim -> Either PeerInputProblem ()
validateRetainedStorePublication = validateRoutedPublicationSemantics

-- | The exact mutable owner slice installed together by the caller.
data PeerInputState = PeerInputState
  { publication :: Publication.State,
    peerStream :: PeerStream.State PeerLogicalPayload,
    store :: Store.State,
    graph :: Graph.State,
    idGenerator :: IdGenerator.State,
    structuralProgress :: GraphProgress.StructuralProgressState,
    alignment :: Alignment.State,
    placement :: Placement.State,
    controlled :: Controlled.State,
    sortRegistry :: SortRegistry.State,
    application :: Application.State,
    wait :: Wait.State,
    labelBarrier :: LabelBarrier.State,
    disappearance :: Disappearance.State
  }
  deriving stock (Eq)

peerInputState ::
  Publication.State ->
  PeerStream.State PeerLogicalPayload ->
  Store.State ->
  Graph.State ->
  IdGenerator.State ->
  GraphProgress.StructuralProgressState ->
  Alignment.State ->
  Placement.State ->
  Controlled.State ->
  SortRegistry.State ->
  Application.State ->
  Wait.State ->
  LabelBarrier.State ->
  Disappearance.State ->
  PeerInputState
peerInputState
  publication
  peerStream
  store
  graph
  idGenerator
  structuralProgress
  alignment
  placement
  controlled
  sortRegistry
  application
  wait
  labelBarrier
  disappearance =
    PeerInputState
      { publication,
        peerStream,
        store,
        graph,
        idGenerator,
        structuralProgress,
        alignment,
        placement,
        controlled,
        sortRegistry,
        application,
        wait,
        labelBarrier,
        disappearance
      }

peerInputPublicationState :: PeerInputState -> Publication.State
peerInputPublicationState state = state.publication

peerInputPeerStreamState :: PeerInputState -> PeerStream.State PeerLogicalPayload
peerInputPeerStreamState state = state.peerStream

peerInputStoreState :: PeerInputState -> Store.State
peerInputStoreState state = state.store

peerInputGraphState :: PeerInputState -> Graph.State
peerInputGraphState state = state.graph

peerInputIdGeneratorState :: PeerInputState -> IdGenerator.State
peerInputIdGeneratorState state = state.idGenerator

peerInputStructuralProgressState ::
  PeerInputState -> GraphProgress.StructuralProgressState
peerInputStructuralProgressState state = state.structuralProgress

peerInputAlignmentState :: PeerInputState -> Alignment.State
peerInputAlignmentState state = state.alignment

peerInputPlacementState :: PeerInputState -> Placement.State
peerInputPlacementState state = state.placement

peerInputControlledState :: PeerInputState -> Controlled.State
peerInputControlledState state = state.controlled

peerInputSortRegistryState :: PeerInputState -> SortRegistry.State
peerInputSortRegistryState state = state.sortRegistry

peerInputApplicationState :: PeerInputState -> Application.State
peerInputApplicationState state = state.application

peerInputWaitState :: PeerInputState -> Wait.State
peerInputWaitState state = state.wait

peerInputLabelBarrierState :: PeerInputState -> LabelBarrier.State
peerInputLabelBarrierState state = state.labelBarrier

peerInputDisappearanceState :: PeerInputState -> Disappearance.State
peerInputDisappearanceState state = state.disappearance

data PeerInputDisposition
  = PeerInputStaleBinding
  | PeerInputReceived ReceiveDisposition
  deriving stock (Eq, Show)

-- | Final cumulative logical progress ready for established-control encoding.
data PeerAcknowledgementPlan
  = PeerAcknowledgementPlan
      StreamDirection
      StreamPrefix
      ReceiptRetirement
      GapSummary
      (Maybe ResumeOffer)
  deriving stock (Eq, Show)

peerAcknowledgementDirection :: PeerAcknowledgementPlan -> StreamDirection
peerAcknowledgementDirection (PeerAcknowledgementPlan direction _ _ _ _) = direction

peerAcknowledgementReceivedPrefix :: PeerAcknowledgementPlan -> StreamPrefix
peerAcknowledgementReceivedPrefix (PeerAcknowledgementPlan _ received _ _ _) = received

peerAcknowledgementCompletedPrefix :: PeerAcknowledgementPlan -> StreamPrefix
peerAcknowledgementCompletedPrefix = streamCompletionPrefix . peerAcknowledgementCompletion

peerAcknowledgementCompletion :: PeerAcknowledgementPlan -> ReceiptRetirement
peerAcknowledgementCompletion (PeerAcknowledgementPlan _ _ completed _ _) = completed

peerAcknowledgementGapSummary :: PeerAcknowledgementPlan -> GapSummary
peerAcknowledgementGapSummary (PeerAcknowledgementPlan _ _ _ gap _) = gap

peerAcknowledgementRepairOffer :: PeerAcknowledgementPlan -> Maybe ResumeOffer
peerAcknowledgementRepairOffer (PeerAcknowledgementPlan _ _ _ _ repair) = repair

-- | A protocol-invalid retained item discovered only after one of its
-- semantic prerequisites became available. The direction identifies the
-- binding to close. Publication failures retain their semantic identifier;
-- control-only stream items deliberately have no Publication owner.
data DeferredPeerRejection = DeferredPeerRejection
  { direction :: StreamDirection,
    publicationId :: Maybe PublicationId
  }
  deriving stock (Eq, Show)

deferredPeerRejectionDirection :: DeferredPeerRejection -> StreamDirection
deferredPeerRejectionDirection rejection = rejection.direction

deferredPeerRejectionPublicationId :: DeferredPeerRejection -> Maybe PublicationId
deferredPeerRejectionPublicationId rejection = rejection.publicationId

data PeerInputResult = PeerInputResult
  { disposition :: PeerInputDisposition,
    acknowledgements :: [PeerAcknowledgementPlan],
    publications :: [PublicationId],
    placementSnapshots :: [PlacementSnapshot],
    wakes :: [Application.ApplicationWaitWake],
    deferredRejections :: [DeferredPeerRejection]
  }
  deriving stock (Eq, Show)

peerInputDisposition :: PeerInputResult -> PeerInputDisposition
peerInputDisposition result = result.disposition

peerInputAcknowledgements :: PeerInputResult -> [PeerAcknowledgementPlan]
peerInputAcknowledgements result = result.acknowledgements

peerInputPublicationIds :: PeerInputResult -> [PublicationId]
peerInputPublicationIds result = result.publications

peerInputPlacementSnapshots :: PeerInputResult -> [PlacementSnapshot]
peerInputPlacementSnapshots result = result.placementSnapshots

peerInputWakes :: PeerInputResult -> [Application.ApplicationWaitWake]
peerInputWakes result = result.wakes

peerInputDeferredRejections :: PeerInputResult -> [DeferredPeerRejection]
peerInputDeferredRejections result = result.deferredRejections

data PeerDependencyReleaseDisposition
  = PeerDependencyStillHeld
  | PeerDependencyReleased
  deriving stock (Eq, Ord, Show)

data PeerDependencyReleaseResult = PeerDependencyReleaseResult
  { disposition :: PeerDependencyReleaseDisposition,
    publications :: [PublicationId],
    acknowledgements :: [PeerAcknowledgementPlan],
    placementSnapshots :: [PlacementSnapshot],
    wakes :: [Application.ApplicationWaitWake],
    deferredRejections :: [DeferredPeerRejection]
  }
  deriving stock (Eq, Show)

peerDependencyReleaseDisposition ::
  PeerDependencyReleaseResult -> PeerDependencyReleaseDisposition
peerDependencyReleaseDisposition result = result.disposition

peerDependencyReleasedPublications :: PeerDependencyReleaseResult -> [PublicationId]
peerDependencyReleasedPublications result = result.publications

peerDependencyAcknowledgements ::
  PeerDependencyReleaseResult -> [PeerAcknowledgementPlan]
peerDependencyAcknowledgements result = result.acknowledgements

peerDependencyPlacementSnapshots ::
  PeerDependencyReleaseResult -> [PlacementSnapshot]
peerDependencyPlacementSnapshots result = result.placementSnapshots

peerDependencyWakes ::
  PeerDependencyReleaseResult -> [Application.ApplicationWaitWake]
peerDependencyWakes result = result.wakes

peerDependencyDeferredRejections ::
  PeerDependencyReleaseResult -> [DeferredPeerRejection]
peerDependencyDeferredRejections result = result.deferredRejections

data PeerControlReleaseResult = PeerControlReleaseResult
  { publications :: [PublicationId],
    acknowledgements :: [PeerAcknowledgementPlan],
    placementSnapshots :: [PlacementSnapshot],
    wakes :: [Application.ApplicationWaitWake],
    deferredRejections :: [DeferredPeerRejection]
  }
  deriving stock (Eq, Show)

peerControlReleasedPublications :: PeerControlReleaseResult -> [PublicationId]
peerControlReleasedPublications result = result.publications

peerControlAcknowledgements :: PeerControlReleaseResult -> [PeerAcknowledgementPlan]
peerControlAcknowledgements result = result.acknowledgements

peerControlPlacementSnapshots ::
  PeerControlReleaseResult -> [PlacementSnapshot]
peerControlPlacementSnapshots result = result.placementSnapshots

peerControlWakes :: PeerControlReleaseResult -> [Application.ApplicationWaitWake]
peerControlWakes result = result.wakes

peerControlDeferredRejections ::
  PeerControlReleaseResult -> [DeferredPeerRejection]
peerControlDeferredRejections result = result.deferredRejections

data PeerInputProtocolViolation
  = PeerInputBindingSourceMismatch HeraldEpoch HeraldEpoch
  | PeerInputBindingDestinationMismatch HeraldEpoch HeraldEpoch
  | PeerInputPublicationSourceEpochMismatch HeraldEpoch HeraldEpoch
  | PeerInputSourceProcessUnknown ProcessEpochId
  | PeerInputSourceProcessResidenceMismatch ProcessEpochId HeraldEpoch HeraldEpoch
  | PeerInputSourceWriterUnknown ProcessEpochId NablaId
  | PeerInputSourceAuthorityMismatch NablaId AuthorityEpoch AuthorityEpoch
  | PeerInputSourceSortMismatch NablaId SortId SortId
  | PeerInputSourceOccurrenceMismatch
      NablaId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | PeerInputSourceTopologyUnavailable TopologyCutId
  | PeerInputSourceControlUnavailable ControlIndex
  | PeerInputStructuralDestinationMismatch
      PublicationDestination
      (NonEmpty PublicationDestination)
  | PeerInputEffectiveOccurrenceMismatch
      SortId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | PeerInputTerminalStructuralSortUnknown SortId
  | PeerInputTerminalStructuralOccurrenceConflict StructuralOccurrenceId
  | PeerInputCurrentDestinationSortMismatch DeltaId SortId SortId
  | PeerInputCurrentDestinationOccurrenceMismatch
      DeltaId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | PeerInputCanonicalValueMismatch PublicationId
  | PeerInputStructuralCarrierReferenceProblem
      PublicationId
      StructuralCarrierReferenceError
  | PeerInputRouteCutoverSourceMismatch HeraldEpoch HeraldEpoch
  | PeerInputRouteCutoverPredecessorMismatch HeraldEpoch HeraldEpoch
  | PeerInputRouteCutoverDigestMismatch
  | PeerInputDisappearanceMarkerCoordinateMismatch
  | PeerInputDisappearanceMarkerDigestMismatch
  | PeerInputControlledObservationRejected
      PublicationId
      Controlled.ControlledPeerObservationError
  deriving stock (Eq, Show)

data PeerInputInvariantViolation
  = PeerInputDependencyWaiterMissing PublicationId
  | PeerInputDependencyWaiterContradiction
      PublicationId
      Publication.PublicationDependency
  | PeerInputReadySortMissing PublicationId SortId
  | PeerInputDefinitionControlPrerequisiteMismatch
      PublicationId
      ControlIndex
      ControlIndex
  | PeerInputControlledObservationContradiction
      PublicationId
      Controlled.ControlledObservationError
  | PeerInputControlledSuppressionContradiction PublicationId Bool Bool
  | PeerInputRetiredOccurrenceStoreApplied PublicationId
  | PeerInputRejectedResolutionContradiction PublicationId
  | PeerInputSuccessorStructuralBaseMembershipMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | PeerInputSuccessorStructuralProgressMembershipMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | PeerInputSuccessorStructuralBaseCutMismatch TopologyCutId TopologyCutId
  deriving stock (Eq, Show)

data PeerInputProblem
  = PeerInputProtocolProblem PeerInputProtocolViolation
  | PeerInputInvariantProblem PeerInputInvariantViolation
  | PeerInputDiscoveryProblem DiscoveryInvariantFault
  | PeerInputPlacementProblem Placement.PlacementProblem
  | PeerInputStructuralPlacementProblem Placement.StructuralPlacementPatchError
  | PeerInputStreamProblem PeerStream.PeerStreamProblem
  | PeerInputPublicationProblem Publication.IncomingPublicationProblem
  | PeerInputStoreProblem Store.StoreTransitionError
  | PeerInputEffectiveStoreProblem StoreObservation.EffectiveStoreProblem
  | PeerInputEffectivePublicationProblem EffectivePublicationProblem
  | PeerInputStructuralStoreProblem Store.StructuralStorePatchError
  | PeerInputValueProblem ValueError
  | PeerInputCheckedPublicationProblem PublicationError
  | PeerInputPublicationBatchProblem PublicationBatchProblem
  | PeerInputPeerPublicationProblem PeerPublicationProblem
  | PeerInputStructuralReconciliationProblem
      Reconciliation.StructuralReconciliationProblem
  | PeerInputStructuralProgressProblem GraphProgress.StructuralProgressProblem
  | PeerInputStructuralProgressInvariantProblem
      GraphProgress.StructuralProgressInvariantProblem
  | PeerInputIdGeneratorProblem IdGenerator.IdGeneratorInvariant
  | PeerInputStoreIncarnationIdentityProblem IdentityError
  | PeerInputAlignmentProblem Alignment.AlignmentDebtProblem
  | PeerInputAlignmentRouteCutoverProblem
      AlignmentUseCase.AlignmentRouteCutoverProblem
  | PeerInputDisappearanceProblem Disappearance.DisappearanceProblem
  | PeerInputDefinitionProblem SortDefinitionIntrinsicError
  | PeerInputSortInductionProblem SortRegistry.SortInductionError
  | PeerInputWaitProblem Wait.WaitPreparationError
  | PeerInputApplicationProblem Application.ApplicationSessionTransitionError
  deriving stock (Eq, Show)

-- | Coordinator policy for an already typed admission failure.
--
-- Malformed or conflicting peer claims close only the current binding. Every
-- contradiction among owner-derived checked facts terminates the transition.
data PeerInputProblemDisposition
  = RejectCurrentPeerBinding
  | PeerInputInvariantFault
  deriving stock (Eq, Ord, Show)

peerInputProblemDisposition :: PeerInputProblem -> PeerInputProblemDisposition
peerInputProblemDisposition problem = case problem of
  PeerInputProtocolProblem _ -> RejectCurrentPeerBinding
  PeerInputStreamProblem (PeerStream.PeerStreamProtocolProblem _) ->
    RejectCurrentPeerBinding
  PeerInputPublicationProblem publicationProblem -> case publicationProblem of
    Publication.IncomingPublicationWrongDestination {} -> RejectCurrentPeerBinding
    Publication.IncomingPublicationWrongSource {} -> RejectCurrentPeerBinding
    Publication.IncomingPublicationDigestMismatch {} -> RejectCurrentPeerBinding
    Publication.IncomingPublicationDirectionConflict {} -> RejectCurrentPeerBinding
    Publication.IncomingPublicationItemConflict {} -> RejectCurrentPeerBinding
    Publication.IncomingPublicationSemanticDigestConflict {} -> RejectCurrentPeerBinding
    Publication.IncomingPublicationRejectedSemanticReassignment {} -> RejectCurrentPeerBinding
    Publication.IncomingAssignmentAlreadyOwned {} -> RejectCurrentPeerBinding
    _ -> PeerInputInvariantFault
  PeerInputValueProblem _ -> RejectCurrentPeerBinding
  PeerInputCheckedPublicationProblem _ -> RejectCurrentPeerBinding
  PeerInputPublicationBatchProblem _ -> RejectCurrentPeerBinding
  PeerInputPeerPublicationProblem _ -> RejectCurrentPeerBinding
  PeerInputStructuralReconciliationProblem _ -> PeerInputInvariantFault
  PeerInputStructuralProgressProblem _ -> PeerInputInvariantFault
  PeerInputStructuralProgressInvariantProblem _ -> PeerInputInvariantFault
  PeerInputIdGeneratorProblem _ -> PeerInputInvariantFault
  PeerInputStoreIncarnationIdentityProblem _ -> PeerInputInvariantFault
  PeerInputAlignmentProblem _ -> PeerInputInvariantFault
  PeerInputAlignmentRouteCutoverProblem _ -> RejectCurrentPeerBinding
  PeerInputDisappearanceProblem _ -> RejectCurrentPeerBinding
  PeerInputDefinitionProblem _ -> RejectCurrentPeerBinding
  PeerInputInvariantProblem _ -> PeerInputInvariantFault
  PeerInputDiscoveryProblem _ -> PeerInputInvariantFault
  PeerInputPlacementProblem _ -> PeerInputInvariantFault
  PeerInputStructuralPlacementProblem _ -> PeerInputInvariantFault
  PeerInputStreamProblem (PeerStream.PeerStreamInvariantProblem _) ->
    PeerInputInvariantFault
  PeerInputStoreProblem _ -> PeerInputInvariantFault
  PeerInputEffectiveStoreProblem _ -> PeerInputInvariantFault
  PeerInputEffectivePublicationProblem _ -> PeerInputInvariantFault
  PeerInputStructuralStoreProblem _ -> PeerInputInvariantFault
  PeerInputSortInductionProblem _ -> PeerInputInvariantFault
  PeerInputWaitProblem _ -> PeerInputInvariantFault
  PeerInputApplicationProblem _ -> PeerInputInvariantFault

-- | Whether one checked terminal-source payload could be installed against
-- the current causal/control cut.  A held result mutates no semantic owner;
-- the terminal coordinator retains the exact payload and may re-drive it when
-- another occurrence or prerequisite advances.
data TerminalStructuralMaterializationDisposition
  = TerminalStructuralMaterializationHeld
  | TerminalStructuralMaterialized
  deriving stock (Eq, Ord, Show)

data TerminalStructuralMaterializationResult = TerminalStructuralMaterializationResult
  { disposition :: TerminalStructuralMaterializationDisposition,
    placementSnapshots :: [PlacementSnapshot],
    wakes :: [Application.ApplicationWaitWake]
  }
  deriving stock (Eq, Show)

terminalStructuralMaterializationDisposition ::
  TerminalStructuralMaterializationResult ->
  TerminalStructuralMaterializationDisposition
terminalStructuralMaterializationDisposition result = result.disposition

terminalStructuralMaterializationPlacementSnapshots ::
  TerminalStructuralMaterializationResult -> [PlacementSnapshot]
terminalStructuralMaterializationPlacementSnapshots result = result.placementSnapshots

terminalStructuralMaterializationWakes ::
  TerminalStructuralMaterializationResult -> [Application.ApplicationWaitWake]
terminalStructuralMaterializationWakes result = result.wakes

-- | Re-admit a survivor-relayed retired-source occurrence without inventing a
-- peer-stream assignment or expanding receiver-local routes.  The opaque
-- transcript is decoded at the 'PeerPublication' boundary, authenticated
-- against the retained stamp and live registry/history, and materialized only
-- into this Herald's mandatory private system view plus structural owners.
--
-- The terminal coordinator, rather than Publication's ordinary stream owner,
-- retains the exact payload for idempotent retry.  Consequently a dependency
-- hold is a complete owner no-op and a later retry replays this same checked
-- function.
materializeTerminalStructuralOccurrence ::
  PeerInputContext ->
  TerminalStructuralOccurrence ->
  PeerInputState ->
  Either
    PeerInputProblem
    (PeerInputState, TerminalStructuralMaterializationResult)
materializeTerminalStructuralOccurrence context occurrence predecessor = do
  validateContext context
  mapLeft
    PeerInputPlacementProblem
    (Placement.validatePlacementState predecessor.placement)
  mapLeft
    PeerInputStructuralProgressInvariantProblem
    (GraphProgress.validateStructuralProgressState predecessor.structuralProgress)
  semantics <-
    mapLeft
      PeerInputPeerPublicationProblem
      ( decodeStructuralPublicationCanonicalBytes
          (terminalStructuralOccurrencePayloadBytes occurrence)
      )
  let stamp = terminalStructuralOccurrenceStamp occurrence
      publicationIdentifier =
        structuralPublicationSemanticsPublicationId semantics
      sortId = structuralPublicationSemanticsSortId semantics
      definitionOccurrence = structuralPublicationSemanticsOccurrenceId semantics
  unless
    (structuralOccurrenceStampPublication stamp == publicationIdentifier)
    ( Left
        ( PeerInputPeerPublicationProblem
            ( StructuralStampPublicationMismatch
                publicationIdentifier
                (structuralOccurrenceStampPublication stamp)
            )
        )
    )
  unless
    ( structuralOccurrenceStampCarrierRole stamp
        == structuralPublicationSemanticsCarrierRole semantics
    )
    ( Left
        ( PeerInputPeerPublicationProblem
            ( StructuralStampCarrierRoleMismatch
                (structuralPublicationSemanticsCarrierRole semantics)
                (structuralOccurrenceStampCarrierRole stamp)
            )
        )
    )
  case SortRegistry.lookupEffectiveSort sortId predecessor.sortRegistry of
    Nothing ->
      Left
        ( PeerInputProtocolProblem
            (PeerInputTerminalStructuralSortUnknown sortId)
        )
    Just entry -> do
      unless
        (SortRegistry.registryEntryOccurrenceId entry == definitionOccurrence)
        ( Left
            ( PeerInputProtocolProblem
                ( PeerInputEffectiveOccurrenceMismatch
                    sortId
                    (SortRegistry.registryEntryOccurrenceId entry)
                    definitionOccurrence
                )
            )
        )
      checked <- checkedTerminalStructuralPublication entry semantics
      batch <- terminalStructuralBatch context stamp semantics
      _ <-
        mapLeft
          PeerInputPeerPublicationProblem
          ( mkStructuralPeerPublication
              (SortRegistry.registryEntryDescriptor entry)
              definitionOccurrence
              checked
              stamp
              batch
          )
      alreadyMaterialized <- terminalStructuralAlreadyMaterialized stamp batch predecessor.structuralProgress
      if alreadyMaterialized
        then Right (predecessor, TerminalStructuralMaterializationResult TerminalStructuralMaterialized [] [])
        else
          if not (terminalStructuralPrerequisitesSatisfied context batch predecessor)
            then held
            else do
              _ <-
                validateRoutedPublicationAuthorityFacts
                  RetainedHistoryOnly
                  context
                  predecessor.structuralProgress
                  predecessor.controlled
                  ( routedPublicationAuthorityClaim
                      (SortRegistry.registryEntryDescriptor entry)
                      definitionOccurrence
                      checked
                      (structuralPublicationSemanticsSourceProcess semantics)
                      (structuralPublicationSemanticsSourceStrength semantics)
                      ( structuralPublicationSemanticsSourceTopologyPrerequisite
                          semantics
                      )
                      (structuralPublicationSemanticsControlPrerequisite semantics)
                      (Just stamp)
                  )
              referenceSuppression <-
                structuralReferenceRetirementSuppression
                  entry
                  checked
                  predecessor.sortRegistry
                  (structuralOccurrenceStampPredecessor stamp)
                  predecessor.structuralProgress
              resolution <-
                case referenceSuppression of
                  Just suppression
                    | structuralRetirementSuppressionApplies
                        (publicationBatchControlPrerequisite batch)
                        suppression ->
                        resolveRetirementSuppressedStructural
                          context
                          suppression
                          stamp
                          batch
                          checked
                          predecessor.store
                          predecessor.graph
                          predecessor.idGenerator
                          predecessor.structuralProgress
                          predecessor.alignment
                          predecessor.placement
                          predecessor.controlled
                          predecessor.sortRegistry
                  _ ->
                    resolveEffectiveStructural
                      context
                      entry
                      stamp
                      batch
                      checked
                      predecessor.store
                      predecessor.graph
                      predecessor.idGenerator
                      predecessor.structuralProgress
                      predecessor.alignment
                      predecessor.placement
                      predecessor.controlled
                      predecessor.sortRegistry
              case Publication.incomingResolutionDisposition resolution.resolution of
                Publication.DependencyHeld -> held
                Publication.Applied -> install resolution
                Publication.TerminallyIgnored -> install resolution
                Publication.ProtocolRejected ->
                  Left
                    ( PeerInputInvariantProblem
                        (PeerInputRejectedResolutionContradiction publicationIdentifier)
                    )
  where
    held =
      Right
        ( predecessor,
          TerminalStructuralMaterializationResult
            TerminalStructuralMaterializationHeld
            []
            []
        )

    install resolution = do
      (application, wait, wakes) <-
        completeReadyWaits
          resolution.store
          resolution.controlled
          resolution.sortRegistry
          predecessor.application
          predecessor.wait
      let successor =
            predecessor
              { store = resolution.store,
                graph = resolution.graph,
                idGenerator = resolution.idGenerator,
                structuralProgress = resolution.structuralProgress,
                alignment = resolution.alignment,
                placement = resolution.placement,
                controlled = resolution.controlled,
                sortRegistry = resolution.sortRegistry,
                application,
                wait
              }
      Right
        ( successor,
          TerminalStructuralMaterializationResult
            TerminalStructuralMaterialized
            resolution.placementSnapshots
            wakes
        )

-- | Destination-free onboarding replay. The original publication/stamp is
-- authenticated against retained Oracle and topology history, then updates the
-- structural owners atomically. It allocates no stream assignment, application
-- possession, local occurrence, or destination-store observation.
replayHistoricalStructuralOccurrence ::
  PeerInputContext ->
  TerminalStructuralOccurrence ->
  PeerInputState ->
  Either PeerInputProblem (Maybe PeerInputState)
replayHistoricalStructuralOccurrence context occurrence predecessor = do
  validateContext context
  semantics <-
    mapLeft
      PeerInputPeerPublicationProblem
      (decodeStructuralPublicationCanonicalBytes (terminalStructuralOccurrencePayloadBytes occurrence))
  let stamp = terminalStructuralOccurrenceStamp occurrence
      sortId = structuralPublicationSemanticsSortId semantics
      definitionOccurrence = structuralPublicationSemanticsOccurrenceId semantics
  case SortRegistry.lookupEffectiveSort sortId predecessor.sortRegistry of
    Nothing -> pure Nothing
    Just entry
      | SortRegistry.registryEntryOccurrenceId entry /= definitionOccurrence -> pure Nothing
      | otherwise -> do
          checked <- checkedTerminalStructuralPublication entry semantics
          -- This checked batch is used only as a metadata view shared by the
          -- canonical/history validators; its destination is never delivered.
          batch <- terminalStructuralBatch context stamp semantics
          _ <-
            mapLeft
              PeerInputPeerPublicationProblem
              (mkStructuralPeerPublication (SortRegistry.registryEntryDescriptor entry) definitionOccurrence checked stamp batch)
          already <- terminalStructuralAlreadyMaterialized stamp batch predecessor.structuralProgress
          if already
            then pure (Just predecessor)
            else
              if not (terminalStructuralPrerequisitesSatisfied context batch predecessor)
                then pure Nothing
                else do
                  validateRoutedPublicationAuthorityHistory
                    context
                    predecessor.structuralProgress
                    predecessor.controlled
                    ( routedPublicationAuthorityClaim
                        (SortRegistry.registryEntryDescriptor entry)
                        definitionOccurrence
                        checked
                        (structuralPublicationSemanticsSourceProcess semantics)
                        (structuralPublicationSemanticsSourceStrength semantics)
                        (structuralPublicationSemanticsSourceTopologyPrerequisite semantics)
                        (structuralPublicationSemanticsControlPrerequisite semantics)
                        (Just stamp)
                    )
                  carried <-
                    if structuralVersionVectorMembershipGenerationId (structuralOccurrenceStampPredecessor stamp)
                      == structuralVersionVectorMembershipGenerationId (GraphProgress.structuralAppliedVector predecessor.structuralProgress)
                      then pure Nothing
                      else
                        Just
                          <$> mapLeft
                            PeerInputStructuralProgressProblem
                            (GraphProgress.carryDescendantStampedSurvivorOccurrence stamp (GraphProgress.structuralAppliedVector predecessor.structuralProgress) predecessor.structuralProgress)
                  let application =
                        Reconciliation.structuralApplication
                          (structuralOccurrenceStampOccurrence stamp)
                          (structuralOccurrenceStampPredecessor stamp)
                          (sortOccurrence sortId definitionOccurrence)
                          checked
                          (publicationBatchControlPrerequisite batch)
                      views = reconciliationViews context predecessor.graph predecessor.controlled predecessor.sortRegistry
                      applied = GraphProgress.structuralProgressReconciliation predecessor.structuralProgress
                      prepare = case carried of
                        Nothing -> Reconciliation.prepareStructuralReconciliation views application Nothing applied
                        Just evidence -> Reconciliation.prepareCarriedStructuralReconciliation views evidence application Nothing applied
                  case prepare of
                    Left Reconciliation.StructuralPredecessorDotMissing {} -> pure Nothing
                    Left problem -> Left (PeerInputStructuralReconciliationProblem problem)
                    Right (Reconciliation.StructuralHeld _) -> pure Nothing
                    Right (Reconciliation.StructuralReady prepared) -> do
                      controlled <- resolveControlledObservation entry batch checked predecessor.controlled
                      storePatch <-
                        mapLeft
                          PeerInputStructuralStoreProblem
                          ( Store.prepareStructuralStorePatch
                              (structuralOccurrenceCause (structuralOccurrenceStampOccurrence stamp))
                              (Reconciliation.preparedStructuralStorePatch prepared)
                              predecessor.store
                          )
                      placementPatch <-
                        mapLeft
                          PeerInputStructuralPlacementProblem
                          ( Placement.prepareStructuralPlacementPatch
                              (publicationBatchControlPrerequisite batch)
                              (Reconciliation.preparedStructuralPlacementPatch prepared)
                              predecessor.placement
                          )
                      progress <-
                        mapLeft
                          PeerInputStructuralProgressProblem
                          ( case carried of
                              Nothing -> GraphProgress.prepareStructuralApplication stamp (publicationBatchSourceTopologyPrerequisite batch) prepared predecessor.graph predecessor.structuralProgress
                              Just evidence -> GraphProgress.prepareCarriedStructuralApplication evidence (publicationBatchSourceTopologyPrerequisite batch) prepared predecessor.graph predecessor.structuralProgress
                          )
                      debts <-
                        mapLeft
                          PeerInputAlignmentProblem
                          ( Alignment.prepareStructuralDebtRetention
                              (structuralOccurrenceCause (structuralOccurrenceStampOccurrence stamp))
                              (Reconciliation.preparedStructuralDebts prepared)
                              predecessor.alignment
                          )
                      let (graph, structuralProgress, _) = GraphProgress.commitStructuralApplication progress
                          (placement, _) = Placement.commitStructuralPlacementPatch placementPatch
                          (alignment, _) = Alignment.commitStructuralDebtRetention debts
                      pure
                        ( Just
                            PeerInputState
                              { publication = predecessor.publication,
                                peerStream = predecessor.peerStream,
                                graph,
                                structuralProgress,
                                placement,
                                alignment,
                                store = Store.commitStructuralStorePatch storePatch,
                                controlled = controlled.state,
                                idGenerator = predecessor.idGenerator,
                                sortRegistry = predecessor.sortRegistry,
                                application = predecessor.application,
                                wait = predecessor.wait,
                                labelBarrier = predecessor.labelBarrier,
                                disappearance = predecessor.disappearance
                              }
                        )

checkedTerminalStructuralPublication ::
  SortRegistry.RegistryEntry ->
  StructuralPublicationSemantics ->
  Either PeerInputProblem CheckedPublication
checkedTerminalStructuralPublication entry semantics = do
  value <-
    mapLeft
      PeerInputValueProblem
      ( decodeCanonicalValue
          ( canonicalValueByteString
              (structuralPublicationSemanticsCanonicalValue semantics)
          )
      )
  checked <-
    mapLeft
      PeerInputCheckedPublicationProblem
      ( mkCheckedPublication
          (SortRegistry.registryEntryDescriptor entry)
          (structuralPublicationSemanticsPublicationId semantics)
          value
      )
  unless
    ( checkedPublicationCanonicalValue checked
        == structuralPublicationSemanticsCanonicalValue semantics
    )
    ( Left
        ( PeerInputProtocolProblem
            ( PeerInputCanonicalValueMismatch
                (structuralPublicationSemanticsPublicationId semantics)
            )
        )
    )
  Right checked

terminalStructuralBatch ::
  PeerInputContext ->
  StructuralOccurrenceStamp ->
  StructuralPublicationSemantics ->
  Either PeerInputProblem PublicationBatch
terminalStructuralBatch context stamp semantics =
  mapLeft
    PeerInputPublicationBatchProblem
    ( mkPublicationBatch
        (structuralPublicationSemanticsPublicationId semantics)
        (structuralPublicationSemanticsSourceProcess semantics)
        (structuralPublicationSemanticsSortId semantics)
        (structuralPublicationSemanticsOccurrenceId semantics)
        (structuralPublicationSemanticsCanonicalValue semantics)
        strength
        (structuralPublicationSemanticsSourceTopologyPrerequisite semantics)
        (structuralPublicationSemanticsControlPrerequisite semantics)
        (mandatoryDestination :| [])
    )
  where
    strength = structuralPublicationSemanticsSourceStrength semantics
    role = carrierCatalogueRole (structuralOccurrenceStampCarrierRole stamp)
    system = checkedSystemId context.genesis
    local = checkedLocalHeraldEpoch context.genesis
    mandatoryDestination =
      publicationDestination
        (deriveSystemViewDeltaId system local role)
        (deriveSystemViewStoreIncarnationId system local role)
        strength

-- | Applied progress retains the exact authenticated transcript digest and
-- semantic coordinates. Replaying that receipt must not allocate a new live
-- occurrence: its original source may have retired in an installed base.
terminalStructuralAlreadyMaterialized ::
  StructuralOccurrenceStamp ->
  PublicationBatch ->
  GraphProgress.StructuralProgressState ->
  Either PeerInputProblem Bool
terminalStructuralAlreadyMaterialized stamp batch progress =
  case GraphProgress.lookupAppliedStructuralOccurrence identifier progress of
    Nothing -> Right False
    Just retained
      | GraphProgress.appliedStructuralOccurrenceStamp retained == stamp,
        GraphProgress.appliedStructuralOccurrenceSourceTopologyPrerequisite retained == publicationBatchSourceTopologyPrerequisite batch,
        GraphProgress.appliedStructuralOccurrenceCarrierSort retained == sortOccurrence (publicationBatchSortId batch) (publicationBatchOccurrenceId batch),
        GraphProgress.appliedStructuralOccurrenceControlPrerequisite retained == publicationBatchControlPrerequisite batch ->
          Right True
      | otherwise -> Left (PeerInputProtocolProblem (PeerInputTerminalStructuralOccurrenceConflict identifier))
  where
    identifier = structuralOccurrenceStampOccurrence stamp

terminalStructuralPrerequisitesSatisfied ::
  PeerInputContext -> PublicationBatch -> PeerInputState -> Bool
terminalStructuralPrerequisitesSatisfied context batch predecessor =
  sourceTopologySatisfied
    (publicationBatchSourceTopologyPrerequisite batch)
    predecessor.structuralProgress
    && controlSatisfied context batch

-- | Admit one item only through an exact current logical binding.
--
-- A stale binding is an exact state identity and emits no acknowledgement.
-- Preparation failure returns no installable successor.
applyPeerPublication ::
  PeerInputContext ->
  PeerBinding ->
  SequencedItem PeerPublication ->
  PeerInputState ->
  Either PeerInputProblem (PeerInputState, PeerInputResult)
applyPeerPublication context binding item =
  applyPeerLogicalItem context binding (liftPeerPublicationItem item)

liftPeerPublicationItem ::
  SequencedItem PeerPublication -> SequencedItem PeerLogicalPayload
liftPeerPublicationItem item =
  replaceSequencedPayload item (PeerLogicalPublication (sequencedItemPayload item))

replaceSequencedPayload :: SequencedItem source -> target -> SequencedItem target
replaceSequencedPayload item payload =
  sequencedItem
    (sequencedItemDirection item)
    (sequencedItemSequence item)
    (sequencedItemDigest item)
    payload

applyPeerLogicalItem ::
  PeerInputContext ->
  PeerBinding ->
  SequencedItem PeerLogicalPayload ->
  PeerInputState ->
  Either PeerInputProblem (PeerInputState, PeerInputResult)
applyPeerLogicalItem context binding item predecessor = do
  validateContext context
  mapLeft
    PeerInputPlacementProblem
    (Placement.validatePlacementState predecessor.placement)
  -- Structural progress is an opaque owner maintained by checked transitions.
  -- Audit its complete retained history at initialization and test/debug
  -- boundaries; ingress checks the new item and its exact prerequisites below.
  let peer = peerBindingRemoteHeraldEpoch binding
  case Discovery.currentPeerBinding peer context.discovery of
    Just current
      | current == binding -> applyCurrentBinding context binding item predecessor
    _ ->
      Right
        ( predecessor,
          PeerInputResult PeerInputStaleBinding [] [] [] [] []
        )

applyCurrentBinding ::
  PeerInputContext ->
  PeerBinding ->
  SequencedItem PeerLogicalPayload ->
  PeerInputState ->
  Either PeerInputProblem (PeerInputState, PeerInputResult)
applyCurrentBinding context binding item predecessor = do
  validateBindingDirection binding item predecessor.peerStream
  -- Completion ends this assignment's semantic admission lifetime. Its compact
  -- stream receipt classifies old traffic before current publication policy:
  -- assignment links have already been released, and a later policy change
  -- must not turn an exact old replay into a fresh semantic reassignment.
  -- PeerStream retains a future item before it becomes contiguous. Reject all
  -- immutable claims that can already be checked so malformed work cannot
  -- reserve its sequence and poison a later corrected retry.
  unless
    (PeerStream.incomingSequenceCompleted (sequencedItemDirection item, sequencedItemSequence item) predecessor.peerStream)
    (validatePreAdmission context item predecessor)
  candidate <-
    mapLeft
      PeerInputStreamProblem
      (PeerStream.prepareReceiveCandidate item predecessor.peerStream)
  (receiveCandidate, plan) <-
    planContiguousPrefix
      context
      candidate
      ( incomingPlan
          predecessor.publication
          predecessor.store
          predecessor.graph
          predecessor.idGenerator
          predecessor.structuralProgress
          predecessor.alignment
          predecessor.placement
          predecessor.controlled
          predecessor.sortRegistry
          predecessor.labelBarrier
          predecessor.disappearance
      )
  preparedReceive <-
    mapLeft
      PeerInputStreamProblem
      (PeerStream.finalizeReceive plan.progress receiveCandidate)
  let (afterReceive, receiveResult) = PeerStream.commitReceive preparedReceive
  afterCompletions <- completeAssignments plan.completions afterReceive
  let affectedDirections =
        Set.insert (sequencedItemDirection item) (Map.keysSet plan.completions)
      acknowledgementDirections =
        affectedDirections `Set.difference` Map.keysSet plan.deferredRejections
  acknowledgements <-
    acknowledgementsFor
      acknowledgementDirections
      predecessor.peerStream
      afterCompletions
  (application, wait, wakes) <-
    completeReadyWaits
      plan.store
      plan.controlled
      plan.sortRegistry
      predecessor.application
      predecessor.wait
  let successor =
        PeerInputState
          { publication = plan.publication,
            peerStream = afterCompletions,
            store = plan.store,
            graph = plan.graph,
            idGenerator = plan.idGenerator,
            structuralProgress = plan.structuralProgress,
            alignment = plan.alignment,
            placement = plan.placement,
            controlled = plan.controlled,
            sortRegistry = plan.sortRegistry,
            application,
            wait,
            labelBarrier = plan.labelBarrier,
            disappearance = plan.disappearance
          }
      result =
        PeerInputResult
          { disposition = PeerInputReceived (receiveResultDisposition receiveResult),
            acknowledgements,
            publications = reverse plan.reversePublications,
            placementSnapshots = reverse plan.reversePlacementSnapshots,
            wakes,
            deferredRejections = Map.elems plan.deferredRejections
          }
  (released, markerResult) <- releasePendingDisappearanceProbeMarkers context successor
  publication <-
    mapLeft
      PeerInputPublicationProblem
      ( Publication.retireIncomingAssignmentReceipts
          (\key -> PeerStream.incomingSequenceCompleted key released.peerStream)
          released.publication
      )
  Right
    ( replaceCompletedPublicationOwner publication released,
      PeerInputResult
        result.disposition
        (result.acknowledgements <> peerDependencyAcknowledgements markerResult)
        result.publications
        result.placementSnapshots
        result.wakes
        (result.deferredRejections <> peerDependencyDeferredRejections markerResult)
    )

-- Constructor-qualified reconstruction keeps this owner boundary explicit;
-- IncomingPlan has a different publication field with a different lifetime.
replaceCompletedPublicationOwner :: Publication.State -> PeerInputState -> PeerInputState
replaceCompletedPublicationOwner
  publication
  PeerInputState
    { peerStream,
      store,
      graph,
      idGenerator,
      structuralProgress,
      alignment,
      placement,
      controlled,
      sortRegistry,
      application,
      wait,
      labelBarrier,
      disappearance
    } =
    PeerInputState
      { publication,
        peerStream,
        store,
        graph,
        idGenerator,
        structuralProgress,
        alignment,
        placement,
        controlled,
        sortRegistry,
        application,
        wait,
        labelBarrier,
        disappearance
      }

validatePreAdmission ::
  PeerInputContext ->
  SequencedItem PeerLogicalPayload ->
  PeerInputState ->
  Either PeerInputProblem ()
validatePreAdmission context item predecessor =
  case sequencedItemPayload item of
    PeerLogicalPublication publication ->
      validatePublicationPreAdmission
        context
        (replaceSequencedPayload item publication)
        predecessor
    PeerLogicalRouteCutover marker -> do
      let direction = sequencedItemDirection item
          expectedSource = streamDirectionSource direction
          expectedPredecessor = streamDirectionDestination direction
      unless
        (alignmentRouteCutoverMarkerSourceHerald marker == expectedSource)
        ( Left
            ( PeerInputProtocolProblem
                ( PeerInputRouteCutoverSourceMismatch
                    expectedSource
                    (alignmentRouteCutoverMarkerSourceHerald marker)
                )
            )
        )
      unless
        (alignmentRouteCutoverMarkerPredecessorHerald marker == expectedPredecessor)
        ( Left
            ( PeerInputProtocolProblem
                ( PeerInputRouteCutoverPredecessorMismatch
                    expectedPredecessor
                    (alignmentRouteCutoverMarkerPredecessorHerald marker)
                )
            )
        )
      unless
        (sequencedItemDigest item == peerLogicalPayloadDigest (PeerLogicalRouteCutover marker))
        (Left (PeerInputProtocolProblem PeerInputRouteCutoverDigestMismatch))
    PeerLogicalDisappearanceProbeMarker marker -> do
      unless
        ( DisappearanceProtocol.disappearancePublicationMarkerDirection marker == sequencedItemDirection item
            && DisappearanceProtocol.disappearancePublicationMarkerSequence marker == sequencedItemSequence item
        )
        (Left (PeerInputProtocolProblem PeerInputDisappearanceMarkerCoordinateMismatch))
      unless
        (DisappearanceProtocol.disappearancePublicationMarkerDigest marker == sequencedItemDigest item)
        (Left (PeerInputProtocolProblem PeerInputDisappearanceMarkerDigestMismatch))

validatePublicationPreAdmission ::
  PeerInputContext ->
  SequencedItem PeerPublication ->
  PeerInputState ->
  Either PeerInputProblem ()
validatePublicationPreAdmission context item predecessor = do
  let direction = sequencedItemDirection item
      sequenceNumber = sequencedItemSequence item
      digest = sequencedItemDigest item
      peerPublication = sequencedItemPayload item
      batch = peerPublicationBatch peerPublication
  validatePublicationSource direction peerPublication
  candidate <-
    mapLeft
      PeerInputPublicationProblem
      ( Publication.prepareIncomingPublicationCandidate
          ( OracleProjection.oracleViewCurrentHeraldMembershipId
              (OracleProjection.oracleView context.oracle)
          )
          direction
          sequenceNumber
          digest
          peerPublication
          predecessor.publication
      )
  validateCandidateStructuralDestination
    context
    predecessor.placement
    peerPublication
    candidate
  unless
    ( Publication.preparedIncomingPublicationClassification candidate
        == Publication.IncomingPublicationAssignmentDuplicate
    )
    (validateSemanticClaims direction peerPublication batch)
  where
    -- An exact retained assignment has already crossed every semantic boundary.
    -- Replaying it must remain transport-idempotent even when later control
    -- makes the original peer claim rejectable. Fresh assignments still take
    -- the complete checked-source validation path below. Their authority is
    -- frozen at source acceptance: a later End/deletion prevents new local
    -- operations but does not recall an already accepted publication.
    validateSemanticClaims direction peerPublication batch = do
      -- When source authority is already decidable, reject malformed ownership
      -- claims before interpreting the claimed effective definition. This keeps
      -- a source-root sort contradiction at its immutable wire boundary and
      -- ensures even a future stream item cannot reserve state under an unknown
      -- owner.
      if sourceTopologySatisfied
        (publicationBatchSourceTopologyPrerequisite batch)
        predecessor.structuralProgress
        && controlSatisfied context batch
        then do
          _ <-
            validateBatchAuthority
              RetainedHistoryOnly
              context
              predecessor.structuralProgress
              predecessor.controlled
              direction
              peerPublication
          Right ()
        else Right ()
      sortResolution <-
        resolveBatchSortEntry
          (controlSatisfied context batch)
          batch
          predecessor.sortRegistry
      let authenticate entry = do
            checked <- checkedIncomingPublication entry batch
            _ <-
              mapLeft
                PeerInputPeerPublicationProblem
                ( authenticatePeerPublication
                    (SortRegistry.registryEntryDescriptor entry)
                    (SortRegistry.registryEntryOccurrenceId entry)
                    checked
                    peerPublication
                )
            Right ()
      case sortResolution of
        BatchSortUnavailable -> Right ()
        BatchSortEffective entry -> authenticate entry
        BatchSortRetired entry -> authenticate entry

data IncomingPlan = IncomingPlan
  { publication :: Publication.State,
    store :: Store.State,
    graph :: Graph.State,
    idGenerator :: IdGenerator.State,
    structuralProgress :: GraphProgress.StructuralProgressState,
    alignment :: Alignment.State,
    placement :: Placement.State,
    controlled :: Controlled.State,
    sortRegistry :: SortRegistry.State,
    labelBarrier :: LabelBarrier.State,
    disappearance :: Disappearance.State,
    progress :: Map StreamSequence ReceiveProgress,
    completions :: Map StreamDirection (Set StreamSequence),
    inducedDependencies :: Set Publication.PublicationDependency,
    deferredRejections :: Map StreamDirection DeferredPeerRejection,
    reversePublications :: [PublicationId],
    reversePlacementSnapshots :: [PlacementSnapshot]
  }

incomingPlan ::
  Publication.State ->
  Store.State ->
  Graph.State ->
  IdGenerator.State ->
  GraphProgress.StructuralProgressState ->
  Alignment.State ->
  Placement.State ->
  Controlled.State ->
  SortRegistry.State ->
  LabelBarrier.State ->
  Disappearance.State ->
  IncomingPlan
incomingPlan publication store graph idGenerator structuralProgress alignment placement controlled sortRegistry labelBarrier disappearance =
  IncomingPlan
    publication
    store
    graph
    idGenerator
    structuralProgress
    alignment
    placement
    controlled
    sortRegistry
    labelBarrier
    disappearance
    Map.empty
    Map.empty
    Set.empty
    Map.empty
    []
    []

-- | Plan the candidate's contiguous prefix in order. A future publication may
-- have been gap-buffered while one of its claims was still undecidable, then
-- become rejectable only after intervening control or sort progress. Such an
-- item belongs to the old binding's buffered suffix: remove that exact slot,
-- attribute the protocol rejection to its source direction, and leave later
-- gaps retained so a replacement binding can fill the reopened sequence. A
-- malformed newly supplied head is rejected earlier by pre-admission or by the
-- ordinary planning failure path.
planContiguousPrefix ::
  PeerInputContext ->
  PeerStream.PreparedReceiveCandidate PeerLogicalPayload ->
  IncomingPlan ->
  Either
    PeerInputProblem
    (PeerStream.PreparedReceiveCandidate PeerLogicalPayload, IncomingPlan)
planContiguousPrefix context candidate = go (PeerStream.preparedContiguousItems candidate)
  where
    go [] plan = Right (candidate, plan)
    go (item : remaining) plan =
      case planContiguousItem context plan item of
        Right successor -> go remaining successor
        Left problem
          | peerInputProblemDisposition problem == RejectCurrentPeerBinding,
            Right trimmed <-
              PeerStream.discardPreviouslyRetainedContiguousAt
                (sequencedItemSequence item)
                candidate ->
              let direction = sequencedItemDirection item
                  rejection =
                    DeferredPeerRejection
                      direction
                      (publicationIdentifier (sequencedItemPayload item))
                  withRejection :: IncomingPlan
                  withRejection =
                    IncomingPlan
                      plan.publication
                      plan.store
                      plan.graph
                      plan.idGenerator
                      plan.structuralProgress
                      plan.alignment
                      plan.placement
                      plan.controlled
                      plan.sortRegistry
                      plan.labelBarrier
                      plan.disappearance
                      plan.progress
                      plan.completions
                      plan.inducedDependencies
                      (Map.insert direction rejection plan.deferredRejections)
                      plan.reversePublications
                      plan.reversePlacementSnapshots
               in Right
                    (trimmed, withRejection)
          | otherwise -> Left problem

    publicationIdentifier = \case
      PeerLogicalPublication peerPublication ->
        Just (publicationBatchId (peerPublicationBatch peerPublication))
      PeerLogicalRouteCutover {} -> Nothing
      PeerLogicalDisappearanceProbeMarker {} -> Nothing

planContiguousItem ::
  PeerInputContext ->
  IncomingPlan ->
  SequencedItem PeerLogicalPayload ->
  Either PeerInputProblem IncomingPlan
planContiguousItem context plan item =
  case sequencedItemPayload item of
    PeerLogicalPublication publication ->
      planContiguousPublication
        context
        plan
        (replaceSequencedPayload item publication)
    PeerLogicalDisappearanceProbeMarker marker -> do
      prepared <-
        mapLeft
          PeerInputDisappearanceProblem
          (Disappearance.preparePublicationMarkerReceipt marker plan.disappearance)
      let (disappearance, _) = Disappearance.commitDisappearanceTransition prepared
      Right plan {disappearance, progress = Map.insert (sequencedItemSequence item) ReceivedPending plan.progress}
    PeerLogicalRouteCutover _marker ->
      Right
        plan
          { progress =
              Map.insert
                (sequencedItemSequence item)
                ReceivedPending
                plan.progress
          }

planContiguousPublication ::
  PeerInputContext ->
  IncomingPlan ->
  SequencedItem PeerPublication ->
  Either PeerInputProblem IncomingPlan
planContiguousPublication context plan item = do
  let direction = sequencedItemDirection item
      sequenceNumber = sequencedItemSequence item
      digest = sequencedItemDigest item
      peerPublication = sequencedItemPayload item
      batch = peerPublicationBatch peerPublication
  validatePublicationSource direction peerPublication
  candidate <-
    mapLeft
      PeerInputPublicationProblem
      ( Publication.prepareIncomingPublicationCandidate
          ( OracleProjection.oracleViewCurrentHeraldMembershipId
              (OracleProjection.oracleView context.oracle)
          )
          direction
          sequenceNumber
          digest
          peerPublication
          plan.publication
      )
  validateCandidateStructuralDestination
    context
    plan.placement
    peerPublication
    candidate
  progressed <- case Publication.preparedIncomingPublicationClassification candidate of
    Publication.IncomingPublicationFirstSeen -> do
      let gateProbes =
            DisappearanceGate.peerPublicationGateProbes
              (streamDirectionSource direction)
              sequenceNumber
              peerPublication
              plan.disappearance
      disappearance <- retainIncomingGateInvalidations gateProbes peerPublication plan.disappearance
      resolution <-
        resolveFirstSeen
          context
          direction
          peerPublication
          (Set.map Publication.DisappearanceProbeDependency gateProbes)
          plan.store
          plan.graph
          plan.idGenerator
          plan.structuralProgress
          plan.alignment
          plan.placement
          plan.controlled
          plan.sortRegistry
      prepared <-
        mapLeft
          PeerInputPublicationProblem
          (Publication.finalizeIncomingPublication (Just resolution.resolution) candidate)
      let (publication, _) = Publication.commitIncomingPublication prepared
          withRecord =
            plan
              { publication,
                disappearance,
                store = resolution.store,
                graph = resolution.graph,
                idGenerator = resolution.idGenerator,
                structuralProgress = resolution.structuralProgress,
                alignment = resolution.alignment,
                placement = resolution.placement,
                controlled = resolution.controlled,
                sortRegistry = resolution.sortRegistry,
                inducedDependencies =
                  plan.inducedDependencies `Set.union` resolution.inducedDependencies,
                reversePublications = publicationBatchId batch : plan.reversePublications,
                reversePlacementSnapshots =
                  reverse resolution.placementSnapshots
                    <> plan.reversePlacementSnapshots
              }
      releaseFixedPoint Nothing context withRecord.inducedDependencies withRecord
    Publication.IncomingPublicationSemanticDuplicate ->
      retainDuplicate candidate plan
    Publication.IncomingPublicationAssignmentDuplicate ->
      retainDuplicate candidate plan
  record <-
    maybe
      (Left (PeerInputInvariantProblem (PeerInputDependencyWaiterMissing (publicationBatchId batch))))
      Right
      (Publication.lookupIncomingPublication (publicationBatchId batch) progressed.publication)
  Right
    progressed
      { progress =
          Map.insert
            sequenceNumber
            (progressForDisposition (Publication.incomingPublicationDisposition record))
            progressed.progress
      }

retainDuplicate ::
  Publication.PreparedIncomingPublicationCandidate ->
  IncomingPlan ->
  Either PeerInputProblem IncomingPlan
retainDuplicate candidate plan = do
  prepared <-
    mapLeft
      PeerInputPublicationProblem
      (Publication.finalizeIncomingPublication Nothing candidate)
  let (publication, record) = Publication.commitIncomingPublication prepared
  Right
    plan
      { publication,
        reversePublications =
          publicationBatchId (Publication.incomingPublicationBatch record)
            : plan.reversePublications
      }

data BatchResolution = BatchResolution
  { store :: Store.State,
    graph :: Graph.State,
    idGenerator :: IdGenerator.State,
    structuralProgress :: GraphProgress.StructuralProgressState,
    alignment :: Alignment.State,
    placement :: Placement.State,
    controlled :: Controlled.State,
    sortRegistry :: SortRegistry.State,
    resolution :: Publication.IncomingPublicationResolution,
    inducedDependencies :: Set Publication.PublicationDependency,
    placementSnapshots :: [PlacementSnapshot]
  }

-- | Resolve the descriptor occurrence named by one peer batch without
-- confusing an exact retired occurrence with an unknown definition.  A
-- non-historical coordinate contradicting the effective entry is rejected only
-- after its claimed control prerequisite is locally decidable; otherwise it
-- may name a successor occurrence from a Resolve that has not arrived yet.
data BatchSortResolution
  = BatchSortUnavailable
  | BatchSortEffective SortRegistry.RegistryEntry
  | BatchSortRetired SortRegistry.RegistryEntry

resolveBatchSortEntry ::
  Bool ->
  PublicationBatch ->
  SortRegistry.State ->
  Either PeerInputProblem BatchSortResolution
resolveBatchSortEntry mismatchDecidable batch registry =
  resolveSortEntry
    mismatchDecidable
    (publicationBatchSortId batch)
    (publicationBatchOccurrenceId batch)
    registry

resolveSortEntry ::
  Bool ->
  SortId ->
  SortDefinitionOccurrenceId ->
  SortRegistry.State ->
  Either PeerInputProblem BatchSortResolution
resolveSortEntry mismatchDecidable sortId observed registry =
  case SortRegistry.lookupEffectiveSort sortId registry of
    Just effective
      | SortRegistry.registryEntryOccurrenceId effective == observed ->
          Right (BatchSortEffective effective)
      | Just retirement <- retainedRetirement,
        Just historical <- SortRegistry.regularSortRetirementEntry retirement ->
          Right (BatchSortRetired historical)
      | Just retirement <- retainedRetirement,
        mismatchDecidable ->
          occurrenceMismatch (SortRegistry.regularSortRetirementSuccessorOccurrenceId retirement)
      | mismatchDecidable ->
          occurrenceMismatch (SortRegistry.registryEntryOccurrenceId effective)
      | otherwise -> Right BatchSortUnavailable
    Nothing
      | Just retirement <- retainedRetirement,
        Just historical <- SortRegistry.regularSortRetirementEntry retirement ->
          Right (BatchSortRetired historical)
      | Just retirement <- retainedRetirement,
        mismatchDecidable ->
          occurrenceMismatch (SortRegistry.regularSortRetirementSuccessorOccurrenceId retirement)
      | Just retirement <- latestRetirement,
        mismatchDecidable,
        observed
          /= SortRegistry.regularSortRetirementSuccessorOccurrenceId retirement ->
          occurrenceMismatch
            (SortRegistry.regularSortRetirementSuccessorOccurrenceId retirement)
      | otherwise -> Right BatchSortUnavailable
  where
    retainedRetirement =
      SortRegistry.lookupRegularSortRetirement sortId observed registry
    latestRetirement =
      SortRegistry.latestRegularSortRetirement sortId registry
    occurrenceMismatch expected =
      Left
        ( PeerInputProtocolProblem
            (PeerInputEffectiveOccurrenceMismatch sortId expected observed)
        )

retainIncomingGateInvalidations :: Set DisappearanceProbeId -> PeerPublication -> Disappearance.State -> Either PeerInputProblem Disappearance.State
retainIncomingGateInvalidations probes publication state =
  mapLeft
    PeerInputDisappearanceProblem
    ( DisappearanceGate.retainGatedPublicationInvalidations
        [ gate
        | gate <- DisappearanceGate.peerPublicationGates publication state,
          Disappearance.matchingWriteGateProbe gate `Set.member` probes
        ]
        (canonicalValueByteString (publicationBatchCanonicalValue (peerPublicationBatch publication)))
        state
    )

resolveFirstSeen ::
  PeerInputContext ->
  StreamDirection ->
  PeerPublication ->
  Set Publication.PublicationDependency ->
  Store.State ->
  Graph.State ->
  IdGenerator.State ->
  GraphProgress.StructuralProgressState ->
  Alignment.State ->
  Placement.State ->
  Controlled.State ->
  SortRegistry.State ->
  Either PeerInputProblem BatchResolution
resolveFirstSeen
  context
  direction
  peerPublication
  gateDependencies
  store
  graph
  idGenerator
  structuralProgress
  alignment
  placement
  controlled
  sortRegistry = do
    sortResolution <-
      resolveBatchSortEntry
        (controlSatisfied context batch)
        batch
        sortRegistry
    resolve sortResolution
    where
      resolve sortResolution = case (dependencies sortResolution, sortResolution) of
        (held, _) | not (Set.null held) -> do
          resolution <-
            makeResolution
              held
              (pendingDestinationOutcomes batch)
          Right
            ( BatchResolution
                store
                graph
                idGenerator
                structuralProgress
                alignment
                placement
                controlled
                sortRegistry
                resolution
                Set.empty
                []
            )
        (_, BatchSortEffective entry) ->
          resolveEffectivePeerPublication
            RetainedHistoryOnly
            (OracleProjection.oracleViewCurrentHeraldMembershipId (OracleProjection.oracleView context.oracle))
            context
            direction
            entry
            peerPublication
            store
            graph
            idGenerator
            structuralProgress
            alignment
            placement
            controlled
            sortRegistry
        (_, BatchSortRetired entry) ->
          resolveRetiredPeerPublication
            RetainedHistoryOnly
            context
            direction
            entry
            peerPublication
            store
            graph
            idGenerator
            structuralProgress
            alignment
            placement
            controlled
            sortRegistry
        (_, BatchSortUnavailable) ->
          Left
            ( PeerInputInvariantProblem
                (PeerInputReadySortMissing (publicationBatchId batch) (publicationBatchSortId batch))
            )
      batch = peerPublicationBatch peerPublication
      dependencies sortResolution =
        gateDependencies
          `Set.union` Set.fromList
            (sortDependency sortResolution <> controlDependency <> topologyDependency)
      sortDependency = \case
        BatchSortUnavailable ->
          [ Publication.EffectiveSortDependency
              (publicationBatchSortId batch)
              (publicationBatchOccurrenceId batch)
          ]
        BatchSortEffective {} -> []
        BatchSortRetired {} -> []
      controlDependency
        | controlSatisfied context batch = []
        | otherwise =
            [Publication.ControlIndexDependency (publicationBatchControlPrerequisite batch)]
      topologyDependency
        | sourceTopologySatisfied
            (publicationBatchSourceTopologyPrerequisite batch)
            structuralProgress =
            []
        | otherwise =
            [ Publication.SourceTopologyDependency
                (publicationBatchSourceTopologyPrerequisite batch)
            ]

resolveEffectivePeerPublication ::
  AuthorityApplicability ->
  HeraldMembershipGenerationId ->
  PeerInputContext ->
  StreamDirection ->
  SortRegistry.RegistryEntry ->
  PeerPublication ->
  Store.State ->
  Graph.State ->
  IdGenerator.State ->
  GraphProgress.StructuralProgressState ->
  Alignment.State ->
  Placement.State ->
  Controlled.State ->
  SortRegistry.State ->
  Either PeerInputProblem BatchResolution
resolveEffectivePeerPublication
  applicability
  admissionGeneration
  context
  direction
  entry
  peerPublication
  store
  graph
  idGenerator
  structuralProgress
  alignment
  placement
  controlled
  sortRegistry = do
    validatePublicationSource direction peerPublication
    validateEffectiveOccurrence entry batch
    checked <- checkedIncomingPublication entry batch
    _ <-
      mapLeft
        PeerInputPeerPublicationProblem
        ( authenticatePeerPublication
            (SortRegistry.registryEntryDescriptor entry)
            (SortRegistry.registryEntryOccurrenceId entry)
            checked
            peerPublication
        )
    _ <-
      validateRoutedPublicationAuthorityFacts
        applicability
        context
        structuralProgress
        controlled
        ( routedPublicationAuthorityClaim
            (SortRegistry.registryEntryDescriptor entry)
            (SortRegistry.registryEntryOccurrenceId entry)
            checked
            (publicationBatchSourceProcess batch)
            (publicationBatchSourceStrength batch)
            (publicationBatchSourceTopologyPrerequisite batch)
            (publicationBatchControlPrerequisite batch)
            (peerPublicationStructuralStamp peerPublication)
        )
    case peerPublicationStructuralStamp peerPublication of
      Nothing ->
        resolveEffectiveOrdinary
          context
          entry
          batch
          store
          graph
          idGenerator
          structuralProgress
          alignment
          placement
          controlled
          sortRegistry
      Just stamp -> do
        referenceSuppression <-
          structuralReferenceRetirementSuppression
            entry
            checked
            sortRegistry
            (structuralOccurrenceStampPredecessor stamp)
            structuralProgress
        case successorStructuralBaseHold admissionGeneration stamp context structuralProgress of
          Just dependency -> do
            resolution <-
              makeResolution
                (Set.singleton dependency)
                (pendingDestinationOutcomes batch)
            Right
              ( BatchResolution
                  store
                  graph
                  idGenerator
                  structuralProgress
                  alignment
                  placement
                  controlled
                  sortRegistry
                  resolution
                  Set.empty
                  []
              )
          Nothing
            | Just suppression <- referenceSuppression,
              structuralRetirementSuppressionApplies
                (publicationBatchControlPrerequisite batch)
                suppression ->
                resolveRetirementSuppressedStructural
                  context
                  suppression
                  stamp
                  batch
                  checked
                  store
                  graph
                  idGenerator
                  structuralProgress
                  alignment
                  placement
                  controlled
                  sortRegistry
            | otherwise ->
                resolveEffectiveStructural
                  context
                  entry
                  stamp
                  batch
                  checked
                  store
                  graph
                  idGenerator
                  structuralProgress
                  alignment
                  placement
                  controlled
                  sortRegistry
    where
      batch = peerPublicationBatch peerPublication

-- | Resolve the minimum control epoch of SortIds embedded by structural root
-- carriers without choosing a current occurrence yet. The payload deliberately
-- carries only the stable public SortId; its batch prerequisite proves which
-- retirement epoch the sender observed.
structuralReferenceRetirementSuppression ::
  SortRegistry.RegistryEntry ->
  CheckedPublication ->
  SortRegistry.State ->
  StructuralVersionVector ->
  GraphProgress.StructuralProgressState ->
  Either
    PeerInputProblem
    (Maybe Reconciliation.StructuralRetirementSuppression)
structuralReferenceRetirementSuppression entry checked registry predecessor structuralProgress = do
  references <-
    mapLeft
      ( PeerInputProtocolProblem
          . PeerInputStructuralCarrierReferenceProblem
            (checkedPublicationId checked)
      )
      ( structuralCarrierSortReferences
          (SortRegistry.registryEntryDescriptor entry)
          (checkedPublicationValue checked)
      )
  inheritedSuppression <-
    mapLeft
      PeerInputStructuralReconciliationProblem
      ( Reconciliation.structuralRetirementSuppressionControlPrerequisite
          predecessor
          checked
          (GraphProgress.structuralProgressReconciliation structuralProgress)
      )
  Right
    ( foldl
        laterSuppression
        inheritedSuppression
        [ Reconciliation.DirectRegularSortRetirementSuppression
            referencedSort
            (SortRegistry.regularSortRetirementResolveIndex retirement)
        | referencedSort <- references,
          Just retirement <- [SortRegistry.latestRegularSortRetirement referencedSort registry]
        ]
    )
  where
    laterSuppression Nothing candidate = Just candidate
    laterSuppression incumbent@(Just retained) candidate
      | suppressionKey candidate > suppressionKey retained = Just candidate
      | otherwise = incumbent
    suppressionKey suppression =
      ( Reconciliation.structuralRetirementSuppressionControlIndex suppression,
        suppression
      )

structuralRetirementSuppressionApplies ::
  ControlIndex ->
  Reconciliation.StructuralRetirementSuppression ->
  Bool
structuralRetirementSuppressionApplies prerequisite = \case
  Reconciliation.DirectRegularSortRetirementSuppression _ floorIndex ->
    prerequisite < floorIndex
  Reconciliation.InheritedEndpointRetirementSuppression {} -> True

-- | Wait for execution admitted or stamped after the applied Graph generation.
-- A sender's stamp can precede the receiver's Oracle watch; both immutable
-- coordinates are checked against the now-admitted history. Older payloads
-- remain available to build terminal closure.
successorStructuralBaseHold ::
  HeraldMembershipGenerationId ->
  StructuralOccurrenceStamp ->
  PeerInputContext ->
  GraphProgress.StructuralProgressState ->
  Maybe Publication.PublicationDependency
successorStructuralBaseHold admission stamp context progress
  | any requiresBase [admission, structuralVersionVectorMembershipGenerationId (structuralOccurrenceStampPredecessor stamp)] =
      Just (Publication.SuccessorStructuralBaseDependency current)
  | otherwise = Nothing
  where
    requiresBase origin =
      case (OracleProjection.oracleViewHeraldMembershipLineage graphGeneration origin view, OracleProjection.oracleViewHeraldMembershipLineage origin current view) of
        (Just _, Just _) -> graphGeneration /= origin
        _ -> False
    view = OracleProjection.oracleView context.oracle
    current = OracleProjection.oracleViewCurrentHeraldMembershipId view
    graphGeneration = GraphProgress.structuralProgressMembershipGenerationId progress

resolveEffectiveOrdinary ::
  PeerInputContext ->
  SortRegistry.RegistryEntry ->
  PublicationBatch ->
  Store.State ->
  Graph.State ->
  IdGenerator.State ->
  GraphProgress.StructuralProgressState ->
  Alignment.State ->
  Placement.State ->
  Controlled.State ->
  SortRegistry.State ->
  Either PeerInputProblem BatchResolution
resolveEffectiveOrdinary
  context
  entry
  batch
  store
  graph
  idGenerator
  structuralProgress
  alignment
  placement
  controlled
  sortRegistry = do
    validateEffectiveOccurrence entry batch
    checked <- checkedIncomingPublication entry batch
    controlledResolution <-
      resolveControlledObservation entry batch checked controlled
    applyEffectiveBatch controlledResolution checked
    where
      applyEffectiveBatch controlledResolution checked = do
        validateCurrentDestinations batch store
        (successorStore, resolutionOutcomes) <-
          if controlledResolution.currentSuppressed
            then
              Right
                ( store,
                  Map.fromList
                    [ (destination, Publication.DestinationTerminallyIgnored)
                    | destination <- NonEmpty.toList (publicationBatchDestinations batch)
                    ]
                )
            else do
              preparedStore <-
                mapLeft
                  PeerInputStoreProblem
                  ( Store.preparePeerStoreApplication
                      (storeObservationEnvelope Nothing batch)
                      (storeDestinations batch)
                      checked
                      store
                  )
              let (appliedStore, outcomes) = Store.commitPeerStoreApplication preparedStore
              Right
                ( appliedStore,
                  Map.fromList
                    ( zip
                        (NonEmpty.toList (publicationBatchDestinations batch))
                        (fmap destinationOutcome (NonEmpty.toList outcomes))
                    )
                )
        let
          anyApplied =
            any (== Publication.DestinationApplied) (Map.elems resolutionOutcomes)
          successorGraph = case controlledResolution.object of
            Just object
              | anyApplied,
                checkedPublicationLifecycle checked == Current,
                descriptorStructuralCarrierRole descriptor == Just NeutralVertexCarrier ->
                  Graph.commitNeutralVertexInsertion
                    (Graph.prepareNeutralVertexInsertion object graph)
            _ -> graph
        resolution <- makeResolution Set.empty resolutionOutcomes
        (successorRegistry, induced) <-
          induceIncomingDefinition
            context
            batch
            checked
            resolutionOutcomes
            successorStore
            sortRegistry
        Right
          ( BatchResolution
              successorStore
              successorGraph
              idGenerator
              structuralProgress
              alignment
              placement
              controlledResolution.state
              successorRegistry
              resolution
              induced
              []
          )

      descriptor = canonicalCheckedDescriptor (SortRegistry.registryEntryDescriptor entry)

-- | Authenticate an exact retired sort occurrence and settle it only through
-- Store's immutable retirement suppression.  In particular, stale ordinary
-- traffic must never recreate Controlled, Graph, registry, placement, or
-- identity state even when the retired descriptor itself was controlled.
resolveRetiredPeerPublication ::
  AuthorityApplicability ->
  PeerInputContext ->
  StreamDirection ->
  SortRegistry.RegistryEntry ->
  PeerPublication ->
  Store.State ->
  Graph.State ->
  IdGenerator.State ->
  GraphProgress.StructuralProgressState ->
  Alignment.State ->
  Placement.State ->
  Controlled.State ->
  SortRegistry.State ->
  Either PeerInputProblem BatchResolution
resolveRetiredPeerPublication
  applicability
  context
  direction
  entry
  peerPublication
  store
  graph
  idGenerator
  structuralProgress
  alignment
  placement
  controlled
  sortRegistry = do
    validatePublicationSource direction peerPublication
    validateEffectiveOccurrence entry batch
    checked <- checkedIncomingPublication entry batch
    _ <-
      mapLeft
        PeerInputPeerPublicationProblem
        ( authenticatePeerPublication
            (SortRegistry.registryEntryDescriptor entry)
            (SortRegistry.registryEntryOccurrenceId entry)
            checked
            peerPublication
        )
    _ <-
      validateRoutedPublicationAuthorityFacts
        applicability
        context
        structuralProgress
        controlled
        ( routedPublicationAuthorityClaim
            (SortRegistry.registryEntryDescriptor entry)
            (SortRegistry.registryEntryOccurrenceId entry)
            checked
            (publicationBatchSourceProcess batch)
            (publicationBatchSourceStrength batch)
            (publicationBatchSourceTopologyPrerequisite batch)
            (publicationBatchControlPrerequisite batch)
            (peerPublicationStructuralStamp peerPublication)
        )
    validateCurrentDestinations batch store
    preparedStore <-
      mapLeft
        PeerInputStoreProblem
        ( Store.preparePeerStoreApplication
            (storeObservationEnvelope Nothing batch)
            (storeDestinations batch)
            checked
            store
        )
    let (successorStore, outcomes) = Store.commitPeerStoreApplication preparedStore
        resolutionOutcomes =
          Map.fromList
            ( zip
                (NonEmpty.toList (publicationBatchDestinations batch))
                (fmap destinationOutcome (NonEmpty.toList outcomes))
            )
        terminal =
          successorStore == store
            && all
              (== Publication.DestinationTerminallyIgnored)
              (Map.elems resolutionOutcomes)
    unless
      terminal
      ( Left
          ( PeerInputInvariantProblem
              (PeerInputRetiredOccurrenceStoreApplied (publicationBatchId batch))
          )
      )
    resolution <- makeResolution Set.empty resolutionOutcomes
    Right
      ( BatchResolution
          successorStore
          graph
          idGenerator
          structuralProgress
          alignment
          placement
          controlled
          sortRegistry
          resolution
          Set.empty
          []
      )
    where
      batch = peerPublicationBatch peerPublication

-- | Causally settle a stale structural-reference occurrence without inducing
-- live semantics. The occurrence and decoded intrinsic carrier are retained by
-- the reconciliation/progress owners so the next source sequence can still
-- prove predecessor closure; every destination settles terminally ignored.
resolveRetirementSuppressedStructural ::
  PeerInputContext ->
  Reconciliation.StructuralRetirementSuppression ->
  StructuralOccurrenceStamp ->
  PublicationBatch ->
  CheckedPublication ->
  Store.State ->
  Graph.State ->
  IdGenerator.State ->
  GraphProgress.StructuralProgressState ->
  Alignment.State ->
  Placement.State ->
  Controlled.State ->
  SortRegistry.State ->
  Either PeerInputProblem BatchResolution
resolveRetirementSuppressedStructural
  context
  suppression
  stamp
  batch
  checked
  store
  graph
  idGenerator
  structuralProgress
  alignment
  placement
  controlled
  sortRegistry = do
    carried <-
      if structuralVersionVectorMembershipGenerationId
        (structuralOccurrenceStampPredecessor stamp)
        == structuralVersionVectorMembershipGenerationId
          (GraphProgress.structuralAppliedVector structuralProgress)
        then Right Nothing
        else
          Just
            <$> mapLeft
              PeerInputStructuralProgressProblem
              (GraphProgress.carryDescendantStampedSurvivorOccurrence stamp (GraphProgress.structuralAppliedVector structuralProgress) structuralProgress)
    preparation <-
      mapLeft
        PeerInputStructuralReconciliationProblem
        ( case carried of
            Nothing ->
              Reconciliation.prepareRetirementSuppressedStructuralReconciliation
                suppression
                views
                application
                (GraphProgress.structuralProgressReconciliation structuralProgress)
            Just carriedSurvivor ->
              Reconciliation.prepareRetirementSuppressedCarriedStructuralReconciliation
                suppression
                views
                carriedSurvivor
                application
                (GraphProgress.structuralProgressReconciliation structuralProgress)
        )
    case preparation of
      Reconciliation.StructuralHeld dependencies -> do
        resolution <-
          makeResolution
            (Set.map Publication.StructuralApplicationDependency dependencies)
            (pendingDestinationOutcomes batch)
        Right
          ( BatchResolution
              store
              graph
              idGenerator
              structuralProgress
              alignment
              placement
              controlled
              sortRegistry
              resolution
              Set.empty
              []
          )
      Reconciliation.StructuralReady prepared -> do
        preparedProgress <-
          mapLeft
            PeerInputStructuralProgressProblem
            ( case carried of
                Nothing ->
                  GraphProgress.prepareStructuralApplication
                    stamp
                    (publicationBatchSourceTopologyPrerequisite batch)
                    prepared
                    graph
                    structuralProgress
                Just carriedSurvivor ->
                  GraphProgress.prepareCarriedStructuralApplication
                    carriedSurvivor
                    (publicationBatchSourceTopologyPrerequisite batch)
                    prepared
                    graph
                    structuralProgress
            )
        let (successorGraph, successorProgress, _) =
              GraphProgress.commitStructuralApplication preparedProgress
            induced =
              Set.map
                Publication.StructuralApplicationDependency
                (Reconciliation.preparedStructuralSatisfiedDependencies prepared)
        resolution <-
          makeResolution
            Set.empty
            ( Map.fromList
                [ (destination, Publication.DestinationTerminallyIgnored)
                | destination <- NonEmpty.toList (publicationBatchDestinations batch)
                ]
            )
        Right
          ( BatchResolution
              store
              successorGraph
              idGenerator
              successorProgress
              alignment
              placement
              controlled
              sortRegistry
              resolution
              induced
              []
          )
    where
      views = reconciliationViews context graph controlled sortRegistry
      application =
        Reconciliation.structuralApplication
          (structuralOccurrenceStampOccurrence stamp)
          (structuralOccurrenceStampPredecessor stamp)
          ( sortOccurrence
              (publicationBatchSortId batch)
              (publicationBatchOccurrenceId batch)
          )
          checked
          (publicationBatchControlPrerequisite batch)

resolveEffectiveStructural ::
  PeerInputContext ->
  SortRegistry.RegistryEntry ->
  StructuralOccurrenceStamp ->
  PublicationBatch ->
  CheckedPublication ->
  Store.State ->
  Graph.State ->
  IdGenerator.State ->
  GraphProgress.StructuralProgressState ->
  Alignment.State ->
  Placement.State ->
  Controlled.State ->
  SortRegistry.State ->
  Either PeerInputProblem BatchResolution
resolveEffectiveStructural
  context
  entry
  stamp
  batch
  checked
  store
  graph
  idGenerator
  structuralProgress
  alignment
  placement
  controlled
  sortRegistry = do
    terminalCause <- do
      observation <-
        mapLeft
          ( PeerInputInvariantProblem
              . PeerInputControlledObservationContradiction
                (publicationBatchId batch)
          )
          ( Controlled.checkControlledObservation
              (SortRegistry.registryEntryDescriptor entry)
              (publicationBatchOccurrenceId batch)
              checked
          )
      Right
        ( Controlled.controlledTerminalDeletionCause
            <$> Controlled.controlledTerminalDeletion
              (Controlled.checkedControlledObservationObject observation)
              controlled
        )
    carried <-
      if structuralVersionVectorMembershipGenerationId
        (structuralOccurrenceStampPredecessor stamp)
        == structuralVersionVectorMembershipGenerationId
          (GraphProgress.structuralAppliedVector structuralProgress)
        then Right Nothing
        else
          Just
            <$> mapLeft
              PeerInputStructuralProgressProblem
              (GraphProgress.carryDescendantStampedSurvivorOccurrence stamp (GraphProgress.structuralAppliedVector structuralProgress) structuralProgress)
    (successorIdGenerator, preparation) <-
      prepareStructuralReconciliationWithFresh
        context
        carried
        terminalCause
        application
        graph
        controlled
        sortRegistry
        structuralProgress
        idGenerator
    case preparation of
      Reconciliation.StructuralHeld dependencies -> do
        resolution <-
          makeResolution
            ( Set.map
                Publication.StructuralApplicationDependency
                dependencies
            )
            (pendingDestinationOutcomes batch)
        Right
          ( BatchResolution
              store
              graph
              successorIdGenerator
              structuralProgress
              alignment
              placement
              controlled
              sortRegistry
              resolution
              Set.empty
              []
          )
      Reconciliation.StructuralReady prepared -> do
        controlledResolution <-
          resolveControlledObservation entry batch checked controlled
        preparedStorePatch <-
          mapLeft
            PeerInputStructuralStoreProblem
            ( Store.prepareStructuralStorePatch
                (structuralOccurrenceCause occurrence)
                (Reconciliation.preparedStructuralStorePatch prepared)
                store
            )
        preparedPlacementPatch <-
          mapLeft
            PeerInputStructuralPlacementProblem
            ( Placement.prepareStructuralPlacementPatch
                (publicationBatchControlPrerequisite batch)
                (Reconciliation.preparedStructuralPlacementPatch prepared)
                placement
            )
        preparedProgress <-
          mapLeft
            PeerInputStructuralProgressProblem
            ( case carried of
                Nothing ->
                  GraphProgress.prepareStructuralApplication
                    stamp
                    (publicationBatchSourceTopologyPrerequisite batch)
                    prepared
                    graph
                    structuralProgress
                Just carriedSurvivor ->
                  GraphProgress.prepareCarriedStructuralApplication
                    carriedSurvivor
                    (publicationBatchSourceTopologyPrerequisite batch)
                    prepared
                    graph
                    structuralProgress
            )
        preparedDebts <-
          mapLeft
            PeerInputAlignmentProblem
            ( Alignment.prepareStructuralDebtRetention
                (structuralOccurrenceCause occurrence)
                (Reconciliation.preparedStructuralDebts prepared)
                alignment
            )
        let structurallyAppliedStore =
              Store.commitStructuralStorePatch preparedStorePatch
            (structurallyAppliedPlacement, placementSnapshot) =
              Placement.commitStructuralPlacementPatch preparedPlacementPatch
            (structurallyAppliedGraph, appliedProgress, _) =
              GraphProgress.commitStructuralApplication preparedProgress
            (appliedAlignment, _) =
              Alignment.commitStructuralDebtRetention preparedDebts
        validateCurrentDestinations batch structurallyAppliedStore
        (appliedStore, resolutionOutcomes) <-
          if controlledResolution.currentSuppressed
            then
              Right
                ( structurallyAppliedStore,
                  Map.fromList
                    [ (destination, Publication.DestinationTerminallyIgnored)
                    | destination <- NonEmpty.toList (publicationBatchDestinations batch)
                    ]
                )
            else do
              preparedPeerStore <-
                mapLeft
                  PeerInputStoreProblem
                  ( Store.preparePeerStoreApplication
                      (storeObservationEnvelope (Just stamp) batch)
                      (storeDestinations batch)
                      checked
                      structurallyAppliedStore
                  )
              let (successorStore, outcomes) =
                    Store.commitPeerStoreApplication preparedPeerStore
              Right
                ( successorStore,
                  Map.fromList
                    ( zip
                        (NonEmpty.toList (publicationBatchDestinations batch))
                        (fmap destinationOutcome (NonEmpty.toList outcomes))
                    )
                )
        let
          induced =
            Set.map
              Publication.StructuralApplicationDependency
              (Reconciliation.preparedStructuralSatisfiedDependencies prepared)
        resolution <- makeResolution Set.empty resolutionOutcomes
        Right
          ( BatchResolution
              appliedStore
              structurallyAppliedGraph
              successorIdGenerator
              appliedProgress
              appliedAlignment
              structurallyAppliedPlacement
              controlledResolution.state
              sortRegistry
              resolution
              induced
              (maybe [] pure placementSnapshot)
          )
    where
      occurrence = structuralOccurrenceStampOccurrence stamp
      application =
        Reconciliation.structuralApplication
          occurrence
          (structuralOccurrenceStampPredecessor stamp)
          ( sortOccurrence
              (publicationBatchSortId batch)
              (publicationBatchOccurrenceId batch)
          )
          checked
          (publicationBatchControlPrerequisite batch)

storeObservationEnvelope ::
  Maybe StructuralOccurrenceStamp ->
  PublicationBatch ->
  Store.StoreObservationEnvelope
storeObservationEnvelope stamp batch =
  Store.storeObservationEnvelope
    (publicationBatchSourceProcess batch)
    (publicationBatchOccurrenceId batch)
    (publicationBatchSourceTopologyPrerequisite batch)
    (publicationBatchControlPrerequisite batch)
    stamp

prepareStructuralReconciliationWithFresh ::
  PeerInputContext ->
  Maybe CarriedStampedSurvivor ->
  Maybe StructuralConsequenceCause ->
  Reconciliation.StructuralApplication ->
  Graph.State ->
  Controlled.State ->
  SortRegistry.State ->
  GraphProgress.StructuralProgressState ->
  IdGenerator.State ->
  Either
    PeerInputProblem
    (IdGenerator.State, Reconciliation.StructuralPreparation)
prepareStructuralReconciliationWithFresh
  context
  carried
  terminalCause
  application
  graph
  controlled
  sortRegistry
  structuralProgress
  idGenerator = do
    first <- prepare Nothing
    case first of
      Reconciliation.StructuralHeld dependencies ->
        case Set.toAscList dependencies of
          [Reconciliation.FreshStoreIncarnationDependency delta] -> do
            generated <-
              mapLeft PeerInputIdGeneratorProblem (IdGenerator.prepareGeneratedId idGenerator)
            incarnation <-
              mapLeft
                PeerInputStoreIncarnationIdentityProblem
                (mkStoreIncarnationId (globalUniqueIdBytes (IdGenerator.preparedGlobalUniqueId generated)))
            let evidence =
                  Reconciliation.freshStoreIncarnationEvidence
                    Reconciliation.GeneratedStoreIncarnation
                    delta
                    incarnation
            second <- prepare (Just evidence)
            case second of
              Reconciliation.StructuralReady _ -> Right (IdGenerator.commitGeneratedId generated, second)
              Reconciliation.StructuralHeld _ -> Right (idGenerator, second)
          _ -> Right (idGenerator, first)
      Reconciliation.StructuralReady _ -> Right (idGenerator, first)
    where
      prepare evidence =
        case prepareReconciliation evidence of
          Left (Reconciliation.StructuralPredecessorDotMissing missing) ->
            Right
              ( Reconciliation.StructuralHeld
                  ( Set.singleton
                      (Reconciliation.StructuralPredecessorDependency missing)
                  )
              )
          Left problem -> Left (PeerInputStructuralReconciliationProblem problem)
          Right preparation -> Right preparation
      prepareReconciliation evidence =
        case (terminalCause, carried) of
          (Nothing, Nothing) ->
            Reconciliation.prepareStructuralReconciliation
              views
              application
              evidence
              applied
          (Nothing, Just carriedSurvivor) ->
            Reconciliation.prepareCarriedStructuralReconciliation
              views
              carriedSurvivor
              application
              evidence
              applied
          (Just cause, Nothing) ->
            Reconciliation.prepareTerminallySuppressedStructuralReconciliation
              cause
              views
              application
              evidence
              applied
          (Just cause, Just carriedSurvivor) ->
            Reconciliation.prepareTerminallySuppressedCarriedStructuralReconciliation
              cause
              views
              carriedSurvivor
              application
              evidence
              applied
      views = reconciliationViews context graph controlled sortRegistry
      applied = GraphProgress.structuralProgressReconciliation structuralProgress

reconciliationViews ::
  PeerInputContext ->
  Graph.State ->
  Controlled.State ->
  SortRegistry.State ->
  Reconciliation.ReconciliationViews
reconciliationViews context graph controlled sortRegistry =
  Reconciliation.reconciliationViewsWithEndedProcesses
    ( Map.fromList
        [ (process, (residence, OracleProjection.projectedEndedProcessControlIndex ended))
        | (process, ended) <- OracleProjection.projectedEndedProcesses context.oracle,
          Just residence <- [OracleProjection.oracleViewProcessResidence process (OracleProjection.oracleView context.oracle)]
        ]
    )
    $ Reconciliation.reconciliationViews
      (checkedLocalHeraldEpoch context.genesis)
      ( Map.fromList
          [ ( SortRegistry.registryEntrySortId registryEntry,
              sortOccurrence
                (SortRegistry.registryEntrySortId registryEntry)
                (SortRegistry.registryEntryOccurrenceId registryEntry)
            )
          | registryEntry <- SortRegistry.registryEntries sortRegistry
          ]
      )
      (Set.fromList (Graph.graphNonStructuralBaselineVertices graph))
      ( Map.fromList
          [ (process, residence)
          | process <- OracleProjection.projectedProcessEpochs context.oracle,
            OracleProjection.oracleViewProcessIsLive
              process
              (OracleProjection.oracleView context.oracle),
            Just residence <-
              [ OracleProjection.oracleViewProcessResidence
                  process
                  (OracleProjection.oracleView context.oracle)
              ]
          ]
      )
      (Set.fromList (Controlled.controlledNormalPossessionEntries controlled))

data ControlledBatchResolution = ControlledBatchResolution
  { state :: Controlled.State,
    object :: Maybe GlobalObjectId,
    currentSuppressed :: Bool
  }

resolveControlledObservation ::
  SortRegistry.RegistryEntry ->
  PublicationBatch ->
  CheckedPublication ->
  Controlled.State ->
  Either PeerInputProblem ControlledBatchResolution
resolveControlledObservation entry batch checked controlled
  | descriptorKind descriptor /= ControlledSort =
      Right (ControlledBatchResolution controlled Nothing False)
  | otherwise = do
      observation <-
        mapLeft
          ( PeerInputInvariantProblem
              . PeerInputControlledObservationContradiction (publicationBatchId batch)
          )
          ( Controlled.checkControlledObservation
              (SortRegistry.registryEntryDescriptor entry)
              (publicationBatchOccurrenceId batch)
              checked
          )
      let object = Controlled.checkedControlledObservationObject observation
      prepared <-
        mapLeft
          (PeerInputProtocolProblem . PeerInputControlledObservationRejected (publicationBatchId batch))
          (Controlled.prepareControlledPeerObservation observation controlled)
      let (successor, result) = Controlled.commitControlledPeerObservation prepared
      sharedSuppressed <- resolveSharedSuppression entry checked object successor (Just result)
      Right
        ControlledBatchResolution
          { state = successor,
            object = Just object,
            currentSuppressed = sharedSuppressed
          }
  where
    descriptor = canonicalCheckedDescriptor (SortRegistry.registryEntryDescriptor entry)

resolveSharedSuppression ::
  SortRegistry.RegistryEntry ->
  CheckedPublication ->
  GlobalObjectId ->
  Controlled.State ->
  Maybe Controlled.ControlledPeerObservationResult ->
  Either PeerInputProblem Bool
resolveSharedSuppression entry publication object controlled ownerResult = do
  (sharedSuppressed, hiddenReason) <-
    if Controlled.controlledObjectTerminallyDeleted object controlled
      then Right (True, Nothing)
      else do
        context <-
          mapLeft
            PeerInputEffectivePublicationProblem
            ( checkEffectivePublicationContext
                (SortRegistry.registryEntryDescriptor entry)
                controlled
                publication
            )
        let reason =
              effectivePublicationHiddenReason
                (interpretEffectivePublication context)
        Right (reason /= Nothing, reason)
  let ownerSuppressed =
        ownerResult == Just Controlled.ControlledPeerCurrentSuppressed
      ownerAgrees = case ownerResult of
        Nothing -> True
        Just Controlled.ControlledPeerCurrentSuppressed -> sharedSuppressed
        Just _ ->
          not
            ( checkedPublicationLifecycle publication == Current
                && hiddenReason == Just PublicationObsoleteSuppressed
            )
  unless
    ownerAgrees
    ( Left
        ( PeerInputInvariantProblem
            ( PeerInputControlledSuppressionContradiction
                (checkedPublicationId publication)
                ownerSuppressed
                sharedSuppressed
            )
        )
    )
  Right sharedSuppressed

checkedIncomingPublication ::
  SortRegistry.RegistryEntry ->
  PublicationBatch ->
  Either PeerInputProblem CheckedPublication
checkedIncomingPublication entry batch = do
  value <-
    mapLeft
      PeerInputValueProblem
      (decodeCanonicalValue (canonicalValueByteString (publicationBatchCanonicalValue batch)))
  checked <-
    mapLeft
      PeerInputCheckedPublicationProblem
      ( mkCheckedPublication
          (SortRegistry.registryEntryDescriptor entry)
          (publicationBatchId batch)
          value
      )
  if checkedPublicationCanonicalValue checked == publicationBatchCanonicalValue batch
    then Right checked
    else
      Left
        (PeerInputProtocolProblem (PeerInputCanonicalValueMismatch (publicationBatchId batch)))

induceIncomingDefinition ::
  PeerInputContext ->
  PublicationBatch ->
  CheckedPublication ->
  Map PublicationDestination Publication.DestinationOutcome ->
  Store.State ->
  SortRegistry.State ->
  Either
    PeerInputProblem
    (SortRegistry.State, Set Publication.PublicationDependency)
induceIncomingDefinition context batch checked outcomes store registry =
  case SortRegistry.lookupPredefinedRole SortDefinitionRole registry of
    Just carrier
      | checkedPublicationSort checked == SortRegistry.registryEntrySortId carrier,
        targetsPrivateDefinitionView context outcomes store -> do
          descriptor <-
            mapLeft
              PeerInputDefinitionProblem
              (decodeSortDefinitionValue (checkedPublicationValue checked))
          plan <-
            mapLeft
              PeerInputSortInductionProblem
              ( SortRegistry.planSortInduction
                  (checkedSystemId context.genesis)
                  descriptor
                  registry
              )
          let sortId = descriptorSortId descriptor
              occurrence = SortRegistry.sortInductionPlanOccurrenceId plan
              dependency = Publication.EffectiveSortDependency sortId occurrence
          unless
            ( publicationBatchControlPrerequisite batch
                >= SortRegistry.sortInductionPlanControlPrerequisite plan
            )
            ( Left
                ( PeerInputInvariantProblem
                    ( PeerInputDefinitionControlPrerequisiteMismatch
                        (publicationBatchId batch)
                        (SortRegistry.sortInductionPlanControlPrerequisite plan)
                        (publicationBatchControlPrerequisite batch)
                    )
                )
            )
          prepared <-
            mapLeft
              PeerInputSortInductionProblem
              (SortRegistry.prepareSortInduction descriptor occurrence checked registry)
          Right
            ( SortRegistry.commitSortInduction prepared,
              Set.singleton dependency
            )
    _ -> Right (registry, Set.empty)
  where
    targetsPrivateDefinitionView currentContext destinationOutcomes currentStore =
      any destinationIsPrivateDefinitionView (Map.toAscList destinationOutcomes)
      where
        destinationIsPrivateDefinitionView (destination, outcome)
          | outcome /= Publication.DestinationApplied = False
          | otherwise =
              case Store.lookupStoreSlot (publicationDestinationDelta destination) currentStore of
                Just slot ->
                  Store.storeSlotIncarnation slot
                    == publicationDestinationStoreIncarnation destination
                    && Store.storeSlotProvenance slot
                      == Store.HeraldSystemView
                        (checkedLocalHeraldEpoch currentContext.genesis)
                        SortDefinitionRole
                Nothing -> False

makeResolution ::
  Set Publication.PublicationDependency ->
  Map PublicationDestination Publication.DestinationOutcome ->
  Either PeerInputProblem Publication.IncomingPublicationResolution
makeResolution dependencies outcomes =
  mapLeft
    PeerInputPublicationProblem
    (Publication.incomingPublicationResolution dependencies outcomes)

pendingDestinationOutcomes ::
  PublicationBatch -> Map PublicationDestination Publication.DestinationOutcome
pendingDestinationOutcomes batch =
  Map.fromList
    [ (destination, Publication.DestinationPending)
    | destination <- NonEmpty.toList (publicationBatchDestinations batch)
    ]

storeDestinations :: PublicationBatch -> NonEmpty Store.PeerStoreDestination
storeDestinations batch =
  fmap
    ( \destination ->
        Store.peerStoreDestination
          (publicationDestinationDelta destination)
          (publicationDestinationStoreIncarnation destination)
          (publicationDestinationStrength destination)
    )
    (publicationBatchDestinations batch)

destinationOutcome :: Store.PeerStoreOutcome -> Publication.DestinationOutcome
destinationOutcome outcome = case Store.peerStoreOutcomeDisposition outcome of
  Store.PeerStoreApplied -> Publication.DestinationApplied
  Store.PeerStoreAlreadyApplied -> Publication.DestinationApplied
  Store.PeerStoreTerminallyIgnored -> Publication.DestinationTerminallyIgnored

validateEffectiveOccurrence ::
  SortRegistry.RegistryEntry -> PublicationBatch -> Either PeerInputProblem ()
validateEffectiveOccurrence entry batch
  | observed == expected = Right ()
  | otherwise =
      Left
        ( PeerInputProtocolProblem
            ( PeerInputEffectiveOccurrenceMismatch
                (publicationBatchSortId batch)
                expected
                observed
            )
        )
  where
    expected = SortRegistry.registryEntryOccurrenceId entry
    observed = publicationBatchOccurrenceId batch

validateCurrentDestinations ::
  PublicationBatch -> Store.State -> Either PeerInputProblem ()
validateCurrentDestinations batch store =
  mapM_ validate (NonEmpty.toList (publicationBatchDestinations batch))
  where
    validate destination =
      case Store.lookupStoreSlot (publicationDestinationDelta destination) store of
        Just slot
          | Store.storeSlotIncarnation slot
              == publicationDestinationStoreIncarnation destination -> do
              unless
                (Store.storeSlotSortId slot == publicationBatchSortId batch)
                ( Left
                    ( PeerInputProtocolProblem
                        ( PeerInputCurrentDestinationSortMismatch
                            (publicationDestinationDelta destination)
                            (Store.storeSlotSortId slot)
                            (publicationBatchSortId batch)
                        )
                    )
                )
              unless
                (Store.storeSlotOccurrenceId slot == publicationBatchOccurrenceId batch)
                ( Left
                    ( PeerInputProtocolProblem
                        ( PeerInputCurrentDestinationOccurrenceMismatch
                            (publicationDestinationDelta destination)
                            (Store.storeSlotOccurrenceId slot)
                            (publicationBatchOccurrenceId batch)
                        )
                    )
                )
        _ -> Right ()

validatePublicationSource ::
  StreamDirection -> PeerPublication -> Either PeerInputProblem ()
validatePublicationSource direction peerPublication =
  unless
    (observedSource == expectedSource)
    ( Left
        (PeerInputProtocolProblem (PeerInputPublicationSourceEpochMismatch expectedSource observedSource))
    )
  where
    expectedSource = streamDirectionSource direction
    observedSource =
      publicationSourceHeraldEpoch
        (publicationBatchId (peerPublicationBatch peerPublication))

validateStructuralDestination ::
  PeerInputContext -> Placement.State -> PeerPublication -> Either PeerInputProblem ()
validateStructuralDestination context placement peerPublication =
  case peerPublicationStructuralStamp peerPublication of
    Nothing -> Right ()
    Just stamp ->
      unless
        (structuralPublicationDestinationsValid context placement stamp batch)
        ( Left
            ( PeerInputProtocolProblem
                ( PeerInputStructuralDestinationMismatch
                    expected
                    (publicationBatchDestinations batch)
                )
            )
        )
      where
        expected =
          publicationDestination
            (deriveSystemViewDeltaId system local catalogueRole)
            (deriveSystemViewStoreIncarnationId system local catalogueRole)
            (publicationBatchSourceStrength batch)
        catalogueRole = carrierCatalogueRole (structuralOccurrenceStampCarrierRole stamp)
  where
    batch = peerPublicationBatch peerPublication
    system = checkedSystemId context.genesis
    local = checkedLocalHeraldEpoch context.genesis

-- A first semantic observation is admitted against current membership. An
-- exact retained semantic publication may be assigned again after membership
-- advances; candidate classification has already proved byte-for-byte equality
-- with that incumbent, whose original generation is retained by Publication.
validateCandidateStructuralDestination ::
  PeerInputContext ->
  Placement.State ->
  PeerPublication ->
  Publication.PreparedIncomingPublicationCandidate ->
  Either PeerInputProblem ()
validateCandidateStructuralDestination context placement peerPublication candidate =
  case Publication.preparedIncomingPublicationClassification candidate of
    Publication.IncomingPublicationFirstSeen ->
      validateStructuralDestination context placement peerPublication
    Publication.IncomingPublicationSemanticDuplicate -> Right ()
    Publication.IncomingPublicationAssignmentDuplicate -> Right ()

-- | A receiver-specific structural batch must retain its exact mandatory
-- private system view.  The same frozen batch may also contain ordinary
-- application-reader destinations, but it cannot name another private view or
-- strengthen any destination beyond the retained source strength.
structuralPublicationDestinationsValid ::
  PeerInputContext ->
  Placement.State ->
  StructuralOccurrenceStamp ->
  PublicationBatch ->
  Bool
structuralPublicationDestinationsValid context placement stamp batch =
  structuralPublicationDestinationsValidAt
    context
    ( OracleProjection.oracleViewCurrentHeraldMembership
        (OracleProjection.oracleView context.oracle)
    )
    placement
    stamp
    batch

-- | Validate immutable retained destinations against the exact membership
-- generation captured when the publication was first admitted. Live ingress
-- calls 'structuralPublicationDestinationsValid' and therefore still uses
-- current authority.
structuralPublicationDestinationsValidAt ::
  PeerInputContext ->
  HeraldMembershipGeneration ->
  Placement.State ->
  StructuralOccurrenceStamp ->
  PublicationBatch ->
  Bool
structuralPublicationDestinationsValidAt context membership placement stamp batch =
  expected `elem` destinations
    && all isPermittedStructuralDestination destinations
  where
    system = checkedSystemId context.genesis
    local = checkedLocalHeraldEpoch context.genesis
    catalogueRole = carrierCatalogueRole (structuralOccurrenceStampCarrierRole stamp)
    expected =
      publicationDestination
        (deriveSystemViewDeltaId system local catalogueRole)
        (deriveSystemViewStoreIncarnationId system local catalogueRole)
        (publicationBatchSourceStrength batch)
    destinations = NonEmpty.toList (publicationBatchDestinations batch)
    privateSystemViewDeltas =
      Set.fromList
        [ deriveSystemViewDeltaId system herald role
        | herald <- NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership),
          role <- allPredefinedSortRoles
        ]
    isPermittedStructuralDestination destination =
      publicationDestinationStrength destination
        <= publicationBatchSourceStrength batch
        && if Set.member
          (publicationDestinationDelta destination)
          privateSystemViewDeltas
          then destination == expected
          else
            Placement.retainedPlacementRouteMatches
              local
              (publicationDestinationDelta destination)
              (publicationDestinationStoreIncarnation destination)
              (publicationBatchSortId batch)
              (publicationBatchOccurrenceId batch)
              placement

validateBatchAuthority ::
  AuthorityApplicability ->
  PeerInputContext ->
  GraphProgress.StructuralProgressState ->
  Controlled.State ->
  StreamDirection ->
  PeerPublication ->
  Either PeerInputProblem ()
validateBatchAuthority applicability context structuralProgress controlled direction peerPublication =
  validatePublicationAuthorityFacts
    applicability
    context
    structuralProgress
    controlled
    PublicationAuthorityFacts
      { expectedSource = streamDirectionSource direction,
        publicationId = publicationBatchId batch,
        sourceProcess = publicationBatchSourceProcess batch,
        sortId = publicationBatchSortId batch,
        sortOccurrence = publicationBatchOccurrenceId batch,
        sourceTopologyPrerequisite = publicationBatchSourceTopologyPrerequisite batch,
        controlPrerequisite = publicationBatchControlPrerequisite batch
      }
  where
    batch = peerPublicationBatch peerPublication

data PublicationAuthorityFacts = PublicationAuthorityFacts
  { expectedSource :: HeraldEpoch,
    publicationId :: PublicationId,
    sourceProcess :: ProcessEpochId,
    sortId :: SortId,
    sortOccurrence :: SortDefinitionOccurrenceId,
    sourceTopologyPrerequisite :: TopologyCutId,
    controlPrerequisite :: ControlIndex
  }

validateRoutedPublicationAuthorityFacts ::
  AuthorityApplicability ->
  PeerInputContext ->
  GraphProgress.StructuralProgressState ->
  Controlled.State ->
  RoutedPublicationAuthorityClaim ->
  Either PeerInputProblem ()
validateRoutedPublicationAuthorityFacts applicability context structuralProgress controlled claim = do
  validateRoutedPublicationSemantics claim
  validatePublicationAuthorityFacts
    applicability
    context
    structuralProgress
    controlled
    PublicationAuthorityFacts
      { expectedSource = publicationSourceHeraldEpoch identifier,
        publicationId = identifier,
        sourceProcess = claim.sourceProcess,
        sortId = checkedPublicationSort claim.publication,
        sortOccurrence = claim.sortOccurrence,
        sourceTopologyPrerequisite = claim.sourceTopologyPrerequisite,
        controlPrerequisite = claim.controlPrerequisite
      }
  where
    identifier = checkedPublicationId claim.publication

validateRoutedPublicationSemantics ::
  RoutedPublicationAuthorityClaim -> Either PeerInputProblem ()
validateRoutedPublicationSemantics claim = do
  unless
    (descriptorSort == publicationSort)
    ( Left
        ( PeerInputPeerPublicationProblem
            (DescriptorPublicationSortMismatch descriptorSort publicationSort)
        )
    )
  case claim.structuralStamp of
    Nothing ->
      case descriptorStructuralCarrierRole
        (canonicalCheckedDescriptor claim.descriptor) of
        Just role
          | role /= ProcessEpochCarrier ->
              Left
                (PeerInputPeerPublicationProblem (OrdinaryStructuralCarrier role))
        _ -> Right ()
    Just stamp -> do
      expectedDigest <-
        mapLeft
          PeerInputPeerPublicationProblem
          ( structuralPublicationDigestForSemantics
              claim.descriptor
              claim.sortOccurrence
              claim.publication
              claim.sourceProcess
              claim.sourceStrength
              claim.sourceTopologyPrerequisite
              claim.controlPrerequisite
          )
      expectedRole <-
        maybe
          (Left (PeerInputPeerPublicationProblem StructuralCarrierMissing))
          Right
          ( descriptorStructuralCarrierRole
              (canonicalCheckedDescriptor claim.descriptor)
          )
      unless
        (structuralOccurrenceStampPublication stamp == identifier)
        ( Left
            ( PeerInputPeerPublicationProblem
                ( StructuralStampPublicationMismatch
                    identifier
                    (structuralOccurrenceStampPublication stamp)
                )
            )
        )
      unless
        (structuralOccurrenceStampCarrierRole stamp == expectedRole)
        ( Left
            ( PeerInputPeerPublicationProblem
                ( StructuralStampCarrierRoleMismatch
                    expectedRole
                    (structuralOccurrenceStampCarrierRole stamp)
                )
            )
        )
      unless
        (structuralOccurrenceStampPublicationDigest stamp == expectedDigest)
        ( Left
            ( PeerInputPeerPublicationProblem
                ( StructuralStampDigestMismatch
                    expectedDigest
                    (structuralOccurrenceStampPublicationDigest stamp)
                )
            )
        )
  where
    identifier = checkedPublicationId claim.publication
    descriptorSort = descriptorSortId claim.descriptor
    publicationSort = checkedPublicationSort claim.publication

validatePublicationAuthorityFacts ::
  AuthorityApplicability ->
  PeerInputContext ->
  GraphProgress.StructuralProgressState ->
  Controlled.State ->
  PublicationAuthorityFacts ->
  Either PeerInputProblem ()
validatePublicationAuthorityFacts applicability context structuralProgress controlled facts = do
  unless
    (observedSource == facts.expectedSource)
    ( Left
        ( PeerInputProtocolProblem
            (PeerInputPublicationSourceEpochMismatch facts.expectedSource observedSource)
        )
    )
  unless
    (sourceTopologySatisfied facts.sourceTopologyPrerequisite structuralProgress)
    ( Left
        ( PeerInputProtocolProblem
            (PeerInputSourceTopologyUnavailable facts.sourceTopologyPrerequisite)
        )
    )
  unless
    ( OracleProjection.oracleViewSatisfiesPrerequisite
        facts.controlPrerequisite
        (OracleProjection.oracleView context.oracle)
    )
    ( Left
        ( PeerInputProtocolProblem
            (PeerInputSourceControlUnavailable facts.controlPrerequisite)
        )
    )
  -- Preserve the protocol boundary's most specific classification before
  -- authenticating the claimed writer.  The process may be real yet resident
  -- at another Herald; that is a residence fault even when the claimed Nabla
  -- belongs to a different process.  Unknown processes continue into the
  -- writer resolver so they retain the closed writer-unknown classification.
  traverse_
    (validateResidence expectedSource process)
    ( OracleProjection.oracleProjectionProcessResidenceAt
        facts.controlPrerequisite
        process
        context.oracle
    )
  validateResolvedWriter
  where
    expectedSource = facts.expectedSource
    process = facts.sourceProcess
    nabla = publicationNabla facts.publicationId
    observedSource = publicationSourceHeraldEpoch facts.publicationId
    observedAuthority = publicationAuthorityEpoch facts.publicationId

    validateResidence source processId residence =
      unless
        (residence == source)
        ( Left
            ( PeerInputProtocolProblem
                (PeerInputSourceProcessResidenceMismatch processId source residence)
            )
        )

    validateResolvedWriter = do
      resolution <-
        either
          (Left . authorityProblem)
          Right
          ( Authority.resolveNablaAuthorityAt
              nabla
              observedAuthority
              facts.sourceTopologyPrerequisite
              facts.controlPrerequisite
              structuralProgress
              context.oracle
          )
      case applicability of
        RequireCurrentApplicability ->
          either
            (const (Left (PeerInputProtocolProblem (PeerInputSourceWriterUnknown process nabla))))
            Right
            (Authority.checkCurrentAuthorityApplicability resolution controlled)
        RetainedHistoryOnly -> Right ()
      let controller = Authority.nablaAuthorityResolutionController resolution
          residence = Authority.nablaAuthorityResolutionResidence resolution
          typedSort = Authority.nablaAuthorityResolutionSort resolution
      unless
        (controller == process)
        (Left (PeerInputProtocolProblem (PeerInputSourceWriterUnknown process nabla)))
      validateResidence expectedSource process residence
      validateWriterFacts
        nabla
        (Authority.nablaAuthorityResolutionAuthority resolution)
        (sortOccurrenceSortId typedSort)
        (sortOccurrenceDefinition typedSort)

    authorityProblem problem =
      PeerInputProtocolProblem $ case problem of
        Authority.NablaAuthorityClaimMismatch sourceNabla expected observed ->
          PeerInputSourceAuthorityMismatch sourceNabla expected observed
        Authority.NablaAuthorityStructuralProblem
          (GraphProgress.StructuralProgressSourceTopologyUnavailable cut) ->
            PeerInputSourceTopologyUnavailable cut
        Authority.NablaAuthorityStructuralProblem
          (GraphProgress.StructuralProgressControlPrerequisiteNotCovered _ supplied) ->
            PeerInputSourceControlUnavailable supplied
        Authority.NablaAuthorityControlPrecedesOrigin _ supplied ->
          PeerInputSourceControlUnavailable supplied
        _ -> PeerInputSourceWriterUnknown process nabla

    validateWriterFacts sourceNabla expectedAuthority expectedSort expectedOccurrence = do
      unless
        (observedAuthority == expectedAuthority)
        ( Left
            ( PeerInputProtocolProblem
                (PeerInputSourceAuthorityMismatch sourceNabla expectedAuthority observedAuthority)
            )
        )
      unless
        (facts.sortId == expectedSort)
        ( Left
            ( PeerInputProtocolProblem
                (PeerInputSourceSortMismatch sourceNabla expectedSort facts.sortId)
            )
        )
      unless
        (facts.sortOccurrence == expectedOccurrence)
        ( Left
            ( PeerInputProtocolProblem
                ( PeerInputSourceOccurrenceMismatch
                    sourceNabla
                    expectedOccurrence
                    facts.sortOccurrence
                )
            )
        )

carrierCatalogueRole :: StructuralCarrierRole -> PredefinedSortRole
carrierCatalogueRole role = case role of
  NeutralVertexCarrier -> NeutralVertexRole
  EdgeCarrier -> EdgeRole
  NablaCarrier -> NablaRole
  DeltaCarrier -> DeltaRole
  ProcessEpochCarrier -> ProcessEpochRole

sourceTopologySatisfied ::
  TopologyCutId -> GraphProgress.StructuralProgressState -> Bool
sourceTopologySatisfied cut structuralProgress =
  cut == GraphProgress.structuralGenesisCutId structuralProgress
    || maybe
      False
      (const True)
      (GraphProgress.lookupInstalledTopologyCut cut structuralProgress)

controlSatisfied :: PeerInputContext -> PublicationBatch -> Bool
controlSatisfied context batch =
  publicationBatchControlPrerequisite batch
    <= OracleProjection.oracleViewControlIndex (OracleProjection.oracleView context.oracle)

progressForDisposition :: Publication.IncomingDisposition -> ReceiveProgress
progressForDisposition disposition = case disposition of
  Publication.DependencyHeld -> ReceivedPending
  Publication.Applied -> Completed
  Publication.TerminallyIgnored -> Completed
  Publication.ProtocolRejected -> Completed

validateBindingDirection ::
  PeerBinding ->
  SequencedItem PeerLogicalPayload ->
  PeerStream.State PeerLogicalPayload ->
  Either PeerInputProblem ()
validateBindingDirection binding item peerStream
  | observedSource /= expectedSource =
      Left (PeerInputProtocolProblem (PeerInputBindingSourceMismatch expectedSource observedSource))
  | observedDestination /= expectedDestination =
      Left
        ( PeerInputProtocolProblem
            (PeerInputBindingDestinationMismatch expectedDestination observedDestination)
        )
  | otherwise = Right ()
  where
    direction = sequencedItemDirection item
    expectedSource = peerBindingRemoteHeraldEpoch binding
    observedSource = streamDirectionSource direction
    expectedDestination = PeerStream.peerStreamLocalEpoch peerStream
    observedDestination = streamDirectionDestination direction

validateContext :: PeerInputContext -> Either PeerInputProblem ()
validateContext context = do
  mapLeft
    PeerInputDiscoveryProblem
    (Discovery.validateDiscoveryState context.discovery)

-- | Revisit every semantic publication indexed by one newly satisfied
-- dependency. A sort-registry successor must already be present in the supplied
-- atomic slice before its effective-sort dependency is released.
releasePeerDependency ::
  PeerInputContext ->
  Publication.PublicationDependency ->
  PeerInputState ->
  Either PeerInputProblem (PeerInputState, PeerDependencyReleaseResult)
releasePeerDependency context dependency predecessor = do
  validateContext context
  mapLeft
    PeerInputPlacementProblem
    (Placement.validatePlacementState predecessor.placement)
  satisfied <-
    dependencySatisfied
      Nothing
      context
      dependency
      predecessor.sortRegistry
      predecessor.structuralProgress
  if not satisfied
    then
      Right
        ( predecessor,
          PeerDependencyReleaseResult PeerDependencyStillHeld [] [] [] [] []
        )
    else do
      let base =
            incomingPlan
              predecessor.publication
              predecessor.store
              predecessor.graph
              predecessor.idGenerator
              predecessor.structuralProgress
              predecessor.alignment
              predecessor.placement
              predecessor.controlled
              predecessor.sortRegistry
              predecessor.labelBarrier
              predecessor.disappearance
      plan <- releaseFixedPoint Nothing context (Set.singleton dependency) base
      finishRelease
        context
        predecessor
        plan
        ( \publications acknowledgements placementSnapshots wakes deferredRejections ->
            PeerDependencyReleaseResult
              PeerDependencyReleased
              publications
              acknowledgements
              placementSnapshots
              wakes
              deferredRejections
        )

-- | Release only current-successor structural batches after the exact
-- membership-successor cut has been installed in the Graph progress owner.
--
-- The opaque base is deliberately supplied only to this package-private
-- redrive. Ordinary peer ingress therefore continues to retain the singleton
-- dependency until the Step-15 structural-base coordinator has completed.
-- Predecessor-generation survivor stamps cross through the distinct
-- terminal-lineage carry path and remain byte-for-byte unchanged.
releasePeerSuccessorStructuralBase ::
  SuccessorStructuralBase ->
  PeerInputContext ->
  PeerInputState ->
  Either PeerInputProblem (PeerInputState, PeerDependencyReleaseResult)
releasePeerSuccessorStructuralBase successorBase context predecessor = do
  validateContext context
  mapLeft
    PeerInputPlacementProblem
    (Placement.validatePlacementState predecessor.placement)
  mapLeft
    PeerInputStructuralProgressInvariantProblem
    (GraphProgress.validateStructuralProgressState predecessor.structuralProgress)
  let expectedMembership =
        OracleProjection.oracleViewCurrentHeraldMembershipId
          (OracleProjection.oracleView context.oracle)
      baseMembership = successorStructuralBaseGenerationId successorBase
      progressMembership =
        structuralVersionVectorMembershipGenerationId
          (GraphProgress.structuralAppliedVector predecessor.structuralProgress)
      expectedCut = successorStructuralBaseCutId successorBase
      currentCut =
        GraphProgress.structuralLastInstalledCutId predecessor.structuralProgress
  unless
    (baseMembership == expectedMembership)
    ( Left
        ( PeerInputInvariantProblem
            ( PeerInputSuccessorStructuralBaseMembershipMismatch
                expectedMembership
                baseMembership
            )
        )
    )
  unless
    (progressMembership == expectedMembership)
    ( Left
        ( PeerInputInvariantProblem
            ( PeerInputSuccessorStructuralProgressMembershipMismatch
                expectedMembership
                progressMembership
            )
        )
    )
  unless
    ( GraphProgress.structuralSuccessorBaseInstalled
        successorBase
        predecessor.structuralProgress
    )
    ( Left
        ( PeerInputInvariantProblem
            (PeerInputSuccessorStructuralBaseCutMismatch expectedCut currentCut)
        )
    )
  let dependency =
        Publication.SuccessorStructuralBaseDependency expectedMembership
      plan =
        incomingPlan
          predecessor.publication
          predecessor.store
          predecessor.graph
          predecessor.idGenerator
          predecessor.structuralProgress
          predecessor.alignment
          predecessor.placement
          predecessor.controlled
          predecessor.sortRegistry
          predecessor.labelBarrier
          predecessor.disappearance
  released <-
    releaseFixedPoint
      (Just successorBase)
      context
      (Set.singleton dependency)
      plan
  finishRelease
    context
    predecessor
    released
    ( \publications acknowledgements placementSnapshots wakes deferredRejections ->
        PeerDependencyReleaseResult
          PeerDependencyReleased
          publications
          acknowledgements
          placementSnapshots
          wakes
          deferredRejections
    )

-- | Revisit only publications naming one newly installed source topology cut.
-- Repeating the notification is an exact no-op after the reverse waiter index
-- has been removed by the first successful release.
releasePeerTopology ::
  PeerInputContext ->
  TopologyCutId ->
  PeerInputState ->
  Either PeerInputProblem (PeerInputState, PeerDependencyReleaseResult)
releasePeerTopology context cut =
  releasePeerDependency context (Publication.SourceTopologyDependency cut)

-- | Revisit the exact control dependency satisfied by the current Oracle cursor.
releasePeerControl ::
  PeerInputContext ->
  PeerInputState ->
  Either PeerInputProblem (PeerInputState, PeerControlReleaseResult)
releasePeerControl context predecessor = do
  validateContext context
  mapLeft
    PeerInputPlacementProblem
    (Placement.validatePlacementState predecessor.placement)
  let dependency =
        Publication.ControlIndexDependency
          (OracleProjection.oracleViewControlIndex (OracleProjection.oracleView context.oracle))
      base =
        incomingPlan
          predecessor.publication
          predecessor.store
          predecessor.graph
          predecessor.idGenerator
          predecessor.structuralProgress
          predecessor.alignment
          predecessor.placement
          predecessor.controlled
          predecessor.sortRegistry
          predecessor.labelBarrier
          predecessor.disappearance
  let cursor = OracleProjection.oracleViewControlIndex (OracleProjection.oracleView context.oracle)
      probeDependencies =
        Set.fromList
          [ Publication.DisappearanceProbeDependency probe
          | (probe, projected) <- OracleProjection.projectedDisappearanceProbes context.oracle,
            disappearanceProbeOpenControlIndex probe == cursor || case OracleProjection.projectedDisappearanceTerminal projected of
              Just (OracleProjection.ProjectedDisappearanceInvalidated _ index) -> index == cursor
              Just (OracleProjection.ProjectedDisappearanceResolved _ index) -> index == cursor
              Just (OracleProjection.ProjectedDisappearanceAborted _ index) -> index == cursor
              Nothing -> False
          ]
  refreshed <- refreshSuccessorBaseDependencies context base
  plan <- releaseFixedPoint Nothing context (Set.insert dependency probeDependencies) refreshed
  finishRelease
    context
    predecessor
    plan
    ( \publications acknowledgements placementSnapshots wakes deferredRejections ->
        PeerControlReleaseResult
          publications
          acknowledgements
          placementSnapshots
          wakes
          deferredRejections
    )

-- The full-scan reference enumerates authoritative incoming maps and retained
-- lists. Both drivers share only the existing checked atomic item handlers.
data MarkerReleaseDriver = SelectiveMarkerRelease | ExhaustiveMarkerRelease
  deriving stock (Eq)

harvestMarkerChanges :: PeerStream.IncomingMarkerFamily -> PeerInputState -> PeerInputState
harvestMarkerChanges family predecessor = case family of
  PeerStream.RouteCutoverMarkers ->
    let (changes, alignment) = Alignment.takeRouteMarkerChanges predecessor.alignment
     in if Set.null changes then predecessor else predecessor {alignment, peerStream = notify (Set.map PeerStream.RouteMarkerGeneration changes)}
  PeerStream.DisappearanceProbeMarkers ->
    let (changes, disappearance) = Disappearance.takeMarkerProbeChanges predecessor.disappearance
     in if Set.null changes then predecessor else predecessor {disappearance, peerStream = notify (Set.map PeerStream.DisappearanceMarkerProbe changes)}
  where
    notify dependencies = PeerStream.notifyIncomingMarkerDependencies dependencies predecessor.peerStream

samePeerInputMarkerWorkState :: PeerInputState -> PeerInputState -> Bool
samePeerInputMarkerWorkState left right = clear left == clear right
  where
    clear state =
      state
        { peerStream = PeerStream.clearIncomingMarkerWork state.peerStream,
          alignment = Alignment.clearRouteMarkerChanges state.alignment,
          disappearance = Disappearance.clearMarkerProbeChanges state.disappearance
        }

type MarkerCompletions = Map StreamDirection (Set StreamSequence)
type MarkerRejections = Map StreamDirection DeferredPeerRejection
type MarkerDirectionProgress owner = (owner, MarkerCompletions, MarkerRejections, StreamSequence, Bool)

releaseMarkerDirections ::
  MarkerReleaseDriver ->
  PeerStream.IncomingMarkerFamily ->
  PeerInputState ->
  (StreamDirection -> MarkerDirectionProgress owner -> (SequencedItem PeerLogicalPayload, PeerStream.IncomingItemStatus) -> Either PeerInputProblem (MarkerDirectionProgress owner)) ->
  owner ->
  Either PeerInputProblem (owner, MarkerCompletions, MarkerRejections, PeerStream.State PeerLogicalPayload)
releaseMarkerDirections driver family predecessor releaseItem owner = do
  case driver of
    ExhaustiveMarkerRelease -> do
      directions <- mapLeft PeerInputStreamProblem (PeerStream.peerStreamIncomingDirections predecessor.peerStream)
      foldM releaseDirection initial directions
    SelectiveMarkerRelease -> nextDirection Nothing initial
  where
    initial = (owner, Map.empty, Map.empty, predecessor.peerStream)
    inventory = PeerStream.incomingMarkerDirections family predecessor.peerStream
    nextDirection cursor accumulated@(_, _, _, stream) =
      case PeerStream.nextIncomingMarkerDirection family inventory cursor stream of
        Nothing -> Right accumulated
        Just direction -> releaseDirection accumulated direction >>= nextDirection (Just direction)
    releaseDirection (current, completions, rejections, stream) direction = do
      watermarks <- mapLeft PeerInputStreamProblem (PeerStream.incomingWatermarks direction predecessor.peerStream)
      let start = (current, completions, rejections, nextAfterStreamPrefix (streamCompletedPrefix watermarks), False)
      (after, nextCompletions, nextRejections, expected, _) <- case driver of
        ExhaustiveMarkerRelease -> do
          retained <- mapLeft PeerInputStreamProblem (PeerStream.incomingRetainedItems direction predecessor.peerStream)
          foldM (releaseItem direction) start retained
        SelectiveMarkerRelease -> releaseHeads direction start
      dependencies <- markerHeadDependencies family direction expected predecessor.peerStream
      let nextStream =
            PeerStream.retainIncomingMarkerDependencies
              family
              direction
              dependencies
              (PeerStream.consumeIncomingMarkerDirection family direction stream)
      Right (after, nextCompletions, nextRejections, nextStream)
    releaseHeads direction accumulated@(_, _, _, expected, blocked)
      | blocked = Right accumulated
      | otherwise = do
          retained <- mapLeft PeerInputStreamProblem (PeerStream.incomingItemAtOrAfter direction expected predecessor.peerStream)
          case retained of
            Nothing -> Right accumulated
            Just item -> releaseItem direction accumulated item >>= releaseHeads direction

markerHeadDependencies :: PeerStream.IncomingMarkerFamily -> StreamDirection -> StreamSequence -> PeerStream.State PeerLogicalPayload -> Either PeerInputProblem (Set PeerStream.IncomingMarkerDependency)
markerHeadDependencies family direction expected stream = do
  retained <- mapLeft PeerInputStreamProblem (PeerStream.incomingItemAtOrAfter direction expected stream)
  Right $ case retained of
    Just (item, PeerStream.IncomingReceivedPending) -> case (family, sequencedItemPayload item) of
      (PeerStream.RouteCutoverMarkers, PeerLogicalRouteCutover marker) -> Set.singleton (PeerStream.RouteMarkerGeneration (alignmentRouteCutoverMarkerGeneration marker))
      (PeerStream.DisappearanceProbeMarkers, PeerLogicalDisappearanceProbeMarker marker) -> Set.singleton (PeerStream.DisappearanceMarkerProbe (DisappearanceProtocol.disappearancePublicationMarkerProbe marker))
      _ -> Set.empty
    _ -> Set.empty

-- | Re-drive ordered alignment markers whose unordered generation and
-- transfer dependencies have become available. A logical marker remains
-- received-pending until its complete semantic transcript is retained, so its
-- completion prefix cannot overtake any preceding publication.
releasePendingAlignmentRouteCutovers ::
  PeerInputContext ->
  PeerInputState ->
  Either PeerInputProblem (PeerInputState, PeerDependencyReleaseResult)
releasePendingAlignmentRouteCutovers context = releasePendingAlignmentRouteCutoversWith SelectiveMarkerRelease context . harvestMarkerChanges PeerStream.RouteCutoverMarkers

releasePendingAlignmentRouteCutoversExhaustive ::
  PeerInputContext ->
  PeerInputState ->
  Either PeerInputProblem (PeerInputState, PeerDependencyReleaseResult)
releasePendingAlignmentRouteCutoversExhaustive context = releasePendingAlignmentRouteCutoversWith ExhaustiveMarkerRelease context . harvestMarkerChanges PeerStream.RouteCutoverMarkers

releasePendingAlignmentRouteCutoversWith :: MarkerReleaseDriver -> PeerInputContext -> PeerInputState -> Either PeerInputProblem (PeerInputState, PeerDependencyReleaseResult)
releasePendingAlignmentRouteCutoversWith driver context predecessor = do
  validateContext context
  (alignment, completions, deferredRejections, scheduledStream) <-
    releaseMarkerDirections driver PeerStream.RouteCutoverMarkers predecessor releaseItem predecessor.alignment
  peerStream <- completeAssignments completions scheduledStream
  acknowledgements <-
    acknowledgementsFor
      (Map.keysSet completions `Set.difference` Map.keysSet deferredRejections)
      predecessor.peerStream
      peerStream
  let disposition =
        if Map.null completions
          then PeerDependencyStillHeld
          else PeerDependencyReleased
      successor =
        predecessor
          { alignment,
            peerStream
          }
  Right
    ( successor,
      PeerDependencyReleaseResult
        disposition
        []
        acknowledgements
        []
        []
        (Map.elems deferredRejections)
    )
  where
    releaseItem
      direction
      accumulated@(alignment, completions, deferredRejections, expected, blocked)
      (item, status)
        | blocked = Right accumulated
        | sequencedItemSequence item < expected = Right accumulated
        | sequencedItemSequence item > expected = do
            next <- mapLeft PeerInputStreamProblem (PeerStream.nextUncompletedIncomingSequence direction expected predecessor.peerStream)
            if next == expected
              then Right (alignment, completions, deferredRejections, expected, True)
              else releaseItem direction (alignment, completions, deferredRejections, next, False) (item, status)
        | otherwise = case (status, sequencedItemPayload item) of
            (PeerStream.IncomingReceivedPending, PeerLogicalRouteCutover marker) ->
              if sourceRetired direction
                then
                  terminallySettle
                    direction
                    item
                    alignment
                    completions
                    deferredRejections
                    expected
                else case AlignmentUseCase.retainAlignmentRouteCutoverMarker
                  (streamDirectionSource direction)
                  (streamDirectionDestination direction)
                  marker
                  alignment of
                  Left (AlignmentUseCase.AlignmentRouteCutoverGenerationMissing _) ->
                    Right
                      ( alignment,
                        completions,
                        deferredRejections,
                        expected,
                        True
                      )
                  Left (AlignmentUseCase.AlignmentRouteCutoverSourceAcceptanceMissing _ _) ->
                    Right
                      ( alignment,
                        completions,
                        deferredRejections,
                        expected,
                        True
                      )
                  Left problem ->
                    terminallyReject
                      direction
                      item
                      alignment
                      completions
                      deferredRejections
                      expected
                      (PeerInputAlignmentRouteCutoverProblem problem)
                  Right successorAlignment ->
                    terminallySettle
                      direction
                      item
                      successorAlignment
                      completions
                      deferredRejections
                      expected
            _ ->
              Right
                ( alignment,
                  completions,
                  deferredRejections,
                  expected,
                  True
                )

    -- A marker's source acceptance is source-epoch-specific. Once that source
    -- has been retired, the route change is no longer applicable even if its
    -- unordered evidence happened to arrive first. Complete the exact retained
    -- assignment locally without mutating Alignment; the ordinary
    -- acknowledgement path suppresses traffic to the retired peer.
    sourceRetired direction =
      PeerStream.peerStreamPeerRetired
        (streamDirectionSource direction)
        predecessor.peerStream

    terminallySettle
      direction
      item
      alignment
      completions
      deferredRejections
      expected =
        Right
          ( alignment,
            Map.insertWith
              Set.union
              direction
              (Set.singleton (sequencedItemSequence item))
              completions,
            deferredRejections,
            nextStreamSequence expected,
            False
          )

    terminallyReject
      direction
      item
      alignment
      completions
      deferredRejections
      expected
      problem =
        case peerInputProblemDisposition problem of
          PeerInputInvariantFault -> Left problem
          RejectCurrentPeerBinding ->
            Right
              ( alignment,
                Map.insertWith
                  Set.union
                  direction
                  (Set.singleton (sequencedItemSequence item))
                  completions,
                Map.insert
                  direction
                  (DeferredPeerRejection direction Nothing)
                  deferredRejections,
                nextStreamSequence expected,
                False
              )

-- | Only the next semantically incomplete stream position may complete a cut.
-- An unknown Open leaves the exact marker in the normal received-pending slot;
-- the caller redrives this transition after Oracle or publication progress.
releasePendingDisappearanceProbeMarkers ::
  PeerInputContext ->
  PeerInputState ->
  Either PeerInputProblem (PeerInputState, PeerDependencyReleaseResult)
releasePendingDisappearanceProbeMarkers context = releasePendingDisappearanceProbeMarkersWith SelectiveMarkerRelease context . harvestMarkerChanges PeerStream.DisappearanceProbeMarkers

releasePendingDisappearanceProbeMarkersExhaustive ::
  PeerInputContext ->
  PeerInputState ->
  Either PeerInputProblem (PeerInputState, PeerDependencyReleaseResult)
releasePendingDisappearanceProbeMarkersExhaustive context = releasePendingDisappearanceProbeMarkersWith ExhaustiveMarkerRelease context . harvestMarkerChanges PeerStream.DisappearanceProbeMarkers

releasePendingDisappearanceProbeMarkersWith :: MarkerReleaseDriver -> PeerInputContext -> PeerInputState -> Either PeerInputProblem (PeerInputState, PeerDependencyReleaseResult)
releasePendingDisappearanceProbeMarkersWith driver context predecessor = do
  validateContext context
  (disappearance, completions, rejections, scheduledStream) <-
    releaseMarkerDirections driver PeerStream.DisappearanceProbeMarkers predecessor releaseItem predecessor.disappearance
  peerStream <- completeAssignments completions scheduledStream
  acknowledgements <-
    acknowledgementsFor
      (Map.keysSet completions `Set.difference` Map.keysSet rejections)
      predecessor.peerStream
      peerStream
  Right
    ( predecessor {disappearance, peerStream},
      PeerDependencyReleaseResult
        (if Map.null completions then PeerDependencyStillHeld else PeerDependencyReleased)
        []
        acknowledgements
        []
        []
        (Map.elems rejections)
    )
  where
    releaseItem direction accumulated@(owner, completions, rejections, expected, blocked) (item, status)
      | blocked || sequencedItemSequence item < expected = Right accumulated
      | sequencedItemSequence item > expected = do
          next <- mapLeft PeerInputStreamProblem (PeerStream.nextUncompletedIncomingSequence direction expected predecessor.peerStream)
          if next == expected
            then Right (owner, completions, rejections, expected, True)
            else releaseItem direction (owner, completions, rejections, next, False) (item, status)
      | otherwise = case (status, sequencedItemPayload item) of
          (PeerStream.IncomingReceivedPending, PeerLogicalDisappearanceProbeMarker marker) ->
            case Disappearance.preparePublicationMarkerReceipt marker owner of
              Left problem -> reject problem
              Right prepared ->
                case Disappearance.preparedDisappearanceOutput prepared of
                  Disappearance.MarkerAwaitingProbeProjection _ -> hold
                  _ -> do
                    let afterReceipt = fst (Disappearance.commitDisappearanceTransition prepared)
                    completed <-
                      mapLeft
                        PeerInputDisappearanceProblem
                        (Disappearance.preparePublicationMarkerCompletion marker afterReceipt)
                    complete (fst (Disappearance.commitDisappearanceTransition completed)) rejections
          (PeerStream.IncomingCompleted, _) ->
            Right (owner, completions, rejections, nextStreamSequence expected, False)
          _ -> hold
      where
        hold = Right (owner, completions, rejections, expected, True)
        complete nextOwner nextRejections =
          Right
            ( nextOwner,
              Map.insertWith Set.union direction (Set.singleton expected) completions,
              nextRejections,
              nextStreamSequence expected,
              False
            )
        reject problem = case peerInputProblemDisposition (PeerInputDisappearanceProblem problem) of
          PeerInputInvariantFault -> Left (PeerInputDisappearanceProblem problem)
          RejectCurrentPeerBinding ->
            complete
              owner
              (Map.insert direction (DeferredPeerRejection direction Nothing) rejections)

finishRelease ::
  PeerInputContext ->
  PeerInputState ->
  IncomingPlan ->
  ( [PublicationId] ->
    [PeerAcknowledgementPlan] ->
    [PlacementSnapshot] ->
    [Application.ApplicationWaitWake] ->
    [DeferredPeerRejection] ->
    result
  ) ->
  Either PeerInputProblem (PeerInputState, result)
finishRelease context predecessor plan makeResult = do
  peerStream <- completeAssignments plan.completions predecessor.peerStream
  acknowledgements <-
    acknowledgementsFor
      ( Map.keysSet plan.completions
          `Set.difference` Map.keysSet plan.deferredRejections
      )
      predecessor.peerStream
      peerStream
  (application, wait, wakes) <-
    completeReadyWaits
      plan.store
      plan.controlled
      plan.sortRegistry
      predecessor.application
      predecessor.wait
  let successor =
        PeerInputState
          { publication = plan.publication,
            peerStream,
            store = plan.store,
            graph = plan.graph,
            idGenerator = plan.idGenerator,
            structuralProgress = plan.structuralProgress,
            alignment = plan.alignment,
            placement = plan.placement,
            controlled = plan.controlled,
            sortRegistry = plan.sortRegistry,
            application,
            wait,
            labelBarrier = plan.labelBarrier,
            disappearance = plan.disappearance
          }
  (released, markerResult) <- releasePendingDisappearanceProbeMarkers context successor
  publication <-
    mapLeft
      PeerInputPublicationProblem
      ( Publication.retireIncomingAssignmentReceipts
          (\key -> PeerStream.incomingSequenceCompleted key released.peerStream)
          released.publication
      )
  Right
    ( replaceCompletedPublicationOwner publication released,
      makeResult
        (reverse plan.reversePublications)
        (acknowledgements <> peerDependencyAcknowledgements markerResult)
        (reverse plan.reversePlacementSnapshots)
        wakes
        (Map.elems plan.deferredRejections <> peerDependencyDeferredRejections markerResult)
    )

-- | A later contraction updates the wait target, never the retained payload's
-- admission generation. Only a checked lineage can authorize this owner change.
refreshSuccessorBaseDependencies :: PeerInputContext -> IncomingPlan -> Either PeerInputProblem IncomingPlan
refreshSuccessorBaseDependencies context initial = foldM retarget initial waits
  where
    view = OracleProjection.oracleView context.oracle
    current = OracleProjection.oracleViewCurrentHeraldMembershipId view
    waits =
      [ (identifier, target)
      | (identifier, record) <- Publication.incomingPublicationEntries initial.publication,
        Publication.incomingPublicationDisposition record == Publication.DependencyHeld,
        Publication.SuccessorStructuralBaseDependency target <- Set.toList (Publication.incomingPublicationDependencies record),
        target /= current
      ]
    retarget plan (identifier, target) = case OracleProjection.oracleViewHeraldMembershipLineage target current view of
      Nothing -> Left (PeerInputInvariantProblem (PeerInputDependencyWaiterContradiction identifier (Publication.SuccessorStructuralBaseDependency target)))
      Just lineage -> do
        prepared <- mapLeft PeerInputPublicationProblem (Publication.prepareIncomingSuccessorBaseRetarget lineage identifier plan.publication)
        let (publication, _) = Publication.commitIncomingPublicationProgress prepared
        pure (IncomingPlan publication plan.store plan.graph plan.idGenerator plan.structuralProgress plan.alignment plan.placement plan.controlled plan.sortRegistry plan.labelBarrier plan.disappearance plan.progress plan.completions plan.inducedDependencies plan.deferredRejections plan.reversePublications plan.reversePlacementSnapshots)

releaseFixedPoint ::
  Maybe SuccessorStructuralBase ->
  PeerInputContext ->
  Set Publication.PublicationDependency ->
  IncomingPlan ->
  Either PeerInputProblem IncomingPlan
releaseFixedPoint successorBase context seeds = go Set.empty seeds
  where
    go completed pending plan = case Set.minView pending of
      Nothing -> Right plan
      Just (dependency, remaining)
        | Set.member dependency completed -> go completed remaining plan
        | otherwise -> do
            satisfied <-
              dependencySatisfied
                successorBase
                context
                dependency
                plan.sortRegistry
                plan.structuralProgress
            case satisfied of
              False -> go (Set.insert dependency completed) remaining plan
              True -> do
                let identifiers =
                      Set.toAscList
                        (Publication.incomingDependencyWaiters dependency plan.publication)
                records <- traverse (waiterRecord plan.publication) identifiers
                progressed <-
                  foldM
                    (releaseDependencyRecord successorBase context dependency)
                    plan
                    (sortIncomingRecords records)
                let newlyInduced =
                      expandSortDependencyNotifications
                        progressed.publication
                        progressed.inducedDependencies
                go
                  (Set.insert dependency completed)
                  (remaining `Set.union` newlyInduced)
                  (clearPlanInducedDependencies progressed)

    waiterRecord publication identifier =
      maybe
        (Left (PeerInputInvariantProblem (PeerInputDependencyWaiterMissing identifier)))
        (Right . (identifier,))
        (Publication.lookupIncomingPublication identifier publication)

    clearPlanInducedDependencies plan =
      IncomingPlan
        plan.publication
        plan.store
        plan.graph
        plan.idGenerator
        plan.structuralProgress
        plan.alignment
        plan.placement
        plan.controlled
        plan.sortRegistry
        plan.labelBarrier
        plan.disappearance
        plan.progress
        plan.completions
        Set.empty
        plan.deferredRejections
        plan.reversePublications
        plan.reversePlacementSnapshots

-- | Installing one occurrence makes every waiter for that public SortId
-- reclassifiable.  Exact successor waiters can proceed, covered malformed
-- occurrences fault, and future-control occurrences remain held by the
-- per-record decision below.
expandSortDependencyNotifications ::
  Publication.State ->
  Set Publication.PublicationDependency ->
  Set Publication.PublicationDependency
expandSortDependencyNotifications publication dependencies =
  Set.unions (fmap expand (Set.toList dependencies))
  where
    expand dependency@(Publication.EffectiveSortDependency sortId _) =
      Set.insert
        dependency
        ( Set.fromList
            [ waiter
            | (waiter@(Publication.EffectiveSortDependency waiterSort _), _) <-
                Publication.incomingDependencyWaiterEntries publication,
              waiterSort == sortId
            ]
        )
    expand dependency = Set.singleton dependency

releaseDependencyRecord ::
  Maybe SuccessorStructuralBase ->
  PeerInputContext ->
  Publication.PublicationDependency ->
  IncomingPlan ->
  (PublicationId, Publication.IncomingPublicationRecord) ->
  Either PeerInputProblem IncomingPlan
releaseDependencyRecord successorBase context dependency plan (identifier, _) = do
  record <-
    maybe
      (Left (PeerInputInvariantProblem (PeerInputDependencyWaiterMissing identifier)))
      Right
      (Publication.lookupIncomingPublication identifier plan.publication)
  unless
    (Set.member dependency (Publication.incomingPublicationDependencies record))
    ( Left
        (PeerInputInvariantProblem (PeerInputDependencyWaiterContradiction identifier dependency))
    )
  let stillGated = case dependency of
        Publication.DisappearanceProbeDependency probe ->
          any
            ((== probe) . Disappearance.matchingWriteGateProbe)
            (DisappearanceGate.peerPublicationGates (Publication.incomingPeerPublication record) plan.disappearance)
        _ -> False
  if stillGated
    then do
      disappearance <-
        retainIncomingGateInvalidations
          (Set.fromList [probe | Publication.DisappearanceProbeDependency probe <- [dependency]])
          (Publication.incomingPeerPublication record)
          plan.disappearance
      let successor :: IncomingPlan
          successor =
            IncomingPlan
              plan.publication
              plan.store
              plan.graph
              plan.idGenerator
              plan.structuralProgress
              plan.alignment
              plan.placement
              plan.controlled
              plan.sortRegistry
              plan.labelBarrier
              disappearance
              plan.progress
              plan.completions
              plan.inducedDependencies
              plan.deferredRejections
              plan.reversePublications
              plan.reversePlacementSnapshots
      Right successor
    else
      if not (successorBaseAppliesToRecord successorBase context dependency record)
        then Right plan
        else case reclassifyReleasedDependency
          context
          dependency
          record
          plan.sortRegistry of
          Left problem -> deferRecordProtocolRejection identifier record problem plan
          Right Nothing -> Right plan
          Right (Just dependencies) ->
            case if Set.null dependencies
              then resolveReadyRecord context record plan
              else do
                held <-
                  makeResolution
                    dependencies
                    (Publication.incomingPublicationDestinationOutcomes record)
                Right
                  ( BatchResolution
                      plan.store
                      plan.graph
                      plan.idGenerator
                      plan.structuralProgress
                      plan.alignment
                      plan.placement
                      plan.controlled
                      plan.sortRegistry
                      held
                      Set.empty
                      []
                  ) of
              Left problem -> deferRecordProtocolRejection identifier record problem plan
              Right resolution -> progressRecord identifier resolution plan

-- | A retained item can become classifiable only while another peer input, an
-- Oracle advance, or a topology release is being processed.  Remote protocol
-- failure must therefore be charged to the retained record's direction rather
-- than to whichever event happened to expose it.  Mark its transport position
-- complete, detach it from semantic waiters, and let the outer coordinator
-- close that exact current binding. Checked owner contradictions still fail the
-- whole transition.
deferRecordProtocolRejection ::
  PublicationId ->
  Publication.IncomingPublicationRecord ->
  PeerInputProblem ->
  IncomingPlan ->
  Either PeerInputProblem IncomingPlan
deferRecordProtocolRejection identifier record problem plan =
  case peerInputProblemDisposition problem of
    PeerInputInvariantFault -> Left problem
    RejectCurrentPeerBinding -> do
      prepared <-
        mapLeft
          PeerInputPublicationProblem
          (Publication.prepareIncomingPublicationRejection identifier plan.publication)
      let (publication, rejected) =
            Publication.commitIncomingPublicationRejection prepared
          completions =
            foldl
              insertAssignmentCompletion
              plan.completions
              (Set.toAscList (Publication.incomingPublicationAssignments rejected))
          rejection =
            DeferredPeerRejection
              (Publication.incomingPublicationDirection record)
              (Just identifier)
      Right
        plan
          { publication,
            completions,
            deferredRejections =
              Map.insert rejection.direction rejection plan.deferredRejections
          }

-- | Reconsider one exact waiter when its dependency's owner advances.  A sort
-- installation also wakes same-SortId speculative occurrences: if their
-- control prerequisite is covered, a different installed occurrence is now a
-- protocol mismatch; if it is still in the future, the exact waiter remains
-- intact for that later control decision.
reclassifyReleasedDependency ::
  PeerInputContext ->
  Publication.PublicationDependency ->
  Publication.IncomingPublicationRecord ->
  SortRegistry.State ->
  Either PeerInputProblem (Maybe (Set Publication.PublicationDependency))
reclassifyReleasedDependency context dependency record registry =
  case dependency of
    Publication.EffectiveSortDependency {} ->
      resolveBatchSortEntry
        (controlSatisfied context batch)
        batch
        registry
        >>= \case
          BatchSortUnavailable -> Right Nothing
          BatchSortEffective {} -> Right (Just withoutReleased)
          BatchSortRetired {} -> Right (Just withoutReleased)
    _ ->
      Just
        <$> reclassifySortAfterControl
          context
          dependency
          record
          registry
          withoutReleased
  where
    batch = Publication.incomingPublicationBatch record
    withoutReleased =
      Set.delete dependency (Publication.incomingPublicationDependencies record)

-- | A batch received ahead of its control prefix cannot yet distinguish an
-- unknown future definition from a malformed occurrence of a known sort. Once
-- that exact control dependency releases, reclassify only this record: an
-- effective or retained occurrence no longer needs its speculative sort hold,
-- an actually unknown sort keeps waiting, and a now-decidable mismatch closes
-- the peer binding instead of remaining held forever.
reclassifySortAfterControl ::
  PeerInputContext ->
  Publication.PublicationDependency ->
  Publication.IncomingPublicationRecord ->
  SortRegistry.State ->
  Set Publication.PublicationDependency ->
  Either PeerInputProblem (Set Publication.PublicationDependency)
reclassifySortAfterControl context released record registry remaining =
  case released of
    Publication.ControlIndexDependency prerequisite
      | prerequisite == publicationBatchControlPrerequisite batch,
        controlSatisfied context batch,
        Set.member sortDependency remaining ->
          resolveBatchSortEntry True batch registry
            >>= \case
              BatchSortUnavailable -> Right remaining
              BatchSortEffective {} -> Right (Set.delete sortDependency remaining)
              BatchSortRetired {} -> Right (Set.delete sortDependency remaining)
    _ -> Right remaining
  where
    batch = Publication.incomingPublicationBatch record
    sortDependency =
      Publication.EffectiveSortDependency
        (publicationBatchSortId batch)
        (publicationBatchOccurrenceId batch)

-- A current base releases work from any admitted ancestor generation. The
-- record and structural stamp remain immutable; actual execution separately
-- proves its installed lineage through the Graph progress owner.
successorBaseAppliesToRecord ::
  Maybe SuccessorStructuralBase ->
  PeerInputContext ->
  Publication.PublicationDependency ->
  Publication.IncomingPublicationRecord ->
  Bool
successorBaseAppliesToRecord successorBase context dependency record =
  case (successorBase, dependency) of
    (Just base, Publication.SuccessorStructuralBaseDependency generation) ->
      let recordGeneration = Publication.incomingPublicationMembershipGenerationId record
       in generation == successorStructuralBaseGenerationId base
            && case OracleProjection.oracleViewHeraldMembershipLineage recordGeneration generation (OracleProjection.oracleView context.oracle) of
              Just _ -> True
              Nothing -> False
            && case peerPublicationStructuralStamp
              (Publication.incomingPeerPublication record) of
              Nothing -> False
              Just _ -> True
    _ -> True

resolveReadyRecord ::
  PeerInputContext ->
  Publication.IncomingPublicationRecord ->
  IncomingPlan ->
  Either PeerInputProblem BatchResolution
resolveReadyRecord context record plan =
  case resolveBatchSortEntry True batch plan.sortRegistry of
    Left problem -> Left problem
    Right BatchSortUnavailable ->
      Left
        ( PeerInputInvariantProblem
            (PeerInputReadySortMissing (publicationBatchId batch) (publicationBatchSortId batch))
        )
    Right (BatchSortEffective entry) ->
      resolveEffectivePeerPublication
        RetainedHistoryOnly
        (Publication.incomingPublicationMembershipGenerationId record)
        context
        (Publication.incomingPublicationDirection record)
        entry
        (Publication.incomingPeerPublication record)
        plan.store
        plan.graph
        plan.idGenerator
        plan.structuralProgress
        plan.alignment
        plan.placement
        plan.controlled
        plan.sortRegistry
    Right (BatchSortRetired entry) ->
      resolveRetiredPeerPublication
        RetainedHistoryOnly
        context
        (Publication.incomingPublicationDirection record)
        entry
        (Publication.incomingPeerPublication record)
        plan.store
        plan.graph
        plan.idGenerator
        plan.structuralProgress
        plan.alignment
        plan.placement
        plan.controlled
        plan.sortRegistry
  where
    batch = Publication.incomingPublicationBatch record

progressRecord ::
  PublicationId ->
  BatchResolution ->
  IncomingPlan ->
  Either PeerInputProblem IncomingPlan
progressRecord identifier resolution plan = do
  prepared <-
    mapLeft
      PeerInputPublicationProblem
      ( Publication.prepareIncomingPublicationProgress
          identifier
          resolution.resolution
          plan.publication
      )
  let (publication, record) = Publication.commitIncomingPublicationProgress prepared
      completed =
        Publication.incomingPublicationDisposition record
          `elem` [Publication.Applied, Publication.TerminallyIgnored]
      completions =
        if completed
          then
            foldl
              insertAssignmentCompletion
              plan.completions
              (Set.toAscList (Publication.incomingPublicationAssignments record))
          else plan.completions
  Right
    plan
      { publication,
        store = resolution.store,
        graph = resolution.graph,
        idGenerator = resolution.idGenerator,
        structuralProgress = resolution.structuralProgress,
        alignment = resolution.alignment,
        placement = resolution.placement,
        controlled = resolution.controlled,
        sortRegistry = resolution.sortRegistry,
        inducedDependencies =
          plan.inducedDependencies `Set.union` resolution.inducedDependencies,
        completions,
        reversePublications = identifier : plan.reversePublications,
        reversePlacementSnapshots =
          reverse resolution.placementSnapshots
            <> plan.reversePlacementSnapshots
      }

sortIncomingRecords ::
  [(PublicationId, Publication.IncomingPublicationRecord)] ->
  [(PublicationId, Publication.IncomingPublicationRecord)]
sortIncomingRecords = sortOn incomingRecordKey
  where
    incomingRecordKey (identifier, record) =
      ( fmap
          ( \assignment ->
              ( assignmentReceiptDirection assignment,
                assignmentReceiptSequence assignment
              )
          )
          (Set.lookupMin (Publication.incomingPublicationAssignments record)),
        identifier
      )

insertAssignmentCompletion ::
  Map StreamDirection (Set StreamSequence) ->
  AssignmentReceipt ->
  Map StreamDirection (Set StreamSequence)
insertAssignmentCompletion completions assignment =
  Map.insertWith
    Set.union
    (assignmentReceiptDirection assignment)
    (Set.singleton (assignmentReceiptSequence assignment))
    completions

completeAssignments ::
  Map StreamDirection (Set StreamSequence) ->
  PeerStream.State PeerLogicalPayload ->
  Either PeerInputProblem (PeerStream.State PeerLogicalPayload)
completeAssignments completions =
  \predecessor -> foldM completeDirection predecessor (Map.toAscList completions)
  where
    completeDirection state (direction, sequences) = do
      prepared <-
        mapLeft
          PeerInputStreamProblem
          (PeerStream.prepareCompletion direction sequences state)
      Right (fst (PeerStream.commitCompletion prepared))

acknowledgementsFor ::
  Set StreamDirection ->
  PeerStream.State PeerLogicalPayload ->
  PeerStream.State PeerLogicalPayload ->
  Either PeerInputProblem [PeerAcknowledgementPlan]
acknowledgementsFor directions predecessor successor =
  traverse
    acknowledgement
    [ direction
    | direction <- Set.toAscList directions,
      not
        ( PeerStream.peerStreamPeerRetired
            (streamDirectionSource direction)
            successor
        )
    ]
  where
    acknowledgement direction = do
      watermarks <-
        mapLeft PeerInputStreamProblem (PeerStream.incomingWatermarks direction successor)
      gap <- mapLeft PeerInputStreamProblem (PeerStream.incomingGapSummary direction successor)
      repair <-
        if null (gapSummaryEntries gap)
          then Right Nothing
          else do
            previousOffer <-
              mapLeft
                PeerInputStreamProblem
                ( PeerStream.resumeOfferForPeer
                    (streamDirectionSource direction)
                    predecessor
                )
            successorOffer <-
              mapLeft
                PeerInputStreamProblem
                ( PeerStream.resumeOfferForPeer
                    (streamDirectionSource direction)
                    successor
                )
            -- Received/Completed are already carried by their cumulative
            -- acknowledgement controls.  A completion-only watermark change
            -- while the same gap remains must not trigger another full resume
            -- exchange; only new receive-frontier/gap evidence can request
            -- additional retransmission work.
            Right
              ( if resumeOfferGapSummary successorOffer
                  == resumeOfferGapSummary previousOffer
                  then Nothing
                  else Just successorOffer
              )
      Right
        ( PeerAcknowledgementPlan
            direction
            (streamReceivedPrefix watermarks)
            (streamCompletionProgress watermarks)
            gap
            repair
        )

completeReadyWaits ::
  Store.State ->
  Controlled.State ->
  SortRegistry.State ->
  Application.State ->
  Wait.State ->
  Either
    PeerInputProblem
    (Application.State, Wait.State, [Application.ApplicationWaitWake])
completeReadyWaits store controlled registry application wait = do
  case Application.applicationGatePhase application of
    Application.ApplicationGateClosed _ -> Right (application, wait, [])
    Application.ApplicationGateOpen _ -> do
      ready <- filterM registrationReady (Wait.waitRegistrations wait)
      let waitIds = fmap Wait.waitRegistrationId ready
      preparedRemoval <-
        mapLeft PeerInputWaitProblem (Wait.prepareWaitRemoval waitIds wait)
      preparedCompletions <-
        mapLeft
          PeerInputApplicationProblem
          (Application.prepareApplicationWaitCompletions waitIds application)
      let (successorApplication, wakes) =
            Application.commitApplicationWaitCompletions preparedCompletions
          (successorWait, _) = Wait.commitWaitRemoval preparedRemoval
      Right (successorApplication, successorWait, wakes)
  where
    registrationReady registration =
      or <$> traverse queryReady (Wait.waitRegistrationQueries registration)
    queryReady query =
      not . null <$> effectiveWaitMatches store controlled registry query

effectiveWaitMatches ::
  Store.State ->
  Controlled.State ->
  SortRegistry.State ->
  ResolvedQuery ->
  Either PeerInputProblem [StoreObservation.EffectiveStoreMatch]
effectiveWaitMatches store controlled registry query = do
  cut <-
    mapLeft
      PeerInputEffectiveStoreProblem
      (StoreObservation.captureEffectiveQueryCut query store)
  evidence <-
    Map.fromList
      <$> traverse bindCandidate (StoreObservation.effectiveQueryCutRawCandidates cut)
  plan <-
    mapLeft
      PeerInputEffectiveStoreProblem
      (StoreObservation.prepareEffectiveWaitPlan evidence cut)
  Right (StoreObservation.effectiveWaitMatches plan)
  where
    bindCandidate (key, publication) = do
      entry <-
        maybe
          ( Left
              ( PeerInputInvariantProblem
                  ( PeerInputReadySortMissing
                      (checkedPublicationId publication)
                      (checkedPublicationSort publication)
                  )
              )
          )
          Right
          (SortRegistry.lookupEffectiveSort (checkedPublicationSort publication) registry)
      context <-
        mapLeft
          PeerInputEffectivePublicationProblem
          ( checkEffectivePublicationContext
              (SortRegistry.registryEntryDescriptor entry)
              controlled
              publication
          )
      bound <-
        mapLeft
          PeerInputEffectiveStoreProblem
          ( StoreObservation.effectiveStoreEvidence
              publication
              (interpretEffectivePublication context)
          )
      Right (key, bound)

dependencySatisfied ::
  Maybe SuccessorStructuralBase ->
  PeerInputContext ->
  Publication.PublicationDependency ->
  SortRegistry.State ->
  GraphProgress.StructuralProgressState ->
  Either PeerInputProblem Bool
dependencySatisfied successorBase context dependency registry structuralProgress = case dependency of
  Publication.EffectiveSortDependency sortId occurrence ->
    Right
      ( case SortRegistry.lookupEffectiveSort sortId registry of
          Just _ -> True
          Nothing ->
            case SortRegistry.lookupRegularSortRetirement sortId occurrence registry of
              Just _ -> True
              Nothing -> False
      )
  Publication.DisappearanceProbeDependency probe ->
    Right
      ( case OracleProjection.oracleViewDisappearanceProbe
          probe
          (OracleProjection.oracleView context.oracle) of
          Nothing -> False
          Just _ -> True
      )
  Publication.ControlIndexDependency prerequisite ->
    Right
      ( prerequisite
          <= OracleProjection.oracleViewControlIndex
            (OracleProjection.oracleView context.oracle)
      )
  Publication.SourceTopologyDependency cut ->
    Right (sourceTopologySatisfied cut structuralProgress)
  Publication.SuccessorStructuralBaseDependency generation ->
    Right
      ( maybe
          False
          (successorStructuralBaseReleases generation)
          successorBase
      )
  Publication.StructuralApplicationDependency structuralDependency ->
    case structuralDependency of
      Reconciliation.StructuralPredecessorDependency occurrence ->
        Right
          ( Reconciliation.structuralAppliedHasOccurrence
              occurrence
              (GraphProgress.structuralProgressReconciliation structuralProgress)
          )
      _ -> Right True

filterM :: (value -> Either problem Bool) -> [value] -> Either problem [value]
filterM action = foldM one []
  where
    one retained value = do
      keep <- action value
      Right (if keep then retained <> [value] else retained)

mapLeft :: (left -> mapped) -> Either left right -> Either mapped right
mapLeft convert = either (Left . convert) Right
