{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Immutable outgoing and incoming publication evidence.
--
-- The owner allocates local publication positions and retains enough immutable
-- semantic evidence to reproduce every peer batch after stream payload release.
-- Incoming semantic identity is likewise owned here rather than independently
-- rediscovered by Store or SortRegistry.
module Eclips.Herald.Publication.State
  ( State,
    initialState,
    initialStateWithGenesis,

    -- * Outgoing publications
    HeraldPublicationPosition,
    heraldPublicationPositionWord64,
    publicationCurrentHeraldPrefix,
    OutgoingPublicationCandidate,
    proposeOutgoingPublication,
    outgoingCandidatePublicationId,
    outgoingCandidateHeraldPosition,
    OutgoingPublicationRecord,
    outgoingPublicationId,
    outgoingPublicationChecked,
    outgoingPublicationSourceProcess,
    outgoingPublicationAcceptancePosition,
    outgoingPublicationOccurrenceId,
    outgoingPublicationHeraldPosition,
    outgoingPublicationRoute,
    outgoingPublicationSourceStrength,
    outgoingPublicationSourceTopologyPrerequisite,
    outgoingPublicationControlPrerequisite,
    outgoingPublicationHasFenceDependency,
    outgoingPublicationRemoteBatches,
    outgoingPublicationDispatchCertified,
    certifyOutgoingPublicationDispatch,
    certifyStructuralPublicationDispatch,
    retireIncomingAssignmentReceipts,
    publicationAssignmentRetentionCount,
    lookupOutgoingPublication,
    outgoingPublications,
    outgoingPublicationEntries,
    PublicationPreparationError (..),
    PreparedOutgoingPublication,
    prepareOutgoingPublication,
    preparedOutgoingPublicationRecord,
    commitOutgoingPublication,

    -- * Incremental caller/object readiness
    publicationGroups,
    applicationPublicationGroupMembership,
    advancePublicationGroupCompletion,
    retirePublicationGroupPeer,

    -- * Package-internal application publication preparation
    AdmittedApplicationPublicationValue,
    admittedOrdinaryApplicationPublicationValue,
    admittedSortDefinitionApplicationPublicationValue,
    admittedApplicationPublicationValue,
    admittedApplicationPublicationSortDefinition,
    ApplicationPublicationOrigin (..),
    ApplicationPublicationRetention,
    applicationPublicationPossession,
    applicationPublicationSuppression,
    UnsequencedStructuralStage,
    unsequencedStructuralChecked,
    unsequencedStructuralSourceProcess,
    unsequencedStructuralAcceptancePosition,
    unsequencedStructuralSortOccurrenceId,
    unsequencedStructuralHeraldPosition,
    unsequencedStructuralRoute,
    unsequencedStructuralSourceStrength,
    unsequencedStructuralSourceTopologyPrerequisite,
    unsequencedStructuralControlPrerequisite,
    unsequencedStructuralRole,
    unsequencedStructuralRetention,
    ApplicationPublicationRecord,
    applicationPublicationId,
    applicationPublicationChecked,
    applicationPublicationSourceProcess,
    applicationPublicationAcceptancePosition,
    applicationPublicationSortOccurrenceId,
    applicationPublicationHeraldPosition,
    applicationPublicationRoute,
    applicationPublicationSourceStrength,
    applicationPublicationSourceTopologyPrerequisite,
    applicationPublicationControlPrerequisite,
    applicationPublicationRetention,
    applicationPublicationOrigin,
    applicationPublicationMembershipGenerationId,
    applicationPublicationTerminalResult,
    applicationPublicationAdmittedValue,
    applicationPublicationOutgoingRecord,
    applicationPublicationUnsequencedStage,
    lookupApplicationPublication,
    applicationPublicationEntries,
    unstampedApplicationStructuralStageEntries,
    StructuralSourceStage,
    structuralSourceChecked,
    structuralSourceProcess,
    structuralSourceSortOccurrenceId,
    structuralSourceHeraldPosition,
    structuralSourceRoute,
    structuralSourceStrength,
    structuralSourceTopologyPrerequisite,
    structuralSourceControlPrerequisite,
    structuralSourceRole,
    structuralSourceApplicationRecord,
    structuralSourceEnvironmentRoot,
    structuralSourceMembershipGenerationId,
    unstampedStructuralSourceStageEntries,
    ApplicationPublicationClassification (..),
    PreparedApplicationPublication,
    prepareApplicationPublication,
    prepareRetainedApplicationPublication,
    preparedApplicationPublicationClassification,
    preparedApplicationPublicationRecord,
    commitApplicationPublication,

    -- * Source structural sequencing and stabilization
    StructuralOccurrenceCandidate,
    proposeStructuralOccurrence,
    structuralCandidateOccurrence,
    structuralCandidatePredecessor,
    StampedStructuralStage,
    stampedStructuralApplicationRecord,
    stampedStructuralApplicationRecordMaybe,
    stampedStructuralEnvironmentRoot,
    stampedStructuralChecked,
    stampedStructuralSourceProcess,
    stampedStructuralSortOccurrenceId,
    stampedStructuralHeraldPosition,
    stampedStructuralRoute,
    stampedStructuralSourceStrength,
    stampedStructuralSourceTopologyPrerequisite,
    stampedStructuralControlPrerequisite,
    stampedStructuralRole,
    stampedStructuralStamp,
    stampedStructuralPeerPublications,
    stampedStructuralDispatchCertified,
    StampedStructuralClassification (..),
    PreparedStampedStructuralStage,
    prepareStampedStructuralStage,
    prepareStampedStructuralSourceStage,
    prepareStampedStructuralSourceStageAfterSuccessorBase,
    prepareStampedStructuralSourceStageAfterMembershipBase,
    preparedStampedStructuralClassification,
    preparedStampedStructuralStage,
    commitStampedStructuralStage,
    lookupStampedStructuralStage,
    stampedStructuralStageEntries,
    StructuralStabilizationRecord,
    structuralStabilizationOccurrence,
    structuralStabilizationCut,
    StructuralStabilizationClassification (..),
    PreparedStructuralStabilization,
    prepareStructuralStabilization,
    preparedStructuralStabilizationClassification,
    preparedStructuralStabilizationRecord,
    commitStructuralStabilization,
    lookupStructuralStabilization,

    -- * Package-internal private-environment ranges
    EnvironmentRootRangeClassification (..),
    PreparedEnvironmentRootRange,
    prepareEnvironmentRootRange,
    prepareEnvironmentWiringRange,
    prepareRetainedEnvironmentRootRange,
    preparedEnvironmentRootRangeClassification,
    preparedEnvironmentManifest,
    commitEnvironmentRootRange,
    lookupEnvironmentManifest,
    lookupPositionedEnvironmentRoot,
    environmentManifestEntries,
    environmentRootStageEntries,
    environmentRootPrefixSettledFor,
    publicationEnvironmentRangesWellFormed,

    -- * Incoming publications
    PublicationDependency (..),
    DestinationOutcome (..),
    IncomingDisposition (..),
    IncomingPublicationResolution,
    incomingPublicationResolution,
    incomingResolutionDependencies,
    incomingResolutionDestinationOutcomes,
    incomingResolutionDisposition,
    IncomingPublicationRecord,
    incomingPublicationDirection,
    incomingPublicationMembershipGenerationId,
    incomingPublicationDigest,
    incomingPeerPublication,
    incomingPublicationBatch,
    incomingPublicationAssignments,
    incomingPublicationDependencies,
    incomingPublicationDisappearanceGates,
    disappearanceGatedIncomingPublicationIds,
    incomingPublicationDestinationOutcomes,
    incomingPublicationDisposition,
    incomingPublicationSemanticallyAuthenticated,
    lookupIncomingPublication,
    incomingPublicationEntries,
    incomingAssignmentEntries,
    incomingDependencyWaiters,
    incomingDependencyWaiterEntries,
    IncomingPublicationClassification (..),
    IncomingPublicationProblem (..),
    PreparedIncomingPublicationCandidate,
    prepareIncomingPublicationCandidate,
    preparedIncomingPublicationClassification,
    finalizeIncomingPublication,
    PreparedIncomingPublication,
    preparedIncomingPublicationRecord,
    commitIncomingPublication,
    PreparedIncomingPublicationProgress,
    prepareIncomingPublicationProgress,
    prepareIncomingSuccessorBaseRetarget,
    preparedIncomingPublicationProgressRecord,
    commitIncomingPublicationProgress,
    PreparedIncomingPublicationRejection,
    prepareIncomingPublicationRejection,
    commitIncomingPublicationRejection,

    -- * Read-only owner witness
    PublicationStateWitness,
    publicationStateWitness,
    publicationWitnessHeraldEpoch,
    publicationWitnessNextSequences,
    publicationWitnessNextHeraldPosition,
    publicationWitnessNextStructuralSequence,
    publicationWitnessStampedStructuralStages,
    publicationWitnessStructuralStabilizations,
    publicationWitnessOutgoing,
    publicationWitnessApplicationPublications,
    publicationWitnessEnvironmentManifests,
    publicationWitnessEnvironmentRootStages,
    publicationWitnessIncoming,
    publicationWitnessIncomingAssignments,
    publicationWitnessIncomingDependencyWaiters,

    -- * Whole-state invariant test seams
    replaceOutgoingPublicationSourceProcessForInvariantTest,
    insertOutgoingPublicationRemoteBatchForInvariantTest,
    insertIncomingAssignmentOwnerForInvariantTest,
    insertIncomingDependencyWaiterForInvariantTest,
  )
where

import Control.Monad (foldM)
import Data.List (find, sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Application.Types.Forward (ForwardResult (ForwardAccepted))
import Eclips.Application.Types.Result
  ( RegularCallResult (ForwardCompleted, WriteCompleted),
  )
import Eclips.Application.Types.Write (WriteResult (WriteAccepted))
import Eclips.Domain.Alignment
  ( HeraldPublicationPosition,
    HeraldPublicationPrefix (..),
    firstHeraldPublicationPosition,
    heraldPublicationPositionPredecessor,
    heraldPublicationPositionWord64,
    nextHeraldPublicationPosition,
  )
import Eclips.Domain.Disappearance (DisappearanceProbeId)
import Eclips.Domain.Environment
  ( environmentConnectedObjectCount,
    environmentManifestRootCount,
    environmentRootSlotStructuralCarrierRole,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    ControlIndex,
    GlobalObjectId,
    GlobalUniqueId,
    HeraldEpoch,
    NablaId,
    NablaSequence,
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    StructuralOccurrenceId,
    StructuralSequence,
    TopologyCutId,
    firstStructuralSequence,
    globalObjectIdFromGlobalUniqueId,
    globalObjectIdFromNablaId,
    nablaSequence,
    nablaSequenceWord64,
    nextStructuralSequence,
    publicationId,
    publicationNablaSequence,
    publicationSourceHeraldEpoch,
    structuralOccurrenceId,
    structuralOccurrenceSourceHeraldEpoch,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    HeraldMembershipLineage,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    heraldMembershipLineageOrigin,
    heraldMembershipLineageTarget,
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
import Eclips.Domain.Route
  ( FrozenRoute,
    ReplicaStrength (..),
    destinationDelta,
    destinationHerald,
    destinationStoreIncarnation,
    destinationStrength,
    routeDestinations,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalCheckedDescriptor,
  )
import Eclips.Domain.Sort.Descriptor
  ( CheckedDescriptor,
    Lifecycle (..),
    SortKind (..),
    StructuralCarrierRole (..),
    descriptorControlledKeyProjection,
    descriptorKind,
    descriptorStructuralCarrierRole,
  )
import Eclips.Domain.Structural (StructuralVersionVector)
import Eclips.Domain.Value
  ( ValueView (GlobalUniqueIdValue),
    valueAt,
    viewValue,
  )
import Eclips.Herald.Application.Environment qualified as Environment
import Eclips.Herald.Application.PublicationEvidence
  ( AdmittedApplicationPublicationValue,
    ApplicationPublicationOrigin (..),
    admittedApplicationPublicationSortDefinition,
    admittedApplicationPublicationValue,
    admittedOrdinaryApplicationPublicationValue,
    admittedSortDefinitionApplicationPublicationValue,
  )
import Eclips.Herald.Application.Request.Internal
  ( ProcessAcceptancePosition,
    processAcceptancePositionProcess,
  )
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    checkedLocalHeraldEpoch,
  )
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
    preparedOutput,
  )
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    PublicationBatch,
    PublicationBatchProblem,
    PublicationDestination,
    StructuralOccurrenceStamp,
    mkPublicationBatch,
    peerPublicationBatch,
    peerPublicationDigest,
    peerPublicationStructuralStamp,
    presentedOrdinaryPeerPublication,
    publicationBatchDestinations,
    publicationBatchId,
    publicationDestination,
    structuralOccurrenceStampOccurrence,
    structuralOccurrenceStampPredecessor,
    structuralOccurrenceStampPublication,
  )
import Eclips.Herald.PeerStream
  ( AssignmentReceipt,
    PeerItemDigest,
    StreamDirection,
    StreamPrefix,
    StreamSequence,
    assignmentReceipt,
    assignmentReceiptDigest,
    assignmentReceiptDirection,
    assignmentReceiptSequence,
    streamDirectionDestination,
    streamDirectionSource,
  )
import Eclips.Herald.Publication.Groups qualified as Groups
import Eclips.Herald.Structural.Reconciliation (StructuralDependency)

-- | Complete immutable evidence retained for one accepted local publication.
data OutgoingPublicationRecord = OutgoingPublicationRecord
  { checked :: CheckedPublication,
    sourceProcess :: ProcessEpochId,
    acceptancePosition :: ProcessAcceptancePosition,
    occurrenceId :: SortDefinitionOccurrenceId,
    heraldPosition :: HeraldPublicationPosition,
    route :: FrozenRoute,
    sourceStrength :: ReplicaStrength,
    sourceTopologyPrerequisite :: TopologyCutId,
    controlPrerequisite :: ControlIndex,
    groupMembership :: !Groups.Membership,
    outgoingDispatchCertified :: Bool,
    remoteBatches :: Map HeraldEpoch PublicationBatch
  }
  deriving stock (Eq, Show)

outgoingPublicationId :: OutgoingPublicationRecord -> PublicationId
outgoingPublicationId record = checkedPublicationId record.checked

outgoingPublicationChecked :: OutgoingPublicationRecord -> CheckedPublication
outgoingPublicationChecked record = record.checked

outgoingPublicationSourceProcess :: OutgoingPublicationRecord -> ProcessEpochId
outgoingPublicationSourceProcess record = record.sourceProcess

outgoingPublicationAcceptancePosition ::
  OutgoingPublicationRecord ->
  ProcessAcceptancePosition
outgoingPublicationAcceptancePosition record = record.acceptancePosition

outgoingPublicationOccurrenceId ::
  OutgoingPublicationRecord ->
  SortDefinitionOccurrenceId
outgoingPublicationOccurrenceId record = record.occurrenceId

outgoingPublicationHeraldPosition ::
  OutgoingPublicationRecord ->
  HeraldPublicationPosition
outgoingPublicationHeraldPosition record = record.heraldPosition

outgoingPublicationRoute :: OutgoingPublicationRecord -> FrozenRoute
outgoingPublicationRoute record = record.route

outgoingPublicationSourceStrength ::
  OutgoingPublicationRecord ->
  ReplicaStrength
outgoingPublicationSourceStrength record = record.sourceStrength

outgoingPublicationSourceTopologyPrerequisite ::
  OutgoingPublicationRecord -> TopologyCutId
outgoingPublicationSourceTopologyPrerequisite record =
  record.sourceTopologyPrerequisite

outgoingPublicationControlPrerequisite ::
  OutgoingPublicationRecord ->
  ControlIndex
outgoingPublicationControlPrerequisite record = record.controlPrerequisite

outgoingPublicationHasFenceDependency :: OutgoingPublicationRecord -> Bool
outgoingPublicationHasFenceDependency _ = False

outgoingPublicationRemoteBatches ::
  OutgoingPublicationRecord ->
  Map HeraldEpoch PublicationBatch
outgoingPublicationRemoteBatches record = record.remoteBatches

-- | Private evidence transferred from the exact owner-prepared stream enqueue.
-- This fact lives with its semantic publication, not in a receipt-history map.
outgoingPublicationDispatchCertified :: OutgoingPublicationRecord -> Bool
outgoingPublicationDispatchCertified record = record.outgoingDispatchCertified

certifyOutgoingPublicationDispatch :: PublicationId -> Map HeraldEpoch (NonEmpty AssignmentReceipt) -> State -> Either PublicationPreparationError State
certifyOutgoingPublicationDispatch identifier assignments state = do
  record <- maybe (Left (PublicationDispatchAssignmentMismatch identifier)) Right (Map.lookup identifier state.outgoingById)
  certifyAssignments identifier state.heraldEpoch (fmap (peerPublicationDigest . presentedOrdinaryPeerPublication) record.remoteBatches) assignments
  groups <- settlePublicationGroups identifier assignments state.groups
  let certified = record {outgoingDispatchCertified = True}
      updateApplication = \case
        OrdinaryApplicationPublication origin membership admitted retention _ -> OrdinaryApplicationPublication origin membership admitted retention certified
        other -> other
  Right
    state
      { outgoingById = Map.insert identifier certified state.outgoingById,
        groups,
        applicationByAcceptance = Map.adjust updateApplication record.acceptancePosition state.applicationByAcceptance
      }

certifyStructuralPublicationDispatch :: StructuralOccurrenceId -> Map HeraldEpoch (NonEmpty AssignmentReceipt) -> State -> Either PublicationPreparationError State
certifyStructuralPublicationDispatch occurrence assignments state = do
  stage <- maybe (Left (PublicationStructuralStabilizationUnknown occurrence)) Right (Map.lookup occurrence state.stampedStructuralStages)
  certifyAssignments (checkedPublicationId (stampedStructuralChecked stage)) state.heraldEpoch (fmap peerPublicationDigest stage.peerPublications) assignments
  groups <- settlePublicationGroups (checkedPublicationId (stampedStructuralChecked stage)) assignments state.groups
  Right state {groups, stampedStructuralStages = Map.insert occurrence (stage {stampedDispatchCertified = True}) state.stampedStructuralStages}

-- The coordinator commits this certificate together with local Store effects
-- (and, for structural work, reconciliation and retained context obligations).
-- Exact retries may repeat certification after its pending local token is gone.
settlePublicationGroups :: PublicationId -> Map HeraldEpoch (NonEmpty AssignmentReceipt) -> Groups.State -> Either PublicationPreparationError Groups.State
settlePublicationGroups identifier assignments groups
  | Groups.publicationPending identifier groups =
      either
        (Left . PublicationGroupProblem)
        (Right . fst)
        (Groups.settleLocalPublication identifier (fmap (maximum . fmap assignmentReceiptSequence) assignments) groups)
  | otherwise = Right groups

certifyAssignments :: PublicationId -> HeraldEpoch -> Map HeraldEpoch PeerItemDigest -> Map HeraldEpoch (NonEmpty AssignmentReceipt) -> Either PublicationPreparationError ()
certifyAssignments identifier local expected assigned =
  requireAgreement
    (Map.keysSet expected == Map.keysSet assigned && all matches (Map.toAscList assigned))
    (PublicationDispatchAssignmentMismatch identifier)
  where
    matches (peer, receipt :| []) =
      streamDirectionSource (assignmentReceiptDirection receipt) == local
        && streamDirectionDestination (assignmentReceiptDirection receipt) == peer
        && Just (assignmentReceiptDigest receipt) == Map.lookup peer expected
    matches _ = False

-- | Only outstanding assignment links are indexed. Completion authority comes
-- from PeerStream; immutable semantic admission and destination outcomes remain.
retireIncomingAssignmentReceipts :: ((StreamDirection, StreamSequence) -> Bool) -> State -> Either IncomingPublicationProblem State
retireIncomingAssignmentReceipts completed state = do
  incoming <- foldM retire state.incomingById (Map.toAscList byPublication)
  Right
    state
      { incomingById = incoming,
        incomingAssignmentOwners = Map.withoutKeys state.incomingAssignmentOwners (Map.keysSet retiring)
      }
  where
    retiring = Map.filterWithKey (\key _ -> completed key) state.incomingAssignmentOwners
    byPublication = Map.fromListWith Set.union [(identifier, Set.singleton key) | (key, identifier) <- Map.toAscList retiring]
    -- Remove a publication's completed links together. Repeated filtering once
    -- per assignment would make a many-assignment completion quadratic.
    retire incoming (identifier, keys) = do
      record <- requireIncomingRecord identifier state
      if record.disposition == DependencyHeld
        then Left (IncomingAssignmentRetirementPending identifier)
        else
          let retained = Set.filter (\receipt -> (assignmentReceiptDirection receipt, assignmentReceiptSequence receipt) `Set.notMember` keys) record.assignments
           in Right (Map.insert identifier record {assignments = retained} incoming)

publicationAssignmentRetentionCount :: State -> Int
publicationAssignmentRetentionCount state = Map.size state.incomingAssignmentOwners

-- | Controlled-owner facts already admitted by the surrounding local
-- transaction.  The controlled arms both retain publisher possession; the
-- obsolete arm additionally requires hidden suppression before any ordinary
-- Store or peer effect can be committed.  Keeping these facts in one closed sum
-- prevents possession and suppression from being supplied independently.
data ApplicationPublicationRetention
  = RegularApplicationPublication
  | CurrentControlledApplicationPublication GlobalObjectId
  | ObsoleteControlledApplicationPublication GlobalObjectId
  deriving stock (Eq, Show)

-- | The publisher/object possession fixed by one accepted application
-- publication, if the effective descriptor is controlled.
applicationPublicationPossession ::
  ProcessEpochId ->
  ApplicationPublicationRetention ->
  Maybe (ProcessEpochId, GlobalObjectId)
applicationPublicationPossession process = \case
  RegularApplicationPublication -> Nothing
  CurrentControlledApplicationPublication object -> Just (process, object)
  ObsoleteControlledApplicationPublication object -> Just (process, object)

-- | The hidden suppression fact which must accompany an obsolete controlled
-- publication.  Current and regular publications install none.
applicationPublicationSuppression ::
  ApplicationPublicationRetention ->
  Maybe GlobalObjectId
applicationPublicationSuppression = \case
  ObsoleteControlledApplicationPublication object -> Just object
  RegularApplicationPublication -> Nothing
  CurrentControlledApplicationPublication _ -> Nothing

-- | Complete source-side state for a structurally admitted publication whose
-- prerequisites are not yet ready.  In particular this type has no structural
-- occurrence/stamp, predecessor vector, applied-vector advance, peer batch, or
-- outbox item.  Increment 3/4 will consume this state when it can authenticate
-- and sequence the occurrence.
data UnsequencedStructuralStage = UnsequencedStructuralStage
  { checked :: CheckedPublication,
    sourceProcess :: ProcessEpochId,
    acceptancePosition :: ProcessAcceptancePosition,
    occurrenceId :: SortDefinitionOccurrenceId,
    heraldPosition :: HeraldPublicationPosition,
    route :: FrozenRoute,
    sourceStrength :: ReplicaStrength,
    sourceTopologyPrerequisite :: TopologyCutId,
    controlPrerequisite :: ControlIndex,
    groupMembership :: !Groups.Membership,
    structuralRole :: StructuralCarrierRole,
    retention :: ApplicationPublicationRetention
  }
  deriving stock (Eq, Show)

unsequencedStructuralChecked :: UnsequencedStructuralStage -> CheckedPublication
unsequencedStructuralChecked stage = stage.checked

unsequencedStructuralSourceProcess ::
  UnsequencedStructuralStage -> ProcessEpochId
unsequencedStructuralSourceProcess stage = stage.sourceProcess

unsequencedStructuralAcceptancePosition ::
  UnsequencedStructuralStage -> ProcessAcceptancePosition
unsequencedStructuralAcceptancePosition stage = stage.acceptancePosition

unsequencedStructuralSortOccurrenceId ::
  UnsequencedStructuralStage -> SortDefinitionOccurrenceId
unsequencedStructuralSortOccurrenceId stage = stage.occurrenceId

unsequencedStructuralHeraldPosition ::
  UnsequencedStructuralStage -> HeraldPublicationPosition
unsequencedStructuralHeraldPosition stage = stage.heraldPosition

unsequencedStructuralRoute :: UnsequencedStructuralStage -> FrozenRoute
unsequencedStructuralRoute stage = stage.route

unsequencedStructuralSourceStrength ::
  UnsequencedStructuralStage -> ReplicaStrength
unsequencedStructuralSourceStrength stage = stage.sourceStrength

unsequencedStructuralSourceTopologyPrerequisite ::
  UnsequencedStructuralStage -> TopologyCutId
unsequencedStructuralSourceTopologyPrerequisite stage =
  stage.sourceTopologyPrerequisite

unsequencedStructuralControlPrerequisite ::
  UnsequencedStructuralStage -> ControlIndex
unsequencedStructuralControlPrerequisite stage = stage.controlPrerequisite

unsequencedStructuralRole ::
  UnsequencedStructuralStage -> StructuralCarrierRole
unsequencedStructuralRole stage = stage.structuralRole

unsequencedStructuralRetention ::
  UnsequencedStructuralStage -> ApplicationPublicationRetention
unsequencedStructuralRetention stage = stage.retention

-- | One retry-stable package-internal application publication.  Ordinary data
-- reuses the existing retained outgoing record and its exact remote batches;
-- structural data remains in the deliberately batch-free stage above.
data ApplicationPublicationRecord
  = OrdinaryApplicationPublication
      ApplicationPublicationOrigin
      HeraldMembershipGenerationId
      AdmittedApplicationPublicationValue
      ApplicationPublicationRetention
      OutgoingPublicationRecord
  | StructuralApplicationPublication
      ApplicationPublicationOrigin
      HeraldMembershipGenerationId
      AdmittedApplicationPublicationValue
      UnsequencedStructuralStage
  deriving stock (Eq, Show)

applicationPublicationId :: ApplicationPublicationRecord -> PublicationId
applicationPublicationId = checkedPublicationId . applicationPublicationChecked

applicationPublicationChecked ::
  ApplicationPublicationRecord -> CheckedPublication
applicationPublicationChecked = \case
  OrdinaryApplicationPublication _ _ _ _ record -> record.checked
  StructuralApplicationPublication _ _ _ stage -> stage.checked

applicationPublicationSourceProcess ::
  ApplicationPublicationRecord -> ProcessEpochId
applicationPublicationSourceProcess = \case
  OrdinaryApplicationPublication _ _ _ _ record -> record.sourceProcess
  StructuralApplicationPublication _ _ _ stage -> stage.sourceProcess

applicationPublicationGroupMembership :: ApplicationPublicationRecord -> Groups.Membership
applicationPublicationGroupMembership = \case
  OrdinaryApplicationPublication _ _ _ _ record -> record.groupMembership
  StructuralApplicationPublication _ _ _ stage -> stage.groupMembership

applicationPublicationAcceptancePosition ::
  ApplicationPublicationRecord -> ProcessAcceptancePosition
applicationPublicationAcceptancePosition = \case
  OrdinaryApplicationPublication _ _ _ _ record -> record.acceptancePosition
  StructuralApplicationPublication _ _ _ stage -> stage.acceptancePosition

applicationPublicationSortOccurrenceId ::
  ApplicationPublicationRecord -> SortDefinitionOccurrenceId
applicationPublicationSortOccurrenceId = \case
  OrdinaryApplicationPublication _ _ _ _ record -> record.occurrenceId
  StructuralApplicationPublication _ _ _ stage -> stage.occurrenceId

applicationPublicationHeraldPosition ::
  ApplicationPublicationRecord -> HeraldPublicationPosition
applicationPublicationHeraldPosition = \case
  OrdinaryApplicationPublication _ _ _ _ record -> record.heraldPosition
  StructuralApplicationPublication _ _ _ stage -> stage.heraldPosition

applicationPublicationRoute :: ApplicationPublicationRecord -> FrozenRoute
applicationPublicationRoute = \case
  OrdinaryApplicationPublication _ _ _ _ record -> record.route
  StructuralApplicationPublication _ _ _ stage -> stage.route

applicationPublicationSourceStrength ::
  ApplicationPublicationRecord -> ReplicaStrength
applicationPublicationSourceStrength = \case
  OrdinaryApplicationPublication _ _ _ _ record -> record.sourceStrength
  StructuralApplicationPublication _ _ _ stage -> stage.sourceStrength

applicationPublicationSourceTopologyPrerequisite ::
  ApplicationPublicationRecord -> TopologyCutId
applicationPublicationSourceTopologyPrerequisite = \case
  OrdinaryApplicationPublication _ _ _ _ record ->
    record.sourceTopologyPrerequisite
  StructuralApplicationPublication _ _ _ stage ->
    stage.sourceTopologyPrerequisite

applicationPublicationControlPrerequisite ::
  ApplicationPublicationRecord -> ControlIndex
applicationPublicationControlPrerequisite = \case
  OrdinaryApplicationPublication _ _ _ _ record -> record.controlPrerequisite
  StructuralApplicationPublication _ _ _ stage -> stage.controlPrerequisite

applicationPublicationRetention ::
  ApplicationPublicationRecord -> ApplicationPublicationRetention
applicationPublicationRetention = \case
  OrdinaryApplicationPublication _ _ _ retention _ -> retention
  StructuralApplicationPublication _ _ _ stage -> stage.retention

applicationPublicationOrigin ::
  ApplicationPublicationRecord -> ApplicationPublicationOrigin
applicationPublicationOrigin = \case
  OrdinaryApplicationPublication origin _ _ _ _ -> origin
  StructuralApplicationPublication origin _ _ _ -> origin

applicationPublicationMembershipGenerationId ::
  ApplicationPublicationRecord -> HeraldMembershipGenerationId
applicationPublicationMembershipGenerationId = \case
  OrdinaryApplicationPublication _ membership _ _ _ -> membership
  StructuralApplicationPublication _ membership _ _ -> membership

-- | Operation-specific terminal result retained independently of the live
-- application session.  The post-cut coordinator uses this after structural
-- stabilization; ending a session may erase reply correlation but cannot
-- erase which operation created the publication.
applicationPublicationTerminalResult ::
  ApplicationPublicationRecord -> RegularCallResult
applicationPublicationTerminalResult record =
  case applicationPublicationOrigin record of
    ForwardApplicationPublicationOrigin {} ->
      ForwardCompleted ForwardAccepted
    RegularApplicationPublicationOrigin -> WriteCompleted WriteAccepted
    ControlledFirstUseApplicationPublicationOrigin _ -> WriteCompleted WriteAccepted
    ControlledUpdateApplicationPublicationOrigin _ -> WriteCompleted WriteAccepted

applicationPublicationAdmittedValue ::
  ApplicationPublicationRecord -> AdmittedApplicationPublicationValue
applicationPublicationAdmittedValue = \case
  OrdinaryApplicationPublication _ _ admitted _ _ -> admitted
  StructuralApplicationPublication _ _ admitted _ -> admitted

applicationPublicationOutgoingRecord ::
  ApplicationPublicationRecord -> Maybe OutgoingPublicationRecord
applicationPublicationOutgoingRecord = \case
  OrdinaryApplicationPublication _ _ _ _ record -> Just record
  StructuralApplicationPublication {} -> Nothing

applicationPublicationUnsequencedStage ::
  ApplicationPublicationRecord -> Maybe UnsequencedStructuralStage
applicationPublicationUnsequencedStage = \case
  OrdinaryApplicationPublication {} -> Nothing
  StructuralApplicationPublication _ _ _ stage -> Just stage

-- | One explicit semantic prerequisite retaining an incoming publication.
data PublicationDependency
  = EffectiveSortDependency SortId SortDefinitionOccurrenceId
  | ControlIndexDependency ControlIndex
  | DisappearanceProbeDependency DisappearanceProbeId
  | SourceTopologyDependency TopologyCutId
  | SuccessorStructuralBaseDependency HeraldMembershipGenerationId
  | StructuralApplicationDependency StructuralDependency
  deriving stock (Eq, Ord, Show)

-- | Exact result for one destination from the immutable incoming batch.
data DestinationOutcome
  = DestinationPending
  | DestinationApplied
  | DestinationTerminallyIgnored
  deriving stock (Eq, Ord, Show)

data IncomingDisposition
  = DependencyHeld
  | Applied
  | TerminallyIgnored
  | ProtocolRejected
  deriving stock (Eq, Ord, Show)

-- | A complete proposed semantic lifecycle state.  The smart constructor
-- derives the aggregate disposition from dependency and destination facts.
data IncomingPublicationResolution = IncomingPublicationResolution
  { dependencies :: Set PublicationDependency,
    destinationOutcomes :: Map PublicationDestination DestinationOutcome,
    disposition :: IncomingDisposition
  }
  deriving stock (Eq, Show)

incomingPublicationResolution ::
  Set PublicationDependency ->
  Map PublicationDestination DestinationOutcome ->
  Either IncomingPublicationProblem IncomingPublicationResolution
incomingPublicationResolution dependencies destinationOutcomes
  | not (Set.null dependencies)
      && any (/= DestinationPending) (Map.elems destinationOutcomes) =
      Left IncomingDependencyOutcomeContradiction
  | Set.null dependencies
      && any (== DestinationPending) (Map.elems destinationOutcomes) =
      Left IncomingDependencyOutcomeContradiction
  | otherwise =
      Right
        IncomingPublicationResolution
          { dependencies,
            destinationOutcomes,
            disposition = deriveDisposition dependencies destinationOutcomes
          }

incomingResolutionDependencies ::
  IncomingPublicationResolution ->
  Set PublicationDependency
incomingResolutionDependencies resolution = resolution.dependencies

incomingResolutionDestinationOutcomes ::
  IncomingPublicationResolution ->
  Map PublicationDestination DestinationOutcome
incomingResolutionDestinationOutcomes resolution = resolution.destinationOutcomes

incomingResolutionDisposition ::
  IncomingPublicationResolution ->
  IncomingDisposition
incomingResolutionDisposition resolution = resolution.disposition

-- | One semantic publication shared by every equal stream assignment.
data IncomingPublicationRecord = IncomingPublicationRecord
  { sourceDirection :: StreamDirection,
    admittedMembershipGeneration :: HeraldMembershipGenerationId,
    digest :: PeerItemDigest,
    peerPublication :: PeerPublication,
    assignments :: Set AssignmentReceipt,
    dependencies :: Set PublicationDependency,
    destinationOutcomes :: Map PublicationDestination DestinationOutcome,
    disposition :: IncomingDisposition
  }
  deriving stock (Eq, Show)

incomingPublicationDirection :: IncomingPublicationRecord -> StreamDirection
incomingPublicationDirection record = record.sourceDirection

incomingPublicationMembershipGenerationId ::
  IncomingPublicationRecord -> HeraldMembershipGenerationId
incomingPublicationMembershipGenerationId record =
  record.admittedMembershipGeneration

incomingPublicationDigest :: IncomingPublicationRecord -> PeerItemDigest
incomingPublicationDigest record = record.digest

incomingPeerPublication :: IncomingPublicationRecord -> PeerPublication
incomingPeerPublication record = record.peerPublication

incomingPublicationBatch :: IncomingPublicationRecord -> PublicationBatch
incomingPublicationBatch record = peerPublicationBatch record.peerPublication

incomingPublicationAssignments ::
  IncomingPublicationRecord ->
  Set AssignmentReceipt
incomingPublicationAssignments record = record.assignments

incomingPublicationDependencies ::
  IncomingPublicationRecord ->
  Set PublicationDependency
incomingPublicationDependencies record = record.dependencies

-- | Exact, retained post-marker holds. These records have no destination
-- effects while any named disappearance gate remains a dependency.
incomingPublicationDisappearanceGates :: IncomingPublicationRecord -> Set DisappearanceProbeId
incomingPublicationDisappearanceGates record =
  Set.fromList
    [probe | DisappearanceProbeDependency probe <- Set.toList record.dependencies]

disappearanceGatedIncomingPublicationIds :: State -> Set PublicationId
disappearanceGatedIncomingPublicationIds state =
  Set.fromList
    [ identifier
    | (identifier, record) <- incomingPublicationEntries state,
      not (Set.null (incomingPublicationDisappearanceGates record))
    ]

incomingPublicationDestinationOutcomes ::
  IncomingPublicationRecord ->
  Map PublicationDestination DestinationOutcome
incomingPublicationDestinationOutcomes record = record.destinationOutcomes

incomingPublicationDisposition ::
  IncomingPublicationRecord ->
  IncomingDisposition
incomingPublicationDisposition record = record.disposition

-- | Whether the retained peer evidence has crossed every boundary needed to
-- authenticate its descriptor, canonical value, and routed source authority.
-- A structural record may still be held after that point on successor-base or
-- causal application ordering; those execution-only dependencies do not make
-- its immutable bytes unsafe to advertise in a terminal-source inventory.
incomingPublicationSemanticallyAuthenticated ::
  IncomingPublicationRecord ->
  Bool
incomingPublicationSemanticallyAuthenticated record =
  case record.disposition of
    Applied -> True
    TerminallyIgnored -> True
    ProtocolRejected -> False
    DependencyHeld ->
      not (Set.null record.dependencies)
        && all postAuthenticationDependency (Set.toList record.dependencies)

preAuthenticationDependency :: PublicationDependency -> Bool
preAuthenticationDependency = \case
  EffectiveSortDependency {} -> True
  ControlIndexDependency {} -> True
  DisappearanceProbeDependency {} -> True
  SourceTopologyDependency {} -> True
  SuccessorStructuralBaseDependency {} -> False
  StructuralApplicationDependency {} -> False

postAuthenticationDependency :: PublicationDependency -> Bool
postAuthenticationDependency = \case
  EffectiveSortDependency {} -> False
  ControlIndexDependency {} -> False
  DisappearanceProbeDependency {} -> False
  SourceTopologyDependency {} -> False
  SuccessorStructuralBaseDependency {} -> True
  StructuralApplicationDependency {} -> True

-- | Non-reserving source-local structural dot proposal. The caller uses this
-- exact candidate to construct the stamp and all receiver-qualified peer items;
-- only 'prepareStampedStructuralStage' advances the supply.
data StructuralOccurrenceCandidate = StructuralOccurrenceCandidate
  { occurrence :: StructuralOccurrenceId,
    predecessor :: StructuralVersionVector
  }
  deriving stock (Eq, Show)

proposeStructuralOccurrence ::
  StructuralVersionVector -> State -> StructuralOccurrenceCandidate
proposeStructuralOccurrence predecessor state =
  StructuralOccurrenceCandidate
    { occurrence = structuralOccurrenceId state.heraldEpoch state.nextStructuralSequence,
      predecessor
    }

structuralCandidateOccurrence ::
  StructuralOccurrenceCandidate -> StructuralOccurrenceId
structuralCandidateOccurrence candidate = candidate.occurrence

structuralCandidatePredecessor ::
  StructuralOccurrenceCandidate -> StructuralVersionVector
structuralCandidatePredecessor candidate = candidate.predecessor

-- | Complete immutable source evidence retained before an occurrence may be
-- reported as applied. The receiver map contains the final closed peer-item sum,
-- never an independently digestible raw batch.
data StampedStructuralStage = StampedStructuralStage
  { source :: StructuralSourceStage,
    sourceTopologyPrerequisite :: TopologyCutId,
    stamp :: StructuralOccurrenceStamp,
    stampedDispatchCertified :: Bool,
    peerPublications :: Map HeraldEpoch PeerPublication
  }
  deriving stock (Eq, Show)

stampedStructuralApplicationRecord ::
  StampedStructuralStage -> ApplicationPublicationRecord
stampedStructuralApplicationRecord stage =
  case stampedStructuralApplicationRecordMaybe stage of
    Just application -> application
    Nothing -> error "non-application stamped stage has no application record"

stampedStructuralApplicationRecordMaybe ::
  StampedStructuralStage -> Maybe ApplicationPublicationRecord
stampedStructuralApplicationRecordMaybe stage =
  structuralSourceApplicationRecord stage.source

stampedStructuralEnvironmentRoot ::
  StampedStructuralStage ->
  Maybe (Environment.EnvironmentManifest, Environment.PositionedEnvironmentRoot)
stampedStructuralEnvironmentRoot stage =
  structuralSourceEnvironmentRoot stage.source

stampedStructuralChecked :: StampedStructuralStage -> CheckedPublication
stampedStructuralChecked = structuralSourceChecked . (.source)

stampedStructuralSourceProcess :: StampedStructuralStage -> ProcessEpochId
stampedStructuralSourceProcess = structuralSourceProcess . (.source)

stampedStructuralSortOccurrenceId ::
  StampedStructuralStage -> SortDefinitionOccurrenceId
stampedStructuralSortOccurrenceId = structuralSourceSortOccurrenceId . (.source)

stampedStructuralHeraldPosition ::
  StampedStructuralStage -> HeraldPublicationPosition
stampedStructuralHeraldPosition = structuralSourceHeraldPosition . (.source)

stampedStructuralRoute :: StampedStructuralStage -> FrozenRoute
stampedStructuralRoute = structuralSourceRoute . (.source)

stampedStructuralSourceStrength :: StampedStructuralStage -> ReplicaStrength
stampedStructuralSourceStrength = structuralSourceStrength . (.source)

stampedStructuralSourceTopologyPrerequisite ::
  StampedStructuralStage -> TopologyCutId
stampedStructuralSourceTopologyPrerequisite stage =
  stage.sourceTopologyPrerequisite

stampedStructuralControlPrerequisite :: StampedStructuralStage -> ControlIndex
stampedStructuralControlPrerequisite = structuralSourceControlPrerequisite . (.source)

stampedStructuralRole :: StampedStructuralStage -> StructuralCarrierRole
stampedStructuralRole = structuralSourceRole . (.source)

stampedStructuralStamp :: StampedStructuralStage -> StructuralOccurrenceStamp
stampedStructuralStamp stage = stage.stamp

stampedStructuralPeerPublications ::
  StampedStructuralStage -> Map HeraldEpoch PeerPublication
stampedStructuralPeerPublications stage = stage.peerPublications

stampedStructuralDispatchCertified :: StampedStructuralStage -> Bool
stampedStructuralDispatchCertified stage = stage.stampedDispatchCertified

data StructuralStabilizationRecord = StructuralStabilizationRecord
  { occurrence :: StructuralOccurrenceId,
    cut :: TopologyCutId
  }
  deriving stock (Eq, Show)

structuralStabilizationOccurrence ::
  StructuralStabilizationRecord -> StructuralOccurrenceId
structuralStabilizationOccurrence record = record.occurrence

structuralStabilizationCut :: StructuralStabilizationRecord -> TopologyCutId
structuralStabilizationCut record = record.cut

data State = State
  { heraldEpoch :: HeraldEpoch,
    nextSequenceByTenure :: Map (NablaId, AuthorityEpoch) NablaSequence,
    nextStructuralSequence :: StructuralSequence,
    nextHeraldPosition :: HeraldPublicationPosition,
    outgoingById :: Map PublicationId OutgoingPublicationRecord,
    applicationByAcceptance ::
      Map ProcessAcceptancePosition ApplicationPublicationRecord,
    environmentByAcceptance ::
      Map Environment.EnvironmentManifestKey Environment.EnvironmentManifest,
    environmentRootsById ::
      Map PublicationId Environment.PositionedEnvironmentRoot,
    stampedStructuralStages ::
      Map StructuralOccurrenceId StampedStructuralStage,
    structuralStabilizations ::
      Map StructuralOccurrenceId StructuralStabilizationRecord,
    incomingById :: Map PublicationId IncomingPublicationRecord,
    incomingAssignmentOwners ::
      Map (StreamDirection, StreamSequence) PublicationId,
    dependencyWaiters :: Map PublicationDependency (Set PublicationId),
    groups :: !Groups.State
  }
  deriving stock (Eq)

-- | Initialize one Herald owner.  New authority tenures begin at sequence zero
-- when first proposed; the Herald-wide position begins at one.
initialState :: HeraldEpoch -> State
initialState herald =
  State
    { heraldEpoch = herald,
      nextSequenceByTenure = Map.empty,
      nextStructuralSequence = firstStructuralSequence,
      nextHeraldPosition = firstHeraldPublicationPosition,
      outgoingById = Map.empty,
      applicationByAcceptance = Map.empty,
      environmentByAcceptance = Map.empty,
      environmentRootsById = Map.empty,
      stampedStructuralStages = Map.empty,
      structuralStabilizations = Map.empty,
      incomingById = Map.empty,
      incomingAssignmentOwners = Map.empty,
      dependencyWaiters = Map.empty,
      groups = Groups.initialState
    }

publicationGroups :: State -> Groups.State
publicationGroups state = state.groups

-- Sparse completion remains a transport concern. Label readiness deliberately
-- consumes only its conservative contiguous prefix, without retaining receipts.
advancePublicationGroupCompletion :: HeraldEpoch -> StreamPrefix -> State -> State
advancePublicationGroupCompletion peer prefix state =
  state {groups = fst (Groups.advanceCompletedPrefix peer prefix state.groups)}

retirePublicationGroupPeer :: HeraldEpoch -> State -> State
retirePublicationGroupPeer peer state =
  state {groups = fst (Groups.retirePeer peer state.groups)}

-- | Inclusive cut of every publication position already allocated by this
-- owner.  The explicit empty prefix is the only state before position one;
-- numeric zero is never overloaded as a publication identity.
publicationCurrentHeraldPrefix :: State -> HeraldPublicationPrefix
publicationCurrentHeraldPrefix state =
  maybe
    EmptyHeraldPublicationPrefix
    HeraldPublicationPrefixThrough
    (heraldPublicationPositionPredecessor state.nextHeraldPosition)

-- | Initialize the ordinary publication supply for the resident Herald.
initialStateWithGenesis :: CheckedHeraldGenesis -> State
initialStateWithGenesis = initialState . checkedLocalHeraldEpoch

-- | An uncommitted identity proposal tied to one exact owner predecessor.
-- Proposing it reserves neither counter.
data OutgoingPublicationCandidate = OutgoingPublicationCandidate
  { tenure :: (NablaId, AuthorityEpoch),
    publication :: PublicationId,
    heraldPosition :: HeraldPublicationPosition
  }
  deriving stock (Eq, Show)

proposeOutgoingPublication ::
  NablaId ->
  AuthorityEpoch ->
  State ->
  OutgoingPublicationCandidate
proposeOutgoingPublication nabla authority state =
  OutgoingPublicationCandidate
    { tenure = key,
      publication =
        publicationId
          nabla
          authority
          state.heraldEpoch
          (nextSequence key state),
      heraldPosition = state.nextHeraldPosition
    }
  where
    key = (nabla, authority)

outgoingCandidatePublicationId :: OutgoingPublicationCandidate -> PublicationId
outgoingCandidatePublicationId candidate = candidate.publication

outgoingCandidateHeraldPosition ::
  OutgoingPublicationCandidate ->
  HeraldPublicationPosition
outgoingCandidateHeraldPosition candidate = candidate.heraldPosition

lookupOutgoingPublication ::
  PublicationId ->
  State ->
  Maybe OutgoingPublicationRecord
lookupOutgoingPublication identifier state =
  Map.lookup identifier state.outgoingById

outgoingPublications :: State -> [OutgoingPublicationRecord]
outgoingPublications state = Map.elems state.outgoingById

outgoingPublicationEntries ::
  State ->
  [(PublicationId, OutgoingPublicationRecord)]
outgoingPublicationEntries state = Map.toAscList state.outgoingById

lookupApplicationPublication ::
  ProcessAcceptancePosition ->
  State ->
  Maybe ApplicationPublicationRecord
lookupApplicationPublication position state =
  Map.lookup position state.applicationByAcceptance

applicationPublicationEntries ::
  State ->
  [(ProcessAcceptancePosition, ApplicationPublicationRecord)]
applicationPublicationEntries state =
  Map.toAscList state.applicationByAcceptance

-- | Accepted application structural stages which do not yet own a source dot,
-- ordered by the global Herald acceptance position. A held earlier stage does
-- not reserve the structural sequence; the coordinator may therefore select
-- the first currently-ready entry from this complete order.
unstampedApplicationStructuralStageEntries ::
  State -> [(ProcessAcceptancePosition, UnsequencedStructuralStage)]
unstampedApplicationStructuralStageEntries state =
  sortOn
    (unsequencedStructuralHeraldPosition . snd)
    [ (position, stage)
    | (position, application) <- Map.toAscList state.applicationByAcceptance,
      Just stage <- [applicationPublicationUnsequencedStage application],
      findStampedAcceptance position state == Nothing
    ]

-- | Every locally positioned structural source which has not yet consumed a
-- structural sequence. Application and private-environment sources share
-- this closed internal sum so readiness is chosen
-- by one Herald-wide publication-position order.
data StructuralSourceStage
  = ApplicationStructuralSourceStage ApplicationPublicationRecord
  | EnvironmentRootStructuralSourceStage
      Environment.EnvironmentManifest
      Environment.PositionedEnvironmentRoot
  deriving stock (Eq, Show)

structuralSourceChecked :: StructuralSourceStage -> CheckedPublication
structuralSourceChecked = \case
  ApplicationStructuralSourceStage application ->
    applicationPublicationChecked application
  EnvironmentRootStructuralSourceStage _ root ->
    Environment.positionedEnvironmentRootChecked root

structuralSourceProcess :: StructuralSourceStage -> ProcessEpochId
structuralSourceProcess = \case
  ApplicationStructuralSourceStage application ->
    applicationPublicationSourceProcess application
  EnvironmentRootStructuralSourceStage manifest _ ->
    Environment.environmentManifestProcess manifest

structuralSourceSortOccurrenceId ::
  StructuralSourceStage -> SortDefinitionOccurrenceId
structuralSourceSortOccurrenceId = \case
  ApplicationStructuralSourceStage application ->
    applicationPublicationSortOccurrenceId application
  EnvironmentRootStructuralSourceStage _ root ->
    Environment.environmentRootPlanOccurrenceId
      (Environment.positionedEnvironmentRootPlan root)

structuralSourceHeraldPosition ::
  StructuralSourceStage -> HeraldPublicationPosition
structuralSourceHeraldPosition = \case
  ApplicationStructuralSourceStage application ->
    applicationPublicationHeraldPosition application
  EnvironmentRootStructuralSourceStage _ root ->
    Environment.positionedEnvironmentRootHeraldPosition root

structuralSourceRoute :: StructuralSourceStage -> FrozenRoute
structuralSourceRoute = \case
  ApplicationStructuralSourceStage application ->
    applicationPublicationRoute application
  EnvironmentRootStructuralSourceStage _ root ->
    Environment.environmentRootPlanRoute
      (Environment.positionedEnvironmentRootPlan root)

-- | Retained ordinary source authority determines publication strength.
structuralSourceStrength :: StructuralSourceStage -> ReplicaStrength
structuralSourceStrength = \case
  ApplicationStructuralSourceStage application ->
    applicationPublicationSourceStrength application
  EnvironmentRootStructuralSourceStage _ root ->
    Environment.environmentRootPlanSourceStrength
      (Environment.positionedEnvironmentRootPlan root)

-- | Application and environment sources freeze their prerequisite at admission.
structuralSourceTopologyPrerequisite ::
  StructuralSourceStage -> Maybe TopologyCutId
structuralSourceTopologyPrerequisite = \case
  ApplicationStructuralSourceStage application ->
    Just (applicationPublicationSourceTopologyPrerequisite application)
  EnvironmentRootStructuralSourceStage _ root ->
    Just
      ( Environment.environmentRootPlanSourceTopologyPrerequisite
          (Environment.positionedEnvironmentRootPlan root)
      )

structuralSourceControlPrerequisite :: StructuralSourceStage -> ControlIndex
structuralSourceControlPrerequisite = \case
  ApplicationStructuralSourceStage application ->
    applicationPublicationControlPrerequisite application
  EnvironmentRootStructuralSourceStage _ root ->
    Environment.environmentRootPlanControlPrerequisite
      (Environment.positionedEnvironmentRootPlan root)

structuralSourceRole :: StructuralSourceStage -> StructuralCarrierRole
structuralSourceRole = \case
  ApplicationStructuralSourceStage application ->
    case applicationPublicationUnsequencedStage application of
      Just stage -> unsequencedStructuralRole stage
      Nothing -> error "non-structural application entered structural source stage"
  EnvironmentRootStructuralSourceStage _ root ->
    environmentRootSlotStructuralCarrierRole
      ( Environment.environmentRootPlanSlot
          (Environment.positionedEnvironmentRootPlan root)
      )

structuralSourceApplicationRecord ::
  StructuralSourceStage -> Maybe ApplicationPublicationRecord
structuralSourceApplicationRecord = \case
  ApplicationStructuralSourceStage application -> Just application
  EnvironmentRootStructuralSourceStage {} -> Nothing

-- | Narrow source-arm view used by structural progress and settlement.  The
-- pair is returned only when the root remains tied to its complete immutable
-- manifest; callers never reconstruct membership or request provenance from a
-- publication identifier.
structuralSourceEnvironmentRoot ::
  StructuralSourceStage ->
  Maybe (Environment.EnvironmentManifest, Environment.PositionedEnvironmentRoot)
structuralSourceEnvironmentRoot = \case
  ApplicationStructuralSourceStage _ -> Nothing
  EnvironmentRootStructuralSourceStage manifest root -> Just (manifest, root)

-- | Admission-time membership for application and environment sources.
-- Environment roots retain the exact manifest generation across a supported
-- successor projection.
structuralSourceMembershipGenerationId ::
  StructuralSourceStage -> Maybe HeraldMembershipGenerationId
structuralSourceMembershipGenerationId = \case
  ApplicationStructuralSourceStage application ->
    Just (applicationPublicationMembershipGenerationId application)
  EnvironmentRootStructuralSourceStage manifest _ ->
    Just (Environment.environmentManifestMembershipGenerationId manifest)

unstampedStructuralSourceStageEntries :: State -> [StructuralSourceStage]
unstampedStructuralSourceStageEntries state =
  sortOn
    structuralSourceHeraldPosition
    (applicationSources <> environmentSources)
  where
    applicationSources =
      [ ApplicationStructuralSourceStage application
      | application <- Map.elems state.applicationByAcceptance,
        applicationPublicationUnsequencedStage application /= Nothing,
        findStampedApplication application state == Nothing
      ]
    environmentSources =
      [ EnvironmentRootStructuralSourceStage manifest root
      | manifest <- Map.elems state.environmentByAcceptance,
        root <- NonEmpty.toList (Environment.environmentManifestRoots manifest),
        findStampedEnvironmentRoot root state == Nothing
      ]

lookupIncomingPublication ::
  PublicationId ->
  State ->
  Maybe IncomingPublicationRecord
lookupIncomingPublication identifier state =
  Map.lookup identifier state.incomingById

incomingPublicationEntries ::
  State ->
  [(PublicationId, IncomingPublicationRecord)]
incomingPublicationEntries state = Map.toAscList state.incomingById

incomingAssignmentEntries ::
  State ->
  [((StreamDirection, StreamSequence), PublicationId)]
incomingAssignmentEntries state = Map.toAscList state.incomingAssignmentOwners

-- | Canonical semantic publications waiting on one exact dependency.
incomingDependencyWaiters ::
  PublicationDependency ->
  State ->
  Set PublicationId
incomingDependencyWaiters dependency state =
  Map.findWithDefault Set.empty dependency state.dependencyWaiters

incomingDependencyWaiterEntries ::
  State ->
  [(PublicationDependency, Set PublicationId)]
incomingDependencyWaiterEntries state = Map.toAscList state.dependencyWaiters

data PublicationPreparationError
  = PublicationCandidateStale
      PublicationId
      HeraldPublicationPosition
      PublicationId
      HeraldPublicationPosition
  | PublicationCheckedIdentityMismatch PublicationId PublicationId
  | PublicationSourcePositionMismatch ProcessEpochId ProcessEpochId
  | PublicationRemoteBatchConstructionFault PublicationBatchProblem
  | PublicationApplicationValueRejected PublicationError
  | PublicationApplicationAcceptanceConflict
      ProcessAcceptancePosition
      PublicationId
  | PublicationApplicationAcceptanceMissing ProcessAcceptancePosition
  | PublicationApplicationNotStructural ProcessAcceptancePosition
  | PublicationStructuralCandidateStale
      StructuralOccurrenceId
      StructuralOccurrenceId
  | PublicationStructuralPredecessorMismatch
  | PublicationStructuralSourceMismatch HeraldEpoch HeraldEpoch
  | PublicationStructuralStampPublicationMismatch PublicationId PublicationId
  | PublicationStructuralPeerSetMismatch [HeraldEpoch] [HeraldEpoch]
  | PublicationStructuralLocalHeraldNotMember HeraldEpoch
  | PublicationStructuralPeerStampMismatch HeraldEpoch
  | PublicationStructuralPeerPublicationMismatch
      HeraldEpoch
      PublicationId
      PublicationId
  | PublicationStructuralStageConflict StructuralOccurrenceId
  | PublicationStructuralAcceptanceAlreadyStamped
      ProcessAcceptancePosition
      StructuralOccurrenceId
  | PublicationEnvironmentRootAlreadyStamped
      PublicationId
      StructuralOccurrenceId
  | PublicationStructuralStabilizationUnknown StructuralOccurrenceId
  | PublicationStructuralStabilizationConflict
      StructuralOccurrenceId
      TopologyCutId
      TopologyCutId
  | PublicationApplicationOriginContradiction
      ApplicationPublicationOrigin
      ApplicationPublicationRetention
  | PublicationApplicationSourceStrengthContradiction
      ApplicationPublicationOrigin
      ReplicaStrength
  | PublicationControlledObjectKeyContradiction
  | PublicationEnvironmentManifestProblem Environment.EnvironmentManifestProblem
  | PublicationEnvironmentAcceptanceConflict Environment.EnvironmentManifestKey
  | PublicationEnvironmentAcceptanceMissing Environment.EnvironmentManifestKey
  | PublicationEnvironmentAcceptanceOccupied ProcessAcceptancePosition
  | PublicationEnvironmentSourceEqualsTarget GlobalObjectId
  | PublicationEnvironmentSourceStrengthMismatch ReplicaStrength
  | PublicationEnvironmentMandatoryRouteMissing [HeraldEpoch]
  | PublicationEnvironmentSourceSequenceNotPositive NablaId AuthorityEpoch
  | PublicationEnvironmentLocalHeraldNotMember HeraldEpoch
  | PublicationEnvironmentRouteMembershipMismatch [HeraldEpoch] [HeraldEpoch]
  | PublicationEnvironmentValueRejected PublicationError
  | PublicationEnvironmentPublicationConflict PublicationId
  | PublicationDispatchAssignmentMismatch PublicationId
  | PublicationGroupProblem Groups.Problem
  deriving stock (Eq, Show)

newtype PreparedOutgoingPublication
  = PreparedOutgoingPublication
      (Prepared State OutgoingPublicationRecord)

-- | Atomically retain a complete local publication and every normalized remote
-- batch derived from its one frozen route.
prepareOutgoingPublication ::
  OutgoingPublicationCandidate ->
  CheckedPublication ->
  ProcessEpochId ->
  ProcessAcceptancePosition ->
  SortDefinitionOccurrenceId ->
  ReplicaStrength ->
  TopologyCutId ->
  ControlIndex ->
  FrozenRoute ->
  State ->
  Either PublicationPreparationError PreparedOutgoingPublication
prepareOutgoingPublication
  candidate
  checked
  sourceProcess
  acceptancePosition
  occurrence
  sourceStrength
  sourceTopologyPrerequisite
  controlPrerequisite
  route
  state =
    PreparedOutgoingPublication
      <$> prepareTransition prepare state
    where
      prepare predecessor = do
        let currentCandidate =
              uncurry proposeOutgoingPublication candidate.tenure predecessor
            expectedIdentifier = candidate.publication
            actualIdentifier = currentCandidate.publication
            expectedHeraldPosition = candidate.heraldPosition
            actualHeraldPosition = currentCandidate.heraldPosition
            checkedIdentifier = checkedPublicationId checked
            positionedProcess =
              processAcceptancePositionProcess acceptancePosition
        requireAgreement
          (currentCandidate == candidate)
          ( PublicationCandidateStale
              expectedIdentifier
              expectedHeraldPosition
              actualIdentifier
              actualHeraldPosition
          )
        requireAgreement
          (checkedIdentifier == expectedIdentifier)
          ( PublicationCheckedIdentityMismatch
              expectedIdentifier
              checkedIdentifier
          )
        requireAgreement
          (positionedProcess == sourceProcess)
          ( PublicationSourcePositionMismatch
              sourceProcess
              positionedProcess
          )
        remoteBatches <-
          makeRemoteBatches
            predecessor.heraldEpoch
            checked
            sourceProcess
            occurrence
            sourceStrength
            sourceTopologyPrerequisite
            controlPrerequisite
            route
        let record =
              OutgoingPublicationRecord
                { checked,
                  sourceProcess,
                  acceptancePosition,
                  occurrenceId = occurrence,
                  heraldPosition = expectedHeraldPosition,
                  route,
                  sourceStrength,
                  sourceTopologyPrerequisite,
                  controlPrerequisite,
                  groupMembership = Groups.membership sourceProcess Nothing Nothing,
                  outgoingDispatchCertified = Map.null remoteBatches,
                  remoteBatches
                }
            successor =
              predecessor
                { nextSequenceByTenure =
                    Map.insert
                      candidate.tenure
                      (advanceNablaSequence (nextSequence candidate.tenure predecessor))
                      predecessor.nextSequenceByTenure,
                  nextHeraldPosition =
                    advanceHeraldPosition predecessor.nextHeraldPosition,
                  outgoingById =
                    Map.insert
                      expectedIdentifier
                      record
                      predecessor.outgoingById
                }
        Right (successor, record)

preparedOutgoingPublicationRecord ::
  PreparedOutgoingPublication ->
  OutgoingPublicationRecord
preparedOutgoingPublicationRecord (PreparedOutgoingPublication prepared) =
  preparedOutput prepared

commitOutgoingPublication ::
  PreparedOutgoingPublication ->
  (State, OutgoingPublicationRecord)
commitOutgoingPublication (PreparedOutgoingPublication prepared) =
  commitPrepared prepared

data ApplicationPublicationClassification
  = ApplicationPublicationFirstAccepted
  | ApplicationPublicationExactRetry
  deriving stock (Eq, Ord, Show)

newtype PreparedApplicationPublication
  = PreparedApplicationPublication
      ( Prepared
          State
          (ApplicationPublicationClassification, ApplicationPublicationRecord)
      )

-- | Prepare the package-internal sole-schema publication after application
-- value globalization.  A first acceptance admits the descriptor/value pair,
-- allocates one stable publication identity and Herald position, and either
-- creates an ordinary retained outgoing record or the deliberately unsequenced
-- structural stage.  An exact retry is found by process acceptance position and
-- returns the retained record without advancing either supply.
prepareApplicationPublication ::
  NablaId ->
  Maybe GlobalObjectId ->
  AuthorityEpoch ->
  CanonicalDescriptor ->
  AdmittedApplicationPublicationValue ->
  ApplicationPublicationOrigin ->
  HeraldMembershipGenerationId ->
  ProcessEpochId ->
  ProcessAcceptancePosition ->
  SortDefinitionOccurrenceId ->
  ReplicaStrength ->
  TopologyCutId ->
  ControlIndex ->
  FrozenRoute ->
  State ->
  Either PublicationPreparationError PreparedApplicationPublication
prepareApplicationPublication
  nabla
  sequencingObject
  authority
  descriptor
  admitted
  origin
  membership
  sourceProcess
  acceptancePosition
  occurrence
  sourceStrength
  sourceTopologyPrerequisite
  controlPrerequisite
  route
  state = do
    let positionedProcess = processAcceptancePositionProcess acceptancePosition
    requireAgreement
      (positionedProcess == sourceProcess)
      ( PublicationSourcePositionMismatch
          sourceProcess
          positionedProcess
      )
    PreparedApplicationPublication <$> prepareTransition prepare state
    where
      value = admittedApplicationPublicationValue admitted

      prepare predecessor = case Map.lookup acceptancePosition predecessor.applicationByAcceptance of
        Just incumbent ->
          Left
            ( PublicationApplicationAcceptanceConflict
                acceptancePosition
                (applicationPublicationId incumbent)
            )
        Nothing -> prepareFirst predecessor

      prepareFirst predecessor = do
        let candidate = proposeOutgoingPublication nabla authority predecessor
        checked <-
          either
            (Left . PublicationApplicationValueRejected)
            Right
            (mkCheckedPublication descriptor candidate.publication value)
        retention <-
          deriveApplicationPublicationRetention
            (canonicalCheckedDescriptor descriptor)
            checked
        validateApplicationPublicationOrigin origin retention
        validateApplicationPublicationSourceStrength origin sourceStrength
        let groupMembership =
              Groups.membership
                sourceProcess
                sequencingObject
                (snd <$> applicationPublicationPossession sourceProcess retention)
        record <-
          case descriptorStructuralCarrierRole
            (canonicalCheckedDescriptor descriptor) of
            Nothing -> prepareOrdinary predecessor candidate checked retention
            Just ProcessEpochCarrier ->
              prepareOrdinary predecessor candidate checked retention
            Just structuralRole ->
              Right
                ( StructuralApplicationPublication
                    origin
                    membership
                    admitted
                    UnsequencedStructuralStage
                      { checked,
                        sourceProcess,
                        acceptancePosition,
                        occurrenceId = occurrence,
                        heraldPosition = candidate.heraldPosition,
                        route,
                        sourceStrength,
                        sourceTopologyPrerequisite,
                        controlPrerequisite,
                        groupMembership,
                        structuralRole,
                        retention
                      }
                )
        groups <-
          either
            (Left . PublicationGroupProblem)
            Right
            ( Groups.acceptPublication
                (applicationPublicationId record)
                groupMembership
                (applicationPublicationUnsequencedStage record /= Nothing)
                predecessor.groups
            )
        let withOutgoing = case applicationPublicationOutgoingRecord record of
              Nothing -> predecessor.outgoingById
              Just outgoing ->
                Map.insert
                  (applicationPublicationId record)
                  outgoing
                  predecessor.outgoingById
            successor =
              predecessor
                { nextSequenceByTenure =
                    Map.insert
                      candidate.tenure
                      (advanceNablaSequence (nextSequence candidate.tenure predecessor))
                      predecessor.nextSequenceByTenure,
                  nextHeraldPosition =
                    advanceHeraldPosition predecessor.nextHeraldPosition,
                  outgoingById = withOutgoing,
                  groups,
                  applicationByAcceptance =
                    Map.insert
                      acceptancePosition
                      record
                      predecessor.applicationByAcceptance
                }
        Right
          ( successor,
            (ApplicationPublicationFirstAccepted, record)
          )

      prepareOrdinary predecessor candidate checked retention = do
        remoteBatches <-
          makeRemoteBatches
            predecessor.heraldEpoch
            checked
            sourceProcess
            occurrence
            sourceStrength
            sourceTopologyPrerequisite
            controlPrerequisite
            route
        let outgoing =
              OutgoingPublicationRecord
                { checked,
                  sourceProcess,
                  acceptancePosition,
                  occurrenceId = occurrence,
                  heraldPosition = candidate.heraldPosition,
                  route,
                  sourceStrength,
                  sourceTopologyPrerequisite,
                  controlPrerequisite,
                  groupMembership =
                    Groups.membership
                      sourceProcess
                      sequencingObject
                      (snd <$> applicationPublicationPossession sourceProcess retention),
                  outgoingDispatchCertified = Map.null remoteBatches,
                  remoteBatches
                }
        Right
          ( OrdinaryApplicationPublication
              origin
              membership
              admitted
              retention
              outgoing
          )

-- | Recover an already accepted semantic publication only after the opaque
-- Application owner has classified the exact private typed call as a retry.
-- Publication retains no raw application request identity and performs no
-- retry/conflict classification of its own.
prepareRetainedApplicationPublication ::
  ProcessAcceptancePosition ->
  State ->
  Either PublicationPreparationError PreparedApplicationPublication
prepareRetainedApplicationPublication
  acceptancePosition
  state =
    PreparedApplicationPublication <$> prepareTransition prepare state
    where
      prepare predecessor = case Map.lookup acceptancePosition predecessor.applicationByAcceptance of
        Nothing ->
          Left (PublicationApplicationAcceptanceMissing acceptancePosition)
        Just incumbent ->
          Right
            ( predecessor,
              (ApplicationPublicationExactRetry, incumbent)
            )

preparedApplicationPublicationClassification ::
  PreparedApplicationPublication -> ApplicationPublicationClassification
preparedApplicationPublicationClassification
  (PreparedApplicationPublication prepared) = fst (preparedOutput prepared)

preparedApplicationPublicationRecord ::
  PreparedApplicationPublication -> ApplicationPublicationRecord
preparedApplicationPublicationRecord
  (PreparedApplicationPublication prepared) = snd (preparedOutput prepared)

commitApplicationPublication ::
  PreparedApplicationPublication ->
  ( State,
    ApplicationPublicationClassification,
    ApplicationPublicationRecord
  )
commitApplicationPublication (PreparedApplicationPublication prepared) =
  let (state, (classification, record)) = commitPrepared prepared
   in (state, classification, record)

data StampedStructuralClassification
  = StampedStructuralFirstRetained
  | StampedStructuralExactRetry
  deriving stock (Eq, Ord, Show)

newtype PreparedStampedStructuralStage
  = PreparedStampedStructuralStage
      ( Prepared
          State
          (StampedStructuralClassification, StampedStructuralStage)
      )

-- | Atomically consume the current source-local structural candidate and
-- retain its complete final receiver-qualified peer items. Constructing a
-- candidate or its stamp reserves nothing; an exact retry advances nothing.
prepareStampedStructuralStage ::
  ProcessAcceptancePosition ->
  StructuralOccurrenceStamp ->
  Map HeraldEpoch PeerPublication ->
  State ->
  Either PublicationPreparationError PreparedStampedStructuralStage
prepareStampedStructuralStage acceptancePosition stamp peerPublications state =
  case Map.lookup acceptancePosition state.applicationByAcceptance of
    Nothing -> Left (PublicationApplicationAcceptanceMissing acceptancePosition)
    Just application -> do
      _ <-
        maybe
          (Left (PublicationApplicationNotStructural acceptancePosition))
          Right
          (applicationPublicationUnsequencedStage application)
      prepareStampedStructuralSourceStage
        (ApplicationStructuralSourceStage application)
        (applicationPublicationSourceTopologyPrerequisite application)
        stamp
        peerPublications
        state

-- | Shared stamped-source admission. Every source repeats its already
-- frozen topology prerequisite exactly.
prepareStampedStructuralSourceStage ::
  StructuralSourceStage ->
  TopologyCutId ->
  StructuralOccurrenceStamp ->
  Map HeraldEpoch PeerPublication ->
  State ->
  Either PublicationPreparationError PreparedStampedStructuralStage
prepareStampedStructuralSourceStage source sourceTopology stamp peerPublications state =
  prepareStampedStructuralSourceStageWithMembership
    Nothing
    source
    sourceTopology
    stamp
    peerPublications
    state

-- | Stamp a source only for the exact active membership already checked
-- against an installed successor base by the structural-progress coordinator.
-- The immutable frozen route remains retained, while only destinations hosted
-- by active successor Heralds become peer publications.
prepareStampedStructuralSourceStageAfterSuccessorBase ::
  HeraldMembershipGeneration ->
  StructuralSourceStage ->
  TopologyCutId ->
  StructuralOccurrenceStamp ->
  Map HeraldEpoch PeerPublication ->
  State ->
  Either PublicationPreparationError PreparedStampedStructuralStage
prepareStampedStructuralSourceStageAfterSuccessorBase membership =
  prepareStampedStructuralSourceStageAfterMembershipBase membership

-- | Stamp under a checked installed generation, including admission successors.
-- Mandatory system-view delivery covers every current member; the source's
-- original route and structural provenance remain immutable.
prepareStampedStructuralSourceStageAfterMembershipBase ::
  HeraldMembershipGeneration ->
  StructuralSourceStage ->
  TopologyCutId ->
  StructuralOccurrenceStamp ->
  Map HeraldEpoch PeerPublication ->
  State ->
  Either PublicationPreparationError PreparedStampedStructuralStage
prepareStampedStructuralSourceStageAfterMembershipBase membership =
  prepareStampedStructuralSourceStageWithMembership
    ( Just
        ( Set.fromList
            (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership))
        )
    )

prepareStampedStructuralSourceStageWithMembership ::
  Maybe (Set HeraldEpoch) ->
  StructuralSourceStage ->
  TopologyCutId ->
  StructuralOccurrenceStamp ->
  Map HeraldEpoch PeerPublication ->
  State ->
  Either PublicationPreparationError PreparedStampedStructuralStage
prepareStampedStructuralSourceStageWithMembership activeHeralds source sourceTopology stamp peerPublications state =
  PreparedStampedStructuralStage <$> prepareTransition prepare state
  where
    prepare predecessor = do
      validateRetainedStructuralSource source predecessor
      case activeHeralds of
        Just active ->
          requireAgreement
            (Set.member predecessor.heraldEpoch active)
            (PublicationStructuralLocalHeraldNotMember predecessor.heraldEpoch)
        Nothing -> Right ()
      case structuralSourceTopologyPrerequisite source of
        Just expected ->
          requireAgreement
            (sourceTopology == expected)
            PublicationStructuralPredecessorMismatch
        Nothing -> Right ()
      let occurrence = structuralOccurrenceStampOccurrence stamp
          currentCandidate =
            proposeStructuralOccurrence
              (structuralOccurrenceStampPredecessor stamp)
              predecessor
          expectedOccurrence = currentCandidate.occurrence
          expectedPublication = checkedPublicationId (structuralSourceChecked source)
          observedPublication = structuralOccurrenceStampPublication stamp
          expectedPeers =
            maybe
              (remoteStructuralSourceHeralds predecessor source)
              (Set.delete predecessor.heraldEpoch)
              activeHeralds
          observedPeers = Map.keysSet peerPublications
      requireAgreement
        (expectedPeers == observedPeers)
        ( PublicationStructuralPeerSetMismatch
            (Set.toAscList expectedPeers)
            (Set.toAscList observedPeers)
        )
      case findStampedStructuralSource source predecessor of
        Just retained
          | retained.stamp == stamp
              && retained.sourceTopologyPrerequisite == sourceTopology
              && retained.peerPublications == peerPublications ->
              Right
                ( predecessor,
                  (StampedStructuralExactRetry, retained)
                )
          | otherwise -> Left (structuralSourceAlreadyStamped source retained)
        Nothing -> do
          requireAgreement
            (structuralOccurrenceSourceHeraldEpoch occurrence == predecessor.heraldEpoch)
            ( PublicationStructuralSourceMismatch
                predecessor.heraldEpoch
                (structuralOccurrenceSourceHeraldEpoch occurrence)
            )
          requireAgreement
            (occurrence == expectedOccurrence)
            (PublicationStructuralCandidateStale expectedOccurrence occurrence)
          requireAgreement
            (structuralOccurrenceStampPredecessor stamp == currentCandidate.predecessor)
            PublicationStructuralPredecessorMismatch
          requireAgreement
            (observedPublication == expectedPublication)
            ( PublicationStructuralStampPublicationMismatch
                expectedPublication
                observedPublication
            )
          mapM_ (validatePeer expectedPublication) (Map.toAscList peerPublications)
          case Map.lookup occurrence predecessor.stampedStructuralStages of
            Just _ -> Left (PublicationStructuralStageConflict occurrence)
            Nothing -> do
              let retained =
                    StampedStructuralStage
                      { source,
                        sourceTopologyPrerequisite = sourceTopology,
                        stamp,
                        stampedDispatchCertified = Map.null peerPublications,
                        peerPublications
                      }
                  successor :: State
                  successor =
                    predecessor
                      { nextStructuralSequence =
                          nextStructuralSequence predecessor.nextStructuralSequence,
                        stampedStructuralStages =
                          Map.insert
                            occurrence
                            retained
                            predecessor.stampedStructuralStages
                      }
              Right
                ( successor,
                  (StampedStructuralFirstRetained, retained)
                )

    validatePeer expectedPublication (peer, publication) = do
      requireAgreement
        (peerPublicationStructuralStamp publication == Just stamp)
        (PublicationStructuralPeerStampMismatch peer)
      let observedPublication = publicationBatchId (peerPublicationBatch publication)
      requireAgreement
        (observedPublication == expectedPublication)
        ( PublicationStructuralPeerPublicationMismatch
            peer
            expectedPublication
            observedPublication
        )

preparedStampedStructuralClassification ::
  PreparedStampedStructuralStage -> StampedStructuralClassification
preparedStampedStructuralClassification
  (PreparedStampedStructuralStage prepared) = fst (preparedOutput prepared)

preparedStampedStructuralStage ::
  PreparedStampedStructuralStage -> StampedStructuralStage
preparedStampedStructuralStage
  (PreparedStampedStructuralStage prepared) = snd (preparedOutput prepared)

commitStampedStructuralStage ::
  PreparedStampedStructuralStage ->
  (State, StampedStructuralClassification, StampedStructuralStage)
commitStampedStructuralStage (PreparedStampedStructuralStage prepared) =
  let (state, (classification, stage)) = commitPrepared prepared
   in (state, classification, stage)

lookupStampedStructuralStage ::
  StructuralOccurrenceId -> State -> Maybe StampedStructuralStage
lookupStampedStructuralStage occurrence state =
  Map.lookup occurrence state.stampedStructuralStages

stampedStructuralStageEntries ::
  State -> [(StructuralOccurrenceId, StampedStructuralStage)]
stampedStructuralStageEntries state =
  Map.toAscList state.stampedStructuralStages

data StructuralStabilizationClassification
  = StructuralStabilizationFirstRetained
  | StructuralStabilizationExactRetry
  deriving stock (Eq, Ord, Show)

newtype PreparedStructuralStabilization
  = PreparedStructuralStabilization
      ( Prepared
          State
          ( StructuralStabilizationClassification,
            StructuralStabilizationRecord
          )
      )

-- | Retain the installed established cut which covers one source occurrence.
-- Graph owns the coverage proof; the coordinator calls this leaf only after
-- querying that checked owner.
prepareStructuralStabilization ::
  StructuralOccurrenceId ->
  TopologyCutId ->
  State ->
  Either PublicationPreparationError PreparedStructuralStabilization
prepareStructuralStabilization occurrence cut state =
  PreparedStructuralStabilization <$> prepareTransition prepare state
  where
    prepare predecessor = do
      if Map.member occurrence predecessor.stampedStructuralStages
        then Right ()
        else Left (PublicationStructuralStabilizationUnknown occurrence)
      case Map.lookup occurrence predecessor.structuralStabilizations of
        Just retained
          | retained.cut == cut ->
              Right
                ( predecessor,
                  (StructuralStabilizationExactRetry, retained)
                )
          | otherwise ->
              Left
                ( PublicationStructuralStabilizationConflict
                    occurrence
                    retained.cut
                    cut
                )
        Nothing ->
          let retained = StructuralStabilizationRecord occurrence cut
              successor :: State
              successor =
                predecessor
                  { structuralStabilizations =
                      Map.insert
                        occurrence
                        retained
                        predecessor.structuralStabilizations
                  }
           in Right
                ( successor,
                  (StructuralStabilizationFirstRetained, retained)
                )

preparedStructuralStabilizationClassification ::
  PreparedStructuralStabilization -> StructuralStabilizationClassification
preparedStructuralStabilizationClassification
  (PreparedStructuralStabilization prepared) = fst (preparedOutput prepared)

preparedStructuralStabilizationRecord ::
  PreparedStructuralStabilization -> StructuralStabilizationRecord
preparedStructuralStabilizationRecord
  (PreparedStructuralStabilization prepared) = snd (preparedOutput prepared)

commitStructuralStabilization ::
  PreparedStructuralStabilization ->
  ( State,
    StructuralStabilizationClassification,
    StructuralStabilizationRecord
  )
commitStructuralStabilization (PreparedStructuralStabilization prepared) =
  let (state, (classification, record)) = commitPrepared prepared
   in (state, classification, record)

lookupStructuralStabilization ::
  StructuralOccurrenceId -> State -> Maybe StructuralStabilizationRecord
lookupStructuralStabilization occurrence state =
  Map.lookup occurrence state.structuralStabilizations

findStampedAcceptance ::
  ProcessAcceptancePosition -> State -> Maybe StampedStructuralStage
findStampedAcceptance acceptancePosition =
  find
    ( maybe
        False
        ((== acceptancePosition) . applicationPublicationAcceptancePosition)
        . stampedStructuralApplicationRecordMaybe
    )
    . Map.elems
    . (.stampedStructuralStages)

findStampedApplication ::
  ApplicationPublicationRecord -> State -> Maybe StampedStructuralStage
findStampedApplication application =
  findStampedAcceptance (applicationPublicationAcceptancePosition application)

findStampedEnvironmentRoot ::
  Environment.PositionedEnvironmentRoot -> State -> Maybe StampedStructuralStage
findStampedEnvironmentRoot root =
  find
    ( maybe
        False
        ((== root) . snd)
        . stampedStructuralEnvironmentRoot
    )
    . Map.elems
    . (.stampedStructuralStages)

findStampedStructuralSource ::
  StructuralSourceStage -> State -> Maybe StampedStructuralStage
findStampedStructuralSource source = case source of
  ApplicationStructuralSourceStage application ->
    findStampedApplication application
  EnvironmentRootStructuralSourceStage _ root ->
    findStampedEnvironmentRoot root

validateRetainedStructuralSource ::
  StructuralSourceStage -> State -> Either PublicationPreparationError ()
validateRetainedStructuralSource source state = case source of
  ApplicationStructuralSourceStage application -> do
    let position = applicationPublicationAcceptancePosition application
    retained <-
      maybe
        (Left (PublicationApplicationAcceptanceMissing position))
        Right
        (Map.lookup position state.applicationByAcceptance)
    requireAgreement
      (retained == application && applicationPublicationUnsequencedStage retained /= Nothing)
      (PublicationApplicationNotStructural position)
  EnvironmentRootStructuralSourceStage manifest root -> do
    let key = Environment.environmentManifestKeyOf manifest
        identifier = Environment.positionedEnvironmentRootPublicationId root
        manifestContainsRoot =
          root `elem` NonEmpty.toList (Environment.environmentManifestRoots manifest)
    requireAgreement
      (maybe False (`Environment.environmentManifestExtends` manifest) (Map.lookup key state.environmentByAcceptance))
      (PublicationEnvironmentAcceptanceMissing key)
    requireAgreement
      (manifestContainsRoot && Map.lookup identifier state.environmentRootsById == Just root)
      (PublicationEnvironmentPublicationConflict identifier)

structuralSourceAlreadyStamped ::
  StructuralSourceStage -> StampedStructuralStage -> PublicationPreparationError
structuralSourceAlreadyStamped source retained = case source of
  ApplicationStructuralSourceStage application ->
    PublicationStructuralAcceptanceAlreadyStamped
      (applicationPublicationAcceptancePosition application)
      occurrence
  EnvironmentRootStructuralSourceStage _ root ->
    PublicationEnvironmentRootAlreadyStamped
      (Environment.positionedEnvironmentRootPublicationId root)
      occurrence
  where
    occurrence = structuralOccurrenceStampOccurrence retained.stamp

remoteStructuralSourceHeralds ::
  State -> StructuralSourceStage -> Set HeraldEpoch
remoteStructuralSourceHeralds state source =
  Set.delete
    state.heraldEpoch
    ( Set.fromList
        ( destinationHerald
            <$> routeDestinations (structuralSourceRoute source)
        )
    )

data EnvironmentRootRangeClassification
  = EnvironmentRootRangeFirstPositioned
  | EnvironmentRootRangeExactRetry
  deriving stock (Eq, Ord, Show)

newtype PreparedEnvironmentRootRange
  = PreparedEnvironmentRootRange
      ( Prepared
          State
          (EnvironmentRootRangeClassification, Environment.EnvironmentManifest)
      )

-- | Atomically position the endpoint phase of one private environment.  A fresh
-- environment source tenure begins at positive sequence one; a tenure already
-- used by an ordinary publication continues from its retained successor.  No
-- prefix successor, structural stamp, peer item, or outgoing batch is exposed.
prepareEnvironmentRootRange ::
  Environment.EnvironmentManifestKey ->
  HeraldMembershipGeneration ->
  NonEmpty GlobalUniqueId ->
  NonEmpty Environment.EnvironmentRootPlan ->
  State ->
  Either PublicationPreparationError PreparedEnvironmentRootRange
prepareEnvironmentRootRange key membership generated plans state =
  prepareEnvironmentRange Nothing key membership generated plans state

prepareEnvironmentWiringRange ::
  Environment.EnvironmentManifest ->
  HeraldMembershipGeneration ->
  NonEmpty Environment.EnvironmentRootPlan ->
  State ->
  Either PublicationPreparationError PreparedEnvironmentRootRange
prepareEnvironmentWiringRange prior membership plans state =
  prepareEnvironmentRange
    (Just prior)
    (Environment.environmentManifestKeyOf prior)
    membership
    (Environment.environmentManifestGeneratedIds prior)
    plans
    state

prepareEnvironmentRange ::
  Maybe Environment.EnvironmentManifest ->
  Environment.EnvironmentManifestKey ->
  HeraldMembershipGeneration ->
  NonEmpty GlobalUniqueId ->
  NonEmpty Environment.EnvironmentRootPlan ->
  State ->
  Either PublicationPreparationError PreparedEnvironmentRootRange
prepareEnvironmentRange prior key membership generated plans state = do
  requireAgreement
    (Map.lookup key state.environmentByAcceptance == prior)
    (PublicationEnvironmentAcceptanceConflict key)
  requireAgreement
    ( Map.notMember
        (Environment.environmentManifestKeyPosition key)
        state.applicationByAcceptance
    )
    ( PublicationEnvironmentAcceptanceOccupied
        (Environment.environmentManifestKeyPosition key)
    )
  let activeMembers =
        sort
          (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership))
  requireAgreement
    (state.heraldEpoch `elem` activeMembers)
    (PublicationEnvironmentLocalHeraldNotMember state.heraldEpoch)
  PreparedEnvironmentRootRange <$> prepareTransition (prepareFirst activeMembers) state
  where
    prepareFirst activeMembers predecessor = do
      (sequences, nextPosition, rootsById, reversePositioned) <-
        foldM
          (positionOne activeMembers predecessor)
          ( predecessor.nextSequenceByTenure,
            predecessor.nextHeraldPosition,
            predecessor.environmentRootsById,
            []
          )
          (NonEmpty.toList plans)
      positioned <- case NonEmpty.nonEmpty (reverse reversePositioned) of
        Nothing -> error "checked non-empty environment range became empty"
        Just roots -> Right roots
      manifest <-
        either
          (Left . PublicationEnvironmentManifestProblem)
          Right
          ( Environment.environmentManifest
              key
              (maybe (heraldMembershipGenerationId membership) Environment.environmentManifestMembershipGenerationId prior)
              (maybe (heraldMembershipGenerationActiveMemberSetDigest membership) Environment.environmentManifestActiveMemberSetDigest prior)
              generated
              (maybe positioned ((<> positioned) . Environment.environmentManifestRoots) prior)
          )
      groups <-
        foldM
          ( \current root ->
              let plan = Environment.positionedEnvironmentRootPlan root
                  groupMembership =
                    Groups.membership
                      (Environment.environmentManifestKeyProcess key)
                      (Environment.environmentRootPlanSourceSequencingObject plan)
                      (Just (Environment.environmentRootPlanTargetObject plan))
               in either
                    (Left . PublicationGroupProblem)
                    Right
                    (Groups.acceptPublication (Environment.positionedEnvironmentRootPublicationId root) groupMembership True current)
          )
          predecessor.groups
          positioned
      let successor =
            predecessor
              { nextSequenceByTenure = sequences,
                nextHeraldPosition = nextPosition,
                environmentByAcceptance =
                  Map.insert key manifest predecessor.environmentByAcceptance,
                environmentRootsById = rootsById,
                groups
              }
      Right
        ( successor,
          (EnvironmentRootRangeFirstPositioned, manifest)
        )

    positionOne activeMembers predecessor (sequences, nextPosition, rootsById, reversePositioned) plan = do
      let source = Environment.environmentRootPlanSourceNabla plan
          authority = Environment.environmentRootPlanSourceAuthority plan
          tenure = (source, authority)
          sequenceNumber = Map.findWithDefault (nablaSequence 1) tenure sequences
          identifier =
            publicationId
              source
              authority
              predecessor.heraldEpoch
              sequenceNumber
          target = Environment.environmentRootPlanTargetObject plan
          route = Environment.environmentRootPlanRoute plan
          routedMembers =
            sort
              ( Set.toList
                  (Set.fromList (destinationHerald <$> routeDestinations route))
              )
          normallyRoutedMembers =
            Set.fromList
              [ destinationHerald destination
              | destination <- routeDestinations route,
                destinationStrength destination == Normal
              ]
          missingMandatoryMembers =
            filter (`Set.notMember` normallyRoutedMembers) activeMembers
      requireAgreement
        (globalObjectIdFromNablaId source /= target)
        (PublicationEnvironmentSourceEqualsTarget target)
      requireAgreement
        (Environment.environmentRootPlanSourceStrength plan == Normal)
        ( PublicationEnvironmentSourceStrengthMismatch
            (Environment.environmentRootPlanSourceStrength plan)
        )
      requireAgreement
        (nablaSequenceWord64 sequenceNumber > 0)
        (PublicationEnvironmentSourceSequenceNotPositive source authority)
      requireAgreement
        (routedMembers == activeMembers)
        (PublicationEnvironmentRouteMembershipMismatch activeMembers routedMembers)
      requireAgreement
        (null missingMandatoryMembers)
        (PublicationEnvironmentMandatoryRouteMissing missingMandatoryMembers)
      requireAgreement
        ( Map.notMember identifier rootsById
            && not (publicationIdentifierIsOtherwiseRetained identifier predecessor)
        )
        (PublicationEnvironmentPublicationConflict identifier)
      checked <-
        either
          (Left . PublicationEnvironmentValueRejected)
          Right
          ( mkCheckedPublication
              (Environment.environmentRootPlanDescriptor plan)
              identifier
              (Environment.environmentRootPlanValue plan)
          )
      positioned <-
        either
          (Left . PublicationEnvironmentManifestProblem)
          Right
          (Environment.positionEnvironmentRoot plan checked nextPosition)
      Right
        ( Map.insert tenure (advanceNablaSequence sequenceNumber) sequences,
          advanceHeraldPosition nextPosition,
          Map.insert identifier positioned rootsById,
          positioned : reversePositioned
        )

-- | Recover one range only after Application classified the exact retained
-- package-private call as a retry.  No current membership or root plans are
-- consulted, so retirement cannot rewrite accepted provenance or routes.
prepareRetainedEnvironmentRootRange ::
  Environment.EnvironmentManifestKey ->
  State ->
  Either PublicationPreparationError PreparedEnvironmentRootRange
prepareRetainedEnvironmentRootRange key state =
  PreparedEnvironmentRootRange <$> prepareTransition prepare state
  where
    prepare predecessor = case Map.lookup key predecessor.environmentByAcceptance of
      Nothing -> Left (PublicationEnvironmentAcceptanceMissing key)
      Just manifest ->
        Right
          ( predecessor,
            (EnvironmentRootRangeExactRetry, manifest)
          )

preparedEnvironmentRootRangeClassification ::
  PreparedEnvironmentRootRange -> EnvironmentRootRangeClassification
preparedEnvironmentRootRangeClassification (PreparedEnvironmentRootRange prepared) =
  fst (preparedOutput prepared)

preparedEnvironmentManifest ::
  PreparedEnvironmentRootRange -> Environment.EnvironmentManifest
preparedEnvironmentManifest (PreparedEnvironmentRootRange prepared) =
  snd (preparedOutput prepared)

commitEnvironmentRootRange ::
  PreparedEnvironmentRootRange ->
  ( State,
    EnvironmentRootRangeClassification,
    Environment.EnvironmentManifest
  )
commitEnvironmentRootRange (PreparedEnvironmentRootRange prepared) =
  let (successor, (classification, manifest)) = commitPrepared prepared
   in (successor, classification, manifest)

lookupEnvironmentManifest ::
  Environment.EnvironmentManifestKey ->
  State ->
  Maybe Environment.EnvironmentManifest
lookupEnvironmentManifest key state = Map.lookup key state.environmentByAcceptance

lookupPositionedEnvironmentRoot ::
  PublicationId ->
  State ->
  Maybe Environment.PositionedEnvironmentRoot
lookupPositionedEnvironmentRoot identifier state =
  Map.lookup identifier state.environmentRootsById

environmentManifestEntries ::
  State ->
  [(Environment.EnvironmentManifestKey, Environment.EnvironmentManifest)]
environmentManifestEntries = Map.toAscList . (.environmentByAcceptance)

environmentRootStageEntries ::
  State ->
  [(PublicationId, Environment.PositionedEnvironmentRoot)]
environmentRootStageEntries = Map.toAscList . (.environmentRootsById)

-- | Whether every private-environment root at or before one publication cut
-- that names the given peer has consumed its structural stamp.  Alignment
-- owns the cut, while Publication owns the source-arm classification; this
-- narrow view avoids making the alignment coordinator interpret private
-- manifest records.
environmentRootPrefixSettledFor ::
  HeraldPublicationPosition -> HeraldEpoch -> State -> Bool
environmentRootPrefixSettledFor through peer state =
  all settled (Map.elems state.environmentRootsById)
  where
    settled root
      | Environment.positionedEnvironmentRootHeraldPosition root > through = True
      | not (routeNamesPeer root) = True
      | otherwise = findStampedEnvironmentRoot root state /= Nothing
    routeNamesPeer =
      any ((== peer) . destinationHerald)
        . routeDestinations
        . Environment.environmentRootPlanRoute
        . Environment.positionedEnvironmentRootPlan

-- | Owner-local cross-index validation used by focused properties and the
-- whole-Herald invariant.  The immutable manifest constructor already proves
-- canonical shape and per-carrier sequencing; this check proves that the two
-- retained indexes are exact and that no ordinary application publication owns
-- the same process acceptance position.
publicationEnvironmentRangesWellFormed :: State -> Bool
publicationEnvironmentRangesWellFormed state =
  Map.size indexedRoots == length retainedRoots
    && indexedRoots == state.environmentRootsById
    && all keyMatches (Map.toAscList state.environmentByAcceptance)
    && all ((> 0) . retainedSequence) retainedRoots
    && all
      (not . publicationIdentifierIsRetainedOutsideEnvironment)
      (Map.keys state.environmentRootsById)
    && all
      ( (`Map.notMember` state.applicationByAcceptance)
          . Environment.environmentManifestPosition
      )
      (Map.elems state.environmentByAcceptance)
  where
    retainedRoots =
      concatMap
        (NonEmpty.toList . Environment.environmentManifestRoots)
        (Map.elems state.environmentByAcceptance)
    indexedRoots =
      Map.fromList
        [ (Environment.positionedEnvironmentRootPublicationId root, root)
        | root <- retainedRoots
        ]
    keyMatches (key, manifest) =
      key == Environment.environmentManifestKeyOf manifest
        && NonEmpty.length (Environment.environmentManifestRoots manifest)
          `elem` [fromIntegral environmentManifestRootCount, fromIntegral environmentConnectedObjectCount]
    retainedSequence =
      nablaSequenceWord64
        . publicationNablaSequence
        . Environment.positionedEnvironmentRootPublicationId
    publicationIdentifierIsRetainedOutsideEnvironment identifier =
      Map.member identifier state.outgoingById
        || Map.member identifier state.incomingById
        || any
          ((== identifier) . applicationPublicationId)
          (Map.elems state.applicationByAcceptance)

publicationIdentifierIsOtherwiseRetained :: PublicationId -> State -> Bool
publicationIdentifierIsOtherwiseRetained identifier state =
  Map.member identifier state.outgoingById
    || Map.member identifier state.incomingById
    || Map.member identifier state.environmentRootsById
    || any
      ((== identifier) . applicationPublicationId)
      (Map.elems state.applicationByAcceptance)

data IncomingPublicationClassification
  = IncomingPublicationFirstSeen
  | IncomingPublicationSemanticDuplicate
  | IncomingPublicationAssignmentDuplicate
  deriving stock (Eq, Ord, Show)

data IncomingPublicationProblem
  = IncomingPublicationWrongDestination HeraldEpoch HeraldEpoch
  | IncomingPublicationWrongSource HeraldEpoch HeraldEpoch
  | IncomingPublicationDigestMismatch PeerItemDigest PeerItemDigest
  | IncomingPublicationDirectionConflict
      PublicationId
      StreamDirection
      StreamDirection
  | IncomingPublicationItemConflict PublicationId
  | IncomingPublicationSemanticDigestConflict
      PublicationId
      PeerItemDigest
      PeerItemDigest
  | IncomingPublicationRejectedSemanticReassignment PublicationId
  | IncomingAssignmentAlreadyOwned
      StreamDirection
      StreamSequence
      PublicationId
      PublicationId
  | IncomingPublicationResolutionRequired PublicationId
  | IncomingPublicationResolutionUnexpected PublicationId
  | IncomingDestinationSetMismatch
      PublicationId
      (Set PublicationDestination)
      (Set PublicationDestination)
  | IncomingDependencyOutcomeContradiction
  | IncomingDependencyRegression
      PublicationId
      (Set PublicationDependency)
      (Set PublicationDependency)
  | IncomingDestinationOutcomeRegression
      PublicationId
      PublicationDestination
      DestinationOutcome
      DestinationOutcome
  | IncomingDispositionRegression
      PublicationId
      IncomingDisposition
      IncomingDisposition
  | IncomingPublicationRejectionContradiction PublicationId
  | IncomingPublicationUnknown PublicationId
  | IncomingAssignmentRetirementPending PublicationId
  deriving stock (Eq, Show)

-- | Non-installable semantic classification of one stream assignment.
data PreparedIncomingPublicationCandidate
  = PreparedIncomingPublicationCandidate
      State
      HeraldMembershipGenerationId
      AssignmentReceipt
      PeerPublication
      IncomingPublicationClassification

prepareIncomingPublicationCandidate ::
  HeraldMembershipGenerationId ->
  StreamDirection ->
  StreamSequence ->
  PeerItemDigest ->
  PeerPublication ->
  State ->
  Either IncomingPublicationProblem PreparedIncomingPublicationCandidate
prepareIncomingPublicationCandidate membershipGeneration direction sequenceNumber digest peerPublication state = do
  requireIncoming
    (streamDirectionDestination direction == state.heraldEpoch)
    ( IncomingPublicationWrongDestination
        state.heraldEpoch
        (streamDirectionDestination direction)
    )
  requireIncoming
    (streamDirectionSource direction == expectedSource)
    ( IncomingPublicationWrongSource
        expectedSource
        (streamDirectionSource direction)
    )
  requireIncoming
    (digest == expectedDigest)
    (IncomingPublicationDigestMismatch expectedDigest digest)
  case Map.lookup assignmentKey state.incomingAssignmentOwners of
    Just incumbent
      | incumbent /= identifier ->
          Left
            ( IncomingAssignmentAlreadyOwned
                direction
                sequenceNumber
                incumbent
                identifier
            )
    _ -> classify
  where
    batch = peerPublicationBatch peerPublication
    identifier = publicationBatchId batch
    expectedSource = publicationSourceHeraldEpoch identifier
    expectedDigest = peerPublicationDigest peerPublication
    assignment = assignmentReceipt direction sequenceNumber digest
    assignmentKey = (direction, sequenceNumber)

    classify = case Map.lookup identifier state.incomingById of
      Nothing ->
        Right
          ( PreparedIncomingPublicationCandidate
              state
              membershipGeneration
              assignment
              peerPublication
              IncomingPublicationFirstSeen
          )
      Just incumbent -> do
        requireIncoming
          (incumbent.sourceDirection == direction)
          ( IncomingPublicationDirectionConflict
              identifier
              incumbent.sourceDirection
              direction
          )
        requireIncoming
          (incumbent.digest == digest)
          ( IncomingPublicationSemanticDigestConflict
              identifier
              incumbent.digest
              digest
          )
        requireIncoming
          (incumbent.peerPublication == peerPublication)
          (IncomingPublicationItemConflict identifier)
        let assignmentDuplicate = Set.member assignment incumbent.assignments
        requireIncoming
          (assignmentDuplicate || incumbent.disposition /= ProtocolRejected)
          (IncomingPublicationRejectedSemanticReassignment identifier)
        Right
          ( PreparedIncomingPublicationCandidate
              state
              membershipGeneration
              assignment
              peerPublication
              ( if assignmentDuplicate
                  then IncomingPublicationAssignmentDuplicate
                  else IncomingPublicationSemanticDuplicate
              )
          )

preparedIncomingPublicationClassification ::
  PreparedIncomingPublicationCandidate ->
  IncomingPublicationClassification
preparedIncomingPublicationClassification
  (PreparedIncomingPublicationCandidate _ _ _ _ classification) = classification

newtype PreparedIncomingPublication
  = PreparedIncomingPublication
      (Prepared State IncomingPublicationRecord)

-- | Retain one classified assignment.  First semantic observation requires its
-- exact initial lifecycle facts; every duplicate inherits the existing record.
finalizeIncomingPublication ::
  Maybe IncomingPublicationResolution ->
  PreparedIncomingPublicationCandidate ->
  Either IncomingPublicationProblem PreparedIncomingPublication
finalizeIncomingPublication
  suppliedResolution
  ( PreparedIncomingPublicationCandidate
      predecessor
      membershipGeneration
      assignment
      peerPublication
      classification
    ) =
    PreparedIncomingPublication
      <$> prepareTransition install predecessor
    where
      batch = peerPublicationBatch peerPublication
      identifier = publicationBatchId batch
      direction = assignmentReceiptDirection assignment
      sequenceNumber = assignmentReceiptSequence assignment

      install state = do
        record <- case classification of
          IncomingPublicationFirstSeen -> do
            resolution <-
              maybe
                (Left (IncomingPublicationResolutionRequired identifier))
                Right
                suppliedResolution
            validateResolutionDestinations identifier batch resolution
            Right
              IncomingPublicationRecord
                { sourceDirection = direction,
                  admittedMembershipGeneration = membershipGeneration,
                  digest = assignmentReceiptDigest assignment,
                  peerPublication,
                  assignments = Set.singleton assignment,
                  dependencies = resolution.dependencies,
                  destinationOutcomes = resolution.destinationOutcomes,
                  disposition = resolution.disposition
                }
          IncomingPublicationSemanticDuplicate -> do
            rejectUnexpectedResolution suppliedResolution identifier
            incumbent <- requireIncomingRecord identifier state
            Right
              incumbent
                { assignments = Set.insert assignment incumbent.assignments
                }
          IncomingPublicationAssignmentDuplicate -> do
            rejectUnexpectedResolution suppliedResolution identifier
            requireIncomingRecord identifier state
        let successor =
              state
                { incomingById =
                    Map.insert identifier record state.incomingById,
                  incomingAssignmentOwners =
                    Map.insert
                      (direction, sequenceNumber)
                      identifier
                      state.incomingAssignmentOwners,
                  dependencyWaiters = case classification of
                    IncomingPublicationFirstSeen ->
                      insertDependencyWaiters
                        identifier
                        record.dependencies
                        state.dependencyWaiters
                    IncomingPublicationSemanticDuplicate -> state.dependencyWaiters
                    IncomingPublicationAssignmentDuplicate -> state.dependencyWaiters
                }
        Right (successor, record)

preparedIncomingPublicationRecord ::
  PreparedIncomingPublication ->
  IncomingPublicationRecord
preparedIncomingPublicationRecord (PreparedIncomingPublication prepared) =
  preparedOutput prepared

commitIncomingPublication ::
  PreparedIncomingPublication ->
  (State, IncomingPublicationRecord)
commitIncomingPublication (PreparedIncomingPublication prepared) =
  commitPrepared prepared

newtype PreparedIncomingPublicationProgress
  = PreparedIncomingPublicationProgress
      (Prepared State IncomingPublicationRecord)

-- | Monotonically refine dependency, destination, and aggregate lifecycle
-- evidence without changing semantic identity or assignments.
prepareIncomingPublicationProgress ::
  PublicationId ->
  IncomingPublicationResolution ->
  State ->
  Either IncomingPublicationProblem PreparedIncomingPublicationProgress
prepareIncomingPublicationProgress identifier resolution state =
  PreparedIncomingPublicationProgress
    <$> prepareTransition install state
  where
    install predecessor = do
      incumbent <- requireIncomingRecord identifier predecessor
      validateResolutionDestinations
        identifier
        (incomingPublicationBatch incumbent)
        resolution
      requireIncoming
        (dependenciesCanAdvance incumbent resolution)
        ( IncomingDependencyRegression
            identifier
            incumbent.dependencies
            resolution.dependencies
        )
      mapM_
        (validateOutcomeAdvance identifier incumbent.destinationOutcomes)
        (Map.toAscList resolution.destinationOutcomes)
      requireIncoming
        ( dispositionCanAdvance
            incumbent.disposition
            resolution.disposition
        )
        ( IncomingDispositionRegression
            identifier
            incumbent.disposition
            resolution.disposition
        )
      let successorRecord =
            IncomingPublicationRecord
              incumbent.sourceDirection
              incumbent.admittedMembershipGeneration
              incumbent.digest
              incumbent.peerPublication
              incumbent.assignments
              resolution.dependencies
              resolution.destinationOutcomes
              resolution.disposition
          successor =
            predecessor
              { incomingById =
                  Map.insert
                    identifier
                    successorRecord
                    predecessor.incomingById,
                dependencyWaiters =
                  replaceDependencyWaiters
                    identifier
                    incumbent.dependencies
                    resolution.dependencies
                    predecessor.dependencyWaiters
              }
      Right (successor, successorRecord)

-- | Advance only an already-retained structural base wait along admitted
-- membership history. The payload's admission generation, assignments and
-- destination outcomes remain immutable.
prepareIncomingSuccessorBaseRetarget ::
  HeraldMembershipLineage ->
  PublicationId ->
  State ->
  Either IncomingPublicationProblem PreparedIncomingPublicationProgress
prepareIncomingSuccessorBaseRetarget lineage identifier state =
  PreparedIncomingPublicationProgress <$> prepareTransition install state
  where
    oldDependency = SuccessorStructuralBaseDependency (heraldMembershipGenerationId (heraldMembershipLineageOrigin lineage))
    newDependency = SuccessorStructuralBaseDependency (heraldMembershipGenerationId (heraldMembershipLineageTarget lineage))
    install predecessor = do
      incumbent <- requireIncomingRecord identifier predecessor
      let dependencies = Set.insert newDependency (Set.delete oldDependency incumbent.dependencies)
      requireIncoming
        ( incumbent.disposition == DependencyHeld
            && Set.member oldDependency incumbent.dependencies
            && case peerPublicationStructuralStamp incumbent.peerPublication of
              Just _ -> True
              Nothing -> False
        )
        (IncomingDependencyRegression identifier incumbent.dependencies dependencies)
      let successorRecord :: IncomingPublicationRecord
          successorRecord =
            IncomingPublicationRecord
              incumbent.sourceDirection
              incumbent.admittedMembershipGeneration
              incumbent.digest
              incumbent.peerPublication
              incumbent.assignments
              dependencies
              incumbent.destinationOutcomes
              incumbent.disposition
          successor =
            predecessor
              { incomingById = Map.insert identifier successorRecord predecessor.incomingById,
                dependencyWaiters = replaceDependencyWaiters identifier incumbent.dependencies dependencies predecessor.dependencyWaiters
              }
      Right (successor, successorRecord)

-- Satisfying the last pre-authentication prerequisite can reveal structural
-- execution prerequisites only after the item's immutable semantics have been
-- checked. Replacing that final wait set with the first post-authentication
-- hold is still held-to-held progress: it changes no destination outcome and
-- cannot retarget an already-installed structural hold.
dependenciesCanAdvance ::
  IncomingPublicationRecord ->
  IncomingPublicationResolution ->
  Bool
dependenciesCanAdvance incumbent resolution =
  resolution.dependencies `Set.isSubsetOf` incumbent.dependencies
    || installsFirstPostAuthenticationHold
  where
    installsFirstPostAuthenticationHold =
      incumbent.disposition == DependencyHeld
        && resolution.disposition == DependencyHeld
        && not (Set.null incumbent.dependencies)
        && all preAuthenticationDependency incumbent.dependencies
        && not (Set.null resolution.dependencies)
        && all postAuthenticationDependency resolution.dependencies
        && resolution.destinationOutcomes == incumbent.destinationOutcomes
        && case peerPublicationStructuralStamp incumbent.peerPublication of
          Just _ -> True
          Nothing -> False

preparedIncomingPublicationProgressRecord ::
  PreparedIncomingPublicationProgress ->
  IncomingPublicationRecord
preparedIncomingPublicationProgressRecord
  (PreparedIncomingPublicationProgress prepared) = preparedOutput prepared

commitIncomingPublicationProgress ::
  PreparedIncomingPublicationProgress ->
  (State, IncomingPublicationRecord)
commitIncomingPublicationProgress
  (PreparedIncomingPublicationProgress prepared) = commitPrepared prepared

newtype PreparedIncomingPublicationRejection
  = PreparedIncomingPublicationRejection
      (Prepared State IncomingPublicationRecord)

-- | Permanently detach one malformed, already-retained peer item from semantic
-- dependency release. Its exact bytes and pending destination outcomes remain
-- as audit evidence, while transport completion and the source-qualified close
-- action are owned by the peer coordinator.
prepareIncomingPublicationRejection ::
  PublicationId ->
  State ->
  Either IncomingPublicationProblem PreparedIncomingPublicationRejection
prepareIncomingPublicationRejection identifier state =
  PreparedIncomingPublicationRejection
    <$> prepareTransition reject state
  where
    reject predecessor = do
      incumbent <- requireIncomingRecord identifier predecessor
      requireIncoming
        ( incumbent.disposition == DependencyHeld
            && not (Set.null incumbent.dependencies)
            && all (== DestinationPending) (Map.elems incumbent.destinationOutcomes)
        )
        (IncomingPublicationRejectionContradiction identifier)
      let rejected =
            IncomingPublicationRecord
              incumbent.sourceDirection
              incumbent.admittedMembershipGeneration
              incumbent.digest
              incumbent.peerPublication
              incumbent.assignments
              Set.empty
              incumbent.destinationOutcomes
              ProtocolRejected
          successor =
            predecessor
              { incomingById = Map.insert identifier rejected predecessor.incomingById,
                dependencyWaiters =
                  replaceDependencyWaiters
                    identifier
                    incumbent.dependencies
                    Set.empty
                    predecessor.dependencyWaiters
              }
      Right (successor, rejected)

commitIncomingPublicationRejection ::
  PreparedIncomingPublicationRejection ->
  (State, IncomingPublicationRecord)
commitIncomingPublicationRejection
  (PreparedIncomingPublicationRejection prepared) = commitPrepared prepared

-- | Read-only allocation and immutable-record evidence for validation and
-- focused properties.
data PublicationStateWitness = PublicationStateWitness
  { witnessHeraldEpoch :: HeraldEpoch,
    witnessNextSequences :: [((NablaId, AuthorityEpoch), NablaSequence)],
    witnessNextStructuralSequence :: StructuralSequence,
    witnessNextHeraldPosition :: HeraldPublicationPosition,
    witnessOutgoing :: [OutgoingPublicationRecord],
    witnessApplicationPublications :: [ApplicationPublicationRecord],
    witnessEnvironmentManifests :: [Environment.EnvironmentManifest],
    witnessEnvironmentRootStages :: [Environment.PositionedEnvironmentRoot],
    witnessStampedStructuralStages :: [StampedStructuralStage],
    witnessStructuralStabilizations :: [StructuralStabilizationRecord],
    witnessIncoming :: [IncomingPublicationRecord],
    witnessIncomingAssignments ::
      [((StreamDirection, StreamSequence), PublicationId)],
    witnessIncomingDependencyWaiters ::
      [(PublicationDependency, Set PublicationId)],
    witnessGroups :: Groups.State
  }
  deriving stock (Eq, Show)

publicationStateWitness :: State -> PublicationStateWitness
publicationStateWitness state =
  PublicationStateWitness
    { witnessHeraldEpoch = state.heraldEpoch,
      witnessNextSequences = Map.toAscList state.nextSequenceByTenure,
      witnessNextStructuralSequence = state.nextStructuralSequence,
      witnessNextHeraldPosition = state.nextHeraldPosition,
      witnessOutgoing = Map.elems state.outgoingById,
      witnessApplicationPublications = Map.elems state.applicationByAcceptance,
      witnessEnvironmentManifests = Map.elems state.environmentByAcceptance,
      witnessEnvironmentRootStages = Map.elems state.environmentRootsById,
      witnessStampedStructuralStages = Map.elems state.stampedStructuralStages,
      witnessStructuralStabilizations = Map.elems state.structuralStabilizations,
      witnessIncoming = Map.elems state.incomingById,
      witnessIncomingAssignments = Map.toAscList state.incomingAssignmentOwners,
      witnessIncomingDependencyWaiters = Map.toAscList state.dependencyWaiters,
      witnessGroups = state.groups
    }

publicationWitnessHeraldEpoch :: PublicationStateWitness -> HeraldEpoch
publicationWitnessHeraldEpoch witness = witness.witnessHeraldEpoch

publicationWitnessNextSequences ::
  PublicationStateWitness ->
  [((NablaId, AuthorityEpoch), NablaSequence)]
publicationWitnessNextSequences witness = witness.witnessNextSequences

publicationWitnessNextHeraldPosition ::
  PublicationStateWitness ->
  HeraldPublicationPosition
publicationWitnessNextHeraldPosition witness = witness.witnessNextHeraldPosition

publicationWitnessNextStructuralSequence ::
  PublicationStateWitness -> StructuralSequence
publicationWitnessNextStructuralSequence witness = witness.witnessNextStructuralSequence

publicationWitnessStampedStructuralStages ::
  PublicationStateWitness -> [StampedStructuralStage]
publicationWitnessStampedStructuralStages witness = witness.witnessStampedStructuralStages

publicationWitnessStructuralStabilizations ::
  PublicationStateWitness -> [StructuralStabilizationRecord]
publicationWitnessStructuralStabilizations witness =
  witness.witnessStructuralStabilizations

publicationWitnessOutgoing ::
  PublicationStateWitness ->
  [OutgoingPublicationRecord]
publicationWitnessOutgoing witness = witness.witnessOutgoing

publicationWitnessApplicationPublications ::
  PublicationStateWitness ->
  [ApplicationPublicationRecord]
publicationWitnessApplicationPublications witness = witness.witnessApplicationPublications

publicationWitnessEnvironmentManifests ::
  PublicationStateWitness -> [Environment.EnvironmentManifest]
publicationWitnessEnvironmentManifests witness =
  witness.witnessEnvironmentManifests

publicationWitnessEnvironmentRootStages ::
  PublicationStateWitness -> [Environment.PositionedEnvironmentRoot]
publicationWitnessEnvironmentRootStages witness =
  witness.witnessEnvironmentRootStages

publicationWitnessIncoming ::
  PublicationStateWitness ->
  [IncomingPublicationRecord]
publicationWitnessIncoming witness = witness.witnessIncoming

publicationWitnessIncomingAssignments ::
  PublicationStateWitness ->
  [((StreamDirection, StreamSequence), PublicationId)]
publicationWitnessIncomingAssignments witness = witness.witnessIncomingAssignments

publicationWitnessIncomingDependencyWaiters ::
  PublicationStateWitness ->
  [(PublicationDependency, Set PublicationId)]
publicationWitnessIncomingDependencyWaiters witness =
  witness.witnessIncomingDependencyWaiters

-- | Corrupt one retained source owner for whole-state invariant tests. The
-- opaque production record remains constructible only through admission.
replaceOutgoingPublicationSourceProcessForInvariantTest ::
  PublicationId ->
  ProcessEpochId ->
  State ->
  State
replaceOutgoingPublicationSourceProcessForInvariantTest identifier replacement state =
  state
    { outgoingById =
        Map.adjust
          replaceSource
          identifier
          state.outgoingById
    }
  where
    replaceSource :: OutgoingPublicationRecord -> OutgoingPublicationRecord
    replaceSource record =
      OutgoingPublicationRecord
        record.checked
        replacement
        record.acceptancePosition
        record.occurrenceId
        record.heraldPosition
        record.route
        record.sourceStrength
        record.sourceTopologyPrerequisite
        record.controlPrerequisite
        record.groupMembership
        record.outgoingDispatchCertified
        record.remoteBatches

-- | Add an unassigned remote batch for whole-state invariant tests.
insertOutgoingPublicationRemoteBatchForInvariantTest ::
  PublicationId ->
  HeraldEpoch ->
  PublicationBatch ->
  State ->
  State
insertOutgoingPublicationRemoteBatchForInvariantTest identifier peer batch state =
  state
    { outgoingById =
        Map.adjust
          ( \record ->
              record
                { outgoingDispatchCertified = False,
                  remoteBatches = Map.insert peer batch record.remoteBatches
                }
          )
          identifier
          state.outgoingById
    }

-- | Add an owner projection with no matching incoming record for whole-state
-- invariant tests.
insertIncomingAssignmentOwnerForInvariantTest ::
  StreamDirection ->
  StreamSequence ->
  PublicationId ->
  State ->
  State
insertIncomingAssignmentOwnerForInvariantTest direction sequenceNumber identifier state =
  state
    { incomingAssignmentOwners =
        Map.insert
          (direction, sequenceNumber)
          identifier
          state.incomingAssignmentOwners
    }

-- | Add a reverse dependency index entry with no matching incoming record for
-- whole-state invariant tests.
insertIncomingDependencyWaiterForInvariantTest ::
  PublicationDependency ->
  PublicationId ->
  State ->
  State
insertIncomingDependencyWaiterForInvariantTest dependency identifier state =
  state
    { dependencyWaiters =
        Map.insertWith
          Set.union
          dependency
          (Set.singleton identifier)
          state.dependencyWaiters
    }

makeRemoteBatches ::
  HeraldEpoch ->
  CheckedPublication ->
  ProcessEpochId ->
  SortDefinitionOccurrenceId ->
  ReplicaStrength ->
  TopologyCutId ->
  ControlIndex ->
  FrozenRoute ->
  Either PublicationPreparationError (Map HeraldEpoch PublicationBatch)
makeRemoteBatches
  localEpoch
  checked
  sourceProcess
  occurrence
  sourceStrength
  sourceTopologyPrerequisite
  controlPrerequisite
  route =
    traverse makeBatch grouped
    where
      grouped =
        foldl addDestination Map.empty (routeDestinations route)
      addDestination current destination
        | destinationHerald destination == localEpoch = current
        | otherwise =
            Map.insertWith
              (flip (<>))
              (destinationHerald destination)
              (toPeerDestination destination :| [])
              current
      makeBatch destinations =
        either
          (Left . PublicationRemoteBatchConstructionFault)
          Right
          ( mkPublicationBatch
              (checkedPublicationId checked)
              sourceProcess
              (checkedPublicationSort checked)
              occurrence
              (checkedPublicationCanonicalValue checked)
              sourceStrength
              sourceTopologyPrerequisite
              controlPrerequisite
              destinations
          )
      toPeerDestination destination =
        publicationDestination
          (destinationDelta destination)
          (destinationStoreIncarnation destination)
          (destinationStrength destination)

deriveApplicationPublicationRetention ::
  CheckedDescriptor ->
  CheckedPublication ->
  Either PublicationPreparationError ApplicationPublicationRetention
deriveApplicationPublicationRetention descriptor checked =
  case descriptorKind descriptor of
    RegularSort -> Right RegularApplicationPublication
    ControlledSort -> do
      object <- controlledPublicationObject descriptor checked
      case checkedPublicationLifecycle checked of
        Current -> Right (CurrentControlledApplicationPublication object)
        Obsolete -> Right (ObsoleteControlledApplicationPublication object)

validateApplicationPublicationOrigin ::
  ApplicationPublicationOrigin ->
  ApplicationPublicationRetention ->
  Either PublicationPreparationError ()
validateApplicationPublicationOrigin origin retention =
  requireAgreement
    ( case (origin, retention) of
        (RegularApplicationPublicationOrigin, RegularApplicationPublication) -> True
        (ControlledFirstUseApplicationPublicationOrigin generated, CurrentControlledApplicationPublication object) ->
          globalObjectIdFromGlobalUniqueId generated == object
        (ControlledUpdateApplicationPublicationOrigin expected, CurrentControlledApplicationPublication object) ->
          expected == object
        (ControlledUpdateApplicationPublicationOrigin expected, ObsoleteControlledApplicationPublication object) ->
          expected == object
        (ForwardApplicationPublicationOrigin expected _, CurrentControlledApplicationPublication object) ->
          expected == object
        (ForwardApplicationPublicationOrigin expected _, ObsoleteControlledApplicationPublication object) ->
          expected == object
        _ -> False
    )
    (PublicationApplicationOriginContradiction origin retention)

validateApplicationPublicationSourceStrength ::
  ApplicationPublicationOrigin ->
  ReplicaStrength ->
  Either PublicationPreparationError ()
validateApplicationPublicationSourceStrength origin sourceStrength =
  requireAgreement
    ( case origin of
        ForwardApplicationPublicationOrigin _ retainedStrength ->
          retainedStrength == sourceStrength
        RegularApplicationPublicationOrigin -> sourceStrength == Normal
        ControlledFirstUseApplicationPublicationOrigin _ -> sourceStrength == Normal
        ControlledUpdateApplicationPublicationOrigin _ -> sourceStrength == Normal
    )
    (PublicationApplicationSourceStrengthContradiction origin sourceStrength)

controlledPublicationObject ::
  CheckedDescriptor ->
  CheckedPublication ->
  Either PublicationPreparationError GlobalObjectId
controlledPublicationObject descriptor checked =
  case descriptorControlledKeyProjection descriptor of
    Nothing -> Left PublicationControlledObjectKeyContradiction
    Just projection ->
      case valueAt projection (checkedPublicationValue checked) of
        Right value -> case viewValue value of
          GlobalUniqueIdValue global ->
            Right (globalObjectIdFromGlobalUniqueId global)
          _ -> Left PublicationControlledObjectKeyContradiction
        Left _ -> Left PublicationControlledObjectKeyContradiction

validateResolutionDestinations ::
  PublicationId ->
  PublicationBatch ->
  IncomingPublicationResolution ->
  Either IncomingPublicationProblem ()
validateResolutionDestinations identifier batch resolution =
  requireIncoming
    (expected == actual)
    (IncomingDestinationSetMismatch identifier expected actual)
  where
    expected = Set.fromList (nonEmptyToList (publicationBatchDestinations batch))
    actual = Map.keysSet resolution.destinationOutcomes

validateOutcomeAdvance ::
  PublicationId ->
  Map PublicationDestination DestinationOutcome ->
  (PublicationDestination, DestinationOutcome) ->
  Either IncomingPublicationProblem ()
validateOutcomeAdvance identifier incumbent (destination, successor) =
  case Map.lookup destination incumbent of
    Just predecessor ->
      requireIncoming
        (outcomeCanAdvance predecessor successor)
        ( IncomingDestinationOutcomeRegression
            identifier
            destination
            predecessor
            successor
        )
    Nothing ->
      Left
        ( IncomingDestinationSetMismatch
            identifier
            (Map.keysSet incumbent)
            (Set.insert destination (Map.keysSet incumbent))
        )

outcomeCanAdvance :: DestinationOutcome -> DestinationOutcome -> Bool
outcomeCanAdvance DestinationPending _ = True
outcomeCanAdvance predecessor successor = predecessor == successor

dispositionCanAdvance :: IncomingDisposition -> IncomingDisposition -> Bool
dispositionCanAdvance DependencyHeld _ = True
dispositionCanAdvance Applied successor = successor == Applied
dispositionCanAdvance TerminallyIgnored successor =
  successor == TerminallyIgnored
dispositionCanAdvance ProtocolRejected successor =
  successor == ProtocolRejected

deriveDisposition ::
  Set PublicationDependency ->
  Map PublicationDestination DestinationOutcome ->
  IncomingDisposition
deriveDisposition dependencies outcomes
  | not (Set.null dependencies) = DependencyHeld
  | any (== DestinationApplied) values = Applied
  | otherwise = TerminallyIgnored
  where
    values = Map.elems outcomes

rejectUnexpectedResolution ::
  Maybe IncomingPublicationResolution ->
  PublicationId ->
  Either IncomingPublicationProblem ()
rejectUnexpectedResolution Nothing _ = Right ()
rejectUnexpectedResolution (Just _) identifier =
  Left (IncomingPublicationResolutionUnexpected identifier)

insertDependencyWaiters ::
  PublicationId ->
  Set PublicationDependency ->
  Map PublicationDependency (Set PublicationId) ->
  Map PublicationDependency (Set PublicationId)
insertDependencyWaiters identifier dependencies waiters =
  Set.foldl'
    ( \current dependency ->
        Map.insertWith Set.union dependency (Set.singleton identifier) current
    )
    waiters
    dependencies

replaceDependencyWaiters ::
  PublicationId ->
  Set PublicationDependency ->
  Set PublicationDependency ->
  Map PublicationDependency (Set PublicationId) ->
  Map PublicationDependency (Set PublicationId)
replaceDependencyWaiters identifier previous next =
  insertDependencyWaiters identifier next . removeOld
  where
    removeOld waiters =
      Set.foldl'
        ( \current dependency ->
            Map.update
              (dropWaiter identifier)
              dependency
              current
        )
        waiters
        (Set.difference previous next)
    dropWaiter waiter identifiers =
      let remaining = Set.delete waiter identifiers
       in if Set.null remaining then Nothing else Just remaining

requireIncomingRecord ::
  PublicationId ->
  State ->
  Either IncomingPublicationProblem IncomingPublicationRecord
requireIncomingRecord identifier state =
  maybe
    (Left (IncomingPublicationUnknown identifier))
    Right
    (Map.lookup identifier state.incomingById)

nextSequence ::
  (NablaId, AuthorityEpoch) ->
  State ->
  NablaSequence
nextSequence key state =
  Map.findWithDefault (nablaSequence 0) key state.nextSequenceByTenure

advanceNablaSequence :: NablaSequence -> NablaSequence
advanceNablaSequence sequenceNumber =
  nablaSequence (nablaSequenceWord64 sequenceNumber + 1)

advanceHeraldPosition ::
  HeraldPublicationPosition ->
  HeraldPublicationPosition
advanceHeraldPosition = nextHeraldPublicationPosition

nonEmptyToList :: NonEmpty value -> [value]
nonEmptyToList (first :| remaining) = first : remaining

requireAgreement :: Bool -> problem -> Either problem ()
requireAgreement agrees problem
  | agrees = Right ()
  | otherwise = Left problem

requireIncoming ::
  Bool ->
  IncomingPublicationProblem ->
  Either IncomingPublicationProblem ()
requireIncoming agrees problem
  | agrees = Right ()
  | otherwise = Left problem
