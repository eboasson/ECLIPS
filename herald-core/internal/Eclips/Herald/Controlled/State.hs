{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Checked bootstrap-controlled roles, reservations, and monotone local
-- controlled-object knowledge.
module Eclips.Herald.Controlled.State
  ( State,
    emptyState,
    ControlledControlBase,
    captureControlledControlBase,
    ControlledControlBaseDerivationError (..),
    controlledControlBaseFromProjection,
    ControlledControlBaseImportError (..),
    PreparedControlledControlBaseImport,
    prepareControlledControlBaseImport,
    commitControlledControlBaseImport,
    PreparationChanges (..),
    takePreparationChanges,
    clearPreparationChanges,
    ProcessFact,
    processFact,
    processFactProcessId,
    processFactProcessEpoch,
    processFactResidence,
    processFactAuthority,
    RootFact,
    writerRootFact,
    readerRootFact,
    rootFactCatalogueRole,
    rootFactProcessEpoch,
    rootFactSortId,
    rootFactOccurrenceId,
    rootFactAuthority,
    rootFactControlPrerequisite,
    rootFactRole,
    rootFactWriterSequencing,
    ControlledRootRole (..),
    CheckedControlledBootstrapOperate,
    ControlledBootstrapOperateError (..),
    checkControlledBootstrapOperate,
    controlledBootstrapOperateProcess,
    controlledBootstrapOperateWriter,
    controlledBootstrapOperateSortId,
    controlledBootstrapOperateOccurrenceId,
    controlledBootstrapOperateAuthority,
    controlledBootstrapOperateControlPrerequisite,
    controlledBootstrapOperateWriterSequencing,
    ControlledWriterBinding,
    controlledWriterBinding,
    controlledWriterBindingProcess,
    controlledWriterBindingWriter,
    controlledWriterBindingSortId,
    controlledWriterBindingOccurrenceId,
    controlledWriterBindingAuthority,
    controlledWriterBindingControlPrerequisite,
    controlledBootstrapOperateWriterBinding,
    controlledProcessFact,
    controlledWriterFact,
    controlledReaderFact,
    controlledProcessFacts,
    controlledRootFacts,
    controlledProcessEnded,
    ControlledLabelPrior,
    controlledLabelPriorObject,
    controlledLabelPriorEffectiveState,
    controlledLabelPriorInitialEvidence,
    controlledLabelPriorRevision,
    controlledLabelPriorAuthority,
    controlledLabelPriorAuthorityJustification,
    ControlledLabelPriorError (..),
    checkControlledLabelPrior,
    checkControlledLabelPriorWithAuthority,
    controlledObjectLabelable,
    controlledReleasedLabelRecord,
    controlledReleasedLabelEntries,
    controlledEffectiveLabelState,
    ControlledLabelReleaseDisposition (..),
    ControlledLabelReleaseError (..),
    PreparedControlledLabelRelease,
    prepareControlledLabelRelease,
    prepareControlledLabelReleaseWithAuthority,
    preparedControlledLabelReleaseDisposition,
    commitControlledLabelRelease,
    ControlledTerminalDeletion,
    controlledTerminalDeletion,
    controlledTerminalDeletionObject,
    controlledTerminalDeletionCause,
    controlledTerminalDeletionGeneration,
    controlledTerminalDeletionEntries,
    controlledObjectTerminallyDeleted,
    ControlledRemovalPayloadCoordinate,
    controlledRemovalPayloadSortId,
    controlledRemovalPayloadObjectKey,
    ControlledRemovalDisposition (..),
    ControlledRemovalError (..),
    PreparedControlledRemoval,
    prepareControlledRemoval,
    preparedControlledRemovalDisposition,
    preparedControlledRemovalPayloadCoordinate,
    preparedControlledRemovalRevokedDirectPossessions,
    preparedControlledRemovalRevokedStorePossessions,
    commitControlledRemoval,
    CheckedControlledGrant,
    ControlledGrantError (..),
    checkControlledGrant,
    checkTransferredControlledGrant,
    controlledGrantObject,
    PreparedControlledGrants,
    prepareControlledGrants,
    commitControlledGrants,
    controlledObjectCurrent,
    controlledObjectReleasedDeleted,
    controlledNamedProcessEnded,
    controlledHasDirectPossession,
    controlledHasNormalPossession,
    controlledEffectivePossessionStrength,
    controlledNormalPossessionEntries,
    controlledHasAnyRole,
    controlledHasAnyNormalPossession,
    controlledCurrentStructuralRole,
    ReservationPhase (..),
    ReservationWitness,
    reservationWitnessGlobalUniqueId,
    reservationWitnessProcess,
    reservationWitnessNabla,
    reservationWitnessSortId,
    reservationWitnessAuthority,
    reservationWitnessPhase,
    reservationWitnessFirstPublicationId,
    controlledReservationWitnesses,
    controlledReservationWitness,
    PreparedControlledReservation,
    prepareControlledReservation,
    commitControlledReservation,
    ControlledReservationDeleteError (..),
    PreparedControlledReservationDelete,
    prepareControlledReservationDelete,
    commitControlledReservationDelete,
    PreparedControlledNablaReservationRetirement,
    prepareControlledNablaReservationRetirement,
    preparedControlledNablaReservationRetirementCancelled,
    commitControlledNablaReservationRetirement,
    PreparedControlledProcessRetirement,
    prepareControlledProcessRetirement,
    commitControlledProcessRetirement,
    ControlledLifecycle (..),
    ControlledSuppression (..),
    ControlledLocalRecord,
    controlledLocalRecord,
    controlledLocalRecords,
    controlledRecordObjectId,
    controlledRecordSortId,
    controlledRecordOccurrenceId,
    controlledRecordLifecycle,
    controlledRecordObservedLabel,
    controlledRecordFirstPublication,
    controlledRecordLatestPublication,
    controlledRecordLatestWinner,
    controlledRecordStructuralRole,
    controlledRecordSuppression,
    controlledRecordObsoleteObservedAt,
    controlledRecordPossessionSources,
    controlledRecordObservation,
    controlledRecordObservationLabelCompatible,
    CheckedControlledObservation,
    ControlledObservationError (..),
    checkControlledObservation,
    checkedControlledObservationObject,
    checkedControlledObservationOccurrence,
    checkedControlledObservationLabel,
    checkedControlledObservationStructuralRole,
    checkedControlledObservationPublication,
    ControlledStorePossessionWitness,
    controlledStorePossessionWitnessProcess,
    controlledStorePossessionWitnessDelta,
    controlledStorePossessionWitnessObject,
    controlledStorePossessionWitnessStrength,
    controlledStorePossessionWitnesses,
    controlledStorePossessionStrength,
    ControlledStoreObservationError (..),
    PreparedControlledStoreObservation,
    prepareControlledStoreRead,
    prepareControlledStoreLocalTake,
    commitControlledStoreObservation,
    ControlledFirstUseResult (..),
    ControlledFirstUseError (..),
    PreparedControlledFirstUse,
    prepareControlledFirstUse,
    prepareControlledFirstUseWithBinding,
    preparedControlledFirstUseResult,
    commitControlledFirstUse,
    ControlledUpdateResult (..),
    ControlledUpdateError (..),
    PreparedControlledUpdate,
    prepareControlledUpdate,
    prepareControlledUpdateWithBinding,
    preparedControlledUpdateResult,
    commitControlledUpdate,
    ControlledPeerObservationResult (..),
    ControlledPeerObservationError (..),
    PreparedControlledPeerObservation,
    prepareControlledPeerObservation,
    prepareControlledStartupObservation,
    preparedControlledPeerObservationResult,
    commitControlledPeerObservation,
    ControlledEnvironmentRoot,
    ControlledEnvironmentRootError (..),
    checkControlledEnvironmentRoot,
    controlledEnvironmentRootProcess,
    controlledEnvironmentRootSlot,
    controlledEnvironmentRootGeneratedId,
    controlledEnvironmentRootTargetObject,
    controlledEnvironmentRootSourceBinding,
    controlledEnvironmentRootObservation,
    ControlledEnvironmentRootsClassification (..),
    ControlledEnvironmentRootsError (..),
    PreparedControlledEnvironmentRoots,
    prepareControlledEnvironmentRoots,
    prepareControlledEnvironmentWiring,
    preparedControlledEnvironmentRootsClassification,
    commitControlledEnvironmentRoots,
    ControlledObsoleteObservationTimeResult (..),
    ControlledObsoleteObservationTimeError (..),
    PreparedControlledObsoleteObservationTime,
    prepareControlledObsoleteObservationTime,
    preparedControlledObsoleteObservationTimeResult,
    commitControlledObsoleteObservationTime,
    PreparedControlledObsoleteWinnerTimes,
    prepareControlledObsoleteWinnerTimes,
    preparedControlledObsoleteWinnerTimesRecorded,
    commitControlledObsoleteWinnerTimes,
    PreparedControlledBootstrap,
    ControlledBootstrapError (..),
    prepareControlledBootstrap,
    prepareControlledBootstrapObservation,
    prepareControlledBootstrapDescriptions,
    commitControlledBootstrap,
  )
where

import Control.Monad (foldM)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word64)
import Eclips.Domain.Disappearance
  ( DisappearanceResolutionOutcomeView (ControlledDisappearanceResolved),
    disappearanceResolutionOutcomeView,
  )
import Eclips.Domain.Environment
  ( EnvironmentRootClaimView (..),
    EnvironmentRootSlot,
    environmentManifestShapeRoots,
    environmentRootClaimView,
    environmentRootSlotClaim,
    environmentRootSlotStructuralCarrierRole,
    environmentWiringSlots,
    profileEnvironmentManifestShape,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    AuthorityEpochView (..),
    ControlIndex,
    DeltaId,
    GlobalObjectId,
    GlobalUniqueId,
    HeraldEpoch,
    NablaId,
    NablaSequencing (..),
    ProcessEpochId,
    ProcessId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    authorityEpochView,
    globalObjectIdFromDeltaId,
    globalObjectIdFromGlobalUniqueId,
    globalObjectIdFromNablaId,
    globalObjectIdFromProcessEpochId,
    globalUniqueIdFromGlobalObjectId,
    labelAuthorityEpoch,
    mkSortId,
    publicationAuthorityEpoch,
    publicationNabla,
  )
import Eclips.Domain.Label
  ( InitialLabelEvidence,
    LabelRecord,
    LabelRecordError,
    LabelRevision,
    PreparedAuthorityDisposition,
    PreparedAuthorityDispositionView (..),
    PreparedLabelFacts,
    PriorAuthorityJustification,
    ReleasedLabelState,
    ReleasedLabelStateView (..),
    checkedGenesisAuthority,
    establishedStructuralAuthority,
    existingReleasedAuthority,
    initialBootstrapLabelEvidence,
    initialPublicationLabelEvidence,
    labelRecordReleasedState,
    labelRecordRetainedAuthority,
    labelRecordRevision,
    labelRevisionControlIndex,
    mkLabelRecord,
    mkLabelRevision,
    preparedAuthorityDispositionView,
    preparedLabelFactsAuthorityDisposition,
    preparedLabelFactsExpectedPriorAuthority,
    preparedLabelFactsExpectedPriorRevision,
    preparedLabelFactsExpectedPriorState,
    preparedLabelFactsObjectId,
    preparedLabelFactsPriorAuthorityJustification,
    preparedLabelFactsProposedOutcome,
    preparedLabelFactsResolveIndex,
    releasedDeleted,
    releasedLabel,
    releasedLabelStateGeneration,
    releasedLabelStateIsDeleted,
    releasedLabelStateView,
  )
import Eclips.Domain.ProcessLifecycle (effectiveProcessLabel)
import Eclips.Domain.Publication
  ( CheckedPublication,
    PublicationWinnerKey,
    checkedPublicationId,
    checkedPublicationKey,
    checkedPublicationLifecycle,
    checkedPublicationSort,
    checkedPublicationValue,
    checkedPublicationWinnerKey,
  )
import Eclips.Domain.Route (ReplicaStrength (..))
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalCheckedDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( Lifecycle (..),
    ObjectKey,
    SortKind (ControlledSort),
    StructuralCarrierRole (..),
    controlledObjectKey,
    descriptorControlledKeyProjection,
    descriptorKind,
    descriptorLabelField,
    descriptorStructuralCarrierRole,
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (DeltaRole, EdgeRole, NablaRole, NeutralVertexRole, ProcessEpochRole),
    profileSortFor,
  )
import Eclips.Domain.Startup (InitialProjectionDigest)
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    StructuralConsequenceCauseError,
    StructuralConsequenceCauseView (..),
    labelReleaseCause,
    predefinedDisappearanceCause,
    structuralConsequenceCauseControlIndex,
    structuralConsequenceCauseView,
  )
import Eclips.Domain.Value
  ( FieldName,
    Label,
    LabelOwner (..),
    ValueView (..),
    directProjection,
    mkFieldName,
    valueAt,
    viewValue,
  )
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
    preparedOutput,
  )
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Time (MonotonicInstant)
import Eclips.Oracle.Label
  ( liveDecisionObject,
    releasedLabelOverlayRetainedAuthority,
    releasedLabelOverlayRevision,
    releasedLabelOverlayState,
  )

-- | The checked process-epoch role installed by Oracle/bootstrap authority.
data ProcessFact = ProcessFact
  { processId :: ProcessId,
    processEpoch :: ProcessEpochId,
    residence :: HeraldEpoch,
    authority :: AuthorityEpoch
  }
  deriving stock (Eq, Show)

processFact ::
  ProcessId ->
  ProcessEpochId ->
  HeraldEpoch ->
  AuthorityEpoch ->
  ProcessFact
processFact = ProcessFact

processFactProcessId :: ProcessFact -> ProcessId
processFactProcessId fact = fact.processId

processFactProcessEpoch :: ProcessFact -> ProcessEpochId
processFactProcessEpoch fact = fact.processEpoch

processFactResidence :: ProcessFact -> HeraldEpoch
processFactResidence fact = fact.residence

processFactAuthority :: ProcessFact -> AuthorityEpoch
processFactAuthority fact = fact.authority

data ControlledRootRole
  = ControlledWriter NablaId NablaSequencing
  | ControlledReader DeltaId
  deriving stock (Eq, Ord, Show)

-- | One canonical predefined writer or reader fact. Genesis installation grants
-- its resident owner possession; remote observation retains the same metadata
-- without granting that historical owner any possession.
data RootFact = RootFact
  { processEpoch :: ProcessEpochId,
    catalogueRole :: PredefinedSortRole,
    sortId :: SortId,
    occurrenceId :: SortDefinitionOccurrenceId,
    authority :: AuthorityEpoch,
    controlPrerequisite :: ControlIndex,
    role :: ControlledRootRole
  }
  deriving stock (Eq, Show)

writerRootFact ::
  ProcessEpochId ->
  PredefinedSortRole ->
  SortId ->
  SortDefinitionOccurrenceId ->
  AuthorityEpoch ->
  ControlIndex ->
  NablaId ->
  NablaSequencing ->
  RootFact
writerRootFact process catalogueRole sortId occurrence authority prerequisite nabla sequencing =
  RootFact
    process
    catalogueRole
    sortId
    occurrence
    authority
    prerequisite
    (ControlledWriter nabla sequencing)

readerRootFact ::
  ProcessEpochId ->
  PredefinedSortRole ->
  SortId ->
  SortDefinitionOccurrenceId ->
  AuthorityEpoch ->
  ControlIndex ->
  DeltaId ->
  RootFact
readerRootFact process catalogueRole sortId occurrence authority prerequisite delta =
  RootFact
    process
    catalogueRole
    sortId
    occurrence
    authority
    prerequisite
    (ControlledReader delta)

rootFactProcessEpoch :: RootFact -> ProcessEpochId
rootFactProcessEpoch fact = fact.processEpoch

rootFactCatalogueRole :: RootFact -> PredefinedSortRole
rootFactCatalogueRole fact = fact.catalogueRole

rootFactSortId :: RootFact -> SortId
rootFactSortId fact = fact.sortId

rootFactOccurrenceId :: RootFact -> SortDefinitionOccurrenceId
rootFactOccurrenceId fact = fact.occurrenceId

rootFactAuthority :: RootFact -> AuthorityEpoch
rootFactAuthority fact = fact.authority

rootFactControlPrerequisite :: RootFact -> ControlIndex
rootFactControlPrerequisite fact = fact.controlPrerequisite

rootFactRole :: RootFact -> ControlledRootRole
rootFactRole fact = fact.role

rootFactWriterSequencing :: RootFact -> Maybe NablaSequencing
rootFactWriterSequencing fact = case fact.role of
  ControlledWriter _ sequencing -> Just sequencing
  ControlledReader _ -> Nothing

-- | Opaque proof of the complete bootstrap-only @operate@ rule available in
-- the current reachable profile.  Both the process and writer roles are
-- immutable current bootstrap facts; their exact role entries establish the
-- process's self label, and both objects retain normal possession by that
-- process.  Later transferred-label authority will use a different witness.
data CheckedControlledBootstrapOperate
  = CheckedControlledBootstrapOperate ProcessFact RootFact NablaId

data ControlledBootstrapOperateError
  = ControlledBootstrapOperateProcessUnavailable ProcessEpochId
  | ControlledBootstrapOperateWriterUnavailable NablaId
  | ControlledBootstrapOperateWriterProcessMismatch
      ProcessEpochId
      ProcessEpochId
  | ControlledBootstrapOperateProcessRoleContradiction ProcessEpochId
  | ControlledBootstrapOperateWriterRoleContradiction NablaId
  | ControlledBootstrapOperateProcessNotPossessed ProcessEpochId
  | ControlledBootstrapOperateWriterNotPossessed ProcessEpochId NablaId
  deriving stock (Eq, Show)

checkControlledBootstrapOperate ::
  ProcessEpochId ->
  NablaId ->
  State ->
  Either ControlledBootstrapOperateError CheckedControlledBootstrapOperate
checkControlledBootstrapOperate process writer state = do
  processEntry <-
    maybe
      (Left (ControlledBootstrapOperateProcessUnavailable process))
      Right
      (Map.lookup process state.processes)
  writerEntry <-
    maybe
      (Left (ControlledBootstrapOperateWriterUnavailable writer))
      Right
      (Map.lookup writer state.writers)
  requireBootstrapOperate
    (rootFactProcessEpoch writerEntry == process)
    ( ControlledBootstrapOperateWriterProcessMismatch
        process
        (rootFactProcessEpoch writerEntry)
    )
  requireBootstrapOperate
    ( Map.lookup (globalObjectIdFromProcessEpochId process) state.roles
        == Just (ProcessRole process)
    )
    (ControlledBootstrapOperateProcessRoleContradiction process)
  requireBootstrapOperate
    ( Map.lookup (globalObjectIdFromNablaId writer) state.roles
        == Just (WriterRole writer)
    )
    (ControlledBootstrapOperateWriterRoleContradiction writer)
  requireBootstrapOperate
    ( controlledHasNormalPossession
        process
        (globalObjectIdFromProcessEpochId process)
        state
    )
    (ControlledBootstrapOperateProcessNotPossessed process)
  requireBootstrapOperate
    ( controlledHasNormalPossession
        process
        (globalObjectIdFromNablaId writer)
        state
    )
    (ControlledBootstrapOperateWriterNotPossessed process writer)
  Right (CheckedControlledBootstrapOperate processEntry writerEntry writer)

controlledBootstrapOperateProcess ::
  CheckedControlledBootstrapOperate -> ProcessEpochId
controlledBootstrapOperateProcess
  (CheckedControlledBootstrapOperate process _ _) =
    processFactProcessEpoch process

controlledBootstrapOperateWriter ::
  CheckedControlledBootstrapOperate -> NablaId
controlledBootstrapOperateWriter
  (CheckedControlledBootstrapOperate _ _ writer) = writer

controlledBootstrapOperateSortId ::
  CheckedControlledBootstrapOperate -> SortId
controlledBootstrapOperateSortId
  (CheckedControlledBootstrapOperate _ root _) = rootFactSortId root

controlledBootstrapOperateOccurrenceId ::
  CheckedControlledBootstrapOperate -> SortDefinitionOccurrenceId
controlledBootstrapOperateOccurrenceId
  (CheckedControlledBootstrapOperate _ root _) = rootFactOccurrenceId root

controlledBootstrapOperateAuthority ::
  CheckedControlledBootstrapOperate -> AuthorityEpoch
controlledBootstrapOperateAuthority
  (CheckedControlledBootstrapOperate _ root _) = rootFactAuthority root

controlledBootstrapOperateControlPrerequisite ::
  CheckedControlledBootstrapOperate -> ControlIndex
controlledBootstrapOperateControlPrerequisite
  (CheckedControlledBootstrapOperate _ root _) =
    rootFactControlPrerequisite root

controlledBootstrapOperateWriterSequencing ::
  CheckedControlledBootstrapOperate -> NablaSequencing
controlledBootstrapOperateWriterSequencing
  (CheckedControlledBootstrapOperate _ root _) =
    case rootFactWriterSequencing root of
      Just sequencing -> sequencing
      Nothing -> error "checked bootstrap writer carried a reader root"

-- | Exact writer facts retained by a checked operate resolver.  This is the
-- narrow capability consumed by controlled first-use and update admission;
-- those transitions must not re-look up bootstrap-only RootFacts because a
-- dynamically installed configured writer has structural, not genesis,
-- authority.
data ControlledWriterBinding = ControlledWriterBinding
  { bindingProcess :: ProcessEpochId,
    bindingWriter :: NablaId,
    bindingSortId :: SortId,
    bindingOccurrenceId :: SortDefinitionOccurrenceId,
    bindingAuthority :: AuthorityEpoch,
    bindingControlPrerequisite :: ControlIndex
  }
  deriving stock (Eq, Show)

-- | Package-internal constructor used only by the checked bootstrap/dynamic
-- operate resolvers.  The public Herald and application surfaces do not expose
-- this representation.
controlledWriterBinding ::
  ProcessEpochId ->
  NablaId ->
  SortId ->
  SortDefinitionOccurrenceId ->
  AuthorityEpoch ->
  ControlIndex ->
  ControlledWriterBinding
controlledWriterBinding = ControlledWriterBinding

controlledWriterBindingProcess :: ControlledWriterBinding -> ProcessEpochId
controlledWriterBindingProcess binding = binding.bindingProcess

controlledWriterBindingWriter :: ControlledWriterBinding -> NablaId
controlledWriterBindingWriter binding = binding.bindingWriter

controlledWriterBindingSortId :: ControlledWriterBinding -> SortId
controlledWriterBindingSortId binding = binding.bindingSortId

controlledWriterBindingOccurrenceId ::
  ControlledWriterBinding -> SortDefinitionOccurrenceId
controlledWriterBindingOccurrenceId binding = binding.bindingOccurrenceId

controlledWriterBindingAuthority :: ControlledWriterBinding -> AuthorityEpoch
controlledWriterBindingAuthority binding = binding.bindingAuthority

controlledWriterBindingControlPrerequisite ::
  ControlledWriterBinding -> ControlIndex
controlledWriterBindingControlPrerequisite binding =
  binding.bindingControlPrerequisite

controlledBootstrapOperateWriterBinding ::
  CheckedControlledBootstrapOperate -> ControlledWriterBinding
controlledBootstrapOperateWriterBinding operate =
  controlledWriterBinding
    (controlledBootstrapOperateProcess operate)
    (controlledBootstrapOperateWriter operate)
    (controlledBootstrapOperateSortId operate)
    (controlledBootstrapOperateOccurrenceId operate)
    (controlledBootstrapOperateAuthority operate)
    (controlledBootstrapOperateControlPrerequisite operate)

requireBootstrapOperate ::
  Bool ->
  ControlledBootstrapOperateError ->
  Either ControlledBootstrapOperateError ()
requireBootstrapOperate condition problem
  | condition = Right ()
  | otherwise = Left problem

-- | Coalesced preparation-readiness facts. These are separate from retained
-- object history: one consumer drains them, and new consumers start dirty.
data PreparationChanges = PreparationChanges
  { changedObjects :: !(Set GlobalObjectId),
    changedProcesses :: !(Set ProcessEpochId),
    changedPossessions :: !(Set (ProcessEpochId, GlobalObjectId))
  }
  deriving stock (Eq, Show)

emptyPreparationChanges :: PreparationChanges
emptyPreparationChanges = PreparationChanges Set.empty Set.empty Set.empty

takePreparationChanges :: State -> (PreparationChanges, State)
takePreparationChanges state =
  let changes = state.pendingPreparationChanges
   in changes `seq` (changes, clearPreparationChanges state)

-- | Initial assembly and explicit semantic comparisons only; operational
-- consumers must take the changes before clearing them.
clearPreparationChanges :: State -> State
clearPreparationChanges state = state {pendingPreparationChanges = emptyPreparationChanges}

notifyPreparationObject :: GlobalObjectId -> State -> State
notifyPreparationObject object state =
  state {pendingPreparationChanges = state.pendingPreparationChanges {changedObjects = Set.insert object state.pendingPreparationChanges.changedObjects}}

notifyPreparationProcess :: ProcessEpochId -> State -> State
notifyPreparationProcess process state =
  state {pendingPreparationChanges = state.pendingPreparationChanges {changedProcesses = Set.insert process state.pendingPreparationChanges.changedProcesses}}

notifyPreparationPossessions :: Set (ProcessEpochId, GlobalObjectId) -> State -> State
notifyPreparationPossessions possessions state
  | Set.null possessions = state
  | otherwise =
      -- Set forces a pair only to WHNF. Force both identifiers as well, so a
      -- singleton grant cannot retain its source record through a field thunk.
      Set.foldl' (\() (process, object) -> process `seq` object `seq` ()) () possessions
        `seq` state {pendingPreparationChanges = state.pendingPreparationChanges {changedPossessions = Set.union possessions state.pendingPreparationChanges.changedPossessions}}

data State = State
  { processes :: (Map ProcessEpochId ProcessFact),
    writers :: (Map NablaId RootFact),
    readers :: (Map DeltaId RootFact),
    roles :: (Map GlobalObjectId ControlledRole),
    -- Bootstrap roots and application publishers retain Normal evidence
    -- independently of the application-visible Store sources below.
    possessions :: (Set (ProcessEpochId, GlobalObjectId)),
    storePossessions :: (Map (ProcessEpochId, DeltaId, GlobalObjectId) ReplicaStrength),
    reservations :: (Map GlobalUniqueId Reservation),
    localRecords :: (Map GlobalObjectId ControlledLocalRecord),
    -- Oracle-released label records are the sole live overlay owner.  The raw
    -- publications remain immutable; every effective-label consumer projects
    -- them through this map and the monotone ended-process set.
    releasedLabels :: (Map GlobalObjectId LabelRecord),
    -- Terminal controlled deletion is deliberately payload-free.  Label
    -- deletion and predefined disappearance retain their distinct checked
    -- provenance here while all publication bytes remain with their immutable
    -- Publication/Store-history owners.
    terminalDeletions :: (Map GlobalObjectId ControlledTerminalDeletion),
    -- End is monotone lifecycle knowledge.  Historical records deliberately
    -- continue to name their original possession sources, while every
    -- effective-authority view filters those sources through this set.
    endedProcesses :: (Set ProcessEpochId),
    pendingPreparationChanges :: !PreparationChanges
  }
  deriving stock (Eq)

data ControlledRole
  = ProcessRole ProcessEpochId
  | WriterRole NablaId
  | ReaderRole DeltaId
  deriving stock (Eq, Show)

emptyState :: State
emptyState =
  State
    { processes = Map.empty,
      writers = Map.empty,
      readers = Map.empty,
      roles = Map.empty,
      possessions = Set.empty,
      storePossessions = Map.empty,
      reservations = Map.empty,
      localRecords = Map.empty,
      releasedLabels = Map.empty,
      terminalDeletions = Map.empty,
      endedProcesses = Set.empty,
      pendingPreparationChanges = emptyPreparationChanges
    }

-- | Portable current control facts, captured from an admitted owner. The
-- composed base authenticates their genesis and common control prefix. Local
-- observations, possessions, reservations and preparation notifications are
-- deliberately absent; this value cannot retain the donor owner.
data ControlledControlBase
  = ControlledControlBase
      !(Map GlobalObjectId LabelRecord)
      !(Map GlobalObjectId ControlledTerminalDeletion)
      !(Set ProcessEpochId)
  deriving stock (Eq, Show)

captureControlledControlBase :: State -> ControlledControlBase
captureControlledControlBase state =
  ControlledControlBase state.releasedLabels state.terminalDeletions state.endedProcesses

data ControlledControlBaseDerivationError
  = ControlledControlBaseLabelProblem LabelRecordError
  | ControlledControlBaseCauseProblem StructuralConsequenceCauseError
  | ControlledControlBaseRemovalFailure ControlledRemovalError
  deriving stock (Eq, Show)

-- | Derive the portable current controls from checked canonical facts. Only the
-- last released overlay for an object contributes a stored label; NotApplied
-- results and later process End do not rewrite it. Terminal removals run against
-- an empty local owner seeded with these labels, so their generations follow
-- the ordinary removal rule without importing possessions or replaying history.
controlledControlBaseFromProjection ::
  Projection.State ->
  Either ControlledControlBaseDerivationError ControlledControlBase
controlledControlBaseFromProjection projection = do
  !labels <- Map.traverseWithKey labelRecord latest
  labelCauses <- traverse labelCause (Map.toAscList (Map.filter deleted latest))
  disappearanceCauses <- traverse disappearanceCause controlledDisappearance
  let !ended = Set.fromList (map fst (Projection.projectedEndedProcesses projection))
      !seeded = emptyState {releasedLabels = labels, endedProcesses = ended}
  !removed <- foldM remove seeded (labelCauses <> disappearanceCauses)
  pure (captureControlledControlBase removed)
  where
    latest =
      Map.fromListWith
        (\new old -> if first new > first old then new else old)
        [ (liveDecisionObject (Projection.projectedLabelWorkflowDecision workflow), (index, decision, overlay))
        | (decision, workflow) <- Projection.projectedLabelWorkflows projection,
          Just (Projection.ProjectedLabelReleased index overlay _) <- [Projection.projectedLabelWorkflowTerminal workflow]
        ]
    first (index, _, _) = index
    deleted (_, _, overlay) = releasedLabelStateIsDeleted (releasedLabelOverlayState overlay)
    labelRecord object (_, _, overlay) =
      either (Left . ControlledControlBaseLabelProblem) Right
        $ mkLabelRecord object (releasedLabelOverlayState overlay) (releasedLabelOverlayRevision overlay) (releasedLabelOverlayRetainedAuthority overlay)
    labelCause (object, (index, decision, _)) =
      (object,) <$> either (Left . ControlledControlBaseCauseProblem) Right (labelReleaseCause decision index)
    controlledDisappearance =
      [ (object, probe, index)
      | (probe, projected) <- Projection.projectedDisappearanceProbes projection,
        Just (Projection.ProjectedDisappearanceResolved outcome index) <- [Projection.projectedDisappearanceTerminal projected],
        ControlledDisappearanceResolved object _ _ _ <- [disappearanceResolutionOutcomeView outcome]
      ]
    disappearanceCause (object, probe, index) =
      (object,) <$> either (Left . ControlledControlBaseCauseProblem) Right (predefinedDisappearanceCause probe index)
    remove current (object, cause) =
      commitControlledRemoval <$> either (Left . ControlledControlBaseRemovalFailure) Right (prepareControlledRemoval cause object current)

data ControlledControlBaseImportError
  = ControlledControlBaseAlreadyAdvanced
  | ControlledControlBaseRemovalProblem ControlledRemovalError
  | ControlledControlBaseTerminalGenerationMismatch GlobalObjectId Word64 Word64
  deriving stock (Eq, Show)

newtype PreparedControlledControlBaseImport
  = PreparedControlledControlBaseImport State

-- | Seed a fresh observer's current-control facts, or repeat the exact same
-- import. Composition checks the observer, system and prefix. Apply End and
-- deletion to this receiver's local evidence through their ordinary leaf
-- transitions; never replace that evidence with the donor's possessions or
-- observations. Composition installs receiver-appropriate process metadata
-- separately from its checked Projection and selected genesis descriptions.
prepareControlledControlBaseImport ::
  ControlledControlBase ->
  State ->
  Either ControlledControlBaseImportError PreparedControlledControlBaseImport
prepareControlledControlBaseImport (ControlledControlBase labels deletions ended) state = do
  if fresh || sameControl
    then Right ()
    else Left ControlledControlBaseAlreadyAdvanced
  let labelled
        | sameControl = state
        | otherwise = Set.foldl' (flip notifyPreparationObject) (state {releasedLabels = labels}) (Map.keysSet labels)
      retired = Set.foldl' (\current process -> commitControlledProcessRetirement (prepareControlledProcessRetirement process current)) labelled ended
  removed <- foldM removeObject retired (Map.toAscList deletions)
  pure (PreparedControlledControlBaseImport removed)
  where
    fresh = Map.null state.releasedLabels && Map.null state.terminalDeletions && Set.null state.endedProcesses
    sameControl = state.releasedLabels == labels && state.terminalDeletions == deletions && state.endedProcesses == ended
    removeObject current (object, deletion) = do
      prepared <- either (Left . ControlledControlBaseRemovalProblem) Right (prepareControlledRemoval (controlledTerminalDeletionCause deletion) object current)
      let successor = commitControlledRemoval prepared
          expected = controlledTerminalDeletionGeneration deletion
          actual = maybe 0 controlledTerminalDeletionGeneration (controlledTerminalDeletion object successor)
      if actual == expected
        then Right successor
        else Left (ControlledControlBaseTerminalGenerationMismatch object expected actual)

commitControlledControlBaseImport :: PreparedControlledControlBaseImport -> State
commitControlledControlBaseImport (PreparedControlledControlBaseImport state) = state

-- | Exact checked prior facts captured by live label admission.  The released
-- state is the current effective projection (and may therefore be zombie),
-- while authority remains the retained historical tenure required to derive a
-- later handoff correctly.
data ControlledLabelPrior
  = ControlledLabelPrior
      GlobalObjectId
      ReleasedLabelState
      (Maybe InitialLabelEvidence)
      (Maybe LabelRevision)
      (Maybe AuthorityEpoch)
      (Maybe PriorAuthorityJustification)
  deriving stock (Eq, Show)

controlledLabelPriorObject :: ControlledLabelPrior -> GlobalObjectId
controlledLabelPriorObject (ControlledLabelPrior object _ _ _ _ _) = object

controlledLabelPriorEffectiveState ::
  ControlledLabelPrior -> ReleasedLabelState
controlledLabelPriorEffectiveState (ControlledLabelPrior _ prior _ _ _ _) = prior

-- | Raw checked initial facts, independent of the caller's expected label and
-- before the current End projection. An existing Oracle overlay needs none.
controlledLabelPriorInitialEvidence :: ControlledLabelPrior -> Maybe InitialLabelEvidence
controlledLabelPriorInitialEvidence (ControlledLabelPrior _ _ evidence _ _ _) = evidence

controlledLabelPriorRevision :: ControlledLabelPrior -> Maybe LabelRevision
controlledLabelPriorRevision (ControlledLabelPrior _ _ _ revision _ _) = revision

controlledLabelPriorAuthority ::
  ControlledLabelPrior -> Maybe AuthorityEpoch
controlledLabelPriorAuthority (ControlledLabelPrior _ _ _ _ authority _) = authority

controlledLabelPriorAuthorityJustification ::
  ControlledLabelPrior -> Maybe PriorAuthorityJustification
controlledLabelPriorAuthorityJustification
  (ControlledLabelPrior _ _ _ _ _ justification) = justification

data ControlledLabelPriorError
  = ControlledLabelPriorObjectUnavailable GlobalObjectId
  | ControlledLabelPriorLabelUnavailable GlobalObjectId
  | ControlledLabelPriorDeleted GlobalObjectId LabelRevision
  | ControlledLabelPriorTerminallyDeleted GlobalObjectId
  | ControlledLabelPriorUnbackedLabelAuthority GlobalObjectId AuthorityEpoch
  deriving stock (Eq, Show)

-- | Obtain the exact current label/revision/tenure relation from the one live
-- Controlled owner.  Absence from the released map means a first overlay and
-- is resolved only from the retained checked publication; it is never treated
-- as an empty prior relation.
checkControlledLabelPrior ::
  InitialProjectionDigest ->
  GlobalObjectId ->
  State ->
  Either ControlledLabelPriorError ControlledLabelPrior
checkControlledLabelPrior initialDigest =
  checkControlledLabelPriorWithAuthority initialDigest Nothing

-- | Resolve the checked prior with an optional topology-derived first tenure.
-- Only a Nabla has an initial operation tenure. Other fresh controlled objects
-- do not inherit the publication source's tenure as their own label history.
-- Once an object has a released label record, that exact record remains the
-- optimistic prior regardless of its role.
checkControlledLabelPriorWithAuthority ::
  InitialProjectionDigest ->
  Maybe AuthorityEpoch ->
  GlobalObjectId ->
  State ->
  Either ControlledLabelPriorError ControlledLabelPrior
checkControlledLabelPriorWithAuthority initialDigest suppliedFirstAuthority object state
  | controlledObjectTerminallyDeleted object state =
      Left (ControlledLabelPriorTerminallyDeleted object)
  | otherwise = case Map.lookup object state.releasedLabels of
      Just record -> do
        let revision = labelRecordRevision record
            authority = labelRecordRetainedAuthority record
        prior <- case labelRecordReleasedState record of
          retained
            | releasedLabelStateIsDeleted retained ->
                Left (ControlledLabelPriorDeleted object revision)
            | otherwise -> Right (effectiveReleasedState state retained)
        Right
          ( ControlledLabelPrior
              object
              prior
              Nothing
              (Just revision)
              authority
              (existingReleasedAuthority revision <$ authority)
          )
      Nothing -> do
        (observed, embeddedAuthority, initialEvidence) <-
          case controlledLocalRecord object state of
            Just record -> do
              label <-
                maybe
                  (Left (ControlledLabelPriorLabelUnavailable object))
                  Right
                  (controlledRecordObservedLabel record)
              Right
                ( label,
                  publicationAuthorityEpoch
                    (checkedPublicationId (controlledRecordLatestPublication record)),
                  initialPublicationLabelEvidence
                    object
                    label
                    (checkedPublicationId (controlledRecordFirstPublication record))
                )
            Nothing -> do
              root <-
                maybe
                  (Left (ControlledLabelPriorObjectUnavailable object))
                  Right
                  (controlledBootstrapRootFact object state)
              Right
                ( (ProcessLabel (rootFactProcessEpoch root), 0),
                  rootFactAuthority root,
                  Just (initialBootstrapLabelEvidence object (rootFactProcessEpoch root))
                )
        let authority =
              if controlledCurrentStructuralRole object state == Just NablaCarrier
                then Just (maybe embeddedAuthority id suppliedFirstAuthority)
                else Nothing
        justification <- traverse (firstAuthorityJustification object initialDigest) authority
        Right
          ( ControlledLabelPrior
              object
              (effectiveReleasedState state (releasedLabel observed))
              initialEvidence
              Nothing
              authority
              justification
          )

-- | Whether one locally owned object can enter the label workflow.  Dynamic
-- controlled objects must still have a current labelled winner; immutable
-- bootstrap Nabla/Delta roots obtain their initial label and authority from
-- the checked root fact rather than from a synthetic publication record.
controlledObjectLabelable :: GlobalObjectId -> State -> Bool
controlledObjectLabelable object state
  | controlledObjectReleasedDeleted object state = False
  | otherwise =
      case controlledLocalRecord object state of
        Just record ->
          controlledRecordLifecycle record == ControlledCurrent
            && controlledRecordObservedLabel record /= Nothing
            && controlledRecordStructuralRole record /= Just ProcessEpochCarrier
        Nothing -> case Map.lookup object state.roles of
          Just (WriterRole _) -> True
          Just (ReaderRole _) -> True
          _ -> False

firstAuthorityJustification ::
  GlobalObjectId ->
  InitialProjectionDigest ->
  AuthorityEpoch ->
  Either ControlledLabelPriorError PriorAuthorityJustification
firstAuthorityJustification object initialDigest authority =
  case authorityEpochView authority of
    GenesisAuthorityEpochView -> Right (checkedGenesisAuthority initialDigest)
    StructuralAuthorityEpochView occurrence cut ->
      Right (establishedStructuralAuthority occurrence cut)
    LabelAuthorityEpochView _ ->
      Left (ControlledLabelPriorUnbackedLabelAuthority object authority)

effectiveReleasedState :: State -> ReleasedLabelState -> ReleasedLabelState
effectiveReleasedState state retained = case releasedLabelStateView retained of
  ReleasedLabelView label ->
    releasedLabel (effectiveProcessLabel (`controlledProcessEnded` state) label)
  ReleasedDeletedView _ -> retained

controlledReleasedLabelRecord ::
  GlobalObjectId -> State -> Maybe LabelRecord
controlledReleasedLabelRecord object state =
  Map.lookup object state.releasedLabels

controlledReleasedLabelEntries :: State -> [(GlobalObjectId, LabelRecord)]
controlledReleasedLabelEntries = Map.toAscList . (.releasedLabels)

-- | Resolve the currently effective released-label state for one locally
-- known object.  A live Oracle overlay wins over the embedded observation;
-- without an overlay the retained observation supplies the initial state.
-- Process End is projected in either case, while a released delete remains
-- terminal.  Immutable publication bytes are never rewritten here.
controlledEffectiveLabelState ::
  GlobalObjectId -> State -> Maybe ReleasedLabelState
controlledEffectiveLabelState object state
  | Just terminal <- controlledTerminalDeletion object state =
      Just (releasedDeleted (controlledTerminalDeletionGeneration terminal))
  | otherwise =
      effectiveReleasedState state
        <$> case Map.lookup object state.releasedLabels of
          Just record -> Just (labelRecordReleasedState record)
          Nothing ->
            case controlledLocalRecord object state of
              Just record -> releasedLabel <$> controlledRecordObservedLabel record
              Nothing ->
                (\root -> releasedLabel (ProcessLabel (rootFactProcessEpoch root), 0))
                  <$> controlledBootstrapRootFact object state

data ControlledLabelReleaseDisposition
  = ControlledLabelReleaseInstalled
  | ControlledLabelReleaseExactReplay
  deriving stock (Eq, Ord, Show)

data ControlledLabelReleaseError
  = ControlledLabelReleaseInvalidRevision
  | ControlledLabelDecisionIndexMismatch ControlIndex ControlIndex
  | ControlledLabelReleasePriorProblem ControlledLabelPriorError
  | ControlledLabelReleasePriorStateMismatch ReleasedLabelState ReleasedLabelState
  | ControlledLabelReleasePriorRevisionMismatch
      (Maybe LabelRevision)
      (Maybe LabelRevision)
  | ControlledLabelReleasePriorAuthorityMismatch
      (Maybe AuthorityEpoch)
      (Maybe AuthorityEpoch)
  | ControlledLabelReleasePriorJustificationMismatch
      (Maybe PriorAuthorityJustification)
      (Maybe PriorAuthorityJustification)
  | ControlledLabelReleaseRecordProblem LabelRecordError
  | ControlledLabelReleaseRecordConflict GlobalObjectId LabelRevision
  deriving stock (Eq, Show)

data PreparedControlledLabelRelease
  = PreparedControlledLabelRelease State ControlledLabelReleaseDisposition

-- | Install one canonical Oracle Release. Check the complete prepared prior
-- wherever this receiver has local evidence. A first canonical overlay may
-- arrive before this receiver learns the object; its absence is not a second
-- CAS. Application admission still requires the separate strict prior lookup.
-- Authority is realized only from the checked Domain disposition; a label tenure
-- is minted at the supplied positive release index and nowhere else.
prepareControlledLabelRelease ::
  InitialProjectionDigest ->
  PreparedLabelFacts ->
  ControlIndex ->
  State ->
  Either ControlledLabelReleaseError PreparedControlledLabelRelease
prepareControlledLabelRelease initialDigest =
  prepareControlledLabelReleaseWithAuthority initialDigest Nothing

prepareControlledLabelReleaseWithAuthority ::
  InitialProjectionDigest ->
  Maybe AuthorityEpoch ->
  PreparedLabelFacts ->
  ControlIndex ->
  State ->
  Either ControlledLabelReleaseError PreparedControlledLabelRelease
prepareControlledLabelReleaseWithAuthority initialDigest suppliedFirstAuthority facts releaseIndex state = do
  revision <-
    either
      (const (Left ControlledLabelReleaseInvalidRevision))
      Right
      (mkLabelRevision releaseIndex)
  let resolveIndex = preparedLabelFactsResolveIndex facts
  if releaseIndex == resolveIndex
    then Right ()
    else
      Left
        (ControlledLabelDecisionIndexMismatch resolveIndex releaseIndex)
  let object = preparedLabelFactsObjectId facts
  case Map.lookup object state.releasedLabels of
    Just incumbent
      | labelRecordRevision incumbent == revision -> do
          expected <- releasedRecord revision facts
          if incumbent == expected
            then
              Right
                ( PreparedControlledLabelRelease
                    state
                    ControlledLabelReleaseExactReplay
                )
            else Left (ControlledLabelReleaseRecordConflict object revision)
    _ -> do
      case checkControlledLabelPriorWithAuthority initialDigest suppliedFirstAuthority object state of
        -- The composition has already authenticated the exact canonical Release
        -- against its Oracle projection. It is authoritative even at a member
        -- outside the publication's frozen route. Do not reuse this exception for
        -- application admission or conceal a missing earlier released record.
        Left (ControlledLabelPriorObjectUnavailable _)
          | preparedLabelFactsExpectedPriorRevision facts == Nothing -> Right ()
        Left problem -> Left (ControlledLabelReleasePriorProblem problem)
        Right prior -> requireReleasedPrior facts prior
      record <- releasedRecord revision facts
      Right
        ( PreparedControlledLabelRelease
            ( notifyPreparationObject
                object
                state
                  { releasedLabels = Map.insert object record state.releasedLabels
                  }
            )
            ControlledLabelReleaseInstalled
        )

releasedRecord ::
  LabelRevision ->
  PreparedLabelFacts ->
  Either ControlledLabelReleaseError LabelRecord
releasedRecord revision facts =
  either
    (Left . ControlledLabelReleaseRecordProblem)
    Right
    ( mkLabelRecord
        (preparedLabelFactsObjectId facts)
        (preparedLabelFactsProposedOutcome facts)
        revision
        (releasedAuthority revision (preparedLabelFactsAuthorityDisposition facts))
    )

releasedAuthority ::
  LabelRevision ->
  PreparedAuthorityDisposition ->
  Maybe AuthorityEpoch
releasedAuthority revision disposition =
  case preparedAuthorityDispositionView disposition of
    NoTargetAuthorityView -> Nothing
    RetainPriorAuthorityView authority -> Just authority
    DeriveAuthorityAtReleaseView ->
      Just (labelAuthorityEpoch (labelRevisionControlIndex revision))
    RetirePriorAuthorityView authority -> Just authority

requireReleasedPrior ::
  PreparedLabelFacts ->
  ControlledLabelPrior ->
  Either ControlledLabelReleaseError ()
requireReleasedPrior facts prior
  | preparedLabelFactsExpectedPriorState facts
      /= controlledLabelPriorEffectiveState prior =
      Left
        ( ControlledLabelReleasePriorStateMismatch
            (preparedLabelFactsExpectedPriorState facts)
            (controlledLabelPriorEffectiveState prior)
        )
  | preparedLabelFactsExpectedPriorRevision facts
      /= controlledLabelPriorRevision prior =
      Left
        ( ControlledLabelReleasePriorRevisionMismatch
            (preparedLabelFactsExpectedPriorRevision facts)
            (controlledLabelPriorRevision prior)
        )
  | preparedLabelFactsExpectedPriorAuthority facts
      /= controlledLabelPriorAuthority prior =
      Left
        ( ControlledLabelReleasePriorAuthorityMismatch
            (preparedLabelFactsExpectedPriorAuthority facts)
            (controlledLabelPriorAuthority prior)
        )
  | preparedLabelFactsPriorAuthorityJustification facts
      /= controlledLabelPriorAuthorityJustification prior =
      Left
        ( ControlledLabelReleasePriorJustificationMismatch
            (preparedLabelFactsPriorAuthorityJustification facts)
            (controlledLabelPriorAuthorityJustification prior)
        )
  | otherwise = Right ()

preparedControlledLabelReleaseDisposition ::
  PreparedControlledLabelRelease -> ControlledLabelReleaseDisposition
preparedControlledLabelReleaseDisposition
  (PreparedControlledLabelRelease _ disposition) = disposition

commitControlledLabelRelease :: PreparedControlledLabelRelease -> State
commitControlledLabelRelease (PreparedControlledLabelRelease successor _) =
  successor

-- | Payload-free, monotone suppression for one globally unique controlled
-- identity.  The cause remains authority-qualified: a label decision and a
-- disappearance probe can therefore never masquerade as one another. The
-- terminal generation is captured from the released successor for Label delete,
-- or from the current label for automatic disappearance.
data ControlledTerminalDeletion
  = ControlledTerminalDeletion GlobalObjectId StructuralConsequenceCause Word64
  deriving stock (Eq, Show)

controlledTerminalDeletion ::
  GlobalObjectId -> State -> Maybe ControlledTerminalDeletion
controlledTerminalDeletion object state =
  Map.lookup object state.terminalDeletions

controlledTerminalDeletionObject ::
  ControlledTerminalDeletion -> GlobalObjectId
controlledTerminalDeletionObject (ControlledTerminalDeletion object _ _) = object

controlledTerminalDeletionCause ::
  ControlledTerminalDeletion -> StructuralConsequenceCause
controlledTerminalDeletionCause (ControlledTerminalDeletion _ cause _) = cause

controlledTerminalDeletionGeneration :: ControlledTerminalDeletion -> Word64
controlledTerminalDeletionGeneration (ControlledTerminalDeletion _ _ generation) = generation

controlledTerminalDeletionEntries ::
  State -> [(GlobalObjectId, ControlledTerminalDeletion)]
controlledTerminalDeletionEntries = Map.toAscList . (.terminalDeletions)

controlledObjectTerminallyDeleted :: GlobalObjectId -> State -> Bool
controlledObjectTerminallyDeleted object state =
  Map.member object state.terminalDeletions

-- | Ephemeral coordinate used by the Store owner to remove the effective
-- retained payload.  It is returned only by successful preparation and is not
-- retained in the payload-free terminal record.
data ControlledRemovalPayloadCoordinate
  = ControlledRemovalPayloadCoordinate SortId ObjectKey
  deriving stock (Eq, Show)

controlledRemovalPayloadSortId ::
  ControlledRemovalPayloadCoordinate -> SortId
controlledRemovalPayloadSortId (ControlledRemovalPayloadCoordinate sortId _) =
  sortId

controlledRemovalPayloadObjectKey ::
  ControlledRemovalPayloadCoordinate -> ObjectKey
controlledRemovalPayloadObjectKey (ControlledRemovalPayloadCoordinate _ key) = key

data ControlledRemovalDisposition
  = ControlledRemovalInstalled
  | ControlledRemovalExactReplay
  deriving stock (Eq, Ord, Show)

data ControlledRemovalError
  = ControlledRemovalCauseNotTerminal StructuralConsequenceCause
  | ControlledRemovalCauseIndexMismatch ControlIndex ControlIndex
  | ControlledRemovalLabelRecordMissing GlobalObjectId
  | ControlledRemovalLabelRecordNotDeleted GlobalObjectId
  | ControlledRemovalLabelRevisionMismatch ControlIndex ControlIndex
  | ControlledRemovalAlreadyLabelDeleted GlobalObjectId
  | ControlledRemovalCauseConflict
      GlobalObjectId
      StructuralConsequenceCause
      StructuralConsequenceCause
  deriving stock (Eq, Show)

data PreparedControlledRemoval
  = PreparedControlledRemoval
      State
      ControlledRemovalDisposition
      (Maybe ControlledRemovalPayloadCoordinate)
      Word64
      Word64

-- | Prepare terminal suppression and revoke every effective publisher/Store
-- possession in one owner successor.  Publication bytes are intentionally not
-- copied into the retained tombstone; the transient Store coordinate is
-- exposed only to the enclosing atomic applicator.
prepareControlledRemoval ::
  StructuralConsequenceCause ->
  GlobalObjectId ->
  State ->
  Either ControlledRemovalError PreparedControlledRemoval
prepareControlledRemoval cause object state = do
  validateRemovalCause cause object state
  case Map.lookup object state.terminalDeletions of
    Just incumbent
      | controlledTerminalDeletionCause incumbent == cause ->
          Right
            ( PreparedControlledRemoval
                state
                ControlledRemovalExactReplay
                Nothing
                0
                0
            )
      | otherwise ->
          Left
            ( ControlledRemovalCauseConflict
                object
                (controlledTerminalDeletionCause incumbent)
                cause
            )
    Nothing ->
      Right
        ( PreparedControlledRemoval
            successor
            ControlledRemovalInstalled
            payload
            (fromIntegral (Set.size directRevoked))
            (fromIntegral (Map.size storeRevoked))
        )
  where
    terminalGeneration = maybe 0 releasedLabelStateGeneration (controlledEffectiveLabelState object state)
    directRevoked = Set.filter ((== object) . snd) state.possessions
    storeRevoked =
      Map.filterWithKey
        (\(_, _, retainedObject) _ -> retainedObject == object)
        state.storePossessions
    payload =
      case Map.lookup object state.localRecords of
        Just record ->
          let publication = controlledRecordLatestPublication record
           in Just
                ( ControlledRemovalPayloadCoordinate
                    (checkedPublicationSort publication)
                    (checkedPublicationKey publication)
                )
        Nothing -> bootstrapPayload
    bootstrapPayload =
      ( \role ->
          ControlledRemovalPayloadCoordinate
            (profileSortFor role)
            (controlledObjectKey (globalUniqueIdFromGlobalObjectId object))
      )
        <$> case Map.lookup object state.roles of
          Just ProcessRole {} -> Just ProcessEpochRole
          Just WriterRole {} -> Just NablaRole
          Just ReaderRole {} -> Just DeltaRole
          Nothing -> Nothing
    successor =
      notifyPreparationObject
        object
        state
          { possessions = Set.difference state.possessions directRevoked,
            storePossessions =
              Map.difference state.storePossessions storeRevoked,
            localRecords = Map.delete object state.localRecords,
            terminalDeletions =
              Map.insert
                object
                (ControlledTerminalDeletion object cause terminalGeneration)
                state.terminalDeletions
          }

validateRemovalCause ::
  StructuralConsequenceCause ->
  GlobalObjectId ->
  State ->
  Either ControlledRemovalError ()
validateRemovalCause cause object state =
  case structuralConsequenceCauseView cause of
    LabelReleaseCauseView _ causeIndex -> do
      record <-
        maybe
          (Left (ControlledRemovalLabelRecordMissing object))
          Right
          (Map.lookup object state.releasedLabels)
      let revisionIndex = labelRevisionControlIndex (labelRecordRevision record)
      if revisionIndex == causeIndex
        then Right ()
        else
          Left
            (ControlledRemovalLabelRevisionMismatch causeIndex revisionIndex)
      case releasedLabelStateView (labelRecordReleasedState record) of
        ReleasedDeletedView {} -> Right ()
        ReleasedLabelView {} ->
          Left (ControlledRemovalLabelRecordNotDeleted object)
    PredefinedDisappearanceCauseView _ causeIndex -> do
      case structuralConsequenceCauseControlIndex cause of
        Just retained
          | retained == causeIndex -> Right ()
          | otherwise ->
              Left (ControlledRemovalCauseIndexMismatch causeIndex retained)
        Nothing -> Left (ControlledRemovalCauseNotTerminal cause)
      case Map.lookup object state.releasedLabels of
        Just record
          | releasedLabelStateIsDeleted (labelRecordReleasedState record) ->
              Left (ControlledRemovalAlreadyLabelDeleted object)
        _ -> Right ()
    StructuralOccurrenceCauseView {} ->
      Left (ControlledRemovalCauseNotTerminal cause)
    ProcessEndCauseView {} ->
      Left (ControlledRemovalCauseNotTerminal cause)

preparedControlledRemovalDisposition ::
  PreparedControlledRemoval -> ControlledRemovalDisposition
preparedControlledRemovalDisposition
  (PreparedControlledRemoval _ disposition _ _ _) = disposition

preparedControlledRemovalPayloadCoordinate ::
  PreparedControlledRemoval -> Maybe ControlledRemovalPayloadCoordinate
preparedControlledRemovalPayloadCoordinate
  (PreparedControlledRemoval _ _ payload _ _) = payload

preparedControlledRemovalRevokedDirectPossessions ::
  PreparedControlledRemoval -> Word64
preparedControlledRemovalRevokedDirectPossessions
  (PreparedControlledRemoval _ _ _ revoked _) = revoked

preparedControlledRemovalRevokedStorePossessions ::
  PreparedControlledRemoval -> Word64
preparedControlledRemovalRevokedStorePossessions
  (PreparedControlledRemoval _ _ _ _ revoked) = revoked

commitControlledRemoval :: PreparedControlledRemoval -> State
commitControlledRemoval (PreparedControlledRemoval successor _ _ _ _) = successor

data ReservationPhase
  = Reserved
  | ConsumedByDelete
  | Published
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A pristine generated identity freezes its nominal sort. The immutable,
-- non-reused nabla names the writer root that owns the currently effective
-- sort-definition occurrence, so duplicating that occurrence here would create
-- a second authority for the same fact.
data Reservation = Reservation
  { process :: ProcessEpochId,
    nabla :: NablaId,
    sortId :: SortId,
    authority :: AuthorityEpoch,
    use :: ReservationUse
  }
  deriving stock (Eq)

-- | The first-use winner is stored in the phase itself so a published
-- reservation without its exact publication identity is unrepresentable.
data ReservationUse
  = ReservationOpen
  | ReservationPublished PublicationId
  | ReservationConsumed
  deriving stock (Eq, Show)

-- | Read-only reservation evidence for composed invariants and properties.
data ReservationWitness = ReservationWitness
  { globalUniqueId :: GlobalUniqueId,
    process :: ProcessEpochId,
    nabla :: NablaId,
    sortId :: SortId,
    authority :: AuthorityEpoch,
    use :: ReservationUse
  }
  deriving stock (Eq, Show)

reservationWitnessGlobalUniqueId :: ReservationWitness -> GlobalUniqueId
reservationWitnessGlobalUniqueId witness = witness.globalUniqueId

reservationWitnessProcess :: ReservationWitness -> ProcessEpochId
reservationWitnessProcess witness = witness.process

reservationWitnessNabla :: ReservationWitness -> NablaId
reservationWitnessNabla witness = witness.nabla

reservationWitnessSortId :: ReservationWitness -> SortId
reservationWitnessSortId witness = witness.sortId

reservationWitnessAuthority :: ReservationWitness -> AuthorityEpoch
reservationWitnessAuthority witness = witness.authority

reservationWitnessPhase :: ReservationWitness -> ReservationPhase
reservationWitnessPhase witness = reservationPhase witness.use

reservationWitnessFirstPublicationId :: ReservationWitness -> Maybe PublicationId
reservationWitnessFirstPublicationId witness = reservationFirstPublicationId witness.use

controlledReservationWitnesses :: State -> [ReservationWitness]
controlledReservationWitnesses state =
  [ ReservationWitness
      global
      reservation.process
      reservation.nabla
      reservation.sortId
      reservation.authority
      reservation.use
  | (global, reservation) <- Map.toAscList state.reservations
  ]

controlledReservationWitness :: GlobalUniqueId -> State -> Maybe ReservationWitness
controlledReservationWitness global state = do
  reservation <- Map.lookup global state.reservations
  pure
    ( ReservationWitness
        global
        reservation.process
        reservation.nabla
        reservation.sortId
        reservation.authority
        reservation.use
    )

newtype PreparedControlledReservation = PreparedControlledReservation State

-- | Install one already admitted pristine reservation. Generated-ID freshness
-- is supplied by the generator and finite benign-run premise, not rescanned by
-- this owner.
prepareControlledReservation ::
  ProcessEpochId ->
  NablaId ->
  SortId ->
  AuthorityEpoch ->
  GlobalUniqueId ->
  State ->
  PreparedControlledReservation
prepareControlledReservation process nabla sortId authority generated state =
  PreparedControlledReservation
    ( if Set.member process state.endedProcesses
        then state
        else
          state
            { reservations =
                Map.insert
                  generated
                  Reservation
                    { process,
                      nabla,
                      sortId,
                      authority,
                      use = ReservationOpen
                    }
                  state.reservations
            }
    )

commitControlledReservation :: PreparedControlledReservation -> State
commitControlledReservation (PreparedControlledReservation successor) = successor

data ControlledReservationDeleteError
  = ControlledReservationUnavailable
  | ControlledReservationAuthorityContradiction
  deriving stock (Eq, Show)

newtype PreparedControlledReservationDelete
  = PreparedControlledReservationDelete State

prepareControlledReservationDelete ::
  ProcessEpochId ->
  NablaId ->
  AuthorityEpoch ->
  GlobalUniqueId ->
  State ->
  Either ControlledReservationDeleteError PreparedControlledReservationDelete
prepareControlledReservationDelete process nabla authority generated state =
  case Map.lookup generated state.reservations of
    Nothing -> Left ControlledReservationUnavailable
    Just reservation
      | reservation.process /= process
          || reservation.nabla /= nabla
          || reservation.use /= ReservationOpen ->
          Left ControlledReservationUnavailable
      | reservation.authority /= authority ->
          Left ControlledReservationAuthorityContradiction
      | otherwise ->
          Right
            ( PreparedControlledReservationDelete
                state
                  { reservations =
                      Map.insert
                        generated
                        ( Reservation
                            { process = reservation.process,
                              nabla = reservation.nabla,
                              sortId = reservation.sortId,
                              authority = reservation.authority,
                              use = ReservationConsumed
                            }
                        )
                        state.reservations
                  }
            )

commitControlledReservationDelete :: PreparedControlledReservationDelete -> State
commitControlledReservationDelete (PreparedControlledReservationDelete successor) =
  successor

-- | Complete cancellation of the pristine reservations associated with one
-- exact writer identity. Published and delete-consumed reservations are
-- immutable history and remain outside this transition's write set.
data PreparedControlledNablaReservationRetirement
  = PreparedControlledNablaReservationRetirement
      State
      [GlobalUniqueId]

-- | Select and remove every pristine reservation for the supplied Nabla in one
-- pure batch. The cancellation report follows canonical GlobalUniqueId order.
-- Preparing the same retirement against its committed successor is an
-- unchanged exact retry with an empty report.
prepareControlledNablaReservationRetirement ::
  NablaId ->
  State ->
  PreparedControlledNablaReservationRetirement
prepareControlledNablaReservationRetirement target state =
  PreparedControlledNablaReservationRetirement successor cancelled
  where
    (cancelledReservations, retainedReservations) =
      Map.partition
        ( \reservation ->
            reservation.nabla == target
              && reservation.use == ReservationOpen
        )
        state.reservations
    cancelled = Map.keys cancelledReservations
    successor = state {reservations = retainedReservations}

preparedControlledNablaReservationRetirementCancelled ::
  PreparedControlledNablaReservationRetirement ->
  [GlobalUniqueId]
preparedControlledNablaReservationRetirementCancelled
  (PreparedControlledNablaReservationRetirement _ cancelled) =
    cancelled

commitControlledNablaReservationRetirement ::
  PreparedControlledNablaReservationRetirement ->
  (State, [GlobalUniqueId])
commitControlledNablaReservationRetirement
  (PreparedControlledNablaReservationRetirement successor cancelled) =
    (successor, cancelled)

-- | Complete revocation of one ended process's effective controlled authority.
-- Process/root facts, local records, publication observations, and already used
-- reservations are immutable history and therefore remain outside this write
-- set.
newtype PreparedControlledProcessRetirement
  = PreparedControlledProcessRetirement State

-- | Remove every direct and Store-derived possession of the process and cancel
-- only its pristine reservations.  Repeating the preparation is an unchanged
-- exact retry.
prepareControlledProcessRetirement ::
  ProcessEpochId ->
  State ->
  PreparedControlledProcessRetirement
prepareControlledProcessRetirement process state =
  PreparedControlledProcessRetirement
    (notifyRetirement state)
      { possessions =
          Set.filter
            (\(owner, object) -> owner /= process && object /= globalObjectIdFromProcessEpochId process)
            state.possessions,
        storePossessions =
          Map.filterWithKey
            (\(owner, _, object) _ -> owner /= process && object /= globalObjectIdFromProcessEpochId process)
            state.storePossessions,
        reservations =
          Map.filter
            ( \reservation ->
                reservation.process /= process
                  || reservation.use /= ReservationOpen
            )
            state.reservations,
        endedProcesses = Set.insert process state.endedProcesses
      }
  where
    notifyRetirement
      | Set.member process state.endedProcesses = id
      | otherwise = notifyPreparationProcess process . notifyPreparationObject (globalObjectIdFromProcessEpochId process)

commitControlledProcessRetirement ::
  PreparedControlledProcessRetirement ->
  State
commitControlledProcessRetirement
  (PreparedControlledProcessRetirement successor) =
    successor

-- | Monotone local lifecycle knowledge for one controlled identity.  Reserved
-- IDs do not have a record; deletion of a reservation likewise creates none.
data ControlledLifecycle
  = ControlledCurrent
  | ControlledObsolete
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Hidden state that prevents an older/current publication from resurrecting
-- an obsolete controlled identity.
data ControlledSuppression
  = ControlledUnsuppressed
  | ControlledObsoleteSuppression PublicationWinnerKey
  deriving stock (Eq, Ord, Show)

-- | The winner and lifecycle are one indivisible fact.  Keeping them in a sum
-- makes an obsolete lifecycle with a current winner (or the converse)
-- unrepresentable inside this owner.
data ControlledStatus
  = ControlledCurrentWinner CheckedPublication
  | ControlledObsoleteWinner CheckedPublication
  deriving stock (Eq, Show)

-- | Complete finite-run local knowledge for one controlled identity.  The
-- checked observations map is retained so an equal PublicationId can be
-- classified as an exact replay or an impossible conflicting payload even
-- after a later winner has advanced.
data ControlledLocalRecord = ControlledLocalRecord
  { objectId :: GlobalObjectId,
    sortId :: SortId,
    occurrenceId :: SortDefinitionOccurrenceId,
    observedLabel :: Maybe Label,
    firstPublication :: CheckedPublication,
    status :: ControlledStatus,
    structuralRole :: Maybe StructuralCarrierRole,
    obsoleteObservedAt :: Maybe MonotonicInstant,
    possessionSources :: Set ProcessEpochId,
    observations :: Map PublicationId CheckedPublication
  }
  deriving stock (Eq, Show)

controlledLocalRecord :: GlobalObjectId -> State -> Maybe ControlledLocalRecord
controlledLocalRecord object state = Map.lookup object state.localRecords

controlledLocalRecords :: State -> [ControlledLocalRecord]
controlledLocalRecords state = Map.elems state.localRecords

controlledRecordObjectId :: ControlledLocalRecord -> GlobalObjectId
controlledRecordObjectId record = record.objectId

controlledRecordSortId :: ControlledLocalRecord -> SortId
controlledRecordSortId record = record.sortId

controlledRecordOccurrenceId :: ControlledLocalRecord -> SortDefinitionOccurrenceId
controlledRecordOccurrenceId record = record.occurrenceId

controlledRecordLifecycle :: ControlledLocalRecord -> ControlledLifecycle
controlledRecordLifecycle record = case record.status of
  ControlledCurrentWinner _ -> ControlledCurrent
  ControlledObsoleteWinner _ -> ControlledObsolete

controlledRecordObservedLabel :: ControlledLocalRecord -> Maybe Label
controlledRecordObservedLabel record = record.observedLabel

controlledRecordFirstPublication :: ControlledLocalRecord -> CheckedPublication
controlledRecordFirstPublication record = record.firstPublication

controlledRecordLatestPublication :: ControlledLocalRecord -> CheckedPublication
controlledRecordLatestPublication record = case record.status of
  ControlledCurrentWinner publication -> publication
  ControlledObsoleteWinner publication -> publication

controlledRecordLatestWinner :: ControlledLocalRecord -> PublicationWinnerKey
controlledRecordLatestWinner = checkedPublicationWinnerKey . controlledRecordLatestPublication

controlledRecordStructuralRole :: ControlledLocalRecord -> Maybe StructuralCarrierRole
controlledRecordStructuralRole record = record.structuralRole

controlledRecordSuppression :: ControlledLocalRecord -> ControlledSuppression
controlledRecordSuppression record = case record.status of
  ControlledCurrentWinner _ -> ControlledUnsuppressed
  ControlledObsoleteWinner publication ->
    ControlledObsoleteSuppression (checkedPublicationWinnerKey publication)

-- | The first local monotonic instant at which an obsolete publication was
-- observed as this object's winner.  A retained obsolete payload without this
-- value has not yet passed through the explicit runtime-observation seam.
controlledRecordObsoleteObservedAt :: ControlledLocalRecord -> Maybe MonotonicInstant
controlledRecordObsoleteObservedAt record = record.obsoleteObservedAt

controlledRecordPossessionSources :: ControlledLocalRecord -> Set ProcessEpochId
controlledRecordPossessionSources record = record.possessionSources

-- | Exact retained evidence for any observed publication, including one that
-- is neither the first observation nor the current winner.
controlledRecordObservation ::
  PublicationId ->
  ControlledLocalRecord ->
  Maybe CheckedPublication
controlledRecordObservation identifier record =
  Map.lookup identifier record.observations

-- | The raw label belongs to authenticated publication history; the released
-- overlay remains the sole current label authority. An unseen publication from
-- an older generation can arrive after another caller's label has released.
-- Retain it under the ordinary winner rules without restoring its old owner.
-- At the current generation, only the same pair after known End projection is
-- compatible. Exact retained evidence remains readable and replayable.
controlledRecordObservationLabelCompatible ::
  ControlledLocalRecord ->
  CheckedControlledObservation ->
  State ->
  Bool
controlledRecordObservationLabelCompatible record observation state =
  Map.lookup (checkedPublicationId checked) record.observations == Just checked
    || case record.observedLabel of
      Nothing -> observed == Nothing
      Just _ ->
        observed == record.observedLabel
          || case controlledEffectiveLabelState record.objectId state of
            Just released -> case releasedLabelStateView released of
              ReleasedLabelView label -> case observed of
                Just supplied ->
                  normalize supplied == normalize label
                    || ( Map.member record.objectId state.releasedLabels
                           && snd supplied < snd label
                       )
                Nothing -> False
              ReleasedDeletedView {} -> False
            Nothing -> False
  where
    checked = checkedControlledObservationPublication observation
    observed = checkedControlledObservationLabel observation
    normalize = effectiveProcessLabel (`controlledProcessEnded` state)

-- | One controlled publication observation whose object, sort, label, and
-- structural role have all been derived from the same canonical descriptor and
-- checked value.  Callers cannot independently pair those facts.
data CheckedControlledObservation = CheckedControlledObservation
  { object :: GlobalObjectId,
    occurrence :: SortDefinitionOccurrenceId,
    label :: Maybe Label,
    structuralRole :: Maybe StructuralCarrierRole,
    publication :: CheckedPublication
  }
  deriving stock (Eq, Show)

data ControlledObservationError
  = ControlledObservationNotControlled
  | ControlledObservationSortMismatch SortId SortId
  | ControlledObservationKeyShapeContradiction
  | ControlledObservationLabelShapeContradiction
  deriving stock (Eq, Show)

checkControlledObservation ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  Either ControlledObservationError CheckedControlledObservation
checkControlledObservation descriptor occurrence publication = do
  let expectedSort = descriptorSortId descriptor
      actualSort = checkedPublicationSort publication
      checkedDescriptor = canonicalCheckedDescriptor descriptor
      value = checkedPublicationValue publication
  requireObservation
    (descriptorKind checkedDescriptor == ControlledSort)
    ControlledObservationNotControlled
  requireObservation
    (expectedSort == actualSort)
    (ControlledObservationSortMismatch expectedSort actualSort)
  keyProjection <-
    maybe
      (Left ControlledObservationKeyShapeContradiction)
      Right
      (descriptorControlledKeyProjection checkedDescriptor)
  global <- case viewValue <$> valueAt keyProjection value of
    Right (GlobalUniqueIdValue identifier) -> Right identifier
    _ -> Left ControlledObservationKeyShapeContradiction
  observedLabel <- case descriptorLabelField checkedDescriptor of
    Nothing -> Right Nothing
    Just field -> case viewValue <$> valueAt (directProjection field) value of
      Right (LabelValue label) -> Right (Just label)
      _ -> Left ControlledObservationLabelShapeContradiction
  Right
    CheckedControlledObservation
      { object = globalObjectIdFromGlobalUniqueId global,
        occurrence,
        label = observedLabel,
        structuralRole = descriptorStructuralCarrierRole checkedDescriptor,
        publication
      }

checkedControlledObservationObject :: CheckedControlledObservation -> GlobalObjectId
checkedControlledObservationObject observation = observation.object

checkedControlledObservationOccurrence :: CheckedControlledObservation -> SortDefinitionOccurrenceId
checkedControlledObservationOccurrence observation = observation.occurrence

checkedControlledObservationLabel :: CheckedControlledObservation -> Maybe Label
checkedControlledObservationLabel observation = observation.label

checkedControlledObservationStructuralRole :: CheckedControlledObservation -> Maybe StructuralCarrierRole
checkedControlledObservationStructuralRole observation = observation.structuralRole

checkedControlledObservationPublication :: CheckedControlledObservation -> CheckedPublication
checkedControlledObservationPublication observation = observation.publication

-- | One application-visible possession source derived from an exact checked
-- Store match.  Its strength is the strength observed by that read; a later
-- Store upgrade does not retroactively strengthen an earlier observation.
data ControlledStorePossessionWitness = ControlledStorePossessionWitness
  { process :: ProcessEpochId,
    delta :: DeltaId,
    object :: GlobalObjectId,
    strength :: ReplicaStrength
  }
  deriving stock (Eq, Show)

controlledStorePossessionWitnessProcess :: ControlledStorePossessionWitness -> ProcessEpochId
controlledStorePossessionWitnessProcess witness = witness.process

controlledStorePossessionWitnessDelta :: ControlledStorePossessionWitness -> DeltaId
controlledStorePossessionWitnessDelta witness = witness.delta

controlledStorePossessionWitnessObject :: ControlledStorePossessionWitness -> GlobalObjectId
controlledStorePossessionWitnessObject witness = witness.object

controlledStorePossessionWitnessStrength :: ControlledStorePossessionWitness -> ReplicaStrength
controlledStorePossessionWitnessStrength witness = witness.strength

controlledStorePossessionWitnesses :: State -> [ControlledStorePossessionWitness]
controlledStorePossessionWitnesses state =
  [ ControlledStorePossessionWitness process delta object strength
  | ((process, delta, object), strength) <- Map.toAscList state.storePossessions
  ]

controlledStorePossessionStrength ::
  ProcessEpochId ->
  DeltaId ->
  GlobalObjectId ->
  State ->
  Maybe ReplicaStrength
controlledStorePossessionStrength process delta object state =
  Map.lookup (process, delta, object) state.storePossessions

data ControlledStoreObservationError
  = ControlledStoreReaderUnavailable DeltaId
  | ControlledStoreReaderProcessMismatch DeltaId ProcessEpochId ProcessEpochId
  | ControlledStoreSortMismatch DeltaId SortId SortId
  | ControlledStoreOccurrenceMismatch DeltaId SortDefinitionOccurrenceId SortDefinitionOccurrenceId
  | ControlledStoreRecordUnavailable GlobalObjectId
  | ControlledStoreRecordSortMismatch GlobalObjectId SortId SortId
  | ControlledStoreRecordOccurrenceMismatch GlobalObjectId SortDefinitionOccurrenceId SortDefinitionOccurrenceId
  | ControlledStoreRecordStructuralRoleMismatch GlobalObjectId (Maybe StructuralCarrierRole) (Maybe StructuralCarrierRole)
  | ControlledStoreRecordLabelMismatch GlobalObjectId (Maybe Label) (Maybe Label)
  | ControlledStorePublicationUnavailable PublicationId
  | ControlledStorePublicationConflict PublicationId
  deriving stock (Eq, Show)

newtype PreparedControlledStoreObservation
  = PreparedControlledStoreObservation (Prepared State ())

-- | Retain the exact strength by which one checked controlled Store match was
-- made visible to its owning process.
prepareControlledStoreRead ::
  ProcessEpochId ->
  DeltaId ->
  ReplicaStrength ->
  CheckedControlledObservation ->
  State ->
  Either ControlledStoreObservationError PreparedControlledStoreObservation
prepareControlledStoreRead =
  prepareControlledStoreObservation RetainControlledStorePossession

-- | Localize the returned match first and then remove only its exact
-- @(process, delta, object)@ source.  Publisher/bootstrap evidence and sources
-- from other deltas are intentionally unaffected.
prepareControlledStoreLocalTake ::
  ProcessEpochId ->
  DeltaId ->
  ReplicaStrength ->
  CheckedControlledObservation ->
  State ->
  Either ControlledStoreObservationError PreparedControlledStoreObservation
prepareControlledStoreLocalTake =
  prepareControlledStoreObservation RemoveControlledStorePossession

commitControlledStoreObservation :: PreparedControlledStoreObservation -> State
commitControlledStoreObservation (PreparedControlledStoreObservation prepared) =
  fst (commitPrepared prepared)

data ControlledStorePossessionDisposition
  = RetainControlledStorePossession
  | RemoveControlledStorePossession

prepareControlledStoreObservation ::
  ControlledStorePossessionDisposition ->
  ProcessEpochId ->
  DeltaId ->
  ReplicaStrength ->
  CheckedControlledObservation ->
  State ->
  Either ControlledStoreObservationError PreparedControlledStoreObservation
prepareControlledStoreObservation disposition process delta strength observation state =
  PreparedControlledStoreObservation
    <$> prepareTransition prepare state
  where
    object = checkedControlledObservationObject observation
    checked = checkedControlledObservationPublication observation
    observedSort = checkedPublicationSort checked
    observedOccurrence = checkedControlledObservationOccurrence observation

    prepare predecessor = do
      reader <-
        maybe
          (Left (ControlledStoreReaderUnavailable delta))
          Right
          (Map.lookup delta predecessor.readers)
      requireStoreObservation
        (rootFactProcessEpoch reader == process)
        ( ControlledStoreReaderProcessMismatch
            delta
            process
            (rootFactProcessEpoch reader)
        )
      requireStoreObservation
        (rootFactSortId reader == observedSort)
        (ControlledStoreSortMismatch delta (rootFactSortId reader) observedSort)
      requireStoreObservation
        (rootFactOccurrenceId reader == observedOccurrence)
        ( ControlledStoreOccurrenceMismatch
            delta
            (rootFactOccurrenceId reader)
            observedOccurrence
        )
      record <-
        maybe
          (Left (ControlledStoreRecordUnavailable object))
          Right
          (Map.lookup object predecessor.localRecords)
      requireStoreObservation
        (record.sortId == observedSort)
        (ControlledStoreRecordSortMismatch object record.sortId observedSort)
      requireStoreObservation
        (record.occurrenceId == observedOccurrence)
        ( ControlledStoreRecordOccurrenceMismatch
            object
            record.occurrenceId
            observedOccurrence
        )
      requireStoreObservation
        (record.structuralRole == checkedControlledObservationStructuralRole observation)
        ( ControlledStoreRecordStructuralRoleMismatch
            object
            record.structuralRole
            (checkedControlledObservationStructuralRole observation)
        )
      requireStoreObservation
        ( controlledRecordObservationLabelCompatible
            record
            observation
            predecessor
        )
        ( ControlledStoreRecordLabelMismatch
            object
            record.observedLabel
            (checkedControlledObservationLabel observation)
        )
      case Map.lookup (checkedPublicationId checked) record.observations of
        Nothing -> Left (ControlledStorePublicationUnavailable (checkedPublicationId checked))
        Just retained ->
          requireStoreObservation
            (retained == checked)
            (ControlledStorePublicationConflict (checkedPublicationId checked))
      let source = (process, delta, object)
          suppressedCurrent =
            checkedPublicationLifecycle checked == Current
              && controlledRecordLifecycle record == ControlledObsolete
          sources = case disposition of
            RetainControlledStorePossession
              | not suppressedCurrent
                  && Set.notMember process predecessor.endedProcesses ->
                  Map.insert source strength predecessor.storePossessions
              | otherwise -> predecessor.storePossessions
            RemoveControlledStorePossession ->
              Map.delete source predecessor.storePossessions
      let successor = predecessor {storePossessions = sources}
          notified
            | Map.lookup source predecessor.storePossessions == Map.lookup source sources = successor
            | otherwise = notifyPreparationPossessions (Set.singleton (process, object)) successor
      Right (notified, ())

data ControlledFirstUseResult
  = ControlledFirstUseWon
  | ControlledFirstUseRetry
  deriving stock (Eq, Show)

data ControlledFirstUseError
  = ControlledFirstUseReservationUnavailable GlobalUniqueId
  | ControlledFirstUseAuthorityContradiction AuthorityEpoch AuthorityEpoch
  | ControlledFirstUseWriterUnavailable NablaId
  | ControlledFirstUseWriterNotPossessed ProcessEpochId NablaId
  | ControlledFirstUsePublicationNablaMismatch NablaId NablaId
  | ControlledFirstUsePublicationAuthorityMismatch AuthorityEpoch AuthorityEpoch
  | ControlledFirstUseSortMismatch SortId SortId
  | ControlledFirstUseOccurrenceMismatch SortDefinitionOccurrenceId SortDefinitionOccurrenceId
  | ControlledFirstUseInitialLabelUnavailable Label
  | ControlledFirstUseMustBeCurrent
  | ControlledFirstUseObjectConflict GlobalObjectId
  | ControlledFirstUseConflict PublicationId PublicationId
  deriving stock (Eq, Show)

newtype PreparedControlledFirstUse
  = PreparedControlledFirstUse (Prepared State ControlledFirstUseResult)

-- | Win or exactly replay the first use of a local controlled reservation.
-- The caller has already globalized and descriptor-checked the value; this
-- owner checks the reservation, immutable writer-derived binding, label, and
-- publication tenure before installing any fact.
prepareControlledFirstUse ::
  ProcessEpochId ->
  NablaId ->
  AuthorityEpoch ->
  GlobalUniqueId ->
  CheckedControlledObservation ->
  State ->
  Either ControlledFirstUseError PreparedControlledFirstUse
prepareControlledFirstUse
  process
  nabla
  authority
  generated
  observation
  state = do
    writer <-
      maybe
        (Left (ControlledFirstUseWriterUnavailable nabla))
        Right
        (Map.lookup nabla state.writers)
    requireFirstUse
      (rootFactAuthority writer == authority)
      ( ControlledFirstUseAuthorityContradiction
          (rootFactAuthority writer)
          authority
      )
    prepareControlledFirstUseWithBinding
      ( controlledWriterBinding
          process
          nabla
          (rootFactSortId writer)
          (rootFactOccurrenceId writer)
          (rootFactAuthority writer)
          (rootFactControlPrerequisite writer)
      )
      generated
      observation
      state

-- | Generic checked-writer first use.  The binding is produced by the shared
-- bootstrap/dynamic operate resolver; validation still rechecks the retained
-- reservation, publication identity, exact sort occurrence, and possession in
-- this owner before preparing any mutation.
prepareControlledFirstUseWithBinding ::
  ControlledWriterBinding ->
  GlobalUniqueId ->
  CheckedControlledObservation ->
  State ->
  Either ControlledFirstUseError PreparedControlledFirstUse
prepareControlledFirstUseWithBinding binding generated observation state =
  PreparedControlledFirstUse
    <$> prepareTransition prepare state
  where
    process = controlledWriterBindingProcess binding
    nabla = controlledWriterBindingWriter binding
    authority = controlledWriterBindingAuthority binding
    writerSort = controlledWriterBindingSortId binding
    expectedOccurrence = controlledWriterBindingOccurrenceId binding
    object = globalObjectIdFromGlobalUniqueId generated
    observedObject = checkedControlledObservationObject observation
    occurrence = checkedControlledObservationOccurrence observation
    structuralRole = checkedControlledObservationStructuralRole observation
    initialLabel = checkedControlledObservationLabel observation
    checked = checkedControlledObservationPublication observation
    identifier = checkedPublicationId checked

    prepare predecessor = do
      reservation <-
        maybe
          (Left (ControlledFirstUseReservationUnavailable generated))
          Right
          (Map.lookup generated predecessor.reservations)
      requireFirstUse
        (reservation.process == process && reservation.nabla == nabla)
        (ControlledFirstUseReservationUnavailable generated)
      requireFirstUse
        (reservation.authority == authority)
        ( ControlledFirstUseAuthorityContradiction
            reservation.authority
            authority
        )
      requireFirstUse
        ( Set.member
            (process, globalObjectIdFromNablaId nabla)
            predecessor.possessions
        )
        (ControlledFirstUseWriterNotPossessed process nabla)
      validateFirstUseBinding reservation
      requireFirstUse
        (observedObject == object)
        (ControlledFirstUseObjectConflict observedObject)
      case reservation.use of
        ReservationOpen -> firstUseWinner predecessor reservation
        ReservationPublished firstPublication ->
          exactFirstUseRetry predecessor reservation firstPublication
        ReservationConsumed ->
          Left (ControlledFirstUseReservationUnavailable generated)

    validateFirstUseBinding reservation = do
      let identifierNabla = publicationNabla identifier
          identifierAuthority = publicationAuthorityEpoch identifier
          reservedSort = reservation.sortId
          actualSort = checkedPublicationSort checked
      requireFirstUse
        (identifierNabla == nabla)
        (ControlledFirstUsePublicationNablaMismatch nabla identifierNabla)
      requireFirstUse
        (identifierAuthority == authority)
        ( ControlledFirstUsePublicationAuthorityMismatch
            authority
            identifierAuthority
        )
      requireFirstUse
        (writerSort == reservedSort)
        (ControlledFirstUseSortMismatch reservedSort writerSort)
      requireFirstUse
        (actualSort == reservedSort)
        (ControlledFirstUseSortMismatch reservedSort actualSort)
      requireFirstUse
        (occurrence == expectedOccurrence)
        ( ControlledFirstUseOccurrenceMismatch
            expectedOccurrence
            occurrence
        )
      requireFirstUse
        ( initialLabel == Nothing
            || initialLabel == Just (VoidLabel, 0)
            || initialLabel == Just (ProcessLabel process, 0)
        )
        ( ControlledFirstUseInitialLabelUnavailable
            (maybe (VoidLabel, 0) id initialLabel)
        )
      requireFirstUse
        (checkedPublicationLifecycle checked == Current)
        ControlledFirstUseMustBeCurrent

    firstUseWinner predecessor reservation = do
      requireFirstUse
        ( Map.notMember object predecessor.roles
            && Map.notMember object predecessor.localRecords
            && not (controlledObjectTerminallyDeleted object predecessor)
            && not (controlledHasAnyNormalPossession object predecessor)
        )
        (ControlledFirstUseObjectConflict object)
      let record =
            initialControlledRecord
              object
              occurrence
              structuralRole
              initialLabel
              (Set.singleton process)
              checked
          publishedReservation :: Reservation
          publishedReservation =
            Reservation
              { process = reservation.process,
                nabla = reservation.nabla,
                sortId = reservation.sortId,
                authority = reservation.authority,
                use = ReservationPublished identifier
              }
          successor =
            installControlledRecord
              record
              predecessor
                { reservations = Map.insert generated publishedReservation predecessor.reservations
                }

      Right (successor, ControlledFirstUseWon)

    exactFirstUseRetry predecessor reservation firstPublication = do
      record <-
        maybe
          (Left (ControlledFirstUseObjectConflict object))
          Right
          (Map.lookup object predecessor.localRecords)
      requireFirstUse
        ( firstPublication == identifier
            && record.firstPublication == checked
            && record.sortId == reservation.sortId
            && record.occurrenceId == occurrence
            && record.structuralRole == structuralRole
            && record.observedLabel == initialLabel
            && Set.member process record.possessionSources
        )
        ( ControlledFirstUseConflict
            (checkedPublicationId record.firstPublication)
            identifier
        )
      Right (predecessor, ControlledFirstUseRetry)

preparedControlledFirstUseResult :: PreparedControlledFirstUse -> ControlledFirstUseResult
preparedControlledFirstUseResult (PreparedControlledFirstUse prepared) =
  preparedOutput prepared

commitControlledFirstUse :: PreparedControlledFirstUse -> (State, ControlledFirstUseResult)
commitControlledFirstUse (PreparedControlledFirstUse prepared) =
  commitPrepared prepared

data ControlledUpdateResult
  = ControlledUpdateWinnerAdvanced
  | ControlledUpdateWinnerRetained
  | ControlledUpdateRetry
  deriving stock (Eq, Show)

data ControlledUpdateError
  = ControlledUpdateObjectUnavailable GlobalObjectId
  | ControlledUpdateNotCurrent GlobalObjectId ControlledLifecycle
  | ControlledUpdateWriterUnavailable NablaId
  | ControlledUpdateWriterNotPossessed ProcessEpochId NablaId
  | ControlledUpdateObjectNotPossessed ProcessEpochId GlobalObjectId
  | ControlledUpdatePublicationNablaMismatch NablaId NablaId
  | ControlledUpdatePublicationAuthorityMismatch AuthorityEpoch AuthorityEpoch
  | ControlledUpdateSortMismatch SortId SortId
  | ControlledUpdateOccurrenceMismatch SortDefinitionOccurrenceId SortDefinitionOccurrenceId
  | ControlledUpdateStructuralRoleMismatch (Maybe StructuralCarrierRole) (Maybe StructuralCarrierRole)
  | ControlledUpdateLatestLabelUnavailable (Maybe Label) ProcessEpochId
  | ControlledUpdateLabelMismatch (Maybe Label) (Maybe Label)
  | ControlledUpdatePublicationConflict PublicationId
  deriving stock (Eq, Show)

newtype PreparedControlledUpdate
  = PreparedControlledUpdate (Prepared State ControlledUpdateResult)

-- | Prepare one application-origin update.  Only a locally current object and
-- a current same-sort writer may enter this path.  The application-supplied
-- label is intentionally absent: the retained latest observed label is used.
prepareControlledUpdate ::
  ProcessEpochId ->
  NablaId ->
  AuthorityEpoch ->
  CheckedControlledObservation ->
  State ->
  Either ControlledUpdateError PreparedControlledUpdate
prepareControlledUpdate
  process
  nabla
  authority
  observation
  state = do
    writer <-
      maybe
        (Left (ControlledUpdateWriterUnavailable nabla))
        Right
        (Map.lookup nabla state.writers)
    requireUpdate
      (rootFactAuthority writer == authority)
      (ControlledUpdatePublicationAuthorityMismatch (rootFactAuthority writer) authority)
    prepareControlledUpdateWithBinding
      ( controlledWriterBinding
          process
          nabla
          (rootFactSortId writer)
          (rootFactOccurrenceId writer)
          (rootFactAuthority writer)
          (rootFactControlPrerequisite writer)
      )
      observation
      state

-- | Generic checked-writer update counterpart to
-- 'prepareControlledFirstUseWithBinding'.
prepareControlledUpdateWithBinding ::
  ControlledWriterBinding ->
  CheckedControlledObservation ->
  State ->
  Either ControlledUpdateError PreparedControlledUpdate
prepareControlledUpdateWithBinding binding observation state =
  PreparedControlledUpdate
    <$> prepareTransition prepare state
  where
    process = controlledWriterBindingProcess binding
    nabla = controlledWriterBindingWriter binding
    authority = controlledWriterBindingAuthority binding
    writerSort = controlledWriterBindingSortId binding
    writerOccurrence = controlledWriterBindingOccurrenceId binding
    object = checkedControlledObservationObject observation
    occurrence = checkedControlledObservationOccurrence observation
    structuralRole = checkedControlledObservationStructuralRole observation
    publicationLabel = checkedControlledObservationLabel observation
    checked = checkedControlledObservationPublication observation
    identifier = checkedPublicationId checked

    prepare predecessor = do
      record <-
        maybe
          (Left (ControlledUpdateObjectUnavailable object))
          Right
          (Map.lookup object predecessor.localRecords)
      requireUpdate
        ( Set.member
            (process, globalObjectIdFromNablaId nabla)
            predecessor.possessions
        )
        (ControlledUpdateWriterNotPossessed process nabla)
      let identifierNabla = publicationNabla identifier
          identifierAuthority = publicationAuthorityEpoch identifier
          publicationSort = checkedPublicationSort checked
      requireUpdate
        (identifierNabla == nabla)
        (ControlledUpdatePublicationNablaMismatch nabla identifierNabla)
      requireUpdate
        (identifierAuthority == authority)
        (ControlledUpdatePublicationAuthorityMismatch authority identifierAuthority)
      requireUpdate
        (publicationSort == record.sortId && writerSort == record.sortId)
        (ControlledUpdateSortMismatch record.sortId publicationSort)
      requireUpdate
        (occurrence == record.occurrenceId && writerOccurrence == record.occurrenceId)
        (ControlledUpdateOccurrenceMismatch record.occurrenceId occurrence)
      requireUpdate
        (structuralRole == record.structuralRole)
        (ControlledUpdateStructuralRoleMismatch record.structuralRole structuralRole)
      requireUpdate
        (controlledHasNormalPossession process object predecessor)
        (ControlledUpdateObjectNotPossessed process object)
      effectiveLabel <-
        case record.observedLabel of
          Nothing -> Right Nothing
          Just _ ->
            case controlledEffectiveLabelState object predecessor of
              Just released -> case releasedLabelStateView released of
                ReleasedLabelView label -> Right (Just label)
                ReleasedDeletedView {} ->
                  Left (ControlledUpdateObjectNotPossessed process object)
              Nothing ->
                Left
                  ( ControlledUpdateLatestLabelUnavailable
                      record.observedLabel
                      process
                  )
      requireUpdate
        ( effectiveLabel == Nothing
            || (fst <$> effectiveLabel) == Just VoidLabel
            || (fst <$> effectiveLabel) == Just (ProcessLabel process)
        )
        (ControlledUpdateLatestLabelUnavailable effectiveLabel process)
      requireUpdate
        (publicationLabel == effectiveLabel)
        (ControlledUpdateLabelMismatch effectiveLabel publicationLabel)
      case Map.lookup identifier record.observations of
        Just retained
          | retained == checked -> Right (predecessor, ControlledUpdateRetry)
          | otherwise -> Left (ControlledUpdatePublicationConflict identifier)
        Nothing -> do
          requireUpdate
            (controlledRecordLifecycle record == ControlledCurrent)
            (ControlledUpdateNotCurrent object (controlledRecordLifecycle record))
          let (updated, peerResult) =
                applyControlledObservation (Just process) checked record
              successor = installControlledRecord updated predecessor
              result = case peerResult of
                ControlledPeerWinnerAdvanced -> ControlledUpdateWinnerAdvanced
                ControlledPeerWinnerRetained -> ControlledUpdateWinnerRetained
                _ -> ControlledUpdateWinnerRetained
          Right (successor, result)

preparedControlledUpdateResult :: PreparedControlledUpdate -> ControlledUpdateResult
preparedControlledUpdateResult (PreparedControlledUpdate prepared) =
  preparedOutput prepared

commitControlledUpdate :: PreparedControlledUpdate -> (State, ControlledUpdateResult)
commitControlledUpdate (PreparedControlledUpdate prepared) =
  commitPrepared prepared

data ControlledPeerObservationResult
  = ControlledPeerEstablished
  | ControlledPeerWinnerAdvanced
  | ControlledPeerWinnerRetained
  | ControlledPeerReplay
  | ControlledPeerCurrentSuppressed
  deriving stock (Eq, Show)

data ControlledPeerObservationError
  = ControlledPeerObjectRoleConflict GlobalObjectId
  | ControlledPeerReservationConflict GlobalObjectId ReservationPhase
  | ControlledPeerSortMismatch GlobalObjectId SortId SortId
  | ControlledPeerOccurrenceMismatch GlobalObjectId SortDefinitionOccurrenceId SortDefinitionOccurrenceId
  | ControlledPeerStructuralRoleMismatch GlobalObjectId (Maybe StructuralCarrierRole) (Maybe StructuralCarrierRole)
  | ControlledPeerLabelMismatch GlobalObjectId (Maybe Label) (Maybe Label)
  | ControlledPeerPublicationConflict PublicationId
  deriving stock (Eq, Show)

newtype PreparedControlledPeerObservation
  = PreparedControlledPeerObservation
      (Prepared State ControlledPeerObservationResult)

-- | Admit one already authenticated peer observation into the monotone local
-- projection.  This seam deliberately contains no authority or protocol
-- decoding: its caller must have reconstructed the checked publication and
-- source facts first.
prepareControlledPeerObservation ::
  CheckedControlledObservation ->
  State ->
  Either ControlledPeerObservationError PreparedControlledPeerObservation
prepareControlledPeerObservation = prepareControlledObservation False

-- | Merge source-attested startup evidence into a receiver that may have first
-- observed the object under a later label. A released local overlay establishes
-- the current lineage; the transferred raw label remains historical evidence.
-- All identity, occurrence, role, publication and monotone winner checks remain
-- shared with ordinary peer observation, and no source possession is installed.
prepareControlledStartupObservation ::
  CheckedControlledObservation ->
  State ->
  Either ControlledPeerObservationError PreparedControlledPeerObservation
prepareControlledStartupObservation = prepareControlledObservation True

prepareControlledObservation ::
  Bool ->
  CheckedControlledObservation ->
  State ->
  Either ControlledPeerObservationError PreparedControlledPeerObservation
prepareControlledObservation retainedStartup observation state =
  PreparedControlledPeerObservation
    <$> prepareTransition prepare state
  where
    object = checkedControlledObservationObject observation
    occurrence = checkedControlledObservationOccurrence observation
    structuralRole = checkedControlledObservationStructuralRole observation
    observedLabel = checkedControlledObservationLabel observation
    checked = checkedControlledObservationPublication observation
    identifier = checkedPublicationId checked

    prepare predecessor
      | controlledObjectTerminallyDeleted object predecessor =
          Right (predecessor, ControlledPeerCurrentSuppressed)
      | otherwise = case Map.lookup object predecessor.localRecords of
          Nothing -> do
            requirePeer
              (Map.notMember object predecessor.roles)
              (ControlledPeerObjectRoleConflict object)
            case Map.lookup (globalUniqueIdFromGlobalObjectId object) predecessor.reservations of
              Nothing -> Right ()
              Just reservation ->
                Left
                  ( ControlledPeerReservationConflict
                      object
                      (reservationPhase reservation.use)
                  )
            let record =
                  initialControlledRecord
                    object
                    occurrence
                    structuralRole
                    observedLabel
                    Set.empty
                    checked
            Right
              ( installControlledRecord record predecessor,
                ControlledPeerEstablished
              )
          Just record -> do
            let publicationSort = checkedPublicationSort checked
            requirePeer
              (publicationSort == record.sortId)
              (ControlledPeerSortMismatch object record.sortId publicationSort)
            requirePeer
              (occurrence == record.occurrenceId)
              (ControlledPeerOccurrenceMismatch object record.occurrenceId occurrence)
            requirePeer
              (structuralRole == record.structuralRole)
              (ControlledPeerStructuralRoleMismatch object record.structuralRole structuralRole)
            requirePeer
              ( controlledRecordObservationLabelCompatible
                  record
                  observation
                  predecessor
                  || (retainedStartup && Map.member object predecessor.releasedLabels)
              )
              (ControlledPeerLabelMismatch object record.observedLabel observedLabel)
            case Map.lookup identifier record.observations of
              Just retained
                | retained == checked -> Right (predecessor, ControlledPeerReplay)
                | otherwise -> Left (ControlledPeerPublicationConflict identifier)
              Nothing ->
                let (updated, result) =
                      applyControlledObservation Nothing checked record
                 in Right (installControlledRecord updated predecessor, result)

preparedControlledPeerObservationResult ::
  PreparedControlledPeerObservation ->
  ControlledPeerObservationResult
preparedControlledPeerObservationResult (PreparedControlledPeerObservation prepared) =
  preparedOutput prepared

commitControlledPeerObservation ::
  PreparedControlledPeerObservation ->
  (State, ControlledPeerObservationResult)
commitControlledPeerObservation (PreparedControlledPeerObservation prepared) =
  commitPrepared prepared

-- | One checked member of the closed private-environment root range.
--
-- The source binding is the caller's already resolved canonical application
-- writer. Its authority belongs to the publication source, not to the new
-- target: a target Nabla receives structural authority only after its
-- occurrence is covered by an installed topology cut.
data ControlledEnvironmentRoot = ControlledEnvironmentRoot
  { environmentProcess :: ProcessEpochId,
    environmentSlot :: EnvironmentRootSlot,
    environmentGeneratedId :: GlobalUniqueId,
    environmentTargetObject :: GlobalObjectId,
    environmentSourceBinding :: ControlledWriterBinding,
    environmentObservation :: CheckedControlledObservation
  }
  deriving stock (Eq, Show)

data ControlledEnvironmentRootError
  = ControlledEnvironmentRootBindingProcessMismatch
      ProcessEpochId
      ProcessEpochId
  | ControlledEnvironmentRootTargetMismatch GlobalObjectId GlobalObjectId
  | ControlledEnvironmentRootSourceEqualsTarget GlobalObjectId
  | ControlledEnvironmentRootPublicationSourceMismatch NablaId NablaId
  | ControlledEnvironmentRootPublicationAuthorityMismatch
      AuthorityEpoch
      AuthorityEpoch
  | ControlledEnvironmentRootSourceSortMismatch SortId SortId
  | ControlledEnvironmentRootCarrierSortMismatch SortId SortId
  | ControlledEnvironmentRootOccurrenceMismatch
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | ControlledEnvironmentRootCarrierRoleMismatch
      StructuralCarrierRole
      (Maybe StructuralCarrierRole)
  | ControlledEnvironmentRootLabelMismatch ProcessEpochId (Maybe Label)
  | ControlledEnvironmentRootMustBeCurrent PublicationId
  | ControlledEnvironmentRootCarriedSortShapeMismatch EnvironmentRootSlot
  | ControlledEnvironmentRootCarriedSortMismatch
      EnvironmentRootSlot
      SortId
      SortId
  | ControlledEnvironmentRootSequencingShapeMismatch EnvironmentRootSlot
  deriving stock (Eq, Show)

-- | Check one generated target against the exact carrier publication prepared
-- for its canonical manifest slot. In addition to the general controlled
-- observation, this pins the carried data sort and the environment-only rule
-- that every new writer starts unsequenced.
checkControlledEnvironmentRoot ::
  ProcessEpochId ->
  EnvironmentRootSlot ->
  GlobalUniqueId ->
  ControlledWriterBinding ->
  CheckedControlledObservation ->
  Either ControlledEnvironmentRootError ControlledEnvironmentRoot
checkControlledEnvironmentRoot process slot generated source observation = do
  requireEnvironmentRoot
    (controlledWriterBindingProcess source == process)
    ( ControlledEnvironmentRootBindingProcessMismatch
        process
        (controlledWriterBindingProcess source)
    )
  requireEnvironmentRoot
    (checkedControlledObservationObject observation == target)
    ( ControlledEnvironmentRootTargetMismatch
        target
        (checkedControlledObservationObject observation)
    )
  requireEnvironmentRoot
    (globalObjectIdFromNablaId (controlledWriterBindingWriter source) /= target)
    (ControlledEnvironmentRootSourceEqualsTarget target)
  requireEnvironmentRoot
    (publicationNabla identifier == controlledWriterBindingWriter source)
    ( ControlledEnvironmentRootPublicationSourceMismatch
        (controlledWriterBindingWriter source)
        (publicationNabla identifier)
    )
  requireEnvironmentRoot
    (publicationAuthorityEpoch identifier == controlledWriterBindingAuthority source)
    ( ControlledEnvironmentRootPublicationAuthorityMismatch
        (controlledWriterBindingAuthority source)
        (publicationAuthorityEpoch identifier)
    )
  requireEnvironmentRoot
    (controlledWriterBindingSortId source == carrierSort)
    ( ControlledEnvironmentRootSourceSortMismatch
        carrierSort
        (controlledWriterBindingSortId source)
    )
  requireEnvironmentRoot
    (checkedPublicationSort checked == carrierSort)
    ( ControlledEnvironmentRootCarrierSortMismatch
        carrierSort
        (checkedPublicationSort checked)
    )
  requireEnvironmentRoot
    ( checkedControlledObservationOccurrence observation
        == controlledWriterBindingOccurrenceId source
    )
    ( ControlledEnvironmentRootOccurrenceMismatch
        (controlledWriterBindingOccurrenceId source)
        (checkedControlledObservationOccurrence observation)
    )
  requireEnvironmentRoot
    (checkedControlledObservationStructuralRole observation == Just carrierRole)
    ( ControlledEnvironmentRootCarrierRoleMismatch
        carrierRole
        (checkedControlledObservationStructuralRole observation)
    )
  requireEnvironmentRoot
    (checkedControlledObservationLabel observation == Just (ProcessLabel process, 0))
    ( ControlledEnvironmentRootLabelMismatch
        process
        (checkedControlledObservationLabel observation)
    )
  requireEnvironmentRoot
    (checkedPublicationLifecycle checked == Current)
    (ControlledEnvironmentRootMustBeCurrent identifier)
  case dataRole of
    Nothing -> Right ()
    Just role -> do
      carriedSort <- environmentCarriedSort slot checked
      let dataSort = profileSortFor role
      requireEnvironmentRoot
        (carriedSort == dataSort)
        (ControlledEnvironmentRootCarriedSortMismatch slot dataSort carriedSort)
  case environmentRootClaimView (environmentRootSlotClaim slot) of
    EnvironmentWriterRootClaimView _ UnsequencedNabla ->
      requireEnvironmentRoot
        (environmentWriterIsUnsequenced checked)
        (ControlledEnvironmentRootSequencingShapeMismatch slot)
    EnvironmentWriterRootClaimView _ NablaSequencedBy {} ->
      Left (ControlledEnvironmentRootSequencingShapeMismatch slot)
    EnvironmentReaderRootClaimView {} -> Right ()
    EnvironmentHubRootClaimView -> Right ()
    EnvironmentEdgeRootClaimView {} -> Right ()
  Right
    ControlledEnvironmentRoot
      { environmentProcess = process,
        environmentSlot = slot,
        environmentGeneratedId = generated,
        environmentTargetObject = target,
        environmentSourceBinding = source,
        environmentObservation = observation
      }
  where
    checked = checkedControlledObservationPublication observation
    identifier = checkedPublicationId checked
    target = globalObjectIdFromGlobalUniqueId generated
    carrierRole = environmentRootSlotStructuralCarrierRole slot
    (carrierCatalogueRole, dataRole) =
      case environmentRootClaimView (environmentRootSlotClaim slot) of
        EnvironmentWriterRootClaimView role _ -> (NablaRole, Just role)
        EnvironmentReaderRootClaimView role -> (DeltaRole, Just role)
        EnvironmentHubRootClaimView -> (NeutralVertexRole, Nothing)
        EnvironmentEdgeRootClaimView {} -> (EdgeRole, Nothing)
    carrierSort = profileSortFor carrierCatalogueRole

controlledEnvironmentRootProcess :: ControlledEnvironmentRoot -> ProcessEpochId
controlledEnvironmentRootProcess root = root.environmentProcess

controlledEnvironmentRootSlot :: ControlledEnvironmentRoot -> EnvironmentRootSlot
controlledEnvironmentRootSlot root = root.environmentSlot

controlledEnvironmentRootGeneratedId :: ControlledEnvironmentRoot -> GlobalUniqueId
controlledEnvironmentRootGeneratedId root = root.environmentGeneratedId

controlledEnvironmentRootTargetObject :: ControlledEnvironmentRoot -> GlobalObjectId
controlledEnvironmentRootTargetObject root = root.environmentTargetObject

controlledEnvironmentRootSourceBinding ::
  ControlledEnvironmentRoot -> ControlledWriterBinding
controlledEnvironmentRootSourceBinding root = root.environmentSourceBinding

controlledEnvironmentRootObservation ::
  ControlledEnvironmentRoot -> CheckedControlledObservation
controlledEnvironmentRootObservation root = root.environmentObservation

environmentCarriedSort ::
  EnvironmentRootSlot ->
  CheckedPublication ->
  Either ControlledEnvironmentRootError SortId
environmentCarriedSort slot checked =
  case valueAt
    (directProjection environmentCarriedSortField)
    (checkedPublicationValue checked) of
    Right value -> case viewValue value of
      BytesValue bytes ->
        case mkSortId bytes of
          Right carried -> Right carried
          Left _ -> shapeMismatch
      _ -> shapeMismatch
    Left _ -> shapeMismatch
  where
    shapeMismatch = Left (ControlledEnvironmentRootCarriedSortShapeMismatch slot)

environmentWriterIsUnsequenced :: CheckedPublication -> Bool
environmentWriterIsUnsequenced checked =
  case valueAt
    (directProjection environmentSequencingObjectField)
    (checkedPublicationValue checked) of
    Right value -> viewValue value == OptionalGlobalUniqueIdValue Nothing
    Left _ -> False

environmentCarriedSortField, environmentSequencingObjectField :: FieldName
environmentCarriedSortField = checkedEnvironmentFieldName "sort_id"
environmentSequencingObjectField = checkedEnvironmentFieldName "sequencing_object"

checkedEnvironmentFieldName :: Text -> FieldName
checkedEnvironmentFieldName name =
  case mkFieldName name of
    Right field -> field
    Left _ -> error "invalid closed environment-root field name"

requireEnvironmentRoot ::
  Bool ->
  ControlledEnvironmentRootError ->
  Either ControlledEnvironmentRootError ()
requireEnvironmentRoot condition problem
  | condition = Right ()
  | otherwise = Left problem

data ControlledEnvironmentRootsClassification
  = ControlledEnvironmentRootsEstablished
  | ControlledEnvironmentRootsExactReplay
  deriving stock (Eq, Ord, Show)

data ControlledEnvironmentRootsError
  = ControlledEnvironmentRootsManifestMismatch [EnvironmentRootSlot]
  | ControlledEnvironmentRootsProcessMismatch ProcessEpochId ProcessEpochId
  | ControlledEnvironmentRootsDuplicateTarget GlobalObjectId
  | ControlledEnvironmentRootsProcessEnded ProcessEpochId
  | ControlledEnvironmentRootsSourceNotPossessed ProcessEpochId NablaId
  | ControlledEnvironmentRootsObjectConflict GlobalObjectId
  | ControlledEnvironmentRootsReplayConflict GlobalObjectId PublicationId
  | ControlledEnvironmentRootsPartialReplay
  deriving stock (Eq, Show)

newtype PreparedControlledEnvironmentRoots
  = PreparedControlledEnvironmentRoots
      (Prepared State ControlledEnvironmentRootsClassification)

data ControlledEnvironmentRootState
  = ControlledEnvironmentRootNew
  | ControlledEnvironmentRootExact
  deriving stock (Eq, Ord, Show)

-- | Atomically install all twelve checked roots or replay the exact retained
-- range. A mixed retained prefix is an invariant contradiction: preparation
-- never fills its missing suffix and no partially installed successor is
-- exposed. End forbids a fresh range but an already accepted range remains
-- replayable as immutable history after its effective possessions are revoked.
prepareControlledEnvironmentRoots ::
  ProcessEpochId ->
  NonEmpty ControlledEnvironmentRoot ->
  State ->
  Either ControlledEnvironmentRootsError PreparedControlledEnvironmentRoots
prepareControlledEnvironmentRoots =
  prepareControlledEnvironmentRange
    (NonEmpty.toList (environmentManifestShapeRoots profileEnvironmentManifestShape))

prepareControlledEnvironmentWiring ::
  ProcessEpochId ->
  NonEmpty ControlledEnvironmentRoot ->
  State ->
  Either ControlledEnvironmentRootsError PreparedControlledEnvironmentRoots
prepareControlledEnvironmentWiring = prepareControlledEnvironmentRange (NonEmpty.toList environmentWiringSlots)

prepareControlledEnvironmentRange ::
  [EnvironmentRootSlot] ->
  ProcessEpochId ->
  NonEmpty ControlledEnvironmentRoot ->
  State ->
  Either ControlledEnvironmentRootsError PreparedControlledEnvironmentRoots
prepareControlledEnvironmentRange expectedSlots process roots state = do
  requireEnvironmentRoots
    (fmap controlledEnvironmentRootSlot supplied == expectedSlots)
    ( ControlledEnvironmentRootsManifestMismatch
        (fmap controlledEnvironmentRootSlot supplied)
    )
  mapM_ requireProcess supplied
  case firstDuplicateTarget supplied of
    Just duplicate -> Left (ControlledEnvironmentRootsDuplicateTarget duplicate)
    Nothing -> Right ()
  classifications <- traverse (classifyEnvironmentRoot state) supplied
  prepared <-
    if all (== ControlledEnvironmentRootNew) classifications
      then prepareFirst
      else
        if all (== ControlledEnvironmentRootExact) classifications
          then Right (state, ControlledEnvironmentRootsExactReplay)
          else Left ControlledEnvironmentRootsPartialReplay
  PreparedControlledEnvironmentRoots
    <$> prepareTransition (const (Right prepared)) state
  where
    supplied = NonEmpty.toList roots
    requireProcess root =
      requireEnvironmentRoots
        (controlledEnvironmentRootProcess root == process)
        ( ControlledEnvironmentRootsProcessMismatch
            process
            (controlledEnvironmentRootProcess root)
        )
    prepareFirst = do
      requireEnvironmentRoots
        (not (controlledProcessEnded process state))
        (ControlledEnvironmentRootsProcessEnded process)
      mapM_ requireSourcePossession supplied
      mapM_ requireAvailableTarget supplied
      let successor = foldl (flip installEnvironmentRoot) state supplied
      Right (successor, ControlledEnvironmentRootsEstablished)
    requireSourcePossession root =
      let source =
            controlledWriterBindingWriter
              (controlledEnvironmentRootSourceBinding root)
       in requireEnvironmentRoots
            ( controlledHasNormalPossession
                process
                (globalObjectIdFromNablaId source)
                state
            )
            (ControlledEnvironmentRootsSourceNotPossessed process source)
    requireAvailableTarget root =
      let target = controlledEnvironmentRootTargetObject root
       in requireEnvironmentRoots
            ( Map.notMember target state.roles
                && not (controlledObjectTerminallyDeleted target state)
                && not (controlledHasAnyNormalPossession target state)
            )
            (ControlledEnvironmentRootsObjectConflict target)

preparedControlledEnvironmentRootsClassification ::
  PreparedControlledEnvironmentRoots ->
  ControlledEnvironmentRootsClassification
preparedControlledEnvironmentRootsClassification
  (PreparedControlledEnvironmentRoots prepared) = preparedOutput prepared

commitControlledEnvironmentRoots ::
  PreparedControlledEnvironmentRoots ->
  (State, ControlledEnvironmentRootsClassification)
commitControlledEnvironmentRoots (PreparedControlledEnvironmentRoots prepared) =
  commitPrepared prepared

classifyEnvironmentRoot ::
  State ->
  ControlledEnvironmentRoot ->
  Either ControlledEnvironmentRootsError ControlledEnvironmentRootState
classifyEnvironmentRoot state root =
  if controlledObjectTerminallyDeleted target state
    then Left (ControlledEnvironmentRootsObjectConflict target)
    else case Map.lookup target state.localRecords of
      Nothing -> Right ControlledEnvironmentRootNew
      Just record
        | environmentRecordMatches root record state ->
            Right ControlledEnvironmentRootExact
        | otherwise ->
            Left
              ( ControlledEnvironmentRootsReplayConflict
                  target
                  (checkedPublicationId checked)
              )
  where
    target = controlledEnvironmentRootTargetObject root
    checked =
      checkedControlledObservationPublication
        (controlledEnvironmentRootObservation root)

environmentRecordMatches ::
  ControlledEnvironmentRoot ->
  ControlledLocalRecord ->
  State ->
  Bool
environmentRecordMatches root record state =
  -- The retained manifest pins the immutable first-use evidence, not the
  -- record's mutable winner or its first obsolete-observation time. Both may
  -- advance normally after the environment was accepted.
  Map.notMember target state.roles
    && record.firstPublication == checked
    && record.sortId == checkedPublicationSort checked
    && record.occurrenceId == checkedControlledObservationOccurrence observation
    && record.structuralRole == checkedControlledObservationStructuralRole observation
    && record.observedLabel == checkedControlledObservationLabel observation
    && Set.member process record.possessionSources
    && Map.lookup (checkedPublicationId checked) record.observations == Just checked
    && environmentRootPossessionMatchesLifecycle process target state
  where
    process = controlledEnvironmentRootProcess root
    target = controlledEnvironmentRootTargetObject root
    observation = controlledEnvironmentRootObservation root
    checked = checkedControlledObservationPublication observation

installEnvironmentRoot :: ControlledEnvironmentRoot -> State -> State
installEnvironmentRoot root = installControlledRecord record
  where
    observation = controlledEnvironmentRootObservation root
    record =
      initialControlledRecord
        (controlledEnvironmentRootTargetObject root)
        (checkedControlledObservationOccurrence observation)
        (checkedControlledObservationStructuralRole observation)
        (checkedControlledObservationLabel observation)
        (Set.singleton (controlledEnvironmentRootProcess root))
        (checkedControlledObservationPublication observation)

firstDuplicateTarget :: [ControlledEnvironmentRoot] -> Maybe GlobalObjectId
firstDuplicateTarget = go Set.empty
  where
    go _ [] = Nothing
    go seen (root : remaining)
      | Set.member target seen = Just target
      | otherwise = go (Set.insert target seen) remaining
      where
        target = controlledEnvironmentRootTargetObject root

requireEnvironmentRoots ::
  Bool ->
  ControlledEnvironmentRootsError ->
  Either ControlledEnvironmentRootsError ()
requireEnvironmentRoots condition problem
  | condition = Right ()
  | otherwise = Left problem

data ControlledObsoleteObservationTimeResult
  = ControlledObsoleteObservationTimeRecorded
  | ControlledObsoleteObservationTimeRetained
  deriving stock (Eq, Show)

data ControlledObsoleteObservationTimeError
  = ControlledObsoleteObservationObjectUnavailable GlobalObjectId
  | ControlledObsoleteObservationSortMismatch GlobalObjectId SortId SortId
  | ControlledObsoleteObservationOccurrenceMismatch
      GlobalObjectId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | ControlledObsoleteObservationStructuralRoleMismatch
      GlobalObjectId
      (Maybe StructuralCarrierRole)
      (Maybe StructuralCarrierRole)
  | ControlledObsoleteObservationPublicationUnavailable PublicationId
  | ControlledObsoleteObservationPublicationConflict PublicationId
  | ControlledObsoleteObservationPublicationCurrent PublicationId
  | ControlledObsoleteObservationPublicationNotWinner PublicationId PublicationId
  deriving stock (Eq, Show)

newtype PreparedControlledObsoleteObservationTime
  = PreparedControlledObsoleteObservationTime
      (Prepared State ControlledObsoleteObservationTimeResult)

-- | Retain the first runtime-supplied monotonic instant at which an exact
-- obsolete publication is observed as the local winner.  This transition does
-- not interpret or add to the instant: the later Forward admission owner is
-- responsible for comparing it with the descriptor's minimum retention.
prepareControlledObsoleteObservationTime ::
  MonotonicInstant ->
  CheckedControlledObservation ->
  State ->
  Either
    ControlledObsoleteObservationTimeError
    PreparedControlledObsoleteObservationTime
prepareControlledObsoleteObservationTime observedAt observation state =
  PreparedControlledObsoleteObservationTime
    <$> prepareTransition prepare state
  where
    object = checkedControlledObservationObject observation
    occurrence = checkedControlledObservationOccurrence observation
    structuralRole = checkedControlledObservationStructuralRole observation
    checked = checkedControlledObservationPublication observation
    identifier = checkedPublicationId checked

    prepare predecessor = do
      record <-
        maybe
          (Left (ControlledObsoleteObservationObjectUnavailable object))
          Right
          (Map.lookup object predecessor.localRecords)
      requireObsoleteObservationTime
        (record.sortId == checkedPublicationSort checked)
        ( ControlledObsoleteObservationSortMismatch
            object
            record.sortId
            (checkedPublicationSort checked)
        )
      requireObsoleteObservationTime
        (record.occurrenceId == occurrence)
        ( ControlledObsoleteObservationOccurrenceMismatch
            object
            record.occurrenceId
            occurrence
        )
      requireObsoleteObservationTime
        (record.structuralRole == structuralRole)
        ( ControlledObsoleteObservationStructuralRoleMismatch
            object
            record.structuralRole
            structuralRole
        )
      retained <-
        maybe
          (Left (ControlledObsoleteObservationPublicationUnavailable identifier))
          Right
          (Map.lookup identifier record.observations)
      requireObsoleteObservationTime
        (retained == checked)
        (ControlledObsoleteObservationPublicationConflict identifier)
      requireObsoleteObservationTime
        (checkedPublicationLifecycle checked == Obsolete)
        (ControlledObsoleteObservationPublicationCurrent identifier)
      let winner = controlledRecordLatestPublication record
      requireObsoleteObservationTime
        (winner == checked)
        ( ControlledObsoleteObservationPublicationNotWinner
            identifier
            (checkedPublicationId winner)
        )
      case record.obsoleteObservedAt of
        Just _ ->
          Right
            (predecessor, ControlledObsoleteObservationTimeRetained)
        Nothing ->
          let successorRecord = record {obsoleteObservedAt = Just observedAt}
           in Right
                ( predecessor
                    { localRecords =
                        Map.insert object successorRecord predecessor.localRecords
                    },
                  ControlledObsoleteObservationTimeRecorded
                )

preparedControlledObsoleteObservationTimeResult ::
  PreparedControlledObsoleteObservationTime ->
  ControlledObsoleteObservationTimeResult
preparedControlledObsoleteObservationTimeResult
  (PreparedControlledObsoleteObservationTime prepared) =
    preparedOutput prepared

commitControlledObsoleteObservationTime ::
  PreparedControlledObsoleteObservationTime ->
  (State, ControlledObsoleteObservationTimeResult)
commitControlledObsoleteObservationTime
  (PreparedControlledObsoleteObservationTime prepared) =
    commitPrepared prepared

-- | Atomic normalization used at the outer Herald transition boundary.
-- Every controlled object that became obsolete during the just-observed input
-- receives that input's monotonic instant before the successor is exposed.
-- Existing instants are never replaced, so later publications and retries
-- cannot restart the Forward retention interval.
newtype PreparedControlledObsoleteWinnerTimes
  = PreparedControlledObsoleteWinnerTimes
      (Prepared State [GlobalObjectId])

prepareControlledObsoleteWinnerTimes ::
  MonotonicInstant ->
  State ->
  Either
    ControlledObsoleteObservationTimeError
    PreparedControlledObsoleteWinnerTimes
prepareControlledObsoleteWinnerTimes observedAt state =
  PreparedControlledObsoleteWinnerTimes
    <$> prepareTransition prepare state
  where
    prepare predecessor =
      foldM
        retainMissing
        (predecessor, [])
        (Map.elems predecessor.localRecords)

    retainMissing (current, recorded) record =
      case (controlledRecordLifecycle record, record.obsoleteObservedAt) of
        (ControlledObsolete, Nothing) -> do
          let winner = controlledRecordLatestPublication record
              observation =
                CheckedControlledObservation
                  { object = record.objectId,
                    occurrence = record.occurrenceId,
                    label = record.observedLabel,
                    structuralRole = record.structuralRole,
                    publication = winner
                  }
          prepared <-
            prepareControlledObsoleteObservationTime
              observedAt
              observation
              current
          let (successor, _) = commitControlledObsoleteObservationTime prepared
          Right (successor, recorded <> [record.objectId])
        _ -> Right (current, recorded)

preparedControlledObsoleteWinnerTimesRecorded ::
  PreparedControlledObsoleteWinnerTimes -> [GlobalObjectId]
preparedControlledObsoleteWinnerTimesRecorded
  (PreparedControlledObsoleteWinnerTimes prepared) =
    preparedOutput prepared

commitControlledObsoleteWinnerTimes ::
  PreparedControlledObsoleteWinnerTimes ->
  (State, [GlobalObjectId])
commitControlledObsoleteWinnerTimes
  (PreparedControlledObsoleteWinnerTimes prepared) =
    commitPrepared prepared

initialControlledRecord ::
  GlobalObjectId ->
  SortDefinitionOccurrenceId ->
  Maybe StructuralCarrierRole ->
  Maybe Label ->
  Set ProcessEpochId ->
  CheckedPublication ->
  ControlledLocalRecord
initialControlledRecord object occurrence structuralRole observedLabel possessionSources checked =
  ControlledLocalRecord
    { objectId = object,
      sortId = checkedPublicationSort checked,
      occurrenceId = occurrence,
      observedLabel,
      firstPublication = checked,
      status = statusFromPublication checked,
      structuralRole,
      obsoleteObservedAt = Nothing,
      possessionSources,
      observations = Map.singleton (checkedPublicationId checked) checked
    }

applyControlledObservation ::
  Maybe ProcessEpochId ->
  CheckedPublication ->
  ControlledLocalRecord ->
  (ControlledLocalRecord, ControlledPeerObservationResult)
applyControlledObservation possession checked record
  | checkedPublicationLifecycle checked == Current
      && controlledRecordLifecycle record /= ControlledCurrent =
      ( retainObservation possession checked record,
        ControlledPeerCurrentSuppressed
      )
  | checkedPublicationWinnerKey checked > controlledRecordLatestWinner record =
      let advanced =
            retainObservation
              possession
              checked
              record
                { status = statusFromPublication checked
                }
       in (advanced, ControlledPeerWinnerAdvanced)
  | otherwise =
      ( retainObservation possession checked record,
        ControlledPeerWinnerRetained
      )

retainObservation ::
  Maybe ProcessEpochId ->
  CheckedPublication ->
  ControlledLocalRecord ->
  ControlledLocalRecord
retainObservation possession checked record =
  record
    { possessionSources =
        maybe
          record.possessionSources
          (`Set.insert` record.possessionSources)
          possession,
      observations =
        Map.insert
          (checkedPublicationId checked)
          checked
          record.observations
    }

installControlledRecord :: ControlledLocalRecord -> State -> State
installControlledRecord record state =
  notifyPreparationPossessions addedPossessions notified
  where
    addedPossessions =
      Set.fromList
        [ (process, record.objectId)
        | process <- Set.toAscList record.possessionSources,
          Set.notMember process state.endedProcesses,
          Set.notMember (process, record.objectId) state.possessions
        ]
    installed =
      state
        { localRecords = Map.insert record.objectId record state.localRecords,
          possessions = Set.union addedPossessions state.possessions
        }
    notified
      | fmap readinessFacts (Map.lookup record.objectId state.localRecords) == Just (readinessFacts record) = installed
      | otherwise = notifyPreparationObject record.objectId installed
    -- Compare identifiers and current facts, never retained publication bytes,
    -- observation history, possession-source history or runtime timestamps.
    readinessFacts value =
      ( value.sortId,
        value.occurrenceId,
        controlledRecordLifecycle value,
        value.observedLabel,
        checkedPublicationId (controlledRecordLatestPublication value),
        value.structuralRole
      )

environmentRootPossessionMatchesLifecycle ::
  ProcessEpochId ->
  GlobalObjectId ->
  State ->
  Bool
environmentRootPossessionMatchesLifecycle process object state
  | Set.member process state.endedProcesses =
      Set.notMember (process, object) state.possessions
  | otherwise = Set.member (process, object) state.possessions

statusFromPublication :: CheckedPublication -> ControlledStatus
statusFromPublication checked = case checkedPublicationLifecycle checked of
  Current -> ControlledCurrentWinner checked
  Obsolete -> ControlledObsoleteWinner checked

reservationPhase :: ReservationUse -> ReservationPhase
reservationPhase use = case use of
  ReservationOpen -> Reserved
  ReservationPublished _ -> Published
  ReservationConsumed -> ConsumedByDelete

reservationFirstPublicationId :: ReservationUse -> Maybe PublicationId
reservationFirstPublicationId use = case use of
  ReservationOpen -> Nothing
  ReservationPublished publication -> Just publication
  ReservationConsumed -> Nothing

requireObservation ::
  Bool ->
  ControlledObservationError ->
  Either ControlledObservationError ()
requireObservation condition problem
  | condition = Right ()
  | otherwise = Left problem

requireFirstUse :: Bool -> ControlledFirstUseError -> Either ControlledFirstUseError ()
requireFirstUse condition problem
  | condition = Right ()
  | otherwise = Left problem

requireUpdate :: Bool -> ControlledUpdateError -> Either ControlledUpdateError ()
requireUpdate condition problem
  | condition = Right ()
  | otherwise = Left problem

requirePeer ::
  Bool ->
  ControlledPeerObservationError ->
  Either ControlledPeerObservationError ()
requirePeer condition problem
  | condition = Right ()
  | otherwise = Left problem

requireStoreObservation ::
  Bool ->
  ControlledStoreObservationError ->
  Either ControlledStoreObservationError ()
requireStoreObservation condition problem
  | condition = Right ()
  | otherwise = Left problem

requireObsoleteObservationTime ::
  Bool ->
  ControlledObsoleteObservationTimeError ->
  Either ControlledObsoleteObservationTimeError ()
requireObsoleteObservationTime condition problem
  | condition = Right ()
  | otherwise = Left problem

controlledProcessFact :: ProcessEpochId -> State -> Maybe ProcessFact
controlledProcessFact process state = Map.lookup process state.processes

controlledWriterFact :: NablaId -> State -> Maybe RootFact
controlledWriterFact nabla state = Map.lookup nabla state.writers

controlledReaderFact :: DeltaId -> State -> Maybe RootFact
controlledReaderFact delta state = Map.lookup delta state.readers

controlledProcessFacts :: State -> [ProcessFact]
controlledProcessFacts state = Map.elems state.processes

controlledRootFacts :: State -> [RootFact]
controlledRootFacts state =
  Map.elems state.writers <> Map.elems state.readers

controlledBootstrapRootFact :: GlobalObjectId -> State -> Maybe RootFact
controlledBootstrapRootFact object state =
  case Map.lookup object state.roles of
    Just (WriterRole nabla) -> Map.lookup nabla state.writers
    Just (ReaderRole delta) -> Map.lookup delta state.readers
    _ -> Nothing

-- | Whether this owner has observed the irrevocable End of the exact epoch.
controlledProcessEnded :: ProcessEpochId -> State -> Bool
controlledProcessEnded process state = Set.member process state.endedProcesses

-- | Whether the mutable direct-possession index still retains this exact
-- process/object pair. Unlike 'controlledHasNormalPossession', this diagnostic
-- view does not suppress evidence merely because the process has ended.
controlledHasDirectPossession :: ProcessEpochId -> GlobalObjectId -> State -> Bool
controlledHasDirectPossession process object state =
  Set.member (process, object) state.possessions

controlledHasNormalPossession :: ProcessEpochId -> GlobalObjectId -> State -> Bool
controlledHasNormalPossession process object state =
  controlledEffectivePossessionStrength process object state == Just Normal

-- | The exact strength of the caller's effective possession.  Bootstrap and
-- publisher evidence is always Normal; otherwise independently retained Store
-- sources join from Weak to Normal.  An ended process and an object with a
-- released delete have no effective source, even though immutable record
-- history may still name the epoch and retain the possession evidence.
controlledEffectivePossessionStrength ::
  ProcessEpochId ->
  GlobalObjectId ->
  State ->
  Maybe ReplicaStrength
controlledEffectivePossessionStrength process object state
  | controlledProcessEnded process state = Nothing
  | controlledNamedProcessEnded object state = Nothing
  | controlledObjectReleasedDeleted object state = Nothing
  | Set.member (process, object) state.possessions = Just Normal
  | otherwise =
      foldr
        joinMatchingSource
        Nothing
        (Map.toList state.storePossessions)
  where
    joinMatchingSource ((sourceProcess, _, sourceObject), strength) aggregate
      | sourceProcess == process && sourceObject == object =
          Just (maybe strength (max strength) aggregate)
      | otherwise = aggregate

-- | Every effective Normal possession pair in ascending order.  Multiple
-- Normal Store sources for the same process/object pair collapse to one entry;
-- Weak-only sources remain absent.
controlledNormalPossessionEntries :: State -> [(ProcessEpochId, GlobalObjectId)]
controlledNormalPossessionEntries state =
  Set.toAscList
    ( Set.filter
        ( \(process, object) ->
            not (Set.member process state.endedProcesses)
              && not (controlledNamedProcessEnded object state)
              && not (controlledObjectReleasedDeleted object state)
        )
        ( Set.union
            state.possessions
            ( Set.fromList
                [ (process, object)
                | ((process, _, object), Normal) <- Map.toList state.storePossessions
                ]
            )
        )
    )

controlledObjectReleasedDeleted :: GlobalObjectId -> State -> Bool
controlledObjectReleasedDeleted object state =
  controlledObjectTerminallyDeleted object state
    || case labelRecordReleasedState <$> Map.lookup object state.releasedLabels of
      Just released -> releasedLabelStateIsDeleted released
      Nothing -> False

controlledHasAnyRole :: GlobalObjectId -> State -> Bool
controlledHasAnyRole object state = Map.member object state.roles

controlledHasAnyNormalPossession :: GlobalObjectId -> State -> Bool
controlledHasAnyNormalPossession object state =
  any ((== object) . snd) (controlledNormalPossessionEntries state)

-- | The current structural vertex role, if the object is one.  Bootstrap
-- roles are immutable current facts; dynamically published controlled objects
-- contribute a role only while their retained winner is current.
controlledCurrentStructuralRole :: GlobalObjectId -> State -> Maybe StructuralCarrierRole
controlledCurrentStructuralRole object state
  | controlledObjectReleasedDeleted object state = Nothing
  | otherwise =
      case Map.lookup object state.localRecords of
        Just record
          | controlledRecordLifecycle record == ControlledCurrent -> record.structuralRole
          | otherwise -> Nothing
        Nothing -> case Map.lookup object state.roles of
          Just (ProcessRole _) -> Just ProcessEpochCarrier
          Just (WriterRole _) -> Just NablaCarrier
          Just (ReaderRole _) -> Just DeltaCarrier
          Nothing -> Nothing

data ControlledBootstrapError
  = ControlledProcessAlreadyInstalled ProcessEpochId
  | ControlledWriterAlreadyInstalled NablaId
  | ControlledReaderAlreadyInstalled DeltaId
  | ControlledIdentityConflict GlobalObjectId
  | ControlledBootstrapDescriptionConflict GlobalObjectId
  deriving stock (Eq, Show)

newtype PreparedControlledBootstrap
  = PreparedControlledBootstrap (Prepared State ())

prepareControlledBootstrap ::
  ProcessFact ->
  [RootFact] ->
  State ->
  Either ControlledBootstrapError PreparedControlledBootstrap
prepareControlledBootstrap process roots state =
  PreparedControlledBootstrap
    <$> prepareTransition (installControlledBootstrap process roots) state

-- | Observe checked canonical genesis metadata selected by a remote grant.
-- This does not grant the historical owner possession or install any application
-- namespace, placement or Store. Existing equal facts make exact retries inert.
prepareControlledBootstrapObservation :: ProcessFact -> [RootFact] -> State -> Either ControlledBootstrapError PreparedControlledBootstrap
prepareControlledBootstrapObservation process roots state =
  PreparedControlledBootstrap <$> prepareTransition observe state
  where
    observe current = do
      withProcess <- case controlledProcessFact (processFactProcessEpoch process) current of
        Nothing -> insertProcess process current
        Just old | old == process -> Right current
        Just _ -> Left (ControlledProcessAlreadyInstalled (processFactProcessEpoch process))
      withRoots <- foldM observeRoot withProcess roots
      -- Bootstrap observation installs metadata, not historical-owner grants.
      -- Preserve preexisting notifications while discarding only these temporary
      -- possession insertions; newly observed object/process facts remain real.
      Right
        ( withRoots
            { possessions = current.possessions,
              pendingPreparationChanges = withRoots.pendingPreparationChanges {changedPossessions = current.pendingPreparationChanges.changedPossessions}
            },
          ()
        )
    observeRoot current root = case controlledBootstrapRootFact (rootObject root) current of
      Nothing -> insertRoot root current
      Just old | old == root -> Right current
      Just _ -> Left (ControlledIdentityConflict (rootObject root))
    rootObject root = case rootFactRole root of
      ControlledWriter writer _ -> globalObjectIdFromNablaId writer
      ControlledReader reader -> globalObjectIdFromDeltaId reader

-- | Install the ordinary immutable descriptions admitted by the complete
-- checked genesis environment. RootFacts retain their authority metadata, while
-- the same objects now have real carrier winners for reads, transfer, deletion
-- and disappearance. Remote observation passes Nothing and grants no historical
-- owner possession. Exact retained observations are idempotent.
prepareControlledBootstrapDescriptions ::
  Maybe ProcessEpochId ->
  [CheckedControlledObservation] ->
  State ->
  Either ControlledBootstrapError PreparedControlledBootstrap
prepareControlledBootstrapDescriptions owner observations state =
  PreparedControlledBootstrap <$> prepareTransition seed state
  where
    seed predecessor = (,()) <$> foldM install predecessor observations
    install predecessor observation
      | controlledObjectTerminallyDeleted (checkedControlledObservationObject observation) predecessor = Right predecessor
      | otherwise = do
          let object = checkedControlledObservationObject observation
              publication = checkedControlledObservationPublication observation
              role = checkedControlledObservationStructuralRole observation
              label = checkedControlledObservationLabel observation
              possession = maybe Set.empty Set.singleton owner
              conflict = ControlledBootstrapDescriptionConflict object
          case owner of
            Just process | label /= Just (ProcessLabel process, 0) -> Left conflict
            _ -> Right ()
          case controlledCurrentStructuralRole object predecessor of
            Just expected | Just expected /= role -> Left conflict
            _ -> Right ()
          case controlledLocalRecord object predecessor of
            Just retained
              | Map.lookup (checkedPublicationId publication) retained.observations == Just publication -> Right predecessor
              | Nothing <- owner ->
                  either
                    (const (Left conflict))
                    (Right . fst . commitControlledPeerObservation)
                    (prepareControlledStartupObservation observation predecessor)
              | otherwise -> Left conflict
            Nothing ->
              Right
                $ installControlledRecord
                  (initialControlledRecord object (checkedControlledObservationOccurrence observation) role label possession publication)
                  predecessor

commitControlledBootstrap :: PreparedControlledBootstrap -> State
commitControlledBootstrap (PreparedControlledBootstrap prepared) =
  fst (commitPrepared prepared)

installControlledBootstrap ::
  ProcessFact ->
  [RootFact] ->
  State ->
  Either ControlledBootstrapError (State, ())
installControlledBootstrap process roots state = do
  withProcess <- insertProcess process state
  successor <- foldM (flip insertRoot) withProcess roots
  Right (successor, ())

insertProcess ::
  ProcessFact ->
  State ->
  Either ControlledBootstrapError State
insertProcess fact state
  | Map.member process state.processes =
      Left (ControlledProcessAlreadyInstalled process)
  | Map.member object state.roles =
      Left (ControlledIdentityConflict object)
  | otherwise =
      Right
        (notifyPreparationProcess process . notifyPreparationObject object . notifyPreparationPossessions (Set.singleton (process, object)) $ state)
          { processes = Map.insert process fact state.processes,
            roles =
              Map.insert object (ProcessRole process) state.roles,
            possessions =
              Set.insert (process, object) state.possessions
          }
  where
    process = processFactProcessEpoch fact
    object = globalObjectIdFromProcessEpochId process

insertRoot ::
  RootFact ->
  State ->
  Either ControlledBootstrapError State
insertRoot fact state = case fact.role of
  ControlledWriter nabla _ -> insertWriter nabla
  ControlledReader delta -> insertReader delta
  where
    insertWriter nabla
      | Map.member nabla state.writers =
          Left (ControlledWriterAlreadyInstalled nabla)
      | Map.member object state.roles =
          Left (ControlledIdentityConflict object)
      | otherwise =
          Right
            (notifyPreparationObject object . notifyPreparationPossessions (Set.singleton (owner, object)) $ state)
              { writers = Map.insert nabla fact state.writers,
                roles =
                  Map.insert object (WriterRole nabla) state.roles,
                possessions =
                  Set.insert (owner, object) state.possessions
              }
      where
        object = globalObjectIdFromNablaId nabla
    insertReader delta
      | Map.member delta state.readers =
          Left (ControlledReaderAlreadyInstalled delta)
      | Map.member object state.roles =
          Left (ControlledIdentityConflict object)
      | otherwise =
          Right
            (notifyPreparationObject object . notifyPreparationPossessions (Set.singleton (owner, object)) $ state)
              { readers = Map.insert delta fact state.readers,
                roles =
                  Map.insert object (ReaderRole delta) state.roles,
                possessions =
                  Set.insert (owner, object) state.possessions
              }
      where
        object = globalObjectIdFromDeltaId delta
    owner = rootFactProcessEpoch fact

-- | Source-admitted ordinary possession. An alias is deliberately insufficient.
newtype CheckedControlledGrant = CheckedControlledGrant GlobalObjectId
  deriving stock (Eq, Show)

data ControlledGrantError
  = ControlledGrantSourceNotPossessed ProcessEpochId GlobalObjectId
  | ControlledGrantObjectNotCurrent GlobalObjectId
  | ControlledGrantRecipientNotLive ProcessEpochId
  deriving stock (Eq, Show)

controlledObjectCurrent :: GlobalObjectId -> State -> Bool
controlledObjectCurrent object state
  | controlledObjectReleasedDeleted object state = False
  | Just record <- controlledLocalRecord object state = controlledRecordLifecycle record == ControlledCurrent
  | otherwise = case Map.lookup object state.roles of
      Just (ProcessRole process) -> not (controlledProcessEnded process state)
      Just _ -> True
      Nothing -> False

checkControlledGrant :: ProcessEpochId -> GlobalObjectId -> State -> Either ControlledGrantError CheckedControlledGrant
checkControlledGrant source object state
  | not (controlledObjectCurrent object state) = Left (ControlledGrantObjectNotCurrent object)
  | not (controlledHasNormalPossession source object state) = Left (ControlledGrantSourceNotPossessed source object)
  | otherwise = Right (CheckedControlledGrant object)

-- | The startup transfer owner has admitted the source's retained Normal grant.
-- Recheck only local object liveness here; source possession is not global state.
checkTransferredControlledGrant :: GlobalObjectId -> State -> Either ControlledGrantError CheckedControlledGrant
checkTransferredControlledGrant object state
  | not (controlledObjectCurrent object state) = Left (ControlledGrantObjectNotCurrent object)
  | otherwise = Right (CheckedControlledGrant object)

controlledGrantObject :: CheckedControlledGrant -> GlobalObjectId
controlledGrantObject (CheckedControlledGrant object) = object

newtype PreparedControlledGrants = PreparedControlledGrants State

-- | Copy accepted grants; later source possession loss does not revoke admission.
-- Destination currentness and liveness remain required when installing locally.
prepareControlledGrants :: ProcessEpochId -> [CheckedControlledGrant] -> State -> Either ControlledGrantError PreparedControlledGrants
prepareControlledGrants recipient grants state
  | controlledProcessEnded recipient state || Map.notMember recipient state.processes = Left (ControlledGrantRecipientNotLive recipient)
  | otherwise = do
      mapM_ requireCurrent grants
      let supplied = Set.fromList [(recipient, controlledGrantObject grant) | grant <- grants]
          inserted = Set.filter (`Set.notMember` state.possessions) supplied
      Right (PreparedControlledGrants (notifyPreparationPossessions inserted state) {possessions = Set.union state.possessions inserted})
  where
    requireCurrent grant
      | controlledObjectCurrent (controlledGrantObject grant) state = Right ()
      | otherwise = Left (ControlledGrantObjectNotCurrent (controlledGrantObject grant))

commitControlledGrants :: PreparedControlledGrants -> State
commitControlledGrants (PreparedControlledGrants state) = state

-- | A retained process alias remains an identity after End, but its ordinary
-- live-process possession capability no longer exists for any holder.
controlledNamedProcessEnded :: GlobalObjectId -> State -> Bool
controlledNamedProcessEnded object state = case Map.lookup object state.roles of
  Just (ProcessRole process) -> controlledProcessEnded process state
  _ -> False
