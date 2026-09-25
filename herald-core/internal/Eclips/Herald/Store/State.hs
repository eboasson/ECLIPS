{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Herald-private system views and local typed application delta stores.
module Eclips.Herald.Store.State
  ( State,
    initialState,
    primordialSeedPublications,
    StoreProvenance (..),
    LocalStoreSpec,
    localStoreSpec,
    systemViewStoreSpecifications,
    StoreSlot,
    storeSlotProvenance,
    storeSlotDelta,
    storeSlotSortId,
    storeSlotOccurrenceId,
    storeSlotIncarnation,
    storeSlotContents,
    storeSlotRevision,
    storeSlotAppliedPublicationIds,
    storeSlotApplicationReceipts,
    storeSlotApplicationObservations,
    storeSlotTerminalObjectPurges,
    storeSlotTerminalObjectPurgeCause,
    storeTerminalObjectPurgeEntries,
    StoreObservationOrigin (..),
    StoreObservationEnvelope,
    storeObservationEnvelope,
    storeObservationEnvelopeSourceProcess,
    storeObservationEnvelopeSortOccurrence,
    storeObservationEnvelopeSourceTopology,
    storeObservationEnvelopeControlPrerequisite,
    storeObservationEnvelopeStructuralStamp,
    StoreObservationProblem (..),
    RetainedStoreObservation,
    retainedStoreObservation,
    retainedStoreObservationPublication,
    retainedStoreObservationIncomingStrength,
    retainedStoreObservationSortOccurrence,
    retainedStoreObservationOrigin,
    RetainedStoreChange,
    retainedStoreChangeRevision,
    retainedStoreChangeObservation,
    retainedStoreChangeTransition,
    RetainedStoreSnapshot,
    retainedStoreSnapshotIncarnation,
    retainedStoreSnapshotRevision,
    retainedStoreSnapshotSortId,
    retainedStoreSnapshotSortOccurrence,
    retainedStoreSnapshotFacts,
    RetainedStoreSnapshotFact,
    retainedStoreSnapshotFactRepresentative,
    retainedStoreSnapshotFactStrengthWitness,
    retainedStoreSnapshotFactStrength,
    StoreHistoryQueryError (..),
    retainedStoreSnapshotAt,
    retainedStoreAlignmentSnapshotAt,
    currentStoreObservationSuppressed,
    retainedStoreChangesAfter,
    StoreHistoryInvariantProblem (..),
    validateStoreHistoryState,
    retainedStoreSlots,
    lookupRetainedStoreSlot,
    changedSourceStoreIncarnations,
    clearSourceStoreChanges,
    takePreparationSlotChanges,
    clearPreparationSlotChanges,
    takeAlignmentLossChange,
    clearAlignmentLossChange,
    takeAlignmentMemberStoreChanges,
    clearAlignmentMemberStoreChanges,
    passivateStoreForInvariantTest,
    reactivateRetainedStoreForInvariantTest,
    removeTerminalObjectPurgeForInvariantTest,
    removeStorePublicationReceiptForInvariantTest,
    removeStoreEffectivePublicationForInvariantTest,
    insertStoreObservationForInvariantTest,
    lookupStoreSlot,
    storeSlots,
    StoreTransitionError (..),
    ExactLocalTakeKey,
    exactLocalTakeKey,
    PreparedExactLocalTake,
    prepareExactLocalTake,
    commitExactLocalTake,
    PreparedStoreApplication,
    prepareStoreApplication,
    prepareRetainedStoreApplication,
    commitStoreApplication,
    PeerStoreDestination,
    peerStoreDestination,
    peerStoreDestinationDelta,
    peerStoreDestinationIncarnation,
    peerStoreDestinationStrength,
    PeerStoreDisposition (..),
    PeerStoreOutcome,
    peerStoreOutcomeDestination,
    peerStoreOutcomeDisposition,
    PreparedPeerStoreApplication,
    preparePeerStoreApplication,
    prepareAlignmentStoreApplication,
    commitPeerStoreApplication,
    ControlledObjectPurgeError (..),
    ControlledObjectPurgeSummary,
    controlledObjectPurgeSummarySortId,
    controlledObjectPurgeSummaryObjectKey,
    controlledObjectPurgeSummaryMatchedIncarnations,
    controlledObjectPurgeSummaryAffectedIncarnations,
    controlledObjectPurgeSummaryPurgedFactCount,
    PreparedControlledObjectPurge,
    prepareControlledObjectPurge,
    commitControlledObjectPurge,
    RegularRetirementSuppression,
    regularRetirementSuppressionSubject,
    regularRetirementSuppressionLocalPublicationCut,
    regularRetirementSuppressionResolveIndex,
    regularRetirementSuppressions,
    lookupRegularRetirementSuppression,
    RegularDefinitionWriteDisposition (..),
    classifyRegularDefinitionWrite,
    RegularDefinitionRetirementError (..),
    RegularDefinitionRetirementDisposition (..),
    RegularDefinitionRetirementSummary,
    regularDefinitionRetirementSummarySubject,
    regularDefinitionRetirementSummarySuppression,
    regularDefinitionRetirementSummaryDisposition,
    regularDefinitionRetirementSummaryMatchedIncarnations,
    regularDefinitionRetirementSummaryAffectedIncarnations,
    regularDefinitionRetirementSummaryPurgedFactCount,
    PreparedRegularDefinitionRetirement,
    prepareRegularDefinitionRetirement,
    preparedRegularDefinitionRetirementSummary,
    commitRegularDefinitionRetirement,
    StructuralStorePatchError (..),
    PreparedStructuralStorePatch,
    prepareStructuralStorePatch,
    commitStructuralStorePatch,
    PreparedStoreBootstrap,
    StoreBootstrapError (..),
    StorePreparationError (..),
    prepareStoreBootstrap,
    commitStoreBootstrap,
    PreparedEnvironmentStoreSeed,
    prepareEnvironmentStoreSeed,
    commitEnvironmentStoreSeed,
  )
where

import Control.Monad (foldM, unless)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment
  ( HeraldPublicationPosition,
    HeraldPublicationPrefix (..),
    StoreRevision,
    initialStoreRevision,
    nextStoreRevision,
  )
import Eclips.Domain.Disappearance
  ( DisappearanceSubject,
    DisappearanceSubjectView (..),
    disappearanceSubjectView,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
    StructuralOccurrenceId,
    TopologyCutId,
    controlIndexWord64,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationId,
    checkedPublicationKey,
    checkedPublicationLogicalState,
    checkedPublicationSort,
  )
import Eclips.Domain.Route
  ( FrozenRoute,
    ReplicaStrength (..),
    destinationDelta,
    destinationStoreIncarnation,
    destinationStrength,
    routeDestinations,
  )
import Eclips.Domain.Sort.Descriptor (ObjectKey)
import Eclips.Domain.Startup
  ( PredefinedSortRole,
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
  )
import Eclips.Domain.Store
  ( DeltaStore,
    RetainedApplyResult (..),
    RetainedSnapshot,
    RetainedTransition,
    StoreError (StoreSortMismatch),
    applyPublication,
    applyPublicationDetailed,
    emptyDeltaStore,
    localTake,
    lookupVisible,
    purgeObjectKey,
    retainedInstances,
    retainedSnapshot,
    retainedSnapshotLogicalStrengthFacts,
    retainedSnapshotSort,
    retainedTransitionIncomingStrength,
    retainedTransitionPublication,
    storedPublication,
    visibleInstances,
  )
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    StructuralConsequenceCauseView (..),
    structuralConsequenceCauseView,
  )
import Eclips.Herald.Disappearance.OwnerEvidence
  ( checkedPublicationIsRegularRetirementResidual,
  )
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    checkedLocalHeraldEpoch,
    checkedPrimordialReplicas,
    checkedSystemId,
    primordialReplicaOccurrenceId,
    primordialReplicaPublication,
    primordialReplicaRole,
    primordialReplicaSortId,
  )
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
  )
import Eclips.Herald.PeerPublication
  ( StructuralOccurrenceStamp,
    structuralOccurrenceStampPublication,
  )
import Eclips.Herald.Structural.Debt
  ( sortOccurrenceDefinition,
    sortOccurrenceSortId,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation

data StoreProvenance
  = HeraldSystemView HeraldEpoch PredefinedSortRole
  | ApplicationReader ProcessEpochId PredefinedSortRole
  | StructuralBaselineReader
      PublicationId
      ProcessEpochId
      GlobalObjectId
      Reconciliation.FreshStoreIncarnationSource
  | StructuralApplicationReader
      StructuralOccurrenceId
      ProcessEpochId
      GlobalObjectId
      Reconciliation.FreshStoreIncarnationSource
  | StructuralControlReader
      StructuralConsequenceCause
      ProcessEpochId
      GlobalObjectId
      Reconciliation.FreshStoreIncarnationSource
  deriving stock (Eq, Show)

data LocalStoreSpec = LocalStoreSpec
  { provenance :: StoreProvenance,
    delta :: DeltaId,
    sortId :: SortId,
    occurrenceId :: SortDefinitionOccurrenceId,
    incarnation :: StoreIncarnationId
  }
  deriving stock (Eq, Show)

localStoreSpec ::
  StoreProvenance ->
  DeltaId ->
  SortId ->
  SortDefinitionOccurrenceId ->
  StoreIncarnationId ->
  LocalStoreSpec
localStoreSpec = LocalStoreSpec

-- | The canonical six private system-view stores owned by this Herald. They do
-- not fabricate controlled, graph, or placement facts; the first data vertical
-- makes these already-real destinations routable.
systemViewStoreSpecifications :: CheckedHeraldGenesis -> [LocalStoreSpec]
systemViewStoreSpecifications genesis =
  [ LocalStoreSpec
      { provenance = HeraldSystemView herald role,
        delta = deriveSystemViewDeltaId system herald role,
        sortId = primordialReplicaSortId replica,
        occurrenceId = primordialReplicaOccurrenceId replica,
        incarnation = deriveSystemViewStoreIncarnationId system herald role
      }
  | replica <- checkedPrimordialReplicas genesis,
    let role = primordialReplicaRole replica
  ]
  where
    system = checkedSystemId genesis
    herald = checkedLocalHeraldEpoch genesis

data StoreSlot = StoreSlot
  { provenance :: StoreProvenance,
    delta :: DeltaId,
    sortId :: SortId,
    occurrenceId :: SortDefinitionOccurrenceId,
    incarnation :: StoreIncarnationId,
    -- Unprojected immutable-history state. Terminal object purges and regular
    -- retirement suppression affect only 'contents'; accepted observations
    -- still advance this transcript through the ordinary winner law.
    transcriptContents :: DeltaStore,
    contents :: DeltaStore,
    appliedPublications :: Map PublicationId ReplicaStrength,
    -- Every accepted Store observation, including one which loses the raw
    -- retained winner law.  The latter matters after regular retirement: an
    -- equal post-Resolve definition can be the first effective observation of
    -- its new occurrence even when its cross-writer PublicationId sorts below
    -- the immutable pre-retirement raw representative.
    applicationObservations :: [RetainedStoreObservation],
    revision :: StoreRevision,
    baseObservations :: [RetainedStoreObservation],
    retainedHistory :: Map StoreRevision RetainedStoreChange,
    retainedSnapshots :: Map StoreRevision RetainedSnapshot,
    terminalObjectPurges :: Map ObjectKey StructuralConsequenceCause
  }
  deriving stock (Eq, Show)

storeSlotProvenance :: StoreSlot -> StoreProvenance
storeSlotProvenance slot = slot.provenance

storeSlotDelta :: StoreSlot -> DeltaId
storeSlotDelta slot = slot.delta

storeSlotSortId :: StoreSlot -> SortId
storeSlotSortId slot = slot.sortId

storeSlotOccurrenceId :: StoreSlot -> SortDefinitionOccurrenceId
storeSlotOccurrenceId slot = slot.occurrenceId

storeSlotIncarnation :: StoreSlot -> StoreIncarnationId
storeSlotIncarnation slot = slot.incarnation

storeSlotContents :: StoreSlot -> DeltaStore
storeSlotContents slot = slot.contents

-- | Current retained-state revision of this exact incarnation.
storeSlotRevision :: StoreSlot -> StoreRevision
storeSlotRevision slot = slot.revision

-- | Immutable identities of publications applied to this exact incarnation.
-- Visibility and retained-winner replacement do not erase a receipt.
storeSlotAppliedPublicationIds :: StoreSlot -> [PublicationId]
storeSlotAppliedPublicationIds slot = Map.keys slot.appliedPublications

-- | Monotone maximum routed strength receipted for every publication applied to
-- this incarnation.
storeSlotApplicationReceipts :: StoreSlot -> [(PublicationId, ReplicaStrength)]
storeSlotApplicationReceipts slot = Map.toAscList slot.appliedPublications

-- | Immutable accepted observation evidence for every receipted publication.
-- Exact duplicate observations are retained once.  Unlike the retained-change
-- transcript this also includes observations which did not advance the raw
-- winner, so an occurrence-qualified effective projection remains replayable.
storeSlotApplicationObservations :: StoreSlot -> [RetainedStoreObservation]
storeSlotApplicationObservations slot = slot.applicationObservations

-- | Monotone, payload-free suppression installed for object keys that have
-- reached an ordered terminal deletion.  The cause remains Store-private
-- provenance; immutable receipt and retained-history transcripts are not
-- rewritten by a purge.
storeSlotTerminalObjectPurges ::
  StoreSlot -> [(ObjectKey, StructuralConsequenceCause)]
storeSlotTerminalObjectPurges slot = Map.toAscList slot.terminalObjectPurges

storeSlotTerminalObjectPurgeCause ::
  ObjectKey -> StoreSlot -> Maybe StructuralConsequenceCause
storeSlotTerminalObjectPurgeCause key slot =
  Map.lookup key slot.terminalObjectPurges

-- | Source facts shared by every destination application of one admitted
-- publication. Destination delta/incarnation and routed strength are
-- deliberately absent.
data StoreObservationEnvelope
  = StoreObservationEnvelope
      ProcessEpochId
      SortDefinitionOccurrenceId
      TopologyCutId
      ControlIndex
      (Maybe StructuralOccurrenceStamp)
  deriving stock (Eq, Show)

storeObservationEnvelope ::
  ProcessEpochId ->
  SortDefinitionOccurrenceId ->
  TopologyCutId ->
  ControlIndex ->
  Maybe StructuralOccurrenceStamp ->
  StoreObservationEnvelope
storeObservationEnvelope = StoreObservationEnvelope

storeObservationEnvelopeSourceProcess ::
  StoreObservationEnvelope -> ProcessEpochId
storeObservationEnvelopeSourceProcess
  (StoreObservationEnvelope process _ _ _ _) = process

storeObservationEnvelopeSortOccurrence ::
  StoreObservationEnvelope -> SortDefinitionOccurrenceId
storeObservationEnvelopeSortOccurrence
  (StoreObservationEnvelope _ occurrence _ _ _) = occurrence

storeObservationEnvelopeSourceTopology ::
  StoreObservationEnvelope -> TopologyCutId
storeObservationEnvelopeSourceTopology
  (StoreObservationEnvelope _ _ topology _ _) = topology

storeObservationEnvelopeControlPrerequisite ::
  StoreObservationEnvelope -> ControlIndex
storeObservationEnvelopeControlPrerequisite
  (StoreObservationEnvelope _ _ _ control _) = control

storeObservationEnvelopeStructuralStamp ::
  StoreObservationEnvelope -> Maybe StructuralOccurrenceStamp
storeObservationEnvelopeStructuralStamp
  (StoreObservationEnvelope _ _ _ _ stamp) = stamp

-- | Exact destination-free origin of one retained observation. Primordial
-- catalogue values form revision-zero trusted base state; every later routed
-- observation retains its complete re-admission envelope.
data StoreObservationOrigin
  = PrimordialStoreObservation
  | RoutedStoreObservation StoreObservationEnvelope
  deriving stock (Eq, Show)

-- | Contradictions within destination-free observation evidence. The
-- publication's sort is checked against each destination Store separately;
-- these checks cover the relationships wholly owned by the observation.
data StoreObservationProblem
  = StoreObservationEnvelopeOccurrenceMismatch
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | StoreObservationStructuralStampPublicationMismatch
      PublicationId
      PublicationId
  deriving stock (Eq, Show)

-- | One exact source observation. Incoming strength is the strength routed to
-- this store, not the joined strength of its logical state.
data RetainedStoreObservation
  = RetainedStoreObservation
      CheckedPublication
      ReplicaStrength
      SortDefinitionOccurrenceId
      StoreObservationOrigin
  deriving stock (Eq, Show)

-- | Admit one exact Store observation. Alignment callers must attenuate the
-- incoming strength before construction; the resulting observation can then
-- be replayed without losing whether its evidence was primordial or routed.
retainedStoreObservation ::
  CheckedPublication ->
  ReplicaStrength ->
  SortDefinitionOccurrenceId ->
  StoreObservationOrigin ->
  Either StoreObservationProblem RetainedStoreObservation
retainedStoreObservation publication strength occurrence origin = do
  case origin of
    PrimordialStoreObservation -> Right ()
    RoutedStoreObservation envelope -> do
      let envelopeOccurrence = storeObservationEnvelopeSortOccurrence envelope
      unless
        (envelopeOccurrence == occurrence)
        ( Left
            ( StoreObservationEnvelopeOccurrenceMismatch
                occurrence
                envelopeOccurrence
            )
        )
      case storeObservationEnvelopeStructuralStamp envelope of
        Nothing -> Right ()
        Just stamp ->
          let identifier = checkedPublicationId publication
              stampedIdentifier = structuralOccurrenceStampPublication stamp
           in unless
                (stampedIdentifier == identifier)
                ( Left
                    ( StoreObservationStructuralStampPublicationMismatch
                        identifier
                        stampedIdentifier
                    )
                )
  Right (RetainedStoreObservation publication strength occurrence origin)

retainedStoreObservationPublication ::
  RetainedStoreObservation -> CheckedPublication
retainedStoreObservationPublication
  (RetainedStoreObservation publication _ _ _) = publication

retainedStoreObservationIncomingStrength ::
  RetainedStoreObservation -> ReplicaStrength
retainedStoreObservationIncomingStrength
  (RetainedStoreObservation _ strength _ _) = strength

retainedStoreObservationSortOccurrence ::
  RetainedStoreObservation -> SortDefinitionOccurrenceId
retainedStoreObservationSortOccurrence
  (RetainedStoreObservation _ _ occurrence _) = occurrence

retainedStoreObservationOrigin ::
  RetainedStoreObservation -> StoreObservationOrigin
retainedStoreObservationOrigin
  (RetainedStoreObservation _ _ _ origin) = origin

-- | One alignment-relevant Store transition. Its key is the exact revision
-- reached by applying the observation. The raw transition is absent when the
-- immutable transcript winner did not change but the accepted observation is
-- nevertheless new: its strength or routed control provenance can affect a
-- later retirement projection and must therefore remain revision-addressable
-- for alignment.
data RetainedStoreChange
  = RetainedStoreChange
      StoreRevision
      RetainedStoreObservation
      (Maybe RetainedTransition)
  deriving stock (Eq, Show)

retainedStoreChangeRevision :: RetainedStoreChange -> StoreRevision
retainedStoreChangeRevision (RetainedStoreChange revision _ _) = revision

retainedStoreChangeObservation ::
  RetainedStoreChange -> RetainedStoreObservation
retainedStoreChangeObservation (RetainedStoreChange _ observation _) = observation

retainedStoreChangeTransition :: RetainedStoreChange -> Maybe RetainedTransition
retainedStoreChangeTransition (RetainedStoreChange _ _ transition) = transition

-- | A canonical logical-state fact at an exact revision. Representative
-- provenance and joined-strength provenance remain separate so transfer never
-- claims that the representative publication arrived at the joined strength.
data RetainedStoreSnapshotFact
  = RetainedStoreSnapshotFact
      RetainedStoreObservation
      RetainedStoreObservation
      ReplicaStrength
  deriving stock (Eq, Show)

retainedStoreSnapshotFactRepresentative ::
  RetainedStoreSnapshotFact -> RetainedStoreObservation
retainedStoreSnapshotFactRepresentative
  (RetainedStoreSnapshotFact representative _ _) = representative

retainedStoreSnapshotFactStrengthWitness ::
  RetainedStoreSnapshotFact -> RetainedStoreObservation
retainedStoreSnapshotFactStrengthWitness
  (RetainedStoreSnapshotFact _ witness _) = witness

retainedStoreSnapshotFactStrength ::
  RetainedStoreSnapshotFact -> ReplicaStrength
retainedStoreSnapshotFactStrength
  (RetainedStoreSnapshotFact _ _ strength) = strength

-- | Complete destination-free hidden state captured at one exact incarnation
-- revision. Facts are in canonical logical-state order.
data RetainedStoreSnapshot
  = RetainedStoreSnapshot
      StoreIncarnationId
      StoreRevision
      SortId
      SortDefinitionOccurrenceId
      [RetainedStoreSnapshotFact]
  deriving stock (Eq, Show)

retainedStoreSnapshotIncarnation ::
  RetainedStoreSnapshot -> StoreIncarnationId
retainedStoreSnapshotIncarnation
  (RetainedStoreSnapshot incarnation _ _ _ _) = incarnation

retainedStoreSnapshotRevision :: RetainedStoreSnapshot -> StoreRevision
retainedStoreSnapshotRevision
  (RetainedStoreSnapshot _ revision _ _ _) = revision

retainedStoreSnapshotSortId :: RetainedStoreSnapshot -> SortId
retainedStoreSnapshotSortId (RetainedStoreSnapshot _ _ sortId _ _) = sortId

retainedStoreSnapshotSortOccurrence ::
  RetainedStoreSnapshot -> SortDefinitionOccurrenceId
retainedStoreSnapshotSortOccurrence
  (RetainedStoreSnapshot _ _ _ occurrence _) = occurrence

retainedStoreSnapshotFacts ::
  RetainedStoreSnapshot -> [RetainedStoreSnapshotFact]
retainedStoreSnapshotFacts (RetainedStoreSnapshot _ _ _ _ facts) = facts

data StoreHistoryQueryError
  = StoreHistoryIncarnationMissing StoreIncarnationId
  | StoreHistoryRevisionUnavailable
      StoreIncarnationId
      StoreRevision
      StoreRevision
  | StoreHistoryRepresentativeEvidenceMissing
      StoreIncarnationId
      StoreRevision
      PublicationId
  | StoreHistoryStrengthEvidenceMissing
      StoreIncarnationId
      StoreRevision
      PublicationId
      ReplicaStrength
  deriving stock (Eq, Show)

-- | Contradictions in the Store owner's retained-history projection. These
-- cannot be produced by checked Store transitions; the top-level Herald
-- invariant composes this pure audit at its ownership boundary.
data StoreHistoryInvariantProblem
  = StoreHistoryRetainedIncarnationKeyMismatch
      StoreIncarnationId
      StoreIncarnationId
  | StoreHistoryActiveDeltaKeyMismatch DeltaId DeltaId
  | StoreHistoryActiveRetainedMismatch DeltaId StoreIncarnationId
  | StoreHistoryActiveIncarnationClosed DeltaId StoreIncarnationId
  | StoreHistoryClosedIncarnationMissing StoreIncarnationId
  | StoreHistorySnapshotRevisionSetMismatch
      StoreIncarnationId
      [StoreRevision]
      [StoreRevision]
  | StoreHistoryChangeRevisionSetMismatch
      StoreIncarnationId
      [StoreRevision]
      [StoreRevision]
  | StoreHistoryApplicationObservationCoverageMismatch StoreIncarnationId
  | StoreHistoryChangeKeyMismatch
      StoreIncarnationId
      StoreRevision
      StoreRevision
  | StoreHistorySnapshotSortMismatch
      StoreIncarnationId
      StoreRevision
      SortId
      SortId
  | StoreHistoryObservationSortMismatch
      StoreIncarnationId
      PublicationId
      SortId
      SortId
  | StoreHistoryObservationOccurrenceMismatch
      StoreIncarnationId
      PublicationId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | StoreHistoryEnvelopeOccurrenceMismatch
      StoreIncarnationId
      PublicationId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | StoreHistoryStructuralStampPublicationMismatch
      StoreIncarnationId
      PublicationId
      PublicationId
  | StoreHistoryTransitionObservationMismatch
      StoreIncarnationId
      StoreRevision
  | StoreHistoryReceiptMissingOrWeaker
      StoreIncarnationId
      PublicationId
      ReplicaStrength
      (Maybe ReplicaStrength)
  | StoreHistoryReplayRejected
      StoreIncarnationId
      StoreRevision
      StoreError
  | StoreHistoryTransitionMismatch
      StoreIncarnationId
      StoreRevision
  | StoreHistorySnapshotReplayMismatch
      StoreIncarnationId
      StoreRevision
  | StoreHistoryTranscriptSnapshotMismatch
      StoreIncarnationId
      StoreRevision
  | StoreHistoryCurrentSnapshotMismatch
      StoreIncarnationId
      StoreRevision
  | StoreHistoryTerminalPurgeProjectionMismatch
      SortId
      StoreIncarnationId
  deriving stock (Eq, Show)

-- | One exact resolved regular-definition occurrence and the local publication
-- cut which its successful absence proof covered.  The descriptor bytes are
-- retained only through the opaque subject claim; this value grants no
-- effective sort-definition visibility by itself.
data RegularRetirementSuppression
  = RegularRetirementSuppression
      DisappearanceSubject
      HeraldPublicationPrefix
      ControlIndex
  deriving stock (Eq, Show)

regularRetirementSuppressionSubject ::
  RegularRetirementSuppression -> DisappearanceSubject
regularRetirementSuppressionSubject
  (RegularRetirementSuppression subject _ _) = subject

regularRetirementSuppressionLocalPublicationCut ::
  RegularRetirementSuppression -> HeraldPublicationPrefix
regularRetirementSuppressionLocalPublicationCut
  (RegularRetirementSuppression _ localCut _) = localCut

regularRetirementSuppressionResolveIndex ::
  RegularRetirementSuppression -> ControlIndex
regularRetirementSuppressionResolveIndex
  (RegularRetirementSuppression _ _ resolveIndex) = resolveIndex

-- | Classification of a matching definition write against one exact retired
-- occurrence.  A caller with a local Herald-wide publication position can
-- distinguish work already covered by the proof from later work which lacks
-- the ordered redefinition prerequisite.  Remote Store application has no
-- local position and conservatively uses the latter stale classification.
data RegularDefinitionWriteDisposition
  = RegularDefinitionWriteUnsuppressed
  | RegularDefinitionWriteCoveredByCut RegularRetirementSuppression
  | RegularDefinitionWriteMissingResolvePrerequisite RegularRetirementSuppression
  | RegularDefinitionWriteAdmissibleAfterResolve RegularRetirementSuppression
  deriving stock (Eq, Show)

data State = State
  { primordialPublications :: [CheckedPublication],
    stores :: Map DeltaId StoreSlot,
    retainedStores :: Map StoreIncarnationId StoreSlot,
    -- Coalesced retained-prefix changes awaiting the source-transfer stage.
    -- Effective projection changes do not extend the immutable raw prefix.
    sourceStoreChanges :: !(Set StoreIncarnationId),
    -- Active slot identity, independently consumed by preparation readiness.
    preparationSlotChanges :: !(Set DeltaId),
    alignmentLossChanged :: !Bool,
    alignmentMemberStoreChanges :: !(Set StoreIncarnationId),
    closedStores :: Set StoreIncarnationId,
    -- Authoritative terminal suppression independent of Store incarnation
    -- lifetime. A controlled object may be deleted while its application-
    -- defined sort has no local Store; retaining the coordinate here ensures
    -- that every later incarnation still inherits the deletion.
    terminalObjectPurges :: Map (SortId, ObjectKey) StructuralConsequenceCause,
    -- Exact occurrence-qualified regular-definition retirements. Raw Store
    -- history remains immutable; this ledger determines the effective
    -- projection and rejects stale equal definitions and Nabla/Delta sort
    -- references until their observation carries the ordered retirement
    -- prerequisite.
    regularRetirementSuppressions ::
      Map DisappearanceSubject RegularRetirementSuppression
  }
  deriving stock (Eq)

regularRetirementSuppressions :: State -> [RegularRetirementSuppression]
regularRetirementSuppressions = Map.elems . (.regularRetirementSuppressions)

lookupRegularRetirementSuppression ::
  DisappearanceSubject -> State -> Maybe RegularRetirementSuppression
lookupRegularRetirementSuppression subject state =
  Map.lookup subject state.regularRetirementSuppressions

-- | Classify a write which has already been established as matching the
-- supplied regular subject.  The resolve prerequisite is authoritative for
-- post-resolution admission; the optional position only refines why an older
-- write is suppressed.
classifyRegularDefinitionWrite ::
  DisappearanceSubject ->
  Maybe HeraldPublicationPosition ->
  ControlIndex ->
  State ->
  RegularDefinitionWriteDisposition
classifyRegularDefinitionWrite subject position prerequisite state =
  case lookupRegularRetirementSuppression subject state of
    Nothing -> RegularDefinitionWriteUnsuppressed
    Just suppression
      | prerequisite >= regularRetirementSuppressionResolveIndex suppression ->
          RegularDefinitionWriteAdmissibleAfterResolve suppression
      | positionCoveredBy
          position
          (regularRetirementSuppressionLocalPublicationCut suppression) ->
          RegularDefinitionWriteCoveredByCut suppression
      | otherwise ->
          RegularDefinitionWriteMissingResolvePrerequisite suppression

positionCoveredBy ::
  Maybe HeraldPublicationPosition -> HeraldPublicationPrefix -> Bool
positionCoveredBy Nothing _ = False
positionCoveredBy (Just _) EmptyHeraldPublicationPrefix = False
positionCoveredBy
  (Just position)
  (HeraldPublicationPrefixThrough through) = position <= through

-- | Every authority-qualified terminal Store suppression retained by this
-- Herald, including coordinates for sorts which currently have no Store.
storeTerminalObjectPurgeEntries ::
  State -> [((SortId, ObjectKey), StructuralConsequenceCause)]
storeTerminalObjectPurgeEntries state = Map.toAscList state.terminalObjectPurges

-- | Audit revision continuity, replayability, exact source evidence, receipt
-- coherence, and active/retained/closed ownership for the whole Store state.
validateStoreHistoryState ::
  State -> Either StoreHistoryInvariantProblem ()
validateStoreHistoryState state = do
  mapM_ validateRetainedSlot (Map.toAscList state.retainedStores)
  mapM_ validateActiveSlot (Map.toAscList state.stores)
  mapM_ validateClosedIncarnation (Set.toAscList state.closedStores)
  where
    validateRetainedSlot (incarnation, slot) = do
      unless
        (incarnation == slot.incarnation)
        ( Left
            ( StoreHistoryRetainedIncarnationKeyMismatch
                incarnation
                slot.incarnation
            )
        )
      unless
        ( slot.terminalObjectPurges
            == terminalObjectPurgesForSort slot.sortId state
        )
        ( Left
            ( StoreHistoryTerminalPurgeProjectionMismatch
                slot.sortId
                incarnation
            )
        )
      let revisions = revisionsThrough slot.revision
          changeRevisions = drop 1 revisions
          observedSnapshots = Map.keys slot.retainedSnapshots
          observedChanges = Map.keys slot.retainedHistory
      unless
        (observedSnapshots == revisions)
        ( Left
            ( StoreHistorySnapshotRevisionSetMismatch
                incarnation
                revisions
                observedSnapshots
            )
        )
      unless
        (observedChanges == changeRevisions)
        ( Left
            ( StoreHistoryChangeRevisionSetMismatch
                incarnation
                changeRevisions
                observedChanges
            )
        )
      mapM_
        (validateSnapshotSort incarnation slot.sortId)
        (Map.toAscList slot.retainedSnapshots)
      let observations = slot.applicationObservations
          retainedChangeObservations =
            fmap
              retainedStoreChangeObservation
              (Map.elems slot.retainedHistory)
          revisionAddressableObservations =
            slot.baseObservations <> retainedChangeObservations
      unless
        ( all (`elem` observations) revisionAddressableObservations
            && all (`elem` revisionAddressableObservations) observations
        )
        ( Left
            ( StoreHistoryApplicationObservationCoverageMismatch
                incarnation
            )
        )
      mapM_ (validateObservation slot) observations
      mapM_ (validateReceiptEvidence slot) observations
      replayedTranscript <- replaySlot slot
      unless
        ( retainedSnapshot replayedTranscript
            == retainedSnapshot slot.transcriptContents
        )
        ( Left
            ( StoreHistoryTranscriptSnapshotMismatch
                incarnation
                slot.revision
            )
        )
      let replayed =
            projectEffectiveStoreContents
              state.regularRetirementSuppressions
              slot.terminalObjectPurges
              slot
      unless
        (retainedSnapshot replayed == retainedSnapshot slot.contents)
        ( Left
            ( StoreHistoryCurrentSnapshotMismatch
                incarnation
                slot.revision
            )
        )

    validateActiveSlot (delta, slot) = do
      unless
        (delta == slot.delta)
        (Left (StoreHistoryActiveDeltaKeyMismatch delta slot.delta))
      unless
        (Map.lookup slot.incarnation state.retainedStores == Just slot)
        ( Left
            ( StoreHistoryActiveRetainedMismatch
                delta
                slot.incarnation
            )
        )
      if Set.member slot.incarnation state.closedStores
        then
          Left
            ( StoreHistoryActiveIncarnationClosed
                delta
                slot.incarnation
            )
        else Right ()

    validateClosedIncarnation incarnation =
      unless
        (Map.member incarnation state.retainedStores)
        (Left (StoreHistoryClosedIncarnationMissing incarnation))

validateSnapshotSort ::
  StoreIncarnationId ->
  SortId ->
  (StoreRevision, RetainedSnapshot) ->
  Either StoreHistoryInvariantProblem ()
validateSnapshotSort incarnation expected (revision, snapshot) =
  unless
    (retainedSnapshotSort snapshot == expected)
    ( Left
        ( StoreHistorySnapshotSortMismatch
            incarnation
            revision
            expected
            (retainedSnapshotSort snapshot)
        )
    )

validateObservation ::
  StoreSlot ->
  RetainedStoreObservation ->
  Either StoreHistoryInvariantProblem ()
validateObservation slot observation = do
  let publication = retainedStoreObservationPublication observation
      identifier = checkedPublicationId publication
      actualSort = checkedPublicationSort publication
      actualOccurrence = retainedStoreObservationSortOccurrence observation
  unless
    (actualSort == slot.sortId)
    ( Left
        ( StoreHistoryObservationSortMismatch
            slot.incarnation
            identifier
            slot.sortId
            actualSort
        )
    )
  unless
    (actualOccurrence == slot.occurrenceId)
    ( Left
        ( StoreHistoryObservationOccurrenceMismatch
            slot.incarnation
            identifier
            slot.occurrenceId
            actualOccurrence
        )
    )
  case retainedStoreObservationOrigin observation of
    PrimordialStoreObservation -> Right ()
    RoutedStoreObservation envelope -> do
      let envelopeOccurrence = storeObservationEnvelopeSortOccurrence envelope
      unless
        (envelopeOccurrence == actualOccurrence)
        ( Left
            ( StoreHistoryEnvelopeOccurrenceMismatch
                slot.incarnation
                identifier
                actualOccurrence
                envelopeOccurrence
            )
        )
      case storeObservationEnvelopeStructuralStamp envelope of
        Nothing -> Right ()
        Just stamp ->
          unless
            (structuralOccurrenceStampPublication stamp == identifier)
            ( Left
                ( StoreHistoryStructuralStampPublicationMismatch
                    slot.incarnation
                    identifier
                    (structuralOccurrenceStampPublication stamp)
                )
            )

validateReceiptEvidence ::
  StoreSlot ->
  RetainedStoreObservation ->
  Either StoreHistoryInvariantProblem ()
validateReceiptEvidence slot observation =
  let identifier =
        checkedPublicationId
          (retainedStoreObservationPublication observation)
      strength = retainedStoreObservationIncomingStrength observation
      receipt = Map.lookup identifier slot.appliedPublications
   in unless
        (maybe False (>= strength) receipt)
        ( Left
            ( StoreHistoryReceiptMissingOrWeaker
                slot.incarnation
                identifier
                strength
                receipt
            )
        )

replaySlot ::
  StoreSlot -> Either StoreHistoryInvariantProblem DeltaStore
replaySlot slot = do
  base <-
    foldM
      (replayBaseObservation slot)
      (emptyDeltaStore slot.sortId)
      slot.baseObservations
  let baseSnapshot = retainedSnapshot base
  unless
    ( Map.lookup initialStoreRevision slot.retainedSnapshots
        == Just baseSnapshot
    )
    ( Left
        ( StoreHistorySnapshotReplayMismatch
            slot.incarnation
            initialStoreRevision
        )
    )
  foldM (replayChange slot) base (Map.toAscList slot.retainedHistory)

applyTerminalObjectPurges ::
  Map ObjectKey StructuralConsequenceCause -> DeltaStore -> DeltaStore
applyTerminalObjectPurges purges store =
  Map.foldlWithKey'
    (\current key _ -> fst (purgeObjectKey key current))
    store
    purges

-- | Derive the effective Store view from the immutable accepted-observation
-- ledger. Terminal object purges are key-qualified; regular-definition
-- suppression is instead subject- and epoch-qualified. It hides both the equal
-- descriptor write and Nabla/Delta carrier values which still refer to the
-- retired public sort, while permitting either kind of observation once its
-- retained source prerequisite reaches Resolve.
projectEffectiveStoreContents ::
  Map DisappearanceSubject RegularRetirementSuppression ->
  Map ObjectKey StructuralConsequenceCause ->
  StoreSlot ->
  DeltaStore
projectEffectiveStoreContents suppressions terminalPurges slot =
  preserveLocalVisibility
    slot.contents
    ( projectEffectiveObservations
        suppressions
        terminalPurges
        slot.sortId
        slot.applicationObservations
    )

-- | Reprojection reconstructs hidden retained state from immutable observation
-- evidence, but a local take deliberately changes only this Herald's visible
-- projection.  Preserve precisely those keys which are retained yet locally
-- hidden in the predecessor; a later publication which legitimately re-exposes
-- a key is already visible there and is therefore not masked.
preserveLocalVisibility :: DeltaStore -> DeltaStore -> DeltaStore
preserveLocalVisibility predecessor rebuilt =
  foldl'
    (\current key -> snd (localTake key current))
    rebuilt
    (Set.toAscList locallyHidden)
  where
    locallyHidden =
      retainedKeys predecessor `Set.difference` visibleKeys predecessor
    retainedKeys = Set.fromList . fmap fst . retainedInstances
    visibleKeys = Set.fromList . fmap fst . visibleInstances

projectEffectiveObservations ::
  Map DisappearanceSubject RegularRetirementSuppression ->
  Map ObjectKey StructuralConsequenceCause ->
  SortId ->
  [RetainedStoreObservation] ->
  DeltaStore
projectEffectiveObservations suppressions terminalPurges sortId observations =
  applyTerminalObjectPurges
    terminalPurges
    ( foldl'
        applyEffectiveObservation
        (emptyDeltaStore sortId)
        observations
    )
  where
    applyEffectiveObservation current observation
      | observationSuppressedByRegularRetirement suppressions observation = current
      | otherwise =
          either
            (error . ("invalid retained Store observation: " <>) . show)
            id
            ( applyPublication
                (retainedStoreObservationIncomingStrength observation)
                (retainedStoreObservationPublication observation)
                current
            )

effectiveStoreFactRemovalCount :: DeltaStore -> DeltaStore -> Word64
effectiveStoreFactRemovalCount predecessor successor =
  fromIntegral
    ( removedKeyedCount
        fst
        (visibleInstances predecessor)
        (visibleInstances successor)
        + removedKeyedCount
          fst
          (retainedInstances predecessor)
          (retainedInstances successor)
        + removedKeyedCount
          (checkedPublicationLogicalState . fst)
          (retainedSnapshotLogicalStrengthFacts (retainedSnapshot predecessor))
          (retainedSnapshotLogicalStrengthFacts (retainedSnapshot successor))
    )
  where
    -- Count exact predecessor facts which disappear, rather than the net
    -- cardinality change. Suppressing one winner may reveal a fallback at the
    -- same key; that still removed the old fact and therefore contributes one
    -- unit of retirement work.
    removedKeyedCount keyOf before after =
      length
        [ ()
        | fact <- before,
          Map.lookup (keyOf fact) afterByKey /= Just fact
        ]
      where
        afterByKey = Map.fromList [(keyOf fact, fact) | fact <- after]

observationSuppressedByRegularRetirement ::
  Map DisappearanceSubject RegularRetirementSuppression ->
  RetainedStoreObservation ->
  Bool
observationSuppressedByRegularRetirement suppressions observation =
  any suppresses (Map.elems suppressions)
  where
    publication = retainedStoreObservationPublication observation
    -- An ordinary value names its definition occurrence explicitly, so an
    -- exact retired occurrence is terminal at every later control prefix.
    -- Definition writes and Nabla/Delta references have a stable outer
    -- carrier occurrence; for those, the Resolve prerequisite distinguishes
    -- stale predecessor evidence from successor-epoch work.
    suppresses suppression =
      checkedPublicationIsRegularRetirementResidual
        (retainedStoreObservationSortOccurrence observation)
        (regularRetirementSuppressionSubject suppression)
        (regularRetirementSuppressionResolveIndex suppression)
        observationControlPrerequisite
        publication
    observationControlPrerequisite =
      case retainedStoreObservationOrigin observation of
        PrimordialStoreObservation -> Nothing
        RoutedStoreObservation envelope ->
          Just (storeObservationEnvelopeControlPrerequisite envelope)

replayBaseObservation ::
  StoreSlot ->
  DeltaStore ->
  RetainedStoreObservation ->
  Either StoreHistoryInvariantProblem DeltaStore
replayBaseObservation slot store observation = do
  (successor, _) <-
    either
      (Left . StoreHistoryReplayRejected slot.incarnation initialStoreRevision)
      Right
      ( applyPublicationDetailed
          (retainedStoreObservationIncomingStrength observation)
          (retainedStoreObservationPublication observation)
          store
      )
  Right successor

replayChange ::
  StoreSlot ->
  DeltaStore ->
  (StoreRevision, RetainedStoreChange) ->
  Either StoreHistoryInvariantProblem DeltaStore
replayChange slot predecessor (revision, change) = do
  unless
    (retainedStoreChangeRevision change == revision)
    ( Left
        ( StoreHistoryChangeKeyMismatch
            slot.incarnation
            revision
            (retainedStoreChangeRevision change)
        )
    )
  let observation = retainedStoreChangeObservation change
      expectedTransition = retainedStoreChangeTransition change
  case expectedTransition of
    Nothing -> Right ()
    Just transition ->
      unless
        ( retainedTransitionPublication transition
            == retainedStoreObservationPublication observation
            && retainedTransitionIncomingStrength transition
              == retainedStoreObservationIncomingStrength observation
        )
        (Left (StoreHistoryTransitionObservationMismatch slot.incarnation revision))
  (successor, result) <-
    either
      (Left . StoreHistoryReplayRejected slot.incarnation revision)
      Right
      ( applyPublicationDetailed
          (retainedStoreObservationIncomingStrength observation)
          (retainedStoreObservationPublication observation)
          predecessor
      )
  let observedTransition = case result of
        NoRetainedChange -> Nothing
        RetainedChanged transition -> Just transition
  unless
    (observedTransition == expectedTransition)
    (Left (StoreHistoryTransitionMismatch slot.incarnation revision))
  unless
    ( Map.lookup revision slot.retainedSnapshots
        == Just (retainedSnapshot successor)
    )
    (Left (StoreHistorySnapshotReplayMismatch slot.incarnation revision))
  Right successor

revisionsThrough :: StoreRevision -> [StoreRevision]
revisionsThrough final = go initialStoreRevision
  where
    go revision
      | revision == final = [revision]
      | otherwise = revision : go (nextStoreRevision revision)

-- | Construct the real store owner from the opaque checked genesis source.
initialState :: CheckedHeraldGenesis -> Either StorePreparationError State
initialState genesis =
  commitStoreBootstrap
    <$> prepareStoreBootstrap (systemViewStoreSpecifications genesis) seedState
  where
    seedState =
      State
        ( fmap
            primordialReplicaPublication
            (checkedPrimordialReplicas genesis)
        )
        Map.empty
        Map.empty
        Set.empty
        Set.empty
        False
        Set.empty
        Set.empty
        Map.empty
        Map.empty

primordialSeedPublications :: State -> [CheckedPublication]
primordialSeedPublications state = state.primordialPublications

lookupStoreSlot :: DeltaId -> State -> Maybe StoreSlot
lookupStoreSlot delta state = Map.lookup delta state.stores

lookupRetainedStoreSlot :: StoreIncarnationId -> State -> Maybe StoreSlot
lookupRetainedStoreSlot incarnation state =
  Map.lookup incarnation state.retainedStores

changedSourceStoreIncarnations :: State -> Set StoreIncarnationId
changedSourceStoreIncarnations state = state.sourceStoreChanges

clearSourceStoreChanges :: State -> State
clearSourceStoreChanges state
  | Set.null state.sourceStoreChanges = state
  | otherwise = state {sourceStoreChanges = Set.empty}

-- | Content writes do not change this predicate. Only active slot admission,
-- passivation or replacement notifies preparation readers.
takePreparationSlotChanges :: State -> (Set DeltaId, State)
takePreparationSlotChanges state =
  let changes = state.preparationSlotChanges
   in changes `seq` (changes, clearPreparationSlotChanges state)

clearPreparationSlotChanges :: State -> State
clearPreparationSlotChanges state
  | Set.null state.preparationSlotChanges = state
  | otherwise = state {preparationSlotChanges = Set.empty}

notifyPreparationSlot :: DeltaId -> State -> State
notifyPreparationSlot delta state = state {preparationSlotChanges = Set.insert delta state.preparationSlotChanges, alignmentLossChanged = True}

-- | Independent consumers: active-slot changes can invalidate a whole loss
-- batch, while retained-prefix changes affect only indexed member evidence.
takeAlignmentLossChange :: State -> (Bool, State)
takeAlignmentLossChange state =
  let changed = state.alignmentLossChanged
   in changed `seq` (changed, clearAlignmentLossChange state)

clearAlignmentLossChange :: State -> State
clearAlignmentLossChange state = state {alignmentLossChanged = False}

takeAlignmentMemberStoreChanges :: State -> (Set StoreIncarnationId, State)
takeAlignmentMemberStoreChanges state =
  let changed = state.alignmentMemberStoreChanges
   in changed `seq` (changed, clearAlignmentMemberStoreChanges state)

clearAlignmentMemberStoreChanges :: State -> State
clearAlignmentMemberStoreChanges state = state {alignmentMemberStoreChanges = Set.empty}

storeSlots :: State -> [StoreSlot]
storeSlots state = Map.elems state.stores

-- | Every immutable application-store resource admitted during this finite
-- run, including resources whose delta is currently passive.  Query and route
-- application use only 'storeSlots'; this history prevents an old incarnation
-- from being treated as fresh after passivation.
retainedStoreSlots :: State -> [StoreSlot]
retainedStoreSlots state = Map.elems state.retainedStores

-- Every new incarnation inherits terminal object suppression from the Store
-- owner's incarnation-independent ledger. This also covers application-
-- defined controlled sorts which had no Store when deletion was applied.
terminalObjectPurgesForSort ::
  SortId -> State -> Map ObjectKey StructuralConsequenceCause
terminalObjectPurgesForSort sortId state =
  terminalObjectPurgesForSortIn sortId state.terminalObjectPurges

terminalObjectPurgesForSortIn ::
  SortId ->
  Map (SortId, ObjectKey) StructuralConsequenceCause ->
  Map ObjectKey StructuralConsequenceCause
terminalObjectPurgesForSortIn sortId purges =
  Map.fromList
    [ (objectKey, cause)
    | ((retainedSort, objectKey), cause) <- Map.toAscList purges,
      retainedSort == sortId
    ]

-- | Exercise the same close primitive as a structural Store patch without
-- fabricating a 'Reconciliation.StorePatch'. This is package-internal support
-- for retained-history and whole-state invariant properties only.
passivateStoreForInvariantTest :: DeltaId -> State -> State
passivateStoreForInvariantTest = passivateActiveStore

-- | Exercise the closed-incarnation gate independently of structural patch
-- construction. Production activation reaches the same gate through
-- 'prepareStructuralStorePatch'.
reactivateRetainedStoreForInvariantTest ::
  DeltaId ->
  StoreIncarnationId ->
  State ->
  Either StructuralStorePatchError State
reactivateRetainedStoreForInvariantTest delta incarnation state = do
  requireOpenIncarnation delta incarnation state
  retained <-
    maybe
      (Left (StructuralStoreActiveResourceMissing delta incarnation))
      Right
      (Map.lookup incarnation state.retainedStores)
  if storeSlotDelta retained == delta
    then
      let notified = if fmap storeSlotIncarnation (lookupStoreSlot delta state) == Just incarnation then state else notifyPreparationSlot delta state
       in Right notified {stores = Map.insert delta retained state.stores}
    else Left (StructuralStoreActiveResourceMismatch delta incarnation)

-- | Coherently omit one authority-qualified terminal suppression from the
-- Store owner for a whole-state invariant test. Production transitions cannot
-- remove ledger entries; rebuilding each slot's live projection here keeps the
-- Store history internally valid so the cross-owner authenticity check is the
-- sole contradiction under test.
removeTerminalObjectPurgeForInvariantTest ::
  SortId ->
  ObjectKey ->
  State ->
  State
removeTerminalObjectPurgeForInvariantTest sortId objectKey state =
  state
    { stores = refreshSlot <$> state.stores,
      retainedStores = refreshSlot <$> state.retainedStores,
      terminalObjectPurges = retainedPurges
    }
  where
    retainedPurges = Map.delete (sortId, objectKey) state.terminalObjectPurges
    refreshSlot slot =
      let projectedPurges =
            terminalObjectPurgesForSortIn slot.sortId retainedPurges
       in slot
            { contents =
                projectEffectiveStoreContents
                  state.regularRetirementSuppressions
                  projectedPurges
                  slot,
              terminalObjectPurges = projectedPurges
            }

-- | Capture the complete hidden semantic state at one exact historical
-- revision. This remains available after the incarnation becomes inactive.
retainedStoreSnapshotAt ::
  StoreIncarnationId ->
  StoreRevision ->
  State ->
  Either StoreHistoryQueryError RetainedStoreSnapshot
retainedStoreSnapshotAt incarnation revision state = do
  slot <- requireRetainedStore incarnation state
  snapshot <- requireHistoricalSnapshot incarnation revision slot
  let observations = observationsThrough revision slot
  facts <-
    traverse
      (snapshotFact incarnation revision observations)
      (retainedSnapshotLogicalStrengthFacts snapshot)
  Right
    ( RetainedStoreSnapshot
        incarnation
        revision
        slot.sortId
        slot.occurrenceId
        facts
    )

-- | Capture the hidden state which alignment may still establish at one exact
-- historical revision. Unlike 'retainedStoreSnapshotAt', this query projects
-- the immutable observation prefix through every currently retained regular
-- retirement suppression. The raw historical snapshots remain untouched for
-- audit, while a lower-provenance definition admitted in a successor epoch can
-- be selected as the effective snapshot representative.
retainedStoreAlignmentSnapshotAt ::
  StoreIncarnationId ->
  StoreRevision ->
  State ->
  Either StoreHistoryQueryError RetainedStoreSnapshot
retainedStoreAlignmentSnapshotAt incarnation revision state = do
  slot <- requireRetainedStore incarnation state
  _ <- requireHistoricalSnapshot incarnation revision slot
  let observations =
        filter
          (alignmentObservationEligible state slot)
          (observationsThrough revision slot)
      snapshot =
        retainedSnapshot
          ( projectEffectiveObservations
              state.regularRetirementSuppressions
              slot.terminalObjectPurges
              slot.sortId
              observations
          )
  facts <-
    traverse
      (snapshotFact incarnation revision observations)
      (retainedSnapshotLogicalStrengthFacts snapshot)
  Right
    ( RetainedStoreSnapshot
        incarnation
        revision
        slot.sortId
        slot.occurrenceId
        facts
    )

-- | Whether the exact current destination suppresses this observation because
-- of an object purge or regular-definition retirement. A missing/replaced Store
-- or a different sort/definition is not suppression evidence. This read shares
-- the alignment eligibility rule and never admits a publication or changes a
-- Store receipt, revision, or effective value.
currentStoreObservationSuppressed ::
  DeltaId ->
  StoreIncarnationId ->
  RetainedStoreObservation ->
  State ->
  Bool
currentStoreObservationSuppressed delta incarnation observation state =
  case lookupStoreSlot delta state of
    Just slot
      | slot.incarnation == incarnation,
        Right () <- requireObservationOccurrence delta observation slot,
        Right () <- requireObservationSort delta (retainedStoreObservationPublication observation) slot ->
          not (alignmentObservationEligible state slot observation)
    _ -> False

alignmentObservationEligible :: State -> StoreSlot -> RetainedStoreObservation -> Bool
alignmentObservationEligible state slot observation =
  Map.notMember
    (checkedPublicationKey (retainedStoreObservationPublication observation))
    slot.terminalObjectPurges
    && not
      ( observationSuppressedByRegularRetirement
          state.regularRetirementSuppressions
          observation
      )

-- | Query every retained transition strictly after an exact historical
-- revision, in contiguous revision order, through the current prefix.
retainedStoreChangesAfter ::
  StoreIncarnationId ->
  StoreRevision ->
  State ->
  Either StoreHistoryQueryError [RetainedStoreChange]
retainedStoreChangesAfter incarnation revision state = do
  slot <- requireRetainedStore incarnation state
  _ <- requireHistoricalSnapshot incarnation revision slot
  let (_, later) = Map.split revision slot.retainedHistory
  Right (Map.elems later)

requireRetainedStore ::
  StoreIncarnationId ->
  State ->
  Either StoreHistoryQueryError StoreSlot
requireRetainedStore incarnation state =
  maybe
    (Left (StoreHistoryIncarnationMissing incarnation))
    Right
    (lookupRetainedStoreSlot incarnation state)

requireHistoricalSnapshot ::
  StoreIncarnationId ->
  StoreRevision ->
  StoreSlot ->
  Either StoreHistoryQueryError RetainedSnapshot
requireHistoricalSnapshot incarnation requested slot =
  maybe
    ( Left
        ( StoreHistoryRevisionUnavailable
            incarnation
            requested
            slot.revision
        )
    )
    Right
    (Map.lookup requested slot.retainedSnapshots)

observationsThrough ::
  StoreRevision -> StoreSlot -> [RetainedStoreObservation]
observationsThrough revision slot =
  slot.baseObservations
    <> fmap
      retainedStoreChangeObservation
      ( Map.elems
          (Map.filterWithKey (\candidate _ -> candidate <= revision) slot.retainedHistory)
      )

snapshotFact ::
  StoreIncarnationId ->
  StoreRevision ->
  [RetainedStoreObservation] ->
  (CheckedPublication, ReplicaStrength) ->
  Either StoreHistoryQueryError RetainedStoreSnapshotFact
snapshotFact incarnation revision observations (representative, strength) = do
  representativeEvidence <-
    maybe
      ( Left
          ( StoreHistoryRepresentativeEvidenceMissing
              incarnation
              revision
              (checkedPublicationId representative)
          )
      )
      Right
      ( find
          ((== representative) . retainedStoreObservationPublication)
          observations
      )
  strengthEvidence <-
    maybe
      ( Left
          ( StoreHistoryStrengthEvidenceMissing
              incarnation
              revision
              (checkedPublicationId representative)
              strength
          )
      )
      Right
      ( find
          ( isStrengthWitness
              (checkedPublicationLogicalState representative)
              strength
          )
          observations
      )
  Right
    ( RetainedStoreSnapshotFact
        representativeEvidence
        strengthEvidence
        strength
    )
  where
    isStrengthWitness logicalState expectedStrength observation =
      checkedPublicationLogicalState
        (retainedStoreObservationPublication observation)
        == logicalState
        && retainedStoreObservationIncomingStrength observation == expectedStrength

-- | Remove one immutable receipt while retaining the applied contents for a
-- whole-state invariant test. Production transitions cannot construct this
-- contradictory owner state.
removeStorePublicationReceiptForInvariantTest ::
  DeltaId ->
  PublicationId ->
  State ->
  State
removeStorePublicationReceiptForInvariantTest delta identifier state =
  state
    { stores =
        Map.adjust
          ( \slot ->
              slot
                { appliedPublications =
                    Map.delete identifier slot.appliedPublications
                }
          )
          delta
          state.stores
    }

-- | Remove only the current effective fact for one receipted publication while
-- retaining its immutable observation/history evidence. Production
-- transitions cannot construct this contradiction; whole-state properties use
-- it to prove that retirement suppression is checked per observation rather
-- than inferred from the existence of any one stale delivery.
removeStoreEffectivePublicationForInvariantTest ::
  DeltaId ->
  PublicationId ->
  State ->
  State
removeStoreEffectivePublicationForInvariantTest delta identifier state =
  case Map.lookup delta state.stores of
    Nothing -> state
    Just slot ->
      case find matchingPublication slot.applicationObservations of
        Nothing -> state
        Just observation ->
          let (contents, _) =
                purgeObjectKey
                  (checkedPublicationKey (retainedStoreObservationPublication observation))
                  slot.contents
           in replaceStoreSlot (slot {contents}) state
  where
    matchingPublication observation =
      checkedPublicationId (retainedStoreObservationPublication observation)
        == identifier

-- | Retain an otherwise checked observation without applying it to the raw or
-- effective Store transcript. This deliberately corrupts only the cross-owner
-- identity relation: properties use it to prove that a receipt authenticates
-- the complete checked publication, not merely its PublicationId.
insertStoreObservationForInvariantTest ::
  DeltaId ->
  RetainedStoreObservation ->
  State ->
  State
insertStoreObservationForInvariantTest delta observation state =
  case Map.lookup delta state.stores of
    Nothing -> state
    Just slot ->
      replaceStoreSlot
        ( slot
            { applicationObservations =
                appendUniqueObservation observation slot.applicationObservations
            }
        )
        state

-- | Checked local-store contradictions. The use-case coordinator decides which
-- missing facts are ordinary pre-admission failures and which are invariant
-- faults after a resolved query or frozen route exists.
data StoreTransitionError
  = StoreTransitionDeltaMissing DeltaId
  | StoreTransitionIncarnationMismatch
      DeltaId
      StoreIncarnationId
      StoreIncarnationId
  | StoreTransitionExactTakeMismatch DeltaId ObjectKey PublicationId
  | StoreTransitionPublicationRejected DeltaId StoreError
  | StoreTransitionSortOccurrenceMismatch
      DeltaId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | StoreTransitionObservationProblem StoreObservationProblem
  | StoreTransitionAlignmentStrengthMismatch
      DeltaId
      ReplicaStrength
      ReplicaStrength
  | StoreTransitionEnvironmentOwnerMismatch DeltaId ProcessEpochId
  deriving stock (Eq, Show)

newtype PreparedExactLocalTake
  = PreparedExactLocalTake (Prepared State [CheckedPublication])

-- | One exact visible raw publication selected by a checked observation cut.
-- The incarnation and publication binding prevent a later preparation from
-- hiding a replacement that happens to reuse the same object key.
data ExactLocalTakeKey
  = ExactLocalTakeKey
      DeltaId
      StoreIncarnationId
      ObjectKey
      CheckedPublication

exactLocalTakeKey ::
  DeltaId ->
  StoreIncarnationId ->
  ObjectKey ->
  CheckedPublication ->
  ExactLocalTakeKey
exactLocalTakeKey = ExactLocalTakeKey

-- | Hide only the exact raw candidates selected by an effective observation
-- plan. Predicate evaluation and visibility projection have already happened
-- above the Store owner; this preparation merely rechecks the immutable raw
-- identities against the same owner cut before mutating visible state.
prepareExactLocalTake ::
  [ExactLocalTakeKey] ->
  State ->
  Either StoreTransitionError PreparedExactLocalTake
prepareExactLocalTake keys state =
  PreparedExactLocalTake <$> prepareTransition takeExact state
  where
    takeExact predecessor = foldM takeOne (predecessor, []) keys
    takeOne (current, matches) (ExactLocalTakeKey delta incarnation objectKey expected) = do
      slot <- requireStore delta current
      requireIncarnation delta incarnation slot
      stored <-
        maybe
          (exactMismatch delta objectKey expected)
          Right
          (lookupVisible objectKey (storeSlotContents slot))
      let actual = storedPublication stored
      unless
        (actual == expected)
        (exactMismatch delta objectKey expected)
      let (_, contents) = localTake objectKey (storeSlotContents slot)
          (_, transcriptContents) = localTake objectKey slot.transcriptContents
          successor =
            replaceStoreSlotContents
              contents
              transcriptContents
              slot
              current
      Right (successor, matches <> [actual])

    exactMismatch delta objectKey expected =
      Left
        ( StoreTransitionExactTakeMismatch
            delta
            objectKey
            (checkedPublicationId expected)
        )

commitExactLocalTake ::
  PreparedExactLocalTake ->
  (State, [CheckedPublication])
commitExactLocalTake (PreparedExactLocalTake prepared) = commitPrepared prepared

newtype PreparedStoreApplication
  = PreparedStoreApplication (Prepared State ())

-- | Apply one checked publication to every exact destination in its frozen cut
-- and retain or join its monotone receipt in the same owner successor.
prepareStoreApplication ::
  StoreObservationEnvelope ->
  FrozenRoute ->
  CheckedPublication ->
  State ->
  Either StoreTransitionError PreparedStoreApplication
prepareStoreApplication envelope route publication state =
  PreparedStoreApplication
    <$> prepareTransition applyEvery state
  where
    applyEvery predecessor = do
      successor <- foldM applyOne predecessor (routeDestinations route)
      Right (successor, ())
    applyOne current destination = do
      let delta = destinationDelta destination
      slot <- requireStore delta current
      requireIncarnation delta (destinationStoreIncarnation destination) slot
      observation <-
        checkedRoutedStoreObservation
          envelope
          (destinationStrength destination)
          publication
      fst
        <$> applyCurrentStoreObservation
          delta
          observation
          slot
          current

commitStoreApplication :: PreparedStoreApplication -> State
commitStoreApplication (PreparedStoreApplication prepared) =
  fst (commitPrepared prepared)

-- | Finish an already accepted local publication after its structural
-- prerequisites arrive. The frozen destination may have passivated meanwhile;
-- use the same exact-incarnation settlement as delayed peer delivery, without
-- retargeting a replacement Store. Immediate local calls retain the stricter
-- same-transition route check in 'prepareStoreApplication'.
prepareRetainedStoreApplication ::
  StoreObservationEnvelope ->
  FrozenRoute ->
  CheckedPublication ->
  State ->
  Either StoreTransitionError PreparedStoreApplication
prepareRetainedStoreApplication envelope route publication state =
  PreparedStoreApplication <$> prepareTransition applyRetained state
  where
    destinations =
      [ peerStoreDestination
          (destinationDelta destination)
          (destinationStoreIncarnation destination)
          (destinationStrength destination)
      | destination <- routeDestinations route
      ]
    applyRetained predecessor = case destinations of
      [] -> Right (predecessor, ())
      first : rest -> do
        prepared <- preparePeerStoreApplication envelope (first :| rest) publication predecessor
        let (successor, _) = commitPeerStoreApplication prepared
        Right (successor, ())

newtype PreparedEnvironmentStoreSeed
  = PreparedEnvironmentStoreSeed (Prepared State ())

-- | Install a checked environment's own descriptions only into its exact
-- owner-local stores. The caller admits the closed manifest and its publication
-- provenance; this owner preserves ordinary monotone receipts, retained history,
-- source revisions and observation notifications. It never seeds private system
-- views or makes these publications defaults for a future unrelated reader.
prepareEnvironmentStoreSeed ::
  ProcessEpochId ->
  [(DeltaId, StoreIncarnationId, [RetainedStoreObservation])] ->
  State ->
  Either StoreTransitionError PreparedEnvironmentStoreSeed
prepareEnvironmentStoreSeed owner supplied state =
  PreparedEnvironmentStoreSeed <$> prepareTransition seed state
  where
    seed predecessor = (,()) <$> foldM seedStore predecessor supplied
    seedStore current (delta, incarnation, observations) = do
      slot <- requireStore delta current
      requireIncarnation delta incarnation slot
      unless (storeOwner slot == Just owner) (Left (StoreTransitionEnvironmentOwnerMismatch delta owner))
      foldM (applyOne delta) current observations
    applyOne delta current observation = do
      slot <- requireStore delta current
      fst <$> applyCurrentStoreObservation delta observation slot current
    storeOwner slot = case storeSlotProvenance slot of
      ApplicationReader process _ -> Just process
      StructuralBaselineReader _ process _ _ -> Just process
      StructuralApplicationReader _ process _ _ -> Just process
      StructuralControlReader _ process _ _ -> Just process
      HeraldSystemView {} -> Nothing

commitEnvironmentStoreSeed :: PreparedEnvironmentStoreSeed -> State
commitEnvironmentStoreSeed (PreparedEnvironmentStoreSeed prepared) = fst (commitPrepared prepared)

-- | One exact destination named by an incoming peer publication batch.
-- The containing stream direction already fixes the hosting Herald, so this
-- owner needs only the local Delta incarnation and routed strength.
data PeerStoreDestination = PeerStoreDestination
  { delta :: DeltaId,
    incarnation :: StoreIncarnationId,
    strength :: ReplicaStrength
  }
  deriving stock (Eq, Ord, Show)

peerStoreDestination ::
  DeltaId ->
  StoreIncarnationId ->
  ReplicaStrength ->
  PeerStoreDestination
peerStoreDestination = PeerStoreDestination

peerStoreDestinationDelta :: PeerStoreDestination -> DeltaId
peerStoreDestinationDelta destination = destination.delta

peerStoreDestinationIncarnation ::
  PeerStoreDestination ->
  StoreIncarnationId
peerStoreDestinationIncarnation destination = destination.incarnation

peerStoreDestinationStrength :: PeerStoreDestination -> ReplicaStrength
peerStoreDestinationStrength destination = destination.strength

-- | Terminal meaning of one exact named peer destination.
data PeerStoreDisposition
  = PeerStoreApplied
  | PeerStoreAlreadyApplied
  | PeerStoreTerminallyIgnored
  deriving stock (Eq, Ord, Show)

data PeerStoreOutcome = PeerStoreOutcome
  { destination :: PeerStoreDestination,
    disposition :: PeerStoreDisposition
  }
  deriving stock (Eq, Show)

peerStoreOutcomeDestination :: PeerStoreOutcome -> PeerStoreDestination
peerStoreOutcomeDestination outcome = outcome.destination

peerStoreOutcomeDisposition :: PeerStoreOutcome -> PeerStoreDisposition
peerStoreOutcomeDisposition outcome = outcome.disposition

newtype PreparedPeerStoreApplication
  = PreparedPeerStoreApplication
      (Prepared State (NonEmpty PeerStoreOutcome))

-- | Apply one incoming publication only to its exact current local
-- incarnations. Missing or stale incarnations are terminal outcomes rather than
-- retargeting opportunities; an equal already-applied publication is
-- idempotent, while a stronger receipt joins without contradiction. All current
-- members of a mixed batch install atomically.
preparePeerStoreApplication ::
  StoreObservationEnvelope ->
  NonEmpty PeerStoreDestination ->
  CheckedPublication ->
  State ->
  Either StoreTransitionError PreparedPeerStoreApplication
preparePeerStoreApplication envelope destinations publication state =
  preparePeerStoreObservations
    destinations
    ( \destination ->
        checkedRoutedStoreObservation
          envelope
          destination.strength
          publication
    )
    state

-- | Re-admit one already checked alignment observation to exact current local
-- incarnations. Source history may be queried after passivation, but this
-- transition deliberately consults only the active Delta index and therefore
-- cannot retarget or reactivate an inactive incarnation. Destination strength
-- must name the attenuation already recorded in the observation.
prepareAlignmentStoreApplication ::
  NonEmpty PeerStoreDestination ->
  RetainedStoreObservation ->
  State ->
  Either StoreTransitionError PreparedPeerStoreApplication
prepareAlignmentStoreApplication destinations observation state =
  preparePeerStoreObservations
    destinations
    observationFor
    state
  where
    observationFor destination
      | destination.strength
          == retainedStoreObservationIncomingStrength observation =
          Right observation
      | otherwise =
          Left
            ( StoreTransitionAlignmentStrengthMismatch
                destination.delta
                (retainedStoreObservationIncomingStrength observation)
                destination.strength
            )

preparePeerStoreObservations ::
  NonEmpty PeerStoreDestination ->
  (PeerStoreDestination -> Either StoreTransitionError RetainedStoreObservation) ->
  State ->
  Either StoreTransitionError PreparedPeerStoreApplication
preparePeerStoreObservations destinations observationFor state =
  PreparedPeerStoreApplication
    <$> prepareTransition applyEvery state
  where
    applyEvery predecessor = case destinations of
      first :| rest -> do
        (afterFirst, firstDisposition) <- applyPeerDestination first predecessor
        (successor, laterOutcomes) <-
          foldM applyOne (afterFirst, []) rest
        Right
          ( successor,
            PeerStoreOutcome first firstDisposition :| laterOutcomes
          )
    applyOne (current, outcomes) destination = do
      (successor, disposition) <- applyPeerDestination destination current
      Right
        ( successor,
          outcomes <> [PeerStoreOutcome destination disposition]
        )
    applyPeerDestination destination current =
      case lookupStoreSlot destination.delta current of
        Nothing -> Right (current, PeerStoreTerminallyIgnored)
        Just slot
          | storeSlotIncarnation slot /= destination.incarnation ->
              Right (current, PeerStoreTerminallyIgnored)
          | otherwise -> do
              observation <- observationFor destination
              applyCurrent destination observation slot current
    applyCurrent destination observation slot current =
      applyCurrentStoreObservation
        destination.delta
        observation
        slot
        current

checkedRoutedStoreObservation ::
  StoreObservationEnvelope ->
  ReplicaStrength ->
  CheckedPublication ->
  Either StoreTransitionError RetainedStoreObservation
checkedRoutedStoreObservation envelope strength publication =
  either
    (Left . StoreTransitionObservationProblem)
    Right
    ( retainedStoreObservation
        publication
        strength
        (storeObservationEnvelopeSortOccurrence envelope)
        (RoutedStoreObservation envelope)
    )

applyCurrentStoreObservation ::
  DeltaId ->
  RetainedStoreObservation ->
  StoreSlot ->
  State ->
  Either StoreTransitionError (State, PeerStoreDisposition)
applyCurrentStoreObservation delta observation slot state = do
  (successorSlot, disposition) <-
    applyStoreObservation
      state.regularRetirementSuppressions
      delta
      observation
      slot
  Right (replaceStoreSlot successorSlot state, disposition)

applyStoreObservation ::
  Map DisappearanceSubject RegularRetirementSuppression ->
  DeltaId ->
  RetainedStoreObservation ->
  StoreSlot ->
  Either StoreTransitionError (StoreSlot, PeerStoreDisposition)
applyStoreObservation suppressions delta observation slot = do
  let publication = retainedStoreObservationPublication observation
      incomingStrength = retainedStoreObservationIncomingStrength observation
      objectKey = checkedPublicationKey publication
  requireObservationOccurrence delta observation slot
  requireObservationSort delta publication slot
  if Map.member objectKey slot.terminalObjectPurges
    || observationSuppressedByRegularRetirement suppressions observation
    then Right (slot, PeerStoreTerminallyIgnored)
    else do
      (contents, retainedResult) <-
        either
          (Left . StoreTransitionPublicationRejected delta)
          Right
          ( applyPublicationDetailed
              incomingStrength
              publication
              slot.transcriptContents
          )
      (effectiveContents, effectiveResult) <-
        either
          (Left . StoreTransitionPublicationRejected delta)
          Right
          ( applyPublicationDetailed
              incomingStrength
              publication
              slot.contents
          )
      let identifier = checkedPublicationId publication
          priorReceipt = Map.lookup identifier slot.appliedPublications
          joinedReceipt = maybe incomingStrength (max incomingStrength) priorReceipt
          receiptChanged = priorReceipt /= Just joinedReceipt
          observationAdded = observation `notElem` slot.applicationObservations
          withReceipt =
            slot
              { transcriptContents = contents,
                contents = effectiveContents,
                appliedPublications =
                  Map.insert identifier joinedReceipt slot.appliedPublications,
                applicationObservations =
                  appendUniqueObservation observation slot.applicationObservations
              }
          successorSlot =
            if retainedStateChanged retainedResult
              || retainedStateChanged effectiveResult
              || observationAdded
              then
                appendRetainedChange
                  observation
                  (retainedTransitionMaybe retainedResult)
                  withReceipt
              else withReceipt
          disposition
            | retainedStateChanged retainedResult
                || retainedStateChanged effectiveResult =
                PeerStoreApplied
            | receiptChanged || observationAdded = PeerStoreApplied
            | otherwise = PeerStoreAlreadyApplied
      Right (successorSlot, disposition)

retainedStateChanged :: RetainedApplyResult -> Bool
retainedStateChanged NoRetainedChange = False
retainedStateChanged (RetainedChanged _) = True

retainedTransitionMaybe :: RetainedApplyResult -> Maybe RetainedTransition
retainedTransitionMaybe NoRetainedChange = Nothing
retainedTransitionMaybe (RetainedChanged transition) = Just transition

appendUniqueObservation ::
  RetainedStoreObservation ->
  [RetainedStoreObservation] ->
  [RetainedStoreObservation]
appendUniqueObservation observation retained
  | observation `elem` retained = retained
  | otherwise = retained <> [observation]

requireObservationSort ::
  DeltaId ->
  CheckedPublication ->
  StoreSlot ->
  Either StoreTransitionError ()
requireObservationSort delta publication slot
  | checkedPublicationSort publication == slot.sortId = Right ()
  | otherwise =
      Left
        ( StoreTransitionPublicationRejected
            delta
            (StoreSortMismatch slot.sortId (checkedPublicationSort publication))
        )

requireObservationOccurrence ::
  DeltaId ->
  RetainedStoreObservation ->
  StoreSlot ->
  Either StoreTransitionError ()
requireObservationOccurrence delta observation slot
  | retainedStoreObservationSortOccurrence observation == slot.occurrenceId = Right ()
  | otherwise =
      Left
        ( StoreTransitionSortOccurrenceMismatch
            delta
            slot.occurrenceId
            (retainedStoreObservationSortOccurrence observation)
        )

appendRetainedChange ::
  RetainedStoreObservation ->
  Maybe RetainedTransition ->
  StoreSlot ->
  StoreSlot
appendRetainedChange observation transition slot =
  slot
    { revision = successorRevision,
      retainedHistory =
        Map.insert successorRevision change slot.retainedHistory,
      retainedSnapshots =
        Map.insert
          successorRevision
          (retainedSnapshot slot.transcriptContents)
          slot.retainedSnapshots
    }
  where
    successorRevision = nextStoreRevision slot.revision
    change = RetainedStoreChange successorRevision observation transition

commitPeerStoreApplication ::
  PreparedPeerStoreApplication ->
  (State, NonEmpty PeerStoreOutcome)
commitPeerStoreApplication (PreparedPeerStoreApplication prepared) =
  commitPrepared prepared

-- | A contradictory attempt to give one already terminal Store key a second
-- ordered cause.  The authority coordinator ensures that only one terminal
-- path wins; observing this at the Store owner is therefore an invariant fault.
data ControlledObjectPurgeError
  = ControlledObjectPurgeCauseConflict
      StoreIncarnationId
      ObjectKey
      StructuralConsequenceCause
      StructuralConsequenceCause
  | ControlledObjectPurgeLedgerCauseConflict
      SortId
      ObjectKey
      StructuralConsequenceCause
      StructuralConsequenceCause
  deriving stock (Eq, Show)

-- | Exact owner-local consequences of one controlled-object purge.  Matched
-- incarnations name every retained Store of the supplied sort.  Affected
-- incarnations are the subset whose current contents or terminal-suppression
-- map changed; fact count counts physical entries removed from the three
-- 'DeltaStore' projections.
data ControlledObjectPurgeSummary
  = ControlledObjectPurgeSummary
      SortId
      ObjectKey
      [StoreIncarnationId]
      [StoreIncarnationId]
      Word64
  deriving stock (Eq, Show)

controlledObjectPurgeSummarySortId :: ControlledObjectPurgeSummary -> SortId
controlledObjectPurgeSummarySortId
  (ControlledObjectPurgeSummary sortId _ _ _ _) = sortId

controlledObjectPurgeSummaryObjectKey ::
  ControlledObjectPurgeSummary -> ObjectKey
controlledObjectPurgeSummaryObjectKey
  (ControlledObjectPurgeSummary _ objectKey _ _ _) = objectKey

controlledObjectPurgeSummaryMatchedIncarnations ::
  ControlledObjectPurgeSummary -> [StoreIncarnationId]
controlledObjectPurgeSummaryMatchedIncarnations
  (ControlledObjectPurgeSummary _ _ incarnations _ _) = incarnations

controlledObjectPurgeSummaryAffectedIncarnations ::
  ControlledObjectPurgeSummary -> [StoreIncarnationId]
controlledObjectPurgeSummaryAffectedIncarnations
  (ControlledObjectPurgeSummary _ _ _ incarnations _) = incarnations

controlledObjectPurgeSummaryPurgedFactCount ::
  ControlledObjectPurgeSummary -> Word64
controlledObjectPurgeSummaryPurgedFactCount
  (ControlledObjectPurgeSummary _ _ _ _ count) = count

newtype PreparedControlledObjectPurge
  = PreparedControlledObjectPurge
      (Prepared State ControlledObjectPurgeSummary)

-- | Atomically retain one controlled object suppression independently of Store
-- lifetime, purge it from every current retained incarnation of the supplied
-- sort, and project the authoritative ledger into those slots. Immutable
-- receipts, observations, changes, and historical snapshots remain available
-- as finite-run audit transcripts. Active slots are refreshed from the
-- canonical retained owner map in the same prepared successor.
prepareControlledObjectPurge ::
  StructuralConsequenceCause ->
  SortId ->
  ObjectKey ->
  State ->
  Either ControlledObjectPurgeError PreparedControlledObjectPurge
prepareControlledObjectPurge cause sortId objectKey state =
  PreparedControlledObjectPurge <$> prepareTransition purgeEvery state
  where
    purgeEvery predecessor = do
      terminalPurges <-
        case Map.lookup (sortId, objectKey) predecessor.terminalObjectPurges of
          Just retainedCause
            | retainedCause /= cause ->
                Left
                  ( ControlledObjectPurgeLedgerCauseConflict
                      sortId
                      objectKey
                      retainedCause
                      cause
                  )
          _ ->
            Right
              ( Map.insert
                  (sortId, objectKey)
                  cause
                  predecessor.terminalObjectPurges
              )
      let projectedPurges = terminalObjectPurgesForSortIn sortId terminalPurges
      (retained, matchedDescending, affectedDescending, purgedFacts) <-
        foldM
          (purgeOne projectedPurges)
          (Map.empty, [], [], 0)
          (Map.toAscList predecessor.retainedStores)
      let successor =
            predecessor
              { retainedStores = retained,
                stores =
                  fmap
                    (refreshActiveSlot retained)
                    predecessor.stores,
                terminalObjectPurges = terminalPurges
              }
          summary =
            ControlledObjectPurgeSummary
              sortId
              objectKey
              (reverse matchedDescending)
              (reverse affectedDescending)
              purgedFacts
      Right (successor, summary)

    purgeOne
      projectedPurges
      (retained, matched, affected, purgedFacts)
      (incarnation, slot)
        | slot.sortId /= sortId =
            Right
              ( Map.insert incarnation slot retained,
                matched,
                affected,
                purgedFacts
              )
        | otherwise = do
            case Map.lookup objectKey slot.terminalObjectPurges of
              Just retainedCause
                | retainedCause /= cause ->
                    Left
                      ( ControlledObjectPurgeCauseConflict
                          incarnation
                          objectKey
                          retainedCause
                          cause
                      )
              _ -> Right ()
            let (contents, removed) = purgeObjectKey objectKey slot.contents
                successorSlot =
                  slot
                    { contents,
                      terminalObjectPurges = projectedPurges
                    }
                changed = successorSlot /= slot
            Right
              ( Map.insert incarnation successorSlot retained,
                incarnation : matched,
                if changed then incarnation : affected else affected,
                purgedFacts + removed
              )

    refreshActiveSlot retained slot =
      Map.findWithDefault slot slot.incarnation retained

commitControlledObjectPurge ::
  PreparedControlledObjectPurge -> (State, ControlledObjectPurgeSummary)
commitControlledObjectPurge (PreparedControlledObjectPurge prepared) =
  commitPrepared prepared

-- | Rejections at the Store projection boundary.  The disappearance owner
-- supplies a checked subject and positive Resolve outcome; checking again here
-- prevents the Store leaf from accepting controlled deletion authority or two
-- incompatible suppression coordinates for one occurrence.
data RegularDefinitionRetirementError
  = RegularDefinitionRetirementRequiresRegularSubject DisappearanceSubject
  | RegularDefinitionRetirementResolveIndexZero DisappearanceSubject
  | RegularDefinitionRetirementSuppressionConflict
      DisappearanceSubject
      HeraldPublicationPrefix
      ControlIndex
      HeraldPublicationPrefix
      ControlIndex
  deriving stock (Eq, Show)

data RegularDefinitionRetirementDisposition
  = RegularDefinitionRetirementApplied
  | RegularDefinitionRetirementExactReplay
  deriving stock (Eq, Ord, Show)

-- | Exact Store-local result of installing one regular retirement.  Historical
-- transcripts, snapshots, observations, and receipts are deliberately absent
-- from the summary because none are rewritten.  The physical fact count covers
-- the visible-winner, retained-winner, and logical-strength projections in the
-- effective 'DeltaStore' value.
data RegularDefinitionRetirementSummary
  = RegularDefinitionRetirementSummary
      DisappearanceSubject
      RegularRetirementSuppression
      RegularDefinitionRetirementDisposition
      [StoreIncarnationId]
      [StoreIncarnationId]
      Word64
  deriving stock (Eq, Show)

regularDefinitionRetirementSummarySubject ::
  RegularDefinitionRetirementSummary -> DisappearanceSubject
regularDefinitionRetirementSummarySubject
  (RegularDefinitionRetirementSummary subject _ _ _ _ _) = subject

regularDefinitionRetirementSummarySuppression ::
  RegularDefinitionRetirementSummary -> RegularRetirementSuppression
regularDefinitionRetirementSummarySuppression
  (RegularDefinitionRetirementSummary _ suppression _ _ _ _) = suppression

regularDefinitionRetirementSummaryDisposition ::
  RegularDefinitionRetirementSummary -> RegularDefinitionRetirementDisposition
regularDefinitionRetirementSummaryDisposition
  (RegularDefinitionRetirementSummary _ _ disposition _ _ _) = disposition

regularDefinitionRetirementSummaryMatchedIncarnations ::
  RegularDefinitionRetirementSummary -> [StoreIncarnationId]
regularDefinitionRetirementSummaryMatchedIncarnations
  (RegularDefinitionRetirementSummary _ _ _ incarnations _ _) = incarnations

regularDefinitionRetirementSummaryAffectedIncarnations ::
  RegularDefinitionRetirementSummary -> [StoreIncarnationId]
regularDefinitionRetirementSummaryAffectedIncarnations
  (RegularDefinitionRetirementSummary _ _ _ _ incarnations _) = incarnations

regularDefinitionRetirementSummaryPurgedFactCount ::
  RegularDefinitionRetirementSummary -> Word64
regularDefinitionRetirementSummaryPurgedFactCount
  (RegularDefinitionRetirementSummary _ _ _ _ _ count) = count

newtype PreparedRegularDefinitionRetirement
  = PreparedRegularDefinitionRetirement
      (Prepared State RegularDefinitionRetirementSummary)

-- | Install one cut-qualified regular retirement and remove matching
-- descriptor publications and pre-Resolve Nabla/Delta references from every
-- effective Store projection atomically. Only 'contents' is projected: raw
-- transcript state, retained-change history, snapshots, and immutable
-- application receipts remain available for audit. An exact replay is a no-op
-- so it cannot erase a definition or structural reference observed after the
-- Resolve index.
prepareRegularDefinitionRetirement ::
  DisappearanceSubject ->
  HeraldPublicationPrefix ->
  ControlIndex ->
  State ->
  Either
    RegularDefinitionRetirementError
    PreparedRegularDefinitionRetirement
prepareRegularDefinitionRetirement subject localCut resolveIndex state = do
  requireRegularRetirementSubject subject
  unless
    (controlIndexWord64 resolveIndex > 0)
    (Left (RegularDefinitionRetirementResolveIndexZero subject))
  PreparedRegularDefinitionRetirement
    <$> prepareTransition installRetirement state
  where
    suppliedSuppression =
      RegularRetirementSuppression subject localCut resolveIndex

    installRetirement predecessor =
      case Map.lookup subject predecessor.regularRetirementSuppressions of
        Just retained
          | retained == suppliedSuppression ->
              Right
                ( predecessor,
                  RegularDefinitionRetirementSummary
                    subject
                    retained
                    RegularDefinitionRetirementExactReplay
                    []
                    []
                    0
                )
          | otherwise ->
              Left
                ( RegularDefinitionRetirementSuppressionConflict
                    subject
                    (regularRetirementSuppressionLocalPublicationCut retained)
                    (regularRetirementSuppressionResolveIndex retained)
                    localCut
                    resolveIndex
                )
        Nothing -> do
          let successorSuppressions =
                Map.insert
                  subject
                  suppliedSuppression
                  predecessor.regularRetirementSuppressions
              (retained, matchedDescending, affectedDescending, purgedFacts) =
                Map.foldlWithKey'
                  (reprojectRetainedSlot successorSuppressions)
                  (Map.empty, [], [], 0)
                  predecessor.retainedStores
              successor =
                predecessor
                  { retainedStores = retained,
                    stores = fmap (refreshActiveSlot retained) predecessor.stores,
                    regularRetirementSuppressions = successorSuppressions
                  }
          Right
            ( successor,
              RegularDefinitionRetirementSummary
                subject
                suppliedSuppression
                RegularDefinitionRetirementApplied
                (reverse matchedDescending)
                (reverse affectedDescending)
                purgedFacts
            )

    reprojectRetainedSlot
      successorSuppressions
      (retained, matched, affected, purgedFacts)
      incarnation
      slot =
        let contents =
              projectEffectiveStoreContents
                successorSuppressions
                slot.terminalObjectPurges
                slot
            removed = effectiveStoreFactRemovalCount slot.contents contents
            successorSlot = slot {contents}
            didMatch = successorSlot /= slot
         in ( Map.insert incarnation successorSlot retained,
              if didMatch then incarnation : matched else matched,
              if didMatch then incarnation : affected else affected,
              purgedFacts + removed
            )

    refreshActiveSlot retained slot =
      Map.findWithDefault slot slot.incarnation retained

requireRegularRetirementSubject ::
  DisappearanceSubject -> Either RegularDefinitionRetirementError ()
requireRegularRetirementSubject subject =
  case disappearanceSubjectView subject of
    RegularSortDefinitionSubjectView {} -> Right ()
    ControlledPredefinedSubjectView {} ->
      Left (RegularDefinitionRetirementRequiresRegularSubject subject)

commitRegularDefinitionRetirement ::
  PreparedRegularDefinitionRetirement ->
  (State, RegularDefinitionRetirementSummary)
commitRegularDefinitionRetirement
  (PreparedRegularDefinitionRetirement prepared) = commitPrepared prepared

preparedRegularDefinitionRetirementSummary ::
  PreparedRegularDefinitionRetirement -> RegularDefinitionRetirementSummary
preparedRegularDefinitionRetirementSummary
  (PreparedRegularDefinitionRetirement prepared) = snd (commitPrepared prepared)

requireStore :: DeltaId -> State -> Either StoreTransitionError StoreSlot
requireStore delta state =
  maybe
    (Left (StoreTransitionDeltaMissing delta))
    Right
    (lookupStoreSlot delta state)

requireIncarnation ::
  DeltaId ->
  StoreIncarnationId ->
  StoreSlot ->
  Either StoreTransitionError ()
requireIncarnation delta expected slot
  | storeSlotIncarnation slot == expected = Right ()
  | otherwise =
      Left
        ( StoreTransitionIncarnationMismatch
            delta
            expected
            (storeSlotIncarnation slot)
        )

replaceStoreSlotContents :: DeltaStore -> DeltaStore -> StoreSlot -> State -> State
replaceStoreSlotContents contents transcriptContents slot =
  replaceStoreSlot slot {contents = contents, transcriptContents = transcriptContents}

replaceStoreSlot :: StoreSlot -> State -> State
replaceStoreSlot slot state =
  state
    { stores = Map.insert (storeSlotDelta slot) slot state.stores,
      retainedStores =
        Map.insert incarnation slot state.retainedStores,
      sourceStoreChanges =
        if prefixChanged
          then Set.insert incarnation state.sourceStoreChanges
          else state.sourceStoreChanges,
      alignmentMemberStoreChanges =
        if prefixChanged
          then Set.insert incarnation state.alignmentMemberStoreChanges
          else state.alignmentMemberStoreChanges
    }
  where
    incarnation = storeSlotIncarnation slot
    prefixChanged =
      case Map.lookup incarnation state.retainedStores of
        Nothing -> True
        Just previous -> storeSlotRevision previous /= storeSlotRevision slot

-- | A contradiction between an Increment-3 exact Store patch and the sole
-- Store owner.  These are invariant faults after structural reconciliation;
-- dependency holding happens before this leaf transition is prepared.
data StructuralStorePatchError
  = StructuralStoreRetainedKeyMismatch
      StoreIncarnationId
      StoreIncarnationId
  | StructuralStoreRetainedBeforeMismatch StoreIncarnationId
  | StructuralStoreRetainedResourceMutation StoreIncarnationId
  | StructuralStoreRetainedResourceRemoval StoreIncarnationId
  | StructuralStoreActiveKeyMismatch DeltaId DeltaId
  | StructuralStoreActiveBeforeMismatch DeltaId
  | StructuralStoreActiveResourceMissing DeltaId StoreIncarnationId
  | StructuralStoreActiveResourceMismatch DeltaId StoreIncarnationId
  | StructuralStoreClosedIncarnationReuse DeltaId StoreIncarnationId
  deriving stock (Eq, Show)

newtype PreparedStructuralStorePatch
  = PreparedStructuralStorePatch (Prepared State ())

-- | Interpret one ready structural reconciliation patch without exposing a
-- partially updated Store owner.  Retained resource creation is applied before
-- activation, while passivation removes only the active delta index.
prepareStructuralStorePatch ::
  StructuralConsequenceCause ->
  Reconciliation.StorePatch ->
  State ->
  Either StructuralStorePatchError PreparedStructuralStorePatch
prepareStructuralStorePatch cause patch state =
  PreparedStructuralStorePatch
    <$> prepareTransition applyPatch state
  where
    applyPatch predecessor = do
      withResources <-
        foldM
          applyRetained
          predecessor
          (Map.toAscList (Reconciliation.storePatchRetainedResourceChanges patch))
      successor <-
        foldM
          applyActive
          withResources
          (Map.toAscList (Reconciliation.storePatchChanges patch))
      Right (successor, ())

    applyRetained current (incarnation, change) = do
      let before = Reconciliation.exactChangeBefore change
          after = Reconciliation.exactChangeAfter change
          retained = Map.lookup incarnation current.retainedStores
      requireRetainedBefore incarnation before retained
      case after of
        Nothing
          | before == Nothing -> Right current
          | otherwise ->
              Left (StructuralStoreRetainedResourceRemoval incarnation)
        Just specification
          | Reconciliation.dynamicStoreIncarnation specification /= incarnation ->
              Left
                ( StructuralStoreRetainedKeyMismatch
                    incarnation
                    (Reconciliation.dynamicStoreIncarnation specification)
                )
          | Just specification == before -> Right current
          | before /= Nothing ->
              Left (StructuralStoreRetainedResourceMutation incarnation)
          | otherwise -> do
              let slot = structuralStoreSlot cause specification current
              Right
                current
                  { retainedStores =
                      Map.insert incarnation slot current.retainedStores,
                    sourceStoreChanges = Set.insert incarnation current.sourceStoreChanges,
                    alignmentMemberStoreChanges = Set.insert incarnation current.alignmentMemberStoreChanges
                  }

    applyActive current (delta, change) = do
      let before = Reconciliation.exactChangeBefore change
          after = Reconciliation.exactChangeAfter change
          active = Map.lookup delta current.stores
      requireActiveBefore delta before active
      case after of
        Nothing -> Right (passivateActiveStore delta current)
        Just specification
          | Reconciliation.dynamicStoreDelta specification /= delta ->
              Left
                ( StructuralStoreActiveKeyMismatch
                    delta
                    (Reconciliation.dynamicStoreDelta specification)
                )
          | otherwise -> do
              let incarnation = Reconciliation.dynamicStoreIncarnation specification
              requireOpenIncarnation delta incarnation current
              retained <-
                maybe
                  (Left (StructuralStoreActiveResourceMissing delta incarnation))
                  Right
                  (Map.lookup incarnation current.retainedStores)
              if storeSlotMatchesDynamic specification retained
                then
                  Right
                    current
                      { stores = Map.insert delta retained current.stores,
                        preparationSlotChanges =
                          if fmap storeSlotIncarnation active == Just incarnation
                            then current.preparationSlotChanges
                            else Set.insert delta current.preparationSlotChanges,
                        alignmentLossChanged = current.alignmentLossChanged || fmap storeSlotIncarnation active /= Just incarnation,
                        closedStores = closeReplaced active incarnation current.closedStores
                      }
                else
                  Left
                    (StructuralStoreActiveResourceMismatch delta incarnation)

    closeReplaced active successorIncarnation closed =
      case active of
        Just predecessor
          | storeSlotIncarnation predecessor /= successorIncarnation ->
              Set.insert (storeSlotIncarnation predecessor) closed
        _ -> closed

passivateActiveStore :: DeltaId -> State -> State
passivateActiveStore delta state =
  state
    { stores = Map.delete delta state.stores,
      preparationSlotChanges =
        if Map.member delta state.stores
          then Set.insert delta state.preparationSlotChanges
          else state.preparationSlotChanges,
      alignmentLossChanged = state.alignmentLossChanged || Map.member delta state.stores,
      closedStores =
        maybe
          state.closedStores
          ( \slot ->
              Set.insert
                (storeSlotIncarnation slot)
                state.closedStores
          )
          (Map.lookup delta state.stores)
    }

requireOpenIncarnation ::
  DeltaId ->
  StoreIncarnationId ->
  State ->
  Either StructuralStorePatchError ()
requireOpenIncarnation delta incarnation state =
  if Set.member incarnation state.closedStores
    then Left (StructuralStoreClosedIncarnationReuse delta incarnation)
    else Right ()

commitStructuralStorePatch :: PreparedStructuralStorePatch -> State
commitStructuralStorePatch (PreparedStructuralStorePatch prepared) =
  fst (commitPrepared prepared)

requireRetainedBefore ::
  StoreIncarnationId ->
  Maybe Reconciliation.DynamicLocalStoreSpec ->
  Maybe StoreSlot ->
  Either StructuralStorePatchError ()
requireRetainedBefore incarnation expected retained =
  case (expected, retained) of
    (Nothing, Nothing) -> Right ()
    (Just specification, Just slot)
      | storeSlotMatchesDynamic specification slot -> Right ()
    _ -> Left (StructuralStoreRetainedBeforeMismatch incarnation)

requireActiveBefore ::
  DeltaId ->
  Maybe Reconciliation.DynamicLocalStoreSpec ->
  Maybe StoreSlot ->
  Either StructuralStorePatchError ()
requireActiveBefore delta expected active =
  case (expected, active) of
    (Nothing, Nothing) -> Right ()
    (Just specification, Just slot)
      | storeSlotMatchesDynamic specification slot -> Right ()
    _ -> Left (StructuralStoreActiveBeforeMismatch delta)

storeSlotMatchesDynamic ::
  Reconciliation.DynamicLocalStoreSpec ->
  StoreSlot ->
  Bool
storeSlotMatchesDynamic specification slot =
  case slot.provenance of
    StructuralBaselineReader _ process controller source ->
      matchesExactDynamic process controller source
    StructuralApplicationReader _ process controller source ->
      matchesExactDynamic process controller source
    StructuralControlReader _ process controller source ->
      matchesExactDynamic process controller source
    ApplicationReader process _ ->
      process == Reconciliation.dynamicStoreProcess specification
        && matchesSpecification
    _ -> False
  where
    matchesExactDynamic process controller source =
      process == Reconciliation.dynamicStoreProcess specification
        && controller == Reconciliation.dynamicStoreController specification
        && source == Reconciliation.dynamicStoreIncarnationSource specification
        && matchesSpecification

    matchesSpecification =
      slot.delta == Reconciliation.dynamicStoreDelta specification
        && slot.sortId
          == sortOccurrenceSortId (Reconciliation.dynamicStoreSort specification)
        && slot.occurrenceId
          == sortOccurrenceDefinition (Reconciliation.dynamicStoreSort specification)
        && slot.incarnation
          == Reconciliation.dynamicStoreIncarnation specification

structuralStoreSlot ::
  StructuralConsequenceCause ->
  Reconciliation.DynamicLocalStoreSpec ->
  State ->
  StoreSlot
structuralStoreSlot cause specification state =
  StoreSlot
    { provenance =
        case structuralConsequenceCauseView cause of
          StructuralOccurrenceCauseView occurrence ->
            StructuralApplicationReader
              occurrence
              process
              controller
              source
          LabelReleaseCauseView {} -> controlReader
          ProcessEndCauseView {} -> controlReader
          PredefinedDisappearanceCauseView {} -> controlReader,
      delta = Reconciliation.dynamicStoreDelta specification,
      sortId = sortOccurrenceSortId (Reconciliation.dynamicStoreSort specification),
      occurrenceId =
        sortOccurrenceDefinition (Reconciliation.dynamicStoreSort specification),
      incarnation = Reconciliation.dynamicStoreIncarnation specification,
      transcriptContents =
        emptyDeltaStore
          (sortOccurrenceSortId (Reconciliation.dynamicStoreSort specification)),
      contents =
        emptyDeltaStore
          (sortOccurrenceSortId (Reconciliation.dynamicStoreSort specification)),
      appliedPublications = Map.empty,
      applicationObservations = [],
      revision = initialStoreRevision,
      baseObservations = [],
      retainedHistory = Map.empty,
      retainedSnapshots =
        Map.singleton
          initialStoreRevision
          ( retainedSnapshot
              ( emptyDeltaStore
                  (sortOccurrenceSortId (Reconciliation.dynamicStoreSort specification))
              )
          ),
      terminalObjectPurges = terminalObjectPurgesForSort sortId state
    }
  where
    process = Reconciliation.dynamicStoreProcess specification
    sortId = sortOccurrenceSortId (Reconciliation.dynamicStoreSort specification)
    controller = Reconciliation.dynamicStoreController specification
    source = Reconciliation.dynamicStoreIncarnationSource specification
    controlReader = StructuralControlReader cause process controller source

data StoreBootstrapError
  = StoreAlreadyInstalled DeltaId
  | StoreIncarnationAlreadyInstalled StoreIncarnationId
  deriving stock (Eq, Show)

data StorePreparationError
  = StoreRejected StoreBootstrapError
  | StoreInvariantContradiction StoreError
  deriving stock (Eq, Show)

newtype PreparedStoreBootstrap
  = PreparedStoreBootstrap (Prepared State ())

prepareStoreBootstrap ::
  [LocalStoreSpec] ->
  State ->
  Either StorePreparationError PreparedStoreBootstrap
prepareStoreBootstrap specifications state =
  PreparedStoreBootstrap
    <$> prepareTransition (installStores specifications) state

commitStoreBootstrap :: PreparedStoreBootstrap -> State
commitStoreBootstrap (PreparedStoreBootstrap prepared) =
  fst (commitPrepared prepared)

installStores ::
  [LocalStoreSpec] ->
  State ->
  Either StorePreparationError (State, ())
installStores specifications state = do
  successor <- foldM (flip insertStore) state specifications
  Right (successor, ())

insertStore ::
  LocalStoreSpec ->
  State ->
  Either StorePreparationError State
insertStore specification state
  | Map.member delta state.stores =
      Left (StoreRejected (StoreAlreadyInstalled delta))
  | Map.member specification.incarnation state.retainedStores =
      Left
        ( StoreRejected
            (StoreIncarnationAlreadyInstalled specification.incarnation)
        )
  | otherwise = do
      transcriptContents <-
        seedMatchingPublications specification state.primordialPublications
      let terminalPurges = terminalObjectPurgesForSort specification.sortId state
          baseObservations =
            [ RetainedStoreObservation
                publication
                Normal
                specification.occurrenceId
                PrimordialStoreObservation
            | publication <- state.primordialPublications,
              checkedPublicationSort publication == specification.sortId
            ]
          contents = applyTerminalObjectPurges terminalPurges transcriptContents
          slot =
            StoreSlot
              { provenance = specification.provenance,
                delta = delta,
                sortId = specification.sortId,
                occurrenceId = specification.occurrenceId,
                incarnation = specification.incarnation,
                transcriptContents = transcriptContents,
                contents = contents,
                appliedPublications =
                  Map.fromList
                    [ (checkedPublicationId publication, Normal)
                    | publication <- state.primordialPublications,
                      checkedPublicationSort publication == specification.sortId
                    ],
                applicationObservations = baseObservations,
                revision = initialStoreRevision,
                baseObservations,
                retainedHistory = Map.empty,
                retainedSnapshots =
                  Map.singleton initialStoreRevision (retainedSnapshot transcriptContents),
                terminalObjectPurges = terminalPurges
              }
      Right
        state
          { stores = Map.insert delta slot state.stores,
            retainedStores =
              Map.insert specification.incarnation slot state.retainedStores,
            sourceStoreChanges = Set.insert specification.incarnation state.sourceStoreChanges,
            preparationSlotChanges = Set.insert delta state.preparationSlotChanges,
            alignmentLossChanged = True,
            alignmentMemberStoreChanges = Set.insert specification.incarnation state.alignmentMemberStoreChanges
          }
  where
    delta = specification.delta

seedMatchingPublications ::
  LocalStoreSpec ->
  [CheckedPublication] ->
  Either StorePreparationError DeltaStore
seedMatchingPublications specification =
  foldM applyOne (emptyDeltaStore specification.sortId)
    . filter ((== specification.sortId) . checkedPublicationSort)
  where
    applyOne store publication = case applyPublicationDetailed Normal publication store of
      Right (successor, _) -> Right successor
      Left problem -> Left (StoreInvariantContradiction problem)
