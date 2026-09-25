{-# LANGUAGE OverloadedStrings #-}

-- | Pure owner for atomic canonical label decisions and installation completion.
-- Fresh requests blocked by their object's pending installation are deferred without
-- a receipt. Exact retries and retired identities are classified first.
module Eclips.Oracle.Internal.Label
  ( -- * Decision identity
    deriveLabelDecisionId,
    heraldAdmissionCommand,
    beginHeraldAdmissionCommand,
    sealHeraldAdmissionCommand,
    acceptHeraldJoinSealCommand,
    reportHeraldJoinBaseReadyCommand,
    reportHeraldJoinReadyCommand,
    activateHeraldCommand,
    cancelHeraldAdmissionCommand,
    oracleCommandAdmission,
    oracleHeraldCatalogue,
    oracleHeraldAdmissions,
    oraclePendingHeraldAdmission,
    oracleHeraldAdmission,
    registerOracleReplicaCommand,
    beginVoterChangeCommand,
    cancelVoterChangeCommand,
    oracleReplicaRegistrations,
    oracleReplicaReplicationTargets,
    oracleVoterConfiguration,
    oracleVoterConfigurations,
    oracleVoterChange,
    oraclePendingVoterChange,
    prepareOracleConfiguration,
    applyCommittedVoterState,
    oracleReceiptVoterResult,
    oracleCommandVoterChangeCancellation,
    AppliedOracleCommandEntry,
    appliedEntryCommand,
    appliedEntryConfiguration,
    appliedEntryReceiptRetirement,
    appliedEntryReceiptRetirementProgress,
    appliedEntryOracleProgress,

    -- * Process lifecycle
    LiveProcessTable,
    LiveProcessProblem (..),
    LiveEndProblem (..),
    LiveProcessStatusView (..),
    liveLiveProcesses,
    liveEndProcess,
    liveProcessStatus,
    liveEffectiveLabel,
    liveEffectiveReleasedState,

    -- * Authority tenures
    LiveAuthority,
    LiveAuthorityView (..),
    liveExistingAuthority,
    liveAuthorityView,
    liveAuthorityCanonicalBytes,

    -- * Prior and prepared label states
    LivePriorLabel,
    LivePriorProblem (..),
    liveOrdinaryPrior,
    liveAuthorityPrior,
    livePriorFromRecord,
    LiveLabelProblem (..),
    LivePreparedLabel,
    prepareLiveLabel,
    livePreparedPriorEffectiveState,
    livePreparedOutcome,
    livePreparedSameEffective,
    LiveAuthorityDispositionView (..),
    livePreparedAuthorityDisposition,

    -- * Release
    LiveReleaseProblem (..),
    ReleasedLabelOverlay,
    releasedLabelOverlayState,
    releasedLabelOverlayRevision,
    releasedLabelOverlayRetainedAuthority,
    LiveLabelRecord,
    releaseLiveLabel,
    liveRecordStoredState,
    liveRecordRevision,
    liveRecordRetainedAuthority,
    liveRecordApplicableAuthority,

    -- * Control-qualified consequence provenance
    LiveConsequenceCause,
    LiveConsequenceCauseView (..),
    LiveConsequenceCauseProblem (..),
    liveStructuralOccurrenceCause,
    liveLabelReleaseCause,
    liveProcessEndCause,
    liveConsequenceCauseView,
    liveConsequenceCauseTag,
    liveConsequenceCauseCanonicalBytes,

    -- * Abstract workflow phase
    LiveWorkflowPhaseView (..),

    -- * Complete private normalized evidence
    LivePreparedLabelFacts,
    livePreparedFactsDecisionId,
    livePreparedFactsResolveIndex,
    livePreparedFactsObjectId,
    livePreparedFactsProposedOutcome,
    livePreparedFactsExpectedPriorState,
    livePreparedFactsExpectedPriorRevision,
    livePreparedFactsCatalogueDigest,
    livePreparedFactsExpectedPriorAuthority,
    livePreparedFactsPriorAuthorityJustification,
    livePreparedFactsAuthorityDisposition,
    livePreparedFactsMemberSetDigest,
    livePreparedLabelFactsCanonicalBytes,
    deriveLivePreparedLabelDigest,
    LiveNotAppliedReason (..),
    liveNotAppliedReasonTag,
    liveNotAppliedReasonCanonicalBytes,
    LiveTerminalOutcomeProblem (..),
    LiveTerminalOutcome,
    liveNotAppliedTerminalOutcome,
    liveReleasedTerminalOutcomeFromFacts,
    LiveTerminalOutcomeView (..),
    liveTerminalOutcomeView,
    liveTerminalOutcomeCanonicalBytes,
    deriveLabelOutcomeDigest,

    -- * Complete private Oracle kernel
    ProcessOriginView (..),
    ProcessLifecycleView (..),
    ProcessEpochRecord,
    processRecordProcessId,
    processRecordProcessEpoch,
    processRecordResidence,
    processRecordOrigin,
    processRecordLifecycle,
    encodeProcessEpochRecord,
    decodeProcessEpochRecord,
    LiveLabelDecision,
    encodeLabelDecision,
    decodeLabelDecision,
    encodeLabelTerminalOutcome,
    decodeLabelTerminalOutcome,
    liveDecisionId,
    liveDecisionOpenRequestId,
    liveDecisionHome,
    liveDecisionCaller,
    liveDecisionObject,
    liveDecisionExpectedLabel,
    liveDecisionInitialLabelEvidence,
    liveDecisionExpectedPriorRevision,
    liveDecisionExpectedPriorAuthority,
    liveDecisionPriorAuthorityJustification,
    liveDecisionTarget,
    liveDecisionRequestControlIndex,
    liveDecisionHomeAcceptanceCut,
    liveDecisionCapturedHeralds,
    liveDecisionMemberSetDigest,
    liveDecisionMembershipGeneration,
    LabelCompletionAttestation,
    labelCompletionAttestation,
    labelCompletionDecisionId,
    labelCompletionControlIndex,
    labelCompletionOutcomeDigest,
    labelCompletionCollector,
    labelCompletionMembershipGeneration,
    openDisappearanceProbeCommand,
    reportPredefinedAbsenceCommand,
    invalidateDisappearanceProbeCommand,
    resolveDisappearanceProbeCommand,
    abortDisappearanceProbeCommand,
    oracleReceiptDisappearanceResult,
    oracleDisappearanceProbe,
    oracleDisappearanceProbes,
    oracleRegularRetirementIndex,
    oracleControlledSubjectDisappeared,
    OracleCommand,
    startProcessEpochCommand,
    retireOracleReceiptsCommand,
    retireOracleReceiptProgressCommand,
    retireOracleProgressCommand,
    oracleCommandProgress,
    oracleCommandReceiptRetirement,
    oracleCommandReceiptRetirementProgress,
    EndProcessCommandProblem (..),
    endProcessEpochCommand,
    decideLabelCommand,
    completeLabelDecisionCommand,
    ProbeResult (..),
    FailureProbeTerminalView (..),
    FailureProbeView (..),
    FailureResolutionReady (..),
    FailureCommandResult (..),
    FailureRejection (..),
    openHeraldFailureProbeCommand,
    reportHeraldFailureProbeCommand,
    dismissHeraldFailureProbeCommand,
    acceptVoterHostFailureCommand,
    retireHeraldEpochCommand,
    oracleCommandStart,
    oracleCommandTag,
    oracleCommandCanonicalBytes,
    OracleCanonicalError (..),
    DecodedOracleEnvelope,
    decodeOracleEnvelopeCanonicalBytes,
    decodedOracleEnvelopeValue,
    decodedOracleEnvelopeCanonicalBytes,
    OracleEnvelope,
    oracleEnvelope,
    oracleEnvelopeRequestId,
    oracleEnvelopeWithReceiptRetirement,
    oracleEnvelopeWithReceiptRetirementProgress,
    oracleEnvelopeWithProgress,
    oracleEnvelopeProgress,
    oracleEnvelopeReceiptRetirement,
    oracleEnvelopeReceiptRetirementProgress,
    oracleEnvelopeExpectedControlIndex,
    oracleEnvelopeHomeHeraldEpoch,
    oracleEnvelopeCommand,
    oracleEnvelopeCanonicalBytes,
    oracleEnvelopeDigest,
    OracleRejection (..),
    oracleRejectionTag,
    oracleRejectionCanonicalBytes,
    OracleReceiptResult (..),
    OracleReceipt,
    oracleReceiptRequestId,
    oracleReceiptCommandDigest,
    oracleReceiptControlIndex,
    oracleReceiptResult,
    oracleReceiptFailureResult,
    oracleReceiptCanonicalBytes,
    DecodedOracleReceipt,
    decodeOracleReceiptCanonicalBytes,
    decodedOracleReceiptValue,
    decodedOracleReceiptCanonicalBytes,
    OracleProjectionEventView (..),
    OracleProjectionEvent,
    oracleProjectionEventView,
    oracleProjectionEventTag,
    oracleProjectionEventVectorCanonicalBytes,
    DecodedOracleProjectionEventVector,
    decodeOracleProjectionEventVectorCanonicalBytes,
    decodedOracleProjectionEventVectorValue,
    decodedOracleProjectionEventVectorCanonicalBytes,
    AppliedOracleEntry,
    appliedEntryControlIndex,
    appliedEntryRequestId,
    appliedEntryCommandDigest,
    appliedEntryReceipt,
    appliedEntryProjectionEvents,
    appliedEntryPostStateDigest,
    appliedOracleEntryCanonicalBytes,
    DecodedAppliedOracleEntry,
    decodeAppliedOracleEntryCanonicalBytes,
    decodedAppliedOracleEntryValue,
    decodedAppliedOracleEntryCanonicalBytes,
    OracleProtocolDisposition (..),
    OracleRequestRetirement (..),
    oracleRequestRetirement,
    oracleReceiptRetirementFrontier,
    oracleReceiptRetirementProgress,
    oracleReceiptRetirementFrontiers,
    oracleReceiptRetirementProgresses,
    oracleProgresses,
    oracleLabelRetiredThrough,
    oracleLatestLabelDecision,
    OracleSubmissionOutcome,
    OracleSubmissionOutcomeView (..),
    oracleSubmissionOutcomeView,
    oracleSubmissionAppliedEntry,
    OracleState,
    initialOracleState,
    initialOracleStateWithDigestMode,
    OracleStateDigestMode (..),
    oracleStateDigestMode,
    oracleSubmissionPreflightBlocker,
    submitOracleState,
    oracleStateGenesis,
    oracleCheckpointBytes,
    decodeOracleCheckpoint,
    oracleRequestReceipts,
    oracleGreatestControlIndex,
    oracleStateCanonicalBytes,
    oracleStateDiagnosticCardinalities,
    oracleStateDiagnosticSections,
    oracleStateDigest,
    oracleCurrentMembership,
    oracleMembershipHistory,
    oracleCheckedMembershipHistory,
    oracleRetiredHeralds,
    oracleFailureProbe,
    oracleActiveFailureProbeFor,
    oracleRequestCount,
    oracleRequestReceipt,
    oracleProcessRecord,
    oracleProcessRecords,
    oracleLabelRecord,
    oracleLabelRecords,
    oracleOpenDecisions,
    oracleObjectDecision,
    oracleOpenDecision,
    oracleWorkflowPhase,
    oracleLabelCollector,
    oracleTerminalOutcome,
    oracleCompletedWorkflowCount,
  )
where

import Control.Monad (replicateM, unless)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as ShortByteString
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize.Get qualified as SerializeGet
import Data.Serialize.Put qualified as SerializePut
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Disappearance qualified as DD
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    AuthorityEpochView (..),
    ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    ProcessEpochId,
    ProcessId,
    SortId,
    StructuralOccurrenceId,
    SystemId,
    authorityEpochCanonicalBytes,
    authorityEpochView,
    controlIndex,
    controlIndexWord64,
    decodeAuthorityEpochCanonicalBytes,
    genesisAuthorityEpoch,
    globalObjectIdBytes,
    heraldEpochBytes,
    heraldIdBytes,
    labelAuthorityEpoch,
    labelDecisionIdBytes,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkLabelDecisionId,
    mkNablaId,
    mkProcessEpochId,
    mkProcessId,
    mkStructuralSequence,
    mkTopologyCutId,
    nablaIdBytes,
    nablaSequence,
    nablaSequenceWord64,
    processEpochIdBytes,
    processIdBytes,
    publicationAuthorityEpoch,
    publicationId,
    publicationNabla,
    publicationNablaSequence,
    publicationSourceHeraldEpoch,
    structuralAuthorityEpoch,
    structuralOccurrenceId,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
    structuralSequenceWord64,
    systemIdBytes,
    topologyCutIdBytes,
  )
import Eclips.Domain.Label
  ( HomeLabelAcceptanceCut,
    InitialLabelEvidence,
    InitialLabelEvidenceSourceView (..),
    LabelOutcomeDigest,
    LabelRecord,
    LabelRevision,
    LabelTarget,
    LabelTargetView (..),
    LabelTransitionError (..),
    LabelTransitionOutcome (..),
    PreparedAuthorityDisposition,
    PreparedAuthorityDispositionView (..),
    PreparedLabelDigest,
    PreparedLabelFacts,
    PriorAuthorityJustification,
    PriorAuthorityJustificationView (..),
    ReleasedLabelState,
    ReleasedLabelStateView (..),
    checkedGenesisAuthority,
    decodeHomeLabelAcceptanceCutCanonicalBytes,
    decodePreparedLabelFactsCanonicalBytes,
    derivePreparedLabelDigest,
    establishedStructuralAuthority,
    existingReleasedAuthority,
    homeLabelAcceptanceCutCanonicalBytes,
    homeLabelAcceptanceCutProcessPosition,
    initialBootstrapLabelEvidence,
    initialLabelEvidenceLabel,
    initialLabelEvidenceObject,
    initialLabelEvidenceSource,
    initialPublicationLabelEvidence,
    labelOutcomeDigestBytes,
    labelProcessAcceptancePositionProcess,
    labelRecordReleasedState,
    labelRecordRetainedAuthority,
    labelRecordRevision,
    labelRevisionControlIndex,
    labelTargetView,
    labelTransition,
    mkLabelOutcomeDigest,
    mkLabelRecord,
    mkLabelRevision,
    mkPreparedLabelDigest,
    mkPreparedLabelFacts,
    preparedAuthorityDisposition,
    preparedAuthorityDispositionView,
    preparedLabelDigestBytes,
    preparedLabelFactsAuthorityDisposition,
    preparedLabelFactsCanonicalBytes,
    preparedLabelFactsCatalogueDigest,
    preparedLabelFactsDecisionId,
    preparedLabelFactsExpectedPriorAuthority,
    preparedLabelFactsExpectedPriorRevision,
    preparedLabelFactsExpectedPriorState,
    preparedLabelFactsMemberSetDigest,
    preparedLabelFactsObjectId,
    preparedLabelFactsPriorAuthorityJustification,
    preparedLabelFactsProposedOutcome,
    preparedLabelFactsResolveIndex,
    priorAuthorityJustificationView,
    releasedDeleted,
    releasedLabel,
    releasedLabelStateIsDeleted,
    releasedLabelStateView,
    targetDelete,
    targetProcess,
    targetVoid,
  )
import Eclips.Domain.Membership
  ( FailureProbeResolution (..),
    FailureProbeResolutionId,
    HeraldAdmissionId,
    HeraldFailureProbeId,
    HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    HeraldMembershipHistory,
    decodeFailureProbeResolutionIdCanonicalBytes,
    decodeHeraldFailureProbeIdCanonicalBytes,
    decodeHeraldMembershipGenerationCanonicalBytes,
    failureProbeResolutionIdCanonicalBytes,
    failureProbeResolutionProbeId,
    heraldFailureProbeControlIndex,
    heraldFailureProbeIdCanonicalBytes,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationCanonicalBytes,
    heraldMembershipGenerationChangeControlIndex,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
    heraldMembershipGenerationRetiredHeraldEpoch,
    mkHeraldMembershipGenerationId,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (..),
    decodeProcessEndReasonCanonicalBytes,
    effectiveProcessLabel,
    processEndReasonCanonicalBytes,
    processEndReasonMayBeHeraldSubmitted,
  )
import Eclips.Domain.ProcessStart
  ( ProcessStart,
    processStart,
    processStartProcessEpochId,
    processStartProcessId,
    processStartResidence,
  )
import Eclips.Domain.Sort.Profile
  ( CatalogueDigest,
    catalogueDigestBytes,
    mkCatalogueDigest,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    HeraldMember (..),
    appliedProcessEpochId,
    appliedProcessId,
    appliedProcessResidence,
    configurationDigestBytes,
    initialProjectionDigestBytes,
    mkInitialProjectionDigest,
  )
import Eclips.Domain.Topology
  ( MemberSetDigest,
    deriveMemberSetDigest,
    memberSetDigestBytes,
    mkMemberSetDigest,
  )
import Eclips.Domain.Topology qualified
import Eclips.Domain.Value
  ( Label,
    LabelOwner (..),
    ValueView (LabelValue),
    canonicalValueByteString,
    canonicalValueBytes,
    decodeCanonicalValue,
    labelValue,
    viewValue,
  )
import Eclips.Oracle.Genesis (CheckedOracleGenesis, checkedOracleActiveHeralds, checkedOracleAppliedBootstraps, checkedOracleCatalogueDigest, checkedOracleConfigurationDigest, checkedOracleInitialProjectionDigest, checkedOracleRaftConfigurationDigest, checkedOracleSystemId, raftVoterBindingHeraldEpoch, raftVoterBindingNode)
import Eclips.Oracle.Identity
  ( OracleClientRequestId,
    OracleCommandDigest,
    OracleStateDigest,
    mkOracleCommandDigest,
    mkOracleStateDigest,
    oracleClientRequestHome,
    oracleClientRequestId,
    oracleClientRequestSequence,
    oracleCommandDigestBytes,
    oracleStateDigestBytes,
    raftConfigurationDigestBytes,
  )
import Eclips.Oracle.Internal.Admission qualified as A
import Eclips.Oracle.Internal.CanonicalState (refreshCanonicalStateDigest)
import Eclips.Oracle.Internal.Disappearance qualified as D
import Eclips.Oracle.Internal.DisappearanceCanonical qualified as DC
import Eclips.Oracle.Internal.Failure
  ( FailureCommandResult (..),
    FailureEvent (..),
    FailureProbeTerminalView (..),
    FailureProbeView (..),
    FailureRejection (..),
    FailureResolutionReady (..),
    FailureState,
    ProbeResult (..),
    dismissFailureProbe,
    failureActiveHeralds,
    failureActiveProbeFor,
    failureCheckedMembershipHistory,
    failureCurrentMembership,
    failureHasPendingRetirement,
    failureMembershipHistory,
    failureProbe,
    failureRetiredHeralds,
    failureStateCanonicalBytes,
    failureStateIsGenesisIdle,
    initialFailureState,
    installAdmissionMembership,
    openFailureProbe,
    reportFailureProbe,
    retireHeraldEpoch,
    supersedeFailureProbesForVoters,
  )
import Eclips.Oracle.Internal.Failure qualified as F
import Eclips.Oracle.Internal.LabelCanonical
  ( NormalizedPreparedFacts (..),
    NormalizedTerminalOutcome (..),
    canonicalTerminalOutcomeBytes,
  )
import Eclips.Oracle.Internal.Voter qualified as V
import Eclips.Oracle.Progress
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import Eclips.Raft.Configuration (RaftVotingConfiguration)
import Eclips.Raft.Effect (RaftCommittedEntry)
import Eclips.Raft.Identity (RaftNodeId)

-- | Derive the stable decision identity from the exact request identity.
--
-- SHA-256 always has the checked Domain identity width, so failure of the final
-- nominal conversion would be an implementation invariant fault.
deriveLabelDecisionId :: SystemId -> OracleClientRequestId -> LabelDecisionId
deriveLabelDecisionId system requestId =
  case mkLabelDecisionId (SHA256.hash transcript) of
    Right decisionId -> decisionId
    Left problem ->
      error
        ( "SHA-256 produced an invalid LabelDecisionId: "
            <> show problem
        )
  where
    transcript =
      SerializePut.runPut $ do
        SerializePut.putByteString labelDecisionDomain
        SerializePut.putByteString (systemIdBytes system)
        SerializePut.putByteString
          (heraldEpochBytes (oracleClientRequestHome requestId))
        SerializePut.putWord64be (oracleClientRequestSequence requestId)

labelDecisionDomain :: ByteString
labelDecisionDomain = "ECLIPS-LABEL-DECISION"

data LiveProcessStatus
  = LiveProcessLive
  | LiveProcessEnded ControlIndex ProcessEndReason
  deriving stock (Eq, Show)

newtype LiveProcessTable
  = LiveProcessTable (Map ProcessEpochId LiveProcessStatus)
  deriving stock (Eq, Show)

data LiveProcessProblem
  = LiveDuplicateProcess ProcessEpochId
  deriving stock (Eq, Show)

data LiveEndProblem
  = LiveEndIndexMustBePositive
  | LiveEndUnknownProcess ProcessEpochId
  | LiveEndAlreadyEnded ProcessEpochId
  deriving stock (Eq, Show)

data LiveProcessStatusView
  = LiveProcessLiveView
  | LiveProcessEndedView ControlIndex ProcessEndReason
  deriving stock (Eq, Show)

-- | Construct an exact live-process table, rejecting duplicate presentation.
liveLiveProcesses ::
  [ProcessEpochId] ->
  Either LiveProcessProblem LiveProcessTable
liveLiveProcesses = fmap LiveProcessTable . foldl insertLive (Right Map.empty)
  where
    insertLive accumulated process = do
      processes <- accumulated
      case Map.lookup process processes of
        Just _ -> Left (LiveDuplicateProcess process)
        Nothing -> Right (Map.insert process LiveProcessLive processes)

-- | Monotonically end one known live process at a positive control index.
--
-- A fresh attempt to end an already ended process is rejected here; exact
-- Oracle-request retry belongs to the request-record layer added later.
liveEndProcess ::
  ControlIndex ->
  ProcessEndReason ->
  ProcessEpochId ->
  LiveProcessTable ->
  Either LiveEndProblem LiveProcessTable
liveEndProcess endIndex reason process (LiveProcessTable processes)
  | controlIndexWord64 endIndex == 0 = Left LiveEndIndexMustBePositive
  | otherwise = case Map.lookup process processes of
      Nothing -> Left (LiveEndUnknownProcess process)
      Just (LiveProcessEnded _ _) -> Left (LiveEndAlreadyEnded process)
      Just LiveProcessLive ->
        Right
          ( LiveProcessTable
              (Map.insert process (LiveProcessEnded endIndex reason) processes)
          )

liveProcessStatus ::
  ProcessEpochId ->
  LiveProcessTable ->
  Maybe LiveProcessStatusView
liveProcessStatus process (LiveProcessTable processes) =
  statusView <$> Map.lookup process processes
  where
    statusView LiveProcessLive = LiveProcessLiveView
    statusView (LiveProcessEnded index reason) =
      LiveProcessEndedView index reason

liveEffectiveLabel :: LiveProcessTable -> Label -> Label
liveEffectiveLabel (LiveProcessTable processes) =
  effectiveProcessLabel isEnded
  where
    isEnded process = case Map.lookup process processes of
      Just (LiveProcessEnded _ _) -> True
      _ -> False

liveEffectiveReleasedState ::
  LiveProcessTable ->
  ReleasedLabelState ->
  ReleasedLabelState
liveEffectiveReleasedState processes state =
  case releasedLabelStateView state of
    ReleasedLabelView label ->
      releasedLabel (liveEffectiveLabel processes label)
    ReleasedDeletedView _ -> state

-- | The kernel uses the one live Domain-owned authority vocabulary.  This
-- alias is retained only to keep the internal workflow signatures readable.
type LiveAuthority = AuthorityEpoch

data LiveAuthorityView
  = LiveExistingAuthorityView AuthorityEpoch
  | LiveLabelAuthorityView ControlIndex
  deriving stock (Eq, Ord, Show)

liveExistingAuthority :: AuthorityEpoch -> LiveAuthority
liveExistingAuthority = id

liveAuthorityView :: LiveAuthority -> LiveAuthorityView
liveAuthorityView authority = case authorityEpochView authority of
  LabelAuthorityEpochView index -> LiveLabelAuthorityView index
  _ -> LiveExistingAuthorityView authority

-- | The exact stored prior label plus whether its controlled role carries an
-- authority tenure.  Absence means an ordinary non-authority-bearing object,
-- not a temporarily passive authority-bearing object.
data LivePriorLabel
  = LivePriorLabel ReleasedLabelState (Maybe LiveAuthority)
  deriving stock (Eq, Show)

data LivePriorProblem
  = LivePriorStoredZombieNotPermitted ProcessEpochId
  deriving stock (Eq, Show)

liveOrdinaryPrior ::
  ReleasedLabelState ->
  Either LivePriorProblem LivePriorLabel
liveOrdinaryPrior state = checkedLivePrior state Nothing

liveAuthorityPrior ::
  ReleasedLabelState ->
  LiveAuthority ->
  Either LivePriorProblem LivePriorLabel
liveAuthorityPrior state authority =
  checkedLivePrior state (Just authority)

checkedLivePrior ::
  ReleasedLabelState ->
  Maybe LiveAuthority ->
  Either LivePriorProblem LivePriorLabel
checkedLivePrior state authority = case releasedLabelStateView state of
  ReleasedLabelView (ZombieLabel process, _) ->
    Left (LivePriorStoredZombieNotPermitted process)
  _ -> Right (LivePriorLabel state authority)

type LiveAuthorityDisposition = PreparedAuthorityDisposition

data LiveAuthorityDispositionView
  = LiveNoTargetAuthorityView
  | LiveRetainPriorAuthorityView LiveAuthority
  | LiveDeriveAuthorityAtReleaseView
  | LiveRetirePriorAuthorityView LiveAuthority
  deriving stock (Eq, Show)

data LivePreparedLabel = LivePreparedLabel
  { preparedPriorEffectiveState :: ReleasedLabelState,
    preparedOutcome :: ReleasedLabelState,
    preparedSameEffective :: Bool,
    preparedDisposition :: LiveAuthorityDisposition
  }
  deriving stock (Eq, Show)

data LiveLabelProblem
  = LivePriorDeleted
  | LiveExpectedLabelMismatch Label Label
  | LiveCallerProcessNotLive ProcessEpochId
  | LiveTargetProcessNotLive ProcessEpochId
  | LiveTransitionRejected LabelTransitionError
  deriving stock (Eq, Show)

-- | Check the value-CAS and complete canonical target/caller rule, then prepare its
-- authority consequence.  Name-capability checking remains a Herald-home fact;
-- this algebra rechecks the Oracle-owned caller and target liveness facts.
prepareLiveLabel ::
  LiveProcessTable ->
  ProcessEpochId ->
  Label ->
  LivePriorLabel ->
  LabelTarget ->
  Either LiveLabelProblem LivePreparedLabel
prepareLiveLabel processes caller expected (LivePriorLabel stored priorAuthority) target = do
  validateCallerLive processes caller
  priorLabel <- case releasedLabelStateView stored of
    ReleasedDeletedView _ -> Left LivePriorDeleted
    ReleasedLabelView label -> Right (liveEffectiveLabel processes label)
  validateTargetLive processes target
  let effectivePrior = releasedLabel priorLabel
  outcome <- case labelTransition caller expected effectivePrior target of
    Left problem -> Left (LiveTransitionRejected problem)
    Right LabelExpectedMismatch ->
      Left (LiveExpectedLabelMismatch expected priorLabel)
    Right (LabelTransitionApplied applied) -> Right applied
  let sameEffective = outcome == effectivePrior
      disposition =
        preparedAuthorityDisposition priorAuthority priorLabel outcome
  Right
    LivePreparedLabel
      { preparedPriorEffectiveState = effectivePrior,
        preparedOutcome = outcome,
        preparedSameEffective = sameEffective,
        preparedDisposition = disposition
      }

validateCallerLive ::
  LiveProcessTable ->
  ProcessEpochId ->
  Either LiveLabelProblem ()
validateCallerLive (LiveProcessTable processes) caller =
  case Map.lookup caller processes of
    Just LiveProcessLive -> Right ()
    _ -> Left (LiveCallerProcessNotLive caller)

validateTargetLive ::
  LiveProcessTable ->
  LabelTarget ->
  Either LiveLabelProblem ()
validateTargetLive (LiveProcessTable processes) target =
  case labelTargetView target of
    TargetProcessView process -> case Map.lookup process processes of
      Just LiveProcessLive -> Right ()
      _ -> Left (LiveTargetProcessNotLive process)
    TargetVoidView -> Right ()
    TargetDeleteView -> Right ()

livePreparedPriorEffectiveState ::
  LivePreparedLabel ->
  ReleasedLabelState
livePreparedPriorEffectiveState = preparedPriorEffectiveState

livePreparedOutcome :: LivePreparedLabel -> ReleasedLabelState
livePreparedOutcome = preparedOutcome

livePreparedSameEffective :: LivePreparedLabel -> Bool
livePreparedSameEffective = preparedSameEffective

livePreparedAuthorityDisposition ::
  LivePreparedLabel ->
  LiveAuthorityDispositionView
livePreparedAuthorityDisposition prepared =
  case preparedAuthorityDispositionView (preparedDisposition prepared) of
    NoTargetAuthorityView -> LiveNoTargetAuthorityView
    RetainPriorAuthorityView authority ->
      LiveRetainPriorAuthorityView authority
    DeriveAuthorityAtReleaseView ->
      LiveDeriveAuthorityAtReleaseView
    RetirePriorAuthorityView authority ->
      LiveRetirePriorAuthorityView authority

-- | Object-free overlay frozen into the atomic Applied decision.
-- The object is already fixed by the referenced decision; Oracle reconstructs
-- the full Domain 'LabelRecord' before installing it in canonical state.
data ReleasedLabelOverlay
  = ReleasedLabelOverlay
      ReleasedLabelState
      LabelRevision
      (Maybe AuthorityEpoch)
  deriving stock (Eq, Show)

type LiveLabelRecord = ReleasedLabelOverlay

data LiveReleaseProblem
  = LiveReleaseIndexMustBePositive
  deriving stock (Eq, Show)

-- | Release one opaque prepared transition at its exact positive control index.
releaseLiveLabel ::
  ControlIndex ->
  LivePreparedLabel ->
  Either LiveReleaseProblem LiveLabelRecord
releaseLiveLabel releaseIndex prepared = do
  revision <-
    case mkLabelRevision releaseIndex of
      Left _ -> Left LiveReleaseIndexMustBePositive
      Right checkedRevision -> Right checkedRevision
  Right
    ( ReleasedLabelOverlay
        (preparedOutcome prepared)
        revision
        (releaseAuthority releaseIndex (preparedDisposition prepared))
    )

releaseAuthority ::
  ControlIndex ->
  LiveAuthorityDisposition ->
  Maybe LiveAuthority
releaseAuthority index disposition =
  case preparedAuthorityDispositionView disposition of
    NoTargetAuthorityView -> Nothing
    RetainPriorAuthorityView authority -> Just authority
    DeriveAuthorityAtReleaseView -> Just (labelAuthorityEpoch index)
    RetirePriorAuthorityView authority -> Just authority

liveRecordStoredState :: LiveLabelRecord -> ReleasedLabelState
liveRecordStoredState (ReleasedLabelOverlay state _ _) = state

releasedLabelOverlayState :: ReleasedLabelOverlay -> ReleasedLabelState
releasedLabelOverlayState = liveRecordStoredState

liveRecordRevision :: LiveLabelRecord -> LabelRevision
liveRecordRevision (ReleasedLabelOverlay _ revision _) = revision

releasedLabelOverlayRevision :: ReleasedLabelOverlay -> LabelRevision
releasedLabelOverlayRevision = liveRecordRevision

liveRecordRetainedAuthority ::
  LiveLabelRecord ->
  Maybe LiveAuthority
liveRecordRetainedAuthority (ReleasedLabelOverlay _ _ authority) = authority

releasedLabelOverlayRetainedAuthority ::
  ReleasedLabelOverlay -> Maybe AuthorityEpoch
releasedLabelOverlayRetainedAuthority = liveRecordRetainedAuthority

-- | Return the retained tenure only when the effective record currently names
-- a known live process.  Void, zombie, and delete keep history but confer no
-- publication authority.
liveRecordApplicableAuthority ::
  LiveProcessTable ->
  LiveLabelRecord ->
  Maybe LiveAuthority
liveRecordApplicableAuthority processes@(LiveProcessTable processMap) record =
  case releasedLabelStateView
    (liveEffectiveReleasedState processes (liveRecordStoredState record)) of
    ReleasedLabelView (ProcessLabel process, _) -> case Map.lookup process processMap of
      Just LiveProcessLive -> liveRecordRetainedAuthority record
      _ -> Nothing
    _ -> Nothing

livePriorFromRecord :: LiveLabelRecord -> LivePriorLabel
livePriorFromRecord record =
  LivePriorLabel
    (liveRecordStoredState record)
    (liveRecordRetainedAuthority record)

-- | Honest provenance for one live structural consequence. Constructor order
-- is the canonical tag order: structural occurrence, label release, then
-- explicit process End.
data LiveConsequenceCause
  = LiveStructuralOccurrenceCause StructuralOccurrenceId
  | LiveLabelReleaseCause LabelDecisionId ControlIndex
  | LiveProcessEndCause ProcessEpochId ControlIndex
  deriving stock (Eq, Ord, Show)

data LiveConsequenceCauseView
  = LiveStructuralOccurrenceCauseView StructuralOccurrenceId
  | LiveLabelReleaseCauseView LabelDecisionId ControlIndex
  | LiveProcessEndCauseView ProcessEpochId ControlIndex
  deriving stock (Eq, Ord, Show)

data LiveConsequenceCauseProblem
  = LiveConsequenceControlIndexMustBePositive
  deriving stock (Eq, Show)

liveStructuralOccurrenceCause ::
  StructuralOccurrenceId ->
  LiveConsequenceCause
liveStructuralOccurrenceCause = LiveStructuralOccurrenceCause

liveLabelReleaseCause ::
  LabelDecisionId ->
  ControlIndex ->
  Either LiveConsequenceCauseProblem LiveConsequenceCause
liveLabelReleaseCause decision index
  | controlIndexWord64 index == 0 =
      Left LiveConsequenceControlIndexMustBePositive
  | otherwise = Right (LiveLabelReleaseCause decision index)

liveProcessEndCause ::
  ProcessEpochId ->
  ControlIndex ->
  Either LiveConsequenceCauseProblem LiveConsequenceCause
liveProcessEndCause process index
  | controlIndexWord64 index == 0 =
      Left LiveConsequenceControlIndexMustBePositive
  | otherwise = Right (LiveProcessEndCause process index)

liveConsequenceCauseView ::
  LiveConsequenceCause ->
  LiveConsequenceCauseView
liveConsequenceCauseView cause = case cause of
  LiveStructuralOccurrenceCause occurrence ->
    LiveStructuralOccurrenceCauseView occurrence
  LiveLabelReleaseCause decision index ->
    LiveLabelReleaseCauseView decision index
  LiveProcessEndCause process index ->
    LiveProcessEndCauseView process index

-- | Future canonical constructor tag, kept explicit so source reordering cannot
-- silently renumber the private live vocabulary.
liveConsequenceCauseTag :: LiveConsequenceCause -> Word8
liveConsequenceCauseTag cause = case cause of
  LiveStructuralOccurrenceCause _ -> 0
  LiveLabelReleaseCause _ _ -> 1
  LiveProcessEndCause _ _ -> 2

-- | Stable private transcript for one consequence provenance value.  This is
-- an observation seam for pre-wire fixtures, not a decoder contract.
liveConsequenceCauseCanonicalBytes ::
  LiveConsequenceCause -> ByteString
liveConsequenceCauseCanonicalBytes cause = SerializePut.runPut $ do
  SerializePut.putWord8 (liveConsequenceCauseTag cause)
  case cause of
    LiveStructuralOccurrenceCause occurrence -> do
      SerializePut.putByteString
        (heraldEpochBytes (structuralOccurrenceSourceHeraldEpoch occurrence))
      SerializePut.putWord64be
        (structuralSequenceWord64 (structuralOccurrenceSourceSequence occurrence))
    LiveLabelReleaseCause decision index -> do
      SerializePut.putByteString (labelDecisionIdBytes decision)
      SerializePut.putWord64be (controlIndexWord64 index)
    LiveProcessEndCause process index -> do
      SerializePut.putByteString (processEpochIdBytes process)
      SerializePut.putWord64be (controlIndexWord64 index)

data LiveWorkflowPhaseView
  = LiveReleasedView
  | LiveCompletedNotAppliedView
  | LiveCompletedReleasedView
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- Complete private normalized evidence ---------------------------------------

type LivePreparedLabelFacts =
  PreparedLabelFacts

livePreparedFactsDecisionId ::
  LivePreparedLabelFacts -> LabelDecisionId
livePreparedFactsDecisionId = preparedLabelFactsDecisionId

livePreparedFactsResolveIndex ::
  LivePreparedLabelFacts -> ControlIndex
livePreparedFactsResolveIndex = preparedLabelFactsResolveIndex

livePreparedFactsObjectId ::
  LivePreparedLabelFacts -> GlobalObjectId
livePreparedFactsObjectId = preparedLabelFactsObjectId

livePreparedFactsProposedOutcome ::
  LivePreparedLabelFacts -> ReleasedLabelState
livePreparedFactsProposedOutcome = preparedLabelFactsProposedOutcome

livePreparedFactsExpectedPriorState ::
  LivePreparedLabelFacts -> ReleasedLabelState
livePreparedFactsExpectedPriorState = preparedLabelFactsExpectedPriorState

livePreparedFactsExpectedPriorRevision ::
  LivePreparedLabelFacts -> Maybe LabelRevision
livePreparedFactsExpectedPriorRevision = preparedLabelFactsExpectedPriorRevision

livePreparedFactsCatalogueDigest ::
  LivePreparedLabelFacts -> CatalogueDigest
livePreparedFactsCatalogueDigest = preparedLabelFactsCatalogueDigest

livePreparedFactsExpectedPriorAuthority ::
  LivePreparedLabelFacts -> Maybe LiveAuthority
livePreparedFactsExpectedPriorAuthority = preparedLabelFactsExpectedPriorAuthority

livePreparedFactsPriorAuthorityJustification ::
  LivePreparedLabelFacts -> Maybe PriorAuthorityJustification
livePreparedFactsPriorAuthorityJustification =
  preparedLabelFactsPriorAuthorityJustification

livePreparedFactsAuthorityDisposition ::
  LivePreparedLabelFacts -> LiveAuthorityDispositionView
livePreparedFactsAuthorityDisposition =
  liveDispositionView . preparedLabelFactsAuthorityDisposition

livePreparedFactsMemberSetDigest ::
  LivePreparedLabelFacts -> MemberSetDigest
livePreparedFactsMemberSetDigest = preparedLabelFactsMemberSetDigest

deriveLivePreparedLabelDigest ::
  LivePreparedLabelFacts -> PreparedLabelDigest
deriveLivePreparedLabelDigest = derivePreparedLabelDigest

-- | Exact digest preimage retained by every private Prepared report.
livePreparedLabelFactsCanonicalBytes ::
  LivePreparedLabelFacts -> ByteString
livePreparedLabelFactsCanonicalBytes = preparedLabelFactsCanonicalBytes

data LiveNotAppliedReason
  = LiveCallerProcessEnded ProcessEpochId
  | LiveTargetProcessEnded ProcessEpochId
  | LivePriorLabelChanged
  | LiveTransitionNoLongerPermitted
  deriving stock (Eq, Ord, Show)

data LiveTerminalOutcome
  = LiveNotAppliedTerminal
      LabelDecisionId
      ControlIndex
      GlobalObjectId
      LiveNotAppliedReason
      CatalogueDigest
      MemberSetDigest
  | LiveReleasedTerminal
      PreparedLabelFacts
      PreparedLabelDigest
      ControlIndex
      ReleasedLabelOverlay
  deriving stock (Eq, Show)

data LiveTerminalOutcomeProblem
  = LiveTerminalControlIndexMustBePositive
  deriving stock (Eq, Show)

-- | Checked constructor used by the canonical fixture boundary to pin every
-- NotApplied reason inside the complete terminal transcript.  Live Oracle
-- transitions derive the same value from an admitted positive control index.
liveNotAppliedTerminalOutcome ::
  LabelDecisionId ->
  ControlIndex ->
  GlobalObjectId ->
  LiveNotAppliedReason ->
  CatalogueDigest ->
  MemberSetDigest ->
  Either LiveTerminalOutcomeProblem LiveTerminalOutcome
liveNotAppliedTerminalOutcome decision index object reason catalogue members
  | controlIndexWord64 index == 0 =
      Left LiveTerminalControlIndexMustBePositive
  | otherwise =
      Right
        ( LiveNotAppliedTerminal
            decision
            index
            object
            reason
            catalogue
            members
        )

-- | Reconstruct the unique released terminal carried by checked preparation
-- facts. The positive coordinate, successor generation and authority disposition
-- already belong to the Domain-owned facts; no process lifecycle or prior owner
-- history is replayed here.
liveReleasedTerminalOutcomeFromFacts :: PreparedLabelFacts -> LiveTerminalOutcome
liveReleasedTerminalOutcomeFromFacts facts =
  LiveReleasedTerminal facts (deriveLivePreparedLabelDigest facts) index overlay
  where
    index = preparedLabelFactsResolveIndex facts
    revision = case mkLabelRevision index of
      Right checkedRevision -> checkedRevision
      Left problem -> error ("checked prepared label coordinate invariant: " <> show problem)
    overlay =
      ReleasedLabelOverlay
        (preparedLabelFactsProposedOutcome facts)
        revision
        (releaseAuthority index (preparedLabelFactsAuthorityDisposition facts))

data LiveTerminalOutcomeView
  = LiveNotAppliedOutcomeView
      LabelDecisionId
      ControlIndex
      GlobalObjectId
      LiveNotAppliedReason
      CatalogueDigest
      MemberSetDigest
  | LiveReleasedOutcomeView
      LivePreparedLabelFacts
      PreparedLabelDigest
      ControlIndex
      LiveLabelRecord
  deriving stock (Eq, Show)

liveTerminalOutcomeView ::
  LiveTerminalOutcome -> LiveTerminalOutcomeView
liveTerminalOutcomeView outcome = case outcome of
  LiveNotAppliedTerminal decision index object reason catalogue members ->
    LiveNotAppliedOutcomeView
      decision
      index
      object
      reason
      catalogue
      members
  LiveReleasedTerminal facts digest index overlay ->
    LiveReleasedOutcomeView
      facts
      digest
      index
      overlay

-- | Exact digest preimage for either terminal outcome arm.
liveTerminalOutcomeCanonicalBytes ::
  LiveTerminalOutcome -> ByteString
liveTerminalOutcomeCanonicalBytes outcome =
  canonicalTerminalOutcomeBytes
    liveAuthorityCanonicalBytes
    liveDispositionEncoding
    liveNotAppliedReasonEncoding
    (normalizedTerminalOutcome outcome)

normalizedTerminalOutcome ::
  LiveTerminalOutcome ->
  NormalizedTerminalOutcome AuthorityEpoch PreparedAuthorityDisposition LiveNotAppliedReason
normalizedTerminalOutcome outcome = case outcome of
  LiveNotAppliedTerminal decision index object reason catalogue members ->
    NormalizedNotApplied decision index object reason catalogue members
  LiveReleasedTerminal facts digest index overlay ->
    NormalizedReleased
      (normalizedPreparedFacts facts)
      digest
      index
      (liveRecordStoredState overlay)
      (liveRecordRevision overlay)
      (liveRecordRetainedAuthority overlay)

normalizedPreparedFacts ::
  PreparedLabelFacts ->
  NormalizedPreparedFacts AuthorityEpoch PreparedAuthorityDisposition
normalizedPreparedFacts facts =
  NormalizedPreparedFacts
    (preparedLabelFactsDecisionId facts)
    (preparedLabelFactsResolveIndex facts)
    (preparedLabelFactsObjectId facts)
    (preparedLabelFactsProposedOutcome facts)
    (preparedLabelFactsExpectedPriorState facts)
    (preparedLabelFactsExpectedPriorRevision facts)
    (preparedLabelFactsCatalogueDigest facts)
    (preparedLabelFactsExpectedPriorAuthority facts)
    (preparedLabelFactsPriorAuthorityJustification facts)
    (preparedLabelFactsAuthorityDisposition facts)
    (preparedLabelFactsMemberSetDigest facts)

-- | Derive common terminal evidence only from the complete private typed sum.
deriveLabelOutcomeDigest :: LiveTerminalOutcome -> LabelOutcomeDigest
deriveLabelOutcomeDigest =
  checkedOutcomeDigest
    . SHA256.hash
    . liveTerminalOutcomeCanonicalBytes

checkedOutcomeDigest :: ByteString -> LabelOutcomeDigest
checkedOutcomeDigest bytes = case mkLabelOutcomeDigest bytes of
  Right digest -> digest
  Left problem -> error ("label outcome digest invariant: " <> show problem)

liveAuthorityCanonicalBytes :: LiveAuthority -> ByteString
liveAuthorityCanonicalBytes = authorityEpochCanonicalBytes

liveDispositionEncoding ::
  LiveAuthorityDisposition -> (Word8, ByteString)
liveDispositionEncoding disposition = case preparedAuthorityDispositionView disposition of
  NoTargetAuthorityView -> (0, ByteString.empty)
  RetainPriorAuthorityView authority ->
    (1, liveAuthorityCanonicalBytes authority)
  DeriveAuthorityAtReleaseView -> (2, ByteString.empty)
  RetirePriorAuthorityView authority ->
    (3, liveAuthorityCanonicalBytes authority)

liveDispositionView ::
  LiveAuthorityDisposition -> LiveAuthorityDispositionView
liveDispositionView disposition = case preparedAuthorityDispositionView disposition of
  NoTargetAuthorityView -> LiveNoTargetAuthorityView
  RetainPriorAuthorityView authority ->
    LiveRetainPriorAuthorityView authority
  DeriveAuthorityAtReleaseView ->
    LiveDeriveAuthorityAtReleaseView
  RetirePriorAuthorityView authority ->
    LiveRetirePriorAuthorityView authority

liveNotAppliedReasonEncoding ::
  LiveNotAppliedReason -> (Word8, ByteString)
liveNotAppliedReasonEncoding reason =
  (liveNotAppliedReasonTag reason, payload)
  where
    payload = case reason of
      LiveCallerProcessEnded process -> processEpochIdBytes process
      LiveTargetProcessEnded process -> processEpochIdBytes process
      LivePriorLabelChanged -> ByteString.empty
      LiveTransitionNoLongerPermitted -> ByteString.empty

-- | Stable constructor tag for all NotApplied reasons.
liveNotAppliedReasonTag :: LiveNotAppliedReason -> Word8
liveNotAppliedReasonTag reason = case reason of
  LiveCallerProcessEnded _ -> 0
  LiveTargetProcessEnded _ -> 1
  LivePriorLabelChanged -> 2
  LiveTransitionNoLongerPermitted -> 5

-- | Exact tagged reason transcript embedded by a NotApplied terminal outcome.
liveNotAppliedReasonCanonicalBytes ::
  LiveNotAppliedReason -> ByteString
liveNotAppliedReasonCanonicalBytes =
  SerializePut.runPut . putLiveNotAppliedReason

-- Complete private Oracle kernel ---------------------------------------------

data ProcessOrigin
  = GenesisProcess
  | DynamicStart ControlIndex OracleClientRequestId
  deriving stock (Eq, Ord, Show)

data ProcessOriginView
  = GenesisProcessView
  | DynamicStartView ControlIndex OracleClientRequestId
  deriving stock (Eq, Ord, Show)

data ProcessLifecycle
  = ProcessRecordLive
  | ProcessRecordEnded ControlIndex ProcessEndReason
  deriving stock (Eq, Show)

data ProcessLifecycleView
  = ProcessRecordLiveView
  | ProcessRecordEndedView ControlIndex ProcessEndReason
  deriving stock (Eq, Show)

data ProcessEpochRecord
  = ProcessEpochRecord
      ProcessId
      ProcessEpochId
      HeraldEpoch
      ProcessOrigin
      ProcessLifecycle
  deriving stock (Eq, Show)

processRecordProcessId :: ProcessEpochRecord -> ProcessId
processRecordProcessId
  (ProcessEpochRecord processId _ _ _ _) = processId

processRecordProcessEpoch ::
  ProcessEpochRecord -> ProcessEpochId
processRecordProcessEpoch
  (ProcessEpochRecord _ process _ _ _) = process

processRecordResidence ::
  ProcessEpochRecord -> HeraldEpoch
processRecordResidence
  (ProcessEpochRecord _ _ residence _ _) = residence

processRecordOrigin ::
  ProcessEpochRecord -> ProcessOriginView
processRecordOrigin
  (ProcessEpochRecord _ _ _ origin _) = case origin of
    GenesisProcess -> GenesisProcessView
    DynamicStart index manifest ->
      DynamicStartView index manifest

processRecordLifecycle ::
  ProcessEpochRecord -> ProcessLifecycleView
processRecordLifecycle
  (ProcessEpochRecord _ _ _ _ lifecycle) = case lifecycle of
    ProcessRecordLive -> ProcessRecordLiveView
    ProcessRecordEnded index reason ->
      ProcessRecordEndedView index reason

data LiveLabelDecision
  = LiveLabelDecision
      LabelDecisionId
      OracleClientRequestId
      HeraldEpoch
      ProcessEpochId
      GlobalObjectId
      Label
      (Maybe InitialLabelEvidence)
      (Maybe LabelRevision)
      (Maybe LiveAuthority)
      (Maybe PriorAuthorityJustification)
      LabelTarget
      ControlIndex
      HomeLabelAcceptanceCut
      MemberSetDigest
      HeraldMembershipGenerationId
  deriving stock (Eq, Show)

liveDecisionId :: LiveLabelDecision -> LabelDecisionId
liveDecisionId
  (LiveLabelDecision decision _ _ _ _ _ _ _ _ _ _ _ _ _ _) = decision

liveDecisionOpenRequestId ::
  LiveLabelDecision -> OracleClientRequestId
liveDecisionOpenRequestId
  (LiveLabelDecision _ requestId _ _ _ _ _ _ _ _ _ _ _ _ _) = requestId

liveDecisionHome :: LiveLabelDecision -> HeraldEpoch
liveDecisionHome
  (LiveLabelDecision _ _ home _ _ _ _ _ _ _ _ _ _ _ _) = home

liveDecisionCaller :: LiveLabelDecision -> ProcessEpochId
liveDecisionCaller
  (LiveLabelDecision _ _ _ caller _ _ _ _ _ _ _ _ _ _ _) = caller

liveDecisionObject :: LiveLabelDecision -> GlobalObjectId
liveDecisionObject
  (LiveLabelDecision _ _ _ _ object _ _ _ _ _ _ _ _ _ _) = object

liveDecisionExpectedLabel :: LiveLabelDecision -> Label
liveDecisionExpectedLabel
  (LiveLabelDecision _ _ _ _ _ expected _ _ _ _ _ _ _ _ _) = expected

liveDecisionInitialLabelEvidence ::
  LiveLabelDecision -> Maybe InitialLabelEvidence
liveDecisionInitialLabelEvidence
  (LiveLabelDecision _ _ _ _ _ _ evidence _ _ _ _ _ _ _ _) = evidence

liveDecisionExpectedPriorRevision ::
  LiveLabelDecision -> Maybe LabelRevision
liveDecisionExpectedPriorRevision
  (LiveLabelDecision _ _ _ _ _ _ _ revision _ _ _ _ _ _ _) = revision

liveDecisionExpectedPriorAuthority ::
  LiveLabelDecision -> Maybe LiveAuthority
liveDecisionExpectedPriorAuthority
  (LiveLabelDecision _ _ _ _ _ _ _ _ authority _ _ _ _ _ _) = authority

liveDecisionPriorAuthorityJustification ::
  LiveLabelDecision -> Maybe PriorAuthorityJustification
liveDecisionPriorAuthorityJustification
  (LiveLabelDecision _ _ _ _ _ _ _ _ _ justification _ _ _ _ _) = justification

liveDecisionTarget :: LiveLabelDecision -> LabelTarget
liveDecisionTarget
  (LiveLabelDecision _ _ _ _ _ _ _ _ _ _ target _ _ _ _) = target

liveDecisionRequestControlIndex ::
  LiveLabelDecision -> ControlIndex
liveDecisionRequestControlIndex
  (LiveLabelDecision _ _ _ _ _ _ _ _ _ _ _ index _ _ _) = index

liveDecisionHomeAcceptanceCut ::
  LiveLabelDecision -> HomeLabelAcceptanceCut
liveDecisionHomeAcceptanceCut
  (LiveLabelDecision _ _ _ _ _ _ _ _ _ _ _ _ cut _ _) = cut

-- | Resolve the membership reference through checked canonical history.
liveDecisionCapturedHeralds :: HeraldMembershipHistory -> LiveLabelDecision -> Maybe [HeraldEpoch]
liveDecisionCapturedHeralds history decision = do
  generation <- Membership.lookupHeraldMembershipGeneration (liveDecisionMembershipGeneration decision) history
  let captured = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs generation)
  if memberDigestFor captured == liveDecisionMemberSetDigest decision
    then Just captured
    else Nothing

liveDecisionMemberSetDigest ::
  LiveLabelDecision -> MemberSetDigest
liveDecisionMemberSetDigest
  (LiveLabelDecision _ _ _ _ _ _ _ _ _ _ _ _ _ digest _) = digest

liveDecisionMembershipGeneration ::
  LiveLabelDecision -> HeraldMembershipGenerationId
liveDecisionMembershipGeneration
  (LiveLabelDecision _ _ _ _ _ _ _ _ _ _ _ _ _ _ generation) = generation

-- | Compact assertion produced after the assigned Herald has collected every
-- captured survivor's installation fact. The benign-deployment model trusts the
-- checked collector; individual peer reports never enter replicated state.
data LabelCompletionAttestation
  = LabelCompletionAttestation
      LabelDecisionId
      ControlIndex
      LabelOutcomeDigest
      HeraldEpoch
      HeraldMembershipGenerationId
  deriving stock (Eq, Show)

labelCompletionAttestation ::
  LabelDecisionId ->
  ControlIndex ->
  LabelOutcomeDigest ->
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  LabelCompletionAttestation
labelCompletionAttestation = LabelCompletionAttestation

labelCompletionDecisionId :: LabelCompletionAttestation -> LabelDecisionId
labelCompletionDecisionId (LabelCompletionAttestation decision _ _ _ _) = decision

labelCompletionControlIndex :: LabelCompletionAttestation -> ControlIndex
labelCompletionControlIndex (LabelCompletionAttestation _ index _ _ _) = index

labelCompletionOutcomeDigest :: LabelCompletionAttestation -> LabelOutcomeDigest
labelCompletionOutcomeDigest (LabelCompletionAttestation _ _ digest _ _) = digest

labelCompletionCollector :: LabelCompletionAttestation -> HeraldEpoch
labelCompletionCollector (LabelCompletionAttestation _ _ _ collector _) = collector

labelCompletionMembershipGeneration :: LabelCompletionAttestation -> HeraldMembershipGenerationId
labelCompletionMembershipGeneration (LabelCompletionAttestation _ _ _ _ generation) = generation

data OracleCommand
  = RetireOracleProgress OracleProgress
  | VoterCommand V.VoterCommand
  | AdmissionCommand A.HeraldAdmissionCommand
  | StartProcessEpoch ProcessStart
  | EndProcessEpoch ProcessEpochId ProcessEndReason
  | DecideLabel
      LabelDecisionId
      ProcessEpochId
      GlobalObjectId
      Label
      (Maybe InitialLabelEvidence)
      (Maybe LiveAuthority)
      (Maybe PriorAuthorityJustification)
      LabelTarget
      HomeLabelAcceptanceCut
  | CompleteLabelDecision LabelCompletionAttestation
  | OpenHeraldFailureProbe
      HeraldEpoch
      HeraldMembershipGenerationId
      V.VoterConfigurationId
  | ReportHeraldFailureProbe
      HeraldFailureProbeId
      V.VoterConfigurationId
      ProbeResult
  | AcceptVoterHostFailure FailureProbeResolutionId
  | DismissHeraldFailureProbe FailureProbeResolutionId
  | RetireHeraldEpoch
      FailureProbeResolutionId
      HeraldEpoch
  | OpenDisappearanceProbe DD.DisappearanceSubject DD.DisappearanceSubjectMembershipCoordinate
  | ReportPredefinedAbsence DD.DisappearanceProbeId DD.DisappearanceEvidenceClaim
  | InvalidateDisappearanceProbe DD.DisappearanceProbeId HeraldEpoch D.DisappearanceInvalidationReason DD.DisappearanceEvidenceDigest
  | ResolveDisappearanceProbe DD.DisappearanceProbeId D.CompleteDisappearanceEvidenceDigest
  | AbortDisappearanceProbe DD.DisappearanceProbeId D.DisappearanceAbortReason
  deriving stock (Eq, Show)

-- | The home promises that no request at or below this sequence needs its
-- Oracle receipt again. This is maintenance, with no request receipt of its own.
retireOracleReceiptsCommand :: Word64 -> OracleCommand
retireOracleReceiptsCommand = retireOracleReceiptProgressCommand . Lifetime.receiptRetirementPrefix . Just

retireOracleReceiptProgressCommand :: ReceiptRetirement -> OracleCommand
retireOracleReceiptProgressCommand receipts = retireOracleProgressCommand (oracleProgress receipts (controlIndex 0))

retireOracleProgressCommand :: OracleProgress -> OracleCommand
retireOracleProgressCommand = RetireOracleProgress

oracleCommandProgress :: OracleCommand -> Maybe OracleProgress
oracleCommandProgress (RetireOracleProgress progress) = Just progress
oracleCommandProgress _ = Nothing

oracleCommandReceiptRetirement :: OracleCommand -> Maybe Word64
oracleCommandReceiptRetirement = fmap retirementHighWater . oracleCommandReceiptRetirementProgress

oracleCommandReceiptRetirementProgress :: OracleCommand -> Maybe ReceiptRetirement
oracleCommandReceiptRetirementProgress = fmap oracleProgressReceipts . oracleCommandProgress

heraldAdmissionCommand :: A.HeraldAdmissionCommand -> OracleCommand
heraldAdmissionCommand = AdmissionCommand

beginHeraldAdmissionCommand :: A.HeraldAdmissionManifest -> Eclips.Domain.Topology.TopologyCut -> OracleCommand
beginHeraldAdmissionCommand manifest = AdmissionCommand . A.BeginHeraldAdmission manifest
sealHeraldAdmissionCommand :: A.HeraldJoinSeal -> OracleCommand
sealHeraldAdmissionCommand = AdmissionCommand . A.SealHeraldAdmission
acceptHeraldJoinSealCommand :: HeraldAdmissionId -> Word64 -> A.HeraldJoinDigest -> OracleCommand
acceptHeraldJoinSealCommand ident attempt = AdmissionCommand . A.AcceptHeraldJoinSeal ident attempt
reportHeraldJoinBaseReadyCommand :: A.HeraldJoinReadyReport -> OracleCommand
reportHeraldJoinBaseReadyCommand = AdmissionCommand . A.HeraldJoinBaseReady
reportHeraldJoinReadyCommand :: A.HeraldJoinReadyReport -> OracleCommand
reportHeraldJoinReadyCommand = AdmissionCommand . A.HeraldJoinReady
activateHeraldCommand :: HeraldAdmissionId -> OracleCommand
activateHeraldCommand = AdmissionCommand . A.ActivateHerald
cancelHeraldAdmissionCommand :: HeraldAdmissionId -> OracleCommand
cancelHeraldAdmissionCommand = AdmissionCommand . A.CancelHeraldAdmission
oracleCommandAdmission :: OracleCommand -> Maybe A.HeraldAdmissionCommand
oracleCommandAdmission (AdmissionCommand command) = Just command
oracleCommandAdmission _ = Nothing

data EndProcessCommandProblem
  = EndReasonNotHeraldSubmittable ProcessEndReason
  deriving stock (Eq, Show)

startProcessEpochCommand ::
  ProcessStart -> OracleCommand
startProcessEpochCommand = StartProcessEpoch

endProcessEpochCommand ::
  ProcessEpochId -> ProcessEndReason -> Either EndProcessCommandProblem OracleCommand
endProcessEpochCommand process reason
  | processEndReasonMayBeHeraldSubmitted reason =
      Right (EndProcessEpoch process reason)
  | otherwise = Left (EndReasonNotHeraldSubmittable reason)

decideLabelCommand ::
  LabelDecisionId ->
  ProcessEpochId ->
  GlobalObjectId ->
  Label ->
  Maybe InitialLabelEvidence ->
  Maybe LiveAuthority ->
  Maybe PriorAuthorityJustification ->
  LabelTarget ->
  HomeLabelAcceptanceCut ->
  OracleCommand
decideLabelCommand = DecideLabel

completeLabelDecisionCommand ::
  LabelCompletionAttestation -> OracleCommand
completeLabelDecisionCommand = CompleteLabelDecision

openHeraldFailureProbeCommand ::
  HeraldEpoch -> HeraldMembershipGenerationId -> V.VoterConfigurationId -> OracleCommand
openHeraldFailureProbeCommand = OpenHeraldFailureProbe

reportHeraldFailureProbeCommand ::
  HeraldFailureProbeId -> V.VoterConfigurationId -> ProbeResult -> OracleCommand
reportHeraldFailureProbeCommand = ReportHeraldFailureProbe

acceptVoterHostFailureCommand :: FailureProbeResolutionId -> OracleCommand
acceptVoterHostFailureCommand = AcceptVoterHostFailure

dismissHeraldFailureProbeCommand ::
  FailureProbeResolutionId -> OracleCommand
dismissHeraldFailureProbeCommand = DismissHeraldFailureProbe

retireHeraldEpochCommand ::
  FailureProbeResolutionId -> HeraldEpoch -> OracleCommand
retireHeraldEpochCommand = RetireHeraldEpoch

openDisappearanceProbeCommand :: DD.DisappearanceSubject -> DD.DisappearanceSubjectMembershipCoordinate -> OracleCommand
openDisappearanceProbeCommand = OpenDisappearanceProbe

reportPredefinedAbsenceCommand :: DD.DisappearanceProbeId -> DD.DisappearanceEvidenceClaim -> OracleCommand
reportPredefinedAbsenceCommand = ReportPredefinedAbsence

invalidateDisappearanceProbeCommand :: DD.DisappearanceProbeId -> HeraldEpoch -> D.DisappearanceInvalidationReason -> DD.DisappearanceEvidenceDigest -> OracleCommand
invalidateDisappearanceProbeCommand = InvalidateDisappearanceProbe

resolveDisappearanceProbeCommand :: DD.DisappearanceProbeId -> D.CompleteDisappearanceEvidenceDigest -> OracleCommand
resolveDisappearanceProbeCommand = ResolveDisappearanceProbe

abortDisappearanceProbeCommand :: DD.DisappearanceProbeId -> D.DisappearanceAbortReason -> OracleCommand
abortDisappearanceProbeCommand = AbortDisappearanceProbe

fromDisappearanceCommand :: D.DisappearanceCommand -> OracleCommand
fromDisappearanceCommand command = case command of
  D.DisappearanceOpen x0 x1 -> OpenDisappearanceProbe x0 x1
  D.DisappearanceReport x0 x1 -> ReportPredefinedAbsence x0 x1
  D.DisappearanceInvalidate x0 x1 x2 x3 -> InvalidateDisappearanceProbe x0 x1 x2 x3
  D.DisappearanceResolve x0 x1 -> ResolveDisappearanceProbe x0 x1
  D.DisappearanceAbort x0 x1 -> AbortDisappearanceProbe x0 x1

-- | Observe the minimal dynamic process fact carried by the Start arm without
-- exposing constructible command constructors.
oracleCommandStart ::
  OracleCommand -> Maybe ProcessStart
oracleCommandStart command = case command of
  StartProcessEpoch bootstrap -> Just bootstrap
  _ -> Nothing

oracleCommandTag :: OracleCommand -> Word8
oracleCommandTag command = case command of
  StartProcessEpoch _ -> 0
  EndProcessEpoch _ _ -> 1
  DecideLabel {} -> 2
  CompleteLabelDecision _ -> 8
  OpenHeraldFailureProbe {} -> 9
  ReportHeraldFailureProbe {} -> 10
  DismissHeraldFailureProbe {} -> 11
  RetireHeraldEpoch {} -> 12
  OpenDisappearanceProbe {} -> 13
  ReportPredefinedAbsence {} -> 14
  InvalidateDisappearanceProbe {} -> 15
  ResolveDisappearanceProbe {} -> 16
  AbortDisappearanceProbe {} -> 17
  AdmissionCommand {} -> 18
  VoterCommand {} -> 19
  AcceptVoterHostFailure {} -> 20
  RetireOracleProgress {} -> 21

-- | Stable command transcript used verbatim inside the private envelope.
-- Keeping the tag outside the framed constructor payload makes all current arms
-- independently inspectable without admitting a wire decoder.
oracleCommandCanonicalBytes :: OracleCommand -> ByteString
oracleCommandCanonicalBytes command = SerializePut.runPut $ do
  SerializePut.putWord8 (oracleCommandTag command)
  putFramedBytes (liveCommandPayloadBytes command)

-- | Failures admitted by the checked live canonical boundary.
data OracleCanonicalError
  = MalformedCanonicalOracleBytes String
  | WrongCanonicalOracleDomain ByteString
  | UnknownOracleCommandTag Word8
  | UnknownOracleRejectionTag Word8
  | UnknownProjectionEventTag Word8
  | DuplicateProjectionEventTag Word8
  | InvalidCanonicalOracleTag Word8
  | InconsistentCanonicalOracleEntry
  deriving stock (Eq, Show)

data DecodedOracleEnvelope
  = DecodedOracleEnvelope OracleEnvelope ByteString
  deriving stock (Eq, Show)

decodedOracleEnvelopeValue ::
  DecodedOracleEnvelope -> OracleEnvelope
decodedOracleEnvelopeValue (DecodedOracleEnvelope envelope _) = envelope

decodedOracleEnvelopeCanonicalBytes ::
  DecodedOracleEnvelope -> ByteString
decodedOracleEnvelopeCanonicalBytes
  (DecodedOracleEnvelope _ bytes) = bytes

data DecodedOracleReceipt
  = DecodedOracleReceipt OracleReceipt ByteString
  deriving stock (Eq, Show)

decodedOracleReceiptValue ::
  DecodedOracleReceipt -> OracleReceipt
decodedOracleReceiptValue (DecodedOracleReceipt receipt _) = receipt

decodedOracleReceiptCanonicalBytes ::
  DecodedOracleReceipt -> ByteString
decodedOracleReceiptCanonicalBytes
  (DecodedOracleReceipt _ bytes) = bytes

data DecodedOracleProjectionEventVector
  = DecodedOracleProjectionEventVector [OracleProjectionEvent] ByteString
  deriving stock (Eq, Show)

decodedOracleProjectionEventVectorValue ::
  DecodedOracleProjectionEventVector -> [OracleProjectionEvent]
decodedOracleProjectionEventVectorValue
  (DecodedOracleProjectionEventVector events _) = events

decodedOracleProjectionEventVectorCanonicalBytes ::
  DecodedOracleProjectionEventVector -> ByteString
decodedOracleProjectionEventVectorCanonicalBytes
  (DecodedOracleProjectionEventVector _ bytes) = bytes

data DecodedAppliedOracleEntry
  = DecodedAppliedOracleEntry AppliedOracleEntry ByteString
  deriving stock (Eq, Show)

decodedAppliedOracleEntryValue ::
  DecodedAppliedOracleEntry -> AppliedOracleEntry
decodedAppliedOracleEntryValue (DecodedAppliedOracleEntry entry _) = entry

decodedAppliedOracleEntryCanonicalBytes ::
  DecodedAppliedOracleEntry -> ByteString
decodedAppliedOracleEntryCanonicalBytes
  (DecodedAppliedOracleEntry _ bytes) = bytes

liveEnvelopeDomain :: ByteString
liveEnvelopeDomain = "ECLIPS-ORACLE-ENVELOPE"

liveReceiptDomain :: ByteString
liveReceiptDomain = "ECLIPS-ORACLE-RECEIPT"

liveProjectionEventVectorDomain :: ByteString
liveProjectionEventVectorDomain =
  "ECLIPS-ORACLE-PROJECTION-EVENT-VECTOR"

liveAppliedEntryDomain :: ByteString
liveAppliedEntryDomain = "ECLIPS-APPLIED-ORACLE-ENTRY"

liveStateDomain :: ByteString
liveStateDomain = "ECLIPS-ORACLE-STATE"

data OracleEnvelope
  = OracleEnvelope
      OracleClientRequestId
      (Maybe ControlIndex)
      HeraldEpoch
      OracleCommand
      (Maybe OracleProgress)
  deriving stock (Eq, Show)

-- | Attach coalescible receipt-lifetime metadata without changing semantic
-- request identity. A prefix must be strictly below this envelope's request ID.
oracleEnvelopeWithReceiptRetirement :: Word64 -> OracleEnvelope -> OracleEnvelope
oracleEnvelopeWithReceiptRetirement = oracleEnvelopeWithReceiptRetirementProgress . Lifetime.receiptRetirementPrefix . Just

oracleEnvelopeWithReceiptRetirementProgress :: ReceiptRetirement -> OracleEnvelope -> OracleEnvelope
oracleEnvelopeWithReceiptRetirementProgress receipts envelope =
  oracleEnvelopeWithProgress (oracleProgress receipts (maybe (controlIndex 0) oracleProgressLabelsThrough (oracleEnvelopeProgress envelope))) envelope

oracleEnvelopeWithProgress :: OracleProgress -> OracleEnvelope -> OracleEnvelope
oracleEnvelopeWithProgress progress (OracleEnvelope request expected home command _) =
  OracleEnvelope request expected home command (Just progress)

oracleEnvelopeProgress :: OracleEnvelope -> Maybe OracleProgress
oracleEnvelopeProgress (OracleEnvelope _ _ _ _ progress) = progress

oracleEnvelopeReceiptRetirement :: OracleEnvelope -> Maybe Word64
oracleEnvelopeReceiptRetirement = fmap retirementHighWater . oracleEnvelopeReceiptRetirementProgress

oracleEnvelopeReceiptRetirementProgress :: OracleEnvelope -> Maybe ReceiptRetirement
oracleEnvelopeReceiptRetirementProgress = fmap oracleProgressReceipts . oracleEnvelopeProgress

oracleEnvelope ::
  OracleClientRequestId ->
  Maybe ControlIndex ->
  HeraldEpoch ->
  OracleCommand ->
  OracleEnvelope
oracleEnvelope requestId expected home command =
  OracleEnvelope requestId normalizedExpected home command Nothing
  where
    -- Lifecycle and maintenance commands validate their own semantic ordering.
    -- Unrelated control progress must not stale their exact retained envelope.
    normalizedExpected = case command of
      EndProcessEpoch {} -> Nothing
      DismissHeraldFailureProbe {} -> Nothing
      AcceptVoterHostFailure {} -> Nothing
      RetireHeraldEpoch {} -> Nothing
      RetireOracleProgress {} -> Nothing
      _ -> expected

oracleEnvelopeRequestId ::
  OracleEnvelope -> OracleClientRequestId
oracleEnvelopeRequestId
  (OracleEnvelope requestId _ _ _ _) = requestId

-- | Exact wire transcript, including optional progress metadata. The semantic
-- digest excludes that metadata so retries can advance the retirement frontier.
oracleEnvelopeCanonicalBytes :: OracleEnvelope -> ByteString
oracleEnvelopeCanonicalBytes = liveEnvelopeBytes

oracleEnvelopeDigest ::
  OracleEnvelope -> OracleCommandDigest
oracleEnvelopeDigest envelope =
  case mkOracleCommandDigest (SHA256.hash (liveEnvelopeIdentityBytes envelope)) of
    Right digest -> digest
    Left problem -> error ("live command digest invariant: " <> show problem)

liveEnvelopeBytes :: OracleEnvelope -> ByteString
liveEnvelopeBytes envelope = case oracleEnvelopeProgress envelope of
  Nothing -> liveEnvelopeIdentityBytes envelope
  Just through -> liveEnvelopeIdentityBytes envelope <> SerializePut.runPut (putOracleProgress through)

-- This transcript commits only the immutable semantic envelope; piggyback
-- acknowledgements can advance independently on exact retries.
liveEnvelopeIdentityBytes :: OracleEnvelope -> ByteString
liveEnvelopeIdentityBytes
  (OracleEnvelope requestId expectedIndex home command _) =
    SerializePut.runPut $ do
      putFramedBytes liveEnvelopeDomain
      putRequestId requestId
      putMaybeControlIndex expectedIndex
      SerializePut.putByteString (heraldEpochBytes home)
      SerializePut.putByteString (oracleCommandCanonicalBytes command)

liveCommandPayloadBytes :: OracleCommand -> ByteString
liveCommandPayloadBytes command = SerializePut.runPut $ case command of
  RetireOracleProgress through -> putOracleProgress through
  StartProcessEpoch start -> do
    SerializePut.putByteString (processIdBytes (processStartProcessId start))
    SerializePut.putByteString (processEpochIdBytes (processStartProcessEpochId start))
    SerializePut.putByteString (heraldEpochBytes (processStartResidence start))
  EndProcessEpoch process reason -> do
    SerializePut.putByteString (processEpochIdBytes process)
    putFramedBytes (processEndReasonCanonicalBytes reason)
  DecideLabel decision caller object expected initialEvidence authority justification target acceptanceCut -> do
    SerializePut.putByteString (labelDecisionIdBytes decision)
    SerializePut.putByteString (processEpochIdBytes caller)
    SerializePut.putByteString (globalObjectIdBytes object)
    putLabel expected
    putMaybeInitialLabelEvidence initialEvidence
    putMaybeLiveAuthority authority
    putMaybeJustification justification
    putLabelTarget target
    putHomeAcceptanceCut acceptanceCut
  CompleteLabelDecision report ->
    putLabelCompletionAttestation report
  OpenHeraldFailureProbe target generation configuration -> do
    SerializePut.putByteString (heraldEpochBytes target)
    SerializePut.putByteString
      (heraldMembershipGenerationIdBytes generation)
    SerializePut.putByteString (V.voterConfigurationIdBytes configuration)
  ReportHeraldFailureProbe probe configuration result -> do
    putFramedBytes (heraldFailureProbeIdCanonicalBytes probe)
    SerializePut.putByteString (V.voterConfigurationIdBytes configuration)
    SerializePut.putWord8 $ case result of
      ProbeReachable -> 0
      ProbeUnreachable -> 1
  AcceptVoterHostFailure resolution -> putFramedBytes (failureProbeResolutionIdCanonicalBytes resolution)
  DismissHeraldFailureProbe resolution ->
    putFramedBytes (failureProbeResolutionIdCanonicalBytes resolution)
  RetireHeraldEpoch resolution target -> do
    putFramedBytes (failureProbeResolutionIdCanonicalBytes resolution)
    SerializePut.putByteString (heraldEpochBytes target)
  OpenDisappearanceProbe x0 x1 -> putFramedBytes (DC.encodeCommand (D.DisappearanceOpen x0 x1))
  ReportPredefinedAbsence x0 x1 -> putFramedBytes (DC.encodeCommand (D.DisappearanceReport x0 x1))
  InvalidateDisappearanceProbe x0 x1 x2 x3 -> putFramedBytes (DC.encodeCommand (D.DisappearanceInvalidate x0 x1 x2 x3))
  ResolveDisappearanceProbe x0 x1 -> putFramedBytes (DC.encodeCommand (D.DisappearanceResolve x0 x1))
  AbortDisappearanceProbe x0 x1 -> putFramedBytes (DC.encodeCommand (D.DisappearanceAbort x0 x1))
  AdmissionCommand admission -> putFramedBytes (A.encodeAdmissionCommand admission)
  VoterCommand administration -> putFramedBytes (V.encodeVoterCommand administration)

putRequestId :: OracleClientRequestId -> SerializePut.Put
putRequestId requestId = do
  SerializePut.putByteString (heraldEpochBytes (oracleClientRequestHome requestId))
  SerializePut.putWord64be (oracleClientRequestSequence requestId)

putMaybeControlIndex :: Maybe ControlIndex -> SerializePut.Put
putMaybeControlIndex Nothing = SerializePut.putWord8 0
putMaybeControlIndex (Just index) = do
  SerializePut.putWord8 1
  SerializePut.putWord64be (controlIndexWord64 index)

putMaybeInitialLabelEvidence :: Maybe InitialLabelEvidence -> SerializePut.Put
putMaybeInitialLabelEvidence Nothing = SerializePut.putWord8 0
putMaybeInitialLabelEvidence (Just evidence) = do
  SerializePut.putWord8 1
  SerializePut.putByteString (globalObjectIdBytes (initialLabelEvidenceObject evidence))
  putLabel (initialLabelEvidenceLabel evidence)
  case initialLabelEvidenceSource evidence of
    InitialBootstrapLabelEvidenceView process -> do
      SerializePut.putWord8 0
      SerializePut.putByteString (processEpochIdBytes process)
    InitialPublicationLabelEvidenceView publication -> do
      SerializePut.putWord8 1
      SerializePut.putByteString (nablaIdBytes (publicationNabla publication))
      putFramedBytes (authorityEpochCanonicalBytes (publicationAuthorityEpoch publication))
      SerializePut.putByteString (heraldEpochBytes (publicationSourceHeraldEpoch publication))
      SerializePut.putWord64be (nablaSequenceWord64 (publicationNablaSequence publication))

putMaybeLabelRevision :: Maybe LabelRevision -> SerializePut.Put
putMaybeLabelRevision Nothing = SerializePut.putWord8 0
putMaybeLabelRevision (Just revision) = do
  SerializePut.putWord8 1
  SerializePut.putWord64be
    (controlIndexWord64 (labelRevisionControlIndex revision))

putMaybeLiveAuthority :: Maybe LiveAuthority -> SerializePut.Put
putMaybeLiveAuthority Nothing = SerializePut.putWord8 0
putMaybeLiveAuthority (Just authority) = do
  SerializePut.putWord8 1
  putFramedBytes (liveAuthorityCanonicalBytes authority)

putMaybeJustification ::
  Maybe PriorAuthorityJustification -> SerializePut.Put
putMaybeJustification Nothing = SerializePut.putWord8 0
putMaybeJustification (Just justification) = do
  SerializePut.putWord8 1
  putJustification justification

putJustification :: PriorAuthorityJustification -> SerializePut.Put
putJustification justification = case priorAuthorityJustificationView justification of
  ExistingReleasedAuthorityView revision -> do
    SerializePut.putWord8 0
    SerializePut.putWord64be
      (controlIndexWord64 (labelRevisionControlIndex revision))
  CheckedGenesisAuthorityView digest -> do
    SerializePut.putWord8 1
    SerializePut.putByteString (initialProjectionDigestBytes digest)
  EstablishedStructuralAuthorityView occurrence cut -> do
    SerializePut.putWord8 2
    SerializePut.putByteString
      (heraldEpochBytes (structuralOccurrenceSourceHeraldEpoch occurrence))
    SerializePut.putWord64be
      (structuralSequenceWord64 (structuralOccurrenceSourceSequence occurrence))
    SerializePut.putByteString (topologyCutIdBytes cut)

putLabelTarget :: LabelTarget -> SerializePut.Put
putLabelTarget target = case labelTargetView target of
  TargetProcessView process -> do
    SerializePut.putWord8 0
    SerializePut.putByteString (processEpochIdBytes process)
  TargetVoidView -> SerializePut.putWord8 1
  TargetDeleteView -> SerializePut.putWord8 2

putLabel :: Label -> SerializePut.Put
putLabel = putFramedBytes . liveCanonicalLabelBytes

putReleasedState :: ReleasedLabelState -> SerializePut.Put
putReleasedState state = case releasedLabelStateView state of
  ReleasedLabelView label -> do
    SerializePut.putWord8 0
    putFramedBytes (liveCanonicalLabelBytes label)
  ReleasedDeletedView generation -> do
    SerializePut.putWord8 1
    SerializePut.putWord64be generation

liveCanonicalLabelBytes :: Label -> ByteString
liveCanonicalLabelBytes =
  canonicalValueByteString . canonicalValueBytes . labelValue

putHomeAcceptanceCut :: HomeLabelAcceptanceCut -> SerializePut.Put
putHomeAcceptanceCut =
  putFramedBytes . homeLabelAcceptanceCutCanonicalBytes

putLabelCompletionAttestation :: LabelCompletionAttestation -> SerializePut.Put
putLabelCompletionAttestation (LabelCompletionAttestation decision index digest collector generation) = do
  SerializePut.putByteString (labelDecisionIdBytes decision)
  SerializePut.putWord64be (controlIndexWord64 index)
  SerializePut.putByteString (labelOutcomeDigestBytes digest)
  SerializePut.putByteString (heraldEpochBytes collector)
  SerializePut.putByteString (heraldMembershipGenerationIdBytes generation)

putFramedBytes :: ByteString -> SerializePut.Put
putFramedBytes bytes = do
  SerializePut.putWord64be (fromIntegral (ByteString.length bytes))
  SerializePut.putByteString bytes

putCountedList :: (value -> SerializePut.Put) -> [value] -> SerializePut.Put
putCountedList putValue values = do
  SerializePut.putWord64be (fromIntegral (length values))
  mapM_ putValue values

putLiveProjectionEvent :: OracleProjectionEvent -> SerializePut.Put
putLiveProjectionEvent event = do
  SerializePut.putWord8 (oracleProjectionEventTag event)
  putFramedBytes
    (SerializePut.runPut (putLiveProjectionEventPayload event))

putLiveProjectionEventPayload ::
  OracleProjectionEvent -> SerializePut.Put
putLiveProjectionEventPayload event = case event of
  OracleReplicaRegistered registration -> putFramedBytes (V.encodeReplicaRegistration registration)
  OracleVoterHostFailureAccepted certificate -> putFramedBytes (F.encodeAcceptedVoterHostFailure certificate)
  OracleVoterChangeChanged change -> putFramedBytes (V.encodeVoterChange change)
  OracleVoterConfigurationChanged configuration -> putFramedBytes (V.encodeVoterConfiguration configuration)
  HeraldAdmissionChanged record -> putFramedBytes (A.encodeAdmissionRecord record)
  ProcessStarted record ->
    putLiveProcessEpochRecord record
  ProcessEpochEnded process index reason -> do
    SerializePut.putByteString (processEpochIdBytes process)
    SerializePut.putWord64be (controlIndexWord64 index)
    putFramedBytes (processEndReasonCanonicalBytes reason)
  LabelDecided decision terminal digest -> do
    putLiveDecision decision
    putCheckpointTerminal terminal
    SerializePut.putByteString (labelOutcomeDigestBytes digest)
  LabelWorkflowCompleted decision digest -> do
    SerializePut.putByteString (labelDecisionIdBytes decision)
    SerializePut.putByteString (labelOutcomeDigestBytes digest)
  OracleFailureProbeOpened probe target generation configuration -> do
    putFramedBytes (heraldFailureProbeIdCanonicalBytes probe)
    SerializePut.putByteString (heraldEpochBytes target)
    SerializePut.putByteString (heraldMembershipGenerationIdBytes generation)
    putFramedBytes (V.encodeVoterConfiguration configuration)
  OracleFailureProbeReportRecorded probe reporter result -> do
    putFramedBytes (heraldFailureProbeIdCanonicalBytes probe)
    SerializePut.putByteString (heraldEpochBytes reporter)
    SerializePut.putWord8 $ case result of
      ProbeReachable -> 0
      ProbeUnreachable -> 1
  OracleFailureProbeDismissed probe resolution -> do
    putFramedBytes (heraldFailureProbeIdCanonicalBytes probe)
    putFramedBytes (failureProbeResolutionIdCanonicalBytes resolution)
  HeraldMembershipAdvanced generation ->
    putFramedBytes (heraldMembershipGenerationCanonicalBytes generation)
  OracleFailureProbeRetired probe resolution successor -> do
    putFramedBytes (heraldFailureProbeIdCanonicalBytes probe)
    putFramedBytes (failureProbeResolutionIdCanonicalBytes resolution)
    SerializePut.putByteString (heraldMembershipGenerationIdBytes successor)
  OracleFailureProbeSuperseded probe successor -> do
    putFramedBytes (heraldFailureProbeIdCanonicalBytes probe)
    SerializePut.putByteString (heraldMembershipGenerationIdBytes successor)
  DisappearanceProbeOpened x0 x1 -> putFramedBytes (DC.encodeEvent (D.DisappearanceOpenProjected x0 x1))
  PredefinedAbsenceReported x0 -> putFramedBytes (DC.encodeEvent (D.DisappearanceReportProjected x0))
  DisappearanceProbeInvalidated x0 x1 -> putFramedBytes (DC.encodeEvent (D.DisappearanceInvalidatedProjected x0 x1))
  DisappearanceProbeResolved x0 x1 -> putFramedBytes (DC.encodeEvent (D.DisappearanceResolvedProjected x0 x1))
  DisappearanceProbeAborted x0 x1 -> putFramedBytes (DC.encodeEvent (D.DisappearanceAbortedProjected x0 x1))

putLiveNotAppliedReason ::
  LiveNotAppliedReason -> SerializePut.Put
putLiveNotAppliedReason reason = do
  let (reasonTag, payload) = liveNotAppliedReasonEncoding reason
  SerializePut.putWord8 reasonTag
  putFramedBytes payload

data OracleRejection
  = RequestHomeEpochMismatch HeraldEpoch HeraldEpoch
  | InactiveHomeHerald HeraldEpoch
  | StaleExpectedControlIndex ControlIndex ControlIndex
  | StartProcessResidenceMismatch ProcessEpochId HeraldEpoch HeraldEpoch
  | ProcessEpochAlreadyStarted ProcessEpochId
  | ProcessAlreadyStarted ProcessId
  | EndProcessUnknown ProcessEpochId
  | EndProcessResidenceMismatch ProcessEpochId HeraldEpoch HeraldEpoch
  | EndProcessAlreadyEnded ProcessEpochId
  | OpenDecisionIdMismatch LabelDecisionId LabelDecisionId
  | OpenAcceptanceCallerMismatch ProcessEpochId ProcessEpochId
  | OpenExpectedPriorLabelMismatch ReleasedLabelState ReleasedLabelState
  | OpenPriorAuthorityJustificationInvalid
  | UnknownLabelDecision LabelDecisionId
  | ReporterHomeMismatch HeraldEpoch HeraldEpoch
  | ReporterNotCaptured HeraldEpoch
  | LabelCompletionDecisionMismatch LabelDecisionId LabelDecisionId
  | LabelCompletionCollectorMismatch HeraldEpoch HeraldEpoch
  | LabelCompletionDigestMismatch LabelOutcomeDigest LabelOutcomeDigest
  | LabelCompletionMembershipGenerationMismatch HeraldMembershipGenerationId HeraldMembershipGenerationId
  | LabelCompletionDecisionIndexMismatch ControlIndex ControlIndex
  | LabelCompletionObsolete LabelDecisionId ControlIndex ControlIndex
  | FailureCommandRejected FailureRejection
  | DisappearanceCommandRejected D.DisappearanceRejection
  | HeraldAdmissionCommandRejected A.HeraldAdmissionProblem
  | VoterCommandRejected V.VoterChangeProblem
  | OpenInitialLabelEvidenceMissing GlobalObjectId
  | OpenInitialLabelEvidenceObjectMismatch GlobalObjectId GlobalObjectId
  deriving stock (Eq, Show)

-- | Stable constructor tag for all semantic Oracle rejection arms.
oracleRejectionTag :: OracleRejection -> Word8
oracleRejectionTag rejection = case rejection of
  RequestHomeEpochMismatch {} -> 0
  InactiveHomeHerald {} -> 1
  StaleExpectedControlIndex {} -> 2
  StartProcessResidenceMismatch {} -> 3
  ProcessEpochAlreadyStarted {} -> 4
  ProcessAlreadyStarted {} -> 5
  EndProcessUnknown {} -> 6
  EndProcessResidenceMismatch {} -> 7
  EndProcessAlreadyEnded {} -> 8
  OpenDecisionIdMismatch {} -> 9
  OpenAcceptanceCallerMismatch {} -> 13
  OpenExpectedPriorLabelMismatch {} -> 14
  OpenPriorAuthorityJustificationInvalid -> 17
  UnknownLabelDecision {} -> 19
  ReporterHomeMismatch {} -> 21
  ReporterNotCaptured {} -> 22
  LabelCompletionDecisionMismatch {} -> 32
  LabelCompletionCollectorMismatch {} -> 33
  LabelCompletionDigestMismatch {} -> 34
  LabelCompletionMembershipGenerationMismatch {} -> 35
  FailureCommandRejected {} -> 40
  DisappearanceCommandRejected {} -> 41
  HeraldAdmissionCommandRejected {} -> 42
  VoterCommandRejected {} -> 43
  OpenInitialLabelEvidenceMissing {} -> 44
  OpenInitialLabelEvidenceObjectMismatch {} -> 45
  LabelCompletionDecisionIndexMismatch {} -> 46
  LabelCompletionObsolete {} -> 47

-- | Exact private rejection transcript used after the receipt's rejected-arm
-- tag.  This remains a one-way pre-wire observation seam.
oracleRejectionCanonicalBytes ::
  OracleRejection -> ByteString
oracleRejectionCanonicalBytes =
  SerializePut.runPut . putLiveRejection

data OracleReceiptResult
  = OracleAccepted
  | OracleRejected OracleRejection
  deriving stock (Eq, Show)

data OracleAcceptedResult
  = AcceptedFailure FailureCommandResult
  | AcceptedDisappearance D.DisappearanceCommandResult
  | AcceptedVoter V.VoterAdministrationResult
  deriving stock (Eq, Show)

data OracleReceipt
  = OracleReceipt
      OracleClientRequestId
      OracleCommandDigest
      ControlIndex
      OracleReceiptResult
      (Maybe OracleAcceptedResult)
  deriving stock (Eq, Show)

oracleReceiptRequestId ::
  OracleReceipt -> OracleClientRequestId
oracleReceiptRequestId
  (OracleReceipt requestId _ _ _ _) = requestId

oracleReceiptCommandDigest ::
  OracleReceipt -> OracleCommandDigest
oracleReceiptCommandDigest
  (OracleReceipt _ digest _ _ _) = digest

oracleReceiptControlIndex :: OracleReceipt -> ControlIndex
oracleReceiptControlIndex
  (OracleReceipt _ _ index _ _) = index

oracleReceiptResult ::
  OracleReceipt -> OracleReceiptResult
oracleReceiptResult
  (OracleReceipt _ _ _ result _) = result

oracleReceiptFailureResult :: OracleReceipt -> Maybe FailureCommandResult
oracleReceiptFailureResult (OracleReceipt _ _ _ _ result) = case result of
  Just (AcceptedFailure accepted) -> Just accepted
  _ -> Nothing

oracleReceiptDisappearanceResult :: OracleReceipt -> Maybe D.DisappearanceCommandResult
oracleReceiptDisappearanceResult (OracleReceipt _ _ _ _ result) = case result of
  Just (AcceptedDisappearance accepted) -> Just accepted
  _ -> Nothing

oracleReceiptVoterResult :: OracleReceipt -> Maybe V.VoterAdministrationResult
oracleReceiptVoterResult (OracleReceipt _ _ _ _ result) = case result of
  Just (AcceptedVoter accepted) -> Just accepted
  _ -> Nothing

-- | Stable receipt transcript retained in the canonical Oracle state and in
-- each applied-entry vector.
oracleReceiptCanonicalBytes :: OracleReceipt -> ByteString
oracleReceiptCanonicalBytes receipt = SerializePut.runPut $ do
  putFramedBytes liveReceiptDomain
  putLiveReceipt receipt

data OracleProjectionEvent
  = OracleReplicaRegistered V.OracleReplicaRegistration
  | OracleVoterHostFailureAccepted F.AcceptedVoterHostFailure
  | OracleVoterChangeChanged V.VoterChange
  | OracleVoterConfigurationChanged V.VoterConfiguration
  | HeraldAdmissionChanged A.HeraldAdmissionRecord
  | ProcessStarted ProcessEpochRecord
  | ProcessEpochEnded ProcessEpochId ControlIndex ProcessEndReason
  | LabelDecided LiveLabelDecision LiveTerminalOutcome LabelOutcomeDigest
  | LabelWorkflowCompleted LabelDecisionId LabelOutcomeDigest
  | OracleFailureProbeOpened
      HeraldFailureProbeId
      HeraldEpoch
      HeraldMembershipGenerationId
      V.VoterConfiguration
  | OracleFailureProbeReportRecorded
      HeraldFailureProbeId
      HeraldEpoch
      ProbeResult
  | OracleFailureProbeDismissed
      HeraldFailureProbeId
      FailureProbeResolutionId
  | HeraldMembershipAdvanced HeraldMembershipGeneration
  | OracleFailureProbeRetired
      HeraldFailureProbeId
      FailureProbeResolutionId
      HeraldMembershipGenerationId
  | OracleFailureProbeSuperseded
      HeraldFailureProbeId
      HeraldMembershipGenerationId
  | DisappearanceProbeOpened D.ProjectedDisappearanceProbeHeader DD.DisappearanceOpenResult
  | PredefinedAbsenceReported DD.DisappearanceEvidenceClaim
  | DisappearanceProbeInvalidated DD.DisappearanceProbeId D.DisappearanceProbeInvalidationView
  | DisappearanceProbeResolved DD.DisappearanceProbeId DD.DisappearanceResolutionOutcome
  | DisappearanceProbeAborted DD.DisappearanceProbeId D.DisappearanceProbeAbortReasonView
  deriving stock (Eq, Show)

data OracleProjectionEventView
  = OracleReplicaRegisteredView V.OracleReplicaRegistration
  | OracleVoterHostFailureAcceptedView F.AcceptedVoterHostFailure
  | OracleVoterChangeChangedView V.VoterChange
  | OracleVoterConfigurationChangedView V.VoterConfiguration
  | HeraldAdmissionChangedView A.HeraldAdmissionRecord
  | ProcessStartedView ProcessEpochRecord
  | ProcessEpochEndedEventView ProcessEpochId ControlIndex ProcessEndReason
  | LabelDecidedView LiveLabelDecision LiveTerminalOutcome LabelOutcomeDigest
  | LabelWorkflowCompletedView LabelDecisionId LabelOutcomeDigest
  | OracleFailureProbeOpenedView
      HeraldFailureProbeId
      HeraldEpoch
      HeraldMembershipGenerationId
      V.VoterConfiguration
  | OracleFailureProbeReportRecordedView
      HeraldFailureProbeId
      HeraldEpoch
      ProbeResult
  | OracleFailureProbeDismissedView
      HeraldFailureProbeId
      FailureProbeResolutionId
  | HeraldMembershipAdvancedView HeraldMembershipGeneration
  | OracleFailureProbeRetiredView
      HeraldFailureProbeId
      FailureProbeResolutionId
      HeraldMembershipGenerationId
  | OracleFailureProbeSupersededView
      HeraldFailureProbeId
      HeraldMembershipGenerationId
  | DisappearanceProbeOpenedView D.ProjectedDisappearanceProbeHeader DD.DisappearanceOpenResult
  | PredefinedAbsenceReportedView DD.DisappearanceEvidenceClaim
  | DisappearanceProbeInvalidatedView DD.DisappearanceProbeId D.DisappearanceProbeInvalidationView
  | DisappearanceProbeResolvedView DD.DisappearanceProbeId DD.DisappearanceResolutionOutcome
  | DisappearanceProbeAbortedView DD.DisappearanceProbeId D.DisappearanceProbeAbortReasonView
  deriving stock (Eq, Show)

oracleProjectionEventView ::
  OracleProjectionEvent -> OracleProjectionEventView
oracleProjectionEventView event = case event of
  OracleReplicaRegistered registration -> OracleReplicaRegisteredView registration
  OracleVoterHostFailureAccepted certificate -> OracleVoterHostFailureAcceptedView certificate
  OracleVoterChangeChanged change -> OracleVoterChangeChangedView change
  OracleVoterConfigurationChanged configuration -> OracleVoterConfigurationChangedView configuration
  HeraldAdmissionChanged record -> HeraldAdmissionChangedView record
  ProcessStarted record -> ProcessStartedView record
  ProcessEpochEnded process index reason ->
    ProcessEpochEndedEventView process index reason
  LabelDecided decision terminal digest ->
    LabelDecidedView decision terminal digest
  LabelWorkflowCompleted decision digest ->
    LabelWorkflowCompletedView decision digest
  OracleFailureProbeOpened probe target generation configuration ->
    OracleFailureProbeOpenedView probe target generation configuration
  OracleFailureProbeReportRecorded probe reporter result ->
    OracleFailureProbeReportRecordedView probe reporter result
  OracleFailureProbeDismissed probe resolution ->
    OracleFailureProbeDismissedView probe resolution
  HeraldMembershipAdvanced generation ->
    HeraldMembershipAdvancedView generation
  OracleFailureProbeRetired probe resolution successor ->
    OracleFailureProbeRetiredView probe resolution successor
  OracleFailureProbeSuperseded probe successor ->
    OracleFailureProbeSupersededView probe successor
  DisappearanceProbeOpened x0 x1 -> DisappearanceProbeOpenedView x0 x1
  PredefinedAbsenceReported x0 -> PredefinedAbsenceReportedView x0
  DisappearanceProbeInvalidated x0 x1 -> DisappearanceProbeInvalidatedView x0 x1
  DisappearanceProbeResolved x0 x1 -> DisappearanceProbeResolvedView x0 x1
  DisappearanceProbeAborted x0 x1 -> DisappearanceProbeAbortedView x0 x1

oracleProjectionEventTag :: OracleProjectionEvent -> Word8
oracleProjectionEventTag event = case event of
  OracleReplicaRegistered {} -> 23
  OracleVoterChangeChanged {} -> 24
  OracleVoterConfigurationChanged {} -> 25
  OracleVoterHostFailureAccepted {} -> 26
  ProcessStarted _ -> 0
  ProcessEpochEnded {} -> 1
  LabelDecided {} -> 2
  LabelWorkflowCompleted {} -> 10
  OracleFailureProbeOpened {} -> 11
  OracleFailureProbeReportRecorded {} -> 12
  OracleFailureProbeDismissed {} -> 13
  HeraldMembershipAdvanced {} -> 14
  OracleFailureProbeRetired {} -> 15
  OracleFailureProbeSuperseded {} -> 16
  DisappearanceProbeOpened {} -> 17
  PredefinedAbsenceReported {} -> 18
  DisappearanceProbeInvalidated {} -> 19
  DisappearanceProbeResolved {} -> 20
  DisappearanceProbeAborted {} -> 21
  HeraldAdmissionChanged {} -> 22

-- | Canonical counted vector.  The empty vector is therefore an explicit
-- eight-byte zero count rather than an absent transcript.
oracleProjectionEventVectorCanonicalBytes ::
  [OracleProjectionEvent] -> ByteString
oracleProjectionEventVectorCanonicalBytes events = SerializePut.runPut $ do
  putFramedBytes liveProjectionEventVectorDomain
  putCountedList putLiveProjectionEvent events

data AppliedOracleCommandEntry
  = AppliedOracleCommandEntry OracleClientRequestId OracleCommandDigest OracleReceipt (Maybe OracleProgress)
  deriving stock (Eq, Show)
data AppliedOracleEntryOrigin
  = AppliedCommandOrigin AppliedOracleCommandEntry
  | AppliedConfigurationOrigin V.VoterConfiguration
  | AppliedProgressOrigin HeraldEpoch OracleProgress
  deriving stock (Eq, Show)
data AppliedOracleEntry
  = AppliedOracleEntry ControlIndex AppliedOracleEntryOrigin [OracleProjectionEvent] (Maybe OracleStateDigest)
  deriving stock (Eq, Show)

appliedEntryControlIndex :: AppliedOracleEntry -> ControlIndex
appliedEntryControlIndex (AppliedOracleEntry index _ _ _) = index
appliedEntryCommand :: AppliedOracleEntry -> Maybe AppliedOracleCommandEntry
appliedEntryCommand (AppliedOracleEntry _ (AppliedCommandOrigin command) _ _) = Just command
appliedEntryCommand _ = Nothing
appliedEntryConfiguration :: AppliedOracleEntry -> Maybe V.VoterConfiguration
appliedEntryConfiguration (AppliedOracleEntry _ (AppliedConfigurationOrigin configuration) _ _) = Just configuration
appliedEntryConfiguration _ = Nothing
appliedEntryReceiptRetirement :: AppliedOracleEntry -> Maybe (HeraldEpoch, Word64)
appliedEntryReceiptRetirement = fmap (fmap retirementHighWater) . appliedEntryReceiptRetirementProgress

appliedEntryReceiptRetirementProgress :: AppliedOracleEntry -> Maybe (HeraldEpoch, ReceiptRetirement)
appliedEntryReceiptRetirementProgress = fmap (fmap oracleProgressReceipts) . appliedEntryOracleProgress

appliedEntryOracleProgress :: AppliedOracleEntry -> Maybe (HeraldEpoch, OracleProgress)
appliedEntryOracleProgress (AppliedOracleEntry _ (AppliedProgressOrigin home progress) _ _) = Just (home, progress)
appliedEntryOracleProgress (AppliedOracleEntry _ (AppliedCommandOrigin (AppliedOracleCommandEntry request _ _ (Just progress))) _ _) = Just (oracleClientRequestHome request, progress)
appliedEntryOracleProgress _ = Nothing
appliedEntryRequestId :: AppliedOracleCommandEntry -> OracleClientRequestId
appliedEntryRequestId (AppliedOracleCommandEntry request _ _ _) = request
appliedEntryCommandDigest :: AppliedOracleCommandEntry -> OracleCommandDigest
appliedEntryCommandDigest (AppliedOracleCommandEntry _ digest _ _) = digest
appliedEntryReceipt :: AppliedOracleCommandEntry -> OracleReceipt
appliedEntryReceipt (AppliedOracleCommandEntry _ _ receipt _) = receipt
appliedEntryProjectionEvents :: AppliedOracleEntry -> [OracleProjectionEvent]
appliedEntryProjectionEvents (AppliedOracleEntry _ _ events _) = events
appliedEntryPostStateDigest :: AppliedOracleEntry -> Maybe OracleStateDigest
appliedEntryPostStateDigest (AppliedOracleEntry _ _ _ digest) = digest

-- Distinct constructor domains tag the closed watch-entry sum. Command entries
-- retain their request/receipt transcript; configuration entries have neither.
liveConfigurationEntryDomain :: ByteString
liveConfigurationEntryDomain = "ECLIPS-APPLIED-ORACLE-CONFIGURATION"
liveReceiptRetirementEntryDomain :: ByteString
liveReceiptRetirementEntryDomain = "ECLIPS-APPLIED-ORACLE-RECEIPT-RETIREMENT"
appliedOracleEntryCanonicalBytes :: AppliedOracleEntry -> ByteString
appliedOracleEntryCanonicalBytes (AppliedOracleEntry index origin events postStateDigest) =
  SerializePut.runPut $ do
    case origin of
      AppliedCommandOrigin (AppliedOracleCommandEntry requestId commandDigest receipt _) -> do
        putFramedBytes liveAppliedEntryDomain
        SerializePut.putWord64be (controlIndexWord64 index)
        putRequestId requestId
        SerializePut.putByteString (oracleCommandDigestBytes commandDigest)
        putFramedBytes (oracleReceiptCanonicalBytes receipt)
      AppliedConfigurationOrigin configuration -> do
        putFramedBytes liveConfigurationEntryDomain
        SerializePut.putWord64be (controlIndexWord64 index)
        putFramedBytes (V.encodeVoterConfiguration configuration)
      AppliedProgressOrigin home through -> do
        putFramedBytes liveReceiptRetirementEntryDomain
        SerializePut.putWord64be (controlIndexWord64 index)
        SerializePut.putByteString (heraldEpochBytes home)
        putOracleProgress through
    putFramedBytes (oracleProjectionEventVectorCanonicalBytes events)
    putOptionalOracleStateDigest postStateDigest
    case origin of
      AppliedCommandOrigin (AppliedOracleCommandEntry _ _ _ retirement) -> maybe (pure ()) putOracleProgress retirement
      _ -> pure ()

data DecodedReceiptFacts
  = DecodedReceiptFacts
      ByteString
      ByteString
      Word64
      ByteString
      Word64
      Bool
  deriving stock (Eq, Show)

decodedReceiptRequestHome :: DecodedReceiptFacts -> ByteString
decodedReceiptRequestHome (DecodedReceiptFacts _ home _ _ _ _) = home

decodedReceiptRequestSequence :: DecodedReceiptFacts -> Word64
decodedReceiptRequestSequence (DecodedReceiptFacts _ _ sequenceNumber _ _ _) =
  sequenceNumber

decodedReceiptCommandDigest :: DecodedReceiptFacts -> ByteString
decodedReceiptCommandDigest (DecodedReceiptFacts _ _ _ digest _ _) = digest

decodedReceiptControlIndex :: DecodedReceiptFacts -> Word64
decodedReceiptControlIndex (DecodedReceiptFacts _ _ _ _ index _) = index

decodedReceiptAccepted :: DecodedReceiptFacts -> Bool
decodedReceiptAccepted (DecodedReceiptFacts _ _ _ _ _ accepted) = accepted

data DecodedEventVectorFacts
  = DecodedEventVectorFacts ByteString [Word8] [Word64]
  deriving stock (Eq, Show)

decodedEventVectorTags :: DecodedEventVectorFacts -> [Word8]
decodedEventVectorTags (DecodedEventVectorFacts _ tags _) = tags

decodedEventVectorControlIndices :: DecodedEventVectorFacts -> [Word64]
decodedEventVectorControlIndices (DecodedEventVectorFacts _ _ indices) = indices

data RawLiveEnvelope
  = RawLiveEnvelope ByteString Bool Word8 ByteString

data RawLiveReceipt
  = RawLiveReceipt
      ByteString
      ByteString
      Word64
      ByteString
      Word64
      RawLiveReceiptResult

data RawLiveReceiptResult
  = RawLiveAccepted
  | RawLiveFailureAccepted
  | RawLiveRejected (Either Word8 ())
  | RawLiveReceiptResultInvalid Word8

data RawLiveEventVector
  = RawLiveEventVector ByteString [(Word8, ByteString)]

data RawLiveAppliedEntry
  = RawLiveAppliedEntry
      ByteString
      Word64
      ByteString
      Word64
      ByteString
      ByteString
      ByteString

decodeOracleEnvelopeCanonicalBytes ::
  ByteString ->
  Either OracleCanonicalError DecodedOracleEnvelope
decodeOracleEnvelopeCanonicalBytes bytes = do
  RawLiveEnvelope domainTag hasExpectedIndex commandTag commandPayload <-
    decodeLiveCanonical getRawLiveEnvelope bytes
  requireLiveDomain liveEnvelopeDomain domainTag
  if commandTag <= 21 && commandTag `notElem` [3, 4, 5, 6, 7]
    then Right ()
    else Left (UnknownOracleCommandTag commandTag)
  if commandTag `elem` [1, 11, 12, 20, 21] && hasExpectedIndex
    then Left InconsistentCanonicalOracleEntry
    else Right ()
  _ <- decodeLiveCanonical (getKnownLiveCommandPayload commandTag) commandPayload
  envelope <- decodeLiveCanonical getTypedLiveEnvelope bytes
  if oracleEnvelopeCanonicalBytes envelope == bytes
    then Right (DecodedOracleEnvelope envelope bytes)
    else Left InconsistentCanonicalOracleEntry

decodeOracleReceiptCanonicalBytes ::
  ByteString ->
  Either OracleCanonicalError DecodedOracleReceipt
decodeOracleReceiptCanonicalBytes bytes =
  do
    _ <- decodeLiveReceiptFacts bytes
    receipt <- decodeLiveCanonical getTypedLiveReceipt bytes
    if oracleReceiptCanonicalBytes receipt == bytes
      then Right (DecodedOracleReceipt receipt bytes)
      else Left InconsistentCanonicalOracleEntry

decodeLiveReceiptFacts ::
  ByteString -> Either OracleCanonicalError DecodedReceiptFacts
decodeLiveReceiptFacts bytes = do
  RawLiveReceipt domainTag home sequenceNumber digest index result <-
    decodeLiveCanonical getRawLiveReceipt bytes
  requireLiveDomain liveReceiptDomain domainTag
  accepted <- case result of
    RawLiveAccepted -> Right True
    RawLiveFailureAccepted -> Right True
    RawLiveRejected (Right ()) -> Right False
    RawLiveRejected (Left rejectionTag) ->
      Left (UnknownOracleRejectionTag rejectionTag)
    RawLiveReceiptResultInvalid resultTag ->
      Left (InvalidCanonicalOracleTag resultTag)
  Right
    ( DecodedReceiptFacts
        bytes
        home
        sequenceNumber
        digest
        index
        accepted
    )

decodeOracleProjectionEventVectorCanonicalBytes ::
  ByteString ->
  Either OracleCanonicalError DecodedOracleProjectionEventVector
decodeOracleProjectionEventVectorCanonicalBytes bytes = do
  _ <- decodeLiveEventVectorFacts bytes
  events <- decodeLiveCanonical getTypedLiveProjectionEventVector bytes
  if oracleProjectionEventVectorCanonicalBytes events == bytes
    then Right (DecodedOracleProjectionEventVector events bytes)
    else Left InconsistentCanonicalOracleEntry

decodeLiveEventVectorFacts ::
  ByteString -> Either OracleCanonicalError DecodedEventVectorFacts
decodeLiveEventVectorFacts bytes = do
  RawLiveEventVector domainTag taggedPayloads <-
    decodeLiveCanonical getRawLiveEventVector bytes
  requireLiveDomain liveProjectionEventVectorDomain domainTag
  let tags = fmap fst taggedPayloads
  case filter (\tag -> tag `elem` [3, 4, 5, 6, 7, 8, 9] || tag > 26) tags of
    unknown : _ -> Left (UnknownProjectionEventTag unknown)
    [] -> case firstDuplicate (filter (not . projectionEventTagMayRepeat) tags) of
      Just duplicate -> Left (DuplicateProjectionEventTag duplicate)
      Nothing -> do
        indices <- traverse decodeEvent taggedPayloads
        Right
          ( DecodedEventVectorFacts
              bytes
              tags
              [index | Just index <- indices]
          )
  where
    decodeEvent (eventTag, payload) =
      decodeLiveCanonical
        (getKnownLiveProjectionEventPayload eventTag)
        payload

decodeAppliedOracleEntryCanonicalBytes ::
  ByteString ->
  Either OracleCanonicalError DecodedAppliedOracleEntry
decodeAppliedOracleEntryCanonicalBytes bytes =
  case SerializeGet.runGetState getFramedBytes bytes 0 of
    Right (domainTag, _) | domainTag == liveConfigurationEntryDomain -> do
      entry <- decodeLiveCanonical getTypedConfigurationEntry bytes
      if appliedOracleEntryCanonicalBytes entry == bytes
        then Right (DecodedAppliedOracleEntry entry bytes)
        else Left InconsistentCanonicalOracleEntry
    Right (domainTag, _) | domainTag == liveReceiptRetirementEntryDomain -> do
      entry <- decodeLiveCanonical getTypedReceiptRetirementEntry bytes
      if appliedOracleEntryCanonicalBytes entry == bytes
        then Right (DecodedAppliedOracleEntry entry bytes)
        else Left InconsistentCanonicalOracleEntry
    _ -> decodeAppliedCommandEntryCanonicalBytes bytes

decodeAppliedCommandEntryCanonicalBytes :: ByteString -> Either OracleCanonicalError DecodedAppliedOracleEntry
decodeAppliedCommandEntryCanonicalBytes bytes = do
  RawLiveAppliedEntry domainTag index home sequenceNumber digest receiptBytes eventBytes <-
    decodeLiveCanonical getRawLiveAppliedEntry bytes
  requireLiveDomain liveAppliedEntryDomain domainTag
  receiptFacts <- decodeLiveReceiptFacts receiptBytes
  eventFacts <- decodeLiveEventVectorFacts eventBytes
  if index == decodedReceiptControlIndex receiptFacts
    && home == decodedReceiptRequestHome receiptFacts
    && sequenceNumber == decodedReceiptRequestSequence receiptFacts
    && digest == decodedReceiptCommandDigest receiptFacts
    && (decodedReceiptAccepted receiptFacts || null (decodedEventVectorTags eventFacts))
    && all (== index) (decodedEventVectorControlIndices eventFacts)
    then Right ()
    else Left InconsistentCanonicalOracleEntry
  entry <- decodeLiveCanonical getTypedLiveAppliedEntry bytes
  if appliedOracleEntryCanonicalBytes entry == bytes
    then Right (DecodedAppliedOracleEntry entry bytes)
    else Left InconsistentCanonicalOracleEntry

decodeLiveCanonical ::
  SerializeGet.Get value ->
  ByteString ->
  Either OracleCanonicalError value
decodeLiveCanonical getter bytes =
  case SerializeGet.runGetState getter bytes 0 of
    Left problem -> Left (MalformedCanonicalOracleBytes problem)
    Right (value, trailing)
      | ByteString.null trailing -> Right value
      | otherwise -> Left (MalformedCanonicalOracleBytes "trailing bytes")

requireLiveDomain ::
  ByteString -> ByteString -> Either OracleCanonicalError ()
requireLiveDomain expected actual
  | expected == actual = Right ()
  | otherwise = Left (WrongCanonicalOracleDomain actual)

-- | Nominal portable facts for a composed checked projection base. These
-- transcripts use the same explicit field encoders as Oracle events; they do
-- not encode any private owner state. Membership, system identity and the
-- enclosing committed cut remain checks of the receiving projection.
encodeLabelDecision :: LiveLabelDecision -> ByteString
encodeLabelDecision = encodePortableFact "ECLIPS-ORACLE-LABEL-DECISION" putLiveDecision

decodeLabelDecision :: ByteString -> Either OracleCanonicalError LiveLabelDecision
decodeLabelDecision = decodePortableFact "ECLIPS-ORACLE-LABEL-DECISION" encodeLabelDecision $ do
  decision <- getTypedLabelDecision
  unless
    (oracleClientRequestHome (liveDecisionOpenRequestId decision) == liveDecisionHome decision)
    (fail "label decision request home mismatch")
  pure decision

-- | Portable terminal facts, including the released overlay. This transport
-- transcript is distinct from the unchanged terminal digest preimage returned
-- by 'liveTerminalOutcomeCanonicalBytes'.
encodeLabelTerminalOutcome :: LiveTerminalOutcome -> ByteString
encodeLabelTerminalOutcome = encodePortableFact "ECLIPS-ORACLE-LABEL-TERMINAL" putCheckpointTerminal

decodeLabelTerminalOutcome :: ByteString -> Either OracleCanonicalError LiveTerminalOutcome
decodeLabelTerminalOutcome = decodePortableFact "ECLIPS-ORACLE-LABEL-TERMINAL" encodeLabelTerminalOutcome $ do
  terminal <- getCheckpointTerminal
  validateTerminalOutcome terminal
  pure terminal

encodeProcessEpochRecord :: ProcessEpochRecord -> ByteString
encodeProcessEpochRecord = encodePortableFact "ECLIPS-ORACLE-PROCESS-RECORD" putLiveProcessEpochRecord

decodeProcessEpochRecord :: ByteString -> Either OracleCanonicalError ProcessEpochRecord
decodeProcessEpochRecord = decodePortableFact "ECLIPS-ORACLE-PROCESS-RECORD" encodeProcessEpochRecord $ do
  record <- getTypedProcessEpochRecord
  case processRecordOrigin record of
    GenesisProcessView -> pure ()
    DynamicStartView start request -> do
      unless (oracleClientRequestHome request == processRecordResidence record) (fail "process Start request home mismatch")
      case processRecordLifecycle record of
        ProcessRecordLiveView -> pure ()
        ProcessRecordEndedView ended _ -> unless (ended > start) (fail "process End does not follow Start")
  pure record

encodePortableFact :: ByteString -> (value -> SerializePut.Put) -> value -> ByteString
encodePortableFact domain putValue value = SerializePut.runPut (putFramedBytes domain >> putValue value)

decodePortableFact :: ByteString -> (value -> ByteString) -> SerializeGet.Get value -> ByteString -> Either OracleCanonicalError value
decodePortableFact domain encodeValue getValue bytes = do
  -- Nested decoding may retain byte slices. Own this small fact transcript so
  -- its nominal IDs cannot keep a complete surrounding transfer allocation.
  value <- decodeLiveCanonical (getFramedBytes >>= requireGetDomain domain >> getValue) (ByteString.copy bytes)
  unless (encodeValue value == bytes) (Left InconsistentCanonicalOracleEntry)
  pure value

-- Typed canonical readers ---------------------------------------------------

getTypedLiveEnvelope :: SerializeGet.Get OracleEnvelope
getTypedLiveEnvelope = do
  domainTag <- getFramedBytes
  requireGetDomain liveEnvelopeDomain domainTag
  requestHome <- getHeraldEpoch
  requestSequence <- SerializeGet.getWord64be
  expected <- getMaybeControlIndex
  home <- getHeraldEpoch
  commandTag <- SerializeGet.getWord8
  commandPayload <- getFramedBytes
  command <- decodeNestedGet (getTypedLiveCommand commandTag) commandPayload
  retirement <- getTrailingOracleProgress
  case retirement of
    Just through
      | requestHome /= home || Lifetime.receiptIsRetired requestSequence (oracleProgressReceipts through) || oracleCommandReceiptRetirement command /= Nothing ->
          fail "incoherent piggyback receipt-retirement prefix"
    _ -> pure ()
  case command of
    EndProcessEpoch {}
      | expected /= Nothing -> fail "End envelope carries an expected control index"
    AcceptVoterHostFailure {}
      | expected /= Nothing -> fail "Accept failure envelope carries an expected control index"
    DismissHeraldFailureProbe {}
      | expected /= Nothing -> fail "Dismiss envelope carries an expected control index"
    RetireHeraldEpoch {}
      | expected /= Nothing -> fail "Retire envelope carries an expected control index"
    RetireOracleProgress through
      | expected /= Nothing || requestHome /= home || requestSequence /= retirementHighWater (oracleProgressReceipts through) ->
          fail "incoherent receipt-retirement envelope"
    _ -> pure ()
  let envelope = oracleEnvelope (oracleClientRequestId requestHome requestSequence) expected home command
  pure (maybe envelope (`oracleEnvelopeWithProgress` envelope) retirement)

getTrailingOracleProgress :: SerializeGet.Get (Maybe OracleProgress)
getTrailingOracleProgress = do
  empty <- SerializeGet.isEmpty
  if empty then pure Nothing else Just <$> getOracleProgress

getTypedLiveCommand :: Word8 -> SerializeGet.Get OracleCommand
getTypedLiveCommand commandTag = case commandTag of
  0 ->
    StartProcessEpoch <$> (processStart <$> getProcessId <*> getProcessEpochId <*> getHeraldEpoch)
  1 -> do
    process <- getProcessEpochId
    reason <- getTypedHeraldSubmittedProcessEndReason
    pure (EndProcessEpoch process reason)
  2 ->
    DecideLabel
      <$> getLabelDecisionId
      <*> getProcessEpochId
      <*> getGlobalObjectId
      <*> getTypedLabel
      <*> getMaybeInitialLabelEvidence
      <*> getMaybeAuthorityEpoch
      <*> getMaybePriorAuthorityJustification
      <*> getTypedLabelTarget
      <*> ( getRequiredFramedValue
              >>= admitGet "home label acceptance cut"
                . decodeHomeLabelAcceptanceCutCanonicalBytes
          )
  8 -> CompleteLabelDecision <$> getTypedLabelCompletionAttestation
  9 ->
    OpenHeraldFailureProbe
      <$> getHeraldEpoch
      <*> ( getIdentityBytes
              >>= admitGet "Herald membership generation ID"
                . mkHeraldMembershipGenerationId
          )
      <*> getVoterConfigurationId
  10 -> do
    probe <-
      getRequiredFramedValue
        >>= admitGet "Herald failure-probe ID"
          . decodeHeraldFailureProbeIdCanonicalBytes
    configuration <- getVoterConfigurationId
    resultTag <- SerializeGet.getWord8
    result <- case resultTag of
      0 -> pure ProbeReachable
      1 -> pure ProbeUnreachable
      _ -> fail ("invalid failure-probe result tag " <> show resultTag)
    pure (ReportHeraldFailureProbe probe configuration result)
  11 ->
    DismissHeraldFailureProbe
      <$> ( getRequiredFramedValue
              >>= admitGet "failure-probe resolution ID"
                . decodeFailureProbeResolutionIdCanonicalBytes
          )
  12 ->
    RetireHeraldEpoch
      <$> ( getRequiredFramedValue
              >>= admitGet "failure-probe resolution ID"
                . decodeFailureProbeResolutionIdCanonicalBytes
          )
      <*> getHeraldEpoch
  tag | tag >= 13 && tag <= 17 -> do
    command <- getRequiredFramedValue >>= admitGet "disappearance command" . DC.decodeCommand
    let live = fromDisappearanceCommand command
    if oracleCommandTag live == tag then pure live else fail "disappearance command tag mismatch"
  18 -> AdmissionCommand <$> (getRequiredFramedValue >>= admitGet "Herald admission command" . A.decodeAdmissionCommand)
  19 -> VoterCommand <$> (getRequiredFramedValue >>= admitGet "voter command" . V.decodeVoterCommand)
  20 -> AcceptVoterHostFailure <$> getFailureProbeResolutionId
  21 -> RetireOracleProgress <$> getOracleProgress
  _ -> fail ("unknown Oracle command tag " <> show commandTag)

getTypedLiveReceipt :: SerializeGet.Get OracleReceipt
getTypedLiveReceipt = do
  domainTag <- getFramedBytes
  requireGetDomain liveReceiptDomain domainTag
  requestId <- getOracleClientRequestId
  digest <- getOracleCommandDigest
  index <- getPositiveControlIndex
  resultTag <- SerializeGet.getWord8
  (result, failureResult) <- case resultTag of
    0 -> pure (OracleAccepted, Nothing)
    1 -> (,Nothing) . OracleRejected <$> getTypedLiveRejection
    2 -> (OracleAccepted,) . Just . AcceptedFailure <$> getTypedFailureCommandResult
    3 -> (OracleAccepted,) . Just . AcceptedDisappearance <$> (getRequiredFramedValue >>= admitGet "disappearance result" . DC.decodeAccepted)
    4 -> (OracleAccepted,) . Just . AcceptedVoter <$> (getRequiredFramedValue >>= admitGet "voter result" . V.decodeVoterResult)
    _ -> fail ("invalid Oracle receipt-result tag " <> show resultTag)
  pure (OracleReceipt requestId digest index result failureResult)

getTypedFailureCommandResult :: SerializeGet.Get FailureCommandResult
getTypedFailureCommandResult = do
  resultTag <- SerializeGet.getWord8
  case resultTag of
    0 -> FailureProbeOpened <$> getHeraldFailureProbeId
    1 -> do
      probe <- getHeraldFailureProbeId
      readyTag <- SerializeGet.getWord8
      ready <- case readyTag of
        0 -> pure Nothing
        1 -> do
          resolution <- getFailureProbeResolutionId
          dispositionTag <- SerializeGet.getWord8
          disposition <- case dispositionTag of
            0 -> pure DismissFailureProbe
            1 -> pure RetireFailureProbeTarget
            _ -> fail ("invalid failure resolution disposition tag " <> show dispositionTag)
          target <- getHeraldEpoch
          pure (Just (FailureResolutionReady resolution disposition target))
        _ -> fail ("invalid failure resolution presence tag " <> show readyTag)
      pure (FailureProbeReportRecorded probe ready)
    2 -> FailureProbeDismissedResult <$> getFailureProbeResolutionId
    3 ->
      HeraldRetiredResult
        <$> getFailureProbeResolutionId
        <*> ( getIdentityBytes
                >>= admitGet "Herald membership generation ID"
                  . mkHeraldMembershipGenerationId
            )
    4 -> VoterHostFailureAcceptedResult <$> getAcceptedVoterHostFailure
    _ -> fail ("invalid failure command-result tag " <> show resultTag)

getVoterConfigurationId :: SerializeGet.Get V.VoterConfigurationId
getVoterConfigurationId = getIdentityBytes >>= admitGet "voter configuration ID" . V.mkVoterConfigurationId
getAcceptedVoterHostFailure :: SerializeGet.Get F.AcceptedVoterHostFailure
getAcceptedVoterHostFailure = getRequiredFramedValue >>= admitGet "accepted voter-host failure" . F.decodeAcceptedVoterHostFailure

getHeraldFailureProbeId :: SerializeGet.Get HeraldFailureProbeId
getHeraldFailureProbeId =
  getRequiredFramedValue
    >>= admitGet "Herald failure-probe ID"
      . decodeHeraldFailureProbeIdCanonicalBytes

getFailureProbeResolutionId :: SerializeGet.Get FailureProbeResolutionId
getFailureProbeResolutionId =
  getRequiredFramedValue
    >>= admitGet "failure-probe resolution ID"
      . decodeFailureProbeResolutionIdCanonicalBytes

getTypedLiveProjectionEventVector :: SerializeGet.Get [OracleProjectionEvent]
getTypedLiveProjectionEventVector = do
  domainTag <- getFramedBytes
  requireGetDomain liveProjectionEventVectorDomain domainTag
  eventCount <- getCanonicalCount
  tagged <- replicateM eventCount $ do
    tag <- SerializeGet.getWord8
    payload <- getFramedBytes
    pure (tag, payload)
  case firstDuplicate (filter (not . projectionEventTagMayRepeat) (fmap fst tagged)) of
    Just duplicate -> fail ("duplicate projection-event tag " <> show duplicate)
    Nothing -> do
      events <- traverse (uncurry decodeEvent) tagged
      case firstDuplicate [process | ProcessEpochEnded process _ _ <- events] of
        Just _ -> fail "duplicate process-End projection event"
        Nothing -> pure ()
      case firstDuplicate [probe | OracleFailureProbeSuperseded probe _ <- events] of
        Just _ -> fail "duplicate superseded-probe projection event"
        Nothing -> do
          validateDisappearanceProjectionOrder events
          pure events
  where
    decodeEvent tag payload = decodeNestedGet (getTypedLiveProjectionEvent tag) payload

getTypedLiveProjectionEvent :: Word8 -> SerializeGet.Get OracleProjectionEvent
getTypedLiveProjectionEvent eventTag = case eventTag of
  0 -> ProcessStarted <$> getTypedProcessEpochRecord
  1 -> do
    process <- getProcessEpochId
    index <- getPositiveControlIndex
    reason <- getTypedProcessEndReason
    pure (ProcessEpochEnded process index reason)
  2 -> do
    decision <- getTypedLabelDecision
    terminal <- getCheckpointTerminal
    digest <- getLabelOutcomeDigest
    validateAtomicDecision decision terminal digest
    pure (LabelDecided decision terminal digest)
  10 -> LabelWorkflowCompleted <$> getLabelDecisionId <*> getLabelOutcomeDigest
  11 ->
    OracleFailureProbeOpened
      <$> getHeraldFailureProbeId
      <*> getHeraldEpoch
      <*> getMembershipGenerationId
      <*> (getRequiredFramedValue >>= admitGet "failure probe voter configuration" . V.decodeVoterConfiguration)
  12 ->
    OracleFailureProbeReportRecorded
      <$> getHeraldFailureProbeId
      <*> getHeraldEpoch
      <*> getTypedProbeResult
  13 ->
    OracleFailureProbeDismissed
      <$> getHeraldFailureProbeId
      <*> getFailureProbeResolutionId
  14 ->
    HeraldMembershipAdvanced
      <$> ( getRequiredFramedValue
              >>= admitGet "Herald membership generation"
                . decodeHeraldMembershipGenerationCanonicalBytes
          )
  15 ->
    OracleFailureProbeRetired
      <$> getHeraldFailureProbeId
      <*> getFailureProbeResolutionId
      <*> getMembershipGenerationId
  16 ->
    OracleFailureProbeSuperseded
      <$> getHeraldFailureProbeId
      <*> getMembershipGenerationId
  tag | tag >= 17 && tag <= 21 -> do
    projected <- getRequiredFramedValue >>= admitGet "disappearance event" . DC.decodeEvent
    let event = projectDisappearanceEvent projected
    if oracleProjectionEventTag event == tag then pure event else fail "disappearance projection tag mismatch"
  22 -> HeraldAdmissionChanged <$> (getRequiredFramedValue >>= admitGet "Herald admission event" . A.decodeAdmissionRecord)
  23 -> OracleReplicaRegistered <$> (getRequiredFramedValue >>= admitGet "replica registration" . V.decodeReplicaRegistration)
  24 -> OracleVoterChangeChanged <$> (getRequiredFramedValue >>= admitGet "voter change" . V.decodeVoterChange)
  25 -> OracleVoterConfigurationChanged <$> (getRequiredFramedValue >>= admitGet "voter configuration" . V.decodeVoterConfiguration)
  26 -> OracleVoterHostFailureAccepted <$> getAcceptedVoterHostFailure
  _ -> fail ("unknown projection-event tag " <> show eventTag)

getTypedLiveAppliedEntry :: SerializeGet.Get AppliedOracleEntry
getTypedLiveAppliedEntry = do
  domainTag <- getFramedBytes
  requireGetDomain liveAppliedEntryDomain domainTag
  index <- getPositiveControlIndex
  requestId <- getOracleClientRequestId
  commandDigest <- getOracleCommandDigest
  receiptBytes <- getFramedBytes
  receipt <- decodeNestedGet getTypedLiveReceipt receiptBytes
  eventBytes <- getFramedBytes
  events <- decodeNestedGet getTypedLiveProjectionEventVector eventBytes
  postStateDigest <- getOptionalOracleStateDigest
  retirement <- getTrailingOracleProgress
  if maybe True (not . Lifetime.receiptIsRetired (oracleClientRequestSequence requestId) . oracleProgressReceipts) retirement
    && maybe True ((<= index) . oracleProgressLabelsThrough) retirement
    && oracleReceiptControlIndex receipt == index
    && oracleReceiptRequestId receipt == requestId
    && oracleReceiptCommandDigest receipt == commandDigest
    && (receiptAccepted receipt || null events)
    && all (eventMatchesIndex index) events
    && disappearanceReceiptMatchesEvents receipt events
    && voterReceiptMatchesEvents receipt events
    && not (any isConfigurationObservation events)
    then
      pure
        ( AppliedOracleEntry
            index
            (AppliedCommandOrigin (AppliedOracleCommandEntry requestId commandDigest receipt retirement))
            events
            postStateDigest
        )
    else fail "incoherent applied Oracle entry"
  where
    isConfigurationObservation OracleVoterConfigurationChanged {} = True
    isConfigurationObservation _ = False
    receiptAccepted receipt = case oracleReceiptResult receipt of
      OracleAccepted -> True
      OracleRejected _ -> False
    eventMatchesIndex index event =
      let probePrecedes probe = DD.disappearanceProbeOpenControlIndex probe < index
          chronology = case event of
            OracleFailureProbeOpened probe _ _ configuration ->
              heraldFailureProbeControlIndex probe == index
                && V.voterConfigurationControlIndex configuration < index
                && case V.voterConfigurationView configuration of V.StableVoterConfigurationView _ -> True; _ -> False
            DisappearanceProbeOpened _ result -> case DD.disappearanceOpenResultView result of
              DD.OpenedDisappearanceProbe _ -> True
              DD.AliasedDisappearanceProbe probe -> probePrecedes probe
            PredefinedAbsenceReported claim -> probePrecedes (DD.disappearanceEvidenceClaimProbeId claim)
            DisappearanceProbeInvalidated probe _ -> probePrecedes probe
            DisappearanceProbeResolved probe _ -> probePrecedes probe
            DisappearanceProbeAborted probe _ -> probePrecedes probe
            _ -> True
       in chronology && case projectionEventControlIndex event of
            Nothing -> True
            Just eventIndex -> eventIndex == index

projectionEventControlIndex :: OracleProjectionEvent -> Maybe ControlIndex
projectionEventControlIndex event = case event of
  OracleReplicaRegistered _ -> Nothing -- repeated registration retains its original admission coordinate
  OracleVoterHostFailureAccepted _ -> Nothing -- immutable acceptance may be retried later
  OracleVoterChangeChanged _ -> Nothing -- a terminal result may be observed by a later command
  OracleVoterConfigurationChanged configuration -> Just (V.voterConfigurationControlIndex configuration)
  HeraldAdmissionChanged record -> Just (A.admissionRecordChangedIndex record)
  ProcessStarted _ -> Nothing
  ProcessEpochEnded _ index _ -> Just index
  LabelDecided decision _ _ -> Just (liveDecisionRequestControlIndex decision)
  LabelWorkflowCompleted {} -> Nothing
  OracleFailureProbeOpened {} -> Nothing
  OracleFailureProbeReportRecorded {} -> Nothing
  OracleFailureProbeDismissed {} -> Nothing
  HeraldMembershipAdvanced generation ->
    -- The canonical generation carries the retirement control index; the
    -- typed constructor has already checked it is a non-genesis successor.
    heraldMembershipGenerationChangeControlIndex generation
  OracleFailureProbeRetired {} -> Nothing
  OracleFailureProbeSuperseded {} -> Nothing
  DisappearanceProbeOpened _ result -> case DD.disappearanceOpenResultView result of
    DD.OpenedDisappearanceProbe probe -> Just (DD.disappearanceProbeOpenControlIndex probe)
    DD.AliasedDisappearanceProbe _ -> Nothing
  PredefinedAbsenceReported {} -> Nothing
  DisappearanceProbeInvalidated {} -> Nothing
  DisappearanceProbeResolved _ outcome -> Just (DD.disappearanceResolutionControlIndex outcome)
  DisappearanceProbeAborted _ (D.AdmissionPreparingDisappearanceAbortView admission) -> Just (Membership.heraldAdmissionControlIndex admission)
  DisappearanceProbeAborted {} -> Nothing

getTypedProcessEpochRecord :: SerializeGet.Get ProcessEpochRecord
getTypedProcessEpochRecord = do
  processId <- getProcessId
  processEpoch <- getProcessEpochId
  residence <- getHeraldEpoch
  originTag <- SerializeGet.getWord8
  origin <- case originTag of
    0 -> pure GenesisProcess
    1 -> DynamicStart <$> getPositiveControlIndex <*> getOracleClientRequestId
    _ -> fail ("invalid process-origin tag " <> show originTag)
  lifecycleTag <- SerializeGet.getWord8
  lifecycle <- case lifecycleTag of
    0 -> pure ProcessRecordLive
    1 -> do
      index <- getPositiveControlIndex
      reason <- getTypedProcessEndReason
      pure (ProcessRecordEnded index reason)
    _ -> fail ("invalid process-lifecycle tag " <> show lifecycleTag)
  pure (ProcessEpochRecord processId processEpoch residence origin lifecycle)

getTypedLabelDecision :: SerializeGet.Get LiveLabelDecision
getTypedLabelDecision = do
  decision <- getLabelDecisionId
  requestId <- getOracleClientRequestId
  home <- getHeraldEpoch
  caller <- getProcessEpochId
  object <- getGlobalObjectId
  expected <- getTypedLabel
  initialEvidence <- getMaybeInitialLabelEvidence
  revision <- getMaybeLabelRevision
  authority <- getMaybeAuthorityEpoch
  justification <- getMaybePriorAuthorityJustification
  target <- getTypedLabelTarget
  requestIndex <- getPositiveControlIndex
  acceptanceCut <-
    getRequiredFramedValue
      >>= admitGet "home label acceptance cut"
        . decodeHomeLabelAcceptanceCutCanonicalBytes
  members <- getMemberSetDigest
  generation <-
    getIdentityBytes
      >>= admitGet "Herald membership generation ID"
        . mkHeraldMembershipGenerationId
  if maybe False ((/= object) . initialLabelEvidenceObject) initialEvidence
    || (revision == Nothing && initialEvidence == Nothing)
    || labelProcessAcceptancePositionProcess
      (homeLabelAcceptanceCutProcessPosition acceptanceCut)
      /= caller
    then fail "invalid captured label-decision facts"
    else
      pure
        ( LiveLabelDecision
            decision
            requestId
            home
            caller
            object
            expected
            initialEvidence
            revision
            authority
            justification
            target
            requestIndex
            acceptanceCut
            members
            generation
        )

getTypedReleasedLabelOverlay :: SerializeGet.Get ReleasedLabelOverlay
getTypedReleasedLabelOverlay = do
  state <- getTypedReleasedState
  case releasedLabelStateView state of
    ReleasedLabelView (ZombieLabel _, _) -> fail "stored released overlay is zombie"
    _ -> pure ()
  ReleasedLabelOverlay
    state
    <$> getPositiveLabelRevision
    <*> getMaybeAuthorityEpoch

getTypedLabelCompletionAttestation :: SerializeGet.Get LabelCompletionAttestation
getTypedLabelCompletionAttestation =
  LabelCompletionAttestation
    <$> getLabelDecisionId
    <*> getPositiveControlIndex
    <*> getLabelOutcomeDigest
    <*> getHeraldEpoch
    <*> getMembershipGenerationId

getTypedLiveRejection :: SerializeGet.Get OracleRejection
getTypedLiveRejection = do
  rejectionTag <- SerializeGet.getWord8
  case rejectionTag of
    0 -> RequestHomeEpochMismatch <$> getHeraldEpoch <*> getHeraldEpoch
    1 -> InactiveHomeHerald <$> getHeraldEpoch
    2 -> StaleExpectedControlIndex <$> getControlIndex <*> getControlIndex
    3 ->
      StartProcessResidenceMismatch
        <$> getProcessEpochId
        <*> getHeraldEpoch
        <*> getHeraldEpoch
    4 -> ProcessEpochAlreadyStarted <$> getProcessEpochId
    5 -> ProcessAlreadyStarted <$> getProcessId
    6 -> EndProcessUnknown <$> getProcessEpochId
    7 ->
      EndProcessResidenceMismatch
        <$> getProcessEpochId
        <*> getHeraldEpoch
        <*> getHeraldEpoch
    8 -> EndProcessAlreadyEnded <$> getProcessEpochId
    9 -> OpenDecisionIdMismatch <$> getLabelDecisionId <*> getLabelDecisionId
    13 ->
      OpenAcceptanceCallerMismatch
        <$> getProcessEpochId
        <*> getProcessEpochId
    14 ->
      OpenExpectedPriorLabelMismatch
        <$> getTypedReleasedState
        <*> getTypedReleasedState
    17 -> pure OpenPriorAuthorityJustificationInvalid
    19 -> UnknownLabelDecision <$> getLabelDecisionId
    21 -> ReporterHomeMismatch <$> getHeraldEpoch <*> getHeraldEpoch
    22 -> ReporterNotCaptured <$> getHeraldEpoch
    32 ->
      LabelCompletionDecisionMismatch
        <$> getLabelDecisionId
        <*> getLabelDecisionId
    33 -> LabelCompletionCollectorMismatch <$> getHeraldEpoch <*> getHeraldEpoch
    34 ->
      LabelCompletionDigestMismatch
        <$> getLabelOutcomeDigest
        <*> getLabelOutcomeDigest
    35 -> LabelCompletionMembershipGenerationMismatch <$> getMembershipGenerationId <*> getMembershipGenerationId
    40 -> FailureCommandRejected <$> getTypedFailureRejection
    41 -> DisappearanceCommandRejected <$> (getRequiredFramedValue >>= admitGet "disappearance rejection" . DC.decodeRejection)
    42 -> HeraldAdmissionCommandRejected <$> (getRequiredFramedValue >>= admitGet "Herald admission rejection" . A.decodeAdmissionProblem)
    43 -> VoterCommandRejected <$> (getRequiredFramedValue >>= admitGet "voter rejection" . V.decodeVoterProblem)
    44 -> OpenInitialLabelEvidenceMissing <$> getGlobalObjectId
    45 -> OpenInitialLabelEvidenceObjectMismatch <$> getGlobalObjectId <*> getGlobalObjectId
    46 -> LabelCompletionDecisionIndexMismatch <$> getPositiveControlIndex <*> (controlIndex <$> SerializeGet.getWord64be)
    47 -> LabelCompletionObsolete <$> getLabelDecisionId <*> (controlIndex <$> SerializeGet.getWord64be) <*> (controlIndex <$> SerializeGet.getWord64be)
    _ -> fail ("unknown Oracle rejection tag " <> show rejectionTag)

getTypedFailureRejection :: SerializeGet.Get FailureRejection
getTypedFailureRejection = do
  rejectionTag <- SerializeGet.getWord8
  case rejectionTag of
    0 ->
      StaleMembershipGeneration
        <$> getMembershipGenerationId
        <*> getMembershipGenerationId
    1 -> FailureProbeTargetNotActive <$> getHeraldEpoch
    2 -> FailureProbeTargetHostsVoter <$> getHeraldEpoch
    3 -> UnknownFailureProbe <$> getHeraldFailureProbeId
    4 ->
      FailureProbeTerminal
        <$> getHeraldFailureProbeId
        <*> getTypedFailureProbeTerminalView
    5 -> IneligibleFailureReporter <$> getHeraldEpoch
    6 ->
      ConflictingFailureReport
        <$> getHeraldFailureProbeId
        <*> getHeraldEpoch
        <*> getTypedProbeResult
        <*> getTypedProbeResult
    7 ->
      FailureProbeThresholdNotReached
        <$> getHeraldFailureProbeId
        <*> getTypedFailureProbeResolution
    8 -> RetirementTargetMismatch <$> getHeraldEpoch <*> getHeraldEpoch
    9 -> StaleFailureVoterConfiguration <$> getVoterConfigurationId <*> getVoterConfigurationId
    10 -> FailureProbeTargetNotVoter <$> getHeraldEpoch
    11 -> FailureVoterExclusionNotApplied <$> getHeraldEpoch
    _ -> fail ("unknown failure rejection tag " <> show rejectionTag)

getTypedFailureProbeTerminalView :: SerializeGet.Get FailureProbeTerminalView
getTypedFailureProbeTerminalView = do
  terminalTag <- SerializeGet.getWord8
  case terminalTag of
    0 -> ProbeDismissedView <$> getFailureProbeResolutionId <*> getPositiveControlIndex
    1 ->
      ProbeRetiredView
        <$> getFailureProbeResolutionId
        <*> getMembershipGenerationId
        <*> getPositiveControlIndex
    2 -> ProbeMembershipSupersededView <$> getMembershipGenerationId <*> getPositiveControlIndex
    3 -> ProbeVoterHostFailureAcceptedView <$> getAcceptedVoterHostFailure
    _ -> fail ("invalid failure-probe terminal tag " <> show terminalTag)

getTypedProbeResult :: SerializeGet.Get ProbeResult
getTypedProbeResult = do
  resultTag <- SerializeGet.getWord8
  case resultTag of
    0 -> pure ProbeReachable
    1 -> pure ProbeUnreachable
    _ -> fail ("invalid failure-probe result tag " <> show resultTag)

getTypedFailureProbeResolution :: SerializeGet.Get FailureProbeResolution
getTypedFailureProbeResolution = do
  dispositionTag <- SerializeGet.getWord8
  case dispositionTag of
    0 -> pure DismissFailureProbe
    1 -> pure RetireFailureProbeTarget
    _ -> fail ("invalid failure-probe disposition tag " <> show dispositionTag)

getMembershipGenerationId :: SerializeGet.Get HeraldMembershipGenerationId
getMembershipGenerationId =
  getIdentityBytes
    >>= admitGet "Herald membership generation ID"
      . mkHeraldMembershipGenerationId

getTypedNotAppliedReason :: SerializeGet.Get LiveNotAppliedReason
getTypedNotAppliedReason = do
  reasonTag <- SerializeGet.getWord8
  payload <- getFramedBytes
  case reasonTag of
    0 -> LiveCallerProcessEnded <$> decodeNestedGet getProcessEpochId payload
    1 -> LiveTargetProcessEnded <$> decodeNestedGet getProcessEpochId payload
    2 -> requireEmpty LivePriorLabelChanged payload
    5 -> requireEmpty LiveTransitionNoLongerPermitted payload
    _ -> fail ("invalid NotApplied-reason tag " <> show reasonTag)
  where
    requireEmpty value payload
      | ByteString.null payload = pure value
      | otherwise = fail "NotApplied reason has an unexpected payload"

getTypedReleasedState :: SerializeGet.Get ReleasedLabelState
getTypedReleasedState = do
  stateTag <- SerializeGet.getWord8
  case stateTag of
    0 -> do
      labelBytes <- getRequiredFramedValue
      value <- admitGet "canonical label" (decodeCanonicalValue labelBytes)
      case viewValue value of
        LabelValue label -> pure (releasedLabel label)
        _ -> fail "released-state value is not a Label"
    1 -> releasedDeleted <$> SerializeGet.getWord64be
    _ -> fail ("invalid released-state tag " <> show stateTag)

getTypedLabel :: SerializeGet.Get Label
getTypedLabel = do
  labelBytes <- getRequiredFramedValue
  value <- admitGet "canonical label" (decodeCanonicalValue labelBytes)
  case viewValue value of
    LabelValue label -> pure label
    _ -> fail "value is not a Label"

getTypedLabelTarget :: SerializeGet.Get LabelTarget
getTypedLabelTarget = do
  targetTag <- SerializeGet.getWord8
  case targetTag of
    0 -> targetProcess <$> getProcessEpochId
    1 -> pure targetVoid
    2 -> pure targetDelete
    _ -> fail ("invalid label-target tag " <> show targetTag)

getMaybeInitialLabelEvidence :: SerializeGet.Get (Maybe InitialLabelEvidence)
getMaybeInitialLabelEvidence = do
  maybeTag <- SerializeGet.getWord8
  case maybeTag of
    0 -> pure Nothing
    1 -> do
      object <- getGlobalObjectId
      label <- getTypedLabel
      sourceTag <- SerializeGet.getWord8
      evidence <- case sourceTag of
        0 -> do
          process <- getProcessEpochId
          let evidence = initialBootstrapLabelEvidence object process
          unless (initialLabelEvidenceLabel evidence == label) (fail "bootstrap initial label does not match its process")
          pure evidence
        1 -> do
          nabla <- getIdentityBytes >>= admitGet "initial publication nabla" . mkNablaId
          authority <- getRequiredFramedValue >>= admitGet "initial publication authority" . decodeAuthorityEpochCanonicalBytes
          source <- getHeraldEpoch
          sequenceNumber <- nablaSequence <$> SerializeGet.getWord64be
          maybe
            (fail "initial publication label has a nonzero generation")
            pure
            (initialPublicationLabelEvidence object label (publicationId nabla authority source sequenceNumber))
        _ -> fail ("invalid initial-label source tag " <> show sourceTag)
      pure (Just evidence)
    _ -> fail ("invalid optional-initial-label tag " <> show maybeTag)

getMaybePriorAuthorityJustification ::
  SerializeGet.Get (Maybe PriorAuthorityJustification)
getMaybePriorAuthorityJustification = do
  maybeTag <- SerializeGet.getWord8
  case maybeTag of
    0 -> pure Nothing
    1 -> Just <$> getPriorAuthorityJustification
    _ -> fail ("invalid optional-justification tag " <> show maybeTag)

getPriorAuthorityJustification :: SerializeGet.Get PriorAuthorityJustification
getPriorAuthorityJustification = do
  justificationTag <- SerializeGet.getWord8
  case justificationTag of
    0 -> existingReleasedAuthority <$> getPositiveLabelRevision
    1 ->
      checkedGenesisAuthority
        <$> (getDigestBytes >>= admitGet "initial projection digest" . mkInitialProjectionDigest)
    2 -> do
      source <- getHeraldEpoch
      sequenceNumber <- SerializeGet.getWord64be
      sequenceValue <- admitGet "structural sequence" (mkStructuralSequence sequenceNumber)
      cut <- getDigestBytes >>= admitGet "topology cut" . mkTopologyCutId
      pure (establishedStructuralAuthority (structuralOccurrenceId source sequenceValue) cut)
    _ -> fail ("invalid prior-justification tag " <> show justificationTag)

getMaybeAuthorityEpoch :: SerializeGet.Get (Maybe AuthorityEpoch)
getMaybeAuthorityEpoch = do
  maybeTag <- SerializeGet.getWord8
  case maybeTag of
    0 -> pure Nothing
    1 ->
      Just
        <$> ( getRequiredFramedValue
                >>= admitGet "authority epoch"
                  . decodeAuthorityEpochCanonicalBytes
            )
    _ -> fail ("invalid optional-authority tag " <> show maybeTag)

getMaybeLabelRevision :: SerializeGet.Get (Maybe LabelRevision)
getMaybeLabelRevision = do
  maybeTag <- SerializeGet.getWord8
  case maybeTag of
    0 -> pure Nothing
    1 -> Just <$> getPositiveLabelRevision
    _ -> fail ("invalid optional-label-revision tag " <> show maybeTag)

getPositiveLabelRevision :: SerializeGet.Get LabelRevision
getPositiveLabelRevision = do
  index <- getPositiveControlIndex
  admitGet "label revision" (mkLabelRevision index)

getMaybeControlIndex :: SerializeGet.Get (Maybe ControlIndex)
getMaybeControlIndex = do
  maybeTag <- SerializeGet.getWord8
  case maybeTag of
    0 -> pure Nothing
    1 -> Just <$> getControlIndex
    _ -> fail ("invalid optional-control-index tag " <> show maybeTag)

getControlIndex :: SerializeGet.Get ControlIndex
getControlIndex = controlIndex <$> SerializeGet.getWord64be

getPositiveControlIndex :: SerializeGet.Get ControlIndex
getPositiveControlIndex = do
  index <- getControlIndex
  if controlIndexWord64 index == 0
    then fail "control index must be positive"
    else pure index

getOracleClientRequestId :: SerializeGet.Get OracleClientRequestId
getOracleClientRequestId =
  oracleClientRequestId <$> getHeraldEpoch <*> SerializeGet.getWord64be

getHeraldEpoch :: SerializeGet.Get HeraldEpoch
getHeraldEpoch = getIdentity "HeraldEpoch" mkHeraldEpoch

getProcessEpochId :: SerializeGet.Get ProcessEpochId
getProcessEpochId = getIdentity "ProcessEpochId" mkProcessEpochId

getProcessId :: SerializeGet.Get ProcessId
getProcessId = getIdentity "ProcessId" mkProcessId

getGlobalObjectId :: SerializeGet.Get GlobalObjectId
getGlobalObjectId = getIdentity "GlobalObjectId" mkGlobalObjectId

getLabelDecisionId :: SerializeGet.Get LabelDecisionId
getLabelDecisionId = getIdentity "LabelDecisionId" mkLabelDecisionId

getIdentity ::
  (Show problem) =>
  String ->
  (ByteString -> Either problem identity) ->
  SerializeGet.Get identity
getIdentity label constructor =
  getIdentityBytes >>= admitGet label . constructor

getOracleCommandDigest :: SerializeGet.Get OracleCommandDigest
getOracleCommandDigest =
  getDigestBytes >>= admitGet "Oracle command digest" . mkOracleCommandDigest

putOptionalOracleStateDigest :: Maybe OracleStateDigest -> SerializePut.Put
putOptionalOracleStateDigest Nothing = SerializePut.putWord8 0
putOptionalOracleStateDigest (Just digest) =
  SerializePut.putWord8 1 >> SerializePut.putByteString (oracleStateDigestBytes digest)

getOptionalOracleStateDigest :: SerializeGet.Get (Maybe OracleStateDigest)
getOptionalOracleStateDigest =
  SerializeGet.getWord8 >>= \case
    0 -> pure Nothing
    1 -> Just <$> getOracleStateDigest
    _ -> fail "invalid Oracle state digest option"

getOracleStateDigest :: SerializeGet.Get OracleStateDigest
getOracleStateDigest =
  getDigestBytes >>= admitGet "Oracle state digest" . mkOracleStateDigest

getPreparedLabelDigest :: SerializeGet.Get PreparedLabelDigest
getPreparedLabelDigest =
  getDigestBytes >>= admitGet "prepared-label digest" . mkPreparedLabelDigest

getLabelOutcomeDigest :: SerializeGet.Get LabelOutcomeDigest
getLabelOutcomeDigest =
  getDigestBytes >>= admitGet "label-outcome digest" . mkLabelOutcomeDigest

getCatalogueDigest :: SerializeGet.Get CatalogueDigest
getCatalogueDigest =
  getDigestBytes >>= admitGet "catalogue digest" . mkCatalogueDigest

getMemberSetDigest :: SerializeGet.Get MemberSetDigest
getMemberSetDigest =
  getDigestBytes >>= admitGet "member-set digest" . mkMemberSetDigest

requireGetDomain :: ByteString -> ByteString -> SerializeGet.Get ()
requireGetDomain expected actual
  | expected == actual = pure ()
  | otherwise = fail ("wrong canonical domain " <> show actual)

decodeNestedGet :: SerializeGet.Get value -> ByteString -> SerializeGet.Get value
decodeNestedGet getter bytes =
  admitGet "nested canonical payload" (decodeLiveCanonical getter bytes)

admitGet :: (Show problem) => String -> Either problem value -> SerializeGet.Get value
admitGet label = either (fail . ((label <> ": ") <>) . show) pure

getRawLiveEnvelope :: SerializeGet.Get RawLiveEnvelope
getRawLiveEnvelope = do
  domainTag <- getFramedBytes
  _requestHome <- getIdentityBytes
  _requestSequence <- SerializeGet.getWord64be
  expectedTag <- SerializeGet.getWord8
  hasExpectedIndex <- case expectedTag of
    0 -> pure False
    1 -> do
      _expectedIndex <- SerializeGet.getWord64be
      pure True
    _ -> fail ("invalid expected-index tag " <> show expectedTag)
  _home <- getIdentityBytes
  commandTag <- SerializeGet.getWord8
  commandPayload <- getFramedBytes
  _retirement <- getTrailingOracleProgress
  pure (RawLiveEnvelope domainTag hasExpectedIndex commandTag commandPayload)

-- Validate the closed grammar of every known command payload without making a
-- semantic command constructible.  Framed values owned by another canonical
-- module remain opaque here, but their presence and enclosing arm shape are
-- exact.
getKnownLiveCommandPayload :: Word8 -> SerializeGet.Get ()
getKnownLiveCommandPayload commandTag = case commandTag of
  0 -> getIdentities 3
  1 -> do
    _process <- getIdentityBytes
    getHeraldSubmittedProcessEndReason
  2 -> do
    getIdentities 3
    getRequiredFramedBytes
    _ <- getMaybeInitialLabelEvidence
    getMaybeLiveAuthority
    getMaybePriorJustification
    getLabelTarget
    getRequiredFramedBytes
  8 -> getLabelCompletionAttestation
  9 -> getIdentities 3
  10 -> do
    getRequiredFramedBytes
    getIdentities 1
    resultTag <- SerializeGet.getWord8
    if resultTag <= 1
      then pure ()
      else fail ("invalid failure-probe result tag " <> show resultTag)
  11 -> getRequiredFramedBytes
  12 -> do
    getRequiredFramedBytes
    getIdentities 1
  tag | tag >= 13 && tag <= 17 -> getTypedLiveCommand tag >> pure ()
  18 -> getTypedLiveCommand 18 >> pure ()
  19 -> getTypedLiveCommand 19 >> pure ()
  20 -> getTypedLiveCommand 20 >> pure ()
  21 -> getTypedLiveCommand 21 >> pure ()
  _ -> fail "unreachable Oracle command tag"

-- Return the control coordinate embedded by those projection events which
-- carry one.  The applied-entry decoder uses it to reject an otherwise
-- well-framed event attached to the wrong control entry.
getKnownLiveProjectionEventPayload ::
  Word8 -> SerializeGet.Get (Maybe Word64)
getKnownLiveProjectionEventPayload eventTag = case eventTag of
  0 -> getLiveProcessRecord >> pure Nothing
  1 -> do
    _process <- getIdentityBytes
    index <- SerializeGet.getWord64be
    getProcessEndReason
    pure (Just index)
  2 -> fmap (fmap controlIndexWord64 . projectionEventControlIndex) (getTypedLiveProjectionEvent 2)
  10 -> getIdentities 1 >> getDigests 1 >> pure Nothing
  11 -> getRequiredFramedBytes >> getIdentities 2 >> getRequiredFramedBytes >> pure Nothing
  12 -> do
    getRequiredFramedBytes
    getIdentities 1
    resultTag <- SerializeGet.getWord8
    if resultTag <= 1
      then pure Nothing
      else fail ("invalid failure-probe result tag " <> show resultTag)
  13 -> getRequiredFramedBytes >> getRequiredFramedBytes >> pure Nothing
  14 -> do
    generation <-
      getRequiredFramedValue
        >>= admitGet "Herald membership generation"
          . decodeHeraldMembershipGenerationCanonicalBytes
    pure
      ( controlIndexWord64
          <$> heraldMembershipGenerationChangeControlIndex generation
      )
  15 -> do
    getRequiredFramedBytes
    getRequiredFramedBytes
    getIdentities 1
    pure Nothing
  16 -> getRequiredFramedBytes >> getIdentities 1 >> pure Nothing
  tag | tag >= 17 && tag <= 21 -> fmap (fmap controlIndexWord64 . projectionEventControlIndex) (getTypedLiveProjectionEvent tag)
  tag | tag >= 22 && tag <= 26 -> fmap (fmap controlIndexWord64 . projectionEventControlIndex) (getTypedLiveProjectionEvent tag)
  _ -> fail "unreachable projection-event tag"

getLiveProcessRecord :: SerializeGet.Get ()
getLiveProcessRecord = do
  getIdentities 3
  originTag <- SerializeGet.getWord8
  case originTag of
    0 -> pure ()
    1 -> SerializeGet.getWord64be >> getOracleClientRequestId >> pure ()
    _ -> fail ("invalid process-origin tag " <> show originTag)
  lifecycleTag <- SerializeGet.getWord8
  case lifecycleTag of
    0 -> pure ()
    1 -> SerializeGet.getWord64be >> getProcessEndReason
    _ -> fail ("invalid process-lifecycle tag " <> show lifecycleTag)

getLabelCompletionAttestation :: SerializeGet.Get ()
getLabelCompletionAttestation = getIdentityBytes >> getWord64s 1 >> getIdentities 3

getProcessEndReason :: SerializeGet.Get ()
getProcessEndReason = getTypedProcessEndReason >> pure ()

getHeraldSubmittedProcessEndReason :: SerializeGet.Get ()
getHeraldSubmittedProcessEndReason = do
  _ <- getTypedHeraldSubmittedProcessEndReason
  pure ()

-- The submitted End command remains narrower than the Oracle-owned process
-- lifecycle: Heralds may submit the two ordinary reasons, while only the
-- canonical retirement transition may derive 'HeraldRetired'. Projection and
-- state codecs admit the complete Domain sum so that derived retirement is
-- replayable without making it caller-constructible.
getTypedHeraldSubmittedProcessEndReason :: SerializeGet.Get ProcessEndReason
getTypedHeraldSubmittedProcessEndReason = do
  reason <- getTypedProcessEndReason
  if processEndReasonMayBeHeraldSubmitted reason
    then pure reason
    else fail "membership-derived process-End reason is not admitted by an End command"

getTypedProcessEndReason :: SerializeGet.Get ProcessEndReason
getTypedProcessEndReason = do
  bytes <- getRequiredFramedValue
  admitGet
    "process-End reason"
    (decodeProcessEndReasonCanonicalBytes bytes)

getMaybePriorJustification :: SerializeGet.Get ()
getMaybePriorJustification = do
  maybeTag <- SerializeGet.getWord8
  case maybeTag of
    0 -> pure ()
    1 -> do
      justificationTag <- SerializeGet.getWord8
      case justificationTag of
        0 -> SerializeGet.getWord64be >> pure ()
        1 -> getDigests 1
        2 -> getIdentities 1 >> SerializeGet.getWord64be >> getDigests 1
        _ -> fail ("invalid prior-justification tag " <> show justificationTag)
    _ -> fail ("invalid optional-justification tag " <> show maybeTag)

getLabelTarget :: SerializeGet.Get ()
getLabelTarget = do
  targetTag <- SerializeGet.getWord8
  case targetTag of
    0 -> getIdentities 1
    1 -> pure ()
    2 -> pure ()
    _ -> fail ("invalid label-target tag " <> show targetTag)

getRawLiveReceipt :: SerializeGet.Get RawLiveReceipt
getRawLiveReceipt = do
  domainTag <- getFramedBytes
  home <- getIdentityBytes
  sequenceNumber <- SerializeGet.getWord64be
  digest <- getDigestBytes
  index <- SerializeGet.getWord64be
  resultTag <- SerializeGet.getWord8
  result <- case resultTag of
    0 -> pure RawLiveAccepted
    1 -> RawLiveRejected <$> getLiveRejectionPayload
    2 -> getKnownFailureCommandResult >> pure RawLiveFailureAccepted
    3 -> (getRequiredFramedValue >>= admitGet "disappearance result" . DC.decodeAccepted) >> pure RawLiveFailureAccepted
    4 -> (getRequiredFramedValue >>= admitGet "voter result" . V.decodeVoterResult) >> pure RawLiveFailureAccepted
    _ -> pure (RawLiveReceiptResultInvalid resultTag)
  pure
    ( RawLiveReceipt
        domainTag
        home
        sequenceNumber
        digest
        index
        result
    )

getKnownFailureCommandResult :: SerializeGet.Get ()
getKnownFailureCommandResult = do
  resultTag <- SerializeGet.getWord8
  case resultTag of
    0 -> getRequiredFramedBytes
    1 -> do
      getRequiredFramedBytes
      readyTag <- SerializeGet.getWord8
      case readyTag of
        0 -> pure ()
        1 -> do
          getRequiredFramedBytes
          dispositionTag <- SerializeGet.getWord8
          if dispositionTag <= 1
            then getIdentities 1
            else fail ("invalid failure resolution disposition tag " <> show dispositionTag)
        _ -> fail ("invalid failure resolution presence tag " <> show readyTag)
    2 -> getRequiredFramedBytes
    3 -> getRequiredFramedBytes >> getIdentities 1
    4 -> getAcceptedVoterHostFailure >> pure ()
    _ -> fail ("invalid failure command-result tag " <> show resultTag)

getLiveRejectionPayload :: SerializeGet.Get (Either Word8 ())
getLiveRejectionPayload = do
  rejectionTag <- SerializeGet.getWord8
  if rejectionTag > 47 || rejectionTag `elem` [10, 11, 12, 15, 16, 18, 20, 23, 24, 25, 26, 27, 28, 29, 30, 31, 36, 37, 38, 39]
    then do
      remainingBytes <- SerializeGet.remaining
      _unknownPayload <- SerializeGet.getByteString remainingBytes
      pure (Left rejectionTag)
    else do
      getKnownLiveRejectionPayload rejectionTag
      pure (Right ())

getKnownLiveRejectionPayload :: Word8 -> SerializeGet.Get ()
getKnownLiveRejectionPayload rejectionTag = case rejectionTag of
  0 -> getIdentities 2
  1 -> getIdentities 1
  2 -> getWord64s 2
  3 -> getIdentities 3
  4 -> getIdentities 1
  5 -> getIdentities 1
  6 -> getIdentities 1
  7 -> getIdentities 3
  8 -> getIdentities 1
  9 -> getIdentities 2
  13 -> getIdentities 2
  14 -> getReleasedStates 2
  17 -> pure ()
  19 -> getIdentities 1
  21 -> getIdentities 2
  22 -> getIdentities 1
  32 -> getIdentities 2
  33 -> getIdentities 2
  34 -> getDigests 2
  35 -> getDigests 2
  40 -> getKnownFailureRejectionPayload
  41 -> (getRequiredFramedValue >>= admitGet "disappearance rejection" . DC.decodeRejection) >> pure ()
  42 -> (getRequiredFramedValue >>= admitGet "Herald admission rejection" . A.decodeAdmissionProblem) >> pure ()
  43 -> (getRequiredFramedValue >>= admitGet "voter rejection" . V.decodeVoterProblem) >> pure ()
  44 -> getIdentities 1
  45 -> getIdentities 2
  46 -> getWord64s 2
  47 -> getIdentities 1 >> getWord64s 2
  _ -> fail "unreachable rejection tag"

getKnownFailureRejectionPayload :: SerializeGet.Get ()
getKnownFailureRejectionPayload = do
  rejectionTag <- SerializeGet.getWord8
  case rejectionTag of
    0 -> getIdentities 2
    1 -> getIdentities 1
    2 -> getIdentities 1
    3 -> getRequiredFramedBytes
    4 -> do
      getRequiredFramedBytes
      terminalTag <- SerializeGet.getWord8
      case terminalTag of
        0 -> getRequiredFramedBytes >> getWord64s 1
        1 -> getRequiredFramedBytes >> getIdentities 1 >> getWord64s 1
        2 -> getIdentities 1 >> getWord64s 1
        3 -> getAcceptedVoterHostFailure >> pure ()
        _ -> fail ("invalid failure-probe terminal tag " <> show terminalTag)
    5 -> getIdentities 1
    6 -> do
      getRequiredFramedBytes
      getIdentities 1
      firstResult <- SerializeGet.getWord8
      secondResult <- SerializeGet.getWord8
      if firstResult <= 1 && secondResult <= 1
        then pure ()
        else fail "invalid conflicting failure-report result tag"
    7 -> do
      getRequiredFramedBytes
      dispositionTag <- SerializeGet.getWord8
      if dispositionTag <= 1
        then pure ()
        else fail ("invalid failure-probe disposition tag " <> show dispositionTag)
    8 -> getIdentities 2
    9 -> getIdentities 2
    10 -> getIdentities 1
    11 -> getIdentities 1
    _ -> fail ("unknown failure rejection tag " <> show rejectionTag)

getRawLiveEventVector :: SerializeGet.Get RawLiveEventVector
getRawLiveEventVector = do
  domainTag <- getFramedBytes
  eventCount <- getCanonicalCount
  taggedPayloads <- replicateM eventCount $ do
    eventTag <- SerializeGet.getWord8
    payload <- getFramedBytes
    pure (eventTag, payload)
  pure (RawLiveEventVector domainTag taggedPayloads)

getRawLiveAppliedEntry :: SerializeGet.Get RawLiveAppliedEntry
getRawLiveAppliedEntry = do
  domainTag <- getFramedBytes
  index <- SerializeGet.getWord64be
  home <- getIdentityBytes
  sequenceNumber <- SerializeGet.getWord64be
  digest <- getDigestBytes
  receiptBytes <- getFramedBytes
  eventBytes <- getFramedBytes
  _postStateDigest <- getOptionalOracleStateDigest
  _retirement <- getTrailingOracleProgress
  pure
    ( RawLiveAppliedEntry
        domainTag
        index
        home
        sequenceNumber
        digest
        receiptBytes
        eventBytes
    )

getFramedBytes :: SerializeGet.Get ByteString
getFramedBytes = do
  byteCount <- SerializeGet.getWord64be
  remainingBytes <- SerializeGet.remaining
  if byteCount <= fromIntegral remainingBytes
    then SerializeGet.getByteString (fromIntegral byteCount)
    else fail "framed byte count exceeds remaining input"

getRequiredFramedValue :: SerializeGet.Get ByteString
getRequiredFramedValue = do
  bytes <- getFramedBytes
  if ByteString.null bytes
    then fail "required framed value is empty"
    else pure bytes

getRequiredFramedBytes :: SerializeGet.Get ()
getRequiredFramedBytes = getRequiredFramedValue >> pure ()

requirePayloadLength :: Int -> ByteString -> SerializeGet.Get ()
requirePayloadLength expected bytes
  | ByteString.length bytes == expected = pure ()
  | otherwise =
      fail
        ( "payload length mismatch: expected "
            <> show expected
            <> ", observed "
            <> show (ByteString.length bytes)
        )

getIdentityBytes :: SerializeGet.Get ByteString
getIdentityBytes = SerializeGet.getByteString 32

getDigestBytes :: SerializeGet.Get ByteString
getDigestBytes = SerializeGet.getByteString 32

getCanonicalCount :: SerializeGet.Get Int
getCanonicalCount = do
  count <- SerializeGet.getWord64be
  if count <= fromIntegral (maxBound :: Int)
    then pure (fromIntegral count)
    else fail "canonical count exceeds host index range"

getIdentities :: Int -> SerializeGet.Get ()
getIdentities count = do
  _values <- replicateM count getIdentityBytes
  pure ()

getDigests :: Int -> SerializeGet.Get ()
getDigests count = do
  _values <- replicateM count getDigestBytes
  pure ()

getWord64s :: Int -> SerializeGet.Get ()
getWord64s count = do
  _values <- replicateM count SerializeGet.getWord64be
  pure ()

getReleasedStates :: Int -> SerializeGet.Get ()
getReleasedStates count = do
  _values <- replicateM count getReleasedState
  pure ()

getReleasedState :: SerializeGet.Get ()
getReleasedState = do
  stateTag <- SerializeGet.getWord8
  case stateTag of
    0 -> do
      labelBytes <- getRequiredFramedValue
      case decodeCanonicalValue labelBytes of
        Right value -> case viewValue value of
          LabelValue _ -> pure ()
          _ -> fail "released-state value is not a Label"
        Left problem -> fail ("invalid canonical Label: " <> show problem)
    1 -> getWord64s 1
    _ -> fail ("invalid released-state tag " <> show stateTag)

getMaybeLiveAuthority :: SerializeGet.Get ()
getMaybeLiveAuthority = do
  maybeTag <- SerializeGet.getWord8
  case maybeTag of
    0 -> pure ()
    1 -> do
      bytes <- getRequiredFramedValue
      case ByteString.uncons bytes of
        Just (0, trailing) -> requirePayloadLength 0 trailing
        Just (1, trailing) -> requirePayloadLength 72 trailing
        Just (2, trailing) -> requirePayloadLength 8 trailing
        Just (authorityTag, _) ->
          fail ("invalid live-authority tag " <> show authorityTag)
        Nothing -> fail "empty live-authority transcript"
    _ -> fail ("invalid optional-authority tag " <> show maybeTag)

projectionEventTagMayRepeat :: Word8 -> Bool
projectionEventTagMayRepeat tag = tag `elem` [1, 16, 19, 21]

firstDuplicate :: (Ord value) => [value] -> Maybe value
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (tag : tags)
      | Set.member tag seen = Just tag
      | otherwise = go (Set.insert tag seen) tags

-- | Explicit end of the exact-result and conflicting-payload retry contract.
-- Closed homes use their existing irrevocable membership record as the fence.
data OracleRequestRetirement
  = OracleRequestPrefixRetired Word64
  | OracleRequestHomeRetired
  deriving stock (Eq, Show)

data OracleProtocolDisposition
  = ConflictingOracleRequestId
      OracleClientRequestId
      OracleCommandDigest
      OracleCommandDigest
  | InvalidOracleProgress OracleClientRequestId
  | OracleProgressInactiveHome HeraldEpoch
  deriving stock (Eq, Show)

data OracleSubmissionOutcome
  = OracleSubmissionCommitted
      OracleReceipt
      AppliedOracleEntry
  | OracleSubmissionDeferred OracleClientRequestId LabelDecisionId ControlIndex
  | OracleSubmissionDuplicate OracleReceipt (Maybe AppliedOracleEntry)
  | OracleSubmissionProtocolRejected OracleProtocolDisposition (Maybe AppliedOracleEntry)
  | OracleSubmissionRetired OracleRequestRetirement
  | OracleSubmissionProgressRetired HeraldEpoch OracleProgress (Maybe AppliedOracleEntry)
  deriving stock (Eq, Show)

data OracleSubmissionOutcomeView
  = OracleSubmissionCommittedView
      OracleReceipt
      AppliedOracleEntry
  | OracleSubmissionDeferredView OracleClientRequestId LabelDecisionId ControlIndex
  | OracleSubmissionDuplicateView OracleReceipt
  | OracleSubmissionProtocolRejectedView OracleProtocolDisposition
  | OracleSubmissionRetiredView OracleRequestRetirement
  | OracleSubmissionProgressRetiredView HeraldEpoch OracleProgress (Maybe AppliedOracleEntry)
  deriving stock (Eq, Show)

oracleSubmissionOutcomeView ::
  OracleSubmissionOutcome -> OracleSubmissionOutcomeView
oracleSubmissionOutcomeView outcome = case outcome of
  OracleSubmissionCommitted receipt entry ->
    OracleSubmissionCommittedView receipt entry
  OracleSubmissionDeferred request blocker index -> OracleSubmissionDeferredView request blocker index
  OracleSubmissionDuplicate receipt _ ->
    OracleSubmissionDuplicateView receipt
  OracleSubmissionProtocolRejected problem _ ->
    OracleSubmissionProtocolRejectedView problem
  OracleSubmissionRetired retirement -> OracleSubmissionRetiredView retirement
  OracleSubmissionProgressRetired home through entry -> OracleSubmissionProgressRetiredView home through entry

oracleSubmissionAppliedEntry :: OracleSubmissionOutcome -> Maybe AppliedOracleEntry
oracleSubmissionAppliedEntry outcome = case outcome of
  OracleSubmissionCommitted _ entry -> Just entry
  OracleSubmissionDeferred {} -> Nothing
  OracleSubmissionDuplicate _ entry -> entry
  OracleSubmissionProtocolRejected _ entry -> entry
  OracleSubmissionRetired _ -> Nothing
  OracleSubmissionProgressRetired _ _ entry -> entry

data OracleRequestRecord
  = OracleRequestRecord OracleCommandDigest OracleReceipt !ByteString
  deriving stock (Eq, Show)

-- Receipts are immutable. Retain their canonical bytes once so later state
-- digest refreshes can reuse the encoding while still hashing the full state.
oracleRequestRecord :: OracleReceipt -> OracleRequestRecord
oracleRequestRecord receipt =
  OracleRequestRecord
    (oracleReceiptCommandDigest receipt)
    receipt
    (oracleReceiptCanonicalBytes receipt)

data OracleWorkflowPhase
  = OracleReleased LiveTerminalOutcome LabelOutcomeDigest LabelRecord
  deriving stock (Eq, Show)

data OracleLabelWorkflow
  = OracleLabelWorkflow LiveLabelDecision OracleWorkflowPhase
  deriving stock (Eq, Show)

-- Derived indices contain only pending installation work. Canonical state and
-- checkpoints encode the workflows once; checkpoint admission rebuilds the
-- indices from the checked membership history.
data OraclePendingWorkflow
  = OraclePendingWorkflow !OracleLabelWorkflow !(Set.Set HeraldEpoch) !HeraldEpoch
  deriving stock (Eq, Show)

data OraclePendingWorkflows = OraclePendingWorkflows
  { pendingByDecision :: !(Map LabelDecisionId OraclePendingWorkflow),
    pendingByObject :: !(Map GlobalObjectId LabelDecisionId),
    pendingByCollector :: !(Map HeraldEpoch (Set.Set LabelDecisionId)),
    pendingByParticipant :: !(Map HeraldEpoch (Set.Set LabelDecisionId))
  }
  deriving stock (Eq, Show)

emptyPendingWorkflows :: OraclePendingWorkflows
emptyPendingWorkflows = OraclePendingWorkflows Map.empty Map.empty Map.empty Map.empty

pendingWorkflow :: OraclePendingWorkflow -> OracleLabelWorkflow
pendingWorkflow (OraclePendingWorkflow workflow _ _) = workflow

pendingWorkflowCollector :: OraclePendingWorkflow -> HeraldEpoch
pendingWorkflowCollector (OraclePendingWorkflow _ _ collector) = collector

-- A completed decision needs only its outcome and captured membership for
-- semantic completion retries. Full decision/terminal evidence belongs to the
-- applied entry and pending installation; retaining it here duplicates history.
-- The checked membership generation determines the captured member set exactly.
-- Long-lived 32-byte SHA outputs must not keep their original pinned
-- allocation blocks alive. Private unpinned copies preserve nominal identity
-- and byte ordering without changing the public identity or wire formats.
newtype CompletedDecisionKey = CompletedDecisionKey ShortByteString
  deriving stock (Eq, Ord, Show)

completedDecisionKey :: LabelDecisionId -> CompletedDecisionKey
completedDecisionKey = CompletedDecisionKey . ShortByteString.toShort . labelDecisionIdBytes

newtype CompletedOutcomeDigest = CompletedOutcomeDigest ShortByteString
  deriving stock (Eq, Show)

completedDigest :: LabelOutcomeDigest -> CompletedOutcomeDigest
completedDigest = CompletedOutcomeDigest . ShortByteString.toShort . labelOutcomeDigestBytes

data CompletedWorkflow
  = CompletedWorkflow !ControlIndex !HeraldMembershipGenerationId !CompletedOutcomeDigest
  deriving stock (Eq, Show)

completedWorkflow :: LiveLabelDecision -> LabelOutcomeDigest -> CompletedWorkflow
completedWorkflow decision digest = CompletedWorkflow (liveDecisionRequestControlIndex decision) (liveDecisionMembershipGeneration decision) (completedDigest digest)

data OracleLifetime = OracleLifetime
  { receiptPromises :: !(Map HeraldEpoch ReceiptRetirement),
    labelPromises :: !(Map HeraldEpoch ControlIndex),
    labelPromiseCounts :: !(Map ControlIndex Int),
    labelRetiredThrough :: !ControlIndex,
    latestLabelDecision :: !ControlIndex,
    completedByIndex :: !(Map ControlIndex CompletedDecisionKey)
  }
  deriving stock (Eq, Show)

initialLifetime :: CheckedOracleGenesis -> OracleLifetime
initialLifetime genesis = OracleLifetime Map.empty homes (Map.singleton zero (Map.size homes)) zero zero Map.empty
  where
    zero = controlIndex 0
    homes = Map.fromList [(home, zero) | home <- Set.toAscList (failureActiveHeralds (initialFailureState genesis))]

-- | Full-state hashing is a diagnostic witness, fixed for the lifetime of the
-- owner. Operational command and workflow digests are independent of this mode.
data OracleStateDigestMode
  = OracleStateDigestDisabled
  | OracleStateDigestEnabled
  deriving stock (Eq, Show)

data OracleState
  = OracleState
      CheckedOracleGenesis
      ControlIndex
      (Map OracleClientRequestId OracleRequestRecord)
      (Map ProcessEpochId ProcessEpochRecord)
      (Map GlobalObjectId LabelRecord)
      OraclePendingWorkflows
      (Map CompletedDecisionKey CompletedWorkflow)
      FailureState
      (D.DisappearanceState, A.AdmissionState, V.VoterState, OracleLifetime)
      (Maybe OracleStateDigest)
  deriving stock (Eq, Show)

initialOracleState :: CheckedOracleGenesis -> OracleState
initialOracleState = initialOracleStateWithDigestMode OracleStateDigestDisabled

initialOracleStateWithDigestMode :: OracleStateDigestMode -> CheckedOracleGenesis -> OracleState
initialOracleStateWithDigestMode mode genesis = refreshLiveOracleStateDigest provisional
  where
    provisional =
      OracleState
        genesis
        (controlIndex 0)
        Map.empty
        (Map.fromList (fmap genesisProcessRecord (checkedOracleAppliedBootstraps genesis)))
        Map.empty
        emptyPendingWorkflows
        Map.empty
        (initialFailureState genesis)
        (D.initialDisappearanceState, A.initialAdmissionState (checkedOracleSystemId genesis) (checkedOracleActiveHeralds genesis), V.initialVoterState genesis, initialLifetime genesis)
        (case mode of OracleStateDigestDisabled -> Nothing; OracleStateDigestEnabled -> Just zeroLiveStateDigest)

genesisProcessRecord ::
  AppliedProcessBootstrap -> (ProcessEpochId, ProcessEpochRecord)
genesisProcessRecord bootstrap =
  ( process,
    ProcessEpochRecord
      (appliedProcessId bootstrap)
      process
      (appliedProcessResidence bootstrap)
      GenesisProcess
      ProcessRecordLive
  )
  where
    process = appliedProcessEpochId bootstrap

zeroLiveStateDigest :: OracleStateDigest
zeroLiveStateDigest =
  case mkOracleStateDigest (ByteString.replicate 32 0) of
    Right digest -> digest
    Left problem -> error ("zero live state digest invariant: " <> show problem)

oracleGreatestControlIndex :: OracleState -> ControlIndex
oracleGreatestControlIndex
  (OracleState _ index _ _ _ _ _ _ _ _) = index

oracleStateGenesis :: OracleState -> CheckedOracleGenesis
oracleStateGenesis (OracleState genesis _ _ _ _ _ _ _ _ _) = genesis

oracleStateDigestMode :: OracleState -> OracleStateDigestMode
oracleStateDigestMode state = case oracleStateDigest state of
  Nothing -> OracleStateDigestDisabled
  Just _ -> OracleStateDigestEnabled

oracleStateDigest :: OracleState -> Maybe OracleStateDigest
oracleStateDigest
  (OracleState _ _ _ _ _ _ _ _ _ digest) = digest

oracleCurrentMembership :: OracleState -> HeraldMembershipGeneration
oracleCurrentMembership = failureCurrentMembership . stateFailure

oracleCheckedMembershipHistory :: OracleState -> HeraldMembershipHistory
oracleCheckedMembershipHistory = failureCheckedMembershipHistory . stateFailure

oracleMembershipHistory :: OracleState -> [HeraldMembershipGeneration]
oracleMembershipHistory = failureMembershipHistory . stateFailure

oracleRetiredHeralds :: OracleState -> [HeraldEpoch]
oracleRetiredHeralds = failureRetiredHeralds . stateFailure

oracleFailureProbe ::
  HeraldFailureProbeId -> OracleState -> Maybe FailureProbeView
oracleFailureProbe identifier = failureProbe identifier . stateFailure

oracleActiveFailureProbeFor ::
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  OracleState ->
  Maybe HeraldFailureProbeId
oracleActiveFailureProbeFor target generation =
  failureActiveProbeFor target generation . stateFailure

oracleRequestCount :: OracleState -> Int
oracleRequestCount
  (OracleState _ _ requests _ _ _ _ _ _ _) = Map.size requests

-- | A retirement marker is checked before ordinary receipt lookup. In
-- particular, a fresh ID from an irrevocably closed home cannot grow history.
oracleRequestRetirement :: OracleClientRequestId -> OracleState -> Maybe OracleRequestRetirement
oracleRequestRetirement request state
  | home `elem` oracleRetiredHeralds state = Just OracleRequestHomeRetired
  | Just through <- oracleReceiptRetirementProgress home state,
    Lifetime.receiptIsRetired (oracleClientRequestSequence request) through =
      Just (OracleRequestPrefixRetired (retirementHighWater through))
  | otherwise = Nothing
  where
    home = oracleClientRequestHome request

oracleReceiptRetirementFrontier :: HeraldEpoch -> OracleState -> Maybe Word64
oracleReceiptRetirementFrontier home = fmap retirementHighWater . oracleReceiptRetirementProgress home

oracleReceiptRetirementProgress :: HeraldEpoch -> OracleState -> Maybe ReceiptRetirement
oracleReceiptRetirementProgress home = Map.lookup home . stateReceiptRetirementFrontiers

oracleReceiptRetirementFrontiers :: OracleState -> [(HeraldEpoch, Word64)]
oracleReceiptRetirementFrontiers = fmap (fmap retirementHighWater) . oracleReceiptRetirementProgresses

oracleReceiptRetirementProgresses :: OracleState -> [(HeraldEpoch, ReceiptRetirement)]
oracleReceiptRetirementProgresses = Map.toAscList . stateReceiptRetirementFrontiers

stateLifetime :: OracleState -> OracleLifetime
stateLifetime (OracleState _ _ _ _ _ _ _ _ (_, _, _, lifetime) _) = lifetime

setStateLifetime :: OracleLifetime -> OracleState -> OracleState
setStateLifetime lifetime (OracleState genesis index requests processes labels workflow completed failure (disappearance, admission, voter, _) digest) =
  OracleState genesis index requests processes labels workflow completed failure (disappearance, admission, voter, lifetime) digest

stateReceiptRetirementFrontiers :: OracleState -> Map HeraldEpoch ReceiptRetirement
stateReceiptRetirementFrontiers = receiptPromises . stateLifetime

setStateReceiptRetirement :: ControlIndex -> Map OracleClientRequestId OracleRequestRecord -> Map HeraldEpoch ReceiptRetirement -> OracleState -> OracleState
setStateReceiptRetirement index requests frontiers (OracleState genesis _ _ processes labels workflow completed failure (disappearance, admission, voter, lifetime) digest) =
  OracleState genesis index requests processes labels workflow completed failure (disappearance, admission, voter, lifetime {receiptPromises = frontiers}) digest

oracleProgresses :: OracleState -> [(HeraldEpoch, OracleProgress)]
oracleProgresses state = [(home, progressFor home through) | (home, through) <- Map.toAscList (labelPromises lifetime)]
  where
    lifetime = stateLifetime state
    progressFor home through = oracleProgress (Map.findWithDefault mempty home (receiptPromises lifetime)) through

oracleProgressFor :: HeraldEpoch -> OracleState -> OracleProgress
oracleProgressFor home state = oracleProgress (Map.findWithDefault mempty home (receiptPromises lifetime)) (Map.findWithDefault (controlIndex 0) home (labelPromises lifetime))
  where
    lifetime = stateLifetime state

oracleLabelRetiredThrough :: OracleState -> ControlIndex
oracleLabelRetiredThrough = labelRetiredThrough . stateLifetime

oracleLatestLabelDecision :: OracleState -> ControlIndex
oracleLatestLabelDecision = latestLabelDecision . stateLifetime

setLatestLabelDecision :: ControlIndex -> OracleState -> OracleState
setLatestLabelDecision index state = setStateLifetime (stateLifetime state) {latestLabelDecision = index} state

completedControlIndex :: CompletedWorkflow -> ControlIndex
completedControlIndex (CompletedWorkflow index _ _) = index

insertCompletedWorkflow :: LabelDecisionId -> CompletedWorkflow -> OracleState -> OracleState
insertCompletedWorkflow ident witness state
  | index <= oracleLabelRetiredThrough state = state
  | otherwise = setStateLifetime lifetime {completedByIndex = Map.insert index key (completedByIndex lifetime)} (setStateCompleted (Map.insert key witness (stateCompleted state)) state)
  where
    lifetime = stateLifetime state
    key = completedDecisionKey ident
    index = completedControlIndex witness

changePromiseCount :: Int -> ControlIndex -> Map ControlIndex Int -> Map ControlIndex Int
changePromiseCount difference = Map.alter update
  where
    update old = case maybe 0 id old + difference of
      0 -> Nothing
      count -> Just count

-- Only a changed minimum visits completed entries, and only the retired prefix
-- is traversed. Pending installations never enter this index.
advanceLabelFloor :: OracleState -> OracleState
advanceLabelFloor state
  | floorIndex <= labelRetiredThrough lifetime = state
  | otherwise = setStateLifetime lifetime {labelRetiredThrough = floorIndex, completedByIndex = retainedIndex} (setStateCompleted retained state)
  where
    lifetime = stateLifetime state
    floorIndex = case Map.lookupMin (labelPromiseCounts lifetime) of
      Just (through, _) -> max through (labelRetiredThrough lifetime)
      Nothing -> error "Oracle active membership has no label promise"
    (before, atFloor, retainedIndex) = Map.splitLookup floorIndex (completedByIndex lifetime)
    removed = maybe (Map.elems before) (: Map.elems before) atFloor
    retained = foldl' (flip Map.delete) (stateCompleted state) removed

advanceLabelPromise :: HeraldEpoch -> ControlIndex -> OracleState -> OracleState
advanceLabelPromise home offered state
  | offered <= previous = state
  | otherwise = advanceLabelFloor (setStateLifetime lifetime {labelPromises = Map.insert home offered (labelPromises lifetime), labelPromiseCounts = counts} state)
  where
    lifetime = stateLifetime state
    previous = Map.findWithDefault (error "admitted Oracle home lacks label promise") home (labelPromises lifetime)
    counts = changePromiseCount 1 offered (changePromiseCount (-1) previous (labelPromiseCounts lifetime))

-- Canonical activation adopts the full label high water already replayed by
-- the joining observer; canonical retirement alone removes a promise.
reconcileProgressMembership :: OracleState -> OracleState -> OracleState
reconcileProgressMembership previous state
  | before == after = state
  | otherwise = advanceLabelFloor (setStateLifetime updated state)
  where
    before = stateActiveHeralds previous
    after = stateActiveHeralds state
    lifetime = stateLifetime state
    remove current home = case Map.lookup home (labelPromises current) of
      Nothing -> error "retired Oracle member lacks label promise"
      Just through -> current {labelPromises = Map.delete home (labelPromises current), labelPromiseCounts = changePromiseCount (-1) through (labelPromiseCounts current)}
    add current home = current {labelPromises = Map.insert home (latestLabelDecision current) (labelPromises current), labelPromiseCounts = changePromiseCount 1 (latestLabelDecision current) (labelPromiseCounts current)}
    updated = Set.foldl' add (Set.foldl' remove lifetime (before Set.\\ after)) (after Set.\\ before)

reclaimClosedHomeReceipts :: OracleState -> OracleState -> OracleState
reclaimClosedHomeReceipts previous state
  | oracleRetiredHeralds previous == oracleRetiredHeralds state = state
  | otherwise =
      setStateReceiptRetirement (oracleGreatestControlIndex state) requests frontiers state
  where
    closed = Set.fromList (oracleRetiredHeralds state)
    requests = Map.filterWithKey (\request _ -> not (Set.member (oracleClientRequestHome request) closed)) (stateRequests state)
    frontiers = Map.withoutKeys (stateReceiptRetirementFrontiers state) closed

oracleRequestReceipt ::
  OracleClientRequestId -> OracleState -> Maybe OracleReceipt
oracleRequestReceipt requestId state = do
  OracleRequestRecord _ receipt _ <- Map.lookup requestId (stateRequests state)
  pure receipt

oracleProcessRecord ::
  ProcessEpochId -> OracleState -> Maybe ProcessEpochRecord
oracleProcessRecord
  process
  (OracleState _ _ _ processes _ _ _ _ _ _) = Map.lookup process processes

oracleProcessRecords :: OracleState -> [ProcessEpochRecord]
oracleProcessRecords
  (OracleState _ _ _ processes _ _ _ _ _ _) = Map.elems processes

oracleLabelRecord ::
  GlobalObjectId -> OracleState -> Maybe LabelRecord
oracleLabelRecord
  object
  (OracleState _ _ _ _ labels _ _ _ _ _) = Map.lookup object labels

oracleLabelRecords :: OracleState -> [LabelRecord]
oracleLabelRecords
  (OracleState _ _ _ _ labels _ _ _ _ _) = Map.elems labels

-- | Pending decisions in canonical decision-identity order. Ordinary admission
-- and completion use the keyed queries below.
oracleOpenDecisions :: OracleState -> [LiveLabelDecision]
oracleOpenDecisions = fmap (workflowDecision . pendingWorkflow) . Map.elems . pendingByDecision . statePendingWorkflows

oracleOpenDecision :: LabelDecisionId -> OracleState -> Maybe LiveLabelDecision
oracleOpenDecision decision state = workflowDecision . pendingWorkflow <$> Map.lookup decision (pendingByDecision (statePendingWorkflows state))

oracleObjectDecision :: GlobalObjectId -> OracleState -> Maybe LiveLabelDecision
oracleObjectDecision object state = do
  decision <- Map.lookup object (pendingByObject (statePendingWorkflows state))
  oracleOpenDecision decision state

oracleWorkflowPhase :: LabelDecisionId -> OracleState -> Maybe LiveWorkflowPhaseView
oracleWorkflowPhase decision state = workflowPhaseView . pendingWorkflow <$> Map.lookup decision (pendingByDecision (statePendingWorkflows state))

-- | Collector assignment is maintained with the pending decision. Canonical
-- retirement visits only the retiring participant's indexed decisions.
oracleLabelCollector :: LabelDecisionId -> OracleState -> Maybe HeraldEpoch
oracleLabelCollector decision state = pendingWorkflowCollector <$> Map.lookup decision (pendingByDecision (statePendingWorkflows state))

workflowCollector :: OracleState -> LiveLabelDecision -> Maybe HeraldEpoch
workflowCollector state decision
  | Set.member home survivors = Just home
  | otherwise = Set.lookupMin survivors
  where
    home = liveDecisionHome decision
    survivors = workflowSurvivors state decision

workflowSurvivors :: OracleState -> LiveLabelDecision -> Set.Set HeraldEpoch
workflowSurvivors state decision =
  Set.intersection (Set.fromList (capturedDecisionHeralds state decision)) (stateActiveHeralds state)

capturedDecisionHeralds :: OracleState -> LiveLabelDecision -> [HeraldEpoch]
capturedDecisionHeralds state decision = case liveDecisionCapturedHeralds (oracleCheckedMembershipHistory state) decision of
  Just captured -> captured
  Nothing -> error "canonical label decision lost checked membership history"

oracleTerminalOutcome :: LabelDecisionId -> OracleState -> Maybe LiveTerminalOutcome
oracleTerminalOutcome decision state = do
  OraclePendingWorkflow (OracleLabelWorkflow _ (OracleReleased terminal _ _)) _ _ <- Map.lookup decision (pendingByDecision (statePendingWorkflows state))
  pure terminal

oracleCompletedWorkflowCount :: OracleState -> Int
oracleCompletedWorkflowCount
  (OracleState _ _ _ _ _ _ completed _ _ _) = Map.size completed

statePendingWorkflows :: OracleState -> OraclePendingWorkflows
statePendingWorkflows (OracleState _ _ _ _ _ workflows _ _ _ _) = workflows

stateWorkflowEntries :: OracleState -> [(LabelDecisionId, OracleLabelWorkflow)]
stateWorkflowEntries = fmap (fmap pendingWorkflow) . Map.toAscList . pendingByDecision . statePendingWorkflows

insertPendingWorkflow :: OracleLabelWorkflow -> OracleState -> OracleState
insertPendingWorkflow workflow state = setStatePendingWorkflows inserted state
  where
    decision = workflowDecision workflow
    ident = liveDecisionId decision
    survivors = workflowSurvivors state decision
    collector = maybe (error "new pending decision without a captured survivor") id (workflowCollector state decision)
    pending = statePendingWorkflows state
    inserted =
      pending
        { pendingByDecision = Map.insert ident (OraclePendingWorkflow workflow survivors collector) (pendingByDecision pending),
          pendingByObject = Map.insert (liveDecisionObject decision) ident (pendingByObject pending),
          pendingByCollector = insertPendingIndex collector ident (pendingByCollector pending),
          pendingByParticipant = Set.foldl' (\index peer -> insertPendingIndex peer ident index) (pendingByParticipant pending) survivors
        }

deletePendingWorkflow :: OracleLabelWorkflow -> OracleState -> OracleState
deletePendingWorkflow workflow state = case Map.lookup ident (pendingByDecision pending) of
  Nothing -> error "completed workflow missing pending installation"
  Just (OraclePendingWorkflow _ survivors collector) ->
    setStatePendingWorkflows
      pending
        { pendingByDecision = Map.delete ident (pendingByDecision pending),
          pendingByObject = Map.delete (liveDecisionObject decision) (pendingByObject pending),
          pendingByCollector = deletePendingIndex collector ident (pendingByCollector pending),
          pendingByParticipant = Set.foldl' (\index peer -> deletePendingIndex peer ident index) (pendingByParticipant pending) survivors
        }
      state
  where
    pending = statePendingWorkflows state
    decision = workflowDecision workflow
    ident = liveDecisionId decision

insertPendingIndex :: HeraldEpoch -> LabelDecisionId -> Map HeraldEpoch (Set.Set LabelDecisionId) -> Map HeraldEpoch (Set.Set LabelDecisionId)
insertPendingIndex peer ident = Map.insertWith Set.union peer (Set.singleton ident)

deletePendingIndex :: HeraldEpoch -> LabelDecisionId -> Map HeraldEpoch (Set.Set LabelDecisionId) -> Map HeraldEpoch (Set.Set LabelDecisionId)
deletePendingIndex peer ident = Map.update (\decisions -> let remaining = Set.delete ident decisions in if Set.null remaining then Nothing else Just remaining) peer

workflowDecision :: OracleLabelWorkflow -> LiveLabelDecision
workflowDecision (OracleLabelWorkflow decision _) = decision

workflowPhaseView :: OracleLabelWorkflow -> LiveWorkflowPhaseView
workflowPhaseView (OracleLabelWorkflow _ (OracleReleased {})) = LiveReleasedView

-- | Apply an already-proposed replicated envelope. Busy installation deferral
-- is repeated here so a proposal race cannot become a durable rejection.
submitOracleState ::
  OracleEnvelope ->
  OracleState ->
  (OracleState, OracleSubmissionOutcome)
submitOracleState envelope state
  | Just through <- oracleCommandProgress (oracleEnvelopeCommand envelope) =
      retireProgress envelope through state
  | Just retirement <- oracleRequestRetirement requestId state =
      (state, OracleSubmissionRetired retirement)
  | Left problem <- validatePiggybackRetirement envelope state =
      (state, OracleSubmissionProtocolRejected problem Nothing)
  | otherwise =
      case Map.lookup requestId (stateRequests state) of
        Just (OracleRequestRecord retainedDigest receipt _)
          | retainedDigest == commandDigest ->
              retainedOutcome (OracleSubmissionDuplicate receipt)
          | otherwise ->
              retainedOutcome
                (OracleSubmissionProtocolRejected (ConflictingOracleRequestId requestId retainedDigest commandDigest))
        Nothing -> case oracleSubmissionPreflightBlocker envelope state of
          Just blocker -> (state, OracleSubmissionDeferred requestId blocker (oracleGreatestControlIndex state))
          Nothing -> commitFreshLiveEnvelope envelope state
  where
    requestId = oracleEnvelopeRequestId envelope
    commandDigest = oracleEnvelopeDigest envelope
    retainedOutcome outcome = case oracleEnvelopeProgress envelope of
      Nothing -> (state, outcome Nothing)
      Just through ->
        let home = oracleEnvelopeHomeHeraldEpoch envelope
            maintenance = oracleEnvelope (oracleClientRequestId home (retirementHighWater (oracleProgressReceipts through))) Nothing home (RetireOracleProgress through)
            (successor, acknowledgement) = retireProgress maintenance through state
         in (successor, outcome (oracleSubmissionAppliedEntry acknowledgement))

validatePiggybackRetirement :: OracleEnvelope -> OracleState -> Either OracleProtocolDisposition ()
validatePiggybackRetirement envelope state = case oracleEnvelopeProgress envelope of
  Nothing -> Right ()
  Just progress
    | oracleClientRequestHome request /= home
        || Lifetime.receiptIsRetired (oracleClientRequestSequence request) (oracleProgressReceipts progress)
        || oracleCommandProgress (oracleEnvelopeCommand envelope) /= Nothing
        || oracleProgressLabelsThrough progress > oracleLatestLabelDecision state ->
        Left (InvalidOracleProgress request)
    | not (Set.member home (stateActiveHeralds state)) ->
        Left (OracleProgressInactiveHome home)
    | otherwise -> Right ()
  where
    request = oracleEnvelopeRequestId envelope
    home = oracleEnvelopeHomeHeraldEpoch envelope

-- Metadata shares the fresh command's canonical index. Independent components
-- trigger only their own reclamation work.
applyPiggybackRetirement :: OracleEnvelope -> OracleState -> OracleState
applyPiggybackRetirement envelope state = case oracleEnvelopeProgress envelope of
  Nothing -> state
  Just progress -> advanceProgress (oracleEnvelopeHomeHeraldEpoch envelope) progress state

advanceReceiptPrefix :: HeraldEpoch -> ReceiptRetirement -> OracleState -> OracleState
advanceReceiptPrefix home through state
  | previous <> through == previous = state
  | otherwise = setStateReceiptRetirement (oracleGreatestControlIndex state) requests frontiers state
  where
    previous = Map.findWithDefault mempty home (stateReceiptRetirementFrontiers state)
    combined = previous <> through
    frontiers = Map.insert home combined (stateReceiptRetirementFrontiers state)
    requests = Map.filterWithKey (\ident _ -> oracleClientRequestHome ident /= home || not (Lifetime.receiptIsRetired (oracleClientRequestSequence ident) combined)) (stateRequests state)

advanceProgress :: HeraldEpoch -> OracleProgress -> OracleState -> OracleState
advanceProgress home progress = advanceLabelPromise home (oracleProgressLabelsThrough progress) . advanceReceiptPrefix home (oracleProgressReceipts progress)

-- Maintenance has exact full-progress correlation, but uses only the receipt
-- high water as routing request sequence and consumes no ordinary request ID.
retireProgress :: OracleEnvelope -> OracleProgress -> OracleState -> (OracleState, OracleSubmissionOutcome)
retireProgress envelope progress state
  | oracleClientRequestHome request /= home
      || oracleClientRequestSequence request /= retirementHighWater (oracleProgressReceipts progress)
      || oracleEnvelopeExpectedControlIndex envelope /= Nothing
      || oracleEnvelopeProgress envelope /= Nothing
      || oracleProgressLabelsThrough progress > oracleLatestLabelDecision state =
      (state, OracleSubmissionProtocolRejected (InvalidOracleProgress request) Nothing)
  | home `elem` oracleRetiredHeralds state =
      (state, OracleSubmissionRetired OracleRequestHomeRetired)
  | not (Set.member home (stateActiveHeralds state)) =
      (state, OracleSubmissionProtocolRejected (OracleProgressInactiveHome home) Nothing)
  | oracleProgressCovers previous progress =
      (state, OracleSubmissionProgressRetired home previous Nothing)
  | otherwise =
      (successor, OracleSubmissionProgressRetired home combined (Just entry))
  where
    request = oracleEnvelopeRequestId envelope
    home = oracleEnvelopeHomeHeraldEpoch envelope
    previous = oracleProgressFor home state
    combined = previous <> progress
    index = controlIndex (controlIndexWord64 (oracleGreatestControlIndex state) + 1)
    advanced = advanceProgress home progress state
    successor = refreshLiveOracleStateDigest (setStateReceiptRetirement index (stateRequests advanced) (stateReceiptRetirementFrontiers advanced) advanced)
    entry = AppliedOracleEntry index (AppliedProgressOrigin home combined) [] (oracleStateDigest successor)

commitFreshLiveEnvelope ::
  OracleEnvelope ->
  OracleState ->
  (OracleState, OracleSubmissionOutcome)
commitFreshLiveEnvelope envelope state =
  (successor, OracleSubmissionCommitted receipt entry)
  where
    requestId = oracleEnvelopeRequestId envelope
    commandDigest = oracleEnvelopeDigest envelope
    nextIndex = controlIndex (controlIndexWord64 (oracleGreatestControlIndex state) + 1)
    (result, acceptedResult, semanticSuccessor, events) =
      case applyFreshLiveCommand nextIndex envelope state of
        Left rejection ->
          (OracleRejected rejection, Nothing, state, [])
        Right (updated, failureResult, acceptedEvents) ->
          let (admitted, events') = reconcileHeraldAdmission nextIndex failureResult acceptedEvents updated
           in (OracleAccepted, failureResult, admitted, events')
    receipt =
      OracleReceipt requestId commandDigest nextIndex result acceptedResult
    successor =
      refreshLiveOracleStateDigest
        ( reconcileProgressMembership
            state
            ( reclaimClosedHomeReceipts
                state
                ( setStateRequestAndIndex
                    nextIndex
                    requestId
                    (oracleRequestRecord receipt)
                    (applyPiggybackRetirement envelope semanticSuccessor)
                )
            )
        )
    entry =
      AppliedOracleEntry
        nextIndex
        (AppliedCommandOrigin (AppliedOracleCommandEntry requestId commandDigest receipt (oracleEnvelopeProgress envelope)))
        events
        (oracleStateDigest successor)

applyFreshLiveCommand ::
  ControlIndex ->
  OracleEnvelope ->
  OracleState ->
  Either
    OracleRejection
    (OracleState, Maybe OracleAcceptedResult, [OracleProjectionEvent])
applyFreshLiveCommand nextIndex envelope state = do
  validateLiveEnvelope envelope state
  case oracleEnvelopeCommand envelope of
    RetireOracleProgress {} -> error "receipt maintenance reached ordinary command admission"
    VoterCommand command -> applyLiveVoter nextIndex command state
    AdmissionCommand command -> applyLiveAdmission nextIndex (oracleEnvelopeHomeHeraldEpoch envelope) command state
    StartProcessEpoch bootstrap ->
      ordinary
        (applyLiveStart nextIndex (oracleEnvelopeRequestId envelope) bootstrap (oracleEnvelopeHomeHeraldEpoch envelope) state)
    EndProcessEpoch process reason ->
      ordinary
        (applyLiveEnd nextIndex process reason (oracleEnvelopeHomeHeraldEpoch envelope) state)
    DecideLabel decision caller object expected initialEvidence authority justification target acceptanceCut ->
      ordinary
        ( applyLiveDecide
            nextIndex
            (oracleEnvelopeRequestId envelope)
            (oracleEnvelopeHomeHeraldEpoch envelope)
            decision
            caller
            object
            expected
            initialEvidence
            authority
            justification
            target
            acceptanceCut
            state
        )
    CompleteLabelDecision report ->
      ordinary (applyLiveCompletion (oracleEnvelopeHomeHeraldEpoch envelope) report state)
    OpenHeraldFailureProbe target generation configuration -> do
      requireNoVoterChange state
      let captured = oracleVoterConfiguration state
      unless (configuration == V.voterConfigurationId captured) (Left (FailureCommandRejected (StaleFailureVoterConfiguration configuration (V.voterConfigurationId captured))))
      applyLiveFailure
        (openFailureProbe nextIndex target generation captured (stateFailure state))
        state
    ReportHeraldFailureProbe probe configuration result ->
      applyLiveFailure
        ( reportFailureProbe
            probe
            configuration
            (oracleEnvelopeHomeHeraldEpoch envelope)
            result
            (stateFailure state)
        )
        state
    DismissHeraldFailureProbe resolution ->
      applyLiveFailure
        ( dismissFailureProbe
            nextIndex
            (oracleEnvelopeHomeHeraldEpoch envelope)
            resolution
            (stateFailure state)
        )
        state
    AcceptVoterHostFailure resolution -> applyLiveAcceptedHostFailure nextIndex (oracleEnvelopeHomeHeraldEpoch envelope) resolution state
    RetireHeraldEpoch resolution target ->
      applyLiveRetirement
        nextIndex
        (oracleEnvelopeHomeHeraldEpoch envelope)
        resolution
        target
        state
    OpenDisappearanceProbe x0 x1 -> applyLiveDisappearance nextIndex (oracleEnvelopeHomeHeraldEpoch envelope) (D.DisappearanceOpen x0 x1) state
    ReportPredefinedAbsence x0 x1 -> applyLiveDisappearance nextIndex (oracleEnvelopeHomeHeraldEpoch envelope) (D.DisappearanceReport x0 x1) state
    InvalidateDisappearanceProbe x0 x1 x2 x3 -> applyLiveDisappearance nextIndex (oracleEnvelopeHomeHeraldEpoch envelope) (D.DisappearanceInvalidate x0 x1 x2 x3) state
    ResolveDisappearanceProbe x0 x1 -> applyLiveDisappearance nextIndex (oracleEnvelopeHomeHeraldEpoch envelope) (D.DisappearanceResolve x0 x1) state
    AbortDisappearanceProbe x0 x1 -> applyLiveDisappearance nextIndex (oracleEnvelopeHomeHeraldEpoch envelope) (D.DisappearanceAbort x0 x1) state
  where
    ordinary = fmap (\(updated, events) -> (updated, Nothing, events))

applyLiveFailure ::
  Either
    FailureRejection
    (FailureState, FailureCommandResult, [FailureEvent]) ->
  OracleState ->
  Either
    OracleRejection
    (OracleState, Maybe OracleAcceptedResult, [OracleProjectionEvent])
applyLiveFailure outcome state = do
  (failure, result, events) <- either (Left . FailureCommandRejected) Right outcome
  Right
    ( setStateFailure failure state,
      Just (AcceptedFailure result),
      fmap projectFailureEvent events
    )

applyLiveRetirement ::
  ControlIndex ->
  HeraldEpoch ->
  FailureProbeResolutionId ->
  HeraldEpoch ->
  OracleState ->
  Either
    OracleRejection
    (OracleState, Maybe OracleAcceptedResult, [OracleProjectionEvent])
applyLiveRetirement nextIndex reporter resolution target state = do
  (failure, result, failureEvents) <-
    either
      (Left . FailureCommandRejected)
      Right
      ( retireHeraldEpoch
          nextIndex
          reporter
          resolution
          target
          exclusionApplied
          (stateFailure state)
      )
  if null failureEvents
    then Right (state, Just (AcceptedFailure result), [])
    else applyRetirementConsequences failure result failureEvents
  where
    certificate = case oracleFailureProbe (failureProbeResolutionProbeId resolution) state of
      Just (FailureProbeView _ _ _ _ _ _ (Just (ProbeVoterHostFailureAcceptedView accepted))) -> Just accepted
      _ -> Nothing
    exclusionApplied = case certificate >>= (\accepted -> oracleVoterChange (F.acceptedVoterHostFailureChangeId accepted) state) of
      Just change -> V.voterChangeReason change == V.AcceptedHostFailureReason resolution && V.voterChangePhase change == V.VoterExcludedAwaitingHeraldRetirement
      Nothing -> False
    applyRetirementConsequences failure result failureEvents = do
      (voter, completionEvents) <- case certificate of
        Nothing -> pure (stateVoter state, [])
        Just accepted -> do
          (updated, change) <- either (Left . VoterCommandRejected) Right (V.completeAcceptedHostFailure nextIndex resolution (F.acceptedVoterHostFailureChangeId accepted) (stateVoter state))
          pure (updated, [OracleVoterChangeChanged change])
      let (processes, processEvents) =
            retireResidentProcesses nextIndex target (stateProcesses state)
          withMembership =
            setStateProcesses processes
              . setStateFailure failure
              . setStateVoter voter
              $ state
          (disappearance, abortEvents) = D.abortDisappearanceForMembership nextIndex (heraldMembershipGenerationId (failureCurrentMembership failure)) (stateDisappearance state)
          (settled, workflowEvents) =
            settleWorkflowsForRetirement target (setStateDisappearance disappearance withMembership)
      Right
        ( settled,
          Just (AcceptedFailure result),
          fmap projectFailureEvent failureEvents
            <> fmap projectDisappearanceEvent abortEvents
            <> processEvents
            <> workflowEvents
            <> completionEvents
        )

projectFailureEvent :: FailureEvent -> OracleProjectionEvent
projectFailureEvent event = case event of
  VoterHostFailureAcceptedEvent certificate -> OracleVoterHostFailureAccepted certificate
  FailureProbeOpenedEvent probe target generation configuration ->
    OracleFailureProbeOpened probe target generation configuration
  FailureProbeReportRecordedEvent probe reporter result ->
    OracleFailureProbeReportRecorded probe reporter result
  FailureProbeDismissedEvent probe resolution ->
    OracleFailureProbeDismissed probe resolution
  HeraldMembershipAdvancedEvent generation ->
    HeraldMembershipAdvanced generation
  FailureProbeRetiredEvent probe resolution successor ->
    OracleFailureProbeRetired probe resolution successor
  FailureProbeSupersededEvent probe successor ->
    OracleFailureProbeSuperseded probe successor

retireResidentProcesses ::
  ControlIndex ->
  HeraldEpoch ->
  Map ProcessEpochId ProcessEpochRecord ->
  (Map ProcessEpochId ProcessEpochRecord, [OracleProjectionEvent])
retireResidentProcesses index target =
  \processes -> Map.foldrWithKey retireOne (processes, []) processes
  where
    retireOne process record (updated, events)
      | processRecordResidence record /= target = (updated, events)
      | processRecordLifecycleInternal record /= ProcessRecordLive = (updated, events)
      | otherwise =
          ( Map.insert
              process
              (setProcessLifecycle (ProcessRecordEnded index HeraldRetired) record)
              updated,
            ProcessEpochEnded process index HeraldRetired : events
          )

settleWorkflowsForRetirement :: HeraldEpoch -> OracleState -> (OracleState, [OracleProjectionEvent])
settleWorkflowsForRetirement retired state = let (updated, events) = Set.foldl' retireOne (withoutParticipant, []) affected in (updated, reverse events)
  where
    pending = statePendingWorkflows state
    affected = Map.findWithDefault Set.empty retired (pendingByParticipant pending)
    collected = Map.findWithDefault Set.empty retired (pendingByCollector pending)
    withoutParticipant = setStatePendingWorkflows pending {pendingByParticipant = Map.delete retired (pendingByParticipant pending)} state
    retireOne (current, events) ident = case Map.lookup ident (pendingByDecision (statePendingWorkflows current)) of
      Nothing -> error "retiring participant index references absent label workflow"
      Just (OraclePendingWorkflow workflow survivors collector)
        | Set.null remaining ->
            let (_, digest) = workflowTerminalEvidence workflow
             in (completeWorkflow workflow current, LabelWorkflowCompleted ident digest : events)
        | otherwise ->
            let nextCollector = if Set.member ident collected then Set.findMin remaining else collector
                currentPending = statePendingWorkflows current
                collectors = if nextCollector == collector then pendingByCollector currentPending else insertPendingIndex nextCollector ident (deletePendingIndex collector ident (pendingByCollector currentPending))
                updated =
                  currentPending
                    { pendingByDecision = Map.insert ident (OraclePendingWorkflow workflow remaining nextCollector) (pendingByDecision currentPending),
                      pendingByCollector = collectors
                    }
             in (setStatePendingWorkflows updated current, events)
        where
          remaining = Set.delete retired survivors

validateLiveEnvelope ::
  OracleEnvelope ->
  OracleState ->
  Either OracleRejection ()
validateLiveEnvelope envelope state = do
  let requestHome = oracleClientRequestHome (oracleEnvelopeRequestId envelope)
      claimedHome = oracleEnvelopeHomeHeraldEpoch envelope
  if requestHome == claimedHome
    then Right ()
    else Left (RequestHomeEpochMismatch requestHome claimedHome)
  if Set.member claimedHome (stateActiveHeralds state)
    then Right ()
    else Left (InactiveHomeHerald claimedHome)
  case oracleEnvelopeExpectedControlIndex envelope of
    Nothing -> Right ()
    Just expected
      | expected == oracleGreatestControlIndex state -> Right ()
      | otherwise ->
          Left
            ( StaleExpectedControlIndex
                expected
                (oracleGreatestControlIndex state)
            )

applyLiveStart ::
  ControlIndex ->
  OracleClientRequestId ->
  ProcessStart ->
  HeraldEpoch ->
  OracleState ->
  Either OracleRejection (OracleState, [OracleProjectionEvent])
applyLiveStart nextIndex request supplied home state = do
  let process = processStartProcessEpochId supplied
      residence = processStartResidence supplied
      processId = processStartProcessId supplied
  if residence == home
    then Right ()
    else Left (StartProcessResidenceMismatch process home residence)
  if Map.member process (stateProcesses state)
    then Left (ProcessEpochAlreadyStarted process)
    else Right ()
  if Set.member processId (stateKnownProcessIds state)
    then Left (ProcessAlreadyStarted processId)
    else Right ()
  let record =
        ProcessEpochRecord
          processId
          process
          residence
          (DynamicStart nextIndex request)
          ProcessRecordLive
      successor = setStateProcesses (Map.insert process record (stateProcesses state)) state
  Right (successor, [ProcessStarted record])

applyLiveEnd ::
  ControlIndex ->
  ProcessEpochId ->
  ProcessEndReason ->
  HeraldEpoch ->
  OracleState ->
  Either OracleRejection (OracleState, [OracleProjectionEvent])
applyLiveEnd nextIndex process reason home state = do
  record <- validateLiveEnd process home state
  let ended = setProcessLifecycle (ProcessRecordEnded nextIndex reason) record
      successor = setStateProcesses (Map.insert process ended (stateProcesses state)) state
  Right (successor, [ProcessEpochEnded process nextIndex reason])

validateLiveEnd ::
  ProcessEpochId ->
  HeraldEpoch ->
  OracleState ->
  Either OracleRejection ProcessEpochRecord
validateLiveEnd process home state =
  case Map.lookup process (stateProcesses state) of
    Nothing -> Left (EndProcessUnknown process)
    Just record
      | processRecordResidence record /= home ->
          Left
            ( EndProcessResidenceMismatch
                process
                home
                (processRecordResidence record)
            )
      | otherwise -> case processRecordLifecycleInternal record of
          ProcessRecordLive -> Right record
          ProcessRecordEnded _ _ ->
            Left (EndProcessAlreadyEnded process)

applyLiveDecide ::
  ControlIndex ->
  OracleClientRequestId ->
  HeraldEpoch ->
  LabelDecisionId ->
  ProcessEpochId ->
  GlobalObjectId ->
  Label ->
  Maybe InitialLabelEvidence ->
  Maybe LiveAuthority ->
  Maybe PriorAuthorityJustification ->
  LabelTarget ->
  HomeLabelAcceptanceCut ->
  OracleState ->
  Either OracleRejection (OracleState, [OracleProjectionEvent])
applyLiveDecide index request home supplied caller object expected initialEvidence initialAuthority initialJustification target acceptanceCut state = do
  let derived = deriveLabelDecisionId (checkedOracleSystemId (stateGenesis state)) request
  unless (supplied == derived) (Left (OpenDecisionIdMismatch derived supplied))
  let cutCaller = labelProcessAcceptancePositionProcess (homeLabelAcceptanceCutProcessPosition acceptanceCut)
  unless (cutCaller == caller) (Left (OpenAcceptanceCallerMismatch caller cutCaller))
  (_, revision, authority, hasRecord) <- liveCurrentPrior object initialEvidence initialAuthority state
  let justification = case (hasRecord, revision, authority) of
        (True, Just priorRevision, Just _) -> Just (existingReleasedAuthority priorRevision)
        (True, _, _) -> Nothing
        _ -> initialJustification
  unless
    (validLiveAuthorityJustification (stateGenesis state) hasRecord revision authority justification)
    (Left OpenPriorAuthorityJustificationInvalid)
  let captured = Set.toAscList (stateActiveHeralds state)
      members = memberDigestFor captured
      generation = heraldMembershipGenerationId (oracleCurrentMembership state)
      decision = LiveLabelDecision supplied request home caller object expected initialEvidence revision authority justification target index acceptanceCut members generation
      decisionState = setLatestLabelDecision index state
      failDecision reason =
        let terminal = notAppliedTerminal index decision reason (stateGenesis state)
            digest = deriveLabelOutcomeDigest terminal
         in ( insertCompletedWorkflow supplied (completedWorkflow decision digest) decisionState,
              [LabelDecided decision terminal digest]
            )
  case prepareAtomicDecision decision state of
    Left reason -> pure (failDecision reason)
    Right prepared -> do
      let facts = atomicDecisionFacts index decision prepared state
          factsDigest = deriveLivePreparedLabelDigest facts
          overlay = either (error . show) id (releaseLiveLabel index prepared)
          record = either (error . show) id (mkLabelRecord object (liveRecordStoredState overlay) (liveRecordRevision overlay) (liveRecordRetainedAuthority overlay))
          terminal = LiveReleasedTerminal facts factsDigest index overlay
          digest = deriveLabelOutcomeDigest terminal
          workflow = OracleLabelWorkflow decision (OracleReleased terminal digest record)
          withLabel = insertPendingWorkflow workflow (setStateLabels (Map.insert object record (stateLabels state)) decisionState)
          (disappearance, invalidations) = D.invalidateDisappearanceForLabel index object supplied (stateDisappearance state)
      pure (setStateDisappearance disappearance withLabel, LabelDecided decision terminal digest : fmap projectDisappearanceEvent invalidations)

prepareAtomicDecision :: LiveLabelDecision -> OracleState -> Either LiveNotAppliedReason LivePreparedLabel
prepareAtomicDecision decision state
  | D.disappearanceControlledSubjectDeleted (liveDecisionObject decision) (stateDisappearance state) = Left LiveTransitionNoLongerPermitted
  | otherwise =
      let (stored, _, _, authority) = currentDecisionPrior decision state
       in case prepareLiveLabel (stateProcessTable state) (liveDecisionCaller decision) (liveDecisionExpectedLabel decision) (LivePriorLabel stored authority) (liveDecisionTarget decision) of
            Right prepared -> Right prepared
            Left (LiveCallerProcessNotLive process) -> Left (LiveCallerProcessEnded process)
            Left (LiveTargetProcessNotLive process) -> Left (LiveTargetProcessEnded process)
            Left LiveExpectedLabelMismatch {} -> Left LivePriorLabelChanged
            Left _ -> Left LiveTransitionNoLongerPermitted

atomicDecisionFacts :: ControlIndex -> LiveLabelDecision -> LivePreparedLabel -> OracleState -> LivePreparedLabelFacts
atomicDecisionFacts index decision prepared state =
  either (error . ("atomic label facts invariant: " <>) . show) id
    $ mkPreparedLabelFacts
      (liveDecisionId decision)
      index
      (liveDecisionObject decision)
      (preparedOutcome prepared)
      (releasedLabel (liveDecisionExpectedLabel decision))
      (liveDecisionExpectedPriorRevision decision)
      (checkedOracleCatalogueDigest (stateGenesis state))
      (liveDecisionExpectedPriorAuthority decision)
      (liveDecisionPriorAuthorityJustification decision)
      (preparedDisposition prepared)
      (liveDecisionMemberSetDigest decision)

liveCurrentPrior ::
  GlobalObjectId ->
  Maybe InitialLabelEvidence ->
  Maybe LiveAuthority ->
  OracleState ->
  Either
    OracleRejection
    (ReleasedLabelState, Maybe LabelRevision, Maybe LiveAuthority, Bool)
liveCurrentPrior object initialEvidence claimedAuthority state = do
  case initialEvidence of
    Just evidence
      | initialLabelEvidenceObject evidence /= object ->
          Left (OpenInitialLabelEvidenceObjectMismatch object (initialLabelEvidenceObject evidence))
    _ -> Right ()
  case Map.lookup object (stateLabels state) of
    Just record ->
      Right
        ( liveEffectiveReleasedState
            (stateProcessTable state)
            (labelRecordReleasedState record),
          Just (labelRecordRevision record),
          labelRecordRetainedAuthority record,
          True
        )
    Nothing -> do
      evidence <- maybe (Left (OpenInitialLabelEvidenceMissing object)) Right initialEvidence
      effective <- liveEffectiveInitialEvidence evidence state
      Right (effective, Nothing, claimedAuthority, False)

-- The evidence constructor already establishes generation zero. Zombie is an
-- observation of canonical End, not a way to assert that lifecycle fact.
liveEffectiveInitialEvidence ::
  InitialLabelEvidence -> OracleState -> Either OracleRejection ReleasedLabelState
liveEffectiveInitialEvidence evidence state = case initialLabelEvidenceLabel evidence of
  label@(ZombieLabel process, generation) ->
    case Map.lookup process (stateProcesses state) of
      Just record
        | ProcessRecordEnded {} <- processRecordLifecycleInternal record ->
            Right (releasedLabel label)
      _ ->
        Left
          ( OpenExpectedPriorLabelMismatch
              (releasedLabel label)
              (releasedLabel (ProcessLabel process, generation))
          )
  label ->
    Right
      ( liveEffectiveReleasedState
          (stateProcessTable state)
          (releasedLabel label)
      )

-- Private state plumbing -----------------------------------------------------

stateGenesis :: OracleState -> CheckedOracleGenesis
stateGenesis (OracleState genesis _ _ _ _ _ _ _ _ _) = genesis

stateRequests ::
  OracleState -> Map OracleClientRequestId OracleRequestRecord
stateRequests (OracleState _ _ requests _ _ _ _ _ _ _) = requests

stateProcesses ::
  OracleState -> Map ProcessEpochId ProcessEpochRecord
stateProcesses (OracleState _ _ _ processes _ _ _ _ _ _) = processes

stateLabels :: OracleState -> Map GlobalObjectId LabelRecord
stateLabels (OracleState _ _ _ _ labels _ _ _ _ _) = labels

stateCompleted ::
  OracleState -> Map CompletedDecisionKey CompletedWorkflow
stateCompleted (OracleState _ _ _ _ _ _ completed _ _ _) = completed

stateFailure :: OracleState -> FailureState
stateFailure (OracleState _ _ _ _ _ _ _ failure _ _) = failure

setStateRequestAndIndex ::
  ControlIndex ->
  OracleClientRequestId ->
  OracleRequestRecord ->
  OracleState ->
  OracleState
setStateRequestAndIndex
  index
  request
  record
  (OracleState genesis _ requests processes labels workflow completed failure disappearance digest) =
    OracleState
      genesis
      index
      (Map.insert request record requests)
      processes
      labels
      workflow
      completed
      failure
      disappearance
      digest

setStateProcesses ::
  Map ProcessEpochId ProcessEpochRecord ->
  OracleState ->
  OracleState
setStateProcesses
  processes
  (OracleState genesis index requests _ labels workflow completed failure disappearance digest) =
    OracleState
      genesis
      index
      requests
      processes
      labels
      workflow
      completed
      failure
      disappearance
      digest

setStateLabels ::
  Map GlobalObjectId LabelRecord ->
  OracleState ->
  OracleState
setStateLabels
  labels
  (OracleState genesis index requests processes _ workflow completed failure disappearance digest) =
    OracleState
      genesis
      index
      requests
      processes
      labels
      workflow
      completed
      failure
      disappearance
      digest

setStatePendingWorkflows ::
  OraclePendingWorkflows ->
  OracleState ->
  OracleState
setStatePendingWorkflows
  workflow
  (OracleState genesis index requests processes labels _ completed failure disappearance digest) =
    OracleState
      genesis
      index
      requests
      processes
      labels
      workflow
      completed
      failure
      disappearance
      digest

setStateCompleted ::
  Map CompletedDecisionKey CompletedWorkflow ->
  OracleState ->
  OracleState
setStateCompleted
  completed
  (OracleState genesis index requests processes labels workflow _ failure disappearance digest) =
    OracleState
      genesis
      index
      requests
      processes
      labels
      workflow
      completed
      failure
      disappearance
      digest

setStateFailure :: FailureState -> OracleState -> OracleState
setStateFailure
  failure
  (OracleState genesis index requests processes labels workflow completed _ disappearance digest) =
    OracleState
      genesis
      index
      requests
      processes
      labels
      workflow
      completed
      failure
      disappearance
      digest

setStateDigest ::
  Maybe OracleStateDigest -> OracleState -> OracleState
setStateDigest
  digest
  (OracleState genesis index requests processes labels workflow completed failure disappearance _) =
    OracleState
      genesis
      index
      requests
      processes
      labels
      workflow
      completed
      failure
      disappearance
      digest

oracleEnvelopeExpectedControlIndex :: OracleEnvelope -> Maybe ControlIndex
oracleEnvelopeExpectedControlIndex (OracleEnvelope _ expected _ _ _) = expected

oracleEnvelopeHomeHeraldEpoch :: OracleEnvelope -> HeraldEpoch
oracleEnvelopeHomeHeraldEpoch (OracleEnvelope _ _ home _ _) = home

oracleEnvelopeCommand :: OracleEnvelope -> OracleCommand
oracleEnvelopeCommand (OracleEnvelope _ _ _ command _) = command

processRecordLifecycleInternal ::
  ProcessEpochRecord -> ProcessLifecycle
processRecordLifecycleInternal (ProcessEpochRecord _ _ _ _ lifecycle) = lifecycle

setProcessLifecycle ::
  ProcessLifecycle ->
  ProcessEpochRecord ->
  ProcessEpochRecord
setProcessLifecycle
  lifecycle
  (ProcessEpochRecord processId process residence origin _) =
    ProcessEpochRecord processId process residence origin lifecycle

stateActiveHeralds :: OracleState -> Set.Set HeraldEpoch
stateActiveHeralds = failureActiveHeralds . stateFailure

stateKnownProcessIds :: OracleState -> Set.Set ProcessId
stateKnownProcessIds = Set.fromList . fmap processRecordProcessId . Map.elems . stateProcesses

stateProcessTable :: OracleState -> LiveProcessTable
stateProcessTable state =
  LiveProcessTable
    ( Map.map
        ( \record -> case processRecordLifecycleInternal record of
            ProcessRecordLive -> LiveProcessLive
            ProcessRecordEnded index reason ->
              LiveProcessEnded index reason
        )
        (stateProcesses state)
    )

memberDigestFor :: [HeraldEpoch] -> MemberSetDigest
memberDigestFor (member : members) = deriveMemberSetDigest (member :| members)
memberDigestFor [] = error "checked Oracle genesis has an empty Herald membership"

validLiveAuthorityJustification ::
  CheckedOracleGenesis ->
  Bool ->
  Maybe LabelRevision ->
  Maybe LiveAuthority ->
  Maybe PriorAuthorityJustification ->
  Bool
validLiveAuthorityJustification genesis hasRecord revision authority justification =
  case (authority, justification) of
    (Nothing, Nothing) -> True
    (Nothing, Just _) -> False
    (Just _, Nothing) -> False
    (Just expected, Just supplied) ->
      case priorAuthorityJustificationView supplied of
        ExistingReleasedAuthorityView justifiedRevision ->
          hasRecord && revision == Just justifiedRevision
        CheckedGenesisAuthorityView digest ->
          not hasRecord
            && revision == Nothing
            && expected == genesisAuthorityEpoch
            && digest == checkedOracleInitialProjectionDigest genesis
        EstablishedStructuralAuthorityView occurrence cut ->
          not hasRecord
            && revision == Nothing
            && expected
              == structuralAuthorityEpoch occurrence cut

-- | Fast preflight and serialized admission share this predicate. Deferral is
-- not a receipt: the original identity and bytes remain retryable after the
-- pending successful decision completes. Retained requests always win.
oracleSubmissionPreflightBlocker :: OracleEnvelope -> OracleState -> Maybe LabelDecisionId
oracleSubmissionPreflightBlocker envelope state
  | Just _ <- oracleRequestRetirement (oracleEnvelopeRequestId envelope) state = Nothing
  | Left _ <- validatePiggybackRetirement envelope state = Nothing
  | Map.member (oracleEnvelopeRequestId envelope) (stateRequests state) = Nothing
  | Left _ <- validateLiveEnvelope envelope state = Nothing
  | DecideLabel _ _ object _ _ _ _ _ _ <- oracleEnvelopeCommand envelope = Map.lookup object (pendingByObject (statePendingWorkflows state))
  | otherwise = Nothing

decisionExpectedAuthority ::
  LiveLabelDecision -> Maybe LiveAuthority
decisionExpectedAuthority = liveDecisionExpectedPriorAuthority

-- Installation completion ---------------------------------------------------

applyLiveCompletion ::
  HeraldEpoch ->
  LabelCompletionAttestation ->
  OracleState ->
  Either OracleRejection (OracleState, [OracleProjectionEvent])
applyLiveCompletion home attestation state =
  case Map.lookup suppliedDecision (pendingByDecision (statePendingWorkflows state)) of
    Just pending -> applyCurrentCompletion home attestation (pendingWorkflow pending) state
    Nothing -> case Map.lookup (completedDecisionKey suppliedDecision) (stateCompleted state) of
      Just completed -> applyCompletedCompletion home attestation completed state
      Nothing
        | labelCompletionControlIndex attestation <= oracleLabelRetiredThrough state -> Left (LabelCompletionObsolete suppliedDecision (labelCompletionControlIndex attestation) (oracleLabelRetiredThrough state))
        | otherwise -> Left (UnknownLabelDecision suppliedDecision)
  where
    suppliedDecision = labelCompletionDecisionId attestation

applyCurrentCompletion ::
  HeraldEpoch ->
  LabelCompletionAttestation ->
  OracleLabelWorkflow ->
  OracleState ->
  Either OracleRejection (OracleState, [OracleProjectionEvent])
applyCurrentCompletion home attestation workflow state = do
  let decision = workflowDecision workflow
      collector = labelCompletionCollector attestation
      generation = heraldMembershipGenerationId (oracleCurrentMembership state)
  validateCompletionIndex (liveDecisionRequestControlIndex decision) attestation
  validateLiveReporter state home collector decision
  let (_, digest) = workflowTerminalEvidence workflow
  validateCompletionDigest digest attestation
  case oracleLabelCollector (liveDecisionId decision) state of
    Just expected | expected /= collector -> Left (LabelCompletionCollectorMismatch expected collector)
    Just _ -> Right ()
    Nothing -> error "terminal workflow without captured survivors was not completed at retirement"
  unless
    (labelCompletionMembershipGeneration attestation == generation)
    (Left (LabelCompletionMembershipGenerationMismatch generation (labelCompletionMembershipGeneration attestation)))
  pure (completeWorkflow workflow state, [LabelWorkflowCompleted (liveDecisionId decision) digest])

-- Completion is semantic idempotence by decision and outcome, independent of a
-- former collector's request identity or membership generation. Envelope
-- admission still requires a currently active submitting Herald.
applyCompletedCompletion ::
  HeraldEpoch ->
  LabelCompletionAttestation ->
  CompletedWorkflow ->
  OracleState ->
  Either OracleRejection (OracleState, [OracleProjectionEvent])
applyCompletedCompletion home attestation completed state = do
  let digest = completedOutcomeDigest completed
      captured = case capturedMembershipHeralds (oracleCheckedMembershipHistory state) (completedMembership completed) of
        Just members -> members
        Nothing -> error "completed label witness lost checked membership history"
  validateCompletionIndex (completedControlIndex completed) attestation
  validateReporter home (labelCompletionCollector attestation) captured
  validateCompletionDigest digest attestation
  pure (state, [LabelWorkflowCompleted (labelCompletionDecisionId attestation) digest])

validateCompletionIndex :: ControlIndex -> LabelCompletionAttestation -> Either OracleRejection ()
validateCompletionIndex expected attestation = unless (labelCompletionControlIndex attestation == expected) (Left (LabelCompletionDecisionIndexMismatch expected (labelCompletionControlIndex attestation)))

validateCompletionDigest :: LabelOutcomeDigest -> LabelCompletionAttestation -> Either OracleRejection ()
validateCompletionDigest expected attestation =
  unless
    (labelCompletionOutcomeDigest attestation == expected)
    (Left (LabelCompletionDigestMismatch expected (labelCompletionOutcomeDigest attestation)))

completeWorkflow :: OracleLabelWorkflow -> OracleState -> OracleState
completeWorkflow workflow state =
  insertCompletedWorkflow
    (liveDecisionId (workflowDecision workflow))
    (completedFromWorkflow workflow)
    (deletePendingWorkflow workflow state)

validateLiveReporter ::
  OracleState ->
  HeraldEpoch ->
  HeraldEpoch ->
  LiveLabelDecision ->
  Either OracleRejection ()
validateLiveReporter state home reporter decision = validateReporter home reporter (capturedDecisionHeralds state decision)

validateReporter :: HeraldEpoch -> HeraldEpoch -> [HeraldEpoch] -> Either OracleRejection ()
validateReporter home reporter captured = do
  if home == reporter
    then Right ()
    else Left (ReporterHomeMismatch reporter home)
  if reporter `elem` captured
    then Right ()
    else Left (ReporterNotCaptured reporter)

workflowTerminalEvidence :: OracleLabelWorkflow -> (LiveTerminalOutcome, LabelOutcomeDigest)
workflowTerminalEvidence (OracleLabelWorkflow _ (OracleReleased terminal digest _)) = (terminal, digest)

completedFromWorkflow :: OracleLabelWorkflow -> CompletedWorkflow
completedFromWorkflow (OracleLabelWorkflow decision (OracleReleased _ digest _)) = completedWorkflow decision digest

completedMembership :: CompletedWorkflow -> HeraldMembershipGenerationId
completedMembership (CompletedWorkflow _ generation _) = generation

completedOutcomeDigest :: CompletedWorkflow -> LabelOutcomeDigest
completedOutcomeDigest (CompletedWorkflow _ _ (CompletedOutcomeDigest digest)) = checkedOutcomeDigest (ShortByteString.fromShort digest)

capturedMembershipHeralds :: HeraldMembershipHistory -> HeraldMembershipGenerationId -> Maybe [HeraldEpoch]
capturedMembershipHeralds history ident = NonEmpty.toList . heraldMembershipGenerationActiveHeraldEpochs <$> Membership.lookupHeraldMembershipGeneration ident history

notAppliedTerminal ::
  ControlIndex ->
  LiveLabelDecision ->
  LiveNotAppliedReason ->
  CheckedOracleGenesis ->
  LiveTerminalOutcome
notAppliedTerminal index decision reason genesis =
  LiveNotAppliedTerminal
    (liveDecisionId decision)
    index
    (liveDecisionObject decision)
    reason
    (checkedOracleCatalogueDigest genesis)
    (liveDecisionMemberSetDigest decision)

currentDecisionPrior ::
  LiveLabelDecision ->
  OracleState ->
  ( ReleasedLabelState,
    ReleasedLabelState,
    Maybe LabelRevision,
    Maybe LiveAuthority
  )
currentDecisionPrior decision state =
  case Map.lookup (liveDecisionObject decision) (stateLabels state) of
    Just record ->
      ( labelRecordReleasedState record,
        liveEffectiveReleasedState
          (stateProcessTable state)
          (labelRecordReleasedState record),
        Just (labelRecordRevision record),
        labelRecordRetainedAuthority record
      )
    Nothing ->
      ( releasedLabel initialLabel,
        liveEffectiveReleasedState
          (stateProcessTable state)
          (releasedLabel initialLabel),
        Nothing,
        decisionExpectedAuthority decision
      )
      where
        initialLabel = case liveDecisionInitialLabelEvidence decision of
          Just evidence -> initialLabelEvidenceLabel evidence
          Nothing -> error "admitted first-label decision lost its initial evidence"

-- Canonical private state ----------------------------------------------------

refreshLiveOracleStateDigest ::
  OracleState -> OracleState
refreshLiveOracleStateDigest state = case oracleStateDigestMode state of
  OracleStateDigestDisabled -> state
  OracleStateDigestEnabled ->
    refreshCanonicalStateDigest liveStateTranscriptBytes (setStateDigest . Just) state

-- | Exact canonical-state digest preimage.  This observes the private
-- live owner only; it does not make the temporary state decodable.
oracleStateCanonicalBytes :: OracleState -> ByteString
oracleStateCanonicalBytes = liveStateTranscriptBytes

-- | Explicit diagnostic inventory. The owner calls this only when a measurement
-- is requested; it is not maintained or traversed by ordinary transitions.
-- Cached byte counts describe retained encoding buffers, not total live heap.
oracleStateDiagnosticCardinalities :: OracleState -> [(String, Int)]
oracleStateDiagnosticCardinalities state =
  [ ("requests", Map.size (stateRequests state)),
    ("processes", Map.size (stateProcesses state)),
    ("labels", Map.size (stateLabels state)),
    ("open_workflows", Map.size (pendingByDecision (statePendingWorkflows state))),
    ("completed_workflows", Map.size (stateCompleted state)),
    ("completed_workflow_cached_bytes", 0),
    ("membership_generations", length (oracleMembershipHistory state)),
    ("disappearance_probes", length (D.disappearanceProbes (stateDisappearance state))),
    ("herald_admissions", length (A.admissionRecords (stateAdmission state))),
    ("voter_configurations", length (V.voterConfigurationHistory (stateVoter state))),
    ("replica_registrations", length (V.voterRegistrations (stateVoter state))),
    ("receipt_retirement_homes", Map.size (stateReceiptRetirementFrontiers state))
  ]

-- | Exact byte contributions to the canonical digest preimage, including list
-- counts and optional-section framing. No digest is computed. Keep the ordinary
-- encoder below unchanged: this extra serialization is opt-in diagnostic work.
oracleStateDiagnosticSections :: OracleState -> [(String, Int)]
oracleStateDiagnosticSections state =
  [ section "domain" (putFramedBytes liveStateDomain),
    section "genesis" (putLiveGenesis (stateGenesis state)),
    section "control_index" (SerializePut.putWord64be (controlIndexWord64 (oracleGreatestControlIndex state))),
    section "requests" (putCountedList putRequestRecord (Map.toAscList (stateRequests state))),
    section "processes" (putCountedList putProcessRecord (Map.toAscList (stateProcesses state))),
    section "labels" (putCountedList putLabelRecordEntry (Map.toAscList (stateLabels state))),
    section "open_workflows" (putCountedList (putLiveWorkflow . snd) (stateWorkflowEntries state)),
    section "completed_workflows" (putCountedList putCompletedEntry (Map.toAscList (stateCompleted state))),
    section "failure" $ unless (failureStateIsGenesisIdle (stateFailure state)) (putFramedBytes (failureStateCanonicalBytes (stateFailure state))),
    section "disappearance" (putFramedBytes (D.disappearanceStateCanonicalBytes (stateDisappearance state))),
    section "admission" $ unless (null (A.admissionRecords (stateAdmission state))) (putFramedBytes (A.admissionStateCanonicalBytes (stateAdmission state))),
    section "voter" $ unless (V.voterStateIsGenesisIdle (stateVoter state)) (putFramedBytes (V.voterStateCanonicalBytes (stateVoter state))),
    section "oracle_progress" (putOracleLifetime state)
  ]
  where
    section name = (name,) . ByteString.length . SerializePut.runPut

putOracleLifetime :: OracleState -> SerializePut.Put
putOracleLifetime state = do
  SerializePut.putWord64be (controlIndexWord64 (oracleLatestLabelDecision state))
  SerializePut.putWord64be (controlIndexWord64 (oracleLabelRetiredThrough state))
  putCountedList (\(home, progress) -> SerializePut.putByteString (heraldEpochBytes home) >> putOracleProgress progress) (oracleProgresses state)

liveStateTranscriptBytes :: OracleState -> ByteString
liveStateTranscriptBytes state = SerializePut.runPut $ do
  putFramedBytes liveStateDomain
  putLiveGenesis (stateGenesis state)
  SerializePut.putWord64be
    (controlIndexWord64 (oracleGreatestControlIndex state))
  putCountedList putRequestRecord (Map.toAscList (stateRequests state))
  putCountedList putProcessRecord (Map.toAscList (stateProcesses state))
  putCountedList putLabelRecordEntry (Map.toAscList (stateLabels state))
  putCountedList (putLiveWorkflow . snd) (stateWorkflowEntries state)
  putCountedList putCompletedEntry (Map.toAscList (stateCompleted state))
  if failureStateIsGenesisIdle (stateFailure state)
    then pure ()
    else putFramedBytes (failureStateCanonicalBytes (stateFailure state))
  putFramedBytes (D.disappearanceStateCanonicalBytes (stateDisappearance state))
  if null (A.admissionRecords (stateAdmission state)) then pure () else putFramedBytes (A.admissionStateCanonicalBytes (stateAdmission state))
  if V.voterStateIsGenesisIdle (stateVoter state) then pure () else putFramedBytes (V.voterStateCanonicalBytes (stateVoter state))
  putOracleLifetime state

putLiveGenesis :: CheckedOracleGenesis -> SerializePut.Put
putLiveGenesis genesis = do
  SerializePut.putByteString (systemIdBytes (checkedOracleSystemId genesis))
  SerializePut.putByteString
    (initialProjectionDigestBytes (checkedOracleInitialProjectionDigest genesis))
  SerializePut.putByteString
    (catalogueDigestBytes (checkedOracleCatalogueDigest genesis))
  putCountedList putMember (checkedOracleActiveHeralds genesis)
  SerializePut.putByteString
    (configurationDigestBytes (checkedOracleConfigurationDigest genesis))
  SerializePut.putByteString
    (raftConfigurationDigestBytes (checkedOracleRaftConfigurationDigest genesis))
  where
    putMember member = do
      SerializePut.putByteString (heraldIdBytes (heraldMemberId member))
      SerializePut.putByteString (heraldEpochBytes (heraldMemberEpoch member))

putRequestRecord ::
  (OracleClientRequestId, OracleRequestRecord) -> SerializePut.Put
putRequestRecord
  (requestId, OracleRequestRecord commandDigest _ receiptBytes) = do
    putRequestId requestId
    SerializePut.putByteString (oracleCommandDigestBytes commandDigest)
    putFramedBytes receiptBytes

putLiveReceipt :: OracleReceipt -> SerializePut.Put
putLiveReceipt (OracleReceipt requestId digest index result failureResult) = do
  putRequestId requestId
  SerializePut.putByteString (oracleCommandDigestBytes digest)
  SerializePut.putWord64be (controlIndexWord64 index)
  case result of
    OracleAccepted -> case failureResult of
      Nothing -> SerializePut.putWord8 0
      Just (AcceptedFailure accepted) -> do
        SerializePut.putWord8 2
        putFailureCommandResult accepted
      Just (AcceptedDisappearance accepted) -> do
        SerializePut.putWord8 3
        putFramedBytes (DC.encodeAccepted accepted)
      Just (AcceptedVoter accepted) -> do
        SerializePut.putWord8 4
        putFramedBytes (V.encodeVoterResult accepted)
    OracleRejected rejection -> do
      SerializePut.putWord8 1
      putLiveRejection rejection

putFailureCommandResult :: FailureCommandResult -> SerializePut.Put
putFailureCommandResult result = case result of
  VoterHostFailureAcceptedResult certificate -> SerializePut.putWord8 4 >> putFramedBytes (F.encodeAcceptedVoterHostFailure certificate)
  FailureProbeOpened probe -> do
    SerializePut.putWord8 0
    putFramedBytes (heraldFailureProbeIdCanonicalBytes probe)
  FailureProbeReportRecorded probe ready -> do
    SerializePut.putWord8 1
    putFramedBytes (heraldFailureProbeIdCanonicalBytes probe)
    case ready of
      Nothing -> SerializePut.putWord8 0
      Just (FailureResolutionReady resolution disposition target) -> do
        SerializePut.putWord8 1
        putFramedBytes (failureProbeResolutionIdCanonicalBytes resolution)
        SerializePut.putWord8 $ case disposition of
          DismissFailureProbe -> 0
          RetireFailureProbeTarget -> 1
        SerializePut.putByteString (heraldEpochBytes target)
  FailureProbeDismissedResult resolution -> do
    SerializePut.putWord8 2
    putFramedBytes (failureProbeResolutionIdCanonicalBytes resolution)
  HeraldRetiredResult resolution generation -> do
    SerializePut.putWord8 3
    putFramedBytes (failureProbeResolutionIdCanonicalBytes resolution)
    SerializePut.putByteString
      (heraldMembershipGenerationIdBytes generation)

putProcessRecord ::
  (ProcessEpochId, ProcessEpochRecord) -> SerializePut.Put
putProcessRecord (processKey, record) = do
  SerializePut.putByteString (processEpochIdBytes processKey)
  putLiveProcessEpochRecord record

putLiveProcessEpochRecord ::
  ProcessEpochRecord -> SerializePut.Put
putLiveProcessEpochRecord record = do
  SerializePut.putByteString
    (processIdBytes (processRecordProcessId record))
  SerializePut.putByteString
    (processEpochIdBytes (processRecordProcessEpoch record))
  SerializePut.putByteString
    (heraldEpochBytes (processRecordResidence record))
  case record of
    ProcessEpochRecord _ _ _ origin lifecycle -> do
      case origin of
        GenesisProcess -> SerializePut.putWord8 0
        DynamicStart index manifest -> do
          SerializePut.putWord8 1
          SerializePut.putWord64be (controlIndexWord64 index)
          putRequestId manifest
      case lifecycle of
        ProcessRecordLive -> SerializePut.putWord8 0
        ProcessRecordEnded index reason -> do
          SerializePut.putWord8 1
          SerializePut.putWord64be (controlIndexWord64 index)
          putFramedBytes (processEndReasonCanonicalBytes reason)

putLabelRecordEntry ::
  (GlobalObjectId, LabelRecord) -> SerializePut.Put
putLabelRecordEntry (object, record) = do
  SerializePut.putByteString (globalObjectIdBytes object)
  putDomainLabelRecord record

putReleasedLabelOverlay :: ReleasedLabelOverlay -> SerializePut.Put
putReleasedLabelOverlay overlay = do
  putReleasedState (liveRecordStoredState overlay)
  SerializePut.putWord64be
    (controlIndexWord64 (labelRevisionControlIndex (liveRecordRevision overlay)))
  putMaybeLiveAuthority (liveRecordRetainedAuthority overlay)

putDomainLabelRecord :: LabelRecord -> SerializePut.Put
putDomainLabelRecord record = do
  putReleasedState (labelRecordReleasedState record)
  SerializePut.putWord64be
    (controlIndexWord64 (labelRevisionControlIndex (labelRecordRevision record)))
  putMaybeLiveAuthority (labelRecordRetainedAuthority record)

putLiveWorkflow :: OracleLabelWorkflow -> SerializePut.Put
putLiveWorkflow (OracleLabelWorkflow decision (OracleReleased terminal digest record)) = do
  putLiveDecision decision
  putLiveTerminal terminal
  SerializePut.putByteString (labelOutcomeDigestBytes digest)
  putDomainLabelRecord record

putLiveDecision :: LiveLabelDecision -> SerializePut.Put
putLiveDecision
  ( LiveLabelDecision
      decision
      requestId
      home
      caller
      object
      expected
      initialEvidence
      revision
      authority
      justification
      target
      index
      acceptanceCut
      members
      generation
    ) = do
    SerializePut.putByteString (labelDecisionIdBytes decision)
    putRequestId requestId
    SerializePut.putByteString (heraldEpochBytes home)
    SerializePut.putByteString (processEpochIdBytes caller)
    SerializePut.putByteString (globalObjectIdBytes object)
    putLabel expected
    putMaybeInitialLabelEvidence initialEvidence
    putMaybeLabelRevision revision
    putMaybeLiveAuthority authority
    putMaybeJustification justification
    putLabelTarget target
    SerializePut.putWord64be (controlIndexWord64 index)
    putHomeAcceptanceCut acceptanceCut
    SerializePut.putByteString (memberSetDigestBytes members)
    SerializePut.putByteString (heraldMembershipGenerationIdBytes generation)

putLivePreparedFacts ::
  LivePreparedLabelFacts -> SerializePut.Put
putLivePreparedFacts =
  putFramedBytes . livePreparedLabelFactsCanonicalBytes

putLiveTerminal :: LiveTerminalOutcome -> SerializePut.Put
putLiveTerminal =
  putFramedBytes . liveTerminalOutcomeCanonicalBytes

putCompletedEntry ::
  (CompletedDecisionKey, CompletedWorkflow) -> SerializePut.Put
putCompletedEntry (CompletedDecisionKey decisionKey, CompletedWorkflow index generation (CompletedOutcomeDigest digest)) = do
  SerializePut.putShortByteString decisionKey
  SerializePut.putWord64be (controlIndexWord64 index)
  SerializePut.putByteString (heraldMembershipGenerationIdBytes generation)
  SerializePut.putShortByteString digest

putLiveRejection :: OracleRejection -> SerializePut.Put
putLiveRejection rejection = case rejection of
  VoterCommandRejected problem -> do
    putTag
    putFramedBytes (V.encodeVoterProblem problem)
  RequestHomeEpochMismatch expected supplied -> do
    putTag
    putHeraldPair expected supplied
  InactiveHomeHerald home -> do
    putTag
    putHerald home
  StaleExpectedControlIndex expected observed -> do
    putTag
    putIndexPair expected observed
  StartProcessResidenceMismatch process supplied expected -> do
    putTag
    putProcess process
    putHeraldPair supplied expected
  ProcessEpochAlreadyStarted process -> do
    putTag
    putProcess process
  ProcessAlreadyStarted processId -> do
    putTag
    SerializePut.putByteString (processIdBytes processId)
  EndProcessUnknown process -> do
    putTag
    putProcess process
  EndProcessResidenceMismatch process supplied expected -> do
    putTag
    putProcess process
    putHeraldPair supplied expected
  EndProcessAlreadyEnded process -> do
    putTag
    putProcess process
  OpenDecisionIdMismatch expected supplied -> do
    putTag
    putDecisionPair expected supplied
  OpenAcceptanceCallerMismatch expected supplied -> do
    putTag
    putProcess expected
    putProcess supplied
  OpenExpectedPriorLabelMismatch expected supplied -> do
    putTag
    putReleasedState expected
    putReleasedState supplied
  OpenInitialLabelEvidenceMissing object -> do
    putTag
    SerializePut.putByteString (globalObjectIdBytes object)
  OpenInitialLabelEvidenceObjectMismatch expected supplied -> do
    putTag
    SerializePut.putByteString (globalObjectIdBytes expected)
    SerializePut.putByteString (globalObjectIdBytes supplied)
  OpenPriorAuthorityJustificationInvalid -> putTag
  UnknownLabelDecision decision -> do
    putTag
    putDecisionId decision
  ReporterHomeMismatch reporter home -> do
    putTag
    putHeraldPair reporter home
  ReporterNotCaptured reporter -> do
    putTag
    putHerald reporter
  LabelCompletionDecisionMismatch expected supplied -> do
    putTag
    putDecisionPair expected supplied
  LabelCompletionCollectorMismatch expected supplied -> do
    putTag
    putHeraldPair expected supplied
  LabelCompletionDigestMismatch expected supplied -> do
    putTag
    SerializePut.putByteString (labelOutcomeDigestBytes expected)
    SerializePut.putByteString (labelOutcomeDigestBytes supplied)
  LabelCompletionMembershipGenerationMismatch expected supplied -> do
    putTag
    SerializePut.putByteString (heraldMembershipGenerationIdBytes expected)
    SerializePut.putByteString (heraldMembershipGenerationIdBytes supplied)
  LabelCompletionDecisionIndexMismatch expected supplied -> do
    putTag
    putIndexPair expected supplied
  LabelCompletionObsolete decision supplied retired -> do
    putTag
    putDecisionId decision
    putIndexPair supplied retired
  FailureCommandRejected failure -> do
    putTag
    putFailureRejection failure
  DisappearanceCommandRejected problem -> do
    SerializePut.putWord8 41
    putFramedBytes (DC.encodeRejection problem)
  HeraldAdmissionCommandRejected problem -> do
    SerializePut.putWord8 42
    putFramedBytes (A.encodeAdmissionProblem problem)
  where
    putTag = SerializePut.putWord8 (oracleRejectionTag rejection)
    putHerald = SerializePut.putByteString . heraldEpochBytes
    putHeraldPair first second = putHerald first >> putHerald second
    putProcess = SerializePut.putByteString . processEpochIdBytes
    putDecisionId = SerializePut.putByteString . labelDecisionIdBytes
    putDecisionPair first second = putDecisionId first >> putDecisionId second
    putIndex = SerializePut.putWord64be . controlIndexWord64
    putIndexPair first second = putIndex first >> putIndex second

putFailureRejection :: FailureRejection -> SerializePut.Put
putFailureRejection rejection = case rejection of
  StaleMembershipGeneration supplied expected -> do
    putTag 0
    putGeneration supplied
    putGeneration expected
  FailureProbeTargetNotActive target -> putTag 1 >> putHerald target
  FailureProbeTargetHostsVoter target -> putTag 2 >> putHerald target
  UnknownFailureProbe probe -> putTag 3 >> putProbe probe
  FailureProbeTerminal probe terminal -> do
    putTag 4
    putProbe probe
    putTerminal terminal
  IneligibleFailureReporter reporter -> putTag 5 >> putHerald reporter
  ConflictingFailureReport probe reporter retained supplied -> do
    putTag 6
    putProbe probe
    putHerald reporter
    putResult retained
    putResult supplied
  FailureProbeThresholdNotReached probe disposition -> do
    putTag 7
    putProbe probe
    putDisposition disposition
  StaleFailureVoterConfiguration supplied expected -> putTag 9 >> SerializePut.putByteString (V.voterConfigurationIdBytes supplied) >> SerializePut.putByteString (V.voterConfigurationIdBytes expected)
  FailureProbeTargetNotVoter target -> putTag 10 >> putHerald target
  FailureVoterExclusionNotApplied target -> putTag 11 >> putHerald target
  RetirementTargetMismatch supplied expected -> do
    putTag 8
    putHerald supplied
    putHerald expected
  where
    putTag = SerializePut.putWord8
    putHerald = SerializePut.putByteString . heraldEpochBytes
    putProbe = putFramedBytes . heraldFailureProbeIdCanonicalBytes
    putGeneration = SerializePut.putByteString . heraldMembershipGenerationIdBytes
    putResolution = putFramedBytes . failureProbeResolutionIdCanonicalBytes
    putDisposition disposition = SerializePut.putWord8 $ case disposition of
      DismissFailureProbe -> 0
      RetireFailureProbeTarget -> 1
    putResult result = SerializePut.putWord8 $ case result of
      ProbeReachable -> 0
      ProbeUnreachable -> 1
    putTerminal terminal = case terminal of
      ProbeVoterHostFailureAcceptedView certificate -> putTag 3 >> putFramedBytes (F.encodeAcceptedVoterHostFailure certificate)
      ProbeDismissedView resolution index -> do
        putTag 0
        putResolution resolution
        SerializePut.putWord64be (controlIndexWord64 index)
      ProbeRetiredView resolution successor index -> do
        putTag 1
        putResolution resolution
        putGeneration successor
        SerializePut.putWord64be (controlIndexWord64 index)
      ProbeMembershipSupersededView successor index -> do
        putTag 2
        putGeneration successor
        SerializePut.putWord64be (controlIndexWord64 index)

stateDisappearance :: OracleState -> D.DisappearanceState
stateDisappearance (OracleState _ _ _ _ _ _ _ _ (disappearance, _, _, _) _) = disappearance
setStateDisappearance :: D.DisappearanceState -> OracleState -> OracleState
setStateDisappearance disappearance (OracleState genesis index requests processes labels workflow completed failure (_, admission, voter, frontiers) digest) =
  OracleState genesis index requests processes labels workflow completed failure (disappearance, admission, voter, frontiers) digest
oracleDisappearanceProbe :: DD.DisappearanceProbeId -> OracleState -> Maybe D.DisappearanceProbeView
oracleDisappearanceProbe probe = D.disappearanceProbe probe . stateDisappearance
oracleDisappearanceProbes :: OracleState -> [D.DisappearanceProbeView]
oracleDisappearanceProbes = D.disappearanceProbes . stateDisappearance
oracleRegularRetirementIndex :: SortId -> OracleState -> Maybe ControlIndex
oracleRegularRetirementIndex sortId = D.disappearanceRegularRetirementIndex sortId . stateDisappearance
oracleControlledSubjectDisappeared :: GlobalObjectId -> OracleState -> Bool
oracleControlledSubjectDisappeared object = D.disappearanceControlledSubjectDeleted object . stateDisappearance

disappearanceContext :: OracleState -> D.DisappearanceContext
disappearanceContext state =
  D.DisappearanceContext
    (checkedOracleSystemId (stateGenesis state))
    (oracleCurrentMembership state)
    (fmap (Just . labelRecordRevision) (stateLabels state))
    (pendingByObject (statePendingWorkflows state))
    (Map.keysSet (Map.filter (releasedLabelStateIsDeleted . labelRecordReleasedState) (stateLabels state)))

applyLiveDisappearance :: ControlIndex -> HeraldEpoch -> D.DisappearanceCommand -> OracleState -> Either OracleRejection (OracleState, Maybe OracleAcceptedResult, [OracleProjectionEvent])
applyLiveDisappearance index home command state = do
  case (command, oraclePendingHeraldAdmission state) of
    (D.DisappearanceOpen {}, Just admission) ->
      Left (DisappearanceCommandRejected (D.DisappearanceAdmissionPreparing (A.admissionRecordId admission)))
    _ -> Right ()
  (accepted, disappearance, events) <-
    either
      (Left . DisappearanceCommandRejected)
      Right
      (D.applyDisappearanceCommand (disappearanceContext state) index home command (stateDisappearance state))
  pure (setStateDisappearance disappearance state, Just (AcceptedDisappearance accepted), fmap projectDisappearanceEvent events)

projectDisappearanceEvent :: D.DisappearanceProjectionEvent -> OracleProjectionEvent
projectDisappearanceEvent event = case event of
  D.DisappearanceOpenProjected x0 x1 -> DisappearanceProbeOpened x0 x1
  D.DisappearanceReportProjected x0 -> PredefinedAbsenceReported x0
  D.DisappearanceInvalidatedProjected x0 x1 -> DisappearanceProbeInvalidated x0 x1
  D.DisappearanceResolvedProjected x0 x1 -> DisappearanceProbeResolved x0 x1
  D.DisappearanceAbortedProjected x0 x1 -> DisappearanceProbeAborted x0 x1

-- | Compound disappearance consequences preserve the barrier/membership prefix
-- and canonical probe order. This checks malformed wire vectors at admission.
validateDisappearanceProjectionOrder :: [OracleProjectionEvent] -> SerializeGet.Get ()
validateDisappearanceProjectionOrder events = do
  let invalidations = [(probe, decision) | DisappearanceProbeInvalidated probe (D.LabelOpenDisappearanceInvalidation decision) <- events]
      aborts = [(probe, generation) | DisappearanceProbeAborted probe (D.MembershipSupersededDisappearanceAbortView generation) <- events]
      preparations = [(probe, admission) | DisappearanceProbeAborted probe (D.AdmissionPreparingDisappearanceAbortView admission) <- events]
      strictlyOrdered values = values == sort values && firstDuplicate values == Nothing
      malformed = fail "incoherent disappearance compound projection order"
      invalidatedProbes = [probe | DisappearanceProbeInvalidated probe _ <- events]
      abortedProbes = [probe | DisappearanceProbeAborted probe _ <- events]
  if strictlyOrdered invalidatedProbes && strictlyOrdered abortedProbes
    then pure ()
    else malformed
  if any isDirectDisappearance events && length events /= 1
    then malformed
    else pure ()
  case invalidations of
    [] -> pure ()
    _ -> case dropWhile isAdmissionChange events of
      LabelDecided decision LiveReleasedTerminal {} _ : remaining
        | all ((== liveDecisionId decision) . snd) invalidations
            && length remaining == length invalidations ->
            pure ()
      _ -> malformed
  case aborts of
    [] -> pure ()
    _ -> case dropWhile isAdmissionChange events of
      HeraldMembershipAdvanced membership : remaining -> do
        let (failurePrefix, afterFailure) = span isFailureConsequence remaining
            (abortPrefix, consequences) = span isMembershipAbort afterFailure
        if not (null failurePrefix)
          && length abortPrefix == length aborts
          && all ((== heraldMembershipGenerationId membership) . snd) aborts
          && all (not . isMembershipAbort) consequences
          then pure ()
          else malformed
      _ -> malformed
  case preparations of
    [] -> pure ()
    _ -> case events of
      HeraldAdmissionChanged record : remaining
        | A.admissionRecordPhase record == A.AdmissionPreparing
            && A.admissionRecordBeginIndex record == A.admissionRecordChangedIndex record
            && A.admissionRecordAttempt record == 1
            && all ((== A.admissionRecordId record) . snd) preparations
            && length remaining == length preparations ->
            pure ()
      _ -> malformed
  where
    isAdmissionChange HeraldAdmissionChanged {} = True
    isAdmissionChange _ = False
    isFailureConsequence OracleFailureProbeRetired {} = True
    isFailureConsequence OracleFailureProbeSuperseded {} = True
    isFailureConsequence _ = False
    isMembershipAbort (DisappearanceProbeAborted _ D.MembershipSupersededDisappearanceAbortView {}) = True
    isMembershipAbort _ = False

isDirectDisappearance :: OracleProjectionEvent -> Bool
isDirectDisappearance event = case event of
  DisappearanceProbeOpened {} -> True
  PredefinedAbsenceReported {} -> True
  DisappearanceProbeResolved {} -> True
  DisappearanceProbeInvalidated _ D.CommandDisappearanceInvalidation {} -> True
  DisappearanceProbeAborted _ D.ExplicitDisappearanceAbortReasonView {} -> True
  _ -> False

disappearanceReceiptMatchesEvents :: OracleReceipt -> [OracleProjectionEvent] -> Bool
disappearanceReceiptMatchesEvents receipt events = case oracleReceiptDisappearanceResult receipt of
  Nothing -> not (any isDirectDisappearance events)
  Just accepted -> case (accepted, events) of
    (D.DisappearanceOpenAccepted result, [DisappearanceProbeOpened _ projected]) -> result == projected
    (D.DisappearanceReportAccepted probe reporter, [PredefinedAbsenceReported claim]) ->
      probe == DD.disappearanceEvidenceClaimProbeId claim && reporter == DD.disappearanceEvidenceClaimReporter claim
    (D.DisappearanceInvalidateAccepted probe, [DisappearanceProbeInvalidated projected D.CommandDisappearanceInvalidation {}]) -> probe == projected
    (D.DisappearanceResolveAccepted outcome, [DisappearanceProbeResolved _ projected]) -> outcome == projected
    (D.DisappearanceAbortAccepted probe, [DisappearanceProbeAborted projected D.ExplicitDisappearanceAbortReasonView {}]) -> probe == projected
    (D.DisappearanceOpenAccepted {}, _) -> False
    (_, []) -> True
    _ -> False

-- Herald admission shares the serialized Oracle request/control owner.
stateAdmission :: OracleState -> A.AdmissionState
stateAdmission (OracleState _ _ _ _ _ _ _ _ (_, admission, _, _) _) = admission
setStateAdmission :: A.AdmissionState -> OracleState -> OracleState
setStateAdmission admission (OracleState genesis index requests processes labels workflow completed failure (disappearance, _, voter, frontiers) digest) =
  OracleState genesis index requests processes labels workflow completed failure (disappearance, admission, voter, frontiers) digest
oracleHeraldCatalogue :: OracleState -> [A.HeraldAdmissionManifest]
oracleHeraldCatalogue = A.admissionCatalogue . stateAdmission
oracleHeraldAdmissions :: OracleState -> [A.HeraldAdmissionRecord]
oracleHeraldAdmissions = A.admissionRecords . stateAdmission
oraclePendingHeraldAdmission :: OracleState -> Maybe A.HeraldAdmissionRecord
oraclePendingHeraldAdmission = A.pendingAdmission . stateAdmission
oracleHeraldAdmission :: HeraldAdmissionId -> OracleState -> Maybe A.HeraldAdmissionRecord
oracleHeraldAdmission ident = A.lookupAdmission ident . stateAdmission

applyLiveAdmission :: ControlIndex -> HeraldEpoch -> A.HeraldAdmissionCommand -> OracleState -> Either OracleRejection (OracleState, Maybe OracleAcceptedResult, [OracleProjectionEvent])
applyLiveAdmission index home command state = do
  requireNoVoterChange state
  unless (not (failureHasPendingRetirement (stateFailure state))) (Left (VoterCommandRejected V.VoterMembershipCoordinationBusy))
  (admission, record, generation) <-
    either
      (Left . HeraldAdmissionCommandRejected)
      Right
      (A.applyAdmissionCommand (checkedOracleSystemId (stateGenesis state)) (oracleCurrentMembership state) (not (Map.null (pendingByDecision (statePendingWorkflows state)))) index home command (stateAdmission state))
  let withAdmission = setStateAdmission admission state
      (updated, membershipEvents) = case generation of
        Nothing -> (withAdmission, [])
        Just successor ->
          let (failure, events) = installAdmissionMembership index successor (stateFailure state)
           in (setStateFailure failure withAdmission, fmap projectFailureEvent events)
      (final, preparationEvents) = case command of
        A.BeginHeraldAdmission {} ->
          let (disappearance, events) = D.abortDisappearanceForAdmission index (A.admissionRecordId record) (stateDisappearance updated)
           in (setStateDisappearance disappearance updated, fmap projectDisappearanceEvent events)
        _ -> (updated, [])
  pure (final, Nothing, HeraldAdmissionChanged record : membershipEvents <> preparationEvents)

reconcileHeraldAdmission :: ControlIndex -> Maybe OracleAcceptedResult -> [OracleProjectionEvent] -> OracleState -> (OracleState, [OracleProjectionEvent])
reconcileHeraldAdmission index accepted events state
  | failureAccepted || any retirement events = apply A.cancelPendingAdmission
  | any changesCut events = apply A.invalidateAdmission
  | otherwise = (state, events)
  where
    failureAccepted = case accepted of
      Just (AcceptedFailure (FailureProbeReportRecorded _ (Just (FailureResolutionReady _ RetireFailureProbeTarget _)))) -> True
      _ -> False
    retirement (HeraldMembershipAdvanced generation) = heraldMembershipGenerationRetiredHeraldEpoch generation /= Nothing
    retirement _ = False
    changesCut ProcessStarted {} = True
    changesCut ProcessEpochEnded {} = True
    changesCut (LabelDecided _ LiveReleasedTerminal {} _) = True
    changesCut DisappearanceProbeResolved {} = True
    changesCut _ = False
    apply transition =
      let (admission, records) = transition index (stateAdmission state)
       in (setStateAdmission admission state, fmap HeraldAdmissionChanged records <> events)

-- Replica administration shares the sole Oracle owner and membership slot.
registerOracleReplicaCommand :: RaftNodeId -> HeraldEpoch -> V.OracleReplicaContact -> OracleCommand
registerOracleReplicaCommand node host = VoterCommand . V.RegisterOracleReplica node host
beginVoterChangeCommand :: V.VoterConfigurationId -> V.ExplicitVoterChangeReason -> V.OracleVoterBindings -> OracleCommand
beginVoterChangeCommand expected reason = VoterCommand . V.BeginVoterChange expected reason
cancelVoterChangeCommand :: V.VoterChangeId -> OracleCommand
cancelVoterChangeCommand = VoterCommand . V.CancelVoterChange
oracleCommandVoterChangeCancellation :: OracleCommand -> Maybe V.VoterChangeId
oracleCommandVoterChangeCancellation (VoterCommand (V.CancelVoterChange ident)) = Just ident
oracleCommandVoterChangeCancellation _ = Nothing
stateVoter :: OracleState -> V.VoterState
stateVoter (OracleState _ _ _ _ _ _ _ _ (_, _, voter, _) _) = voter
setStateVoter :: V.VoterState -> OracleState -> OracleState
setStateVoter voter (OracleState genesis index requests processes labels workflow completed failure (disappearance, admission, _, frontiers) digest) =
  OracleState genesis index requests processes labels workflow completed failure (disappearance, admission, voter, frontiers) digest
oracleReplicaRegistrations :: OracleState -> [V.OracleReplicaRegistration]
oracleReplicaRegistrations = V.voterRegistrations . stateVoter

-- Current registration facts only: final automatic exclusion stops replication
-- before the later semantic retirement. Explicit demotion retains a learner.
oracleReplicaReplicationTargets :: OracleState -> [RaftNodeId]
oracleReplicaReplicationTargets state =
  [ V.replicaRegistrationNode registration
  | registration <- oracleReplicaRegistrations state,
    Set.member (V.replicaRegistrationHost registration) (stateActiveHeralds state),
    not (Set.member (V.replicaRegistrationNode registration) excluded)
  ]
  where
    excluded = case oraclePendingVoterChange state of
      Just change
        | V.voterChangePhase change == V.VoterExcludedAwaitingHeraldRetirement,
          V.AcceptedHostFailureReason _ <- V.voterChangeReason change ->
            Set.fromList (fmap raftVoterBindingNode (V.oracleVoterBindingList (V.voterChangeOldBindings change))) Set.\\ Set.fromList (fmap raftVoterBindingNode (V.oracleVoterBindingList (V.voterChangeNewBindings change)))
      _ -> Set.empty
oracleVoterConfiguration :: OracleState -> V.VoterConfiguration
oracleVoterConfiguration = V.voterCurrentConfiguration . stateVoter
oracleVoterConfigurations :: OracleState -> [V.VoterConfiguration]
oracleVoterConfigurations = V.voterConfigurationHistory . stateVoter
oracleVoterChange :: V.VoterChangeId -> OracleState -> Maybe V.VoterChange
oracleVoterChange ident = V.voterLookupChange ident . stateVoter
oraclePendingVoterChange :: OracleState -> Maybe V.VoterChange
oraclePendingVoterChange = V.voterPendingChange . stateVoter
prepareOracleConfiguration :: V.VoterChangeId -> V.ConfigurationStage -> OracleState -> Either V.VoterChangeProblem (RaftVotingConfiguration, ByteString)
prepareOracleConfiguration ident stage = V.prepareVoterConfiguration ident stage . stateVoter

applyLiveVoter :: ControlIndex -> V.VoterCommand -> OracleState -> Either OracleRejection (OracleState, Maybe OracleAcceptedResult, [OracleProjectionEvent])
applyLiveVoter index command state = do
  let busy = oraclePendingHeraldAdmission state /= Nothing || failureHasPendingRetirement (stateFailure state)
  (voter, result) <- either (Left . VoterCommandRejected) Right (V.applyVoterCommand (stateActiveHeralds state) busy index command (stateVoter state))
  let updated = setStateVoter voter state
      (successor, superseded) = case (command, result) of
        (V.BeginVoterChange {}, V.VoterChangeAccepted {}) ->
          let (failure, failureEvents) = supersedeFailureProbesForVoters index (currentVoterHosts state) (stateFailure state)
           in (setStateFailure failure updated, fmap projectFailureEvent failureEvents)
        _ -> (updated, [])
      events = case result of
        V.ReplicaRegistered registration -> [OracleReplicaRegistered registration]
        V.ReplicaAlreadyVoter registration -> [OracleReplicaRegistered registration]
        V.ReplicaAlreadyLearner registration -> [OracleReplicaRegistered registration]
        V.VoterChangeAccepted change -> [OracleVoterChangeChanged change]
        V.VoterChangeCancelledResult change -> [OracleVoterChangeChanged change]
  pure (successor, Just (AcceptedVoter result), events <> superseded)

currentVoterHosts :: OracleState -> Set.Set HeraldEpoch
currentVoterHosts = Set.fromList . fmap raftVoterBindingHeraldEpoch . V.voterConfigurationBindings . oracleVoterConfiguration

applyCommittedVoterState :: RaftCommittedEntry ByteString -> OracleState -> Either V.VoterChangeProblem (OracleState, AppliedOracleEntry)
applyCommittedVoterState native state = do
  let nextIndex = controlIndex (controlIndexWord64 (oracleGreatestControlIndex state) + 1)
  (voter, configuration, change) <- V.applyVoterConfiguration nextIndex native (stateVoter state)
  let withVoters = setStateVoter voter state
      (failure, _) = supersedeFailureProbesForVoters nextIndex (currentVoterHosts withVoters) (stateFailure withVoters)
      withFailure = setStateFailure failure withVoters
      successor = refreshLiveOracleStateDigest (setControlIndex nextIndex withFailure)
      events = [OracleVoterConfigurationChanged configuration, OracleVoterChangeChanged change]
  pure (successor, AppliedOracleEntry nextIndex (AppliedConfigurationOrigin configuration) events (oracleStateDigest successor))
  where
    setControlIndex index (OracleState genesis _ requests processes labels workflow completed failure rest digest) =
      OracleState genesis index requests processes labels workflow completed failure rest digest

getTypedReceiptRetirementEntry :: SerializeGet.Get AppliedOracleEntry
getTypedReceiptRetirementEntry = do
  domainTag <- getFramedBytes
  requireGetDomain liveReceiptRetirementEntryDomain domainTag
  index <- getPositiveControlIndex
  home <- getHeraldEpoch
  through <- getOracleProgress
  unless (oracleProgressLabelsThrough through <= index) (fail "Oracle progress exceeds its control index")
  events <- getFramedBytes >>= decodeNestedGet getTypedLiveProjectionEventVector
  unless (null events) (fail "receipt-retirement entry carries semantic projection events")
  digest <- getOptionalOracleStateDigest
  pure (AppliedOracleEntry index (AppliedProgressOrigin home through) [] digest)

getTypedConfigurationEntry :: SerializeGet.Get AppliedOracleEntry
getTypedConfigurationEntry = do
  domainTag <- getFramedBytes
  requireGetDomain liveConfigurationEntryDomain domainTag
  index <- getPositiveControlIndex
  configuration <- getRequiredFramedValue >>= admitGet "voter configuration" . V.decodeVoterConfiguration
  events <- getFramedBytes >>= decodeNestedGet getTypedLiveProjectionEventVector
  digest <- getOptionalOracleStateDigest
  let coherent = case events of
        [OracleVoterConfigurationChanged same, OracleVoterChangeChanged change] ->
          same == configuration
            && V.voterConfigurationControlIndex configuration == index
            && V.voterChangeControlIndex change == index
            && case (V.voterConfigurationView configuration, V.voterChangePhase change) of
              (V.JointVoterConfigurationView old new, V.VoterChangeJointCommitted) -> old == V.voterChangeOldBindings change && new == V.voterChangeNewBindings change
              (V.StableVoterConfigurationView new, V.VoterChangeCompleted) -> new == V.voterChangeNewBindings change && case V.voterChangeReason change of V.ExplicitVoterReason _ -> True; _ -> False
              (V.StableVoterConfigurationView new, V.VoterExcludedAwaitingHeraldRetirement) -> new == V.voterChangeNewBindings change && case V.voterChangeReason change of V.AcceptedHostFailureReason _ -> True; _ -> False
              _ -> False
        _ -> False
  if coherent
    then pure (AppliedOracleEntry index (AppliedConfigurationOrigin configuration) events digest)
    else fail "incoherent committed Oracle configuration entry"

requireNoVoterChange :: OracleState -> Either OracleRejection ()
requireNoVoterChange state = case oraclePendingVoterChange state of
  Just _ -> Left (VoterCommandRejected V.VoterChangeInProgress)
  Nothing -> Right ()

voterReceiptMatchesEvents :: OracleReceipt -> [OracleProjectionEvent] -> Bool
voterReceiptMatchesEvents receipt events = case oracleReceiptVoterResult receipt of
  Nothing -> case oracleReceiptFailureResult receipt of
    Just (VoterHostFailureAcceptedResult expected) -> case events of
      [OracleVoterHostFailureAccepted observed] -> expected == observed && F.acceptedVoterHostFailureControlIndex observed <= oracleReceiptControlIndex receipt
      OracleVoterHostFailureAccepted observed : OracleVoterChangeChanged change : rest ->
        expected == observed
          && F.acceptedVoterHostFailureControlIndex observed == oracleReceiptControlIndex receipt
          && V.voterChangeId change == F.acceptedVoterHostFailureChangeId observed
          && V.voterChangeExpectedConfiguration change == V.voterConfigurationId (F.acceptedVoterHostFailureConfiguration observed)
          && V.voterChangeReason change == V.AcceptedHostFailureReason (F.acceptedVoterHostFailureResolution observed)
          && V.voterChangePhase change == V.VoterChangePreparing
          && V.voterChangeControlIndex change == oracleReceiptControlIndex receipt
          && V.oracleVoterBindingList (V.voterChangeOldBindings change) == V.voterConfigurationBindings (F.acceptedVoterHostFailureConfiguration observed)
          && V.oracleVoterBindingList (V.voterChangeNewBindings change) == filter ((/= F.acceptedVoterHostFailureTarget observed) . raftVoterBindingHeraldEpoch) (V.voterConfigurationBindings (F.acceptedVoterHostFailureConfiguration observed))
          && all superseded rest
      _ -> False
    Just (HeraldRetiredResult resolution successor) -> case [change | OracleVoterChangeChanged change <- events] of
      [] -> not (any voterEvent events)
      [change] ->
        V.voterChangeReason change == V.AcceptedHostFailureReason resolution
          && V.voterChangePhase change == V.VoterChangeCompleted
          && V.voterChangeControlIndex change == oracleReceiptControlIndex receipt
          && any (\event -> case event of HeraldMembershipAdvanced generation -> heraldMembershipGenerationId generation == successor; _ -> False) events
          && all (\event -> case event of OracleVoterChangeChanged _ -> True; _ -> not (voterEvent event)) events
      _ -> False
    _ -> not (any voterEvent events)
  Just result -> case (result, events) of
    (V.ReplicaRegistered expected, [OracleReplicaRegistered observed]) -> expected == observed && registrationIndex observed
    (V.ReplicaAlreadyVoter expected, [OracleReplicaRegistered observed]) -> expected == observed && registrationIndex observed
    (V.ReplicaAlreadyLearner expected, [OracleReplicaRegistered observed]) -> expected == observed && registrationIndex observed
    (V.VoterChangeAccepted expected, OracleVoterChangeChanged observed : rest) ->
      expected == observed
        && V.voterChangePhase observed == V.VoterChangePreparing
        && V.voterChangeControlIndex observed == oracleReceiptControlIndex receipt
        && all superseded rest
    (V.VoterChangeCancelledResult expected, [OracleVoterChangeChanged observed]) ->
      expected == observed
        && V.voterChangePhase observed == V.VoterChangeCancelled
        && V.voterChangeControlIndex observed <= oracleReceiptControlIndex receipt
    _ -> False
  where
    registrationIndex registration = V.replicaRegistrationControlIndex registration <= oracleReceiptControlIndex receipt
    voterEvent OracleVoterHostFailureAccepted {} = True
    voterEvent OracleReplicaRegistered {} = True
    voterEvent OracleVoterChangeChanged {} = True
    voterEvent OracleVoterConfigurationChanged {} = True
    voterEvent _ = False
    superseded OracleFailureProbeSuperseded {} = True
    superseded _ = False

applyLiveAcceptedHostFailure :: ControlIndex -> HeraldEpoch -> FailureProbeResolutionId -> OracleState -> Either OracleRejection (OracleState, Maybe OracleAcceptedResult, [OracleProjectionEvent])
applyLiveAcceptedHostFailure index reporter resolution state = do
  (failure, result, acceptedEvents) <- either (Left . FailureCommandRejected) Right (F.acceptVoterHostFailure index reporter resolution (oracleVoterConfiguration state) (stateFailure state))
  case result of
    VoterHostFailureAcceptedResult certificate
      | null acceptedEvents -> pure (state, Just (AcceptedFailure result), [OracleVoterHostFailureAccepted certificate])
      | otherwise -> do
          requireNoVoterChange state
          unless (oraclePendingHeraldAdmission state == Nothing) (Left (VoterCommandRejected V.VoterMembershipCoordinationBusy))
          (voter, change) <- either (Left . VoterCommandRejected) Right (V.beginAcceptedHostFailure index resolution (F.acceptedVoterHostFailureTarget certificate) (stateVoter state))
          let (supersededFailure, supersededEvents) = supersedeFailureProbesForVoters index (currentVoterHosts state) failure
          pure
            ( setStateFailure supersededFailure (setStateVoter voter state),
              Just (AcceptedFailure result),
              [OracleVoterHostFailureAccepted certificate, OracleVoterChangeChanged change] <> fmap projectFailureEvent supersededEvents
            )
    _ -> error "admitted voter-host acceptance returned another result family"

-- Retirement is canonical lifetime metadata. The high-water mark is a transport
-- correlation; membership in the released set is the authority to forget.
retirementHighWater :: ReceiptRetirement -> Word64
retirementHighWater = maybe 0 id . Lifetime.receiptRetirementHighWater

putReceiptProgress :: ReceiptRetirement -> SerializePut.Put
putReceiptProgress progress = do
  case Lifetime.receiptRetirementHighWater progress of
    Nothing -> SerializePut.putWord8 0
    Just through -> SerializePut.putWord8 1 >> SerializePut.putWord64be through
  putCountedList SerializePut.putWord64be (Set.toAscList (Lifetime.receiptRetirementExceptions progress))

putOracleProgress :: OracleProgress -> SerializePut.Put
putOracleProgress progress = do
  putReceiptProgress (oracleProgressReceipts progress)
  SerializePut.putWord64be (controlIndexWord64 (oracleProgressLabelsThrough progress))

getOracleProgress :: SerializeGet.Get OracleProgress
getOracleProgress = oracleProgress <$> getReceiptProgress <*> (controlIndex <$> SerializeGet.getWord64be)

getReceiptProgress :: SerializeGet.Get ReceiptRetirement
getReceiptProgress = do
  through <-
    SerializeGet.getWord8 >>= \case
      0 -> pure Nothing
      1 -> Just <$> SerializeGet.getWord64be
      _ -> fail "invalid receipt retirement high-water tag"
  count <- getCanonicalCount
  exceptions <- replicateM count SerializeGet.getWord64be
  unless (exceptions == Set.toAscList (Set.fromList exceptions)) (fail "noncanonical receipt retirement exceptions")
  either (fail . show) pure (Lifetime.receiptRetirement through (Set.fromList exceptions))

-- A checkpoint carries the current owner state, never the command transcript.
-- Genesis is admitted locally; the wire snapshot must name that same immutable
-- initial projection. Every nested checked value keeps its existing admission.
oracleCheckpointBytes :: OracleState -> ByteString
oracleCheckpointBytes state = SerializePut.runPut $ do
  putFramedBytes "ECLIPS-ORACLE-CHECKPOINT"
  putFramedBytes (SerializePut.runPut (putLiveGenesis (stateGenesis state)))
  SerializePut.putWord64be (controlIndexWord64 (oracleGreatestControlIndex state))
  putCountedList putRequestRecord (Map.toAscList (stateRequests state))
  putCountedList putProcessRecord (Map.toAscList (stateProcesses state))
  putCountedList putLabelRecordEntry (Map.toAscList (stateLabels state))
  putCountedList (putCheckpointWorkflow . snd) (stateWorkflowEntries state)
  putCountedList putCompletedEntry (Map.toAscList (stateCompleted state))
  putFramedBytes (F.encodeFailureCheckpoint (stateFailure state))
  putFramedBytes (D.encodeDisappearanceCheckpoint (stateDisappearance state))
  putFramedBytes (A.encodeAdmissionCheckpoint (stateAdmission state))
  putFramedBytes (V.encodeVoterCheckpoint (stateVoter state))
  putOracleLifetime state
  putOptionalOracleStateDigest (oracleStateDigest state)

-- | A complete canonical snapshot is the public admission boundary for remote
-- Raft state installation. When enabled, the exact diagnostic digest is checked
-- after reconstruction. Both modes retain nested checked-value admission and
-- canonical reencoding; the diagnostic mode is preserved by the snapshot.
decodeOracleCheckpoint :: CheckedOracleGenesis -> ByteString -> Either OracleCanonicalError OracleState
decodeOracleCheckpoint genesis bytes = do
  state <- decodeLiveCanonical getter bytes
  unless
    (oracleStateDigest (refreshLiveOracleStateDigest state) == oracleStateDigest state)
    (Left (MalformedCanonicalOracleBytes "checkpoint state digest mismatch"))
  unless
    (oracleCheckpointBytes state == bytes)
    (Left (MalformedCanonicalOracleBytes "noncanonical Oracle checkpoint"))
  pure state
  where
    getter = do
      getFramedBytes >>= requireGetDomain "ECLIPS-ORACLE-CHECKPOINT"
      initial <- getFramedBytes
      unless (initial == SerializePut.runPut (putLiveGenesis genesis)) (fail "checkpoint genesis mismatch")
      index <- controlIndex <$> SerializeGet.getWord64be
      requests <- getCheckpointMap $ do
        request <- getOracleClientRequestId
        digest <- getOracleCommandDigest
        receiptBytes <- getRequiredFramedValue
        receipt <- decodedOracleReceiptValue <$> admitGet "checkpoint receipt" (decodeOracleReceiptCanonicalBytes receiptBytes)
        unless (oracleReceiptRequestId receipt == request && oracleReceiptCommandDigest receipt == digest) (fail "checkpoint receipt key mismatch")
        pure (request, oracleRequestRecord receipt)
      processes <- getCheckpointMap $ do
        key <- getProcessEpochId
        record <- getTypedProcessEpochRecord
        unless (key == processRecordProcessEpoch record) (fail "checkpoint process key mismatch")
        pure (key, record)
      labels <- getCheckpointMap $ do
        object <- getGlobalObjectId
        record <- getCheckpointLabel object
        pure (object, record)
      workflows <- getCheckpointMap $ do
        workflow <- getCheckpointWorkflow
        pure (liveDecisionId (workflowDecision workflow), workflow)
      completed <- getCheckpointMap $ do
        key <- getLabelDecisionId
        decisionIndex <- getPositiveControlIndex
        generation <- getMembershipGenerationId
        digest <- getLabelOutcomeDigest
        pure (completedDecisionKey key, CompletedWorkflow decisionIndex generation (completedDigest digest))
      failure <- getFramedBytes >>= either fail pure . F.decodeFailureCheckpoint genesis
      disappearance <- getFramedBytes >>= either fail pure . D.decodeDisappearanceCheckpoint
      admission <- getFramedBytes >>= either fail pure . A.decodeAdmissionCheckpoint genesis
      voter <- getFramedBytes >>= either fail pure . V.decodeVoterCheckpoint genesis
      latest <- controlIndex <$> SerializeGet.getWord64be
      retired <- controlIndex <$> SerializeGet.getWord64be
      progress <- getCheckpointMap ((,) <$> getHeraldEpoch <*> getOracleProgress)
      digest <- getOptionalOracleStateDigest
      let history = failureCheckedMembershipHistory failure
          decisions = map workflowDecision (Map.elems workflows)
      unless
        (all (maybe False (not . null) . liveDecisionCapturedHeralds history) decisions)
        (fail "checkpoint decision references unknown membership")
      unless
        (all (maybe False (not . null) . capturedMembershipHeralds history . completedMembership) (Map.elems completed))
        (fail "checkpoint completed witness references unknown membership")
      let validAdmissionProbe (D.DisappearanceProbeView _ _ _ captured _ _ _ phase) = case phase of
            D.DisappearanceAdmissionPreparingView ident terminalIndex ->
              terminalIndex <= index
                && case A.lookupAdmission ident admission of
                  Just record ->
                    A.admissionRecordBeginIndex record == terminalIndex
                      && heraldMembershipGenerationId (A.admissionRecordPredecessor record) == captured
                  Nothing -> False
            D.DisappearanceCollectingView -> A.pendingAdmission admission == Nothing
            _ -> True
      unless
        (all validAdmissionProbe (D.disappearanceProbes disappearance))
        (fail "checkpoint disappearance admission cause mismatch")
      let promises = Map.map oracleProgressLabelsThrough progress
          receipts = Map.filter (/= mempty) (Map.map oracleProgressReceipts progress)
          counts = Map.fromListWith (+) [(through, 1) | through <- Map.elems promises]
          completedEntries = [(completedControlIndex witness, key) | (key, witness) <- Map.toAscList completed]
          byIndex = Map.fromList completedEntries
          lifetime = OracleLifetime receipts promises counts retired latest byIndex
          objects = map (liveDecisionObject . workflowDecision) (Map.elems workflows)
          provisional = OracleState genesis index requests processes labels emptyPendingWorkflows completed failure (disappearance, admission, voter, lifetime) digest
      unless (Map.keysSet promises == failureActiveHeralds failure) (fail "checkpoint label promises do not cover exact active membership")
      unless (retired <= latest && latest <= index && all (\through -> retired <= through && through <= latest) (Map.elems promises)) (fail "checkpoint label progress frontier ordering")
      unless (all (\witness -> retired < completedControlIndex witness && completedControlIndex witness <= latest) (Map.elems completed)) (fail "checkpoint completed witness outside retained label interval")
      unless (retired == fst (Map.findMin counts)) (fail "checkpoint label floor differs from active promise minimum")
      let originalIndices = map fst completedEntries <> map (liveDecisionRequestControlIndex . workflowDecision) (Map.elems workflows)
      unless (length originalIndices == Set.size (Set.fromList originalIndices)) (fail "checkpoint label decisions share an original index")
      unless (all ((<= latest) . liveDecisionRequestControlIndex . workflowDecision) (Map.elems workflows)) (fail "checkpoint pending decision exceeds label high water")
      unless (length objects == Set.size (Set.fromList objects)) (fail "checkpoint has multiple pending decisions for one object")
      unless (all (not . (`Map.member` completed) . completedDecisionKey) (Map.keys workflows)) (fail "checkpoint decision is pending and complete")
      unless (all (not . Set.null . workflowSurvivors provisional . workflowDecision) (Map.elems workflows)) (fail "checkpoint pending decision has no survivor")
      pure (foldl' (flip insertPendingWorkflow) provisional (Map.elems workflows))

oracleRequestReceipts :: OracleState -> [OracleReceipt]
oracleRequestReceipts = fmap (\(OracleRequestRecord _ receipt _) -> receipt) . Map.elems . stateRequests

getCheckpointMap :: (Ord key) => SerializeGet.Get (key, value) -> SerializeGet.Get (Map key value)
getCheckpointMap entry = do
  count <- getCanonicalCount
  entries <- replicateM count entry
  unless (map fst entries == Set.toAscList (Set.fromList (map fst entries))) (fail "noncanonical checkpoint map")
  pure (Map.fromDistinctAscList entries)

getCheckpointLabel :: GlobalObjectId -> SerializeGet.Get LabelRecord
getCheckpointLabel object = do
  value <- getTypedReleasedState
  revision <- getPositiveLabelRevision
  authority <- getMaybeAuthorityEpoch
  admitGet "checkpoint label" (mkLabelRecord object value revision authority)

putCheckpointWorkflow :: OracleLabelWorkflow -> SerializePut.Put
putCheckpointWorkflow (OracleLabelWorkflow decision (OracleReleased terminal digest record)) = do
  putLiveDecision decision
  putCheckpointTerminal terminal
  SerializePut.putByteString (labelOutcomeDigestBytes digest)
  putDomainLabelRecord record

getCheckpointWorkflow :: SerializeGet.Get OracleLabelWorkflow
getCheckpointWorkflow = do
  decision <- getTypedLabelDecision
  terminal <- getCheckpointTerminal
  digest <- getLabelOutcomeDigest
  validateAtomicDecision decision terminal digest
  record <- getCheckpointLabel (liveDecisionObject decision)
  case terminal of
    LiveReleasedTerminal _ _ _ overlay -> do
      unless
        (labelRecordReleasedState record == liveRecordStoredState overlay && labelRecordRevision record == liveRecordRevision overlay && labelRecordRetainedAuthority record == liveRecordRetainedAuthority overlay)
        (fail "checkpoint pending label record mismatch")
      pure (OracleLabelWorkflow decision (OracleReleased terminal digest record))
    _ -> fail "NotApplied retained as pending installation"

validateAtomicDecision :: LiveLabelDecision -> LiveTerminalOutcome -> LabelOutcomeDigest -> SerializeGet.Get ()
validateAtomicDecision decision terminal digest = do
  unless (deriveLabelOutcomeDigest terminal == digest) (fail "label decision outcome digest mismatch")
  validateTerminalOutcome terminal
  let index = liveDecisionRequestControlIndex decision
      object = liveDecisionObject decision
      ident = liveDecisionId decision
      members = liveDecisionMemberSetDigest decision
  case terminal of
    LiveNotAppliedTerminal terminalDecision terminalIndex terminalObject _ _ terminalMembers ->
      unless
        (terminalDecision == ident && terminalIndex == index && terminalObject == object && terminalMembers == members)
        (fail "NotApplied facts mismatch decision")
    LiveReleasedTerminal facts _ terminalIndex _ -> do
      unless
        (livePreparedFactsDecisionId facts == ident && livePreparedFactsResolveIndex facts == index && livePreparedFactsObjectId facts == object && livePreparedFactsMemberSetDigest facts == members && livePreparedFactsExpectedPriorState facts == releasedLabel (liveDecisionExpectedLabel decision) && livePreparedFactsExpectedPriorRevision facts == liveDecisionExpectedPriorRevision decision && livePreparedFactsExpectedPriorAuthority facts == liveDecisionExpectedPriorAuthority decision && livePreparedFactsPriorAuthorityJustification facts == liveDecisionPriorAuthorityJustification decision && terminalIndex == index)
        (fail "Applied facts mismatch decision")

-- Terminal consistency is independent of its enclosing decision or checkpoint.
-- Reuse it at both canonical event admission and the portable fact boundary.
validateTerminalOutcome :: LiveTerminalOutcome -> SerializeGet.Get ()
validateTerminalOutcome (LiveNotAppliedTerminal {}) = pure ()
validateTerminalOutcome (LiveReleasedTerminal facts digest index overlay) =
  unless
    ( deriveLivePreparedLabelDigest facts == digest
        && livePreparedFactsResolveIndex facts == index
        && labelRevisionControlIndex (liveRecordRevision overlay) == index
        && livePreparedFactsProposedOutcome facts == liveRecordStoredState overlay
        && releaseAuthority index (preparedLabelFactsAuthorityDisposition facts) == liveRecordRetainedAuthority overlay
    )
    (fail "inconsistent released label terminal")

putCheckpointTerminal :: LiveTerminalOutcome -> SerializePut.Put
putCheckpointTerminal = \case
  LiveNotAppliedTerminal decision index object reason catalogue members -> do
    SerializePut.putWord8 0
    SerializePut.putByteString (labelDecisionIdBytes decision)
    SerializePut.putWord64be (controlIndexWord64 index)
    SerializePut.putByteString (globalObjectIdBytes object)
    putLiveNotAppliedReason reason
    SerializePut.putByteString (catalogueDigestBytes catalogue)
    SerializePut.putByteString (memberSetDigestBytes members)
  LiveReleasedTerminal facts digest index overlay -> do
    SerializePut.putWord8 1
    putLivePreparedFacts facts
    SerializePut.putByteString (preparedLabelDigestBytes digest)
    SerializePut.putWord64be (controlIndexWord64 index)
    putReleasedLabelOverlay overlay

getCheckpointTerminal :: SerializeGet.Get LiveTerminalOutcome
getCheckpointTerminal =
  SerializeGet.getWord8 >>= \case
    0 -> LiveNotAppliedTerminal <$> getLabelDecisionId <*> getPositiveControlIndex <*> getGlobalObjectId <*> getTypedNotAppliedReason <*> getCatalogueDigest <*> getMemberSetDigest
    1 ->
      LiveReleasedTerminal
        <$> (getRequiredFramedValue >>= admitGet "checkpoint prepared facts" . decodePreparedLabelFactsCanonicalBytes)
        <*> getPreparedLabelDigest
        <*> getPositiveControlIndex
        <*> getTypedReleasedLabelOverlay
    _ -> fail "invalid checkpoint terminal"
