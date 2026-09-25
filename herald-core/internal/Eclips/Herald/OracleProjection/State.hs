{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure contiguous Oracle projection owned by one Herald.
--
-- The state starts from the complete checked index-zero projection. Later
-- entries are admitted only in contiguous Oracle order. A checked replay base
-- can cover released canonical prefixes; surviving exact pins still classify
-- overlapping bytes without consulting a runtime ledger.
module Eclips.Herald.OracleProjection.State
  ( State,
    View,
    initialState,
    initializeOracleVoters,
    joiningReplayOrigin,
    adoptJoiningReplay,
    oracleView,
    oracleViewSystemId,
    oracleViewAppliedBootstraps,
    oracleViewControlIndex,
    oracleViewVoterConfiguration,
    oracleViewOracleReplicas,
    oracleViewVoterChanges,
    oracleViewVoterChange,
    oracleViewPendingVoterChange,
    oracleViewLocalHeraldEpoch,
    oracleViewCurrentHeraldMembership,
    oracleViewCurrentHeraldMembershipId,
    oracleViewHeraldMembershipHistory,
    oracleViewHeraldMembershipHistoryChecked,
    oracleViewHeraldMembershipLineage,
    oracleViewHeraldMembershipById,
    oracleViewHeraldMembershipAt,
    oracleViewActiveHeralds,
    oracleViewActiveHeraldEpochs,
    oracleViewLocalHeraldIsCurrent,
    oracleViewInitialProjectionDigest,
    oracleViewIsActiveHerald,
    oracleViewHeraldAdmission,
    oracleViewPendingHeraldAdmission,
    projectedHeraldAdmissions,
    oracleViewProcessResidence,
    oracleViewProcessIsLive,
    oracleViewEndedProcess,
    oracleViewLabelWorkflowCompleted,
    oracleViewLabelWorkflow,
    oracleViewAuthorizesProcess,
    oracleViewStartedProcess,
    oracleViewSatisfiesPrerequisite,
    oracleProjectionProcessResidenceAt,
    oracleProjectionProcessIsLiveAt,
    oracleProjectionReleasedLabelAt,
    projectedProcessEpochs,
    projectedBootstraps,
    ProjectedStartedProcess,
    projectedStartedProcessBootstrap,
    projectedStartedProcessControlIndex,
    projectedStartedProcessRequest,
    projectedStartedProcesses,
    ProjectedEndedProcess,
    projectedEndedProcessEpoch,
    projectedEndedProcessControlIndex,
    projectedEndedProcessReason,
    projectedEndedProcesses,
    ProjectedLabelWorkflow,
    LabelWorkflowPhase (..),
    projectedLabelWorkflowDecision,
    projectedLabelWorkflowPhase,
    projectedLabelWorkflowPreparedFacts,
    projectedLabelWorkflowPreparedDigest,
    projectedLabelWorkflowTerminal,
    ProjectedFailureProbe,
    ProjectedFailureProbeTerminal (..),
    projectedFailureProbeTarget,
    projectedFailureProbeGeneration,
    projectedFailureProbeConfiguration,
    projectedFailureProbeReports,
    projectedFailureProbeTerminal,
    oracleViewFailureProbe,
    oracleViewMayReportFailureProbe,
    ProjectedLabelTerminal (..),
    projectedLabelWorkflows,
    ProjectedDisappearanceProbe,
    ProjectedDisappearanceTerminal (..),
    projectedDisappearanceHeader,
    projectedDisappearanceReports,
    projectedDisappearanceTerminal,
    projectedDisappearanceProbes,
    oracleViewDisappearanceProbe,
    oracleProjectionControlledDisappearanceAt,
    oracleProjectionRegularRetirementAt,
    TopologyControlCheckpointProblem (..),
    topologyControlCheckpointAt,
    appliedEntryEvidence,
    retainedControlSuffixAfter,
    appliedEntryForRequest,
    projectedOracleStateDigest,
    AppliedEntryClassification (..),
    classifyAppliedEntry,
    OracleProjectionFault (..),
    PreparedAppliedEntry,
    prepareAppliedEntry,
    preparedAppliedEntryClassification,
    commitAppliedEntry,
    MembershipAdvanceProcessEnd,
    membershipAdvanceProcessEnd,
    membershipAdvanceProcessEndEpoch,
    membershipAdvanceProcessEndControlIndex,
    membershipAdvanceProcessEndReason,
    MembershipAdvanceDisposition (..),
    MembershipAdvanceProblem (..),
    PreparedMembershipAdvance,
    prepareMembershipAdvance,
    preparedMembershipAdvanceDisposition,
    preparedMembershipAdvanceSuccessor,
    preparedMembershipAdvanceProcessEnds,
    commitMembershipAdvance,
    OracleProjectionInvariant (..),
    OracleProjectionStateWitness,
    stateWitness,
    validateState,
    validateStateFromGenesis,
    advanceReplayBase,
    replayBaseControlIndex,
    reclaimAppliedPrefixThroughBase,
    reclaimCurrentAppliedPrefix,
    projectionCanonicalCoveredThrough,
    projectionLastSemanticControlIndex,
    ProjectionIdentityBase,
    ProjectionIdentityBaseProblem (..),
    captureProjectionIdentityBase,
    encodeProjectionIdentityBase,
    decodeProjectionIdentityBase,
    ProjectionLabelBase,
    captureProjectionLabelBase,
    encodeProjectionLabelBase,
    decodeProjectionLabelBase,
    ProjectionDisappearanceBase,
    captureProjectionDisappearanceBase,
    encodeProjectionDisappearanceBase,
    decodeProjectionDisappearanceBase,
    ProjectionVoterFailureBase,
    captureProjectionVoterFailureBase,
    encodeProjectionVoterFailureBase,
    decodeProjectionVoterFailureBase,
    ProjectionBaseCodecProblem (..),
    encodeProjectionBase,
    decodeProjectionBase,
    ProjectionBaseCapture,
    captureProjectionBase,
    captureProjectionBaseWithDiagnostics,
    projectionBaseControlIndex,
    projectionBaseIdentity,
    projectionBaseAppliedEntries,
    projectionBaseCanonicalCoveredThrough,
    projectionBaseHeraldMembershipHistory,
    projectionBaseCurrentHeraldMembershipId,
    projectionBaseLabelWorkflows,
    projectionBaseLastSemanticControlIndex,
    ProjectionBaseInstallProblem (..),
    installProjectionBase,
    projectionWitnessControlIndex,
    projectionWitnessAppliedEntries,
    projectionWitnessStartedProcesses,
    projectionWitnessEndedProcesses,
    projectionWitnessPostStateDigest,
    replaceCurrentHeraldMembershipForInvariantTest,
    OracleGenesisParityFault (..),
    OracleGenesisParityWitness,
    validateOracleGenesisParity,
    parityWitnessSystemId,
    parityWitnessActiveHeraldEpochs,
    parityWitnessInitialProjectionDigest,
  )
where

import Control.Monad (foldM, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.List (find, sort, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize qualified as Codec
import Data.Serialize.Put qualified as Serialize
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceClaim,
    DisappearanceOpenResultView (..),
    DisappearanceProbeId,
    DisappearanceResolutionOutcome,
    DisappearanceResolutionOutcomeView (..),
    DisappearanceSubjectView (..),
    deriveDisappearanceProbeId,
    disappearanceEvidenceClaimCoordinate,
    disappearanceEvidenceClaimProbeId,
    disappearanceEvidenceClaimReporter,
    disappearanceOpenResultView,
    disappearanceProbeIdBytes,
    disappearanceProbeOpenControlIndex,
    disappearanceResolutionControlIndex,
    disappearanceResolutionOutcomeCanonicalBytes,
    disappearanceResolutionOutcomeView,
    disappearanceSubjectMembershipCoordinate,
    disappearanceSubjectView,
    resolveDisappearanceSubject,
  )
import Eclips.Domain.Disappearance qualified as Disappearance
import Eclips.Domain.Identity
  ( ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    ProcessEpochId,
    ProcessId,
    SortId,
    SystemId,
    controlIndex,
    controlIndexWord64,
    globalObjectIdBytes,
    heraldEpochBytes,
    labelDecisionIdBytes,
    processEpochIdBytes,
    processIdBytes,
  )
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Label
  ( LabelOutcomeDigest,
    PreparedLabelDigest,
    PreparedLabelFacts,
    ReleasedLabelState,
    ReleasedLabelStateView (..),
    preparedLabelFactsObjectId,
    preparedLabelFactsProposedOutcome,
    releasedLabelStateView,
  )
import Eclips.Domain.Label qualified as Label
import Eclips.Domain.Membership
  ( FailureProbeResolutionId,
    HeraldFailureProbeId,
    HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    HeraldMembershipHistory,
    HeraldMembershipLineage,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    heraldMembershipGenerationPredecessor,
    heraldMembershipGenerationRetiredHeraldEpoch,
    heraldMembershipGenerationRetirementControlIndex,
    heraldMembershipGenerationRetirementId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason,
    processEndReasonCanonicalBytes,
    processEndReasonIsHeraldRetirement,
  )
import Eclips.Domain.ProcessLifecycle qualified as Lifecycle
import Eclips.Domain.ProcessStart (ProcessStart, processStart, processStartProcessEpochId, processStartProcessId, processStartResidence)
import Eclips.Domain.Sort.Canonical (descriptorSortId)
import Eclips.Domain.Sort.Profile
  ( predefinedCatalogueDescriptor,
    profileCatalogueDigest,
    profilePredefinedCatalogue,
  )
import Eclips.Domain.SortOccurrence qualified as SortOccurrence
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    CatalogueDigest,
    CheckedInitialTopologyProjection,
    ConfigurationDigest,
    HeraldMember (..),
    InitialProjectionDigest,
    PredefinedOccurrenceSet,
    appliedProcessEpochId,
    appliedProcessId,
    appliedProcessResidence,
    deriveInitialProjectionDigest,
    genesisPredefinedOccurrenceSet,
  )
import Eclips.Domain.Startup qualified as Startup
import Eclips.Domain.Value
  ( canonicalValueByteString,
    canonicalValueBytes,
    labelValue,
  )
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks (..), runDiagnosticCheck)
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    checkedActiveHeralds,
    checkedCatalogueDigest,
    checkedConfigurationDigest,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedInitialTopologyProjection,
    checkedLocalHeraldEpoch,
    checkedOracleControlIndex,
    checkedPredefinedOccurrenceSet,
    checkedStartupAdmission,
    checkedSystemId,
  )
import Eclips.Oracle.Admission qualified as Admission
import Eclips.Oracle.Canonical
  ( CanonicalAppliedOracleEntry,
    canonicalAppliedOracleEntryBytes,
    canonicalAppliedOracleEntryValue,
    canonicalizeAppliedOracleEntry,
    decodeCanonicalAppliedOracleEntry,
  )
import Eclips.Oracle.Command (ProbeResult (..))
import Eclips.Oracle.Disappearance
  ( DisappearanceProbeAbortReasonView (..),
    DisappearanceProbeInvalidationView (..),
    ProjectedDisappearanceProbeHeader,
    projectedDisappearanceProbeHeaderCoordinate,
    projectedDisappearanceProbeHeaderId,
    projectedDisappearanceProbeHeaderMembership,
    projectedDisappearanceProbeHeaderSubject,
  )
import Eclips.Oracle.Disappearance qualified as OracleDisappearance
import Eclips.Oracle.Failure qualified as Failure
import Eclips.Oracle.Genesis (CheckedOracleGenesis)
import Eclips.Oracle.Genesis qualified as OracleGenesis
import Eclips.Oracle.Identity
  ( OracleClientRequestId,
    OracleCommandDigest,
    OracleStateDigest,
    oracleClientRequestHome,
  )
import Eclips.Oracle.Identity qualified as OracleIdentity
import Eclips.Oracle.Label
  ( LiveLabelDecision,
    LiveTerminalOutcome,
    LiveTerminalOutcomeView (..),
    ProcessEpochRecord,
    ProcessLifecycleView (..),
    ProcessOriginView (..),
    ReleasedLabelOverlay,
    liveDecisionId,
    liveDecisionMemberSetDigest,
    liveDecisionMembershipGeneration,
    liveDecisionObject,
    liveTerminalOutcomeView,
    processRecordLifecycle,
    processRecordOrigin,
    processRecordProcessEpoch,
    processRecordProcessId,
    processRecordResidence,
  )
import Eclips.Oracle.Label qualified as Live
import Eclips.Oracle.Projection
  ( AppliedOracleCommandEntry,
    AppliedOracleEntry,
    OracleProjectionEvent,
    OracleProjectionEventView (..),
    appliedEntryCommand,
    appliedEntryCommandDigest,
    appliedEntryConfiguration,
    appliedEntryControlIndex,
    appliedEntryOracleProgress,
    appliedEntryPostStateDigest,
    appliedEntryProjectionEvents,
    appliedEntryReceipt,
    appliedEntryReceiptRetirement,
    appliedEntryRequestId,
    oracleProjectionEventView,
  )
import Eclips.Oracle.Receipt
  ( OracleReceiptResult (..),
    oracleReceiptCommandDigest,
    oracleReceiptControlIndex,
    oracleReceiptRequestId,
    oracleReceiptResult,
  )
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Raft.Identity (RaftNodeId)

-- | Greatest applied prefix plus immutable genesis facts and exact watch
-- evidence. Every field is private to the serialized projection owner.
data State = State
  { systemId :: SystemId,
    catalogueDigest :: CatalogueDigest,
    configurationDigest :: ConfigurationDigest,
    predefinedOccurrences :: PredefinedOccurrenceSet,
    initialTopology :: CheckedInitialTopologyProjection,
    controlIndex :: ControlIndex,
    localHeraldEpoch :: HeraldEpoch,
    localStartupAdmission :: Maybe Admission.HeraldAdmissionRecord,
    heraldCatalogue :: Map HeraldEpoch HeraldMember,
    genesisHeraldCatalogue :: Map HeraldEpoch HeraldMember,
    heraldAdmissions :: Map Membership.HeraldAdmissionId Admission.HeraldAdmissionRecord,
    genesisHeraldMembership :: HeraldMembershipGeneration,
    currentHeraldMembership :: HeraldMembershipGeneration,
    heraldMembershipHistory :: HeraldMembershipHistory,
    membershipAdvances :: Map ControlIndex MembershipAdvanceEvidence,
    initialProjectionDigest :: InitialProjectionDigest,
    genesisProcesses :: Map ProcessEpochId AppliedProcessBootstrap,
    startedProcesses :: Map ProcessEpochId ProjectedStartedProcess,
    endedProcesses :: Map ProcessEpochId ProjectedEndedProcess,
    labelWorkflows :: Map LabelDecisionId ProjectedLabelWorkflow,
    completedLabelWorkflows :: Map LabelDecisionId LabelOutcomeDigest,
    failureProbes :: Map HeraldFailureProbeId ProjectedFailureProbe,
    disappearanceProbes :: Map DisappearanceProbeId ProjectedDisappearanceProbe,
    initialVoterConfiguration :: Maybe Voter.VoterConfiguration,
    voterConfiguration :: Maybe Voter.VoterConfiguration,
    initialOracleReplicas :: Map RaftNodeId Voter.OracleReplicaRegistration,
    oracleReplicas :: Map RaftNodeId Voter.OracleReplicaRegistration,
    voterChanges :: Map Voter.VoterChangeId Voter.VoterChange,
    pendingVoterChange :: Maybe Voter.VoterChange,
    appliedEntries :: !(Map ControlIndex CanonicalAppliedOracleEntry),
    replayBase :: !(Maybe ReplayBase),
    canonicalEvidenceCoveredThrough :: !ControlIndex,
    lastSemanticControlIndex :: !ControlIndex,
    latestCanonicalStateDigest :: !(Maybe OracleStateDigest)
  }
  deriving stock (Eq)

-- | Checked replay origin owned by this projection. This deliberately contains
-- neither a State nor a previous base nor either exact-evidence archive: replacing
-- it cannot retain a chain of prior replay origins. Historical semantic facts are
-- still retained until their individual consumers acquire release contracts.
-- It carries no owner identity and is also the semantic payload of a checked
-- in-memory projection-base capture. It does not authorize evidence reclamation.
data ReplayBase = ReplayBase
  { baseControlIndex :: !(ControlIndex),
    baseHeraldCatalogue :: !(Map HeraldEpoch HeraldMember),
    baseHeraldAdmissions :: !(Map Membership.HeraldAdmissionId Admission.HeraldAdmissionRecord),
    baseCurrentHeraldMembership :: !(HeraldMembershipGeneration),
    baseHeraldMembershipHistory :: !(HeraldMembershipHistory),
    baseStartedProcesses :: !(Map ProcessEpochId ProjectedStartedProcess),
    baseEndedProcesses :: !(Map ProcessEpochId ProjectedEndedProcess),
    baseLabelWorkflows :: !(Map LabelDecisionId ProjectedLabelWorkflow),
    baseCompletedLabelWorkflows :: !(Map LabelDecisionId LabelOutcomeDigest),
    baseFailureProbes :: !(Map HeraldFailureProbeId ProjectedFailureProbe),
    baseDisappearanceProbes :: !(Map DisappearanceProbeId ProjectedDisappearanceProbe),
    baseVoterConfiguration :: !(Maybe Voter.VoterConfiguration),
    baseOracleReplicas :: !(Map RaftNodeId Voter.OracleReplicaRegistration),
    baseVoterChanges :: !(Map Voter.VoterChangeId Voter.VoterChange),
    basePendingVoterChange :: !(Maybe Voter.VoterChange),
    baseLastSemanticControlIndex :: !ControlIndex,
    baseLatestCanonicalStateDigest :: !(Maybe OracleStateDigest)
  }
  deriving stock (Eq)

-- | Immutable provenance shared by projections from the same checked genesis.
-- The configuration digest binds the initial Herald catalogue; the initial
-- projection digest binds the initial process and topology facts. Receiver-local
-- identity and startup admission deliberately do not occur here.
data ProjectionBaseGenesis
  = ProjectionBaseGenesis
      !SystemId
      !CatalogueDigest
      !ConfigurationDigest
      !InitialProjectionDigest
      !HeraldMembershipGeneration
      !(Maybe Voter.VoterConfiguration)
      !(Map RaftNodeId Voter.OracleReplicaRegistration)
  deriving stock (Eq)

-- | Checked semantic payload, independent of the source owner and its previous
-- replay base. Historical semantic maps remain present in this first increment.
data CheckedProjectionBase = CheckedProjectionBase !ProjectionBaseGenesis !ReplayBase
  deriving stock (Eq)

-- Exact overlap evidence above the covered floor and explicitly pinned older
-- entries travel separately from semantic facts. Detached membership evidence
-- remains exact at every coordinate.
data ProjectionBaseArchive
  = ProjectionBaseArchive
      !(Map ControlIndex CanonicalAppliedOracleEntry)
      !(Map ControlIndex MembershipAdvanceEvidence)
      !ControlIndex
  deriving stock (Eq)

-- | Portable membership and process facts. The catalogue and current generation
-- are derived rather than duplicated on the wire. This is one admitted semantic
-- leaf, not an installed Projection or authority to discard canonical evidence.
data ProjectionIdentityBase
  = ProjectionIdentityBase
      !(SystemId, CatalogueDigest, ConfigurationDigest, InitialProjectionDigest)
      !ControlIndex
      !HeraldMembershipHistory
      !(Map Membership.HeraldAdmissionId Admission.HeraldAdmissionRecord)
      !(Map ProcessEpochId ProjectedStartedProcess)
      !(Map ProcessEpochId ProjectedEndedProcess)
  deriving stock (Eq, Show)

data ProjectionIdentityBaseProblem
  = ProjectionIdentityBaseMalformed String
  | ProjectionIdentityBaseNoncanonical
  | ProjectionIdentityBaseGenesisMismatch
  | ProjectionIdentityBaseMembershipMismatch
  | ProjectionIdentityBaseAdmissionMismatch
  | ProjectionIdentityBaseProcessMismatch
  deriving stock (Eq, Show)

type ProjectionIdentityTranscript =
  ( ByteString,
    (ByteString, ByteString, ByteString, ByteString),
    Word64,
    ByteString,
    [ByteString],
    [(ByteString, ByteString, ByteString, Word64, Word64)],
    [(ByteString, Word64, ByteString)]
  )

captureProjectionIdentityBase :: State -> Either ProjectionIdentityBaseProblem ProjectionIdentityBase
captureProjectionIdentityBase state = do
  let base = ProjectionIdentityBase (identityBaseGenesis state) state.controlIndex state.heraldMembershipHistory state.heraldAdmissions state.startedProcesses state.endedProcesses
  catalogue <- admitIdentityBase state base
  unless
    (catalogue == state.heraldCatalogue && Membership.heraldMembershipHistoryCurrent state.heraldMembershipHistory == state.currentHeraldMembership)
    (Left ProjectionIdentityBaseMembershipMismatch)
  pure base

encodeProjectionIdentityBase :: ProjectionIdentityBase -> ByteString
encodeProjectionIdentityBase (ProjectionIdentityBase identity index history admissions started ended) = Codec.encode transcript
  where
    transcript :: ProjectionIdentityTranscript
    transcript =
      ( "ECLIPS-PROJECTION-IDENTITY-BASE",
        identityBaseGenesisBytes identity,
        controlIndexWord64 index,
        Membership.heraldMembershipHistoryCanonicalBytes history,
        map Admission.encodeAdmissionRecord (Map.elems admissions),
        [ (processEpochIdBytes epoch, processIdBytes (processStartProcessId start), heraldEpochBytes (processStartResidence start), controlIndexWord64 at, OracleIdentity.oracleClientRequestSequence request)
        | (epoch, ProjectedStartedProcess start at request) <- Map.toAscList started
        ],
        [(processEpochIdBytes epoch, controlIndexWord64 at, processEndReasonCanonicalBytes reason) | (epoch, ProjectedEndedProcess _ at reason) <- Map.toAscList ended]
      )

-- | Decode against receiver-owned immutable genesis, then check relationships
-- between membership, admission and process facts independently of replay. The
-- enclosing base must bind this leaf's control cut to its other semantic leaves.
-- Every retained nested frame is detached from the enclosing transfer buffer.
decodeProjectionIdentityBase ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  ByteString ->
  Either ProjectionIdentityBaseProblem ProjectionIdentityBase
decodeProjectionIdentityBase genesis bootstraps = decodeProjectionIdentityBaseFor (initialState genesis bootstraps)

decodeProjectionIdentityBaseFor :: State -> ByteString -> Either ProjectionIdentityBaseProblem ProjectionIdentityBase
decodeProjectionIdentityBaseFor initial bytes = do
  (domain, identity, rawIndex, rawHistory, rawAdmissions, rawStarted, rawEnded) <- identityBaseClaim (Codec.decode bytes :: Either String ProjectionIdentityTranscript)
  unless (domain == "ECLIPS-PROJECTION-IDENTITY-BASE") (Left (ProjectionIdentityBaseMalformed "wrong domain"))
  let checkedIdentity = identityBaseGenesis initial
  unless (identity == identityBaseGenesisBytes checkedIdentity) (Left ProjectionIdentityBaseGenesisMismatch)
  history <- identityBaseClaim (Membership.decodeHeraldMembershipHistoryCanonicalBytes (ByteString.copy rawHistory))
  admissions <- traverse (identityBaseClaim . Admission.decodeAdmissionRecord . ByteString.copy) rawAdmissions
  started <- traverse readStarted rawStarted
  ended <- traverse readEnded rawEnded
  let base = ProjectionIdentityBase checkedIdentity (controlIndex rawIndex) history (Map.fromList [(Admission.admissionRecordId record, record) | record <- admissions]) (Map.fromList started) (Map.fromList ended)
  _ <- admitIdentityBase initial base
  unless (encodeProjectionIdentityBase base == bytes) (Left ProjectionIdentityBaseNoncanonical)
  pure base
  where
    readStarted (rawEpoch, rawProcess, rawResidence, at, sequenceNumber) = do
      epoch <- identityBaseClaim (Identity.mkProcessEpochId (ByteString.copy rawEpoch))
      process <- identityBaseClaim (Identity.mkProcessId (ByteString.copy rawProcess))
      residence <- identityBaseClaim (Identity.mkHeraldEpoch (ByteString.copy rawResidence))
      pure (epoch, ProjectedStartedProcess (processStart process epoch residence) (controlIndex at) (OracleIdentity.oracleClientRequestId residence sequenceNumber))
    readEnded (rawEpoch, at, rawReason) = do
      epoch <- identityBaseClaim (Identity.mkProcessEpochId (ByteString.copy rawEpoch))
      reason <- identityBaseClaim (Lifecycle.decodeProcessEndReasonCanonicalBytes (ByteString.copy rawReason))
      pure (epoch, ProjectedEndedProcess epoch (controlIndex at) reason)

identityBaseClaim :: (Show problem) => Either problem value -> Either ProjectionIdentityBaseProblem value
identityBaseClaim = either (Left . ProjectionIdentityBaseMalformed . show) Right

identityBaseGenesis :: State -> (SystemId, CatalogueDigest, ConfigurationDigest, InitialProjectionDigest)
identityBaseGenesis state = (state.systemId, state.catalogueDigest, state.configurationDigest, state.initialProjectionDigest)

identityBaseGenesisBytes :: (SystemId, CatalogueDigest, ConfigurationDigest, InitialProjectionDigest) -> (ByteString, ByteString, ByteString, ByteString)
identityBaseGenesisBytes (system, catalogue, configuration, initial) =
  (Identity.systemIdBytes system, Startup.catalogueDigestBytes catalogue, Startup.configurationDigestBytes configuration, Startup.initialProjectionDigestBytes initial)

admitIdentityBase :: State -> ProjectionIdentityBase -> Either ProjectionIdentityBaseProblem (Map HeraldEpoch HeraldMember)
admitIdentityBase initial (ProjectionIdentityBase identity cursor history admissions started ended) = do
  unless
    ( identity == identityBaseGenesis initial
        && Membership.heraldMembershipHistoryGenesis history == initial.genesisHeraldMembership
        && initial.initialProjectionDigest == deriveInitialProjectionDigest (Map.elems initial.genesisProcesses) initial.initialTopology
    )
    (Left ProjectionIdentityBaseGenesisMismatch)
  unless (all (maybe True (<= cursor) . Membership.heraldMembershipGenerationChangeControlIndex) generations) membershipProblem
  let newMembers = [(Admission.admissionManifestHeraldEpoch manifest, HeraldMember (Admission.admissionManifestHeraldId manifest) (Admission.admissionManifestHeraldEpoch manifest)) | record <- Map.elems admissions, let manifest = Admission.admissionRecordManifest record]
      newCatalogue = Map.fromList newMembers
      catalogue = initial.genesisHeraldCatalogue `Map.union` newCatalogue
  unless
    (Map.size newCatalogue == length newMembers && Set.null (Map.keysSet initial.genesisHeraldCatalogue `Set.intersection` Map.keysSet newCatalogue) && all (all (`Map.member` catalogue) . NonEmpty.toList . heraldMembershipGenerationActiveHeraldEpochs) generations)
    admissionProblem
  mapM_ (checkAdmission catalogue) (Map.toAscList admissions)
  let orderedAdmissions = sortOn Admission.admissionRecordBeginIndex (Map.elems admissions)
  unless (all (\(before, after) -> not (pending before) && Admission.admissionRecordChangedIndex before < Admission.admissionRecordBeginIndex after) (zip orderedAdmissions (drop 1 orderedAdmissions))) admissionProblem
  unless (length [() | record <- Map.elems admissions, pending record] <= 1) admissionProblem
  mapM_ checkAdmittedGeneration generations
  let allProcessIds = map appliedProcessId (Map.elems initial.genesisProcesses) <> map (processStartProcessId . projectedStartedProcessBootstrap) (Map.elems started)
  unless (Set.size (Set.fromList allProcessIds) == length allProcessIds && Set.null (Map.keysSet initial.genesisProcesses `Set.intersection` Map.keysSet started)) processProblem
  let primaryCoordinates = map projectedStartedProcessControlIndex (Map.elems started) <> [projectedEndedProcessControlIndex process | process <- Map.elems ended, not (processEndReasonIsHeraldRetirement (projectedEndedProcessReason process))] <> map Admission.admissionRecordBeginIndex (Map.elems admissions) <> [at | generation <- generations, Just at <- [Membership.heraldMembershipGenerationChangeControlIndex generation]]
      startRequests = map projectedStartedProcessRequest (Map.elems started)
  unless (Set.size (Set.fromList primaryCoordinates) == length primaryCoordinates && Set.size (Set.fromList startRequests) == length startRequests) processProblem
  mapM_ checkStarted (Map.toAscList started)
  mapM_ checkEnded (Map.toAscList ended)
  mapM_ checkRetirement [(epoch, residence, born) | (epoch, (residence, born)) <- Map.toAscList processLifetimes]
  pure catalogue
  where
    generations = NonEmpty.toList (Membership.heraldMembershipHistoryGenerations history)
    current = Membership.heraldMembershipHistoryCurrent history
    membershipProblem = Left ProjectionIdentityBaseMembershipMismatch
    admissionProblem = Left ProjectionIdentityBaseAdmissionMismatch
    processProblem = Left ProjectionIdentityBaseProcessMismatch
    knownGeneration generation = Membership.lookupHeraldMembershipGeneration (heraldMembershipGenerationId generation) history == Just generation
    pending record = case Admission.admissionRecordPhase record of
      Admission.AdmissionActivated {} -> False
      Admission.AdmissionCancelled {} -> False
      _ -> True
    checkAdmission catalogue (key, record) = do
      let predecessor = Admission.admissionRecordPredecessor record
          manifest = Admission.admissionRecordManifest record
          begin = Admission.admissionRecordBeginIndex record
          changed = Admission.admissionRecordChangedIndex record
          active = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (membershipBefore begin))
      unless
        ( key == Admission.admissionRecordId record
            && Admission.admissionManifestSystem (Admission.admissionRecordManifest record) == initial.systemId
            && Admission.admissionRecordChangedIndex record <= cursor
            && (Admission.admissionRecordAttempt record == 1) == (Admission.admissionRecordSemanticToken record == begin)
            && knownGeneration predecessor
            && predecessor == membershipBefore begin
            && predecessor == membershipBefore changed
            && all (\epoch -> maybe False ((/= Admission.admissionManifestHeraldId manifest) . heraldMemberId) (Map.lookup epoch catalogue)) active
            && maybe True ((< changed) . Admission.joinSealControlPrefix) (Admission.admissionRecordSeal record)
            && all ((< changed) . Admission.joinReadyControlPrefix) (Admission.admissionRecordBaseReports record <> maybe [] pure (Admission.admissionRecordNewcomerReport record))
            && (not (pending record) || Admission.admissionRecordPredecessor record == current)
        )
        admissionProblem
      case Admission.admissionRecordPhase record of
        Admission.AdmissionActivated _ successor -> unless (knownGeneration successor) admissionProblem
        Admission.AdmissionCancelled at -> unless (at > begin) admissionProblem
        _ -> pure ()
    checkAdmittedGeneration generation = case Membership.heraldMembershipGenerationAdmissionId generation of
      Nothing -> pure ()
      Just admission -> case Map.lookup admission admissions of
        Just record | Admission.AdmissionActivated index successor <- Admission.admissionRecordPhase record, successor == generation, Just index == Membership.heraldMembershipGenerationChangeControlIndex generation -> pure ()
        _ -> admissionProblem
    membershipAt index = foldl select initial.genesisHeraldMembership generations
      where
        select before generation = case Membership.heraldMembershipGenerationChangeControlIndex generation of
          Just at | at > index -> before
          _ -> generation
    membershipBefore index = foldl select initial.genesisHeraldMembership generations
      where
        select before generation = case Membership.heraldMembershipGenerationChangeControlIndex generation of
          Just at | at >= index -> before
          _ -> generation
    memberAt epoch index = epoch `elem` NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (membershipAt index))
    ordinaryIndex index = all ((/= Just index) . Membership.heraldMembershipGenerationChangeControlIndex) generations
    checkStarted (epoch, ProjectedStartedProcess start index request) =
      unless
        (epoch == processStartProcessEpochId start && index > controlIndex 0 && index <= cursor && ordinaryIndex index && OracleIdentity.oracleClientRequestHome request == processStartResidence start && memberAt (processStartResidence start) index)
        processProblem
    processLifetimes =
      Map.map (\bootstrap -> (appliedProcessResidence bootstrap, controlIndex 0)) initial.genesisProcesses
        `Map.union` Map.map (\(ProjectedStartedProcess start at _) -> (processStartResidence start, at)) started
    checkEnded (epoch, ProjectedEndedProcess embedded index reason) = case Map.lookup epoch processLifetimes of
      Nothing -> processProblem
      Just (residence, born) -> do
        unless (embedded == epoch && index > born && index <= cursor) processProblem
        if processEndReasonIsHeraldRetirement reason
          then unless (any (\generation -> heraldMembershipGenerationRetiredHeraldEpoch generation == Just residence && heraldMembershipGenerationRetirementControlIndex generation == Just index) generations) processProblem
          else unless (ordinaryIndex index && memberAt residence index) processProblem
    checkRetirement (epoch, residence, born) = mapM_ check generations
      where
        check generation = case (heraldMembershipGenerationRetiredHeraldEpoch generation, heraldMembershipGenerationRetirementControlIndex generation) of
          (Just retired, Just at)
            | retired == residence && born < at ->
                unless (maybe False ((<= at) . projectedEndedProcessControlIndex) (Map.lookup epoch ended)) processProblem
          _ -> pure ()

-- | An in-memory semantic base paired with its retained exact archives. The
-- opaque pairing prevents installing another capture's archive at the same cut.
-- Neither component contains a State, donor-local identity, or a previous base.
data ProjectionBaseCapture = ProjectionBaseCapture !CheckedProjectionBase !ProjectionBaseArchive
  deriving stock (Eq)

-- | Immutable label outcomes at an admitted control cut. A NotApplied outcome
-- keeps its already-canonical digest; its discarded reason is not reconstructed.
-- Released overlays and digests derive from the retained checked facts.
data ProjectionLabelBase
  = ProjectionLabelBase
      !(SystemId, CatalogueDigest, ConfigurationDigest, InitialProjectionDigest)
      !ControlIndex
      !(Map LabelDecisionId ProjectedLabelWorkflow)
      !(Map LabelDecisionId LabelOutcomeDigest)
  deriving stock (Eq, Show)

type ProjectionLabelTranscript =
  ( ByteString,
    (ByteString, ByteString, ByteString, ByteString),
    Word64,
    [(ByteString, Either ByteString (ByteString, Bool))]
  )

captureProjectionLabelBase :: State -> Either ProjectionBaseCodecProblem ProjectionLabelBase
captureProjectionLabelBase state = do
  workflows <- traverse normalizeProjectionLabelWorkflow state.labelWorkflows
  admitProjectionLabelWorkflows state workflows
  let completed = projectionLabelCompletions workflows
  unless (completed == state.completedLabelWorkflows) (Left (ProjectionBaseCodecInconsistent "label completion map differs from workflow phases"))
  pure (ProjectionLabelBase (identityBaseGenesis state) state.controlIndex workflows completed)

encodeProjectionLabelBase :: ProjectionLabelBase -> ByteString
encodeProjectionLabelBase (ProjectionLabelBase identity index workflows _) = Codec.encode transcript
  where
    transcript :: ProjectionLabelTranscript
    transcript =
      ( "ECLIPS-PROJECTION-LABEL-BASE",
        identityBaseGenesisBytes identity,
        controlIndexWord64 index,
        map encodeWorkflow (Map.elems workflows)
      )
    encodeWorkflow workflow = (Live.encodeLabelDecision workflow.decision, outcome)
      where
        outcome = case (workflow.preparedFacts, workflow.terminal) of
          (Nothing, Just (ProjectedLabelNotApplied _ digest)) -> Left (Label.labelOutcomeDigestBytes digest)
          (Just facts, Just ProjectedLabelReleased {}) -> Right (Label.preparedLabelFactsCanonicalBytes facts, workflow.phase == LabelWorkflowCompleted)
          _ -> error "checked Projection label base has incomplete outcome"

-- | The supplied owner contributes only admitted identity, process lifetimes,
-- membership and cut. Its label maps are not consulted, including for old
-- NotApplied outcomes whose exact reason no longer belongs to the projection.
decodeProjectionLabelBase :: State -> ByteString -> Either ProjectionBaseCodecProblem ProjectionLabelBase
decodeProjectionLabelBase context bytes = do
  (domain, identity, rawIndex, rows) <- projectionBaseClaim (Codec.decode bytes :: Either String ProjectionLabelTranscript)
  unless (domain == "ECLIPS-PROJECTION-LABEL-BASE") (Left (ProjectionBaseCodecMalformed "wrong label base domain"))
  unless (identity == identityBaseGenesisBytes (identityBaseGenesis context) && controlIndex rawIndex == context.controlIndex) (Left (ProjectionBaseCodecInconsistent "label base identity or cut mismatch"))
  decoded <- traverse readWorkflow rows
  let workflows = Map.fromList [(liveDecisionId workflow.decision, workflow) | workflow <- decoded]
      base = ProjectionLabelBase (identityBaseGenesis context) context.controlIndex workflows (projectionLabelCompletions workflows)
  admitProjectionLabelWorkflows context workflows
  unless (encodeProjectionLabelBase base == bytes) (Left (ProjectionBaseCodecMalformed "noncanonical label base"))
  pure base
  where
    readWorkflow (decisionBytes, outcome) = do
      decision <- projectionBaseClaim (Live.decodeLabelDecision decisionBytes)
      case outcome of
        Left rawDigest -> do
          digest <- projectionBaseClaim (Label.mkLabelOutcomeDigest (ByteString.copy rawDigest))
          pure (ProjectedLabelWorkflow decision LabelWorkflowCompleted Nothing Nothing (Just (ProjectedLabelNotApplied (Live.liveDecisionRequestControlIndex decision) digest)))
        Right (rawFacts, completed) -> do
          facts <- projectionBaseClaim (Label.decodePreparedLabelFactsCanonicalBytes (ByteString.copy rawFacts))
          let terminal = Live.liveReleasedTerminalOutcomeFromFacts facts
              workflow = decidedWorkflow decision terminal (Live.deriveLabelOutcomeDigest terminal)
          pure workflow {phase = if completed then LabelWorkflowCompleted else LabelWorkflowReleased}

-- Capture and wire admission share the same source-independent field relation.
-- No old predecessor workflow or canonical entry is needed to derive an overlay.
normalizeProjectionLabelWorkflow :: ProjectedLabelWorkflow -> Either ProjectionBaseCodecProblem ProjectedLabelWorkflow
normalizeProjectionLabelWorkflow supplied = do
  normalized <- case (supplied.preparedFacts, supplied.terminal) of
    (Nothing, Just (ProjectedLabelNotApplied index digest)) ->
      pure (ProjectedLabelWorkflow supplied.decision LabelWorkflowCompleted Nothing Nothing (Just (ProjectedLabelNotApplied index digest)))
    (Just facts, Just ProjectedLabelReleased {}) -> do
      let terminal = Live.liveReleasedTerminalOutcomeFromFacts facts
      pure (decidedWorkflow supplied.decision terminal (Live.deriveLabelOutcomeDigest terminal)) {phase = supplied.phase}
    _ -> Left (ProjectionBaseCodecInconsistent "incomplete projected label outcome")
  unless (normalized == supplied) (Left (ProjectionBaseCodecInconsistent "label outcome differs from its checked facts"))
  pure normalized

projectionLabelCompletions :: Map LabelDecisionId ProjectedLabelWorkflow -> Map LabelDecisionId LabelOutcomeDigest
projectionLabelCompletions = Map.mapMaybe completed
  where
    completed workflow
      | workflow.phase /= LabelWorkflowCompleted = Nothing
      | otherwise = case workflow.terminal of
          Just (ProjectedLabelNotApplied _ digest) -> Just digest
          Just (ProjectedLabelReleased _ _ digest) -> Just digest
          Nothing -> Nothing

admitProjectionLabelWorkflows :: State -> Map LabelDecisionId ProjectedLabelWorkflow -> Either ProjectionBaseCodecProblem ()
admitProjectionLabelWorkflows context workflows = do
  mapM_ checkWorkflow (Map.toAscList workflows)
  require (distinct (map (Live.liveDecisionRequestControlIndex . (.decision)) values)) "label decisions share a canonical coordinate"
  require (distinct (map (Live.liveDecisionOpenRequestId . (.decision)) values)) "label decisions reuse a request identity"
  require (distinct (map (liveDecisionObject . (.decision)) pending)) "concurrent pending labels share an object"
  mapM_ checkPending pending
  where
    values = Map.elems workflows
    pending = filter ((== LabelWorkflowReleased) . (.phase)) values
    generations = NonEmpty.toList (Membership.heraldMembershipHistoryGenerations context.heraldMembershipHistory)
    currentMembers = Set.fromList (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs context.currentHeraldMembership))
    latest = Map.fromListWith max [(liveDecisionObject workflow.decision, Live.liveDecisionRequestControlIndex workflow.decision) | workflow <- values]
    distinct entries = Set.size (Set.fromList entries) == length entries
    require condition message = unless condition (Left (ProjectionBaseCodecInconsistent message))
    membershipAt index = foldl' select context.genesisHeraldMembership generations
      where
        select previous generation = case Membership.heraldMembershipGenerationChangeControlIndex generation of
          Just at | at > index -> previous
          _ -> generation
    checkWorkflow (ident, workflow) = do
      _ <- normalizeProjectionLabelWorkflow workflow
      let decision = workflow.decision
          index = Live.liveDecisionRequestControlIndex decision
          membership = membershipAt index
      require (ident == liveDecisionId decision && ident == Live.deriveLabelDecisionId context.systemId (Live.liveDecisionOpenRequestId decision)) "label decision identity differs from its request"
      require (index > controlIndex 0 && index <= context.controlIndex) "label decision is outside the admitted cut"
      require (liveDecisionMembershipGeneration decision == heraldMembershipGenerationId membership && liveDecisionMemberSetDigest decision == heraldMembershipGenerationActiveMemberSetDigest membership && Live.liveDecisionHome decision `elem` NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership)) "label decision captured a different membership"
      require (all ((/= Just index) . Membership.heraldMembershipGenerationChangeControlIndex) generations) "label decision shares a membership transition"
      case workflow.terminal of
        Just (ProjectedLabelNotApplied at _) -> require (at == index) "NotApplied coordinate differs from its decision"
        Just (ProjectedLabelReleased at _ _) -> case workflow.preparedFacts of
          Nothing -> require False "released label lacks preparation facts"
          Just facts -> do
            require
              ( Label.preparedLabelFactsDecisionId facts == ident
                  && Label.preparedLabelFactsResolveIndex facts == index
                  && at == index
                  && preparedLabelFactsObjectId facts == liveDecisionObject decision
                  && Label.preparedLabelFactsExpectedPriorState facts == Label.releasedLabel (Live.liveDecisionExpectedLabel decision)
                  && Label.preparedLabelFactsExpectedPriorRevision facts == Live.liveDecisionExpectedPriorRevision decision
                  && Label.preparedLabelFactsExpectedPriorAuthority facts == Live.liveDecisionExpectedPriorAuthority decision
                  && Label.preparedLabelFactsPriorAuthorityJustification facts == Live.liveDecisionPriorAuthorityJustification decision
                  && Label.preparedLabelFactsCatalogueDigest facts == context.catalogueDigest
                  && Label.preparedLabelFactsMemberSetDigest facts == liveDecisionMemberSetDigest decision
                  && maybe True ((< index) . Label.labelRevisionControlIndex) (Label.preparedLabelFactsExpectedPriorRevision facts)
              )
              "released facts disagree with their decision"
            require (oracleProjectionProcessIsLiveAt index (Live.liveDecisionCaller decision) context) "released label caller was not live"
            case Label.labelTargetView (Live.liveDecisionTarget decision) of
              Label.TargetProcessView process -> require (oracleProjectionProcessIsLiveAt index process context) "released label target was not live"
              _ -> pure ()
            require
              (Label.labelTransition (Live.liveDecisionCaller decision) (Live.liveDecisionExpectedLabel decision) (Label.preparedLabelFactsExpectedPriorState facts) (Live.liveDecisionTarget decision) == Right (Label.LabelTransitionApplied (preparedLabelFactsProposedOutcome facts)))
              "released label violates caller/target transition"
        Nothing -> require False "label workflow has no outcome"
    checkPending workflow = do
      let decision = workflow.decision
          captured = Set.fromList (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (membershipAt (Live.liveDecisionRequestControlIndex decision))))
      require (not (Set.null (captured `Set.intersection` currentMembers))) "pending label has no surviving participant"
      require (Map.lookup (liveDecisionObject decision) latest == Just (Live.liveDecisionRequestControlIndex decision)) "pending label is superseded for its object"

-- | Nominal disappearance facts at the enclosing admitted control cut. The
-- source/cut/Join binding belongs to the aggregate, not this standalone codec.
newtype ProjectionDisappearanceBase
  = ProjectionDisappearanceBase (Map DisappearanceProbeId ProjectedDisappearanceProbe)
  deriving stock (Eq, Show)

type RawProjectionDisappearanceProbe =
  (Word64, ByteString, ByteString, [ByteString], Maybe (Word8, Word64, [ByteString]))

captureProjectionDisappearanceBase :: State -> Either ProjectionBaseCodecProblem ProjectionDisappearanceBase
captureProjectionDisappearanceBase context = do
  let base = ProjectionDisappearanceBase context.disappearanceProbes
  admitProjectionDisappearanceBase context base
  pure base

encodeProjectionDisappearanceBase :: ProjectionDisappearanceBase -> ByteString
encodeProjectionDisappearanceBase (ProjectionDisappearanceBase probes) =
  Codec.encode ("ECLIPS-PROJECTION-DISAPPEARANCE-BASE" :: ByteString, map encodeProbe (Map.elems probes))
  where
    encodeProbe :: ProjectedDisappearanceProbe -> RawProjectionDisappearanceProbe
    encodeProbe (ProjectedDisappearanceProbe header reports terminal) =
      ( controlIndexWord64 (disappearanceProbeOpenControlIndex (projectedDisappearanceProbeHeaderId header)),
        Disappearance.disappearanceSubjectCanonicalBytes (projectedDisappearanceProbeHeaderSubject header),
        Membership.heraldMembershipGenerationIdBytes (heraldMembershipGenerationId (projectedDisappearanceProbeHeaderMembership header)),
        map Disappearance.disappearanceEvidenceClaimCanonicalBytes (Map.elems reports),
        encodeTerminal <$> terminal
      )
    encodeTerminal = \case
      ProjectedDisappearanceInvalidated (CommandDisappearanceInvalidation reporter reason evidence) index ->
        (0, controlIndexWord64 index, [heraldEpochBytes reporter, ByteString.singleton (invalidationTag reason), Disappearance.disappearanceEvidenceDigestBytes evidence])
      ProjectedDisappearanceInvalidated (LabelOpenDisappearanceInvalidation decision) index ->
        (1, controlIndexWord64 index, [labelDecisionIdBytes decision])
      ProjectedDisappearanceResolved outcome index ->
        (2, controlIndexWord64 index, [disappearanceResolutionOutcomeCanonicalBytes outcome])
      ProjectedDisappearanceAborted (ExplicitDisappearanceAbortReasonView _) index ->
        (3, controlIndexWord64 index, [])
      ProjectedDisappearanceAborted (MembershipSupersededDisappearanceAbortView generation) index ->
        (4, controlIndexWord64 index, [Membership.heraldMembershipGenerationIdBytes generation])
      ProjectedDisappearanceAborted (AdmissionPreparingDisappearanceAbortView admission) index ->
        (5, controlIndexWord64 index, [Membership.heraldAdmissionIdCanonicalBytes admission])
    invalidationTag = \case
      OracleDisappearance.MatchingPublicationObserved -> 0
      OracleDisappearance.EvidenceContradicted -> 1

-- | The context supplies admitted identity, membership, label facts and cut.
-- Its old disappearance map is never consulted to authenticate supplied rows.
-- The aggregate still binds this leaf to its admitted donor and Join snapshot.
decodeProjectionDisappearanceBase :: State -> ByteString -> Either ProjectionBaseCodecProblem ProjectionDisappearanceBase
decodeProjectionDisappearanceBase context bytes = do
  (domain, rows) <- projectionBaseClaim (Codec.decode bytes :: Either String (ByteString, [RawProjectionDisappearanceProbe]))
  disappearanceBaseCheck (domain == "ECLIPS-PROJECTION-DISAPPEARANCE-BASE") "wrong disappearance base domain"
  probes <- traverse decodeProbe rows
  let base = ProjectionDisappearanceBase (Map.fromList probes)
  admitProjectionDisappearanceBase context base
  disappearanceBaseCheck (encodeProjectionDisappearanceBase base == bytes) "noncanonical disappearance base"
  pure base
  where
    decodeProbe (rawOpened, rawSubject, rawMembership, rawReports, rawTerminal) = do
      let opened = controlIndex rawOpened
      subject <- projectionBaseClaim (Disappearance.decodeDisappearanceSubjectCanonicalBytes (ByteString.copy rawSubject))
      generation <- projectionBaseClaim (Membership.mkHeraldMembershipGenerationId (ByteString.copy rawMembership))
      membership <- maybe (Left (ProjectionBaseCodecInconsistent "unknown disappearance membership")) Right (Membership.lookupHeraldMembershipGeneration generation context.heraldMembershipHistory)
      header <- projectionBaseClaim (OracleDisappearance.admitProjectedDisappearanceProbeHeader opened subject membership)
      reports <- traverse (projectionBaseClaim . Disappearance.decodeDisappearanceEvidenceClaimCanonicalBytes opened . ByteString.copy) rawReports
      terminal <- traverse decodeTerminal rawTerminal
      pure
        ( projectedDisappearanceProbeHeaderId header,
          ProjectedDisappearanceProbe header (Map.fromList [(disappearanceEvidenceClaimReporter report, report) | report <- reports]) terminal
        )
    decodeTerminal (tag, rawIndex, fields) = do
      let index = controlIndex rawIndex
      case (tag, fields) of
        (0, [reporter, reason, evidence]) -> do
          home <- projectionBaseClaim (Identity.mkHeraldEpoch (ByteString.copy reporter))
          why <- case ByteString.unpack reason of
            [0] -> pure OracleDisappearance.MatchingPublicationObserved
            [1] -> pure OracleDisappearance.EvidenceContradicted
            _ -> Left (ProjectionBaseCodecMalformed "unknown disappearance invalidation reason")
          digest <- projectionBaseClaim (Disappearance.mkDisappearanceEvidenceDigest (ByteString.copy evidence))
          pure (ProjectedDisappearanceInvalidated (CommandDisappearanceInvalidation home why digest) index)
        (1, [decision]) -> do
          identifier <- projectionBaseClaim (Identity.mkLabelDecisionId (ByteString.copy decision))
          pure (ProjectedDisappearanceInvalidated (LabelOpenDisappearanceInvalidation identifier) index)
        (2, [outcome]) -> ProjectedDisappearanceResolved <$> projectionBaseClaim (Disappearance.decodeDisappearanceResolutionOutcomeCanonicalBytes (ByteString.copy outcome)) <*> pure index
        (3, []) -> pure (ProjectedDisappearanceAborted (ExplicitDisappearanceAbortReasonView OracleDisappearance.authorizedDisappearanceAbortReason) index)
        (4, [generation]) -> do
          successor <- projectionBaseClaim (Membership.mkHeraldMembershipGenerationId (ByteString.copy generation))
          pure (ProjectedDisappearanceAborted (MembershipSupersededDisappearanceAbortView successor) index)
        (5, [admission]) -> do
          identifier <- projectionBaseClaim (Membership.decodeHeraldAdmissionIdCanonicalBytes (ByteString.copy admission))
          pure (ProjectedDisappearanceAborted (AdmissionPreparingDisappearanceAbortView identifier) index)
        _ -> Left (ProjectionBaseCodecMalformed "unknown disappearance terminal shape")

admitProjectionDisappearanceBase :: State -> ProjectionDisappearanceBase -> Either ProjectionBaseCodecProblem ()
admitProjectionDisappearanceBase context (ProjectionDisappearanceBase probes) = do
  mapM_ checkProbe (Map.toAscList probes)
  let collecting = [projected | projected <- Map.elems probes, projectedDisappearanceTerminal projected == Nothing]
      coordinates = map (projectedDisappearanceProbeHeaderCoordinate . projectedDisappearanceHeader) collecting
  disappearanceBaseCheck (Set.size (Set.fromList coordinates) == length coordinates) "duplicate collecting disappearance coordinate"
  disappearanceBaseCheck (null collecting || oracleViewPendingHeraldAdmission (oracleView context) == Nothing) "collecting disappearance survives admission preparation"
  mapM_ checkCollecting collecting
  where
    checkProbe (key, ProjectedDisappearanceProbe header reports terminal) = do
      let opened = disappearanceProbeOpenControlIndex key
          subject = projectedDisappearanceProbeHeaderSubject header
          membership = projectedDisappearanceProbeHeaderMembership header
          members = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership)
          coordinate = disappearanceSubjectMembershipCoordinate subject membership
      disappearanceBaseCheck
        ( key == projectedDisappearanceProbeHeaderId header
            && opened > controlIndex 0
            && opened <= context.controlIndex
            && deriveDisappearanceProbeId opened == Right key
            && Membership.lookupHeraldMembershipGeneration (heraldMembershipGenerationId membership) context.heraldMembershipHistory == Just membership
            && maybe True (< opened) (Membership.heraldMembershipGenerationChangeControlIndex membership)
            && projectedDisappearanceProbeHeaderCoordinate header == coordinate
        )
        "inconsistent disappearance header"
      mapM_
        (\(reporter, claim) -> disappearanceBaseCheck (reporter == disappearanceEvidenceClaimReporter claim && reporter `elem` members && disappearanceEvidenceClaimProbeId claim == key && disappearanceEvidenceClaimCoordinate claim == coordinate) "inconsistent disappearance report")
        (Map.toAscList reports)
      mapM_ (checkTerminal opened subject membership members reports) terminal
    checkTerminal opened subject membership members reports terminal = do
      let index = case terminal of
            ProjectedDisappearanceInvalidated _ at -> at
            ProjectedDisappearanceResolved _ at -> at
            ProjectedDisappearanceAborted _ at -> at
      disappearanceBaseCheck (opened < index && index <= context.controlIndex) "disappearance terminal outside admitted cut"
      case terminal of
        ProjectedDisappearanceInvalidated (CommandDisappearanceInvalidation reporter _ _) _ ->
          disappearanceBaseCheck (reporter `elem` members) "disappearance invalidator not captured"
        ProjectedDisappearanceInvalidated (LabelOpenDisappearanceInvalidation decision) _ ->
          disappearanceBaseCheck
            ( case (Disappearance.disappearanceSubjectView subject, Map.lookup decision context.labelWorkflows) of
                (ControlledPredefinedSubjectView object _ _, Just workflow)
                  | liveDecisionObject (projectedLabelWorkflowDecision workflow) == object,
                    Just (ProjectedLabelReleased released _ _) <- projectedLabelWorkflowTerminal workflow ->
                      released == index
                _ -> False
            )
            "disappearance invalidation does not match released label"
        ProjectedDisappearanceResolved outcome _ -> do
          disappearanceBaseCheck (Map.keys reports == members) "resolved disappearance lacks complete captured evidence"
          disappearanceBaseCheck (resolveDisappearanceSubject context.systemId index subject == Right outcome) "disappearance outcome differs from deterministic resolution"
        ProjectedDisappearanceAborted (ExplicitDisappearanceAbortReasonView _) _ -> pure ()
        ProjectedDisappearanceAborted (MembershipSupersededDisappearanceAbortView generation) _ ->
          disappearanceBaseCheck
            ( case Membership.lookupHeraldMembershipGeneration generation context.heraldMembershipHistory of
                Just successor ->
                  heraldMembershipGenerationPredecessor successor == Just (heraldMembershipGenerationId membership)
                    && Membership.heraldMembershipGenerationChangeControlIndex successor == Just index
                Nothing -> False
            )
            "disappearance membership abort does not match successor"
        ProjectedDisappearanceAborted (AdmissionPreparingDisappearanceAbortView admission) _ ->
          disappearanceBaseCheck
            (maybe False (\record -> Admission.admissionRecordBeginIndex record == index && Admission.admissionRecordPredecessor record == membership) (Map.lookup admission context.heraldAdmissions))
            "disappearance preparation abort does not match admission Begin"
    checkCollecting projected = do
      let header = projectedDisappearanceHeader projected
          subject = projectedDisappearanceProbeHeaderSubject header
      disappearanceBaseCheck (projectedDisappearanceProbeHeaderMembership header == context.currentHeraldMembership) "collecting disappearance uses stale membership"
      case Disappearance.disappearanceSubjectView subject of
        ControlledPredefinedSubjectView object _ revision -> do
          let latest = oracleProjectionReleasedLabelAt context.controlIndex object context
              labelDeleted = maybe False (Label.releasedLabelStateIsDeleted . Live.liveRecordStoredState) latest
              resolvedDeleted = any (resolvedObject object) (Map.elems probes)
              pendingLabel = any (\workflow -> projectedLabelWorkflowPhase workflow == LabelWorkflowReleased && liveDecisionObject (projectedLabelWorkflowDecision workflow) == object) (Map.elems context.labelWorkflows)
          disappearanceBaseCheck
            (revision == (Live.liveRecordRevision <$> latest) && not labelDeleted && not resolvedDeleted && not pendingLabel)
            "collecting controlled disappearance conflicts with current label/deletion"
        RegularSortDefinitionSubjectView sortId _ occurrence -> do
          let latest = maximum (controlIndex 0 : [index | projectedProbe <- Map.elems probes, Just (ProjectedDisappearanceResolved outcome index) <- [projectedDisappearanceTerminal projectedProbe], RegularSortDefinitionSubjectView retired _ _ <- [Disappearance.disappearanceSubjectView (Disappearance.disappearanceResolutionSubject outcome)], retired == sortId])
          base <- if latest == controlIndex 0 then pure SortOccurrence.Genesis else projectionBaseClaim (SortOccurrence.resolvedRetirementOccurrenceBase latest)
          disappearanceBaseCheck (occurrence == SortOccurrence.deriveSortDefinitionOccurrenceId context.systemId sortId base) "collecting regular disappearance uses stale occurrence"
    resolvedObject object projected = case projectedDisappearanceTerminal projected of
      Just (ProjectedDisappearanceResolved outcome _) -> case Disappearance.disappearanceSubjectView (Disappearance.disappearanceResolutionSubject outcome) of
        ControlledPredefinedSubjectView resolved _ _ -> resolved == object
        _ -> False
      _ -> False

disappearanceBaseCheck :: Bool -> String -> Either ProjectionBaseCodecProblem ()
disappearanceBaseCheck condition message = unless condition (Left (ProjectionBaseCodecInconsistent message))

-- | Current voter authority and retained administration/failure facts. The
-- pending slot is derived from nonterminal changes rather than transmitted.
-- This leaf contains no source owner or historical configuration ledger.
data ProjectionVoterFailureBase
  = ProjectionVoterFailureBase
      !(Maybe Voter.VoterConfiguration)
      !(Map RaftNodeId Voter.OracleReplicaRegistration)
      !(Map Voter.VoterChangeId Voter.VoterChange)
      !(Maybe Voter.VoterChange)
      !(Map HeraldFailureProbeId ProjectedFailureProbe)
  deriving stock (Eq, Show)

type RawProjectionFailureProbe =
  (Word64, ByteString, ByteString, ByteString, [(ByteString, Word8)], Maybe (Word8, ByteString, Maybe ByteString))

captureProjectionVoterFailureBase :: State -> Either ProjectionBaseCodecProblem ProjectionVoterFailureBase
captureProjectionVoterFailureBase state = do
  pending <- projectionPendingVoterChange state.voterChanges
  unless (pending == state.pendingVoterChange) (Left (ProjectionBaseCodecInconsistent "voter pending slot"))
  let !base = ProjectionVoterFailureBase state.voterConfiguration state.oracleReplicas state.voterChanges pending state.failureProbes
  admitProjectionVoterFailureBase state base
  pure base

encodeProjectionVoterFailureBase :: ProjectionVoterFailureBase -> ByteString
encodeProjectionVoterFailureBase (ProjectionVoterFailureBase configuration registrations changes _ probes) =
  Codec.encode
    ( "ECLIPS-PROJECTION-VOTER-FAILURE-BASE" :: ByteString,
      Voter.encodeVoterConfiguration <$> configuration,
      map Voter.encodeReplicaRegistration (Map.elems registrations),
      map Voter.encodeVoterChange (Map.elems changes),
      map rawProbe (Map.toAscList probes)
    )
  where
    rawProbe :: (HeraldFailureProbeId, ProjectedFailureProbe) -> RawProjectionFailureProbe
    rawProbe (identifier, ProjectedFailureProbe target generation captured reports terminal) =
      ( controlIndexWord64 (Membership.heraldFailureProbeControlIndex identifier),
        heraldEpochBytes target,
        Membership.heraldMembershipGenerationIdBytes generation,
        Voter.encodeVoterConfiguration captured,
        [(heraldEpochBytes reporter, case result of ProbeReachable -> 0; ProbeUnreachable -> 1) | (reporter, result) <- Map.toAscList reports],
        rawTerminal <$> terminal
      )
    rawTerminal = \case
      ProjectedFailureProbeDismissed resolution -> (0, Membership.failureProbeResolutionIdCanonicalBytes resolution, Nothing)
      ProjectedFailureProbeRetired resolution generation -> (1, Membership.failureProbeResolutionIdCanonicalBytes resolution, Just (Membership.heraldMembershipGenerationIdBytes generation))
      ProjectedFailureProbeSuperseded generation -> (2, Membership.heraldMembershipGenerationIdBytes generation, Nothing)
      ProjectedVoterHostFailureAccepted certificate -> (3, Failure.encodeAcceptedVoterHostFailure certificate, Nothing)

decodeProjectionVoterFailureBase :: State -> ByteString -> Either ProjectionBaseCodecProblem ProjectionVoterFailureBase
decodeProjectionVoterFailureBase context bytes = do
  (domain, rawConfiguration, rawRegistrations, rawChanges, rawProbes) <- projectionBaseClaim (Codec.decode bytes :: Either String (ByteString, Maybe ByteString, [ByteString], [ByteString], [RawProjectionFailureProbe]))
  unless (domain == "ECLIPS-PROJECTION-VOTER-FAILURE-BASE") (Left (ProjectionBaseCodecMalformed "voter/failure domain"))
  configuration <- traverse (projectionBaseClaim . Voter.decodeVoterConfiguration . ByteString.copy) rawConfiguration
  registrations <- traverse (projectionBaseClaim . Voter.decodeReplicaRegistration . ByteString.copy) rawRegistrations
  changes <- traverse (projectionBaseClaim . Voter.decodeVoterChange . ByteString.copy) rawChanges
  probes <- traverse decodeProbe rawProbes
  let !byNode = Map.fromList [(Voter.replicaRegistrationNode registration, registration) | registration <- registrations]
      !byChange = Map.fromList [(Voter.voterChangeId change, change) | change <- changes]
  pending <- projectionPendingVoterChange byChange
  let !base = ProjectionVoterFailureBase configuration byNode byChange pending (Map.fromList probes)
  unless (encodeProjectionVoterFailureBase base == bytes) (Left (ProjectionBaseCodecMalformed "noncanonical voter/failure base"))
  admitProjectionVoterFailureBase context base
  pure base
  where
    decodeProbe (opened, rawTarget, rawGeneration, rawConfiguration, rawReports, rawTerminal) = do
      identifier <- projectionBaseClaim (Membership.deriveHeraldFailureProbeId (controlIndex opened))
      target <- projectionBaseClaim (Identity.mkHeraldEpoch (ByteString.copy rawTarget))
      generation <- projectionBaseClaim (Membership.mkHeraldMembershipGenerationId (ByteString.copy rawGeneration))
      configuration <- projectionBaseClaim (Voter.decodeVoterConfiguration (ByteString.copy rawConfiguration))
      reports <- traverse decodeReport rawReports
      terminal <- traverse decodeTerminal rawTerminal
      pure (identifier, ProjectedFailureProbe target generation configuration (Map.fromList reports) terminal)
    decodeReport (rawReporter, result) = do
      reporter <- projectionBaseClaim (Identity.mkHeraldEpoch (ByteString.copy rawReporter))
      value <- case result of
        0 -> Right ProbeReachable
        1 -> Right ProbeUnreachable
        _ -> Left (ProjectionBaseCodecMalformed "failure report tag")
      pure (reporter, value)
    decodeTerminal = \case
      (0, resolution, Nothing) -> ProjectedFailureProbeDismissed <$> projectionBaseClaim (Membership.decodeFailureProbeResolutionIdCanonicalBytes (ByteString.copy resolution))
      (1, resolution, Just generation) -> ProjectedFailureProbeRetired <$> projectionBaseClaim (Membership.decodeFailureProbeResolutionIdCanonicalBytes (ByteString.copy resolution)) <*> projectionBaseClaim (Membership.mkHeraldMembershipGenerationId (ByteString.copy generation))
      (2, generation, Nothing) -> ProjectedFailureProbeSuperseded <$> projectionBaseClaim (Membership.mkHeraldMembershipGenerationId (ByteString.copy generation))
      (3, certificate, Nothing) -> ProjectedVoterHostFailureAccepted <$> projectionBaseClaim (Failure.decodeAcceptedVoterHostFailure (ByteString.copy certificate))
      _ -> Left (ProjectionBaseCodecMalformed "failure terminal tag")

projectionPendingVoterChange :: Map Voter.VoterChangeId Voter.VoterChange -> Either ProjectionBaseCodecProblem (Maybe Voter.VoterChange)
projectionPendingVoterChange changes = case filter nonterminal (Map.elems changes) of
  [] -> Right Nothing
  [change] -> Right (Just change)
  _ -> Left (ProjectionBaseCodecInconsistent "multiple pending voter changes")
  where
    nonterminal change = Voter.voterChangePhase change `elem` [Voter.VoterChangePreparing, Voter.VoterChangeJointCommitted, Voter.VoterExcludedAwaitingHeraldRetirement]

-- Snapshot closure, not a replay of historical native configurations. Terminal
-- probes keep their own captured denominator, even after native exclusion and
-- semantic retirement have changed the current configuration and membership.
admitProjectionVoterFailureBase :: State -> ProjectionVoterFailureBase -> Either ProjectionBaseCodecProblem ()
admitProjectionVoterFailureBase context (ProjectionVoterFailureBase configuration registrations changes pending probes) = do
  mapM_ checkRegistration (Map.toAscList registrations)
  mapM_ checkInitialRegistration (Map.toAscList context.initialOracleReplicas)
  require "replica hosts are unique" (Set.size (Set.fromList (map Voter.replicaRegistrationHost (Map.elems registrations))) == Map.size registrations)
  case configuration of
    Nothing -> require "initialized voter configuration absent" (context.initialVoterConfiguration == Nothing)
    Just current -> do
      checkConfiguration current
      require "current voters are active" (all ((`Set.member` active) . OracleGenesis.raftVoterBindingHeraldEpoch) (Voter.voterConfigurationBindings current))
      require "genesis voter configuration differs" (Voter.voterConfigurationControlIndex current /= controlIndex 0 || maybe True (== current) context.initialVoterConfiguration)
      case (Voter.voterConfigurationView current, pending) of
        (Voter.JointVoterConfigurationView {}, Nothing) -> require "joint configuration lacks pending intention" False
        _ -> pure ()
  mapM_ checkChange (Map.toAscList changes)
  mapM_ checkPending pending
  mapM_ checkProbe (Map.toAscList probes)
  let liveTargets = [projectedFailureProbeTarget probe | probe <- Map.elems probes, projectedFailureProbeTerminal probe == Nothing]
  require "duplicate open failure target" (Set.size (Set.fromList liveTargets) == length liveTargets)
  where
    require detail condition = unless condition (Left (ProjectionBaseCodecInconsistent detail))
    cursor = context.controlIndex
    generations = Map.fromList [(heraldMembershipGenerationId generation, generation) | generation <- NonEmpty.toList (Membership.heraldMembershipHistoryGenerations context.heraldMembershipHistory)]
    active = Set.fromList (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs context.currentHeraldMembership))
    currentGeneration = heraldMembershipGenerationId context.currentHeraldMembership
    knownHosts = Set.unions (map (Set.fromList . NonEmpty.toList . heraldMembershipGenerationActiveHeraldEpochs) (Map.elems generations))
    knownBinding binding = case Map.lookup (OracleGenesis.raftVoterBindingNode binding) registrations of
      Just registration -> Voter.replicaRegistrationHost registration == OracleGenesis.raftVoterBindingHeraldEpoch binding
      -- Optional-voter projections can observe native changes without having
      -- bootstrapped the registration catalogue. Preserve that supported state.
      Nothing -> Map.null context.initialOracleReplicas && Set.member (OracleGenesis.raftVoterBindingHeraldEpoch binding) knownHosts
    checkRegistration (node, registration) = require "replica registration closure" (node == Voter.replicaRegistrationNode registration && Voter.replicaRegistrationControlIndex registration <= cursor && Set.member (Voter.replicaRegistrationHost registration) knownHosts)
    checkInitialRegistration (node, initial) =
      require "initial replica registration lost or changed host" (maybe False ((== Voter.replicaRegistrationHost initial) . Voter.replicaRegistrationHost) (Map.lookup node registrations))
    checkConfiguration current = require "voter configuration closure" (Voter.voterConfigurationControlIndex current <= cursor && all knownBinding (Voter.voterConfigurationBindings current))
    checkChange (identifier, change) = do
      require "voter change coordinate or bindings" (identifier == Voter.voterChangeId change && Voter.voterChangeControlIndex change <= cursor && all knownBinding (Voter.oracleVoterBindingList (Voter.voterChangeOldBindings change) <> Voter.oracleVoterBindingList (Voter.voterChangeNewBindings change)))
      case Voter.voterChangeReason change of
        Voter.ExplicitVoterReason _ -> pure ()
        Voter.AcceptedHostFailureReason resolution -> case Map.lookup (Membership.failureProbeResolutionProbeId resolution) probes of
          Just (ProjectedFailureProbe _ _ _ _ (Just (ProjectedVoterHostFailureAccepted certificate))) -> do
            require "failure intention does not match certificate" (Failure.acceptedVoterHostFailureResolution certificate == resolution && Failure.acceptedVoterHostFailureChangeId certificate == identifier && Voter.voterChangeExpectedConfiguration change == Voter.voterConfigurationId (Failure.acceptedVoterHostFailureConfiguration certificate) && Voter.StableVoterConfigurationView (Voter.voterChangeOldBindings change) == Voter.voterConfigurationView (Failure.acceptedVoterHostFailureConfiguration certificate) && Voter.oracleVoterBindingList (Voter.voterChangeNewBindings change) == filter ((/= Failure.acceptedVoterHostFailureTarget certificate) . OracleGenesis.raftVoterBindingHeraldEpoch) (Voter.oracleVoterBindingList (Voter.voterChangeOldBindings change)))
            case Voter.voterChangePhase change of
              Voter.VoterChangeCompleted -> require "completed failure intention lacks retirement" (any (\generation -> heraldMembershipGenerationRetirementId generation == Just resolution && heraldMembershipGenerationRetiredHeraldEpoch generation == Just (Failure.acceptedVoterHostFailureTarget certificate) && heraldMembershipGenerationRetirementControlIndex generation == Just (Voter.voterChangeControlIndex change)) (Map.elems generations))
              _ -> require "pending failure target already retired" (Set.member (Failure.acceptedVoterHostFailureTarget certificate) active)
          _ -> require "failure intention lacks accepted certificate" False
    checkPending change = case configuration of
      Nothing -> pure ()
      Just current -> require "pending voter intention and current configuration differ" $ case (Voter.voterChangePhase change, Voter.voterConfigurationView current) of
        (Voter.VoterChangePreparing, Voter.StableVoterConfigurationView old) -> old == Voter.voterChangeOldBindings change && Voter.voterConfigurationId current == Voter.voterChangeExpectedConfiguration change && Voter.voterConfigurationControlIndex current < Voter.voterChangeIdControlIndex (Voter.voterChangeId change)
        (Voter.VoterChangeJointCommitted, Voter.JointVoterConfigurationView old new) -> old == Voter.voterChangeOldBindings change && new == Voter.voterChangeNewBindings change && Voter.voterConfigurationPredecessor current == Just (Voter.voterChangeExpectedConfiguration change) && Voter.voterConfigurationControlIndex current == Voter.voterChangeControlIndex change
        (Voter.VoterExcludedAwaitingHeraldRetirement, Voter.StableVoterConfigurationView new) -> new == Voter.voterChangeNewBindings change && Voter.voterConfigurationControlIndex current == Voter.voterChangeControlIndex change
        _ -> False
    checkProbe (identifier, probe@(ProjectedFailureProbe target generation captured reports terminal)) = do
      let opened = Membership.heraldFailureProbeControlIndex identifier
          eligible = Set.fromList (map OracleGenesis.raftVoterBindingHeraldEpoch (Voter.voterConfigurationBindings captured))
          majority result = 2 * length (filter (== result) (Map.elems reports)) > Set.size eligible
          resolutionMatches disposition resolution = resolution == Membership.deriveFailureProbeResolutionId identifier disposition
      membership <- maybe (Left (ProjectionBaseCodecInconsistent "unknown failure membership")) Right (Map.lookup generation generations)
      checkConfiguration captured
      require "failure probe coordinate or captured denominator" (opened <= cursor && Voter.voterConfigurationControlIndex captured < opened && target `elem` heraldMembershipGenerationActiveHeraldEpochs membership && Set.isSubsetOf eligible (Set.fromList (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership))) && Map.keysSet reports `Set.isSubsetOf` eligible && case Voter.voterConfigurationView captured of Voter.StableVoterConfigurationView {} -> True; _ -> False)
      require "failure terminal cannot precede its Open" (terminal == Nothing || opened < cursor)
      case terminal of
        Nothing -> require "open failure probe is stale" (generation == currentGeneration && maybe True (== captured) configuration)
        Just (ProjectedFailureProbeDismissed resolution) -> require "failure dismissal lacks majority" (resolutionMatches Membership.DismissFailureProbe resolution && majority ProbeReachable)
        Just (ProjectedFailureProbeRetired resolution successor) -> require "failure retirement lacks membership evidence" (resolutionMatches Membership.RetireFailureProbeTarget resolution && majority ProbeUnreachable && maybe False (\next -> heraldMembershipGenerationRetirementId next == Just resolution && heraldMembershipGenerationRetiredHeraldEpoch next == Just target && heraldMembershipGenerationPredecessor next == Just generation && maybe False (> opened) (heraldMembershipGenerationRetirementControlIndex next)) (Map.lookup successor generations))
        Just (ProjectedFailureProbeSuperseded successor) -> do
          _ <- projectionBaseClaim (Membership.heraldMembershipLineage generation successor context.heraldMembershipHistory)
          -- Beginning an intention supersedes probes before native authority
          -- changes; cancellation can leave the captured configuration current.
          require
            "failure supersession has no membership or voter intention"
            (successor /= generation || any (\change -> Voter.voterChangeIdControlIndex (Voter.voterChangeId change) > opened && Voter.voterChangeExpectedConfiguration change == Voter.voterConfigurationId captured) (Map.elems changes))
        Just (ProjectedVoterHostFailureAccepted certificate) -> do
          require "accepted failure certificate differs from retained probe" (resolutionMatches Membership.RetireFailureProbeTarget (Failure.acceptedVoterHostFailureResolution certificate) && Failure.acceptedVoterHostFailureTarget certificate == target && Failure.acceptedVoterHostFailureMembership certificate == generation && Failure.acceptedVoterHostFailureConfiguration certificate == captured && Failure.acceptedVoterHostFailureReports certificate == projectedFailureProbeReports probe && Failure.acceptedVoterHostFailureControlIndex certificate <= cursor)
          require "accepted failure has no voter intention" (Map.member (Failure.acceptedVoterHostFailureChangeId certificate) changes)

-- | Portable snapshots carry already-admitted donor facts. These errors describe
-- nominal and cross-fact admission, not a replay proof of historical commands.
data ProjectionBaseCodecProblem
  = ProjectionBaseCodecMalformed String
  | ProjectionBaseCodecInconsistent String
  deriving stock (Eq, Show)

projectionBaseClaim :: (Show problem) => Either problem value -> Either ProjectionBaseCodecProblem value
projectionBaseClaim = either (Left . ProjectionBaseCodecMalformed . show) Right

type ProjectionBaseTranscript =
  ( ByteString,
    (ByteString, Maybe ByteString, [ByteString]),
    (ByteString, ByteString, ByteString),
    (Word64, Maybe ByteString),
    (Word64, [(Word64, ByteString)], [(Word64, ByteString, [(ByteString, Word64, ByteString)])])
  )

-- | Explicit fact frames and retained exact evidence; no owner State, previous
-- replay base, local identity or transport/session state is serialized.
encodeProjectionBase :: ProjectionBaseCapture -> ByteString
encodeProjectionBase (ProjectionBaseCapture (CheckedProjectionBase genesis base) (ProjectionBaseArchive entries advances through)) = Codec.encode transcript
  where
    ProjectionBaseGenesis system catalogue configuration initial _ initialVoters initialReplicas = genesis
    identity = (system, catalogue, configuration, initial)
    transcript :: ProjectionBaseTranscript
    transcript =
      ( "ECLIPS-PROJECTION-BASE",
        ( encodeProjectionIdentityBase (ProjectionIdentityBase identity base.baseControlIndex base.baseHeraldMembershipHistory base.baseHeraldAdmissions base.baseStartedProcesses base.baseEndedProcesses),
          Voter.encodeVoterConfiguration <$> initialVoters,
          map Voter.encodeReplicaRegistration (Map.elems initialReplicas)
        ),
        ( encodeProjectionLabelBase (ProjectionLabelBase identity base.baseControlIndex base.baseLabelWorkflows base.baseCompletedLabelWorkflows),
          encodeProjectionDisappearanceBase (ProjectionDisappearanceBase base.baseDisappearanceProbes),
          encodeProjectionVoterFailureBase (ProjectionVoterFailureBase base.baseVoterConfiguration base.baseOracleReplicas base.baseVoterChanges base.basePendingVoterChange base.baseFailureProbes)
        ),
        (controlIndexWord64 base.baseLastSemanticControlIndex, OracleIdentity.oracleStateDigestBytes <$> base.baseLatestCanonicalStateDigest),
        ( controlIndexWord64 through,
          [(controlIndexWord64 key, canonicalAppliedOracleEntryBytes entry) | (key, entry) <- Map.toAscList entries],
          [ (controlIndexWord64 key, Membership.heraldMembershipGenerationCanonicalBytes generation, [(processEpochIdBytes process, controlIndexWord64 at, processEndReasonCanonicalBytes reason) | MembershipAdvanceProcessEnd process at reason <- ends])
          | (key, MembershipAdvanceEvidence _ generation ends) <- Map.toAscList advances
          ]
        )
      )

-- | Admit portable facts using only immutable receiver-owned genesis. The Join
-- envelope separately binds donor, admission attempt, sealed contribution and
-- control cut. A decoded payload alone does not authorize its live installation.
decodeProjectionBase :: State -> ByteString -> Either ProjectionBaseCodecProblem ProjectionBaseCapture
decodeProjectionBase receiver bytes = do
  projectionBaseRequire (receiver.controlIndex == controlIndex 0) "Projection base receiver is not fresh"
  (domain, (identityFrame, initialVoters, initialReplicas), (labelsFrame, disappearanceFrame, votersFrame), (rawSemantic, rawDigest), (rawThrough, rawEntries, rawAdvances)) <- projectionBaseClaim (Codec.decode bytes :: Either String ProjectionBaseTranscript)
  projectionBaseRequire (domain == "ECLIPS-PROJECTION-BASE") "Projection base domain"
  projectionBaseRequire
    (initialVoters == (Voter.encodeVoterConfiguration <$> receiver.initialVoterConfiguration) && initialReplicas == map Voter.encodeReplicaRegistration (Map.elems receiver.initialOracleReplicas))
    "Projection base initial voter facts differ"
  identity@(ProjectionIdentityBase _ cursor history admissions started ended) <- projectionBaseClaim (decodeProjectionIdentityBaseFor receiver identityFrame)
  catalogue <- projectionBaseClaim (admitIdentityBase receiver identity)
  let identityContext =
        (genesisOnlyState receiver)
          { controlIndex = cursor,
            heraldCatalogue = catalogue,
            heraldAdmissions = admissions,
            currentHeraldMembership = Membership.heraldMembershipHistoryCurrent history,
            heraldMembershipHistory = history,
            startedProcesses = started,
            endedProcesses = ended
          }
  ProjectionLabelBase _ _ labels completed <- decodeProjectionLabelBase identityContext labelsFrame
  ProjectionVoterFailureBase voters replicas changes pending failures <- decodeProjectionVoterFailureBase identityContext votersFrame
  let labelContext =
        identityContext
          { labelWorkflows = labels,
            completedLabelWorkflows = completed,
            voterConfiguration = voters,
            oracleReplicas = replicas,
            voterChanges = changes,
            pendingVoterChange = pending,
            failureProbes = failures
          }
  ProjectionDisappearanceBase disappearances <- decodeProjectionDisappearanceBase labelContext disappearanceFrame
  digest <- traverse (projectionBaseClaim . OracleIdentity.mkOracleStateDigest . ByteString.copy) rawDigest
  let semantic = controlIndex rawSemantic
      through = controlIndex rawThrough
      facts = labelContext {disappearanceProbes = disappearances, lastSemanticControlIndex = semantic, latestCanonicalStateDigest = digest}
  projectionBaseRequire (semantic <= cursor && through <= cursor) "Projection base scalar beyond cut"
  projectionBaseRequire (cursor /= controlIndex 0 || (semantic == controlIndex 0 && digest == Nothing)) "nonempty Projection genesis scalars"
  admitProjectionPrimaryCoordinates facts
  entries <- Map.fromList <$> traverse decodeEntry rawEntries
  advances <- Map.fromList <$> traverse decodeAdvance rawAdvances
  admitProjectionBaseArchive facts entries advances through
  let base = ProjectionBaseCapture (CheckedProjectionBase (projectionBaseGenesis receiver) (captureReplayBase facts)) (ProjectionBaseArchive entries advances through)
  projectionBaseRequire (encodeProjectionBase base == bytes) "noncanonical Projection base"
  pure base
  where
    decodeEntry (rawKey, frame) = do
      entry <- projectionBaseClaim (decodeCanonicalAppliedOracleEntry (ByteString.copy frame))
      let key = controlIndex rawKey
      projectionBaseRequire (key == appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry)) "Projection entry key mismatch"
      pure (key, entry)
    decodeAdvance (rawKey, frame, rawEnds) = do
      generation <- projectionBaseClaim (Membership.decodeHeraldMembershipGenerationCanonicalBytes (ByteString.copy frame))
      ends <- traverse decodeEnd rawEnds
      let key = controlIndex rawKey
      pure (key, MembershipAdvanceEvidence key generation ends)
    decodeEnd (rawProcess, rawAt, frame) =
      MembershipAdvanceProcessEnd
        <$> projectionBaseClaim (Identity.mkProcessEpochId (ByteString.copy rawProcess))
        <*> pure (controlIndex rawAt)
        <*> projectionBaseClaim (Lifecycle.decodeProcessEndReasonCanonicalBytes (ByteString.copy frame))

projectionBaseRequire :: Bool -> String -> Either ProjectionBaseCodecProblem ()
projectionBaseRequire condition detail = unless condition (Left (ProjectionBaseCodecInconsistent detail))

-- These are distinct primary commands; derived invalidations, phase changes and
-- retirement Ends may legitimately share their causing command's coordinate.
admitProjectionPrimaryCoordinates :: State -> Either ProjectionBaseCodecProblem ()
admitProjectionPrimaryCoordinates state = do
  let generations = NonEmpty.toList (Membership.heraldMembershipHistoryGenerations state.heraldMembershipHistory)
      primary =
        map projectedStartedProcessControlIndex (Map.elems state.startedProcesses)
          <> [projectedEndedProcessControlIndex ended | ended <- Map.elems state.endedProcesses, not (processEndReasonIsHeraldRetirement (projectedEndedProcessReason ended))]
          <> map Admission.admissionRecordBeginIndex (Map.elems state.heraldAdmissions)
          <> [at | generation <- generations, Just at <- [Membership.heraldMembershipGenerationChangeControlIndex generation]]
          <> map (Live.liveDecisionRequestControlIndex . projectedLabelWorkflowDecision) (Map.elems state.labelWorkflows)
          <> map Membership.heraldFailureProbeControlIndex (Map.keys state.failureProbes)
          <> map (disappearanceProbeOpenControlIndex . projectedDisappearanceProbeHeaderId . projectedDisappearanceHeader) (Map.elems state.disappearanceProbes)
      requests = map projectedStartedProcessRequest (Map.elems state.startedProcesses) <> map (Live.liveDecisionOpenRequestId . projectedLabelWorkflowDecision) (Map.elems state.labelWorkflows)
  projectionBaseRequire (length primary == Set.size (Set.fromList primary)) "Projection primary command coordinates collide"
  projectionBaseRequire (length requests == Set.size (Set.fromList requests)) "Projection primary request identities collide"
  let changed =
        primary
          <> map Admission.admissionRecordChangedIndex (Map.elems state.heraldAdmissions)
          <> map Voter.replicaRegistrationControlIndex (Map.elems state.oracleReplicas)
          <> maybe [] (pure . Voter.voterConfigurationControlIndex) state.voterConfiguration
          <> map Voter.voterChangeControlIndex (Map.elems state.voterChanges)
          <> [Failure.acceptedVoterHostFailureControlIndex certificate | probe <- Map.elems state.failureProbes, Just (ProjectedVoterHostFailureAccepted certificate) <- [projectedFailureProbeTerminal probe]]
          <> [at | probe <- Map.elems state.disappearanceProbes, Just terminal <- [projectedDisappearanceTerminal probe], let at = case terminal of ProjectedDisappearanceInvalidated _ index -> index; ProjectedDisappearanceResolved _ index -> index; ProjectedDisappearanceAborted _ index -> index]
  projectionBaseRequire (all (<= state.lastSemanticControlIndex) changed) "Projection semantic frontier precedes retained semantic facts"

admitProjectionBaseArchive :: State -> Map ControlIndex CanonicalAppliedOracleEntry -> Map ControlIndex MembershipAdvanceEvidence -> ControlIndex -> Either ProjectionBaseCodecProblem ()
admitProjectionBaseArchive facts entries advances through = do
  let keys = Map.keysSet entries `Set.union` Map.keysSet advances
      after = Set.toAscList (snd (Set.split through keys))
      adjacent before next = next == nextControlIndex before
  projectionBaseRequire (Set.null (Map.keysSet entries `Set.intersection` Map.keysSet advances)) "Projection evidence kinds overlap"
  projectionBaseRequire (all (\key -> key > controlIndex 0 && key <= facts.controlIndex) (Set.toList keys)) "Projection evidence outside cut"
  -- Check adjacency over retained rows, without enumerating a claimed interval.
  projectionBaseRequire (all (uncurry adjacent) (zip (through : after) after) && foldl' (\_ key -> key) through after == facts.controlIndex) "Projection uncovered evidence gap"
  mapM_ checkAdvance (Map.toAscList advances)
  let semanticEntries = Map.filter (isSemanticEntry . canonicalAppliedOracleEntryValue) entries
      semanticKeys = Map.keysSet semanticEntries `Set.union` Map.keysSet advances
      frontier = facts.lastSemanticControlIndex
  projectionBaseRequire (all (<= frontier) (Set.toList semanticKeys)) "Projection semantic frontier precedes exact semantic evidence"
  projectionBaseRequire (frontier <= through || Set.member frontier semanticKeys) "Projection semantic frontier has no uncovered semantic evidence"
  projectionBaseRequire (maybe True (isSemanticEntry . canonicalAppliedOracleEntryValue) (Map.lookup frontier entries)) "Projection semantic frontier names maintenance evidence"
  case Map.lookupMax entries of
    Just (index, entry)
      | toInteger (Map.size (snd (Map.split index advances))) == toInteger (controlIndexWord64 facts.controlIndex) - toInteger (controlIndexWord64 index) ->
          projectionBaseRequire (facts.latestCanonicalStateDigest == appliedEntryPostStateDigest (canonicalAppliedOracleEntryValue entry)) "Projection state digest differs from latest exact canonical evidence"
    _ -> pure ()
  where
    isSemanticEntry entry = case (appliedEntryCommand entry, appliedEntryOracleProgress entry) of
      (Nothing, Just _) -> False
      _ -> True
    checkAdvance (key, MembershipAdvanceEvidence embedded generation ends) = do
      projectionBaseRequire
        (key == embedded && Membership.heraldMembershipGenerationChangeControlIndex generation == Just key && generation `elem` Membership.heraldMembershipHistoryGenerations facts.heraldMembershipHistory)
        "Projection detached membership evidence differs from history"
      let supplied = [(process, ProjectedEndedProcess process at reason) | MembershipAdvanceProcessEnd process at reason <- ends]
          expected = Map.filter (\ended -> projectedEndedProcessControlIndex ended == key && processEndReasonIsHeraldRetirement (projectedEndedProcessReason ended)) facts.endedProcesses
      projectionBaseRequire (length supplied == Map.size (Map.fromList supplied) && Map.fromList supplied == expected) "Projection detached retirement Ends differ"

-- | Narrow authority projection passed to current admission coordinators.
data View = View
  { viewSystemId :: SystemId,
    viewControlIndex :: ControlIndex,
    viewVoterConfiguration :: Maybe Voter.VoterConfiguration,
    viewOracleReplicas :: Map RaftNodeId Voter.OracleReplicaRegistration,
    viewVoterChanges :: Map Voter.VoterChangeId Voter.VoterChange,
    viewPendingVoterChange :: Maybe Voter.VoterChange,
    viewLocalHeraldEpoch :: HeraldEpoch,
    viewCurrentHeraldMembership :: HeraldMembershipGeneration,
    viewHeraldMembershipHistory :: HeraldMembershipHistory,
    viewActiveHeralds :: [HeraldMember],
    viewHeraldAdmissions :: Map Membership.HeraldAdmissionId Admission.HeraldAdmissionRecord,
    viewActiveHeraldEpochs :: Set HeraldEpoch,
    viewInitialProjectionDigest :: InitialProjectionDigest,
    viewGenesisProcesses :: Map ProcessEpochId AppliedProcessBootstrap,
    viewStartedProcesses :: Map ProcessEpochId ProjectedStartedProcess,
    viewEndedProcesses :: Map ProcessEpochId ProjectedEndedProcess,
    viewLabelWorkflows :: Map LabelDecisionId ProjectedLabelWorkflow,
    viewCompletedLabelWorkflows :: Map LabelDecisionId LabelOutcomeDigest,
    viewFailureProbes :: Map HeraldFailureProbeId ProjectedFailureProbe,
    viewDisappearanceProbes :: Map DisappearanceProbeId ProjectedDisappearanceProbe
  }
  deriving stock (Eq)

-- | Complete immutable Oracle evidence, retained after terminal cleanup in the
-- live disappearance owner so historical topology cuts remain reconstructible.
data ProjectedDisappearanceProbe
  = ProjectedDisappearanceProbe
      ProjectedDisappearanceProbeHeader
      (Map HeraldEpoch DisappearanceEvidenceClaim)
      (Maybe ProjectedDisappearanceTerminal)
  deriving stock (Eq, Show)

data ProjectedDisappearanceTerminal
  = ProjectedDisappearanceInvalidated DisappearanceProbeInvalidationView ControlIndex
  | ProjectedDisappearanceResolved DisappearanceResolutionOutcome ControlIndex
  | ProjectedDisappearanceAborted DisappearanceProbeAbortReasonView ControlIndex
  deriving stock (Eq, Show)

projectedDisappearanceHeader :: ProjectedDisappearanceProbe -> ProjectedDisappearanceProbeHeader
projectedDisappearanceHeader (ProjectedDisappearanceProbe header _ _) = header

projectedDisappearanceReports :: ProjectedDisappearanceProbe -> [(HeraldEpoch, DisappearanceEvidenceClaim)]
projectedDisappearanceReports (ProjectedDisappearanceProbe _ reports _) = Map.toAscList reports

projectedDisappearanceTerminal :: ProjectedDisappearanceProbe -> Maybe ProjectedDisappearanceTerminal
projectedDisappearanceTerminal (ProjectedDisappearanceProbe _ _ terminal) = terminal

projectedDisappearanceProbes :: State -> [(DisappearanceProbeId, ProjectedDisappearanceProbe)]
projectedDisappearanceProbes state = Map.toAscList state.disappearanceProbes

oracleViewDisappearanceProbe :: DisappearanceProbeId -> View -> Maybe ProjectedDisappearanceProbe
oracleViewDisappearanceProbe probe view = Map.lookup probe view.viewDisappearanceProbes

oracleProjectionControlledDisappearanceAt :: ControlIndex -> GlobalObjectId -> State -> Maybe DisappearanceResolutionOutcome
oracleProjectionControlledDisappearanceAt requested object state =
  latestDisappearanceOutcome requested matches state
  where
    matches outcome = case disappearanceResolutionOutcomeView outcome of
      ControlledDisappearanceResolved resolved _ _ _ -> resolved == object
      _ -> False

oracleProjectionRegularRetirementAt :: ControlIndex -> SortId -> State -> Maybe ControlIndex
oracleProjectionRegularRetirementAt requested sortId state =
  disappearanceResolutionControlIndex <$> latestDisappearanceOutcome requested matches state
  where
    matches outcome = case disappearanceResolutionOutcomeView outcome of
      RegularSortDefinitionRetired retired _ _ _ _ -> retired == sortId
      _ -> False

latestDisappearanceOutcome :: ControlIndex -> (DisappearanceResolutionOutcome -> Bool) -> State -> Maybe DisappearanceResolutionOutcome
latestDisappearanceOutcome requested matches state =
  case sortOn
    disappearanceResolutionControlIndex
    [ outcome
    | projected <- Map.elems state.disappearanceProbes,
      Just (ProjectedDisappearanceResolved outcome index) <- [projectedDisappearanceTerminal projected],
      index <= requested,
      matches outcome
    ] of
    [] -> Nothing
    outcomes -> Just (last outcomes)

-- | Herald-owned configured-Start projection. Oracle keeps its process record
-- opaque; this projection resolves the retained configured bootstrap once and
-- stores only the exact facts used by Herald owners.
data ProjectedStartedProcess
  = ProjectedStartedProcess ProcessStart ControlIndex OracleClientRequestId
  deriving stock (Eq, Show)

projectedStartedProcessBootstrap ::
  ProjectedStartedProcess -> ProcessStart
projectedStartedProcessBootstrap (ProjectedStartedProcess bootstrap _ _) = bootstrap

projectedStartedProcessControlIndex :: ProjectedStartedProcess -> ControlIndex
projectedStartedProcessControlIndex (ProjectedStartedProcess _ index _) = index

projectedStartedProcessRequest :: ProjectedStartedProcess -> OracleClientRequestId
projectedStartedProcessRequest (ProjectedStartedProcess _ _ request) = request

data ProjectedEndedProcess
  = ProjectedEndedProcess ProcessEpochId ControlIndex ProcessEndReason
  deriving stock (Eq, Show)

projectedEndedProcessEpoch :: ProjectedEndedProcess -> ProcessEpochId
projectedEndedProcessEpoch (ProjectedEndedProcess process _ _) = process

projectedEndedProcessControlIndex :: ProjectedEndedProcess -> ControlIndex
projectedEndedProcessControlIndex (ProjectedEndedProcess _ index _) = index

projectedEndedProcessReason :: ProjectedEndedProcess -> ProcessEndReason
projectedEndedProcessReason (ProjectedEndedProcess _ _ reason) = reason

-- | One staged Oracle-derived resident End carried with a checked membership
-- advance. The constructor is opaque so the batch admission below is the only
-- path from supplied facts to projected process liveness.
data MembershipAdvanceProcessEnd
  = MembershipAdvanceProcessEnd ProcessEpochId ControlIndex ProcessEndReason
  deriving stock (Eq, Show)

membershipAdvanceProcessEnd ::
  ProcessEpochId ->
  ControlIndex ->
  ProcessEndReason ->
  MembershipAdvanceProcessEnd
membershipAdvanceProcessEnd = MembershipAdvanceProcessEnd

membershipAdvanceProcessEndEpoch :: MembershipAdvanceProcessEnd -> ProcessEpochId
membershipAdvanceProcessEndEpoch (MembershipAdvanceProcessEnd process _ _) = process

membershipAdvanceProcessEndControlIndex ::
  MembershipAdvanceProcessEnd -> ControlIndex
membershipAdvanceProcessEndControlIndex (MembershipAdvanceProcessEnd _ index _) = index

membershipAdvanceProcessEndReason ::
  MembershipAdvanceProcessEnd -> ProcessEndReason
membershipAdvanceProcessEndReason (MembershipAdvanceProcessEnd _ _ reason) = reason

-- | Exact retained staged evidence. It remains separate from live canonical
-- watch entries, while occupying the same contiguous control coordinate space.
data MembershipAdvanceEvidence
  = MembershipAdvanceEvidence
      ControlIndex
      HeraldMembershipGeneration
      [MembershipAdvanceProcessEnd]
  deriving stock (Eq, Show)

-- | Bounded live label workflow evidence needed by the Herald coordinator.
-- Oracle remains the authority for phase admission; this projection retains
-- only the immutable decision, reporter sets, and terminal facts which let a
-- Herald derive level-triggered follow-up intentions without rescanning watch
-- history.
data LabelWorkflowPhase
  = LabelWorkflowReleased
  | LabelWorkflowCompleted
  deriving stock (Eq, Ord, Show)

data ProjectedLabelTerminal
  = ProjectedLabelNotApplied ControlIndex LabelOutcomeDigest
  | ProjectedLabelReleased ControlIndex ReleasedLabelOverlay LabelOutcomeDigest
  deriving stock (Eq, Show)

data ProjectedLabelWorkflow = ProjectedLabelWorkflow
  { decision :: LiveLabelDecision,
    phase :: LabelWorkflowPhase,
    preparedFacts :: Maybe PreparedLabelFacts,
    preparedDigest :: Maybe PreparedLabelDigest,
    terminal :: Maybe ProjectedLabelTerminal
  }
  deriving stock (Eq, Show)

projectedLabelWorkflowDecision :: ProjectedLabelWorkflow -> LiveLabelDecision
projectedLabelWorkflowDecision workflow = workflow.decision

projectedLabelWorkflowPhase :: ProjectedLabelWorkflow -> LabelWorkflowPhase
projectedLabelWorkflowPhase workflow = workflow.phase

projectedLabelWorkflowPreparedFacts :: ProjectedLabelWorkflow -> Maybe PreparedLabelFacts
projectedLabelWorkflowPreparedFacts workflow = workflow.preparedFacts

projectedLabelWorkflowPreparedDigest :: ProjectedLabelWorkflow -> Maybe PreparedLabelDigest
projectedLabelWorkflowPreparedDigest workflow = workflow.preparedDigest

projectedLabelWorkflowTerminal :: ProjectedLabelWorkflow -> Maybe ProjectedLabelTerminal
projectedLabelWorkflowTerminal workflow = workflow.terminal

-- | One failure probe as observed through the canonical Oracle watch stream.
-- Reports remain keyed by their voter-host Herald so direct-probe retries can
-- be level-triggered without rescanning retained watch entries.
data ProjectedFailureProbe = ProjectedFailureProbe
  { target :: HeraldEpoch,
    generation :: HeraldMembershipGenerationId,
    capturedVoterConfiguration :: Voter.VoterConfiguration,
    reports :: Map HeraldEpoch ProbeResult,
    terminal :: Maybe ProjectedFailureProbeTerminal
  }
  deriving stock (Eq, Show)

data ProjectedFailureProbeTerminal
  = ProjectedFailureProbeDismissed FailureProbeResolutionId
  | ProjectedFailureProbeRetired FailureProbeResolutionId HeraldMembershipGenerationId
  | ProjectedFailureProbeSuperseded HeraldMembershipGenerationId
  | ProjectedVoterHostFailureAccepted Failure.AcceptedVoterHostFailure
  deriving stock (Eq, Show)

projectedFailureProbeTarget :: ProjectedFailureProbe -> HeraldEpoch
projectedFailureProbeTarget probe = probe.target

projectedFailureProbeGeneration :: ProjectedFailureProbe -> HeraldMembershipGenerationId
projectedFailureProbeGeneration probe = probe.generation

projectedFailureProbeConfiguration :: ProjectedFailureProbe -> Voter.VoterConfiguration
projectedFailureProbeConfiguration probe = probe.capturedVoterConfiguration

projectedFailureProbeReports :: ProjectedFailureProbe -> [(HeraldEpoch, ProbeResult)]
projectedFailureProbeReports probe = Map.toAscList probe.reports

projectedFailureProbeTerminal :: ProjectedFailureProbe -> Maybe ProjectedFailureProbeTerminal
projectedFailureProbeTerminal probe = probe.terminal

-- | Total projection from opaque checked Herald genesis and its checked startup
-- selection. The existing signature is retained: Oracle/Herald parity is a
-- separate explicit validation against 'CheckedOracleGenesis'.
initialState ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  State
initialState genesis bootstraps =
  State
    { systemId = checkedSystemId genesis,
      catalogueDigest = checkedCatalogueDigest genesis,
      configurationDigest = checkedConfigurationDigest genesis,
      predefinedOccurrences = occurrences,
      initialTopology = checkedInitialTopologyProjection bootstraps,
      controlIndex = checkedOracleControlIndex genesis,
      localHeraldEpoch = checkedLocalHeraldEpoch genesis,
      localStartupAdmission = checkedStartupAdmission genesis,
      heraldCatalogue = catalogue,
      genesisHeraldCatalogue = catalogue,
      heraldAdmissions = Map.empty,
      genesisHeraldMembership = genesisMembership,
      currentHeraldMembership = genesisMembership,
      heraldMembershipHistory = singletonMembershipHistory genesisMembership,
      membershipAdvances = Map.empty,
      initialProjectionDigest = checkedInitialProjectionDigest bootstraps,
      genesisProcesses = processMap,
      startedProcesses = Map.empty,
      endedProcesses = Map.empty,
      labelWorkflows = Map.empty,
      completedLabelWorkflows = Map.empty,
      failureProbes = Map.empty,
      disappearanceProbes = Map.empty,
      initialVoterConfiguration = Nothing,
      voterConfiguration = Nothing,
      initialOracleReplicas = Map.empty,
      oracleReplicas = Map.empty,
      voterChanges = Map.empty,
      pendingVoterChange = Nothing,
      appliedEntries = Map.empty,
      replayBase = Nothing,
      canonicalEvidenceCoveredThrough = controlIndex 0,
      lastSemanticControlIndex = controlIndex 0,
      latestCanonicalStateDigest = Nothing
    }
  where
    occurrences = checkedPredefinedOccurrenceSet genesis
    members = checkedActiveHeralds genesis
    catalogue = Map.fromList [(heraldMemberEpoch member, member) | member <- members]
    genesisMembership =
      case NonEmpty.nonEmpty (fmap heraldMemberEpoch members) of
        Nothing -> error "checked Herald genesis has an empty membership"
        Just epochs ->
          either
            (error . ("checked Herald membership invariant: " <>) . show)
            id
            (genesisHeraldMembershipGeneration (checkedSystemId genesis) epochs)
    processMap =
      Map.fromList
        [ (appliedProcessEpochId bootstrap, bootstrap)
        | bootstrap <- checkedInitialBootstraps bootstraps
        ]

-- | Install the checked immutable Oracle bootstrap before any control entry.
-- Later configuration authority comes only from the canonical watch stream.
initializeOracleVoters :: Voter.VoterConfiguration -> [Voter.OracleReplicaRegistration] -> State -> Either OracleProjectionFault State
initializeOracleVoters configuration registrations state = do
  unless
    (state.controlIndex == controlIndex 0 && Voter.voterConfigurationControlIndex configuration == controlIndex 0 && all ((== controlIndex 0) . Voter.replicaRegistrationControlIndex) registrations)
    (Left (AppliedEntryVoterProjectionMismatch state.controlIndex))
  let replicas = Map.fromList [(Voter.replicaRegistrationNode registration, registration) | registration <- registrations]
  pure
    state
      { initialVoterConfiguration = Just configuration,
        voterConfiguration = Just configuration,
        initialOracleReplicas = replicas,
        oracleReplicas = replicas
      }

oracleViewVoterConfiguration :: View -> Maybe Voter.VoterConfiguration
oracleViewVoterConfiguration = (.viewVoterConfiguration)

oracleViewOracleReplicas :: View -> [Voter.OracleReplicaRegistration]
oracleViewOracleReplicas = Map.elems . (.viewOracleReplicas)

oracleViewVoterChanges :: View -> [Voter.VoterChange]
oracleViewVoterChanges = Map.elems . (.viewVoterChanges)

oracleViewVoterChange :: Voter.VoterChangeId -> View -> Maybe Voter.VoterChange
oracleViewVoterChange change = Map.lookup change . (.viewVoterChanges)

oracleViewPendingVoterChange :: View -> Maybe Voter.VoterChange
oracleViewPendingVoterChange = (.viewPendingVoterChange)

oracleView :: State -> View
oracleView state =
  View
    { viewSystemId = state.systemId,
      viewControlIndex = state.controlIndex,
      viewVoterConfiguration = state.voterConfiguration,
      viewOracleReplicas = state.oracleReplicas,
      viewVoterChanges = state.voterChanges,
      viewPendingVoterChange = state.pendingVoterChange,
      viewLocalHeraldEpoch = state.localHeraldEpoch,
      viewCurrentHeraldMembership = state.currentHeraldMembership,
      viewHeraldMembershipHistory = state.heraldMembershipHistory,
      viewActiveHeralds = currentActiveHeralds state,
      viewHeraldAdmissions = state.heraldAdmissions,
      viewActiveHeraldEpochs = currentActiveHeraldEpochSet state,
      viewInitialProjectionDigest = state.initialProjectionDigest,
      viewGenesisProcesses = state.genesisProcesses,
      viewStartedProcesses = state.startedProcesses,
      viewEndedProcesses = state.endedProcesses,
      viewLabelWorkflows = state.labelWorkflows,
      viewCompletedLabelWorkflows = state.completedLabelWorkflows,
      viewFailureProbes = state.failureProbes,
      viewDisappearanceProbes = state.disappearanceProbes
    }

projectedHeraldAdmissions :: State -> [Admission.HeraldAdmissionRecord]
projectedHeraldAdmissions state = Map.elems state.heraldAdmissions

oracleViewHeraldAdmission :: Membership.HeraldAdmissionId -> View -> Maybe Admission.HeraldAdmissionRecord
oracleViewHeraldAdmission admission view = Map.lookup admission view.viewHeraldAdmissions

oracleViewPendingHeraldAdmission :: View -> Maybe Admission.HeraldAdmissionRecord
oracleViewPendingHeraldAdmission view = find pending (Map.elems view.viewHeraldAdmissions)
  where
    pending record = case Admission.admissionRecordPhase record of
      Admission.AdmissionActivated {} -> False
      Admission.AdmissionCancelled {} -> False
      _ -> True

oracleViewSystemId :: View -> SystemId
oracleViewSystemId view = view.viewSystemId

oracleViewAppliedBootstraps :: View -> [AppliedProcessBootstrap]
oracleViewAppliedBootstraps view = Map.elems view.viewGenesisProcesses

oracleViewControlIndex :: View -> ControlIndex
oracleViewControlIndex view = view.viewControlIndex

oracleViewLocalHeraldEpoch :: View -> HeraldEpoch
oracleViewLocalHeraldEpoch view = view.viewLocalHeraldEpoch

oracleViewCurrentHeraldMembership :: View -> HeraldMembershipGeneration
oracleViewCurrentHeraldMembership view = view.viewCurrentHeraldMembership

oracleViewCurrentHeraldMembershipId :: View -> HeraldMembershipGenerationId
oracleViewCurrentHeraldMembershipId =
  heraldMembershipGenerationId . oracleViewCurrentHeraldMembership

oracleViewHeraldMembershipHistory :: View -> [HeraldMembershipGeneration]
oracleViewHeraldMembershipHistory =
  NonEmpty.toList . Membership.heraldMembershipHistoryGenerations . oracleViewHeraldMembershipHistoryChecked

oracleViewHeraldMembershipHistoryChecked :: View -> HeraldMembershipHistory
oracleViewHeraldMembershipHistoryChecked view = view.viewHeraldMembershipHistory

singletonMembershipHistory :: HeraldMembershipGeneration -> HeraldMembershipHistory
singletonMembershipHistory generation =
  either
    (error . ("checked genesis membership history invariant: " <>) . show)
    id
    (Membership.heraldMembershipHistory (generation NonEmpty.:| []))

-- | Admit ancestry only through the exact ordered control history retained by
-- this projection. A decoded generation or a smaller member set is insufficient.
oracleViewHeraldMembershipLineage ::
  HeraldMembershipGenerationId -> HeraldMembershipGenerationId -> View -> Maybe HeraldMembershipLineage
oracleViewHeraldMembershipLineage origin target view =
  either (const Nothing) Just (Membership.heraldMembershipLineage origin target (oracleViewHeraldMembershipHistoryChecked view))

oracleViewHeraldMembershipById ::
  HeraldMembershipGenerationId ->
  View ->
  Maybe HeraldMembershipGeneration
oracleViewHeraldMembershipById requested =
  find ((== requested) . heraldMembershipGenerationId)
    . oracleViewHeraldMembershipHistory

-- | Resolve the exact membership generation effective at one already-applied
-- Oracle control coordinate. This is suitable only when that coordinate is
-- the fact's actual admission coordinate (for example, a configured Start);
-- unrelated control prerequisites must retain an explicit generation ID.
oracleViewHeraldMembershipAt ::
  ControlIndex ->
  View ->
  Maybe HeraldMembershipGeneration
oracleViewHeraldMembershipAt requested view
  | requested > view.viewControlIndex = Nothing
  | otherwise = foldl select Nothing (oracleViewHeraldMembershipHistory view)
  where
    select selected generation =
      case Membership.heraldMembershipGenerationChangeControlIndex generation of
        Nothing -> Just generation
        Just activation
          | activation <= requested -> Just generation
          | otherwise -> selected

oracleViewActiveHeralds :: View -> [HeraldMember]
oracleViewActiveHeralds view = view.viewActiveHeralds

oracleViewActiveHeraldEpochs :: View -> [HeraldEpoch]
oracleViewActiveHeraldEpochs view = Set.toAscList view.viewActiveHeraldEpochs

oracleViewLocalHeraldIsCurrent :: View -> Bool
oracleViewLocalHeraldIsCurrent view =
  oracleViewIsActiveHerald view.viewLocalHeraldEpoch view

oracleViewInitialProjectionDigest :: View -> InitialProjectionDigest
oracleViewInitialProjectionDigest view = view.viewInitialProjectionDigest

oracleViewIsActiveHerald :: HeraldEpoch -> View -> Bool
oracleViewIsActiveHerald herald view = Set.member herald view.viewActiveHeraldEpochs

currentActiveHeraldEpochSet :: State -> Set HeraldEpoch
currentActiveHeraldEpochSet =
  Set.fromList
    . NonEmpty.toList
    . heraldMembershipGenerationActiveHeraldEpochs
    . (.currentHeraldMembership)

currentActiveHeralds :: State -> [HeraldMember]
currentActiveHeralds state =
  [ member
  | epoch <- Set.toAscList (currentActiveHeraldEpochSet state),
    Just member <- [Map.lookup epoch state.heraldCatalogue]
  ]

oracleViewProcessResidence :: ProcessEpochId -> View -> Maybe HeraldEpoch
oracleViewProcessResidence process view =
  case Map.lookup process view.viewGenesisProcesses of
    Just bootstrap -> Just (appliedProcessResidence bootstrap)
    Nothing ->
      processStartResidence . projectedStartedProcessBootstrap
        <$> Map.lookup process view.viewStartedProcesses

oracleViewProcessIsLive :: ProcessEpochId -> View -> Bool
oracleViewProcessIsLive process view =
  maybe
    False
    (const (Map.notMember process view.viewEndedProcesses))
    (oracleViewProcessResidence process view)

oracleViewEndedProcess :: ProcessEpochId -> View -> Maybe ProjectedEndedProcess
oracleViewEndedProcess process view = Map.lookup process view.viewEndedProcesses

oracleViewLabelWorkflowCompleted :: LabelDecisionId -> View -> Bool
oracleViewLabelWorkflowCompleted decision view =
  Map.member decision view.viewCompletedLabelWorkflows

oracleViewLabelWorkflow :: LabelDecisionId -> View -> Maybe ProjectedLabelWorkflow
oracleViewLabelWorkflow decision view = Map.lookup decision view.viewLabelWorkflows

oracleViewFailureProbe :: HeraldFailureProbeId -> View -> Maybe ProjectedFailureProbe
oracleViewFailureProbe probe view = Map.lookup probe view.viewFailureProbes

-- | A local voter may report only after it has projected this exact Open and
-- before Oracle has terminalized the probe.
oracleViewMayReportFailureProbe :: HeraldFailureProbeId -> View -> Bool
oracleViewMayReportFailureProbe probe view =
  case oracleViewFailureProbe probe view of
    Just projected ->
      projected.terminal == Nothing
        && projected.generation == oracleViewCurrentHeraldMembershipId view
    Nothing -> False

oracleViewAuthorizesProcess :: AppliedProcessBootstrap -> View -> Bool
oracleViewAuthorizesProcess bootstrap view =
  Map.lookup (appliedProcessEpochId bootstrap) view.viewGenesisProcesses
    == Just bootstrap

oracleViewStartedProcess :: ProcessEpochId -> View -> Maybe ProjectedStartedProcess
oracleViewStartedProcess process view = Map.lookup process view.viewStartedProcesses

oracleViewSatisfiesPrerequisite :: ControlIndex -> View -> Bool
oracleViewSatisfiesPrerequisite prerequisite view =
  prerequisite <= view.viewControlIndex

-- | Resolve process residence at an exact retained control prefix.  Starts
-- and Ends after the requested coordinate are deliberately ignored.
oracleProjectionProcessResidenceAt ::
  ControlIndex -> ProcessEpochId -> State -> Maybe HeraldEpoch
oracleProjectionProcessResidenceAt requested process state
  | requested > state.controlIndex = Nothing
  | otherwise =
      case Map.lookup process state.genesisProcesses of
        Just bootstrap -> Just (appliedProcessResidence bootstrap)
        Nothing -> do
          started <- Map.lookup process state.startedProcesses
          if projectedStartedProcessControlIndex started <= requested
            then
              Just
                ( processStartResidence
                    (projectedStartedProcessBootstrap started)
                )
            else Nothing

oracleProjectionProcessIsLiveAt ::
  ControlIndex -> ProcessEpochId -> State -> Bool
oracleProjectionProcessIsLiveAt requested process state =
  case oracleProjectionProcessResidenceAt requested process state of
    Nothing -> False
    Just _ -> case Map.lookup process state.endedProcesses of
      Nothing -> True
      Just ended -> projectedEndedProcessControlIndex ended > requested

-- | Latest exact released overlay for an object at a retained prefix.  Every
-- released workflow keeps both its normalized prepared object and the opaque
-- Oracle-produced overlay, so no authority is re-derived from current state.
oracleProjectionReleasedLabelAt ::
  ControlIndex -> GlobalObjectId -> State -> Maybe ReleasedLabelOverlay
oracleProjectionReleasedLabelAt requested object state =
  snd <$> foldr newest Nothing candidates
  where
    candidates =
      [ (index, overlay)
      | workflow <- Map.elems state.labelWorkflows,
        Just facts <- [workflow.preparedFacts],
        preparedLabelFactsObjectId facts == object,
        Just (ProjectedLabelReleased index overlay _) <- [workflow.terminal],
        index <= requested
      ]

    newest candidate Nothing = Just candidate
    newest candidate@(candidateIndex, _) incumbent@(Just (incumbentIndex, _))
      | candidateIndex > incumbentIndex = Just candidate
      | otherwise = incumbent

projectedProcessEpochs :: State -> [ProcessEpochId]
projectedProcessEpochs state =
  Set.toAscList
    (Map.keysSet state.genesisProcesses `Set.union` Map.keysSet state.startedProcesses)

-- | Enumerate the fully materialized index-zero bootstrap set. A live Start is
-- intentionally retained separately because its control index does not grant
-- authority to the configured roots.
projectedBootstraps :: State -> [(ProcessEpochId, AppliedProcessBootstrap)]
projectedBootstraps state = Map.toAscList state.genesisProcesses

projectedStartedProcesses :: State -> [(ProcessEpochId, ProjectedStartedProcess)]
projectedStartedProcesses state = Map.toAscList state.startedProcesses

projectedEndedProcesses :: State -> [(ProcessEpochId, ProjectedEndedProcess)]
projectedEndedProcesses state = Map.toAscList state.endedProcesses

projectedLabelWorkflows :: State -> [(LabelDecisionId, ProjectedLabelWorkflow)]
projectedLabelWorkflows state = Map.toAscList state.labelWorkflows

data TopologyControlCheckpointProblem
  = TopologyControlCheckpointAhead ControlIndex ControlIndex
  deriving stock (Eq, Show)

-- | Canonical system-wide topology-affecting control projection at one exact
-- retained prefix. Rejected commands and request/receipt metadata advance the
-- control cursor but contribute no semantic checkpoint entry.
--
-- Checked index-zero processes remain committed by InitialProjectionDigest in
-- the distinguished genesis topology-cut predecessor and are therefore not
-- re-encoded here.
topologyControlCheckpointAt ::
  ControlIndex ->
  State ->
  Either TopologyControlCheckpointProblem ByteString
topologyControlCheckpointAt requested state
  | requested > state.controlIndex =
      Left (TopologyControlCheckpointAhead requested state.controlIndex)
  | otherwise = Right (Serialize.runPut (putCheckpoint events))
  where
    starts =
      sortOn
        (\(process, started) -> (projectedStartedProcessControlIndex started, process))
        [ (process, started)
        | (process, started) <- Map.toAscList state.startedProcesses,
          projectedStartedProcessControlIndex started <= requested
        ]
    ends =
      [ (process, ended)
      | (process, ended) <- Map.toAscList state.endedProcesses,
        projectedEndedProcessControlIndex ended <= requested
      ]
    releases =
      [ ( decision,
          index,
          preparedLabelFactsObjectId facts,
          preparedLabelFactsProposedOutcome facts
        )
      | (decision, workflow) <- Map.toAscList state.labelWorkflows,
        Just (ProjectedLabelReleased index _ _) <- [workflow.terminal],
        index <= requested,
        Just facts <- [workflow.preparedFacts]
      ]
    events =
      sortOn
        topologyEventOrder
        ( fmap (uncurry TopologyProcessStarted) starts
            <> fmap (uncurry TopologyProcessEnded) ends
            <> fmap
              ( \(decision, index, object, outcome) ->
                  TopologyLabelReleased decision index object outcome
              )
              releases
            <> [ TopologyDisappearanceResolved probe outcome
               | (probe, projected) <- Map.toAscList state.disappearanceProbes,
                 Just (ProjectedDisappearanceResolved outcome index) <- [projectedDisappearanceTerminal projected],
                 index <= requested
               ]
        )

data TopologyProcessEvent
  = TopologyProcessStarted ProcessEpochId ProjectedStartedProcess
  | TopologyProcessEnded ProcessEpochId ProjectedEndedProcess
  | TopologyLabelReleased
      LabelDecisionId
      ControlIndex
      GlobalObjectId
      ReleasedLabelState
  | TopologyDisappearanceResolved DisappearanceProbeId DisappearanceResolutionOutcome

topologyEventOrder :: TopologyProcessEvent -> (ControlIndex, Word8, ByteString)
topologyEventOrder event = case event of
  TopologyProcessStarted process started ->
    (projectedStartedProcessControlIndex started, 0, processEpochIdBytes process)
  TopologyProcessEnded process ended ->
    (projectedEndedProcessControlIndex ended, 1, processEpochIdBytes process)
  TopologyLabelReleased decision index _ _ ->
    (index, 2, labelDecisionIdBytes decision)
  TopologyDisappearanceResolved probe outcome ->
    (disappearanceResolutionControlIndex outcome, 3, disappearanceProbeIdBytes probe)

putCheckpoint :: [TopologyProcessEvent] -> Serialize.Put
putCheckpoint events = do
  putSizedBytes (ByteString.Char8.pack "ECLIPS-TOPOLOGY-CONTROL-CHECKPOINT")
  Serialize.putWord64be (fromIntegral (length events))
  mapM_ putTopologyProcessEvent events

putTopologyProcessEvent :: TopologyProcessEvent -> Serialize.Put
putTopologyProcessEvent event = case event of
  TopologyProcessStarted process started -> do
    Serialize.putWord8 0
    putStartedProcess (process, started)
  TopologyProcessEnded process ended -> do
    Serialize.putWord8 1
    Serialize.putWord64be (controlIndexWord64 (projectedEndedProcessControlIndex ended))
    Serialize.putByteString (processEpochIdBytes process)
    putSizedBytes (processEndReasonCanonicalBytes (projectedEndedProcessReason ended))
  TopologyLabelReleased decision index object outcome -> do
    Serialize.putWord8 2
    Serialize.putWord64be (controlIndexWord64 index)
    Serialize.putByteString (labelDecisionIdBytes decision)
    Serialize.putByteString (globalObjectIdBytes object)
    case releasedLabelStateView outcome of
      ReleasedDeletedView generation -> do
        Serialize.putWord8 0
        Serialize.putWord64be generation
      ReleasedLabelView label -> do
        Serialize.putWord8 1
        putSizedBytes
          (canonicalValueByteString (canonicalValueBytes (labelValue label)))
  TopologyDisappearanceResolved probe outcome -> do
    Serialize.putWord8 3
    Serialize.putByteString (disappearanceProbeIdBytes probe)
    putSizedBytes (disappearanceResolutionOutcomeCanonicalBytes outcome)

putStartedProcess :: (ProcessEpochId, ProjectedStartedProcess) -> Serialize.Put
putStartedProcess (process, started) = do
  let bootstrap = projectedStartedProcessBootstrap started
  Serialize.putWord64be
    (controlIndexWord64 (projectedStartedProcessControlIndex started))
  Serialize.putByteString
    (processIdBytes (processStartProcessId bootstrap))
  Serialize.putByteString (processEpochIdBytes process)
  Serialize.putByteString
    (heraldEpochBytes (processStartResidence bootstrap))
putSizedBytes :: ByteString -> Serialize.Put
putSizedBytes bytes = do
  Serialize.putWord64be (fromIntegral (ByteString.length bytes))
  Serialize.putByteString bytes

-- | Exact canonical evidence retained for equal-overlap classification.
appliedEntryEvidence :: ControlIndex -> State -> Maybe CanonicalAppliedOracleEntry
appliedEntryEvidence index state = Map.lookup index state.appliedEntries

-- | Exact contiguous canonical tail through the current cursor. Covered
-- coordinates and isolated old pins cannot supply missing replay bytes.
-- Inspect retained rows instead of enumerating an absent numeric interval.
retainedControlSuffixAfter :: ControlIndex -> State -> Maybe [CanonicalAppliedOracleEntry]
retainedControlSuffixAfter after state
  | after > state.controlIndex = Nothing
  | otherwise =
      let retained = Map.toAscList (snd (Map.split after state.appliedEntries))
       in if contiguous after retained then Just (map snd retained) else Nothing
  where
    contiguous previous [] = previous == state.controlIndex
    contiguous previous ((index, _) : rest) =
      index == nextControlIndex previous && contiguous index rest

-- | Inspect exact evidence already installed in this Herald's projection.
-- This historical query searches the archive; local request-result consumers
-- use the Oracle client's request index instead.
appliedEntryForRequest ::
  OracleClientRequestId ->
  State ->
  Maybe CanonicalAppliedOracleEntry
appliedEntryForRequest request state =
  snd
    <$> find
      ( (== Just request)
          . fmap appliedEntryRequestId
          . appliedEntryCommand
          . canonicalAppliedOracleEntryValue
          . snd
      )
      (Map.toAscList state.appliedEntries)

-- | The post-state digest claimed by the greatest applied entry. Index zero has
-- no watch entry; a default-mode Oracle also omits this diagnostic witness.
projectedOracleStateDigest :: State -> Maybe OracleStateDigest
projectedOracleStateDigest state = state.latestCanonicalStateDigest

-- | Canonical progress acknowledgements do not advance semantic work. Rejected
-- commands, configurations and detached membership advances still do.
projectionLastSemanticControlIndex :: State -> ControlIndex
projectionLastSemanticControlIndex state = state.lastSemanticControlIndex

-- | Exact retained bytes take precedence over a checked covered prefix.
data AppliedEntryClassification
  = AppliedEntryNext ControlIndex
  | AppliedEntryExactDuplicate ControlIndex
  | AppliedEntryCoveredByBase ControlIndex
  | AppliedEntryGap ControlIndex ControlIndex
  | AppliedEntryUnequalConflict ControlIndex
  deriving stock (Eq, Show)

classifyAppliedEntry :: CanonicalAppliedOracleEntry -> State -> AppliedEntryClassification
classifyAppliedEntry canonical state
  | received == nextControlIndex state.controlIndex = AppliedEntryNext received
  | received > nextControlIndex state.controlIndex =
      AppliedEntryGap (nextControlIndex state.controlIndex) received
  | otherwise = case Map.lookup received state.appliedEntries of
      Just retained
        | retained == canonical -> AppliedEntryExactDuplicate received
        | otherwise -> AppliedEntryUnequalConflict received
      Nothing
        | received > controlIndex 0 && received <= state.canonicalEvidenceCoveredThrough -> AppliedEntryCoveredByBase received
        | otherwise -> AppliedEntryUnequalConflict received
  where
    received = appliedEntryControlIndex (canonicalAppliedOracleEntryValue canonical)

-- | Contradictions in a newly contiguous canonical entry.
data OracleProjectionFault
  = AppliedEntryUnequalConflictFault ControlIndex
  | AppliedEntryCanonicalEvidenceMismatch ControlIndex
  | AppliedEntryOriginMismatch ControlIndex
  | AppliedEntryVoterProjectionMismatch ControlIndex
  | AppliedEntryReceiptRequestMismatch OracleClientRequestId OracleClientRequestId
  | AppliedEntryReceiptDigestMismatch OracleCommandDigest OracleCommandDigest
  | AppliedEntryReceiptIndexMismatch ControlIndex ControlIndex
  | AppliedEntryRejectedWithEvents ControlIndex
  | AppliedEntryMultipleMembershipAdvances ControlIndex
  | AppliedEntryMembershipAdvanceFault ControlIndex MembershipAdvanceProblem
  | FailureProbeAlreadyProjected HeraldFailureProbeId
  | FailureProbeEventWithoutOpen HeraldFailureProbeId
  | StartedProcessAlreadyProjected ProcessEpochId
  | StartedProcessIdAlreadyUsed ProcessId
  | StartedProcessOriginMismatch ProcessEpochId
  | StartedProcessLifecycleMismatch ProcessEpochId
  | StartedProcessIndexMismatch ControlIndex ControlIndex
  | StartedProcessRequestHomeMismatch HeraldEpoch HeraldEpoch
  | EndedProcessUnknown ProcessEpochId
  | EndedProcessAlreadyProjected ProcessEpochId
  | EndedProcessIndexMismatch ControlIndex ControlIndex
  | EndedProcessRequestHomeMismatch HeraldEpoch HeraldEpoch
  | LabelWorkflowAlreadyProjected LabelDecisionId
  | LabelWorkflowMembershipGenerationMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | LabelWorkflowMembershipSetMismatch LabelDecisionId
  | LabelWorkflowEventWithoutOpen LabelDecisionId
  | LabelWorkflowCompletionConflict LabelDecisionId
  | DisappearanceProjectionConflict DisappearanceProbeId
  | DisappearanceProjectionEventWithoutOpen DisappearanceProbeId
  | DisappearanceProjectionCompoundMismatch ControlIndex
  | AppliedEntryAdmissionMismatch ControlIndex
  deriving stock (Eq, Show)

-- | Repeatable preparation containing either the exact successor or the
-- unchanged predecessor for duplicate/gap classifications.
data PreparedAppliedEntry = PreparedAppliedEntry
  { classification :: AppliedEntryClassification,
    successor :: State
  }

prepareAppliedEntry ::
  CanonicalAppliedOracleEntry ->
  State ->
  Either OracleProjectionFault PreparedAppliedEntry
prepareAppliedEntry canonical state =
  case classification of
    AppliedEntryNext _ ->
      PreparedAppliedEntry classification <$> validateAndApplyNext canonical state
    AppliedEntryExactDuplicate _ ->
      Right (PreparedAppliedEntry classification state)
    AppliedEntryCoveredByBase _ ->
      Right (PreparedAppliedEntry classification state)
    AppliedEntryGap _ _ ->
      Right (PreparedAppliedEntry classification state)
    AppliedEntryUnequalConflict index ->
      Left (AppliedEntryUnequalConflictFault index)
  where
    classification = classifyAppliedEntry canonical state

preparedAppliedEntryClassification :: PreparedAppliedEntry -> AppliedEntryClassification
preparedAppliedEntryClassification prepared = prepared.classification

commitAppliedEntry :: PreparedAppliedEntry -> State
commitAppliedEntry prepared = prepared.successor

-- | Classification of a successfully prepared staged membership observation.
-- A duplicate is exact retained evidence and commits the unchanged state.
data MembershipAdvanceDisposition
  = MembershipAdvanceApplied
  | MembershipAdvanceExactDuplicate
  deriving stock (Eq, Show)

-- | Rejections at the boundary between detached Step-15 Oracle evidence and
-- the ordinary Herald projection. No constructor grants live EORC dispatch;
-- the checked seam only projects an already established contiguous fact.
data MembershipAdvanceProblem
  = MembershipAdvanceControlGap ControlIndex ControlIndex
  | MembershipAdvanceControlConflict ControlIndex
  | MembershipAdvanceRetirementSuccessorRequired
  | MembershipAdvanceControlIndexMismatch ControlIndex ControlIndex
  | MembershipAdvanceStalePredecessor
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | MembershipAdvanceCatalogueMismatch [HeraldEpoch] [HeraldEpoch]
  | MembershipAdvanceSuccessorMismatch
  | MembershipAdvanceProcessEndsNotCanonical
  | MembershipAdvanceProcessEndIndexMismatch
      ProcessEpochId
      ControlIndex
      ControlIndex
  | MembershipAdvanceProcessEndReasonMismatch ProcessEpochId
  | MembershipAdvanceProcessNotLive ProcessEpochId
  | MembershipAdvanceProcessResidenceMismatch
      ProcessEpochId
      HeraldEpoch
      HeraldEpoch
  | MembershipAdvanceResidentProcessSetMismatch
      [ProcessEpochId]
      [ProcessEpochId]
  deriving stock (Eq, Show)

-- | Opaque repeatable preparation. Only 'commitMembershipAdvance' can expose
-- its checked successor state.
data PreparedMembershipAdvance = PreparedMembershipAdvance
  { disposition :: MembershipAdvanceDisposition,
    membershipSuccessor :: HeraldMembershipGeneration,
    projectedProcessEnds :: [ProjectedEndedProcess],
    stateSuccessor :: State
  }

prepareMembershipAdvance ::
  ControlIndex ->
  HeraldMembershipGeneration ->
  [MembershipAdvanceProcessEnd] ->
  State ->
  Either MembershipAdvanceProblem PreparedMembershipAdvance
prepareMembershipAdvance index successor suppliedEnds state =
  case Map.lookup index state.membershipAdvances of
    Just retained
      | retained == suppliedEvidence ->
          Right
            PreparedMembershipAdvance
              { disposition = MembershipAdvanceExactDuplicate,
                membershipSuccessor = successor,
                projectedProcessEnds = projectedEnds suppliedEnds,
                stateSuccessor = state
              }
      | otherwise -> Left (MembershipAdvanceControlConflict index)
    Nothing
      | index <= state.controlIndex ->
          Left (MembershipAdvanceControlConflict index)
      | index /= expectedIndex ->
          Left (MembershipAdvanceControlGap expectedIndex index)
      | otherwise -> do
          next <- applyNewMembershipAdvance index successor suppliedEnds state
          pure
            PreparedMembershipAdvance
              { disposition = MembershipAdvanceApplied,
                membershipSuccessor = successor,
                projectedProcessEnds = projectedEnds suppliedEnds,
                stateSuccessor = next
              }
  where
    expectedIndex = nextControlIndex state.controlIndex
    suppliedEvidence = MembershipAdvanceEvidence index successor suppliedEnds

preparedMembershipAdvanceDisposition ::
  PreparedMembershipAdvance -> MembershipAdvanceDisposition
preparedMembershipAdvanceDisposition prepared = prepared.disposition

preparedMembershipAdvanceSuccessor ::
  PreparedMembershipAdvance -> HeraldMembershipGeneration
preparedMembershipAdvanceSuccessor prepared = prepared.membershipSuccessor

preparedMembershipAdvanceProcessEnds ::
  PreparedMembershipAdvance -> [ProjectedEndedProcess]
preparedMembershipAdvanceProcessEnds prepared = prepared.projectedProcessEnds

commitMembershipAdvance :: PreparedMembershipAdvance -> State
commitMembershipAdvance prepared = prepared.stateSuccessor

applyNewMembershipAdvance ::
  ControlIndex ->
  HeraldMembershipGeneration ->
  [MembershipAdvanceProcessEnd] ->
  State ->
  Either MembershipAdvanceProblem State
applyNewMembershipAdvance index successor suppliedEnds state = do
  advanced <- applyCanonicalMembershipAdvance index successor suppliedEnds state
  pure
    advanced
      { Eclips.Herald.OracleProjection.State.controlIndex = index,
        lastSemanticControlIndex = index,
        Eclips.Herald.OracleProjection.State.membershipAdvances =
          Map.insert
            index
            (MembershipAdvanceEvidence index successor suppliedEnds)
            state.membershipAdvances
      }

-- | Apply the semantic contents of a membership advance. The caller decides
-- whether its evidence is the legacy detached entry or an ordinary canonical
-- watch entry; therefore this helper deliberately claims neither evidence map
-- nor the projection cursor.
applyCanonicalMembershipAdvance ::
  ControlIndex ->
  HeraldMembershipGeneration ->
  [MembershipAdvanceProcessEnd] ->
  State ->
  Either MembershipAdvanceProblem State
applyCanonicalMembershipAdvance index successor suppliedEnds state
  | Just admission <- Membership.heraldMembershipGenerationAdmissionId successor = do
      record <- maybe (Left MembershipAdvanceSuccessorMismatch) Right (Map.lookup admission state.heraldAdmissions)
      let applicant = Admission.admissionManifestHeraldEpoch (Admission.admissionRecordManifest record)
      unless
        (null suppliedEnds && Admission.admissionRecordPhase record == Admission.AdmissionReady)
        (Left MembershipAdvanceSuccessorMismatch)
      expected <-
        either
          (const (Left MembershipAdvanceSuccessorMismatch))
          Right
          (Membership.admitHeraldMembershipGeneration index admission applicant state.currentHeraldMembership)
      unless (expected == successor) (Left MembershipAdvanceSuccessorMismatch)
      history <-
        either
          (const (Left MembershipAdvanceSuccessorMismatch))
          Right
          (Membership.appendHeraldMembershipGeneration successor state.heraldMembershipHistory)
      pure state {currentHeraldMembership = successor, heraldMembershipHistory = history}
applyCanonicalMembershipAdvance index successor suppliedEnds state = do
  retired <-
    maybe
      (Left MembershipAdvanceRetirementSuccessorRequired)
      Right
      (heraldMembershipGenerationRetiredHeraldEpoch successor)
  let suppliedCatalogue =
        Set.insert
          retired
          ( Set.fromList
              ( NonEmpty.toList
                  (heraldMembershipGenerationActiveHeraldEpochs successor)
              )
          )
      expectedCatalogue =
        Set.fromList
          (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs state.currentHeraldMembership))
  unless
    (suppliedCatalogue == expectedCatalogue)
    ( Left
        ( MembershipAdvanceCatalogueMismatch
            (Set.toAscList expectedCatalogue)
            (Set.toAscList suppliedCatalogue)
        )
    )
  let expectedPredecessor = heraldMembershipGenerationId state.currentHeraldMembership
  suppliedPredecessor <-
    maybe
      (Left MembershipAdvanceRetirementSuccessorRequired)
      Right
      (heraldMembershipGenerationPredecessor successor)
  unless
    (suppliedPredecessor == expectedPredecessor)
    (Left (MembershipAdvanceStalePredecessor suppliedPredecessor expectedPredecessor))
  embeddedIndex <-
    maybe
      (Left MembershipAdvanceRetirementSuccessorRequired)
      Right
      (heraldMembershipGenerationRetirementControlIndex successor)
  unless
    (embeddedIndex == index)
    (Left (MembershipAdvanceControlIndexMismatch index embeddedIndex))
  retirement <-
    maybe
      (Left MembershipAdvanceRetirementSuccessorRequired)
      Right
      (heraldMembershipGenerationRetirementId successor)
  expectedSuccessor <-
    either
      (const (Left MembershipAdvanceSuccessorMismatch))
      Right
      (retireHeraldMembershipGeneration index retirement retired state.currentHeraldMembership)
  unless
    (successor == expectedSuccessor)
    (Left MembershipAdvanceSuccessorMismatch)
  successorHistory <-
    either
      (const (Left MembershipAdvanceSuccessorMismatch))
      Right
      (Membership.appendHeraldMembershipGeneration successor state.heraldMembershipHistory)
  validateMembershipProcessEnds index retired suppliedEnds state
  let ends = projectedEnds suppliedEnds
      installedEnds =
        Map.fromList
          [ (projectedEndedProcessEpoch ended, ended)
          | ended <- ends
          ]
  pure
    state
      { Eclips.Herald.OracleProjection.State.currentHeraldMembership = successor,
        Eclips.Herald.OracleProjection.State.heraldMembershipHistory =
          successorHistory,
        Eclips.Herald.OracleProjection.State.endedProcesses =
          Map.union installedEnds state.endedProcesses
      }

validateMembershipProcessEnds ::
  ControlIndex ->
  HeraldEpoch ->
  [MembershipAdvanceProcessEnd] ->
  State ->
  Either MembershipAdvanceProblem ()
validateMembershipProcessEnds index retired supplied state = do
  let suppliedProcesses = fmap membershipAdvanceProcessEndEpoch supplied
      sortedSupplied = sortOn membershipAdvanceProcessEndEpoch supplied
  unless
    ( supplied == sortedSupplied
        && length suppliedProcesses == Set.size (Set.fromList suppliedProcesses)
    )
    (Left MembershipAdvanceProcessEndsNotCanonical)
  mapM_ validateOne supplied
  let expectedProcesses =
        sort
          [ process
          | process <- projectedProcessEpochs state,
            projectedProcessResidence process state == Just retired,
            Map.notMember process state.endedProcesses
          ]
  unless
    (suppliedProcesses == expectedProcesses)
    ( Left
        ( MembershipAdvanceResidentProcessSetMismatch
            expectedProcesses
            suppliedProcesses
        )
    )
  where
    validateOne ended = do
      let process = membershipAdvanceProcessEndEpoch ended
          suppliedIndex = membershipAdvanceProcessEndControlIndex ended
      unless
        (suppliedIndex == index)
        (Left (MembershipAdvanceProcessEndIndexMismatch process index suppliedIndex))
      unless
        (processEndReasonIsHeraldRetirement (membershipAdvanceProcessEndReason ended))
        (Left (MembershipAdvanceProcessEndReasonMismatch process))
      unless
        (Map.notMember process state.endedProcesses)
        (Left (MembershipAdvanceProcessNotLive process))
      residence <-
        maybe
          (Left (MembershipAdvanceProcessNotLive process))
          Right
          (projectedProcessResidence process state)
      unless
        (residence == retired)
        (Left (MembershipAdvanceProcessResidenceMismatch process retired residence))

projectedEnds :: [MembershipAdvanceProcessEnd] -> [ProjectedEndedProcess]
projectedEnds = fmap toProjected
  where
    toProjected (MembershipAdvanceProcessEnd process index reason) =
      ProjectedEndedProcess process index reason

validateAndApplyNext ::
  CanonicalAppliedOracleEntry ->
  State ->
  Either OracleProjectionFault State
validateAndApplyNext canonical state = do
  let entry = canonicalAppliedOracleEntryValue canonical
      index = appliedEntryControlIndex entry
      recanonicalized = canonicalizeAppliedOracleEntry entry
  unless
    (canonicalAppliedOracleEntryBytes canonical == canonicalAppliedOracleEntryBytes recanonicalized)
    (Left (AppliedEntryCanonicalEvidenceMismatch index))
  validateReceiptCoherence entry
  successor <- validateAndApplyEvents entry state
  let !semantic = case (appliedEntryCommand entry, appliedEntryOracleProgress entry) of
        (Nothing, Just _) -> state.lastSemanticControlIndex
        _ -> index
      !digest = case appliedEntryPostStateDigest entry of
        Nothing -> Nothing
        Just !value -> Just value
  pure
    successor
      { Eclips.Herald.OracleProjection.State.controlIndex = index,
        lastSemanticControlIndex = semantic,
        latestCanonicalStateDigest = digest,
        Eclips.Herald.OracleProjection.State.appliedEntries =
          Map.insert index canonical state.appliedEntries
      }

validateReceiptCoherence :: AppliedOracleEntry -> Either OracleProjectionFault ()
validateReceiptCoherence entry = case appliedEntryCommand entry of
  Nothing -> Right ()
  Just command -> validateCommandReceipt entry command

validateCommandReceipt :: AppliedOracleEntry -> AppliedOracleCommandEntry -> Either OracleProjectionFault ()
validateCommandReceipt entry command = do
  let receipt = appliedEntryReceipt command
      entryRequest = appliedEntryRequestId command
      receiptRequest = oracleReceiptRequestId receipt
      entryDigest = appliedEntryCommandDigest command
      receiptDigest = oracleReceiptCommandDigest receipt
      entryIndex = appliedEntryControlIndex entry
      receiptIndex = oracleReceiptControlIndex receipt
  unless
    (entryRequest == receiptRequest)
    (Left (AppliedEntryReceiptRequestMismatch entryRequest receiptRequest))
  unless
    (entryDigest == receiptDigest)
    (Left (AppliedEntryReceiptDigestMismatch entryDigest receiptDigest))
  unless
    (entryIndex == receiptIndex)
    (Left (AppliedEntryReceiptIndexMismatch entryIndex receiptIndex))

validateAndApplyEvents ::
  AppliedOracleEntry ->
  State ->
  Either OracleProjectionFault State
validateAndApplyEvents entry state =
  case appliedEntryCommand entry of
    Just command -> validateAndApplyCommandEvents entry command state
    Nothing | Just _ <- appliedEntryReceiptRetirement entry, null (appliedEntryProjectionEvents entry) -> Right state
    Nothing -> case (appliedEntryConfiguration entry, fmap oracleProjectionEventView (appliedEntryProjectionEvents entry)) of
      (Just configuration, [OracleVoterConfigurationChangedView observed, OracleVoterChangeChangedView _])
        | configuration == observed -> foldM (applyProjectionEvent entry) state (appliedEntryProjectionEvents entry)
      _ -> Left (AppliedEntryOriginMismatch (appliedEntryControlIndex entry))

validateAndApplyCommandEvents :: AppliedOracleEntry -> AppliedOracleCommandEntry -> State -> Either OracleProjectionFault State
validateAndApplyCommandEvents entry command state =
  case oracleReceiptResult (appliedEntryReceipt command) of
    OracleAccepted -> do
      validateDisappearanceCompoundEvents entry state
      (withMembership, remaining) <-
        applyCanonicalMembershipEvents entry (appliedEntryProjectionEvents entry) state
      foldM (applyProjectionEvent entry) withMembership remaining
    OracleRejected _
      | null (appliedEntryProjectionEvents entry) -> Right state
      | otherwise ->
          Left (AppliedEntryRejectedWithEvents (appliedEntryControlIndex entry))

applyCanonicalMembershipEvents ::
  AppliedOracleEntry ->
  [OracleProjectionEvent] ->
  State ->
  Either OracleProjectionFault (State, [OracleProjectionEvent])
applyCanonicalMembershipEvents entry events state =
  case [ generation
       | event <- events,
         HeraldMembershipAdvancedView generation <- [oracleProjectionEventView event]
       ] of
    [] -> Right (state, events)
    [generation] -> do
      let retirementEnds =
            [ MembershipAdvanceProcessEnd process endIndex reason
            | event <- events,
              ProcessEpochEndedEventView process endIndex reason <- [oracleProjectionEventView event],
              processEndReasonIsHeraldRetirement reason
            ]
          index = appliedEntryControlIndex entry
      advanced <-
        either
          (Left . AppliedEntryMembershipAdvanceFault index)
          Right
          (applyCanonicalMembershipAdvance index generation retirementEnds state)
      pure (advanced, filter (not . consumedRetirementEvent) events)
    _ -> Left (AppliedEntryMultipleMembershipAdvances (appliedEntryControlIndex entry))
  where
    consumedRetirementEvent event = case oracleProjectionEventView event of
      HeraldMembershipAdvancedView _ -> True
      ProcessEpochEndedEventView _ _ reason -> processEndReasonIsHeraldRetirement reason
      _ -> False

-- | Configuration entries continue the one retained intention. A terminal
-- command retry may repeat an older result while a newer change owns the slot;
-- that replay cannot clear or replace the current pending change.
applyProjectedVoterChange :: AppliedOracleEntry -> Voter.VoterChange -> State -> Either OracleProjectionFault State
applyProjectedVoterChange entry change state = do
  let identifier = Voter.voterChangeId change
      index = appliedEntryControlIndex entry
      failProjection = Left (AppliedEntryVoterProjectionMismatch index)
      pendingIsSame = fmap Voter.voterChangeId state.pendingVoterChange == Just identifier
      unchangedIntent previous =
        Voter.voterChangeExpectedConfiguration previous == Voter.voterChangeExpectedConfiguration change
          && Voter.voterChangeOldBindings previous == Voter.voterChangeOldBindings change
          && Voter.voterChangeNewBindings previous == Voter.voterChangeNewBindings change
          && Voter.voterChangeReason previous == Voter.voterChangeReason change
      install pending = Right state {voterChanges = Map.insert identifier change state.voterChanges, pendingVoterChange = pending}
  case Map.lookup identifier state.voterChanges of
    Just previous | previous == change, Voter.voterChangeControlIndex change <= index -> Right state
    Nothing
      | Voter.voterChangePhase change == Voter.VoterChangePreparing,
        Voter.voterChangeControlIndex change == index,
        Voter.voterChangeIdControlIndex identifier == index,
        state.pendingVoterChange == Nothing,
        appliedEntryConfiguration entry == Nothing,
        maybe True (\configuration -> Voter.voterConfigurationId configuration == Voter.voterChangeExpectedConfiguration change && Voter.voterConfigurationView configuration == Voter.StableVoterConfigurationView (Voter.voterChangeOldBindings change)) state.voterConfiguration ->
          install (Just change)
    Just previous
      | unchangedIntent previous,
        pendingIsSame,
        Voter.voterChangeControlIndex change == index ->
          case (Voter.voterChangePhase previous, Voter.voterChangePhase change, appliedEntryConfiguration entry) of
            (Voter.VoterChangePreparing, Voter.VoterChangeJointCommitted, Just _) -> install (Just change)
            (Voter.VoterChangeJointCommitted, Voter.VoterChangeCompleted, Just _) -> install Nothing
            (Voter.VoterChangeJointCommitted, Voter.VoterExcludedAwaitingHeraldRetirement, Just _) -> install (Just change)
            (Voter.VoterExcludedAwaitingHeraldRetirement, Voter.VoterChangeCompleted, Nothing) -> install Nothing
            (Voter.VoterChangePreparing, Voter.VoterChangeCancelled, Nothing) -> install Nothing
            _ -> failProjection
    _ -> failProjection

configurationMatchesPending :: Voter.VoterConfiguration -> Voter.VoterChange -> Bool
configurationMatchesPending configuration change = case (Voter.voterChangePhase change, Voter.voterConfigurationView configuration) of
  (Voter.VoterChangePreparing, Voter.JointVoterConfigurationView old new) -> old == Voter.voterChangeOldBindings change && new == Voter.voterChangeNewBindings change
  (Voter.VoterChangeJointCommitted, Voter.StableVoterConfigurationView new) -> new == Voter.voterChangeNewBindings change
  _ -> False

applyProjectionEvent ::
  AppliedOracleEntry ->
  State ->
  OracleProjectionEvent ->
  Either OracleProjectionFault State
applyProjectionEvent entry state event = case oracleProjectionEventView event of
  OracleReplicaRegisteredView registration -> do
    let node = Voter.replicaRegistrationNode registration
        index = appliedEntryControlIndex entry
    unless
      (Voter.replicaRegistrationControlIndex registration <= index)
      (Left (AppliedEntryVoterProjectionMismatch index))
    case Map.lookup node state.oracleReplicas of
      Just existing
        | existing /= registration,
          not
            ( Voter.replicaRegistrationContact existing == Nothing
                && Voter.replicaRegistrationHost existing == Voter.replicaRegistrationHost registration
                && Voter.replicaRegistrationControlIndex existing == controlIndex 0
                && Voter.replicaRegistrationContact registration /= Nothing
                && Voter.replicaRegistrationControlIndex registration == index
            ) ->
            Left (AppliedEntryVoterProjectionMismatch index)
      _ -> pure state {oracleReplicas = Map.insert node registration state.oracleReplicas}
  OracleVoterChangeChangedView change -> applyProjectedVoterChange entry change state
  OracleVoterConfigurationChangedView configuration -> do
    let index = appliedEntryControlIndex entry
    unless
      ( appliedEntryConfiguration entry == Just configuration
          && Voter.voterConfigurationControlIndex configuration == index
          && maybe False (configurationMatchesPending configuration) state.pendingVoterChange
          && maybe True (\previous -> Voter.voterConfigurationPredecessor configuration == Just (Voter.voterConfigurationId previous)) state.voterConfiguration
      )
      (Left (AppliedEntryVoterProjectionMismatch index))
    pure state {voterConfiguration = Just configuration}
  HeraldAdmissionChangedView record -> do
    let manifest = Admission.admissionRecordManifest record
        admission = Admission.admissionRecordId record
        index = appliedEntryControlIndex entry
        member = HeraldMember (Admission.admissionManifestHeraldId manifest) (Admission.admissionManifestHeraldEpoch manifest)
    unless
      (Admission.admissionManifestSystem manifest == state.systemId && Admission.admissionRecordChangedIndex record <= index)
      (Left (AppliedEntryAdmissionMismatch index))
    case Map.lookup admission state.heraldAdmissions of
      Nothing ->
        unless
          ( Admission.admissionRecordBeginIndex record == index
              && Admission.admissionRecordPhase record == Admission.AdmissionPreparing
              && Admission.admissionRecordPredecessor record == state.currentHeraldMembership
          )
          (Left (AppliedEntryAdmissionMismatch index))
      Just prior ->
        unless
          ( Admission.admissionRecordManifest prior == manifest
              && Admission.admissionRecordBeginIndex prior == Admission.admissionRecordBeginIndex record
              && Admission.admissionRecordAttempt prior <= Admission.admissionRecordAttempt record
          )
          (Left (AppliedEntryAdmissionMismatch index))
    pure
      state
        { heraldAdmissions = Map.insert admission record state.heraldAdmissions,
          heraldCatalogue = Map.insert (heraldMemberEpoch member) member state.heraldCatalogue
        }
  ProcessStartedView supplied -> do
    started <- validateStartedProcess entry supplied state
    let process =
          processStartProcessEpochId
            (projectedStartedProcessBootstrap started)
    Right
      state
        { Eclips.Herald.OracleProjection.State.startedProcesses =
            Map.insert process started state.startedProcesses
        }
  ProcessEpochEndedEventView process index reason -> do
    ended <- validateEndedProcess entry process index reason state
    Right
      state
        { Eclips.Herald.OracleProjection.State.endedProcesses =
            Map.insert process ended state.endedProcesses
        }
  LabelDecidedView opened outcome digest -> do
    let decision = liveDecisionId opened
        membership = state.currentHeraldMembership
        expectedGeneration = heraldMembershipGenerationId membership
        observedGeneration = liveDecisionMembershipGeneration opened
        workflow = decidedWorkflow opened outcome digest
    unless
      (Map.notMember decision state.labelWorkflows)
      (Left (LabelWorkflowAlreadyProjected decision))
    unless
      (observedGeneration == expectedGeneration)
      (Left (LabelWorkflowMembershipGenerationMismatch expectedGeneration observedGeneration))
    unless
      (liveDecisionMemberSetDigest opened == heraldMembershipGenerationActiveMemberSetDigest membership)
      (Left (LabelWorkflowMembershipSetMismatch decision))
    Right
      state
        { labelWorkflows = Map.insert decision workflow state.labelWorkflows,
          completedLabelWorkflows = case projectedLabelWorkflowTerminal workflow of
            Just ProjectedLabelNotApplied {} -> Map.insert decision digest state.completedLabelWorkflows
            _ -> state.completedLabelWorkflows
        }
  LabelWorkflowCompletedView decision digest -> do
    withCompleted <-
      adjustLabelWorkflow
        decision
        (\workflow -> workflow {phase = LabelWorkflowCompleted})
        state
    case Map.lookup decision state.completedLabelWorkflows of
      Nothing ->
        Right
          withCompleted
            { Eclips.Herald.OracleProjection.State.completedLabelWorkflows =
                Map.insert decision digest state.completedLabelWorkflows
            }
      Just retained
        | retained == digest -> Right withCompleted
        | otherwise -> Left (LabelWorkflowCompletionConflict decision)
  DisappearanceProbeOpenedView header result -> do
    let probe = projectedDisappearanceProbeHeaderId header
        subject = projectedDisappearanceProbeHeaderSubject header
        coordinate = projectedDisappearanceProbeHeaderCoordinate header
        membership = projectedDisappearanceProbeHeaderMembership header
        validHeader =
          oracleViewPendingHeraldAdmission (oracleView state) == Nothing
            && membership == state.currentHeraldMembership
            && coordinate == disappearanceSubjectMembershipCoordinate subject membership
    unless validHeader (Left (DisappearanceProjectionConflict probe))
    case disappearanceOpenResultView result of
      OpenedDisappearanceProbe opened -> do
        unless
          ( opened == probe
              && deriveDisappearanceProbeId (appliedEntryControlIndex entry) == Right probe
              && Map.notMember probe state.disappearanceProbes
              && all
                ((/= coordinate) . projectedDisappearanceProbeHeaderCoordinate . projectedDisappearanceHeader)
                [ current
                | current <- Map.elems state.disappearanceProbes,
                  projectedDisappearanceTerminal current == Nothing
                ]
          )
          (Left (DisappearanceProjectionConflict probe))
        pure
          state
            { disappearanceProbes =
                Map.insert
                  probe
                  (ProjectedDisappearanceProbe header Map.empty Nothing)
                  state.disappearanceProbes
            }
      AliasedDisappearanceProbe aliased -> do
        current <- lookupDisappearanceProjection probe state
        unless
          ( aliased == probe
              && projectedDisappearanceHeader current == header
              && projectedDisappearanceTerminal current == Nothing
              && disappearanceProbeOpenControlIndex probe < appliedEntryControlIndex entry
          )
          (Left (DisappearanceProjectionConflict probe))
        pure state
  PredefinedAbsenceReportedView claim -> do
    let probe = disappearanceEvidenceClaimProbeId claim
        reporter = disappearanceEvidenceClaimReporter claim
    ProjectedDisappearanceProbe header reports terminal <- lookupDisappearanceProjection probe state
    unless
      ( terminal == Nothing
          && disappearanceEvidenceClaimCoordinate claim == projectedDisappearanceProbeHeaderCoordinate header
          && reporter
            `elem` NonEmpty.toList
              ( heraldMembershipGenerationActiveHeraldEpochs
                  (projectedDisappearanceProbeHeaderMembership header)
              )
          && maybe True (== claim) (Map.lookup reporter reports)
      )
      (Left (DisappearanceProjectionConflict probe))
    pure
      state
        { disappearanceProbes =
            Map.insert
              probe
              (ProjectedDisappearanceProbe header (Map.insert reporter claim reports) Nothing)
              state.disappearanceProbes
        }
  DisappearanceProbeInvalidatedView probe cause ->
    terminalizeDisappearance entry probe (ProjectedDisappearanceInvalidated cause (appliedEntryControlIndex entry)) state
  DisappearanceProbeResolvedView probe outcome -> do
    current <- lookupDisappearanceProjection probe state
    let header = projectedDisappearanceHeader current
        members =
          NonEmpty.toList
            ( heraldMembershipGenerationActiveHeraldEpochs
                (projectedDisappearanceProbeHeaderMembership header)
            )
    unless
      ( map fst (projectedDisappearanceReports current) == members
          && resolveDisappearanceSubject
            state.systemId
            (appliedEntryControlIndex entry)
            (projectedDisappearanceProbeHeaderSubject header)
            == Right outcome
      )
      (Left (DisappearanceProjectionConflict probe))
    terminalizeDisappearance entry probe (ProjectedDisappearanceResolved outcome (appliedEntryControlIndex entry)) state
  DisappearanceProbeAbortedView probe cause ->
    terminalizeDisappearance entry probe (ProjectedDisappearanceAborted cause (appliedEntryControlIndex entry)) state
  OracleFailureProbeOpenedView probe target generation configuration -> do
    unless
      ( generation == heraldMembershipGenerationId state.currentHeraldMembership
          && maybe True (== configuration) state.voterConfiguration
          && state.pendingVoterChange == Nothing
          && case Voter.voterConfigurationView configuration of
            Voter.StableVoterConfigurationView _ -> True
            Voter.JointVoterConfigurationView {} -> False
      )
      (Left (AppliedEntryVoterProjectionMismatch (appliedEntryControlIndex entry)))
    if Map.member probe state.failureProbes
      then Left (FailureProbeAlreadyProjected probe)
      else Right state {failureProbes = Map.insert probe (ProjectedFailureProbe target generation configuration Map.empty Nothing) state.failureProbes}
  OracleFailureProbeReportRecordedView probe reporter result ->
    adjustFailureProbe
      probe
      (\projected -> projected {reports = Map.insert reporter result projected.reports})
      state
  OracleFailureProbeDismissedView probe resolution ->
    adjustFailureProbe
      probe
      (setProjectedFailureProbeTerminal (ProjectedFailureProbeDismissed resolution))
      state
  HeraldMembershipAdvancedView _ ->
    -- Membership advances are batch-validated before individual projection.
    Right state
  OracleFailureProbeRetiredView probe resolution successor ->
    adjustFailureProbe
      probe
      ( \projected -> case projected.terminal of
          Just ProjectedVoterHostFailureAccepted {} -> projected
          _ -> setProjectedFailureProbeTerminal (ProjectedFailureProbeRetired resolution successor) projected
      )
      state
  OracleVoterHostFailureAcceptedView certificate -> do
    let probe = Membership.failureProbeResolutionProbeId (Failure.acceptedVoterHostFailureResolution certificate)
    projected <- maybe (Left (FailureProbeEventWithoutOpen probe)) Right (Map.lookup probe state.failureProbes)
    unless
      ( projected.target == Failure.acceptedVoterHostFailureTarget certificate
          && projected.generation == Failure.acceptedVoterHostFailureMembership certificate
          && projected.capturedVoterConfiguration == Failure.acceptedVoterHostFailureConfiguration certificate
          && Map.toAscList projected.reports == Failure.acceptedVoterHostFailureReports certificate
          && Failure.acceptedVoterHostFailureControlIndex certificate <= appliedEntryControlIndex entry
          && (projected.terminal == Nothing || projected.terminal == Just (ProjectedVoterHostFailureAccepted certificate))
      )
      (Left (AppliedEntryVoterProjectionMismatch (appliedEntryControlIndex entry)))
    adjustFailureProbe probe (setProjectedFailureProbeTerminal (ProjectedVoterHostFailureAccepted certificate)) state
  OracleFailureProbeSupersededView probe successor ->
    adjustFailureProbe
      probe
      (setProjectedFailureProbeTerminal (ProjectedFailureProbeSuperseded successor))
      state

lookupDisappearanceProjection :: DisappearanceProbeId -> State -> Either OracleProjectionFault ProjectedDisappearanceProbe
lookupDisappearanceProjection probe state =
  maybe (Left (DisappearanceProjectionEventWithoutOpen probe)) Right (Map.lookup probe state.disappearanceProbes)

terminalizeDisappearance :: AppliedOracleEntry -> DisappearanceProbeId -> ProjectedDisappearanceTerminal -> State -> Either OracleProjectionFault State
terminalizeDisappearance entry probe supplied state = do
  ProjectedDisappearanceProbe header reports terminal <- lookupDisappearanceProjection probe state
  unless (terminal == Nothing) (Left (DisappearanceProjectionConflict probe))
  let members =
        NonEmpty.toList
          ( heraldMembershipGenerationActiveHeraldEpochs
              (projectedDisappearanceProbeHeaderMembership header)
          )
  case supplied of
    ProjectedDisappearanceInvalidated (CommandDisappearanceInvalidation reporter _ _) _ ->
      unless
        (reporter `elem` members && fmap (oracleClientRequestHome . appliedEntryRequestId) (appliedEntryCommand entry) == Just reporter)
        (Left (DisappearanceProjectionConflict probe))
    ProjectedDisappearanceInvalidated (LabelOpenDisappearanceInvalidation decision) _ ->
      unless
        (any (matchesLabel header decision) (appliedEntryProjectionEvents entry))
        (Left (DisappearanceProjectionConflict probe))
    ProjectedDisappearanceAborted (AdmissionPreparingDisappearanceAbortView admission) _ ->
      unless
        (any (matchesAdmission header admission) (appliedEntryProjectionEvents entry))
        (Left (DisappearanceProjectionConflict probe))
    ProjectedDisappearanceAborted (MembershipSupersededDisappearanceAbortView generation) _ ->
      unless
        ( generation == heraldMembershipGenerationId state.currentHeraldMembership
            && heraldMembershipGenerationPredecessor state.currentHeraldMembership
              == Just (heraldMembershipGenerationId (projectedDisappearanceProbeHeaderMembership header))
        )
        (Left (DisappearanceProjectionConflict probe))
    _ -> pure ()
  pure
    state
      { disappearanceProbes =
          Map.insert
            probe
            (ProjectedDisappearanceProbe header reports (Just supplied))
            state.disappearanceProbes
      }
  where
    matchesAdmission header expected event = case oracleProjectionEventView event of
      HeraldAdmissionChangedView record ->
        Admission.admissionRecordId record == expected
          && Admission.admissionRecordBeginIndex record == appliedEntryControlIndex entry
          && Admission.admissionRecordPhase record == Admission.AdmissionPreparing
          && Admission.admissionRecordPredecessor record == projectedDisappearanceProbeHeaderMembership header
      _ -> False
    matchesLabel header expected event = case oracleProjectionEventView event of
      LabelDecidedView opened outcome _
        | LiveReleasedOutcomeView {} <- liveTerminalOutcomeView outcome ->
            liveDecisionId opened == expected && case disappearanceSubjectView (projectedDisappearanceProbeHeaderSubject header) of
              ControlledPredefinedSubjectView object _ _ -> liveDecisionObject opened == object
              _ -> False
      _ -> False

-- | Compound consequences are complete and ordered in the canonical entry.
-- Validate against the predecessor before installing its membership or label
-- prefix, so a missing abort/invalidation cannot leave a collecting old probe.
validateDisappearanceCompoundEvents :: AppliedOracleEntry -> State -> Either OracleProjectionFault ()
validateDisappearanceCompoundEvents entry state = do
  let views = fmap oracleProjectionEventView (appliedEntryProjectionEvents entry)
      collecting =
        [ (probe, projectedDisappearanceHeader current)
        | (probe, current) <- Map.toAscList state.disappearanceProbes,
          projectedDisappearanceTerminal current == Nothing
        ]
      expectedLabels =
        [ (probe, liveDecisionId opened)
        | LabelDecidedView opened outcome _ <- views,
          LiveReleasedOutcomeView {} <- [liveTerminalOutcomeView outcome],
          (probe, header) <- collecting,
          ControlledPredefinedSubjectView object _ _ <-
            [disappearanceSubjectView (projectedDisappearanceProbeHeaderSubject header)],
          object == liveDecisionObject opened
        ]
      actualLabels =
        [ (probe, decision)
        | DisappearanceProbeInvalidatedView probe (LabelOpenDisappearanceInvalidation decision) <- views
        ]
      expectedAborts =
        [ (probe, heraldMembershipGenerationId successor)
        | HeraldMembershipAdvancedView successor <- views,
          (probe, header) <- collecting,
          Just (heraldMembershipGenerationId (projectedDisappearanceProbeHeaderMembership header))
            == heraldMembershipGenerationPredecessor successor
        ]
      actualAborts =
        [ (probe, generation)
        | DisappearanceProbeAbortedView probe (MembershipSupersededDisappearanceAbortView generation) <- views
        ]
      beginningAdmissions =
        [ Admission.admissionRecordId record
        | HeraldAdmissionChangedView record <- views,
          Admission.admissionRecordBeginIndex record == appliedEntryControlIndex entry,
          Admission.admissionRecordPhase record == Admission.AdmissionPreparing,
          Admission.admissionRecordPredecessor record == state.currentHeraldMembership,
          Map.notMember (Admission.admissionRecordId record) state.heraldAdmissions
        ]
      expectedPreparationAborts = [(probe, admission) | admission <- beginningAdmissions, (probe, _) <- collecting]
      actualPreparationAborts = [(probe, admission) | DisappearanceProbeAbortedView probe (AdmissionPreparingDisappearanceAbortView admission) <- views]
      preparationOrder = case actualPreparationAborts of
        [] -> True
        _ -> case views of
          HeraldAdmissionChangedView _ : rest -> all isPreparationAbort rest
          _ -> False
      labelOrder = case expectedLabels of
        [] -> True
        _ -> case dropWhile (\case HeraldAdmissionChangedView {} -> True; _ -> False) views of
          LabelDecidedView _ _ _ : rest -> length rest == length expectedLabels
          _ -> False
      abortOrder = case actualAborts of
        [] -> True
        _ -> case dropWhile (\case HeraldMembershipAdvancedView _ -> False; _ -> True) views of
          _ : rest ->
            let afterPrefix = dropWhile isFailureConsequence rest
                (aborts, remaining) = span isMembershipAbort afterPrefix
             in length aborts == length actualAborts && not (any isMembershipAbort remaining)
          [] -> False
  unless
    (expectedLabels == actualLabels && expectedAborts == actualAborts && expectedPreparationAborts == actualPreparationAborts && labelOrder && abortOrder && preparationOrder)
    (Left (DisappearanceProjectionCompoundMismatch (appliedEntryControlIndex entry)))
  where
    isPreparationAbort = \case
      DisappearanceProbeAbortedView _ (AdmissionPreparingDisappearanceAbortView _) -> True
      _ -> False
    isMembershipAbort = \case
      DisappearanceProbeAbortedView _ (MembershipSupersededDisappearanceAbortView _) -> True
      _ -> False
    isFailureConsequence = \case
      OracleFailureProbeRetiredView {} -> True
      OracleFailureProbeSupersededView {} -> True
      _ -> False

setProjectedFailureProbeTerminal ::
  ProjectedFailureProbeTerminal -> ProjectedFailureProbe -> ProjectedFailureProbe
setProjectedFailureProbeTerminal supplied (ProjectedFailureProbe target generation configuration reports _) = ProjectedFailureProbe target generation configuration reports (Just supplied)

adjustFailureProbe ::
  HeraldFailureProbeId ->
  (ProjectedFailureProbe -> ProjectedFailureProbe) ->
  State ->
  Either OracleProjectionFault State
adjustFailureProbe probe update state =
  case Map.lookup probe state.failureProbes of
    Nothing -> Left (FailureProbeEventWithoutOpen probe)
    Just projected ->
      Right
        state
          { Eclips.Herald.OracleProjection.State.failureProbes =
              Map.insert probe (update projected) state.failureProbes
          }

decidedWorkflow :: LiveLabelDecision -> LiveTerminalOutcome -> LabelOutcomeDigest -> ProjectedLabelWorkflow
decidedWorkflow decision outcome digest = case liveTerminalOutcomeView outcome of
  LiveNotAppliedOutcomeView _ index _ _ _ _ ->
    ProjectedLabelWorkflow decision LabelWorkflowCompleted Nothing Nothing (Just (ProjectedLabelNotApplied index digest))
  LiveReleasedOutcomeView facts preparedDigest index overlay ->
    ProjectedLabelWorkflow decision LabelWorkflowReleased (Just facts) (Just preparedDigest) (Just (ProjectedLabelReleased index overlay digest))

adjustLabelWorkflow ::
  LabelDecisionId ->
  (ProjectedLabelWorkflow -> ProjectedLabelWorkflow) ->
  State ->
  Either OracleProjectionFault State
adjustLabelWorkflow decision update state =
  case Map.lookup decision state.labelWorkflows of
    Nothing -> Left (LabelWorkflowEventWithoutOpen decision)
    Just workflow ->
      Right
        state
          { Eclips.Herald.OracleProjection.State.labelWorkflows =
              Map.insert decision (update workflow) state.labelWorkflows
          }

validateStartedProcess ::
  AppliedOracleEntry ->
  ProcessEpochRecord ->
  State ->
  Either OracleProjectionFault ProjectedStartedProcess
validateStartedProcess entry record state = do
  command <- requireCommandEntry entry
  let process = processRecordProcessEpoch record
      processId = processRecordProcessId record
      index = appliedEntryControlIndex entry
      knownProcessIds =
        Set.fromList
          ( fmap appliedProcessId (Map.elems state.genesisProcesses)
              <> fmap
                (processStartProcessId . projectedStartedProcessBootstrap)
                (Map.elems state.startedProcesses)
          )
  unless
    ( Map.notMember process state.genesisProcesses
        && Map.notMember process state.startedProcesses
    )
    (Left (StartedProcessAlreadyProjected process))
  unless
    (Set.notMember processId knownProcessIds)
    (Left (StartedProcessIdAlreadyUsed processId))
  request <- case processRecordOrigin record of
    DynamicStartView originIndex retainedRequest
      | originIndex == index && retainedRequest == appliedEntryRequestId command -> Right retainedRequest
      | originIndex /= index -> Left (StartedProcessIndexMismatch originIndex index)
      | otherwise -> Left (StartedProcessOriginMismatch process)
    _ -> Left (StartedProcessOriginMismatch process)
  let expected = processStart processId process (processRecordResidence record)
  unless
    (processRecordLifecycle record == ProcessRecordLiveView)
    (Left (StartedProcessLifecycleMismatch process))
  let expectedHome = processStartResidence expected
      requestHome = oracleClientRequestHome (appliedEntryRequestId command)
  unless
    (requestHome == expectedHome)
    (Left (StartedProcessRequestHomeMismatch requestHome expectedHome))
  pure (ProjectedStartedProcess expected index request)

validateEndedProcess ::
  AppliedOracleEntry ->
  ProcessEpochId ->
  ControlIndex ->
  ProcessEndReason ->
  State ->
  Either OracleProjectionFault ProjectedEndedProcess
validateEndedProcess entry process endIndex reason state = do
  command <- requireCommandEntry entry
  let index = appliedEntryControlIndex entry
      requestHome = oracleClientRequestHome (appliedEntryRequestId command)
  residence <-
    maybe
      (Left (EndedProcessUnknown process))
      Right
      (projectedProcessResidence process state)
  unless
    (Map.notMember process state.endedProcesses)
    (Left (EndedProcessAlreadyProjected process))
  unless
    (endIndex == index)
    (Left (EndedProcessIndexMismatch endIndex index))
  unless
    (requestHome == residence)
    (Left (EndedProcessRequestHomeMismatch requestHome residence))
  pure (ProjectedEndedProcess process endIndex reason)

requireCommandEntry :: AppliedOracleEntry -> Either OracleProjectionFault AppliedOracleCommandEntry
requireCommandEntry entry = maybe (Left (AppliedEntryOriginMismatch (appliedEntryControlIndex entry))) Right (appliedEntryCommand entry)

projectedProcessResidence :: ProcessEpochId -> State -> Maybe HeraldEpoch
projectedProcessResidence process state =
  case Map.lookup process state.genesisProcesses of
    Just bootstrap -> Just (appliedProcessResidence bootstrap)
    Nothing ->
      processStartResidence . projectedStartedProcessBootstrap
        <$> Map.lookup process state.startedProcesses

-- | Contradictions observable only through composed validation or a kernel bug.
data OracleProjectionInvariant
  = ProjectionCatalogueDigestInvariant
  | ProjectionOccurrenceSetInvariant
  | ProjectionHeraldCatalogueInvariant HeraldEpoch HeraldEpoch
  | ProjectionGenesisHeraldMembershipInvariant
  | ProjectionLocalHeraldInvariant HeraldEpoch
  | ProjectionInitialDigestInvariant
  | ProjectionProcessKeyInvariant ProcessEpochId ProcessEpochId
  | ProjectionStartedProcessKeyInvariant ProcessEpochId ProcessEpochId
  | ProjectionEndedProcessKeyInvariant ProcessEpochId ProcessEpochId
  | ProjectionStartedProcessBeyondCursor ProcessEpochId ControlIndex ControlIndex
  | ProjectionEndedProcessBeyondCursor ProcessEpochId ControlIndex ControlIndex
  | ProjectionEvidenceKeyMismatch ControlIndex ControlIndex
  | ProjectionEvidenceGap ControlIndex ControlIndex
  | ProjectionEvidenceInvalid ControlIndex OracleProjectionFault
  | ProjectionEvidenceKindConflict ControlIndex
  | ProjectionEvidenceBeyondCursor ControlIndex ControlIndex
  | ProjectionCanonicalCoverageInvariant ControlIndex ControlIndex ControlIndex
  | ProjectionCanonicalPinMissing ControlIndex
  | ProjectionCanonicalTailFloorBeyondCursor ControlIndex ControlIndex
  | ProjectionCanonicalTailUnavailable ControlIndex ControlIndex
  | ProjectionGenesisEvidenceCovered ControlIndex
  | ProjectionLastSemanticControlIndexMismatch ControlIndex ControlIndex
  | ProjectionPostStateDigestMismatch
  | ProjectionMembershipEvidenceKeyMismatch ControlIndex ControlIndex
  | ProjectionMembershipEvidenceInvalid ControlIndex MembershipAdvanceProblem
  | ProjectionCursorMismatch ControlIndex ControlIndex
  | ProjectionCurrentHeraldMembershipMismatch
  | ProjectionHeraldMembershipHistoryMismatch
  | ProjectionHeraldAdmissionsMismatch
  | ProjectionMembershipAdvancesMismatch
  | ProjectionStartedProcessesMismatch
  | ProjectionEndedProcessesMismatch
  | ProjectionLabelWorkflowsMismatch
  | ProjectionCompletedLabelWorkflowsMismatch
  | ProjectionFailureProbesMismatch
  | ProjectionDisappearanceProbesMismatch
  | ProjectionVoterFactsMismatch
  | ProjectionPortableFactsInvalid ProjectionBaseCodecProblem
  deriving stock (Eq, Show)

-- | Checked evidence used by composed invariants and property tests.
data OracleProjectionStateWitness = OracleProjectionStateWitness
  { witnessedControlIndex :: ControlIndex,
    witnessedAppliedEntries :: [(ControlIndex, CanonicalAppliedOracleEntry)],
    witnessedStartedProcesses :: [(ProcessEpochId, ProjectedStartedProcess)],
    witnessedEndedProcesses :: [(ProcessEpochId, ProjectedEndedProcess)],
    witnessedPostStateDigest :: Maybe OracleStateDigest
  }
  deriving stock (Eq, Show)

-- | Capture this owner's checked current facts as its next replay origin. Exact
-- result/retry evidence and historical authority remain available at their
-- original coordinates. This must precede, not imply, any later reclamation.
advanceReplayBase :: State -> Either OracleProjectionInvariant State
advanceReplayBase state = do
  _ <- validateState state
  pure
    state
      { replayBase =
          if state.controlIndex == controlIndex 0
            then Nothing
            else let !base = captureReplayBase state in Just base
      }

replayBaseControlIndex :: State -> ControlIndex
replayBaseControlIndex state = maybe (controlIndex 0) (\base -> base.baseControlIndex) state.replayBase

-- | Explicitly replace canonical evidence through the checked replay base with
-- covered-prefix admission. Exact pins and an optional exclusive canonical-tail
-- floor must already be readable; this seam cannot recover released bytes.
-- The tail remains contiguous even below the semantic base. Advancing the
-- replay base alone deliberately does not perform this reclamation.
reclaimAppliedPrefixThroughBase :: Set ControlIndex -> Maybe ControlIndex -> State -> Either OracleProjectionInvariant State
reclaimAppliedPrefixThroughBase pins tailFloor state = do
  _ <- validateState state
  case Set.lookupMin (Set.filter (`Map.notMember` state.appliedEntries) pins) of
    Just missing -> Left (ProjectionCanonicalPinMissing missing)
    Nothing -> pure ()
  case tailFloor of
    Nothing -> pure ()
    Just floorIndex
      | floorIndex > state.controlIndex -> Left (ProjectionCanonicalTailFloorBeyondCursor floorIndex state.controlIndex)
      | otherwise -> case retainedControlSuffixAfter floorIndex state of
          Nothing -> Left (ProjectionCanonicalTailUnavailable floorIndex state.controlIndex)
          Just _ -> pure ()
  let !through = replayBaseControlIndex state
      !retainedFloor = maybe through (min through) tailFloor
      !retained = Map.restrictKeys state.appliedEntries (Set.filter (<= retainedFloor) pins) `Map.union` snd (Map.split retainedFloor state.appliedEntries)
  pure state {appliedEntries = retained, canonicalEvidenceCoveredThrough = through}

-- | Completed-batch reclamation for this opaque, already admitted owner. Its
-- semantic facts have been maintained by checked transitions; capturing them
-- does not replay history or repeat the independent 'validateState' audit.
-- Composition supplies its surviving exact evidence and any outstanding
-- exclusive tail floor. Both must already be retained here. The supplied map
-- selects coordinates only: original Projection bytes remain authoritative.
--
-- Unique archive keys are already bounded by the current cursor, so the size
-- of the suffix above a floor proves continuity without visiting every row.
-- Repeating this transition at the same cursor can release pins or a tail.
reclaimCurrentAppliedPrefix :: Map ControlIndex CanonicalAppliedOracleEntry -> Maybe ControlIndex -> State -> Either OracleProjectionInvariant State
reclaimCurrentAppliedPrefix pins tailFloor state = do
  let !pinned = Map.intersection state.appliedEntries pins
  unless
    (Map.size pinned == Map.size pins)
    -- The intersection is a proper subset here, so the difference is nonempty.
    (Left (ProjectionCanonicalPinMissing (fst (Map.findMin (Map.difference pins pinned)))))
  retained <- case tailFloor of
    Nothing -> pure pinned
    Just floorIndex -> do
      unless
        (floorIndex <= state.controlIndex)
        (Left (ProjectionCanonicalTailFloorBeyondCursor floorIndex state.controlIndex))
      let !suffix = snd (Map.split floorIndex state.appliedEntries)
      unless
        (toInteger (Map.size suffix) == toInteger (controlIndexWord64 state.controlIndex) - toInteger (controlIndexWord64 floorIndex))
        (Left (ProjectionCanonicalTailUnavailable floorIndex state.controlIndex))
      let (earlierPins, atFloor, _) = Map.splitLookup floorIndex pinned
          !coveredPins = maybe earlierPins (\entry -> Map.insert floorIndex entry earlierPins) atFloor
      pure (Map.union coveredPins suffix)
  let !base =
        if state.controlIndex == controlIndex 0
          then Nothing
          else let !facts = captureReplayBase state in Just facts
      !successor = state {replayBase = base, appliedEntries = retained, canonicalEvidenceCoveredThrough = state.controlIndex}
  pure successor

projectionCanonicalCoveredThrough :: State -> ControlIndex
projectionCanonicalCoveredThrough state = state.canonicalEvidenceCoveredThrough

-- | Capture semantic facts validated from the source's checked replay base and
-- suffix without retaining the source projection. Retained exact evidence and
-- its covered floor travel alongside the base, outside its semantic payload.
captureProjectionBase :: State -> Either OracleProjectionInvariant ProjectionBaseCapture
captureProjectionBase = captureProjectionBaseWithDiagnostics DiagnosticChecksEnabled

-- | Local captures may rely on their already-admitted owner invariants. Wire
-- admission remains independent and always checks every portable relationship.
captureProjectionBaseWithDiagnostics :: DiagnosticChecks -> State -> Either OracleProjectionInvariant ProjectionBaseCapture
captureProjectionBaseWithDiagnostics checks state = do
  runDiagnosticCheck checks $ do
    _ <- validateState state
    _ <- either (Left . ProjectionPortableFactsInvalid . ProjectionBaseCodecMalformed . show) Right (captureProjectionIdentityBase state)
    _ <- either (Left . ProjectionPortableFactsInvalid) Right (captureProjectionLabelBase state)
    _ <- either (Left . ProjectionPortableFactsInvalid) Right (captureProjectionVoterFailureBase state)
    _ <- either (Left . ProjectionPortableFactsInvalid) Right (captureProjectionDisappearanceBase state)
    _ <- either (Left . ProjectionPortableFactsInvalid) Right (admitProjectionPrimaryCoordinates state)
    _ <- either (Left . ProjectionPortableFactsInvalid) Right (admitProjectionBaseArchive state state.appliedEntries state.membershipAdvances state.canonicalEvidenceCoveredThrough)
    pure ()
  let !base = CheckedProjectionBase (projectionBaseGenesis state) (captureReplayBase state)
      !archive = ProjectionBaseArchive state.appliedEntries state.membershipAdvances state.canonicalEvidenceCoveredThrough
  pure (ProjectionBaseCapture base archive)

projectionBaseControlIndex :: ProjectionBaseCapture -> ControlIndex
projectionBaseControlIndex (ProjectionBaseCapture (CheckedProjectionBase _ base) _) = base.baseControlIndex

projectionBaseIdentity :: ProjectionBaseCapture -> (SystemId, CatalogueDigest, ConfigurationDigest, InitialProjectionDigest)
projectionBaseIdentity (ProjectionBaseCapture (CheckedProjectionBase (ProjectionBaseGenesis system catalogue configuration initial _ _ _) _) _) =
  (system, catalogue, configuration, initial)

-- | Share the immutable exact archive from its checked capture. The map does
-- not retain the donor projection or reconstruct an additional map spine.
projectionBaseAppliedEntries :: ProjectionBaseCapture -> Map ControlIndex CanonicalAppliedOracleEntry
projectionBaseAppliedEntries (ProjectionBaseCapture _ (ProjectionBaseArchive entries _ _)) = entries

projectionBaseCanonicalCoveredThrough :: ProjectionBaseCapture -> ControlIndex
projectionBaseCanonicalCoveredThrough (ProjectionBaseCapture _ (ProjectionBaseArchive _ _ through)) = through

-- | Read detached semantic facts from the same checked capture as its archive.
-- Dependent captures need neither the donor owner nor a second Projection audit.
projectionBaseHeraldMembershipHistory :: ProjectionBaseCapture -> [HeraldMembershipGeneration]
projectionBaseHeraldMembershipHistory (ProjectionBaseCapture (CheckedProjectionBase _ base) _) =
  NonEmpty.toList (Membership.heraldMembershipHistoryGenerations base.baseHeraldMembershipHistory)

projectionBaseCurrentHeraldMembershipId :: ProjectionBaseCapture -> HeraldMembershipGenerationId
projectionBaseCurrentHeraldMembershipId (ProjectionBaseCapture (CheckedProjectionBase _ base) _) =
  heraldMembershipGenerationId base.baseCurrentHeraldMembership

projectionBaseLabelWorkflows :: ProjectionBaseCapture -> [(LabelDecisionId, ProjectedLabelWorkflow)]
projectionBaseLabelWorkflows (ProjectionBaseCapture (CheckedProjectionBase _ base) _) = Map.toAscList base.baseLabelWorkflows

projectionBaseLastSemanticControlIndex :: ProjectionBaseCapture -> ControlIndex
projectionBaseLastSemanticControlIndex (ProjectionBaseCapture (CheckedProjectionBase _ base) _) = base.baseLastSemanticControlIndex

data ProjectionBaseInstallProblem
  = ProjectionBaseReceiverNotFresh ControlIndex
  | ProjectionBaseGenesisMismatch
  deriving stock (Eq, Show)

-- | Install a checked capture into a fresh receiver. Immutable genesis and local
-- identity/admission belong to the receiver and are preserved. The composed
-- onboarding transition separately checks the observer and admission attempt.
-- Installation does not replay the captured canonical prefix.
installProjectionBase :: ProjectionBaseCapture -> State -> Either ProjectionBaseInstallProblem State
installProjectionBase (ProjectionBaseCapture (CheckedProjectionBase genesis base) (ProjectionBaseArchive entries memberships through)) receiver = do
  unless
    (receiver.controlIndex == controlIndex 0)
    (Left (ProjectionBaseReceiverNotFresh receiver.controlIndex))
  unless
    (projectionBaseGenesis receiver == genesis)
    (Left ProjectionBaseGenesisMismatch)
  let !restored = restoreReplayBase base receiver
  pure
    restored
      { appliedEntries = entries,
        membershipAdvances = memberships,
        canonicalEvidenceCoveredThrough = through,
        replayBase = if base.baseControlIndex == controlIndex 0 then Nothing else Just base
      }

projectionBaseGenesis :: State -> ProjectionBaseGenesis
projectionBaseGenesis state =
  ProjectionBaseGenesis
    state.systemId
    state.catalogueDigest
    state.configurationDigest
    state.initialProjectionDigest
    state.genesisHeraldMembership
    state.initialVoterConfiguration
    state.initialOracleReplicas

captureReplayBase :: State -> ReplayBase
captureReplayBase state =
  ReplayBase
    { baseControlIndex = state.controlIndex,
      baseHeraldCatalogue = state.heraldCatalogue,
      baseHeraldAdmissions = state.heraldAdmissions,
      baseCurrentHeraldMembership = state.currentHeraldMembership,
      baseHeraldMembershipHistory = state.heraldMembershipHistory,
      baseStartedProcesses = state.startedProcesses,
      baseEndedProcesses = state.endedProcesses,
      baseLabelWorkflows = state.labelWorkflows,
      baseCompletedLabelWorkflows = state.completedLabelWorkflows,
      baseFailureProbes = state.failureProbes,
      baseDisappearanceProbes = state.disappearanceProbes,
      baseVoterConfiguration = state.voterConfiguration,
      baseOracleReplicas = state.oracleReplicas,
      baseVoterChanges = state.voterChanges,
      basePendingVoterChange = state.pendingVoterChange,
      baseLastSemanticControlIndex = state.lastSemanticControlIndex,
      baseLatestCanonicalStateDigest = state.latestCanonicalStateDigest
    }

-- | Independent append-only reference audit while the complete archive remains
-- available. Ordinary validation uses the checked base and contiguous suffix.
validateStateFromGenesis :: State -> Either OracleProjectionInvariant OracleProjectionStateWitness
validateStateFromGenesis state = do
  unless (state.canonicalEvidenceCoveredThrough == controlIndex 0) (Left (ProjectionGenesisEvidenceCovered state.canonicalEvidenceCoveredThrough))
  validateState state {replayBase = Nothing}

validateState :: State -> Either OracleProjectionInvariant OracleProjectionStateWitness
validateState state = do
  let baseIndex = replayBaseControlIndex state
  unless
    (state.canonicalEvidenceCoveredThrough <= baseIndex && baseIndex <= state.controlIndex)
    (Left (ProjectionCanonicalCoverageInvariant state.canonicalEvidenceCoveredThrough baseIndex state.controlIndex))
  unless
    (state.lastSemanticControlIndex <= state.controlIndex)
    (Left (ProjectionLastSemanticControlIndexMismatch state.controlIndex state.lastSemanticControlIndex))
  unless
    (state.catalogueDigest == profileCatalogueDigest)
    (Left ProjectionCatalogueDigestInvariant)
  unless
    (state.predefinedOccurrences == genesisPredefinedOccurrenceSet state.systemId)
    (Left ProjectionOccurrenceSetInvariant)
  mapM_ validateHeraldCatalogueEntry (Map.toAscList state.heraldCatalogue)
  expectedGenesisMembership <-
    case NonEmpty.nonEmpty (Map.keys state.genesisHeraldCatalogue) of
      Nothing -> Left ProjectionGenesisHeraldMembershipInvariant
      Just epochs ->
        either
          (const (Left ProjectionGenesisHeraldMembershipInvariant))
          Right
          (genesisHeraldMembershipGeneration state.systemId epochs)
  unless
    (state.genesisHeraldMembership == expectedGenesisMembership)
    (Left ProjectionGenesisHeraldMembershipInvariant)
  unless
    ( Map.member state.localHeraldEpoch state.heraldCatalogue
        || maybe False ((== state.localHeraldEpoch) . Admission.admissionManifestHeraldEpoch . Admission.admissionRecordManifest) state.localStartupAdmission
    )
    (Left (ProjectionLocalHeraldInvariant state.localHeraldEpoch))
  unless
    ( state.initialProjectionDigest
        == deriveInitialProjectionDigest (Map.elems state.genesisProcesses) state.initialTopology
    )
    (Left ProjectionInitialDigestInvariant)
  mapM_ validateProcessKey (Map.toAscList state.genesisProcesses)
  mapM_ validateStartedKey (Map.toAscList state.startedProcesses)
  mapM_ validateEndedKey (Map.toAscList state.endedProcesses)
  mapM_ (validateStartedBounds state.controlIndex) (Map.toAscList state.startedProcesses)
  mapM_ (validateEndedBounds state.controlIndex) (Map.toAscList state.endedProcesses)
  validateRetainedBounds state.appliedEntries
  validateRetainedBounds state.membershipAdvances
  case Set.lookupMin
    (Map.keysSet (afterReplayBase state state.appliedEntries) `Set.intersection` Map.keysSet (afterReplayBase state state.membershipAdvances)) of
    Nothing -> Right ()
    Just index -> Left (ProjectionEvidenceKindConflict index)
  replayed <-
    foldM
      replayProjectionEvidence
      (replayOriginState state)
      (retainedProjectionEvidence state)
  unless
    (replayed.controlIndex == state.controlIndex)
    (Left (ProjectionCursorMismatch state.controlIndex replayed.controlIndex))
  unless
    (replayed.lastSemanticControlIndex == state.lastSemanticControlIndex)
    (Left (ProjectionLastSemanticControlIndexMismatch state.lastSemanticControlIndex replayed.lastSemanticControlIndex))
  unless
    (replayed.latestCanonicalStateDigest == state.latestCanonicalStateDigest)
    (Left ProjectionPostStateDigestMismatch)
  unless
    (replayed.heraldCatalogue == state.heraldCatalogue && replayed.heraldAdmissions == state.heraldAdmissions)
    (Left ProjectionHeraldAdmissionsMismatch)
  unless
    (replayed.voterConfiguration == state.voterConfiguration && replayed.oracleReplicas == state.oracleReplicas && replayed.voterChanges == state.voterChanges && replayed.pendingVoterChange == state.pendingVoterChange)
    (Left ProjectionVoterFactsMismatch)
  unless
    (replayed.startedProcesses == state.startedProcesses)
    (Left ProjectionStartedProcessesMismatch)
  unless
    (replayed.currentHeraldMembership == state.currentHeraldMembership)
    (Left ProjectionCurrentHeraldMembershipMismatch)
  unless
    (replayed.heraldMembershipHistory == state.heraldMembershipHistory)
    (Left ProjectionHeraldMembershipHistoryMismatch)
  unless
    (replayed.membershipAdvances == afterReplayBase state state.membershipAdvances)
    (Left ProjectionMembershipAdvancesMismatch)
  unless
    (replayed.endedProcesses == state.endedProcesses)
    (Left ProjectionEndedProcessesMismatch)
  unless
    (replayed.labelWorkflows == state.labelWorkflows)
    (Left ProjectionLabelWorkflowsMismatch)
  unless
    (replayed.completedLabelWorkflows == state.completedLabelWorkflows)
    (Left ProjectionCompletedLabelWorkflowsMismatch)
  unless
    (replayed.failureProbes == state.failureProbes)
    (Left ProjectionFailureProbesMismatch)
  unless
    (replayed.disappearanceProbes == state.disappearanceProbes)
    (Left ProjectionDisappearanceProbesMismatch)
  pure (stateWitness state)
  where
    -- Base capture already authenticated old evidence. Reclamation only deletes
    -- keys, so its sparse exact pins need bounds checks, not prefix replay.
    validateRetainedBounds :: Map ControlIndex value -> Either OracleProjectionInvariant ()
    validateRetainedBounds entries = do
      mapM_ validateEvidenceBounds (fst <$> Map.lookupMin entries)
      mapM_ validateEvidenceBounds (fst <$> Map.lookupMax entries)
    validateEvidenceBounds key =
      unless (key > controlIndex 0 && key <= state.controlIndex) (Left (ProjectionEvidenceBeyondCursor key state.controlIndex))
    validateHeraldCatalogueEntry (epoch, member) =
      unless
        (epoch == heraldMemberEpoch member)
        (Left (ProjectionHeraldCatalogueInvariant epoch (heraldMemberEpoch member)))
    validateProcessKey (key, bootstrap) =
      unless
        (key == appliedProcessEpochId bootstrap)
        (Left (ProjectionProcessKeyInvariant key (appliedProcessEpochId bootstrap)))
    validateStartedKey (key, started) =
      let process =
            processStartProcessEpochId
              (projectedStartedProcessBootstrap started)
       in unless
            (key == process)
            (Left (ProjectionStartedProcessKeyInvariant key process))
    validateEndedKey (key, ended) =
      unless
        (key == projectedEndedProcessEpoch ended)
        ( Left
            ( ProjectionEndedProcessKeyInvariant
                key
                (projectedEndedProcessEpoch ended)
            )
        )

data RetainedProjectionEvidence
  = RetainedAppliedEntry CanonicalAppliedOracleEntry
  | RetainedMembershipAdvance MembershipAdvanceEvidence

retainedProjectionEvidence ::
  State -> [(ControlIndex, RetainedProjectionEvidence)]
retainedProjectionEvidence state =
  sortOn
    fst
    ( fmap
        (\(index, entry) -> (index, RetainedAppliedEntry entry))
        (Map.toAscList (afterReplayBase state state.appliedEntries))
        <> fmap
          (\(index, evidence) -> (index, RetainedMembershipAdvance evidence))
          (Map.toAscList (afterReplayBase state state.membershipAdvances))
    )

-- Sparse old exact pins are not part of the replay stream. Splitting by
-- coordinate avoids enumerating covered evidence when selecting the suffix.
afterReplayBase :: State -> Map ControlIndex value -> Map ControlIndex value
afterReplayBase state = snd . Map.split (replayBaseControlIndex state)

replayProjectionEvidence ::
  State ->
  (ControlIndex, RetainedProjectionEvidence) ->
  Either OracleProjectionInvariant State
replayProjectionEvidence replayed (key, evidence) = do
  let expected = nextControlIndex replayed.controlIndex
  unless
    (key == expected)
    (Left (ProjectionEvidenceGap expected key))
  case evidence of
    RetainedAppliedEntry canonical -> do
      let entryIndex = appliedEntryControlIndex (canonicalAppliedOracleEntryValue canonical)
      unless
        (key == entryIndex)
        (Left (ProjectionEvidenceKeyMismatch key entryIndex))
      either
        (Left . ProjectionEvidenceInvalid key)
        Right
        (validateAndApplyNext canonical replayed)
    RetainedMembershipAdvance retained ->
      case retained of
        MembershipAdvanceEvidence embeddedIndex successor ends -> do
          unless
            (key == embeddedIndex)
            (Left (ProjectionMembershipEvidenceKeyMismatch key embeddedIndex))
          either
            (Left . ProjectionMembershipEvidenceInvalid key)
            Right
            (applyNewMembershipAdvance key successor ends replayed)

validateStartedBounds ::
  ControlIndex ->
  (ProcessEpochId, ProjectedStartedProcess) ->
  Either OracleProjectionInvariant ()
validateStartedBounds cursor (process, started) = do
  unless
    (projectedStartedProcessControlIndex started <= cursor)
    ( Left
        ( ProjectionStartedProcessBeyondCursor
            process
            (projectedStartedProcessControlIndex started)
            cursor
        )
    )

validateEndedBounds ::
  ControlIndex ->
  (ProcessEpochId, ProjectedEndedProcess) ->
  Either OracleProjectionInvariant ()
validateEndedBounds cursor (process, ended) = do
  unless
    (projectedEndedProcessControlIndex ended <= cursor)
    ( Left
        ( ProjectionEndedProcessBeyondCursor
            process
            (projectedEndedProcessControlIndex ended)
            cursor
        )
    )

replayOriginState :: State -> State
replayOriginState state = case state.replayBase of
  Nothing -> genesisOnlyState state
  Just base -> restoreReplayBase base state

restoreReplayBase :: ReplayBase -> State -> State
restoreReplayBase base state =
  state
    { Eclips.Herald.OracleProjection.State.controlIndex = base.baseControlIndex,
      Eclips.Herald.OracleProjection.State.heraldCatalogue = base.baseHeraldCatalogue,
      Eclips.Herald.OracleProjection.State.heraldAdmissions = base.baseHeraldAdmissions,
      Eclips.Herald.OracleProjection.State.currentHeraldMembership = base.baseCurrentHeraldMembership,
      Eclips.Herald.OracleProjection.State.heraldMembershipHistory = base.baseHeraldMembershipHistory,
      Eclips.Herald.OracleProjection.State.startedProcesses = base.baseStartedProcesses,
      Eclips.Herald.OracleProjection.State.endedProcesses = base.baseEndedProcesses,
      Eclips.Herald.OracleProjection.State.labelWorkflows = base.baseLabelWorkflows,
      Eclips.Herald.OracleProjection.State.completedLabelWorkflows = base.baseCompletedLabelWorkflows,
      Eclips.Herald.OracleProjection.State.failureProbes = base.baseFailureProbes,
      Eclips.Herald.OracleProjection.State.disappearanceProbes = base.baseDisappearanceProbes,
      Eclips.Herald.OracleProjection.State.voterConfiguration = base.baseVoterConfiguration,
      Eclips.Herald.OracleProjection.State.oracleReplicas = base.baseOracleReplicas,
      Eclips.Herald.OracleProjection.State.voterChanges = base.baseVoterChanges,
      Eclips.Herald.OracleProjection.State.pendingVoterChange = base.basePendingVoterChange,
      lastSemanticControlIndex = base.baseLastSemanticControlIndex,
      latestCanonicalStateDigest = base.baseLatestCanonicalStateDigest,
      membershipAdvances = Map.empty,
      appliedEntries = Map.empty
    }

genesisOnlyState :: State -> State
genesisOnlyState state =
  state
    { Eclips.Herald.OracleProjection.State.controlIndex = controlIndex 0,
      heraldCatalogue = state.genesisHeraldCatalogue,
      heraldAdmissions = Map.empty,
      Eclips.Herald.OracleProjection.State.currentHeraldMembership =
        state.genesisHeraldMembership,
      Eclips.Herald.OracleProjection.State.heraldMembershipHistory =
        singletonMembershipHistory state.genesisHeraldMembership,
      Eclips.Herald.OracleProjection.State.membershipAdvances = Map.empty,
      Eclips.Herald.OracleProjection.State.startedProcesses = Map.empty,
      Eclips.Herald.OracleProjection.State.endedProcesses = Map.empty,
      Eclips.Herald.OracleProjection.State.labelWorkflows = Map.empty,
      Eclips.Herald.OracleProjection.State.completedLabelWorkflows = Map.empty,
      Eclips.Herald.OracleProjection.State.failureProbes = Map.empty,
      Eclips.Herald.OracleProjection.State.disappearanceProbes = Map.empty,
      voterConfiguration = state.initialVoterConfiguration,
      oracleReplicas = state.initialOracleReplicas,
      voterChanges = Map.empty,
      pendingVoterChange = Nothing,
      Eclips.Herald.OracleProjection.State.appliedEntries = Map.empty,
      canonicalEvidenceCoveredThrough = controlIndex 0,
      lastSemanticControlIndex = controlIndex 0,
      latestCanonicalStateDigest = Nothing,
      replayBase = Nothing
    }

-- | A private reconstruction origin, preserving the original native-voter
-- bootstrap as well as immutable identity. The enclosing joining transaction
-- supplies its current canonical admission and keeps this origin unpublished.
joiningReplayOrigin :: Admission.HeraldAdmissionRecord -> State -> State
joiningReplayOrigin admission state = (genesisOnlyState state) {localStartupAdmission = Just admission}

-- | Adopt a checked reconstruction as one canonical owner. The caller binds
-- its pending admission and semantic consumers before publication; original
-- local startup provenance remains unchanged even after attempt invalidation.
adoptJoiningReplay :: State -> State -> Either OracleProjectionInvariant State
adoptJoiningReplay candidate current = do
  _ <- validateState candidate
  _ <- validateState current
  unless
    (projectionBaseGenesis candidate == projectionBaseGenesis current)
    (Left ProjectionInitialDigestInvariant)
  unless
    ( candidate.localHeraldEpoch == current.localHeraldEpoch
        && candidate.localStartupAdmission /= Nothing
        && current.localStartupAdmission /= Nothing
        && not (oracleViewLocalHeraldIsCurrent (oracleView candidate))
        && not (oracleViewLocalHeraldIsCurrent (oracleView current))
    )
    (Left (ProjectionLocalHeraldInvariant current.localHeraldEpoch))
  unless
    (candidate.controlIndex >= current.controlIndex)
    (Left (ProjectionCursorMismatch current.controlIndex candidate.controlIndex))
  let !successor = candidate {localStartupAdmission = current.localStartupAdmission}
  _ <- validateState successor
  pure successor

stateWitness :: State -> OracleProjectionStateWitness
stateWitness state =
  OracleProjectionStateWitness
    { witnessedControlIndex = state.controlIndex,
      witnessedAppliedEntries = Map.toAscList state.appliedEntries,
      witnessedStartedProcesses = Map.toAscList state.startedProcesses,
      witnessedEndedProcesses = Map.toAscList state.endedProcesses,
      witnessedPostStateDigest = projectedOracleStateDigest state
    }

projectionWitnessControlIndex :: OracleProjectionStateWitness -> ControlIndex
projectionWitnessControlIndex witness = witness.witnessedControlIndex

projectionWitnessAppliedEntries ::
  OracleProjectionStateWitness ->
  [(ControlIndex, CanonicalAppliedOracleEntry)]
projectionWitnessAppliedEntries witness = witness.witnessedAppliedEntries

projectionWitnessStartedProcesses ::
  OracleProjectionStateWitness ->
  [(ProcessEpochId, ProjectedStartedProcess)]
projectionWitnessStartedProcesses witness = witness.witnessedStartedProcesses

projectionWitnessEndedProcesses ::
  OracleProjectionStateWitness ->
  [(ProcessEpochId, ProjectedEndedProcess)]
projectionWitnessEndedProcesses witness = witness.witnessedEndedProcesses

projectionWitnessPostStateDigest :: OracleProjectionStateWitness -> Maybe OracleStateDigest
projectionWitnessPostStateDigest witness = witness.witnessedPostStateDigest

-- | Deliberately break the retained current-generation claim without changing
-- its checked evidence. Kept in the package-internal module solely so property
-- tests can prove that whole-state validation is evidence-derived.
replaceCurrentHeraldMembershipForInvariantTest ::
  HeraldMembershipGeneration -> State -> State
replaceCurrentHeraldMembershipForInvariantTest replacement state =
  state
    { Eclips.Herald.OracleProjection.State.currentHeraldMembership = replacement
    }

-- | Mismatches among the genesis facts independently admitted by the Oracle
-- and Herald boundaries.
data OracleGenesisParityFault
  = OracleGenesisSystemMismatch
  | OracleGenesisActiveHeraldsMismatch
  | OracleGenesisCatalogueMismatch
  | OracleGenesisPredefinedDescriptorsMismatch
  | OracleGenesisConfigurationDigestMismatch
  | OracleGenesisBootstrapsMismatch
  | OracleGenesisTopologyMismatch
  | OracleGenesisProjectionDigestMismatch
  | OracleGenesisOccurrencesMismatch
  | OracleGenesisControlIndexMismatch
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data OracleGenesisParityWitness = OracleGenesisParityWitness
  { systemId :: SystemId,
    activeHeraldEpochs :: [HeraldEpoch],
    initialProjectionDigest :: InitialProjectionDigest
  }
  deriving stock (Eq, Show)

validateOracleGenesisParity ::
  CheckedOracleGenesis ->
  State ->
  Either OracleGenesisParityFault OracleGenesisParityWitness
validateOracleGenesisParity oracleGenesis state = do
  require
    (OracleGenesis.checkedOracleSystemId oracleGenesis == state.systemId)
    OracleGenesisSystemMismatch
  require
    ( sort (OracleGenesis.checkedOracleActiveHeralds oracleGenesis)
        == sort (Map.elems state.genesisHeraldCatalogue)
    )
    OracleGenesisActiveHeraldsMismatch
  require
    (OracleGenesis.checkedOracleCatalogueDigest oracleGenesis == state.catalogueDigest)
    OracleGenesisCatalogueMismatch
  require
    ( normalizedDescriptors (OracleGenesis.checkedOraclePredefinedDescriptors oracleGenesis)
        == normalizedDescriptors (fmap predefinedCatalogueDescriptor profilePredefinedCatalogue)
    )
    OracleGenesisPredefinedDescriptorsMismatch
  require
    (OracleGenesis.checkedOracleConfigurationDigest oracleGenesis == state.configurationDigest)
    OracleGenesisConfigurationDigestMismatch
  require
    ( sortOn appliedProcessEpochId (OracleGenesis.checkedOracleAppliedBootstraps oracleGenesis)
        == sortOn appliedProcessEpochId (Map.elems state.genesisProcesses)
    )
    OracleGenesisBootstrapsMismatch
  require
    (OracleGenesis.checkedOracleInitialTopologyProjection oracleGenesis == state.initialTopology)
    OracleGenesisTopologyMismatch
  require
    (OracleGenesis.checkedOracleInitialProjectionDigest oracleGenesis == state.initialProjectionDigest)
    OracleGenesisProjectionDigestMismatch
  require
    (OracleGenesis.checkedOraclePredefinedOccurrences oracleGenesis == state.predefinedOccurrences)
    OracleGenesisOccurrencesMismatch
  require
    (OracleGenesis.checkedOracleControlIndex oracleGenesis == controlIndex 0)
    OracleGenesisControlIndexMismatch
  pure
    ( OracleGenesisParityWitness
        { systemId = state.systemId,
          activeHeraldEpochs = Map.keys state.heraldCatalogue,
          initialProjectionDigest = state.initialProjectionDigest
        }
    )
  where
    require condition problem = unless condition (Left problem)
    normalizedDescriptors = sortOn descriptorSortId

parityWitnessSystemId :: OracleGenesisParityWitness -> SystemId
parityWitnessSystemId witness = witness.systemId

parityWitnessActiveHeraldEpochs :: OracleGenesisParityWitness -> [HeraldEpoch]
parityWitnessActiveHeraldEpochs witness = witness.activeHeraldEpochs

parityWitnessInitialProjectionDigest :: OracleGenesisParityWitness -> InitialProjectionDigest
parityWitnessInitialProjectionDigest witness = witness.initialProjectionDigest

nextControlIndex :: ControlIndex -> ControlIndex
nextControlIndex index = controlIndex (controlIndexWord64 index + 1)
