{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure single-owner state for Oracle watch and semantic command dispatch.
module Eclips.Herald.OracleClient.State
  ( State,
    initialState,
    initialJoiningState,
    startJoiningParticipation,
    OracleClientStateWitness (..),
    oracleClientWitnessContacts,
    oracleClientWitnessAppliedCursor,
    oracleClientWitnessRequestedCursor,
    oracleClientWitnessBinding,
    oracleClientWitnessRetainedEntryBytes,
    oracleClientWitnessMembershipAdvances,
    oracleClientWitnessNextRequestSequence,
    oracleClientWitnessRequests,
    oracleClientWitnessLabelRetirement,
    oracleClientWitnessLabelConsumers,
    oracleClientWitnessLabelCompletionRequests,
    oracleClientWitnessRetired,
    oracleClientStateWitness,
    oracleClientHelloClaims,
    oracleClientContacts,
    oracleClientAppliedCursor,
    oracleClientRequestedCursor,
    oracleClientCurrentBinding,
    oracleClientLeaderHint,
    oracleClientBindingIsCurrent,
    oracleClientRetainedEntry,
    oracleClientRequestEvidence,
    reclaimOracleClientCanonicalPrefix,
    oracleClientCanonicalCoveredThrough,
    oracleClientProtectedPrefixEvidence,
    oracleClientMatchesCoveredEntry,
    CheckedJoiningClientBase,
    captureJoiningClientBase,
    captureJoiningClientBaseWithDiagnostics,
    captureJoiningClientBaseFromProjection,
    joiningClientBaseControlIndex,
    installJoiningClientBase,
    adoptJoiningReplay,
    compactOracleClientEvidence,
    retireSettledLabelRequests,
    compactLabelDecisionEvidence,
    compactCompletedBatch,
    canonicalEvidenceReleased,
    oracleClientEvidenceCompactedThrough,
    oracleClientRetainsEntry,
    oracleClientMatchesProjectedEntry,
    oracleClientMembershipAdvance,
    oracleClientActions,
    oracleClientRequestActions,
    oracleClientProgressActions,
    oracleClientProgressReady,
    oracleClientProgressConfirmed,
    observeLabelDecision,
    consumeLabelDecision,
    oracleClientReceiptRetirementReady,
    oracleClientReceiptRetirementProgressReady,
    oracleClientReceiptRetirementConfirmed,
    oracleClientReceiptRetirementProgressConfirmed,
    oracleClientLabelRequestActions,
    OracleRequestRef,
    oracleRequestRefRequestId,
    OracleRequestStatus (..),
    OracleSemanticIntent (..),
    OracleVoterOperation (..),
    OracleRequestWitness,
    oracleRequestWitnessRef,
    oracleRequestWitnessSemanticKey,
    oracleRequestWitnessIntent,
    oracleRequestWitnessBootstrap,
    oracleRequestWitnessEnvelope,
    oracleRequestWitnessStatus,
    OracleRequestDispatch,
    oracleRequestDispatchRef,
    oracleRequestDispatchEnvelope,
    oracleClientNextRequestSequence,
    oracleClientRequestEntries,
    lookupOracleRequest,
    lookupOracleRequestById,
    lookupOracleRequestBySemanticKey,
    oracleClientRequestDispatches,
    OracleRequestClassification (..),
    PreparedOracleRequest,
    prepareHeraldAdmissionRequest,
    prepareVoterAdministrationRequest,
    prepareFailureVoterCancellationRequest,
    oracleClientLocalReplica,
    prepareStartProcessEpochRequest,
    prepareEndProcessEpochRequest,
    prepareLabelCompletionRequest,
    prepareOpenHeraldFailureProbeRequest,
    prepareReportHeraldFailureProbeRequest,
    prepareDismissHeraldFailureProbeRequest,
    prepareAcceptVoterHostFailureRequest,
    prepareRetireHeraldEpochRequest,
    prepareOpenDisappearanceProbeRequest,
    preparePredefinedAbsenceReportRequest,
    prepareInvalidateDisappearanceProbeRequest,
    prepareResolveDisappearanceProbeRequest,
    prepareAbortDisappearanceProbeRequest,
    suppressDisappearanceProbeRequests,
    oracleClientDisappearanceRequestActions,
    PreparedLabelDecisionRequest,
    prepareLabelDecisionRequest,
    prepareDrainingLabelDecisionRequest,
    prepareDrainedLabelSubmission,
    prepareUnsentLabelEnd,
    preparedLabelDecisionRequestClassification,
    preparedLabelDecisionId,
    preparedLabelDecisionRequestRef,
    commitLabelDecisionRequest,
    preparedOracleRequestClassification,
    preparedOracleRequestRef,
    commitOracleRequest,
    OracleRequestResult (..),
    OracleResultClassification (..),
    PreparedOracleResult,
    prepareOracleResult,
    preparedOracleResultClassification,
    preparedOracleResult,
    commitOracleResult,
    interpretLabelCompletionEventView,
    labelCompletionSemanticKey,
    PreparedOracleSubmissionDeferral,
    prepareOracleSubmissionDeferral,
    commitOracleSubmissionDeferral,
    releaseCompletedOracleDeferrals,
    OracleClientProblem (..),
    PreparedClientIngress,
    prepareClientIngress,
    observeOracleContactHints,
    preferOracleVoters,
    preparedClientIngressActions,
    commitClientIngress,
    PreparedCursorAdvance,
    prepareCursorAdvance,
    commitCursorAdvance,
    PreparedMembershipCursorAdvance,
    prepareMembershipCursorAdvance,
    prepareCanonicalMembershipCursorAdvance,
    commitMembershipCursorAdvance,
    prepareCommittedPrefixProgress,
    prepareRewatch,
    prepareDrain,
    validateState,
  )
where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Serialize.Put qualified as Serialize
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceClaim,
    DisappearanceEvidenceDigest,
    DisappearanceOpenResult,
    DisappearanceOpenResultView (..),
    DisappearanceProbeId,
    DisappearanceResolutionOutcome,
    DisappearanceSubject,
    DisappearanceSubjectMembershipCoordinate,
    disappearanceCoordinateMemberSetDigest,
    disappearanceCoordinateMembershipGenerationId,
    disappearanceEvidenceClaimProbeId,
    disappearanceEvidenceClaimReporter,
    disappearanceOpenResultView,
    disappearanceProbeIdBytes,
    disappearanceSubjectCanonicalBytes,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    ProcessEpochId,
    SystemId,
    controlIndex,
    controlIndexWord64,
    heraldEpochBytes,
    labelDecisionIdBytes,
  )
import Eclips.Domain.Label
  ( HomeLabelAcceptanceCut,
    InitialLabelEvidence,
    LabelOutcomeDigest,
    LabelTarget,
    PriorAuthorityJustification,
  )
import Eclips.Domain.MemberSet (memberSetDigestBytes)
import Eclips.Domain.Membership
  ( FailureProbeResolutionId,
    HeraldFailureProbeId,
    HeraldMembershipGenerationId,
    failureProbeResolutionIdCanonicalBytes,
    heraldFailureProbeIdCanonicalBytes,
    heraldMembershipGenerationChangeControlIndex,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
  )
import Eclips.Domain.ProcessLifecycle (ProcessEndReason)
import Eclips.Domain.ProcessStart
  ( ProcessStart,
    processStartProcessEpochId,
    processStartProcessId,
    processStartResidence,
  )
import Eclips.Domain.Startup (CatalogueDigest, ConfigurationDigest, InitialProjectionDigest)
import Eclips.Domain.Value (Label)
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks (..))
import Eclips.Herald.OracleClient.Internal
  ( OracleBinding (..),
    OracleBindingGeneration (..),
    OracleClientAction (..),
    OracleClientDiagnostic (..),
    OracleClientIngress (..),
    OracleConnectAttempt (..),
    OracleContact (..),
    OracleContactSet (..),
    OracleHelloAcceptance (..),
    OracleHelloClaims (..),
    OracleLane (..),
    OracleNodeClaim,
    OracleObservedTerm,
    OracleRedirect (..),
    OracleRetry (..),
    OracleRetryPurpose (..),
  )
import Eclips.Herald.OracleClient.Request
  ( OracleRequestDispatch (..),
    OracleRequestRef (..),
    OracleRequestStatus (..),
    OracleRequestWitness (..),
    OracleSemanticIntent (..),
    OracleVoterOperation (..),
    oracleRequestDispatchEnvelope,
    oracleRequestDispatchRef,
    oracleRequestRefRequestId,
    oracleRequestWitnessBootstrap,
    oracleRequestWitnessEnvelope,
    oracleRequestWitnessIntent,
    oracleRequestWitnessRef,
    oracleRequestWitnessSemanticKey,
    oracleRequestWitnessStatus,
  )
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.PeerLiveness.Internal
  ( PeerRecoveryGeneration,
    peerRecoveryGenerationWord64,
  )
import Eclips.Oracle.Admission qualified as Admission
import Eclips.Oracle.Canonical
  ( CanonicalAppliedOracleEntry,
    CanonicalOracleEnvelope,
    canonicalAppliedOracleEntryBytes,
    canonicalAppliedOracleEntryValue,
    canonicalOracleEnvelopeDigest,
    canonicalOracleEnvelopeValue,
    canonicalizeOracleEnvelope,
  )
import Eclips.Oracle.Command
  ( EndProcessCommandProblem,
    OracleCommand,
    ProbeResult,
    abortDisappearanceProbeCommand,
    acceptVoterHostFailureCommand,
    beginVoterChangeCommand,
    cancelVoterChangeCommand,
    completeLabelDecisionCommand,
    decideLabelCommand,
    dismissHeraldFailureProbeCommand,
    endProcessEpochCommand,
    heraldAdmissionCommand,
    invalidateDisappearanceProbeCommand,
    openDisappearanceProbeCommand,
    openHeraldFailureProbeCommand,
    oracleEnvelope,
    oracleEnvelopeCommand,
    oracleEnvelopeExpectedControlIndex,
    oracleEnvelopeHomeHeraldEpoch,
    oracleEnvelopeRequestId,
    oracleEnvelopeWithProgress,
    registerOracleReplicaCommand,
    reportHeraldFailureProbeCommand,
    reportPredefinedAbsenceCommand,
    resolveDisappearanceProbeCommand,
    retireHeraldEpochCommand,
    retireOracleProgressCommand,
    startProcessEpochCommand,
  )
import Eclips.Oracle.Disappearance
  ( CompleteDisappearanceEvidenceDigest,
    DisappearanceAbortReason,
    DisappearanceCommandResult (..),
    DisappearanceInvalidationReason,
    DisappearanceProbeAbortReasonView (..),
    DisappearanceProbeInvalidationView (..),
    projectedDisappearanceProbeHeaderCoordinate,
    projectedDisappearanceProbeHeaderId,
    projectedDisappearanceProbeHeaderSubject,
  )
import Eclips.Oracle.Failure qualified as Failure
import Eclips.Oracle.Identity
  ( OracleClientRequestId,
    oracleClientRequestHome,
    oracleClientRequestId,
    oracleClientRequestSequence,
  )
import Eclips.Oracle.Label
  ( LabelCompletionAttestation,
    ProcessOriginView (DynamicStartView),
    deriveLabelDecisionId,
    labelCompletionCollector,
    labelCompletionControlIndex,
    labelCompletionDecisionId,
    labelCompletionMembershipGeneration,
    labelCompletionOutcomeDigest,
    liveDecisionCaller,
    liveDecisionExpectedLabel,
    liveDecisionHomeAcceptanceCut,
    liveDecisionId,
    liveDecisionInitialLabelEvidence,
    liveDecisionObject,
    liveDecisionOpenRequestId,
    liveDecisionRequestControlIndex,
    liveDecisionTarget,
    processRecordOrigin,
    processRecordProcessEpoch,
    processRecordProcessId,
    processRecordResidence,
  )
import Eclips.Oracle.Progress (OracleProgress, oracleProgress, oracleProgressCovers, oracleProgressLabelsThrough, oracleProgressReceipts)
import Eclips.Oracle.Projection
  ( AppliedOracleEntry,
    OracleProjectionEvent,
    OracleProjectionEventView (..),
    appliedEntryCommand,
    appliedEntryCommandDigest,
    appliedEntryConfiguration,
    appliedEntryControlIndex,
    appliedEntryOracleProgress,
    appliedEntryProjectionEvents,
    appliedEntryReceipt,
    appliedEntryRequestId,
    oracleProjectionEventView,
  )
import Eclips.Oracle.Receipt
  ( FailureCommandResult (..),
    FailureResolutionReady,
    OracleReceiptResult (OracleAccepted, OracleRejected),
    OracleRejection,
    OracleRequestRetirement (..),
    oracleReceiptDisappearanceResult,
    oracleReceiptFailureResult,
    oracleReceiptResult,
    oracleReceiptVoterResult,
  )
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import Eclips.Raft.Identity (RaftNodeId)

data Phase
  = Joining
  | Connecting OracleConnectAttempt
  | Retrying OracleRetry
  | Bound OracleBinding
  | Retired
  deriving stock (Eq, Show)

-- A pending decision retains only its own reference count and preceding actual
-- label coordinate. Intervening maintenance or other commands cannot advance a
-- label-only promise when the next decision is observed but still blocked.
data LabelPin = LabelPin !Int !ControlIndex
  deriving stock (Eq, Show)

-- Exact decision coordinates derived only from a checked Projection base when
-- its original canonical entries have already been covered. These sparse
-- global ordering facts never supply a local command receipt or result.
newtype ImportedLabelEvidence = ImportedLabelEvidence (Map ControlIndex LabelDecisionId)
  deriving stock (Eq, Show)

data State = State
  { claims :: OracleHelloClaims,
    contacts :: OracleContactSet,
    preferredVoters :: Set OracleNodeClaim,
    localReplica :: Maybe (RaftNodeId, Voter.OracleReplicaContact),
    initialCursor :: ControlIndex,
    appliedCursor :: ControlIndex,
    lastSemanticCursor :: ControlIndex,
    requestedCursor :: ControlIndex,
    retainedEntries :: !(Map ControlIndex CanonicalAppliedOracleEntry),
    canonicalCoveredThrough :: !ControlIndex,
    evidenceCompactedThrough :: !ControlIndex,
    labelEvidenceCompactedThrough :: !ControlIndex,
    membershipAdvances :: Map ControlIndex HeraldMembershipGenerationId,
    nextRequestSequence :: Word64,
    receiptRetirementReady :: ReceiptRetirement,
    receiptRetirementConfirmed :: ReceiptRetirement,
    receiptOutstanding :: Set Word64,
    latestLabelDecision :: !ControlIndex,
    labelRetirementReady :: !ControlIndex,
    labelRetirementConfirmed :: !ControlIndex,
    labelConsumers :: !(Map LabelDecisionId ControlIndex),
    labelCompletionRequests :: !(Map OracleRequestRef ControlIndex),
    settledLabelRequests :: !(Map ControlIndex (Set OracleRequestRef)),
    labelPins :: !(Map ControlIndex LabelPin),
    importedLabelEvidence :: !ImportedLabelEvidence,
    receiptHomeClosed :: Bool,
    oracleRequests :: Map OracleRequestRef OracleRequestWitness,
    requestPreparedAt :: Map OracleRequestRef ControlIndex,
    requestObservedAt :: !(Map OracleRequestRef ControlIndex),
    oracleRequestKeys :: Map ByteString OracleRequestRef,
    suppressedDisappearanceRequests :: Map OracleRequestRef (DisappearanceProbeId, ControlIndex),
    phase :: Phase,
    nextContactOffset :: Int,
    nextAttemptOrdinal :: Word64,
    nextRetryOrdinal :: Word64,
    nextBindingGeneration :: Word64,
    pendingRequestRetry :: Maybe OracleRetry,
    observedTerm :: Maybe OracleObservedTerm,
    leaderHint :: Maybe OracleNodeClaim
  }
  deriving stock (Eq, Show)

-- | Stable facts used by composed invariants and deterministic replay tests.
data OracleClientStateWitness = OracleClientStateWitness
  { oracleClientWitnessContacts :: [OracleContact],
    oracleClientWitnessAppliedCursor :: ControlIndex,
    oracleClientWitnessLastSemanticCursor :: ControlIndex,
    oracleClientWitnessRequestedCursor :: ControlIndex,
    oracleClientWitnessBinding :: Maybe OracleBinding,
    oracleClientWitnessRetainedEntryBytes :: [(ControlIndex, ByteString)],
    oracleClientWitnessMembershipAdvances ::
      [(ControlIndex, HeraldMembershipGenerationId)],
    oracleClientWitnessNextRequestSequence :: Word64,
    oracleClientWitnessReceiptRetirement :: (Word64, Word64, Bool),
    oracleClientWitnessLabelRetirement :: (ControlIndex, ControlIndex, ControlIndex),
    oracleClientWitnessLabelConsumers :: [(LabelDecisionId, ControlIndex)],
    oracleClientWitnessLabelCompletionRequests :: [(OracleRequestRef, ControlIndex)],
    oracleClientWitnessRequests ::
      [(OracleRequestRef, OracleRequestWitness)],
    oracleClientWitnessRetired :: Bool
  }
  deriving stock (Eq, Show)

data OracleClientProblem
  = OracleClientStateContradiction
  | OracleJoiningBaseReceiverNotFresh
  | OracleJoiningBaseIdentityMismatch
  | OracleCanonicalPrefixAhead ControlIndex ControlIndex
  | OracleEntriesRequireProjection
  | OracleDeferralRequiresProjection
  | OracleCursorNotNext ControlIndex ControlIndex
  | OracleMembershipCursorNotBound
  | OracleMembershipCursorEntryMismatch
  | OracleRequestConflict ByteString
  | OracleRequestMissing OracleRequestRef
  | OracleRequestEntryMissing ControlIndex
  | OracleRequestEntryConflict ControlIndex
  | OracleRequestIdMismatch
  | OracleRequestDigestMismatch
  | OracleRequestBootstrapMismatch
  | OracleRequestControlMismatch
  | OracleRequestResultMismatch
  | OracleRequestRetiredBeforeSettlement OracleClientRequestId
  | OracleSubmissionDeferralUnsupportedIntent OracleClientRequestId
  | OracleSubmissionDeferralConflict OracleClientRequestId
  | OracleEndProcessCommandProblem EndProcessCommandProblem
  deriving stock (Eq, Show)

data PreparedClientIngress = PreparedClientIngress State [OracleClientAction]

data PreparedCursorAdvance = PreparedCursorAdvance State

data PreparedMembershipCursorAdvance = PreparedMembershipCursorAdvance State

initialState ::
  OracleHelloClaims ->
  OracleContactSet ->
  ControlIndex ->
  State
initialState claims contacts initialCursor =
  fst (beginConnect Nothing (initialJoiningState claims contacts initialCursor))

-- | Canonical onboarding replay owns a cursor without a physical Oracle lane.
-- The first ordinary connection is allocated only after self activation.
initialJoiningState :: OracleHelloClaims -> OracleContactSet -> ControlIndex -> State
initialJoiningState claims contacts initialCursor =
  State
    { claims,
      contacts,
      preferredVoters = Set.empty,
      localReplica = Nothing,
      initialCursor,
      appliedCursor = initialCursor,
      lastSemanticCursor = initialCursor,
      requestedCursor = initialCursor,
      retainedEntries = Map.empty,
      canonicalCoveredThrough = initialCursor,
      evidenceCompactedThrough = initialCursor,
      labelEvidenceCompactedThrough = controlIndex 0,
      membershipAdvances = Map.empty,
      nextRequestSequence = 1,
      receiptRetirementReady = Lifetime.receiptRetirementPrefix (Just 0),
      receiptRetirementConfirmed = Lifetime.receiptRetirementPrefix (Just 0),
      receiptOutstanding = Set.empty,
      latestLabelDecision = controlIndex 0,
      labelRetirementReady = controlIndex 0,
      labelRetirementConfirmed = controlIndex 0,
      labelConsumers = Map.empty,
      labelCompletionRequests = Map.empty,
      settledLabelRequests = Map.empty,
      labelPins = Map.empty,
      importedLabelEvidence = ImportedLabelEvidence Map.empty,
      receiptHomeClosed = False,
      oracleRequests = Map.empty,
      requestPreparedAt = Map.empty,
      requestObservedAt = Map.empty,
      oracleRequestKeys = Map.empty,
      suppressedDisappearanceRequests = Map.empty,
      phase = Joining,
      nextContactOffset = 0,
      nextAttemptOrdinal = 1,
      nextRetryOrdinal = 1,
      nextBindingGeneration = 1,
      pendingRequestRetry = Nothing,
      observedTerm = Nothing,
      leaderHint = Nothing
    }

-- | Global control facts needed by a fresh observer's client. Exact archives
-- and their covered prefix are separate staging evidence; neither contains donor requests,
-- receipt promises, completion offers, routing state or an earlier client.
data CheckedJoiningClientBase
  = CheckedJoiningClientBase
      !(SystemId, CatalogueDigest, ConfigurationDigest, InitialProjectionDigest)
      !ControlIndex
      !ControlIndex
      !HeraldMembershipGenerationId
      !ControlIndex
      !(Map LabelDecisionId ControlIndex)
      !(Map ControlIndex LabelPin)
      !ImportedLabelEvidence
      !JoiningClientBaseArchive
  deriving stock (Eq, Show)

data JoiningClientBaseArchive
  = JoiningClientBaseArchive
      !ControlIndex
      !(Map ControlIndex CanonicalAppliedOracleEntry)
      !(Map ControlIndex HeraldMembershipGenerationId)
  deriving stock (Eq, Show)

captureJoiningClientBase :: Projection.State -> Either OracleClientProblem CheckedJoiningClientBase
captureJoiningClientBase = captureJoiningClientBaseWithDiagnostics DiagnosticChecksEnabled

-- | The caller's local diagnostic policy also controls the nested Projection
-- audit; consumer-pin construction and missing-evidence checks are unconditional.
captureJoiningClientBaseWithDiagnostics :: DiagnosticChecks -> Projection.State -> Either OracleClientProblem CheckedJoiningClientBase
captureJoiningClientBaseWithDiagnostics checks projection = do
  captured <- either (const (Left OracleClientStateContradiction)) Right (Projection.captureProjectionBaseWithDiagnostics checks projection)
  captureJoiningClientBaseFromProjection captured

-- | Derive client progress from an already checked, detached Projection capture.
-- Consumer pins and missing-evidence checks remain unconditional; composition
-- can share the captured archive without recapturing or reauditing its owner.
captureJoiningClientBaseFromProjection :: Projection.ProjectionBaseCapture -> Either OracleClientProblem CheckedJoiningClientBase
captureJoiningClientBaseFromProjection captured = do
  let !entries = Projection.projectionBaseAppliedEntries captured
      !memberships = Map.fromList [(index, heraldMembershipGenerationId generation) | generation <- Projection.projectionBaseHeraldMembershipHistory captured, Just index <- [heraldMembershipGenerationChangeControlIndex generation]]
      decisions = [(decision, liveDecisionRequestControlIndex (Projection.projectedLabelWorkflowDecision workflow), Projection.projectedLabelWorkflowPhase workflow) | (decision, workflow) <- Projection.projectionBaseLabelWorkflows captured]
      !latest = maximum (controlIndex 0 : [index | (_, index, _) <- decisions])
      !consumers = Map.fromList [(decision, index) | (decision, index, Projection.LabelWorkflowReleased) <- decisions]
      decisionIds = Map.fromList [(index, decision) | (decision, index, _) <- decisions]
      decisionIndices = Map.keys decisionIds
      previousLabels = Map.fromAscList (zip decisionIndices (controlIndex 0 : decisionIndices))
      !lastSemantic = Projection.projectionBaseLastSemanticControlIndex captured
      !identity = Projection.projectionBaseIdentity captured
      !prefix = Projection.projectionBaseControlIndex captured
      !membership = Projection.projectionBaseCurrentHeraldMembershipId captured
      !covered = Projection.projectionBaseCanonicalCoveredThrough captured
  pins <- Map.traverseWithKey (\index count -> maybe (Left OracleClientStateContradiction) (Right . LabelPin count) (Map.lookup index previousLabels)) (Map.fromListWith (+) [(index, 1) | index <- Map.elems consumers])
  let anchors = Set.delete (controlIndex 0) (Set.fromList (latest : Map.keys pins <> [previous | LabelPin _ previous <- Map.elems pins]))
      missing = Set.filter (`Map.notMember` entries) anchors
      !imported = ImportedLabelEvidence (Map.restrictKeys decisionIds missing)
  unless (all (<= covered) missing) (Left OracleClientStateContradiction)
  let !retained =
        if covered == controlIndex 0
          then entries
          else Map.restrictKeys entries (Set.filter (<= covered) anchors) `Map.union` snd (Map.split covered entries)
      !archive = JoiningClientBaseArchive covered retained memberships
  pure (CheckedJoiningClientBase identity prefix lastSemantic membership latest consumers pins imported archive)

joiningClientBaseControlIndex :: CheckedJoiningClientBase -> ControlIndex
joiningClientBaseControlIndex (CheckedJoiningClientBase _ prefix _ _ _ _ _ _ _) = prefix

-- | Adopt global progress without replaying semantic controls. This first seam
-- accepts a pristine genesis observer and preserves its original initial cursor,
-- identity, request allocator, receipt state, routing and Joining phase.
installJoiningClientBase :: CheckedJoiningClientBase -> State -> Either OracleClientProblem State
installJoiningClientBase (CheckedJoiningClientBase identity prefix semantic membership latest consumers pins imported (JoiningClientBaseArchive covered entries memberships)) state = do
  validateState state
  if state.phase == Joining
    && state.initialCursor == controlIndex 0
    && state.appliedCursor == state.initialCursor
    && Map.null state.retainedEntries
    && Map.null state.membershipAdvances
    && Map.null state.oracleRequests
    && state.nextRequestSequence == 1
    && state.latestLabelDecision == controlIndex 0
    && Map.null state.labelConsumers
    && Map.null state.labelCompletionRequests
    then Right ()
    else Left OracleJoiningBaseReceiverNotFresh
  if identity == (state.claims.systemId, state.claims.catalogueDigest, state.claims.configurationDigest, state.claims.initialProjectionDigest)
    then Right ()
    else Left OracleJoiningBaseIdentityMismatch
  let !successor =
        advanceLabelRetirement
          state
            { claims = state.claims {membershipGeneration = membership},
              appliedCursor = prefix,
              requestedCursor = prefix,
              lastSemanticCursor = semantic,
              retainedEntries = entries,
              canonicalCoveredThrough = covered,
              evidenceCompactedThrough = covered,
              membershipAdvances = memberships,
              latestLabelDecision = latest,
              labelConsumers = consumers,
              labelPins = pins,
              importedLabelEvidence = imported
            }
  validateState successor
  pure successor

-- | Adopt the canonical progress of a reconstructed passive observer. Local
-- receipt promises, allocation and routing remain owned by the live client.
-- Pending observers cannot issue Oracle commands; a retained request here is
-- a kernel contradiction, not authority to replay against discarded staging.
adoptJoiningReplay :: State -> State -> Either OracleClientProblem State
adoptJoiningReplay candidate current = do
  validateState candidate
  validateState current
  unless
    ( candidate.phase == Joining
        && current.phase == Joining
        && Map.null candidate.oracleRequests
        && Map.null current.oracleRequests
        && candidate.initialCursor == current.initialCursor
        && candidate.appliedCursor >= current.appliedCursor
        && candidate.claims {membershipGeneration = current.claims.membershipGeneration} == current.claims
    )
    (Left OracleClientStateContradiction)
  let !successor =
        current
          { claims = current.claims {membershipGeneration = candidate.claims.membershipGeneration},
            appliedCursor = candidate.appliedCursor,
            lastSemanticCursor = candidate.lastSemanticCursor,
            requestedCursor = candidate.requestedCursor,
            retainedEntries = candidate.retainedEntries,
            canonicalCoveredThrough = candidate.canonicalCoveredThrough,
            evidenceCompactedThrough = candidate.evidenceCompactedThrough,
            labelEvidenceCompactedThrough = candidate.labelEvidenceCompactedThrough,
            membershipAdvances = candidate.membershipAdvances,
            latestLabelDecision = candidate.latestLabelDecision,
            labelRetirementReady = candidate.labelRetirementReady,
            labelConsumers = candidate.labelConsumers,
            labelPins = candidate.labelPins,
            importedLabelEvidence = candidate.importedLabelEvidence
          }
  validateState successor
  pure successor

-- Joining replay consumes every historical label before service. The checked
-- activation cut may be later than its seal, so retain the complete replay's H.
startJoiningParticipation :: State -> Either OracleClientProblem State
startJoiningParticipation state = do
  validateOperationalState state
  case state.phase of
    Joining
      | Map.null state.labelConsumers,
        Map.null state.labelCompletionRequests,
        state.labelRetirementReady == state.latestLabelDecision -> do
          let (successor, _) = beginConnect Nothing state
          validateOperationalState successor
          pure successor
    _ -> Left OracleClientStateContradiction

oracleClientStateWitness :: State -> OracleClientStateWitness
oracleClientStateWitness state =
  OracleClientStateWitness
    { oracleClientWitnessContacts = NonEmpty.toList contacts,
      oracleClientWitnessAppliedCursor = state.appliedCursor,
      oracleClientWitnessLastSemanticCursor = state.lastSemanticCursor,
      oracleClientWitnessRequestedCursor = state.requestedCursor,
      oracleClientWitnessBinding = case state.phase of
        Bound binding -> Just binding
        _ -> Nothing,
      oracleClientWitnessRetainedEntryBytes =
        [ (index, canonicalAppliedOracleEntryBytes entry)
        | (index, entry) <- Map.toAscList state.retainedEntries
        ],
      oracleClientWitnessMembershipAdvances =
        Map.toAscList state.membershipAdvances,
      oracleClientWitnessNextRequestSequence = state.nextRequestSequence,
      oracleClientWitnessReceiptRetirement = (contiguousReceiptPrefix state.receiptRetirementReady, contiguousReceiptPrefix state.receiptRetirementConfirmed, state.receiptHomeClosed),
      oracleClientWitnessLabelRetirement = (state.latestLabelDecision, state.labelRetirementReady, state.labelRetirementConfirmed),
      oracleClientWitnessLabelConsumers = Map.toAscList state.labelConsumers,
      oracleClientWitnessLabelCompletionRequests = Map.toAscList state.labelCompletionRequests,
      oracleClientWitnessRequests =
        Map.toAscList state.oracleRequests,
      oracleClientWitnessRetired = case state.phase of
        Retired -> True
        _ -> False
    }
  where
    OracleContactSet contacts = state.contacts

oracleClientHelloClaims :: State -> OracleHelloClaims
oracleClientHelloClaims = (.claims)

oracleClientHome :: State -> HeraldEpoch
oracleClientHome = (.heraldEpoch) . (.claims)

oracleClientSystem :: State -> SystemId
oracleClientSystem = (.systemId) . (.claims)

oracleClientWitnessContacts :: OracleClientStateWitness -> [OracleContact]
oracleClientWitnessContacts witness = witness.oracleClientWitnessContacts

oracleClientWitnessAppliedCursor :: OracleClientStateWitness -> ControlIndex
oracleClientWitnessAppliedCursor witness = witness.oracleClientWitnessAppliedCursor

oracleClientWitnessRequestedCursor :: OracleClientStateWitness -> ControlIndex
oracleClientWitnessRequestedCursor witness = witness.oracleClientWitnessRequestedCursor

oracleClientWitnessBinding :: OracleClientStateWitness -> Maybe OracleBinding
oracleClientWitnessBinding witness = witness.oracleClientWitnessBinding

oracleClientWitnessRetainedEntryBytes ::
  OracleClientStateWitness ->
  [(ControlIndex, ByteString)]
oracleClientWitnessRetainedEntryBytes witness = witness.oracleClientWitnessRetainedEntryBytes

oracleClientWitnessMembershipAdvances ::
  OracleClientStateWitness ->
  [(ControlIndex, HeraldMembershipGenerationId)]
oracleClientWitnessMembershipAdvances witness =
  witness.oracleClientWitnessMembershipAdvances

oracleClientWitnessNextRequestSequence :: OracleClientStateWitness -> Word64
oracleClientWitnessNextRequestSequence witness =
  witness.oracleClientWitnessNextRequestSequence

oracleClientWitnessRequests ::
  OracleClientStateWitness ->
  [(OracleRequestRef, OracleRequestWitness)]
oracleClientWitnessRequests witness =
  witness.oracleClientWitnessRequests

oracleClientWitnessLabelRetirement :: OracleClientStateWitness -> (ControlIndex, ControlIndex, ControlIndex)
oracleClientWitnessLabelRetirement witness = witness.oracleClientWitnessLabelRetirement

oracleClientWitnessLabelConsumers :: OracleClientStateWitness -> [(LabelDecisionId, ControlIndex)]
oracleClientWitnessLabelConsumers witness = witness.oracleClientWitnessLabelConsumers

oracleClientWitnessLabelCompletionRequests :: OracleClientStateWitness -> [(OracleRequestRef, ControlIndex)]
oracleClientWitnessLabelCompletionRequests witness = witness.oracleClientWitnessLabelCompletionRequests

oracleClientWitnessRetired :: OracleClientStateWitness -> Bool
oracleClientWitnessRetired witness = witness.oracleClientWitnessRetired

oracleClientContacts :: State -> OracleContactSet
oracleClientContacts = (.contacts)

oracleClientAppliedCursor :: State -> ControlIndex
oracleClientAppliedCursor = (.appliedCursor)

oracleClientRequestedCursor :: State -> ControlIndex
oracleClientRequestedCursor = (.requestedCursor)

oracleClientLeaderHint :: State -> Maybe OracleNodeClaim
oracleClientLeaderHint = (.leaderHint)

oracleClientCurrentBinding :: State -> Maybe OracleBinding
oracleClientCurrentBinding state = case state.phase of
  Bound binding -> Just binding
  _ -> Nothing

oracleClientBindingIsCurrent :: OracleBinding -> State -> Bool
oracleClientBindingIsCurrent binding state =
  oracleClientCurrentBinding state == Just binding

oracleClientRetainedEntry ::
  ControlIndex ->
  State ->
  Maybe CanonicalAppliedOracleEntry
oracleClientRetainedEntry index = Map.lookup index . (.retainedEntries)

-- | Exact evidence for an existing local request observed through the checked
-- contiguous cursor. Observation precedes result settlement: a first result is
-- available while its witness still awaits projection. Request retirement
-- releases this sparse coordinate together with the witness.
oracleClientRequestEvidence :: OracleRequestRef -> State -> Maybe CanonicalAppliedOracleEntry
oracleClientRequestEvidence reference state = do
  index <- Map.lookup reference state.requestObservedAt
  Map.lookup index state.retainedEntries

oracleClientCanonicalCoveredThrough :: State -> ControlIndex
oracleClientCanonicalCoveredThrough = (.canonicalCoveredThrough)

-- | Exact exceptions retained below the covered prefix. Composition preserves
-- these same bytes in its Projection archive before removing that prefix.
-- This map contains no suffix and requires no scan through local requests.
oracleClientProtectedPrefixEvidence :: State -> Map ControlIndex CanonicalAppliedOracleEntry
oracleClientProtectedPrefixEvidence state =
  let (older, atBoundary, _) = Map.splitLookup state.canonicalCoveredThrough state.retainedEntries
   in maybe older (\entry -> Map.insert state.canonicalCoveredThrough entry older) atBoundary

-- | A completed composed batch may replace its canonical prefix by coverage
-- and exact sparse consumer evidence. This private seam does not acknowledge
-- receipts, consume labels, settle requests, or advance the watch cursor.
-- The caller must coordinate the same covered prefix with checked Projection.
reclaimOracleClientCanonicalPrefix :: ControlIndex -> State -> Either OracleClientProblem State
reclaimOracleClientCanonicalPrefix through initial = do
  validateOperationalState initial
  unless (through <= initial.appliedCursor) (Left (OracleCanonicalPrefixAhead initial.appliedCursor through))
  if through < initial.canonicalCoveredThrough
    then Right initial
    else do
      state <- compactOracleClientEvidence initial
      let anchors = protectedCanonicalIndices state
          (older, atBoundary, later) = Map.splitLookup through state.retainedEntries
          prefix = maybe older (\entry -> Map.insert through entry older) atBoundary
          !retained = Map.union (Map.restrictKeys prefix anchors) later
          !successor = pruneImportedLabelEvidence state {canonicalCoveredThrough = through, retainedEntries = retained}
      pure successor

protectedCanonicalIndices :: State -> Set ControlIndex
protectedCanonicalIndices state =
  (labelEvidenceCoordinates state `Set.difference` Map.keysSet imported)
    `Set.union` Set.fromList
      ( Map.elems state.requestObservedAt
          <> [index | witness <- Map.elems state.oracleRequests, index <- requestCoordinates witness]
          <> [index | (_, index) <- Map.elems state.suppressedDisappearanceRequests]
      )
  where
    ImportedLabelEvidence imported = state.importedLabelEvidence
    requestCoordinates witness = case oracleRequestWitnessStatus witness of
      OracleRequestProjected index -> [index]
      OracleRequestEndedBeforeSubmission index -> [index]
      _ -> []

labelEvidenceCoordinates :: State -> Set ControlIndex
labelEvidenceCoordinates state =
  Set.fromList (state.latestLabelDecision : state.labelEvidenceCompactedThrough : Map.keys state.labelPins <> [previous | LabelPin _ previous <- Map.elems state.labelPins])

pruneImportedLabelEvidence :: State -> State
pruneImportedLabelEvidence state
  | Map.null imported = state
  | otherwise = state {importedLabelEvidence = ImportedLabelEvidence (Map.restrictKeys imported (labelEvidenceCoordinates state))}
  where
    ImportedLabelEvidence imported = state.importedLabelEvidence

-- | Covered history is already applied, but no longer proves supplied bytes
-- equal to the original. A retained exception still requires exact equality.
-- This predicate never authorizes request result settlement or receipt changes.
oracleClientMatchesCoveredEntry :: CanonicalAppliedOracleEntry -> State -> Bool
oracleClientMatchesCoveredEntry canonical state
  | index <= state.initialCursor || index > state.canonicalCoveredThrough = False
  | otherwise = case Map.lookup index state.retainedEntries of
      Just retained -> retained == canonical
      Nothing -> True
  where
    index = appliedEntryControlIndex (canonicalAppliedOracleEntryValue canonical)

oracleClientEvidenceCompactedThrough :: State -> ControlIndex
oracleClientEvidenceCompactedThrough = (.evidenceCompactedThrough)

-- | Retire only this client's duplicate copies of completed-batch maintenance
-- and nonlocal unowned rejections. Projection separately owns canonical
-- replay evidence. Semantic/configuration entries and every locally prepared request
-- keep their exact evidence, independently of their age. Each range is visited
-- once, after its composed consumers have finished; no oldest pin holds back
-- this coordinate and no watch or semantic cursor is moved.
compactOracleClientEvidence :: State -> Either OracleClientProblem State
compactOracleClientEvidence state = do
  validateOperationalState state
  if state.evidenceCompactedThrough == state.appliedCursor
    then Right state
    else do
      let (earlier, atBoundary, recent) = Map.splitLookup state.evidenceCompactedThrough state.retainedEntries
          preserved = maybe earlier (\entry -> Map.insert state.evidenceCompactedThrough entry earlier) atBoundary
          !retained = Map.union preserved (Map.filter (not . discardableClientEntry state) recent)
          !successor = state {retainedEntries = retained, evidenceCompactedThrough = state.appliedCursor}
      Right successor

-- | Relinquish local request rediscovery only after the composed label
-- consumers and every completion offer have released the original decision.
-- Application replies have their own lifetime. Exact canonical watch evidence
-- remains retained independently of these request envelopes and semantic keys.
-- The ordered index visits only newly releasable requests, never older history.
retireSettledLabelRequests :: State -> Either OracleClientProblem State
retireSettledLabelRequests state = do
  validateOperationalState state
  let (earlier, atFrontier, later) = Map.splitLookup state.labelRetirementReady state.settledLabelRequests
      !references = Set.unions (maybe [] pure atFrontier <> Map.elems earlier)
      !retired = Map.restrictKeys state.oracleRequests references
      !keys = Set.fromList (map oracleRequestWitnessSemanticKey (Map.elems retired))
      !requests = Map.withoutKeys state.oracleRequests references
      !prepared = Map.withoutKeys state.requestPreparedAt references
      !observed = Map.withoutKeys state.requestObservedAt references
      !semanticKeys = Map.withoutKeys state.oracleRequestKeys keys
      !releasedEvidence =
        Set.fromList
          [ index
          | witness <- Map.elems retired,
            OracleRequestProjected index <- [oracleRequestWitnessStatus witness],
            index < state.labelEvidenceCompactedThrough,
            Just canonical <- [Map.lookup index state.retainedEntries],
            isPlainLabelDecisionEntry canonical
          ]
      !evidence = Map.withoutKeys state.retainedEntries releasedEvidence
      !successor =
        state
          { settledLabelRequests = later,
            oracleRequests = requests,
            requestPreparedAt = prepared,
            requestObservedAt = observed,
            oracleRequestKeys = semanticKeys,
            retainedEntries = evidence
          }
  Right successor

-- | Discard duplicate plain decision entries behind the completed label
-- frontier. Keep its exact decision entry as the predecessor for the earliest
-- remaining pin. Each newly covered control interval is inspected once;
-- compound decisions retain their independent admission/disappearance facts.
-- Call after composed consumers and request retirement finish. A local result
-- not yet settled keeps sparse evidence; its eventual request retirement above
-- releases that entry without scanning this old interval again.
compactLabelDecisionEvidence :: State -> Either OracleClientProblem State
compactLabelDecisionEvidence initial = do
  state <- compactOracleClientEvidence initial
  if state.labelEvidenceCompactedThrough == state.labelRetirementReady
    then Right (pruneImportedLabelEvidence state)
    else do
      let frontier = state.labelRetirementReady
          (older, oldAnchor, newer) = Map.splitLookup state.labelEvidenceCompactedThrough state.retainedEntries
          window = maybe newer (\entry -> Map.insert state.labelEvidenceCompactedThrough entry newer) oldAnchor
          (covered, anchor, after) = Map.splitLookup frontier window
          !advanced = state {labelEvidenceCompactedThrough = frontier}
          !kept = Map.filter (not . discardableLabelDecisionEntry advanced) covered
          !evidence = Map.unions [older, kept, maybe Map.empty (Map.singleton frontier) anchor, after]
          !successor = advanced {retainedEntries = evidence}
      Right (pruneImportedLabelEvidence successor)

-- | Finish composed consumers before reclaiming their shared canonical
-- archive. Report same-cursor request/evidence release without comparing map
-- contents or walking retained request history a second time.
compactCompletedBatch :: State -> Either OracleClientProblem (State, Bool)
compactCompletedBatch initial = do
  retired <- retireSettledLabelRequests initial
  successor <- compactLabelDecisionEvidence retired
  let !released = canonicalEvidenceReleased initial successor
  pure (successor, released)

-- | Compare completed replay/compaction boundaries for actual removals.
-- Canonical cursor changes separately cover new entries and label consumers.
-- This is not a general change detector for arbitrary prepare/retire mixtures
-- which might add and remove equally many requests between observations.
canonicalEvidenceReleased :: State -> State -> Bool
canonicalEvidenceReleased before after =
  Map.size after.oracleRequests < Map.size before.oracleRequests
    || Map.size after.retainedEntries < Map.size before.retainedEntries

isPlainLabelDecisionEntry :: CanonicalAppliedOracleEntry -> Bool
isPlainLabelDecisionEntry canonical =
  appliedEntryConfiguration entry == Nothing
    && case map oracleProjectionEventView (appliedEntryProjectionEvents entry) of
      [LabelDecidedView {}] -> True
      _ -> False
  where
    entry = canonicalAppliedOracleEntryValue canonical

discardableLabelDecisionEntry :: State -> CanonicalAppliedOracleEntry -> Bool
discardableLabelDecisionEntry state canonical =
  appliedEntryControlIndex entry < state.labelEvidenceCompactedThrough
    && isPlainLabelDecisionEntry canonical
    && case appliedEntryCommand entry of
      Just command -> Map.notMember (OracleRequestRef (appliedEntryRequestId command)) state.oracleRequests
      Nothing -> False
  where
    entry = canonicalAppliedOracleEntryValue canonical

settledLabelRequestDecisionIndex :: OracleRequestWitness -> State -> Maybe ControlIndex
settledLabelRequestDecisionIndex witness state = case oracleRequestWitnessStatus witness of
  OracleRequestProjected index -> case oracleRequestWitnessIntent witness of
    CompleteLabelDecisionIntent attestation -> Just (labelCompletionControlIndex attestation)
    DecideLabelIntent decision _ _ _ _ _ _ _ _ -> do
      canonical <- Map.lookup index state.retainedEntries
      case [ liveDecisionRequestControlIndex opened
           | event <- appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical),
             LabelDecidedView opened _ _ <- [oracleProjectionEventView event],
             liveDecisionId opened == decision
           ] of
        [original] -> Just original
        _ -> Nothing
    _ -> Nothing
  _ -> Nothing

indexSettledLabelRequest :: OracleRequestRef -> State -> State
indexSettledLabelRequest reference state =
  case Map.lookup reference state.oracleRequests >>= (`settledLabelRequestDecisionIndex` state) of
    Nothing -> state
    Just index ->
      state
        { settledLabelRequests =
            Map.insertWith Set.union index (Set.singleton reference) state.settledLabelRequests
        }

-- | Expected retained subset of a checked Projection archive. Entries beyond
-- the completed-batch boundary are still present in full. Covered history has
-- explicit sparse exceptions, independently retained until a later reclamation
-- or consumer release. Composed validation checks their exact bytes as well.
oracleClientRetainsEntry :: CanonicalAppliedOracleEntry -> State -> Bool
oracleClientRetainsEntry canonical state =
  index > state.initialCursor
    && index <= state.appliedCursor
    && if index <= state.canonicalCoveredThrough
      then Map.lookup index state.retainedEntries == Just canonical
      else index > state.evidenceCompactedThrough || not (discardableClientEntry state canonical)
  where
    index = appliedEntryControlIndex (canonicalAppliedOracleEntryValue canonical)

-- | The caller must first establish exact byte equality against the matching
-- checked Projection owner. This only checks that the client observed that
-- coordinate, using sparse evidence or the narrow completed-batch discard rule.
-- It is not an independent admission path for a supplied old entry.
oracleClientMatchesProjectedEntry :: CanonicalAppliedOracleEntry -> State -> Bool
oracleClientMatchesProjectedEntry canonical state =
  case Map.lookup index state.retainedEntries of
    Just retained -> retained == canonical
    Nothing ->
      index > state.initialCursor
        && index <= state.evidenceCompactedThrough
        && (index <= state.canonicalCoveredThrough || discardableClientEntry state canonical)
  where
    index = appliedEntryControlIndex (canonicalAppliedOracleEntryValue canonical)

discardableClientEntry :: State -> CanonicalAppliedOracleEntry -> Bool
discardableClientEntry state canonical
  | discardableLabelDecisionEntry state canonical = True
  | appliedEntryConfiguration entry /= Nothing || not (null (appliedEntryProjectionEvents entry)) = False
  | otherwise = case appliedEntryCommand entry of
      Nothing -> isJust (appliedEntryOracleProgress entry)
      -- Every prepared request belongs to this client home, so a foreign
      -- request cannot own a local witness. No history lookup is required.
      Just command ->
        oracleClientRequestHome (appliedEntryRequestId command) /= oracleClientHome state
          && case oracleReceiptResult (appliedEntryReceipt command) of
            OracleRejected _ -> True
            OracleAccepted -> False
  where
    entry = canonicalAppliedOracleEntryValue canonical

oracleClientMembershipAdvance ::
  ControlIndex ->
  State ->
  Maybe HeraldMembershipGenerationId
oracleClientMembershipAdvance index = Map.lookup index . (.membershipAdvances)

-- | Level-triggered intent for restart/replay. Ordinary unrelated Herald
-- transitions do not re-emit it; client transitions and initialization do.
oracleClientActions :: State -> [OracleClientAction]
oracleClientActions state = case state.phase of
  Joining -> []
  Connecting attempt -> [ConnectAndHelloOracle attempt state.claims]
  Retrying retry -> [ScheduleOracleRetry OracleConnectionRetry retry]
  Bound binding ->
    WatchOracle binding state.appliedCursor
      : oracleClientRequestActions state
  Retired -> []

-- | Level-triggered command submissions for the current established Oracle
-- binding. Reconnect installs a new binding and reoffers every still-pending
-- canonical envelope under that binding.
oracleClientRequestActions :: State -> [OracleClientAction]
oracleClientRequestActions state = case state.phase of
  Bound binding ->
    oracleClientProgressActions state
      <> [ SubmitOracleRequest binding dispatch
         | dispatch <- oracleClientRequestDispatches state,
           not state.receiptHomeClosed,
           state.pendingRequestRetry == Nothing || cancellation dispatch
         ]
  _ -> []
  where
    cancellation dispatch = case lookupOracleRequest (oracleRequestDispatchRef dispatch) state of
      Just witness -> case oracleRequestWitnessIntent witness of
        VoterAdministrationIntent (CancelOracleVoterChange _) -> True
        FailureVoterCancellationIntent {} -> True
        _ -> False
      Nothing -> False

-- | Progress has its own idle fallback, including when no command is pending.
oracleClientProgressActions :: State -> [OracleClientAction]
oracleClientProgressActions state = case state.phase of
  Bound binding
    | not state.receiptHomeClosed,
      state.pendingRequestRetry == Nothing,
      oracleClientProgressReady state /= oracleClientProgressConfirmed state ->
        [ScheduleOracleProgress binding]
  _ -> []

oracleClientProgressReady :: State -> OracleProgress
oracleClientProgressReady state = oracleProgress state.receiptRetirementReady state.labelRetirementReady

oracleClientProgressConfirmed :: State -> OracleProgress
oracleClientProgressConfirmed state = oracleProgress state.receiptRetirementConfirmed state.labelRetirementConfirmed

-- | The composed owner registers the decision before applying its consumers.
-- Only an identical still-pinned registration is an exact retry.
observeLabelDecision :: LabelDecisionId -> ControlIndex -> State -> Either OracleClientProblem State
observeLabelDecision decision index state = do
  validateOperationalState state
  case Map.lookup decision state.labelConsumers of
    Just retained | retained == index -> Right state
    Just _ -> Left OracleClientStateContradiction
    Nothing
      | index <= state.latestLabelDecision || index > state.appliedCursor -> Left OracleClientStateContradiction
      | otherwise ->
          Right
            ( advanceLabelRetirement
                state
                  { latestLabelDecision = index,
                    labelConsumers = Map.insert decision index state.labelConsumers,
                    labelPins = Map.insert index (LabelPin 1 state.latestLabelDecision) state.labelPins
                  }
            )

-- | Checked semantic completion may be rediscovered by a different request.
-- Its absent local consumer is already consumed; request pins are independent.
consumeLabelDecision :: LabelDecisionId -> State -> Either OracleClientProblem State
consumeLabelDecision decision state = do
  validateOperationalState state
  case Map.lookup decision state.labelConsumers of
    Nothing -> Right state
    Just index ->
      Right
        ( advanceLabelRetirement
            state
              { labelConsumers = Map.delete decision state.labelConsumers,
                labelPins = removeLabelPin index state.labelPins
              }
        )

advanceLabelRetirement :: State -> State
advanceLabelRetirement state = state {labelRetirementReady = ready}
  where
    ready = case Map.lookupMin state.labelPins of
      Nothing -> state.latestLabelDecision
      Just (_, LabelPin _ previousLabel) -> min state.latestLabelDecision previousLabel

removeLabelPin :: ControlIndex -> Map ControlIndex LabelPin -> Map ControlIndex LabelPin
removeLabelPin index = Map.update release index
  where
    release (LabelPin count previousLabel) = if count == 1 then Nothing else Just (LabelPin (count - 1) previousLabel)

pinCompletionRequest :: OracleRequestRef -> OracleSemanticIntent -> State -> Either OracleClientProblem State
pinCompletionRequest reference intent state = case intent of
  CompleteLabelDecisionIntent attestation
    | let index = labelCompletionControlIndex attestation,
      index > state.labelRetirementReady,
      index <= state.latestLabelDecision,
      Just (LabelPin count previousLabel) <- Map.lookup index state.labelPins ->
        Right
          state
            { labelCompletionRequests = Map.insert reference index state.labelCompletionRequests,
              labelPins = Map.insert index (LabelPin (count + 1) previousLabel) state.labelPins
            }
    | otherwise -> Left OracleClientStateContradiction
  _ -> Right state

releaseCompletionRequest :: OracleRequestRef -> State -> State
releaseCompletionRequest reference state = case Map.lookup reference state.labelCompletionRequests of
  Nothing -> state
  Just index ->
    advanceLabelRetirement
      state
        { labelCompletionRequests = Map.delete reference state.labelCompletionRequests,
          labelPins = removeLabelPin index state.labelPins
        }

oracleClientReceiptRetirementReady :: State -> Word64
oracleClientReceiptRetirementReady = contiguousReceiptPrefix . (.receiptRetirementReady)

oracleClientReceiptRetirementProgressReady :: State -> ReceiptRetirement
oracleClientReceiptRetirementProgressReady = (.receiptRetirementReady)

oracleClientReceiptRetirementConfirmed :: State -> Word64
oracleClientReceiptRetirementConfirmed = contiguousReceiptPrefix . (.receiptRetirementConfirmed)

oracleClientReceiptRetirementProgressConfirmed :: State -> ReceiptRetirement
oracleClientReceiptRetirementProgressConfirmed = (.receiptRetirementConfirmed)

-- The issued high-water mark advances independently of unresolved requests.
-- The set contains only pending work, so completed history is never rescanned.
advanceReceiptRetirement :: State -> State
advanceReceiptRetirement state = state {receiptRetirementReady = ready}
  where
    ready =
      either
        (error . show)
        id
        (Lifetime.receiptRetirement (Just (state.nextRequestSequence - 1)) state.receiptOutstanding)

contiguousReceiptPrefix :: ReceiptRetirement -> Word64
contiguousReceiptPrefix progress = case Set.lookupMin (Lifetime.receiptRetirementExceptions progress) of
  Just first -> if first == 0 then 0 else first - 1
  Nothing -> retirementHighWater progress

retirementHighWater :: ReceiptRetirement -> Word64
retirementHighWater = maybe 0 id . Lifetime.receiptRetirementHighWater

-- Retirement metadata is transport progress, not part of semantic identity.
-- Keep the retained witness immutable while refreshing progress on dispatch.
dispatchEnvelope :: State -> OracleRequestWitness -> CanonicalOracleEnvelope
dispatchEnvelope state witness
  | oracleClientProgressReady state /= oracleProgress (Lifetime.receiptRetirementPrefix (Just 0)) (controlIndex 0) =
      canonicalizeOracleEnvelope
        (oracleEnvelopeWithProgress (oracleClientProgressReady state) (canonicalOracleEnvelopeValue (oracleRequestWitnessEnvelope witness)))
  | otherwise = oracleRequestWitnessEnvelope witness

progressAction :: OracleBinding -> State -> OracleClientAction
progressAction binding state =
  SubmitOracleProgress
    binding
    ( canonicalizeOracleEnvelope
        ( oracleEnvelope
            (oracleClientRequestId (oracleClientHome state) (retirementHighWater state.receiptRetirementReady))
            Nothing
            (oracleClientHome state)
            (retireOracleProgressCommand (oracleClientProgressReady state))
        )
    )

-- | Level-triggered submissions for retained label-workflow intentions.
-- Reconnect, explicit retry, and recovery callers may reoffer these exact
-- canonical envelopes; an entry disappears from this view as soon as its
-- local projection is settled.
oracleClientLabelRequestActions :: State -> [OracleClientAction]
oracleClientLabelRequestActions state = case state.phase of
  Bound binding
    | Nothing <- state.pendingRequestRetry,
      not state.receiptHomeClosed ->
        [ SubmitOracleRequest binding (OracleRequestDispatch reference envelope)
        | (reference, witness) <- Map.toAscList state.oracleRequests,
          oracleRequestWitnessStatus witness == OracleRequestAwaitingProjection,
          isLabelWorkflowIntent (oracleRequestWitnessIntent witness),
          let envelope = dispatchEnvelope state witness
        ]
  _ -> []

isLabelWorkflowIntent :: OracleSemanticIntent -> Bool
isLabelWorkflowIntent intent = case intent of
  HeraldAdmissionIntent {} -> False
  VoterAdministrationIntent {} -> False
  FailureVoterCancellationIntent {} -> False
  DecideLabelIntent {} -> True
  CompleteLabelDecisionIntent {} -> True
  DynamicStartIntent {} -> False
  ProcessEndIntent {} -> False
  OpenHeraldFailureProbeIntent {} -> False
  ReportHeraldFailureProbeIntent {} -> False
  DismissHeraldFailureProbeIntent {} -> False
  AcceptVoterHostFailureIntent {} -> False
  RetireHeraldEpochIntent {} -> False
  OpenDisappearanceProbeIntent {} -> False
  ReportPredefinedAbsenceIntent {} -> False
  InvalidateDisappearanceProbeIntent {} -> False
  ResolveDisappearanceProbeIntent {} -> False
  AbortDisappearanceProbeIntent {} -> False

oracleClientNextRequestSequence :: State -> Word64
oracleClientNextRequestSequence state = state.nextRequestSequence

oracleClientRequestEntries ::
  State ->
  [(OracleRequestRef, OracleRequestWitness)]
oracleClientRequestEntries = Map.toAscList . (.oracleRequests)

lookupOracleRequest ::
  OracleRequestRef ->
  State ->
  Maybe OracleRequestWitness
lookupOracleRequest reference = Map.lookup reference . (.oracleRequests)

-- | Resolve a canonical entry's exact client request without enumerating the
-- retained request inventory. The opaque reference has this same identity.
lookupOracleRequestById :: OracleClientRequestId -> State -> Maybe OracleRequestWitness
lookupOracleRequestById request = lookupOracleRequest (OracleRequestRef request)

lookupOracleRequestBySemanticKey :: ByteString -> State -> Maybe OracleRequestWitness
lookupOracleRequestBySemanticKey key state = do
  reference <- Map.lookup key state.oracleRequestKeys
  lookupOracleRequest reference state

-- | Pending command dispatch is level-triggered and retry-stable.  The public
-- action vocabulary can expose only these exact owner-retained envelopes under
-- the current Oracle binding.
oracleClientRequestDispatches :: State -> [OracleRequestDispatch]
oracleClientRequestDispatches state = case state.phase of
  Joining -> []
  Retired -> []
  _ | state.receiptHomeClosed -> []
  _ ->
    [ OracleRequestDispatch reference envelope
    | (reference, witness) <- Map.toAscList state.oracleRequests,
      oracleRequestWitnessStatus witness == OracleRequestAwaitingProjection,
      Map.notMember reference state.suppressedDisappearanceRequests,
      let envelope = dispatchEnvelope state witness
    ]

data OracleRequestClassification
  = OracleRequestFirstPrepared
  | OracleRequestExactRetry
  deriving stock (Eq, Ord, Show)

data PreparedOracleRequest = PreparedOracleRequest
  { successor :: State,
    classification :: OracleRequestClassification,
    reference :: OracleRequestRef
  }

-- | Closed admission intentions are keyed by their exact canonical claim;
-- connection retries retain the same request and cannot alter the seal attempt.
prepareHeraldAdmissionRequest :: Admission.HeraldAdmissionCommand -> State -> Either OracleClientProblem PreparedOracleRequest
prepareHeraldAdmissionRequest command =
  prepareSemanticOracleRequest
    (Admission.encodeAdmissionCommand command)
    (const (HeraldAdmissionIntent command))
    (const (heraldAdmissionCommand command))

-- | The coordinator supplies a stable semantic correlation; the owner retains
-- the exact closed operation, request sequence, and canonical envelope.
prepareVoterAdministrationRequest :: ByteString -> OracleVoterOperation -> State -> Either OracleClientProblem PreparedOracleRequest
prepareVoterAdministrationRequest key operation =
  prepareSemanticOracleRequest key (const (VoterAdministrationIntent operation)) (const (commandForVoterOperation operation))

-- | Failure-driven cancellation shares the explicit coordinator operation but
-- owns a stable semantic key independent of any administration connection.
prepareFailureVoterCancellationRequest :: Voter.VoterChangeId -> State -> Either OracleClientProblem PreparedOracleRequest
prepareFailureVoterCancellationRequest identifier =
  prepareSemanticOracleRequest
    (failureSemanticKey 5 [Serialize.runPut (Serialize.putWord64be (controlIndexWord64 (Voter.voterChangeIdControlIndex identifier)))])
    (const (FailureVoterCancellationIntent identifier))
    (const (Voter.cancelVoterChangeCommand identifier))

oracleClientLocalReplica :: State -> Maybe (RaftNodeId, Voter.OracleReplicaContact)
oracleClientLocalReplica = (.localReplica)

prepareStartProcessEpochRequest ::
  ByteString ->
  ProcessStart ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareStartProcessEpochRequest semanticKey bootstrap predecessor =
  prepareSemanticOracleRequest
    semanticKey
    (const (DynamicStartIntent bootstrap))
    (const (startProcessEpochCommand bootstrap))
    predecessor

prepareEndProcessEpochRequest ::
  ByteString ->
  ProcessEpochId ->
  ProcessEndReason ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareEndProcessEpochRequest semanticKey process reason predecessor = do
  command <-
    either
      (Left . OracleEndProcessCommandProblem)
      Right
      (endProcessEpochCommand process reason)
  prepareSemanticOracleRequest
    semanticKey
    (const (ProcessEndIntent process reason))
    (const command)
    predecessor

prepareLabelCompletionRequest ::
  LabelCompletionAttestation ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareLabelCompletionRequest report =
  prepareSemanticOracleRequest
    (labelCompletionSemanticKey report)
    (const (CompleteLabelDecisionIntent report))
    (const (completeLabelDecisionCommand report))

labelCompletionSemanticKey :: LabelCompletionAttestation -> ByteString
labelCompletionSemanticKey report =
  labelDriverSemanticKey 5 (labelCompletionDecisionId report) (Just (labelCompletionCollector report))
    <> heraldMembershipGenerationIdBytes (labelCompletionMembershipGeneration report)

prepareOpenHeraldFailureProbeRequest ::
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  PeerRecoveryGeneration ->
  Voter.VoterConfigurationId ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareOpenHeraldFailureProbeRequest target generation recovery configuration =
  prepareSemanticOracleRequest
    ( failureSemanticKey
        0
        [ heraldEpochBytes target,
          heraldMembershipGenerationIdBytes generation,
          Serialize.runPut
            (Serialize.putWord64be (peerRecoveryGenerationWord64 recovery)),
          Voter.voterConfigurationIdBytes configuration
        ]
    )
    (const (OpenHeraldFailureProbeIntent target generation recovery configuration))
    (const (openHeraldFailureProbeCommand target generation configuration))

prepareReportHeraldFailureProbeRequest ::
  HeraldFailureProbeId ->
  Voter.VoterConfigurationId ->
  ProbeResult ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareReportHeraldFailureProbeRequest probe configuration result =
  prepareSemanticOracleRequest
    ( failureSemanticKey
        1
        [heraldFailureProbeIdCanonicalBytes probe, Voter.voterConfigurationIdBytes configuration]
    )
    (const (ReportHeraldFailureProbeIntent probe configuration result))
    (const (reportHeraldFailureProbeCommand probe configuration result))

prepareAcceptVoterHostFailureRequest ::
  FailureProbeResolutionId ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareAcceptVoterHostFailureRequest resolution =
  prepareSemanticOracleRequest
    (failureSemanticKey 4 [failureProbeResolutionIdCanonicalBytes resolution])
    (const (AcceptVoterHostFailureIntent resolution))
    (const (acceptVoterHostFailureCommand resolution))

prepareDismissHeraldFailureProbeRequest ::
  FailureProbeResolutionId ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareDismissHeraldFailureProbeRequest resolution =
  prepareSemanticOracleRequest
    ( failureSemanticKey
        2
        [failureProbeResolutionIdCanonicalBytes resolution]
    )
    (const (DismissHeraldFailureProbeIntent resolution))
    (const (dismissHeraldFailureProbeCommand resolution))

prepareRetireHeraldEpochRequest ::
  FailureProbeResolutionId ->
  HeraldEpoch ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareRetireHeraldEpochRequest resolution target =
  prepareSemanticOracleRequest
    ( failureSemanticKey
        3
        [failureProbeResolutionIdCanonicalBytes resolution]
    )
    (const (RetireHeraldEpochIntent resolution target))
    (const (retireHeraldEpochCommand resolution target))

-- | Open identity belongs to the checked subject generation.  Every later
-- intention belongs to its exact probe and command kind, so unrelated reports
-- can remain pending concurrently and retries cannot replace their claims.
prepareOpenDisappearanceProbeRequest ::
  DisappearanceSubject ->
  DisappearanceSubjectMembershipCoordinate ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareOpenDisappearanceProbeRequest subject coordinate =
  prepareSemanticOracleRequest
    ( disappearanceSemanticKey
        0
        [ disappearanceSubjectCanonicalBytes subject,
          heraldMembershipGenerationIdBytes (disappearanceCoordinateMembershipGenerationId coordinate),
          memberSetDigestBytes (disappearanceCoordinateMemberSetDigest coordinate)
        ]
    )
    (const (OpenDisappearanceProbeIntent subject coordinate))
    (const (openDisappearanceProbeCommand subject coordinate))

preparePredefinedAbsenceReportRequest ::
  DisappearanceEvidenceClaim -> State -> Either OracleClientProblem PreparedOracleRequest
preparePredefinedAbsenceReportRequest claim =
  prepareDisappearanceRequest
    1
    (disappearanceEvidenceClaimProbeId claim)
    (ReportPredefinedAbsenceIntent claim)
    (reportPredefinedAbsenceCommand (disappearanceEvidenceClaimProbeId claim) claim)

prepareInvalidateDisappearanceProbeRequest ::
  DisappearanceProbeId ->
  HeraldEpoch ->
  DisappearanceInvalidationReason ->
  DisappearanceEvidenceDigest ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareInvalidateDisappearanceProbeRequest probe reporter reason witness =
  prepareDisappearanceRequest
    2
    probe
    (InvalidateDisappearanceProbeIntent probe reporter reason witness)
    (invalidateDisappearanceProbeCommand probe reporter reason witness)

prepareResolveDisappearanceProbeRequest ::
  DisappearanceProbeId ->
  CompleteDisappearanceEvidenceDigest ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareResolveDisappearanceProbeRequest probe digest =
  prepareDisappearanceRequest
    3
    probe
    (ResolveDisappearanceProbeIntent probe digest)
    (resolveDisappearanceProbeCommand probe digest)

prepareAbortDisappearanceProbeRequest ::
  DisappearanceProbeId ->
  DisappearanceAbortReason ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareAbortDisappearanceProbeRequest probe reason =
  prepareDisappearanceRequest
    4
    probe
    (AbortDisappearanceProbeIntent probe reason)
    (abortDisappearanceProbeCommand probe reason)

prepareDisappearanceRequest ::
  Word8 ->
  DisappearanceProbeId ->
  OracleSemanticIntent ->
  OracleCommand ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareDisappearanceRequest tag probe intent command =
  prepareSemanticOracleRequest
    (disappearanceSemanticKey tag [disappearanceProbeIdBytes probe])
    (const intent)
    (const command)

disappearanceSemanticKey :: Word8 -> [ByteString] -> ByteString
disappearanceSemanticKey tag fields =
  Serialize.runPut $ do
    Serialize.putByteString "ECLIPS-ORACLE-DISAPPEARANCE-REQUEST"
    Serialize.putWord8 tag
    mapM_
      ( \bytes -> do
          Serialize.putWord64be (fromIntegral (ByteString.length bytes))
          Serialize.putByteString bytes
      )
      fields

oracleClientDisappearanceRequestActions :: State -> [OracleClientAction]
oracleClientDisappearanceRequestActions state = case state.phase of
  Bound binding
    | Nothing <- state.pendingRequestRetry ->
        [ SubmitOracleRequest binding dispatch
        | dispatch <- oracleClientRequestDispatches state,
          Just witness <- [lookupOracleRequest (oracleRequestDispatchRef dispatch) state],
          isDisappearanceIntent (oracleRequestWitnessIntent witness)
        ]
  _ -> []

isDisappearanceIntent :: OracleSemanticIntent -> Bool
isDisappearanceIntent = \case
  OpenDisappearanceProbeIntent {} -> True
  ReportPredefinedAbsenceIntent {} -> True
  InvalidateDisappearanceProbeIntent {} -> True
  ResolveDisappearanceProbeIntent {} -> True
  AbortDisappearanceProbeIntent {} -> True
  _ -> False

-- | Terminal settlement suppresses transport retries without erasing exact
-- envelopes or their eventual committed results.  A racing aliased Open has
-- the same immutable subject coordinate even before its own result arrives.
suppressDisappearanceProbeRequests ::
  DisappearanceProbeId ->
  DisappearanceSubject ->
  DisappearanceSubjectMembershipCoordinate ->
  ControlIndex ->
  State ->
  Either OracleClientProblem State
suppressDisappearanceProbeRequests probe subject coordinate index predecessor = do
  validateOperationalState predecessor
  let references =
        [ reference
        | (reference, witness) <- Map.toAscList predecessor.oracleRequests,
          belongs (oracleRequestWitnessIntent witness),
          maybe False (< index) (Map.lookup reference predecessor.requestPreparedAt)
        ]
      successor =
        predecessor
          { oracleRequestKeys = foldr releaseOpenKey predecessor.oracleRequestKeys references,
            suppressedDisappearanceRequests =
              foldr
                (\reference -> Map.insert reference (probe, index))
                predecessor.suppressedDisappearanceRequests
                references
          }
  if null references || suppressionEvidenceMatches
    then Right ()
    else Left OracleClientStateContradiction
  validateOperationalState successor
  pure successor
  where
    suppressionEvidenceMatches = case Map.lookup index predecessor.retainedEntries of
      Nothing -> False
      Just canonical -> any terminalForProbe (appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical))
    terminalForProbe event = case oracleProjectionEventView event of
      DisappearanceProbeInvalidatedView observed _ -> observed == probe
      DisappearanceProbeResolvedView observed _ -> observed == probe
      DisappearanceProbeAbortedView observed _ -> observed == probe
      _ -> False
    releaseOpenKey reference retained = case Map.lookup reference predecessor.oracleRequests of
      Just witness
        | OpenDisappearanceProbeIntent {} <- oracleRequestWitnessIntent witness,
          Map.lookup (oracleRequestWitnessSemanticKey witness) retained == Just reference ->
            Map.delete (oracleRequestWitnessSemanticKey witness) retained
      _ -> retained
    belongs = \case
      OpenDisappearanceProbeIntent retainedSubject retainedCoordinate ->
        retainedSubject == subject && retainedCoordinate == coordinate
      ReportPredefinedAbsenceIntent claim -> disappearanceEvidenceClaimProbeId claim == probe
      InvalidateDisappearanceProbeIntent retained _ _ _ -> retained == probe
      ResolveDisappearanceProbeIntent retained _ -> retained == probe
      AbortDisappearanceProbeIntent retained _ -> retained == probe
      _ -> False

failureSemanticKey :: Word8 -> [ByteString] -> ByteString
failureSemanticKey tag fields =
  Serialize.runPut $ do
    Serialize.putByteString "ECLIPS-ORACLE-FAILURE-REQUEST"
    Serialize.putWord8 tag
    mapM_
      ( \bytes -> do
          Serialize.putWord64be (fromIntegral (ByteString.length bytes))
          Serialize.putByteString bytes
      )
      fields

labelDriverSemanticKey :: Word8 -> LabelDecisionId -> Maybe HeraldEpoch -> ByteString
labelDriverSemanticKey tag decision reporter =
  Serialize.runPut $ do
    Serialize.putByteString "ECLIPS-ORACLE-LABEL-DRIVER-REQUEST"
    Serialize.putWord8 tag
    Serialize.putByteString (labelDecisionIdBytes decision)
    case reporter of
      Nothing -> Serialize.putWord8 0
      Just herald -> do
        Serialize.putWord8 1
        Serialize.putByteString (heraldEpochBytes herald)

data PreparedLabelDecisionRequest = PreparedLabelDecisionRequest
  { request :: PreparedOracleRequest,
    decision :: LabelDecisionId
  }

prepareLabelDecisionRequest ::
  ByteString ->
  ProcessEpochId ->
  GlobalObjectId ->
  Label ->
  Maybe InitialLabelEvidence ->
  Maybe AuthorityEpoch ->
  Maybe PriorAuthorityJustification ->
  LabelTarget ->
  HomeLabelAcceptanceCut ->
  State ->
  Either OracleClientProblem PreparedLabelDecisionRequest
prepareLabelDecisionRequest =
  prepareLabelDecisionRequestWithStatus OracleRequestAwaitingProjection

-- | Reserve the exact label decision identity and bytes while its source group drains.
-- Repeating the accepted call preserves the retained status, including a
-- submission or caller End that has already been recorded.
prepareDrainingLabelDecisionRequest ::
  ByteString ->
  ProcessEpochId ->
  GlobalObjectId ->
  Label ->
  Maybe InitialLabelEvidence ->
  Maybe AuthorityEpoch ->
  Maybe PriorAuthorityJustification ->
  LabelTarget ->
  HomeLabelAcceptanceCut ->
  State ->
  Either OracleClientProblem PreparedLabelDecisionRequest
prepareDrainingLabelDecisionRequest =
  prepareLabelDecisionRequestWithStatus OracleRequestAwaitingSourceDrain

prepareLabelDecisionRequestWithStatus ::
  OracleRequestStatus ->
  ByteString ->
  ProcessEpochId ->
  GlobalObjectId ->
  Label ->
  Maybe InitialLabelEvidence ->
  Maybe AuthorityEpoch ->
  Maybe PriorAuthorityJustification ->
  LabelTarget ->
  HomeLabelAcceptanceCut ->
  State ->
  Either OracleClientProblem PreparedLabelDecisionRequest
prepareLabelDecisionRequestWithStatus
  initialStatus
  semanticKey
  caller
  object
  expected
  initialEvidence
  authority
  justification
  target
  acceptanceCut
  predecessor = do
    prepared <-
      prepareSemanticOracleRequestWithStatus
        initialStatus
        semanticKey
        intentFor
        commandFor
        predecessor
    let decision = decisionFor (oracleRequestRefRequestId prepared.reference)
    Right PreparedLabelDecisionRequest {request = prepared, decision}
    where
      decisionFor = deriveLabelDecisionId (oracleClientSystem predecessor)
      intentFor requestId =
        DecideLabelIntent
          (decisionFor requestId)
          caller
          object
          expected
          initialEvidence
          authority
          justification
          target
          acceptanceCut
      commandFor requestId =
        decideLabelCommand
          (decisionFor requestId)
          caller
          object
          expected
          initialEvidence
          authority
          justification
          target
          acceptanceCut

-- | Enable submission of the reserved label decision; its canonical envelope
-- and request identity remain the ones captured at application acceptance.
prepareDrainedLabelSubmission ::
  OracleRequestRef ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareDrainedLabelSubmission reference predecessor = do
  validateOperationalState predecessor
  witness <- maybe (Left (OracleRequestMissing reference)) Right (lookupOracleRequest reference predecessor)
  case oracleRequestWitnessIntent witness of
    DecideLabelIntent {} -> Right ()
    _ -> Left OracleRequestResultMismatch
  case oracleRequestWitnessStatus witness of
    OracleRequestAwaitingSourceDrain -> do
      let successor =
            predecessor
              { oracleRequests =
                  Map.insert reference (markOracleRequestAwaiting witness) predecessor.oracleRequests
              }
      validateOperationalState successor
      Right PreparedOracleRequest {successor, classification = OracleRequestFirstPrepared, reference}
    OracleRequestAwaitingProjection -> exactRetry
    OracleRequestProjected {} -> exactRetry
    _ -> Left OracleRequestResultMismatch
  where
    exactRetry = Right PreparedOracleRequest {successor = predecessor, classification = OracleRequestExactRetry, reference}

-- | A canonical caller End can settle a provably unsent label decision locally.
-- It is not an Oracle receipt and cannot cancel submitted work.
prepareUnsentLabelEnd ::
  OracleRequestRef ->
  ProcessEpochId ->
  ControlIndex ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareUnsentLabelEnd reference process index predecessor = do
  validateOperationalState predecessor
  witness <- maybe (Left (OracleRequestMissing reference)) Right (lookupOracleRequest reference predecessor)
  case oracleRequestWitnessIntent witness of
    DecideLabelIntent _ caller _ _ _ _ _ _ _
      | caller == process -> Right ()
    _ -> Left OracleRequestResultMismatch
  if retainedProcessEndAt process index predecessor
    then Right ()
    else Left OracleRequestControlMismatch
  case oracleRequestWitnessStatus witness of
    OracleRequestAwaitingSourceDrain -> do
      let successor =
            advanceReceiptRetirement
              $ predecessor
                { oracleRequests =
                    Map.insert reference (replaceOracleRequestStatus (OracleRequestEndedBeforeSubmission index) witness) predecessor.oracleRequests,
                  receiptOutstanding =
                    Set.delete (oracleClientRequestSequence (oracleRequestRefRequestId reference)) predecessor.receiptOutstanding
                }
      validateOperationalState successor
      Right PreparedOracleRequest {successor, classification = OracleRequestFirstPrepared, reference}
    OracleRequestEndedBeforeSubmission retainedIndex
      | retainedIndex == index ->
          Right PreparedOracleRequest {successor = predecessor, classification = OracleRequestExactRetry, reference}
    _ -> Left OracleRequestResultMismatch

retainedProcessEndAt :: ProcessEpochId -> ControlIndex -> State -> Bool
retainedProcessEndAt process index state =
  case Map.lookup index state.retainedEntries of
    Nothing -> False
    Just retained ->
      any
        ( \event -> case oracleProjectionEventView event of
            ProcessEpochEndedEventView ended endIndex _ -> ended == process && endIndex == index
            _ -> False
        )
        (appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue retained))

preparedLabelDecisionRequestClassification ::
  PreparedLabelDecisionRequest -> OracleRequestClassification
preparedLabelDecisionRequestClassification prepared =
  prepared.request.classification

preparedLabelDecisionId :: PreparedLabelDecisionRequest -> LabelDecisionId
preparedLabelDecisionId prepared = prepared.decision

preparedLabelDecisionRequestRef ::
  PreparedLabelDecisionRequest -> OracleRequestRef
preparedLabelDecisionRequestRef prepared = prepared.request.reference

commitLabelDecisionRequest ::
  PreparedLabelDecisionRequest ->
  ( State,
    OracleRequestClassification,
    LabelDecisionId,
    OracleRequestRef
  )
commitLabelDecisionRequest prepared =
  ( prepared.request.successor,
    prepared.request.classification,
    prepared.decision,
    prepared.request.reference
  )

prepareSemanticOracleRequest ::
  ByteString ->
  (OracleClientRequestId -> OracleSemanticIntent) ->
  (OracleClientRequestId -> OracleCommand) ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareSemanticOracleRequest = prepareSemanticOracleRequestWithStatus OracleRequestAwaitingProjection

prepareSemanticOracleRequestWithStatus ::
  OracleRequestStatus ->
  ByteString ->
  (OracleClientRequestId -> OracleSemanticIntent) ->
  (OracleClientRequestId -> OracleCommand) ->
  State ->
  Either OracleClientProblem PreparedOracleRequest
prepareSemanticOracleRequestWithStatus initialStatus semanticKey intentFor commandFor predecessor = do
  validateOperationalState predecessor
  case Map.lookup semanticKey predecessor.oracleRequestKeys of
    Just reference -> do
      witness <-
        maybe
          (Left OracleClientStateContradiction)
          Right
          (Map.lookup reference predecessor.oracleRequests)
      let requestId = oracleRequestRefRequestId reference
      if oracleRequestWitnessIntent witness == intentFor requestId
        then
          Right
            PreparedOracleRequest
              { successor = predecessor,
                classification = OracleRequestExactRetry,
                reference
              }
        else Left (OracleRequestConflict semanticKey)
    Nothing -> do
      let requestId =
            oracleClientRequestId
              (oracleClientHome predecessor)
              predecessor.nextRequestSequence
          reference = OracleRequestRef requestId
          intent = intentFor requestId
          command = commandFor requestId
          envelope =
            canonicalizeOracleEnvelope
              ( oracleEnvelope
                  requestId
                  Nothing
                  (oracleClientHome predecessor)
                  command
              )
          witness =
            OracleRequestWitness
              reference
              semanticKey
              intent
              envelope
              initialStatus
          registered =
            predecessor
              { nextRequestSequence = predecessor.nextRequestSequence + 1,
                receiptOutstanding = Set.insert predecessor.nextRequestSequence predecessor.receiptOutstanding,
                oracleRequests =
                  Map.insert reference witness predecessor.oracleRequests,
                requestPreparedAt = Map.insert reference predecessor.appliedCursor predecessor.requestPreparedAt,
                oracleRequestKeys =
                  Map.insert semanticKey reference predecessor.oracleRequestKeys
              }
      successor <- pinCompletionRequest reference intent registered
      validateOperationalState successor
      Right
        PreparedOracleRequest
          { successor,
            classification = OracleRequestFirstPrepared,
            reference
          }

preparedOracleRequestClassification ::
  PreparedOracleRequest ->
  OracleRequestClassification
preparedOracleRequestClassification prepared = prepared.classification

preparedOracleRequestRef ::
  PreparedOracleRequest ->
  OracleRequestRef
preparedOracleRequestRef prepared = prepared.reference

commitOracleRequest ::
  PreparedOracleRequest ->
  (State, OracleRequestClassification, OracleRequestRef)
commitOracleRequest prepared =
  (prepared.successor, prepared.classification, prepared.reference)

data OracleRequestResult
  = OracleRequestHeraldAdmissionChanged Admission.HeraldAdmissionRecord
  | OracleRequestVoterAdministration Voter.VoterAdministrationResult
  | OracleRequestRejected OracleRejection
  | OracleRequestStarted ProcessStart ControlIndex
  | OracleRequestEnded ProcessEpochId ControlIndex ProcessEndReason
  | OracleRequestLabelDecided LabelDecisionId ControlIndex
  | OracleRequestLabelCompleted LabelDecisionId ControlIndex
  | OracleRequestFailureProbeOpened HeraldFailureProbeId
  | OracleRequestFailureProbeReportRecorded
      HeraldFailureProbeId
      (Maybe FailureResolutionReady)
  | OracleRequestFailureProbeDismissed FailureProbeResolutionId
  | OracleRequestVoterHostFailureAccepted Failure.AcceptedVoterHostFailure
  | OracleRequestHeraldRetired
      FailureProbeResolutionId
      HeraldMembershipGenerationId
  | OracleRequestDisappearanceOpened DisappearanceOpenResult
  | OracleRequestPredefinedAbsenceReported DisappearanceProbeId HeraldEpoch
  | OracleRequestDisappearanceInvalidated DisappearanceProbeId
  | OracleRequestDisappearanceResolved DisappearanceProbeId DisappearanceResolutionOutcome
  | OracleRequestDisappearanceAborted DisappearanceProbeId
  deriving stock (Eq, Show)

data OracleResultClassification
  = OracleResultFirstRecorded
  | OracleResultExactRetry
  deriving stock (Eq, Ord, Show)

data PreparedOracleResult = PreparedOracleResult
  { successor :: State,
    classification :: OracleResultClassification,
    result :: OracleRequestResult
  }

prepareOracleResult ::
  OracleRequestRef ->
  CanonicalAppliedOracleEntry ->
  State ->
  Either OracleClientProblem PreparedOracleResult
prepareOracleResult reference canonical predecessor = do
  validateOperationalState predecessor
  witness <-
    maybe
      (Left (OracleRequestMissing reference))
      Right
      (Map.lookup reference predecessor.oracleRequests)
  let entry = canonicalAppliedOracleEntryValue canonical
      index = appliedEntryControlIndex entry
  case Map.lookup index predecessor.retainedEntries of
    Nothing -> Left (OracleRequestEntryMissing index)
    Just retained
      | retained /= canonical -> Left (OracleRequestEntryConflict index)
      | otherwise -> Right ()
  result <- interpretOracleResult predecessor witness entry
  (successor, classification) <-
    case oracleRequestWitnessStatus witness of
      OracleRequestAwaitingSourceDrain ->
        Left OracleRequestResultMismatch
      OracleRequestEndedBeforeSubmission {} ->
        Left OracleRequestResultMismatch
      OracleRequestAwaitingProjection -> do
        let projected = markOracleRequestProjected index witness
        -- Advancing past a rejected Open releases its key only after its
        -- result is projected. A delayed result must not leave that key
        -- pointing at a now-obsolete witness.
        if rejectedDisappearanceOpenBeforeCursor predecessor projected
          && Map.lookup (oracleRequestWitnessSemanticKey projected) predecessor.oracleRequestKeys == Just reference
          then Left OracleClientStateContradiction
          else
            Right
              ( advanceReceiptRetirement . releaseCompletionRequest reference
                  $ predecessor
                    { receiptOutstanding = Set.delete (oracleClientRequestSequence (oracleRequestRefRequestId reference)) predecessor.receiptOutstanding,
                      oracleRequests =
                        Map.insert reference projected predecessor.oracleRequests
                    },
                OracleResultFirstRecorded
              )
      OracleRequestDeferred {} ->
        Left OracleRequestResultMismatch
      OracleRequestProjected retainedIndex
        | retainedIndex == index ->
            Right (predecessor, OracleResultExactRetry)
        | otherwise -> Left (OracleRequestEntryConflict index)
  let !indexed = indexSettledLabelRequest reference successor
  validateOperationalState indexed
  Right PreparedOracleResult {successor = indexed, classification, result}

preparedOracleResultClassification ::
  PreparedOracleResult ->
  OracleResultClassification
preparedOracleResultClassification prepared = prepared.classification

preparedOracleResult ::
  PreparedOracleResult ->
  OracleRequestResult
preparedOracleResult prepared = prepared.result

commitOracleResult ::
  PreparedOracleResult ->
  (State, OracleResultClassification, OracleRequestResult)
commitOracleResult prepared =
  (prepared.successor, prepared.classification, prepared.result)

interpretOracleResult ::
  State ->
  OracleRequestWitness ->
  AppliedOracleEntry ->
  Either OracleClientProblem OracleRequestResult
interpretOracleResult state witness entry = do
  command <- maybe (Left OracleRequestResultMismatch) Right (appliedEntryCommand entry)
  let reference = oracleRequestWitnessRef witness
      envelope = oracleRequestWitnessEnvelope witness
      expectedRequest = oracleRequestRefRequestId reference
      expectedDigest = canonicalOracleEnvelopeDigest envelope
  if appliedEntryRequestId command /= expectedRequest
    then Left OracleRequestIdMismatch
    else Right ()
  if appliedEntryCommandDigest command /= expectedDigest
    then Left OracleRequestDigestMismatch
    else Right ()
  let receipt = appliedEntryReceipt command
      intent = oracleRequestWitnessIntent witness
  let commandEvents = filter (\event -> case oracleProjectionEventView event of HeraldAdmissionChangedView {} -> False; _ -> True) (appliedEntryProjectionEvents entry)
  case oracleReceiptResult receipt of
    OracleRejected rejection
      | oracleReceiptFailureResult receipt == Nothing,
        oracleReceiptDisappearanceResult receipt == Nothing,
        null (appliedEntryProjectionEvents entry) ->
          Right (OracleRequestRejected rejection)
      | otherwise -> Left OracleRequestResultMismatch
    OracleAccepted
      | VoterAdministrationIntent {} <- intent,
        Just result <- oracleReceiptVoterResult receipt ->
          Right (OracleRequestVoterAdministration result)
    OracleAccepted
      | FailureVoterCancellationIntent {} <- intent,
        Just result <- oracleReceiptVoterResult receipt ->
          Right (OracleRequestVoterAdministration result)
    OracleAccepted
      | HeraldAdmissionIntent admissionCommand <- intent ->
          interpretAdmissionResult admissionCommand (appliedEntryControlIndex entry) (appliedEntryProjectionEvents entry)
    OracleAccepted
      | Just result <- oracleReceiptDisappearanceResult receipt -> do
          events <- case commandEvents of
            [] -> retainedDisappearanceTerminalEvents expectedRequest state entry
            current -> Right current
          interpretDisappearanceResult intent result events
    OracleAccepted ->
      case oracleReceiptFailureResult receipt of
        Just failure -> interpretFailureResult intent failure
        Nothing ->
          case commandEvents of
            [event] ->
              interpretAcceptedEvent
                intent
                expectedRequest
                (appliedEntryControlIndex entry)
                event
            opened : invalidations
              | DecideLabelIntent {} <- intent,
                all isLabelDisappearanceInvalidation invalidations ->
                  interpretAcceptedEvent intent expectedRequest (appliedEntryControlIndex entry) opened
            _ -> Left OracleRequestResultMismatch

-- A different member can commit the same terminal intention after the
-- canonical terminal already suppressed our transport retry. Oracle accepts
-- that alias without emitting a second event. Its receipt must still agree
-- with the exact terminal which settled this retained request.
retainedDisappearanceTerminalEvents ::
  OracleClientRequestId -> State -> AppliedOracleEntry -> Either OracleClientProblem [OracleProjectionEvent]
retainedDisappearanceTerminalEvents request state entry = do
  (probe, index) <-
    maybe
      (Left OracleRequestResultMismatch)
      Right
      (Map.lookup (OracleRequestRef request) state.suppressedDisappearanceRequests)
  if index < appliedEntryControlIndex entry
    then Right ()
    else Left OracleRequestResultMismatch
  retained <- maybe (Left (OracleRequestEntryMissing index)) Right (Map.lookup index state.retainedEntries)
  Right
    [ event
    | event <- appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue retained),
      case oracleProjectionEventView event of
        DisappearanceProbeInvalidatedView observed _ -> observed == probe
        DisappearanceProbeResolvedView observed _ -> observed == probe
        DisappearanceProbeAbortedView observed _ -> observed == probe
        _ -> False
    ]

isLabelDisappearanceInvalidation :: OracleProjectionEvent -> Bool
isLabelDisappearanceInvalidation event = case oracleProjectionEventView event of
  DisappearanceProbeInvalidatedView _ (LabelOpenDisappearanceInvalidation _) -> True
  _ -> False

interpretDisappearanceResult ::
  OracleSemanticIntent ->
  DisappearanceCommandResult ->
  [OracleProjectionEvent] ->
  Either OracleClientProblem OracleRequestResult
interpretDisappearanceResult intent result events = case (intent, result, fmap oracleProjectionEventView events) of
  ( OpenDisappearanceProbeIntent subject coordinate,
    DisappearanceOpenAccepted accepted,
    [DisappearanceProbeOpenedView header projected]
    )
      | accepted == projected,
        projectedDisappearanceProbeHeaderId header == openProbe accepted,
        projectedDisappearanceProbeHeaderSubject header == subject,
        projectedDisappearanceProbeHeaderCoordinate header == coordinate ->
          Right (OracleRequestDisappearanceOpened accepted)
  ( ReportPredefinedAbsenceIntent expected,
    DisappearanceReportAccepted probe reporter,
    [PredefinedAbsenceReportedView claim]
    )
      | claim == expected,
        disappearanceEvidenceClaimProbeId claim == probe,
        disappearanceEvidenceClaimReporter claim == reporter ->
          Right (OracleRequestPredefinedAbsenceReported probe reporter)
  ( InvalidateDisappearanceProbeIntent expected reporter reason witness,
    DisappearanceInvalidateAccepted probe,
    [DisappearanceProbeInvalidatedView projected (CommandDisappearanceInvalidation actualReporter actualReason actualWitness)]
    )
      | expected == probe,
        projected == probe,
        reporter == actualReporter,
        reason == actualReason,
        witness == actualWitness ->
          Right (OracleRequestDisappearanceInvalidated probe)
  ( ResolveDisappearanceProbeIntent expected _,
    DisappearanceResolveAccepted outcome,
    [DisappearanceProbeResolvedView probe projected]
    )
      | expected == probe,
        outcome == projected ->
          Right (OracleRequestDisappearanceResolved probe outcome)
  ( AbortDisappearanceProbeIntent expected reason,
    DisappearanceAbortAccepted probe,
    [DisappearanceProbeAbortedView projected (ExplicitDisappearanceAbortReasonView actualReason)]
    )
      | expected == probe,
        projected == probe,
        reason == actualReason ->
          Right (OracleRequestDisappearanceAborted probe)
  _ -> Left OracleRequestResultMismatch
  where
    openProbe accepted = case disappearanceOpenResultView accepted of
      OpenedDisappearanceProbe probe -> probe
      AliasedDisappearanceProbe probe -> probe

interpretFailureResult ::
  OracleSemanticIntent ->
  FailureCommandResult ->
  Either OracleClientProblem OracleRequestResult
interpretFailureResult intent result = case (intent, result) of
  (OpenHeraldFailureProbeIntent {}, FailureProbeOpened probe) ->
    Right (OracleRequestFailureProbeOpened probe)
  ( ReportHeraldFailureProbeIntent expected _ _,
    FailureProbeReportRecorded probe intention
    )
      | probe == expected ->
          Right (OracleRequestFailureProbeReportRecorded probe intention)
  ( DismissHeraldFailureProbeIntent expected,
    FailureProbeDismissedResult resolution
    )
      | resolution == expected ->
          Right (OracleRequestFailureProbeDismissed resolution)
  ( AcceptVoterHostFailureIntent expected,
    VoterHostFailureAcceptedResult certificate
    )
      | Failure.acceptedVoterHostFailureResolution certificate == expected ->
          Right (OracleRequestVoterHostFailureAccepted certificate)
  ( RetireHeraldEpochIntent expected _,
    HeraldRetiredResult resolution generation
    )
      | resolution == expected ->
          Right (OracleRequestHeraldRetired resolution generation)
  _ -> Left OracleRequestResultMismatch

-- | A compact completion command has exactly one canonical completion event.
interpretLabelCompletionEventView ::
  LabelDecisionId ->
  LabelOutcomeDigest ->
  ControlIndex ->
  OracleProjectionEventView ->
  Either OracleClientProblem OracleRequestResult
interpretLabelCompletionEventView expectedDecision expectedDigest index event =
  case event of
    LabelWorkflowCompletedView decision digest
      | decision == expectedDecision && digest == expectedDigest ->
          Right (OracleRequestLabelCompleted decision index)
    _ -> Left OracleRequestResultMismatch

interpretAcceptedEvent ::
  OracleSemanticIntent ->
  OracleClientRequestId ->
  ControlIndex ->
  OracleProjectionEvent ->
  Either OracleClientProblem OracleRequestResult
interpretAcceptedEvent intent expectedRequest index event =
  case (intent, oracleProjectionEventView event) of
    (DynamicStartIntent bootstrap, ProcessStartedView record) -> do
      if processRecordProcessEpoch record
        /= processStartProcessEpochId bootstrap
        || processRecordProcessId record /= processStartProcessId bootstrap
        || processRecordResidence record /= processStartResidence bootstrap
        then Left OracleRequestBootstrapMismatch
        else Right ()
      case processRecordOrigin record of
        DynamicStartView startIndex request
          | startIndex == index
              && request == expectedRequest ->
              Right (OracleRequestStarted bootstrap index)
          | startIndex /= index -> Left OracleRequestControlMismatch
          | otherwise -> Left OracleRequestBootstrapMismatch
        _ -> Left OracleRequestBootstrapMismatch
    ( ProcessEndIntent process reason,
      ProcessEpochEndedEventView endedProcess endIndex endedReason
      ) -> do
        if endedProcess /= process || endedReason /= reason
          then Left OracleRequestResultMismatch
          else Right ()
        if endIndex /= index
          then Left OracleRequestControlMismatch
          else Right (OracleRequestEnded process index reason)
    ( DecideLabelIntent decision caller object expected initialEvidence _ _ target acceptanceCut,
      LabelDecidedView decided _ _
      )
        | liveDecisionId decided == decision,
          liveDecisionOpenRequestId decided == expectedRequest,
          liveDecisionCaller decided == caller,
          liveDecisionObject decided == object,
          liveDecisionExpectedLabel decided == expected,
          liveDecisionInitialLabelEvidence decided == initialEvidence,
          liveDecisionTarget decided == target,
          liveDecisionHomeAcceptanceCut decided == acceptanceCut,
          liveDecisionRequestControlIndex decided == index ->
            Right (OracleRequestLabelDecided decision index)
        | otherwise -> Left OracleRequestResultMismatch
    (CompleteLabelDecisionIntent report, completion@LabelWorkflowCompletedView {}) ->
      interpretLabelCompletionEventView (labelCompletionDecisionId report) (labelCompletionOutcomeDigest report) index completion
    _ -> Left OracleRequestResultMismatch

markOracleRequestProjected ::
  ControlIndex ->
  OracleRequestWitness ->
  OracleRequestWitness
markOracleRequestProjected index (OracleRequestWitness reference semanticKey intent envelope _) =
  OracleRequestWitness
    reference
    semanticKey
    intent
    envelope
    (OracleRequestProjected index)

newtype PreparedOracleSubmissionDeferral
  = PreparedOracleSubmissionDeferral State

-- | Retain a busy conditional decision without changing its exact envelope,
-- receipt, query, watch, or cursor state.
prepareOracleSubmissionDeferral ::
  OracleClientRequestId ->
  LabelDecisionId ->
  ControlIndex ->
  State ->
  Either OracleClientProblem PreparedOracleSubmissionDeferral
prepareOracleSubmissionDeferral requestId decision observedIndex predecessor = do
  validateOperationalState predecessor
  let reference = OracleRequestRef requestId
  witness <-
    maybe
      (Left (OracleRequestMissing reference))
      Right
      (Map.lookup reference predecessor.oracleRequests)
  case oracleRequestWitnessIntent witness of
    DecideLabelIntent {} -> Right ()
    _ -> Left (OracleSubmissionDeferralUnsupportedIntent requestId)
  successor <- case oracleRequestWitnessStatus witness of
    OracleRequestAwaitingProjection ->
      Right
        predecessor
          { oracleRequests =
              Map.insert
                reference
                (markOracleRequestDeferred decision observedIndex witness)
                predecessor.oracleRequests
          }
    OracleRequestDeferred retainedDecision retainedIndex
      | retainedDecision == decision && retainedIndex == observedIndex ->
          Right predecessor
      | otherwise -> Left (OracleSubmissionDeferralConflict requestId)
    OracleRequestProjected {} -> Left (OracleSubmissionDeferralConflict requestId)
    OracleRequestAwaitingSourceDrain -> Left (OracleSubmissionDeferralConflict requestId)
    OracleRequestEndedBeforeSubmission {} -> Left (OracleSubmissionDeferralConflict requestId)
  if observedIndex < predecessor.initialCursor
    then Left OracleClientStateContradiction
    else Right ()
  validateOperationalState successor
  Right (PreparedOracleSubmissionDeferral successor)

commitOracleSubmissionDeferral :: PreparedOracleSubmissionDeferral -> State
commitOracleSubmissionDeferral (PreparedOracleSubmissionDeferral successor) = successor

-- | Reactivate each exact deferred label decision only after completion of its
-- blocker is in the caller's contiguous Oracle projection. The canonical
-- envelope is unchanged.
releaseCompletedOracleDeferrals ::
  (LabelDecisionId -> Bool) ->
  State ->
  Either OracleClientProblem State
releaseCompletedOracleDeferrals isCompleted predecessor = do
  validateOperationalState predecessor
  let successor =
        predecessor
          { oracleRequests =
              Map.map release predecessor.oracleRequests
          }
  validateOperationalState successor
  Right successor
  where
    release witness = case oracleRequestWitnessStatus witness of
      OracleRequestDeferred decision _
        | isCompleted decision -> markOracleRequestAwaiting witness
      _ -> witness

markOracleRequestDeferred ::
  LabelDecisionId ->
  ControlIndex ->
  OracleRequestWitness ->
  OracleRequestWitness
markOracleRequestDeferred decision observedIndex witness =
  replaceOracleRequestStatus
    (OracleRequestDeferred decision observedIndex)
    witness

markOracleRequestAwaiting ::
  OracleRequestWitness -> OracleRequestWitness
markOracleRequestAwaiting =
  replaceOracleRequestStatus OracleRequestAwaitingProjection

replaceOracleRequestStatus ::
  OracleRequestStatus ->
  OracleRequestWitness ->
  OracleRequestWitness
replaceOracleRequestStatus status (OracleRequestWitness reference semanticKey intent envelope _) =
  OracleRequestWitness reference semanticKey intent envelope status

prepareClientIngress ::
  OracleClientIngress ->
  State ->
  Either OracleClientProblem PreparedClientIngress
prepareClientIngress ingress predecessor = do
  validateOperationalState predecessor
  case ingress of
    OracleLocalReplicaConfigured node contact ->
      case predecessor.localReplica of
        Nothing -> checkedPrepared (predecessor {localReplica = Just (node, contact)}) []
        Just retained | retained == (node, contact) -> checkedPrepared predecessor []
        Just _ -> Left OracleClientStateContradiction
    OracleContactsDiscovered hints ->
      checkedPrepared (observeOracleContactHints hints predecessor) []
    OracleConnectFailed attempt ->
      if currentAttemptIs attempt predecessor
        then preparedRetry (forgetFailedContact attempt.contact predecessor) []
        else stale predecessor
    OracleHelloReceived attempt acceptance ->
      prepareHello attempt acceptance predecessor
    OracleRedirectReceived lane redirect ->
      prepareRedirect lane redirect predecessor
    OracleBindingLost binding ->
      if oracleClientBindingIsCurrent binding predecessor
        then preparedRetry (forgetFailedContact binding.contact predecessor) []
        else stale predecessor
    OracleRetryElapsed retry ->
      case predecessor.phase of
        Retrying current
          | current == retry ->
              let (successor, connectActions) = beginConnect predecessor.leaderHint predecessor
               in checkedPrepared successor (CancelOracleRetry retry : connectActions)
        Bound _
          | predecessor.pendingRequestRetry == Just retry ->
              let successor = predecessor {pendingRequestRetry = Nothing}
               in checkedPrepared
                    successor
                    ( CancelOracleRetry retry
                        : oracleClientRequestActions successor
                    )
        _ -> stale predecessor
    OracleSubmissionNotReadyReceived binding request term ->
      prepareSubmissionNotReady binding request term predecessor
    OracleProgressConfirmed binding request through
      | not (oracleClientBindingIsCurrent binding predecessor) -> stale predecessor
      | oracleClientRequestHome request /= oracleClientHome predecessor
          || oracleClientRequestSequence request > retirementHighWater (oracleProgressReceipts through) ->
          Left OracleClientStateContradiction
      | otherwise -> do
          successor <- confirmProgress through predecessor
          checkedPrepared successor (oracleClientRequestActions successor)
    OracleProgressNotReadyReceived binding request offered term ->
      prepareRetirementNotReady binding request offered term predecessor
    OracleProgressFlush binding
      | not (oracleClientBindingIsCurrent binding predecessor) -> stale predecessor
      | not predecessor.receiptHomeClosed,
        predecessor.pendingRequestRetry == Nothing,
        oracleClientProgressReady predecessor /= oracleClientProgressConfirmed predecessor ->
          checkedPrepared predecessor [progressAction binding predecessor]
      | otherwise -> checkedPrepared predecessor []
    OracleRequestRetiredReceived binding request reason
      | not (oracleClientBindingIsCurrent binding predecessor) -> stale predecessor
      | oracleClientRequestHome request /= oracleClientHome predecessor -> Left OracleClientStateContradiction
      | OracleRequestPrefixRetired through <- reason,
        through < oracleClientRequestSequence request ->
          Left OracleClientStateContradiction
      | OracleRequestHomeRetired <- reason ->
          -- Stop submissions while the committed watch catches up to the
          -- existing semantic fencing transition. Keep that watch alive.
          checkedPrepared (predecessor {receiptHomeClosed = True}) []
      | Lifetime.receiptIsRetired (oracleClientRequestSequence request) predecessor.receiptRetirementReady ->
          checkedPrepared predecessor []
      | otherwise -> Left (OracleRequestRetiredBeforeSettlement request)
    OracleSubmissionDeferredReceived {} -> Left OracleDeferralRequiresProjection
    OracleEntriesReceived {} -> Left OracleEntriesRequireProjection

preparedClientIngressActions :: PreparedClientIngress -> [OracleClientAction]
preparedClientIngressActions (PreparedClientIngress _ actions) = actions

-- | Discovery and committed registrations supply routes only. Preserve the
-- selected physical lane while replacing other hints, and keep older contacts
-- as fallbacks. Conflicting historical endpoint hints cannot confer authority.
observeOracleContactHints :: OracleContactSet -> State -> State
observeOracleContactHints (OracleContactSet incoming) state =
  state {contacts = OracleContactSet (NonEmpty.fromList selected)}
  where
    OracleContactSet previous = state.contacts
    pinned = case state.phase of
      Connecting attempt -> [attempt.contact]
      Bound binding -> [binding.contact]
      _ -> []
    selected = reverse (foldl retain [] (pinned <> NonEmpty.toList incoming <> NonEmpty.toList previous))
    retain retained contact
      | any (\known -> known.node == contact.node || (known.host, known.port) == (contact.host, contact.port)) retained = retained
      | otherwise = contact : retained

-- | Only a committed configuration supplies the current voting preference.
-- Missing contacts remain unresolved; learning an address never adds a voter.
preferOracleVoters :: Set OracleNodeClaim -> State -> State
preferOracleVoters voters state =
  state
    { preferredVoters = voters,
      nextContactOffset = 0,
      leaderHint = case state.leaderHint of
        Just hint | Set.member hint voters -> Just hint
        _ -> Nothing
    }

commitClientIngress :: PreparedClientIngress -> State
commitClientIngress (PreparedClientIngress state _) = state

prepareCursorAdvance ::
  CanonicalAppliedOracleEntry ->
  State ->
  Either OracleClientProblem PreparedCursorAdvance
prepareCursorAdvance canonical predecessor = do
  validateOperationalState predecessor
  let entry = canonicalAppliedOracleEntryValue canonical
      actual = appliedEntryControlIndex entry
      expected = nextControlIndex predecessor.appliedCursor
  if actual /= expected
    then Left (OracleCursorNotNext expected actual)
    else do
      let successor =
            releaseRejectedDisappearanceOpenKeys
              $ predecessor
                { appliedCursor = actual,
                  lastSemanticCursor = case (appliedEntryCommand entry, appliedEntryOracleProgress entry) of
                    (Nothing, Just _) -> predecessor.lastSemanticCursor
                    _ -> actual,
                  requestedCursor = actual,
                  retainedEntries = Map.insert actual canonical predecessor.retainedEntries
                }
      indexed <- observeRequestEvidence canonical successor
      validateOperationalState indexed
      confirmed <- case appliedEntryOracleProgress entry of
        Just (home, through) | home == oracleClientHome indexed -> confirmProgress through indexed
        _ -> Right indexed
      Right (PreparedCursorAdvance confirmed)

observeRequestEvidence :: CanonicalAppliedOracleEntry -> State -> Either OracleClientProblem State
observeRequestEvidence canonical state = case appliedEntryCommand entry of
  Just command
    | let reference = OracleRequestRef (appliedEntryRequestId command),
      Map.member reference state.oracleRequests ->
        case Map.lookup reference state.requestObservedAt of
          Nothing -> Right state {requestObservedAt = Map.insert reference index state.requestObservedAt}
          Just retained
            | retained == index -> Right state
            | otherwise -> Left (OracleRequestEntryConflict index)
  _ -> Right state
  where
    entry = canonicalAppliedOracleEntryValue canonical
    index = appliedEntryControlIndex entry

commitCursorAdvance :: PreparedCursorAdvance -> State
commitCursorAdvance (PreparedCursorAdvance state) = state

-- | Advance the watch cursor across one package-private membership composition
-- seam without an ordinary canonical watch entry. This is the detached
-- reference schedule; the live EORC path uses
-- 'prepareCanonicalMembershipCursorAdvance'.
prepareMembershipCursorAdvance ::
  ControlIndex ->
  HeraldMembershipGenerationId ->
  State ->
  Either OracleClientProblem PreparedMembershipCursorAdvance
prepareMembershipCursorAdvance actual generation predecessor = do
  validateOperationalState predecessor
  case predecessor.phase of
    Bound _ -> Right ()
    _ -> Left OracleMembershipCursorNotBound
  let expected = nextControlIndex predecessor.appliedCursor
  if actual /= expected
    then Left (OracleCursorNotNext expected actual)
    else do
      let successor =
            releaseRejectedDisappearanceOpenKeys
              $ predecessor
                { appliedCursor = actual,
                  requestedCursor = actual,
                  claims = predecessor.claims {membershipGeneration = generation},
                  lastSemanticCursor = actual,
                  membershipAdvances =
                    Map.insert actual generation predecessor.membershipAdvances
                }
      validateOperationalState successor
      Right (PreparedMembershipCursorAdvance successor)

-- | Atomically retain one canonical membership entry, advance the watch
-- cursor, and replace the generation claim used by every later Hello. The
-- supplied generation must be the entry's unique projected membership
-- successor; an ordinary entry cannot masquerade as a membership cut.
prepareCanonicalMembershipCursorAdvance ::
  CanonicalAppliedOracleEntry ->
  HeraldMembershipGenerationId ->
  State ->
  Either OracleClientProblem PreparedMembershipCursorAdvance
prepareCanonicalMembershipCursorAdvance canonical generation predecessor = do
  validateOperationalState predecessor
  case predecessor.phase of
    Bound _ -> Right ()
    Joining -> Right ()
    _ -> Left OracleMembershipCursorNotBound
  let entry = canonicalAppliedOracleEntryValue canonical
      actual = appliedEntryControlIndex entry
      expected = nextControlIndex predecessor.appliedCursor
  if canonicalMembershipGeneration canonical /= Just generation
    then Left OracleMembershipCursorEntryMismatch
    else
      if actual /= expected
        then Left (OracleCursorNotNext expected actual)
        else do
          let successor =
                releaseRejectedDisappearanceOpenKeys
                  $ predecessor
                    { appliedCursor = actual,
                      requestedCursor = actual,
                      claims = predecessor.claims {membershipGeneration = generation},
                      lastSemanticCursor = actual,
                      retainedEntries = Map.insert actual canonical predecessor.retainedEntries,
                      membershipAdvances =
                        Map.insert actual generation predecessor.membershipAdvances
                    }
          indexed <- observeRequestEvidence canonical successor
          validateOperationalState indexed
          confirmed <- case appliedEntryOracleProgress entry of
            Just (home, through) | home == oracleClientHome indexed -> confirmProgress through indexed
            _ -> Right indexed
          Right (PreparedMembershipCursorAdvance confirmed)

commitMembershipCursorAdvance :: PreparedMembershipCursorAdvance -> State
commitMembershipCursorAdvance (PreparedMembershipCursorAdvance state) = state

-- | Interpret one owner-applied committed-prefix observation. A strict
-- advance makes the retained submission retry obsolete immediately: the
-- exact timer is cancelled before the caller emits any potentially large
-- semantic release batch, and the caller may then reoffer the still-pending
-- requests from its complete post-release state. Duplicate and regressed
-- observations leave both the token and the state unchanged.
prepareCommittedPrefixProgress ::
  ControlIndex ->
  State ->
  Either OracleClientProblem PreparedClientIngress
prepareCommittedPrefixProgress previous predecessor = do
  validateOperationalState predecessor
  if predecessor.appliedCursor > previous
    then
      let cancellation = maybe [] (pure . CancelOracleRetry) predecessor.pendingRequestRetry
          successor = predecessor {pendingRequestRetry = Nothing}
       in checkedPrepared successor cancellation
    else checkedPrepared predecessor []

-- | A gap changes no owner fact; re-emitting the level-triggered watch requests
-- the exact retained cursor again.
prepareRewatch :: State -> Either OracleClientProblem PreparedClientIngress
prepareRewatch state = do
  validateOperationalState state
  Right (PreparedClientIngress state (oracleClientActions state))

-- | Retire every watch/retry intention at the Herald admission cut.
prepareDrain :: State -> Either OracleClientProblem State
prepareDrain state = do
  validateOperationalState state
  let successor = state {phase = Retired, pendingRequestRetry = Nothing}
  validateOperationalState successor
  Right successor

prepareHello ::
  OracleConnectAttempt ->
  OracleHelloAcceptance ->
  State ->
  Either OracleClientProblem PreparedClientIngress
prepareHello attempt acceptance predecessor
  | not (currentAttemptIs attempt predecessor) = stale predecessor
  | acceptance.node /= contactNode attempt.contact = rejectCandidate attempt acceptance predecessor
  | maybe False (> acceptance.term) predecessor.observedTerm =
      rejectCandidate attempt acceptance predecessor
  | acceptance.localApplied < predecessor.appliedCursor =
      rejectCandidateWith
        attempt
        acceptance
        predecessor
        [ ReportOracleClientDiagnostic
            (OracleReplicaBehind predecessor.appliedCursor acceptance.localApplied)
        ]
  | not acceptance.serviceReady =
      followCandidateRedirect attempt acceptance predecessor
  | otherwise = do
      let generation = OracleBindingGeneration predecessor.nextBindingGeneration
          binding = OracleBinding generation attempt.contact
          (hint, diagnostics) = admittedHint acceptance.leaderHint predecessor.contacts
          successor =
            predecessor
              { phase = Bound binding,
                nextBindingGeneration = predecessor.nextBindingGeneration + 1,
                pendingRequestRetry = Nothing,
                observedTerm = Just acceptance.term,
                leaderHint = hint
              }
          actions =
            diagnostics
              <> (BindOracleConnection attempt binding : oracleClientActions successor)
      checkedPrepared successor actions

prepareRedirect ::
  OracleLane ->
  OracleRedirect ->
  State ->
  Either OracleClientProblem PreparedClientIngress
prepareRedirect lane redirect predecessor
  | not (laneIsCurrent lane predecessor) = stale predecessor
  | maybe False (> redirect.term) predecessor.observedTerm =
      followRedirect lane Nothing predecessor.observedTerm predecessor
  | otherwise =
      followRedirect
        lane
        (hintDistinctFromContact (laneContact lane) redirect.leaderHint)
        (Just redirect.term)
        predecessor

followRedirect ::
  OracleLane ->
  Maybe OracleNodeClaim ->
  Maybe OracleObservedTerm ->
  State ->
  Either OracleClientProblem PreparedClientIngress
followRedirect lane suppliedHint term predecessor =
  let (hint, diagnostics) = admittedHint suppliedHint predecessor.contacts
      base = predecessor {observedTerm = term, leaderHint = hint}
      -- The first admitted term, or one strictly greater than the retained term,
      -- plus a different checked fixed contact is new routing evidence, not
      -- another fruitless attempt. Repeated, stale, absent, unknown, and self
      -- hints stay on the paced retry path.
      freshTerm = case (predecessor.observedTerm, term) of
        (Nothing, Just _) -> True
        (Just previous, Just current) -> current > previous
        _ -> False
      distinctHint =
        maybe False (/= contactNode (laneContact lane)) hint
      prefix = RejectOracleConnection lane : diagnostics
   in if freshTerm && distinctHint
        then preparedImmediateConnect base prefix
        else preparedRetry base prefix

followCandidateRedirect ::
  OracleConnectAttempt ->
  OracleHelloAcceptance ->
  State ->
  Either OracleClientProblem PreparedClientIngress
followCandidateRedirect attempt acceptance predecessor =
  followRedirect
    (OracleCandidateLane attempt)
    acceptance.leaderHint
    (Just acceptance.term)
    predecessor

rejectCandidate ::
  OracleConnectAttempt ->
  OracleHelloAcceptance ->
  State ->
  Either OracleClientProblem PreparedClientIngress
rejectCandidate attempt acceptance predecessor =
  rejectCandidateWith attempt acceptance predecessor []

rejectCandidateWith ::
  OracleConnectAttempt ->
  OracleHelloAcceptance ->
  State ->
  [OracleClientAction] ->
  Either OracleClientProblem PreparedClientIngress
rejectCandidateWith attempt acceptance predecessor diagnostics =
  let regressed = maybe False (> acceptance.term) predecessor.observedTerm
      (hint, hintDiagnostics)
        | regressed = (Nothing, [])
        | otherwise =
            admittedHint
              (hintDistinctFromContact attempt.contact acceptance.leaderHint)
              predecessor.contacts
      base =
        predecessor
          { observedTerm =
              if regressed
                then predecessor.observedTerm
                else greatestObservedTerm predecessor.observedTerm acceptance.term,
            leaderHint = hint
          }
   in preparedRetry
        base
        (RejectOracleConnection (OracleCandidateLane attempt) : diagnostics <> hintDiagnostics)

preparedRetry ::
  State ->
  [OracleClientAction] ->
  Either OracleClientProblem PreparedClientIngress
preparedRetry predecessor prefix =
  let cancelled = maybe [] (\pending -> [CancelOracleRetry pending]) predecessor.pendingRequestRetry
      base = predecessor {pendingRequestRetry = Nothing}
      retry = OracleRetry base.nextRetryOrdinal
      successor =
        base
          { phase = Retrying retry,
            nextRetryOrdinal = base.nextRetryOrdinal + 1
          }
   in checkedPrepared
        successor
        (prefix <> cancelled <> [ScheduleOracleRetry OracleConnectionRetry retry])

preparedImmediateConnect ::
  State ->
  [OracleClientAction] ->
  Either OracleClientProblem PreparedClientIngress
preparedImmediateConnect predecessor prefix =
  let cancelled = maybe [] (pure . CancelOracleRetry) predecessor.pendingRequestRetry
      base = predecessor {pendingRequestRetry = Nothing}
      (successor, connectActions) = beginConnect base.leaderHint base
   in checkedPrepared successor (prefix <> cancelled <> connectActions)

confirmProgress :: OracleProgress -> State -> Either OracleClientProblem State
confirmProgress through state
  | not (oracleProgressCovers (oracleClientProgressReady state) through) = Left OracleClientStateContradiction
  | otherwise =
      Right
        state
          { receiptRetirementConfirmed = oracleProgressReceipts through <> state.receiptRetirementConfirmed,
            labelRetirementConfirmed = max (oracleProgressLabelsThrough through) state.labelRetirementConfirmed
          }

prepareRetirementNotReady :: OracleBinding -> OracleClientRequestId -> OracleProgress -> OracleObservedTerm -> State -> Either OracleClientProblem PreparedClientIngress
prepareRetirementNotReady binding request offered term predecessor
  | not (oracleClientBindingIsCurrent binding predecessor) = stale predecessor
  | predecessor.receiptHomeClosed = stale predecessor
  | oracleClientRequestHome request /= oracleClientHome predecessor = Left OracleClientStateContradiction
  | oracleClientRequestSequence request /= retirementHighWater (oracleProgressReceipts offered) = Left OracleClientStateContradiction
  | not (oracleProgressCovers (oracleClientProgressReady predecessor) offered) = Left OracleClientStateContradiction
  | oracleProgressCovers (oracleClientProgressConfirmed predecessor) offered = stale predecessor
  | otherwise =
      let observed = predecessor {observedTerm = greatestObservedTerm predecessor.observedTerm term}
          release = ReleaseOracleProgress binding offered
       in case observed.pendingRequestRetry of
            Just _ -> checkedPrepared observed [release]
            Nothing ->
              let retry = OracleRetry observed.nextRetryOrdinal
                  successor = observed {pendingRequestRetry = Just retry, nextRetryOrdinal = observed.nextRetryOrdinal + 1}
               in checkedPrepared successor [release, ScheduleOracleRetry OracleSubmissionRetry retry]

prepareSubmissionNotReady ::
  OracleBinding ->
  OracleClientRequestId ->
  OracleObservedTerm ->
  State ->
  Either OracleClientProblem PreparedClientIngress
prepareSubmissionNotReady binding request term predecessor
  | not (oracleClientBindingIsCurrent binding predecessor) = stale predecessor
  | not (requestAwaitsProjection request predecessor) = stale predecessor
  | otherwise =
      let observed =
            predecessor
              { observedTerm = greatestObservedTerm predecessor.observedTerm term
              }
       in case observed.pendingRequestRetry of
            Just retry ->
              checkedPrepared
                observed
                [AssociateOracleSubmissionRetry binding request retry]
            Nothing ->
              let retry = OracleRetry observed.nextRetryOrdinal
                  successor =
                    observed
                      { pendingRequestRetry = Just retry,
                        nextRetryOrdinal = observed.nextRetryOrdinal + 1
                      }
               in checkedPrepared
                    successor
                    [ AssociateOracleSubmissionRetry binding request retry,
                      ScheduleOracleRetry OracleSubmissionRetry retry
                    ]

requestAwaitsProjection :: OracleClientRequestId -> State -> Bool
requestAwaitsProjection request state =
  maybe
    False
    ((== OracleRequestAwaitingProjection) . oracleRequestWitnessStatus)
    (Map.lookup (OracleRequestRef request) state.oracleRequests)

laneContact :: OracleLane -> OracleContact
laneContact = \case
  OracleCandidateLane attempt -> attempt.contact
  OracleEstablishedLane binding -> binding.contact

hintDistinctFromContact :: OracleContact -> Maybe OracleNodeClaim -> Maybe OracleNodeClaim
hintDistinctFromContact contact = \case
  Just hint
    | hint == contact.node -> Nothing
  other -> other

beginConnect ::
  Maybe OracleNodeClaim ->
  State ->
  (State, [OracleClientAction])
beginConnect preferred predecessor =
  (successor, oracleClientActions successor)
  where
    OracleContactSet contacts = predecessor.contacts
    retained = NonEmpty.toList contacts
    values =
      filter (\contact -> Set.member contact.node predecessor.preferredVoters) retained
        <> filter (\contact -> Set.notMember contact.node predecessor.preferredVoters) retained
    (selectedOffset, selected) = case preferred >>= (`lookupContactWithOffset` values) of
      Just hinted -> hinted
      Nothing ->
        let offset = predecessor.nextContactOffset `mod` length values
         in (offset, values !! offset)
    attempt =
      OracleConnectAttempt
        { ordinal = predecessor.nextAttemptOrdinal,
          contact = selected,
          fromExclusive = predecessor.appliedCursor
        }
    successor =
      predecessor
        { phase = Connecting attempt,
          nextContactOffset = selectedOffset + 1,
          nextAttemptOrdinal = predecessor.nextAttemptOrdinal + 1
        }

lookupContact :: OracleNodeClaim -> [OracleContact] -> Maybe OracleContact
lookupContact node contacts = snd <$> lookupContactWithOffset node contacts

lookupContactWithOffset ::
  OracleNodeClaim ->
  [OracleContact] ->
  Maybe (Int, OracleContact)
lookupContactWithOffset node = go 0
  where
    go _ [] = Nothing
    go offset (contact : remaining)
      | contactNode contact == node = Just (offset, contact)
      | otherwise = go (offset + 1) remaining

-- | A leader hint is a one-attempt routing preference, not sticky authority.
-- Once its exact endpoint fails or its binding is lost, fixed-contact retry
-- continues at the successor already recorded by 'beginConnect'.
forgetFailedContact :: OracleContact -> State -> State
forgetFailedContact failed state
  | state.leaderHint == Just (contactNode failed) =
      state {Eclips.Herald.OracleClient.State.leaderHint = Nothing}
  | otherwise = state

contactNode :: OracleContact -> OracleNodeClaim
contactNode (OracleContact node _ _) = node

admittedHint ::
  Maybe OracleNodeClaim ->
  OracleContactSet ->
  (Maybe OracleNodeClaim, [OracleClientAction])
admittedHint Nothing _ = (Nothing, [])
admittedHint (Just hint) (OracleContactSet contacts)
  | isJust (lookupContact hint (NonEmpty.toList contacts)) = (Just hint, [])
  | otherwise =
      ( Nothing,
        [ReportOracleClientDiagnostic (UnknownOracleLeaderHint hint)]
      )

stale :: State -> Either OracleClientProblem PreparedClientIngress
stale state =
  Right
    ( PreparedClientIngress
        state
        [ReportOracleClientDiagnostic StaleOracleClientIngress]
    )

checkedPrepared ::
  State ->
  [OracleClientAction] ->
  Either OracleClientProblem PreparedClientIngress
checkedPrepared successor actions = do
  validateOperationalState successor
  Right (PreparedClientIngress successor actions)

currentAttemptIs :: OracleConnectAttempt -> State -> Bool
currentAttemptIs attempt state = case state.phase of
  Connecting current -> current == attempt
  _ -> False

laneIsCurrent :: OracleLane -> State -> Bool
laneIsCurrent lane state = case (lane, state.phase) of
  (OracleCandidateLane attempt, Connecting current) -> attempt == current
  (OracleEstablishedLane binding, Bound current) -> binding == current
  _ -> False

-- The opaque owner is constructed here and every mutation preserves its
-- retained-history invariants. Live transitions check the changed input and
-- these bounded routing facts; they do not re-audit every completed request.
-- 'validateState' remains the exhaustive independent audit for properties and
-- composition checks. In particular, deferral and suppression admission above
-- retain their input-dependent history checks before installing any change.
validateOperationalState :: State -> Either OracleClientProblem ()
validateOperationalState state
  | state.lastSemanticCursor < state.initialCursor || state.lastSemanticCursor > state.appliedCursor = contradiction
  | state.canonicalCoveredThrough < state.initialCursor || state.canonicalCoveredThrough > state.evidenceCompactedThrough = contradiction
  | state.evidenceCompactedThrough < state.initialCursor || state.evidenceCompactedThrough > state.appliedCursor = contradiction
  | state.labelEvidenceCompactedThrough > state.labelRetirementReady = contradiction
  | state.receiptRetirementConfirmed <> state.receiptRetirementReady /= state.receiptRetirementReady
      || retirementHighWater state.receiptRetirementReady >= state.nextRequestSequence =
      contradiction
  | state.labelRetirementConfirmed > state.labelRetirementReady
      || state.labelRetirementReady > state.latestLabelDecision
      || state.latestLabelDecision > state.appliedCursor =
      contradiction
  | maybe False ((<= state.labelRetirementReady) . fst) (Map.lookupMin state.labelPins) = contradiction
  | state.requestedCursor /= state.appliedCursor = contradiction
  | not (hintIsKnown state.leaderHint state.contacts) = contradiction
  | not (phaseIsValid state) = contradiction
  | not (pendingRequestRetryIsValid state) = contradiction
  | otherwise = Right ()
  where
    contradiction = Left OracleClientStateContradiction

validateState :: State -> Either OracleClientProblem ()
validateState state
  | state.lastSemanticCursor < state.initialCursor || state.lastSemanticCursor > state.appliedCursor = contradiction
  | state.canonicalCoveredThrough < state.initialCursor || state.canonicalCoveredThrough > state.evidenceCompactedThrough = contradiction
  | state.evidenceCompactedThrough < state.initialCursor || state.evidenceCompactedThrough > state.appliedCursor = contradiction
  | state.labelEvidenceCompactedThrough > state.labelRetirementReady || not (labelEvidenceAnchorValid state) = contradiction
  | state.receiptRetirementConfirmed <> state.receiptRetirementReady /= state.receiptRetirementReady
      || retirementHighWater state.receiptRetirementReady >= state.nextRequestSequence =
      contradiction
  | state.receiptOutstanding /= Set.fromList [oracleClientRequestSequence (oracleRequestRefRequestId reference) | (reference, witness) <- Map.toList state.oracleRequests, not (isSettled witness)] = contradiction
  | any (not . isSettled) [witness | (reference, witness) <- Map.toAscList state.oracleRequests, Lifetime.receiptIsRetired (oracleClientRequestSequence (oracleRequestRefRequestId reference)) state.receiptRetirementReady] = contradiction
  | state.labelRetirementConfirmed > state.labelRetirementReady
      || state.labelRetirementReady > state.latestLabelDecision
      || state.latestLabelDecision > state.appliedCursor =
      contradiction
  | maybe False ((<= state.labelRetirementReady) . fst) (Map.lookupMin state.labelPins) = contradiction
  | state.requestedCursor /= state.appliedCursor = contradiction
  | not (hintIsKnown state.leaderHint state.contacts) = contradiction
  | not (phaseIsValid state) = contradiction
  | not (pendingRequestRetryIsValid state) = contradiction
  | not (oracleRequestsAreValid state) = contradiction
  | not (importedLabelEvidenceIsValid state) = contradiction
  | not (labelPinsAreValid state) = contradiction
  | Set.filter (> state.evidenceCompactedThrough) (Map.keysSet state.retainedEntries `Set.union` Map.keysSet state.membershipAdvances)
      /= Set.fromList (expectedEvidenceIndices state) =
      contradiction
  | any (\index -> index <= state.initialCursor || index > state.appliedCursor) (Map.keys state.retainedEntries <> Map.keys state.membershipAdvances) = contradiction
  | any (\canonical -> not (oracleClientRetainsEntry canonical state)) (Map.elems state.retainedEntries) = contradiction
  | not (overlappingEvidenceIsValid state) =
      contradiction
  | any
      (\(index, entry) -> appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry) /= index)
      (Map.toAscList state.retainedEntries) =
      contradiction
  | otherwise = Right ()
  where
    contradiction = Left OracleClientStateContradiction

    isSettled witness = case oracleRequestWitnessStatus witness of
      OracleRequestProjected _ -> True
      OracleRequestEndedBeforeSubmission _ -> True
      _ -> False

-- Independent reconstruction at explicit invariant/property boundaries only.
labelEvidenceAnchorValid :: State -> Bool
labelEvidenceAnchorValid state
  | state.labelEvidenceCompactedThrough == controlIndex 0 = True
  | otherwise = case Map.lookup state.labelEvidenceCompactedThrough state.retainedEntries of
      Nothing -> Map.member state.labelEvidenceCompactedThrough imported
      Just canonical -> entryDecidesLabelAt state.labelEvidenceCompactedThrough canonical
  where
    ImportedLabelEvidence imported = state.importedLabelEvidence

importedLabelEvidenceIsValid :: State -> Bool
importedLabelEvidenceIsValid state = all valid (Map.toAscList imported)
  where
    ImportedLabelEvidence imported = state.importedLabelEvidence
    valid (index, decision) =
      index > controlIndex 0
        && index <= state.canonicalCoveredThrough
        && index <= state.latestLabelDecision
        && case Map.lookup index state.retainedEntries of
          Nothing -> True
          Just canonical ->
            [ liveDecisionId observed
            | event <- appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical),
              LabelDecidedView observed _ _ <- [oracleProjectionEventView event],
              liveDecisionRequestControlIndex observed == index
            ]
              == [decision]

entryDecidesLabelAt :: ControlIndex -> CanonicalAppliedOracleEntry -> Bool
entryDecidesLabelAt index canonical =
  any
    ( \event -> case oracleProjectionEventView event of
        LabelDecidedView decision _ _ -> liveDecisionRequestControlIndex decision == index
        _ -> False
    )
    (appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical))

labelPinsAreValid :: State -> Bool
labelPinsAreValid state =
  state.labelCompletionRequests == expectedRequests
    && Just state.labelPins == Map.traverseWithKey (\index count -> LabelPin count <$> Map.lookup index previousLabels) expectedCounts
    && all (\index -> index > controlIndex 0 && index <= state.latestLabelDecision) (Map.keys state.labelPins)
    && state.labelRetirementReady == expectedReady
  where
    expectedCounts = Map.fromListWith (+) [(index, 1) | index <- Map.elems state.labelConsumers <> Map.elems expectedRequests]
    ImportedLabelEvidence imported = state.importedLabelEvidence
    decisionIndices =
      Set.toAscList
        ( Map.keysSet imported
            `Set.union` Set.fromList
              [ index
              | (index, canonical) <- Map.toAscList state.retainedEntries,
                index <= state.latestLabelDecision,
                any (\event -> case oracleProjectionEventView event of LabelDecidedView {} -> True; _ -> False) (appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical))
              ]
        )
    previousLabels = Map.fromAscList (zip decisionIndices (controlIndex 0 : decisionIndices))
    expectedRequests =
      Map.fromList
        [ (reference, labelCompletionControlIndex attestation)
        | (reference, witness) <- Map.toList state.oracleRequests,
          CompleteLabelDecisionIntent attestation <- [oracleRequestWitnessIntent witness],
          case oracleRequestWitnessStatus witness of
            OracleRequestProjected _ -> False
            _ -> True
        ]
    expectedReady = case Map.lookupMin expectedCounts of
      Nothing -> state.latestLabelDecision
      Just (index, _) -> Map.findWithDefault (controlIndex 0) index previousLabels

overlappingEvidenceIsValid :: State -> Bool
overlappingEvidenceIsValid state =
  all matchesMembershipEntry overlapping
  where
    overlapping =
      Set.toAscList
        ( Map.keysSet state.retainedEntries
            `Set.intersection` Map.keysSet state.membershipAdvances
        )
    matchesMembershipEntry index =
      case ( Map.lookup index state.retainedEntries,
             Map.lookup index state.membershipAdvances
           ) of
        (Just canonical, Just generation) ->
          canonicalMembershipGeneration canonical == Just generation
        _ -> False

canonicalMembershipGeneration ::
  CanonicalAppliedOracleEntry ->
  Maybe HeraldMembershipGenerationId
canonicalMembershipGeneration canonical =
  case [ heraldMembershipGenerationId generation
       | event <-
           appliedEntryProjectionEvents
             (canonicalAppliedOracleEntryValue canonical),
         HeraldMembershipAdvancedView generation <- [oracleProjectionEventView event]
       ] of
    [generation] -> Just generation
    _ -> Nothing

-- A rejected Open remains stable on unchanged input. Advancing the canonical
-- prefix allows its owner to re-evaluate the candidate and mint a fresh cycle;
-- the live candidate coordinator waits while its matching label is active.
releaseRejectedDisappearanceOpenKeys :: State -> State
releaseRejectedDisappearanceOpenKeys state =
  state
    { oracleRequestKeys =
        Map.filter
          ( \reference ->
              maybe
                False
                (not . rejectedDisappearanceOpenBeforeCursor state)
                (Map.lookup reference state.oracleRequests)
          )
          state.oracleRequestKeys
    }

rejectedDisappearanceOpenBeforeCursor :: State -> OracleRequestWitness -> Bool
rejectedDisappearanceOpenBeforeCursor state witness =
  case (oracleRequestWitnessIntent witness, oracleRequestWitnessStatus witness) of
    (OpenDisappearanceProbeIntent {}, OracleRequestProjected index)
      | index < state.lastSemanticCursor,
        Just canonical <- Map.lookup index state.retainedEntries,
        Just command <- appliedEntryCommand (canonicalAppliedOracleEntryValue canonical),
        OracleRejected _ <- oracleReceiptResult (appliedEntryReceipt command) ->
          True
    _ -> False

oracleRequestsAreValid :: State -> Bool
oracleRequestsAreValid state =
  Map.fromList
    [ (oracleRequestWitnessSemanticKey witness, reference)
    | (reference, witness) <- requests,
      case oracleRequestWitnessIntent witness of
        OpenDisappearanceProbeIntent {} ->
          Map.notMember reference state.suppressedDisappearanceRequests
            && not (rejectedDisappearanceOpenBeforeCursor state witness)
        _ -> True
    ]
    == state.oracleRequestKeys
    && state.nextRequestSequence == expectedNextSequence
    && Map.keysSet state.requestPreparedAt == Map.keysSet state.oracleRequests
    && state.requestObservedAt == Map.fromList observedRequests
    && Map.size state.requestObservedAt == length observedRequests
    && all (<= state.appliedCursor) (Map.elems state.requestPreparedAt)
    && all requestIsValid requests
    && all suppressedRequestIsValid (Map.toAscList state.suppressedDisappearanceRequests)
    && state.settledLabelRequests
      == Map.fromListWith
        Set.union
        [ (index, Set.singleton reference)
        | (reference, witness) <- requests,
          Just index <- [settledLabelRequestDecisionIndex witness state]
        ]
  where
    requests = Map.toAscList state.oracleRequests
    observedRequests =
      [ (reference, index)
      | (index, canonical) <- Map.toAscList state.retainedEntries,
        Just command <- [appliedEntryCommand (canonicalAppliedOracleEntryValue canonical)],
        let reference = OracleRequestRef (appliedEntryRequestId command),
        -- Cursor admission indexes only requests that already existed when
        -- the entry arrived. Historical entries preceding local preparation
        -- cannot become this request's result merely by sharing its raw ID.
        Just preparedAt <- [Map.lookup reference state.requestPreparedAt],
        index > preparedAt
      ]
    -- Settled labels can relinquish their local witnesses. The existing issued
    -- receipt high water still proves those request identities were allocated.
    expectedNextSequence =
      1
        + maximum
          ( retirementHighWater state.receiptRetirementReady
              : [ oracleClientRequestSequence (oracleRequestRefRequestId reference)
                | (reference, _) <- requests
                ]
          )
    suppressedRequestIsValid (reference, (probe, index)) =
      case (Map.lookup reference state.oracleRequests, Map.lookup index state.retainedEntries) of
        (Just witness, Just canonical) ->
          isDisappearanceIntent (oracleRequestWitnessIntent witness)
            && any
              (terminalFor probe . oracleProjectionEventView)
              (appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical))
        _ -> False
    terminalFor expected = \case
      DisappearanceProbeInvalidatedView probe _ -> probe == expected
      DisappearanceProbeResolvedView probe _ -> probe == expected
      DisappearanceProbeAbortedView probe _ -> probe == expected
      _ -> False
    requestIsValid (reference, witness) =
      let requestId = oracleRequestRefRequestId reference
          envelope = oracleRequestWitnessEnvelope witness
          rawEnvelope = canonicalOracleEnvelopeValue envelope
          requestMatches =
            oracleRequestWitnessRef witness == reference
              && oracleClientRequestHome requestId == oracleClientHome state
              && oracleEnvelopeRequestId rawEnvelope == requestId
              && oracleEnvelopeHomeHeraldEpoch rawEnvelope == oracleClientHome state
              && oracleEnvelopeExpectedControlIndex rawEnvelope == Nothing
              && Right (oracleEnvelopeCommand rawEnvelope)
                == commandForIntent
                  (oracleRequestWitnessIntent witness)
       in requestMatches
            && case oracleRequestWitnessStatus witness of
              OracleRequestAwaitingSourceDrain -> case oracleRequestWitnessIntent witness of
                DecideLabelIntent {} -> True
                _ -> False
              OracleRequestEndedBeforeSubmission index -> case oracleRequestWitnessIntent witness of
                DecideLabelIntent _ caller _ _ _ _ _ _ _ -> retainedProcessEndAt caller index state
                _ -> False
              OracleRequestAwaitingProjection -> True
              OracleRequestDeferred _ observedIndex ->
                observedIndex >= state.initialCursor
                  && case oracleRequestWitnessIntent witness of
                    DecideLabelIntent {} -> True
                    _ -> False
              OracleRequestProjected index ->
                case Map.lookup index state.retainedEntries of
                  Nothing -> False
                  Just retained ->
                    case appliedEntryCommand (canonicalAppliedOracleEntryValue retained) of
                      Nothing -> False
                      Just command ->
                        appliedEntryRequestId command == requestId
                          && appliedEntryCommandDigest command == canonicalOracleEnvelopeDigest envelope

commandForIntent :: OracleSemanticIntent -> Either OracleClientProblem OracleCommand
commandForIntent intent = case intent of
  HeraldAdmissionIntent command -> Right (heraldAdmissionCommand command)
  VoterAdministrationIntent operation -> Right (commandForVoterOperation operation)
  FailureVoterCancellationIntent identifier -> Right (Voter.cancelVoterChangeCommand identifier)
  DynamicStartIntent bootstrap ->
    Right (startProcessEpochCommand bootstrap)
  ProcessEndIntent process reason ->
    either
      (Left . OracleEndProcessCommandProblem)
      Right
      (endProcessEpochCommand process reason)
  DecideLabelIntent
    decision
    caller
    object
    expected
    initialEvidence
    authority
    justification
    target
    acceptanceCut ->
      Right
        ( decideLabelCommand
            decision
            caller
            object
            expected
            initialEvidence
            authority
            justification
            target
            acceptanceCut
        )
  CompleteLabelDecisionIntent report ->
    Right (completeLabelDecisionCommand report)
  OpenHeraldFailureProbeIntent target generation _ configuration ->
    Right (openHeraldFailureProbeCommand target generation configuration)
  ReportHeraldFailureProbeIntent probe configuration result ->
    Right (reportHeraldFailureProbeCommand probe configuration result)
  DismissHeraldFailureProbeIntent resolution ->
    Right (dismissHeraldFailureProbeCommand resolution)
  AcceptVoterHostFailureIntent resolution ->
    Right (acceptVoterHostFailureCommand resolution)
  RetireHeraldEpochIntent resolution target ->
    Right (retireHeraldEpochCommand resolution target)
  OpenDisappearanceProbeIntent subject coordinate ->
    Right (openDisappearanceProbeCommand subject coordinate)
  ReportPredefinedAbsenceIntent claim ->
    Right (reportPredefinedAbsenceCommand (disappearanceEvidenceClaimProbeId claim) claim)
  InvalidateDisappearanceProbeIntent probe reporter reason witness ->
    Right (invalidateDisappearanceProbeCommand probe reporter reason witness)
  ResolveDisappearanceProbeIntent probe digest ->
    Right (resolveDisappearanceProbeCommand probe digest)
  AbortDisappearanceProbeIntent probe reason ->
    Right (abortDisappearanceProbeCommand probe reason)

commandForVoterOperation :: OracleVoterOperation -> OracleCommand
commandForVoterOperation = \case
  RegisterOracleReplica node host contact -> registerOracleReplicaCommand node host contact
  BeginOracleVoterChange expected reason bindings -> beginVoterChangeCommand expected reason bindings
  CancelOracleVoterChange change -> cancelVoterChangeCommand change

phaseIsValid :: State -> Bool
phaseIsValid state = case state.phase of
  Joining -> True
  Connecting attempt ->
    attempt.ordinal < state.nextAttemptOrdinal
      && attempt.fromExclusive == state.appliedCursor
      && contactIsKnown attempt.contact state.contacts
  Retrying (OracleRetry ordinal) -> ordinal < state.nextRetryOrdinal
  Bound binding ->
    case binding of
      OracleBinding (OracleBindingGeneration generation) contact ->
        generation < state.nextBindingGeneration
          && contactIsKnown contact state.contacts
  Retired -> True

pendingRequestRetryIsValid :: State -> Bool
pendingRequestRetryIsValid state = case state.pendingRequestRetry of
  Nothing -> True
  Just (OracleRetry ordinal) ->
    ordinal < state.nextRetryOrdinal
      && case state.phase of
        Bound _ -> True
        _ -> False

contactIsKnown :: OracleContact -> OracleContactSet -> Bool
contactIsKnown contact (OracleContactSet contacts) =
  contact `elem` NonEmpty.toList contacts

hintIsKnown :: Maybe OracleNodeClaim -> OracleContactSet -> Bool
hintIsKnown Nothing _ = True
hintIsKnown (Just hint) (OracleContactSet contacts) =
  isJust (lookupContact hint (NonEmpty.toList contacts))

greatestObservedTerm :: Maybe OracleObservedTerm -> OracleObservedTerm -> Maybe OracleObservedTerm
greatestObservedTerm Nothing observed = Just observed
greatestObservedTerm (Just retained) observed = Just (max retained observed)

expectedEvidenceIndices :: State -> [ControlIndex]
expectedEvidenceIndices state =
  fmap
    controlIndex
    [ controlIndexWord64 state.evidenceCompactedThrough + 1
      .. controlIndexWord64 state.appliedCursor
    ]

nextControlIndex :: ControlIndex -> ControlIndex
nextControlIndex = controlIndex . (+ 1) . controlIndexWord64

interpretAdmissionResult :: Admission.HeraldAdmissionCommand -> ControlIndex -> [OracleProjectionEvent] -> Either OracleClientProblem OracleRequestResult
interpretAdmissionResult command index events = case [record | event <- events, HeraldAdmissionChangedView record <- [oracleProjectionEventView event]] of
  [record] | Admission.admissionRecordChangedIndex record == index && matches record -> Right (OracleRequestHeraldAdmissionChanged record)
  _ -> Left OracleRequestResultMismatch
  where
    matches record = case command of
      Admission.BeginHeraldAdmission manifest _ -> Admission.admissionRecordManifest record == manifest && Admission.admissionRecordBeginIndex record == index
      Admission.SealHeraldAdmission seal -> Admission.admissionRecordId record == Admission.joinSealAdmissionId seal && Admission.admissionRecordSeal record == Just seal
      Admission.AcceptHeraldJoinSeal ident attempt digest -> Admission.admissionRecordId record == ident && Admission.admissionRecordAttempt record == attempt && fmap Admission.joinSealDigest (Admission.admissionRecordSeal record) == Just digest
      Admission.HeraldJoinBaseReady report -> Admission.admissionRecordId record == Admission.joinReadyAdmissionId report && Admission.joinReadyReporter report `elem` Admission.admissionRecordBaseReporters record
      Admission.HeraldJoinReady report -> Admission.admissionRecordId record == Admission.joinReadyAdmissionId report && Admission.admissionRecordNewcomerReport record == Just report
      Admission.ActivateHerald ident -> Admission.admissionRecordId record == ident && case Admission.admissionRecordPhase record of Admission.AdmissionActivated activated _ -> activated == index; _ -> False
      Admission.CancelHeraldAdmission ident -> Admission.admissionRecordId record == ident && Admission.admissionRecordPhase record == Admission.AdmissionCancelled index
