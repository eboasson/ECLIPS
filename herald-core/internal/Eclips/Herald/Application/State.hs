{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The application owner: one stable private namespace and bootstrap access
-- record for every live resident process epoch, plus the Step-4 ordinary-value
-- translation boundary over those namespaces.
module Eclips.Herald.Application.State
  ( State,
    emptyState,
    takePreparationSessionChanges,
    clearPreparationSessionChanges,
    ApplicationRoot (..),
    BootstrapAccess,
    bootstrapAccessProcessEpoch,
    bootstrapAccessProcess,
    bootstrapAccessRoots,
    bootstrapAccessPrimordial,
    bootstrapAccessEnvironmentSources,
    RootAccess (..),
    applicationBootstrapAccess,
    applicationAttachmentProcess,
    applicationPrivateIdentity,
    applicationProcessWitnesses,
    ApplicationStateWitness (..),
    ApplicationSessionWitness (..),
    ApplicationRequestStateWitness (..),
    ApplicationRequestOwnerWitness (..),
    ApplicationRequestWitness (..),
    applicationStateWitness,
    applicationRequestStateWitness,
    applicationRequestOwnerEntries,
    applicationRequestEntries,
    applicationRequestReplyBodyEntries,
    applicationWitnessAttachments,
    applicationWitnessSessions,
    applicationWitnessOpenRetryIndex,
    applicationWitnessNextSessionOrdinal,
    applicationSessionWitnessId,
    applicationSessionWitnessProcess,
    applicationSessionWitnessAttachment,
    applicationSessionWitnessNonce,
    applicationSessionWitnessResumeToken,
    applicationSessionWitnessBinding,
    applicationSessionWitnessDeliveryLive,
    applicationSessionWitnessStartupAccess,
    ApplicationSessionRecoveryWitness (..),
    applicationSessionRecoveryEntries,
    ApplicationProcessRecoveryStatusWitness (..),
    ApplicationProcessRecoveryWitness (..),
    applicationProcessRecoveryEntries,
    applicationRequestOwnerWitnesses,
    applicationRequestNextAcceptancePositions,
    applicationRequestOwnerSession,
    applicationRequestOwnerBindingEstablishedBy,
    applicationRequestOwnerLastIssuedReplyCursor,
    applicationRequestOwnerRetiredReplyCursor,
    applicationReceiptRetirementProgress,
    applicationRequestRetirement,
    PreparedApplicationReceiptRetirement,
    prepareApplicationReceiptRetirement,
    commitApplicationReceiptRetirement,
    applicationRequestOwnerRequests,
    applicationRequestWitnessId,
    applicationRequestWitnessCall,
    applicationRequestWitnessProcess,
    applicationRequestWitnessPosition,
    applicationRequestWitnessCursor,
    applicationRequestWitnessStatus,
    ApplicationPublicationCall,
    applicationPublicationCall,
    ApplicationPublicationRequestWitness,
    applicationPublicationRequestEntries,
    applicationPublicationRequestWitnessId,
    applicationPublicationRequestWitnessPrivateNabla,
    applicationPublicationRequestWitnessValue,
    applicationPublicationRequestWitnessProcess,
    applicationPublicationRequestWitnessPosition,
    NewEnvironmentCoordinatorInput,
    newEnvironmentCoordinatorInput,
    EnvironmentRequestStage (..),
    EnvironmentRequestWitness,
    applicationEnvironmentRequestEntries,
    environmentRequestWitnessId,
    environmentRequestWitnessProcess,
    environmentRequestWitnessStage,
    PendingEnvironment,
    applicationPendingEnvironmentEntries,
    CompletedEnvironment,
    applicationCompletedEnvironmentEntries,
    replaceApplicationAttachmentProcessForInvariantTest,
    replaceApplicationAttachmentForProcessForInvariantTest,
    replaceApplicationSessionProcessForInvariantTest,
    replaceApplicationSessionBindingForInvariantTest,
    replaceApplicationSessionStartupAccessForInvariantTest,
    replaceApplicationOpenRetryIndexForInvariantTest,
    replaceApplicationNextSessionOrdinalForInvariantTest,
    forceApplicationProcessAvailableForInvariantTest,
    ProcessRoleView,
    processRoleView,
    ApplicationValueRejection (..),
    ApplicationValueInvariant (..),
    ApplicationValueGlobalizationError (..),
    globalizeOrdinaryApplicationValue,
    resolveApplicationPrivateUniqueId,
    globalizeApplicationLabel,
    PreparedOrdinaryApplicationValueLocalization,
    prepareOrdinaryApplicationValueLocalization,
    prepareOrdinaryApplicationValueLocalizationWithZombieOverlay,
    preparedLocalizedOrdinaryApplicationValue,
    commitOrdinaryApplicationValueLocalization,
    ApplicationSessionInvariant (..),
    applicationStartupAccessForBootstrap,
    ApplicationSessionTransitionError (..),
    ApplicationInitialClaimError (..),
    ApplicationInitialClaimOutcome (..),
    ApplicationInitialClaimStatus (..),
    applicationInitialClaimEntries,
    applicationInitialClaimIsClaimed,
    PreparedApplicationInitialClaim,
    prepareApplicationInitialClaim,
    prepareApplicationGenesisInitialClaim,
    preparedApplicationInitialClaimOutcome,
    preparedApplicationInitialClaimTimerCancellation,
    commitApplicationInitialClaim,
    PreparedApplicationSessionAcceptance,
    prepareApplicationSessionOpen,
    prepareApplicationSessionResume,
    preparedApplicationSessionAcceptance,
    commitApplicationSessionAcceptance,
    PreparedApplicationBindingLoss,
    prepareApplicationBindingLoss,
    prepareApplicationBindingTermination,
    ApplicationBindingLossDisposition (..),
    preparedApplicationBindingLossDisposition,
    commitApplicationBindingLoss,
    ApplicationRecoveryExpiration,
    applicationRecoveryExpiredSessionIds,
    applicationRecoveryExpiredWaitIds,
    applicationRecoveryCancelledTimers,
    applicationRecoverySealedProcess,
    PreparedApplicationRecoverySweep,
    prepareApplicationRecoverySweep,
    preparedApplicationRecoverySweepExpiration,
    commitApplicationRecoverySweep,
    ApplicationRecoveryTimerDisposition (..),
    PreparedApplicationRecoveryTimerObservation,
    prepareApplicationRecoveryTimerObservation,
    preparedApplicationRecoveryTimerDisposition,
    commitApplicationRecoveryTimerObservation,
    preparedApplicationSessionAcceptanceTimerCancellation,
    PreparedApplicationSessionEnd,
    prepareApplicationSessionEnd,
    preparedApplicationSessionEndWaitIds,
    commitApplicationSessionEnd,
    ApplicationProcessRetirement,
    applicationProcessRetirementSessionIds,
    applicationProcessRetirementWaitIds,
    applicationProcessRetirementTimerAttempts,
    PreparedApplicationProcessRetirement,
    prepareApplicationProcessRetirement,
    preparedApplicationProcessRetirement,
    commitApplicationProcessRetirement,
    applicationSessionProcess,
    applicationSessionAttachment,
    applicationSessionCurrentBinding,
    applicationSessionDeliveryLive,
    ApplicationIsolationNotice,
    applicationIsolationNoticeBinding,
    applicationIsolationNoticeCursor,
    applicationIsolationNoticeSession,
    beginApplicationIsolationDrain,
    cutApplicationRecoveryForDrain,
    ApplicationPublicationRequestClassification (..),
    ApplicationPublicationRequest,
    applicationPublicationRequestProcess,
    applicationPublicationRequestPrivateNabla,
    applicationPublicationRequestValue,
    applicationPublicationRequestPosition,
    applicationPublicationRequestState,
    applicationPublicationRequestIsRetry,
    classifyApplicationPublicationRequest,
    PreparedApplicationPublicationRequest,
    prepareApplicationPublicationRequestAcceptance,
    commitApplicationPublicationRequest,
    EnvironmentRequestClassification (..),
    EnvironmentRequest,
    environmentRequestSession,
    environmentRequestBinding,
    environmentRequestProcess,
    environmentRequestId,
    environmentRequestPosition,
    environmentRequestState,
    environmentRequestIsRetry,
    environmentRequestApplicationCandidate,
    classifyEnvironmentRequest,
    applicationEnvironmentCandidateKeys,
    prepareApplicationEnvironmentCandidate,
    applicationEnvironmentCandidateForRedrive,
    PreparedEnvironmentRequestCandidate,
    prepareEnvironmentRequestCandidate,
    commitEnvironmentRequestCandidate,
    EnvironmentRequestAcceptanceError (..),
    PreparedEnvironmentRequestAcceptance,
    prepareApplicationEnvironmentAcceptance,
    prepareEnvironmentRequestAcceptance,
    preparedEnvironmentRequestAcceptanceReply,
    commitEnvironmentRequestAcceptance,
    EnvironmentSettlementError (..),
    EnvironmentSettlementDisposition (..),
    PreparedEnvironmentSettlement,
    prepareEnvironmentSettlement,
    expandPendingEnvironment,
    preparedEnvironmentSettlementDisposition,
    preparedEnvironmentSettlementCompletion,
    preparedEnvironmentSettlementCompletionOutcome,
    commitEnvironmentSettlement,
    ApplicationRequestClassification (..),
    ApplicationRequestClass (..),
    ApplicationRequestKey,
    applicationRequestKey,
    applicationRequestKeySession,
    applicationRequestKeyRequest,
    applicationRequestKeyCanonicalBytes,
    LabelApplication,
    labelApplication,
    labelApplicationOperation,
    applicationOperationLabel,
    labelApplicationObject,
    labelApplicationExpected,
    labelApplicationTarget,
    SequencingSelection,
    sequencingSelection,
    sequencingSelectionTarget,
    sequencingSelectionGroup,
    ApplicationGateGeneration,
    applicationGateGenerationWord64,
    ApplicationGatePhase (..),
    applicationGatePhase,
    applicationMembershipGateClosed,
    setApplicationMembershipGate,
    ApplicationLabelCandidateView,
    applicationLabelCandidateKey,
    applicationLabelCandidateProcess,
    applicationLabelCandidateCall,
    applicationLabelCandidateEntries,
    ApplicationGatedIngressView,
    applicationGatedIngressKey,
    applicationGatedIngressProcess,
    applicationGatedIngressOperation,
    applicationGatedIngressGeneration,
    applicationGatedIngressReady,
    applicationGatedIngressEntries,
    applicationEndpointAdmissionHold,
    applicationEndpointAdmissionHoldFor,
    ApplicationFenceHeldWork,
    applicationFenceHeldWork,
    applicationFenceHeldWorkWriter,
    applicationFenceHeldWorkSortId,
    applicationFenceHeldWorkSortOccurrenceId,
    applicationFenceHeldWorkAdmittedValue,
    applicationFenceHeldWorkOrigin,
    applicationFenceHeldWorkSourceStrength,
    applicationFenceHeldWorkControlPrerequisite,
    applicationFenceHeldWorkGroupMembership,
    ApplicationFenceHeldView,
    applicationFenceHeldPosition,
    applicationFenceHeldOperation,
    applicationFenceHeldSemanticWork,
    applicationFenceHeldReplyKey,
    applicationFenceHeldDisappearanceProbes,
    applicationFenceHeldEntries,
    ActiveLabelApplicationView,
    activeLabelApplicationDecision,
    activeLabelApplicationKey,
    activeLabelApplicationCall,
    activeLabelApplicationSelection,
    activeLabelApplicationPosition,
    activeLabelApplicationTerminalResult,
    applicationActiveLabel,
    applicationActiveLabels,
    applicationHasActiveLabels,
    applicationLabelIndexesValid,
    applicationActiveLabelForDecision,
    applicationActiveLabelForObject,
    applicationActiveLabelForGroup,
    ApplicationRequestCandidate,
    applicationRequestCandidateSession,
    applicationRequestCandidateBinding,
    applicationRequestCandidateProcess,
    applicationRequestCandidateId,
    applicationRequestCandidateCall,
    applicationRequestCandidatePosition,
    applicationRequestCandidateWaitId,
    applicationRequestCandidateState,
    applicationRequestCandidateReplyOwned,
    applicationRequestCandidateReplyDeliverable,
    applicationLabelCandidateForReconsideration,
    applicationGatedIngressForRedrive,
    applicationFenceHeldForRedrive,
    moveGatedLabelToCandidate,
    classifyApplicationRequest,
    applicationRequestClassificationReply,
    PreparedApplicationRequest,
    prepareApplicationRequestRejection,
    prepareApplicationRequestCompletion,
    prepareApplicationOperationAcceptance,
    prepareApplicationRequestCompletionFromLocalizedState,
    prepareApplicationGeneratedIdCompletion,
    prepareApplicationWaitAcceptance,
    preparedApplicationRequestReply,
    commitApplicationRequest,
    PreparedApplicationDetachedRequest,
    prepareApplicationLabelCandidate,
    prepareApplicationGatedIngress,
    prepareApplicationEndpointHeldIngress,
    prepareApplicationFenceHeld,
    prepareApplicationDisappearanceHeld,
    commitApplicationDetachedRequest,
    PreparedApplicationLabelAcceptance,
    prepareApplicationLabelAcceptance,
    prepareRetainedApplicationLabelAcceptance,
    retainApplicationLabelEndpointHold,
    preparedApplicationLabelAcceptancePosition,
    preparedApplicationLabelAcceptanceReply,
    commitApplicationLabelAcceptance,
    PreparedApplicationLabelCandidateRejection,
    prepareApplicationLabelCandidateRejection,
    preparedApplicationLabelCandidateRejectionReply,
    commitApplicationLabelCandidateRejection,
    PreparedApplicationLabelCandidateCompletion,
    prepareApplicationLabelCandidateCompletion,
    preparedApplicationLabelCandidateCompletionReply,
    commitApplicationLabelCandidateCompletion,
    PreparedApplicationLabelTerminal,
    prepareApplicationLabelTerminal,
    commitApplicationLabelTerminal,
    PreparedApplicationLabelWorkflowCompletion,
    prepareApplicationLabelWorkflowCompletion,
    preparedApplicationLabelWorkflowCompletionOutcome,
    commitApplicationLabelWorkflowCompletion,
    applicationRequestResult,
    applicationRequestResultDisposition,
    ApplicationOperationCompletionOutcome (..),
    ApplicationOperationCompletionObservation,
    applicationOperationCompletionBinding,
    applicationOperationCompletionCursor,
    applicationOperationCompletionRequestId,
    applicationOperationCompletionResult,
    PreparedApplicationOperationCompletion,
    prepareApplicationOperationCompletion,
    preparedApplicationOperationCompletionOutcome,
    commitApplicationOperationCompletion,
    ApplicationWaitCancellation (..),
    PreparedApplicationWaitCancellation,
    prepareApplicationWaitCancellation,
    preparedApplicationWaitCancellationWaitId,
    preparedApplicationWaitCancellationReply,
    commitApplicationWaitCancellation,
    ApplicationWaitWake,
    applicationWaitWakeBinding,
    applicationWaitWakeCursor,
    applicationWaitWakeRequestId,
    applicationWaitWakeWaitId,
    applicationWaitWakeResult,
    PreparedApplicationWaitCompletions,
    prepareApplicationWaitCompletions,
    preparedApplicationWaitWakes,
    commitApplicationWaitCompletions,
    PreparedApplicationBootstrap,
    ApplicationBootstrapError (..),
    prepareApplicationBootstrap,
    prepareApplicationProcessRegistration,
    prepareApplicationPrimordialBootstrap,
    prepareApplicationChildBootstrap,
    PreparedApplicationProcessAlias,
    prepareApplicationProcessAlias,
    preparedApplicationProcessAlias,
    commitApplicationProcessAlias,
    commitApplicationBootstrap,
  )
where

import Control.Monad (foldM, unless)
import Data.ByteString (ByteString)
import Data.List (find, sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize qualified as Serialize
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word64)
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole,
    ApplicationStartupAccess,
    EnvironmentAccess,
    StartupAccessError,
    applicationStartupAccess,
    environmentAccess,
    predefinedAccess,
  )
import Eclips.Application.Types.Access qualified as ApplicationAccess
import Eclips.Application.Types.Identity
  ( PrivateDeltaId,
    PrivateNablaId,
    PrivateObjectId,
    PrivateProcessId,
    PrivateUniqueId,
    asPrivateDeltaId,
    asPrivateNablaId,
    asPrivateObjectId,
    asPrivateProcessId,
    privateObjectUniqueId,
    privateProcessUniqueId,
  )
import Eclips.Application.Types.Label
  ( ApplicationLabelTarget,
    LabelResult,
  )
import Eclips.Application.Types.Lifecycle (InitialClaimId, StartupError (..))
import Eclips.Application.Types.Lifetime
  ( ApplicationReceiptRetirement,
    emptyApplicationReceiptRetirement,
    ordinaryReceiptRetirement,
    ordinaryReceiptRetirementThrough,
  )
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Rejection (ApplicationRejection)
import Eclips.Application.Types.Result
  ( OperationPendingReason (..),
    RegularCallResult (..),
    WaitResult (..),
  )
import Eclips.Application.Types.Value qualified as Application
import Eclips.Domain.Disappearance (DisappearanceProbeId)
import Eclips.Domain.Environment
  ( EnvironmentRootClaimView (..),
    environmentRootClaimView,
    environmentRootSlotClaim,
  )
import Eclips.Domain.Graph (EdgePayload)
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    GlobalObjectId,
    GlobalUniqueId,
    HeraldEpoch,
    LabelDecisionId,
    NablaId,
    NablaSequencing,
    ProcessEpochId,
    SortDefinitionOccurrenceId,
    SortId,
    globalObjectIdFromDeltaId,
    globalObjectIdFromGlobalUniqueId,
    globalObjectIdFromNablaId,
    globalObjectIdFromProcessEpochId,
    globalUniqueIdFromGlobalObjectId,
    heraldEpochBytes,
    processEpochIdFromGlobalObjectId,
  )
import Eclips.Domain.Label
  ( LabelProcessAcceptancePosition,
    labelProcessAcceptancePositionOrdinal,
    labelProcessAcceptancePositionProcess,
  )
import Eclips.Domain.Route (ReplicaStrength)
import Eclips.Domain.Sort.Profile qualified as SortProfile
import Eclips.Domain.Startup (PredefinedSortRole)
import Eclips.Domain.Startup qualified as Startup
import Eclips.Domain.Value
  ( FieldNameError,
    Label,
    Value,
  )
import Eclips.Domain.Value qualified as Domain
import Eclips.Herald.Application.Environment
  ( CompletedEnvironment,
    PendingEnvironment,
  )
import Eclips.Herald.Application.Environment qualified as Environment
import Eclips.Herald.Application.Primordial qualified as Primordial
import Eclips.Herald.Application.PrivateIdentity
  ( PreparedPrivateUniqueIdAllocation,
    PrivateIdentity,
    PrivateIdentityError (..),
    ProcessIdentityWitness,
    commitLocalization,
    commitOracleProcessRetirement,
    commitPrivateUniqueIdAllocation,
    commitProcessRegistration,
    emptyPrivateIdentity,
    prepareLocalization,
    prepareOracleProcessRetirement,
    prepareProcessRegistration,
    processIdentityWitnesses,
    resolvePrivateUniqueId,
  )
import Eclips.Herald.Application.PublicationEvidence qualified as Publication
import Eclips.Herald.Application.Recovery.Internal
  ( ApplicationProcessRecoveryGeneration,
    ApplicationRecoveryConfiguration,
    ApplicationSessionRecoveryGeneration,
    applicationRecoveryGraceMicroseconds,
    firstApplicationProcessRecoveryGeneration,
    firstApplicationSessionRecoveryGeneration,
    nextApplicationProcessRecoveryGeneration,
    nextApplicationSessionRecoveryGeneration,
  )
import Eclips.Herald.Application.Request.Internal
  ( ApplicationReplyCursor,
    ApplicationRequestReply (..),
    ApplicationRequestStatus,
    ApplicationRequestSummary,
    ProcessAcceptancePosition,
    RequestId,
    RetainedRequestReplyBody (..),
    WaitId,
  )
import Eclips.Herald.Application.Request.Internal qualified as Request
import Eclips.Herald.Application.Session.Internal
  ( ApplicationAttachment,
    ApplicationResumeToken,
    ApplicationSessionAcceptance,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionRejection,
    ClientNonce,
  )
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
    preparedOutput,
  )
import Eclips.Herald.Publication.Groups qualified as PublicationGroups
import Eclips.Herald.Time
  ( MonotonicInstant,
    monotonicInstant,
    monotonicInstantWord64,
  )
import Eclips.Herald.Timer.Internal
  ( TimerAttempt,
    TimerAttemptGeneration,
    TimerOutcome (..),
    TimerSpec,
    absoluteTimerSpec,
    applicationSessionRecoveryTimerAttempt,
    firstTimerAttemptGeneration,
    nextTimerAttemptGeneration,
    timerAttemptApplicationSessionRecovery,
  )
import Eclips.Public.Types.ReceiptRetirement qualified as Retirement

-- | A typed root in the checked manifest's canonical localization order.
data ApplicationRoot
  = ApplicationWriterRoot PredefinedSortRole NablaId NablaSequencing
  | ApplicationReaderRoot PredefinedSortRole DeltaId
  deriving stock (Eq, Show)

-- | Application-visible typed handles retained for one root.
data RootAccess
  = WriterAccess PredefinedSortRole PrivateNablaId NablaSequencing
  | ReaderAccess PredefinedSortRole PrivateDeltaId
  deriving stock (Eq, Show)

-- | Stable process/root handles shared by every later session of one epoch.
data BootstrapAccess = BootstrapAccess
  { processEpoch :: ProcessEpochId,
    process :: PrivateProcessId,
    roots :: [RootAccess],
    primordial :: ApplicationAccess.PrimordialAccess
  }
  deriving stock (Eq, Show)

bootstrapAccessProcessEpoch :: BootstrapAccess -> ProcessEpochId
bootstrapAccessProcessEpoch access = access.processEpoch

bootstrapAccessProcess :: BootstrapAccess -> PrivateProcessId
bootstrapAccessProcess access = access.process

bootstrapAccessRoots :: BootstrapAccess -> [RootAccess]
bootstrapAccessRoots access = access.roots

bootstrapAccessPrimordial :: BootstrapAccess -> ApplicationAccess.PrimordialAccess
bootstrapAccessPrimordial access = access.primordial

bootstrapAccessEnvironmentSources :: BootstrapAccess -> Maybe (PrivateNablaId, PrivateNablaId)
bootstrapAccessEnvironmentSources access = do
  (nablaKey, deltaKey) <- ApplicationAccess.accessEnvironmentSources access.primordial
  ApplicationAccess.Writer nabla <- Map.lookup nablaKey (ApplicationAccess.accessEntries access.primordial)
  ApplicationAccess.Writer delta <- Map.lookup deltaKey (ApplicationAccess.accessEntries access.primordial)
  pure (nabla, delta)

-- | One live request identity in the process-wide label admission classes.
-- Candidate and gated requests deliberately live outside a 'SessionRecord':
-- they have no reply cursor, process position, status, or resume entry yet.
data ApplicationRequestKey
  = ApplicationRequestKey ApplicationSessionId RequestId
  deriving stock (Eq, Ord, Show)

applicationRequestKey ::
  ApplicationSessionId -> RequestId -> ApplicationRequestKey
applicationRequestKey = ApplicationRequestKey

applicationRequestKeySession :: ApplicationRequestKey -> ApplicationSessionId
applicationRequestKeySession (ApplicationRequestKey session _) = session

applicationRequestKeyRequest :: ApplicationRequestKey -> RequestId
applicationRequestKeyRequest (ApplicationRequestKey _ request) = request

-- | Stable semantic key for the one label Open intention owned by this EAPP
-- request.  Binding generations are deliberately absent: reconnect and exact
-- retry must continue to name the same OracleClient request.  The exact typed
-- call remains separately retained by Application for conflict detection.
applicationRequestKeyCanonicalBytes :: ApplicationRequestKey -> ByteString
applicationRequestKeyCanonicalBytes (ApplicationRequestKey session request) =
  Serialize.runPut $ do
    Serialize.putByteString "ECLIPS-APPLICATION-LABEL-REQUEST"
    Serialize.putByteString
      (heraldEpochBytes (Session.applicationSessionIdHeraldEpoch session))
    Serialize.putWord64be (Session.applicationSessionIdOrdinal session)
    Serialize.putWord64be (Request.requestIdWord64 request)

data ApplicationRequestClass
  = OrdinaryApplicationRequestClass
  | LabelCandidateApplicationRequestClass
  | GatedIngressApplicationRequestClass
  | FenceHeldApplicationRequestClass
  | EnvironmentCandidateApplicationRequestClass
  deriving stock (Eq, Ord, Show)

-- | Checked private representation of the live label arm.  The surrounding
-- EAPP value remains the shared 'ApplicationOperation'; this nominal view only
-- makes it impossible for candidate and active-work owners to retain a
-- non-label operation by construction.
data LabelApplication
  = CheckedLabelApplication
      PrivateObjectId
      Application.ApplicationLabel
      ApplicationLabelTarget
  deriving stock (Eq, Show)

labelApplication ::
  PrivateObjectId ->
  Application.ApplicationLabel ->
  ApplicationLabelTarget ->
  LabelApplication
labelApplication = CheckedLabelApplication

labelApplicationOperation :: LabelApplication -> ApplicationOperation
labelApplicationOperation (CheckedLabelApplication object expected target) =
  LabelApplication object expected target

applicationOperationLabel :: ApplicationOperation -> Maybe LabelApplication
applicationOperationLabel operation = case operation of
  LabelApplication object expected target ->
    Just (CheckedLabelApplication object expected target)
  _ -> Nothing

labelApplicationObject :: LabelApplication -> PrivateObjectId
labelApplicationObject (CheckedLabelApplication object _ _) = object

labelApplicationExpected :: LabelApplication -> Application.ApplicationLabel
labelApplicationExpected (CheckedLabelApplication _ expected _) = expected

labelApplicationTarget :: LabelApplication -> ApplicationLabelTarget
labelApplicationTarget (CheckedLabelApplication _ _ target) = target

-- | A label selects exactly one caller/object publication group. The immutable
-- publication's admitted membership supplies the writer association; accepting
-- a label does not enumerate or freeze a catalogue of related writers.
newtype SequencingSelection
  = SequencingSelection PublicationGroups.GroupKey
  deriving stock (Eq, Show)

sequencingSelection ::
  ProcessEpochId -> GlobalObjectId -> SequencingSelection
sequencingSelection process target =
  SequencingSelection (PublicationGroups.groupKey process target)

sequencingSelectionTarget :: SequencingSelection -> GlobalObjectId
sequencingSelectionTarget = PublicationGroups.groupObject . sequencingSelectionGroup

sequencingSelectionGroup :: SequencingSelection -> PublicationGroups.GroupKey
sequencingSelectionGroup (SequencingSelection group) = group

newtype ApplicationGateGeneration = ApplicationGateGeneration Word64
  deriving stock (Eq, Ord, Show)

applicationGateGenerationWord64 :: ApplicationGateGeneration -> Word64
applicationGateGenerationWord64 (ApplicationGateGeneration generation) = generation

data ApplicationGatePhase
  = ApplicationGateOpen ApplicationGateGeneration
  | ApplicationGateClosed ApplicationGateGeneration
  deriving stock (Eq, Show)

data LabelCandidateRecord
  = LabelCandidateRecord ProcessEpochId LabelApplication
  deriving stock (Eq)

data IngressGate
  = WholeApplicationIngressGate ApplicationGateGeneration
  | EndpointIngressGate LabelDecisionId
  deriving stock (Eq, Show)

data GatedIngressRecord
  = GatedIngressRecord
      ProcessEpochId
      ApplicationOperation
      IngressGate
  deriving stock (Eq)

-- | Private identifiers are deliberately absent from positioned held work.
-- The source sort coordinate, admitted value, origin, and control prerequisite
-- are the complete acceptance-time semantic payload; publication identity,
-- authority, route, and visibility are derived only when the label fence
-- releases and the work is revalidated.  The retained coordinate prevents
-- release into another sort epoch, while the retained prerequisite is an
-- immutable lower bound on the released publication's rederived prerequisite.
-- The already classified caller/object groups identify which local label can
-- hold this work, without replaying its admission merely to test that question.
data ApplicationFenceHeldWork
  = ApplicationFenceHeldWork
      NablaId
      SortId
      SortDefinitionOccurrenceId
      Publication.AdmittedApplicationPublicationValue
      Publication.ApplicationPublicationOrigin
      ReplicaStrength
      ControlIndex
      PublicationGroups.Membership
  deriving stock (Eq, Show)

applicationFenceHeldWork ::
  NablaId ->
  SortId ->
  SortDefinitionOccurrenceId ->
  Publication.AdmittedApplicationPublicationValue ->
  Publication.ApplicationPublicationOrigin ->
  ReplicaStrength ->
  ControlIndex ->
  PublicationGroups.Membership ->
  ApplicationFenceHeldWork
applicationFenceHeldWork = ApplicationFenceHeldWork

applicationFenceHeldWorkWriter :: ApplicationFenceHeldWork -> NablaId
applicationFenceHeldWorkWriter
  (ApplicationFenceHeldWork writer _ _ _ _ _ _ _) = writer

applicationFenceHeldWorkSortId :: ApplicationFenceHeldWork -> SortId
applicationFenceHeldWorkSortId
  (ApplicationFenceHeldWork _ sortId _ _ _ _ _ _) = sortId

applicationFenceHeldWorkSortOccurrenceId ::
  ApplicationFenceHeldWork -> SortDefinitionOccurrenceId
applicationFenceHeldWorkSortOccurrenceId
  (ApplicationFenceHeldWork _ _ occurrence _ _ _ _ _) = occurrence

applicationFenceHeldWorkAdmittedValue ::
  ApplicationFenceHeldWork -> Publication.AdmittedApplicationPublicationValue
applicationFenceHeldWorkAdmittedValue
  (ApplicationFenceHeldWork _ _ _ admitted _ _ _ _) = admitted

applicationFenceHeldWorkOrigin ::
  ApplicationFenceHeldWork -> Publication.ApplicationPublicationOrigin
applicationFenceHeldWorkOrigin
  (ApplicationFenceHeldWork _ _ _ _ origin _ _ _) = origin

applicationFenceHeldWorkSourceStrength ::
  ApplicationFenceHeldWork -> ReplicaStrength
applicationFenceHeldWorkSourceStrength
  (ApplicationFenceHeldWork _ _ _ _ _ strength _ _) = strength

applicationFenceHeldWorkControlPrerequisite ::
  ApplicationFenceHeldWork -> ControlIndex
applicationFenceHeldWorkControlPrerequisite
  (ApplicationFenceHeldWork _ _ _ _ _ _ prerequisite _) = prerequisite

applicationFenceHeldWorkGroupMembership ::
  ApplicationFenceHeldWork -> PublicationGroups.Membership
applicationFenceHeldWorkGroupMembership
  (ApplicationFenceHeldWork _ _ _ _ _ _ _ membership) = membership

-- | Zero-payload evidence that the immutable request provenance still names a
-- live reply owner. Binding, wait, cursor, status, and body remain solely in
-- the ordinary SessionRecord owner; session/process retirement erases this
-- marker without deleting positioned semantic work.
data FenceHeldReplyLink = FenceHeldReplyLink
  deriving stock (Eq)

data FenceHeldRecord
  = FenceHeldRecord
      LabelProcessAcceptancePosition
      ApplicationOperation
      ApplicationFenceHeldWork
      ApplicationRequestKey
      (Maybe FenceHeldReplyLink)
      (Set DisappearanceProbeId)
  deriving stock (Eq)

data ActiveLabelApplication
  = ActiveLabelApplication
      LabelDecisionId
      ApplicationRequestKey
      LabelApplication
      SequencingSelection
      LabelProcessAcceptancePosition
      (Maybe LabelResult)
  deriving stock (Eq)

data ApplicationLabelCandidateView
  = ApplicationLabelCandidateView
      ApplicationRequestKey
      ProcessEpochId
      LabelApplication
  deriving stock (Eq, Show)

applicationLabelCandidateKey ::
  ApplicationLabelCandidateView -> ApplicationRequestKey
applicationLabelCandidateKey (ApplicationLabelCandidateView key _ _) = key

applicationLabelCandidateProcess ::
  ApplicationLabelCandidateView -> ProcessEpochId
applicationLabelCandidateProcess (ApplicationLabelCandidateView _ process _) = process

applicationLabelCandidateCall ::
  ApplicationLabelCandidateView -> LabelApplication
applicationLabelCandidateCall (ApplicationLabelCandidateView _ _ call) = call

data ApplicationGatedIngressView
  = ApplicationGatedIngressView
      ApplicationRequestKey
      ProcessEpochId
      ApplicationOperation
      IngressGate
  deriving stock (Eq, Show)

applicationGatedIngressKey ::
  ApplicationGatedIngressView -> ApplicationRequestKey
applicationGatedIngressKey (ApplicationGatedIngressView key _ _ _) = key

applicationGatedIngressProcess ::
  ApplicationGatedIngressView -> ProcessEpochId
applicationGatedIngressProcess (ApplicationGatedIngressView _ process _ _) = process

applicationGatedIngressOperation ::
  ApplicationGatedIngressView -> ApplicationOperation
applicationGatedIngressOperation (ApplicationGatedIngressView _ _ operation _) = operation

applicationGatedIngressGeneration ::
  ApplicationGatedIngressView -> Maybe ApplicationGateGeneration
applicationGatedIngressGeneration (ApplicationGatedIngressView _ _ _ gate) = case gate of
  WholeApplicationIngressGate generation -> Just generation
  EndpointIngressGate _ -> Nothing

applicationGatedIngressReady :: ApplicationGatedIngressView -> State -> Bool
applicationGatedIngressReady (ApplicationGatedIngressView _ _ _ gate) state =
  ingressGateReleased gate state

-- | First endpoint hold for singleton diagnostic fixtures.
applicationEndpointAdmissionHold :: State -> Maybe (LabelDecisionId, NablaId)
applicationEndpointAdmissionHold state = Map.lookupMin state.endpointAdmissionHolds

applicationEndpointAdmissionHoldFor :: NablaId -> State -> Maybe LabelDecisionId
applicationEndpointAdmissionHoldFor nabla state = do
  decision <- Map.lookup (globalObjectIdFromNablaId nabla) state.labelByObject
  retained <- Map.lookup decision state.endpointAdmissionHolds
  if retained == nabla then Just decision else Nothing

data ApplicationFenceHeldView
  = ApplicationFenceHeldView
      LabelProcessAcceptancePosition
      ApplicationOperation
      ApplicationFenceHeldWork
      (Maybe ApplicationRequestKey)
      (Set DisappearanceProbeId)
  deriving stock (Eq, Show)

applicationFenceHeldPosition ::
  ApplicationFenceHeldView -> LabelProcessAcceptancePosition
applicationFenceHeldPosition (ApplicationFenceHeldView position _ _ _ _) = position

applicationFenceHeldOperation ::
  ApplicationFenceHeldView -> ApplicationOperation
applicationFenceHeldOperation (ApplicationFenceHeldView _ operation _ _ _) = operation

applicationFenceHeldSemanticWork ::
  ApplicationFenceHeldView -> ApplicationFenceHeldWork
applicationFenceHeldSemanticWork (ApplicationFenceHeldView _ _ work _ _) = work

applicationFenceHeldReplyKey ::
  ApplicationFenceHeldView -> Maybe ApplicationRequestKey
applicationFenceHeldReplyKey (ApplicationFenceHeldView _ _ _ key _) = key

applicationFenceHeldDisappearanceProbes :: ApplicationFenceHeldView -> Set DisappearanceProbeId
applicationFenceHeldDisappearanceProbes (ApplicationFenceHeldView _ _ _ _ probes) = probes

data ActiveLabelApplicationView
  = ActiveLabelApplicationView
      LabelDecisionId
      ApplicationRequestKey
      LabelApplication
      SequencingSelection
      LabelProcessAcceptancePosition
      (Maybe LabelResult)
  deriving stock (Eq, Show)

activeLabelApplicationDecision :: ActiveLabelApplicationView -> LabelDecisionId
activeLabelApplicationDecision (ActiveLabelApplicationView decision _ _ _ _ _) = decision

activeLabelApplicationKey ::
  ActiveLabelApplicationView -> ApplicationRequestKey
activeLabelApplicationKey (ActiveLabelApplicationView _ key _ _ _ _) = key

activeLabelApplicationCall :: ActiveLabelApplicationView -> LabelApplication
activeLabelApplicationCall (ActiveLabelApplicationView _ _ call _ _ _) = call

activeLabelApplicationSelection ::
  ActiveLabelApplicationView -> SequencingSelection
activeLabelApplicationSelection (ActiveLabelApplicationView _ _ _ selection _ _) = selection

activeLabelApplicationPosition ::
  ActiveLabelApplicationView -> LabelProcessAcceptancePosition
activeLabelApplicationPosition (ActiveLabelApplicationView _ _ _ _ position _) = position

activeLabelApplicationTerminalResult ::
  ActiveLabelApplicationView -> Maybe LabelResult
activeLabelApplicationTerminalResult (ActiveLabelApplicationView _ _ _ _ _ result) = result

-- | One fixed-deadline recovery episode for a detached application session.
-- The logical generation is never reused; only an early physical timer firing
-- advances the attempt generation against the same absolute deadline.
data SessionRecovery = SessionRecovery
  { generation :: ApplicationSessionRecoveryGeneration,
    deadline :: MonotonicInstant,
    timerAttempt :: TimerAttemptGeneration
  }
  deriving stock (Eq)

data ApplicationProcessRecoveryPhase
  = ApplicationProcessAvailable
  | ApplicationProcessRecovering ApplicationProcessRecoveryGeneration
  | ApplicationProcessSealed ApplicationProcessRecoveryGeneration
  deriving stock (Eq)

data ApplicationProcessRecovery = ApplicationProcessRecovery
  { lastGeneration :: Maybe ApplicationProcessRecoveryGeneration,
    phase :: ApplicationProcessRecoveryPhase
  }
  deriving stock (Eq)

-- | Initial claims retain their winner separately from the ordinary session
-- disposition, which Resume may advance. The first lost binding fixes the
-- claim recovery deadline; subsequent claim retries cannot renew it.
data ApplicationInitialClaimStatus
  = ApplicationInitialClaimUnclaimed (Set InitialClaimId)
  | ApplicationInitialClaimClaimed InitialClaimId ApplicationSessionAcceptance (Maybe MonotonicInstant) Bool
  deriving stock (Eq, Show)

data InitialClaimRecord = InitialClaimRecord ProcessEpochId ApplicationInitialClaimStatus
  deriving stock (Eq)

-- | All application bootstrap state owned by this Herald.
data State = State
  { privateIdentity :: PrivateIdentity,
    accessByProcess :: (Map ProcessEpochId BootstrapAccess),
    processByAttachment :: (Map ApplicationAttachment ProcessEpochId),
    sessionsById :: (Map ApplicationSessionId SessionRecord),
    preparationSessionChanges :: !(Set ApplicationSessionId),
    openSessionByNonce :: (Map (ProcessEpochId, ClientNonce) ApplicationSessionId),
    processRecoveryByProcess :: Map ProcessEpochId ApplicationProcessRecovery,
    initialClaimsByAttachment :: Map ApplicationAttachment InitialClaimRecord,
    nextSessionOrdinal :: Word64,
    nextAcceptanceByProcess :: (Map ProcessEpochId Word64),
    labelCandidates :: Map ApplicationRequestKey LabelCandidateRecord,
    gatedIngress :: Map ApplicationRequestKey GatedIngressRecord,
    acceptedFenceHeld ::
      Map LabelProcessAcceptancePosition FenceHeldRecord,
    pendingEnvironments ::
      Map ProcessAcceptancePosition PendingEnvironment,
    completedEnvironments ::
      Map ProcessAcceptancePosition CompletedEnvironment,
    applicationGate :: ApplicationGatePhase,
    activeLabels :: !(Map LabelDecisionId ActiveLabelApplication),
    labelByObject :: !(Map GlobalObjectId LabelDecisionId),
    endpointAdmissionHolds :: !(Map LabelDecisionId NablaId)
  }
  deriving stock (Eq)

emptyState :: State
emptyState =
  State
    { privateIdentity = emptyPrivateIdentity,
      accessByProcess = Map.empty,
      processByAttachment = Map.empty,
      sessionsById = Map.empty,
      preparationSessionChanges = Set.empty,
      openSessionByNonce = Map.empty,
      processRecoveryByProcess = Map.empty,
      initialClaimsByAttachment = Map.empty,
      nextSessionOrdinal = 1,
      nextAcceptanceByProcess = Map.empty,
      labelCandidates = Map.empty,
      gatedIngress = Map.empty,
      acceptedFenceHeld = Map.empty,
      pendingEnvironments = Map.empty,
      completedEnvironments = Map.empty,
      applicationGate = ApplicationGateOpen (ApplicationGateGeneration 1),
      activeLabels = Map.empty,
      labelByObject = Map.empty,
      endpointAdmissionHolds = Map.empty
    }

-- | Exact session removals release lifecycle observers and retry entitlement.
-- Ordinary request/acknowledgement updates do not wake preparation work.
takePreparationSessionChanges :: State -> (Set ApplicationSessionId, State)
takePreparationSessionChanges state =
  let changes = state.preparationSessionChanges
   in changes `seq` (changes, clearPreparationSessionChanges state)

clearPreparationSessionChanges :: State -> State
clearPreparationSessionChanges state
  | Set.null state.preparationSessionChanges = state
  | otherwise = state {preparationSessionChanges = Set.empty}

applicationGatePhase :: State -> ApplicationGatePhase
applicationGatePhase state = state.applicationGate

applicationLabelCandidateEntries :: State -> [ApplicationLabelCandidateView]
applicationLabelCandidateEntries state =
  [ ApplicationLabelCandidateView key process call
  | (key, LabelCandidateRecord process call) <-
      Map.toAscList state.labelCandidates
  ]

applicationGatedIngressEntries :: State -> [ApplicationGatedIngressView]
applicationGatedIngressEntries state =
  [ ApplicationGatedIngressView key process operation generation
  | (key, GatedIngressRecord process operation generation) <-
      Map.toAscList state.gatedIngress
  ]

applicationFenceHeldEntries :: State -> [ApplicationFenceHeldView]
applicationFenceHeldEntries state =
  [ ApplicationFenceHeldView
      position
      operation
      work
      (key <$ replyLink)
      probes
  | (position, FenceHeldRecord _ operation work key replyLink probes) <-
      Map.toAscList state.acceptedFenceHeld
  ]

-- | First active invocation for diagnostics and singleton test fixtures only.
-- Production transitions use the decision/object/group lookups below.
applicationActiveLabel :: State -> Maybe ActiveLabelApplicationView
applicationActiveLabel state = activeLabelView . snd <$> Map.lookupMin state.activeLabels

applicationHasActiveLabels :: State -> Bool
applicationHasActiveLabels = not . Map.null . (.activeLabels)

applicationActiveLabels :: State -> [ActiveLabelApplicationView]
applicationActiveLabels state = map activeLabelView (Map.elems state.activeLabels)

applicationActiveLabelForDecision :: LabelDecisionId -> State -> Maybe ActiveLabelApplicationView
applicationActiveLabelForDecision decision state = activeLabelView <$> Map.lookup decision state.activeLabels

applicationActiveLabelForObject :: GlobalObjectId -> State -> Maybe ActiveLabelApplicationView
applicationActiveLabelForObject object state = do
  decision <- Map.lookup object state.labelByObject
  applicationActiveLabelForDecision decision state

applicationActiveLabelForGroup :: PublicationGroups.GroupKey -> State -> Maybe ActiveLabelApplicationView
applicationActiveLabelForGroup group state = do
  active <- applicationActiveLabelForObject (PublicationGroups.groupObject group) state
  if sequencingSelectionGroup (activeLabelApplicationSelection active) == group
    then Just active
    else Nothing

-- | Exhaustive debug/test check; ordinary admission uses indexed lookups.
applicationLabelIndexesValid :: State -> Bool
applicationLabelIndexesValid state =
  state.labelByObject == expectedObjects
    && Map.size state.labelByObject == Map.size state.activeLabels
    && all validEndpoint (Map.toList state.endpointAdmissionHolds)
  where
    expectedObjects =
      Map.fromList
        [ (sequencingSelectionTarget selection, decision)
        | (decision, ActiveLabelApplication _ _ _ selection _ _) <- Map.toList state.activeLabels
        ]
    validEndpoint (decision, nabla) = case Map.lookup decision state.activeLabels of
      Just (ActiveLabelApplication retained _ _ selection _ Nothing) ->
        retained == decision && sequencingSelectionTarget selection == globalObjectIdFromNablaId nabla
      _ -> False

activeLabelView :: ActiveLabelApplication -> ActiveLabelApplicationView
activeLabelView (ActiveLabelApplication decision key call selection position terminal) =
  ActiveLabelApplicationView decision key call selection position terminal

applicationBootstrapAccess :: ProcessEpochId -> State -> Maybe BootstrapAccess
applicationBootstrapAccess process state = Map.lookup process state.accessByProcess

-- | Resolve one admitted primordial attachment to its resident process epoch.
applicationAttachmentProcess ::
  ApplicationAttachment ->
  State ->
  Maybe ProcessEpochId
applicationAttachmentProcess attachment state =
  Map.lookup attachment state.processByAttachment

-- | Narrow read view used by later application transitions and whole-state
-- validation.  It does not permit replacement of the owned registry.
applicationPrivateIdentity :: State -> PrivateIdentity
applicationPrivateIdentity state = state.privateIdentity

applicationProcessWitnesses :: State -> [ProcessIdentityWitness]
applicationProcessWitnesses state = processIdentityWitnesses state.privateIdentity

-- | Read-only facts retained for one request.  The witness is an observation for
-- validation and properties, never a way to replace owner state.
data ApplicationRequestWitness = ApplicationRequestWitness
  { applicationRequestWitnessId :: RequestId,
    applicationRequestWitnessCall :: ApplicationOperation,
    applicationRequestWitnessProcess :: ProcessEpochId,
    applicationRequestWitnessPosition :: Maybe ProcessAcceptancePosition,
    applicationRequestWitnessCursor :: ApplicationReplyCursor,
    applicationRequestWitnessStatus :: ApplicationRequestStatus
  }
  deriving stock (Eq, Show)

-- | The exact private application-level call retained for the package-internal
-- generic publication seam.  It deliberately remains outside the live
-- 'ApplicationOperation' union until the sole write schema is exposed.
data ApplicationPublicationCall
  = ApplicationPublicationCall PrivateNablaId Application.ApplicationValue
  deriving stock (Eq, Show)

applicationPublicationCall ::
  PrivateNablaId ->
  Application.ApplicationValue ->
  ApplicationPublicationCall
applicationPublicationCall = ApplicationPublicationCall

-- | Read-only evidence for one accepted package-internal publication request.
-- The containing session remains the request owner; ending it removes this
-- record without affecting the self-contained semantic publication.
data ApplicationPublicationRequestWitness
  = ApplicationPublicationRequestWitness
      RequestId
      PrivateNablaId
      Application.ApplicationValue
      ProcessEpochId
      ProcessAcceptancePosition
  deriving stock (Eq, Show)

applicationPublicationRequestWitnessId ::
  ApplicationPublicationRequestWitness -> RequestId
applicationPublicationRequestWitnessId
  (ApplicationPublicationRequestWitness request _ _ _ _) = request

applicationPublicationRequestWitnessPrivateNabla ::
  ApplicationPublicationRequestWitness -> PrivateNablaId
applicationPublicationRequestWitnessPrivateNabla
  (ApplicationPublicationRequestWitness _ nabla _ _ _) = nabla

applicationPublicationRequestWitnessValue ::
  ApplicationPublicationRequestWitness -> Application.ApplicationValue
applicationPublicationRequestWitnessValue
  (ApplicationPublicationRequestWitness _ _ value _ _) = value

applicationPublicationRequestWitnessProcess ::
  ApplicationPublicationRequestWitness -> ProcessEpochId
applicationPublicationRequestWitnessProcess
  (ApplicationPublicationRequestWitness _ _ _ process _) = process

applicationPublicationRequestWitnessPosition ::
  ApplicationPublicationRequestWitness -> ProcessAcceptancePosition
applicationPublicationRequestWitnessPosition
  (ApplicationPublicationRequestWitness _ _ _ _ position) = position

-- | Nullary package-private input for the direct semantic-owner test seam.
-- The live EAPP coordinator uses 'NewEnvironmentApplication'; this value keeps
-- lower-level ownership properties independent of RPC classification.
data NewEnvironmentCoordinatorInput = NewEnvironmentCoordinatorInput
  deriving stock (Eq, Ord, Show)

newEnvironmentCoordinatorInput :: NewEnvironmentCoordinatorInput
newEnvironmentCoordinatorInput = NewEnvironmentCoordinatorInput

-- | Whether one live session request has merely retained its intention or has
-- consumed a process acceptance position and installed global retained work.
data EnvironmentRequestStage
  = EnvironmentRequestCandidateStage
  | EnvironmentRequestAcceptedStage ProcessAcceptancePosition
  | EnvironmentRequestCompletedStage ProcessAcceptancePosition
  deriving stock (Eq, Show)

data EnvironmentRequestWitness
  = EnvironmentRequestWitness
      RequestId
      ProcessEpochId
      EnvironmentRequestStage
  deriving stock (Eq, Show)

environmentRequestWitnessId :: EnvironmentRequestWitness -> RequestId
environmentRequestWitnessId (EnvironmentRequestWitness request _ _) = request

environmentRequestWitnessProcess ::
  EnvironmentRequestWitness -> ProcessEpochId
environmentRequestWitnessProcess (EnvironmentRequestWitness _ process _) = process

environmentRequestWitnessStage ::
  EnvironmentRequestWitness -> EnvironmentRequestStage
environmentRequestWitnessStage (EnvironmentRequestWitness _ _ stage) = stage

-- | Read-only facts retained for one live application session.
data ApplicationSessionWitness = ApplicationSessionWitness
  { applicationSessionWitnessId :: ApplicationSessionId,
    applicationSessionWitnessProcess :: ProcessEpochId,
    applicationSessionWitnessAttachment :: ApplicationAttachment,
    applicationSessionWitnessNonce :: ClientNonce,
    applicationSessionWitnessResumeToken :: ApplicationResumeToken,
    applicationSessionWitnessBinding :: ApplicationSessionBinding,
    applicationSessionWitnessDeliveryLive :: Bool,
    applicationSessionWitnessStartupAccess :: ApplicationStartupAccess
  }
  deriving stock (Eq, Show)

-- | Read-only exact recovery evidence for one detached session.
data ApplicationSessionRecoveryWitness = ApplicationSessionRecoveryWitness
  { session :: ApplicationSessionId,
    generation :: ApplicationSessionRecoveryGeneration,
    deadline :: MonotonicInstant,
    timer :: TimerAttempt
  }
  deriving stock (Eq, Show)

data ApplicationProcessRecoveryStatusWitness
  = ApplicationProcessAvailableWitness
  | ApplicationProcessRecoveringWitness ApplicationProcessRecoveryGeneration
  | ApplicationProcessSealedWitness ApplicationProcessRecoveryGeneration
  deriving stock (Eq, Show)

data ApplicationProcessRecoveryWitness = ApplicationProcessRecoveryWitness
  { process :: ProcessEpochId,
    lastGeneration :: Maybe ApplicationProcessRecoveryGeneration,
    status :: ApplicationProcessRecoveryStatusWitness
  }
  deriving stock (Eq, Show)

applicationSessionRecoveryEntries :: State -> [ApplicationSessionRecoveryWitness]
applicationSessionRecoveryEntries state =
  [ ApplicationSessionRecoveryWitness
      { session,
        generation = recovery.generation,
        deadline = recovery.deadline,
        timer =
          applicationSessionRecoveryTimerAttempt
            session
            recovery.generation
            recovery.timerAttempt
      }
  | (session, record) <- Map.toAscList state.sessionsById,
    recovery <- maybe [] pure record.recovery
  ]

applicationProcessRecoveryEntries :: State -> [ApplicationProcessRecoveryWitness]
applicationProcessRecoveryEntries state =
  [ ApplicationProcessRecoveryWitness
      { process,
        lastGeneration = recovery.lastGeneration,
        status = case recovery.phase of
          ApplicationProcessAvailable -> ApplicationProcessAvailableWitness
          ApplicationProcessRecovering generation ->
            ApplicationProcessRecoveringWitness generation
          ApplicationProcessSealed generation ->
            ApplicationProcessSealedWitness generation
      }
  | (process, recovery) <- Map.toAscList state.processRecoveryByProcess
  ]

-- | Request/cursor evidence kept separate from the established Step-4 session
-- witness so downstream session validation remains source compatible.
data ApplicationRequestOwnerWitness = ApplicationRequestOwnerWitness
  { applicationRequestOwnerSession :: ApplicationSessionId,
    applicationRequestOwnerBindingEstablishedBy :: ApplicationReplyCursor,
    applicationRequestOwnerLastIssuedReplyCursor :: ApplicationReplyCursor,
    applicationRequestOwnerRetiredReplyCursor :: Maybe ApplicationReplyCursor,
    applicationRequestOwnerRequests :: [ApplicationRequestWitness]
  }
  deriving stock (Eq, Show)

data ApplicationRequestStateWitness = ApplicationRequestStateWitness
  { applicationRequestOwnerWitnesses :: [ApplicationRequestOwnerWitness],
    applicationRequestNextAcceptancePositions :: [(ProcessEpochId, Word64)]
  }
  deriving stock (Eq, Show)

-- | Read-only attachment, session, retry-index, and allocation evidence.
--
-- This value cannot replace any owner state; it exists for composed invariant
-- validation and focused properties.
data ApplicationStateWitness = ApplicationStateWitness
  { applicationWitnessAttachments :: [(ApplicationAttachment, ProcessEpochId)],
    applicationWitnessSessions :: [ApplicationSessionWitness],
    applicationWitnessOpenRetryIndex ::
      [((ProcessEpochId, ClientNonce), ApplicationSessionId)],
    applicationWitnessNextSessionOrdinal :: Word64
  }
  deriving stock (Eq, Show)

applicationWitnessAttachments ::
  ApplicationStateWitness ->
  [(ApplicationAttachment, ProcessEpochId)]
applicationWitnessAttachments witness = witness.applicationWitnessAttachments

applicationWitnessSessions ::
  ApplicationStateWitness ->
  [ApplicationSessionWitness]
applicationWitnessSessions witness = witness.applicationWitnessSessions

applicationWitnessOpenRetryIndex ::
  ApplicationStateWitness ->
  [((ProcessEpochId, ClientNonce), ApplicationSessionId)]
applicationWitnessOpenRetryIndex witness = witness.applicationWitnessOpenRetryIndex

applicationWitnessNextSessionOrdinal :: ApplicationStateWitness -> Word64
applicationWitnessNextSessionOrdinal witness = witness.applicationWitnessNextSessionOrdinal

applicationSessionWitnessId :: ApplicationSessionWitness -> ApplicationSessionId
applicationSessionWitnessId witness = witness.applicationSessionWitnessId

applicationSessionWitnessProcess :: ApplicationSessionWitness -> ProcessEpochId
applicationSessionWitnessProcess witness = witness.applicationSessionWitnessProcess

applicationSessionWitnessAttachment ::
  ApplicationSessionWitness ->
  ApplicationAttachment
applicationSessionWitnessAttachment witness = witness.applicationSessionWitnessAttachment

applicationSessionWitnessNonce :: ApplicationSessionWitness -> ClientNonce
applicationSessionWitnessNonce witness = witness.applicationSessionWitnessNonce

applicationSessionWitnessResumeToken ::
  ApplicationSessionWitness ->
  ApplicationResumeToken
applicationSessionWitnessResumeToken witness = witness.applicationSessionWitnessResumeToken

applicationSessionWitnessBinding ::
  ApplicationSessionWitness ->
  ApplicationSessionBinding
applicationSessionWitnessBinding witness = witness.applicationSessionWitnessBinding

applicationSessionWitnessDeliveryLive :: ApplicationSessionWitness -> Bool
applicationSessionWitnessDeliveryLive witness =
  witness.applicationSessionWitnessDeliveryLive

applicationSessionWitnessStartupAccess ::
  ApplicationSessionWitness ->
  ApplicationStartupAccess
applicationSessionWitnessStartupAccess witness =
  witness.applicationSessionWitnessStartupAccess

applicationRequestOwnerWitnesses ::
  ApplicationRequestStateWitness ->
  [ApplicationRequestOwnerWitness]
applicationRequestOwnerWitnesses witness = witness.applicationRequestOwnerWitnesses

applicationRequestNextAcceptancePositions ::
  ApplicationRequestStateWitness ->
  [(ProcessEpochId, Word64)]
applicationRequestNextAcceptancePositions witness =
  witness.applicationRequestNextAcceptancePositions

applicationRequestOwnerSession ::
  ApplicationRequestOwnerWitness ->
  ApplicationSessionId
applicationRequestOwnerSession witness = witness.applicationRequestOwnerSession

applicationRequestOwnerBindingEstablishedBy ::
  ApplicationRequestOwnerWitness ->
  ApplicationReplyCursor
applicationRequestOwnerBindingEstablishedBy witness =
  witness.applicationRequestOwnerBindingEstablishedBy

applicationRequestOwnerLastIssuedReplyCursor ::
  ApplicationRequestOwnerWitness ->
  ApplicationReplyCursor
applicationRequestOwnerLastIssuedReplyCursor witness =
  witness.applicationRequestOwnerLastIssuedReplyCursor

applicationRequestOwnerRetiredReplyCursor ::
  ApplicationRequestOwnerWitness -> Maybe ApplicationReplyCursor
applicationRequestOwnerRetiredReplyCursor witness =
  witness.applicationRequestOwnerRetiredReplyCursor

applicationRequestOwnerRequests ::
  ApplicationRequestOwnerWitness ->
  [ApplicationRequestWitness]
applicationRequestOwnerRequests witness = witness.applicationRequestOwnerRequests

applicationRequestWitnessId :: ApplicationRequestWitness -> RequestId
applicationRequestWitnessId witness = witness.applicationRequestWitnessId

applicationRequestWitnessCall ::
  ApplicationRequestWitness ->
  ApplicationOperation
applicationRequestWitnessCall witness = witness.applicationRequestWitnessCall

applicationRequestWitnessProcess ::
  ApplicationRequestWitness ->
  ProcessEpochId
applicationRequestWitnessProcess witness = witness.applicationRequestWitnessProcess

applicationRequestWitnessPosition ::
  ApplicationRequestWitness ->
  Maybe ProcessAcceptancePosition
applicationRequestWitnessPosition witness = witness.applicationRequestWitnessPosition

applicationRequestWitnessCursor ::
  ApplicationRequestWitness ->
  ApplicationReplyCursor
applicationRequestWitnessCursor witness = witness.applicationRequestWitnessCursor

applicationRequestWitnessStatus ::
  ApplicationRequestWitness ->
  ApplicationRequestStatus
applicationRequestWitnessStatus witness = witness.applicationRequestWitnessStatus

applicationStateWitness :: State -> ApplicationStateWitness
applicationStateWitness state =
  ApplicationStateWitness
    { applicationWitnessAttachments = Map.toAscList state.processByAttachment,
      applicationWitnessSessions =
        fmap sessionWitness (Map.elems state.sessionsById),
      applicationWitnessOpenRetryIndex =
        Map.toAscList state.openSessionByNonce,
      applicationWitnessNextSessionOrdinal = state.nextSessionOrdinal
    }

applicationRequestStateWitness :: State -> ApplicationRequestStateWitness
applicationRequestStateWitness state =
  ApplicationRequestStateWitness
    { applicationRequestOwnerWitnesses =
        fmap requestOwnerWitness (Map.elems state.sessionsById),
      applicationRequestNextAcceptancePositions =
        Map.toAscList state.nextAcceptanceByProcess
    }

-- | Key-preserving request-owner evidence for whole-product validation.
--
-- The ordinary witness remains source-compatible and convenient for callers
-- that do not need to distinguish a map key from the identity retained inside
-- its value.
applicationRequestOwnerEntries ::
  State ->
  [(ApplicationSessionId, ApplicationRequestOwnerWitness)]
applicationRequestOwnerEntries state =
  [ (session, requestOwnerWitness record)
  | (session, record) <- Map.toAscList state.sessionsById
  ]

-- | Key-preserving request evidence for one live session.
applicationRequestEntries ::
  ApplicationSessionId ->
  State ->
  [(RequestId, ApplicationRequestWitness)]
applicationRequestEntries session state =
  maybe
    []
    (fmap (\(request, record) -> (request, requestWitness record)) . Map.toAscList . (.requests))
    (Map.lookup session state.sessionsById)

-- | Key-preserving accepted generic-publication requests for one live session.
-- This is a separate package-internal witness so the established live request
-- witness and protocol vocabulary remain unchanged.
applicationPublicationRequestEntries ::
  ApplicationSessionId ->
  State ->
  [(RequestId, ApplicationPublicationRequestWitness)]
applicationPublicationRequestEntries session state =
  maybe
    []
    ( fmap
        (\(request, record) -> (request, publicationRequestWitness record))
        . Map.toAscList
        . (.publicationRequests)
    )
    (Map.lookup session state.sessionsById)

-- | Key-preserving environment requests for one live session. Deferred
-- candidates live in the private candidate map; accepted and completed calls
-- are projected from the ordinary retained EAPP request owner.
applicationEnvironmentRequestEntries ::
  ApplicationSessionId ->
  State ->
  [(RequestId, EnvironmentRequestWitness)]
applicationEnvironmentRequestEntries session state =
  maybe
    []
    ( \record ->
        Map.toAscList
          ( Map.map environmentRequestWitness record.environmentRequests
              <> Map.mapMaybe environmentRequestWitnessFromApplication record.requests
          )
    )
    (Map.lookup session state.sessionsById)

environmentRequestWitnessFromApplication ::
  RequestRecord -> Maybe EnvironmentRequestWitness
environmentRequestWitnessFromApplication record =
  case (record.call, record.acceptancePosition, record.replyBody) of
    ( NewEnvironmentApplication,
      Just position,
      OperationAccepted _ EnvironmentStabilizationPending
      ) ->
        Just
          ( EnvironmentRequestWitness
              record.requestId
              record.processEpoch
              (EnvironmentRequestAcceptedStage position)
          )
    ( NewEnvironmentApplication,
      Just position,
      Completed _ (NewEnvironmentCompleted _)
      ) ->
        Just
          ( EnvironmentRequestWitness
              record.requestId
              record.processEpoch
              (EnvironmentRequestCompletedStage position)
          )
    _ -> Nothing

applicationPendingEnvironmentEntries ::
  State -> [(ProcessAcceptancePosition, PendingEnvironment)]
applicationPendingEnvironmentEntries = Map.toAscList . (.pendingEnvironments)

-- | Terminal private environment outcomes in process-position order.  This
-- read-only projection cannot replace either the pending or completed owner.
applicationCompletedEnvironmentEntries ::
  State -> [(ProcessAcceptancePosition, CompletedEnvironment)]
applicationCompletedEnvironmentEntries =
  Map.toAscList . (.completedEnvironments)

-- | Key-preserving retained reply bodies for exact request-identity validation.
-- The reply body is immutable application-owner evidence, not a constructor for
-- owner state.
applicationRequestReplyBodyEntries ::
  ApplicationSessionId ->
  State ->
  [(RequestId, RetainedRequestReplyBody)]
applicationRequestReplyBodyEntries session state =
  maybe
    []
    (fmap (\(request, record) -> (request, record.replyBody)) . Map.toAscList . (.requests))
    (Map.lookup session state.sessionsById)

-- | Narrow contradiction fixture for composed invariant properties.
replaceApplicationAttachmentProcessForInvariantTest ::
  ApplicationAttachment ->
  ProcessEpochId ->
  State ->
  State
replaceApplicationAttachmentProcessForInvariantTest attachment process state =
  state
    { processByAttachment =
        Map.insert attachment process state.processByAttachment
    }

-- | Replace the sole attachment key for one retained process while preserving
-- its access and private-identity owners. This is a narrow cross-owner
-- contradiction fixture.
replaceApplicationAttachmentForProcessForInvariantTest ::
  ProcessEpochId ->
  ApplicationAttachment ->
  State ->
  State
replaceApplicationAttachmentForProcessForInvariantTest process replacement state =
  state
    { processByAttachment =
        Map.insert
          replacement
          process
          (Map.filter (/= process) state.processByAttachment)
    }

-- | Narrow contradiction fixture for composed invariant properties.
replaceApplicationSessionProcessForInvariantTest ::
  ApplicationSessionId ->
  ProcessEpochId ->
  State ->
  State
replaceApplicationSessionProcessForInvariantTest session process state =
  state
    { sessionsById =
        Map.adjust
          (replaceSessionProcessForInvariantTest process)
          session
          state.sessionsById
    }

-- | Narrow contradiction fixture for composed invariant properties.
replaceApplicationSessionBindingForInvariantTest ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  State ->
  State
replaceApplicationSessionBindingForInvariantTest session binding state =
  state
    { sessionsById =
        Map.adjust
          (replaceSessionBindingForInvariantTest binding)
          session
          state.sessionsById
    }

-- | Narrow contradiction fixture for composed invariant properties.
replaceApplicationSessionStartupAccessForInvariantTest ::
  ApplicationSessionId ->
  ApplicationStartupAccess ->
  State ->
  State
replaceApplicationSessionStartupAccessForInvariantTest session access state =
  state
    { sessionsById =
        Map.adjust
          (replaceSessionStartupAccessForInvariantTest access)
          session
          state.sessionsById
    }

-- | Narrow contradiction fixture for composed invariant properties.
replaceApplicationOpenRetryIndexForInvariantTest ::
  ProcessEpochId ->
  ClientNonce ->
  ApplicationSessionId ->
  State ->
  State
replaceApplicationOpenRetryIndexForInvariantTest process nonce session state =
  state
    { openSessionByNonce =
        Map.insert (process, nonce) session state.openSessionByNonce
    }

-- | Narrow contradiction fixture for composed invariant properties.
replaceApplicationNextSessionOrdinalForInvariantTest :: Word64 -> State -> State
replaceApplicationNextSessionOrdinalForInvariantTest replacement state =
  state {nextSessionOrdinal = replacement}

forceApplicationProcessAvailableForInvariantTest :: ProcessEpochId -> State -> State
forceApplicationProcessAvailableForInvariantTest process state =
  state
    { processRecoveryByProcess =
        Map.adjust markAvailable process state.processRecoveryByProcess
    }

replaceSessionProcessForInvariantTest :: ProcessEpochId -> SessionRecord -> SessionRecord
replaceSessionProcessForInvariantTest process record =
  SessionRecord
    { sessionId = record.sessionId,
      processEpoch = process,
      attachment = record.attachment,
      nonce = record.nonce,
      resumeToken = record.resumeToken,
      binding = record.binding,
      bindingEstablishedBy = record.bindingEstablishedBy,
      lastIssuedReplyCursor = record.lastIssuedReplyCursor,
      receiptRetirement = record.receiptRetirement,
      retiredReplyCursor = record.retiredReplyCursor,
      retainedDisposition = record.retainedDisposition,
      deliveryLive = record.deliveryLive,
      lastRecoveryGeneration = record.lastRecoveryGeneration,
      recovery = record.recovery,
      startupAccess = record.startupAccess,
      requests = record.requests,
      publicationRequests = record.publicationRequests,
      environmentRequests = record.environmentRequests
    }

replaceSessionBindingForInvariantTest ::
  ApplicationSessionBinding -> SessionRecord -> SessionRecord
replaceSessionBindingForInvariantTest replacement record =
  record {binding = replacement}

replaceSessionStartupAccessForInvariantTest ::
  ApplicationStartupAccess -> SessionRecord -> SessionRecord
replaceSessionStartupAccessForInvariantTest replacement record =
  record {startupAccess = replacement}

-- | Complete session-owned facts.  Process identity and bootstrap access remain
-- authoritative in their process-scoped maps and are not copied here.
data SessionRecord = SessionRecord
  { sessionId :: ApplicationSessionId,
    processEpoch :: ProcessEpochId,
    attachment :: ApplicationAttachment,
    nonce :: ClientNonce,
    resumeToken :: ApplicationResumeToken,
    binding :: ApplicationSessionBinding,
    bindingEstablishedBy :: ApplicationReplyCursor,
    lastIssuedReplyCursor :: ApplicationReplyCursor,
    receiptRetirement :: ApplicationReceiptRetirement,
    retiredReplyCursor :: Maybe ApplicationReplyCursor,
    retainedDisposition :: ApplicationSessionAcceptance,
    deliveryLive :: Bool,
    lastRecoveryGeneration :: Maybe ApplicationSessionRecoveryGeneration,
    recovery :: Maybe SessionRecovery,
    startupAccess :: ApplicationStartupAccess,
    requests :: (Map RequestId RequestRecord),
    publicationRequests :: (Map RequestId ApplicationPublicationRequestRecord),
    environmentRequests :: (Map RequestId EnvironmentRequestRecord)
  }
  deriving stock (Eq)

data RequestRecord = RequestRecord
  { requestId :: RequestId,
    call :: ApplicationOperation,
    processEpoch :: ProcessEpochId,
    acceptancePosition :: Maybe ProcessAcceptancePosition,
    replyCursor :: ApplicationReplyCursor,
    replyBody :: RetainedRequestReplyBody
  }
  deriving stock (Eq)

data ApplicationPublicationRequestRecord
  = ApplicationPublicationRequestRecord
      RequestId
      ApplicationPublicationCall
      ProcessEpochId
      ProcessAcceptancePosition
  deriving stock (Eq)

data EnvironmentRequestRecord
  = EnvironmentRequestRecord
      RequestId
      NewEnvironmentCoordinatorInput
      ProcessEpochId
      EnvironmentRequestStage
  deriving stock (Eq)

sessionWitness :: SessionRecord -> ApplicationSessionWitness
sessionWitness record =
  ApplicationSessionWitness
    { applicationSessionWitnessId = record.sessionId,
      applicationSessionWitnessProcess = record.processEpoch,
      applicationSessionWitnessAttachment = record.attachment,
      applicationSessionWitnessNonce = record.nonce,
      applicationSessionWitnessResumeToken = record.resumeToken,
      applicationSessionWitnessBinding = record.binding,
      applicationSessionWitnessDeliveryLive = record.deliveryLive,
      applicationSessionWitnessStartupAccess = record.startupAccess
    }

requestOwnerWitness :: SessionRecord -> ApplicationRequestOwnerWitness
requestOwnerWitness record =
  ApplicationRequestOwnerWitness
    { applicationRequestOwnerSession = record.sessionId,
      applicationRequestOwnerBindingEstablishedBy = record.bindingEstablishedBy,
      applicationRequestOwnerLastIssuedReplyCursor = record.lastIssuedReplyCursor,
      applicationRequestOwnerRetiredReplyCursor = record.retiredReplyCursor,
      applicationRequestOwnerRequests =
        fmap requestWitness (Map.elems record.requests)
    }

requestWitness :: RequestRecord -> ApplicationRequestWitness
requestWitness record =
  ApplicationRequestWitness
    { applicationRequestWitnessId = record.requestId,
      applicationRequestWitnessCall = record.call,
      applicationRequestWitnessProcess = record.processEpoch,
      applicationRequestWitnessPosition = record.acceptancePosition,
      applicationRequestWitnessCursor = record.replyCursor,
      applicationRequestWitnessStatus =
        Request.retainedReplyBodyStatus record.replyBody
    }

publicationRequestWitness ::
  ApplicationPublicationRequestRecord ->
  ApplicationPublicationRequestWitness
publicationRequestWitness
  (ApplicationPublicationRequestRecord request call process position) =
    case call of
      ApplicationPublicationCall privateNabla value ->
        ApplicationPublicationRequestWitness
          request
          privateNabla
          value
          process
          position

environmentRequestWitness ::
  EnvironmentRequestRecord -> EnvironmentRequestWitness
environmentRequestWitness
  (EnvironmentRequestRecord request _ process stage) =
    EnvironmentRequestWitness request process stage

-- | Immutable process-role membership supplied by applied Oracle projection.
--
-- The projection includes remote as well as locally resident applied process
-- roles.  The application leaf consumes only this first-order fact and neither
-- imports nor observes the Oracle-projection owner's private state.
newtype ProcessRoleView = ProcessRoleView (Set ProcessEpochId)
  deriving stock (Eq, Show)

-- | Construct one role projection from established process epochs.
--
-- The later application-call coordinator obtains this list from one predecessor
-- applied Oracle projection.  Keeping the constructor private prevents a value
-- traversal from replacing the view's representation.
processRoleView :: [ProcessEpochId] -> ProcessRoleView
processRoleView = ProcessRoleView . Set.fromList

-- | An ordinary rejection of application-provided value data.
data ApplicationValueRejection
  = ApplicationValueUnknownPrivateId PrivateUniqueId
  | ApplicationValueInvalidFieldName Text FieldNameError
  | ApplicationValueProcessRoleMismatch PrivateProcessId
  | ApplicationValueMisplacedSortDefinition
  deriving stock (Eq, Show)

-- | A contradiction between checked Herald state and an internal value.
--
-- The surrounding coordinator terminates on this fault; it must not expose or
-- recover from it as an application rejection.
data ApplicationValueInvariant
  = ApplicationValuePrivateIdentityInvariant PrivateIdentityError
  | ApplicationValueInternalProcessRoleMismatch ProcessEpochId
  deriving stock (Eq, Show)

-- | The explicitly classified failure of inbound ordinary-value translation.
data ApplicationValueGlobalizationError
  = ApplicationValueRejected ApplicationValueRejection
  | ApplicationValueGlobalizationInvariant ApplicationValueInvariant
  deriving stock (Eq, Show)

-- | Recursively admit and globalize an ordinary application value.
--
-- Globalization only resolves existing aliases, so a failure at any nesting depth
-- leaves the predecessor unchanged without requiring a prepared successor.  A
-- structured sort definition belongs to its dedicated checked adapter rather than
-- this structural traversal.
globalizeOrdinaryApplicationValue ::
  ProcessRoleView ->
  ProcessEpochId ->
  Application.ApplicationValue ->
  State ->
  Either ApplicationValueGlobalizationError Value
globalizeOrdinaryApplicationValue roles process value state =
  globalizeValue roles process state value

-- | Narrow coordinator port for query identity literals.  It uses the same
-- rejection/invariant classification as recursive ordinary-value globalization.
resolveApplicationPrivateUniqueId ::
  ProcessEpochId ->
  PrivateUniqueId ->
  State ->
  Either ApplicationValueGlobalizationError GlobalUniqueId
resolveApplicationPrivateUniqueId process privateIdentity state =
  resolveMappedPrivate process privateIdentity state.privateIdentity

-- | Narrow coordinator port for query label literals.
globalizeApplicationLabel ::
  ProcessRoleView ->
  ProcessEpochId ->
  Application.ApplicationLabel ->
  State ->
  Either ApplicationValueGlobalizationError Label
globalizeApplicationLabel roles process label state =
  globalizeLabel roles process state label

-- | Complete application-state successor and recursively localized ordinary value.
newtype PreparedOrdinaryApplicationValueLocalization
  = PreparedOrdinaryApplicationValueLocalization
      (Prepared State Application.ApplicationValue)

-- | Prepare recursive ordinary-value localization and every fresh alias as one
-- transition.
--
-- This structural primitive has no sort context.  A use-case coordinator must
-- route a checked sort-definition value to the dedicated definition adapter
-- before calling it; this function does not recognize a definition carrier from
-- record shape.
prepareOrdinaryApplicationValueLocalization ::
  ProcessRoleView ->
  ProcessEpochId ->
  Value ->
  State ->
  Either ApplicationValueInvariant PreparedOrdinaryApplicationValueLocalization
prepareOrdinaryApplicationValueLocalization roles process value state =
  prepareOrdinaryApplicationValueLocalizationWithZombieOverlay Set.empty roles process value state

-- | Localize against a losing-branch presentation overlay. Canonical stored
-- labels remain untouched; only resident process labels are projected Zombie.
prepareOrdinaryApplicationValueLocalizationWithZombieOverlay ::
  Set ProcessEpochId ->
  ProcessRoleView ->
  ProcessEpochId ->
  Value ->
  State ->
  Either ApplicationValueInvariant PreparedOrdinaryApplicationValueLocalization
prepareOrdinaryApplicationValueLocalizationWithZombieOverlay zombieProcesses roles process value state =
  PreparedOrdinaryApplicationValueLocalization
    <$> prepareTransition (localizeValueTransition zombieProcesses roles process value) state

-- | Inspect the complete localized output without exposing its successor state.
preparedLocalizedOrdinaryApplicationValue ::
  PreparedOrdinaryApplicationValueLocalization ->
  Application.ApplicationValue
preparedLocalizedOrdinaryApplicationValue
  (PreparedOrdinaryApplicationValueLocalization prepared) =
    preparedOutput prepared

-- | Reveal the already complete application successor and localized value.
commitOrdinaryApplicationValueLocalization ::
  PreparedOrdinaryApplicationValueLocalization ->
  (State, Application.ApplicationValue)
commitOrdinaryApplicationValueLocalization
  (PreparedOrdinaryApplicationValueLocalization prepared) =
    commitPrepared prepared

globalizeValue ::
  ProcessRoleView ->
  ProcessEpochId ->
  State ->
  Application.ApplicationValue ->
  Either ApplicationValueGlobalizationError Value
globalizeValue roles process state = \case
  Application.BoolValue value -> Right (Domain.boolValue value)
  Application.Int64Value value -> Right (Domain.int64Value value)
  Application.BytesValue value -> Right (Domain.bytesValue value)
  Application.TextValue value -> Right (Domain.textValue value)
  Application.UniqueIdValue privateIdentity ->
    Domain.globalUniqueIdValue
      <$> resolveMappedPrivate process privateIdentity state.privateIdentity
  Application.LabelValue label ->
    Domain.labelValue
      <$> globalizeLabel roles process state label
  Application.RecordValue fields ->
    Domain.recordValueMap . Map.fromAscList
      <$> traverse
        (globalizeField roles process state)
        (Map.toAscList fields)
  Application.SortDefinitionValue _ ->
    Left
      ( ApplicationValueRejected
          ApplicationValueMisplacedSortDefinition
      )
  Application.EnumValue value ->
    Right (Domain.enumValue (Domain.enumSymbol value))
  Application.OptionalUniqueIdValue privateIdentity ->
    Domain.optionalGlobalUniqueIdValue
      <$> traverse
        (\identity -> resolveMappedPrivate process identity state.privateIdentity)
        privateIdentity

globalizeField ::
  ProcessRoleView ->
  ProcessEpochId ->
  State ->
  (Text, Application.ApplicationValue) ->
  Either ApplicationValueGlobalizationError (Domain.FieldName, Value)
globalizeField roles process state (name, value) = do
  field <-
    either
      ( Left
          . ApplicationValueRejected
          . ApplicationValueInvalidFieldName name
      )
      Right
      (Domain.mkFieldName name)
  globalized <- globalizeValue roles process state value
  Right (field, globalized)

globalizeLabel ::
  ProcessRoleView ->
  ProcessEpochId ->
  State ->
  Application.ApplicationLabel ->
  Either ApplicationValueGlobalizationError Label
globalizeLabel _ _ _ (Application.VoidLabel, generation) = Right (Domain.VoidLabel, generation)
globalizeLabel roles process state (Application.ProcessLabel privateProcess, generation) =
  (,generation) . Domain.ProcessLabel
    <$> resolveProcessLabel roles process state privateProcess
globalizeLabel roles process state (Application.ZombieLabel privateProcess, generation) =
  (,generation) . Domain.ZombieLabel
    <$> resolveProcessLabel roles process state privateProcess

-- | Resolve one process-qualified label and classify missing role authority.
--
-- Oracle role absence is an invariant only when the application owner already
-- retains the same locally installed process.  An arbitrary mapped identity is
-- ordinary application input and remains a rejection.
resolveProcessLabel ::
  ProcessRoleView ->
  ProcessEpochId ->
  State ->
  PrivateProcessId ->
  Either ApplicationValueGlobalizationError ProcessEpochId
resolveProcessLabel roles process state privateProcess = do
  globalIdentity <-
    resolveMappedPrivate
      process
      (privateProcessUniqueId privateProcess)
      state.privateIdentity
  let candidate = processEpochFromGlobalUniqueId globalIdentity
  classify candidate
  where
    classify candidate
      | hasProcessRole candidate roles = Right candidate
      | Map.member candidate state.accessByProcess =
          Left
            ( ApplicationValueGlobalizationInvariant
                (ApplicationValueInternalProcessRoleMismatch candidate)
            )
      | otherwise =
          Left
            ( ApplicationValueRejected
                (ApplicationValueProcessRoleMismatch privateProcess)
            )

resolveMappedPrivate ::
  ProcessEpochId ->
  PrivateUniqueId ->
  PrivateIdentity ->
  Either ApplicationValueGlobalizationError GlobalUniqueId
resolveMappedPrivate process privateIdentity identityState = do
  resolved <-
    mapPrivateIdentityGlobalizationError
      (resolvePrivateUniqueId process privateIdentity identityState)
  maybe
    ( Left
        ( ApplicationValueRejected
            (ApplicationValueUnknownPrivateId privateIdentity)
        )
    )
    Right
    resolved

localizeValueTransition ::
  Set ProcessEpochId ->
  ProcessRoleView ->
  ProcessEpochId ->
  Value ->
  State ->
  Either ApplicationValueInvariant (State, Application.ApplicationValue)
localizeValueTransition zombieProcesses roles process value state = do
  (successorIdentity, localized) <-
    localizeValue zombieProcesses roles process value state.privateIdentity
  Right
    ( state {privateIdentity = successorIdentity},
      localized
    )

localizeValue ::
  Set ProcessEpochId ->
  ProcessRoleView ->
  ProcessEpochId ->
  Value ->
  PrivateIdentity ->
  Either ApplicationValueInvariant (PrivateIdentity, Application.ApplicationValue)
localizeValue zombieProcesses roles process value identityState = case Domain.viewValue value of
  Domain.BoolValue scalar -> unchanged (Application.BoolValue scalar)
  Domain.Int64Value scalar -> unchanged (Application.Int64Value scalar)
  Domain.BytesValue scalar -> unchanged (Application.BytesValue scalar)
  Domain.TextValue scalar -> unchanged (Application.TextValue scalar)
  Domain.GlobalUniqueIdValue globalIdentity -> do
    (successor, privateIdentity) <-
      localizeGlobal process globalIdentity identityState
    Right (successor, Application.UniqueIdValue privateIdentity)
  Domain.LabelValue label -> do
    (successor, localized) <-
      localizeLabel zombieProcesses roles process label identityState
    Right (successor, Application.LabelValue localized)
  Domain.RecordValue fields -> do
    (successor, localizedFields) <-
      foldM
        (localizeField zombieProcesses roles process)
        (identityState, Map.empty)
        (Map.toAscList fields)
    Right (successor, Application.RecordValue localizedFields)
  Domain.EnumValue scalar ->
    unchanged (Application.EnumValue (Domain.enumSymbolText scalar))
  Domain.OptionalGlobalUniqueIdValue globalIdentity ->
    case globalIdentity of
      Nothing -> unchanged (Application.OptionalUniqueIdValue Nothing)
      Just identity -> do
        (successor, privateIdentity) <-
          localizeGlobal process identity identityState
        Right
          ( successor,
            Application.OptionalUniqueIdValue (Just privateIdentity)
          )
  where
    unchanged localized = Right (identityState, localized)

localizeField ::
  Set ProcessEpochId ->
  ProcessRoleView ->
  ProcessEpochId ->
  (PrivateIdentity, Map Text Application.ApplicationValue) ->
  (Domain.FieldName, Value) ->
  Either
    ApplicationValueInvariant
    (PrivateIdentity, Map Text Application.ApplicationValue)
localizeField zombieProcesses roles process (identityState, accumulated) (field, value) = do
  (successor, localized) <- localizeValue zombieProcesses roles process value identityState
  Right
    ( successor,
      Map.insert (Domain.fieldNameText field) localized accumulated
    )

localizeLabel ::
  Set ProcessEpochId ->
  ProcessRoleView ->
  ProcessEpochId ->
  Label ->
  PrivateIdentity ->
  Either ApplicationValueInvariant (PrivateIdentity, Application.ApplicationLabel)
localizeLabel _ _ _ (Domain.VoidLabel, generation) identityState =
  Right (identityState, (Application.VoidLabel, generation))
localizeLabel zombieProcesses roles process (Domain.ProcessLabel labeledProcess, generation) identityState = do
  (successor, privateProcess) <-
    if Set.member labeledProcess zombieProcesses
      then localizeKnownZombieProcessLabel process labeledProcess identityState
      else localizeProcessLabel roles process labeledProcess identityState
  Right
    ( successor,
      ( if Set.member labeledProcess zombieProcesses
          then Application.ZombieLabel privateProcess
          else Application.ProcessLabel privateProcess,
        generation
      )
    )
localizeLabel _ roles process (Domain.ZombieLabel labeledProcess, generation) identityState = do
  (successor, privateProcess) <-
    localizeProcessLabel roles process labeledProcess identityState
  Right (successor, (Application.ZombieLabel privateProcess, generation))

-- | A process retained in the isolation overlay is already witnessed by the
-- fenced application owner. Its canonical process-role projection may have
-- advanced to End, so localizing the read-only presentation must not consult
-- that newer projection again.
localizeKnownZombieProcessLabel ::
  ProcessEpochId ->
  ProcessEpochId ->
  PrivateIdentity ->
  Either ApplicationValueInvariant (PrivateIdentity, PrivateProcessId)
localizeKnownZombieProcessLabel process labeledProcess identityState = do
  (successor, privateIdentity) <-
    localizeGlobal
      process
      (globalUniqueIdForProcessEpoch labeledProcess)
      identityState
  Right (successor, asPrivateProcessId privateIdentity)

localizeProcessLabel ::
  ProcessRoleView ->
  ProcessEpochId ->
  ProcessEpochId ->
  PrivateIdentity ->
  Either ApplicationValueInvariant (PrivateIdentity, PrivateProcessId)
localizeProcessLabel roles process labeledProcess identityState
  | not (hasProcessRole labeledProcess roles) =
      Left (ApplicationValueInternalProcessRoleMismatch labeledProcess)
  | otherwise = do
      (successor, privateIdentity) <-
        localizeGlobal
          process
          (globalUniqueIdForProcessEpoch labeledProcess)
          identityState
      Right (successor, asPrivateProcessId privateIdentity)

localizeGlobal ::
  ProcessEpochId ->
  GlobalUniqueId ->
  PrivateIdentity ->
  Either ApplicationValueInvariant (PrivateIdentity, PrivateUniqueId)
localizeGlobal process globalIdentity identityState =
  commitLocalization
    <$> mapPrivateIdentityLocalizationError
      (prepareLocalization process globalIdentity identityState)

hasProcessRole :: ProcessEpochId -> ProcessRoleView -> Bool
hasProcessRole process (ProcessRoleView processes) = Set.member process processes

processEpochFromGlobalUniqueId :: GlobalUniqueId -> ProcessEpochId
processEpochFromGlobalUniqueId =
  processEpochIdFromGlobalObjectId . globalObjectIdFromGlobalUniqueId

globalUniqueIdForProcessEpoch :: ProcessEpochId -> GlobalUniqueId
globalUniqueIdForProcessEpoch =
  globalUniqueIdFromGlobalObjectId . globalObjectIdFromProcessEpochId

mapPrivateIdentityGlobalizationError ::
  Either PrivateIdentityError value ->
  Either ApplicationValueGlobalizationError value
mapPrivateIdentityGlobalizationError =
  either
    ( Left
        . ApplicationValueGlobalizationInvariant
        . ApplicationValuePrivateIdentityInvariant
    )
    Right

mapPrivateIdentityLocalizationError ::
  Either PrivateIdentityError value ->
  Either ApplicationValueInvariant value
mapPrivateIdentityLocalizationError =
  either (Left . ApplicationValuePrivateIdentityInvariant) Right

-- | A contradiction in the opaque application owner encountered while preparing
-- a session transition.
data ApplicationSessionInvariant
  = ApplicationSessionBootstrapContradiction ProcessEpochId
  | ApplicationSessionStartupAccessContradiction StartupAccessError
  | ApplicationSessionRetryIndexContradiction ApplicationSessionId
  | ApplicationSessionAllocationContradiction ApplicationSessionId
  | ApplicationProcessRecoveryContradiction ProcessEpochId
  | ApplicationAcceptanceCounterContradiction ProcessEpochId
  | ApplicationRequestRecordContradiction ApplicationSessionId RequestId
  | ApplicationEnvironmentRequestContradiction RequestId
  | ApplicationOperationCompletionContradiction ProcessAcceptancePosition
  | ApplicationWaitRequestContradiction WaitId
  | ApplicationLocalizedCompletionPredecessorContradiction
      ApplicationSessionId
      RequestId
  | ApplicationDetachedRequestContradiction ApplicationRequestKey
  | ApplicationGatePhaseContradiction ApplicationGatePhase
  | ApplicationActiveLabelContradiction LabelDecisionId
  | ApplicationLabelSelectionContradiction ProcessEpochId PrivateObjectId
  deriving stock (Eq, Show)

-- | Explicit classification of ordinary session rejection versus an internal
-- checked-state fault.
data ApplicationSessionTransitionError
  = ApplicationSessionRejected ApplicationSessionRejection
  | ApplicationSessionInvariantFault ApplicationSessionInvariant
  | ApplicationSessionGatePending
  deriving stock (Eq, Show)

-- | Complete application-owner successor and indivisible binding/reply result.
data PreparedApplicationSessionAcceptance
  = PreparedApplicationSessionAcceptance
      (Prepared State ApplicationSessionAcceptance)
      (Maybe TimerAttempt)

-- | Prepare a new session or exact live nonce retry.
prepareApplicationSessionOpen ::
  HeraldEpoch ->
  ApplicationAttachment ->
  ClientNonce ->
  State ->
  Either
    ApplicationSessionTransitionError
    PreparedApplicationSessionAcceptance
prepareApplicationSessionOpen herald attachment nonce state =
  prepareAcceptance
    state
    (prepareTransition (openSessionTransition herald attachment nonce) state)

-- | Resume from the last reply cursor observed by the client.
--
-- A cursor below the one that established the current binding reoffers the
-- retained disposition.  An observed issued cursor advances the binding once.
prepareApplicationSessionResume ::
  ApplicationSessionId ->
  ApplicationResumeToken ->
  ApplicationReplyCursor ->
  State ->
  Either
    ApplicationSessionTransitionError
    PreparedApplicationSessionAcceptance
prepareApplicationSessionResume session token lastObserved state =
  prepareAcceptance
    state
    (prepareTransition (resumeSessionTransition session token lastObserved) state)

prepareAcceptance ::
  State ->
  Either
    ApplicationSessionTransitionError
    (Prepared State ApplicationSessionAcceptance) ->
  Either
    ApplicationSessionTransitionError
    PreparedApplicationSessionAcceptance
prepareAcceptance predecessor preparedResult = do
  prepared <- preparedResult
  let acceptance = preparedOutput prepared
      session = Session.sessionBindingSessionId (Session.sessionAcceptanceBinding acceptance)
  pure
    ( PreparedApplicationSessionAcceptance
        prepared
        (applicationSessionTimerAttempt session predecessor)
    )

preparedApplicationSessionAcceptance ::
  PreparedApplicationSessionAcceptance ->
  ApplicationSessionAcceptance
preparedApplicationSessionAcceptance
  (PreparedApplicationSessionAcceptance prepared _) =
    preparedOutput prepared

preparedApplicationSessionAcceptanceTimerCancellation ::
  PreparedApplicationSessionAcceptance ->
  Maybe TimerAttempt
preparedApplicationSessionAcceptanceTimerCancellation
  (PreparedApplicationSessionAcceptance _ cancellation) =
    cancellation

commitApplicationSessionAcceptance ::
  PreparedApplicationSessionAcceptance ->
  (State, ApplicationSessionAcceptance)
commitApplicationSessionAcceptance
  (PreparedApplicationSessionAcceptance prepared _) =
    commitPrepared prepared

-- | A live session owns both ordinary and lifecycle receipt frontiers. The
-- composed caller checks lifecycle pins before committing this preparation.
applicationReceiptRetirementProgress ::
  ApplicationSessionId -> State -> Maybe ApplicationReceiptRetirement
applicationReceiptRetirementProgress session state =
  (.receiptRetirement) <$> Map.lookup session state.sessionsById

applicationRequestRetirement ::
  ApplicationSessionId -> RequestId -> State -> Maybe Word64
applicationRequestRetirement session request state = do
  record <- Map.lookup session state.sessionsById
  retiredRequestThrough record request

retiredRequestThrough :: SessionRecord -> RequestId -> Maybe Word64
retiredRequestThrough record request = do
  through <- ordinaryReceiptRetirementThrough record.receiptRetirement
  if Retirement.receiptIsRetired (Request.requestIdWord64 request) (ordinaryReceiptRetirement record.receiptRetirement) then Just through else Nothing

newtype PreparedApplicationReceiptRetirement
  = PreparedApplicationReceiptRetirement State

-- | Retire completed correlations outside the unresolved exceptions. Missing
-- released identities are sealed too, so a late first offer cannot execute.
prepareApplicationReceiptRetirement ::
  ApplicationSessionBinding ->
  ApplicationSessionId ->
  ApplicationReceiptRetirement ->
  State ->
  Either ApplicationSessionTransitionError PreparedApplicationReceiptRetirement
prepareApplicationReceiptRetirement binding session progress state = do
  record <- checkedRequestSession session binding state
  let merged = record.receiptRetirement <> progress
      transferred = Map.adjust (transferInitialClaim session) record.attachment state.initialClaimsByAttachment
      advancing = ordinaryReceiptRetirement merged /= ordinaryReceiptRetirement record.receiptRetirement
  if not advancing
    then
      Right
        ( PreparedApplicationReceiptRetirement
            state
              { sessionsById = Map.insert session record {receiptRetirement = merged} state.sessionsById,
                initialClaimsByAttachment = transferred
              }
        )
    else do
      let covered request = Retirement.receiptIsRetired (Request.requestIdWord64 request) (ordinaryReceiptRetirement merged)
          coveredKey key = applicationRequestKeySession key == session && covered (applicationRequestKeyRequest key)
          (removed, retained) = Map.partitionWithKey (\request _ -> covered request) record.requests
          isTerminal request = case request.replyBody of
            Completed {} -> True
            Rejected {} -> True
            Cancelled {} -> True
            WaitAccepted {} -> False
            OperationAccepted {} -> False
          pins =
            any (not . isTerminal) removed
              || any covered (Map.keys record.publicationRequests)
              || any covered (Map.keys record.environmentRequests)
              || any coveredKey (Map.keys state.labelCandidates)
              || any coveredKey (Map.keys state.gatedIngress)
              || any (maybe False coveredKey . applicationFenceHeldReplyKey) (applicationFenceHeldEntries state)
      if pins
        then rejectSession Session.ApplicationReceiptRetirementNotReady
        else do
          let cursor = foldr (max . Just . (.replyCursor)) record.retiredReplyCursor removed
              successorRecord =
                record
                  { requests = retained,
                    receiptRetirement = merged,
                    retiredReplyCursor = cursor,
                    retainedDisposition = retireDisposition covered record.retainedDisposition
                  }
              retireDisposition coveredRequest acceptance = case Session.sessionAcceptanceReply acceptance of
                Session.SessionResumed retainedSession summary ->
                  Session.applicationSessionAcceptance
                    (Session.sessionAcceptanceBinding acceptance)
                    (Session.sessionAcceptanceCursor acceptance)
                    ( Session.SessionResumed
                        retainedSession
                        ( Request.applicationRequestSummary
                            (filter (not . coveredRequest . Request.requestSummaryEntryRequestId) (Request.requestSummaryEntries summary))
                        )
                    )
                Session.SessionOpened {} -> acceptance
              detachCompleted retainedEnvironments removedRequest = case (removedRequest.call, removedRequest.acceptancePosition) of
                (NewEnvironmentApplication, Just position) ->
                  Map.adjust Environment.detachCompletedEnvironmentReply position retainedEnvironments
                _ -> retainedEnvironments
              successor =
                state
                  { sessionsById = Map.insert session successorRecord state.sessionsById,
                    initialClaimsByAttachment = transferred,
                    completedEnvironments = foldl detachCompleted state.completedEnvironments removed
                  }
          Right (PreparedApplicationReceiptRetirement successor)

commitApplicationReceiptRetirement :: PreparedApplicationReceiptRetirement -> State
commitApplicationReceiptRetirement (PreparedApplicationReceiptRetirement successor) = successor

-- | A current TCP loss ends its session immediately. Other attached sessions
-- still keep the process live, and previously accepted semantic work survives
-- the same reply detachment used by ordinary session end.
prepareApplicationBindingTermination ::
  ApplicationSessionBinding ->
  State ->
  Either ApplicationSessionTransitionError PreparedApplicationRecoverySweep
prepareApplicationBindingTermination lostBinding state =
  case Map.lookup session state.sessionsById of
    Just record | record.binding == lostBinding && record.deliveryLive -> do
      recovery <-
        maybe
          (sessionInvariant (ApplicationProcessRecoveryContradiction record.processEpoch))
          Right
          (Map.lookup record.processEpoch state.processRecoveryByProcess)
      let endingRecovery = case recovery.phase of
            ApplicationProcessAvailable -> startProcessRecovery recovery
            _ -> recovery
          prepared = state {processRecoveryByProcess = Map.insert record.processEpoch endingRecovery state.processRecoveryByProcess}
          cancellations = maybe [] pure (applicationSessionTimerAttempt session state)
      (successor, expiration) <- expireApplicationSessions record.processEpoch [session] cancellations prepared
      Right (PreparedApplicationRecoverySweep successor expiration)
    _ -> Right (PreparedApplicationRecoverySweep state (ApplicationRecoveryExpiration [] [] [] Nothing))
  where
    session = Session.sessionBindingSessionId lostBinding

-- | A complete binding-loss observation.  Stale and duplicate observations
-- prepare the unchanged predecessor.
data ApplicationBindingLossDisposition
  = ApplicationBindingLossStale
  | ApplicationRecoveryStarted TimerAttempt TimerSpec
  deriving stock (Eq, Show)

data PreparedApplicationBindingLoss
  = PreparedApplicationBindingLoss State ApplicationBindingLossDisposition

prepareApplicationBindingLoss ::
  ApplicationRecoveryConfiguration ->
  MonotonicInstant ->
  ApplicationSessionBinding ->
  State ->
  PreparedApplicationBindingLoss
prepareApplicationBindingLoss configuration observedAt lostBinding state =
  uncurry
    PreparedApplicationBindingLoss
    (bindingLossSuccessor configuration observedAt lostBinding state)

preparedApplicationBindingLossDisposition ::
  PreparedApplicationBindingLoss ->
  ApplicationBindingLossDisposition
preparedApplicationBindingLossDisposition
  (PreparedApplicationBindingLoss _ disposition) =
    disposition

commitApplicationBindingLoss :: PreparedApplicationBindingLoss -> State
commitApplicationBindingLoss (PreparedApplicationBindingLoss successor _) = successor

-- | Canonically ordered reachability removed by one or more expired session
-- recoveries.  A process is present only for the one transition that sealed it.
data ApplicationRecoveryExpiration
  = ApplicationRecoveryExpiration
      [ApplicationSessionId]
      [WaitId]
      [TimerAttempt]
      (Maybe ProcessEpochId)
  deriving stock (Eq, Show)

applicationRecoveryExpiredSessionIds ::
  ApplicationRecoveryExpiration -> [ApplicationSessionId]
applicationRecoveryExpiredSessionIds
  (ApplicationRecoveryExpiration sessions _ _ _) = sessions

applicationRecoveryExpiredWaitIds ::
  ApplicationRecoveryExpiration -> [WaitId]
applicationRecoveryExpiredWaitIds
  (ApplicationRecoveryExpiration _ waits _ _) = waits

applicationRecoveryCancelledTimers ::
  ApplicationRecoveryExpiration -> [TimerAttempt]
applicationRecoveryCancelledTimers
  (ApplicationRecoveryExpiration _ _ attempts _) = attempts

applicationRecoverySealedProcess ::
  ApplicationRecoveryExpiration -> Maybe ProcessEpochId
applicationRecoverySealedProcess
  (ApplicationRecoveryExpiration _ _ _ process) = process

data PreparedApplicationRecoverySweep
  = PreparedApplicationRecoverySweep
      State
      ApplicationRecoveryExpiration

-- | Lazily expire every due recovery for one process before admitting a new
-- Open/Resume input. This makes deadline equality deterministic even when the
-- physical timer callback is delayed in another runtime source queue.
prepareApplicationRecoverySweep ::
  MonotonicInstant ->
  ProcessEpochId ->
  State ->
  Either ApplicationSessionTransitionError PreparedApplicationRecoverySweep
prepareApplicationRecoverySweep observedAt process state = do
  (successor, expiration) <- expireApplicationSessions process due attempts state
  pure (PreparedApplicationRecoverySweep successor expiration)
  where
    due =
      [ session
      | (session, record) <- Map.toAscList state.sessionsById,
        record.processEpoch == process,
        recovery <- maybe [] pure record.recovery,
        observedAt >= recovery.deadline
      ]
    attempts =
      [ applicationSessionRecoveryTimerAttempt
          session
          recovery.generation
          recovery.timerAttempt
      | session <- due,
        record <- maybe [] pure (Map.lookup session state.sessionsById),
        recovery <- maybe [] pure record.recovery
      ]

preparedApplicationRecoverySweepExpiration ::
  PreparedApplicationRecoverySweep -> ApplicationRecoveryExpiration
preparedApplicationRecoverySweepExpiration
  (PreparedApplicationRecoverySweep _ expiration) = expiration

commitApplicationRecoverySweep :: PreparedApplicationRecoverySweep -> State
commitApplicationRecoverySweep
  (PreparedApplicationRecoverySweep successor _) = successor

data ApplicationRecoveryTimerDisposition
  = ApplicationRecoveryTimerStale
  | ApplicationRecoveryTimerRearmed TimerAttempt TimerSpec
  | ApplicationRecoveryTimerExpired ApplicationRecoveryExpiration
  deriving stock (Eq, Show)

data PreparedApplicationRecoveryTimerObservation
  = PreparedApplicationRecoveryTimerObservation
      State
      ApplicationRecoveryTimerDisposition

prepareApplicationRecoveryTimerObservation ::
  TimerAttempt ->
  TimerOutcome ->
  State ->
  Either
    ApplicationSessionTransitionError
    PreparedApplicationRecoveryTimerObservation
prepareApplicationRecoveryTimerObservation observedAttempt outcome state =
  case timerAttemptApplicationSessionRecovery observedAttempt of
    Nothing -> stale
    Just (session, generation, attemptGeneration) ->
      case Map.lookup session state.sessionsById of
        Just record
          | Just recovery <- record.recovery,
            recovery.generation == generation,
            recovery.timerAttempt == attemptGeneration ->
              applyCurrent record recovery
        _ -> stale
  where
    stale =
      Right
        ( PreparedApplicationRecoveryTimerObservation
            state
            ApplicationRecoveryTimerStale
        )
    applyCurrent record recovery = case outcome of
      TimerFired firedAt
        | firedAt >= recovery.deadline -> do
            (successor, expiration) <-
              expireApplicationSessions
                record.processEpoch
                [record.sessionId]
                []
                state
            pure
              ( PreparedApplicationRecoveryTimerObservation
                  successor
                  (ApplicationRecoveryTimerExpired expiration)
              )
        | otherwise ->
            let successorAttempt = nextTimerAttemptGeneration recovery.timerAttempt
                successorRecovery = recovery {timerAttempt = successorAttempt}
                successorRecord = record {recovery = Just successorRecovery}
                successor =
                  state
                    { sessionsById =
                        Map.insert record.sessionId successorRecord state.sessionsById
                    }
                attempt =
                  applicationSessionRecoveryTimerAttempt
                    record.sessionId
                    recovery.generation
                    successorAttempt
             in Right
                  ( PreparedApplicationRecoveryTimerObservation
                      successor
                      ( ApplicationRecoveryTimerRearmed
                          attempt
                          (absoluteTimerSpec recovery.deadline)
                      )
                  )

preparedApplicationRecoveryTimerDisposition ::
  PreparedApplicationRecoveryTimerObservation ->
  ApplicationRecoveryTimerDisposition
preparedApplicationRecoveryTimerDisposition
  (PreparedApplicationRecoveryTimerObservation _ disposition) = disposition

commitApplicationRecoveryTimerObservation ::
  PreparedApplicationRecoveryTimerObservation -> State
commitApplicationRecoveryTimerObservation
  (PreparedApplicationRecoveryTimerObservation successor _) = successor

expireApplicationSessions ::
  ProcessEpochId ->
  [ApplicationSessionId] ->
  [TimerAttempt] ->
  State ->
  Either
    ApplicationSessionTransitionError
    (State, ApplicationRecoveryExpiration)
expireApplicationSessions _ [] _ state =
  Right (state, ApplicationRecoveryExpiration [] [] [] Nothing)
expireApplicationSessions process sessions cancelled state = do
  processRecovery <-
    maybe
      (sessionInvariant (ApplicationProcessRecoveryContradiction process))
      Right
      (Map.lookup process state.processRecoveryByProcess)
  let removed =
        Map.restrictKeys state.sessionsById (Set.fromList sessions)
  if Map.size removed /= length sessions
    then sessionInvariant (ApplicationProcessRecoveryContradiction process)
    else do
      let removedSessionSet = Map.keysSet removed
          removedWaits = sort (concatMap pendingWaitIds (Map.elems removed))
          withoutSessions =
            state
              { sessionsById = Map.withoutKeys state.sessionsById removedSessionSet,
                preparationSessionChanges = Set.union removedSessionSet state.preparationSessionChanges,
                openSessionByNonce =
                  Map.filter (`Set.notMember` removedSessionSet) state.openSessionByNonce,
                labelCandidates =
                  Map.filterWithKey
                    (\key _ -> applicationRequestKeySession key `Set.notMember` removedSessionSet)
                    state.labelCandidates,
                gatedIngress =
                  Map.filterWithKey
                    (\key _ -> applicationRequestKeySession key `Set.notMember` removedSessionSet)
                    state.gatedIngress,
                acceptedFenceHeld =
                  detachFenceHeldReplyLinks
                    ((`Set.member` removedSessionSet) . applicationRequestKeySession)
                    state.acceptedFenceHeld,
                pendingEnvironments =
                  detachPendingEnvironmentReplies
                    ( maybe
                        False
                        ( (`Set.member` removedSessionSet)
                            . Environment.environmentReplyKeySession
                        )
                        . Environment.pendingEnvironmentReplyKey
                    )
                    state.pendingEnvironments,
                completedEnvironments =
                  detachCompletedEnvironmentReplies
                    ( maybe
                        False
                        ( (`Set.member` removedSessionSet)
                            . Environment.environmentReplyKeySession
                        )
                        . Environment.completedEnvironmentReplyKey
                    )
                    state.completedEnvironments
              }
      (successor, sealed) <-
        settleProcessAfterUnexpectedExpiry process processRecovery withoutSessions
      pure
        ( successor,
          ApplicationRecoveryExpiration
            (Map.keys removed)
            removedWaits
            cancelled
            (if sealed then Just process else Nothing)
        )

settleProcessAfterUnexpectedExpiry ::
  ProcessEpochId ->
  ApplicationProcessRecovery ->
  State ->
  Either ApplicationSessionTransitionError (State, Bool)
settleProcessAfterUnexpectedExpiry process recovery state
  | processHasAttachedSession process state =
      Right
        ( state
            { processRecoveryByProcess =
                markProcessAvailable process state.processRecoveryByProcess
            },
          False
        )
  | processHasRecoveringSession process state =
      Right (beginProcessRecoveryIfNeeded process state, False)
  | otherwise = case recovery.phase of
      ApplicationProcessRecovering generation ->
        Right
          ( state
              { processRecoveryByProcess =
                  Map.insert
                    process
                    recovery {phase = ApplicationProcessSealed generation}
                    state.processRecoveryByProcess
              },
            True
          )
      ApplicationProcessSealed _ -> Right (state, False)
      ApplicationProcessAvailable ->
        sessionInvariant (ApplicationProcessRecoveryContradiction process)

-- | Complete end-session successor.  The process-scoped identity and startup
-- records are deliberately absent from this preparation's write set.
newtype PreparedApplicationSessionEnd
  = PreparedApplicationSessionEnd (Prepared State [WaitId])

prepareApplicationSessionEnd ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  State ->
  Either ApplicationSessionTransitionError PreparedApplicationSessionEnd
prepareApplicationSessionEnd session currentBinding state =
  PreparedApplicationSessionEnd
    <$> prepareTransition
      (endSessionTransition session currentBinding)
      state

preparedApplicationSessionEndWaitIds ::
  PreparedApplicationSessionEnd ->
  [WaitId]
preparedApplicationSessionEndWaitIds
  (PreparedApplicationSessionEnd prepared) =
    preparedOutput prepared

commitApplicationSessionEnd :: PreparedApplicationSessionEnd -> State
commitApplicationSessionEnd (PreparedApplicationSessionEnd prepared) =
  fst (commitPrepared prepared)

-- | Canonically ordered application-owned reachability removed for one ended
-- process epoch.  The semantic work referenced by those requests is owned by
-- other leaves and deliberately does not occur in this report.
data ApplicationProcessRetirement
  = ApplicationProcessRetirement
      [ApplicationSessionId]
      [WaitId]
      [TimerAttempt]
  deriving stock (Eq, Show)

applicationProcessRetirementSessionIds ::
  ApplicationProcessRetirement ->
  [ApplicationSessionId]
applicationProcessRetirementSessionIds
  (ApplicationProcessRetirement sessions _ _) =
    sessions

applicationProcessRetirementWaitIds ::
  ApplicationProcessRetirement ->
  [WaitId]
applicationProcessRetirementWaitIds
  (ApplicationProcessRetirement _ waits _) =
    waits

applicationProcessRetirementTimerAttempts ::
  ApplicationProcessRetirement ->
  [TimerAttempt]
applicationProcessRetirementTimerAttempts
  (ApplicationProcessRetirement _ _ attempts) =
    attempts

-- | Complete process-wide application-owner retirement.  Preparation is total
-- for an Oracle-authorized End: the private-identity owner can tombstone an
-- epoch even when its local attachment was never installed, and a repeated End
-- prepares the unchanged state with empty removal lists.
data PreparedApplicationProcessRetirement
  = PreparedApplicationProcessRetirement
      State
      ApplicationProcessRetirement

prepareApplicationProcessRetirement ::
  ProcessEpochId ->
  State ->
  PreparedApplicationProcessRetirement
prepareApplicationProcessRetirement process state =
  PreparedApplicationProcessRetirement successor retirement
  where
    (successorIdentity, _) =
      commitOracleProcessRetirement
        (prepareOracleProcessRetirement process state.privateIdentity)
    (removedSessions, retainedSessions) =
      Map.partition
        ((== process) . (.processEpoch))
        state.sessionsById
    removedSessionIds = Map.keys removedSessions
    removedWaitIds =
      sort
        ( concatMap
            pendingWaitIds
            (Map.elems removedSessions)
        )
    removedTimerAttempts =
      [ applicationSessionRecoveryTimerAttempt
          session
          recovery.generation
          recovery.timerAttempt
      | (session, record) <- Map.toAscList removedSessions,
        recovery <- maybe [] pure record.recovery
      ]
    retirement =
      ApplicationProcessRetirement
        removedSessionIds
        removedWaitIds
        removedTimerAttempts
    successor =
      state
        { privateIdentity = successorIdentity,
          accessByProcess = Map.delete process state.accessByProcess,
          processByAttachment =
            Map.filter (/= process) state.processByAttachment,
          sessionsById = retainedSessions,
          preparationSessionChanges = Set.union (Map.keysSet removedSessions) state.preparationSessionChanges,
          openSessionByNonce =
            Map.filterWithKey
              (\(owner, _) _ -> owner /= process)
              state.openSessionByNonce,
          processRecoveryByProcess =
            Map.delete process state.processRecoveryByProcess,
          nextAcceptanceByProcess =
            Map.delete process state.nextAcceptanceByProcess,
          labelCandidates =
            Map.filter
              (\(LabelCandidateRecord owner _) -> owner /= process)
              state.labelCandidates,
          gatedIngress =
            Map.filter
              (\(GatedIngressRecord owner _ _) -> owner /= process)
              state.gatedIngress,
          acceptedFenceHeld =
            detachFenceHeldReplyLinks
              ( \key ->
                  case Map.lookup
                    (applicationRequestKeySession key)
                    removedSessions of
                    Just _ -> True
                    Nothing -> False
              )
              state.acceptedFenceHeld,
          pendingEnvironments =
            detachPendingEnvironmentReplies
              ( (== process)
                  . Environment.environmentManifestProcess
                  . Environment.pendingEnvironmentManifest
              )
              state.pendingEnvironments,
          completedEnvironments =
            Map.map
              ( \completed ->
                  if Environment.environmentManifestProcess
                    (Environment.completedEnvironmentManifest completed)
                    == process
                    then Environment.retireCompletedEnvironmentAccess completed
                    else completed
              )
              state.completedEnvironments
        }

preparedApplicationProcessRetirement ::
  PreparedApplicationProcessRetirement ->
  ApplicationProcessRetirement
preparedApplicationProcessRetirement
  (PreparedApplicationProcessRetirement _ retirement) =
    retirement

commitApplicationProcessRetirement ::
  PreparedApplicationProcessRetirement ->
  (State, ApplicationProcessRetirement)
commitApplicationProcessRetirement
  (PreparedApplicationProcessRetirement successor retirement) =
    (successor, retirement)

-- | Resolve the process that owns one live session.
applicationSessionProcess ::
  ApplicationSessionId ->
  State ->
  Maybe ProcessEpochId
applicationSessionProcess session state =
  (.processEpoch) <$> Map.lookup session state.sessionsById

applicationSessionAttachment :: ApplicationSessionId -> State -> Maybe ApplicationAttachment
applicationSessionAttachment session state =
  (.attachment) <$> Map.lookup session state.sessionsById

-- | Observe the exact current logical binding of one live session.
applicationSessionCurrentBinding ::
  ApplicationSessionId ->
  State ->
  Maybe ApplicationSessionBinding
applicationSessionCurrentBinding session state =
  (.binding) <$> Map.lookup session state.sessionsById

-- | Whether the current binding still has a delivery path.
applicationSessionDeliveryLive ::
  ApplicationSessionId ->
  State ->
  Maybe Bool
applicationSessionDeliveryLive session state =
  (.deliveryLive) <$> Map.lookup session state.sessionsById

-- | One ordered notice for a session whose current physical binding survives
-- the atomic local self-fence. The constructor stays private so a caller
-- cannot invent a reply cursor independently of the application owner.
data ApplicationIsolationNotice
  = ApplicationIsolationNotice
      ApplicationSessionBinding
      ApplicationReplyCursor
      ApplicationSessionId
  deriving stock (Eq, Show)

applicationIsolationNoticeBinding ::
  ApplicationIsolationNotice -> ApplicationSessionBinding
applicationIsolationNoticeBinding
  (ApplicationIsolationNotice binding _ _) = binding

applicationIsolationNoticeCursor ::
  ApplicationIsolationNotice -> ApplicationReplyCursor
applicationIsolationNoticeCursor
  (ApplicationIsolationNotice _ cursor _) = cursor

applicationIsolationNoticeSession ::
  ApplicationIsolationNotice -> ApplicationSessionId
applicationIsolationNoticeSession
  (ApplicationIsolationNotice _ _ session) = session

-- | Advance every currently deliverable session by exactly one ordered reply
-- cursor and return its isolation notice. Detached sessions cannot observe the
-- notice and remain absent from the bounded read-only drain.
beginApplicationIsolationDrain ::
  State ->
  (State, [ApplicationIsolationNotice])
beginApplicationIsolationDrain state =
  (state {sessionsById = sessions}, notices)
  where
    (sessions, notices) =
      foldl advance (state.sessionsById, []) (Map.toAscList state.sessionsById)
    advance (retained, accumulated) (session, record)
      | not record.deliveryLive = (retained, accumulated)
      | otherwise =
          let cursor = Request.nextApplicationReplyCursor record.lastIssuedReplyCursor
              successor = record {lastIssuedReplyCursor = cursor}
           in ( Map.insert session successor retained,
                accumulated
                  <> [ApplicationIsolationNotice record.binding cursor session]
              )

-- | Orderly Herald drain is not application crash evidence. Cancel every
-- outstanding recovery timer and make all retained reply lanes unreachable
-- without sealing a process or creating an Oracle intention.
cutApplicationRecoveryForDrain :: State -> (State, [TimerAttempt])
cutApplicationRecoveryForDrain state =
  ( state
      { sessionsById = Map.map detach state.sessionsById,
        processRecoveryByProcess =
          Map.map clearProcessRecovery state.processRecoveryByProcess
      },
    [ applicationSessionRecoveryTimerAttempt
        session
        recovery.generation
        recovery.timerAttempt
    | (session, record) <- Map.toAscList state.sessionsById,
      recovery <- maybe [] pure record.recovery
    ]
  )
  where
    detach record = record {deliveryLive = False, recovery = Nothing}
    clearProcessRecovery recovery = case recovery.phase of
      ApplicationProcessSealed _ -> recovery
      _ -> recovery {phase = ApplicationProcessAvailable}

-- | Session-owned classification for the package-internal generic
-- @PublishValue@ seam.  Conflict carries no semantic candidate and therefore
-- cannot allocate an acceptance position or publication identity.
data ApplicationPublicationRequestClassification
  = FirstApplicationPublicationRequest ApplicationPublicationRequest
  | RetainedApplicationPublicationRequest ApplicationPublicationRequest
  | ConflictingApplicationPublicationRequest RequestId

-- | Opaque first/retry proof for one exact private typed call.  The captured
-- predecessor is the only application state which can be committed with the
-- semantic publication, preventing a caller from pairing request evidence with
-- an unrelated session or private-identity registry.
data ApplicationPublicationRequest
  = ApplicationPublicationRequest
      State
      Bool
      ApplicationSessionId
      ApplicationSessionBinding
      ProcessEpochId
      RequestId
      ApplicationPublicationCall
      ProcessAcceptancePosition

applicationPublicationRequestProcess ::
  ApplicationPublicationRequest -> ProcessEpochId
applicationPublicationRequestProcess
  (ApplicationPublicationRequest _ _ _ _ process _ _ _) = process

applicationPublicationRequestPrivateNabla ::
  ApplicationPublicationRequest -> PrivateNablaId
applicationPublicationRequestPrivateNabla
  ( ApplicationPublicationRequest
      _
      _
      _
      _
      _
      _
      (ApplicationPublicationCall privateNabla _)
      _
    ) = privateNabla

applicationPublicationRequestValue ::
  ApplicationPublicationRequest -> Application.ApplicationValue
applicationPublicationRequestValue
  ( ApplicationPublicationRequest
      _
      _
      _
      _
      _
      _
      (ApplicationPublicationCall _ value)
      _
    ) = value

applicationPublicationRequestPosition ::
  ApplicationPublicationRequest -> ProcessAcceptancePosition
applicationPublicationRequestPosition
  (ApplicationPublicationRequest _ _ _ _ _ _ _ position) = position

applicationPublicationRequestState :: ApplicationPublicationRequest -> State
applicationPublicationRequestState
  (ApplicationPublicationRequest predecessor _ _ _ _ _ _ _) = predecessor

applicationPublicationRequestIsRetry :: ApplicationPublicationRequest -> Bool
applicationPublicationRequestIsRetry
  (ApplicationPublicationRequest _ retained _ _ _ _ _ _) = retained

-- | Classify a package-private generic publication in the same live session and
-- request-ID namespace as ordinary EAPP calls.  Exact equality includes both
-- the private nabla handle and the complete application value.
classifyApplicationPublicationRequest ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  RequestId ->
  ApplicationPublicationCall ->
  State ->
  Either
    ApplicationSessionTransitionError
    ApplicationPublicationRequestClassification
classifyApplicationPublicationRequest session binding request call state = do
  sessionRecord <- checkedRequestSession session binding state
  case retiredRequestThrough sessionRecord request of
    Just _ -> rejectSession Session.ApplicationRequestAlreadyRetired
    Nothing -> Right ()
  let key = applicationRequestKey session request
  case Map.lookup request sessionRecord.publicationRequests of
    Just
      ( ApplicationPublicationRequestRecord
          _
          retainedCall
          retainedProcess
          retainedPosition
        )
        | retainedCall == call ->
            Right
              ( RetainedApplicationPublicationRequest
                  ( ApplicationPublicationRequest
                      state
                      True
                      session
                      binding
                      retainedProcess
                      request
                      retainedCall
                      retainedPosition
                  )
              )
        | otherwise ->
            Right (ConflictingApplicationPublicationRequest request)
    Nothing
      | Map.member request sessionRecord.requests ->
          Right (ConflictingApplicationPublicationRequest request)
      | Map.member request sessionRecord.environmentRequests ->
          Right (ConflictingApplicationPublicationRequest request)
      | detachedRequestClassAndOperation key state /= Nothing ->
          Right (ConflictingApplicationPublicationRequest request)
      | otherwise -> do
          nextOrdinal <-
            maybe
              ( sessionInvariant
                  (ApplicationAcceptanceCounterContradiction sessionRecord.processEpoch)
              )
              Right
              (Map.lookup sessionRecord.processEpoch state.nextAcceptanceByProcess)
          let position =
                Request.processAcceptancePosition
                  sessionRecord.processEpoch
                  nextOrdinal
          Right
            ( FirstApplicationPublicationRequest
                ( ApplicationPublicationRequest
                    state
                    False
                    session
                    binding
                    sessionRecord.processEpoch
                    request
                    call
                    position
                )
            )

-- | Complete application-owner successor for a successfully admitted generic
-- publication.  Exact retry retains the already current predecessor and does
-- not advance the shared process acceptance-position supply.
newtype PreparedApplicationPublicationRequest
  = PreparedApplicationPublicationRequest State

prepareApplicationPublicationRequestAcceptance ::
  ApplicationPublicationRequest ->
  PreparedApplicationPublicationRequest
prepareApplicationPublicationRequestAcceptance request =
  PreparedApplicationPublicationRequest
    (retainApplicationPublicationRequest request)

commitApplicationPublicationRequest ::
  PreparedApplicationPublicationRequest -> State
commitApplicationPublicationRequest
  (PreparedApplicationPublicationRequest successor) = successor

data EnvironmentRequestClassification
  = FirstEnvironmentRequest EnvironmentRequest
  | RetainedEnvironmentCandidate EnvironmentRequest
  | RetainedEnvironmentRequest EnvironmentRequest PendingEnvironment
  | RetainedCompletedEnvironment EnvironmentRequest CompletedEnvironment
  | ConflictingEnvironmentRequest RequestId

data EnvironmentRequestDisposition
  = FirstEnvironmentRequestDisposition
  | CandidateEnvironmentRequestDisposition
  | AcceptedEnvironmentRequestDisposition

-- | Opaque proof of one private environment request classification.  A first
-- call and retained candidate carry the currently available (but unconsumed)
-- position; an accepted retry carries the position already owned by its
-- retained manifest.
data EnvironmentRequest
  = EnvironmentRequest
      State
      EnvironmentRequestDisposition
      ApplicationSessionId
      ApplicationSessionBinding
      ProcessEpochId
      RequestId
      NewEnvironmentCoordinatorInput
      ProcessAcceptancePosition

environmentRequestSession :: EnvironmentRequest -> ApplicationSessionId
environmentRequestSession
  (EnvironmentRequest _ _ session _ _ _ _ _) = session

environmentRequestBinding ::
  EnvironmentRequest -> ApplicationSessionBinding
environmentRequestBinding
  (EnvironmentRequest _ _ _ binding _ _ _ _) = binding

environmentRequestProcess :: EnvironmentRequest -> ProcessEpochId
environmentRequestProcess
  (EnvironmentRequest _ _ _ _ process _ _ _) = process

environmentRequestId :: EnvironmentRequest -> RequestId
environmentRequestId
  (EnvironmentRequest _ _ _ _ _ request _ _) = request

environmentRequestPosition ::
  EnvironmentRequest -> ProcessAcceptancePosition
environmentRequestPosition
  (EnvironmentRequest _ _ _ _ _ _ _ position) = position

environmentRequestState :: EnvironmentRequest -> State
environmentRequestState
  (EnvironmentRequest predecessor _ _ _ _ _ _ _) = predecessor

environmentRequestIsRetry :: EnvironmentRequest -> Bool
environmentRequestIsRetry
  (EnvironmentRequest _ disposition _ _ _ _ _ _) =
    case disposition of
      FirstEnvironmentRequestDisposition -> False
      CandidateEnvironmentRequestDisposition -> True
      AcceptedEnvironmentRequestDisposition -> True

-- | Re-express a first or retained cursorless environment intention as the
-- ordinary public request candidate consumed by the live coordinator.
environmentRequestApplicationCandidate ::
  EnvironmentRequest ->
  Either ApplicationSessionTransitionError ApplicationRequestCandidate
environmentRequestApplicationCandidate request = do
  owner <- checkedRequestSession session binding predecessor
  case disposition of
    AcceptedEnvironmentRequestDisposition -> contradiction
    _ ->
      Right
        ( ApplicationRequestCandidate
            predecessor
            session
            binding
            process
            requestId
            NewEnvironmentApplication
            position
            (waitIdForRequest session requestId)
            (Request.nextApplicationReplyCursor owner.lastIssuedReplyCursor)
            False
            (ApplicationRequestCandidateReplyOwned owner.deliveryLive)
        )
  where
    EnvironmentRequest
      predecessor
      disposition
      session
      binding
      process
      requestId
      _
      position = request
    contradiction =
      sessionInvariant (ApplicationEnvironmentRequestContradiction requestId)

-- | Classify the direct semantic-owner input in the same session-local RequestId
-- namespace as public operations and the generic publication seam. Retaining
-- a candidate never reserves a process position; it is recalculated from the
-- current owner state on every re-drive.
classifyEnvironmentRequest ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  RequestId ->
  NewEnvironmentCoordinatorInput ->
  State ->
  Either ApplicationSessionTransitionError EnvironmentRequestClassification
classifyEnvironmentRequest session binding request input state = do
  sessionRecord <- checkedRequestSession session binding state
  case retiredRequestThrough sessionRecord request of
    Just _ -> rejectSession Session.ApplicationRequestAlreadyRetired
    Nothing -> Right ()
  let key = applicationRequestKey session request
      process = sessionRecord.processEpoch
      makeRequest disposition position =
        EnvironmentRequest
          state
          disposition
          session
          binding
          process
          request
          input
          position
      availablePosition = do
        nextOrdinal <-
          maybe
            (sessionInvariant (ApplicationAcceptanceCounterContradiction process))
            Right
            (Map.lookup process state.nextAcceptanceByProcess)
        Right (Request.processAcceptancePosition process nextOrdinal)
  case Map.lookup request sessionRecord.requests of
    Just retained
      | retained.call /= NewEnvironmentApplication ->
          Right (ConflictingEnvironmentRequest request)
      | retained.processEpoch /= process ->
          contradiction
      | otherwise -> case (retained.acceptancePosition, retained.replyBody) of
          ( Just position,
            OperationAccepted retainedRequest EnvironmentStabilizationPending
            )
              | retainedRequest == request -> do
                  pending <-
                    maybe contradiction Right (Map.lookup position state.pendingEnvironments)
                  let expectedReply = Environment.environmentReplyKey session request
                      manifest = Environment.pendingEnvironmentManifest pending
                  if Environment.environmentManifestProcess manifest == process
                    && Environment.environmentManifestPosition manifest == position
                    && Environment.pendingEnvironmentReplyKey pending == Just expectedReply
                    then
                      Right
                        ( RetainedEnvironmentRequest
                            (makeRequest AcceptedEnvironmentRequestDisposition position)
                            pending
                        )
                    else contradiction
          ( Just position,
            Completed retainedRequest (NewEnvironmentCompleted access)
            )
              | retainedRequest == request -> do
                  completed <-
                    maybe contradiction Right (Map.lookup position state.completedEnvironments)
                  let expectedReply = Environment.environmentReplyKey session request
                      manifest = Environment.completedEnvironmentManifest completed
                  if Environment.environmentManifestProcess manifest == process
                    && Environment.environmentManifestPosition manifest == position
                    && Environment.completedEnvironmentReplyKey completed == Just expectedReply
                    && Environment.completedEnvironmentAccess completed == Just access
                    then
                      Right
                        ( RetainedCompletedEnvironment
                            (makeRequest AcceptedEnvironmentRequestDisposition position)
                            completed
                        )
                    else contradiction
          _ -> contradiction
    Nothing -> case Map.lookup request sessionRecord.environmentRequests of
      Just (EnvironmentRequestRecord _ retainedInput retainedProcess retainedStage)
        | retainedInput /= input ->
            Right (ConflictingEnvironmentRequest request)
        | retainedProcess /= process -> contradiction
        | retainedStage /= EnvironmentRequestCandidateStage -> contradiction
        | otherwise -> do
            position <- availablePosition
            Right
              ( RetainedEnvironmentCandidate
                  (makeRequest CandidateEnvironmentRequestDisposition position)
              )
      Nothing
        | Map.member request sessionRecord.publicationRequests ->
            Right (ConflictingEnvironmentRequest request)
        | detachedRequestClassAndOperation key state /= Nothing ->
            Right (ConflictingEnvironmentRequest request)
        | otherwise -> do
            position <- availablePosition
            Right
              ( FirstEnvironmentRequest
                  (makeRequest FirstEnvironmentRequestDisposition position)
              )
  where
    contradiction :: Either ApplicationSessionTransitionError value
    contradiction =
      sessionInvariant (ApplicationEnvironmentRequestContradiction request)

data EnvironmentRequestAcceptanceError
  = EnvironmentRequestCannotRetainAcceptedCandidate
  | EnvironmentRequestCandidateMissing RequestId
  | EnvironmentRequestCandidateConflict RequestId
  | EnvironmentRequestPositionStale
      ProcessAcceptancePosition
      ProcessAcceptancePosition
  | EnvironmentRequestManifestProcessMismatch ProcessEpochId ProcessEpochId
  | EnvironmentRequestManifestPositionMismatch
      ProcessAcceptancePosition
      ProcessAcceptancePosition
  | EnvironmentRequestManifestConflict ProcessAcceptancePosition
  | EnvironmentRequestPositionConflict ProcessAcceptancePosition
  deriving stock (Eq, Show)

newtype PreparedEnvironmentRequestCandidate
  = PreparedEnvironmentRequestCandidate State

-- | Retain an unaccepted intention while a transient prerequisite is absent.
-- No identity, acceptance position, cursor, alias, or semantic owner state is
-- allocated by this transition.
prepareEnvironmentRequestCandidate ::
  EnvironmentRequest ->
  Either EnvironmentRequestAcceptanceError PreparedEnvironmentRequestCandidate
prepareEnvironmentRequestCandidate request =
  case disposition of
    AcceptedEnvironmentRequestDisposition ->
      Left EnvironmentRequestCannotRetainAcceptedCandidate
    CandidateEnvironmentRequestDisposition ->
      case Map.lookup requestId sessionRecord.environmentRequests of
        Just retained
          | retained == candidateRecord ->
              Right (PreparedEnvironmentRequestCandidate predecessor)
          | otherwise -> Left (EnvironmentRequestCandidateConflict requestId)
        Nothing -> Left (EnvironmentRequestCandidateMissing requestId)
    FirstEnvironmentRequestDisposition ->
      case Map.lookup requestId sessionRecord.environmentRequests of
        Nothing ->
          Right
            ( PreparedEnvironmentRequestCandidate
                predecessor
                  { sessionsById =
                      Map.adjust retain session predecessor.sessionsById
                  }
            )
        Just _ -> Left (EnvironmentRequestCandidateConflict requestId)
  where
    EnvironmentRequest
      predecessor
      disposition
      session
      _
      process
      requestId
      input
      _ = request
    sessionRecord =
      case Map.lookup session predecessor.sessionsById of
        Just retained -> retained
        Nothing -> error "classified environment request lost its live session"
    candidateRecord =
      EnvironmentRequestRecord
        requestId
        input
        process
        EnvironmentRequestCandidateStage
    retain retained =
      retained
        { environmentRequests =
            Map.insert requestId candidateRecord retained.environmentRequests
        }

commitEnvironmentRequestCandidate ::
  PreparedEnvironmentRequestCandidate -> State
commitEnvironmentRequestCandidate
  (PreparedEnvironmentRequestCandidate successor) = successor

-- | Canonical keys of every reply-less environment candidate. The process is
-- included in the ordering so redrive remains deterministic across sessions.
applicationEnvironmentCandidateKeys :: State -> [ApplicationRequestKey]
applicationEnvironmentCandidateKeys state =
  fmap (\(_, key) -> key) . sort
    $ [ ( process,
          applicationRequestKey session request
        )
      | (session, owner) <- Map.toAscList state.sessionsById,
        (request, EnvironmentRequestRecord _ _ process stage) <-
          Map.toAscList owner.environmentRequests,
        stage == EnvironmentRequestCandidateStage
      ]

-- | Retain one first-seen public @newenv@ while a transient structural
-- prerequisite is absent. No position, cursor, or reply is allocated.
prepareApplicationEnvironmentCandidate ::
  ApplicationRequestCandidate ->
  Either EnvironmentRequestAcceptanceError PreparedEnvironmentRequestCandidate
prepareApplicationEnvironmentCandidate candidate
  | applicationRequestCandidateCall candidate /= NewEnvironmentApplication =
      Left (EnvironmentRequestCandidateConflict request)
  | otherwise =
      case Map.lookup session predecessor.sessionsById of
        Nothing -> Left (EnvironmentRequestCandidateMissing request)
        Just owner -> case Map.lookup request owner.environmentRequests of
          Nothing ->
            Right
              ( PreparedEnvironmentRequestCandidate
                  predecessor
                    { sessionsById =
                        Map.adjust retain session predecessor.sessionsById
                    }
              )
          Just retained
            | retained == candidateRecord ->
                Right (PreparedEnvironmentRequestCandidate predecessor)
            | otherwise -> Left (EnvironmentRequestCandidateConflict request)
  where
    predecessor = applicationRequestCandidateState candidate
    session = applicationRequestCandidateSession candidate
    request = applicationRequestCandidateId candidate
    process = applicationRequestCandidateProcess candidate
    candidateRecord =
      EnvironmentRequestRecord
        request
        newEnvironmentCoordinatorInput
        process
        EnvironmentRequestCandidateStage
    retain owner =
      owner
        { environmentRequests =
            Map.insert request candidateRecord owner.environmentRequests
        }

-- | Remove one retained candidate and reconstruct its current ordinary request
-- candidate. A subsequent failed readiness check may insert it again without
-- consuming any owner coordinate.
applicationEnvironmentCandidateForRedrive ::
  ApplicationRequestKey ->
  State ->
  Either ApplicationSessionTransitionError ApplicationRequestCandidate
applicationEnvironmentCandidateForRedrive key state = do
  owner <- liveSession session state
  checkedLiveRecord owner state
  retained <-
    maybe
      (sessionInvariant (ApplicationDetachedRequestContradiction key))
      Right
      (Map.lookup request owner.environmentRequests)
  process <- case retained of
    EnvironmentRequestRecord retainedRequest retainedInput retainedProcess EnvironmentRequestCandidateStage
      | retainedRequest == request,
        retainedInput == newEnvironmentCoordinatorInput,
        retainedProcess == owner.processEpoch ->
          Right retainedProcess
    _ -> sessionInvariant (ApplicationDetachedRequestContradiction key)
  let withoutCandidate =
        state
          { sessionsById =
              Map.adjust
                ( \record ->
                    record
                      { environmentRequests =
                          Map.delete request record.environmentRequests
                      }
                )
                session
                state.sessionsById
          }
  attachedFreshCandidate key process NewEnvironmentApplication withoutCandidate
  where
    session = applicationRequestKeySession key
    request = applicationRequestKeyRequest key

data PreparedEnvironmentRequestAcceptance
  = PreparedEnvironmentRequestAcceptance State ApplicationRequestReply

-- | Promote one ready public @newenv@ candidate into the ordinary retained
-- request owner and the environment manifest relation as one transition.
prepareApplicationEnvironmentAcceptance ::
  ApplicationRequestCandidate ->
  Environment.EnvironmentManifest ->
  Either EnvironmentRequestAcceptanceError PreparedEnvironmentRequestAcceptance
prepareApplicationEnvironmentAcceptance candidate manifest
  | applicationRequestCandidateCall candidate /= NewEnvironmentApplication =
      Left
        ( EnvironmentRequestCandidateConflict
            (applicationRequestCandidateId candidate)
        )
  | otherwise = prepareEnvironmentRequestAcceptance request manifest
  where
    disposition =
      case Map.lookup
        (applicationRequestCandidateSession candidate)
        (applicationRequestCandidateState candidate).sessionsById
        >>= Map.lookup
          (applicationRequestCandidateId candidate)
          . (.environmentRequests) of
        Just _ -> CandidateEnvironmentRequestDisposition
        Nothing -> FirstEnvironmentRequestDisposition
    request =
      EnvironmentRequest
        (applicationRequestCandidateState candidate)
        disposition
        (applicationRequestCandidateSession candidate)
        (applicationRequestCandidateBinding candidate)
        (applicationRequestCandidateProcess candidate)
        (applicationRequestCandidateId candidate)
        newEnvironmentCoordinatorInput
        (applicationRequestCandidatePosition candidate)

-- | Atomically attach one complete publication-owned manifest to its process
-- position.  Exact accepted retry is a no-op and requires byte-for-byte typed
-- equality with the retained manifest.
prepareEnvironmentRequestAcceptance ::
  EnvironmentRequest ->
  Environment.EnvironmentManifest ->
  Either
    EnvironmentRequestAcceptanceError
    PreparedEnvironmentRequestAcceptance
prepareEnvironmentRequestAcceptance request manifest = do
  requireEnvironmentManifestAgreement request manifest
  case disposition of
    AcceptedEnvironmentRequestDisposition -> do
      retained <-
        maybe
          (Left (EnvironmentRequestManifestConflict position))
          Right
          (Map.lookup position predecessor.pendingEnvironments)
      requestRecord <-
        maybe
          (Left (EnvironmentRequestCandidateMissing requestId))
          Right
          ( Map.lookup session predecessor.sessionsById
              >>= Map.lookup requestId . (.requests)
          )
      if Environment.pendingEnvironmentManifest retained == manifest
        && requestRecord.call == NewEnvironmentApplication
        && requestRecord.processEpoch == process
        && requestRecord.acceptancePosition == Just position
        && requestRecord.replyBody
          == OperationAccepted requestId EnvironmentStabilizationPending
        then
          Right
            ( PreparedEnvironmentRequestAcceptance
                predecessor
                (requestRecordReply requestRecord)
            )
        else Left (EnvironmentRequestManifestConflict position)
    FirstEnvironmentRequestDisposition -> retainAccepted Nothing
    CandidateEnvironmentRequestDisposition -> retainAccepted (Just candidateRecord)
  where
    EnvironmentRequest
      predecessor
      disposition
      session
      binding
      process
      requestId
      input
      position = request
    candidateRecord =
      EnvironmentRequestRecord
        requestId
        input
        process
        EnvironmentRequestCandidateStage
    retainedPending =
      Environment.pendingEnvironment
        manifest
        (Just (Environment.environmentReplyKey session requestId))
    retainAccepted expectedCandidate = do
      sessionRecord <-
        maybe
          (Left (EnvironmentRequestCandidateMissing requestId))
          Right
          (Map.lookup session predecessor.sessionsById)
      case (expectedCandidate, Map.lookup requestId sessionRecord.environmentRequests) of
        (Nothing, Nothing) -> Right ()
        (Just expected, Just actual)
          | expected == actual -> Right ()
        (Just _, Nothing) -> Left (EnvironmentRequestCandidateMissing requestId)
        _ -> Left (EnvironmentRequestCandidateConflict requestId)
      if Map.member position predecessor.pendingEnvironments
        || Map.member position predecessor.completedEnvironments
        then Left (EnvironmentRequestPositionConflict position)
        else Right ()
      nextOrdinal <-
        maybe
          (Left (EnvironmentRequestCandidateMissing requestId))
          Right
          (Map.lookup process predecessor.nextAcceptanceByProcess)
      let available = Request.processAcceptancePosition process nextOrdinal
      if available /= position
        then Left (EnvironmentRequestPositionStale position available)
        else Right ()
      let genericCandidate =
            ApplicationRequestCandidate
              predecessor
              session
              binding
              process
              requestId
              NewEnvironmentApplication
              position
              (waitIdForRequest session requestId)
              (Request.nextApplicationReplyCursor sessionRecord.lastIssuedReplyCursor)
              False
              (ApplicationRequestCandidateReplyOwned sessionRecord.deliveryLive)
          preparedRequest =
            prepareApplicationOperationAcceptance
              genericCandidate
              EnvironmentStabilizationPending
          (accepted, reply) = commitApplicationRequest preparedRequest
          detachCandidate retained =
            retained
              { environmentRequests =
                  Map.delete requestId retained.environmentRequests
              }
          successor =
            accepted
              { sessionsById =
                  Map.adjust detachCandidate session accepted.sessionsById,
                pendingEnvironments =
                  Map.insert position retainedPending accepted.pendingEnvironments
              }
      Right (PreparedEnvironmentRequestAcceptance successor reply)

preparedEnvironmentRequestAcceptanceReply ::
  PreparedEnvironmentRequestAcceptance -> ApplicationRequestReply
preparedEnvironmentRequestAcceptanceReply
  (PreparedEnvironmentRequestAcceptance _ reply) = reply

commitEnvironmentRequestAcceptance ::
  PreparedEnvironmentRequestAcceptance -> State
commitEnvironmentRequestAcceptance
  (PreparedEnvironmentRequestAcceptance successor _) = successor

requireEnvironmentManifestAgreement ::
  EnvironmentRequest ->
  Environment.EnvironmentManifest ->
  Either EnvironmentRequestAcceptanceError ()
requireEnvironmentManifestAgreement request manifest
  | actualProcess /= expectedProcess =
      Left
        ( EnvironmentRequestManifestProcessMismatch
            expectedProcess
            actualProcess
        )
  | actualPosition /= expectedPosition =
      Left
        ( EnvironmentRequestManifestPositionMismatch
            expectedPosition
            actualPosition
        )
  | otherwise = Right ()
  where
    expectedProcess = environmentRequestProcess request
    actualProcess = Environment.environmentManifestProcess manifest
    expectedPosition = environmentRequestPosition request
    actualPosition = Environment.environmentManifestPosition manifest

-- | Leaf-local contradictions while converting an already structurally
-- covered environment into process-private access.  Structural coverage and
-- membership lineage are coordinator proofs and therefore do not occur here.
data EnvironmentSettlementError
  = EnvironmentSettlementMissing ProcessAcceptancePosition
  | EnvironmentSettlementManifestConflict ProcessAcceptancePosition
  | EnvironmentSettlementOwnerConflict ProcessAcceptancePosition
  | EnvironmentSettlementEvidenceConflict ProcessAcceptancePosition
  | EnvironmentSettlementReplyContradiction Environment.EnvironmentReplyKey
  | EnvironmentSettlementProcessContradiction ProcessEpochId
  | EnvironmentSettlementPrivateIdentityContradiction
      ProcessEpochId
      PrivateIdentityError
  | EnvironmentSettlementCanonicalAccessContradiction
      ProcessEpochId
      ApplicationSessionInvariant
  deriving stock (Eq, Show)

data EnvironmentSettlementDisposition
  = EnvironmentSettlementApplied
  | EnvironmentSettlementExactRetry
  deriving stock (Eq, Ord, Show)

-- | One complete access-only successor.  The prepared product exposes only its
-- terminal record and retry disposition; no partially localized identity map
-- can be committed by a caller.
data PreparedEnvironmentSettlement
  = PreparedEnvironmentSettlement
      State
      CompletedEnvironment
      EnvironmentSettlementDisposition
      ApplicationOperationCompletionOutcome

-- | Finalize one exact retained manifest after the coordinator has proved its
-- covering established cut.  This transition mutates only Application-owned
-- aliases, request reachability, and the pending/completed relation.
--
-- A live process localizes roots in immutable manifest order and reuses the
-- startup access check for the canonical six writer/reader pairs.  A process
-- already retired by End records global completion without rebuilding its map
-- or allocating aliases.  Exact replay is state-identical.
-- | Append the checked wiring phase to the existing retry identity. Reply
-- ownership is preserved, including a session that detached meanwhile.
expandPendingEnvironment ::
  Environment.EnvironmentManifest -> State -> Either EnvironmentSettlementError State
expandPendingEnvironment manifest state = case Map.lookup position state.pendingEnvironments of
  Nothing -> Left (EnvironmentSettlementMissing position)
  Just pending
    | Environment.environmentManifestExtends manifest (Environment.pendingEnvironmentManifest pending) ->
        Right
          state
            { pendingEnvironments =
                Map.insert
                  position
                  (Environment.pendingEnvironment manifest (Environment.pendingEnvironmentReplyKey pending))
                  state.pendingEnvironments
            }
    | otherwise -> Left (EnvironmentSettlementManifestConflict position)
  where
    position = Environment.environmentManifestPosition manifest

prepareEnvironmentSettlement ::
  Environment.EnvironmentSettlementEvidence ->
  Environment.EnvironmentManifest ->
  State ->
  Either EnvironmentSettlementError PreparedEnvironmentSettlement
prepareEnvironmentSettlement evidence manifest predecessor =
  case (Map.lookup position predecessor.pendingEnvironments, Map.lookup position predecessor.completedEnvironments) of
    (Nothing, Nothing) -> Left (EnvironmentSettlementMissing position)
    (Just _, Just _) -> Left (EnvironmentSettlementOwnerConflict position)
    (Nothing, Just completed)
      | Environment.completedEnvironmentManifest completed /= manifest ->
          Left (EnvironmentSettlementManifestConflict position)
      | Environment.completedEnvironmentSettlementEvidence completed /= evidence ->
          Left (EnvironmentSettlementEvidenceConflict position)
      | otherwise ->
          Right
            ( PreparedEnvironmentSettlement
                predecessor
                completed
                EnvironmentSettlementExactRetry
                (ApplicationOperationCompleted Nothing)
            )
    (Just pending, Nothing)
      | Environment.pendingEnvironmentManifest pending /= manifest ->
          Left (EnvironmentSettlementManifestConflict position)
      | otherwise -> settlePendingEnvironment pending
  where
    position = Environment.environmentManifestPosition manifest
    process = Environment.environmentManifestProcess manifest

    settlePendingEnvironment pending = do
      (successorIdentity, access) <- prepareEnvironmentAliases process manifest predecessor
      (successorSessions, completionOutcome) <-
        completeEnvironmentReplyOwner
          position
          process
          (Environment.pendingEnvironmentReplyKey pending)
          access
          predecessor.sessionsById
      let completed = Environment.completePendingEnvironment evidence access pending
          successor =
            predecessor
              { privateIdentity = successorIdentity,
                sessionsById = successorSessions,
                pendingEnvironments = Map.delete position predecessor.pendingEnvironments,
                completedEnvironments =
                  Map.insert position completed predecessor.completedEnvironments
              }
      Right
        ( PreparedEnvironmentSettlement
            successor
            completed
            EnvironmentSettlementApplied
            completionOutcome
        )

preparedEnvironmentSettlementDisposition ::
  PreparedEnvironmentSettlement -> EnvironmentSettlementDisposition
preparedEnvironmentSettlementDisposition
  (PreparedEnvironmentSettlement _ _ disposition _) = disposition

preparedEnvironmentSettlementCompletion ::
  PreparedEnvironmentSettlement -> CompletedEnvironment
preparedEnvironmentSettlementCompletion
  (PreparedEnvironmentSettlement _ completed _ _) = completed

preparedEnvironmentSettlementCompletionOutcome ::
  PreparedEnvironmentSettlement -> ApplicationOperationCompletionOutcome
preparedEnvironmentSettlementCompletionOutcome
  (PreparedEnvironmentSettlement _ _ _ outcome) = outcome

commitEnvironmentSettlement ::
  PreparedEnvironmentSettlement -> (State, CompletedEnvironment)
commitEnvironmentSettlement
  (PreparedEnvironmentSettlement successor completed _ _) =
    (successor, completed)

prepareEnvironmentAliases ::
  ProcessEpochId ->
  Environment.EnvironmentManifest ->
  State ->
  Either
    EnvironmentSettlementError
    (PrivateIdentity, Maybe EnvironmentAccess)
prepareEnvironmentAliases process manifest state =
  case Map.lookup process state.accessByProcess of
    Nothing ->
      if Map.member process state.nextAcceptanceByProcess
        || any ((== process) . (.processEpoch)) (Map.elems state.sessionsById)
        then Left (EnvironmentSettlementProcessContradiction process)
        else Right (state.privateIdentity, Nothing)
    Just bootstrap
      | bootstrap.processEpoch /= process
          || Map.notMember process state.nextAcceptanceByProcess ->
          Left (EnvironmentSettlementProcessContradiction process)
      | otherwise -> do
          (successorIdentity, reversedRoots, reversedWiring) <-
            foldM
              (localizeEnvironmentRoot process)
              (state.privateIdentity, [], [])
              (NonEmpty.toList (Environment.environmentManifestRoots manifest))
          pairs <-
            either
              (Left . EnvironmentSettlementCanonicalAccessContradiction process)
              Right
              (collectRootPairs process (reverse reversedRoots))
          (hub, edges) <- case reverse reversedWiring of
            hub : edges -> Right (hub, edges)
            [] -> Left (EnvironmentSettlementCanonicalAccessContradiction process (ApplicationSessionBootstrapContradiction process))
          access <-
            either
              (Left . EnvironmentSettlementCanonicalAccessContradiction process . ApplicationSessionStartupAccessContradiction)
              Right
              (environmentAccess pairs hub edges)
          Right (successorIdentity, Just access)

localizeEnvironmentRoot ::
  ProcessEpochId ->
  (PrivateIdentity, [RootAccess], [PrivateObjectId]) ->
  Environment.PositionedEnvironmentRoot ->
  Either EnvironmentSettlementError (PrivateIdentity, [RootAccess], [PrivateObjectId])
localizeEnvironmentRoot process (identityState, accumulated, wiring) positioned = do
  let plan = Environment.positionedEnvironmentRootPlan positioned
      slot = Environment.environmentRootPlanSlot plan
      globalIdentity =
        globalUniqueIdFromGlobalObjectId
          (Environment.environmentRootPlanTargetObject plan)
  (successor, privateIdentity) <-
    commitLocalization
      <$> either
        ( Left
            . EnvironmentSettlementPrivateIdentityContradiction process
        )
        Right
        (prepareLocalization process globalIdentity identityState)
  case environmentRootClaimView (environmentRootSlotClaim slot) of
    EnvironmentWriterRootClaimView role sequencing ->
      Right (successor, WriterAccess role (asPrivateNablaId privateIdentity) sequencing : accumulated, wiring)
    EnvironmentReaderRootClaimView role ->
      Right (successor, ReaderAccess role (asPrivateDeltaId privateIdentity) : accumulated, wiring)
    EnvironmentHubRootClaimView -> Right (successor, accumulated, asPrivateObjectId privateIdentity : wiring)
    EnvironmentEdgeRootClaimView _ _ -> Right (successor, accumulated, asPrivateObjectId privateIdentity : wiring)

completeEnvironmentReplyOwner ::
  ProcessAcceptancePosition ->
  ProcessEpochId ->
  Maybe Environment.EnvironmentReplyKey ->
  Maybe EnvironmentAccess ->
  Map ApplicationSessionId SessionRecord ->
  Either
    EnvironmentSettlementError
    (Map ApplicationSessionId SessionRecord, ApplicationOperationCompletionOutcome)
completeEnvironmentReplyOwner position _ Nothing _ sessions =
  Right (sessions, ApplicationOperationNoLiveRequest position)
completeEnvironmentReplyOwner position process (Just reply) maybeAccess sessions = do
  access <-
    maybe
      (Left (EnvironmentSettlementReplyContradiction reply))
      Right
      maybeAccess
  sessionRecord <-
    maybe
      (Left (EnvironmentSettlementReplyContradiction reply))
      Right
      (Map.lookup session sessions)
  if sessionRecord.processEpoch /= process
    then Left (EnvironmentSettlementReplyContradiction reply)
    else case Map.lookup request sessionRecord.requests of
      Just retained
        | retained.requestId == request,
          retained.call == NewEnvironmentApplication,
          retained.processEpoch == process,
          retained.acceptancePosition == Just position,
          retained.replyBody
            == OperationAccepted request EnvironmentStabilizationPending ->
            let cursor =
                  Request.nextApplicationReplyCursor sessionRecord.lastIssuedReplyCursor
                result = NewEnvironmentCompleted access
                completedRecord =
                  retained
                    { replyCursor = cursor,
                      replyBody = Completed request result
                    }
                completedSession =
                  sessionRecord
                    { lastIssuedReplyCursor = cursor,
                      requests =
                        Map.insert request completedRecord sessionRecord.requests
                    }
                observation
                  | sessionRecord.deliveryLive =
                      Just
                        ( ApplicationOperationCompletionObservation
                            sessionRecord.binding
                            cursor
                            request
                            result
                        )
                  | otherwise = Nothing
             in Right
                  ( Map.insert session completedSession sessions,
                    ApplicationOperationCompleted observation
                  )
      _ -> Left (EnvironmentSettlementReplyContradiction reply)
  where
    session = Environment.environmentReplyKeySession reply
    request = Environment.environmentReplyKeyRequest reply

-- | Classification performed before any semantic owner is prepared.
data ApplicationRequestClassification
  = FirstApplicationRequest ApplicationRequestCandidate
  | RetainedApplicationRequest ApplicationRequestReply
  | AttachedApplicationRequest ApplicationRequestClass
  | ConflictingApplicationRequest ApplicationRequestReply

data ApplicationRequestCandidateReply
  = ApplicationRequestCandidateReplyDetached
  | ApplicationRequestCandidateReplyOwned Bool

-- | Opaque proof that a request ID is new in the exact current session binding.
-- It captures the one process position that a successful semantic admission may
-- claim; rejecting the candidate leaves that position available.
data ApplicationRequestCandidate
  = ApplicationRequestCandidate
      State
      ApplicationSessionId
      ApplicationSessionBinding
      ProcessEpochId
      RequestId
      ApplicationOperation
      ProcessAcceptancePosition
      WaitId
      ApplicationReplyCursor
      Bool
      ApplicationRequestCandidateReply

applicationRequestCandidateSession ::
  ApplicationRequestCandidate ->
  ApplicationSessionId
applicationRequestCandidateSession
  (ApplicationRequestCandidate _ session _ _ _ _ _ _ _ _ _) = session

applicationRequestCandidateBinding ::
  ApplicationRequestCandidate ->
  ApplicationSessionBinding
applicationRequestCandidateBinding
  (ApplicationRequestCandidate _ _ binding _ _ _ _ _ _ _ _) = binding

applicationRequestCandidateProcess ::
  ApplicationRequestCandidate ->
  ProcessEpochId
applicationRequestCandidateProcess
  (ApplicationRequestCandidate _ _ _ process _ _ _ _ _ _ _) = process

applicationRequestCandidateId :: ApplicationRequestCandidate -> RequestId
applicationRequestCandidateId
  (ApplicationRequestCandidate _ _ _ _ request _ _ _ _ _ _) = request

applicationRequestCandidateCall ::
  ApplicationRequestCandidate ->
  ApplicationOperation
applicationRequestCandidateCall
  (ApplicationRequestCandidate _ _ _ _ _ call _ _ _ _ _) = call

applicationRequestCandidatePosition ::
  ApplicationRequestCandidate ->
  ProcessAcceptancePosition
applicationRequestCandidatePosition
  (ApplicationRequestCandidate _ _ _ _ _ _ position _ _ _ _) = position

applicationRequestCandidateWaitId :: ApplicationRequestCandidate -> WaitId
applicationRequestCandidateWaitId
  (ApplicationRequestCandidate _ _ _ _ _ _ _ wait _ _ _) = wait

applicationRequestCandidateState :: ApplicationRequestCandidate -> State
applicationRequestCandidateState = applicationRequestCandidatePredecessor

applicationRequestCandidateReplyOwned :: ApplicationRequestCandidate -> Bool
applicationRequestCandidateReplyOwned candidate =
  case applicationRequestCandidateReply candidate of
    ApplicationRequestCandidateReplyDetached -> False
    ApplicationRequestCandidateReplyOwned _ -> True

-- | Whether the retained reply owner also has a live delivery path in this
-- transition. A disconnected session remains an owner for resume even though
-- no direct reply effect may be emitted.
applicationRequestCandidateReplyDeliverable ::
  ApplicationRequestCandidate -> Bool
applicationRequestCandidateReplyDeliverable candidate =
  case applicationRequestCandidateReply candidate of
    ApplicationRequestCandidateReplyDetached -> False
    ApplicationRequestCandidateReplyOwned deliverable -> deliverable

applicationRequestCandidateReply ::
  ApplicationRequestCandidate -> ApplicationRequestCandidateReply
applicationRequestCandidateReply
  (ApplicationRequestCandidate _ _ _ _ _ _ _ _ _ _ reply) = reply

-- | Classify exact typed-call retry or conflicting request-ID reuse before any
-- semantic preparation.  The separately supplied session must name the exact
-- current live binding.
classifyApplicationRequest ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  RequestId ->
  ApplicationOperation ->
  State ->
  Either ApplicationSessionTransitionError ApplicationRequestClassification
classifyApplicationRequest session binding request call state = do
  sessionRecord <- checkedRequestSession session binding state
  let key = applicationRequestKey session request
  case retiredRequestThrough sessionRecord request of
    Just through -> Right (RetainedApplicationRequest (RequestRetired request through))
    Nothing ->
      if Map.member request sessionRecord.publicationRequests
        then
          Right
            (ConflictingApplicationRequest (RequestConflict request))
        else case Map.lookup request sessionRecord.requests of
          Just retained
            | retained.call == call ->
                Right
                  (RetainedApplicationRequest (requestRecordReply retained))
            | otherwise ->
                Right
                  (ConflictingApplicationRequest (RequestConflict request))
          Nothing -> case detachedRequestClassAndOperation key state of
            Just (retainedClass, retainedCall)
              | retainedCall == call ->
                  Right (AttachedApplicationRequest retainedClass)
              | otherwise ->
                  Right
                    (ConflictingApplicationRequest (RequestConflict request))
            Nothing -> do
              nextOrdinal <-
                maybe
                  ( sessionInvariant
                      (ApplicationAcceptanceCounterContradiction sessionRecord.processEpoch)
                  )
                  Right
                  (Map.lookup sessionRecord.processEpoch state.nextAcceptanceByProcess)
              let position =
                    Request.processAcceptancePosition
                      sessionRecord.processEpoch
                      nextOrdinal
                  wait =
                    waitIdForRequest sessionRecord.sessionId request
                  cursor =
                    Request.nextApplicationReplyCursor
                      sessionRecord.lastIssuedReplyCursor
              Right
                ( FirstApplicationRequest
                    ( ApplicationRequestCandidate
                        state
                        session
                        binding
                        sessionRecord.processEpoch
                        request
                        call
                        position
                        wait
                        cursor
                        False
                        (ApplicationRequestCandidateReplyOwned True)
                    )
                )

retainApplicationPublicationRequest ::
  ApplicationPublicationRequest -> State
retainApplicationPublicationRequest
  (ApplicationPublicationRequest predecessor retained session _ process requestId call position)
    | retained = predecessor
    | otherwise =
        predecessor
          { sessionsById =
              Map.adjust retainInSession session predecessor.sessionsById,
            nextAcceptanceByProcess =
              Map.adjust (+ 1) process predecessor.nextAcceptanceByProcess
          }
    where
      record =
        ApplicationPublicationRequestRecord
          requestId
          call
          process
          position
      retainInSession sessionRecord =
        sessionRecord
          { publicationRequests =
              Map.insert
                requestId
                record
                sessionRecord.publicationRequests
          }

-- | Extract the immutable retry/conflict response, if classification did not
-- produce a first-seen semantic candidate.
applicationRequestClassificationReply ::
  ApplicationRequestClassification ->
  Maybe ApplicationRequestReply
applicationRequestClassificationReply = \case
  FirstApplicationRequest _ -> Nothing
  RetainedApplicationRequest reply -> Just reply
  AttachedApplicationRequest _ -> Nothing
  ConflictingApplicationRequest reply -> Just reply

detachedRequestClassAndOperation ::
  ApplicationRequestKey ->
  State ->
  Maybe (ApplicationRequestClass, ApplicationOperation)
detachedRequestClassAndOperation key state =
  case Map.lookup key state.labelCandidates of
    Just (LabelCandidateRecord _ call) ->
      Just
        ( LabelCandidateApplicationRequestClass,
          labelApplicationOperation call
        )
    Nothing -> case lookupEnvironmentCandidate key state of
      Just _ ->
        Just
          ( EnvironmentCandidateApplicationRequestClass,
            NewEnvironmentApplication
          )
      Nothing -> case Map.lookup key state.gatedIngress of
        Just (GatedIngressRecord _ operation _) ->
          Just (GatedIngressApplicationRequestClass, operation)
        Nothing ->
          ( \(FenceHeldRecord _ operation _ _ _ _) ->
              (FenceHeldApplicationRequestClass, operation)
          )
            <$> findFenceHeldByReplyKey key state.acceptedFenceHeld

lookupEnvironmentCandidate ::
  ApplicationRequestKey -> State -> Maybe EnvironmentRequestRecord
lookupEnvironmentCandidate key state = do
  session <- Map.lookup (applicationRequestKeySession key) state.sessionsById
  retained <- Map.lookup (applicationRequestKeyRequest key) session.environmentRequests
  case retained of
    EnvironmentRequestRecord _ _ _ EnvironmentRequestCandidateStage -> Just retained
    _ -> Nothing

findFenceHeldByReplyKey ::
  ApplicationRequestKey ->
  Map LabelProcessAcceptancePosition FenceHeldRecord ->
  Maybe FenceHeldRecord
findFenceHeldByReplyKey wanted =
  Map.foldr
    ( \held@(FenceHeldRecord _ _ _ key replyLink _) found ->
        case found of
          Just _ -> found
          Nothing
            | replyLink /= Nothing && key == wanted -> Just held
            | otherwise -> Nothing
    )
    Nothing

-- | Complete application-owner successor and newly retained direct reply.
data PreparedApplicationRequest
  = PreparedApplicationRequest State ApplicationRequestReply

-- | Retain a first-seen semantic rejection without consuming its proposed
-- process acceptance position.
prepareApplicationRequestRejection ::
  ApplicationRequestCandidate ->
  ApplicationRejection ->
  PreparedApplicationRequest
prepareApplicationRequestRejection candidate rejection =
  retainApplicationRequest
    False
    candidate
    (Rejected (applicationRequestCandidateId candidate) rejection)

-- | Retain a successfully completed request and consume its one process
-- acceptance position.
prepareApplicationRequestCompletion ::
  ApplicationRequestCandidate ->
  RegularCallResult ->
  PreparedApplicationRequest
prepareApplicationRequestCompletion candidate result =
  retainApplicationRequest
    True
    candidate
    (Completed (applicationRequestCandidateId candidate) result)

-- | Retain a successfully admitted operation whose semantic result depends on
-- a later structural-stabilization event.  Acceptance consumes the candidate's
-- one process position exactly as an immediate completion does; the eventual
-- completion updates this same retained request rather than admitting another
-- call.
prepareApplicationOperationAcceptance ::
  ApplicationRequestCandidate ->
  OperationPendingReason ->
  PreparedApplicationRequest
prepareApplicationOperationAcceptance candidate reason =
  retainApplicationRequest
    True
    candidate
    (OperationAccepted (applicationRequestCandidateId candidate) reason)

-- | Retain a completion on a predecessor produced solely by application-value
-- localization.  The check prevents a coordinator from accidentally combining
-- a candidate with changed session, access, allocation, or acceptance state.
prepareApplicationRequestCompletionFromLocalizedState ::
  ApplicationRequestCandidate ->
  State ->
  RegularCallResult ->
  Either ApplicationSessionInvariant PreparedApplicationRequest
prepareApplicationRequestCompletionFromLocalizedState candidate localized result
  | sameApplicationOwnershipExceptPrivateIdentity predecessor localized =
      Right
        ( retainApplicationRequestFrom
            localized
            True
            candidate
            (Completed (applicationRequestCandidateId candidate) result)
        )
  | otherwise =
      Left
        ( ApplicationLocalizedCompletionPredecessorContradiction
            (applicationRequestCandidateSession candidate)
            (applicationRequestCandidateId candidate)
        )
  where
    predecessor = applicationRequestCandidatePredecessor candidate

-- | Combine a freshly generated global identity with the exact private slot and
-- request predecessor prepared for this first-seen call.
prepareApplicationGeneratedIdCompletion ::
  ApplicationRequestCandidate ->
  PreparedPrivateUniqueIdAllocation ->
  GlobalUniqueId ->
  Either ApplicationSessionInvariant PreparedApplicationRequest
prepareApplicationGeneratedIdCompletion candidate allocation generated = do
  let predecessor = applicationRequestCandidatePredecessor candidate
      (identitySuccessor, privateIdentity) =
        commitPrivateUniqueIdAllocation generated allocation
      localized = predecessor {privateIdentity = identitySuccessor}
  prepareApplicationRequestCompletionFromLocalizedState
    candidate
    localized
    (NewIdCompleted privateIdentity)

-- | Retain a successfully admitted pending wait.  The derived wait identity is
-- exposed only by this acceptance.
prepareApplicationWaitAcceptance ::
  ApplicationRequestCandidate ->
  PreparedApplicationRequest
prepareApplicationWaitAcceptance candidate =
  retainApplicationRequest
    True
    candidate
    ( WaitAccepted
        (applicationRequestCandidateId candidate)
        (applicationRequestCandidateWaitId candidate)
    )

preparedApplicationRequestReply ::
  PreparedApplicationRequest ->
  ApplicationRequestReply
preparedApplicationRequestReply (PreparedApplicationRequest _ reply) = reply

commitApplicationRequest ::
  PreparedApplicationRequest ->
  (State, ApplicationRequestReply)
commitApplicationRequest (PreparedApplicationRequest successor reply) =
  (successor, reply)

-- | A first-seen request retained without a reply body.  These preparations
-- are deliberately distinct from 'PreparedApplicationRequest': committing one
-- cannot accidentally emit a cursor or public pending reason.
newtype PreparedApplicationDetachedRequest
  = PreparedApplicationDetachedRequest State

prepareApplicationLabelCandidate ::
  ApplicationRequestCandidate ->
  Either ApplicationSessionTransitionError PreparedApplicationDetachedRequest
prepareApplicationLabelCandidate candidate = do
  call <-
    maybe
      (detachedContradiction candidate)
      Right
      (applicationOperationLabel (applicationRequestCandidateCall candidate))
  let state = applicationRequestCandidatePredecessor candidate
      key = candidateRequestKey candidate
  requireGateOpen state
  requireRequestKeyFree key state
  Right
    ( PreparedApplicationDetachedRequest
        state
          { labelCandidates =
              Map.insert
                key
                ( LabelCandidateRecord
                    (applicationRequestCandidateProcess candidate)
                    call
                )
                state.labelCandidates
          }
    )

prepareApplicationGatedIngress ::
  ApplicationRequestCandidate ->
  Either ApplicationSessionTransitionError PreparedApplicationDetachedRequest
prepareApplicationGatedIngress candidate = do
  let state = applicationRequestCandidatePredecessor candidate
  generation <- case state.applicationGate of
    ApplicationGateClosed retained -> Right retained
    phase ->
      sessionInvariant (ApplicationGatePhaseContradiction phase)
  retainGatedIngress (WholeApplicationIngressGate generation) candidate

-- | Endpoint handoff admission retains the unaccepted call. It acquires no
-- authority, publication identity, route or process position before release.
prepareApplicationEndpointHeldIngress ::
  LabelDecisionId ->
  ApplicationRequestCandidate ->
  Either ApplicationSessionTransitionError PreparedApplicationDetachedRequest
prepareApplicationEndpointHeldIngress decision candidate = do
  let state = applicationRequestCandidatePredecessor candidate
  unless (Map.member decision state.endpointAdmissionHolds) (detachedContradiction candidate)
  retainGatedIngress (EndpointIngressGate decision) candidate

retainGatedIngress ::
  IngressGate ->
  ApplicationRequestCandidate ->
  Either ApplicationSessionTransitionError PreparedApplicationDetachedRequest
retainGatedIngress gate candidate = do
  let state = applicationRequestCandidatePredecessor candidate
      key = candidateRequestKey candidate
  requireRequestKeyFree key state
  Right
    ( PreparedApplicationDetachedRequest
        state
          { gatedIngress =
              Map.insert
                key
                ( GatedIngressRecord
                    (applicationRequestCandidateProcess candidate)
                    (applicationRequestCandidateCall candidate)
                    gate
                )
                state.gatedIngress
          }
    )

prepareApplicationFenceHeld ::
  ApplicationRequestCandidate ->
  ApplicationFenceHeldWork ->
  Either ApplicationSessionTransitionError PreparedApplicationDetachedRequest
prepareApplicationFenceHeld candidate work = do
  let state = applicationRequestCandidatePredecessor candidate
      selected group = case applicationActiveLabelForGroup group state of
        Just active -> activeLabelApplicationTerminalResult active == Nothing
        Nothing -> False
  if any selected (PublicationGroups.membershipKeys (applicationFenceHeldWorkGroupMembership work))
    then prepareHeldPublication Set.empty candidate work
    else detachedContradiction candidate

-- | Retain complete, already checked work behind exact disappearance probes.
-- It has a process position but no Publication, Store or transport effects.
prepareApplicationDisappearanceHeld ::
  Set DisappearanceProbeId ->
  ApplicationRequestCandidate ->
  ApplicationFenceHeldWork ->
  Either ApplicationSessionTransitionError PreparedApplicationDetachedRequest
prepareApplicationDisappearanceHeld probes candidate work
  | Set.null probes = detachedContradiction candidate
  | otherwise = prepareHeldPublication probes candidate work

prepareHeldPublication ::
  Set DisappearanceProbeId ->
  ApplicationRequestCandidate ->
  ApplicationFenceHeldWork ->
  Either ApplicationSessionTransitionError PreparedApplicationDetachedRequest
prepareHeldPublication probes candidate work = do
  let state = applicationRequestCandidatePredecessor candidate
      key = candidateRequestKey candidate
      process = applicationRequestCandidateProcess candidate
      position = applicationRequestCandidatePosition candidate
  requireGateOpen state
  requireRequestKeyFree key state
  requireCandidatePosition position process state
  Right
    ( PreparedApplicationDetachedRequest
        state
          { acceptedFenceHeld =
              Map.insert
                position
                ( FenceHeldRecord
                    position
                    (applicationRequestCandidateCall candidate)
                    work
                    key
                    (Just FenceHeldReplyLink)
                    probes
                )
                state.acceptedFenceHeld,
            nextAcceptanceByProcess =
              Map.adjust (+ 1) process state.nextAcceptanceByProcess
          }
    )

commitApplicationDetachedRequest :: PreparedApplicationDetachedRequest -> State
commitApplicationDetachedRequest (PreparedApplicationDetachedRequest successor) =
  successor

-- | Reconstruct the exact still-unaccepted label candidate against the current
-- live session owner.  This allocates nothing; a composed coordinator may
-- revalidate candidates in canonical order and either leave the returned
-- predecessor untouched, retain a rejection, or atomically promote it with a
-- Barrier and Oracle Open intention.
applicationLabelCandidateForReconsideration ::
  ApplicationRequestKey ->
  State ->
  Either ApplicationSessionTransitionError ApplicationRequestCandidate
applicationLabelCandidateForReconsideration key state = do
  requireGateOpen state
  (process, call) <- case Map.lookup key state.labelCandidates of
    Nothing -> sessionInvariant (ApplicationDetachedRequestContradiction key)
    Just (LabelCandidateRecord owner retained) -> Right (owner, labelApplicationOperation retained)
  attachedFreshCandidate key process call state

-- | Remove one ingress from the just-reopened gate generation and expose it as
-- an ordinary first-seen candidate.  No process position is consumed until the
-- returned candidate is committed by its semantic operation.
applicationGatedIngressForRedrive ::
  ApplicationRequestKey ->
  State ->
  Either ApplicationSessionTransitionError ApplicationRequestCandidate
applicationGatedIngressForRedrive key state = do
  requireGateOpen state
  (process, operation, gate) <- case Map.lookup key state.gatedIngress of
    Nothing -> sessionInvariant (ApplicationDetachedRequestContradiction key)
    Just (GatedIngressRecord owner retained retainedGeneration) ->
      Right (owner, retained, retainedGeneration)
  requireIngressGateReleased gate state
  let predecessor = state {gatedIngress = Map.delete key state.gatedIngress}
  attachedFreshCandidate key process operation predecessor

-- | Move one gated label call back to the distinct pre-acceptance class.  A
-- label may never pass through the ordinary gate-redrive acceptance branch,
-- because its process position must be allocated in the same transition as
-- the singular Barrier/Open workflow.
moveGatedLabelToCandidate ::
  ApplicationRequestKey ->
  State ->
  Either ApplicationSessionTransitionError State
moveGatedLabelToCandidate key state = do
  requireGateOpen state
  (process, call, gate) <- case Map.lookup key state.gatedIngress of
    Nothing -> sessionInvariant (ApplicationDetachedRequestContradiction key)
    Just (GatedIngressRecord owner operation retainedGeneration) -> do
      label <-
        maybe
          (sessionInvariant (ApplicationDetachedRequestContradiction key))
          Right
          (applicationOperationLabel operation)
      Right (owner, label, retainedGeneration)
  requireIngressGateReleased gate state
  Right
    state
      { gatedIngress = Map.delete key state.gatedIngress,
        labelCandidates =
          Map.insert key (LabelCandidateRecord process call) state.labelCandidates
      }

ingressGateReleased :: IngressGate -> State -> Bool
ingressGateReleased gate state = case state.applicationGate of
  ApplicationGateClosed _ -> False
  ApplicationGateOpen current -> case gate of
    WholeApplicationIngressGate generation ->
      applicationGateGenerationWord64 current > applicationGateGenerationWord64 generation
    EndpointIngressGate decision -> Map.notMember decision state.endpointAdmissionHolds

requireIngressGateReleased :: IngressGate -> State -> Either ApplicationSessionTransitionError ()
requireIngressGateReleased gate state
  | ingressGateReleased gate state = Right ()
  | otherwise = sessionInvariant (ApplicationGatePhaseContradiction state.applicationGate)

-- | Remove one already-positioned fence-held item and expose a replay
-- candidate whose application acceptance position is explicitly preallocated.
-- Completion therefore cannot advance the process counter twice.  Session or
-- process retirement may detach reply reachability, but the semantic replay
-- candidate and its immutable position remain available.
applicationFenceHeldForRedrive ::
  LabelProcessAcceptancePosition ->
  State ->
  Either
    ApplicationSessionTransitionError
    (ApplicationRequestCandidate, ApplicationFenceHeldWork)
applicationFenceHeldForRedrive position state = do
  requireGateOpen state
  held <-
    maybe
      (sessionInvariant (ApplicationAcceptanceCounterContradiction process))
      Right
      (Map.lookup position state.acceptedFenceHeld)
  let FenceHeldRecord
        _
        operation
        work
        key
        replyLink
        _ = held
      session = applicationRequestKeySession key
      request = applicationRequestKeyRequest key
      predecessor =
        state
          { acceptedFenceHeld =
              Map.delete position state.acceptedFenceHeld
          }
      attachedSession = do
        record <- Map.lookup session predecessor.sessionsById
        if record.processEpoch == process then Just record else Nothing
      (_, _, detachedBinding) =
        Session.initialSessionAllocation
          (Session.applicationSessionIdHeraldEpoch session)
          (Session.applicationSessionIdOrdinal session)
      detachedWait = waitIdForRequest session request
      (binding, wait, cursor, reply) =
        case (attachedSession, replyLink) of
          (Just record, Just FenceHeldReplyLink) ->
            ( record.binding,
              detachedWait,
              Request.nextApplicationReplyCursor record.lastIssuedReplyCursor,
              ApplicationRequestCandidateReplyOwned record.deliveryLive
            )
          _ ->
            ( detachedBinding,
              detachedWait,
              Request.firstApplicationReplyCursor,
              ApplicationRequestCandidateReplyDetached
            )
  Right
    ( ApplicationRequestCandidate
        predecessor
        session
        binding
        process
        request
        operation
        position
        wait
        cursor
        True
        reply,
      work
    )
  where
    process = labelProcessAcceptancePositionProcess position

attachedFreshCandidate ::
  ApplicationRequestKey ->
  ProcessEpochId ->
  ApplicationOperation ->
  State ->
  Either ApplicationSessionTransitionError ApplicationRequestCandidate
attachedFreshCandidate key process operation predecessor = do
  let session = applicationRequestKeySession key
      request = applicationRequestKeyRequest key
  record <- liveSession session predecessor
  checkedLiveRecord record predecessor
  if record.processEpoch == process
    then Right ()
    else sessionInvariant (ApplicationDetachedRequestContradiction key)
  nextOrdinal <-
    maybe
      (sessionInvariant (ApplicationAcceptanceCounterContradiction process))
      Right
      (Map.lookup process predecessor.nextAcceptanceByProcess)
  let position = Request.processAcceptancePosition process nextOrdinal
      wait = waitIdForRequest session request
      cursor = Request.nextApplicationReplyCursor record.lastIssuedReplyCursor
  Right
    ( ApplicationRequestCandidate
        predecessor
        session
        record.binding
        process
        request
        operation
        position
        wait
        cursor
        False
        (ApplicationRequestCandidateReplyOwned record.deliveryLive)
    )

data PreparedApplicationLabelAcceptance
  = PreparedApplicationLabelAcceptance
      State
      LabelProcessAcceptancePosition
      ApplicationRequestReply

prepareApplicationLabelAcceptance ::
  LabelDecisionId ->
  SequencingSelection ->
  ApplicationRequestCandidate ->
  Either ApplicationSessionTransitionError PreparedApplicationLabelAcceptance
prepareApplicationLabelAcceptance decision selection candidate = do
  let predecessor = applicationRequestCandidatePredecessor candidate
  requireNoActiveLabel decision selection predecessor
  call <-
    maybe
      (detachedContradiction candidate)
      Right
      (applicationOperationLabel (applicationRequestCandidateCall candidate))
  requireSelectionMatches
    (applicationRequestCandidateProcess candidate)
    call
    selection
    predecessor
  let prepared =
        prepareApplicationOperationAcceptance candidate LabelSettlementPending
      (accepted, reply) = commitApplicationRequest prepared
      position = applicationRequestCandidatePosition candidate
      successor =
        accepted
          { activeLabels = Map.insert decision (ActiveLabelApplication decision (candidateRequestKey candidate) call selection position Nothing) accepted.activeLabels,
            labelByObject = Map.insert (sequencingSelectionTarget selection) decision accepted.labelByObject
          }
  Right (PreparedApplicationLabelAcceptance successor position reply)

-- | Add the narrow handoff hold to the same checked acceptance transaction.
-- The coordinator establishes the endpoint's local role and caller ownership;
-- this owner checks that it is exactly the accepted label target.
retainApplicationLabelEndpointHold ::
  NablaId ->
  PreparedApplicationLabelAcceptance ->
  Either ApplicationSessionTransitionError PreparedApplicationLabelAcceptance
retainApplicationLabelEndpointHold nabla (PreparedApplicationLabelAcceptance state position reply) =
  case applicationActiveLabelForObject (globalObjectIdFromNablaId nabla) state of
    Just active
      | activeLabelApplicationPosition active == position,
        activeLabelApplicationTerminalResult active == Nothing,
        let decision = activeLabelApplicationDecision active,
        Map.notMember decision state.endpointAdmissionHolds ->
          Right
            (PreparedApplicationLabelAcceptance (state {endpointAdmissionHolds = Map.insert decision nabla state.endpointAdmissionHolds}) position reply)
    _ -> sessionInvariant (ApplicationAcceptanceCounterContradiction (labelProcessAcceptancePositionProcess position))

prepareRetainedApplicationLabelAcceptance ::
  LabelDecisionId ->
  SequencingSelection ->
  ApplicationRequestKey ->
  State ->
  Either ApplicationSessionTransitionError PreparedApplicationLabelAcceptance
prepareRetainedApplicationLabelAcceptance decision selection key state = do
  requireNoActiveLabel decision selection state
  (process, call) <- case Map.lookup key state.labelCandidates of
    Just (LabelCandidateRecord owner retained) -> Right (owner, retained)
    Nothing ->
      sessionInvariant (ApplicationDetachedRequestContradiction key)
  requireSelectionMatches process call selection state
  sessionRecord <- liveSession (applicationRequestKeySession key) state
  checkedLiveRecord sessionRecord state
  nextOrdinal <-
    maybe
      (sessionInvariant (ApplicationAcceptanceCounterContradiction process))
      Right
      (Map.lookup process state.nextAcceptanceByProcess)
  let position = Request.processAcceptancePosition process nextOrdinal
      request = applicationRequestKeyRequest key
      cursor = Request.nextApplicationReplyCursor sessionRecord.lastIssuedReplyCursor
      body = OperationAccepted request LabelSettlementPending
      record =
        RequestRecord
          { requestId = request,
            call = labelApplicationOperation call,
            processEpoch = process,
            acceptancePosition = Just position,
            replyCursor = cursor,
            replyBody = body
          }
      successorSession =
        sessionRecord
          { lastIssuedReplyCursor = cursor,
            requests = Map.insert request record sessionRecord.requests
          }
      reply = RetainedRequestReply cursor body
      successor =
        state
          { sessionsById =
              Map.insert sessionRecord.sessionId successorSession state.sessionsById,
            nextAcceptanceByProcess =
              Map.insert process (nextOrdinal + 1) state.nextAcceptanceByProcess,
            labelCandidates = Map.delete key state.labelCandidates,
            activeLabels = Map.insert decision (ActiveLabelApplication decision key call selection position Nothing) state.activeLabels,
            labelByObject = Map.insert (sequencingSelectionTarget selection) decision state.labelByObject
          }
  Right (PreparedApplicationLabelAcceptance successor position reply)

preparedApplicationLabelAcceptancePosition ::
  PreparedApplicationLabelAcceptance -> LabelProcessAcceptancePosition
preparedApplicationLabelAcceptancePosition
  (PreparedApplicationLabelAcceptance _ position _) = position

preparedApplicationLabelAcceptanceReply ::
  PreparedApplicationLabelAcceptance -> ApplicationRequestReply
preparedApplicationLabelAcceptanceReply
  (PreparedApplicationLabelAcceptance _ _ reply) = reply

commitApplicationLabelAcceptance ::
  PreparedApplicationLabelAcceptance -> (State, ApplicationRequestReply)
commitApplicationLabelAcceptance
  (PreparedApplicationLabelAcceptance successor _ reply) = (successor, reply)

data PreparedApplicationLabelCandidateRejection
  = PreparedApplicationLabelCandidateRejection State ApplicationRequestReply

prepareApplicationLabelCandidateRejection ::
  ApplicationRequestKey ->
  ApplicationRejection ->
  State ->
  Either
    ApplicationSessionTransitionError
    PreparedApplicationLabelCandidateRejection
prepareApplicationLabelCandidateRejection key rejection state = do
  (process, call) <- case Map.lookup key state.labelCandidates of
    Just (LabelCandidateRecord owner retained) -> Right (owner, retained)
    Nothing -> sessionInvariant (ApplicationDetachedRequestContradiction key)
  sessionRecord <- liveSession (applicationRequestKeySession key) state
  checkedLiveRecord sessionRecord state
  let request = applicationRequestKeyRequest key
      cursor = Request.nextApplicationReplyCursor sessionRecord.lastIssuedReplyCursor
      body = Rejected request rejection
      record =
        RequestRecord
          { requestId = request,
            call = labelApplicationOperation call,
            processEpoch = process,
            acceptancePosition = Nothing,
            replyCursor = cursor,
            replyBody = body
          }
      successorSession =
        sessionRecord
          { lastIssuedReplyCursor = cursor,
            requests = Map.insert request record sessionRecord.requests
          }
      reply = RetainedRequestReply cursor body
      successor =
        state
          { sessionsById =
              Map.insert sessionRecord.sessionId successorSession state.sessionsById,
            labelCandidates = Map.delete key state.labelCandidates
          }
  Right (PreparedApplicationLabelCandidateRejection successor reply)

preparedApplicationLabelCandidateRejectionReply ::
  PreparedApplicationLabelCandidateRejection -> ApplicationRequestReply
preparedApplicationLabelCandidateRejectionReply
  (PreparedApplicationLabelCandidateRejection _ reply) = reply

commitApplicationLabelCandidateRejection ::
  PreparedApplicationLabelCandidateRejection -> (State, ApplicationRequestReply)
commitApplicationLabelCandidateRejection
  (PreparedApplicationLabelCandidateRejection successor reply) = (successor, reply)

data PreparedApplicationLabelCandidateCompletion
  = PreparedApplicationLabelCandidateCompletion State ApplicationRequestReply

-- | Complete a retained pre-acceptance label candidate after its value-CAS
-- operand no longer matches.  Unlike a rejection, this is an accepted semantic
-- call and therefore consumes its process acceptance position.
prepareApplicationLabelCandidateCompletion ::
  ApplicationRequestKey ->
  LabelResult ->
  State ->
  Either
    ApplicationSessionTransitionError
    PreparedApplicationLabelCandidateCompletion
prepareApplicationLabelCandidateCompletion key result state = do
  (process, call) <- case Map.lookup key state.labelCandidates of
    Just (LabelCandidateRecord owner retained) -> Right (owner, retained)
    Nothing -> sessionInvariant (ApplicationDetachedRequestContradiction key)
  sessionRecord <- liveSession (applicationRequestKeySession key) state
  checkedLiveRecord sessionRecord state
  nextOrdinal <-
    maybe
      (sessionInvariant (ApplicationAcceptanceCounterContradiction process))
      Right
      (Map.lookup process state.nextAcceptanceByProcess)
  let request = applicationRequestKeyRequest key
      position = Request.processAcceptancePosition process nextOrdinal
      cursor = Request.nextApplicationReplyCursor sessionRecord.lastIssuedReplyCursor
      body = Completed request (LabelCompleted result)
      record =
        RequestRecord
          { requestId = request,
            call = labelApplicationOperation call,
            processEpoch = process,
            acceptancePosition = Just position,
            replyCursor = cursor,
            replyBody = body
          }
      successorSession =
        sessionRecord
          { lastIssuedReplyCursor = cursor,
            requests = Map.insert request record sessionRecord.requests
          }
      reply = RetainedRequestReply cursor body
      successor =
        state
          { sessionsById =
              Map.insert sessionRecord.sessionId successorSession state.sessionsById,
            nextAcceptanceByProcess =
              Map.insert process (nextOrdinal + 1) state.nextAcceptanceByProcess,
            labelCandidates = Map.delete key state.labelCandidates
          }
  Right (PreparedApplicationLabelCandidateCompletion successor reply)

preparedApplicationLabelCandidateCompletionReply ::
  PreparedApplicationLabelCandidateCompletion -> ApplicationRequestReply
preparedApplicationLabelCandidateCompletionReply
  (PreparedApplicationLabelCandidateCompletion _ reply) = reply

commitApplicationLabelCandidateCompletion ::
  PreparedApplicationLabelCandidateCompletion -> (State, ApplicationRequestReply)
commitApplicationLabelCandidateCompletion
  (PreparedApplicationLabelCandidateCompletion successor reply) =
    (successor, reply)

applicationMembershipGateClosed :: State -> Bool
applicationMembershipGateClosed state = case state.applicationGate of
  ApplicationGateClosed _ -> True
  ApplicationGateOpen _ -> False

-- | Only membership preparation pauses universal admission. Label requests
-- own their selected publication fence and optional endpoint hold separately.
setApplicationMembershipGate :: Bool -> State -> State
setApplicationMembershipGate closed state =
  let phase = case (state.applicationGate, closed) of
        (ApplicationGateOpen generation, True) -> ApplicationGateClosed generation
        (ApplicationGateClosed (ApplicationGateGeneration generation), False) ->
          ApplicationGateOpen (ApplicationGateGeneration (generation + 1))
        (retained, _) -> retained
   in state {applicationGate = phase}

newtype PreparedApplicationLabelTerminal
  = PreparedApplicationLabelTerminal State

prepareApplicationLabelTerminal ::
  LabelDecisionId ->
  LabelResult ->
  State ->
  Either ApplicationSessionTransitionError PreparedApplicationLabelTerminal
prepareApplicationLabelTerminal decision result state =
  case Map.lookup decision state.activeLabels of
    Just (ActiveLabelApplication retained key call selection position terminal)
      | retained == decision,
        terminal == Nothing ->
          Right
            ( PreparedApplicationLabelTerminal
                state
                  { endpointAdmissionHolds = Map.delete decision state.endpointAdmissionHolds,
                    activeLabels = Map.insert decision (ActiveLabelApplication retained key call selection position (Just result)) state.activeLabels
                  }
            )
      | retained == decision,
        terminal == Just result ->
          Right (PreparedApplicationLabelTerminal state)
    _ -> sessionInvariant (ApplicationActiveLabelContradiction decision)

commitApplicationLabelTerminal :: PreparedApplicationLabelTerminal -> State
commitApplicationLabelTerminal (PreparedApplicationLabelTerminal successor) =
  successor

data PreparedApplicationLabelWorkflowCompletion
  = PreparedApplicationLabelWorkflowCompletion
      State
      ApplicationOperationCompletionOutcome

prepareApplicationLabelWorkflowCompletion ::
  LabelDecisionId ->
  State ->
  Either
    ApplicationSessionTransitionError
    PreparedApplicationLabelWorkflowCompletion
prepareApplicationLabelWorkflowCompletion decision state = do
  (position, result, object) <- case Map.lookup decision state.activeLabels of
    Just (ActiveLabelApplication retained _ _ selection acceptedPosition terminal)
      | retained == decision -> case terminal of
          Just completed -> Right (acceptedPosition, completed, sequencingSelectionTarget selection)
          Nothing -> sessionInvariant (ApplicationActiveLabelContradiction decision)
    _ -> sessionInvariant (ApplicationActiveLabelContradiction decision)
  prepared <-
    prepareApplicationOperationCompletion
      position
      (LabelCompleted result)
      state
  let (completed, outcome) = commitApplicationOperationCompletion prepared
  Right
    ( PreparedApplicationLabelWorkflowCompletion
        completed {activeLabels = Map.delete decision completed.activeLabels, labelByObject = Map.delete object completed.labelByObject}
        outcome
    )

preparedApplicationLabelWorkflowCompletionOutcome ::
  PreparedApplicationLabelWorkflowCompletion ->
  ApplicationOperationCompletionOutcome
preparedApplicationLabelWorkflowCompletionOutcome
  (PreparedApplicationLabelWorkflowCompletion _ outcome) = outcome

commitApplicationLabelWorkflowCompletion ::
  PreparedApplicationLabelWorkflowCompletion ->
  (State, ApplicationOperationCompletionOutcome)
commitApplicationLabelWorkflowCompletion
  (PreparedApplicationLabelWorkflowCompletion successor outcome) =
    (successor, outcome)

candidateRequestKey :: ApplicationRequestCandidate -> ApplicationRequestKey
candidateRequestKey candidate =
  applicationRequestKey
    (applicationRequestCandidateSession candidate)
    (applicationRequestCandidateId candidate)

detachedContradiction ::
  ApplicationRequestCandidate -> Either ApplicationSessionTransitionError value
detachedContradiction =
  sessionInvariant
    . ApplicationDetachedRequestContradiction
    . candidateRequestKey

requireRequestKeyFree ::
  ApplicationRequestKey ->
  State ->
  Either ApplicationSessionTransitionError ()
requireRequestKeyFree key state =
  case detachedRequestClassAndOperation key state of
    Nothing -> Right ()
    Just _ -> sessionInvariant (ApplicationDetachedRequestContradiction key)

requireGateOpen :: State -> Either ApplicationSessionTransitionError ()
requireGateOpen state = case state.applicationGate of
  ApplicationGateOpen _ -> Right ()
  phase -> sessionInvariant (ApplicationGatePhaseContradiction phase)

requireCandidatePosition ::
  LabelProcessAcceptancePosition ->
  ProcessEpochId ->
  State ->
  Either ApplicationSessionTransitionError ()
requireCandidatePosition position process state =
  case Map.lookup process state.nextAcceptanceByProcess of
    Just ordinal
      | labelProcessAcceptancePositionProcess position == process,
        labelProcessAcceptancePositionOrdinal position == ordinal ->
          Right ()
    _ -> sessionInvariant (ApplicationAcceptanceCounterContradiction process)

requireNoActiveLabel ::
  LabelDecisionId ->
  SequencingSelection ->
  State ->
  Either ApplicationSessionTransitionError ()
requireNoActiveLabel decision selection state
  | Map.member decision state.activeLabels || Map.member (sequencingSelectionTarget selection) state.labelByObject = sessionInvariant (ApplicationActiveLabelContradiction decision)
  | otherwise = Right ()

requireSelectionMatches ::
  ProcessEpochId ->
  LabelApplication ->
  SequencingSelection ->
  State ->
  Either ApplicationSessionTransitionError ()
requireSelectionMatches process call selection state =
  case resolveMappedPrivate
    process
    (privateObjectUniqueId (labelApplicationObject call))
    state.privateIdentity of
    Right global
      | globalObjectIdFromGlobalUniqueId global
          == sequencingSelectionTarget selection,
        process == PublicationGroups.groupProcess (sequencingSelectionGroup selection) ->
          Right ()
    _ ->
      sessionInvariant
        ( ApplicationLabelSelectionContradiction
            process
            (labelApplicationObject call)
        )

-- | Reoffer the current retained request state without allocating a cursor.  An
-- unknown request is a typed unretained observation.
applicationRequestResult ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  RequestId ->
  State ->
  Either ApplicationSessionTransitionError ApplicationRequestReply
applicationRequestResult session binding request state = do
  sessionRecord <- checkedRequestSession session binding state
  Right $ case retiredRequestThrough sessionRecord request of
    Just through -> RequestRetired request through
    Nothing -> case Map.lookup request sessionRecord.requests of
      Nothing -> RequestAbsent request
      Just retained -> requestRecordReply retained

-- | Live EAPP result lookup.  'Nothing' means the exact key is retained in a
-- pre-reply candidate, gated, or fence-held class, so the connection remains
-- attached without emitting a synthetic public pending body.
applicationRequestResultDisposition ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  RequestId ->
  State ->
  Either ApplicationSessionTransitionError (Maybe ApplicationRequestReply)
applicationRequestResultDisposition session binding request state = do
  sessionRecord <- checkedRequestSession session binding state
  let key = applicationRequestKey session request
  Right $ case retiredRequestThrough sessionRecord request of
    Just through -> Just (RequestRetired request through)
    Nothing -> case Map.lookup request sessionRecord.requests of
      Just retained -> Just (requestRecordReply retained)
      Nothing -> case detachedRequestClassAndOperation key state of
        Just _ -> Nothing
        Nothing -> Just (RequestAbsent request)

-- | Optional live-delivery observation for an operation that became terminal.
-- The completed reply itself is retained regardless of delivery state.  This
-- observation exists only while the current session binding has a delivery
-- path, so runtime effects cannot accidentally target a disconnected binding.
data ApplicationOperationCompletionObservation
  = ApplicationOperationCompletionObservation
      ApplicationSessionBinding
      ApplicationReplyCursor
      RequestId
      RegularCallResult
  deriving stock (Eq, Show)

applicationOperationCompletionBinding ::
  ApplicationOperationCompletionObservation ->
  ApplicationSessionBinding
applicationOperationCompletionBinding
  (ApplicationOperationCompletionObservation binding _ _ _) = binding

applicationOperationCompletionCursor ::
  ApplicationOperationCompletionObservation ->
  ApplicationReplyCursor
applicationOperationCompletionCursor
  (ApplicationOperationCompletionObservation _ cursor _ _) = cursor

applicationOperationCompletionRequestId ::
  ApplicationOperationCompletionObservation ->
  RequestId
applicationOperationCompletionRequestId
  (ApplicationOperationCompletionObservation _ _ request _) = request

applicationOperationCompletionResult ::
  ApplicationOperationCompletionObservation ->
  RegularCallResult
applicationOperationCompletionResult
  (ApplicationOperationCompletionObservation _ _ _ result) = result

-- | Result of attempting to reflect semantic stabilization in the live
-- application-request owner.  Absence is a successful no-op: an ended session
-- deliberately forgets its application correlation while the semantic
-- publication continues independently.
data ApplicationOperationCompletionOutcome
  = ApplicationOperationNoLiveRequest ProcessAcceptancePosition
  | ApplicationOperationCompleted
      (Maybe ApplicationOperationCompletionObservation)
  deriving stock (Eq, Show)

-- | Complete checked successor and its delivery-neutral observation.
data PreparedApplicationOperationCompletion
  = PreparedApplicationOperationCompletion
      State
      ApplicationOperationCompletionOutcome

-- | Find the unique live retained request at an exact process acceptance
-- position and move only the nonterminal operation arm to 'Completed'.  A
-- missing position is the normal late-stabilization case after session end;
-- a duplicate position or any other retained status is an owner invariant
-- contradiction.
prepareApplicationOperationCompletion ::
  ProcessAcceptancePosition ->
  RegularCallResult ->
  State ->
  Either
    ApplicationSessionTransitionError
    PreparedApplicationOperationCompletion
prepareApplicationOperationCompletion position result state =
  case requestRecordsAtPosition position state of
    [] ->
      case publicationRecordsAtPosition position state
        <> environmentRecordsAtPosition position state of
        [] ->
          Right
            ( PreparedApplicationOperationCompletion
                state
                (ApplicationOperationNoLiveRequest position)
            )
        _ -> completionContradiction position
    [(sessionRecord, requestRecord)]
      | null (publicationRecordsAtPosition position state)
          && null (environmentRecordsAtPosition position state) ->
          case requestRecord.replyBody of
            OperationAccepted request _ ->
              let cursor =
                    Request.nextApplicationReplyCursor
                      sessionRecord.lastIssuedReplyCursor
                  body = Completed request result
                  successor =
                    replaceRequestReply
                      sessionRecord
                      requestRecord
                      cursor
                      body
                      state
                  observation
                    | sessionRecord.deliveryLive =
                        Just
                          ( ApplicationOperationCompletionObservation
                              sessionRecord.binding
                              cursor
                              request
                              result
                          )
                    | otherwise = Nothing
               in Right
                    ( PreparedApplicationOperationCompletion
                        successor
                        (ApplicationOperationCompleted observation)
                    )
            _ -> completionContradiction position
      | otherwise -> completionContradiction position
    _ -> completionContradiction position

preparedApplicationOperationCompletionOutcome ::
  PreparedApplicationOperationCompletion ->
  ApplicationOperationCompletionOutcome
preparedApplicationOperationCompletionOutcome
  (PreparedApplicationOperationCompletion _ outcome) = outcome

commitApplicationOperationCompletion ::
  PreparedApplicationOperationCompletion ->
  (State, ApplicationOperationCompletionOutcome)
commitApplicationOperationCompletion
  (PreparedApplicationOperationCompletion successor outcome) =
    (successor, outcome)

-- | Cancellation classification.  Only the exact pending arm has a successor;
-- all other arms reuse a retained reply or an unretained bookkeeping outcome
-- without allocating a cursor.
data ApplicationWaitCancellation
  = ApplicationWaitCancellationReply ApplicationRequestReply
  | ExactApplicationWaitCancellation PreparedApplicationWaitCancellation

-- | Prepared transition from one exact pending wait to cancelled.
data PreparedApplicationWaitCancellation
  = PreparedApplicationWaitCancellation State WaitId ApplicationRequestReply

prepareApplicationWaitCancellation ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  RequestId ->
  WaitId ->
  State ->
  Either
    ApplicationSessionTransitionError
    ApplicationWaitCancellation
prepareApplicationWaitCancellation session binding request wait state = do
  sessionRecord <- checkedRequestSession session binding state
  Right $ case retiredRequestThrough sessionRecord request of
    Just through -> ApplicationWaitCancellationReply (RequestRetired request through)
    Nothing -> case Map.lookup request sessionRecord.requests of
      Nothing -> ApplicationWaitCancellationReply (RequestAbsent request)
      Just requestRecord -> case pendingRequestWait requestRecord of
        Nothing ->
          ApplicationWaitCancellationReply (requestRecordReply requestRecord)
        Just retainedWait
          | retainedWait /= wait ->
              ApplicationWaitCancellationReply (RequestConflict request)
          | otherwise ->
              let cursor =
                    Request.nextApplicationReplyCursor
                      sessionRecord.lastIssuedReplyCursor
                  body = Cancelled request wait
                  successor =
                    replaceRequestReply
                      sessionRecord
                      requestRecord
                      cursor
                      body
                      state
                  reply = RetainedRequestReply cursor body
               in ExactApplicationWaitCancellation
                    (PreparedApplicationWaitCancellation successor wait reply)

preparedApplicationWaitCancellationWaitId ::
  PreparedApplicationWaitCancellation ->
  WaitId
preparedApplicationWaitCancellationWaitId
  (PreparedApplicationWaitCancellation _ wait _) = wait

preparedApplicationWaitCancellationReply ::
  PreparedApplicationWaitCancellation ->
  ApplicationRequestReply
preparedApplicationWaitCancellationReply
  (PreparedApplicationWaitCancellation _ _ reply) = reply

commitApplicationWaitCancellation ::
  PreparedApplicationWaitCancellation ->
  (State, ApplicationRequestReply)
commitApplicationWaitCancellation
  (PreparedApplicationWaitCancellation successor _ reply) =
    (successor, reply)

-- | One wake delivery observation.  The same cursor is retained on the request
-- as its completed direct-reply identity.
data ApplicationWaitWake
  = ApplicationWaitWake
      ApplicationSessionBinding
      ApplicationReplyCursor
      RequestId
      WaitId
      WaitResult
  deriving stock (Eq, Show)

applicationWaitWakeBinding :: ApplicationWaitWake -> ApplicationSessionBinding
applicationWaitWakeBinding (ApplicationWaitWake binding _ _ _ _) = binding

applicationWaitWakeCursor :: ApplicationWaitWake -> ApplicationReplyCursor
applicationWaitWakeCursor (ApplicationWaitWake _ cursor _ _ _) = cursor

applicationWaitWakeRequestId :: ApplicationWaitWake -> RequestId
applicationWaitWakeRequestId (ApplicationWaitWake _ _ request _ _) = request

applicationWaitWakeWaitId :: ApplicationWaitWake -> WaitId
applicationWaitWakeWaitId (ApplicationWaitWake _ _ _ wait _) = wait

applicationWaitWakeResult :: ApplicationWaitWake -> WaitResult
applicationWaitWakeResult (ApplicationWaitWake _ _ _ _ result) = result

-- | Application successor for an already owner-checked set of ready waits.
-- Inputs are sorted here so per-session cursor allocation and wake order cannot
-- depend on a caller's collection representation.
data PreparedApplicationWaitCompletions
  = PreparedApplicationWaitCompletions State [ApplicationWaitWake]

prepareApplicationWaitCompletions ::
  [WaitId] ->
  State ->
  Either
    ApplicationSessionTransitionError
    PreparedApplicationWaitCompletions
prepareApplicationWaitCompletions waits state = do
  (successor, reverseWakes) <-
    foldM completeApplicationWait (state, []) (sort waits)
  Right (PreparedApplicationWaitCompletions successor (reverse reverseWakes))

preparedApplicationWaitWakes ::
  PreparedApplicationWaitCompletions ->
  [ApplicationWaitWake]
preparedApplicationWaitWakes
  (PreparedApplicationWaitCompletions _ wakes) = wakes

commitApplicationWaitCompletions ::
  PreparedApplicationWaitCompletions ->
  (State, [ApplicationWaitWake])
commitApplicationWaitCompletions
  (PreparedApplicationWaitCompletions successor wakes) =
    (successor, wakes)

retainApplicationRequest ::
  Bool ->
  ApplicationRequestCandidate ->
  RetainedRequestReplyBody ->
  PreparedApplicationRequest
retainApplicationRequest accepted candidate =
  retainApplicationRequestFrom
    (applicationRequestCandidatePredecessor candidate)
    accepted
    candidate

retainApplicationRequestFrom ::
  State ->
  Bool ->
  ApplicationRequestCandidate ->
  RetainedRequestReplyBody ->
  PreparedApplicationRequest
retainApplicationRequestFrom
  base
  accepted
  ( ApplicationRequestCandidate
      _predecessor
      session
      _binding
      process
      request
      call
      position
      _wait
      cursor
      positionAlreadyAllocated
      reply
    )
  body =
    PreparedApplicationRequest successor (RetainedRequestReply cursor body)
    where
      requestRecord =
        RequestRecord
          { requestId = request,
            call = call,
            processEpoch = process,
            acceptancePosition = retainedPosition,
            replyCursor = cursor,
            replyBody = body
          }
      retainInSession sessionRecord =
        sessionRecord
          { lastIssuedReplyCursor = cursor,
            requests = Map.insert request requestRecord sessionRecord.requests
          }
      acceptanceCounters
        | accepted && not positionAlreadyAllocated =
            Map.adjust (+ 1) process base.nextAcceptanceByProcess
        | otherwise = base.nextAcceptanceByProcess
      retainedPosition
        | accepted || positionAlreadyAllocated = Just position
        | otherwise = Nothing
      retainedSessions =
        case reply of
          ApplicationRequestCandidateReplyDetached -> base.sessionsById
          ApplicationRequestCandidateReplyOwned _ ->
            Map.adjust retainInSession session base.sessionsById
      successor =
        base
          { sessionsById = retainedSessions,
            nextAcceptanceByProcess = acceptanceCounters
          }

applicationRequestCandidatePredecessor :: ApplicationRequestCandidate -> State
applicationRequestCandidatePredecessor
  (ApplicationRequestCandidate predecessor _ _ _ _ _ _ _ _ _ _) = predecessor

sameApplicationOwnershipExceptPrivateIdentity :: State -> State -> Bool
sameApplicationOwnershipExceptPrivateIdentity left right =
  left.accessByProcess == right.accessByProcess
    && left.processByAttachment == right.processByAttachment
    && left.sessionsById == right.sessionsById
    && left.preparationSessionChanges == right.preparationSessionChanges
    && left.openSessionByNonce == right.openSessionByNonce
    && left.processRecoveryByProcess == right.processRecoveryByProcess
    && left.nextSessionOrdinal == right.nextSessionOrdinal
    && left.nextAcceptanceByProcess == right.nextAcceptanceByProcess
    && left.labelCandidates == right.labelCandidates
    && left.gatedIngress == right.gatedIngress
    && left.acceptedFenceHeld == right.acceptedFenceHeld
    && left.pendingEnvironments == right.pendingEnvironments
    && left.completedEnvironments == right.completedEnvironments
    && left.applicationGate == right.applicationGate
    && left.activeLabels == right.activeLabels
    && left.labelByObject == right.labelByObject
    && left.endpointAdmissionHolds == right.endpointAdmissionHolds

requestRecordReply :: RequestRecord -> ApplicationRequestReply
requestRecordReply record =
  RetainedRequestReply record.replyCursor record.replyBody

requestRecordsAtPosition ::
  ProcessAcceptancePosition ->
  State ->
  [(SessionRecord, RequestRecord)]
requestRecordsAtPosition position state =
  [ (sessionRecord, requestRecord)
  | sessionRecord <- Map.elems state.sessionsById,
    requestRecord <- Map.elems sessionRecord.requests,
    requestRecord.acceptancePosition == Just position
  ]

-- The package-private publication map shares the process-position allocator
-- with regular calls.  Finding the requested position there is not session
-- absence: it proves that a caller attempted to complete the wrong owner arm.
publicationRecordsAtPosition ::
  ProcessAcceptancePosition ->
  State ->
  [()]
publicationRecordsAtPosition position state =
  [ ()
  | sessionRecord <- Map.elems state.sessionsById,
    ApplicationPublicationRequestRecord _ _ _ retainedPosition <-
      Map.elems sessionRecord.publicationRequests,
    retainedPosition == position
  ]

environmentRecordsAtPosition ::
  ProcessAcceptancePosition ->
  State ->
  [()]
environmentRecordsAtPosition position state =
  [ ()
  | pending <- Map.elems state.pendingEnvironments,
    Environment.environmentManifestPosition
      (Environment.pendingEnvironmentManifest pending)
      == position
  ]
    <> [ ()
       | completed <- Map.elems state.completedEnvironments,
         Environment.environmentManifestPosition
           (Environment.completedEnvironmentManifest completed)
           == position
       ]

completionContradiction ::
  ProcessAcceptancePosition ->
  Either ApplicationSessionTransitionError value
completionContradiction =
  sessionInvariant . ApplicationOperationCompletionContradiction

pendingRequestWait :: RequestRecord -> Maybe WaitId
pendingRequestWait record = case Request.retainedReplyBodyStatus record.replyBody of
  Request.ApplicationRequestPending wait -> Just wait
  _ -> Nothing

replaceRequestReply ::
  SessionRecord ->
  RequestRecord ->
  ApplicationReplyCursor ->
  RetainedRequestReplyBody ->
  State ->
  State
replaceRequestReply sessionRecord requestRecord cursor body state =
  state
    { sessionsById =
        Map.insert
          sessionRecord.sessionId
          updatedSession
          state.sessionsById
    }
  where
    updatedRequest =
      requestRecord
        { replyCursor = cursor,
          replyBody = body
        }
    updatedSession =
      sessionRecord
        { lastIssuedReplyCursor = cursor,
          requests =
            Map.insert
              requestRecord.requestId
              updatedRequest
              sessionRecord.requests
        }

completeApplicationWait ::
  (State, [ApplicationWaitWake]) ->
  WaitId ->
  Either
    ApplicationSessionTransitionError
    (State, [ApplicationWaitWake])
completeApplicationWait (state, reverseWakes) wait = do
  sessionRecord <-
    maybe
      (sessionInvariant (ApplicationWaitRequestContradiction wait))
      Right
      (findWaitSession wait state)
  let request = Request.waitIdRequestId wait
  requestRecord <-
    maybe
      (sessionInvariant (ApplicationWaitRequestContradiction wait))
      Right
      (Map.lookup request sessionRecord.requests)
  retainedWait <-
    maybe
      (sessionInvariant (ApplicationWaitRequestContradiction wait))
      Right
      (pendingRequestWait requestRecord)
  if retainedWait /= wait
    then sessionInvariant (ApplicationWaitRequestContradiction wait)
    else do
      let cursor =
            Request.nextApplicationReplyCursor
              sessionRecord.lastIssuedReplyCursor
          waitResult = WaitReady
          body = Completed request (WaitCompleted waitResult)
          successor =
            replaceRequestReply sessionRecord requestRecord cursor body state
          successorWakes
            | sessionRecord.deliveryLive =
                ApplicationWaitWake
                  sessionRecord.binding
                  cursor
                  request
                  wait
                  waitResult
                  : reverseWakes
            | otherwise = reverseWakes
      Right (successor, successorWakes)

findWaitSession :: WaitId -> State -> Maybe SessionRecord
findWaitSession wait state =
  snd
    <$> find
      (waitNamesSession wait . fst)
      (Map.toAscList state.sessionsById)

waitNamesSession :: WaitId -> ApplicationSessionId -> Bool
waitNamesSession wait session =
  Request.waitIdHeraldEpoch wait
    == Session.applicationSessionIdHeraldEpoch session
    && Request.waitIdSessionOrdinal wait
      == Session.applicationSessionIdOrdinal session

waitIdForRequest :: ApplicationSessionId -> RequestId -> WaitId
waitIdForRequest session =
  Request.waitIdForSessionParts
    (Session.applicationSessionIdHeraldEpoch session)
    (Session.applicationSessionIdOrdinal session)

checkedRequestSession ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  State ->
  Either ApplicationSessionTransitionError SessionRecord
checkedRequestSession session binding state
  | Session.sessionBindingSessionId binding /= session =
      rejectSession Session.ApplicationRequestSessionMismatch
  | otherwise = do
      record <- liveSession session state
      checkedLiveRecord record state
      if record.binding /= binding
        then rejectSession Session.ApplicationRequestBindingMismatch
        else
          if not record.deliveryLive
            then rejectSession Session.ApplicationRequestBindingNotLive
            else Right record

initialApplicationProcessRecovery :: ApplicationProcessRecovery
initialApplicationProcessRecovery =
  ApplicationProcessRecovery
    { lastGeneration = Nothing,
      phase = ApplicationProcessAvailable
    }

markAvailable :: ApplicationProcessRecovery -> ApplicationProcessRecovery
markAvailable recovery = recovery {phase = ApplicationProcessAvailable}

markProcessAvailable ::
  ProcessEpochId ->
  Map ProcessEpochId ApplicationProcessRecovery ->
  Map ProcessEpochId ApplicationProcessRecovery
markProcessAvailable process =
  Map.alter
    (Just . maybe initialApplicationProcessRecovery markAvailable)
    process

requireProcessAcceptingSessions ::
  ProcessEpochId ->
  State ->
  Either ApplicationSessionTransitionError ()
requireProcessAcceptingSessions process state =
  case Map.lookup process state.processRecoveryByProcess of
    Just recovery -> case recovery.phase of
      ApplicationProcessSealed _ ->
        rejectSession Session.ApplicationAttachmentNotAdmitted
      _ -> Right ()
    Nothing -> Right ()

openSessionTransition ::
  HeraldEpoch ->
  ApplicationAttachment ->
  ClientNonce ->
  State ->
  Either ApplicationSessionTransitionError (State, ApplicationSessionAcceptance)
openSessionTransition herald attachment nonce state = do
  if Map.member attachment state.initialClaimsByAttachment
    then rejectSession Session.ApplicationAttachmentNotAdmitted
    else Right ()
  process <-
    maybe
      (rejectSession Session.ApplicationAttachmentNotAdmitted)
      Right
      (Map.lookup attachment state.processByAttachment)
  requireProcessAcceptingSessions process state
  access <- checkedBootstrapAccess process state
  startupAccess <-
    either
      sessionInvariant
      Right
      (applicationStartupAccessForBootstrap access)
  case Map.lookup (process, nonce) state.openSessionByNonce of
    Just session -> retryOpen process attachment nonce session state
    Nothing -> newOpen herald process attachment nonce startupAccess state

retryOpen ::
  ProcessEpochId ->
  ApplicationAttachment ->
  ClientNonce ->
  ApplicationSessionId ->
  State ->
  Either ApplicationSessionTransitionError (State, ApplicationSessionAcceptance)
retryOpen process attachment nonce session state = do
  record <-
    maybe
      (sessionInvariant (ApplicationSessionRetryIndexContradiction session))
      Right
      (Map.lookup session state.sessionsById)
  if record.processEpoch /= process
    || record.attachment /= attachment
    || record.nonce /= nonce
    then contradiction
    else
      if applicationMembershipGateClosed state && not record.deliveryLive
        then Left ApplicationSessionGatePending
        else
          let reoffered =
                record
                  { deliveryLive = True,
                    recovery = Nothing
                  }
              successor =
                state
                  { sessionsById =
                      Map.insert session reoffered state.sessionsById,
                    processRecoveryByProcess =
                      markProcessAvailable process state.processRecoveryByProcess
                  }
           in Right (successor, openedAcceptance reoffered)
  where
    contradiction =
      sessionInvariant (ApplicationSessionRetryIndexContradiction session)

newOpen ::
  HeraldEpoch ->
  ProcessEpochId ->
  ApplicationAttachment ->
  ClientNonce ->
  ApplicationStartupAccess ->
  State ->
  Either ApplicationSessionTransitionError (State, ApplicationSessionAcceptance)
newOpen herald process attachment nonce startupAccess state
  | applicationMembershipGateClosed state = Left ApplicationSessionGatePending
  | Map.member session state.sessionsById =
      sessionInvariant (ApplicationSessionAllocationContradiction session)
  | otherwise =
      Right
        ( state
            { sessionsById = Map.insert session record state.sessionsById,
              openSessionByNonce =
                Map.insert
                  (process, nonce)
                  session
                  state.openSessionByNonce,
              nextSessionOrdinal = state.nextSessionOrdinal + 1,
              processRecoveryByProcess =
                Map.alter
                  (Just . maybe initialApplicationProcessRecovery markAvailable)
                  process
                  state.processRecoveryByProcess
            },
          openedAcceptance record
        )
  where
    (session, token, binding) =
      Session.initialSessionAllocation herald state.nextSessionOrdinal
    record =
      SessionRecord
        { sessionId = session,
          processEpoch = process,
          attachment = attachment,
          nonce = nonce,
          resumeToken = token,
          binding = binding,
          bindingEstablishedBy = cursor,
          lastIssuedReplyCursor = cursor,
          receiptRetirement = emptyApplicationReceiptRetirement,
          retiredReplyCursor = Nothing,
          retainedDisposition = acceptance,
          deliveryLive = True,
          lastRecoveryGeneration = Nothing,
          recovery = Nothing,
          startupAccess = startupAccess,
          requests = Map.empty,
          publicationRequests = Map.empty,
          environmentRequests = Map.empty
        }
    cursor = Request.firstApplicationReplyCursor
    acceptance =
      Session.applicationSessionAcceptance
        binding
        cursor
        (Session.SessionOpened session token startupAccess)

openedAcceptance :: SessionRecord -> ApplicationSessionAcceptance
openedAcceptance record = record.retainedDisposition

resumeSessionTransition ::
  ApplicationSessionId ->
  ApplicationResumeToken ->
  ApplicationReplyCursor ->
  State ->
  Either ApplicationSessionTransitionError (State, ApplicationSessionAcceptance)
resumeSessionTransition session token lastObserved state = do
  (successor, acceptance) <- resumeSessionTransitionChecked session token lastObserved state
  let transferred = Map.map (transferInitialClaim session) successor.initialClaimsByAttachment
  Right (successor {initialClaimsByAttachment = transferred}, acceptance)

transferInitialClaim :: ApplicationSessionId -> InitialClaimRecord -> InitialClaimRecord
transferInitialClaim session (InitialClaimRecord process status) =
  InitialClaimRecord process $ case status of
    ApplicationInitialClaimClaimed claim acceptance deadline resumed
      | Session.sessionBindingSessionId (Session.sessionAcceptanceBinding acceptance) == session ->
          ApplicationInitialClaimClaimed claim acceptance deadline True
      | otherwise -> ApplicationInitialClaimClaimed claim acceptance deadline resumed
    unclaimed -> unclaimed

resumeSessionTransitionChecked :: ApplicationSessionId -> ApplicationResumeToken -> ApplicationReplyCursor -> State -> Either ApplicationSessionTransitionError (State, ApplicationSessionAcceptance)
resumeSessionTransitionChecked session token lastObserved state = do
  record <- liveSession session state
  requireProcessAcceptingSessions record.processEpoch state
  if record.resumeToken /= token
    then rejectSession Session.ApplicationResumeTokenMismatch
    else
      if lastObserved > record.lastIssuedReplyCursor
        then rejectSession Session.ApplicationReplyCursorNotIssued
        else
          if lastObserved < record.bindingEstablishedBy
            then retryResume record state
            else advanceResume record state

advanceResume ::
  SessionRecord ->
  State ->
  Either ApplicationSessionTransitionError (State, ApplicationSessionAcceptance)
advanceResume record state = do
  if applicationMembershipGateClosed state then Left ApplicationSessionGatePending else Right ()
  checkedLiveRecord record state
  let currentBinding = Session.advanceSessionBinding record.binding
      cursor = Request.nextApplicationReplyCursor record.lastIssuedReplyCursor
      acceptance =
        Session.applicationSessionAcceptance
          currentBinding
          cursor
          ( Session.SessionResumed
              record.sessionId
              (requestSummary record)
          )
      resumed =
        record
          { binding = currentBinding,
            bindingEstablishedBy = cursor,
            lastIssuedReplyCursor = cursor,
            retainedDisposition = acceptance,
            deliveryLive = True,
            recovery = Nothing
          }
  Right
    ( state
        { sessionsById =
            Map.insert record.sessionId resumed state.sessionsById,
          processRecoveryByProcess =
            markProcessAvailable
              record.processEpoch
              state.processRecoveryByProcess
        },
      acceptance
    )

retryResume ::
  SessionRecord ->
  State ->
  Either ApplicationSessionTransitionError (State, ApplicationSessionAcceptance)
retryResume record state = do
  if applicationMembershipGateClosed state && not record.deliveryLive then Left ApplicationSessionGatePending else Right ()
  checkedLiveRecord record state
  let reoffered = record {deliveryLive = True, recovery = Nothing}
  Right
    ( state
        { sessionsById =
            Map.insert record.sessionId reoffered state.sessionsById,
          processRecoveryByProcess =
            markProcessAvailable
              record.processEpoch
              state.processRecoveryByProcess
        },
      reoffered.retainedDisposition
    )

requestSummary :: SessionRecord -> ApplicationRequestSummary
requestSummary record =
  Request.applicationRequestSummary
    [ Request.applicationRequestSummaryEntry
        requestRecord.requestId
        (Request.retainedReplyBodyStatus requestRecord.replyBody)
    | requestRecord <- Map.elems record.requests
    ]

bindingLossSuccessor ::
  ApplicationRecoveryConfiguration ->
  MonotonicInstant ->
  ApplicationSessionBinding ->
  State ->
  (State, ApplicationBindingLossDisposition)
bindingLossSuccessor configuration observedAt lostBinding state =
  case Map.lookup session state.sessionsById of
    Just record
      | record.binding == lostBinding,
        record.deliveryLive,
        record.recovery == Nothing ->
          let generation =
                maybe
                  firstApplicationSessionRecoveryGeneration
                  nextApplicationSessionRecoveryGeneration
                  record.lastRecoveryGeneration
              attemptGeneration = firstTimerAttemptGeneration
              freshDeadline =
                monotonicInstant
                  ( monotonicInstantWord64 observedAt
                      + applicationRecoveryGraceMicroseconds configuration
                  )
              (deadline, claims) = retainInitialClaimDeadline record.attachment freshDeadline state.initialClaimsByAttachment
              recovery =
                SessionRecovery
                  { generation,
                    deadline,
                    timerAttempt = attemptGeneration
                  }
              detached =
                record
                  { deliveryLive = False,
                    lastRecoveryGeneration = Just generation,
                    recovery = Just recovery
                  }
              withSession =
                state
                  { sessionsById =
                      Map.insert session detached state.sessionsById,
                    initialClaimsByAttachment = claims
                  }
              successor = beginProcessRecoveryIfNeeded record.processEpoch withSession
              attempt =
                applicationSessionRecoveryTimerAttempt
                  session
                  generation
                  attemptGeneration
           in ( successor,
                ApplicationRecoveryStarted attempt (absoluteTimerSpec deadline)
              )
    _ -> (state, ApplicationBindingLossStale)
  where
    session = Session.sessionBindingSessionId lostBinding

applicationSessionTimerAttempt :: ApplicationSessionId -> State -> Maybe TimerAttempt
applicationSessionTimerAttempt session state = do
  record <- Map.lookup session state.sessionsById
  recovery <- record.recovery
  pure
    ( applicationSessionRecoveryTimerAttempt
        session
        recovery.generation
        recovery.timerAttempt
    )

beginProcessRecoveryIfNeeded :: ProcessEpochId -> State -> State
beginProcessRecoveryIfNeeded process state
  | processHasAttachedSession process state =
      state
        { processRecoveryByProcess =
            markProcessAvailable process state.processRecoveryByProcess
        }
  | processHasRecoveringSession process state =
      state
        { processRecoveryByProcess =
            Map.alter begin process state.processRecoveryByProcess
        }
  | otherwise = state
  where
    begin Nothing = Just (startProcessRecovery initialApplicationProcessRecovery)
    begin (Just recovery) = Just $ case recovery.phase of
      ApplicationProcessAvailable -> startProcessRecovery recovery
      ApplicationProcessRecovering _ -> recovery
      ApplicationProcessSealed _ -> recovery

startProcessRecovery :: ApplicationProcessRecovery -> ApplicationProcessRecovery
startProcessRecovery recovery =
  recovery
    { lastGeneration = Just generation,
      phase = ApplicationProcessRecovering generation
    }
  where
    generation =
      maybe
        firstApplicationProcessRecoveryGeneration
        nextApplicationProcessRecoveryGeneration
        recovery.lastGeneration

processHasAttachedSession :: ProcessEpochId -> State -> Bool
processHasAttachedSession process =
  any
    (\record -> record.processEpoch == process && record.deliveryLive)
    . Map.elems
    . (.sessionsById)

processHasRecoveringSession :: ProcessEpochId -> State -> Bool
processHasRecoveringSession process =
  any
    (\record -> record.processEpoch == process && record.recovery /= Nothing)
    . Map.elems
    . (.sessionsById)

endSessionTransition ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  State ->
  Either ApplicationSessionTransitionError (State, [WaitId])
endSessionTransition session currentBinding state = do
  record <- liveSession session state
  if Session.sessionBindingSessionId currentBinding /= session
    || record.binding /= currentBinding
    then rejectSession Session.ApplicationEndSessionBindingMismatch
    else do
      checkedLiveRecord record state
      if not record.deliveryLive
        then rejectSession Session.ApplicationEndSessionBindingMismatch
        else pure ()
      let retryKey = (record.processEpoch, record.nonce)
      if Map.lookup retryKey state.openSessionByNonce /= Just session
        then
          sessionInvariant
            (ApplicationSessionRetryIndexContradiction session)
        else
          let withoutSession =
                state
                  { sessionsById = Map.delete session state.sessionsById,
                    preparationSessionChanges = Set.insert session state.preparationSessionChanges,
                    openSessionByNonce =
                      Map.delete retryKey state.openSessionByNonce,
                    labelCandidates =
                      Map.filterWithKey
                        (\key _ -> applicationRequestKeySession key /= session)
                        state.labelCandidates,
                    gatedIngress =
                      Map.filterWithKey
                        (\key _ -> applicationRequestKeySession key /= session)
                        state.gatedIngress,
                    acceptedFenceHeld =
                      detachFenceHeldReplyLinks
                        ((== session) . applicationRequestKeySession)
                        state.acceptedFenceHeld,
                    pendingEnvironments =
                      detachPendingEnvironmentReplies
                        ( maybe
                            False
                            ((== session) . Environment.environmentReplyKeySession)
                            . Environment.pendingEnvironmentReplyKey
                        )
                        state.pendingEnvironments,
                    completedEnvironments =
                      detachCompletedEnvironmentReplies
                        ( maybe
                            False
                            ((== session) . Environment.environmentReplyKeySession)
                            . Environment.completedEnvironmentReplyKey
                        )
                        state.completedEnvironments
                  }
              successor =
                if processHasRecoveringSession record.processEpoch withoutSession
                  then beginProcessRecoveryIfNeeded record.processEpoch withoutSession
                  else
                    withoutSession
                      { processRecoveryByProcess =
                          markProcessAvailable
                            record.processEpoch
                            withoutSession.processRecoveryByProcess
                      }
           in Right (successor, pendingWaitIds record)

detachFenceHeldReplyLinks ::
  (ApplicationRequestKey -> Bool) ->
  Map LabelProcessAcceptancePosition FenceHeldRecord ->
  Map LabelProcessAcceptancePosition FenceHeldRecord
detachFenceHeldReplyLinks shouldDetach =
  Map.map
    ( \held@(FenceHeldRecord position operation work key replyLink probes) ->
        if replyLink /= Nothing && shouldDetach key
          then
            FenceHeldRecord
              position
              operation
              work
              key
              Nothing
              probes
          else held
    )

detachPendingEnvironmentReplies ::
  (PendingEnvironment -> Bool) ->
  Map ProcessAcceptancePosition PendingEnvironment ->
  Map ProcessAcceptancePosition PendingEnvironment
detachPendingEnvironmentReplies shouldDetach =
  Map.map
    ( \pending ->
        if shouldDetach pending
          then Environment.detachPendingEnvironmentReply pending
          else pending
    )

detachCompletedEnvironmentReplies ::
  (CompletedEnvironment -> Bool) ->
  Map ProcessAcceptancePosition CompletedEnvironment ->
  Map ProcessAcceptancePosition CompletedEnvironment
detachCompletedEnvironmentReplies shouldDetach =
  Map.map
    ( \completed ->
        if shouldDetach completed
          then Environment.detachCompletedEnvironmentReply completed
          else completed
    )

pendingWaitIds :: SessionRecord -> [WaitId]
pendingWaitIds record =
  [ wait
  | requestRecord <- Map.elems record.requests,
    Request.ApplicationRequestPending wait <-
      [Request.retainedReplyBodyStatus requestRecord.replyBody]
  ]

liveSession ::
  ApplicationSessionId ->
  State ->
  Either ApplicationSessionTransitionError SessionRecord
liveSession session state =
  maybe
    (rejectSession Session.ApplicationSessionNotLive)
    Right
    (Map.lookup session state.sessionsById)

checkedLiveRecord ::
  SessionRecord ->
  State ->
  Either ApplicationSessionTransitionError ()
checkedLiveRecord record state
  | Map.lookup record.attachment state.processByAttachment
      /= Just record.processEpoch =
      sessionInvariant
        (ApplicationSessionBootstrapContradiction record.processEpoch)
  | Map.notMember record.processEpoch state.accessByProcess =
      sessionInvariant
        (ApplicationSessionBootstrapContradiction record.processEpoch)
  | otherwise = Right ()

checkedBootstrapAccess ::
  ProcessEpochId ->
  State ->
  Either ApplicationSessionTransitionError BootstrapAccess
checkedBootstrapAccess process state =
  case Map.lookup process state.accessByProcess of
    Just access
      | access.processEpoch == process -> Right access
    _ -> sessionInvariant (ApplicationSessionBootstrapContradiction process)

-- | Convert one authoritative checked bootstrap access record to the opaque
-- application-facing startup bundle.
--
-- The ordinary bootstrap path makes this total.  Returning the leaf invariant
-- explicitly lets whole-state validation diagnose a deliberately corrupted
-- fixture without duplicating the canonical root-pair conversion.
applicationStartupAccessForBootstrap ::
  BootstrapAccess ->
  Either ApplicationSessionInvariant ApplicationStartupAccess
applicationStartupAccessForBootstrap access =
  Right (applicationStartupAccess access.process access.primordial)

collectRootPairs ::
  ProcessEpochId ->
  [RootAccess] ->
  Either ApplicationSessionInvariant [ApplicationAccess.PredefinedAccess]
collectRootPairs _ [] = Right []
collectRootPairs
  process
  (WriterAccess writerRole writer _ : ReaderAccess readerRole reader : remaining)
    | writerRole == readerRole = do
        rest <- collectRootPairs process remaining
        Right
          ( predefinedAccess
              (applicationPredefinedRole writerRole)
              writer
              reader
              : rest
          )
collectRootPairs process _ =
  Left (ApplicationSessionBootstrapContradiction process)

applicationPredefinedRole ::
  PredefinedSortRole ->
  ApplicationPredefinedSortRole
applicationPredefinedRole = \case
  Startup.SortDefinitionRole -> ApplicationAccess.SortDefinitionRole
  Startup.NeutralVertexRole -> ApplicationAccess.NeutralVertexRole
  Startup.EdgeRole -> ApplicationAccess.EdgeRole
  Startup.NablaRole -> ApplicationAccess.NablaRole
  Startup.DeltaRole -> ApplicationAccess.DeltaRole
  Startup.ProcessEpochRole -> ApplicationAccess.ProcessEpochRole

rejectSession ::
  ApplicationSessionRejection ->
  Either ApplicationSessionTransitionError value
rejectSession = Left . ApplicationSessionRejected

sessionInvariant ::
  ApplicationSessionInvariant ->
  Either ApplicationSessionTransitionError value
sessionInvariant = Left . ApplicationSessionInvariantFault

data ApplicationBootstrapError
  = ApplicationProcessAlreadyInstalled ProcessEpochId
  | ApplicationBootstrapAccessInvalid StartupAccessError
  | ApplicationBootstrapRootShapeInvalid ProcessEpochId
  | ApplicationAttachmentAlreadyInstalled ApplicationAttachment
  | ApplicationPrivateIdentityContradiction ProcessEpochId PrivateIdentityError
  deriving stock (Eq, Show)

-- | Complete checked application-owner successor plus its stable access record.
newtype PreparedApplicationBootstrap
  = PreparedApplicationBootstrap (Prepared State BootstrapAccess)

-- | Prepare process registration followed by canonical process/root localization.
--
-- The caller supplies roots in checked catalogue order.  This leaf preserves the
-- order exactly and always allocates the process object before the first root.
prepareApplicationBootstrap ::
  ApplicationAttachment ->
  ProcessEpochId ->
  [ApplicationRoot] ->
  GlobalObjectId ->
  [(GlobalObjectId, EdgePayload)] ->
  State ->
  Either ApplicationBootstrapError PreparedApplicationBootstrap
prepareApplicationBootstrap attachment process roots hub edges state =
  PreparedApplicationBootstrap
    <$> prepareTransition
      (installApplicationBootstrap attachment process roots (Just (hub, fmap fst edges)))
      state

-- | Register a process with explicitly empty primordial access. Configured
-- process preparation uses this before selecting or creating an environment.
prepareApplicationProcessRegistration :: ApplicationAttachment -> ProcessEpochId -> State -> Either ApplicationBootstrapError PreparedApplicationBootstrap
prepareApplicationProcessRegistration attachment process state =
  PreparedApplicationBootstrap <$> prepareTransition (installApplicationBootstrap attachment process [] Nothing) state

commitApplicationBootstrap ::
  PreparedApplicationBootstrap ->
  (State, BootstrapAccess)
commitApplicationBootstrap (PreparedApplicationBootstrap prepared) =
  commitPrepared prepared

installApplicationBootstrap ::
  ApplicationAttachment ->
  ProcessEpochId ->
  [ApplicationRoot] ->
  Maybe (GlobalObjectId, [GlobalObjectId]) ->
  State ->
  Either ApplicationBootstrapError (State, BootstrapAccess)
installApplicationBootstrap attachment process roots wiring state
  | Map.member process state.accessByProcess =
      Left (ApplicationProcessAlreadyInstalled process)
  | Map.member attachment state.processByAttachment =
      Left (ApplicationAttachmentAlreadyInstalled attachment)
  | otherwise = do
      registered <-
        commitProcessRegistration
          <$> mapPrivateIdentityError
            process
            (prepareProcessRegistration process state.privateIdentity)
      (withProcess, privateProcess) <-
        commitLocalization
          <$> mapPrivateIdentityError
            process
            ( prepareLocalization
                process
                ( globalUniqueIdFromGlobalObjectId
                    (globalObjectIdFromProcessEpochId process)
                )
                registered
            )
      (withRoots, rootHandles) <-
        foldM
          (localizeRoot process)
          (withProcess, [])
          roots
      (successorIdentity, wiringHandles) <- case wiring of
        Nothing -> Right (withRoots, [])
        Just (hub, edges) -> foldM (localizeBootstrapObject process) (withRoots, []) (hub : edges)
      selected <- case (reverse rootHandles, reverse wiringHandles) of
        ([], []) ->
          either
            (Left . ApplicationBootstrapAccessInvalid)
            Right
            (ApplicationAccess.primordialAccess [] Set.empty Nothing)
        (retainedRoots, hub : edges) -> do
          pairs <- either (const (Left (ApplicationBootstrapRootShapeInvalid process))) Right (collectRootPairs process retainedRoots)
          environment <- either (Left . ApplicationBootstrapAccessInvalid) Right (environmentAccess pairs hub edges)
          let selection = ApplicationAccess.selectEnvironment environment
              endpointSorts = Map.fromList (fmap rootSort roots)
              objectSorts =
                Map.fromList
                  ( (ApplicationAccess.environmentHubKey, SortProfile.profileSortFor SortProfile.NeutralVertexRole)
                      : [(ApplicationAccess.environmentEdgeKey role direction, SortProfile.profileSortFor SortProfile.EdgeRole) | role <- ApplicationAccess.allApplicationPredefinedSortRoles, direction <- ApplicationAccess.allEnvironmentEdgeRoles]
                  )
          either
            (Left . ApplicationBootstrapAccessInvalid)
            Right
            ( ApplicationAccess.primordialAccessWithSorts
                (Map.toAscList (ApplicationAccess.selectionEntries selection))
                (ApplicationAccess.selectionRequiredEndpoints selection)
                (ApplicationAccess.selectionEnvironmentSources selection)
                endpointSorts
                objectSorts
            )
        _ -> Left (ApplicationBootstrapRootShapeInvalid process)
      let access =
            BootstrapAccess
              { processEpoch = process,
                process = asPrivateProcessId privateProcess,
                roots = reverse rootHandles,
                primordial = selected
              }
          successor =
            State
              { privateIdentity = successorIdentity,
                accessByProcess =
                  Map.insert process access state.accessByProcess,
                processByAttachment =
                  Map.insert attachment process state.processByAttachment,
                sessionsById = state.sessionsById,
                preparationSessionChanges = state.preparationSessionChanges,
                openSessionByNonce = state.openSessionByNonce,
                processRecoveryByProcess = state.processRecoveryByProcess,
                initialClaimsByAttachment = state.initialClaimsByAttachment,
                nextSessionOrdinal = state.nextSessionOrdinal,
                nextAcceptanceByProcess =
                  Map.insert process 1 state.nextAcceptanceByProcess,
                labelCandidates = state.labelCandidates,
                gatedIngress = state.gatedIngress,
                acceptedFenceHeld = state.acceptedFenceHeld,
                pendingEnvironments = state.pendingEnvironments,
                completedEnvironments = state.completedEnvironments,
                applicationGate = state.applicationGate,
                activeLabels = state.activeLabels,
                labelByObject = state.labelByObject,
                endpointAdmissionHolds = state.endpointAdmissionHolds
              }
      Right (successor, access)
  where
    rootSort = \case
      ApplicationWriterRoot role _ _ -> (ApplicationAccess.environmentWriterKey (applicationPredefinedRole role), SortProfile.profileSortFor role)
      ApplicationReaderRoot role _ -> (ApplicationAccess.environmentReaderKey (applicationPredefinedRole role), SortProfile.profileSortFor role)

localizeBootstrapObject :: ProcessEpochId -> (PrivateIdentity, [PrivateObjectId]) -> GlobalObjectId -> Either ApplicationBootstrapError (PrivateIdentity, [PrivateObjectId])
localizeBootstrapObject process (identities, objects) object = do
  (successor, private) <- commitLocalization <$> mapPrivateIdentityError process (prepareLocalization process (globalUniqueIdFromGlobalObjectId object) identities)
  Right (successor, asPrivateObjectId private : objects)

localizeRoot ::
  ProcessEpochId ->
  (PrivateIdentity, [RootAccess]) ->
  ApplicationRoot ->
  Either ApplicationBootstrapError (PrivateIdentity, [RootAccess])
localizeRoot process (identityState, accumulated) root = do
  let globalIdentity =
        globalUniqueIdFromGlobalObjectId $ case root of
          ApplicationWriterRoot _ nabla _ -> globalObjectIdFromNablaId nabla
          ApplicationReaderRoot _ delta -> globalObjectIdFromDeltaId delta
  (successor, privateIdentity) <-
    commitLocalization
      <$> mapPrivateIdentityError
        process
        (prepareLocalization process globalIdentity identityState)
  let access = case root of
        ApplicationWriterRoot role _ sequencing ->
          WriterAccess role (asPrivateNablaId privateIdentity) sequencing
        ApplicationReaderRoot role _ ->
          ReaderAccess role (asPrivateDeltaId privateIdentity)
  Right (successor, access : accumulated)

mapPrivateIdentityError ::
  ProcessEpochId ->
  Either PrivateIdentityError value ->
  Either ApplicationBootstrapError value
mapPrivateIdentityError process =
  either (Left . ApplicationPrivateIdentityContradiction process) Right

-- | Install exactly the admitted selection into a fresh stable private namespace.
-- The composed caller commits the associated Controlled grants in the same step.
prepareApplicationPrimordialBootstrap ::
  ApplicationAttachment ->
  ProcessEpochId ->
  Primordial.CheckedPrimordialGrant ->
  State ->
  Either ApplicationBootstrapError PreparedApplicationBootstrap
prepareApplicationPrimordialBootstrap attachment process grant state =
  PreparedApplicationBootstrap <$> prepareTransition install state
  where
    install predecessor = do
      (registered, emptyAccess) <- installApplicationBootstrap attachment process [] Nothing predecessor
      (identities, entries) <- foldM localize (registered.privateIdentity, []) (Map.toAscList (Primordial.primordialGrantEntries grant))
      selected <-
        either
          (Left . ApplicationBootstrapAccessInvalid)
          Right
          (ApplicationAccess.primordialAccessWithSorts (reverse entries) (Primordial.primordialGrantRequiredEndpoints grant) (Primordial.primordialGrantEnvironmentSources grant) endpointSorts objectSorts)
      let access = emptyAccess {primordial = selected}
      Right (registered {privateIdentity = identities, accessByProcess = Map.insert process access registered.accessByProcess}, access)
    objectSorts = Map.mapMaybe (\case Primordial.GlobalObject _ sortId -> Just sortId; _ -> Nothing) (Primordial.primordialGrantEntries grant)
    endpointSorts = Map.mapMaybe endpointSort (Primordial.primordialGrantEntries grant)
    endpointSort = \case
      Primordial.GlobalWriter _ sortId -> Just sortId
      Primordial.GlobalReader _ sortId -> Just sortId
      _ -> Nothing
    localize (identities, entries) (key, entry) = do
      (successor, private) <-
        commitLocalization
          <$> mapPrivateIdentityError
            process
            (prepareLocalization process (Primordial.globalPrimordialIdentity entry) identities)
      let localized = case entry of
            Primordial.GlobalIdentity _ -> ApplicationAccess.Identity private
            Primordial.GlobalObject _ _ -> ApplicationAccess.Object (asPrivateObjectId private)
            Primordial.GlobalWriter _ _ -> ApplicationAccess.Writer (asPrivateNablaId private)
            Primordial.GlobalReader _ _ -> ApplicationAccess.Reader (asPrivateDeltaId private)
            Primordial.GlobalProcess _ -> ApplicationAccess.Process (asPrivateProcessId private)
      Right (successor, (key, localized) : entries)

-- | Child attachment installation is claim-only. Genesis bootstrap keeps its
-- independent ordinary OpenSession contract.
prepareApplicationChildBootstrap :: ApplicationAttachment -> ProcessEpochId -> Primordial.CheckedPrimordialGrant -> State -> Either ApplicationBootstrapError PreparedApplicationBootstrap
prepareApplicationChildBootstrap attachment process grant state =
  PreparedApplicationBootstrap <$> prepareTransition install state
  where
    install predecessor = do
      prepared <- prepareApplicationPrimordialBootstrap attachment process grant predecessor
      let (successor, access) = commitApplicationBootstrap prepared
      Right (successor {initialClaimsByAttachment = Map.insert attachment (InitialClaimRecord process (ApplicationInitialClaimUnclaimed Set.empty)) successor.initialClaimsByAttachment}, access)

newtype PreparedApplicationProcessAlias = PreparedApplicationProcessAlias (Prepared State PrivateProcessId)

prepareApplicationProcessAlias :: ProcessEpochId -> ProcessEpochId -> State -> Either ApplicationBootstrapError PreparedApplicationProcessAlias
prepareApplicationProcessAlias parent child state =
  PreparedApplicationProcessAlias <$> prepareTransition localize state
  where
    localize predecessor = do
      prepared <- mapPrivateIdentityError parent (prepareLocalization parent (globalUniqueIdForProcessEpoch child) predecessor.privateIdentity)
      let (identities, private) = commitLocalization prepared
      Right (predecessor {privateIdentity = identities}, asPrivateProcessId private)

preparedApplicationProcessAlias :: PreparedApplicationProcessAlias -> PrivateProcessId
preparedApplicationProcessAlias (PreparedApplicationProcessAlias prepared) = preparedOutput prepared

commitApplicationProcessAlias :: PreparedApplicationProcessAlias -> (State, PrivateProcessId)
commitApplicationProcessAlias (PreparedApplicationProcessAlias prepared) = commitPrepared prepared

data ApplicationInitialClaimError
  = ApplicationInitialClaimRejected StartupError
  | ApplicationInitialClaimSessionError ApplicationSessionTransitionError
  deriving stock (Eq, Show)

data ApplicationInitialClaimOutcome
  = ApplicationInitialClaimPending
  | ApplicationInitialClaimOpened ApplicationSessionAcceptance
  deriving stock (Eq, Show)

data PreparedApplicationInitialClaim = PreparedApplicationInitialClaim (Prepared State ApplicationInitialClaimOutcome) (Maybe TimerAttempt)

-- | Admit the explicit genesis launcher's first uniform connection without
-- allocating another process, namespace, or root set. The composed caller
-- supplies the already-checked sole genesis attachment and process.
prepareApplicationGenesisInitialClaim :: MonotonicInstant -> HeraldEpoch -> ApplicationAttachment -> ProcessEpochId -> InitialClaimId -> State -> Either ApplicationInitialClaimError PreparedApplicationInitialClaim
prepareApplicationGenesisInitialClaim observedAt herald attachment process claim state
  | Map.lookup attachment state.processByAttachment /= Just process = Left (ApplicationInitialClaimRejected StartupNotLive)
  | Map.member attachment state.initialClaimsByAttachment = prepareApplicationInitialClaim observedAt herald attachment claim True state
  | any ((== process) . (.processEpoch)) (Map.elems state.sessionsById) = Left (ApplicationInitialClaimRejected StartupAlreadyClaimed)
  | otherwise =
      prepareApplicationInitialClaim
        observedAt
        herald
        attachment
        claim
        True
        state {initialClaimsByAttachment = Map.insert attachment (InitialClaimRecord process (ApplicationInitialClaimUnclaimed Set.empty)) state.initialClaimsByAttachment}

-- | Readiness is established by the composed preparation owner. Pending attempts
-- merely retain correlations; only the first ready transition consumes a claim.
prepareApplicationInitialClaim :: MonotonicInstant -> HeraldEpoch -> ApplicationAttachment -> InitialClaimId -> Bool -> State -> Either ApplicationInitialClaimError PreparedApplicationInitialClaim
prepareApplicationInitialClaim observedAt herald attachment claim ready state = do
  prepared <- prepareTransition transition state
  let cancellation = case preparedOutput prepared of
        ApplicationInitialClaimPending -> Nothing
        ApplicationInitialClaimOpened acceptance -> applicationSessionTimerAttempt (Session.sessionBindingSessionId (Session.sessionAcceptanceBinding acceptance)) state
  Right (PreparedApplicationInitialClaim prepared cancellation)
  where
    transition predecessor = do
      InitialClaimRecord process status <- maybe (rejected StartupUnknownAttachment) Right (Map.lookup attachment predecessor.initialClaimsByAttachment)
      if Map.lookup attachment predecessor.processByAttachment /= Just process
        then rejected StartupNotLive
        else Right ()
      case status of
        ApplicationInitialClaimUnclaimed pending
          | not ready || applicationMembershipGateClosed predecessor -> Right (predecessor {initialClaimsByAttachment = Map.insert attachment (InitialClaimRecord process (ApplicationInitialClaimUnclaimed (Set.insert claim pending))) predecessor.initialClaimsByAttachment}, ApplicationInitialClaimPending)
          | otherwise -> do
              accepted (requireProcessAcceptingSessions process predecessor)
              access <- accepted (checkedBootstrapAccess process predecessor)
              startup <- accepted (either sessionInvariant Right (applicationStartupAccessForBootstrap access))
              (successor, acceptance) <- accepted (newOpen herald process attachment (Session.clientNonce 0) startup predecessor)
              let record = InitialClaimRecord process (ApplicationInitialClaimClaimed claim acceptance Nothing False)
              Right (successor {initialClaimsByAttachment = Map.insert attachment record successor.initialClaimsByAttachment}, ApplicationInitialClaimOpened acceptance)
        ApplicationInitialClaimClaimed winner acceptance deadline resumed
          | maybe False (observedAt >=) deadline -> rejected StartupSessionExpired
          | Map.notMember session predecessor.sessionsById -> rejected StartupSessionExpired
          | claim /= winner || resumed -> rejected StartupAlreadyClaimed
          | applicationMembershipGateClosed predecessor && applicationSessionDeliveryLive session predecessor /= Just True -> Right (predecessor, ApplicationInitialClaimPending)
          | otherwise -> do
              accepted (requireProcessAcceptingSessions process predecessor)
              (successor, _) <- accepted (retryOpen process attachment (Session.clientNonce 0) session predecessor)
              Right (successor, ApplicationInitialClaimOpened acceptance)
          where
            session = Session.sessionBindingSessionId (Session.sessionAcceptanceBinding acceptance)
    rejected = Left . ApplicationInitialClaimRejected
    accepted = either (Left . ApplicationInitialClaimSessionError) Right

preparedApplicationInitialClaimOutcome :: PreparedApplicationInitialClaim -> ApplicationInitialClaimOutcome
preparedApplicationInitialClaimOutcome (PreparedApplicationInitialClaim prepared _) = preparedOutput prepared

preparedApplicationInitialClaimTimerCancellation :: PreparedApplicationInitialClaim -> Maybe TimerAttempt
preparedApplicationInitialClaimTimerCancellation (PreparedApplicationInitialClaim _ cancellation) = cancellation

commitApplicationInitialClaim :: PreparedApplicationInitialClaim -> (State, ApplicationInitialClaimOutcome)
commitApplicationInitialClaim (PreparedApplicationInitialClaim prepared _) = commitPrepared prepared

applicationInitialClaimIsClaimed :: ApplicationAttachment -> State -> Bool
applicationInitialClaimIsClaimed attachment state = case Map.lookup attachment state.initialClaimsByAttachment of
  Just (InitialClaimRecord _ (ApplicationInitialClaimClaimed {})) -> True
  _ -> False

applicationInitialClaimEntries :: State -> [(ApplicationAttachment, ProcessEpochId, ApplicationInitialClaimStatus)]
applicationInitialClaimEntries state = [(attachment, process, status) | (attachment, InitialClaimRecord process status) <- Map.toAscList state.initialClaimsByAttachment]

retainInitialClaimDeadline :: ApplicationAttachment -> MonotonicInstant -> Map ApplicationAttachment InitialClaimRecord -> (MonotonicInstant, Map ApplicationAttachment InitialClaimRecord)
retainInitialClaimDeadline attachment fresh claims = case Map.lookup attachment claims of
  Just (InitialClaimRecord process (ApplicationInitialClaimClaimed claim acceptance retained False)) ->
    let deadline = maybe fresh (min fresh) retained
        record = InitialClaimRecord process (ApplicationInitialClaimClaimed claim acceptance (Just deadline) False)
     in (deadline, Map.insert attachment record claims)
  _ -> (fresh, claims)
