-- | Test-only projection driver for the independent disappearance reference.
-- It translates immutable views for the existing private owner fixtures and
-- owns no transition rules. Live integration tests use the sole Oracle kernel.
module DisappearanceReferenceDriver
  ( TargetState,
    TargetCommand,
    TargetInvalidationReason (..),
    TargetAbortReason,
    TargetCompleteEvidenceDigest,
    TargetEnvelope,
    TargetAcceptedResult (..),
    TargetRejection (..),
    TargetReceiptResult (..),
    TargetReceipt,
    TargetProjectedProbeHeader,
    TargetProbeInvalidationView (..),
    TargetProbeAbortReasonView (..),
    TargetProjectionEvent (..),
    TargetProtocolDisposition,
    TargetSubmissionOutcome (..),
    TargetLabelTerminal (..),
    TargetContextInput,
    TargetContextResult (..),
    TargetContextOutcome (..),
    initialTargetState,
    targetOpenCommand,
    targetReportCommand,
    targetInvalidateCommand,
    targetResolveCommand,
    targetAbortCommand,
    targetAuthorizedAbortReason,
    targetCompleteEvidenceDigest,
    targetEnvelope,
    submitTarget,
    targetReceiptRequestId,
    targetReceiptControlIndex,
    targetReceiptResult,
    targetProjectedProbeHeaderId,
    targetProjectedProbeHeaderSubject,
    targetProjectedProbeHeaderCoordinate,
    targetProjectedProbeHeaderMembership,
    targetMembershipSuccessorInput,
    targetLabelOpenInput,
    targetLabelTerminalInput,
    applyTargetContext,
    targetGreatestControlIndex,
    targetCurrentMembership,
    targetProbe,
  )
where

import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceClaim,
    DisappearanceEvidenceDigest,
    DisappearanceOpenResult,
    DisappearanceProbeId,
    DisappearanceResolutionOutcome,
    DisappearanceSubject,
    DisappearanceSubjectMembershipCoordinate,
  )
import Eclips.Domain.Identity (ControlIndex, GlobalObjectId, HeraldEpoch, LabelDecisionId, SystemId)
import Eclips.Domain.Label (LabelRevision)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    heraldMembershipGenerationId,
  )
import Eclips.Oracle.Identity (OracleClientRequestId)
import Eclips.Oracle.Step16.DisappearanceReference qualified as Reference

type TargetState = Reference.ReferenceState
type TargetCommand = Reference.ReferenceCommand
type TargetAbortReason = Reference.ReferenceAbortReason
type TargetCompleteEvidenceDigest = Reference.ReferenceCompleteEvidenceDigest
type TargetEnvelope = Reference.ReferenceEnvelope
type TargetProjectedProbeHeader = Reference.ReferenceProjectedProbeHeader
type TargetProtocolDisposition = Reference.ReferenceProtocolProblem

data TargetInvalidationReason = TargetMatchingPublicationObserved | TargetEvidenceContradicted
  deriving stock (Eq, Show)

data TargetAcceptedResult
  = TargetOpenAccepted DisappearanceOpenResult
  | TargetReportAccepted DisappearanceProbeId HeraldEpoch
  | TargetInvalidateAccepted DisappearanceProbeId
  | TargetResolveAccepted DisappearanceResolutionOutcome
  | TargetAbortAccepted DisappearanceProbeId
  deriving stock (Eq, Show)

data TargetRejection
  = TargetCommandRejected Reference.ReferenceRejection
  | TargetContextRejectedReason Reference.ReferenceContextProblem
  deriving stock (Eq, Show)

data TargetReceiptResult
  = TargetReceiptAccepted TargetAcceptedResult
  | TargetReceiptRejected TargetRejection
  deriving stock (Eq, Show)

newtype TargetReceipt = TargetReceipt Reference.ReferenceReceipt
  deriving stock (Eq, Show)

data TargetProbeInvalidationView
  = TargetCommandInvalidation HeraldEpoch TargetInvalidationReason DisappearanceEvidenceDigest
  | TargetLabelOpenInvalidation LabelDecisionId
  deriving stock (Eq, Show)

data TargetProbeAbortReasonView
  = TargetExplicitAbortReasonView TargetAbortReason
  | TargetMembershipSupersededAbortView HeraldMembershipGenerationId
  deriving stock (Eq, Show)

data TargetProjectionEvent
  = TargetOpenProjected TargetProjectedProbeHeader DisappearanceOpenResult
  | TargetReportProjected DisappearanceEvidenceClaim
  | TargetInvalidatedProjected DisappearanceProbeId TargetProbeInvalidationView
  | TargetResolvedProjected DisappearanceProbeId DisappearanceResolutionOutcome
  | TargetAbortedProjected DisappearanceProbeId TargetProbeAbortReasonView
  | TargetMembershipAdvancedProjected HeraldMembershipGeneration
  | TargetLabelOpenedProjected GlobalObjectId LabelDecisionId
  | TargetLabelTerminalProjected LabelDecisionId GlobalObjectId TargetLabelTerminal
  deriving stock (Eq, Show)

data TargetSubmissionOutcome
  = TargetSubmissionCommitted TargetReceipt [TargetProjectionEvent]
  | TargetSubmissionDuplicate TargetReceipt
  | TargetSubmissionProtocolRejected TargetProtocolDisposition
  deriving stock (Eq, Show)

data TargetLabelTerminal
  = TargetLabelNotApplied
  | TargetLabelReleased LabelRevision
  | TargetLabelDeleted LabelRevision
  deriving stock (Eq, Show)

data TargetContextInput
  = MembershipInput HeraldMembershipGeneration
  | LabelOpenInput DisappearanceSubject LabelDecisionId
  | LabelTerminalInput LabelDecisionId TargetLabelTerminal

data TargetContextResult
  = TargetMembershipSuccessorAccepted HeraldMembershipGenerationId
  | TargetLabelOpenAccepted LabelDecisionId
  | TargetLabelTerminalAccepted TargetLabelTerminal
  deriving stock (Eq, Show)

data TargetContextOutcome
  = TargetContextCommitted ControlIndex TargetContextResult [TargetProjectionEvent]
  | TargetContextDuplicate ControlIndex
  | TargetContextRejected ControlIndex TargetRejection
  deriving stock (Eq, Show)

initialTargetState :: SystemId -> HeraldMembershipGeneration -> TargetState
initialTargetState = Reference.initialReferenceState

targetOpenCommand :: DisappearanceSubject -> DisappearanceSubjectMembershipCoordinate -> TargetCommand
targetOpenCommand = Reference.referenceOpenDisappearanceProbeCommand

targetReportCommand :: DisappearanceProbeId -> DisappearanceEvidenceClaim -> TargetCommand
targetReportCommand = Reference.referenceReportPredefinedAbsenceCommand

targetInvalidateCommand :: DisappearanceProbeId -> HeraldEpoch -> TargetInvalidationReason -> DisappearanceEvidenceDigest -> TargetCommand
targetInvalidateCommand probe reporter reason = Reference.referenceInvalidateDisappearanceProbeCommand probe reporter (toReason reason)

targetResolveCommand :: DisappearanceProbeId -> TargetCompleteEvidenceDigest -> TargetCommand
targetResolveCommand = Reference.referenceResolveDisappearanceProbeCommand

targetAbortCommand :: DisappearanceProbeId -> TargetAbortReason -> TargetCommand
targetAbortCommand = Reference.referenceAbortDisappearanceProbeCommand

targetAuthorizedAbortReason :: TargetAbortReason
targetAuthorizedAbortReason = Reference.referenceAuthorizedAbortReason

targetCompleteEvidenceDigest :: [DisappearanceEvidenceClaim] -> TargetCompleteEvidenceDigest
targetCompleteEvidenceDigest = Reference.deriveReferenceCompleteEvidenceDigest

targetEnvelope :: OracleClientRequestId -> Maybe ControlIndex -> HeraldEpoch -> TargetCommand -> TargetEnvelope
targetEnvelope = Reference.referenceEnvelope

submitTarget :: TargetEnvelope -> TargetState -> (TargetState, TargetSubmissionOutcome)
submitTarget envelope state =
  let (successor, outcome) = Reference.submitReferenceEnvelope envelope state
   in ( successor,
        case Reference.referenceSubmissionOutcomeView outcome of
          Reference.ReferenceSubmissionCommittedView receipt events ->
            TargetSubmissionCommitted (TargetReceipt receipt) (fmap eventView events)
          Reference.ReferenceSubmissionDuplicateView receipt -> TargetSubmissionDuplicate (TargetReceipt receipt)
          Reference.ReferenceSubmissionProtocolRejectedView problem -> TargetSubmissionProtocolRejected problem
      )

targetReceiptRequestId :: TargetReceipt -> OracleClientRequestId
targetReceiptRequestId (TargetReceipt receipt) = case Reference.referenceReceiptView receipt of
  Reference.ReferenceReceiptView request _ _ _ -> request

targetReceiptControlIndex :: TargetReceipt -> ControlIndex
targetReceiptControlIndex (TargetReceipt receipt) = case Reference.referenceReceiptView receipt of
  Reference.ReferenceReceiptView _ _ index _ -> index

targetReceiptResult :: TargetReceipt -> TargetReceiptResult
targetReceiptResult (TargetReceipt receipt) = case Reference.referenceReceiptView receipt of
  Reference.ReferenceReceiptView _ _ _ result -> case result of
    Reference.ReferenceReceiptRejectedView problem -> TargetReceiptRejected (TargetCommandRejected problem)
    Reference.ReferenceReceiptAcceptedView accepted -> TargetReceiptAccepted $ case accepted of
      Reference.ReferenceOpenAcceptedView opened -> TargetOpenAccepted opened
      Reference.ReferenceReportAcceptedView probe reporter -> TargetReportAccepted probe reporter
      Reference.ReferenceInvalidationAcceptedView probe -> TargetInvalidateAccepted probe
      Reference.ReferenceResolutionAcceptedView outcome -> TargetResolveAccepted outcome
      Reference.ReferenceAbortAcceptedView probe -> TargetAbortAccepted probe

targetProjectedProbeHeaderId :: TargetProjectedProbeHeader -> DisappearanceProbeId
targetProjectedProbeHeaderId = Reference.referenceProjectedProbeHeaderId

targetProjectedProbeHeaderSubject :: TargetProjectedProbeHeader -> DisappearanceSubject
targetProjectedProbeHeaderSubject = Reference.referenceProjectedProbeHeaderSubject

targetProjectedProbeHeaderCoordinate :: TargetProjectedProbeHeader -> DisappearanceSubjectMembershipCoordinate
targetProjectedProbeHeaderCoordinate = Reference.referenceProjectedProbeHeaderCoordinate

targetProjectedProbeHeaderMembership :: TargetProjectedProbeHeader -> HeraldMembershipGeneration
targetProjectedProbeHeaderMembership = Reference.referenceProjectedProbeHeaderMembership

targetMembershipSuccessorInput :: HeraldMembershipGeneration -> TargetContextInput
targetMembershipSuccessorInput = MembershipInput

targetLabelOpenInput :: DisappearanceSubject -> LabelDecisionId -> TargetContextInput
targetLabelOpenInput = LabelOpenInput

targetLabelTerminalInput :: LabelDecisionId -> TargetLabelTerminal -> TargetContextInput
targetLabelTerminalInput = LabelTerminalInput

applyTargetContext :: TargetContextInput -> TargetState -> (TargetState, TargetContextOutcome)
applyTargetContext input state =
  let (successor, outcome, events) = case input of
        MembershipInput membership -> Reference.applyReferenceMembershipSuccessor membership state
        LabelOpenInput subject decision -> Reference.applyReferenceLabelOpen decision subject state
        LabelTerminalInput decision terminal -> Reference.applyReferenceLabelTerminal decision (toLabel terminal) state
      index = Reference.referenceGreatestControlIndex successor
      accepted = case input of
        MembershipInput membership -> TargetMembershipSuccessorAccepted (heraldMembershipGenerationId membership)
        LabelOpenInput _ decision -> TargetLabelOpenAccepted decision
        LabelTerminalInput _ terminal -> TargetLabelTerminalAccepted terminal
   in ( successor,
        case outcome of
          Reference.ReferenceContextApplied -> TargetContextCommitted index accepted (fmap (eventView . Reference.referenceProjectionEventView) events)
          Reference.ReferenceContextDuplicate -> TargetContextDuplicate index
          Reference.ReferenceContextRejected problem -> TargetContextRejected index (TargetContextRejectedReason problem)
      )

targetGreatestControlIndex :: TargetState -> ControlIndex
targetGreatestControlIndex = Reference.referenceGreatestControlIndex

targetCurrentMembership :: TargetState -> HeraldMembershipGeneration
targetCurrentMembership = Reference.referenceCurrentMembership

targetProbe :: DisappearanceProbeId -> TargetState -> Maybe Reference.ReferenceProbeView
targetProbe = Reference.referenceProbe

eventView :: Reference.ReferenceProjectionEventView -> TargetProjectionEvent
eventView = \case
  Reference.ReferenceProbeOpenProjectedView header result -> TargetOpenProjected header result
  Reference.ReferenceReportProjectedView claim -> TargetReportProjected claim
  Reference.ReferenceInvalidationProjectedView probe reason _ ->
    TargetInvalidatedProjected
      probe
      ( case reason of
          Reference.ReferenceCommandInvalidationView reporter cause witness -> TargetCommandInvalidation reporter (fromReason cause) witness
          Reference.ReferenceLabelWorkflowOpenedInvalidationView decision -> TargetLabelOpenInvalidation decision
      )
  Reference.ReferenceResolutionProjectedView probe outcome -> TargetResolvedProjected probe outcome
  Reference.ReferenceAbortProjectedView probe reason _ ->
    TargetAbortedProjected
      probe
      ( case reason of
          Reference.ReferenceAuthorizedAbortView -> TargetExplicitAbortReasonView targetAuthorizedAbortReason
          Reference.ReferenceMembershipSupersededView generation -> TargetMembershipSupersededAbortView generation
      )
  Reference.ReferenceMembershipProjectedView membership -> TargetMembershipAdvancedProjected membership
  Reference.ReferenceLabelOpenProjectedView decision object -> TargetLabelOpenedProjected object decision
  Reference.ReferenceLabelTerminalProjectedView decision object terminal -> TargetLabelTerminalProjected decision object (fromLabel terminal)

toReason :: TargetInvalidationReason -> Reference.ReferenceInvalidationReason
toReason TargetMatchingPublicationObserved = Reference.ReferenceMatchingPublication
toReason TargetEvidenceContradicted = Reference.ReferenceInvalidEvidence

fromReason :: Reference.ReferenceInvalidationReason -> TargetInvalidationReason
fromReason Reference.ReferenceMatchingPublication = TargetMatchingPublicationObserved
fromReason Reference.ReferenceInvalidEvidence = TargetEvidenceContradicted

toLabel :: TargetLabelTerminal -> Reference.ReferenceLabelTerminalDisposition
toLabel TargetLabelNotApplied = Reference.ReferenceLabelNotApplied
toLabel (TargetLabelReleased revision) = Reference.ReferenceLabelReleased revision
toLabel (TargetLabelDeleted revision) = Reference.ReferenceLabelDeleted revision

fromLabel :: Reference.ReferenceLabelTerminalDisposition -> TargetLabelTerminal
fromLabel Reference.ReferenceLabelNotApplied = TargetLabelNotApplied
fromLabel (Reference.ReferenceLabelReleased revision) = TargetLabelReleased revision
fromLabel (Reference.ReferenceLabelDeleted revision) = TargetLabelDeleted revision
