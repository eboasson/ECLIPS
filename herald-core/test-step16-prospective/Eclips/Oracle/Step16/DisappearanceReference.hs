{-# LANGUAGE OverloadedStrings #-}

-- | Independent, deliberately simple Step-16 disappearance reference model.
--
-- The state uses association lists and recomputes its secondary views.  It does
-- not import the prospective target and deliberately shares with it only the
-- already checked Domain and Oracle identity vocabulary.  The module is a
-- temporary, one-way-canonical test seam; it defines no wire decoder.
module Eclips.Oracle.Step16.DisappearanceReference
  ( ReferenceInvalidationReason (..),
    ReferenceAbortReason,
    referenceAuthorizedAbortReason,
    ReferenceCompleteEvidenceDigest,
    deriveReferenceCompleteEvidenceDigest,
    referenceCompleteEvidenceDigestBytes,
    ReferenceCommand,
    ReferenceCommandView (..),
    referenceOpenDisappearanceProbeCommand,
    referenceReportPredefinedAbsenceCommand,
    referenceInvalidateDisappearanceProbeCommand,
    referenceResolveDisappearanceProbeCommand,
    referenceAbortDisappearanceProbeCommand,
    referenceCommandView,
    referenceCommandTag,
    referenceCommandCanonicalBytes,
    referenceCommandDigestBytes,
    ReferenceEnvelope,
    referenceEnvelope,
    referenceEnvelopeRequestId,
    referenceEnvelopeExpectedControlIndex,
    referenceEnvelopeHomeHeraldEpoch,
    referenceEnvelopeCommand,
    referenceEnvelopeCanonicalBytes,
    referenceEnvelopeDigestBytes,
    ReferenceInvalidationCauseView (..),
    ReferenceProbeAbortReasonView (..),
    ReferenceProbePhaseView (..),
    ReferenceProbeView (..),
    ReferenceAcceptedResult,
    ReferenceAcceptedResultView (..),
    referenceAcceptedResultView,
    ReferenceProbePhaseClass (..),
    ReferenceRejection (..),
    ReferenceReceiptResult,
    ReferenceReceiptResultView (..),
    referenceReceiptResultView,
    ReferenceReceipt,
    ReferenceReceiptView (..),
    referenceReceiptView,
    referenceReceiptCanonicalBytes,
    ReferenceProjectedProbeHeader,
    referenceProjectedProbeHeaderId,
    referenceProjectedProbeHeaderSubject,
    referenceProjectedProbeHeaderCoordinate,
    referenceProjectedProbeHeaderMembership,
    ReferenceProjectionEvent,
    ReferenceProjectionEventView (..),
    referenceProjectionEventView,
    referenceProjectionEventCanonicalBytes,
    referenceProjectionEventsCanonicalBytes,
    ReferenceProtocolProblem (..),
    ReferenceSubmissionOutcome,
    ReferenceSubmissionOutcomeView (..),
    referenceSubmissionOutcomeView,
    referenceSubmissionProjectionEventsCanonicalBytes,
    ReferenceLabelTerminalDisposition (..),
    ReferenceContextProblem (..),
    ReferenceContextOutcome (..),
    ReferenceState,
    initialReferenceState,
    submitReferenceEnvelope,
    applyReferenceMembershipSuccessor,
    applyReferenceAtomicLabelDecision,
    applyReferenceLabelCompletion,
    applyReferenceLabelOpen,
    applyReferenceLabelTerminal,
    referenceGreatestControlIndex,
    referenceCurrentMembership,
    referenceRequestCount,
    referenceRequestReceipt,
    referenceOpenRequestResult,
    referenceActiveProbe,
    referenceProbe,
    referenceProbes,
    referenceRegularRetirementIndex,
    referenceCurrentLabelRevision,
    referenceOpenLabelDecision,
    referenceControlledSubjectDeleted,
    referenceStateCanonicalBytes,
    referenceStateDigestBytes,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (sort, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word8)
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceClaim,
    DisappearanceEvidenceDigest,
    DisappearanceOpenResult,
    DisappearanceOpenResultView (..),
    DisappearanceProbeId,
    DisappearanceResolutionOutcome,
    DisappearanceResolutionOutcomeView (..),
    DisappearanceSubject,
    DisappearanceSubjectMembershipCoordinate,
    DisappearanceSubjectView (..),
    aliasedDisappearanceProbe,
    deriveDisappearanceProbeId,
    disappearanceCoordinateMemberSetDigest,
    disappearanceCoordinateMembershipGenerationId,
    disappearanceCoordinateSubjectDigest,
    disappearanceEvidenceClaimCanonicalBytes,
    disappearanceEvidenceClaimCoordinate,
    disappearanceEvidenceClaimProbeId,
    disappearanceEvidenceClaimReporter,
    disappearanceEvidenceDigestBytes,
    disappearanceOpenResultView,
    disappearanceProbeIdBytes,
    disappearanceResolutionOutcomeCanonicalBytes,
    disappearanceResolutionOutcomeView,
    disappearanceSubjectCanonicalBytes,
    disappearanceSubjectDigestBytes,
    disappearanceSubjectMembershipCoordinate,
    disappearanceSubjectView,
    openedDisappearanceProbe,
    resolveDisappearanceSubject,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    SortDefinitionOccurrenceId,
    SortId,
    SystemId,
    controlIndex,
    controlIndexWord64,
    globalObjectIdBytes,
    heraldEpochBytes,
    labelDecisionIdBytes,
    sortDefinitionOccurrenceIdBytes,
    sortIdBytes,
    systemIdBytes,
  )
import Eclips.Domain.Label
  ( LabelRevision,
    labelRevisionControlIndex,
  )
import Eclips.Domain.MemberSet (memberSetDigestBytes)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationCanonicalBytes,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
    heraldMembershipGenerationPredecessor,
    heraldMembershipGenerationRetiredHeraldEpoch,
    heraldMembershipGenerationRetirementControlIndex,
    heraldMembershipGenerationRetirementId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
    resolvedRetirementOccurrenceBase,
  )
import Eclips.Oracle.Identity
  ( OracleClientRequestId,
    oracleClientRequestHome,
    oracleClientRequestSequence,
  )

-- NOTE: keep these six domains byte-for-byte aligned with the prospective
-- target.  They deliberately differ from the current live Oracle domains.
commandDomain, envelopeDomain, completeEvidenceDomain, receiptDomain, eventDomain, stateDomain :: ByteString
commandDomain = "ECLIPS-STEP16-DISAPPEARANCE-COMMAND"
envelopeDomain = "ECLIPS-STEP16-DISAPPEARANCE-ENVELOPE"
completeEvidenceDomain = "ECLIPS-STEP16-DISAPPEARANCE-COMPLETE-EVIDENCE"
receiptDomain = "ECLIPS-STEP16-DISAPPEARANCE-RECEIPT"
eventDomain = "ECLIPS-STEP16-DISAPPEARANCE-EVENT"
stateDomain = "ECLIPS-STEP16-DISAPPEARANCE-STATE"

-- | The two Herald-observed invalidation classes admitted by the explicit
-- Invalidate command.  Label-open invalidation has a distinct internal cause.
data ReferenceInvalidationReason
  = ReferenceMatchingPublication
  | ReferenceInvalidEvidence
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | Only the explicit authorized management reason is constructible here.
-- Membership supersession is installed solely by the membership transition.
data ReferenceAbortReason = ReferenceAuthorizedAbort
  deriving stock (Eq, Ord, Show)

referenceAuthorizedAbortReason :: ReferenceAbortReason
referenceAuthorizedAbortReason = ReferenceAuthorizedAbort

newtype ReferenceCompleteEvidenceDigest
  = ReferenceCompleteEvidenceDigest ByteString
  deriving stock (Eq, Ord, Show)

deriveReferenceCompleteEvidenceDigest ::
  [DisappearanceEvidenceClaim] -> ReferenceCompleteEvidenceDigest
deriveReferenceCompleteEvidenceDigest claims =
  ReferenceCompleteEvidenceDigest
    . SHA256.hash
    . buildCanonical
    $ framedBytes completeEvidenceDomain
      <> Builder.word64BE (fromIntegral (length normalized))
      <> foldMap
        (framedBytes . disappearanceEvidenceClaimCanonicalBytes)
        normalized
  where
    normalized =
      sortOn
        ( \claim ->
            ( disappearanceEvidenceClaimReporter claim,
              disappearanceEvidenceClaimCanonicalBytes claim
            )
        )
        claims

referenceCompleteEvidenceDigestBytes ::
  ReferenceCompleteEvidenceDigest -> ByteString
referenceCompleteEvidenceDigestBytes
  (ReferenceCompleteEvidenceDigest bytes) = bytes

data ReferenceCommand
  = ReferenceOpenDisappearanceProbe
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
  | ReferenceReportPredefinedAbsence
      DisappearanceProbeId
      DisappearanceEvidenceClaim
  | ReferenceInvalidateDisappearanceProbe
      DisappearanceProbeId
      HeraldEpoch
      ReferenceInvalidationReason
      DisappearanceEvidenceDigest
  | ReferenceResolveDisappearanceProbe
      DisappearanceProbeId
      ReferenceCompleteEvidenceDigest
  | ReferenceAbortDisappearanceProbe
      DisappearanceProbeId
      ReferenceAbortReason
  deriving stock (Eq, Show)

data ReferenceCommandView
  = ReferenceOpenDisappearanceProbeView
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
  | ReferenceReportPredefinedAbsenceView
      DisappearanceProbeId
      DisappearanceEvidenceClaim
  | ReferenceInvalidateDisappearanceProbeView
      DisappearanceProbeId
      HeraldEpoch
      ReferenceInvalidationReason
      DisappearanceEvidenceDigest
  | ReferenceResolveDisappearanceProbeView
      DisappearanceProbeId
      ReferenceCompleteEvidenceDigest
  | ReferenceAbortDisappearanceProbeView
      DisappearanceProbeId
      ReferenceAbortReason
  deriving stock (Eq, Show)

referenceOpenDisappearanceProbeCommand ::
  DisappearanceSubject ->
  DisappearanceSubjectMembershipCoordinate ->
  ReferenceCommand
referenceOpenDisappearanceProbeCommand = ReferenceOpenDisappearanceProbe

referenceReportPredefinedAbsenceCommand ::
  DisappearanceProbeId -> DisappearanceEvidenceClaim -> ReferenceCommand
referenceReportPredefinedAbsenceCommand = ReferenceReportPredefinedAbsence

referenceInvalidateDisappearanceProbeCommand ::
  DisappearanceProbeId ->
  HeraldEpoch ->
  ReferenceInvalidationReason ->
  DisappearanceEvidenceDigest ->
  ReferenceCommand
referenceInvalidateDisappearanceProbeCommand =
  ReferenceInvalidateDisappearanceProbe

referenceResolveDisappearanceProbeCommand ::
  DisappearanceProbeId ->
  ReferenceCompleteEvidenceDigest ->
  ReferenceCommand
referenceResolveDisappearanceProbeCommand = ReferenceResolveDisappearanceProbe

referenceAbortDisappearanceProbeCommand ::
  DisappearanceProbeId -> ReferenceAbortReason -> ReferenceCommand
referenceAbortDisappearanceProbeCommand = ReferenceAbortDisappearanceProbe

referenceCommandView :: ReferenceCommand -> ReferenceCommandView
referenceCommandView command = case command of
  ReferenceOpenDisappearanceProbe subject coordinate ->
    ReferenceOpenDisappearanceProbeView subject coordinate
  ReferenceReportPredefinedAbsence probe claim ->
    ReferenceReportPredefinedAbsenceView probe claim
  ReferenceInvalidateDisappearanceProbe probe reporter reason witness ->
    ReferenceInvalidateDisappearanceProbeView probe reporter reason witness
  ReferenceResolveDisappearanceProbe probe digest ->
    ReferenceResolveDisappearanceProbeView probe digest
  ReferenceAbortDisappearanceProbe probe reason ->
    ReferenceAbortDisappearanceProbeView probe reason

referenceCommandTag :: ReferenceCommand -> Word8
referenceCommandTag command = case command of
  ReferenceOpenDisappearanceProbe {} -> 13
  ReferenceReportPredefinedAbsence {} -> 14
  ReferenceInvalidateDisappearanceProbe {} -> 15
  ReferenceResolveDisappearanceProbe {} -> 16
  ReferenceAbortDisappearanceProbe {} -> 17

referenceCommandCanonicalBytes :: ReferenceCommand -> ByteString
referenceCommandCanonicalBytes command =
  buildCanonical
    ( framedBytes commandDomain
        <> Builder.word8 (referenceCommandTag command)
        <> framedBuilder (referenceCommandPayload command)
    )

referenceCommandDigestBytes :: ReferenceCommand -> ByteString
referenceCommandDigestBytes = SHA256.hash . referenceCommandCanonicalBytes

data ReferenceEnvelope
  = ReferenceEnvelope
      OracleClientRequestId
      (Maybe ControlIndex)
      HeraldEpoch
      ReferenceCommand
  deriving stock (Eq, Show)

referenceEnvelope ::
  OracleClientRequestId ->
  Maybe ControlIndex ->
  HeraldEpoch ->
  ReferenceCommand ->
  ReferenceEnvelope
referenceEnvelope = ReferenceEnvelope

referenceEnvelopeRequestId :: ReferenceEnvelope -> OracleClientRequestId
referenceEnvelopeRequestId (ReferenceEnvelope request _ _ _) = request

referenceEnvelopeExpectedControlIndex ::
  ReferenceEnvelope -> Maybe ControlIndex
referenceEnvelopeExpectedControlIndex (ReferenceEnvelope _ expected _ _) = expected

referenceEnvelopeHomeHeraldEpoch :: ReferenceEnvelope -> HeraldEpoch
referenceEnvelopeHomeHeraldEpoch (ReferenceEnvelope _ _ home _) = home

referenceEnvelopeCommand :: ReferenceEnvelope -> ReferenceCommand
referenceEnvelopeCommand (ReferenceEnvelope _ _ _ command) = command

referenceEnvelopeCanonicalBytes :: ReferenceEnvelope -> ByteString
referenceEnvelopeCanonicalBytes
  (ReferenceEnvelope request expected home command) =
    buildCanonical
      ( framedBytes envelopeDomain
          <> requestIdBuilder request
          <> maybeControlIndexBuilder expected
          <> Builder.byteString (heraldEpochBytes home)
          <> Builder.byteString (referenceCommandCanonicalBytes command)
      )

referenceEnvelopeDigestBytes :: ReferenceEnvelope -> ByteString
referenceEnvelopeDigestBytes = SHA256.hash . referenceEnvelopeCanonicalBytes

data ReferenceInvalidationCause
  = ReferenceCommandInvalidation
      HeraldEpoch
      ReferenceInvalidationReason
      DisappearanceEvidenceDigest
  | ReferenceLabelInvalidation LabelDecisionId
  deriving stock (Eq, Show)

data ReferenceInvalidationCauseView
  = ReferenceCommandInvalidationView
      HeraldEpoch
      ReferenceInvalidationReason
      DisappearanceEvidenceDigest
  | ReferenceLabelWorkflowOpenedInvalidationView LabelDecisionId
  deriving stock (Eq, Show)

data ReferenceProbeAbortReason
  = ReferenceExplicitAbort ReferenceAbortReason
  | ReferenceMembershipAbort HeraldMembershipGenerationId
  deriving stock (Eq, Show)

data ReferenceProbeAbortReasonView
  = ReferenceAuthorizedAbortView
  | ReferenceMembershipSupersededView HeraldMembershipGenerationId
  deriving stock (Eq, Show)

data ReferenceProbePhase
  = ReferenceCollectingEvidence
  | ReferenceInvalidated
      ReferenceInvalidationCause
      ControlIndex
  | ReferenceResolved
      ControlIndex
      DisappearanceResolutionOutcome
  | ReferenceAborted
      ReferenceProbeAbortReason
      ControlIndex
  deriving stock (Eq, Show)

data ReferenceProbePhaseView
  = ReferenceCollectingEvidenceView
  | ReferenceInvalidatedView
      ReferenceInvalidationCauseView
      ControlIndex
  | ReferenceResolvedView
      ControlIndex
      DisappearanceResolutionOutcome
  | ReferenceAbortedView
      ReferenceProbeAbortReasonView
      ControlIndex
  deriving stock (Eq, Show)

data ReferenceProbeRecord
  = ReferenceProbeRecord
      DisappearanceProbeId
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
      ControlIndex
      [HeraldEpoch]
      [(HeraldEpoch, DisappearanceEvidenceClaim)]
      ReferenceProbePhase
  deriving stock (Eq, Show)

data ReferenceProbeView
  = ReferenceProbeView
      DisappearanceProbeId
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
      ControlIndex
      [HeraldEpoch]
      [(HeraldEpoch, DisappearanceEvidenceClaim)]
      ReferenceProbePhaseView
  deriving stock (Eq, Show)

data ReferenceAcceptedResult
  = ReferenceOpenAccepted DisappearanceOpenResult
  | ReferenceReportAccepted DisappearanceProbeId HeraldEpoch
  | ReferenceInvalidationAccepted DisappearanceProbeId
  | ReferenceResolutionAccepted DisappearanceResolutionOutcome
  | ReferenceAbortAccepted DisappearanceProbeId
  deriving stock (Eq, Show)

data ReferenceAcceptedResultView
  = ReferenceOpenAcceptedView DisappearanceOpenResult
  | ReferenceReportAcceptedView DisappearanceProbeId HeraldEpoch
  | ReferenceInvalidationAcceptedView DisappearanceProbeId
  | ReferenceResolutionAcceptedView DisappearanceResolutionOutcome
  | ReferenceAbortAcceptedView DisappearanceProbeId
  deriving stock (Eq, Show)

referenceAcceptedResultView ::
  ReferenceAcceptedResult -> ReferenceAcceptedResultView
referenceAcceptedResultView accepted = case accepted of
  ReferenceOpenAccepted result -> ReferenceOpenAcceptedView result
  ReferenceReportAccepted probe reporter ->
    ReferenceReportAcceptedView probe reporter
  ReferenceInvalidationAccepted probe ->
    ReferenceInvalidationAcceptedView probe
  ReferenceResolutionAccepted outcome ->
    ReferenceResolutionAcceptedView outcome
  ReferenceAbortAccepted probe -> ReferenceAbortAcceptedView probe

data ReferenceProbePhaseClass
  = ReferenceProbeInvalidatedClass
  | ReferenceProbeResolvedClass
  | ReferenceProbeAbortedClass
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data ReferenceRejection
  = ReferenceRequestHomeEpochMismatch HeraldEpoch HeraldEpoch
  | ReferenceInactiveHomeHerald HeraldEpoch
  | ReferenceStaleExpectedControlIndex ControlIndex ControlIndex
  | ReferenceSubjectMembershipCoordinateMismatch
      DisappearanceSubjectMembershipCoordinate
      DisappearanceSubjectMembershipCoordinate
  | ReferenceControlledSubjectDeleted GlobalObjectId
  | ReferenceControlledLabelRevisionMismatch
      GlobalObjectId
      (Maybe LabelRevision)
      (Maybe LabelRevision)
  | ReferenceControlledLabelWorkflowIncomplete
      GlobalObjectId
      LabelDecisionId
  | ReferenceRegularOccurrenceMismatch
      SortId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | ReferenceUnknownProbe DisappearanceProbeId
  | ReferenceProbeNotCollecting
      DisappearanceProbeId
      ReferenceProbePhaseClass
  | ReferenceReportHomeMismatch HeraldEpoch HeraldEpoch
  | ReferenceEvidenceProbeMismatch
      DisappearanceProbeId
      DisappearanceProbeId
  | ReferenceEvidenceCoordinateMismatch
      DisappearanceSubjectMembershipCoordinate
      DisappearanceSubjectMembershipCoordinate
  | ReferenceReporterNotCaptured HeraldEpoch
  | ReferenceConflictingReporterEvidence HeraldEpoch
  | ReferenceIncompleteEvidence [HeraldEpoch]
  | ReferenceCompleteEvidenceDigestMismatch
      ReferenceCompleteEvidenceDigest
      ReferenceCompleteEvidenceDigest
  deriving stock (Eq, Show)

data ReferenceReceiptResult
  = ReferenceReceiptAccepted ReferenceAcceptedResult
  | ReferenceReceiptRejected ReferenceRejection
  deriving stock (Eq, Show)

data ReferenceReceiptResultView
  = ReferenceReceiptAcceptedView ReferenceAcceptedResultView
  | ReferenceReceiptRejectedView ReferenceRejection
  deriving stock (Eq, Show)

referenceReceiptResultView ::
  ReferenceReceiptResult -> ReferenceReceiptResultView
referenceReceiptResultView result = case result of
  ReferenceReceiptAccepted accepted ->
    ReferenceReceiptAcceptedView (referenceAcceptedResultView accepted)
  ReferenceReceiptRejected rejection ->
    ReferenceReceiptRejectedView rejection

data ReferenceReceipt
  = ReferenceReceipt
      OracleClientRequestId
      ByteString
      ControlIndex
      ReferenceReceiptResult
  deriving stock (Eq, Show)

data ReferenceReceiptView
  = ReferenceReceiptView
      OracleClientRequestId
      ByteString
      ControlIndex
      ReferenceReceiptResultView
  deriving stock (Eq, Show)

referenceReceiptView :: ReferenceReceipt -> ReferenceReceiptView
referenceReceiptView
  (ReferenceReceipt request digest index result) =
    ReferenceReceiptView
      request
      digest
      index
      (referenceReceiptResultView result)

data ReferenceProjectedProbeHeader
  = ReferenceProjectedProbeHeader
      DisappearanceProbeId
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
      HeraldMembershipGeneration
  deriving stock (Eq, Show)

referenceProjectedProbeHeaderId :: ReferenceProjectedProbeHeader -> DisappearanceProbeId
referenceProjectedProbeHeaderId (ReferenceProjectedProbeHeader probe _ _ _) = probe

referenceProjectedProbeHeaderSubject :: ReferenceProjectedProbeHeader -> DisappearanceSubject
referenceProjectedProbeHeaderSubject (ReferenceProjectedProbeHeader _ subject _ _) = subject

referenceProjectedProbeHeaderCoordinate ::
  ReferenceProjectedProbeHeader -> DisappearanceSubjectMembershipCoordinate
referenceProjectedProbeHeaderCoordinate (ReferenceProjectedProbeHeader _ _ coordinate _) = coordinate

referenceProjectedProbeHeaderMembership ::
  ReferenceProjectedProbeHeader -> HeraldMembershipGeneration
referenceProjectedProbeHeaderMembership (ReferenceProjectedProbeHeader _ _ _ membership) = membership

data ReferenceProjectionEvent
  = ReferenceProbeOpenProjected ReferenceProjectedProbeHeader DisappearanceOpenResult
  | ReferenceReportProjected DisappearanceEvidenceClaim
  | ReferenceInvalidationProjected
      DisappearanceProbeId
      ReferenceInvalidationCause
      ControlIndex
  | ReferenceResolutionProjected
      DisappearanceProbeId
      DisappearanceResolutionOutcome
  | ReferenceAbortProjected
      DisappearanceProbeId
      ReferenceProbeAbortReason
      ControlIndex
  | ReferenceMembershipProjected HeraldMembershipGeneration
  | ReferenceLabelOpenProjected LabelDecisionId GlobalObjectId
  | ReferenceLabelTerminalProjected
      LabelDecisionId
      GlobalObjectId
      ReferenceLabelTerminalDisposition
  deriving stock (Eq, Show)

data ReferenceProjectionEventView
  = ReferenceProbeOpenProjectedView ReferenceProjectedProbeHeader DisappearanceOpenResult
  | ReferenceReportProjectedView DisappearanceEvidenceClaim
  | ReferenceInvalidationProjectedView
      DisappearanceProbeId
      ReferenceInvalidationCauseView
      ControlIndex
  | ReferenceResolutionProjectedView
      DisappearanceProbeId
      DisappearanceResolutionOutcome
  | ReferenceAbortProjectedView
      DisappearanceProbeId
      ReferenceProbeAbortReasonView
      ControlIndex
  | ReferenceMembershipProjectedView HeraldMembershipGeneration
  | ReferenceLabelOpenProjectedView LabelDecisionId GlobalObjectId
  | ReferenceLabelTerminalProjectedView
      LabelDecisionId
      GlobalObjectId
      ReferenceLabelTerminalDisposition
  deriving stock (Eq, Show)

referenceProjectionEventView ::
  ReferenceProjectionEvent -> ReferenceProjectionEventView
referenceProjectionEventView event = case event of
  ReferenceProbeOpenProjected header result ->
    ReferenceProbeOpenProjectedView header result
  ReferenceReportProjected claim ->
    ReferenceReportProjectedView claim
  ReferenceInvalidationProjected probe reason index ->
    ReferenceInvalidationProjectedView
      probe
      (referenceInvalidationCauseView reason)
      index
  ReferenceResolutionProjected probe outcome ->
    ReferenceResolutionProjectedView probe outcome
  ReferenceAbortProjected probe reason index ->
    ReferenceAbortProjectedView
      probe
      (referenceProbeAbortReasonView reason)
      index
  ReferenceMembershipProjected membership ->
    ReferenceMembershipProjectedView membership
  ReferenceLabelOpenProjected decision object ->
    ReferenceLabelOpenProjectedView decision object
  ReferenceLabelTerminalProjected decision object disposition ->
    ReferenceLabelTerminalProjectedView decision object disposition

data ReferenceProtocolProblem
  = ReferenceConflictingRequestId
      OracleClientRequestId
      ByteString
      ByteString
  deriving stock (Eq, Show)

data ReferenceSubmissionOutcome
  = ReferenceSubmissionCommitted
      ReferenceReceipt
      [ReferenceProjectionEvent]
  | ReferenceSubmissionDuplicate ReferenceReceipt
  | ReferenceSubmissionProtocolRejected ReferenceProtocolProblem
  deriving stock (Eq, Show)

data ReferenceSubmissionOutcomeView
  = ReferenceSubmissionCommittedView
      ReferenceReceipt
      [ReferenceProjectionEventView]
  | ReferenceSubmissionDuplicateView ReferenceReceipt
  | ReferenceSubmissionProtocolRejectedView ReferenceProtocolProblem
  deriving stock (Eq, Show)

referenceSubmissionOutcomeView ::
  ReferenceSubmissionOutcome -> ReferenceSubmissionOutcomeView
referenceSubmissionOutcomeView outcome = case outcome of
  ReferenceSubmissionCommitted receipt events ->
    ReferenceSubmissionCommittedView
      receipt
      (fmap referenceProjectionEventView events)
  ReferenceSubmissionDuplicate receipt ->
    ReferenceSubmissionDuplicateView receipt
  ReferenceSubmissionProtocolRejected problem ->
    ReferenceSubmissionProtocolRejectedView problem

referenceSubmissionProjectionEventsCanonicalBytes ::
  ReferenceSubmissionOutcome -> ByteString
referenceSubmissionProjectionEventsCanonicalBytes outcome =
  referenceProjectionEventsCanonicalBytes events
  where
    events = case outcome of
      ReferenceSubmissionCommitted _ projected -> projected
      ReferenceSubmissionDuplicate {} -> []
      ReferenceSubmissionProtocolRejected {} -> []

data ReferenceLabelTerminalDisposition
  = ReferenceLabelNotApplied
  | ReferenceLabelReleased LabelRevision
  | ReferenceLabelDeleted LabelRevision
  deriving stock (Eq, Show)

data ReferenceContextProblem
  = ReferenceMembershipSuccessorMissingPredecessor
  | ReferenceMembershipPredecessorMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | ReferenceMembershipSuccessorMissingControlIndex
  | ReferenceMembershipSuccessorMissingRetiredHerald
  | ReferenceMembershipSuccessorControlIndexMismatch ControlIndex ControlIndex
  | ReferenceMembershipSuccessorMalformed
  | ReferenceLabelSubjectMustBeControlled
  | ReferenceLabelSubjectAlreadyDeleted GlobalObjectId
  | ReferenceLabelWorkflowAlreadyOpen GlobalObjectId LabelDecisionId
  | ReferenceLabelExpectedRevisionMismatch
      GlobalObjectId
      (Maybe LabelRevision)
      (Maybe LabelRevision)
  | ReferenceLabelWorkflowUnknown LabelDecisionId
  | ReferenceLabelTerminalRevisionIndexMismatch ControlIndex ControlIndex
  deriving stock (Eq, Show)

data ReferenceContextOutcome
  = ReferenceContextApplied
  | ReferenceContextDuplicate
  | ReferenceContextRejected ReferenceContextProblem
  deriving stock (Eq, Show)

data ReferenceSubjectKey
  = ReferenceSubjectKey
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
  deriving stock (Eq, Ord, Show)

data ReferenceRequestRecord
  = ReferenceRequestRecord ReferenceEnvelope ReferenceReceipt
  deriving stock (Eq, Show)

data ReferenceState
  = ReferenceState
      SystemId
      ControlIndex
      HeraldMembershipGeneration
      [(OracleClientRequestId, ReferenceRequestRecord)]
      [(ReferenceSubjectKey, DisappearanceProbeId)]
      [(DisappearanceProbeId, ReferenceProbeRecord)]
      [(SortId, ControlIndex)]
      [(GlobalObjectId, LabelRevision)]
      [(GlobalObjectId, LabelDecisionId)]
      [GlobalObjectId]
  deriving stock (Eq, Show)

initialReferenceState ::
  SystemId -> HeraldMembershipGeneration -> ReferenceState
initialReferenceState system membership =
  ReferenceState
    system
    (controlIndex 0)
    membership
    []
    []
    []
    []
    []
    []
    []

submitReferenceEnvelope ::
  ReferenceEnvelope ->
  ReferenceState ->
  (ReferenceState, ReferenceSubmissionOutcome)
submitReferenceEnvelope envelope state =
  case lookup (referenceEnvelopeRequestId envelope) (stateRequests state) of
    Just (ReferenceRequestRecord retained retainedReceipt)
      | retained == envelope ->
          (state, ReferenceSubmissionDuplicate retainedReceipt)
      | otherwise ->
          ( state,
            ReferenceSubmissionProtocolRejected
              ( ReferenceConflictingRequestId
                  (referenceEnvelopeRequestId envelope)
                  (referenceCommandDigestBytes (referenceEnvelopeCommand retained))
                  (referenceCommandDigestBytes (referenceEnvelopeCommand envelope))
              )
          )
    Nothing -> commitFreshReferenceEnvelope envelope state

commitFreshReferenceEnvelope ::
  ReferenceEnvelope ->
  ReferenceState ->
  (ReferenceState, ReferenceSubmissionOutcome)
commitFreshReferenceEnvelope envelope state =
  (successor, ReferenceSubmissionCommitted receipt events)
  where
    index = nextControlIndex state
    (semanticState, result, events) =
      case validateReferenceEnvelope envelope state of
        Left rejection ->
          (state, ReferenceReceiptRejected rejection, [])
        Right () -> case applyReferenceCommand index envelope state of
          Left rejection ->
            (state, ReferenceReceiptRejected rejection, [])
          Right (updated, accepted, projected) ->
            (updated, ReferenceReceiptAccepted accepted, projected)
    receipt =
      ReferenceReceipt
        (referenceEnvelopeRequestId envelope)
        (referenceCommandDigestBytes (referenceEnvelopeCommand envelope))
        index
        result
    successor =
      setStateRequestAndIndex
        index
        (referenceEnvelopeRequestId envelope)
        (ReferenceRequestRecord envelope receipt)
        semanticState

validateReferenceEnvelope ::
  ReferenceEnvelope -> ReferenceState -> Either ReferenceRejection ()
validateReferenceEnvelope envelope state = do
  let requestHome = oracleClientRequestHome (referenceEnvelopeRequestId envelope)
      suppliedHome = referenceEnvelopeHomeHeraldEpoch envelope
  if requestHome == suppliedHome
    then Right ()
    else
      Left
        (ReferenceRequestHomeEpochMismatch requestHome suppliedHome)
  if suppliedHome `elem` referenceActiveMembers state
    then Right ()
    else Left (ReferenceInactiveHomeHerald suppliedHome)
  case referenceEnvelopeExpectedControlIndex envelope of
    Nothing -> Right ()
    Just expected
      | expected == referenceGreatestControlIndex state -> Right ()
      | otherwise ->
          Left
            ( ReferenceStaleExpectedControlIndex
                expected
                (referenceGreatestControlIndex state)
            )

applyReferenceCommand ::
  ControlIndex ->
  ReferenceEnvelope ->
  ReferenceState ->
  Either
    ReferenceRejection
    (ReferenceState, ReferenceAcceptedResult, [ReferenceProjectionEvent])
applyReferenceCommand index envelope state =
  case referenceEnvelopeCommand envelope of
    ReferenceOpenDisappearanceProbe subject coordinate ->
      applyReferenceOpen index subject coordinate state
    ReferenceReportPredefinedAbsence probe claim ->
      applyReferenceReport
        probe
        claim
        (referenceEnvelopeHomeHeraldEpoch envelope)
        state
    ReferenceInvalidateDisappearanceProbe probe reporter reason witness ->
      applyReferenceInvalidate
        index
        probe
        reporter
        reason
        witness
        (referenceEnvelopeHomeHeraldEpoch envelope)
        state
    ReferenceResolveDisappearanceProbe probe digest ->
      applyReferenceResolve
        index
        (referenceEnvelopeHomeHeraldEpoch envelope)
        probe
        digest
        state
    ReferenceAbortDisappearanceProbe probe reason ->
      applyReferenceAbort
        index
        (referenceEnvelopeHomeHeraldEpoch envelope)
        probe
        reason
        state

applyReferenceOpen ::
  ControlIndex ->
  DisappearanceSubject ->
  DisappearanceSubjectMembershipCoordinate ->
  ReferenceState ->
  Either
    ReferenceRejection
    (ReferenceState, ReferenceAcceptedResult, [ReferenceProjectionEvent])
applyReferenceOpen index subject suppliedCoordinate state = do
  let expectedCoordinate =
        disappearanceSubjectMembershipCoordinate
          subject
          (referenceCurrentMembership state)
  if suppliedCoordinate == expectedCoordinate
    then Right ()
    else
      Left
        ( ReferenceSubjectMembershipCoordinateMismatch
            suppliedCoordinate
            expectedCoordinate
        )
  validateReferenceSubject subject state
  let key = ReferenceSubjectKey subject suppliedCoordinate
  case lookup key (stateActive state) of
    Just probe ->
      let result = aliasedDisappearanceProbe probe
          header =
            ReferenceProjectedProbeHeader
              probe
              subject
              suppliedCoordinate
              (referenceCurrentMembership state)
       in Right
            ( state,
              ReferenceOpenAccepted result,
              [ReferenceProbeOpenProjected header result]
            )
    Nothing -> do
      probe <-
        either
          (error . ("positive Open index rejected: " <>) . show)
          Right
          (deriveDisappearanceProbeId index)
      let result = openedDisappearanceProbe probe
          header =
            ReferenceProjectedProbeHeader
              probe
              subject
              suppliedCoordinate
              (referenceCurrentMembership state)
          record =
            ReferenceProbeRecord
              probe
              subject
              suppliedCoordinate
              index
              (referenceActiveMembers state)
              []
              ReferenceCollectingEvidence
          successor =
            setStateActive
              (insertAssoc key probe (stateActive state))
              . setStateProbes
                (insertAssoc probe record (stateProbes state))
              $ state
      Right
        ( successor,
          ReferenceOpenAccepted result,
          [ReferenceProbeOpenProjected header result]
        )

validateReferenceSubject ::
  DisappearanceSubject -> ReferenceState -> Either ReferenceRejection ()
validateReferenceSubject subject state =
  case disappearanceSubjectView subject of
    ControlledPredefinedSubjectView object _ expectedRevision -> do
      if referenceControlledSubjectDeleted object state
        then Left (ReferenceControlledSubjectDeleted object)
        else Right ()
      case referenceOpenLabelDecision object state of
        Just decision ->
          Left
            ( ReferenceControlledLabelWorkflowIncomplete
                object
                decision
            )
        Nothing -> Right ()
      let currentRevision = referenceCurrentLabelRevision object state
      if expectedRevision == currentRevision
        then Right ()
        else
          Left
            ( ReferenceControlledLabelRevisionMismatch
                object
                expectedRevision
                currentRevision
            )
    RegularSortDefinitionSubjectView sortId _ suppliedOccurrence ->
      let expectedOccurrence = referenceExpectedRegularOccurrence sortId state
       in if suppliedOccurrence == expectedOccurrence
            then Right ()
            else
              Left
                ( ReferenceRegularOccurrenceMismatch
                    sortId
                    suppliedOccurrence
                    expectedOccurrence
                )

applyReferenceReport ::
  DisappearanceProbeId ->
  DisappearanceEvidenceClaim ->
  HeraldEpoch ->
  ReferenceState ->
  Either
    ReferenceRejection
    (ReferenceState, ReferenceAcceptedResult, [ReferenceProjectionEvent])
applyReferenceReport probe claim home state = do
  record <- requireCollectingProbe probe state
  let reporter = disappearanceEvidenceClaimReporter claim
  if home == reporter
    then Right ()
    else Left (ReferenceReportHomeMismatch home reporter)
  if reporter `elem` probeCapturedMembers record
    then Right ()
    else Left (ReferenceReporterNotCaptured reporter)
  if disappearanceEvidenceClaimProbeId claim == probe
    then Right ()
    else
      Left
        ( ReferenceEvidenceProbeMismatch
            (disappearanceEvidenceClaimProbeId claim)
            probe
        )
  let expectedCoordinate = probeCoordinate record
      suppliedCoordinate = disappearanceEvidenceClaimCoordinate claim
  if suppliedCoordinate == expectedCoordinate
    then Right ()
    else
      Left
        ( ReferenceEvidenceCoordinateMismatch
            suppliedCoordinate
            expectedCoordinate
        )
  case lookup reporter (probeReports record) of
    Just retained
      | retained == claim ->
          Right (state, ReferenceReportAccepted probe reporter, [])
      | otherwise -> Left (ReferenceConflictingReporterEvidence reporter)
    Nothing ->
      let reports = insertAssoc reporter claim (probeReports record)
          updated = setProbeReports reports record
          successor =
            setStateProbes
              (insertAssoc probe updated (stateProbes state))
              state
       in Right
            ( successor,
              ReferenceReportAccepted probe reporter,
              [ReferenceReportProjected claim]
            )

applyReferenceInvalidate ::
  ControlIndex ->
  DisappearanceProbeId ->
  HeraldEpoch ->
  ReferenceInvalidationReason ->
  DisappearanceEvidenceDigest ->
  HeraldEpoch ->
  ReferenceState ->
  Either
    ReferenceRejection
    (ReferenceState, ReferenceAcceptedResult, [ReferenceProjectionEvent])
applyReferenceInvalidate index probe reporter reason witness home state = do
  record <- requireReferenceProbe probe state
  let cause = ReferenceCommandInvalidation reporter reason witness
  if home == reporter
    then Right ()
    else Left (ReferenceReportHomeMismatch home reporter)
  case probePhase record of
    ReferenceInvalidated retained _
      | retained == cause ->
          Right (state, ReferenceInvalidationAccepted probe, [])
    ReferenceCollectingEvidence -> do
      if reporter `elem` probeCapturedMembers record
        then Right ()
        else Left (ReferenceReporterNotCaptured reporter)
      let updated = setProbePhase (ReferenceInvalidated cause index) record
          successor = terminalizeReferenceProbe updated state
      Right
        ( successor,
          ReferenceInvalidationAccepted probe,
          [ReferenceInvalidationProjected probe cause index]
        )
    terminal ->
      Left
        ( ReferenceProbeNotCollecting
            probe
            (referenceProbePhaseClass terminal)
        )

applyReferenceResolve ::
  ControlIndex ->
  HeraldEpoch ->
  DisappearanceProbeId ->
  ReferenceCompleteEvidenceDigest ->
  ReferenceState ->
  Either
    ReferenceRejection
    (ReferenceState, ReferenceAcceptedResult, [ReferenceProjectionEvent])
applyReferenceResolve index home probe suppliedDigest state = do
  record <- requireReferenceProbe probe state
  let expectedDigest =
        deriveReferenceCompleteEvidenceDigest
          (fmap snd (probeReports record))
  case probePhase record of
    ReferenceResolved _ retained
      | suppliedDigest == expectedDigest ->
          Right (state, ReferenceResolutionAccepted retained, [])
    ReferenceCollectingEvidence -> do
      if home `elem` probeCapturedMembers record
        then Right ()
        else Left (ReferenceReporterNotCaptured home)
      let reporters = sort (fmap fst (probeReports record))
          missing = filter (`notElem` reporters) (probeCapturedMembers record)
      if null missing
        then Right ()
        else Left (ReferenceIncompleteEvidence missing)
      if suppliedDigest == expectedDigest
        then Right ()
        else
          Left
            ( ReferenceCompleteEvidenceDigestMismatch
                suppliedDigest
                expectedDigest
            )
      outcome <-
        either
          (error . ("positive Resolve index rejected: " <>) . show)
          Right
          (resolveDisappearanceSubject (stateSystem state) index (probeSubject record))
      let updated = setProbePhase (ReferenceResolved index outcome) record
          terminal = terminalizeReferenceProbe updated state
          successor = applyReferenceResolutionIndex outcome terminal
      Right
        ( successor,
          ReferenceResolutionAccepted outcome,
          [ReferenceResolutionProjected probe outcome]
        )
    terminal ->
      Left
        ( ReferenceProbeNotCollecting
            probe
            (referenceProbePhaseClass terminal)
        )

applyReferenceAbort ::
  ControlIndex ->
  HeraldEpoch ->
  DisappearanceProbeId ->
  ReferenceAbortReason ->
  ReferenceState ->
  Either
    ReferenceRejection
    (ReferenceState, ReferenceAcceptedResult, [ReferenceProjectionEvent])
applyReferenceAbort index home probe reason state = do
  record <- requireReferenceProbe probe state
  let abortReason = ReferenceExplicitAbort reason
  case probePhase record of
    ReferenceAborted (ReferenceExplicitAbort retained) _
      | retained == reason ->
          Right (state, ReferenceAbortAccepted probe, [])
    ReferenceCollectingEvidence -> do
      if home `elem` probeCapturedMembers record
        then Right ()
        else Left (ReferenceReporterNotCaptured home)
      let updated = setProbePhase (ReferenceAborted abortReason index) record
          successor = terminalizeReferenceProbe updated state
      Right
        ( successor,
          ReferenceAbortAccepted probe,
          [ReferenceAbortProjected probe abortReason index]
        )
    terminal ->
      Left
        ( ReferenceProbeNotCollecting
            probe
            (referenceProbePhaseClass terminal)
        )

applyReferenceMembershipSuccessor ::
  HeraldMembershipGeneration ->
  ReferenceState ->
  (ReferenceState, ReferenceContextOutcome, [ReferenceProjectionEvent])
applyReferenceMembershipSuccessor successor state
  | successor == referenceCurrentMembership state =
      (state, ReferenceContextDuplicate, [])
  | otherwise =
      case validateReferenceMembershipSuccessor index successor state of
        Left problem ->
          ( setStateIndex index state,
            ReferenceContextRejected problem,
            []
          )
        Right () ->
          let collecting =
                sortOn
                  probeIdentifier
                  ( filter
                      ( \record ->
                          probeIsCollecting record
                            && disappearanceCoordinateMembershipGenerationId
                              (probeCoordinate record)
                              == heraldMembershipGenerationId
                                (referenceCurrentMembership state)
                      )
                      (fmap snd (stateProbes state))
                  )
              supersededId = heraldMembershipGenerationId successor
              abortOne record =
                setProbePhase
                  ( ReferenceAborted
                      (ReferenceMembershipAbort supersededId)
                      index
                  )
                  record
              updatedRecords = fmap abortOne collecting
              supersededProbeIds = fmap probeIdentifier updatedRecords
              retainedTerminal =
                foldr
                  (\record -> insertAssoc (probeIdentifier record) record)
                  (stateProbes state)
                  updatedRecords
              updatedState =
                setStateIndex index
                  . setStateMembership successor
                  . setStateActive
                    ( filter
                        (\(_, probe) -> probe `notElem` supersededProbeIds)
                        (stateActive state)
                    )
                  . setStateProbes retainedTerminal
                  $ state
              abortEvents =
                fmap
                  ( \record ->
                      ReferenceAbortProjected
                        (probeIdentifier record)
                        (ReferenceMembershipAbort supersededId)
                        index
                  )
                  updatedRecords
           in ( updatedState,
                ReferenceContextApplied,
                ReferenceMembershipProjected successor : abortEvents
              )
  where
    index = nextControlIndex state

validateReferenceMembershipSuccessor ::
  ControlIndex ->
  HeraldMembershipGeneration ->
  ReferenceState ->
  Either ReferenceContextProblem ()
validateReferenceMembershipSuccessor index successor state = do
  predecessor <-
    maybe
      (Left ReferenceMembershipSuccessorMissingPredecessor)
      Right
      (heraldMembershipGenerationPredecessor successor)
  let current = heraldMembershipGenerationId (referenceCurrentMembership state)
  if predecessor == current
    then Right ()
    else Left (ReferenceMembershipPredecessorMismatch predecessor current)
  retirementIndex <-
    maybe
      (Left ReferenceMembershipSuccessorMissingControlIndex)
      Right
      (heraldMembershipGenerationRetirementControlIndex successor)
  if retirementIndex == index
    then Right ()
    else
      Left
        (ReferenceMembershipSuccessorControlIndexMismatch retirementIndex index)
  retired <-
    maybe
      (Left ReferenceMembershipSuccessorMissingRetiredHerald)
      Right
      (heraldMembershipGenerationRetiredHeraldEpoch successor)
  retirement <-
    maybe
      (Left ReferenceMembershipSuccessorMalformed)
      Right
      (heraldMembershipGenerationRetirementId successor)
  expected <-
    either
      (const (Left ReferenceMembershipSuccessorMalformed))
      Right
      ( retireHeraldMembershipGeneration
          index
          retirement
          retired
          (referenceCurrentMembership state)
      )
  if expected == successor
    then Right ()
    else Left ReferenceMembershipSuccessorMalformed

-- | Atomic-label seam for the live protocol: the canonical revision and
-- disappearance invalidation occur together, while per-object installation
-- exclusion survives until the separate completion. This deliberately keeps
-- association-list recomputation independent of the production indices.
applyReferenceAtomicLabelDecision ::
  LabelDecisionId ->
  DisappearanceSubject ->
  ReferenceLabelTerminalDisposition ->
  ReferenceState ->
  (ReferenceState, ReferenceContextOutcome, [ReferenceProjectionEvent])
applyReferenceAtomicLabelDecision decision subject disposition state =
  case validateReferenceLabelTerminalIndex index disposition of
    Left problem -> (setStateIndex index state, ReferenceContextRejected problem, [])
    Right () -> case disappearanceSubjectView subject of
      RegularSortDefinitionSubjectView {} -> (setStateIndex index state, ReferenceContextRejected ReferenceLabelSubjectMustBeControlled, [])
      ControlledPredefinedSubjectView object _ _ -> case disposition of
        ReferenceLabelNotApplied -> (setStateIndex index state, ReferenceContextApplied, [ReferenceLabelTerminalProjected decision object disposition])
        ReferenceLabelReleased revision -> applySuccess object revision False
        ReferenceLabelDeleted revision -> applySuccess object revision True
  where
    index = nextControlIndex state
    applySuccess object revision deleting = case applyReferenceLabelOpen decision subject state of
      (opened, ReferenceContextApplied, _openEvent : invalidations) ->
        let deleted = if deleting then sort (object : filter (/= object) (stateDeletedObjects opened)) else stateDeletedObjects opened
            updated = setStateLabelRevisions (insertAssoc object revision (stateLabelRevisions opened)) (setStateDeletedObjects deleted opened)
         in (updated, ReferenceContextApplied, ReferenceLabelTerminalProjected decision object disposition : invalidations)
      result -> result

applyReferenceLabelCompletion ::
  LabelDecisionId -> ReferenceState -> (ReferenceState, ReferenceContextOutcome)
applyReferenceLabelCompletion decision state = case findOpenLabelObject decision (stateOpenLabels state) of
  Nothing -> (setStateIndex index state, ReferenceContextRejected (ReferenceLabelWorkflowUnknown decision))
  Just object -> (setStateIndex index (setStateOpenLabels (deleteAssoc object (stateOpenLabels state)) state), ReferenceContextApplied)
  where
    index = nextControlIndex state

applyReferenceLabelOpen ::
  LabelDecisionId ->
  DisappearanceSubject ->
  ReferenceState ->
  (ReferenceState, ReferenceContextOutcome, [ReferenceProjectionEvent])
applyReferenceLabelOpen decision subject state =
  case disappearanceSubjectView subject of
    RegularSortDefinitionSubjectView {} ->
      rejected ReferenceLabelSubjectMustBeControlled
    ControlledPredefinedSubjectView object _ expectedRevision ->
      case validateReferenceLabelOpen decision object expectedRevision state of
        Left problem -> rejected problem
        Right () ->
          let affected =
                sortOn
                  probeIdentifier
                  ( filter
                      (probeCollectingControlledObject object)
                      (fmap snd (stateProbes state))
                  )
              cause = ReferenceLabelInvalidation decision
              invalidate record =
                setProbePhase (ReferenceInvalidated cause index) record
              invalidated = fmap invalidate affected
              affectedIds = fmap probeIdentifier invalidated
              updatedProbes =
                foldr
                  (\record -> insertAssoc (probeIdentifier record) record)
                  (stateProbes state)
                  invalidated
              updated =
                setStateIndex index
                  . setStateOpenLabels
                    (insertAssoc object decision (stateOpenLabels state))
                  . setStateActive
                    ( filter
                        (\(_, probe) -> probe `notElem` affectedIds)
                        (stateActive state)
                    )
                  . setStateProbes updatedProbes
                  $ state
              invalidationEvents =
                fmap
                  ( \record ->
                      ReferenceInvalidationProjected
                        (probeIdentifier record)
                        cause
                        index
                  )
                  invalidated
           in ( updated,
                ReferenceContextApplied,
                ReferenceLabelOpenProjected decision object : invalidationEvents
              )
  where
    index = nextControlIndex state
    rejected problem =
      ( setStateIndex index state,
        ReferenceContextRejected problem,
        []
      )

validateReferenceLabelOpen ::
  LabelDecisionId ->
  GlobalObjectId ->
  Maybe LabelRevision ->
  ReferenceState ->
  Either ReferenceContextProblem ()
validateReferenceLabelOpen decision object expectedRevision state
  | referenceControlledSubjectDeleted object state =
      Left (ReferenceLabelSubjectAlreadyDeleted object)
  | otherwise = do
      case referenceOpenLabelDecision object state of
        Just retained ->
          Left (ReferenceLabelWorkflowAlreadyOpen object retained)
        Nothing -> Right ()
      let currentRevision = referenceCurrentLabelRevision object state
      if expectedRevision == currentRevision
        then Right ()
        else
          Left
            ( ReferenceLabelExpectedRevisionMismatch
                object
                expectedRevision
                currentRevision
            )
      case findOpenLabelObject decision (stateOpenLabels state) of
        Just retainedObject ->
          Left (ReferenceLabelWorkflowAlreadyOpen retainedObject decision)
        Nothing -> Right ()

applyReferenceLabelTerminal ::
  LabelDecisionId ->
  ReferenceLabelTerminalDisposition ->
  ReferenceState ->
  (ReferenceState, ReferenceContextOutcome, [ReferenceProjectionEvent])
applyReferenceLabelTerminal decision disposition state =
  case findOpenLabelObject decision (stateOpenLabels state) of
    Nothing -> rejected (ReferenceLabelWorkflowUnknown decision)
    Just object -> case validateReferenceLabelTerminalIndex index disposition of
      Left problem -> rejected problem
      Right () ->
        let withoutOpen = deleteAssoc object (stateOpenLabels state)
            withRevision = case disposition of
              ReferenceLabelNotApplied -> stateLabelRevisions state
              ReferenceLabelReleased revision ->
                insertAssoc object revision (stateLabelRevisions state)
              ReferenceLabelDeleted revision ->
                insertAssoc object revision (stateLabelRevisions state)
            deleted = case disposition of
              ReferenceLabelDeleted _ ->
                sort (object : filter (/= object) (stateDeletedObjects state))
              _ -> stateDeletedObjects state
            updated =
              setStateIndex index
                . setStateOpenLabels withoutOpen
                . setStateLabelRevisions withRevision
                . setStateDeletedObjects deleted
                $ state
         in ( updated,
              ReferenceContextApplied,
              [ReferenceLabelTerminalProjected decision object disposition]
            )
  where
    index = nextControlIndex state
    rejected problem =
      ( setStateIndex index state,
        ReferenceContextRejected problem,
        []
      )

validateReferenceLabelTerminalIndex ::
  ControlIndex ->
  ReferenceLabelTerminalDisposition ->
  Either ReferenceContextProblem ()
validateReferenceLabelTerminalIndex expected disposition =
  case disposition of
    ReferenceLabelNotApplied -> Right ()
    ReferenceLabelReleased revision -> check revision
    ReferenceLabelDeleted revision -> check revision
  where
    check revision
      | labelRevisionControlIndex revision == expected = Right ()
      | otherwise =
          Left
            ( ReferenceLabelTerminalRevisionIndexMismatch
                (labelRevisionControlIndex revision)
                expected
            )

referenceGreatestControlIndex :: ReferenceState -> ControlIndex
referenceGreatestControlIndex
  (ReferenceState _ index _ _ _ _ _ _ _ _) = index

referenceCurrentMembership ::
  ReferenceState -> HeraldMembershipGeneration
referenceCurrentMembership
  (ReferenceState _ _ membership _ _ _ _ _ _ _) = membership

referenceRequestCount :: ReferenceState -> Int
referenceRequestCount = length . stateRequests

referenceRequestReceipt ::
  OracleClientRequestId -> ReferenceState -> Maybe ReferenceReceipt
referenceRequestReceipt request state = do
  ReferenceRequestRecord _ receipt <- lookup request (stateRequests state)
  pure receipt

referenceOpenRequestResult ::
  OracleClientRequestId ->
  ReferenceState ->
  Maybe DisappearanceOpenResult
referenceOpenRequestResult request state = do
  receipt <- referenceRequestReceipt request state
  case receiptResult receipt of
    ReferenceReceiptAccepted (ReferenceOpenAccepted result) -> Just result
    _ -> Nothing

referenceActiveProbe ::
  DisappearanceSubject ->
  DisappearanceSubjectMembershipCoordinate ->
  ReferenceState ->
  Maybe DisappearanceProbeId
referenceActiveProbe subject coordinate state =
  lookup (ReferenceSubjectKey subject coordinate) (stateActive state)

referenceProbe ::
  DisappearanceProbeId -> ReferenceState -> Maybe ReferenceProbeView
referenceProbe probe state =
  referenceProbeRecordView <$> lookup probe (stateProbes state)

referenceProbes :: ReferenceState -> [ReferenceProbeView]
referenceProbes =
  fmap (referenceProbeRecordView . snd)
    . sortOn fst
    . stateProbes

referenceRegularRetirementIndex ::
  SortId -> ReferenceState -> Maybe ControlIndex
referenceRegularRetirementIndex sortId =
  lookup sortId . stateRetirements

referenceCurrentLabelRevision ::
  GlobalObjectId -> ReferenceState -> Maybe LabelRevision
referenceCurrentLabelRevision object =
  lookup object . stateLabelRevisions

referenceOpenLabelDecision ::
  GlobalObjectId -> ReferenceState -> Maybe LabelDecisionId
referenceOpenLabelDecision object =
  lookup object . stateOpenLabels

referenceControlledSubjectDeleted ::
  GlobalObjectId -> ReferenceState -> Bool
referenceControlledSubjectDeleted object =
  elem object . stateDeletedObjects

referenceProbeRecordView :: ReferenceProbeRecord -> ReferenceProbeView
referenceProbeRecordView record =
  ReferenceProbeView
    (probeIdentifier record)
    (probeSubject record)
    (probeCoordinate record)
    (probeOpenIndex record)
    (probeCapturedMembers record)
    (sortOn fst (probeReports record))
    (referenceProbePhaseView (probePhase record))

referenceProbePhaseView :: ReferenceProbePhase -> ReferenceProbePhaseView
referenceProbePhaseView phase = case phase of
  ReferenceCollectingEvidence -> ReferenceCollectingEvidenceView
  ReferenceInvalidated cause index ->
    ReferenceInvalidatedView (referenceInvalidationCauseView cause) index
  ReferenceResolved index outcome -> ReferenceResolvedView index outcome
  ReferenceAborted reason index ->
    ReferenceAbortedView (referenceProbeAbortReasonView reason) index

referenceInvalidationCauseView ::
  ReferenceInvalidationCause -> ReferenceInvalidationCauseView
referenceInvalidationCauseView cause = case cause of
  ReferenceCommandInvalidation reporter reason witness ->
    ReferenceCommandInvalidationView reporter reason witness
  ReferenceLabelInvalidation decision ->
    ReferenceLabelWorkflowOpenedInvalidationView decision

referenceProbeAbortReasonView ::
  ReferenceProbeAbortReason -> ReferenceProbeAbortReasonView
referenceProbeAbortReasonView reason = case reason of
  ReferenceExplicitAbort _ -> ReferenceAuthorizedAbortView
  ReferenceMembershipAbort successor ->
    ReferenceMembershipSupersededView successor

referenceActiveMembers :: ReferenceState -> [HeraldEpoch]
referenceActiveMembers =
  sort
    . NonEmpty.toList
    . heraldMembershipGenerationActiveHeraldEpochs
    . referenceCurrentMembership

referenceExpectedRegularOccurrence ::
  SortId -> ReferenceState -> SortDefinitionOccurrenceId
referenceExpectedRegularOccurrence sortId state =
  deriveSortDefinitionOccurrenceId (stateSystem state) sortId base
  where
    base = case referenceRegularRetirementIndex sortId state of
      Nothing -> Genesis
      Just index ->
        either
          (error . ("positive retained retirement index rejected: " <>) . show)
          id
          (resolvedRetirementOccurrenceBase index)

requireCollectingProbe ::
  DisappearanceProbeId ->
  ReferenceState ->
  Either ReferenceRejection ReferenceProbeRecord
requireCollectingProbe probe state = do
  record <- requireReferenceProbe probe state
  case probePhase record of
    ReferenceCollectingEvidence -> Right record
    phase ->
      Left
        ( ReferenceProbeNotCollecting
            probe
            (referenceProbePhaseClass phase)
        )

requireReferenceProbe ::
  DisappearanceProbeId ->
  ReferenceState ->
  Either ReferenceRejection ReferenceProbeRecord
requireReferenceProbe probe state =
  maybe
    (Left (ReferenceUnknownProbe probe))
    Right
    (lookup probe (stateProbes state))

referenceProbePhaseClass :: ReferenceProbePhase -> ReferenceProbePhaseClass
referenceProbePhaseClass phase = case phase of
  ReferenceCollectingEvidence ->
    error "collecting probe has no terminal phase class"
  ReferenceInvalidated {} -> ReferenceProbeInvalidatedClass
  ReferenceResolved {} -> ReferenceProbeResolvedClass
  ReferenceAborted {} -> ReferenceProbeAbortedClass

terminalizeReferenceProbe ::
  ReferenceProbeRecord -> ReferenceState -> ReferenceState
terminalizeReferenceProbe record state =
  setStateActive
    ( filter
        (\(_, retained) -> retained /= probeIdentifier record)
        (stateActive state)
    )
    . setStateProbes
      ( insertAssoc
          (probeIdentifier record)
          record
          (stateProbes state)
      )
    $ state

applyReferenceResolutionIndex ::
  DisappearanceResolutionOutcome -> ReferenceState -> ReferenceState
applyReferenceResolutionIndex outcome state =
  case disappearanceResolutionOutcomeView outcome of
    ControlledDisappearanceResolved object _ _ _ ->
      setStateDeletedObjects
        (sort (object : filter (/= object) (stateDeletedObjects state)))
        state
    RegularSortDefinitionRetired sortId _ _ index _ ->
      setStateRetirements
        (insertAssoc sortId index (stateRetirements state))
        state

probeCollectingControlledObject ::
  GlobalObjectId -> ReferenceProbeRecord -> Bool
probeCollectingControlledObject object record =
  probeIsCollecting record
    && case disappearanceSubjectView (probeSubject record) of
      ControlledPredefinedSubjectView retained _ _ -> retained == object
      RegularSortDefinitionSubjectView {} -> False

probeIsCollecting :: ReferenceProbeRecord -> Bool
probeIsCollecting record = case probePhase record of
  ReferenceCollectingEvidence -> True
  _ -> False

findOpenLabelObject ::
  LabelDecisionId ->
  [(GlobalObjectId, LabelDecisionId)] ->
  Maybe GlobalObjectId
findOpenLabelObject decision =
  fmap fst . findFirst ((== decision) . snd)

findFirst :: (value -> Bool) -> [value] -> Maybe value
findFirst predicate values = case values of
  [] -> Nothing
  value : remaining
    | predicate value -> Just value
    | otherwise -> findFirst predicate remaining

nextControlIndex :: ReferenceState -> ControlIndex
nextControlIndex state =
  controlIndex (controlIndexWord64 (referenceGreatestControlIndex state) + 1)

insertAssoc :: (Eq key) => key -> value -> [(key, value)] -> [(key, value)]
insertAssoc key value entries =
  (key, value) : filter ((/= key) . fst) entries

deleteAssoc :: (Eq key) => key -> [(key, value)] -> [(key, value)]
deleteAssoc key = filter ((/= key) . fst)

stateSystem :: ReferenceState -> SystemId
stateSystem (ReferenceState system _ _ _ _ _ _ _ _ _) = system

stateRequests ::
  ReferenceState -> [(OracleClientRequestId, ReferenceRequestRecord)]
stateRequests (ReferenceState _ _ _ requests _ _ _ _ _ _) = requests

stateActive ::
  ReferenceState -> [(ReferenceSubjectKey, DisappearanceProbeId)]
stateActive (ReferenceState _ _ _ _ active _ _ _ _ _) = active

stateProbes ::
  ReferenceState -> [(DisappearanceProbeId, ReferenceProbeRecord)]
stateProbes (ReferenceState _ _ _ _ _ probes _ _ _ _) = probes

stateRetirements :: ReferenceState -> [(SortId, ControlIndex)]
stateRetirements (ReferenceState _ _ _ _ _ _ retirements _ _ _) =
  retirements

stateLabelRevisions :: ReferenceState -> [(GlobalObjectId, LabelRevision)]
stateLabelRevisions (ReferenceState _ _ _ _ _ _ _ revisions _ _) = revisions

stateOpenLabels :: ReferenceState -> [(GlobalObjectId, LabelDecisionId)]
stateOpenLabels (ReferenceState _ _ _ _ _ _ _ _ labels _) = labels

stateDeletedObjects :: ReferenceState -> [GlobalObjectId]
stateDeletedObjects (ReferenceState _ _ _ _ _ _ _ _ _ deleted) = deleted

setStateRequestAndIndex ::
  ControlIndex ->
  OracleClientRequestId ->
  ReferenceRequestRecord ->
  ReferenceState ->
  ReferenceState
setStateRequestAndIndex index request record state =
  setStateIndex index
    . setStateRequests (insertAssoc request record (stateRequests state))
    $ state

setStateIndex :: ControlIndex -> ReferenceState -> ReferenceState
setStateIndex
  index
  (ReferenceState system _ membership requests active probes retirements revisions labels deleted) =
    ReferenceState
      system
      index
      membership
      requests
      active
      probes
      retirements
      revisions
      labels
      deleted

setStateMembership ::
  HeraldMembershipGeneration -> ReferenceState -> ReferenceState
setStateMembership
  membership
  (ReferenceState system index _ requests active probes retirements revisions labels deleted) =
    ReferenceState
      system
      index
      membership
      requests
      active
      probes
      retirements
      revisions
      labels
      deleted

setStateRequests ::
  [(OracleClientRequestId, ReferenceRequestRecord)] ->
  ReferenceState ->
  ReferenceState
setStateRequests
  requests
  (ReferenceState system index membership _ active probes retirements revisions labels deleted) =
    ReferenceState
      system
      index
      membership
      requests
      active
      probes
      retirements
      revisions
      labels
      deleted

setStateActive ::
  [(ReferenceSubjectKey, DisappearanceProbeId)] ->
  ReferenceState ->
  ReferenceState
setStateActive
  active
  (ReferenceState system index membership requests _ probes retirements revisions labels deleted) =
    ReferenceState
      system
      index
      membership
      requests
      active
      probes
      retirements
      revisions
      labels
      deleted

setStateProbes ::
  [(DisappearanceProbeId, ReferenceProbeRecord)] ->
  ReferenceState ->
  ReferenceState
setStateProbes
  probes
  (ReferenceState system index membership requests active _ retirements revisions labels deleted) =
    ReferenceState
      system
      index
      membership
      requests
      active
      probes
      retirements
      revisions
      labels
      deleted

setStateRetirements ::
  [(SortId, ControlIndex)] -> ReferenceState -> ReferenceState
setStateRetirements
  retirements
  (ReferenceState system index membership requests active probes _ revisions labels deleted) =
    ReferenceState
      system
      index
      membership
      requests
      active
      probes
      retirements
      revisions
      labels
      deleted

setStateLabelRevisions ::
  [(GlobalObjectId, LabelRevision)] -> ReferenceState -> ReferenceState
setStateLabelRevisions
  revisions
  (ReferenceState system index membership requests active probes retirements _ labels deleted) =
    ReferenceState
      system
      index
      membership
      requests
      active
      probes
      retirements
      revisions
      labels
      deleted

setStateOpenLabels ::
  [(GlobalObjectId, LabelDecisionId)] -> ReferenceState -> ReferenceState
setStateOpenLabels
  labels
  (ReferenceState system index membership requests active probes retirements revisions _ deleted) =
    ReferenceState
      system
      index
      membership
      requests
      active
      probes
      retirements
      revisions
      labels
      deleted

setStateDeletedObjects ::
  [GlobalObjectId] -> ReferenceState -> ReferenceState
setStateDeletedObjects
  deleted
  (ReferenceState system index membership requests active probes retirements revisions labels _) =
    ReferenceState
      system
      index
      membership
      requests
      active
      probes
      retirements
      revisions
      labels
      deleted

probeIdentifier :: ReferenceProbeRecord -> DisappearanceProbeId
probeIdentifier (ReferenceProbeRecord probe _ _ _ _ _ _) = probe

probeSubject :: ReferenceProbeRecord -> DisappearanceSubject
probeSubject (ReferenceProbeRecord _ subject _ _ _ _ _) = subject

probeCoordinate ::
  ReferenceProbeRecord -> DisappearanceSubjectMembershipCoordinate
probeCoordinate (ReferenceProbeRecord _ _ coordinate _ _ _ _) = coordinate

probeOpenIndex :: ReferenceProbeRecord -> ControlIndex
probeOpenIndex (ReferenceProbeRecord _ _ _ index _ _ _) = index

probeCapturedMembers :: ReferenceProbeRecord -> [HeraldEpoch]
probeCapturedMembers (ReferenceProbeRecord _ _ _ _ members _ _) = members

probeReports ::
  ReferenceProbeRecord -> [(HeraldEpoch, DisappearanceEvidenceClaim)]
probeReports (ReferenceProbeRecord _ _ _ _ _ reports _) = reports

probePhase :: ReferenceProbeRecord -> ReferenceProbePhase
probePhase (ReferenceProbeRecord _ _ _ _ _ _ phase) = phase

setProbeReports ::
  [(HeraldEpoch, DisappearanceEvidenceClaim)] ->
  ReferenceProbeRecord ->
  ReferenceProbeRecord
setProbeReports
  reports
  (ReferenceProbeRecord probe subject coordinate index members _ phase) =
    ReferenceProbeRecord probe subject coordinate index members reports phase

setProbePhase ::
  ReferenceProbePhase -> ReferenceProbeRecord -> ReferenceProbeRecord
setProbePhase
  phase
  (ReferenceProbeRecord probe subject coordinate index members reports _) =
    ReferenceProbeRecord probe subject coordinate index members reports phase

receiptResult :: ReferenceReceipt -> ReferenceReceiptResult
receiptResult (ReferenceReceipt _ _ _ result) = result

-- Canonical prospective transcripts -----------------------------------------

referenceCommandPayload :: ReferenceCommand -> Builder.Builder
referenceCommandPayload command = case command of
  ReferenceOpenDisappearanceProbe subject coordinate ->
    framedBytes (disappearanceSubjectCanonicalBytes subject)
      <> coordinateBuilder coordinate
  ReferenceReportPredefinedAbsence probe claim ->
    probeIdBuilder probe
      <> framedBytes (disappearanceEvidenceClaimCanonicalBytes claim)
  ReferenceInvalidateDisappearanceProbe probe reporter reason witness ->
    probeIdBuilder probe
      <> Builder.byteString (heraldEpochBytes reporter)
      <> Builder.word8 (referenceInvalidationReasonTag reason)
      <> Builder.byteString (disappearanceEvidenceDigestBytes witness)
  ReferenceResolveDisappearanceProbe probe digest ->
    probeIdBuilder probe
      <> Builder.byteString (referenceCompleteEvidenceDigestBytes digest)
  ReferenceAbortDisappearanceProbe probe reason ->
    probeIdBuilder probe <> Builder.word8 (referenceAbortReasonTag reason)

referenceReceiptCanonicalBytes :: ReferenceReceipt -> ByteString
referenceReceiptCanonicalBytes
  (ReferenceReceipt request digest index result) =
    buildCanonical
      ( framedBytes receiptDomain
          <> requestIdBuilder request
          <> Builder.byteString digest
          <> controlIndexBuilder index
          <> referenceReceiptResultBuilder result
      )

referenceReceiptResultBuilder :: ReferenceReceiptResult -> Builder.Builder
referenceReceiptResultBuilder result = case result of
  ReferenceReceiptAccepted accepted ->
    Builder.word8 0 <> framedBuilder (referenceAcceptedResultBuilder accepted)
  ReferenceReceiptRejected rejection ->
    Builder.word8 1 <> framedBuilder (referenceRejectionBuilder rejection)

referenceAcceptedResultBuilder :: ReferenceAcceptedResult -> Builder.Builder
referenceAcceptedResultBuilder accepted = case accepted of
  ReferenceOpenAccepted result ->
    Builder.word8 0 <> disappearanceOpenResultBuilder result
  ReferenceReportAccepted probe reporter ->
    Builder.word8 1
      <> probeIdBuilder probe
      <> Builder.byteString (heraldEpochBytes reporter)
  ReferenceInvalidationAccepted probe ->
    Builder.word8 2 <> probeIdBuilder probe
  ReferenceResolutionAccepted outcome ->
    Builder.word8 3
      <> framedBytes (disappearanceResolutionOutcomeCanonicalBytes outcome)
  ReferenceAbortAccepted probe ->
    Builder.word8 4 <> probeIdBuilder probe

referenceRejectionBuilder :: ReferenceRejection -> Builder.Builder
referenceRejectionBuilder rejected = case rejected of
  ReferenceRequestHomeEpochMismatch expected supplied ->
    taggedRejection
      0
      ( Builder.byteString (heraldEpochBytes expected)
          <> Builder.byteString (heraldEpochBytes supplied)
      )
  ReferenceInactiveHomeHerald home ->
    taggedRejection 1 (Builder.byteString (heraldEpochBytes home))
  ReferenceStaleExpectedControlIndex expected observed ->
    taggedRejection
      2
      (controlIndexBuilder expected <> controlIndexBuilder observed)
  ReferenceSubjectMembershipCoordinateMismatch supplied expected ->
    taggedRejection
      3
      (coordinateBuilder supplied <> coordinateBuilder expected)
  ReferenceControlledSubjectDeleted object ->
    taggedRejection 4 (Builder.byteString (globalObjectIdBytes object))
  ReferenceControlledLabelRevisionMismatch object supplied expected ->
    taggedRejection
      5
      ( Builder.byteString (globalObjectIdBytes object)
          <> maybeLabelRevisionBuilder supplied
          <> maybeLabelRevisionBuilder expected
      )
  ReferenceControlledLabelWorkflowIncomplete object decision ->
    taggedRejection
      6
      ( Builder.byteString (globalObjectIdBytes object)
          <> Builder.byteString (labelDecisionIdBytes decision)
      )
  ReferenceRegularOccurrenceMismatch sortId supplied expected ->
    taggedRejection
      7
      ( Builder.byteString (sortIdBytes sortId)
          <> Builder.byteString (sortDefinitionOccurrenceIdBytes supplied)
          <> Builder.byteString (sortDefinitionOccurrenceIdBytes expected)
      )
  ReferenceUnknownProbe probe ->
    taggedRejection 8 (probeIdBuilder probe)
  ReferenceProbeNotCollecting probe phaseClass ->
    taggedRejection
      9
      (probeIdBuilder probe <> Builder.word8 (referenceProbePhaseClassTag phaseClass))
  ReferenceReportHomeMismatch home reporter ->
    taggedRejection
      10
      ( Builder.byteString (heraldEpochBytes home)
          <> Builder.byteString (heraldEpochBytes reporter)
      )
  ReferenceEvidenceProbeMismatch supplied expected ->
    taggedRejection 11 (probeIdBuilder supplied <> probeIdBuilder expected)
  ReferenceEvidenceCoordinateMismatch supplied expected ->
    taggedRejection
      12
      (coordinateBuilder supplied <> coordinateBuilder expected)
  ReferenceReporterNotCaptured reporter ->
    taggedRejection 13 (Builder.byteString (heraldEpochBytes reporter))
  ReferenceConflictingReporterEvidence reporter ->
    taggedRejection 14 (Builder.byteString (heraldEpochBytes reporter))
  ReferenceIncompleteEvidence missing ->
    taggedRejection 15 (heraldListBuilder missing)
  ReferenceCompleteEvidenceDigestMismatch supplied expected ->
    taggedRejection
      16
      ( Builder.byteString (referenceCompleteEvidenceDigestBytes supplied)
          <> Builder.byteString (referenceCompleteEvidenceDigestBytes expected)
      )
  where
    taggedRejection tag payload = Builder.word8 tag <> framedBuilder payload

referenceProjectionEventCanonicalBytes ::
  ReferenceProjectionEvent -> ByteString
referenceProjectionEventCanonicalBytes event =
  buildCanonical
    ( framedBytes eventDomain
        <> Builder.word8 (referenceProjectionEventTag event)
        <> framedBuilder (referenceProjectionEventPayload event)
    )

referenceProjectionEventsCanonicalBytes ::
  [ReferenceProjectionEvent] -> ByteString
referenceProjectionEventsCanonicalBytes events =
  buildCanonical
    ( framedBytes eventDomain
        <> Builder.word64BE (fromIntegral (length events))
        <> foldMap
          (framedBytes . referenceProjectionEventCanonicalBytes)
          events
    )

referenceProjectionEventTag :: ReferenceProjectionEvent -> Word8
referenceProjectionEventTag event = case event of
  ReferenceProbeOpenProjected {} -> 17
  ReferenceReportProjected {} -> 18
  ReferenceInvalidationProjected {} -> 19
  ReferenceResolutionProjected {} -> 20
  ReferenceAbortProjected {} -> 21
  ReferenceMembershipProjected {} -> 22
  ReferenceLabelOpenProjected {} -> 23
  ReferenceLabelTerminalProjected {} -> 24

referenceProjectionEventPayload ::
  ReferenceProjectionEvent -> Builder.Builder
referenceProjectionEventPayload event = case event of
  ReferenceProbeOpenProjected header result ->
    referenceProjectedProbeHeaderBuilder header
      <> disappearanceOpenResultBuilder result
  ReferenceReportProjected claim ->
    framedBytes (disappearanceEvidenceClaimCanonicalBytes claim)
  ReferenceInvalidationProjected probe cause _index ->
    probeIdBuilder probe
      <> referenceInvalidationCauseBuilder cause
  ReferenceResolutionProjected probe outcome ->
    probeIdBuilder probe
      <> framedBytes (disappearanceResolutionOutcomeCanonicalBytes outcome)
  ReferenceAbortProjected probe reason _index ->
    probeIdBuilder probe
      <> referenceProbeAbortReasonBuilder reason
  ReferenceMembershipProjected membership ->
    framedBytes (heraldMembershipGenerationCanonicalBytes membership)
  ReferenceLabelOpenProjected decision object ->
    Builder.byteString (labelDecisionIdBytes decision)
      <> Builder.byteString (globalObjectIdBytes object)
  ReferenceLabelTerminalProjected decision object disposition ->
    Builder.byteString (labelDecisionIdBytes decision)
      <> Builder.byteString (globalObjectIdBytes object)
      <> referenceLabelTerminalDispositionBuilder disposition

referenceProjectedProbeHeaderBuilder ::
  ReferenceProjectedProbeHeader -> Builder.Builder
referenceProjectedProbeHeaderBuilder header =
  probeIdBuilder (referenceProjectedProbeHeaderId header)
    <> framedBytes
      ( disappearanceSubjectCanonicalBytes
          (referenceProjectedProbeHeaderSubject header)
      )
    <> coordinateBuilder (referenceProjectedProbeHeaderCoordinate header)
    <> framedBytes
      ( heraldMembershipGenerationCanonicalBytes
          (referenceProjectedProbeHeaderMembership header)
      )

referenceStateCanonicalBytes :: ReferenceState -> ByteString
referenceStateCanonicalBytes state =
  buildCanonical
    ( framedBytes stateDomain
        <> Builder.byteString (systemIdBytes (stateSystem state))
        <> controlIndexBuilder (referenceGreatestControlIndex state)
        <> framedBytes
          (heraldMembershipGenerationCanonicalBytes (referenceCurrentMembership state))
        <> countedBuilders
          ( fmap
              referenceRequestEntryBuilder
              (sortOn fst (stateRequests state))
          )
        <> countedBuilders
          (fmap referenceActiveEntryBuilder (sortOn fst (stateActive state)))
        <> countedBuilders
          (fmap (referenceProbeRecordBuilder . snd) (sortOn fst (stateProbes state)))
        <> countedBuilders
          ( fmap
              referenceRetirementEntryBuilder
              (sortOn fst (stateRetirements state))
          )
        <> countedBuilders
          ( fmap
              referenceLabelRevisionEntryBuilder
              (sortOn fst (stateLabelRevisions state))
          )
        <> countedBuilders
          ( fmap
              referenceOpenLabelEntryBuilder
              (sortOn fst (stateOpenLabels state))
          )
        <> countedBuilders
          ( fmap
              (Builder.byteString . globalObjectIdBytes)
              (sort (stateDeletedObjects state))
          )
    )

referenceStateDigestBytes :: ReferenceState -> ByteString
referenceStateDigestBytes = SHA256.hash . referenceStateCanonicalBytes

referenceRequestEntryBuilder ::
  (OracleClientRequestId, ReferenceRequestRecord) -> Builder.Builder
referenceRequestEntryBuilder
  (request, ReferenceRequestRecord envelope retainedReceipt) =
    requestIdBuilder request
      <> framedBytes (referenceEnvelopeCanonicalBytes envelope)
      <> framedBytes (referenceReceiptCanonicalBytes retainedReceipt)

referenceActiveEntryBuilder ::
  (ReferenceSubjectKey, DisappearanceProbeId) -> Builder.Builder
referenceActiveEntryBuilder
  (ReferenceSubjectKey subject coordinate, probe) =
    framedBytes (disappearanceSubjectCanonicalBytes subject)
      <> coordinateBuilder coordinate
      <> probeIdBuilder probe

referenceProbeRecordBuilder :: ReferenceProbeRecord -> Builder.Builder
referenceProbeRecordBuilder record =
  probeIdBuilder (probeIdentifier record)
    <> framedBytes (disappearanceSubjectCanonicalBytes (probeSubject record))
    <> coordinateBuilder (probeCoordinate record)
    <> controlIndexBuilder (probeOpenIndex record)
    <> heraldListBuilder (probeCapturedMembers record)
    <> countedBuilders
      ( fmap
          referenceReportEntryBuilder
          (sortOn fst (probeReports record))
      )
    <> referenceProbePhaseBuilder (probePhase record)

referenceReportEntryBuilder ::
  (HeraldEpoch, DisappearanceEvidenceClaim) -> Builder.Builder
referenceReportEntryBuilder (reporter, claim) =
  Builder.byteString (heraldEpochBytes reporter)
    <> framedBytes (disappearanceEvidenceClaimCanonicalBytes claim)

referenceRetirementEntryBuilder ::
  (SortId, ControlIndex) -> Builder.Builder
referenceRetirementEntryBuilder (sortId, index) =
  Builder.byteString (sortIdBytes sortId) <> controlIndexBuilder index

referenceLabelRevisionEntryBuilder ::
  (GlobalObjectId, LabelRevision) -> Builder.Builder
referenceLabelRevisionEntryBuilder (object, revision) =
  Builder.byteString (globalObjectIdBytes object)
    <> controlIndexBuilder (labelRevisionControlIndex revision)

referenceOpenLabelEntryBuilder ::
  (GlobalObjectId, LabelDecisionId) -> Builder.Builder
referenceOpenLabelEntryBuilder (object, decision) =
  Builder.byteString (globalObjectIdBytes object)
    <> Builder.byteString (labelDecisionIdBytes decision)

referenceProbePhaseBuilder :: ReferenceProbePhase -> Builder.Builder
referenceProbePhaseBuilder phase = case phase of
  ReferenceCollectingEvidence -> Builder.word8 0 <> framedBuilder mempty
  ReferenceInvalidated cause index ->
    Builder.word8 1
      <> framedBuilder
        (referenceInvalidationCauseBuilder cause <> controlIndexBuilder index)
  ReferenceResolved index outcome ->
    Builder.word8 2
      <> framedBuilder
        ( controlIndexBuilder index
            <> framedBytes (disappearanceResolutionOutcomeCanonicalBytes outcome)
        )
  ReferenceAborted reason index ->
    Builder.word8 3
      <> framedBuilder
        (referenceProbeAbortReasonBuilder reason <> controlIndexBuilder index)

referenceInvalidationCauseBuilder ::
  ReferenceInvalidationCause -> Builder.Builder
referenceInvalidationCauseBuilder cause = case cause of
  ReferenceCommandInvalidation reporter reason witness ->
    Builder.word8 0
      <> Builder.byteString (heraldEpochBytes reporter)
      <> Builder.word8 (referenceInvalidationReasonTag reason)
      <> Builder.byteString (disappearanceEvidenceDigestBytes witness)
  ReferenceLabelInvalidation decision ->
    Builder.word8 1 <> Builder.byteString (labelDecisionIdBytes decision)

referenceProbeAbortReasonBuilder ::
  ReferenceProbeAbortReason -> Builder.Builder
referenceProbeAbortReasonBuilder reason = case reason of
  ReferenceExplicitAbort authorized ->
    Builder.word8 0 <> Builder.word8 (referenceAbortReasonTag authorized)
  ReferenceMembershipAbort successor ->
    Builder.word8 1
      <> Builder.byteString (heraldMembershipGenerationIdBytes successor)

referenceLabelTerminalDispositionBuilder ::
  ReferenceLabelTerminalDisposition -> Builder.Builder
referenceLabelTerminalDispositionBuilder disposition = case disposition of
  ReferenceLabelNotApplied -> Builder.word8 0
  ReferenceLabelReleased revision ->
    Builder.word8 1 <> controlIndexBuilder (labelRevisionControlIndex revision)
  ReferenceLabelDeleted revision ->
    Builder.word8 2 <> controlIndexBuilder (labelRevisionControlIndex revision)

disappearanceOpenResultBuilder ::
  DisappearanceOpenResult -> Builder.Builder
disappearanceOpenResultBuilder result = case disappearanceOpenResultView result of
  OpenedDisappearanceProbe probe -> Builder.word8 0 <> probeIdBuilder probe
  AliasedDisappearanceProbe probe -> Builder.word8 1 <> probeIdBuilder probe

coordinateBuilder ::
  DisappearanceSubjectMembershipCoordinate -> Builder.Builder
coordinateBuilder coordinate =
  Builder.byteString
    ( disappearanceSubjectDigestBytes
        (disappearanceCoordinateSubjectDigest coordinate)
    )
    <> Builder.byteString
      ( heraldMembershipGenerationIdBytes
          (disappearanceCoordinateMembershipGenerationId coordinate)
      )
    <> Builder.byteString
      ( memberSetDigestBytes
          (disappearanceCoordinateMemberSetDigest coordinate)
      )

probeIdBuilder :: DisappearanceProbeId -> Builder.Builder
probeIdBuilder = Builder.byteString . disappearanceProbeIdBytes

requestIdBuilder :: OracleClientRequestId -> Builder.Builder
requestIdBuilder request =
  Builder.byteString (heraldEpochBytes (oracleClientRequestHome request))
    <> Builder.word64BE (oracleClientRequestSequence request)

controlIndexBuilder :: ControlIndex -> Builder.Builder
controlIndexBuilder = Builder.word64BE . controlIndexWord64

maybeControlIndexBuilder :: Maybe ControlIndex -> Builder.Builder
maybeControlIndexBuilder maybeIndex = case maybeIndex of
  Nothing -> Builder.word8 0
  Just index -> Builder.word8 1 <> controlIndexBuilder index

maybeLabelRevisionBuilder :: Maybe LabelRevision -> Builder.Builder
maybeLabelRevisionBuilder maybeRevision = case maybeRevision of
  Nothing -> Builder.word8 0
  Just revision ->
    Builder.word8 1 <> controlIndexBuilder (labelRevisionControlIndex revision)

heraldListBuilder :: [HeraldEpoch] -> Builder.Builder
heraldListBuilder heralds =
  Builder.word64BE (fromIntegral (length normalized))
    <> foldMap (Builder.byteString . heraldEpochBytes) normalized
  where
    normalized = sort heralds

countedBuilders :: [Builder.Builder] -> Builder.Builder
countedBuilders builders =
  Builder.word64BE (fromIntegral (length builders))
    <> foldMap framedBuilder builders

referenceInvalidationReasonTag :: ReferenceInvalidationReason -> Word8
referenceInvalidationReasonTag reason = case reason of
  ReferenceMatchingPublication -> 0
  ReferenceInvalidEvidence -> 1

referenceAbortReasonTag :: ReferenceAbortReason -> Word8
referenceAbortReasonTag ReferenceAuthorizedAbort = 0

referenceProbePhaseClassTag :: ReferenceProbePhaseClass -> Word8
referenceProbePhaseClassTag phaseClass = case phaseClass of
  ReferenceProbeInvalidatedClass -> 0
  ReferenceProbeResolvedClass -> 1
  ReferenceProbeAbortedClass -> 2

framedBytes :: ByteString -> Builder.Builder
framedBytes bytes =
  Builder.word64BE (fromIntegral (ByteString.length bytes))
    <> Builder.byteString bytes

framedBuilder :: Builder.Builder -> Builder.Builder
framedBuilder = framedBytes . buildCanonical

buildCanonical :: Builder.Builder -> ByteString
buildCanonical = LazyByteString.toStrict . Builder.toLazyByteString
