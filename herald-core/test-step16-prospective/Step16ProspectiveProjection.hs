{-# LANGUAGE ImportQualifiedPost #-}

-- | Test-only translation from the prospective Oracle target's applied
-- outcomes to the exact private Herald disappearance inputs.  The live Oracle
-- projection remains unchanged until the atomic cutover increment.
module Step16ProspectiveProjection
  ( DisappearanceProjectionInput (..),
    disappearanceProjectionInputControlIndex,
    TargetProjectionTranslationProblem (..),
    translateTargetSubmissionOutcome,
    translateTargetContextOutcome,
  )
where

import Control.Monad (unless)
import DisappearanceReferenceDriver qualified as Target
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceClaim,
    DisappearanceOpenResult,
    DisappearanceOpenResultView (..),
    DisappearanceProbeId,
    disappearanceEvidenceClaimProbeId,
    disappearanceEvidenceClaimReporter,
    disappearanceOpenResultView,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
  )
import Eclips.Domain.Label (labelRevisionControlIndex)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    heraldMembershipGenerationId,
    heraldMembershipGenerationRetirementControlIndex,
  )
import Eclips.Herald.Disappearance.Protocol qualified as Protocol

-- | The four leaf-level input families produced by prospective Oracle
-- projection.  Context-only MembershipAdvanced and LabelOpened events produce
-- no value themselves; they authorize the immediately following compound
-- consequences in the same vector.
data DisappearanceProjectionInput
  = DisappearanceProjectedOpen
      ControlIndex
      Protocol.ProjectedDisappearanceProbe
  | DisappearanceProjectedReport
      ControlIndex
      DisappearanceEvidenceClaim
  | DisappearanceProjectedTerminal
      ControlIndex
      Protocol.ProjectedDisappearanceTerminal
  | DisappearanceProjectedLabelTerminal
      ControlIndex
      Protocol.ProjectedLabelTerminal
  deriving stock (Eq, Show)

disappearanceProjectionInputControlIndex ::
  DisappearanceProjectionInput -> ControlIndex
disappearanceProjectionInputControlIndex input = case input of
  DisappearanceProjectedOpen index _ -> index
  DisappearanceProjectedReport index _ -> index
  DisappearanceProjectedTerminal index _ -> index
  DisappearanceProjectedLabelTerminal index _ -> index

data TargetProjectionTranslationProblem
  = TargetProjectionDuplicateSubmission
  | TargetProjectionProtocolRejected Target.TargetProtocolDisposition
  | TargetProjectionReceiptRejected Target.TargetRejection
  | TargetProjectionDuplicateContext ControlIndex
  | TargetProjectionContextRejected ControlIndex Target.TargetRejection
  | TargetProjectionEventless
  | TargetProjectionSubmissionEventMismatch
      Target.TargetAcceptedResult
      [Target.TargetProjectionEvent]
  | TargetProjectionContextEventMismatch
      Target.TargetContextResult
      [Target.TargetProjectionEvent]
  | TargetProjectionOpenResultMismatch
      DisappearanceOpenResult
      DisappearanceOpenResult
  | TargetProjectionOpenProbeMismatch
      DisappearanceProbeId
      DisappearanceProbeId
  | TargetProjectionOpenProtocolProblem Protocol.DisappearanceProtocolProblem
  | TargetProjectionReportProbeMismatch
      DisappearanceProbeId
      DisappearanceProbeId
  | TargetProjectionReportReporterMismatch HeraldEpoch HeraldEpoch
  | TargetProjectionInvalidationProbeMismatch
      DisappearanceProbeId
      DisappearanceProbeId
  | TargetProjectionResolveOutcomeMismatch
  | TargetProjectionAbortProbeMismatch
      DisappearanceProbeId
      DisappearanceProbeId
  | TargetProjectionAbortAuthorityMismatch
  | TargetProjectionMembershipGenerationMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | TargetProjectionMembershipIndexMismatch
      ControlIndex
      (Maybe ControlIndex)
  | TargetProjectionMembershipAbortWithoutMatchingAdvance
      Target.TargetProjectionEvent
  | TargetProjectionMembershipAbortGenerationMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | TargetProjectionLabelDecisionMismatch LabelDecisionId LabelDecisionId
  | TargetProjectionLabelInvalidationWithoutMatchingOpen
      Target.TargetProjectionEvent
  | TargetProjectionLabelInvalidationDecisionMismatch
      LabelDecisionId
      LabelDecisionId
  | TargetProjectionLabelTerminalMismatch
      Target.TargetLabelTerminal
      Target.TargetLabelTerminal
  | TargetProjectionLabelTerminalRevisionMismatch ControlIndex ControlIndex
  deriving stock (Eq, Show)

-- | Translate one accepted command receipt and its complete event vector.
-- Exact applied duplicates are deliberately rejected: they settle request
-- ownership but confer no new leaf projection authority.
translateTargetSubmissionOutcome ::
  Target.TargetSubmissionOutcome ->
  Either TargetProjectionTranslationProblem [DisappearanceProjectionInput]
translateTargetSubmissionOutcome outcome = case outcome of
  Target.TargetSubmissionDuplicate {} ->
    Left TargetProjectionDuplicateSubmission
  Target.TargetSubmissionProtocolRejected rejection ->
    Left (TargetProjectionProtocolRejected rejection)
  Target.TargetSubmissionCommitted receipt events ->
    case Target.targetReceiptResult receipt of
      Target.TargetReceiptRejected rejection ->
        Left (TargetProjectionReceiptRejected rejection)
      Target.TargetReceiptAccepted accepted
        | null events -> Left TargetProjectionEventless
        | otherwise ->
            translateAcceptedSubmission
              (Target.targetReceiptControlIndex receipt)
              accepted
              events

translateAcceptedSubmission ::
  ControlIndex ->
  Target.TargetAcceptedResult ->
  [Target.TargetProjectionEvent] ->
  Either TargetProjectionTranslationProblem [DisappearanceProjectionInput]
translateAcceptedSubmission index accepted events = case (accepted, events) of
  ( Target.TargetOpenAccepted acceptedResult,
    [Target.TargetOpenProjected header projectedResult]
    ) -> do
      unless
        (acceptedResult == projectedResult)
        ( Left
            (TargetProjectionOpenResultMismatch acceptedResult projectedResult)
        )
      let resultProbe = openResultProbe projectedResult
          headerProbe = Target.targetProjectedProbeHeaderId header
      unless
        (resultProbe == headerProbe)
        (Left (TargetProjectionOpenProbeMismatch resultProbe headerProbe))
      projected <-
        mapLeft
          TargetProjectionOpenProtocolProblem
          ( Protocol.projectedDisappearanceProbe
              headerProbe
              (Target.targetProjectedProbeHeaderSubject header)
              (Target.targetProjectedProbeHeaderCoordinate header)
              (Target.targetProjectedProbeHeaderMembership header)
          )
      Right [DisappearanceProjectedOpen index projected]
  ( Target.TargetReportAccepted acceptedProbe acceptedReporter,
    [Target.TargetReportProjected claim]
    ) -> do
      let projectedProbe = disappearanceEvidenceClaimProbeId claim
          projectedReporter = disappearanceEvidenceClaimReporter claim
      unless
        (acceptedProbe == projectedProbe)
        ( Left
            ( TargetProjectionReportProbeMismatch
                acceptedProbe
                projectedProbe
            )
        )
      unless
        (acceptedReporter == projectedReporter)
        ( Left
            ( TargetProjectionReportReporterMismatch
                acceptedReporter
                projectedReporter
            )
        )
      Right [DisappearanceProjectedReport index claim]
  ( Target.TargetInvalidateAccepted acceptedProbe,
    [ Target.TargetInvalidatedProjected
        projectedProbe
        (Target.TargetCommandInvalidation reporter reason witness)
      ]
    ) -> do
      unless
        (acceptedProbe == projectedProbe)
        ( Left
            ( TargetProjectionInvalidationProbeMismatch
                acceptedProbe
                projectedProbe
            )
        )
      let cause =
            Protocol.ProjectedCommandInvalidation
              reporter
              (translateInvalidationReason reason)
              witness
          terminal =
            Protocol.ProjectedDisappearanceInvalidated
              projectedProbe
              cause
              index
      Right [DisappearanceProjectedTerminal index terminal]
  ( Target.TargetResolveAccepted acceptedOutcome,
    [Target.TargetResolvedProjected probe projectedOutcome]
    ) -> do
      unless
        (acceptedOutcome == projectedOutcome)
        (Left TargetProjectionResolveOutcomeMismatch)
      let terminal =
            Protocol.ProjectedDisappearanceResolved
              probe
              projectedOutcome
              index
      Right [DisappearanceProjectedTerminal index terminal]
  ( Target.TargetAbortAccepted acceptedProbe,
    [ Target.TargetAbortedProjected
        projectedProbe
        (Target.TargetExplicitAbortReasonView reason)
      ]
    ) -> do
      unless
        (acceptedProbe == projectedProbe)
        ( Left
            ( TargetProjectionAbortProbeMismatch
                acceptedProbe
                projectedProbe
            )
        )
      unless
        (reason == Target.targetAuthorizedAbortReason)
        (Left TargetProjectionAbortAuthorityMismatch)
      let terminal =
            Protocol.ProjectedDisappearanceAborted
              projectedProbe
              Protocol.ProjectedAuthorizedAbort
              index
      Right [DisappearanceProjectedTerminal index terminal]
  _ -> Left (TargetProjectionSubmissionEventMismatch accepted events)

-- | Translate one accepted context transition.  Membership and label Open are
-- compound vectors: their first event supplies the immutable context for every
-- immediately following consequence, and no unrelated event may intervene.
translateTargetContextOutcome ::
  Target.TargetContextOutcome ->
  Either TargetProjectionTranslationProblem [DisappearanceProjectionInput]
translateTargetContextOutcome outcome = case outcome of
  Target.TargetContextDuplicate index ->
    Left (TargetProjectionDuplicateContext index)
  Target.TargetContextRejected index rejection ->
    Left (TargetProjectionContextRejected index rejection)
  Target.TargetContextCommitted index accepted events
    | null events -> Left TargetProjectionEventless
    | otherwise -> translateAcceptedContext index accepted events

translateAcceptedContext ::
  ControlIndex ->
  Target.TargetContextResult ->
  [Target.TargetProjectionEvent] ->
  Either TargetProjectionTranslationProblem [DisappearanceProjectionInput]
translateAcceptedContext index accepted events = case (accepted, events) of
  ( Target.TargetMembershipSuccessorAccepted acceptedGeneration,
    Target.TargetMembershipAdvancedProjected successor : consequences
    ) -> do
      let projectedGeneration = heraldMembershipGenerationId successor
      unless
        (acceptedGeneration == projectedGeneration)
        ( Left
            ( TargetProjectionMembershipGenerationMismatch
                acceptedGeneration
                projectedGeneration
            )
        )
      unless
        (heraldMembershipGenerationRetirementControlIndex successor == Just index)
        ( Left
            ( TargetProjectionMembershipIndexMismatch
                index
                (heraldMembershipGenerationRetirementControlIndex successor)
            )
        )
      traverse (translateMembershipAbort index successor) consequences
  ( Target.TargetLabelOpenAccepted acceptedDecision,
    Target.TargetLabelOpenedProjected object projectedDecision : consequences
    ) -> do
      unless
        (acceptedDecision == projectedDecision)
        ( Left
            ( TargetProjectionLabelDecisionMismatch
                acceptedDecision
                projectedDecision
            )
        )
      traverse
        (translateLabelInvalidation index object projectedDecision)
        consequences
  ( Target.TargetLabelTerminalAccepted acceptedTerminal,
    [ Target.TargetLabelTerminalProjected
        decision
        object
        projectedTerminal
      ]
    ) -> do
      unless
        (acceptedTerminal == projectedTerminal)
        ( Left
            ( TargetProjectionLabelTerminalMismatch
                acceptedTerminal
                projectedTerminal
            )
        )
      translated <- translateLabelTerminal index projectedTerminal
      Right
        [ DisappearanceProjectedLabelTerminal
            index
            (Protocol.projectedLabelTerminal decision object translated)
        ]
  _ -> Left (TargetProjectionContextEventMismatch accepted events)

translateMembershipAbort ::
  ControlIndex ->
  HeraldMembershipGeneration ->
  Target.TargetProjectionEvent ->
  Either TargetProjectionTranslationProblem DisappearanceProjectionInput
translateMembershipAbort index successor event = case event of
  Target.TargetAbortedProjected
    probe
    (Target.TargetMembershipSupersededAbortView projectedGeneration) -> do
      let expectedGeneration = heraldMembershipGenerationId successor
      unless
        (projectedGeneration == expectedGeneration)
        ( Left
            ( TargetProjectionMembershipAbortGenerationMismatch
                expectedGeneration
                projectedGeneration
            )
        )
      let terminal =
            Protocol.ProjectedDisappearanceAborted
              probe
              (Protocol.ProjectedMembershipSuperseded successor)
              index
      Right (DisappearanceProjectedTerminal index terminal)
  _ -> Left (TargetProjectionMembershipAbortWithoutMatchingAdvance event)

translateLabelInvalidation ::
  ControlIndex ->
  GlobalObjectId ->
  LabelDecisionId ->
  Target.TargetProjectionEvent ->
  Either TargetProjectionTranslationProblem DisappearanceProjectionInput
translateLabelInvalidation index object expectedDecision event = case event of
  Target.TargetInvalidatedProjected
    probe
    (Target.TargetLabelOpenInvalidation projectedDecision) -> do
      unless
        (projectedDecision == expectedDecision)
        ( Left
            ( TargetProjectionLabelInvalidationDecisionMismatch
                expectedDecision
                projectedDecision
            )
        )
      let terminal =
            Protocol.ProjectedDisappearanceInvalidated
              probe
              (Protocol.ProjectedLabelInvalidation expectedDecision object)
              index
      Right (DisappearanceProjectedTerminal index terminal)
  _ -> Left (TargetProjectionLabelInvalidationWithoutMatchingOpen event)

translateLabelTerminal ::
  ControlIndex ->
  Target.TargetLabelTerminal ->
  Either
    TargetProjectionTranslationProblem
    Protocol.ProjectedLabelTerminalOutcome
translateLabelTerminal index terminal = case terminal of
  Target.TargetLabelNotApplied -> Right Protocol.ProjectedLabelNotApplied
  Target.TargetLabelReleased revision -> do
    checkLabelRevision index (labelRevisionControlIndex revision)
    Right (Protocol.ProjectedLabelReleased revision)
  Target.TargetLabelDeleted revision -> do
    checkLabelRevision index (labelRevisionControlIndex revision)
    Right (Protocol.ProjectedLabelDeleted revision)

checkLabelRevision ::
  ControlIndex ->
  ControlIndex ->
  Either TargetProjectionTranslationProblem ()
checkLabelRevision expected actual =
  unless
    (actual == expected)
    (Left (TargetProjectionLabelTerminalRevisionMismatch expected actual))

translateInvalidationReason ::
  Target.TargetInvalidationReason -> Protocol.DisappearanceInvalidationReason
translateInvalidationReason reason = case reason of
  Target.TargetMatchingPublicationObserved -> Protocol.MatchingPublicationObserved
  Target.TargetEvidenceContradicted -> Protocol.LocalEvidenceContradicted

openResultProbe :: DisappearanceOpenResult -> DisappearanceProbeId
openResultProbe result = case disappearanceOpenResultView result of
  OpenedDisappearanceProbe probe -> probe
  AliasedDisappearanceProbe probe -> probe

mapLeft :: (left -> mapped) -> Either left right -> Either mapped right
mapLeft convert = either (Left . convert) Right
