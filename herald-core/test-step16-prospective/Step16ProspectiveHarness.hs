{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | The sole test-only adapter between the prospective Oracle machine and the
-- private Herald disappearance vertical.  No production component may import
-- the prospective Oracle module.
module Step16ProspectiveHarness
  ( ProspectiveOracle,
    initialProspectiveOracle,
    submitTargetCommand,
    submitOpenIntention,
    submitLocalReport,
    submitInvalidationIntention,
    submitResolveIntention,
    projectedProbeFromOpen,
    projectedClaimFromReport,
    committedTargetEvents,
    prospectiveOracleState,
    ProspectiveHerald,
    initialProspectiveHerald,
    prospectiveHeraldState,
    prospectiveHeraldPublicationStream,
    ProspectiveHeraldProblem (..),
    heldMarkerProbeIds,
    heldMarkerCount,
    commitProspectiveHerald,
    receivePublicationMarker,
    receiveAlignmentMarker,
    redriveMarkersAfterOpen,
    ProspectiveRequestLedger,
    initialProspectiveRequestLedger,
    ProspectiveRequestDisposition (..),
    ProspectiveRequestProblem (..),
    ProspectiveRequestRelease (..),
    ProspectiveRequestSettlement (..),
    ProspectiveRequestSubmission,
    ProspectiveTerminalProjection,
    prospectiveRequestDispositionRef,
    prospectiveRequestSubmissionOutcome,
    prospectiveTerminalFromSubmission,
    prospectiveRequestCount,
    prospectiveOutstandingRequestCount,
    retainOpenRequest,
    retainReportRequest,
    retainInvalidationRequest,
    retainResolveRequest,
    settleProspectiveRequest,
    releaseProspectiveOpenRequest,
    submitRetainedOpenRequest,
    submitRetainedReportRequest,
    submitRetainedInvalidationRequest,
    submitRetainedResolveRequest,
  )
where

import Control.Monad (unless)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import DisappearanceReferenceDriver qualified as Target
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceClaim,
    DisappearanceOpenResult,
    DisappearanceOpenResultView (..),
    DisappearanceProbeId,
    DisappearanceSubject,
    DisappearanceSubjectMembershipCoordinate,
    disappearanceEvidenceClaimReporter,
    disappearanceOpenResultView,
  )
import Eclips.Domain.Identity (ControlIndex, HeraldEpoch, SystemId)
import Eclips.Domain.Membership (HeraldMembershipGeneration)
import Eclips.Herald.Disappearance.Protocol qualified as Protocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.OracleClient.Request
  ( OracleRequestRef (..),
    oracleRequestRefRequestId,
  )
import Eclips.Herald.PeerStream (SequencedItem, sequencedItemPayload)
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.UseCase.Disappearance qualified as DisappearanceUseCase
import Eclips.Oracle.Identity (OracleClientRequestId, oracleClientRequestId)
import Step16ProspectiveProjection qualified as Projection

newtype ProspectiveOracle = ProspectiveOracle Target.TargetState

initialProspectiveOracle ::
  SystemId -> HeraldMembershipGeneration -> ProspectiveOracle
initialProspectiveOracle system membership =
  ProspectiveOracle (Target.initialTargetState system membership)

submitTargetCommand ::
  OracleClientRequestId ->
  Maybe ControlIndex ->
  HeraldEpoch ->
  Target.TargetCommand ->
  ProspectiveOracle ->
  (ProspectiveOracle, Target.TargetSubmissionOutcome)
submitTargetCommand request expected home command (ProspectiveOracle state) =
  let (successor, outcome) =
        Target.submitTarget
          (Target.targetEnvelope request expected home command)
          state
   in (ProspectiveOracle successor, outcome)

submitOpenIntention ::
  OracleClientRequestId ->
  Maybe ControlIndex ->
  HeraldEpoch ->
  Protocol.DisappearanceOpenIntention ->
  ProspectiveOracle ->
  (ProspectiveOracle, Target.TargetSubmissionOutcome)
submitOpenIntention request expected home intention =
  submitTargetCommand
    request
    expected
    home
    ( Target.targetOpenCommand
        (Protocol.disappearanceOpenIntentionSubject intention)
        (Protocol.disappearanceOpenIntentionCoordinate intention)
    )

submitLocalReport ::
  OracleClientRequestId ->
  Maybe ControlIndex ->
  HeraldEpoch ->
  Protocol.LocalAbsenceReport ->
  ProspectiveOracle ->
  (ProspectiveOracle, Target.TargetSubmissionOutcome)
submitLocalReport request expected home report =
  submitTargetCommand
    request
    expected
    home
    ( Target.targetReportCommand
        (Protocol.localAbsenceReportProbe report)
        (Protocol.localAbsenceReportClaim report)
    )

submitInvalidationIntention ::
  OracleClientRequestId ->
  Maybe ControlIndex ->
  Protocol.DisappearanceInvalidationIntention ->
  ProspectiveOracle ->
  (ProspectiveOracle, Target.TargetSubmissionOutcome)
submitInvalidationIntention request expected intention =
  submitTargetCommand
    request
    expected
    (Protocol.disappearanceInvalidationReporter intention)
    ( Target.targetInvalidateCommand
        (Protocol.disappearanceInvalidationProbe intention)
        (Protocol.disappearanceInvalidationReporter intention)
        (targetInvalidationReason (Protocol.disappearanceInvalidationReason intention))
        (Protocol.disappearanceInvalidationWitness intention)
    )

submitResolveIntention ::
  OracleClientRequestId ->
  Maybe ControlIndex ->
  HeraldEpoch ->
  Protocol.DisappearanceResolveIntention ->
  ProspectiveOracle ->
  (ProspectiveOracle, Target.TargetSubmissionOutcome)
submitResolveIntention request expected home intention =
  submitTargetCommand
    request
    expected
    home
    ( Target.targetResolveCommand
        (Protocol.disappearanceResolveProbe intention)
        ( Target.targetCompleteEvidenceDigest
            (Protocol.disappearanceResolveClaims intention)
        )
    )

projectedProbeFromOpen ::
  Target.TargetSubmissionOutcome ->
  Either String Protocol.ProjectedDisappearanceProbe
projectedProbeFromOpen outcome =
  case Projection.translateTargetSubmissionOutcome outcome of
    Right [Projection.DisappearanceProjectedOpen _ projected] -> Right projected
    translated -> Left ("expected one translated Open projection, got " <> show translated)

projectedClaimFromReport ::
  Target.TargetSubmissionOutcome -> Either String DisappearanceEvidenceClaim
projectedClaimFromReport outcome =
  case Projection.translateTargetSubmissionOutcome outcome of
    Right [Projection.DisappearanceProjectedReport _ claim] -> Right claim
    translated -> Left ("expected one translated Report projection, got " <> show translated)

committedTargetEvents ::
  Target.TargetSubmissionOutcome -> [Target.TargetProjectionEvent]
committedTargetEvents outcome = case outcome of
  Target.TargetSubmissionCommitted _ events -> events
  Target.TargetSubmissionDuplicate {} -> []
  Target.TargetSubmissionProtocolRejected {} -> []

prospectiveOracleState :: ProspectiveOracle -> Target.TargetState
prospectiveOracleState (ProspectiveOracle state) = state

-- | Small test-only owner for inputs which legitimately outrun the Oracle
-- projection.  The production cutover will place the same prerequisite hold in
-- PeerInput/OracleAdvance; this detached harness makes that ordering executable
-- without introducing a dormant live-protocol constructor.
data ProspectiveHerald = ProspectiveHerald
  { disappearanceState :: Disappearance.State,
    publicationStream :: PeerStream.State Protocol.DisappearancePublicationMarker,
    heldMarkers :: Map DisappearanceProbeId [HeldMarker]
  }
  deriving stock (Eq, Show)

data HeldMarker
  = HeldPublicationMarker (SequencedItem Protocol.DisappearancePublicationMarker)
  | HeldAlignmentMarker HeraldEpoch Protocol.DisappearanceAlignmentMarker
  deriving stock (Eq, Show)

data ProspectiveHeraldProblem
  = ProspectiveHeraldLeafProblem Disappearance.DisappearanceProblem
  | ProspectiveHeraldCoordinationProblem DisappearanceUseCase.DisappearanceCoordinationProblem
  deriving stock (Eq, Show)

initialProspectiveHerald :: HeraldEpoch -> ProspectiveHerald
initialProspectiveHerald local =
  ProspectiveHerald
    { disappearanceState = Disappearance.initialState local,
      publicationStream = PeerStream.initialState local,
      heldMarkers = Map.empty
    }

prospectiveHeraldState :: ProspectiveHerald -> Disappearance.State
prospectiveHeraldState = (.disappearanceState)

prospectiveHeraldPublicationStream ::
  ProspectiveHerald -> PeerStream.State Protocol.DisappearancePublicationMarker
prospectiveHeraldPublicationStream = (.publicationStream)

heldMarkerProbeIds :: ProspectiveHerald -> [DisappearanceProbeId]
heldMarkerProbeIds = sort . Map.keys . (.heldMarkers)

heldMarkerCount :: ProspectiveHerald -> Int
heldMarkerCount = sum . fmap length . Map.elems . (.heldMarkers)

commitProspectiveHerald ::
  Disappearance.PreparedDisappearanceTransition output ->
  ProspectiveHerald ->
  (ProspectiveHerald, output)
commitProspectiveHerald prepared herald =
  let (successor, output) = Disappearance.commitDisappearanceTransition prepared
   in (herald {disappearanceState = successor}, output)

receivePublicationMarker ::
  SequencedItem Protocol.DisappearancePublicationMarker ->
  ProspectiveHerald ->
  Either ProspectiveHeraldProblem ProspectiveHerald
receivePublicationMarker marker herald = do
  (successorLeaf, successorStream, coordination) <-
    either
      (Left . ProspectiveHeraldCoordinationProblem)
      Right
      ( DisappearanceUseCase.coordinatePublicationMarkerReceipt
          marker
          herald.disappearanceState
          herald.publicationStream
      )
  let successor =
        herald
          { disappearanceState = successorLeaf,
            publicationStream = successorStream
          }
  Right
    ( foldl
        retainAwaitingPublication
        successor
        (DisappearanceUseCase.publicationMarkerReceiptAdmissions coordination)
    )

retainAwaitingPublication ::
  ProspectiveHerald ->
  ( SequencedItem Protocol.DisappearancePublicationMarker,
    Disappearance.MarkerAdmissionDisposition
  ) ->
  ProspectiveHerald
retainAwaitingPublication herald (item, disposition) = case disposition of
  Disappearance.MarkerAwaitingProbeProjection identifier ->
    holdMarker identifier (HeldPublicationMarker item) herald
  _ -> herald

receiveAlignmentMarker ::
  HeraldEpoch ->
  Protocol.DisappearanceAlignmentMarker ->
  ProspectiveHerald ->
  Either ProspectiveHeraldProblem ProspectiveHerald
receiveAlignmentMarker source marker herald = do
  prepared <-
    either
      (Left . ProspectiveHeraldLeafProblem)
      Right
      ( Disappearance.prepareAlignmentMarkerReceipt
          source
          marker
          herald.disappearanceState
      )
  case Disappearance.preparedDisappearanceOutput prepared of
    Disappearance.MarkerAwaitingProbeProjection identifier ->
      Right (holdMarker identifier (HeldAlignmentMarker source marker) herald)
    _ ->
      let (successor, _) = Disappearance.commitDisappearanceTransition prepared
       in Right herald {disappearanceState = successor}

redriveMarkersAfterOpen ::
  DisappearanceProbeId ->
  ProspectiveHerald ->
  Either ProspectiveHeraldProblem ProspectiveHerald
redriveMarkersAfterOpen identifier herald = do
  successor <- foldl redrive (Right herald.disappearanceState) pending
  Right
    herald
      { disappearanceState = successor,
        heldMarkers = Map.delete identifier herald.heldMarkers
      }
  where
    pending = Map.findWithDefault [] identifier herald.heldMarkers
    redrive retained held = do
      state <- retained
      prepared <-
        either
          (Left . ProspectiveHeraldLeafProblem)
          Right
          ( case held of
              HeldPublicationMarker item ->
                Disappearance.preparePublicationMarkerReceipt (sequencedItemPayload item) state
              HeldAlignmentMarker source marker ->
                Disappearance.prepareAlignmentMarkerReceipt source marker state
          )
      case Disappearance.preparedDisappearanceOutput prepared of
        Disappearance.MarkerAwaitingProbeProjection missing ->
          Left (ProspectiveHeraldLeafProblem (Disappearance.DisappearanceProbeMissing missing))
        _ -> Right (fst (Disappearance.commitDisappearanceTransition prepared))

holdMarker ::
  DisappearanceProbeId ->
  HeldMarker ->
  ProspectiveHerald ->
  ProspectiveHerald
holdMarker identifier marker herald =
  herald
    { heldMarkers =
        Map.insertWith
          (\new retained -> nub (new <> retained))
          identifier
          [marker]
          herald.heldMarkers
    }

-- | Detached Oracle-client intention ownership for the prospective vertical.
-- The live OracleClient command sum is intentionally unchanged until the
-- atomic cutover; this ledger nevertheless proves stable semantic keys and
-- request references across reconnect/reoffer.
data ProspectiveRequestLedger = ProspectiveRequestLedger
  { nextSequence :: Map HeraldEpoch Word64,
    retainedRequests :: Map ProspectiveRequestKey RetainedRequest,
    retainedRequestKeys :: Map OracleRequestRef ProspectiveRequestKey,
    requestHistory :: Map OracleRequestRef RetainedRequest
  }
  deriving stock (Eq, Show)

data ProspectiveRequestKind
  = ProspectiveReportRequest
  | ProspectiveInvalidateRequest
  | ProspectiveResolveRequest
  deriving stock (Eq, Ord, Show)

data ProspectiveRequestKey
  = ProspectiveOpenKey
      HeraldEpoch
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
  | ProspectiveProbeKey
      HeraldEpoch
      DisappearanceProbeId
      ProspectiveRequestKind
  deriving stock (Eq, Ord, Show)

data ProspectiveIntention
  = ProspectiveOpenIntention Protocol.DisappearanceOpenIntention
  | ProspectiveReportIntention Protocol.LocalAbsenceReport
  | ProspectiveInvalidationIntention Protocol.DisappearanceInvalidationIntention
  | ProspectiveResolveIntention Protocol.DisappearanceResolveIntention
  deriving stock (Eq, Show)

data RetainedRequest = RetainedRequest
  { reference :: OracleRequestRef,
    intention :: ProspectiveIntention,
    status :: ProspectiveRequestStatus
  }
  deriving stock (Eq, Show)

data ProspectiveRequestStatus
  = ProspectiveRequestAwaitingProjection
  | ProspectiveRequestSettled ProspectiveSettlementProof
  deriving stock (Eq, Show)

data ProspectiveSettlementProof
  = ProspectiveAcceptedProjection ProspectiveProjectionProof
  | ProspectiveRejectedProjection ControlIndex Target.TargetRejection
  deriving stock (Eq, Show)

data ProspectiveProjectionProof
  = ProspectiveOpenProjection DisappearanceProbeId
  | ProspectiveReportProjection DisappearanceProbeId HeraldEpoch
  | ProspectiveInvalidationProjection DisappearanceProbeId
  | ProspectiveResolveProjection DisappearanceProbeId
  deriving stock (Eq, Show)

-- | Opaque evidence that an exact retained request was dispatched to the
-- prospective Oracle.  Settlement consumes this evidence rather than trusting
-- a caller-supplied request reference.
data ProspectiveRequestSubmission = ProspectiveRequestSubmission
  { submissionReference :: OracleRequestRef,
    submissionIntention :: ProspectiveIntention,
    submissionOutcome :: Target.TargetSubmissionOutcome
  }
  deriving stock (Eq, Show)

-- | Opaque evidence of a terminal event emitted by the prospective Oracle.
-- The contained leaf input cannot be fabricated by callers of this harness.
data ProspectiveTerminalProjection
  = ProspectiveTerminalProjection
      OracleRequestRef
      Protocol.ProjectedDisappearanceTerminal
  deriving stock (Eq, Show)

data ProspectiveRequestDisposition
  = ProspectiveRequestRetained OracleRequestRef
  | ProspectiveRequestDuplicate OracleRequestRef
  | ProspectiveRequestConflict OracleRequestRef
  deriving stock (Eq, Show)

data ProspectiveRequestProblem
  = ProspectiveRequestNotRetained
  | ProspectiveRequestIntentionMismatch OracleRequestRef
  | ProspectiveRequestAlreadySettled OracleRequestRef
  | ProspectiveRequestNotSettled OracleRequestRef
  | ProspectiveRequestReferenceUnknown OracleRequestRef
  | ProspectiveRequestProjectionRejected OracleRequestRef
  | ProspectiveRequestProjectionMismatch OracleRequestRef
  | ProspectiveRequestHasNoTerminalProjection OracleRequestRef
  | ProspectiveRequestTerminalProbeMismatch
      OracleRequestRef
      DisappearanceProbeId
      DisappearanceProbeId
  | ProspectiveRequestTerminalRejected Disappearance.DisappearanceProblem
  deriving stock (Eq, Show)

data ProspectiveRequestSettlement
  = ProspectiveRequestSettledNow
  | ProspectiveRequestSettlementDuplicate
  deriving stock (Eq, Show)

data ProspectiveRequestRelease
  = ProspectiveRequestReleasedNow
  | ProspectiveRequestReleaseDuplicate
  deriving stock (Eq, Show)

initialProspectiveRequestLedger :: ProspectiveRequestLedger
initialProspectiveRequestLedger =
  ProspectiveRequestLedger
    { nextSequence = Map.empty,
      retainedRequests = Map.empty,
      retainedRequestKeys = Map.empty,
      requestHistory = Map.empty
    }

prospectiveRequestDispositionRef ::
  ProspectiveRequestDisposition -> OracleRequestRef
prospectiveRequestDispositionRef disposition = case disposition of
  ProspectiveRequestRetained reference -> reference
  ProspectiveRequestDuplicate reference -> reference
  ProspectiveRequestConflict reference -> reference

prospectiveRequestCount :: ProspectiveRequestLedger -> Int
prospectiveRequestCount = Map.size . (.requestHistory)

prospectiveOutstandingRequestCount :: ProspectiveRequestLedger -> Int
prospectiveOutstandingRequestCount ledger =
  length
    [ ()
    | retained <- Map.elems ledger.retainedRequests,
      isAwaitingProjection retained.status
    ]

isAwaitingProjection :: ProspectiveRequestStatus -> Bool
isAwaitingProjection status = case status of
  ProspectiveRequestAwaitingProjection -> True
  ProspectiveRequestSettled {} -> False

retainOpenRequest ::
  HeraldEpoch ->
  Protocol.DisappearanceOpenIntention ->
  ProspectiveRequestLedger ->
  (ProspectiveRequestLedger, ProspectiveRequestDisposition)
retainOpenRequest home intention =
  retainRequest
    home
    ( ProspectiveOpenKey
        home
        (Protocol.disappearanceOpenIntentionSubject intention)
        (Protocol.disappearanceOpenIntentionCoordinate intention)
    )
    (ProspectiveOpenIntention intention)

retainReportRequest ::
  HeraldEpoch ->
  Protocol.LocalAbsenceReport ->
  ProspectiveRequestLedger ->
  (ProspectiveRequestLedger, ProspectiveRequestDisposition)
retainReportRequest home report =
  retainRequest
    home
    ( ProspectiveProbeKey
        home
        (Protocol.localAbsenceReportProbe report)
        ProspectiveReportRequest
    )
    (ProspectiveReportIntention report)

retainInvalidationRequest ::
  Protocol.DisappearanceInvalidationIntention ->
  ProspectiveRequestLedger ->
  (ProspectiveRequestLedger, ProspectiveRequestDisposition)
retainInvalidationRequest intention =
  retainRequest
    (Protocol.disappearanceInvalidationReporter intention)
    ( ProspectiveProbeKey
        (Protocol.disappearanceInvalidationReporter intention)
        (Protocol.disappearanceInvalidationProbe intention)
        ProspectiveInvalidateRequest
    )
    (ProspectiveInvalidationIntention intention)

retainResolveRequest ::
  HeraldEpoch ->
  Protocol.DisappearanceResolveIntention ->
  ProspectiveRequestLedger ->
  (ProspectiveRequestLedger, ProspectiveRequestDisposition)
retainResolveRequest home intention =
  retainRequest
    home
    ( ProspectiveProbeKey
        home
        (Protocol.disappearanceResolveProbe intention)
        ProspectiveResolveRequest
    )
    (ProspectiveResolveIntention intention)

retainRequest ::
  HeraldEpoch ->
  ProspectiveRequestKey ->
  ProspectiveIntention ->
  ProspectiveRequestLedger ->
  (ProspectiveRequestLedger, ProspectiveRequestDisposition)
retainRequest home key supplied ledger =
  case Map.lookup key ledger.retainedRequests of
    Just retained
      | retained.intention == supplied ->
          (ledger, ProspectiveRequestDuplicate retained.reference)
      | otherwise ->
          (ledger, ProspectiveRequestConflict retained.reference)
    Nothing ->
      let sequenceNumber = Map.findWithDefault 1 home ledger.nextSequence
          reference = OracleRequestRef (oracleClientRequestId home sequenceNumber)
          successor =
            ledger
              { nextSequence = Map.insert home (sequenceNumber + 1) ledger.nextSequence,
                retainedRequests =
                  Map.insert
                    key
                    RetainedRequest
                      { reference,
                        intention = supplied,
                        status = ProspectiveRequestAwaitingProjection
                      }
                    ledger.retainedRequests,
                retainedRequestKeys =
                  Map.insert reference key ledger.retainedRequestKeys,
                requestHistory =
                  Map.insert
                    reference
                    RetainedRequest
                      { reference,
                        intention = supplied,
                        status = ProspectiveRequestAwaitingProjection
                      }
                    ledger.requestHistory
              }
       in (successor, ProspectiveRequestRetained reference)

settleProspectiveRequest ::
  ProspectiveRequestSubmission ->
  ProspectiveRequestLedger ->
  Either
    ProspectiveRequestProblem
    (ProspectiveRequestLedger, ProspectiveRequestSettlement)
settleProspectiveRequest submission ledger = do
  let reference = submission.submissionReference
  retained <-
    maybe
      (Left (ProspectiveRequestReferenceUnknown reference))
      Right
      (Map.lookup reference ledger.requestHistory)
  unless
    (retained.intention == submission.submissionIntention)
    (Left (ProspectiveRequestIntentionMismatch reference))
  proof <- submissionSettlementProof submission
  case retained.status of
    ProspectiveRequestSettled retainedProof
      | retainedProof == proof ->
          Right (ledger, ProspectiveRequestSettlementDuplicate)
      | otherwise -> Left (ProspectiveRequestProjectionMismatch reference)
    ProspectiveRequestAwaitingProjection -> do
      key <-
        maybe
          (Left (ProspectiveRequestReferenceUnknown reference))
          Right
          (Map.lookup reference ledger.retainedRequestKeys)
      let settled = retained {status = ProspectiveRequestSettled proof}
      Right
        ( ledger
            { retainedRequests =
                Map.adjust
                  (\active -> if active.reference == reference then settled else active)
                  key
                  ledger.retainedRequests,
              requestHistory = Map.insert reference settled ledger.requestHistory
            },
          ProspectiveRequestSettledNow
        )

prospectiveRequestSubmissionOutcome ::
  ProspectiveRequestSubmission -> Target.TargetSubmissionOutcome
prospectiveRequestSubmissionOutcome = (.submissionOutcome)

-- | Extract an opaque terminal projection from an accepted Target submission.
-- This is deliberately unavailable for Open and Report: only a command which
-- actually emitted a terminal may authorize releasing an Open binding.
prospectiveTerminalFromSubmission ::
  ProspectiveRequestSubmission ->
  Either ProspectiveRequestProblem ProspectiveTerminalProjection
prospectiveTerminalFromSubmission submission = do
  proof <- submissionProjectionProof submission
  let reference = submission.submissionReference
  case (submission.submissionIntention, proof, submission.submissionOutcome) of
    ( ProspectiveInvalidationIntention intention,
      ProspectiveInvalidationProjection probe,
      Target.TargetSubmissionCommitted
        receipt
        [ Target.TargetInvalidatedProjected
            projectedProbe
            (Target.TargetCommandInvalidation reporter reason witness)
          ]
      )
        | projectedProbe == probe,
          reporter == Protocol.disappearanceInvalidationReporter intention,
          reason == targetInvalidationReason (Protocol.disappearanceInvalidationReason intention),
          witness == Protocol.disappearanceInvalidationWitness intention ->
            Right
              ( ProspectiveTerminalProjection
                  reference
                  ( Protocol.ProjectedDisappearanceInvalidated
                      probe
                      ( Protocol.ProjectedCommandInvalidation
                          (Protocol.disappearanceInvalidationReporter intention)
                          (Protocol.disappearanceInvalidationReason intention)
                          (Protocol.disappearanceInvalidationWitness intention)
                      )
                      (Target.targetReceiptControlIndex receipt)
                  )
              )
    ( ProspectiveResolveIntention _,
      ProspectiveResolveProjection probe,
      Target.TargetSubmissionCommitted
        receipt
        [Target.TargetResolvedProjected projectedProbe outcome]
      ) ->
        if projectedProbe == probe
          then
            Right
              ( ProspectiveTerminalProjection
                  reference
                  ( Protocol.ProjectedDisappearanceResolved
                      probe
                      outcome
                      (Target.targetReceiptControlIndex receipt)
                  )
              )
          else Left (ProspectiveRequestProjectionMismatch reference)
    _ -> Left (ProspectiveRequestHasNoTerminalProjection reference)

-- | Apply an Oracle-projected terminal and release its exact settled Open
-- reference atomically in the test harness. Historical request identity is
-- retained. Replaying an old reference is inert and cannot delete a later
-- active binding for the same semantic key (the ABA case). Increment 9 replaces
-- this detached owner with the real OracleClient lifecycle.
releaseProspectiveOpenRequest ::
  OracleRequestRef ->
  ProspectiveTerminalProjection ->
  Disappearance.State ->
  ProspectiveRequestLedger ->
  Either
    ProspectiveRequestProblem
    ( Disappearance.State,
      ProspectiveRequestLedger,
      Disappearance.TerminalDisposition,
      ProspectiveRequestRelease
    )
releaseProspectiveOpenRequest reference (ProspectiveTerminalProjection terminalSource terminal) state ledger = do
  retained <-
    maybe
      (Left (ProspectiveRequestReferenceUnknown reference))
      Right
      (Map.lookup reference ledger.requestHistory)
  key <-
    maybe
      (Left (ProspectiveRequestReferenceUnknown reference))
      Right
      (Map.lookup reference ledger.retainedRequestKeys)
  case retained.intention of
    ProspectiveOpenIntention {} -> Right ()
    _ -> Left ProspectiveRequestNotRetained
  projectedProbe <- case retained.status of
    ProspectiveRequestAwaitingProjection ->
      Left (ProspectiveRequestNotSettled reference)
    ProspectiveRequestSettled settlement -> case settlement of
      ProspectiveAcceptedProjection (ProspectiveOpenProjection probe) -> Right probe
      _ -> Left (ProspectiveRequestProjectionMismatch reference)
  let terminalProbe = projectedTerminalProbe terminal
  unless
    (terminalProbe == projectedProbe)
    ( Left
        ( ProspectiveRequestTerminalProbeMismatch
            reference
            projectedProbe
            terminalProbe
        )
    )
  sourceRequest <-
    maybe
      (Left (ProspectiveRequestReferenceUnknown terminalSource))
      Right
      (Map.lookup terminalSource ledger.requestHistory)
  unless
    (terminalSourceAuthorizes terminalProbe sourceRequest.status)
    (Left (ProspectiveRequestNotSettled terminalSource))
  prepared <-
    either
      (Left . ProspectiveRequestTerminalRejected)
      Right
      (Disappearance.prepareProjectedTerminal terminal state)
  let (successor, terminalDisposition) =
        Disappearance.commitDisappearanceTransition prepared
      releaseDisposition =
        case Map.lookup key ledger.retainedRequests of
          Just active
            | active.reference == reference ->
                ProspectiveRequestReleasedNow
          _ -> ProspectiveRequestReleaseDuplicate
      successorLedger =
        ledger
          { retainedRequests =
              Map.filter
                (not . requestBelongsToTerminalProbe terminalProbe)
                ledger.retainedRequests
          }
  Right
    ( successor,
      successorLedger,
      terminalDisposition,
      releaseDisposition
    )

terminalSourceAuthorizes ::
  DisappearanceProbeId -> ProspectiveRequestStatus -> Bool
terminalSourceAuthorizes probe status = case status of
  ProspectiveRequestSettled (ProspectiveAcceptedProjection proof) ->
    case proof of
      ProspectiveInvalidationProjection projected -> projected == probe
      ProspectiveResolveProjection projected -> projected == probe
      _ -> False
  _ -> False

requestBelongsToTerminalProbe :: DisappearanceProbeId -> RetainedRequest -> Bool
requestBelongsToTerminalProbe probe retained =
  case retained.status of
    ProspectiveRequestSettled (ProspectiveAcceptedProjection proof) ->
      projectionProofProbe proof == probe
    _ -> case retained.intention of
      ProspectiveReportIntention report ->
        Protocol.localAbsenceReportProbe report == probe
      ProspectiveInvalidationIntention invalidation ->
        Protocol.disappearanceInvalidationProbe invalidation == probe
      ProspectiveResolveIntention resolve ->
        Protocol.disappearanceResolveProbe resolve == probe
      ProspectiveOpenIntention {} -> False

projectionProofProbe :: ProspectiveProjectionProof -> DisappearanceProbeId
projectionProofProbe proof = case proof of
  ProspectiveOpenProjection probe -> probe
  ProspectiveReportProjection probe _ -> probe
  ProspectiveInvalidationProjection probe -> probe
  ProspectiveResolveProjection probe -> probe

submitRetainedOpenRequest ::
  Maybe ControlIndex ->
  HeraldEpoch ->
  Protocol.DisappearanceOpenIntention ->
  ProspectiveRequestLedger ->
  ProspectiveOracle ->
  Either ProspectiveRequestProblem (ProspectiveOracle, ProspectiveRequestSubmission)
submitRetainedOpenRequest expected home intention ledger oracle = do
  reference <-
    dispatchReference
      ( ProspectiveOpenKey
          home
          (Protocol.disappearanceOpenIntentionSubject intention)
          (Protocol.disappearanceOpenIntentionCoordinate intention)
      )
      (ProspectiveOpenIntention intention)
      ledger
  let (successor, outcome) =
        submitOpenIntention
          (oracleRequestRefRequestId reference)
          expected
          home
          intention
          oracle
  Right
    ( successor,
      ProspectiveRequestSubmission reference (ProspectiveOpenIntention intention) outcome
    )

submitRetainedReportRequest ::
  Maybe ControlIndex ->
  HeraldEpoch ->
  Protocol.LocalAbsenceReport ->
  ProspectiveRequestLedger ->
  ProspectiveOracle ->
  Either ProspectiveRequestProblem (ProspectiveOracle, ProspectiveRequestSubmission)
submitRetainedReportRequest expected home report ledger oracle = do
  reference <-
    dispatchReference
      ( ProspectiveProbeKey
          home
          (Protocol.localAbsenceReportProbe report)
          ProspectiveReportRequest
      )
      (ProspectiveReportIntention report)
      ledger
  let (successor, outcome) =
        submitLocalReport
          (oracleRequestRefRequestId reference)
          expected
          home
          report
          oracle
  Right
    ( successor,
      ProspectiveRequestSubmission reference (ProspectiveReportIntention report) outcome
    )

submitRetainedInvalidationRequest ::
  Maybe ControlIndex ->
  Protocol.DisappearanceInvalidationIntention ->
  ProspectiveRequestLedger ->
  ProspectiveOracle ->
  Either ProspectiveRequestProblem (ProspectiveOracle, ProspectiveRequestSubmission)
submitRetainedInvalidationRequest expected intention ledger oracle = do
  reference <-
    dispatchReference
      ( ProspectiveProbeKey
          home
          (Protocol.disappearanceInvalidationProbe intention)
          ProspectiveInvalidateRequest
      )
      (ProspectiveInvalidationIntention intention)
      ledger
  let (successor, outcome) =
        submitInvalidationIntention
          (oracleRequestRefRequestId reference)
          expected
          intention
          oracle
  Right
    ( successor,
      ProspectiveRequestSubmission
        reference
        (ProspectiveInvalidationIntention intention)
        outcome
    )
  where
    home = Protocol.disappearanceInvalidationReporter intention

submitRetainedResolveRequest ::
  Maybe ControlIndex ->
  HeraldEpoch ->
  Protocol.DisappearanceResolveIntention ->
  ProspectiveRequestLedger ->
  ProspectiveOracle ->
  Either ProspectiveRequestProblem (ProspectiveOracle, ProspectiveRequestSubmission)
submitRetainedResolveRequest expected home intention ledger oracle = do
  reference <-
    dispatchReference
      ( ProspectiveProbeKey
          home
          (Protocol.disappearanceResolveProbe intention)
          ProspectiveResolveRequest
      )
      (ProspectiveResolveIntention intention)
      ledger
  let (successor, outcome) =
        submitResolveIntention
          (oracleRequestRefRequestId reference)
          expected
          home
          intention
          oracle
  Right
    ( successor,
      ProspectiveRequestSubmission reference (ProspectiveResolveIntention intention) outcome
    )

dispatchReference ::
  ProspectiveRequestKey ->
  ProspectiveIntention ->
  ProspectiveRequestLedger ->
  Either ProspectiveRequestProblem OracleRequestRef
dispatchReference key supplied ledger = case Map.lookup key ledger.retainedRequests of
  Nothing -> Left ProspectiveRequestNotRetained
  Just retained
    | retained.intention /= supplied ->
        Left (ProspectiveRequestIntentionMismatch retained.reference)
    | otherwise -> case retained.status of
        ProspectiveRequestSettled {} ->
          Left (ProspectiveRequestAlreadySettled retained.reference)
        ProspectiveRequestAwaitingProjection -> Right retained.reference

submissionProjectionProof ::
  ProspectiveRequestSubmission -> Either ProspectiveRequestProblem ProspectiveProjectionProof
submissionProjectionProof submission = do
  settlement <- submissionSettlementProof submission
  case settlement of
    ProspectiveAcceptedProjection proof -> Right proof
    ProspectiveRejectedProjection {} ->
      Left (ProspectiveRequestProjectionRejected submission.submissionReference)

submissionSettlementProof ::
  ProspectiveRequestSubmission -> Either ProspectiveRequestProblem ProspectiveSettlementProof
submissionSettlementProof submission = do
  receipt <- submissionReceipt submission
  let reference = submission.submissionReference
  unless
    (Target.targetReceiptRequestId receipt == oracleRequestRefRequestId reference)
    (Left (ProspectiveRequestProjectionMismatch reference))
  case Target.targetReceiptResult receipt of
    Target.TargetReceiptAccepted accepted -> do
      proof <- acceptedProjectionProof reference submission.submissionIntention accepted
      case submission.submissionOutcome of
        Target.TargetSubmissionCommitted _ events ->
          unless
            (committedEventsMatch submission.submissionIntention accepted events)
            (Left (ProspectiveRequestProjectionMismatch reference))
        Target.TargetSubmissionDuplicate {} -> Right ()
        Target.TargetSubmissionProtocolRejected {} ->
          Left (ProspectiveRequestProjectionRejected reference)
      Right (ProspectiveAcceptedProjection proof)
    Target.TargetReceiptRejected rejection -> do
      case submission.submissionOutcome of
        Target.TargetSubmissionCommitted _ events ->
          unless
            (null events)
            (Left (ProspectiveRequestProjectionMismatch reference))
        Target.TargetSubmissionDuplicate {} -> Right ()
        Target.TargetSubmissionProtocolRejected {} ->
          Left (ProspectiveRequestProjectionRejected reference)
      Right
        ( ProspectiveRejectedProjection
            (Target.targetReceiptControlIndex receipt)
            rejection
        )

submissionReceipt ::
  ProspectiveRequestSubmission -> Either ProspectiveRequestProblem Target.TargetReceipt
submissionReceipt submission = case submission.submissionOutcome of
  Target.TargetSubmissionCommitted receipt _ -> Right receipt
  Target.TargetSubmissionDuplicate receipt -> Right receipt
  Target.TargetSubmissionProtocolRejected {} ->
    Left (ProspectiveRequestProjectionRejected submission.submissionReference)

acceptedProjectionProof ::
  OracleRequestRef ->
  ProspectiveIntention ->
  Target.TargetAcceptedResult ->
  Either ProspectiveRequestProblem ProspectiveProjectionProof
acceptedProjectionProof reference intention accepted =
  case (intention, accepted) of
    (ProspectiveOpenIntention _, Target.TargetOpenAccepted result) ->
      Right (ProspectiveOpenProjection (openResultProbe result))
    ( ProspectiveReportIntention report,
      Target.TargetReportAccepted probe reporter
      )
        | probe == Protocol.localAbsenceReportProbe report
            && reporter
              == disappearanceEvidenceClaimReporter
                (Protocol.localAbsenceReportClaim report) ->
            Right (ProspectiveReportProjection probe reporter)
    ( ProspectiveInvalidationIntention invalidation,
      Target.TargetInvalidateAccepted probe
      )
        | probe == Protocol.disappearanceInvalidationProbe invalidation ->
            Right (ProspectiveInvalidationProjection probe)
    ( ProspectiveResolveIntention resolve,
      Target.TargetResolveAccepted _
      ) ->
        Right (ProspectiveResolveProjection (Protocol.disappearanceResolveProbe resolve))
    _ -> Left (ProspectiveRequestProjectionMismatch reference)

committedEventsMatch ::
  ProspectiveIntention ->
  Target.TargetAcceptedResult ->
  [Target.TargetProjectionEvent] ->
  Bool
committedEventsMatch supplied accepted events = case (supplied, accepted, events) of
  ( ProspectiveOpenIntention openIntention,
    Target.TargetOpenAccepted result,
    [Target.TargetOpenProjected header projected]
    ) ->
      projected == result
        && Target.targetProjectedProbeHeaderId header == openResultProbe result
        && Target.targetProjectedProbeHeaderSubject header
          == Protocol.disappearanceOpenIntentionSubject openIntention
        && Target.targetProjectedProbeHeaderCoordinate header
          == Protocol.disappearanceOpenIntentionCoordinate openIntention
  ( ProspectiveReportIntention report,
    Target.TargetReportAccepted probe reporter,
    [Target.TargetReportProjected projectedClaim]
    ) ->
      projectedClaim == Protocol.localAbsenceReportClaim report
        && probe == Protocol.localAbsenceReportProbe report
        && reporter == disappearanceEvidenceClaimReporter projectedClaim
  ( ProspectiveReportIntention report,
    Target.TargetReportAccepted probe reporter,
    []
    ) ->
      probe == Protocol.localAbsenceReportProbe report
        && reporter
          == disappearanceEvidenceClaimReporter
            (Protocol.localAbsenceReportClaim report)
  ( ProspectiveInvalidationIntention invalidation,
    Target.TargetInvalidateAccepted probe,
    [ Target.TargetInvalidatedProjected
        projectedProbe
        (Target.TargetCommandInvalidation reporter reason witness)
      ]
    ) ->
      projectedProbe == probe
        && probe == Protocol.disappearanceInvalidationProbe invalidation
        && reporter == Protocol.disappearanceInvalidationReporter invalidation
        && reason == targetInvalidationReason (Protocol.disappearanceInvalidationReason invalidation)
        && witness == Protocol.disappearanceInvalidationWitness invalidation
  ( ProspectiveInvalidationIntention invalidation,
    Target.TargetInvalidateAccepted probe,
    []
    ) -> probe == Protocol.disappearanceInvalidationProbe invalidation
  ( ProspectiveResolveIntention resolve,
    Target.TargetResolveAccepted outcome,
    [Target.TargetResolvedProjected projectedProbe projectedOutcome]
    ) ->
      projectedProbe == Protocol.disappearanceResolveProbe resolve
        && projectedOutcome == outcome
  ( ProspectiveResolveIntention _,
    Target.TargetResolveAccepted _,
    []
    ) -> True
  _ -> False

openResultProbe :: DisappearanceOpenResult -> DisappearanceProbeId
openResultProbe result = case disappearanceOpenResultView result of
  OpenedDisappearanceProbe probe -> probe
  AliasedDisappearanceProbe probe -> probe

projectedTerminalProbe :: Protocol.ProjectedDisappearanceTerminal -> DisappearanceProbeId
projectedTerminalProbe terminal = case terminal of
  Protocol.ProjectedDisappearanceInvalidated probe _ _ -> probe
  Protocol.ProjectedDisappearanceResolved probe _ _ -> probe
  Protocol.ProjectedDisappearanceAborted probe _ _ -> probe

targetInvalidationReason ::
  Protocol.DisappearanceInvalidationReason -> Target.TargetInvalidationReason
targetInvalidationReason reason = case reason of
  Protocol.MatchingPublicationObserved -> Target.TargetMatchingPublicationObserved
  Protocol.LocalEvidenceContradicted -> Target.TargetEvidenceContradicted
