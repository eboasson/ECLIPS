{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Step16DisappearanceProperties (tests) where

import Control.Monad (foldM, forM, forM_)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (permutations, sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Word (Word64, Word8)
import Eclips.Domain.Alignment (HeraldPublicationPrefix (EmptyHeraldPublicationPrefix))
import Eclips.Domain.Disappearance
import Eclips.Domain.Identity
import Eclips.Domain.Label
import Eclips.Domain.Membership
import Eclips.Domain.Sort.Canonical (CanonicalDescriptor, descriptorSortId)
import Eclips.Domain.Sort.Profile
import Eclips.Domain.SortOccurrence
import Eclips.Domain.Startup
import Eclips.Domain.Value (LabelOwner (ProcessLabel, VoidLabel))
import Eclips.Oracle.Disappearance qualified as D
import Eclips.Oracle.Failure qualified as F
import Eclips.Oracle.Genesis
import Eclips.Oracle.Identity
import Eclips.Oracle.Label qualified as L
import Eclips.Oracle.State qualified as Checkpoint
import Eclips.Oracle.Step16.DisappearanceReference qualified as R
import Eclips.Oracle.Voter qualified as V
import Numeric (showHex)
import OracleFixtures hiding (fixtureInitialState)
import Step15ReferenceProperties qualified as FailureFixture
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (witness)
import VoterFailureFixtures qualified as VF

data NormalInvalidation
  = NormalMatchingPublication HeraldEpoch DisappearanceEvidenceDigest
  | NormalInvalidEvidence HeraldEpoch DisappearanceEvidenceDigest
  | NormalLabelOpen LabelDecisionId
  deriving stock (Eq, Show)

data NormalAbort
  = NormalAuthorizedAbort
  | NormalMembershipSuperseded HeraldMembershipGenerationId
  | NormalAdmissionPreparing HeraldAdmissionId
  deriving stock (Eq, Show)

data NormalPhase
  = NormalCollecting
  | NormalInvalidated NormalInvalidation ControlIndex
  | NormalResolved DisappearanceResolutionOutcome ControlIndex
  | NormalAborted NormalAbort ControlIndex
  deriving stock (Eq, Show)

data NormalProbe = NormalProbe
  { normalProbeId :: DisappearanceProbeId,
    normalProbeSubject :: DisappearanceSubject,
    normalProbeCoordinate :: DisappearanceSubjectMembershipCoordinate,
    normalProbeOpenIndex :: ControlIndex,
    normalProbeMembers :: [HeraldEpoch],
    normalProbeReports :: [(HeraldEpoch, ByteString)],
    normalProbePhase :: NormalPhase
  }
  deriving stock (Eq, Show)

data NormalAccepted
  = NormalOpenAccepted DisappearanceOpenResult
  | NormalReportAccepted DisappearanceProbeId
  | NormalInvalidateAccepted DisappearanceProbeId
  | NormalResolveAccepted DisappearanceResolutionOutcome
  | NormalAbortAccepted DisappearanceProbeId
  deriving stock (Eq, Show)

data NormalRejection
  = NormalHomeMismatch
  | NormalInactiveHome
  | NormalStaleIndex
  | NormalCoordinateMismatch
  | NormalControlledDeleted
  | NormalControlledRevision
  | NormalLabelInProgress
  | NormalRegularOccurrence
  | NormalUnknownProbe
  | NormalTerminalProbe
  | NormalReporterHome
  | NormalReporterNotCaptured
  | NormalEvidenceProbe
  | NormalEvidenceCoordinate
  | NormalConflictingReport
  | NormalIncompleteEvidence
  | NormalCompleteEvidenceMismatch
  | NormalAdmissionBusy
  | NormalMembershipSuccessor
  | NormalMembershipIndex
  | NormalLabelMustBeControlled
  | NormalLabelFromDeleted
  | NormalLabelTerminalWithoutOpen
  | NormalLabelRevisionIndex
  deriving stock (Eq, Show)

data NormalReceiptResult
  = NormalReceiptAccepted NormalAccepted
  | NormalReceiptRejected NormalRejection
  deriving stock (Eq, Show)

data NormalProjectedProbeHeader
  = NormalProjectedProbeHeader
      DisappearanceProbeId
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
      HeraldMembershipGeneration
  deriving stock (Eq, Show)

data NormalEvent
  = NormalOpenProjected NormalProjectedProbeHeader DisappearanceOpenResult
  | NormalReportProjected DisappearanceEvidenceClaim
  | NormalInvalidatedProjected DisappearanceProbeId NormalInvalidation
  | NormalResolvedProjected DisappearanceProbeId DisappearanceResolutionOutcome
  | NormalAbortedProjected DisappearanceProbeId NormalAbort
  | NormalMembershipProjected HeraldMembershipGeneration
  | NormalLabelOpenedProjected LabelDecisionId GlobalObjectId
  | NormalLabelNotAppliedProjected LabelDecisionId GlobalObjectId
  | NormalLabelReleasedProjected LabelDecisionId GlobalObjectId LabelRevision
  | NormalLabelDeletedProjected LabelDecisionId GlobalObjectId LabelRevision
  deriving stock (Eq, Show)

data NormalSubmission
  = NormalCommitted ControlIndex NormalReceiptResult [NormalEvent]
  | NormalDuplicate ControlIndex NormalReceiptResult
  | NormalProtocolRejected
  deriving stock (Eq, Show)

data NormalContext
  = NormalContextCommitted ControlIndex [NormalEvent]
  | NormalContextRejected ControlIndex NormalRejection
  | NormalContextDuplicate ControlIndex
  deriving stock (Eq, Show)

data NormalState = NormalState
  { normalIndex :: ControlIndex,
    normalMembership :: HeraldMembershipGeneration,
    normalRequestCount :: Int,
    normalProbes :: [NormalProbe],
    normalRetirements :: [(SortId, Maybe ControlIndex)],
    normalOpenRequests :: [(OracleClientRequestId, Maybe DisappearanceProbeId)]
  }
  deriving stock (Eq, Show)

normalReferenceProbe :: R.ReferenceProbeView -> NormalProbe
normalReferenceProbe
  (R.ReferenceProbeView probe subject coordinate openedAt capturedMembers reports phase) =
    NormalProbe
      probe
      subject
      coordinate
      openedAt
      (sort capturedMembers)
      (normalizeReports reports)
      (normalReferencePhase phase)

normalReferencePhase :: R.ReferenceProbePhaseView -> NormalPhase
normalReferencePhase phase = case phase of
  R.ReferenceCollectingEvidenceView -> NormalCollecting
  R.ReferenceInvalidatedView cause index ->
    NormalInvalidated (normalReferenceInvalidation cause) index
  R.ReferenceResolvedView index outcome -> NormalResolved outcome index
  R.ReferenceAbortedView reason index ->
    NormalAborted (normalReferenceAbort reason) index

normalReferenceInvalidation :: R.ReferenceInvalidationCauseView -> NormalInvalidation
normalReferenceInvalidation cause = case cause of
  R.ReferenceCommandInvalidationView reporter reason witness ->
    case reason of
      R.ReferenceMatchingPublication -> NormalMatchingPublication reporter witness
      R.ReferenceInvalidEvidence -> NormalInvalidEvidence reporter witness
  R.ReferenceLabelWorkflowOpenedInvalidationView decision -> NormalLabelOpen decision

normalReferenceAbort :: R.ReferenceProbeAbortReasonView -> NormalAbort
normalReferenceAbort reason = case reason of
  R.ReferenceAuthorizedAbortView -> NormalAuthorizedAbort
  R.ReferenceMembershipSupersededView generation ->
    NormalMembershipSuperseded generation

normalLiveProbe :: D.DisappearanceProbeView -> NormalProbe
normalLiveProbe
  (D.DisappearanceProbeView probe subject coordinate _generation capturedMembers openedAt reports phase) =
    NormalProbe
      probe
      subject
      coordinate
      openedAt
      (sort capturedMembers)
      (normalizeReports reports)
      (normalLivePhase phase)

normalLivePhase :: D.DisappearanceProbePhaseView -> NormalPhase
normalLivePhase phase = case phase of
  D.DisappearanceCollectingView -> NormalCollecting
  D.DisappearanceInvalidatedView cause index ->
    NormalInvalidated (normalLiveInvalidation cause) index
  D.DisappearanceResolvedView outcome index -> NormalResolved outcome index
  D.DisappearanceAbortedView _ index -> NormalAborted NormalAuthorizedAbort index
  D.DisappearanceMembershipSupersededView generation index ->
    NormalAborted (NormalMembershipSuperseded generation) index
  D.DisappearanceAdmissionPreparingView admission index ->
    NormalAborted (NormalAdmissionPreparing admission) index

normalLiveInvalidation :: D.DisappearanceProbeInvalidationView -> NormalInvalidation
normalLiveInvalidation cause = case cause of
  D.CommandDisappearanceInvalidation reporter reason witness -> case reason of
    D.MatchingPublicationObserved -> NormalMatchingPublication reporter witness
    D.EvidenceContradicted -> NormalInvalidEvidence reporter witness
  D.LabelOpenDisappearanceInvalidation decision -> NormalLabelOpen decision

normalizeReports :: [(HeraldEpoch, DisappearanceEvidenceClaim)] -> [(HeraldEpoch, ByteString)]
normalizeReports =
  sortOn fst
    . fmap (\(reporter, claim) -> (reporter, disappearanceEvidenceClaimCanonicalBytes claim))

normalReferenceSubmission :: R.ReferenceSubmissionOutcome -> NormalSubmission
normalReferenceSubmission outcome =
  case R.referenceSubmissionOutcomeView outcome of
    R.ReferenceSubmissionCommittedView receipt events ->
      let (index, result) = normalReferenceReceipt receipt
       in NormalCommitted index result (fmap normalReferenceEvent events)
    R.ReferenceSubmissionDuplicateView receipt ->
      let (index, result) = normalReferenceReceipt receipt
       in NormalDuplicate index result
    R.ReferenceSubmissionProtocolRejectedView {} -> NormalProtocolRejected

normalReferenceReceipt :: R.ReferenceReceipt -> (ControlIndex, NormalReceiptResult)
normalReferenceReceipt receipt = case R.referenceReceiptView receipt of
  R.ReferenceReceiptView _request _digest index result ->
    (index, normalReferenceReceiptResult result)

normalReferenceReceiptResult :: R.ReferenceReceiptResultView -> NormalReceiptResult
normalReferenceReceiptResult result = case result of
  R.ReferenceReceiptAcceptedView accepted ->
    NormalReceiptAccepted (normalReferenceAccepted accepted)
  R.ReferenceReceiptRejectedView rejection ->
    NormalReceiptRejected (normalReferenceRejection rejection)

normalReferenceAccepted :: R.ReferenceAcceptedResultView -> NormalAccepted
normalReferenceAccepted accepted = case accepted of
  R.ReferenceOpenAcceptedView result -> NormalOpenAccepted result
  R.ReferenceReportAcceptedView probe _reporter -> NormalReportAccepted probe
  R.ReferenceInvalidationAcceptedView probe -> NormalInvalidateAccepted probe
  R.ReferenceResolutionAcceptedView outcome -> NormalResolveAccepted outcome
  R.ReferenceAbortAcceptedView probe -> NormalAbortAccepted probe

normalReferenceRejection :: R.ReferenceRejection -> NormalRejection
normalReferenceRejection rejection = case rejection of
  R.ReferenceRequestHomeEpochMismatch {} -> NormalHomeMismatch
  R.ReferenceInactiveHomeHerald {} -> NormalInactiveHome
  R.ReferenceStaleExpectedControlIndex {} -> NormalStaleIndex
  R.ReferenceSubjectMembershipCoordinateMismatch {} -> NormalCoordinateMismatch
  R.ReferenceControlledSubjectDeleted {} -> NormalControlledDeleted
  R.ReferenceControlledLabelRevisionMismatch {} -> NormalControlledRevision
  R.ReferenceControlledLabelWorkflowIncomplete {} -> NormalLabelInProgress
  R.ReferenceRegularOccurrenceMismatch {} -> NormalRegularOccurrence
  R.ReferenceUnknownProbe {} -> NormalUnknownProbe
  R.ReferenceProbeNotCollecting {} -> NormalTerminalProbe
  R.ReferenceReportHomeMismatch {} -> NormalReporterHome
  R.ReferenceEvidenceProbeMismatch {} -> NormalEvidenceProbe
  R.ReferenceEvidenceCoordinateMismatch {} -> NormalEvidenceCoordinate
  R.ReferenceReporterNotCaptured {} -> NormalReporterNotCaptured
  R.ReferenceConflictingReporterEvidence {} -> NormalConflictingReport
  R.ReferenceIncompleteEvidence {} -> NormalIncompleteEvidence
  R.ReferenceCompleteEvidenceDigestMismatch {} -> NormalCompleteEvidenceMismatch

normalReferenceEvent :: R.ReferenceProjectionEventView -> NormalEvent
normalReferenceEvent event = case event of
  R.ReferenceProbeOpenProjectedView header result ->
    NormalOpenProjected
      ( NormalProjectedProbeHeader
          (R.referenceProjectedProbeHeaderId header)
          (R.referenceProjectedProbeHeaderSubject header)
          (R.referenceProjectedProbeHeaderCoordinate header)
          (R.referenceProjectedProbeHeaderMembership header)
      )
      result
  R.ReferenceReportProjectedView claim ->
    NormalReportProjected claim
  R.ReferenceInvalidationProjectedView probe cause _index ->
    NormalInvalidatedProjected probe (normalReferenceInvalidation cause)
  R.ReferenceResolutionProjectedView probe outcome ->
    NormalResolvedProjected probe outcome
  R.ReferenceAbortProjectedView probe reason _index ->
    NormalAbortedProjected probe (normalReferenceAbort reason)
  R.ReferenceMembershipProjectedView membership ->
    NormalMembershipProjected membership
  R.ReferenceLabelOpenProjectedView decision object ->
    NormalLabelOpenedProjected decision object
  R.ReferenceLabelTerminalProjectedView decision object disposition ->
    case disposition of
      R.ReferenceLabelNotApplied ->
        NormalLabelNotAppliedProjected decision object
      R.ReferenceLabelReleased revision ->
        NormalLabelReleasedProjected decision object revision
      R.ReferenceLabelDeleted revision ->
        NormalLabelDeletedProjected decision object revision

normalLiveAccepted :: D.DisappearanceCommandResult -> NormalAccepted
normalLiveAccepted accepted = case accepted of
  D.DisappearanceOpenAccepted result -> NormalOpenAccepted result
  D.DisappearanceReportAccepted probe _reporter -> NormalReportAccepted probe
  D.DisappearanceInvalidateAccepted probe -> NormalInvalidateAccepted probe
  D.DisappearanceResolveAccepted outcome -> NormalResolveAccepted outcome
  D.DisappearanceAbortAccepted probe -> NormalAbortAccepted probe

normalLiveRejection :: D.DisappearanceRejection -> NormalRejection
normalLiveRejection rejection = case rejection of
  D.DisappearanceAdmissionPreparing {} -> NormalAdmissionBusy
  D.DisappearanceSubjectMembershipCoordinateMismatch {} -> NormalCoordinateMismatch
  D.DisappearanceControlledRevisionMismatch {} -> NormalControlledRevision
  D.DisappearanceControlledSubjectDeleted {} -> NormalControlledDeleted
  D.DisappearanceLabelWorkflowInProgress {} -> NormalLabelInProgress
  D.DisappearanceStaleRegularOccurrence {} -> NormalRegularOccurrence
  D.DisappearanceUnknownProbe {} -> NormalUnknownProbe
  D.DisappearanceProbeAlreadyTerminal {} -> NormalTerminalProbe
  D.DisappearanceReporterHomeMismatch {} -> NormalReporterHome
  D.DisappearanceReporterNotCaptured {} -> NormalReporterNotCaptured
  D.DisappearanceEvidenceProbeMismatch {} -> NormalEvidenceProbe
  D.DisappearanceEvidenceCoordinateMismatch {} -> NormalEvidenceCoordinate
  D.DisappearanceConflictingReport {} -> NormalConflictingReport
  D.DisappearanceIncompleteEvidence {} -> NormalIncompleteEvidence
  D.DisappearanceCompleteEvidenceMismatch {} -> NormalCompleteEvidenceMismatch

normalLiveAbort :: D.DisappearanceProbeAbortReasonView -> NormalAbort
normalLiveAbort reason = case reason of
  D.ExplicitDisappearanceAbortReasonView _ -> NormalAuthorizedAbort
  D.MembershipSupersededDisappearanceAbortView generation ->
    NormalMembershipSuperseded generation
  D.AdmissionPreparingDisappearanceAbortView admission -> NormalAdmissionPreparing admission

normalReferenceState ::
  [OracleClientRequestId] -> R.ReferenceState -> NormalState
normalReferenceState requests state =
  NormalState
    (R.referenceGreatestControlIndex state)
    (R.referenceCurrentMembership state)
    (R.referenceRequestCount state)
    (sortOn normalProbeOpenIndex (fmap normalReferenceProbe (R.referenceProbes state)))
    [ (sortId, R.referenceRegularRetirementIndex sortId state)
    | sortId <- observedSortIds
    ]
    [ (request, openResultProbe <$> R.referenceOpenRequestResult request state)
    | request <- sort requests
    ]

openResultProbe :: DisappearanceOpenResult -> DisappearanceProbeId
openResultProbe result = case disappearanceOpenResultView result of
  OpenedDisappearanceProbe probe -> probe
  AliasedDisappearanceProbe probe -> probe

claimFor ::
  DisappearanceSubject ->
  HeraldMembershipGeneration ->
  DisappearanceProbeId ->
  HeraldEpoch ->
  Word8 ->
  DisappearanceEvidenceClaim
claimFor subject membership probe reporter seed =
  checked
    "prospective disappearance evidence claim"
    ( admitDisappearanceEvidenceClaim
        subject
        membership
        probe
        reporter
        (evidenceDigest seed)
    )

evidenceDigest :: Word8 -> DisappearanceEvidenceDigest
evidenceDigest seed =
  deriveDisappearanceEvidenceDigest (ByteString.pack [seed, seed + 1, seed + 2])

assertPureEqual :: (Eq value, Show value) => String -> value -> value -> Either String ()
assertPureEqual context expected actual
  | expected == actual = Right ()
  | otherwise =
      Left
        ( context
            <> " mismatch\nexpected: "
            <> show expected
            <> "\n but got: "
            <> show actual
        )
h1, h2, h3, h4 :: HeraldEpoch
h1 = fixtureHeraldEpoch
h2 = fixtureRemoteHeraldEpoch
h3 = fixtureThirdHeraldEpoch
h4 = heraldEpoch 17

members :: [HeraldEpoch]
members = sort [h1, h2, h3, h4]

genesisMembership :: HeraldMembershipGeneration
genesisMembership =
  checked
    "Step-16 genesis membership"
    (genesisHeraldMembershipGeneration fixtureSystemId (h1 :| [h2, h3, h4]))

objectA, objectB :: GlobalObjectId
objectA = globalObjectId 201
objectB = globalObjectId 211

controlledA, controlledB :: DisappearanceSubject
controlledA = controlledSubject NeutralVertexRole objectA 1 Nothing
controlledB = controlledSubject EdgeRole objectB 2 Nothing

controlledSubject ::
  PredefinedSortRole -> GlobalObjectId -> Word64 -> Maybe LabelRevision -> DisappearanceSubject
controlledSubject role object sequenceNumber revision =
  checked
    "controlled disappearance subject"
    ( controlledPredefinedDisappearanceSubject
        role
        object
        ( structuralOccurrenceId
            h1
            (checked "positive structural sequence" (mkStructuralSequence sequenceNumber))
        )
        revision
    )

descriptorA, descriptorB :: CanonicalDescriptor
descriptorA = predefinedCatalogueDescriptor (profileEntryFor NeutralVertexRole)
descriptorB = predefinedCatalogueDescriptor (profileEntryFor EdgeRole)

regularA, regularB :: DisappearanceSubject
regularA = regularSubject descriptorA Genesis
regularB = regularSubject descriptorB Genesis

regularSubject :: CanonicalDescriptor -> SortOccurrenceBase -> DisappearanceSubject
regularSubject descriptor base =
  regularSortDefinitionDisappearanceSubject
    (deriveRegularSortOccurrenceClaim fixtureSystemId descriptor base)

observedSortIds :: [SortId]
observedSortIds = sort [descriptorSortId descriptorA, descriptorSortId descriptorB]

liveInitial :: L.OracleState
liveInitial = L.initialOracleState liveGenesis

liveGenesis :: CheckedOracleGenesis
liveGenesis =
  checked
    "live disappearance genesis"
    ( checkOracleGenesis
        ( oracleGenesis
            fixtureSystemId
            liveMembers
            profileCatalogueDigest
            fixtureDescriptors
            fixtureBootstraps
            fixtureConfigurationDigest
            topology
            (deriveInitialProjectionDigest fixtureBootstraps topology)
            fixtureBindings
            fixtureRaftConfiguration
            (deriveRaftConfigurationDigest fixtureSystemId fixtureBindings fixtureRaftConfiguration)
        )
    )
  where
    liveMembers = fixtureMembers <> [HeraldMember (heraldId 16) h4]
    topology = fixtureTopologyFor fixtureSystemId liveMembers fixtureBootstraps

normalLiveSubmission :: L.OracleSubmissionOutcome -> NormalSubmission
normalLiveSubmission outcome = case L.oracleSubmissionOutcomeView outcome of
  L.OracleSubmissionCommittedView receipt entry ->
    NormalCommitted
      (L.oracleReceiptControlIndex receipt)
      (normalLiveReceipt receipt)
      (fmap (normalLiveEvent . L.oracleProjectionEventView) (L.appliedEntryProjectionEvents entry))
  L.OracleSubmissionDuplicateView receipt -> NormalDuplicate (L.oracleReceiptControlIndex receipt) (normalLiveReceipt receipt)
  L.OracleSubmissionProtocolRejectedView {} -> NormalProtocolRejected
  L.OracleSubmissionRetiredView {} -> error "disappearance reference trace does not generate receipt retirement"
  L.OracleSubmissionProgressRetiredView {} -> error "disappearance reference trace does not generate receipt maintenance"
  L.OracleSubmissionDeferredView {} -> error "disappearance reference trace does not generate concurrent labels"

normalLiveReceipt :: L.OracleReceipt -> NormalReceiptResult
normalLiveReceipt receipt = case L.oracleReceiptResult receipt of
  L.OracleAccepted -> case L.oracleReceiptDisappearanceResult receipt of
    Just accepted -> NormalReceiptAccepted (normalLiveAccepted accepted)
    Nothing -> error "disappearance trace accepted an unrelated command"
  L.OracleRejected rejection -> NormalReceiptRejected $ case rejection of
    L.RequestHomeEpochMismatch {} -> NormalHomeMismatch
    L.InactiveHomeHerald {} -> NormalInactiveHome
    L.StaleExpectedControlIndex {} -> NormalStaleIndex
    L.DisappearanceCommandRejected problem -> normalLiveRejection problem
    other -> error ("unexpected Oracle rejection: " <> show other)

normalLiveEvent :: L.OracleProjectionEventView -> NormalEvent
normalLiveEvent event = case event of
  L.DisappearanceProbeOpenedView header result ->
    NormalOpenProjected
      ( NormalProjectedProbeHeader
          (D.projectedDisappearanceProbeHeaderId header)
          (D.projectedDisappearanceProbeHeaderSubject header)
          (D.projectedDisappearanceProbeHeaderCoordinate header)
          (D.projectedDisappearanceProbeHeaderMembership header)
      )
      result
  L.PredefinedAbsenceReportedView claim -> NormalReportProjected claim
  L.DisappearanceProbeInvalidatedView probe reason -> NormalInvalidatedProjected probe (normalLiveInvalidation reason)
  L.DisappearanceProbeResolvedView probe outcome -> NormalResolvedProjected probe outcome
  L.DisappearanceProbeAbortedView probe reason -> NormalAbortedProjected probe (normalLiveAbort reason)
  other -> error ("unexpected disappearance trace event: " <> show other)

normalLiveState :: [OracleClientRequestId] -> L.OracleState -> NormalState
normalLiveState requests state =
  NormalState
    (L.oracleGreatestControlIndex state)
    (L.oracleCurrentMembership state)
    (L.oracleRequestCount state)
    (sortOn normalProbeOpenIndex (fmap normalLiveProbe (L.oracleDisappearanceProbes state)))
    [(sortId, L.oracleRegularRetirementIndex sortId state) | sortId <- observedSortIds]
    [ ( request,
        do
          receipt <- L.oracleRequestReceipt request state
          result <- L.oracleReceiptDisappearanceResult receipt
          case result of { D.DisappearanceOpenAccepted opened -> Just (openResultProbe opened); _ -> Nothing }
      )
    | request <- sort requests
    ]

-- Every live transcript is admitted back through its checked current decoder.
-- Reference semantic observations also form an independent canonical normal form.
verifyLiveCanonical :: L.OracleEnvelope -> L.OracleState -> L.OracleSubmissionOutcome -> Either String ()
verifyLiveCanonical envelope successor outcome = do
  let bytes = L.oracleEnvelopeCanonicalBytes envelope
  decoded <- either (Left . show) Right (L.decodeOracleEnvelopeCanonicalBytes bytes)
  assertPureEqual "checked envelope bytes" bytes (L.decodedOracleEnvelopeCanonicalBytes decoded)
  case L.oracleSubmissionOutcomeView outcome of
    L.OracleSubmissionCommittedView receipt entry -> do
      let receiptBytes = L.oracleReceiptCanonicalBytes receipt
          eventBytes = L.oracleProjectionEventVectorCanonicalBytes (L.appliedEntryProjectionEvents entry)
          entryBytes = L.appliedOracleEntryCanonicalBytes entry
      decodedReceipt <- either (Left . show) Right (L.decodeOracleReceiptCanonicalBytes receiptBytes)
      assertPureEqual "checked receipt" receiptBytes (L.decodedOracleReceiptCanonicalBytes decodedReceipt)
      decodedEvents <- either (Left . show) Right (L.decodeOracleProjectionEventVectorCanonicalBytes eventBytes)
      assertPureEqual "checked events" eventBytes (L.decodedOracleProjectionEventVectorCanonicalBytes decodedEvents)
      decodedEntry <- either (Left . show) Right (L.decodeAppliedOracleEntryCanonicalBytes entryBytes)
      assertPureEqual "checked entry" entryBytes (L.decodedAppliedOracleEntryCanonicalBytes decodedEntry)
      assertPureEqual "entry post-state digest" (L.oracleStateDigest successor) (L.appliedEntryPostStateDigest entry)
    _ -> Right ()

submitPair :: OracleClientRequestId -> Maybe ControlIndex -> HeraldEpoch -> R.ReferenceCommand -> L.OracleCommand -> (R.ReferenceState, L.OracleState) -> Either String ((R.ReferenceState, L.OracleState), NormalSubmission)
submitPair request expected home referenceCommand command (reference, live) = do
  restored <- either (Left . ("checkpoint admission: " <>) . show) Right (Checkpoint.decodeOracleCheckpoint (Checkpoint.oracleStateGenesis live) (Checkpoint.oracleCheckpointBytes live))
  assertPureEqual "checkpoint restored state" live restored
  let referenceEnvelope = R.referenceEnvelope request expected home referenceCommand
      envelope = L.oracleEnvelope request expected home command
      (referenceNext, referenceOutcome) = R.submitReferenceEnvelope referenceEnvelope reference
      (liveNext, liveOutcome) = L.submitOracleState envelope live
      (replayedNext, replayedOutcome) = L.submitOracleState envelope restored
  assertPureEqual "command tag" (R.referenceCommandTag referenceCommand) (L.oracleCommandTag command)
  assertPureEqual "reference result/projection" (normalReferenceSubmission referenceOutcome) (normalLiveSubmission liveOutcome)
  assertPureEqual "reference successor facts" (normalReferenceState [request] referenceNext) (normalLiveState [request] liveNext)
  assertPureEqual "deterministic state bytes" (L.oracleStateCanonicalBytes liveNext) (L.oracleStateCanonicalBytes replayedNext)
  assertPureEqual "deterministic receipt/projection" liveOutcome replayedOutcome
  verifyLiveCanonical envelope liveNext liveOutcome
  assertPureEqual "checkpoint successor" (Right liveNext) (Checkpoint.decodeOracleCheckpoint (Checkpoint.oracleStateGenesis liveNext) (Checkpoint.oracleCheckpointBytes liveNext))
  pure ((referenceNext, liveNext), normalLiveSubmission liveOutcome)

initialPair :: (R.ReferenceState, L.OracleState)
initialPair = (R.initialReferenceState fixtureSystemId genesisMembership, liveInitial)
data TraceOperation
  = TraceOpen Int
  | TraceReport Int Word8
  | TraceInvalidate Int Word8
  | TraceResolve Int
  | TraceAbort Int
  deriving stock (Eq, Show)

data TraceAction = TraceAction Int Word64 TraceOperation
  deriving stock (Eq, Show)

propDifferentialTrace :: Property
propDifferentialTrace =
  forAll generatedTrace $ \actions ->
    case runDifferentialTrace actions of
      Left problem -> counterexample ("trace: " <> show actions <> "\n" <> problem) False
      Right () -> counterexample ("trace: " <> show actions) True

generatedTrace :: Gen [TraceAction]
generatedTrace = resize 30 (listOf1 generatedAction)

generatedAction :: Gen TraceAction
generatedAction = do
  subjectSlot <- chooseInt (0, length traceSubjects - 1)
  sequenceNumber <- fromIntegral <$> chooseInt (1, 14)
  operation <-
    frequency
      [ (4, TraceOpen <$> chooseInt (0, length members - 1)),
        (5, TraceReport <$> chooseInt (0, length members - 1) <*> arbitrarySeed),
        (2, TraceInvalidate <$> chooseInt (0, length members - 1) <*> arbitrarySeed),
        (2, TraceResolve <$> chooseInt (0, length members - 1)),
        (2, TraceAbort <$> chooseInt (0, length members - 1))
      ]
  pure (TraceAction subjectSlot sequenceNumber operation)
  where
    arbitrarySeed = fromIntegral <$> chooseInt (0, 7)

runDifferentialTrace :: [TraceAction] -> Either String ()
runDifferentialTrace actions = do
  _ <- foldM step initialPair actions
  pure ()
  where
    step pair@(reference, live) action = do
      let (request, home, referenceCommand) = translateReferenceAction action reference
          (liveRequest, liveHome, command) = translateLiveAction action live
      assertPureEqual "independently translated request" request liveRequest
      assertPureEqual "independently translated home" home liveHome
      fst <$> submitPair request Nothing home referenceCommand command pair

translateReferenceAction ::
  TraceAction -> R.ReferenceState -> (OracleClientRequestId, HeraldEpoch, R.ReferenceCommand)
translateReferenceAction (TraceAction subjectSlot sequenceNumber operation) state =
  let subject = traceSubject subjectSlot
      membership = R.referenceCurrentMembership state
      coordinate = disappearanceSubjectMembershipCoordinate subject membership
      active = R.referenceActiveProbe subject coordinate state
      probe = fromMaybe missingProbe active
      reports = referenceReports probe state
      (home, command) = case operation of
        TraceOpen actor ->
          ( traceMember actor,
            R.referenceOpenDisappearanceProbeCommand subject coordinate
          )
        TraceReport reporterSlot seed ->
          let reporter = traceMember reporterSlot
              claim = claimFor subject membership probe reporter seed
           in ( reporter,
                R.referenceReportPredefinedAbsenceCommand probe claim
              )
        TraceInvalidate reporterSlot seed ->
          let reporter = traceMember reporterSlot
           in ( reporter,
                R.referenceInvalidateDisappearanceProbeCommand
                  probe
                  reporter
                  R.ReferenceInvalidEvidence
                  (evidenceDigest seed)
              )
        TraceResolve actor ->
          ( traceMember actor,
            R.referenceResolveDisappearanceProbeCommand
              probe
              (R.deriveReferenceCompleteEvidenceDigest reports)
          )
        TraceAbort actor ->
          ( traceMember actor,
            R.referenceAbortDisappearanceProbeCommand
              probe
              R.referenceAuthorizedAbortReason
          )
   in (oracleClientRequestId home sequenceNumber, home, command)

translateLiveAction ::
  TraceAction -> L.OracleState -> (OracleClientRequestId, HeraldEpoch, L.OracleCommand)
translateLiveAction (TraceAction subjectSlot sequenceNumber operation) state =
  let subject = traceSubject subjectSlot
      membership = L.oracleCurrentMembership state
      coordinate = disappearanceSubjectMembershipCoordinate subject membership
      active = case [identifier | D.DisappearanceProbeView identifier _ captured _ _ _ _ D.DisappearanceCollectingView <- L.oracleDisappearanceProbes state, captured == coordinate] of first : _ -> Just first; [] -> Nothing
      probe = fromMaybe missingProbe active
      reports = liveReports probe state
      (home, command) = case operation of
        TraceOpen actor ->
          (traceMember actor, L.openDisappearanceProbeCommand subject coordinate)
        TraceReport reporterSlot seed ->
          let reporter = traceMember reporterSlot
              claim = claimFor subject membership probe reporter seed
           in (reporter, L.reportPredefinedAbsenceCommand probe claim)
        TraceInvalidate reporterSlot seed ->
          let reporter = traceMember reporterSlot
           in ( reporter,
                L.invalidateDisappearanceProbeCommand
                  probe
                  reporter
                  D.EvidenceContradicted
                  (evidenceDigest seed)
              )
        TraceResolve actor ->
          ( traceMember actor,
            L.resolveDisappearanceProbeCommand probe (D.completeDisappearanceEvidenceDigest reports)
          )
        TraceAbort actor ->
          ( traceMember actor,
            L.abortDisappearanceProbeCommand probe D.authorizedDisappearanceAbortReason
          )
   in (oracleClientRequestId home sequenceNumber, home, command)

liveReports :: DisappearanceProbeId -> L.OracleState -> [DisappearanceEvidenceClaim]
liveReports probe state = case L.oracleDisappearanceProbe probe state of
  Just (D.DisappearanceProbeView _ _ _ _ _ _ reports _) -> fmap snd reports
  Nothing -> []

referenceReports :: DisappearanceProbeId -> R.ReferenceState -> [DisappearanceEvidenceClaim]
referenceReports probe state = case R.referenceProbe probe state of
  Just (R.ReferenceProbeView _ _ _ _ _ reports _) -> fmap snd reports
  Nothing -> []

traceSubjects :: [DisappearanceSubject]
traceSubjects = [controlledA, controlledB, regularA, regularB]

traceSubject :: Int -> DisappearanceSubject
traceSubject slot = traceSubjects !! (slot `mod` length traceSubjects)

traceMember :: Int -> HeraldEpoch
traceMember slot = members !! (slot `mod` length members)

missingProbe :: DisappearanceProbeId
missingProbe = checked "generated missing probe" (deriveDisappearanceProbeId (controlIndex 1000))

tests :: TestTree
tests =
  testGroup
    "Step-16 live Oracle disappearance"
    [ localOption (QuickCheckTests 400) (testProperty "independently translated generated traces match reference receipts, probes, canonical normal forms, and retirement indexes" propDifferentialTrace),
      testCase "both subject arms resolve through every report permutation" caseReportPermutations,
      testCase "concurrent subjects each collect and alias racing Opens" caseConcurrent,
      testCase "terminal races, exact retry, and conflicting request reuse are retained" caseTerminalRaces,
      testCase "malformed evidence and envelope precedence match independent reference" caseAdmission,
      testCase "regular retirement admits only the Resolve-derived successor occurrence" caseRegularReopen,
      testCase "label Open invalidates in the same entry and blocks disappearance Open" caseLabelCoupling,
      testCase "concurrent label installation and disappearance match the independent object model" caseConcurrentLabelDisappearance,
      testCase "a wrong first label comparison cannot invalidate disappearance" caseWrongInitialLabelLeavesDisappearance,
      testCase "controlled Resolve rejects later label Open through the existing deleted path" caseDeletedLabel,
      testCase "membership retirement atomically aborts every collecting predecessor probe" caseMembership,
      testCase "repeated contractions seal old absence probes and require fresh survivor evidence" caseRepeatedMembership,
      testCase "accepted voter exclusion preserves probes until compound retirement and requires fresh survivor evidence" caseAcceptedVoterMembership,
      testCase "wire codecs reject truncation, trailing bytes, and inconsistent subject arms" caseMalformedCanonical,
      testCase "current disappearance command receipt event entry and state transcripts have fixed goldens" caseCurrentCanonicalGoldens
    ]

isAccepted :: NormalSubmission -> Bool
isAccepted (NormalCommitted _ (NormalReceiptAccepted _) _) = True
isAccepted (NormalDuplicate _ (NormalReceiptAccepted _)) = True
isAccepted _ = False

runPair :: OracleClientRequestId -> Maybe ControlIndex -> HeraldEpoch -> R.ReferenceCommand -> L.OracleCommand -> (R.ReferenceState, L.OracleState) -> IO ((R.ReferenceState, L.OracleState), NormalSubmission)
runPair request expected home reference command pair = either assertFailure pure (submitPair request expected home reference command pair)

openPair :: Word64 -> HeraldEpoch -> DisappearanceSubject -> (R.ReferenceState, L.OracleState) -> IO ((R.ReferenceState, L.OracleState), DisappearanceProbeId)
openPair sequenceNumber home subject pair@(_, live) = do
  let coordinate = disappearanceSubjectMembershipCoordinate subject (L.oracleCurrentMembership live)
  (next, result) <-
    runPair
      (oracleClientRequestId home sequenceNumber)
      Nothing
      home
      (R.referenceOpenDisappearanceProbeCommand subject coordinate)
      (L.openDisappearanceProbeCommand subject coordinate)
      pair
  probe <- case result of
    NormalCommitted _ (NormalReceiptAccepted (NormalOpenAccepted resultValue)) _ -> pure (openResultProbe resultValue)
    other -> assertFailure ("Open failed: " <> show other)
  pure (next, probe)

reportPair :: Word64 -> DisappearanceSubject -> DisappearanceProbeId -> HeraldEpoch -> Word8 -> (R.ReferenceState, L.OracleState) -> IO (R.ReferenceState, L.OracleState)
reportPair sequenceNumber subject probe reporter seed pair@(_, live) = do
  let claim = claimFor subject (L.oracleCurrentMembership live) probe reporter seed
  (next, result) <-
    runPair
      (oracleClientRequestId reporter sequenceNumber)
      Nothing
      reporter
      (R.referenceReportPredefinedAbsenceCommand probe claim)
      (L.reportPredefinedAbsenceCommand probe claim)
      pair
  assertBool "report admitted" (isAccepted result)
  pure next

resolvePair :: Word64 -> DisappearanceProbeId -> (R.ReferenceState, L.OracleState) -> IO ((R.ReferenceState, L.OracleState), NormalSubmission)
resolvePair sequenceNumber probe pair@(reference, live) =
  runPair
    (oracleClientRequestId h1 sequenceNumber)
    Nothing
    h1
    (R.referenceResolveDisappearanceProbeCommand probe (R.deriveReferenceCompleteEvidenceDigest (referenceReports probe reference)))
    (L.resolveDisappearanceProbeCommand probe (D.completeDisappearanceEvidenceDigest (liveReports probe live)))
    pair

completePair :: DisappearanceSubject -> [HeraldEpoch] -> IO ((R.ReferenceState, L.OracleState), DisappearanceProbeId)
completePair subject order = do
  (opened, probe) <- openPair 1 h1 subject initialPair
  (aliased, alias) <- openPair 1 h2 subject opened
  assertEqual "Open aliases original probe" probe alias
  ready <- foldM (\pair reporter -> reportPair 2 subject probe reporter 4 pair) aliased order
  pure (ready, probe)

caseReportPermutations :: Assertion
caseReportPermutations = forM_ traceSubjects $ \subject -> forM_ (permutations members) $ \order -> do
  (ready, probe) <- completePair subject order
  (resolved, result) <- resolvePair 3 probe ready
  assertBool "all-member Resolve accepted" (isAccepted result)
  (_, duplicate) <- resolvePair 3 probe resolved
  assertBool "exact Resolve retry accepted" (isAccepted duplicate)
  assertEqual
    "complete evidence canonical order"
    (D.completeDisappearanceEvidenceDigest (liveReports probe (snd ready)))
    (D.completeDisappearanceEvidenceDigest (reverse (liveReports probe (snd ready))))

caseConcurrent :: Assertion
caseConcurrent = do
  (pair, probes) <-
    foldM
      ( \(pair, probes) (sequenceNumber, subject) -> do
          (next, probe) <- openPair sequenceNumber h1 subject pair
          pure (next, probes <> [probe])
      )
      (initialPair, [])
      (zip [1 ..] traceSubjects)
  assertEqual "every unrelated subject collecting" 4 (length (L.oracleDisappearanceProbes (snd pair)))
  forM_ (zip traceSubjects probes) $ \(subject, probe) -> do
    (_, alias) <- openPair 20 h2 subject pair
    assertEqual "each alias names original" probe alias

caseTerminalRaces :: Assertion
caseTerminalRaces = forM_ traceSubjects $ \subject -> forM_ (permutations [0 :: Int, 1, 2]) $ \order -> do
  (ready, probe) <- completePair subject members
  _ <-
    foldM
      ( \pair (sequenceNumber, kind) -> do
          (next, result) <- case kind of
            0 -> resolvePair sequenceNumber probe pair
            1 ->
              runPair
                (oracleClientRequestId h1 sequenceNumber)
                Nothing
                h1
                (R.referenceInvalidateDisappearanceProbeCommand probe h1 R.ReferenceMatchingPublication (evidenceDigest 9))
                (L.invalidateDisappearanceProbeCommand probe h1 D.MatchingPublicationObserved (evidenceDigest 9))
                pair
            _ ->
              runPair
                (oracleClientRequestId h1 sequenceNumber)
                Nothing
                h1
                (R.referenceAbortDisappearanceProbeCommand probe R.referenceAuthorizedAbortReason)
                (L.abortDisappearanceProbeCommand probe D.authorizedDisappearanceAbortReason)
                pair
          assertEqual "only first terminal operation accepted" (sequenceNumber == 10) (isAccepted result)
          pure next
      )
      ready
      (zip [10 ..] order)
  let (reference, live) = ready
      claim = claimFor subject genesisMembership probe h1 7
  (_, conflict) <-
    runPair
      (oracleClientRequestId h1 2)
      Nothing
      h1
      (R.referenceReportPredefinedAbsenceCommand probe claim)
      (L.reportPredefinedAbsenceCommand probe claim)
      (reference, live)
  assertEqual "conflicting request ID is protocol rejection" NormalProtocolRejected conflict

caseAdmission :: Assertion
caseAdmission = do
  let coordinate = disappearanceSubjectMembershipCoordinate controlledA genesisMembership
      openR = R.referenceOpenDisappearanceProbeCommand controlledA coordinate
      openL = L.openDisappearanceProbeCommand controlledA coordinate
  forM_
    [ (oracleClientRequestId h1 1, Just (controlIndex 9), h2),
      (oracleClientRequestId (heraldEpoch 100) 1, Nothing, heraldEpoch 100),
      (oracleClientRequestId h1 1, Just (controlIndex 9), h1)
    ]
    $ \(request, expected, home) -> do
      _ <- runPair request expected home openR openL initialPair
      pure ()
  (opened, probe) <- openPair 1 h1 controlledA initialPair
  (_, incomplete) <- resolvePair 3 probe opened
  assertEqual
    "missing all reporters"
    (NormalReceiptRejected NormalIncompleteEvidence)
    (case incomplete of NormalCommitted _ result _ -> result; _ -> error "expected committed")
  let wrongProbe = missingProbe
      badClaim = claimFor controlledB genesisMembership wrongProbe h2 1
  _ <-
    runPair
      (oracleClientRequestId h1 4)
      Nothing
      h1
      (R.referenceReportPredefinedAbsenceCommand probe badClaim)
      (L.reportPredefinedAbsenceCommand probe badClaim)
      opened
  pure ()

caseRegularReopen :: Assertion
caseRegularReopen = do
  (ready, probe) <- completePair regularA members
  (retired, result) <- resolvePair 3 probe ready
  resolveIndex <- case result of NormalCommitted index _ _ -> pure index; _ -> assertFailure "missing Resolve"
  let successorSubject = regularSubject descriptorA (checked "retirement base" (resolvedRetirementOccurrenceBase resolveIndex))
      oldCoordinate = disappearanceSubjectMembershipCoordinate regularA genesisMembership
  (_, stale) <-
    runPair
      (oracleClientRequestId h1 5)
      Nothing
      h1
      (R.referenceOpenDisappearanceProbeCommand regularA oldCoordinate)
      (L.openDisappearanceProbeCommand regularA oldCoordinate)
      retired
  assertEqual
    "old occurrence rejected"
    (NormalReceiptRejected NormalRegularOccurrence)
    (case stale of NormalCommitted _ value _ -> value; _ -> error "expected receipt")
  _ <- openPair 6 h1 successorSubject retired
  pure ()

labelEnvelope :: Word64 -> L.OracleEnvelope
labelEnvelope sequenceNumber =
  L.oracleEnvelope
    request
    Nothing
    h1
    ( L.decideLabelCommand
        decision
        fixtureProcessEpoch
        objectA
        ((ProcessLabel fixtureProcessEpoch, 0))
        (Just (initialBootstrapLabelEvidence objectA fixtureProcessEpoch))
        Nothing
        Nothing
        targetVoid
        (homeLabelAcceptanceCut (firstLabelProcessAcceptancePosition fixtureProcessEpoch) EmptyHeraldPublicationPrefix)
    )
  where
    request = oracleClientRequestId h1 sequenceNumber
    decision = L.deriveLabelDecisionId fixtureSystemId request

commitLive :: L.OracleEnvelope -> L.OracleState -> IO (L.OracleState, L.OracleReceipt, [L.OracleProjectionEvent])
commitLive envelope state = do
  let (next, outcome) = L.submitOracleState envelope state
  either assertFailure pure (verifyLiveCanonical envelope next outcome)
  case L.oracleSubmissionOutcomeView outcome of
    L.OracleSubmissionCommittedView receipt entry -> pure (next, receipt, L.appliedEntryProjectionEvents entry)
    other -> assertFailure ("expected live commit: " <> show other)

caseLabelCoupling :: Assertion
caseLabelCoupling = do
  (pair, probe) <- openPair 1 h1 controlledA initialPair
  (labelled, receipt, events) <- commitLive (labelEnvelope 20) (snd pair)
  assertEqual "label accepted" L.OracleAccepted (L.oracleReceiptResult receipt)
  assertEqual "atomic decision first, invalidation second" [2, 19] (fmap L.oracleProjectionEventTag events)
  assertBool
    "invalidation cannot precede its atomic decision"
    (case L.decodeOracleProjectionEventVectorCanonicalBytes (L.oracleProjectionEventVectorCanonicalBytes (reverse events)) of Left _ -> True; Right _ -> False)
  case L.oracleDisappearanceProbe probe labelled of
    Just (D.DisappearanceProbeView _ _ _ _ _ _ _ (D.DisappearanceInvalidatedView D.LabelOpenDisappearanceInvalidation {} _)) -> pure ()
    other -> assertFailure ("missing label-coupled invalidation: " <> show other)
  (_, blocked, _) <-
    commitLive
      ( L.oracleEnvelope
          (oracleClientRequestId h2 21)
          Nothing
          h2
          (L.openDisappearanceProbeCommand controlledA (disappearanceSubjectMembershipCoordinate controlledA genesisMembership))
      )
      labelled
  case L.oracleReceiptResult blocked of
    L.OracleRejected (L.DisappearanceCommandRejected D.DisappearanceLabelWorkflowInProgress {}) -> pure ()
    other -> assertFailure ("expected label exclusion: " <> show other)

caseConcurrentLabelDisappearance :: Assertion
caseConcurrentLabelDisappearance = forM_ [[objectA, objectB], [objectB, objectA]] $ \completionOrder -> do
  (openedA, _) <- openPair 1 h1 controlledA initialPair
  (openedBoth, _) <- openPair 2 h1 controlledB openedA
  (first, decisionA) <- decide 10 objectA controlledA openedBoth
  (both, decisionB) <- decide 11 objectB controlledB first
  assertEqual "unrelated object labels coexist" 2 (length (L.oracleOpenDecisions (snd both)))
  blockedA <- open 20 (currentSubject objectA (fst both)) both
  blockedB <- open 21 (currentSubject objectB (fst blockedA)) blockedA
  assertEqual "both objects exclude disappearance while installation is pending" 2 (length (L.oracleOpenDecisions (snd blockedB)))
  let identities = [(objectA, decisionA), (objectB, decisionB)]
  _ <-
    foldM
      ( \pair (sequenceNumber, object) -> do
          let decision = fromMaybe (error "missing generated decision") (lookup object identities)
          completed <- complete sequenceNumber decision pair
          opened <- open (sequenceNumber + 1) (currentSubject object (fst completed)) completed
          let other = if object == objectA then objectB else objectA
          open (sequenceNumber + 2) (currentSubject other (fst opened)) opened
      )
      blockedB
      (zip [30, 40] completionOrder)
  pure ()
  where
    currentSubject object reference = controlledSubject (if object == objectA then NeutralVertexRole else EdgeRole) object (if object == objectA then 1 else 2) (R.referenceCurrentLabelRevision object reference)
    assertFacts (reference, live) = do
      assertEqual "canonical index" (R.referenceGreatestControlIndex reference) (L.oracleGreatestControlIndex live)
      assertEqual "independent disappearance probes" (sortOn normalProbeId (fmap normalReferenceProbe (R.referenceProbes reference))) (sortOn normalProbeId (fmap normalLiveProbe (L.oracleDisappearanceProbes live)))
      forM_ [objectA, objectB] $ \object -> do
        assertEqual "current label revision belongs to the decision" (R.referenceCurrentLabelRevision object reference) (labelRecordRevision <$> L.oracleLabelRecord object live)
        assertEqual "per-object pending installation" (R.referenceOpenLabelDecision object reference) (L.liveDecisionId <$> L.oracleObjectDecision object live)
      assertEqual "indexed checkpoint reconstructs every active fact" (Right live) (Checkpoint.decodeOracleCheckpoint liveGenesis (Checkpoint.oracleCheckpointBytes live))
    decide sequenceNumber object subject (reference, live) = do
      let request = oracleClientRequestId h1 sequenceNumber
          decision = L.deriveLabelDecisionId fixtureSystemId request
          revision = checked "decision revision" (mkLabelRevision (controlIndex (controlIndexWord64 (L.oracleGreatestControlIndex live) + 1)))
          (referenceNext, referenceOutcome, referenceEvents) = R.applyReferenceAtomicLabelDecision decision subject (R.ReferenceLabelReleased revision) reference
          command = L.decideLabelCommand decision fixtureProcessEpoch object (ProcessLabel fixtureProcessEpoch, 0) (Just (initialBootstrapLabelEvidence object fixtureProcessEpoch)) Nothing Nothing targetVoid (homeLabelAcceptanceCut (firstLabelProcessAcceptancePosition fixtureProcessEpoch) EmptyHeraldPublicationPrefix)
      (liveNext, receipt, events) <- commitLive (L.oracleEnvelope request Nothing h1 command) live
      assertEqual "independent successful decision" R.ReferenceContextApplied referenceOutcome
      assertEqual "live decision accepted" L.OracleAccepted (L.oracleReceiptResult receipt)
      assertEqual "both invalidate only this object's historical probe" (fmap (normalReferenceEvent . R.referenceProjectionEventView) (drop 1 referenceEvents)) (fmap (normalLiveEvent . L.oracleProjectionEventView) (drop 1 events))
      assertFacts (referenceNext, liveNext)
      pure ((referenceNext, liveNext), decision)
    complete sequenceNumber decision (reference, live) = do
      let terminal = fromMaybe (error "missing concurrent outcome") (L.oracleTerminalOutcome decision live)
          attestation = L.labelCompletionAttestation decision (L.liveDecisionRequestControlIndex (fromMaybe (error "missing concurrent decision") (L.oracleOpenDecision decision live))) (L.deriveLabelOutcomeDigest terminal) h1 (heraldMembershipGenerationId (L.oracleCurrentMembership live))
          (referenceNext, referenceOutcome) = R.applyReferenceLabelCompletion decision reference
      (liveNext, receipt, _) <- commitLive (L.oracleEnvelope (oracleClientRequestId h1 sequenceNumber) Nothing h1 (L.completeLabelDecisionCommand attestation)) live
      assertEqual "independent completion admitted" R.ReferenceContextApplied referenceOutcome
      assertEqual "live completion admitted" L.OracleAccepted (L.oracleReceiptResult receipt)
      assertFacts (referenceNext, liveNext)
      pure (referenceNext, liveNext)
    open sequenceNumber subject (reference, live) = do
      let request = oracleClientRequestId h1 sequenceNumber
          coordinate = disappearanceSubjectMembershipCoordinate subject (L.oracleCurrentMembership live)
          (referenceNext, referenceOutcome) = R.submitReferenceEnvelope (R.referenceEnvelope request Nothing h1 (R.referenceOpenDisappearanceProbeCommand subject coordinate)) reference
          (liveNext, liveOutcome) = L.submitOracleState (L.oracleEnvelope request Nothing h1 (L.openDisappearanceProbeCommand subject coordinate)) live
      assertEqual "independent Open outcome and projection" (normalReferenceSubmission referenceOutcome) (normalLiveSubmission liveOutcome)
      assertFacts (referenceNext, liveNext)
      pure (referenceNext, liveNext)

caseWrongInitialLabelLeavesDisappearance :: Assertion
caseWrongInitialLabelLeavesDisappearance = do
  (pair, probe) <- openPair 1 h1 controlledA initialPair
  let initial = snd pair
      request = oracleClientRequestId h1 20
      decision = L.deriveLabelDecisionId fixtureSystemId request
      wrongExpected =
        L.oracleEnvelope
          request
          Nothing
          h1
          ( L.decideLabelCommand
              decision
              fixtureProcessEpoch
              objectA
              (VoidLabel, 0)
              (Just (initialBootstrapLabelEvidence objectA fixtureProcessEpoch))
              Nothing
              Nothing
              targetVoid
              (homeLabelAcceptanceCut (firstLabelProcessAcceptancePosition fixtureProcessEpoch) EmptyHeraldPublicationPrefix)
          )
  (rejected, receipt, events) <- commitLive wrongExpected initial
  assertEqual "failed comparison is a canonical accepted NotApplied" L.OracleAccepted (L.oracleReceiptResult receipt)
  assertEqual "NotApplied cannot invalidate the probe" [2] (fmap L.oracleProjectionEventTag events)
  assertLabelNotApplied L.LivePriorLabelChanged events
  assertEqual "active disappearance evidence remains exact" (L.oracleDisappearanceProbe probe initial) (L.oracleDisappearanceProbe probe rejected)
  assertEqual "wrong first comparison creates no label workflow" Nothing (L.oracleWorkflowPhase decision rejected)

caseDeletedLabel :: Assertion
caseDeletedLabel = do
  (ready, probe) <- completePair controlledA members
  (resolved, _) <- resolvePair 3 probe ready
  (_, receipt, events) <- commitLive (labelEnvelope 20) (snd resolved)
  assertEqual "deleted-object decision is accepted NotApplied" L.OracleAccepted (L.oracleReceiptResult receipt)
  assertEqual "NotApplied emits only the decision" [2] (fmap L.oracleProjectionEventTag events)
  assertLabelNotApplied L.LiveTransitionNoLongerPermitted events

assertLabelNotApplied :: L.LiveNotAppliedReason -> [L.OracleProjectionEvent] -> Assertion
assertLabelNotApplied expected events = case [ reason
                                             | event <- events,
                                               L.LabelDecidedView _ terminal _ <- [L.oracleProjectionEventView event],
                                               L.LiveNotAppliedOutcomeView _ _ _ reason _ _ <- [L.liveTerminalOutcomeView terminal]
                                             ] of
  [actual] -> assertEqual "immutable NotApplied reason" expected actual
  other -> assertFailure ("expected one NotApplied terminal, got " <> show other)

caseMembership :: Assertion
caseMembership = do
  (pair, probeA) <- openPair 1 h1 controlledA initialPair
  (pair2, probeB) <- openPair 2 h1 regularA pair
  let opened = snd pair2
      submit sequenceNumber home command state = commitLive (L.oracleEnvelope (oracleClientRequestId home sequenceNumber) Nothing home command) state
  (failureOpened, openReceipt, _) <- submit 30 h1 (L.openHeraldFailureProbeCommand h4 (heraldMembershipGenerationId genesisMembership) (V.voterConfigurationId (V.oracleVoterConfiguration liveInitial))) opened
  failureProbe <- case L.oracleReceiptFailureResult openReceipt of
    Just (L.FailureProbeOpened probe) -> pure probe
    other -> assertFailure ("failure Open missing: " <> show other)
  (reported, _, _) <- submit 31 h1 (L.reportHeraldFailureProbeCommand failureProbe (V.voterConfigurationId (V.oracleVoterConfiguration liveInitial)) L.ProbeUnreachable) failureOpened
  (ready, readyReceipt, _) <- submit 32 h2 (L.reportHeraldFailureProbeCommand failureProbe (V.voterConfigurationId (V.oracleVoterConfiguration liveInitial)) L.ProbeUnreachable) reported
  resolution <- case L.oracleReceiptFailureResult readyReceipt of
    Just (L.FailureProbeReportRecorded _ (Just (L.FailureResolutionReady resolution _ _))) -> pure resolution
    other -> assertFailure ("majority missing: " <> show other)
  (retired, receipt, events) <- submit 33 h1 (L.retireHeraldEpochCommand resolution h4) ready
  assertEqual "retirement accepted" L.OracleAccepted (L.oracleReceiptResult receipt)
  let tags = fmap L.oracleProjectionEventTag events
      aborts = [(probe, generation) | event <- events, L.DisappearanceProbeAbortedView probe (D.MembershipSupersededDisappearanceAbortView generation) <- [L.oracleProjectionEventView event]]
  assertEqual "all captured probes abort in probe order" (sort [probeA, probeB]) (fmap fst aborts)
  assertBool "membership/failure prefix precedes aborts" (take 2 tags == [14, 15] && drop 2 tags == [21, 21])
  assertEqual "member set contracts" 3 (length (heraldMembershipGenerationActiveHeraldEpochs (L.oracleCurrentMembership retired)))
  let successorMembership = L.oracleCurrentMembership retired
  (reopened, openReceipt2, _) <- submit 40 h1 (L.openDisappearanceProbeCommand controlledA (disappearanceSubjectMembershipCoordinate controlledA successorMembership)) retired
  successorProbe <- case L.oracleReceiptDisappearanceResult openReceipt2 of
    Just (D.DisappearanceOpenAccepted result) -> pure (openResultProbe result)
    other -> assertFailure ("successor Open missing: " <> show other)
  (repeated, repeatReceipt, repeatEvents) <- submit 41 h1 (L.retireHeraldEpochCommand resolution h4) reopened
  assertEqual "fresh equal Retire still accepted" L.OracleAccepted (L.oracleReceiptResult repeatReceipt)
  assertEqual "fresh equal Retire has no new consequences" [] repeatEvents
  assertEqual
    "fresh equal Retire preserves successor collecting probe"
    (L.oracleDisappearanceProbe successorProbe reopened)
    (L.oracleDisappearanceProbe successorProbe repeated)
  assertBool
    "reversed compound membership order rejects"
    (case L.decodeOracleProjectionEventVectorCanonicalBytes (L.oracleProjectionEventVectorCanonicalBytes (reverse events)) of Left _ -> True; Right _ -> False)

caseRepeatedMembership :: Assertion
caseRepeatedMembership = forM_ (permutations [FailureFixture.targetH4, FailureFixture.targetH5]) $ \order -> do
  let initial = L.initialOracleState FailureFixture.step15Genesis
  (opened, originalProbes) <- openSubjects initial
  (contracted, allOldProbes) <- foldM retireAndReopen (opened, originalProbes) order
  let active = heraldMembershipGenerationActiveHeraldEpochs (L.oracleCurrentMembership contracted)
  assertEqual "both contractions retain exactly the original voter hosts" (sort [h1, h2, h3]) (sort (foldr (:) [] active))
  final <- foldM completeCurrent contracted traceSubjects
  forM_ [(probe, captured, terminal) | (probe, captured, terminal) <- allOldProbes, terminal /= D.DisappearanceCollectingView] $ \(probe, captured, terminal) -> do
    assertEqual "old captured report sets and terminal outcomes remain immutable" (Just (captured, terminal)) (probeSnapshot probe final)
  where
    submit home command state = do
      let sequenceNumber = 50_000 + controlIndexWord64 (L.oracleGreatestControlIndex state)
      commitLive (L.oracleEnvelope (oracleClientRequestId home sequenceNumber) Nothing home command) state
    openSubject state subject = do
      let membership = L.oracleCurrentMembership state
          coordinate = disappearanceSubjectMembershipCoordinate subject membership
      (opened, receipt, _) <- submit h1 (L.openDisappearanceProbeCommand subject coordinate) state
      probe <- case L.oracleReceiptDisappearanceResult receipt of
        Just (D.DisappearanceOpenAccepted result) -> pure (openResultProbe result)
        other -> assertFailure ("fresh absence Open missing: " <> show other)
      pure (opened, probe)
    openSubjects state = do
      (opened, identifiers) <-
        foldM
          ( \(current, acc) subject -> do
              (next, probe) <- openSubject current subject
              pure (next, acc <> [probe])
          )
          (state, [])
          traceSubjects
      pure (opened, [(probe, captured, terminal) | probe <- identifiers, Just (captured, terminal) <- [probeSnapshot probe opened]])
    retireAndReopen (state, oldProbes) target = do
      let membership = L.oracleCurrentMembership state
          collecting = [(probe, subject, coordinate, captured) | D.DisappearanceProbeView probe subject coordinate _ captured _ _ D.DisappearanceCollectingView <- L.oracleDisappearanceProbes state]
          retired = FailureFixture.retireLiveTarget target state
          successor = heraldMembershipGenerationId (L.oracleCurrentMembership retired)
      sealed <- forM collecting $ \(probe, subject, coordinate, captured) -> do
        snapshot <- case probeSnapshot probe retired of
          Just (retained, terminal@(D.DisappearanceMembershipSupersededView generation _)) -> do
            assertEqual "retirement retains the exact captured member set" captured retained
            assertEqual "terminal records the exact successor generation" successor generation
            pure (probe, retained, terminal)
          other -> assertFailure ("old absence probe did not become terminal: " <> show other)
        let claim = claimFor subject membership probe h1 9
        (_, staleReceipt, staleEvents) <- submit h1 (L.reportPredefinedAbsenceCommand probe claim) retired
        assertBool "late predecessor evidence rejects" (L.oracleReceiptResult staleReceipt /= L.OracleAccepted)
        assertEqual "late evidence cannot project new facts" [] staleEvents
        (_, oldOpen, _) <- submit h1 (L.openDisappearanceProbeCommand subject coordinate) retired
        assertBool "old membership coordinates cannot reopen" (L.oracleReceiptResult oldOpen /= L.OracleAccepted)
        pure snapshot
      (reopened, fresh) <- openSubjects retired
      assertBool "all fresh probes use new identities" (all (\(probe, _, _) -> probe `notElem` [old | (old, _, _) <- oldProbes]) fresh)
      let previouslySealed = [(probe, captured, terminal) | (probe, captured, terminal) <- oldProbes, terminal /= D.DisappearanceCollectingView]
      pure (reopened, previouslySealed <> sealed <> fresh)
    completeCurrent state subject = do
      let membership = L.oracleCurrentMembership state
          captured = foldr (:) [] (heraldMembershipGenerationActiveHeraldEpochs membership)
      (opened, probe) <- openSubject state subject
      reported <-
        foldM
          ( \current reporter -> do
              let claim = claimFor subject membership probe reporter 7
              (next, receipt, _) <- submit reporter (L.reportPredefinedAbsenceCommand probe claim) current
              assertEqual "only current survivor reports are required" L.OracleAccepted (L.oracleReceiptResult receipt)
              pure next
          )
          opened
          captured
      (resolved, receipt, _) <- submit h1 (L.resolveDisappearanceProbeCommand probe (D.completeDisappearanceEvidenceDigest (liveReports probe reported))) reported
      assertEqual "the current captured set can resolve" L.OracleAccepted (L.oracleReceiptResult receipt)
      pure resolved
    probeSnapshot probe state = case L.oracleDisappearanceProbe probe state of
      Just (D.DisappearanceProbeView _ _ _ _ captured _ _ terminal) -> Just (captured, terminal)
      Nothing -> Nothing

caseAcceptedVoterMembership :: Assertion
caseAcceptedVoterMembership = do
  (pair, probeA) <- openPair 1 h1 controlledA initialPair
  (pair2, probeB) <- openPair 2 h1 regularA pair
  let opened = snd pair2
      (excluded, certificate, _) = VF.excludeVoterHost h3 opened
      (retired, retirementEntry) = VF.retireExcludedHost certificate excluded
      retirementEvents = L.appliedEntryProjectionEvents retirementEntry
      aborts = [probe | event <- retirementEvents, L.DisappearanceProbeAbortedView probe (D.MembershipSupersededDisappearanceAbortView _) <- [L.oracleProjectionEventView event]]
      completedChanges = [V.voterChangePhase change | event <- retirementEvents, L.OracleVoterChangeChangedView change <- [L.oracleProjectionEventView event]]
  forM_ [probeA, probeB] $ \probe ->
    assertEqual "native exclusion alone preserves the exact collecting probe and captured sources" (L.oracleDisappearanceProbe probe opened) (L.oracleDisappearanceProbe probe excluded)
  assertEqual "certified retirement aborts all predecessor probes in canonical order" (sort [probeA, probeB]) aborts
  assertEqual "semantic retirement completes the accepted native change in the same entry" [V.VoterChangeCompleted] completedChanges
  let retirementBytes = L.appliedOracleEntryCanonicalBytes retirementEntry
  assertEqual "compound automatic-retirement entry remains canonical" (Right retirementBytes) (L.decodedAppliedOracleEntryCanonicalBytes <$> L.decodeAppliedOracleEntryCanonicalBytes retirementBytes)
  forM_ [(probeA, controlledA), (probeB, regularA)] $ \(probe, subject) -> do
    (_, staleReceipt, staleEvents) <- submit h1 (L.reportPredefinedAbsenceCommand probe (claimFor subject genesisMembership probe h1 9)) retired
    assertBool "old captured source evidence cannot certify absence after retirement" (L.oracleReceiptResult staleReceipt /= L.OracleAccepted)
    assertEqual "late predecessor evidence has no projected effect" [] staleEvents
  (reopened, freshProbes) <- openSubjects retired
  let contracted = FailureFixture.retireLiveTarget h4 reopened
  forM_ (freshProbes <> [probeA, probeB]) $ \probe -> case L.oracleDisappearanceProbe probe contracted of
    Just (D.DisappearanceProbeView _ _ _ _ _ _ _ D.DisappearanceMembershipSupersededView {}) -> pure ()
    other -> assertFailure ("repeated compound contraction left a live predecessor probe: " <> show other)
  (fresh, _) <- openSubjects contracted
  (replayed, replayReceipt, replayEvents) <- submit h1 (L.retireHeraldEpochCommand (F.acceptedVoterHostFailureResolution certificate) h3) fresh
  assertEqual "old accepted voter retirement is exactly replayable during new probes" L.OracleAccepted (L.oracleReceiptResult replayReceipt)
  assertEqual "replay cannot abort successor source evidence" [] replayEvents
  assertEqual "replay preserves complete successor probe state" (L.oracleDisappearanceProbes fresh) (L.oracleDisappearanceProbes replayed)
  _ <- foldM completeSubject replayed [controlledA, regularA]
  pure ()
  where
    submit home command state = commitLive (L.oracleEnvelope (oracleClientRequestId home (70_000 + controlIndexWord64 (L.oracleGreatestControlIndex state))) Nothing home command) state
    openSubject state subject = do
      let membership = L.oracleCurrentMembership state
      (opened, receipt, _) <- submit h1 (L.openDisappearanceProbeCommand subject (disappearanceSubjectMembershipCoordinate subject membership)) state
      probe <- case L.oracleReceiptDisappearanceResult receipt of
        Just (D.DisappearanceOpenAccepted result) -> pure (openResultProbe result)
        other -> assertFailure ("successor Open missing: " <> show other)
      pure (opened, probe)
    openSubjects state = foldM (\(current, probes) subject -> do (next, probe) <- openSubject current subject; pure (next, probes <> [probe])) (state, []) [controlledA, regularA]
    completeSubject state subject = do
      (opened, probe) <- openSubject state subject
      let membership = L.oracleCurrentMembership opened
      assertEqual "the final proof denominator contains only actual semantic survivors" (sort [h1, h2]) (sort (foldr (:) [] (heraldMembershipGenerationActiveHeraldEpochs membership)))
      reported <-
        foldM
          ( \current reporter -> do
              (next, receipt, _) <- submit reporter (L.reportPredefinedAbsenceCommand probe (claimFor subject membership probe reporter 7)) current
              assertEqual "fresh survivor evidence is admitted" L.OracleAccepted (L.oracleReceiptResult receipt)
              pure next
          )
          opened
          [h1, h2]
      (resolved, receipt, _) <- submit h1 (L.resolveDisappearanceProbeCommand probe (D.completeDisappearanceEvidenceDigest (liveReports probe reported))) reported
      assertEqual "fresh evidence resolves after both contractions" L.OracleAccepted (L.oracleReceiptResult receipt)
      pure resolved

caseMalformedCanonical :: Assertion
caseMalformedCanonical = forM_ traceSubjects $ \subject -> do
  let probe = checked "codec probe" (deriveDisappearanceProbeId (controlIndex 1))
      claim = claimFor subject genesisMembership probe h1 3
      commands =
        [ L.openDisappearanceProbeCommand subject (disappearanceSubjectMembershipCoordinate subject genesisMembership),
          L.reportPredefinedAbsenceCommand probe claim,
          L.invalidateDisappearanceProbeCommand probe h1 D.EvidenceContradicted (evidenceDigest 4),
          L.resolveDisappearanceProbeCommand probe (D.completeDisappearanceEvidenceDigest [claim]),
          L.abortDisappearanceProbeCommand probe D.authorizedDisappearanceAbortReason
        ]
  forM_ commands $ \command -> do
    let bytes = L.oracleEnvelopeCanonicalBytes (L.oracleEnvelope (oracleClientRequestId h1 1) Nothing h1 command)
    assertBool "truncated current payload rejects" (isLeft (L.decodeOracleEnvelopeCanonicalBytes (ByteString.init bytes)))
    assertBool "trailing bytes reject" (isLeft (L.decodeOracleEnvelopeCanonicalBytes (bytes <> "x")))
  let subjectBytes = disappearanceSubjectCanonicalBytes subject
  assertEqual "subject checked codec" (Right subject) (decodeDisappearanceSubjectCanonicalBytes subjectBytes)
  assertBool "subject trailing bytes reject" (isLeft (decodeDisappearanceSubjectCanonicalBytes (subjectBytes <> "x")))
  let outcome = checked "codec outcome" (resolveDisappearanceSubject fixtureSystemId (controlIndex 2) subject)
      outcomeBytes = disappearanceResolutionOutcomeCanonicalBytes outcome
      domainWidth = ByteString.foldl' (\width byte -> width * 256 + fromIntegral byte) 0 (ByteString.take 8 outcomeBytes)
      tagOffset = 8 + domainWidth
      mismatchedTag = case disappearanceSubjectView subject of ControlledPredefinedSubjectView {} -> 1; RegularSortDefinitionSubjectView {} -> 0
      mismatchedOutcome = ByteString.take tagOffset outcomeBytes <> ByteString.singleton mismatchedTag <> ByteString.drop (tagOffset + 1) outcomeBytes
  assertEqual "outcome checked codec" (Right outcome) (decodeDisappearanceResolutionOutcomeCanonicalBytes outcomeBytes)
  assertBool "outcome arm must match its complete subject" (isLeft (decodeDisappearanceResolutionOutcomeCanonicalBytes mismatchedOutcome))
  assertBool "probe Open index must authenticate its claim" (isLeft (decodeDisappearanceEvidenceClaimCanonicalBytes (controlIndex 2) (disappearanceEvidenceClaimCanonicalBytes claim)))
  where
    isLeft (Left _) = True
    isLeft _ = False

-- Each fixed digest covers unambiguously concatenated 32-byte fragment hashes:
-- envelope, receipt, projection vector, applied entry, and complete sole state
-- at every step of Open, racing alias, all Reports, and one terminal. Both subject
-- arms run Resolve, Invalidate, and Abort, so all five current command/result and
-- projection payloads participate, including retained alias evidence.
caseCurrentCanonicalGoldens :: Assertion
caseCurrentCanonicalGoldens = do
  let golden subject terminal = do
        let probe = checked "golden probe" (deriveDisappearanceProbeId (controlIndex 1))
            coordinate = disappearanceSubjectMembershipCoordinate subject genesisMembership
            claims = [claimFor subject genesisMembership probe reporter 4 | reporter <- members]
            terminalCommand = case terminal of
              0 -> L.resolveDisappearanceProbeCommand probe (D.completeDisappearanceEvidenceDigest claims)
              1 -> L.invalidateDisappearanceProbeCommand probe h1 D.EvidenceContradicted (evidenceDigest 7)
              _ -> L.abortDisappearanceProbeCommand probe D.authorizedDisappearanceAbortReason
            commands =
              [ (h1, L.openDisappearanceProbeCommand subject coordinate),
                (h2, L.openDisappearanceProbeCommand subject coordinate)
              ]
                <> [(reporter, L.reportPredefinedAbsenceCommand probe claim) | (reporter, claim) <- zip members claims]
                <> [(h1, terminalCommand)]
        (_, fragments) <- foldM capture (snd initialPair, []) (zip [1 ..] commands)
        pure
          ( concatMap
              (\byte -> let digits = showHex byte "" in if length digits == 1 then '0' : digits else digits)
              (ByteString.unpack (SHA256.hash (ByteString.concat (fmap SHA256.hash fragments))))
          )
      capture (state, fragments) (number, (home, command)) = do
        let envelope = L.oracleEnvelope (oracleClientRequestId home number) Nothing home command
            (successor, outcome) = L.submitOracleState envelope state
        case L.oracleSubmissionOutcomeView outcome of
          L.OracleSubmissionCommittedView receipt entry -> do
            assertEqual "golden command accepted" L.OracleAccepted (L.oracleReceiptResult receipt)
            pure
              ( successor,
                fragments
                  <> [ L.oracleEnvelopeCanonicalBytes envelope,
                       L.oracleReceiptCanonicalBytes receipt,
                       L.oracleProjectionEventVectorCanonicalBytes (L.appliedEntryProjectionEvents entry),
                       L.appliedOracleEntryCanonicalBytes entry,
                       L.oracleStateCanonicalBytes successor
                     ]
              )
          other -> assertFailure ("golden requires committed live entry: " <> show other)
  actual <- sequence [golden subject terminal | subject <- [controlledA, regularA], terminal <- [0 :: Int, 1, 2]]
  assertEqual
    "six complete current disappearance transcript goldens"
    -- Each transcript contains the state stream with L/R and active-home
    -- progress promises; compact completion witnesses carry original indices.
    [ "7641c6ac9278091397b85c103fc2cbaa849be0d0a82a63c11bfb58ffd2faa67a",
      "5820a4caa7e2356868c4baf8c4aa95eb62b9885fb4e3d0c8d48201768445c9fc",
      "9385dbb8973bb3cc8b88d71555b539043da77e19dd0a33a22bbf94dca34753a2",
      "351ed0f68854de302135ae5de90b8fe952f035a49476be1414fc4df82ef555d3",
      "7092c396d7ed7aaf4d114fd2e92ffc722faf58d0e60f2aafe281b94f91710cf8",
      "0ee372e48844a18f392aab19ef76a9a847ff280a49c18a7210623177b7933d85"
    ]
    actual
