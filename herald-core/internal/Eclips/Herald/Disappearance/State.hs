{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Detached pure owner for Step-16 disappearance candidates and local cut
-- evidence.
--
-- The owner is deliberately not yet embedded in the live 'HeraldState'.  It
-- proves the private state transitions needed by the later atomic Oracle/EPRP
-- cutover without adding a dormant wire constructor or a second runtime path.
-- Its inputs are narrow immutable observations; Store, Publication, PeerStream,
-- Alignment, Controlled, and Graph remain the owners of the underlying facts.
module Eclips.Herald.Disappearance.State
  ( State,
    initialState,
    takeMarkerProbeChanges,
    clearMarkerProbeChanges,
    disappearanceLocalHerald,

    -- * Candidate and probe observations
    CandidateDisposition (..),
    CandidateWitness (..),
    candidateWitnesses,
    MatchingWriteGate,
    matchingWriteGateProbe,
    matchingWriteGateSubject,
    matchingWriteGateCoordinate,
    matchingWriteGateLocalCut,
    matchingWriteGateForProbe,
    ProbePhaseView (..),
    MarkerCellView (..),
    AlignmentCellView (..),
    ProbeWitness (..),
    probeWitness,
    probeWitnesses,
    openIntentions,
    reportIntentions,
    invalidationIntentions,
    resolveIntentions,
    projectedLabelTerminals,
    pendingAlignmentMarkers,
    pendingPublicationMarkers,

    -- * Work accounting
    DisappearanceWorkLedger (..),
    disappearanceWorkLedger,
    disappearanceLogicalWork,
    DisappearanceWorkDimensions,
    disappearanceWorkDimensions,
    disappearanceDimensionMembers,
    disappearanceDimensionPublicationMarkers,
    disappearanceDimensionAlignmentMarkers,
    disappearanceDimensionMatchingWork,
    disappearanceSuccessfulWorkBound,

    -- * Prepared transitions
    PreparedDisappearanceTransition,
    preparedDisappearanceOutput,
    commitDisappearanceTransition,
    DisappearanceProblem (..),
    prepareLocalTakeCandidate,
    prepareCandidateReevaluation,
    prepareCandidateLabelWait,
    ProjectedOpenDisposition (..),
    ProjectedOpenOutput (..),
    prepareProjectedOpen,
    preparePublicationMarkerAssignments,
    MarkerAdmissionDisposition (..),
    preparePublicationMarkerReceipt,
    preparePublicationMarkerCompletion,
    prepareLocalPublicationCutCompletion,
    prepareAlignmentMarkerReceipt,
    prepareAlignmentMarkerCompletion,
    AlignmentMutation (..),
    prepareAlignmentMutation,
    prepareEvidenceObservation,
    prepareBlockerObservation,
    prepareMatchingPublicationObservation,
    prepareGatedPublicationInvalidation,
    ReportDisposition (..),
    prepareLocalAbsenceReport,
    prepareLocalAbsenceReportFromEvidence,
    ProjectedReportDisposition (..),
    prepareProjectedReport,
    TerminalDisposition (..),
    prepareProjectedTerminal,
    ControlledRemovalAuthority,
    controlledRemovalAuthorityProbe,
    controlledRemovalAuthoritySubject,
    controlledRemovalAuthorityCoordinate,
    controlledRemovalAuthorityObject,
    controlledRemovalAuthorityStructuralOccurrence,
    controlledRemovalAuthorityExpectedLabelRevision,
    controlledRemovalAuthorityResolveIndex,
    ProjectedControlledResolutionDisposition (..),
    PreparedProjectedControlledResolution,
    prepareProjectedControlledResolution,
    preparedProjectedControlledResolutionDisposition,
    preparedProjectedControlledResolutionAuthority,
    commitProjectedControlledResolution,
    RegularRetirementAuthority,
    regularRetirementAuthorityProbe,
    regularRetirementAuthoritySubject,
    regularRetirementAuthorityCoordinate,
    regularRetirementAuthoritySortId,
    regularRetirementAuthorityDescriptorDigest,
    regularRetirementAuthorityOccurrence,
    regularRetirementAuthorityResolveIndex,
    regularRetirementAuthoritySuccessorOccurrence,
    regularRetirementAuthorityLocalPublicationCut,
    regularRetirementAuthorityEvidenceSnapshot,
    ProjectedRegularResolutionDisposition (..),
    PreparedProjectedRegularResolution,
    prepareProjectedRegularResolution,
    preparedProjectedRegularResolutionDisposition,
    preparedProjectedRegularResolutionAuthority,
    commitProjectedRegularResolution,
    LabelTerminalDisposition (..),
    prepareProjectedLabelTerminal,

    -- * Invariants
    DisappearanceInvariantViolation (..),
    validateState,
  )
where

import Control.Monad (foldM, unless, when)
import Data.ByteString qualified as ByteString
import Data.List (nub)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment
  ( HeraldPublicationPosition,
    HeraldPublicationPrefix (..),
    StoreRevision,
  )
import Eclips.Domain.Disappearance
  ( CanonicalDescriptorDigest,
    DisappearanceEvidenceClaim,
    DisappearanceEvidenceDigest,
    DisappearanceProbeId,
    DisappearanceResolutionOutcome,
    DisappearanceResolutionOutcomeView (..),
    DisappearanceSubject,
    DisappearanceSubjectMembershipCoordinate,
    DisappearanceSubjectView (..),
    deriveDisappearanceEvidenceDigest,
    disappearanceEvidenceClaimCoordinate,
    disappearanceEvidenceClaimProbeId,
    disappearanceEvidenceClaimReporter,
    disappearanceProbeOpenControlIndex,
    disappearanceResolutionControlIndex,
    disappearanceResolutionOutcomeView,
    disappearanceResolutionSubject,
    disappearanceSubjectCanonicalBytes,
    disappearanceSubjectMembershipCoordinate,
    disappearanceSubjectView,
    reviseControlledDisappearanceSubject,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    SortDefinitionOccurrenceId,
    SortId,
    StructuralOccurrenceId,
  )
import Eclips.Domain.Label (LabelRevision)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldAdmissionControlIndex,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationId,
    heraldMembershipGenerationPredecessor,
    heraldMembershipGenerationRetiredHeraldEpoch,
    heraldMembershipGenerationRetirementControlIndex,
    heraldMembershipGenerationRetirementId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Herald.Alignment.Protocol
  ( AlignmentSubscriptionId,
    alignmentSubscriptionIdDestinationHerald,
  )
import Eclips.Herald.Disappearance.Evidence.Internal
  ( DisappearanceEvidenceSnapshot,
    disappearanceEvidenceSnapshotAbsenceAttestation,
    disappearanceEvidenceSnapshotBlockerCount,
    disappearanceEvidenceSnapshotBlockers,
    disappearanceEvidenceSnapshotIncomingAlignmentCuts,
    disappearanceEvidenceSnapshotLocalPublicationCut,
    disappearanceEvidenceSnapshotMatchingPublications,
    disappearanceEvidenceSnapshotOutgoingAlignmentCuts,
    disappearanceEvidenceSnapshotSubject,
  )
import Eclips.Herald.Disappearance.Protocol
  ( CompletedIncomingAlignmentEvidence,
    DisappearanceAbsenceAttestation,
    DisappearanceAlignmentMarker,
    DisappearanceBlockerClass (..),
    DisappearanceBlockerWitness,
    DisappearanceCandidateObservation,
    DisappearanceInvalidationIntention,
    DisappearanceInvalidationReason (..),
    DisappearanceOpenContext,
    DisappearanceOpenIntention,
    DisappearanceProtocolProblem,
    DisappearancePublicationMarker,
    DisappearanceResolveIntention,
    IncomingAlignmentCut,
    LocalAbsenceReport,
    MatchingPublicationObservation,
    OutgoingAlignmentCut,
    ProjectedAbortCause (..),
    ProjectedDisappearanceProbe,
    ProjectedDisappearanceTerminal (..),
    ProjectedInvalidationCause (..),
    ProjectedLabelTerminal,
    ProjectedLabelTerminalOutcome (..),
    completedIncomingAlignmentEvidenceForOwner,
    completedIncomingAlignmentEvidenceProbe,
    completedIncomingAlignmentEvidenceSource,
    completedIncomingAlignmentEvidenceSubscription,
    completedIncomingAlignmentEvidenceThroughRevision,
    disappearanceAbsenceAttestationSubject,
    disappearanceAlignmentMarker,
    disappearanceAlignmentMarkerProbe,
    disappearanceAlignmentMarkerSubscription,
    disappearanceAlignmentMarkerThroughRevision,
    disappearanceBlockerClass,
    disappearanceBlockerDigest,
    disappearanceCandidateSubject,
    disappearanceInvalidationIntention,
    disappearanceInvalidationProbe,
    disappearanceInvalidationReporter,
    disappearanceOpenContextMembership,
    disappearanceOpenIntention,
    disappearanceOpenIntentionSubject,
    disappearancePublicationMarkerCoordinate,
    disappearancePublicationMarkerDirection,
    disappearancePublicationMarkerItem,
    disappearancePublicationMarkerProbe,
    disappearanceResolveClaims,
    disappearanceResolveIntention,
    disappearanceResolveProbe,
    incomingAlignmentCutCanonicalBytes,
    incomingAlignmentCutSource,
    incomingAlignmentCutSubscription,
    localAbsenceReportAlignmentEvidence,
    localAbsenceReportAttestation,
    localAbsenceReportClaim,
    localAbsenceReportForOwner,
    localAbsenceReportProbe,
    matchingPublicationPosition,
    matchingPublicationSubject,
    matchingPublicationWitness,
    outgoingAlignmentCutCanonicalBytes,
    outgoingAlignmentCutSubscription,
    projectedLabelTerminalDecision,
    projectedLabelTerminalObject,
    projectedLabelTerminalOutcome,
    projectedProbeCoordinate,
    projectedProbeId,
    projectedProbeMembers,
    projectedProbeMembership,
    projectedProbeSubject,
  )
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
    preparedOutput,
  )
import Eclips.Herald.PeerStream
  ( StreamDirection,
    streamDirectionDestination,
    streamDirectionSource,
  )
import Eclips.Herald.PeerStream.State (SequenceBoundPeerItem)

data Candidate = Candidate
  { subject :: DisappearanceSubject,
    eligible :: Bool,
    awaitingLabelDecision :: Maybe LabelDecisionId,
    openIntention :: Maybe DisappearanceOpenIntention,
    projectedProbe :: Maybe DisappearanceProbeId
  }
  deriving stock (Eq, Show)

-- | The owner-private write gate installed in the same leaf transition as the
-- projected Open. Its constructor is hidden so no use case can mint a token in
-- lieu of serializing the actual leaf transition.
data MatchingWriteGate
  = MatchingWriteGate
      DisappearanceProbeId
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
      HeraldPublicationPrefix
  deriving stock (Eq, Show)

matchingWriteGateProbe :: MatchingWriteGate -> DisappearanceProbeId
matchingWriteGateProbe (MatchingWriteGate identifier _ _ _) = identifier

matchingWriteGateSubject :: MatchingWriteGate -> DisappearanceSubject
matchingWriteGateSubject (MatchingWriteGate _ subject _ _) = subject

matchingWriteGateCoordinate ::
  MatchingWriteGate -> DisappearanceSubjectMembershipCoordinate
matchingWriteGateCoordinate (MatchingWriteGate _ _ coordinate _) = coordinate

matchingWriteGateLocalCut :: MatchingWriteGate -> HeraldPublicationPrefix
matchingWriteGateLocalCut (MatchingWriteGate _ _ _ localCut) = localCut

matchingWriteGateForProbe ::
  DisappearanceProbeId -> State -> Maybe MatchingWriteGate
matchingWriteGateForProbe identifier state =
  (.matchingWriteGate) <$> Map.lookup identifier state.probes

data MarkerCell = MarkerCell
  { marker :: DisappearancePublicationMarker,
    completed :: Bool
  }
  deriving stock (Eq, Show)

data AlignmentCell = AlignmentCell
  { capture :: IncomingAlignmentCut,
    marker :: Maybe DisappearanceAlignmentMarker,
    completedThrough :: Maybe StoreRevision
  }
  deriving stock (Eq, Show)

data ProbePhase
  = ProbeCollecting
  | ProbeInvalidated ProjectedInvalidationCause ControlIndex
  | ProbeResolved DisappearanceResolutionOutcome ControlIndex
  | ProbeAborted ProjectedAbortCause ControlIndex
  deriving stock (Eq, Show)

data Probe = Probe
  { projected :: ProjectedDisappearanceProbe,
    evidenceSnapshot :: DisappearanceEvidenceSnapshot,
    localPublicationCut :: HeraldPublicationPrefix,
    matchingWriteGate :: MatchingWriteGate,
    localPublicationCutComplete :: Bool,
    publicationAssignmentsInstalled :: Bool,
    outgoingPublicationMarkers :: Map HeraldEpoch DisappearancePublicationMarker,
    incomingPublicationMarkers :: Map HeraldEpoch MarkerCell,
    incomingAlignmentCuts :: Map AlignmentSubscriptionId AlignmentCell,
    outgoingAlignmentMarkers :: Map AlignmentSubscriptionId DisappearanceAlignmentMarker,
    blockers :: Set DisappearanceBlockerWitness,
    matchingPublicationWitnesses :: Set DisappearanceEvidenceDigest,
    invalidationIntention :: Maybe DisappearanceInvalidationIntention,
    report :: Maybe LocalAbsenceReport,
    reportIntention :: Maybe LocalAbsenceReport,
    acceptedReports :: Map HeraldEpoch DisappearanceEvidenceClaim,
    resolveIntention :: Maybe DisappearanceResolveIntention,
    phase :: ProbePhase
  }
  deriving stock (Eq, Show)

data DisappearanceWorkLedger = DisappearanceWorkLedger
  { candidateCreations :: Word64,
    openIntentionCreations :: Word64,
    projectedOpenInstallations :: Word64,
    publicationMarkerAssignments :: Word64,
    publicationMarkerReceipts :: Word64,
    publicationMarkerCompletions :: Word64,
    localPublicationCutCompletions :: Word64,
    alignmentMarkerAssignments :: Word64,
    alignmentMarkerReceipts :: Word64,
    alignmentMarkerCompletions :: Word64,
    blockerObservations :: Word64,
    matchingPublicationObservations :: Word64,
    invalidationIntentionCreations :: Word64,
    reportCreations :: Word64,
    projectedReportInstallations :: Word64,
    resolveIntentionCreations :: Word64,
    terminalInstallations :: Word64,
    labelTerminalApplications :: Word64
  }
  deriving stock (Eq, Show)

emptyWorkLedger :: DisappearanceWorkLedger
emptyWorkLedger =
  DisappearanceWorkLedger
    { candidateCreations = 0,
      openIntentionCreations = 0,
      projectedOpenInstallations = 0,
      publicationMarkerAssignments = 0,
      publicationMarkerReceipts = 0,
      publicationMarkerCompletions = 0,
      localPublicationCutCompletions = 0,
      alignmentMarkerAssignments = 0,
      alignmentMarkerReceipts = 0,
      alignmentMarkerCompletions = 0,
      blockerObservations = 0,
      matchingPublicationObservations = 0,
      invalidationIntentionCreations = 0,
      reportCreations = 0,
      projectedReportInstallations = 0,
      resolveIntentionCreations = 0,
      terminalInstallations = 0,
      labelTerminalApplications = 0
    }

data State = State
  { localHerald :: !HeraldEpoch,
    candidates :: Map DisappearanceSubject Candidate,
    probes :: Map DisappearanceProbeId Probe,
    probeByCoordinate :: Map DisappearanceSubjectMembershipCoordinate DisappearanceProbeId,
    labelTerminalsByDecision :: Map LabelDecisionId ProjectedLabelTerminal,
    candidateLabelWaits :: Map (LabelDecisionId, DisappearanceSubject) GlobalObjectId,
    pendingPublication :: Map (DisappearanceProbeId, HeraldEpoch) DisappearancePublicationMarker,
    pendingAlignment :: Map (DisappearanceProbeId, AlignmentSubscriptionId) (HeraldEpoch, DisappearanceAlignmentMarker),
    workLedger :: DisappearanceWorkLedger,
    markerProbeChanges :: !(Set DisappearanceProbeId)
  }
  deriving stock (Eq, Show)

initialState :: HeraldEpoch -> State
initialState localHerald =
  State
    { localHerald,
      candidates = Map.empty,
      probes = Map.empty,
      probeByCoordinate = Map.empty,
      labelTerminalsByDecision = Map.empty,
      candidateLabelWaits = Map.empty,
      pendingPublication = Map.empty,
      pendingAlignment = Map.empty,
      workLedger = emptyWorkLedger,
      markerProbeChanges = Set.empty
    }

takeMarkerProbeChanges :: State -> (Set DisappearanceProbeId, State)
takeMarkerProbeChanges state = let changes = state.markerProbeChanges in changes `seq` (changes, clearMarkerProbeChanges state)

clearMarkerProbeChanges :: State -> State
clearMarkerProbeChanges state = state {markerProbeChanges = Set.empty}

disappearanceLocalHerald :: State -> HeraldEpoch
disappearanceLocalHerald state = state.localHerald

data CandidateDisposition
  = CandidateCreated
  | CandidateAlreadyRetained
  | CandidateRearmed
  | CandidateNotEligible
  | CandidateOpenPrepared
  | CandidateOpenAlreadyPrepared
  deriving stock (Eq, Show)

data CandidateWitness = CandidateWitness
  { candidateSubject :: DisappearanceSubject,
    candidateEligible :: Bool,
    candidateAwaitingLabelDecision :: Maybe LabelDecisionId,
    candidateOpenIntention :: Maybe DisappearanceOpenIntention,
    candidateProjectedProbe :: Maybe DisappearanceProbeId
  }
  deriving stock (Eq, Show)

candidateWitnesses :: State -> [CandidateWitness]
candidateWitnesses state =
  [ CandidateWitness
      candidate.subject
      candidate.eligible
      candidate.awaitingLabelDecision
      candidate.openIntention
      candidate.projectedProbe
  | candidate <- Map.elems state.candidates
  ]

data ProbePhaseView
  = ProbeCollectingView
  | ProbeInvalidatedView ProjectedInvalidationCause ControlIndex
  | ProbeResolvedView DisappearanceResolutionOutcome ControlIndex
  | ProbeAbortedView ProjectedAbortCause ControlIndex
  deriving stock (Eq, Show)

data MarkerCellView = MarkerCellView
  { markerCellMarker :: DisappearancePublicationMarker,
    markerCellCompleted :: Bool
  }
  deriving stock (Eq, Show)

data AlignmentCellView = AlignmentCellView
  { alignmentCellCapture :: IncomingAlignmentCut,
    alignmentCellMarker :: Maybe DisappearanceAlignmentMarker,
    alignmentCellCompletedThrough :: Maybe StoreRevision
  }
  deriving stock (Eq, Show)

data ProbeWitness = ProbeWitness
  { witnessProjectedProbe :: ProjectedDisappearanceProbe,
    witnessEvidenceSnapshot :: DisappearanceEvidenceSnapshot,
    witnessLocalPublicationCut :: HeraldPublicationPrefix,
    witnessMatchingWriteGate :: MatchingWriteGate,
    witnessLocalPublicationCutComplete :: Bool,
    witnessPublicationAssignmentsInstalled :: Bool,
    witnessOutgoingPublicationMarkers :: [(HeraldEpoch, DisappearancePublicationMarker)],
    witnessIncomingPublicationMarkers :: [(HeraldEpoch, MarkerCellView)],
    witnessIncomingAlignmentCuts :: [(AlignmentSubscriptionId, AlignmentCellView)],
    witnessOutgoingAlignmentMarkers :: [(AlignmentSubscriptionId, DisappearanceAlignmentMarker)],
    witnessBlockers :: [DisappearanceBlockerWitness],
    witnessMatchingPublicationWitnesses :: [DisappearanceEvidenceDigest],
    witnessInvalidationIntention :: Maybe DisappearanceInvalidationIntention,
    witnessReport :: Maybe LocalAbsenceReport,
    witnessReportIntention :: Maybe LocalAbsenceReport,
    witnessAcceptedReports :: [(HeraldEpoch, DisappearanceEvidenceClaim)],
    witnessResolveIntention :: Maybe DisappearanceResolveIntention,
    witnessPhase :: ProbePhaseView
  }
  deriving stock (Eq, Show)

probeWitness :: DisappearanceProbeId -> State -> Maybe ProbeWitness
probeWitness identifier state = toProbeWitness <$> Map.lookup identifier state.probes

probeWitnesses :: State -> [ProbeWitness]
probeWitnesses = fmap toProbeWitness . Map.elems . (.probes)

toProbeWitness :: Probe -> ProbeWitness
toProbeWitness probe =
  ProbeWitness
    { witnessProjectedProbe = probe.projected,
      witnessEvidenceSnapshot = probe.evidenceSnapshot,
      witnessLocalPublicationCut = probe.localPublicationCut,
      witnessMatchingWriteGate = probe.matchingWriteGate,
      witnessLocalPublicationCutComplete = probe.localPublicationCutComplete,
      witnessPublicationAssignmentsInstalled = probe.publicationAssignmentsInstalled,
      witnessOutgoingPublicationMarkers = Map.toAscList probe.outgoingPublicationMarkers,
      witnessIncomingPublicationMarkers =
        [ (source, MarkerCellView cell.marker cell.completed)
        | (source, cell) <- Map.toAscList probe.incomingPublicationMarkers
        ],
      witnessIncomingAlignmentCuts =
        [ (identifier, AlignmentCellView cell.capture cell.marker cell.completedThrough)
        | (identifier, cell) <- Map.toAscList probe.incomingAlignmentCuts
        ],
      witnessOutgoingAlignmentMarkers = Map.toAscList probe.outgoingAlignmentMarkers,
      witnessBlockers = Set.toAscList probe.blockers,
      witnessMatchingPublicationWitnesses = Set.toAscList probe.matchingPublicationWitnesses,
      witnessInvalidationIntention = probe.invalidationIntention,
      witnessReport = probe.report,
      witnessReportIntention = probe.reportIntention,
      witnessAcceptedReports = Map.toAscList probe.acceptedReports,
      witnessResolveIntention = probe.resolveIntention,
      witnessPhase = phaseView probe.phase
    }

phaseView :: ProbePhase -> ProbePhaseView
phaseView phase = case phase of
  ProbeCollecting -> ProbeCollectingView
  ProbeInvalidated cause index -> ProbeInvalidatedView cause index
  ProbeResolved outcome index -> ProbeResolvedView outcome index
  ProbeAborted cause index -> ProbeAbortedView cause index

openIntentions :: State -> [DisappearanceOpenIntention]
openIntentions state =
  [ intention
  | candidate <- Map.elems state.candidates,
    Just intention <- [candidate.openIntention]
  ]

reportIntentions :: State -> [LocalAbsenceReport]
reportIntentions state =
  [ report
  | probe <- Map.elems state.probes,
    Just report <- [probe.reportIntention]
  ]

invalidationIntentions :: State -> [DisappearanceInvalidationIntention]
invalidationIntentions state =
  [ intention
  | probe <- Map.elems state.probes,
    Just intention <- [probe.invalidationIntention]
  ]

resolveIntentions :: State -> [DisappearanceResolveIntention]
resolveIntentions state =
  [ intention
  | probe <- Map.elems state.probes,
    Just intention <- [probe.resolveIntention]
  ]

-- | Canonical decision order for the immutable label-terminal history retained
-- by this leaf.  Unrelated terminals are not retained here because they do not
-- concern a disappearance probe; the label owner remains their authority.
projectedLabelTerminals :: State -> [ProjectedLabelTerminal]
projectedLabelTerminals = Map.elems . (.labelTerminalsByDecision)

disappearanceWorkLedger :: State -> DisappearanceWorkLedger
disappearanceWorkLedger state = state.workLedger

disappearanceLogicalWork :: DisappearanceWorkLedger -> Word64
disappearanceLogicalWork ledger =
  ledger.candidateCreations
    + ledger.openIntentionCreations
    + ledger.projectedOpenInstallations
    + ledger.publicationMarkerAssignments
    + ledger.publicationMarkerReceipts
    + ledger.publicationMarkerCompletions
    + ledger.localPublicationCutCompletions
    + ledger.alignmentMarkerAssignments
    + ledger.alignmentMarkerReceipts
    + ledger.alignmentMarkerCompletions
    + ledger.blockerObservations
    + ledger.matchingPublicationObservations
    + ledger.invalidationIntentionCreations
    + ledger.reportCreations
    + ledger.projectedReportInstallations
    + ledger.resolveIntentionCreations
    + ledger.terminalInstallations
    + ledger.labelTerminalApplications

data DisappearanceWorkDimensions = DisappearanceWorkDimensions
  { members :: Word64,
    publicationMarkers :: Word64,
    alignmentMarkers :: Word64,
    matchingWork :: Word64
  }
  deriving stock (Eq, Show)

disappearanceWorkDimensions :: Word64 -> Word64 -> Word64 -> Word64 -> DisappearanceWorkDimensions
disappearanceWorkDimensions = DisappearanceWorkDimensions

disappearanceDimensionMembers :: DisappearanceWorkDimensions -> Word64
disappearanceDimensionMembers dimensions = dimensions.members

disappearanceDimensionPublicationMarkers :: DisappearanceWorkDimensions -> Word64
disappearanceDimensionPublicationMarkers dimensions = dimensions.publicationMarkers

disappearanceDimensionAlignmentMarkers :: DisappearanceWorkDimensions -> Word64
disappearanceDimensionAlignmentMarkers dimensions = dimensions.alignmentMarkers

disappearanceDimensionMatchingWork :: DisappearanceWorkDimensions -> Word64
disappearanceDimensionMatchingWork dimensions = dimensions.matchingWork

-- | The first pinned Increment-6 kernel formula.
--
-- Each of the @M@ captured Heralds may independently create the racing
-- local-take candidate and stable Open intention, in addition to installing
-- the projection, completing its local cut, preparing a report, observing its
-- own report, and retaining one Resolve intention; each directed publication marker
-- is assigned, received, completed, and contributes one non-local report
-- projection; each alignment marker is assigned, received, and completed.  A
-- matching-work fact is observed once and retains at most one invalidation.
disappearanceSuccessfulWorkBound :: DisappearanceWorkDimensions -> Word64
disappearanceSuccessfulWorkBound dimensions =
  7 * dimensions.members
    + 4 * dimensions.publicationMarkers
    + 3 * dimensions.alignmentMarkers
    + 2 * dimensions.matchingWork

newtype PreparedDisappearanceTransition output
  = PreparedDisappearanceTransition (Prepared State output)

preparedDisappearanceOutput :: PreparedDisappearanceTransition output -> output
preparedDisappearanceOutput (PreparedDisappearanceTransition prepared) = preparedOutput prepared

commitDisappearanceTransition :: PreparedDisappearanceTransition output -> (State, output)
commitDisappearanceTransition (PreparedDisappearanceTransition prepared) = commitPrepared prepared

data DisappearanceProblem
  = DisappearanceCandidateMissing DisappearanceSubject
  | DisappearanceCandidateProjected DisappearanceSubject DisappearanceProbeId
  | DisappearanceLocalHeraldNotCaptured HeraldEpoch
  | DisappearanceBlockerNotApplicable DisappearanceSubject DisappearanceBlockerClass
  | DisappearanceProjectedProbeConflict DisappearanceProbeId
  | DisappearanceSubjectAlreadyProjected DisappearanceSubject DisappearanceProbeId
  | DisappearanceCoordinateAlreadyProjected
      DisappearanceSubjectMembershipCoordinate
      DisappearanceProbeId
  | DisappearanceProbeMissing DisappearanceProbeId
  | DisappearanceProbeNotCollecting DisappearanceProbeId ProbePhaseView
  | DisappearancePublicationAssignmentsAlreadyInstalled DisappearanceProbeId
  | DisappearancePublicationAssignmentSetMismatch (Set HeraldEpoch) (Set HeraldEpoch)
  | DisappearancePublicationMarkerProbeMismatch DisappearanceProbeId DisappearanceProbeId
  | DisappearancePublicationMarkerCoordinateMismatch
      DisappearanceSubjectMembershipCoordinate
      DisappearanceSubjectMembershipCoordinate
  | DisappearancePublicationMarkerDirectionMismatch StreamDirection
  | DisappearancePublicationMarkerSourceNotCaptured HeraldEpoch
  | DisappearancePublicationMarkerConflict HeraldEpoch
  | DisappearancePublicationMarkerMissing HeraldEpoch
  | DisappearanceLocalPublicationCutAlreadyComplete DisappearanceProbeId
  | DisappearanceAlignmentCaptureDuplicate AlignmentSubscriptionId
  | DisappearanceAlignmentDestinationMismatch AlignmentSubscriptionId HeraldEpoch
  | DisappearanceAlignmentSourceNotCaptured HeraldEpoch
  | DisappearanceAlignmentMarkerProbeMismatch DisappearanceProbeId DisappearanceProbeId
  | DisappearanceAlignmentMarkerNotCaptured AlignmentSubscriptionId
  | DisappearanceAlignmentMarkerConflict AlignmentSubscriptionId
  | DisappearanceAlignmentMarkerMissing AlignmentSubscriptionId
  | DisappearanceAlignmentCompletionBeforeMarker AlignmentSubscriptionId
  | DisappearanceAlignmentCompletionShort AlignmentSubscriptionId StoreRevision StoreRevision
  | DisappearanceBlockerObservationAfterTerminal DisappearanceProbeId
  | DisappearanceEvidenceSubjectMismatch
      DisappearanceProbeId
      DisappearanceSubject
      DisappearanceSubject
  | DisappearanceMatchingWriteGateMismatch DisappearanceProbeId
  | DisappearanceMatchingPublicationSubjectMismatch
      DisappearanceProbeId
      DisappearanceSubject
      DisappearanceSubject
  | DisappearanceMatchingPublicationAtOrBeforeCut
      DisappearanceProbeId
      HeraldPublicationPosition
      HeraldPublicationPrefix
  | DisappearanceReportBlocked DisappearanceProbeId
  | DisappearanceReportAlreadyPrepared DisappearanceProbeId
  | DisappearanceProjectedReportProbeMismatch DisappearanceProbeId DisappearanceProbeId
  | DisappearanceProjectedReportCoordinateMismatch
      DisappearanceSubjectMembershipCoordinate
      DisappearanceSubjectMembershipCoordinate
  | DisappearanceProjectedReporterNotCaptured HeraldEpoch
  | DisappearanceProjectedReportConflict HeraldEpoch
  | DisappearanceTerminalProbeMismatch DisappearanceProbeId DisappearanceProbeId
  | DisappearanceTerminalConflict DisappearanceProbeId
  | DisappearanceTerminalEvidenceIncomplete DisappearanceProbeId
  | DisappearanceControlledResolutionTerminalInvalidated DisappearanceProbeId
  | DisappearanceControlledResolutionTerminalAborted DisappearanceProbeId
  | DisappearanceControlledResolutionSubjectRegular
      DisappearanceProbeId
      DisappearanceSubject
  | DisappearanceRegularResolutionTerminalInvalidated DisappearanceProbeId
  | DisappearanceRegularResolutionTerminalAborted DisappearanceProbeId
  | DisappearanceRegularResolutionSubjectControlled
      DisappearanceProbeId
      DisappearanceSubject
  | DisappearanceMembershipSuccessorMismatch DisappearanceProbeId
  | DisappearanceLabelInvalidationSubjectNotControlled
      DisappearanceProbeId
      DisappearanceSubject
  | DisappearanceLabelInvalidationObjectMismatch
      DisappearanceProbeId
      GlobalObjectId
      GlobalObjectId
  | DisappearanceLabelTerminalSubjectNotControlled
      LabelDecisionId
      DisappearanceSubject
  | DisappearanceLabelTerminalObjectMismatch
      LabelDecisionId
      GlobalObjectId
      GlobalObjectId
  | DisappearanceLabelTerminalConflict LabelDecisionId
  | DisappearanceLabelTerminalCandidateConflict DisappearanceSubject
  | DisappearanceProtocolFailure DisappearanceProtocolProblem
  deriving stock (Eq, Show)

prepareLocalTakeCandidate ::
  DisappearanceCandidateObservation ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition CandidateDisposition)
prepareLocalTakeCandidate observation state =
  PreparedDisappearanceTransition
    <$> prepareTransition transition state
  where
    subject = disappearanceCandidateSubject observation
    transition predecessor =
      let activeProbe = activeProbeForSubject subject predecessor
          pendingLabelDecision =
            pendingLabelDecisionForSubject subject predecessor
       in case Map.lookup subject predecessor.candidates of
            Just candidate
              | candidate.awaitingLabelDecision /= Nothing ->
                  Right (predecessor, CandidateNotEligible)
              | candidate.eligible ->
                  Right
                    ( predecessor
                        { candidates =
                            Map.insert
                              subject
                              candidate
                                { openIntention =
                                    case activeProbe of
                                      Nothing -> candidate.openIntention
                                      Just _ -> Nothing,
                                  projectedProbe =
                                    case activeProbe of
                                      Nothing -> candidate.projectedProbe
                                      Just identifier -> Just identifier
                                }
                              predecessor.candidates
                        },
                      CandidateAlreadyRetained
                    )
              | otherwise ->
                  Right
                    ( predecessor
                        { candidates =
                            Map.insert
                              subject
                              candidate
                                { eligible = True,
                                  awaitingLabelDecision = Nothing,
                                  openIntention = Nothing,
                                  projectedProbe = activeProbe
                                }
                              predecessor.candidates,
                          workLedger =
                            predecessor.workLedger
                              { candidateCreations = predecessor.workLedger.candidateCreations + 1
                              }
                        },
                      CandidateRearmed
                    )
            Nothing ->
              Right
                ( predecessor
                    { candidates =
                        Map.insert
                          subject
                          Candidate
                            { subject,
                              eligible = pendingLabelDecision == Nothing,
                              awaitingLabelDecision = pendingLabelDecision,
                              openIntention = Nothing,
                              projectedProbe =
                                case pendingLabelDecision of
                                  Nothing -> activeProbe
                                  Just _ -> Nothing
                            }
                          predecessor.candidates,
                      workLedger =
                        predecessor.workLedger
                          { candidateCreations = predecessor.workLedger.candidateCreations + 1
                          }
                    },
                  CandidateCreated
                )

-- State admission permits only one collecting probe for a subject coordinate,
-- so this canonical scan has at most one result. A local take observed after a
-- remote home projected Open joins that all-member workflow instead of opening
-- a redundant probe.
activeProbeForSubject :: DisappearanceSubject -> State -> Maybe DisappearanceProbeId
activeProbeForSubject subject state =
  case [ identifier
       | (identifier, probe) <- Map.toAscList state.probes,
         probe.phase == ProbeCollecting,
         projectedProbeSubject probe.projected == subject
       ] of
    [] -> Nothing
    identifier : _ -> Just identifier

pendingLabelDecisionForSubject ::
  DisappearanceSubject -> State -> Maybe LabelDecisionId
pendingLabelDecisionForSubject subject state =
  case Set.toAscList (pendingLabelDecisionsForSubject subject state) of
    [] -> Nothing
    decision : _ -> Just decision

pendingLabelDecisionsForSubject ::
  DisappearanceSubject -> State -> Set LabelDecisionId
pendingLabelDecisionsForSubject subject state =
  Set.fromList
    [ decision
    | ((decision, retainedSubject), _) <- Map.toAscList state.candidateLabelWaits,
      retainedSubject == subject,
      Map.notMember decision state.labelTerminalsByDecision
    ]
    `Set.union` Set.fromList
      [ decision
      | probe <- Map.elems state.probes,
        projectedProbeSubject probe.projected == subject,
        ProbeInvalidated (ProjectedLabelInvalidation decision _) _ <- [probe.phase],
        Map.notMember decision state.labelTerminalsByDecision
      ]

-- | A take can precede closure of an already projected label fence. Preserve
-- that exact local candidate until the label owner supplies its terminal; no
-- disappearance Open is needed merely to discover the active label workflow.
prepareCandidateLabelWait ::
  LabelDecisionId ->
  GlobalObjectId ->
  DisappearanceSubject ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition ())
prepareCandidateLabelWait decision object subject state = do
  candidate <- maybe (Left (DisappearanceCandidateMissing subject)) Right (Map.lookup subject state.candidates)
  unless
    (controlledSubjectObject subject == Just object && candidate.projectedProbe == Nothing)
    (Left (DisappearanceLabelTerminalCandidateConflict subject))
  PreparedDisappearanceTransition
    <$> prepareTransition
      ( \predecessor ->
          Right
            ( predecessor
                { candidates =
                    Map.insert
                      subject
                      candidate {eligible = False, awaitingLabelDecision = Just decision, openIntention = Nothing}
                      predecessor.candidates,
                  candidateLabelWaits = Map.insert (decision, subject) object predecessor.candidateLabelWaits
                },
              ()
            )
      )
      state

prepareCandidateReevaluation ::
  DisappearanceOpenContext ->
  DisappearanceSubject ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition CandidateDisposition)
prepareCandidateReevaluation context subject state = do
  candidate <-
    maybe
      (Left (DisappearanceCandidateMissing subject))
      Right
      (Map.lookup subject state.candidates)
  let membership = disappearanceOpenContextMembership context
      activeMembers =
        Set.fromList (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership))
      intention =
        disappearanceOpenIntention
          subject
          membership
  unless
    (Set.member state.localHerald activeMembers)
    (Left (DisappearanceLocalHeraldNotCaptured state.localHerald))
  if not candidate.eligible || candidate.awaitingLabelDecision /= Nothing
    then unchanged CandidateNotEligible state
    else case candidate.projectedProbe of
      Just _ -> unchanged CandidateAlreadyRetained state
      Nothing ->
        PreparedDisappearanceTransition
          <$> prepareTransition
            ( \predecessor ->
                case candidate.openIntention of
                  Just retained
                    | retained == intention ->
                        Right (predecessor, CandidateOpenAlreadyPrepared)
                  -- A retained Open is stable only for its exact subject and
                  -- membership coordinate.  Once the established membership
                  -- changes, keeping the predecessor intention here would
                  -- permanently suppress the successor-generation Open even
                  -- after the Oracle has rejected the stale request.
                  Just _ ->
                    Right
                      ( predecessor
                          { candidates =
                              Map.insert
                                subject
                                candidate {openIntention = Just intention}
                                predecessor.candidates,
                            workLedger =
                              predecessor.workLedger
                                { openIntentionCreations =
                                    predecessor.workLedger.openIntentionCreations + 1
                                }
                          },
                        CandidateOpenPrepared
                      )
                  Nothing ->
                    Right
                      ( predecessor
                          { candidates =
                              Map.insert
                                subject
                                candidate {openIntention = Just intention}
                                predecessor.candidates,
                            workLedger =
                              predecessor.workLedger
                                { openIntentionCreations =
                                    predecessor.workLedger.openIntentionCreations + 1
                                }
                          },
                        CandidateOpenPrepared
                      )
            )
            state

data ProjectedOpenDisposition
  = ProjectedOpenInstalled
  | ProjectedOpenDuplicate
  deriving stock (Eq, Show)

data ProjectedOpenOutput = ProjectedOpenOutput
  { projectedOpenDisposition :: ProjectedOpenDisposition,
    projectedPublicationMarkerItems :: Map HeraldEpoch (SequenceBoundPeerItem DisappearancePublicationMarker),
    projectedAlignmentMarkers :: [DisappearanceAlignmentMarker]
  }

prepareProjectedOpen ::
  ProjectedDisappearanceProbe ->
  DisappearanceEvidenceSnapshot ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition ProjectedOpenOutput)
prepareProjectedOpen projected evidenceSnapshot state = do
  let identifier = projectedProbeId projected
      subject = projectedProbeSubject projected
      coordinate = projectedProbeCoordinate projected
      members = Set.fromList (NonEmpty.toList (projectedProbeMembers projected))
      remoteMembers = Set.delete state.localHerald members
      localCut = disappearanceEvidenceSnapshotLocalPublicationCut evidenceSnapshot
      incomingCuts = disappearanceEvidenceSnapshotIncomingAlignmentCuts evidenceSnapshot
      outgoingCuts = disappearanceEvidenceSnapshotOutgoingAlignmentCuts evidenceSnapshot
      blockers = disappearanceEvidenceSnapshotBlockers evidenceSnapshot
  unless
    (Set.member state.localHerald members)
    (Left (DisappearanceLocalHeraldNotCaptured state.localHerald))
  unless
    (disappearanceEvidenceSnapshotSubject evidenceSnapshot == subject)
    ( Left
        ( DisappearanceEvidenceSubjectMismatch
            identifier
            subject
            (disappearanceEvidenceSnapshotSubject evidenceSnapshot)
        )
    )
  mapM_ (validateBlockerForSubject subject) blockers
  incomingMap <- checkedIncomingAlignment state.localHerald members incomingCuts
  alignmentMap <-
    checkedOutgoingAlignment
      state.localHerald
      members
      identifier
      outgoingCuts
  let sameProjectionInputs retained =
        retained.projected == projected
  case Map.lookup identifier state.probes of
    Just retained
      | sameProjectionInputs retained ->
          PreparedDisappearanceTransition
            <$> prepareTransition
              ( \predecessor ->
                  Right
                    ( predecessor,
                      ProjectedOpenOutput ProjectedOpenDuplicate Map.empty []
                    )
              )
              state
      | otherwise -> Left (DisappearanceProjectedProbeConflict identifier)
    Nothing -> do
      case activeProbeForSubject subject state of
        Just incumbent ->
          Left (DisappearanceSubjectAlreadyProjected subject incumbent)
        Nothing -> Right ()
      case Map.lookup coordinate state.probeByCoordinate of
        Just incumbent ->
          Left (DisappearanceCoordinateAlreadyProjected coordinate incumbent)
        Nothing -> Right ()
      let markerItems =
            Map.fromSet
              (const (disappearancePublicationMarkerItem identifier coordinate))
              remoteMembers
          probe =
            Probe
              { projected,
                evidenceSnapshot,
                localPublicationCut = localCut,
                matchingWriteGate =
                  MatchingWriteGate identifier subject coordinate localCut,
                localPublicationCutComplete = False,
                publicationAssignmentsInstalled = False,
                outgoingPublicationMarkers = Map.empty,
                incomingPublicationMarkers = Map.empty,
                incomingAlignmentCuts = incomingMap,
                outgoingAlignmentMarkers = alignmentMap,
                blockers = Set.fromList blockers,
                matchingPublicationWitnesses =
                  Set.fromList
                    ( fmap
                        matchingPublicationWitness
                        ( disappearanceEvidenceSnapshotMatchingPublications
                            evidenceSnapshot
                        )
                    ),
                invalidationIntention = Nothing,
                report = Nothing,
                reportIntention = Nothing,
                acceptedReports = Map.empty,
                resolveIntention = Nothing,
                phase = ProbeCollecting
              }
          nextCandidates =
            Map.adjust
              ( \candidate ->
                  if candidate.eligible
                    then candidate {openIntention = Nothing, projectedProbe = Just identifier}
                    else candidate
              )
              subject
              state.candidates
          output =
            ProjectedOpenOutput
              ProjectedOpenInstalled
              markerItems
              (Map.elems alignmentMap)
      PreparedDisappearanceTransition
        <$> prepareTransition
          ( \predecessor ->
              Right
                ( predecessor
                    { candidates = nextCandidates,
                      probes = Map.insert identifier probe predecessor.probes,
                      markerProbeChanges = Set.insert identifier predecessor.markerProbeChanges,
                      probeByCoordinate =
                        Map.insert coordinate identifier predecessor.probeByCoordinate,
                      workLedger =
                        predecessor.workLedger
                          { projectedOpenInstallations =
                              predecessor.workLedger.projectedOpenInstallations + 1,
                            alignmentMarkerAssignments =
                              predecessor.workLedger.alignmentMarkerAssignments
                                + fromIntegral (Map.size alignmentMap),
                            blockerObservations =
                              predecessor.workLedger.blockerObservations
                                + disappearanceEvidenceSnapshotBlockerCount evidenceSnapshot
                          }
                    },
                  output
                )
          )
          state
checkedIncomingAlignment ::
  HeraldEpoch ->
  Set HeraldEpoch ->
  [IncomingAlignmentCut] ->
  Either DisappearanceProblem (Map AlignmentSubscriptionId AlignmentCell)
checkedIncomingAlignment local members = foldM insertOne Map.empty
  where
    insertOne retained cut = do
      let identifier = incomingAlignmentCutSubscription cut
          source = incomingAlignmentCutSource cut
          destination = alignmentSubscriptionIdDestinationHerald identifier
      when
        (Map.member identifier retained)
        (Left (DisappearanceAlignmentCaptureDuplicate identifier))
      unless
        (destination == local)
        (Left (DisappearanceAlignmentDestinationMismatch identifier local))
      unless
        (Set.member source members)
        (Left (DisappearanceAlignmentSourceNotCaptured source))
      Right
        ( Map.insert
            identifier
            AlignmentCell
              { capture = cut,
                marker = Nothing,
                completedThrough = Nothing
              }
            retained
        )

checkedOutgoingAlignment ::
  HeraldEpoch ->
  Set HeraldEpoch ->
  DisappearanceProbeId ->
  [OutgoingAlignmentCut] ->
  Either DisappearanceProblem (Map AlignmentSubscriptionId DisappearanceAlignmentMarker)
checkedOutgoingAlignment local members identifier = foldM insertOne Map.empty
  where
    insertOne retained cut = do
      let subscription = outgoingAlignmentCutSubscription cut
          destination = alignmentSubscriptionIdDestinationHerald subscription
      when
        (Map.member subscription retained)
        (Left (DisappearanceAlignmentCaptureDuplicate subscription))
      unless
        (Set.member destination members)
        (Left (DisappearanceAlignmentDestinationMismatch subscription local))
      Right
        ( Map.insert
            subscription
            (disappearanceAlignmentMarker identifier cut)
            retained
        )

validateBlockerForSubject ::
  DisappearanceSubject ->
  DisappearanceBlockerWitness ->
  Either DisappearanceProblem ()
validateBlockerForSubject subject witness =
  case disappearanceSubjectView subject of
    RegularSortDefinitionSubjectView {} -> Right ()
    ControlledPredefinedSubjectView {}
      | commonBlocker (disappearanceBlockerClass witness) -> Right ()
      | otherwise ->
          Left
            ( DisappearanceBlockerNotApplicable
                subject
                (disappearanceBlockerClass witness)
            )

commonBlocker :: DisappearanceBlockerClass -> Bool
commonBlocker blockerClass = case blockerClass of
  VisibleApplicationCopyBlocker -> True
  UnsequencedPublicationBlocker -> True
  HeldPublicationBlocker -> True
  PeerInboxBlocker -> True
  PeerOutboxBlocker -> True
  RetainedStoreValueBlocker -> False
  RetainedPublicationStageBlocker -> True
  AlignmentDebtBlocker -> True
  AlignmentObligationBlocker -> True
  AlignmentAttemptBlocker -> True
  AlignmentSubscriptionBlocker -> True
  AlignmentSnapshotBlocker -> True
  AlignmentChangeLogBlocker -> True
  AlignmentCertificateBlocker -> True
  UncertainSemanticPayloadBlocker -> True
  _ -> False

preparePublicationMarkerAssignments ::
  DisappearanceProbeId ->
  [DisappearancePublicationMarker] ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition ())
preparePublicationMarkerAssignments identifier markers state = do
  probe <- requireProbe identifier state
  let projected = probe.projected
      expected =
        Set.delete
          state.localHerald
          (Set.fromList (NonEmpty.toList (projectedProbeMembers projected)))
  markerMap <- foldM (insertOutgoingMarker state.localHerald projected) Map.empty markers
  let actual = Map.keysSet markerMap
  unless
    (actual == expected)
    (Left (DisappearancePublicationAssignmentSetMismatch expected actual))
  if probe.publicationAssignmentsInstalled
    then
      if probe.outgoingPublicationMarkers == markerMap
        then unchanged () state
        else Left (DisappearancePublicationAssignmentsAlreadyInstalled identifier)
    else do
      requireCollectingPhase identifier probe
      PreparedDisappearanceTransition
        <$> prepareTransition
          ( \predecessor ->
              Right
                ( updateProbe
                    identifier
                    ( \current ->
                        current
                          { publicationAssignmentsInstalled = True,
                            outgoingPublicationMarkers = markerMap
                          }
                    )
                    ( incrementLedger
                        ( \ledger ->
                            ledger
                              { publicationMarkerAssignments =
                                  ledger.publicationMarkerAssignments
                                    + fromIntegral (length markers)
                              }
                        )
                        predecessor
                    ),
                  ()
                )
          )
          state

insertOutgoingMarker ::
  HeraldEpoch ->
  ProjectedDisappearanceProbe ->
  Map HeraldEpoch DisappearancePublicationMarker ->
  DisappearancePublicationMarker ->
  Either DisappearanceProblem (Map HeraldEpoch DisappearancePublicationMarker)
insertOutgoingMarker local projected retained marker = do
  validateMarkerForProbe projected marker
  let direction = disappearancePublicationMarkerDirection marker
      source = streamDirectionSource direction
      destination = streamDirectionDestination direction
  unless (source == local) (Left (DisappearancePublicationMarkerDirectionMismatch direction))
  case Map.lookup destination retained of
    Nothing -> Right (Map.insert destination marker retained)
    Just _ -> Left (DisappearancePublicationMarkerConflict destination)

data MarkerAdmissionDisposition
  = MarkerAwaitingProbeProjection DisappearanceProbeId
  | MarkerRetained
  | MarkerDuplicate
  | MarkerTerminallyObsolete
  | MarkerCompleted
  | MarkerCompletionDuplicate
  deriving stock (Eq, Show)

preparePublicationMarkerReceipt ::
  DisappearancePublicationMarker ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition MarkerAdmissionDisposition)
preparePublicationMarkerReceipt marker state = do
  prepared <- preparePublicationMarkerReceiptRetained marker state
  let (successor, disposition) = commitDisappearanceTransition prepared
      key = (disappearancePublicationMarkerProbe marker, streamDirectionSource (disappearancePublicationMarkerDirection marker))
      retained = case disposition of
        MarkerAwaitingProbeProjection _ -> successor
        _ -> successor {pendingPublication = Map.delete key successor.pendingPublication}
  PreparedDisappearanceTransition <$> prepareTransition (\_ -> Right (retained, disposition)) state

preparePublicationMarkerReceiptRetained ::
  DisappearancePublicationMarker ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition MarkerAdmissionDisposition)
preparePublicationMarkerReceiptRetained marker state =
  case Map.lookup identifier state.probes of
    Nothing ->
      let source = streamDirectionSource (disappearancePublicationMarkerDirection marker)
          key = (identifier, source)
       in case Map.lookup key state.pendingPublication of
            Just retained | retained /= marker -> Left (DisappearancePublicationMarkerConflict source)
            _ ->
              PreparedDisappearanceTransition
                <$> prepareTransition
                  ( \predecessor ->
                      Right
                        ( predecessor {pendingPublication = Map.insert key marker predecessor.pendingPublication},
                          MarkerAwaitingProbeProjection identifier
                        )
                  )
                  state
    Just probe -> do
      validateMarkerForProbe probe.projected marker
      let direction = disappearancePublicationMarkerDirection marker
          source = streamDirectionSource direction
          destination = streamDirectionDestination direction
          members = Set.fromList (NonEmpty.toList (projectedProbeMembers probe.projected))
      unless
        (destination == state.localHerald && source /= state.localHerald)
        (Left (DisappearancePublicationMarkerDirectionMismatch direction))
      unless
        (Set.member source members)
        (Left (DisappearancePublicationMarkerSourceNotCaptured source))
      case Map.lookup source probe.incomingPublicationMarkers of
        Just cell
          | cell.marker == marker ->
              case probe.phase of
                ProbeCollecting -> unchanged MarkerDuplicate state
                _ -> unchanged MarkerTerminallyObsolete state
          | otherwise -> Left (DisappearancePublicationMarkerConflict source)
        Nothing -> do
          case probe.phase of
            ProbeCollecting ->
              changedProbe
                identifier
                ( \current ->
                    current
                      { incomingPublicationMarkers =
                          Map.insert
                            source
                            MarkerCell {marker, completed = False}
                            current.incomingPublicationMarkers
                      }
                )
                ( \ledger ->
                    ledger
                      { publicationMarkerReceipts = ledger.publicationMarkerReceipts + 1
                      }
                )
                MarkerRetained
                state
            _ -> unchanged MarkerTerminallyObsolete state
  where
    identifier = disappearancePublicationMarkerProbe marker

pendingPublicationMarkers :: State -> [DisappearancePublicationMarker]
pendingPublicationMarkers = Map.elems . (.pendingPublication)

preparePublicationMarkerCompletion ::
  DisappearancePublicationMarker ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition MarkerAdmissionDisposition)
preparePublicationMarkerCompletion marker state = do
  let identifier = disappearancePublicationMarkerProbe marker
      source = streamDirectionSource (disappearancePublicationMarkerDirection marker)
  probe <- requireProbe identifier state
  validateMarkerForProbe probe.projected marker
  case Map.lookup source probe.incomingPublicationMarkers of
    Nothing -> case probe.phase of
      ProbeCollecting -> Left (DisappearancePublicationMarkerMissing source)
      _ -> unchanged MarkerCompletionDuplicate state
    Just cell -> do
      unless
        (cell.marker == marker)
        (Left (DisappearancePublicationMarkerConflict source))
      if cell.completed
        then unchanged MarkerCompletionDuplicate state
        else case probe.phase of
          ProbeCollecting ->
            changedProbe
              identifier
              ( \current ->
                  current
                    { incomingPublicationMarkers =
                        Map.insert source cell {completed = True} current.incomingPublicationMarkers
                    }
              )
              ( \ledger ->
                  ledger
                    { publicationMarkerCompletions = ledger.publicationMarkerCompletions + 1
                    }
              )
              MarkerCompleted
              state
          _ -> unchanged MarkerCompletionDuplicate state

prepareLocalPublicationCutCompletion ::
  DisappearanceProbeId ->
  HeraldPublicationPrefix ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition MarkerAdmissionDisposition)
prepareLocalPublicationCutCompletion identifier completedPrefix state = do
  probe <- requireProbe identifier state
  unless
    (completedPrefix >= probe.localPublicationCut)
    (Left (DisappearanceReportBlocked identifier))
  if probe.localPublicationCutComplete
    then unchanged MarkerCompletionDuplicate state
    else case probe.phase of
      ProbeCollecting ->
        changedProbe
          identifier
          (\current -> current {localPublicationCutComplete = True})
          ( \ledger ->
              ledger
                { localPublicationCutCompletions = ledger.localPublicationCutCompletions + 1
                }
          )
          MarkerCompleted
          state
      _ -> unchanged MarkerCompletionDuplicate state

pendingAlignmentMarkers :: State -> [(HeraldEpoch, DisappearanceAlignmentMarker)]
pendingAlignmentMarkers = Map.elems . (.pendingAlignment)

prepareAlignmentMarkerReceipt ::
  HeraldEpoch ->
  DisappearanceAlignmentMarker ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition MarkerAdmissionDisposition)
prepareAlignmentMarkerReceipt source marker state = do
  let subscription = disappearanceAlignmentMarkerSubscription marker
  unless
    (alignmentSubscriptionIdDestinationHerald subscription == state.localHerald)
    (Left (DisappearanceAlignmentDestinationMismatch subscription state.localHerald))
  prepared <- prepareAlignmentMarkerReceiptRetained source marker state
  let (successor, disposition) = commitDisappearanceTransition prepared
      retained = case disposition of
        MarkerAwaitingProbeProjection _ -> successor
        _ -> successor {pendingAlignment = Map.delete (disappearanceAlignmentMarkerProbe marker, disappearanceAlignmentMarkerSubscription marker) successor.pendingAlignment}
  PreparedDisappearanceTransition <$> prepareTransition (\_ -> Right (retained, disposition)) state

prepareAlignmentMarkerReceiptRetained :: HeraldEpoch -> DisappearanceAlignmentMarker -> State -> Either DisappearanceProblem (PreparedDisappearanceTransition MarkerAdmissionDisposition)
prepareAlignmentMarkerReceiptRetained source marker state =
  case Map.lookup identifier state.probes of
    Nothing -> case Map.lookup key state.pendingAlignment of
      Just retained
        | retained == (source, marker) -> unchanged (MarkerAwaitingProbeProjection identifier) state
        | otherwise -> Left (DisappearanceAlignmentMarkerConflict (disappearanceAlignmentMarkerSubscription marker))
      Nothing ->
        PreparedDisappearanceTransition
          <$> prepareTransition
            (\predecessor -> Right (predecessor {pendingAlignment = Map.insert key (source, marker) predecessor.pendingAlignment}, MarkerAwaitingProbeProjection identifier))
            state
    Just probe -> do
      unless
        (disappearanceAlignmentMarkerProbe marker == projectedProbeId probe.projected)
        ( Left
            ( DisappearanceAlignmentMarkerProbeMismatch
                (disappearanceAlignmentMarkerProbe marker)
                (projectedProbeId probe.projected)
            )
        )
      let subscription = disappearanceAlignmentMarkerSubscription marker
      unless
        (alignmentSubscriptionIdDestinationHerald subscription == state.localHerald)
        (Left (DisappearanceAlignmentDestinationMismatch subscription state.localHerald))
      cell <-
        maybe
          (Left (DisappearanceAlignmentMarkerNotCaptured subscription))
          Right
          (Map.lookup subscription probe.incomingAlignmentCuts)
      unless
        (incomingAlignmentCutSource cell.capture == source)
        (Left (DisappearanceAlignmentSourceNotCaptured source))
      case cell.marker of
        Just incumbent
          | incumbent == marker -> unchanged MarkerDuplicate state
          | otherwise -> Left (DisappearanceAlignmentMarkerConflict subscription)
        Nothing -> do
          case probe.phase of
            ProbeCollecting ->
              changedProbe
                identifier
                ( \current ->
                    current
                      { incomingAlignmentCuts =
                          Map.insert
                            subscription
                            (AlignmentCell cell.capture (Just marker) cell.completedThrough)
                            current.incomingAlignmentCuts
                      }
                )
                (\ledger -> ledger {alignmentMarkerReceipts = ledger.alignmentMarkerReceipts + 1})
                MarkerRetained
                state
            _ -> unchanged MarkerDuplicate state
  where
    identifier = disappearanceAlignmentMarkerProbe marker
    key = (identifier, disappearanceAlignmentMarkerSubscription marker)

prepareAlignmentMarkerCompletion ::
  DisappearanceAlignmentMarker ->
  StoreRevision ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition MarkerAdmissionDisposition)
prepareAlignmentMarkerCompletion marker appliedThrough state = do
  let identifier = disappearanceAlignmentMarkerProbe marker
      subscription = disappearanceAlignmentMarkerSubscription marker
      requiredRevision = disappearanceAlignmentMarkerThroughRevision marker
  probe <- requireProbe identifier state
  unless
    (disappearanceAlignmentMarkerProbe marker == projectedProbeId probe.projected)
    ( Left
        ( DisappearanceAlignmentMarkerProbeMismatch
            (disappearanceAlignmentMarkerProbe marker)
            (projectedProbeId probe.projected)
        )
    )
  cell <-
    maybe
      (Left (DisappearanceAlignmentMarkerMissing subscription))
      Right
      (Map.lookup subscription probe.incomingAlignmentCuts)
  retainedMarker <-
    maybe
      (Left (DisappearanceAlignmentCompletionBeforeMarker subscription))
      Right
      cell.marker
  unless
    (retainedMarker == marker)
    (Left (DisappearanceAlignmentMarkerConflict subscription))
  unless
    (appliedThrough >= requiredRevision)
    ( Left
        ( DisappearanceAlignmentCompletionShort
            subscription
            requiredRevision
            appliedThrough
        )
    )
  case cell.completedThrough of
    Just retained
      | retained >= requiredRevision -> unchanged MarkerCompletionDuplicate state
    _ -> case probe.phase of
      ProbeCollecting ->
        changedProbe
          identifier
          ( \current ->
              current
                { incomingAlignmentCuts =
                    Map.insert
                      subscription
                      cell {completedThrough = Just appliedThrough}
                      current.incomingAlignmentCuts
                }
          )
          (\ledger -> ledger {alignmentMarkerCompletions = ledger.alignmentMarkerCompletions + 1})
          MarkerCompleted
          state
      _ -> unchanged MarkerCompletionDuplicate state

data AlignmentMutation
  = AlignmentSubscriptionCreated AlignmentSubscriptionId
  | AlignmentSubscriptionReplaced AlignmentSubscriptionId
  | AlignmentSubscriptionCancelled AlignmentSubscriptionId
  | AlignmentSubscriptionSourceLost AlignmentSubscriptionId HeraldEpoch
  deriving stock (Eq, Ord, Show)

prepareAlignmentMutation ::
  DisappearanceProbeId ->
  AlignmentMutation ->
  DisappearanceBlockerWitness ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition (Maybe DisappearanceInvalidationIntention))
prepareAlignmentMutation identifier mutation witness state = do
  probe <- requireProbe identifier state
  validateBlockerForSubject (projectedProbeSubject probe.projected) witness
  validateAlignmentMutation state probe mutation
  prepareContradiction identifier LocalEvidenceContradicted (disappearanceBlockerDigest witness) state

validateAlignmentMutation :: State -> Probe -> AlignmentMutation -> Either DisappearanceProblem ()
validateAlignmentMutation state probe mutation = case mutation of
  AlignmentSubscriptionCreated subscription -> validateLocalDestination subscription
  AlignmentSubscriptionReplaced subscription -> validateCaptured subscription
  AlignmentSubscriptionCancelled subscription -> validateCaptured subscription
  AlignmentSubscriptionSourceLost subscription source -> do
    cell <- captured subscription
    unless
      (incomingAlignmentCutSource cell.capture == source)
      (Left (DisappearanceAlignmentSourceNotCaptured source))
  where
    validateLocalDestination subscription =
      unless
        (alignmentSubscriptionIdDestinationHerald subscription == state.localHerald)
        (Left (DisappearanceAlignmentDestinationMismatch subscription state.localHerald))
    captured subscription =
      maybe
        (Left (DisappearanceAlignmentMarkerNotCaptured subscription))
        Right
        (Map.lookup subscription probe.incomingAlignmentCuts)
    validateCaptured subscription = () <$ captured subscription

prepareBlockerObservation ::
  DisappearanceProbeId ->
  [DisappearanceBlockerWitness] ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition (Maybe DisappearanceInvalidationIntention))
prepareBlockerObservation identifier supplied state = do
  probe <- requireProbe identifier state
  mapM_ (validateBlockerForSubject (projectedProbeSubject probe.projected)) supplied
  case probe.phase of
    ProbeCollecting -> prepareCollecting probe
    _ -> unchanged Nothing state
  where
    prepareCollecting probe = do
      let nextBlockers = Set.fromList supplied
          introduced = nextBlockers `Set.difference` probe.blockers
          touched = symmetricDifferenceSize nextBlockers probe.blockers
      if nextBlockers == probe.blockers
        then unchanged probe.invalidationIntention state
        else case Set.lookupMin introduced of
          Just witness -> do
            prepared <-
              prepareContradiction
                identifier
                LocalEvidenceContradicted
                (disappearanceBlockerDigest witness)
                state
            let (successor, intention) = commitDisappearanceTransition prepared
            PreparedDisappearanceTransition
              <$> prepareTransition
                ( \_ ->
                    Right
                      ( updateProbe
                          identifier
                          (\current -> current {blockers = nextBlockers})
                          ( incrementLedger
                              ( \ledger ->
                                  ledger
                                    { blockerObservations =
                                        ledger.blockerObservations + touched
                                    }
                              )
                              successor
                          ),
                        intention
                      )
                )
                state
          Nothing ->
            changedProbe
              identifier
              (\current -> current {blockers = nextBlockers})
              ( \ledger ->
                  ledger
                    { blockerObservations =
                        ledger.blockerObservations + touched
                    }
              )
              probe.invalidationIntention
              state

-- | Observe a fresh simultaneous read from the actual evidence owners.
-- Blocker additions/removals are charged per normalized fact, matching writes
-- are admitted through the retained leaf-owned gate, and any change to the
-- captured alignment subscription/source set retains one contradiction.  The
-- snapshot replaces the prior read only after every subtransition prepares.
prepareEvidenceObservation ::
  DisappearanceProbeId ->
  DisappearanceEvidenceSnapshot ->
  State ->
  Either
    DisappearanceProblem
    (PreparedDisappearanceTransition [DisappearanceInvalidationIntention])
prepareEvidenceObservation identifier supplied state = do
  probe <- requireProbe identifier state
  let expectedSubject = projectedProbeSubject probe.projected
      actualSubject = disappearanceEvidenceSnapshotSubject supplied
  unless
    (actualSubject == expectedSubject)
    ( Left
        ( DisappearanceEvidenceSubjectMismatch
            identifier
            expectedSubject
            actualSubject
        )
    )
  case probe.phase of
    ProbeCollecting -> prepareCollectingEvidence probe
    _ -> unchanged [] state
  where
    prepareCollectingEvidence probe = do
      preparedBlockers <-
        prepareBlockerObservation
          identifier
          (disappearanceEvidenceSnapshotBlockers supplied)
          state
      let (afterBlockers, blockerIntention) =
            commitDisappearanceTransition preparedBlockers
          unseenMatching =
            filter
              ( \observation ->
                  Set.notMember
                    (matchingPublicationWitness observation)
                    probe.matchingPublicationWitnesses
              )
              (disappearanceEvidenceSnapshotMatchingPublications supplied)
      (afterMatching, matchingIntentions) <-
        foldM observeMatching (afterBlockers, []) unseenMatching
      let alignmentTouches =
            alignmentEvidenceChangeCount probe.evidenceSnapshot supplied
      (afterAlignment, alignmentIntention) <-
        if alignmentTouches == 0
          then Right (afterMatching, Nothing)
          else do
            prepared <-
              prepareContradiction
                identifier
                LocalEvidenceContradicted
                (alignmentEvidenceChangeWitness supplied)
                afterMatching
            let (contradicted, intention) =
                  commitDisappearanceTransition prepared
                charged =
                  incrementLedger
                    ( \ledger ->
                        ledger
                          { blockerObservations =
                              ledger.blockerObservations + alignmentTouches
                          }
                    )
                    contradicted
            Right (charged, intention)
      let successor =
            updateProbe
              identifier
              (\current -> current {evidenceSnapshot = supplied})
              afterAlignment
          intentions =
            nub
              ( maybe [] pure blockerIntention
                  <> matchingIntentions
                  <> maybe [] pure alignmentIntention
              )
      PreparedDisappearanceTransition
        <$> prepareTransition (\_ -> Right (successor, intentions)) state

    observeMatching (predecessor, retained) observation = do
      currentProbe <- requireProbe identifier predecessor
      prepared <-
        prepareMatchingPublicationObservation
          currentProbe.matchingWriteGate
          observation
          predecessor
      let (successor, intentions) =
            commitDisappearanceTransition prepared
      Right (successor, retained <> intentions)

symmetricDifferenceSize :: (Ord value) => Set value -> Set value -> Word64
symmetricDifferenceSize left right =
  fromIntegral
    ( Set.size (left `Set.difference` right)
        + Set.size (right `Set.difference` left)
    )

alignmentEvidenceChangeCount ::
  DisappearanceEvidenceSnapshot ->
  DisappearanceEvidenceSnapshot ->
  Word64
alignmentEvidenceChangeCount predecessor successor =
  symmetricDifferenceSize predecessorIncoming successorIncoming
    + symmetricDifferenceSize predecessorOutgoing successorOutgoing
  where
    predecessorIncoming =
      Set.fromList
        (disappearanceEvidenceSnapshotIncomingAlignmentCuts predecessor)
    successorIncoming =
      Set.fromList
        (disappearanceEvidenceSnapshotIncomingAlignmentCuts successor)
    predecessorOutgoing =
      Set.fromList
        ( fmap
            outgoingAlignmentCutSubscription
            (disappearanceEvidenceSnapshotOutgoingAlignmentCuts predecessor)
        )
    successorOutgoing =
      Set.fromList
        ( fmap
            outgoingAlignmentCutSubscription
            (disappearanceEvidenceSnapshotOutgoingAlignmentCuts successor)
        )

alignmentEvidenceChangeWitness ::
  DisappearanceEvidenceSnapshot -> DisappearanceEvidenceDigest
alignmentEvidenceChangeWitness snapshot =
  deriveDisappearanceEvidenceDigest
    ( ByteString.concat
        ( "ECLIPS-DISAPPEARANCE-ALIGNMENT-CAPTURE-CHANGE"
            : disappearanceSubjectCanonicalBytes
              (disappearanceEvidenceSnapshotSubject snapshot)
            : fmap
              incomingAlignmentCutCanonicalBytes
              (disappearanceEvidenceSnapshotIncomingAlignmentCuts snapshot)
              <> fmap
                outgoingAlignmentCutCanonicalBytes
                (disappearanceEvidenceSnapshotOutgoingAlignmentCuts snapshot)
        )
    )

prepareMatchingPublicationObservation ::
  MatchingWriteGate ->
  MatchingPublicationObservation ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition [DisappearanceInvalidationIntention])
prepareMatchingPublicationObservation suppliedGate observation state = do
  probe <- requireProbe identifier state
  validateMatchingWriteGate identifier suppliedGate probe
  unless
    (matchingPublicationSubject observation == matchingWriteGateSubject suppliedGate)
    ( Left
        ( DisappearanceMatchingPublicationSubjectMismatch
            identifier
            (matchingWriteGateSubject suppliedGate)
            (matchingPublicationSubject observation)
        )
    )
  unless
    (publicationPositionIsAfterCut (matchingPublicationPosition observation) probe.localPublicationCut)
    ( Left
        ( DisappearanceMatchingPublicationAtOrBeforeCut
            identifier
            (matchingPublicationPosition observation)
            probe.localPublicationCut
        )
    )
  case probe.phase of
    ProbeCollecting -> applyOne probe
    _ -> unchanged [] state
  where
    identifier = matchingWriteGateProbe suppliedGate
    applyOne probe = do
      let witness = matchingPublicationWitness observation
      if Set.member witness probe.matchingPublicationWitnesses
        then unchanged [] state
        else do
          observed <-
            changedProbe
              identifier
              ( \current ->
                  current
                    { matchingPublicationWitnesses =
                        Set.insert witness current.matchingPublicationWitnesses
                    }
              )
              ( \ledger ->
                  ledger
                    { matchingPublicationObservations =
                        ledger.matchingPublicationObservations + 1
                    }
              )
              ()
              state
          let (withObservation, ()) = commitDisappearanceTransition observed
              hadIntention = probe.invalidationIntention /= Nothing
          prepared <-
            prepareContradiction
              identifier
              MatchingPublicationObserved
              witness
              withObservation
          let (successor, maybeIntention) = commitDisappearanceTransition prepared
              created = if hadIntention then [] else maybe [] pure maybeIntention
          PreparedDisappearanceTransition
            <$> prepareTransition (\_ -> Right (successor, created)) state

validateMatchingWriteGate ::
  DisappearanceProbeId ->
  MatchingWriteGate ->
  Probe ->
  Either DisappearanceProblem ()
validateMatchingWriteGate identifier supplied retained =
  unless
    ( supplied == retained.matchingWriteGate
        && matchingWriteGateProbe supplied == projectedProbeId retained.projected
        && matchingWriteGateSubject supplied == projectedProbeSubject retained.projected
        && matchingWriteGateCoordinate supplied == projectedProbeCoordinate retained.projected
        && matchingWriteGateLocalCut supplied == retained.localPublicationCut
    )
    (Left (DisappearanceMatchingWriteGateMismatch identifier))

publicationPositionIsAfterCut ::
  HeraldPublicationPosition -> HeraldPublicationPrefix -> Bool
publicationPositionIsAfterCut _ EmptyHeraldPublicationPrefix = True
publicationPositionIsAfterCut position (HeraldPublicationPrefixThrough through) =
  position > through

-- | Gated work has not consumed a Herald publication position. Its complete
-- owner-retained payload authorizes one stable invalidation without inventing
-- a position in the captured publication cut.
prepareGatedPublicationInvalidation ::
  MatchingWriteGate ->
  DisappearanceEvidenceDigest ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition (Maybe DisappearanceInvalidationIntention))
prepareGatedPublicationInvalidation gate witness state = do
  probe <- requireProbe (matchingWriteGateProbe gate) state
  validateMatchingWriteGate (matchingWriteGateProbe gate) gate probe
  prepareContradiction (matchingWriteGateProbe gate) MatchingPublicationObserved witness state

prepareContradiction ::
  DisappearanceProbeId ->
  DisappearanceInvalidationReason ->
  DisappearanceEvidenceDigest ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition (Maybe DisappearanceInvalidationIntention))
prepareContradiction identifier reason witness state = do
  probe <- requireProbe identifier state
  case probe.phase of
    ProbeCollecting ->
      case probe.invalidationIntention of
        Just retained -> unchanged (Just retained) state
        Nothing ->
          let intention =
                disappearanceInvalidationIntention identifier state.localHerald reason witness
           in changedProbe
                identifier
                (\current -> current {invalidationIntention = Just intention})
                ( \ledger ->
                    ledger
                      { invalidationIntentionCreations = ledger.invalidationIntentionCreations + 1
                      }
                )
                (Just intention)
                state
    _ -> unchanged Nothing state

data ReportDisposition
  = LocalReportPrepared LocalAbsenceReport
  | LocalReportDuplicate LocalAbsenceReport
  deriving stock (Eq, Show)

prepareLocalAbsenceReport ::
  DisappearanceProbeId ->
  DisappearanceAbsenceAttestation ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition ReportDisposition)
prepareLocalAbsenceReport identifier attestation state = do
  probe <- requireCollecting identifier state
  case probe.report of
    Just retained
      | localAbsenceReportAttestation retained == attestation ->
          unchanged (LocalReportDuplicate retained) state
      | otherwise -> Left (DisappearanceReportAlreadyPrepared identifier)
    Nothing -> do
      unless (probeReady state probe) (Left (DisappearanceReportBlocked identifier))
      alignmentEvidence <-
        traverse
          (completedAlignmentEvidence probe.projected)
          (Map.elems probe.incomingAlignmentCuts)
      report <-
        either
          (Left . DisappearanceProtocolFailure)
          Right
          ( localAbsenceReportForOwner
              probe.projected
              state.localHerald
              probe.localPublicationCut
              (fmap (.marker) (Map.elems probe.incomingPublicationMarkers))
              alignmentEvidence
              attestation
          )
      changedProbe
        identifier
        ( \current ->
            current
              { report = Just report,
                reportIntention = Just report
              }
        )
        (\ledger -> ledger {reportCreations = ledger.reportCreations + 1})
        (LocalReportPrepared report)
        state

-- | Refresh every owner-derived fact immediately before preparing negative
-- evidence.  A caller cannot supply an attestation separately: it must be the
-- checked empty-set result carried by this exact snapshot.
prepareLocalAbsenceReportFromEvidence ::
  DisappearanceProbeId ->
  DisappearanceEvidenceSnapshot ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition ReportDisposition)
prepareLocalAbsenceReportFromEvidence identifier snapshot state = do
  preparedObservation <- prepareEvidenceObservation identifier snapshot state
  let (observed, _) = commitDisappearanceTransition preparedObservation
  attestation <-
    maybe
      (Left (DisappearanceReportBlocked identifier))
      Right
      (disappearanceEvidenceSnapshotAbsenceAttestation snapshot)
  preparedReport <- prepareLocalAbsenceReport identifier attestation observed
  let (successor, disposition) = commitDisappearanceTransition preparedReport
  PreparedDisappearanceTransition
    <$> prepareTransition (\_ -> Right (successor, disposition)) state

completedAlignmentEvidence ::
  ProjectedDisappearanceProbe ->
  AlignmentCell ->
  Either DisappearanceProblem CompletedIncomingAlignmentEvidence
completedAlignmentEvidence projected cell =
  case (cell.marker, cell.completedThrough) of
    (Just marker, Just completed) ->
      either
        (Left . DisappearanceProtocolFailure)
        Right
        ( completedIncomingAlignmentEvidenceForOwner
            projected
            cell.capture
            marker
            completed
        )
    _ -> Left (DisappearanceReportBlocked (projectedProbeId projected))

probeReady :: State -> Probe -> Bool
probeReady state probe =
  probe.phase == ProbeCollecting
    && probe.invalidationIntention == Nothing
    && probeCutComplete state probe
    && Set.null probe.blockers

data ProjectedReportDisposition
  = ProjectedReportInstalled
  | ProjectedReportDuplicate
  | ProjectedReportCompletedSet DisappearanceResolveIntention
  deriving stock (Eq, Show)

prepareProjectedReport ::
  DisappearanceEvidenceClaim ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition ProjectedReportDisposition)
prepareProjectedReport claim state = do
  let identifier = disappearanceEvidenceClaimProbeId claim
      reporter = disappearanceEvidenceClaimReporter claim
  probe <- requireProbe identifier state
  unless
    (disappearanceEvidenceClaimCoordinate claim == projectedProbeCoordinate probe.projected)
    ( Left
        ( DisappearanceProjectedReportCoordinateMismatch
            (disappearanceEvidenceClaimCoordinate claim)
            (projectedProbeCoordinate probe.projected)
        )
    )
  let members = Set.fromList (NonEmpty.toList (projectedProbeMembers probe.projected))
  unless
    (Set.member reporter members)
    (Left (DisappearanceProjectedReporterNotCaptured reporter))
  when (reporter == state.localHerald)
    $ case probe.report of
      Just localReport
        | localAbsenceReportClaim localReport == claim -> Right ()
      _ -> Left (DisappearanceProjectedReportConflict reporter)
  case Map.lookup reporter probe.acceptedReports of
    Just retained
      | retained == claim -> unchanged ProjectedReportDuplicate state
      | otherwise -> Left (DisappearanceProjectedReportConflict reporter)
    Nothing -> do
      requireCollectingPhase identifier probe
      let nextReports = Map.insert reporter claim probe.acceptedReports
          complete = Map.keysSet nextReports == members
          intention = disappearanceResolveIntention identifier (Map.elems nextReports)
          createsResolve = complete && probe.resolveIntention == Nothing
          nextResolve = if createsResolve then Just intention else probe.resolveIntention
          disposition =
            if createsResolve
              then ProjectedReportCompletedSet intention
              else ProjectedReportInstalled
      changedProbe
        identifier
        ( \current ->
            current
              { acceptedReports = nextReports,
                resolveIntention = nextResolve,
                reportIntention =
                  if reporter == state.localHerald
                    then Nothing
                    else current.reportIntention
              }
        )
        ( \ledger ->
            ledger
              { projectedReportInstallations = ledger.projectedReportInstallations + 1,
                resolveIntentionCreations =
                  ledger.resolveIntentionCreations
                    + if createsResolve then 1 else 0
              }
        )
        disposition
        state

data TerminalDisposition
  = TerminalInstalled
  | TerminalDuplicate
  deriving stock (Eq, Show)

prepareProjectedTerminal ::
  ProjectedDisappearanceTerminal ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition TerminalDisposition)
prepareProjectedTerminal terminal state = do
  let (identifier, nextPhase) = terminalFacts terminal
  probe <-
    maybe
      (Left (DisappearanceProbeMissing identifier))
      Right
      (Map.lookup identifier state.probes)
  case probe.phase of
    ProbeCollecting -> do
      validateCollectingTerminal identifier probe nextPhase state
      installTerminal identifier probe nextPhase state
    retained
      | retained == nextPhase -> unchanged TerminalDuplicate state
      | otherwise -> Left (DisappearanceTerminalConflict identifier)

-- | Opaque authority for the shared controlled-removal applicator.
--
-- The only constructor site is 'prepareProjectedControlledResolution', after
-- the disappearance leaf has checked the exact retained probe, complete
-- captured-member evidence, subject, and resolve index.  In particular, a
-- caller cannot turn a bare Oracle outcome or a regular-definition retirement
-- into controlled deletion authority.
data ControlledRemovalAuthority
  = ControlledRemovalAuthority
      DisappearanceProbeId
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
      GlobalObjectId
      StructuralOccurrenceId
      (Maybe LabelRevision)
      ControlIndex
  deriving stock (Eq, Show)

controlledRemovalAuthorityProbe ::
  ControlledRemovalAuthority -> DisappearanceProbeId
controlledRemovalAuthorityProbe
  (ControlledRemovalAuthority probe _ _ _ _ _ _) = probe

controlledRemovalAuthoritySubject ::
  ControlledRemovalAuthority -> DisappearanceSubject
controlledRemovalAuthoritySubject
  (ControlledRemovalAuthority _ subject _ _ _ _ _) = subject

controlledRemovalAuthorityCoordinate ::
  ControlledRemovalAuthority -> DisappearanceSubjectMembershipCoordinate
controlledRemovalAuthorityCoordinate
  (ControlledRemovalAuthority _ _ coordinate _ _ _ _) = coordinate

controlledRemovalAuthorityObject ::
  ControlledRemovalAuthority -> GlobalObjectId
controlledRemovalAuthorityObject
  (ControlledRemovalAuthority _ _ _ object _ _ _) = object

controlledRemovalAuthorityStructuralOccurrence ::
  ControlledRemovalAuthority -> StructuralOccurrenceId
controlledRemovalAuthorityStructuralOccurrence
  (ControlledRemovalAuthority _ _ _ _ occurrence _ _) = occurrence

controlledRemovalAuthorityExpectedLabelRevision ::
  ControlledRemovalAuthority -> Maybe LabelRevision
controlledRemovalAuthorityExpectedLabelRevision
  (ControlledRemovalAuthority _ _ _ _ _ revision _) = revision

controlledRemovalAuthorityResolveIndex ::
  ControlledRemovalAuthority -> ControlIndex
controlledRemovalAuthorityResolveIndex
  (ControlledRemovalAuthority _ _ _ _ _ _ index) = index

data ProjectedControlledResolutionDisposition
  = ProjectedControlledResolutionFresh
  | ProjectedControlledResolutionDuplicate
  deriving stock (Eq, Show)

-- | A checked leaf successor kept uncommitted so the disappearance use case
-- can prepare every removal-owner patch before installing any of them.
data PreparedProjectedControlledResolution
  = PreparedProjectedControlledResolution
      (PreparedDisappearanceTransition TerminalDisposition)
      ProjectedControlledResolutionDisposition
      (Maybe ControlledRemovalAuthority)

prepareProjectedControlledResolution ::
  ProjectedDisappearanceTerminal ->
  State ->
  Either DisappearanceProblem PreparedProjectedControlledResolution
prepareProjectedControlledResolution terminal state =
  case terminal of
    ProjectedDisappearanceInvalidated probe _ _ ->
      Left (DisappearanceControlledResolutionTerminalInvalidated probe)
    ProjectedDisappearanceAborted probe _ _ ->
      Left (DisappearanceControlledResolutionTerminalAborted probe)
    ProjectedDisappearanceResolved probe outcome _ ->
      case disappearanceResolutionOutcomeView outcome of
        RegularSortDefinitionRetired {} ->
          Left
            ( DisappearanceControlledResolutionSubjectRegular
                probe
                (disappearanceResolutionSubject outcome)
            )
        ControlledDisappearanceResolved object occurrence revision index -> do
          retainedProbe <-
            maybe
              (Left (DisappearanceProbeMissing probe))
              Right
              (Map.lookup probe state.probes)
          prepared <- prepareProjectedTerminal terminal state
          case preparedDisappearanceOutput prepared of
            TerminalInstalled ->
              Right
                ( PreparedProjectedControlledResolution
                    prepared
                    ProjectedControlledResolutionFresh
                    ( Just
                        ( ControlledRemovalAuthority
                            probe
                            (disappearanceResolutionSubject outcome)
                            (projectedProbeCoordinate retainedProbe.projected)
                            object
                            occurrence
                            revision
                            index
                        )
                    )
                )
            TerminalDuplicate ->
              Right
                ( PreparedProjectedControlledResolution
                    prepared
                    ProjectedControlledResolutionDuplicate
                    Nothing
                )

preparedProjectedControlledResolutionDisposition ::
  PreparedProjectedControlledResolution ->
  ProjectedControlledResolutionDisposition
preparedProjectedControlledResolutionDisposition
  (PreparedProjectedControlledResolution _ disposition _) = disposition

preparedProjectedControlledResolutionAuthority ::
  PreparedProjectedControlledResolution -> Maybe ControlledRemovalAuthority
preparedProjectedControlledResolutionAuthority
  (PreparedProjectedControlledResolution _ _ authority) = authority

commitProjectedControlledResolution ::
  PreparedProjectedControlledResolution ->
  ( State,
    ProjectedControlledResolutionDisposition,
    Maybe ControlledRemovalAuthority
  )
commitProjectedControlledResolution
  (PreparedProjectedControlledResolution prepared disposition authority) =
    let (successor, _) = commitDisappearanceTransition prepared
     in (successor, disposition, authority)

-- | One-shot authority for retiring an effective regular sort occurrence.
--
-- Its constructor remains private.  The authority binds the exact checked
-- subject and successor occurrence from the Oracle outcome to the immutable
-- local publication cut captured by Open and the latest audited evidence
-- snapshot retained by the probe; a caller cannot manufacture retirement
-- authority from a bare sort identifier or resolve index.
data RegularRetirementAuthority
  = RegularRetirementAuthority
      DisappearanceProbeId
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
      SortId
      CanonicalDescriptorDigest
      SortDefinitionOccurrenceId
      ControlIndex
      SortDefinitionOccurrenceId
      HeraldPublicationPrefix
      DisappearanceEvidenceSnapshot
  deriving stock (Eq, Show)

regularRetirementAuthorityProbe ::
  RegularRetirementAuthority -> DisappearanceProbeId
regularRetirementAuthorityProbe
  (RegularRetirementAuthority probe _ _ _ _ _ _ _ _ _) = probe

regularRetirementAuthoritySubject ::
  RegularRetirementAuthority -> DisappearanceSubject
regularRetirementAuthoritySubject
  (RegularRetirementAuthority _ subject _ _ _ _ _ _ _ _) = subject

regularRetirementAuthorityCoordinate ::
  RegularRetirementAuthority -> DisappearanceSubjectMembershipCoordinate
regularRetirementAuthorityCoordinate
  (RegularRetirementAuthority _ _ coordinate _ _ _ _ _ _ _) = coordinate

regularRetirementAuthoritySortId :: RegularRetirementAuthority -> SortId
regularRetirementAuthoritySortId
  (RegularRetirementAuthority _ _ _ sortId _ _ _ _ _ _) = sortId

regularRetirementAuthorityDescriptorDigest ::
  RegularRetirementAuthority -> CanonicalDescriptorDigest
regularRetirementAuthorityDescriptorDigest
  (RegularRetirementAuthority _ _ _ _ digest _ _ _ _ _) = digest

regularRetirementAuthorityOccurrence ::
  RegularRetirementAuthority -> SortDefinitionOccurrenceId
regularRetirementAuthorityOccurrence
  (RegularRetirementAuthority _ _ _ _ _ occurrence _ _ _ _) = occurrence

regularRetirementAuthorityResolveIndex ::
  RegularRetirementAuthority -> ControlIndex
regularRetirementAuthorityResolveIndex
  (RegularRetirementAuthority _ _ _ _ _ _ index _ _ _) = index

regularRetirementAuthoritySuccessorOccurrence ::
  RegularRetirementAuthority -> SortDefinitionOccurrenceId
regularRetirementAuthoritySuccessorOccurrence
  (RegularRetirementAuthority _ _ _ _ _ _ _ occurrence _ _) = occurrence

regularRetirementAuthorityLocalPublicationCut ::
  RegularRetirementAuthority -> HeraldPublicationPrefix
regularRetirementAuthorityLocalPublicationCut
  (RegularRetirementAuthority _ _ _ _ _ _ _ _ localCut _) = localCut

regularRetirementAuthorityEvidenceSnapshot ::
  RegularRetirementAuthority -> DisappearanceEvidenceSnapshot
regularRetirementAuthorityEvidenceSnapshot
  (RegularRetirementAuthority _ _ _ _ _ _ _ _ _ snapshot) = snapshot

data ProjectedRegularResolutionDisposition
  = ProjectedRegularResolutionFresh
  | ProjectedRegularResolutionDuplicate
  deriving stock (Eq, Show)

-- | A checked regular terminal kept uncommitted until every Herald owner has
-- prepared its retirement successor.
data PreparedProjectedRegularResolution
  = PreparedProjectedRegularResolution
      (PreparedDisappearanceTransition TerminalDisposition)
      ProjectedRegularResolutionDisposition
      (Maybe RegularRetirementAuthority)

prepareProjectedRegularResolution ::
  ProjectedDisappearanceTerminal ->
  State ->
  Either DisappearanceProblem PreparedProjectedRegularResolution
prepareProjectedRegularResolution terminal state =
  case terminal of
    ProjectedDisappearanceInvalidated probe _ _ ->
      Left (DisappearanceRegularResolutionTerminalInvalidated probe)
    ProjectedDisappearanceAborted probe _ _ ->
      Left (DisappearanceRegularResolutionTerminalAborted probe)
    ProjectedDisappearanceResolved probe outcome _ ->
      case disappearanceResolutionOutcomeView outcome of
        ControlledDisappearanceResolved {} ->
          Left
            ( DisappearanceRegularResolutionSubjectControlled
                probe
                (disappearanceResolutionSubject outcome)
            )
        RegularSortDefinitionRetired sortId digest occurrence index successorOccurrence -> do
          retainedProbe <-
            maybe
              (Left (DisappearanceProbeMissing probe))
              Right
              (Map.lookup probe state.probes)
          prepared <- prepareProjectedTerminal terminal state
          case preparedDisappearanceOutput prepared of
            TerminalInstalled ->
              Right
                ( PreparedProjectedRegularResolution
                    prepared
                    ProjectedRegularResolutionFresh
                    ( Just
                        ( RegularRetirementAuthority
                            probe
                            (disappearanceResolutionSubject outcome)
                            (projectedProbeCoordinate retainedProbe.projected)
                            sortId
                            digest
                            occurrence
                            index
                            successorOccurrence
                            retainedProbe.localPublicationCut
                            retainedProbe.evidenceSnapshot
                        )
                    )
                )
            TerminalDuplicate ->
              Right
                ( PreparedProjectedRegularResolution
                    prepared
                    ProjectedRegularResolutionDuplicate
                    Nothing
                )

preparedProjectedRegularResolutionDisposition ::
  PreparedProjectedRegularResolution -> ProjectedRegularResolutionDisposition
preparedProjectedRegularResolutionDisposition
  (PreparedProjectedRegularResolution _ disposition _) = disposition

preparedProjectedRegularResolutionAuthority ::
  PreparedProjectedRegularResolution -> Maybe RegularRetirementAuthority
preparedProjectedRegularResolutionAuthority
  (PreparedProjectedRegularResolution _ _ authority) = authority

commitProjectedRegularResolution ::
  PreparedProjectedRegularResolution ->
  ( State,
    ProjectedRegularResolutionDisposition,
    Maybe RegularRetirementAuthority
  )
commitProjectedRegularResolution
  (PreparedProjectedRegularResolution prepared disposition authority) =
    let (successor, _) = commitDisappearanceTransition prepared
     in (successor, disposition, authority)

validateCollectingTerminal ::
  DisappearanceProbeId ->
  Probe ->
  ProbePhase ->
  State ->
  Either DisappearanceProblem ()
validateCollectingTerminal identifier probe nextPhase state =
  case nextPhase of
    ProbeResolved outcome index ->
      unless
        ( disappearanceResolutionSubject outcome == projectedProbeSubject probe.projected
            && disappearanceResolutionControlIndex outcome == index
            && Map.keysSet probe.acceptedReports
              == Set.fromList (NonEmpty.toList (projectedProbeMembers probe.projected))
        )
        (Left (DisappearanceTerminalEvidenceIncomplete identifier))
    ProbeInvalidated (ProjectedCommandInvalidation reporter reason witness) _ -> do
      unless
        (Set.member reporter (Set.fromList (NonEmpty.toList (projectedProbeMembers probe.projected))))
        (Left (DisappearanceTerminalConflict identifier))
      when (reporter == state.localHerald)
        $ unless
          ( probe.invalidationIntention
              == Just
                (disappearanceInvalidationIntention identifier reporter reason witness)
          )
          (Left (DisappearanceTerminalConflict identifier))
    ProbeInvalidated (ProjectedLabelInvalidation _ suppliedObject) _ ->
      case controlledSubjectObject (projectedProbeSubject probe.projected) of
        Nothing ->
          Left
            ( DisappearanceLabelInvalidationSubjectNotControlled
                identifier
                (projectedProbeSubject probe.projected)
            )
        Just expectedObject ->
          unless
            (suppliedObject == expectedObject)
            ( Left
                ( DisappearanceLabelInvalidationObjectMismatch
                    identifier
                    suppliedObject
                    expectedObject
                )
            )
    ProbeAborted (ProjectedMembershipSuperseded successor) index ->
      unless
        (validMembershipSuccessor probe index successor)
        (Left (DisappearanceMembershipSuccessorMismatch identifier))
    ProbeAborted (ProjectedAdmissionPreparing admission) index ->
      unless
        (index == heraldAdmissionControlIndex admission && disappearanceProbeOpenControlIndex identifier < index)
        (Left (DisappearanceTerminalConflict identifier))
    _ -> Right ()

validMembershipSuccessor ::
  Probe ->
  ControlIndex ->
  HeraldMembershipGeneration ->
  Bool
validMembershipSuccessor probe index successor =
  case (heraldMembershipGenerationRetiredHeraldEpoch successor, heraldMembershipGenerationRetirementId successor) of
    (Just retired, Just resolution) ->
      Set.member retired capturedMembers
        && heraldMembershipGenerationPredecessor successor
          == Just (heraldMembershipGenerationId predecessor)
        && heraldMembershipGenerationRetirementControlIndex successor == Just index
        && either
          (const False)
          (== successor)
          (retireHeraldMembershipGeneration index resolution retired predecessor)
    _ -> False
  where
    predecessor = projectedProbeMembership probe.projected
    capturedMembers = Set.fromList (NonEmpty.toList (projectedProbeMembers probe.projected))

terminalFacts ::
  ProjectedDisappearanceTerminal ->
  (DisappearanceProbeId, ProbePhase)
terminalFacts terminal = case terminal of
  ProjectedDisappearanceInvalidated identifier cause index ->
    (identifier, ProbeInvalidated cause index)
  ProjectedDisappearanceResolved identifier outcome index ->
    (identifier, ProbeResolved outcome index)
  ProjectedDisappearanceAborted identifier cause index ->
    (identifier, ProbeAborted cause index)

installTerminal ::
  DisappearanceProbeId ->
  Probe ->
  ProbePhase ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition TerminalDisposition)
installTerminal identifier probe nextPhase state =
  PreparedDisappearanceTransition
    <$> prepareTransition
      ( \predecessor ->
          let subject = projectedProbeSubject probe.projected
              candidateUpdate =
                terminalCandidates
                  predecessor.localHerald
                  subject
                  nextPhase
                  predecessor.candidates
              nextProbe =
                probe
                  { phase = nextPhase,
                    invalidationIntention = Nothing,
                    reportIntention = Nothing,
                    resolveIntention = Nothing
                  }
           in Right
                ( predecessor
                    { candidates = candidateUpdate,
                      probes = Map.insert identifier nextProbe predecessor.probes,
                      probeByCoordinate = Map.delete (projectedProbeCoordinate probe.projected) predecessor.probeByCoordinate,
                      workLedger =
                        predecessor.workLedger
                          { terminalInstallations = predecessor.workLedger.terminalInstallations + 1,
                            candidateCreations =
                              predecessor.workLedger.candidateCreations
                                + terminalCandidateActivations predecessor.localHerald subject nextPhase predecessor.candidates
                          }
                    },
                  TerminalInstalled
                )
      )
      state

terminalCandidateActivations :: HeraldEpoch -> DisappearanceSubject -> ProbePhase -> Map DisappearanceSubject Candidate -> Word64
terminalCandidateActivations local subject phase candidates = case phase of
  ProbeAborted (ProjectedMembershipSuperseded successor) _
    | Set.member
        local
        (Set.fromList (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successor))) ->
        1
  ProbeAborted (ProjectedAdmissionPreparing _) _
    | Map.member subject candidates -> 1
  _ -> 0

terminalCandidates ::
  HeraldEpoch ->
  DisappearanceSubject ->
  ProbePhase ->
  Map DisappearanceSubject Candidate ->
  Map DisappearanceSubject Candidate
terminalCandidates local subject phase candidates = case phase of
  ProbeResolved {} -> Map.delete subject candidates
  ProbeInvalidated (ProjectedLabelInvalidation decision _) _ ->
    Map.adjust (parkForLabel decision) subject candidates
  ProbeInvalidated {} -> Map.adjust park subject candidates
  ProbeAborted ProjectedAuthorizedAbort _ -> Map.adjust park subject candidates
  -- Admission preparation retains only local candidates. The coordinator gates
  -- Opens until admission ends, then rechecks the current structural base.
  ProbeAborted (ProjectedAdmissionPreparing _) _ -> Map.adjust rearm subject candidates
  -- This is only the narrow leaf law: survivors retain eligibility, but no
  -- Open is prepared here. Increment 10 composes membership retirement and
  -- calls reevaluation only after the successor structural base supplies an
  -- explicit 'DisappearanceOpenContext'.
  ProbeAborted (ProjectedMembershipSuperseded successor) _
    | Set.member
        local
        (Set.fromList (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successor))) ->
        Map.alter (Just . maybe fresh rearm) subject candidates
    | otherwise -> Map.delete subject candidates
  ProbeCollecting -> candidates
  where
    fresh =
      Candidate
        { subject,
          eligible = True,
          awaitingLabelDecision = Nothing,
          openIntention = Nothing,
          projectedProbe = Nothing
        }
    park candidate =
      candidate
        { eligible = False,
          awaitingLabelDecision = Nothing,
          openIntention = Nothing,
          projectedProbe = Nothing
        }
    parkForLabel decision candidate =
      candidate
        { eligible = False,
          awaitingLabelDecision = Just decision,
          openIntention = Nothing,
          projectedProbe = Nothing
        }
    rearm candidate =
      candidate
        { eligible = True,
          awaitingLabelDecision = Nothing,
          openIntention = Nothing,
          projectedProbe = Nothing
        }

data LabelTerminalDisposition
  = LabelTerminalApplied
  | LabelTerminalDuplicate
  | LabelTerminalUnrelated
  deriving stock (Eq, Show)

-- | Apply the later label-owner transition which settles a label-caused
-- invalidation.  Only probes retaining the exact decision participate.  The
-- leaf never invents a candidate for an all-member participant: it consumes or
-- rearms only a candidate which this Herald already retained locally.
prepareProjectedLabelTerminal ::
  ProjectedLabelTerminal ->
  State ->
  Either
    DisappearanceProblem
    (PreparedDisappearanceTransition LabelTerminalDisposition)
prepareProjectedLabelTerminal terminal state =
  case Map.lookup decision state.labelTerminalsByDecision of
    Just retained
      | retained == terminal -> unchanged LabelTerminalDuplicate state
      | otherwise -> Left (DisappearanceLabelTerminalConflict decision)
    Nothing
      | Set.null affectedSubjects -> unchanged LabelTerminalUnrelated state
      | otherwise -> do
          mapM_ validateAffectedSubject (Set.toAscList affectedSubjects)
          mapM_ validateAffectedCandidate (Set.toAscList affectedSubjects)
          (nextCandidates, activations) <-
            applyLabelTerminalCandidates
              decision
              terminalOutcome
              affectedSubjects
              state.candidates
          PreparedDisappearanceTransition
            <$> prepareTransition
              ( \predecessor ->
                  Right
                    ( predecessor
                        { candidates = nextCandidates,
                          labelTerminalsByDecision =
                            Map.insert
                              decision
                              terminal
                              predecessor.labelTerminalsByDecision,
                          workLedger =
                            predecessor.workLedger
                              { candidateCreations =
                                  predecessor.workLedger.candidateCreations
                                    + activations,
                                labelTerminalApplications =
                                  predecessor.workLedger.labelTerminalApplications + 1
                              }
                        },
                      LabelTerminalApplied
                    )
              )
              state
  where
    decision = projectedLabelTerminalDecision terminal
    suppliedObject = projectedLabelTerminalObject terminal
    terminalOutcome = projectedLabelTerminalOutcome terminal
    affectedInvalidations =
      Set.fromList
        [ (projectedProbeSubject probe.projected, retainedObject)
        | probe <- Map.elems state.probes,
          ProbeInvalidated
            (ProjectedLabelInvalidation retainedDecision retainedObject)
            _ <-
            [probe.phase],
          retainedDecision == decision
        ]
        `Set.union` Set.fromList
          [ (subject, object)
          | ((retainedDecision, subject), object) <- Map.toAscList state.candidateLabelWaits,
            retainedDecision == decision
          ]
    affectedSubjects =
      Set.map fst affectedInvalidations
    validateAffectedSubject subject = do
      case controlledSubjectObject subject of
        Nothing ->
          Left (DisappearanceLabelTerminalSubjectNotControlled decision subject)
        Just expectedObject ->
          mapM_
            ( \retainedObject ->
                unless
                  ( suppliedObject == retainedObject
                      && retainedObject == expectedObject
                  )
                  ( Left
                      ( DisappearanceLabelTerminalObjectMismatch
                          decision
                          suppliedObject
                          retainedObject
                      )
                  )
            )
            [ retainedObject
            | (retainedSubject, retainedObject) <- Set.toAscList affectedInvalidations,
              retainedSubject == subject
            ]
    validateAffectedCandidate subject =
      case Map.lookup subject state.candidates of
        Nothing -> Right ()
        Just candidate ->
          unless
            (candidate.awaitingLabelDecision == Just decision)
            (Left (DisappearanceLabelTerminalCandidateConflict subject))

applyLabelTerminalCandidates ::
  LabelDecisionId ->
  ProjectedLabelTerminalOutcome ->
  Set DisappearanceSubject ->
  Map DisappearanceSubject Candidate ->
  Either DisappearanceProblem (Map DisappearanceSubject Candidate, Word64)
applyLabelTerminalCandidates decision outcome affected candidates = case outcome of
  ProjectedLabelDeleted _ ->
    Right (Set.foldr Map.delete candidates affected, 0)
  ProjectedLabelNotApplied ->
    Right (rearmSameSubjects affected candidates)
  ProjectedLabelReleased revision ->
    rekeyReleasedCandidates decision revision affected candidates

rearmSameSubjects ::
  Set DisappearanceSubject ->
  Map DisappearanceSubject Candidate ->
  (Map DisappearanceSubject Candidate, Word64)
rearmSameSubjects affected candidates =
  Set.foldl'
    ( \(retainedCandidates, activations) subject ->
        case Map.lookup subject retainedCandidates of
          Nothing -> (retainedCandidates, activations)
          Just candidate ->
            let successor = rearmedCandidate subject candidate
             in ( Map.insert subject successor retainedCandidates,
                  activations + if successor == candidate then 0 else 1
                )
    )
    (candidates, 0)
    affected

rekeyReleasedCandidates ::
  LabelDecisionId ->
  LabelRevision ->
  Set DisappearanceSubject ->
  Map DisappearanceSubject Candidate ->
  Either DisappearanceProblem (Map DisappearanceSubject Candidate, Word64)
rekeyReleasedCandidates decision revision affected candidates = do
  revised <-
    traverse
      ( \(subject, candidate) -> do
          revisedSubject <-
            either
              (const (Left (DisappearanceLabelTerminalSubjectNotControlled decision subject)))
              Right
              (reviseControlledDisappearanceSubject revision subject)
          Right (subject, revisedSubject, rearmedCandidate revisedSubject candidate)
      )
      retained
  let withoutAffected = Set.foldr Map.delete candidates affected
      revisedKeys = fmap (\(_, subject, _) -> subject) revised
  case firstDuplicate revisedKeys of
    Just subject -> Left (DisappearanceLabelTerminalCandidateConflict subject)
    Nothing -> Right ()
  case firstPresent withoutAffected revisedKeys of
    Just subject -> Left (DisappearanceLabelTerminalCandidateConflict subject)
    Nothing -> Right ()
  let nextCandidates =
        foldr
          (\(_, subject, candidate) -> Map.insert subject candidate)
          withoutAffected
          revised
      activations =
        fromIntegral
          ( length
              [ ()
              | (oldSubject, newSubject, successor) <- revised,
                Map.lookup oldSubject candidates /= Just successor
                  || oldSubject /= newSubject
              ]
          )
  Right (nextCandidates, activations)
  where
    retained =
      [ (subject, candidate)
      | subject <- Set.toAscList affected,
        Just candidate <- [Map.lookup subject candidates]
      ]

rearmedCandidate :: DisappearanceSubject -> Candidate -> Candidate
rearmedCandidate subject candidate =
  candidate
    { subject,
      eligible = True,
      awaitingLabelDecision = Nothing,
      openIntention = Nothing,
      projectedProbe = Nothing
    }

firstDuplicate :: (Ord value) => [value] -> Maybe value
firstDuplicate values = go Set.empty values
  where
    go _ [] = Nothing
    go seen (value : rest)
      | Set.member value seen = Just value
      | otherwise = go (Set.insert value seen) rest

firstPresent :: (Ord key) => Map key value -> [key] -> Maybe key
firstPresent values = go
  where
    go [] = Nothing
    go (key : rest)
      | Map.member key values = Just key
      | otherwise = go rest

controlledSubjectObject :: DisappearanceSubject -> Maybe GlobalObjectId
controlledSubjectObject subject = case disappearanceSubjectView subject of
  ControlledPredefinedSubjectView object _ _ -> Just object
  RegularSortDefinitionSubjectView {} -> Nothing

requireCollecting :: DisappearanceProbeId -> State -> Either DisappearanceProblem Probe
requireCollecting identifier state = do
  probe <- requireProbe identifier state
  requireCollectingPhase identifier probe
  Right probe

requireProbe :: DisappearanceProbeId -> State -> Either DisappearanceProblem Probe
requireProbe identifier state =
  maybe
    (Left (DisappearanceProbeMissing identifier))
    Right
    (Map.lookup identifier state.probes)

requireCollectingPhase :: DisappearanceProbeId -> Probe -> Either DisappearanceProblem ()
requireCollectingPhase identifier probe = case probe.phase of
  ProbeCollecting -> Right ()
  terminal -> Left (DisappearanceProbeNotCollecting identifier (phaseView terminal))

validateMarkerForProbe ::
  ProjectedDisappearanceProbe ->
  DisappearancePublicationMarker ->
  Either DisappearanceProblem ()
validateMarkerForProbe projected marker = do
  unless
    (disappearancePublicationMarkerProbe marker == projectedProbeId projected)
    ( Left
        ( DisappearancePublicationMarkerProbeMismatch
            (disappearancePublicationMarkerProbe marker)
            (projectedProbeId projected)
        )
    )
  unless
    (disappearancePublicationMarkerCoordinate marker == projectedProbeCoordinate projected)
    ( Left
        ( DisappearancePublicationMarkerCoordinateMismatch
            (disappearancePublicationMarkerCoordinate marker)
            (projectedProbeCoordinate projected)
        )
    )

unchanged :: output -> State -> Either error (PreparedDisappearanceTransition output)
unchanged output state =
  PreparedDisappearanceTransition
    <$> prepareTransition (\predecessor -> Right (predecessor, output)) state

changedProbe ::
  DisappearanceProbeId ->
  (Probe -> Probe) ->
  (DisappearanceWorkLedger -> DisappearanceWorkLedger) ->
  output ->
  State ->
  Either DisappearanceProblem (PreparedDisappearanceTransition output)
changedProbe identifier update updateLedger output state =
  PreparedDisappearanceTransition
    <$> prepareTransition
      ( \predecessor ->
          Right
            ( updateProbe identifier update (incrementLedger updateLedger predecessor),
              output
            )
      )
      state

updateProbe :: DisappearanceProbeId -> (Probe -> Probe) -> State -> State
updateProbe identifier update state =
  state {probes = Map.adjust update identifier state.probes}

incrementLedger ::
  (DisappearanceWorkLedger -> DisappearanceWorkLedger) ->
  State ->
  State
incrementLedger update state = state {workLedger = update state.workLedger}

data DisappearanceInvariantViolation
  = DisappearanceCandidateKeyMismatch DisappearanceSubject
  | DisappearanceCandidateEligibilityInvariant DisappearanceSubject
  | DisappearanceCandidateLabelWaitInvariant
      DisappearanceSubject
      (Maybe LabelDecisionId)
      [LabelDecisionId]
  | DisappearanceCandidateIntentionInvariant DisappearanceSubject
  | DisappearanceCandidateNamesMissingProbe DisappearanceSubject DisappearanceProbeId
  | DisappearanceCandidateProbeSubjectMismatch DisappearanceSubject DisappearanceProbeId
  | DisappearanceProbeKeyMismatch DisappearanceProbeId
  | DisappearanceProbeEvidenceSubjectMismatch DisappearanceProbeId
  | DisappearanceMultipleActiveSubjectProbes DisappearanceSubject
  | DisappearanceProjectedCoordinateInvariant DisappearanceProbeId
  | DisappearanceMatchingWriteGateInvariant DisappearanceProbeId
  | DisappearanceProbeCoordinateIndexMismatch DisappearanceSubjectMembershipCoordinate
  | DisappearanceActiveCoordinateNamesMissingProbe DisappearanceProbeId
  | DisappearanceProbeLocalMemberMissing DisappearanceProbeId HeraldEpoch
  | DisappearanceOutgoingMarkerSetIncomplete DisappearanceProbeId
  | DisappearanceOutgoingMarkerInvalid DisappearanceProbeId HeraldEpoch
  | DisappearanceIncomingMarkerSourceInvalid DisappearanceProbeId HeraldEpoch
  | DisappearanceIncomingMarkerInvalid DisappearanceProbeId HeraldEpoch
  | DisappearanceIncomingAlignmentKeyMismatch DisappearanceProbeId AlignmentSubscriptionId
  | DisappearanceIncomingAlignmentDestinationInvalid DisappearanceProbeId AlignmentSubscriptionId
  | DisappearanceIncomingAlignmentSourceInvalid DisappearanceProbeId AlignmentSubscriptionId
  | DisappearanceIncomingAlignmentMarkerInvalid DisappearanceProbeId AlignmentSubscriptionId
  | DisappearanceOutgoingAlignmentMarkerInvalid DisappearanceProbeId AlignmentSubscriptionId
  | DisappearanceBlockerInvariant DisappearanceProbeId DisappearanceBlockerClass
  | DisappearanceReportWithoutCompleteEvidence DisappearanceProbeId
  | DisappearanceReportInvariant DisappearanceProbeId
  | DisappearanceReportIntentionInvariant DisappearanceProbeId
  | DisappearanceAcceptedReportInvariant DisappearanceProbeId HeraldEpoch
  | DisappearanceResolveWithoutCompleteReports DisappearanceProbeId
  | DisappearanceResolveIntentionInvariant DisappearanceProbeId
  | DisappearanceInvalidationIntentionInvariant DisappearanceProbeId
  | DisappearanceTerminalOutcomeInvariant DisappearanceProbeId
  | DisappearanceLabelInvalidationSubjectInvariant
      DisappearanceProbeId
      DisappearanceSubject
  | DisappearanceLabelInvalidationObjectInvariant
      DisappearanceProbeId
      GlobalObjectId
      GlobalObjectId
  | DisappearanceTerminalRetainsIntention DisappearanceProbeId
  | DisappearanceTerminalRetainsActiveCoordinate DisappearanceProbeId
  | DisappearanceLabelTerminalKeyMismatch LabelDecisionId
  | DisappearanceLabelTerminalWithoutInvalidatedProbe LabelDecisionId
  | DisappearanceLabelTerminalSubjectInvariant LabelDecisionId DisappearanceSubject
  | DisappearanceLabelTerminalObjectInvariant
      LabelDecisionId
      GlobalObjectId
      GlobalObjectId
  deriving stock (Eq, Show)

validateState :: State -> Either DisappearanceInvariantViolation ()
validateState state = do
  mapM_ validateCandidate (Map.toAscList state.candidates)
  mapM_ validateProbe (Map.toAscList state.probes)
  mapM_ validateCoordinate (Map.toAscList state.probeByCoordinate)
  mapM_ validateLabelTerminal (Map.toAscList state.labelTerminalsByDecision)
  where
    validateCandidate (key, candidate) = do
      unless (key == candidate.subject) (Left (DisappearanceCandidateKeyMismatch key))
      let pendingLabelDecisions =
            Set.toAscList (pendingLabelDecisionsForSubject key state)
      unless
        ( case pendingLabelDecisions of
            [] -> candidate.awaitingLabelDecision == Nothing
            [decision] ->
              candidate.awaitingLabelDecision == Just decision
                && not candidate.eligible
            _ -> False
        )
        ( Left
            ( DisappearanceCandidateLabelWaitInvariant
                key
                candidate.awaitingLabelDecision
                pendingLabelDecisions
            )
        )
      when
        ( not candidate.eligible
            && (candidate.openIntention /= Nothing || candidate.projectedProbe /= Nothing)
        )
        (Left (DisappearanceCandidateEligibilityInvariant key))
      case candidate.openIntention of
        Nothing -> Right ()
        Just intention ->
          unless
            ( candidate.eligible
                && candidate.projectedProbe == Nothing
                && disappearanceOpenIntentionSubject intention == key
            )
            (Left (DisappearanceCandidateIntentionInvariant key))
      case candidate.projectedProbe of
        Nothing -> Right ()
        Just identifier -> case Map.lookup identifier state.probes of
          Nothing -> Left (DisappearanceCandidateNamesMissingProbe key identifier)
          Just probe ->
            unless
              ( candidate.eligible
                  && probe.phase == ProbeCollecting
                  && projectedProbeSubject probe.projected == key
              )
              (Left (DisappearanceCandidateProbeSubjectMismatch key identifier))
      case activeProbeForSubject key state of
        Just identifier
          | candidate.eligible ->
              unless
                (candidate.projectedProbe == Just identifier)
                (Left (DisappearanceCandidateProbeSubjectMismatch key identifier))
        _ -> Right ()

    validateProbe (identifier, probe) = do
      unless
        (identifier == projectedProbeId probe.projected)
        (Left (DisappearanceProbeKeyMismatch identifier))
      unless
        ( disappearanceEvidenceSnapshotSubject probe.evidenceSnapshot
            == projectedProbeSubject probe.projected
        )
        (Left (DisappearanceProbeEvidenceSubjectMismatch identifier))
      let coordinate = projectedProbeCoordinate probe.projected
          members = Set.fromList (NonEmpty.toList (projectedProbeMembers probe.projected))
          remotes = Set.delete state.localHerald members
          activeSubjectProbes =
            [ candidateIdentifier
            | (candidateIdentifier, candidateProbe) <- Map.toAscList state.probes,
              candidateProbe.phase == ProbeCollecting,
              projectedProbeSubject candidateProbe.projected
                == projectedProbeSubject probe.projected
            ]
          expectedCoordinate =
            disappearanceSubjectMembershipCoordinate
              (projectedProbeSubject probe.projected)
              (projectedProbeMembership probe.projected)
      when
        (probe.phase == ProbeCollecting && activeSubjectProbes /= [identifier])
        (Left (DisappearanceMultipleActiveSubjectProbes (projectedProbeSubject probe.projected)))
      unless
        (coordinate == expectedCoordinate)
        (Left (DisappearanceProjectedCoordinateInvariant identifier))
      unless
        ( matchingWriteGateProbe probe.matchingWriteGate == identifier
            && matchingWriteGateSubject probe.matchingWriteGate
              == projectedProbeSubject probe.projected
            && matchingWriteGateCoordinate probe.matchingWriteGate == coordinate
            && matchingWriteGateLocalCut probe.matchingWriteGate
              == probe.localPublicationCut
        )
        (Left (DisappearanceMatchingWriteGateInvariant identifier))
      unless
        (Set.member state.localHerald members)
        (Left (DisappearanceProbeLocalMemberMissing identifier state.localHerald))
      case probe.phase of
        ProbeCollecting ->
          unless
            (Map.lookup coordinate state.probeByCoordinate == Just identifier)
            (Left (DisappearanceProbeCoordinateIndexMismatch coordinate))
        _ ->
          when
            (Map.lookup coordinate state.probeByCoordinate == Just identifier)
            (Left (DisappearanceTerminalRetainsActiveCoordinate identifier))
      unless
        ( if probe.publicationAssignmentsInstalled
            then Map.keysSet probe.outgoingPublicationMarkers == remotes
            else Map.null probe.outgoingPublicationMarkers
        )
        (Left (DisappearanceOutgoingMarkerSetIncomplete identifier))
      mapM_
        ( \(destination, marker) ->
            let direction = disappearancePublicationMarkerDirection marker
             in unless
                  ( disappearancePublicationMarkerProbe marker == identifier
                      && disappearancePublicationMarkerCoordinate marker == coordinate
                      && streamDirectionSource direction == state.localHerald
                      && streamDirectionDestination direction == destination
                      && Set.member destination remotes
                  )
                  (Left (DisappearanceOutgoingMarkerInvalid identifier destination))
        )
        (Map.toAscList probe.outgoingPublicationMarkers)
      mapM_
        ( \(source, cell) -> do
            unless
              (Set.member source remotes)
              (Left (DisappearanceIncomingMarkerSourceInvalid identifier source))
            let marker = cell.marker
                direction = disappearancePublicationMarkerDirection marker
            unless
              ( disappearancePublicationMarkerProbe marker == identifier
                  && disappearancePublicationMarkerCoordinate marker == coordinate
                  && streamDirectionSource direction == source
                  && streamDirectionDestination direction == state.localHerald
              )
              (Left (DisappearanceIncomingMarkerInvalid identifier source))
        )
        (Map.toAscList probe.incomingPublicationMarkers)
      mapM_
        ( \(subscription, cell) -> do
            unless
              (subscription == incomingAlignmentCutSubscription cell.capture)
              (Left (DisappearanceIncomingAlignmentKeyMismatch identifier subscription))
            unless
              (alignmentSubscriptionIdDestinationHerald subscription == state.localHerald)
              (Left (DisappearanceIncomingAlignmentDestinationInvalid identifier subscription))
            unless
              (Set.member (incomingAlignmentCutSource cell.capture) members)
              (Left (DisappearanceIncomingAlignmentSourceInvalid identifier subscription))
            case cell.marker of
              Nothing ->
                when
                  (cell.completedThrough /= Nothing)
                  (Left (DisappearanceIncomingAlignmentMarkerInvalid identifier subscription))
              Just marker -> do
                unless
                  ( disappearanceAlignmentMarkerProbe marker == identifier
                      && disappearanceAlignmentMarkerSubscription marker == subscription
                  )
                  (Left (DisappearanceIncomingAlignmentMarkerInvalid identifier subscription))
                case cell.completedThrough of
                  Nothing -> Right ()
                  Just completed ->
                    unless
                      (completed >= disappearanceAlignmentMarkerThroughRevision marker)
                      (Left (DisappearanceIncomingAlignmentMarkerInvalid identifier subscription))
        )
        (Map.toAscList probe.incomingAlignmentCuts)
      mapM_
        ( \(subscription, marker) ->
            unless
              ( disappearanceAlignmentMarkerProbe marker == identifier
                  && disappearanceAlignmentMarkerSubscription marker == subscription
                  && Set.member
                    (alignmentSubscriptionIdDestinationHerald subscription)
                    members
              )
              (Left (DisappearanceOutgoingAlignmentMarkerInvalid identifier subscription))
        )
        (Map.toAscList probe.outgoingAlignmentMarkers)
      mapM_
        ( \blocker ->
            either
              ( const
                  ( Left
                      ( DisappearanceBlockerInvariant
                          identifier
                          (disappearanceBlockerClass blocker)
                      )
                  )
              )
              Right
              (validateBlockerForSubject (projectedProbeSubject probe.projected) blocker)
        )
        (Set.toAscList probe.blockers)
      when
        (probe.report /= Nothing && not (probeCutComplete state probe))
        (Left (DisappearanceReportWithoutCompleteEvidence identifier))
      case probe.report of
        Nothing -> Right ()
        Just report ->
          let claim = localAbsenceReportClaim report
              alignmentEvidence = localAbsenceReportAlignmentEvidence report
              alignmentBySubscription =
                Map.fromList
                  [ (completedIncomingAlignmentEvidenceSubscription evidence, evidence)
                  | evidence <- alignmentEvidence
                  ]
              alignmentEvidenceValid =
                Map.size alignmentBySubscription == length alignmentEvidence
                  && Map.keysSet alignmentBySubscription
                    == Map.keysSet probe.incomingAlignmentCuts
                  && all
                    ( \(subscription, evidence) ->
                        case Map.lookup subscription probe.incomingAlignmentCuts of
                          Nothing -> False
                          Just cell ->
                            completedIncomingAlignmentEvidenceProbe evidence == identifier
                              && completedIncomingAlignmentEvidenceSource evidence
                                == incomingAlignmentCutSource cell.capture
                              && case cell.marker of
                                Nothing -> False
                                Just marker ->
                                  completedIncomingAlignmentEvidenceThroughRevision evidence
                                    == disappearanceAlignmentMarkerThroughRevision marker
                    )
                    (Map.toAscList alignmentBySubscription)
           in unless
                ( localAbsenceReportProbe report == identifier
                    && disappearanceEvidenceClaimProbeId claim == identifier
                    && disappearanceEvidenceClaimCoordinate claim == coordinate
                    && disappearanceEvidenceClaimReporter claim == state.localHerald
                    && disappearanceAbsenceAttestationSubject
                      (localAbsenceReportAttestation report)
                      == projectedProbeSubject probe.projected
                    && alignmentEvidenceValid
                )
                (Left (DisappearanceReportInvariant identifier))
      case probe.reportIntention of
        Nothing -> Right ()
        Just intention ->
          unless
            (probe.report == Just intention && probe.phase == ProbeCollecting)
            (Left (DisappearanceReportIntentionInvariant identifier))
      mapM_
        ( \(reporter, claim) ->
            unless
              ( disappearanceEvidenceClaimProbeId claim == identifier
                  && disappearanceEvidenceClaimCoordinate claim == coordinate
                  && disappearanceEvidenceClaimReporter claim == reporter
                  && Set.member reporter members
              )
              (Left (DisappearanceAcceptedReportInvariant identifier reporter))
        )
        (Map.toAscList probe.acceptedReports)
      when
        ( probe.resolveIntention /= Nothing
            && Map.keysSet probe.acceptedReports /= members
        )
        (Left (DisappearanceResolveWithoutCompleteReports identifier))
      case probe.resolveIntention of
        Nothing -> Right ()
        Just intention ->
          unless
            ( disappearanceResolveProbe intention == identifier
                && disappearanceResolveClaims intention
                  == disappearanceResolveClaims
                    (disappearanceResolveIntention identifier (Map.elems probe.acceptedReports))
                && probe.phase == ProbeCollecting
            )
            (Left (DisappearanceResolveIntentionInvariant identifier))
      case probe.invalidationIntention of
        Nothing -> Right ()
        Just intention ->
          unless
            ( disappearanceInvalidationProbe intention == identifier
                && disappearanceInvalidationReporter intention == state.localHerald
                && probe.phase == ProbeCollecting
            )
            (Left (DisappearanceInvalidationIntentionInvariant identifier))
      case probe.phase of
        ProbeResolved outcome index ->
          unless
            ( disappearanceResolutionSubject outcome == projectedProbeSubject probe.projected
                && disappearanceResolutionControlIndex outcome == index
                && Map.keysSet probe.acceptedReports == members
            )
            (Left (DisappearanceTerminalOutcomeInvariant identifier))
        ProbeInvalidated (ProjectedCommandInvalidation reporter _ _) _ ->
          unless
            (Set.member reporter members)
            (Left (DisappearanceTerminalOutcomeInvariant identifier))
        ProbeInvalidated (ProjectedLabelInvalidation _ suppliedObject) _ ->
          case controlledSubjectObject (projectedProbeSubject probe.projected) of
            Nothing ->
              Left
                ( DisappearanceLabelInvalidationSubjectInvariant
                    identifier
                    (projectedProbeSubject probe.projected)
                )
            Just expectedObject ->
              unless
                (suppliedObject == expectedObject)
                ( Left
                    ( DisappearanceLabelInvalidationObjectInvariant
                        identifier
                        suppliedObject
                        expectedObject
                    )
                )
        ProbeAborted (ProjectedMembershipSuperseded successor) index ->
          unless
            (validMembershipSuccessor probe index successor)
            (Left (DisappearanceTerminalOutcomeInvariant identifier))
        ProbeAborted (ProjectedAdmissionPreparing admission) index ->
          unless
            (index == heraldAdmissionControlIndex admission && disappearanceProbeOpenControlIndex identifier < index)
            (Left (DisappearanceTerminalOutcomeInvariant identifier))
        _ -> Right ()
      case probe.phase of
        ProbeCollecting -> Right ()
        _ ->
          when
            ( probe.invalidationIntention /= Nothing
                || probe.reportIntention /= Nothing
                || probe.resolveIntention /= Nothing
            )
            (Left (DisappearanceTerminalRetainsIntention identifier))

    validateCoordinate (coordinate, identifier) =
      case Map.lookup identifier state.probes of
        Nothing -> Left (DisappearanceActiveCoordinateNamesMissingProbe identifier)
        Just probe ->
          unless
            ( projectedProbeCoordinate probe.projected == coordinate
                && probe.phase == ProbeCollecting
            )
            (Left (DisappearanceProbeCoordinateIndexMismatch coordinate))

    validateLabelTerminal (decision, terminal) = do
      unless
        (decision == projectedLabelTerminalDecision terminal)
        (Left (DisappearanceLabelTerminalKeyMismatch decision))
      let object = projectedLabelTerminalObject terminal
          invalidations =
            Set.fromList
              [ (projectedProbeSubject probe.projected, retainedObject)
              | probe <- Map.elems state.probes,
                ProbeInvalidated
                  (ProjectedLabelInvalidation retainedDecision retainedObject)
                  _ <-
                  [probe.phase],
                retainedDecision == decision
              ]
              `Set.union` Set.fromList
                [ (subject, retainedObject)
                | ((retainedDecision, subject), retainedObject) <- Map.toAscList state.candidateLabelWaits,
                  retainedDecision == decision
                ]
      when
        (Set.null invalidations)
        (Left (DisappearanceLabelTerminalWithoutInvalidatedProbe decision))
      mapM_
        ( \(subject, retainedObject) -> case controlledSubjectObject subject of
            Nothing ->
              Left (DisappearanceLabelTerminalSubjectInvariant decision subject)
            Just expectedObject ->
              unless
                (object == retainedObject && retainedObject == expectedObject)
                ( Left
                    ( DisappearanceLabelTerminalObjectInvariant
                        decision
                        object
                        retainedObject
                    )
                )
        )
        (Set.toAscList invalidations)

probeCutComplete :: State -> Probe -> Bool
probeCutComplete state probe =
  probe.localPublicationCutComplete
    && probe.publicationAssignmentsInstalled
    && Map.keysSet probe.outgoingPublicationMarkers == remoteMembers
    && Map.keysSet probe.incomingPublicationMarkers == remoteMembers
    && all (.completed) (Map.elems probe.incomingPublicationMarkers)
    && all alignmentComplete (Map.elems probe.incomingAlignmentCuts)
  where
    remoteMembers =
      Set.delete
        state.localHerald
        (Set.fromList (NonEmpty.toList (projectedProbeMembers probe.projected)))
    alignmentComplete cell = case (cell.marker, cell.completedThrough) of
      (Just marker, Just completed) ->
        completed >= disappearanceAlignmentMarkerThroughRevision marker
      _ -> False
