{-# LANGUAGE ImportQualifiedPost #-}

-- | Stateless pure composition for the disappearance leaf.
--
-- The coordinator owns no facts. It commits only opaque, owner-prepared leaf
-- and peer-stream transitions and returns the resulting states for atomic
-- installation by the live coordinator and owner-level property harness.
module Eclips.Herald.UseCase.Disappearance
  ( DisappearanceCoordinationProblem (..),
    CandidateCoordination,
    candidateCoordinationTakeDisposition,
    candidateCoordinationReevaluationDisposition,
    coordinateLocalTakeCandidate,
    coordinateCandidateReevaluation,
    ProjectedOpenCoordination,
    projectedOpenCoordinationDisposition,
    projectedOpenCoordinationPublicationAssignments,
    projectedOpenCoordinationAlignmentMarkers,
    projectedOpenCoordinationFrontierAdvances,
    projectedOpenCoordinationDispatchTickets,
    coordinateProjectedOpen,
    PublicationMarkerReceiptCoordination,
    publicationMarkerReceiptStreamResult,
    publicationMarkerReceiptAdmissions,
    coordinatePublicationMarkerReceipt,
    PublicationMarkerCompletionCoordination (..),
    coordinatePublicationMarkerCompletion,
    coordinateLocalPublicationCutCompletion,
    coordinateAlignmentMarkerReceipt,
    coordinateAlignmentMarkerCompletion,
    coordinateAlignmentMutation,
    coordinateEvidenceObservation,
    coordinateBlockerObservation,
    coordinateMatchingPublicationObservation,
    coordinateLocalAbsenceReport,
    coordinateProjectedReport,
    coordinateProjectedTerminal,
    ProjectedControlledResolutionCoordination,
    projectedControlledResolutionCoordinationDisposition,
    projectedControlledResolutionCoordinationSummary,
    coordinateProjectedControlledResolution,
    ProjectedRegularResolutionCoordination,
    projectedRegularResolutionCoordinationDisposition,
    projectedRegularResolutionCoordinationSummary,
    coordinateProjectedRegularResolution,
    replayProjectedDisappearanceResolution,
    coordinateProjectedLabelTerminal,
  )
where

import Control.Monad (foldM, unless)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Alignment (HeraldPublicationPrefix, StoreRevision)
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceClaim,
    DisappearanceProbeId,
    DisappearanceResolutionOutcomeView
      ( ControlledDisappearanceResolved,
        RegularSortDefinitionRetired
      ),
    DisappearanceSubject,
    disappearanceResolutionOutcomeView,
  )
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Herald.Disappearance.Evidence
  ( DisappearanceEvidenceSnapshot,
  )
import Eclips.Herald.Disappearance.Protocol
  ( DisappearanceAlignmentMarker,
    DisappearanceBlockerWitness,
    DisappearanceCandidateObservation,
    DisappearanceInvalidationIntention,
    DisappearanceOpenContext,
    DisappearancePublicationMarker,
    MatchingPublicationObservation,
    ProjectedDisappearanceProbe,
    ProjectedDisappearanceTerminal (..),
    ProjectedLabelTerminal,
    disappearanceCandidateSubject,
    disappearancePublicationMarkerDigest,
    disappearancePublicationMarkerDirection,
    disappearancePublicationMarkerSequence,
    projectedProbeId,
  )
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.EffectBatch (EffectBatch)
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch)
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerStream
  ( PeerDispatchTicket,
    ReceiveProgress (..),
    ReceiveResult,
    SequencedItem,
    StreamDirection,
    StreamSequence,
    StreamWatermarks,
    sequencedItemDigest,
    sequencedItemDirection,
    sequencedItemPayload,
    sequencedItemSequence,
    streamCompletedPrefix,
    streamDirectionDestination,
    streamPrefixThrough,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Startup.State (HeraldState, startupGenesis)
import Eclips.Herald.UseCase.ControlledRemoval qualified as ControlledRemoval
import Eclips.Herald.UseCase.RegularRetirement qualified as RegularRetirement

data DisappearanceCoordinationProblem
  = DisappearanceLeafProblem Disappearance.DisappearanceProblem
  | DisappearancePeerStreamProblem PeerStream.PeerStreamProblem
  | DisappearanceCoordinatorOwnerMismatch HeraldEpoch HeraldEpoch
  | DisappearancePublicationAssignmentCardinality HeraldEpoch
  | DisappearancePublicationMarkerEnvelopeMismatch StreamDirection StreamSequence
  | DisappearancePublicationMarkerNotRetained StreamDirection StreamSequence
  | DisappearanceControlledRemovalProblem
      ControlledRemoval.ControlledRemovalProblem
  | DisappearanceControlledResolutionRequiresOwnerCoordination DisappearanceProbeId
  | DisappearanceRegularResolutionRequiresOwnerCoordination DisappearanceProbeId
  | DisappearanceControlledRemovalOwnerMismatch HeraldEpoch HeraldEpoch
  | DisappearanceControlledRemovalAuthorityMissing
  | DisappearanceControlledRemovalDispositionMismatch
  | DisappearanceRegularRetirementProblem
      RegularRetirement.RegularRetirementProblem
  | DisappearanceRegularRetirementOwnerMismatch HeraldEpoch HeraldEpoch
  | DisappearanceRegularRetirementAuthorityMissing
  | DisappearanceRegularRetirementDispositionMismatch
  | DisappearanceImportedResolutionMissing
  deriving stock (Eq, Show)

data CandidateCoordination
  = CandidateCoordination
      Disappearance.CandidateDisposition
      (Maybe Disappearance.CandidateDisposition)
  deriving stock (Eq, Show)

candidateCoordinationTakeDisposition ::
  CandidateCoordination -> Disappearance.CandidateDisposition
candidateCoordinationTakeDisposition (CandidateCoordination disposition _) = disposition

candidateCoordinationReevaluationDisposition ::
  CandidateCoordination -> Maybe Disappearance.CandidateDisposition
candidateCoordinationReevaluationDisposition (CandidateCoordination _ disposition) = disposition

-- | Record one accepted application-visible local take, then optionally drive
-- just that subject against a structural-base-ready membership observation.
coordinateLocalTakeCandidate ::
  DisappearanceCandidateObservation ->
  Maybe DisappearanceOpenContext ->
  Disappearance.State ->
  Either
    DisappearanceCoordinationProblem
    (Disappearance.State, CandidateCoordination)
coordinateLocalTakeCandidate observation maybeContext state = do
  preparedTake <-
    mapLeaf (Disappearance.prepareLocalTakeCandidate observation state)
  let (afterTake, takeDisposition) =
        Disappearance.commitDisappearanceTransition preparedTake
  case maybeContext of
    Nothing ->
      Right (afterTake, CandidateCoordination takeDisposition Nothing)
    Just context -> do
      preparedReevaluation <-
        mapLeaf
          ( Disappearance.prepareCandidateReevaluation
              context
              (disappearanceCandidateSubject observation)
              afterTake
          )
      let (successor, reevaluationDisposition) =
            Disappearance.commitDisappearanceTransition preparedReevaluation
      Right
        ( successor,
          CandidateCoordination takeDisposition (Just reevaluationDisposition)
        )

coordinateCandidateReevaluation ::
  DisappearanceOpenContext ->
  DisappearanceSubject ->
  Disappearance.State ->
  Either
    DisappearanceCoordinationProblem
    (Disappearance.State, Disappearance.CandidateDisposition)
coordinateCandidateReevaluation context subject state =
  commitLeaf
    (Disappearance.prepareCandidateReevaluation context subject state)

data ProjectedOpenCoordination
  = ProjectedOpenCoordination
      Disappearance.ProjectedOpenDisposition
      (Map HeraldEpoch (SequencedItem DisappearancePublicationMarker))
      [DisappearanceAlignmentMarker]
      (Map HeraldEpoch (StreamDirection, StreamSequence))
      [PeerDispatchTicket]
  deriving stock (Eq, Show)

projectedOpenCoordinationDisposition ::
  ProjectedOpenCoordination -> Disappearance.ProjectedOpenDisposition
projectedOpenCoordinationDisposition
  (ProjectedOpenCoordination disposition _ _ _ _) = disposition

projectedOpenCoordinationPublicationAssignments ::
  ProjectedOpenCoordination ->
  Map HeraldEpoch (SequencedItem DisappearancePublicationMarker)
projectedOpenCoordinationPublicationAssignments
  (ProjectedOpenCoordination _ assignments _ _ _) = assignments

projectedOpenCoordinationAlignmentMarkers ::
  ProjectedOpenCoordination -> [DisappearanceAlignmentMarker]
projectedOpenCoordinationAlignmentMarkers
  (ProjectedOpenCoordination _ _ markers _ _) = markers

projectedOpenCoordinationFrontierAdvances ::
  ProjectedOpenCoordination -> Map HeraldEpoch (StreamDirection, StreamSequence)
projectedOpenCoordinationFrontierAdvances
  (ProjectedOpenCoordination _ _ _ advances _) = advances

projectedOpenCoordinationDispatchTickets ::
  ProjectedOpenCoordination -> [PeerDispatchTicket]
projectedOpenCoordinationDispatchTickets
  (ProjectedOpenCoordination _ _ _ _ tickets) = tickets

-- | Install one projected probe and allocate exactly one sequence-bound marker
-- in every captured non-self peer stream as one pure coordination step.
coordinateProjectedOpen ::
  ProjectedDisappearanceProbe ->
  DisappearanceEvidenceSnapshot ->
  Disappearance.State ->
  PeerStream.State DisappearancePublicationMarker ->
  Either
    DisappearanceCoordinationProblem
    ( Disappearance.State,
      PeerStream.State DisappearancePublicationMarker,
      ProjectedOpenCoordination
    )
coordinateProjectedOpen projected evidenceSnapshot leaf stream = do
  let leafOwner = Disappearance.disappearanceLocalHerald leaf
      streamOwner = PeerStream.peerStreamLocalEpoch stream
  if leafOwner /= streamOwner
    then Left (DisappearanceCoordinatorOwnerMismatch leafOwner streamOwner)
    else Right ()
  preparedOpen <-
    mapLeaf
      ( Disappearance.prepareProjectedOpen
          projected
          evidenceSnapshot
          leaf
      )
  let openOutput = Disappearance.preparedDisappearanceOutput preparedOpen
      (withProjection, _) =
        Disappearance.commitDisappearanceTransition preparedOpen
  case openOutput of
    Disappearance.ProjectedOpenOutput
      Disappearance.ProjectedOpenDuplicate
      _
      _ ->
        Right
          ( withProjection,
            stream,
            ProjectedOpenCoordination
              Disappearance.ProjectedOpenDuplicate
              Map.empty
              []
              Map.empty
              []
          )
    Disappearance.ProjectedOpenOutput
      Disappearance.ProjectedOpenInstalled
      markerItems
      alignmentMarkers -> do
        preparedEnqueue <-
          mapPeer
            ( PeerStream.prepareSequenceBoundEnqueue
                (fmap (:| []) markerItems)
                stream
            )
        assignments <-
          traverse
            exactlyOneAssignment
            (PeerStream.preparedEnqueueAssignments preparedEnqueue)
        let markers = fmap sequencedItemPayload (Map.elems assignments)
        preparedAssignments <-
          mapLeaf
            ( Disappearance.preparePublicationMarkerAssignments
                (projectedProbeId projected)
                markers
                withProjection
            )
        let (successorLeaf, ()) =
              Disappearance.commitDisappearanceTransition preparedAssignments
            (successorStream, _) = PeerStream.commitEnqueue preparedEnqueue
            output =
              ProjectedOpenCoordination
                Disappearance.ProjectedOpenInstalled
                assignments
                alignmentMarkers
                (PeerStream.preparedEnqueueFrontierAdvances preparedEnqueue)
                (PeerStream.preparedEnqueueDispatchTickets preparedEnqueue)
        Right (successorLeaf, successorStream, output)

data PublicationMarkerReceiptCoordination
  = PublicationMarkerReceiptCoordination
      ReceiveResult
      [(SequencedItem DisappearancePublicationMarker, Disappearance.MarkerAdmissionDisposition)]
  deriving stock (Eq, Show)

publicationMarkerReceiptStreamResult ::
  PublicationMarkerReceiptCoordination -> ReceiveResult
publicationMarkerReceiptStreamResult
  (PublicationMarkerReceiptCoordination result _) = result

publicationMarkerReceiptAdmissions ::
  PublicationMarkerReceiptCoordination ->
  [(SequencedItem DisappearancePublicationMarker, Disappearance.MarkerAdmissionDisposition)]
publicationMarkerReceiptAdmissions
  (PublicationMarkerReceiptCoordination _ admissions) = admissions

-- | Admit one destination-side item through the generic ordered peer-stream
-- owner before exposing any newly contiguous disappearance marker to the leaf.
-- A collecting or not-yet-projected marker remains pending until the separate
-- completion coordinator proves that the stream's completed prefix reaches it;
-- a marker for an already-terminal probe is obsolete and completes immediately.
coordinatePublicationMarkerReceipt ::
  SequencedItem DisappearancePublicationMarker ->
  Disappearance.State ->
  PeerStream.State DisappearancePublicationMarker ->
  Either
    DisappearanceCoordinationProblem
    ( Disappearance.State,
      PeerStream.State DisappearancePublicationMarker,
      PublicationMarkerReceiptCoordination
    )
coordinatePublicationMarkerReceipt item leaf stream = do
  validateCoordinatedOwners leaf stream
  validateSequencedMarker item
  candidate <- mapPeer (PeerStream.prepareReceiveCandidate item stream)
  (successorLeaf, reverseAdmissions, progress) <-
    foldM
      admitContiguousMarker
      (leaf, [], Map.empty)
      (PeerStream.preparedContiguousItems candidate)
  preparedReceive <- mapPeer (PeerStream.finalizeReceive progress candidate)
  let (successorStream, result) = PeerStream.commitReceive preparedReceive
  Right
    ( successorLeaf,
      successorStream,
      PublicationMarkerReceiptCoordination result (reverse reverseAdmissions)
    )

admitContiguousMarker ::
  ( Disappearance.State,
    [(SequencedItem DisappearancePublicationMarker, Disappearance.MarkerAdmissionDisposition)],
    Map StreamSequence ReceiveProgress
  ) ->
  SequencedItem DisappearancePublicationMarker ->
  Either
    DisappearanceCoordinationProblem
    ( Disappearance.State,
      [(SequencedItem DisappearancePublicationMarker, Disappearance.MarkerAdmissionDisposition)],
      Map StreamSequence ReceiveProgress
    )
admitContiguousMarker (leaf, reverseAdmissions, progress) item = do
  validateSequencedMarker item
  (successorLeaf, disposition) <-
    commitLeaf
      ( Disappearance.preparePublicationMarkerReceipt
          (sequencedItemPayload item)
          leaf
      )
  Right
    ( successorLeaf,
      (item, disposition) : reverseAdmissions,
      Map.insert
        (sequencedItemSequence item)
        (markerReceiveProgress disposition)
        progress
    )

markerReceiveProgress ::
  Disappearance.MarkerAdmissionDisposition -> ReceiveProgress
markerReceiveProgress disposition = case disposition of
  Disappearance.MarkerTerminallyObsolete -> Completed
  _ -> ReceivedPending

data PublicationMarkerCompletionCoordination
  = PublicationMarkerCompletionAwaitingPriorStreamWork StreamWatermarks
  | PublicationMarkerCompletionInstalled
      StreamWatermarks
      Disappearance.MarkerAdmissionDisposition
  deriving stock (Eq, Show)

-- | Complete a retained disappearance marker only when the generic stream can
-- advance its completed prefix through that exact item. If an earlier item is
-- still pending, neither owner transition is committed.
coordinatePublicationMarkerCompletion ::
  SequencedItem DisappearancePublicationMarker ->
  Disappearance.State ->
  PeerStream.State DisappearancePublicationMarker ->
  Either
    DisappearanceCoordinationProblem
    ( Disappearance.State,
      PeerStream.State DisappearancePublicationMarker,
      PublicationMarkerCompletionCoordination
    )
coordinatePublicationMarkerCompletion item leaf stream = do
  validateCoordinatedOwners leaf stream
  validateSequencedMarker item
  requireRetainedMarker item stream
  let direction = sequencedItemDirection item
      sequenceNumber = sequencedItemSequence item
  preparedCompletion <-
    mapPeer
      ( PeerStream.prepareCompletion
          direction
          (Set.singleton sequenceNumber)
          stream
      )
  let watermarks = PeerStream.preparedCompletionWatermarks preparedCompletion
  if streamCompletedPrefix watermarks < streamPrefixThrough sequenceNumber
    then
      Right
        ( leaf,
          stream,
          PublicationMarkerCompletionAwaitingPriorStreamWork watermarks
        )
    else do
      preparedLeaf <-
        mapLeaf
          ( Disappearance.preparePublicationMarkerCompletion
              (sequencedItemPayload item)
              leaf
          )
      let (successorLeaf, disposition) =
            Disappearance.commitDisappearanceTransition preparedLeaf
          (successorStream, committedWatermarks) =
            PeerStream.commitCompletion preparedCompletion
      Right
        ( successorLeaf,
          successorStream,
          PublicationMarkerCompletionInstalled committedWatermarks disposition
        )

validateCoordinatedOwners ::
  Disappearance.State ->
  PeerStream.State payload ->
  Either DisappearanceCoordinationProblem ()
validateCoordinatedOwners leaf stream =
  let leafOwner = Disappearance.disappearanceLocalHerald leaf
      streamOwner = PeerStream.peerStreamLocalEpoch stream
   in unless
        (leafOwner == streamOwner)
        (Left (DisappearanceCoordinatorOwnerMismatch leafOwner streamOwner))

validateSequencedMarker ::
  SequencedItem DisappearancePublicationMarker ->
  Either DisappearanceCoordinationProblem ()
validateSequencedMarker item =
  let marker = sequencedItemPayload item
   in unless
        ( sequencedItemDirection item == disappearancePublicationMarkerDirection marker
            && sequencedItemSequence item == disappearancePublicationMarkerSequence marker
            && sequencedItemDigest item == disappearancePublicationMarkerDigest marker
        )
        ( Left
            ( DisappearancePublicationMarkerEnvelopeMismatch
                (sequencedItemDirection item)
                (sequencedItemSequence item)
            )
        )

requireRetainedMarker ::
  SequencedItem DisappearancePublicationMarker ->
  PeerStream.State DisappearancePublicationMarker ->
  Either DisappearanceCoordinationProblem ()
requireRetainedMarker item stream = do
  retained <-
    mapPeer
      (PeerStream.incomingRetainedItems (sequencedItemDirection item) stream)
  unless
    (any ((== item) . fst) retained || PeerStream.incomingSequenceCompleted (sequencedItemDirection item, sequencedItemSequence item) stream)
    ( Left
        ( DisappearancePublicationMarkerNotRetained
            (sequencedItemDirection item)
            (sequencedItemSequence item)
        )
    )

coordinateLocalPublicationCutCompletion ::
  DisappearanceProbeId ->
  HeraldPublicationPrefix ->
  Disappearance.State ->
  Either
    DisappearanceCoordinationProblem
    (Disappearance.State, Disappearance.MarkerAdmissionDisposition)
coordinateLocalPublicationCutCompletion identifier prefix state =
  commitLeaf
    (Disappearance.prepareLocalPublicationCutCompletion identifier prefix state)

coordinateAlignmentMarkerReceipt ::
  HeraldEpoch ->
  DisappearanceAlignmentMarker ->
  Disappearance.State ->
  Either
    DisappearanceCoordinationProblem
    (Disappearance.State, Disappearance.MarkerAdmissionDisposition)
coordinateAlignmentMarkerReceipt source marker state =
  commitLeaf (Disappearance.prepareAlignmentMarkerReceipt source marker state)

coordinateAlignmentMarkerCompletion ::
  DisappearanceAlignmentMarker ->
  StoreRevision ->
  Disappearance.State ->
  Either
    DisappearanceCoordinationProblem
    (Disappearance.State, Disappearance.MarkerAdmissionDisposition)
coordinateAlignmentMarkerCompletion marker revision state =
  commitLeaf
    (Disappearance.prepareAlignmentMarkerCompletion marker revision state)

coordinateAlignmentMutation ::
  DisappearanceProbeId ->
  Disappearance.AlignmentMutation ->
  DisappearanceBlockerWitness ->
  Disappearance.State ->
  Either
    DisappearanceCoordinationProblem
    (Disappearance.State, Maybe DisappearanceInvalidationIntention)
coordinateAlignmentMutation identifier mutation witness state =
  commitLeaf
    (Disappearance.prepareAlignmentMutation identifier mutation witness state)

coordinateBlockerObservation ::
  DisappearanceProbeId ->
  [DisappearanceBlockerWitness] ->
  Disappearance.State ->
  Either
    DisappearanceCoordinationProblem
    (Disappearance.State, Maybe DisappearanceInvalidationIntention)
coordinateBlockerObservation identifier blockers state =
  commitLeaf
    (Disappearance.prepareBlockerObservation identifier blockers state)

coordinateEvidenceObservation ::
  DisappearanceProbeId ->
  DisappearanceEvidenceSnapshot ->
  Disappearance.State ->
  Either
    DisappearanceCoordinationProblem
    (Disappearance.State, [DisappearanceInvalidationIntention])
coordinateEvidenceObservation identifier snapshot state =
  commitLeaf
    (Disappearance.prepareEvidenceObservation identifier snapshot state)

coordinateMatchingPublicationObservation ::
  Disappearance.MatchingWriteGate ->
  MatchingPublicationObservation ->
  Disappearance.State ->
  Either
    DisappearanceCoordinationProblem
    (Disappearance.State, [DisappearanceInvalidationIntention])
coordinateMatchingPublicationObservation gate observation state =
  commitLeaf
    (Disappearance.prepareMatchingPublicationObservation gate observation state)

coordinateLocalAbsenceReport ::
  DisappearanceProbeId ->
  DisappearanceEvidenceSnapshot ->
  Disappearance.State ->
  Either
    DisappearanceCoordinationProblem
    (Disappearance.State, Disappearance.ReportDisposition)
coordinateLocalAbsenceReport identifier snapshot state =
  commitLeaf
    (Disappearance.prepareLocalAbsenceReportFromEvidence identifier snapshot state)

coordinateProjectedReport ::
  DisappearanceEvidenceClaim ->
  Disappearance.State ->
  Either
    DisappearanceCoordinationProblem
    (Disappearance.State, Disappearance.ProjectedReportDisposition)
coordinateProjectedReport claim state =
  commitLeaf (Disappearance.prepareProjectedReport claim state)

coordinateProjectedTerminal ::
  ProjectedDisappearanceTerminal ->
  Disappearance.State ->
  Either
    DisappearanceCoordinationProblem
    (Disappearance.State, Disappearance.TerminalDisposition)
coordinateProjectedTerminal terminal state = do
  case terminal of
    ProjectedDisappearanceResolved probe outcome _ ->
      case disappearanceResolutionOutcomeView outcome of
        ControlledDisappearanceResolved {} ->
          Left
            (DisappearanceControlledResolutionRequiresOwnerCoordination probe)
        RegularSortDefinitionRetired {} ->
          Left
            (DisappearanceRegularResolutionRequiresOwnerCoordination probe)
    _ -> Right ()
  commitLeaf (Disappearance.prepareProjectedTerminal terminal state)

data ProjectedControlledResolutionCoordination
  = ProjectedControlledResolutionCoordination
      Disappearance.ProjectedControlledResolutionDisposition
      (Maybe ControlledRemoval.ControlledRemovalSummary)
  deriving stock (Eq, Show)

projectedControlledResolutionCoordinationDisposition ::
  ProjectedControlledResolutionCoordination ->
  Disappearance.ProjectedControlledResolutionDisposition
projectedControlledResolutionCoordinationDisposition
  (ProjectedControlledResolutionCoordination disposition _) = disposition

projectedControlledResolutionCoordinationSummary ::
  ProjectedControlledResolutionCoordination ->
  Maybe ControlledRemoval.ControlledRemovalSummary
projectedControlledResolutionCoordinationSummary
  (ProjectedControlledResolutionCoordination _ summary) = summary

-- | Prepare the checked leaf terminal and every removal-owner patch before
-- committing either successor.  A duplicate terminal is an exact no-op for
-- the whole Herald state and carries no reusable deletion authority.
coordinateProjectedControlledResolution ::
  ProjectedDisappearanceTerminal ->
  Disappearance.State ->
  HeraldState ->
  Either
    DisappearanceCoordinationProblem
    ( Disappearance.State,
      HeraldState,
      EffectBatch,
      ProjectedControlledResolutionCoordination
    )
coordinateProjectedControlledResolution terminal leaf herald = do
  let leafOwner = Disappearance.disappearanceLocalHerald leaf
      heraldOwner = checkedLocalHeraldEpoch (startupGenesis herald)
  unless
    (leafOwner == heraldOwner)
    (Left (DisappearanceControlledRemovalOwnerMismatch leafOwner heraldOwner))
  preparedLeaf <-
    mapLeaf (Disappearance.prepareProjectedControlledResolution terminal leaf)
  let disposition =
        Disappearance.preparedProjectedControlledResolutionDisposition preparedLeaf
  case disposition of
    Disappearance.ProjectedControlledResolutionDuplicate -> do
      let (successorLeaf, _, authority) =
            Disappearance.commitProjectedControlledResolution preparedLeaf
      case authority of
        Nothing ->
          Right
            ( successorLeaf,
              herald,
              mempty,
              ProjectedControlledResolutionCoordination disposition Nothing
            )
        Just _ -> Left DisappearanceControlledRemovalDispositionMismatch
    Disappearance.ProjectedControlledResolutionFresh -> do
      authority <-
        maybe
          (Left DisappearanceControlledRemovalAuthorityMissing)
          Right
          (Disappearance.preparedProjectedControlledResolutionAuthority preparedLeaf)
      preparedRemoval <-
        either
          (Left . DisappearanceControlledRemovalProblem)
          Right
          (ControlledRemoval.prepareDisappearanceControlledRemoval authority herald)
      let (_, _, summary) =
            ControlledRemoval.commitControlledRemoval preparedRemoval
      unless
        ( ControlledRemoval.controlledRemovalSummaryDisposition summary
            == ControlledRemoval.ControlledRemovalApplied
        )
        (Left DisappearanceControlledRemovalDispositionMismatch)
      let (successorLeaf, _, _) =
            Disappearance.commitProjectedControlledResolution preparedLeaf
          (successorHerald, effects, committedSummary) =
            ControlledRemoval.commitControlledRemoval preparedRemoval
      Right
        ( successorLeaf,
          successorHerald,
          effects,
          ProjectedControlledResolutionCoordination
            disposition
            (Just committedSummary)
        )

data ProjectedRegularResolutionCoordination
  = ProjectedRegularResolutionCoordination
      Disappearance.ProjectedRegularResolutionDisposition
      (Maybe RegularRetirement.RegularRetirementSummary)
  deriving stock (Eq, Show)

projectedRegularResolutionCoordinationDisposition ::
  ProjectedRegularResolutionCoordination ->
  Disappearance.ProjectedRegularResolutionDisposition
projectedRegularResolutionCoordinationDisposition
  (ProjectedRegularResolutionCoordination disposition _) = disposition

projectedRegularResolutionCoordinationSummary ::
  ProjectedRegularResolutionCoordination ->
  Maybe RegularRetirement.RegularRetirementSummary
projectedRegularResolutionCoordinationSummary
  (ProjectedRegularResolutionCoordination _ summary) = summary

-- | Prepare the checked regular terminal, exact Registry retirement, and
-- cut-qualified Store projection before committing any successor.  A duplicate
-- terminal is a whole-state/effect no-op and carries no reusable authority.
coordinateProjectedRegularResolution ::
  ProjectedDisappearanceTerminal ->
  Disappearance.State ->
  HeraldState ->
  Either
    DisappearanceCoordinationProblem
    ( Disappearance.State,
      HeraldState,
      EffectBatch,
      ProjectedRegularResolutionCoordination
    )
coordinateProjectedRegularResolution terminal leaf herald = do
  let leafOwner = Disappearance.disappearanceLocalHerald leaf
      heraldOwner = checkedLocalHeraldEpoch (startupGenesis herald)
  unless
    (leafOwner == heraldOwner)
    (Left (DisappearanceRegularRetirementOwnerMismatch leafOwner heraldOwner))
  preparedLeaf <-
    mapLeaf (Disappearance.prepareProjectedRegularResolution terminal leaf)
  let disposition =
        Disappearance.preparedProjectedRegularResolutionDisposition preparedLeaf
  case disposition of
    Disappearance.ProjectedRegularResolutionDuplicate -> do
      let (successorLeaf, _, authority) =
            Disappearance.commitProjectedRegularResolution preparedLeaf
      case authority of
        Nothing ->
          Right
            ( successorLeaf,
              herald,
              mempty,
              ProjectedRegularResolutionCoordination disposition Nothing
            )
        Just _ -> Left DisappearanceRegularRetirementDispositionMismatch
    Disappearance.ProjectedRegularResolutionFresh -> do
      authority <-
        maybe
          (Left DisappearanceRegularRetirementAuthorityMissing)
          Right
          (Disappearance.preparedProjectedRegularResolutionAuthority preparedLeaf)
      preparedRetirement <-
        either
          (Left . DisappearanceRegularRetirementProblem)
          Right
          ( RegularRetirement.prepareDisappearanceRegularRetirement
              authority
              herald
          )
      let (successorHerald, committedSummary) =
            RegularRetirement.commitRegularRetirement preparedRetirement
      unless
        ( RegularRetirement.regularRetirementSummaryDisposition committedSummary
            == RegularRetirement.RegularRetirementApplied
        )
        (Left DisappearanceRegularRetirementDispositionMismatch)
      let (successorLeaf, _, _) =
            Disappearance.commitProjectedRegularResolution preparedLeaf
      Right
        ( successorLeaf,
          successorHerald,
          mempty,
          ProjectedRegularResolutionCoordination
            disposition
            (Just committedSummary)
        )

-- | Replay a resolved canonical history record on a restricted observer. The
-- receiving owners authenticate the retained projection and observer state;
-- this path never installs a local disappearance probe or emits old-member
-- evidence.
replayProjectedDisappearanceResolution ::
  OracleProjection.ProjectedDisappearanceProbe ->
  HeraldState ->
  Either DisappearanceCoordinationProblem (HeraldState, EffectBatch)
replayProjectedDisappearanceResolution projected state =
  case OracleProjection.projectedDisappearanceTerminal projected of
    Just (OracleProjection.ProjectedDisappearanceResolved outcome _) ->
      case disappearanceResolutionOutcomeView outcome of
        ControlledDisappearanceResolved {} -> do
          prepared <-
            either
              (Left . DisappearanceControlledRemovalProblem)
              Right
              (ControlledRemoval.prepareImportedDisappearanceControlledRemoval projected state)
          let (successor, effects, _) = ControlledRemoval.commitControlledRemoval prepared
          Right (successor, effects)
        RegularSortDefinitionRetired {} -> do
          prepared <-
            either
              (Left . DisappearanceRegularRetirementProblem)
              Right
              (RegularRetirement.prepareImportedRegularRetirement projected state)
          let (successor, _) = RegularRetirement.commitRegularRetirement prepared
          Right (successor, mempty)
    _ -> Left DisappearanceImportedResolutionMissing

coordinateProjectedLabelTerminal ::
  ProjectedLabelTerminal ->
  Disappearance.State ->
  Either
    DisappearanceCoordinationProblem
    (Disappearance.State, Disappearance.LabelTerminalDisposition)
coordinateProjectedLabelTerminal terminal state =
  commitLeaf (Disappearance.prepareProjectedLabelTerminal terminal state)

commitLeaf ::
  Either
    Disappearance.DisappearanceProblem
    (Disappearance.PreparedDisappearanceTransition output) ->
  Either DisappearanceCoordinationProblem (Disappearance.State, output)
commitLeaf = fmap Disappearance.commitDisappearanceTransition . mapLeaf

mapLeaf ::
  Either Disappearance.DisappearanceProblem value ->
  Either DisappearanceCoordinationProblem value
mapLeaf = either (Left . DisappearanceLeafProblem) Right

mapPeer ::
  Either PeerStream.PeerStreamProblem value ->
  Either DisappearanceCoordinationProblem value
mapPeer = either (Left . DisappearancePeerStreamProblem) Right

exactlyOneAssignment ::
  NonEmpty (SequencedItem DisappearancePublicationMarker) ->
  Either
    DisappearanceCoordinationProblem
    (SequencedItem DisappearancePublicationMarker)
exactlyOneAssignment (item :| []) = Right item
exactlyOneAssignment (item :| _) =
  Left
    ( DisappearancePublicationAssignmentCardinality
        (streamDirectionDestination (sequencedItemDirection item))
    )
