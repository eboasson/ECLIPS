{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | The live disappearance vertical. The kernel installs the candidate, cuts,
-- report and exact Oracle intention together; transport only executes retained
-- work. No observation of time or a socket can create a candidate or Abort.
module Eclips.Herald.UseCase.DisappearanceLive
  ( observeTakenPublications,
    advanceDisappearanceWork,
    applyDisappearanceOpen,
    applyDisappearanceReport,
    applyDisappearanceTerminal,
    applyDisappearanceAbort,
    receiveDisappearanceAlignmentMarker,
    disappearanceAlignmentRetryControls,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, guard, unless)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Eclips.Domain.Disappearance
import Eclips.Domain.Identity
import Eclips.Domain.Label (labelRecordRevision)
import Eclips.Domain.Publication (CheckedPublication, checkedPublicationId, checkedPublicationSort, checkedPublicationValue)
import Eclips.Domain.Sort.Canonical (descriptorSortId)
import Eclips.Domain.Sort.Profile (PredefinedSortRole (..), decodeSortDefinitionValue)
import Eclips.Domain.SortOccurrence (SortOccurrenceBase (Genesis), resolvedRetirementOccurrenceBase)
import Eclips.Herald.Alignment.Protocol qualified as AlignmentProtocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Disappearance.Evidence qualified as Evidence
import Eclips.Herald.Disappearance.Protocol qualified as Protocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch (EffectBatch, HeraldEffect (..), orderedEffectBatch)
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch, checkedSystemId)
import Eclips.Herald.Graph.DisappearanceReadiness qualified as Readiness
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Input (PeerControl (..))
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerPayload (PeerLogicalPayload (..), peerLogicalDisappearanceProbeMarkerItem)
import Eclips.Herald.PeerStream (sequencedItemPayload)
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.Invariant (HeraldInvariantFault (..), HeraldTransitionInvariantViolation (..))
import Eclips.Herald.Startup.State
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.UseCase.Disappearance qualified as Coordination
import Eclips.Oracle.Disappearance qualified as Oracle
import Eclips.Oracle.Label (liveDecisionObject)

-- | Only the exact publications removed by an accepted application query enter
-- this boundary. An empty query consequently creates no candidate.
observeTakenPublications :: [CheckedPublication] -> HeraldState -> Either HeraldInvariantFault HeraldState
observeTakenPublications publications state =
  foldM retain state (nub (concatMap (takenSubjects state) publications))
  where
    retain current subject = do
      prepared <- checked (Disappearance.prepareLocalTakeCandidate (Protocol.localTakeCandidateObservation subject) (startupDisappearanceState current))
      let (leaf, _) = Disappearance.commitDisappearanceTransition prepared
      Right (replaceStartupDisappearanceState leaf current)

takenSubjects :: HeraldState -> CheckedPublication -> [DisappearanceSubject]
takenSubjects state publication = case SortRegistry.lookupEffectiveSort (checkedPublicationSort publication) registry of
  Nothing -> []
  Just carrier -> case SortRegistry.registryEntryPredefinedRole carrier of
    Just SortDefinitionRole -> case decodeSortDefinitionValue (checkedPublicationValue publication) of
      Left _ -> []
      Right descriptor -> case SortRegistry.lookupEffectiveSort (descriptorSortId descriptor) registry of
        Just entry
          | SortRegistry.registryEntryPredefinedRole entry == Nothing ->
              let base = case SortRegistry.latestRegularSortRetirement (descriptorSortId descriptor) registry of
                    Nothing -> Right Genesis
                    Just retirement -> resolvedRetirementOccurrenceBase (SortRegistry.regularSortRetirementResolveIndex retirement)
               in case base of
                    Left _ -> []
                    Right occurrenceBase -> [regularSortDefinitionDisappearanceSubject (deriveRegularSortOccurrenceClaim (checkedSystemId (startupGenesis state)) descriptor occurrenceBase)]
        _ -> []
    Just role
      | role `elem` [NablaRole, DeltaRole, EdgeRole, NeutralVertexRole] ->
          [ subject
          | occurrence <- Reconciliation.structuralAppliedHistoryOccurrences reconciliation,
            Just projection <- [Reconciliation.structuralAppliedControlledProjectionAtOccurrence occurrence reconciliation],
            Reconciliation.appliedControlledProjectionPublication projection == Just (checkedPublicationId publication),
            let object = Reconciliation.appliedControlledProjectionObject projection,
            let revision = labelRecordRevision <$> Controlled.controlledReleasedLabelRecord object (startupControlledState state),
            Right subject <- [controlledPredefinedDisappearanceSubject role object occurrence revision]
          ]
    _ -> []
  where
    registry = startupSortRegistryState state
    reconciliation = GraphProgress.structuralProgressReconciliation (startupStructuralProgressState state)

-- | Advance only retained candidates and probes. Each leaf transition is
-- idempotent, and the Oracle client owns stable intention identities.
advanceDisappearanceWork :: HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
advanceDisappearanceWork predecessor = do
  withCandidates <- case openContext predecessor of
    Nothing -> Right predecessor
    Just context -> foldM (reevaluate context) predecessor (Disappearance.candidateWitnesses (startupDisappearanceState predecessor))
  withEvidence <- foldM advanceProbe withCandidates (Disappearance.probeWitnesses (startupDisappearanceState withCandidates))
  successor <- retainIntentions withEvidence
  Right (successor, oracleEffectsSince predecessor successor)
  where
    reevaluate context state candidate = do
      case candidateLabelDecision (candidate.candidateSubject) state of
        Just (decision, object)
          | candidate.candidateProjectedProbe == Nothing ->
              commitLeaf (Disappearance.prepareCandidateLabelWait decision object (candidate.candidateSubject)) state
        _ -> do
          prepared <- checked (Disappearance.prepareCandidateReevaluation context (candidate.candidateSubject) (startupDisappearanceState state))
          let (leaf, _) = Disappearance.commitDisappearanceTransition prepared
          Right (replaceStartupDisappearanceState leaf state)

candidateLabelDecision :: DisappearanceSubject -> HeraldState -> Maybe (LabelDecisionId, GlobalObjectId)
candidateLabelDecision subject state = case disappearanceSubjectView subject of
  ControlledPredefinedSubjectView object _ _ -> case [ (decision, object)
                                                     | (decision, workflow) <- OracleProjection.projectedLabelWorkflows (startupOracleProjectionState state),
                                                       liveDecisionObject (OracleProjection.projectedLabelWorkflowDecision workflow) == object,
                                                       OracleProjection.projectedLabelWorkflowPhase workflow
                                                         == OracleProjection.LabelWorkflowReleased
                                                     ] of
    first : _ -> Just first
    [] -> Nothing
  _ -> Nothing

openContext :: HeraldState -> Maybe Protocol.DisappearanceOpenContext
openContext state = do
  guard (not (admissionPreparing state))
  base <- either (const Nothing) Just (Readiness.captureEstablishedStructuralBase (startupStructuralProgressState state))
  either (const Nothing) Just (Readiness.disappearanceOpenContextFromOwnerCaptures (Readiness.captureCurrentMembership (startupOracleProjectionState state)) base)

admissionPreparing :: HeraldState -> Bool
admissionPreparing = (/= Nothing) . OracleProjection.oracleViewPendingHeraldAdmission . OracleProjection.oracleView . startupOracleProjectionState

advanceProbe :: HeraldState -> Disappearance.ProbeWitness -> Either HeraldInvariantFault HeraldState
advanceProbe state witness = case witness.witnessPhase of
  Disappearance.ProbeCollectingView -> do
    let projected = witness.witnessProjectedProbe
        probe = Protocol.projectedProbeId projected
    snapshot <- checked (Evidence.disappearanceEvidenceSnapshotForHerald (Protocol.projectedProbeSubject projected) state)
    observed <- commitLeaf (Disappearance.prepareEvidenceObservation probe snapshot) state
    withAlignment <- completeAlignmentMarkers probe observed
    withLocal <-
      if null (Evidence.disappearanceEvidenceSnapshotBlockers snapshot)
        then commitLeaf (Disappearance.prepareLocalPublicationCutCompletion probe (witness.witnessLocalPublicationCut)) withAlignment
        else Right withAlignment
    case Disappearance.prepareLocalAbsenceReportFromEvidence probe snapshot (startupDisappearanceState withLocal) of
      Left Disappearance.DisappearanceReportBlocked {} -> Right withLocal
      Left _ -> fault
      Right prepared -> let (leaf, _) = Disappearance.commitDisappearanceTransition prepared in Right (replaceStartupDisappearanceState leaf withLocal)
  _ -> Right state

retainIntentions :: HeraldState -> Either HeraldInvariantFault HeraldState
retainIntentions state = do
  let leaf = startupDisappearanceState state
      pendingOpens = if admissionPreparing state then [] else Disappearance.openIntentions leaf
  opened <- foldM (\s intent -> retainRequest (OracleClient.prepareOpenDisappearanceProbeRequest (Protocol.disappearanceOpenIntentionSubject intent) (Protocol.disappearanceOpenIntentionCoordinate intent)) s) state pendingOpens
  reported <- foldM (\s report -> retainRequest (OracleClient.preparePredefinedAbsenceReportRequest (Protocol.localAbsenceReportClaim report)) s) opened (Disappearance.reportIntentions leaf)
  invalidated <- foldM (\s intent -> retainRequest (OracleClient.prepareInvalidateDisappearanceProbeRequest (Protocol.disappearanceInvalidationProbe intent) (Protocol.disappearanceInvalidationReporter intent) (invalidationReason (Protocol.disappearanceInvalidationReason intent)) (Protocol.disappearanceInvalidationWitness intent)) s) reported (Disappearance.invalidationIntentions leaf)
  foldM (\s intent -> retainRequest (OracleClient.prepareResolveDisappearanceProbeRequest (Protocol.disappearanceResolveProbe intent) (Oracle.completeDisappearanceEvidenceDigest (Protocol.disappearanceResolveClaims intent))) s) invalidated (Disappearance.resolveIntentions leaf)

invalidationReason :: Protocol.DisappearanceInvalidationReason -> Oracle.DisappearanceInvalidationReason
invalidationReason Protocol.MatchingPublicationObserved = Oracle.MatchingPublicationObserved
invalidationReason Protocol.LocalEvidenceContradicted = Oracle.EvidenceContradicted

retainRequest :: (OracleClient.State -> Either OracleClient.OracleClientProblem OracleClient.PreparedOracleRequest) -> HeraldState -> Either HeraldInvariantFault HeraldState
retainRequest prepare state = do
  prepared <- checked (prepare (startupOracleClientState state))
  let (client, _, _) = OracleClient.commitOracleRequest prepared
  Right (replaceStartupOracleClientState client state)

-- Reconnect and paced retry are owned by OracleClient. A disappearance pass
-- schedules only newly retained work, so unchanged kernel input is effect-free.
oracleEffectsSince :: HeraldState -> HeraldState -> EffectBatch
oracleEffectsSince predecessor successor =
  orderedEffectBatch
    [ RunOracleClientAction action
    | action <- OracleClient.oracleClientRequestActions (startupOracleClientState successor),
      action `notElem` OracleClient.oracleClientRequestActions (startupOracleClientState predecessor)
    ]

-- | The Open and every peer assignment are installed before dispatch. Equal
-- racing Opens therefore settle against the original probe without new markers.
applyDisappearanceOpen :: Protocol.ProjectedDisappearanceProbe -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
applyDisappearanceOpen projected state = do
  snapshot <- checked (Evidence.disappearanceEvidenceSnapshotForHerald (Protocol.projectedProbeSubject projected) state)
  prepared <- checked (Disappearance.prepareProjectedOpen projected snapshot (startupDisappearanceState state))
  let (leaf, output) = Disappearance.commitDisappearanceTransition prepared
      withOpen = replaceStartupDisappearanceState leaf state
  case output of
    Disappearance.ProjectedOpenOutput Disappearance.ProjectedOpenDuplicate _ _ -> Right (withOpen, mempty)
    Disappearance.ProjectedOpenOutput Disappearance.ProjectedOpenInstalled markerItems alignmentMarkers -> do
      enqueued <- checked (PeerStream.prepareSequenceBoundEnqueue (Map.map (const (peerLogicalDisappearanceProbeMarkerItem (Protocol.projectedProbeId projected) (Protocol.projectedProbeCoordinate projected) :| [])) markerItems) (startupPeerStreamState state))
      markers <- traverse extractMarker (concatMap NonEmpty.toList (Map.elems (PeerStream.preparedEnqueueAssignments enqueued)))
      assigned <- checked (Disappearance.preparePublicationMarkerAssignments (Protocol.projectedProbeId projected) markers leaf)
      let (assignedLeaf, _) = Disappearance.commitDisappearanceTransition assigned
          (stream, _) = PeerStream.commitEnqueue enqueued
          successor = replaceStartupPeerStreamState stream . replaceStartupDisappearanceState assignedLeaf $ withOpen
          frontiers = [SendPeerControl binding (PeerStreamFrontierAdvanced direction sequenceNumber) | (remote, (direction, sequenceNumber)) <- Map.toAscList (PeerStream.preparedEnqueueFrontierAdvances enqueued), Just binding <- [Discovery.currentPeerBinding remote (startupDiscoveryState successor)]]
          dispatch = fmap SchedulePeerDispatch (PeerStream.preparedEnqueueDispatchTickets enqueued)
          local = checkedLocalHeraldEpoch (startupGenesis successor)
      -- Local subscriptions use the same retained marker and authenticated
      -- destination watermark as remote subscriptions. Their owner delivers
      -- the marker directly after all Open assignments have been installed.
      withLocalAlignment <-
        foldM
          (\current marker -> checked (receiveDisappearanceAlignmentMarker local marker current))
          successor
          [marker | marker <- alignmentMarkers, AlignmentProtocol.alignmentSubscriptionIdDestinationHerald (Protocol.disappearanceAlignmentMarkerSubscription marker) == local]
      withEarlyAlignment <-
        foldM
          (\current (source, marker) -> checked (receiveDisappearanceAlignmentMarker source marker current))
          withLocalAlignment
          [held | held@(_, marker) <- Disappearance.pendingAlignmentMarkers assignedLeaf, Protocol.disappearanceAlignmentMarkerProbe marker == Protocol.projectedProbeId projected]
      completed <- completeAlignmentMarkers (Protocol.projectedProbeId projected) withEarlyAlignment
      Right (completed, orderedEffectBatch (frontiers <> dispatch <> alignmentMarkerEffects successor alignmentMarkers))
  where
    extractMarker item = case sequencedItemPayload item of
      PeerLogicalDisappearanceProbeMarker marker -> Right marker
      _ -> fault

applyDisappearanceReport :: DisappearanceEvidenceClaim -> HeraldState -> Either HeraldInvariantFault HeraldState
applyDisappearanceReport claim = commitLeaf (Disappearance.prepareProjectedReport claim)

applyDisappearanceTerminal :: Protocol.ProjectedDisappearanceTerminal -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
applyDisappearanceTerminal terminal state = do
  let leaf = startupDisappearanceState state
      probe = terminalProbe terminal
  witness <- maybe fault Right (Disappearance.probeWitness probe leaf)
  (successorLeaf, successor, effects) <- case terminal of
    Protocol.ProjectedDisappearanceResolved _ outcome _ -> case disappearanceResolutionOutcomeView outcome of
      ControlledDisappearanceResolved {} -> do
        (l, s, e, _) <- checked (Coordination.coordinateProjectedControlledResolution terminal leaf state)
        Right (l, s, e)
      RegularSortDefinitionRetired {} -> do
        (l, s, e, _) <- checked (Coordination.coordinateProjectedRegularResolution terminal leaf state)
        Right (l, s, e)
    _ -> do
      (l, _) <- checked (Coordination.coordinateProjectedTerminal terminal leaf)
      Right (l, state, mempty)
  let projected = witness.witnessProjectedProbe
  client <- checked (OracleClient.suppressDisappearanceProbeRequests probe (Protocol.projectedProbeSubject projected) (Protocol.projectedProbeCoordinate projected) (terminalIndex terminal) (startupOracleClientState successor))
  Right (replaceStartupOracleClientState client . replaceStartupDisappearanceState successorLeaf $ successor, effects)

terminalProbe :: Protocol.ProjectedDisappearanceTerminal -> DisappearanceProbeId
terminalProbe terminal = case terminal of
  Protocol.ProjectedDisappearanceResolved probe _ _ -> probe
  Protocol.ProjectedDisappearanceInvalidated probe _ _ -> probe
  Protocol.ProjectedDisappearanceAborted probe _ _ -> probe

terminalIndex :: Protocol.ProjectedDisappearanceTerminal -> ControlIndex
terminalIndex terminal = case terminal of
  Protocol.ProjectedDisappearanceResolved _ _ index -> index
  Protocol.ProjectedDisappearanceInvalidated _ _ index -> index
  Protocol.ProjectedDisappearanceAborted _ _ index -> index

applyDisappearanceAbort :: DisappearanceProbeId -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
applyDisappearanceAbort probe state = case Disappearance.probeWitness probe (startupDisappearanceState state) of
  Just witness | witness.witnessPhase == Disappearance.ProbeCollectingView -> do
    successor <- retainRequest (OracleClient.prepareAbortDisappearanceProbeRequest probe Oracle.authorizedDisappearanceAbortReason) state
    Right (successor, oracleEffectsSince state successor)
  _ -> Right (state, mempty)

receiveDisappearanceAlignmentMarker :: HeraldEpoch -> Protocol.DisappearanceAlignmentMarker -> HeraldState -> Either Disappearance.DisappearanceProblem HeraldState
receiveDisappearanceAlignmentMarker source marker state = do
  let subscription = Protocol.disappearanceAlignmentMarkerSubscription marker
  destination <-
    maybe
      (Left (Disappearance.DisappearanceAlignmentMarkerNotCaptured subscription))
      Right
      (Transfer.lookupDestinationSubscription subscription (Alignment.alignmentTransferState (startupAlignmentState state)))
  let expectedSource =
        (AlignmentProtocol.alignmentAttemptSourceHerald <$> Transfer.destinationSubscriptionAttempt destination)
          <|> Transfer.destinationSubscriptionBootstrapSourceHerald destination
  unless
    (expectedSource == Just source)
    (Left (Disappearance.DisappearanceAlignmentSourceNotCaptured source))
  prepared <- Disappearance.prepareAlignmentMarkerReceipt source marker (startupDisappearanceState state)
  let (leaf, _) = Disappearance.commitDisappearanceTransition prepared
  Right (replaceStartupDisappearanceState leaf state)

completeAlignmentMarkers :: DisappearanceProbeId -> HeraldState -> Either HeraldInvariantFault HeraldState
completeAlignmentMarkers probe state = case Disappearance.probeWitness probe (startupDisappearanceState state) of
  Nothing -> fault
  Just witness -> foldM complete state (witness.witnessIncomingAlignmentCuts)
  where
    complete current (subscription, cell) = case (cell.alignmentCellMarker, Transfer.lookupDestinationSubscription subscription (Alignment.alignmentTransferState (startupAlignmentState current))) of
      (Just marker, Just destination) -> case Transfer.destinationSubscriptionAppliedThrough destination of
        Just through | through >= Protocol.disappearanceAlignmentMarkerThroughRevision marker -> commitLeaf (Disappearance.prepareAlignmentMarkerCompletion marker through) current
        _ -> Right current
      _ -> Right current

alignmentMarkerEffects :: HeraldState -> [Protocol.DisappearanceAlignmentMarker] -> [HeraldEffect]
alignmentMarkerEffects state markers =
  [ SendPeerControl binding (PeerAlignmentControl (markerControl marker))
  | marker <- markers,
    let remote = AlignmentProtocol.alignmentSubscriptionIdDestinationHerald (Protocol.disappearanceAlignmentMarkerSubscription marker),
    remote /= checkedLocalHeraldEpoch (startupGenesis state),
    Just binding <- [Discovery.currentPeerBinding remote (startupDiscoveryState state)]
  ]

markerControl :: Protocol.DisappearanceAlignmentMarker -> AlignmentProtocol.AlignmentControl
markerControl marker = AlignmentProtocol.AlignmentProbeMarker (Protocol.disappearanceAlignmentMarkerProbe marker) (Protocol.disappearanceAlignmentMarkerSubscription marker) (Protocol.disappearanceAlignmentMarkerThroughRevision marker)

disappearanceAlignmentRetryControls :: HeraldEpoch -> HeraldState -> [AlignmentProtocol.AlignmentControl]
disappearanceAlignmentRetryControls remote state =
  [ markerControl marker
  | witness <- Disappearance.probeWitnesses (startupDisappearanceState state),
    witness.witnessPhase == Disappearance.ProbeCollectingView,
    (_, marker) <- witness.witnessOutgoingAlignmentMarkers,
    AlignmentProtocol.alignmentSubscriptionIdDestinationHerald (Protocol.disappearanceAlignmentMarkerSubscription marker) == remote
  ]

commitLeaf :: (Disappearance.State -> Either Disappearance.DisappearanceProblem (Disappearance.PreparedDisappearanceTransition value)) -> HeraldState -> Either HeraldInvariantFault HeraldState
commitLeaf prepare state = do
  prepared <- checked (prepare (startupDisappearanceState state))
  let (leaf, _) = Disappearance.commitDisappearanceTransition prepared
  Right (replaceStartupDisappearanceState leaf state)

checked :: Either problem value -> Either HeraldInvariantFault value
checked = either (const fault) Right

fault :: Either HeraldInvariantFault value
fault = Left (HeraldTransitionInvariant HeraldOracleTransitionContradiction)
