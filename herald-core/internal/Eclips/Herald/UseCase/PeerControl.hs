-- | Atomic coordination of Step-7 discovery, placement, resume, acknowledgement,
-- and logical dispatch control. Publication-item admission lives in the sibling
-- @Eclips.Herald.UseCase.PeerInput@ coordinator because it composes the
-- semantic data owners.
module Eclips.Herald.UseCase.PeerControl
  ( observePeerCandidateOpened,
    applyPeerHello,
    applyPeerControl,
    PeerControlServingDisposition (..),
    applyPeerControlWithInstalledCuts,
    redriveTerminalSourceReadinessIfProgressed,
    advanceLocalTopologyCutIfReady,
    reconnectRepairEffects,
    structuralRetryControls,
    applyPeerDispatchSelection,
    PeerBindingLossDisposition (..),
    applyPeerBindingLoss,
    applyPeerDispatchObservation,
    applyDrainingPeerDispatchObservation,
    cutPeerDispatchForDrain,
  )
where

import Control.Monad (foldM, unless)
import Data.ByteString qualified as ByteString
import Data.List (find, partition, stripPrefix)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Eclips.Application.Types.Lifecycle (HeraldLocator)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    StructuralOccurrenceId,
    TopologyCutId,
    controlIndex,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGenerationId,
    HeraldMembershipHistory,
    HeraldMembershipLineage,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    heraldMembershipHistoryCurrent,
    heraldMembershipHistoryGenesis,
    heraldMembershipLineage,
    heraldMembershipLineageOrigin,
    heraldMembershipLineageTarget,
  )
import Eclips.Domain.Topology
  ( MemberSetDigest,
    TopologyPredecessorView (MembershipSuccessorPredecessorView),
    topologyCutFrontier,
    topologyCutPredecessor,
    topologyFrontierAppliedControlPrefix,
    topologyPredecessorCutId,
    topologyPredecessorView,
  )
import Eclips.Herald.Alignment.Loss qualified as AlignmentLoss
import Eclips.Herald.Alignment.Protocol (AlignmentControl (..))
import Eclips.Herald.Alignment.State qualified as AlignmentState
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Disappearance.Protocol qualified as DisappearanceProtocol
import Eclips.Herald.Discovery
  ( HelloRejection (HelloMembershipMismatch),
    KnownContactsDisposition (..),
    KnownHerald,
    PeerAddress,
    PeerBinding,
    PeerCandidate,
    PeerCandidateOpened,
    PeerDialIntent,
    PeerHello,
    PeerHelloDisposition (..),
    candidateHelloCandidate,
    candidateHelloMessage,
    peerBindingGeneration,
    peerBindingGenerationWord64,
    peerBindingRemoteHeraldEpoch,
    peerBindingRemoteHeraldId,
    peerCandidateConnectionNonce,
    peerCandidateInitiatorHeraldId,
    peerCandidateOpened,
    peerDialIntentHeraldEpoch,
    peerHelloAppliedControlIndex,
    peerHelloHeraldEpoch,
  )
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    PeerProtocolDisposition (..),
    effectBatchMembers,
    emptyEffectBatch,
    orderedEffectBatch,
    singletonEffectBatch,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedLocalHeraldEpoch,
    checkedLocalHeraldId,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.Protocol
  ( StructuralAppliedReport,
    TopologyCutAcceptance,
    TopologyCutAnnounce,
    TopologyCutEstablished,
    TopologyCutEstablishedAck,
    structuralAppliedReportReporter,
    topologyCutAcceptanceReporter,
    topologyCutAnnounceAnnouncer,
    topologyCutAnnounceCut,
    topologyCutEstablishedAckReporter,
    topologyCutEstablishedCut,
    topologyCutEstablishedId,
  )
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource qualified as TerminalSource
import Eclips.Herald.Input
  ( PeerControl (..),
  )
import Eclips.Herald.Join.State qualified as Join
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerDelivery qualified as DeliveryOwner
import Eclips.Herald.PeerLiveness.State qualified as PeerLiveness
import Eclips.Herald.PeerPayload (PeerLogicalPayload)
import Eclips.Herald.PeerStream
  ( PeerDispatchAttempt,
    PeerDispatchBindingGeneration,
    PeerDispatchOutcome (..),
    PeerDispatchTicket,
    ResumeOffer,
    ResumeResponse,
    StreamDirection,
    StreamPrefix,
    StreamSequence,
    mkPeerDispatchBindingGeneration,
    mkStreamDirection,
    nextAfterStreamPrefix,
    peerDispatchTicketDirection,
    resumeOfferDestination,
    resumeOfferSource,
    resumeResponseCompletion,
    resumeResponseReceivedPrefix,
    resumeResponseRetransmitFrom,
    streamCompletedPrefix,
    streamDirectionDestination,
    streamDirectionSource,
    streamPrefixSequence,
    streamSequenceWord64,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement
  ( DeltaRoute,
    PlacementAcknowledgement,
    PlacementUpdate (..),
    deltaRouteDelta,
    deltaRouteStoreIncarnation,
    placementSnapshotOwner,
  )
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (..),
    HeraldTransitionInvariantViolation (..),
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupAlignmentCutQueryCache,
    replaceStartupAlignmentState,
    replaceStartupApplicationState,
    replaceStartupControlledState,
    replaceStartupDisappearanceState,
    replaceStartupDiscoveryState,
    replaceStartupGraphState,
    replaceStartupIdGeneratorState,
    replaceStartupLabelBarrierState,
    replaceStartupPeerLivenessState,
    replaceStartupPeerStreamState,
    replaceStartupPlacementState,
    replaceStartupPublicationState,
    replaceStartupSortRegistryState,
    replaceStartupStoreState,
    replaceStartupStructuralBaseCoordinator,
    replaceStartupStructuralProgressState,
    replaceStartupTerminalSourceHoldState,
    replaceStartupWaitState,
    startupAlignmentCutQueryCache,
    startupAlignmentState,
    startupApplicationState,
    startupControlledState,
    startupDiagnosticChecks,
    startupDisappearanceState,
    startupDiscoveryState,
    startupGenesis,
    startupGraphState,
    startupIdGeneratorState,
    startupJoinState,
    startupLabelBarrierState,
    startupLastObservedTime,
    startupOracleProjectionState,
    startupPeerLivenessState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralBaseCoordinator,
    startupStructuralProgressState,
    startupTerminalSourceHoldState,
    startupWaitState,
  )
import Eclips.Herald.Structural.Debt (sortOccurrence)
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.TerminalSourceHold.State qualified as TerminalSourceHold
import Eclips.Herald.Time (MonotonicInstant)
import Eclips.Herald.UseCase.Alignment qualified as Alignment
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransfer
import Eclips.Herald.UseCase.ControlReclamation qualified as ControlReclamation
import Eclips.Herald.UseCase.DisappearanceLive qualified as DisappearanceLive
import Eclips.Herald.UseCase.JoinControlTails qualified as JoinControlTails
import Eclips.Herald.UseCase.LabelCollection qualified as LabelCollection
import Eclips.Herald.UseCase.PeerDelivery qualified as PeerDelivery
import Eclips.Herald.UseCase.PeerInput qualified as PeerInput
import Eclips.Herald.UseCase.PeerPlacement (placementUpdateClaimsAreValid)
import Eclips.Herald.UseCase.ProcessPreparation qualified as ProcessPreparation
import Eclips.Herald.UseCase.Step15StructuralBase qualified as StructuralBase
import Eclips.Herald.UseCase.StructuralProgress qualified as StructuralProgress
import Eclips.Oracle.Admission qualified as Admission
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement, receiptRetirementHighWater)

type PeerTransition = Either HeraldInvariantFault (HeraldState, EffectBatch)

-- | Explicit evidence for the top-level serving-driver decision.  Only a
-- current, successfully admitted, exact generation-evidence duplicate may
-- report 'EvidenceUnchanged'.
data PeerControlServingDisposition
  = PeerControlServingWorkRequired
  | PeerControlGenerationAcknowledgementHandled
  | PeerControlGenerationEvidenceUnchanged
  | PeerControlCurrentEvidenceRejected
  deriving stock (Eq, Ord, Show)

-- | Exact Discovery-owned classification of a physical loss observation.
-- A replaced logical generation may still report its later physical close;
-- only loss of the current binding changes peer or voter reachability.
data PeerBindingLossDisposition
  = CurrentPeerBindingLost
  | StalePeerBindingLoss
  deriving stock (Eq, Ord, Show)

observePeerCandidateOpened ::
  PeerCandidateOpened ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
observePeerCandidateOpened opened state =
  let membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState state))
      generation = heraldMembershipGenerationId membership
      members = heraldMembershipGenerationActiveMemberSetDigest membership
   in case Discovery.prepareCandidateHello
        ( OracleProjection.oracleViewControlIndex
            (OracleProjection.oracleView (startupOracleProjectionState state))
        )
        opened
        (startupDiscoveryState state) of
        Left _ -> peerInvariantFault
        Right prepared ->
          let (successorDiscovery, offered) =
                Discovery.commitCandidateHello prepared
           in checkedPeerSuccess
                (replaceStartupDiscoveryState successorDiscovery state)
                ( singletonEffectBatch
                    ( SendPeerCandidate
                        (candidateHelloCandidate offered)
                        generation
                        members
                        (candidateHelloMessage offered)
                    )
                )

applyPeerHello ::
  PeerCandidate ->
  Set.Set PeerAddress ->
  PeerHello ->
  HeraldMembershipGenerationId ->
  MemberSetDigest ->
  Maybe HeraldLocator ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyPeerHello candidate localAddresses hello generation members locator predecessor
  | generation /= heraldMembershipGenerationId membership
      || members /= heraldMembershipGenerationActiveMemberSetDigest membership =
      checkedPeerSuccess
        predecessor
        ( singletonEffectBatch
            ( SetPeerCandidateDisposition
                candidate
                (PeerHelloRejected HelloMembershipMismatch)
            )
        )
  | otherwise = case Discovery.preparePeerHello candidate hello (Discovery.rememberLocalApplicationLocator locator (startupDiscoveryState predecessor)) of
      Left _ -> peerInvariantFault
      Right preparedDiscovery ->
        let (discoverySuccessor, disposition) =
              Discovery.commitPeerHello preparedDiscovery
         in case disposition of
              PeerHelloRejected _ ->
                checkedPeerSuccess
                  (replaceStartupDiscoveryState discoverySuccessor predecessor)
                  (singletonEffectBatch (SetPeerCandidateDisposition candidate disposition))
              PeerHelloAccepted _ binding -> do
                (successor, effects) <-
                  finishAcceptedHello
                    candidate
                    localAddresses
                    locator
                    disposition
                    binding
                    discoverySuccessor
                    predecessor
                let applicant = peerHelloHeraldEpoch hello
                    priorTail = Join.lookupAdmissionControlTail applicant (startupJoinState successor)
                released <- JoinControlTails.observeAcceptedPeer applicant (peerHelloAppliedControlIndex hello) successor
                reclaimed <- case (priorTail, Join.lookupAdmissionControlTail applicant (startupJoinState released)) of
                  (Just _, Nothing) -> ControlReclamation.reclaimControlPrefixOrFault released
                  _ -> pure released
                checkedPeerSuccess reclaimed effects
  where
    membership =
      OracleProjection.oracleViewCurrentHeraldMembership
        (OracleProjection.oracleView (startupOracleProjectionState predecessor))

finishAcceptedHello ::
  PeerCandidate ->
  Set.Set PeerAddress ->
  Maybe HeraldLocator ->
  PeerHelloDisposition ->
  PeerBinding ->
  Discovery.State ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
finishAcceptedHello candidate localAddresses locator disposition binding discoverySuccessor predecessor = do
  direction <- peerDirection binding predecessor
  dispatchBinding <- peerDispatchBinding binding
  preparedBinding <-
    mapStreamInvariant
      ( PeerStream.prepareDispatchBinding
          direction
          dispatchBinding
          (startupPeerStreamState predecessor)
      )
  preparedPlacement <-
    mapPlacementInvariant
      (Placement.prepareRetainedLocalSnapshotWithDiagnostics (startupDiagnosticChecks predecessor) (startupPlacementState predecessor))
  let (peerStreamSuccessor, ticket) = PeerStream.commitDispatchBinding preparedBinding
      (placementSuccessor, snapshotResult) = Placement.commitLocalSnapshot preparedPlacement
  resume <-
    mapStreamInvariant
      ( PeerStream.resumeOfferForPeer
          (peerBindingRemoteHeraldEpoch binding)
          peerStreamSuccessor
      )
  helloResponse <-
    peerHelloResponse candidate localAddresses locator discoverySuccessor predecessor
  let successor =
        replaceStartupPeerLivenessState peerLivenessSuccessor
          . replaceStartupPlacementState placementSuccessor
          . replaceStartupPeerStreamState peerStreamSuccessor
          . replaceStartupDiscoveryState discoverySuccessor
          $ predecessor
      effects =
        orderedEffectBatch
          ( maybe [] (pure . CancelTimer) cancelledTimer
              <> helloResponse
              <> [SetPeerCandidateDisposition candidate disposition]
              <> [ SendPeerControl binding (PeerKnownHeralds (Discovery.knownHeralds discoverySuccessor)),
                   SendPeerControl
                     binding
                     ( PeerPlacementUpdate
                         (FullPlacementSnapshot (Placement.localSnapshotMessage snapshotResult))
                     ),
                   SendPeerControl binding (PeerStreamResumeOffered resume)
                 ]
              <> maybe [] (\value -> [SchedulePeerDispatch value]) ticket
          )
      (peerLivenessSuccessor, cancelledTimer) =
        PeerLiveness.clearRecovery
          (peerBindingRemoteHeraldEpoch binding)
          (startupPeerLivenessState predecessor)
  checkedPeerSuccess successor effects

peerHelloResponse ::
  PeerCandidate ->
  Set.Set PeerAddress ->
  Maybe HeraldLocator ->
  Discovery.State ->
  HeraldState ->
  Either HeraldInvariantFault [HeraldEffect]
peerHelloResponse candidate localAddresses locator discovery state
  | peerCandidateInitiatorHeraldId candidate == checkedLocalHeraldId genesis = Right []
  | otherwise =
      case Discovery.prepareCandidateHello
        ( OracleProjection.oracleViewControlIndex
            (OracleProjection.oracleView (startupOracleProjectionState state))
        )
        (peerCandidateOpened (peerCandidateConnectionNonce candidate) localAddresses locator)
        discovery of
        Left _ -> peerInvariantFault
        Right prepared ->
          Right
            [ SendPeerCandidate
                candidate
                generation
                members
                (candidateHelloMessage (Discovery.preparedCandidateHello prepared))
            ]
  where
    genesis = startupGenesis state
    membership =
      OracleProjection.oracleViewCurrentHeraldMembership
        (OracleProjection.oracleView (startupOracleProjectionState state))
    generation = heraldMembershipGenerationId membership
    members = heraldMembershipGenerationActiveMemberSetDigest membership

applyPeerControl ::
  PeerBinding ->
  PeerControl ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyPeerControl binding control predecessor = do
  (successor, effects) <-
    applyPeerControlBeforeInstalledCutSettlement binding control predecessor
  checkedPeerSuccess successor effects

-- | Dispatch one control message while allowing a newly installed cut to
-- remain temporarily cross-owner incomplete.  The only caller which may let
-- that half-commit escape is 'applyPeerControlWithInstalledCuts'; its enclosing
-- Transition immediately releases source-topology waiters and settles the cut
-- before the composed Herald state escapes.
applyPeerControlBeforeInstalledCutSettlement ::
  PeerBinding ->
  PeerControl ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyPeerControlBeforeInstalledCutSettlement binding control predecessor = do
  (successor, effects, _) <-
    applyPeerControlBeforeInstalledCutSettlementWithDisposition
      binding
      control
      predecessor
  Right (successor, effects)

applyPeerControlBeforeInstalledCutSettlementWithDisposition ::
  PeerBinding ->
  PeerControl ->
  HeraldState ->
  Either
    HeraldInvariantFault
    (HeraldState, EffectBatch, PeerControlServingDisposition)
applyPeerControlBeforeInstalledCutSettlementWithDisposition binding control predecessor
  | not (bindingIsCurrent binding predecessor) =
      withServingWorkRequired
        (checkedPeerSuccess predecessor emptyEffectBatch)
  | Just target <- TerminalSourceHold.terminalSourceControlTarget control,
    target /= heraldMembershipGenerationId (OracleProjection.oracleViewCurrentHeraldMembership view) =
      withServingWorkRequired
        ( case OracleProjection.oracleViewHeraldMembershipById target view of
            Just _ -> checkedPeerSuccess predecessor emptyEffectBatch
            Nothing -> holdAheadTerminalControl binding control predecessor
        )
  | otherwise = case control of
      PeerAlignmentDeliveryProgress ->
        Right (predecessor, emptyEffectBatch, PeerControlGenerationAcknowledgementHandled)
      PeerAlignmentEvidenceDelivered sequenceNumber evidence
        | Nothing <- DeliveryOwner.evidenceKey evidence ->
            withServingWorkRequired (peerProtocolClose binding ClosePeerControlProtocol predecessor)
        | PeerDelivery.evidenceReceived binding sequenceNumber predecessor ->
            Right (PeerDelivery.recordEvidence binding sequenceNumber predecessor, emptyEffectBatch, PeerControlGenerationEvidenceUnchanged)
        | otherwise -> do
            (successor, effects, disposition) <- applyAlignmentControlWithDisposition binding evidence predecessor
            -- A close emitted while releasing older pending evidence does not
            -- undo admission of this payload. Only its own checked admission
            -- result controls whether the delivery may be confirmed.
            Right
              ( if disposition == PeerControlCurrentEvidenceRejected
                  then successor
                  else PeerDelivery.recordEvidence binding sequenceNumber successor,
                effects,
                disposition
              )
      PeerKnownHeralds contacts ->
        withServingWorkRequired (applyKnownContacts binding contacts predecessor)
      PeerPlacementUpdate update ->
        withServingWorkRequired (applyPlacementUpdate binding update predecessor)
      PeerPlacementAcknowledged acknowledgement ->
        withServingWorkRequired
          (applyPlacementAcknowledgement binding acknowledgement predecessor)
      PeerStreamReceived direction prefix ->
        withServingWorkRequired (applyReceivedAck binding direction prefix predecessor)
      PeerStreamCompleted direction prefix ->
        withServingWorkRequired (applyCompletedAck binding direction prefix predecessor)
      PeerStreamFrontierAdvanced direction nextSequence ->
        withServingWorkRequired
          (applyFrontierAdvance binding direction nextSequence predecessor)
      PeerStreamResumeOffered offer ->
        withServingWorkRequired (applyResumeOffer binding offer predecessor)
      PeerStreamResumeAccepted response ->
        withServingWorkRequired (applyResumeResponse binding response predecessor)
      PeerLabelInstalled report ->
        withServingWorkRequired $ do
          (next, effects) <- LabelCollection.receiveInstallationReport binding report predecessor
          checkedPeerSuccess next (orderedEffectBatch effects)
      PeerStructuralAppliedReported report ->
        withServingWorkRequired (applyStructuralReport binding report predecessor)
      PeerTopologyCutAnnounced announce ->
        withServingWorkRequired (applyTopologyCutAnnounce binding announce predecessor)
      PeerTopologyCutAccepted acceptance ->
        withServingWorkRequired
          (applyTopologyCutAcceptance binding acceptance predecessor)
      PeerTopologyCutEstablished established ->
        withServingWorkRequired
          (applyTopologyCutEstablished binding established predecessor)
      PeerTopologyCutEstablishedAcknowledged acknowledgement ->
        withServingWorkRequired
          (applyTopologyCutEstablishedAck binding acknowledgement predecessor)
      PeerPreparationControl preparation ->
        withServingWorkRequired (ProcessPreparation.applyPeerPreparationControl binding preparation predecessor)
      PeerAlignmentControl alignment ->
        applyAlignmentControlWithDisposition binding alignment predecessor
      PeerTerminalSourceInventoryAdvertised inventory ->
        withServingWorkRequired
          (applyTerminalSourceInventory binding inventory predecessor)
      PeerTerminalSourcePayloadRequested request ->
        withServingWorkRequired
          (applyTerminalSourcePayloadRequest binding request predecessor)
      PeerTerminalSourcePayloadRelayed relay ->
        withServingWorkRequired
          (applyTerminalSourcePayloadRelay binding relay predecessor)
      PeerTerminalSourceUnionAnnounced announce ->
        withServingWorkRequired
          (applyTerminalSourceUnionAnnounce binding announce predecessor)
      PeerTerminalSourceUnionAccepted acceptance ->
        withServingWorkRequired
          (applyTerminalSourceUnionAcceptance binding acceptance predecessor)
      PeerTerminalSourceUnionEstablished established ->
        withServingWorkRequired
          (applyTerminalSourceUnionEstablished binding established predecessor)
      -- Transition consumes the direct-probe family before this ordinary peer
      -- coordinator so the failure owner can compose its timer and retained
      -- Oracle intention atomically. Reaching either arm here is therefore a
      -- kernel composition fault, not an untrusted-peer protocol outcome.
      PeerDirectFailureProbeRequested {} -> peerInvariantFault
      PeerDirectFailureProbeResponded {} -> peerInvariantFault
  where
    view = OracleProjection.oracleView (startupOracleProjectionState predecessor)

terminalMembershipHistory :: HeraldState -> HeraldMembershipHistory
terminalMembershipHistory = OracleProjection.oracleViewHeraldMembershipHistoryChecked . OracleProjection.oracleView . startupOracleProjectionState

withServingWorkRequired ::
  PeerTransition ->
  Either
    HeraldInvariantFault
    (HeraldState, EffectBatch, PeerControlServingDisposition)
withServingWorkRequired transition = do
  (successor, effects) <- transition
  Right (successor, effects, PeerControlServingWorkRequired)

applyAlignmentControlWithDisposition ::
  PeerBinding ->
  AlignmentControl ->
  HeraldState ->
  Either
    HeraldInvariantFault
    (HeraldState, EffectBatch, PeerControlServingDisposition)
applyAlignmentControlWithDisposition binding (AlignmentProbeMarker probe subscription revision) predecessor =
  case DisappearanceLive.receiveDisappearanceAlignmentMarker
    (peerBindingRemoteHeraldEpoch binding)
    (DisappearanceProtocol.disappearanceAlignmentMarker probe (DisappearanceProtocol.outgoingAlignmentCut subscription revision))
    predecessor of
    Left _ -> withServingWorkRequired (peerProtocolClose binding ClosePeerControlProtocol predecessor)
    Right successor -> Right (successor, mempty, PeerControlServingWorkRequired)
applyAlignmentControlWithDisposition binding control predecessor =
  case Alignment.applyAlignmentGenerationControlWithDisposition binding control predecessor of
    Left Alignment.AlignmentGenerationControlProtocolViolation ->
      do
        (successor, effects) <- peerProtocolClose binding ClosePeerControlProtocol predecessor
        Right (successor, effects, PeerControlCurrentEvidenceRejected)
    Left (Alignment.AlignmentGenerationControlInvariantViolation _) ->
      peerInvariantFault
    Right Nothing ->
      withServingWorkRequired
        (applyAlignmentTransferControl binding control predecessor)
    Right (Just (successor, effects, disposition)) ->
      Right
        ( successor,
          effects,
          case disposition of
            Alignment.AlignmentGenerationControlWorkRequired ->
              PeerControlServingWorkRequired
            Alignment.AlignmentGenerationEvidenceUnchanged ->
              PeerControlGenerationEvidenceUnchanged
        )

-- | Explicit seam for snapshot/change subscription fulfilment.  Increment 5's
-- transfer owner replaces this fallback; generation controls never enter it.
applyAlignmentTransferControl ::
  PeerBinding -> AlignmentControl -> HeraldState -> PeerTransition
applyAlignmentTransferControl binding control predecessor =
  case AlignmentTransfer.applyAlignmentTransferControl binding control predecessor of
    Left AlignmentTransfer.AlignmentTransferRemoteProtocolViolation ->
      peerProtocolClose binding ClosePeerControlProtocol predecessor
    Left AlignmentTransfer.AlignmentTransferOwnerContradiction ->
      peerInvariantFault
    Right (successor, effects) ->
      checkedPeerSuccess successor effects

-- | An active reporter carries complete established evidence and the bytes it
-- actually retains. Repair the historical anchor before resealing this target;
-- an advertised certificate never changes Graph authority by itself.
applyTerminalSourceInventory ::
  PeerBinding -> TerminalSource.TerminalSourceInventory -> HeraldState -> PeerTransition
applyTerminalSourceInventory binding inventory predecessor =
  case currentTerminalCoordinator binding predecessor of
    Nothing -> holdAheadTerminalControl binding control predecessor
    Just coordinator
      | TerminalSource.terminalSourceInventoryReporter inventory /= peerBindingRemoteHeraldEpoch binding ->
          peerProtocolClose binding ClosePeerControlProtocol predecessor
      | StructuralBase.structuralBaseEvidence coordinator /= Nothing,
        inventory `elem` fmap snd (StructuralBase.structuralBaseInventoryEntries coordinator) ->
          checkedPeerSuccess predecessor emptyEffectBatch
      | otherwise -> case fullTerminalLineage predecessor of
          Nothing -> peerInvariantFault
          Just history -> case StructuralBase.structuralBasePayloadRequestsForInventory history inventory coordinator of
            Left _ -> peerProtocolClose binding ClosePeerControlProtocol predecessor
            Right requests -> do
              retainedEvidence <- either (const peerInvariantFault) Right (StructuralBase.retainStructuralBaseInventoryEvidence history inventory coordinator)
              let withEvidence = retainTerminalCoordinator retainedEvidence predecessor
              (held, _) <- holdCurrentTerminalControl retainedEvidence binding control withEvidence
              if not (null requests)
                then checkedPeerSuccess held (orderedEffectBatch [SendPeerControl binding (PeerTerminalSourcePayloadRequested request) | request <- requests])
                else case materializeTerminalPayloadArchive retainedEvidence held of
                  Left _ -> peerInvariantFault
                  Right (materialized, materializationEffects) ->
                    case repairTerminalInventoryAnchor inventory materialized of
                      Left problem
                        | terminalStructuralBaseNotReady problem -> checkedPeerSuccess materialized materializationEffects
                        | otherwise -> peerProtocolClose binding ClosePeerControlProtocol predecessor
                      Right repaired -> do
                        (rebased, rebaseEffects) <- rebaseTerminalCoordinatorFromInstalled repaired
                        case currentTerminalCoordinator binding rebased of
                          Nothing -> peerInvariantFault
                          Just current
                            | terminalInventoryAnchorInstalled inventory rebased ->
                                case StructuralBase.receiveStructuralBaseInventory inventory current of
                                  Left _ -> peerProtocolClose binding ClosePeerControlProtocol predecessor
                                  Right admitted -> do
                                    (advanced, effects) <- advanceTerminalSourceCoordinator admitted (consumeHeldTerminalControl binding control rebased)
                                    checkedPeerSuccess advanced (materializationEffects <> rebaseEffects <> effects)
                            | terminalInventoryAnchorObsolete inventory rebased ->
                                checkedPeerSuccess (consumeHeldTerminalControl binding control rebased) (materializationEffects <> rebaseEffects <> terminalInventoryOffer binding current)
                            | otherwise -> checkedPeerSuccess rebased (materializationEffects <> rebaseEffects)
  where
    control = PeerTerminalSourceInventoryAdvertised inventory

applyTerminalSourcePayloadRequest ::
  PeerBinding -> TerminalSource.TerminalSourcePayloadRequest -> HeraldState -> PeerTransition
applyTerminalSourcePayloadRequest binding request predecessor =
  case (currentTerminalCoordinator binding predecessor, fullTerminalLineage predecessor, retainedInventoryForRequest request predecessor) of
    (Just coordinator, Just history, Just inventory)
      | TerminalSource.terminalSourceInventoryReporter inventory == checkedLocalHeraldEpoch (startupGenesis predecessor) ->
          case StructuralBase.prepareStructuralBaseEvidencePayloadRelay history inventory request coordinator of
            Right relay -> checkedPeerSuccess predecessor (singletonEffectBatch (SendPeerControl binding (PeerTerminalSourcePayloadRelayed relay)))
            Left (StructuralBase.StructuralBasePayloadUnavailable _) -> peerInvariantFault
            Left _ -> peerProtocolClose binding ClosePeerControlProtocol predecessor
    _ -> holdAheadTerminalControl binding (PeerTerminalSourcePayloadRequested request) predecessor

applyTerminalSourcePayloadRelay ::
  PeerBinding -> TerminalSource.TerminalSourcePayloadRelay -> HeraldState -> PeerTransition
applyTerminalSourcePayloadRelay binding relay predecessor =
  case (currentTerminalCoordinator binding predecessor, fullTerminalLineage predecessor, requestFromRelay relay) of
    (Just coordinator, Just history, Right request)
      | TerminalSource.terminalSourcePayloadRelayHerald relay == peerBindingRemoteHeraldEpoch binding,
        Just inventory <- retainedInventoryForRequest request predecessor,
        TerminalSource.terminalSourceInventoryReporter inventory == peerBindingRemoteHeraldEpoch binding ->
          case StructuralBase.receiveStructuralBaseEvidencePayloadRelay history inventory request relay coordinator of
            Left _ -> peerProtocolClose binding ClosePeerControlProtocol predecessor
            Right repaired
              | StructuralBase.structuralBasePayloadEntries repaired == StructuralBase.structuralBasePayloadEntries coordinator ->
                  checkedPeerSuccess (retainTerminalCoordinator repaired predecessor) emptyEffectBatch
              | otherwise -> case materializeTerminalPayloadArchive repaired (retainTerminalCoordinator repaired predecessor) of
                  Left problem -> case PeerInput.peerInputProblemDisposition problem of
                    PeerInput.RejectCurrentPeerBinding -> peerProtocolClose binding ClosePeerControlProtocol predecessor
                    PeerInput.PeerInputInvariantFault -> peerInvariantFault
                  Right (materialized, effects) -> do
                    (advanced, coordinatorEffects) <- advanceTerminalSourceCoordinator repaired materialized
                    checkedPeerSuccess advanced (effects <> coordinatorEffects)
    _ -> peerProtocolClose binding ClosePeerControlProtocol predecessor

fullTerminalLineage :: HeraldState -> Maybe HeraldMembershipLineage
fullTerminalLineage state =
  let history = terminalMembershipHistory state
   in either (const Nothing) Just (heraldMembershipLineage (heraldMembershipGenerationId (heraldMembershipHistoryGenesis history)) (heraldMembershipGenerationId (heraldMembershipHistoryCurrent history)) history)

retainedInventoryForRequest :: TerminalSource.TerminalSourcePayloadRequest -> HeraldState -> Maybe TerminalSource.TerminalSourceInventory
retainedInventoryForRequest request state = find ((== TerminalSource.terminalSourcePayloadRequestSupportingInventory request) . TerminalSource.terminalSourceInventoryDigest) inventories
  where
    inventories =
      maybe [] (fmap snd . StructuralBase.structuralBaseRetainedInventoryEntries) (startupStructuralBaseCoordinator state)
        <> [inventory | (_, PeerTerminalSourceInventoryAdvertised inventory) <- TerminalSourceHold.heldTerminalSourceControls (startupTerminalSourceHoldState state)]

requestFromRelay :: TerminalSource.TerminalSourcePayloadRelay -> Either TerminalSource.TerminalSourceProblem TerminalSource.TerminalSourcePayloadRequest
requestFromRelay relay =
  TerminalSource.terminalSourcePayloadRequestFromClaimedCoordinates
    (TerminalSource.terminalSourcePayloadRelayPredecessorGenerationId relay)
    (TerminalSource.terminalSourcePayloadRelaySuccessorGenerationId relay)
    (TerminalSource.terminalSourcePayloadRelayRetiredSource relay)
    (TerminalSource.terminalStructuralOccurrenceId (TerminalSource.terminalSourcePayloadRelayOccurrence relay))
    (TerminalSource.terminalSourcePayloadRelaySupportingInventory relay)

terminalInventoryOffer :: PeerBinding -> TerminalSource.MembershipBaseClosure -> EffectBatch
terminalInventoryOffer binding coordinator =
  orderedEffectBatch
    [SendPeerControl binding (PeerTerminalSourceInventoryAdvertised inventory) | inventory <- StructuralBase.structuralBaseLocalInventories coordinator]

terminalInventoryAnchorInstalled :: TerminalSource.TerminalSourceInventory -> HeraldState -> Bool
terminalInventoryAnchorInstalled inventory state =
  TerminalSource.terminalSourceInventoryEstablishedChain inventory == GraphProgress.structuralInstalledEstablishedChain (startupStructuralProgressState state)

terminalInventoryAnchorObsolete :: TerminalSource.TerminalSourceInventory -> HeraldState -> Bool
terminalInventoryAnchorObsolete inventory state =
  let advertised = TerminalSource.terminalSourceInventoryEstablishedChain inventory
      installed = GraphProgress.structuralInstalledEstablishedChain (startupStructuralProgressState state)
   in length advertised < length installed && take (length advertised) installed == advertised

repairTerminalInventoryAnchor :: TerminalSource.TerminalSourceInventory -> HeraldState -> Either StructuralBase.StructuralBaseProblem HeraldState
repairTerminalInventoryAnchor inventory initial = do
  let advertised = TerminalSource.terminalSourceInventoryEstablishedChain inventory
      installed = GraphProgress.structuralInstalledEstablishedChain (startupStructuralProgressState initial)
      common = min (length advertised) (length installed)
  unless (take common advertised == take common installed) (Left StructuralBase.StructuralBaseEstablishedLineageConflict)
  installReadyPrefix initial (drop (length installed) advertised)
  where
    installReadyPrefix state [] = Right state
    installReadyPrefix state (certificate : remaining) = case install state certificate of
      Left problem
        | terminalStructuralBaseNotReady problem -> Right state
        | otherwise -> Left problem
      Right successor -> installReadyPrefix successor remaining
    install state certificate = do
      let cut = topologyCutEstablishedCut certificate
          progress = startupStructuralProgressState state
          view = OracleProjection.oracleView (startupOracleProjectionState state)
          graph = either (Left . StructuralBase.StructuralBaseGraphProgressProblem) Right
          terminal = either (Left . StructuralBase.StructuralBaseTerminalSourceProblem) Right
      preparedProgress <- case topologyPredecessorView (topologyCutPredecessor cut) of
        MembershipSuccessorPredecessorView _ origin target _ _ _ -> do
          lineage <- maybe (Left StructuralBase.StructuralBaseEstablishedLineageConflict) Right (OracleProjection.oracleViewHeraldMembershipLineage origin target view)
          established <-
            maybe
              (Left StructuralBase.StructuralBaseEstablishedLineageConflict)
              Right
              (find (\candidate -> TerminalSource.terminalSourceUnionPredecessorCutId (TerminalSource.terminalSourceUnionEstablishedUnion candidate) == topologyPredecessorCutId (topologyCutPredecessor cut) && TerminalSource.terminalSourceUnionSuccessorGenerationId (TerminalSource.terminalSourceUnionEstablishedUnion candidate) == target) (TerminalSource.terminalSourceInventoryEstablishedBases inventory))
          base <- terminal (TerminalSource.establishedSuccessorStructuralBase lineage established cut)
          GraphProgress.commitMembershipSuccessorBaseInstallation <$> graph (GraphProgress.prepareMembershipSuccessorBaseInstallation (heraldMembershipLineageTarget lineage) base cut progress)
        _ -> do
          prepared <- graph (GraphProgress.prepareTopologyCutEstablished (structuralReconciliationViews state) (topologyCheckpointFor (topologyFrontierAppliedControlPrefix (topologyCutFrontier cut)) state) certificate progress)
          case GraphProgress.preparedTopologyCutEstablishedClassification prepared of
            GraphProgress.TopologyCutEstablishedHeld -> Left (StructuralBase.StructuralBaseAnchorNotInstalled (topologyCutEstablishedId certificate))
            _ -> Right (GraphProgress.commitTopologyCutEstablished prepared)
      -- The full canonical certificate must equal the ordinary Graph certificate
      -- at this exact cut, including its complete original reporter evidence.
      checked <- graph (GraphProgress.prepareTopologyCutEstablished (structuralReconciliationViews state) (topologyCheckpointFor (topologyFrontierAppliedControlPrefix (topologyCutFrontier cut)) state) certificate preparedProgress)
      pure (replaceStartupStructuralProgressState (GraphProgress.commitTopologyCutEstablished checked) state)

rebaseTerminalCoordinatorFromInstalled :: HeraldState -> PeerTransition
rebaseTerminalCoordinatorFromInstalled state = case startupStructuralBaseCoordinator state of
  Nothing -> checkedPeerSuccess state emptyEffectBatch
  Just coordinator
    | Just installed <- StructuralBase.structuralBaseEvidence coordinator,
      GraphProgress.structuralSuccessorBaseInstalled installed progress ->
        checkedPeerSuccess state emptyEffectBatch
    | StructuralBase.structuralBaseAnchorCutId coordinator == GraphProgress.structuralLastInstalledCutId progress -> checkedPeerSuccess state emptyEffectBatch
    | GraphProgress.structuralProgressMembershipGenerationId progress == heraldMembershipGenerationId (StructuralBase.structuralBaseSuccessorMembership coordinator) -> checkedPeerSuccess state emptyEffectBatch
    | otherwise -> case OracleProjection.oracleViewHeraldMembershipLineage (GraphProgress.structuralProgressMembershipGenerationId progress) (heraldMembershipGenerationId (StructuralBase.structuralBaseSuccessorMembership coordinator)) view of
        Nothing -> peerInvariantFault
        Just lineage -> case (fullTerminalLineage state, GraphProgress.lookupInstalledTopologyCut (GraphProgress.structuralLastInstalledCutId progress) progress) of
          (Just history, Just installed) -> case TerminalSource.terminalSourceInstalledPredecessorBase (heraldMembershipLineageOrigin lineage) (GraphProgress.installedTopologyCutCut installed) of
            Left _ -> peerInvariantFault
            Right base -> case StructuralBase.rebaseStructuralBaseAnchor history lineage base (GraphProgress.structuralInstalledEstablishedChain progress) (fmap TerminalSource.successorStructuralBaseEstablishedUnion (GraphProgress.structuralInstalledSuccessorBases progress)) coordinator of
              Left _ -> peerInvariantFault
              Right replacement -> checkedPeerSuccess (retainTerminalCoordinator replacement state) (orderedEffectBatch [SendPeerControl binding (PeerTerminalSourceInventoryAdvertised inventory) | binding <- Discovery.currentPeerBindings (startupDiscoveryState state), inventory <- StructuralBase.structuralBaseLocalInventories replacement])
          _ -> peerInvariantFault
  where
    progress = startupStructuralProgressState state
    view = OracleProjection.oracleView (startupOracleProjectionState state)

-- | Re-drive every exact payload retained by the terminal coordinator in
-- source order.  An ahead occurrence is a pure hold; when a newly relayed gap
-- is filled, later retained occurrences are reconsidered in the same atomic
-- owner transition.  No ordinary peer-stream assignment is invented.
materializeTerminalPayloadArchive ::
  TerminalSource.MembershipBaseClosure ->
  HeraldState ->
  Either PeerInput.PeerInputProblem (HeraldState, EffectBatch)
materializeTerminalPayloadArchive coordinator predecessor = do
  (successor, reverseEffects) <-
    foldM
      materializeOne
      (predecessor, [])
      (StructuralBase.structuralBasePayloadEntries coordinator)
  let progressAdvanced =
        startupStructuralProgressState successor
          /= startupStructuralProgressState predecessor
      reportEffects =
        [ SendPeerControl
            current
            ( PeerStructuralAppliedReported
                ( GraphProgress.structuralLocalReport
                    (startupStructuralProgressState successor)
                )
            )
        | progressAdvanced,
          current <- StructuralProgress.structuralReportBindings (startupStructuralProgressState successor) (Discovery.currentPeerBindings (startupDiscoveryState successor))
        ]
  Right
    ( successor,
      orderedEffectBatch (reverse reverseEffects <> reportEffects)
    )
  where
    materializeOne (state, reverseEffects) (_, occurrence) = do
      (owners, result) <-
        PeerInput.materializeTerminalStructuralOccurrence
          (terminalPeerInputContext state)
          occurrence
          (terminalPeerInputState state)
      let successor = installTerminalPeerInputState owners state
          effects = terminalMaterializationEffects successor result
      Right (successor, reverse effects <> reverseEffects)

terminalPeerInputContext :: HeraldState -> PeerInput.PeerInputContext
terminalPeerInputContext state =
  PeerInput.peerInputContext
    (startupGenesis state)
    (startupOracleProjectionState state)
    (startupDiscoveryState state)
    (startupPlacementState state)

terminalPeerInputState :: HeraldState -> PeerInput.PeerInputState
terminalPeerInputState state =
  PeerInput.peerInputState
    (startupPublicationState state)
    (startupPeerStreamState state)
    (startupStoreState state)
    (startupGraphState state)
    (startupIdGeneratorState state)
    (startupStructuralProgressState state)
    (startupAlignmentState state)
    (startupPlacementState state)
    (startupControlledState state)
    (startupSortRegistryState state)
    (startupApplicationState state)
    (startupWaitState state)
    (startupLabelBarrierState state)
    (startupDisappearanceState state)

installTerminalPeerInputState :: PeerInput.PeerInputState -> HeraldState -> HeraldState
installTerminalPeerInputState owners =
  replaceStartupWaitState (PeerInput.peerInputWaitState owners)
    . replaceStartupLabelBarrierState (PeerInput.peerInputLabelBarrierState owners)
    . replaceStartupDisappearanceState (PeerInput.peerInputDisappearanceState owners)
    . replaceStartupApplicationState (PeerInput.peerInputApplicationState owners)
    . replaceStartupSortRegistryState (PeerInput.peerInputSortRegistryState owners)
    . replaceStartupControlledState (PeerInput.peerInputControlledState owners)
    . replaceStartupPlacementState (PeerInput.peerInputPlacementState owners)
    . replaceStartupAlignmentState (PeerInput.peerInputAlignmentState owners)
    . replaceStartupStructuralProgressState
      (PeerInput.peerInputStructuralProgressState owners)
    . replaceStartupIdGeneratorState (PeerInput.peerInputIdGeneratorState owners)
    . replaceStartupGraphState (PeerInput.peerInputGraphState owners)
    . replaceStartupStoreState (PeerInput.peerInputStoreState owners)
    . replaceStartupPeerStreamState (PeerInput.peerInputPeerStreamState owners)
    . replaceStartupPublicationState (PeerInput.peerInputPublicationState owners)

terminalMaterializationEffects ::
  HeraldState ->
  PeerInput.TerminalStructuralMaterializationResult ->
  [HeraldEffect]
terminalMaterializationEffects state result =
  [ SendPeerControl binding (PeerPlacementUpdate (FullPlacementSnapshot snapshot))
  | snapshot <- PeerInput.terminalStructuralMaterializationPlacementSnapshots result,
    binding <- Discovery.currentPeerBindings (startupDiscoveryState state)
  ]
    <> fmap
      ( \wake ->
          SendApplicationWaitWake
            (Application.applicationWaitWakeBinding wake)
            (Application.applicationWaitWakeCursor wake)
            (Application.applicationWaitWakeRequestId wake)
            (Application.applicationWaitWakeWaitId wake)
            (Application.applicationWaitWakeResult wake)
      )
      (PeerInput.terminalStructuralMaterializationWakes result)

applyTerminalSourceUnionAnnounce ::
  PeerBinding -> TerminalSource.TerminalSourceUnionAnnounce -> HeraldState -> PeerTransition
applyTerminalSourceUnionAnnounce binding announce predecessor =
  case currentTerminalCoordinator binding predecessor of
    Nothing ->
      holdAheadTerminalControl
        binding
        (PeerTerminalSourceUnionAnnounced announce)
        predecessor
    Just coordinator
      | TerminalSource.terminalSourceUnionAnnounceAnnouncer announce
          /= peerBindingRemoteHeraldEpoch binding ->
          peerProtocolClose binding ClosePeerControlProtocol predecessor
      | StructuralBase.structuralBaseEvidence coordinator /= Nothing,
        StructuralBase.structuralBaseAgreedUnion coordinator == Just (TerminalSource.terminalSourceUnionAnnounceUnion announce),
        Just acceptance <- lookup (StructuralBase.structuralBaseLocalHerald coordinator) (StructuralBase.structuralBaseAcceptanceEntries coordinator) ->
          checkedPeerSuccess predecessor (singletonEffectBatch (SendPeerControl binding (PeerTerminalSourceUnionAccepted acceptance)))
      | TerminalSource.terminalSourceUnionPredecessorCutId (TerminalSource.terminalSourceUnionAnnounceUnion announce) /= StructuralBase.structuralBaseAnchorCutId coordinator ->
          holdOrDiscardTerminalUnionAnchor coordinator binding (PeerTerminalSourceUnionAnnounced announce) (TerminalSource.terminalSourceUnionAnnounceUnion announce) predecessor
      | otherwise ->
          case StructuralBase.acceptStructuralBaseUnionAnnouncement
            (terminalTopologyDigestReconstructor predecessor)
            (GraphProgress.structuralAppliedVector progress)
            (GraphProgress.structuralAppliedControlPrefix progress)
            announce
            coordinator of
            Left problem
              | terminalStructuralBaseRemoteControlNotReady problem ->
                  holdCurrentTerminalControl
                    coordinator
                    binding
                    (PeerTerminalSourceUnionAnnounced announce)
                    predecessor
              | otherwise ->
                  peerProtocolClose binding ClosePeerControlProtocol predecessor
            Right (successor, acceptance) ->
              checkedPeerSuccess
                ( retainTerminalCoordinator
                    successor
                    ( consumeHeldTerminalControl
                        binding
                        (PeerTerminalSourceUnionAnnounced announce)
                        predecessor
                    )
                )
                ( singletonEffectBatch
                    (SendPeerControl binding (PeerTerminalSourceUnionAccepted acceptance))
                )
  where
    progress = startupStructuralProgressState predecessor

applyTerminalSourceUnionAcceptance ::
  PeerBinding -> TerminalSource.TerminalSourceUnionAcceptance -> HeraldState -> PeerTransition
applyTerminalSourceUnionAcceptance binding acceptance predecessor =
  case currentTerminalCoordinator binding predecessor of
    Nothing ->
      holdAheadTerminalControl
        binding
        (PeerTerminalSourceUnionAccepted acceptance)
        predecessor
    Just coordinator
      | TerminalSource.terminalSourceUnionAcceptanceReporter acceptance
          /= peerBindingRemoteHeraldEpoch binding ->
          peerProtocolClose binding ClosePeerControlProtocol predecessor
      | fmap TerminalSource.terminalSourceUnionDigest (StructuralBase.structuralBaseAgreedUnion coordinator) /= Just (TerminalSource.terminalSourceUnionAcceptanceUnionDigest acceptance) ->
          checkedPeerSuccess predecessor (terminalInventoryOffer binding coordinator)
      | otherwise ->
          case StructuralBase.receiveStructuralBaseUnionAcceptance acceptance coordinator of
            Left _ -> peerProtocolClose binding ClosePeerControlProtocol predecessor
            Right successor ->
              case StructuralBase.structuralBaseEstablishedUnion successor of
                Just established ->
                  checkedPeerSuccess
                    (retainTerminalCoordinator successor predecessor)
                    ( singletonEffectBatch
                        (SendPeerControl binding (PeerTerminalSourceUnionEstablished established))
                    )
                Nothing -> completeTerminalSourceUnionIfReady successor predecessor emptyEffectBatch

applyTerminalSourceUnionEstablished ::
  PeerBinding -> TerminalSource.TerminalSourceUnionEstablished -> HeraldState -> PeerTransition
applyTerminalSourceUnionEstablished binding established predecessor =
  case currentTerminalCoordinator binding predecessor of
    Nothing ->
      holdAheadTerminalControl
        binding
        (PeerTerminalSourceUnionEstablished established)
        predecessor
    Just coordinator
      | peerBindingRemoteHeraldEpoch binding
          /= TerminalSource.terminalSourceUnionAnnouncer
            (StructuralBase.structuralBaseSuccessorMembership coordinator) ->
          peerProtocolClose binding ClosePeerControlProtocol predecessor
      | TerminalSource.terminalSourceUnionPredecessorCutId (TerminalSource.terminalSourceUnionEstablishedUnion established) /= StructuralBase.structuralBaseAnchorCutId coordinator ->
          holdOrDiscardTerminalUnionAnchor coordinator binding (PeerTerminalSourceUnionEstablished established) (TerminalSource.terminalSourceUnionEstablishedUnion established) predecessor
      | StructuralBase.structuralBaseEvidence coordinator /= Nothing ->
          if StructuralBase.structuralBaseEstablishedUnion coordinator == Just established
            then
              checkedPeerSuccess
                ( consumeHeldTerminalControl
                    binding
                    (PeerTerminalSourceUnionEstablished established)
                    predecessor
                )
                emptyEffectBatch
            else peerProtocolClose binding ClosePeerControlProtocol predecessor
      | otherwise ->
          case StructuralBase.receiveStructuralBaseUnionEstablished
            (terminalTopologyDigestReconstructor predecessor)
            (GraphProgress.structuralAppliedVector progress)
            (GraphProgress.structuralAppliedControlPrefix progress)
            established
            coordinator of
            Left problem
              | terminalStructuralBaseRemoteControlNotReady problem ->
                  holdCurrentTerminalControl
                    coordinator
                    binding
                    (PeerTerminalSourceUnionEstablished established)
                    predecessor
              | otherwise ->
                  peerProtocolClose binding ClosePeerControlProtocol predecessor
            Right successor ->
              installTerminalSourceBase
                successor
                ( consumeHeldTerminalControl
                    binding
                    (PeerTerminalSourceUnionEstablished established)
                    predecessor
                )
                emptyEffectBatch
  where
    progress = startupStructuralProgressState predecessor

holdOrDiscardTerminalUnionAnchor :: TerminalSource.MembershipBaseClosure -> PeerBinding -> PeerControl -> TerminalSource.TerminalSourceUnion -> HeraldState -> PeerTransition
holdOrDiscardTerminalUnionAnchor coordinator binding control union state
  | TerminalSource.terminalSourceUnionPredecessorCutId union `elem` GraphProgress.structuralInstalledCutIds (startupStructuralProgressState state) =
      checkedPeerSuccess (consumeHeldTerminalControl binding control state) (terminalInventoryOffer binding coordinator)
  | otherwise = do
      (held, effects) <- holdCurrentTerminalControl coordinator binding control state
      checkedPeerSuccess held (effects <> terminalInventoryOffer binding coordinator)

advanceTerminalSourceCoordinator ::
  TerminalSource.MembershipBaseClosure -> HeraldState -> PeerTransition
advanceTerminalSourceCoordinator coordinator predecessor =
  case StructuralBase.structuralBasePayloadRequests coordinator of
    Left _ -> peerInvariantFault
    Right requests
      | not (null requests) ->
          checkedPeerSuccess
            retained
            (orderedEffectBatch (payloadRequestEffects coordinator predecessor requests))
      | not (terminalInventorySetComplete coordinator) ->
          checkedPeerSuccess retained emptyEffectBatch
      | local /= announcer ->
          checkedPeerSuccess retained emptyEffectBatch
      | StructuralBase.structuralBaseAgreedUnion coordinator == Nothing ->
          case StructuralBase.prepareStructuralBaseUnionAnnouncement
            (terminalTopologyDigestReconstructor retained)
            coordinator of
            Left problem
              | terminalStructuralBaseNotReady problem ->
                  checkedPeerSuccess retained emptyEffectBatch
              | otherwise -> peerInvariantFault
            Right (withUnion, announce) ->
              acceptLocalTerminalSourceUnion
                withUnion
                announce
                retained
                (broadcastTerminalControl retained (PeerTerminalSourceUnionAnnounced announce))
      | terminalLocalAcceptanceMissing coordinator ->
          case StructuralBase.structuralBaseAgreedUnion coordinator of
            Nothing -> peerInvariantFault
            Just union ->
              case TerminalSource.terminalSourceUnionAnnounce
                (StructuralBase.structuralBaseSuccessorMembership coordinator)
                local
                union of
                Left _ -> peerInvariantFault
                Right announce ->
                  acceptLocalTerminalSourceUnion
                    coordinator
                    announce
                    retained
                    emptyEffectBatch
      | otherwise ->
          completeTerminalSourceUnionIfReady coordinator retained emptyEffectBatch
  where
    retained = retainTerminalCoordinator coordinator predecessor
    local = StructuralBase.structuralBaseLocalHerald coordinator
    announcer =
      TerminalSource.terminalSourceUnionAnnouncer
        (StructuralBase.structuralBaseSuccessorMembership coordinator)

acceptLocalTerminalSourceUnion ::
  TerminalSource.MembershipBaseClosure ->
  TerminalSource.TerminalSourceUnionAnnounce ->
  HeraldState ->
  EffectBatch ->
  PeerTransition
acceptLocalTerminalSourceUnion coordinator announce predecessor effects =
  case StructuralBase.acceptStructuralBaseUnionAnnouncement
    (terminalTopologyDigestReconstructor predecessor)
    (GraphProgress.structuralAppliedVector progress)
    (GraphProgress.structuralAppliedControlPrefix progress)
    announce
    coordinator of
    Left problem
      | terminalStructuralBaseNotReady problem ->
          checkedPeerSuccess
            (retainTerminalCoordinator coordinator predecessor)
            effects
      | otherwise -> peerInvariantFault
    Right (withAcceptance, _) ->
      completeTerminalSourceUnionIfReady withAcceptance predecessor effects
  where
    progress = startupStructuralProgressState predecessor

terminalLocalAcceptanceMissing ::
  TerminalSource.MembershipBaseClosure -> Bool
terminalLocalAcceptanceMissing coordinator =
  all
    ((/= StructuralBase.structuralBaseLocalHerald coordinator) . fst)
    (StructuralBase.structuralBaseAcceptanceEntries coordinator)

completeTerminalSourceUnionIfReady ::
  TerminalSource.MembershipBaseClosure ->
  HeraldState ->
  EffectBatch ->
  PeerTransition
completeTerminalSourceUnionIfReady coordinator predecessor effects
  | StructuralBase.structuralBaseEstablishedUnion coordinator /= Nothing =
      installTerminalSourceBase coordinator predecessor effects
  | Set.fromList (fmap fst (StructuralBase.structuralBaseAcceptanceEntries coordinator))
      /= terminalSuccessorMembers coordinator =
      checkedPeerSuccess (retainTerminalCoordinator coordinator predecessor) effects
  | otherwise =
      case StructuralBase.establishStructuralBaseUnion coordinator of
        Left _ -> peerInvariantFault
        Right (establishedCoordinator, established) ->
          installTerminalSourceBase
            establishedCoordinator
            predecessor
            (effects <> broadcastTerminalControl predecessor (PeerTerminalSourceUnionEstablished established))

installTerminalSourceBase ::
  TerminalSource.MembershipBaseClosure ->
  HeraldState ->
  EffectBatch ->
  PeerTransition
installTerminalSourceBase coordinator predecessor effects
  | StructuralBase.structuralBaseEvidence coordinator /= Nothing =
      checkedPeerSuccess (retainTerminalCoordinator coordinator predecessor) effects
  | otherwise =
      case StructuralBase.installSuccessorStructuralBase
        (startupStructuralProgressState predecessor)
        coordinator of
        Right (progress, successor) ->
          let installed =
                replaceStartupStructuralProgressState
                  progress
                  (retainTerminalCoordinator successor predecessor)
           in do
                (replayed, replayEffects) <-
                  replayHeldTerminalSourceReadiness installed
                checkedPeerSuccess replayed (effects <> replayEffects)
        Left problem
          | terminalStructuralBaseNotReady problem ->
              checkedPeerSuccess (retainTerminalCoordinator coordinator predecessor) effects
          | otherwise -> peerInvariantFault

currentTerminalCoordinator ::
  PeerBinding -> HeraldState -> Maybe TerminalSource.MembershipBaseClosure
currentTerminalCoordinator binding state = do
  coordinator <- startupStructuralBaseCoordinator state
  let view = OracleProjection.oracleView (startupOracleProjectionState state)
      predecessorMembership = StructuralBase.structuralBasePredecessorMembership coordinator
      successorMembership = StructuralBase.structuralBaseSuccessorMembership coordinator
      predecessorId = heraldMembershipGenerationId predecessorMembership
      remote = peerBindingRemoteHeraldEpoch binding
  if OracleProjection.oracleViewCurrentHeraldMembership view == successorMembership
    && OracleProjection.oracleViewHeraldMembershipById predecessorId view
      == Just predecessorMembership
    && Set.member remote (terminalSuccessorMembers coordinator)
    then Just coordinator
    else Nothing

-- | Checked terminal controls may outrun this Herald's Oracle watch by several
-- membership successors. Retain each exact authenticated coordinate; known old
-- targets are inert and conflicting controls keep ordinary protocol rejection.
-- Transition owns authoritative replay after projection.
holdAheadTerminalControl ::
  PeerBinding -> PeerControl -> HeraldState -> PeerTransition
holdAheadTerminalControl binding control predecessor =
  case disposition of
    TerminalSourceHold.TerminalSourceControlRejected ->
      peerProtocolClose binding ClosePeerControlProtocol predecessor
    TerminalSourceHold.TerminalSourceControlHeld -> retained
    TerminalSourceHold.TerminalSourceControlAdvanced -> retained
    TerminalSourceHold.TerminalSourceControlDuplicate -> retained
  where
    membershipHistory =
      OracleProjection.oracleViewHeraldMembershipHistoryChecked
        (OracleProjection.oracleView (startupOracleProjectionState predecessor))
    (successorHold, disposition) =
      TerminalSourceHold.retainAheadControl
        membershipHistory
        binding
        control
        (startupTerminalSourceHoldState predecessor)
    retained =
      checkedPeerSuccess
        (replaceStartupTerminalSourceHoldState successorHold predecessor)
        emptyEffectBatch

-- | A control which passed coordinate, sender, and semantic admission may be
-- temporarily blocked only by local immutable-history readiness.  Retain that
-- exact checked occurrence without disturbing the stable binding.  Malformed,
-- stale, or contradictory evidence never enters this owner.
holdCurrentTerminalControl ::
  TerminalSource.MembershipBaseClosure ->
  PeerBinding ->
  PeerControl ->
  HeraldState ->
  PeerTransition
holdCurrentTerminalControl _coordinator binding control predecessor =
  case disposition of
    TerminalSourceHold.TerminalSourceControlRejected ->
      peerProtocolClose binding ClosePeerControlProtocol predecessor
    TerminalSourceHold.TerminalSourceControlHeld -> retained
    TerminalSourceHold.TerminalSourceControlAdvanced -> retained
    TerminalSourceHold.TerminalSourceControlDuplicate -> retained
  where
    (successorHold, disposition) =
      TerminalSourceHold.retainReadinessControl
        (terminalMembershipHistory predecessor)
        binding
        control
        (startupTerminalSourceHoldState predecessor)
    retained =
      checkedPeerSuccess
        (replaceStartupTerminalSourceHoldState successorHold predecessor)
        emptyEffectBatch

consumeHeldTerminalControl ::
  PeerBinding -> PeerControl -> HeraldState -> HeraldState
consumeHeldTerminalControl binding control predecessor =
  replaceStartupTerminalSourceHoldState
    ( TerminalSourceHold.consumeReadinessControl
        binding
        control
        (startupTerminalSourceHoldState predecessor)
    )
    predecessor

retainTerminalCoordinator ::
  TerminalSource.MembershipBaseClosure -> HeraldState -> HeraldState
retainTerminalCoordinator coordinator =
  replaceStartupStructuralBaseCoordinator (Just coordinator)

terminalSuccessorMembers ::
  TerminalSource.MembershipBaseClosure -> Set.Set HeraldEpoch
terminalSuccessorMembers =
  Set.fromList
    . NonEmpty.toList
    . heraldMembershipGenerationActiveHeraldEpochs
    . StructuralBase.structuralBaseSuccessorMembership

terminalInventorySetComplete ::
  TerminalSource.MembershipBaseClosure -> Bool
terminalInventorySetComplete coordinator =
  Set.fromList (fmap fst (StructuralBase.structuralBaseInventoryEntries coordinator))
    == Set.cartesianProduct
      (StructuralBase.structuralBaseRetiredSources coordinator)
      (terminalSuccessorMembers coordinator)

supportingInventoryForRequest ::
  TerminalSource.TerminalSourcePayloadRequest ->
  TerminalSource.MembershipBaseClosure ->
  Maybe TerminalSource.TerminalSourceInventory
supportingInventoryForRequest request =
  fmap snd
    . find
      ( (== TerminalSource.terminalSourcePayloadRequestSupportingInventory request)
          . TerminalSource.terminalSourceInventoryDigest
          . snd
      )
    . StructuralBase.structuralBaseInventoryEntries

payloadRequestEffects ::
  TerminalSource.MembershipBaseClosure ->
  HeraldState ->
  [TerminalSource.TerminalSourcePayloadRequest] ->
  [HeraldEffect]
payloadRequestEffects coordinator state requests =
  [ SendPeerControl binding (PeerTerminalSourcePayloadRequested request)
  | request <- requests,
    Just inventory <- [supportingInventoryForRequest request coordinator],
    Just binding <-
      [ Discovery.currentPeerBinding
          (TerminalSource.terminalSourceInventoryReporter inventory)
          (startupDiscoveryState state)
      ]
  ]

broadcastTerminalControl :: HeraldState -> PeerControl -> EffectBatch
broadcastTerminalControl state control =
  orderedEffectBatch
    [ SendPeerControl binding control
    | binding <- Discovery.currentPeerBindings (startupDiscoveryState state)
    ]

terminalTopologyDigestReconstructor ::
  HeraldState -> StructuralBase.TopologyDigestReconstructor
terminalTopologyDigestReconstructor state vector control =
  case GraphProgress.structuralTopologyOccurrenceDigestAt
    (structuralReconciliationViews state)
    vector
    control
    (startupStructuralProgressState state) of
    Right (Right digest) -> Right digest
    _ -> Left TerminalSource.TerminalSourceTopologyReconstructionUnavailable

terminalStructuralBaseNotReady :: StructuralBase.StructuralBaseProblem -> Bool
terminalStructuralBaseNotReady problem = case problem of
  StructuralBase.StructuralBaseTerminalSourceProblem terminal -> case terminal of
    TerminalSource.TerminalSourceHistoryNotApplied {} -> True
    TerminalSource.TerminalSourceControlNotApplied {} -> True
    TerminalSource.TerminalSourceTopologyReconstructionUnavailable -> True
    _ -> False
  StructuralBase.StructuralBaseAnchorNotInstalled {} -> True
  StructuralBase.StructuralBaseGraphProgressProblem graph -> case graph of
    GraphProgress.StructuralProgressSuccessorBaseAhead -> True
    GraphProgress.StructuralProgressSuccessorBaseControlAhead {} -> True
    _ -> False
  _ -> False

-- | A survivor can receive the announcer's exact union before independent
-- inventory advertisements or requested payload relays reach it. Those two
-- omissions are receiver-local readiness holds, while the same result during
-- local union derivation remains an owner contradiction.
terminalStructuralBaseRemoteControlNotReady ::
  StructuralBase.StructuralBaseProblem -> Bool
terminalStructuralBaseRemoteControlNotReady problem =
  terminalStructuralBaseNotReady problem
    || case problem of
      StructuralBase.StructuralBaseTerminalSourceProblem terminal -> case terminal of
        TerminalSource.TerminalSourceInventoryMissingReporters {} -> True
        TerminalSource.TerminalSourcePayloadMissing {} -> True
        _ -> False
      _ -> False

-- | Detailed handoff for the top-level post-cut settlement coordinator. One
-- peer-control transition may release an ordered chain of dependency-held
-- successors; exposing those identities lets Transition immediately settle
-- every newly covered source occurrence before the composed state escapes.
applyPeerControlWithInstalledCuts ::
  PeerBinding ->
  PeerControl ->
  HeraldState ->
  Either
    HeraldInvariantFault
    (HeraldState, EffectBatch, [TopologyCutId], PeerControlServingDisposition)
applyPeerControlWithInstalledCuts binding control predecessor = do
  (afterControl, controlEffects, servingDisposition) <-
    applyPeerControlBeforeInstalledCutSettlementWithDisposition
      binding
      control
      predecessor
  (successor, readinessEffects, _) <-
    redriveTerminalSourceReadinessIfProgressed predecessor afterControl
  installed <-
    newlyInstalledCutIds
      (startupStructuralProgressState predecessor)
      (startupStructuralProgressState successor)
  Right
    ( successor,
      controlEffects <> readinessEffects,
      installed,
      servingDisposition
    )

-- | Re-drive retained current-generation terminal evidence only after one of
-- the immutable owners consulted by the typed readiness checks actually
-- advanced.  The hold is cleared before replay; an occurrence which is still
-- not ready is retained again exactly once, so this is neither polling nor an
-- internal fixed-point loop. Without a successor coordinator every readiness
-- step is inert, so its potentially large owners need not be compared.
redriveTerminalSourceReadinessIfProgressed ::
  HeraldState ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch, [TopologyCutId])
redriveTerminalSourceReadinessIfProgressed predecessor successor
  | Nothing <- startupStructuralBaseCoordinator successor =
      Right (successor, emptyEffectBatch, [])
  | not (terminalReadinessOwnersAdvanced predecessor successor) =
      Right (successor, emptyEffectBatch, [])
  | otherwise = do
      (afterRebase, rebaseEffects) <- rebaseTerminalCoordinatorFromInstalled successor
      (afterArchive, archiveEffects) <-
        redriveRetainedTerminalPayloadArchive afterRebase
      (afterHeld, heldEffects) <- replayHeldTerminalSourceReadiness afterArchive
      (afterCoordinator, coordinatorEffects) <-
        redriveRetainedTerminalSourceCoordinator afterHeld
      installed <-
        newlyInstalledCutIds
          (startupStructuralProgressState successor)
          (startupStructuralProgressState afterCoordinator)
      Right
        ( afterCoordinator,
          rebaseEffects <> archiveEffects <> heldEffects <> coordinatorEffects,
          installed
        )

terminalReadinessOwnersAdvanced :: HeraldState -> HeraldState -> Bool
terminalReadinessOwnersAdvanced predecessor successor =
  GraphProgress.structuralAppliedVector predecessorProgress
    /= GraphProgress.structuralAppliedVector successorProgress
    || GraphProgress.structuralAppliedControlPrefix predecessorProgress
      /= GraphProgress.structuralAppliedControlPrefix successorProgress
    || startupGraphState predecessor /= startupGraphState successor
    || GraphProgress.structuralLastInstalledCutId predecessorProgress
      /= GraphProgress.structuralLastInstalledCutId successorProgress
    || startupSortRegistryState predecessor /= startupSortRegistryState successor
    || startupControlledState predecessor /= startupControlledState successor
    || startupOracleProjectionState predecessor
      /= startupOracleProjectionState successor
    || terminalCoordinatorInventoryAndArchive predecessor
      /= terminalCoordinatorInventoryAndArchive successor
  where
    predecessorProgress = startupStructuralProgressState predecessor
    successorProgress = startupStructuralProgressState successor

terminalCoordinatorInventoryAndArchive ::
  HeraldState ->
  Maybe
    ( [((HeraldEpoch, HeraldEpoch), TerminalSource.TerminalSourceInventory)],
      [(StructuralOccurrenceId, TerminalSource.TerminalStructuralOccurrence)]
    )
terminalCoordinatorInventoryAndArchive state =
  fmap
    ( \coordinator ->
        ( StructuralBase.structuralBaseInventoryEntries coordinator,
          StructuralBase.structuralBasePayloadEntries coordinator
        )
    )
    (startupStructuralBaseCoordinator state)

-- | Reconsider the complete retained archive only while the union is still
-- open. Exact materialization is idempotent, and an installed successor base
-- must never feed predecessor-generation occurrences back through the ordinary
-- materializer. Holds remain pure results; a later owner edge invokes this
-- function again.
redriveRetainedTerminalPayloadArchive :: HeraldState -> PeerTransition
redriveRetainedTerminalPayloadArchive state =
  case startupStructuralBaseCoordinator state of
    Just coordinator
      | StructuralBase.structuralBaseEvidence coordinator == Nothing,
        StructuralBase.structuralBaseAgreedUnion coordinator == Nothing ->
          case materializeTerminalPayloadArchive coordinator state of
            Left _ -> peerInvariantFault
            Right (successor, effects) -> checkedPeerSuccess successor effects
    _ -> checkedPeerSuccess state emptyEffectBatch

replayHeldTerminalSourceReadiness :: HeraldState -> PeerTransition
replayHeldTerminalSourceReadiness state =
  case startupStructuralBaseCoordinator state of
    Nothing -> checkedPeerSuccess state emptyEffectBatch
    Just _coordinator ->
      case TerminalSourceHold.resolveForReadinessAdvance
        (terminalMembershipHistory state)
        (startupTerminalSourceHoldState state) of
        TerminalSourceHold.TerminalSourceReadinessUnchanged _ ->
          checkedPeerSuccess state emptyEffectBatch
        TerminalSourceHold.TerminalSourceReadinessMatched remaining controls ->
          -- Inventories can themselves wait on payload or anchor repair. Admit
          -- those prerequisites before reconsidering a union received earlier.
          let (inventories, dependentControls) = partition isInventory controls
           in foldM replayOne (replaceStartupTerminalSourceHoldState remaining state, emptyEffectBatch) (inventories <> dependentControls)
  where
    isInventory (_, PeerTerminalSourceInventoryAdvertised _) = True
    isInventory _ = False
    replayOne (predecessor, retainedEffects) (binding, control) = do
      (successor, effects, _) <-
        applyPeerControlBeforeInstalledCutSettlementWithDisposition
          binding
          control
          predecessor
      Right (successor, retainedEffects <> withoutPendingPayloadRequests binding control effects)

    -- A local-readiness inventory has already emitted every request for its
    -- missing bytes on this binding. Its claims are immutable and the retained
    -- archive only grows, including across an anchor rebase, so owner progress
    -- can only remove missing requests. Keep all admission/materialization work
    -- while awaiting those existing replies. First admission, membership-ahead
    -- replay, and inventories offered on a fresh binding use the ordinary path.
    withoutPendingPayloadRequests binding (PeerTerminalSourceInventoryAdvertised inventory) effects =
      orderedEffectBatch
        [ effect
        | effect <- effectBatchMembers effects,
          case effect of
            SendPeerControl requestedBinding (PeerTerminalSourcePayloadRequested request) ->
              requestedBinding /= binding
                || TerminalSource.terminalSourcePayloadRequestSupportingInventory request
                  /= TerminalSource.terminalSourceInventoryDigest inventory
            _ -> True
        ]
    withoutPendingPayloadRequests _ _ effects = effects

redriveRetainedTerminalSourceCoordinator :: HeraldState -> PeerTransition
redriveRetainedTerminalSourceCoordinator state =
  case startupStructuralBaseCoordinator state of
    Nothing -> checkedPeerSuccess state emptyEffectBatch
    Just coordinator
      | StructuralBase.structuralBaseEvidence coordinator /= Nothing ->
          checkedPeerSuccess state emptyEffectBatch
      | StructuralBase.structuralBaseEstablishedUnion coordinator /= Nothing ->
          installTerminalSourceBase coordinator state emptyEffectBatch
      | StructuralBase.structuralBaseLocalHerald coordinator == announcer ->
          case StructuralBase.structuralBasePayloadRequests coordinator of
            Left _ -> peerInvariantFault
            Right requests
              | null requests && terminalInventorySetComplete coordinator ->
                  advanceTerminalSourceCoordinator coordinator state
              | otherwise -> checkedPeerSuccess state emptyEffectBatch
      | otherwise -> checkedPeerSuccess state emptyEffectBatch
      where
        announcer =
          TerminalSource.terminalSourceUnionAnnouncer
            (StructuralBase.structuralBaseSuccessorMembership coordinator)

-- | Level-trigger the local applied report and, when the fixed report matrix
-- permits it, open or immediately install the next deterministic topology cut.
-- The composed top-level transition suppresses only exact same-binding report
-- repeats after every local fixed point has completed.
--
-- This compositional seam deliberately returns the installed cut identity to
-- its caller. The enclosing top-level transition must settle those newly
-- covered occurrences before publishing it; no intermediate
-- installed-but-unsettled state escapes. Explicit invariant properties verify
-- the completed composition.
advanceLocalTopologyCutIfReady ::
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch, [TopologyCutId])
advanceLocalTopologyCutIfReady predecessor = do
  let beforeProgress = startupStructuralProgressState predecessor
      report = GraphProgress.structuralLocalReport beforeProgress
      reportEffects =
        orderedEffectBatch
          [ SendPeerControl binding (PeerStructuralAppliedReported report)
          | binding <-
              StructuralProgress.structuralReportBindings
                beforeProgress
                (Discovery.currentPeerBindings (startupDiscoveryState predecessor)),
            structuralGenerationIsCurrent beforeProgress predecessor
          ]
  (releasedProgress, releaseEffects) <-
    releasePendingTopologyEvidence beforeProgress predecessor
  let releasedState =
        replaceStartupStructuralProgressState releasedProgress predecessor
  (afterProgress, cutEffects) <-
    advanceTopologyCutIfReady releasedProgress releasedState
  installed <- newlyInstalledCutIds beforeProgress afterProgress
  let replaced = replaceStartupStructuralProgressState afterProgress predecessor
      -- This protocol step changes reports and pending/open evidence, but not
      -- reconciliation, Graph, SortRegistry or Placement. Historical cut
      -- queries change only if it actually installs a new cut.
      successor
        | null installed =
            let !cache = startupAlignmentCutQueryCache predecessor
             in replaceStartupAlignmentCutQueryCache cache replaced
        | otherwise = replaced
  Right
    ( successor,
      reportEffects <> releaseEffects <> cutEffects,
      installed
    )

applyKnownContacts :: PeerBinding -> [KnownHerald] -> HeraldState -> PeerTransition
applyKnownContacts binding contacts predecessor =
  case Discovery.prepareKnownContacts binding contacts (startupDiscoveryState predecessor) of
    Left _ -> peerInvariantFault
    Right prepared ->
      let (successorDiscovery, disposition) = Discovery.commitKnownContacts prepared
          complete = Discovery.knownHeralds successorDiscovery
          withDiscovery = replaceStartupDiscoveryState successorDiscovery predecessor
       in case disposition of
            KnownContactsStaleBinding -> peerInvariantFault
            KnownContactsUnchanged -> checkedPeerSuccess withDiscovery emptyEffectBatch
            KnownContactsChanged ->
              let dialIntents = Discovery.peerDialIntents successorDiscovery
                  (successorLiveness, recoveryEffects) =
                    beginUnboundPeerRecoveries
                      (startupLastObservedTime predecessor)
                      dialIntents
                      (startupPeerLivenessState predecessor)
                  successor =
                    replaceStartupPeerLivenessState successorLiveness withDiscovery
               in checkedPeerSuccess
                    successor
                    ( orderedEffectBatch
                        ( [ SendPeerControl current (PeerKnownHeralds complete)
                          | current <- Discovery.currentPeerBindings successorDiscovery,
                            current /= binding
                          ]
                            <> recoveryEffects
                            <> fmap DialPeer dialIntents
                        )
                    )

-- | The first exact, active, unbound contact starts one fixed recovery episode.
-- Later address growth and every physical retry retain that absolute deadline;
-- a successfully admitted current binding clears it through 'finishAcceptedHello'.
beginUnboundPeerRecoveries ::
  MonotonicInstant ->
  [PeerDialIntent] ->
  PeerLiveness.State ->
  (PeerLiveness.State, [HeraldEffect])
beginUnboundPeerRecoveries observedAt intents predecessor =
  let (successor, reversedEffects) =
        foldl' beginOne (predecessor, []) intents
   in (successor, reverse reversedEffects)
  where
    beginOne (state, effects) intent =
      case PeerLiveness.beginRecovery
        observedAt
        (peerDialIntentHeraldEpoch intent)
        state of
        (successor, PeerLiveness.RecoveryAlreadyActive) ->
          (successor, effects)
        (successor, PeerLiveness.RecoveryStarted attempt spec) ->
          (successor, ArmTimer attempt spec : effects)

applyStructuralReport ::
  PeerBinding -> StructuralAppliedReport -> HeraldState -> PeerTransition
applyStructuralReport binding report predecessor
  | structuralAppliedReportReporter report
      /= peerBindingRemoteHeraldEpoch binding =
      peerProtocolClose binding ClosePeerControlProtocol predecessor
  | otherwise =
      case GraphProgress.prepareStructuralReport
        report
        (startupStructuralProgressState predecessor) of
        Left _ -> holdAheadSuccessorStructuralReport binding report predecessor
        Right prepared ->
          case GraphProgress.commitStructuralReport prepared of
            (_, GraphProgress.StructuralReportHistoricalStale) ->
              checkedPeerSuccess predecessor emptyEffectBatch
            (reportedProgress, _) -> do
              (successorProgress, effects) <-
                advanceTopologyCutIfReady reportedProgress predecessor
              checkedPeerSuccess
                (replaceStartupStructuralProgressState successorProgress predecessor)
                effects

-- A survivor may install the successor base from another peer and immediately
-- report it here before this owner processes its own terminal Established.
-- Hold only while Oracle names the exact successor and Graph still names its
-- exact predecessor; once Graph advances, ordinary duplicate/fork admission
-- remains authoritative.
holdAheadSuccessorStructuralReport ::
  PeerBinding -> StructuralAppliedReport -> HeraldState -> PeerTransition
holdAheadSuccessorStructuralReport binding report predecessor =
  case startupStructuralBaseCoordinator predecessor of
    Just coordinator
      | GraphProgress.structuralProgressMembershipGenerationId
          (startupStructuralProgressState predecessor)
          == heraldMembershipGenerationId predecessorMembership ->
          case disposition of
            TerminalSourceHold.TerminalSourceControlRejected ->
              peerProtocolClose binding ClosePeerControlProtocol predecessor
            TerminalSourceHold.TerminalSourceControlHeld -> retained
            TerminalSourceHold.TerminalSourceControlAdvanced -> retained
            TerminalSourceHold.TerminalSourceControlDuplicate -> retained
      where
        predecessorMembership =
          StructuralBase.structuralBasePredecessorMembership coordinator
        (successorHold, disposition) =
          TerminalSourceHold.retainSuccessorStructuralReport
            (terminalMembershipHistory predecessor)
            binding
            report
            (startupTerminalSourceHoldState predecessor)
        retained =
          checkedPeerSuccess
            (replaceStartupTerminalSourceHoldState successorHold predecessor)
            emptyEffectBatch
    _ -> peerProtocolClose binding ClosePeerControlProtocol predecessor

applyTopologyCutAnnounce ::
  PeerBinding -> TopologyCutAnnounce -> HeraldState -> PeerTransition
applyTopologyCutAnnounce binding announce predecessor
  | topologyCutAnnounceAnnouncer announce
      /= peerBindingRemoteHeraldEpoch binding =
      peerProtocolClose binding ClosePeerControlProtocol predecessor
  | otherwise =
      case GraphProgress.prepareTopologyCutAnnounce
        (structuralReconciliationViews predecessor)
        (topologyCheckpointFor frontierControl predecessor)
        announce
        (startupStructuralProgressState predecessor) of
        Left _ -> peerProtocolClose binding ClosePeerControlProtocol predecessor
        Right prepared -> do
          let admittedProgress =
                GraphProgress.commitTopologyCutAnnounce prepared
              admittedState =
                replaceStartupStructuralProgressState admittedProgress predecessor
              effects = case GraphProgress.preparedTopologyCutAnnounceClassification prepared of
                GraphProgress.TopologyCutAnnounceAccepted acceptance ->
                  singletonEffectBatch
                    (SendPeerControl binding (PeerTopologyCutAccepted acceptance))
                GraphProgress.TopologyCutAnnounceDuplicate acceptance ->
                  singletonEffectBatch
                    (SendPeerControl binding (PeerTopologyCutAccepted acceptance))
                GraphProgress.TopologyCutAnnounceHeld -> emptyEffectBatch
                GraphProgress.TopologyCutAnnounceStale established ->
                  singletonEffectBatch
                    (SendPeerControl binding (PeerTopologyCutEstablished established))
          (successorProgress, releaseEffects) <-
            releasePendingTopologyEvidence admittedProgress admittedState
          checkedPeerSuccess
            (replaceStartupStructuralProgressState successorProgress predecessor)
            (effects <> releaseEffects)
  where
    frontierControl =
      topologyFrontierAppliedControlPrefix
        (topologyCutFrontier (topologyCutAnnounceCut announce))

applyTopologyCutAcceptance ::
  PeerBinding -> TopologyCutAcceptance -> HeraldState -> PeerTransition
applyTopologyCutAcceptance binding acceptance predecessor
  | topologyCutAcceptanceReporter acceptance
      /= peerBindingRemoteHeraldEpoch binding =
      peerProtocolClose binding ClosePeerControlProtocol predecessor
  | otherwise =
      case GraphProgress.prepareTopologyCutAcceptance
        acceptance
        (startupStructuralProgressState predecessor) of
        Left _ -> peerProtocolClose binding ClosePeerControlProtocol predecessor
        Right prepared ->
          let acceptedProgress =
                GraphProgress.commitTopologyCutAcceptance prepared
           in case GraphProgress.preparedTopologyCutAcceptanceClassification prepared of
                GraphProgress.TopologyCutAcceptanceCompletesSet ->
                  establishAcceptedCut acceptedProgress
                GraphProgress.TopologyCutAcceptanceReoffersEstablished established ->
                  checkedPeerSuccess
                    (replaceStartupStructuralProgressState acceptedProgress predecessor)
                    ( singletonEffectBatch
                        (SendPeerControl binding (PeerTopologyCutEstablished established))
                    )
                GraphProgress.TopologyCutAcceptanceRetained ->
                  retain acceptedProgress
                GraphProgress.TopologyCutAcceptanceDuplicate ->
                  retain acceptedProgress
                GraphProgress.TopologyCutAcceptanceHistoricalStale ->
                  checkedPeerSuccess predecessor emptyEffectBatch
  where
    retain progress =
      checkedPeerSuccess
        (replaceStartupStructuralProgressState progress predecessor)
        emptyEffectBatch

    establishAcceptedCut acceptedProgress =
      case GraphProgress.prepareTopologyCutEstablishment acceptedProgress of
        Left _ -> peerInvariantFault
        Right establishedPrepared ->
          let successorProgress =
                GraphProgress.commitTopologyCutEstablishment establishedPrepared
              established =
                GraphProgress.preparedTopologyCutEstablished establishedPrepared
              effects =
                orderedEffectBatch
                  [ SendPeerControl current (PeerTopologyCutEstablished established)
                  | current <-
                      Discovery.currentPeerBindings
                        (startupDiscoveryState predecessor)
                  ]
           in checkedPeerSuccess
                (replaceStartupStructuralProgressState successorProgress predecessor)
                effects

applyTopologyCutEstablished ::
  PeerBinding -> TopologyCutEstablished -> HeraldState -> PeerTransition
applyTopologyCutEstablished binding established predecessor
  | peerBindingRemoteHeraldEpoch binding /= expectedAnnouncer =
      peerProtocolClose binding ClosePeerControlProtocol predecessor
  | otherwise =
      case GraphProgress.prepareTopologyCutEstablished
        (structuralReconciliationViews predecessor)
        (topologyCheckpointFor frontierControl predecessor)
        established
        progress of
        Left _ -> peerProtocolClose binding ClosePeerControlProtocol predecessor
        Right prepared -> do
          let admittedProgress =
                GraphProgress.commitTopologyCutEstablished prepared
              admittedState =
                replaceStartupStructuralProgressState admittedProgress predecessor
              effects = case GraphProgress.preparedTopologyCutEstablishedClassification prepared of
                GraphProgress.TopologyCutEstablishedInstalled acknowledgement ->
                  singletonEffectBatch
                    ( SendPeerControl
                        binding
                        (PeerTopologyCutEstablishedAcknowledged acknowledgement)
                    )
                GraphProgress.TopologyCutEstablishedDuplicate acknowledgement ->
                  singletonEffectBatch
                    ( SendPeerControl
                        binding
                        (PeerTopologyCutEstablishedAcknowledged acknowledgement)
                    )
                GraphProgress.TopologyCutEstablishedHeld -> emptyEffectBatch
                GraphProgress.TopologyCutEstablishedImported -> emptyEffectBatch
          (successorProgress, releaseEffects) <-
            releasePendingTopologyEvidence admittedProgress admittedState
          checkedPeerSuccess
            (replaceStartupStructuralProgressState successorProgress predecessor)
            (effects <> releaseEffects)
  where
    progress = startupStructuralProgressState predecessor
    expectedAnnouncer = NonEmpty.head (GraphProgress.structuralProgressMembers progress)
    frontierControl =
      topologyFrontierAppliedControlPrefix
        ( topologyCutFrontier
            (topologyCutEstablishedCut established)
        )

applyTopologyCutEstablishedAck ::
  PeerBinding -> TopologyCutEstablishedAck -> HeraldState -> PeerTransition
applyTopologyCutEstablishedAck binding acknowledgement predecessor
  | topologyCutEstablishedAckReporter acknowledgement
      /= peerBindingRemoteHeraldEpoch binding =
      peerProtocolClose binding ClosePeerControlProtocol predecessor
  | otherwise =
      case GraphProgress.prepareTopologyCutEstablishedAck
        acknowledgement
        (startupStructuralProgressState predecessor) of
        Left _ -> peerProtocolClose binding ClosePeerControlProtocol predecessor
        Right prepared -> do
          let acknowledgedProgress =
                GraphProgress.commitTopologyCutEstablishedAck prepared
          (successorProgress, effects) <-
            case GraphProgress.preparedTopologyCutAckClassification prepared of
              GraphProgress.TopologyCutAckAllMembers ->
                advanceTopologyCutIfReady acknowledgedProgress predecessor
              _ -> Right (acknowledgedProgress, emptyEffectBatch)
          checkedPeerSuccess
            (replaceStartupStructuralProgressState successorProgress predecessor)
            effects

-- | Re-evaluate retained cut evidence after any local structural/control or
-- predecessor-chain progress. Admission is level-triggered: a fact that was
-- dependency-held is never dependent on another wire duplicate to make
-- progress. The recursion is a small fixed point because installing a retained
-- predecessor may immediately release a retained successor announce.
releasePendingTopologyEvidence ::
  GraphProgress.StructuralProgressState ->
  HeraldState ->
  Either
    HeraldInvariantFault
    (GraphProgress.StructuralProgressState, EffectBatch)
releasePendingTopologyEvidence progress state =
  case GraphProgress.structuralPendingEstablished progress of
    Just established -> releaseEstablished established
    Nothing -> releaseAnnounce progress
  where
    releaseEstablished established = do
      checkpoint <-
        retainedTopologyCheckpoint
          ( topologyFrontierAppliedControlPrefix
              (topologyCutFrontier (topologyCutEstablishedCut established))
          )
          progress
          state
      prepared <-
        either
          (const peerInvariantFault)
          Right
          ( GraphProgress.prepareTopologyCutEstablished
              (structuralReconciliationViews state)
              checkpoint
              established
              progress
          )
      let successor = GraphProgress.commitTopologyCutEstablished prepared
      case GraphProgress.preparedTopologyCutEstablishedClassification prepared of
        GraphProgress.TopologyCutEstablishedHeld -> releaseAnnounce successor
        GraphProgress.TopologyCutEstablishedImported -> continue successor emptyEffectBatch
        GraphProgress.TopologyCutEstablishedInstalled acknowledgement ->
          continue
            successor
            (replyToAnnouncer (PeerTopologyCutEstablishedAcknowledged acknowledgement))
        GraphProgress.TopologyCutEstablishedDuplicate acknowledgement ->
          continue
            successor
            (replyToAnnouncer (PeerTopologyCutEstablishedAcknowledged acknowledgement))

    releaseAnnounce current =
      case GraphProgress.structuralPendingAnnounce current of
        Nothing -> Right (current, emptyEffectBatch)
        Just announce -> do
          checkpoint <-
            retainedTopologyCheckpoint
              ( topologyFrontierAppliedControlPrefix
                  (topologyCutFrontier (topologyCutAnnounceCut announce))
              )
              current
              state
          prepared <-
            either
              (const peerInvariantFault)
              Right
              ( GraphProgress.prepareTopologyCutAnnounce
                  (structuralReconciliationViews state)
                  checkpoint
                  announce
                  current
              )
          let successor = GraphProgress.commitTopologyCutAnnounce prepared
          case GraphProgress.preparedTopologyCutAnnounceClassification prepared of
            GraphProgress.TopologyCutAnnounceHeld ->
              Right (successor, emptyEffectBatch)
            GraphProgress.TopologyCutAnnounceAccepted acceptance ->
              continue
                successor
                (replyToAnnouncer (PeerTopologyCutAccepted acceptance))
            GraphProgress.TopologyCutAnnounceDuplicate acceptance ->
              continue
                successor
                (replyToAnnouncer (PeerTopologyCutAccepted acceptance))
            GraphProgress.TopologyCutAnnounceStale established ->
              continue
                successor
                (replyToAnnouncer (PeerTopologyCutEstablished established))

    continue successor effects = do
      let successorState =
            replaceStartupStructuralProgressState successor state
      (complete, laterEffects) <-
        releasePendingTopologyEvidence successor successorState
      Right (complete, effects <> laterEffects)

    replyToAnnouncer message =
      maybe
        emptyEffectBatch
        (\binding -> singletonEffectBatch (SendPeerControl binding message))
        ( Discovery.currentPeerBinding
            (NonEmpty.head (GraphProgress.structuralProgressMembers progress))
            (startupDiscoveryState state)
        )

checkedTopologyCheckpoint ::
  ControlIndex ->
  HeraldState ->
  Either HeraldInvariantFault GraphProgress.TopologyControlCheckpoint
checkedTopologyCheckpoint index state =
  if index <= OracleProjection.oracleViewControlIndex oracleView
    then
      let (affectsTopology, bytes) =
            Reconciliation.structuralControlCheckpointAt
              index
              ( GraphProgress.structuralProgressReconciliation
                  (startupStructuralProgressState state)
              )
          progress = startupStructuralProgressState state
          installedPrefix =
            maybe
              (controlIndex 0)
              (topologyFrontierAppliedControlPrefix . topologyCutFrontier . GraphProgress.installedTopologyCutCut)
              (lookup (GraphProgress.structuralLastInstalledCutId progress) (GraphProgress.structuralInstalledCutEntries progress))
       in Right $ case OracleProjection.oracleViewPendingHeraldAdmission oracleView of
            Just admission
              | Admission.admissionRecordSemanticToken admission <= index
                  && installedPrefix < Admission.admissionRecordSemanticToken admission ->
                  GraphProgress.topologyJoinSealCheckpoint (Admission.admissionRecordId admission) index bytes
            _ -> GraphProgress.topologyControlCheckpointWithConsequence index affectsTopology bytes
    else peerInvariantFault
  where
    oracleView =
      OracleProjection.oracleView (startupOracleProjectionState state)

retainedTopologyCheckpoint ::
  ControlIndex ->
  GraphProgress.StructuralProgressState ->
  HeraldState ->
  Either HeraldInvariantFault GraphProgress.TopologyControlCheckpoint
retainedTopologyCheckpoint index progress state
  | GraphProgress.structuralAppliedControlPrefix progress < index =
      Right (GraphProgress.topologyControlCheckpoint index ByteString.empty)
  | otherwise = checkedTopologyCheckpoint index state

newlyInstalledCutIds ::
  GraphProgress.StructuralProgressState ->
  GraphProgress.StructuralProgressState ->
  Either HeraldInvariantFault [TopologyCutId]
newlyInstalledCutIds predecessor successor =
  case stripPrefix before after of
    Just installed -> Right installed
    Nothing -> peerInvariantFault
  where
    before = GraphProgress.structuralInstalledCutIds predecessor
    after = GraphProgress.structuralInstalledCutIds successor

advanceTopologyCutIfReady ::
  GraphProgress.StructuralProgressState ->
  HeraldState ->
  Either HeraldInvariantFault (GraphProgress.StructuralProgressState, EffectBatch)
advanceTopologyCutIfReady progress state
  | not (structuralGenerationIsCurrent progress state) =
      Right (progress, emptyEffectBatch)
  | GraphProgress.structuralProgressLocalHerald progress /= expectedAnnouncer =
      Right (progress, emptyEffectBatch)
  | GraphProgress.structuralOpenCut progress /= Nothing =
      Right (progress, emptyEffectBatch)
  | otherwise = case GraphProgress.structuralStableFrontier progress of
      Nothing -> Right (progress, emptyEffectBatch)
      Just frontier ->
        case checkedTopologyCheckpoint
          (topologyFrontierAppliedControlPrefix frontier)
          state of
          Left _ -> peerInvariantFault
          Right checkpoint ->
            case GraphProgress.prepareTopologyCutProposalCached
              (structuralReconciliationViews state)
              checkpoint
              progress of
              Left _ -> peerInvariantFault
              Right (_, retained, GraphProgress.TopologyCutProposalUnavailable) ->
                Right (retained, emptyEffectBatch)
              Right (_, retained, GraphProgress.TopologyCutProposalHeld _) ->
                Right (retained, emptyEffectBatch)
              Right (_, _, GraphProgress.TopologyCutProposalReady prepared) ->
                finishProposal prepared
  where
    expectedAnnouncer =
      NonEmpty.head (GraphProgress.structuralProgressMembers progress)

    finishProposal prepared =
      let proposed = GraphProgress.commitTopologyCutProposal prepared
       in case GraphProgress.prepareTopologyCutEstablishment proposed of
            Right establishment ->
              let installed =
                    GraphProgress.commitTopologyCutEstablishment establishment
                  established =
                    GraphProgress.preparedTopologyCutEstablished establishment
               in Right
                    ( installed,
                      broadcast (PeerTopologyCutEstablished established)
                    )
            Left GraphProgress.StructuralProgressIncompleteEstablishedEvidence ->
              Right
                ( proposed,
                  broadcast
                    ( PeerTopologyCutAnnounced
                        (GraphProgress.preparedTopologyCutProposalAnnounce prepared)
                    )
                )
            Left _ -> peerInvariantFault

    broadcast message =
      orderedEffectBatch
        [ SendPeerControl current message
        | current <-
            Discovery.currentPeerBindings
              (startupDiscoveryState state)
        ]

-- Historical certificates may still repair the installed anchor. Fresh reports
-- and proposals require the structural owner to have reached current membership.
structuralGenerationIsCurrent :: GraphProgress.StructuralProgressState -> HeraldState -> Bool
structuralGenerationIsCurrent progress state =
  GraphProgress.structuralProgressMembershipGenerationId progress
    == OracleProjection.oracleViewCurrentHeraldMembershipId
      (OracleProjection.oracleView (startupOracleProjectionState state))

topologyCheckpointFor ::
  ControlIndex ->
  HeraldState ->
  GraphProgress.TopologyControlCheckpoint
topologyCheckpointFor index state =
  case checkedTopologyCheckpoint index state of
    Right checkpoint -> checkpoint
    Left _ -> GraphProgress.topologyControlCheckpoint index ByteString.empty

structuralReconciliationViews :: HeraldState -> Reconciliation.ReconciliationViews
structuralReconciliationViews state =
  Reconciliation.withReconciliationDiagnosticChecks (startupDiagnosticChecks state)
    $ Reconciliation.reconciliationViewsWithEndedProcesses
      ( Map.fromList
          [ (process, (residence, OracleProjection.projectedEndedProcessControlIndex ended))
          | (process, ended) <- OracleProjection.projectedEndedProcesses (startupOracleProjectionState state),
            Just residence <- [OracleProjection.oracleViewProcessResidence process (OracleProjection.oracleView (startupOracleProjectionState state))]
          ]
      )
    $ Reconciliation.reconciliationViews
      (checkedLocalHeraldEpoch (startupGenesis state))
      ( Map.fromList
          [ ( SortRegistry.registryEntrySortId entry,
              sortOccurrence
                (SortRegistry.registryEntrySortId entry)
                (SortRegistry.registryEntryOccurrenceId entry)
            )
          | entry <- SortRegistry.registryEntries (startupSortRegistryState state)
          ]
      )
      (Set.fromList (Graph.graphNonStructuralBaselineVertices (startupGraphState state)))
      ( Map.fromList
          [ (process, residence)
          | process <-
              OracleProjection.projectedProcessEpochs
                (startupOracleProjectionState state),
            OracleProjection.oracleViewProcessIsLive
              process
              (OracleProjection.oracleView (startupOracleProjectionState state)),
            Just residence <-
              [ OracleProjection.oracleViewProcessResidence
                  process
                  (OracleProjection.oracleView (startupOracleProjectionState state))
              ]
          ]
      )
      ( Set.fromList
          ( Controlled.controlledNormalPossessionEntries
              (startupControlledState state)
          )
      )

-- | Level-trigger the progress and retained cut evidence needed after one
-- binding is established. No protocol correctness relies on a one-shot send.
structuralRetryEffects :: PeerBinding -> HeraldState -> [HeraldEffect]
structuralRetryEffects binding state =
  [ SendPeerControl binding control
  | control <- structuralRetryControls remote progress
  ]
  where
    progress = startupStructuralProgressState state
    remote = peerBindingRemoteHeraldEpoch binding

alignmentRetryEffects :: PeerBinding -> HeraldState -> [HeraldEffect]
alignmentRetryEffects binding state =
  [ SendPeerControl binding control
  | control <-
      fmap PeerAlignmentControl otherRoots
        <> PeerDelivery.reconnectEvidence binding deliveryRoots state
        <> fmap PeerAlignmentControl transferRoots
  ]
  where
    local = checkedLocalHeraldEpoch (startupGenesis state)
    remote = peerBindingRemoteHeraldEpoch binding
    (deliveryRoots, otherRoots) =
      partition
        (\control -> case DeliveryOwner.evidenceKey control of Just _ -> True; Nothing -> False)
        (Alignment.alignmentReconnectRetryControls local remote (startupAlignmentState state))
    transferRoots =
      AlignmentTransfer.alignmentTransferRetryControls local remote state
        <> DisappearanceLive.disappearanceAlignmentRetryControls remote state

-- | Compose the first semantic catalogue on a replacement physical
-- association.  Terminal-source repair must lead ordinary structural and
-- alignment replay: after the announcer has installed a membership-successor
-- base, a lagging survivor still owns predecessor-generation report state.
-- Sending the successor report first would make that valid transient look
-- like an incomparable peer report and close the association before the
-- retained Established control could install the missing base.  The reliable
-- peer-control lane preserves this order.
reconnectRepairEffects ::
  PeerBinding -> HeraldState -> Either HeraldInvariantFault [HeraldEffect]
reconnectRepairEffects binding state = do
  terminal <- terminalStructuralRetryEffects binding state
  Right
    ( terminal
        <> structuralRetryEffects binding state
        <> alignmentRetryEffects binding state
    )

terminalStructuralRetryEffects ::
  PeerBinding -> HeraldState -> Either HeraldInvariantFault [HeraldEffect]
terminalStructuralRetryEffects binding state =
  case startupStructuralBaseCoordinator state of
    Nothing -> Right []
    Just coordinator
      | Set.notMember remote (terminalSuccessorMembers coordinator) -> Right []
      | otherwise -> do
          requests <-
            either
              (const peerInvariantFault)
              Right
              (StructuralBase.structuralBasePayloadRequests coordinator)
          announceOrEstablished <- retainedAnnouncerControl coordinator
          let inventories =
                fmap
                  PeerTerminalSourceInventoryAdvertised
                  (StructuralBase.structuralBaseLocalInventories coordinator)
              targetedRequests =
                [ PeerTerminalSourcePayloadRequested request
                | request <- requests,
                  Just supporting <- [supportingInventoryForRequest request coordinator],
                  TerminalSource.terminalSourceInventoryReporter supporting == remote
                ]
              localAcceptances =
                [ PeerTerminalSourceUnionAccepted acceptance
                | (reporter, acceptance) <- StructuralBase.structuralBaseAcceptanceEntries coordinator,
                  reporter == local,
                  remote == announcer
                ]
          Right
            [ SendPeerControl binding control
            | control <- inventories <> targetedRequests <> announceOrEstablished <> localAcceptances
            ]
  where
    remote = peerBindingRemoteHeraldEpoch binding
    local = checkedLocalHeraldEpoch (startupGenesis state)

    retainedAnnouncerControl coordinator
      | local /= coordinatorAnnouncer = Right []
      | Just established <- StructuralBase.structuralBaseEstablishedUnion coordinator =
          Right [PeerTerminalSourceUnionEstablished established]
      | Just union <- StructuralBase.structuralBaseAgreedUnion coordinator =
          case TerminalSource.terminalSourceUnionAnnounce
            (StructuralBase.structuralBaseSuccessorMembership coordinator)
            local
            union of
            Left _ -> peerInvariantFault
            Right announce -> Right [PeerTerminalSourceUnionAnnounced announce]
      | otherwise = Right []
      where
        coordinatorAnnouncer =
          TerminalSource.terminalSourceUnionAnnouncer
            (StructuralBase.structuralBaseSuccessorMembership coordinator)

    announcer = case startupStructuralBaseCoordinator state of
      Nothing -> local
      Just coordinator ->
        TerminalSource.terminalSourceUnionAnnouncer
          (StructuralBase.structuralBaseSuccessorMembership coordinator)

-- | Level-triggered semantic repair payload for one current peer binding. The
-- announcer reoffers the complete installed predecessor chain in order, not
-- merely the latest cut, before reoffering a successor announce.
structuralRetryControls ::
  HeraldEpoch ->
  GraphProgress.StructuralProgressState ->
  [PeerControl]
structuralRetryControls remote progress =
  [ PeerStructuralAppliedReported (GraphProgress.structuralLocalReport progress)
  | local /= announcer,
    not (GraphProgress.structuralProgressIsJoiningObserver progress),
    remote == announcer
  ]
    <> cutRetry
  where
    local = GraphProgress.structuralProgressLocalHerald progress
    announcer = NonEmpty.head (GraphProgress.structuralProgressMembers progress)

    cutRetry
      | local == announcer =
          [ PeerTopologyCutEstablished established
          | established <- GraphProgress.structuralInstalledEstablishedChain progress
          ]
            <> case GraphProgress.structuralOpenEstablished progress of
              Just _ -> []
              Nothing ->
                maybe
                  []
                  (\announce -> [PeerTopologyCutAnnounced announce])
                  (GraphProgress.structuralOpenCut progress)
      | otherwise = []

applyPlacementUpdate :: PeerBinding -> PlacementUpdate -> HeraldState -> PeerTransition
applyPlacementUpdate binding update predecessor
  | placementUpdateOwner update /= peerBindingRemoteHeraldEpoch binding =
      peerProtocolClose binding ClosePeerPlacementProtocol predecessor
  | not
      ( placementUpdateClaimsAreValid
          (checkedSystemId (startupGenesis predecessor))
          (peerBindingRemoteHeraldEpoch binding)
          (OracleProjection.projectedBootstraps (startupOracleProjectionState predecessor))
          (startupGraphState predecessor)
          (startupStructuralProgressState predecessor)
          update
      ) =
      peerProtocolClose binding ClosePeerPlacementProtocol predecessor
  | otherwise =
      case Placement.prepareRemotePlacementWithDiagnostics (startupDiagnosticChecks predecessor) update (startupPlacementState predecessor) of
        Left (Placement.PlacementProtocolProblem _) ->
          peerProtocolClose binding ClosePeerPlacementProtocol predecessor
        Left (Placement.PlacementInvariantProblem _) -> peerInvariantFault
        Right prepared -> do
          let (successorPlacement, result) = Placement.commitRemotePlacement prepared
              placementEffects =
                maybe
                  emptyEffectBatch
                  (singletonEffectBatch . SendPeerControl binding . PeerPlacementAcknowledged)
                  (Placement.remotePlacementAcknowledgement result)
              afterPlacement =
                replaceStartupPlacementState successorPlacement predecessor
          (afterLoss, lossEffects) <-
            applyVanishedRemotePlacementLosses
              (peerBindingRemoteHeraldEpoch binding)
              predecessor
              afterPlacement
          (afterReconciliation, reconciliationEffects) <-
            mapAlignmentTransferCoordinator
              (AlignmentTransfer.reconcileAlignmentLosses afterLoss)
          case Alignment.advanceAlignmentGenerations afterReconciliation of
            Left _ -> peerInvariantFault
            Right (successor, alignmentEffects) ->
              checkedPeerSuccess
                successor
                (placementEffects <> lossEffects <> reconciliationEffects <> alignmentEffects)

-- | A placement update is the checked observation boundary for a remote
-- physical Store incarnation.  Retain every vanished coordinate before
-- reconsidering generation debt; the causes are the exact retained alignment
-- promotions whose executable work names that coordinate.  A loss observed
-- before any such work exists still records the source exclusion durably.
applyVanishedRemotePlacementLosses ::
  HeraldEpoch ->
  HeraldState ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyVanishedRemotePlacementLosses owner predecessor successor = do
  let vanished =
        remotePlacementVanishes
          owner
          (startupPlacementState predecessor)
          (startupPlacementState successor)
  alignment <-
    foldM
      applyOne
      (startupAlignmentState successor)
      (Set.toAscList vanished)
  mapAlignmentTransferCoordinator
    ( AlignmentTransfer.deliverAlignmentLossTransferDelta
        predecessor
        (replaceStartupAlignmentState alignment successor)
    )
  where
    local = checkedLocalHeraldEpoch (startupGenesis successor)

    applyOne alignment lost = do
      let causes =
            Set.toAscList
              (AlignmentLoss.alignmentLossRelevantCauses lost alignment)
      case causes of
        [] ->
          let unavailable =
                AlignmentState.unavailableAlignmentSource
                  (AlignmentLoss.qualifiedStoreCoordinateHerald lost)
                  (AlignmentLoss.qualifiedStoreCoordinateDelta lost)
                  (AlignmentLoss.qualifiedStoreCoordinateIncarnation lost)
           in Right
                ( fst
                    ( AlignmentState.commitUnavailableAlignmentSourceRetention
                        ( AlignmentState.prepareUnavailableAlignmentSourceRetention
                            unavailable
                            alignment
                        )
                    )
                )
        _ -> do
          coordinator <-
            mapAlignmentLoss
              (AlignmentLoss.alignmentLossCoordinatorState local alignment)
          afterLoss <- foldM (applyCause lost) coordinator causes
          Right (AlignmentLoss.alignmentLossCoordinatorOwner afterLoss)

    applyCause lost coordinator cause = do
      prepared <-
        mapAlignmentLoss
          (AlignmentLoss.prepareAlignmentLoss cause lost coordinator)
      Right (AlignmentLoss.commitAlignmentLoss prepared)

-- Full snapshots replace the current projection atomically.  A peer may first
-- observe any revision, or skip revisions between observations; only the
-- actual predecessor and successor describe an observed Store loss.
remotePlacementVanishes ::
  HeraldEpoch ->
  Placement.State ->
  Placement.State ->
  Set.Set AlignmentLoss.QualifiedStoreCoordinate
remotePlacementVanishes owner predecessor successor =
  remotePlacementStoreCoordinates owner predecessor
    Set.\\ remotePlacementStoreCoordinates owner successor

remotePlacementStoreCoordinates ::
  HeraldEpoch ->
  Placement.State ->
  Set.Set AlignmentLoss.QualifiedStoreCoordinate
remotePlacementStoreCoordinates owner =
  routeStoreCoordinates owner . Placement.remotePlacementRoutes owner

routeStoreCoordinates ::
  HeraldEpoch ->
  [DeltaRoute] ->
  Set.Set AlignmentLoss.QualifiedStoreCoordinate
routeStoreCoordinates owner routes =
  Set.fromList
    [ AlignmentLoss.qualifiedStoreCoordinate
        owner
        (deltaRouteDelta route)
        (deltaRouteStoreIncarnation route)
    | route <- routes
    ]

applyPlacementAcknowledgement ::
  PeerBinding -> PlacementAcknowledgement -> HeraldState -> PeerTransition
applyPlacementAcknowledgement binding acknowledgement predecessor =
  case Placement.preparePlacementAcknowledgementWithDiagnostics
    (startupDiagnosticChecks predecessor)
    (peerBindingRemoteHeraldEpoch binding)
    acknowledgement
    (startupPlacementState predecessor) of
    Left (Placement.PlacementProtocolProblem _) ->
      peerProtocolClose binding ClosePeerPlacementProtocol predecessor
    Left (Placement.PlacementInvariantProblem _) -> peerInvariantFault
    Right prepared ->
      let (successorPlacement, _) = Placement.commitPlacementAcknowledgement prepared
       in checkedPeerSuccess
            (replaceStartupPlacementState successorPlacement predecessor)
            emptyEffectBatch

applyReceivedAck ::
  PeerBinding -> StreamDirection -> StreamPrefix -> HeraldState -> PeerTransition
applyReceivedAck binding direction prefix predecessor
  | not (directionMatchesBinding binding direction predecessor) =
      peerProtocolClose binding ClosePeerStreamProtocol predecessor
  | otherwise =
      case PeerStream.prepareReceivedAck direction prefix (startupPeerStreamState predecessor) of
        Left (PeerStream.PeerStreamProtocolProblem _) ->
          peerProtocolClose binding ClosePeerStreamProtocol predecessor
        Left (PeerStream.PeerStreamInvariantProblem _) -> peerInvariantFault
        Right prepared ->
          let (successorStream, _) = PeerStream.commitReceivedAck prepared
           in checkedPeerSuccess
                (replaceStartupPeerStreamState successorStream predecessor)
                (ticketEffect (PeerStream.preparedReceivedAckDispatchTicket prepared))

applyCompletedAck ::
  PeerBinding -> StreamDirection -> ReceiptRetirement -> HeraldState -> PeerTransition
applyCompletedAck binding direction prefix predecessor
  | not (directionMatchesBinding binding direction predecessor) =
      peerProtocolClose binding ClosePeerStreamProtocol predecessor
  | otherwise =
      case PeerStream.prepareCompletedProgress direction prefix (startupPeerStreamState predecessor) of
        Left (PeerStream.PeerStreamProtocolProblem _) ->
          peerProtocolClose binding ClosePeerStreamProtocol predecessor
        Left (PeerStream.PeerStreamInvariantProblem _) -> peerInvariantFault
        Right prepared -> do
          let (successorStream, _) = PeerStream.commitCompletedAck prepared
              successor = replaceStartupPeerStreamState successorStream predecessor
          withGroups <- recordPublicationGroupCompletion binding successor
          checkedPeerSuccess
            withGroups
            (ticketEffect (PeerStream.preparedCompletedAckDispatchTicket prepared))

-- Keep the readiness index at the same committed prefix as the peer owner.
-- Transport still reclaims sparse completions; conservative groups wait for
-- contiguous progress. Retirement has a separate authoritative discharge path.
recordPublicationGroupCompletion :: PeerBinding -> HeraldState -> Either HeraldInvariantFault HeraldState
recordPublicationGroupCompletion binding state
  | PeerStream.peerStreamPeerRetired peer (startupPeerStreamState state) = Right state
  | otherwise = do
      direction <- peerDirection binding state
      watermarks <- case PeerStream.outgoingWatermarks direction (startupPeerStreamState state) of
        Left _ -> peerInvariantFault
        Right value -> Right value
      Right
        ( replaceStartupPublicationState
            (Publication.advancePublicationGroupCompletion peer (streamCompletedPrefix watermarks) (startupPublicationState state))
            state
        )
  where
    peer = peerBindingRemoteHeraldEpoch binding

applyFrontierAdvance ::
  PeerBinding ->
  StreamDirection ->
  StreamSequence ->
  HeraldState ->
  PeerTransition
applyFrontierAdvance binding direction nextSequence predecessor
  | not (incomingDirectionMatchesBinding binding direction predecessor) =
      peerProtocolClose binding ClosePeerStreamProtocol predecessor
  | otherwise =
      case PeerStream.prepareFrontierAdvance
        direction
        nextSequence
        (startupPeerStreamState predecessor) of
        Left (PeerStream.PeerStreamProtocolProblem _) ->
          peerProtocolClose binding ClosePeerStreamProtocol predecessor
        Left (PeerStream.PeerStreamInvariantProblem _) -> peerInvariantFault
        Right prepared ->
          checkedPeerSuccess
            ( replaceStartupPeerStreamState
                (PeerStream.commitFrontierAdvance prepared)
                predecessor
            )
            emptyEffectBatch

applyResumeOffer :: PeerBinding -> ResumeOffer -> HeraldState -> PeerTransition
applyResumeOffer binding offer predecessor
  | resumeOfferSource offer /= peerBindingRemoteHeraldEpoch binding
      || resumeOfferDestination offer
        /= checkedLocalHeraldEpoch (startupGenesis predecessor) =
      peerProtocolClose binding ClosePeerStreamProtocol predecessor
  | otherwise =
      case Discovery.prepareReconnectCatalogue
        binding
        (startupDiscoveryState predecessor) of
        Left _ -> peerInvariantFault
        Right preparedCatalogue ->
          let firstOnAssociation =
                Discovery.preparedReconnectCatalogueWasPending preparedCatalogue
              association
                | firstOnAssociation = PeerStream.FirstResumeOnAssociation
                | otherwise = PeerStream.ContinuingResumeOnAssociation
           in case PeerStream.prepareResumeForAssociation
                association
                offer
                (startupPeerStreamState predecessor) of
                Left (PeerStream.PeerStreamProtocolProblem _) ->
                  peerProtocolClose binding ClosePeerStreamProtocol predecessor
                Left (PeerStream.PeerStreamInvariantProblem _) -> peerInvariantFault
                Right prepared -> do
                  let (successorStream, response, _) = PeerStream.commitResume prepared
                      (successorDiscovery, retryCatalogue) =
                        Discovery.commitReconnectCatalogue preparedCatalogue
                      successor =
                        replaceStartupDiscoveryState successorDiscovery
                          . replaceStartupPeerStreamState successorStream
                          $ predecessor
                  catalogueEffects <-
                    if retryCatalogue
                      then reconnectRepairEffects binding successor
                      else Right []
                  let effects =
                        orderedEffectBatch
                          ( [SendPeerControl binding (PeerStreamResumeAccepted response)]
                              <> maybe
                                []
                                (\ticket -> [SchedulePeerDispatch ticket])
                                (PeerStream.preparedResumeDispatchTicket prepared)
                              <> catalogueEffects
                          )
                  withGroups <- recordPublicationGroupCompletion binding successor
                  checkedPeerSuccess (if retryCatalogue then PeerDelivery.markSeeded binding withGroups else withGroups) effects

applyResumeResponse :: PeerBinding -> ResumeResponse -> HeraldState -> PeerTransition
applyResumeResponse binding response predecessor
  | receiptRetirementHighWater (resumeResponseCompletion response)
      > fmap streamSequenceWord64 (streamPrefixSequence (resumeResponseReceivedPrefix response))
      || resumeResponseRetransmitFrom response
        /= nextAfterStreamPrefix (resumeResponseReceivedPrefix response) =
      peerProtocolClose binding ClosePeerStreamProtocol predecessor
  | otherwise = do
      direction <- peerDirection binding predecessor
      case PeerStream.prepareReceivedAck
        direction
        (resumeResponseReceivedPrefix response)
        (startupPeerStreamState predecessor) of
        Left (PeerStream.PeerStreamProtocolProblem _) ->
          peerProtocolClose binding ClosePeerStreamProtocol predecessor
        Left (PeerStream.PeerStreamInvariantProblem _) -> peerInvariantFault
        Right preparedReceived -> do
          let (afterReceived, _) = PeerStream.commitReceivedAck preparedReceived
          case PeerStream.prepareCompletedProgress
            direction
            (resumeResponseCompletion response)
            afterReceived of
            Left (PeerStream.PeerStreamProtocolProblem _) ->
              peerProtocolClose binding ClosePeerStreamProtocol predecessor
            Left (PeerStream.PeerStreamInvariantProblem _) -> peerInvariantFault
            Right preparedCompleted -> do
              let (successorStream, _) = PeerStream.commitCompletedAck preparedCompleted
                  successor = replaceStartupPeerStreamState successorStream predecessor
                  tickets =
                    catMaybes
                      [ PeerStream.preparedReceivedAckDispatchTicket preparedReceived,
                        PeerStream.preparedCompletedAckDispatchTicket preparedCompleted
                      ]
              withGroups <- recordPublicationGroupCompletion binding successor
              checkedPeerSuccess
                withGroups
                (orderedEffectBatch (fmap SchedulePeerDispatch tickets))

applyPeerDispatchSelection ::
  PeerDispatchTicket ->
  PeerBinding ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyPeerDispatchSelection ticket binding predecessor
  | not (bindingIsCurrent binding predecessor)
      || not
        ( directionMatchesBinding
            binding
            (peerDispatchTicketDirection ticket)
            predecessor
        ) =
      checkedPeerSuccess predecessor emptyEffectBatch
  | otherwise = do
      dispatchBinding <- peerDispatchBinding binding
      case PeerStream.prepareDispatchSelection
        ticket
        dispatchBinding
        (startupPeerStreamState predecessor) of
        Left (PeerStream.PeerStreamProtocolProblem _) -> peerInvariantFault
        Left (PeerStream.PeerStreamInvariantProblem _) -> peerInvariantFault
        Right prepared ->
          let (successorStream, attempt) = PeerStream.commitDispatchSelection prepared
           in checkedPeerSuccess
                (replaceStartupPeerStreamState successorStream predecessor)
                ( maybe
                    emptyEffectBatch
                    (singletonEffectBatch . SendPeerItem binding)
                    attempt
                )

applyPeerBindingLoss ::
  PeerBinding ->
  HeraldState ->
  Either
    HeraldInvariantFault
    (HeraldState, EffectBatch, PeerBindingLossDisposition)
applyPeerBindingLoss binding predecessor =
  case Discovery.prepareBindingLoss binding (startupDiscoveryState predecessor) of
    Left _ -> peerInvariantFault
    Right preparedDiscovery -> do
      direction <- peerDirection binding predecessor
      dispatchBinding <- peerDispatchBinding binding
      case PeerStream.prepareDispatchBindingLoss
        direction
        dispatchBinding
        (startupPeerStreamState predecessor) of
        Left _ -> peerInvariantFault
        Right preparedStream ->
          let (successorDiscovery, discoveryLost) =
                Discovery.commitBindingLoss preparedDiscovery
              (successorStream, streamLost) =
                PeerStream.commitDispatchBindingLoss preparedStream
           in if discoveryLost /= streamLost
                then peerInvariantFault
                else
                  let (successorLiveness, livenessEffects) =
                        if discoveryLost
                          then case PeerLiveness.beginRecovery
                            (startupLastObservedTime predecessor)
                            (peerBindingRemoteHeraldEpoch binding)
                            (startupPeerLivenessState predecessor) of
                            (retained, PeerLiveness.RecoveryAlreadyActive) ->
                              (retained, [])
                            (retained, PeerLiveness.RecoveryStarted attempt spec) ->
                              (retained, [ArmTimer attempt spec])
                          else (startupPeerLivenessState predecessor, [])
                      dialEffects =
                        if discoveryLost
                          then
                            maybe
                              []
                              (pure . DialPeer)
                              (dialIntentForBinding binding successorDiscovery)
                          else []
                      effects = orderedEffectBatch (livenessEffects <> dialEffects)
                      disposition =
                        if discoveryLost
                          then CurrentPeerBindingLost
                          else StalePeerBindingLoss
                   in do
                        (successor, checkedEffects) <-
                          checkedPeerSuccess
                            ( replaceStartupPeerLivenessState successorLiveness
                                . replaceStartupPeerStreamState successorStream
                                . replaceStartupDiscoveryState successorDiscovery
                                $ predecessor
                            )
                            effects
                        Right (successor, checkedEffects, disposition)

applyPeerDispatchObservation ::
  Bool ->
  PeerDispatchAttempt PeerLogicalPayload ->
  PeerDispatchOutcome ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyPeerDispatchObservation allowReplacement attempt outcome predecessor =
  case PeerStream.prepareDispatchOutcome attempt outcome (startupPeerStreamState predecessor) of
    Left (PeerStream.PeerStreamProtocolProblem _) -> peerInvariantFault
    Left (PeerStream.PeerStreamInvariantProblem _) -> peerInvariantFault
    Right prepared ->
      let (successorStream, retry) = PeerStream.commitDispatchOutcome prepared
          successor = replaceStartupPeerStreamState successorStream predecessor
          withReceiptRetry = case (outcome, retry) of
            (PeerDispatchDeferred, Just ticket) -> PeerDelivery.retryReceipt (streamDirectionDestination (peerDispatchTicketDirection ticket)) successor
            (PeerDispatchFailed, Just ticket) -> PeerDelivery.retryReceipt (streamDirectionDestination (peerDispatchTicketDirection ticket)) successor
            _ -> successor
          effect
            | allowReplacement =
                ticketEffect (PeerStream.preparedDispatchOutcomeTicket prepared)
            | otherwise = emptyEffectBatch
       in checkedPeerSuccess
            withReceiptRetry
            effect

-- | Settle one dispatch attempt selected before the drain cut without creating
-- replacement work.
applyDrainingPeerDispatchObservation ::
  PeerDispatchAttempt PeerLogicalPayload ->
  PeerDispatchOutcome ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyDrainingPeerDispatchObservation attempt outcome predecessor =
  case PeerStream.prepareDrainingDispatchOutcome
    attempt
    outcome
    (startupPeerStreamState predecessor) of
    Left _ -> peerInvariantFault
    Right prepared ->
      let (successorStream, _) =
            PeerStream.commitDrainingDispatchOutcome prepared
       in checkedPeerSuccess
            (replaceStartupPeerStreamState successorStream predecessor)
            emptyEffectBatch

-- | Clear every current logical dispatch binding at the admission cut.
--
-- Discovery remains intact and all stream payload/progress evidence survives.
-- The caller installs the returned state only together with the Draining phase,
-- so the transient serving product never escapes as a transition successor.
cutPeerDispatchForDrain ::
  HeraldState ->
  Either HeraldInvariantFault HeraldState
cutPeerDispatchForDrain predecessor =
  case PeerStream.prepareDispatchDrainCut (startupPeerStreamState predecessor) of
    Left _ -> peerInvariantFault
    Right prepared ->
      let (successorStream, _) = PeerStream.commitDispatchDrainCut prepared
       in Right (replaceStartupPeerStreamState successorStream predecessor)

placementUpdateOwner :: PlacementUpdate -> HeraldEpoch
placementUpdateOwner (FullPlacementSnapshot snapshot) = placementSnapshotOwner snapshot

bindingIsCurrent :: PeerBinding -> HeraldState -> Bool
bindingIsCurrent binding state =
  OracleProjection.oracleViewIsActiveHerald remote projection
    && OracleProjection.oracleViewIsActiveHerald local projection
    && Discovery.currentPeerBinding remote (startupDiscoveryState state)
      == Just binding
  where
    remote = peerBindingRemoteHeraldEpoch binding
    projection = OracleProjection.oracleView (startupOracleProjectionState state)
    local = checkedLocalHeraldEpoch (startupGenesis state)

directionMatchesBinding :: PeerBinding -> StreamDirection -> HeraldState -> Bool
directionMatchesBinding binding direction state =
  streamDirectionSource direction == checkedLocalHeraldEpoch (startupGenesis state)
    && streamDirectionDestination direction == peerBindingRemoteHeraldEpoch binding

incomingDirectionMatchesBinding :: PeerBinding -> StreamDirection -> HeraldState -> Bool
incomingDirectionMatchesBinding binding direction state =
  streamDirectionSource direction == peerBindingRemoteHeraldEpoch binding
    && streamDirectionDestination direction == checkedLocalHeraldEpoch (startupGenesis state)

peerDirection ::
  PeerBinding -> HeraldState -> Either HeraldInvariantFault StreamDirection
peerDirection binding state =
  mapStreamShape
    ( mkStreamDirection
        (checkedLocalHeraldEpoch (startupGenesis state))
        (peerBindingRemoteHeraldEpoch binding)
    )

peerDispatchBinding ::
  PeerBinding -> Either HeraldInvariantFault PeerDispatchBindingGeneration
peerDispatchBinding binding =
  mapStreamShape
    ( mkPeerDispatchBindingGeneration
        (peerBindingGenerationWord64 (peerBindingGeneration binding))
    )

dialIntentForBinding :: PeerBinding -> Discovery.State -> Maybe PeerDialIntent
dialIntentForBinding binding =
  Discovery.peerDialIntentFor
    (peerBindingRemoteHeraldId binding)
    (peerBindingRemoteHeraldEpoch binding)

ticketEffect :: Maybe PeerDispatchTicket -> EffectBatch
ticketEffect = maybe emptyEffectBatch (singletonEffectBatch . SchedulePeerDispatch)

peerProtocolClose ::
  PeerBinding -> PeerProtocolDisposition -> HeraldState -> PeerTransition
peerProtocolClose binding disposition state =
  checkedPeerSuccess state (singletonEffectBatch (RejectPeerConnection binding disposition))

mapStreamShape :: Either error value -> Either HeraldInvariantFault value
mapStreamShape = either (const peerInvariantFault) Right

mapStreamInvariant :: Either error value -> Either HeraldInvariantFault value
mapStreamInvariant result = case result of
  Left _ -> peerInvariantFault
  Right value -> Right value

mapPlacementInvariant :: Either error value -> Either HeraldInvariantFault value
mapPlacementInvariant result = case result of
  Left _ -> peerInvariantFault
  Right value -> Right value

mapAlignmentLoss :: Either problem value -> Either HeraldInvariantFault value
mapAlignmentLoss result = case result of
  Left _ -> peerInvariantFault
  Right value -> Right value

mapAlignmentTransferCoordinator ::
  Either AlignmentTransfer.AlignmentTransferCoordinatorProblem value ->
  Either HeraldInvariantFault value
mapAlignmentTransferCoordinator result = case result of
  Left _ -> peerInvariantFault
  Right value -> Right value

checkedPeerSuccess :: HeraldState -> EffectBatch -> PeerTransition
checkedPeerSuccess state effects = Right (state, effects)

peerInvariantFault :: Either HeraldInvariantFault value
peerInvariantFault =
  Left (HeraldTransitionInvariant HeraldPeerTransitionContradiction)
