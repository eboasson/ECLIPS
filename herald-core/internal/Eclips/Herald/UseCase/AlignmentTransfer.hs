{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Whole-Herald fulfilment of retained alignment subscriptions.
--
-- The nested transfer owner admits transcript order only.  This coordinator
-- composes that transcript with immutable generation evidence, Store history,
-- effective sort definitions, Oracle/topology prerequisites, and the logical
-- peer stream.  A destination Store successor and its corresponding transfer
-- successor are installed together; local source selection traverses the same
-- checked control path as a remote subscription.
module Eclips.Herald.UseCase.AlignmentTransfer
  ( AlignmentTransferCoordinatorProblem (..),
    applyAlignmentTransferControl,
    terminalSourceRetryControls,
    validateRetainedSourceSubscribe,
    validateAlignmentTransferCoordinatorRelationships,
    AlignmentTransferWorkDisposition (..),
    advanceAlignmentTransferPass,
    advanceAlignmentTransferPassExhaustive,
    sameAlignmentTransferWorkState,
    redriveDeferredDestinationControls,
    advanceAlignmentTransfers,
    advanceBootstrapAttempts,
    advanceBootstrapAttemptsExhaustive,
    advanceOrdinaryAttempts,
    advanceOrdinaryAttemptsExhaustive,
    advanceBootstrapFulfillments,
    advanceBootstrapFulfillmentsExhaustive,
    advanceReadinessAndCertificates,
    advanceReadinessAndCertificatesExhaustive,
    advanceSourcePrefixes,
    advanceSourcePrefixesExhaustive,
    advancePredecessorInputClosures,
    advancePredecessorInputClosuresExhaustive,
    reconcileAlignmentLosses,
    advanceRouteCutovers,
    deliverAlignmentLossTransferDelta,
    alignmentLossTransferDeltaControls,
    alignmentTransferRetryControls,
    alignmentTransferReconnectControls,
    alignmentPlanLossCauses,
    alignmentSnapshotWithdrawnSources,
    predecessorInputClosureCandidates,
    routeCutoverSetComplete,
    completeInputClosureWitnesses,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (filterM, foldM, guard)
import Data.ByteString qualified as ByteString
import Data.Foldable (traverse_)
import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Serialize.Put qualified as Serialize
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Alignment
  ( AlignmentCut,
    AlignmentMember,
    BootstrapEvidenceDigest,
    ContextClassGenerationId,
    FreshMemberBaseEvidence,
    HeraldPublicationPrefix (..),
    MemberReadyEvidenceDigest,
    StoreRevision,
    alignmentCutExactMembers,
    alignmentCutFreshMemberBaseEvidence,
    alignmentCutPhysicalPlacementRevisionVector,
    alignmentCutPredecessorGenerationIds,
    alignmentCutSortDefinitionOccurrenceId,
    alignmentCutSortId,
    alignmentMemberDelta,
    alignmentMemberHerald,
    alignmentMemberStoreIncarnation,
    contextClassGenerationIdBytes,
    deriveBootstrapEvidenceDigest,
    deriveMemberReadyEvidenceDigest,
    freshMemberBaseDelta,
    freshMemberBaseRevision,
    freshMemberBaseStoreIncarnation,
    memberReadyEvidenceDigestBytes,
    physicalPlacementRevisionEntries,
    storeRevisionWord64,
  )
import Eclips.Domain.Identity
  ( HeraldEpoch,
    StoreIncarnationId,
    deltaIdBytes,
    heraldEpochBytes,
    storeIncarnationIdBytes,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationCanonicalValue,
    checkedPublicationId,
    checkedPublicationSort,
    mkCheckedPublication,
  )
import Eclips.Domain.Route
  ( ReplicaStrength (..),
    destinationHerald,
    routeDestinations,
  )
import Eclips.Domain.Sort.Canonical (CanonicalDescriptor)
import Eclips.Domain.Startup (appliedProcessEnvironmentPublications)
import Eclips.Domain.Value
  ( canonicalValueByteString,
    decodeCanonicalValue,
  )
import Eclips.Herald.Alignment.Generation
  ( AlignmentGeneration,
    alignmentGenerationCut,
    alignmentGenerationId,
    alignmentGenerationRelationDestination,
    alignmentGenerationRelationSource,
    alignmentGenerationRelationStrength,
  )
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.Protocol
  ( AlignmentAck,
    AlignmentAttempt,
    AlignmentCancel,
    AlignmentCancelReason (..),
    AlignmentChange,
    AlignmentControl (..),
    AlignmentObligation,
    AlignmentObligationId,
    AlignmentProtocolProblem,
    AlignmentSourceProof (..),
    AlignmentSubscribe,
    AlignmentSubscriptionId,
    ClassMemberReady,
    DestinationStore,
    HistoricalCertificate,
    RetainedObservationOrigin (..),
    RetainedPublicationEvidence,
    RetainedStateEvidence,
    alignmentAckSubscriptionId,
    alignmentAttemptHistoricalCertificate,
    alignmentAttemptObligation,
    alignmentAttemptSourceHerald,
    alignmentAttemptSubscriptionId,
    alignmentCancel,
    alignmentCancelReason,
    alignmentCancelSubscriptionId,
    alignmentChange,
    alignmentChangeRetainedTransition,
    alignmentChangeStoreRevision,
    alignmentChangeSubscriptionId,
    alignmentCutAcceptedPublicationPrefix,
    alignmentLive,
    alignmentLiveSubscriptionId,
    alignmentObligationCause,
    alignmentObligationDestinationGeneration,
    alignmentObligationDestinationStores,
    alignmentObligationFrozenContextPathStrength,
    alignmentObligationIdValue,
    alignmentObligationSortDefinitionOccurrenceId,
    alignmentObligationSortId,
    alignmentObligationSourceGeneration,
    alignmentRouteCutoverMarker,
    alignmentSnapshotChunkRetainedStates,
    alignmentSnapshotChunkSubscriptionId,
    alignmentSnapshotEndSubscriptionId,
    alignmentSnapshotStartBaseRevision,
    alignmentSnapshotStartSubscriptionId,
    alignmentSubscribe,
    alignmentSubscribeObligation,
    alignmentSubscribeObligationId,
    alignmentSubscribeSourceProof,
    alignmentSubscribeSourceStoreIncarnation,
    alignmentSubscribeSubscriptionId,
    alignmentSubscriptionIdDestinationHerald,
    alignmentSubscriptionIdSequence,
    alignmentSubscriptionSequenceWord64,
    classMemberReadyPredecessorAndBasePrefixDigest,
    classMemberReadySetDigest,
    classMemberReadyStoreIncarnation,
    classMemberReadyStoreRevision,
    destinationStore,
    destinationStoreDelta,
    destinationStoreIncarnation,
    historicalCertificate,
    historicalCertificateCanonicalBytes,
    historicalCertificateCertifiedStoreRevision,
    historicalCertificateClassGeneration,
    historicalCertificateDigest,
    historicalCertificateSourceStoreIncarnation,
    primordialRetainedPublicationEvidence,
    retainedPublicationEvidence,
    retainedPublicationEvidenceCanonicalValue,
    retainedPublicationEvidenceOrigin,
    retainedPublicationEvidencePublicationId,
    retainedPublicationEvidenceSortDefinitionOccurrenceId,
    retainedPublicationEvidenceSortId,
    retainedPublicationEvidenceSourceStrength,
    retainedStateEvidence,
    retainedStateEvidenceRepresentative,
    retainedStateEvidenceStrengthWitness,
  )
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Disappearance.Gate qualified as DisappearanceGate
import Eclips.Herald.Disappearance.OwnerEvidence qualified as DisappearanceEvidence
import Eclips.Herald.Disappearance.Protocol qualified as DisappearanceProtocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.Discovery
  ( PeerBinding,
    peerBindingRemoteHeraldEpoch,
  )
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    PeerProtocolDisposition (ClosePeerControlProtocol),
    orderedEffectBatch,
  )
import Eclips.Herald.EffectivePublication
  ( checkEffectivePublicationContext,
    interpretEffectivePublication,
  )
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch, checkedSystemId)
import Eclips.Herald.Input (PeerControl (..))
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerDelivery qualified as DeliveryOwner
import Eclips.Herald.PeerPayload
  ( PeerLogicalPayload (PeerLogicalRouteCutover),
    peerLogicalAssignmentReceipt,
    peerLogicalPayloadItem,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement qualified as Placement
import Eclips.Herald.Placement.State qualified as PlacementState
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Query
  ( ResolvedQuery,
    resolvedQueryBranchDelta,
    resolvedQueryBranchStoreIncarnation,
    resolvedQueryBranches,
  )
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.State
  ( HeraldState,
    clearStartupCoordinationScheduling,
    replaceStartupAlignmentCutQueryCache,
    replaceStartupAlignmentState,
    replaceStartupApplicationState,
    replaceStartupControlledState,
    replaceStartupDisappearanceState,
    replaceStartupPeerStreamState,
    replaceStartupPlacementState,
    replaceStartupStoreState,
    replaceStartupWaitState,
    startupAlignmentCutQueryCache,
    startupAlignmentState,
    startupApplicationState,
    startupControlledState,
    startupDisappearanceState,
    startupDiscoveryState,
    startupGenesis,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupWaitState,
  )
import Eclips.Herald.Store.Observation qualified as StoreObservation
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    sortOccurrence,
    sortOccurrenceDefinition,
    sortOccurrenceSortId,
  )
import Eclips.Herald.UseCase.Alignment qualified as AlignmentCoordinator
import Eclips.Herald.UseCase.PeerInput qualified as PeerInput
import Eclips.Herald.Wait.State qualified as Wait

-- | The peer-control caller maps a remote violation to a scoped connection
-- close and an owner contradiction to the Herald invariant fault.  Detailed
-- leaf errors deliberately do not escape their ownership boundary.
data AlignmentTransferCoordinatorProblem
  = AlignmentTransferRemoteProtocolViolation
  | AlignmentTransferOwnerContradiction
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Exact semantic progress for one whole-Herald alignment pass.  Every
-- changed value is contributed by an admitted owner transition (including
-- synchronous loopback controls and retirement of completed work candidates);
-- effects alone never constitute progress.
data AlignmentTransferWorkDisposition
  = AlignmentTransferWorkUnchanged
  | AlignmentTransferWorkChanged
  deriving stock (Eq, Ord, Show)

instance Semigroup AlignmentTransferWorkDisposition where
  AlignmentTransferWorkChanged <> _ = AlignmentTransferWorkChanged
  _ <> AlignmentTransferWorkChanged = AlignmentTransferWorkChanged
  AlignmentTransferWorkUnchanged <> AlignmentTransferWorkUnchanged =
    AlignmentTransferWorkUnchanged

instance Monoid AlignmentTransferWorkDisposition where
  mempty = AlignmentTransferWorkUnchanged

transferWorkDisposition ::
  Transfer.AlignmentTransferDisposition ->
  AlignmentTransferWorkDisposition
transferWorkDisposition disposition = case disposition of
  Transfer.AlignmentTransferUnchanged -> AlignmentTransferWorkUnchanged
  Transfer.AlignmentTransferRetained -> AlignmentTransferWorkChanged
  Transfer.AlignmentTransferHeld -> AlignmentTransferWorkChanged
  Transfer.AlignmentTransferAdvanced -> AlignmentTransferWorkChanged

workWhen :: Bool -> AlignmentTransferWorkDisposition
workWhen changed =
  if changed
    then AlignmentTransferWorkChanged
    else AlignmentTransferWorkUnchanged

-- | Apply one already decoded transfer control from a current binding.
applyAlignmentTransferControl ::
  PeerBinding ->
  AlignmentControl ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch)
applyAlignmentTransferControl binding control predecessor = do
  let remote = peerBindingRemoteHeraldEpoch binding
  (successor, replies) <- applyCheckedTransferControl remote control predecessor
  let retainedCancellationEffects =
        newlyRetainedCancellationEffects
          remote
          control
          replies
          predecessor
          successor
  Right
    ( successor,
      orderedEffectBatch
        ( [SendPeerControl binding (PeerAlignmentControl reply) | reply <- replies]
            <> retainedCancellationEffects
        )
    )

newlyRetainedCancellationEffects ::
  HeraldEpoch ->
  AlignmentControl ->
  [AlignmentControl] ->
  HeraldState ->
  HeraldState ->
  [HeraldEffect]
newlyRetainedCancellationEffects incomingRemote incoming replies predecessor successor =
  [ SendPeerControl binding (PeerAlignmentControl (AlignmentCancelled cancellation))
  | (identifier, destination) <- Transfer.destinationSubscriptionEntries successorTransfer,
    Just cancellation <- [Transfer.destinationSubscriptionCancellation destination],
    Map.lookup identifier predecessorCancellations /= Just cancellation,
    Just remote <- [destinationSourceHerald destination],
    remote /= local,
    not
      ( remote == incomingRemote
          && ( incoming == AlignmentCancelled cancellation
                 || AlignmentCancelled cancellation `elem` replies
             )
      ),
    Just binding <- [Discovery.currentPeerBinding remote (startupDiscoveryState successor)]
  ]
  where
    local = checkedLocalHeraldEpoch (startupGenesis successor)
    predecessorTransfer =
      Alignment.alignmentTransferState (startupAlignmentState predecessor)
    successorTransfer =
      Alignment.alignmentTransferState (startupAlignmentState successor)
    predecessorCancellations =
      Map.fromList
        [ (identifier, cancellation)
        | (identifier, destination) <-
            Transfer.destinationSubscriptionEntries predecessorTransfer,
          Just cancellation <- [Transfer.destinationSubscriptionCancellation destination]
        ]

-- | Level-trigger all finite local alignment work.  A local source is driven
-- through the same checked control handlers until its reply queue is empty;
-- remote work is retained and reoffered on reconnect.
advanceAlignmentTransfers ::
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch)
advanceAlignmentTransfers initial = fixedPoint initial mempty
  where
    fixedPoint predecessor effects = do
      (successor, passEffects, disposition) <-
        advanceAlignmentTransferPass predecessor
      let accumulated = effects <> passEffects
      case disposition of
        AlignmentTransferWorkUnchanged -> Right (successor, accumulated)
        AlignmentTransferWorkChanged -> fixedPoint successor accumulated

-- | The legacy outer retirement loop compares whole Herald states. Consuming
-- notifications alone must not create another semantic round (which could
-- replay deferred replies). Ordinary State equality continues to include all
-- scheduling data; only these temporary comparison copies erase dirty work.
sameAlignmentTransferWorkState :: HeraldState -> HeraldState -> Bool
sameAlignmentTransferWorkState left right = semantic left == semantic right
  where
    semantic predecessor =
      let state = clearStartupCoordinationScheduling predecessor
          alignment = startupAlignmentState state
          transfer = Alignment.alignmentTransferState alignment
       in replaceStartupStoreState
            (Store.clearSourceStoreChanges (startupStoreState state))
            ( replaceStartupAlignmentState
                (Alignment.clearInputClosureChanges (Alignment.replaceAlignmentTransferState (Transfer.clearInputClosureWork (Transfer.clearSourcePrefixWork transfer)) alignment))
                state
            )

-- | Execute exactly one ordered alignment pass and return the admitted work
-- witness that determines whether another pass is necessary.
advanceAlignmentTransferPass ::
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceAlignmentTransferPass = advanceAlignmentTransferPassWith SelectiveTransferTraversal advanceSourcePrefixes advancePredecessorInputClosures

-- | Differential-test oracle: original full inventories and readiness joins
-- within the same ordered coordinator stages. Dirty sets never choose its
-- entries, while journal consumption keeps ordinary metadata comparable.
advanceAlignmentTransferPassExhaustive ::
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceAlignmentTransferPassExhaustive = advanceAlignmentTransferPassWith ExhaustiveTransferTraversal advanceSourcePrefixesExhaustive advancePredecessorInputClosuresExhaustive

data TransferTraversal = SelectiveTransferTraversal | ExhaustiveTransferTraversal
  deriving stock (Eq, Show)

advanceAlignmentTransferPassWith ::
  TransferTraversal ->
  (HeraldState -> Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)) ->
  (HeraldState -> Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)) ->
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceAlignmentTransferPassWith traversal advancePrefixes advanceClosures predecessor = do
  (afterDeferred, deferredEffects, deferredWork) <- redriveDeferredDestinationControls predecessor
  (afterLost, lostEffects, lostWork) <- cancelLostDestinationsWith traversal afterDeferred
  (afterBootstrap, bootstrapEffects, bootstrapWork) <-
    advanceBootstrapAttemptsWith traversal afterLost
  (afterAttempts, attemptEffects, attemptWork) <-
    advanceOrdinaryAttemptsWith traversal afterBootstrap
  (afterSources, sourceEffects, sourceWork) <-
    advancePrefixes afterAttempts
  (afterRecoveryPlans, recoveryPlanEffects, recoveryPlanDisposition) <-
    mapOwner
      ( case traversal of
          SelectiveTransferTraversal -> AlignmentCoordinator.advanceAlignmentGenerationsWithDisposition afterSources
          ExhaustiveTransferTraversal -> AlignmentCoordinator.advanceAlignmentGenerationsWithDispositionExhaustive afterSources
      )
  (afterFulfilled, fulfillmentEffects, fulfillmentWork) <-
    advanceBootstrapFulfillmentsWith traversal afterRecoveryPlans
  (afterReady, readyEffects, readyWork) <-
    advanceReadinessAndCertificatesWith traversal afterFulfilled
  (afterCutovers, cutoverEffects, cutoverWork) <-
    advanceRouteCutovers afterReady
  (afterClosure, closureEffects, closureWork) <-
    advanceClosures afterCutovers
  (successor, wakeEffects, wakeWork) <- completeAlignmentWaits afterClosure
  let passEffects =
        deferredEffects
          <> lostEffects
          <> bootstrapEffects
          <> attemptEffects
          <> sourceEffects
          <> recoveryPlanEffects
          <> fulfillmentEffects
          <> readyEffects
          <> cutoverEffects
          <> closureEffects
          <> wakeEffects
      recoveryPlanWork = case recoveryPlanDisposition of
        AlignmentCoordinator.AlignmentGenerationWorkUnchanged ->
          AlignmentTransferWorkUnchanged
        AlignmentCoordinator.AlignmentGenerationWorkChanged ->
          AlignmentTransferWorkChanged
  Right
    ( successor,
      passEffects,
      deferredWork
        <> lostWork
        <> bootstrapWork
        <> attemptWork
        <> sourceWork
        <> recoveryPlanWork
        <> fulfillmentWork
        <> readyWork
        <> cutoverWork
        <> closureWork
        <> wakeWork
    )

-- | Project the exact protocol roots introduced by one atomic alignment-loss
-- fold.  Loss admission may retire several Stores at once, so controls are
-- derived from the final transfer delta rather than from intermediate loss
-- plans: this suppresses replacements invalidated by a later Store loss and
-- emits only the final dominant cancellation.
--
-- A source-incarnation-loss cancellation is source-to-destination traffic and
-- is therefore never reflected by the destination that admitted the remote
-- placement loss.  A source owner emits its newly retained source cancellation
-- here, while every other changed cancellation is destination-to-source
-- traffic.  All cancellations precede every replacement Subscribe.  Once the
-- loss controls have reached their local owners, waits naming a passivated
-- exact Store incarnation are released through their permitted spurious-ready
-- result so no application remains parked on an impossible branch.
deliverAlignmentLossTransferDelta ::
  HeraldState ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch)
deliverAlignmentLossTransferDelta predecessor successor = do
  controls <-
    alignmentLossTransferDeltaControls
      local
      predecessorTransfer
      successorTransfer
  (afterControls, controlEffects) <-
    foldM deliverOne (successor, mempty) controls
  (afterWaits, waitEffects, _) <- completeAlignmentWaits afterControls
  Right (afterWaits, controlEffects <> waitEffects)
  where
    predecessorTransfer =
      Alignment.alignmentTransferState (startupAlignmentState predecessor)
    successorTransfer =
      Alignment.alignmentTransferState (startupAlignmentState successor)
    local = checkedLocalHeraldEpoch (startupGenesis successor)

    deliverOne (state, effects) (remote, control) = do
      (afterDelivery, deliveryEffects) <- deliverControls remote [control] state
      Right (afterDelivery, effects <> deliveryEffects)

-- | Pure final-delta projection used by the atomic loss boundaries.  An
-- existing source transcript newly terminalized by local Store loss owns the
-- SourceLost wire control; destination transcripts own every other
-- cancellation and every replacement Subscribe.
alignmentLossTransferDeltaControls ::
  HeraldEpoch ->
  Transfer.State ->
  Transfer.State ->
  Either AlignmentTransferCoordinatorProblem [(HeraldEpoch, AlignmentControl)]
alignmentLossTransferDeltaControls local predecessorTransfer successorTransfer =
  traverse resolveTarget projected
  where
    predecessorDestinations =
      Map.fromList (Transfer.destinationSubscriptionEntries predecessorTransfer)
    predecessorSources =
      Map.fromList (Transfer.sourceSubscriptionEntries predecessorTransfer)
    successorDestinations = Transfer.destinationSubscriptionEntries successorTransfer
    successorSources = Map.fromList (Transfer.sourceSubscriptionEntries successorTransfer)

    projected =
      changedSourceCancellations
        <> changedDestinationCancellations
        <> newSubscribes

    changedSourceCancellations =
      [ ( Left (alignmentSubscriptionIdDestinationHerald identifier),
          AlignmentCancelled cancellation
        )
      | (identifier, source) <- Map.toAscList successorSources,
        Just predecessorSource <- [Map.lookup identifier predecessorSources],
        Just cancellation <- [Transfer.sourceSubscriptionCancellation source],
        alignmentCancelReason cancellation == AlignmentSourceIncarnationLost,
        Transfer.sourceSubscriptionCancellation predecessorSource
          /= Just cancellation
      ]

    changedDestinationCancellations =
      [ (Right destination, AlignmentCancelled cancellation)
      | (identifier, destination) <- successorDestinations,
        Just cancellation <- [Transfer.destinationSubscriptionCancellation destination],
        cancellationIsNew identifier destination cancellation
      ]

    cancellationIsNew identifier destination cancellation =
      case Map.lookup identifier predecessorDestinations of
        Nothing ->
          destinationSourceHerald destination == Just local
            && Map.member identifier successorSources
        Just predecessorDestination ->
          Transfer.destinationSubscriptionCancellation predecessorDestination
            /= Just cancellation
            && alignmentCancelReason cancellation /= AlignmentSourceIncarnationLost

    newSubscribes =
      [ ( Right destination,
          AlignmentSubscribeRequested
            (Transfer.destinationSubscriptionSubscribe destination)
        )
      | (identifier, destination) <- successorDestinations,
        Map.notMember identifier predecessorDestinations,
        Transfer.destinationSubscriptionCancellation destination == Nothing
      ]

    resolveTarget (target, control) = do
      remote <- case target of
        Left sourceDestination -> Right sourceDestination
        Right destination ->
          maybe
            (Left AlignmentTransferOwnerContradiction)
            Right
            (destinationSourceHerald destination)
      Right (remote, control)

-- | Reconnect roots for alignment transfers involving one remote Herald.
-- Logical data-stream repair remains owned by 'PeerStream'; replaying these
-- destination-owned roots makes the source reconstruct the remaining control
-- exchange without proactively duplicating its retained replies.
alignmentTransferRetryControls ::
  HeraldEpoch ->
  HeraldEpoch ->
  HeraldState ->
  [AlignmentControl]
alignmentTransferRetryControls local remote state
  | local == remote = []
  | otherwise = alignmentTransferReconnectControls remote transfer
  where
    alignment = startupAlignmentState state
    transfer = Alignment.alignmentTransferState alignment

-- | Destination-owned subscription roots are sufficient to reconstruct a
-- transfer after reconnect. Replayed Subscribe elicits the source's retained
-- unacknowledged suffix or cancellation; transferred data in turn elicits its
-- exact cumulative acknowledgement. A retained source-loss cancellation was
-- itself received from the source and must not be reflected back to it.
alignmentTransferReconnectControls ::
  HeraldEpoch ->
  Transfer.State ->
  [AlignmentControl]
alignmentTransferReconnectControls remote transfer = destinationControls
  where
    destinationControls =
      concat
        [ filter
            isReconnectRoot
            (Transfer.destinationRetryControls identifier transfer)
        | (identifier, destination) <- Transfer.destinationSubscriptionEntries transfer,
          destinationSourceHerald destination == Just remote
        ]

    isReconnectRoot (AlignmentAcknowledged _) = False
    isReconnectRoot (AlignmentCancelled cancellation) =
      alignmentCancelReason cancellation /= AlignmentSourceIncarnationLost
    isReconnectRoot _ = True

applyCheckedTransferControl ::
  HeraldEpoch ->
  AlignmentControl ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, [AlignmentControl])
applyCheckedTransferControl = applyCheckedTransferControlWithReplay False

applyCheckedTransferControlWithReplay ::
  Bool ->
  HeraldEpoch ->
  AlignmentControl ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, [AlignmentControl])
applyCheckedTransferControlWithReplay replay remote control predecessor = case control of
  AlignmentSubscribeRequested subscribe ->
    serveAlignmentSubscribe remote subscribe predecessor
  AlignmentSnapshotStarted start ->
    applyDestinationControl
      replay
      control
      remote
      (alignmentSnapshotStartSubscriptionId start)
      (Transfer.prepareDestinationSnapshotStart start)
      predecessor
  AlignmentSnapshotChunkTransferred chunk ->
    applyDestinationControl
      replay
      control
      remote
      (alignmentSnapshotChunkSubscriptionId chunk)
      (Transfer.prepareDestinationSnapshotChunk chunk)
      predecessor
  AlignmentSnapshotEnded end ->
    applyDestinationControl
      replay
      control
      remote
      (alignmentSnapshotEndSubscriptionId end)
      (Transfer.prepareDestinationSnapshotEnd end)
      predecessor
  AlignmentChangeTransferred change ->
    applyDestinationControl
      replay
      control
      remote
      (alignmentChangeSubscriptionId change)
      (Transfer.prepareDestinationChange change)
      predecessor
  AlignmentLiveAdvertised live ->
    applyDestinationControl
      replay
      control
      remote
      (alignmentLiveSubscriptionId live)
      (Transfer.prepareDestinationLive live)
      predecessor
  AlignmentAcknowledged acknowledgement ->
    applySourceAcknowledgement remote acknowledgement predecessor
  AlignmentCancelled cancellation ->
    applyCancellation remote cancellation predecessor
  _ -> Left AlignmentTransferRemoteProtocolViolation

serveAlignmentSubscribe ::
  HeraldEpoch ->
  AlignmentSubscribe ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, [AlignmentControl])
serveAlignmentSubscribe remote subscribe predecessor = do
  terminal <- terminalSourceRetryControls remote subscribe transfer
  case terminal of
    Just controls -> Right (predecessor, controls)
    Nothing -> do
      base <- validateSourceSubscribe remote subscribe predecessor
      case Store.retainedStoreAlignmentSnapshotAt sourceStore base (startupStoreState predecessor) of
        Left (Store.StoreHistoryIncarnationMissing _) ->
          rejectSource subscribe AlignmentSourceIncarnationLost predecessor
        Left _ -> Left AlignmentTransferOwnerContradiction
        Right snapshot -> do
          projected <- projectSourceSnapshot predecessor snapshot
          facts <-
            mapOwner
              ( traverse
                  effectiveSnapshotFactEvidence
                  (StoreObservation.effectiveSnapshotFacts projected)
              )
          prepared <-
            mapOwner
              ( Transfer.prepareSourceSubscription
                  subscribe
                  base
                  facts
                  (sourceLiveMode subscribe)
                  transfer
              )
          let initialControls = Transfer.preparedSourceSubscriptionControls prepared
              (withSource, _) = Transfer.commitSourceSubscription prepared
              aligned = Alignment.replaceAlignmentTransferState withSource alignment
              afterSource = replaceStartupAlignmentState aligned predecessor
          appendCurrentSourceChanges identifier initialControls afterSource
  where
    identifier = alignmentSubscribeSubscriptionId subscribe
    sourceStore = alignmentSubscribeSourceStoreIncarnation subscribe
    alignment = startupAlignmentState predecessor
    transfer = Alignment.alignmentTransferState alignment

-- | Resolve an exact replay of a terminal source subscription without
-- consulting Store history that may have been lost.  The immutable retained
-- subscription authenticates the replay; unequal bytes under its never-reused
-- identifier remain a remote protocol violation.
terminalSourceRetryControls ::
  HeraldEpoch ->
  AlignmentSubscribe ->
  Transfer.State ->
  Either AlignmentTransferCoordinatorProblem (Maybe [AlignmentControl])
terminalSourceRetryControls remote subscribe transfer = do
  requireRemote (alignmentSubscriptionIdDestinationHerald identifier == remote)
  case Transfer.lookupSourceSubscription identifier transfer of
    Nothing -> Right Nothing
    Just retained -> do
      requireRemote (Transfer.sourceSubscriptionSubscribe retained == subscribe)
      Right
        ( case Transfer.sourceSubscriptionCancellation retained of
            Nothing -> Nothing
            Just cancellation -> Just [AlignmentCancelled cancellation]
        )
  where
    identifier = alignmentSubscribeSubscriptionId subscribe

sourceLiveMode :: AlignmentSubscribe -> Transfer.SourceLiveMode
sourceLiveMode subscribe = case alignmentSubscribeSourceProof subscribe of
  BootstrapProof _ _
    | alignmentObligationSourceGeneration obligation
        /= alignmentObligationDestinationGeneration obligation ->
        Transfer.SourceLiveWithheld
  _ -> Transfer.SourceLiveImmediate
  where
    obligation = alignmentSubscribeObligation subscribe

rejectSource ::
  AlignmentSubscribe ->
  AlignmentCancelReason ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, [AlignmentControl])
rejectSource subscribe reason predecessor = do
  let alignment = startupAlignmentState predecessor
  prepared <-
    mapOwner
      ( Transfer.prepareSourceRejection
          subscribe
          reason
          (Alignment.alignmentTransferState alignment)
      )
  let controls = Transfer.preparedSourceRejectionControls prepared
      (transfer, _) = Transfer.commitSourceRejection prepared
  Right
    ( replaceStartupAlignmentState
        (Alignment.replaceAlignmentTransferState transfer alignment)
        predecessor,
      controls
    )

validateSourceSubscribe ::
  HeraldEpoch ->
  AlignmentSubscribe ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem StoreRevision
validateSourceSubscribe remote subscribe state = do
  let alignment = startupAlignmentState state
      obligation = alignmentSubscribeObligation subscribe
      sourceGenerationId = alignmentObligationSourceGeneration obligation
      destinationGenerationId = alignmentObligationDestinationGeneration obligation
      sourceStore = alignmentSubscribeSourceStoreIncarnation subscribe
      local = checkedLocalHeraldEpoch (startupGenesis state)
  sourceGeneration <- requireGeneration sourceGenerationId alignment
  destinationGeneration <- requireGeneration destinationGenerationId alignment
  requireRemote
    (acceptanceRetained sourceGenerationId local alignment && acceptanceRetained destinationGenerationId local alignment)
  requireRemote
    (acceptanceRetained destinationGenerationId remote alignment)
  requireRemote
    ( memberHeraldForStore sourceStore sourceGeneration
        == Just local
    )
  validateRetainedSourceSlot sourceStore sourceGeneration state
  case alignmentSubscribeSourceProof subscribe of
    FinalCertificate generation digest -> do
      requireOrdinaryObligationShape
        remote
        obligation
        sourceGeneration
        destinationGeneration
        alignment
      requireRemote (generation == sourceGenerationId)
      retained <-
        maybe
          (Left AlignmentTransferRemoteProtocolViolation)
          Right
          (findCertificate sourceGenerationId sourceStore alignment)
      requireRemote (historicalCertificateDigest retained == digest)
      Right (historicalCertificateCertifiedStoreRevision retained)
    BootstrapProof generation digest -> do
      requireBootstrapObligationShape
        remote
        obligation
        sourceGeneration
        destinationGeneration
        alignment
      requireRemote (generation == destinationGenerationId)
      requireRemote
        (acceptanceRetained destinationGenerationId local alignment)
      validateBootstrapSourceProof
        sourceGeneration
        destinationGeneration
        sourceStore
        digest
        state

-- | Re-run the complete carried source-subscribe authority law against one
-- retained transcript.  Whole-state validation uses this so a checked leaf
-- cannot self-justify mutated destination, path, acceptance, or proof facts.
validateRetainedSourceSubscribe ::
  HeraldEpoch ->
  AlignmentSubscribe ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem StoreRevision
validateRetainedSourceSubscribe = validateSourceSubscribe

validateBootstrapSourceProof ::
  AlignmentGeneration ->
  AlignmentGeneration ->
  StoreIncarnationId ->
  BootstrapEvidenceDigest ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem StoreRevision
validateBootstrapSourceProof sourceGeneration destinationGeneration sourceStore observed state =
  case predecessorEvidence <> freshEvidence of
    [expected] -> do
      requireRemote (fst expected == observed)
      Right (snd expected)
    _ -> Left AlignmentTransferRemoteProtocolViolation
  where
    destinationCut = alignmentGenerationCut destinationGeneration
    sourceId = alignmentGenerationId sourceGeneration
    destinationId = alignmentGenerationId destinationGeneration
    predecessorEvidence =
      [ ( predecessorBootstrapEvidenceDigest destinationId certificate,
          historicalCertificateCertifiedStoreRevision certificate
        )
      | sourceId `elem` alignmentCutPredecessorGenerationIds destinationCut,
        Just certificate <-
          [ findCertificate
              sourceId
              sourceStore
              (startupAlignmentState state)
          ],
        historicalCertificateClassGeneration certificate == sourceId,
        historicalCertificateSourceStoreIncarnation certificate == sourceStore
      ]
    freshEvidence =
      [ (freshBootstrapEvidenceDigest destinationId fresh, freshMemberBaseRevision fresh)
      | sourceId == destinationId,
        fresh <- alignmentCutFreshMemberBaseEvidence destinationCut,
        freshMemberBaseStoreIncarnation fresh == sourceStore,
        freshMemberHostedLocally fresh destinationGeneration state
      ]

requireOrdinaryObligationShape ::
  HeraldEpoch ->
  AlignmentObligation ->
  AlignmentGeneration ->
  AlignmentGeneration ->
  Alignment.State ->
  Either AlignmentTransferCoordinatorProblem ()
requireOrdinaryObligationShape remote obligation sourceGeneration destinationGeneration alignment = do
  let sourceCut = alignmentGenerationCut sourceGeneration
      destinationCut = alignmentGenerationCut destinationGeneration
      suppliedDestinations =
        Set.fromList (NonEmpty.toList (alignmentObligationDestinationStores obligation))
      expectedDestinations =
        Set.fromList
          [ destinationFor member
          | member <- NonEmpty.toList (alignmentCutExactMembers destinationCut),
            alignmentMemberHerald member == remote
          ]
      related =
        any
          ( \relation ->
              alignmentGenerationRelationSource relation
                == alignmentGenerationId sourceGeneration
                && alignmentGenerationRelationDestination relation
                  == alignmentGenerationId destinationGeneration
                && alignmentGenerationRelationStrength relation
                  == alignmentObligationFrozenContextPathStrength obligation
          )
          (Alignment.alignmentGenerationRelationEntries alignment)
      promoted =
        any
          ( \(key, receipt) ->
              Alignment.alignmentPromotionKeyCause key
                == alignmentObligationCause obligation
                && Alignment.alignmentPromotionKeySort key
                  == sortOccurrence
                    (alignmentObligationSortId obligation)
                    (alignmentObligationSortDefinitionOccurrenceId obligation)
                && all
                  (`elem` Alignment.alignmentPromotionReceiptGenerationIds receipt)
                  [ alignmentGenerationId sourceGeneration,
                    alignmentGenerationId destinationGeneration
                  ]
          )
          (Alignment.alignmentPromotionEntries alignment)
  requireRemote
    ( alignmentCutSortId sourceCut == alignmentObligationSortId obligation
        && alignmentCutSortDefinitionOccurrenceId sourceCut
          == alignmentObligationSortDefinitionOccurrenceId obligation
        && alignmentCutSortId destinationCut == alignmentObligationSortId obligation
        && alignmentCutSortDefinitionOccurrenceId destinationCut
          == alignmentObligationSortDefinitionOccurrenceId obligation
    )
  requireRemote (not (Set.null expectedDestinations))
  requireRemote (suppliedDestinations == expectedDestinations)
  requireRemote related
  requireRemote promoted

requireBootstrapObligationShape ::
  HeraldEpoch ->
  AlignmentObligation ->
  AlignmentGeneration ->
  AlignmentGeneration ->
  Alignment.State ->
  Either AlignmentTransferCoordinatorProblem ()
requireBootstrapObligationShape remote obligation sourceGeneration destinationGeneration alignment = do
  let sourceCut = alignmentGenerationCut sourceGeneration
      destinationCut = alignmentGenerationCut destinationGeneration
      suppliedDestinations = NonEmpty.toList (alignmentObligationDestinationStores obligation)
      exactRemoteMembers =
        [ destinationFor member
        | member <- NonEmpty.toList (alignmentCutExactMembers destinationCut),
          alignmentMemberHerald member == remote
        ]
      sourceId = alignmentGenerationId sourceGeneration
      destinationId = alignmentGenerationId destinationGeneration
      hasValidAncestry =
        sourceId == destinationId
          || sourceId `elem` alignmentCutPredecessorGenerationIds destinationCut
      promoted =
        any
          ( \(key, receipt) ->
              Alignment.alignmentPromotionKeyCause key
                == alignmentObligationCause obligation
                && Alignment.alignmentPromotionKeySort key
                  == sortOccurrence
                    (alignmentObligationSortId obligation)
                    (alignmentObligationSortDefinitionOccurrenceId obligation)
                && destinationId
                  `elem` Alignment.alignmentPromotionReceiptGenerationIds receipt
          )
          (Alignment.alignmentPromotionEntries alignment)
  requireRemote
    ( alignmentCutSortId sourceCut == alignmentObligationSortId obligation
        && alignmentCutSortDefinitionOccurrenceId sourceCut
          == alignmentObligationSortDefinitionOccurrenceId obligation
        && alignmentCutSortId destinationCut == alignmentObligationSortId obligation
        && alignmentCutSortDefinitionOccurrenceId destinationCut
          == alignmentObligationSortDefinitionOccurrenceId obligation
    )
  requireRemote
    ( case suppliedDestinations of
        [destination] -> destination `elem` exactRemoteMembers
        _ -> False
    )
  requireRemote
    (alignmentObligationFrozenContextPathStrength obligation == Normal)
  requireRemote hasValidAncestry
  requireRemote promoted

validateRetainedSourceSlot ::
  StoreIncarnationId ->
  AlignmentGeneration ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem ()
validateRetainedSourceSlot sourceStore generation state =
  case Store.lookupRetainedStoreSlot sourceStore (startupStoreState state) of
    Nothing -> Right ()
    Just slot ->
      requireRemote
        ( any
            ( \member ->
                alignmentMemberStoreIncarnation member == sourceStore
                  && alignmentMemberDelta member == Store.storeSlotDelta slot
            )
            members
            && Store.storeSlotSortId slot == alignmentCutSortId cut
            && Store.storeSlotOccurrenceId slot
              == alignmentCutSortDefinitionOccurrenceId cut
        )
  where
    cut = alignmentGenerationCut generation
    members = NonEmpty.toList (alignmentCutExactMembers cut)

destinationFor :: AlignmentMember -> DestinationStore
destinationFor member =
  destinationStore
    (alignmentMemberDelta member)
    (alignmentMemberStoreIncarnation member)

effectiveSnapshotFactEvidence ::
  StoreObservation.EffectiveSnapshotFact ->
  Either Eclips.Herald.Alignment.Protocol.AlignmentProtocolProblem RetainedStateEvidence
effectiveSnapshotFactEvidence fact = do
  representative <-
    storeObservationEvidence
      (StoreObservation.effectiveSnapshotFactRepresentative fact)
  witness <-
    storeObservationEvidence
      (StoreObservation.effectiveSnapshotFactStrengthWitness fact)
  retainedStateEvidence
    representative
    witness
    (StoreObservation.effectiveSnapshotFactStrength fact)

projectSourceSnapshot ::
  HeraldState ->
  Store.RetainedStoreSnapshot ->
  Either AlignmentTransferCoordinatorProblem StoreObservation.EffectiveSnapshot
projectSourceSnapshot state snapshot = do
  evidence <-
    effectiveStoreEvidenceMap
      state
      (StoreObservation.effectiveSnapshotRawEvidence snapshot)
  mapOwner (StoreObservation.filterEffectiveSnapshot evidence snapshot)

storeObservationEvidence ::
  Store.RetainedStoreObservation ->
  Either Eclips.Herald.Alignment.Protocol.AlignmentProtocolProblem RetainedPublicationEvidence
storeObservationEvidence observation =
  case Store.retainedStoreObservationOrigin observation of
    Store.PrimordialStoreObservation ->
      Right
        ( primordialRetainedPublicationEvidence
            identifier
            (checkedPublicationSort publication)
            occurrence
            (checkedPublicationCanonicalValue publication)
            strength
        )
    Store.RoutedStoreObservation envelope ->
      retainedPublicationEvidence
        identifier
        (Store.storeObservationEnvelopeSourceProcess envelope)
        (checkedPublicationSort publication)
        occurrence
        (checkedPublicationCanonicalValue publication)
        strength
        (Store.storeObservationEnvelopeSourceTopology envelope)
        (Store.storeObservationEnvelopeControlPrerequisite envelope)
        (Store.storeObservationEnvelopeStructuralStamp envelope)
  where
    publication = Store.retainedStoreObservationPublication observation
    identifier = checkedPublicationId publication
    occurrence = Store.retainedStoreObservationSortOccurrence observation
    strength = Store.retainedStoreObservationIncomingStrength observation

effectiveStoreEvidenceMap ::
  (Ord key) =>
  HeraldState ->
  [(key, CheckedPublication)] ->
  Either
    AlignmentTransferCoordinatorProblem
    (Map key StoreObservation.EffectiveStoreEvidence)
effectiveStoreEvidenceMap state rawEvidence =
  Map.fromList <$> traverse bindOne rawEvidence
  where
    controlled = startupControlledState state
    registry = startupSortRegistryState state
    bindOne (key, publication) = do
      entry <-
        maybe
          (Left AlignmentTransferOwnerContradiction)
          Right
          (SortRegistry.lookupEffectiveSort (checkedPublicationSort publication) registry)
      context <-
        mapOwner
          ( checkEffectivePublicationContext
              (SortRegistry.registryEntryDescriptor entry)
              controlled
              publication
          )
      evidence <-
        mapOwner
          ( StoreObservation.effectiveStoreEvidence
              publication
              (interpretEffectivePublication context)
          )
      Right (key, evidence)

appendCurrentSourceChanges ::
  AlignmentSubscriptionId ->
  [AlignmentControl] ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, [AlignmentControl])
appendCurrentSourceChanges identifier initialControls predecessor = do
  (successor, controls, _) <-
    appendCurrentSourceChangesWithDisposition
      identifier
      initialControls
      predecessor
  Right (successor, controls)

appendCurrentSourceChangesWithDisposition ::
  AlignmentSubscriptionId ->
  [AlignmentControl] ->
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, [AlignmentControl], AlignmentTransferWorkDisposition)
appendCurrentSourceChangesWithDisposition identifier initialControls predecessor = do
  let alignment = startupAlignmentState predecessor
      transfer = Alignment.alignmentTransferState alignment
  sourceSubscription <- requireSourceSubscription identifier transfer
  let subscribe = Transfer.sourceSubscriptionSubscribe sourceSubscription
      incarnation = alignmentSubscribeSourceStoreIncarnation subscribe
      through = Transfer.sourceSubscriptionSentThrough sourceSubscription
  slot <-
    maybe
      (Left AlignmentTransferOwnerContradiction)
      Right
      (Store.lookupRetainedStoreSlot incarnation (startupStoreState predecessor))
  changes <-
    mapOwner
      (Store.retainedStoreChangesAfter incarnation through (startupStoreState predecessor))
  effective <- projectSourceChanges predecessor changes
  projected <- traverse (storeChangeControl identifier) effective
  let live = alignmentLive identifier (Store.storeSlotRevision slot)
  prepared <- mapOwner (Transfer.prepareSourceChanges identifier projected live transfer)
  let controls = initialControls <> Transfer.preparedSourceChangeControls prepared
      (successorTransfer, disposition) = Transfer.commitSourceChanges prepared
      successorAlignment = Alignment.replaceAlignmentTransferState successorTransfer alignment
      successor = replaceStartupAlignmentState successorAlignment predecessor
  Right (successor, controls, transferWorkDisposition disposition)

storeChangeControl ::
  AlignmentSubscriptionId ->
  StoreObservation.EffectiveChangeDisposition ->
  Either AlignmentTransferCoordinatorProblem AlignmentChange
storeChangeControl identifier disposition = case disposition of
  -- Alignment transports authenticated raw evidence. Effective projection
  -- decides visibility, collapse, and terminal treatment, but must never pair
  -- a different payload with the retained publication identity.
  StoreObservation.EffectiveChangeVisible change _ ->
    makeChange change
      <$> mapOwner
        (storeObservationEvidence (Store.retainedStoreChangeObservation change))
  StoreObservation.EffectiveChangeTerminallyIgnored change _ ->
    makeChange change
      <$> mapOwner
        (storeObservationEvidence (Store.retainedStoreChangeObservation change))
  where
    makeChange change =
      alignmentChange identifier (Store.retainedStoreChangeRevision change)

projectSourceChanges ::
  HeraldState ->
  [Store.RetainedStoreChange] ->
  Either
    AlignmentTransferCoordinatorProblem
    [StoreObservation.EffectiveChangeDisposition]
projectSourceChanges state changes = do
  evidence <-
    effectiveStoreEvidenceMap
      state
      (StoreObservation.effectiveChangeRawEvidence changes)
  mapOwner (StoreObservation.projectEffectiveChanges evidence changes)

applyDestinationControl ::
  Bool ->
  AlignmentControl ->
  HeraldEpoch ->
  AlignmentSubscriptionId ->
  (Transfer.State -> Either Transfer.AlignmentTransferProblem Transfer.PreparedDestinationWork) ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, [AlignmentControl])
applyDestinationControl replay control remote identifier prepare predecessor = do
  destination <-
    requireDestinationSubscription
      identifier
      (Alignment.alignmentTransferState (startupAlignmentState predecessor))
  requireRemote (destinationSourceHerald destination == Just remote)
  let transfer = Alignment.alignmentTransferState (startupAlignmentState predecessor)
      queued = Transfer.deferredDestinationControls identifier transfer
      (rawBlocked, rawGates, rawEvidence) = destinationRawControlGate identifier control queued predecessor
      retain gates = do
        retained <- mapRemote (Transfer.retainDeferredDestinationControl identifier remote control transfer)
        leaf <-
          mapOwner
            ( DisappearanceGate.retainGatedPublicationInvalidations
                gates
                ( ByteString.concat
                    ( fmap
                        (canonicalValueByteString . retainedPublicationEvidenceCanonicalValue . snd)
                        (rawEvidence <> maybe [] (destinationEvidence . Transfer.preparedDestinationWork) preparedForEvidence)
                    )
                )
                (startupDisappearanceState predecessor)
            )
        Right
          ( replaceStartupDisappearanceState leaf
              . replaceStartupAlignmentState (Alignment.replaceAlignmentTransferState retained (startupAlignmentState predecessor))
              $ predecessor,
            []
          )
      preparedForEvidence = either (const Nothing) Just (prepare transfer)
  if rawBlocked || (not replay && not (null queued))
    then retain rawGates
    else do
      prepared <- mapRemote (prepare transfer)
      let work = Transfer.preparedDestinationWork prepared
          (blocked, gates) = destinationDisappearanceGate identifier work predecessor
      if blocked
        then retain gates
        else do
          (successor, replies) <- applyPreparedDestinationWork identifier destination prepared predecessor
          let alignment = startupAlignmentState successor
              released = Transfer.removeDeferredDestinationControl identifier remote control (Alignment.alignmentTransferState alignment)
          Right (replaceStartupAlignmentState (Alignment.replaceAlignmentTransferState released alignment) successor, replies)

-- A snapshot starts an ordered raw range. Hold its Start before it can install
-- partial assembly when its base lies after a captured marker. Matching chunks
-- in the retained range can then invalidate the probe without becoming owner
-- evidence in that probe's negative cut.
destinationRawControlGate ::
  AlignmentSubscriptionId ->
  AlignmentControl ->
  [(HeraldEpoch, AlignmentControl)] ->
  HeraldState ->
  (Bool, [Disappearance.MatchingWriteGate], [(StoreRevision, RetainedPublicationEvidence)])
destinationRawControlGate identifier control queued state =
  (rangeBlocked, gates, evidence)
  where
    leaf = startupDisappearanceState state
    snapshotBases =
      [ alignmentSnapshotStartBaseRevision start
      | (_, AlignmentSnapshotStarted start) <- queued
      ]
    startBases = case control of
      AlignmentSnapshotStarted start -> [alignmentSnapshotStartBaseRevision start]
      _ -> []
    evidence = case control of
      AlignmentSnapshotChunkTransferred chunk ->
        [ (revision, retained)
        | revision <- snapshotBases,
          fact <- alignmentSnapshotChunkRetainedStates chunk,
          retained <- [retainedStateEvidenceRepresentative fact, retainedStateEvidenceStrengthWitness fact]
        ]
      AlignmentChangeTransferred change ->
        [(alignmentChangeStoreRevision change, alignmentChangeRetainedTransition change)]
      _ -> []
    known =
      [ (witness, marker)
      | witness <- Disappearance.probeWitnesses leaf,
        witness.witnessPhase == Disappearance.ProbeCollectingView,
        Just cell <- [lookup identifier witness.witnessIncomingAlignmentCuts],
        Just marker <- [cell.alignmentCellMarker]
      ]
    rangeBlocked =
      or
        [ revision > DisappearanceProtocol.disappearanceAlignmentMarkerThroughRevision marker
        | revision <- startBases,
          marker <- fmap snd known <> unknownMarkers
        ]
    unknownMarkers =
      [ marker
      | (_, marker) <- Disappearance.pendingAlignmentMarkers leaf,
        DisappearanceProtocol.disappearanceAlignmentMarkerSubscription marker == identifier,
        Disappearance.probeWitness (DisappearanceProtocol.disappearanceAlignmentMarkerProbe marker) leaf == Nothing
      ]
    gates =
      [ witness.witnessMatchingWriteGate
      | (witness, marker) <- known,
        any
          ( \(revision, retained) ->
              revision > DisappearanceProtocol.disappearanceAlignmentMarkerThroughRevision marker
                && DisappearanceEvidence.canonicalPublicationIsSubjectRelevant
                  (DisappearanceProtocol.projectedProbeSubject witness.witnessProjectedProbe)
                  (retainedPublicationEvidenceSortId retained)
                  (retainedPublicationEvidenceSortDefinitionOccurrenceId retained)
                  (retainedPublicationEvidenceCanonicalValue retained)
          )
          evidence
      ]

destinationEvidence :: Transfer.DestinationWork -> [(StoreRevision, RetainedPublicationEvidence)]
destinationEvidence work =
  [ (revision, evidence)
  | (revision, facts) <- maybe [] pure (Transfer.destinationWorkSnapshot work),
    fact <- facts,
    evidence <- [retainedStateEvidenceRepresentative fact, retainedStateEvidenceStrengthWitness fact]
  ]
    <> [ (alignmentChangeStoreRevision change, alignmentChangeRetainedTransition change)
       | change <- Transfer.destinationWorkChanges work
       ]

destinationDisappearanceGate ::
  AlignmentSubscriptionId ->
  Transfer.DestinationWork ->
  HeraldState ->
  (Bool, [Disappearance.MatchingWriteGate])
destinationDisappearanceGate identifier work state =
  (controlBlocked || unknownMarkerBlocked || not (null gates), gates)
  where
    leaf = startupDisappearanceState state
    evidence = destinationEvidence work
    cursor = OracleProjection.oracleViewControlIndex (OracleProjection.oracleView (startupOracleProjectionState state))
    controlBlocked =
      any
        ( \(_, retained) -> case retainedPublicationEvidenceOrigin retained of
            RoutedRetainedObservation _ _ required _ -> required > cursor
            PrimordialRetainedObservation -> False
        )
        evidence
    unknownMarkerBlocked =
      or
        [ revision > DisappearanceProtocol.disappearanceAlignmentMarkerThroughRevision marker
        | (_, marker) <- Disappearance.pendingAlignmentMarkers leaf,
          DisappearanceProtocol.disappearanceAlignmentMarkerSubscription marker == identifier,
          Disappearance.probeWitness (DisappearanceProtocol.disappearanceAlignmentMarkerProbe marker) leaf == Nothing,
          (revision, _) <- evidence
        ]
    gates =
      [ witness.witnessMatchingWriteGate
      | witness <- Disappearance.probeWitnesses leaf,
        witness.witnessPhase == Disappearance.ProbeCollectingView,
        Just cell <- [lookup identifier witness.witnessIncomingAlignmentCuts],
        Just marker <- [cell.alignmentCellMarker],
        any (matches witness marker) evidence
      ]
    matches witness marker (revision, retained) =
      revision > DisappearanceProtocol.disappearanceAlignmentMarkerThroughRevision marker
        && DisappearanceEvidence.canonicalPublicationIsSubjectRelevant
          (DisappearanceProtocol.projectedProbeSubject witness.witnessProjectedProbe)
          (retainedPublicationEvidenceSortId retained)
          (retainedPublicationEvidenceSortDefinitionOccurrenceId retained)
          (retainedPublicationEvidenceCanonicalValue retained)

-- Retry one exact head per subscription. An unchanged gate produces no work
-- disposition, so the existing alignment fixed point cannot spin on it.
redriveDeferredDestinationControls ::
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
redriveDeferredDestinationControls predecessor =
  foldM
    redrive
    (predecessor, mempty, AlignmentTransferWorkUnchanged)
    (Transfer.firstDeferredDestinationControls (Alignment.alignmentTransferState (startupAlignmentState predecessor)))
  where
    redrive (state, effects, disposition) (identifier, remote, control) =
      case applyCheckedTransferControlWithReplay True remote control state of
        Left AlignmentTransferRemoteProtocolViolation ->
          let alignment = startupAlignmentState state
              transfer = Transfer.discardDeferredDestinationControls identifier (Alignment.alignmentTransferState alignment)
              successor = replaceStartupAlignmentState (Alignment.replaceAlignmentTransferState transfer alignment) state
              rejection = case Discovery.currentPeerBinding remote (startupDiscoveryState state) of
                Nothing -> mempty
                Just binding -> orderedEffectBatch [RejectPeerConnection binding ClosePeerControlProtocol]
           in Right (successor, effects <> rejection, disposition <> AlignmentTransferWorkChanged)
        Left AlignmentTransferOwnerContradiction -> Left AlignmentTransferOwnerContradiction
        Right (successor, replies) ->
          let emitted = case Discovery.currentPeerBinding remote (startupDiscoveryState successor) of
                Nothing -> mempty
                Just binding -> orderedEffectBatch [SendPeerControl binding (PeerAlignmentControl reply) | reply <- replies]
              changed =
                Alignment.alignmentTransferState (startupAlignmentState state)
                  /= Alignment.alignmentTransferState (startupAlignmentState successor)
                  || startupDisappearanceState state /= startupDisappearanceState successor
           in Right (successor, effects <> emitted, disposition <> workWhen changed)

applyPreparedDestinationWork ::
  AlignmentSubscriptionId ->
  Transfer.DestinationSubscription ->
  Transfer.PreparedDestinationWork ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, [AlignmentControl])
applyPreparedDestinationWork identifier destination prepared predecessor = do
  let subscribe = Transfer.destinationSubscriptionSubscribe destination
      obligation = alignmentSubscribeObligation subscribe
  case Transfer.destinationSubscriptionCancellation destination of
    Just cancellation -> do
      let (successorTransfer, _) = Transfer.commitDestinationWork prepared
          alignment = startupAlignmentState predecessor
          successor =
            replaceStartupAlignmentState
              (Alignment.replaceAlignmentTransferState successorTransfer alignment)
              predecessor
      Right (successor, [AlignmentCancelled cancellation])
    Nothing -> case exactDestinationApplication destination predecessor of
      Nothing -> Left AlignmentTransferOwnerContradiction
      Just destinations -> do
        let work = Transfer.preparedDestinationWork prepared
        ownersAfterSnapshot <-
          case Transfer.destinationWorkSnapshot work of
            Nothing ->
              Right
                ( startupStoreState predecessor,
                  startupControlledState predecessor,
                  []
                )
            Just (sourceRevision, facts) ->
              foldM
                ( applyStateEvidence
                    predecessor
                    identifier
                    obligation
                    destinations
                    sourceRevision
                )
                ( startupStoreState predecessor,
                  startupControlledState predecessor,
                  []
                )
                facts
        ownersAfterChanges <-
          foldM
            ( \owners change ->
                applyPublicationEvidence
                  predecessor
                  identifier
                  obligation
                  destinations
                  (alignmentChangeStoreRevision change)
                  Transfer.AppliedContinuingChange
                  (alignmentChangeRetainedTransition change)
                  owners
            )
            ownersAfterSnapshot
            (Transfer.destinationWorkChanges work)
        let (storeAfterChanges, controlledAfterChanges, appliedEvidence) =
              ownersAfterChanges
            alignment = startupAlignmentState predecessor
            (afterWork, _) = Transfer.commitDestinationWork prepared
        preparedEvidence <-
          mapOwner
            ( Transfer.prepareAppliedDestinationEvidence
                appliedEvidence
                afterWork
            )
        let (successorTransfer, _) =
              Transfer.commitAppliedDestinationEvidence preparedEvidence
            successorAlignment =
              Alignment.replaceAlignmentTransferState successorTransfer alignment
            replies =
              maybe
                []
                (pure . AlignmentAcknowledged)
                (Transfer.destinationWorkAcknowledgement work)
        Right
          ( replaceStartupStoreState storeAfterChanges
              . replaceStartupControlledState controlledAfterChanges
              . replaceStartupAlignmentState successorAlignment
              $ predecessor,
            replies
          )

applyStateEvidence ::
  HeraldState ->
  AlignmentSubscriptionId ->
  AlignmentObligation ->
  NonEmpty DestinationStore ->
  StoreRevision ->
  (Store.State, Controlled.State, [Transfer.AppliedDestinationEvidence]) ->
  RetainedStateEvidence ->
  Either
    AlignmentTransferCoordinatorProblem
    (Store.State, Controlled.State, [Transfer.AppliedDestinationEvidence])
applyStateEvidence state identifier obligation destinations sourceRevision predecessor evidence = do
  afterRepresentative <-
    applyPublicationEvidence
      state
      identifier
      obligation
      destinations
      sourceRevision
      Transfer.AppliedSnapshotRepresentative
      (retainedStateEvidenceRepresentative evidence)
      predecessor
  applyPublicationEvidence
    state
    identifier
    obligation
    destinations
    sourceRevision
    Transfer.AppliedSnapshotStrengthWitness
    (retainedStateEvidenceStrengthWitness evidence)
    afterRepresentative

applyPublicationEvidence ::
  HeraldState ->
  AlignmentSubscriptionId ->
  AlignmentObligation ->
  NonEmpty DestinationStore ->
  StoreRevision ->
  Transfer.AppliedDestinationEvidenceKind ->
  RetainedPublicationEvidence ->
  (Store.State, Controlled.State, [Transfer.AppliedDestinationEvidence]) ->
  Either
    AlignmentTransferCoordinatorProblem
    (Store.State, Controlled.State, [Transfer.AppliedDestinationEvidence])
applyPublicationEvidence state identifier obligation destinations sourceRevision evidenceKind evidence (predecessorStore, predecessorControlled, appliedEvidence) = do
  (descriptor, checked) <- checkedEvidencePublication obligation evidence state
  (successorControlled, currentSuppressed) <-
    applyControlledEvidence descriptor evidence checked predecessorControlled
  let strength =
        min
          (retainedPublicationEvidenceSourceStrength evidence)
          (alignmentObligationFrozenContextPathStrength obligation)
      retainedEvidence disposition =
        fmap
          ( \destination ->
              Transfer.appliedDestinationEvidence
                identifier
                destination
                evidence
                strength
                sourceRevision
                evidenceKind
                disposition
          )
          (NonEmpty.toList destinations)
  if currentSuppressed
    then
      Right
        ( predecessorStore,
          successorControlled,
          appliedEvidence <> retainedEvidence Transfer.ControlledSuppressed
        )
    else do
      observation <- checkedStoreObservation strength checked evidence state
      prepared <-
        mapOwner
          ( Store.prepareAlignmentStoreApplication
              (fmap (storeDestination strength) destinations)
              observation
              predecessorStore
          )
      let (successorStore, outcomes) = Store.commitPeerStoreApplication prepared
          dispositions =
            fmap Store.peerStoreOutcomeDisposition (NonEmpty.toList outcomes)
      if all (/= Store.PeerStoreTerminallyIgnored) dispositions
        then
          Right
            ( successorStore,
              successorControlled,
              appliedEvidence
                <> retainedEvidence Transfer.StoreAppliedOrAlreadyApplied
            )
        else
          if all (== Store.PeerStoreTerminallyIgnored) dispositions
            then
              -- Controlled admission and Store application form one semantic
              -- observation. If every exact Store destination rejects the
              -- stale epoch, retain only the terminal transfer evidence: a
              -- newly observed controlled carrier must not become live by
              -- itself.
              Right
                ( successorStore,
                  predecessorControlled,
                  appliedEvidence
                    <> retainedEvidence Transfer.StoreTerminallyIgnored
                )
            else Left AlignmentTransferOwnerContradiction
applyControlledEvidence ::
  CanonicalDescriptor ->
  RetainedPublicationEvidence ->
  CheckedPublication ->
  Controlled.State ->
  Either AlignmentTransferCoordinatorProblem (Controlled.State, Bool)
applyControlledEvidence descriptor evidence checked predecessor =
  case Controlled.checkControlledObservation
    descriptor
    (retainedPublicationEvidenceSortDefinitionOccurrenceId evidence)
    checked of
    Left Controlled.ControlledObservationNotControlled ->
      Right (predecessor, False)
    Left _ -> Left AlignmentTransferOwnerContradiction
    Right observation -> do
      prepared <-
        mapRemote
          (Controlled.prepareControlledPeerObservation observation predecessor)
      let (successor, result) = Controlled.commitControlledPeerObservation prepared
      Right
        ( successor,
          result == Controlled.ControlledPeerCurrentSuppressed
        )

checkedEvidencePublication ::
  AlignmentObligation ->
  RetainedPublicationEvidence ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (CanonicalDescriptor, CheckedPublication)
checkedEvidencePublication obligation evidence state = do
  requireRemote
    ( retainedPublicationEvidenceSortId evidence
        == alignmentObligationSortId obligation
        && retainedPublicationEvidenceSortDefinitionOccurrenceId evidence
          == alignmentObligationSortDefinitionOccurrenceId obligation
    )
  entry <- exactEvidenceRegistryEntry evidence (startupSortRegistryState state)
  value <-
    mapRemote
      ( decodeCanonicalValue
          (canonicalValueByteString (retainedPublicationEvidenceCanonicalValue evidence))
      )
  checked <-
    mapRemote
      ( mkCheckedPublication
          (SortRegistry.registryEntryDescriptor entry)
          (retainedPublicationEvidencePublicationId evidence)
          value
      )
  requireRemote
    (checkedPublicationCanonicalValue checked == retainedPublicationEvidenceCanonicalValue evidence)
  validateEvidenceOrigin
    (SortRegistry.registryEntryDescriptor entry)
    checked
    evidence
    state
  Right (SortRegistry.registryEntryDescriptor entry, checked)

-- Alignment evidence is immutable source history.  Its outer sort occurrence
-- therefore remains authentic after regular retirement removes that entry
-- from the effective projection, and after an equal redefinition installs a
-- successor occurrence under the same public SortId.  Only an exact retained
-- retirement entry grants that historical meaning; an unknown occurrence is
-- still a remote protocol violation.
exactEvidenceRegistryEntry ::
  RetainedPublicationEvidence ->
  SortRegistry.State ->
  Either AlignmentTransferCoordinatorProblem SortRegistry.RegistryEntry
exactEvidenceRegistryEntry evidence registry =
  case SortRegistry.lookupEffectiveSort sortId registry of
    Just entry
      | SortRegistry.registryEntryOccurrenceId entry == occurrence ->
          Right entry
    _ ->
      maybe
        (Left AlignmentTransferRemoteProtocolViolation)
        Right
        (SortRegistry.lookupRegularSortRetirement sortId occurrence registry >>= SortRegistry.regularSortRetirementEntry)
  where
    sortId = retainedPublicationEvidenceSortId evidence
    occurrence = retainedPublicationEvidenceSortDefinitionOccurrenceId evidence

validateEvidenceOrigin ::
  CanonicalDescriptor ->
  CheckedPublication ->
  RetainedPublicationEvidence ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem ()
validateEvidenceOrigin descriptor checked evidence state =
  case retainedPublicationEvidenceOrigin evidence of
    PrimordialRetainedObservation ->
      requireRemote
        ( (retainedPublicationEvidenceSourceStrength evidence == Normal && checked `elem` Store.primordialSeedPublications (startupStoreState state))
            || checked `elem` canonicalEnvironment
        )
    RoutedRetainedObservation source topology control stamp -> do
      mapPeerInputAuthority
        ( PeerInput.validateRetainedStorePublication
            ( PeerInput.routedPublicationAuthorityClaim
                descriptor
                (retainedPublicationEvidenceSortDefinitionOccurrenceId evidence)
                checked
                source
                (retainedPublicationEvidenceSourceStrength evidence)
                topology
                control
                stamp
            )
        )
  where
    canonicalEnvironment = [publication | bootstrap <- bootstraps, (_, _, publication) <- appliedProcessEnvironmentPublications (checkedSystemId (startupGenesis state)) bootstraps bootstrap]
    bootstraps = OracleProjection.oracleViewAppliedBootstraps (OracleProjection.oracleView (startupOracleProjectionState state))

checkedStoreObservation ::
  ReplicaStrength ->
  CheckedPublication ->
  RetainedPublicationEvidence ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem Store.RetainedStoreObservation
checkedStoreObservation strength checked evidence _ =
  mapRemote
    ( Store.retainedStoreObservation
        checked
        strength
        occurrence
        origin
    )
  where
    occurrence = retainedPublicationEvidenceSortDefinitionOccurrenceId evidence
    origin = case retainedPublicationEvidenceOrigin evidence of
      PrimordialRetainedObservation -> Store.PrimordialStoreObservation
      RoutedRetainedObservation source topology control stamp ->
        Store.RoutedStoreObservation
          (Store.storeObservationEnvelope source occurrence topology control stamp)

storeDestination :: ReplicaStrength -> DestinationStore -> Store.PeerStoreDestination
storeDestination strength destination =
  Store.peerStoreDestination
    (destinationStoreDelta destination)
    (destinationStoreIncarnation destination)
    strength

exactDestinationApplication ::
  Transfer.DestinationSubscription ->
  HeraldState ->
  Maybe (NonEmpty DestinationStore)
exactDestinationApplication destination state
  | all (destinationStoreIsActiveExact obligation state) supplied =
      Just destinations
  | otherwise = Nothing
  where
    subscribe = Transfer.destinationSubscriptionSubscribe destination
    obligation = alignmentSubscribeObligation subscribe
    destinations = alignmentObligationDestinationStores obligation
    supplied = NonEmpty.toList destinations

destinationStoreIsActiveExact ::
  AlignmentObligation ->
  HeraldState ->
  DestinationStore ->
  Bool
destinationStoreIsActiveExact obligation state destination =
  activeAlignmentDestination
    state
    ( sortOccurrence
        (alignmentObligationSortId obligation)
        (alignmentObligationSortDefinitionOccurrenceId obligation)
    )
    destination

activeAlignmentDestination ::
  HeraldState ->
  SortOccurrence ->
  DestinationStore ->
  Bool
activeAlignmentDestination state affectedSort destination =
  case Store.lookupStoreSlot (destinationStoreDelta destination) (startupStoreState state) of
    Nothing -> False
    Just slot -> exactSlot destination slot
  where
    exactSlot expected slot =
      Store.storeSlotDelta slot == destinationStoreDelta expected
        && Store.storeSlotIncarnation slot == destinationStoreIncarnation expected
        && Store.storeSlotSortId slot == sortOccurrenceSortId affectedSort
        && Store.storeSlotOccurrenceId slot
          == sortOccurrenceDefinition affectedSort

invalidateDestinationAttempt ::
  AlignmentCancel ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, Bool)
invalidateDestinationAttempt cancellation predecessor =
  case (ordinaryAttempt, bootstrapAttempt) of
    (Just attempt, Nothing) -> do
      prepared <-
        mapOwner
          ( Alignment.prepareAlignmentAttemptInvalidation
              attempt
              reason
              alignment
          )
      let disposition = Alignment.preparedAlignmentAttemptInvalidationDisposition prepared
          (successorAlignment, _) = Alignment.commitAlignmentAttemptInvalidation prepared
      Right
        ( replaceStartupAlignmentState successorAlignment predecessor,
          disposition == Alignment.AlignmentAttemptInvalidated
        )
    (Nothing, Just attempt)
      | reason == AlignmentSourceIncarnationLost,
        Alignment.BootstrapFromFreshBase fresh <-
          Alignment.bootstrapImportKeySource
            ( Alignment.bootstrapImportKeyValue
                (Alignment.bootstrapImportAttemptImport attempt)
            ),
        let key =
              Alignment.bootstrapImportKeyValue
                (Alignment.bootstrapImportAttemptImport attempt),
        activeBootstrapAttempt || bootstrapPlanAlreadyInvalidated key -> do
          let subscribe = Alignment.bootstrapImportAttemptSubscribe attempt
              source =
                Alignment.alignmentSourceCoordinate
                  (Alignment.bootstrapImportAttemptSourceHerald attempt)
                  (alignmentSubscribeSourceStoreIncarnation subscribe)
          (afterPlanInvalidation, _, planWork) <-
            invalidateFreshBasePlan
              (Alignment.bootstrapImportAttemptImport attempt)
              fresh
              source
              predecessor
          prepared <-
            mapOwner
              ( Alignment.prepareBootstrapImportAttemptInvalidation
                  attempt
                  reason
                  (startupAlignmentState afterPlanInvalidation)
              )
          let disposition =
                Alignment.preparedBootstrapImportAttemptInvalidationDisposition prepared
              (successorAlignment, _) =
                Alignment.commitBootstrapImportAttemptInvalidation prepared
          Right
            ( replaceStartupAlignmentState successorAlignment afterPlanInvalidation,
              planWork == AlignmentTransferWorkChanged
                || disposition == Alignment.BootstrapImportAttemptInvalidated
            )
      | otherwise -> do
          prepared <-
            mapOwner
              ( Alignment.prepareBootstrapImportAttemptInvalidation
                  attempt
                  reason
                  alignment
              )
          let disposition =
                Alignment.preparedBootstrapImportAttemptInvalidationDisposition prepared
              (successorAlignment, _) =
                Alignment.commitBootstrapImportAttemptInvalidation prepared
          Right
            ( replaceStartupAlignmentState successorAlignment predecessor,
              disposition == Alignment.BootstrapImportAttemptInvalidated
            )
    (Nothing, Nothing) -> do
      prepared <-
        mapOwner
          (Transfer.prepareAlignmentCancellation cancellation transfer)
      let (successorTransfer, disposition) = Transfer.commitAlignmentCancellation prepared
      Right
        ( replaceStartupAlignmentState
            (Alignment.replaceAlignmentTransferState successorTransfer alignment)
            predecessor,
          disposition /= Transfer.AlignmentTransferUnchanged
        )
    (Just _, Just _) -> Left AlignmentTransferOwnerContradiction
  where
    identifier = alignmentCancelSubscriptionId cancellation
    reason = alignmentCancelReason cancellation
    alignment = startupAlignmentState predecessor
    transfer = Alignment.alignmentTransferState alignment
    ordinaryAttempt = findAttemptBySubscription identifier alignment
    bootstrapAttempt = findBootstrapAttemptBySubscription identifier alignment
    activeBootstrapAttempt =
      any
        ( (== identifier)
            . alignmentSubscribeSubscriptionId
            . Alignment.bootstrapImportAttemptSubscribe
            . snd
        )
        (Alignment.bootstrapImportAttemptEntries alignment)
    bootstrapPlanAlreadyInvalidated key =
      maybe
        False
        (`generationPlanInvalidated` alignment)
        (Alignment.lookupAlignmentGeneration (Alignment.bootstrapImportKeyGeneration key) alignment)

invalidateFreshBasePlan ::
  Alignment.BootstrapImport ->
  FreshMemberBaseEvidence ->
  Alignment.AlignmentSourceCoordinate ->
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, [AlignmentCancel], AlignmentTransferWorkDisposition)
invalidateFreshBasePlan bootstrapImport fresh source predecessor =
  foldM retainCause (predecessor, Map.empty, mempty) causes
    >>= \(successor, cancellations, work) ->
      Right (successor, Map.elems cancellations, work)
  where
    key = Alignment.bootstrapImportKeyValue bootstrapImport
    generation = Alignment.bootstrapImportKeyGeneration key
    destination = Alignment.bootstrapImportKeyDestination key
    obligation = Alignment.bootstrapImportSubscribeEnvelope bootstrapImport
    causes =
      [ Alignment.AlignmentPlanDestinationStoreLost generation destination
      | not (destinationStoreIsActiveExact obligation predecessor destination)
      ]
        <> [Alignment.AlignmentPlanFreshBaseStoreLost generation fresh source]

    retainCause (state, retained, work) cause = do
      (successor, cancellations, causeWork) <- invalidateAlignmentPlan cause state
      Right
        ( successor,
          foldl
            ( \current cancellation ->
                Map.insert
                  (alignmentCancelSubscriptionId cancellation)
                  cancellation
                  current
            )
            retained
            cancellations,
          work <> causeWork
        )

applySourceAcknowledgement ::
  HeraldEpoch ->
  AlignmentAck ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, [AlignmentControl])
applySourceAcknowledgement remote acknowledgement predecessor = do
  let identifier = alignmentAckSubscriptionId acknowledgement
      alignment = startupAlignmentState predecessor
      transfer = Alignment.alignmentTransferState alignment
  _ <- requireSourceSubscription identifier transfer
  requireRemote (alignmentSubscriptionIdDestinationHerald identifier == remote)
  prepared <- mapRemote (Transfer.prepareSourceAcknowledgement acknowledgement transfer)
  let (successorTransfer, _) = Transfer.commitSourceAcknowledgement prepared
  Right
    ( replaceStartupAlignmentState
        (Alignment.replaceAlignmentTransferState successorTransfer alignment)
        predecessor,
      []
    )

applyCancellation ::
  HeraldEpoch ->
  Eclips.Herald.Alignment.Protocol.AlignmentCancel ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, [AlignmentControl])
applyCancellation remote cancellation predecessor = do
  let identifier = alignmentCancelSubscriptionId cancellation
      alignment = startupAlignmentState predecessor
      transfer = Alignment.alignmentTransferState alignment
      sourceRemote =
        alignmentSubscriptionIdDestinationHerald identifier == remote
          && any ((== identifier) . fst) (Transfer.sourceSubscriptionEntries transfer)
      destinationRemote =
        any
          ( \(retained, destination) ->
              retained == identifier && destinationSourceHerald destination == Just remote
          )
          (Transfer.destinationSubscriptionEntries transfer)
      reason = alignmentCancelReason cancellation
      local = checkedLocalHeraldEpoch (startupGenesis predecessor)
      loopback = remote == local && sourceRemote && destinationRemote
  requireRemote
    ( loopback
        || (destinationRemote && reason == AlignmentSourceIncarnationLost)
        || (sourceRemote && reason /= AlignmentSourceIncarnationLost)
    )
  if destinationRemote && reason == AlignmentSourceIncarnationLost
    then do
      (successor, _) <- invalidateDestinationAttempt cancellation predecessor
      Right (successor, [])
    else do
      prepared <- mapRemote (Transfer.prepareAlignmentCancellation cancellation transfer)
      let (successorTransfer, _) = Transfer.commitAlignmentCancellation prepared
      Right
        ( replaceStartupAlignmentState
            (Alignment.replaceAlignmentTransferState successorTransfer alignment)
            predecessor,
          []
        )

-- | Harvest each single-consumer journal before selection. Synchronous local
-- delivery may change a later key in this same frozen stage inventory.
wakeTransferWork :: HeraldState -> HeraldState
wakeTransferWork predecessor
  | Set.null destinations && Set.null stores = state
  | otherwise =
      replaceStartupAlignmentState
        (Alignment.wakeMemberStoreChanges stores (Alignment.wakeDestinationProgress destinations retainedAlignment))
        (if Set.null stores then state else replaceStartupStoreState retainedStores state)
  where
    local = checkedLocalHeraldEpoch (startupGenesis predecessor)
    previousAlignment = startupAlignmentState predecessor
    state
      | Alignment.transferMemberWorkHerald previousAlignment == Just local = predecessor
      | otherwise = replaceStartupAlignmentState (Alignment.initializeTransferMemberWork local previousAlignment) predecessor
    alignment = startupAlignmentState state
    (destinations, transfer) = Transfer.takeDestinationProgressChanges (Alignment.alignmentTransferState alignment)
    (stores, retainedStores) = Store.takeAlignmentMemberStoreChanges (startupStoreState state)
    retainedAlignment = if Set.null destinations then alignment else Alignment.replaceAlignmentTransferState transfer alignment

consumeTransferWork :: (Ord key) => Alignment.TransferWorkStage key -> key -> HeraldState -> HeraldState
consumeTransferWork stage key state =
  replaceStartupAlignmentState
    (Alignment.consumeTransferWork stage key (startupAlignmentState state))
    state

driveTransferStage ::
  (Ord key, Monoid effects) =>
  TransferTraversal ->
  Alignment.TransferWorkStage key ->
  (Alignment.State -> [key]) ->
  ((HeraldState, effects, AlignmentTransferWorkDisposition) -> key -> Either AlignmentTransferCoordinatorProblem (HeraldState, effects, AlignmentTransferWorkDisposition)) ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, effects, AlignmentTransferWorkDisposition)
driveTransferStage traversal stage allKeys action predecessor =
  let state = wakeTransferWork predecessor
      alignment = startupAlignmentState state
      inventory = case traversal of
        SelectiveTransferTraversal -> Alignment.transferWorkIds stage alignment
        ExhaustiveTransferTraversal -> Set.fromList (allKeys alignment)
   in driveTransferInventory traversal stage inventory action (state, mempty, mempty)

driveTransferInventory ::
  (Ord key) =>
  TransferTraversal ->
  Alignment.TransferWorkStage key ->
  Set key ->
  ((HeraldState, effects, AlignmentTransferWorkDisposition) -> key -> Either AlignmentTransferCoordinatorProblem (HeraldState, effects, AlignmentTransferWorkDisposition)) ->
  (HeraldState, effects, AlignmentTransferWorkDisposition) ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, effects, AlignmentTransferWorkDisposition)
driveTransferInventory traversal stage inventory action = go Nothing
  where
    go cursor (predecessor, effects, work) =
      let state = wakeTransferWork predecessor
          alignment = startupAlignmentState state
          selected = case traversal of
            SelectiveTransferTraversal -> Alignment.nextTransferWork stage inventory cursor alignment
            ExhaustiveTransferTraversal -> maybe (Set.lookupMin inventory) (`Set.lookupGT` inventory) cursor
       in case selected of
            Nothing -> Right (state, effects, work)
            Just key -> do
              successor <- action (consumeTransferWork stage key state, effects, work) key
              go (Just key) successor

-- | Original stage boundary, with an independent full-inventory test oracle.
advanceBootstrapAttempts :: HeraldState -> Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceBootstrapAttempts = advanceBootstrapAttemptsWith SelectiveTransferTraversal

advanceBootstrapAttemptsExhaustive :: HeraldState -> Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceBootstrapAttemptsExhaustive = advanceBootstrapAttemptsWith ExhaustiveTransferTraversal

advanceBootstrapAttemptsWith ::
  TransferTraversal ->
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceBootstrapAttemptsWith traversal predecessor =
  driveTransferStage traversal Alignment.BootstrapAttemptStage (fmap fst . Alignment.bootstrapImportEntries) advanceOne predecessor
  where
    advanceOne (state, effects, work) key
      | not (isJust (Alignment.lookupBootstrapImport key (startupAlignmentState state))) =
          Right (state, effects, work)
      | not
          ( acceptanceRetained
              (Alignment.bootstrapImportKeyGeneration key)
              (checkedLocalHeraldEpoch (startupGenesis state))
              (startupAlignmentState state)
          ) =
          Right (state, effects, work)
      | otherwise =
          case Alignment.prepareBootstrapImportAttempt key (startupAlignmentState state) of
            Left (Alignment.BootstrapImportSourceEvidenceUnavailable _) ->
              Right (state, effects, work)
            Left (Alignment.BootstrapImportFreshBaseSourceFailed retainedKey source) ->
              case Alignment.bootstrapImportKeySource retainedKey of
                Alignment.BootstrapFromFreshBase fresh -> do
                  bootstrapImport <-
                    maybe
                      (Left AlignmentTransferOwnerContradiction)
                      Right
                      (Alignment.lookupBootstrapImport retainedKey (startupAlignmentState state))
                  (invalidated, cancellations, invalidationWork) <-
                    invalidateFreshBasePlan bootstrapImport fresh source state
                  foldM
                    deliverPlanCancellation
                    (invalidated, effects, work <> invalidationWork)
                    cancellations
                Alignment.BootstrapFromPredecessor _ ->
                  Left AlignmentTransferOwnerContradiction
            Left _ -> Left AlignmentTransferOwnerContradiction
            Right prepared -> do
              let subscribe = Alignment.preparedBootstrapImportSubscribe prepared
                  attempt = Alignment.preparedBootstrapImportAttempt prepared
                  source = Alignment.bootstrapImportAttemptSourceHerald attempt
                  disposition = Alignment.preparedBootstrapImportDisposition prepared
                  (alignment, _) = Alignment.commitBootstrapImportAttempt prepared
                  retained = replaceStartupAlignmentState alignment state
              if disposition == Alignment.BootstrapImportAttemptUnchanged
                then Right (retained, effects, work)
                else do
                  (successor, delivery) <- deliverControls source [AlignmentSubscribeRequested subscribe] retained
                  Right
                    ( successor,
                      effects <> delivery,
                      work <> AlignmentTransferWorkChanged
                    )

    deliverPlanCancellation (state, effects, work) cancellation = do
      let identifier = alignmentCancelSubscriptionId cancellation
          transfer =
            Alignment.alignmentTransferState (startupAlignmentState state)
      destination <- requireDestinationSubscription identifier transfer
      remote <-
        maybe
          (Left AlignmentTransferOwnerContradiction)
          Right
          (destinationSourceHerald destination)
      (successor, delivery) <-
        deliverControls remote [AlignmentCancelled cancellation] state
      Right (successor, effects <> delivery, work)

-- | Original stage boundary, with an independent full-inventory test oracle.
advanceOrdinaryAttempts :: HeraldState -> Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceOrdinaryAttempts = advanceOrdinaryAttemptsWith SelectiveTransferTraversal

advanceOrdinaryAttemptsExhaustive :: HeraldState -> Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceOrdinaryAttemptsExhaustive = advanceOrdinaryAttemptsWith ExhaustiveTransferTraversal

advanceOrdinaryAttemptsWith ::
  TransferTraversal ->
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceOrdinaryAttemptsWith traversal predecessor =
  driveTransferStage traversal Alignment.OrdinaryAttemptStage (fmap fst . Alignment.activeAlignmentObligationEntries) advanceOne predecessor
  where
    advanceOne (state, effects, work) identifier = do
      obligation <-
        maybe
          (Left AlignmentTransferOwnerContradiction)
          Right
          (Alignment.lookupAlignmentObligation identifier (startupAlignmentState state))
      if not
        ( acceptanceRetained
            (alignmentObligationDestinationGeneration obligation)
            (checkedLocalHeraldEpoch (startupGenesis state))
            (startupAlignmentState state)
        )
        then Right (state, effects, work)
        else case Alignment.prepareAlignmentAttempt identifier (startupAlignmentState state) of
          Left (Alignment.AlignmentAttemptSourceEvidenceUnavailable _) ->
            Right (state, effects, work)
          Left _ -> Left AlignmentTransferOwnerContradiction
          Right prepared -> do
            let attempt = Alignment.preparedAlignmentAttempt prepared
                disposition = Alignment.preparedAlignmentAttemptDisposition prepared
                (withAttempt, _) = Alignment.commitAlignmentAttempt prepared
                transfer = Alignment.alignmentTransferState withAttempt
            preparedTransfer <- mapOwner (Transfer.prepareDestinationAttempt attempt transfer)
            let subscribe = Transfer.preparedDestinationAttemptSubscribe preparedTransfer
                (successorTransfer, transferDisposition) =
                  Transfer.commitDestinationAttempt preparedTransfer
                alignment = Alignment.replaceAlignmentTransferState successorTransfer withAttempt
                retained = replaceStartupAlignmentState alignment state
                newWork =
                  disposition == Alignment.AlignmentAttemptCreated
                    || transferDisposition /= Transfer.AlignmentTransferUnchanged
            if not newWork
              then Right (retained, effects, work)
              else do
                (successor, delivery) <-
                  deliverControls
                    (alignmentAttemptSourceHerald attempt)
                    [AlignmentSubscribeRequested subscribe]
                    retained
                Right
                  ( successor,
                    effects <> delivery,
                    work <> AlignmentTransferWorkChanged
                  )

-- | Preserve the source stage's entry inventory and ascending cursor while
-- visiting only sources whose Store prefix or lifetime may have changed.
advanceSourcePrefixes ::
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceSourcePrefixes predecessor =
  let woken = wakeSourcePrefixWork predecessor
      transfer = Alignment.alignmentTransferState (startupAlignmentState woken)
   in if Set.null (Transfer.pendingSourcePrefixIds transfer)
        then Right (woken, mempty, mempty)
        else go (Transfer.sourceSubscriptionIds transfer) Nothing (woken, mempty, mempty)
  where
    go inventory cursor (state, effects, work) =
      let woken = wakeSourcePrefixWork state
          transfer = Alignment.alignmentTransferState (startupAlignmentState woken)
       in case Transfer.nextSourcePrefixWork inventory cursor transfer of
            Nothing -> Right (woken, effects, work)
            Just identifier -> do
              successor <- advanceSourcePrefix (consumeSourcePrefixWork identifier woken, effects, work) identifier
              go inventory (Just identifier) successor

-- | Independent full-inventory traversal retained as the scheduling oracle.
-- This intentionally does not select through the dirty-work index. Consuming
-- the same notification bookkeeping makes complete owner states comparable.
advanceSourcePrefixesExhaustive ::
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceSourcePrefixesExhaustive predecessor = do
  result <- foldM advanceOne (wakeSourcePrefixWork predecessor, mempty, mempty) identifiers
  let (successor, effects, work) = result
  Right (wakeSourcePrefixWork successor, effects, work)
  where
    identifiers = fmap fst (Transfer.sourceSubscriptionEntries (Alignment.alignmentTransferState (startupAlignmentState predecessor)))
    advanceOne (state, effects, work) identifier =
      advanceSourcePrefix
        (consumeSourcePrefixWork identifier (wakeSourcePrefixWork state), effects, work)
        identifier

wakeSourcePrefixWork :: HeraldState -> HeraldState
wakeSourcePrefixWork predecessor
  | Set.null changed = predecessor
  | otherwise =
      replaceStartupStoreState (Store.clearSourceStoreChanges store)
        . replaceStartupAlignmentState
          (Alignment.replaceAlignmentTransferState (Transfer.wakeSourcePrefixes changed transfer) alignment)
        $ predecessor
  where
    store = startupStoreState predecessor
    changed = Store.changedSourceStoreIncarnations store
    alignment = startupAlignmentState predecessor
    transfer = Alignment.alignmentTransferState alignment

consumeSourcePrefixWork :: AlignmentSubscriptionId -> HeraldState -> HeraldState
consumeSourcePrefixWork identifier predecessor =
  let alignment = startupAlignmentState predecessor
      transfer = Alignment.alignmentTransferState alignment
   in replaceStartupAlignmentState
        (Alignment.replaceAlignmentTransferState (Transfer.consumeSourcePrefixWork identifier transfer) alignment)
        predecessor

advanceSourcePrefix ::
  (HeraldState, EffectBatch, AlignmentTransferWorkDisposition) ->
  AlignmentSubscriptionId ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceSourcePrefix (state, effects, work) identifier = do
  let alignment = startupAlignmentState state
      transfer = Alignment.alignmentTransferState alignment
  source <- requireSourceSubscription identifier transfer
  case Transfer.sourceSubscriptionCancellation source of
    Just _ -> Right (state, effects, work)
    Nothing -> do
      let incarnation =
            alignmentSubscribeSourceStoreIncarnation
              (Transfer.sourceSubscriptionSubscribe source)
      case Store.lookupRetainedStoreSlot incarnation (startupStoreState state) of
        Nothing -> do
          (cancelled, controls, disposition) <- cancelSource identifier state
          deliverAndCombine identifier controls cancelled effects (work <> disposition)
        Just slot -> do
          changes <-
            mapOwner
              ( Store.retainedStoreChangesAfter
                  incarnation
                  (Transfer.sourceSubscriptionSentThrough source)
                  (startupStoreState state)
              )
          effective <- projectSourceChanges state changes
          projected <- traverse (storeChangeControl identifier) effective
          let live = alignmentLive identifier (Store.storeSlotRevision slot)
          prepared <-
            mapOwner
              (Transfer.prepareSourceChanges identifier projected live transfer)
          let controls = Transfer.preparedSourceChangeControls prepared
              (successorTransfer, disposition) = Transfer.commitSourceChanges prepared
              successor =
                replaceStartupAlignmentState
                  (Alignment.replaceAlignmentTransferState successorTransfer alignment)
                  state
          if disposition == Transfer.AlignmentTransferUnchanged
            then Right (successor, effects, work)
            else
              deliverAndCombine
                identifier
                controls
                successor
                effects
                (work <> transferWorkDisposition disposition)
  where
    deliverAndCombine subscription controls current accumulated disposition = do
      let remote = alignmentSubscriptionIdDestinationHerald subscription
      (successor, delivery) <- deliverControls remote controls current
      Right (successor, accumulated <> delivery, disposition)

cancelSource ::
  AlignmentSubscriptionId ->
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, [AlignmentControl], AlignmentTransferWorkDisposition)
cancelSource identifier predecessor = do
  let cancellation = alignmentCancel identifier AlignmentSourceIncarnationLost
      alignment = startupAlignmentState predecessor
  prepared <-
    mapOwner
      ( Transfer.prepareAlignmentCancellation
          cancellation
          (Alignment.alignmentTransferState alignment)
      )
  let (transfer, disposition) = Transfer.commitAlignmentCancellation prepared
      controls =
        if disposition == Transfer.AlignmentTransferUnchanged
          then []
          else [AlignmentCancelled cancellation]
  Right
    ( replaceStartupAlignmentState
        (Alignment.replaceAlignmentTransferState transfer alignment)
        predecessor,
      controls,
      transferWorkDisposition disposition
    )

-- | Reconcile exact physical loss before an eager generation-planning entry
-- point can replace the latest frontier. This is the same ordered loss phase
-- as the transfer driver, including its retained cancellation delivery.
reconcileAlignmentLosses ::
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch)
reconcileAlignmentLosses predecessor = do
  (successor, effects, _) <- cancelLostDestinations predecessor
  Right (successor, effects)

cancelLostDestinations ::
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
cancelLostDestinations = cancelLostDestinationsWith SelectiveTransferTraversal

cancelLostDestinationsWith :: TransferTraversal -> HeraldState -> Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
cancelLostDestinationsWith traversal predecessor =
  if traversal == SelectiveTransferTraversal && not pending
    then Right (drained, mempty, mempty)
    else cancelLostDestinationsExhaustiveBody drained
  where
    (storeChanged, stores) = Store.takeAlignmentLossChange (startupStoreState predecessor)
    (placementChanged, placement) = PlacementState.takeAlignmentLossChange (startupPlacementState predecessor)
    physical =
      (if storeChanged then replaceStartupStoreState stores else id)
        ((if placementChanged then replaceStartupPlacementState placement else id) predecessor)
    owner = startupAlignmentState physical
    (pending, consumed) =
      Alignment.consumeLossCheck
        (if storeChanged || placementChanged then Alignment.wakeLossCheck owner else owner)
    -- These replacements only consume notification metadata. In particular,
    -- clearing Placement's journal does not alter any prepared cut-query input.
    -- Restore before the real loss body, whose mutations keep normal invalidation.
    cache = startupAlignmentCutQueryCache predecessor
    consumedState = if pending then replaceStartupAlignmentState consumed physical else physical
    drained
      | storeChanged || placementChanged || pending = cache `seq` replaceStartupAlignmentCutQueryCache cache consumedState
      | otherwise = consumedState

-- The atomic cause discovery, invalidation and normalized cancellation body is
-- shared unchanged. Only selection of that whole operation becomes conditional.
cancelLostDestinationsExhaustiveBody :: HeraldState -> Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
cancelLostDestinationsExhaustiveBody predecessor = do
  let (observedAlignment, observedWork) =
        Set.foldl'
          retainUnavailable
          (startupAlignmentState predecessor, mempty)
          (alignmentSnapshotWithdrawnSources local liveSnapshot (startupAlignmentState predecessor))
      observed = replaceStartupAlignmentState observedAlignment predecessor
      causes =
        Set.toAscList
          (alignmentPlanLossCauses local (activeAlignmentDestination observed) observedAlignment)
  (invalidated, cancellations, work) <-
    foldM invalidateOne (observed, Map.empty, observedWork) causes
  foldM deliverCancellation (invalidated, mempty, work) (Map.elems cancellations)
  where
    local = checkedLocalHeraldEpoch (startupGenesis predecessor)
    liveSnapshot remote = do
      revision <- PlacementState.remotePlacementSequence remote (startupPlacementState predecessor)
      pure (revision, PlacementState.remotePlacementRoutes remote (startupPlacementState predecessor))
    retainUnavailable (alignment, work) source =
      let prepared = Alignment.prepareUnavailableAlignmentSourceRetention source alignment
          (successor, disposition) = Alignment.commitUnavailableAlignmentSourceRetention prepared
       in ( successor,
            work <> case disposition of
              Alignment.AlignmentSourceRetainedUnavailable -> AlignmentTransferWorkChanged
              Alignment.AlignmentSourceRetentionUnchanged -> mempty
          )

    invalidateOne (state, retainedCancellations, work) cause = do
      (successor, newlyCancelled, invalidationWork) <-
        invalidateAlignmentPlan cause state
      Right
        ( successor,
          foldl
            ( \retained cancellation ->
                Map.insert
                  (alignmentCancelSubscriptionId cancellation)
                  cancellation
                  retained
            )
            retainedCancellations
            newlyCancelled,
          work <> invalidationWork
        )

    deliverCancellation (state, effects, work) cancellation = do
      let identifier = alignmentCancelSubscriptionId cancellation
          transfer =
            Alignment.alignmentTransferState (startupAlignmentState state)
      destination <- requireDestinationSubscription identifier transfer
      remote <-
        maybe
          (Left AlignmentTransferOwnerContradiction)
          Right
          (destinationSourceHerald destination)
      (successor, delivery) <-
        deliverControls remote [AlignmentCancelled cancellation] state
      Right (successor, effects <> delivery, work)

-- | A first live placement snapshot can already reflect a withdrawal which
-- predates this owner's passive frontier adoption. Compare only latest foreign
-- members against a snapshot at or beyond their frozen owner revision. Missing
-- or older snapshots prove no loss; an exact delta/incarnation pair remains
-- available even if unrelated routes changed. The caller retains these facts
-- through the ordinary unavailable-source owner before plan invalidation.
alignmentSnapshotWithdrawnSources ::
  HeraldEpoch ->
  (HeraldEpoch -> Maybe (Placement.PlacementSequence, [Placement.DeltaRoute])) ->
  Alignment.State ->
  Set Alignment.UnavailableAlignmentSource
alignmentSnapshotWithdrawnSources local liveSnapshot alignment =
  Set.fromList
    [ Alignment.unavailableAlignmentSource remote delta incarnation
    | (_, generation) <- latest,
      let cut = alignmentGenerationCut generation,
      member <- NonEmpty.toList (alignmentCutExactMembers cut),
      let remote = alignmentMemberHerald member,
      remote /= local,
      Just frozen <- [lookup remote (NonEmpty.toList (physicalPlacementRevisionEntries (currentPlacement generation)))],
      Just (revision, routes) <- [Map.lookup remote snapshots],
      revision >= frozen,
      let delta = alignmentMemberDelta member,
      let incarnation = alignmentMemberStoreIncarnation member,
      Set.notMember (delta, incarnation) routes
    ]
  where
    currentPlacement generation = maybe (alignmentCutPhysicalPlacementRevisionVector (alignmentGenerationCut generation)) (Plan.alignmentPlanIdPlacement . Plan.alignmentPlanId) (Alignment.currentAlignmentPlanForSort (sortOccurrence (alignmentCutSortId (alignmentGenerationCut generation)) (alignmentCutSortDefinitionOccurrenceId (alignmentGenerationCut generation))) alignment)
    latest = Alignment.latestAlignmentGenerationEntries alignment
    remoteMembers =
      Set.fromList
        [ alignmentMemberHerald member
        | (_, generation) <- latest,
          member <- NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)),
          alignmentMemberHerald member /= local
        ]
    snapshots =
      Map.fromList
        [ (remote, (revision, Set.fromList [(Placement.deltaRouteDelta route, Placement.deltaRouteStoreIncarnation route) | route <- routes]))
        | remote <- Set.toAscList remoteMembers,
          Just (revision, routes) <- [liveSnapshot remote]
        ]

-- | Discover every immutable plan coordinate invalidated by the current exact
-- local Store projection.  The callback is deliberately the only Store seam:
-- tests may supply a finite coordinate set while production performs the
-- checked active-slot lookup.
alignmentPlanLossCauses ::
  HeraldEpoch ->
  (SortOccurrence -> DestinationStore -> Bool) ->
  Alignment.State ->
  Set Alignment.AlignmentPlanInvalidationCause
alignmentPlanLossCauses local destinationIsActive alignment =
  Set.fromList
    ( ordinaryDestinationLosses
        <> bootstrapDestinationLosses
        <> currentGenerationDestinationLosses
        <> bootstrapFreshBaseSourceLosses
    )
  where
    ordinaryDestinationLosses =
      [ Alignment.AlignmentPlanDestinationStoreLost generation lostDestination
      | (_, obligation) <- Alignment.activeAlignmentObligationEntries alignment,
        let generation = alignmentObligationDestinationGeneration obligation,
        let affectedSort =
              sortOccurrence
                (alignmentObligationSortId obligation)
                (alignmentObligationSortDefinitionOccurrenceId obligation),
        lostDestination <- NonEmpty.toList (alignmentObligationDestinationStores obligation),
        not (destinationIsActive affectedSort lostDestination)
      ]

    bootstrapDestinationLosses =
      [ Alignment.AlignmentPlanDestinationStoreLost generation lostDestination
      | (_, bootstrapImport) <- Alignment.bootstrapImportEntries alignment,
        let key = Alignment.bootstrapImportKeyValue bootstrapImport,
        let generation = Alignment.bootstrapImportKeyGeneration key,
        let lostDestination = Alignment.bootstrapImportKeyDestination key,
        let obligation = Alignment.bootstrapImportSubscribeEnvelope bootstrapImport,
        let affectedSort =
              sortOccurrence
                (alignmentObligationSortId obligation)
                (alignmentObligationSortDefinitionOccurrenceId obligation),
        not (destinationIsActive affectedSort lostDestination)
      ]

    -- The latest generation set still owns its physical members after one-time
    -- imports finish. Retained exact loss also applies to foreign members and
    -- to a historical anchor admitted after its placement loss was observed.
    -- Superseded generations disappear from this set and remain historical.
    currentGenerationDestinationLosses =
      [ Alignment.AlignmentPlanDestinationStoreLost
          (alignmentGenerationId generation)
          (destinationFor member)
      | (_, generation) <- Alignment.latestAlignmentGenerationEntries alignment,
        let affectedSort = generationSort generation,
        member <- NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)),
        ( alignmentMemberHerald member == local
            && not (destinationIsActive affectedSort (destinationFor member))
        )
          || Set.member
            ( Alignment.unavailableAlignmentSource
                (alignmentMemberHerald member)
                (alignmentMemberDelta member)
                (alignmentMemberStoreIncarnation member)
            )
            (Alignment.alignmentUnavailableSources alignment)
      ]

    bootstrapFreshBaseSourceLosses =
      [ Alignment.AlignmentPlanFreshBaseStoreLost generation fresh source
      | (_, bootstrapImport) <- Alignment.bootstrapImportEntries alignment,
        let key = Alignment.bootstrapImportKeyValue bootstrapImport,
        let generation = Alignment.bootstrapImportKeyGeneration key,
        Alignment.BootstrapFromFreshBase fresh <- [Alignment.bootstrapImportKeySource key],
        Just retainedGeneration <-
          [Alignment.lookupAlignmentGeneration generation alignment],
        let affectedSort = generationSort retainedGeneration,
        sourceMember <-
          NonEmpty.toList
            (alignmentCutExactMembers (alignmentGenerationCut retainedGeneration)),
        alignmentMemberDelta sourceMember == freshMemberBaseDelta fresh,
        alignmentMemberStoreIncarnation sourceMember == freshMemberBaseStoreIncarnation fresh,
        let sourceHerald = alignmentMemberHerald sourceMember,
        sourceHerald == local,
        not (destinationIsActive affectedSort (destinationFor sourceMember)),
        let source =
              Alignment.alignmentSourceCoordinate
                sourceHerald
                (freshMemberBaseStoreIncarnation fresh)
      ]

    generationSort generation =
      let cut = alignmentGenerationCut generation
       in sortOccurrence
            (alignmentCutSortId cut)
            (alignmentCutSortDefinitionOccurrenceId cut)

invalidateAlignmentPlan ::
  Alignment.AlignmentPlanInvalidationCause ->
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, [AlignmentCancel], AlignmentTransferWorkDisposition)
invalidateAlignmentPlan cause predecessor = do
  prepared <-
    mapOwner
      ( Alignment.prepareAlignmentPlanInvalidation
          cause
          (startupAlignmentState predecessor)
      )
  let cancellations =
        Alignment.preparedAlignmentPlanInvalidationCancellations prepared
      receipt = Alignment.preparedAlignmentPlanInvalidationReceipt prepared
      retiredOwners =
        Set.fromList
          (fmap fst (Alignment.alignmentPlanInvalidationReceiptObligations receipt))
      (afterPlanInvalidation, planDisposition) =
        Alignment.commitAlignmentPlanInvalidation prepared
  preparedBarrierRemoval <-
    mapOwner
      ( Transfer.prepareIncompletePredecessorInputClosureRemoval
          retiredOwners
          (Alignment.alignmentTransferState afterPlanInvalidation)
      )
  let (successorTransfer, barrierDisposition) =
        Transfer.commitIncompletePredecessorInputClosureRemoval preparedBarrierRemoval
      successorAlignment =
        Alignment.replaceAlignmentTransferState
          successorTransfer
          afterPlanInvalidation
  Right
    ( replaceStartupAlignmentState successorAlignment predecessor,
      cancellations,
      workWhen
        ( planDisposition == Alignment.AlignmentPlanInvalidated
            || barrierDisposition /= Transfer.AlignmentTransferUnchanged
        )
    )

-- | Original stage boundary, with an independent full-inventory test oracle.
advanceBootstrapFulfillments :: HeraldState -> Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceBootstrapFulfillments = advanceBootstrapFulfillmentsWith SelectiveTransferTraversal

advanceBootstrapFulfillmentsExhaustive :: HeraldState -> Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceBootstrapFulfillmentsExhaustive = advanceBootstrapFulfillmentsWith ExhaustiveTransferTraversal

advanceBootstrapFulfillmentsWith ::
  TransferTraversal ->
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceBootstrapFulfillmentsWith traversal predecessor =
  driveTransferStage traversal Alignment.BootstrapFulfillmentStage (fmap fst . Alignment.bootstrapImportAttemptEntries) advanceOne predecessor
  where
    advanceOne (state, effects, work) key = do
      attempt <-
        maybe
          (Left AlignmentTransferOwnerContradiction)
          Right
          ( Alignment.lookupBootstrapImportAttempt
              key
              (startupAlignmentState state)
          )
      let identifier =
            alignmentSubscribeSubscriptionId
              (Alignment.bootstrapImportAttemptSubscribe attempt)
          transfer =
            Alignment.alignmentTransferState (startupAlignmentState state)
      destination <- requireDestinationSubscription identifier transfer
      case ( Transfer.destinationSubscriptionAppliedThrough destination,
             Transfer.destinationSubscriptionLiveThrough destination,
             Transfer.destinationSubscriptionAcknowledgedThrough destination,
             Transfer.destinationSubscriptionCancellation destination
           ) of
        (Just applied, Just live, Just acknowledged, Nothing)
          | applied == live,
            acknowledged == live -> do
              prepared <-
                mapOwner
                  ( Alignment.prepareBootstrapImportFulfillment
                      attempt
                      live
                      (startupAlignmentState state)
                  )
              let cancellations =
                    Alignment.preparedBootstrapImportFulfillmentCancellations prepared
                  (successorAlignment, disposition) =
                    Alignment.commitBootstrapImportFulfillment prepared
                  fulfilled =
                    replaceStartupAlignmentState successorAlignment state
              foldM
                deliverFulfillmentCancellation
                ( fulfilled,
                  effects,
                  work
                    <> workWhen
                      ( disposition
                          == Alignment.BootstrapImportFulfilled
                      )
                )
                cancellations
        _ -> Right (state, effects, work)

    deliverFulfillmentCancellation (state, effects, work) cancellation = do
      let identifier = alignmentCancelSubscriptionId cancellation
          transfer =
            Alignment.alignmentTransferState (startupAlignmentState state)
      destination <- requireDestinationSubscription identifier transfer
      remote <-
        maybe
          (Left AlignmentTransferOwnerContradiction)
          Right
          (destinationSourceHerald destination)
      (successor, delivery) <-
        deliverControls remote [AlignmentCancelled cancellation] state
      Right (successor, effects <> delivery, work)

-- | Original stage boundary, with an independent full-inventory test oracle.
advanceReadinessAndCertificates :: HeraldState -> Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceReadinessAndCertificates = advanceReadinessAndCertificatesWith SelectiveTransferTraversal

advanceReadinessAndCertificatesExhaustive :: HeraldState -> Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceReadinessAndCertificatesExhaustive = advanceReadinessAndCertificatesWith ExhaustiveTransferTraversal

advanceReadinessAndCertificatesWith ::
  TransferTraversal ->
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceReadinessAndCertificatesWith traversal predecessor = do
  (withReady, readyControls, readyWork) <-
    runMembers (Alignment.MemberReadinessStage local) readyOne (initial, [], mempty)
  (withCertificates, certificateControls, certificateWork) <-
    runMembers (Alignment.MemberCertificateStage local) certificateOne (withReady, [], mempty)
  Right
    ( withCertificates,
      alignmentEvidenceEffects
        (readyControls <> certificateControls)
        withCertificates,
      readyWork <> certificateWork
    )
  where
    local = checkedLocalHeraldEpoch (startupGenesis predecessor)
    initial = wakeTransferWork predecessor
    -- Both phases see the same entry membership; local-ready effects all precede
    -- certificates. The full-scan oracle keeps the original immutable payloads.
    inventory = Alignment.pendingTransferMemberIds local (startupAlignmentState initial)
    localMembers = Alignment.pendingAlignmentMemberProgress local (startupAlignmentState initial)
    runMembers stage action beginning = case traversal of
      SelectiveTransferTraversal -> driveTransferInventory traversal stage inventory selected beginning
        where
          selected accumulated@(state, _, _) key = do
            member <-
              maybe
                (Left AlignmentTransferOwnerContradiction)
                Right
                (Alignment.lookupPendingAlignmentMember local key (startupAlignmentState state))
            action accumulated member
      ExhaustiveTransferTraversal -> do
        (successor, controls, work) <- foldM runOne beginning localMembers
        Right (wakeTransferWork successor, controls, work)
        where
          runOne (state, controls, work) member@(generation, retained) =
            let key = (alignmentGenerationId generation, alignmentMemberDelta retained)
             in action (consumeTransferWork stage key (wakeTransferWork state), controls, work) member

    readyOne (state, controls, work) (generation, member) = do
      let generationId = alignmentGenerationId generation
          store = alignmentMemberStoreIncarnation member
          alreadyReady =
            case Alignment.lookupAlignmentLocalMemberReadiness
              generationId
              store
              (startupAlignmentState state) of
              Just _ -> True
              Nothing -> False
      if alreadyReady
        then Right (state, controls, work)
        else do
          evidence <- memberReadinessEvidence traversal generation member state
          case evidence of
            Nothing -> Right (state, controls, work)
            Just (prefixDigest, revision, destination, prefixes) -> do
              let predecessorAlignment = startupAlignmentState state
              preparedFulfillment <-
                mapOwner
                  ( Transfer.prepareLocalMemberReadinessFulfillment
                      generationId
                      destination
                      revision
                      prefixes
                      prefixDigest
                      (Alignment.alignmentTransferState predecessorAlignment)
                  )
              let (transfer, fulfillmentDisposition) =
                    Transfer.commitLocalMemberReadinessFulfillment preparedFulfillment
                  withFulfillment =
                    Alignment.replaceAlignmentTransferState transfer predecessorAlignment
              prepared <-
                mapOwner
                  ( Alignment.prepareLocalClassMemberReady
                      generationId
                      (alignmentMemberStoreIncarnation member)
                      prefixDigest
                      revision
                      withFulfillment
                  )
              let disposition = Alignment.preparedLocalClassMemberReadyDisposition prepared
                  ready = Alignment.preparedLocalClassMemberReady prepared
                  (alignment, _) = Alignment.commitLocalClassMemberReady prepared
              Right
                ( replaceStartupAlignmentState alignment state,
                  controls
                    <> [AlignmentMemberReadyAdvertised ready | disposition == Alignment.AlignmentReadinessRetained],
                  work
                    <> transferWorkDisposition fulfillmentDisposition
                    <> workWhen
                      (disposition == Alignment.AlignmentReadinessRetained)
                )

    certificateOne (state, controls, work) (generation, member) = do
      let alignment = startupAlignmentState state
          generationId = alignmentGenerationId generation
          store = alignmentMemberStoreIncarnation member
          members =
            Set.fromList
              ( fmap
                  alignmentMemberStoreIncarnation
                  (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)))
              )
          ready =
            [ evidence
            | retainedStore <- Set.toAscList members,
              Just evidence <-
                [ Alignment.lookupAlignmentMemberReadiness
                    generationId
                    retainedStore
                    alignment
                ]
            ]
      case findCertificate generationId store alignment of
        Just _ -> Right (state, controls, work)
        Nothing
          | Set.fromList (fmap classMemberReadyStoreIncarnation ready) /= members ->
              Right (state, controls, work)
          | otherwise -> do
              ownReady <-
                maybe
                  (Left AlignmentTransferOwnerContradiction)
                  Right
                  ( find
                      ( (== store)
                          . classMemberReadyStoreIncarnation
                      )
                      ready
                  )
              let certificate =
                    historicalCertificate
                      generationId
                      store
                      (classMemberReadySetDigest ready)
                      ( deriveBootstrapEvidenceDigest
                          (memberReadyEvidenceDigestBytes (classMemberReadyPredecessorAndBasePrefixDigest ownReady))
                      )
                      (classMemberReadyStoreRevision ownReady)
              prepared <- mapOwner (Alignment.prepareHistoricalCertificate certificate alignment)
              let (successorAlignment, disposition) = Alignment.commitHistoricalCertificate prepared
              Right
                ( replaceStartupAlignmentState successorAlignment state,
                  controls
                    <> [ AlignmentHistoricalCertificateAdvertised certificate
                       | disposition == Alignment.AlignmentReadinessRetained
                       ],
                  work
                    <> workWhen
                      (disposition == Alignment.AlignmentReadinessRetained)
                )

memberReadinessEvidence ::
  TransferTraversal ->
  AlignmentGeneration ->
  Eclips.Domain.Alignment.AlignmentMember ->
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    ( Maybe
        ( Eclips.Domain.Alignment.MemberReadyEvidenceDigest,
          StoreRevision,
          DestinationStore,
          [Transfer.MemberReadinessPrefix]
        )
    )
memberReadinessEvidence traversal generation member state =
  case Store.lookupRetainedStoreSlot
    (alignmentMemberStoreIncarnation member)
    (startupStoreState state) of
    Just retained
      | Store.storeSlotDelta retained == alignmentMemberDelta member ->
          memberReadinessEvidenceAt traversal generation member retained state
    _ -> Right Nothing

memberReadinessEvidenceAt ::
  TransferTraversal ->
  AlignmentGeneration ->
  AlignmentMember ->
  Store.StoreSlot ->
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    ( Maybe
        ( MemberReadyEvidenceDigest,
          StoreRevision,
          DestinationStore,
          [Transfer.MemberReadinessPrefix]
        )
    )
memberReadinessEvidenceAt traversal generation member slot state = do
  let alignment = startupAlignmentState state
      transfer = Alignment.alignmentTransferState alignment
      generationId = alignmentGenerationId generation
      destination = destinationFor member
      activeBootstrapImports = case traversal of
        SelectiveTransferTraversal -> Alignment.bootstrapImportsAtMember generationId destination alignment
        ExhaustiveTransferTraversal ->
          [ bootstrapImport
          | (_, bootstrapImport) <- Alignment.bootstrapImportEntries alignment,
            let key = Alignment.bootstrapImportKeyValue bootstrapImport,
            Alignment.bootstrapImportKeyGeneration key == generationId,
            Alignment.bootstrapImportKeyDestination key == destination
          ]
      bootstrapFulfillments = case traversal of
        SelectiveTransferTraversal -> Alignment.bootstrapFulfillmentsAtMember generationId destination alignment
        ExhaustiveTransferTraversal ->
          [ fulfillment
          | (key, fulfillment) <- Alignment.bootstrapImportFulfillmentEntries alignment,
            Alignment.bootstrapImportKeyGeneration key == generationId,
            Alignment.bootstrapImportKeyDestination key == destination
          ]
      ordinaryAttempts = case traversal of
        SelectiveTransferTraversal -> Alignment.alignmentAttemptsAtMember generationId destination alignment
        ExhaustiveTransferTraversal ->
          [ attempt
          | (_, attempt) <- Alignment.alignmentAttemptEntries alignment,
            let obligation = alignmentAttemptObligation attempt,
            alignmentObligationDestinationGeneration obligation == generationId,
            destination `elem` NonEmpty.toList (alignmentObligationDestinationStores obligation)
          ]
      requiredOrdinaryCount = case traversal of
        SelectiveTransferTraversal -> Alignment.activeAlignmentObligationCountAtMember generationId destination alignment
        ExhaustiveTransferTraversal ->
          length
            [ ()
            | (_, obligation) <- Alignment.activeAlignmentObligationEntries alignment,
              alignmentObligationDestinationGeneration obligation == generationId,
              destination `elem` NonEmpty.toList (alignmentObligationDestinationStores obligation)
            ]
  if not (null activeBootstrapImports)
    || length ordinaryAttempts /= requiredOrdinaryCount
    then Right Nothing
    else do
      ordinarySubscriptions <- traverse attemptSubscribe ordinaryAttempts
      let bootstrapCompletions =
            fmap (Just . fulfilledBootstrapPrefix) bootstrapFulfillments
      ordinaryCompletions <-
        traverse (completedDestinationPrefix transfer) ordinarySubscriptions
      case sequence (bootstrapCompletions <> ordinaryCompletions) of
        Nothing -> Right Nothing
        Just prefixes ->
          Right
            ( Just
                ( deriveMemberReadyEvidenceDigest
                    (memberReadyPrefixBytes generationId destination (Store.storeSlotRevision slot) prefixes),
                  Store.storeSlotRevision slot,
                  destination,
                  prefixes
                )
            )

fulfilledBootstrapPrefix ::
  Alignment.BootstrapImportFulfillmentReceipt -> Transfer.MemberReadinessPrefix
fulfilledBootstrapPrefix fulfillment =
  Transfer.memberReadinessPrefix identifier through through through
  where
    attempt = Alignment.bootstrapImportFulfillmentAttempt fulfillment
    identifier =
      alignmentSubscribeSubscriptionId
        (Alignment.bootstrapImportAttemptSubscribe attempt)
    through = Alignment.bootstrapImportFulfillmentThrough fulfillment

completedDestinationPrefix ::
  Transfer.State ->
  AlignmentSubscribe ->
  Either AlignmentTransferCoordinatorProblem (Maybe Transfer.MemberReadinessPrefix)
completedDestinationPrefix transfer subscribe = do
  destination <- requireDestinationSubscription (alignmentSubscribeSubscriptionId subscribe) transfer
  case ( Transfer.destinationSubscriptionAppliedThrough destination,
         Transfer.destinationSubscriptionLiveThrough destination,
         Transfer.destinationSubscriptionAcknowledgedThrough destination,
         Transfer.destinationSubscriptionCancellation destination
       ) of
    (Just applied, Just live, Just acknowledged, Nothing)
      | destinationHasCoveredLive destination ->
          Right
            ( Just
                ( Transfer.memberReadinessPrefix
                    (alignmentSubscribeSubscriptionId subscribe)
                    applied
                    live
                    acknowledged
                )
            )
    _ -> Right Nothing

memberReadyPrefixBytes ::
  ContextClassGenerationId ->
  DestinationStore ->
  StoreRevision ->
  [Transfer.MemberReadinessPrefix] ->
  ByteString.ByteString
memberReadyPrefixBytes generation destination revision supplied = Serialize.runPut $ do
  putSizedBytes "ECLIPS-ALIGNMENT-MEMBER-COMPLETE-PREFIX-V1"
  Serialize.putByteString (contextClassGenerationIdBytes generation)
  Serialize.putByteString (storeIncarnationIdBytes (destinationStoreIncarnation destination))
  Serialize.putWord64be (storeRevisionWord64 revision)
  let prefixes = sortOn Transfer.memberReadinessPrefixSubscription supplied
  Serialize.putWord64be (fromIntegral (length prefixes))
  mapM_ putCompletedPrefix prefixes

putCompletedPrefix :: Transfer.MemberReadinessPrefix -> Serialize.Put
putCompletedPrefix prefix = do
  Serialize.putByteString
    ( heraldEpochBytes
        ( alignmentSubscriptionIdDestinationHerald
            (Transfer.memberReadinessPrefixSubscription prefix)
        )
    )
  Serialize.putWord64be
    ( alignmentSubscriptionSequenceWord64
        ( alignmentSubscriptionIdSequence
            (Transfer.memberReadinessPrefixSubscription prefix)
        )
    )
  Serialize.putWord64be
    (storeRevisionWord64 (Transfer.memberReadinessPrefixApplied prefix))
  Serialize.putWord64be
    (storeRevisionWord64 (Transfer.memberReadinessPrefixLive prefix))
  Serialize.putWord64be
    (storeRevisionWord64 (Transfer.memberReadinessPrefixAcknowledged prefix))

advanceRouteCutovers ::
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceRouteCutovers predecessor = do
  (successor, effects, work) <- advanceRouteCutoversRetaining predecessor
  let local = checkedLocalHeraldEpoch (startupGenesis successor)
      (alignment, retiredCandidates) =
        Alignment.retireCompletedAlignmentRouteCutovers local (startupAlignmentState successor)
  Right
    ( replaceStartupAlignmentState alignment successor,
      effects,
      work <> workWhen retiredCandidates
    )

advanceRouteCutoversRetaining ::
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advanceRouteCutoversRetaining predecessor = do
  let retainedTransfer = Alignment.alignmentTransferState (startupAlignmentState predecessor)
      pending candidate
        | candidate.source == candidate.predecessor =
            not (Transfer.localRoutePrefixSettled candidate.generation candidate.predecessor retainedTransfer)
        | otherwise =
            case Transfer.lookupRouteCutover candidate.generation candidate.source candidate.predecessor retainedTransfer of
              Nothing -> True
              Just _ -> False
      candidates =
        routeCutoverCandidatesFrom
          ( Alignment.pendingAlignmentRouteCutoverEntries
              (checkedLocalHeraldEpoch (startupGenesis predecessor))
              (startupAlignmentState predecessor)
          )
          pending
          predecessor
  (withTransfer, requests, retainedWork) <-
    foldM retainCandidate (predecessor, Map.empty, mempty) candidates
  case Map.mapMaybe NonEmpty.nonEmpty requests of
    empty | Map.null empty -> Right (withTransfer, mempty, retainedWork)
    nonEmptyMarkers -> do
      let nonEmpty =
            fmap
              (fmap (peerLogicalPayloadItem . PeerLogicalRouteCutover))
              nonEmptyMarkers
      prepared <-
        mapOwner
          ( PeerStream.prepareEnqueue
              nonEmpty
              (startupPeerStreamState withTransfer)
          )
      assigned <-
        concat
          <$> traverse
            (assignedMarkers prepared)
            (Map.toAscList nonEmptyMarkers)
      let alignment = startupAlignmentState withTransfer
      preparedAssignments <-
        mapOwner
          ( Transfer.prepareRouteCutoverAssignments
              assigned
              (Alignment.alignmentTransferState alignment)
          )
      let (transfer, assignmentDisposition) =
            Transfer.commitRouteCutoverAssignments preparedAssignments
          (peerStream, _) = PeerStream.commitEnqueue prepared
          successor =
            replaceStartupPeerStreamState peerStream
              . replaceStartupAlignmentState
                (Alignment.replaceAlignmentTransferState transfer alignment)
              $ withTransfer
          frontierEffects =
            [ SendPeerControl binding (PeerStreamFrontierAdvanced direction nextSequence)
            | (peer, (direction, nextSequence)) <-
                Map.toAscList (PeerStream.preparedEnqueueFrontierAdvances prepared),
              Just binding <- [Discovery.currentPeerBinding peer (startupDiscoveryState successor)]
            ]
          ticketEffects =
            fmap SchedulePeerDispatch (PeerStream.preparedEnqueueDispatchTickets prepared)
      Right
        ( successor,
          orderedEffectBatch (frontierEffects <> ticketEffects),
          retainedWork <> transferWorkDisposition assignmentDisposition
        )
  where
    assignedMarkers prepared (peer, markers) =
      case Map.lookup peer (PeerStream.preparedEnqueueAssignments prepared) of
        Just items
          | NonEmpty.length items == NonEmpty.length markers ->
              Right
                ( zipWith
                    (\marker item -> (marker, peerLogicalAssignmentReceipt item))
                    (NonEmpty.toList markers)
                    (NonEmpty.toList items)
                )
        _ -> Left AlignmentTransferOwnerContradiction

    retainCandidate (state, requests, work) candidate = do
      let alignment = startupAlignmentState state
          transfer = Alignment.alignmentTransferState alignment
      if candidate.source == candidate.predecessor
        then do
          prepared <-
            mapOwner
              ( Transfer.prepareLocalRoutePrefixSettlement
                  candidate.generation
                  candidate.predecessor
                  transfer
              )
          let (successorTransfer, disposition) =
                Transfer.commitLocalRoutePrefixSettlement prepared
          Right
            ( replaceStartupAlignmentState
                (Alignment.replaceAlignmentTransferState successorTransfer alignment)
                state,
              requests,
              work <> transferWorkDisposition disposition
            )
        else do
          marker <-
            mapRemote
              ( alignmentRouteCutoverMarker
                  candidate.plan
                  candidate.generation
                  candidate.source
                  candidate.predecessor
              )
          prepared <- mapOwner (Transfer.prepareRouteCutoverMarker marker transfer)
          let (successorTransfer, disposition) = Transfer.commitRouteCutoverMarker prepared
              successor =
                replaceStartupAlignmentState
                  (Alignment.replaceAlignmentTransferState successorTransfer alignment)
                  state
              updated =
                if disposition == Transfer.AlignmentTransferUnchanged
                  then requests
                  else
                    Map.insertWith
                      (<>)
                      candidate.predecessor
                      [marker]
                      requests
          Right
            ( successor,
              updated,
              work <> transferWorkDisposition disposition
            )

data RouteCutoverCandidate = RouteCutoverCandidate
  { plan :: Plan.AlignmentPlanId,
    generation :: ContextClassGenerationId,
    source :: HeraldEpoch,
    predecessor :: HeraldEpoch
  }

routeCutoverCandidates :: HeraldState -> [RouteCutoverCandidate]
routeCutoverCandidates = routeCutoverCandidatesMatching (const True)

-- Runtime production skips an already retained marker or local settlement
-- before revisiting its immutable publication prefix. The exhaustive composed
-- audit above still enumerates every eligible candidate, including history.
routeCutoverCandidatesMatching ::
  (RouteCutoverCandidate -> Bool) -> HeraldState -> [RouteCutoverCandidate]
routeCutoverCandidatesMatching include state =
  routeCutoverCandidatesFrom (Alignment.alignmentGenerationEntries (startupAlignmentState state)) include state

routeCutoverCandidatesFrom ::
  [(ContextClassGenerationId, AlignmentGeneration)] ->
  (RouteCutoverCandidate -> Bool) ->
  HeraldState ->
  [RouteCutoverCandidate]
routeCutoverCandidatesFrom entries include state =
  [ candidate
  | (generationId, generation) <- entries,
    Set.member local active,
    local `elem` fixedSources generation,
    acceptanceRetained generationId local alignment,
    predecessorId <- alignmentCutPredecessorGenerationIds (alignmentGenerationCut generation),
    Just predecessorGeneration <- [Alignment.lookupAlignmentGeneration predecessorId alignment],
    predecessorHerald <-
      Set.toAscList
        ( Set.fromList
            ( fmap
                alignmentMemberHerald
                (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut predecessorGeneration)))
            )
        ),
    let candidate = RouteCutoverCandidate (Alignment.generationBirthPlanId generation) generationId local predecessorHerald,
    include candidate,
    not (generationPlanInvalidated generation alignment),
    Set.member predecessorHerald active,
    acceptanceRetained generationId predecessorHerald alignment,
    publicationPrefixSettled
      generationId
      local
      predecessorHerald
      (startupPublicationState state)
      alignment
  ]
  where
    alignment = startupAlignmentState state
    local = checkedLocalHeraldEpoch (startupGenesis state)
    active = currentActiveHeralds state
    fixedSources generation =
      fmap
        fst
        ( NonEmpty.toList
            ( physicalPlacementRevisionEntries
                ( alignmentCutPhysicalPlacementRevisionVector
                    (alignmentGenerationCut generation)
                )
            )
        )

generationPlanInvalidated :: AlignmentGeneration -> Alignment.State -> Bool
generationPlanInvalidated = Alignment.generationCurrentPlanInvalidated

publicationPrefixSettled ::
  ContextClassGenerationId ->
  HeraldEpoch ->
  HeraldEpoch ->
  Publication.State ->
  Alignment.State ->
  Bool
publicationPrefixSettled generation local predecessor publication alignment =
  case Alignment.lookupAlignmentCutAcceptance generation local alignment of
    Nothing -> False
    Just accepted -> case Eclips.Herald.Alignment.Protocol.alignmentCutAcceptedPublicationPrefix accepted of
      EmptyHeraldPublicationPrefix -> True
      HeraldPublicationPrefixThrough through ->
        all
          (settledApplication through)
          (Publication.applicationPublicationEntries publication)
          && Publication.environmentRootPrefixSettledFor
            through
            predecessor
            publication
  where
    settledApplication through (_, record)
      | Publication.applicationPublicationHeraldPosition record > through = True
      | not (routeNames predecessor (Publication.applicationPublicationRoute record)) = True
      | otherwise =
          maybe False (const True) (Publication.applicationPublicationOutgoingRecord record)
            || any
              ( (== Just record)
                  . Publication.stampedStructuralApplicationRecordMaybe
                  . snd
              )
              (Publication.stampedStructuralStageEntries publication)
    routeNames herald =
      any ((== herald) . destinationHerald) . routeDestinations

advancePredecessorInputClosures ::
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advancePredecessorInputClosures predecessor =
  let woken = wakePredecessorInputClosureWork predecessor
      transfer = Alignment.alignmentTransferState (startupAlignmentState woken)
   in if Set.null (Transfer.pendingInputClosureSourceIds transfer)
        then Right (woken, mempty, mempty)
        else go (Transfer.inputClosureSourceIds transfer) Nothing (woken, mempty, mempty)
  where
    go inventory cursor (state, effects, work) =
      let woken = wakePredecessorInputClosureWork state
          transfer = Alignment.alignmentTransferState (startupAlignmentState woken)
       in case Transfer.nextInputClosureWork inventory cursor transfer of
            Nothing -> Right (woken, effects, work)
            Just identifier -> do
              successor <- advancePredecessorInputClosure IndexedInputClosureQueries (consumePredecessorInputClosureWork identifier woken, effects, work) identifier
              go inventory (Just identifier) successor

-- | The original full source, owner, destination and barrier projections form
-- an independent oracle. Only notification bookkeeping is shared with the sparse
-- stage, never any index used to select a source or decide its closure predicate.
advancePredecessorInputClosuresExhaustive ::
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advancePredecessorInputClosuresExhaustive predecessor = do
  (successor, effects, work) <- foldM advanceOne (wakePredecessorInputClosureWork predecessor, mempty, mempty) identifiers
  Right (wakePredecessorInputClosureWork successor, effects, work)
  where
    identifiers = Transfer.pendingPredecessorInputClosureSourceIds (Alignment.alignmentTransferState (startupAlignmentState predecessor))
    advanceOne (state, effects, work) identifier =
      advancePredecessorInputClosure ExhaustiveInputClosureQueries (consumePredecessorInputClosureWork identifier (wakePredecessorInputClosureWork state), effects, work) identifier

wakePredecessorInputClosureWork :: HeraldState -> HeraldState
wakePredecessorInputClosureWork predecessor
  | Set.null changes.changedTargets,
    Set.null changes.invalidatedGenerations,
    Transfer.inputClosureMembership transfer == Just membership =
      predecessor
  | otherwise =
      replaceStartupAlignmentState
        (Alignment.replaceAlignmentTransferState woken consumed)
        predecessor
  where
    alignment = startupAlignmentState predecessor
    transfer = Alignment.alignmentTransferState alignment
    (changes, consumed) = Alignment.takeInputClosureChanges alignment
    membership = OracleProjection.oracleViewCurrentHeraldMembershipId (OracleProjection.oracleView (startupOracleProjectionState predecessor))
    woken =
      Transfer.wakeInputClosureTargets changes.changedTargets
        . Transfer.wakeInputClosureGenerations changes.invalidatedGenerations
        . Transfer.synchronizeInputClosureMembership membership
        $ transfer

consumePredecessorInputClosureWork :: AlignmentSubscriptionId -> HeraldState -> HeraldState
consumePredecessorInputClosureWork identifier predecessor =
  let alignment = startupAlignmentState predecessor
      transfer = Alignment.alignmentTransferState alignment
   in replaceStartupAlignmentState
        (Alignment.replaceAlignmentTransferState (Transfer.consumeInputClosureWork identifier transfer) alignment)
        predecessor

data InputClosureQueryMode = IndexedInputClosureQueries | ExhaustiveInputClosureQueries

advancePredecessorInputClosure ::
  InputClosureQueryMode ->
  (HeraldState, EffectBatch, AlignmentTransferWorkDisposition) ->
  AlignmentSubscriptionId ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
advancePredecessorInputClosure queries (state, effects, work) identifier = do
  let alignment = startupAlignmentState state
      transfer = Alignment.alignmentTransferState alignment
      local = checkedLocalHeraldEpoch (startupGenesis state)
      active = currentActiveHeralds state
  source <- requireSourceSubscription identifier transfer
  let subscribe = Transfer.sourceSubscriptionSubscribe source
      obligation = alignmentSubscribeObligation subscribe
      sourceStore = alignmentSubscribeSourceStoreIncarnation subscribe
      predecessorGenerationId = alignmentObligationSourceGeneration obligation
  case ( Transfer.sourceSubscriptionCancellation source,
         Transfer.sourceSubscriptionLiveReleased source,
         alignmentSubscribeSourceProof subscribe
       ) of
    (Nothing, False, BootstrapProof generationId _)
      | predecessorGenerationId /= generationId,
        Just _ <- Transfer.lookupPredecessorInputClosureReceipt generationId sourceStore transfer -> do
          generation <- requireGeneration generationId alignment
          if generationPlanInvalidated generation alignment
            then Right (state, effects, work)
            else do
              -- A later destination reuses the exact immutable source closure.
              -- Its own snapshot/change stream must still reach that receipt's
              -- lower bound before Live can be released. Continuing carried
              -- inputs may have advanced since the receipt was first sealed.
              (afterPrefix, prefixControls, prefixWork) <-
                appendCurrentSourceChangesWithDisposition identifier [] state
              let remote = alignmentSubscriptionIdDestinationHerald identifier
              (withPrefix, prefixEffects) <- deliverControls remote prefixControls afterPrefix
              let withPrefixAlignment = startupAlignmentState withPrefix
              preparedRelease <- mapOwner (Transfer.prepareSourceLiveRelease generationId sourceStore identifier (Alignment.alignmentTransferState withPrefixAlignment))
              let releaseControls = Transfer.preparedSourceLiveReleaseControls preparedRelease
                  (releasedTransfer, disposition) = Transfer.commitSourceLiveRelease preparedRelease
                  released = replaceStartupAlignmentState (Alignment.replaceAlignmentTransferState releasedTransfer withPrefixAlignment) withPrefix
              (successor, releaseEffects) <- deliverControls remote releaseControls released
              Right (successor, effects <> prefixEffects <> releaseEffects, work <> prefixWork <> transferWorkDisposition disposition)
    (Nothing, False, BootstrapProof generationId _)
      | predecessorGenerationId /= generationId -> do
          generation <- requireGeneration generationId alignment
          let incoming =
                case queries of
                  IndexedInputClosureQueries -> Transfer.predecessorInputClosureCandidateEntries (predecessorGenerationId, sourceStore) transfer
                  ExhaustiveInputClosureQueries -> predecessorInputClosureCandidates predecessorGenerationId sourceStore transfer
              expectedOwners =
                case queries of
                  IndexedInputClosureQueries -> Alignment.requiredPredecessorInputOwnersAt (predecessorGenerationId, sourceStore) alignment
                  ExhaustiveInputClosureQueries -> requiredPredecessorInputOwners predecessorGenerationId sourceStore alignment
              incomingOwners =
                Set.fromList
                  [ alignmentSubscribeObligationId
                      (Transfer.destinationSubscriptionSubscribe destination)
                  | (_, destination) <- incoming
                  ]
                  <> Set.intersection
                    expectedOwners
                    ( case queries of
                        IndexedInputClosureQueries -> Transfer.completedInputClosureOwners generationId transfer
                        ExhaustiveInputClosureQueries -> completedInputClosureOwners generationId transfer
                    )
          -- Source admission proves local cut acceptance and predecessor
          -- Store ownership. Marker admission proves source acceptance;
          -- these immutable facts need no repeated history scan here.
          if not
            ( not (generationPlanInvalidated generation alignment)
                && incomingOwners == expectedOwners
                && Transfer.localRoutePrefixSettled generationId local transfer
                && routeCutoverSetComplete
                  active
                  generationId
                  local
                  (alignmentGenerationCut generation)
                  transfer
            )
            then Right (state, effects, work)
            else do
              (afterBarriers, barrierEffects, witnessed, barrierWork) <-
                ensurePredecessorInputClosures
                  queries
                  generationId
                  expectedOwners
                  state
              case witnessed of
                Nothing ->
                  Right
                    ( afterBarriers,
                      effects <> barrierEffects,
                      work <> barrierWork
                    )
                Just prefixes -> do
                  (afterPrefix, prefixControls, prefixWork) <-
                    appendCurrentSourceChangesWithDisposition
                      identifier
                      []
                      afterBarriers
                  let prefixRemote = alignmentSubscriptionIdDestinationHerald identifier
                  (withPrefixDelivery, prefixEffects) <-
                    deliverControls prefixRemote prefixControls afterPrefix
                  let withPrefixAlignment = startupAlignmentState withPrefixDelivery
                      withPrefixTransfer =
                        Alignment.alignmentTransferState withPrefixAlignment
                  retainedSource <-
                    requireSourceSubscription identifier withPrefixTransfer
                  let through = Transfer.sourceSubscriptionSentThrough retainedSource
                  preparedReceipt <-
                    mapOwner
                      ( Transfer.preparePredecessorInputClosureReceipt
                          generationId
                          local
                          sourceStore
                          through
                          prefixes
                          withPrefixTransfer
                      )
                  let (withReceiptTransfer, receiptDisposition) =
                        Transfer.commitPredecessorInputClosureReceipt preparedReceipt
                      withReceipt =
                        replaceStartupAlignmentState
                          ( Alignment.replaceAlignmentTransferState
                              withReceiptTransfer
                              withPrefixAlignment
                          )
                          withPrefixDelivery
                  (afterOwnerClosures, ownerClosureEffects, ownerClosureWork) <-
                    completeInputClosureOwners expectedOwners withReceipt
                  let afterClosureAlignment =
                        startupAlignmentState afterOwnerClosures
                      afterClosureTransfer =
                        Alignment.alignmentTransferState afterClosureAlignment
                  preparedRelease <-
                    mapOwner
                      ( Transfer.prepareSourceLiveRelease
                          generationId
                          sourceStore
                          identifier
                          afterClosureTransfer
                      )
                  let releaseControls =
                        Transfer.preparedSourceLiveReleaseControls preparedRelease
                      (releasedTransfer, releaseDisposition) =
                        Transfer.commitSourceLiveRelease preparedRelease
                      released =
                        replaceStartupAlignmentState
                          ( Alignment.replaceAlignmentTransferState
                              releasedTransfer
                              afterClosureAlignment
                          )
                          afterOwnerClosures
                  (successor, releaseEffects) <-
                    deliverControls prefixRemote releaseControls released
                  Right
                    ( successor,
                      effects
                        <> barrierEffects
                        <> prefixEffects
                        <> ownerClosureEffects
                        <> releaseEffects,
                      work
                        <> barrierWork
                        <> prefixWork
                        <> transferWorkDisposition receiptDisposition
                        <> ownerClosureWork
                        <> transferWorkDisposition releaseDisposition
                    )
    _ -> Right (state, effects, work)

completeInputClosureOwners ::
  Set AlignmentObligationId ->
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
completeInputClosureOwners expectedOwners predecessor =
  foldM completeOne (predecessor, mempty, mempty) closingOwners
  where
    closingOwners =
      filter
        (`Alignment.alignmentObligationIsClosing` startupAlignmentState predecessor)
        (Set.toAscList expectedOwners)

    completeOne (state, effects, work) owner = do
      prepared <-
        mapOwner
          ( Alignment.prepareAlignmentObligationClosureCompletion
              owner
              (startupAlignmentState state)
          )
      let cancellations =
            Alignment.preparedAlignmentObligationClosureCompletionCancellations prepared
          (successorAlignment, disposition) =
            Alignment.commitAlignmentObligationClosureCompletion prepared
          completed = replaceStartupAlignmentState successorAlignment state
      foldM
        deliverClosureCancellation
        ( completed,
          effects,
          work
            <> workWhen
              ( disposition
                  == Alignment.AlignmentObligationClosureCompleted
              )
        )
        cancellations

    deliverClosureCancellation (state, effects, work) cancellation = do
      let identifier = alignmentCancelSubscriptionId cancellation
          transfer =
            Alignment.alignmentTransferState (startupAlignmentState state)
      destination <- requireDestinationSubscription identifier transfer
      remote <-
        maybe
          (Left AlignmentTransferOwnerContradiction)
          Right
          (destinationSourceHerald destination)
      (successor, delivery) <-
        deliverControls remote [AlignmentCancelled cancellation] state
      Right (successor, effects <> delivery, work)

-- | Frozen cuts and retained markers remain immutable history.  Completeness
-- is projected through current active membership; an inactive predecessor no
-- longer owns a closure barrier. Marker admission proves its source belongs to
-- this fixed placement and differs from its predecessor, so checking every
-- expected key is sufficient. The composed invariant validates those facts.
routeCutoverSetComplete ::
  Set HeraldEpoch ->
  ContextClassGenerationId ->
  HeraldEpoch ->
  AlignmentCut ->
  Transfer.State ->
  Bool
routeCutoverSetComplete active generation predecessor cut transfer
  | Set.notMember predecessor active = True
  | otherwise = all markerRetained placement
  where
    placement =
      physicalPlacementRevisionEntries (alignmentCutPhysicalPlacementRevisionVector cut)
    markerRetained (source, _) =
      source == predecessor
        || Set.notMember source active
        || isJust (Transfer.lookupRouteCutover generation source predecessor transfer)

ensurePredecessorInputClosures ::
  InputClosureQueryMode ->
  ContextClassGenerationId ->
  Set AlignmentObligationId ->
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    ( HeraldState,
      EffectBatch,
      Maybe [Transfer.MemberReadinessPrefix],
      AlignmentTransferWorkDisposition
    )
ensurePredecessorInputClosures queries generation expectedOwners predecessor = do
  let alignment = startupAlignmentState predecessor
      transfer = Alignment.alignmentTransferState alignment
      currentByOwner = case queries of
        IndexedInputClosureQueries ->
          Map.fromList
            [ (owner, identifier)
            | owner <- Set.toAscList expectedOwners,
              Just identifier <-
                [ Transfer.completedInputClosureSubscription generation owner transfer
                    <|> (fst <$> Transfer.availableInputClosureDestination owner transfer)
                ]
            ]
        ExhaustiveInputClosureQueries -> exhaustiveCurrentByOwner
      exhaustiveCurrentByOwner =
        completedByOwner
          `Map.union` Map.fromList
            [ (alignmentSubscribeObligationId subscribe, identifier)
            | (identifier, destination) <- Transfer.destinationSubscriptionEntries transfer,
              let subscribe = Transfer.destinationSubscriptionSubscribe destination,
              Set.member (alignmentSubscribeObligationId subscribe) expectedOwners,
              destinationAvailableForInputClosure destination
            ]
      completedByOwner =
        Map.fromList
          [ (owner, alignmentSubscribeSubscriptionId subscribe)
          | ((retainedGeneration, owner), (subscribe, Just _)) <-
              Transfer.predecessorInputClosureEntries transfer,
            retainedGeneration == generation,
            Set.member owner expectedOwners
          ]
  if Map.keysSet currentByOwner /= expectedOwners
    then Right (predecessor, mempty, Nothing, mempty)
    else do
      prepared <-
        mapOwner
          ( Transfer.preparePredecessorInputClosures
              generation
              (Set.fromList (Map.elems currentByOwner))
              transfer
          )
      let reoffers = Transfer.preparedPredecessorInputClosureSubscribes prepared
          (successorTransfer, disposition) =
            Transfer.commitPredecessorInputClosures prepared
          retained =
            replaceStartupAlignmentState
              (Alignment.replaceAlignmentTransferState successorTransfer alignment)
              predecessor
      if null reoffers
        then
          Right
            ( retained,
              mempty,
              completeInputClosureWitnesses generation expectedOwners successorTransfer,
              transferWorkDisposition disposition
            )
        else do
          (delivered, effects, loopbackWork) <-
            foldM deliverOne (retained, mempty, mempty) reoffers
          Right
            ( delivered,
              effects,
              Nothing,
              transferWorkDisposition disposition <> loopbackWork
            )
  where
    deliverOne (state, effects, work) subscribe = do
      destination <-
        requireDestinationSubscription
          (alignmentSubscribeSubscriptionId subscribe)
          (Alignment.alignmentTransferState (startupAlignmentState state))
      remote <-
        maybe
          (Left AlignmentTransferOwnerContradiction)
          Right
          (destinationSourceHerald destination)
      (successor, delivery) <-
        deliverControls remote [AlignmentSubscribeRequested subscribe] state
      -- New or replaced barrier subscriptions may synchronously advance local
      -- destination evidence and its Store application. Both commit with the
      -- transfer owner, which witnesses that additional loopback work.
      let loopbackChanged =
            remote == checkedLocalHeraldEpoch (startupGenesis state)
              && Alignment.alignmentTransferState (startupAlignmentState successor)
                /= Alignment.alignmentTransferState (startupAlignmentState state)
      Right
        ( successor,
          effects <> delivery,
          work <> workWhen loopbackChanged
        )

completeInputClosureWitnesses ::
  ContextClassGenerationId ->
  Set AlignmentObligationId ->
  Transfer.State ->
  Maybe [Transfer.MemberReadinessPrefix]
completeInputClosureWitnesses generation expectedOwners transfer =
  traverse completed (Set.toAscList expectedOwners)
  where
    completed owner = do
      (subscribe, witnessed) <- Transfer.lookupPredecessorInputClosure generation owner transfer
      through <- witnessed
      let identifier = alignmentSubscribeSubscriptionId subscribe
      destination <- Transfer.lookupDestinationSubscription identifier transfer
      applied <- Transfer.destinationSubscriptionAppliedThrough destination
      live <- Transfer.destinationSubscriptionLiveThrough destination
      acknowledged <- Transfer.destinationSubscriptionAcknowledgedThrough destination
      guard
        ( applied >= through
            && live >= through
            && acknowledged >= through
        )
      pure
        ( Transfer.memberReadinessPrefix
            identifier
            applied
            through
            acknowledged
        )

completedInputClosureOwners ::
  ContextClassGenerationId ->
  Transfer.State ->
  Set AlignmentObligationId
completedInputClosureOwners generation transfer =
  Set.fromList
    [ owner
    | ((retainedGeneration, owner), (_, Just _)) <-
        Transfer.predecessorInputClosureEntries transfer,
      retainedGeneration == generation
    ]

requiredPredecessorInputOwners ::
  ContextClassGenerationId ->
  StoreIncarnationId ->
  Alignment.State ->
  Set AlignmentObligationId
requiredPredecessorInputOwners generation sourceStore alignment =
  Set.fromList ordinaryOwners
  where
    targets obligation =
      alignmentObligationDestinationGeneration obligation == generation
        && any
          ((== sourceStore) . destinationStoreIncarnation)
          (NonEmpty.toList (alignmentObligationDestinationStores obligation))
    ordinaryOwners =
      [ identifier
      | (identifier, obligation) <-
          Alignment.activeAlignmentObligationEntries alignment
            <> Alignment.alignmentClosedObligationEntries alignment,
        targets obligation
      ]

-- | Enumerate only ordinary relation destinations eligible to witness one
-- predecessor input closure. Bootstrap-purpose subscriptions deliberately do
-- not own this barrier.
predecessorInputClosureCandidates ::
  ContextClassGenerationId ->
  StoreIncarnationId ->
  Transfer.State ->
  [(AlignmentSubscriptionId, Transfer.DestinationSubscription)]
predecessorInputClosureCandidates generation sourceStore transfer =
  [ (identifier, destination)
  | (identifier, destination) <- Transfer.destinationSubscriptionEntries transfer,
    Just _ <- [Transfer.destinationSubscriptionAttempt destination],
    let obligation =
          alignmentSubscribeObligation
            (Transfer.destinationSubscriptionSubscribe destination),
    alignmentObligationDestinationGeneration obligation == generation,
    any
      ((== sourceStore) . destinationStoreIncarnation)
      (NonEmpty.toList (alignmentObligationDestinationStores obligation)),
    destinationAvailableForInputClosure destination
  ]

destinationAvailableForInputClosure :: Transfer.DestinationSubscription -> Bool
destinationAvailableForInputClosure destination =
  case Transfer.destinationSubscriptionCancellation destination of
    Nothing -> True
    Just cancellation ->
      alignmentCancelReason cancellation == AlignmentRelationRemoved
        && destinationHasCoveredLive destination

destinationHasCoveredLive :: Transfer.DestinationSubscription -> Bool
destinationHasCoveredLive destination =
  case ( Transfer.destinationSubscriptionAppliedThrough destination,
         Transfer.destinationSubscriptionLiveThrough destination,
         Transfer.destinationSubscriptionAcknowledgedThrough destination
       ) of
    (Just applied, Just live, Just acknowledged) ->
      applied >= live
        && acknowledged >= live
    _ -> False

-- | Finite composed proof for closure and local-readiness facts that the
-- transcript owner cannot derive from generation and Store owners alone.
validateAlignmentTransferCoordinatorRelationships ::
  HeraldState -> Either AlignmentTransferCoordinatorProblem ()
validateAlignmentTransferCoordinatorRelationships state = do
  validateLocalRouteCutoverProduction state
  validatePredecessorInputClosureRelationships state
  let localReady =
        Map.fromList
          [ (key, ready)
          | (key@(generation, store), ready) <-
              Alignment.alignmentMemberReadinessEntries alignment,
            memberHostedBy generation store local alignment
          ]
      fulfillments =
        Map.fromList (Transfer.localMemberReadinessFulfillmentEntries transfer)
  requireOwner (Map.keysSet localReady == Map.keysSet fulfillments)
  traverse_
    (validateLocalReadinessFulfillment state localReady)
    (Map.toAscList fulfillments)
  where
    alignment = startupAlignmentState state
    transfer = Alignment.alignmentTransferState alignment
    local = checkedLocalHeraldEpoch (startupGenesis state)

validateLocalRouteCutoverProduction ::
  HeraldState -> Either AlignmentTransferCoordinatorProblem ()
validateLocalRouteCutoverProduction state = do
  requireOwner (observedRemote `Set.isSubsetOf` expectedRemote)
  requireOwner (observedLocal `Set.isSubsetOf` expectedLocal)
  where
    local = checkedLocalHeraldEpoch (startupGenesis state)
    alignment = startupAlignmentState state
    transfer = Alignment.alignmentTransferState alignment
    active = currentActiveHeralds state
    candidates = routeCutoverCandidates state
    expectedRemote =
      Set.fromList
        [ (candidate.generation, candidate.source, candidate.predecessor)
        | candidate <- candidates,
          candidate.source /= candidate.predecessor
        ]
    expectedLocal =
      Set.fromList
        [ (candidate.generation, candidate.predecessor)
        | candidate <- candidates,
          candidate.source == candidate.predecessor
        ]
    observedRemote =
      Set.fromList
        [ key
        | (key@(generation, source, predecessor), _) <- Transfer.routeCutoverEntries transfer,
          source == local,
          Set.member source active,
          Set.member predecessor active,
          maybe
            False
            (not . (`generationPlanInvalidated` alignment))
            (Alignment.lookupAlignmentGeneration generation alignment)
        ]
    observedLocal =
      Set.fromList
        [ key
        | key@(generation, predecessor) <- Transfer.localRoutePrefixSettlementEntries transfer,
          Set.member predecessor active,
          maybe
            False
            (not . (`generationPlanInvalidated` alignment))
            (Alignment.lookupAlignmentGeneration generation alignment)
        ]

validatePredecessorInputClosureRelationships ::
  HeraldState -> Either AlignmentTransferCoordinatorProblem ()
validatePredecessorInputClosureRelationships state = do
  expected <- expectedPredecessorInputClosureOwners state
  let barriers = Map.fromList (Transfer.predecessorInputClosureEntries transfer)
      receipts = Map.fromList (Transfer.predecessorInputClosureReceiptEntries transfer)
  requireOwner (Map.keysSet barriers == expected)
  traverse_ (validateBarrier barriers) (Map.toAscList barriers)
  traverse_ (validateReceipt barriers) (Map.toAscList receipts)
  traverse_ (validateReleasedSource receipts) (Transfer.sourceSubscriptionEntries transfer)
  where
    alignment = startupAlignmentState state
    transfer = Alignment.alignmentTransferState alignment
    local = checkedLocalHeraldEpoch (startupGenesis state)

    validateBarrier _ ((_, owner), (subscribe, witnessed)) = do
      requireOwner (alignmentSubscribeObligationId subscribe == owner)
      destination <-
        maybe
          (Left AlignmentTransferOwnerContradiction)
          Right
          ( lookup
              (alignmentSubscribeSubscriptionId subscribe)
              (Transfer.destinationSubscriptionEntries transfer)
          )
      requireOwner
        ( Transfer.destinationSubscriptionSubscribe destination == subscribe
            && case Transfer.destinationSubscriptionCancellation destination of
              Nothing -> True
              Just _ -> witnessed /= Nothing
        )

    validateReceipt barriers ((generation, sourceStore), receipt) = do
      requireOwner
        ( Transfer.predecessorInputClosureReceiptGeneration receipt == generation
            && Transfer.predecessorInputClosureReceiptSourceStore receipt == sourceStore
            && Transfer.predecessorInputClosureReceiptPredecessorHerald receipt == local
            && (generation, local)
              `elem` Transfer.localRoutePrefixSettlementEntries transfer
            && any
              (sourceMatchesReceipt generation sourceStore)
              (fmap snd (Transfer.sourceSubscriptionEntries transfer))
        )
      generationValue <- requireGeneration generation alignment
      predecessorGeneration <-
        maybe
          (Left AlignmentTransferOwnerContradiction)
          Right
          ( find
              (\candidate -> memberHeraldForStore sourceStore candidate == Just local)
              [ candidate
              | predecessorId <-
                  alignmentCutPredecessorGenerationIds
                    (alignmentGenerationCut generationValue),
                Just candidate <- [Alignment.lookupAlignmentGeneration predecessorId alignment]
              ]
          )
      requireOwner
        ( routeCutoverSetComplete
            (currentActiveHeralds state)
            generation
            local
            (alignmentGenerationCut generationValue)
            transfer
        )
      let predecessorId = alignmentGenerationId predecessorGeneration
          expectedOwners =
            Set.union
              (requiredPredecessorInputOwners predecessorId sourceStore alignment)
              (retiredPlanInputOwners predecessorId sourceStore alignment)
          prefixes = Transfer.predecessorInputClosureReceiptPrefixes receipt
          prefixOwners =
            Set.fromList
              [ owner
              | prefix <- prefixes,
                let identifier = Transfer.memberReadinessPrefixSubscription prefix,
                Just destination <-
                  [Transfer.lookupDestinationSubscription identifier transfer],
                let owner =
                      alignmentSubscribeObligationId
                        (Transfer.destinationSubscriptionSubscribe destination),
                Map.lookup (generation, owner) barriers
                  == Just
                    ( Transfer.destinationSubscriptionSubscribe destination,
                      Just (Transfer.memberReadinessPrefixLive prefix)
                    )
              ]
      requireOwner (prefixOwners == expectedOwners)
      sourceSlot <-
        maybe
          (Left AlignmentTransferOwnerContradiction)
          Right
          (Store.lookupRetainedStoreSlot sourceStore (startupStoreState state))
      requireOwner
        ( Transfer.predecessorInputClosureReceiptSourceRevision receipt
            <= Store.storeSlotRevision sourceSlot
        )

    sourceMatchesReceipt generation sourceStore source =
      let subscribe = Transfer.sourceSubscriptionSubscribe source
          obligation = alignmentSubscribeObligation subscribe
       in alignmentSubscribeSourceStoreIncarnation subscribe == sourceStore
            && alignmentObligationDestinationGeneration obligation == generation
            && alignmentObligationSourceGeneration obligation /= generation
            && case alignmentSubscribeSourceProof subscribe of
              BootstrapProof retained _ -> retained == generation
              FinalCertificate {} -> False

    validateReleasedSource receipts (_, source) =
      let subscribe = Transfer.sourceSubscriptionSubscribe source
          obligation = alignmentSubscribeObligation subscribe
          generation = alignmentObligationDestinationGeneration obligation
          sourceStore = alignmentSubscribeSourceStoreIncarnation subscribe
          isPredecessorBootstrap = case alignmentSubscribeSourceProof subscribe of
            BootstrapProof retained _ ->
              retained == generation
                && alignmentObligationSourceGeneration obligation /= generation
            FinalCertificate {} -> False
       in requireOwner
            ( not isPredecessorBootstrap
                || not (Transfer.sourceSubscriptionLiveReleased source)
                || Map.member (generation, sourceStore) receipts
            )
validateLocalReadinessFulfillment ::
  HeraldState ->
  Map
    (ContextClassGenerationId, StoreIncarnationId)
    ClassMemberReady ->
  ( (ContextClassGenerationId, StoreIncarnationId),
    Transfer.LocalMemberReadinessFulfillment
  ) ->
  Either AlignmentTransferCoordinatorProblem ()
validateLocalReadinessFulfillment state localReady (key@(generation, store), fulfillment) = do
  ready <- maybe (Left AlignmentTransferOwnerContradiction) Right (Map.lookup key localReady)
  let alignment = startupAlignmentState state
      destination = Transfer.localMemberReadinessFulfillmentDestination fulfillment
      revision = Transfer.localMemberReadinessFulfillmentStoreRevision fulfillment
      prefixes = Transfer.localMemberReadinessFulfillmentPrefixes fulfillment
      prefixIds = Set.fromList (fmap Transfer.memberReadinessPrefixSubscription prefixes)
      prefixOwners =
        Set.fromList
          [ alignmentSubscribeObligationId
              (Transfer.destinationSubscriptionSubscribe retained)
          | prefix <- prefixes,
            Just retained <-
              [ lookup
                  (Transfer.memberReadinessPrefixSubscription prefix)
                  (Transfer.destinationSubscriptionEntries (Alignment.alignmentTransferState alignment))
              ]
          ]
      expectedOwners = localMemberRequiredOwners generation destination alignment
      expectedDigest =
        deriveMemberReadyEvidenceDigest
          (memberReadyPrefixBytes generation destination revision prefixes)
  requireOwner
    ( Transfer.localMemberReadinessFulfillmentGeneration fulfillment == generation
        && destinationStoreIncarnation destination == store
        && Transfer.localMemberReadinessFulfillmentDigest fulfillment == expectedDigest
        && classMemberReadyPredecessorAndBasePrefixDigest ready == expectedDigest
        && classMemberReadyStoreRevision ready == revision
        && Set.size prefixIds == length prefixes
        && Set.size prefixOwners == length prefixes
        && prefixOwners == expectedOwners
        && maybe
          False
          ((>= revision) . Store.storeSlotRevision)
          (Store.lookupRetainedStoreSlot store (startupStoreState state))
    )

retiredPlanInputOwners ::
  ContextClassGenerationId ->
  StoreIncarnationId ->
  Alignment.State ->
  Set AlignmentObligationId
retiredPlanInputOwners generation sourceStore alignment =
  Set.fromList
    [ identifier
    | (_, receipt) <- Alignment.alignmentPlanInvalidationEntries alignment,
      (identifier, obligation) <-
        Alignment.alignmentPlanInvalidationReceiptObligations receipt,
      alignmentObligationDestinationGeneration obligation == generation,
      any
        ((== sourceStore) . destinationStoreIncarnation)
        (NonEmpty.toList (alignmentObligationDestinationStores obligation))
    ]

localMemberRequiredOwners ::
  ContextClassGenerationId ->
  DestinationStore ->
  Alignment.State ->
  Set AlignmentObligationId
localMemberRequiredOwners generation destination alignment =
  Set.fromList (bootstrapOwners <> activeOwners <> closedOwners <> retiredPlanOwners)
  where
    bootstrapOwners =
      [ alignmentObligationIdValue (Alignment.bootstrapImportSubscribeEnvelope bootstrapImport)
      | bootstrapImport <- retainedBootstrapImports,
        let key = Alignment.bootstrapImportKeyValue bootstrapImport,
        Alignment.bootstrapImportKeyGeneration key == generation,
        Alignment.bootstrapImportKeyDestination key == destination
      ]
    retainedBootstrapImports =
      fmap snd (Alignment.bootstrapImportEntries alignment)
        <> [ Alignment.bootstrapImportAttemptImport
               (Alignment.bootstrapImportFulfillmentAttempt fulfillment)
           | (_, fulfillment) <- Alignment.bootstrapImportFulfillmentEntries alignment
           ]
        <> [ bootstrapImport
           | (_, receipt) <- Alignment.alignmentPlanInvalidationEntries alignment,
             (_, bootstrapImport) <-
               Alignment.alignmentPlanInvalidationReceiptBootstrapImports receipt
           ]
    activeOwners =
      [ identifier
      | (identifier, obligation) <- Alignment.activeAlignmentObligationEntries alignment,
        targets obligation
      ]
    closedOwners =
      [ identifier
      | (identifier, obligation) <- Alignment.alignmentClosedObligationEntries alignment,
        targets obligation
      ]
    retiredPlanOwners =
      [ identifier
      | (_, receipt) <- Alignment.alignmentPlanInvalidationEntries alignment,
        (identifier, obligation) <-
          Alignment.alignmentPlanInvalidationReceiptObligations receipt,
        targets obligation
      ]
    targets obligation =
      alignmentObligationDestinationGeneration obligation == generation
        && destination
          `elem` NonEmpty.toList (alignmentObligationDestinationStores obligation)

expectedPredecessorInputClosureOwners ::
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (Set (ContextClassGenerationId, AlignmentObligationId))
expectedPredecessorInputClosureOwners state =
  Set.union retainedReceiptOwners . Set.unions
    <$> traverse sourceOwners (Transfer.sourceSubscriptionEntries transfer)
  where
    alignment = startupAlignmentState state
    transfer = Alignment.alignmentTransferState alignment
    local = checkedLocalHeraldEpoch (startupGenesis state)
    active = currentActiveHeralds state
    retainedReceiptOwners =
      Set.fromList
        [ (generation, owner)
        | ((generation, _), receipt) <-
            Transfer.predecessorInputClosureReceiptEntries transfer,
          prefix <- Transfer.predecessorInputClosureReceiptPrefixes receipt,
          Just destination <-
            [ Transfer.lookupDestinationSubscription
                (Transfer.memberReadinessPrefixSubscription prefix)
                transfer
            ],
          let owner =
                alignmentSubscribeObligationId
                  (Transfer.destinationSubscriptionSubscribe destination)
        ]

    sourceOwners (_, source) = do
      let subscribe = Transfer.sourceSubscriptionSubscribe source
          obligation = alignmentSubscribeObligation subscribe
          generationId = alignmentObligationDestinationGeneration obligation
          predecessorId = alignmentObligationSourceGeneration obligation
          sourceStore = alignmentSubscribeSourceStoreIncarnation subscribe
      case (Transfer.sourceSubscriptionCancellation source, alignmentSubscribeSourceProof subscribe) of
        (Just _, _) -> Right Set.empty
        (Nothing, BootstrapProof retained _)
          | retained == generationId,
            predecessorId /= generationId -> do
              generation <- requireGeneration generationId alignment
              predecessor <- requireGeneration predecessorId alignment
              let required =
                    requiredPredecessorInputOwners predecessorId sourceStore alignment
                  current =
                    Set.fromList
                      [ alignmentSubscribeObligationId
                          (Transfer.destinationSubscriptionSubscribe destination)
                      | (_, destination) <-
                          predecessorInputClosureCandidates
                            predecessorId
                            sourceStore
                            transfer
                      ]
                      <> Set.intersection
                        required
                        (completedInputClosureOwners generationId transfer)
                  markersAccepted =
                    all
                      ( \((retainedGeneration, sourceHerald, retainedPredecessor), _) ->
                          retainedGeneration /= generationId
                            || retainedPredecessor /= local
                            || not
                              ( Set.member sourceHerald active
                                  && Set.member retainedPredecessor active
                              )
                            || acceptanceRetained generationId sourceHerald alignment
                      )
                      (Transfer.routeCutoverEntries transfer)
                  eligible =
                    not (generationPlanInvalidated generation alignment)
                      && acceptanceRetained generationId local alignment
                      && memberHeraldForStore sourceStore predecessor == Just local
                      && current == required
                      && markersAccepted
                      && (generationId, local)
                        `elem` Transfer.localRoutePrefixSettlementEntries transfer
                      && routeCutoverSetComplete
                        active
                        generationId
                        local
                        (alignmentGenerationCut generation)
                        transfer
              pure
                ( if eligible
                    then Set.map (\owner -> (generationId, owner)) required
                    else Set.empty
                )
        _ -> Right Set.empty
requireOwner :: Bool -> Either AlignmentTransferCoordinatorProblem ()
requireOwner True = Right ()
requireOwner False = Left AlignmentTransferOwnerContradiction

deliverControls ::
  HeraldEpoch ->
  [AlignmentControl] ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem (HeraldState, EffectBatch)
deliverControls remote controls predecessor
  | remote == local = do
      successor <- runLoopback controls predecessor
      Right (successor, mempty)
  | otherwise =
      Right
        ( predecessor,
          orderedEffectBatch
            [ SendPeerControl binding (PeerAlignmentControl control)
            | Just binding <- [Discovery.currentPeerBinding remote (startupDiscoveryState predecessor)],
              control <- controls
            ]
        )
  where
    local = checkedLocalHeraldEpoch (startupGenesis predecessor)

runLoopback ::
  [AlignmentControl] ->
  HeraldState ->
  Either AlignmentTransferCoordinatorProblem HeraldState
runLoopback initial predecessor = go predecessor initial
  where
    local = checkedLocalHeraldEpoch (startupGenesis predecessor)
    go state [] = Right state
    go state (control : remaining) = do
      (successor, replies) <- applyCheckedTransferControl local control state
      go successor (remaining <> replies)

alignmentEvidenceEffects :: [AlignmentControl] -> HeraldState -> EffectBatch
alignmentEvidenceEffects controls state =
  orderedEffectBatch
    (concat [emit remote control | remote <- NonEmpty.toList (Membership.heraldMembershipGenerationActiveHeraldEpochs (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState state)))), remote /= local, control <- controls])
  where
    local = checkedLocalHeraldEpoch (startupGenesis state)
    emit remote control
      | Just _ <- DeliveryOwner.evidenceKey control = [QueuePeerAlignmentEvidence remote control]
      | Just binding <- Discovery.currentPeerBinding remote (startupDiscoveryState state) = [SendPeerControl binding (PeerAlignmentControl control)]
      | otherwise = []

completeAlignmentWaits ::
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
completeAlignmentWaits predecessor =
  case Application.applicationGatePhase (startupApplicationState predecessor) of
    Application.ApplicationGateClosed _ -> Right (predecessor, mempty, mempty)
    Application.ApplicationGateOpen _ -> completeOpenGateWaits predecessor

completeOpenGateWaits ::
  HeraldState ->
  Either
    AlignmentTransferCoordinatorProblem
    (HeraldState, EffectBatch, AlignmentTransferWorkDisposition)
completeOpenGateWaits predecessor = do
  ready <-
    filterM
      registrationReady
      (Wait.waitRegistrations (startupWaitState predecessor))
  let waitIds = fmap Wait.waitRegistrationId ready
  preparedRemoval <-
    mapOwner
      (Wait.prepareWaitRemoval waitIds (startupWaitState predecessor))
  preparedCompletions <-
    mapOwner
      ( Application.prepareApplicationWaitCompletions
          waitIds
          (startupApplicationState predecessor)
      )
  let (successorApplication, wakes) =
        Application.commitApplicationWaitCompletions preparedCompletions
      (successorWait, _) = Wait.commitWaitRemoval preparedRemoval
      successor =
        replaceStartupApplicationState successorApplication
          . replaceStartupWaitState successorWait
          $ predecessor
  Right
    ( successor,
      orderedEffectBatch (fmap waitWakeEffect wakes),
      workWhen (not (null waitIds))
    )
  where
    -- A resolved query is tied to an exact Store incarnation.  Losing or
    -- replacing any named branch invalidates the registration even when the
    -- remaining positive query result is empty; WaitReady is explicitly
    -- allowed to be spurious and makes the application re-resolve its query.
    registrationReady registration =
      if any queryInvalidated (Wait.waitRegistrationQueries registration)
        then Right True
        else or <$> traverse queryReady (Wait.waitRegistrationQueries registration)
    queryInvalidated query =
      any branchInvalidated (resolvedQueryBranches query)
    branchInvalidated branch =
      case Store.lookupStoreSlot
        (resolvedQueryBranchDelta branch)
        (startupStoreState predecessor) of
        Nothing -> True
        Just slot ->
          Store.storeSlotIncarnation slot
            /= resolvedQueryBranchStoreIncarnation branch
    queryReady query =
      not . null <$> effectiveWaitMatches predecessor query

effectiveWaitMatches ::
  HeraldState ->
  ResolvedQuery ->
  Either
    AlignmentTransferCoordinatorProblem
    [StoreObservation.EffectiveStoreMatch]
effectiveWaitMatches state query = do
  cut <-
    mapOwner
      ( StoreObservation.captureEffectiveQueryCut
          query
          (startupStoreState state)
      )
  evidence <-
    effectiveStoreEvidenceMap
      state
      (StoreObservation.effectiveQueryCutRawCandidates cut)
  plan <-
    mapOwner
      (StoreObservation.prepareEffectiveWaitPlan evidence cut)
  Right (StoreObservation.effectiveWaitMatches plan)

waitWakeEffect :: Application.ApplicationWaitWake -> HeraldEffect
waitWakeEffect wake =
  SendApplicationWaitWake
    (Application.applicationWaitWakeBinding wake)
    (Application.applicationWaitWakeCursor wake)
    (Application.applicationWaitWakeRequestId wake)
    (Application.applicationWaitWakeWaitId wake)
    (Application.applicationWaitWakeResult wake)

memberHostedBy ::
  ContextClassGenerationId ->
  StoreIncarnationId ->
  HeraldEpoch ->
  Alignment.State ->
  Bool
memberHostedBy generationId store herald alignment =
  maybe
    False
    ((== Just herald) . memberHeraldForStore store)
    (Alignment.lookupAlignmentGeneration generationId alignment)

attemptSubscribe ::
  AlignmentAttempt ->
  Either AlignmentTransferCoordinatorProblem AlignmentSubscribe
attemptSubscribe attempt =
  mapOwner
    ( alignmentSubscribe
        (alignmentAttemptObligation attempt)
        (alignmentAttemptSubscriptionId attempt)
        (historicalCertificateSourceStoreIncarnation certificate)
        ( FinalCertificate
            (historicalCertificateClassGeneration certificate)
            (historicalCertificateDigest certificate)
        )
    )
  where
    certificate = alignmentAttemptHistoricalCertificate attempt

destinationSourceHerald :: Transfer.DestinationSubscription -> Maybe HeraldEpoch
destinationSourceHerald destination =
  alignmentAttemptSourceHerald
    <$> Transfer.destinationSubscriptionAttempt destination
    <|> Transfer.destinationSubscriptionBootstrapSourceHerald destination

findAttemptBySubscription ::
  AlignmentSubscriptionId ->
  Alignment.State ->
  Maybe AlignmentAttempt
findAttemptBySubscription identifier alignment =
  find
    ((== identifier) . alignmentAttemptSubscriptionId)
    ( fmap snd (Alignment.alignmentAttemptEntries alignment)
        <> fmap
          (Alignment.alignmentAttemptInvalidationAttempt . snd)
          (Alignment.alignmentAttemptInvalidationEntries alignment)
    )

findBootstrapAttemptBySubscription ::
  AlignmentSubscriptionId ->
  Alignment.State ->
  Maybe Alignment.BootstrapImportAttempt
findBootstrapAttemptBySubscription identifier alignment =
  find
    ( (== identifier)
        . alignmentSubscribeSubscriptionId
        . Alignment.bootstrapImportAttemptSubscribe
    )
    ( fmap snd (Alignment.bootstrapImportAttemptEntries alignment)
        <> fmap
          (Alignment.bootstrapImportAttemptInvalidationAttempt . snd)
          (Alignment.bootstrapImportAttemptInvalidationEntries alignment)
    )

findCertificate ::
  ContextClassGenerationId ->
  StoreIncarnationId ->
  Alignment.State ->
  Maybe HistoricalCertificate
findCertificate generation store alignment =
  Alignment.lookupAlignmentHistoricalCertificate generation store alignment

acceptanceRetained ::
  ContextClassGenerationId ->
  HeraldEpoch ->
  Alignment.State ->
  Bool
acceptanceRetained generation herald alignment =
  Alignment.lookupAlignmentCutAcceptance generation herald alignment /= Nothing

currentActiveHeralds :: HeraldState -> Set HeraldEpoch
currentActiveHeralds =
  Set.fromList
    . NonEmpty.toList
    . Membership.heraldMembershipGenerationActiveHeraldEpochs
    . OracleProjection.oracleViewCurrentHeraldMembership
    . OracleProjection.oracleView
    . startupOracleProjectionState

requireGeneration ::
  ContextClassGenerationId ->
  Alignment.State ->
  Either AlignmentTransferCoordinatorProblem AlignmentGeneration
requireGeneration identifier alignment =
  maybe
    (Left AlignmentTransferRemoteProtocolViolation)
    Right
    (Alignment.lookupAlignmentGeneration identifier alignment)

memberHeraldForStore ::
  StoreIncarnationId ->
  AlignmentGeneration ->
  Maybe HeraldEpoch
memberHeraldForStore store generation =
  alignmentMemberHerald
    <$> find
      ((== store) . alignmentMemberStoreIncarnation)
      (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)))

freshMemberHostedLocally ::
  FreshMemberBaseEvidence ->
  AlignmentGeneration ->
  HeraldState ->
  Bool
freshMemberHostedLocally fresh generation state =
  any
    ( \member ->
        alignmentMemberDelta member == freshMemberBaseDelta fresh
          && alignmentMemberStoreIncarnation member == freshMemberBaseStoreIncarnation fresh
          && alignmentMemberHerald member == checkedLocalHeraldEpoch (startupGenesis state)
    )
    (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)))

predecessorBootstrapEvidenceDigest ::
  ContextClassGenerationId ->
  HistoricalCertificate ->
  BootstrapEvidenceDigest
predecessorBootstrapEvidenceDigest newGeneration certificate =
  deriveBootstrapEvidenceDigest
    ( Serialize.runPut $ do
        Serialize.putWord8 0
        Serialize.putByteString (contextClassGenerationIdBytes newGeneration)
        putSizedBytes (historicalCertificateCanonicalBytes certificate)
    )

freshBootstrapEvidenceDigest ::
  ContextClassGenerationId ->
  FreshMemberBaseEvidence ->
  BootstrapEvidenceDigest
freshBootstrapEvidenceDigest newGeneration fresh =
  deriveBootstrapEvidenceDigest
    ( Serialize.runPut $ do
        Serialize.putWord8 1
        Serialize.putByteString (contextClassGenerationIdBytes newGeneration)
        Serialize.putByteString (deltaIdBytes (freshMemberBaseDelta fresh))
        Serialize.putByteString
          (storeIncarnationIdBytes (freshMemberBaseStoreIncarnation fresh))
        Serialize.putWord64be (storeRevisionWord64 (freshMemberBaseRevision fresh))
    )

requireSourceSubscription ::
  AlignmentSubscriptionId ->
  Transfer.State ->
  Either AlignmentTransferCoordinatorProblem Transfer.SourceSubscription
requireSourceSubscription identifier transfer =
  maybe
    (Left AlignmentTransferOwnerContradiction)
    Right
    (Transfer.lookupSourceSubscription identifier transfer)

requireDestinationSubscription ::
  AlignmentSubscriptionId ->
  Transfer.State ->
  Either AlignmentTransferCoordinatorProblem Transfer.DestinationSubscription
requireDestinationSubscription identifier transfer =
  maybe
    (Left AlignmentTransferRemoteProtocolViolation)
    Right
    (Transfer.lookupDestinationSubscription identifier transfer)

mapRemote :: Either problem value -> Either AlignmentTransferCoordinatorProblem value
mapRemote = either (const (Left AlignmentTransferRemoteProtocolViolation)) Right

mapOwner :: Either problem value -> Either AlignmentTransferCoordinatorProblem value
mapOwner = either (const (Left AlignmentTransferOwnerContradiction)) Right

mapPeerInputAuthority ::
  Either PeerInput.PeerInputProblem value ->
  Either AlignmentTransferCoordinatorProblem value
mapPeerInputAuthority = \case
  Right value -> Right value
  Left problem -> case PeerInput.peerInputProblemDisposition problem of
    PeerInput.RejectCurrentPeerBinding ->
      Left AlignmentTransferRemoteProtocolViolation
    PeerInput.PeerInputInvariantFault ->
      Left AlignmentTransferOwnerContradiction

requireRemote :: Bool -> Either AlignmentTransferCoordinatorProblem ()
requireRemote True = Right ()
requireRemote False = Left AlignmentTransferRemoteProtocolViolation

putSizedBytes :: ByteString.ByteString -> Serialize.Put
putSizedBytes bytes = do
  Serialize.putWord64be (fromIntegral (ByteString.length bytes))
  Serialize.putByteString bytes
