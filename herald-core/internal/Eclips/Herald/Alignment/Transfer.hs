{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure retained state for alignment snapshot and continuing-change/live/ack
-- transfer. Semantic source validation and Store application stay
-- in the enclosing whole-Herald coordinator; this owner only admits exact
-- subscription transcripts and contiguous revision progress.
module Eclips.Herald.Alignment.Transfer
  ( State,
    emptyState,
    takeDestinationProgressChanges,
    clearDestinationProgressChanges,
    deferredDestinationControls,
    firstDeferredDestinationControls,
    retainDeferredDestinationControl,
    removeDeferredDestinationControl,
    discardDeferredDestinationControls,
    SourceSubscription,
    sourceSubscriptionSubscribe,
    sourceSubscriptionBaseRevision,
    sourceSubscriptionSnapshotFacts,
    sourceSubscriptionSentChanges,
    sourceSubscriptionSentThrough,
    sourceSubscriptionAcknowledgedThrough,
    sourceSubscriptionCancellation,
    sourceSubscriptionLiveReleased,
    sourceSubscriptionIsLiveAcknowledged,
    sourceSubscriptionEntries,
    sourceSubscriptionIds,
    pendingSourcePrefixIds,
    nextSourcePrefixWork,
    wakeSourcePrefixes,
    consumeSourcePrefixWork,
    clearSourcePrefixWork,
    pendingPredecessorInputClosureSourceIds,
    InputClosureTarget,
    inputClosureSourceIds,
    pendingInputClosureSourceIds,
    nextInputClosureWork,
    consumeInputClosureWork,
    wakeInputClosureTargets,
    wakeInputClosureGenerations,
    wakeAllInputClosures,
    inputClosureMembership,
    seedInputClosureMembership,
    synchronizeInputClosureMembership,
    clearInputClosureWork,
    predecessorInputClosureCandidateEntries,
    availableInputClosureDestination,
    completedInputClosureOwners,
    completedInputClosureSubscription,
    lookupSourceSubscription,
    SourceRejection,
    sourceRejectionSubscribe,
    sourceRejectionCancellation,
    sourceRejectionEntries,
    DestinationSubscription,
    destinationSubscriptionSubscribe,
    destinationSubscriptionAttempt,
    destinationSubscriptionBootstrapSourceHerald,
    destinationSubscriptionHasSnapshotTranscript,
    destinationSubscriptionAppliedSnapshotBaseRevision,
    destinationSubscriptionSnapshotFacts,
    destinationSubscriptionChanges,
    destinationSubscriptionAppliedThrough,
    destinationSubscriptionLiveThrough,
    destinationSubscriptionAcknowledgedThrough,
    destinationSubscriptionCancellation,
    destinationSubscriptionEntries,
    lookupDestinationSubscription,
    AppliedDestinationEvidenceKind (..),
    AppliedDestinationEvidenceDisposition (..),
    AppliedDestinationEvidence,
    appliedDestinationEvidence,
    appliedDestinationEvidenceSubscription,
    appliedDestinationEvidenceDestination,
    appliedDestinationEvidencePublication,
    appliedDestinationEvidenceStrength,
    appliedDestinationEvidenceSourceRevision,
    appliedDestinationEvidenceKind,
    appliedDestinationEvidenceDisposition,
    appliedDestinationEvidenceEntries,
    PreparedAppliedDestinationEvidence,
    prepareAppliedDestinationEvidence,
    commitAppliedDestinationEvidence,
    routeCutoverEntries,
    lookupRouteCutover,
    routeCutoverAssignmentEntries,
    localRoutePrefixSettlementEntries,
    localRoutePrefixSettled,
    predecessorInputClosureEntries,
    lookupPredecessorInputClosure,
    PredecessorInputClosureReceipt,
    predecessorInputClosureReceiptGeneration,
    predecessorInputClosureReceiptPredecessorHerald,
    predecessorInputClosureReceiptSourceStore,
    predecessorInputClosureReceiptSourceRevision,
    predecessorInputClosureReceiptPrefixes,
    predecessorInputClosureReceiptEntries,
    lookupPredecessorInputClosureReceipt,
    MemberReadinessPrefix,
    memberReadinessPrefix,
    memberReadinessPrefixSubscription,
    memberReadinessPrefixApplied,
    memberReadinessPrefixLive,
    memberReadinessPrefixAcknowledged,
    LocalMemberReadinessFulfillment,
    localMemberReadinessFulfillmentGeneration,
    localMemberReadinessFulfillmentDestination,
    localMemberReadinessFulfillmentStoreRevision,
    localMemberReadinessFulfillmentPrefixes,
    localMemberReadinessFulfillmentDigest,
    localMemberReadinessFulfillmentEntries,
    AlignmentTransferDisposition (..),
    AlignmentTransferProblem (..),
    SourceLiveMode (..),
    PreparedSourceSubscription,
    prepareSourceSubscription,
    preparedSourceSubscriptionControls,
    commitSourceSubscription,
    PreparedSourceLiveRelease,
    prepareSourceLiveRelease,
    preparedSourceLiveReleaseControls,
    commitSourceLiveRelease,
    PreparedSourceChanges,
    prepareSourceChanges,
    preparedSourceChangeControls,
    commitSourceChanges,
    PreparedSourceAcknowledgement,
    prepareSourceAcknowledgement,
    commitSourceAcknowledgement,
    PreparedSourceRejection,
    prepareSourceRejection,
    preparedSourceRejectionControls,
    commitSourceRejection,
    sourceRetryControls,
    PreparedDestinationAttempt,
    prepareDestinationAttempt,
    preparedDestinationAttemptSubscribe,
    commitDestinationAttempt,
    PreparedDestinationBootstrap,
    prepareDestinationBootstrap,
    preparedDestinationBootstrapSubscribe,
    commitDestinationBootstrap,
    PreparedRouteCutoverMarker,
    prepareRouteCutoverMarker,
    commitRouteCutoverMarker,
    PreparedRouteCutoverAssignments,
    prepareRouteCutoverAssignments,
    commitRouteCutoverAssignments,
    PreparedLocalRoutePrefixSettlement,
    prepareLocalRoutePrefixSettlement,
    commitLocalRoutePrefixSettlement,
    PreparedPredecessorInputClosures,
    preparePredecessorInputClosures,
    preparedPredecessorInputClosureSubscribes,
    commitPredecessorInputClosures,
    PreparedPredecessorInputClosureReceipt,
    preparePredecessorInputClosureReceipt,
    commitPredecessorInputClosureReceipt,
    PreparedIncompletePredecessorInputClosureRemoval,
    prepareIncompletePredecessorInputClosureRemoval,
    commitIncompletePredecessorInputClosureRemoval,
    PreparedLocalMemberReadinessFulfillment,
    prepareLocalMemberReadinessFulfillment,
    commitLocalMemberReadinessFulfillment,
    DestinationWork,
    destinationWorkSnapshot,
    destinationWorkChanges,
    destinationWorkAcknowledgement,
    PreparedDestinationWork,
    prepareDestinationSnapshotStart,
    prepareDestinationSnapshotChunk,
    prepareDestinationSnapshotEnd,
    prepareDestinationChange,
    prepareDestinationLive,
    preparedDestinationWork,
    commitDestinationWork,
    PreparedAlignmentCancellation,
    prepareAlignmentCancellation,
    commitAlignmentCancellation,
    destinationRetryControls,
    AlignmentTransferInvariantProblem (..),
    validateAlignmentTransferState,
  )
where

import Control.Monad (foldM)
import Data.List (nub, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment
  ( AlignmentSnapshotDigest,
    ContextClassGenerationId,
    MemberReadyEvidenceDigest,
    StoreRevision,
    nextStoreRevision,
  )
import Eclips.Domain.Identity
  ( HeraldEpoch,
    PublicationId,
    StoreIncarnationId,
  )
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.Route (ReplicaStrength)
import Eclips.Herald.Alignment.Protocol
  ( AlignmentAck,
    AlignmentAttempt,
    AlignmentCancel,
    AlignmentCancelReason (..),
    AlignmentChange,
    AlignmentControl (..),
    AlignmentLive,
    AlignmentObligationId,
    AlignmentRouteCutoverMarker,
    AlignmentSnapshotChunk,
    AlignmentSnapshotEnd,
    AlignmentSnapshotStart,
    AlignmentSourceProof (..),
    AlignmentSubscribe,
    AlignmentSubscriptionId,
    DestinationStore,
    RetainedPublicationEvidence,
    RetainedStateEvidence,
    alignmentAck,
    alignmentAckAppliedSourceStoreRevision,
    alignmentAckSubscriptionId,
    alignmentAttemptHistoricalCertificate,
    alignmentAttemptHistoricalCertificateDigest,
    alignmentAttemptObligation,
    alignmentAttemptSourceHerald,
    alignmentAttemptSubscriptionId,
    alignmentCancel,
    alignmentCancelReason,
    alignmentCancelSubscriptionId,
    alignmentChangeRetainedTransition,
    alignmentChangeStoreRevision,
    alignmentChangeSubscriptionId,
    alignmentLive,
    alignmentLiveSubscriptionId,
    alignmentLiveThroughSourceStoreRevision,
    alignmentObligationDestinationGeneration,
    alignmentObligationDestinationStores,
    alignmentObligationFrozenContextPathStrength,
    alignmentObligationSourceGeneration,
    alignmentRouteCutoverMarkerGeneration,
    alignmentRouteCutoverMarkerPredecessorHerald,
    alignmentRouteCutoverMarkerSourceHerald,
    alignmentSemanticSnapshotDigest,
    alignmentSnapshotChunk,
    alignmentSnapshotChunkNumber,
    alignmentSnapshotChunkRetainedStates,
    alignmentSnapshotChunkSubscriptionId,
    alignmentSnapshotEnd,
    alignmentSnapshotEndBaseRevision,
    alignmentSnapshotEndSemanticDigest,
    alignmentSnapshotEndSubscriptionId,
    alignmentSnapshotStart,
    alignmentSnapshotStartBaseRevision,
    alignmentSnapshotStartChunkCount,
    alignmentSnapshotStartSemanticDigest,
    alignmentSnapshotStartSubscriptionId,
    alignmentSubscribe,
    alignmentSubscribeObligation,
    alignmentSubscribeObligationId,
    alignmentSubscribeSourceProof,
    alignmentSubscribeSourceStoreIncarnation,
    alignmentSubscribeSubscriptionId,
    destinationStoreIncarnation,
    historicalCertificateClassGeneration,
    historicalCertificateSourceStoreIncarnation,
    retainedPublicationEvidencePublicationId,
    retainedPublicationEvidenceSourceStrength,
    retainedStateEvidenceCanonicalBytes,
    retainedStateEvidenceRepresentative,
    retainedStateEvidenceStrengthWitness,
  )
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
    preparedOutput,
  )
import Eclips.Herald.PeerPayload
  ( PeerLogicalPayload (PeerLogicalRouteCutover),
    peerLogicalPayloadDigest,
  )
import Eclips.Herald.PeerStream
  ( AssignmentReceipt,
    assignmentReceiptDigest,
    assignmentReceiptDirection,
    streamDirectionDestination,
    streamDirectionSource,
  )

data SourceSubscription = SourceSubscription
  { subscribe :: AlignmentSubscribe,
    baseRevision :: StoreRevision,
    snapshotDigest :: AlignmentSnapshotDigest,
    snapshotFacts :: [RetainedStateEvidence],
    sentChanges :: Map StoreRevision AlignmentChange,
    sentThrough :: StoreRevision,
    acknowledgedThrough :: Maybe StoreRevision,
    liveInitiallyWithheld :: Bool,
    liveReleased :: Bool,
    cancellation :: Maybe AlignmentCancel
  }
  deriving stock (Eq, Show)

sourceSubscriptionSubscribe :: SourceSubscription -> AlignmentSubscribe
sourceSubscriptionSubscribe source = source.subscribe

sourceSubscriptionBaseRevision :: SourceSubscription -> StoreRevision
sourceSubscriptionBaseRevision source = source.baseRevision

sourceSubscriptionSnapshotFacts ::
  SourceSubscription -> [RetainedStateEvidence]
sourceSubscriptionSnapshotFacts source = source.snapshotFacts

sourceSubscriptionSentChanges :: SourceSubscription -> [AlignmentChange]
sourceSubscriptionSentChanges source = Map.elems source.sentChanges

sourceSubscriptionSentThrough :: SourceSubscription -> StoreRevision
sourceSubscriptionSentThrough source = source.sentThrough

sourceSubscriptionAcknowledgedThrough ::
  SourceSubscription -> Maybe StoreRevision
sourceSubscriptionAcknowledgedThrough source = source.acknowledgedThrough

sourceSubscriptionCancellation ::
  SourceSubscription -> Maybe AlignmentCancel
sourceSubscriptionCancellation source = source.cancellation

sourceSubscriptionLiveReleased :: SourceSubscription -> Bool
sourceSubscriptionLiveReleased source = source.liveReleased

sourceSubscriptionIsLiveAcknowledged :: SourceSubscription -> Bool
sourceSubscriptionIsLiveAcknowledged = sourceHasCoveredLive

-- | A checked subscription that cannot be served because its exact source
-- incarnation is gone.  Retaining the immutable request beside the cancel
-- makes reconnect retry deterministic without manufacturing a snapshot.
data SourceRejection = SourceRejection
  { subscribe :: AlignmentSubscribe,
    cancellation :: AlignmentCancel
  }
  deriving stock (Eq, Show)

sourceRejectionSubscribe :: SourceRejection -> AlignmentSubscribe
sourceRejectionSubscribe rejection = rejection.subscribe

sourceRejectionCancellation :: SourceRejection -> AlignmentCancel
sourceRejectionCancellation rejection = rejection.cancellation

setSourceAcknowledgedThrough ::
  Maybe StoreRevision -> SourceSubscription -> SourceSubscription
setSourceAcknowledgedThrough replacement (SourceSubscription subscribe base digest facts changes sent _ withheld released cancellation) =
  SourceSubscription subscribe base digest facts changes sent replacement withheld released cancellation

setSourceCancellation ::
  Maybe AlignmentCancel -> SourceSubscription -> SourceSubscription
setSourceCancellation replacement (SourceSubscription subscribe base digest facts changes sent acknowledged withheld released _) =
  SourceSubscription subscribe base digest facts changes sent acknowledged withheld released replacement

data SnapshotAssembly = SnapshotAssembly
  { start :: Maybe AlignmentSnapshotStart,
    chunks :: Map Word64 AlignmentSnapshotChunk,
    end :: Maybe AlignmentSnapshotEnd,
    appliedBase :: Maybe StoreRevision
  }
  deriving stock (Eq, Show)

emptySnapshotAssembly :: SnapshotAssembly
emptySnapshotAssembly =
  SnapshotAssembly
    { start = Nothing,
      chunks = Map.empty,
      end = Nothing,
      appliedBase = Nothing
    }

setSnapshotStart ::
  Maybe AlignmentSnapshotStart -> SnapshotAssembly -> SnapshotAssembly
setSnapshotStart start (SnapshotAssembly _ chunks end applied) =
  SnapshotAssembly start chunks end applied

setSnapshotChunks ::
  Map Word64 AlignmentSnapshotChunk -> SnapshotAssembly -> SnapshotAssembly
setSnapshotChunks chunks (SnapshotAssembly start _ end applied) =
  SnapshotAssembly start chunks end applied

setSnapshotEnd ::
  Maybe AlignmentSnapshotEnd -> SnapshotAssembly -> SnapshotAssembly
setSnapshotEnd end (SnapshotAssembly start chunks _ applied) =
  SnapshotAssembly start chunks end applied

setSnapshotAppliedBase ::
  Maybe StoreRevision -> SnapshotAssembly -> SnapshotAssembly
setSnapshotAppliedBase applied (SnapshotAssembly start chunks end _) =
  SnapshotAssembly start chunks end applied

data DestinationPurpose
  = ContextDestination AlignmentAttempt
  | BootstrapDestination HeraldEpoch
  deriving stock (Eq, Show)

data DestinationSubscription = DestinationSubscription
  { purpose :: DestinationPurpose,
    subscribe :: AlignmentSubscribe,
    snapshot :: SnapshotAssembly,
    heldChanges :: Map StoreRevision AlignmentChange,
    appliedChanges :: Map StoreRevision AlignmentChange,
    appliedThrough :: Maybe StoreRevision,
    liveThrough :: Maybe StoreRevision,
    acknowledgedThrough :: Maybe StoreRevision,
    cancellation :: Maybe AlignmentCancel
  }
  deriving stock (Eq, Show)

destinationSubscriptionAttempt ::
  DestinationSubscription -> Maybe AlignmentAttempt
destinationSubscriptionAttempt destination = case destination.purpose of
  ContextDestination attempt -> Just attempt
  BootstrapDestination _ -> Nothing

destinationSubscriptionSubscribe ::
  DestinationSubscription -> AlignmentSubscribe
destinationSubscriptionSubscribe destination = destination.subscribe

destinationSubscriptionBootstrapSourceHerald ::
  DestinationSubscription -> Maybe HeraldEpoch
destinationSubscriptionBootstrapSourceHerald destination = case destination.purpose of
  ContextDestination _ -> Nothing
  BootstrapDestination source -> Just source

-- | Whether any snapshot protocol unit or applied-base fact is retained for
-- this destination.  This narrow observation exists so disappearance evidence
-- can distinguish a subscription from actual retained snapshot work without
-- exposing the private assembly for mutation.
destinationSubscriptionHasSnapshotTranscript ::
  DestinationSubscription -> Bool
destinationSubscriptionHasSnapshotTranscript destination =
  case destination.snapshot of
    SnapshotAssembly start chunks end appliedBase ->
      start /= Nothing
        || not (Map.null chunks)
        || end /= Nothing
        || appliedBase /= Nothing

-- | The exact source revision whose completed snapshot facts have been
-- applied at this destination.  Disappearance evidence uses this immutable
-- transcript coordinate to distinguish settled stale history from an
-- incomplete snapshot without exposing the assembly for mutation.
destinationSubscriptionAppliedSnapshotBaseRevision ::
  DestinationSubscription -> Maybe StoreRevision
destinationSubscriptionAppliedSnapshotBaseRevision destination =
  case destination.snapshot of
    SnapshotAssembly _ _ _ appliedBase -> appliedBase

-- | Every retained state fact in the destination's snapshot chunks.  Chunks
-- and their contained facts are returned in canonical chunk/element order.
destinationSubscriptionSnapshotFacts ::
  DestinationSubscription -> [RetainedStateEvidence]
destinationSubscriptionSnapshotFacts destination =
  case destination.snapshot of
    SnapshotAssembly _ chunks _ _ ->
      concatMap alignmentSnapshotChunkRetainedStates (Map.elems chunks)

-- | All continuing changes still retained by the destination, with an
-- already-applied revision winning over an equal held revision.  Owner
-- admission guarantees equal revisions cannot carry conflicting changes.
destinationSubscriptionChanges ::
  DestinationSubscription -> [AlignmentChange]
destinationSubscriptionChanges destination =
  Map.elems (Map.union destination.appliedChanges destination.heldChanges)

destinationSubscriptionAppliedThrough ::
  DestinationSubscription -> Maybe StoreRevision
destinationSubscriptionAppliedThrough destination = destination.appliedThrough

destinationSubscriptionLiveThrough ::
  DestinationSubscription -> Maybe StoreRevision
destinationSubscriptionLiveThrough destination = destination.liveThrough

destinationSubscriptionAcknowledgedThrough ::
  DestinationSubscription -> Maybe StoreRevision
destinationSubscriptionAcknowledgedThrough destination =
  destination.acknowledgedThrough

destinationSubscriptionCancellation ::
  DestinationSubscription -> Maybe AlignmentCancel
destinationSubscriptionCancellation destination = destination.cancellation

setDestinationSnapshot ::
  SnapshotAssembly -> DestinationSubscription -> DestinationSubscription
setDestinationSnapshot replacement (DestinationSubscription purpose subscribe _ held applied appliedThrough liveThrough acknowledged cancellation) =
  DestinationSubscription purpose subscribe replacement held applied appliedThrough liveThrough acknowledged cancellation

setDestinationCancellation ::
  Maybe AlignmentCancel -> DestinationSubscription -> DestinationSubscription
setDestinationCancellation replacement (DestinationSubscription purpose subscribe snapshot held applied appliedThrough liveThrough acknowledged _) =
  DestinationSubscription purpose subscribe snapshot held applied appliedThrough liveThrough acknowledged replacement

setDestinationAppliedThrough ::
  Maybe StoreRevision -> DestinationSubscription -> DestinationSubscription
setDestinationAppliedThrough replacement destination =
  destination {appliedThrough = replacement}

setDestinationAcknowledgedThrough ::
  Maybe StoreRevision -> DestinationSubscription -> DestinationSubscription
setDestinationAcknowledgedThrough replacement (DestinationSubscription purpose subscribe snapshot held applied appliedThrough liveThrough _ cancellation) =
  DestinationSubscription purpose subscribe snapshot held applied appliedThrough liveThrough replacement cancellation

data MemberReadinessPrefix = MemberReadinessPrefix
  { subscription :: AlignmentSubscriptionId,
    applied :: StoreRevision,
    live :: StoreRevision,
    acknowledged :: StoreRevision
  }
  deriving stock (Eq, Ord, Show)

memberReadinessPrefix ::
  AlignmentSubscriptionId ->
  StoreRevision ->
  StoreRevision ->
  StoreRevision ->
  MemberReadinessPrefix
memberReadinessPrefix = MemberReadinessPrefix

memberReadinessPrefixSubscription ::
  MemberReadinessPrefix -> AlignmentSubscriptionId
memberReadinessPrefixSubscription prefix = prefix.subscription

memberReadinessPrefixApplied :: MemberReadinessPrefix -> StoreRevision
memberReadinessPrefixApplied prefix = prefix.applied

memberReadinessPrefixLive :: MemberReadinessPrefix -> StoreRevision
memberReadinessPrefixLive prefix = prefix.live

memberReadinessPrefixAcknowledged :: MemberReadinessPrefix -> StoreRevision
memberReadinessPrefixAcknowledged prefix = prefix.acknowledged

data LocalMemberReadinessFulfillment = LocalMemberReadinessFulfillment
  { generation :: ContextClassGenerationId,
    destination :: DestinationStore,
    storeRevision :: StoreRevision,
    prefixes :: [MemberReadinessPrefix],
    digest :: MemberReadyEvidenceDigest
  }
  deriving stock (Eq, Show)

localMemberReadinessFulfillmentGeneration ::
  LocalMemberReadinessFulfillment -> ContextClassGenerationId
localMemberReadinessFulfillmentGeneration fulfillment = fulfillment.generation

localMemberReadinessFulfillmentDestination ::
  LocalMemberReadinessFulfillment -> DestinationStore
localMemberReadinessFulfillmentDestination fulfillment = fulfillment.destination

localMemberReadinessFulfillmentStoreRevision ::
  LocalMemberReadinessFulfillment -> StoreRevision
localMemberReadinessFulfillmentStoreRevision fulfillment = fulfillment.storeRevision

localMemberReadinessFulfillmentPrefixes ::
  LocalMemberReadinessFulfillment -> [MemberReadinessPrefix]
localMemberReadinessFulfillmentPrefixes fulfillment = fulfillment.prefixes

localMemberReadinessFulfillmentDigest ::
  LocalMemberReadinessFulfillment -> MemberReadyEvidenceDigest
localMemberReadinessFulfillmentDigest fulfillment = fulfillment.digest

data PredecessorInputClosureReceipt = PredecessorInputClosureReceipt
  { generation :: ContextClassGenerationId,
    predecessorHerald :: HeraldEpoch,
    sourceStore :: StoreIncarnationId,
    sourceRevision :: StoreRevision,
    prefixes :: [MemberReadinessPrefix]
  }
  deriving stock (Eq, Show)

predecessorInputClosureReceiptGeneration ::
  PredecessorInputClosureReceipt -> ContextClassGenerationId
predecessorInputClosureReceiptGeneration receipt = receipt.generation

predecessorInputClosureReceiptPredecessorHerald ::
  PredecessorInputClosureReceipt -> HeraldEpoch
predecessorInputClosureReceiptPredecessorHerald receipt = receipt.predecessorHerald

predecessorInputClosureReceiptSourceStore ::
  PredecessorInputClosureReceipt -> StoreIncarnationId
predecessorInputClosureReceiptSourceStore receipt = receipt.sourceStore

predecessorInputClosureReceiptSourceRevision ::
  PredecessorInputClosureReceipt -> StoreRevision
predecessorInputClosureReceiptSourceRevision receipt = receipt.sourceRevision

predecessorInputClosureReceiptPrefixes ::
  PredecessorInputClosureReceipt -> [MemberReadinessPrefix]
predecessorInputClosureReceiptPrefixes receipt = receipt.prefixes

data AppliedDestinationEvidenceKind
  = AppliedSnapshotRepresentative
  | AppliedSnapshotStrengthWitness
  | AppliedContinuingChange
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data AppliedDestinationEvidenceDisposition
  = StoreAppliedOrAlreadyApplied
  | StoreTerminallyIgnored
  | ControlledSuppressed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data AppliedDestinationEvidence = AppliedDestinationEvidence
  { subscription :: AlignmentSubscriptionId,
    destination :: DestinationStore,
    publication :: RetainedPublicationEvidence,
    strength :: ReplicaStrength,
    sourceRevision :: StoreRevision,
    kind :: AppliedDestinationEvidenceKind,
    disposition :: AppliedDestinationEvidenceDisposition
  }
  deriving stock (Eq, Show)

appliedDestinationEvidence ::
  AlignmentSubscriptionId ->
  DestinationStore ->
  RetainedPublicationEvidence ->
  ReplicaStrength ->
  StoreRevision ->
  AppliedDestinationEvidenceKind ->
  AppliedDestinationEvidenceDisposition ->
  AppliedDestinationEvidence
appliedDestinationEvidence = AppliedDestinationEvidence

appliedDestinationEvidenceSubscription ::
  AppliedDestinationEvidence -> AlignmentSubscriptionId
appliedDestinationEvidenceSubscription evidence = evidence.subscription

appliedDestinationEvidenceDestination ::
  AppliedDestinationEvidence -> DestinationStore
appliedDestinationEvidenceDestination evidence = evidence.destination

appliedDestinationEvidencePublication ::
  AppliedDestinationEvidence -> RetainedPublicationEvidence
appliedDestinationEvidencePublication evidence = evidence.publication

appliedDestinationEvidenceStrength ::
  AppliedDestinationEvidence -> ReplicaStrength
appliedDestinationEvidenceStrength evidence = evidence.strength

appliedDestinationEvidenceSourceRevision ::
  AppliedDestinationEvidence -> StoreRevision
appliedDestinationEvidenceSourceRevision evidence = evidence.sourceRevision

appliedDestinationEvidenceKind ::
  AppliedDestinationEvidence -> AppliedDestinationEvidenceKind
appliedDestinationEvidenceKind evidence = evidence.kind

appliedDestinationEvidenceDisposition ::
  AppliedDestinationEvidence -> AppliedDestinationEvidenceDisposition
appliedDestinationEvidenceDisposition evidence = evidence.disposition

type AppliedDestinationEvidenceKey =
  ( AlignmentSubscriptionId,
    StoreIncarnationId,
    PublicationId,
    StoreRevision,
    AppliedDestinationEvidenceKind
  )

type InputClosureTarget = (ContextClassGenerationId, StoreIncarnationId)

type InputClosureBarrierKey = (ContextClassGenerationId, AlignmentObligationId)

data State = State
  { sources :: Map AlignmentSubscriptionId SourceSubscription,
    sourceIds :: !(Set AlignmentSubscriptionId),
    sourcesByRetainedStore :: !(Map StoreIncarnationId (Set AlignmentSubscriptionId)),
    pendingSourcePrefixes :: !(Set AlignmentSubscriptionId),
    eligibleInputClosureSources :: !(Set AlignmentSubscriptionId),
    pendingInputClosureSources :: !(Set AlignmentSubscriptionId),
    predecessorSourcesByChild :: !(Map ContextClassGenerationId (Set AlignmentSubscriptionId)),
    predecessorSourcesByTarget :: !(Map InputClosureTarget (Set AlignmentSubscriptionId)),
    ordinaryClosureDestinationsByTarget :: !(Map InputClosureTarget (Set AlignmentSubscriptionId)),
    availableClosureDestinationsByOwner :: !(Map AlignmentObligationId (Set AlignmentSubscriptionId)),
    inputClosureBarrierKeysBySubscription :: !(Map AlignmentSubscriptionId (Set InputClosureBarrierKey)),
    completedInputClosureOwnersByChild :: !(Map ContextClassGenerationId (Set AlignmentObligationId)),
    observedInputClosureMembership :: !(Maybe HeraldMembershipGenerationId),
    destinationProgressChanges :: !(Set AlignmentSubscriptionId),
    sourceRejections :: Map AlignmentSubscriptionId SourceRejection,
    destinations :: Map AlignmentSubscriptionId DestinationSubscription,
    routeCutovers ::
      Map
        (ContextClassGenerationId, HeraldEpoch, HeraldEpoch)
        AlignmentRouteCutoverMarker,
    routeCutoverAssignments ::
      Map
        (ContextClassGenerationId, HeraldEpoch, HeraldEpoch)
        AssignmentReceipt,
    localRoutePrefixSettlements ::
      Set (ContextClassGenerationId, HeraldEpoch),
    predecessorInputClosures ::
      Map
        (ContextClassGenerationId, AlignmentObligationId)
        (AlignmentSubscribe, Maybe StoreRevision),
    predecessorInputClosureReceipts ::
      Map
        (ContextClassGenerationId, StoreIncarnationId)
        PredecessorInputClosureReceipt,
    localMemberReadinessFulfillments ::
      Map
        (ContextClassGenerationId, StoreIncarnationId)
        LocalMemberReadinessFulfillment,
    deferredControls :: Map AlignmentSubscriptionId [(HeraldEpoch, AlignmentControl)],
    appliedDestinationEvidence ::
      Map AppliedDestinationEvidenceKey AppliedDestinationEvidence
  }
  deriving stock (Eq, Show)

emptyState :: State
emptyState =
  State
    { sources = Map.empty,
      sourceIds = Set.empty,
      sourcesByRetainedStore = Map.empty,
      pendingSourcePrefixes = Set.empty,
      eligibleInputClosureSources = Set.empty,
      pendingInputClosureSources = Set.empty,
      predecessorSourcesByChild = Map.empty,
      predecessorSourcesByTarget = Map.empty,
      ordinaryClosureDestinationsByTarget = Map.empty,
      availableClosureDestinationsByOwner = Map.empty,
      inputClosureBarrierKeysBySubscription = Map.empty,
      completedInputClosureOwnersByChild = Map.empty,
      observedInputClosureMembership = Nothing,
      destinationProgressChanges = Set.empty,
      sourceRejections = Map.empty,
      destinations = Map.empty,
      routeCutovers = Map.empty,
      routeCutoverAssignments = Map.empty,
      localRoutePrefixSettlements = Set.empty,
      predecessorInputClosures = Map.empty,
      predecessorInputClosureReceipts = Map.empty,
      localMemberReadinessFulfillments = Map.empty,
      deferredControls = Map.empty,
      appliedDestinationEvidence = Map.empty
    }

-- | Exact pre-application destination frames held behind a disappearance
-- marker or a retained control prerequisite. Frames stay in arrival order per
-- subscription; equal retries never append duplicate retained work.
deferredDestinationControls :: AlignmentSubscriptionId -> State -> [(HeraldEpoch, AlignmentControl)]
deferredDestinationControls identifier state = Map.findWithDefault [] identifier state.deferredControls

firstDeferredDestinationControls :: State -> [(AlignmentSubscriptionId, HeraldEpoch, AlignmentControl)]
firstDeferredDestinationControls state =
  [(identifier, remote, control) | (identifier, (remote, control) : _) <- Map.toAscList state.deferredControls]

retainDeferredDestinationControl :: AlignmentSubscriptionId -> HeraldEpoch -> AlignmentControl -> State -> Either AlignmentTransferProblem State
retainDeferredDestinationControl identifier remote control state
  | Map.notMember identifier state.destinations = Left (AlignmentTransferSubscriptionMissing identifier)
  | deferredControlSubscription control /= Just identifier = Left (AlignmentTransferSubscriptionConflict identifier)
  | maybe True ((/= remote) . destinationSourceEpoch) (Map.lookup identifier state.destinations) = Left (AlignmentTransferSubscriptionConflict identifier)
  | (remote, control) `elem` retained = Right state
  | otherwise = do
      -- Validate the complete raw prefix against an isolated transcript before
      -- retaining it. This commits no destination semantic work to the owner.
      _ <- foldM validateFrame state (fmap snd retained <> [control])
      Right state {deferredControls = Map.insert identifier (retained <> [(remote, control)]) state.deferredControls}
  where
    retained = deferredDestinationControls identifier state
    validateFrame current frame = do
      prepared <- case frame of
        AlignmentSnapshotStarted start -> prepareDestinationSnapshotStart start current
        AlignmentSnapshotChunkTransferred chunk -> prepareDestinationSnapshotChunk chunk current
        AlignmentSnapshotEnded end -> prepareDestinationSnapshotEnd end current
        AlignmentChangeTransferred change -> prepareDestinationChange change current
        AlignmentLiveAdvertised live -> prepareDestinationLive live current
        _ -> Left (AlignmentTransferSubscriptionConflict identifier)
      pure (fst (commitDestinationWork prepared))

destinationSourceEpoch :: DestinationSubscription -> HeraldEpoch
destinationSourceEpoch destination = case destination.purpose of
  ContextDestination attempt -> alignmentAttemptSourceHerald attempt
  BootstrapDestination source -> source

discardDeferredDestinationControls :: AlignmentSubscriptionId -> State -> State
discardDeferredDestinationControls identifier state =
  state {deferredControls = Map.delete identifier state.deferredControls}

removeDeferredDestinationControl :: AlignmentSubscriptionId -> HeraldEpoch -> AlignmentControl -> State -> State
removeDeferredDestinationControl identifier remote control state =
  state
    { deferredControls = Map.update remove identifier state.deferredControls
    }
  where
    remove entries = case filter (/= (remote, control)) entries of
      [] -> Nothing
      retained -> Just retained

deferredControlSubscription :: AlignmentControl -> Maybe AlignmentSubscriptionId
deferredControlSubscription = \case
  AlignmentSnapshotStarted start -> Just (alignmentSnapshotStartSubscriptionId start)
  AlignmentSnapshotChunkTransferred chunk -> Just (alignmentSnapshotChunkSubscriptionId chunk)
  AlignmentSnapshotEnded end -> Just (alignmentSnapshotEndSubscriptionId end)
  AlignmentChangeTransferred change -> Just (alignmentChangeSubscriptionId change)
  AlignmentLiveAdvertised live -> Just (alignmentLiveSubscriptionId live)
  _ -> Nothing

sourceSubscriptionEntries ::
  State -> [(AlignmentSubscriptionId, SourceSubscription)]
sourceSubscriptionEntries state = Map.toAscList state.sources

-- | Freeze the live source identities belonging to one coordinator stage.
-- The stage takes this snapshot only when at least one source is dirty; later
-- local loopback admission must not extend that stage's iteration inventory.
sourceSubscriptionIds :: State -> Set AlignmentSubscriptionId
sourceSubscriptionIds state = state.sourceIds

pendingSourcePrefixIds :: State -> Set AlignmentSubscriptionId
pendingSourcePrefixIds state = state.pendingSourcePrefixes

-- | Select within a frozen stage-entry inventory. New identities wait for the
-- next pass; existing identities dirtied ahead of the cursor still run now.
nextSourcePrefixWork ::
  Set AlignmentSubscriptionId -> Maybe AlignmentSubscriptionId -> State -> Maybe AlignmentSubscriptionId
nextSourcePrefixWork inventory cursor state = next cursor
  where
    next lower = do
      identifier <- case lower of
        Nothing -> Set.lookupMin state.pendingSourcePrefixes
        Just previous -> Set.lookupGT previous state.pendingSourcePrefixes
      if Set.member identifier inventory then Just identifier else next (Just identifier)

-- | Store commits name only the incarnations whose retained prefix or lifetime
-- changed. Cancelled transcripts no longer own executable prefix work.
wakeSourcePrefixes :: Set StoreIncarnationId -> State -> State
wakeSourcePrefixes incarnations state
  | Set.null affected = state
  | otherwise =
      state {pendingSourcePrefixes = state.pendingSourcePrefixes `Set.union` affected}
  where
    affected =
      Set.foldl'
        (\identifiers incarnation -> identifiers `Set.union` Map.findWithDefault Set.empty incarnation state.sourcesByRetainedStore)
        Set.empty
        incarnations

-- | Consume before running the source. A synchronous delivery may append a
-- Store change and must be able to dirty this same source again for the next
-- ordered pass.
consumeSourcePrefixWork :: AlignmentSubscriptionId -> State -> State
consumeSourcePrefixWork identifier state =
  state {pendingSourcePrefixes = Set.delete identifier state.pendingSourcePrefixes}

-- | Erase scheduling notifications in a temporary semantic-comparison copy.
-- Operational advancement consumes individual keys before their handlers.
clearSourcePrefixWork :: State -> State
clearSourcePrefixWork state
  | Set.null state.pendingSourcePrefixes = state
  | otherwise = state {pendingSourcePrefixes = Set.empty}

retainSourcePrefixWork :: AlignmentSubscriptionId -> SourceSubscription -> State -> State
retainSourcePrefixWork identifier source state =
  state
    { sourcesByRetainedStore =
        Map.insertWith
          Set.union
          (alignmentSubscribeSourceStoreIncarnation source.subscribe)
          (Set.singleton identifier)
          state.sourcesByRetainedStore,
      pendingSourcePrefixes = Set.insert identifier state.pendingSourcePrefixes
    }

retireSourcePrefixWork :: AlignmentSubscriptionId -> State -> State
retireSourcePrefixWork identifier state = case Map.lookup identifier state.sources of
  Just source
    | source.cancellation /= Nothing ->
        state
          { sourceIds = Set.delete identifier state.sourceIds,
            sourcesByRetainedStore =
              Map.update
                remove
                (alignmentSubscribeSourceStoreIncarnation source.subscribe)
                state.sourcesByRetainedStore,
            pendingSourcePrefixes = Set.delete identifier state.pendingSourcePrefixes
          }
  _ -> state
  where
    remove identifiers =
      let remaining = Set.delete identifier identifiers
       in if Set.null remaining then Nothing else Just remaining

-- | Retained sources that can still require predecessor-input closure. The
-- subscription proof is immutable; cancellation and Live release only advance.
-- Callers advancing multiple sources must recheck each selected source because
-- an earlier transition may cancel or release a later one.
pendingPredecessorInputClosureSourceIds :: State -> [AlignmentSubscriptionId]
pendingPredecessorInputClosureSourceIds state =
  Map.foldrWithKey select [] state.sources
  where
    select identifier source rest =
      case (source.cancellation, source.liveReleased, alignmentSubscribeSourceProof source.subscribe) of
        (Nothing, False, BootstrapProof generation _)
          | alignmentObligationSourceGeneration (alignmentSubscribeObligation source.subscribe) /= generation ->
              identifier : rest
        _ -> rest

-- | The executable inventory is separate from retained source relations:
-- Live-released predecessor sources still justify their closure barriers.
inputClosureSourceIds :: State -> Set AlignmentSubscriptionId
inputClosureSourceIds state = state.eligibleInputClosureSources

pendingInputClosureSourceIds :: State -> Set AlignmentSubscriptionId
pendingInputClosureSourceIds state = state.pendingInputClosureSources

nextInputClosureWork ::
  Set AlignmentSubscriptionId -> Maybe AlignmentSubscriptionId -> State -> Maybe AlignmentSubscriptionId
nextInputClosureWork inventory cursor state = next cursor
  where
    next lower = do
      identifier <- case lower of
        Nothing -> Set.lookupMin state.pendingInputClosureSources
        Just previous -> Set.lookupGT previous state.pendingInputClosureSources
      if Set.member identifier inventory then Just identifier else next (Just identifier)

consumeInputClosureWork :: AlignmentSubscriptionId -> State -> State
consumeInputClosureWork identifier state =
  state {pendingInputClosureSources = Set.delete identifier state.pendingInputClosureSources}

wakeInputClosureTargets :: Set InputClosureTarget -> State -> State
wakeInputClosureTargets targets state =
  wakeInputClosureSources
    (Set.foldl' (\found target -> found `Set.union` Map.findWithDefault Set.empty target state.predecessorSourcesByTarget) Set.empty targets)
    state

wakeInputClosureGenerations :: Set ContextClassGenerationId -> State -> State
wakeInputClosureGenerations generations state =
  wakeInputClosureSources
    (Set.foldl' (\found generation -> found `Set.union` Map.findWithDefault Set.empty generation state.predecessorSourcesByChild) Set.empty generations)
    state

wakeInputClosureSources :: Set AlignmentSubscriptionId -> State -> State
wakeInputClosureSources identifiers state
  | Set.null affected = state
  | otherwise = state {pendingInputClosureSources = state.pendingInputClosureSources `Set.union` affected}
  where
    affected = identifiers `Set.intersection` state.eligibleInputClosureSources

wakeAllInputClosures :: State -> State
wakeAllInputClosures state = state {pendingInputClosureSources = state.eligibleInputClosureSources}

inputClosureMembership :: State -> Maybe HeraldMembershipGenerationId
inputClosureMembership state = state.observedInputClosureMembership

-- | Seed the initial serving owner before it has subscriptions. Subsequent
-- membership observations must use synchronization, which wakes existing work.
seedInputClosureMembership :: HeraldMembershipGenerationId -> State -> State
seedInputClosureMembership membership state = state {observedInputClosureMembership = Just membership}

synchronizeInputClosureMembership :: HeraldMembershipGenerationId -> State -> State
synchronizeInputClosureMembership membership state
  | state.observedInputClosureMembership == Just membership = state
  | otherwise = seedInputClosureMembership membership (wakeAllInputClosures state)

-- | Scheduling-only normalization for reference comparisons. Never use this
-- to consume operational work: removing the stamp wakes all work on next sync.
clearInputClosureWork :: State -> State
clearInputClosureWork state
  | Set.null state.pendingInputClosureSources && state.observedInputClosureMembership == Nothing = state
  | otherwise = state {pendingInputClosureSources = Set.empty, observedInputClosureMembership = Nothing}

predecessorInputClosureCandidateEntries :: InputClosureTarget -> State -> [(AlignmentSubscriptionId, DestinationSubscription)]
predecessorInputClosureCandidateEntries target state =
  [ (identifier, destination)
  | identifier <- Set.toAscList (Map.findWithDefault Set.empty target state.ordinaryClosureDestinationsByTarget),
    Just destination <- [Map.lookup identifier state.destinations]
  ]

-- | The old ascending destination scan selected the greatest available ID for
-- each owner. Bootstrap-purpose destinations participate in this projection.
availableInputClosureDestination :: AlignmentObligationId -> State -> Maybe (AlignmentSubscriptionId, DestinationSubscription)
availableInputClosureDestination owner state = do
  identifiers <- Map.lookup owner state.availableClosureDestinationsByOwner
  identifier <- Set.lookupMax identifiers
  destination <- Map.lookup identifier state.destinations
  pure (identifier, destination)

completedInputClosureOwners :: ContextClassGenerationId -> State -> Set AlignmentObligationId
completedInputClosureOwners generation state = Map.findWithDefault Set.empty generation state.completedInputClosureOwnersByChild

completedInputClosureSubscription :: ContextClassGenerationId -> AlignmentObligationId -> State -> Maybe AlignmentSubscriptionId
completedInputClosureSubscription generation owner state = do
  (subscribe, Just _) <- Map.lookup (generation, owner) state.predecessorInputClosures
  pure (alignmentSubscribeSubscriptionId subscribe)

sourceInputClosureScope :: SourceSubscription -> Maybe (ContextClassGenerationId, InputClosureTarget)
sourceInputClosureScope source = case (source.cancellation, alignmentSubscribeSourceProof source.subscribe) of
  (Nothing, BootstrapProof generation _)
    | predecessor /= generation -> Just (generation, (predecessor, alignmentSubscribeSourceStoreIncarnation source.subscribe))
  _ -> Nothing
  where
    predecessor = alignmentObligationSourceGeneration (alignmentSubscribeObligation source.subscribe)

retainInputClosureSource :: AlignmentSubscriptionId -> SourceSubscription -> State -> State
retainInputClosureSource identifier source state = case sourceInputClosureScope source of
  Nothing -> state
  Just (generation, target) ->
    let retained =
          state
            { predecessorSourcesByChild = insertBucket generation identifier state.predecessorSourcesByChild,
              predecessorSourcesByTarget = insertBucket target identifier state.predecessorSourcesByTarget
            }
     in if source.liveReleased
          then retained
          else
            retained
              { eligibleInputClosureSources = Set.insert identifier retained.eligibleInputClosureSources,
                pendingInputClosureSources = Set.insert identifier retained.pendingInputClosureSources
              }

retireInputClosureWork :: AlignmentSubscriptionId -> State -> State
retireInputClosureWork identifier state =
  state
    { eligibleInputClosureSources = Set.delete identifier state.eligibleInputClosureSources,
      pendingInputClosureSources = Set.delete identifier state.pendingInputClosureSources
    }

retireInputClosureSource :: AlignmentSubscriptionId -> SourceSubscription -> State -> State
retireInputClosureSource identifier source state = case sourceInputClosureScope source of
  Nothing -> state
  Just (generation, target) ->
    (retireInputClosureWork identifier state)
      { predecessorSourcesByChild = deleteBucket generation identifier state.predecessorSourcesByChild,
        predecessorSourcesByTarget = deleteBucket target identifier state.predecessorSourcesByTarget
      }

insertBucket :: (Ord key, Ord value) => key -> value -> Map key (Set value) -> Map key (Set value)
insertBucket key value = Map.insertWith Set.union key (Set.singleton value)

deleteBucket :: (Ord key, Ord value) => key -> value -> Map key (Set value) -> Map key (Set value)
deleteBucket key value = Map.update remove key
  where
    remove values =
      let remaining = Set.delete value values
       in if Set.null remaining then Nothing else Just remaining

destinationInputClosureTargets :: DestinationSubscription -> Set InputClosureTarget
destinationInputClosureTargets destination =
  Set.fromList
    [ (alignmentObligationDestinationGeneration obligation, destinationStoreIncarnation store)
    | store <- NonEmpty.toList (alignmentObligationDestinationStores obligation)
    ]
  where
    obligation = alignmentSubscribeObligation destination.subscribe

destinationAvailableForInputClosure :: DestinationSubscription -> Bool
destinationAvailableForInputClosure destination = case destination.cancellation of
  Nothing -> True
  Just cancellation ->
    alignmentCancelReason cancellation == AlignmentRelationRemoved
      && case (destination.appliedThrough, destination.liveThrough, destination.acknowledgedThrough) of
        (Just applied, Just live, Just acknowledged) -> applied >= live && acknowledged >= live
        _ -> False

destinationInputClosureProgress :: DestinationSubscription -> (Maybe StoreRevision, Maybe StoreRevision, Maybe StoreRevision, Maybe AlignmentCancel)
destinationInputClosureProgress destination =
  (destination.appliedThrough, destination.liveThrough, destination.acknowledgedThrough, destination.cancellation)

patchInputClosureDestination :: AlignmentSubscriptionId -> Maybe DestinationSubscription -> DestinationSubscription -> State -> State
patchInputClosureDestination identifier previous successor state
  | fmap destinationInputClosureProgress previous == Just (destinationInputClosureProgress successor) = state
  | otherwise =
      let changed = wakeInputClosureGenerations generations (wakeInputClosureTargets targets indexed)
       in identifier `seq` changed {destinationProgressChanges = Set.insert identifier changed.destinationProgressChanges}
  where
    targets = destinationInputClosureTargets successor
    generations = Set.map fst (Map.findWithDefault Set.empty identifier state.inputClosureBarrierKeysBySubscription)
    wasAvailable = maybe False destinationAvailableForInputClosure previous
    available = destinationAvailableForInputClosure successor
    owner = alignmentSubscribeObligationId successor.subscribe
    indexed
      | wasAvailable == available = state
      | otherwise =
          state
            { availableClosureDestinationsByOwner = change owner identifier state.availableClosureDestinationsByOwner,
              ordinaryClosureDestinationsByTarget = case successor.purpose of
                BootstrapDestination _ -> state.ordinaryClosureDestinationsByTarget
                ContextDestination _ -> Set.foldl' (\index target -> change target identifier index) state.ordinaryClosureDestinationsByTarget targets
            }
    change :: (Ord key, Ord value) => key -> value -> Map key (Set value) -> Map key (Set value)
    change = if available then insertBucket else deleteBucket

-- | One consumer receives only destination prefix/lifetime transitions. Held
-- bytes and exact frame retries do not change readiness or fulfillment truth.
takeDestinationProgressChanges :: State -> (Set AlignmentSubscriptionId, State)
takeDestinationProgressChanges state =
  let identifiers = state.destinationProgressChanges
   in identifiers `seq` (identifiers, clearDestinationProgressChanges state)

-- | Comparison-only erasure, also used by the unique journal consumer.
clearDestinationProgressChanges :: State -> State
clearDestinationProgressChanges state = state {destinationProgressChanges = Set.empty}

-- | Apply one changed barrier and its compact reverse projections. Comparing
-- this single immutable transcript never scans retained barrier history.
setInputClosureBarrier :: InputClosureBarrierKey -> Maybe (AlignmentSubscribe, Maybe StoreRevision) -> State -> (State, Bool)
setInputClosureBarrier key@(generation, owner) replacement state
  | previous == replacement = (state, False)
  | otherwise = (wakeInputClosureGenerations (Set.singleton generation) installed, True)
  where
    previous = Map.lookup key state.predecessorInputClosures
    removed = case previous of
      Nothing -> state
      Just (subscribe, witnessed) ->
        state
          { inputClosureBarrierKeysBySubscription = deleteBucket (alignmentSubscribeSubscriptionId subscribe) key state.inputClosureBarrierKeysBySubscription,
            completedInputClosureOwnersByChild = case witnessed of
              Nothing -> state.completedInputClosureOwnersByChild
              Just _ -> deleteBucket generation owner state.completedInputClosureOwnersByChild
          }
    installed = case replacement of
      Nothing -> removed {predecessorInputClosures = Map.delete key state.predecessorInputClosures}
      Just value@(subscribe, witnessed) ->
        removed
          { predecessorInputClosures = Map.insert key value state.predecessorInputClosures,
            inputClosureBarrierKeysBySubscription = insertBucket (alignmentSubscribeSubscriptionId subscribe) key removed.inputClosureBarrierKeysBySubscription,
            completedInputClosureOwnersByChild = case witnessed of
              Nothing -> removed.completedInputClosureOwnersByChild
              Just _ -> insertBucket generation owner removed.completedInputClosureOwnersByChild
          }

lookupSourceSubscription ::
  AlignmentSubscriptionId ->
  State ->
  Maybe SourceSubscription
lookupSourceSubscription identifier state = Map.lookup identifier state.sources

sourceRejectionEntries ::
  State -> [(AlignmentSubscriptionId, SourceRejection)]
sourceRejectionEntries state = Map.toAscList state.sourceRejections

destinationSubscriptionEntries ::
  State -> [(AlignmentSubscriptionId, DestinationSubscription)]
destinationSubscriptionEntries state = Map.toAscList state.destinations

lookupDestinationSubscription ::
  AlignmentSubscriptionId ->
  State ->
  Maybe DestinationSubscription
lookupDestinationSubscription identifier state =
  Map.lookup identifier state.destinations

routeCutoverEntries ::
  State ->
  [ ( (ContextClassGenerationId, HeraldEpoch, HeraldEpoch),
      AlignmentRouteCutoverMarker
    )
  ]
routeCutoverEntries state = Map.toAscList state.routeCutovers

lookupRouteCutover ::
  ContextClassGenerationId -> HeraldEpoch -> HeraldEpoch -> State -> Maybe AlignmentRouteCutoverMarker
lookupRouteCutover generation source predecessor state =
  Map.lookup (generation, source, predecessor) state.routeCutovers

routeCutoverAssignmentEntries ::
  State ->
  [ ( (ContextClassGenerationId, HeraldEpoch, HeraldEpoch),
      AssignmentReceipt
    )
  ]
routeCutoverAssignmentEntries state = Map.toAscList state.routeCutoverAssignments

localRoutePrefixSettlementEntries ::
  State -> [(ContextClassGenerationId, HeraldEpoch)]
localRoutePrefixSettlementEntries state =
  Set.toAscList state.localRoutePrefixSettlements

localRoutePrefixSettled :: ContextClassGenerationId -> HeraldEpoch -> State -> Bool
localRoutePrefixSettled generation predecessor state =
  Set.member (generation, predecessor) state.localRoutePrefixSettlements

predecessorInputClosureEntries ::
  State ->
  [ ( (ContextClassGenerationId, AlignmentObligationId),
      (AlignmentSubscribe, Maybe StoreRevision)
    )
  ]
predecessorInputClosureEntries state =
  Map.toAscList state.predecessorInputClosures

lookupPredecessorInputClosure ::
  ContextClassGenerationId ->
  AlignmentObligationId ->
  State ->
  Maybe (AlignmentSubscribe, Maybe StoreRevision)
lookupPredecessorInputClosure generation owner state =
  Map.lookup (generation, owner) state.predecessorInputClosures

predecessorInputClosureReceiptEntries ::
  State ->
  [ ( (ContextClassGenerationId, StoreIncarnationId),
      PredecessorInputClosureReceipt
    )
  ]
predecessorInputClosureReceiptEntries state =
  Map.toAscList state.predecessorInputClosureReceipts

lookupPredecessorInputClosureReceipt :: ContextClassGenerationId -> StoreIncarnationId -> State -> Maybe PredecessorInputClosureReceipt
lookupPredecessorInputClosureReceipt generation sourceStore state =
  Map.lookup (generation, sourceStore) state.predecessorInputClosureReceipts

localMemberReadinessFulfillmentEntries ::
  State ->
  [ ( (ContextClassGenerationId, StoreIncarnationId),
      LocalMemberReadinessFulfillment
    )
  ]
localMemberReadinessFulfillmentEntries state =
  Map.toAscList state.localMemberReadinessFulfillments

appliedDestinationEvidenceEntries :: State -> [AppliedDestinationEvidence]
appliedDestinationEvidenceEntries state =
  Map.elems state.appliedDestinationEvidence

newtype PreparedAppliedDestinationEvidence
  = PreparedAppliedDestinationEvidence
      (Prepared State AlignmentTransferDisposition)

prepareAppliedDestinationEvidence ::
  [AppliedDestinationEvidence] ->
  State ->
  Either AlignmentTransferProblem PreparedAppliedDestinationEvidence
prepareAppliedDestinationEvidence supplied state = do
  mapM_ (validateAppliedDestinationEvidence state) supplied
  (retained, changed) <-
    foldM retainOne (state.appliedDestinationEvidence, False) supplied
  PreparedAppliedDestinationEvidence
    <$> prepareTransition
      ( \predecessor ->
          Right
            ( predecessor {appliedDestinationEvidence = retained},
              if changed
                then AlignmentTransferAdvanced
                else AlignmentTransferUnchanged
            )
      )
      state
  where
    retainOne (retained, changed) evidence =
      let key = appliedDestinationEvidenceKey evidence
       in case Map.lookup key retained of
            Nothing -> Right (Map.insert key evidence retained, True)
            Just incumbent
              | incumbent == evidence -> Right (retained, changed)
              | otherwise ->
                  Left
                    ( AlignmentTransferAppliedEvidenceConflict
                        evidence.subscription
                        (destinationStoreIncarnation evidence.destination)
                        (retainedPublicationEvidencePublicationId evidence.publication)
                        evidence.sourceRevision
                    )

commitAppliedDestinationEvidence ::
  PreparedAppliedDestinationEvidence ->
  (State, AlignmentTransferDisposition)
commitAppliedDestinationEvidence
  (PreparedAppliedDestinationEvidence prepared) = commitPrepared prepared

appliedDestinationEvidenceKey ::
  AppliedDestinationEvidence -> AppliedDestinationEvidenceKey
appliedDestinationEvidenceKey evidence =
  ( evidence.subscription,
    destinationStoreIncarnation evidence.destination,
    retainedPublicationEvidencePublicationId evidence.publication,
    evidence.sourceRevision,
    evidence.kind
  )

validateAppliedDestinationEvidence ::
  State ->
  AppliedDestinationEvidence ->
  Either AlignmentTransferProblem ()
validateAppliedDestinationEvidence state evidence = do
  destination <- requireDestination evidence.subscription state
  let obligation = alignmentSubscribeObligation destination.subscribe
      expectedStrength =
        min
          (retainedPublicationEvidenceSourceStrength evidence.publication)
          (alignmentObligationFrozenContextPathStrength obligation)
  if evidence.destination
    `elem` NonEmpty.toList (alignmentObligationDestinationStores obligation)
    && evidence.strength == expectedStrength
    && maybe False (>= evidence.sourceRevision) destination.appliedThrough
    && transcriptMatches destination evidence
    then Right ()
    else
      Left
        ( AlignmentTransferAppliedEvidenceMismatch
            evidence.subscription
            evidence.sourceRevision
        )

transcriptMatches :: DestinationSubscription -> AppliedDestinationEvidence -> Bool
transcriptMatches destination evidence = case evidence.kind of
  AppliedSnapshotRepresentative ->
    snapshotFactMatches retainedStateEvidenceRepresentative
  AppliedSnapshotStrengthWitness ->
    snapshotFactMatches retainedStateEvidenceStrengthWitness
  AppliedContinuingChange ->
    maybe
      False
      ( (== evidence.publication)
          . alignmentChangeRetainedTransition
      )
      (Map.lookup evidence.sourceRevision destination.appliedChanges)
  where
    snapshotFactMatches projection =
      destination.snapshot.appliedBase == Just evidence.sourceRevision
        && any
          ((== evidence.publication) . projection)
          snapshotFacts
    snapshotFacts =
      concatMap
        alignmentSnapshotChunkRetainedStates
        (Map.elems destination.snapshot.chunks)

data AlignmentTransferDisposition
  = AlignmentTransferRetained
  | AlignmentTransferHeld
  | AlignmentTransferAdvanced
  | AlignmentTransferUnchanged
  deriving stock (Eq, Ord, Show)

data SourceLiveMode
  = SourceLiveImmediate
  | SourceLiveWithheld
  deriving stock (Eq, Ord, Show)

data AlignmentTransferProblem
  = AlignmentTransferSubscriptionMissing AlignmentSubscriptionId
  | AlignmentTransferSubscriptionConflict AlignmentSubscriptionId
  | AlignmentTransferSourceProofMismatch
  | AlignmentTransferChangeConflict AlignmentSubscriptionId StoreRevision
  | AlignmentTransferChangeNotContiguous
      AlignmentSubscriptionId
      StoreRevision
      StoreRevision
  | AlignmentTransferLiveBeforeSent
      AlignmentSubscriptionId
      StoreRevision
      StoreRevision
  | AlignmentTransferAckBeyondSent
      AlignmentSubscriptionId
      StoreRevision
      StoreRevision
  | AlignmentTransferAckBeforeLiveRelease AlignmentSubscriptionId
  | AlignmentTransferSourceLiveClosureMissing AlignmentSubscriptionId
  | AlignmentTransferSnapshotConflict AlignmentSubscriptionId
  | AlignmentTransferSnapshotChunkOutOfRange
      AlignmentSubscriptionId
      Word64
      Word64
  | AlignmentTransferSnapshotDigestMismatch
      AlignmentSnapshotDigest
      AlignmentSnapshotDigest
  | AlignmentTransferSnapshotBaseMismatch StoreRevision StoreRevision
  | AlignmentTransferRouteCutoverConflict
      ContextClassGenerationId
      HeraldEpoch
      HeraldEpoch
  | AlignmentTransferInputClosureConflict
      ContextClassGenerationId
      AlignmentSubscriptionId
  | AlignmentTransferInputClosureReceiptConflict
      ContextClassGenerationId
      StoreIncarnationId
  | AlignmentTransferMemberReadinessConflict
      ContextClassGenerationId
      StoreIncarnationId
  | AlignmentTransferAppliedEvidenceMismatch AlignmentSubscriptionId StoreRevision
  | AlignmentTransferAppliedEvidenceConflict
      AlignmentSubscriptionId
      StoreIncarnationId
      PublicationId
      StoreRevision
  deriving stock (Eq, Show)

newtype PreparedSourceSubscription
  = PreparedSourceSubscription
      (Prepared State (AlignmentTransferDisposition, [AlignmentControl]))

prepareSourceSubscription ::
  AlignmentSubscribe ->
  StoreRevision ->
  [RetainedStateEvidence] ->
  SourceLiveMode ->
  State ->
  Either AlignmentTransferProblem PreparedSourceSubscription
prepareSourceSubscription subscribe base facts liveMode state = do
  generation <- sourceProofGeneration subscribe
  let identifier = alignmentSubscribeSubscriptionId subscribe
      sourceStore = alignmentSubscribeSourceStoreIncarnation subscribe
      canonicalFacts = sortOn retainedStateEvidenceCanonicalBytes facts
      digest =
        alignmentSemanticSnapshotDigest generation sourceStore base canonicalFacts
      candidate =
        SourceSubscription
          { subscribe,
            baseRevision = base,
            snapshotDigest = digest,
            snapshotFacts = canonicalFacts,
            sentChanges = Map.empty,
            sentThrough = base,
            acknowledgedThrough = Nothing,
            liveInitiallyWithheld = liveMode == SourceLiveWithheld,
            liveReleased = liveMode == SourceLiveImmediate,
            cancellation = Nothing
          }
  case Map.lookup identifier state.sourceRejections of
    Nothing -> Right ()
    Just _ -> Left (AlignmentTransferSubscriptionConflict identifier)
  case Map.lookup identifier state.sources of
    Just incumbent
      | sameSourceSubscription candidate incumbent ->
          PreparedSourceSubscription
            <$> prepareTransition
              ( \predecessor ->
                  Right
                    ( predecessor,
                      ( AlignmentTransferUnchanged,
                        sourceRetryControls identifier predecessor
                      )
                    )
              )
              state
      | otherwise -> Left (AlignmentTransferSubscriptionConflict identifier)
    Nothing ->
      PreparedSourceSubscription
        <$> prepareTransition
          ( \predecessor ->
              Right
                ( retainInputClosureSource
                    identifier
                    candidate
                    ( retainSourcePrefixWork
                        identifier
                        candidate
                        predecessor
                          { sources = Map.insert identifier candidate predecessor.sources,
                            sourceIds = Set.insert identifier predecessor.sourceIds
                          }
                    ),
                  (AlignmentTransferRetained, sourceAllControls candidate)
                )
          )
          state

sameSourceSubscription :: SourceSubscription -> SourceSubscription -> Bool
sameSourceSubscription expected observed =
  expected.subscribe == observed.subscribe
    && expected.baseRevision == observed.baseRevision
    && expected.snapshotDigest == observed.snapshotDigest
    && expected.snapshotFacts == observed.snapshotFacts
    && expected.liveInitiallyWithheld == observed.liveInitiallyWithheld

sourceProofGeneration ::
  AlignmentSubscribe -> Either AlignmentTransferProblem ContextClassGenerationId
sourceProofGeneration subscribe =
  case alignmentSubscribeSourceProof subscribe of
    FinalCertificate generation _ ->
      if generation
        == alignmentObligationSourceGeneration
          (alignmentSubscribeObligation subscribe)
        then Right generation
        else Left AlignmentTransferSourceProofMismatch
    BootstrapProof generation _ ->
      if generation
        == alignmentObligationDestinationGeneration
          (alignmentSubscribeObligation subscribe)
        then
          Right
            ( alignmentObligationSourceGeneration
                (alignmentSubscribeObligation subscribe)
            )
        else Left AlignmentTransferSourceProofMismatch

sourceSnapshotControls :: SourceSubscription -> [AlignmentControl]
sourceSnapshotControls source =
  [ AlignmentSnapshotStarted start,
    AlignmentSnapshotChunkTransferred chunk,
    AlignmentSnapshotEnded end
  ]
  where
    identifier = alignmentSubscribeSubscriptionId source.subscribe
    start =
      alignmentSnapshotStart
        identifier
        source.baseRevision
        source.snapshotDigest
        1
    chunk = alignmentSnapshotChunk identifier 0 source.snapshotFacts
    end =
      alignmentSnapshotEnd
        identifier
        source.baseRevision
        source.snapshotDigest

sourceAllControls :: SourceSubscription -> [AlignmentControl]
sourceAllControls source =
  sourceSnapshotControls source
    <> fmap AlignmentChangeTransferred (Map.elems source.sentChanges)
    <> sourceLiveControls source

sourceLiveControls :: SourceSubscription -> [AlignmentControl]
sourceLiveControls source
  | source.liveReleased =
      [ AlignmentLiveAdvertised
          (alignmentLive identifier source.sentThrough)
      ]
  | otherwise = []
  where
    identifier = alignmentSubscribeSubscriptionId source.subscribe

preparedSourceSubscriptionControls ::
  PreparedSourceSubscription -> [AlignmentControl]
preparedSourceSubscriptionControls (PreparedSourceSubscription prepared) =
  snd (preparedOutput prepared)

commitSourceSubscription ::
  PreparedSourceSubscription -> (State, AlignmentTransferDisposition)
commitSourceSubscription (PreparedSourceSubscription prepared) =
  let (state, (disposition, _)) = commitPrepared prepared
   in (state, disposition)

newtype PreparedSourceLiveRelease
  = PreparedSourceLiveRelease
      (Prepared State (AlignmentTransferDisposition, [AlignmentControl]))

prepareSourceLiveRelease ::
  ContextClassGenerationId ->
  StoreIncarnationId ->
  AlignmentSubscriptionId ->
  State ->
  Either AlignmentTransferProblem PreparedSourceLiveRelease
prepareSourceLiveRelease generation sourceStore identifier state = do
  source <- requireSource identifier state
  let closureKey = (generation, sourceStore)
  closure <-
    maybe
      (Left (AlignmentTransferSourceLiveClosureMissing identifier))
      Right
      (Map.lookup closureKey state.predecessorInputClosureReceipts)
  if alignmentSubscribeSourceStoreIncarnation source.subscribe == sourceStore
    && alignmentObligationDestinationGeneration
      (alignmentSubscribeObligation source.subscribe)
      == generation
    && source.sentThrough >= closure.sourceRevision
    && case alignmentSubscribeSourceProof source.subscribe of
      BootstrapProof retainedGeneration _ ->
        retainedGeneration == generation
          && alignmentObligationSourceGeneration
            (alignmentSubscribeObligation source.subscribe)
            /= generation
      FinalCertificate _ _ -> False
    then Right ()
    else Left (AlignmentTransferSourceLiveClosureMissing identifier)
  case source.cancellation of
    Just cancellation ->
      PreparedSourceLiveRelease
        <$> prepareTransition
          ( \predecessor ->
              Right
                ( predecessor,
                  (AlignmentTransferUnchanged, [AlignmentCancelled cancellation])
                )
          )
          state
    Nothing
      | source.liveReleased ->
          PreparedSourceLiveRelease
            <$> prepareTransition
              (\predecessor -> Right (predecessor, (AlignmentTransferUnchanged, [])))
              state
      | otherwise ->
          let successorSource = source {liveReleased = True}
           in PreparedSourceLiveRelease
                <$> prepareTransition
                  ( \predecessor ->
                      Right
                        ( retireInputClosureWork
                            identifier
                            predecessor
                              { sources =
                                  Map.insert identifier successorSource predecessor.sources
                              },
                          (AlignmentTransferAdvanced, sourceLiveControls successorSource)
                        )
                  )
                  state

preparedSourceLiveReleaseControls ::
  PreparedSourceLiveRelease -> [AlignmentControl]
preparedSourceLiveReleaseControls (PreparedSourceLiveRelease prepared) =
  snd (preparedOutput prepared)

commitSourceLiveRelease ::
  PreparedSourceLiveRelease -> (State, AlignmentTransferDisposition)
commitSourceLiveRelease (PreparedSourceLiveRelease prepared) =
  let (successor, (disposition, _)) = commitPrepared prepared
   in (successor, disposition)

newtype PreparedSourceChanges
  = PreparedSourceChanges
      (Prepared State (AlignmentTransferDisposition, [AlignmentControl]))

prepareSourceChanges ::
  AlignmentSubscriptionId ->
  [AlignmentChange] ->
  AlignmentLive ->
  State ->
  Either AlignmentTransferProblem PreparedSourceChanges
prepareSourceChanges identifier supplied live state = do
  source <- requireSource identifier state
  case source.cancellation of
    Just cancellation ->
      PreparedSourceChanges
        <$> prepareTransition
          ( \predecessor ->
              Right
                ( predecessor,
                  ( AlignmentTransferUnchanged,
                    [AlignmentCancelled cancellation]
                  )
                )
          )
          state
    Nothing -> prepareActiveSourceChanges identifier source supplied live state

prepareActiveSourceChanges ::
  AlignmentSubscriptionId ->
  SourceSubscription ->
  [AlignmentChange] ->
  AlignmentLive ->
  State ->
  Either AlignmentTransferProblem PreparedSourceChanges
prepareActiveSourceChanges identifier source supplied live state = do
  requireLiveIdentity identifier live
  let ordered = sortOn alignmentChangeStoreRevision supplied
  mapM_ (requireChangeIdentity identifier) ordered
  (changes, through) <- retainSourceChanges identifier source.sentThrough source.sentChanges ordered
  let liveRevision = alignmentLiveThroughSourceStoreRevision live
  if liveRevision == through
    then Right ()
    else Left (AlignmentTransferLiveBeforeSent identifier through liveRevision)
  let changed = changes /= source.sentChanges || through /= source.sentThrough
      successorSource =
        source
          { sentChanges = changes,
            sentThrough = through
          }
      controls =
        fmap AlignmentChangeTransferred ordered
          <> if source.liveReleased then [AlignmentLiveAdvertised live] else []
      disposition = if changed then AlignmentTransferAdvanced else AlignmentTransferUnchanged
  PreparedSourceChanges
    <$> prepareTransition
      ( \predecessor ->
          Right
            ( predecessor
                { sources = Map.insert identifier successorSource predecessor.sources
                },
              (disposition, controls)
            )
      )
      state

retainSourceChanges ::
  AlignmentSubscriptionId ->
  StoreRevision ->
  Map StoreRevision AlignmentChange ->
  [AlignmentChange] ->
  Either
    AlignmentTransferProblem
    (Map StoreRevision AlignmentChange, StoreRevision)
retainSourceChanges identifier = go
  where
    go through retained [] = Right (retained, through)
    go through retained (change : remaining) =
      let revision = alignmentChangeStoreRevision change
       in case Map.lookup revision retained of
            Just incumbent
              | incumbent == change -> go (max through revision) retained remaining
              | otherwise -> Left (AlignmentTransferChangeConflict identifier revision)
            Nothing ->
              let expected = nextStoreRevision through
               in if revision == expected
                    then go revision (Map.insert revision change retained) remaining
                    else
                      Left
                        (AlignmentTransferChangeNotContiguous identifier expected revision)

preparedSourceChangeControls :: PreparedSourceChanges -> [AlignmentControl]
preparedSourceChangeControls (PreparedSourceChanges prepared) =
  snd (preparedOutput prepared)

commitSourceChanges ::
  PreparedSourceChanges -> (State, AlignmentTransferDisposition)
commitSourceChanges (PreparedSourceChanges prepared) =
  let (state, (disposition, _)) = commitPrepared prepared
   in (state, disposition)

newtype PreparedSourceAcknowledgement
  = PreparedSourceAcknowledgement
      (Prepared State AlignmentTransferDisposition)

prepareSourceAcknowledgement ::
  AlignmentAck ->
  State ->
  Either AlignmentTransferProblem PreparedSourceAcknowledgement
prepareSourceAcknowledgement acknowledgement state = do
  let identifier = alignmentAckSubscriptionId acknowledgement
      revision = alignmentAckAppliedSourceStoreRevision acknowledgement
  source <- requireSource identifier state
  case source.cancellation of
    Just _ ->
      PreparedSourceAcknowledgement
        <$> prepareTransition
          (\predecessor -> Right (predecessor, AlignmentTransferUnchanged))
          state
    Nothing -> prepareActiveSourceAcknowledgement identifier revision source state

prepareActiveSourceAcknowledgement ::
  AlignmentSubscriptionId ->
  StoreRevision ->
  SourceSubscription ->
  State ->
  Either AlignmentTransferProblem PreparedSourceAcknowledgement
prepareActiveSourceAcknowledgement identifier revision source state = do
  if source.liveReleased
    then Right ()
    else Left (AlignmentTransferAckBeforeLiveRelease identifier)
  if revision <= source.sentThrough
    then Right ()
    else Left (AlignmentTransferAckBeyondSent identifier source.sentThrough revision)
  let joined = maybe revision (max revision) source.acknowledgedThrough
      disposition =
        if source.acknowledgedThrough == Just joined
          then AlignmentTransferUnchanged
          else AlignmentTransferAdvanced
      successorSource = setSourceAcknowledgedThrough (Just joined) source
  PreparedSourceAcknowledgement
    <$> prepareTransition
      ( \predecessor ->
          Right
            ( predecessor
                { sources = Map.insert identifier successorSource predecessor.sources
                },
              disposition
            )
      )
      state

commitSourceAcknowledgement ::
  PreparedSourceAcknowledgement -> (State, AlignmentTransferDisposition)
commitSourceAcknowledgement (PreparedSourceAcknowledgement prepared) =
  commitPrepared prepared

newtype PreparedSourceRejection
  = PreparedSourceRejection
      (Prepared State (AlignmentTransferDisposition, [AlignmentControl]))

prepareSourceRejection ::
  AlignmentSubscribe ->
  AlignmentCancelReason ->
  State ->
  Either AlignmentTransferProblem PreparedSourceRejection
prepareSourceRejection subscribe reason state = do
  _ <- sourceProofGeneration subscribe
  let identifier = alignmentSubscribeSubscriptionId subscribe
      cancellation = alignmentCancel identifier reason
      candidate = SourceRejection {subscribe, cancellation}
  case Map.lookup identifier state.sources of
    Just _ -> Left (AlignmentTransferSubscriptionConflict identifier)
    Nothing -> case Map.lookup identifier state.sourceRejections of
      Just incumbent
        | incumbent == candidate ->
            PreparedSourceRejection
              <$> prepareTransition
                ( \predecessor ->
                    Right
                      ( predecessor,
                        ( AlignmentTransferUnchanged,
                          [AlignmentCancelled incumbent.cancellation]
                        )
                      )
                )
                state
        | otherwise -> Left (AlignmentTransferSubscriptionConflict identifier)
      Nothing ->
        PreparedSourceRejection
          <$> prepareTransition
            ( \predecessor ->
                Right
                  ( predecessor
                      { sourceRejections =
                          Map.insert
                            identifier
                            candidate
                            predecessor.sourceRejections
                      },
                    ( AlignmentTransferRetained,
                      [AlignmentCancelled cancellation]
                    )
                  )
            )
            state

preparedSourceRejectionControls ::
  PreparedSourceRejection -> [AlignmentControl]
preparedSourceRejectionControls (PreparedSourceRejection prepared) =
  snd (preparedOutput prepared)

commitSourceRejection ::
  PreparedSourceRejection -> (State, AlignmentTransferDisposition)
commitSourceRejection (PreparedSourceRejection prepared) =
  let (state, (disposition, _)) = commitPrepared prepared
   in (state, disposition)

sourceRetryControls :: AlignmentSubscriptionId -> State -> [AlignmentControl]
sourceRetryControls identifier state =
  case Map.lookup identifier state.sources of
    Nothing ->
      maybe
        []
        (pure . AlignmentCancelled . sourceRejectionCancellation)
        (Map.lookup identifier state.sourceRejections)
    Just source -> case source.cancellation of
      Just cancellation -> [AlignmentCancelled cancellation]
      Nothing -> sourcePendingControls source

-- | Retry only the part of the retained source transcript that the destination
-- has not acknowledged.  Before the first acknowledgement the snapshot is
-- still needed; afterwards any acknowledgement proves that the snapshot and
-- the prefix through that revision were applied.  A released Live marker is
-- repeated while it names a revision beyond that acknowledged prefix so a
-- lost acknowledgement can still be recovered.
sourcePendingControls :: SourceSubscription -> [AlignmentControl]
sourcePendingControls source = case source.acknowledgedThrough of
  Nothing -> sourceAllControls source
  Just acknowledged ->
    fmap AlignmentChangeTransferred pendingChanges
      <> pendingLive
    where
      pendingChanges =
        [ transferred
        | (revision, transferred) <- Map.toAscList source.sentChanges,
          revision > acknowledged
        ]
      pendingLive
        | source.liveReleased && acknowledged < source.sentThrough =
            [ AlignmentLiveAdvertised
                (alignmentLive identifier source.sentThrough)
            ]
        | otherwise = []
      identifier = alignmentSubscribeSubscriptionId source.subscribe

data PreparedDestinationAttempt = PreparedDestinationAttempt
  { preparedState :: Prepared State AlignmentTransferDisposition,
    subscribe :: AlignmentSubscribe
  }

prepareDestinationAttempt ::
  AlignmentAttempt ->
  State ->
  Either AlignmentTransferProblem PreparedDestinationAttempt
prepareDestinationAttempt attempt state = do
  let identifier = alignmentAttemptSubscriptionId attempt
  subscribe <- destinationSubscribe attempt
  let
    candidate =
      DestinationSubscription
        { purpose = ContextDestination attempt,
          subscribe,
          snapshot = emptySnapshotAssembly,
          heldChanges = Map.empty,
          appliedChanges = Map.empty,
          appliedThrough = Nothing,
          liveThrough = Nothing,
          acknowledgedThrough = Nothing,
          cancellation = Nothing
        }
  case Map.lookup identifier state.destinations of
    Just incumbent
      | incumbent.purpose == ContextDestination attempt ->
          PreparedDestinationAttempt
            <$> prepareTransition
              (\predecessor -> Right (predecessor, AlignmentTransferUnchanged))
              state
            <*> pure incumbent.subscribe
      | otherwise -> Left (AlignmentTransferSubscriptionConflict identifier)
    Nothing ->
      PreparedDestinationAttempt
        <$> prepareTransition
          ( \predecessor ->
              Right
                ( patchInputClosureDestination
                    identifier
                    Nothing
                    candidate
                    predecessor
                      { destinations =
                          Map.insert identifier candidate predecessor.destinations
                      },
                  AlignmentTransferRetained
                )
          )
          state
        <*> pure subscribe

destinationSubscribe ::
  AlignmentAttempt -> Either AlignmentTransferProblem AlignmentSubscribe
destinationSubscribe attempt =
  case alignmentSubscribe
    (alignmentAttemptObligation attempt)
    (alignmentAttemptSubscriptionId attempt)
    (historicalCertificateSourceStoreIncarnation certificate)
    ( FinalCertificate
        (historicalCertificateClassGeneration certificate)
        (alignmentAttemptHistoricalCertificateDigest attempt)
    ) of
    Left _ -> Left AlignmentTransferSourceProofMismatch
    Right subscribe -> Right subscribe
  where
    certificate = alignmentAttemptHistoricalCertificate attempt

preparedDestinationAttemptSubscribe ::
  PreparedDestinationAttempt -> AlignmentSubscribe
preparedDestinationAttemptSubscribe prepared = prepared.subscribe

commitDestinationAttempt ::
  PreparedDestinationAttempt -> (State, AlignmentTransferDisposition)
commitDestinationAttempt prepared = commitPrepared prepared.preparedState

data PreparedDestinationBootstrap = PreparedDestinationBootstrap
  { preparedState :: Prepared State AlignmentTransferDisposition,
    subscribe :: AlignmentSubscribe
  }

-- | Retain a destination-owned exceptional pre-certificate subscription.
-- Its existing 'BootstrapProof' authenticates the admitted new generation;
-- the envelope's semantic source generation remains the snapshot identity.
prepareDestinationBootstrap ::
  HeraldEpoch ->
  AlignmentSubscribe ->
  State ->
  Either AlignmentTransferProblem PreparedDestinationBootstrap
prepareDestinationBootstrap sourceHerald subscribe state = do
  case alignmentSubscribeSourceProof subscribe of
    BootstrapProof _ _ -> Right ()
    FinalCertificate _ _ -> Left AlignmentTransferSourceProofMismatch
  _ <- sourceProofGeneration subscribe
  let identifier = alignmentSubscribeSubscriptionId subscribe
      purpose = BootstrapDestination sourceHerald
      candidate =
        DestinationSubscription
          { purpose,
            subscribe,
            snapshot = emptySnapshotAssembly,
            heldChanges = Map.empty,
            appliedChanges = Map.empty,
            appliedThrough = Nothing,
            liveThrough = Nothing,
            acknowledgedThrough = Nothing,
            cancellation = Nothing
          }
  case Map.lookup identifier state.destinations of
    Just incumbent
      | incumbent.purpose == purpose && incumbent.subscribe == subscribe ->
          PreparedDestinationBootstrap
            <$> prepareTransition
              (\predecessor -> Right (predecessor, AlignmentTransferUnchanged))
              state
            <*> pure incumbent.subscribe
      | otherwise -> Left (AlignmentTransferSubscriptionConflict identifier)
    Nothing ->
      PreparedDestinationBootstrap
        <$> prepareTransition
          ( \predecessor ->
              Right
                ( patchInputClosureDestination
                    identifier
                    Nothing
                    candidate
                    predecessor
                      { destinations =
                          Map.insert identifier candidate predecessor.destinations
                      },
                  AlignmentTransferRetained
                )
          )
          state
        <*> pure subscribe

preparedDestinationBootstrapSubscribe ::
  PreparedDestinationBootstrap -> AlignmentSubscribe
preparedDestinationBootstrapSubscribe prepared = prepared.subscribe

commitDestinationBootstrap ::
  PreparedDestinationBootstrap -> (State, AlignmentTransferDisposition)
commitDestinationBootstrap prepared = commitPrepared prepared.preparedState

newtype PreparedRouteCutoverMarker
  = PreparedRouteCutoverMarker (Prepared State AlignmentTransferDisposition)

prepareRouteCutoverMarker ::
  AlignmentRouteCutoverMarker ->
  State ->
  Either AlignmentTransferProblem PreparedRouteCutoverMarker
prepareRouteCutoverMarker marker state =
  case Map.lookup key state.routeCutovers of
    Just incumbent
      | incumbent == marker ->
          PreparedRouteCutoverMarker <$> prepareTransition unchanged state
      | otherwise ->
          Left
            ( AlignmentTransferRouteCutoverConflict
                (alignmentRouteCutoverMarkerGeneration marker)
                (alignmentRouteCutoverMarkerSourceHerald marker)
                (alignmentRouteCutoverMarkerPredecessorHerald marker)
            )
    Nothing ->
      PreparedRouteCutoverMarker
        <$> prepareTransition
          ( \predecessor ->
              Right
                ( wakeInputClosureGenerations
                    (Set.singleton (alignmentRouteCutoverMarkerGeneration marker))
                    predecessor
                      { routeCutovers =
                          Map.insert key marker predecessor.routeCutovers
                      },
                  AlignmentTransferRetained
                )
          )
          state
  where
    key =
      ( alignmentRouteCutoverMarkerGeneration marker,
        alignmentRouteCutoverMarkerSourceHerald marker,
        alignmentRouteCutoverMarkerPredecessorHerald marker
      )
    unchanged predecessor = Right (predecessor, AlignmentTransferUnchanged)

commitRouteCutoverMarker ::
  PreparedRouteCutoverMarker -> (State, AlignmentTransferDisposition)
commitRouteCutoverMarker (PreparedRouteCutoverMarker prepared) =
  commitPrepared prepared

newtype PreparedRouteCutoverAssignments
  = PreparedRouteCutoverAssignments
      (Prepared State AlignmentTransferDisposition)

prepareRouteCutoverAssignments ::
  [(AlignmentRouteCutoverMarker, AssignmentReceipt)] ->
  State ->
  Either AlignmentTransferProblem PreparedRouteCutoverAssignments
prepareRouteCutoverAssignments supplied state = do
  retained <- foldM retainOne state.routeCutoverAssignments supplied
  PreparedRouteCutoverAssignments
    <$> prepareTransition
      ( \predecessor ->
          Right
            ( predecessor {routeCutoverAssignments = retained},
              if retained == predecessor.routeCutoverAssignments
                then AlignmentTransferUnchanged
                else AlignmentTransferRetained
            )
      )
      state
  where
    retainOne retained (marker, receipt) = do
      let key = routeCutoverMarkerKey marker
      if Map.lookup key state.routeCutovers == Just marker
        && routeCutoverAssignmentMatches marker receipt
        then Right ()
        else Left (routeCutoverConflict marker)
      case Map.lookup key retained of
        Nothing -> Right (Map.insert key receipt retained)
        Just incumbent
          | incumbent == receipt -> Right retained
          | otherwise -> Left (routeCutoverConflict marker)

commitRouteCutoverAssignments ::
  PreparedRouteCutoverAssignments ->
  (State, AlignmentTransferDisposition)
commitRouteCutoverAssignments (PreparedRouteCutoverAssignments prepared) =
  commitPrepared prepared

newtype PreparedLocalRoutePrefixSettlement
  = PreparedLocalRoutePrefixSettlement
      (Prepared State AlignmentTransferDisposition)

prepareLocalRoutePrefixSettlement ::
  ContextClassGenerationId ->
  HeraldEpoch ->
  State ->
  Either AlignmentTransferProblem PreparedLocalRoutePrefixSettlement
prepareLocalRoutePrefixSettlement generation predecessor state =
  PreparedLocalRoutePrefixSettlement
    <$> prepareTransition
      ( \incumbent ->
          let key = (generation, predecessor)
           in if Set.member key incumbent.localRoutePrefixSettlements
                then Right (incumbent, AlignmentTransferUnchanged)
                else
                  Right
                    ( wakeInputClosureGenerations
                        (Set.singleton generation)
                        incumbent
                          { localRoutePrefixSettlements =
                              Set.insert key incumbent.localRoutePrefixSettlements
                          },
                      AlignmentTransferRetained
                    )
      )
      state

commitLocalRoutePrefixSettlement ::
  PreparedLocalRoutePrefixSettlement ->
  (State, AlignmentTransferDisposition)
commitLocalRoutePrefixSettlement
  (PreparedLocalRoutePrefixSettlement prepared) = commitPrepared prepared

data PreparedPredecessorInputClosures = PreparedPredecessorInputClosures
  { preparedState :: Prepared State AlignmentTransferDisposition,
    subscribes :: [AlignmentSubscribe]
  }

preparePredecessorInputClosures ::
  ContextClassGenerationId ->
  Set AlignmentSubscriptionId ->
  State ->
  Either AlignmentTransferProblem PreparedPredecessorInputClosures
preparePredecessorInputClosures generation identifiers state = do
  (retained, reoffers, changed) <- foldM retainOne (state, [], False) (Set.toAscList identifiers)
  preparedState <-
    prepareTransition
      (\_ -> Right (retained, if changed then AlignmentTransferRetained else AlignmentTransferUnchanged))
      state
  Right PreparedPredecessorInputClosures {preparedState, subscribes = reoffers}
  where
    retainOne (retained, reoffers, changed) identifier = do
      destination <- requireDestination identifier state
      let subscribe = destination.subscribe
          key = (generation, alignmentSubscribeObligationId subscribe)
          install witness offered =
            let (successor, admitted) = setInputClosureBarrier key (Just (subscribe, witness)) retained
             in Right (successor, reoffers <> offered, changed || admitted)
      case Map.lookup key retained.predecessorInputClosures of
        Nothing -> case reusableTerminalWitness destination of
          Just through -> install (Just through) []
          Nothing -> do
            requireActive destination identifier
            install Nothing [subscribe]
        Just (_, Just _) -> Right (retained, reoffers, changed)
        Just (incumbent, Nothing)
          | incumbent == subscribe -> do
              requireActive destination identifier
              Right (retained, reoffers, changed)
          | otherwise ->
              let incumbentIdentifier = alignmentSubscribeSubscriptionId incumbent
                  incumbentCancelled = maybe True ((/= Nothing) . (.cancellation)) (Map.lookup incumbentIdentifier state.destinations)
               in if incumbentCancelled
                    then do
                      requireActive destination identifier
                      install Nothing [subscribe]
                    else Left (AlignmentTransferInputClosureConflict generation identifier)

    requireActive destination identifier
      | destination.cancellation == Nothing = Right ()
      | otherwise = Left (AlignmentTransferInputClosureConflict generation identifier)

    reusableTerminalWitness destination = do
      cancellation <- destination.cancellation
      if alignmentCancelReason cancellation == AlignmentRelationRemoved then Just () else Nothing
      applied <- destination.appliedThrough
      live <- destination.liveThrough
      acknowledged <- destination.acknowledgedThrough
      if applied >= live && acknowledged >= live then Just live else Nothing

preparedPredecessorInputClosureSubscribes ::
  PreparedPredecessorInputClosures -> [AlignmentSubscribe]
preparedPredecessorInputClosureSubscribes prepared = prepared.subscribes

commitPredecessorInputClosures ::
  PreparedPredecessorInputClosures ->
  (State, AlignmentTransferDisposition)
commitPredecessorInputClosures prepared = commitPrepared prepared.preparedState

newtype PreparedPredecessorInputClosureReceipt
  = PreparedPredecessorInputClosureReceipt
      (Prepared State AlignmentTransferDisposition)

preparePredecessorInputClosureReceipt ::
  ContextClassGenerationId ->
  HeraldEpoch ->
  StoreIncarnationId ->
  StoreRevision ->
  [MemberReadinessPrefix] ->
  State ->
  Either AlignmentTransferProblem PreparedPredecessorInputClosureReceipt
preparePredecessorInputClosureReceipt generation predecessor sourceStore revision supplied state = do
  let prefixes = sortOn memberReadinessPrefixSubscription supplied
      key = (generation, sourceStore)
      candidate =
        PredecessorInputClosureReceipt
          { generation,
            predecessorHerald = predecessor,
            sourceStore,
            sourceRevision = revision,
            prefixes
          }
  if Set.member (generation, predecessor) state.localRoutePrefixSettlements
    && length prefixes
      == Set.size (Set.fromList (fmap memberReadinessPrefixSubscription prefixes))
    && all prefixIsClosed prefixes
    then Right ()
    else Left (AlignmentTransferInputClosureReceiptConflict generation sourceStore)
  retained <- case Map.lookup key state.predecessorInputClosureReceipts of
    Nothing -> Right (Map.insert key candidate state.predecessorInputClosureReceipts)
    Just incumbent
      | incumbent == candidate -> Right state.predecessorInputClosureReceipts
      | otherwise -> Left (AlignmentTransferInputClosureReceiptConflict generation sourceStore)
  PreparedPredecessorInputClosureReceipt
    <$> prepareTransition
      ( \incumbent ->
          Right
            ( incumbent {predecessorInputClosureReceipts = retained},
              if retained == incumbent.predecessorInputClosureReceipts
                then AlignmentTransferUnchanged
                else AlignmentTransferRetained
            )
      )
      state
  where
    prefixIsClosed prefix =
      memberReadinessPrefixIsCovered prefix
        && case Map.lookup prefix.subscription state.destinations of
          Nothing -> False
          Just destination ->
            Map.lookup
              ( generation,
                alignmentSubscribeObligationId destination.subscribe
              )
              state.predecessorInputClosures
              == Just (destination.subscribe, Just prefix.live)
              && any
                ((== sourceStore) . destinationStoreIncarnation)
                ( NonEmpty.toList
                    ( alignmentObligationDestinationStores
                        (alignmentSubscribeObligation destination.subscribe)
                    )
                )
              && destination.appliedThrough == Just prefix.applied
              && maybe False (>= prefix.live) destination.liveThrough
              && destination.acknowledgedThrough == Just prefix.acknowledged

commitPredecessorInputClosureReceipt ::
  PreparedPredecessorInputClosureReceipt ->
  (State, AlignmentTransferDisposition)
commitPredecessorInputClosureReceipt
  (PreparedPredecessorInputClosureReceipt prepared) = commitPrepared prepared

newtype PreparedIncompletePredecessorInputClosureRemoval
  = PreparedIncompletePredecessorInputClosureRemoval
      (Prepared State AlignmentTransferDisposition)

-- | Remove executable closure barriers for logical owners retired by a plan
-- invalidation.  A barrier already named by an immutable closure receipt is
-- historical evidence and remains retained; a witnessed prefix without that
-- whole-owner receipt cannot outlive its plan coordinate.
prepareIncompletePredecessorInputClosureRemoval ::
  Set AlignmentObligationId ->
  State ->
  Either
    AlignmentTransferProblem
    PreparedIncompletePredecessorInputClosureRemoval
prepareIncompletePredecessorInputClosureRemoval owners state =
  PreparedIncompletePredecessorInputClosureRemoval
    <$> prepareTransition
      ( \predecessor ->
          let (retained, changed) = Map.foldlWithKey' removeOne (predecessor, False) predecessor.predecessorInputClosures
           in Right (retained, if changed then AlignmentTransferRetained else AlignmentTransferUnchanged)
      )
      state
  where
    removeOne (retained, changed) key@(generation, owner) (subscribe, _)
      | Set.notMember owner owners || predecessorInputClosureHasReceipt state generation subscribe = (retained, changed)
      | otherwise =
          let (successor, removed) = setInputClosureBarrier key Nothing retained
           in (successor, changed || removed)

-- | Terminal source cleanup checks only retained relations for the barrier's
-- child and predecessor targets. Live-released sources retain those relations.
-- Immutable whole-closure receipts continue to justify historical barriers.
removeOrphanedPredecessorInputClosures :: State -> State
removeOrphanedPredecessorInputClosures state =
  Map.foldlWithKey' removeOne state state.predecessorInputClosures
  where
    removeOne retained key@(generation, _) (subscribe, _)
      | predecessorInputClosureHasReceipt state generation subscribe
          || barrierHasRetainedSource key subscribe state =
          retained
      | otherwise = fst (setInputClosureBarrier key Nothing retained)

barrierHasRetainedSource :: InputClosureBarrierKey -> AlignmentSubscribe -> State -> Bool
barrierHasRetainedSource (generation, owner) subscribe state =
  alignmentSubscribeObligationId subscribe == owner
    && any hasSource (NonEmpty.toList (alignmentObligationDestinationStores obligation))
  where
    obligation = alignmentSubscribeObligation subscribe
    childSources = Map.findWithDefault Set.empty generation state.predecessorSourcesByChild
    hasSource store =
      not
        ( Set.disjoint
            childSources
            ( Map.findWithDefault
                Set.empty
                (alignmentObligationDestinationGeneration obligation, destinationStoreIncarnation store)
                state.predecessorSourcesByTarget
            )
        )

predecessorInputClosureHasReceipt ::
  State ->
  ContextClassGenerationId ->
  AlignmentSubscribe ->
  Bool
predecessorInputClosureHasReceipt state generation subscribe =
  any
    ( \receipt ->
        any
          ( (== alignmentSubscribeSubscriptionId subscribe)
              . memberReadinessPrefixSubscription
          )
          receipt.prefixes
    )
    [ receipt
    | destination <- NonEmpty.toList (alignmentObligationDestinationStores (alignmentSubscribeObligation subscribe)),
      Just receipt <- [Map.lookup (generation, destinationStoreIncarnation destination) state.predecessorInputClosureReceipts]
    ]

commitIncompletePredecessorInputClosureRemoval ::
  PreparedIncompletePredecessorInputClosureRemoval ->
  (State, AlignmentTransferDisposition)
commitIncompletePredecessorInputClosureRemoval
  (PreparedIncompletePredecessorInputClosureRemoval prepared) =
    commitPrepared prepared

routeCutoverMarkerKey ::
  AlignmentRouteCutoverMarker ->
  (ContextClassGenerationId, HeraldEpoch, HeraldEpoch)
routeCutoverMarkerKey marker =
  ( alignmentRouteCutoverMarkerGeneration marker,
    alignmentRouteCutoverMarkerSourceHerald marker,
    alignmentRouteCutoverMarkerPredecessorHerald marker
  )

routeCutoverConflict :: AlignmentRouteCutoverMarker -> AlignmentTransferProblem
routeCutoverConflict marker =
  AlignmentTransferRouteCutoverConflict
    (alignmentRouteCutoverMarkerGeneration marker)
    (alignmentRouteCutoverMarkerSourceHerald marker)
    (alignmentRouteCutoverMarkerPredecessorHerald marker)

routeCutoverAssignmentMatches ::
  AlignmentRouteCutoverMarker -> AssignmentReceipt -> Bool
routeCutoverAssignmentMatches marker receipt =
  let direction = assignmentReceiptDirection receipt
   in streamDirectionSource direction
        == alignmentRouteCutoverMarkerSourceHerald marker
        && streamDirectionDestination direction
          == alignmentRouteCutoverMarkerPredecessorHerald marker
        && assignmentReceiptDigest receipt
          == peerLogicalPayloadDigest (PeerLogicalRouteCutover marker)

newtype PreparedLocalMemberReadinessFulfillment
  = PreparedLocalMemberReadinessFulfillment
      (Prepared State AlignmentTransferDisposition)

prepareLocalMemberReadinessFulfillment ::
  ContextClassGenerationId ->
  DestinationStore ->
  StoreRevision ->
  [MemberReadinessPrefix] ->
  MemberReadyEvidenceDigest ->
  State ->
  Either AlignmentTransferProblem PreparedLocalMemberReadinessFulfillment
prepareLocalMemberReadinessFulfillment generation destination revision supplied digest state = do
  let prefixes = sortOn memberReadinessPrefixSubscription supplied
      key = (generation, destinationStoreIncarnation destination)
      candidate =
        LocalMemberReadinessFulfillment
          { generation,
            destination,
            storeRevision = revision,
            prefixes,
            digest
          }
  if not (null prefixes)
    && length prefixes
      == Set.size (Set.fromList (fmap memberReadinessPrefixSubscription prefixes))
    && all (readinessPrefixCurrentlyCovered destination state) prefixes
    then Right ()
    else Left (AlignmentTransferMemberReadinessConflict generation (destinationStoreIncarnation destination))
  case Map.lookup key state.localMemberReadinessFulfillments of
    Just incumbent
      | incumbent == candidate ->
          PreparedLocalMemberReadinessFulfillment
            <$> prepareTransition unchanged state
      | otherwise -> Left (AlignmentTransferMemberReadinessConflict generation (destinationStoreIncarnation destination))
    Nothing ->
      PreparedLocalMemberReadinessFulfillment
        <$> prepareTransition
          ( \predecessor ->
              Right
                ( predecessor
                    { localMemberReadinessFulfillments =
                        Map.insert
                          key
                          candidate
                          predecessor.localMemberReadinessFulfillments
                    },
                  AlignmentTransferRetained
                )
          )
          state
  where
    unchanged predecessor = Right (predecessor, AlignmentTransferUnchanged)

commitLocalMemberReadinessFulfillment ::
  PreparedLocalMemberReadinessFulfillment ->
  (State, AlignmentTransferDisposition)
commitLocalMemberReadinessFulfillment
  (PreparedLocalMemberReadinessFulfillment prepared) = commitPrepared prepared

readinessPrefixCurrentlyCovered ::
  DestinationStore -> State -> MemberReadinessPrefix -> Bool
readinessPrefixCurrentlyCovered memberDestination state prefix =
  memberReadinessPrefixIsCovered prefix
    && case Map.lookup prefix.subscription state.destinations of
      Nothing -> False
      Just destination ->
        memberDestination
          `elem` NonEmpty.toList
            ( alignmentObligationDestinationStores
                (alignmentSubscribeObligation destination.subscribe)
            )
          && case destination.cancellation of
            Nothing -> True
            Just cancellation ->
              alignmentCancelReason cancellation == AlignmentRelationRemoved
          && destination.appliedThrough == Just prefix.applied
          && destination.liveThrough == Just prefix.live
          && destination.acknowledgedThrough == Just prefix.acknowledged

memberReadinessPrefixIsCovered :: MemberReadinessPrefix -> Bool
memberReadinessPrefixIsCovered prefix =
  prefix.applied >= prefix.live
    && prefix.acknowledged >= prefix.live

data DestinationWork = DestinationWork
  { snapshot :: Maybe (StoreRevision, [RetainedStateEvidence]),
    changes :: [AlignmentChange],
    acknowledgement :: Maybe AlignmentAck
  }
  deriving stock (Eq, Show)

emptyDestinationWork :: DestinationWork
emptyDestinationWork = DestinationWork Nothing [] Nothing

destinationWorkSnapshot ::
  DestinationWork -> Maybe (StoreRevision, [RetainedStateEvidence])
destinationWorkSnapshot work = work.snapshot

destinationWorkChanges :: DestinationWork -> [AlignmentChange]
destinationWorkChanges work = work.changes

destinationWorkAcknowledgement :: DestinationWork -> Maybe AlignmentAck
destinationWorkAcknowledgement work = work.acknowledgement

newtype PreparedDestinationWork
  = PreparedDestinationWork
      (Prepared State (AlignmentTransferDisposition, DestinationWork))

prepareDestinationSnapshotStart ::
  AlignmentSnapshotStart ->
  State ->
  Either AlignmentTransferProblem PreparedDestinationWork
prepareDestinationSnapshotStart start =
  prepareDestinationMessage
    (alignmentSnapshotStartSubscriptionId start)
    Nothing
    (retainSnapshotStart start)

prepareDestinationSnapshotChunk ::
  AlignmentSnapshotChunk ->
  State ->
  Either AlignmentTransferProblem PreparedDestinationWork
prepareDestinationSnapshotChunk chunk =
  prepareDestinationMessage
    (alignmentSnapshotChunkSubscriptionId chunk)
    Nothing
    (retainSnapshotChunk chunk)

prepareDestinationSnapshotEnd ::
  AlignmentSnapshotEnd ->
  State ->
  Either AlignmentTransferProblem PreparedDestinationWork
prepareDestinationSnapshotEnd end =
  prepareDestinationMessage
    (alignmentSnapshotEndSubscriptionId end)
    Nothing
    (retainSnapshotEnd end)

prepareDestinationChange ::
  AlignmentChange ->
  State ->
  Either AlignmentTransferProblem PreparedDestinationWork
prepareDestinationChange change =
  prepareDestinationMessage
    (alignmentChangeSubscriptionId change)
    Nothing
    (retainDestinationChange change)

prepareDestinationLive ::
  AlignmentLive ->
  State ->
  Either AlignmentTransferProblem PreparedDestinationWork
prepareDestinationLive live =
  prepareDestinationMessage
    (alignmentLiveSubscriptionId live)
    (Just live)
    (retainDestinationLive live)

prepareDestinationMessage ::
  AlignmentSubscriptionId ->
  Maybe AlignmentLive ->
  (DestinationSubscription -> Either AlignmentTransferProblem DestinationSubscription) ->
  State ->
  Either AlignmentTransferProblem PreparedDestinationWork
prepareDestinationMessage identifier observedLive retain state = do
  destination <- requireDestination identifier state
  case destination.cancellation of
    Just _ ->
      PreparedDestinationWork
        <$> prepareTransition
          ( \predecessor ->
              Right
                ( predecessor,
                  (AlignmentTransferUnchanged, emptyDestinationWork)
                )
          )
          state
    Nothing ->
      prepareActiveDestinationMessage
        identifier
        observedLive
        retain
        destination
        state

prepareActiveDestinationMessage ::
  AlignmentSubscriptionId ->
  Maybe AlignmentLive ->
  (DestinationSubscription -> Either AlignmentTransferProblem DestinationSubscription) ->
  DestinationSubscription ->
  State ->
  Either AlignmentTransferProblem PreparedDestinationWork
prepareActiveDestinationMessage identifier observedLive retain destination state = do
  retained <- retain destination
  (successorDestination, work) <- advanceDestination identifier retained
  let (withClosures, closuresChanged) = maybe (state, False) (\live -> observeInputClosureLive identifier live state) observedLive
      successor =
        patchInputClosureDestination
          identifier
          (Just destination)
          successorDestination
          withClosures {destinations = Map.insert identifier successorDestination withClosures.destinations}
      disposition
        | successorDestination == destination && not closuresChanged && work == emptyDestinationWork = AlignmentTransferUnchanged
        | work == emptyDestinationWork = AlignmentTransferHeld
        | otherwise = AlignmentTransferAdvanced
  PreparedDestinationWork
    <$> prepareTransition (\_ -> Right (successor, (disposition, work))) state

observeInputClosureLive :: AlignmentSubscriptionId -> AlignmentLive -> State -> (State, Bool)
observeInputClosureLive identifier live state =
  Set.foldl'
    observe
    (state, False)
    (Map.findWithDefault Set.empty identifier state.inputClosureBarrierKeysBySubscription)
  where
    revision = alignmentLiveThroughSourceStoreRevision live
    observe (retained, changed) key = case Map.lookup key retained.predecessorInputClosures of
      Nothing -> (retained, changed)
      Just (subscribe, _)
        | predecessorInputClosureHasReceipt retained (fst key) subscribe ->
            -- A continuing carried stream may advance after this exact lower
            -- bound was sealed. Keep the immutable receipt's witness while its
            -- ordinary applied/live/acknowledged counters continue advancing.
            (retained, changed)
      Just (subscribe, witnessed) ->
        let (successor, advanced) = setInputClosureBarrier key (Just (subscribe, Just (maybe revision (max revision) witnessed))) retained
         in (successor, changed || advanced)

retainSnapshotStart ::
  AlignmentSnapshotStart ->
  DestinationSubscription ->
  Either AlignmentTransferProblem DestinationSubscription
retainSnapshotStart start destination = do
  let identifier = alignmentSnapshotStartSubscriptionId start
      snapshot = destination.snapshot
  case snapshot.start of
    Nothing -> do
      case Map.lookupGE (alignmentSnapshotStartChunkCount start) snapshot.chunks of
        Nothing -> Right ()
        Just (number, _) ->
          Left
            ( AlignmentTransferSnapshotChunkOutOfRange
                identifier
                (alignmentSnapshotStartChunkCount start)
                number
            )
      Right
        ( setDestinationSnapshot
            (setSnapshotStart (Just start) snapshot)
            destination
        )
    Just incumbent
      | incumbent == start -> Right destination
      | otherwise -> Left (AlignmentTransferSnapshotConflict identifier)

retainSnapshotChunk ::
  AlignmentSnapshotChunk ->
  DestinationSubscription ->
  Either AlignmentTransferProblem DestinationSubscription
retainSnapshotChunk chunk destination = do
  let identifier = alignmentSnapshotChunkSubscriptionId chunk
      number = alignmentSnapshotChunkNumber chunk
      snapshot = destination.snapshot
  case snapshot.start of
    Just start
      | number >= alignmentSnapshotStartChunkCount start ->
          Left
            ( AlignmentTransferSnapshotChunkOutOfRange
                identifier
                (alignmentSnapshotStartChunkCount start)
                number
            )
    _ -> Right ()
  case Map.lookup number snapshot.chunks of
    Nothing ->
      Right
        ( setDestinationSnapshot
            (setSnapshotChunks (Map.insert number chunk snapshot.chunks) snapshot)
            destination
        )
    Just incumbent
      | incumbent == chunk -> Right destination
      | otherwise -> Left (AlignmentTransferSnapshotConflict identifier)

retainSnapshotEnd ::
  AlignmentSnapshotEnd ->
  DestinationSubscription ->
  Either AlignmentTransferProblem DestinationSubscription
retainSnapshotEnd end destination = do
  let identifier = alignmentSnapshotEndSubscriptionId end
      snapshot = destination.snapshot
  case snapshot.end of
    Nothing ->
      Right
        ( setDestinationSnapshot
            (setSnapshotEnd (Just end) snapshot)
            destination
        )
    Just incumbent
      | incumbent == end -> Right destination
      | otherwise -> Left (AlignmentTransferSnapshotConflict identifier)

retainDestinationChange ::
  AlignmentChange ->
  DestinationSubscription ->
  Either AlignmentTransferProblem DestinationSubscription
retainDestinationChange change destination = do
  let identifier = alignmentChangeSubscriptionId change
      revision = alignmentChangeStoreRevision change
  case Map.lookup revision destination.appliedChanges of
    Just incumbent
      | incumbent == change -> Right destination
      | otherwise -> Left (AlignmentTransferChangeConflict identifier revision)
    Nothing -> case Map.lookup revision destination.heldChanges of
      Just incumbent
        | incumbent == change -> Right destination
        | otherwise -> Left (AlignmentTransferChangeConflict identifier revision)
      Nothing ->
        case destination.appliedThrough of
          Just through
            | revision <= through ->
                Left
                  ( AlignmentTransferChangeNotContiguous
                      identifier
                      (nextStoreRevision through)
                      revision
                  )
          _ ->
            Right
              destination
                { heldChanges = Map.insert revision change destination.heldChanges
                }

retainDestinationLive ::
  AlignmentLive ->
  DestinationSubscription ->
  Either AlignmentTransferProblem DestinationSubscription
retainDestinationLive live destination =
  Right
    destination
      { liveThrough =
          Just
            ( maybe
                revision
                (max revision)
                destination.liveThrough
            )
      }
  where
    revision = alignmentLiveThroughSourceStoreRevision live

advanceDestination ::
  AlignmentSubscriptionId ->
  DestinationSubscription ->
  Either AlignmentTransferProblem (DestinationSubscription, DestinationWork)
advanceDestination identifier destination = do
  (afterSnapshot, snapshotWork) <- applyCompleteSnapshot destination
  let (afterChanges, changes) = drainDestinationChanges afterSnapshot
      (successor, acknowledgement) = acknowledgeCoveredLive identifier afterChanges
  Right
    ( successor,
      DestinationWork
        { snapshot = snapshotWork,
          changes,
          acknowledgement
        }
    )

applyCompleteSnapshot ::
  DestinationSubscription ->
  Either
    AlignmentTransferProblem
    (DestinationSubscription, Maybe (StoreRevision, [RetainedStateEvidence]))
applyCompleteSnapshot destination = case destination.snapshot.appliedBase of
  Just _ -> Right (destination, Nothing)
  Nothing -> case (destination.snapshot.start, destination.snapshot.end) of
    (Just start, Just end) -> do
      let expectedCount = alignmentSnapshotStartChunkCount start
          expectedNumbers = take (fromIntegral expectedCount) [0 ..]
      if Map.keys destination.snapshot.chunks /= expectedNumbers
        then Right (destination, Nothing)
        else do
          let base = alignmentSnapshotStartBaseRevision start
          if alignmentSnapshotEndBaseRevision end == base
            then Right ()
            else
              Left
                ( AlignmentTransferSnapshotBaseMismatch
                    base
                    (alignmentSnapshotEndBaseRevision end)
                )
          let facts =
                concatMap
                  alignmentSnapshotChunkRetainedStates
                  (Map.elems destination.snapshot.chunks)
          generation <- sourceProofGeneration destination.subscribe
          let sourceStore =
                alignmentSubscribeSourceStoreIncarnation destination.subscribe
              expectedDigest =
                alignmentSemanticSnapshotDigest generation sourceStore base facts
              observedStart = alignmentSnapshotStartSemanticDigest start
              observedEnd = alignmentSnapshotEndSemanticDigest end
          if expectedDigest == observedStart && expectedDigest == observedEnd
            then
              let successor =
                    setDestinationAppliedThrough (Just base)
                      . setDestinationSnapshot
                        (setSnapshotAppliedBase (Just base) destination.snapshot)
                      $ destination
               in Right (successor, Just (base, facts))
            else
              Left
                ( AlignmentTransferSnapshotDigestMismatch
                    expectedDigest
                    (if observedStart /= expectedDigest then observedStart else observedEnd)
                )
    _ -> Right (destination, Nothing)

drainDestinationChanges ::
  DestinationSubscription -> (DestinationSubscription, [AlignmentChange])
drainDestinationChanges destination = case destination.appliedThrough of
  Nothing -> (destination, [])
  Just through -> go through destination []
  where
    go through current applied =
      let next = nextStoreRevision through
       in case Map.lookup next current.heldChanges of
            Nothing -> (current {appliedThrough = Just through}, applied)
            Just change ->
              go
                next
                current
                  { heldChanges = Map.delete next current.heldChanges,
                    appliedChanges = Map.insert next change current.appliedChanges,
                    appliedThrough = Just next
                  }
                (applied <> [change])

acknowledgeCoveredLive ::
  AlignmentSubscriptionId ->
  DestinationSubscription ->
  (DestinationSubscription, Maybe AlignmentAck)
acknowledgeCoveredLive identifier destination =
  case (destination.appliedThrough, destination.liveThrough) of
    (Just applied, Just live)
      | applied >= live ->
          let joined = maybe live (max live) destination.acknowledgedThrough
              acknowledgement = alignmentAck identifier joined
           in ( setDestinationAcknowledgedThrough (Just joined) destination,
                Just acknowledgement
              )
    _ -> (destination, Nothing)

preparedDestinationWork :: PreparedDestinationWork -> DestinationWork
preparedDestinationWork (PreparedDestinationWork prepared) =
  snd (preparedOutput prepared)

commitDestinationWork ::
  PreparedDestinationWork -> (State, AlignmentTransferDisposition)
commitDestinationWork (PreparedDestinationWork prepared) =
  let (state, (disposition, _)) = commitPrepared prepared
   in (state, disposition)

newtype PreparedAlignmentCancellation
  = PreparedAlignmentCancellation
      (Prepared State AlignmentTransferDisposition)

prepareAlignmentCancellation ::
  AlignmentCancel ->
  State ->
  Either AlignmentTransferProblem PreparedAlignmentCancellation
prepareAlignmentCancellation cancellation state = do
  let identifier = alignmentCancelSubscriptionId cancellation
  case (Map.lookup identifier state.sources, Map.lookup identifier state.destinations) of
    (Nothing, Nothing) -> Left (AlignmentTransferSubscriptionMissing identifier)
    (Just source, Just destination) -> do
      successorSource <- retainSourceCancellation identifier cancellation source
      successorDestination <-
        retainDestinationCancellation identifier cancellation destination
      prepareCancellation
        ( patchInputClosureDestination
            identifier
            (Just destination)
            successorDestination
            (retireSourceRelation identifier source successorSource state)
              { sources = Map.insert identifier successorSource state.sources,
                destinations = Map.insert identifier successorDestination state.destinations
              }
        )
    (Just source, Nothing) -> do
      successorSource <- retainSourceCancellation identifier cancellation source
      prepareCancellation
        (retireSourceRelation identifier source successorSource state)
          { sources = Map.insert identifier successorSource state.sources
          }
    (Nothing, Just destination) -> do
      successorDestination <-
        retainDestinationCancellation identifier cancellation destination
      prepareCancellation
        ( patchInputClosureDestination
            identifier
            (Just destination)
            successorDestination
            state
              { destinations = Map.insert identifier successorDestination state.destinations
              }
        )
  where
    retireSourceRelation identifier previous successor stateWithSource
      | previous.cancellation == Nothing && successor.cancellation /= Nothing = retireInputClosureSource identifier previous stateWithSource
      | otherwise = stateWithSource
    prepareCancellation candidate =
      let successor = removeOrphanedPredecessorInputClosures (retireSourcePrefixWork (alignmentCancelSubscriptionId cancellation) candidate)
       in PreparedAlignmentCancellation
            <$> prepareTransition
              ( \predecessor ->
                  Right
                    ( successor,
                      if successor == predecessor
                        then AlignmentTransferUnchanged
                        else AlignmentTransferRetained
                    )
              )
              state

retainSourceCancellation ::
  AlignmentSubscriptionId ->
  AlignmentCancel ->
  SourceSubscription ->
  Either AlignmentTransferProblem SourceSubscription
retainSourceCancellation _ cancellation source = case source.cancellation of
  Nothing -> Right (setSourceCancellation (Just cancellation) source)
  Just incumbent
    | incumbent == cancellation -> Right source
    | cancellationPriority cancellation > cancellationPriority incumbent ->
        Right (setSourceCancellation (Just cancellation) source)
    | otherwise -> Right source

retainDestinationCancellation ::
  AlignmentSubscriptionId ->
  AlignmentCancel ->
  DestinationSubscription ->
  Either AlignmentTransferProblem DestinationSubscription
retainDestinationCancellation _ cancellation destination = case destination.cancellation of
  Nothing -> Right (setDestinationCancellation (Just cancellation) destination)
  Just incumbent
    | incumbent == cancellation -> Right destination
    | cancellationPriority cancellation > cancellationPriority incumbent ->
        Right (setDestinationCancellation (Just cancellation) destination)
    | otherwise -> Right destination

cancellationPriority :: AlignmentCancel -> Int
cancellationPriority cancellation = case alignmentCancelReason cancellation of
  AlignmentSourceIncarnationLost -> 0
  AlignmentRelationRemoved -> 1
  AlignmentDestinationIncarnationLost -> 2

sourceHasCoveredLive :: SourceSubscription -> Bool
sourceHasCoveredLive source =
  source.liveReleased
    && case source.acknowledgedThrough of
      Nothing -> False
      Just acknowledged -> acknowledged >= source.sentThrough

commitAlignmentCancellation ::
  PreparedAlignmentCancellation -> (State, AlignmentTransferDisposition)
commitAlignmentCancellation (PreparedAlignmentCancellation prepared) =
  commitPrepared prepared

destinationRetryControls ::
  AlignmentSubscriptionId -> State -> [AlignmentControl]
destinationRetryControls identifier state =
  case Map.lookup identifier state.destinations of
    Nothing -> []
    Just destination -> case destination.cancellation of
      Just cancellation ->
        [AlignmentCancelled cancellation]
      Nothing ->
        [AlignmentSubscribeRequested destination.subscribe]
          <> maybe
            []
            (pure . AlignmentAcknowledged . alignmentAck identifier)
            destination.acknowledgedThrough

requireSource ::
  AlignmentSubscriptionId ->
  State ->
  Either AlignmentTransferProblem SourceSubscription
requireSource identifier state =
  maybe
    (Left (AlignmentTransferSubscriptionMissing identifier))
    Right
    (Map.lookup identifier state.sources)

requireDestination ::
  AlignmentSubscriptionId ->
  State ->
  Either AlignmentTransferProblem DestinationSubscription
requireDestination identifier state =
  maybe
    (Left (AlignmentTransferSubscriptionMissing identifier))
    Right
    (Map.lookup identifier state.destinations)

requireLiveIdentity ::
  AlignmentSubscriptionId ->
  AlignmentLive ->
  Either AlignmentTransferProblem ()
requireLiveIdentity identifier live
  | alignmentLiveSubscriptionId live == identifier = Right ()
  | otherwise = Left (AlignmentTransferSubscriptionConflict identifier)

requireChangeIdentity ::
  AlignmentSubscriptionId ->
  AlignmentChange ->
  Either AlignmentTransferProblem ()
requireChangeIdentity identifier change
  | alignmentChangeSubscriptionId change == identifier = Right ()
  | otherwise = Left (AlignmentTransferSubscriptionConflict identifier)

data AlignmentTransferInvariantProblem
  = AlignmentTransferSourceInvariant
  | AlignmentTransferDestinationInvariant
  | AlignmentTransferRevisionInvariant
  | AlignmentTransferReadinessInvariant
  | AlignmentTransferAppliedEvidenceInvariant
  | AlignmentTransferSourcePrefixWorkInvariant
  | AlignmentTransferInputClosureWorkInvariant
  | AlignmentTransferDestinationProgressWorkInvariant
  deriving stock (Eq, Ord, Show, Enum, Bounded)

validateAlignmentTransferState ::
  State -> Either AlignmentTransferInvariantProblem ()
validateAlignmentTransferState state = do
  requireInvariant (state.destinationProgressChanges `Set.isSubsetOf` Map.keysSet state.destinations) AlignmentTransferDestinationProgressWorkInvariant
  let expectedSourcesByStore =
        Map.fromListWith
          Set.union
          [ (alignmentSubscribeSourceStoreIncarnation source.subscribe, Set.singleton identifier)
          | (identifier, source) <- Map.toAscList state.sources,
            source.cancellation == Nothing
          ]
      activeSources = Set.unions (Map.elems expectedSourcesByStore)
  requireInvariant
    ( state.sourceIds == activeSources
        && state.sourcesByRetainedStore == expectedSourcesByStore
        && state.pendingSourcePrefixes `Set.isSubsetOf` activeSources
    )
    AlignmentTransferSourcePrefixWorkInvariant
  let retainedSources =
        [ (identifier, generation, target)
        | (identifier, source) <- Map.toAscList state.sources,
          Just (generation, target) <- [sourceInputClosureScope source]
        ]
      expectedByChild = Map.fromListWith Set.union [(generation, Set.singleton identifier) | (identifier, generation, _) <- retainedSources]
      expectedByTarget = Map.fromListWith Set.union [(target, Set.singleton identifier) | (identifier, _, target) <- retainedSources]
      eligible = Set.fromList (pendingPredecessorInputClosureSourceIds state)
      availableDestinations = [(identifier, destination) | (identifier, destination) <- Map.toAscList state.destinations, destinationAvailableForInputClosure destination]
      expectedOrdinaryByTarget =
        Map.fromListWith
          Set.union
          [ (target, Set.singleton identifier)
          | (identifier, destination) <- availableDestinations,
            ContextDestination _ <- [destination.purpose],
            target <- Set.toAscList (destinationInputClosureTargets destination)
          ]
      expectedAvailableByOwner =
        Map.fromListWith
          Set.union
          [(alignmentSubscribeObligationId destination.subscribe, Set.singleton identifier) | (identifier, destination) <- availableDestinations]
      expectedBarrierKeys =
        Map.fromListWith
          Set.union
          [(alignmentSubscribeSubscriptionId subscribe, Set.singleton key) | (key, (subscribe, _)) <- Map.toAscList state.predecessorInputClosures]
      expectedCompleted =
        Map.fromListWith
          Set.union
          [(generation, Set.singleton owner) | ((generation, owner), (_, Just _)) <- Map.toAscList state.predecessorInputClosures]
  requireInvariant
    ( state.eligibleInputClosureSources == eligible
        && state.pendingInputClosureSources `Set.isSubsetOf` eligible
        && state.predecessorSourcesByChild == expectedByChild
        && state.predecessorSourcesByTarget == expectedByTarget
        && state.ordinaryClosureDestinationsByTarget == expectedOrdinaryByTarget
        && state.availableClosureDestinationsByOwner == expectedAvailableByOwner
        && state.inputClosureBarrierKeysBySubscription == expectedBarrierKeys
        && state.completedInputClosureOwnersByChild == expectedCompleted
    )
    AlignmentTransferInputClosureWorkInvariant
  mapM_ validateDeferred (Map.toAscList state.deferredControls)
  mapM_ validateSource (Map.toAscList state.sources)
  mapM_ validateSourceRejection (Map.toAscList state.sourceRejections)
  mapM_ validateDestination (Map.toAscList state.destinations)
  mapM_ validateRouteCutover (Map.toAscList state.routeCutovers)
  mapM_ validateRouteCutoverAssignment (Map.toAscList state.routeCutoverAssignments)
  mapM_ validatePredecessorInputClosure (Map.toAscList state.predecessorInputClosures)
  mapM_
    validatePredecessorInputClosureReceipt
    (Map.toAscList state.predecessorInputClosureReceipts)
  mapM_
    validateLocalMemberReadinessFulfillment
    (Map.toAscList state.localMemberReadinessFulfillments)
  mapM_ validateAppliedEvidence (Map.toAscList state.appliedDestinationEvidence)
  requireInvariant
    ( Map.keysSet state.appliedDestinationEvidence
        == expectedAppliedDestinationEvidenceKeys state
    )
    AlignmentTransferAppliedEvidenceInvariant
  where
    validateDeferred (identifier, controls) =
      requireInvariant
        ( Map.member identifier state.destinations
            && not (null controls)
            && all ((== Just identifier) . deferredControlSubscription . snd) controls
            && maybe False (\destination -> all ((== destinationSourceEpoch destination) . fst) controls) (Map.lookup identifier state.destinations)
            && length controls == length (nub controls)
        )
        AlignmentTransferDestinationInvariant
    validateSource (identifier, source) = do
      requireInvariant
        ( alignmentSubscribeSubscriptionId source.subscribe == identifier
            && Map.notMember identifier state.sourceRejections
            && either (const False) (const True) (sourceProofGeneration source.subscribe)
            && (source.liveInitiallyWithheld || source.liveReleased)
            && (source.acknowledgedThrough == Nothing || source.liveReleased)
            && sourceLiveClosureValid source
        )
        AlignmentTransferSourceInvariant
      requireInvariant
        ( all
            (\(revision, change) -> alignmentChangeStoreRevision change == revision)
            (Map.toAscList source.sentChanges)
            && maybe True (<= source.sentThrough) source.acknowledgedThrough
        )
        AlignmentTransferRevisionInvariant

    validateSourceRejection (identifier, rejection) =
      requireInvariant
        ( alignmentSubscribeSubscriptionId rejection.subscribe == identifier
            && alignmentCancelSubscriptionId rejection.cancellation == identifier
            && Map.notMember identifier state.sources
            && either
              (const False)
              (const True)
              (sourceProofGeneration rejection.subscribe)
        )
        AlignmentTransferSourceInvariant

    validateDestination (identifier, destination) = do
      requireInvariant
        ( alignmentSubscribeSubscriptionId destination.subscribe == identifier
            && destinationPurposeValid identifier destination
        )
        AlignmentTransferDestinationInvariant
      requireInvariant
        ( all
            (\(revision, change) -> alignmentChangeStoreRevision change == revision)
            (Map.toAscList destination.heldChanges <> Map.toAscList destination.appliedChanges)
            && Map.keysSet destination.heldChanges
              `Set.disjoint` Map.keysSet destination.appliedChanges
            && maybe
              (Map.null destination.appliedChanges)
              ( \base ->
                  appliedChangesContiguous
                    base
                    destination.appliedThrough
                    (Map.keys destination.appliedChanges)
              )
              destination.snapshot.appliedBase
            && maybe
              True
              (\through -> all (> through) (Map.keys destination.heldChanges))
              destination.appliedThrough
            && maybe
              True
              (\acknowledged -> maybe False (>= acknowledged) destination.appliedThrough)
              destination.acknowledgedThrough
        )
        AlignmentTransferRevisionInvariant

    validateRouteCutover (key, marker) =
      requireInvariant
        ( key
            == ( alignmentRouteCutoverMarkerGeneration marker,
                 alignmentRouteCutoverMarkerSourceHerald marker,
                 alignmentRouteCutoverMarkerPredecessorHerald marker
               )
        )
        AlignmentTransferDestinationInvariant

    validateRouteCutoverAssignment (key, receipt) =
      requireInvariant
        ( case Map.lookup key state.routeCutovers of
            Just marker -> routeCutoverAssignmentMatches marker receipt
            Nothing -> False
        )
        AlignmentTransferDestinationInvariant

    sourceLiveClosureValid source
      | not source.liveInitiallyWithheld = source.liveReleased
      | not source.liveReleased = True
      | otherwise =
          let obligation = alignmentSubscribeObligation source.subscribe
              key =
                ( alignmentObligationDestinationGeneration obligation,
                  alignmentSubscribeSourceStoreIncarnation source.subscribe
                )
           in case Map.lookup key state.predecessorInputClosureReceipts of
                Nothing -> False
                Just receipt -> receipt.sourceRevision <= source.sentThrough

    validatePredecessorInputClosure ((_, owner), (subscribe, witnessed)) =
      requireInvariant
        ( alignmentSubscribeObligationId subscribe == owner
            && case Map.lookup
              (alignmentSubscribeSubscriptionId subscribe)
              state.destinations of
              Nothing -> False
              Just destination ->
                destination.subscribe == subscribe
                  && maybe
                    True
                    (\revision -> maybe False (>= revision) destination.liveThrough)
                    witnessed
        )
        AlignmentTransferReadinessInvariant

    validatePredecessorInputClosureReceipt (key, receipt) =
      requireInvariant
        ( key == (receipt.generation, receipt.sourceStore)
            && Set.member
              (receipt.generation, receipt.predecessorHerald)
              state.localRoutePrefixSettlements
            && length receipt.prefixes
              == Set.size
                (Set.fromList (fmap memberReadinessPrefixSubscription receipt.prefixes))
            && all (inputClosurePrefixRetained receipt) receipt.prefixes
        )
        AlignmentTransferReadinessInvariant

    inputClosurePrefixRetained receipt prefix =
      memberReadinessPrefixIsCovered prefix
        && case Map.lookup prefix.subscription state.destinations of
          Nothing -> False
          Just destination ->
            Map.lookup
              ( receipt.generation,
                alignmentSubscribeObligationId destination.subscribe
              )
              state.predecessorInputClosures
              == Just (destination.subscribe, Just prefix.live)
              && any
                ((== receipt.sourceStore) . destinationStoreIncarnation)
                ( NonEmpty.toList
                    ( alignmentObligationDestinationStores
                        (alignmentSubscribeObligation destination.subscribe)
                    )
                )
              && maybe False (>= prefix.applied) destination.appliedThrough
              && maybe False (>= prefix.live) destination.liveThrough
              && maybe False (>= prefix.acknowledged) destination.acknowledgedThrough

    destinationPurposeValid identifier destination = case destination.purpose of
      ContextDestination attempt ->
        alignmentAttemptSubscriptionId attempt == identifier
          && case alignmentSubscribeSourceProof destination.subscribe of
            FinalCertificate _ _ -> True
            BootstrapProof _ _ -> False
      BootstrapDestination _ -> case alignmentSubscribeSourceProof destination.subscribe of
        BootstrapProof _ _ -> True
        FinalCertificate _ _ -> False

    appliedChangesContiguous base through revisions =
      go base revisions == through
      where
        go retained [] = Just retained
        go retained (revision : remaining)
          | revision == nextStoreRevision retained = go revision remaining
          | otherwise = Nothing

    validateLocalMemberReadinessFulfillment (key, fulfillment) =
      requireInvariant
        ( key
            == ( fulfillment.generation,
                 destinationStoreIncarnation fulfillment.destination
               )
            && not (null fulfillment.prefixes)
            && length fulfillment.prefixes
              == Set.size
                (Set.fromList (fmap memberReadinessPrefixSubscription fulfillment.prefixes))
            && all
              (readinessPrefixRetained fulfillment.destination)
              fulfillment.prefixes
        )
        AlignmentTransferReadinessInvariant

    readinessPrefixRetained memberDestination prefix =
      memberReadinessPrefixIsCovered prefix
        && case Map.lookup prefix.subscription state.destinations of
          Nothing -> False
          Just destination ->
            memberDestination
              `elem` NonEmpty.toList
                ( alignmentObligationDestinationStores
                    (alignmentSubscribeObligation destination.subscribe)
                )
              && maybe False (>= prefix.applied) destination.appliedThrough
              && maybe False (>= prefix.live) destination.liveThrough
              && maybe False (>= prefix.acknowledged) destination.acknowledgedThrough

    validateAppliedEvidence (key, evidence) =
      requireInvariant
        ( key == appliedDestinationEvidenceKey evidence
            && either
              (const False)
              (const True)
              (validateAppliedDestinationEvidence state evidence)
        )
        AlignmentTransferAppliedEvidenceInvariant

expectedAppliedDestinationEvidenceKeys :: State -> Set AppliedDestinationEvidenceKey
expectedAppliedDestinationEvidenceKeys state =
  Set.fromList
    [ ( identifier,
        destinationStoreIncarnation destinationStoreValue,
        retainedPublicationEvidencePublicationId publication,
        revision,
        kind
      )
    | (identifier, destination) <- Map.toAscList state.destinations,
      (revision, kind, publication) <- destinationAppliedTranscript destination,
      destinationStoreValue <-
        NonEmpty.toList
          ( alignmentObligationDestinationStores
              (alignmentSubscribeObligation destination.subscribe)
          )
    ]

destinationAppliedTranscript ::
  DestinationSubscription ->
  [(StoreRevision, AppliedDestinationEvidenceKind, RetainedPublicationEvidence)]
destinationAppliedTranscript destination = snapshotEvidence <> changeEvidence
  where
    snapshotEvidence = case destination.snapshot.appliedBase of
      Nothing -> []
      Just revision ->
        concatMap
          ( \stateEvidence ->
              [ ( revision,
                  AppliedSnapshotRepresentative,
                  retainedStateEvidenceRepresentative stateEvidence
                ),
                ( revision,
                  AppliedSnapshotStrengthWitness,
                  retainedStateEvidenceStrengthWitness stateEvidence
                )
              ]
          )
          ( concatMap
              alignmentSnapshotChunkRetainedStates
              (Map.elems destination.snapshot.chunks)
          )
    changeEvidence =
      [ ( revision,
          AppliedContinuingChange,
          alignmentChangeRetainedTransition change
        )
      | (revision, change) <- Map.toAscList destination.appliedChanges
      ]

requireInvariant ::
  Bool -> problem -> Either problem ()
requireInvariant condition problem
  | condition = Right ()
  | otherwise = Left problem
