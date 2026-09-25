-- | Pure opt-in measurement views. Calling these does not advance a kernel or
-- construct a protocol message. The runtime aggregates their finite key sets.
module Eclips.Herald.Diagnostics
  ( heraldAlignmentInventory,
    heraldLabelWorkCounts,
    heraldPublicationGroupInventory,
    heraldStructuralReportCounts,
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch (EffectBatch)
import Eclips.Herald.Graph.Progress qualified as Progress
import Eclips.Herald.Internal.Diagnostics qualified as Diagnostics
import Eclips.Herald.Publication.Groups qualified as Groups
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Startup.State
  ( HeraldState,
    startupAlignmentState,
    startupDiscoveryState,
    startupLabelPatchState,
    startupPublicationState,
    startupStructuralProgressState,
  )
import Eclips.Herald.UseCase.LabelPatch qualified as LabelPatch

-- | Finite cumulative label preparation/application counters. Unlike the
-- alignment inventory, these scalars count work when it actually happens.
heraldLabelWorkCounts :: HeraldState -> [(String, Word64)]
heraldLabelWorkCounts state =
  ("label.structural.snapshots", 1)
    : LabelPatch.labelStructuralWorkCounts (startupLabelPatchState state)

-- | Current conservative readiness bookkeeping, not cumulative publication
-- history. This diagnostic scan runs only for an explicitly requested census.
heraldPublicationGroupInventory :: HeraldState -> [(String, Word64)]
heraldPublicationGroupInventory state =
  [ ("publication.groups.snapshots", 1),
    ("publication.groups.active", fromIntegral (Groups.retainedGroupCount groups)),
    ("publication.groups.local_pending", fromIntegral (Groups.retainedPublicationCount groups)),
    ("publication.groups.peer_frontiers", fromIntegral (Groups.retainedFrontierCount groups))
  ]
  where
    groups = Publication.publicationGroups (startupPublicationState state)

-- | Once-at-shutdown inventory. Counts are per owner; summing owners does not
-- deduplicate generations replicated on multiple Heralds. Birth-generation
-- history, explicit current-plan bindings and retained operational state are
-- separate inventories, not lifetime event counts or byte/heap estimates.
heraldAlignmentInventory :: HeraldState -> [(String, Word64)]
heraldAlignmentInventory state =
  Diagnostics.alignmentInventoryCounts
    (map snd (Alignment.alignmentGenerationEntries alignment))
    (Alignment.alignmentGenerationRelationEntries alignment)
    <> [("alignment.plans.snapshots", 1), ("alignment.state.snapshots", 1)]
    <> Diagnostics.alignmentPlanInventoryCounts "retained" plans
    <> Diagnostics.alignmentPlanInventoryCounts "current" current
    <> Diagnostics.alignmentPlanInventoryCounts "live" (Alignment.liveCurrentAlignmentPlans alignment)
    <> [ count "alignment.plans.retained.invalidated" (filter (\plan -> Alignment.alignmentPlanInvalidated (Plan.alignmentPlanId plan) alignment) plans),
         count "alignment.plans.unsettled" (Alignment.unsettledAlignmentPlans alignment),
         count "alignment.plans.acceptances" (Alignment.alignmentPlanAcceptanceEntries alignment),
         stateCount "obligations.active" (Alignment.activeAlignmentObligationEntries alignment),
         stateCount "obligations.closing" (Alignment.alignmentClosingObligationIds alignment),
         stateCount "obligations.closed" (Alignment.alignmentClosedObligationEntries alignment),
         stateCount "attempts.retained" (Alignment.alignmentAttemptEntries alignment),
         stateCount "attempts.invalidations" (Alignment.alignmentAttemptInvalidationEntries alignment),
         stateCount "plans.invalidations" (Alignment.alignmentPlanInvalidationEntries alignment),
         stateCount "promotions.retained" (Alignment.alignmentPromotionEntries alignment),
         stateCount "structural_debts.live" (Alignment.liveAlignmentStructuralDebtEntries alignment),
         stateCount "generation_evidence.pending" (Alignment.pendingGenerationEvidenceEntries alignment),
         stateCount "bootstraps.retained" (Alignment.bootstrapImportEntries alignment),
         stateCount "bootstraps.attempts" (Alignment.bootstrapImportAttemptEntries alignment),
         stateCount "bootstraps.invalidations" (Alignment.bootstrapImportAttemptInvalidationEntries alignment),
         stateCount "bootstraps.fulfillments" (Alignment.bootstrapImportFulfillmentEntries alignment),
         stateCount "certificates.retained" (Alignment.alignmentHistoricalCertificateEntries alignment),
         stateCount "certificates.live" (Alignment.liveAlignmentHistoricalCertificateEntries alignment),
         stateCount "readiness.retained" (Alignment.alignmentMemberReadinessEntries alignment),
         stateCount "readiness.local" (Alignment.alignmentLocalMemberReadinessEntries alignment),
         stateCount "source_subscriptions.retained" sources,
         stateCount "source_subscriptions.cancelled" (filter (isJust . Transfer.sourceSubscriptionCancellation) sources),
         stateCount "source_subscriptions.live_released" (filter Transfer.sourceSubscriptionLiveReleased sources),
         stateCount "source_subscriptions.live_acknowledged" (filter Transfer.sourceSubscriptionIsLiveAcknowledged sources),
         stateCount "source_subscriptions.snapshot_facts" (concatMap Transfer.sourceSubscriptionSnapshotFacts sources),
         stateCount "source_subscriptions.retained_changes" (concatMap Transfer.sourceSubscriptionSentChanges sources),
         stateCount "source_subscriptions.rejections" (Transfer.sourceRejectionEntries transfer),
         stateCount "destination_subscriptions.retained" destinations,
         stateCount "destination_subscriptions.cancelled" (filter (isJust . Transfer.destinationSubscriptionCancellation) destinations),
         stateCount "destination_subscriptions.live" (filter (isJust . Transfer.destinationSubscriptionLiveThrough) destinations),
         stateCount "destination_subscriptions.snapshot_facts" (concatMap Transfer.destinationSubscriptionSnapshotFacts destinations),
         stateCount "destination_subscriptions.retained_changes" (concatMap Transfer.destinationSubscriptionChanges destinations),
         stateCount "route_cutovers.retained" (Transfer.routeCutoverEntries transfer),
         stateCount "route_assignments.retained" (Transfer.routeCutoverAssignmentEntries transfer),
         stateCount "route_settlements.retained" (Transfer.localRoutePrefixSettlementEntries transfer),
         stateCount "input_closures.retained" (Transfer.predecessorInputClosureEntries transfer),
         stateCount "input_closure_receipts.retained" (Transfer.predecessorInputClosureReceiptEntries transfer),
         stateCount "readiness_fulfillments.retained" (Transfer.localMemberReadinessFulfillmentEntries transfer),
         stateCount "applied_destination_evidence.retained" (Transfer.appliedDestinationEvidenceEntries transfer),
         ("alignment.state.source_prefixes.pending", fromIntegral (Set.size (Transfer.pendingSourcePrefixIds transfer))),
         ("alignment.state.input_closure_sources.pending", fromIntegral (Set.size (Transfer.pendingInputClosureSourceIds transfer)))
       ]
  where
    alignment = startupAlignmentState state
    plans = map snd (Alignment.alignmentPlanEntries alignment)
    occurrences = Set.fromList (map (Plan.alignmentPlanIdSort . Plan.alignmentPlanId) plans)
    current = [plan | occurrence <- Set.toAscList occurrences, Just plan <- [Alignment.currentAlignmentPlanForSort occurrence alignment]]
    transfer = Alignment.alignmentTransferState alignment
    sources = map snd (Transfer.sourceSubscriptionEntries transfer)
    destinations = map snd (Transfer.destinationSubscriptionEntries transfer)
    count :: String -> [a] -> (String, Word64)
    count key values = (key, fromIntegral (length values))
    stateCount :: String -> [a] -> (String, Word64)
    stateCount key = count ("alignment.state." <> key)

-- | Attribute only actual structural-report send intents. No state-wide
-- comparison is performed, and no remote reports are confused with the local
-- report previously offered to each current binding.
heraldStructuralReportCounts :: HeraldState -> EffectBatch -> [(String, Word64)]
heraldStructuralReportCounts predecessor =
  Diagnostics.structuralReportCounts
    ( Map.fromList
        [ (binding, report)
        | binding <- Discovery.currentPeerBindings (startupDiscoveryState predecessor)
        ]
    )
  where
    report = Progress.structuralLocalReport (startupStructuralProgressState predecessor)
