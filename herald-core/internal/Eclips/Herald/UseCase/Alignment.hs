{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Whole-Herald coordination for deterministic context-generation cuts.
--
-- Structural reconciliation first records cause-qualified debt.  This
-- coordinator is the later cut boundary: it reconstructs the exact installed
-- topology projection, combines it with one complete retained placement
-- vector, derives the universal per-sort context graph, and atomically promotes
-- every debt covered by that cut before retaining any cut acceptance.
-- Snapshot/change subscription fulfilment intentionally lives behind the
-- separate transfer seam in 'Eclips.Herald.UseCase.PeerControl'.
module Eclips.Herald.UseCase.Alignment
  ( AlignmentCoordinationProblem (..),
    advanceAlignmentGenerations,
    AlignmentGenerationWorkDisposition (..),
    advanceAlignmentGenerationsWithDisposition,
    advanceAlignmentGenerationsWithPreparationForTest,
    advanceAlignmentGenerationsWithDispositionExhaustive,
    AlignmentGenerationControlProblem (..),
    AlignmentGenerationControlDisposition (..),
    applyAlignmentGenerationControl,
    applyAlignmentGenerationControlWithDisposition,
    applyHistoricalAlignmentPlan,
    AlignmentRouteCutoverProblem (..),
    retainAlignmentRouteCutoverMarker,
    applyAlignmentRouteCutoverMarker,
    alignmentRetryControls,
    alignmentReconnectRetryControls,
    alignmentGenerationControlDelta,
    retainedAnchorGenerationIds,
    replayAlignmentGenerationPlanAt,
    replayAlignmentPlanAt,
    replayAnnouncedAlignmentPlan,
    deriveAlignmentGenerationPreparationAt,
    deriveAlignmentInputsAt,
    activePlacementRoutesForSort,
    alignmentPromotionHandoffPending,
    alignmentPlanAnchorGeneration,
    AlignmentCausePromotionDisposition (..),
    alignmentCausePromotionDisposition,
    alignmentReplayTargetIsNext,
    alignmentRecoveryCandidateCuts,
    alignmentFirstViableRecoveryTopologyCut,
    AlignmentRecoveryCutView,
    prepareAlignmentRecoveryCutView,
    alignmentFirstMatchingRecoveryCut,
    liveLatestAlignmentGenerationsForSort,
    spontaneousAlignmentPromotionMayDerive,
    spontaneousAlignmentPromotionAuthorized,
    anchorAlignmentPromotionMayDerive,
    anchorAlignmentPromotionAuthorized,
    alignmentCausePromotionAuthorized,
    alignmentCausesInProjectionOrder,
    alignmentCausesInInstalledCutProjectionOrder,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.List (find, nub, sort, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Alignment
  ( AlignmentCut,
    AlignmentPlanAttempt,
    ContextClassGenerationId,
    PhysicalPlacementRevisionVector,
    StoreRevision,
    alignmentCutAttempt,
    alignmentCutExactMembers,
    alignmentCutFreshMemberBaseEvidence,
    alignmentCutPhysicalPlacementRevisionVector,
    alignmentCutPredecessorGenerationIds,
    alignmentCutSortDefinitionOccurrenceId,
    alignmentCutSortId,
    alignmentCutTopologyCut,
    alignmentMemberDelta,
    alignmentMemberHerald,
    alignmentMemberStoreIncarnation,
    alignmentPlanAttemptMembershipChange,
    deriveContextClassGenerationId,
    freshMemberBaseDelta,
    freshMemberBaseRevision,
    freshMemberBaseStoreIncarnation,
    initialAlignmentPlanAttempt,
    initialStoreRevision,
    mkAlignmentPlanAttemptAtMembership,
    nextAlignmentPlanAttempt,
    physicalPlacementRevisionEntries,
    physicalPlacementRevisionMembershipGenerationId,
  )
import Eclips.Domain.Context
  ( ContextGraph,
    ContextProblem,
    deriveContextGraph,
  )
import Eclips.Domain.Graph
  ( edgeDestination,
    edgeSource,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    HeraldEpoch,
    StoreIncarnationId,
    TopologyCutId,
    controlIndex,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
  )
import Eclips.Domain.Membership (heraldMembershipGenerationActiveHeraldEpochs, heraldMembershipGenerationChangeControlIndex, heraldMembershipGenerationId)
import Eclips.Domain.Structural
  ( structuralPrefixThrough,
    structuralVersionVectorComponent,
  )
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    StructuralConsequenceCauseView (..),
    structuralConsequenceCauseCanonicalBytes,
    structuralConsequenceCauseView,
  )
import Eclips.Domain.Topology
  ( TopologyCut,
    TopologyFrontier,
    deriveTopologyCutId,
    topologyCutFrontier,
    topologyFrontierAppliedControlPrefix,
    topologyFrontierMembershipGenerationId,
    topologyFrontierStructuralVersionVector,
  )
import Eclips.Herald.Alignment.CutQueries
  ( AlignmentRecoveryCutView,
    activityIsActive,
    alignmentFirstMatchingRecoveryCut,
    installedDeltaRouteCoordinate,
    placementDeltaRouteCoordinate,
    prepareAlignmentRecoveryCutView,
  )
import Eclips.Herald.Alignment.CutQueries qualified as CutQueries
import Eclips.Herald.Alignment.Generation
  ( AlignmentGeneration,
    AlignmentGenerationPlan,
    AlignmentGenerationPreparation (..),
    AlignmentGenerationProblem,
    AlignmentGenerationRelation,
    GenerationMemberInput,
    alignmentGenerationAnnouncer,
    alignmentGenerationCut,
    alignmentGenerationCutAnnounce,
    alignmentGenerationId,
    alignmentGenerationPlanGenerations,
    alignmentGenerationRelationDestination,
    alignmentGenerationRelationSource,
    generationMemberInput,
    prepareAlignmentGenerationPlan,
  )
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.Protocol
  ( AlignmentControl (..),
    AlignmentCutAccepted,
    AlignmentCutAnnounce,
    AlignmentRouteCutoverMarker,
    ClassMemberReady,
    HistoricalCertificate,
    alignmentCutAccepted,
    alignmentCutAcceptedGeneration,
    alignmentCutAcceptedHerald,
    alignmentCutAnnounceCut,
    alignmentCutAnnounceGeneration,
    alignmentRouteCutoverMarkerGeneration,
    alignmentRouteCutoverMarkerPredecessorHerald,
    alignmentRouteCutoverMarkerSourceHerald,
    classMemberReadyGeneration,
    classMemberReadyStoreIncarnation,
    historicalCertificateClassGeneration,
    historicalCertificateSourceStoreIncarnation,
  )
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks)
import Eclips.Herald.Discovery
  ( PeerBinding,
    peerBindingRemoteHeraldEpoch,
  )
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (QueuePeerAlignmentEvidence, RejectPeerConnection, SendPeerControl),
    PeerProtocolDisposition (ClosePeerControlProtocol),
    orderedEffectBatch,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedLocalHeraldEpoch,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Input (PeerControl (PeerAlignmentControl))
import Eclips.Herald.Join.State qualified as Join
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerDelivery qualified as DeliveryOwner
import Eclips.Herald.Placement
  ( DeltaRoute,
    deltaRouteDelta,
    deltaRouteOccurrenceId,
    deltaRouteSortId,
    deltaRouteStoreIncarnation,
  )
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupAlignmentCutQueryCache,
    replaceStartupAlignmentState,
    replaceStartupGraphState,
    replaceStartupPlacementState,
    replaceStartupSortRegistryState,
    replaceStartupStructuralProgressState,
    startupAlignmentCutQueryCache,
    startupAlignmentState,
    startupControlledState,
    startupDiagnosticChecks,
    startupDiscoveryState,
    startupGenesis,
    startupGraphState,
    startupJoinState,
    startupOracleProjectionState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    sortOccurrence,
    sortOccurrenceDefinition,
    sortOccurrenceSortId,
    structuralConsequenceDebtKey,
    structuralDebtKeyCause,
    structuralDebtKeySort,
    structuralDebtSetEntries,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Oracle.Admission (admissionRecordAttempt, admissionRecordId)

data AlignmentCoordinationProblem
  = AlignmentCoordinationTopologyProblem GraphProgress.StructuralProgressProblem
  | AlignmentCoordinationPlacementProblem Placement.PlacementCutProblem
  | AlignmentCoordinationPlacementMembershipMismatch
      [DeltaId]
      [DeltaId]
  | AlignmentCoordinationContextProblem ContextProblem
  | AlignmentCoordinationGenerationProblem AlignmentGenerationProblem
  | AlignmentCoordinationPlanProblem Plan.AlignmentPlanProblem
  | AlignmentCoordinationGenerationHeld (Set ContextClassGenerationId)
  | AlignmentCoordinationReplayGenerationMissing SortOccurrence TopologyCutId
  | AlignmentCoordinationPromotionProblem Alignment.AlignmentPromotionProblem
  | AlignmentCoordinationCutEvidenceProblem Alignment.AlignmentCutEvidenceProblem
  | AlignmentCoordinationPendingEvidenceInvariant
  | AlignmentCoordinationCauseOutsideTopologyCoordinate
      StructuralConsequenceCause
  deriving stock (Eq, Show)

-- | Errors at the peer boundary distinguish malformed remote evidence from a
-- contradiction in already checked local owner state.  The former closes only
-- the current peer-control binding; the latter remains an invariant fault.
data AlignmentGenerationControlProblem
  = AlignmentGenerationControlProtocolViolation
  | AlignmentGenerationControlInvariantViolation AlignmentCoordinationProblem
  deriving stock (Eq, Show)

data AlignmentRouteCutoverProblem
  = AlignmentRouteCutoverSourceMismatch HeraldEpoch HeraldEpoch
  | AlignmentRouteCutoverPredecessorMismatch HeraldEpoch HeraldEpoch
  | AlignmentRouteCutoverGenerationMissing ContextClassGenerationId
  | AlignmentRouteCutoverSourceAcceptanceMissing
      ContextClassGenerationId
      HeraldEpoch
  | AlignmentRouteCutoverPredecessorStoreMissing ContextClassGenerationId HeraldEpoch
  | AlignmentRouteCutoverRetentionProblem Transfer.AlignmentTransferProblem
  deriving stock (Eq, Show)

data PendingGroup = PendingGroup
  { topologyCut :: !TopologyCutId,
    affectedSort :: !SortOccurrence,
    causes :: !(Set StructuralConsequenceCause)
  }
  deriving stock (Eq, Show)

-- | The complete plan may be frozen either by its deterministic anchor Herald
-- or by replaying that anchor's exact announced cut.  A non-anchor class
-- announcement is useful only after the shared plan has already been retained.
data AlignmentPromotionTrigger
  = SpontaneousAlignmentPromotion
  | AnnouncedAlignmentPromotion HeraldEpoch Plan.AlignmentPlanId

-- | Coordinator interpretation of one retained structural consequence cause.
-- Coverage and candidate authorization are intentionally distinct: an active
-- coordinate satisfies the debt even when the placement vector advances, but
-- a tombstoned exact coordinate remains blocked until a fresh vector exists.
data AlignmentCausePromotionDisposition
  = AlignmentCauseCovered
  | AlignmentCausePromotionDue
  | AlignmentCausePromotionBlocked
  deriving stock (Eq, Show)

-- | Whether an announced generation belongs to the first same-sort debt group
-- which still has work. Promotion receipts deliberately retain their original
-- debts, so a raw first-group check would let an already covered removal-only
-- group block every later anchor replay forever. A genuinely due or blocked
-- earlier group remains the sequencing barrier.
alignmentReplayTargetIsNext ::
  TopologyCutId ->
  [(TopologyCutId, [AlignmentCausePromotionDisposition])] ->
  Bool
alignmentReplayTargetIsNext target groups =
  case find groupHasOutstandingCause groups of
    Just (topology, _) -> topology == target
    Nothing -> False
  where
    groupHasOutstandingCause (_, dispositions) =
      any (/= AlignmentCauseCovered) dispositions

-- | Retain canonical installed-chain order while preventing a recovery from
-- moving behind any previously retained coordinate. The latest incumbent is
-- included because a fresh placement vector may legitimately recover a Store
-- incarnation replacement without changing topology.
alignmentRecoveryCandidateCuts ::
  [TopologyCutId] ->
  Set TopologyCutId ->
  Either TopologyCutId [TopologyCutId]
alignmentRecoveryCandidateCuts installed incumbentTopologies = do
  positions <-
    traverse
      ( \topology ->
          maybe (Left topology) Right (Map.lookup topology installedOrder)
      )
      (Set.toAscList incumbentTopologies)
  case positions of
    [] -> Right installed
    _ -> Right (drop (maximum positions) installed)
  where
    installedOrder = Map.fromList (zip installed [(0 :: Int) ..])

-- | Stop at the first viable candidate while threading a caller-owned cache.
-- In particular, a failure while checking an earlier candidate is returned;
-- it is never hidden by speculatively selecting a later installed cut.
alignmentFirstViableRecoveryTopologyCut ::
  (cache -> TopologyCutId -> Either problem (Bool, cache)) ->
  cache ->
  [TopologyCutId] ->
  Either problem (Maybe TopologyCutId, cache)
alignmentFirstViableRecoveryTopologyCut check = go
  where
    go cache [] = Right (Nothing, cache)
    go cache (candidate : rest) = do
      (viable, successorCache) <- check cache candidate
      if viable
        then Right (Just candidate, successorCache)
        else go successorCache rest

-- | Retain only predecessor generations whose complete plan coordinate is
-- still live. An invalidated generation's historical certificate may never
-- arrive, so carrying it into replacement planning could turn recovery into a
-- permanent prerequisite hold. It is excluded even when already certified so
-- evidence arrival order cannot fork replacement ancestry.
liveLatestAlignmentGenerationsForSort ::
  SortOccurrence ->
  Alignment.State ->
  [AlignmentGeneration]
liveLatestAlignmentGenerationsForSort affectedSort alignment =
  filter (not . (`Alignment.generationCurrentPlanInvalidated` alignment)) (Alignment.latestAlignmentGenerationsForSort affectedSort alignment)

-- | Total order matching the detached topology fold: the installed structural
-- snapshot is the base and the exact bounded Oracle control projection is its
-- overlay. Structural occurrences therefore promote first; control causes
-- promote globally by increasing ControlIndex, with canonical cause bytes as
-- the deterministic tie-break between distinct facts at one index.
data AlignmentCauseProjectionOrder
  = StructuralBaseCauseOrder StructuralConsequenceCause
  | ControlOverlayCauseOrder ControlIndex ByteString
  deriving stock (Eq, Ord, Show)

alignmentCausesInProjectionOrder ::
  TopologyFrontier ->
  Set StructuralConsequenceCause ->
  Either AlignmentCoordinationProblem [StructuralConsequenceCause]
alignmentCausesInProjectionOrder frontier =
  alignmentCausesInProjectionOrderWhere
    (`causeCoveredByFrontier` frontier)

-- | Order only causes covered by one exact installed cut.  Unlike the raw
-- frontier helper, this delegates membership-successor coverage to Graph, whose
-- retained terminal predecessor vector can cover a retired-source occurrence
-- intentionally absent from the successor frontier.
alignmentCausesInInstalledCutProjectionOrder ::
  TopologyCutId ->
  GraphProgress.StructuralProgressState ->
  Set StructuralConsequenceCause ->
  Either AlignmentCoordinationProblem [StructuralConsequenceCause]
alignmentCausesInInstalledCutProjectionOrder cut progress =
  alignmentCausesInProjectionOrderWhere
    (\cause -> GraphProgress.installedCutCoversCause cut cause progress)

alignmentCausesInProjectionOrderWhere ::
  (StructuralConsequenceCause -> Bool) ->
  Set StructuralConsequenceCause ->
  Either AlignmentCoordinationProblem [StructuralConsequenceCause]
alignmentCausesInProjectionOrderWhere covered causes = do
  mapM_ requireCovered (Set.toAscList causes)
  Right (sortOn alignmentCauseProjectionOrder (Set.toList causes))
  where
    requireCovered cause
      | covered cause = Right ()
      | otherwise =
          Left (AlignmentCoordinationCauseOutsideTopologyCoordinate cause)

alignmentCauseProjectionOrder ::
  StructuralConsequenceCause -> AlignmentCauseProjectionOrder
alignmentCauseProjectionOrder cause = case structuralConsequenceCauseView cause of
  StructuralOccurrenceCauseView {} -> StructuralBaseCauseOrder cause
  LabelReleaseCauseView _ index ->
    ControlOverlayCauseOrder
      index
      (structuralConsequenceCauseCanonicalBytes cause)
  ProcessEndCauseView _ index ->
    ControlOverlayCauseOrder
      index
      (structuralConsequenceCauseCanonicalBytes cause)
  PredefinedDisappearanceCauseView _ index ->
    ControlOverlayCauseOrder
      index
      (structuralConsequenceCauseCanonicalBytes cause)

causeCoveredByFrontier :: StructuralConsequenceCause -> TopologyFrontier -> Bool
causeCoveredByFrontier cause frontier =
  case structuralConsequenceCauseView cause of
    StructuralOccurrenceCauseView occurrence ->
      maybe
        False
        (>= structuralPrefixThrough (structuralOccurrenceSourceSequence occurrence))
        ( structuralVersionVectorComponent
            (structuralOccurrenceSourceHeraldEpoch occurrence)
            (topologyFrontierStructuralVersionVector frontier)
        )
    LabelReleaseCauseView _ index ->
      topologyFrontierAppliedControlPrefix frontier >= index
    ProcessEndCauseView _ index ->
      topologyFrontierAppliedControlPrefix frontier >= index
    PredefinedDisappearanceCauseView _ index ->
      topologyFrontierAppliedControlPrefix frontier >= index

-- | Exact change evidence for one generation-work pass.  Composition is the
-- monotone disjunction used by the bounded fixed point.
data AlignmentGenerationWorkDisposition
  = AlignmentGenerationWorkUnchanged
  | AlignmentGenerationWorkChanged
  deriving stock (Eq, Ord, Show)

instance Semigroup AlignmentGenerationWorkDisposition where
  AlignmentGenerationWorkChanged <> _ = AlignmentGenerationWorkChanged
  _ <> AlignmentGenerationWorkChanged = AlignmentGenerationWorkChanged
  AlignmentGenerationWorkUnchanged <> AlignmentGenerationWorkUnchanged =
    AlignmentGenerationWorkUnchanged

instance Monoid AlignmentGenerationWorkDisposition where
  mempty = AlignmentGenerationWorkUnchanged

-- | Exact catalogue dependencies accumulated only within one serialized
-- coordinator call. No facts are retained in owner state or reused across
-- independently admitted branches.
data GenerationWork = GenerationWork
  { workDisposition :: AlignmentGenerationWorkDisposition,
    evidenceGenerations :: Set ContextClassGenerationId,
    promotedGenerations :: Set ContextClassGenerationId,
    evidencePlans :: Set Plan.AlignmentPlanId,
    promotedPlans :: Set Plan.AlignmentPlanId,
    cancellations :: [Protocol.AlignmentCancel]
  }

instance Semigroup GenerationWork where
  left <> right =
    GenerationWork
      (left.workDisposition <> right.workDisposition)
      (left.evidenceGenerations <> right.evidenceGenerations)
      (left.promotedGenerations <> right.promotedGenerations)
      (left.evidencePlans <> right.evidencePlans)
      (left.promotedPlans <> right.promotedPlans)
      (left.cancellations <> right.cancellations)

instance Monoid GenerationWork where
  mempty = GenerationWork mempty Set.empty Set.empty Set.empty Set.empty []

cutEvidenceWorkDisposition ::
  ContextClassGenerationId ->
  Alignment.AlignmentCutEvidenceDisposition ->
  GenerationWork
cutEvidenceWorkDisposition generation disposition = case disposition of
  Alignment.AlignmentCutEvidenceRetained -> changedGenerationWork generation
  Alignment.AlignmentCutEvidenceUnchanged -> mempty

readinessWorkDisposition ::
  ContextClassGenerationId ->
  Alignment.AlignmentReadinessDisposition ->
  GenerationWork
readinessWorkDisposition generation disposition = case disposition of
  Alignment.AlignmentReadinessRetained -> changedGenerationWork generation
  Alignment.AlignmentReadinessUnchanged -> mempty

changedGenerationWork :: ContextClassGenerationId -> GenerationWork
changedGenerationWork generation =
  GenerationWork AlignmentGenerationWorkChanged (Set.singleton generation) Set.empty Set.empty Set.empty []

pendingGenerationEvidenceWorkDisposition ::
  Alignment.PendingGenerationEvidenceDisposition ->
  GenerationWork
pendingGenerationEvidenceWorkDisposition disposition = case disposition of
  Alignment.PendingGenerationEvidenceRetained -> changed
  Alignment.PendingGenerationEvidenceRemoved -> changed
  Alignment.PendingGenerationEvidenceUnchanged -> mempty
  where
    changed = GenerationWork AlignmentGenerationWorkChanged Set.empty Set.empty Set.empty Set.empty []

-- | Level-trigger every debt whose canonical installed topology cut and
-- complete placement history are now available. Virgin debt selects its first
-- covering cut matching the selected placement routes. Recovery after every
-- incumbent coordinate has been invalidated applies the same selection at or
-- after the latest incumbent cut.
-- Missing placement history or predecessor certificates is ordinary dependency
-- blocking and performs no partial mutation.
advanceAlignmentGenerations ::
  HeraldState ->
  Either AlignmentCoordinationProblem (HeraldState, EffectBatch)
advanceAlignmentGenerations initial = do
  (successor, effects, _) <- advanceAlignmentGenerationsWithDisposition initial
  Right (successor, effects)

advanceAlignmentGenerationsWithDisposition ::
  HeraldState ->
  Either
    AlignmentCoordinationProblem
    (HeraldState, EffectBatch, AlignmentGenerationWorkDisposition)
advanceAlignmentGenerationsWithDisposition = advanceAlignmentGenerationsWithDispositionUsing SelectiveGenerationWork

-- | Independent inventory traversal for differential tests. Checked item leaves
-- and effect assembly are shared; no dirty/index enumeration selects reference work.
advanceAlignmentGenerationsWithDispositionExhaustive :: HeraldState -> Either AlignmentCoordinationProblem (HeraldState, EffectBatch, AlignmentGenerationWorkDisposition)
advanceAlignmentGenerationsWithDispositionExhaustive = advanceAlignmentGenerationsWithDispositionUsing ExhaustiveGenerationWork

data GenerationTraversal = SelectiveGenerationWork | ExhaustiveGenerationWork

advanceAlignmentGenerationsWithDispositionUsing :: GenerationTraversal -> HeraldState -> Either AlignmentCoordinationProblem (HeraldState, EffectBatch, AlignmentGenerationWorkDisposition)
advanceAlignmentGenerationsWithDispositionUsing traversal =
  advanceAlignmentGenerationsWithPreparation traversal (prepareAlignmentCutSelection [])

-- | Demand seam for the preparation boundary. Tests may supply an unreachable
-- preparer while real owner journals remain readable. The ordinary driver uses
-- exactly this path with the actual retained-query preparation callback.
advanceAlignmentGenerationsWithPreparationForTest ::
  (HeraldState -> (AlignmentCutSelection, HeraldState)) ->
  HeraldState ->
  Either AlignmentCoordinationProblem (HeraldState, EffectBatch, AlignmentGenerationWorkDisposition)
advanceAlignmentGenerationsWithPreparationForTest = advanceAlignmentGenerationsWithPreparation SelectiveGenerationWork

advanceAlignmentGenerationsWithPreparation :: GenerationTraversal -> (HeraldState -> (AlignmentCutSelection, HeraldState)) -> HeraldState -> Either AlignmentCoordinationProblem (HeraldState, EffectBatch, AlignmentGenerationWorkDisposition)
advanceAlignmentGenerationsWithPreparation traversal prepare initial = do
  (successor, pendingEffects, disposition) <-
    advanceAlignmentGenerationStateWithPreparation traversal prepare initial
  Right
    ( successor,
      pendingEffects
        <> case disposition.workDisposition of
          AlignmentGenerationWorkUnchanged -> mempty
          AlignmentGenerationWorkChanged ->
            alignmentNewlyEnabledEffects disposition initial successor,
      disposition.workDisposition
    )

advanceAlignmentGenerationState ::
  HeraldState ->
  Either
    AlignmentCoordinationProblem
    (HeraldState, EffectBatch, GenerationWork)
advanceAlignmentGenerationState = advanceAlignmentGenerationStateWith SelectiveGenerationWork

-- Consume each single-reader journal only here. Owner replacements happen only
-- for an actual change, so metadata draining never discards a reusable cut view.
harvestGenerationWork :: HeraldState -> HeraldState
harvestGenerationWork state =
  replaceStartupAlignmentCutQueryCache cache (replaceStartupAlignmentState contextual afterPlacement)
  where
    !cache = startupAlignmentCutQueryCache state
    (progressChanged, progress) = GraphProgress.takeAlignmentPlanChange (startupStructuralProgressState state)
    (graphChanged, graph) = Graph.takeAlignmentPlanChange (startupGraphState state)
    (sortChanged, registry) = SortRegistry.takeAlignmentPlanChange (startupSortRegistryState state)
    (placementChanged, placement) = Placement.takeAlignmentPlanChange (startupPlacementState state)
    afterProgress = if progressChanged then replaceStartupStructuralProgressState progress state else state
    afterGraph = if graphChanged then replaceStartupGraphState graph afterProgress else afterProgress
    afterSort = if sortChanged then replaceStartupSortRegistryState registry afterGraph else afterGraph
    afterPlacement = if placementChanged then replaceStartupPlacementState placement afterSort else afterSort
    alignment = startupAlignmentState afterPlacement
    notified = if progressChanged || graphChanged || sortChanged || placementChanged then Alignment.wakeGenerationPlanInputs alignment else alignment
    contextual =
      Alignment.synchronizeGenerationPlanContext
        (heraldMembershipGenerationId (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState state))))
        (alignmentPromotionHandoffPending state)
        notified

advanceAlignmentGenerationStateWith :: GenerationTraversal -> HeraldState -> Either AlignmentCoordinationProblem (HeraldState, EffectBatch, GenerationWork)
advanceAlignmentGenerationStateWith traversal = advanceAlignmentGenerationStateWithPreparation traversal (prepareAlignmentCutSelection [])

advanceAlignmentGenerationStateWithPreparation :: GenerationTraversal -> (HeraldState -> (AlignmentCutSelection, HeraldState)) -> HeraldState -> Either AlignmentCoordinationProblem (HeraldState, EffectBatch, GenerationWork)
advanceAlignmentGenerationStateWithPreparation traversal prepare supplied
  | noWork =
      if Set.null (Alignment.generationPlanWorkIds alignment) && not (Alignment.hasPendingGenerationAnnouncements alignment)
        then
          let !retained = CutQueries.invalidatePlacementCutQueries (startupAlignmentCutQueryCache initial)
           in Right (replaceStartupAlignmentCutQueryCache retained initial, mempty, mempty)
        else
          if queryScopeChanged
            then Right (trimRetiredAlignmentCutQueries initial, mempty, mempty)
            else Right (initial, mempty, mempty)
  | otherwise = do
      let (selection, prepared) = prepare initial
      (successor, effects, work) <- advanceAlignmentGenerationStateUsingWith traversal selection prepared
      Right (trimAlignmentCutSelection selection successor, effects, work)
  where
    harvested = harvestGenerationWork supplied
    (queryScopeChanged, alignment) = Alignment.takeGenerationQueryScopeChange (startupAlignmentState harvested)
    initial = if queryScopeChanged then replaceStartupAlignmentState alignment harvested else harvested
    noWork = case traversal of
      SelectiveGenerationWork -> Set.null (Alignment.pendingGenerationEvidenceWork alignment) && Set.null (Alignment.pendingGenerationPlanWork alignment)
      ExhaustiveGenerationWork ->
        null (Alignment.pendingGenerationEvidenceEntries alignment)
          && (alignmentPromotionHandoffPending initial || (not (hasInstalledLiveAlignmentDebt initial) && Set.null (Alignment.alignmentResetRequiredSorts alignment)))

advanceAlignmentGenerationStateUsing ::
  AlignmentCutSelection ->
  HeraldState ->
  Either AlignmentCoordinationProblem (HeraldState, EffectBatch, GenerationWork)
advanceAlignmentGenerationStateUsing selection = advanceAlignmentGenerationStateUsingWith SelectiveGenerationWork selection . harvestGenerationWork

advanceAlignmentGenerationStateUsingWith :: GenerationTraversal -> AlignmentCutSelection -> HeraldState -> Either AlignmentCoordinationProblem (HeraldState, EffectBatch, GenerationWork)
advanceAlignmentGenerationStateUsingWith traversal selection initial = do
  fixedPoint initial mempty
  where
    fixedPoint predecessor effects = do
      (afterPlans, planDisposition) <-
        advanceAlignmentGenerationPlans traversal selection predecessor
      (afterPending, releasedEffects, pendingDisposition) <-
        drainPendingGenerationEvidence traversal selection afterPlans
      let combinedEffects = effects <> releasedEffects
          passDisposition = planDisposition <> pendingDisposition
      case passDisposition.workDisposition of
        AlignmentGenerationWorkUnchanged ->
          Right
            ( afterPending,
              combinedEffects,
              mempty
            )
        AlignmentGenerationWorkChanged -> do
          (successor, successorEffects, successorWork) <-
            fixedPoint afterPending combinedEffects
          Right
            ( successor,
              successorEffects,
              passDisposition <> successorWork
            )

advanceAlignmentGenerationPlans ::
  GenerationTraversal ->
  AlignmentCutSelection ->
  HeraldState ->
  Either
    AlignmentCoordinationProblem
    (HeraldState, GenerationWork)
advanceAlignmentGenerationPlans traversal selection supplied = do
  if Set.null selectedSorts || alignmentPromotionHandoffPending initial
    then Right (initial, mempty)
    else do
      placement <- selection.currentPlacement
      case placement of
        Nothing -> Right (initial, mempty)
        Just vector -> do
          (reported, reportWork) <- reportInvalidatedCurrentPlans selectedSorts initial
          (reset, resetWork) <- advanceAlignmentResets selectedSorts vector reported
          let coordinates = Alignment.liveAlignmentDebtCoordinatesAtSorts selectedSorts (startupAlignmentState reset)
          groups <- pendingGroupsAtPlacementFor coordinates selection vector reset
          (successor, _, disposition) <-
            foldM (advanceOne vector) (reset, Set.empty, mempty) groups
          Right (successor, reportWork <> resetWork <> disposition)
  where
    alignment = startupAlignmentState supplied
    selectedSorts = case traversal of
      SelectiveGenerationWork -> Alignment.pendingGenerationPlanWork alignment
      ExhaustiveGenerationWork -> Set.fromList (map snd (alignmentDebtCoordinates alignment)) <> Alignment.alignmentResetRequiredSorts alignment
    initial = replaceStartupAlignmentState (Alignment.consumeGenerationPlanWork selectedSorts alignment) supplied
    advanceOne vector (state, blockedSorts, disposition) group
      | Set.member group.affectedSort blockedSorts =
          Right (state, blockedSorts, disposition)
      | otherwise = do
          orderedCauses <-
            pendingGroupCausesInProjectionOrder group state group.causes
          let causeDispositions =
                [ ( cause,
                    alignmentCausePromotionDisposition
                      cause
                      group.affectedSort
                      group.topologyCut
                      vector
                      (startupAlignmentState state)
                  )
                | cause <- orderedCauses
                ]
              promotionBlocked =
                any
                  ((== AlignmentCausePromotionBlocked) . snd)
                  causeDispositions
              pendingCauses =
                Set.fromList
                  [ cause
                  | (cause, AlignmentCausePromotionDue) <-
                      causeDispositions
                  ]
          case (promotionBlocked, Set.null pendingCauses) of
            (True, _) ->
              Right
                ( state,
                  Set.insert group.affectedSort blockedSorts,
                  disposition
                )
            (False, True) -> Right (state, blockedSorts, disposition)
            (False, False) -> do
              promoted <-
                prepareGroup
                  SpontaneousAlignmentPromotion
                  vector
                  Map.empty
                  group {causes = pendingCauses}
                  state
              case promoted of
                Nothing ->
                  Right
                    ( state,
                      Set.insert group.affectedSort blockedSorts,
                      disposition
                    )
                Just (afterPromotion, plan, promotionDisposition) -> do
                  (successor, acceptanceDisposition) <-
                    retainOneLocalPlanAcceptance plan afterPromotion
                  Right
                    ( successor,
                      blockedSorts,
                      disposition
                        <> promotionDisposition
                        <> acceptanceDisposition
                    )

-- | Whether a successfully handled control explicitly proved that its
-- generation evidence was already retained.  Other controls conservatively
-- retain the complete serving-driver path.
data AlignmentGenerationControlDisposition
  = AlignmentGenerationControlWorkRequired
  | AlignmentGenerationEvidenceUnchanged
  deriving stock (Eq, Ord, Show)

-- | Admit only generation/cut/readiness controls.  Returning 'Nothing' is the
-- explicit handoff to the snapshot/change transfer coordinator.
applyAlignmentGenerationControl ::
  PeerBinding ->
  AlignmentControl ->
  HeraldState ->
  Either
    AlignmentGenerationControlProblem
    (Maybe (HeraldState, EffectBatch))
applyAlignmentGenerationControl binding control predecessor =
  fmap
    (fmap discardDisposition)
    (applyAlignmentGenerationControlWithDisposition binding control predecessor)
  where
    discardDisposition (successor, effects, _) = (successor, effects)

-- | Detailed generation-control handoff for top-level driver suppression.
-- An admitted exact evidence duplicate reports 'Unchanged', while a
-- successfully handled generation acknowledgement reports its narrower
-- acknowledgement disposition. Blocked, changed, and transfer controls remain
-- conservative.
applyAlignmentGenerationControlWithDisposition ::
  PeerBinding ->
  AlignmentControl ->
  HeraldState ->
  Either
    AlignmentGenerationControlProblem
    (Maybe (HeraldState, EffectBatch, AlignmentGenerationControlDisposition))
applyAlignmentGenerationControlWithDisposition binding control predecessor = case control of
  AlignmentProbeMarker {} -> Right Nothing
  AlignmentPlanAnnounced _ -> Just <$> applyGenerationEvidenceControl remote control predecessor
  AlignmentPlanAcceptanceAdvertised _ -> Just <$> applyGenerationEvidenceControl remote control predecessor
  AlignmentPlanObsoleteAdvertised _ -> Just <$> applyGenerationEvidenceControl remote control predecessor
  AlignmentCutAnnounced _ ->
    Just <$> applyGenerationEvidenceControl remote control predecessor
  AlignmentCutAcceptanceAdvertised _ ->
    Just <$> applyGenerationEvidenceControl remote control predecessor
  AlignmentMemberReadyAdvertised _ ->
    Just <$> applyGenerationEvidenceControl remote control predecessor
  AlignmentHistoricalCertificateAdvertised _ ->
    Just <$> applyGenerationEvidenceControl remote control predecessor
  AlignmentSubscribeRequested _ -> Right Nothing
  AlignmentSnapshotStarted _ -> Right Nothing
  AlignmentSnapshotChunkTransferred _ -> Right Nothing
  AlignmentSnapshotEnded _ -> Right Nothing
  AlignmentChangeTransferred _ -> Right Nothing
  AlignmentLiveAdvertised _ -> Right Nothing
  AlignmentAcknowledged _ -> Right Nothing
  AlignmentCancelled _ -> Right Nothing
  where
    remote = peerBindingRemoteHeraldEpoch binding

-- | Authenticate and retain one ordered publication-stream cutover marker.
-- The marker may name only the connected remote source, this local predecessor
-- Herald, and a retained new generation with a frozen predecessor generation
-- for which this Herald hosts an exact store member.
applyAlignmentRouteCutoverMarker ::
  HeraldEpoch ->
  AlignmentRouteCutoverMarker ->
  HeraldState ->
  Either AlignmentRouteCutoverProblem HeraldState
applyAlignmentRouteCutoverMarker remote marker predecessor = do
  let local = checkedLocalHeraldEpoch (startupGenesis predecessor)
  successorAlignment <-
    retainAlignmentRouteCutoverMarker
      remote
      local
      marker
      (startupAlignmentState predecessor)
  Right (replaceStartupAlignmentState successorAlignment predecessor)

-- | Owner-slice variant used when a newly contiguous logical peer prefix
-- contains publications and cutover markers that must commit together.
retainAlignmentRouteCutoverMarker ::
  HeraldEpoch ->
  HeraldEpoch ->
  AlignmentRouteCutoverMarker ->
  Alignment.State ->
  Either AlignmentRouteCutoverProblem Alignment.State
retainAlignmentRouteCutoverMarker remote local marker alignment = do
  let
    source = alignmentRouteCutoverMarkerSourceHerald marker
    predecessorHerald = alignmentRouteCutoverMarkerPredecessorHerald marker
    generationId = alignmentRouteCutoverMarkerGeneration marker
  if source == remote
    then Right ()
    else Left (AlignmentRouteCutoverSourceMismatch remote source)
  if predecessorHerald == local
    then Right ()
    else
      Left
        ( AlignmentRouteCutoverPredecessorMismatch
            local
            predecessorHerald
        )
  generation <-
    maybe
      (Left (AlignmentRouteCutoverGenerationMissing generationId))
      Right
      (Alignment.lookupAlignmentGeneration generationId alignment)
  if Protocol.alignmentRouteCutoverMarkerPlan marker == Alignment.generationBirthPlanId generation
    then Right ()
    else Left (AlignmentRouteCutoverGenerationMissing generationId)
  case Alignment.lookupAlignmentCutAcceptance generationId source alignment of
    Just _ -> Right ()
    Nothing ->
      Left
        ( AlignmentRouteCutoverSourceAcceptanceMissing
            generationId
            source
        )
  let predecessorGenerations =
        [ retained
        | predecessorId <-
            alignmentCutPredecessorGenerationIds
              (alignmentGenerationCut generation),
          Just retained <- [Alignment.lookupAlignmentGeneration predecessorId alignment]
        ]
      localHostsPredecessor =
        any
          ( any ((== local) . alignmentMemberHerald)
              . NonEmpty.toList
              . alignmentCutExactMembers
              . alignmentGenerationCut
          )
          predecessorGenerations
  if localHostsPredecessor
    then Right ()
    else
      Left
        ( AlignmentRouteCutoverPredecessorStoreMissing
            generationId
            local
        )
  prepared <-
    mapLeft
      AlignmentRouteCutoverRetentionProblem
      ( Transfer.prepareRouteCutoverMarker
          marker
          (Alignment.alignmentTransferState alignment)
      )
  let (successorTransfer, _) = Transfer.commitRouteCutoverMarker prepared
  Right (Alignment.replaceAlignmentTransferState successorTransfer alignment)

data GenerationEvidenceAdmission
  = GenerationEvidenceBlocked
  | GenerationEvidenceAdmitted HeraldState GenerationWork

applyGenerationEvidenceControl ::
  HeraldEpoch ->
  AlignmentControl ->
  HeraldState ->
  Either
    AlignmentGenerationControlProblem
    (HeraldState, EffectBatch, AlignmentGenerationControlDisposition)
applyGenerationEvidenceControl remote control predecessor = do
  (selection, prepared, admission) <- case control of
    AlignmentPlanAnnounced _ -> do
      let (selected, withQueries) = prepareAlignmentCutSelection (controlPlacementVectors control) predecessor
      admitted <- tryApplyGenerationEvidence selected remote control withQueries
      Right (Just selected, withQueries, admitted)
    AlignmentCutAnnounced _ -> do
      let (selected, withQueries) = prepareAlignmentCutSelection (controlPlacementVectors control) predecessor
      admitted <- tryApplyGenerationEvidence selected remote control withQueries
      Right (Just selected, withQueries, admitted)
    _ -> do
      -- These evidence families do not query cut selection during admission.
      -- An exact replay should not construct or replace any derived queries.
      admitted <- tryApplyNonAnnouncementEvidence remote control predecessor
      Right (Nothing, predecessor, admitted)
  (afterCurrent, currentDisposition, evidenceUnchanged) <- case admission of
    GenerationEvidenceBlocked -> do
      (retained, disposition) <- retainPending remote control prepared
      Right (retained, disposition, False)
    GenerationEvidenceAdmitted admitted evidenceDisposition -> do
      (removed, pendingDisposition) <- removePending remote control admitted
      let disposition = evidenceDisposition <> pendingDisposition
      Right
        ( removed,
          disposition,
          disposition.workDisposition == AlignmentGenerationWorkUnchanged
        )
  (successor, pendingEffects, advancementDisposition) <-
    case currentDisposition.workDisposition of
      AlignmentGenerationWorkUnchanged ->
        Right
          ( afterCurrent,
            mempty,
            mempty
          )
      AlignmentGenerationWorkChanged -> case selection of
        Nothing -> mapInvariant (advanceAlignmentGenerationState afterCurrent)
        Just selected -> mapInvariant (advanceAlignmentGenerationStateUsing selected afterCurrent)
  let enabledEffects =
        if currentNeedsCatalogueFanout || advancementChangedCatalogue
          then alignmentNewlyEnabledEffectMembers (currentDisposition <> advancementDisposition) predecessor successor
          else []
      currentNeedsCatalogueFanout = case control of
        AlignmentPlanAnnounced announce -> not (planAnnounceResolved announce predecessor)
        AlignmentCutAnnounced announce ->
          not (cutAnnounceResolved announce predecessor)
        _ -> False
      advancementChangedCatalogue = case advancementDisposition.workDisposition of
        AlignmentGenerationWorkUnchanged -> False
        AlignmentGenerationWorkChanged -> True
  Right
    ( maybe successor (`trimAlignmentCutSelection` successor) selection,
      pendingEffects
        <> orderedEffectBatch (nub enabledEffects),
      if evidenceUnchanged
        then AlignmentGenerationEvidenceUnchanged
        else AlignmentGenerationControlWorkRequired
    )

tryApplyGenerationEvidence ::
  AlignmentCutSelection ->
  HeraldEpoch ->
  AlignmentControl ->
  HeraldState ->
  Either AlignmentGenerationControlProblem GenerationEvidenceAdmission
tryApplyGenerationEvidence selection remote control predecessor
  | supersededGenerationEvidence control (startupAlignmentState predecessor) = Right (GenerationEvidenceAdmitted predecessor mempty)
  | otherwise = case control of
      AlignmentPlanAnnounced announce -> do
        (successor, work) <- applyPlanAnnounce selection remote announce predecessor
        if planAnnounceResolved announce successor then Right (GenerationEvidenceAdmitted successor work) else Right GenerationEvidenceBlocked
      AlignmentCutAnnounced announce -> do
        (successor, _, disposition) <- applyCutAnnounce selection remote announce predecessor
        if cutAnnounceResolved announce successor then Right (GenerationEvidenceAdmitted successor disposition) else Right GenerationEvidenceBlocked
      _ -> tryApplyNonAnnouncementEvidence remote control predecessor

tryApplyNonAnnouncementEvidence ::
  HeraldEpoch ->
  AlignmentControl ->
  HeraldState ->
  Either AlignmentGenerationControlProblem GenerationEvidenceAdmission
tryApplyNonAnnouncementEvidence remote control predecessor
  | supersededGenerationEvidence control (startupAlignmentState predecessor) = Right (GenerationEvidenceAdmitted predecessor mempty)
  | otherwise = case control of
      AlignmentPlanObsoleteAdvertised report -> do
        requireProtocol (Protocol.alignmentPlanObsoleteReporter report == remote)
        (retained, disposition) <- mapProtocol (Alignment.retainAlignmentPlanObsolete report (startupAlignmentState predecessor))
        Right (GenerationEvidenceAdmitted (replaceStartupAlignmentState retained predecessor) (obsoleteEvidenceWork report disposition))
      AlignmentPlanAcceptanceAdvertised accepted -> do
        requireProtocol (Protocol.alignmentPlanAcceptedHerald accepted == remote)
        let identifier = Protocol.alignmentPlanAcceptedId accepted
            alignment = startupAlignmentState predecessor
        case Alignment.lookupAlignmentPlan identifier alignment of
          Nothing -> Right GenerationEvidenceBlocked
          Just plan -> do
            (retained, disposition) <- mapProtocol (Alignment.retainAlignmentPlanAcceptance accepted alignment)
            Right (GenerationEvidenceAdmitted (replaceStartupAlignmentState retained predecessor) (planEvidenceWork plan disposition))
      AlignmentCutAcceptanceAdvertised accepted -> do
        requireProtocol (alignmentCutAcceptedHerald accepted == remote)
        if generationRetained
          (alignmentCutAcceptedGeneration accepted)
          predecessor
          then do
            (successor, _, disposition) <-
              applyCutAccepted remote accepted predecessor
            Right (GenerationEvidenceAdmitted successor disposition)
          else Right GenerationEvidenceBlocked
      AlignmentMemberReadyAdvertised ready ->
        if generationRetained (classMemberReadyGeneration ready) predecessor
          then do
            (successor, _, disposition) <- applyMemberReady remote ready predecessor
            Right (GenerationEvidenceAdmitted successor disposition)
          else Right GenerationEvidenceBlocked
      AlignmentHistoricalCertificateAdvertised certificate ->
        case Alignment.lookupAlignmentGeneration
          (historicalCertificateClassGeneration certificate)
          (startupAlignmentState predecessor) of
          Just generation
            | certificateDependenciesSatisfied generation predecessor -> do
                (successor, _, disposition) <-
                  applyHistoricalCertificate remote certificate predecessor
                Right (GenerationEvidenceAdmitted successor disposition)
          _ -> Right GenerationEvidenceBlocked
      _ -> Left AlignmentGenerationControlProtocolViolation

retainPending ::
  HeraldEpoch ->
  AlignmentControl ->
  HeraldState ->
  Either
    AlignmentGenerationControlProblem
    (HeraldState, GenerationWork)
retainPending remote control predecessor = do
  prepared <-
    mapProtocol
      ( Alignment.preparePendingGenerationEvidence
          remote
          control
          (startupAlignmentState predecessor)
      )
  let (successorAlignment, disposition) =
        Alignment.commitPendingGenerationEvidence prepared
  Right
    ( replaceStartupAlignmentState successorAlignment predecessor,
      pendingGenerationEvidenceWorkDisposition disposition
    )

removePending ::
  HeraldEpoch ->
  AlignmentControl ->
  HeraldState ->
  Either
    AlignmentGenerationControlProblem
    (HeraldState, GenerationWork)
-- Obsolete reports have their own retained owner, including reports whose
-- rejected plan was never adopted. They never enter the dependency inbox.
removePending _ (AlignmentPlanObsoleteAdvertised _) predecessor = Right (predecessor, mempty)
removePending remote control predecessor = do
  prepared <-
    mapProtocol
      ( Alignment.preparePendingGenerationEvidenceRemoval
          remote
          control
          (startupAlignmentState predecessor)
      )
  let (successorAlignment, disposition) =
        Alignment.commitPendingGenerationEvidence prepared
  Right
    ( replaceStartupAlignmentState successorAlignment predecessor,
      pendingGenerationEvidenceWorkDisposition disposition
    )

drainPendingGenerationEvidence ::
  GenerationTraversal ->
  AlignmentCutSelection ->
  HeraldState ->
  Either
    AlignmentCoordinationProblem
    (HeraldState, EffectBatch, GenerationWork)
drainPendingGenerationEvidence traversal selection initial = case traversal of
  ExhaustiveGenerationWork ->
    foldM
      drainOne
      (initial, mempty, mempty)
      (Alignment.pendingGenerationEvidenceEntries (startupAlignmentState initial))
  SelectiveGenerationWork -> drainNext Nothing (initial, mempty, mempty)
  where
    inventory = Alignment.generationEvidenceWorkIds (startupAlignmentState initial)
    drainNext cursor result@(state, effects, work) =
      case Alignment.nextGenerationEvidenceWork inventory cursor (startupAlignmentState state) of
        Nothing -> Right result
        Just key -> do
          pending <-
            maybe
              (Left AlignmentCoordinationPendingEvidenceInvariant)
              Right
              (Alignment.lookupPendingGenerationEvidence key (startupAlignmentState state))
          successor <- drainOne (state, effects, work) pending
          drainNext (Just key) successor
    drainOne (supplied, effects, disposition) pending = do
      let remote = Alignment.pendingGenerationEvidenceRemoteHerald pending
          control = Alignment.pendingGenerationEvidenceControl pending
      key <- mapLeft (const AlignmentCoordinationPendingEvidenceInvariant) (Alignment.pendingGenerationEvidenceKey control)
      let predecessor = replaceStartupAlignmentState (Alignment.consumeGenerationEvidenceWork key (startupAlignmentState supplied)) supplied
      case tryApplyGenerationEvidence selection remote control predecessor of
        Right GenerationEvidenceBlocked ->
          Right (predecessor, effects, disposition)
        Right (GenerationEvidenceAdmitted admitted evidenceDisposition) -> do
          (successor, removalDisposition) <-
            removePendingAsCoordination remote control admitted
          Right
            ( successor,
              effects,
              disposition <> evidenceDisposition <> removalDisposition
            )
        Left _ -> do
          (successor, removalDisposition) <-
            removePendingAsCoordination remote control predecessor
          Right
            ( successor,
              effects <> pendingProtocolCloseEffect remote predecessor,
              disposition <> removalDisposition
            )

removePendingAsCoordination ::
  HeraldEpoch ->
  AlignmentControl ->
  HeraldState ->
  Either
    AlignmentCoordinationProblem
    (HeraldState, GenerationWork)
removePendingAsCoordination remote control predecessor = do
  prepared <-
    mapLeft
      (const AlignmentCoordinationPendingEvidenceInvariant)
      ( Alignment.preparePendingGenerationEvidenceRemoval
          remote
          control
          (startupAlignmentState predecessor)
      )
  let (successorAlignment, disposition) =
        Alignment.commitPendingGenerationEvidence prepared
  Right
    ( replaceStartupAlignmentState successorAlignment predecessor,
      pendingGenerationEvidenceWorkDisposition disposition
    )

pendingProtocolCloseEffect :: HeraldEpoch -> HeraldState -> EffectBatch
pendingProtocolCloseEffect remote state =
  orderedEffectBatch
    [ RejectPeerConnection binding ClosePeerControlProtocol
    | Just binding <-
        [Discovery.currentPeerBinding remote (startupDiscoveryState state)]
    ]

generationRetained :: ContextClassGenerationId -> HeraldState -> Bool
generationRetained generationId =
  maybe False (const True)
    . Alignment.lookupAlignmentGeneration generationId
    . startupAlignmentState

cutAnnounceResolved :: AlignmentCutAnnounce -> HeraldState -> Bool
cutAnnounceResolved announce state =
  generationRetained generationId state
    && case Alignment.lookupAlignmentCutAcceptance
      generationId
      local
      (startupAlignmentState state) of
      Just _ -> True
      Nothing -> False
  where
    generationId = alignmentCutAnnounceGeneration announce
    local = checkedLocalHeraldEpoch (startupGenesis state)

certificateDependenciesSatisfied ::
  AlignmentGeneration -> HeraldState -> Bool
certificateDependenciesSatisfied generation state =
  memberStores == readyStores
  where
    generationId = alignmentGenerationId generation
    memberStores =
      Set.fromList
        ( fmap
            alignmentMemberStoreIncarnation
            ( NonEmpty.toList
                (alignmentCutExactMembers (alignmentGenerationCut generation))
            )
        )
    readyStores =
      Set.fromList
        [ store
        | store <- Set.toAscList memberStores,
          Just _ <-
            [ Alignment.lookupAlignmentMemberReadiness
                generationId
                store
                (startupAlignmentState state)
            ]
        ]

applyCutAnnounce ::
  AlignmentCutSelection ->
  HeraldEpoch ->
  AlignmentCutAnnounce ->
  HeraldState ->
  Either
    AlignmentGenerationControlProblem
    (HeraldState, EffectBatch, GenerationWork)
applyCutAnnounce selection remote announce predecessor = do
  let suppliedCut = alignmentCutAnnounceCut announce
      generationId = alignmentCutAnnounceGeneration announce
      topologyId = deriveTopologyCutId (alignmentCutTopologyCut suppliedCut)
  requireProtocol
    (generationId == deriveContextClassGenerationId suppliedCut)
  case GraphProgress.lookupInstalledTopologyCut
    topologyId
    (startupStructuralProgressState predecessor) of
    Nothing ->
      Right
        ( predecessor,
          mempty,
          mempty
        )
    Just installed -> do
      requireProtocol
        (GraphProgress.installedTopologyCutCut installed == alignmentCutTopologyCut suppliedCut)
      applyInstalledCutAnnounce selection remote suppliedCut generationId topologyId predecessor

applyInstalledCutAnnounce ::
  AlignmentCutSelection ->
  HeraldEpoch ->
  AlignmentCut ->
  ContextClassGenerationId ->
  TopologyCutId ->
  HeraldState ->
  Either
    AlignmentGenerationControlProblem
    (HeraldState, EffectBatch, GenerationWork)
applyInstalledCutAnnounce _selection remote suppliedCut generationId _topologyId predecessor = do
  let alignment = startupAlignmentState predecessor
  case Alignment.lookupAlignmentGeneration generationId alignment of
    Nothing -> Right (predecessor, mempty, mempty)
    Just retained -> do
      requireProtocol (alignmentGenerationCut retained == suppliedCut)
      requireProtocol (expectedCutAnnouncer (retainedAnchorGenerationIds alignment) retained == remote)
      -- A birth-cut witness cannot authorize a new current routing plan.
      (accepted, work) <- mapInvariant (retainOneLocalCutAcceptance retained predecessor)
      Right (accepted, alignmentNewlyEnabledEffects work predecessor accepted, work)

applyCutAccepted ::
  HeraldEpoch ->
  AlignmentCutAccepted ->
  HeraldState ->
  Either
    AlignmentGenerationControlProblem
    (HeraldState, EffectBatch, GenerationWork)
applyCutAccepted remote accepted predecessor = do
  requireProtocol (alignmentCutAcceptedHerald accepted == remote)
  prepared <-
    mapProtocol
      ( Alignment.prepareAlignmentCutAcceptance
          accepted
          (startupAlignmentState predecessor)
      )
  let (successorAlignment, disposition) =
        Alignment.commitAlignmentCutAcceptance prepared
      successor = replaceStartupAlignmentState successorAlignment predecessor
  -- Retaining this remote acceptance cannot itself enable another generation
  -- control, so avoid rebuilding and diffing the complete reconnect catalogue
  -- for every accepted cut.
  Right (successor, mempty, cutEvidenceWorkDisposition (alignmentCutAcceptedGeneration accepted) disposition)

applyMemberReady ::
  HeraldEpoch ->
  ClassMemberReady ->
  HeraldState ->
  Either
    AlignmentGenerationControlProblem
    (HeraldState, EffectBatch, GenerationWork)
applyMemberReady remote ready predecessor = do
  generation <-
    requireGeneration
      (classMemberReadyGeneration ready)
      (startupAlignmentState predecessor)
  requireProtocol
    ( memberHeraldForStore
        (classMemberReadyStoreIncarnation ready)
        generation
        == Just remote
    )
  prepared <-
    mapProtocol
      ( Alignment.prepareClassMemberReady
          ready
          (startupAlignmentState predecessor)
      )
  let (successorAlignment, disposition) = Alignment.commitClassMemberReady prepared
      successor = replaceStartupAlignmentState successorAlignment predecessor
      work = readinessWorkDisposition (classMemberReadyGeneration ready) disposition
  Right
    ( successor,
      alignmentNewlyEnabledEffects work predecessor successor,
      work
    )

applyHistoricalCertificate ::
  HeraldEpoch ->
  HistoricalCertificate ->
  HeraldState ->
  Either
    AlignmentGenerationControlProblem
    (HeraldState, EffectBatch, GenerationWork)
applyHistoricalCertificate remote certificate predecessor = do
  let alignment = startupAlignmentState predecessor
  generation <-
    requireGeneration
      (historicalCertificateClassGeneration certificate)
      alignment
  requireProtocol
    ( memberHeraldForStore
        (historicalCertificateSourceStoreIncarnation certificate)
        generation
        == Just remote
    )
  prepared <-
    mapProtocol
      ( Alignment.prepareHistoricalCertificate
          certificate
          alignment
      )
  let (successorAlignment, disposition) =
        Alignment.commitHistoricalCertificate prepared
      successor = replaceStartupAlignmentState successorAlignment predecessor
      work = readinessWorkDisposition (historicalCertificateClassGeneration certificate) disposition
  Right
    ( successor,
      alignmentNewlyEnabledEffects work predecessor successor,
      work
    )

-- | Complete generation-evidence catalogue for one fixed remote Herald.
-- Delivery retry records are separately compacted by the peer delivery owner.
alignmentRetryControls ::
  HeraldEpoch ->
  HeraldEpoch ->
  Alignment.State ->
  [AlignmentControl]
alignmentRetryControls local remote state =
  fmap snd (alignmentRetryControlEntries local remote state)

-- | Private lookup identities for the four evidence families in this
-- catalogue. Each admitted catalogue has at most one payload per key. Keep
-- one payload equality check after lookup so stable difference remains exact
-- even when comparing independently admitted state branches.
data GenerationControlKey
  = PlanAnnounceKey Plan.AlignmentPlanId
  | PlanAcceptanceKey Plan.AlignmentPlanId HeraldEpoch
  | PlanObsoleteKey Plan.AlignmentPlanId HeraldEpoch
  | GenerationAnnounceKey ContextClassGenerationId
  | GenerationAcceptanceKey ContextClassGenerationId HeraldEpoch
  | GenerationReadinessKey ContextClassGenerationId StoreIncarnationId
  | GenerationCertificateKey ContextClassGenerationId StoreIncarnationId
  deriving stock (Eq, Ord, Show)

alignmentRetryControlEntries ::
  HeraldEpoch ->
  HeraldEpoch ->
  Alignment.State ->
  [(GenerationControlKey, AlignmentControl)]
alignmentRetryControlEntries local remote state =
  alignmentCurrentControlEntries state (alignmentControlEntries local remote (Alignment.alignmentControlEvidence Nothing state)) <> obsoleteControlEntries local remote state

-- Reset suppresses obsolete executable announcements on reconnect, while
-- immutable readiness/certificates still serve captured historical recipients.
alignmentCurrentControlEntries :: Alignment.State -> [(GenerationControlKey, AlignmentControl)] -> [(GenerationControlKey, AlignmentControl)]
alignmentCurrentControlEntries state = filter executableAnnouncement
  where
    executableAnnouncement (_, control) = case control of
      AlignmentPlanAnnounced _ -> not (supersededGenerationEvidence control state)
      AlignmentCutAnnounced _ -> not (supersededGenerationEvidence control state)
      _ -> True

alignmentControlEntries :: HeraldEpoch -> HeraldEpoch -> Alignment.AlignmentControlEvidence -> [(GenerationControlKey, AlignmentControl)]
alignmentControlEntries = alignmentControlEntriesForPlans Nothing

-- Existing evidence insertion can only suppress an announcement. Only a
-- committed plan can enable one, including a changed sibling anchor role.
-- Skipping that family for evidence-only scopes also avoids forcing the
-- retained-plan grouping used to compute announcement roles.
alignmentControlEntriesForPlans :: Maybe (Set ContextClassGenerationId) -> HeraldEpoch -> HeraldEpoch -> Alignment.AlignmentControlEvidence -> [(GenerationControlKey, AlignmentControl)]
alignmentControlEntriesForPlans changedPlans local remote evidence =
  planAnnounces
    <> planAcceptances
    <> announces
    <> acceptances
    <> readiness
    <> certificates
  where
    remoteInPlan identifier = remote `elem` map fst (NonEmpty.toList (physicalPlacementRevisionEntries (Plan.alignmentPlanIdPlacement identifier)))
    planAcceptedKeys = Set.fromList (map fst evidence.currentPlanAcceptances)
    planAnnounces =
      [ (PlanAnnounceKey identifier, AlignmentPlanAnnounced (Plan.alignmentPlanAnnouncement plan))
      | (identifier, plan) <- evidence.currentPlanEntries,
        Plan.alignmentPlanAnnouncer plan == Just local,
        remoteInPlan identifier,
        Set.notMember (identifier, remote) planAcceptedKeys
      ]
    planAcceptances =
      [ (PlanAcceptanceKey identifier accepting, AlignmentPlanAcceptanceAdvertised accepted)
      | ((identifier, accepting), accepted) <- evidence.currentPlanAcceptances,
        accepting == local,
        remoteInPlan identifier
      ]
    explicitBirth generation = Set.member (Alignment.generationBirthPlanId generation) evidence.explicitBirthPlans
    generations = evidence.generationEntries
    anchorGenerationIds = Set.fromList [alignmentGenerationId anchor | plan <- evidence.plans, Just anchor <- [anchorGeneration plan]]
    generationFor identifier = Map.lookup identifier generationById
    generationById = Map.fromList generations
    remoteIsFixedMember generation =
      any
        ((== remote) . fst)
        ( NonEmpty.toList
            ( physicalPlacementRevisionEntries
                ( alignmentCutPhysicalPlacementRevisionVector
                    (alignmentGenerationCut generation)
                )
            )
        )
    remoteNeedsHistoricalEvidence generation =
      remoteIsFixedMember generation
        || Set.member
          (alignmentGenerationId generation)
          (Map.findWithDefault Set.empty remote evidence.historicalRecipientGenerations)
    acceptedKeys =
      Set.fromList
        [ key
        | (key, _) <- evidence.acceptances
        ]

    announces =
      [ ( GenerationAnnounceKey generationId,
          AlignmentCutAnnounced (alignmentGenerationCutAnnounce generation)
        )
      | (generationId, generation) <- generations,
        not (explicitBirth generation),
        maybe True (Set.member generationId) changedPlans,
        expectedCutAnnouncer anchorGenerationIds generation == local,
        remoteIsFixedMember generation,
        Set.notMember (generationId, remote) acceptedKeys
      ]

    acceptances =
      [ ( GenerationAcceptanceKey generationId accepting,
          AlignmentCutAcceptanceAdvertised accepted
        )
      | ((generationId, accepting), accepted) <-
          evidence.acceptances,
        accepting == local,
        Just generation <- [generationFor generationId],
        not (explicitBirth generation),
        remoteIsFixedMember generation
      ]

    readiness =
      [ ( GenerationReadinessKey generationId store,
          AlignmentMemberReadyAdvertised ready
        )
      | ((generationId, store), ready) <-
          evidence.readiness,
        Just generation <- [generationFor generationId],
        memberHeraldForStore store generation == Just local,
        remoteNeedsHistoricalEvidence generation
      ]

    certificates =
      [ ( GenerationCertificateKey generationId store,
          AlignmentHistoricalCertificateAdvertised certificate
        )
      | ((generationId, store), certificate) <-
          evidence.certificateEntries,
        Just generation <- [generationFor generationId],
        memberHeraldForStore store generation == Just local,
        remoteNeedsHistoricalEvidence generation
      ]

-- | Semantic reconnect roots. The peer delivery owner substitutes its indexed
-- outstanding tail for acceptance/readiness roots after initial peer seeding.
alignmentReconnectRetryControls :: HeraldEpoch -> HeraldEpoch -> Alignment.State -> [AlignmentControl]
alignmentReconnectRetryControls = alignmentRetryControls

-- | Stable newly-enabled subset of the reconnect catalogue for one ordinary
-- state transition. Controls which were already outstanding are not re-sent.
alignmentGenerationControlDelta ::
  HeraldEpoch ->
  HeraldEpoch ->
  Alignment.State ->
  Alignment.State ->
  [AlignmentControl]
alignmentGenerationControlDelta _ _ predecessor successor
  | predecessor == successor = []
alignmentGenerationControlDelta local remote predecessor successor =
  alignmentSelectedControlDelta
    (Alignment.alignmentChangedControlGenerations predecessor successor)
    local
    remote
    predecessor
    successor

alignmentSelectedControlDelta ::
  Set ContextClassGenerationId ->
  HeraldEpoch ->
  HeraldEpoch ->
  Alignment.State ->
  Alignment.State ->
  [AlignmentControl]
alignmentSelectedControlDelta = alignmentSelectedControlDeltaForPlans Nothing Nothing

alignmentSelectedControlDeltaForPlans ::
  Maybe (Set Plan.AlignmentPlanId) ->
  Maybe (Set ContextClassGenerationId) ->
  Set ContextClassGenerationId ->
  HeraldEpoch ->
  HeraldEpoch ->
  Alignment.State ->
  Alignment.State ->
  [AlignmentControl]
alignmentSelectedControlDeltaForPlans selectedPlans changedPlans candidates local remote predecessor successor =
  [ control
  | (key, control) <- successorControls,
    Map.lookup key predecessorByKey /= Just control
  ]
  where
    predecessorByKey =
      Map.fromList (alignmentCurrentControlEntries predecessor (alignmentControlEntriesForPlans changedPlans local remote (Alignment.alignmentControlEvidenceForPlans (Just candidates) selectedPlans predecessor)) <> obsoleteControlEntries local remote predecessor)
    successorControls = alignmentCurrentControlEntries successor (alignmentControlEntriesForPlans changedPlans local remote (Alignment.alignmentControlEvidenceForPlans (Just candidates) selectedPlans successor)) <> obsoleteControlEntries local remote successor

hasInstalledLiveAlignmentDebt :: HeraldState -> Bool
hasInstalledLiveAlignmentDebt state =
  any
    debtHasCoveringCut
    (Alignment.liveAlignmentStructuralDebtEntries alignment)
  where
    alignment = startupAlignmentState state
    progress = startupStructuralProgressState state
    debtHasCoveringCut debt =
      GraphProgress.installedCutCoveringCause cause progress /= Nothing
      where
        key = structuralConsequenceDebtKey debt
        cause = structuralDebtKeyCause key

-- | The current placement choice belongs to this input; historical structural
-- and exact-vector query answers belong to the retained derived cache. Only
-- Alignment changes during the fixed point, so this shared selection remains
-- valid throughout it without retaining a whole predecessor Herald product.
data AlignmentCutSelection = AlignmentCutSelection
  { currentPlacement :: Either AlignmentCoordinationProblem (Maybe PhysicalPlacementRevisionVector),
    retainedCauses :: !(Set StructuralConsequenceCause),
    retainedSorts :: !(Set SortOccurrence),
    queries :: CutQueries.CutQueryCache
  }

controlPlacementVectors :: AlignmentControl -> [PhysicalPlacementRevisionVector]
controlPlacementVectors control = case control of
  AlignmentPlanAnnounced announce -> [Plan.alignmentPlanIdPlacement (Protocol.alignmentPlanAnnounceId announce)]
  AlignmentCutAnnounced announce ->
    [alignmentCutPhysicalPlacementRevisionVector (alignmentCutAnnounceCut announce)]
  _ -> []

alignmentDebtCoordinates :: Alignment.State -> [(StructuralConsequenceCause, SortOccurrence)]
alignmentDebtCoordinates alignment =
  Set.toAscList
    ( Set.fromList
        [ (structuralDebtKeyCause key, structuralDebtKeySort key)
        | debt <- Alignment.liveAlignmentStructuralDebtEntries alignment,
          let key = structuralConsequenceDebtKey debt
        ]
    )

-- | Include non-live debt: evidence admission can make it queryable without
-- inserting a new retained structural consequence during this fixed point.
alignmentRetainedDebtCoordinates :: Alignment.State -> (Set StructuralConsequenceCause, Set SortOccurrence)
alignmentRetainedDebtCoordinates alignment =
  (Set.fromList (fmap fst coordinates), Set.fromList (fmap snd coordinates))
  where
    coordinates =
      [ (structuralDebtKeyCause key, structuralDebtKeySort key)
      | debt <- structuralDebtSetEntries (Alignment.alignmentStructuralDebts alignment),
        let key = structuralConsequenceDebtKey debt
      ]

alignmentRetainedPlacementVectors ::
  Either AlignmentCoordinationProblem (Maybe PhysicalPlacementRevisionVector) ->
  Alignment.State ->
  Set PhysicalPlacementRevisionVector
alignmentRetainedPlacementVectors current alignment =
  Set.fromList
    ( currentVectors
        <> concatMap
          (controlPlacementVectors . Alignment.pendingGenerationEvidenceControl)
          (Alignment.pendingGenerationEvidenceEntries alignment)
    )
  where
    currentVectors = case current of
      Right (Just vector) -> [vector]
      _ -> []

alignmentCurrentPlacement :: HeraldState -> Either AlignmentCoordinationProblem (Maybe PhysicalPlacementRevisionVector)
alignmentCurrentPlacement state =
  case Placement.currentPhysicalPlacementRevisionVector
    (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState state)))
    (startupPlacementState state) of
    Left (Placement.PlacementCutRevisionUnavailable _ _) -> Right Nothing
    Left Placement.PlacementCutLocalOwnerMissing -> Right Nothing
    Left problem -> Left (AlignmentCoordinationPlacementProblem problem)
    Right vector -> Right (Just vector)

-- Retiring an announcement changes only the retained vector scope. Trim the
-- existing memo directly; do not prepare structural projections or evaluate a
-- parked plan/evidence payload merely to release its former vector.
trimRetiredAlignmentCutQueries :: HeraldState -> HeraldState
trimRetiredAlignmentCutQueries state = replaceStartupAlignmentCutQueryCache retained state
  where
    !cache = startupAlignmentCutQueryCache state
    (!causes, !sorts, _) = CutQueries.cutQueryRetainedKeys cache
    !placements = alignmentRetainedPlacementVectors (alignmentCurrentPlacement state) (startupAlignmentState state)
    !retained = CutQueries.trimCutQueryCache causes sorts placements cache

prepareAlignmentCutSelection ::
  [PhysicalPlacementRevisionVector] -> HeraldState -> (AlignmentCutSelection, HeraldState)
prepareAlignmentCutSelection extraPlacements state =
  (AlignmentCutSelection current causes requestedSorts retained, replaceStartupAlignmentCutQueryCache retained state)
  where
    !alignment = startupAlignmentState state
    (!causes, !requestedSorts) = alignmentRetainedDebtCoordinates alignment
    !current = alignmentCurrentPlacement state
    !placements = Set.fromList extraPlacements <> alignmentRetainedPlacementVectors current alignment
    -- Select owner values before a retained thunk can capture the whole product.
    !progress = startupStructuralProgressState state
    !placement = startupPlacementState state
    !registry = startupSortRegistryState state
    !graph = startupGraphState state
    !local = checkedLocalHeraldEpoch (startupGenesis state)
    !checks = startupDiagnosticChecks state
    !cache = startupAlignmentCutQueryCache state
    !retained =
      CutQueries.prepareCutQueryCache
        (alignmentCutReconciliationViews checks local registry graph)
        progress
        placement
        causes
        requestedSorts
        placements
        cache

-- | Historical projection consumes the sort-shape guard and nonstructural
-- baseline only. Do not capture current Controlled/Oracle owners in a retained
-- thunk through the broader current-reconciliation view constructor.
alignmentCutReconciliationViews :: DiagnosticChecks -> HeraldEpoch -> SortRegistry.State -> Graph.State -> Reconciliation.ReconciliationViews
alignmentCutReconciliationViews checks local registry graph =
  Reconciliation.withReconciliationDiagnosticChecks checks
    $ Reconciliation.reconciliationViews local sorts vertices Map.empty Set.empty
  where
    !sorts =
      Map.fromList
        [ ( SortRegistry.registryEntrySortId entry,
            sortOccurrence (SortRegistry.registryEntrySortId entry) (SortRegistry.registryEntryOccurrenceId entry)
          )
        | entry <- SortRegistry.registryEntries registry
        ]
    !vertices = Set.fromList (Graph.graphNonStructuralBaselineVertices graph)

trimAlignmentCutSelection :: AlignmentCutSelection -> HeraldState -> HeraldState
trimAlignmentCutSelection selection state =
  replaceStartupAlignmentCutQueryCache retained consumed
  where
    (scopeChanged, alignment) = Alignment.takeGenerationQueryScopeChange (startupAlignmentState state)
    consumed = if scopeChanged then replaceStartupAlignmentState alignment state else state
    !placements = alignmentRetainedPlacementVectors selection.currentPlacement alignment
    -- These entry points may change live debt and pending evidence, but never
    -- insert/remove retained structural debts. Reuse the exact entry-time sets.
    !retained = CutQueries.trimCutQueryCache selection.retainedCauses selection.retainedSorts placements selection.queries

cutQueryProblem :: CutQueries.CutQueryProblem -> AlignmentCoordinationProblem
cutQueryProblem problem = case problem of
  CutQueries.CutQueryTopologyProblem inner -> AlignmentCoordinationTopologyProblem inner
  CutQueries.CutQueryPlacementProblem inner -> AlignmentCoordinationPlacementProblem inner

-- | Each cause supplies a lower bound to a shared placement/sort suffix answer.
-- Keeping the earliest matching cut makes the plan independent of an unrelated
-- installed suffix. Live incumbents retain their coverage-only special path.
pendingGroupsAtPlacement ::
  AlignmentCutSelection ->
  PhysicalPlacementRevisionVector ->
  HeraldState ->
  Either AlignmentCoordinationProblem [PendingGroup]
pendingGroupsAtPlacement selection placement state = pendingGroupsAtPlacementFor (alignmentDebtCoordinates (startupAlignmentState state)) selection placement state

pendingGroupsAtPlacementFor :: [(StructuralConsequenceCause, SortOccurrence)] -> AlignmentCutSelection -> PhysicalPlacementRevisionVector -> HeraldState -> Either AlignmentCoordinationProblem [PendingGroup]
pendingGroupsAtPlacementFor coordinates selection placement state = do
  queries <-
    maybe
      (Left AlignmentCoordinationPendingEvidenceInvariant)
      (mapLeft cutQueryProblem)
      (CutQueries.cutQueriesAtPlacement placement selection.queries)
  grouped <- foldM (insertDebt queries) Map.empty coordinates
  Right (sortOn groupOrder (Map.elems grouped))
  where
    alignment = startupAlignmentState state
    insertDebt queries retained (cause, affectedSort) = do
      query <-
        maybe
          (Left AlignmentCoordinationPendingEvidenceInvariant)
          Right
          (Map.lookup affectedSort queries)
      covering <-
        maybe
          (Left AlignmentCoordinationPendingEvidenceInvariant)
          Right
          (CutQueries.cutQueryCauseCoverage cause selection.queries)
      let incumbents = alignmentCausePromotionKeys cause affectedSort alignment
          liveIncumbent = any (not . alignmentPromotionCoordinateInvalidated affectedSort alignment) incumbents
      selected <-
        mapLeft
          cutQueryProblem
          ( alignmentFirstMatchingRecoveryCut
              CutQueries.cutQueryMissingTopology
              liveIncumbent
              covering
              (Set.fromList (fmap Alignment.alignmentPromotionKeyTopologyCut incumbents))
              query
          )
      case selected of
        Nothing -> Right retained
        Just cut ->
          Right
            (Map.insertWith mergeGroup (cut, affectedSort) (PendingGroup cut affectedSort (Set.singleton cause)) retained)
    mergeGroup incoming incumbent = incumbent {causes = Set.union incumbent.causes incoming.causes}
    groupOrder group = (Map.findWithDefault maxBound group.topologyCut (CutQueries.cutQueryPositions selection.queries), group.affectedSort)

pendingGroupCausesInProjectionOrder ::
  PendingGroup ->
  HeraldState ->
  Set StructuralConsequenceCause ->
  Either AlignmentCoordinationProblem [StructuralConsequenceCause]
pendingGroupCausesInProjectionOrder group state =
  alignmentCausesInInstalledCutProjectionOrder
    group.topologyCut
    (startupStructuralProgressState state)

mergePendingGroups ::
  TopologyCutId ->
  SortOccurrence ->
  [PendingGroup] ->
  Maybe PendingGroup
mergePendingGroups topologyCut affectedSort groups = case groups of
  [] -> Nothing
  _ ->
    Just
      ( PendingGroup
          topologyCut
          affectedSort
          (Set.unions (fmap (.causes) groups))
      )

prepareGroup :: AlignmentPromotionTrigger -> PhysicalPlacementRevisionVector -> Map DeltaId StoreRevision -> PendingGroup -> HeraldState -> Either AlignmentCoordinationProblem (Maybe (HeraldState, Plan.AlignmentPlan, GenerationWork))
prepareGroup trigger placement suppliedFreshBases group predecessor = do
  let alignment = startupAlignmentState predecessor
      fixedMembers = fmap fst (physicalPlacementRevisionEntries placement)
      local = checkedLocalHeraldEpoch (startupGenesis predecessor)
      sourceAllowed = case trigger of
        SpontaneousAlignmentPromotion -> not (alignmentPromotionHandoffPending predecessor)
        AnnouncedAlignmentPromotion remote _ -> anchorAlignmentPromotionMayDerive remote fixedMembers
      causes = Set.filter (\cause -> alignmentCausePromotionAuthorized cause group.affectedSort group.topologyCut placement alignment) group.causes
      attempt = case trigger of
        AnnouncedAlignmentPromotion _ announced -> Plan.alignmentPlanIdAttempt announced
        SpontaneousAlignmentPromotion -> maybe initialAlignmentPlanAttempt (Plan.alignmentPlanIdAttempt . Plan.alignmentPlanId) (Alignment.currentAlignmentPlanForSort group.affectedSort alignment)
      identifier = Plan.alignmentPlanIdFromClaimedCoordinatesAtAttempt attempt group.affectedSort group.topologyCut placement
  ordered <- pendingGroupCausesInProjectionOrder group predecessor causes
  if not sourceAllowed || null ordered
    then Right Nothing
    else do
      routes <- activePlacementRoutesForSort predecessor group.affectedSort placement
      let mayDerive = case trigger of
            SpontaneousAlignmentPromotion -> spontaneousAlignmentPromotionMayDerive local fixedMembers (not (null routes))
            _ -> True
      if not mayDerive
        then Right Nothing
        else do
          preparation <- case Alignment.lookupAlignmentPlan identifier alignment of
            Just retained -> Right (Plan.AlignmentPlanReady retained)
            Nothing -> case deriveAlignmentInputsAt predecessor group.affectedSort group.topologyCut placement routes suppliedFreshBases of
              Left (AlignmentCoordinationPlacementMembershipMismatch _ _) | SpontaneousAlignmentPromotion <- trigger -> Right (Plan.AlignmentPlanHeld Set.empty)
              Left problem -> Left problem
              Right (cut, context, members) -> do
                let parent = fmap (\plan -> (plan, if Alignment.alignmentPlanInvalidated (Plan.alignmentPlanId plan) alignment then Plan.AlignmentPredecessorInvalidated else Plan.AlignmentPredecessorUsable)) (Alignment.currentAlignmentPlanForSort group.affectedSort alignment)
                mapLeft AlignmentCoordinationPlanProblem (Plan.prepareAlignmentPlanAtAttempt attempt group.affectedSort cut placement context members parent (retainedCertificateGenerations alignment))
          case preparation of
            Plan.AlignmentPlanHeld _ -> Right Nothing
            Plan.AlignmentPlanReady plan -> do
              let authorized = case trigger of
                    SpontaneousAlignmentPromotion -> Plan.alignmentPlanAnnouncer plan == Just local
                    AnnouncedAlignmentPromotion remote announced -> Plan.alignmentPlanId plan == announced && Plan.alignmentPlanAnnouncer plan == Just remote
              if not authorized
                then Right Nothing
                else do
                  (promoted, work) <- foldM (promote plan) (predecessor, mempty) ordered
                  Right (Just (promoted, plan, work))
  where
    promote plan (state, work) cause = do
      prepared <- mapLeft AlignmentCoordinationPromotionProblem (Alignment.prepareAlignmentPlanPromotion (checkedLocalHeraldEpoch (startupGenesis state)) cause plan (startupAlignmentState state))
      let (alignment, disposition) = Alignment.commitAlignmentPromotion prepared
          changed = case disposition of
            Alignment.AlignmentPromotionUnchanged -> mempty
            Alignment.AlignmentPromotionCommitted -> GenerationWork AlignmentGenerationWorkChanged Set.empty (Set.fromList (map alignmentGenerationId (currentPlanGenerations plan))) Set.empty (Set.singleton (Plan.alignmentPlanId plan)) []
      Right (replaceStartupAlignmentState alignment state, work <> changed)

-- A rejected attempt is retried only after its concrete withdrawal witness is
-- represented by the selected placement. Retiring the home also discharges it.
alignmentObsoleteCauseIncorporated :: HeraldState -> PhysicalPlacementRevisionVector -> Protocol.AlignmentPlanObsolete -> Bool
alignmentObsoleteCauseIncorporated state placement report =
  all incorporated (NonEmpty.toList (Protocol.alignmentPlanObsoleteLostStores report))
  where
    members = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState state))))
    incorporated lost
      | Protocol.alignmentLostStoreHome lost `notElem` members = True
      | otherwise = case lookup (Protocol.alignmentLostStoreHome lost) (NonEmpty.toList (physicalPlacementRevisionEntries placement)) of
          Just revision
            | revision >= Protocol.alignmentLostStorePlacementRevision lost ->
                maybe False (all (not . exact lost)) (Placement.placementRoutesAtRevision (Protocol.alignmentLostStoreHome lost) revision (startupPlacementState state))
          _ -> False
    exact lost route = deltaRouteDelta route == Protocol.alignmentLostStoreDelta lost && deltaRouteStoreIncarnation route == Protocol.alignmentLostStoreIncarnation lost

obsoleteEvidenceWork :: Protocol.AlignmentPlanObsolete -> Alignment.AlignmentCutEvidenceDisposition -> GenerationWork
obsoleteEvidenceWork report disposition = case disposition of
  Alignment.AlignmentCutEvidenceUnchanged -> mempty
  Alignment.AlignmentCutEvidenceRetained -> GenerationWork AlignmentGenerationWorkChanged Set.empty Set.empty (Set.singleton (Protocol.alignmentPlanObsoleteId report)) Set.empty []

obsoleteControlEntries :: HeraldEpoch -> HeraldEpoch -> Alignment.State -> [(GenerationControlKey, AlignmentControl)]
obsoleteControlEntries local remote alignment =
  [ (PlanObsoleteKey (Protocol.alignmentPlanObsoleteId report) local, AlignmentPlanObsoleteAdvertised report)
  | remote /= local,
    report <- Alignment.alignmentPlanObsoleteEntries alignment,
    Protocol.alignmentPlanObsoleteReporter report == local
  ]

-- Loss receipts preserve the exact Store identity. The current placement
-- snapshot supplies the progress witness; no timer or fabricated revision is
-- involved. A supersession receipt does not claim that a Store was lost.
obsoleteStoreFacts :: Set Plan.AlignmentPlanId -> Set ContextClassGenerationId -> HeraldState -> [Protocol.AlignmentLostStore]
obsoleteStoreFacts plans carried state = Set.toAscList (Set.fromList facts)
  where
    alignment = startupAlignmentState state
    local = checkedLocalHeraldEpoch (startupGenesis state)
    placement = startupPlacementState state
    active = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState state))))
    facts =
      [ Protocol.alignmentLostStore home delta incarnation revision
      | (coordinate, receipt) <- Alignment.alignmentPlanInvalidationEntries alignment,
        Set.member (Alignment.alignmentPlanCoordinateId coordinate) plans || not (Set.disjoint carried (Alignment.alignmentPlanInvalidationReceiptGenerations receipt)),
        cause <- Set.toAscList (Alignment.alignmentPlanInvalidationReceiptCauses receipt),
        (generationId, delta, incarnation) <- case cause of
          Alignment.AlignmentPlanDestinationStoreLost generation destination -> [(generation, Protocol.destinationStoreDelta destination, Protocol.destinationStoreIncarnation destination)]
          Alignment.AlignmentPlanFreshBaseStoreLost generation fresh _ -> [(generation, freshMemberBaseDelta fresh, freshMemberBaseStoreIncarnation fresh)]
          _ -> [],
        Just generation <- [Alignment.lookupAlignmentGeneration generationId alignment],
        member <- NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)),
        alignmentMemberDelta member == delta,
        alignmentMemberStoreIncarnation member == incarnation,
        let home = alignmentMemberHerald member,
        let currentRevision = if home == local then Placement.localPlacementSequence placement else Placement.remotePlacementSequence home placement,
        let frozenRevision = lookup home (NonEmpty.toList (physicalPlacementRevisionEntries (alignmentCutPhysicalPlacementRevisionVector (alignmentGenerationCut generation)))),
        Just revision <- [if home `notElem` active then currentRevision <|> frozenRevision else currentRevision],
        home `notElem` active || maybe False (all (\route -> deltaRouteDelta route /= delta || deltaRouteStoreIncarnation route /= incarnation)) (Placement.placementRoutesAtRevision home revision placement)
      ]

retainObsoleteReport :: Plan.AlignmentPlanId -> [Protocol.AlignmentLostStore] -> HeraldState -> Either AlignmentCoordinationProblem (HeraldState, GenerationWork)
retainObsoleteReport identifier facts state = case NonEmpty.nonEmpty facts of
  Nothing -> Right (state, mempty)
  Just lost -> do
    report <- mapLeft (const AlignmentCoordinationPendingEvidenceInvariant) (Protocol.alignmentPlanObsolete identifier (checkedLocalHeraldEpoch (startupGenesis state)) lost)
    (retained, disposition) <- mapLeft (const AlignmentCoordinationPendingEvidenceInvariant) (Alignment.retainAlignmentPlanObsolete report (startupAlignmentState state))
    Right (replaceStartupAlignmentState retained state, obsoleteEvidenceWork report disposition)

reportInvalidatedCurrentPlans :: Set SortOccurrence -> HeraldState -> Either AlignmentCoordinationProblem (HeraldState, GenerationWork)
reportInvalidatedCurrentPlans occurrences initial = foldM reportOne (initial, mempty) (Set.toAscList occurrences)
  where
    reportOne (state, work) occurrence = case Alignment.currentAlignmentPlanForSort occurrence (startupAlignmentState state) of
      Just plan
        | Alignment.alignmentPlanInvalidated (Plan.alignmentPlanId plan) (startupAlignmentState state),
          checkedLocalHeraldEpoch (startupGenesis state) `elem` map fst (NonEmpty.toList (physicalPlacementRevisionEntries (Plan.alignmentPlanIdPlacement (Plan.alignmentPlanId plan)))) -> do
            let identifier = Plan.alignmentPlanId plan
            (successor, changed) <- retainObsoleteReport identifier (obsoleteStoreFacts (Set.singleton identifier) Set.empty state) state
            Right (successor, work <> changed)
      _ -> Right (state, work)

advanceAlignmentResets :: Set SortOccurrence -> PhysicalPlacementRevisionVector -> HeraldState -> Either AlignmentCoordinationProblem (HeraldState, GenerationWork)
advanceAlignmentResets occurrences placement initial = foldM advanceOne (initial, mempty) (Set.toAscList occurrences)
  where
    advanceOne (state, work) occurrence
      | checkedLocalHeraldEpoch (startupGenesis state) /= minimum (fmap fst (physicalPlacementRevisionEntries placement)) = Right (state, work)
      | otherwise = do
          let alignment = startupAlignmentState state
              reports = Alignment.alignmentPlanObsoleteForSort occurrence alignment
              incorporated report = alignmentObsoleteCauseIncorporated state placement report
              membership = OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState state))
              epoch = fromMaybe (controlIndex 0) (heraldMembershipGenerationChangeControlIndex membership)
              knownAttempts = [Plan.alignmentPlanIdAttempt identifier | (identifier, _) <- Alignment.alignmentPlanEntries alignment, Plan.alignmentPlanIdSort identifier == occurrence]
              observedAttempts = knownAttempts <> map (Plan.alignmentPlanIdAttempt . Protocol.alignmentPlanObsoleteId) reports
          if null reports || not (all incorporated reports) || any ((> epoch) . alignmentPlanAttemptMembershipChange) observedAttempts
            then Right (state, work)
            else do
              let attempt = nextAlignmentPlanAttempt (maximum (mkAlignmentPlanAttemptAtMembership epoch 0 : observedAttempts))
                  topology = GraphProgress.structuralLastInstalledCutId (startupStructuralProgressState state)
              prepared <- prepareResetPlan occurrence attempt topology placement Map.empty state
              case prepared of
                Nothing -> Right (state, work)
                Just plan -> do
                  (promoted, changed) <- promoteResetPlan plan state
                  (accepted, acceptance) <- retainOneLocalPlanAcceptance plan promoted
                  Right (accepted, work <> changed <> acceptance)

prepareResetPlan :: SortOccurrence -> AlignmentPlanAttempt -> TopologyCutId -> PhysicalPlacementRevisionVector -> Map DeltaId StoreRevision -> HeraldState -> Either AlignmentCoordinationProblem (Maybe Plan.AlignmentPlan)
prepareResetPlan occurrence attempt topology placement bases state
  | Set.null (coveredResetCauses occurrence topology state) = Right Nothing
  | not topologyMembershipReady = Right Nothing
  | otherwise = do
      routes <- activePlacementRoutesForSort state occurrence placement
      case deriveAlignmentInputsAt state occurrence topology placement routes bases of
        Left (AlignmentCoordinationPlacementMembershipMismatch _ _) -> Right Nothing
        Left problem -> Left problem
        Right (cut, context, members) -> do
          preparation <- mapLeft AlignmentCoordinationPlanProblem (Plan.prepareAlignmentResetPlan attempt occurrence cut placement context members)
          case preparation of
            Plan.AlignmentPlanHeld _ -> Right Nothing
            Plan.AlignmentPlanReady plan -> Right (Just plan)
  where
    topologyMembershipReady =
      maybe
        False
        ((== physicalPlacementRevisionMembershipGenerationId placement) . topologyFrontierMembershipGenerationId . topologyCutFrontier . GraphProgress.installedTopologyCutCut)
        (GraphProgress.lookupInstalledTopologyCut topology (startupStructuralProgressState state))

-- The Oracle orders announcer changes. Retry ordinals are local to that
-- checked membership epoch, so a new announcer cannot collide with a retry
-- which it missed before the membership transition.
resetAttemptMembershipMatches :: Plan.AlignmentPlanId -> HeraldState -> Bool
resetAttemptMembershipMatches identifier state =
  case OracleProjection.oracleViewHeraldMembershipById (physicalPlacementRevisionMembershipGenerationId (Plan.alignmentPlanIdPlacement identifier)) (OracleProjection.oracleView (startupOracleProjectionState state)) of
    Nothing -> False
    Just membership -> alignmentPlanAttemptMembershipChange (Plan.alignmentPlanIdAttempt identifier) == fromMaybe (controlIndex 0) (heraldMembershipGenerationChangeControlIndex membership)

coveredResetCauses :: SortOccurrence -> TopologyCutId -> HeraldState -> Set StructuralConsequenceCause
coveredResetCauses occurrence topology state =
  Set.fromList
    [ structuralDebtKeyCause key
    | debt <- structuralDebtSetEntries (Alignment.alignmentStructuralDebts (startupAlignmentState state)),
      let key = structuralConsequenceDebtKey debt,
      structuralDebtKeySort key == occurrence,
      GraphProgress.installedCutCoversCause topology (structuralDebtKeyCause key) (startupStructuralProgressState state)
    ]

promoteResetPlan :: Plan.AlignmentPlan -> HeraldState -> Either AlignmentCoordinationProblem (HeraldState, GenerationWork)
promoteResetPlan plan state = do
  let identifier = Plan.alignmentPlanId plan
      occurrence = Plan.alignmentPlanIdSort identifier
      topology = Plan.alignmentPlanIdTopology identifier
      progress = startupStructuralProgressState state
      causes = coveredResetCauses occurrence topology state
  ordered <- alignmentCausesInInstalledCutProjectionOrder topology progress causes
  covered <- maybe (Left AlignmentCoordinationPendingEvidenceInvariant) Right (NonEmpty.nonEmpty ordered)
  prepared <- mapLeft AlignmentCoordinationPromotionProblem (Alignment.prepareAlignmentPlanResetPromotion (checkedLocalHeraldEpoch (startupGenesis state)) covered plan (startupAlignmentState state))
  let (alignment, disposition) = Alignment.commitAlignmentPlanResetPromotion prepared
      changed = case disposition of
        Alignment.AlignmentPromotionUnchanged -> mempty
        Alignment.AlignmentPromotionCommitted -> GenerationWork AlignmentGenerationWorkChanged Set.empty (Set.fromList (map alignmentGenerationId (currentPlanGenerations plan))) Set.empty (Set.singleton identifier) (Alignment.preparedAlignmentPlanResetPromotionCancellations prepared)
  Right (replaceStartupAlignmentState alignment state, changed)

currentPlanGenerations :: Plan.AlignmentPlan -> [AlignmentGeneration]
currentPlanGenerations = map (Plan.alignmentPlanBindingGeneration . snd) . Plan.alignmentPlanBindings

-- | Reconstruct the exact complete generation plan for one already retained
-- cut coordinate from the installed topology projection and placement
-- history for the initial attempt. Explicit routing plans replay through
-- 'replayAlignmentPlanAt', which retains their complete attempt identity.
replayAlignmentGenerationPlanAt ::
  HeraldState ->
  SortOccurrence ->
  TopologyCutId ->
  PhysicalPlacementRevisionVector ->
  Either AlignmentCoordinationProblem AlignmentGenerationPlan
replayAlignmentGenerationPlanAt state affectedSort topologyCut placement = do
  let alignment = startupAlignmentState state
      retainedGenerations =
        [ generation
        | (_, generation) <- Alignment.alignmentGenerationEntries alignment,
          generationHasCoordinate affectedSort topologyCut placement generation
        ]
  if null retainedGenerations
    then Left (AlignmentCoordinationReplayGenerationMissing affectedSort topologyCut)
    else Right ()
  let predecessorIds =
        Set.fromList
          [ predecessor
          | generation <- retainedGenerations,
            predecessor <-
              alignmentCutPredecessorGenerationIds
                (alignmentGenerationCut generation)
          ]
      prior =
        [ generation
        | identifier <- Set.toAscList predecessorIds,
          Just generation <- [Alignment.lookupAlignmentGeneration identifier alignment]
        ]
      priorRelations =
        filter
          (relationWithin predecessorIds)
          (Alignment.alignmentGenerationRelationEntries alignment)
      suppliedFreshBases =
        Map.fromList
          [ (freshMemberBaseDelta evidence, freshMemberBaseRevision evidence)
          | generation <- retainedGenerations,
            evidence <-
              alignmentCutFreshMemberBaseEvidence
                (alignmentGenerationCut generation)
          ]
  if length prior == Set.size predecessorIds
    then Right ()
    else Left (AlignmentCoordinationReplayGenerationMissing affectedSort topologyCut)
  activeRoutes <- activePlacementRoutesForSort state affectedSort placement
  preparation <-
    deriveAlignmentGenerationPreparationAt
      state
      affectedSort
      topologyCut
      placement
      activeRoutes
      suppliedFreshBases
      prior
      priorRelations
      (retainedCertificateGenerations alignment)
  case preparation of
    AlignmentGenerationHeld missing ->
      Left (AlignmentCoordinationGenerationHeld missing)
    AlignmentGenerationReady plan -> Right plan

deriveAlignmentGenerationPreparationAt ::
  HeraldState ->
  SortOccurrence ->
  TopologyCutId ->
  PhysicalPlacementRevisionVector ->
  [(HeraldEpoch, DeltaRoute)] ->
  Map DeltaId StoreRevision ->
  [AlignmentGeneration] ->
  [AlignmentGenerationRelation] ->
  Set ContextClassGenerationId ->
  Either AlignmentCoordinationProblem AlignmentGenerationPreparation
deriveAlignmentGenerationPreparationAt state affectedSort topologyCut placement activeRoutes suppliedFreshBases prior priorRelations certificates = do
  (topology, context, members) <- deriveAlignmentInputsAt state affectedSort topologyCut placement activeRoutes suppliedFreshBases
  mapLeft
    AlignmentCoordinationGenerationProblem
    ( prepareAlignmentGenerationPlan
        affectedSort
        topology
        placement
        context
        members
        prior
        priorRelations
        certificates
    )

deriveAlignmentInputsAt :: HeraldState -> SortOccurrence -> TopologyCutId -> PhysicalPlacementRevisionVector -> [(HeraldEpoch, DeltaRoute)] -> Map DeltaId StoreRevision -> Either AlignmentCoordinationProblem (TopologyCut, ContextGraph, [GenerationMemberInput])
deriveAlignmentInputsAt state affectedSort topologyCut _placement activeRoutes suppliedFreshBases = do
  installed <-
    maybe
      ( Left
          ( AlignmentCoordinationTopologyProblem
              (GraphProgress.StructuralProgressSourceTopologyUnavailable topologyCut)
          )
      )
      Right
      ( GraphProgress.lookupInstalledTopologyCut
          topologyCut
          (startupStructuralProgressState state)
      )
  projection <-
    mapLeft
      AlignmentCoordinationTopologyProblem
      ( GraphProgress.structuralProjectionAtInstalledCut
          (structuralReconciliationViews state)
          topologyCut
          (startupStructuralProgressState state)
      )
  let members =
        [ generationMemberInput
            (deltaRouteDelta route)
            (deltaRouteStoreIncarnation route)
            owner
            ( Map.findWithDefault
                initialStoreRevision
                (deltaRouteDelta route)
                suppliedFreshBases
            )
        | (owner, route) <- activeRoutes
        ]
      expectedApplicationMembers =
        [ delta
        | projectionVertex <-
            Map.elems
              (Reconciliation.structuralProjectionSnapshotVertices projection),
          Reconciliation.DeltaVertexProjection _ delta typedSort _ activity <-
            [projectionVertex],
          typedSort == affectedSort,
          activityIsActive activity
        ]
      activeApplicationRoutes =
        [ (owner, route)
        | (owner, route) <- activeRoutes,
          placementDeltaRouteCoordinate owner route /= Nothing
        ]
      observedApplicationMembers = fmap (deltaRouteDelta . snd) activeApplicationRoutes
      vertices =
        Graph.graphNonStructuralBaselineVertices (startupGraphState state)
          <> Reconciliation.structuralProjectionSnapshotAdmissionVertices projection
          <> fmap
            Reconciliation.structuralVertexProjectionVertex
            (Map.elems (Reconciliation.structuralProjectionSnapshotVertices projection))
      vertexSet = Set.fromList vertices
      edges =
        filter
          ( \edge ->
              Set.member (edgeSource edge) vertexSet
                && Set.member (edgeDestination edge) vertexSet
          )
          ( Graph.graphNonStructuralBaselineEdges (startupGraphState state)
              <> fmap
                Reconciliation.ownedEdgeProjectionPayload
                (Map.elems (Reconciliation.structuralProjectionSnapshotEdges projection))
          )
      topology = GraphProgress.installedTopologyCutCut installed
  if applicationRoutesMatchProjection affectedSort projection activeRoutes
    then Right ()
    else
      Left
        ( AlignmentCoordinationPlacementMembershipMismatch
            (sort expectedApplicationMembers)
            (sort observedApplicationMembers)
        )
  context <-
    mapLeft
      AlignmentCoordinationContextProblem
      (deriveContextGraph vertices edges (fmap (deltaRouteDelta . snd) activeRoutes))
  Right (topology, context, members)

-- Rebuild an explicit mixed-age plan against its exact retained predecessor.
replayAlignmentPlanAt :: HeraldState -> Plan.AlignmentPlanId -> Either AlignmentCoordinationProblem Plan.AlignmentPlan
replayAlignmentPlanAt state identifier = do
  retained <- maybe (Left (AlignmentCoordinationReplayGenerationMissing (Plan.alignmentPlanIdSort identifier) (Plan.alignmentPlanIdTopology identifier))) Right (Alignment.lookupAlignmentPlan identifier (startupAlignmentState state))
  replayAnnouncedAlignmentPlan state (Plan.alignmentPlanAnnouncement retained)

-- Passive sealed history fixes its predecessor policy; this reconstructs no
-- executable owners and does not establish local invalidation or acceptance.
replayAnnouncedAlignmentPlan :: HeraldState -> Protocol.AlignmentPlanAnnounce -> Either AlignmentCoordinationProblem Plan.AlignmentPlan
replayAnnouncedAlignmentPlan state announce = do
  let identifier = Protocol.alignmentPlanAnnounceId announce
      occurrence = Plan.alignmentPlanIdSort identifier
      topology = Plan.alignmentPlanIdTopology identifier
      placement = Plan.alignmentPlanIdPlacement identifier
      alignment = startupAlignmentState state
      bases = Map.fromList [(freshMemberBaseDelta evidence, freshMemberBaseRevision evidence) | cut <- Protocol.alignmentPlanAnnounceCreatedCuts announce, evidence <- alignmentCutFreshMemberBaseEvidence (alignmentCutAnnounceCut cut)]
  if Protocol.alignmentPlanAnnouncePredecessorStatus announce == Plan.AlignmentPredecessorReset && not (resetAttemptMembershipMatches identifier state)
    then Left (AlignmentCoordinationPlanProblem Plan.AlignmentPlanResetShapeMismatch)
    else Right ()
  prior <- traverse (\parent -> maybe (Left (AlignmentCoordinationReplayGenerationMissing occurrence topology)) Right (Alignment.lookupAlignmentPlan parent alignment)) (Protocol.alignmentPlanAnnouncePredecessor announce)
  routes <- activePlacementRoutesForSort state occurrence placement
  (cut, context, members) <- deriveAlignmentInputsAt state occurrence topology placement routes bases
  preparation <-
    mapLeft
      AlignmentCoordinationPlanProblem
      ( case Protocol.alignmentPlanAnnouncePredecessorStatus announce of
          Plan.AlignmentPredecessorReset -> Plan.prepareAlignmentResetPlan (Plan.alignmentPlanIdAttempt identifier) occurrence cut placement context members
          _ -> Plan.prepareAlignmentPlanAtAttempt (Plan.alignmentPlanIdAttempt identifier) occurrence cut placement context members (fmap (\parent -> (parent, Protocol.alignmentPlanAnnouncePredecessorStatus announce)) prior) (retainedCertificateGenerations alignment)
      )
  case preparation of
    Plan.AlignmentPlanHeld missing -> Left (AlignmentCoordinationGenerationHeld missing)
    Plan.AlignmentPlanReady plan -> mapLeft AlignmentCoordinationPlanProblem (Plan.checkAlignmentPlanAnnouncement plan announce)

-- | Select the exact route set from which generation-plan members and the
-- active-delta context seed are derived. Keeping this selection outside the
-- expensive structural projection makes the deterministic anchor preflight
-- observationally identical to the eventual plan shape.
activePlacementRoutesForSort ::
  HeraldState ->
  SortOccurrence ->
  PhysicalPlacementRevisionVector ->
  Either AlignmentCoordinationProblem [(HeraldEpoch, DeltaRoute)]
activePlacementRoutesForSort state affectedSort placement = do
  routes <-
    mapLeft
      AlignmentCoordinationPlacementProblem
      (Placement.placementRoutesAtVector placement (startupPlacementState state))
  Right
    [ (owner, route)
    | (owner, ownerRoutes) <- routes,
      route <- ownerRoutes,
      deltaRouteSortId route == sortOccurrenceSortId affectedSort,
      deltaRouteOccurrenceId route == sortOccurrenceDefinition affectedSort
    ]

generationHasCoordinate ::
  SortOccurrence ->
  TopologyCutId ->
  PhysicalPlacementRevisionVector ->
  AlignmentGeneration ->
  Bool
generationHasCoordinate affectedSort topologyCut placement generation =
  Plan.alignmentGenerationBirthPlanId generation
    == Plan.alignmentPlanIdFromClaimedCoordinates affectedSort topologyCut placement

relationWithin ::
  Set ContextClassGenerationId -> AlignmentGenerationRelation -> Bool
relationWithin identifiers relation =
  Set.member (alignmentGenerationRelationSource relation) identifiers
    && Set.member (alignmentGenerationRelationDestination relation) identifiers

retainedCertificateGenerations :: Alignment.State -> Set ContextClassGenerationId
retainedCertificateGenerations alignment =
  Set.fromList
    [ generationId
    | ((generationId, _), _) <- Alignment.alignmentHistoricalCertificateEntries alignment
    ]

-- | Compare the exact application-Delta route coordinates represented by an
-- installed structural projection with one retained placement cut. This is
-- both the final plan-admission check and the recovery-cut selector: keeping
-- one predicate prevents peers from disagreeing about which successor cut is
-- the first viable coordinate. Store incarnation is intentionally absent; an
-- incarnation replacement is expressed by the placement revision vector.
applicationRoutesMatchProjection ::
  SortOccurrence ->
  Reconciliation.StructuralProjectionSnapshot ->
  [(HeraldEpoch, DeltaRoute)] ->
  Bool
applicationRoutesMatchProjection affectedSort projection activeRoutes =
  case (expectedApplicationRouteCoordinates, observedApplicationRouteCoordinates) of
    (Just expected, Just observed) -> sort expected == sort observed
    _ -> False
  where
    expectedApplicationRouteCoordinates =
      traverse
        (installedDeltaRouteCoordinate projection)
        [ (object, projectionVertex)
        | (object, projectionVertex) <-
            Map.toAscList
              (Reconciliation.structuralProjectionSnapshotVertices projection),
          Reconciliation.DeltaVertexProjection _ _ typedSort _ activity <-
            [projectionVertex],
          typedSort == affectedSort,
          activityIsActive activity
        ]
    observedApplicationRouteCoordinates =
      traverse
        (uncurry placementDeltaRouteCoordinate)
        [ (owner, route)
        | (owner, route) <- activeRoutes,
          placementDeltaRouteCoordinate owner route /= Nothing
        ]

retainOneLocalCutAcceptance ::
  AlignmentGeneration ->
  HeraldState ->
  Either
    AlignmentCoordinationProblem
    (HeraldState, GenerationWork)
retainOneLocalCutAcceptance generation predecessor = do
  let generationId = alignmentGenerationId generation
      local = checkedLocalHeraldEpoch (startupGenesis predecessor)
      alignment = startupAlignmentState predecessor
  case Alignment.lookupAlignmentCutAcceptance generationId local alignment of
    Just _ ->
      Right (predecessor, mempty)
    Nothing -> do
      let cut = alignmentGenerationCut generation
          accepted =
            alignmentCutAccepted
              generationId
              local
              (deriveTopologyCutId (alignmentCutTopologyCut cut))
              (alignmentCutPhysicalPlacementRevisionVector cut)
              ( Publication.publicationCurrentHeraldPrefix
                  (startupPublicationState predecessor)
              )
      prepared <-
        mapLeft
          AlignmentCoordinationCutEvidenceProblem
          (Alignment.prepareAlignmentCutAcceptance accepted alignment)
      let (successorAlignment, disposition) =
            Alignment.commitAlignmentCutAcceptance prepared
      Right
        ( replaceStartupAlignmentState successorAlignment predecessor,
          cutEvidenceWorkDisposition generationId disposition
        )

planGenerations :: AlignmentGenerationPlan -> [AlignmentGeneration]
planGenerations plan =
  [ generation
  | (_, generation) <-
      sortOn
        fst
        [ (alignmentGenerationId generation, generation)
        | generation <-
            alignmentGenerationPlanGenerations plan
        ]
  ]

alignmentPlanAnchorGeneration ::
  AlignmentGenerationPlan -> Maybe AlignmentGeneration
alignmentPlanAnchorGeneration = anchorGeneration . planGenerations

anchorGeneration :: [AlignmentGeneration] -> Maybe AlignmentGeneration
anchorGeneration generations =
  case sortOn
    ( minimum
        . fmap alignmentMemberDelta
        . NonEmpty.toList
        . alignmentCutExactMembers
        . alignmentGenerationCut
    )
    generations of
    [] -> Nothing
    anchor : _ -> Just anchor

-- | The immutable source capture hands the old coordinator's last authored
-- frontier to the joining member. Keep completing and replaying those plans,
-- but do not author another frontier behind that capture. Activation,
-- cancellation, and an invalidated attempt release this barrier through the
-- committed admission projection; a retained capture from an older attempt
-- cannot freeze a new one.
alignmentPromotionHandoffPending :: HeraldState -> Bool
alignmentPromotionHandoffPending state =
  case OracleProjection.oracleViewPendingHeraldAdmission
    (OracleProjection.oracleView (startupOracleProjectionState state)) of
    Nothing -> False
    Just record ->
      isJust
        ( Join.lookupCapture
            (admissionRecordId record)
            (admissionRecordAttempt record)
            (startupJoinState state)
        )

-- | Classify retained debt without confusing live coverage with whether the
-- currently selected coordinate may be promoted.  In particular, an exact
-- tombstone is uncovered but cannot be replayed into executable work, whereas
-- a different vector becomes due under the recovery authorization below.
alignmentCausePromotionDisposition ::
  StructuralConsequenceCause ->
  SortOccurrence ->
  TopologyCutId ->
  PhysicalPlacementRevisionVector ->
  Alignment.State ->
  AlignmentCausePromotionDisposition
alignmentCausePromotionDisposition cause affectedSort topology placement alignment
  | any (not . coordinateInvalidated) incumbentKeys = AlignmentCauseCovered
  | requestedCoordinateInvalidated = AlignmentCausePromotionBlocked
  | promotionAuthorized = AlignmentCausePromotionDue
  | otherwise = AlignmentCausePromotionBlocked
  where
    incumbentKeys =
      alignmentCausePromotionKeys cause affectedSort alignment
    coordinateInvalidated =
      alignmentPromotionCoordinateInvalidated affectedSort alignment
    requestedCoordinateInvalidated =
      alignmentPlanCoordinateInvalidated
        affectedSort
        topology
        placement
        alignment
    promotionAuthorized =
      alignmentCausePromotionAuthorized
        cause
        affectedSort
        topology
        placement
        alignment

-- | One consequence cause has one active anchor coordinate. A distinct
-- coordinate is recovery work only after every prior coordinate for the same
-- cause/sort has been tombstoned. Recovery may advance topology, but the
-- coordinator above admits only the canonical earliest viable installed cut
-- at or after the latest incumbent. This owner-local predicate deliberately
-- supplies only the all-invalidated authorization. An exact retained
-- coordinate remains replay-idempotent (and an exact tombstone is rejected by
-- 'alignmentCausePromotionDisposition' before this predicate is consulted).
alignmentCausePromotionAuthorized ::
  StructuralConsequenceCause ->
  SortOccurrence ->
  TopologyCutId ->
  PhysicalPlacementRevisionVector ->
  Alignment.State ->
  Bool
alignmentCausePromotionAuthorized cause affectedSort topology placement alignment =
  case incumbentKeys of
    [] -> True
    _
      | any isExact incumbentKeys -> True
      | otherwise -> all coordinateInvalidated incumbentKeys
  where
    incumbentKeys =
      alignmentCausePromotionKeys cause affectedSort alignment
    isExact key =
      Plan.alignmentPlanIdAttempt (Alignment.alignmentPromotionKeyPlanId key) == maybe initialAlignmentPlanAttempt (Plan.alignmentPlanIdAttempt . Plan.alignmentPlanId) (Alignment.currentAlignmentPlanForSort affectedSort alignment)
        && Alignment.alignmentPromotionKeyTopologyCut key == topology
        && Alignment.alignmentPromotionKeyPlacementVector key == placement
    coordinateInvalidated =
      alignmentPromotionCoordinateInvalidated affectedSort alignment

alignmentCausePromotionKeys ::
  StructuralConsequenceCause ->
  SortOccurrence ->
  Alignment.State ->
  [Alignment.AlignmentPromotionKey]
alignmentCausePromotionKeys = Alignment.alignmentCauseCoverageKeysFor

alignmentPromotionCoordinateInvalidated ::
  SortOccurrence ->
  Alignment.State ->
  Alignment.AlignmentPromotionKey ->
  Bool
alignmentPromotionCoordinateInvalidated _ alignment key =
  Alignment.alignmentPlanInvalidated (Alignment.alignmentPromotionKeyPlanId key) alignment

alignmentPlanCoordinateInvalidated ::
  SortOccurrence ->
  TopologyCutId ->
  PhysicalPlacementRevisionVector ->
  Alignment.State ->
  Bool
alignmentPlanCoordinateInvalidated occurrence topology placement alignment =
  Alignment.alignmentPlanInvalidated
    (Plan.alignmentPlanIdFromClaimedCoordinatesAtAttempt attempt occurrence topology placement)
    alignment
  where
    attempt = maybe initialAlignmentPlanAttempt (Plan.alignmentPlanIdAttempt . Plan.alignmentPlanId) (Alignment.currentAlignmentPlanForSort occurrence alignment)

-- | Cheap necessary-and-sufficient plan-shape check for spontaneous work.
-- Empty plans also retain a parent coordinate, so their authority is the same
-- fixed least Herald used for plans with active routes.
spontaneousAlignmentPromotionMayDerive ::
  HeraldEpoch -> NonEmpty HeraldEpoch -> Bool -> Bool
spontaneousAlignmentPromotionMayDerive local fixedMembers _ =
  local == minimum (NonEmpty.toList fixedMembers)

-- | Pure policy used by the whole-Herald coordinator and convergence
-- properties. Every current plan is frozen by the fixed least Herald.
spontaneousAlignmentPromotionAuthorized ::
  HeraldEpoch -> NonEmpty HeraldEpoch -> AlignmentGenerationPlan -> Bool
spontaneousAlignmentPromotionAuthorized local fixedMembers plan =
  spontaneousAlignmentPromotionMayDerive
    local
    fixedMembers
    (not (null (alignmentGenerationPlanGenerations plan)))

-- | Plan-independent source admission for anchor replay.  The exact announced
-- generation still requires the derived plan below, but a non-least source can
-- never satisfy that later check and must not trigger speculative derivation.
anchorAlignmentPromotionMayDerive :: HeraldEpoch -> NonEmpty HeraldEpoch -> Bool
anchorAlignmentPromotionMayDerive remote fixedMembers =
  remote == minimum (NonEmpty.toList fixedMembers)

-- | An unpromoted peer may replay only the fixed least Herald's exact plan
-- anchor announcement.
anchorAlignmentPromotionAuthorized ::
  HeraldEpoch ->
  NonEmpty HeraldEpoch ->
  ContextClassGenerationId ->
  AlignmentGenerationPlan ->
  Bool
anchorAlignmentPromotionAuthorized remote fixedMembers announced plan =
  anchorAlignmentPromotionMayDerive remote fixedMembers
    && maybe
      False
      ((== announced) . alignmentGenerationId)
      (alignmentPlanAnchorGeneration plan)

alignmentNewlyEnabledEffects :: GenerationWork -> HeraldState -> HeraldState -> EffectBatch
alignmentNewlyEnabledEffects work predecessor successor =
  orderedEffectBatch
    (alignmentNewlyEnabledEffectMembers work predecessor successor)

alignmentNewlyEnabledEffectMembers :: GenerationWork -> HeraldState -> HeraldState -> [HeraldEffect]
alignmentNewlyEnabledEffectMembers work predecessor successor
  | Set.null candidates && Set.null selectedPlans && null work.cancellations = []
  | otherwise =
      cancellationEffects
        <> concatMap
          newlyEnabledForPeer
          [remote | remote <- NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState successor)))), remote /= local]
  where
    before = startupAlignmentState predecessor
    after = startupAlignmentState successor
    changedPlans = Alignment.alignmentControlPlanGenerations work.promotedGenerations before after
    candidates = work.evidenceGenerations <> changedPlans
    selectedPlans = work.evidencePlans <> work.promotedPlans
    local = checkedLocalHeraldEpoch (startupGenesis successor)
    cancellationEffects =
      [ SendPeerControl binding (PeerAlignmentControl (AlignmentCancelled cancellation))
      | cancellation <- nub work.cancellations,
        Just destination <- [Transfer.lookupDestinationSubscription (Protocol.alignmentCancelSubscriptionId cancellation) (Alignment.alignmentTransferState after)],
        Just remote <- [fmap Protocol.alignmentAttemptSourceHerald (Transfer.destinationSubscriptionAttempt destination) <|> Transfer.destinationSubscriptionBootstrapSourceHerald destination],
        remote /= local,
        Just binding <- [Discovery.currentPeerBinding remote (startupDiscoveryState successor)]
      ]
    newlyEnabledForPeer remote =
      concatMap (emit remote) (alignmentSelectedControlDeltaForPlans (Just selectedPlans) (Just changedPlans) candidates local remote before after)
    emit remote control
      | Just _ <- DeliveryOwner.evidenceKey control = [QueuePeerAlignmentEvidence remote control]
      | Just binding <- Discovery.currentPeerBinding remote (startupDiscoveryState successor) = [SendPeerControl binding (PeerAlignmentControl control)]
      | otherwise = []

requireGeneration ::
  ContextClassGenerationId ->
  Alignment.State ->
  Either AlignmentGenerationControlProblem AlignmentGeneration
requireGeneration generationId state =
  maybe
    (Left AlignmentGenerationControlProtocolViolation)
    Right
    (Alignment.lookupAlignmentGeneration generationId state)

expectedCutAnnouncer ::
  Set ContextClassGenerationId -> AlignmentGeneration -> HeraldEpoch
expectedCutAnnouncer anchorGenerationIds generation
  | Set.member (alignmentGenerationId generation) anchorGenerationIds =
      minimum
        ( fmap
            fst
            ( NonEmpty.toList
                ( physicalPlacementRevisionEntries
                    ( alignmentCutPhysicalPlacementRevisionVector
                        (alignmentGenerationCut generation)
                    )
                )
            )
        )
  | otherwise = alignmentGenerationAnnouncer generation

-- | Every retained generation was inserted by a checked promotion receipt.
-- The receipt already groups the complete plan at one exact sort, topology,
-- and placement coordinate.  Recover its anchor directly instead of
-- rediscovering sibling groups by repeatedly hashing immutable topology cuts.
retainedAnchorGenerationIds :: Alignment.State -> Set ContextClassGenerationId
retainedAnchorGenerationIds state =
  Set.fromList
    [ alignmentGenerationId anchor
    | identifiers <- Map.elems generationIdsByCoordinate,
      let retainedGenerations =
            [ generation
            | identifier <- Set.toAscList identifiers,
              Just generation <- [Alignment.lookupAlignmentGeneration identifier state]
            ],
      Just anchor <- [anchorGeneration retainedGenerations]
    ]
  where
    generationIdsByCoordinate =
      Map.fromListWith
        Set.union
        [ ( Alignment.alignmentPromotionKeyPlanId key,
            Set.fromList
              (Alignment.alignmentPromotionReceiptGenerationIds receipt)
          )
        | (key, receipt) <- Alignment.alignmentPromotionEntries state
        ]

memberHeraldForStore ::
  StoreIncarnationId ->
  AlignmentGeneration ->
  Maybe HeraldEpoch
memberHeraldForStore store generation =
  alignmentMemberHerald
    <$> find
      ((== store) . alignmentMemberStoreIncarnation)
      (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)))

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

requireProtocol :: Bool -> Either AlignmentGenerationControlProblem ()
requireProtocol True = Right ()
requireProtocol False = Left AlignmentGenerationControlProtocolViolation

mapProtocol :: Either problem value -> Either AlignmentGenerationControlProblem value
mapProtocol = either (const (Left AlignmentGenerationControlProtocolViolation)) Right

mapInvariant ::
  Either AlignmentCoordinationProblem value ->
  Either AlignmentGenerationControlProblem value
mapInvariant =
  either (Left . AlignmentGenerationControlInvariantViolation) Right

mapLocalPlanAsProtocol ::
  Either AlignmentCoordinationProblem value ->
  Either AlignmentGenerationControlProblem value
mapLocalPlanAsProtocol =
  either (const (Left AlignmentGenerationControlProtocolViolation)) Right

mapLeft :: (left -> mapped) -> Either left value -> Either mapped value
mapLeft mapping = either (Left . mapping) Right

planEvidenceWork :: Plan.AlignmentPlan -> Alignment.AlignmentCutEvidenceDisposition -> GenerationWork
planEvidenceWork plan disposition = case disposition of
  Alignment.AlignmentCutEvidenceUnchanged -> mempty
  Alignment.AlignmentCutEvidenceRetained -> GenerationWork AlignmentGenerationWorkChanged (Set.fromList (map alignmentGenerationId (currentPlanGenerations plan))) Set.empty (Set.singleton (Plan.alignmentPlanId plan)) Set.empty []

retainOneLocalPlanAcceptance :: Plan.AlignmentPlan -> HeraldState -> Either AlignmentCoordinationProblem (HeraldState, GenerationWork)
retainOneLocalPlanAcceptance plan state =
  case Alignment.lookupAlignmentPlanAcceptance identifier local alignment of
    Just _ -> Right (state, mempty)
    Nothing -> do
      accepted <- mapLeft (const AlignmentCoordinationPendingEvidenceInvariant) (Protocol.alignmentPlanAccepted identifier local (Publication.publicationCurrentHeraldPrefix (startupPublicationState state)))
      (retained, disposition) <- mapLeft AlignmentCoordinationCutEvidenceProblem (Alignment.retainAlignmentPlanAcceptance accepted alignment)
      Right (replaceStartupAlignmentState retained state, planEvidenceWork plan disposition)
  where
    identifier = Plan.alignmentPlanId plan
    local = checkedLocalHeraldEpoch (startupGenesis state)
    alignment = startupAlignmentState state

-- Old attempts retain immutable evidence for history, but delayed executable
-- controls cannot revive them after an explicit fresh reset.
supersededGenerationEvidence :: AlignmentControl -> Alignment.State -> Bool
supersededGenerationEvidence control alignment = case control of
  AlignmentPlanAnnounced announce -> planAttemptSuperseded (Protocol.alignmentPlanAnnounceId announce) alignment
  AlignmentPlanAcceptanceAdvertised accepted -> planAttemptSuperseded (Protocol.alignmentPlanAcceptedId accepted) alignment
  AlignmentCutAnnounced announce ->
    let cut = alignmentCutAnnounceCut announce
        occurrence = sortOccurrence (alignmentCutSortId cut) (alignmentCutSortDefinitionOccurrenceId cut)
        identifier = Plan.alignmentPlanIdFromClaimedCoordinatesAtAttempt (alignmentCutAttempt cut) occurrence (deriveTopologyCutId (alignmentCutTopologyCut cut)) (alignmentCutPhysicalPlacementRevisionVector cut)
     in planAttemptSuperseded identifier alignment
  AlignmentCutAcceptanceAdvertised accepted -> oldGeneration (alignmentCutAcceptedGeneration accepted)
  _ -> False
  where
    oldGeneration identifier = maybe False (\generation -> planAttemptSuperseded (Plan.alignmentGenerationBirthPlanId generation) alignment) (Alignment.lookupAlignmentGeneration identifier alignment)

planAttemptSuperseded :: Plan.AlignmentPlanId -> Alignment.State -> Bool
planAttemptSuperseded identifier alignment =
  maybe False ((> Plan.alignmentPlanIdAttempt identifier) . Plan.alignmentPlanIdAttempt . Plan.alignmentPlanId) (Alignment.currentAlignmentPlanForSort (Plan.alignmentPlanIdSort identifier) alignment)

planAnnounceResolved :: Protocol.AlignmentPlanAnnounce -> HeraldState -> Bool
planAnnounceResolved announce state =
  planAttemptSuperseded identifier alignment
    || any (\report -> Protocol.alignmentPlanObsoleteId report == identifier && Protocol.alignmentPlanObsoleteReporter report == local) (Alignment.alignmentPlanObsoleteForSort (Plan.alignmentPlanIdSort identifier) alignment)
    || isJust (Alignment.lookupAlignmentPlanAcceptance identifier local alignment)
  where
    identifier = Protocol.alignmentPlanAnnounceId announce
    local = checkedLocalHeraldEpoch (startupGenesis state)
    alignment = startupAlignmentState state

applyPlanAnnounce :: AlignmentCutSelection -> HeraldEpoch -> Protocol.AlignmentPlanAnnounce -> HeraldState -> Either AlignmentGenerationControlProblem (HeraldState, GenerationWork)
applyPlanAnnounce selection remote announce state = do
  requireProtocol (remote == minimum (fmap fst (physicalPlacementRevisionEntries placement)))
  if planAttemptSuperseded identifier alignment
    then Right (state, mempty)
    else case NonEmpty.nonEmpty lost of
      Just facts -> mapInvariant (retainObsoleteReport identifier (NonEmpty.toList facts) state)
      Nothing -> admit
  where
    identifier = Protocol.alignmentPlanAnnounceId announce
    occurrence = Plan.alignmentPlanIdSort identifier
    topology = Plan.alignmentPlanIdTopology identifier
    placement = Plan.alignmentPlanIdPlacement identifier
    alignment = startupAlignmentState state
    parentStatus = Protocol.alignmentPlanAnnouncePredecessorStatus announce
    failedParents = Set.fromList (identifier : [parent | parentStatus == Plan.AlignmentPredecessorUsable, Just parent <- [Protocol.alignmentPlanAnnouncePredecessor announce]])
    carried = Set.fromList [Protocol.alignmentPlanBindingClaimGeneration binding | binding <- Protocol.alignmentPlanAnnounceBindings announce, Protocol.alignmentPlanBindingClaimDisposition binding == Plan.AlignmentCarried]
    lost = obsoleteStoreFacts failedParents carried state
    bases = Map.fromList [(freshMemberBaseDelta evidence, freshMemberBaseRevision evidence) | cut <- Protocol.alignmentPlanAnnounceCreatedCuts announce, evidence <- alignmentCutFreshMemberBaseEvidence (alignmentCutAnnounceCut cut)]
    admit = case Alignment.lookupAlignmentPlan identifier alignment of
      Just retained -> do
        _ <- mapProtocol (Plan.checkAlignmentPlanAnnouncement retained announce)
        mapInvariant (retainOneLocalPlanAcceptance retained state)
      Nothing -> case GraphProgress.lookupInstalledTopologyCut topology (startupStructuralProgressState state) of
        Nothing -> Right (state, mempty)
        Just installed -> do
          requireProtocol (GraphProgress.installedTopologyCutCut installed == Protocol.alignmentPlanAnnounceTopology announce)
          case Placement.placementRoutesAtVector placement (startupPlacementState state) of
            Left _ -> Right (state, mempty)
            Right _ | parentStatus == Plan.AlignmentPredecessorReset -> do
              requireProtocol (maybe True ((< Plan.alignmentPlanIdAttempt identifier) . Plan.alignmentPlanIdAttempt . Plan.alignmentPlanId) (Alignment.currentAlignmentPlanForSort occurrence alignment))
              requireProtocol (resetAttemptMembershipMatches identifier state)
              prepared <- mapInvariant (prepareResetPlan occurrence (Plan.alignmentPlanIdAttempt identifier) topology placement bases state)
              case prepared of
                Nothing -> Right (state, mempty)
                Just plan -> do
                  _ <- mapProtocol (Plan.checkAlignmentPlanAnnouncement plan announce)
                  (promoted, changed) <- mapInvariant (promoteResetPlan plan state)
                  (accepted, acceptance) <- mapInvariant (retainOneLocalPlanAcceptance plan promoted)
                  Right (accepted, changed <> acceptance)
            Right _ -> do
              let parentRetained = case Protocol.alignmentPlanAnnouncePredecessor announce of
                    Nothing -> Alignment.currentAlignmentPlanForSort occurrence alignment == Nothing
                    Just parent ->
                      fmap Plan.alignmentPlanId (Alignment.currentAlignmentPlanForSort occurrence alignment) == Just parent
                        && (Alignment.alignmentPlanInvalidated parent alignment == (parentStatus == Plan.AlignmentPredecessorInvalidated))
              if not parentRetained
                then Right (state, mempty)
                else do
                  groups <- fmap (filter ((== occurrence) . (.affectedSort))) (mapLocalPlanAsProtocol (pendingGroupsAtPlacement selection placement state))
                  let dispositions group = (group.topologyCut, [alignmentCausePromotionDisposition cause occurrence group.topologyCut placement alignment | cause <- Set.toAscList group.causes])
                  if not (alignmentReplayTargetIsNext topology (map dispositions groups))
                    then Right (state, mempty)
                    else do
                      group <- maybe (Left AlignmentGenerationControlProtocolViolation) Right (mergePendingGroups topology occurrence (filter ((== topology) . (.topologyCut)) groups))
                      promoted <- mapLocalPlanAsProtocol (prepareGroup (AnnouncedAlignmentPromotion remote identifier) placement bases group state)
                      case promoted of
                        Nothing -> Right (state, mempty)
                        Just (after, plan, work) -> do
                          _ <- mapProtocol (Plan.checkAlignmentPlanAnnouncement plan announce)
                          (accepted, acceptanceWork) <- mapInvariant (retainOneLocalPlanAcceptance plan after)
                          Right (accepted, work <> acceptanceWork)

applyHistoricalAlignmentPlan :: HeraldEpoch -> Protocol.AlignmentPlanAnnounce -> HeraldState -> Either AlignmentGenerationControlProblem (HeraldState, EffectBatch)
applyHistoricalAlignmentPlan remote announce state = do
  let (selection, prepared) = prepareAlignmentCutSelection (controlPlacementVectors (AlignmentPlanAnnounced announce)) state
  (successor, work) <- applyPlanAnnounce selection remote announce prepared
  Right (trimAlignmentCutSelection selection successor, alignmentNewlyEnabledEffects work state successor)
