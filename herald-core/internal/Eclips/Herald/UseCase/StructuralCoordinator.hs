-- | Whole-Herald coordination for source sequencing, topology proposal, and
-- application settlement.  Keeping these three phases behind one seam ensures
-- that a singleton cut cannot escape installed while its newly covered local
-- operation is still reported as pending.
module Eclips.Herald.UseCase.StructuralCoordinator
  ( StructuralCoordinationProblem (..),
    advanceLocalStructuralWork,
    advanceSuccessorStructuralBaseWork,
    settleInstalledStructuralCuts,
    releasePendingAlignmentRouteCutovers,
    PendingAlignmentRouteCutoverDisposition (..),
    releasePendingAlignmentRouteCutoversWithDisposition,
    advanceAlignmentAndPending,
  )
where

import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( DeltaId,
    GlobalObjectId,
    ProcessEpochId,
    PublicationId,
    TopologyCutId,
  )
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    structuralConsequenceCauseControlIndex,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    PeerProtocolDisposition (ClosePeerPublicationProtocol),
    orderedEffectBatch,
  )
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch)
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource (SuccessorStructuralBase, successorStructuralBaseGenerationId)
import Eclips.Herald.Input (PeerControl (..))
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerStream (streamDirectionSource)
import Eclips.Herald.Placement
  ( DeltaRoute,
    PlacementUpdate (FullPlacementSnapshot),
    applicationDeltaRoute,
    privateSystemViewDeltaRoute,
  )
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.Invariant (HeraldInvariantFault)
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupAlignmentCutQueryCache,
    replaceStartupAlignmentState,
    replaceStartupApplicationState,
    replaceStartupControlledState,
    replaceStartupDisappearanceState,
    replaceStartupGraphState,
    replaceStartupIdGeneratorState,
    replaceStartupLabelBarrierState,
    replaceStartupPeerStreamState,
    replaceStartupPlacementState,
    replaceStartupPublicationState,
    replaceStartupSortRegistryState,
    replaceStartupStoreState,
    replaceStartupStructuralProgressState,
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
    startupLabelBarrierState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralBaseCoordinator,
    startupStructuralProgressState,
    startupWaitState,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    sortOccurrence,
    sortOccurrenceDefinition,
    sortOccurrenceSortId,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.UseCase.Alignment qualified as Alignment
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransfer
import Eclips.Herald.UseCase.PeerControl qualified as PeerControl
import Eclips.Herald.UseCase.PeerInput qualified as PeerInput
import Eclips.Herald.UseCase.Step15StructuralBase qualified as StructuralBase
import Eclips.Herald.UseCase.StructuralProgress qualified as StructuralProgress
import Eclips.Herald.UseCase.StructuralSettlement qualified as StructuralSettlement

data StructuralCoordinationProblem
  = StructuralCoordinationWorkProblem StructuralProgress.LocalStructuralWorkProblem
  | StructuralCoordinationProtocolProblem HeraldInvariantFault
  | StructuralCoordinationPeerInputProblem PeerInput.PeerInputProblem
  | StructuralCoordinationSettlementProblem StructuralSettlement.StructuralSettlementProblem
  | StructuralCoordinationAlignmentProblem Alignment.AlignmentCoordinationProblem
  | StructuralCoordinationAlignmentTransferProblem AlignmentTransfer.AlignmentTransferCoordinatorProblem
  | StructuralCoordinationCutPlacementProblem CutQualifiedPlacementProblem
  deriving stock (Eq, Show)

data CutQualifiedPlacementProblem
  = CutPlacementTopologyProblem GraphProgress.StructuralProgressProblem
  | CutPlacementSnapshotProblem Placement.PlacementProblem
  | CutPlacementResourceMissing GlobalObjectId
  | CutPlacementResourceAmbiguous GlobalObjectId
  | CutPlacementDigestEntryMissing GlobalObjectId
  | CutPlacementControlCauseNotOrdered GlobalObjectId
  deriving stock (Eq, Show)

-- | Whether dependency release changed an ordered alignment route marker.
-- The transfer coordinator uses this exact disposition to decide whether its
-- already-quiescent work needs another pass, without comparing whole Herald
-- states.
data PendingAlignmentRouteCutoverDisposition
  = PendingAlignmentRouteCutoversHeld
  | PendingAlignmentRouteCutoversReleased
  deriving stock (Eq, Ord, Show)

-- | Drain ready local stages, level-trigger the deterministic cut proposer,
-- and settle any singleton cut installed by that proposal before returning.
advanceLocalStructuralWork ::
  HeraldState ->
  Either StructuralCoordinationProblem (HeraldState, EffectBatch)
advanceLocalStructuralWork initial = do
  (afterStages, stageEffects) <-
    case retainedInstalledSuccessorBase initial of
      Nothing ->
        mapLeft
          StructuralCoordinationWorkProblem
          (StructuralProgress.advanceReadyApplicationStructuralStages initial)
      Just successorBase -> do
        (successor, _, effects) <-
          advanceSuccessorStructuralBaseWork successorBase initial
        Right (successor, effects)
  (afterProposal, proposalEffects, installedCuts) <-
    mapLeft
      StructuralCoordinationProtocolProblem
      (PeerControl.advanceLocalTopologyCutIfReady afterStages)
  (afterReadiness, readinessEffects, readinessCuts) <-
    mapLeft
      StructuralCoordinationProtocolProblem
      ( PeerControl.redriveTerminalSourceReadinessIfProgressed
          initial
          afterProposal
      )
  (afterInstalled, installedEffects, allInstalledCuts) <-
    releaseSettleAndRedrive
      (installedCuts <> readinessCuts)
      afterReadiness
  if null allInstalledCuts
    then
      Right
        ( afterInstalled,
          stageEffects
            <> proposalEffects
            <> readinessEffects
            <> installedEffects
        )
    else do
      (successor, furtherEffects) <- advanceLocalStructuralWork afterInstalled
      Right
        ( successor,
          stageEffects
            <> proposalEffects
            <> readinessEffects
            <> installedEffects
            <> furtherEffects
        )

-- | Level-trigger every kind of work fenced by one exact installed
-- membership-successor base. The same seam serves the live protocol, where
-- the coordinator is retained in 'HeraldState', and the detached closure,
-- where the opaque base witness is supplied externally.
--
-- A released publication can expose an ordered route marker behind it, while
-- applying a base-aware local stage can expose further work in the same owner
-- slice. Iterate the three owners together until their durable state is
-- unchanged; none of them emits an effect without committing owner progress.
advanceSuccessorStructuralBaseWork ::
  SuccessorStructuralBase ->
  HeraldState ->
  Either
    StructuralCoordinationProblem
    (HeraldState, [PublicationId], EffectBatch)
advanceSuccessorStructuralBaseWork successorBase = fixedPoint [] mempty
  where
    fixedPoint releasedPublications effects predecessor = do
      (owners, releaseResult) <-
        mapLeft
          StructuralCoordinationPeerInputProblem
          ( PeerInput.releasePeerSuccessorStructuralBase
              successorBase
              (peerInputContextFor predecessor)
              (peerInputStateFor predecessor)
          )
      let afterPeerRelease = installReleasedPeerInputState owners releaseResult predecessor
          peerEffects =
            peerDependencyReleaseEffects afterPeerRelease releaseResult
      (afterStages, stageEffects) <-
        mapLeft
          StructuralCoordinationWorkProblem
          ( StructuralProgress.advanceReadyApplicationStructuralStagesAfterSuccessorBase
              successorBase
              afterPeerRelease
          )
      (successor, routeCutoverEffects) <-
        releasePendingAlignmentRouteCutovers afterStages
      let nextReleasedPublications =
            releasedPublications
              <> PeerInput.peerDependencyReleasedPublications releaseResult
          nextEffects =
            effects
              <> peerEffects
              <> stageEffects
              <> routeCutoverEffects
      if successor == predecessor
        then Right (successor, nextReleasedPublications, nextEffects)
        else fixedPoint nextReleasedPublications nextEffects successor

retainedInstalledSuccessorBase :: HeraldState -> Maybe SuccessorStructuralBase
retainedInstalledSuccessorBase state = do
  coordinator <- startupStructuralBaseCoordinator state
  successorBase <- StructuralBase.structuralBaseEvidence coordinator
  if successorStructuralBaseGenerationId successorBase
    == OracleProjection.oracleViewCurrentHeraldMembershipId
      (OracleProjection.oracleView (startupOracleProjectionState state))
    && GraphProgress.structuralSuccessorBaseInstalled
      successorBase
      (startupStructuralProgressState state)
    then Just successorBase
    else Nothing

-- | Settle cuts installed by an already admitted peer-control transition.
settleInstalledStructuralCuts ::
  [TopologyCutId] ->
  HeraldState ->
  Either StructuralCoordinationProblem (HeraldState, EffectBatch)
settleInstalledStructuralCuts cuts state = do
  (afterInstalled, installedEffects, allInstalledCuts) <-
    releaseSettleAndRedrive cuts state
  if null allInstalledCuts
    then Right (afterInstalled, installedEffects)
    else do
      (successor, structuralEffects) <- advanceLocalStructuralWork afterInstalled
      Right (successor, installedEffects <> structuralEffects)

-- | Settlement can itself materialize the topology view needed by a held
-- terminal announce or established base.  Re-drive once after that exact
-- owner progress and settle any resulting successor cut before returning.
releaseSettleAndRedrive ::
  [TopologyCutId] ->
  HeraldState ->
  Either
    StructuralCoordinationProblem
    (HeraldState, EffectBatch, [TopologyCutId])
releaseSettleAndRedrive cuts initial
  | null cuts = Right (initial, mempty, [])
  | otherwise = do
      (afterInstalled, installedEffects) <-
        releaseAndSettleInstalledCuts cuts initial
      (afterReadiness, readinessEffects, readinessCuts) <-
        mapLeft
          StructuralCoordinationProtocolProblem
          ( PeerControl.redriveTerminalSourceReadinessIfProgressed
              initial
              afterInstalled
          )
      (successor, furtherEffects, furtherCuts) <-
        releaseSettleAndRedrive readinessCuts afterReadiness
      Right
        ( successor,
          installedEffects <> readinessEffects <> furtherEffects,
          cuts <> furtherCuts
        )

releaseAndSettleInstalledCuts ::
  [TopologyCutId] ->
  HeraldState ->
  Either StructuralCoordinationProblem (HeraldState, EffectBatch)
releaseAndSettleInstalledCuts cuts initial = do
  (afterRelease, releaseEffects) <- foldM releaseOne (initial, mempty) cuts
  (afterSettlement, settlementEffects) <-
    mapLeft
      StructuralCoordinationSettlementProblem
      -- This is the single finalizer for structural operations covered by cuts
      -- installed both locally and by a peer callback.
      (StructuralSettlement.settleInstalledTopologyCuts cuts afterRelease)
  (afterPlacement, placementEffects) <-
    mapLeft
      StructuralCoordinationCutPlacementProblem
      (qualifyInstalledPlacementCuts cuts afterSettlement)
  (afterLoss, lossEffects) <-
    mapLeft
      StructuralCoordinationAlignmentTransferProblem
      (AlignmentTransfer.reconcileAlignmentLosses afterPlacement)
  (afterAlignment, alignmentEffects) <-
    mapLeft
      StructuralCoordinationAlignmentProblem
      (Alignment.advanceAlignmentGenerations afterLoss)
  (successor, routeCutoverEffects) <-
    releasePendingAlignmentRouteCutovers afterAlignment
  Right
    ( successor,
      releaseEffects
        <> placementEffects
        <> settlementEffects
        <> lossEffects
        <> alignmentEffects
        <> routeCutoverEffects
    )
  where
    releaseOne (state, effects) cut = do
      (owners, result) <-
        mapLeft
          StructuralCoordinationPeerInputProblem
          ( PeerInput.releasePeerTopology
              (peerInputContextFor state)
              cut
              (peerInputStateFor state)
          )
      let successor = installReleasedPeerInputState owners result state
      Right
        ( successor,
          effects <> peerDependencyReleaseEffects successor result
        )

qualifyInstalledPlacementCuts ::
  [TopologyCutId] ->
  HeraldState ->
  Either CutQualifiedPlacementProblem (HeraldState, EffectBatch)
qualifyInstalledPlacementCuts cuts initial =
  foldM qualifyOne (initial, mempty) cuts
  where
    qualifyOne (state, effects) cut = do
      routes <- exactLocalRoutesAtInstalledCut cut state
      prepared <-
        mapLeft
          CutPlacementSnapshotProblem
          (Placement.prepareLocalSnapshotForRoutesWithDiagnostics (startupDiagnosticChecks state) routes (startupPlacementState state))
      let (qualifiedPlacement, result) = Placement.commitLocalSnapshot prepared
          successor = replaceStartupPlacementState qualifiedPlacement state
          advertisementEffects =
            case Placement.localSnapshotDisposition result of
              Placement.AdvancedLocalSnapshot ->
                orderedEffectBatch
                  [ SendPeerControl
                      binding
                      ( PeerPlacementUpdate
                          (FullPlacementSnapshot (Placement.localSnapshotMessage result))
                      )
                  | binding <- Discovery.currentPeerBindings (startupDiscoveryState state)
                  ]
              Placement.InitialLocalSnapshot -> mempty
              Placement.ReofferedLocalSnapshot -> mempty
      Right (successor, effects <> advertisementEffects)

exactLocalRoutesAtInstalledCut ::
  TopologyCutId ->
  HeraldState ->
  Either CutQualifiedPlacementProblem [DeltaRoute]
exactLocalRoutesAtInstalledCut cut state = do
  projection <-
    mapLeft
      CutPlacementTopologyProblem
      ( GraphProgress.structuralProjectionAtInstalledCut
          (structuralReconciliationViews state)
          cut
          progress
      )
  applicationRoutes <-
    traverse
      (applicationRouteAtCut projection)
      [ (object, provenance, delta, typedSort, process)
      | ( object,
          Reconciliation.DeltaVertexProjection
            provenance
            delta
            typedSort
            (Reconciliation.LiveProcessController process residence)
            (Reconciliation.ActiveRootHere activeProcess)
          ) <-
          Map.toAscList
            (Reconciliation.structuralProjectionSnapshotVertices projection),
        residence == local,
        activeProcess == process
      ]
  Right (privateSystemViewRoutes <> applicationRoutes)
  where
    progress = startupStructuralProgressState state
    local = checkedLocalHeraldEpoch (startupGenesis state)

    privateSystemViewRoutes =
      [ privateSystemViewDeltaRoute
          (Placement.systemViewPlacementRole placement)
          (Placement.systemViewPlacementDelta placement)
          (Placement.systemViewPlacementSortId placement)
          (Placement.systemViewPlacementOccurrenceId placement)
          (Placement.systemViewPlacementStoreIncarnation placement)
      | placement <- Placement.systemViewPlacements (startupPlacementState state)
      ]

    applicationRouteAtCut projection (object, provenance, delta, typedSort, process) = do
      digestEntry <-
        maybe
          (Left (CutPlacementDigestEntryMissing object))
          Right
          ( Map.lookup
              object
              (Reconciliation.structuralProjectionSnapshotDigestEntries projection)
          )
      let cause = Reconciliation.structuralProjectionDigestEntryControlCause digestEntry
          matching =
            filter
              (storeSlotMatchesCut object provenance cause delta typedSort process)
              (Store.retainedStoreSlots (startupStoreState state))
      slot <- case matching of
        [] -> Left (CutPlacementResourceMissing object)
        [retained] -> Right retained
        _ -> Left (CutPlacementResourceAmbiguous object)
      prerequisite <- case cause of
        Nothing ->
          Right
            (Reconciliation.structuralProjectionDigestEntryControlPrerequisite digestEntry)
        Just retainedCause ->
          maybe
            (Left (CutPlacementControlCauseNotOrdered object))
            ( Right
                . max
                  ( Reconciliation.structuralProjectionDigestEntryControlPrerequisite
                      digestEntry
                  )
            )
            (structuralConsequenceCauseControlIndex retainedCause)
      Right
        ( applicationDeltaRoute
            delta
            (sortOccurrenceSortId typedSort)
            (sortOccurrenceDefinition typedSort)
            object
            process
            (Store.storeSlotIncarnation slot)
            prerequisite
        )

storeSlotMatchesCut ::
  GlobalObjectId ->
  Reconciliation.StructuralProjectionProvenance ->
  Maybe StructuralConsequenceCause ->
  DeltaId ->
  SortOccurrence ->
  ProcessEpochId ->
  Store.StoreSlot ->
  Bool
storeSlotMatchesCut object provenance cause delta typedSort process slot =
  common && provenanceMatches
  where
    common =
      Store.storeSlotDelta slot == delta
        && Store.storeSlotSortId slot == sortOccurrenceSortId typedSort
        && Store.storeSlotOccurrenceId slot == sortOccurrenceDefinition typedSort
    provenanceMatches = case (cause, provenance, Store.storeSlotProvenance slot) of
      ( Just expectedCause,
        _,
        Store.StructuralControlReader observedCause observedProcess controller _
        ) ->
          observedCause == expectedCause
            && observedProcess == process
            && controller == object
      ( Nothing,
        Reconciliation.GenesisStructuralProvenance expectedObject,
        Store.ApplicationReader observedProcess _
        ) ->
          expectedObject == object && observedProcess == process
      ( Nothing,
        Reconciliation.BaselineStructuralProvenance expectedPublication,
        Store.StructuralBaselineReader observedPublication observedProcess controller _
        ) ->
          observedPublication == expectedPublication
            && observedProcess == process
            && controller == object
      ( Nothing,
        Reconciliation.OccurrenceStructuralProvenance expectedOccurrence _,
        Store.StructuralApplicationReader observedOccurrence observedProcess controller _
        ) ->
          observedOccurrence == expectedOccurrence
            && observedProcess == process
            && controller == object
      _ -> False

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

-- | Complete only those ordered route-cutover items whose generation
-- dependency has become retained. This is called after every alignment
-- fixed-point, including the no-new-topology level trigger.
releasePendingAlignmentRouteCutovers ::
  HeraldState ->
  Either StructuralCoordinationProblem (HeraldState, EffectBatch)
releasePendingAlignmentRouteCutovers state = do
  (successor, effects, _) <-
    releasePendingAlignmentRouteCutoversWithDisposition state
  Right (successor, effects)

releasePendingAlignmentRouteCutoversWithDisposition ::
  HeraldState ->
  Either
    StructuralCoordinationProblem
    (HeraldState, EffectBatch, PendingAlignmentRouteCutoverDisposition)
releasePendingAlignmentRouteCutoversWithDisposition state = do
  (owners, result) <-
    mapLeft
      StructuralCoordinationPeerInputProblem
      ( PeerInput.releasePendingAlignmentRouteCutovers
          (peerInputContextFor state)
          (peerInputStateFor state)
      )
  -- Route-marker release owns only Alignment and PeerStream. Installing the
  -- untouched structural owners would invalidate reusable cut queries after
  -- every alignment pass, including a pass with no pending marker.
  let successor =
        replaceStartupAlignmentState (PeerInput.peerInputAlignmentState owners)
          . replaceStartupPeerStreamState (PeerInput.peerInputPeerStreamState owners)
          $ state
      disposition =
        case PeerInput.peerDependencyReleaseDisposition result of
          PeerInput.PeerDependencyStillHeld ->
            PendingAlignmentRouteCutoversHeld
          PeerInput.PeerDependencyReleased ->
            PendingAlignmentRouteCutoversReleased
  Right
    ( successor,
      peerDependencyReleaseEffects successor result,
      disposition
    )

-- | Reach the fixed point shared by alignment transfer and ordered route-cutover
-- release. Each subsequent pass requires a released route-cutover obligation.
advanceAlignmentAndPending ::
  HeraldState ->
  HeraldState ->
  Either StructuralCoordinationProblem (HeraldState, EffectBatch)
advanceAlignmentAndPending _streamBaseline initial = go initial mempty
  where
    go predecessor effects = do
      (afterAlignment, alignmentEffects) <-
        mapLeft
          StructuralCoordinationAlignmentTransferProblem
          (AlignmentTransfer.advanceAlignmentTransfers predecessor)
      (successor, pendingEffects, pendingDisposition) <-
        releasePendingAlignmentRouteCutoversWithDisposition afterAlignment
      let accumulated = effects <> alignmentEffects <> pendingEffects
      if pendingDisposition == PendingAlignmentRouteCutoversReleased
        then go successor accumulated
        else Right (successor, accumulated)

peerInputContextFor :: HeraldState -> PeerInput.PeerInputContext
peerInputContextFor state =
  PeerInput.peerInputContext
    (startupGenesis state)
    (startupOracleProjectionState state)
    (startupDiscoveryState state)
    (startupPlacementState state)

peerInputStateFor :: HeraldState -> PeerInput.PeerInputState
peerInputStateFor state =
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

-- | Dependency release records every resolved publication, even a partial or
-- terminally suppressed one. Without any such record, it can change receipt,
-- wait and marker bookkeeping only; historical cut-query inputs remain exact.
installReleasedPeerInputState :: PeerInput.PeerInputState -> PeerInput.PeerDependencyReleaseResult -> HeraldState -> HeraldState
installReleasedPeerInputState owners result predecessor
  | null (PeerInput.peerDependencyReleasedPublications result) =
      let !cache = startupAlignmentCutQueryCache predecessor
       in replaceStartupAlignmentCutQueryCache cache replaced
  | otherwise = replaced
  where
    replaced =
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
        $ predecessor

peerDependencyReleaseEffects ::
  HeraldState ->
  PeerInput.PeerDependencyReleaseResult ->
  EffectBatch
peerDependencyReleaseEffects state result =
  orderedEffectBatch
    ( rejectionEffects
        <> [ SendPeerControl
               binding
               (PeerPlacementUpdate (FullPlacementSnapshot snapshot))
           | snapshot <- PeerInput.peerDependencyPlacementSnapshots result,
             binding <- Discovery.currentPeerBindings (startupDiscoveryState state),
             binding `Set.notMember` rejectedBindings
           ]
        <> concatMap
          acknowledgementEffects
          (PeerInput.peerDependencyAcknowledgements result)
        <> fmap wakeEffect (PeerInput.peerDependencyWakes result)
    )
  where
    rejectedBindings =
      Set.fromList
        [ binding
        | rejection <- PeerInput.peerDependencyDeferredRejections result,
          Just binding <-
            [ Discovery.currentPeerBinding
                (streamDirectionSource (PeerInput.deferredPeerRejectionDirection rejection))
                (startupDiscoveryState state)
            ]
        ]
    rejectionEffects =
      [ RejectPeerConnection binding ClosePeerPublicationProtocol
      | binding <- Set.toAscList rejectedBindings
      ]
    acknowledgementEffects acknowledgement =
      case Discovery.currentPeerBinding peer (startupDiscoveryState state) of
        Just binding
          | binding `Set.notMember` rejectedBindings ->
              [SendPeerControl binding (PeerStreamCompleted direction (PeerInput.peerAcknowledgementCompletion acknowledgement))]
                <> maybe
                  []
                  (\offer -> [SendPeerControl binding (PeerStreamResumeOffered offer)])
                  (PeerInput.peerAcknowledgementRepairOffer acknowledgement)
        _ -> []
      where
        direction = PeerInput.peerAcknowledgementDirection acknowledgement
        peer = streamDirectionSource direction

    wakeEffect wake =
      SendApplicationWaitWake
        (Application.applicationWaitWakeBinding wake)
        (Application.applicationWaitWakeCursor wake)
        (Application.applicationWaitWakeRequestId wake)
        (Application.applicationWaitWakeWaitId wake)
        (Application.applicationWaitWakeResult wake)

mapLeft :: (left -> mapped) -> Either left right -> Either mapped right
mapLeft convert = either (Left . convert) Right
