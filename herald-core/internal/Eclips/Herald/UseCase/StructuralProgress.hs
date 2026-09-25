{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

-- | Atomic source-side structural sequencing and local application.
--
-- Accepted structural application publications own no dot or peer item until
-- this coordinator can satisfy their exact causal/data/control prerequisites.
-- Among the currently-ready stages it chooses the least Herald publication
-- position, commits every semantic leaf together, and then restarts the scan:
-- a later-position endpoint may therefore release an earlier held edge without
-- letting the held edge reserve a sequence number.
module Eclips.Herald.UseCase.StructuralProgress
  ( LocalStructuralWorkProblem (..),
    advanceNextReadyApplicationStructuralStage,
    advanceReadyApplicationStructuralStages,
    advanceReadyApplicationStructuralStagesAfterSuccessorBase,
    normalizeStructuralReportEffects,
    structuralReportBindings,
    installedLineageAdmitsApplicationStage,
  )
where

import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( DeltaId,
    HeraldEpoch,
    StructuralOccurrenceId,
    TopologyCutId,
    globalUniqueIdBytes,
    mkStoreIncarnationId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGenerationId,
    heraldMembershipGenerationActiveHeraldEpochs,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationCanonicalValue,
    checkedPublicationId,
    checkedPublicationSort,
  )
import Eclips.Domain.Route
  ( FrozenRoute,
    RouteError,
    attenuateFrozenRoute,
    destinationDelta,
    destinationHerald,
    destinationStoreIncarnation,
    destinationStrength,
    partitionFrozenRoute,
    routeDestinations,
    routePartitionLocal,
  )
import Eclips.Domain.Structural
  ( StructuralVersionVector,
    structuralVersionVectorMembershipGenerationId,
  )
import Eclips.Domain.StructuralConsequence (structuralOccurrenceCause)
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Discovery
  ( PeerBinding,
    PeerHelloDisposition (PeerHelloAccepted),
    peerBindingRemoteHeraldEpoch,
  )
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect
      ( SchedulePeerDispatch,
        SendPeerControl,
        SetPeerCandidateDisposition
      ),
    effectBatchMembers,
    orderedEffectBatch,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedLocalHeraldEpoch,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.Protocol
  ( StructuralAppliedReport,
    structuralAppliedReportMembershipGenerationId,
  )
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource
  ( SuccessorStructuralBase,
    TerminalSourceProblem,
    allocateSuccessorStructuralOccurrence,
    successorStructuralAdvanceOccurrence,
    successorStructuralAdvancePredecessor,
    successorStructuralBaseCutId,
    successorStructuralBaseGenerationId,
  )
import Eclips.Herald.IdGenerator.State qualified as IdGenerator
import Eclips.Herald.Input
  ( PeerControl
      ( PeerPlacementUpdate,
        PeerStreamFrontierAdvanced,
        PeerStreamResumeAccepted,
        PeerStructuralAppliedReported
      ),
  )
import Eclips.Herald.Join.State qualified as Join
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerPayload (peerLogicalAssignmentReceipt, peerLogicalPublicationItem)
import Eclips.Herald.PeerPublication
  ( PeerPublicationProblem,
    PublicationBatch,
    PublicationBatchProblem,
    StructuralOccurrenceStampProblem,
    mkPublicationBatch,
    mkStructuralOccurrenceStamp,
    mkStructuralPeerPublication,
    publicationDestination,
    structuralPublicationDigestForSemantics,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement (PlacementUpdate (FullPlacementSnapshot))
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.Route qualified as PublicationRoute
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupAlignmentState,
    replaceStartupGraphState,
    replaceStartupIdGeneratorState,
    replaceStartupPeerStreamState,
    replaceStartupPlacementState,
    replaceStartupPublicationState,
    replaceStartupStoreState,
    replaceStartupStructuralProgressState,
    startupAlignmentState,
    startupControlledState,
    startupDiagnosticChecks,
    startupDiscoveryState,
    startupGenesis,
    startupGraphState,
    startupIdGeneratorState,
    startupJoinState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt
  ( sortOccurrence,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Oracle.Admission qualified as Admission

-- | Only the deterministic cut announcer needs the complete report matrix.
-- An observer is not a reporter until its structural membership base includes
-- it. The announcer already retains its own cumulative report in Graph.
structuralReportBindings ::
  GraphProgress.StructuralProgressState -> [PeerBinding] -> [PeerBinding]
structuralReportBindings progress bindings
  | local == announcer || GraphProgress.structuralProgressIsJoiningObserver progress = []
  | otherwise = filter ((== announcer) . peerBindingRemoteHeraldEpoch) bindings
  where
    local = GraphProgress.structuralProgressLocalHerald progress
    members = GraphProgress.structuralProgressMembers progress
    announcer = NonEmpty.head members

-- | Collapse the structural-report offers produced during one composed
-- transition without moving their protocol-ordering slot. Only reports in the
-- final owner's membership generation are normalized: a predecessor-
-- generation report may legitimately precede terminal Established, while the
-- first successor-generation slot must carry the one authoritative final
-- snapshot and every later same-generation slot is redundant.
normalizeStructuralReportEffects ::
  Map PeerBinding StructuralAppliedReport ->
  [PeerBinding] ->
  StructuralAppliedReport ->
  EffectBatch ->
  EffectBatch
normalizeStructuralReportEffects initialReports currentBindings finalReport effects =
  orderedEffectBatch (retained <> finalReportOffers)
  where
    members = effectBatchMembers effects
    finalGeneration = structuralAppliedReportMembershipGenerationId finalReport
    acceptedBindings =
      [ binding
      | SetPeerCandidateDisposition _ (PeerHelloAccepted _ binding) <- members
      ]
    reconnectReportBindings =
      [ binding
      | SendPeerControl binding (PeerStructuralAppliedReported _) <- members
      ]
    (offeredReports, retained) = go initialReports [] members
    finalReportOffers =
      [ SendPeerControl binding (PeerStructuralAppliedReported finalReport)
      | binding <- currentBindings,
        binding `notElem` acceptedBindings,
        Map.lookup binding offeredReports /= Just finalReport
      ]

    go offered reverseRetained remaining = case remaining of
      [] -> (offered, reverse reverseRetained)
      effect : rest -> case effect of
        SetPeerCandidateDisposition _ (PeerHelloAccepted _ binding) ->
          go (Map.delete binding offered) (effect : reverseRetained) rest
        SendPeerControl binding (PeerStreamResumeAccepted _) ->
          go
            ( if binding `elem` reconnectReportBindings
                then Map.delete binding offered
                else offered
            )
            (effect : reverseRetained)
            rest
        SendPeerControl binding (PeerStructuralAppliedReported report)
          | binding `elem` acceptedBindings ->
              go offered reverseRetained rest
          | structuralAppliedReportMembershipGenerationId report == finalGeneration ->
              if Map.lookup binding offered == Just finalReport
                then go offered reverseRetained rest
                else
                  go
                    (Map.insert binding finalReport offered)
                    ( SendPeerControl binding (PeerStructuralAppliedReported finalReport)
                        : reverseRetained
                    )
                    rest
          | Map.lookup binding offered == Just report ->
              go offered reverseRetained rest
          | otherwise ->
              go
                (Map.insert binding report offered)
                (effect : reverseRetained)
                rest
        _ -> go offered (effect : reverseRetained) rest

data LocalStructuralWorkProblem
  = LocalStructuralSortMissing
  | LocalStructuralSortOccurrenceMismatch
  | LocalStructuralIdentityProblem
  | LocalStructuralReconciliationProblem Reconciliation.StructuralReconciliationProblem
  | LocalStructuralPublicationBatchProblem PublicationBatchProblem
  | LocalStructuralPeerPublicationProblem PeerPublicationProblem
  | LocalStructuralStampProblem StructuralOccurrenceStampProblem
  | LocalStructuralPublicationOwnerProblem Publication.PublicationPreparationError
  | LocalStructuralDeliveryRouteProblem RouteError
  | LocalStructuralStorePatchProblem Store.StructuralStorePatchError
  | LocalStructuralStoreProblem Store.StoreTransitionError
  | LocalStructuralPlacementProblem Placement.StructuralPlacementPatchError
  | LocalStructuralProgressProblem GraphProgress.StructuralProgressProblem
  | LocalStructuralAlignmentProblem Alignment.AlignmentDebtProblem
  | LocalStructuralPeerStreamProblem PeerStream.PeerStreamProblem
  | LocalStructuralSuccessorBaseMembershipMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | LocalStructuralSuccessorProgressMembershipMismatch
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | LocalStructuralSuccessorBaseCutMismatch TopologyCutId TopologyCutId
  | LocalStructuralSuccessorAllocationProblem TerminalSourceProblem
  | LocalStructuralSuccessorOccurrenceMismatch
  deriving stock (Eq, Show)

data StageDisposition
  = StageHeld
  | StageApplied HeraldState [HeraldEffect]

-- | Drain the finite currently-ready source stages to a fixed point. Held
-- stages retain no dot, generated incarnation, leaf patch, outbox, or report.
advanceReadyApplicationStructuralStages ::
  HeraldState ->
  Either LocalStructuralWorkProblem (HeraldState, EffectBatch)
advanceReadyApplicationStructuralStages =
  advanceReadyApplicationStructuralStagesWithBase Nothing

-- | Drain successor-generation source stages only after the exact installed
-- structural-base witness is available. Supplying the witness here, rather
-- than storing a Boolean in the publication owner, keeps live ingress fenced
-- until the terminal-source protocol installs its exact successor base.
advanceReadyApplicationStructuralStagesAfterSuccessorBase ::
  SuccessorStructuralBase ->
  HeraldState ->
  Either LocalStructuralWorkProblem (HeraldState, EffectBatch)
advanceReadyApplicationStructuralStagesAfterSuccessorBase successorBase initial = do
  validateSuccessorBase successorBase initial
  advanceReadyApplicationStructuralStagesWithBase (Just successorBase) initial

advanceReadyApplicationStructuralStagesWithBase ::
  Maybe SuccessorStructuralBase ->
  HeraldState ->
  Either LocalStructuralWorkProblem (HeraldState, EffectBatch)
advanceReadyApplicationStructuralStagesWithBase successorBase initial = do
  (successor, reverseEffects) <- go initial []
  Right (successor, orderedEffectBatch (reverse reverseEffects))
  where
    go state reverseEffects = do
      next <- advanceNextReadyApplicationStructuralStageWithBase successorBase state
      case next of
        Nothing -> Right (state, reverseEffects)
        Just (successor, effects) ->
          go successor (reverse (effectBatchMembers effects) <> reverseEffects)

-- | Apply at most the least-positioned currently-ready source stage. Runtime
-- coordination normally drains to a fixed point through
-- 'advanceReadyApplicationStructuralStages'; the single-step seam makes the
-- deterministic owner transition independently composable and testable.
advanceNextReadyApplicationStructuralStage ::
  HeraldState ->
  Either LocalStructuralWorkProblem (Maybe (HeraldState, EffectBatch))
advanceNextReadyApplicationStructuralStage =
  advanceNextReadyApplicationStructuralStageWithBase Nothing

advanceNextReadyApplicationStructuralStageWithBase ::
  Maybe SuccessorStructuralBase ->
  HeraldState ->
  Either LocalStructuralWorkProblem (Maybe (HeraldState, EffectBatch))
advanceNextReadyApplicationStructuralStageWithBase successorBase state
  | Just admission <- OracleProjection.oracleViewPendingHeraldAdmission (OracleProjection.oracleView (startupOracleProjectionState state)),
    Just _ <- Join.lookupCapture (Admission.admissionRecordId admission) (Admission.admissionRecordAttempt admission) (startupJoinState state) =
      Right Nothing
  | otherwise =
      scan
        (Publication.unstampedStructuralSourceStageEntries (startupPublicationState state))
  where
    scan [] = Right Nothing
    scan (stage : remaining) = do
      disposition <- applyStage successorBase stage state
      case disposition of
        StageHeld -> scan remaining
        StageApplied successor effects ->
          Right (Just (successor, orderedEffectBatch effects))

data PreparedStage
  = PreparedHeld
  | PreparedReady ReadyStage

data ReadyStage = ReadyStage
  { preparedReconciliation :: Reconciliation.PreparedStructuralReconciliation,
    preparedIdGenerator :: Maybe IdGenerator.PreparedGeneratedId
  }

prepareStage ::
  Publication.StructuralSourceStage ->
  HeraldState ->
  Either LocalStructuralWorkProblem PreparedStage
prepareStage stage state = do
  _ <- exactCarrierEntry stage state
  let progress = startupStructuralProgressState state
      controlReady =
        GraphProgress.structuralAppliedControlPrefix progress
          >= Publication.structuralSourceControlPrerequisite stage
      topologyReady =
        sourceTopologyInstalled
          (structuralSourceTopology stage progress)
          progress
      membershipReady = structuralSourceMembershipReady stage state
  if not controlReady || not topologyReady || not membershipReady
    then Right PreparedHeld
    else do
      let candidate =
            Publication.proposeStructuralOccurrence
              (GraphProgress.structuralAppliedVector progress)
              (startupPublicationState state)
          application =
            Reconciliation.structuralApplication
              (Publication.structuralCandidateOccurrence candidate)
              (Publication.structuralCandidatePredecessor candidate)
              ( sortOccurrence
                  (checkedPublicationSort (Publication.structuralSourceChecked stage))
                  (Publication.structuralSourceSortOccurrenceId stage)
              )
              (Publication.structuralSourceChecked stage)
              (Publication.structuralSourceControlPrerequisite stage)
          views =
            reconciliationViews
              (startupControlledState state)
              state
      first <-
        mapLeft
          LocalStructuralReconciliationProblem
          ( Reconciliation.prepareStructuralReconciliationAtAdmittedPredecessor
              views
              application
              Nothing
              (GraphProgress.structuralProgressReconciliation progress)
          )
      case first of
        Reconciliation.StructuralReady prepared ->
          Right
            ( PreparedReady
                ( ReadyStage
                    prepared
                    Nothing
                )
            )
        Reconciliation.StructuralHeld dependencies ->
          case Set.toAscList dependencies of
            [Reconciliation.FreshStoreIncarnationDependency delta] -> do
              (evidence, generated) <-
                generatedIncarnation delta state
              second <-
                mapLeft
                  LocalStructuralReconciliationProblem
                  ( Reconciliation.prepareStructuralReconciliationAtAdmittedPredecessor
                      views
                      application
                      (Just evidence)
                      (GraphProgress.structuralProgressReconciliation progress)
                  )
              case second of
                Reconciliation.StructuralReady prepared ->
                  Right
                    ( PreparedReady
                        ( ReadyStage
                            prepared
                            generated
                        )
                    )
                Reconciliation.StructuralHeld _ -> Right PreparedHeld
            _ -> Right PreparedHeld

-- | Sequencing runs only in the installed current generation. Admission is
-- immutable: an old application or environment stage uses the retained checked
-- installed bases for every retirement between its origin and this frontier.
structuralSourceMembershipReady ::
  Publication.StructuralSourceStage -> HeraldState -> Bool
structuralSourceMembershipReady stage state =
  GraphProgress.structuralProgressMembershipGenerationId progress == currentId
    && maybe
      True
      (installedLineageAdmitsAdmission progress currentId)
      (Publication.structuralSourceMembershipGenerationId stage)
  where
    progress = startupStructuralProgressState state
    currentId = OracleProjection.oracleViewCurrentHeraldMembershipId (OracleProjection.oracleView (startupOracleProjectionState state))

installedLineageAdmitsApplicationStage ::
  GraphProgress.StructuralProgressState ->
  HeraldMembershipGenerationId ->
  Publication.ApplicationPublicationRecord ->
  Bool
installedLineageAdmitsApplicationStage progress currentId application =
  installedLineageAdmitsAdmission progress currentId (Publication.applicationPublicationMembershipGenerationId application)

installedLineageAdmitsAdmission :: GraphProgress.StructuralProgressState -> HeraldMembershipGenerationId -> HeraldMembershipGenerationId -> Bool
installedLineageAdmitsAdmission progress currentId admission =
  GraphProgress.structuralProgressMembershipGenerationId progress == currentId
    && case GraphProgress.structuralDescendantSettlement admission currentId progress of
      Just _ -> True
      Nothing -> False

generatedIncarnation ::
  DeltaId ->
  HeraldState ->
  Either
    LocalStructuralWorkProblem
    (Reconciliation.FreshStoreIncarnationEvidence, Maybe IdGenerator.PreparedGeneratedId)
generatedIncarnation delta state = do
  generated <-
    mapLeft
      (const LocalStructuralIdentityProblem)
      (IdGenerator.prepareGeneratedId (startupIdGeneratorState state))
  incarnation <-
    mapLeft
      (const LocalStructuralIdentityProblem)
      (mkStoreIncarnationId (globalUniqueIdBytes (IdGenerator.preparedGlobalUniqueId generated)))
  Right
    ( Reconciliation.freshStoreIncarnationEvidence
        Reconciliation.GeneratedStoreIncarnation
        delta
        incarnation,
      Just generated
    )

applyStage ::
  Maybe SuccessorStructuralBase ->
  Publication.StructuralSourceStage ->
  HeraldState ->
  Either LocalStructuralWorkProblem StageDisposition
applyStage successorBase stage state = do
  readiness <- prepareStage stage state
  case readiness of
    PreparedHeld -> Right StageHeld
    PreparedReady ready -> do
      (successor, effects) <- commitReady successorBase stage ready state
      Right (StageApplied successor effects)

commitReady ::
  Maybe SuccessorStructuralBase ->
  Publication.StructuralSourceStage ->
  ReadyStage ->
  HeraldState ->
  Either LocalStructuralWorkProblem (HeraldState, [HeraldEffect])
commitReady successorBase stage ready state = do
  entry <- exactCarrierEntry stage state
  let descriptor = SortRegistry.registryEntryDescriptor entry
      occurrenceId = Publication.structuralSourceSortOccurrenceId stage
      checked = Publication.structuralSourceChecked stage
      sourceProcess = Publication.structuralSourceProcess stage
      sourceTopology = structuralSourceTopology stage progress
      control = Publication.structuralSourceControlPrerequisite stage
      progress = startupStructuralProgressState state
      candidate =
        Publication.proposeStructuralOccurrence
          (GraphProgress.structuralAppliedVector progress)
          (startupPublicationState state)
      occurrence = Publication.structuralCandidateOccurrence candidate
      predecessor = Publication.structuralCandidatePredecessor candidate
  validateSuccessorAllocation successorBase occurrence predecessor state
  digest <-
    mapLeft
      LocalStructuralPeerPublicationProblem
      ( structuralPublicationDigestForSemantics
          descriptor
          occurrenceId
          checked
          sourceProcess
          (Publication.structuralSourceStrength stage)
          sourceTopology
          control
      )
  stamp <-
    mapLeft
      LocalStructuralStampProblem
      ( mkStructuralOccurrenceStamp
          occurrence
          predecessor
          (checkedPublicationId checked)
          digest
          (Publication.structuralSourceRole stage)
      )
  unattenuatedDeliveryRoute <-
    mapLeft
      LocalStructuralDeliveryRouteProblem
      ( PublicationRoute.extendStructuralSystemViews
          (checkedSystemId (startupGenesis state))
          (OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView (startupOracleProjectionState state)))
          (Publication.structuralSourceRole stage)
          (Publication.structuralSourceRoute stage)
      )
  let deliveryRoute = attenuateFrozenRoute (Publication.structuralSourceStrength stage) unattenuatedDeliveryRoute
  batches <-
    makeRemoteBatches
      (checkedLocalHeraldEpoch (startupGenesis state))
      ( currentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState state))
      )
      sourceTopology
      stage
      deliveryRoute
      checked
  peerPublications <-
    mapLeft
      LocalStructuralPeerPublicationProblem
      ( traverse
          (mkStructuralPeerPublication descriptor occurrenceId checked stamp)
          batches
      )
  preparedPublication <-
    mapLeft
      LocalStructuralPublicationOwnerProblem
      ( Publication.prepareStampedStructuralSourceStageAfterMembershipBase
          ( OracleProjection.oracleViewCurrentHeraldMembership
              (OracleProjection.oracleView (startupOracleProjectionState state))
          )
          stage
          sourceTopology
          stamp
          peerPublications
          (startupPublicationState state)
      )
  preparedStorePatch <-
    mapLeft
      LocalStructuralStorePatchProblem
      ( Store.prepareStructuralStorePatch
          (structuralOccurrenceCause occurrence)
          (Reconciliation.preparedStructuralStorePatch (preparedReconciliation ready))
          (startupStoreState state)
      )
  let storeAfterPatch = Store.commitStructuralStorePatch preparedStorePatch
      localRoute =
        routePartitionLocal
          ( partitionFrozenRoute
              (checkedLocalHeraldEpoch (startupGenesis state))
              (Publication.structuralSourceRoute stage)
          )
  preparedLocalStore <-
    mapLeft
      LocalStructuralStoreProblem
      ( Store.prepareRetainedStoreApplication
          ( Store.storeObservationEnvelope
              sourceProcess
              occurrenceId
              sourceTopology
              control
              (Just stamp)
          )
          localRoute
          checked
          storeAfterPatch
      )
  preparedPlacement <-
    mapLeft
      LocalStructuralPlacementProblem
      ( Placement.prepareStructuralPlacementPatch
          control
          (Reconciliation.preparedStructuralPlacementPatch (preparedReconciliation ready))
          (startupPlacementState state)
      )
  preparedProgress <-
    mapLeft
      LocalStructuralProgressProblem
      ( GraphProgress.prepareStructuralApplication
          stamp
          sourceTopology
          (preparedReconciliation ready)
          (startupGraphState state)
          progress
      )
  preparedAlignment <-
    mapLeft
      LocalStructuralAlignmentProblem
      ( Alignment.prepareStructuralDebtRetention
          (structuralOccurrenceCause occurrence)
          (Reconciliation.preparedStructuralDebts (preparedReconciliation ready))
          (startupAlignmentState state)
      )
  preparedPeerStream <-
    mapLeft
      LocalStructuralPeerStreamProblem
      ( PeerStream.prepareEnqueue
          (Map.map (\publication -> peerLogicalPublicationItem publication :| []) peerPublications)
          (startupPeerStreamState state)
      )
  let (uncertifiedPublication, _, _) = Publication.commitStampedStructuralStage preparedPublication
      (peerStream, assignments) = PeerStream.commitEnqueue preparedPeerStream
  publication <-
    mapLeft
      LocalStructuralPublicationOwnerProblem
      (Publication.certifyStructuralPublicationDispatch occurrence (fmap (fmap peerLogicalAssignmentReceipt) assignments) uncertifiedPublication)
  let store = Store.commitStoreApplication preparedLocalStore
      (placement, placementSnapshot) =
        Placement.commitStructuralPlacementPatch preparedPlacement
      (graph, structuralProgress, _) =
        GraphProgress.commitStructuralApplication preparedProgress
      (alignment, _) = Alignment.commitStructuralDebtRetention preparedAlignment
      idGenerator =
        maybe
          (startupIdGeneratorState state)
          IdGenerator.commitGeneratedId
          (preparedIdGenerator ready)
      successor =
        replaceStartupIdGeneratorState idGenerator
          . replaceStartupPublicationState publication
          . replaceStartupStoreState store
          . replaceStartupPlacementState placement
          . replaceStartupGraphState graph
          . replaceStartupStructuralProgressState structuralProgress
          . replaceStartupAlignmentState alignment
          . replaceStartupPeerStreamState peerStream
          $ state
      frontierEffects =
        [ SendPeerControl binding (PeerStreamFrontierAdvanced direction nextSequence)
        | (peer, (direction, nextSequence)) <-
            Map.toAscList (PeerStream.preparedEnqueueFrontierAdvances preparedPeerStream),
          Just binding <- [Discovery.currentPeerBinding peer (startupDiscoveryState state)]
        ]
      placementEffects =
        [ SendPeerControl binding (PeerPlacementUpdate (FullPlacementSnapshot snapshot))
        | snapshot <- maybe [] pure placementSnapshot,
          binding <- Discovery.currentPeerBindings (startupDiscoveryState state)
        ]
      ticketEffects =
        fmap SchedulePeerDispatch (PeerStream.preparedEnqueueDispatchTickets preparedPeerStream)
      report = GraphProgress.structuralLocalReport structuralProgress
      reportEffects =
        [ SendPeerControl binding (PeerStructuralAppliedReported report)
        | binding <- structuralReportBindings structuralProgress (Discovery.currentPeerBindings (startupDiscoveryState state))
        ]
  Right
    ( successor,
      placementEffects <> frontierEffects <> ticketEffects <> reportEffects
    )

validateSuccessorBase ::
  SuccessorStructuralBase ->
  HeraldState ->
  Either LocalStructuralWorkProblem ()
validateSuccessorBase successorBase state = do
  let expectedMembership =
        OracleProjection.oracleViewCurrentHeraldMembershipId
          (OracleProjection.oracleView (startupOracleProjectionState state))
      baseMembership = successorStructuralBaseGenerationId successorBase
      progress = startupStructuralProgressState state
      progressMembership =
        structuralVersionVectorMembershipGenerationId
          (GraphProgress.structuralAppliedVector progress)
      expectedCut = successorStructuralBaseCutId successorBase
      currentCut = GraphProgress.structuralLastInstalledCutId progress
  unless
    (baseMembership == expectedMembership)
    ( Left
        ( LocalStructuralSuccessorBaseMembershipMismatch
            expectedMembership
            baseMembership
        )
    )
  unless
    (progressMembership == expectedMembership)
    ( Left
        ( LocalStructuralSuccessorProgressMembershipMismatch
            expectedMembership
            progressMembership
        )
    )
  unless
    (GraphProgress.structuralSuccessorBaseInstalled successorBase progress)
    (Left (LocalStructuralSuccessorBaseCutMismatch expectedCut currentCut))

validateSuccessorAllocation ::
  Maybe SuccessorStructuralBase ->
  StructuralOccurrenceId ->
  StructuralVersionVector ->
  HeraldState ->
  Either LocalStructuralWorkProblem ()
validateSuccessorAllocation Nothing _ _ _ = Right ()
validateSuccessorAllocation (Just successorBase) occurrence predecessor state = do
  advance <-
    either
      (Left . LocalStructuralSuccessorAllocationProblem)
      Right
      ( allocateSuccessorStructuralOccurrence
          (checkedLocalHeraldEpoch (startupGenesis state))
          predecessor
          successorBase
      )
  unless
    ( successorStructuralAdvanceOccurrence advance == occurrence
        && successorStructuralAdvancePredecessor advance == predecessor
    )
    (Left LocalStructuralSuccessorOccurrenceMismatch)

exactCarrierEntry ::
  Publication.StructuralSourceStage ->
  HeraldState ->
  Either LocalStructuralWorkProblem SortRegistry.RegistryEntry
exactCarrierEntry stage state = do
  entry <-
    maybe
      (Left LocalStructuralSortMissing)
      Right
      ( SortRegistry.lookupEffectiveSort
          (checkedPublicationSort (Publication.structuralSourceChecked stage))
          (startupSortRegistryState state)
      )
  if SortRegistry.registryEntryOccurrenceId entry
    == Publication.structuralSourceSortOccurrenceId stage
    then Right entry
    else Left LocalStructuralSortOccurrenceMismatch

reconciliationViews ::
  Controlled.State ->
  HeraldState ->
  Reconciliation.ReconciliationViews
reconciliationViews controlled state =
  Reconciliation.withReconciliationDiagnosticChecks (startupDiagnosticChecks state)
    $ Reconciliation.reconciliationViewsWithEndedProcesses
      ( Map.fromList
          [ (process, (residence, OracleProjection.projectedEndedProcessControlIndex ended))
          | (process, ended) <- OracleProjection.projectedEndedProcesses oracle,
            Just residence <- [OracleProjection.oracleViewProcessResidence process (OracleProjection.oracleView oracle)]
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
          | process <- OracleProjection.projectedProcessEpochs oracle,
            OracleProjection.oracleViewProcessIsLive
              process
              (OracleProjection.oracleView oracle),
            Just residence <-
              [ OracleProjection.oracleViewProcessResidence
                  process
                  (OracleProjection.oracleView oracle)
              ]
          ]
      )
      (Set.fromList (Controlled.controlledNormalPossessionEntries controlled))
  where
    oracle = startupOracleProjectionState state

structuralSourceTopology ::
  Publication.StructuralSourceStage ->
  GraphProgress.StructuralProgressState ->
  TopologyCutId
structuralSourceTopology stage progress =
  case Publication.structuralSourceTopologyPrerequisite stage of
    Just prerequisite -> prerequisite
    Nothing -> GraphProgress.structuralGenesisCutId progress

sourceTopologyInstalled ::
  TopologyCutId ->
  GraphProgress.StructuralProgressState ->
  Bool
sourceTopologyInstalled cut progress =
  cut == GraphProgress.structuralGenesisCutId progress
    || GraphProgress.lookupInstalledTopologyCut cut progress /= Nothing

makeRemoteBatches ::
  HeraldEpoch ->
  Set.Set HeraldEpoch ->
  TopologyCutId ->
  Publication.StructuralSourceStage ->
  FrozenRoute ->
  CheckedPublication ->
  Either LocalStructuralWorkProblem (Map HeraldEpoch PublicationBatch)
makeRemoteBatches local activeHeralds sourceTopology stage deliveryRoute checked =
  traverse makeBatch grouped
  where
    grouped =
      foldl addDestination Map.empty (routeDestinations deliveryRoute)
    addDestination current destination
      | destinationHerald destination == local = current
      | not (Set.member (destinationHerald destination) activeHeralds) = current
      | otherwise =
          Map.insertWith
            (flip (<>))
            (destinationHerald destination)
            (toPeerDestination destination :| [])
            current
    makeBatch destinations =
      mapLeft
        LocalStructuralPublicationBatchProblem
        ( mkPublicationBatch
            (checkedPublicationId checked)
            (Publication.structuralSourceProcess stage)
            (checkedPublicationSort checked)
            (Publication.structuralSourceSortOccurrenceId stage)
            (checkedPublicationCanonicalValue checked)
            (Publication.structuralSourceStrength stage)
            sourceTopology
            (Publication.structuralSourceControlPrerequisite stage)
            destinations
        )
    toPeerDestination destination =
      publicationDestination
        (destinationDelta destination)
        (destinationStoreIncarnation destination)
        (destinationStrength destination)

currentHeraldMembership :: OracleProjection.View -> Set.Set HeraldEpoch
currentHeraldMembership =
  Set.fromList
    . NonEmpty.toList
    . heraldMembershipGenerationActiveHeraldEpochs
    . OracleProjection.oracleViewCurrentHeraldMembership

mapLeft :: (left -> mapped) -> Either left right -> Either mapped right
mapLeft convert = either (Left . convert) Right
