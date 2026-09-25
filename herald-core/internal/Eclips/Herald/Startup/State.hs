{-# LANGUAGE OverloadedRecordDot #-}

-- | The complete current Herald state product and its shutdown phases.
--
-- This module is package-private.  The ordinary initialization facade exports
-- 'HeraldState' abstractly; the read views and replacement helpers here exist
-- for composed transitions, invariant validation, and focused invariant tests.
module Eclips.Herald.Startup.State
  ( HeraldState,
    HeraldPhase (..),
    heraldPhase,
    HeraldDrainWitness,
    heraldDrainWitnessId,
    heraldDrainWitnessRequest,
    startupDrainWitness,
    HeraldStatic,
    heraldStatic,
    heraldStaticWithTiming,
    startupTakeoverTarget,
    startupDiagnosticChecks,
    configureHeraldDiagnosticChecks,
    heraldStaticGenesis,
    heraldStaticInitialBootstraps,
    servingHeraldState,
    startupApplicationRecoveryConfiguration,
    startupLastObservedTime,
    startupGenesis,
    startupInitialBootstraps,
    startupIdGeneratorState,
    startupOracleClientState,
    startupOracleProjectionState,
    startupControlOracleProjectionState,
    freezeStartupSemanticControl,
    startupSemanticControlIsFrozen,
    startupSortRegistryState,
    startupApplicationState,
    startupAdministrationState,
    startupConfiguredProcessState,
    startupProcessPreparationState,
    startupControlledState,
    startupDiscoveryState,
    startupDisappearanceState,
    startupPeerLivenessState,
    startupFailureDetectionState,
    startupIsolationState,
    startupTerminalSourceHoldState,
    startupGraphState,
    startupJoinState,
    replaceStartupJoinState,
    startupStructuralProgressState,
    startupStructuralBaseCoordinator,
    startupAlignmentState,
    startupAlignmentCutQueryCache,
    startupPeerStreamState,
    startupPeerDeliveryState,
    startupPlacementState,
    startupStoreState,
    startupPublicationState,
    startupWaitState,
    startupLabelBarrierState,
    startupLabelPatchState,
    startupVisibilityState,
    StartupStateWitness (..),
    startupStateWitness,
    replaceStartupLastObservedTime,
    replaceStartupIdGeneratorState,
    replaceStartupOracleClientState,
    replaceStartupOracleProjectionState,
    replaceStartupSortRegistryState,
    replaceStartupApplicationState,
    replaceStartupAdministrationState,
    replaceStartupConfiguredProcessState,
    replaceStartupProcessPreparationState,
    clearStartupPreparationScheduling,
    clearStartupCoordinationScheduling,
    replaceStartupControlledState,
    replaceStartupDiscoveryState,
    replaceStartupDisappearanceState,
    replaceStartupPeerLivenessState,
    replaceStartupFailureDetectionState,
    replaceStartupIsolationState,
    replaceStartupTerminalSourceHoldState,
    replaceStartupGraphState,
    replaceStartupStructuralProgressState,
    advanceStartupStructuralControlProgress,
    replaceStartupStructuralBaseCoordinator,
    replaceStartupAlignmentState,
    replaceStartupAlignmentCutQueryCache,
    replaceStartupPeerStreamState,
    replaceStartupPeerDeliveryState,
    replaceStartupPlacementState,
    replaceStartupStoreState,
    replaceStartupPublicationState,
    replaceStartupWaitState,
    replaceStartupLabelBarrierState,
    replaceStartupLabelPatchState,
    replaceStartupVisibilityState,
    beginHeraldDrain,
    finishHeraldDrain,
    replaceStartupDrainIdForInvariantTest,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Word (Word64)
import Eclips.Domain.Graph (EdgePayload, VertexId)
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    ControlIndex,
    HeraldEpoch,
    LabelDecisionId,
    ProcessEpochId,
  )
import Eclips.Domain.Publication (CheckedPublication)
import Eclips.Domain.Startup
  ( ConfigurationDigest,
    InitialProjectionDigest,
    appliedBootstrapManifestId,
  )
import Eclips.Herald.Administration (DrainId)
import Eclips.Herald.Administration.State qualified as Administration
import Eclips.Herald.Alignment.CutQueries qualified as CutQueries
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Application.PrivateIdentity (ProcessIdentityWitness, witnessedProcessEpoch)
import Eclips.Herald.Application.Recovery (ApplicationRecoveryConfiguration)
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Bootstrap
  ( BootstrapOwners,
    bootstrapApplicationState,
    bootstrapControlledState,
    bootstrapPlacementState,
    bootstrapStoreState,
  )
import Eclips.Herald.ConfiguredProcess.State qualified as Configured
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks (..))
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.Discovery (KnownHerald, PeerBinding)
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.FailureDetection.State qualified as FailureDetection
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    checkedConfigurationDigest,
    checkedLocalHeraldEpoch,
    checkedStartupAdmission,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource qualified as TerminalSource
import Eclips.Herald.IdGenerator.State qualified as IdGenerator
import Eclips.Herald.Internal.Derived (Derived, derive, derivedValue)
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.Join.State qualified as Join
import Eclips.Herald.LabelBarrier.State qualified as LabelBarrier
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerDelivery qualified as PeerDelivery
import Eclips.Herald.PeerLiveness.State qualified as PeerLiveness
import Eclips.Herald.PeerPayload (PeerLogicalPayload)
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.ProcessPreparation.State qualified as ProcessPreparation
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.TerminalSourceHold.State qualified as TerminalSourceHold
import Eclips.Herald.Time (MonotonicInstant)
import Eclips.Herald.UseCase.LabelPatch qualified as LabelPatch
import Eclips.Herald.Visibility.State qualified as Visibility
import Eclips.Herald.Wait.State qualified as Wait
import Eclips.Public.Types.Timing (TakeoverTarget, defaultTakeoverTarget)

-- | Immutable checked deployment and index-zero projection facts.
--
-- Retaining the opaque checked inputs makes the complete common topology
-- auditable without copying an independently authoritative graph manifest.
data HeraldStatic
  = HeraldStatic CheckedHeraldGenesis CheckedInitialBootstraps TakeoverTarget !DiagnosticChecks
  deriving stock (Eq)

heraldStatic :: CheckedHeraldGenesis -> CheckedInitialBootstraps -> HeraldStatic
heraldStatic genesis bootstraps = HeraldStatic genesis bootstraps defaultTakeoverTarget DiagnosticChecksEnabled

heraldStaticWithTiming :: TakeoverTarget -> CheckedHeraldGenesis -> CheckedInitialBootstraps -> HeraldStatic
heraldStaticWithTiming target genesis bootstraps = HeraldStatic genesis bootstraps target DiagnosticChecksEnabled

startupTakeoverTarget :: HeraldState -> TakeoverTarget
startupTakeoverTarget state = case (.facts) (servingProduct state) of
  HeraldStatic _ _ target _ -> target

-- | Immutable local execution policy, excluded from portable bases and wire data.
startupDiagnosticChecks :: HeraldState -> DiagnosticChecks
startupDiagnosticChecks state = case (.facts) (servingProduct state) of
  HeraldStatic _ _ _ checks -> checks

-- | Configure before starting the owner; internal reconstruction preserves it.
configureHeraldDiagnosticChecks :: DiagnosticChecks -> HeraldState -> HeraldState
configureHeraldDiagnosticChecks !checks = mapServingProduct $ \state ->
  case state.facts of
    HeraldStatic genesis bootstraps target _ ->
      state {facts = HeraldStatic genesis bootstraps target checks}

heraldStaticGenesis :: HeraldStatic -> CheckedHeraldGenesis
heraldStaticGenesis (HeraldStatic genesis _ _ _) = genesis

heraldStaticInitialBootstraps :: HeraldStatic -> CheckedInitialBootstraps
heraldStaticInitialBootstraps (HeraldStatic _ bootstraps _ _) = bootstraps

-- | The complete serving product retained through orderly shutdown.
data ServingState = ServingState
  { facts :: HeraldStatic,
    applicationRecoveryConfiguration :: ApplicationRecoveryConfiguration,
    lastObservedTime :: MonotonicInstant,
    idGenerator :: IdGenerator.State,
    oracleClient :: OracleClient.State,
    oracleProjection :: OracleProjection.State,
    frozenOracleProjection :: Maybe OracleProjection.State,
    sortRegistry :: SortRegistry.State,
    application :: Application.State,
    administration :: Administration.State,
    configuredProcesses :: Configured.State,
    processPreparation :: ProcessPreparation.State,
    controlled :: Controlled.State,
    discovery :: Discovery.State,
    disappearance :: Disappearance.State,
    peerLiveness :: PeerLiveness.State,
    failureDetection :: FailureDetection.State,
    isolation :: Isolation.State,
    terminalSourceHold :: TerminalSourceHold.State,
    graph :: Graph.State,
    join :: Join.State,
    structuralProgress :: GraphProgress.StructuralProgressState,
    structuralBaseCoordinator :: Maybe TerminalSource.MembershipBaseClosure,
    alignment :: Alignment.State,
    alignmentCutQueries :: Derived CutQueries.CutQueryCache,
    peerStream :: PeerStream.State PeerLogicalPayload,
    peerDelivery :: PeerDelivery.State,
    placement :: Placement.State,
    store :: Store.State,
    publication :: Publication.State,
    wait :: Wait.State,
    labelBarrier :: LabelBarrier.State,
    labelPatches :: LabelPatch.State,
    visibility :: Visibility.State LabelPatch.TargetWorkKey
  }
  deriving stock (Eq)

data DrainState = DrainState DrainId Administration.DrainRequestRef
  deriving stock (Eq)

data StoppedState = StoppedState DrainState ServingState
  deriving stock (Eq)

-- | The phase sum makes the shutdown admission cut explicit.
data HeraldState
  = Serving ServingState
  | Draining DrainState ServingState
  | Stopped StoppedState
  deriving stock (Eq)

data HeraldPhase
  = HeraldServing
  | HeraldDraining
  | HeraldStopped
  deriving stock (Eq, Ord, Show, Enum, Bounded)

heraldPhase :: HeraldState -> HeraldPhase
heraldPhase (Serving _) = HeraldServing
heraldPhase (Draining _ _) = HeraldDraining
heraldPhase (Stopped _) = HeraldStopped

-- | Read-only relation between the phase and administration-owned request.
data HeraldDrainWitness = HeraldDrainWitness
  { heraldDrainWitnessId :: DrainId,
    heraldDrainWitnessRequest :: Administration.DrainRequestRef
  }
  deriving stock (Eq, Show)

startupDrainWitness :: HeraldState -> Maybe HeraldDrainWitness
startupDrainWitness (Serving _) = Nothing
startupDrainWitness (Draining (DrainState drain reference) _) =
  Just (HeraldDrainWitness drain reference)
startupDrainWitness (Stopped (StoppedState (DrainState drain reference) _)) =
  Just (HeraldDrainWitness drain reference)

servingHeraldState ::
  HeraldStatic ->
  ApplicationRecoveryConfiguration ->
  MonotonicInstant ->
  IdGenerator.State ->
  OracleClient.State ->
  OracleProjection.State ->
  SortRegistry.State ->
  Graph.State ->
  GraphProgress.StructuralProgressState ->
  Discovery.State ->
  PeerLiveness.State ->
  FailureDetection.State ->
  Isolation.State ->
  PeerStream.State PeerLogicalPayload ->
  BootstrapOwners ->
  HeraldState
servingHeraldState staticFacts applicationRecoveryConfiguration observedTime idGenerator oracleClient oracleProjection sortRegistry initialGraph structuralProgress discovery peerLiveness failureDetection isolation peerStream owners =
  Serving
    ServingState
      { facts = staticFacts,
        applicationRecoveryConfiguration,
        lastObservedTime = observedTime,
        idGenerator,
        oracleClient,
        oracleProjection = oracleProjection,
        frozenOracleProjection = Nothing,
        sortRegistry = SortRegistry.clearAlignmentPlanChange sortRegistry,
        application = Application.clearPreparationSessionChanges initialApplication,
        administration =
          Administration.initialState
            (checkedLocalHeraldEpoch (heraldStaticGenesis staticFacts)),
        configuredProcesses = Configured.initialState,
        processPreparation =
          ProcessPreparation.seedPreparationContext
            ( ProcessPreparation.PreparationContext
                (OracleProjection.oracleViewCurrentHeraldMembershipId initialOracleView)
                (OracleProjection.oracleViewLocalHeraldIsCurrent initialOracleView && Isolation.isolationWitnessPhase (Isolation.stateWitness isolation) `notElem` [Isolation.IsolationReadOnlyDrainView, Isolation.IsolationTerminalView])
                (Application.applicationGatePhase initialApplication)
                (Application.applicationMembershipGateClosed initialApplication)
                (OracleClient.oracleClientCurrentBinding oracleClient)
            )
            ProcessPreparation.emptyState,
        controlled = Controlled.clearPreparationChanges (bootstrapControlledState owners),
        discovery = Discovery.clearPreparationBindingChanges discovery,
        disappearance = Disappearance.initialState (checkedLocalHeraldEpoch (heraldStaticGenesis staticFacts)),
        peerLiveness,
        failureDetection,
        isolation,
        terminalSourceHold = TerminalSourceHold.initialState,
        graph = Graph.clearAlignmentPlanChange initialGraph,
        join = Join.emptyState,
        structuralProgress = GraphProgress.clearAlignmentPlanChange (GraphProgress.clearPreparationReadinessChange structuralProgress),
        structuralBaseCoordinator = Nothing,
        -- Initial assembly has no closure waiters. Record its semantic
        -- membership now so the first unrelated input cannot create a wakeup.
        alignment =
          Alignment.clearLossScheduling
            . Alignment.initializeTransferMemberWork (checkedLocalHeraldEpoch (heraldStaticGenesis staticFacts))
            . Alignment.synchronizeGenerationPlanContext (OracleProjection.oracleViewCurrentHeraldMembershipId initialOracleView) False
            $ Alignment.initialState (OracleProjection.oracleViewCurrentHeraldMembershipId initialOracleView),
        alignmentCutQueries = derive CutQueries.emptyCutQueryCache,
        peerStream = PeerStream.clearIncomingMarkerWork peerStream,
        peerDelivery = PeerDelivery.emptyState,
        placement = Placement.clearAlignmentLossChange . Placement.clearAlignmentPlanChange . Placement.clearPreparationPlacementChanges $ bootstrapPlacementState owners,
        -- No alignment subscription exists in this initial product. Future
        -- source admissions start dirty, so pre-runtime Store births have no
        -- outstanding observers and need no first-input notification drain.
        store = Store.clearAlignmentMemberStoreChanges . Store.clearAlignmentLossChange . Store.clearPreparationSlotChanges . Store.clearSourceStoreChanges $ bootstrapStoreState owners,
        publication =
          Publication.initialStateWithGenesis
            (heraldStaticGenesis staticFacts),
        wait = Wait.emptyState,
        labelBarrier =
          LabelBarrier.initialState
            (checkedLocalHeraldEpoch (heraldStaticGenesis staticFacts)),
        labelPatches = LabelPatch.initialState,
        visibility = Visibility.initialState
      }
  where
    initialOracleView = OracleProjection.oracleView oracleProjection
    initialApplication =
      Application.setApplicationMembershipGate
        (isJust (checkedStartupAdmission (heraldStaticGenesis staticFacts)))
        (bootstrapApplicationState owners)

startupApplicationRecoveryConfiguration ::
  HeraldState -> ApplicationRecoveryConfiguration
startupApplicationRecoveryConfiguration =
  (.applicationRecoveryConfiguration) . servingProduct

startupLastObservedTime :: HeraldState -> MonotonicInstant
startupLastObservedTime = (.lastObservedTime) . servingProduct

startupGenesis :: HeraldState -> CheckedHeraldGenesis
startupGenesis = heraldStaticGenesis . (.facts) . servingProduct

startupInitialBootstraps :: HeraldState -> CheckedInitialBootstraps
startupInitialBootstraps = heraldStaticInitialBootstraps . (.facts) . servingProduct

startupIdGeneratorState :: HeraldState -> IdGenerator.State
startupIdGeneratorState = (.idGenerator) . servingProduct

startupOracleClientState :: HeraldState -> OracleClient.State
startupOracleClientState = (.oracleClient) . servingProduct

startupOracleProjectionState :: HeraldState -> OracleProjection.State
startupOracleProjectionState state =
  let serving = servingProduct state
   in fromMaybe serving.oracleProjection serving.frozenOracleProjection

-- | Current control observations remain available after the semantic epoch
-- has fenced. Semantic owners continue to interpret their immutable fence cut.
startupControlOracleProjectionState :: HeraldState -> OracleProjection.State
startupControlOracleProjectionState = (.oracleProjection) . servingProduct

startupSemanticControlIsFrozen :: HeraldState -> Bool
startupSemanticControlIsFrozen = isJust . (.frozenOracleProjection) . servingProduct

freezeStartupSemanticControl :: HeraldState -> HeraldState
freezeStartupSemanticControl = mapServingProduct $ \state ->
  state {frozenOracleProjection = Just (fromMaybe state.oracleProjection state.frozenOracleProjection)}

startupSortRegistryState :: HeraldState -> SortRegistry.State
startupSortRegistryState = (.sortRegistry) . servingProduct

startupApplicationState :: HeraldState -> Application.State
startupApplicationState = (.application) . servingProduct

startupAdministrationState :: HeraldState -> Administration.State
startupAdministrationState = (.administration) . servingProduct

startupConfiguredProcessState :: HeraldState -> Configured.State
startupConfiguredProcessState = (.configuredProcesses) . servingProduct

startupProcessPreparationState :: HeraldState -> ProcessPreparation.State
startupProcessPreparationState = (.processPreparation) . servingProduct

startupControlledState :: HeraldState -> Controlled.State
startupControlledState = (.controlled) . servingProduct

startupDisappearanceState :: HeraldState -> Disappearance.State
startupDisappearanceState = (.disappearance) . servingProduct

startupDiscoveryState :: HeraldState -> Discovery.State
startupDiscoveryState = (.discovery) . servingProduct

startupPeerLivenessState :: HeraldState -> PeerLiveness.State
startupPeerLivenessState = (.peerLiveness) . servingProduct

startupFailureDetectionState :: HeraldState -> FailureDetection.State
startupFailureDetectionState = (.failureDetection) . servingProduct

startupIsolationState :: HeraldState -> Isolation.State
startupIsolationState = (.isolation) . servingProduct

startupTerminalSourceHoldState :: HeraldState -> TerminalSourceHold.State
startupTerminalSourceHoldState = (.terminalSourceHold) . servingProduct

startupJoinState :: HeraldState -> Join.State
startupJoinState = (.join) . servingProduct

replaceStartupJoinState :: Join.State -> HeraldState -> HeraldState
replaceStartupJoinState replacement = mapServingProduct (\state -> state {join = replacement})

startupGraphState :: HeraldState -> Graph.State
startupGraphState = (.graph) . servingProduct

startupStructuralProgressState ::
  HeraldState -> GraphProgress.StructuralProgressState
startupStructuralProgressState = (.structuralProgress) . servingProduct

startupStructuralBaseCoordinator ::
  HeraldState -> Maybe TerminalSource.MembershipBaseClosure
startupStructuralBaseCoordinator = (.structuralBaseCoordinator) . servingProduct

startupAlignmentState :: HeraldState -> Alignment.State
startupAlignmentState = (.alignment) . servingProduct

-- | Non-authoritative historical cut answers, scoped to their owner inputs.
startupAlignmentCutQueryCache :: HeraldState -> CutQueries.CutQueryCache
startupAlignmentCutQueryCache = derivedValue . (.alignmentCutQueries) . servingProduct

startupPeerDeliveryState :: HeraldState -> PeerDelivery.State
startupPeerDeliveryState = (.peerDelivery) . servingProduct

startupPeerStreamState :: HeraldState -> PeerStream.State PeerLogicalPayload
startupPeerStreamState = (.peerStream) . servingProduct

startupPlacementState :: HeraldState -> Placement.State
startupPlacementState = (.placement) . servingProduct

startupStoreState :: HeraldState -> Store.State
startupStoreState = (.store) . servingProduct

startupPublicationState :: HeraldState -> Publication.State
startupPublicationState = (.publication) . servingProduct

startupWaitState :: HeraldState -> Wait.State
startupWaitState = (.wait) . servingProduct

startupLabelBarrierState :: HeraldState -> LabelBarrier.State
startupLabelBarrierState = (.labelBarrier) . servingProduct

startupLabelPatchState :: HeraldState -> LabelPatch.State
startupLabelPatchState = (.labelPatches) . servingProduct

startupVisibilityState ::
  HeraldState -> Visibility.State LabelPatch.TargetWorkKey
startupVisibilityState = (.visibility) . servingProduct

-- | Stable, read-only evidence for deterministic and cross-owner properties.
-- It contains facts rather than owner constructors, and therefore cannot be
-- used to synthesize a 'HeraldState'.
data StartupStateWitness = StartupStateWitness
  { startupWitnessPhase :: HeraldPhase,
    startupWitnessDrain :: Maybe HeraldDrainWitness,
    startupWitnessAdministration :: Administration.AdministrationStateWitness,
    startupWitnessConfigurationDigest :: ConfigurationDigest,
    startupWitnessInitialProjectionDigest :: InitialProjectionDigest,
    startupWitnessLastObservedTime :: MonotonicInstant,
    startupWitnessNextGeneratorCounter :: Word64,
    startupWitnessOracleClient :: OracleClient.OracleClientStateWitness,
    startupWitnessOracleControlIndex :: ControlIndex,
    startupWitnessLocalHeraldEpoch :: HeraldEpoch,
    startupWitnessActiveHeraldEpochs :: [HeraldEpoch],
    startupWitnessProjectedBootstraps :: [(ProcessEpochId, BootstrapManifestId)],
    startupWitnessApplicationIdentities :: [ProcessIdentityWitness],
    startupWitnessApplicationAccess :: [(ProcessEpochId, Maybe Application.BootstrapAccess)],
    startupWitnessConfiguredScaffolds ::
      [(ProcessEpochId, Configured.LiveProcessScaffold)],
    startupWitnessControlledProcesses :: [Controlled.ProcessFact],
    startupWitnessControlledRoots :: [Controlled.RootFact],
    startupWitnessPeerBindings :: [PeerBinding],
    startupWitnessKnownHeralds :: [KnownHerald],
    startupWitnessPeerLiveness :: PeerLiveness.StateWitness,
    startupWitnessFailureDetection ::
      FailureDetection.FailureDetectionStateWitness,
    startupWitnessIsolation :: Isolation.IsolationStateWitness,
    startupWitnessPeerStream ::
      Either PeerStream.PeerStreamInvariantViolation PeerStream.PeerStreamStateWitness,
    startupWitnessGraphVertices :: [VertexId],
    startupWitnessGraphEdges :: [EdgePayload],
    startupWitnessPlacements :: [Placement.LocalPlacement],
    startupWitnessSystemViewPlacements :: [Placement.SystemViewPlacement],
    startupWitnessStoreSlots :: [Store.StoreSlot],
    startupWitnessPrimordialPublications :: [CheckedPublication],
    startupWitnessSortRegistry :: [SortRegistry.RegistryEntry],
    startupWitnessPublication :: Publication.PublicationStateWitness,
    startupWitnessWaits :: [Wait.WaitRegistration],
    startupWitnessLabelBarrier :: [LabelBarrier.TerminalOutcomeEvidence],
    startupWitnessLabelPatches ::
      [(LabelDecisionId, LabelPatch.RetainedPatchView)],
    startupWitnessVisibilityDependencies ::
      [(LabelPatch.TargetWorkKey, NonEmpty Visibility.VisibilityDependency)]
  }
  deriving stock (Eq, Show)

startupStateWitness :: HeraldState -> StartupStateWitness
startupStateWitness state =
  StartupStateWitness
    { startupWitnessPhase = heraldPhase state,
      startupWitnessDrain = startupDrainWitness state,
      startupWitnessAdministration =
        Administration.administrationStateWitness
          (startupAdministrationState state),
      startupWitnessConfigurationDigest = checkedConfigurationDigest genesis,
      startupWitnessInitialProjectionDigest =
        OracleProjection.oracleViewInitialProjectionDigest oracleView,
      startupWitnessLastObservedTime = startupLastObservedTime state,
      startupWitnessNextGeneratorCounter =
        IdGenerator.witnessedNextGeneratorCounter
          (IdGenerator.idGeneratorStateWitness (startupIdGeneratorState state)),
      startupWitnessOracleClient =
        OracleClient.oracleClientStateWitness (startupOracleClientState state),
      startupWitnessOracleControlIndex = OracleProjection.oracleViewControlIndex oracleView,
      startupWitnessLocalHeraldEpoch = OracleProjection.oracleViewLocalHeraldEpoch oracleView,
      startupWitnessActiveHeraldEpochs = OracleProjection.oracleViewActiveHeraldEpochs oracleView,
      startupWitnessProjectedBootstraps =
        [ (process, appliedBootstrapManifestId bootstrap)
        | (process, bootstrap) <- OracleProjection.projectedBootstraps oracleState
        ],
      startupWitnessApplicationIdentities = applicationIdentities,
      startupWitnessApplicationAccess =
        [ ( process,
            Application.applicationBootstrapAccess process applicationState
          )
        | process <- fmap witnessedProcessEpoch applicationIdentities
        ],
      startupWitnessConfiguredScaffolds =
        Configured.configuredProcessScaffoldEntries
          (startupConfiguredProcessState state),
      startupWitnessControlledProcesses = Controlled.controlledProcessFacts controlledState,
      startupWitnessControlledRoots = Controlled.controlledRootFacts controlledState,
      startupWitnessPeerBindings = Discovery.currentPeerBindings discoveryState,
      startupWitnessKnownHeralds = Discovery.knownHeralds discoveryState,
      startupWitnessPeerLiveness =
        PeerLiveness.stateWitness (startupPeerLivenessState state),
      startupWitnessFailureDetection =
        FailureDetection.stateWitness (startupFailureDetectionState state),
      startupWitnessIsolation =
        Isolation.stateWitness (startupIsolationState state),
      startupWitnessPeerStream =
        PeerStream.peerStreamStateWitness (startupPeerStreamState state),
      startupWitnessGraphVertices = Graph.graphVertices graphState,
      startupWitnessGraphEdges = Graph.graphEdges graphState,
      startupWitnessPlacements = Placement.localPlacements placementState,
      startupWitnessSystemViewPlacements =
        Placement.systemViewPlacements placementState,
      startupWitnessStoreSlots = Store.storeSlots storeState,
      startupWitnessPrimordialPublications = Store.primordialSeedPublications storeState,
      startupWitnessSortRegistry = SortRegistry.registryEntries sortRegistryState,
      startupWitnessPublication =
        Publication.publicationStateWitness (startupPublicationState state),
      startupWitnessWaits = Wait.waitRegistrations (startupWaitState state),
      startupWitnessLabelBarrier =
        LabelBarrier.barrierTerminalEvidences (startupLabelBarrierState state),
      startupWitnessLabelPatches =
        Map.toAscList (LabelPatch.retainedLabelPatches (startupLabelPatchState state)),
      startupWitnessVisibilityDependencies =
        Map.toAscList
          ( Visibility.visibilityDependencyRelations
              (startupVisibilityState state)
          )
    }
  where
    genesis = startupGenesis state
    oracleState = startupOracleProjectionState state
    oracleView = OracleProjection.oracleView oracleState
    applicationState = startupApplicationState state
    applicationIdentities = Application.applicationProcessWitnesses applicationState
    controlledState = startupControlledState state
    discoveryState = startupDiscoveryState state
    graphState = startupGraphState state
    placementState = startupPlacementState state
    storeState = startupStoreState state
    sortRegistryState = startupSortRegistryState state

replaceStartupLastObservedTime ::
  MonotonicInstant -> HeraldState -> HeraldState
replaceStartupLastObservedTime replacement =
  mapServingProduct (\state -> state {lastObservedTime = replacement})

replaceStartupIdGeneratorState :: IdGenerator.State -> HeraldState -> HeraldState
replaceStartupIdGeneratorState replacement =
  mapServingProduct (\state -> state {idGenerator = replacement})

replaceStartupOracleClientState :: OracleClient.State -> HeraldState -> HeraldState
replaceStartupOracleClientState replacement =
  mapServingProduct (\state -> state {oracleClient = replacement})

replaceStartupOracleProjectionState ::
  OracleProjection.State -> HeraldState -> HeraldState
replaceStartupOracleProjectionState replacement =
  mapServingProduct (\state -> state {oracleProjection = replacement})

replaceStartupSortRegistryState :: SortRegistry.State -> HeraldState -> HeraldState
replaceStartupSortRegistryState replacement =
  mapServingProduct (\state -> state {sortRegistry = replacement, alignmentCutQueries = derive CutQueries.emptyCutQueryCache})

replaceStartupApplicationState :: Application.State -> HeraldState -> HeraldState
replaceStartupApplicationState replacement =
  mapServingProduct (\state -> state {application = replacement})

replaceStartupAdministrationState ::
  Administration.State -> HeraldState -> HeraldState
replaceStartupAdministrationState replacement =
  mapServingProduct (\state -> state {administration = replacement})

replaceStartupConfiguredProcessState ::
  Configured.State -> HeraldState -> HeraldState
replaceStartupConfiguredProcessState replacement =
  mapServingProduct (\state -> state {configuredProcesses = replacement})

replaceStartupProcessPreparationState ::
  ProcessPreparation.State -> HeraldState -> HeraldState
replaceStartupProcessPreparationState replacement =
  mapServingProduct (\state -> state {processPreparation = replacement})

-- | Comparison-only copies erase consumed notifications and cached outcomes.
-- Ordinary owner equality still observes all metadata. Relationship indexes,
-- dependency watches and result observation facts remain visible and auditable.
-- Never install this normalized copy as a transition successor.
clearStartupPreparationScheduling :: HeraldState -> HeraldState
clearStartupPreparationScheduling = mapServingProduct $ \state ->
  state
    { processPreparation = ProcessPreparation.clearPreparationScheduling state.processPreparation,
      application = Application.clearPreparationSessionChanges state.application,
      controlled = Controlled.clearPreparationChanges state.controlled,
      discovery = Discovery.clearPreparationBindingChanges state.discovery,
      structuralProgress = GraphProgress.clearPreparationReadinessChange state.structuralProgress,
      placement = Placement.clearPreparationPlacementChanges state.placement,
      store = Store.clearPreparationSlotChanges state.store,
      alignment = Alignment.clearPreparationReadinessChanges state.alignment
    }

-- | Comparison-only extension for the remaining coordination stages. Exact
-- dependency relations and the immutable local-member identity stay visible.
clearStartupCoordinationScheduling :: HeraldState -> HeraldState
clearStartupCoordinationScheduling = clearCoordination . clearStartupPreparationScheduling
  where
    clearCoordination = mapServingProduct $ \state ->
      state
        { graph = Graph.clearAlignmentPlanChange state.graph,
          structuralProgress = GraphProgress.clearAlignmentPlanChange state.structuralProgress,
          sortRegistry = SortRegistry.clearAlignmentPlanChange state.sortRegistry,
          placement = Placement.clearAlignmentLossChange (Placement.clearAlignmentPlanChange state.placement),
          store = Store.clearAlignmentMemberStoreChanges (Store.clearAlignmentLossChange state.store),
          alignment = Alignment.clearDestinationProgressChanges . Alignment.clearRouteMarkerChanges . Alignment.clearLossScheduling . Alignment.clearTransferWork . Alignment.clearGenerationScheduling $ state.alignment,
          peerStream = PeerStream.clearIncomingMarkerWork state.peerStream,
          disappearance = Disappearance.clearMarkerProbeChanges state.disappearance
        }

replaceStartupControlledState :: Controlled.State -> HeraldState -> HeraldState
replaceStartupControlledState replacement =
  mapServingProduct (\state -> state {controlled = replacement})

replaceStartupDisappearanceState :: Disappearance.State -> HeraldState -> HeraldState
replaceStartupDisappearanceState replacement =
  mapServingProduct (\state -> state {disappearance = replacement})

replaceStartupDiscoveryState :: Discovery.State -> HeraldState -> HeraldState
replaceStartupDiscoveryState replacement =
  mapServingProduct (\state -> state {discovery = replacement})

replaceStartupPeerLivenessState :: PeerLiveness.State -> HeraldState -> HeraldState
replaceStartupPeerLivenessState replacement =
  mapServingProduct (\state -> state {peerLiveness = replacement})

replaceStartupFailureDetectionState ::
  FailureDetection.State -> HeraldState -> HeraldState
replaceStartupFailureDetectionState replacement =
  mapServingProduct (\state -> state {failureDetection = replacement})

replaceStartupIsolationState :: Isolation.State -> HeraldState -> HeraldState
replaceStartupIsolationState replacement =
  mapServingProduct (\state -> state {isolation = replacement})

replaceStartupTerminalSourceHoldState ::
  TerminalSourceHold.State -> HeraldState -> HeraldState
replaceStartupTerminalSourceHoldState replacement =
  mapServingProduct (\state -> state {terminalSourceHold = replacement})

replaceStartupGraphState :: Graph.State -> HeraldState -> HeraldState
replaceStartupGraphState replacement =
  mapServingProduct (\state -> state {graph = replacement, alignmentCutQueries = derive CutQueries.emptyCutQueryCache})

replaceStartupStructuralProgressState ::
  GraphProgress.StructuralProgressState -> HeraldState -> HeraldState
replaceStartupStructuralProgressState replacement =
  mapServingProduct (\state -> state {structuralProgress = replacement, alignmentCutQueries = derive CutQueries.emptyCutQueryCache})

-- | Advancing the applied cursor changes reports and control readiness, never
-- installed cut frontiers, retained reconciliation, or placement. Keep historical
-- cut queries, including negative coverage answers, across this checked step.
advanceStartupStructuralControlProgress ::
  ControlIndex -> HeraldState -> Either GraphProgress.StructuralProgressProblem HeraldState
advanceStartupStructuralControlProgress prefix state = do
  prepared <- GraphProgress.prepareStructuralControlProgress prefix (startupStructuralProgressState state)
  let (progress, _) = GraphProgress.commitStructuralControlProgress prepared
  pure (mapServingProduct (\owners -> owners {structuralProgress = progress}) state)

replaceStartupStructuralBaseCoordinator ::
  Maybe TerminalSource.MembershipBaseClosure -> HeraldState -> HeraldState
replaceStartupStructuralBaseCoordinator replacement =
  mapServingProduct (\state -> state {structuralBaseCoordinator = replacement})

replaceStartupAlignmentState :: Alignment.State -> HeraldState -> HeraldState
replaceStartupAlignmentState replacement =
  mapServingProduct (\state -> state {alignment = replacement})

replaceStartupAlignmentCutQueryCache :: CutQueries.CutQueryCache -> HeraldState -> HeraldState
replaceStartupAlignmentCutQueryCache replacement =
  mapServingProduct (\state -> state {alignmentCutQueries = derive replacement})

replaceStartupPeerDeliveryState :: PeerDelivery.State -> HeraldState -> HeraldState
replaceStartupPeerDeliveryState replacement = mapServingProduct (\state -> state {peerDelivery = replacement})

replaceStartupPeerStreamState ::
  PeerStream.State PeerLogicalPayload -> HeraldState -> HeraldState
replaceStartupPeerStreamState replacement =
  mapServingProduct (\state -> state {peerStream = replacement})

replaceStartupPlacementState :: Placement.State -> HeraldState -> HeraldState
replaceStartupPlacementState replacement =
  mapServingProduct $ \state ->
    let !queries = CutQueries.invalidatePlacementCutQueries (derivedValue state.alignmentCutQueries)
     in state {placement = replacement, alignmentCutQueries = derive queries}

replaceStartupStoreState :: Store.State -> HeraldState -> HeraldState
replaceStartupStoreState replacement =
  mapServingProduct (\state -> state {store = replacement})

replaceStartupPublicationState :: Publication.State -> HeraldState -> HeraldState
replaceStartupPublicationState replacement =
  mapServingProduct (\state -> state {publication = replacement})

replaceStartupWaitState :: Wait.State -> HeraldState -> HeraldState
replaceStartupWaitState replacement =
  mapServingProduct (\state -> state {wait = replacement})

replaceStartupLabelBarrierState ::
  LabelBarrier.State -> HeraldState -> HeraldState
replaceStartupLabelBarrierState replacement =
  mapServingProduct (\state -> state {labelBarrier = replacement})

replaceStartupLabelPatchState ::
  LabelPatch.State -> HeraldState -> HeraldState
replaceStartupLabelPatchState replacement =
  mapServingProduct (\state -> state {labelPatches = replacement})

replaceStartupVisibilityState ::
  Visibility.State LabelPatch.TargetWorkKey -> HeraldState -> HeraldState
replaceStartupVisibilityState replacement =
  mapServingProduct (\state -> state {visibility = replacement})

-- | Cross the semantic shutdown cut from the sole serving phase.
beginHeraldDrain ::
  Administration.DrainRequestRef -> HeraldState -> Maybe HeraldState
beginHeraldDrain reference (Serving state) =
  Just
    ( Draining
        ( DrainState
            (Administration.drainRequestRefDrainId reference)
            reference
        )
        state
    )
beginHeraldDrain _ _ = Nothing

-- | Retain the final serving product and drain reference for diagnostics.
finishHeraldDrain :: HeraldState -> Maybe HeraldState
finishHeraldDrain (Draining drain state) =
  Just (Stopped (StoppedState drain state))
finishHeraldDrain _ = Nothing

-- | Package-private contradiction fixture for the production phase validator.
replaceStartupDrainIdForInvariantTest :: DrainId -> HeraldState -> HeraldState
replaceStartupDrainIdForInvariantTest _ state@(Serving _) = state
replaceStartupDrainIdForInvariantTest replacement (Draining (DrainState _ reference) state) =
  Draining (DrainState replacement reference) state
replaceStartupDrainIdForInvariantTest
  replacement
  (Stopped (StoppedState (DrainState _ reference) state)) =
    Stopped (StoppedState (DrainState replacement reference) state)

servingProduct :: HeraldState -> ServingState
servingProduct (Serving state) = state
servingProduct (Draining _ state) = state
servingProduct (Stopped (StoppedState _ state)) = state

mapServingProduct :: (ServingState -> ServingState) -> HeraldState -> HeraldState
mapServingProduct update (Serving state) = Serving (update state)
mapServingProduct update (Draining drain state) = Draining drain (update state)
mapServingProduct update (Stopped (StoppedState drain state)) =
  Stopped (StoppedState drain (update state))
