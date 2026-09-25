-- | Fresh semantic owners derived only from the checked immutable bootstrap.
-- Operational owners, identity allocation and runtime policy stay with the
-- enclosing initialization or private joining transaction.
module Eclips.Herald.Startup.Semantic
  ( InitialSemanticOwners,
    prepareInitialSemanticOwners,
    initialSemanticBootstrapOwners,
    initialSemanticPlacement,
    initialSemanticRegistry,
    initialSemanticGraph,
    initialSemanticProgress,
  )
where

import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    appliedBootstrapManifestId,
    appliedProcessEpochId,
    appliedProcessResidence,
  )
import Eclips.Domain.Structural (emptyStructuralVersionVector)
import Eclips.Herald.Bootstrap
  ( BootstrapOwners,
    SystemViewRoutingInvariant (..),
    bootstrapControlledState,
    bootstrapPlacementState,
    bootstrapViews,
    commitProcessBootstrap,
    initialBootstrapOwners,
    installSystemViewRouting,
    prepareProcessBootstrap,
  )
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedInitialTopologyProjection,
    checkedLocalHeraldEpoch,
    checkedStartupAdmission,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (..),
    StartupInvariantSubject (..),
    StartupInvariantViolation (..),
    bootstrapInvariantFault,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt (sortOccurrence)
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation

data InitialSemanticOwners
  = InitialSemanticOwners
      !BootstrapOwners
      !Placement.State
      !SortRegistry.State
      !Graph.State
      !GraphProgress.StructuralProgressState

initialSemanticBootstrapOwners :: InitialSemanticOwners -> BootstrapOwners
initialSemanticBootstrapOwners (InitialSemanticOwners owners _ _ _ _) = owners

initialSemanticPlacement :: InitialSemanticOwners -> Placement.State
initialSemanticPlacement (InitialSemanticOwners _ placement _ _ _) = placement

initialSemanticRegistry :: InitialSemanticOwners -> SortRegistry.State
initialSemanticRegistry (InitialSemanticOwners _ _ registry _ _) = registry

initialSemanticGraph :: InitialSemanticOwners -> Graph.State
initialSemanticGraph (InitialSemanticOwners _ _ _ graph _) = graph

initialSemanticProgress :: InitialSemanticOwners -> GraphProgress.StructuralProgressState
initialSemanticProgress (InitialSemanticOwners _ _ _ _ progress) = progress

-- | Admit the same bootstrap projection for ordinary startup and a private
-- joining origin. The returned owners contain no source Herald or live protocol
-- state, and joining observers cannot contribute ordinary structural progress.
prepareInitialSemanticOwners :: CheckedHeraldGenesis -> CheckedInitialBootstraps -> Either HeraldInvariantFault InitialSemanticOwners
prepareInitialSemanticOwners genesis checkedBootstraps = do
  initialOwners <-
    either
      (Left . initialStoreFault)
      Right
      (initialBootstrapOwners genesis)
  processOwners <- foldM installResident initialOwners residentBootstraps
  owners <-
    either
      (Left . systemViewRoutingFault)
      Right
      (installSystemViewRouting localEpoch processOwners)
  preparedInitialPlacement <-
    either
      (const (Left placementInitializationFault))
      Right
      (Placement.prepareLocalSnapshot (bootstrapPlacementState owners))
  let (initialPlacement, _) = Placement.commitLocalSnapshot preparedInitialPlacement
      initialMembership = OracleProjection.oracleViewCurrentHeraldMembership (OracleProjection.oracleView oracleState)
      emptyVector = emptyStructuralVersionVector initialMembership
      initialGraph = Graph.initialTopologyState (checkedInitialTopologyProjection checkedBootstraps)
      reconciliationViews =
        Reconciliation.reconciliationViews
          localEpoch
          ( Map.fromList
              [ ( SortRegistry.registryEntrySortId entry,
                  sortOccurrence
                    (SortRegistry.registryEntrySortId entry)
                    (SortRegistry.registryEntryOccurrenceId entry)
                )
              | entry <- SortRegistry.registryEntries registry
              ]
          )
          (Set.fromList (Graph.graphBaselineVertices initialGraph))
          (Map.fromList [(appliedProcessEpochId bootstrap, appliedProcessResidence bootstrap) | bootstrap <- allBootstraps])
          (Set.fromList (Controlled.controlledNormalPossessionEntries (bootstrapControlledState owners)))
  seededResult <-
    either
      (const (Left structuralProgressInitializationFault))
      Right
      ( Reconciliation.structuralAppliedStateWithGenesis
          (checkedSystemId genesis)
          reconciliationViews
          emptyVector
          allBootstraps
      )
  reconciliation <-
    either
      (const (Left structuralProgressInitializationFault))
      Right
      seededResult
  preparedGraphBaseline <-
    either
      (const (Left structuralProgressInitializationFault))
      Right
      (Graph.prepareStructuralGraphBaseline reconciliation initialGraph)
  let claimedGraph = Graph.commitStructuralGraphBaseline preparedGraphBaseline
  structuralProgress <-
    either
      (const (Left structuralProgressInitializationFault))
      Right
      ( (if isJust (checkedStartupAdmission genesis) then GraphProgress.initialJoiningStructuralProgressState else GraphProgress.initialStructuralProgressState)
          (checkedSystemId genesis)
          localEpoch
          initialMembership
          (checkedInitialProjectionDigest checkedBootstraps)
          reconciliation
      )
  pure (InitialSemanticOwners owners initialPlacement registry claimedGraph structuralProgress)
  where
    oracleState = OracleProjection.initialState genesis checkedBootstraps
    registry = SortRegistry.initialState genesis
    views = bootstrapViews (OracleProjection.oracleView oracleState) (SortRegistry.sortView registry)
    localEpoch = checkedLocalHeraldEpoch genesis
    residentBootstraps = filter ((== localEpoch) . appliedProcessResidence) allBootstraps
    allBootstraps = checkedInitialBootstraps checkedBootstraps
    installResident :: BootstrapOwners -> AppliedProcessBootstrap -> Either HeraldInvariantFault BootstrapOwners
    installResident owners bootstrap =
      case prepareProcessBootstrap views bootstrap owners of
        Left problem -> Left (bootstrapInvariantFault (appliedBootstrapManifestId bootstrap) problem)
        Right prepared -> Right (fst (commitProcessBootstrap prepared))

initialStoreFault :: Store.StorePreparationError -> HeraldInvariantFault
initialStoreFault problem =
  HeraldStartupInvariant StartupStatic $ case problem of
    Store.StoreRejected _ -> SystemViewStoreSetInvariant
    Store.StoreInvariantContradiction _ -> SystemViewStoreContentsInvariant

systemViewRoutingFault :: SystemViewRoutingInvariant -> HeraldInvariantFault
systemViewRoutingFault problem =
  HeraldStartupInvariant StartupStatic $ case problem of
    SystemViewGraphContradiction _ -> SystemViewGraphInvariant
    SystemViewPlacementContradiction _ -> SystemViewPlacementInvariant

placementInitializationFault :: HeraldInvariantFault
placementInitializationFault = HeraldStartupInvariant StartupStatic SystemViewPlacementInvariant

structuralProgressInitializationFault :: HeraldInvariantFault
structuralProgressInitializationFault = HeraldStartupInvariant StartupStatic StructuralProgressInvariant
