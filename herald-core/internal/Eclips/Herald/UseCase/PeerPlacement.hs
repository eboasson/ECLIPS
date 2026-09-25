-- | Admission of remote placement claims against the immutable checked
-- startup projection and retained structurally applied history. Sequence
-- ownership remains in 'Placement.State'; this module proves that a claimed
-- route is one the named Herald may host.
module Eclips.Herald.UseCase.PeerPlacement
  ( placementUpdateClaimsAreValid,
    remotePlacementRouteIsValid,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Eclips.Domain.Graph (VertexId (DeltaVertex))
import Eclips.Domain.Identity (DeltaId, HeraldEpoch, ProcessEpochId, SystemId)
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    AppliedRoot,
    AppliedRootRole (ReaderRoot),
    appliedProcessEpochId,
    appliedProcessResidence,
    appliedProcessRoots,
    appliedRootControlPrerequisite,
    appliedRootObjectId,
    appliedRootOccurrenceId,
    appliedRootPlacement,
    appliedRootRole,
    appliedRootSortId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Placement
  ( DeltaRoute,
    DeltaRouteView (..),
    PlacementUpdate (..),
    placementSnapshotRoutes,
    validatePrivateSystemViewDeltaRoute,
    viewDeltaRoute,
  )
import Eclips.Herald.Structural.Debt
  ( sortOccurrence,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation

-- | Validate every route claim carried by one complete placement snapshot.
placementUpdateClaimsAreValid ::
  SystemId ->
  HeraldEpoch ->
  [(ProcessEpochId, AppliedProcessBootstrap)] ->
  Graph.State ->
  GraphProgress.StructuralProgressState ->
  PlacementUpdate ->
  Bool
placementUpdateClaimsAreValid system owner bootstraps graph progress (FullPlacementSnapshot snapshot) =
  all
    (preparedRemotePlacementRouteIsValid system owner roots graph admission)
    (placementSnapshotRoutes snapshot)
  where
    roots = prepareReaderRoots owner bootstraps
    admission =
      Reconciliation.prepareDeltaRouteAdmission
        (GraphProgress.structuralProgressReconciliation progress)

-- | Check an application route against either its exact applied reader root
-- or an occurrence-exact retained dynamic Delta meaning. Private system-view
-- routes remain checked against their closed derivation. The uncontrolled
-- configured shortcut and private routes must still name the common checked
-- Graph vertex; retained provenance deliberately admits an older owner after
-- the receiver's current Graph winner or control overlay has advanced.
remotePlacementRouteIsValid ::
  SystemId ->
  HeraldEpoch ->
  [(ProcessEpochId, AppliedProcessBootstrap)] ->
  Graph.State ->
  GraphProgress.StructuralProgressState ->
  DeltaRoute ->
  Bool
remotePlacementRouteIsValid system owner bootstraps graph progress =
  preparedRemotePlacementRouteIsValid
    system
    owner
    (prepareReaderRoots owner bootstraps)
    graph
    ( Reconciliation.prepareDeltaRouteAdmission
        (GraphProgress.structuralProgressReconciliation progress)
    )

preparedRemotePlacementRouteIsValid ::
  SystemId ->
  HeraldEpoch ->
  Map (ProcessEpochId, DeltaId) AppliedRoot ->
  Graph.State ->
  Reconciliation.PreparedDeltaRouteAdmission ->
  DeltaRoute ->
  Bool
preparedRemotePlacementRouteIsValid system owner roots graph admission route =
  case viewDeltaRoute route of
    PrivateSystemViewDeltaRouteView {} ->
      Graph.graphHasVertex (DeltaVertex delta) graph
        && validatePrivateSystemViewDeltaRoute system owner route == Right ()
    ApplicationDeltaRouteView
      _
      sortId
      occurrence
      controller
      process
      _incarnation
      prerequisite ->
        validUncontrolledGenesis
          || Reconciliation.preparedDeltaRouteIsValid
            delta
            controller
            (sortOccurrence sortId occurrence)
            (Reconciliation.LiveProcessController process owner)
            prerequisite
            admission
        where
          validUncontrolledGenesis =
            case Map.lookup (process, delta) roots of
              Just root ->
                not
                  ( Reconciliation.preparedDeltaRouteObjectHasControlHistory
                      controller
                      admission
                  )
                  && Graph.graphHasVertex (DeltaVertex delta) graph
                  && appliedRootSortId root == sortId
                  && appliedRootOccurrenceId root == occurrence
                  && appliedRootObjectId root == controller
                  -- The checked root fixes residence, while its current hosting
                  -- Herald is authoritative for the fresh physical incarnation.
                  -- Requiring the index-zero incarnation here would reject a
                  -- legitimate later reactivation of the same checked Delta.
                  && maybe False ((== owner) . fst) (appliedRootPlacement root)
                  && appliedRootControlPrerequisite root == prerequisite
              Nothing -> False
  where
    delta = case viewDeltaRoute route of
      ApplicationDeltaRouteView observed _ _ _ _ _ _ -> observed
      PrivateSystemViewDeltaRouteView _ observed _ _ _ -> observed

-- Both lookups deliberately retain the first entry, matching the checked
-- bootstrap-list lookup and reader-root lookup used by the single-route path.
prepareReaderRoots ::
  HeraldEpoch ->
  [(ProcessEpochId, AppliedProcessBootstrap)] ->
  Map (ProcessEpochId, DeltaId) AppliedRoot
prepareReaderRoots owner bootstraps =
  Map.fromListWith
    keepFirst
    [ ((process, delta), root)
    | (process, bootstrap) <- Map.toAscList (Map.fromListWith keepFirst bootstraps),
      appliedProcessEpochId bootstrap == process,
      appliedProcessResidence bootstrap == owner,
      root <- appliedProcessRoots bootstrap,
      ReaderRoot delta <- [appliedRootRole root]
    ]
  where
    keepFirst _ old = old
