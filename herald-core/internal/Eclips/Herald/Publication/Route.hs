-- | Package-internal derivation of the immutable publication cut for one
-- writer.  The route is computed solely from the Graph and Placement owner
-- views; callers cannot supply destinations independently of those views.
module Eclips.Herald.Publication.Route
  ( WriterRouteError (..),
    freezeWriterRoute,
    extendStructuralSystemViews,
    extendPredefinedSystemViews,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Eclips.Domain.Identity (DeltaId, NablaId, SortId, SystemId)
import Eclips.Domain.Membership (HeraldMembershipGeneration, heraldMembershipGenerationActiveHeraldEpochs)
import Eclips.Domain.Route
  ( FrozenRoute,
    ReplicaStrength (Normal),
    RouteError,
    destinationDelta,
    destinationHerald,
    destinationStoreIncarnation,
    freezeRoute,
    routeDestination,
    routeDestinations,
  )
import Eclips.Domain.Sort.Descriptor (StructuralCarrierRole (..))
import Eclips.Domain.Sort.Profile (PredefinedSortRole (..))
import Eclips.Domain.Startup (deriveSystemViewDeltaId, deriveSystemViewStoreIncarnationId)
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Placement qualified as PlacementMessage
import Eclips.Herald.Placement.State qualified as Placement

data WriterRouteError
  = WriterRouteAmbiguousDestination DeltaId
  | WriterRouteShapeError RouteError
  deriving stock (Eq, Show)

-- | Preserve the admitted ordinary destinations while extending mandatory
-- structural delivery to every member of the installed stamping generation.
-- Retired destinations remain in the immutable route evidence and are omitted
-- only when the source constructs current peer assignments.
extendStructuralSystemViews ::
  SystemId -> HeraldMembershipGeneration -> StructuralCarrierRole -> FrozenRoute -> Either RouteError FrozenRoute
extendStructuralSystemViews system membership carrier frozen =
  extendPredefinedSystemViews system membership role frozen
  where
    role = case carrier of
      NeutralVertexCarrier -> NeutralVertexRole
      EdgeCarrier -> EdgeRole
      NablaCarrier -> NablaRole
      DeltaCarrier -> DeltaRole
      ProcessEpochCarrier -> ProcessEpochRole

-- | The same mandatory private destinations apply to regular sort-definition
-- publication. These destinations confer no application reachability.
extendPredefinedSystemViews ::
  SystemId -> HeraldMembershipGeneration -> PredefinedSortRole -> FrozenRoute -> Either RouteError FrozenRoute
extendPredefinedSystemViews system membership role frozen =
  freezeRoute (mandatory <> map normalize (routeDestinations frozen))
  where
    mandatory =
      [ routeDestination
          (deriveSystemViewDeltaId system herald role)
          (deriveSystemViewStoreIncarnationId system herald role)
          herald
          Normal
      | herald <- NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership)
      ]
    mandatoryDeltas = Set.fromList (map destinationDelta mandatory)
    normalize destination
      | Set.member (destinationDelta destination) mandatoryDeltas =
          routeDestination (destinationDelta destination) (destinationStoreIncarnation destination) (destinationHerald destination) Normal
      | otherwise = destination

-- | Freeze the unique matching placement, if any, for every reachable reader.
-- Reachable readers without a placement for this exact sort are omitted;
-- competing local/remote placements are a checked-state contradiction.
freezeWriterRoute ::
  NablaId ->
  SortId ->
  Graph.State ->
  Placement.State ->
  Either WriterRouteError FrozenRoute
freezeWriterRoute nabla sourceSort graph placement = do
  candidates <- traverse resolveDestination (Graph.graphReachableDeltas nabla graph)
  either (Left . WriterRouteShapeError) Right (freezeRoute (catMaybes candidates))
  where
    resolveDestination (delta, strength) =
      case localCandidates <> remoteCandidates of
        [] -> Right Nothing
        [destination] -> Right (Just destination)
        _ -> Left (WriterRouteAmbiguousDestination delta)
      where
        localCandidates =
          catMaybes
            [ do
                local <- Placement.lookupLocalPlacement delta placement
                if Placement.localPlacementSortId local == sourceSort
                  then
                    Just
                      ( routeDestination
                          delta
                          (Placement.localPlacementStoreIncarnation local)
                          (Placement.localPlacementHeraldEpoch local)
                          strength
                      )
                  else Nothing,
              do
                systemView <- Placement.lookupSystemViewPlacement delta placement
                if Placement.systemViewPlacementSortId systemView == sourceSort
                  then
                    Just
                      ( routeDestination
                          delta
                          (Placement.systemViewPlacementStoreIncarnation systemView)
                          (Placement.systemViewPlacementHeraldEpoch systemView)
                          strength
                      )
                  else Nothing
            ]
        remoteCandidates =
          [ routeDestination
              delta
              (PlacementMessage.deltaRouteStoreIncarnation route)
              owner
              strength
          | (owner, route) <- Placement.lookupRemotePlacements delta placement,
            PlacementMessage.deltaRouteSortId route == sourceSort
          ]
