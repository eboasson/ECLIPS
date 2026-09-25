{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Immutable publication routing cuts.
module Eclips.Domain.Route
  ( ReplicaStrength (..),
    RouteDestination,
    routeDestination,
    destinationDelta,
    destinationStoreIncarnation,
    destinationHerald,
    destinationStrength,
    FrozenRoute,
    RouteError (..),
    freezeRoute,
    attenuateFrozenRoute,
    routeDestinations,
    routeDestinationCount,
    RoutePartition,
    partitionFrozenRoute,
    routePartitionLocal,
    routePartitionRemote,
  )
where

import Control.Monad (foldM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Eclips.Domain.Identity
  ( DeltaId,
    HeraldEpoch,
    StoreIncarnationId,
  )

-- | Replica strength ordered from weak to normal so joins use 'max'.
data ReplicaStrength
  = Weak
  | Normal
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | One exact destination captured at publication acceptance.
data RouteDestination = RouteDestination
  { delta :: DeltaId,
    storeIncarnation :: StoreIncarnationId,
    herald :: HeraldEpoch,
    strength :: ReplicaStrength
  }
  deriving stock (Eq, Ord, Show)

routeDestination ::
  DeltaId ->
  StoreIncarnationId ->
  HeraldEpoch ->
  ReplicaStrength ->
  RouteDestination
routeDestination = RouteDestination

destinationDelta :: RouteDestination -> DeltaId
destinationDelta destination = destination.delta

destinationStoreIncarnation :: RouteDestination -> StoreIncarnationId
destinationStoreIncarnation destination = destination.storeIncarnation

destinationHerald :: RouteDestination -> HeraldEpoch
destinationHerald destination = destination.herald

destinationStrength :: RouteDestination -> ReplicaStrength
destinationStrength destination = destination.strength

-- | A frozen route has at most one exact current incarnation for each delta.
newtype FrozenRoute = FrozenRoute (Map DeltaId RouteDestination)
  deriving stock (Eq, Show)

-- | One lossless partition of a frozen cut around a hosting Herald.
--
-- The local cut remains a 'FrozenRoute', including when empty. Remote cuts are
-- non-empty by construction, keyed and traversed in Herald order, with each
-- destination list retaining the canonical Delta order of the source cut.
data RoutePartition = RoutePartition
  { local :: FrozenRoute,
    remote :: Map HeraldEpoch (NonEmpty RouteDestination)
  }
  deriving stock (Eq, Show)

-- | Conflicting placement facts cannot form a routing cut.
data RouteError
  = ConflictingRouteDestination DeltaId RouteDestination RouteDestination
  deriving stock (Eq, Show)

-- | Freeze a complete route. Exact duplicates collapse; unequal observations of
-- one delta are rejected. An empty route is a valid cut.
freezeRoute :: [RouteDestination] -> Either RouteError FrozenRoute
freezeRoute destinations = FrozenRoute <$> foldM insertDestination Map.empty destinations
  where
    insertDestination ::
      Map DeltaId RouteDestination ->
      RouteDestination ->
      Either RouteError (Map DeltaId RouteDestination)
    insertDestination current candidate =
      case Map.lookup candidate.delta current of
        Nothing -> Right (Map.insert candidate.delta candidate current)
        Just incumbent
          | incumbent == candidate -> Right current
          | otherwise ->
              Left
                ( ConflictingRouteDestination
                    candidate.delta
                    incumbent
                    candidate
                )

-- | Attenuate every destination in an already frozen cut by one source
-- strength.  Because 'Weak < Normal', taking the minimum preserves every route
-- constraint while ensuring a weak source can never create a normal replica.
-- Delta identity, store incarnation, and hosting Herald remain unchanged.
attenuateFrozenRoute :: ReplicaStrength -> FrozenRoute -> FrozenRoute
attenuateFrozenRoute sourceStrength (FrozenRoute destinations) =
  FrozenRoute (Map.map attenuate destinations)
  where
    attenuate destination =
      destination {strength = min sourceStrength destination.strength}

routeDestinations :: FrozenRoute -> [RouteDestination]
routeDestinations (FrozenRoute destinations) = Map.elems destinations

routeDestinationCount :: FrozenRoute -> Int
routeDestinationCount (FrozenRoute destinations) = Map.size destinations

-- | Partition one already frozen route without recomputing or weakening it.
-- Every destination appears in exactly one result cut.
partitionFrozenRoute :: HeraldEpoch -> FrozenRoute -> RoutePartition
partitionFrozenRoute localHerald (FrozenRoute destinations) =
  RoutePartition
    { local = FrozenRoute localDestinations,
      remote = remoteDestinations
    }
  where
    (localDestinations, remoteDestinations) =
      foldl insertDestination (Map.empty, Map.empty) (Map.toAscList destinations)
    insertDestination (localCut, remoteCuts) (delta, destination)
      | destinationHerald destination == localHerald =
          (Map.insert delta destination localCut, remoteCuts)
      | otherwise =
          ( localCut,
            Map.alter
              (Just . appendDestination destination)
              (destinationHerald destination)
              remoteCuts
          )
    appendDestination destination Nothing = destination :| []
    appendDestination destination (Just current) = current <> (destination :| [])

routePartitionLocal :: RoutePartition -> FrozenRoute
routePartitionLocal (RoutePartition localCut _) = localCut

routePartitionRemote :: RoutePartition -> Map HeraldEpoch (NonEmpty RouteDestination)
routePartitionRemote (RoutePartition _ remoteCuts) = remoteCuts
