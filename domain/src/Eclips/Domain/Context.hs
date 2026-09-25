{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Canonical per-sort context classes over the universal profile-0.1 graph.
--
-- Edges are never filtered by sort.  The caller supplies the active deltas of
-- one already-checked sort occurrence; every other vertex, including a passive
-- or differently typed delta, remains a neutral routing vertex.  Context
-- relations stop at the first other state-bearing class and retain the
-- strongest eligible path, where a wholly preserving path is stronger than a
-- path containing a weakening edge.
module Eclips.Domain.Context
  ( ContextProblem (..),
    ContextClass,
    contextClassMembers,
    ContextRelation,
    contextRelationSource,
    contextRelationDestination,
    contextRelationStrength,
    ContextGraph,
    contextGraphClasses,
    contextGraphRelations,
    deriveContextGraph,
  )
where

import Data.Graph (SCC (..), stronglyConnComp)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Graph
  ( EdgePayload,
    EdgeStrength (..),
    VertexId (..),
    edgeDestination,
    edgeSource,
    edgeStrength,
  )
import Eclips.Domain.Identity (DeltaId)
import Eclips.Domain.Route (ReplicaStrength (..))

-- | A malformed graph/context input.  Production callers derive these inputs
-- from checked graph and placement projections; keeping this operation total
-- also gives model tests a precise rejection boundary.
data ContextProblem
  = ContextEdgeSourceMissing VertexId
  | ContextEdgeDestinationMissing VertexId
  | ContextActiveDeltaMissing DeltaId
  deriving stock (Eq, Show)

-- | One state-bearing SCC, represented by its non-empty ascending active-delta
-- membership.  Neutral vertices in the same SCC are deliberately absent.
newtype ContextClass = ContextClass (NonEmpty DeltaId)
  deriving stock (Eq, Ord, Show)

contextClassMembers :: ContextClass -> NonEmpty DeltaId
contextClassMembers (ContextClass members) = members

-- | One immediate upstream-to-downstream context relation.  Strength is the
-- best path which reaches the destination without crossing a third active
-- class; @Normal@ therefore wins over @Weak@.
data ContextRelation = ContextRelation
  { source :: ContextClass,
    destination :: ContextClass,
    strength :: ReplicaStrength
  }
  deriving stock (Eq, Ord, Show)

contextRelationSource :: ContextRelation -> ContextClass
contextRelationSource relation = relation.source

contextRelationDestination :: ContextRelation -> ContextClass
contextRelationDestination relation = relation.destination

contextRelationStrength :: ContextRelation -> ReplicaStrength
contextRelationStrength relation = relation.strength

-- | Canonically ordered classes and relations for one sort occurrence.
data ContextGraph = ContextGraph
  { classes :: [ContextClass],
    relations :: [ContextRelation]
  }
  deriving stock (Eq, Show)

contextGraphClasses :: ContextGraph -> [ContextClass]
contextGraphClasses graph = graph.classes

contextGraphRelations :: ContextGraph -> [ContextRelation]
contextGraphRelations graph = graph.relations

-- | Contract the universal graph into state-bearing context classes.
--
-- Exact duplicate vertices, edges, and active-delta claims are harmless set
-- presentation differences.  An edge endpoint or active delta absent from the
-- supplied vertex set is rejected.  With no active deltas the result is the
-- empty context graph.
deriveContextGraph ::
  [VertexId] ->
  [EdgePayload] ->
  [DeltaId] ->
  Either ContextProblem ContextGraph
deriveContextGraph suppliedVertices suppliedEdges suppliedActiveDeltas = do
  mapM_ validateEdge edges
  mapM_ validateActiveDelta (Set.toAscList activeDeltas)
  let components = zip [0 :: Int ..] (fmap componentVertices stronglyConnected)
      componentByVertex =
        Map.fromList
          [ (vertex, component)
          | (component, members) <- components,
            vertex <- Set.toAscList members
          ]
      activeMembersByComponent =
        Map.fromListWith
          Set.union
          [ (componentByVertex Map.! DeltaVertex delta, Set.singleton delta)
          | delta <- Set.toAscList activeDeltas
          ]
      classByComponent = Map.map contextClassFromSet activeMembersByComponent
      classByActiveVertex =
        Map.fromList
          [ (DeltaVertex delta, classByComponent Map.! component)
          | delta <- Set.toAscList activeDeltas,
            let component = componentByVertex Map.! DeltaVertex delta
          ]
      canonicalClasses = Set.toAscList (Set.fromList (Map.elems classByComponent))
      relationMap =
        foldl
          (deriveFromClass classByActiveVertex adjacency)
          Map.empty
          (Map.elems classByComponent)
      canonicalRelations =
        [ ContextRelation source destination strength
        | ((source, destination), strength) <- Map.toAscList relationMap
        ]
  Right
    ContextGraph
      { classes = canonicalClasses,
        relations = canonicalRelations
      }
  where
    vertices = Set.fromList suppliedVertices
    edges = Set.toAscList (Set.fromList suppliedEdges)
    activeDeltas = Set.fromList suppliedActiveDeltas
    adjacency =
      Map.fromListWith
        (<>)
        [ (edgeSource edge, [(edgeDestination edge, edgeStrength edge)])
        | edge <- edges
        ]
    stronglyConnected =
      stronglyConnComp
        [ (vertex, vertex, fmap fst (Map.findWithDefault [] vertex adjacency))
        | vertex <- Set.toAscList vertices
        ]

    validateEdge edge
      | Set.notMember (edgeSource edge) vertices =
          Left (ContextEdgeSourceMissing (edgeSource edge))
      | Set.notMember (edgeDestination edge) vertices =
          Left (ContextEdgeDestinationMissing (edgeDestination edge))
      | otherwise = Right ()

    validateActiveDelta delta
      | Set.member (DeltaVertex delta) vertices = Right ()
      | otherwise = Left (ContextActiveDeltaMissing delta)

componentVertices :: SCC VertexId -> Set VertexId
componentVertices (AcyclicSCC vertex) = Set.singleton vertex
componentVertices (CyclicSCC vertices) = Set.fromList vertices

contextClassFromSet :: Set DeltaId -> ContextClass
contextClassFromSet members = case Set.toAscList members of
  first : remaining -> ContextClass (first :| remaining)
  [] -> error "active context component normalized to an empty member set"

deriveFromClass ::
  Map VertexId ContextClass ->
  Map VertexId [(VertexId, EdgeStrength)] ->
  Map (ContextClass, ContextClass) ReplicaStrength ->
  ContextClass ->
  Map (ContextClass, ContextClass) ReplicaStrength
deriveFromClass classByActiveVertex adjacency accumulated sourceClass =
  snd (walk initialPending initialBest accumulated)
  where
    initialVertices =
      fmap DeltaVertex (toList (contextClassMembers sourceClass))
    initialPending = fmap (,Normal) initialVertices
    initialBest = Map.fromList (fmap (,Normal) initialVertices)

    walk [] best relations = (best, relations)
    walk ((vertex, incoming) : pending) best relations =
      let (nextPending, nextBest, nextRelations) =
            foldl
              (advance incoming)
              (pending, best, relations)
              (Map.findWithDefault [] vertex adjacency)
       in walk nextPending nextBest nextRelations

    advance incoming (pending, best, relations) (destination, edge) =
      let candidate = attenuate incoming edge
       in case Map.lookup destination classByActiveVertex of
            Just destinationClass
              | destinationClass /= sourceClass ->
                  ( pending,
                    best,
                    Map.insertWith
                      max
                      (sourceClass, destinationClass)
                      candidate
                      relations
                  )
            _ ->
              case Map.lookup destination best of
                Just incumbent
                  | incumbent >= candidate -> (pending, best, relations)
                _ ->
                  ( pending <> [(destination, candidate)],
                    Map.insert destination candidate best,
                    relations
                  )

attenuate :: ReplicaStrength -> EdgeStrength -> ReplicaStrength
attenuate Normal Preserve = Normal
attenuate _ _ = Weak

toList :: NonEmpty value -> [value]
toList (first :| remaining) = first : remaining
