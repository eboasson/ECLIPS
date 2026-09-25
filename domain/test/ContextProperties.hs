{-# LANGUAGE OverloadedStrings #-}

module ContextProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Context
  ( ContextClass,
    ContextGraph,
    ContextProblem (..),
    contextClassMembers,
    contextGraphClasses,
    contextGraphRelations,
    contextRelationDestination,
    contextRelationSource,
    contextRelationStrength,
    deriveContextGraph,
  )
import Eclips.Domain.Graph
  ( EdgePayload,
    EdgeStrength (..),
    VertexId (..),
    edgeDestination,
    edgePayload,
    edgeSource,
    edgeStrength,
  )
import Eclips.Domain.Identity
  ( DeltaId,
    GlobalObjectId,
    mkDeltaId,
    mkGlobalObjectId,
  )
import Eclips.Domain.Route (ReplicaStrength (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    conjoin,
    counterexample,
    elements,
    forAll,
    shuffle,
    sublistOf,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "context derivation"
    [ testCase "missing graph facts are rejected precisely" caseMalformed,
      testCase "active same-sort deltas in one universal SCC form one class" caseUniversalScc,
      testCase "passive deltas are neutral and relations stop at the first active class" caseImmediateRelations,
      testProperty "presentation order and exact duplicates do not affect derivation" propPresentationCanonical,
      testProperty "finite generated graphs agree with an independent SCC/path model" propGeneratedReference
    ]

caseMalformed :: IO ()
caseMalformed = do
  assertEqual
    "missing edge source"
    (Left (ContextEdgeSourceMissing (DeltaVertex deltaA)))
    ( deriveContextGraph
        [DeltaVertex deltaB]
        [edgePayload (DeltaVertex deltaA) (DeltaVertex deltaB) Preserve]
        [deltaB]
    )
  assertEqual
    "missing active delta"
    (Left (ContextActiveDeltaMissing deltaA))
    (deriveContextGraph [DeltaVertex deltaB] [] [deltaA])

caseUniversalScc :: IO ()
caseUniversalScc = do
  let vertices = [DeltaVertex deltaA, neutralVertexA, DeltaVertex deltaB]
      edges =
        [ edgePayload (DeltaVertex deltaA) neutralVertexA Weaken,
          edgePayload neutralVertexA (DeltaVertex deltaB) Preserve,
          edgePayload (DeltaVertex deltaB) (DeltaVertex deltaA) Preserve
        ]
      graph = checked "universal SCC" (deriveContextGraph vertices edges [deltaB, deltaA])
  assertEqual
    "only active delta members appear in the class"
    [[deltaA, deltaB]]
    (fmap classMembers (contextGraphClasses graph))
  assertEqual "a class has no relation to itself" [] (relationTriples graph)

caseImmediateRelations :: IO ()
caseImmediateRelations = do
  let graph = checked "immediate relations" (deriveContextGraph contextVertices contextEdges contextActive)
  assertEqual
    "passive delta is not a state-bearing class"
    [[deltaA], [deltaB], [deltaC]]
    (fmap classMembers (contextGraphClasses graph))
  assertEqual
    "the preserving path wins and traversal stops before crossing B"
    [ ([deltaA], [deltaB], Normal),
      ([deltaB], [deltaC], Weak)
    ]
    (relationTriples graph)

propPresentationCanonical :: Property
propPresentationCanonical =
  forAll (shuffle contextVertices) $ \vertices ->
    forAll (shuffle contextEdges) $ \edges ->
      forAll (shuffle contextActive) $ \active ->
        let expected = deriveContextGraph contextVertices contextEdges contextActive
            duplicated =
              deriveContextGraph
                (vertices <> take 2 vertices)
                (edges <> take 2 edges)
                (active <> take 1 active)
         in conjoin
              [ deriveContextGraph vertices edges active === expected,
                duplicated === expected
              ]

data FiniteContextCase = FiniteContextCase
  { finiteVertices :: [VertexId],
    finiteEdges :: [EdgePayload],
    finiteActive :: [DeltaId]
  }
  deriving stock (Show)

genFiniteContextCase :: Gen FiniteContextCase
genFiniteContextCase = do
  deltaCount <- chooseInt (0, 4)
  neutralCount <- chooseInt (0, 3)
  let deltas = [delta (0x70 + fromIntegral ordinal) | ordinal <- [0 .. deltaCount - 1]]
      neutrals =
        [ NeutralVertex (globalObject (0x90 + fromIntegral ordinal))
        | ordinal <- [0 .. neutralCount - 1]
        ]
      vertices = fmap DeltaVertex deltas <> neutrals
      pairs = [(source, destination) | source <- vertices, destination <- vertices]
  active <- sublistOf deltas
  selectedPairs <- sublistOf pairs
  strengths <- traverse (const (elements [Preserve, Weaken])) selectedPairs
  pure
    FiniteContextCase
      { finiteVertices = vertices,
        finiteEdges = zipWith (uncurry edgePayload) selectedPairs strengths,
        finiteActive = active
      }

propGeneratedReference :: Property
propGeneratedReference =
  forAll genFiniteContextCase $ \sample ->
    let expected = referenceContext sample
        observed =
          projectContext
            <$> deriveContextGraph
              (finiteVertices sample)
              (finiteEdges sample)
              (finiteActive sample)
     in counterexample ("generated context: " <> show sample) (observed === Right expected)

type ContextProjection =
  ( [[DeltaId]],
    [([DeltaId], [DeltaId], ReplicaStrength)]
  )

projectContext :: ContextGraph -> ContextProjection
projectContext graph =
  ( fmap classMembers (contextGraphClasses graph),
    relationTriples graph
  )

referenceContext :: FiniteContextCase -> ContextProjection
referenceContext sample =
  (classes, relations)
  where
    adjacency =
      Map.fromListWith
        (<>)
        [ (edgeSource edge, [(edgeDestination edge, edgeStrength edge)])
        | edge <- finiteEdges sample
        ]
    activeSet = Set.fromList (finiteActive sample)
    components =
      Set.toAscList
        ( Set.fromList
            [ Set.filter
                (\candidate -> mutuallyReachable adjacency deltaId candidate)
                activeSet
            | deltaId <- Set.toAscList activeSet
            ]
        )
    classes = fmap Set.toAscList components
    relations =
      [ (Set.toAscList source, Set.toAscList destination, strength)
      | source <- components,
        destination <- components,
        source /= destination,
        Just strength <- [referenceRelationStrength adjacency activeSet source destination]
      ]

mutuallyReachable ::
  Map VertexId [(VertexId, EdgeStrength)] ->
  DeltaId ->
  DeltaId ->
  Bool
mutuallyReachable adjacency left right =
  Set.member (DeltaVertex right) (reachableVertices adjacency (Set.singleton (DeltaVertex left)))
    && Set.member (DeltaVertex left) (reachableVertices adjacency (Set.singleton (DeltaVertex right)))

reachableVertices ::
  Map VertexId [(VertexId, EdgeStrength)] ->
  Set VertexId ->
  Set VertexId
reachableVertices adjacency initial = go initial (Set.toList initial)
  where
    go visited [] = visited
    go visited (vertex : remaining) =
      let unseen =
            [ destination
            | (destination, _) <- Map.findWithDefault [] vertex adjacency,
              Set.notMember destination visited
            ]
          successorVisited = Set.union visited (Set.fromList unseen)
       in go successorVisited (remaining <> unseen)

referenceRelationStrength ::
  Map VertexId [(VertexId, EdgeStrength)] ->
  Set DeltaId ->
  Set DeltaId ->
  Set DeltaId ->
  Maybe ReplicaStrength
referenceRelationStrength adjacency active source destination
  | immediatePath (== Preserve) = Just Normal
  | immediatePath (const True) = Just Weak
  | otherwise = Nothing
  where
    sourceVertices = Set.map DeltaVertex source
    destinationVertices = Set.map DeltaVertex destination
    foreignActiveVertices = Set.map DeltaVertex (active `Set.difference` source)

    immediatePath edgeAllowed = walk sourceVertices (Set.toList sourceVertices)
      where
        walk _ [] = False
        walk visited (vertex : remaining) =
          let candidates =
                [ next
                | (next, strength) <- Map.findWithDefault [] vertex adjacency,
                  edgeAllowed strength,
                  Set.notMember next visited
                ]
           in any (`Set.member` destinationVertices) candidates
                || let traversable =
                         filter (`Set.notMember` foreignActiveVertices) candidates
                       successorVisited = Set.union visited (Set.fromList candidates)
                    in walk successorVisited (remaining <> traversable)

contextVertices :: [VertexId]
contextVertices =
  [ DeltaVertex deltaA,
    DeltaVertex passiveDelta,
    neutralVertexA,
    neutralVertexB,
    DeltaVertex deltaB,
    DeltaVertex deltaC
  ]

contextEdges :: [EdgePayload]
contextEdges =
  [ edgePayload (DeltaVertex deltaA) (DeltaVertex passiveDelta) Preserve,
    edgePayload (DeltaVertex passiveDelta) neutralVertexA Weaken,
    edgePayload neutralVertexA (DeltaVertex deltaB) Preserve,
    edgePayload (DeltaVertex deltaA) neutralVertexB Preserve,
    edgePayload neutralVertexB (DeltaVertex deltaB) Preserve,
    edgePayload (DeltaVertex deltaB) (DeltaVertex deltaC) Weaken
  ]

contextActive :: [DeltaId]
contextActive = [deltaA, deltaB, deltaC]

relationTriples ::
  ContextGraph ->
  [([DeltaId], [DeltaId], ReplicaStrength)]
relationTriples graph =
  [ ( classMembers (contextRelationSource relation),
      classMembers (contextRelationDestination relation),
      contextRelationStrength relation
    )
  | relation <- contextGraphRelations graph
  ]

classMembers :: ContextClass -> [DeltaId]
classMembers = NonEmpty.toList . contextClassMembers

deltaA, deltaB, deltaC, passiveDelta :: DeltaId
deltaA = delta 0x11
deltaB = delta 0x22
deltaC = delta 0x33
passiveDelta = delta 0x44

neutralVertexA, neutralVertexB :: VertexId
neutralVertexA = NeutralVertex (globalObject 0x55)
neutralVertexB = NeutralVertex (globalObject 0x66)

delta :: Word -> DeltaId
delta byte =
  checked "delta" (mkDeltaId (ByteString.replicate 32 (fromIntegral byte)))

globalObject :: Word -> GlobalObjectId
globalObject byte =
  checked
    "global object"
    (mkGlobalObjectId (ByteString.replicate 32 (fromIntegral byte)))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
