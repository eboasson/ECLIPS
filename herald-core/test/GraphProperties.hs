module GraphProperties
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word8)
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
    NablaId,
    mkDeltaId,
    mkGlobalObjectId,
    mkNablaId,
  )
import Eclips.Domain.Route (ReplicaStrength (..))
import Eclips.Herald.Graph.State qualified as Graph
import GenesisFixtures (fixtureIdentifierBytes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    chooseInt,
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
    "Graph reachability"
    [ testCase
        "neutral insertion is prepared, idempotent, and preserves every edge"
        caseNeutralInsertion,
      testProperty
        "generated cyclic graphs agree with an independent closure model and ignore presentation order"
        propReachabilityModel
    ]

caseNeutralInsertion :: Assertion
caseNeutralInsertion = do
  let source = checked "source NablaId" (mkNablaId (fixtureIdentifierBytes 91))
      delta = checked "destination DeltaId" (mkDeltaId (fixtureIdentifierBytes 92))
      object = checked "neutral GlobalObjectId" (mkGlobalObjectId (fixtureIdentifierBytes 93))
      edge = edgePayload (NablaVertex source) (DeltaVertex delta) Preserve
      initial =
        Graph.graphStateForReachabilityTest
          [NablaVertex source, DeltaVertex delta]
          [edge]
      once =
        Graph.commitNeutralVertexInsertion
          (Graph.prepareNeutralVertexInsertion object initial)
      twice =
        Graph.commitNeutralVertexInsertion
          (Graph.prepareNeutralVertexInsertion object once)
  assertBool
    "the controlled object becomes a neutral vertex"
    (Graph.graphHasVertex (NeutralVertex object) once)
  assertEqual "insertion leaves edges exact" (Graph.graphEdges initial) (Graph.graphEdges once)
  assertBool "repeated insertion is state-idempotent" (once == twice)
  assertEqual "new baseline vertices wake plan selection" True (fst (Graph.takeAlignmentPlanChange once))
  let clean = snd (Graph.takeAlignmentPlanChange once)
      replay = Graph.commitNeutralVertexInsertion (Graph.prepareNeutralVertexInsertion object clean)
  assertEqual "drained exact insertion does not wake plans" False (fst (Graph.takeAlignmentPlanChange replay))
  assertBool "taking preserves graph facts" (Graph.graphVertices clean == Graph.graphVertices once && Graph.graphEdges clean == Graph.graphEdges once)

propReachabilityModel :: Property
propReachabilityModel =
  forAll reachabilityCase $ \(source, vertices, presentedEdges) ->
    forAll (shuffle presentedEdges) $ \permutedEdges ->
      let expected = referenceReachableDeltas source vertices presentedEdges
          direct =
            Graph.graphReachableDeltas
              source
              (Graph.graphStateForReachabilityTest vertices presentedEdges)
          permuted =
            Graph.graphReachableDeltas
              source
              (Graph.graphStateForReachabilityTest (reverse vertices) permutedEdges)
       in counterexample
            ( "expected "
                <> show expected
                <> ", direct "
                <> show direct
                <> ", permuted "
                <> show permuted
            )
            ((direct, permuted) === (expected, expected))

-- Every generated case deliberately contains all graph shapes that previously
-- lacked coverage: a weak-only multi-hop destination, competing weak and
-- preserving paths, a reachable cycle, and an unreachable delta. Random
-- optional edges and strengths broaden the closure while retaining those four
-- witnesses.
reachabilityCase :: Gen (NablaId, [VertexId], [EdgePayload])
reachabilityCase = do
  offset <- chooseInt (0, 80)
  let identity seed = fromIntegral ((seed + offset) `mod` 251) :: Word8
      source = checked "source NablaId" (mkNablaId (fixtureIdentifierBytes (identity 1)))
      weakDelta = checked "weak DeltaId" (mkDeltaId (fixtureIdentifierBytes (identity 2)))
      strongestDelta = checked "strongest DeltaId" (mkDeltaId (fixtureIdentifierBytes (identity 3)))
      unreachableDelta = checked "unreachable DeltaId" (mkDeltaId (fixtureIdentifierBytes (identity 4)))
      neutral seed =
        NeutralVertex
          (checked "neutral GlobalObjectId" (mkGlobalObjectId (fixtureIdentifierBytes (identity seed))))
      weakHop = neutral 5
      preservingHop = neutral 6
      cycleLeft = neutral 7
      cycleRight = neutral 8
      vertices =
        [ NablaVertex source,
          weakHop,
          preservingHop,
          cycleLeft,
          cycleRight,
          DeltaVertex weakDelta,
          DeltaVertex strongestDelta,
          DeltaVertex unreachableDelta
        ]
      baseEdges =
        [ edgePayload (NablaVertex source) weakHop Weaken,
          edgePayload weakHop (DeltaVertex weakDelta) Preserve,
          edgePayload weakHop (DeltaVertex strongestDelta) Preserve,
          edgePayload (NablaVertex source) preservingHop Preserve,
          edgePayload preservingHop (DeltaVertex strongestDelta) Preserve,
          edgePayload (NablaVertex source) cycleLeft Preserve,
          edgePayload cycleLeft cycleRight Preserve,
          edgePayload cycleRight cycleLeft Weaken
        ]
      optionalPairs =
        [ (preservingHop, cycleLeft),
          (cycleRight, preservingHop),
          (cycleLeft, DeltaVertex strongestDelta),
          (weakHop, cycleLeft),
          (cycleRight, DeltaVertex strongestDelta)
        ]
  optional <- sublistOf optionalPairs
  optionalEdges <- traverse randomEdge optional
  presentedEdges <- shuffle (baseEdges <> optionalEdges)
  pure (source, vertices, presentedEdges)
  where
    randomEdge (from, to) = edgePayload from to <$> elements [Preserve, Weaken]

-- This reference uses two ordinary set closures rather than the production
-- work-list lattice: all-edge closure establishes reachability and a separate
-- preserve-only closure determines whether the strongest path stays normal.
referenceReachableDeltas ::
  NablaId ->
  [VertexId] ->
  [EdgePayload] ->
  [(DeltaId, ReplicaStrength)]
referenceReachableDeltas source vertices suppliedEdges
  | Set.notMember origin vertexSet = []
  | otherwise =
      [ (delta, if Set.member vertex preserving then Normal else Weak)
      | vertex@(DeltaVertex delta) <- Set.toAscList reachable
      ]
  where
    origin = NablaVertex source
    vertexSet = Set.fromList vertices
    edges =
      [ edge
      | edge <- suppliedEdges,
        Set.member (edgeSource edge) vertexSet,
        Set.member (edgeDestination edge) vertexSet
      ]
    reachable = closure edges origin
    preserving = closure (filter ((== Preserve) . edgeStrength) edges) origin

closure :: [EdgePayload] -> VertexId -> Set VertexId
closure edges origin = go (Set.singleton origin)
  where
    adjacency =
      Map.fromListWith
        Set.union
        [ (edgeSource edge, Set.singleton (edgeDestination edge))
        | edge <- edges
        ]
    go reached =
      let next =
            Set.unions
              [ Map.findWithDefault Set.empty vertex adjacency
              | vertex <- Set.toAscList reached
              ]
          successor = Set.union reached next
       in if successor == reached then reached else go successor

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
