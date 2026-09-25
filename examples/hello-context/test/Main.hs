module Main (main) where

import ContextVocabulary (contextEdges)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (NonEmptyList (..), Property, conjoin, counterexample, testProperty, (===))

main :: IO ()
main =
  defaultMain
    $ testGroup
      "greeting context topology"
      [ testProperty "every holder can reach every other holder" propStronglyConnected,
        testProperty "context edges refer only to the holders" propContextOnly
      ]

-- Duplicate generated values designate the same vertex; normalize them before
-- checking the graph property so every generated holder has a distinct identity.
holders :: NonEmptyList Int -> NonEmpty Int
holders (NonEmpty values) = case Set.toList (Set.fromList values) of
  first : rest -> first :| rest
  [] -> error "QuickCheck NonEmptyList produced an empty list"

propStronglyConnected :: NonEmptyList Int -> Property
propStronglyConnected generated =
  let vertices = holders generated
      edges = contextEdges vertices
      expected = Set.fromList (NonEmpty.toList vertices)
   in conjoin
        [ counterexample ("unreachable context from " <> show source <> " in " <> show edges)
            $ reachable edges source === expected
        | source <- NonEmpty.toList vertices
        ]

propContextOnly :: NonEmptyList Int -> Property
propContextOnly generated =
  let vertices = holders generated
      expected = Set.fromList (NonEmpty.toList vertices)
      edges = contextEdges vertices
   in conjoin
        [ Set.fromList (map fst edges) === expected,
          Set.fromList (map snd edges) === expected
        ]

reachable :: [(Int, Int)] -> Int -> Set.Set Int
reachable edges source = visit Set.empty [source]
  where
    visit seen [] = seen
    visit seen (current : pending)
      | Set.member current seen = visit seen pending
      | otherwise = visit (Set.insert current seen) ([target | (origin, target) <- edges, origin == current] <> pending)
