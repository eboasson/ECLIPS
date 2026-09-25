module PreparedProperties
  ( tests,
  )
where

import Data.Word (Word16)
import Eclips.Herald.Internal.Prepared
  ( commitPrepared,
    prepareTransition,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Property,
    counterexample,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "prepared transition"
    [ testCase "failure exposes no successor" casePreparedFailure,
      testProperty "commit reveals the complete checked result" propPreparedCommit,
      testProperty "a checked preparation preserves its invariant" propPreparedInvariant
    ]

casePreparedFailure :: IO ()
casePreparedFailure =
  case prepareTransition rejects (41 :: Int) of
    Left problem -> assertEqual "failure" "rejected" problem
    Right _ -> assertFailure "failed preparation exposed a successor"
  where
    rejects :: Int -> Either String (Int, Int)
    rejects _ = Left "rejected"

propPreparedCommit :: Int -> Property
propPreparedCommit predecessor =
  case prepareTransition succeeds predecessor of
    Left problem -> counterexample problem False
    Right plan -> commitPrepared plan === succeedsResult predecessor
  where
    succeeds state = Right (succeedsResult state)
    succeedsResult state = (state + 1, state * 2)

propPreparedInvariant :: Word16 -> Property
propPreparedInvariant predecessor =
  case prepareTransition checkedStep (fromIntegral predecessor) of
    Left problem -> counterexample problem False
    Right plan ->
      let (successor, ()) = commitPrepared plan
       in assertInvariant successor
  where
    checkedStep :: Int -> Either String (Int, ())
    checkedStep state
      | state >= 0 = Right (state + 1, ())
      | otherwise = Left "negative predecessor"
    assertInvariant :: Int -> Property
    assertInvariant successor =
      counterexample ("invalid successor: " <> show successor) (successor > 0)
