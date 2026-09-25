{-# LANGUAGE ImportQualifiedPost #-}

module PreparationWorkIndexProperties (tests) where

import Data.List (scanl')
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Herald.Internal.WorkIndex qualified as Work
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "preparation ordered dependency index"
    [ QC.testProperty "mutation histories agree with independent map scans" propWorkIndex,
      testCase "entry membership and exclusive cursor preserve forward and deferred wakes" caseCursor
    ]

data Mutation = Register Int [Int] | Replace Int [Int] | Remove Int | Consume Int | Notify [Int]
  deriving stock (Show)

mutation :: QC.Gen Mutation
mutation =
  QC.oneof
    [ Register <$> key <*> dependencies,
      Replace <$> key <*> dependencies,
      Remove <$> key,
      Consume <$> key,
      Notify <$> dependencies
    ]
  where
    key = QC.chooseInt (0, 8)
    dependencies = QC.listOf (QC.chooseInt (0, 8))

propWorkIndex :: QC.Property
propWorkIndex = QC.forAll (QC.vectorOf 70 mutation) $ \schedule ->
  QC.conjoin
    [ QC.counterexample
        ("work history " <> show position)
        ( QC.conjoin
            ( [ QC.property (Work.valid index),
                Work.registeredKeys index QC.=== Map.keysSet forward,
                Work.pendingKeys index QC.=== pending
              ]
                <> [Work.dependenciesFor key index QC.=== Map.lookup key forward | key <- [0 .. 8]]
                <> [fst (Work.notifyDependencies (Set.singleton dependency) index) QC.=== matching (Set.singleton dependency) forward | dependency <- [0 .. 8]]
            )
        )
    | (position, (index, forward, pending)) <- zip [0 :: Int ..] (scanl' apply (Work.empty, Map.empty, Set.empty) schedule)
    ]
  where
    matching dependencies forward = Map.keysSet (Map.filter (not . Set.disjoint dependencies) forward)
    apply (index, forward, pending) change = case change of
      Register key supplied ->
        let dependencies = Set.fromList supplied
         in (Work.registerWork key dependencies index, Map.insert key dependencies forward, Set.insert key pending)
      Replace key supplied ->
        let dependencies = Set.fromList supplied
         in (Work.replaceDependencies key dependencies index, Map.adjust (const dependencies) key forward, pending)
      Remove key -> (Work.removeWork key index, Map.delete key forward, Set.delete key pending)
      Consume key -> (Work.consumeWork key index, forward, Set.delete key pending)
      Notify supplied ->
        let dependencies = Set.fromList supplied
         in (snd (Work.notifyDependencies dependencies index), forward, pending <> matching dependencies forward)

caseCursor :: Assertion
caseCursor = do
  let initial = Work.registerWork (3 :: Int) (Set.singleton (30 :: Int)) (Work.registerWork 1 (Set.singleton 10) Work.empty)
      entry = Work.consumeWork 3 initial
      inventory = Work.registeredKeys entry
      consumed = Work.consumeWork 1 entry
      changed = Work.registerWork 2 (Set.singleton 20) (snd (Work.notifyDependencies (Set.fromList [10, 30]) consumed))
      afterLater = Work.consumeWork 3 changed
      replaced = Work.replaceDependencies 1 (Set.singleton 40) (Work.clearPendingWork afterLater)
      oldNotification = snd (Work.notifyDependencies (Set.singleton 10) replaced)
      newNotification = snd (Work.notifyDependencies (Set.singleton 40) oldNotification)
      removed = Work.removeWork 1 newNotification
  assertEqual "first work belongs to frozen inventory" (Just 1) (Work.nextWork inventory Nothing entry)
  assertEqual "existing later work wakes in this pass" (Just 3) (Work.nextWork inventory (Just 1) changed)
  assertEqual "new and earlier keys wait for next pass" Nothing (Work.nextWork inventory (Just 3) afterLater)
  assertEqual "deferred keys remain dirty" (Set.fromList [1, 2]) (Work.pendingKeys afterLater)
  assertEqual "obsolete dependency membership is removed" Set.empty (Work.pendingKeys oldNotification)
  assertEqual "replacement dependency can wake the work" (Set.singleton 1) (Work.pendingKeys newNotification)
  assertEqual "removed work cannot be resurrected" Set.empty (Work.pendingKeys (snd (Work.notifyDependencies (Set.singleton 40) removed)))
  assertEqual "registration and all inverse buckets remain valid" True (Work.valid removed)
