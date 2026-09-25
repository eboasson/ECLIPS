module ArbiterProperties
  ( tests,
  )
where

import Control.Concurrent.STM (atomically)
import Control.Monad (foldM)
import Data.List (nub)
import Data.Word (Word64)
import Eclips.Herald.Runtime.Internal.Arbiter
  ( Arbiter,
    ArbiterModel,
    ArbiterSelection (..),
    ReadyRing,
    SourceFamily (..),
    SourceId (..),
    emptyArbiterModel,
    emptyReadyRing,
    enqueueArbiter,
    enqueueModel,
    insertReady,
    newArbiter,
    pauseArbiterSource,
    pauseSourceModel,
    readyRingMembers,
    removeArbiterSource,
    removeReady,
    removeSourceModel,
    resumeArbiterSource,
    resumeSourceModel,
    selectArbiter,
    selectModel,
    selectReady,
    snapshotArbiter,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    counterexample,
    elements,
    forAll,
    ioProperty,
    listOf1,
    property,
    testProperty,
    vectorOf,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "fair ingress arbiter"
    [ testGroup
        "ready ring"
        [ testProperty "insertion is unique and preserves first insertion order" propReadyInsertion,
          testProperty "removal affects exactly the named member" propReadyRemoval,
          testCase "selection takes the cursor head without hidden rotation" caseReadySelection
        ],
      testGroup
        "nested fairness"
        [ testCase "families and sources cycle independently" caseNestedCycle,
          testCase "a replenished hot source cannot starve a cold source" caseHotCannotStarveCold,
          testCase "pause retains FIFO and resume restores eligibility" casePauseResume,
          testCase "removal returns only the removed source FIFO" caseRemoval,
          testProperty "every source preserves FIFO under generated interleavings" propGeneratedFifo
        ],
      testProperty
        "STM refinement agrees with the pure model after every generated operation"
        propStmRefinesModel
    ]

propReadyInsertion :: [Int] -> Property
propReadyInsertion values =
  readyRingMembers (foldl (flip insertReady) emptyReadyRing values)
    === nub values

propReadyRemoval :: [Int] -> Int -> Property
propReadyRemoval values removed =
  readyRingMembers (removeReady removed ring)
    === filter (/= removed) (nub values)
  where
    ring = foldl (flip insertReady) emptyReadyRing values

caseReadySelection :: Assertion
caseReadySelection = do
  let ring = foldl (flip insertReady) emptyReadyRing [7 :: Int, 8, 9]
  assertEqual
    "the head and exact remainder are returned"
    (Just (7, readyRing [8, 9]))
    (selectReady ring)

caseNestedCycle :: Assertion
caseNestedCycle = do
  let applicationA = source ApplicationSources 1
      applicationB = source ApplicationSources 2
      peerA = source PeerSources 1
      model =
        enqueueMany
          [ (applicationA, 11 :: Int),
            (applicationA, 12),
            (applicationB, 21),
            (applicationB, 22),
            (peerA, 31),
            (peerA, 32),
            (peerA, 33),
            (peerA, 34)
          ]
          emptyArbiterModel
      (selected, _) = drainModel model
  assertEqual
    "family visits alternate while application visits rotate sources"
    [ (applicationA, 11),
      (peerA, 31),
      (applicationB, 21),
      (peerA, 32),
      (applicationA, 12),
      (peerA, 33),
      (applicationB, 22),
      (peerA, 34)
    ]
    selected

caseHotCannotStarveCold :: Assertion
caseHotCannotStarveCold = do
  let hot = source ApplicationSources 1
      cold = source ApplicationSources 2
      model = enqueueModel cold (999 :: Int) (enqueueModel hot 0 emptyArbiterModel)
      (firstSelection, afterFirst) = requireSelection model
      replenished = enqueueModel hot 1 afterFirst
      (secondSelection, _) = requireSelection replenished
  assertEqual "hot is initially selected" (hot, 0) (selectionPair firstSelection)
  assertEqual
    "tail re-insertion leaves the already waiting cold source next"
    (cold, 999)
    (selectionPair secondSelection)

casePauseResume :: Assertion
casePauseResume = do
  let paused = source PeerSources 9
      other = source RuntimeCompletionSources 3
      initial = enqueueMany [(paused, 1 :: Int), (paused, 2), (other, 3)] emptyArbiterModel
      whilePaused = enqueueModel paused 4 (pauseSourceModel paused initial)
      (beforeResume, selectedOther) = requireSelection whilePaused
  assertEqual "another family remains eligible" (other, 3) (selectionPair beforeResume)
  assertEqual "only the paused FIFO remains" Nothing (selectModel selectedOther)
  let (afterResume, successor) = requireSelection (resumeSourceModel paused selectedOther)
      (second, _) = requireSelection successor
  assertEqual "resume exposes the oldest retained member" (paused, 1) (selectionPair afterResume)
  assertEqual "the retained FIFO remains ordered" (paused, 2) (selectionPair second)

caseRemoval :: Assertion
caseRemoval = do
  let removed = source AdministrationSources 4
      retained = source PeerSources 7
      model = enqueueMany [(removed, 1 :: Int), (retained, 2), (removed, 3)] emptyArbiterModel
      (discarded, successor) = removeSourceModel removed model
  assertEqual "only the removed source is discarded" [1, 3] discarded
  assertEqual "the unrelated source is unchanged" [(retained, 2)] (fst (drainModel successor))

propGeneratedFifo :: Property
propGeneratedFifo =
  forAll generatedEnqueues $ \entries ->
    let model = enqueueMany entries emptyArbiterModel
        (selected, _) = drainModel model
        sources = nub (fmap fst entries)
        expectedFor sourceId = [value | (candidate, value) <- entries, candidate == sourceId]
        actualFor sourceId = [value | (candidate, value) <- selected, candidate == sourceId]
     in counterexample ("selection: " <> show selected)
          $ property (all (\sourceId -> actualFor sourceId == expectedFor sourceId) sources)

propStmRefinesModel :: Property
propStmRefinesModel =
  forAll (listOf1 genCommand) $ \commands ->
    ioProperty $ do
      arbiter <- atomically newArbiter
      outcome <- foldM (applyAndCompare arbiter) (Right (emptyArbiterModel :: ArbiterModel Int)) commands
      pure $ case outcome of
        Left mismatch -> counterexample mismatch False
        Right _ -> property True

data Command
  = Enqueue SourceId Int
  | Pause SourceId
  | Resume SourceId
  | Remove SourceId
  | Select
  deriving stock (Show)

genCommand :: Gen Command
genCommand = do
  tag <- chooseInt (0, 9)
  sourceId <- genSource
  value <- chooseInt (0, 1000)
  pure $ case tag of
    0 -> Pause sourceId
    1 -> Resume sourceId
    2 -> Remove sourceId
    3 -> Select
    _ -> Enqueue sourceId value

generatedEnqueues :: Gen [(SourceId, Int)]
generatedEnqueues = do
  count <- chooseInt (1, 80)
  vectorOf count ((,) <$> genSource <*> chooseInt (0, 10000))

genSource :: Gen SourceId
genSource =
  SourceId
    <$> elements [minBound .. maxBound]
    <*> (fromIntegral <$> chooseInt (0, 5))

applyAndCompare ::
  Arbiter Int ->
  Either String (ArbiterModel Int) ->
  Command ->
  IO (Either String (ArbiterModel Int))
applyAndCompare _ mismatch@(Left _) _ = pure mismatch
applyAndCompare arbiter (Right model) command = do
  expected <- case command of
    Enqueue sourceId value -> do
      atomically (enqueueArbiter arbiter sourceId value)
      pure (Nothing, enqueueModel sourceId value model)
    Pause sourceId -> do
      atomically (pauseArbiterSource arbiter sourceId)
      pure (Nothing, pauseSourceModel sourceId model)
    Resume sourceId -> do
      atomically (resumeArbiterSource arbiter sourceId)
      pure (Nothing, resumeSourceModel sourceId model)
    Remove sourceId -> do
      actualDiscarded <- atomically (removeArbiterSource arbiter sourceId)
      let (expectedDiscarded, successor) = removeSourceModel sourceId model
      pure (Just (show expectedDiscarded, show actualDiscarded), successor)
    Select -> case selectModel model of
      Nothing -> pure (Nothing, model)
      Just (expectedSelection, successor) -> do
        actualSelection <- atomically (selectArbiter arbiter)
        pure (Just (show expectedSelection, show actualSelection), successor)
  actualModel <- atomically (snapshotArbiter arbiter)
  let (operationResult, expectedModel) = expected
      resultMismatch = case operationResult of
        Just (expectedResult, actualResult)
          | expectedResult /= actualResult ->
              Just
                ( "operation result differs after "
                    <> show command
                    <> ": expected "
                    <> expectedResult
                    <> ", got "
                    <> actualResult
                )
        _ -> Nothing
  pure $ case resultMismatch of
    Just mismatch -> Left mismatch
    Nothing
      | actualModel /= expectedModel ->
          Left
            ( "model differs after "
                <> show command
                <> ": expected "
                <> show expectedModel
                <> ", got "
                <> show actualModel
            )
      | otherwise -> Right expectedModel

enqueueMany :: [(SourceId, value)] -> ArbiterModel value -> ArbiterModel value
enqueueMany entries initial =
  foldl (\model (sourceId, value) -> enqueueModel sourceId value model) initial entries

drainModel :: ArbiterModel value -> ([(SourceId, value)], ArbiterModel value)
drainModel model = case selectModel model of
  Nothing -> ([], model)
  Just (selection, successor) ->
    let (remaining, finalModel) = drainModel successor
     in (selectionPair selection : remaining, finalModel)

requireSelection :: ArbiterModel value -> (ArbiterSelection value, ArbiterModel value)
requireSelection model = case selectModel model of
  Just selected -> selected
  Nothing -> error "test fixture unexpectedly has no ready source"

selectionPair :: ArbiterSelection value -> (SourceId, value)
selectionPair (ArbiterSelection sourceId value) = (sourceId, value)

source :: SourceFamily -> Word64 -> SourceId
source = SourceId

readyRing :: (Eq value) => [value] -> ReadyRing value
readyRing = foldl (flip insertReady) emptyReadyRing
