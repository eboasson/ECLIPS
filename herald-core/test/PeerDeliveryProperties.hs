module PeerDeliveryProperties
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment
  ( AlignmentDeliverySequence,
    alignmentDeliverySequence,
    alignmentDeliverySequenceWord64,
  )
import Eclips.Herald.PeerDelivery.State qualified as Delivery
import Eclips.Public.Types.ReceiptRetirement
  ( ReceiptRetirement,
    receiptRetirement,
    receiptRetirementPrefix,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    chooseInt,
    conjoin,
    counterexample,
    forAll,
    shuffle,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "alignment evidence delivery"
    [ testProperty "reordered duplicate receipt retains only missing positions" propReceivePermutation,
      testProperty "reordered sparse acknowledgements release exactly their payloads" propAcknowledgementPermutation,
      testProperty "a pending head and a long completed tail retain one record" propPendingHeadBounded,
      testCase "an outstanding key has one immutable delivery position" casePendingKey,
      testCase "confirmed keys are forgotten and a later offer is new work" caseConfirmedKey,
      testCase "acknowledgements cannot name zero or unallocated positions" caseInvalidAcknowledgements,
      testCase "reconnect replays only the exact outstanding evidence" caseReconnect
    ]

type DeliveryState = Delivery.State Int Int

propReceivePermutation :: Property
propReceivePermutation =
  forAll (chooseInt (0, 150)) $ \count ->
    forAll (shuffle (concatMap (replicate 2) [1 .. count])) $ \positions ->
      let (final, fresh) = foldl' receiveOne (Delivery.emptyState :: DeliveryState, 0 :: Int) positions
          receiveOne (state, total) position =
            let (successor, isFresh) = Delivery.receive (sequenceAt position) state
             in (successor, total + if isFresh then 1 else 0)
       in conjoin
            [ fresh === count,
              Delivery.receiptProgress final === receiptRetirementPrefix (if count == 0 then Nothing else Just (fromIntegral count)),
              Delivery.retainedCounts final === (0, 0, 0),
              Delivery.outstanding final === []
            ]

propAcknowledgementPermutation :: Property
propAcknowledgementPermutation =
  forAll (chooseInt (1, 100)) $ \count ->
    forAll (shuffle (concatMap (replicate 2) [1 .. count])) $ \positions ->
      let acknowledgeOne (state, observed, exactSoFar) position = do
            successor <- Delivery.acknowledge (onlyPosition position) state
            let updated = Set.insert position observed
                actual = map (fromIntegral . alignmentDeliverySequenceWord64 . fst) (Delivery.outstanding successor)
                expected = filter (`Set.notMember` updated) [1 .. count]
            Right (successor, updated, exactSoFar && actual == expected)
       in case allocate count >>= \initial -> foldM acknowledgeOne (initial, Set.empty, True) positions of
            Left problem -> counterexample (show problem) False
            Right (final, acknowledged, exactAtEveryStep) ->
              conjoin
                [ counterexample "each receipt releases only its certified position" exactAtEveryStep,
                  acknowledged === Set.fromList [1 .. count],
                  Delivery.retainedCounts final === (0, 0, 0),
                  Delivery.outstanding final === []
                ]

propPendingHeadBounded :: Property
propPendingHeadBounded =
  forAll (chooseInt (100, 1500)) $ \tailLength ->
    case Delivery.offer 1 10 (Delivery.emptyState :: DeliveryState) >>= \(initial, _) -> foldM advance (initial, Delivery.emptyState :: DeliveryState, True) [2 .. tailLength + 1] of
      Left problem -> counterexample (show problem) False
      Right (sender, receiver, boundedAtEveryStep) ->
        let (closedReceiver, fresh) = Delivery.receive (sequenceAt 1) receiver
         in case Delivery.acknowledge (Delivery.receiptProgress closedReceiver) sender of
              Left problem -> counterexample (show problem) False
              Right closedSender ->
                conjoin
                  [ counterexample "only the missing head remains throughout the tail" boundedAtEveryStep,
                    Delivery.outstanding sender === [(sequenceAt 1, 10)],
                    Delivery.retainedCounts receiver === (0, 0, 1),
                    fresh === True,
                    Delivery.retainedCounts closedSender === (0, 0, 0),
                    Delivery.retainedCounts closedReceiver === (0, 0, 0)
                  ]
  where
    advance (sender, receiver, boundedSoFar) position = do
      (offered, sequenceNumber) <- Delivery.offer position (position * 10) sender
      let (received, fresh) = Delivery.receive sequenceNumber receiver
      confirmed <- Delivery.acknowledge (Delivery.receiptProgress received) offered
      Right
        ( confirmed,
          received,
          boundedSoFar
            && fresh
            && Delivery.retainedCounts confirmed == (1, 1, 0)
            && Delivery.retainedCounts received == (0, 0, 1)
        )

casePendingKey :: IO ()
casePendingKey = do
  (initial, position) <- checked (Delivery.offer 7 70 (Delivery.emptyState :: DeliveryState))
  assertEqual "exact repeated offer reuses the position" (Right (initial, position)) (Delivery.offer 7 70 initial)
  assertEqual "same pending key cannot change payload" (Left Delivery.DeliveryConflictingPendingKey) (Delivery.offer 7 71 initial)
  assertEqual "only one outstanding record and key" (1, 1, 0) (Delivery.retainedCounts initial)

caseConfirmedKey :: IO ()
caseConfirmedKey = do
  (initial, first) <- checked (Delivery.offer 7 70 (Delivery.emptyState :: DeliveryState))
  confirmed <- checked (Delivery.acknowledge (receiptRetirementPrefix (Just 1)) initial)
  assertEqual "completed key has no retained history" (0, 0, 0) (Delivery.retainedCounts confirmed)
  (offeredAgain, second) <- checked (Delivery.offer 7 71 confirmed)
  assertEqual "new work gets a later sequence" 2 (alignmentDeliverySequenceWord64 second)
  afterStale <- checked (Delivery.acknowledge (receiptRetirementPrefix (Just (alignmentDeliverySequenceWord64 first))) offeredAgain)
  assertEqual "old receipt cannot release a reused semantic key" [(second, 71)] (Delivery.outstanding afterStale)

caseInvalidAcknowledgements :: IO ()
caseInvalidAcknowledgements = do
  let empty = Delivery.emptyState :: DeliveryState
  assertEqual "positive high water without allocation" (Left Delivery.DeliveryAcknowledgementBeyondAllocation) (Delivery.acknowledge (receiptRetirementPrefix (Just 1)) empty)
  initial <- checked (allocate 2)
  assertEqual "high water beyond allocation" (Left Delivery.DeliveryAcknowledgementBeyondAllocation) (Delivery.acknowledge (receiptRetirementPrefix (Just 3)) initial)
  assertEqual "zero is not an explicit empty prefix" (Left Delivery.DeliveryInvalidAcknowledgement) (Delivery.acknowledge (receiptRetirementPrefix (Just 0)) initial)
  assertEqual "zero cannot be a pending position" (Left Delivery.DeliveryInvalidAcknowledgement) (Delivery.acknowledge (progress (Just 2) [0]) initial)
  assertEqual "empty progress is an identity" (Right initial) (Delivery.acknowledge (receiptRetirementPrefix Nothing) initial)

caseReconnect :: IO ()
caseReconnect = do
  sender <- checked (allocate 4)
  let (firstReceived, _) = Delivery.receive (sequenceAt 1) (Delivery.emptyState :: DeliveryState)
      (receiver, _) = Delivery.receive (sequenceAt 3) firstReceived
  awaiting <- checked (Delivery.acknowledge (Delivery.receiptProgress receiver) sender)
  assertEqual "repair retains only unconfirmed payloads in sequence order" [(sequenceAt 2, 20), (sequenceAt 4, 40)] (Delivery.outstanding awaiting)
  let (repaired, freshCount) =
        foldl'
          (\(state, count) (position, _) -> let (next, fresh) = Delivery.receive position state in (next, count + if fresh then 1 else 0))
          (receiver, 0 :: Int)
          (Delivery.outstanding awaiting)
  assertEqual "reconnect repair admits each missing payload once" 2 freshCount
  assertEqual "an old binding replay has no receive effect" (repaired, False) (Delivery.receive (sequenceAt 3) repaired)
  complete <- checked (Delivery.acknowledge (Delivery.receiptProgress repaired) awaiting)
  assertEqual "no reconnect work after cumulative confirmation" [] (Delivery.outstanding complete)
  stale <- checked (Delivery.acknowledge (Delivery.receiptProgress receiver) complete)
  assertEqual "stale sparse receipt cannot recreate payloads" complete stale

allocate :: Int -> Either Delivery.DeliveryProblem DeliveryState
allocate count = foldM (\state key -> fst <$> Delivery.offer key (key * 10) state) Delivery.emptyState [1 .. count]

sequenceAt :: Int -> AlignmentDeliverySequence
sequenceAt = either (error . show) id . alignmentDeliverySequence . fromIntegral

onlyPosition :: Int -> ReceiptRetirement
onlyPosition position = progress (Just (fromIntegral position)) [1 .. fromIntegral position - 1]

progress :: Maybe Word64 -> [Word64] -> ReceiptRetirement
progress high = either (error . show) id . receiptRetirement high . Set.fromList

checked :: (Show problem) => Either problem result -> IO result
checked = either (fail . show) pure
