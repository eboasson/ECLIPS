module LifetimeProperties (tests) where

import Data.Binary qualified as Binary
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Lifetime
import Eclips.Public.Types.ReceiptRetirement qualified as Retirement
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck (conjoin, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "receipt retirement frontiers"
    [ testProperty "join is associative, commutative, idempotent, and has an identity" $ \a b c d e f ->
        let first = applicationReceiptRetirement a b
            second = applicationReceiptRetirement c d
            third = applicationReceiptRetirement e f
         in conjoin
              [ (first <> second) <> third === first <> (second <> third),
                first <> second === second <> first,
                first <> first === first,
                first <> mempty === first,
                mempty <> first === first
              ],
      testProperty "independent inclusive frontiers survive binary encoding" $ \ordinary lifecycle ->
        let progress = applicationReceiptRetirement (ordinary :: Maybe Word64) lifecycle
         in Binary.decode (Binary.encode progress) === progress,
      testCase "request zero differs from an empty frontier" $ do
        let progress = applicationReceiptRetirement (Just 0) Nothing
        assertEqual "ordinary zero" (Just 0) (ordinaryReceiptRetirementThrough progress)
        assertEqual "lifecycle has no consumed request" Nothing (lifecycleReceiptRetirementThrough progress)
        assertEqual "empty is neutral" progress (emptyApplicationReceiptRetirement <> progress),
      testProperty "two hole-bearing namespaces round trip and release independently" $ \a b ->
        let highA = a :: Word64
            highB = b :: Word64
            first = either (error . show) id (Retirement.receiptRetirement (Just highA) (Set.singleton highA))
            second = either (error . show) id (Retirement.receiptRetirement (Just highB) (Set.singleton highB))
            progress = applicationReceiptRetirementWithExceptions first second
            ordinaryReleased = progress <> applicationReceiptRetirement (Just highA) Nothing
         in conjoin
              [ Binary.decode (Binary.encode progress) === progress,
                Retirement.receiptIsRetired highA (ordinaryReceiptRetirement ordinaryReleased) === True,
                Retirement.receiptIsRetired highB (lifecycleReceiptRetirement ordinaryReleased) === False
              ]
    ]
