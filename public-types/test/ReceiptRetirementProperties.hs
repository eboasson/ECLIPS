module ReceiptRetirementProperties (tests) where

import Data.Binary qualified as Binary
import Data.ByteString.Lazy qualified as Lazy
import Data.Either (isLeft)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Public.Types.ReceiptRetirement
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (Gen, arbitrary, choose, conjoin, forAll, sublistOf, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "receipt release algebra"
    [ testProperty "join obeys semilattice and monoid laws with holes"
        $ forAll progress
        $ \a -> forAll progress $ \b -> forAll progress $ \c ->
          conjoin [(a <> b) <> c === a <> (b <> c), a <> b === b <> a, a <> a === a, a <> mempty === a, mempty <> a === a],
      testProperty "join is exactly the union of released identities"
        $ forAll progress
        $ \a -> forAll progress $ \b -> forAll (choose (0, 90)) $ \request ->
          receiptIsRetired request (a <> b) === (receiptIsRetired request a || receiptIsRetired request b),
      testProperty "canonical Binary preserves both water mark and exceptions"
        $ forAll progress
        $ \value -> decode (Binary.encode value) === Right value,
      testProperty "repeating a released exception never reopens it"
        $ forAll (choose (0, 80))
        $ \high -> forAll (choose (0, high)) $ \request ->
          let released = receiptRetirementPrefix (Just high)
              stale = checked (receiptRetirement (Just high) (Set.singleton request))
           in receiptIsRetired request (released <> stale) === True,
      testCase "zero is a live exception until specifically released" $ do
        let pending = checked (receiptRetirement (Just 0) (Set.singleton 0))
        receiptIsRetired 0 pending @?= False
        receiptIsRetired 0 (pending <> receiptRetirementPrefix (Just 0)) @?= True,
      testCase "constructors and Binary reject invalid or noncanonical exceptions" $ do
        receiptRetirement Nothing (Set.singleton 0) @?= Left ReceiptRetirementExceptionAboveHighWater
        receiptRetirement (Just 3) (Set.singleton 4) @?= Left ReceiptRetirementExceptionAboveHighWater
        mapM_
          (assertBool "invalid encoding" . isLeft . decode . Binary.encode)
          [(Nothing, [0]), (Just 3, [4]), (Just 3, [1, 1]), (Just 3, [2, 1]) :: (Maybe Word64, [Word64])],
      testCase "Binary rejects every noncanonical optional high-water tag" $ do
        let values =
              [ receiptRetirementPrefix (Just 0),
                checked (receiptRetirement (Just 0) (Set.singleton 0)),
                checked (receiptRetirement (Just 3) (Set.singleton 1))
              ]
        mapM_
          (\value -> mapM_ (\tag -> assertBool "noncanonical optional tag" (isLeft (decode (Lazy.cons tag (Lazy.drop 1 (Binary.encode value)))))) [2 .. 255])
          values
    ]

progress :: Gen ReceiptRetirement
progress = do
  hasHigh <- arbitrary
  if not hasHigh
    then pure mempty
    else do
      high <- choose (0, 80)
      exceptions <- sublistOf [0 .. high]
      pure (checked (receiptRetirement (Just high) (Set.fromList exceptions)))

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

decode :: Lazy.ByteString -> Either String ReceiptRetirement
decode bytes = case Binary.decodeOrFail bytes of
  Left (_, _, problem) -> Left problem
  Right (remaining, _, value) | Lazy.null remaining -> Right value
  Right _ -> Left "trailing bytes"
