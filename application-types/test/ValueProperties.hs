{-# LANGUAGE OverloadedStrings #-}

module ValueProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Eclips.Application.Types.Identity
  ( PrivateUniqueId,
    asPrivateProcessId,
    mkPrivateUniqueId,
  )
import Eclips.Application.Types.Value
  ( ApplicationLabelOwner (..),
    ApplicationValue (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    arbitrary,
    elements,
    forAll,
    frequency,
    listOf,
    oneof,
    resize,
    sized,
    sublistOf,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "application value"
    [ testCase "the ordinary tree preserves every constructor" caseOrdinaryTree,
      testProperty "generated recursive ordinary trees preserve structure" propRecursiveOrdinaryTree,
      testProperty "record field text is not admitted in the carrier" propFieldTextIsOrdinary
    ]

caseOrdinaryTree :: IO ()
caseOrdinaryTree = do
  let privateIdentity = checkedPrivateUniqueId 7
      privateProcess = asPrivateProcessId privateIdentity
      fields =
        Map.fromList
          [ ("bool", BoolValue True),
            ("bytes", BytesValue (ByteString.pack [0, 1, 2])),
            ("enum", EnumValue "ordinary"),
            ("identity", UniqueIdValue privateIdentity),
            ("optional_none", OptionalUniqueIdValue Nothing),
            ("optional_some", OptionalUniqueIdValue (Just privateIdentity)),
            ("int", Int64Value (-9)),
            ("process", LabelValue (ProcessLabel privateProcess, 7)),
            ("text", TextValue "ordinary"),
            ("void", LabelValue (VoidLabel, 0)),
            ("zombie", LabelValue (ZombieLabel privateProcess, 0))
          ]
      value =
        RecordValue fields
  assertEqual "the record retains every ordinary field" (Just fields) (recordFields value)
  assertBool
    "process and zombie labels remain distinct"
    (LabelValue (ProcessLabel privateProcess, 7) /= LabelValue (ZombieLabel privateProcess, 7))

propFieldTextIsOrdinary :: String -> Property
propFieldTextIsOrdinary generated =
  let name = Text.pack generated
      value = BoolValue True
      record = RecordValue (Map.singleton name value)
   in recordFields record === Just (Map.singleton name value)

propRecursiveOrdinaryTree :: Property
propRecursiveOrdinaryTree =
  forAll (genOrdinaryValue privateIdentity) $ \value ->
    rebuildOrdinaryValue value === value
  where
    privateIdentity = checkedPrivateUniqueId 7

genOrdinaryValue :: PrivateUniqueId -> Gen ApplicationValue
genOrdinaryValue privateIdentity = sized generate
  where
    generate remaining
      | remaining <= 0 = scalar
      | otherwise =
          frequency
            [ (5, scalar),
              (2, record (remaining `div` 2))
            ]
    scalar =
      oneof
        [ BoolValue <$> arbitrary,
          Int64Value <$> (arbitrary :: Gen Int64),
          BytesValue . ByteString.pack <$> resize 8 (listOf arbitrary),
          TextValue . Text.pack <$> resize 8 (listOf (elements ['a' .. 'z'])),
          pure (UniqueIdValue privateIdentity),
          OptionalUniqueIdValue
            <$> elements [Nothing, Just privateIdentity],
          LabelValue
            <$> ((,) <$> elements [VoidLabel, ProcessLabel privateProcess, ZombieLabel privateProcess] <*> arbitrary),
          EnumValue . Text.pack <$> resize 8 (listOf (elements ['a' .. 'z']))
        ]
    record remaining = do
      names <- sublistOf ["alpha", "beta", "gamma", "nested"]
      values <- traverse (const (generate remaining)) names
      pure (RecordValue (Map.fromList (zip names values)))
    privateProcess = asPrivateProcessId privateIdentity

rebuildOrdinaryValue :: ApplicationValue -> ApplicationValue
rebuildOrdinaryValue value = case value of
  BoolValue scalar -> BoolValue scalar
  Int64Value scalar -> Int64Value scalar
  BytesValue scalar -> BytesValue scalar
  TextValue scalar -> TextValue scalar
  UniqueIdValue identifier -> UniqueIdValue identifier
  LabelValue label -> LabelValue label
  RecordValue fields -> RecordValue (fmap rebuildOrdinaryValue fields)
  SortDefinitionValue definition -> SortDefinitionValue definition
  EnumValue symbol -> EnumValue symbol
  OptionalUniqueIdValue identifier -> OptionalUniqueIdValue identifier

recordFields :: ApplicationValue -> Maybe (Map.Map Text.Text ApplicationValue)
recordFields (RecordValue fields) = Just fields
recordFields _ = Nothing

checkedPrivateUniqueId :: Word -> PrivateUniqueId
checkedPrivateUniqueId value =
  case mkPrivateUniqueId (fromIntegral value) of
    Left problem -> error ("private-ID fixture failed: " <> show problem)
    Right identifier -> identifier
