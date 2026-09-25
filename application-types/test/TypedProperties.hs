{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module TypedProperties (tests) where

import Data.ByteString qualified as Bytes
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Eclips.Application.Types.Identity (PrivateUniqueId, asPrivateProcessId, mkPrivateUniqueId)
import Eclips.Application.Types.Query qualified as Query
import Eclips.Application.Types.SortDescriptor
  ( ApplicationProjection (..),
    ApplicationValueSchema (..),
  )
import Eclips.Application.Types.SortDescriptor qualified as Descriptor
import Eclips.Application.Types.Typed
import Eclips.Application.Types.Value (ApplicationLabel, ApplicationLabelOwner (..), ApplicationValue (..))
import GHC.Generics (Generic)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (Gen, Property, arbitrary, choose, elements, forAll, listOf, resize, testProperty, (===))

data Priority = Urgent | Normal | Deferred
  deriving stock (Eq, Show, Generic)
instance ValueType Priority
instance Scalar Priority
instance QueryScalar Priority
instance DescriptorScalar Priority

newtype Counter = Counter Int64
  deriving stock (Eq, Show, Generic)
instance ValueType Counter
instance Scalar Counter
instance QueryScalar Counter
instance DescriptorScalar Counter

data Counted = Counted {counter :: Counter}
  deriving stock (Eq, Show, Generic)
instance ValueType Counted
instance ApplicationSort Counted where
  sortPolicy =
    withValidity
      (descriptorCompare (field @"counter") ScalarGreaterThan (Counter 0))
      (withRank [descending (field @"counter")] Ascending (regularPolicy [key (field @"counter")]))

data Detail = Detail
  { enabled :: Bool,
    priority :: Priority
  }
  deriving stock (Eq, Show, Generic)
instance ValueType Detail

data Message = Message
  { message :: Text,
    sequenceNumber :: Int64,
    detail :: Detail
  }
  deriving stock (Eq, Show, Generic)
instance ValueType Message
instance ApplicationSort Message where
  sortPolicy = regularPolicy [key (field @"message")]

newtype RetainedMessage = RetainedMessage Message
  deriving stock (Eq, Show, Generic)
instance ValueType RetainedMessage
instance ApplicationSort RetainedMessage where
  sortPolicy = withRetention 1_000 (regularPolicy [key (field @"message")])

newtype SamePolicyMessage = SamePolicyMessage Message
  deriving stock (Eq, Show, Generic)
instance ValueType SamePolicyMessage
instance ApplicationSort SamePolicyMessage where
  sortPolicy = regularPolicy [key (field @"message")]

data AllShapes = AllShapes
  { boolean :: Bool,
    integer :: Int64,
    bytes :: Bytes.ByteString,
    text :: Text,
    identity :: PrivateUniqueId,
    label :: ApplicationLabel,
    optionalIdentity :: Maybe PrivateUniqueId,
    nested :: Detail
  }
  deriving stock (Eq, Show, Generic)
instance ValueType AllShapes

data Controlled = Controlled
  { objectId :: PrivateUniqueId,
    objectLabel :: ApplicationLabel,
    value :: Int64
  }
  deriving stock (Eq, Show, Generic)
instance ValueType Controlled
instance ApplicationSort Controlled where
  type SortKindOf Controlled = 'ControlledSort
  sortPolicy = withImmutable True (controlledPolicy (field @"objectId") (Just (field @"objectLabel")))

newtype LabelRanked = LabelRanked Controlled
  deriving stock (Eq, Show, Generic)
instance ValueType LabelRanked
instance ApplicationSort LabelRanked where
  type SortKindOf LabelRanked = 'ControlledSort
  sortPolicy = withRank [ascending (field @"objectLabel")] Ascending (controlledPolicy (field @"objectId") (Just (field @"objectLabel")))

data NestedControlled = NestedControlled {object :: Controlled}
  deriving stock (Eq, Show, Generic)
instance ValueType NestedControlled
instance ApplicationSort NestedControlled where
  type SortKindOf NestedControlled = 'ControlledSort
  sortPolicy = controlledPolicy (composeField (field @"object") (field @"objectId")) Nothing

data Recursive = Recursive {child :: Recursive}
  deriving stock (Generic)
instance ValueType Recursive

data First = First {second :: Second}
  deriving stock (Generic)
data Second = Second {first :: First}
  deriving stock (Generic)
instance ValueType First
instance ValueType Second

data InvalidName = InvalidName {invalid'name :: Text}
  deriving stock (Generic)
instance ValueType InvalidName

data SingletonEnum = SingletonEnum
  deriving stock (Eq, Show, Generic)
instance ValueType SingletonEnum

tests :: TestTree
tests =
  testGroup
    "typed application values and sorts"
    [ testProperty "nested generic records round trip" propRecordRoundTrip,
      testProperty "every generated schema-admitted value decodes and reencodes exactly" propSchemaCoverage,
      testProperty "transparent newtypes preserve value representation" propNewtype,
      testCase "enum declaration order and exact symbols are preserved" caseEnum,
      testCase "single-constructor nullary enums use the enum schema" caseSingletonEnum,
      testCase "scalar newtypes inherit representation and literal capabilities" caseScalarNewtype,
      testCase "nested typed projections lower to the same closed query AST" caseProjection,
      testCase "identity and label literals remain query-only scalars" caseIdentityQuery,
      testCase "full policy determines identity independently of host names" caseSortIdentity,
      testCase "regular sort rank ends in exactly one whole-value fallback" caseRank,
      testCase "controlled descriptors admit direct identity and label fields" caseControlled,
      testCase "controlled policies reject nested identity keys and label rank" caseControlledRejection,
      testCase "record decoding rejects missing extra and incorrectly typed fields" caseDecodeRejection,
      testCase "generic schemas reject invalid names and recursive types" caseSchemaRejection
    ]

genText :: Gen Text
genText = Text.pack <$> resize 12 (listOf arbitrary)

genDetail :: Gen Detail
genDetail = Detail <$> arbitrary <*> elements [Urgent, Normal, Deferred]

genMessage :: Gen Message
genMessage = Message <$> genText <*> arbitrary <*> genDetail

propRecordRoundTrip :: Property
propRecordRoundTrip = forAll genMessage $ \input -> decodeValue (encodeValue input) === Right input

propNewtype :: Property
propNewtype = forAll genMessage $ \input ->
  (encodeValue (RetainedMessage input), decodeValue @RetainedMessage (encodeValue input))
    === (encodeValue input, Right (RetainedMessage input))

propSchemaCoverage :: Property
propSchemaCoverage = forAll (genSchemaValue (checked (valueSchema @AllShapes))) $ \raw ->
  (encodeValue <$> decodeValue @AllShapes raw) === Right raw

-- Generate from the advertised schema, independently of the Haskell encoder.
-- This exercises coverage rather than only the easier decode . encode law.
genSchemaValue :: ApplicationValueSchema -> Gen ApplicationValue
genSchemaValue = \case
  BoolSchema -> BoolValue <$> arbitrary
  Int64Schema -> Int64Value <$> arbitrary
  BytesSchema -> BytesValue . Bytes.pack <$> resize 12 (listOf arbitrary)
  TextSchema -> TextValue <$> genText
  UniqueIdSchema -> UniqueIdValue <$> genIdentity
  LabelSchema -> do
    process <- asPrivateProcessId <$> genIdentity
    owner <- elements [VoidLabel, ProcessLabel process, ZombieLabel process]
    generation <- choose (0, 10_000)
    pure (LabelValue (owner, generation))
  RecordSchema fields -> RecordValue <$> traverse genSchemaValue fields
  EnumSchema members -> EnumValue <$> elements (foldr (:) [] members)
  OptionalUniqueIdSchema -> do
    identifier <- genIdentity
    OptionalUniqueIdValue <$> elements [Nothing, Just identifier]

genIdentity :: Gen PrivateUniqueId
genIdentity = checked . mkPrivateUniqueId <$> choose (1, 10_000)

caseEnum :: IO ()
caseEnum = do
  assertEqual "declared order" (Right (EnumSchema ("Urgent" :| ["Normal", "Deferred"]))) (valueSchema @Priority)
  assertEqual "exact symbol" (EnumValue "Normal") (encodeValue Normal)
  assertBool "unknown enum rejected" (isLeft (decodeValue @Priority (EnumValue "normal")))
  assertEqual
    "typed enum query"
    (Query.QueryCompare (ApplicationProjection ("detail" :| ["priority"])) ScalarEqual (Query.QueryEnum "Urgent"))
    (lowerQueryPredicate (queryEqual (composeField (field @"detail" @Message) (field @"priority")) Urgent))

caseSingletonEnum :: IO ()
caseSingletonEnum = do
  assertEqual "singleton schema" (Right (EnumSchema ("SingletonEnum" :| []))) (valueSchema @SingletonEnum)
  assertEqual "singleton round trip" (Right SingletonEnum) (decodeValue (encodeValue SingletonEnum))

caseScalarNewtype :: IO ()
caseScalarNewtype = do
  assertEqual "newtype schema" (Right Int64Schema) (valueSchema @Counter)
  assertEqual "newtype encoding" (Int64Value 4) (encodeValue (Counter 4))
  assertEqual
    "newtype query"
    (Query.QueryCompare (ApplicationProjection ("counter" :| [])) ScalarEqual (Query.QueryInt64 4))
    (lowerQueryPredicate (queryEqual (field @"counter" @Counted) (Counter 4)))
  let descriptor = sortDescriptor (checked (compileSort @Counted))
  assertEqual
    "newtype descriptor literal"
    (Descriptor.CompareField (ApplicationProjection ("counter" :| [])) ScalarGreaterThan (Descriptor.LiteralInt64 0))
    (Descriptor.validityPredicate descriptor)
  assertEqual
    "rank with one whole-value fallback"
    (Descriptor.RankField (ApplicationProjection ("counter" :| [])) Descending :| [Descriptor.RankApplicationValue Ascending])
    (Descriptor.rankTerms descriptor)

caseProjection :: IO ()
caseProjection =
  assertEqual
    "nested projection"
    (Query.QueryCompare (ApplicationProjection ("detail" :| ["enabled"])) ScalarEqual (Query.QueryBool True))
    (lowerQueryPredicate (queryEqual (composeField (field @"detail" @Message) (field @"enabled")) True))

caseIdentityQuery :: IO ()
caseIdentityQuery = do
  let identifier = checked (mkPrivateUniqueId 7)
      sampledLabel = (ProcessLabel (asPrivateProcessId identifier), 3)
  assertEqual
    "identity query"
    (Query.QueryCompare (ApplicationProjection ("objectId" :| [])) ScalarEqual (Query.QueryUniqueId identifier))
    (lowerQueryPredicate (queryEqual (field @"objectId" @Controlled) identifier))
  assertEqual
    "label query"
    (Query.QueryCompare (ApplicationProjection ("objectLabel" :| [])) ScalarEqual (Query.QueryLabel sampledLabel))
    (lowerQueryPredicate (queryEqual (field @"objectLabel" @Controlled) sampledLabel))

caseSortIdentity :: IO ()
caseSortIdentity = do
  let ordinary = checked (compileSort @Message)
      retained = checked (compileSort @RetainedMessage)
      same = checked (compileSort @SamePolicyMessage)
  assertEqual "same representation" (valueSchema @Message) (valueSchema @RetainedMessage)
  assertBool "retention changes full sort identity" (sortId ordinary /= sortId retained)
  assertEqual "host newtype name is not wire identity" (sortId ordinary) (sortId same)

caseRank :: IO ()
caseRank =
  assertEqual
    "whole-value rank"
    (Descriptor.RankApplicationValue Ascending :| [])
    (Descriptor.rankTerms (sortDescriptor (checked (compileSort @Message))))

caseControlled :: IO ()
caseControlled = do
  let descriptor = sortDescriptor (checked (compileSort @Controlled))
  assertEqual "kind" ControlledSort (Descriptor.sortKind descriptor)
  assertEqual "direct identity key" [ApplicationProjection ("objectId" :| [])] (Descriptor.keyProjections descriptor)
  assertEqual "label field" (Just "objectLabel") (Descriptor.labelField descriptor)
  assertBool "immutable" (Descriptor.isImmutable descriptor)

caseControlledRejection :: IO ()
caseControlledRejection = do
  assertEqual "nested key" (Left ControlledKeyMustBeDirectIdentity) (compileSort @NestedControlled)
  assertEqual "label rank" (Left (ControlledLabelUsedByPolicy "objectLabel")) (compileSort @LabelRanked)

caseDecodeRejection :: IO ()
caseDecodeRejection = do
  let fields = Map.fromList [("message", TextValue "hello"), ("sequenceNumber", Int64Value 4), ("detail", encodeValue (Detail True Normal))]
  assertBool "missing" (isLeft (decodeValue @Message (RecordValue (Map.delete "message" fields))))
  assertBool "extra" (isLeft (decodeValue @Message (RecordValue (Map.insert "extra" (BoolValue True) fields))))
  case decodeValue @Message (RecordValue (Map.insert "message" (Int64Value 0) fields)) of
    Left (ValueError path _ _) -> assertEqual "field diagnostic" ["message"] path
    Right _ -> fail "wrong field type decoded"

caseSchemaRejection :: IO ()
caseSchemaRejection = do
  assertEqual "field-name admission" (Left (InvalidFieldName "invalid'name")) (valueSchema @InvalidName)
  assertBool "direct recursion" (isLeft (valueSchema @Recursive))
  assertBool "mutual recursion" (isLeft (valueSchema @First))

checked :: (Show problem) => Either problem result -> result
checked = either (error . show) id
