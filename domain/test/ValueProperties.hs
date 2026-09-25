{-# LANGUAGE OverloadedStrings #-}

module ValueProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Eclips.Domain.Identity (mkGlobalUniqueId, mkProcessEpochId)
import Eclips.Domain.Value
  ( LabelOwner (..),
    SchemaError (DuplicateEnumSymbol),
    SchemaView (EnumSchema, OptionalGlobalUniqueIdSchema, RecordSchema),
    ValueError (ValueSchemaMismatch),
    ValueView (EnumValue, GlobalUniqueIdValue, OptionalGlobalUniqueIdValue, RecordValue),
    boolSchema,
    boolValue,
    canonicalValueByteString,
    canonicalValueBytes,
    checkValueSchema,
    decodeCanonicalValue,
    enumMemberOrdinal,
    enumSchema,
    enumSymbol,
    enumValue,
    fieldNameText,
    globalUniqueIdValue,
    int64Schema,
    int64Value,
    labelValue,
    mkFieldName,
    optionalGlobalUniqueIdSchema,
    optionalGlobalUniqueIdValue,
    recordSchema,
    recordSchemaMap,
    recordValue,
    recordValueMap,
    viewSchema,
    viewValue,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck (chooseInt, conjoin, elements, forAll, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "domain value"
    [ testCase "a checked schema map constructs a record without another admission branch" caseRecordSchemaMap,
      testCase "a checked value map constructs a record without another admission branch" caseRecordValueMap,
      testCase "enum schemas retain declaration order and reject duplicates" caseEnumSchema,
      testCase "enum schemas admit exactly their declared members" caseEnumMembership,
      testCase "optional global identities preserve explicit None and Some" caseOptionalGlobalUniqueId,
      testProperty "canonical label values retain and distinguish generations"
        $ forAll (chooseInt (0, 100000))
        $ \generation ->
          let process = either (error . show) id (mkProcessEpochId (ByteString.replicate 32 7))
           in forAll (elements [VoidLabel, ProcessLabel process, ZombieLabel process]) $ \owner ->
                let value = labelValue (owner, fromIntegral generation)
                    nextValue = labelValue (owner, fromIntegral generation + 1)
                    bytes = canonicalValueByteString (canonicalValueBytes value)
                 in conjoin
                      [ decodeCanonicalValue bytes === Right value,
                        (bytes /= canonicalValueByteString (canonicalValueBytes nextValue)) === True
                      ]
    ]

caseRecordSchemaMap :: IO ()
caseRecordSchemaMap = do
  first <- checked "first field" (mkFieldName "first")
  second <- checked "second field" (mkFieldName "second")
  let fields = Map.fromList [(second, int64Schema), (first, boolSchema)]
      fromMap = recordSchemaMap fields
  assertEqual
    "map constructor retains admitted fields"
    (RecordSchema fields)
    (viewSchema fromMap)
  assertEqual
    "list and map constructors agree"
    (Right fromMap)
    (recordSchema (Map.toAscList fields))

caseRecordValueMap :: IO ()
caseRecordValueMap = do
  first <- checked "first field" (mkFieldName "first")
  second <- checked "second field" (mkFieldName "second")
  let fields =
        Map.fromList
          [ (second, int64Value 2),
            (first, boolValue True)
          ]
      fromMap = recordValueMap fields
  assertEqual
    "map constructor retains admitted fields"
    (RecordValue fields)
    (viewValue fromMap)
  assertEqual
    "list and map constructors agree"
    (Right fromMap)
    (recordValue (Map.toAscList fields))
  assertEqual
    "the map uses checked field-name ordering"
    ["first", "second"]
    (fmap fieldNameText (Map.keys fields))

caseEnumSchema :: IO ()
caseEnumSchema = do
  let first = enumSymbol "zeta"
      second = enumSymbol "alpha"
      third = enumSymbol "middle"
      members = first :| [second, third]
      duplicate = first :| [second, first]
  schema <- checked "enum schema" (enumSchema members)
  assertEqual "declaration order" (EnumSchema members) (viewSchema schema)
  assertEqual "first ordinal" (Just 0) (enumMemberOrdinal schema first)
  assertEqual "second ordinal" (Just 1) (enumMemberOrdinal schema second)
  assertEqual
    "duplicate member"
    (Left (DuplicateEnumSymbol first))
    (enumSchema duplicate)

caseEnumMembership :: IO ()
caseEnumMembership = do
  let first = enumSymbol "ready"
      second = enumSymbol "done"
      missing = enumSymbol "unknown"
      members = first :| [second]
      admitted = enumValue second
      rejected = enumValue missing
  schema <- checked "enum schema" (enumSchema members)
  assertEqual "declared member" (Right ()) (checkValueSchema schema admitted)
  assertEqual
    "undeclared member"
    ( Left
        ( ValueSchemaMismatch
            (EnumSchema members)
            (EnumValue missing)
        )
    )
    (checkValueSchema schema rejected)

caseOptionalGlobalUniqueId :: IO ()
caseOptionalGlobalUniqueId = do
  identifier <- checked "global unique ID" (mkGlobalUniqueId (ByteString.replicate 32 0x5a))
  let none = optionalGlobalUniqueIdValue Nothing
      some = optionalGlobalUniqueIdValue (Just identifier)
      ordinary = globalUniqueIdValue identifier
  assertEqual "schema view" OptionalGlobalUniqueIdSchema (viewSchema optionalGlobalUniqueIdSchema)
  assertEqual "None view" (OptionalGlobalUniqueIdValue Nothing) (viewValue none)
  assertEqual "Some view" (OptionalGlobalUniqueIdValue (Just identifier)) (viewValue some)
  assertEqual "None admission" (Right ()) (checkValueSchema optionalGlobalUniqueIdSchema none)
  assertEqual "Some admission" (Right ()) (checkValueSchema optionalGlobalUniqueIdSchema some)
  assertEqual
    "a plain identity is not an implicit Some"
    ( Left
        ( ValueSchemaMismatch
            OptionalGlobalUniqueIdSchema
            (GlobalUniqueIdValue identifier)
        )
    )
    (checkValueSchema optionalGlobalUniqueIdSchema ordinary)
  mapM_
    ( \value ->
        assertEqual
          "canonical round-trip"
          (Right value)
          (decodeCanonicalValue (canonicalValueByteString (canonicalValueBytes value)))
    )
    [none, some]

checked :: (Show problem) => String -> Either problem value -> IO value
checked description =
  either (fail . ((description <> ": ") <>) . show) pure
