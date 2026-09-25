{-# LANGUAGE OverloadedStrings #-}

module CanonicalGoldenProperties
  ( tests,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Data.Word (Word64)
import Eclips.Domain.Identity
  ( GlobalUniqueId,
    ProcessEpochId,
    mkGlobalUniqueId,
    mkProcessEpochId,
  )
import Eclips.Domain.Sort.Canonical
  ( canonicalDescriptorBytes,
    canonicalizeDescriptor,
    decodeCanonicalDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( ApplicationMutation (..),
    DescriptorAdmission (..),
    DescriptorSpec (..),
    PredicateExpression (..),
    RankDirection (..),
    RankTerm (..),
    ScalarComparison (..),
    ScalarLiteral (..),
    SortKind (..),
    StructuralCarrierRole (..),
  )
import Eclips.Domain.Value
  ( FieldName,
    LabelOwner (..),
    Projection,
    Value,
    ValueSchema,
    boolSchema,
    boolValue,
    bytesSchema,
    bytesValue,
    canonicalValueByteString,
    canonicalValueBytes,
    decodeCanonicalValue,
    directProjection,
    enumSchema,
    enumSymbol,
    enumValue,
    globalUniqueIdSchema,
    globalUniqueIdValue,
    int64Schema,
    int64Value,
    labelSchema,
    labelValue,
    mkFieldName,
    mkProjection,
    optionalGlobalUniqueIdValue,
    recordSchema,
    recordValue,
    textSchema,
    textValue,
  )
import Eclips.Public.Types.SortCanonical qualified as Shared
import Numeric (readHex, showHex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "canonical format goldens"
    [ testGroup "descriptors" (map descriptorGoldenTest descriptorVectors),
      testGroup "values" (map valueGoldenTest valueVectors)
    ]

data DescriptorVector = DescriptorVector
  { descriptorVectorName :: String,
    descriptorVectorAdmission :: DescriptorAdmission,
    descriptorVectorSpec :: DescriptorSpec,
    descriptorVectorHex :: String
  }

descriptorGoldenTest :: DescriptorVector -> TestTree
descriptorGoldenTest vector = testCase (descriptorVectorName vector) $ do
  descriptor <-
    checkedIO
      "canonical descriptor"
      (canonicalizeDescriptor (descriptorVectorAdmission vector) (descriptorVectorSpec vector))
  let expectedBytes = parseHex (descriptorVectorHex vector)
  assertEqual
    "fixed canonical bytes"
    (descriptorVectorHex vector)
    (renderHex (canonicalDescriptorBytes descriptor))
  assertEqual
    "fixed bytes decode to the intended descriptor"
    (Right descriptor)
    (decodeCanonicalDescriptor (descriptorVectorAdmission vector) expectedBytes)
  shared <- checkedIO "shared canonical descriptor" (Shared.decodeRawDescriptor expectedBytes)
  assertEqual
    "the shared encoder preserves the fixed descriptor bytes"
    expectedBytes
    (Shared.encodeRawDescriptor shared)
  assertEqual
    "the shared content address agrees with semantic admission"
    (descriptorSortId descriptor)
    (Shared.rawDescriptorSortId shared)

descriptorVectors :: [DescriptorVector]
descriptorVectors =
  [ DescriptorVector
      "complete regular grammar"
      ApplicationDescriptor
      completeRegularSpec
      ( "45434c4950532d44000600000000000000070000000000000006615f626f6f6c000000"
          <> "000000000007625f6279746573020000000000000005635f696e740100000000000000"
          <> "07645f6c6162656c050000000000000008655f6e657374656406000000000000000100"
          <> "00000000000005696e6e6572030000000000000006665f746578740300000000000000"
          <> "05675f75696404000000000000000200000000000000010000000000000005675f7569"
          <> "6400000000000000020000000000000008655f6e65737465640000000000000005696e"
          <> "6e657204000000000000000b00010200000000000000010000000000000006615f626f"
          <> "6f6c0000000200000000000000010000000000000005635f696e740101ffffffffff"
          <> "ffffff0200000000000000010000000000000005635f696e740201fffffffffffffffe"
          <> "0200000000000000010000000000000005635f696e7403010000000000000000020000"
          <> "0000000000010000000000000005635f696e7404010000000000000001020000000000"
          <> "0000010000000000000005635f696e7405017fffffffffffffff020000000000000001"
          <> "0000000000000007625f62797465730002000000000000000200ff0200000000000000"
          <> "010000000000000006665f74657874000300000000000000096c616d6264613dcebb03"
          <> "0500000000000000020100010000000000000003000000000000000002000000000000"
          <> "0008655f6e65737465640000000000000005696e6e6572000000000000000000010000"
          <> "000000000006665f74657874010101ffffffffffffffff00000000"
      ),
    DescriptorVector
      "controlled policy and present label"
      ApplicationDescriptor
      controlledPolicySpec
      ( "45434c4950532d44010600000000000000030000000000000007645f6c6162656c05"
          <> "0000000000000005675f7569640400000000000000077061796c6f6164030000000000"
          <> "00000100000000000000010000000000000005675f7569640001000000000000000101"
          <> "00000000000000000001010000000000000007645f6c6162656c0000"
      ),
    DescriptorVector
      "enum schema, literal, and rank"
      ApplicationDescriptor
      enumDescriptorSpec
      ( "45434c4950532d44000600000000000000010000000000000004656e756d07000000"
          <> "000000000200000000000000047a6574610000000000000005616c7068610000000000"
          <> "00000100000000000000010000000000000004656e756d020000000000000001000000"
          <> "0000000004656e756d040400000000000000047a65746101000000000000000200000000"
          <> "00000000010000000000000004656e756d000100000000000000000000000000"
      )
  ]
    <> map carrierVector [minBound .. maxBound]

carrierVector :: StructuralCarrierRole -> DescriptorVector
carrierVector role =
  DescriptorVector
    ("present structural role " <> show role)
    PrimordialDescriptor
    (carrierSpec role)
    (carrierHex role)

carrierHex :: StructuralCarrierRole -> String
carrierHex NeutralVertexCarrier =
  "45434c4950532d44010600000000000000010000000000000005675f75696404000000000000000100000000000000010000000000000005675f75696400010000000000000001010000000000000000000000000100"
carrierHex EdgeCarrier =
  "45434c4950532d44010600000000000000010000000000000005675f75696404000000000000000100000000000000010000000000000005675f75696400010000000000000001010000000000000000000000000101"
carrierHex NablaCarrier =
  "45434c4950532d44010600000000000000010000000000000005675f75696404000000000000000100000000000000010000000000000005675f75696400010000000000000001010000000000000000000000000102"
carrierHex DeltaCarrier =
  "45434c4950532d44010600000000000000010000000000000005675f75696404000000000000000100000000000000010000000000000005675f75696400010000000000000001010000000000000000000000000103"
carrierHex ProcessEpochCarrier =
  "45434c4950532d44010600000000000000010000000000000005675f75696404000000000000000100000000000000010000000000000005675f75696400010000000000000001010000000000000000000000010104"

completeRegularSpec :: DescriptorSpec
completeRegularSpec =
  DescriptorSpec
    { descriptorSpecKind = RegularSort,
      descriptorSpecSchema = completeSchema,
      descriptorSpecKeyProjections =
        [directProjection uidField, nestedProjection],
      descriptorSpecValidity =
        AllPredicates
          ( AlwaysPredicate
              :| [ NeverPredicate,
                   CompareField (directProjection boolField) ScalarEqual (LiteralBool False),
                   CompareField (directProjection intField) ScalarNotEqual (LiteralInt64 (-1)),
                   CompareField (directProjection intField) ScalarLessThan (LiteralInt64 (-2)),
                   CompareField (directProjection intField) ScalarLessThanOrEqual (LiteralInt64 0),
                   CompareField (directProjection intField) ScalarGreaterThan (LiteralInt64 1),
                   CompareField (directProjection intField) ScalarGreaterThanOrEqual (LiteralInt64 (maxBound :: Int64)),
                   CompareField (directProjection bytesField) ScalarEqual (LiteralBytes (ByteString.pack [0, 255])),
                   CompareField (directProjection textField) ScalarEqual (LiteralText "lambda=\955"),
                   NotPredicate (AnyPredicates (NeverPredicate :| [AlwaysPredicate]))
                 ]
          ),
      descriptorSpecObsolescence = NeverPredicate,
      descriptorSpecRankTerms =
        RankField nestedProjection Ascending
          :| [ RankField (directProjection textField) Descending,
               RankApplicationValue Descending
             ],
      descriptorSpecMinimumRetentionMicros = maxBound :: Word64,
      descriptorSpecImmutable = False,
      descriptorSpecLabelField = Nothing,
      descriptorSpecApplicationMutation = OrdinaryApplicationMutation,
      descriptorSpecStructuralCarrierRole = Nothing
    }

completeSchema :: ValueSchema
completeSchema =
  checked
    "complete schema"
    ( recordSchema
        [ (uidField, globalUniqueIdSchema),
          (textField, textSchema),
          (nestedField, nestedSchema),
          (labelField, labelSchema),
          (intField, int64Schema),
          (bytesField, bytesSchema),
          (boolField, boolSchema)
        ]
    )

nestedSchema :: ValueSchema
nestedSchema = checked "nested schema" (recordSchema [(innerField, textSchema)])

nestedProjection :: Projection
nestedProjection = mkProjection (nestedField :| [innerField])

controlledPolicySpec :: DescriptorSpec
controlledPolicySpec =
  baseControlledSpec
    { descriptorSpecSchema =
        checked
          "controlled policy schema"
          ( recordSchema
              [ (payloadField, textSchema),
                (labelField, labelSchema),
                (uidField, globalUniqueIdSchema)
              ]
          ),
      descriptorSpecMinimumRetentionMicros = 0,
      descriptorSpecImmutable = True,
      descriptorSpecLabelField = Just labelField
    }

carrierSpec :: StructuralCarrierRole -> DescriptorSpec
carrierSpec role =
  baseControlledSpec
    { descriptorSpecApplicationMutation =
        if role == ProcessEpochCarrier
          then HeraldManagedMutation
          else OrdinaryApplicationMutation,
      descriptorSpecStructuralCarrierRole = Just role
    }

baseControlledSpec :: DescriptorSpec
baseControlledSpec =
  DescriptorSpec
    { descriptorSpecKind = ControlledSort,
      descriptorSpecSchema =
        checked "controlled base schema" (recordSchema [(uidField, globalUniqueIdSchema)]),
      descriptorSpecKeyProjections = [directProjection uidField],
      descriptorSpecValidity = AlwaysPredicate,
      descriptorSpecObsolescence = NeverPredicate,
      descriptorSpecRankTerms = RankApplicationValue Ascending :| [],
      descriptorSpecMinimumRetentionMicros = 0,
      descriptorSpecImmutable = False,
      descriptorSpecLabelField = Nothing,
      descriptorSpecApplicationMutation = OrdinaryApplicationMutation,
      descriptorSpecStructuralCarrierRole = Nothing
    }

enumDescriptorSpec :: DescriptorSpec
enumDescriptorSpec =
  DescriptorSpec
    { descriptorSpecKind = RegularSort,
      descriptorSpecSchema =
        checked
          "enum descriptor schema"
          ( recordSchema
              [ ( enumField,
                  checked
                    "enum member schema"
                    (enumSchema (enumSymbol "zeta" :| [enumSymbol "alpha"]))
                )
              ]
          ),
      descriptorSpecKeyProjections = [directProjection enumField],
      descriptorSpecValidity =
        CompareField
          (directProjection enumField)
          ScalarGreaterThan
          (LiteralEnum (enumSymbol "zeta")),
      descriptorSpecObsolescence = NeverPredicate,
      descriptorSpecRankTerms =
        RankField (directProjection enumField) Ascending
          :| [RankApplicationValue Ascending],
      descriptorSpecMinimumRetentionMicros = 0,
      descriptorSpecImmutable = False,
      descriptorSpecLabelField = Nothing,
      descriptorSpecApplicationMutation = OrdinaryApplicationMutation,
      descriptorSpecStructuralCarrierRole = Nothing
    }

data ValueVector = ValueVector
  { valueVectorName :: String,
    valueVectorValue :: Value,
    valueVectorHex :: String
  }

valueGoldenTest :: ValueVector -> TestTree
valueGoldenTest vector = testCase (valueVectorName vector) $ do
  let expectedBytes = parseHex (valueVectorHex vector)
      actualBytes = canonicalValueByteString (canonicalValueBytes (valueVectorValue vector))
  assertEqual "fixed canonical bytes" (valueVectorHex vector) (renderHex actualBytes)
  assertEqual
    "fixed bytes decode to the intended value"
    (Right (valueVectorValue vector))
    (decodeCanonicalValue expectedBytes)

valueVectors :: [ValueVector]
valueVectors =
  [ ValueVector "false" (boolValue False) "45434c4950532d560000",
    ValueVector "true" (boolValue True) "45434c4950532d560001",
    ValueVector "minimum int64" (int64Value minBound) "45434c4950532d56018000000000000000",
    ValueVector "maximum int64" (int64Value maxBound) "45434c4950532d56017fffffffffffffff",
    ValueVector
      "bytes"
      (bytesValue (ByteString.pack [0, 255]))
      "45434c4950532d5602000000000000000200ff",
    ValueVector
      "UTF-8 text"
      (textValue "lambda=\955")
      "45434c4950532d560300000000000000096c616d6264613dcebb",
    ValueVector
      "global unique ID"
      (globalUniqueIdValue fixtureGlobalUniqueId)
      "45434c4950532d56040000000000000020000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
    ValueVector "void label" (labelValue (VoidLabel, 0)) "45434c4950532d5605000000000000000000",
    ValueVector
      "process label"
      (labelValue ((ProcessLabel processEpoch, 0)))
      "45434c4950532d5605010000000000000020aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0000000000000000",
    ValueVector
      "zombie label"
      (labelValue ((ZombieLabel processEpoch, 0)))
      "45434c4950532d5605020000000000000020aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0000000000000000",
    ValueVector
      "sorted nested record"
      nestedRecordValue
      "45434c4950532d5606000000000000000200000000000000016106000000000000000100000000000000017801ffffffffffffffff00000000000000017a0001",
    ValueVector
      "enum symbol"
      (enumValue (enumSymbol "preserve"))
      "45434c4950532d560700000000000000087072657365727665",
    ValueVector
      "optional global unique ID: None"
      (optionalGlobalUniqueIdValue Nothing)
      "45434c4950532d560800",
    ValueVector
      "optional global unique ID: Some"
      (optionalGlobalUniqueIdValue (Just fixtureGlobalUniqueId))
      "45434c4950532d5608010000000000000020000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
  ]

nestedRecordValue :: Value
nestedRecordValue =
  checked
    "outer record"
    ( recordValue
        [ (zField, boolValue True),
          (aField, checked "inner record" (recordValue [(xField, int64Value (-1))]))
        ]
    )

fixtureGlobalUniqueId :: GlobalUniqueId
fixtureGlobalUniqueId =
  checked
    "global unique ID"
    (mkGlobalUniqueId (ByteString.pack [0 .. 31]))

processEpoch :: ProcessEpochId
processEpoch =
  checked
    "process epoch ID"
    (mkProcessEpochId (ByteString.replicate 32 0xaa))

boolField, bytesField, enumField, intField, labelField, nestedField, innerField, payloadField, textField, uidField, aField, xField, zField :: FieldName
boolField = field "a_bool"
bytesField = field "b_bytes"
enumField = field "enum"
intField = field "c_int"
labelField = field "d_label"
nestedField = field "e_nested"
innerField = field "inner"
payloadField = field "payload"
textField = field "f_text"
uidField = field "g_uid"
aField = field "a"
xField = field "x"
zField = field "z"

field :: String -> FieldName
field = checked "field name" . mkFieldName . Text.pack

parseHex :: String -> ByteString
parseHex [] = ByteString.empty
parseHex (high : low : remaining) =
  case readHex [high, low] of
    [(value, "")] -> ByteString.cons value (parseHex remaining)
    _ -> error "invalid golden hexadecimal digit"
parseHex [_] = error "odd-length golden hexadecimal string"

renderHex :: ByteString -> String
renderHex = concatMap renderByte . ByteString.unpack
  where
    renderByte byte = case showHex byte "" of
      [digit] -> ['0', digit]
      digits -> digits

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (error . ((label <> ": ") <>) . show) id

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO label = either (assertFailure . ((label <> ": ") <>) . show) pure
