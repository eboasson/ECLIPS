{-# LANGUAGE OverloadedStrings #-}

module DescriptorProperties
  ( tests,
    emptyKeyDescriptor,
    regularDescriptor,
    regularDescriptorSpec,
    regularValue,
    fixtureField,
    fixturePublicationObject,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as ByteString
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)
import Eclips.Domain.Identity
  ( GlobalUniqueId,
    mkGlobalUniqueId,
    mkProcessEpochId,
    sortIdBytes,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    CanonicalDescriptorError (..),
    DescriptorDecodeError (..),
    canonicalCheckedDescriptor,
    canonicalDescriptorBytes,
    canonicalizeDescriptor,
    decodeCanonicalDescriptor,
    descriptorSortId,
    validateCanonicalDescriptorIdentity,
  )
import Eclips.Domain.Sort.Descriptor
  ( ApplicationMutation (..),
    CheckedInstance,
    DescriptorAdmission (..),
    DescriptorError (..),
    DescriptorSpec (..),
    Lifecycle (..),
    PredicateExpression (..),
    RankDirection (..),
    RankTerm (..),
    ScalarComparison (..),
    ScalarLiteral (..),
    SortKind (..),
    StructuralCarrierRole (..),
    ValueEvaluationError (..),
    checkDescriptor,
    checkedInstanceKey,
    checkedInstanceLifecycle,
    checkedInstanceRank,
    controlledPolicyProjection,
    descriptorControlledKeyProjection,
    evaluateValue,
  )
import Eclips.Domain.Value
  ( EnumSymbol,
    FieldName,
    FieldNameError (..),
    Label,
    LabelOwner (..),
    SchemaView (EnumSchema),
    Value,
    ValueError (..),
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
    optionalGlobalUniqueIdSchema,
    optionalGlobalUniqueIdValue,
    recordSchema,
    recordValue,
    textSchema,
    textValue,
  )
import Numeric (showHex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
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
    "checked descriptors and values"
    [ testGroup
        "canonical identity"
        [ testCase "descriptor bytes decode and re-encode exactly" caseDescriptorRoundTrip,
          testCase "an empty regular key list round-trips canonically" caseEmptyKeyRoundTrip,
          testCase "every current descriptor constructor family round-trips" caseDescriptorConstructorCoverage,
          testCase "descriptor bytes match the fixed golden vector" caseDescriptorGolden,
          testCase "SortId is SHA-256 of canonical bytes" caseSortIdHash,
          testCase "sort-definition identity is re-parsed and recomputed" caseClaimedSortId,
          testCase "private encoding starts with the descriptor domain tag" caseDescriptorDomainTag,
          testCase "the descriptor domain tag is checked on decode" caseWrongDescriptorDomain,
          testCase "decoded syntax still passes descriptor admission" caseDecodedDescriptorAdmission,
          testCase "trailing descriptor bytes are rejected" caseTrailingBytes,
          testCase "malformed descriptor bytes are rejected" caseMalformedDescriptor,
          testCase "record construction order does not affect identity" caseRecordOrder,
          testCase "signed value extrema round-trip" caseSignedExtrema,
          testCase "every current value constructor round-trips" caseValueConstructorRoundTrips,
          testCase "malformed value bytes have a closed error" caseMalformedValue,
          testCase "the value domain tag is checked on decode" caseWrongValueDomain,
          testCase "decoded syntax still passes value admission" caseDecodedValueAdmission,
          testCase "trailing value bytes are rejected" caseTrailingValueBytes,
          testCase "noncanonical value record order is rejected" caseNonCanonicalValueOrder,
          testProperty "canonical values decode and re-encode" propValueRoundTrip
        ],
      testGroup
        "descriptor admission"
        [ testCase "regular sorts cannot be immutable" caseRegularImmutable,
          testCase "rank requires a final application-value tie-break" caseRankMustBeTotal,
          testCase "predicate literals are checked against projected schema" casePredicateType,
          testCase "enum predicate literals must be declared members" caseEnumPredicateAdmission,
          testCase "controlled key is one direct global-unique-ID field" caseControlledKey,
          testCase "controlled label is excluded from predicates" caseControlledLabelPredicate,
          testCase "application descriptors cannot claim primordial policy" caseApplicationPolicy
        ],
      testGroup
        "total evaluator"
        [ testCase "invalid application value is rejected" caseValidity,
          testCase "obsolescence produces the dominant lifecycle" caseLifecycle,
          testCase "rank fields order values" caseRankField,
          testCase "enum keys, predicates, and ranks use declaration order" caseEnumEvaluation,
          testCase "application-value term breaks remaining ties" caseRankTieBreak,
          testCase "equal projected keys name one object" caseObjectKey,
          testCase "an empty key list makes every value one object" caseEmptyObjectKey,
          testCase "the controlled key accessor is kind-qualified" caseControlledKeyProjection,
          testCase "controlled label does not affect application rank" caseLabelRankExclusion,
          testCase "controlled descriptors expose an immutable policy projection" casePolicyProjection
        ]
    ]

regularDescriptor :: CanonicalDescriptor
regularDescriptor = checked "regular descriptor" (canonicalizeDescriptor ApplicationDescriptor regularDescriptorSpec)

emptyKeyDescriptor :: CanonicalDescriptor
emptyKeyDescriptor =
  checked
    "empty-key regular descriptor"
    ( canonicalizeDescriptor
        ApplicationDescriptor
        regularDescriptorSpec {descriptorSpecKeyProjections = []}
    )

regularDescriptorSpec :: DescriptorSpec
regularDescriptorSpec =
  DescriptorSpec
    { descriptorSpecKind = RegularSort,
      descriptorSpecSchema = regularSchema,
      descriptorSpecKeyProjections = [directProjection objectField],
      descriptorSpecValidity =
        CompareField
          (directProjection revisionField)
          ScalarGreaterThanOrEqual
          (LiteralInt64 0),
      descriptorSpecObsolescence =
        CompareField
          (directProjection obsoleteField)
          ScalarEqual
          (LiteralBool True),
      descriptorSpecRankTerms =
        RankField (directProjection revisionField) Ascending
          :| [RankApplicationValue Ascending],
      descriptorSpecMinimumRetentionMicros = 1000,
      descriptorSpecImmutable = False,
      descriptorSpecLabelField = Nothing,
      descriptorSpecApplicationMutation = OrdinaryApplicationMutation,
      descriptorSpecStructuralCarrierRole = Nothing
    }

regularSchema :: ValueSchema
regularSchema =
  checked
    "regular schema"
    ( recordSchema
        [ (objectField, globalUniqueIdSchema),
          (revisionField, int64Schema),
          (obsoleteField, boolSchema),
          (payloadField, textSchema)
        ]
    )

regularValue :: Word8 -> Int64 -> Bool -> Text -> Value
regularValue objectByte revision obsolete payload =
  checked
    "regular value"
    ( recordValue
        [ (objectField, globalUniqueIdValue (fixturePublicationObject objectByte)),
          (revisionField, int64Value revision),
          (obsoleteField, boolValue obsolete),
          (payloadField, textValue payload)
        ]
    )

caseDescriptorRoundTrip :: IO ()
caseDescriptorRoundTrip =
  assertEqual
    "decoded descriptor"
    (Right regularDescriptor)
    (decodeCanonicalDescriptor ApplicationDescriptor (canonicalDescriptorBytes regularDescriptor))

caseEmptyKeyRoundTrip :: IO ()
caseEmptyKeyRoundTrip =
  assertEqual
    "decoded empty-key descriptor"
    (Right emptyKeyDescriptor)
    (decodeCanonicalDescriptor ApplicationDescriptor (canonicalDescriptorBytes emptyKeyDescriptor))

caseDescriptorConstructorCoverage :: IO ()
caseDescriptorConstructorCoverage = mapM_ assertRoundTrip specifications
  where
    specifications =
      (ApplicationDescriptor, coverageDescriptorSpec)
        : (ApplicationDescriptor, controlledDescriptorSpec)
        : [ ( PrimordialDescriptor,
              carrierDescriptorSpec
                role
                (if role == ProcessEpochCarrier then HeraldManagedMutation else OrdinaryApplicationMutation)
            )
          | role <- [minBound .. maxBound]
          ]
    assertRoundTrip (admission, specification) = do
      let descriptor = checked "coverage descriptor" (canonicalizeDescriptor admission specification)
      assertEqual
        ("round-trip " <> show (admission, descriptorSpecStructuralCarrierRole specification))
        (Right descriptor)
        (decodeCanonicalDescriptor admission (canonicalDescriptorBytes descriptor))

coverageDescriptorSpec :: DescriptorSpec
coverageDescriptorSpec =
  regularDescriptorSpec
    { descriptorSpecSchema = coverageSchema,
      descriptorSpecValidity =
        AllPredicates
          ( CompareField (directProjection boolField) ScalarEqual (LiteralBool True)
              :| [ CompareField (directProjection boolField) ScalarNotEqual (LiteralBool False),
                   CompareField (directProjection revisionField) ScalarLessThan (LiteralInt64 10),
                   CompareField (directProjection revisionField) ScalarLessThanOrEqual (LiteralInt64 10),
                   CompareField (directProjection revisionField) ScalarGreaterThan (LiteralInt64 0),
                   CompareField (directProjection revisionField) ScalarGreaterThanOrEqual (LiteralInt64 0),
                   CompareField (directProjection bytesField) ScalarEqual (LiteralBytes "bytes"),
                   CompareField (directProjection payloadField) ScalarNotEqual (LiteralText ""),
                   CompareField (directProjection enumField) ScalarEqual (LiteralEnum enumSecond),
                   NotPredicate (AnyPredicates (NeverPredicate :| [NeverPredicate]))
                 ]
          ),
      descriptorSpecObsolescence =
        AnyPredicates (NeverPredicate :| [NotPredicate AlwaysPredicate]),
      descriptorSpecRankTerms =
        RankField (directProjection payloadField) Descending
          :| [RankApplicationValue Descending]
    }

coverageSchema :: ValueSchema
coverageSchema =
  checked
    "coverage schema"
    ( recordSchema
        [ (objectField, globalUniqueIdSchema),
          (revisionField, int64Schema),
          (boolField, boolSchema),
          (bytesField, bytesSchema),
          (payloadField, textSchema),
          (enumField, enumMemberSchema),
          (labelField, labelSchema),
          (optionalField, optionalGlobalUniqueIdSchema)
        ]
    )

carrierDescriptorSpec :: StructuralCarrierRole -> ApplicationMutation -> DescriptorSpec
carrierDescriptorSpec role mutation =
  controlledDescriptorSpec
    { descriptorSpecObsolescence = NeverPredicate,
      descriptorSpecLabelField = Nothing,
      descriptorSpecApplicationMutation = mutation,
      descriptorSpecStructuralCarrierRole = Just role
    }

caseDescriptorGolden :: IO ()
caseDescriptorGolden =
  assertEqual
    "canonical descriptor hex"
    goldenDescriptorHex
    (renderHex (canonicalDescriptorBytes regularDescriptor))

goldenDescriptorHex :: String
goldenDescriptorHex =
  "45434c4950532d440006000000000000000400000000000000066f626a65637404000000"
    <> "00000000086f62736f6c6574650000000000000000077061796c6f616403000000000000"
    <> "00087265766973696f6e010000000000000001000000000000000100000000000000066f"
    <> "626a65637402000000000000000100000000000000087265766973696f6e050100000000"
    <> "0000000002000000000000000100000000000000086f62736f6c65746500000100000000"
    <> "0000000200000000000000000100000000000000087265766973696f6e00010000000000"
    <> "000003e800000000"

renderHex :: ByteString.ByteString -> String
renderHex = concatMap renderByte . ByteString.unpack
  where
    renderByte byte = case showHex byte "" of
      [digit] -> ['0', digit]
      digits -> digits

caseSortIdHash :: IO ()
caseSortIdHash =
  assertEqual
    "digest"
    (SHA256.hash (canonicalDescriptorBytes regularDescriptor))
    (sortIdBytes (descriptorSortId regularDescriptor))

caseClaimedSortId :: IO ()
caseClaimedSortId = do
  assertEqual
    "matching identity"
    (Right regularDescriptor)
    ( validateCanonicalDescriptorIdentity
        ApplicationDescriptor
        (descriptorSortId regularDescriptor)
        (canonicalDescriptorBytes regularDescriptor)
    )
  let otherDescriptor =
        checked
          "other descriptor"
          ( canonicalizeDescriptor
              ApplicationDescriptor
              regularDescriptorSpec {descriptorSpecMinimumRetentionMicros = 1001}
          )
  assertEqual
    "mismatched identity"
    ( Left
        ( DescriptorClaimedSortIdMismatch
            (descriptorSortId otherDescriptor)
            (descriptorSortId regularDescriptor)
        )
    )
    ( validateCanonicalDescriptorIdentity
        ApplicationDescriptor
        (descriptorSortId otherDescriptor)
        (canonicalDescriptorBytes regularDescriptor)
    )

caseDescriptorDomainTag :: IO ()
caseDescriptorDomainTag = do
  let bytes = canonicalDescriptorBytes regularDescriptor
  assertBool "descriptor is non-empty" (not (ByteString.null bytes))
  assertEqual "domain tag" "ECLIPS-D" (ByteString.take 8 bytes)

caseWrongDescriptorDomain :: IO ()
caseWrongDescriptorDomain =
  assertEqual
    "wrong domain"
    (Left (DescriptorBytesRejected DescriptorDecodeWrongDomain))
    ( decodeCanonicalDescriptor
        ApplicationDescriptor
        (ByteString.cons 0 (ByteString.drop 1 (canonicalDescriptorBytes regularDescriptor)))
    )

caseDecodedDescriptorAdmission :: IO ()
caseDecodedDescriptorAdmission =
  assertEqual
    "immutable regular descriptor"
    (Left (DescriptorAdmissionFailed RegularSortCannotBeImmutable))
    (decodeCanonicalDescriptor ApplicationDescriptor invalid)
  where
    bytes = canonicalDescriptorBytes regularDescriptor
    immutableOffset = ByteString.length bytes - 4
    invalid =
      ByteString.take immutableOffset bytes
        <> ByteString.singleton 1
        <> ByteString.drop (immutableOffset + 1) bytes

caseTrailingBytes :: IO ()
caseTrailingBytes =
  case decodeCanonicalDescriptor ApplicationDescriptor (canonicalDescriptorBytes regularDescriptor <> ByteString.singleton 0) of
    Left DescriptorEncodingNotCanonical -> pure ()
    other -> assertFailure ("unexpected result: " <> show other)

caseMalformedDescriptor :: IO ()
caseMalformedDescriptor =
  case decodeCanonicalDescriptor ApplicationDescriptor malformed of
    Left (DescriptorBytesRejected DescriptorDecodeMalformed) -> pure ()
    other -> assertFailure ("unexpected result: " <> show other)
  where
    bytes = canonicalDescriptorBytes regularDescriptor
    malformed = ByteString.take (ByteString.length bytes - 1) bytes

caseRecordOrder :: IO ()
caseRecordOrder = do
  let reverseSchema =
        checked
          "reverse schema"
          ( recordSchema
              [ (payloadField, textSchema),
                (obsoleteField, boolSchema),
                (revisionField, int64Schema),
                (objectField, globalUniqueIdSchema)
              ]
          )
      reverseSpec = regularDescriptorSpec {descriptorSpecSchema = reverseSchema}
      reverseDescriptor = checked "reverse descriptor" (canonicalizeDescriptor ApplicationDescriptor reverseSpec)
  assertEqual
    "canonical bytes"
    (canonicalDescriptorBytes regularDescriptor)
    (canonicalDescriptorBytes reverseDescriptor)

caseSignedExtrema :: IO ()
caseSignedExtrema =
  mapM_ assertRoundTrip [minBound, maxBound]
  where
    assertRoundTrip value =
      let encoded = canonicalValueByteString (canonicalValueBytes (int64Value value))
       in assertEqual ("round-trip " <> show value) (Right (int64Value value)) (decodeCanonicalValue encoded)

caseValueConstructorRoundTrips :: IO ()
caseValueConstructorRoundTrips = mapM_ assertRoundTrip values
  where
    process = checked "ProcessEpochId" (mkProcessEpochId (ByteString.replicate 32 9))
    values =
      [ boolValue True,
        int64Value (-7),
        bytesValue "bytes",
        textValue "text",
        globalUniqueIdValue (fixturePublicationObject 7),
        optionalGlobalUniqueIdValue Nothing,
        optionalGlobalUniqueIdValue (Just (fixturePublicationObject 8)),
        labelValue (VoidLabel, 0),
        labelValue ((ProcessLabel process, 0)),
        labelValue ((ZombieLabel process, 0)),
        enumValue enumSecond,
        checked
          "nested record"
          ( recordValue
              [ (boolField, boolValue False),
                (bytesField, bytesValue "nested")
              ]
          )
      ]
    assertRoundTrip value =
      assertEqual
        ("round-trip " <> show value)
        (Right value)
        (decodeCanonicalValue (canonicalValueByteString (canonicalValueBytes value)))

caseMalformedValue :: IO ()
caseMalformedValue =
  assertEqual
    "invalid cereal Bool"
    (Left ValueDecodeMalformed)
    (decodeCanonicalValue malformed)
  where
    encoded = canonicalValueByteString (canonicalValueBytes (boolValue False))
    malformed = ByteString.init encoded <> ByteString.singleton 2

caseWrongValueDomain :: IO ()
caseWrongValueDomain =
  assertEqual
    "wrong value domain"
    (Left (ValueDecodeUnknownDomainTag 0x00434c4950532d56))
    (decodeCanonicalValue wrongDomain)
  where
    encoded = canonicalValueByteString (canonicalValueBytes (boolValue False))
    wrongDomain = replaceByte 0 0 encoded

caseDecodedValueAdmission :: IO ()
caseDecodedValueAdmission = mapM_ assertRejected malformedValues
  where
    assertRejected (label, expected, bytes) =
      assertEqual label (Left expected) (decodeCanonicalValue bytes)
    malformedValues =
      [ ("invalid UTF-8", ValueDecodeInvalidUtf8, invalidUtf8),
        ("invalid field name", ValueDecodeInvalidFieldName (InvalidFieldNameStart '1'), invalidFieldName),
        ("duplicate field", DuplicateValueField (fixtureField "a"), duplicateField),
        ("31-byte global unique ID", ValueDecodeInvalidGlobalUniqueId, shortGlobalUniqueId),
        ("31-byte optional global unique ID", ValueDecodeInvalidGlobalUniqueId, shortOptionalGlobalUniqueId),
        ("31-byte process epoch", ValueDecodeInvalidProcessEpoch, shortProcessEpoch)
      ]
    invalidUtf8 =
      let encoded = canonicalValueByteString (canonicalValueBytes (textValue "x"))
       in replaceByte (ByteString.length encoded - 1) 0xff encoded
    invalidFieldName =
      replaceFirstByte 0x61 0x31
        . canonicalValueByteString
        . canonicalValueBytes
        $ checked "record" (recordValue [(fixtureField "a", boolValue False)])
    duplicateField =
      replaceFirstByte 0x7a 0x61
        . canonicalValueByteString
        . canonicalValueBytes
        $ checked
          "record"
          (recordValue [(fixtureField "a", boolValue False), (fixtureField "z", boolValue True)])
    shortGlobalUniqueId =
      shorten32BytePayload
        . canonicalValueByteString
        . canonicalValueBytes
        $ globalUniqueIdValue (fixturePublicationObject 7)
    shortOptionalGlobalUniqueId =
      shorten32BytePayload
        . canonicalValueByteString
        . canonicalValueBytes
        $ optionalGlobalUniqueIdValue (Just (fixturePublicationObject 7))
    shortProcessEpoch =
      shorten32BytePayload
        . canonicalValueByteString
        . canonicalValueBytes
        . labelValue
        . (,0)
        . ProcessLabel
        $ checked "ProcessEpochId" (mkProcessEpochId (ByteString.replicate 32 9))
    shorten32BytePayload = ByteString.init . replaceFirstByte 32 31

caseTrailingValueBytes :: IO ()
caseTrailingValueBytes =
  assertEqual
    "trailing byte"
    (Left ValueDecodeNonCanonicalEncoding)
    (decodeCanonicalValue (encoded <> ByteString.singleton 0))
  where
    encoded = canonicalValueByteString (canonicalValueBytes (boolValue False))

caseNonCanonicalValueOrder :: IO ()
caseNonCanonicalValueOrder =
  assertEqual
    "fields must be ascending"
    (Left ValueDecodeNonCanonicalEncoding)
    (decodeCanonicalValue reordered)
  where
    encoded =
      canonicalValueByteString . canonicalValueBytes
        $ checked
          "record"
          ( recordValue
              [ (fixtureField "z", boolValue True),
                (fixtureField "a", checked "inner record" (recordValue [(fixtureField "x", int64Value (-1))]))
              ]
          )
    reordered =
      ByteString.take 17 encoded
        <> ByteString.drop 53 encoded
        <> ByteString.take 36 (ByteString.drop 17 encoded)

replaceFirstByte :: Word8 -> Word8 -> ByteString.ByteString -> ByteString.ByteString
replaceFirstByte expected replacement bytes =
  case ByteString.elemIndex expected bytes of
    Nothing -> error "byte absent from test fixture"
    Just offset -> replaceByte offset replacement bytes

replaceByte :: Int -> Word8 -> ByteString.ByteString -> ByteString.ByteString
replaceByte offset replacement bytes =
  ByteString.take offset bytes
    <> ByteString.singleton replacement
    <> ByteString.drop (offset + 1) bytes

propValueRoundTrip :: Word8 -> Int64 -> Bool -> String -> Property
propValueRoundTrip objectByte revision obsolete rawPayload =
  let payload = Text.pack (take 128 rawPayload)
      value = regularValue objectByte revision obsolete payload
      bytes = canonicalValueByteString (canonicalValueBytes value)
   in counterexample (show bytes) (decodeCanonicalValue bytes === Right value)

caseRegularImmutable :: IO ()
caseRegularImmutable =
  assertEqual
    "immutable regular sort"
    (Left RegularSortCannotBeImmutable)
    (checkDescriptor ApplicationDescriptor regularDescriptorSpec {descriptorSpecImmutable = True})

caseRankMustBeTotal :: IO ()
caseRankMustBeTotal =
  assertEqual
    "missing final term"
    (Left RankMustEndInApplicationValue)
    ( checkDescriptor
        ApplicationDescriptor
        regularDescriptorSpec
          { descriptorSpecRankTerms =
              RankField (directProjection revisionField) Ascending :| []
          }
    )

casePredicateType :: IO ()
casePredicateType =
  case checkDescriptor ApplicationDescriptor badSpec of
    Left (PredicateLiteralTypeMismatch projection _ (LiteralText "wrong")) ->
      assertEqual "projection" (directProjection revisionField) projection
    other -> assertFailure ("unexpected result: " <> show other)
  where
    badSpec =
      regularDescriptorSpec
        { descriptorSpecValidity =
            CompareField
              (directProjection revisionField)
              ScalarEqual
              (LiteralText "wrong")
        }

caseEnumPredicateAdmission :: IO ()
caseEnumPredicateAdmission =
  assertEqual
    "undeclared enum literal"
    ( Left
        ( PredicateLiteralTypeMismatch
            (directProjection enumField)
            (EnumSchema enumMembers)
            (LiteralEnum enumMissing)
        )
    )
    ( checkDescriptor
        ApplicationDescriptor
        enumDescriptorSpec
          { descriptorSpecValidity =
              CompareField
                (directProjection enumField)
                ScalarEqual
                (LiteralEnum enumMissing)
          }
    )

caseControlledKey :: IO ()
caseControlledKey =
  mapM_
    ( \(context, projections) ->
        assertEqual
          context
          (Left ControlledKeyMustBeOneDirectGlobalUniqueIdField)
          ( checkDescriptor
              ApplicationDescriptor
              controlledDescriptorSpec
                { descriptorSpecKeyProjections = projections
                }
          )
    )
    [ ("empty controlled key", []),
      ("non-unique-ID controlled key", [directProjection revisionField]),
      ( "multiple controlled keys",
        [directProjection objectField, directProjection revisionField]
      )
    ]

caseControlledLabelPredicate :: IO ()
caseControlledLabelPredicate =
  case checkDescriptor
    ApplicationDescriptor
    controlledDescriptorSpec
      { descriptorSpecValidity =
          CompareField
            (directProjection labelField)
            ScalarEqual
            (LiteralBool False)
      } of
    Left (PredicateLiteralTypeMismatch projection _ (LiteralBool False)) ->
      assertEqual "label projection" (directProjection labelField) projection
    other -> assertFailure ("controlled label predicate was not rejected: " <> show other)

caseApplicationPolicy :: IO ()
caseApplicationPolicy =
  assertEqual
    "managed policy"
    (Left ApplicationCannotClaimHeraldManagedMutation)
    ( checkDescriptor
        ApplicationDescriptor
        controlledDescriptorSpec
          { descriptorSpecApplicationMutation = HeraldManagedMutation,
            descriptorSpecStructuralCarrierRole = Just ProcessEpochCarrier,
            descriptorSpecObsolescence = NeverPredicate
          }
    )

caseValidity :: IO ()
caseValidity =
  assertEqual
    "negative revision"
    (Left ValueValidityRejected)
    (evaluateValue (canonicalCheckedDescriptor regularDescriptor) (regularValue 1 (-1) False "x"))

caseLifecycle :: IO ()
caseLifecycle = do
  current <- evaluatedRegular (regularValue 1 1 False "x")
  obsolete <- evaluatedRegular (regularValue 1 1 True "x")
  assertEqual "current" Current (checkedInstanceLifecycle current)
  assertEqual "obsolete" Obsolete (checkedInstanceLifecycle obsolete)

caseRankField :: IO ()
caseRankField = do
  lower <- evaluatedRegular (regularValue 1 1 False "z")
  higher <- evaluatedRegular (regularValue 1 2 False "a")
  assertBool "rank order" (checkedInstanceRank lower < checkedInstanceRank higher)

caseEnumEvaluation :: IO ()
caseEnumEvaluation = do
  let checkedDescriptor =
        checked
          "enum descriptor"
          (checkDescriptor ApplicationDescriptor enumDescriptorSpec)
  first <- checkedIO "first enum value" (evaluateValue checkedDescriptor (enumRecord enumFirst "first"))
  second <- checkedIO "second enum value" (evaluateValue checkedDescriptor (enumRecord enumSecond "second"))
  repeatedSecond <- checkedIO "repeated second enum value" (evaluateValue checkedDescriptor (enumRecord enumSecond "other"))
  assertBool
    "rank follows declaration order rather than lexical symbol order"
    (checkedInstanceRank first < checkedInstanceRank second)
  assertBool "different enum members make different keys" (checkedInstanceKey first /= checkedInstanceKey second)
  assertEqual "equal enum members make equal keys" (checkedInstanceKey second) (checkedInstanceKey repeatedSecond)
  let predicateDescriptor =
        checked
          "enum predicate descriptor"
          ( checkDescriptor
              ApplicationDescriptor
              enumDescriptorSpec
                { descriptorSpecValidity =
                    CompareField
                      (directProjection enumField)
                      ScalarLessThan
                      (LiteralEnum enumSecond)
                }
          )
  assertEqual
    "the first declared symbol satisfies an ordinal predicate"
    (Right ())
    (() <$ evaluateValue predicateDescriptor (enumRecord enumFirst "accepted"))
  assertEqual
    "the literal itself does not satisfy strict ordering"
    (Left ValueValidityRejected)
    (evaluateValue predicateDescriptor (enumRecord enumSecond "rejected"))

caseRankTieBreak :: IO ()
caseRankTieBreak = do
  left <- evaluatedRegular (regularValue 1 1 False "a")
  right <- evaluatedRegular (regularValue 1 1 False "b")
  assertBool "full value breaks tie" (checkedInstanceRank left /= checkedInstanceRank right)

caseObjectKey :: IO ()
caseObjectKey = do
  left <- evaluatedRegular (regularValue 1 1 False "a")
  right <- evaluatedRegular (regularValue 1 99 True "b")
  assertEqual "object key" (checkedInstanceKey left) (checkedInstanceKey right)

caseEmptyObjectKey :: IO ()
caseEmptyObjectKey = do
  left <- evaluatedEmptyKeyRegular (regularValue 1 1 False "left")
  right <- evaluatedEmptyKeyRegular (regularValue 2 99 True "right")
  assertEqual "unit object key" (checkedInstanceKey left) (checkedInstanceKey right)

caseControlledKeyProjection :: IO ()
caseControlledKeyProjection = do
  let controlled = checked "controlled descriptor" (checkDescriptor ApplicationDescriptor controlledDescriptorSpec)
      regular = canonicalCheckedDescriptor regularDescriptor
  assertEqual
    "controlled projection"
    (Just (directProjection objectField))
    (descriptorControlledKeyProjection controlled)
  assertEqual "regular descriptor" Nothing (descriptorControlledKeyProjection regular)

caseLabelRankExclusion :: IO ()
caseLabelRankExclusion = do
  let descriptor = checked "controlled descriptor" (canonicalizeDescriptor ApplicationDescriptor controlledDescriptorSpec)
      checkedDescriptor = canonicalCheckedDescriptor descriptor
      left = controlledValue (VoidLabel, 0)
      right =
        controlledValue
          ((ProcessLabel (checked "ProcessEpochId" (mkProcessEpochId (ByteString.replicate 32 7))), 0))
  evaluatedLeft <- checkedIO "left controlled value" (evaluateValue checkedDescriptor left)
  evaluatedRight <- checkedIO "right controlled value" (evaluateValue checkedDescriptor right)
  assertEqual "rank" (checkedInstanceRank evaluatedLeft) (checkedInstanceRank evaluatedRight)

casePolicyProjection :: IO ()
casePolicyProjection = do
  let descriptor = checked "controlled descriptor" (checkDescriptor ApplicationDescriptor controlledDescriptorSpec)
  case controlledPolicyProjection descriptor of
    Nothing -> assertFailure "controlled policy missing"
    Just _ -> pure ()

controlledDescriptorSpec :: DescriptorSpec
controlledDescriptorSpec =
  regularDescriptorSpec
    { descriptorSpecKind = ControlledSort,
      descriptorSpecSchema = controlledSchema,
      descriptorSpecValidity = AlwaysPredicate,
      descriptorSpecObsolescence = NeverPredicate,
      descriptorSpecRankTerms = RankApplicationValue Ascending :| [],
      descriptorSpecLabelField = Just labelField
    }

controlledSchema :: ValueSchema
controlledSchema =
  checked
    "controlled schema"
    ( recordSchema
        [ (objectField, globalUniqueIdSchema),
          (revisionField, int64Schema),
          (obsoleteField, boolSchema),
          (payloadField, textSchema),
          (labelField, labelSchema)
        ]
    )

controlledValue :: Label -> Value
controlledValue label =
  checked
    "controlled value"
    ( recordValue
        [ (objectField, globalUniqueIdValue (fixturePublicationObject 1)),
          (revisionField, int64Value 1),
          (obsoleteField, boolValue False),
          (payloadField, textValue "payload"),
          (labelField, labelValue label)
        ]
    )

evaluatedRegular :: Value -> IO CheckedInstance
evaluatedRegular value =
  checkedIO
    "regular evaluation"
    (evaluateValue (canonicalCheckedDescriptor regularDescriptor) value)

evaluatedEmptyKeyRegular :: Value -> IO CheckedInstance
evaluatedEmptyKeyRegular value =
  checkedIO
    "empty-key regular evaluation"
    (evaluateValue (canonicalCheckedDescriptor emptyKeyDescriptor) value)

enumDescriptorSpec :: DescriptorSpec
enumDescriptorSpec =
  DescriptorSpec
    { descriptorSpecKind = RegularSort,
      descriptorSpecSchema =
        checked
          "enum record schema"
          ( recordSchema
              [ (enumField, enumMemberSchema),
                (payloadField, textSchema)
              ]
          ),
      descriptorSpecKeyProjections = [directProjection enumField],
      descriptorSpecValidity = AlwaysPredicate,
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

enumMemberSchema :: ValueSchema
enumMemberSchema = checked "enum schema" (enumSchema enumMembers)

enumMembers :: NonEmpty EnumSymbol
enumMembers = enumFirst :| [enumSecond, enumThird]

enumFirst, enumSecond, enumThird, enumMissing :: EnumSymbol
enumFirst = enumSymbol "zeta"
enumSecond = enumSymbol "alpha"
enumThird = enumSymbol "middle"
enumMissing = enumSymbol "missing"

enumRecord :: EnumSymbol -> Text -> Value
enumRecord member payload =
  checked
    "enum value"
    ( recordValue
        [ (enumField, enumValue member),
          (payloadField, textValue payload)
        ]
    )

fixturePublicationObject :: Word8 -> GlobalUniqueId
fixturePublicationObject byte =
  checked "GlobalUniqueId" (mkGlobalUniqueId (ByteString.replicate 32 byte))

objectField, revisionField, obsoleteField, payloadField, labelField, boolField, bytesField, enumField, optionalField :: FieldName
objectField = fixtureField "object"
revisionField = fixtureField "revision"
obsoleteField = fixtureField "obsolete"
payloadField = fixtureField "payload"
labelField = fixtureField "label"
boolField = fixtureField "flag"
bytesField = fixtureField "bytes"
enumField = fixtureField "enum"
optionalField = fixtureField "optional"

fixtureField :: String -> FieldName
fixtureField = checked "FieldName" . mkFieldName . Text.pack

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (error . ((label <> ": ") <>) . show) id

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO label = either (assertFailure . ((label <> ": ") <>) . show) pure
