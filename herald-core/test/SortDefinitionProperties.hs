{-# LANGUAGE OverloadedStrings #-}

module SortDefinitionProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Word (Word8)
import Eclips.Application.Types.Identity (SortId, mkSortId)
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (..),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending, Descending),
    ApplicationRankTerm (RankApplicationValue, RankField),
    ApplicationScalarLiteral
      ( LiteralBool,
        LiteralBytes,
        LiteralEnum,
        LiteralInt64,
        LiteralText
      ),
    ApplicationSortDefinition (..),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (ControlledSort, RegularSort),
    ApplicationValueSchema
      ( BoolSchema,
        BytesSchema,
        EnumSchema,
        Int64Schema,
        LabelSchema,
        OptionalUniqueIdSchema,
        RecordSchema,
        TextSchema,
        UniqueIdSchema
      ),
  )
import Eclips.Application.Types.Write (WriteResult (SortDefinitionWritten))
import Eclips.Domain.Sort.Canonical
  ( canonicalizeCheckedDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( ApplicationMutation (OrdinaryApplicationMutation),
    DescriptorAdmission (ApplicationDescriptor),
    DescriptorError (RankMustEndInApplicationValue),
    DescriptorSpec (..),
    checkDescriptor,
  )
import Eclips.Domain.Sort.Profile (sortDefinitionValue)
import Eclips.Domain.Value qualified as DomainValue
import Eclips.Herald.Application.SortDefinition
  ( ApplicationSortDefinitionRejection (..),
    admitApplicationSortDefinition,
    admittedApplicationSortId,
    admittedApplicationSortValue,
    admittedApplicationSortWriteResult,
    applicationDescriptorSpec,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    arbitrary,
    conjoin,
    counterexample,
    elements,
    forAll,
    frequency,
    listOf,
    oneof,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "structured sort definitions"
    [ testProperty "adapter agrees with semantic admission for generated drafts" propSemanticAgreement,
      testCase "adapter agrees with direct semantic admission" caseSemanticAgreement,
      testCase "an empty regular key list crosses the adapter" caseEmptyKey,
      testCase "an equal public SortId claim succeeds" caseEqualClaim,
      testCase "a different public SortId claim rejects" caseDifferentClaim,
      testCase "invalid textual fields reject at the adapter" caseInvalidField,
      testCase "duplicate enum members reject at the adapter" caseDuplicateEnum,
      testCase "semantic descriptor errors remain semantic rejections" caseSemanticRejection,
      testCase "controlled application descriptors remain available" caseControlledDescriptor,
      testCase "applications can claim neither mutation nor carrier roles" caseFixedInternalFacts
    ]

caseSemanticAgreement :: IO ()
caseSemanticAgreement = do
  let definition = regularDefinition Nothing
      descriptor = regularDescriptor
  specification <- checked "application descriptor translation" (applicationDescriptorSpec descriptor)
  semanticChecked <- checked "direct semantic admission" (checkDescriptor ApplicationDescriptor specification)
  admitted <- checked "structured definition admission" (admitApplicationSortDefinition definition)
  let canonical = canonicalizeCheckedDescriptor semanticChecked
      expectedSortId = descriptorSortId canonical
  assertEqual
    "the domain alone determines the content address"
    expectedSortId
    (admittedApplicationSortId admitted)
  assertEqual
    "the adapter constructs the ordinary semantic carrier"
    (sortDefinitionValue canonical)
    (admittedApplicationSortValue admitted)
  assertEqual
    "the successful branch carries that exact public identity"
    (SortDefinitionWritten expectedSortId)
    (admittedApplicationSortWriteResult admitted)

caseEmptyKey :: IO ()
caseEmptyKey = do
  let descriptor = regularDescriptor {keyProjections = []}
  specification <- checked "empty-key descriptor translation" (applicationDescriptorSpec descriptor)
  assertEqual "translated empty key" [] (descriptorSpecKeyProjections specification)
  _ <-
    checked
      "empty-key definition admission"
      (admitApplicationSortDefinition (DeclaredSortDefinition descriptor Nothing))
  pure ()

caseEqualClaim :: IO ()
caseEqualClaim = do
  withoutClaim <-
    checked
      "unclaimed definition"
      (admitApplicationSortDefinition (regularDefinition Nothing))
  let computed = admittedApplicationSortId withoutClaim
  withClaim <-
    checked
      "equal claimed identity"
      (admitApplicationSortDefinition (regularDefinition (Just computed)))
  assertEqual "claim does not replace the computed identity" computed (admittedApplicationSortId withClaim)

caseDifferentClaim :: IO ()
caseDifferentClaim = do
  admitted <-
    checked
      "unclaimed definition"
      (admitApplicationSortDefinition (regularDefinition Nothing))
  firstCandidate <- sortIdFilled 1
  secondCandidate <- sortIdFilled 2
  let computed = admittedApplicationSortId admitted
      different = if firstCandidate /= computed then firstCandidate else secondCandidate
  assertEqual
    "a caller cannot override the domain-derived identity"
    (Left (ApplicationDescriptorClaimedSortIdMismatch different computed))
    (admitApplicationSortDefinition (regularDefinition (Just different)))

caseInvalidField :: IO ()
caseInvalidField = do
  let descriptor =
        regularDescriptor
          { valueSchema =
              RecordSchema (Map.singleton "not-valid" TextSchema)
          }
  case applicationDescriptorSpec descriptor of
    Left (ApplicationDescriptorInvalidFieldName name _) ->
      assertEqual "the private draft field is reported" "not-valid" name
    result -> fail ("expected invalid field rejection, got " <> show result)

caseDuplicateEnum :: IO ()
caseDuplicateEnum = do
  let descriptor =
        regularDescriptor
          { valueSchema = EnumSchema ("same" :| ["same"]),
            keyProjections = []
          }
  case applicationDescriptorSpec descriptor of
    Left (ApplicationDescriptorInvalidSchema (DomainValue.DuplicateEnumSymbol symbol)) ->
      assertEqual "duplicate symbol" "same" (DomainValue.enumSymbolText symbol)
    result -> fail ("expected duplicate enum rejection, got " <> show result)

caseSemanticRejection :: IO ()
caseSemanticRejection = do
  let descriptor =
        regularDescriptor
          { rankTerms =
              RankField keyProjection Ascending :| []
          }
      definition = DeclaredSortDefinition descriptor Nothing
  assertEqual
    "rank admission stays in the semantic domain"
    (Left (ApplicationDescriptorRejected RankMustEndInApplicationValue))
    (admitApplicationSortDefinition definition)

caseControlledDescriptor :: IO ()
caseControlledDescriptor = do
  admitted <-
    checked
      "controlled application descriptor"
      ( admitApplicationSortDefinition
          (DeclaredSortDefinition controlledDescriptor Nothing)
      )
  assertEqual
    "controlled declarations return the same dedicated success branch"
    (SortDefinitionWritten (admittedApplicationSortId admitted))
    (admittedApplicationSortWriteResult admitted)

caseFixedInternalFacts :: IO ()
caseFixedInternalFacts = do
  specification <-
    checked
      "application descriptor translation"
      (applicationDescriptorSpec regularDescriptor)
  assertEqual
    "mutation is always the ordinary application mode"
    OrdinaryApplicationMutation
    (descriptorSpecApplicationMutation specification)
  assertEqual
    "no structural carrier can be selected by the application AST"
    Nothing
    (descriptorSpecStructuralCarrierRole specification)

regularDefinition :: Maybe SortId -> ApplicationSortDefinition
regularDefinition claimed = DeclaredSortDefinition regularDescriptor claimed

regularDescriptor :: ApplicationSortDescriptor
regularDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema =
        RecordSchema
          ( Map.fromList
              [ ("key", TextSchema),
                ("payload", UniqueIdSchema)
              ]
          ),
      keyProjections = [keyProjection],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 17,
      isImmutable = False,
      labelField = Nothing
    }

controlledDescriptor :: ApplicationSortDescriptor
controlledDescriptor =
  ApplicationSortDescriptor
    { sortKind = ControlledSort,
      valueSchema =
        RecordSchema
          ( Map.fromList
              [ ("object", UniqueIdSchema),
                ("label", LabelSchema),
                ("payload", TextSchema)
              ]
          ),
      keyProjections = [objectProjection],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Just "label"
    }

keyProjection :: ApplicationProjection
keyProjection = ApplicationProjection ("key" :| [])

objectProjection :: ApplicationProjection
objectProjection = ApplicationProjection ("object" :| [])

sortIdFilled :: Word8 -> IO SortId
sortIdFilled byte = checked "public sort identity" (mkSortId (ByteString.replicate 32 byte))

propSemanticAgreement :: Property
propSemanticAgreement =
  forAll genRegularDescriptor $ \descriptor ->
    case applicationDescriptorSpec descriptor of
      Left problem -> counterexample ("translation rejected generated descriptor: " <> show problem) False
      Right specification ->
        case checkDescriptor ApplicationDescriptor specification of
          Left problem -> counterexample ("semantic admission rejected generated descriptor: " <> show problem) False
          Right semanticChecked ->
            case admitApplicationSortDefinition (DeclaredSortDefinition descriptor Nothing) of
              Left problem -> counterexample ("adapter rejected generated descriptor: " <> show problem) False
              Right admitted ->
                let canonical = canonicalizeCheckedDescriptor semanticChecked
                    expectedSortId = descriptorSortId canonical
                    claimedResult =
                      admitApplicationSortDefinition
                        (DeclaredSortDefinition descriptor (Just expectedSortId))
                 in conjoin
                      [ admittedApplicationSortId admitted === expectedSortId,
                        admittedApplicationSortValue admitted === sortDefinitionValue canonical,
                        admittedApplicationSortWriteResult admitted
                          === SortDefinitionWritten expectedSortId,
                        fmap admittedApplicationSortId claimedResult === Right expectedSortId
                      ]

genRegularDescriptor :: Gen ApplicationSortDescriptor
genRegularDescriptor = do
  (keySchema, keyLiteral) <- genScalar
  payloadSchema <- genSchema 3
  validity <- genPredicate 3 keyLiteral
  obsolescence <- genPredicate 3 keyLiteral
  rankDirection <- elements [Ascending, Descending]
  includeKey <- arbitrary
  includeKeyRank <- arbitrary
  minimumRetention <- arbitrary
  let rank =
        if includeKeyRank
          then RankField keyProjection rankDirection :| [RankApplicationValue rankDirection]
          else RankApplicationValue rankDirection :| []
  pure
    regularDescriptor
      { valueSchema =
          RecordSchema
            (Map.fromList [("key", keySchema), ("payload", payloadSchema)]),
        keyProjections = if includeKey then [keyProjection] else [],
        validityPredicate = validity,
        obsolescencePredicate = obsolescence,
        rankTerms = rank,
        minimumRetentionMicros = minimumRetention
      }

genSchema :: Int -> Gen ApplicationValueSchema
genSchema depth
  | depth <= 0 = genScalarSchema
  | otherwise =
      frequency
        [ (4, genScalarSchema),
          ( 1,
            RecordSchema . Map.singleton "nested"
              <$> genSchema (depth - 1)
          )
        ]

genScalarSchema :: Gen ApplicationValueSchema
genScalarSchema =
  oneof
    [ elements
        [ BoolSchema,
          Int64Schema,
          BytesSchema,
          TextSchema,
          UniqueIdSchema,
          LabelSchema,
          OptionalUniqueIdSchema
        ],
      pure (EnumSchema ("zeta" :| ["alpha"]))
    ]

genScalar :: Gen (ApplicationValueSchema, ApplicationScalarLiteral)
genScalar =
  elements [0 :: Word8, 1, 2, 3, 4] >>= \case
    0 -> do
      value <- arbitrary
      pure (BoolSchema, LiteralBool value)
    1 -> do
      value <- arbitrary :: Gen Int64
      pure (Int64Schema, LiteralInt64 value)
    2 -> do
      value <- ByteString.pack <$> listOf arbitrary
      pure (BytesSchema, LiteralBytes value)
    3 -> do
      value <- Text.pack <$> listOf (elements ['a' .. 'z'])
      pure (TextSchema, LiteralText value)
    _ -> pure (EnumSchema ("zeta" :| ["alpha"]), LiteralEnum "alpha")

genPredicate ::
  Int ->
  ApplicationScalarLiteral ->
  Gen ApplicationPredicateExpression
genPredicate depth literal
  | depth <= 0 = genPredicateLeaf literal
  | otherwise =
      frequency
        [ (4, genPredicateLeaf literal),
          (1, NotPredicate <$> genPredicate (depth - 1) literal),
          ( 1,
            AllPredicates
              <$> ( (:|)
                      <$> genPredicate (depth - 1) literal
                      <*> ((: []) <$> genPredicate (depth - 1) literal)
                  )
          ),
          ( 1,
            AnyPredicates
              <$> ( (:|)
                      <$> genPredicate (depth - 1) literal
                      <*> ((: []) <$> genPredicate (depth - 1) literal)
                  )
          )
        ]

genPredicateLeaf ::
  ApplicationScalarLiteral ->
  Gen ApplicationPredicateExpression
genPredicateLeaf literal = do
  comparison <- elements [minBound .. maxBound]
  elements
    [ AlwaysPredicate,
      NeverPredicate,
      CompareField keyProjection comparison literal
    ]

checked :: (Show problem) => String -> Either problem value -> IO value
checked description =
  either (fail . ((description <> ": ") <>) . show) pure
