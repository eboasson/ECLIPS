{-# LANGUAGE OverloadedStrings #-}

module QueryProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Word (Word8)
import Eclips.Domain.Identity
  ( GlobalUniqueId,
    ProcessEpochId,
    mkGlobalUniqueId,
    mkProcessEpochId,
  )
import Eclips.Domain.Query
  ( QueryAdmissionError (..),
    QueryEvaluationError (QueryValueContradictsCheckedSchema),
    QueryLiteral (..),
    QueryPredicate (..),
    checkQueryPredicate,
    matchesQueryPredicate,
  )
import Eclips.Domain.Sort.Descriptor (ScalarComparison (..))
import Eclips.Domain.Value
  ( EnumSymbol,
    FieldName,
    LabelOwner (..),
    SchemaError (SchemaProjectionMissing),
    SchemaView (..),
    Value,
    ValueSchema,
    boolSchema,
    boolValue,
    bytesSchema,
    bytesValue,
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
    recordSchema,
    recordValue,
    textSchema,
    textValue,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "checked query predicates"
    [ testCase "every scalar literal is admitted and evaluated" caseEveryScalarLiteral,
      testCase "every scalar kind uses its semantic order" caseScalarOrdering,
      testCase "label queries compare the complete owner and generation" $ do
        assertMatches False (equals labelField (QueryLabel (ProcessLabel fixtureProcess, 1))) fixtureValue
        assertMatches True (lessThan labelField (QueryLabel (ProcessLabel fixtureProcess, 1))) fixtureValue,
      testCase "nested Boolean predicates are total" caseBooleanComposition,
      testCase "missing and record projections are rejected" caseProjectionAdmission,
      testCase "literal types must match projected schemas" caseLiteralAdmission,
      testCase "enum query literals must be declared members" caseEnumLiteralAdmission,
      testCase "arbitrary mismatched values return an evaluator contradiction" caseEvaluationMismatch,
      testProperty "all six integer comparisons have their ordinary meaning" propIntegerComparison,
      testCase "all six enum comparisons use declaration order" caseEnumComparisons
    ]

caseEveryScalarLiteral :: IO ()
caseEveryScalarLiteral =
  mapM_
    (\predicate -> assertMatches True predicate fixtureValue)
    [ equals boolField (QueryBool True),
      equals intField (QueryInt64 7),
      equals bytesField (QueryBytes "bytes"),
      equals textField (QueryText "text"),
      equals uniqueField (QueryGlobalUniqueId fixtureUniqueId),
      equals labelField (QueryLabel ((ProcessLabel fixtureProcess, 0))),
      equals enumField (QueryEnum enumSecond)
    ]

caseScalarOrdering :: IO ()
caseScalarOrdering =
  mapM_
    (\predicate -> assertMatches True predicate fixtureValue)
    [ greaterThan boolField (QueryBool False),
      greaterThan intField (QueryInt64 6),
      greaterThan bytesField (QueryBytes "bytea"),
      greaterThan textField (QueryText "tes"),
      greaterThan uniqueField (QueryGlobalUniqueId (globalUniqueIdFilled 2)),
      greaterThan labelField (QueryLabel (VoidLabel, 0)),
      greaterThan labelField (QueryLabel ((ProcessLabel (processEpochFilled 3), 0))),
      lessThan labelField (QueryLabel ((ZombieLabel fixtureProcess, 0))),
      greaterThan enumField (QueryEnum enumFirst)
    ]

caseBooleanComposition :: IO ()
caseBooleanComposition =
  assertMatches
    True
    ( QueryAll
        ( equals boolField (QueryBool True)
            :| [ QueryNot (equals intField (QueryInt64 8)),
                 QueryAny
                   ( QueryNever
                       :| [equals textField (QueryText "text")]
                   )
               ]
        )
    )
    fixtureValue

caseProjectionAdmission :: IO ()
caseProjectionAdmission = do
  let missing = fixtureField "missing"
      missingProjection = directProjection missing
      recordProjection = directProjection nestedField
  assertEqual
    "missing field"
    ( Left
        ( QueryProjectionInvalid
            missingProjection
            (SchemaProjectionMissing missing)
        )
    )
    (checkQueryPredicate fixtureSchema (equals missing (QueryBool True)))
  assertEqual
    "record field"
    (Left (QueryProjectionMustBeScalar recordProjection))
    ( checkQueryPredicate
        fixtureSchema
        (QueryCompare recordProjection ScalarEqual (QueryText "not a record"))
    )
  assertMatches
    True
    ( QueryCompare
        (mkProjection (nestedField :| [nestedTextField]))
        ScalarEqual
        (QueryText "nested")
    )
    fixtureValue

caseLiteralAdmission :: IO ()
caseLiteralAdmission =
  assertEqual
    "wrong literal kind"
    ( Left
        ( QueryLiteralTypeMismatch
            (directProjection intField)
            Int64Schema
            (QueryText "seven")
        )
    )
    (checkQueryPredicate fixtureSchema (equals intField (QueryText "seven")))

caseEnumLiteralAdmission :: IO ()
caseEnumLiteralAdmission =
  assertEqual
    "undeclared enum member"
    ( Left
        ( QueryLiteralTypeMismatch
            (directProjection enumField)
            (EnumSchema enumMembers)
            (QueryEnum enumMissing)
        )
    )
    (checkQueryPredicate fixtureSchema (equals enumField (QueryEnum enumMissing)))

caseEvaluationMismatch :: IO ()
caseEvaluationMismatch = do
  let checkedPredicate =
        checked
          "query predicate"
          (checkQueryPredicate fixtureSchema QueryAlways)
  case matchesQueryPredicate checkedPredicate (boolValue True) of
    Left (QueryValueContradictsCheckedSchema _) -> pure ()
    other -> fail ("unexpected evaluator result: " <> show other)

propIntegerComparison :: Int64 -> Int64 -> Property
propIntegerComparison left right =
  conjoin
    [ counterexample (show (left, comparison, right))
        $ result comparison === Right (expected comparison)
    | comparison <- [minBound .. maxBound]
    ]
  where
    schema = checked "integer schema" (recordSchema [(intField, int64Schema)])
    value = checked "integer value" (recordValue [(intField, int64Value left)])
    result comparison =
      matchesQueryPredicate
        ( checked
            "integer query"
            ( checkQueryPredicate
                schema
                (QueryCompare (directProjection intField) comparison (QueryInt64 right))
            )
        )
        value
    expected comparison = case comparison of
      ScalarEqual -> left == right
      ScalarNotEqual -> left /= right
      ScalarLessThan -> left < right
      ScalarLessThanOrEqual -> left <= right
      ScalarGreaterThan -> left > right
      ScalarGreaterThanOrEqual -> left >= right

caseEnumComparisons :: IO ()
caseEnumComparisons =
  mapM_
    ( \(comparison, expected) ->
        assertMatches
          expected
          ( QueryCompare
              (directProjection enumField)
              comparison
              (QueryEnum enumFirst)
          )
          fixtureValue
    )
    [ (ScalarEqual, False),
      (ScalarNotEqual, True),
      (ScalarLessThan, False),
      (ScalarLessThanOrEqual, False),
      (ScalarGreaterThan, True),
      (ScalarGreaterThanOrEqual, True)
    ]

assertMatches :: Bool -> QueryPredicate -> Value -> IO ()
assertMatches expected predicate value = do
  let checkedPredicate =
        checked "query predicate" (checkQueryPredicate fixtureSchema predicate)
  assertEqual
    "predicate result"
    (Right expected)
    (matchesQueryPredicate checkedPredicate value)

equals :: FieldName -> QueryLiteral -> QueryPredicate
equals field = QueryCompare (directProjection field) ScalarEqual

greaterThan :: FieldName -> QueryLiteral -> QueryPredicate
greaterThan field = QueryCompare (directProjection field) ScalarGreaterThan

lessThan :: FieldName -> QueryLiteral -> QueryPredicate
lessThan field = QueryCompare (directProjection field) ScalarLessThan

fixtureSchema :: ValueSchema
fixtureSchema =
  checked
    "query fixture schema"
    ( recordSchema
        [ (boolField, boolSchema),
          (intField, int64Schema),
          (bytesField, bytesSchema),
          (textField, textSchema),
          (uniqueField, globalUniqueIdSchema),
          (labelField, labelSchema),
          (enumField, enumMemberSchema),
          ( nestedField,
            checked
              "nested schema"
              (recordSchema [(nestedTextField, textSchema)])
          )
        ]
    )

fixtureValue :: Value
fixtureValue =
  checked
    "query fixture value"
    ( recordValue
        [ (boolField, boolValue True),
          (intField, int64Value 7),
          (bytesField, bytesValue "bytes"),
          (textField, textValue "text"),
          (uniqueField, globalUniqueIdValue fixtureUniqueId),
          (labelField, labelValue ((ProcessLabel fixtureProcess, 0))),
          (enumField, enumValue enumSecond),
          ( nestedField,
            checked
              "nested value"
              (recordValue [(nestedTextField, textValue "nested")])
          )
        ]
    )

boolField, intField, bytesField, textField, uniqueField, labelField, enumField :: FieldName
boolField = fixtureField "bool"
intField = fixtureField "int"
bytesField = fixtureField "bytes"
textField = fixtureField "text"
uniqueField = fixtureField "identity"
labelField = fixtureField "label"
enumField = fixtureField "enum"

enumMemberSchema :: ValueSchema
enumMemberSchema = checked "enum schema" (enumSchema enumMembers)

enumMembers :: NonEmpty EnumSymbol
enumMembers = enumFirst :| [enumSecond, enumThird]

enumFirst, enumSecond, enumThird, enumMissing :: EnumSymbol
enumFirst = enumSymbol "zeta"
enumSecond = enumSymbol "alpha"
enumThird = enumSymbol "middle"
enumMissing = enumSymbol "missing"

nestedField, nestedTextField :: FieldName
nestedField = fixtureField "nested"
nestedTextField = fixtureField "value"

fixtureField :: Text -> FieldName
fixtureField name = checked "field" (mkFieldName name)

fixtureUniqueId :: GlobalUniqueId
fixtureUniqueId = globalUniqueIdFilled 3

globalUniqueIdFilled :: Word8 -> GlobalUniqueId
globalUniqueIdFilled byte =
  checked "global unique ID" (mkGlobalUniqueId (ByteString.replicate 32 byte))

fixtureProcess :: ProcessEpochId
fixtureProcess = processEpochFilled 4

processEpochFilled :: Word8 -> ProcessEpochId
processEpochFilled byte =
  checked "process epoch" (mkProcessEpochId (ByteString.replicate 32 byte))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
