-- | Descriptor-checked local query predicates.
--
-- Raw query syntax is first-order data. A predicate becomes executable only
-- after every projection and literal has been checked against a value schema.
-- The evaluator remains total when handed an arbitrary value: a contradiction
-- with the admitted schema is returned explicitly rather than hidden in a
-- partial projection.
module Eclips.Domain.Query
  ( ScalarComparison (..),
    QueryLiteral (..),
    QueryPredicate (..),
    CheckedQueryPredicate,
    QueryAdmissionError (..),
    QueryEvaluationError (..),
    checkQueryPredicate,
    matchesQueryPredicate,
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Eclips.Domain.Identity (GlobalUniqueId)
import Eclips.Domain.Sort.Descriptor (ScalarComparison (..))
import Eclips.Domain.Value
  ( EnumSymbol,
    Label,
    Projection,
    SchemaError,
    SchemaView (..),
    Value,
    ValueError,
    ValueSchema,
    ValueView (..),
    checkValueSchema,
    enumMemberOrdinal,
    schemaAt,
    valueAt,
    viewSchema,
    viewValue,
  )

-- | Every scalar kind that may be selected by a query.
--
-- This is intentionally broader than descriptor predicate literals: a query may
-- compare globalized unique identities and process labels.
data QueryLiteral
  = QueryBool Bool
  | QueryInt64 Int64
  | QueryBytes ByteString
  | QueryText Text
  | QueryGlobalUniqueId GlobalUniqueId
  | QueryLabel Label
  | QueryEnum EnumSymbol
  deriving stock (Eq, Ord, Show)

-- | Raw Boolean query syntax over non-empty field projections.
data QueryPredicate
  = QueryAlways
  | QueryNever
  | QueryCompare Projection ScalarComparison QueryLiteral
  | QueryNot QueryPredicate
  | QueryAll (NonEmpty QueryPredicate)
  | QueryAny (NonEmpty QueryPredicate)
  deriving stock (Eq, Show)

-- | One predicate whose complete projection/literal tree was admitted against
-- the retained schema.
data CheckedQueryPredicate
  = CheckedQueryPredicate ValueSchema QueryPredicate
  deriving stock (Eq, Show)

-- | A raw query expression is not meaningful for the selected descriptor.
data QueryAdmissionError
  = QueryProjectionInvalid Projection SchemaError
  | QueryProjectionMustBeScalar Projection
  | QueryLiteralTypeMismatch Projection SchemaView QueryLiteral
  deriving stock (Eq, Show)

-- | An evaluator input contradicts the schema retained by a checked predicate.
--
-- These branches are invariant diagnostics for callers evaluating already
-- descriptor-checked publications, while keeping the exported function total for
-- arbitrary values.
data QueryEvaluationError
  = QueryValueContradictsCheckedSchema ValueError
  | QueryProjectionContradictsCheckedSchema Projection ValueError
  | QueryProjectionContradictsScalarAdmission Projection ValueView
  deriving stock (Eq, Show)

-- | Admit every projection and literal in one predicate against a descriptor's
-- value schema.
checkQueryPredicate ::
  ValueSchema ->
  QueryPredicate ->
  Either QueryAdmissionError CheckedQueryPredicate
checkQueryPredicate schema predicate = do
  checkPredicate schema predicate
  Right (CheckedQueryPredicate schema predicate)

-- | Evaluate an admitted predicate against one value.
--
-- Supplying a value admitted through the same descriptor cannot produce an
-- error. Supplying arbitrary data still returns an explicit contradiction rather
-- than invoking a partial projection.
matchesQueryPredicate ::
  CheckedQueryPredicate ->
  Value ->
  Either QueryEvaluationError Bool
matchesQueryPredicate (CheckedQueryPredicate schema predicate) value = do
  either (Left . QueryValueContradictsCheckedSchema) Right (checkValueSchema schema value)
  evaluatePredicate schema value predicate

checkPredicate ::
  ValueSchema ->
  QueryPredicate ->
  Either QueryAdmissionError ()
checkPredicate schema = \case
  QueryAlways -> Right ()
  QueryNever -> Right ()
  QueryCompare projection _ literal -> do
    projected <-
      either
        (Left . QueryProjectionInvalid projection)
        Right
        (schemaAt projection schema)
    case viewSchema projected of
      RecordSchema _ -> Left (QueryProjectionMustBeScalar projection)
      OptionalGlobalUniqueIdSchema ->
        Left (QueryProjectionMustBeScalar projection)
      scalarSchema
        | literalMatchesSchema literal scalarSchema -> Right ()
        | otherwise ->
            Left
              (QueryLiteralTypeMismatch projection scalarSchema literal)
  QueryNot child -> checkPredicate schema child
  QueryAll children -> mapM_ (checkPredicate schema) children
  QueryAny children -> mapM_ (checkPredicate schema) children

literalMatchesSchema :: QueryLiteral -> SchemaView -> Bool
literalMatchesSchema literal schema = case (literal, schema) of
  (QueryBool _, BoolSchema) -> True
  (QueryInt64 _, Int64Schema) -> True
  (QueryBytes _, BytesSchema) -> True
  (QueryText _, TextSchema) -> True
  (QueryGlobalUniqueId _, GlobalUniqueIdSchema) -> True
  (QueryLabel _, LabelSchema) -> True
  (QueryEnum symbol, EnumSchema members) -> symbol `elem` members
  _ -> False

evaluatePredicate ::
  ValueSchema ->
  Value ->
  QueryPredicate ->
  Either QueryEvaluationError Bool
evaluatePredicate schema value = \case
  QueryAlways -> Right True
  QueryNever -> Right False
  QueryCompare projection comparison literal -> do
    projected <- queryScalarAt schema projection value
    Right
      ( applyComparison
          comparison
          projected
          (literalScalar (projectedSchema schema projection) literal)
      )
  QueryNot child -> not <$> evaluatePredicate schema value child
  QueryAll children -> and <$> traverse (evaluatePredicate schema value) children
  QueryAny children -> or <$> traverse (evaluatePredicate schema value) children

data QueryScalar
  = ScalarBool Bool
  | ScalarInt64 Int64
  | ScalarBytes ByteString
  | ScalarText Text
  | ScalarGlobalUniqueId GlobalUniqueId
  | ScalarLabel Label
  | ScalarEnum Int
  deriving stock (Eq, Ord, Show)

queryScalarAt ::
  ValueSchema ->
  Projection ->
  Value ->
  Either QueryEvaluationError QueryScalar
queryScalarAt schema projection value = do
  projected <-
    either
      (Left . QueryProjectionContradictsCheckedSchema projection)
      Right
      (valueAt projection value)
  case viewValue projected of
    BoolValue scalar -> Right (ScalarBool scalar)
    Int64Value scalar -> Right (ScalarInt64 scalar)
    BytesValue scalar -> Right (ScalarBytes scalar)
    TextValue scalar -> Right (ScalarText scalar)
    GlobalUniqueIdValue scalar -> Right (ScalarGlobalUniqueId scalar)
    LabelValue scalar -> Right (ScalarLabel scalar)
    EnumValue scalar -> Right (ScalarEnum (enumOrdinal memberSchema scalar))
    record@(RecordValue _) ->
      Left (QueryProjectionContradictsScalarAdmission projection record)
    optional@(OptionalGlobalUniqueIdValue _) ->
      Left (QueryProjectionContradictsScalarAdmission projection optional)
  where
    memberSchema = projectedSchema schema projection

literalScalar :: ValueSchema -> QueryLiteral -> QueryScalar
literalScalar schema = \case
  QueryBool value -> ScalarBool value
  QueryInt64 value -> ScalarInt64 value
  QueryBytes value -> ScalarBytes value
  QueryText value -> ScalarText value
  QueryGlobalUniqueId value -> ScalarGlobalUniqueId value
  QueryLabel value -> ScalarLabel value
  QueryEnum value -> ScalarEnum (enumOrdinal schema value)

projectedSchema :: ValueSchema -> Projection -> ValueSchema
projectedSchema schema projection =
  case schemaAt projection schema of
    Right result -> result
    Left _ -> invariantFault "a checked query projection is absent from its retained schema"

enumOrdinal :: ValueSchema -> EnumSymbol -> Int
enumOrdinal schema symbol =
  case enumMemberOrdinal schema symbol of
    Just ordinal -> ordinal
    Nothing -> invariantFault "a schema-checked enum symbol has no retained ordinal"

invariantFault :: String -> result
invariantFault detail =
  error ("Eclips.Domain.Query invariant violated: " <> detail)

applyComparison :: ScalarComparison -> QueryScalar -> QueryScalar -> Bool
applyComparison comparison left right = case comparison of
  ScalarEqual -> left == right
  ScalarNotEqual -> left /= right
  ScalarLessThan -> left < right
  ScalarLessThanOrEqual -> left <= right
  ScalarGreaterThan -> left > right
  ScalarGreaterThanOrEqual -> left >= right
