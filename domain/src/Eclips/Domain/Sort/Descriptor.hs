{-# LANGUAGE OverloadedRecordDot #-}

-- | Closed, checked sort-descriptor language and total evaluator.
--
-- The raw syntax exported by this module is untrusted first-order data. Only a
-- 'CheckedDescriptor' may enter semantic state. The grammar is deliberately
-- small: scalar field projections, typed scalar comparisons, Boolean predicate
-- composition, and a lexicographic rank whose final term is the complete
-- application value. For controlled sorts that final value omits the canonical
-- label field, so label changes cannot affect application rank.
module Eclips.Domain.Sort.Descriptor
  ( -- * Raw descriptor syntax
    DescriptorAdmission (..),
    SortKind (..),
    ScalarComparison (..),
    ScalarLiteral (..),
    PredicateExpression (..),
    RankDirection (..),
    RankTerm (..),
    ApplicationMutation (..),
    StructuralCarrierRole (..),
    structuralCarrierRoleTag,
    DescriptorSpec (..),

    -- * Admission
    CheckedDescriptor,
    DescriptorError (..),
    checkDescriptor,
    checkedDescriptorSpec,
    descriptorKind,
    descriptorSchema,
    descriptorKeyProjections,
    descriptorControlledKeyProjection,
    descriptorValidity,
    descriptorObsolescence,
    descriptorRankTerms,
    descriptorMinimumRetentionMicros,
    descriptorImmutable,
    descriptorLabelField,
    descriptorApplicationMutation,
    descriptorStructuralCarrierRole,

    -- * Oracle policy projection
    ApplicationObsolescence (..),
    ControlledPolicyProjection,
    policyHasLabelField,
    policyImmutable,
    policyApplicationObsolescence,
    policyApplicationMutation,
    policyStructuralCarrierRole,
    controlledPolicyProjection,

    -- * Evaluation
    ObjectKey,
    controlledObjectKey,
    Lifecycle (..),
    ApplicationRank,
    CheckedInstance,
    ValueEvaluationError (..),
    evaluateValue,
    checkedInstanceKey,
    checkedInstanceLifecycle,
    checkedInstanceRank,
    checkedInstanceCanonicalValue,
  )
where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity (GlobalUniqueId)
import Eclips.Domain.Value
  ( CanonicalValueBytes,
    EnumSymbol,
    FieldName,
    Label,
    Projection,
    SchemaError,
    SchemaView (..),
    Value,
    ValueError,
    ValueSchema,
    ValueView (..),
    canonicalValueBytes,
    checkValueSchema,
    directProjection,
    enumMemberOrdinal,
    projectionFields,
    recordValue,
    schemaAt,
    valueAt,
    viewSchema,
    viewValue,
  )

-- | Which trust path is admitting descriptor syntax.
data DescriptorAdmission
  = ApplicationDescriptor
  | PrimordialDescriptor
  deriving stock (Eq, Ord, Show)

data SortKind
  = RegularSort
  | ControlledSort
  deriving stock (Eq, Ord, Show)

data ScalarComparison
  = ScalarEqual
  | ScalarNotEqual
  | ScalarLessThan
  | ScalarLessThanOrEqual
  | ScalarGreaterThan
  | ScalarGreaterThanOrEqual
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Literal values admitted by predicate comparisons. Records are excluded.
data ScalarLiteral
  = LiteralBool Bool
  | LiteralInt64 Int64
  | LiteralBytes ByteString
  | LiteralText Text
  | LiteralEnum EnumSymbol
  deriving stock (Eq, Ord, Show)

data PredicateExpression
  = AlwaysPredicate
  | NeverPredicate
  | CompareField Projection ScalarComparison ScalarLiteral
  | NotPredicate PredicateExpression
  | AllPredicates (NonEmpty PredicateExpression)
  | AnyPredicates (NonEmpty PredicateExpression)
  deriving stock (Eq, Show)

data RankDirection
  = Ascending
  | Descending
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data RankTerm
  = RankField Projection RankDirection
  | RankApplicationValue RankDirection
  deriving stock (Eq, Show)

data ApplicationMutation
  = OrdinaryApplicationMutation
  | HeraldManagedMutation
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data StructuralCarrierRole
  = NeutralVertexCarrier
  | EdgeCarrier
  | NablaCarrier
  | DeltaCarrier
  | ProcessEpochCarrier
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Canonical closed tag shared by every identity and digest transcript.
--
-- These values are explicit profile constants rather than the derived 'Enum'
-- representation, so declaration-order refactoring cannot silently change a
-- semantic transcript.
structuralCarrierRoleTag :: StructuralCarrierRole -> Word8
structuralCarrierRoleTag role = case role of
  NeutralVertexCarrier -> 0
  EdgeCarrier -> 1
  NablaCarrier -> 2
  DeltaCarrier -> 3
  ProcessEpochCarrier -> 4

-- | Untrusted construction input. Every field contributes to descriptor
-- identity after successful admission.
data DescriptorSpec = DescriptorSpec
  { descriptorSpecKind :: SortKind,
    descriptorSpecSchema :: ValueSchema,
    descriptorSpecKeyProjections :: [Projection],
    descriptorSpecValidity :: PredicateExpression,
    descriptorSpecObsolescence :: PredicateExpression,
    descriptorSpecRankTerms :: NonEmpty RankTerm,
    descriptorSpecMinimumRetentionMicros :: Word64,
    descriptorSpecImmutable :: Bool,
    descriptorSpecLabelField :: Maybe FieldName,
    descriptorSpecApplicationMutation :: ApplicationMutation,
    descriptorSpecStructuralCarrierRole :: Maybe StructuralCarrierRole
  }
  deriving stock (Eq, Show)

-- | Validated descriptor. Its constructor is intentionally absent from the
-- public interface.
newtype CheckedDescriptor = CheckedDescriptor DescriptorSpec
  deriving stock (Eq, Show)

data DescriptorError
  = KeyProjectionInvalid Projection SchemaError
  | KeyProjectionMustBeScalar Projection
  | PredicateProjectionInvalid Projection SchemaError
  | PredicateLiteralTypeMismatch Projection SchemaView ScalarLiteral
  | RankProjectionInvalid Projection SchemaError
  | RankProjectionMustBeScalar Projection
  | RankMustEndInApplicationValue
  | RankApplicationValueMustBeUnique
  | RegularSortCannotBeImmutable
  | RegularSortCannotHaveLabel FieldName
  | RegularSortCannotHaveControlledPolicy
  | ControlledKeyMustBeOneDirectGlobalUniqueIdField
  | ControlledLabelMustBeDirectLabelField FieldName
  | ControlledLabelUsedByKey FieldName
  | ControlledLabelUsedByValidity FieldName
  | ControlledLabelUsedByObsolescence FieldName
  | ControlledLabelUsedByRank FieldName
  | ApplicationCannotClaimHeraldManagedMutation
  | ApplicationCannotClaimStructuralCarrier StructuralCarrierRole
  | StructuralCarrierMustBeControlled StructuralCarrierRole
  | StructuralCarrierMayNotBeApplicationObsolete StructuralCarrierRole
  | HeraldManagedMustBeProcessEpochCarrier
  deriving stock (Eq, Show)

checkDescriptor ::
  DescriptorAdmission ->
  DescriptorSpec ->
  Either DescriptorError CheckedDescriptor
checkDescriptor admission specification = do
  mapM_ (checkKeyProjection (descriptorSpecSchema specification)) keyProjections
  checkPredicate (descriptorSpecSchema specification) (descriptorSpecValidity specification)
  checkPredicate (descriptorSpecSchema specification) (descriptorSpecObsolescence specification)
  checkRank (descriptorSpecSchema specification) rankTerms
  checkKindRules specification
  checkAdmission admission specification
  Right (CheckedDescriptor specification)
  where
    keyProjections = descriptorSpecKeyProjections specification
    rankTerms = NonEmpty.toList (descriptorSpecRankTerms specification)

checkedDescriptorSpec :: CheckedDescriptor -> DescriptorSpec
checkedDescriptorSpec (CheckedDescriptor specification) = specification

descriptorKind :: CheckedDescriptor -> SortKind
descriptorKind = descriptorSpecKind . checkedDescriptorSpec

descriptorSchema :: CheckedDescriptor -> ValueSchema
descriptorSchema = descriptorSpecSchema . checkedDescriptorSpec

descriptorKeyProjections :: CheckedDescriptor -> [Projection]
descriptorKeyProjections = descriptorSpecKeyProjections . checkedDescriptorSpec

-- | The unique-ID key projection guaranteed by controlled-sort admission.
-- Regular sorts, including singleton-key regular sorts, return 'Nothing'.
descriptorControlledKeyProjection :: CheckedDescriptor -> Maybe Projection
descriptorControlledKeyProjection descriptor =
  case descriptorKind descriptor of
    RegularSort -> Nothing
    ControlledSort -> case descriptorKeyProjections descriptor of
      [projection] -> Just projection
      _ -> invariantFault "a checked controlled descriptor has no singleton key projection"

descriptorValidity :: CheckedDescriptor -> PredicateExpression
descriptorValidity = descriptorSpecValidity . checkedDescriptorSpec

descriptorObsolescence :: CheckedDescriptor -> PredicateExpression
descriptorObsolescence = descriptorSpecObsolescence . checkedDescriptorSpec

descriptorRankTerms :: CheckedDescriptor -> NonEmpty RankTerm
descriptorRankTerms = descriptorSpecRankTerms . checkedDescriptorSpec

descriptorMinimumRetentionMicros :: CheckedDescriptor -> Word64
descriptorMinimumRetentionMicros = descriptorSpecMinimumRetentionMicros . checkedDescriptorSpec

descriptorImmutable :: CheckedDescriptor -> Bool
descriptorImmutable = descriptorSpecImmutable . checkedDescriptorSpec

descriptorLabelField :: CheckedDescriptor -> Maybe FieldName
descriptorLabelField = descriptorSpecLabelField . checkedDescriptorSpec

descriptorApplicationMutation :: CheckedDescriptor -> ApplicationMutation
descriptorApplicationMutation = descriptorSpecApplicationMutation . checkedDescriptorSpec

descriptorStructuralCarrierRole :: CheckedDescriptor -> Maybe StructuralCarrierRole
descriptorStructuralCarrierRole = descriptorSpecStructuralCarrierRole . checkedDescriptorSpec

data ApplicationObsolescence
  = NeverApplicationObsolete
  | MayBecomeApplicationObsolete
  deriving stock (Eq, Ord, Show)

data ControlledPolicyProjection = ControlledPolicyProjection
  { hasLabelField :: Bool,
    immutable :: Bool,
    applicationObsolescence :: ApplicationObsolescence,
    applicationMutation :: ApplicationMutation,
    structuralCarrierRole :: Maybe StructuralCarrierRole
  }
  deriving stock (Eq, Show)

policyHasLabelField :: ControlledPolicyProjection -> Bool
policyHasLabelField policy = policy.hasLabelField

policyImmutable :: ControlledPolicyProjection -> Bool
policyImmutable policy = policy.immutable

policyApplicationObsolescence :: ControlledPolicyProjection -> ApplicationObsolescence
policyApplicationObsolescence policy = policy.applicationObsolescence

policyApplicationMutation :: ControlledPolicyProjection -> ApplicationMutation
policyApplicationMutation policy = policy.applicationMutation

policyStructuralCarrierRole :: ControlledPolicyProjection -> Maybe StructuralCarrierRole
policyStructuralCarrierRole policy = policy.structuralCarrierRole

controlledPolicyProjection :: CheckedDescriptor -> Maybe ControlledPolicyProjection
controlledPolicyProjection descriptor
  | descriptorKind descriptor == RegularSort = Nothing
  | otherwise =
      Just
        ControlledPolicyProjection
          { hasLabelField = maybe False (const True) (descriptorLabelField descriptor),
            immutable = descriptorImmutable descriptor,
            applicationObsolescence =
              if descriptorObsolescence descriptor == NeverPredicate
                then NeverApplicationObsolete
                else MayBecomeApplicationObsolete,
            applicationMutation = descriptorApplicationMutation descriptor,
            structuralCarrierRole = descriptorStructuralCarrierRole descriptor
          }

checkKeyProjection :: ValueSchema -> Projection -> Either DescriptorError ()
checkKeyProjection schema projection =
  case schemaAt projection schema of
    Left problem -> Left (KeyProjectionInvalid projection problem)
    Right projected
      | isScalarSchema projected -> Right ()
      | otherwise -> Left (KeyProjectionMustBeScalar projection)

checkPredicate :: ValueSchema -> PredicateExpression -> Either DescriptorError ()
checkPredicate schema = \case
  AlwaysPredicate -> Right ()
  NeverPredicate -> Right ()
  CompareField projection _ literal -> do
    projected <-
      either
        (Left . PredicateProjectionInvalid projection)
        Right
        (schemaAt projection schema)
    unless (literalMatchesSchema literal projected)
      $ Left (PredicateLiteralTypeMismatch projection (viewSchema projected) literal)
  NotPredicate child -> checkPredicate schema child
  AllPredicates children -> mapM_ (checkPredicate schema) children
  AnyPredicates children -> mapM_ (checkPredicate schema) children

checkRank :: ValueSchema -> [RankTerm] -> Either DescriptorError ()
checkRank schema terms = do
  case reverse terms of
    RankApplicationValue _ : _ -> Right ()
    _ -> Left RankMustEndInApplicationValue
  when (length (filter isApplicationValue terms) /= 1)
    $ Left RankApplicationValueMustBeUnique
  mapM_ checkTerm terms
  where
    isApplicationValue (RankApplicationValue _) = True
    isApplicationValue (RankField _ _) = False
    checkTerm (RankApplicationValue _) = Right ()
    checkTerm (RankField projection _) =
      case schemaAt projection schema of
        Left problem -> Left (RankProjectionInvalid projection problem)
        Right projected
          | isScalarSchema projected -> Right ()
          | otherwise -> Left (RankProjectionMustBeScalar projection)

checkKindRules :: DescriptorSpec -> Either DescriptorError ()
checkKindRules specification = case descriptorSpecKind specification of
  RegularSort -> do
    when (descriptorSpecImmutable specification) (Left RegularSortCannotBeImmutable)
    case descriptorSpecLabelField specification of
      Nothing -> Right ()
      Just label -> Left (RegularSortCannotHaveLabel label)
    when
      ( descriptorSpecApplicationMutation specification /= OrdinaryApplicationMutation
          || descriptorSpecStructuralCarrierRole specification /= Nothing
      )
      (Left RegularSortCannotHaveControlledPolicy)
  ControlledSort -> checkControlledRules specification

checkControlledRules :: DescriptorSpec -> Either DescriptorError ()
checkControlledRules specification = do
  case descriptorSpecKeyProjections specification of
    [projection]
      | isDirect projection,
        Right projected <- schemaAt projection (descriptorSpecSchema specification),
        viewSchema projected == GlobalUniqueIdSchema ->
          Right ()
    _ -> Left ControlledKeyMustBeOneDirectGlobalUniqueIdField
  case descriptorSpecLabelField specification of
    Nothing -> Right ()
    Just label -> do
      let labelProjection = directProjection label
      case schemaAt labelProjection (descriptorSpecSchema specification) of
        Right projected | viewSchema projected == LabelSchema -> Right ()
        _ -> Left (ControlledLabelMustBeDirectLabelField label)
      when
        (any (projectionReferences label) (descriptorSpecKeyProjections specification))
        (Left (ControlledLabelUsedByKey label))
      when
        (predicateReferences label (descriptorSpecValidity specification))
        (Left (ControlledLabelUsedByValidity label))
      when
        (predicateReferences label (descriptorSpecObsolescence specification))
        (Left (ControlledLabelUsedByObsolescence label))
      when
        (any (rankReferences label) (descriptorSpecRankTerms specification))
        (Left (ControlledLabelUsedByRank label))

checkAdmission :: DescriptorAdmission -> DescriptorSpec -> Either DescriptorError ()
checkAdmission admission specification = do
  case (admission, descriptorSpecApplicationMutation specification) of
    (ApplicationDescriptor, HeraldManagedMutation) ->
      Left ApplicationCannotClaimHeraldManagedMutation
    _ -> Right ()
  case (admission, descriptorSpecStructuralCarrierRole specification) of
    (ApplicationDescriptor, Just role) -> Left (ApplicationCannotClaimStructuralCarrier role)
    _ -> Right ()
  case descriptorSpecStructuralCarrierRole specification of
    Nothing ->
      when
        (descriptorSpecApplicationMutation specification == HeraldManagedMutation)
        (Left HeraldManagedMustBeProcessEpochCarrier)
    Just role -> do
      when
        (descriptorSpecKind specification /= ControlledSort)
        (Left (StructuralCarrierMustBeControlled role))
      when
        (descriptorSpecObsolescence specification /= NeverPredicate)
        (Left (StructuralCarrierMayNotBeApplicationObsolete role))
      when
        ( descriptorSpecApplicationMutation specification == HeraldManagedMutation
            && role /= ProcessEpochCarrier
        )
        (Left HeraldManagedMustBeProcessEpochCarrier)
      when
        ( role == ProcessEpochCarrier
            && descriptorSpecApplicationMutation specification /= HeraldManagedMutation
        )
        (Left HeraldManagedMustBeProcessEpochCarrier)

isScalarSchema :: ValueSchema -> Bool
isScalarSchema schema = case viewSchema schema of
  RecordSchema _ -> False
  OptionalGlobalUniqueIdSchema -> False
  _ -> True

literalMatchesSchema :: ScalarLiteral -> ValueSchema -> Bool
literalMatchesSchema literal schema = case (literal, viewSchema schema) of
  (LiteralBool _, BoolSchema) -> True
  (LiteralInt64 _, Int64Schema) -> True
  (LiteralBytes _, BytesSchema) -> True
  (LiteralText _, TextSchema) -> True
  (LiteralEnum symbol, EnumSchema members) -> symbol `elem` members
  _ -> False

isDirect :: Projection -> Bool
isDirect projection = NonEmpty.length (projectionFields projection) == 1

projectionReferences :: FieldName -> Projection -> Bool
projectionReferences target projection =
  NonEmpty.head (projectionFields projection) == target

predicateReferences :: FieldName -> PredicateExpression -> Bool
predicateReferences target = \case
  AlwaysPredicate -> False
  NeverPredicate -> False
  CompareField projection _ _ -> projectionReferences target projection
  NotPredicate child -> predicateReferences target child
  AllPredicates children -> any (predicateReferences target) children
  AnyPredicates children -> any (predicateReferences target) children

rankReferences :: FieldName -> RankTerm -> Bool
rankReferences target = \case
  RankField projection _ -> projectionReferences target projection
  RankApplicationValue _ -> False

-- | Scalar key tuple. The constructor is private so every member has been
-- projected from a checked value through a checked descriptor. The empty tuple
-- is the one key of a regular sort whose key-projection list is empty.
newtype ObjectKey = ObjectKey [ScalarValue]
  deriving stock (Eq, Ord, Show)

-- | The canonical key shared by every checked controlled sort.  Controlled
-- descriptor admission proves that its sole key projection is a direct
-- 'GlobalUniqueId' field, so terminal suppression can name an object even
-- when no publication payload remains available.
controlledObjectKey :: GlobalUniqueId -> ObjectKey
controlledObjectKey identifier = ObjectKey [ScalarGlobalUniqueId identifier]

data Lifecycle
  = Current
  | Obsolete
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data ScalarValue
  = ScalarBool Bool
  | ScalarInt64 Int64
  | ScalarBytes ByteString
  | ScalarText Text
  | ScalarGlobalUniqueId GlobalUniqueId
  | ScalarLabel Label
  | ScalarEnum Int
  deriving stock (Eq, Ord, Show)

data RankValue
  = RankScalar ScalarValue
  | RankCanonicalValue CanonicalValueBytes
  deriving stock (Eq, Ord, Show)

data RankedValue = RankedValue RankDirection RankValue
  deriving stock (Eq, Show)

instance Ord RankedValue where
  compare (RankedValue leftDirection left) (RankedValue rightDirection right)
    | leftDirection /= rightDirection = compare leftDirection rightDirection
    | leftDirection == Ascending = compare left right
    | otherwise = compare right left

newtype ApplicationRank = ApplicationRank (NonEmpty RankedValue)
  deriving stock (Eq, Ord, Show)

data CheckedInstance = CheckedInstance
  { value :: Value,
    key :: ObjectKey,
    lifecycle :: Lifecycle,
    rank :: ApplicationRank,
    canonicalValue :: CanonicalValueBytes
  }
  deriving stock (Eq, Show)

data ValueEvaluationError
  = ValueSchemaRejected ValueError
  | ValueValidityRejected
  deriving stock (Eq, Show)

evaluateValue ::
  CheckedDescriptor ->
  Value ->
  Either ValueEvaluationError CheckedInstance
evaluateValue descriptor value = do
  either (Left . ValueSchemaRejected) Right (checkValueSchema schema value)
  let valid = evaluatePredicate schema value (descriptorValidity descriptor)
  unless valid (Left ValueValidityRejected)
  let key = ObjectKey (fmap (projectScalar schema value) (descriptorKeyProjections descriptor))
      obsolete = evaluatePredicate schema value (descriptorObsolescence descriptor)
      rank = ApplicationRank (fmap (evaluateRankTerm descriptor value) (descriptorRankTerms descriptor))
  Right
    CheckedInstance
      { value = value,
        key = key,
        lifecycle = if obsolete then Obsolete else Current,
        rank = rank,
        canonicalValue = canonicalValueBytes value
      }
  where
    schema = descriptorSchema descriptor

checkedInstanceKey :: CheckedInstance -> ObjectKey
checkedInstanceKey checked = checked.key

checkedInstanceLifecycle :: CheckedInstance -> Lifecycle
checkedInstanceLifecycle checked = checked.lifecycle

checkedInstanceRank :: CheckedInstance -> ApplicationRank
checkedInstanceRank checked = checked.rank

checkedInstanceCanonicalValue :: CheckedInstance -> CanonicalValueBytes
checkedInstanceCanonicalValue checked = checked.canonicalValue

evaluatePredicate :: ValueSchema -> Value -> PredicateExpression -> Bool
evaluatePredicate schema value = \case
  AlwaysPredicate -> True
  NeverPredicate -> False
  CompareField projection comparison literal ->
    applyComparison
      comparison
      (projectScalar schema value projection)
      (literalScalar (projectedSchema schema projection) literal)
  NotPredicate child -> not (evaluatePredicate schema value child)
  AllPredicates children -> all (evaluatePredicate schema value) children
  AnyPredicates children -> any (evaluatePredicate schema value) children

evaluateRankTerm ::
  CheckedDescriptor ->
  Value ->
  RankTerm ->
  RankedValue
evaluateRankTerm descriptor value (RankField projection direction) =
  RankedValue direction (RankScalar (projectScalar (descriptorSchema descriptor) value projection))
evaluateRankTerm descriptor value (RankApplicationValue direction) =
  RankedValue direction (RankCanonicalValue (applicationValueBytes descriptor value))

applicationValueBytes ::
  CheckedDescriptor ->
  Value ->
  CanonicalValueBytes
applicationValueBytes descriptor value = case descriptorLabelField descriptor of
  Nothing -> canonicalValueBytes value
  Just label -> case viewValue value of
    RecordValue fields ->
      case recordValue (Map.toAscList (Map.delete label fields)) of
        Right withoutLabel -> canonicalValueBytes withoutLabel
        Left problem ->
          invariantFault
            ( "removing an admitted label field failed: "
                <> show problem
            )
    _ -> invariantFault "an admitted label field was evaluated against a scalar"

projectScalar :: ValueSchema -> Value -> Projection -> ScalarValue
projectScalar schema value projection =
  case viewValue projected of
    BoolValue scalar -> ScalarBool scalar
    Int64Value scalar -> ScalarInt64 scalar
    BytesValue scalar -> ScalarBytes scalar
    TextValue scalar -> ScalarText scalar
    GlobalUniqueIdValue scalar -> ScalarGlobalUniqueId scalar
    LabelValue scalar -> ScalarLabel scalar
    EnumValue scalar -> ScalarEnum (enumOrdinal memberSchema scalar)
    RecordValue _ -> invariantFault "an admitted scalar projection selected a record"
    OptionalGlobalUniqueIdValue _ ->
      invariantFault "an admitted scalar projection selected an optional global unique ID"
  where
    memberSchema = projectedSchema schema projection
    projected =
      case valueAt projection value of
        Right result -> result
        Left problem ->
          invariantFault
            ( "an admitted projection failed against a schema-checked value: "
                <> show problem
            )

projectedSchema :: ValueSchema -> Projection -> ValueSchema
projectedSchema schema projection =
  case schemaAt projection schema of
    Right result -> result
    Left problem ->
      invariantFault
        ( "an admitted projection failed against its checked schema: "
            <> show problem
        )

enumOrdinal :: ValueSchema -> EnumSymbol -> Int
enumOrdinal schema symbol =
  case enumMemberOrdinal schema symbol of
    Just ordinal -> ordinal
    Nothing -> invariantFault "an admitted enum symbol has no schema ordinal"

invariantFault :: String -> result
invariantFault detail =
  error ("Eclips.Domain.Sort.Descriptor invariant violated: " <> detail)

literalScalar :: ValueSchema -> ScalarLiteral -> ScalarValue
literalScalar schema = \case
  LiteralBool value -> ScalarBool value
  LiteralInt64 value -> ScalarInt64 value
  LiteralBytes value -> ScalarBytes value
  LiteralText value -> ScalarText value
  LiteralEnum value -> ScalarEnum (enumOrdinal schema value)

applyComparison :: ScalarComparison -> ScalarValue -> ScalarValue -> Bool
applyComparison comparison left right = case comparison of
  ScalarEqual -> left == right
  ScalarNotEqual -> left /= right
  ScalarLessThan -> left < right
  ScalarLessThanOrEqual -> left <= right
  ScalarGreaterThan -> left > right
  ScalarGreaterThanOrEqual -> left >= right
