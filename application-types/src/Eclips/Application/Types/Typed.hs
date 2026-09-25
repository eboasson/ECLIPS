{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- Field/scalar admission constraints intentionally restrict which expressions
-- typecheck even though their dictionaries erase when lowering the closed AST.
-- Compile-fail boundary fixtures cover these constraints.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Application records, their complete sort policy, and typed projections.
--
-- Generic derivation supports named product records, nested records, transparent
-- newtypes and closed nullary enums. It deliberately does not add a value grammar.
-- User schemas are generic-derived; schema overrides are deliberately absent.
-- A manual codec implementation must preserve both laws: decoding an encoding returns
-- the original value, and every value admitted by the schema decodes and reencodes
-- unchanged. Its named fields must agree with the generic representation used by
-- 'field'. In particular, a refined integer is not an unrestricted Int64 codec.
--
-- One application type specifies one complete sort definition. Newtypes select
-- alternate policies while reusing a representation; host type names themselves
-- are not part of a distributed sort identity.
module Eclips.Application.Types.Typed
  ( ValueType (encodeValue, decodeValue),
    valueSchema,
    ApplicationSortKind (..),
    ApplicationRankDirection (..),
    ApplicationScalarComparison (..),
    SchemaError (..),
    ValueError (..),
    Field,
    field,
    composeField,
    fieldProjection,
    Scalar,
    QueryScalar,
    DescriptorScalar,
    QueryPredicate,
    queryAlways,
    queryNever,
    queryCompare,
    queryEqual,
    queryNot,
    queryAll,
    queryAny,
    lowerQueryPredicate,
    DescriptorPredicate,
    descriptorAlways,
    descriptorNever,
    descriptorCompare,
    descriptorEqual,
    descriptorNot,
    descriptorAll,
    descriptorAny,
    Key,
    key,
    RankField,
    ascending,
    descending,
    SortPolicy,
    regularPolicy,
    controlledPolicy,
    withValidity,
    withObsolescence,
    withRank,
    withRetention,
    withImmutable,
    ApplicationSort (..),
    Sort,
    SortError (..),
    compileSort,
    sortId,
    sortDescriptor,
  ) where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Eclips.Application.Types.Identity (PrivateUniqueId, SortId)
import Eclips.Application.Types.Query qualified as Query
import Eclips.Application.Types.SortDescriptor
  ( ApplicationProjection (..),
    ApplicationRankDirection (..),
    ApplicationScalarComparison (..),
    ApplicationSortDescriptor,
    ApplicationSortKind (..),
    ApplicationValueSchema (..),
  )
import Eclips.Application.Types.SortDescriptor qualified as Descriptor
import Eclips.Application.Types.Value (ApplicationLabel, ApplicationValue (..))
import Eclips.Public.Types.SortCanonical qualified as Canonical
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)
import Type.Reflection (SomeTypeRep, someTypeRep)

data SchemaError
  = InvalidFieldName Text
  | DuplicateFieldName Text
  | EmptyEnumSchema
  | DuplicateEnumSymbol Text
  | RecursiveValueType String
  deriving stock (Eq, Show)

-- | A path identifies the failing field; errors preserve the actual raw value.
data ValueError = ValueError [Text] Text ApplicationValue
  deriving stock (Eq, Show)

class (Typeable a) => ValueType a where
  encodeValue :: a -> ApplicationValue
  default encodeValue :: (Generic a, GValue (Rep a)) => a -> ApplicationValue
  encodeValue = gEncode . from

  decodeValue :: ApplicationValue -> Either ValueError a
  default decodeValue :: (Generic a, GValue (Rep a)) => ApplicationValue -> Either ValueError a
  decodeValue value = to <$> gDecode value

  -- Thread the active type stack through generic fields so unsupported recursive
  -- schemas fail finitely, including mutual recursion. This is not a depth limit.
  schemaWith :: [SomeTypeRep] -> Either SchemaError ApplicationValueSchema
  default schemaWith :: (GValue (Rep a)) => [SomeTypeRep] -> Either SchemaError ApplicationValueSchema
  schemaWith seen
    | identity `elem` seen = Left (RecursiveValueType (show identity))
    | otherwise = gSchema @(Rep a) (identity : seen)
    where
      identity = someTypeRep (Proxy @a)

valueSchema :: forall a. (ValueType a) => Either SchemaError ApplicationValueSchema
valueSchema = schemaWith @a []

instance ValueType Bool where
  schemaWith _ = Right BoolSchema
  encodeValue = BoolValue
  decodeValue (BoolValue value) = Right value
  decodeValue value = wrong "Bool" value

instance ValueType Int64 where
  schemaWith _ = Right Int64Schema
  encodeValue = Int64Value
  decodeValue (Int64Value value) = Right value
  decodeValue value = wrong "Int64" value

instance ValueType ByteString where
  schemaWith _ = Right BytesSchema
  encodeValue = BytesValue
  decodeValue (BytesValue value) = Right value
  decodeValue value = wrong "bytes" value

instance ValueType Text where
  schemaWith _ = Right TextSchema
  encodeValue = TextValue
  decodeValue (TextValue value) = Right value
  decodeValue value = wrong "text" value

instance ValueType PrivateUniqueId where
  schemaWith _ = Right UniqueIdSchema
  encodeValue = UniqueIdValue
  decodeValue (UniqueIdValue value) = Right value
  decodeValue value = wrong "unique identity" value

instance ValueType ApplicationLabel where
  schemaWith _ = Right LabelSchema
  encodeValue = LabelValue
  decodeValue (LabelValue value) = Right value
  decodeValue value = wrong "label" value

instance ValueType (Maybe PrivateUniqueId) where
  schemaWith _ = Right OptionalUniqueIdSchema
  encodeValue = OptionalUniqueIdValue
  decodeValue (OptionalUniqueIdValue value) = Right value
  decodeValue value = wrong "optional unique identity" value

wrong :: Text -> ApplicationValue -> Either ValueError a
wrong expected = Left . ValueError [] ("expected " <> expected)

atField :: Text -> Either ValueError a -> Either ValueError a
atField name = either (\(ValueError path message value) -> Left (ValueError (name : path) message value)) Right

class GValue (f :: Type -> Type) where
  gSchema :: [SomeTypeRep] -> Either SchemaError ApplicationValueSchema
  gEncode :: f p -> ApplicationValue
  gDecode :: ApplicationValue -> Either ValueError (f p)

instance (GBody f) => GValue (M1 D ('MetaData name moduleName packageName 'False) f) where
  gSchema = gBodySchema @f
  gEncode (M1 value) = gBodyEncode value
  gDecode value = M1 <$> gBodyDecode value

instance (GNewtype f) => GValue (M1 D ('MetaData name moduleName packageName 'True) f) where
  gSchema = gNewtypeSchema @f
  gEncode (M1 value) = gNewtypeEncode value
  gDecode value = M1 <$> gNewtypeDecode value

class GNewtype (f :: Type -> Type) where
  gNewtypeSchema :: [SomeTypeRep] -> Either SchemaError ApplicationValueSchema
  gNewtypeEncode :: f p -> ApplicationValue
  gNewtypeDecode :: ApplicationValue -> Either ValueError (f p)

instance (ValueType a) => GNewtype (M1 C constructor (M1 S selector (K1 index a))) where
  gNewtypeSchema = schemaWith @a
  gNewtypeEncode (M1 (M1 (K1 value))) = encodeValue value
  gNewtypeDecode value = M1 . M1 . K1 <$> decodeValue value

class GBody (f :: Type -> Type) where
  gBodySchema :: [SomeTypeRep] -> Either SchemaError ApplicationValueSchema
  gBodyEncode :: f p -> ApplicationValue
  gBodyDecode :: ApplicationValue -> Either ValueError (f p)

instance (Constructor constructor, GFields f) => GBody (M1 C constructor f) where
  gBodySchema seen
    | isRecord = RecordSchema . Map.fromList <$> (gFieldsSchema @f seen >>= checkedFields)
    | otherwise = Right (EnumSchema (name :| []))
    where
      isRecord = conIsRecord (undefined :: M1 C constructor f ())
      name = Text.pack (conName (undefined :: M1 C constructor f ()))
  gBodyEncode value@(M1 fields)
    | conIsRecord value = RecordValue (gFieldsEncode fields)
    | otherwise = EnumValue (Text.pack (conName value))
  gBodyDecode value
    | conIsRecord (undefined :: M1 C constructor f ()) = case value of
        RecordValue fields -> do
          unless
            (Map.keysSet fields == Set.fromList (gFieldNames @f))
            (Left (ValueError [] "record fields differ from schema" value))
          M1 <$> gFieldsDecode value fields
        _ -> wrong "record" value
    | otherwise = case value of
        EnumValue symbol | symbol == name -> M1 <$> gFieldsDecode value Map.empty
        _ -> wrong ("enum member " <> name) value
    where
      name = Text.pack (conName (undefined :: M1 C constructor f ()))

instance (GEnum left, GEnum right) => GBody (left :+: right) where
  gBodySchema _ = enumSchema (gEnumNames @(left :+: right))
  gBodyEncode = EnumValue . gEnumEncode
  gBodyDecode value = case value of
    EnumValue symbol -> maybe (wrong "declared enum member" value) Right (gEnumDecode symbol)
    _ -> wrong "enum" value

class GEnum (f :: Type -> Type) where
  gEnumNames :: [Text]
  gEnumEncode :: f p -> Text
  gEnumDecode :: Text -> Maybe (f p)

instance (Constructor constructor) => GEnum (M1 C constructor U1) where
  gEnumNames = [Text.pack (conName (undefined :: M1 C constructor U1 ()))]
  gEnumEncode value = Text.pack (conName value)
  gEnumDecode symbol
    | symbol == Text.pack (conName (undefined :: M1 C constructor U1 ())) = Just (M1 U1)
    | otherwise = Nothing

instance (GEnum left, GEnum right) => GEnum (left :+: right) where
  gEnumNames = gEnumNames @left <> gEnumNames @right
  gEnumEncode (L1 value) = gEnumEncode value
  gEnumEncode (R1 value) = gEnumEncode value
  gEnumDecode symbol = case gEnumDecode @left symbol of
    Just value -> Just (L1 value)
    Nothing -> R1 <$> gEnumDecode @right symbol

class GFields (f :: Type -> Type) where
  gFieldsSchema :: [SomeTypeRep] -> Either SchemaError [(Text, ApplicationValueSchema)]
  gFieldNames :: [Text]
  gFieldsEncode :: f p -> Map Text ApplicationValue
  gFieldsDecode :: ApplicationValue -> Map Text ApplicationValue -> Either ValueError (f p)

instance GFields U1 where
  gFieldsSchema _ = Right []
  gFieldNames = []
  gFieldsEncode U1 = Map.empty
  gFieldsDecode _ _ = Right U1

instance (GFields left, GFields right) => GFields (left :*: right) where
  gFieldsSchema seen = (<>) <$> gFieldsSchema @left seen <*> gFieldsSchema @right seen
  gFieldNames = gFieldNames @left <> gFieldNames @right
  gFieldsEncode (left :*: right) = Map.union (gFieldsEncode left) (gFieldsEncode right)
  gFieldsDecode raw fields = (:*:) <$> gFieldsDecode raw fields <*> gFieldsDecode raw fields

instance (KnownSymbol name, ValueType a) => GFields (M1 S ('MetaSel ('Just name) unpacked strictness decided) (K1 index a)) where
  gFieldsSchema seen = do
    schema <- schemaWith @a seen
    pure [(Text.pack (symbolVal (Proxy @name)), schema)]
  gFieldNames = [Text.pack (symbolVal (Proxy @name))]
  gFieldsEncode (M1 (K1 value)) = Map.singleton (Text.pack (symbolVal (Proxy @name))) (encodeValue value)
  gFieldsDecode raw fields = case Map.lookup name fields of
    Nothing -> Left (ValueError [name] "missing record field" raw)
    Just value -> M1 . K1 <$> atField name (decodeValue value)
    where
      name = Text.pack (symbolVal (Proxy @name))

checkedFields :: [(Text, a)] -> Either SchemaError [(Text, a)]
checkedFields fields = do
  mapM_ (checkFieldName . fst) fields
  case duplicate (map fst fields) of
    Just name -> Left (DuplicateFieldName name)
    Nothing -> Right fields

checkFieldName :: Text -> Either SchemaError ()
checkFieldName name = case Text.uncons name of
  Just (first, rest) | letter first && Text.all (\c -> letter c || isDigit c || c == '_') rest -> Right ()
  _ -> Left (InvalidFieldName name)
  where
    letter c = isAsciiLower c || isAsciiUpper c

duplicate :: (Ord a) => [a] -> Maybe a
duplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (value : rest)
      | Set.member value seen = Just value
      | otherwise = go (Set.insert value seen) rest

enumSchema :: [Text] -> Either SchemaError ApplicationValueSchema
enumSchema [] = Left EmptyEnumSchema
enumSchema values@(first : rest) = case duplicate values of
  Just value -> Left (DuplicateEnumSymbol value)
  Nothing -> Right (EnumSchema (first :| rest))

-- Generic lookup is deliberately restricted to one named product constructor;
-- transparent newtypes expose the fields of their representation, not an extra
-- wrapper field absent from the value schema.
type family FindField (name :: Symbol) (f :: Type -> Type) :: Maybe Type where
  FindField name (M1 D ('MetaData datatypeName moduleName packageName 'True) (M1 C constructor (M1 S selector (K1 index a)))) = FindField name (Rep a)
  FindField name (M1 D metadata f) = FindField name f
  FindField name (M1 C metadata f) = FindField name f
  FindField name (left :*: right) = FirstField (FindField name left) (FindField name right)
  FindField name (M1 S ('MetaSel ('Just name) unpacked strictness decided) (K1 index a)) = 'Just a
  FindField name f = 'Nothing

type family FirstField (left :: Maybe Type) (right :: Maybe Type) :: Maybe Type where
  FirstField ('Just a) right = 'Just a
  FirstField 'Nothing right = right

type family RequiredField (name :: Symbol) (a :: Type) (found :: Maybe Type) :: Type where
  RequiredField name a ('Just b) = b
  RequiredField name a 'Nothing = TypeError ('Text "Unknown field " ':<>: 'ShowType name ':<>: 'Text " in encoded representation of " ':<>: 'ShowType a)

type role Field nominal nominal
newtype Field a b = Field ApplicationProjection

field :: forall name a b. (KnownSymbol name, ValueType a, Generic a, b ~ RequiredField name a (FindField name (Rep a))) => Field a b
field = Field (ApplicationProjection (Text.pack (symbolVal (Proxy @name)) :| []))

composeField :: Field a b -> Field b c -> Field a c
composeField (Field (ApplicationProjection left)) (Field (ApplicationProjection right)) = Field (ApplicationProjection (left <> right))

fieldProjection :: Field a b -> ApplicationProjection
fieldProjection (Field projection) = projection

-- | Scalar capabilities are separate from Haskell Eq/Ord. Comparisons and ranks
-- use the admitted ECLIPS semantics, including enum declaration order.
class (ValueType a) => Scalar a where
  scalarWitness :: Proxy a -> ()
  default scalarWitness :: (Generic a, GScalar (Rep a)) => Proxy a -> ()
  scalarWitness _ = ()

class (Scalar a) => QueryScalar a where
  queryLiteral :: a -> Query.ApplicationQueryLiteral
  default queryLiteral :: (Generic a, GQueryLiteral (Rep a)) => a -> Query.ApplicationQueryLiteral
  queryLiteral = gQueryLiteral . from
class (QueryScalar a) => DescriptorScalar a where
  descriptorLiteral :: a -> Descriptor.ApplicationScalarLiteral
  default descriptorLiteral :: (Generic a, GDescriptorLiteral (Rep a)) => a -> Descriptor.ApplicationScalarLiteral
  descriptorLiteral = gDescriptorLiteral . from

class GQueryLiteral (f :: Type -> Type) where
  gQueryLiteral :: f p -> Query.ApplicationQueryLiteral
instance (GEnum f) => GQueryLiteral (M1 D ('MetaData name moduleName packageName 'False) f) where
  gQueryLiteral (M1 value) = Query.QueryEnum (gEnumEncode value)
instance (QueryScalar a) => GQueryLiteral (M1 D ('MetaData name moduleName packageName 'True) (M1 C constructor (M1 S selector (K1 index a)))) where
  gQueryLiteral (M1 (M1 (M1 (K1 value)))) = queryLiteral value

class GDescriptorLiteral (f :: Type -> Type) where
  gDescriptorLiteral :: f p -> Descriptor.ApplicationScalarLiteral
instance (GEnum f) => GDescriptorLiteral (M1 D ('MetaData name moduleName packageName 'False) f) where
  gDescriptorLiteral (M1 value) = Descriptor.LiteralEnum (gEnumEncode value)
instance (DescriptorScalar a) => GDescriptorLiteral (M1 D ('MetaData name moduleName packageName 'True) (M1 C constructor (M1 S selector (K1 index a)))) where
  gDescriptorLiteral (M1 (M1 (M1 (K1 value)))) = descriptorLiteral value

class GScalar (f :: Type -> Type)
instance (GEnum f) => GScalar (M1 D ('MetaData name moduleName packageName 'False) f)
instance (Scalar a) => GScalar (M1 D ('MetaData name moduleName packageName 'True) (M1 C constructor (M1 S selector (K1 index a))))

instance Scalar Bool where scalarWitness _ = ()
instance QueryScalar Bool where queryLiteral = Query.QueryBool
instance DescriptorScalar Bool where descriptorLiteral = Descriptor.LiteralBool
instance Scalar Int64 where scalarWitness _ = ()
instance QueryScalar Int64 where queryLiteral = Query.QueryInt64
instance DescriptorScalar Int64 where descriptorLiteral = Descriptor.LiteralInt64
instance Scalar ByteString where scalarWitness _ = ()
instance QueryScalar ByteString where queryLiteral = Query.QueryBytes
instance DescriptorScalar ByteString where descriptorLiteral = Descriptor.LiteralBytes
instance Scalar Text where scalarWitness _ = ()
instance QueryScalar Text where queryLiteral = Query.QueryText
instance DescriptorScalar Text where descriptorLiteral = Descriptor.LiteralText
instance Scalar PrivateUniqueId where scalarWitness _ = ()
instance QueryScalar PrivateUniqueId where queryLiteral = Query.QueryUniqueId
instance Scalar ApplicationLabel where scalarWitness _ = ()
instance QueryScalar ApplicationLabel where queryLiteral = Query.QueryLabel

type role QueryPredicate nominal
newtype QueryPredicate a = QueryPredicate Query.ApplicationQueryPredicate

queryAlways, queryNever :: QueryPredicate a
queryAlways = QueryPredicate Query.QueryAlways
queryNever = QueryPredicate Query.QueryNever

queryCompare :: (QueryScalar b) => Field a b -> ApplicationScalarComparison -> b -> QueryPredicate a
queryCompare (Field projection) comparison value = QueryPredicate (Query.QueryCompare projection comparison (queryLiteral value))

queryEqual :: (QueryScalar b) => Field a b -> b -> QueryPredicate a
queryEqual projection = queryCompare projection ScalarEqual

queryNot :: QueryPredicate a -> QueryPredicate a
queryNot = QueryPredicate . Query.QueryNot . lowerQueryPredicate

queryAll, queryAny :: NonEmpty (QueryPredicate a) -> QueryPredicate a
queryAll = QueryPredicate . Query.QueryAll . fmap lowerQueryPredicate
queryAny = QueryPredicate . Query.QueryAny . fmap lowerQueryPredicate

lowerQueryPredicate :: QueryPredicate a -> Query.ApplicationQueryPredicate
lowerQueryPredicate (QueryPredicate predicate) = predicate

type role DescriptorPredicate nominal
newtype DescriptorPredicate a = DescriptorPredicate Descriptor.ApplicationPredicateExpression

descriptorAlways, descriptorNever :: DescriptorPredicate a
descriptorAlways = DescriptorPredicate Descriptor.AlwaysPredicate
descriptorNever = DescriptorPredicate Descriptor.NeverPredicate

descriptorCompare :: (DescriptorScalar b) => Field a b -> ApplicationScalarComparison -> b -> DescriptorPredicate a
descriptorCompare (Field projection) comparison value = DescriptorPredicate (Descriptor.CompareField projection comparison (descriptorLiteral value))

descriptorEqual :: (DescriptorScalar b) => Field a b -> b -> DescriptorPredicate a
descriptorEqual projection = descriptorCompare projection ScalarEqual

descriptorNot :: DescriptorPredicate a -> DescriptorPredicate a
descriptorNot (DescriptorPredicate predicate) = DescriptorPredicate (Descriptor.NotPredicate predicate)

descriptorAll, descriptorAny :: NonEmpty (DescriptorPredicate a) -> DescriptorPredicate a
descriptorAll = DescriptorPredicate . Descriptor.AllPredicates . fmap lowerDescriptorPredicate
descriptorAny = DescriptorPredicate . Descriptor.AnyPredicates . fmap lowerDescriptorPredicate

lowerDescriptorPredicate :: DescriptorPredicate a -> Descriptor.ApplicationPredicateExpression
lowerDescriptorPredicate (DescriptorPredicate predicate) = predicate

type role Key nominal
newtype Key a = Key ApplicationProjection

key :: forall a b. (Scalar b) => Field a b -> Key a
key (Field projection) = scalarWitness (Proxy @b) `seq` Key projection

type role RankField nominal
newtype RankField a = TypedRankField Descriptor.ApplicationRankTerm

ascending, descending :: forall a b. (Scalar b) => Field a b -> RankField a
ascending (Field projection) = scalarWitness (Proxy @b) `seq` TypedRankField (Descriptor.RankField projection Ascending)
descending (Field projection) = scalarWitness (Proxy @b) `seq` TypedRankField (Descriptor.RankField projection Descending)

type role SortPolicy nominal nominal
data SortPolicy (kind :: ApplicationSortKind) a
  = SortPolicy
      ApplicationSortKind
      [ApplicationProjection]
      Descriptor.ApplicationPredicateExpression
      Descriptor.ApplicationPredicateExpression
      [Descriptor.ApplicationRankTerm]
      ApplicationRankDirection
      Word64
      Bool
      (Maybe ApplicationProjection)

regularPolicy :: [Key a] -> SortPolicy 'RegularSort a
regularPolicy keys = SortPolicy RegularSort [projection | Key projection <- keys] Descriptor.AlwaysPredicate Descriptor.NeverPredicate [] Ascending 0 False Nothing

controlledPolicy :: Field a PrivateUniqueId -> Maybe (Field a ApplicationLabel) -> SortPolicy 'ControlledSort a
controlledPolicy (Field identity) label = SortPolicy ControlledSort [identity] Descriptor.AlwaysPredicate Descriptor.NeverPredicate [] Ascending 0 False (fieldProjection <$> label)

withValidity :: DescriptorPredicate a -> SortPolicy kind a -> SortPolicy kind a
withValidity (DescriptorPredicate predicate) (SortPolicy kind keys _ obsolete ranks fallback retention immutable label) = SortPolicy kind keys predicate obsolete ranks fallback retention immutable label

withObsolescence :: DescriptorPredicate a -> SortPolicy kind a -> SortPolicy kind a
withObsolescence (DescriptorPredicate predicate) (SortPolicy kind keys valid _ ranks fallback retention immutable label) = SortPolicy kind keys valid predicate ranks fallback retention immutable label

-- | A whole-value fallback is appended exactly once and always last.
withRank :: [RankField a] -> ApplicationRankDirection -> SortPolicy kind a -> SortPolicy kind a
withRank fields fallback (SortPolicy kind keys valid obsolete _ _ retention immutable label) = SortPolicy kind keys valid obsolete [term | TypedRankField term <- fields] fallback retention immutable label

withRetention :: Word64 -> SortPolicy kind a -> SortPolicy kind a
withRetention retention (SortPolicy kind keys valid obsolete ranks fallback _ immutable label) = SortPolicy kind keys valid obsolete ranks fallback retention immutable label

withImmutable :: Bool -> SortPolicy 'ControlledSort a -> SortPolicy 'ControlledSort a
withImmutable immutable (SortPolicy kind keys valid obsolete ranks fallback retention _ label) = SortPolicy kind keys valid obsolete ranks fallback retention immutable label

class (ValueType a) => ApplicationSort a where
  type SortKindOf a :: ApplicationSortKind
  type SortKindOf a = 'RegularSort
  sortPolicy :: SortPolicy (SortKindOf a) a

type role Sort nominal
data Sort a = Sort SortId ApplicationSortDescriptor
  deriving stock (Eq, Show)

data SortError
  = SortSchemaError SchemaError
  | SortProjectionMissing ApplicationProjection
  | SortProjectionNotScalar ApplicationProjection
  | SortPredicateTypeMismatch ApplicationProjection
  | ControlledKeyMustBeDirectIdentity
  | ControlledLabelMustBeDirectLabel
  | ControlledLabelUsedByPolicy Text
  deriving stock (Eq, Show)

compileSort :: forall a. (ApplicationSort a) => Either SortError (Sort a)
compileSort = do
  schema <- either (Left . SortSchemaError) Right (valueSchema @a)
  validateSchema schema
  let SortPolicy kind keys valid obsolete ranks fallback retention immutable label = sortPolicy @a
  mapM_ (checkScalarProjection schema) keys
  mapM_ (\case Descriptor.RankField projection _ -> checkScalarProjection schema projection; Descriptor.RankApplicationValue _ -> Right ()) ranks
  checkPredicate schema valid
  checkPredicate schema obsolete
  labelName <- case (kind, keys, label) of
    (RegularSort, _, _) -> Right Nothing
    (ControlledSort, [projection], designated) -> do
      unless (isDirect projection && schemaAt projection schema == Just UniqueIdSchema) (Left ControlledKeyMustBeDirectIdentity)
      case designated of
        Nothing -> Right Nothing
        Just labelProjection@(ApplicationProjection (name :| _)) -> do
          unless (isDirect labelProjection && schemaAt labelProjection schema == Just LabelSchema) (Left ControlledLabelMustBeDirectLabel)
          when
            (any (references name) keys || predicateReferences name valid || predicateReferences name obsolete || any (rankReferences name) ranks)
            (Left (ControlledLabelUsedByPolicy name))
          Right (Just name)
    (ControlledSort, _, _) -> Left ControlledKeyMustBeDirectIdentity
  let nonemptyTerms = foldr NonEmpty.cons (Descriptor.RankApplicationValue fallback :| []) ranks
      descriptor = Descriptor.ApplicationSortDescriptor kind schema keys valid obsolete nonemptyTerms retention immutable labelName
  pure (Sort (Canonical.rawDescriptorSortId (rawDescriptor descriptor)) descriptor)

sortId :: Sort a -> SortId
sortId (Sort identity _) = identity

sortDescriptor :: Sort a -> ApplicationSortDescriptor
sortDescriptor (Sort _ descriptor) = descriptor

validateSchema :: ApplicationValueSchema -> Either SortError ()
validateSchema = \case
  RecordSchema fields -> do
    either (Left . SortSchemaError) (const (Right ())) (checkedFields (Map.toList fields))
    mapM_ validateSchema fields
  EnumSchema members -> either (Left . SortSchemaError) (const (Right ())) (enumSchema (NonEmpty.toList members))
  _ -> Right ()

schemaAt :: ApplicationProjection -> ApplicationValueSchema -> Maybe ApplicationValueSchema
schemaAt (ApplicationProjection fields) = go (NonEmpty.toList fields)
  where
    go [] schema = Just schema
    go (name : rest) (RecordSchema fields') = Map.lookup name fields' >>= go rest
    go (_ : _) _ = Nothing

checkScalarProjection :: ApplicationValueSchema -> ApplicationProjection -> Either SortError ()
checkScalarProjection schema projection = case schemaAt projection schema of
  Nothing -> Left (SortProjectionMissing projection)
  Just (RecordSchema _) -> Left (SortProjectionNotScalar projection)
  Just OptionalUniqueIdSchema -> Left (SortProjectionNotScalar projection)
  Just _ -> Right ()

checkPredicate :: ApplicationValueSchema -> Descriptor.ApplicationPredicateExpression -> Either SortError ()
checkPredicate schema = \case
  Descriptor.AlwaysPredicate -> Right ()
  Descriptor.NeverPredicate -> Right ()
  Descriptor.CompareField projection _ literal -> do
    checkScalarProjection schema projection
    unless (matches literal (schemaAt projection schema)) (Left (SortPredicateTypeMismatch projection))
  Descriptor.NotPredicate child -> checkPredicate schema child
  Descriptor.AllPredicates children -> mapM_ (checkPredicate schema) children
  Descriptor.AnyPredicates children -> mapM_ (checkPredicate schema) children
  where
    matches (Descriptor.LiteralBool _) (Just BoolSchema) = True
    matches (Descriptor.LiteralInt64 _) (Just Int64Schema) = True
    matches (Descriptor.LiteralBytes _) (Just BytesSchema) = True
    matches (Descriptor.LiteralText _) (Just TextSchema) = True
    matches (Descriptor.LiteralEnum symbol) (Just (EnumSchema members)) = symbol `elem` members
    matches _ _ = False

isDirect :: ApplicationProjection -> Bool
isDirect (ApplicationProjection (_ :| rest)) = null rest

references :: Text -> ApplicationProjection -> Bool
references name (ApplicationProjection (first :| _)) = name == first

predicateReferences :: Text -> Descriptor.ApplicationPredicateExpression -> Bool
predicateReferences name = \case
  Descriptor.AlwaysPredicate -> False
  Descriptor.NeverPredicate -> False
  Descriptor.CompareField projection _ _ -> references name projection
  Descriptor.NotPredicate child -> predicateReferences name child
  Descriptor.AllPredicates children -> any (predicateReferences name) children
  Descriptor.AnyPredicates children -> any (predicateReferences name) children

rankReferences :: Text -> Descriptor.ApplicationRankTerm -> Bool
rankReferences name (Descriptor.RankField projection _) = references name projection
rankReferences _ (Descriptor.RankApplicationValue _) = False

rawDescriptor :: ApplicationSortDescriptor -> Canonical.RawDescriptor
rawDescriptor descriptor =
  Canonical.RawDescriptor
    Canonical.descriptorDomainTag
    (case Descriptor.sortKind descriptor of RegularSort -> Canonical.RawRegularSort; ControlledSort -> Canonical.RawControlledSort)
    (rawSchema (Descriptor.valueSchema descriptor))
    (map rawProjection (Descriptor.keyProjections descriptor))
    (rawPredicate (Descriptor.validityPredicate descriptor))
    (rawPredicate (Descriptor.obsolescencePredicate descriptor))
    (map rawRank (NonEmpty.toList (Descriptor.rankTerms descriptor)))
    (Descriptor.minimumRetentionMicros descriptor)
    (Descriptor.isImmutable descriptor)
    (TextEncoding.encodeUtf8 <$> Descriptor.labelField descriptor)
    Canonical.RawOrdinaryApplicationMutation
    Nothing

rawSchema :: ApplicationValueSchema -> Canonical.RawSchema
rawSchema = \case
  BoolSchema -> Canonical.RawBoolSchema
  Int64Schema -> Canonical.RawInt64Schema
  BytesSchema -> Canonical.RawBytesSchema
  TextSchema -> Canonical.RawTextSchema
  UniqueIdSchema -> Canonical.RawGlobalUniqueIdSchema
  LabelSchema -> Canonical.RawLabelSchema
  RecordSchema fields -> Canonical.RawRecordSchema [(TextEncoding.encodeUtf8 name, rawSchema schema) | (name, schema) <- Map.toAscList fields]
  EnumSchema members -> Canonical.RawEnumSchema (map TextEncoding.encodeUtf8 (NonEmpty.toList members))
  OptionalUniqueIdSchema -> Canonical.RawOptionalGlobalUniqueIdSchema

rawProjection :: ApplicationProjection -> Canonical.RawProjection
rawProjection (ApplicationProjection fields) = Canonical.RawProjection (map TextEncoding.encodeUtf8 (NonEmpty.toList fields))

rawPredicate :: Descriptor.ApplicationPredicateExpression -> Canonical.RawPredicate
rawPredicate = \case
  Descriptor.AlwaysPredicate -> Canonical.RawAlwaysPredicate
  Descriptor.NeverPredicate -> Canonical.RawNeverPredicate
  Descriptor.CompareField projection comparison literal -> Canonical.RawCompareField (rawProjection projection) (rawComparison comparison) (rawLiteral literal)
  Descriptor.NotPredicate child -> Canonical.RawNotPredicate (rawPredicate child)
  Descriptor.AllPredicates children -> Canonical.RawAllPredicates (map rawPredicate (NonEmpty.toList children))
  Descriptor.AnyPredicates children -> Canonical.RawAnyPredicates (map rawPredicate (NonEmpty.toList children))

rawComparison :: ApplicationScalarComparison -> Canonical.RawScalarComparison
rawComparison = \case
  ScalarEqual -> Canonical.RawScalarEqual
  ScalarNotEqual -> Canonical.RawScalarNotEqual
  ScalarLessThan -> Canonical.RawScalarLessThan
  ScalarLessThanOrEqual -> Canonical.RawScalarLessThanOrEqual
  ScalarGreaterThan -> Canonical.RawScalarGreaterThan
  ScalarGreaterThanOrEqual -> Canonical.RawScalarGreaterThanOrEqual

rawLiteral :: Descriptor.ApplicationScalarLiteral -> Canonical.RawScalarLiteral
rawLiteral = \case
  Descriptor.LiteralBool value -> Canonical.RawLiteralBool value
  Descriptor.LiteralInt64 value -> Canonical.RawLiteralInt64 value
  Descriptor.LiteralBytes value -> Canonical.RawLiteralBytes value
  Descriptor.LiteralText value -> Canonical.RawLiteralText (TextEncoding.encodeUtf8 value)
  Descriptor.LiteralEnum value -> Canonical.RawLiteralEnum (TextEncoding.encodeUtf8 value)

rawRank :: Descriptor.ApplicationRankTerm -> Canonical.RawRankTerm
rawRank (Descriptor.RankField projection direction) = Canonical.RawRankField (rawProjection projection) (rawDirection direction)
rawRank (Descriptor.RankApplicationValue direction) = Canonical.RawRankApplicationValue (rawDirection direction)

rawDirection :: ApplicationRankDirection -> Canonical.RawRankDirection
rawDirection Ascending = Canonical.RawAscending
rawDirection Descending = Canonical.RawDescending
