{-# LANGUAGE DeriveAnyClass #-}

-- | Checked first-order values and value schemas.
--
-- Profile 0.1 deliberately fixes a small grammar but places no semantic size,
-- member-count, or nesting limit on values admitted by that grammar.  Host
-- resource exhaustion ends the finite experimental run; it is not a domain
-- rejection outcome.
--
-- Constructors for checked schemas and values are intentionally hidden.  The
-- read-only views let descriptor evaluation remain total without allowing a
-- caller to forge a value that bypassed these checks.
module Eclips.Domain.Value
  ( -- * Field names and projections
    FieldName,
    FieldNameError (..),
    mkFieldName,
    fieldNameText,
    Projection,
    mkProjection,
    directProjection,
    projectionFields,
    isDirectProjection,

    -- * Enum symbols
    EnumSymbol,
    enumSymbol,
    enumSymbolText,
    enumSymbolOrdinal,
    enumMemberOrdinal,

    -- * Schemas
    ValueSchema,
    SchemaView (..),
    SchemaError (..),
    boolSchema,
    int64Schema,
    bytesSchema,
    textSchema,
    globalUniqueIdSchema,
    labelSchema,
    optionalGlobalUniqueIdSchema,
    enumSchema,
    recordSchema,
    recordSchemaMap,
    viewSchema,
    schemaAt,

    -- * Values
    Label,
    LabelOwner (..),
    Value,
    ValueView (..),
    ValueError (..),
    boolValue,
    int64Value,
    bytesValue,
    textValue,
    globalUniqueIdValue,
    labelValue,
    optionalGlobalUniqueIdValue,
    enumValue,
    recordValue,
    recordValueMap,
    viewValue,
    valueAt,
    checkValueSchema,

    -- * Canonical value bytes
    CanonicalValueBytes,
    canonicalValueBytes,
    canonicalValueByteString,
    decodeCanonicalValue,
  ) where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Serialize qualified as Serialize
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import Eclips.Domain.Identity
  ( GlobalUniqueId,
    ProcessEpochId,
    globalUniqueIdBytes,
    mkGlobalUniqueId,
    mkProcessEpochId,
    processEpochIdBytes,
  )
import GHC.Generics (Generic)

newtype FieldName = FieldName Text
  deriving stock (Eq, Ord, Show)

data FieldNameError
  = EmptyFieldName
  | InvalidFieldNameCharacter Char
  | InvalidFieldNameStart Char
  deriving stock (Eq, Show)

mkFieldName :: Text -> Either FieldNameError FieldName
mkFieldName name = do
  when (Text.null name) (Left EmptyFieldName)
  case Text.uncons name of
    Nothing -> Left EmptyFieldName
    Just (firstCharacter, rest) -> do
      unless (asciiLetter firstCharacter) (Left (InvalidFieldNameStart firstCharacter))
      case Text.find (not . fieldTailCharacter) rest of
        Nothing -> Right (FieldName name)
        Just invalidCharacter -> Left (InvalidFieldNameCharacter invalidCharacter)
  where
    asciiLetter character =
      isAscii character && (isAsciiLower character || isAsciiUpper character)
    fieldTailCharacter character =
      asciiLetter character || isDigit character || character == '_'

fieldNameText :: FieldName -> Text
fieldNameText (FieldName name) = name

newtype Projection = Projection (NonEmpty FieldName)
  deriving stock (Eq, Ord, Show)

mkProjection :: NonEmpty FieldName -> Projection
mkProjection = Projection

directProjection :: FieldName -> Projection
directProjection field = Projection (field :| [])

projectionFields :: Projection -> NonEmpty FieldName
projectionFields (Projection fields) = fields

isDirectProjection :: Projection -> Bool
isDirectProjection (Projection (_ :| rest)) = null rest

-- | One symbolic member of a closed enum schema.
--
-- Symbols are ordinary Unicode text. Their exact text is semantic; the owning
-- enum schema assigns each symbol its declaration-order scalar position.
newtype EnumSymbol = EnumSymbol Text
  deriving stock (Eq, Ord, Show)

enumSymbol :: Text -> EnumSymbol
enumSymbol = EnumSymbol

enumSymbolText :: EnumSymbol -> Text
enumSymbolText (EnumSymbol symbol) = symbol

data ValueSchema
  = SchemaBool
  | SchemaInt64
  | SchemaBytes
  | SchemaText
  | SchemaGlobalUniqueId
  | SchemaLabel
  | SchemaRecord (Map FieldName ValueSchema)
  | SchemaEnum (NonEmpty EnumSymbol)
  | SchemaOptionalGlobalUniqueId
  deriving stock (Eq, Show)

data SchemaView
  = BoolSchema
  | Int64Schema
  | BytesSchema
  | TextSchema
  | GlobalUniqueIdSchema
  | LabelSchema
  | RecordSchema (Map FieldName ValueSchema)
  | EnumSchema (NonEmpty EnumSymbol)
  | OptionalGlobalUniqueIdSchema
  deriving stock (Eq, Show)

data SchemaError
  = DuplicateSchemaField FieldName
  | DuplicateEnumSymbol EnumSymbol
  | SchemaProjectionMissing FieldName
  | SchemaProjectionThroughScalar FieldName
  deriving stock (Eq, Show)

boolSchema, int64Schema, bytesSchema, textSchema, globalUniqueIdSchema, labelSchema, optionalGlobalUniqueIdSchema :: ValueSchema
boolSchema = SchemaBool
int64Schema = SchemaInt64
bytesSchema = SchemaBytes
textSchema = SchemaText
globalUniqueIdSchema = SchemaGlobalUniqueId
labelSchema = SchemaLabel
optionalGlobalUniqueIdSchema = SchemaOptionalGlobalUniqueId

-- | Construct a closed, non-empty enum schema.
--
-- Declaration order is semantic and defines scalar ordering. Repeated symbols
-- are rejected so every admitted value has one unambiguous ordinal.
enumSchema :: NonEmpty EnumSymbol -> Either SchemaError ValueSchema
enumSchema members = case firstDuplicate (NE.toList members) of
  Nothing -> Right (SchemaEnum members)
  Just duplicate -> Left (DuplicateEnumSymbol duplicate)

enumSymbolOrdinal :: NonEmpty EnumSymbol -> EnumSymbol -> Maybe Int
enumSymbolOrdinal members target = go 0 (NE.toList members)
  where
    go _ [] = Nothing
    go ordinal (member : remaining)
      | member == target = Just ordinal
      | otherwise = go (ordinal + 1) remaining

enumMemberOrdinal :: ValueSchema -> EnumSymbol -> Maybe Int
enumMemberOrdinal (SchemaEnum members) = enumSymbolOrdinal members
enumMemberOrdinal _ = const Nothing

firstDuplicate :: (Ord value) => [value] -> Maybe value
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (value : remaining)
      | Set.member value seen = Just value
      | otherwise = go (Set.insert value seen) remaining

recordSchema :: [(FieldName, ValueSchema)] -> Either SchemaError ValueSchema
recordSchema fields = do
  members <- uniqueMap DuplicateSchemaField fields
  Right (recordSchemaMap members)

-- | Construct a schema record from already admitted, uniquely keyed fields.
--
-- The map representation makes duplicate fields unrepresentable, so typed AST
-- adapters need no defensive rejection for that impossible state.
recordSchemaMap :: Map FieldName ValueSchema -> ValueSchema
recordSchemaMap = SchemaRecord

viewSchema :: ValueSchema -> SchemaView
viewSchema = \case
  SchemaBool -> BoolSchema
  SchemaInt64 -> Int64Schema
  SchemaBytes -> BytesSchema
  SchemaText -> TextSchema
  SchemaGlobalUniqueId -> GlobalUniqueIdSchema
  SchemaLabel -> LabelSchema
  SchemaRecord fields -> RecordSchema fields
  SchemaEnum members -> EnumSchema members
  SchemaOptionalGlobalUniqueId -> OptionalGlobalUniqueIdSchema

schemaAt :: Projection -> ValueSchema -> Either SchemaError ValueSchema
schemaAt (Projection fields) = descend (NE.toList fields)
  where
    descend [] schema = Right schema
    descend (field : remaining) schema = case schema of
      SchemaRecord members ->
        maybe
          (Left (SchemaProjectionMissing field))
          (descend remaining)
          (Map.lookup field members)
      _ -> Left (SchemaProjectionThroughScalar field)

-- | The owner component is distinct from the object-scoped successful-operation
-- generation. Equality and ordering of the complete pair retain both components.
type Label = (LabelOwner, Word64)

data LabelOwner
  = VoidLabel
  | ProcessLabel ProcessEpochId
  | ZombieLabel ProcessEpochId
  deriving stock (Eq, Ord, Show)

data Value
  = ValueBool Bool
  | ValueInt64 Int64
  | ValueBytes ByteString
  | ValueText Text
  | ValueGlobalUniqueId GlobalUniqueId
  | ValueLabel Label
  | ValueRecord (Map FieldName Value)
  | ValueEnum EnumSymbol
  | ValueOptionalGlobalUniqueId (Maybe GlobalUniqueId)
  deriving stock (Eq, Ord, Show)

data ValueView
  = BoolValue Bool
  | Int64Value Int64
  | BytesValue ByteString
  | TextValue Text
  | GlobalUniqueIdValue GlobalUniqueId
  | LabelValue Label
  | RecordValue (Map FieldName Value)
  | EnumValue EnumSymbol
  | OptionalGlobalUniqueIdValue (Maybe GlobalUniqueId)
  deriving stock (Eq, Show)

data ValueError
  = DuplicateValueField FieldName
  | ValueProjectionMissing FieldName
  | ValueProjectionThroughScalar FieldName
  | ValueSchemaMismatch SchemaView ValueView
  | ValueDecodeMalformed
  | ValueDecodeInvalidUtf8
  | ValueDecodeInvalidFieldName FieldNameError
  | ValueDecodeInvalidGlobalUniqueId
  | ValueDecodeInvalidProcessEpoch
  | ValueDecodeUnknownDomainTag Word64
  | ValueDecodeNonCanonicalEncoding
  deriving stock (Eq, Show)

boolValue :: Bool -> Value
boolValue = ValueBool

int64Value :: Int64 -> Value
int64Value = ValueInt64

bytesValue :: ByteString -> Value
bytesValue = ValueBytes

textValue :: Text -> Value
textValue = ValueText

globalUniqueIdValue :: GlobalUniqueId -> Value
globalUniqueIdValue = ValueGlobalUniqueId

labelValue :: Label -> Value
labelValue = ValueLabel

optionalGlobalUniqueIdValue :: Maybe GlobalUniqueId -> Value
optionalGlobalUniqueIdValue = ValueOptionalGlobalUniqueId

enumValue :: EnumSymbol -> Value
enumValue = ValueEnum

recordValue :: [(FieldName, Value)] -> Either ValueError Value
recordValue fields = do
  members <- uniqueMap DuplicateValueField fields
  Right (recordValueMap members)

-- | Construct a checked record from already admitted, uniquely keyed fields.
--
-- The map representation makes duplicate fields unrepresentable, so this is the
-- total companion to the list-oriented boundary constructor.
recordValueMap :: Map FieldName Value -> Value
recordValueMap = ValueRecord

viewValue :: Value -> ValueView
viewValue = \case
  ValueBool value -> BoolValue value
  ValueInt64 value -> Int64Value value
  ValueBytes value -> BytesValue value
  ValueText value -> TextValue value
  ValueGlobalUniqueId value -> GlobalUniqueIdValue value
  ValueLabel value -> LabelValue value
  ValueRecord fields -> RecordValue fields
  ValueEnum value -> EnumValue value
  ValueOptionalGlobalUniqueId value -> OptionalGlobalUniqueIdValue value

valueAt :: Projection -> Value -> Either ValueError Value
valueAt (Projection fields) = descend (NE.toList fields)
  where
    descend [] value = Right value
    descend (field : remaining) value = case value of
      ValueRecord members ->
        maybe
          (Left (ValueProjectionMissing field))
          (descend remaining)
          (Map.lookup field members)
      _ -> Left (ValueProjectionThroughScalar field)

checkValueSchema :: ValueSchema -> Value -> Either ValueError ()
checkValueSchema schema value = case (schema, value) of
  (SchemaBool, ValueBool _) -> Right ()
  (SchemaInt64, ValueInt64 _) -> Right ()
  (SchemaBytes, ValueBytes _) -> Right ()
  (SchemaText, ValueText _) -> Right ()
  (SchemaGlobalUniqueId, ValueGlobalUniqueId _) -> Right ()
  (SchemaLabel, ValueLabel _) -> Right ()
  (SchemaOptionalGlobalUniqueId, ValueOptionalGlobalUniqueId _) -> Right ()
  (SchemaEnum members, ValueEnum member)
    | member `elem` members -> Right ()
  (SchemaRecord expected, ValueRecord actual)
    | Map.keysSet expected /= Map.keysSet actual -> mismatch
    | otherwise ->
        mapM_
          ( \(field, fieldSchema) ->
              maybe mismatch (checkValueSchema fieldSchema) (Map.lookup field actual)
          )
          (Map.toAscList expected)
  _ -> mismatch
  where
    mismatch = Left (ValueSchemaMismatch (viewSchema schema) (viewValue value))

newtype CanonicalValueBytes = CanonicalValueBytes ByteString
  deriving stock (Eq, Ord, Show)

canonicalValueBytes :: Value -> CanonicalValueBytes
canonicalValueBytes = CanonicalValueBytes . encodeValue

canonicalValueByteString :: CanonicalValueBytes -> ByteString
canonicalValueByteString (CanonicalValueBytes bytes) = bytes

decodeCanonicalValue :: ByteString -> Either ValueError Value
decodeCanonicalValue bytes = do
  raw <- case Serialize.decode bytes of
    Left _ -> Left ValueDecodeMalformed
    Right value -> Right value
  value <- admitRawEnvelope raw
  unless (encodeValue value == bytes) (Left ValueDecodeNonCanonicalEncoding)
  Right value

uniqueMap :: (Ord key) => (key -> error) -> [(key, value)] -> Either error (Map key value)
uniqueMap duplicate = go Map.empty
  where
    go accumulator [] = Right accumulator
    go accumulator ((key, value) : remaining)
      | Map.member key accumulator = Left (duplicate key)
      | otherwise = go (Map.insert key value accumulator) remaining

valueDomainTag :: Word64
valueDomainTag = 0x45434c4950532d56 -- ASCII "ECLIPS-V"; a domain tag, not a version.

encodeValue :: Value -> ByteString
encodeValue = Serialize.encode . RawValueEnvelope valueDomainTag . rawValue

data RawValueEnvelope = RawValueEnvelope Word64 RawValue
  deriving stock (Generic)
  deriving anyclass (Serialize)

data RawValue
  = RawValueBool Bool
  | RawValueInt64 Int64
  | RawValueBytes ByteString
  | RawValueText ByteString
  | RawValueGlobalUniqueId ByteString
  | RawValueLabel RawLabel
  | RawValueRecord [(ByteString, RawValue)]
  | RawValueEnum ByteString
  | RawValueOptionalGlobalUniqueId (Maybe ByteString)
  deriving stock (Generic)
  deriving anyclass (Serialize)

type RawLabel = (RawLabelOwner, Word64)

data RawLabelOwner
  = RawVoidLabel
  | RawProcessLabel ByteString
  | RawZombieLabel ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

rawValue :: Value -> RawValue
rawValue = \case
  ValueBool value -> RawValueBool value
  ValueInt64 value -> RawValueInt64 value
  ValueBytes value -> RawValueBytes value
  ValueText value -> RawValueText (Text.encodeUtf8 value)
  ValueGlobalUniqueId value -> RawValueGlobalUniqueId (globalUniqueIdBytes value)
  ValueLabel value -> RawValueLabel (rawLabel value)
  ValueRecord fields ->
    RawValueRecord
      [ (Text.encodeUtf8 (fieldNameText field), rawValue value)
      | (field, value) <- Map.toAscList fields
      ]
  ValueEnum value -> RawValueEnum (Text.encodeUtf8 (enumSymbolText value))
  ValueOptionalGlobalUniqueId value ->
    RawValueOptionalGlobalUniqueId (globalUniqueIdBytes <$> value)

rawLabel :: Label -> RawLabel
rawLabel (owner, generation) = (rawLabelOwner owner, generation)

rawLabelOwner :: LabelOwner -> RawLabelOwner
rawLabelOwner = \case
  VoidLabel -> RawVoidLabel
  ProcessLabel process -> RawProcessLabel (processEpochIdBytes process)
  ZombieLabel process -> RawZombieLabel (processEpochIdBytes process)

admitRawEnvelope :: RawValueEnvelope -> Either ValueError Value
admitRawEnvelope (RawValueEnvelope domainTag raw) = do
  unless (domainTag == valueDomainTag) (Left (ValueDecodeUnknownDomainTag domainTag))
  admitRawValue raw

admitRawValue :: RawValue -> Either ValueError Value
admitRawValue = \case
  RawValueBool value -> Right (boolValue value)
  RawValueInt64 value -> Right (int64Value value)
  RawValueBytes value -> Right (bytesValue value)
  RawValueText bytes -> textValue <$> admitText bytes
  RawValueGlobalUniqueId bytes ->
    case mkGlobalUniqueId bytes of
      Left _ -> Left ValueDecodeInvalidGlobalUniqueId
      Right value -> Right (globalUniqueIdValue value)
  RawValueLabel value -> labelValue <$> admitRawLabel value
  RawValueRecord fields -> recordValue =<< traverse admitRawField fields
  RawValueEnum bytes -> enumValue . enumSymbol <$> admitText bytes
  RawValueOptionalGlobalUniqueId raw ->
    optionalGlobalUniqueIdValue <$> traverse admitGlobalUniqueId raw

admitRawField :: (ByteString, RawValue) -> Either ValueError (FieldName, Value)
admitRawField (rawName, raw) = do
  nameText <- admitText rawName
  name <- case mkFieldName nameText of
    Left fieldError -> Left (ValueDecodeInvalidFieldName fieldError)
    Right field -> Right field
  value <- admitRawValue raw
  Right (name, value)

admitRawLabel :: RawLabel -> Either ValueError Label
admitRawLabel (owner, generation) = (,generation) <$> admitRawLabelOwner owner

admitRawLabelOwner :: RawLabelOwner -> Either ValueError LabelOwner
admitRawLabelOwner = \case
  RawVoidLabel -> Right VoidLabel
  RawProcessLabel bytes -> ProcessLabel <$> admitProcessEpoch bytes
  RawZombieLabel bytes -> ZombieLabel <$> admitProcessEpoch bytes

admitProcessEpoch :: ByteString -> Either ValueError ProcessEpochId
admitProcessEpoch bytes = case mkProcessEpochId bytes of
  Left _ -> Left ValueDecodeInvalidProcessEpoch
  Right process -> Right process

admitGlobalUniqueId :: ByteString -> Either ValueError GlobalUniqueId
admitGlobalUniqueId bytes = case mkGlobalUniqueId bytes of
  Left _ -> Left ValueDecodeInvalidGlobalUniqueId
  Right value -> Right value

admitText :: ByteString -> Either ValueError Text
admitText bytes = case Text.decodeUtf8' bytes of
  Left _ -> Left ValueDecodeInvalidUtf8
  Right value -> Right value
