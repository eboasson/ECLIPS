{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Canonical descriptor bytes and content-addressed sort identity.
--
-- The shared identity-free raw representation has one canonical encoder. Decoding
-- reconstructs the public raw syntax through its smart constructors, runs normal
-- descriptor admission, and requires byte-for-byte re-encoding before returning
-- the opaque checked result. The representation contains a domain-separation tag
-- and deliberately no schema-version or negotiation field.
module Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    CanonicalDescriptorError (..),
    DescriptorDecodeError (..),
    canonicalizeDescriptor,
    canonicalizeCheckedDescriptor,
    decodeCanonicalDescriptor,
    validateCanonicalDescriptorIdentity,
    canonicalCheckedDescriptor,
    canonicalDescriptorBytes,
    descriptorSortId,
  )
where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Eclips.Domain.Identity (SortId)
import Eclips.Domain.Sort.Descriptor
  ( ApplicationMutation (..),
    CheckedDescriptor,
    DescriptorAdmission,
    DescriptorError,
    DescriptorSpec (..),
    PredicateExpression (..),
    RankDirection (..),
    RankTerm (..),
    ScalarComparison (..),
    ScalarLiteral (..),
    SortKind (..),
    StructuralCarrierRole (..),
    checkDescriptor,
    checkedDescriptorSpec,
  )
import Eclips.Domain.Value
  ( FieldName,
    Projection,
    SchemaView (..),
    ValueSchema,
    boolSchema,
    bytesSchema,
    enumSchema,
    enumSymbol,
    enumSymbolText,
    fieldNameText,
    globalUniqueIdSchema,
    int64Schema,
    labelSchema,
    mkFieldName,
    mkProjection,
    optionalGlobalUniqueIdSchema,
    projectionFields,
    recordSchema,
    textSchema,
    viewSchema,
  )
import Eclips.Public.Types.SortCanonical
  ( RawApplicationMutation (..),
    RawDescriptor (..),
    RawPredicate (..),
    RawProjection (..),
    RawRankDirection (..),
    RawRankTerm (..),
    RawScalarComparison (..),
    RawScalarLiteral (..),
    RawSchema (..),
    RawSortKind (..),
    RawStructuralCarrierRole (..),
    canonicalizeRawDescriptor,
    decodeRawDescriptor,
    descriptorDomainTag,
    encodeRawDescriptor,
  )

data CanonicalDescriptor = CanonicalDescriptor
  { checked :: CheckedDescriptor,
    bytes :: ByteString,
    sortId :: SortId
  }
  deriving stock (Eq, Show)

data CanonicalDescriptorError
  = DescriptorAdmissionFailed DescriptorError
  | DescriptorBytesRejected DescriptorDecodeError
  | DescriptorEncodingNotCanonical
  | DescriptorClaimedSortIdMismatch SortId SortId
  deriving stock (Eq, Show)

data DescriptorDecodeError
  = DescriptorDecodeMalformed
  | DescriptorDecodeWrongDomain
  | DescriptorDecodeInvalidText
  | DescriptorDecodeInvalidFieldName
  | DescriptorDecodeInvalidSchema
  | DescriptorDecodeEmptySequence
  deriving stock (Eq, Show)

canonicalizeDescriptor ::
  DescriptorAdmission ->
  DescriptorSpec ->
  Either CanonicalDescriptorError CanonicalDescriptor
canonicalizeDescriptor admission specification = do
  checked <- either (Left . DescriptorAdmissionFailed) Right (checkDescriptor admission specification)
  pure (canonicalizeCheckedDescriptor checked)

-- | Canonicalize an already admitted descriptor without reopening an error path.
--
-- This is useful at typed adapters that first translate and admit raw syntax,
-- then need the unique bytes and content address for exactly that checked value.
canonicalizeCheckedDescriptor :: CheckedDescriptor -> CanonicalDescriptor
canonicalizeCheckedDescriptor checked =
  let (bytes, sortId) =
        canonicalizeRawDescriptor (descriptorToRaw (checkedDescriptorSpec checked))
   in CanonicalDescriptor
        { checked = checked,
          bytes = bytes,
          sortId = sortId
        }

decodeCanonicalDescriptor ::
  DescriptorAdmission ->
  ByteString ->
  Either CanonicalDescriptorError CanonicalDescriptor
decodeCanonicalDescriptor admission bytes = do
  raw <-
    either
      (const (Left (DescriptorBytesRejected DescriptorDecodeMalformed)))
      Right
      (decodeRawDescriptor bytes)
  specification <-
    either (Left . DescriptorBytesRejected) Right (descriptorFromRaw raw)
  checked <- either (Left . DescriptorAdmissionFailed) Right (checkDescriptor admission specification)
  let encoded = encodeCheckedDescriptor checked
  unless (encoded == bytes) (Left DescriptorEncodingNotCanonical)
  pure (canonicalizeCheckedDescriptor checked)

-- | Re-parse a descriptor declaration and require its content address. This is
-- the intrinsic validity check used by the regular predefined sort-definition
-- carrier; sender-supplied identity is never authoritative.
validateCanonicalDescriptorIdentity ::
  DescriptorAdmission ->
  SortId ->
  ByteString ->
  Either CanonicalDescriptorError CanonicalDescriptor
validateCanonicalDescriptorIdentity admission claimedSortId bytes = do
  descriptor <- decodeCanonicalDescriptor admission bytes
  if descriptorSortId descriptor == claimedSortId
    then Right descriptor
    else
      Left
        ( DescriptorClaimedSortIdMismatch
            claimedSortId
            (descriptorSortId descriptor)
        )

canonicalCheckedDescriptor :: CanonicalDescriptor -> CheckedDescriptor
canonicalCheckedDescriptor descriptor = descriptor.checked

canonicalDescriptorBytes :: CanonicalDescriptor -> ByteString
canonicalDescriptorBytes descriptor = descriptor.bytes

descriptorSortId :: CanonicalDescriptor -> SortId
descriptorSortId descriptor = descriptor.sortId

encodeCheckedDescriptor :: CheckedDescriptor -> ByteString
encodeCheckedDescriptor =
  encodeRawDescriptor . descriptorToRaw . checkedDescriptorSpec

descriptorToRaw :: DescriptorSpec -> RawDescriptor
descriptorToRaw specification =
  RawDescriptor
    { domain = descriptorDomainTag,
      kind = sortKindToRaw (descriptorSpecKind specification),
      schema = schemaToRaw (descriptorSpecSchema specification),
      keyProjections =
        map projectionToRaw (descriptorSpecKeyProjections specification),
      validity = predicateToRaw (descriptorSpecValidity specification),
      obsolescence = predicateToRaw (descriptorSpecObsolescence specification),
      rankTerms =
        map rankTermToRaw (NonEmpty.toList (descriptorSpecRankTerms specification)),
      minimumRetentionMicros =
        descriptorSpecMinimumRetentionMicros specification,
      immutable = descriptorSpecImmutable specification,
      labelField =
        encodeFieldName <$> descriptorSpecLabelField specification,
      applicationMutation =
        applicationMutationToRaw (descriptorSpecApplicationMutation specification),
      structuralCarrierRole =
        structuralCarrierRoleToRaw <$> descriptorSpecStructuralCarrierRole specification
    }

descriptorFromRaw :: RawDescriptor -> Either DescriptorDecodeError DescriptorSpec
descriptorFromRaw raw = do
  unless
    (raw.domain == descriptorDomainTag)
    (Left DescriptorDecodeWrongDomain)
  schema <- schemaFromRaw raw.schema
  keyProjections <- traverse projectionFromRaw raw.keyProjections
  validity <- predicateFromRaw raw.validity
  obsolescence <- predicateFromRaw raw.obsolescence
  rankTerms <- nonEmptyFromRaw rankTermFromRaw raw.rankTerms
  labelField <- traverse decodeFieldName raw.labelField
  pure
    DescriptorSpec
      { descriptorSpecKind = sortKindFromRaw raw.kind,
        descriptorSpecSchema = schema,
        descriptorSpecKeyProjections = keyProjections,
        descriptorSpecValidity = validity,
        descriptorSpecObsolescence = obsolescence,
        descriptorSpecRankTerms = rankTerms,
        descriptorSpecMinimumRetentionMicros =
          raw.minimumRetentionMicros,
        descriptorSpecImmutable = raw.immutable,
        descriptorSpecLabelField = labelField,
        descriptorSpecApplicationMutation =
          applicationMutationFromRaw raw.applicationMutation,
        descriptorSpecStructuralCarrierRole =
          structuralCarrierRoleFromRaw <$> raw.structuralCarrierRole
      }

sortKindToRaw :: SortKind -> RawSortKind
sortKindToRaw = \case
  RegularSort -> RawRegularSort
  ControlledSort -> RawControlledSort

sortKindFromRaw :: RawSortKind -> SortKind
sortKindFromRaw = \case
  RawRegularSort -> RegularSort
  RawControlledSort -> ControlledSort

schemaToRaw :: ValueSchema -> RawSchema
schemaToRaw schema = case viewSchema schema of
  BoolSchema -> RawBoolSchema
  Int64Schema -> RawInt64Schema
  BytesSchema -> RawBytesSchema
  TextSchema -> RawTextSchema
  GlobalUniqueIdSchema -> RawGlobalUniqueIdSchema
  LabelSchema -> RawLabelSchema
  RecordSchema fields ->
    RawRecordSchema
      [ (encodeFieldName name, schemaToRaw memberSchema)
      | (name, memberSchema) <- Map.toAscList fields
      ]
  EnumSchema members ->
    RawEnumSchema
      (map (Text.encodeUtf8 . enumSymbolText) (NonEmpty.toList members))
  OptionalGlobalUniqueIdSchema -> RawOptionalGlobalUniqueIdSchema

schemaFromRaw :: RawSchema -> Either DescriptorDecodeError ValueSchema
schemaFromRaw = \case
  RawBoolSchema -> Right boolSchema
  RawInt64Schema -> Right int64Schema
  RawBytesSchema -> Right bytesSchema
  RawTextSchema -> Right textSchema
  RawGlobalUniqueIdSchema -> Right globalUniqueIdSchema
  RawLabelSchema -> Right labelSchema
  RawRecordSchema rawFields -> do
    fields <- traverse fieldFromRaw rawFields
    either
      (const (Left DescriptorDecodeInvalidSchema))
      Right
      (recordSchema fields)
  RawEnumSchema rawMembers -> do
    members <-
      nonEmptyFromRaw
        (fmap enumSymbol . decodeText)
        rawMembers
    either
      (const (Left DescriptorDecodeInvalidSchema))
      Right
      (enumSchema members)
  RawOptionalGlobalUniqueIdSchema -> Right optionalGlobalUniqueIdSchema
  where
    fieldFromRaw (rawName, rawSchema) =
      (,) <$> decodeFieldName rawName <*> schemaFromRaw rawSchema

projectionToRaw :: Projection -> RawProjection
projectionToRaw =
  RawProjection
    . map encodeFieldName
    . NonEmpty.toList
    . projectionFields

projectionFromRaw :: RawProjection -> Either DescriptorDecodeError Projection
projectionFromRaw (RawProjection rawFields) =
  mkProjection <$> nonEmptyFromRaw decodeFieldName rawFields

predicateToRaw :: PredicateExpression -> RawPredicate
predicateToRaw = \case
  AlwaysPredicate -> RawAlwaysPredicate
  NeverPredicate -> RawNeverPredicate
  CompareField projection comparison literal ->
    RawCompareField
      (projectionToRaw projection)
      (scalarComparisonToRaw comparison)
      (scalarLiteralToRaw literal)
  NotPredicate child -> RawNotPredicate (predicateToRaw child)
  AllPredicates children ->
    RawAllPredicates (map predicateToRaw (NonEmpty.toList children))
  AnyPredicates children ->
    RawAnyPredicates (map predicateToRaw (NonEmpty.toList children))

predicateFromRaw :: RawPredicate -> Either DescriptorDecodeError PredicateExpression
predicateFromRaw = \case
  RawAlwaysPredicate -> Right AlwaysPredicate
  RawNeverPredicate -> Right NeverPredicate
  RawCompareField rawProjection rawComparison rawLiteral ->
    CompareField
      <$> projectionFromRaw rawProjection
      <*> pure (scalarComparisonFromRaw rawComparison)
      <*> scalarLiteralFromRaw rawLiteral
  RawNotPredicate child -> NotPredicate <$> predicateFromRaw child
  RawAllPredicates children ->
    AllPredicates <$> nonEmptyFromRaw predicateFromRaw children
  RawAnyPredicates children ->
    AnyPredicates <$> nonEmptyFromRaw predicateFromRaw children

scalarComparisonToRaw :: ScalarComparison -> RawScalarComparison
scalarComparisonToRaw = \case
  ScalarEqual -> RawScalarEqual
  ScalarNotEqual -> RawScalarNotEqual
  ScalarLessThan -> RawScalarLessThan
  ScalarLessThanOrEqual -> RawScalarLessThanOrEqual
  ScalarGreaterThan -> RawScalarGreaterThan
  ScalarGreaterThanOrEqual -> RawScalarGreaterThanOrEqual

scalarComparisonFromRaw :: RawScalarComparison -> ScalarComparison
scalarComparisonFromRaw = \case
  RawScalarEqual -> ScalarEqual
  RawScalarNotEqual -> ScalarNotEqual
  RawScalarLessThan -> ScalarLessThan
  RawScalarLessThanOrEqual -> ScalarLessThanOrEqual
  RawScalarGreaterThan -> ScalarGreaterThan
  RawScalarGreaterThanOrEqual -> ScalarGreaterThanOrEqual

scalarLiteralToRaw :: ScalarLiteral -> RawScalarLiteral
scalarLiteralToRaw = \case
  LiteralBool value -> RawLiteralBool value
  LiteralInt64 value -> RawLiteralInt64 value
  LiteralBytes value -> RawLiteralBytes value
  LiteralText value -> RawLiteralText (Text.encodeUtf8 value)
  LiteralEnum value -> RawLiteralEnum (Text.encodeUtf8 (enumSymbolText value))

scalarLiteralFromRaw :: RawScalarLiteral -> Either DescriptorDecodeError ScalarLiteral
scalarLiteralFromRaw = \case
  RawLiteralBool value -> Right (LiteralBool value)
  RawLiteralInt64 value -> Right (LiteralInt64 value)
  RawLiteralBytes value -> Right (LiteralBytes value)
  RawLiteralText value -> LiteralText <$> decodeText value
  RawLiteralEnum value -> LiteralEnum . enumSymbol <$> decodeText value

rankTermToRaw :: RankTerm -> RawRankTerm
rankTermToRaw = \case
  RankField projection direction ->
    RawRankField (projectionToRaw projection) (rankDirectionToRaw direction)
  RankApplicationValue direction ->
    RawRankApplicationValue (rankDirectionToRaw direction)

rankTermFromRaw :: RawRankTerm -> Either DescriptorDecodeError RankTerm
rankTermFromRaw = \case
  RawRankField projection direction ->
    RankField <$> projectionFromRaw projection <*> pure (rankDirectionFromRaw direction)
  RawRankApplicationValue direction ->
    Right (RankApplicationValue (rankDirectionFromRaw direction))

rankDirectionToRaw :: RankDirection -> RawRankDirection
rankDirectionToRaw = \case
  Ascending -> RawAscending
  Descending -> RawDescending

rankDirectionFromRaw :: RawRankDirection -> RankDirection
rankDirectionFromRaw = \case
  RawAscending -> Ascending
  RawDescending -> Descending

applicationMutationToRaw :: ApplicationMutation -> RawApplicationMutation
applicationMutationToRaw = \case
  OrdinaryApplicationMutation -> RawOrdinaryApplicationMutation
  HeraldManagedMutation -> RawHeraldManagedMutation

applicationMutationFromRaw :: RawApplicationMutation -> ApplicationMutation
applicationMutationFromRaw = \case
  RawOrdinaryApplicationMutation -> OrdinaryApplicationMutation
  RawHeraldManagedMutation -> HeraldManagedMutation

structuralCarrierRoleToRaw :: StructuralCarrierRole -> RawStructuralCarrierRole
structuralCarrierRoleToRaw = \case
  NeutralVertexCarrier -> RawNeutralVertexCarrier
  EdgeCarrier -> RawEdgeCarrier
  NablaCarrier -> RawNablaCarrier
  DeltaCarrier -> RawDeltaCarrier
  ProcessEpochCarrier -> RawProcessEpochCarrier

structuralCarrierRoleFromRaw :: RawStructuralCarrierRole -> StructuralCarrierRole
structuralCarrierRoleFromRaw = \case
  RawNeutralVertexCarrier -> NeutralVertexCarrier
  RawEdgeCarrier -> EdgeCarrier
  RawNablaCarrier -> NablaCarrier
  RawDeltaCarrier -> DeltaCarrier
  RawProcessEpochCarrier -> ProcessEpochCarrier

encodeFieldName :: FieldName -> ByteString
encodeFieldName = Text.encodeUtf8 . fieldNameText

decodeFieldName :: ByteString -> Either DescriptorDecodeError FieldName
decodeFieldName bytes = do
  name <- decodeText bytes
  either
    (const (Left DescriptorDecodeInvalidFieldName))
    Right
    (mkFieldName name)

decodeText :: ByteString -> Either DescriptorDecodeError Text
decodeText =
  either
    (const (Left DescriptorDecodeInvalidText))
    Right
    . Text.decodeUtf8'

nonEmptyFromRaw ::
  (raw -> Either DescriptorDecodeError value) ->
  [raw] ->
  Either DescriptorDecodeError (NonEmpty value)
nonEmptyFromRaw convert = \case
  [] -> Left DescriptorDecodeEmptySequence
  first : remaining ->
    (:|)
      <$> convert first
      <*> traverse convert remaining
