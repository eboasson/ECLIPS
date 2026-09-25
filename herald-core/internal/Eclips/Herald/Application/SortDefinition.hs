-- | Dedicated admission of structured application sort definitions.
--
-- This is the sole bridge from application sort-definition syntax to checked
-- semantic descriptors. Declared syntax supplies only ordinary application
-- mutation and no structural-carrier claim; the separate predefined arm selects
-- one exact catalogue entry. Both directions delegate canonical identity to the
-- domain and never return canonical bytes to the application boundary.
module Eclips.Herald.Application.SortDefinition
  ( ApplicationSortDefinitionRejection (..),
    applicationDescriptorSpec,
    AdmittedApplicationSortDefinition,
    admitApplicationSortDefinition,
    admittedApplicationSortDescriptor,
    admittedApplicationSortId,
    admittedApplicationSortValue,
    admittedApplicationSortWriteResult,
    presentApplicationSortDefinition,
  )
where

import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Eclips.Application.Types.Access qualified as ApplicationAccess
import Eclips.Application.Types.SortDescriptor qualified as Application
import Eclips.Application.Types.Write (WriteResult (SortDefinitionWritten))
import Eclips.Domain.Identity (SortId)
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalCheckedDescriptor,
    canonicalizeCheckedDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( ApplicationMutation (OrdinaryApplicationMutation),
    DescriptorAdmission (ApplicationDescriptor),
    DescriptorError,
    DescriptorSpec (..),
    PredicateExpression,
    RankDirection,
    RankTerm,
    ScalarComparison,
    ScalarLiteral,
    SortKind,
    checkDescriptor,
    descriptorImmutable,
    descriptorKeyProjections,
    descriptorKind,
    descriptorLabelField,
    descriptorMinimumRetentionMicros,
    descriptorObsolescence,
    descriptorRankTerms,
    descriptorSchema,
    descriptorValidity,
  )
import Eclips.Domain.Sort.Descriptor qualified as Domain
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole,
    SortDefinitionIntrinsicError,
    decodeSortDefinitionValue,
    predefinedCatalogueDescriptor,
    predefinedCatalogueRole,
    predefinedCatalogueSortId,
    profileEntryFor,
    profilePredefinedCatalogue,
    sortDefinitionValue,
  )
import Eclips.Domain.Sort.Profile qualified as Profile
import Eclips.Domain.Value
  ( FieldName,
    FieldNameError,
    Projection,
    SchemaError,
    Value,
    ValueSchema,
  )
import Eclips.Domain.Value qualified as Domain

-- | Ordinary rejection categories at the structured definition boundary.
data ApplicationSortDefinitionRejection
  = ApplicationDescriptorInvalidFieldName Text FieldNameError
  | ApplicationDescriptorInvalidSchema SchemaError
  | ApplicationDescriptorRejected DescriptorError
  | ApplicationDescriptorClaimedSortIdMismatch SortId SortId
  deriving stock (Eq, Show)

-- | Translate application-selectable descriptor facts to raw semantic syntax.
--
-- Field names are admitted during this traversal.  Descriptor invariants such as
-- projection types, rank ending, controlled keys, and label placement remain the
-- semantic domain's responsibility.
applicationDescriptorSpec ::
  Application.ApplicationSortDescriptor ->
  Either ApplicationSortDefinitionRejection DescriptorSpec
applicationDescriptorSpec descriptor = do
  schema <- convertSchema (Application.valueSchema descriptor)
  keyProjections <-
    traverse
      convertProjection
      (Application.keyProjections descriptor)
  validity <-
    convertPredicate (Application.validityPredicate descriptor)
  obsolescence <-
    convertPredicate (Application.obsolescencePredicate descriptor)
  rankTerms <-
    traverse
      convertRankTerm
      (Application.rankTerms descriptor)
  labelField <-
    traverse
      convertFieldName
      (Application.labelField descriptor)
  Right
    DescriptorSpec
      { descriptorSpecKind =
          convertSortKind (Application.sortKind descriptor),
        descriptorSpecSchema = schema,
        descriptorSpecKeyProjections = keyProjections,
        descriptorSpecValidity = validity,
        descriptorSpecObsolescence = obsolescence,
        descriptorSpecRankTerms = rankTerms,
        descriptorSpecMinimumRetentionMicros =
          Application.minimumRetentionMicros descriptor,
        descriptorSpecImmutable =
          Application.isImmutable descriptor,
        descriptorSpecLabelField = labelField,
        descriptorSpecApplicationMutation = OrdinaryApplicationMutation,
        descriptorSpecStructuralCarrierRole = Nothing
      }

-- | Checked domain value and public success result produced by one admission.
data AdmittedApplicationSortDefinition
  = AdmittedApplicationSortDefinition CanonicalDescriptor Value WriteResult
  deriving stock (Eq, Show)

-- | Admit, canonicalize, content-address, and materialize one structured draft.
admitApplicationSortDefinition ::
  Application.ApplicationSortDefinition ->
  Either ApplicationSortDefinitionRejection AdmittedApplicationSortDefinition
admitApplicationSortDefinition = \case
  Application.DeclaredSortDefinition descriptor claimedSortId -> do
    specification <- applicationDescriptorSpec descriptor
    checked <-
      either
        (Left . ApplicationDescriptorRejected)
        Right
        (checkDescriptor ApplicationDescriptor specification)
    admitCanonicalDefinition claimedSortId (canonicalizeCheckedDescriptor checked)
  Application.PredefinedSortDefinition role claimedSortId ->
    admitCanonicalDefinition
      (Just claimedSortId)
      (predefinedCatalogueDescriptor (profileEntryFor (domainPredefinedRole role)))

admitCanonicalDefinition ::
  Maybe SortId ->
  CanonicalDescriptor ->
  Either ApplicationSortDefinitionRejection AdmittedApplicationSortDefinition
admitCanonicalDefinition claimedSortId canonical =
  case claimedSortId of
    Just claimed
      | claimed /= computedSortId ->
          Left
            ( ApplicationDescriptorClaimedSortIdMismatch
                claimed
                computedSortId
            )
    _ ->
      Right
        ( AdmittedApplicationSortDefinition
            canonical
            (sortDefinitionValue canonical)
            (SortDefinitionWritten computedSortId)
        )
  where
    computedSortId = descriptorSortId canonical

-- | Recover the closed application syntax from one checked @sort-sort@ value.
--
-- Exact catalogue entries use the predefined arm. Every other admitted payload
-- has necessarily passed the application descriptor path, so its complete draft
-- syntax can be reconstructed without exposing canonical bytes or primordial-
-- only policy fields. The derived identity is always materialized on output.
presentApplicationSortDefinition ::
  Value ->
  Either SortDefinitionIntrinsicError Application.ApplicationSortDefinition
presentApplicationSortDefinition value = do
  canonical <- decodeSortDefinitionValue value
  Right $ case catalogueEntryFor canonical of
    Just entry ->
      Application.PredefinedSortDefinition
        (applicationPredefinedRole (predefinedCatalogueRole entry))
        (predefinedCatalogueSortId entry)
    Nothing ->
      Application.DeclaredSortDefinition
        (applicationDescriptorFromCanonical canonical)
        (Just (descriptorSortId canonical))

catalogueEntryFor ::
  CanonicalDescriptor ->
  Maybe Profile.PredefinedCatalogueEntry
catalogueEntryFor canonical =
  find
    ((== canonical) . predefinedCatalogueDescriptor)
    profilePredefinedCatalogue

applicationDescriptorFromCanonical ::
  CanonicalDescriptor ->
  Application.ApplicationSortDescriptor
applicationDescriptorFromCanonical canonical =
  Application.ApplicationSortDescriptor
    { Application.sortKind =
        applicationSortKind (descriptorKind checked),
      Application.valueSchema =
        applicationSchema (descriptorSchema checked),
      Application.keyProjections =
        fmap applicationProjection (descriptorKeyProjections checked),
      Application.validityPredicate =
        applicationPredicate (descriptorValidity checked),
      Application.obsolescencePredicate =
        applicationPredicate (descriptorObsolescence checked),
      Application.rankTerms =
        fmap applicationRankTerm (descriptorRankTerms checked),
      Application.minimumRetentionMicros =
        descriptorMinimumRetentionMicros checked,
      Application.isImmutable =
        descriptorImmutable checked,
      Application.labelField =
        Domain.fieldNameText <$> descriptorLabelField checked
    }
  where
    checked = canonicalCheckedDescriptor canonical

applicationSortKind :: Domain.SortKind -> Application.ApplicationSortKind
applicationSortKind = \case
  Domain.RegularSort -> Application.RegularSort
  Domain.ControlledSort -> Application.ControlledSort

applicationSchema :: ValueSchema -> Application.ApplicationValueSchema
applicationSchema schema = case Domain.viewSchema schema of
  Domain.BoolSchema -> Application.BoolSchema
  Domain.Int64Schema -> Application.Int64Schema
  Domain.BytesSchema -> Application.BytesSchema
  Domain.TextSchema -> Application.TextSchema
  Domain.GlobalUniqueIdSchema -> Application.UniqueIdSchema
  Domain.LabelSchema -> Application.LabelSchema
  Domain.RecordSchema fields ->
    Application.RecordSchema
      ( Map.fromList
          [ (Domain.fieldNameText field, applicationSchema fieldSchema)
          | (field, fieldSchema) <- Map.toAscList fields
          ]
      )
  Domain.EnumSchema members ->
    Application.EnumSchema (fmap Domain.enumSymbolText members)
  Domain.OptionalGlobalUniqueIdSchema -> Application.OptionalUniqueIdSchema

applicationProjection :: Projection -> Application.ApplicationProjection
applicationProjection projection =
  Application.ApplicationProjection
    (fmap Domain.fieldNameText (Domain.projectionFields projection))

applicationPredicate ::
  Domain.PredicateExpression ->
  Application.ApplicationPredicateExpression
applicationPredicate = \case
  Domain.AlwaysPredicate -> Application.AlwaysPredicate
  Domain.NeverPredicate -> Application.NeverPredicate
  Domain.CompareField projection comparison literal ->
    Application.CompareField
      (applicationProjection projection)
      (applicationComparison comparison)
      (applicationLiteral literal)
  Domain.NotPredicate predicate ->
    Application.NotPredicate (applicationPredicate predicate)
  Domain.AllPredicates predicates ->
    Application.AllPredicates (fmap applicationPredicate predicates)
  Domain.AnyPredicates predicates ->
    Application.AnyPredicates (fmap applicationPredicate predicates)

applicationComparison ::
  Domain.ScalarComparison ->
  Application.ApplicationScalarComparison
applicationComparison = \case
  Domain.ScalarEqual -> Application.ScalarEqual
  Domain.ScalarNotEqual -> Application.ScalarNotEqual
  Domain.ScalarLessThan -> Application.ScalarLessThan
  Domain.ScalarLessThanOrEqual -> Application.ScalarLessThanOrEqual
  Domain.ScalarGreaterThan -> Application.ScalarGreaterThan
  Domain.ScalarGreaterThanOrEqual -> Application.ScalarGreaterThanOrEqual

applicationLiteral ::
  Domain.ScalarLiteral ->
  Application.ApplicationScalarLiteral
applicationLiteral = \case
  Domain.LiteralBool value -> Application.LiteralBool value
  Domain.LiteralInt64 value -> Application.LiteralInt64 value
  Domain.LiteralBytes value -> Application.LiteralBytes value
  Domain.LiteralText value -> Application.LiteralText value
  Domain.LiteralEnum value -> Application.LiteralEnum (Domain.enumSymbolText value)

applicationRankTerm :: Domain.RankTerm -> Application.ApplicationRankTerm
applicationRankTerm = \case
  Domain.RankField projection direction ->
    Application.RankField
      (applicationProjection projection)
      (applicationRankDirection direction)
  Domain.RankApplicationValue direction ->
    Application.RankApplicationValue (applicationRankDirection direction)

applicationRankDirection ::
  Domain.RankDirection ->
  Application.ApplicationRankDirection
applicationRankDirection = \case
  Domain.Ascending -> Application.Ascending
  Domain.Descending -> Application.Descending

domainPredefinedRole ::
  ApplicationAccess.ApplicationPredefinedSortRole ->
  PredefinedSortRole
domainPredefinedRole = \case
  ApplicationAccess.SortDefinitionRole -> Profile.SortDefinitionRole
  ApplicationAccess.NeutralVertexRole -> Profile.NeutralVertexRole
  ApplicationAccess.EdgeRole -> Profile.EdgeRole
  ApplicationAccess.NablaRole -> Profile.NablaRole
  ApplicationAccess.DeltaRole -> Profile.DeltaRole
  ApplicationAccess.ProcessEpochRole -> Profile.ProcessEpochRole

applicationPredefinedRole ::
  PredefinedSortRole ->
  ApplicationAccess.ApplicationPredefinedSortRole
applicationPredefinedRole = \case
  Profile.SortDefinitionRole -> ApplicationAccess.SortDefinitionRole
  Profile.NeutralVertexRole -> ApplicationAccess.NeutralVertexRole
  Profile.EdgeRole -> ApplicationAccess.EdgeRole
  Profile.NablaRole -> ApplicationAccess.NablaRole
  Profile.DeltaRole -> ApplicationAccess.DeltaRole
  Profile.ProcessEpochRole -> ApplicationAccess.ProcessEpochRole

admittedApplicationSortId :: AdmittedApplicationSortDefinition -> SortId
admittedApplicationSortId
  (AdmittedApplicationSortDefinition canonical _ _) =
    descriptorSortId canonical

admittedApplicationSortDescriptor ::
  AdmittedApplicationSortDefinition -> CanonicalDescriptor
admittedApplicationSortDescriptor
  (AdmittedApplicationSortDefinition canonical _ _) = canonical

admittedApplicationSortValue :: AdmittedApplicationSortDefinition -> Value
admittedApplicationSortValue
  (AdmittedApplicationSortDefinition _ value _) = value

admittedApplicationSortWriteResult ::
  AdmittedApplicationSortDefinition ->
  WriteResult
admittedApplicationSortWriteResult
  (AdmittedApplicationSortDefinition _ _ result) = result

convertSortKind :: Application.ApplicationSortKind -> SortKind
convertSortKind = \case
  Application.RegularSort -> Domain.RegularSort
  Application.ControlledSort -> Domain.ControlledSort

convertSchema ::
  Application.ApplicationValueSchema ->
  Either ApplicationSortDefinitionRejection ValueSchema
convertSchema = \case
  Application.BoolSchema -> Right Domain.boolSchema
  Application.Int64Schema -> Right Domain.int64Schema
  Application.BytesSchema -> Right Domain.bytesSchema
  Application.TextSchema -> Right Domain.textSchema
  Application.UniqueIdSchema -> Right Domain.globalUniqueIdSchema
  Application.LabelSchema -> Right Domain.labelSchema
  Application.EnumSchema members ->
    either
      (Left . ApplicationDescriptorInvalidSchema)
      Right
      (Domain.enumSchema (fmap Domain.enumSymbol members))
  Application.OptionalUniqueIdSchema ->
    Right Domain.optionalGlobalUniqueIdSchema
  Application.RecordSchema fields -> do
    converted <- traverse convertSchemaField (Map.toAscList fields)
    Right (Domain.recordSchemaMap (Map.fromAscList converted))

convertSchemaField ::
  (Text, Application.ApplicationValueSchema) ->
  Either ApplicationSortDefinitionRejection (FieldName, ValueSchema)
convertSchemaField (name, schema) = do
  field <- convertFieldName name
  converted <- convertSchema schema
  Right (field, converted)

convertProjection ::
  Application.ApplicationProjection ->
  Either ApplicationSortDefinitionRejection Projection
convertProjection (Application.ApplicationProjection fields) =
  Domain.mkProjection <$> traverse convertFieldName fields

convertFieldName ::
  Text ->
  Either ApplicationSortDefinitionRejection FieldName
convertFieldName name =
  either
    (Left . ApplicationDescriptorInvalidFieldName name)
    Right
    (Domain.mkFieldName name)

convertPredicate ::
  Application.ApplicationPredicateExpression ->
  Either ApplicationSortDefinitionRejection PredicateExpression
convertPredicate = \case
  Application.AlwaysPredicate -> Right Domain.AlwaysPredicate
  Application.NeverPredicate -> Right Domain.NeverPredicate
  Application.CompareField projection comparison literal ->
    Domain.CompareField
      <$> convertProjection projection
      <*> pure (convertComparison comparison)
      <*> pure (convertLiteral literal)
  Application.NotPredicate predicate ->
    Domain.NotPredicate <$> convertPredicate predicate
  Application.AllPredicates predicates ->
    Domain.AllPredicates <$> traverse convertPredicate predicates
  Application.AnyPredicates predicates ->
    Domain.AnyPredicates <$> traverse convertPredicate predicates

convertComparison ::
  Application.ApplicationScalarComparison ->
  ScalarComparison
convertComparison = \case
  Application.ScalarEqual -> Domain.ScalarEqual
  Application.ScalarNotEqual -> Domain.ScalarNotEqual
  Application.ScalarLessThan -> Domain.ScalarLessThan
  Application.ScalarLessThanOrEqual -> Domain.ScalarLessThanOrEqual
  Application.ScalarGreaterThan -> Domain.ScalarGreaterThan
  Application.ScalarGreaterThanOrEqual -> Domain.ScalarGreaterThanOrEqual

convertLiteral :: Application.ApplicationScalarLiteral -> ScalarLiteral
convertLiteral = \case
  Application.LiteralBool value -> Domain.LiteralBool value
  Application.LiteralInt64 value -> Domain.LiteralInt64 value
  Application.LiteralBytes value -> Domain.LiteralBytes value
  Application.LiteralText value -> Domain.LiteralText value
  Application.LiteralEnum value -> Domain.LiteralEnum (Domain.enumSymbol value)

convertRankTerm ::
  Application.ApplicationRankTerm ->
  Either ApplicationSortDefinitionRejection RankTerm
convertRankTerm = \case
  Application.RankField projection direction ->
    Domain.RankField
      <$> convertProjection projection
      <*> pure (convertRankDirection direction)
  Application.RankApplicationValue direction ->
    Right (Domain.RankApplicationValue (convertRankDirection direction))

convertRankDirection :: Application.ApplicationRankDirection -> RankDirection
convertRankDirection = \case
  Application.Ascending -> Domain.Ascending
  Application.Descending -> Domain.Descending
