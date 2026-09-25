{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Retained startup transfer material. Decoding proves only canonical shape;
-- a current peer admits the origin, then ordinary local owners interpret the
-- retained control, structural, definition and publication evidence.
module Eclips.Herald.Application.Primordial.Transfer
  ( StartupTransfer,
    StartupTransferError (..),
    captureStartupTransfer,
    encodeStartupTransfer,
    decodeStartupTransfer,
    transferParent,
    transferSourceHerald,
    admitStartupTransfer,
  ) where

import Control.Monad (foldM, unless)
import Data.ByteString (ByteString)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Serialize qualified as Serialize
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
import Eclips.Domain.Publication
import Eclips.Domain.Sort.Canonical
import Eclips.Domain.Sort.Descriptor (DescriptorAdmission (PrimordialDescriptor))
import Eclips.Domain.Sort.Profile (PredefinedSortRole (SortDefinitionRole), predefinedCatalogueDescriptor, profileEntryFor, sortDefinitionValue)
import Eclips.Domain.Structural
import Eclips.Domain.Value (Value, canonicalValueByteString, decodeCanonicalValue)
import Eclips.Herald.Application.Primordial qualified as Primordial
import Eclips.Herald.Bootstrap qualified as Bootstrap
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.Progress qualified as Progress
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.SortRegistry.State qualified as Registry
import Eclips.Herald.Startup.State
import GHC.Generics (Generic)

-- All byte strings below are parsed before the opaque outer value is returned.
-- Lists have one canonical ascending order; repeated aliases share identity.
data RawEntry = RawEntry ByteString Word8 ByteString (Maybe ByteString)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
data RawPublication = RawPublication ByteString ByteString ByteString Word64
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
data RawObservation = RawObservation ByteString ByteString RawPublication ByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- Records are ordered by object; each retains its first observation before the
-- optional later winner. Publication byte order is not temporal order.
data RawRecord = RawRecord ByteString RawObservation (Maybe RawObservation)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
data RawDefinition = RawDefinition ByteString ByteString RawPublication
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
data RawTransfer = RawTransfer
  { system :: ByteString,
    source :: ByteString,
    parent :: ByteString,
    control :: Word64,
    structural :: [(ByteString, Word64)],
    entries :: [RawEntry],
    required :: [ByteString],
    environmentSources :: Maybe (ByteString, ByteString),
    sequencing :: [(ByteString, ByteString)],
    definitions :: [RawDefinition],
    observations :: [RawRecord]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
newtype StartupTransfer = StartupTransfer RawTransfer deriving stock (Eq, Show)
data StartupTransferError
  = StartupTransferMalformed
  | StartupTransferSourceMismatch
  | StartupTransferObjectUnavailable
  deriving stock (Eq, Show)

encodeStartupTransfer :: StartupTransfer -> ByteString
encodeStartupTransfer (StartupTransfer raw) = Serialize.encode raw

decodeStartupTransfer :: ByteString -> Either StartupTransferError StartupTransfer
decodeStartupTransfer bytes = do
  raw <- shape (Serialize.decode bytes)
  unless (Serialize.encode raw == bytes) (Left StartupTransferMalformed)
  _ <- shape (mkSystemId raw.system)
  _ <- shape (mkHeraldEpoch raw.source)
  _ <- shape (mkProcessEpochId raw.parent)
  vector <- traverse (\(epoch, sequenceNumber) -> (,sequenceNumber) <$> shape (mkHeraldEpoch epoch)) raw.structural
  canonicalKeys (map fst vector)
  entries <- traverse entryFromRaw raw.entries
  canonicalKeys (map fst entries)
  required <- traverse text raw.required
  canonicalKeys required
  sources <- traverse (\(writer, reader) -> (,) <$> text writer <*> text reader) raw.environmentSources
  sequencing <- traverse (\(writer, object) -> (,) <$> shape (mkNablaId writer) <*> shape (mkGlobalObjectId object)) raw.sequencing
  canonicalKeys (map fst sequencing)
  _ <- traverse definitionFromRaw raw.definitions
  canonicalKeys (map Serialize.encode raw.definitions)
  recordObjects <- traverse recordShape raw.observations
  canonicalKeys recordObjects
  shape (Primordial.checkTransferredPrimordialShape (Map.fromList entries) (Set.fromList required) sources (Map.fromList sequencing))
  pure (StartupTransfer raw)

transferParent :: StartupTransfer -> ProcessEpochId
transferParent (StartupTransfer raw) = decoded (mkProcessEpochId raw.parent)
transferSourceHerald :: StartupTransfer -> HeraldEpoch
transferSourceHerald (StartupTransfer raw) = decoded (mkHeraldEpoch raw.source)

captureStartupTransfer :: ProcessEpochId -> HeraldEpoch -> Primordial.CheckedPrimordialGrant -> HeraldState -> Either StartupTransferError StartupTransfer
captureStartupTransfer parent source grant state = do
  unless
    ( source == Genesis.checkedLocalHeraldEpoch (startupGenesis state)
        && Projection.oracleViewProcessResidence parent view == Just source
        && Projection.oracleViewProcessIsLive parent view
    )
    (Left StartupTransferSourceMismatch)
  let objects = Set.fromList (map Controlled.controlledGrantObject (Primordial.primordialGrantPossessions grant))
      records = [record | object <- Set.toAscList objects, Just record <- [Controlled.controlledLocalRecord object controlled]]
      observations =
        map
          ( \record ->
              let first = Controlled.controlledRecordFirstPublication record
                  latest = Controlled.controlledRecordLatestPublication record
                  rawPublication = observationToRaw (Controlled.controlledRecordOccurrenceId record)
               in RawRecord
                    (globalObjectIdBytes (Controlled.controlledRecordObjectId record))
                    (rawPublication first)
                    (if first == latest then Nothing else Just (rawPublication latest))
          )
          records
      sorts = Set.fromList (map Controlled.controlledRecordSortId records)
  definitions <- traverse definitionFor (Set.toAscList sorts)
  let raw =
        RawTransfer
          (systemIdBytes (Genesis.checkedSystemId (startupGenesis state)))
          (heraldEpochBytes source)
          (processEpochIdBytes parent)
          (controlIndexWord64 (Projection.oracleViewControlIndex view))
          [ (heraldEpochBytes epoch, maybe 0 structuralSequenceWord64 (structuralPrefixSequence prefix))
          | (epoch, prefix) <- structuralVersionVectorEntries (Progress.structuralAppliedVector (startupStructuralProgressState state))
          ]
          [entryToRaw key entry | (key, entry) <- Map.toAscList (Primordial.primordialGrantEntries grant)]
          (map Text.encodeUtf8 (Set.toAscList (Primordial.primordialGrantRequiredEndpoints grant)))
          (fmap (\(a, b) -> (Text.encodeUtf8 a, Text.encodeUtf8 b)) (Primordial.primordialGrantEnvironmentSources grant))
          [(nablaIdBytes writer, globalObjectIdBytes object) | (writer, object) <- Map.toAscList (Primordial.primordialGrantSequencing grant)]
          (sortOn Serialize.encode definitions)
          observations
  decodeStartupTransfer (Serialize.encode raw)
  where
    controlled = startupControlledState state
    view = Projection.oracleView (startupOracleProjectionState state)
    definitionFor sort = do
      entry <- maybe (Left StartupTransferObjectUnavailable) Right (Registry.lookupEffectiveSort sort (startupSortRegistryState state))
      pure
        ( RawDefinition
            (canonicalDescriptorBytes (Registry.registryEntryDescriptor entry))
            (sortDefinitionOccurrenceIdBytes (Registry.registryEntryOccurrenceId entry))
            (publicationToRaw (Registry.registryEntryPublicationId entry))
        )

-- | The source epoch has already passed peer admission. Its historical process
-- residence is checked at acceptance, so parent End or source retirement after
-- retention cannot revoke this grant. No parent private namespace is consulted.
admitStartupTransfer :: HeraldEpoch -> StartupTransfer -> HeraldState -> Either StartupTransferError (Maybe (Primordial.CheckedPrimordialGrant, HeraldState))
admitStartupTransfer source transfer@(StartupTransfer raw) state
  | source /= transferSourceHerald transfer || raw.system /= systemIdBytes (Genesis.checkedSystemId (startupGenesis state)) = Left StartupTransferSourceMismatch
  | Projection.oracleViewControlIndex view < controlIndex raw.control = Right Nothing
  | otherwise = do
      unless
        ( Projection.oracleProjectionProcessResidenceAt (controlIndex raw.control) (transferParent transfer) oracle == Just source
            && Projection.oracleProjectionProcessIsLiveAt (controlIndex raw.control) (transferParent transfer) oracle
        )
        (Left StartupTransferSourceMismatch)
      if not vectorCovered
        then Right Nothing
        else do
          entries <- Map.fromList <$> traverse entryFromRaw raw.entries
          let objects = [globalObjectIdFromGlobalUniqueId (Primordial.globalPrimordialIdentity entry) | entry <- Map.elems entries, case entry of Primordial.GlobalIdentity _ -> False; _ -> True]
          observedGenesis <- available (Bootstrap.observeCanonicalBootstrapObjects (Projection.oracleViewSystemId (Projection.oracleView oracle)) (map snd (Projection.projectedBootstraps oracle)) (Set.fromList objects) (startupControlledState state))
          registry <- foldM installDefinition (startupSortRegistryState state) raw.definitions
          controlled <- foldM (installRecord registry) observedGenesis raw.observations
          if any (\object -> Controlled.controlledObjectReleasedDeleted object controlled || Controlled.controlledNamedProcessEnded object controlled) objects
            then Left StartupTransferObjectUnavailable
            else
              if any (\object -> not (Controlled.controlledObjectCurrent object controlled)) objects
                then Right Nothing
                else do
                  required <- Set.fromList <$> traverse text raw.required
                  sources <- traverse (\(a, b) -> (,) <$> text a <*> text b) raw.environmentSources
                  sequencing <- Map.fromList <$> traverse (\(a, b) -> (,) <$> shape (mkNablaId a) <*> shape (mkGlobalObjectId b)) raw.sequencing
                  grant <- available (Primordial.admitTransferredPrimordialGrant entries required sources sequencing controlled (startupStructuralProgressState state))
                  pure (Just (grant, replaceStartupSortRegistryState registry (replaceStartupControlledState controlled state)))
  where
    oracle = startupOracleProjectionState state
    view = Projection.oracleView oracle
    applied =
      Map.fromList
        [ (heraldEpochBytes epoch, maybe 0 structuralSequenceWord64 (structuralPrefixSequence prefix))
        | (epoch, prefix) <- structuralVersionVectorEntries (Progress.structuralAppliedVector (startupStructuralProgressState state))
        ]
    vectorCovered = all coversComponent raw.structural
    coversComponent (_, 0) = True
    coversComponent (epoch, required) = case Map.lookup epoch applied of
      Just current -> current >= required
      Nothing -> all (retainedOccurrence epoch) [1 .. required]
    -- Retirement removes the component from the current membership vector but
    -- does not erase the exact applied occurrences in its retained history.
    retainedOccurrence epoch sequenceNumber = case (mkHeraldEpoch epoch, mkStructuralSequence sequenceNumber) of
      (Right sourceEpoch, Right sequenceValue) ->
        case Progress.lookupAppliedStructuralOccurrence (structuralOccurrenceId sourceEpoch sequenceValue) (startupStructuralProgressState state) of
          Just _ -> True
          Nothing -> False
      _ -> False
    installDefinition registry rawDefinition = do
      (descriptor, occurrence, publication) <- definitionFromRaw rawDefinition
      plan <- available (Registry.planSortInduction (Genesis.checkedSystemId (startupGenesis state)) descriptor registry)
      unless (Registry.sortInductionPlanOccurrenceId plan == occurrence) (Left StartupTransferObjectUnavailable)
      prepared <- available (Registry.prepareSortInduction descriptor occurrence publication registry)
      pure (Registry.commitSortInduction prepared)
    installRecord registry controlled (RawRecord bytes first latest) = do
      object <- shape (mkGlobalObjectId bytes)
      -- The receiver owns its immutable first observation. Source-first evidence
      -- establishes only an absent record; its retained latest winner is merged
      -- without replacing an already established receiver history.
      case Controlled.controlledLocalRecord object controlled of
        Nothing -> do
          firstObserved <- installObservation registry object controlled first
          maybe (Right firstObserved) (installObservation registry object firstObserved) latest
        Just _ -> do
          _ <- checkObservation registry object first
          installObservation registry object controlled (maybe first id latest)
    checkObservation registry object rawObservation = do
      (sort, occurrence, identifier, value) <- observationFromRaw rawObservation
      entry <- maybe (Left StartupTransferObjectUnavailable) Right (Registry.lookupEffectiveSort sort registry)
      unless (Registry.registryEntryOccurrenceId entry == occurrence) (Left StartupTransferObjectUnavailable)
      publication <- available (mkCheckedPublication (Registry.registryEntryDescriptor entry) identifier value)
      observation <- available (Controlled.checkControlledObservation (Registry.registryEntryDescriptor entry) occurrence publication)
      unless (Controlled.checkedControlledObservationObject observation == object) (Left StartupTransferMalformed)
      pure observation
    installObservation registry object controlled rawObservation = do
      observation <- checkObservation registry object rawObservation
      prepared <- available (Controlled.prepareControlledStartupObservation observation controlled)
      pure (fst (Controlled.commitControlledPeerObservation prepared))

entryToRaw :: Text -> Primordial.GlobalPrimordialEntry -> RawEntry
entryToRaw key entry = RawEntry (Text.encodeUtf8 key) tag (globalUniqueIdBytes (Primordial.globalPrimordialIdentity entry)) sort
  where
    (tag, sort) = case entry of
      Primordial.GlobalIdentity _ -> (0, Nothing)
      Primordial.GlobalObject _ value -> (1, Just (sortIdBytes value))
      Primordial.GlobalWriter _ value -> (2, Just (sortIdBytes value))
      Primordial.GlobalReader _ value -> (3, Just (sortIdBytes value))
      Primordial.GlobalProcess _ -> (4, Nothing)
entryFromRaw :: RawEntry -> Either StartupTransferError (Text, Primordial.GlobalPrimordialEntry)
entryFromRaw (RawEntry key tag identity sort) = do
  name <- text key
  unique <- shape (mkGlobalUniqueId identity)
  let object = globalObjectIdFromGlobalUniqueId unique
  entry <- case (tag, sort) of
    (0, Nothing) -> pure (Primordial.GlobalIdentity unique)
    (1, Just value) -> Primordial.GlobalObject object <$> shape (mkSortId value)
    (2, Just value) -> Primordial.GlobalWriter (nablaIdFromGlobalObjectId object) <$> shape (mkSortId value)
    (3, Just value) -> Primordial.GlobalReader (deltaIdFromGlobalObjectId object) <$> shape (mkSortId value)
    (4, Nothing) -> pure (Primordial.GlobalProcess (processEpochIdFromGlobalObjectId object))
    _ -> Left StartupTransferMalformed
  pure (name, entry)

publicationToRaw :: PublicationId -> RawPublication
publicationToRaw identifier =
  RawPublication
    (nablaIdBytes (publicationNabla identifier))
    (authorityEpochCanonicalBytes (publicationAuthorityEpoch identifier))
    (heraldEpochBytes (publicationSourceHeraldEpoch identifier))
    (nablaSequenceWord64 (publicationNablaSequence identifier))
publicationFromRaw :: RawPublication -> Either StartupTransferError PublicationId
publicationFromRaw (RawPublication writer authority source sequenceNumber) =
  publicationId
    <$> shape (mkNablaId writer)
    <*> shape (decodeAuthorityEpochCanonicalBytes authority)
    <*> shape (mkHeraldEpoch source)
    <*> pure (nablaSequence sequenceNumber)
observationToRaw :: SortDefinitionOccurrenceId -> CheckedPublication -> RawObservation
observationToRaw occurrence publication =
  RawObservation
    (sortIdBytes (checkedPublicationSort publication))
    (sortDefinitionOccurrenceIdBytes occurrence)
    (publicationToRaw (checkedPublicationId publication))
    (canonicalValueByteString (checkedPublicationCanonicalValue publication))
observationFromRaw :: RawObservation -> Either StartupTransferError (SortId, SortDefinitionOccurrenceId, PublicationId, Value)
observationFromRaw (RawObservation sort occurrence identifier bytes) =
  (,,,)
    <$> shape (mkSortId sort)
    <*> shape (mkSortDefinitionOccurrenceId occurrence)
    <*> publicationFromRaw identifier
    <*> shape (decodeCanonicalValue bytes)
recordShape :: RawRecord -> Either StartupTransferError GlobalObjectId
recordShape (RawRecord bytes first latest) = do
  object <- shape (mkGlobalObjectId bytes)
  _ <- observationFromRaw first
  _ <- traverse observationFromRaw latest
  unless (latest /= Just first) (Left StartupTransferMalformed)
  pure object

definitionFromRaw :: RawDefinition -> Either StartupTransferError (CanonicalDescriptor, SortDefinitionOccurrenceId, CheckedPublication)
definitionFromRaw (RawDefinition bytes occurrence identifier) = do
  descriptor <- shape (decodeCanonicalDescriptor PrimordialDescriptor bytes)
  publicationIdentifier <- publicationFromRaw identifier
  publication <-
    shape
      ( mkCheckedPublication
          (predefinedCatalogueDescriptor (profileEntryFor SortDefinitionRole))
          publicationIdentifier
          (sortDefinitionValue descriptor)
      )
  (descriptor,,publication) <$> shape (mkSortDefinitionOccurrenceId occurrence)
text :: ByteString -> Either StartupTransferError Text
text = shape . Text.decodeUtf8'
shape :: Either problem value -> Either StartupTransferError value
shape = either (const (Left StartupTransferMalformed)) Right
available :: Either problem value -> Either StartupTransferError value
available = either (const (Left StartupTransferObjectUnavailable)) Right
canonicalKeys :: (Ord value) => [value] -> Either StartupTransferError ()
canonicalKeys values = unless (Set.toAscList (Set.fromList values) == values) (Left StartupTransferMalformed)
decoded :: (Show problem) => Either problem value -> value
decoded = either (error . show) id
