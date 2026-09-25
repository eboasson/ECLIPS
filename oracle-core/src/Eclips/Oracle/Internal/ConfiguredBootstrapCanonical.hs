{-# LANGUAGE DeriveAnyClass #-}

-- | The single canonical grammar for configured-process bootstrap values.
--
-- This module is deliberately independent of the Oracle command grammar: the
-- this grammar describes immutable full-root genesis assignments only. Dynamic
-- Start commands carry minimal process facts and use no bootstrap transcript.
module Eclips.Oracle.Internal.ConfiguredBootstrapCanonical
  ( ConfiguredBootstrapCanonicalError (..),
    configuredProcessBootstrapTranscriptBytes,
    decodeConfiguredProcessBootstrapTranscriptBytes,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Serialize (Serialize (get), encode)
import Data.Serialize.Get qualified as SerializeGet
import Data.Word (Word8)
import Eclips.Domain.Environment (environmentConnectedShapeCanonicalBytes)
import Eclips.Domain.Identity
  ( NablaSequencing (..),
    bootstrapManifestIdBytes,
    deltaIdBytes,
    globalObjectIdBytes,
    heraldEpochBytes,
    mkBootstrapManifestId,
    mkDeltaId,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkNablaId,
    mkProcessEpochId,
    mkProcessId,
    mkStoreIncarnationId,
    nablaIdBytes,
    processEpochIdBytes,
    processIdBytes,
    storeIncarnationIdBytes,
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (..),
    predefinedSortRoleTag,
  )
import Eclips.Domain.Startup
  ( ConfiguredProcessBootstrap,
    ConfiguredRootBootstrap,
    configuredProcessBootstrapManifestId,
    configuredProcessBootstrapProcessEpochId,
    configuredProcessBootstrapProcessId,
    configuredProcessBootstrapResidence,
    configuredProcessBootstrapRoots,
    configuredRootBootstrap,
    configuredRootBootstrapCatalogueRole,
    configuredRootBootstrapReaderDelta,
    configuredRootBootstrapStoreIncarnation,
    configuredRootBootstrapWriterNabla,
    configuredRootBootstrapWriterSequencing,
    mkConfiguredProcessBootstrap,
  )
import GHC.Generics (Generic)

data ConfiguredBootstrapCanonicalError
  = MalformedConfiguredBootstrapBytes String
  | InvalidConfiguredBootstrapIdentity
  | InvalidConfiguredBootstrapRole Word8
  | InvalidConfiguredProcessBootstrap
  deriving stock (Eq, Show)

configuredProcessBootstrapTranscriptBytes :: ConfiguredProcessBootstrap -> ByteString
configuredProcessBootstrapTranscriptBytes = encode . configuredProcessTranscript

decodeConfiguredProcessBootstrapTranscriptBytes ::
  ByteString -> Either ConfiguredBootstrapCanonicalError ConfiguredProcessBootstrap
decodeConfiguredProcessBootstrapTranscriptBytes bytes =
  decodeCanonical bytes >>= admitConfiguredProcess

data ConfiguredProcessTranscript
  = ConfiguredProcessTranscript
      ByteString
      ByteString
      ByteString
      ByteString
      [ConfiguredRootTranscript]
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ConfiguredRootTranscript
  = ConfiguredRootTranscript
      Word8
      ByteString
      NablaSequencingTranscript
      ByteString
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data NablaSequencingTranscript
  = UnsequencedNablaTranscript
  | NablaSequencedByTranscript ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

configuredProcessTranscript :: ConfiguredProcessBootstrap -> ConfiguredProcessTranscript
configuredProcessTranscript bootstrap =
  ConfiguredProcessTranscript
    (bootstrapManifestIdBytes (configuredProcessBootstrapManifestId bootstrap))
    (processIdBytes (configuredProcessBootstrapProcessId bootstrap))
    (processEpochIdBytes (configuredProcessBootstrapProcessEpochId bootstrap))
    (heraldEpochBytes (configuredProcessBootstrapResidence bootstrap))
    (fmap configuredRootTranscript (configuredProcessBootstrapRoots bootstrap))
    environmentConnectedShapeCanonicalBytes

configuredRootTranscript :: ConfiguredRootBootstrap -> ConfiguredRootTranscript
configuredRootTranscript root =
  ConfiguredRootTranscript
    (predefinedSortRoleTag (configuredRootBootstrapCatalogueRole root))
    (nablaIdBytes (configuredRootBootstrapWriterNabla root))
    (nablaSequencingTranscript (configuredRootBootstrapWriterSequencing root))
    (deltaIdBytes (configuredRootBootstrapReaderDelta root))
    (storeIncarnationIdBytes (configuredRootBootstrapStoreIncarnation root))

nablaSequencingTranscript :: NablaSequencing -> NablaSequencingTranscript
nablaSequencingTranscript sequencing = case sequencing of
  UnsequencedNabla -> UnsequencedNablaTranscript
  NablaSequencedBy object ->
    NablaSequencedByTranscript (globalObjectIdBytes object)

admitConfiguredProcess ::
  ConfiguredProcessTranscript ->
  Either ConfiguredBootstrapCanonicalError ConfiguredProcessBootstrap
admitConfiguredProcess
  (ConfiguredProcessTranscript manifestBytes processIdClaim processEpochBytes residenceBytes rootClaims shapeBytes) = do
    if shapeBytes == environmentConnectedShapeCanonicalBytes
      then Right ()
      else Left InvalidConfiguredProcessBootstrap
    manifest <- admitIdentity (mkBootstrapManifestId manifestBytes)
    processId <- admitIdentity (mkProcessId processIdClaim)
    processEpoch <- admitIdentity (mkProcessEpochId processEpochBytes)
    residence <- admitIdentity (mkHeraldEpoch residenceBytes)
    roots <- traverse admitConfiguredRoot rootClaims
    maybe
      (Left InvalidConfiguredProcessBootstrap)
      Right
      (mkConfiguredProcessBootstrap manifest processId processEpoch residence roots)

admitConfiguredRoot ::
  ConfiguredRootTranscript ->
  Either ConfiguredBootstrapCanonicalError ConfiguredRootBootstrap
admitConfiguredRoot
  (ConfiguredRootTranscript roleTag writerBytes sequencingClaim readerBytes storeBytes) =
    configuredRootBootstrap
      <$> admitPredefinedSortRole roleTag
      <*> admitIdentity (mkNablaId writerBytes)
      <*> admitNablaSequencing sequencingClaim
      <*> admitIdentity (mkDeltaId readerBytes)
      <*> admitIdentity (mkStoreIncarnationId storeBytes)

admitNablaSequencing ::
  NablaSequencingTranscript -> Either ConfiguredBootstrapCanonicalError NablaSequencing
admitNablaSequencing claim = case claim of
  UnsequencedNablaTranscript -> Right UnsequencedNabla
  NablaSequencedByTranscript objectBytes ->
    NablaSequencedBy <$> admitIdentity (mkGlobalObjectId objectBytes)

admitPredefinedSortRole ::
  Word8 -> Either ConfiguredBootstrapCanonicalError PredefinedSortRole
admitPredefinedSortRole tag = case tag of
  0 -> Right SortDefinitionRole
  1 -> Right NeutralVertexRole
  2 -> Right EdgeRole
  3 -> Right NablaRole
  4 -> Right DeltaRole
  5 -> Right ProcessEpochRole
  _ -> Left (InvalidConfiguredBootstrapRole tag)

decodeCanonical ::
  (Serialize value) =>
  ByteString -> Either ConfiguredBootstrapCanonicalError value
decodeCanonical bytes =
  case SerializeGet.runGetState get bytes 0 of
    Left problem -> Left (MalformedConfiguredBootstrapBytes problem)
    Right (value, trailing)
      | ByteString.null trailing -> Right value
      | otherwise -> Left (MalformedConfiguredBootstrapBytes "trailing bytes")

admitIdentity ::
  Either problem value -> Either ConfiguredBootstrapCanonicalError value
admitIdentity = either (const (Left InvalidConfiguredBootstrapIdentity)) Right
