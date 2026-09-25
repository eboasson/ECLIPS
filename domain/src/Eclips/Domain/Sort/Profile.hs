{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Closed profile-0.1 sort catalogue and carrier-specific admission rules.
--
-- The portable descriptor language deliberately contains no callback-like
-- validity predicate.  The regular sort-definition carrier therefore has one
-- closed intrinsic at the checked-publication boundary: its embedded descriptor
-- must be canonical and its claimed content address must match.
module Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (..),
    allPredefinedSortRoles,
    predefinedSortRoleTag,
    PredefinedCatalogueEntry,
    predefinedCatalogueRole,
    predefinedCatalogueDescriptor,
    predefinedCatalogueSortId,
    profilePredefinedCatalogue,
    profileEntryFor,
    profileSortFor,
    CatalogueDigest,
    catalogueDigestBytes,
    DigestError (..),
    mkCatalogueDigest,
    profileCatalogueDigest,
    sortDefinitionValue,
    SortDefinitionIntrinsicError (..),
    decodeSortDefinitionValue,
    StructuralCarrierReferenceError (..),
    structuralCarrierSortReferences,
    validateProfilePublicationIntrinsic,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize, encode)
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word8)
import Eclips.Domain.Identity
  ( SortId,
    mkSortId,
    sortIdBytes,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    CanonicalDescriptorError (..),
    canonicalCheckedDescriptor,
    canonicalDescriptorBytes,
    decodeCanonicalDescriptor,
    descriptorSortId,
    validateCanonicalDescriptorIdentity,
  )
import Eclips.Domain.Sort.Descriptor
  ( DescriptorAdmission (..),
    StructuralCarrierRole (..),
    descriptorStructuralCarrierRole,
  )
import Eclips.Domain.Value
  ( FieldName,
    Value,
    ValueView (..),
    bytesValue,
    directProjection,
    mkFieldName,
    recordValue,
    valueAt,
    viewValue,
  )
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import Eclips.Public.Types.SortCanonical (encodeRawDescriptor)
import Eclips.Public.Types.SortCatalogue
  ( PredefinedSortRole (..),
    allPredefinedSortRoles,
    predefinedRawDescriptor,
    predefinedSortRoleTag,
  )
import GHC.Generics (Generic)

data PredefinedCatalogueEntry
  = PredefinedCatalogueEntry
      PredefinedSortRole
      CanonicalDescriptor
      SortId
  deriving stock (Eq, Show)

predefinedCatalogueRole :: PredefinedCatalogueEntry -> PredefinedSortRole
predefinedCatalogueRole (PredefinedCatalogueEntry role _ _) = role

predefinedCatalogueDescriptor :: PredefinedCatalogueEntry -> CanonicalDescriptor
predefinedCatalogueDescriptor (PredefinedCatalogueEntry _ descriptor _) = descriptor

predefinedCatalogueSortId :: PredefinedCatalogueEntry -> SortId
predefinedCatalogueSortId (PredefinedCatalogueEntry _ _ sortId) = sortId

newtype CatalogueDigest = CatalogueDigest ByteString
  deriving stock (Eq, Ord)

instance Show CatalogueDigest where
  show = renderGroupedHex . catalogueDigestBytes

catalogueDigestBytes :: CatalogueDigest -> ByteString
catalogueDigestBytes (CatalogueDigest bytes) = bytes

data DigestError = WrongDigestByteCount
  { expectedDigestByteCount :: Int,
    actualDigestByteCount :: Int
  }
  deriving stock (Eq, Show)

mkCatalogueDigest :: ByteString -> Either DigestError CatalogueDigest
mkCatalogueDigest bytes
  | ByteString.length bytes == sha256ByteCount = Right (CatalogueDigest bytes)
  | otherwise =
      Left
        WrongDigestByteCount
          { expectedDigestByteCount = sha256ByteCount,
            actualDigestByteCount = ByteString.length bytes
          }

sha256ByteCount :: Int
sha256ByteCount = 32

profilePredefinedCatalogue :: [PredefinedCatalogueEntry]
profilePredefinedCatalogue = fmap profileEntryFor allPredefinedSortRoles

profileEntryFor :: PredefinedSortRole -> PredefinedCatalogueEntry
profileEntryFor role =
  let descriptor =
        profileInvariant
          ("descriptor for " <> show role)
          (decodeCanonicalDescriptor PrimordialDescriptor (encodeRawDescriptor (predefinedRawDescriptor role)))
   in PredefinedCatalogueEntry role descriptor (descriptorSortId descriptor)

profileSortFor :: PredefinedSortRole -> SortId
profileSortFor = predefinedCatalogueSortId . profileEntryFor

profileCatalogueDigest :: CatalogueDigest
profileCatalogueDigest =
  profileInvariant
    "catalogue digest"
    (mkCatalogueDigest (hashTranscript transcript))
  where
    transcript =
      CatalogueDigestTranscript
        "ECLIPS-CATALOGUE"
        (fmap catalogueEntryTranscript profilePredefinedCatalogue)

data CatalogueDigestTranscript
  = CatalogueDigestTranscript
      ByteString
      [CatalogueEntryTranscript]
  deriving stock (Generic)
  deriving anyclass (Serialize)

data CatalogueEntryTranscript = CatalogueEntryTranscript Word8 ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

catalogueEntryTranscript :: PredefinedCatalogueEntry -> CatalogueEntryTranscript
catalogueEntryTranscript entry =
  CatalogueEntryTranscript
    (predefinedSortRoleTag (predefinedCatalogueRole entry))
    (canonicalDescriptorBytes (predefinedCatalogueDescriptor entry))

sortDefinitionValue :: CanonicalDescriptor -> Value
sortDefinitionValue descriptor =
  profileInvariant
    "sort-definition value"
    ( recordValue
        [ (sortIdField, bytesValue (sortIdBytes (descriptorSortId descriptor))),
          (descriptorBytesField, bytesValue (canonicalDescriptorBytes descriptor))
        ]
    )

-- | Rejection categories unique to the closed sort-definition carrier.
data SortDefinitionIntrinsicError
  = SortDefinitionIntrinsicExpectedRecord
  | SortDefinitionIntrinsicMissingSortId
  | SortDefinitionIntrinsicSortIdNotBytes
  | SortDefinitionIntrinsicMissingDescriptor
  | SortDefinitionIntrinsicDescriptorNotBytes
  | SortDefinitionIntrinsicClaimedSortIdWrongSize
  | SortDefinitionIntrinsicDescriptorRejected
  | SortDefinitionIntrinsicClaimedSortIdMismatch
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Apply the one carrier-specific intrinsic, or do nothing for every other
-- descriptor.  This function is pure and is called only by the sole checked
-- publication constructor.
validateProfilePublicationIntrinsic ::
  CanonicalDescriptor ->
  Value ->
  Either SortDefinitionIntrinsicError ()
validateProfilePublicationIntrinsic descriptor value
  | descriptor /= sortDefinitionDescriptor = Right ()
  | otherwise = () <$ decodeSortDefinitionValue value
  where
    sortDefinitionDescriptor =
      predefinedCatalogueDescriptor (profileEntryFor SortDefinitionRole)

-- | Decode and revalidate the checked payload carried by @sort-sort@.
--
-- Exact catalogue bytes are admitted through the primordial path; every other
-- descriptor is admitted through the application path. The returned descriptor
-- has therefore passed canonical re-encoding and public-identity recomputation,
-- and no canonical bytes need cross the application boundary.
decodeSortDefinitionValue ::
  Value ->
  Either SortDefinitionIntrinsicError CanonicalDescriptor
decodeSortDefinitionValue value = do
  fields <- case viewValue value of
    RecordValue members -> Right members
    _ -> Left SortDefinitionIntrinsicExpectedRecord
  claimedValue <-
    maybe
      (Left SortDefinitionIntrinsicMissingSortId)
      Right
      (Map.lookup sortIdField fields)
  claimedBytes <- case viewValue claimedValue of
    BytesValue bytes -> Right bytes
    _ -> Left SortDefinitionIntrinsicSortIdNotBytes
  descriptorValue <-
    maybe
      (Left SortDefinitionIntrinsicMissingDescriptor)
      Right
      (Map.lookup descriptorBytesField fields)
  descriptorBytes <- case viewValue descriptorValue of
    BytesValue bytes -> Right bytes
    _ -> Left SortDefinitionIntrinsicDescriptorNotBytes
  claimedSortId <-
    either
      (const (Left SortDefinitionIntrinsicClaimedSortIdWrongSize))
      Right
      (mkSortId claimedBytes)
  case validateCanonicalDescriptorIdentity
    (descriptorAdmissionFor descriptorBytes)
    claimedSortId
    descriptorBytes of
    Right descriptor -> Right descriptor
    Left DescriptorClaimedSortIdMismatch {} ->
      Left SortDefinitionIntrinsicClaimedSortIdMismatch
    Left _ -> Left SortDefinitionIntrinsicDescriptorRejected

-- | Decode the public sort identities carried by the closed structural root
-- values. Profile 0.1 Nabla and Delta carriers each name exactly one sort; the
-- other carrier roles name none. Keeping this decoder next to the closed
-- catalogue schema gives local and peer admission one definition of the
-- embedded dependency.
data StructuralCarrierReferenceError
  = StructuralCarrierReferenceValueShapeContradiction StructuralCarrierRole
  deriving stock (Eq, Ord, Show)

structuralCarrierSortReferences ::
  CanonicalDescriptor ->
  Value ->
  Either StructuralCarrierReferenceError [SortId]
structuralCarrierSortReferences descriptor value =
  case descriptorStructuralCarrierRole (canonicalCheckedDescriptor descriptor) of
    Just role@NablaCarrier -> pure <$> carriedSort role
    Just role@DeltaCarrier -> pure <$> carriedSort role
    _ -> Right []
  where
    carriedSort role = do
      projected <-
        either
          (const (shapeContradiction role))
          Right
          (valueAt (directProjection sortIdField) value)
      case viewValue projected of
        BytesValue bytes ->
          either
            (const (shapeContradiction role))
            Right
            (mkSortId bytes)
        _ -> shapeContradiction role
    shapeContradiction role =
      Left (StructuralCarrierReferenceValueShapeContradiction role)

descriptorAdmissionFor :: ByteString -> DescriptorAdmission
descriptorAdmissionFor bytes
  | any ((== bytes) . canonicalDescriptorBytes . predefinedCatalogueDescriptor) profilePredefinedCatalogue =
      PrimordialDescriptor
  | otherwise = ApplicationDescriptor

hashTranscript :: (Serialize transcript) => transcript -> ByteString
hashTranscript = SHA256.hash . encode

profileInvariant :: (Show problem) => String -> Either problem value -> value
profileInvariant context =
  either
    (error . (("invalid closed profile " <> context <> ": ") <>) . show)
    id

field :: ByteString -> FieldName
field bytes = profileInvariant "field name" (mkFieldName (TextEncoding.decodeUtf8 bytes))

sortIdField, descriptorBytesField :: FieldName
sortIdField = field "sort_id"
descriptorBytesField = field "canonical_descriptor"
