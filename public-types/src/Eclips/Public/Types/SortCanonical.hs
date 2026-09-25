{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Shared descriptor encoding and content addresses, without application or
-- semantic-domain identities. The raw grammar contains schema tags for unique
-- IDs and labels, but it cannot contain an actual value of either type.
--
-- This module preserves the sole current-build descriptor representation. Its
-- inputs and decoder results are untrusted syntax: encoding or hashing a value
-- does not prove descriptor admission. The semantic domain and typed application
-- adapters retain responsibility for their respective admission boundaries.
module Eclips.Public.Types.SortCanonical
  ( RawDescriptor (..),
    RawSortKind (..),
    RawSchema (..),
    RawProjection (..),
    RawPredicate (..),
    RawScalarComparison (..),
    RawScalarLiteral (..),
    RawRankDirection (..),
    RawRankTerm (..),
    RawApplicationMutation (..),
    RawStructuralCarrierRole (..),
    descriptorDomainTag,
    encodeRawDescriptor,
    decodeRawDescriptor,
    rawDescriptorSortId,
    canonicalizeRawDescriptor,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Serialize (Serialize)
import Data.Serialize qualified as Serialize
import Data.Word (Word64)
import Eclips.Public.Types.SortId (SortId, mkSortId)
import GHC.Generics (Generic)

descriptorDomainTag :: Word64
descriptorDomainTag = 0x45434c4950532d44 -- ASCII "ECLIPS-D", not a version.

-- Constructor and field order define the current canonical byte representation.
-- Keep application and semantic adapters on this one encoder.
data RawDescriptor = RawDescriptor
  { domain :: Word64,
    kind :: RawSortKind,
    schema :: RawSchema,
    keyProjections :: [RawProjection],
    validity :: RawPredicate,
    obsolescence :: RawPredicate,
    rankTerms :: [RawRankTerm],
    minimumRetentionMicros :: Word64,
    immutable :: Bool,
    labelField :: Maybe ByteString,
    applicationMutation :: RawApplicationMutation,
    structuralCarrierRole :: Maybe RawStructuralCarrierRole
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

data RawSortKind
  = RawRegularSort
  | RawControlledSort
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

data RawSchema
  = RawBoolSchema
  | RawInt64Schema
  | RawBytesSchema
  | RawTextSchema
  | RawGlobalUniqueIdSchema
  | RawLabelSchema
  | RawRecordSchema [(ByteString, RawSchema)]
  | RawEnumSchema [ByteString]
  | RawOptionalGlobalUniqueIdSchema
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

newtype RawProjection = RawProjection [ByteString]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

data RawPredicate
  = RawAlwaysPredicate
  | RawNeverPredicate
  | RawCompareField RawProjection RawScalarComparison RawScalarLiteral
  | RawNotPredicate RawPredicate
  | RawAllPredicates [RawPredicate]
  | RawAnyPredicates [RawPredicate]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

data RawScalarComparison
  = RawScalarEqual
  | RawScalarNotEqual
  | RawScalarLessThan
  | RawScalarLessThanOrEqual
  | RawScalarGreaterThan
  | RawScalarGreaterThanOrEqual
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

data RawScalarLiteral
  = RawLiteralBool Bool
  | RawLiteralInt64 Int64
  | RawLiteralBytes ByteString
  | RawLiteralText ByteString
  | RawLiteralEnum ByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

data RawRankDirection
  = RawAscending
  | RawDescending
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

data RawRankTerm
  = RawRankField RawProjection RawRankDirection
  | RawRankApplicationValue RawRankDirection
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

data RawApplicationMutation
  = RawOrdinaryApplicationMutation
  | RawHeraldManagedMutation
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

data RawStructuralCarrierRole
  = RawNeutralVertexCarrier
  | RawEdgeCarrier
  | RawNablaCarrier
  | RawDeltaCarrier
  | RawProcessEpochCarrier
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Encode raw syntax without admitting its semantic meaning.
encodeRawDescriptor :: RawDescriptor -> ByteString
encodeRawDescriptor = Serialize.encode

-- | Decode raw syntax. Callers must still check the domain tag, semantic shape,
-- and byte-for-byte canonical re-encoding before admitting the descriptor.
decodeRawDescriptor :: ByteString -> Either String RawDescriptor
decodeRawDescriptor = Serialize.decode

-- | Compute the public content address of the exact raw descriptor encoding.
rawDescriptorSortId :: RawDescriptor -> SortId
rawDescriptorSortId = snd . canonicalizeRawDescriptor

-- | Share one encoding when both canonical bytes and their identity are needed.
-- This does not admit the descriptor or establish that its sort exists.
canonicalizeRawDescriptor :: RawDescriptor -> (ByteString, SortId)
canonicalizeRawDescriptor raw =
  let bytes = encodeRawDescriptor raw
      identity = case mkSortId (SHA256.hash bytes) of
        Right value -> value
        Left problem ->
          error
            ( "SHA-256 violated its fixed 32-byte result invariant: "
                <> show problem
            )
   in (bytes, identity)
