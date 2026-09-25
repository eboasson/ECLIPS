{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Low-level canonical identity of one non-empty Herald-epoch set.
--
-- This module sits below both membership generations and topology cuts so the
-- latter can become membership-qualified without creating an import cycle.
module Eclips.Domain.MemberSet
  ( TopologyDigestError (..),
    MemberSetDigest,
    mkMemberSetDigest,
    memberSetDigestBytes,
    deriveMemberSetDigest,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Serialize (Serialize)
import Data.Serialize qualified as Serialize
import Data.Set qualified as Set
import Eclips.Domain.Identity (HeraldEpoch, heraldEpochBytes)
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import GHC.Generics (Generic)

sha256ByteCount :: Int
sha256ByteCount = 32

data TopologyDigestError = TopologyDigestWrongByteCount
  { expectedTopologyDigestByteCount :: Int,
    actualTopologyDigestByteCount :: Int
  }
  deriving stock (Eq, Show)

newtype MemberSetDigest = MemberSetDigest ByteString
  deriving stock (Eq, Ord)

instance Show MemberSetDigest where
  show = renderGroupedHex . memberSetDigestBytes

mkMemberSetDigest :: ByteString -> Either TopologyDigestError MemberSetDigest
mkMemberSetDigest = fmap MemberSetDigest . checkDigestBytes

memberSetDigestBytes :: MemberSetDigest -> ByteString
memberSetDigestBytes (MemberSetDigest bytes) = bytes

-- | Digest the exact Herald-epoch set in ascending order.
deriveMemberSetDigest :: NonEmpty HeraldEpoch -> MemberSetDigest
deriveMemberSetDigest members =
  either (error . ("member-set digest invariant: " <>) . show) id
    . mkMemberSetDigest
    . SHA256.hash
    . Serialize.encode
    $ MemberSetDigestTranscript
      "ECLIPS-STRUCTURAL-MEMBER-SET"
      (fmap heraldEpochBytes (Set.toAscList (Set.fromList (NonEmpty.toList members))))

checkDigestBytes :: ByteString -> Either TopologyDigestError ByteString
checkDigestBytes bytes
  | ByteString.length bytes == sha256ByteCount = Right bytes
  | otherwise =
      Left
        TopologyDigestWrongByteCount
          { expectedTopologyDigestByteCount = sha256ByteCount,
            actualTopologyDigestByteCount = ByteString.length bytes
          }

data MemberSetDigestTranscript
  = MemberSetDigestTranscript ByteString [ByteString]
  deriving stock (Generic)
  deriving anyclass (Serialize)
