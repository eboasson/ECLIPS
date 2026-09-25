-- | Nominal Oracle request and digest identities.
module Eclips.Oracle.Identity
  ( OracleClientRequestId,
    oracleClientRequestId,
    oracleClientRequestHome,
    oracleClientRequestSequence,
    OracleCommandDigest,
    mkOracleCommandDigest,
    oracleCommandDigestBytes,
    OracleStateDigest,
    mkOracleStateDigest,
    oracleStateDigestBytes,
    RaftConfigurationDigest,
    mkRaftConfigurationDigest,
    raftConfigurationDigestBytes,
    OracleDigestError (..),
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word (Word64)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Public.Types.Diagnostic (renderGroupedHex)

-- | A request sequence is scoped by the submitting Herald epoch.
data OracleClientRequestId = OracleClientRequestId HeraldEpoch Word64
  deriving stock (Eq, Ord, Show)

oracleClientRequestId :: HeraldEpoch -> Word64 -> OracleClientRequestId
oracleClientRequestId = OracleClientRequestId

oracleClientRequestHome :: OracleClientRequestId -> HeraldEpoch
oracleClientRequestHome (OracleClientRequestId home _) = home

oracleClientRequestSequence :: OracleClientRequestId -> Word64
oracleClientRequestSequence (OracleClientRequestId _ sequenceNumber) = sequenceNumber

newtype OracleCommandDigest = OracleCommandDigest ByteString
  deriving stock (Eq, Ord)

newtype OracleStateDigest = OracleStateDigest ByteString
  deriving stock (Eq, Ord)

newtype RaftConfigurationDigest = RaftConfigurationDigest ByteString
  deriving stock (Eq, Ord)

instance Show OracleCommandDigest where
  show = renderGroupedHex . oracleCommandDigestBytes

instance Show OracleStateDigest where
  show = renderGroupedHex . oracleStateDigestBytes

instance Show RaftConfigurationDigest where
  show = renderGroupedHex . raftConfigurationDigestBytes

data OracleDigestError = WrongOracleDigestByteCount Int
  deriving stock (Eq, Show)

mkOracleCommandDigest :: ByteString -> Either OracleDigestError OracleCommandDigest
mkOracleCommandDigest = fmap OracleCommandDigest . checkDigest

oracleCommandDigestBytes :: OracleCommandDigest -> ByteString
oracleCommandDigestBytes (OracleCommandDigest bytes) = bytes

mkOracleStateDigest :: ByteString -> Either OracleDigestError OracleStateDigest
mkOracleStateDigest = fmap OracleStateDigest . checkDigest

oracleStateDigestBytes :: OracleStateDigest -> ByteString
oracleStateDigestBytes (OracleStateDigest bytes) = bytes

mkRaftConfigurationDigest :: ByteString -> Either OracleDigestError RaftConfigurationDigest
mkRaftConfigurationDigest = fmap RaftConfigurationDigest . checkDigest

raftConfigurationDigestBytes :: RaftConfigurationDigest -> ByteString
raftConfigurationDigestBytes (RaftConfigurationDigest bytes) = bytes

checkDigest :: ByteString -> Either OracleDigestError ByteString
checkDigest bytes
  | ByteString.length bytes == sha256ByteCount = Right bytes
  | otherwise = Left (WrongOracleDigestByteCount (ByteString.length bytes))

sha256ByteCount :: Int
sha256ByteCount = 32
