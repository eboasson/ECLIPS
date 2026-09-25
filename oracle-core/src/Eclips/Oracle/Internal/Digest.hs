{-# LANGUAGE DeriveAnyClass #-}

-- | Package-private canonical hashing helpers.
module Eclips.Oracle.Internal.Digest
  ( sha256Transcript,
    checkedCommandDigest,
    checkedStateDigest,
    checkedStateDigestBytes,
    checkedRaftConfigurationDigest,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.Serialize (Serialize, encode)
import Eclips.Oracle.Identity
  ( OracleCommandDigest,
    OracleStateDigest,
    RaftConfigurationDigest,
    mkOracleCommandDigest,
    mkOracleStateDigest,
    mkRaftConfigurationDigest,
  )

sha256Transcript :: (Serialize transcript) => transcript -> ByteString
sha256Transcript = SHA256.hash . encode

checkedCommandDigest :: (Serialize transcript) => transcript -> OracleCommandDigest
checkedCommandDigest = checked "Oracle command digest" mkOracleCommandDigest . sha256Transcript

checkedStateDigest :: (Serialize transcript) => transcript -> OracleStateDigest
checkedStateDigest = checked "Oracle state digest" mkOracleStateDigest . sha256Transcript

checkedStateDigestBytes :: ByteString -> OracleStateDigest
checkedStateDigestBytes = checked "Oracle state digest" mkOracleStateDigest . SHA256.hash

checkedRaftConfigurationDigest :: (Serialize transcript) => transcript -> RaftConfigurationDigest
checkedRaftConfigurationDigest = checked "Raft configuration digest" mkRaftConfigurationDigest . sha256Transcript

checked :: (Show problem) => String -> (value -> Either problem result) -> value -> result
checked label admit value = case admit value of
  Right result -> result
  Left problem -> error (label <> " invariant: " <> show problem)
