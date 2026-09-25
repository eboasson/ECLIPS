-- | Canonical transport payload for one coordinated Oracle/Raft checkpoint.
-- The Oracle owner encodes semantic state; the watch owner supplies the control
-- history still required by its consumers. Neither owner exposes mutable state.
module Eclips.Oracle.Runtime.Internal.Checkpoint
  ( encodeRuntimeCheckpoint,
    decodeRuntimeCheckpoint,
    encodeLocalRuntimeCheckpoint,
    localRuntimeCheckpoint,
  )
where

import Control.Monad (unless)
import Data.Binary (decodeOrFail, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity (ControlIndex, controlIndex, controlIndexWord64)
import Eclips.Oracle.Canonical (canonicalAppliedOracleEntryBytes, canonicalAppliedOracleEntryValue, canonicalizeAppliedOracleEntry, decodeCanonicalAppliedOracleEntry)
import Eclips.Oracle.Projection (AppliedOracleEntry, appliedEntryControlIndex)

encodeRuntimeCheckpoint :: ByteString -> ControlIndex -> [AppliedOracleEntry] -> ByteString
encodeRuntimeCheckpoint oracle through entries = Lazy.toStrict (encode (1 :: Word8, oracle, controlIndexWord64 through, map (canonicalAppliedOracleEntryBytes . canonicalizeAppliedOracleEntry) entries))

decodeRuntimeCheckpoint :: ByteString -> Either String (ByteString, ControlIndex, [AppliedOracleEntry])
decodeRuntimeCheckpoint bytes = do
  (kind, oracle, throughWord, encoded) <- case decodeOrFail (Lazy.fromStrict bytes) of
    Left (_, _, problem) -> Left problem
    Right (trailing, _, value)
      | Lazy.null trailing -> Right value
      | otherwise -> Left "trailing runtime checkpoint bytes"
  unless (kind == (1 :: Word8)) (Left "local checkpoint recipe is not a remote application snapshot")
  entries <- traverse (fmap canonicalAppliedOracleEntryValue . either (Left . show) Right . decodeCanonicalAppliedOracleEntry) encoded
  let through = controlIndex throughWord
  unless
    (map (controlIndexWord64 . appliedEntryControlIndex) entries == [1 .. throughWord])
    (Left "checkpoint watch history is not contiguous")
  unless
    (encodeRuntimeCheckpoint oracle through entries == bytes)
    (Left "noncanonical runtime checkpoint")
  pure (oracle, through, entries)

-- A local checkpoint owns only the current Oracle state. Historical watch data
-- is already owned by the ledger; materialize it only for an actual transfer.
encodeLocalRuntimeCheckpoint :: ByteString -> ControlIndex -> ByteString
encodeLocalRuntimeCheckpoint oracle through = Lazy.toStrict (encode (0 :: Word8, oracle, controlIndexWord64 through, [] :: [ByteString]))

localRuntimeCheckpoint :: ByteString -> Either String (Maybe (ByteString, ControlIndex))
localRuntimeCheckpoint bytes = case decodeOrFail (Lazy.fromStrict bytes) of
  Left (_, _, problem) -> Left problem
  Right (trailing, _, (kind :: Word8, oracle :: ByteString, through :: Word64, entries :: [ByteString]))
    | not (Lazy.null trailing) -> Left "trailing checkpoint recipe bytes"
    | kind == 1 -> pure Nothing
    | kind == 0 && null entries && encodeLocalRuntimeCheckpoint oracle (controlIndex through) == bytes -> pure (Just (oracle, controlIndex through))
    | otherwise -> Left "invalid local checkpoint recipe"
