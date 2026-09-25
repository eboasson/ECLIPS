-- | Package-private canonical Oracle-state digest seam.
--
-- A state owner supplies its complete canonical transcript bytes and the way to
-- install the resulting digest. Keeping both operations parameterized lets
-- state owners reuse the digest discipline without coupling their state types
-- to this helper.
module Eclips.Oracle.Internal.CanonicalState
  ( deriveCanonicalStateDigest,
    refreshCanonicalStateDigest,
  )
where

import Data.ByteString (ByteString)
import Eclips.Oracle.Identity (OracleStateDigest)
import Eclips.Oracle.Internal.Digest (checkedStateDigestBytes)

-- | Hash already-canonical transcript bytes exactly as supplied.
deriveCanonicalStateDigest :: (state -> ByteString) -> state -> OracleStateDigest
deriveCanonicalStateDigest canonicalTranscriptBytes =
  checkedStateDigestBytes . canonicalTranscriptBytes

-- | Recompute and install a state's digest from its canonical transcript bytes.
refreshCanonicalStateDigest ::
  (state -> ByteString) ->
  (OracleStateDigest -> state -> state) ->
  state ->
  state
refreshCanonicalStateDigest canonicalTranscriptBytes installDigest state =
  installDigest (deriveCanonicalStateDigest canonicalTranscriptBytes state) state
