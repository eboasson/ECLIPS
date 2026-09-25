-- | Current-build ERFT payload serialization with checked whole-body admission.
module Eclips.Protocol.Raft.Codec
  ( RaftPayloadError (..),
    encodeRaftEnvelope,
    decodeRaftEnvelope,
  )
where

import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Eclips.Protocol.Raft.Types (RaftProtocolEnvelope)

-- | Coarse failure at the ERFT payload boundary.  Decoder implementation
-- diagnostics are deliberately not protocol surface.
data RaftPayloadError
  = RaftPayloadMalformed
  deriving stock (Eq, Show)

-- | Serialize one closed protocol-owned envelope.
encodeRaftEnvelope :: RaftProtocolEnvelope -> ByteString
encodeRaftEnvelope = LazyByteString.toStrict . Binary.encode

-- | Decode exactly one complete envelope, rejecting trailing bytes and every
-- shape rejected by its checked DTO constructors.
decodeRaftEnvelope :: ByteString -> Either RaftPayloadError RaftProtocolEnvelope
decodeRaftEnvelope body =
  case Binary.decodeOrFail (LazyByteString.fromStrict body) of
    Left _ -> Left RaftPayloadMalformed
    Right (residual, _, envelope)
      | not (LazyByteString.null residual) -> Left RaftPayloadMalformed
      | otherwise -> Right envelope
