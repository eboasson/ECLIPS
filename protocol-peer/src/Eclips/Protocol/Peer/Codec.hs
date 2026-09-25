-- | Generic current-build peer-envelope serialization with checked whole-body
-- admission.
module Eclips.Protocol.Peer.Codec
  ( PeerPayloadError (..),
    encodePeerEnvelope,
    decodePeerEnvelope,
  )
where

import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Eclips.Protocol.Peer.Types (PeerEnvelope)

-- | Coarse failure at the peer payload boundary.  Decoder implementation
-- diagnostics are deliberately not protocol surface.
data PeerPayloadError
  = PeerPayloadMalformed
  deriving stock (Eq, Show)

-- | Serialize one closed peer envelope using current-build 'Binary' instances.
encodePeerEnvelope :: PeerEnvelope -> ByteString
encodePeerEnvelope = LazyByteString.toStrict . Binary.encode

-- | Decode exactly one complete peer envelope, rejecting trailing bytes.
decodePeerEnvelope :: ByteString -> Either PeerPayloadError PeerEnvelope
decodePeerEnvelope body =
  case Binary.decodeOrFail (LazyByteString.fromStrict body) of
    Left _ -> Left PeerPayloadMalformed
    Right (residual, _, envelope)
      | not (LazyByteString.null residual) -> Left PeerPayloadMalformed
      | otherwise -> Right envelope
