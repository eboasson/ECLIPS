-- | Generic current-build application-envelope serialization with checked
-- whole-payload admission.
module Eclips.Protocol.Application.Codec
  ( ApplicationPayloadError (..),
    encodeApplicationEnvelope,
    decodeApplicationEnvelope,
  )
where

import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Eclips.Protocol.Application.Types (ApplicationEnvelope)

-- | Coarse failure categories at the application payload boundary.
data ApplicationPayloadError
  = ApplicationPayloadMalformed
  deriving stock (Eq, Show)

-- | Serialize one closed envelope using the current build's 'Binary.Binary' instances.
encodeApplicationEnvelope :: ApplicationEnvelope -> ByteString
encodeApplicationEnvelope = LazyByteString.toStrict . Binary.encode

-- | Decode exactly one complete envelope.
--
-- Decoder diagnostics are deliberately not exposed as protocol surface.
decodeApplicationEnvelope :: ByteString -> Either ApplicationPayloadError ApplicationEnvelope
decodeApplicationEnvelope body =
  case Binary.decodeOrFail (LazyByteString.fromStrict body) of
    Left _ -> Left ApplicationPayloadMalformed
    Right (residual, _, envelope)
      | not (LazyByteString.null residual) -> Left ApplicationPayloadMalformed
      | otherwise -> Right envelope
