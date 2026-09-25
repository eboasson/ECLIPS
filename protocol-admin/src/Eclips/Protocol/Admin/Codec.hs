-- | Current-build administration serialization with complete-payload
-- admission.
module Eclips.Protocol.Admin.Codec
  ( AdminPayloadError (..),
    encodeAdminEnvelope,
    decodeAdminEnvelope,
  )
where

import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Eclips.Protocol.Admin.Types (AdminEnvelope)

data AdminPayloadError
  = AdminPayloadMalformed
  deriving stock (Eq, Show)

encodeAdminEnvelope :: AdminEnvelope -> ByteString
encodeAdminEnvelope = LazyByteString.toStrict . Binary.encode

decodeAdminEnvelope :: ByteString -> Either AdminPayloadError AdminEnvelope
decodeAdminEnvelope body =
  case Binary.decodeOrFail (LazyByteString.fromStrict body) of
    Left _ -> Left AdminPayloadMalformed
    Right (residual, _, envelope)
      | LazyByteString.null residual -> Right envelope
      | otherwise -> Left AdminPayloadMalformed
