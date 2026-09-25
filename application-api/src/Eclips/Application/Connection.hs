-- | Portable opaque connection material for arguments and descriptor files.
module Eclips.Application.Connection
  ( ConnectionDescriptor,
    ConnectionDescriptorError (..),
    encodeConnectionDescriptor,
    decodeConnectionDescriptor,
  )
where

import Data.Binary (decodeOrFail, encode)
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as Lazy
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)
import Eclips.Application.Types.Lifecycle (ConnectionDescriptor)

-- | Malformed text or a body that fails the descriptor's checked decoder.
data ConnectionDescriptorError
  = ConnectionDescriptorInvalidHex
  | ConnectionDescriptorInvalidBody
  deriving stock (Eq, Show)

-- | Canonical lowercase hex of this build's checked descriptor representation.
-- No API version, alternative schema, or global application handle is added.
encodeConnectionDescriptor :: ConnectionDescriptor -> Text
encodeConnectionDescriptor = Text.pack . concatMap hexByte . Lazy.unpack . encode
  where
    hexByte byte = [digit (byte `div` 16), digit (byte `mod` 16)]
    digit nibble = toEnum (fromEnum (if nibble < 10 then '0' else 'a') + fromIntegral (if nibble < 10 then nibble else nibble - 10))

-- | Accept canonical descriptor text, with surrounding file whitespace ignored.
-- The binary body must be complete, canonical, and pass every shape check.
decodeConnectionDescriptor :: Text -> Either ConnectionDescriptorError ConnectionDescriptor
decodeConnectionDescriptor supplied = do
  bytes <- Bytes.pack <$> parseHex (Text.unpack text)
  case decodeOrFail (Lazy.fromStrict bytes) of
    Left _ -> Left ConnectionDescriptorInvalidBody
    Right (remaining, _, descriptor)
      | Lazy.null remaining && encodeConnectionDescriptor descriptor == text -> Right descriptor
      | otherwise -> Left ConnectionDescriptorInvalidBody
  where
    text = Text.strip supplied
    parseHex [] = Right []
    parseHex (high : low : rest) = do
      upper <- nibble high
      lower <- nibble low
      ((upper * 16 + lower) :) <$> parseHex rest
    parseHex [_] = Left ConnectionDescriptorInvalidHex
    nibble :: Char -> Either ConnectionDescriptorError Word8
    nibble character
      | character >= '0' && character <= '9' = Right (fromIntegral (ord character - ord '0'))
      | character >= 'a' && character <= 'f' = Right (fromIntegral (ord character - ord 'a' + 10))
      | otherwise = Left ConnectionDescriptorInvalidHex
