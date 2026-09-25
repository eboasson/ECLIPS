-- | Family-neutral framing and its pure incremental decoder.
module Eclips.Protocol.Frame
  ( FrameError (..),
    FrameDecoder,
    initialFrameDecoder,
    FrameFeedResult (..),
    FrameContinuation (..),
    encodeFrame,
    feedFrame,
    finishFrameDecoder,
  )
where

import Data.Binary.Get (getWord32be, runGet)
import Data.Binary.Put (putByteString, putWord32be, runPut)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Word (Word32)

-- | Terminal failures owned by the common envelope rather than a family body.
data FrameError
  = FrameInvalidLength
  | FrameWrongFamily
  | FrameTruncated
  deriving stock (Eq, Show)

-- | Opaque expected family and retained bytes for one stream.
data FrameDecoder
  = FrameDecoder Word32 ByteString
  deriving stock (Eq, Show)

-- | Begin decoding frames for one expected family magic.
initialFrameDecoder :: Word32 -> FrameDecoder
initialFrameDecoder familyMagic = FrameDecoder familyMagic ByteString.empty

-- | Complete opaque bodies admitted before the first incomplete or failed frame.
data FrameFeedResult
  = FrameFeedResult
      [ByteString]
      FrameContinuation
  deriving stock (Eq, Show)

-- | The sole continuation after a feed: retain a decoder or stop terminally.
data FrameContinuation
  = NeedFrameBytes FrameDecoder
  | FrameFailed FrameError
  deriving stock (Eq, Show)

-- | Prefix an opaque family body with its family magic and declared length.
--
-- Profile 0.1 assumes friendly finite bodies fit the unsigned frame-length
-- representation, so there is no configurable size rejection outcome.
encodeFrame :: Word32 -> ByteString -> ByteString
encodeFrame familyMagic body =
  LazyByteString.toStrict
    ( runPut $ do
        putWord32be (fromIntegral (ByteString.length body + familyMagicByteCount))
        putWord32be familyMagic
        putByteString body
    )

-- | Feed any TCP-shaped byte chunk while preserving every valid prefix body.
feedFrame :: FrameDecoder -> ByteString -> FrameFeedResult
feedFrame (FrameDecoder familyMagic retained) chunk =
  parseFrames familyMagic (retained <> chunk) []

-- | Declare end-of-input, rejecting any retained partial frame.
finishFrameDecoder :: FrameDecoder -> Either FrameError ()
finishFrameDecoder (FrameDecoder _ retained)
  | ByteString.null retained = Right ()
  | otherwise = Left FrameTruncated

frameLengthByteCount :: Int
frameLengthByteCount = 4

familyMagicByteCount :: Int
familyMagicByteCount = 4

parseFrames :: Word32 -> ByteString -> [ByteString] -> FrameFeedResult
parseFrames familyMagic bytes reversedBodies
  | ByteString.length bytes < frameLengthByteCount = needMore
  | declaredLength < familyMagicByteCount = failed FrameInvalidLength
  | ByteString.length frameAndFollowing < familyMagicByteCount = needMore
  | decodedMagic /= familyMagic = failed FrameWrongFamily
  | ByteString.length frameAndFollowing < declaredLength = needMore
  | otherwise = parseFrames familyMagic following (body : reversedBodies)
  where
    declaredLength = fromIntegral (word32Prefix bytes)
    frameAndFollowing = ByteString.drop frameLengthByteCount bytes
    (declaredFrame, following) = ByteString.splitAt declaredLength frameAndFollowing
    decodedMagic = word32Prefix declaredFrame
    body = ByteString.drop familyMagicByteCount declaredFrame
    needMore =
      FrameFeedResult
        (reverse reversedBodies)
        (NeedFrameBytes (FrameDecoder familyMagic bytes))
    failed frameError =
      FrameFeedResult
        (reverse reversedBodies)
        (FrameFailed frameError)

word32Prefix :: ByteString -> Word32
word32Prefix = runGet getWord32be . LazyByteString.fromStrict . ByteString.take 4
