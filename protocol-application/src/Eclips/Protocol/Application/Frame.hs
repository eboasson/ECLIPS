-- | Fixed application-family framing and its pure incremental decoder.
module Eclips.Protocol.Application.Frame
  ( applicationFrameMagic,
    ApplicationFrameDirection (..),
    ApplicationFrameError (..),
    ApplicationFrameDecoder,
    initialApplicationFrameDecoder,
    ApplicationFrameFeedResult (..),
    ApplicationFrameContinuation (..),
    encodeApplicationFrame,
    feedApplicationFrame,
    finishApplicationFrameDecoder,
  )
where

import Data.ByteString (ByteString)
import Data.Word (Word32)
import Eclips.Protocol.Application.Codec
  ( ApplicationPayloadError,
    decodeApplicationEnvelope,
    encodeApplicationEnvelope,
  )
import Eclips.Protocol.Application.Types (ApplicationEnvelope (..))
import Eclips.Protocol.Frame qualified as Frame

-- | The fixed ASCII @EAPP@ application-family discriminator.
applicationFrameMagic :: Word32
applicationFrameMagic = 0x45415050

-- | The envelope arm admitted by one endpoint decoder.
data ApplicationFrameDirection
  = ApplicationClientFrames
  | ApplicationServerFrames
  deriving stock (Eq, Show)

-- | Terminal framing and payload admission failures.
data ApplicationFrameError
  = ApplicationFrameInvalidLength
  | ApplicationFrameWrongFamily
  | ApplicationFrameWrongEnvelopeDirection
  | ApplicationFramePayloadError ApplicationPayloadError
  | ApplicationFrameTruncated
  deriving stock (Eq, Show)

-- | Opaque retained bytes and endpoint direction for one stream.
data ApplicationFrameDecoder
  = ApplicationFrameDecoder ApplicationFrameDirection Frame.FrameDecoder
  deriving stock (Eq, Show)

-- | Begin decoding frames for one endpoint direction.
initialApplicationFrameDecoder :: ApplicationFrameDirection -> ApplicationFrameDecoder
initialApplicationFrameDecoder direction =
  ApplicationFrameDecoder direction (Frame.initialFrameDecoder applicationFrameMagic)

-- | Complete envelopes admitted before the first incomplete or failed frame.
data ApplicationFrameFeedResult
  = ApplicationFrameFeedResult
      [ApplicationEnvelope]
      ApplicationFrameContinuation
  deriving stock (Eq, Show)

-- | The sole continuation after a feed: retain a decoder or stop terminally.
data ApplicationFrameContinuation
  = NeedApplicationFrameBytes ApplicationFrameDecoder
  | ApplicationFrameFailed ApplicationFrameError
  deriving stock (Eq, Show)

-- | Prefix one envelope with the fixed family header.
--
-- Profile 0.1 assumes friendly finite payloads fit the unsigned frame-length
-- representation, so there is no configurable size rejection outcome.
encodeApplicationFrame :: ApplicationEnvelope -> ByteString
encodeApplicationFrame envelope =
  Frame.encodeFrame applicationFrameMagic (encodeApplicationEnvelope envelope)

-- | Feed any TCP-shaped byte chunk while preserving every valid prefix frame.
feedApplicationFrame :: ApplicationFrameDecoder -> ByteString -> ApplicationFrameFeedResult
feedApplicationFrame (ApplicationFrameDecoder direction decoder) chunk =
  decodeBodies direction (Frame.feedFrame decoder chunk) []

-- | Declare end-of-input, rejecting any retained partial frame.
finishApplicationFrameDecoder :: ApplicationFrameDecoder -> Either ApplicationFrameError ()
finishApplicationFrameDecoder (ApplicationFrameDecoder _ decoder) =
  case Frame.finishFrameDecoder decoder of
    Left frameError -> Left (applicationFrameError frameError)
    Right () -> Right ()

decodeBodies :: ApplicationFrameDirection -> Frame.FrameFeedResult -> [ApplicationEnvelope] -> ApplicationFrameFeedResult
decodeBodies direction (Frame.FrameFeedResult [] continuation) reversedEnvelopes =
  ApplicationFrameFeedResult
    (reverse reversedEnvelopes)
    (applicationFrameContinuation direction continuation)
decodeBodies direction (Frame.FrameFeedResult (body : bodies) continuation) reversedEnvelopes =
  case decodeApplicationEnvelope body of
    Left payloadError ->
      ApplicationFrameFeedResult
        (reverse reversedEnvelopes)
        (ApplicationFrameFailed (ApplicationFramePayloadError payloadError))
    Right envelope
      | directionAccepts direction envelope ->
          decodeBodies
            direction
            (Frame.FrameFeedResult bodies continuation)
            (envelope : reversedEnvelopes)
      | otherwise ->
          ApplicationFrameFeedResult
            (reverse reversedEnvelopes)
            (ApplicationFrameFailed ApplicationFrameWrongEnvelopeDirection)

applicationFrameContinuation :: ApplicationFrameDirection -> Frame.FrameContinuation -> ApplicationFrameContinuation
applicationFrameContinuation direction (Frame.NeedFrameBytes decoder) =
  NeedApplicationFrameBytes (ApplicationFrameDecoder direction decoder)
applicationFrameContinuation _ (Frame.FrameFailed frameError) =
  ApplicationFrameFailed (applicationFrameError frameError)

applicationFrameError :: Frame.FrameError -> ApplicationFrameError
applicationFrameError Frame.FrameInvalidLength = ApplicationFrameInvalidLength
applicationFrameError Frame.FrameWrongFamily = ApplicationFrameWrongFamily
applicationFrameError Frame.FrameTruncated = ApplicationFrameTruncated

directionAccepts :: ApplicationFrameDirection -> ApplicationEnvelope -> Bool
directionAccepts ApplicationClientFrames (ApplicationClientEnvelope _) = True
directionAccepts ApplicationServerFrames (ApplicationServerEnvelope _) = True
directionAccepts _ _ = False
