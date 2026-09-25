-- | Fixed administration-family framing and pure incremental admission.
module Eclips.Protocol.Admin.Frame
  ( adminFrameMagic,
    AdminFrameDirection (..),
    AdminFrameError (..),
    AdminFrameDecoder,
    initialAdminFrameDecoder,
    AdminFrameFeedResult (..),
    AdminFrameContinuation (..),
    encodeAdminFrame,
    feedAdminFrame,
    finishAdminFrameDecoder,
  )
where

import Data.ByteString (ByteString)
import Data.Word (Word32)
import Eclips.Protocol.Admin.Codec
  ( AdminPayloadError,
    decodeAdminEnvelope,
    encodeAdminEnvelope,
  )
import Eclips.Protocol.Admin.Types (AdminEnvelope (..))
import Eclips.Protocol.Frame qualified as Frame

-- | Fixed ASCII @EADM@ administration-family discriminator.
adminFrameMagic :: Word32
adminFrameMagic = 0x4541444d

data AdminFrameDirection
  = AdminClientFrames
  | AdminServerFrames
  deriving stock (Eq, Show)

data AdminFrameError
  = AdminFrameInvalidLength
  | AdminFrameWrongFamily
  | AdminFrameWrongEnvelopeDirection
  | AdminFramePayloadError AdminPayloadError
  | AdminFrameTruncated
  deriving stock (Eq, Show)

data AdminFrameDecoder
  = AdminFrameDecoder AdminFrameDirection Frame.FrameDecoder
  deriving stock (Eq, Show)

initialAdminFrameDecoder :: AdminFrameDirection -> AdminFrameDecoder
initialAdminFrameDecoder direction =
  AdminFrameDecoder direction (Frame.initialFrameDecoder adminFrameMagic)

data AdminFrameFeedResult
  = AdminFrameFeedResult [AdminEnvelope] AdminFrameContinuation
  deriving stock (Eq, Show)

data AdminFrameContinuation
  = NeedAdminFrameBytes AdminFrameDecoder
  | AdminFrameFailed AdminFrameError
  deriving stock (Eq, Show)

encodeAdminFrame :: AdminEnvelope -> ByteString
encodeAdminFrame envelope =
  Frame.encodeFrame adminFrameMagic (encodeAdminEnvelope envelope)

feedAdminFrame :: AdminFrameDecoder -> ByteString -> AdminFrameFeedResult
feedAdminFrame (AdminFrameDecoder direction decoder) chunk =
  decodeBodies direction (Frame.feedFrame decoder chunk) []

finishAdminFrameDecoder :: AdminFrameDecoder -> Either AdminFrameError ()
finishAdminFrameDecoder (AdminFrameDecoder _ decoder) =
  case Frame.finishFrameDecoder decoder of
    Left frameError -> Left (adminFrameError frameError)
    Right () -> Right ()

decodeBodies ::
  AdminFrameDirection ->
  Frame.FrameFeedResult ->
  [AdminEnvelope] ->
  AdminFrameFeedResult
decodeBodies direction (Frame.FrameFeedResult [] continuation) reversed =
  AdminFrameFeedResult
    (reverse reversed)
    (adminFrameContinuation direction continuation)
decodeBodies direction (Frame.FrameFeedResult (body : bodies) continuation) reversed =
  case decodeAdminEnvelope body of
    Left payloadError ->
      AdminFrameFeedResult
        (reverse reversed)
        (AdminFrameFailed (AdminFramePayloadError payloadError))
    Right envelope
      | directionAccepts direction envelope ->
          decodeBodies
            direction
            (Frame.FrameFeedResult bodies continuation)
            (envelope : reversed)
      | otherwise ->
          AdminFrameFeedResult
            (reverse reversed)
            (AdminFrameFailed AdminFrameWrongEnvelopeDirection)

adminFrameContinuation ::
  AdminFrameDirection ->
  Frame.FrameContinuation ->
  AdminFrameContinuation
adminFrameContinuation direction continuation = case continuation of
  Frame.NeedFrameBytes decoder ->
    NeedAdminFrameBytes (AdminFrameDecoder direction decoder)
  Frame.FrameFailed frameError ->
    AdminFrameFailed (adminFrameError frameError)

adminFrameError :: Frame.FrameError -> AdminFrameError
adminFrameError frameError = case frameError of
  Frame.FrameInvalidLength -> AdminFrameInvalidLength
  Frame.FrameWrongFamily -> AdminFrameWrongFamily
  Frame.FrameTruncated -> AdminFrameTruncated

directionAccepts :: AdminFrameDirection -> AdminEnvelope -> Bool
directionAccepts AdminClientFrames (AdminClientEnvelope _) = True
directionAccepts AdminServerFrames (AdminServerEnvelope _) = True
directionAccepts _ _ = False
