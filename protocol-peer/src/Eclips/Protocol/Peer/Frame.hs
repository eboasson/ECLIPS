-- | Fixed EPRP peer-family framing and its pure incremental decoder.
module Eclips.Protocol.Peer.Frame
  ( peerFrameMagic,
    PeerFrameError (..),
    PeerFrameDecoder,
    initialPeerFrameDecoder,
    PeerFrameFeedResult (..),
    PeerFrameContinuation (..),
    encodePeerFrame,
    feedPeerFrame,
    finishPeerFrameDecoder,
  )
where

import Data.ByteString (ByteString)
import Data.Word (Word32)
import Eclips.Protocol.Frame
  ( FrameContinuation (..),
    FrameDecoder,
    FrameError (..),
    FrameFeedResult (..),
    encodeFrame,
    feedFrame,
    finishFrameDecoder,
    initialFrameDecoder,
  )
import Eclips.Protocol.Peer.Codec
  ( PeerPayloadError,
    decodePeerEnvelope,
    encodePeerEnvelope,
  )
import Eclips.Protocol.Peer.Types (PeerEnvelope)

-- | The fixed ASCII @EPRP@ peer-family discriminator.
peerFrameMagic :: Word32
peerFrameMagic = 0x45505250

-- | Terminal peer framing and payload-admission failures.
data PeerFrameError
  = PeerFrameInvalidLength
  | PeerFrameWrongFamily
  | PeerFramePayloadError PeerPayloadError
  | PeerFrameTruncated
  deriving stock (Eq, Show)

-- | Opaque common-frame continuation for one EPRP stream.
newtype PeerFrameDecoder = PeerFrameDecoder FrameDecoder
  deriving stock (Eq, Show)

-- | Begin decoding a symmetric Herald-peer stream.
initialPeerFrameDecoder :: PeerFrameDecoder
initialPeerFrameDecoder = PeerFrameDecoder (initialFrameDecoder peerFrameMagic)

-- | Complete peer envelopes admitted before the first partial or failed frame.
data PeerFrameFeedResult
  = PeerFrameFeedResult [PeerEnvelope] PeerFrameContinuation
  deriving stock (Eq, Show)

-- | The sole continuation after a peer-frame feed.
data PeerFrameContinuation
  = NeedPeerFrameBytes PeerFrameDecoder
  | PeerFrameFailed PeerFrameError
  deriving stock (Eq, Show)

-- | Encode one peer envelope with the EPRP discriminator.
encodePeerFrame :: PeerEnvelope -> ByteString
encodePeerFrame = encodeFrame peerFrameMagic . encodePeerEnvelope

-- | Feed any TCP-shaped chunk, preserving every valid envelope before a
-- partial frame or the first terminal framing/payload failure.
feedPeerFrame :: PeerFrameDecoder -> ByteString -> PeerFrameFeedResult
feedPeerFrame (PeerFrameDecoder decoder) chunk =
  case feedFrame decoder chunk of
    FrameFeedResult bodies continuation ->
      decodeBodies bodies [] continuation

-- | Declare end-of-input, rejecting a retained partial EPRP frame.
finishPeerFrameDecoder :: PeerFrameDecoder -> Either PeerFrameError ()
finishPeerFrameDecoder (PeerFrameDecoder decoder) =
  either (Left . commonFrameError) Right (finishFrameDecoder decoder)

decodeBodies ::
  [ByteString] ->
  [PeerEnvelope] ->
  FrameContinuation ->
  PeerFrameFeedResult
decodeBodies [] reversedEnvelopes continuation =
  PeerFrameFeedResult
    (reverse reversedEnvelopes)
    (translateContinuation continuation)
decodeBodies (body : remaining) reversedEnvelopes continuation =
  case decodePeerEnvelope body of
    Left payloadError ->
      PeerFrameFeedResult
        (reverse reversedEnvelopes)
        (PeerFrameFailed (PeerFramePayloadError payloadError))
    Right envelope -> decodeBodies remaining (envelope : reversedEnvelopes) continuation

translateContinuation :: FrameContinuation -> PeerFrameContinuation
translateContinuation (NeedFrameBytes decoder) =
  NeedPeerFrameBytes (PeerFrameDecoder decoder)
translateContinuation (FrameFailed frameError) =
  PeerFrameFailed (commonFrameError frameError)

commonFrameError :: FrameError -> PeerFrameError
commonFrameError FrameInvalidLength = PeerFrameInvalidLength
commonFrameError FrameWrongFamily = PeerFrameWrongFamily
commonFrameError FrameTruncated = PeerFrameTruncated
