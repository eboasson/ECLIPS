-- | Fixed ERFT framing, hello-before-RPC role admission, and a pure
-- incremental decoder.
module Eclips.Protocol.Raft.Frame
  ( raftFrameMagic,
    RaftFrameError (..),
    RaftFrameDecoder,
    initialRaftFrameDecoder,
    RaftFrameFeedResult (..),
    RaftFrameContinuation (..),
    encodeRaftFrame,
    feedRaftFrame,
    finishRaftFrameDecoder,
  )
where

import Data.ByteString (ByteString)
import Data.Word (Word32)
import Eclips.Protocol.Frame qualified as Frame
import Eclips.Protocol.Raft.Codec
  ( RaftPayloadError,
    decodeRaftEnvelope,
    encodeRaftEnvelope,
  )
import Eclips.Protocol.Raft.Types
  ( RaftAdmissionError,
    RaftConnectionContext,
    RaftConnectionPhase (..),
    RaftProtocolEnvelope (..),
    admitRaftEnvelope,
  )

-- | The fixed ASCII @ERFT@ family discriminator.
raftFrameMagic :: Word32
raftFrameMagic = 0x45524654

-- | Terminal framing, payload, or physical-role admission failure.
data RaftFrameError
  = RaftFrameInvalidLength
  | RaftFrameWrongFamily
  | RaftFramePayloadError RaftPayloadError
  | RaftFrameAdmissionError RaftAdmissionError
  | RaftFrameTruncated
  deriving stock (Eq, Show)

-- | Opaque retained framing bytes plus immutable configured-voter context and
-- hello/established phase.
data RaftFrameDecoder
  = RaftFrameDecoder
      RaftConnectionContext
      RaftConnectionPhase
      Frame.FrameDecoder
  deriving stock (Eq, Show)

-- | Begin a configured voter connection.  The first admitted envelope must be
-- its matching hello; the decoder then advances permanently to RPC admission.
initialRaftFrameDecoder :: RaftConnectionContext -> RaftFrameDecoder
initialRaftFrameDecoder context =
  RaftFrameDecoder
    context
    AwaitingRaftHello
    (Frame.initialFrameDecoder raftFrameMagic)

-- | Complete, role-admitted envelopes before the first partial or failed frame.
data RaftFrameFeedResult
  = RaftFrameFeedResult [RaftProtocolEnvelope] RaftFrameContinuation
  deriving stock (Eq, Show)

-- | Retain a live decoder or stop terminally after one feed.
data RaftFrameContinuation
  = NeedRaftFrameBytes RaftFrameDecoder
  | RaftFrameFailed RaftFrameError
  deriving stock (Eq, Show)

-- | Prefix a protocol-owned envelope with the ERFT discriminator.
encodeRaftFrame :: RaftProtocolEnvelope -> ByteString
encodeRaftFrame = Frame.encodeFrame raftFrameMagic . encodeRaftEnvelope

-- | Feed any TCP-shaped byte chunk.  A coalesced matching hello and first RPC
-- are admitted in order; later hellos and pre-hello RPCs fail closed.
feedRaftFrame :: RaftFrameDecoder -> ByteString -> RaftFrameFeedResult
feedRaftFrame (RaftFrameDecoder context phase decoder) chunk =
  case Frame.feedFrame decoder chunk of
    Frame.FrameFeedResult bodies continuation ->
      decodeBodies context phase bodies [] continuation

-- | Declare end-of-input, rejecting retained partial ERFT bytes.
finishRaftFrameDecoder :: RaftFrameDecoder -> Either RaftFrameError ()
finishRaftFrameDecoder (RaftFrameDecoder _ _ decoder) =
  either (Left . commonFrameError) Right (Frame.finishFrameDecoder decoder)

decodeBodies ::
  RaftConnectionContext ->
  RaftConnectionPhase ->
  [ByteString] ->
  [RaftProtocolEnvelope] ->
  Frame.FrameContinuation ->
  RaftFrameFeedResult
decodeBodies context phase [] reversedEnvelopes continuation =
  RaftFrameFeedResult
    (reverse reversedEnvelopes)
    (translateContinuation context phase continuation)
decodeBodies context phase (body : bodies) reversedEnvelopes continuation =
  case decodeRaftEnvelope body of
    Left payloadError ->
      RaftFrameFeedResult
        (reverse reversedEnvelopes)
        (RaftFrameFailed (RaftFramePayloadError payloadError))
    Right envelope ->
      case admitRaftEnvelope phase context envelope of
        Left admissionError ->
          RaftFrameFeedResult
            (reverse reversedEnvelopes)
            (RaftFrameFailed (RaftFrameAdmissionError admissionError))
        Right admitted ->
          decodeBodies
            context
            (phaseAfter phase admitted)
            bodies
            (admitted : reversedEnvelopes)
            continuation

phaseAfter :: RaftConnectionPhase -> RaftProtocolEnvelope -> RaftConnectionPhase
phaseAfter AwaitingRaftHello (RaftHello _) = EstablishedRaftBinding
phaseAfter phase _ = phase

translateContinuation ::
  RaftConnectionContext ->
  RaftConnectionPhase ->
  Frame.FrameContinuation ->
  RaftFrameContinuation
translateContinuation context phase (Frame.NeedFrameBytes decoder) =
  NeedRaftFrameBytes (RaftFrameDecoder context phase decoder)
translateContinuation _ _ (Frame.FrameFailed frameError) =
  RaftFrameFailed (commonFrameError frameError)

commonFrameError :: Frame.FrameError -> RaftFrameError
commonFrameError Frame.FrameInvalidLength = RaftFrameInvalidLength
commonFrameError Frame.FrameWrongFamily = RaftFrameWrongFamily
commonFrameError Frame.FrameTruncated = RaftFrameTruncated
