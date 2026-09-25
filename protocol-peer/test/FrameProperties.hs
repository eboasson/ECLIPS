module FrameProperties
  ( tests,
  )
where

import Data.Binary.Get (getWord32be, runGet)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Word (Word32, Word8)
import Eclips.Protocol.Peer.Codec (PeerPayloadError (..))
import Eclips.Protocol.Peer.Frame
  ( PeerFrameContinuation (..),
    PeerFrameDecoder,
    PeerFrameError (..),
    PeerFrameFeedResult (..),
    encodePeerFrame,
    feedPeerFrame,
    finishPeerFrameDecoder,
    initialPeerFrameDecoder,
    peerFrameMagic,
  )
import Eclips.Protocol.Peer.Types
  ( PeerEnvelope (..),
    PeerHeartbeatDto (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    testProperty,
    (===),
  )
import TestFixtures (allEnvelopes, heartbeatNonce)

tests :: TestTree
tests =
  testGroup
    "EPRP frame"
    [ testCase "header has fixed EPRP magic and excludes its own length" caseHeader,
      testCase "heartbeat frames have stable current-build golden bytes" caseHeartbeatFrameGolden,
      testCase "every split point preserves one frame" caseEverySplit,
      testCase "every split point preserves both heartbeat arms" caseEveryHeartbeatSplit,
      testCase "byte-at-a-time input matches one-shot input" caseByteAtATime,
      testProperty "generated chunk schedules match one-shot decoding" propGeneratedChunkEquivalence,
      testCase "coalesced frames preserve order" caseCoalesced,
      testCase "one complete frame plus a partial frame retains exact continuation" caseExactPartialContinuation,
      testCase "valid prefixes survive a following malformed payload" caseValidPrefixBeforeFailure,
      testCase "EAPP family bytes fail as the wrong family" caseFamilyIsolation,
      testCase "invalid declared length fails closed" caseInvalidLength,
      testCase "malformed payload fails closed" caseMalformedPayload,
      testCase "EOF distinguishes empty state from every retained strict prefix" caseEndOfInput
    ]

caseHeader :: IO ()
caseHeader = do
  assertEqual "declared body length" (fromIntegral (ByteString.length frame - 4)) (word32At 0 frame)
  assertEqual "family magic" peerFrameMagic (word32At 4 frame)
  where
    frame = encodePeerFrame firstEnvelope

caseHeartbeatFrameGolden :: IO ()
caseHeartbeatFrameGolden = do
  assertEqual "Ping frame" heartbeatPingFrameGolden (encodePeerFrame heartbeatPingEnvelope)
  assertEqual "Pong frame" heartbeatPongFrameGolden (encodePeerFrame heartbeatPongEnvelope)

heartbeatPingFrameGolden :: ByteString
heartbeatPingFrameGolden =
  ByteString.pack
    [ 0,
      0,
      0,
      14,
      0x45,
      0x50,
      0x52,
      0x50,
      1,
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8
    ]

heartbeatPongFrameGolden :: ByteString
heartbeatPongFrameGolden =
  ByteString.pack
    [ 0,
      0,
      0,
      14,
      0x45,
      0x50,
      0x52,
      0x50,
      1,
      1,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8
    ]

caseEverySplit :: IO ()
caseEverySplit =
  mapM_
    ( \point ->
        assertEqual
          ("split point " <> show point)
          (Right [firstEnvelope])
          (driveChunks [ByteString.take point frame, ByteString.drop point frame])
    )
    [0 .. ByteString.length frame]
  where
    frame = encodePeerFrame firstEnvelope

caseEveryHeartbeatSplit :: IO ()
caseEveryHeartbeatSplit =
  mapM_ assertEverySplit [heartbeatPingEnvelope, heartbeatPongEnvelope]
  where
    assertEverySplit envelope =
      mapM_
        ( \point ->
            assertEqual
              (show envelope <> ", split point " <> show point)
              (Right [envelope])
              (driveChunks [ByteString.take point frame, ByteString.drop point frame])
        )
        [0 .. ByteString.length frame]
      where
        frame = encodePeerFrame envelope

caseByteAtATime :: IO ()
caseByteAtATime =
  assertEqual
    "byte-at-a-time"
    (Right [firstEnvelope])
    (driveChunks (fmap ByteString.singleton (ByteString.unpack frame)))
  where
    frame = encodePeerFrame firstEnvelope

propGeneratedChunkEquivalence :: [Word8] -> Property
propGeneratedChunkEquivalence widths =
  conjoin (fmap assertEnvelope allEnvelopes)
  where
    assertEnvelope envelope =
      counterexample diagnostic (incremental === oneShot)
      where
        frame = encodePeerFrame envelope
        chunks = scheduledChunks widths frame
        oneShot = driveChunks [frame]
        incremental = driveChunks chunks
        diagnostic =
          "envelope: "
            <> show envelope
            <> "\nschedule widths: "
            <> show widths
            <> "\nactual chunk sizes: "
            <> show (fmap ByteString.length chunks)

scheduledChunks :: [Word8] -> ByteString -> [ByteString]
scheduledChunks = go
  where
    go _ remaining | ByteString.null remaining = []
    go [] remaining = [remaining]
    go (width : widths) remaining =
      chunk : go widths following
      where
        byteCount = 1 + fromIntegral width `mod` ByteString.length remaining
        (chunk, following) = ByteString.splitAt byteCount remaining

caseCoalesced :: IO ()
caseCoalesced =
  assertEqual
    "two frames"
    (Right [firstEnvelope, secondEnvelope])
    (driveChunks [encodePeerFrame firstEnvelope <> encodePeerFrame secondEnvelope])

caseExactPartialContinuation :: IO ()
caseExactPartialContinuation =
  case feedPeerFrame initialPeerFrameDecoder (firstFrame <> partialSecond) of
    PeerFrameFeedResult [actual] (NeedPeerFrameBytes combinedDecoder) -> do
      assertEqual "valid prefix" firstEnvelope actual
      case feedPeerFrame initialPeerFrameDecoder partialSecond of
        PeerFrameFeedResult [] (NeedPeerFrameBytes partialDecoder) ->
          assertEqual "exact retained partial bytes" partialDecoder combinedDecoder
        other -> assertFailure ("unexpected direct partial result: " <> show other)
    other -> assertFailure ("unexpected coalesced partial result: " <> show other)
  where
    firstFrame = encodePeerFrame firstEnvelope
    partialSecond = ByteString.take 5 (encodePeerFrame secondEnvelope)

caseValidPrefixBeforeFailure :: IO ()
caseValidPrefixBeforeFailure = do
  let bytes = encodePeerFrame firstEnvelope <> malformedPayloadFrame
  mapM_
    ( \point ->
        assertEqual
          ("surrounding split point " <> show point)
          ([firstEnvelope], PeerFrameFailed (PeerFramePayloadError PeerPayloadMalformed))
          ( feedChunksUntilStop
              initialPeerFrameDecoder
              [ByteString.take point bytes, ByteString.drop point bytes]
          )
    )
    [0 .. ByteString.length bytes]

caseFamilyIsolation :: IO ()
caseFamilyIsolation = do
  assertWrongFamily "header" eappHeader
  assertWrongFamily "Ping" (withEappMagic heartbeatPingFrameGolden)
  assertWrongFamily "Pong" (withEappMagic heartbeatPongFrameGolden)
  where
    eappHeader = ByteString.pack [0, 0, 1, 0, 0x45, 0x41, 0x50, 0x50]
    withEappMagic frame =
      ByteString.take 4 frame
        <> ByteString.pack [0x45, 0x41, 0x50, 0x50]
        <> ByteString.drop 8 frame
    assertWrongFamily label bytes =
      assertEqual
        ("EAPP magic in " <> label)
        (PeerFrameFeedResult [] (PeerFrameFailed PeerFrameWrongFamily))
        (feedPeerFrame initialPeerFrameDecoder bytes)

caseInvalidLength :: IO ()
caseInvalidLength =
  assertEqual
    "length shorter than family magic"
    (PeerFrameFeedResult [] (PeerFrameFailed PeerFrameInvalidLength))
    (feedPeerFrame initialPeerFrameDecoder (ByteString.replicate 4 0))

caseMalformedPayload :: IO ()
caseMalformedPayload =
  assertEqual
    "invalid generic outer constructor"
    (PeerFrameFeedResult [] (PeerFrameFailed (PeerFramePayloadError PeerPayloadMalformed)))
    (feedPeerFrame initialPeerFrameDecoder malformedPayloadFrame)

caseEndOfInput :: IO ()
caseEndOfInput = do
  assertEqual "empty decoder" (Right ()) (finishPeerFrameDecoder initialPeerFrameDecoder)
  mapM_ assertTruncatedPrefix [1 .. ByteString.length frame - 1]
  case feedPeerFrame initialPeerFrameDecoder frame of
    PeerFrameFeedResult [_] (NeedPeerFrameBytes decoder) ->
      assertEqual "complete frame leaves empty state" (Right ()) (finishPeerFrameDecoder decoder)
    other -> assertFailure ("unexpected complete-frame result: " <> show other)
  where
    frame = encodePeerFrame firstEnvelope
    assertTruncatedPrefix byteCount =
      case feedPeerFrame initialPeerFrameDecoder (ByteString.take byteCount frame) of
        PeerFrameFeedResult [] (NeedPeerFrameBytes decoder) ->
          assertEqual
            ("truncated prefix " <> show byteCount)
            (Left PeerFrameTruncated)
            (finishPeerFrameDecoder decoder)
        other -> assertFailure ("unexpected prefix result: " <> show other)

driveChunks :: [ByteString] -> Either PeerFrameError [PeerEnvelope]
driveChunks = go initialPeerFrameDecoder []
  where
    go decoder reversedEnvelopes [] =
      case finishPeerFrameDecoder decoder of
        Left frameError -> Left frameError
        Right () -> Right (reverse reversedEnvelopes)
    go decoder reversedEnvelopes (chunk : chunks) =
      case feedPeerFrame decoder chunk of
        PeerFrameFeedResult _ (PeerFrameFailed frameError) -> Left frameError
        PeerFrameFeedResult envelopes (NeedPeerFrameBytes nextDecoder) ->
          go nextDecoder (reverse envelopes <> reversedEnvelopes) chunks

feedChunksUntilStop ::
  PeerFrameDecoder ->
  [ByteString] ->
  ([PeerEnvelope], PeerFrameContinuation)
feedChunksUntilStop decoder = go decoder []
  where
    go current reversedEnvelopes [] =
      (reverse reversedEnvelopes, NeedPeerFrameBytes current)
    go current reversedEnvelopes (chunk : chunks) =
      case feedPeerFrame current chunk of
        PeerFrameFeedResult envelopes failed@(PeerFrameFailed _) ->
          (reverse (reverse envelopes <> reversedEnvelopes), failed)
        PeerFrameFeedResult envelopes (NeedPeerFrameBytes next) ->
          go next (reverse envelopes <> reversedEnvelopes) chunks

word32At :: Int -> ByteString -> Word32
word32At offset =
  runGet getWord32be
    . LazyByteString.fromStrict
    . ByteString.take 4
    . ByteString.drop offset

malformedPayloadFrame :: ByteString
malformedPayloadFrame =
  ByteString.pack [0, 0, 0, 5, 0x45, 0x50, 0x52, 0x50, 0xff]

firstEnvelope :: PeerEnvelope
firstEnvelope = case allEnvelopes of
  envelope : _ -> envelope
  [] -> error "the closed peer-envelope fixture catalogue is empty"

secondEnvelope :: PeerEnvelope
secondEnvelope = case allEnvelopes of
  _ : envelope : _ -> envelope
  _ -> error "the closed peer-envelope fixture catalogue has fewer than two entries"

heartbeatPingEnvelope :: PeerEnvelope
heartbeatPingEnvelope = PeerHeartbeatEnvelope (Ping heartbeatNonce)

heartbeatPongEnvelope :: PeerEnvelope
heartbeatPongEnvelope = PeerHeartbeatEnvelope (Pong heartbeatNonce)
