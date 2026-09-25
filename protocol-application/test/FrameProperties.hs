module FrameProperties
  ( tests,
  )
where

import Data.Binary.Get (getWord32be, runGet)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Word (Word32, Word8)
import Eclips.Protocol.Application.Codec (ApplicationPayloadError (..))
import Eclips.Protocol.Application.Frame
  ( ApplicationFrameContinuation (..),
    ApplicationFrameDecoder,
    ApplicationFrameDirection (..),
    ApplicationFrameError (..),
    ApplicationFrameFeedResult (..),
    applicationFrameMagic,
    encodeApplicationFrame,
    feedApplicationFrame,
    finishApplicationFrameDecoder,
    initialApplicationFrameDecoder,
  )
import Eclips.Protocol.Application.Types
  ( ApplicationClientDto (..),
    ApplicationEnvelope (..),
    ApplicationServerDto (..),
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
import TestFixtures
  ( allEnvelopes,
    clientEnvelopes,
    currentClientEnvelope,
    currentServerEnvelope,
    heartbeatNonce,
    serverEnvelopes,
  )

tests :: TestTree
tests =
  testGroup
    "EAPP frame"
    [ testCase "header has fixed EAPP magic and excludes its own length" caseHeader,
      testCase "every split point preserves one frame" caseEverySplit,
      testCase "byte-at-a-time input matches one-shot input" caseByteAtATime,
      testProperty "generated chunk schedules match one-shot decoding for every envelope" propGeneratedChunkEquivalence,
      testCase "coalesced frames preserve order" caseCoalesced,
      testCase "one complete frame plus a partial frame retains exact continuation" caseExactPartialContinuation,
      testCase "valid prefixes survive a following malformed frame" caseValidPrefixBeforeFailure,
      testCase "wrong family fails as soon as the complete header arrives" caseEarlyWrongFamily,
      testCase "wrong envelope direction fails closed" caseWrongDirection,
      testCase "heartbeat request and response cannot cross EAPP directions" caseHeartbeatDirection,
      testCase "invalid declared length fails closed" caseInvalidLength,
      testCase "malformed payload fails closed" caseMalformedPayload,
      testCase "a later framing failure does not leapfrog an earlier payload failure" casePayloadFailurePrecedesLaterFrameFailure,
      testCase "EOF distinguishes empty state from every retained strict prefix" caseEndOfInput
    ]

caseHeader :: IO ()
caseHeader = do
  assertEqual "declared body length" (fromIntegral (ByteString.length frame - 4)) (word32At 0 frame)
  assertEqual "family magic" applicationFrameMagic (word32At 4 frame)
  where
    frame = encodeApplicationFrame clientEnvelope

caseEverySplit :: IO ()
caseEverySplit =
  mapM_
    ( \point ->
        assertEqual
          ("split point " <> show point)
          (Right [clientEnvelope])
          ( driveChunks
              ApplicationClientFrames
              [ByteString.take point frame, ByteString.drop point frame]
          )
    )
    [0 .. ByteString.length frame]
  where
    frame = encodeApplicationFrame clientEnvelope

caseByteAtATime :: IO ()
caseByteAtATime =
  assertEqual
    "byte-at-a-time"
    (Right [clientEnvelope])
    (driveChunks ApplicationClientFrames (fmap ByteString.singleton (ByteString.unpack frame)))
  where
    frame = encodeApplicationFrame clientEnvelope

propGeneratedChunkEquivalence :: [Word8] -> Property
propGeneratedChunkEquivalence widths =
  conjoin (fmap assertEnvelope allEnvelopes)
  where
    assertEnvelope envelope =
      counterexample diagnostic (incremental === oneShot)
      where
        direction = envelopeDirection envelope
        frame = encodeApplicationFrame envelope
        chunks = scheduledChunks widths frame
        oneShot = driveChunks direction [frame]
        incremental = driveChunks direction chunks
        diagnostic =
          "envelope: "
            <> show envelope
            <> "\nschedule widths: "
            <> show widths
            <> "\nactual chunk sizes: "
            <> show (fmap ByteString.length chunks)

envelopeDirection :: ApplicationEnvelope -> ApplicationFrameDirection
envelopeDirection (ApplicationClientEnvelope _) = ApplicationClientFrames
envelopeDirection (ApplicationServerEnvelope _) = ApplicationServerFrames

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
caseCoalesced = do
  assertEqual
    "a new-environment call follows an earlier client frame"
    (Right [firstClientEnvelope, currentClientEnvelope])
    (driveChunks ApplicationClientFrames [encodeApplicationFrame firstClientEnvelope <> encodeApplicationFrame currentClientEnvelope])
  assertEqual
    "a new-environment result follows an earlier server frame"
    (Right [serverEnvelope, currentServerEnvelope])
    (driveChunks ApplicationServerFrames [encodeApplicationFrame serverEnvelope <> encodeApplicationFrame currentServerEnvelope])

caseExactPartialContinuation :: IO ()
caseExactPartialContinuation =
  case feedApplicationFrame (initialApplicationFrameDecoder ApplicationClientFrames) (firstFrame <> partialSecond) of
    ApplicationFrameFeedResult [actual] (NeedApplicationFrameBytes combinedDecoder) -> do
      assertEqual "valid prefix" firstClientEnvelope actual
      case feedApplicationFrame (initialApplicationFrameDecoder ApplicationClientFrames) partialSecond of
        ApplicationFrameFeedResult [] (NeedApplicationFrameBytes partialDecoder) ->
          assertEqual "exact retained partial bytes" partialDecoder combinedDecoder
        other -> assertFailure ("unexpected direct partial result: " <> show other)
    other -> assertFailure ("unexpected coalesced partial result: " <> show other)
  where
    firstFrame = encodeApplicationFrame firstClientEnvelope
    partialSecond = ByteString.take 5 (encodeApplicationFrame currentClientEnvelope)

caseValidPrefixBeforeFailure :: IO ()
caseValidPrefixBeforeFailure = do
  let bytes = encodeApplicationFrame clientEnvelope <> wrongFamilyFrame
  mapM_
    ( \point ->
        assertEqual
          ("surrounding split point " <> show point)
          ([clientEnvelope], ApplicationFrameFailed ApplicationFrameWrongFamily)
          ( feedChunksUntilStop
              (initialApplicationFrameDecoder ApplicationClientFrames)
              [ByteString.take point bytes, ByteString.drop point bytes]
          )
    )
    [0 .. ByteString.length bytes]

caseEarlyWrongFamily :: IO ()
caseEarlyWrongFamily =
  assertEqual
    "a declared payload is not awaited after its family is already wrong"
    (ApplicationFrameFeedResult [] (ApplicationFrameFailed ApplicationFrameWrongFamily))
    ( feedApplicationFrame
        (initialApplicationFrameDecoder ApplicationClientFrames)
        (ByteString.pack [0, 0, 1, 0, 0, 0, 0, 0])
    )

caseWrongDirection :: IO ()
caseWrongDirection =
  assertEqual
    "server envelope on client-input decoder"
    (ApplicationFrameFeedResult [] (ApplicationFrameFailed ApplicationFrameWrongEnvelopeDirection))
    (feedApplicationFrame (initialApplicationFrameDecoder ApplicationClientFrames) (encodeApplicationFrame serverEnvelope))

caseHeartbeatDirection :: IO ()
caseHeartbeatDirection = do
  assertEqual
    "Ping on the server-to-client decoder"
    wrongDirection
    ( feedApplicationFrame
        (initialApplicationFrameDecoder ApplicationServerFrames)
        (encodeApplicationFrame (ApplicationClientEnvelope (Ping heartbeatNonce)))
    )
  assertEqual
    "Pong on the client-to-server decoder"
    wrongDirection
    ( feedApplicationFrame
        (initialApplicationFrameDecoder ApplicationClientFrames)
        (encodeApplicationFrame (ApplicationServerEnvelope (Pong heartbeatNonce)))
    )
  where
    wrongDirection =
      ApplicationFrameFeedResult
        []
        (ApplicationFrameFailed ApplicationFrameWrongEnvelopeDirection)

caseInvalidLength :: IO ()
caseInvalidLength =
  assertEqual
    "length shorter than family magic"
    (ApplicationFrameFeedResult [] (ApplicationFrameFailed ApplicationFrameInvalidLength))
    (feedApplicationFrame (initialApplicationFrameDecoder ApplicationClientFrames) (ByteString.replicate 4 0))

caseMalformedPayload :: IO ()
caseMalformedPayload =
  assertEqual
    "invalid generic outer constructor"
    ( ApplicationFrameFeedResult
        []
        (ApplicationFrameFailed (ApplicationFramePayloadError ApplicationPayloadMalformed))
    )
    (feedApplicationFrame (initialApplicationFrameDecoder ApplicationClientFrames) malformedPayloadFrame)

casePayloadFailurePrecedesLaterFrameFailure :: IO ()
casePayloadFailurePrecedesLaterFrameFailure =
  assertEqual
    "the first semantic failure in stream order wins"
    ( ApplicationFrameFeedResult
        [clientEnvelope]
        (ApplicationFrameFailed (ApplicationFramePayloadError ApplicationPayloadMalformed))
    )
    ( feedApplicationFrame
        (initialApplicationFrameDecoder ApplicationClientFrames)
        (encodeApplicationFrame clientEnvelope <> malformedPayloadFrame <> wrongFamilyFrame)
    )

caseEndOfInput :: IO ()
caseEndOfInput = do
  assertEqual
    "empty decoder"
    (Right ())
    (finishApplicationFrameDecoder (initialApplicationFrameDecoder ApplicationClientFrames))
  mapM_ assertTruncatedPrefix [1 .. ByteString.length frame - 1]
  case feedApplicationFrame (initialApplicationFrameDecoder ApplicationClientFrames) frame of
    ApplicationFrameFeedResult [_] (NeedApplicationFrameBytes decoder) ->
      assertEqual "complete frame leaves empty state" (Right ()) (finishApplicationFrameDecoder decoder)
    other -> assertFailure ("unexpected complete-frame result: " <> show other)
  where
    frame = encodeApplicationFrame clientEnvelope
    assertTruncatedPrefix byteCount =
      case feedApplicationFrame
        (initialApplicationFrameDecoder ApplicationClientFrames)
        (ByteString.take byteCount frame) of
        ApplicationFrameFeedResult [] (NeedApplicationFrameBytes decoder) ->
          assertEqual
            ("truncated prefix " <> show byteCount)
            (Left ApplicationFrameTruncated)
            (finishApplicationFrameDecoder decoder)
        other -> assertFailure ("unexpected prefix result: " <> show other)

driveChunks :: ApplicationFrameDirection -> [ByteString] -> Either ApplicationFrameError [ApplicationEnvelope]
driveChunks direction = go (initialApplicationFrameDecoder direction) []
  where
    go decoder reversedEnvelopes [] =
      case finishApplicationFrameDecoder decoder of
        Left frameError -> Left frameError
        Right () -> Right (reverse reversedEnvelopes)
    go decoder reversedEnvelopes (chunk : chunks) =
      case feedApplicationFrame decoder chunk of
        ApplicationFrameFeedResult _ (ApplicationFrameFailed frameError) ->
          Left frameError
        ApplicationFrameFeedResult envelopes (NeedApplicationFrameBytes nextDecoder) ->
          go nextDecoder (reverse envelopes <> reversedEnvelopes) chunks

feedChunksUntilStop :: ApplicationFrameDecoder -> [ByteString] -> ([ApplicationEnvelope], ApplicationFrameContinuation)
feedChunksUntilStop decoder = go decoder []
  where
    go current reversedEnvelopes [] =
      (reverse reversedEnvelopes, NeedApplicationFrameBytes current)
    go current reversedEnvelopes (chunk : chunks) =
      case feedApplicationFrame current chunk of
        ApplicationFrameFeedResult envelopes failed@(ApplicationFrameFailed _) ->
          (reverse (reverse envelopes <> reversedEnvelopes), failed)
        ApplicationFrameFeedResult envelopes (NeedApplicationFrameBytes next) ->
          go next (reverse envelopes <> reversedEnvelopes) chunks

word32At :: Int -> ByteString -> Word32
word32At offset =
  runGet getWord32be
    . LazyByteString.fromStrict
    . ByteString.take 4
    . ByteString.drop offset

wrongFamilyFrame :: ByteString
wrongFamilyFrame =
  ByteString.take 4 frame
    <> ByteString.replicate 4 0
    <> ByteString.drop 8 frame
  where
    frame = encodeApplicationFrame clientEnvelope

malformedPayloadFrame :: ByteString
malformedPayloadFrame = ByteString.pack [0, 0, 0, 5, 0x45, 0x41, 0x50, 0x50, 0xff]

clientEnvelope :: ApplicationEnvelope
clientEnvelope = firstClientEnvelope

serverEnvelope :: ApplicationEnvelope
serverEnvelope = case serverEnvelopes of
  envelope : _ -> envelope
  [] -> error "the server-envelope fixture catalogue is empty"

firstClientEnvelope :: ApplicationEnvelope
firstClientEnvelope = case clientEnvelopes of
  envelope : _ -> envelope
  [] -> error "the client-envelope fixture catalogue is empty"
