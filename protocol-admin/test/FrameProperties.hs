module FrameProperties
  ( tests,
  )
where

import Data.Binary.Get (getWord32be, runGet)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Word (Word32, Word8)
import Eclips.Protocol.Admin.Codec (AdminPayloadError (AdminPayloadMalformed))
import Eclips.Protocol.Admin.Frame
import Eclips.Protocol.Admin.Types (AdminEnvelope (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    testProperty,
    (===),
  )
import TestFixtures (allEnvelopes, clientEnvelopes, serverEnvelopes)

tests :: TestTree
tests =
  testGroup
    "EADM frame"
    [ testCase "header has fixed EADM family magic" caseHeader,
      testCase "every split point preserves one frame" caseEverySplit,
      testProperty "generated chunk schedules match one-shot decoding" propChunkEquivalence,
      testCase "coalesced frames preserve order" caseCoalesced,
      testCase "wrong envelope direction fails closed" caseWrongDirection,
      testCase "wrong family fails at the header" caseWrongFamily,
      testCase "invalid declared length fails closed" caseInvalidLength,
      testCase "malformed payload fails closed" caseMalformedPayload,
      testCase "EOF rejects every retained strict prefix" caseEndOfInput
    ]

caseHeader :: IO ()
caseHeader = do
  assertEqual "declared body length" (fromIntegral (ByteString.length frame - 4)) (word32At 0 frame)
  assertEqual "family magic" adminFrameMagic (word32At 4 frame)
  where
    frame = encodeAdminFrame clientEnvelope

caseEverySplit :: IO ()
caseEverySplit =
  mapM_
    ( \point ->
        assertEqual
          ("split point " <> show point)
          (Right [clientEnvelope])
          (driveChunks AdminClientFrames [ByteString.take point frame, ByteString.drop point frame])
    )
    [0 .. ByteString.length frame]
  where
    frame = encodeAdminFrame clientEnvelope

propChunkEquivalence :: [Word8] -> Property
propChunkEquivalence widths =
  conjoin (fmap assertEnvelope allEnvelopes)
  where
    assertEnvelope envelope =
      counterexample (show envelope) (incremental === oneShot)
      where
        direction = envelopeDirection envelope
        frame = encodeAdminFrame envelope
        oneShot = driveChunks direction [frame]
        incremental = driveChunks direction (scheduledChunks widths frame)

caseCoalesced :: IO ()
caseCoalesced =
  assertEqual
    "client order"
    (Right clientEnvelopes)
    (driveChunks AdminClientFrames [foldMap encodeAdminFrame clientEnvelopes])

caseWrongDirection :: IO ()
caseWrongDirection =
  assertEqual
    "server envelope at client endpoint"
    (AdminFrameFeedResult [] (AdminFrameFailed AdminFrameWrongEnvelopeDirection))
    (feedAdminFrame (initialAdminFrameDecoder AdminClientFrames) (encodeAdminFrame serverEnvelope))

caseWrongFamily :: IO ()
caseWrongFamily =
  assertEqual
    "family mismatch"
    (AdminFrameFeedResult [] (AdminFrameFailed AdminFrameWrongFamily))
    (feedAdminFrame (initialAdminFrameDecoder AdminClientFrames) wrongFamilyHeader)

caseInvalidLength :: IO ()
caseInvalidLength =
  assertEqual
    "length shorter than family magic"
    (AdminFrameFeedResult [] (AdminFrameFailed AdminFrameInvalidLength))
    (feedAdminFrame (initialAdminFrameDecoder AdminClientFrames) (ByteString.replicate 4 0))

caseMalformedPayload :: IO ()
caseMalformedPayload =
  assertEqual
    "malformed generic payload"
    ( AdminFrameFeedResult
        []
        (AdminFrameFailed (AdminFramePayloadError AdminPayloadMalformed))
    )
    (feedAdminFrame (initialAdminFrameDecoder AdminClientFrames) malformedPayloadFrame)

caseEndOfInput :: IO ()
caseEndOfInput = do
  assertEqual
    "empty decoder"
    (Right ())
    (finishAdminFrameDecoder (initialAdminFrameDecoder AdminClientFrames))
  mapM_ assertTruncated [1 .. ByteString.length frame - 1]
  where
    frame = encodeAdminFrame clientEnvelope
    assertTruncated byteCount =
      case feedAdminFrame
        (initialAdminFrameDecoder AdminClientFrames)
        (ByteString.take byteCount frame) of
        AdminFrameFeedResult [] (NeedAdminFrameBytes decoder) ->
          assertEqual
            ("prefix " <> show byteCount)
            (Left AdminFrameTruncated)
            (finishAdminFrameDecoder decoder)
        other -> assertFailure ("unexpected strict-prefix result: " <> show other)

envelopeDirection :: AdminEnvelope -> AdminFrameDirection
envelopeDirection (AdminClientEnvelope _) = AdminClientFrames
envelopeDirection (AdminServerEnvelope _) = AdminServerFrames

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

driveChunks :: AdminFrameDirection -> [ByteString] -> Either AdminFrameError [AdminEnvelope]
driveChunks direction = go (initialAdminFrameDecoder direction) []
  where
    go decoder reversed [] =
      case finishAdminFrameDecoder decoder of
        Left frameError -> Left frameError
        Right () -> Right (reverse reversed)
    go decoder reversed (chunk : chunks) =
      case feedAdminFrame decoder chunk of
        AdminFrameFeedResult _ (AdminFrameFailed frameError) -> Left frameError
        AdminFrameFeedResult envelopes (NeedAdminFrameBytes successor) ->
          go successor (reverse envelopes <> reversed) chunks

word32At :: Int -> ByteString -> Word32
word32At offset =
  runGet getWord32be
    . LazyByteString.fromStrict
    . ByteString.take 4
    . ByteString.drop offset

wrongFamilyHeader :: ByteString
wrongFamilyHeader = ByteString.pack [0, 0, 0, 4, 0, 0, 0, 0]

malformedPayloadFrame :: ByteString
malformedPayloadFrame = ByteString.pack [0, 0, 0, 5, 0x45, 0x41, 0x44, 0x4d, 0xff]

clientEnvelope :: AdminEnvelope
clientEnvelope = case clientEnvelopes of
  envelope : _ -> envelope
  [] -> error "the client fixture catalogue is empty"

serverEnvelope :: AdminEnvelope
serverEnvelope = case serverEnvelopes of
  envelope : _ -> envelope
  [] -> error "the server fixture catalogue is empty"
