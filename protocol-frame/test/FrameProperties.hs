module FrameProperties
  ( tests,
  )
where

import Data.Binary.Get (getWord32be, runGet)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Word (Word32, Word8)
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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    counterexample,
    testProperty,
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "family-neutral frame"
    [ testCase "header includes magic and excludes its own length" caseHeader,
      testCase "every split point preserves one body" caseEverySplit,
      testCase "byte-at-a-time input matches one-shot input" caseByteAtATime,
      testProperty "generated chunk schedules match one-shot decoding" propGeneratedChunkEquivalence,
      testCase "coalesced frames preserve body order" caseCoalesced,
      testCase "one complete frame plus a partial frame retains the exact continuation" caseExactPartialContinuation,
      testCase "valid prefixes survive a following framing failure" caseValidPrefixBeforeFailure,
      testCase "wrong family fails as soon as the complete header arrives" caseEarlyWrongFamily,
      testCase "declared length below the magic width fails" caseInvalidLength,
      testCase "EOF distinguishes empty state from every retained strict prefix" caseEndOfInput
    ]

caseHeader :: IO ()
caseHeader = do
  assertEqual "declared frame length" (fromIntegral (ByteString.length frame - 4)) (word32At 0 frame)
  assertEqual "family magic" familyMagic (word32At 4 frame)
  assertEqual "opaque body" body (ByteString.drop 8 frame)
  where
    frame = encodeFrame familyMagic body

caseEverySplit :: IO ()
caseEverySplit =
  mapM_
    ( \point ->
        assertEqual
          ("split point " <> show point)
          (Right [body])
          (driveChunks [ByteString.take point frame, ByteString.drop point frame])
    )
    [0 .. ByteString.length frame]
  where
    frame = encodeFrame familyMagic body

caseByteAtATime :: IO ()
caseByteAtATime =
  assertEqual
    "byte-at-a-time"
    (Right [body])
    (driveChunks (fmap ByteString.singleton (ByteString.unpack frame)))
  where
    frame = encodeFrame familyMagic body

propGeneratedChunkEquivalence :: [Word8] -> Property
propGeneratedChunkEquivalence widths =
  counterexample diagnostic (incremental === oneShot)
  where
    bytes = mconcat (fmap (encodeFrame familyMagic) bodies)
    chunks = scheduledChunks widths bytes
    oneShot = driveChunks [bytes]
    incremental = driveChunks chunks
    diagnostic =
      "schedule widths: "
        <> show widths
        <> "\nactual chunk sizes: "
        <> show (fmap ByteString.length chunks)

caseCoalesced :: IO ()
caseCoalesced =
  assertEqual
    "three bodies"
    (Right bodies)
    (driveChunks [mconcat (fmap (encodeFrame familyMagic) bodies)])

caseExactPartialContinuation :: IO ()
caseExactPartialContinuation =
  case feedFrame (initialFrameDecoder familyMagic) (firstFrame <> partialSecond) of
    FrameFeedResult [actual] (NeedFrameBytes combinedDecoder) -> do
      assertEqual "complete prefix" firstBody actual
      case feedFrame (initialFrameDecoder familyMagic) partialSecond of
        FrameFeedResult [] (NeedFrameBytes partialDecoder) ->
          assertEqual "exact retained partial bytes" partialDecoder combinedDecoder
        other -> assertFailure ("unexpected direct partial result: " <> show other)
    other -> assertFailure ("unexpected coalesced partial result: " <> show other)
  where
    firstFrame = encodeFrame familyMagic firstBody
    partialSecond = ByteString.take 5 (encodeFrame familyMagic secondBody)

caseValidPrefixBeforeFailure :: IO ()
caseValidPrefixBeforeFailure =
  mapM_
    ( \point ->
        assertEqual
          ("surrounding split point " <> show point)
          ([body], FrameFailed FrameWrongFamily)
          ( feedChunksUntilStop
              (initialFrameDecoder familyMagic)
              [ByteString.take point bytes, ByteString.drop point bytes]
          )
    )
    [0 .. ByteString.length bytes]
  where
    bytes = encodeFrame familyMagic body <> encodeFrame wrongFamilyMagic ByteString.empty

caseEarlyWrongFamily :: IO ()
caseEarlyWrongFamily =
  assertEqual
    "a declared body is not awaited after its family is already wrong"
    (FrameFeedResult [] (FrameFailed FrameWrongFamily))
    ( feedFrame
        (initialFrameDecoder familyMagic)
        (ByteString.pack [0, 0, 1, 0, 0, 0, 0, 0])
    )

caseInvalidLength :: IO ()
caseInvalidLength =
  assertEqual
    "zero declared length"
    (FrameFeedResult [] (FrameFailed FrameInvalidLength))
    (feedFrame (initialFrameDecoder familyMagic) (ByteString.replicate 4 0))

caseEndOfInput :: IO ()
caseEndOfInput = do
  assertEqual
    "empty decoder"
    (Right ())
    (finishFrameDecoder (initialFrameDecoder familyMagic))
  mapM_ assertTruncatedPrefix [1 .. ByteString.length frame - 1]
  case feedFrame (initialFrameDecoder familyMagic) frame of
    FrameFeedResult [_] (NeedFrameBytes decoder) ->
      assertEqual "complete frame leaves empty state" (Right ()) (finishFrameDecoder decoder)
    other -> assertFailure ("unexpected complete-frame result: " <> show other)
  where
    frame = encodeFrame familyMagic body
    assertTruncatedPrefix byteCount =
      case feedFrame
        (initialFrameDecoder familyMagic)
        (ByteString.take byteCount frame) of
        FrameFeedResult [] (NeedFrameBytes decoder) ->
          assertEqual
            ("truncated prefix " <> show byteCount)
            (Left FrameTruncated)
            (finishFrameDecoder decoder)
        other -> assertFailure ("unexpected prefix result: " <> show other)

driveChunks :: [ByteString] -> Either FrameError [ByteString]
driveChunks = go (initialFrameDecoder familyMagic) []
  where
    go decoder reversedBodies [] =
      case finishFrameDecoder decoder of
        Left frameError -> Left frameError
        Right () -> Right (reverse reversedBodies)
    go decoder reversedBodies (chunk : chunks) =
      case feedFrame decoder chunk of
        FrameFeedResult _ (FrameFailed frameError) -> Left frameError
        FrameFeedResult completeBodies (NeedFrameBytes nextDecoder) ->
          go nextDecoder (reverse completeBodies <> reversedBodies) chunks

feedChunksUntilStop :: FrameDecoder -> [ByteString] -> ([ByteString], FrameContinuation)
feedChunksUntilStop decoder = go decoder []
  where
    go current reversedBodies [] =
      (reverse reversedBodies, NeedFrameBytes current)
    go current reversedBodies (chunk : chunks) =
      case feedFrame current chunk of
        FrameFeedResult completeBodies failed@(FrameFailed _) ->
          (reverse (reverse completeBodies <> reversedBodies), failed)
        FrameFeedResult completeBodies (NeedFrameBytes next) ->
          go next (reverse completeBodies <> reversedBodies) chunks

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

word32At :: Int -> ByteString -> Word32
word32At offset =
  runGet getWord32be
    . LazyByteString.fromStrict
    . ByteString.take 4
    . ByteString.drop offset

familyMagic :: Word32
familyMagic = 0x45415050

wrongFamilyMagic :: Word32
wrongFamilyMagic = 0x45505250

body :: ByteString
body = ByteString.pack [0, 1, 2, 3, 4, 255]

firstBody :: ByteString
firstBody = ByteString.empty

secondBody :: ByteString
secondBody = body

bodies :: [ByteString]
bodies = [firstBody, secondBody, ByteString.pack [9, 8, 7]]
