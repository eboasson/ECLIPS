module FrameProperties
  ( tests,
  )
where

import Data.Binary.Get (getWord32be, runGet)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Word (Word32, Word8)
import Eclips.Protocol.Frame qualified as CommonFrame
import Eclips.Protocol.Raft.Codec
  ( RaftPayloadError (..),
    encodeRaftEnvelope,
  )
import Eclips.Protocol.Raft.Frame
  ( RaftFrameContinuation (..),
    RaftFrameDecoder,
    RaftFrameError (..),
    RaftFrameFeedResult (..),
    encodeRaftFrame,
    feedRaftFrame,
    finishRaftFrameDecoder,
    initialRaftFrameDecoder,
    raftFrameMagic,
  )
import Eclips.Protocol.Raft.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    counterexample,
    testProperty,
    (===),
  )
import TestFixtures

tests :: TestTree
tests =
  testGroup
    "ERFT frame and role admission"
    [ testCase "header has fixed ERFT magic and excludes its own length" caseHeader,
      testCase "every split point preserves the complete current stream" caseEverySplit,
      testCase "byte-at-a-time input matches one-shot input" caseByteAtATime,
      testProperty "generated chunk schedules match one-shot decoding" propGeneratedChunkEquivalence,
      testCase "coalesced hello and all current RPC frames preserve order" caseCoalesced,
      testCase "hello phase advances before a retained partial RPC" caseExactPartialContinuation,
      testCase "an RPC before hello fails role admission" caseRpcBeforeHello,
      testCase "a second hello after establishment fails role admission" caseHelloAfterEstablishment,
      testCase "every RPC source role is checked after hello" caseWrongSourceRole,
      testCase "valid prefixes survive a following malformed payload" caseValidPrefixBeforeFailure,
      testCase "EAPP, EPRP, and EORC bytes fail as the wrong family" caseFamilyIsolation,
      testCase "invalid declared length fails closed" caseInvalidLength,
      testCase "malformed payload fails closed" caseMalformedPayload,
      testCase "EOF distinguishes empty state from every retained strict prefix" caseEndOfInput
    ]

caseHeader :: IO ()
caseHeader = do
  assertEqual "declared body length" (fromIntegral (ByteString.length frame - 4)) (word32At 0 frame)
  assertEqual "independent ERFT constant" 0x45524654 raftFrameMagic
  assertEqual "literal ERFT header" 0x45524654 (word32At 4 frame)
  case CommonFrame.feedFrame (CommonFrame.initialFrameDecoder raftFrameMagic) frame of
    CommonFrame.FrameFeedResult [body] (CommonFrame.NeedFrameBytes _) ->
      assertEqual "common-frame body" (encodeRaftEnvelope (RaftHello hello)) body
    other -> assertFailure ("unexpected common-frame result: " <> show other)
  where
    frame = encodeRaftFrame (RaftHello hello)

caseEverySplit :: IO ()
caseEverySplit =
  mapM_
    ( \point ->
        assertEqual
          ("split point " <> show point)
          (Right validConnectionEnvelopes)
          (driveChunks [ByteString.take point bytes, ByteString.drop point bytes])
    )
    [0 .. ByteString.length bytes]
  where
    bytes = encodeEnvelopes validConnectionEnvelopes

caseByteAtATime :: IO ()
caseByteAtATime =
  assertEqual
    "byte-at-a-time"
    (Right validConnectionEnvelopes)
    (driveChunks (fmap ByteString.singleton (ByteString.unpack bytes)))
  where
    bytes = encodeEnvelopes validConnectionEnvelopes

propGeneratedChunkEquivalence :: [Word8] -> Property
propGeneratedChunkEquivalence widths =
  counterexample diagnostic (incremental === oneShot)
  where
    bytes = encodeEnvelopes validConnectionEnvelopes
    chunks = scheduledChunks widths bytes
    oneShot = driveChunks [bytes]
    incremental = driveChunks chunks
    diagnostic =
      "schedule widths: "
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
    "one symmetric stream"
    (Right validConnectionEnvelopes)
    (driveChunks [encodeEnvelopes validConnectionEnvelopes])

caseExactPartialContinuation :: IO ()
caseExactPartialContinuation =
  case feedRaftFrame (initialRaftFrameDecoder connectionContext) (helloFrame <> partialRpc) of
    RaftFrameFeedResult [RaftHello actualHello] (NeedRaftFrameBytes combinedDecoder) -> do
      assertEqual "valid hello" hello actualHello
      case feedRaftFrame (initialRaftFrameDecoder connectionContext) helloFrame of
        RaftFrameFeedResult [_] (NeedRaftFrameBytes establishedDecoder) ->
          case feedRaftFrame establishedDecoder partialRpc of
            RaftFrameFeedResult [] (NeedRaftFrameBytes partialDecoder) ->
              assertEqual "exact retained partial bytes and phase" partialDecoder combinedDecoder
            other -> assertFailure ("unexpected direct partial result: " <> show other)
        other -> assertFailure ("unexpected hello result: " <> show other)
    other -> assertFailure ("unexpected coalesced partial result: " <> show other)
  where
    helloFrame = encodeRaftFrame (RaftHello hello)
    partialRpc = ByteString.take 5 (encodeRaftFrame (RaftRpc requestVote))

caseRpcBeforeHello :: IO ()
caseRpcBeforeHello =
  assertEqual
    "pre-hello request"
    ( RaftFrameFeedResult
        []
        (RaftFrameFailed (RaftFrameAdmissionError RaftHelloExpected))
    )
    ( feedRaftFrame
        (initialRaftFrameDecoder connectionContext)
        (encodeRaftFrame (RaftRpc requestVote))
    )

caseHelloAfterEstablishment :: IO ()
caseHelloAfterEstablishment =
  assertEqual
    "duplicate hello"
    ( [RaftHello hello],
      RaftFrameFailed (RaftFrameAdmissionError RaftRpcExpected)
    )
    ( feedChunksUntilStop
        (initialRaftFrameDecoder connectionContext)
        [encodeRaftFrame (RaftHello hello) <> encodeRaftFrame (RaftHello hello)]
    )

caseWrongSourceRole :: IO ()
caseWrongSourceRole = do
  let wrongDtos =
        [ mustAdmit (requestVoteDto term1 otherNode index0 term0),
          mustAdmit (requestVoteResponseDto term1 otherNode False),
          mustAdmit (appendEntriesDto term1 otherNode index0 term0 [] index0),
          mustAdmit
            ( appendEntriesResponseDto
                term1
                otherNode
                False
                index0
                index0
                (Just (MissingSuffixFromDto index1))
            ),
          mustAdmit
            (appendEntriesResponseDto term1 otherNode True index2 index0 Nothing),
          mustAdmit
            ( appendEntriesResponseDto
                term1
                otherNode
                False
                index1
                index0
                (Just (ConflictingTermFromDto term1 index2))
            )
        ]
  mapM_
    ( \wrongRpc ->
        assertEqual
          (show (raftRpcKind wrongRpc))
          ( [RaftHello hello],
            RaftFrameFailed
              ( RaftFrameAdmissionError
                  (RaftRpcSourceMismatch remoteNode otherNode)
              )
          )
          ( feedChunksUntilStop
              (initialRaftFrameDecoder connectionContext)
              [ encodeRaftFrame (RaftHello hello)
                  <> encodeRaftFrame (RaftRpc wrongRpc)
              ]
          )
    )
    wrongDtos

caseValidPrefixBeforeFailure :: IO ()
caseValidPrefixBeforeFailure = do
  let prefix = take 2 validConnectionEnvelopes
      bytes = encodeEnvelopes prefix <> malformedPayloadFrame
  mapM_
    ( \point ->
        assertEqual
          ("surrounding split point " <> show point)
          ( prefix,
            RaftFrameFailed (RaftFramePayloadError RaftPayloadMalformed)
          )
          ( feedChunksUntilStop
              (initialRaftFrameDecoder connectionContext)
              [ByteString.take point bytes, ByteString.drop point bytes]
          )
    )
    [0 .. ByteString.length bytes]

caseFamilyIsolation :: IO ()
caseFamilyIsolation = do
  assertBool
    "ERFT has no current family-magic collision"
    (all ((/= raftFrameMagic) . third) foreignFamilies)
  mapM_
    ( \(label, magicBytes, magic) -> do
        let header = foreignHeader magicBytes
        assertEqual (label <> " literal") magic (word32At 4 header)
        assertEqual
          label
          (RaftFrameFeedResult [] (RaftFrameFailed RaftFrameWrongFamily))
          (feedRaftFrame (initialRaftFrameDecoder connectionContext) header)
    )
    foreignFamilies

foreignFamilies :: [(String, [Word8], Word32)]
foreignFamilies =
  [ ("EAPP", [0x45, 0x41, 0x50, 0x50], 0x45415050),
    ("EPRP", [0x45, 0x50, 0x52, 0x50], 0x45505250),
    ("EORC", [0x45, 0x4f, 0x52, 0x43], 0x454f5243)
  ]

third :: (first, second, third) -> third
third (_, _, value) = value

foreignHeader :: [Word8] -> ByteString
foreignHeader magic = ByteString.pack ([0, 0, 1, 0] <> magic)

caseInvalidLength :: IO ()
caseInvalidLength =
  assertEqual
    "length shorter than family magic"
    (RaftFrameFeedResult [] (RaftFrameFailed RaftFrameInvalidLength))
    (feedRaftFrame (initialRaftFrameDecoder connectionContext) (ByteString.replicate 4 0))

caseMalformedPayload :: IO ()
caseMalformedPayload =
  assertEqual
    "invalid generic outer constructor"
    ( RaftFrameFeedResult
        []
        (RaftFrameFailed (RaftFramePayloadError RaftPayloadMalformed))
    )
    (feedRaftFrame (initialRaftFrameDecoder connectionContext) malformedPayloadFrame)

caseEndOfInput :: IO ()
caseEndOfInput = do
  assertEqual
    "empty decoder"
    (Right ())
    (finishRaftFrameDecoder (initialRaftFrameDecoder connectionContext))
  mapM_ assertTruncatedPrefix [1 .. ByteString.length frame - 1]
  case feedRaftFrame (initialRaftFrameDecoder connectionContext) frame of
    RaftFrameFeedResult [_] (NeedRaftFrameBytes decoder) ->
      assertEqual "complete frame leaves empty state" (Right ()) (finishRaftFrameDecoder decoder)
    other -> assertFailure ("unexpected complete-frame result: " <> show other)
  where
    frame = encodeRaftFrame (RaftHello hello)
    assertTruncatedPrefix byteCount =
      case feedRaftFrame
        (initialRaftFrameDecoder connectionContext)
        (ByteString.take byteCount frame) of
        RaftFrameFeedResult [] (NeedRaftFrameBytes decoder) ->
          assertEqual
            ("truncated prefix " <> show byteCount)
            (Left RaftFrameTruncated)
            (finishRaftFrameDecoder decoder)
        other -> assertFailure ("unexpected prefix result: " <> show other)

driveChunks :: [ByteString] -> Either RaftFrameError [RaftProtocolEnvelope]
driveChunks = go (initialRaftFrameDecoder connectionContext) []
  where
    go decoder reversedEnvelopes [] =
      case finishRaftFrameDecoder decoder of
        Left frameError -> Left frameError
        Right () -> Right (reverse reversedEnvelopes)
    go decoder reversedEnvelopes (chunk : chunks) =
      case feedRaftFrame decoder chunk of
        RaftFrameFeedResult _ (RaftFrameFailed frameError) -> Left frameError
        RaftFrameFeedResult envelopes (NeedRaftFrameBytes nextDecoder) ->
          go nextDecoder (reverse envelopes <> reversedEnvelopes) chunks

feedChunksUntilStop ::
  RaftFrameDecoder ->
  [ByteString] ->
  ([RaftProtocolEnvelope], RaftFrameContinuation)
feedChunksUntilStop decoder = go decoder []
  where
    go current reversedEnvelopes [] =
      (reverse reversedEnvelopes, NeedRaftFrameBytes current)
    go current reversedEnvelopes (chunk : chunks) =
      case feedRaftFrame current chunk of
        RaftFrameFeedResult envelopes failed@(RaftFrameFailed _) ->
          (reverse (reverse envelopes <> reversedEnvelopes), failed)
        RaftFrameFeedResult envelopes (NeedRaftFrameBytes next) ->
          go next (reverse envelopes <> reversedEnvelopes) chunks

encodeEnvelopes :: [RaftProtocolEnvelope] -> ByteString
encodeEnvelopes = foldMap encodeRaftFrame

word32At :: Int -> ByteString -> Word32
word32At offset =
  runGet getWord32be
    . LazyByteString.fromStrict
    . ByteString.take 4
    . ByteString.drop offset

malformedPayloadFrame :: ByteString
malformedPayloadFrame =
  ByteString.pack [0, 0, 0, 5, 0x45, 0x52, 0x46, 0x54, 0xff]
