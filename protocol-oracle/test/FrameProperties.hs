module FrameProperties
  ( tests,
  )
where

import Data.Binary.Get (getWord32be, runGet)
import Data.Binary.Put (putWord32be, runPut)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (nub)
import Data.Word (Word32, Word8)
import Eclips.Protocol.Application.Frame qualified as Application
import Eclips.Protocol.Frame qualified as CommonFrame
import Eclips.Protocol.Oracle.Codec
  ( OraclePayloadError (..),
    encodeOracleEnvelope,
  )
import Eclips.Protocol.Oracle.Frame
import Eclips.Protocol.Oracle.Types
  ( OracleAuthorityError (..),
    OracleClientMessage (OracleHealthQuery),
    OracleHelloAcceptedDto,
    OracleProtocolEnvelope (..),
    OracleRoleError (..),
    OracleServerMessage (..),
    OracleServiceReadyLeaderContext,
    OracleShapeError (..),
    configurationDigestClaim,
    controlIndexDto,
    oracleHelloAcceptedCurrentTerm,
    oracleHelloAcceptedDto,
    oracleHelloAcceptedLeaderHint,
    oracleHelloAcceptedLocalAppliedControlIndex,
    oracleHelloAcceptedRaftNodeId,
    oracleHelloContext,
    oracleServiceReadyLeaderContext,
    raftTermDto,
    systemIdClaim,
  )
import Eclips.Protocol.Peer.Frame qualified as Peer
import Eclips.Protocol.Raft.Frame qualified as Raft
import Eclips.Protocol.Raft.Types qualified as Raft
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    testProperty,
    (===),
  )
import TestFixtures
  ( allEnvelopes,
    catalogue,
    clientEnvelopes,
    configuration,
    healthReply,
    hello,
    helloAccepted,
    helloContext,
    leaderNode,
    projection,
    raftNode,
    requestId,
    serverEnvelopes,
    system,
  )

tests :: TestTree
tests =
  testGroup
    "EORC frame"
    [ testCase "all four production magics equal independent literals and do not collide" caseLiteralMagics,
      testCase "EORC delegates the exact family envelope to the common frame" caseCommonFrame,
      testCase "header has fixed EORC magic and excludes its own length" caseHeader,
      testCase "every split point preserves one frame" caseEverySplit,
      testCase "byte-at-a-time input matches one-shot input" caseByteAtATime,
      testProperty "generated chunk schedules match one-shot decoding" propGeneratedChunkEquivalence,
      testCase "coalesced frames preserve order" caseCoalesced,
      testCase "one complete frame plus a partial frame retains exact continuation" caseExactPartialContinuation,
      testCase "valid prefixes survive a following malformed payload" caseValidPrefixBeforeFailure,
      testCase "every constructor in the wrong envelope direction fails closed" caseWrongDirection,
      testCase "composed ingress admits ordinary client and server arms" caseIngressCatalogues,
      testCase "one-shot health checks immutable run identity before reaching an owner" caseHealthIngress,
      testCase "composed ingress enforces deployment Hello and every phase arm" caseIngressContextAndPhase,
      testCase "composed ingress requires independent terminal-absence authority" caseIngressAbsenceAuthority,
      testCase "invalid declared length fails closed" caseInvalidLength,
      testCase "malformed payload fails closed" caseMalformedPayload,
      testCase "EOF distinguishes empty state from every retained strict prefix" caseEndOfInput,
      testCase "EAPP/EPRP/EORC/ERFT reject every other family" caseCompleteFamilyMatrix
    ]

caseLiteralMagics :: IO ()
caseLiteralMagics = do
  assertEqual "EAPP literal" 0x45415050 Application.applicationFrameMagic
  assertEqual "EPRP literal" 0x45505250 Peer.peerFrameMagic
  assertEqual "EORC literal" 0x454f5243 oracleFrameMagic
  assertEqual "ERFT literal" 0x45524654 Raft.raftFrameMagic
  assertEqual
    "four literal families are distinct"
    4
    (length (nub [0x45415050, 0x45505250, 0x454f5243, 0x45524654 :: Word32]))

caseCommonFrame :: IO ()
caseCommonFrame =
  assertEqual
    "common family frame"
    (CommonFrame.encodeFrame oracleFrameMagic (encodeOracleEnvelope firstClientEnvelope))
    (encodeOracleFrame firstClientEnvelope)

caseHeader :: IO ()
caseHeader = do
  assertEqual "declared body length" (fromIntegral (ByteString.length frame - 4)) (word32At 0 frame)
  assertEqual "literal family magic" 0x454f5243 (word32At 4 frame)
  where
    frame = encodeOracleFrame firstClientEnvelope

caseEverySplit :: IO ()
caseEverySplit =
  mapM_
    ( \point ->
        assertEqual
          ("split point " <> show point)
          (Right [firstClientEnvelope])
          (driveChunks OracleClientFrames [ByteString.take point frame, ByteString.drop point frame])
    )
    [0 .. ByteString.length frame]
  where
    frame = encodeOracleFrame firstClientEnvelope

caseByteAtATime :: IO ()
caseByteAtATime =
  assertEqual
    "byte-at-a-time"
    (Right [firstClientEnvelope])
    (driveChunks OracleClientFrames (fmap ByteString.singleton (ByteString.unpack frame)))
  where
    frame = encodeOracleFrame firstClientEnvelope

propGeneratedChunkEquivalence :: [Word8] -> Property
propGeneratedChunkEquivalence widths =
  conjoin (fmap assertEnvelope allEnvelopes)
  where
    assertEnvelope envelope =
      counterexample diagnostic (incremental === oneShot)
      where
        direction = envelopeDirection envelope
        frame = encodeOracleFrame envelope
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

caseCoalesced :: IO ()
caseCoalesced = do
  assertEqual
    "two client frames"
    (Right [firstClientEnvelope, secondClientEnvelope])
    (driveChunks OracleClientFrames [encodeOracleFrame firstClientEnvelope <> encodeOracleFrame secondClientEnvelope])
  assertEqual
    "two server frames"
    (Right [firstServerEnvelope, secondServerEnvelope])
    (driveChunks OracleServerFrames [encodeOracleFrame firstServerEnvelope <> encodeOracleFrame secondServerEnvelope])

caseExactPartialContinuation :: IO ()
caseExactPartialContinuation =
  case feedOracleFrame (initialOracleFrameDecoder OracleClientFrames) (firstFrame <> partialSecond) of
    OracleFrameFeedResult [actual] (NeedOracleFrameBytes combinedDecoder) -> do
      assertEqual "valid prefix" firstClientEnvelope actual
      case feedOracleFrame (initialOracleFrameDecoder OracleClientFrames) partialSecond of
        OracleFrameFeedResult [] (NeedOracleFrameBytes partialDecoder) ->
          assertEqual "exact retained partial bytes" partialDecoder combinedDecoder
        other -> assertFailure ("unexpected direct partial result: " <> show other)
    other -> assertFailure ("unexpected coalesced partial result: " <> show other)
  where
    firstFrame = encodeOracleFrame firstClientEnvelope
    partialSecond = ByteString.take 5 (encodeOracleFrame secondClientEnvelope)

caseValidPrefixBeforeFailure :: IO ()
caseValidPrefixBeforeFailure = do
  let bytes = encodeOracleFrame firstClientEnvelope <> malformedPayloadFrame
  mapM_
    ( \point ->
        assertEqual
          ("surrounding split point " <> show point)
          ([firstClientEnvelope], OracleFrameFailed (OracleFramePayloadError OraclePayloadMalformed))
          ( feedChunksUntilStop
              (initialOracleFrameDecoder OracleClientFrames)
              [ByteString.take point bytes, ByteString.drop point bytes]
          )
    )
    [0 .. ByteString.length bytes]

caseWrongDirection :: IO ()
caseWrongDirection = do
  mapM_ (assertWrongDirection OracleClientFrames) serverEnvelopes
  mapM_ (assertWrongDirection OracleServerFrames) clientEnvelopes
  where
    assertWrongDirection direction envelope =
      assertEqual
        (show envelope)
        (OracleFrameFeedResult [] (OracleFrameFailed OracleFrameWrongEnvelopeDirection))
        (feedOracleFrame (initialOracleFrameDecoder direction) (encodeOracleFrame envelope))

caseIngressCatalogues :: IO ()
caseIngressCatalogues = do
  assertEqual
    "server ingress catalogue"
    (Right (init clientEnvelopes))
    (driveIngress (oracleServerIngressContext helloContext) (fmap encodeOracleFrame (init clientEnvelopes)))
  let serverHello = envelopeAt "server HelloAccepted" 0 serverEnvelopes
      serverRedirect = envelopeAt "server redirect" 1 serverEnvelopes
      serverReceipt = envelopeAt "server receipt" 2 serverEnvelopes
      serverAbsent = envelopeAt "server absence" 3 serverEnvelopes
      serverDeferred = envelopeAt "server deferred submission" 4 serverEnvelopes
      serverEntries = envelopeAt "server committed entries" 5 serverEnvelopes
      serverNotReady = envelopeAt "server submission not ready" 6 serverEnvelopes
  helloDecoder <-
    assertIngressContinues
      "client HelloAccepted arm"
      (initialOracleIngressDecoder oracleClientIngressContext)
      [serverHello]
  mapM_
    ( \envelope ->
        assertIngressContinues
          ("client established arm: " <> show envelope)
          helloDecoder
          [envelope]
    )
    [serverReceipt, serverDeferred, serverEntries, serverNotReady]
  authorizedDecoder <-
    expectAuthorityUpdated
      (authorityContextForHello helloAccepted)
      helloDecoder
  _ <-
    assertIngressContinues
      "client authorized absence arm"
      authorizedDecoder
      [serverAbsent]
  case serverRedirect of
    OracleServerEnvelope (OracleRedirect redirect) ->
      assertEqual
        "client redirect arm"
        ( OracleIngressFeedResult
            [serverRedirect]
            (OracleIngressRedirected redirect)
        )
        ( feedOracleIngress
            (initialOracleIngressDecoder oracleClientIngressContext)
            (encodeOracleFrame serverRedirect)
        )
    _ -> assertFailure "the redirect fixture has the wrong constructor"

caseHealthIngress :: IO ()
caseHealthIngress = do
  let query = OracleClientEnvelope (OracleHealthQuery 17 hello)
      reply = OracleServerEnvelope (OracleHealthReply healthReply)
      badSystem = mustAdmit (systemIdClaim (ByteString.replicate 32 99))
  assertEqual "health query lane" (Right [query]) (driveIngress (oracleServerIngressContext helloContext) [encodeOracleFrame query])
  assertEqual "health reply lane" (Right [reply]) (driveIngress oracleClientIngressContext [encodeOracleFrame reply])
  assertIngressFailure
    "health wrong run"
    (oracleServerIngressContext (oracleHelloContext badSystem catalogue configuration projection))
    []
    (OracleIngressShapeError OracleHelloSystemMismatch)
    [query]
  assertIngressFailure
    "health does not grant commands"
    (oracleServerIngressContext helloContext)
    [query]
    (OracleIngressRoleError OracleHealthLaneClosed)
    [query, envelopeAt "query" 2 clientEnvelopes]

caseIngressContextAndPhase :: IO ()
caseIngressContextAndPhase = do
  let badSystem = mustAdmit (systemIdClaim (ByteString.replicate 32 99))
      badConfiguration = mustAdmit (configurationDigestClaim (ByteString.replicate 32 99))
      badContext = oracleServerIngressContext (oracleHelloContext badSystem catalogue configuration projection)
      badConfigurationContext =
        oracleServerIngressContext (oracleHelloContext system catalogue badConfiguration projection)
      serverContext = oracleServerIngressContext helloContext
      clientContext = oracleClientIngressContext
      clientHello = envelopeAt "client Hello" 0 clientEnvelopes
      serverHello = envelopeAt "server HelloAccepted" 0 serverEnvelopes
      serverRedirect = envelopeAt "server redirect" 1 serverEnvelopes
  assertIngressFailure
    "deployment mismatch"
    badContext
    []
    (OracleIngressShapeError OracleHelloSystemMismatch)
    [clientHello]
  assertIngressFailure
    "configuration mismatch"
    badConfigurationContext
    []
    (OracleIngressShapeError OracleHelloConfigurationMismatch)
    [clientHello]
  mapM_
    ( \envelope ->
        assertIngressFailure
          "client arm before Hello"
          serverContext
          []
          (OracleIngressRoleError OracleHelloRequired)
          [envelope]
    )
    (drop 1 (init clientEnvelopes))
  mapM_
    ( \envelope ->
        assertIngressFailure
          "server arm before HelloAccepted"
          clientContext
          []
          (OracleIngressRoleError OracleHelloRequired)
          [envelope]
    )
    (drop 2 (init serverEnvelopes))
  case serverRedirect of
    OracleServerEnvelope (OracleRedirect redirect) ->
      assertEqual
        "redirect is a terminal connection outcome"
        ( OracleIngressFeedResult
            [serverRedirect]
            (OracleIngressRedirected redirect)
        )
        ( feedOracleIngress
            (initialOracleIngressDecoder clientContext)
            (encodeOracleFrame serverRedirect)
        )
    _ -> assertFailure "the redirect fixture has the wrong constructor"
  assertIngressFailure
    "client Hello repeats"
    serverContext
    [clientHello]
    (OracleIngressRoleError OracleHelloRepeated)
    [clientHello, clientHello]
  assertIngressFailure
    "server HelloAccepted repeats"
    clientContext
    [serverHello]
    (OracleIngressRoleError OracleHelloRepeated)
    [serverHello, serverHello]

caseIngressAbsenceAuthority :: IO ()
caseIngressAbsenceAuthority = do
  let serverHello = envelopeAt "server HelloAccepted" 0 serverEnvelopes
      serverRedirect = envelopeAt "server redirect" 1 serverEnvelopes
      absent = envelopeAt "OracleRequestAbsent" 3 serverEnvelopes
      exactContext = authorityContextForHello helloAccepted
      initialDecoder = initialOracleIngressDecoder oracleClientIngressContext
      currentTerm = oracleHelloAcceptedCurrentTerm helloAccepted
      completePrefix = oracleHelloAcceptedLocalAppliedControlIndex helloAccepted
      foreignContext =
        oracleServiceReadyLeaderContext
          leaderNode
          currentTerm
          completePrefix
      staleTermContext =
        oracleServiceReadyLeaderContext
          raftNode
          (raftTermDto 99)
          completePrefix
      stalePrefixContext =
        oracleServiceReadyLeaderContext
          raftNode
          currentTerm
          (controlIndexDto 4)
      wrongReportedPrefix =
        OracleServerEnvelope
          (OracleRequestAbsent requestId (controlIndexDto 4))
      notReadyHello =
        oracleHelloAcceptedDto
          raftNode
          currentTerm
          completePrefix
          (oracleHelloAcceptedLeaderHint helloAccepted)
          False
      notReadyEnvelope =
        OracleServerEnvelope (OracleHelloAccepted notReadyHello)
      replacementHello =
        oracleHelloAcceptedDto
          leaderNode
          (raftTermDto 4)
          (controlIndexDto 6)
          Nothing
          True
      replacementHelloEnvelope =
        OracleServerEnvelope (OracleHelloAccepted replacementHello)
      replacementAbsent =
        OracleServerEnvelope
          (OracleRequestAbsent requestId (controlIndexDto 6))
  _ <-
    expectAuthorityRejected
      "authority updates are client-lane only"
      OracleIngressAuthorityRequiresClientLane
      exactContext
      (initialOracleIngressDecoder (oracleServerIngressContext helloContext))
  awaitingDecoder <-
    expectAuthorityRejected
      "authority before Hello"
      OracleIngressAuthorityRequiresAcceptedHello
      exactContext
      initialDecoder
  helloDecoder <-
    assertIngressContinues
      "accepted Hello starts without authority"
      awaitingDecoder
      [serverHello]
  assertIngressDecoderFailure
    "wire serviceReady does not authorize absence"
    helloDecoder
    []
    (OracleIngressAuthorityError OracleRequestAbsentNotAuthoritative)
    [absent]
  authorizedDecoder <-
    expectAuthorityUpdated exactContext helloDecoder
  assertNotAuthoritative
    "explicit runtime invalidation clears authority"
    (invalidateOracleClientIngressAuthority authorizedDecoder)
    absent
  _ <-
    assertIngressContinues
      "a later exact out-of-band update authorizes absence"
      authorizedDecoder
      [absent]
  assertIngressDecoderFailure
    "reported absence prefix must equal the authorized complete prefix"
    authorizedDecoder
    []
    ( OracleIngressAuthorityError
        (OracleRequestAbsentPrefixMismatch completePrefix (controlIndexDto 4))
    )
    [wrongReportedPrefix]
  foreignCleared <-
    expectAuthorityRejected
      "foreign node replaces and clears old authority"
      ( OracleIngressAuthorityContextError
          (OracleAuthorityNodeMismatch raftNode leaderNode)
      )
      foreignContext
      authorizedDecoder
  assertNotAuthoritative "foreign absence" foreignCleared absent
  staleTermCleared <-
    expectAuthorityRejected
      "term change clears old authority"
      ( OracleIngressAuthorityContextError
          (OracleAuthorityTermMismatch currentTerm (raftTermDto 99))
      )
      staleTermContext
      authorizedDecoder
  assertNotAuthoritative "stale-term absence" staleTermCleared absent
  stalePrefixCleared <-
    expectAuthorityRejected
      "prefix change clears old authority"
      ( OracleIngressAuthorityContextError
          ( OracleAuthorityCompletePrefixMismatch
              completePrefix
              (controlIndexDto 4)
          )
      )
      stalePrefixContext
      authorizedDecoder
  assertNotAuthoritative "stale-prefix absence" stalePrefixCleared absent
  notReadyDecoder <-
    assertIngressContinues
      "not-ready Hello is structurally admitted"
      initialDecoder
      [notReadyEnvelope]
  notReadyCleared <-
    expectAuthorityRejected
      "not-ready Hello cannot receive leader authority"
      (OracleIngressAuthorityContextError OracleAuthorityHelloNotServiceReady)
      exactContext
      notReadyDecoder
  assertNotAuthoritative "not-ready absence" notReadyCleared absent
  case serverRedirect of
    OracleServerEnvelope (OracleRedirect redirect) ->
      assertEqual
        "redirect terminates the authorized lane without a reusable decoder"
        ( OracleIngressFeedResult
            [serverRedirect]
            (OracleIngressRedirected redirect)
        )
        ( feedOracleIngress
            authorizedDecoder
            (encodeOracleFrame serverRedirect)
        )
    _ -> assertFailure "the redirect fixture has the wrong constructor"
  assertIngressDecoderFailure
    "re-Hello on an established stream is rejected"
    authorizedDecoder
    []
    (OracleIngressRoleError OracleHelloRepeated)
    [serverHello]
  replacementDecoder <-
    assertIngressContinues
      "fresh connection accepts replacement Hello"
      initialDecoder
      [replacementHelloEnvelope]
  replacementAuthorized <-
    expectAuthorityUpdated
      (authorityContextForHello replacementHello)
      replacementDecoder
  _ <-
    assertIngressContinues
      "fresh exact node/term/prefix authority replaces the old connection"
      replacementAuthorized
      [replacementAbsent]
  pure ()

caseInvalidLength :: IO ()
caseInvalidLength =
  assertEqual
    "length shorter than family magic"
    (OracleFrameFeedResult [] (OracleFrameFailed OracleFrameInvalidLength))
    (feedOracleFrame (initialOracleFrameDecoder OracleClientFrames) (ByteString.replicate 4 0))

caseMalformedPayload :: IO ()
caseMalformedPayload =
  assertEqual
    "invalid generic outer constructor"
    (OracleFrameFeedResult [] (OracleFrameFailed (OracleFramePayloadError OraclePayloadMalformed)))
    (feedOracleFrame (initialOracleFrameDecoder OracleClientFrames) malformedPayloadFrame)

caseEndOfInput :: IO ()
caseEndOfInput = do
  assertEqual "empty decoder" (Right ()) (finishOracleFrameDecoder (initialOracleFrameDecoder OracleClientFrames))
  mapM_ assertTruncatedPrefix [1 .. ByteString.length frame - 1]
  case feedOracleFrame (initialOracleFrameDecoder OracleClientFrames) frame of
    OracleFrameFeedResult [_] (NeedOracleFrameBytes decoder) ->
      assertEqual "complete frame leaves empty state" (Right ()) (finishOracleFrameDecoder decoder)
    other -> assertFailure ("unexpected complete-frame result: " <> show other)
  where
    frame = encodeOracleFrame firstClientEnvelope
    assertTruncatedPrefix byteCount =
      case feedOracleFrame
        (initialOracleFrameDecoder OracleClientFrames)
        (ByteString.take byteCount frame) of
        OracleFrameFeedResult [] (NeedOracleFrameBytes decoder) ->
          assertEqual
            ("truncated prefix " <> show byteCount)
            (Left OracleFrameTruncated)
            (finishOracleFrameDecoder decoder)
        other -> assertFailure ("unexpected prefix result: " <> show other)

caseCompleteFamilyMatrix :: IO ()
caseCompleteFamilyMatrix = do
  mapM_ (uncurry assertApplicationRejects) (foreignFor 0x45415050)
  mapM_ (uncurry assertPeerRejects) (foreignFor 0x45505250)
  mapM_ (uncurry assertOracleRejects) (foreignFor 0x454f5243)
  mapM_ (uncurry assertRaftRejects) (foreignFor 0x45524654)
  where
    foreignFor ownMagic = filter ((/= ownMagic) . snd) familyMagics

assertApplicationRejects :: String -> Word32 -> Assertion
assertApplicationRejects label magic =
  assertEqual
    ("EAPP rejects " <> label)
    ( Application.ApplicationFrameFeedResult
        []
        (Application.ApplicationFrameFailed Application.ApplicationFrameWrongFamily)
    )
    ( Application.feedApplicationFrame
        (Application.initialApplicationFrameDecoder Application.ApplicationClientFrames)
        (familyHeader magic)
    )

assertPeerRejects :: String -> Word32 -> Assertion
assertPeerRejects label magic =
  assertEqual
    ("EPRP rejects " <> label)
    (Peer.PeerFrameFeedResult [] (Peer.PeerFrameFailed Peer.PeerFrameWrongFamily))
    (Peer.feedPeerFrame Peer.initialPeerFrameDecoder (familyHeader magic))

assertOracleRejects :: String -> Word32 -> Assertion
assertOracleRejects label magic =
  assertEqual
    ("EORC rejects " <> label)
    (OracleFrameFeedResult [] (OracleFrameFailed OracleFrameWrongFamily))
    (feedOracleFrame (initialOracleFrameDecoder OracleClientFrames) (familyHeader magic))

assertRaftRejects :: String -> Word32 -> Assertion
assertRaftRejects label magic =
  assertEqual
    ("ERFT rejects " <> label)
    (Raft.RaftFrameFeedResult [] (Raft.RaftFrameFailed Raft.RaftFrameWrongFamily))
    (Raft.feedRaftFrame (Raft.initialRaftFrameDecoder raftContext) (familyHeader magic))

familyMagics :: [(String, Word32)]
familyMagics =
  [ ("EAPP", 0x45415050),
    ("EPRP", 0x45505250),
    ("EORC", 0x454f5243),
    ("ERFT", 0x45524654)
  ]

raftContext :: Raft.RaftConnectionContext
raftContext =
  mustAdmit
    ( Raft.raftConnectionContext
        (mustAdmit (Raft.raftGenesisDigestDto (ByteString.replicate 32 1)))
        (mustAdmit (Raft.raftNodeIdDto (ByteString.replicate 32 2)))
        (mustAdmit (Raft.raftNodeIdDto (ByteString.replicate 32 3)))
    )

familyHeader :: Word32 -> ByteString
familyHeader magic =
  LazyByteString.toStrict
    (runPut (putWord32be 4 >> putWord32be magic))

envelopeDirection :: OracleProtocolEnvelope -> OracleFrameDirection
envelopeDirection (OracleClientEnvelope _) = OracleClientFrames
envelopeDirection (OracleServerEnvelope _) = OracleServerFrames

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

driveChunks :: OracleFrameDirection -> [ByteString] -> Either OracleFrameError [OracleProtocolEnvelope]
driveChunks direction = go (initialOracleFrameDecoder direction) []
  where
    go decoder reversedEnvelopes [] =
      case finishOracleFrameDecoder decoder of
        Left frameError -> Left frameError
        Right () -> Right (reverse reversedEnvelopes)
    go decoder reversedEnvelopes (chunk : chunks) =
      case feedOracleFrame decoder chunk of
        OracleFrameFeedResult _ (OracleFrameFailed frameError) -> Left frameError
        OracleFrameFeedResult envelopes (NeedOracleFrameBytes nextDecoder) ->
          go nextDecoder (reverse envelopes <> reversedEnvelopes) chunks

driveIngress :: OracleIngressContext -> [ByteString] -> Either OracleIngressError [OracleProtocolEnvelope]
driveIngress context = go (initialOracleIngressDecoder context) []
  where
    go decoder reversedEnvelopes [] =
      case finishOracleIngressDecoder decoder of
        Left ingressError -> Left ingressError
        Right () -> Right (reverse reversedEnvelopes)
    go decoder reversedEnvelopes (chunk : chunks) =
      case feedOracleIngress decoder chunk of
        OracleIngressFeedResult _ (OracleIngressFailed ingressError) ->
          Left ingressError
        OracleIngressFeedResult envelopes (OracleIngressRedirected _) ->
          Right (reverse (reverse envelopes <> reversedEnvelopes))
        OracleIngressFeedResult envelopes (NeedOracleIngressBytes nextDecoder) ->
          go nextDecoder (reverse envelopes <> reversedEnvelopes) chunks

assertIngressFailure ::
  String ->
  OracleIngressContext ->
  [OracleProtocolEnvelope] ->
  OracleIngressError ->
  [OracleProtocolEnvelope] ->
  Assertion
assertIngressFailure label context expectedPrefix expectedError envelopes =
  assertEqual
    label
    (OracleIngressFeedResult expectedPrefix (OracleIngressFailed expectedError))
    ( feedOracleIngress
        (initialOracleIngressDecoder context)
        (mconcat (fmap encodeOracleFrame envelopes))
    )

assertIngressContinues ::
  String ->
  OracleIngressDecoder ->
  [OracleProtocolEnvelope] ->
  IO OracleIngressDecoder
assertIngressContinues label decoder envelopes =
  case feedOracleIngress decoder (mconcat (fmap encodeOracleFrame envelopes)) of
    OracleIngressFeedResult actual (NeedOracleIngressBytes nextDecoder) -> do
      assertEqual label envelopes actual
      pure nextDecoder
    OracleIngressFeedResult actual continuation -> do
      assertFailure
        ( label
            <> ": expected continuation after "
            <> show actual
            <> ", got "
            <> show continuation
        )
      pure decoder

assertIngressDecoderFailure ::
  String ->
  OracleIngressDecoder ->
  [OracleProtocolEnvelope] ->
  OracleIngressError ->
  [OracleProtocolEnvelope] ->
  Assertion
assertIngressDecoderFailure label decoder expectedPrefix expectedError envelopes =
  assertEqual
    label
    (OracleIngressFeedResult expectedPrefix (OracleIngressFailed expectedError))
    ( feedOracleIngress
        decoder
        (mconcat (fmap encodeOracleFrame envelopes))
    )

expectAuthorityUpdated ::
  OracleServiceReadyLeaderContext ->
  OracleIngressDecoder ->
  IO OracleIngressDecoder
expectAuthorityUpdated authority decoder =
  case updateOracleClientIngressAuthority authority decoder of
    OracleIngressAuthorityUpdated nextDecoder -> pure nextDecoder
    OracleIngressAuthorityUpdateRejected updateError _ -> do
      assertFailure ("expected authority update, got " <> show updateError)
      pure decoder

expectAuthorityRejected ::
  String ->
  OracleIngressAuthorityUpdateError ->
  OracleServiceReadyLeaderContext ->
  OracleIngressDecoder ->
  IO OracleIngressDecoder
expectAuthorityRejected label expected authority decoder =
  case updateOracleClientIngressAuthority authority decoder of
    OracleIngressAuthorityUpdated _ -> do
      assertFailure (label <> ": authority unexpectedly admitted")
      pure decoder
    OracleIngressAuthorityUpdateRejected actual clearedDecoder -> do
      assertEqual label expected actual
      pure clearedDecoder

assertNotAuthoritative ::
  String ->
  OracleIngressDecoder ->
  OracleProtocolEnvelope ->
  Assertion
assertNotAuthoritative label decoder absence =
  assertIngressDecoderFailure
    label
    decoder
    []
    (OracleIngressAuthorityError OracleRequestAbsentNotAuthoritative)
    [absence]

authorityContextForHello :: OracleHelloAcceptedDto -> OracleServiceReadyLeaderContext
authorityContextForHello accepted =
  oracleServiceReadyLeaderContext
    (oracleHelloAcceptedRaftNodeId accepted)
    (oracleHelloAcceptedCurrentTerm accepted)
    (oracleHelloAcceptedLocalAppliedControlIndex accepted)

feedChunksUntilStop :: OracleFrameDecoder -> [ByteString] -> ([OracleProtocolEnvelope], OracleFrameContinuation)
feedChunksUntilStop decoder = go decoder []
  where
    go current reversedEnvelopes [] =
      (reverse reversedEnvelopes, NeedOracleFrameBytes current)
    go current reversedEnvelopes (chunk : chunks) =
      case feedOracleFrame current chunk of
        OracleFrameFeedResult envelopes failed@(OracleFrameFailed _) ->
          (reverse (reverse envelopes <> reversedEnvelopes), failed)
        OracleFrameFeedResult envelopes (NeedOracleFrameBytes next) ->
          go next (reverse envelopes <> reversedEnvelopes) chunks

word32At :: Int -> ByteString -> Word32
word32At offset =
  runGet getWord32be
    . LazyByteString.fromStrict
    . ByteString.take 4
    . ByteString.drop offset

malformedPayloadFrame :: ByteString
malformedPayloadFrame =
  ByteString.pack [0, 0, 0, 5, 0x45, 0x4f, 0x52, 0x43, 0xff]

firstClientEnvelope :: OracleProtocolEnvelope
firstClientEnvelope = firstOf "client" clientEnvelopes

secondClientEnvelope :: OracleProtocolEnvelope
secondClientEnvelope = secondOf "client" clientEnvelopes

firstServerEnvelope :: OracleProtocolEnvelope
firstServerEnvelope = firstOf "server" serverEnvelopes

secondServerEnvelope :: OracleProtocolEnvelope
secondServerEnvelope = secondOf "server" serverEnvelopes

firstOf :: String -> [value] -> value
firstOf _ (value : _) = value
firstOf label [] = error ("the " <> label <> " fixture catalogue is empty")

secondOf :: String -> [value] -> value
secondOf _ (_ : value : _) = value
secondOf label _ = error ("the " <> label <> " fixture catalogue has fewer than two entries")

envelopeAt :: String -> Int -> [value] -> value
envelopeAt label index values =
  case drop index values of
    value : _ -> value
    [] -> error ("missing " <> label <> " fixture")

mustAdmit :: (Show problem) => Either problem value -> value
mustAdmit (Left problem) = error (show problem)
mustAdmit (Right value) = value
