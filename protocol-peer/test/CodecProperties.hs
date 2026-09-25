module CodecProperties
  ( tests,
  )
where

import Data.Binary qualified as Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either (isLeft)
import Data.Set qualified as Set
import Data.Word (Word8)
import Eclips.Protocol.Peer.Codec
  ( PeerPayloadError (..),
    decodePeerEnvelope,
    encodePeerEnvelope,
  )
import Eclips.Protocol.Peer.Types
  ( AlignmentCutDto (..),
    AlignmentPlanIdDto (..),
    PeerControlDto (..),
    PeerEnvelope (..),
    PeerHeartbeatDto (..),
  )
import Eclips.Public.Types.ReceiptRetirement qualified as Receipt
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    counterexample,
    elements,
    forAll,
    testProperty,
    (===),
  )
import TestFixtures
  ( alignmentCut,
    alignmentPlanId,
    alignmentProbeMarker,
    allEnvelopes,
    directFailureProbeRequest,
    directFailureProbeResponse,
    disappearanceProbeMarker,
    heartbeatNonce,
    labelInstallation,
    terminalSourceUnionAcceptanceA,
  )

tests :: TestTree
tests =
  testGroup
    "peer envelope codec"
    [ testCase "every current constructor fixture round-trips" caseAllConstructorsRoundTrip,
      testProperty "generated current envelopes round-trip" propGeneratedRoundTrip,
      testCase "heartbeat arms have stable current-build golden bytes" caseHeartbeatGolden,
      testCase "failure and terminal controls have stable current-build golden bytes" caseStep15Golden,
      testCase "disappearance markers have stable current-build golden bytes" caseDisappearanceGolden,
      testCase "label installation reports bind their terminal coordinate in compact golden bytes" caseLabelInstallationGolden,
      testCase "sparse alignment progress is carried by control and publication envelopes" caseAlignmentProgress,
      testCase "alignment plan and cut bytes retain the attempt before their shared coordinates" caseAlignmentAttemptGolden,
      testCase "trailing bytes inside one declared body reject" caseTrailingBody,
      testCase "an unknown generic constructor discriminator rejects" caseUnknownConstructor,
      testCase "every strict prefix of a valid envelope rejects" caseTruncation
    ]

caseAllConstructorsRoundTrip :: IO ()
caseAllConstructorsRoundTrip =
  mapM_
    (\envelope -> assertEqual (show envelope) (Right envelope) (roundTrip envelope))
    allEnvelopes

propGeneratedRoundTrip :: Property
propGeneratedRoundTrip =
  forAll (elements allEnvelopes) $ \envelope ->
    counterexample (show envelope) (roundTrip envelope === Right envelope)

caseHeartbeatGolden :: IO ()
caseHeartbeatGolden = do
  assertGolden "Ping" (Ping heartbeatNonce) heartbeatPingGolden
  assertGolden "Pong" (Pong heartbeatNonce) heartbeatPongGolden
  where
    assertGolden label heartbeat golden = do
      let envelope = PeerHeartbeatEnvelope heartbeat
      assertEqual (label <> " envelope bytes") golden (encodePeerEnvelope envelope)
      assertEqual (label <> " golden decodes") (Right envelope) (decodePeerEnvelope golden)

heartbeatPingGolden :: ByteString.ByteString
heartbeatPingGolden =
  ByteString.pack [1, 0, 1, 2, 3, 4, 5, 6, 7, 8]

heartbeatPongGolden :: ByteString.ByteString
heartbeatPongGolden =
  ByteString.pack [1, 1, 1, 2, 3, 4, 5, 6, 7, 8]

caseStep15Golden :: IO ()
caseStep15Golden = do
  assertGolden
    "direct failure probe request"
    (PeerControlEnvelope mempty (DirectFailureProbeRequestedDto directFailureProbeRequest))
    directFailureProbeRequestGolden
  assertGolden
    "direct failure probe response echoes captured voter configuration"
    (PeerControlEnvelope mempty (DirectFailureProbeRespondedDto directFailureProbeResponse))
    (ByteString.pack ([2] <> replicate 9 0 <> [18]) <> ByteString.drop 11 directFailureProbeRequestGolden <> ByteString.singleton 1)
  assertGolden
    "terminal-source union acceptance"
    (PeerControlEnvelope mempty (TerminalSourceUnionAcceptedDto terminalSourceUnionAcceptanceA))
    terminalSourceUnionAcceptanceGolden
  where
    assertGolden label envelope golden = do
      assertEqual (label <> " envelope bytes") golden (encodePeerEnvelope envelope)
      assertEqual (label <> " golden decodes") (Right envelope) (decodePeerEnvelope golden)

directFailureProbeRequestGolden :: ByteString.ByteString
directFailureProbeRequestGolden =
  ByteString.pack
    ( ([2] <> replicate 9 0 <> [17])
        <> goldenSizedClaim 33
        <> [0, 0, 0, 0, 0, 0, 0, 7]
        <> goldenSizedClaim 5
        <> goldenSizedClaim 30
        <> goldenSizedClaim 50
    )

terminalSourceUnionAcceptanceGolden :: ByteString.ByteString
terminalSourceUnionAcceptanceGolden =
  ByteString.pack
    ( ([2] <> replicate 9 0 <> [23])
        <> concatMap goldenSizedClaim [30, 32]
        <> [0, 0, 0, 0, 0, 0, 0, 1]
        <> concatMap goldenSizedClaim [5, 31, 4, 35]
    )

caseDisappearanceGolden :: IO ()
caseDisappearanceGolden = do
  let publication = PeerPublicationEnvelope mempty disappearanceProbeMarker
      alignment = PeerControlEnvelope mempty (AlignmentControlDto alignmentProbeMarker)
      probe = goldenSizedClaim 37 <> counter 7
      direction = goldenSizedClaim 4 <> goldenSizedClaim 5
      publicationGolden =
        ByteString.pack
          ( ([3] <> replicate 9 0 <> [2])
              <> direction
              <> counter 3
              <> goldenSizedClaim 16
              <> probe
              <> concatMap goldenSizedClaim [38, 30, 18]
              <> direction
              <> counter 3
          )
      alignmentGolden =
        ByteString.pack
          (([2] <> replicate 9 0 <> [13, 11]) <> probe <> goldenSizedClaim 4 <> counter 1 <> counter 10)
  mapM_
    ( \(envelope, golden) -> do
        assertEqual "exact current marker bytes" golden (encodePeerEnvelope envelope)
        assertEqual "raw marker golden decodes" (Right envelope) (decodePeerEnvelope golden)
    )
    [(publication, publicationGolden), (alignment, alignmentGolden)]
  assertEqual
    "raw alignment marker with zero Open coordinate rejects"
    (Left PeerPayloadMalformed)
    ( decodePeerEnvelope
        ( ByteString.pack
            ( ([2] <> replicate 9 0 <> [13, 11])
                <> goldenSizedClaim 37
                <> counter 0
                <> goldenSizedClaim 4
                <> counter 1
                <> counter 10
            )
        )
    )
  where
    counter value = replicate 7 0 <> [value]

goldenSizedClaim :: Word8 -> [Word8]
goldenSizedClaim octet =
  [0, 0, 0, 0, 0, 0, 0, 32] <> replicate 32 octet

caseLabelInstallationGolden :: IO ()
caseLabelInstallationGolden = do
  let envelope = PeerControlEnvelope mempty (LabelInstalledDto labelInstallation)
      reportPrefix = [2] <> replicate 9 0 <> [25] <> goldenSizedClaim 51 <> goldenSizedClaim 4
      reportBytes index = ByteString.pack (reportPrefix <> replicate 7 0 <> [index] <> goldenSizedClaim 52)
      golden = reportBytes 9
  assertEqual "installation report bytes" golden (encodePeerEnvelope envelope)
  assertEqual "installation report golden decodes" (Right envelope) (decodePeerEnvelope golden)
  assertEqual "complete report envelope stays constant sized" 139 (ByteString.length golden)
  assertEqual
    "zero terminal coordinate rejects at the wire boundary"
    (Left PeerPayloadMalformed)
    (decodePeerEnvelope (reportBytes 0))

caseAlignmentProgress :: IO ()
caseAlignmentProgress = do
  let progress = either (error . show) id (Receipt.receiptRetirement (Just 4) (Set.singleton 2))
      envelopes =
        [ PeerControlEnvelope progress AlignmentDeliveryProgressDto,
          PeerControlEnvelope progress (DirectFailureProbeRequestedDto directFailureProbeRequest),
          PeerPublicationEnvelope progress disappearanceProbeMarker
        ]
      emptyProgressGolden = ByteString.pack ([2] <> replicate 9 0 <> [15])
  mapM_ (\envelope -> assertEqual "sparse receipt header survives" (Right envelope) (roundTrip envelope)) envelopes
  assertEqual "receipt-only empty progress golden" emptyProgressGolden (encodePeerEnvelope (PeerControlEnvelope mempty AlignmentDeliveryProgressDto))
  assertEqual "receipt-only golden decodes" (Right (PeerControlEnvelope mempty AlignmentDeliveryProgressDto)) (decodePeerEnvelope emptyProgressGolden)
  assertEqual
    "unsorted sparse receipt holes reject before semantic admission"
    (Left PeerPayloadMalformed)
    (decodePeerEnvelope (ByteString.pack ([2, 1] <> counter 4 <> counter 2 <> counter 3 <> counter 2 <> [15])))
  where
    counter value = replicate 7 0 <> [value]

caseTrailingBody :: IO ()
caseTrailingBody =
  mapM_
    ( \envelope ->
        assertEqual
          (show envelope)
          (Left PeerPayloadMalformed)
          (decodePeerEnvelope (encodePeerEnvelope envelope <> ByteString.singleton 0))
    )
    allEnvelopes

caseUnknownConstructor :: IO ()
caseUnknownConstructor =
  assertEqual
    "unknown outer constructor"
    (Left PeerPayloadMalformed)
    (decodePeerEnvelope (ByteString.singleton 0xff))

caseTruncation :: IO ()
caseTruncation =
  assertBool
    "every strict prefix fails closed"
    (all allStrictPrefixesReject allEnvelopes)
  where
    allStrictPrefixesReject envelope =
      let body = encodePeerEnvelope envelope
       in all (isLeft . decodePeerEnvelope . (`ByteString.take` body)) [0 .. ByteString.length body - 1]

roundTrip :: PeerEnvelope -> Either PeerPayloadError PeerEnvelope
roundTrip = decodePeerEnvelope . encodePeerEnvelope

caseAlignmentAttemptGolden :: IO ()
caseAlignmentAttemptGolden = do
  let AlignmentPlanIdDto _ _ sortId occurrence topology placement = alignmentPlanId
      AlignmentCutDto _ _ cutSort cutOccurrence cutTopology cutPlacement members predecessors fresh = alignmentCut
      planAt membershipChange attempt = AlignmentPlanIdDto membershipChange attempt sortId occurrence topology placement
      cutAt membershipChange attempt = AlignmentCutDto membershipChange attempt cutSort cutOccurrence cutTopology cutPlacement members predecessors fresh
      bytes :: (Binary.Binary value) => value -> ByteString.ByteString
      bytes = LazyByteString.toStrict . Binary.encode
      expectedAttempt = ByteString.pack [0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0, 7]
  assertEqual "plan attempt has membership change then ordinal in big-endian order" expectedAttempt (ByteString.take 16 (bytes (planAt 11 7)))
  assertEqual "cut attempt has membership change then ordinal in big-endian order" expectedAttempt (ByteString.take 16 (bytes (cutAt 11 7)))
  assertEqual "plan coordinates stay unchanged across attempt scopes" (ByteString.drop 16 (bytes (planAt 0 0))) (ByteString.drop 16 (bytes (planAt 11 7)))
  assertEqual "cut coordinates stay unchanged across attempt scopes" (ByteString.drop 16 (bytes (cutAt 0 0))) (ByteString.drop 16 (bytes (cutAt 11 7)))
  assertEqual "initial attempt also survives the DTO codec" (planAt 0 0, cutAt 0 0) (Binary.decode (Binary.encode (planAt 0 0, cutAt 0 0)))
