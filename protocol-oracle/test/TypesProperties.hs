{-# LANGUAGE TypeApplications #-}

module TypesProperties
  ( tests,
  )
where

import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.Binary.Put (putWord64be, putWord8, runPut)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either (isLeft)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word64)
import Eclips.Protocol.Oracle.Types
import Eclips.Raft.Configuration
import Eclips.Raft.Identity (RaftNodeId, mkRaftNodeId, raftLogIndex, raftNodeIdBytes, raftTerm)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    chooseInt,
    conjoin,
    counterexample,
    forAll,
    testProperty,
    (===),
  )
import TestFixtures

tests :: TestTree
tests =
  testGroup
    "Oracle DTO types"
    [ testProperty "every nominal claim admits exactly 32 bytes" propNominalClaimWidths,
      testCase "nominal claims render as grouped lowercase hexadecimal" caseNominalClaimRendering,
      testProperty "every nominal Binary decoder rechecks width" propNominalBinaryWidths,
      testCase "ordinary counters and indexes admit zero" caseUnsignedZero,
      testCase "request identity retains nominal epoch and sequence" caseRequestIdentity,
      testCase "canonical byte DTOs retain only their exact bytes" caseCanonicalByteDtos,
      testCase "committed entry batches require a non-empty contiguous canonical suffix" caseCommittedBatchShape,
      testCase "committed entry Binary decoding repeats every structural check" caseCommittedBatchDecoder,
      testCase "Hello context requires equal deployment claims" caseHelloContext,
      testCase "lane role admission requires one direction and first Hello" caseLaneRole,
      testCase "terminal request absence requires out-of-band authority" caseRequestAbsenceAuthority,
      testCase "the explicit catalogue covers all five client and eight server arms" caseCurrentCatalogue,
      testCase "health is a one-shot exchange and never establishes an Oracle command lane" caseHealthLane,
      testProperty "health round trips exact stable and joint effective configurations" propHealthConfiguration,
      testCase "health decoding checks canonical voter sets and native references" caseHealthMalformed,
      testCase "all checked aggregates Binary-round-trip" caseAggregateRoundTrips
    ]

propNominalClaimWidths :: Property
propNominalClaimWidths =
  forAllByteCount $ \byteCount ->
    let bytes = ByteString.replicate byteCount 0
        malformed = byteCount /= 32
     in counterexample ("byte count: " <> show byteCount)
          $ conjoin
            [ isLeft (systemIdClaim bytes) === malformed,
              isLeft (catalogueDigestClaim bytes) === malformed,
              isLeft (configurationDigestClaim bytes) === malformed,
              isLeft (initialProjectionDigestClaim bytes) === malformed,
              isLeft (heraldIdClaim bytes) === malformed,
              isLeft (heraldEpochClaim bytes) === malformed,
              isLeft (heraldMembershipGenerationClaim bytes) === malformed,
              isLeft (raftNodeIdClaim bytes) === malformed,
              isLeft (labelDecisionIdClaim bytes) === malformed
            ]

caseNominalClaimRendering :: IO ()
caseNominalClaimRendering =
  assertEqual "system claim" (Right groupedHex) (show <$> systemIdClaim bytes)
  where
    bytes = ByteString.pack [0 .. 31]
    groupedHex =
      "00010203:04050607:08090a0b:0c0d0e0f:10111213:14151617:18191a1b:1c1d1e1f"

propNominalBinaryWidths :: Property
propNominalBinaryWidths =
  forAllByteCount $ \byteCount ->
    let encoded = Binary.encode (ByteString.replicate byteCount 0)
        malformed = byteCount /= 32
     in counterexample ("encoded byte count: " <> show byteCount)
          $ conjoin
            [ isLeft (decodeBinary @SystemIdClaim encoded) === malformed,
              isLeft (decodeBinary @CatalogueDigestClaim encoded) === malformed,
              isLeft (decodeBinary @ConfigurationDigestClaim encoded) === malformed,
              isLeft (decodeBinary @InitialProjectionDigestClaim encoded) === malformed,
              isLeft (decodeBinary @HeraldIdClaim encoded) === malformed,
              isLeft (decodeBinary @HeraldEpochClaim encoded) === malformed,
              isLeft (decodeBinary @HeraldMembershipGenerationClaim encoded) === malformed,
              isLeft (decodeBinary @RaftNodeIdClaim encoded) === malformed,
              isLeft (decodeBinary @LabelDecisionIdClaim encoded) === malformed
            ]

caseUnsignedZero :: IO ()
caseUnsignedZero = do
  assertEqual "control index" 0 (controlIndexDtoWord64 (controlIndexDto 0))
  assertEqual "Raft term" 0 (raftTermDtoWord64 (raftTermDto 0))
  assertEqual "request sequence" 0 (oracleRequestSequenceDtoWord64 (oracleRequestSequenceDto 0))

caseRequestIdentity :: IO ()
caseRequestIdentity = do
  assertEqual "epoch" heraldEpoch (oracleRequestHeraldEpoch requestId)
  assertEqual "sequence" 11 (oracleRequestSequenceDtoWord64 (oracleRequestSequence requestId))

caseCanonicalByteDtos :: IO ()
caseCanonicalByteDtos = do
  let envelopeBytes = ByteString.pack [1, 2, 3, 4]
      receiptBytes = ByteString.pack [5, 6, 7, 8]
      entryBytes = ByteString.pack [9, 10, 11, 12]
  assertEqual "envelope" envelopeBytes (canonicalOracleEnvelopeDtoBytes (canonicalOracleEnvelopeDto envelopeBytes))
  assertEqual "receipt" receiptBytes (canonicalOracleReceiptDtoBytes (canonicalOracleReceiptDto receiptBytes))
  assertEqual "entry" entryBytes (canonicalAppliedOracleEntryDtoBytes (canonicalAppliedOracleEntryDto entryBytes))

caseCommittedBatchShape :: IO ()
caseCommittedBatchShape = do
  assertEqual "empty batch" (Left CommittedOracleEntriesEmpty) (committedOracleEntriesAfter (controlIndexDto 0) [])
  assertEqual
    "contiguous entries"
    [canonicalEntry, canonicalEntry2]
    (NonEmpty.toList (committedOracleEntryDtos committedEntries))
  assertEqual
    "malformed canonical entry"
    (Left CanonicalAppliedOracleEntryMalformed)
    (committedOracleEntriesAfter (controlIndexDto 0) [canonicalAppliedOracleEntryDto (ByteString.singleton 0)])
  assertEqual
    "noncontiguous canonical entries"
    (Left (CommittedOracleEntriesNotContiguous (controlIndexDto 2) (controlIndexDto 4)))
    (committedOracleEntriesAfter (controlIndexDto 0) [canonicalEntry, canonicalEntry4])
  assertEqual
    "batch does not follow supplied cursor"
    (Left (CommittedOracleEntriesDoNotFollowCursor (controlIndexDto 2) (controlIndexDto 1)))
    (committedOracleEntriesAfter (controlIndexDto 1) [canonicalEntry])

caseCommittedBatchDecoder :: IO ()
caseCommittedBatchDecoder = do
  assertBool
    "raw empty list"
    (isLeft (decodeBinary @CommittedOracleEntriesDto (Binary.encode ([] :: [CanonicalAppliedOracleEntryDto]))))
  assertBool
    "raw malformed canonical entry"
    ( isLeft
        ( decodeBinary @CommittedOracleEntriesDto
            (Binary.encode [canonicalAppliedOracleEntryDto (ByteString.singleton 0)])
        )
    )
  assertBool
    "raw noncontiguous canonical entries"
    ( isLeft
        ( decodeBinary @CommittedOracleEntriesDto
            (Binary.encode [canonicalEntry, canonicalEntry4])
        )
    )

caseHelloContext :: IO ()
caseHelloContext = do
  assertEqual "matching context" (Right ()) (validateOracleHelloContext helloContext hello)
  assertEqual
    "current membership generation"
    membershipGeneration
    (oracleHelloMembershipGeneration hello)
  assertMismatch
    OracleHelloSystemMismatch
    (oracleHelloContext (mustAdmit (systemIdClaim (ByteString.replicate 32 99))) catalogue configuration projection)
  assertMismatch
    OracleHelloCatalogueMismatch
    (oracleHelloContext system (mustAdmit (catalogueDigestClaim (ByteString.replicate 32 99))) configuration projection)
  assertMismatch
    OracleHelloConfigurationMismatch
    (oracleHelloContext system catalogue (mustAdmit (configurationDigestClaim (ByteString.replicate 32 99))) projection)
  assertMismatch
    OracleHelloInitialProjectionMismatch
    (oracleHelloContext system catalogue configuration (mustAdmit (initialProjectionDigestClaim (ByteString.replicate 32 99))))
  where
    assertMismatch expected context =
      assertEqual "mismatch" (Left expected) (validateOracleHelloContext context hello)

caseLaneRole :: IO ()
caseLaneRole = do
  mapM_
    (assertRole "server requires client Hello" OracleServerLane AwaitingOracleHello (Left OracleHelloRequired))
    (drop 1 (init clientEnvelopes))
  mapM_
    (assertRole "client requires server Hello response" OracleClientLane AwaitingOracleHello (Left OracleHelloRequired))
    (drop 2 (init serverEnvelopes))
  assertEqual "client Hello establishes server lane" (Right OracleLaneEstablished) (admitOracleEnvelopeRole OracleServerLane AwaitingOracleHello clientHello)
  assertEqual "accepted Hello establishes client lane" (Right OracleLaneEstablished) (admitOracleEnvelopeRole OracleClientLane AwaitingOracleHello serverHello)
  assertEqual "redirect leaves client awaiting a fresh Hello" (Right AwaitingOracleHello) (admitOracleEnvelopeRole OracleClientLane AwaitingOracleHello serverRedirect)
  assertEqual "repeated client Hello" (Left OracleHelloRepeated) (admitOracleEnvelopeRole OracleServerLane OracleLaneEstablished clientHello)
  assertEqual "repeated server Hello" (Left OracleHelloRepeated) (admitOracleEnvelopeRole OracleClientLane OracleLaneEstablished serverHello)
  mapM_
    (assertRole "established server admits every non-Hello client arm" OracleServerLane OracleLaneEstablished (Right OracleLaneEstablished))
    (drop 1 (init clientEnvelopes))
  mapM_
    (assertRole "established client admits every non-HelloAccepted server arm" OracleClientLane OracleLaneEstablished (Right OracleLaneEstablished))
    (drop 1 (init serverEnvelopes))
  mapM_
    (assertRole "every client arm is wrong on a client lane" OracleClientLane AwaitingOracleHello (Left OracleWrongEnvelopeDirection))
    clientEnvelopes
  mapM_
    (assertRole "every server arm is wrong on a server lane" OracleServerLane AwaitingOracleHello (Left OracleWrongEnvelopeDirection))
    serverEnvelopes
  where
    clientHello = OracleClientEnvelope (OracleHello hello)
    serverHello = OracleServerEnvelope (OracleHelloAccepted helloAccepted)
    serverRedirect = OracleServerEnvelope (OracleRedirect redirect)
    assertRole label role phase expected envelope =
      assertEqual (label <> ": " <> show envelope) expected (admitOracleEnvelopeRole role phase envelope)

caseRequestAbsenceAuthority :: IO ()
caseRequestAbsenceAuthority = do
  assertEqual
    "exact accepted Hello context"
    (Right exactAuthority)
    (admitOracleHelloAuthority helloAccepted exactContext)
  assertEqual
    "foreign node"
    (Left (OracleAuthorityNodeMismatch raftNode leaderNode))
    (admitOracleHelloAuthority helloAccepted foreignContext)
  assertEqual
    "stale term"
    ( Left
        ( OracleAuthorityTermMismatch
            (oracleHelloAcceptedCurrentTerm helloAccepted)
            (raftTermDto 99)
        )
    )
    (admitOracleHelloAuthority helloAccepted staleTermContext)
  assertEqual
    "stale prefix"
    ( Left
        ( OracleAuthorityCompletePrefixMismatch
            (controlIndexDto 5)
            (controlIndexDto 4)
        )
    )
    (admitOracleHelloAuthority helloAccepted stalePrefixContext)
  assertEqual
    "wire Hello is not service ready"
    (Left OracleAuthorityHelloNotServiceReady)
    (admitOracleHelloAuthority notReadyHello exactContext)
  assertEqual
    "wire readiness is not authority"
    (Left OracleRequestAbsentNotAuthoritative)
    (admitOracleEnvelopeAuthority noOracleRequestAbsenceAuthority absentEnvelope)
  assertEqual
    "complete service-ready leader prefix"
    (Right ())
    (admitOracleEnvelopeAuthority exactAuthority absentEnvelope)
  assertEqual
    "reported prefix must equal the authoritative complete prefix"
    (Left (OracleRequestAbsentPrefixMismatch (controlIndexDto 5) (controlIndexDto 4)))
    (admitOracleEnvelopeAuthority exactAuthority stalePrefixEnvelope)
  mapM_
    ( \envelope ->
        assertEqual
          ("non-absence needs no authority: " <> show envelope)
          (Right ())
          (admitOracleEnvelopeAuthority noOracleRequestAbsenceAuthority envelope)
    )
    (filter (/= absentEnvelope) allEnvelopes)
  where
    absentEnvelope = OracleServerEnvelope (OracleRequestAbsent requestId (controlIndexDto 5))
    stalePrefixEnvelope = OracleServerEnvelope (OracleRequestAbsent requestId (controlIndexDto 4))
    exactContext =
      oracleServiceReadyLeaderContext
        raftNode
        (oracleHelloAcceptedCurrentTerm helloAccepted)
        (controlIndexDto 5)
    exactAuthority = mustAdmit (admitOracleHelloAuthority helloAccepted exactContext)
    foreignContext =
      oracleServiceReadyLeaderContext
        leaderNode
        (oracleHelloAcceptedCurrentTerm helloAccepted)
        (controlIndexDto 5)
    staleTermContext =
      oracleServiceReadyLeaderContext
        raftNode
        (raftTermDto 99)
        (controlIndexDto 5)
    stalePrefixContext =
      oracleServiceReadyLeaderContext
        raftNode
        (oracleHelloAcceptedCurrentTerm helloAccepted)
        (controlIndexDto 4)
    notReadyHello =
      oracleHelloAcceptedDto
        raftNode
        (oracleHelloAcceptedCurrentTerm helloAccepted)
        (controlIndexDto 5)
        (oracleHelloAcceptedLeaderHint helloAccepted)
        False

caseCurrentCatalogue :: IO ()
caseCurrentCatalogue = do
  assertEqual "client count" [0, 1, 2, 3, 4] (fmap clientTag clientMessages)
  assertEqual "server count" [0, 1, 2, 3, 4, 5, 6, 8, 9, 10, 10, 7] (fmap serverTag serverMessages)
  where
    clientTag (OracleHello _) = 0 :: Int
    clientTag (SubmitOracleCommand _) = 1
    clientTag (WatchOracle _) = 2
    clientTag (GetOracleRequestResult _) = 3
    clientTag (OracleHealthQuery _ _) = 4
    serverTag (OracleHelloAccepted _) = 0 :: Int
    serverTag (OracleRedirect _) = 1
    serverTag (OracleReceipt _) = 2
    serverTag (OracleRequestAbsent _ _) = 3
    serverTag (OracleSubmissionDeferred _ _ _) = 4
    serverTag (CommittedOracleEntries _) = 5
    serverTag (OracleSubmissionNotReady _ _) = 6
    serverTag (OracleHealthReply _) = 7
    serverTag (OracleProgressRetired _ _) = 8
    serverTag (OracleProgressNotReady _ _ _) = 9
    serverTag (OracleRequestRetired _ _) = 10

caseHealthLane :: IO ()
caseHealthLane = do
  let query = OracleClientEnvelope (OracleHealthQuery 17 hello)
      reply = OracleServerEnvelope (OracleHealthReply healthReply)
  assertEqual "query before Hello" (Right OracleHealthLaneComplete) (admitOracleEnvelopeRole OracleServerLane AwaitingOracleHello query)
  assertEqual "reply before HelloAccepted" (Right OracleHealthLaneComplete) (admitOracleEnvelopeRole OracleClientLane AwaitingOracleHello reply)
  assertEqual "query cannot interrupt established lane" (Left OracleHealthLaneClosed) (admitOracleEnvelopeRole OracleServerLane OracleLaneEstablished query)
  assertEqual "reply cannot interrupt established lane" (Left OracleHealthLaneClosed) (admitOracleEnvelopeRole OracleClientLane OracleLaneEstablished reply)
  mapM_ (\message -> assertEqual "health grants no command lane" (Left OracleHealthLaneClosed) (admitOracleEnvelopeRole OracleServerLane OracleHealthLaneComplete message)) clientEnvelopes
  mapM_ (\message -> assertEqual "health grants no result lane" (Left OracleHealthLaneClosed) (admitOracleEnvelopeRole OracleClientLane OracleHealthLaneComplete message)) serverEnvelopes

propHealthConfiguration :: Word64 -> Property
propHealthConfiguration roundNumber = forAll (chooseInt (1, 5)) $ \count ->
  let old = mustAdmit (raftVoterSet (map healthNode [1 .. count]))
      new = mustAdmit (raftVoterSet (map healthNode [2 .. count + 1]))
      reference = mustAdmit (raftConfigurationEntryRef (raftLogIndex 11) (raftTerm 3))
      reply configurationValue = oracleHealthReplyDto roundNumber raftNode (raftTermDto 4) 8 reference configurationValue
   in conjoin
        [ let value = reply configurationValue
           in decodeBinary (Binary.encode value) === Right value
        | configurationValue <- [stableRaftConfiguration old, jointRaftConfiguration old new]
        ]

healthNode :: Int -> RaftNodeId
healthNode ordinal = mustAdmit (mkRaftNodeId (ByteString.replicate 32 (fromIntegral ordinal)))

caseHealthMalformed :: IO ()
caseHealthMalformed = do
  let prefix = Binary.put (17 :: Word64) >> Binary.put raftNode >> Binary.put (raftTermDto 3) >> Binary.put (2 :: Word64)
      stable nodes = putWord8 0 >> Binary.put (map raftNodeIdBytes nodes)
      malformed label suffix = assertBool label (isLeft (decodeBinary @OracleHealthReplyDto (runPut (prefix >> suffix))))
  malformed "empty stable voters" (putWord8 0 >> stable [])
  malformed "duplicate voter" (putWord8 0 >> stable [healthNode 1, healthNode 1])
  malformed "noncanonical voter order" (putWord8 0 >> stable [healthNode 2, healthNode 1])
  malformed "empty new joint set" (putWord8 0 >> putWord8 1 >> Binary.put [raftNodeIdBytes (healthNode 1)] >> Binary.put ([] :: [ByteString.ByteString]))
  malformed "configuration entry index zero" (putWord8 1 >> putWord64be 0 >> putWord64be 3 >> stable [healthNode 1])
  malformed "configuration entry term zero" (putWord8 1 >> putWord64be 2 >> putWord64be 0 >> stable [healthNode 1])
  malformed "unknown configuration discriminator" (putWord8 0 >> putWord8 2)

caseAggregateRoundTrips :: IO ()
caseAggregateRoundTrips = mapM_ assertRoundTrip allEnvelopes

forAllByteCount :: (Int -> Property) -> Property
forAllByteCount = forAll (chooseInt (0, 64))

decodeBinary :: forall value. (Binary value) => LazyByteString.ByteString -> Either String value
decodeBinary bytes =
  case Binary.decodeOrFail bytes of
    Left (_, _, message) -> Left message
    Right (residual, _, value)
      | LazyByteString.null residual -> Right value
      | otherwise -> Left "trailing bytes"

assertRoundTrip :: (Binary value, Eq value, Show value) => value -> IO ()
assertRoundTrip value = assertEqual (show value) (Right value) (decodeBinary (Binary.encode value))

mustAdmit :: (Show problem) => Either problem value -> value
mustAdmit (Left problem) = error (show problem)
mustAdmit (Right value) = value
