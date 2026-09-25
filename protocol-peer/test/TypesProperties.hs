{-# LANGUAGE TypeApplications #-}

module TypesProperties
  ( tests,
  )
where

import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either (isLeft, isRight)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Protocol.Peer.Types
import Eclips.Public.Types.ReceiptRetirement qualified as Receipt
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck
  ( Positive (..),
    Property,
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
    "peer DTO types"
    [ testProperty "every nominal claim admits exactly 32 bytes" propNominalClaimWidths,
      testCase "nominal claims render as grouped lowercase hexadecimal" caseNominalClaimRendering,
      testProperty "every nominal Binary decoder rechecks width" propNominalBinaryWidths,
      testCase "positive sequences reject zero in construction and decoding" casePositiveSequences,
      testProperty "positive sequences preserve generated values" propPositiveSequences,
      testProperty "heartbeat nonces and both closed arms preserve all unsigned values" propHeartbeatNonces,
      testCase "ordinary unsigned carriers retain zero" caseUnsignedZero,
      testCase "peer-address Binary decoding rejects malformed UTF-8" caseAddressUtf8,
      testCase "Hello and contact addresses normalize canonically" caseAddressNormalization,
      testCase "known-contact collections normalize and reject conflicts" caseKnownCollection,
      testCase "known-contact Binary decoding rechecks conflicts" caseKnownCollectionDecoder,
      testCase "placement snapshots normalize and reject duplicate keys" casePlacementNormalization,
      testCase "placement Binary decoding rechecks duplicate shapes" casePlacementDecoder,
      testCase "stream directions require distinct endpoints" caseDirection,
      testCase "gap summaries require an exact ordered transcript beyond the first missing item" caseGapSummary,
      testCase "gap Binary decoding rechecks transcript ordering" caseGapDecoder,
      testCase "resume offers check direction, gap prefix, and watermarks" caseResumeOffer,
      testCase "resume responses check watermarks and exact retransmit frontier" caseResumeResponse,
      testCase "sparse completion round-trips through ordinary and Resume controls" caseSparseCompletion,
      testCase "structural vectors normalize and reject empty or duplicate membership" caseStructuralVectors,
      testCase "physical-placement vectors normalize and reject empty or duplicate membership" casePhysicalPlacementVectors,
      testCase "all authority epoch arms Binary-round-trip in canonical order" caseAuthorityEpochs,
      testCase "all three topology predecessor forms Binary-round-trip" caseTopologyPredecessors,
      testCase "publication destinations normalize and reject empty or duplicate keys" caseDestinations,
      testCase "publication-destination Binary decoding rechecks shape" caseDestinationsDecoder,
      testCase "topology acceptances normalize and reject empty or duplicate reporters" caseTopologyAcceptances,
      testCase "direct failure-probe identities require a positive Oracle coordinate" caseFailureProbeIdentity,
      testCase "terminal-source acceptances normalize and reject empty or duplicate reporters" caseTerminalSourceAcceptances,
      testProperty "retired origins form a canonical nonempty set" propRetiredOrigins,
      testCase "retired-origin Binary decoder rejects duplicate claims" caseRetiredOriginsDecoder,
      testCase "all current wire enums Binary-round-trip" caseWireEnums,
      testCase "checked aggregate fixtures Binary-round-trip" caseAggregateRoundTrips
    ]

propNominalClaimWidths :: Property
propNominalClaimWidths =
  forAllByteCount $ \byteCount ->
    let bytes = ByteString.replicate byteCount 0
        malformed = byteCount /= 32
     in counterexample ("byte count: " <> show byteCount)
          $ conjoin
            [ isLeft (systemClaim bytes) === malformed,
              isLeft (heraldIdClaim bytes) === malformed,
              isLeft (heraldEpochClaim bytes) === malformed,
              isLeft (globalObjectClaim bytes) === malformed,
              isLeft (labelDecisionClaim bytes) === malformed,
              isLeft (labelOutcomeDigestClaim bytes) === malformed,
              isLeft (disappearanceProbeDigestClaim bytes) === malformed,
              isLeft (disappearanceSubjectDigestClaim bytes) === malformed,
              isLeft (processEpochClaim bytes) === malformed,
              isLeft (nablaClaim bytes) === malformed,
              isLeft (deltaClaim bytes) === malformed,
              isLeft (sortOccurrenceClaim bytes) === malformed,
              isLeft (storeIncarnationClaim bytes) === malformed,
              isLeft (catalogueDigestClaim bytes) === malformed,
              isLeft (initialProjectionDigestClaim bytes) === malformed,
              isLeft (peerItemDigestClaim bytes) === malformed,
              isLeft (heraldMembershipGenerationClaim bytes) === malformed,
              isLeft (oracleVoterConfigurationClaim bytes) === malformed,
              isLeft (heraldFailureProbeDigestClaim bytes) === malformed,
              isLeft (memberSetDigestClaim bytes) === malformed,
              isLeft (structuralPublicationDigestClaim bytes) === malformed,
              isLeft (topologyOccurrenceDigestClaim bytes) === malformed,
              isLeft (topologyCutClaim bytes) === malformed,
              isLeft (terminalSourceUnionDigestClaim bytes) === malformed,
              isLeft (terminalSourceInventoryDigestClaim bytes) === malformed,
              isLeft (terminalSourceAcceptanceDigestClaim bytes) === malformed,
              isLeft (contextClassGenerationClaim bytes) === malformed,
              isLeft (memberReadyEvidenceDigestClaim bytes) === malformed,
              isLeft (historicalCertificateDigestClaim bytes) === malformed,
              isLeft (alignmentSnapshotDigestClaim bytes) === malformed,
              isLeft (bootstrapEvidenceDigestClaim bytes) === malformed
            ]

caseNominalClaimRendering :: IO ()
caseNominalClaimRendering =
  assertEqual "Herald claim" (Right groupedHex) (show <$> heraldIdClaim bytes)
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
            [ isLeft (decodeBinary @SystemClaim encoded) === malformed,
              isLeft (decodeBinary @HeraldIdClaim encoded) === malformed,
              isLeft (decodeBinary @HeraldEpochClaim encoded) === malformed,
              isLeft (decodeBinary @GlobalObjectClaim encoded) === malformed,
              isLeft (decodeBinary @LabelDecisionClaim encoded) === malformed,
              isLeft (decodeBinary @LabelOutcomeDigestClaim encoded) === malformed,
              isLeft (decodeBinary @DisappearanceProbeDigestClaim encoded) === malformed,
              isLeft (decodeBinary @DisappearanceSubjectDigestClaim encoded) === malformed,
              isLeft (decodeBinary @ProcessEpochClaim encoded) === malformed,
              isLeft (decodeBinary @NablaClaim encoded) === malformed,
              isLeft (decodeBinary @DeltaClaim encoded) === malformed,
              isLeft (decodeBinary @SortOccurrenceClaim encoded) === malformed,
              isLeft (decodeBinary @StoreIncarnationClaim encoded) === malformed,
              isLeft (decodeBinary @CatalogueDigestClaim encoded) === malformed,
              isLeft (decodeBinary @InitialProjectionDigestClaim encoded) === malformed,
              isLeft (decodeBinary @PeerItemDigestClaim encoded) === malformed,
              isLeft (decodeBinary @HeraldMembershipGenerationClaim encoded) === malformed,
              isLeft (decodeBinary @OracleVoterConfigurationClaim encoded) === malformed,
              isLeft (decodeBinary @HeraldFailureProbeDigestClaim encoded) === malformed,
              isLeft (decodeBinary @MemberSetDigestClaim encoded) === malformed,
              isLeft (decodeBinary @StructuralPublicationDigestClaim encoded) === malformed,
              isLeft (decodeBinary @TopologyOccurrenceDigestClaim encoded) === malformed,
              isLeft (decodeBinary @TopologyCutClaim encoded) === malformed,
              isLeft (decodeBinary @TerminalSourceUnionDigestClaim encoded) === malformed,
              isLeft (decodeBinary @TerminalSourceInventoryDigestClaim encoded) === malformed,
              isLeft (decodeBinary @TerminalSourceAcceptanceDigestClaim encoded) === malformed,
              isLeft (decodeBinary @ContextClassGenerationClaim encoded) === malformed,
              isLeft (decodeBinary @MemberReadyEvidenceDigestClaim encoded) === malformed,
              isLeft (decodeBinary @HistoricalCertificateDigestClaim encoded) === malformed,
              isLeft (decodeBinary @AlignmentSnapshotDigestClaim encoded) === malformed,
              isLeft (decodeBinary @BootstrapEvidenceDigestClaim encoded) === malformed
            ]

casePositiveSequences :: IO ()
casePositiveSequences = do
  let installationDecision = labelInstallationDecisionId labelInstallation
      installationReporter = labelInstallationReporter labelInstallation
      installationDigest = labelInstallationOutcomeDigest labelInstallation
  assertEqual
    "label installation constructor rejects zero terminal index"
    (Left LabelInstallationControlIndexMustBePositive)
    (labelInstallationReportDto installationDecision installationReporter (controlIndexDto 0) installationDigest)
  assertBool
    "label installation decoder rejects zero terminal index"
    ( isLeft
        ( decodeBinary @LabelInstallationReportDto
            ( Binary.encode installationDecision
                <> Binary.encode installationReporter
                <> Binary.encode (controlIndexDto 0)
                <> Binary.encode installationDigest
            )
        )
    )
  let probeDigest = disappearanceProbeIdDigest disappearanceProbeId
  assertEqual
    "disappearance probe constructor"
    (Left DisappearanceProbeControlIndexMustBePositive)
    (disappearanceProbeIdDto probeDigest (controlIndexDto 0))
  assertBool
    "disappearance probe decoder rejects zero Open index"
    ( isLeft
        ( decodeBinary @DisappearanceProbeIdDto
            (Binary.encode probeDigest <> Binary.encode (controlIndexDto 0))
        )
    )
  assertEqual "placement constructor" (Left PlacementSequenceMustBePositive) (positivePlacementSequenceDto 0)
  assertEqual "stream constructor" (Left StreamSequenceMustBePositive) (positiveStreamSequenceDto 0)
  assertEqual "structural constructor" (Left StructuralSequenceMustBePositive) (positiveStructuralSequenceDto 0)
  assertEqual "obligation constructor" (Left AlignmentObligationSequenceMustBePositive) (positiveAlignmentObligationSequenceDto 0)
  assertEqual "subscription constructor" (Left AlignmentSubscriptionSequenceMustBePositive) (positiveAlignmentSubscriptionSequenceDto 0)
  assertEqual "delivery constructor" (Left AlignmentDeliverySequenceMustBePositive) (positiveAlignmentDeliverySequenceDto 0)
  assertBool "delivery decoder" (isLeft (decodeBinary @PositiveAlignmentDeliverySequenceDto (Binary.encode (0 :: Word64))))
  assertEqual "evidence constructor" (Left AlignmentEvidenceSequenceMustBePositive) (positiveAlignmentEvidenceSequenceDto 0)
  assertEqual "publication position constructor" (Left HeraldPublicationPositionMustBePositive) (positiveHeraldPublicationPositionDto 0)
  assertBool "placement decoder" (isLeft (decodeBinary @PositivePlacementSequenceDto (Binary.encode (0 :: Word64))))
  assertBool "stream decoder" (isLeft (decodeBinary @PositiveStreamSequenceDto (Binary.encode (0 :: Word64))))
  assertBool "structural decoder" (isLeft (decodeBinary @PositiveStructuralSequenceDto (Binary.encode (0 :: Word64))))
  assertBool "obligation decoder" (isLeft (decodeBinary @PositiveAlignmentObligationSequenceDto (Binary.encode (0 :: Word64))))
  assertBool "subscription decoder" (isLeft (decodeBinary @PositiveAlignmentSubscriptionSequenceDto (Binary.encode (0 :: Word64))))
  assertBool "evidence decoder" (isLeft (decodeBinary @PositiveAlignmentEvidenceSequenceDto (Binary.encode (0 :: Word64))))
  assertBool "publication position decoder" (isLeft (decodeBinary @PositiveHeraldPublicationPositionDto (Binary.encode (0 :: Word64))))

propPositiveSequences :: Positive Word64 -> Property
propPositiveSequences (Positive value) =
  case ( positivePlacementSequenceDto value,
         positiveStreamSequenceDto value,
         positiveStructuralSequenceDto value,
         positiveAlignmentObligationSequenceDto value,
         positiveAlignmentSubscriptionSequenceDto value,
         positiveAlignmentEvidenceSequenceDto value,
         positiveAlignmentDeliverySequenceDto value,
         positiveHeraldPublicationPositionDto value
       ) of
    (Right placement, Right stream, Right structural, Right obligation, Right subscription, Right evidence, Right delivery, Right publicationPosition) ->
      conjoin
        [ positivePlacementSequenceDtoWord64 placement === value,
          positiveStreamSequenceDtoWord64 stream === value,
          positiveStructuralSequenceDtoWord64 structural === value,
          positiveAlignmentObligationSequenceDtoWord64 obligation === value,
          positiveAlignmentSubscriptionSequenceDtoWord64 subscription === value,
          positiveAlignmentEvidenceSequenceDtoWord64 evidence === value,
          positiveAlignmentDeliverySequenceDtoWord64 delivery === value,
          positiveHeraldPublicationPositionDtoWord64 publicationPosition === value,
          binaryRoundTrip placement,
          binaryRoundTrip stream,
          binaryRoundTrip structural,
          binaryRoundTrip obligation,
          binaryRoundTrip subscription,
          binaryRoundTrip evidence,
          binaryRoundTrip delivery,
          binaryRoundTrip publicationPosition
        ]
    malformed -> counterexample (show malformed) False

propHeartbeatNonces :: Word64 -> Property
propHeartbeatNonces value =
  conjoin
    [ peerHeartbeatNonceWord64 nonce === value,
      binaryRoundTrip nonce,
      binaryRoundTrip (Ping nonce),
      binaryRoundTrip (Pong nonce)
    ]
  where
    nonce = peerHeartbeatNonce value

caseUnsignedZero :: IO ()
caseUnsignedZero = do
  assertEqual "connection nonce" 0 (connectionNonceDtoWord64 (connectionNonceDto 0))
  assertEqual "heartbeat nonce" 0 (peerHeartbeatNonceWord64 (peerHeartbeatNonce 0))
  assertEqual "control index" 0 (controlIndexDtoWord64 (controlIndexDto 0))
  assertEqual "genesis authority" GenesisAuthorityEpochDto GenesisAuthorityEpochDto
  assertEqual "nabla sequence" 0 (nablaSequenceDtoWord64 (nablaSequenceDto 0))
  assertEqual "store revision" 0 (storeRevisionDtoWord64 (storeRevisionDto 0))

caseAuthorityEpochs :: IO ()
caseAuthorityEpochs = do
  mapM_ assertRoundTrip authorityEpochDtos
  case authorityEpochDtos of
    [ genesis@GenesisAuthorityEpochDto,
      structural@(StructuralAuthorityEpochDto _ _),
      label@(LabelAuthorityEpochDto _)
      ] -> do
        assertBool "genesis before structural" (genesis < structural)
        assertBool "structural before label" (structural < label)
    other -> assertFailure ("authority fixture inventory: " <> show other)

caseAddressUtf8 :: IO ()
caseAddressUtf8 =
  assertBool
    "invalid one-byte UTF-8 sequence"
    ( isLeft
        ( decodeBinary @PeerAddressDto
            (Binary.encode (ByteString.singleton 0xff))
        )
    )

caseAddressNormalization :: IO ()
caseAddressNormalization = do
  assertEqual "Hello addresses" [addressA, addressB] (peerHelloAdvertisedAddresses hello)
  assertEqual "Hello membership generation" membershipGeneration (peerHelloMembershipGeneration hello)
  assertEqual "Hello active-member digest" memberSetDigest (peerHelloActiveMemberSetDigest hello)
  assertEqual "known addresses" [addressA, addressB] (knownHeraldAddresses knownA)

caseKnownCollection :: IO ()
caseKnownCollection = do
  exactDuplicates <- mustAdmitIO (knownHeraldCollectionDto [knownB, knownA, knownA])
  assertEqual "canonical key order and exact collapse" [knownA, knownB] (knownHeraldCollectionEntries exactDuplicates)
  let conflicting = knownHeraldDto heraldIdA heraldEpochA [addressB]
  assertEqual
    "conflicting same identity/epoch"
    (Left (KnownHeraldConflictingDuplicate heraldIdA heraldEpochA))
    (knownHeraldCollectionDto [knownA, conflicting])

caseKnownCollectionDecoder :: IO ()
caseKnownCollectionDecoder = do
  let conflicting = knownHeraldDto heraldIdA heraldEpochA [addressB]
  assertBool
    "raw conflicting collection"
    (isLeft (decodeBinary @KnownHeraldCollectionDto (Binary.encode [knownA, conflicting])))

casePlacementNormalization :: IO ()
casePlacementNormalization = do
  assertEqual "snapshot route order" [applicationRoute, privateRoute] (placementSnapshotRoutes snapshot)
  assertEqual
    "snapshot duplicate"
    (Left (PlacementSnapshotDuplicateDelta deltaA))
    (placementSnapshotDto heraldEpochA placementSequence1 [applicationRoute, applicationRoute])

casePlacementDecoder :: IO ()
casePlacementDecoder = do
  assertBool
    "raw duplicate snapshot"
    ( isLeft
        ( decodeBinary @PlacementSnapshotDto
            ( Binary.encode heraldEpochA
                <> Binary.encode placementSequence1
                <> Binary.encode [applicationRoute, applicationRoute]
            )
        )
    )

caseDirection :: IO ()
caseDirection = do
  assertEqual
    "equal endpoints"
    (Left (StreamDirectionEndpointsEqual heraldEpochA))
    (streamDirectionDto heraldEpochA heraldEpochA)
  assertBool
    "decoder repeats check"
    ( isLeft
        ( decodeBinary @StreamDirectionDto
            (Binary.encode heraldEpochA <> Binary.encode heraldEpochA)
        )
    )

caseGapSummary :: IO ()
caseGapSummary = do
  assertEqual
    "descending entries"
    (Left GapEntriesNotStrictlyAscending)
    (gapSummaryDto directionBA EmptyStreamPrefixDto [(streamSequence3, itemDigest), (streamSequence2, itemDigest)])
  assertEqual
    "duplicate entries"
    (Left GapEntriesNotStrictlyAscending)
    (gapSummaryDto directionBA EmptyStreamPrefixDto [(streamSequence2, itemDigest), (streamSequence2, itemDigest)])
  assertEqual
    "the first missing sequence is not a gap"
    (Left (GapEntryNotBeyondFirstMissing streamSequence2))
    ( gapSummaryDto
        directionBA
        (StreamPrefixThroughDto streamSequence1)
        [(streamSequence2, itemDigest)]
    )
  assertBool
    "a later ordered gap is admitted"
    ( isRight
        ( gapSummaryDto
            directionBA
            (StreamPrefixThroughDto streamSequence1)
            [(streamSequence3, itemDigest)]
        )
    )

caseGapDecoder :: IO ()
caseGapDecoder =
  assertBool
    "raw descending gap"
    ( isLeft
        ( decodeBinary @GapSummaryDto
            ( Binary.encode directionBA
                <> Binary.encode EmptyStreamPrefixDto
                <> Binary.encode [(streamSequence3, itemDigest), (streamSequence2, itemDigest)]
            )
        )
    )

caseResumeOffer :: IO ()
caseResumeOffer = do
  let received = StreamPrefixThroughDto streamSequence1
      completedBeyond = StreamPrefixThroughDto streamSequence2
      wrongPrefixGap = mustAdmit (gapSummaryDto directionBA EmptyStreamPrefixDto [])
      wrongDirectionGap = mustAdmit (gapSummaryDto directionAB received [])
  assertEqual
    "equal endpoints"
    (Left (StreamDirectionEndpointsEqual heraldEpochA))
    (resumeOfferDto heraldEpochA heraldEpochA streamSequence3 received EmptyStreamPrefixDto gap)
  assertEqual
    "reverse gap direction"
    (Left (ResumeGapDirectionMismatch directionBA directionAB))
    (resumeOfferDto heraldEpochA heraldEpochB streamSequence3 received EmptyStreamPrefixDto wrongDirectionGap)
  assertEqual
    "gap prefix"
    (Left (ResumeGapPrefixMismatch received EmptyStreamPrefixDto))
    (resumeOfferDto heraldEpochA heraldEpochB streamSequence3 received EmptyStreamPrefixDto wrongPrefixGap)
  assertEqual
    "completion beyond received"
    (Left (ResumeCompletedBeyondReceived completedBeyond received))
    (resumeOfferDto heraldEpochA heraldEpochB streamSequence3 received completedBeyond gap)

caseResumeResponse :: IO ()
caseResumeResponse = do
  let received = StreamPrefixThroughDto streamSequence1
      completedBeyond = StreamPrefixThroughDto streamSequence2
  assertEqual
    "completion beyond received"
    (Left (ResumeCompletedBeyondReceived completedBeyond received))
    (resumeResponseDto received completedBeyond streamSequence2)
  assertEqual
    "wrong retransmit frontier"
    (Left (ResumeRetransmitFromMismatch streamSequence2 streamSequence3))
    (resumeResponseDto received EmptyStreamPrefixDto streamSequence3)
  assertBool
    "decoder repeats response check"
    ( isLeft
        ( decodeBinary @ResumeResponseDto
            ( Binary.encode received
                <> Binary.encode EmptyStreamPrefixDto
                <> Binary.encode streamSequence3
            )
        )
    )

caseSparseCompletion :: IO ()
caseSparseCompletion = do
  let progress = mustAdmit (Receipt.receiptRetirement (Just 2) (Set.singleton 1))
      received = StreamPrefixThroughDto streamSequence2
      gaps = mustAdmit (gapSummaryDto directionBA received [])
      offer = mustAdmit (resumeOfferWithCompletionDto heraldEpochA heraldEpochB streamSequence3 received progress gaps)
      response = mustAdmit (resumeResponseWithCompletionDto received progress streamSequence3)
      controls = [StreamCompletedDto directionAB progress, StreamResumeOfferedDto offer, StreamResumeAcceptedDto response]
  assertEqual "pending exception survives offer admission" progress (resumeOfferCompletion offer)
  assertEqual "pending exception survives response admission" progress (resumeResponseCompletion response)
  assertEqual "barrier prefix stays behind its pending head" EmptyStreamPrefixDto (resumeOfferCompletedPrefix offer)
  mapM_ (\control -> assertEqual "sparse peer control canonical roundtrip" (Right control) (decodeBinary (Binary.encode control))) controls
  assertEqual
    "peer progress cannot certify sequence zero"
    (Left InvalidStreamCompletionProgress)
    (resumeResponseWithCompletionDto received (Receipt.receiptRetirementPrefix (Just 0)) streamSequence3)

caseStructuralVectors :: IO ()
caseStructuralVectors = do
  let entryA =
        StructuralVectorEntryDto
          heraldEpochA
          (StructuralPrefixThroughDto structuralSequence1)
      entryB = StructuralVectorEntryDto heraldEpochB EmptyStructuralPrefixDto
  assertEqual
    "canonical Herald order"
    [entryA, entryB]
    (NonEmpty.toList (structuralVersionVectorEntries structuralVector))
  assertEqual
    "generation coordinate"
    membershipGeneration
    (structuralVersionVectorMembershipGeneration structuralVector)
  assertEqual
    "member-set coordinate"
    memberSetDigest
    (structuralVersionVectorMemberSetDigest structuralVector)
  assertEqual
    "empty"
    (Left StructuralVectorEmpty)
    (structuralVersionVectorDto membershipGeneration memberSetDigest [])
  assertEqual
    "duplicate Herald"
    (Left (StructuralVectorDuplicateHerald heraldEpochA))
    ( structuralVersionVectorDto
        membershipGeneration
        memberSetDigest
        [entryA, entryA]
    )
  assertBool
    "decoder repeats duplicate check"
    ( isLeft
        ( decodeBinary @StructuralVersionVectorDto
            ( Binary.encode membershipGeneration
                <> Binary.encode memberSetDigest
                <> Binary.encode (NonEmpty.fromList [entryA, entryA])
            )
        )
    )

casePhysicalPlacementVectors :: IO ()
casePhysicalPlacementVectors = do
  let entryA = PhysicalPlacementRevisionEntryDto heraldEpochA placementSequence1
      entryB = PhysicalPlacementRevisionEntryDto heraldEpochB placementSequence1
  checked <-
    mustAdmitIO
      ( physicalPlacementRevisionVectorDto
          membershipGeneration
          memberSetDigest
          [entryB, entryA]
      )
  assertEqual
    "canonical Herald order"
    [entryA, entryB]
    (NonEmpty.toList (physicalPlacementRevisionVectorEntries checked))
  assertEqual
    "generation coordinate"
    membershipGeneration
    (physicalPlacementRevisionVectorMembershipGeneration checked)
  assertEqual
    "member-set coordinate"
    memberSetDigest
    (physicalPlacementRevisionVectorMemberSetDigest checked)
  assertEqual
    "empty"
    (Left PhysicalPlacementRevisionVectorEmpty)
    (physicalPlacementRevisionVectorDto membershipGeneration memberSetDigest [])
  assertEqual
    "duplicate Herald"
    (Left (PhysicalPlacementRevisionVectorDuplicateHerald heraldEpochA))
    ( physicalPlacementRevisionVectorDto
        membershipGeneration
        memberSetDigest
        [entryA, entryA]
    )
  assertBool
    "decoder repeats duplicate check"
    ( isLeft
        ( decodeBinary @PhysicalPlacementRevisionVectorDto
            ( Binary.encode membershipGeneration
                <> Binary.encode memberSetDigest
                <> Binary.encode (NonEmpty.fromList [entryA, entryA])
            )
        )
    )

caseTopologyPredecessors :: IO ()
caseTopologyPredecessors = do
  successorGeneration <-
    mustAdmitIO
      (heraldMembershipGenerationClaim (ByteString.replicate 32 32))
  successorVector <-
    mustAdmitIO
      ( structuralVersionVectorDto
          successorGeneration
          memberSetDigest
          (NonEmpty.toList (structuralVersionVectorEntries structuralVector))
      )
  let sameGeneration = SameGenerationPredecessorDto topologyCut
      membershipSuccessor =
        MembershipSuccessorPredecessorDto
          topologyCut
          membershipGeneration
          successorGeneration
          structuralVector
          successorVector
          terminalSourceUnionDigest
  assertRoundTrip sameGeneration
  assertRoundTrip membershipSuccessor
  assertRoundTrip admissionTopologyPredecessor

caseDestinations :: IO ()
caseDestinations = do
  let destinationA = PublicationDestinationDto deltaA incarnationA NormalReplicaDto
      destinationB = PublicationDestinationDto deltaB incarnationB WeakReplicaDto
  assertEqual
    "canonical delta order"
    [destinationA, destinationB]
    (NonEmpty.toList (publicationDestinationEntries destinations))
  assertEqual "empty" (Left PublicationDestinationsEmpty) (publicationDestinationsDto [])
  assertEqual
    "duplicate delta"
    (Left (PublicationDestinationDuplicateDelta deltaA))
    (publicationDestinationsDto [destinationA, destinationA])
  assertEqual
    "conflicting delta"
    (Left (PublicationDestinationDuplicateDelta deltaA))
    ( publicationDestinationsDto
        [destinationA, PublicationDestinationDto deltaA incarnationB WeakReplicaDto]
    )

caseDestinationsDecoder :: IO ()
caseDestinationsDecoder = do
  let destinationA = PublicationDestinationDto deltaA incarnationA NormalReplicaDto
  assertBool
    "raw empty destinations"
    (isLeft (decodeBinary @PublicationDestinationsDto (Binary.encode ([] :: [PublicationDestinationDto]))))
  assertBool
    "raw duplicate destinations"
    (isLeft (decodeBinary @PublicationDestinationsDto (Binary.encode [destinationA, destinationA])))

caseTopologyAcceptances :: IO ()
caseTopologyAcceptances = do
  let acceptanceA =
        TopologyCutAcceptanceDto
          topologyCut
          heraldEpochA
          structuralVector
          (controlIndexDto 0)
      acceptanceB =
        TopologyCutAcceptanceDto
          topologyCut
          heraldEpochB
          structuralVector
          (controlIndexDto 0)
  checked <- mustAdmitIO (topologyCutAcceptancesDto [acceptanceB, acceptanceA])
  assertEqual
    "canonical reporter order"
    [acceptanceA, acceptanceB]
    (NonEmpty.toList (topologyCutAcceptanceEntries checked))
  assertEqual
    "empty"
    (Left TopologyCutAcceptancesEmpty)
    (topologyCutAcceptancesDto [])
  assertEqual
    "duplicate reporter"
    (Left (TopologyCutAcceptanceDuplicateReporter heraldEpochA))
    (topologyCutAcceptancesDto [acceptanceA, acceptanceA])

caseFailureProbeIdentity :: IO ()
caseFailureProbeIdentity = do
  assertEqual
    "zero control index"
    (Left FailureProbeControlIndexMustBePositive)
    (heraldFailureProbeIdDto failureProbeDigest (controlIndexDto 0))
  assertEqual "digest" failureProbeDigest (heraldFailureProbeIdDigest failureProbeId)
  assertEqual "control index" (controlIndexDto 7) (heraldFailureProbeIdControlIndex failureProbeId)
  assertBool
    "decoder rechecks positive control index"
    ( isLeft
        ( decodeBinary @HeraldFailureProbeIdDto
            (Binary.encode (failureProbeDigest, controlIndexDto 0))
        )
    )

caseTerminalSourceAcceptances :: IO ()
caseTerminalSourceAcceptances = do
  checked <-
    mustAdmitIO
      ( terminalSourceUnionAcceptancesDto
          [terminalSourceUnionAcceptanceC, terminalSourceUnionAcceptanceA]
      )
  assertEqual
    "canonical reporter order"
    [terminalSourceUnionAcceptanceA, terminalSourceUnionAcceptanceC]
    (NonEmpty.toList (terminalSourceUnionAcceptanceEntries checked))
  assertEqual
    "empty"
    (Left TerminalSourceUnionAcceptancesEmpty)
    (terminalSourceUnionAcceptancesDto [])
  assertEqual
    "duplicate reporter"
    (Left (TerminalSourceUnionAcceptanceDuplicateReporter heraldEpochA))
    ( terminalSourceUnionAcceptancesDto
        [terminalSourceUnionAcceptanceA, terminalSourceUnionAcceptanceA]
    )
  assertBool
    "decoder rechecks duplicate reporters"
    ( isLeft
        ( decodeBinary @TerminalSourceUnionAcceptancesDto
            ( Binary.encode
                (NonEmpty.fromList [terminalSourceUnionAcceptanceA, terminalSourceUnionAcceptanceA])
            )
        )
    )

caseWireEnums :: IO ()
caseWireEnums = do
  mapM_ assertRoundTrip ([minBound .. maxBound] :: [PredefinedSortRoleDto])
  mapM_ assertRoundTrip ([minBound .. maxBound] :: [ReplicaStrengthDto])
  mapM_ assertRoundTrip ([minBound .. maxBound] :: [StructuralCarrierRoleDto])
  mapM_ assertRoundTrip ([minBound .. maxBound] :: [AlignmentCancelReasonDto])
  mapM_ assertRoundTrip ([minBound .. maxBound] :: [DirectFailureProbeResultDto])

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

binaryRoundTrip :: (Binary value, Eq value, Show value) => value -> Property
binaryRoundTrip value =
  counterexample (show value) (decodeBinary (Binary.encode value) === Right value)

assertRoundTrip :: (Binary value, Eq value, Show value) => value -> IO ()
assertRoundTrip value = assertEqual (show value) (Right value) (decodeBinary (Binary.encode value))

mustAdmitIO :: (Show error) => Either error value -> IO value
mustAdmitIO = either (fail . show) pure

propRetiredOrigins :: Property
propRetiredOrigins =
  conjoin
    [ retiredHeraldsDto [heraldEpochB, heraldEpochA] === retiredHeraldsDto [heraldEpochA, heraldEpochB],
      retiredHeraldsDto [] === Left RetiredHeraldsEmpty,
      retiredHeraldsDto [heraldEpochA, heraldEpochA] === Left (RetiredHeraldDuplicate heraldEpochA),
      fmap (NonEmpty.toList . retiredHeraldEntries) (retiredHeraldsDto [heraldEpochB, heraldEpochA]) === Right [heraldEpochA, heraldEpochB]
    ]

caseRetiredOriginsDecoder :: IO ()
caseRetiredOriginsDecoder = do
  let encoded = Binary.encode (heraldEpochA NonEmpty.:| [heraldEpochA])
  assertBool "duplicate origins cannot enter checked decoded union coordinates" (isLeft (Binary.decodeOrFail @RetiredHeraldsDto encoded))
