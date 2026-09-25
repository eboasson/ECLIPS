module CodecProperties
  ( tests,
  )
where

import Control.Monad (forM_)
import Data.Binary qualified as Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either (isLeft)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word64)
import Eclips.Domain.Alignment (HeraldPublicationPrefix (EmptyHeraldPublicationPrefix))
import Eclips.Domain.Identity
  ( controlIndex,
    genesisAuthorityEpoch,
    globalObjectIdFromProcessEpochId,
    mkProcessEpochId,
    nablaIdFromGlobalObjectId,
    nablaSequence,
    publicationId,
  )
import Eclips.Domain.Label
  ( firstLabelProcessAcceptancePosition,
    homeLabelAcceptanceCut,
    initialBootstrapLabelEvidence,
    initialPublicationLabelEvidence,
    targetVoid,
  )
import Eclips.Domain.Value (LabelOwner (ProcessLabel, ZombieLabel))
import Eclips.Oracle.Label qualified as Label
import Eclips.Oracle.Progress (oracleProgress)
import Eclips.Protocol.Oracle.Codec
  ( OraclePayloadError (..),
    canonicalAppliedOracleEntryDtoFromCore,
    canonicalAppliedOracleEntryDtoFromValue,
    canonicalAppliedOracleEntryDtoToCore,
    canonicalAppliedOracleEntryDtoValue,
    canonicalOracleEnvelopeDtoFromCore,
    canonicalOracleEnvelopeDtoFromValue,
    canonicalOracleEnvelopeDtoToCore,
    canonicalOracleEnvelopeDtoValue,
    canonicalOracleReceiptDtoFromCore,
    canonicalOracleReceiptDtoFromValue,
    canonicalOracleReceiptDtoToCore,
    canonicalOracleReceiptDtoValue,
    catalogueDigestClaimFromDomain,
    catalogueDigestClaimToDomain,
    committedOracleEntriesDtoFromValues,
    committedOracleEntriesDtoValues,
    configurationDigestClaimFromDomain,
    configurationDigestClaimToDomain,
    controlIndexDtoFromDomain,
    controlIndexDtoToDomain,
    decodeOracleEnvelope,
    encodeOracleEnvelope,
    heraldEpochClaimFromDomain,
    heraldEpochClaimToDomain,
    heraldIdClaimFromDomain,
    heraldIdClaimToDomain,
    heraldMembershipGenerationClaimFromDomain,
    heraldMembershipGenerationClaimToDomain,
    initialProjectionDigestClaimFromDomain,
    initialProjectionDigestClaimToDomain,
    labelDecisionIdClaimFromDomain,
    labelDecisionIdClaimToDomain,
    oracleClientRequestIdDtoFromCore,
    oracleClientRequestIdDtoToCore,
    oracleProgressDtoFromCore,
    oracleProgressDtoToCore,
    raftNodeIdClaimFromCore,
    raftNodeIdClaimToCore,
    raftTermDtoFromCore,
    raftTermDtoToCore,
    systemIdClaimFromDomain,
    systemIdClaimToDomain,
  )
import Eclips.Protocol.Oracle.Types
  ( OracleClientMessage (SubmitOracleCommand),
    OracleProtocolEnvelope (OracleClientEnvelope, OracleServerEnvelope),
    OracleServerMessage (OracleProgressNotReady, OracleProgressRetired, OracleReceipt),
    canonicalAppliedOracleEntryDtoBytes,
    canonicalOracleEnvelopeDto,
    canonicalOracleEnvelopeDtoBytes,
    canonicalOracleReceiptDto,
    canonicalOracleReceiptDtoBytes,
    controlIndexDto,
    oracleProgressDto,
  )
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    conjoin,
    counterexample,
    testProperty,
    (===),
  )
import TestFixtures
  ( allEnvelopes,
    canonicalEntry,
    canonicalEnvelope,
    canonicalReceipt,
    committedEntries,
    coreRaftNode,
    coreRaftTerm,
    coreRequestId,
    domainCatalogue,
    domainConfiguration,
    domainDecision,
    domainHeraldEpoch,
    domainHeraldId,
    domainMembershipGeneration,
    domainProjection,
    domainSystem,
    generatedEnvelopes,
    oracleEnvelopeValue,
  )

tests :: TestTree
tests =
  testGroup
    "Oracle envelope codec"
    [ testCase "every domain, Raft, request, and index adapter round-trips" caseIdentityAdapters,
      testCase "every canonical-byte adapter round-trips through core" caseCanonicalAdapters,
      testCase "Open preserves every initial-evidence arm through the EORC carrier" caseInitialLabelEvidenceCarriers,
      testCase "every current constructor fixture round-trips" caseAllConstructorsRoundTrip,
      testProperty "generated fields round-trip every current constructor" propGeneratedRoundTrip,
      testProperty "combined progress preserves both independent components" propProgressAdapter,
      testCase "same-high-water maintenance responses carry their full progress snapshot" caseProgressCorrelation,
      testCase "malformed embedded canonical payloads reject" caseMalformedCanonicalPayload,
      testCase "trailing bytes inside one declared body reject" caseTrailingBody,
      testCase "unknown outer, client, and server constructor discriminators reject" caseUnknownConstructor,
      testCase "every strict prefix of every constructor rejects" caseTruncation,
      testCase "canonical envelope, receipt, and entry bytes differ from generic DTO bytes" caseCanonicalDistinct
    ]

propProgressAdapter :: Word64 -> Word64 -> Property
propProgressAdapter receiptsThrough labelsThrough =
  let receipts = Lifetime.receiptRetirementPrefix (Just receiptsThrough)
      progress = oracleProgress receipts (controlIndex labelsThrough)
      dto = oracleProgressDtoFromCore progress
   in conjoin
        [ oracleProgressDtoToCore dto === progress,
          Binary.encode dto === Binary.encode receipts <> Binary.encode labelsThrough
        ]

caseProgressCorrelation :: IO ()
caseProgressCorrelation = do
  let request = oracleClientRequestIdDtoFromCore coreRequestId
      receipts = Lifetime.receiptRetirementPrefix (Just 10)
      older = oracleProgressDto receipts (controlIndexDto 20)
      newer = oracleProgressDto receipts (controlIndexDto 30)
      term = raftTermDtoFromCore coreRaftTerm
      acknowledgement progress = OracleServerEnvelope (OracleProgressRetired request progress)
      notReady progress = OracleServerEnvelope (OracleProgressNotReady request progress term)
  forM_ [acknowledgement, notReady] $ \message -> do
    assertBool "label-only progress changes the wire snapshot" (encodeOracleEnvelope (message older) /= encodeOracleEnvelope (message newer))
    forM_ [older, newer] $ \progress ->
      assertEqual "the exact response snapshot survives decoding" (Right (message progress)) (decodeOracleEnvelope (encodeOracleEnvelope (message progress)))
  assertEqual "maintenance ACK keeps server tag eight" (ByteString.pack [1, 8]) (ByteString.take 2 (encodeOracleEnvelope (acknowledgement older)))
  assertEqual "maintenance NotReady keeps server tag nine" (ByteString.pack [1, 9]) (ByteString.take 2 (encodeOracleEnvelope (notReady older)))

caseIdentityAdapters :: IO ()
caseIdentityAdapters = do
  assertEqual "system" (Right domainSystem) (systemIdClaimToDomain (systemIdClaimFromDomain domainSystem))
  assertEqual "catalogue" (Right domainCatalogue) (catalogueDigestClaimToDomain (catalogueDigestClaimFromDomain domainCatalogue))
  assertEqual "configuration" (Right domainConfiguration) (configurationDigestClaimToDomain (configurationDigestClaimFromDomain domainConfiguration))
  assertEqual "initial projection" (Right domainProjection) (initialProjectionDigestClaimToDomain (initialProjectionDigestClaimFromDomain domainProjection))
  assertEqual "Herald ID" (Right domainHeraldId) (heraldIdClaimToDomain (heraldIdClaimFromDomain domainHeraldId))
  assertEqual "Herald epoch" (Right domainHeraldEpoch) (heraldEpochClaimToDomain (heraldEpochClaimFromDomain domainHeraldEpoch))
  assertEqual
    "membership generation"
    (Right domainMembershipGeneration)
    ( heraldMembershipGenerationClaimToDomain
        (heraldMembershipGenerationClaimFromDomain domainMembershipGeneration)
    )
  assertEqual "label decision" (Right domainDecision) (labelDecisionIdClaimToDomain (labelDecisionIdClaimFromDomain domainDecision))
  assertEqual "control index" (controlIndex 17) (controlIndexDtoToDomain (controlIndexDtoFromDomain (controlIndex 17)))
  assertEqual "Raft node" (Right coreRaftNode) (raftNodeIdClaimToCore (raftNodeIdClaimFromCore coreRaftNode))
  assertEqual "Raft term" coreRaftTerm (raftTermDtoToCore (raftTermDtoFromCore coreRaftTerm))
  assertEqual "request ID" (Right coreRequestId) (oracleClientRequestIdDtoToCore (oracleClientRequestIdDtoFromCore coreRequestId))

caseCanonicalAdapters :: IO ()
caseCanonicalAdapters = do
  assertEqual "envelope value" (Right oracleEnvelopeValue) (canonicalOracleEnvelopeDtoValue canonicalEnvelope)
  assertEqual "envelope value constructor" canonicalEnvelope (canonicalOracleEnvelopeDtoFromValue oracleEnvelopeValue)
  assertCanonicalCoreRoundTrip canonicalOracleEnvelopeDtoToCore canonicalOracleEnvelopeDtoFromCore canonicalEnvelope
  assertCanonicalCoreRoundTrip canonicalOracleReceiptDtoToCore canonicalOracleReceiptDtoFromCore canonicalReceipt
  assertCanonicalCoreRoundTrip canonicalAppliedOracleEntryDtoToCore canonicalAppliedOracleEntryDtoFromCore canonicalEntry
  assertEqual
    "receipt semantic value"
    (Right canonicalReceipt)
    (canonicalOracleReceiptDtoFromValue <$> canonicalOracleReceiptDtoValue canonicalReceipt)
  assertEqual
    "applied-entry semantic value"
    (Right canonicalEntry)
    (canonicalAppliedOracleEntryDtoFromValue <$> canonicalAppliedOracleEntryDtoValue canonicalEntry)
  case committedOracleEntriesDtoValues committedEntries of
    Left _ -> assertBool "committed entries must decode" False
    Right values ->
      assertEqual
        "committed semantic values"
        (Right committedEntries)
        (committedOracleEntriesDtoFromValues (controlIndex 0) (NonEmpty.toList values))

caseInitialLabelEvidenceCarriers :: IO ()
caseInitialLabelEvidenceCarriers = do
  let process = either (error . show) id (mkProcessEpochId (ByteString.replicate 32 71))
      object = globalObjectIdFromProcessEpochId process
      publication = publicationId (nablaIdFromGlobalObjectId object) genesisAuthorityEpoch domainHeraldEpoch (nablaSequence 1)
      evidenceArms =
        [ Nothing,
          Just (initialBootstrapLabelEvidence object process),
          initialPublicationLabelEvidence object (ProcessLabel process, 0) publication,
          initialPublicationLabelEvidence object (ZombieLabel process, 0) publication
        ]
      acceptance = homeLabelAcceptanceCut (firstLabelProcessAcceptancePosition process) EmptyHeraldPublicationPrefix
  forM_ evidenceArms $ \evidence -> do
    let command = Label.decideLabelCommand domainDecision process object (ProcessLabel process, 0) evidence Nothing Nothing targetVoid acceptance
        envelope = Label.oracleEnvelope coreRequestId Nothing domainHeraldEpoch command
        dto = canonicalOracleEnvelopeDtoFromValue envelope
        carrier = OracleClientEnvelope (SubmitOracleCommand dto)
    assertEqual "canonical adapter preserves exact evidence and provenance" (Right envelope) (canonicalOracleEnvelopeDtoValue dto)
    assertEqual "EORC Submit preserves the full typed decision envelope" (Right carrier) (decodeOracleEnvelope (encodeOracleEnvelope carrier))

caseMalformedCanonicalPayload :: IO ()
caseMalformedCanonicalPayload = do
  assertEqual
    "SubmitOracleCommand"
    (Left OraclePayloadMalformed)
    ( decodeOracleEnvelope
        ( encodeOracleEnvelope
            (OracleClientEnvelope (SubmitOracleCommand (canonicalOracleEnvelopeDto (ByteString.singleton 0))))
        )
    )
  assertEqual
    "OracleReceipt"
    (Left OraclePayloadMalformed)
    ( decodeOracleEnvelope
        ( encodeOracleEnvelope
            (OracleServerEnvelope (OracleReceipt (canonicalOracleReceiptDto (ByteString.singleton 0))))
        )
    )

caseAllConstructorsRoundTrip :: IO ()
caseAllConstructorsRoundTrip =
  mapM_
    (\envelope -> assertEqual (show envelope) (Right envelope) (roundTrip envelope))
    allEnvelopes

propGeneratedRoundTrip :: Word64 -> Bool -> Bool -> Property
propGeneratedRoundTrip counter includeLeader serviceReady =
  conjoin
    [ counterexample (show envelope) (roundTrip envelope === Right envelope)
    | envelope <- generatedEnvelopes counter includeLeader serviceReady
    ]

caseTrailingBody :: IO ()
caseTrailingBody =
  assertEqual
    "trailing byte"
    (Left OraclePayloadMalformed)
    (decodeOracleEnvelope (encodeOracleEnvelope firstEnvelope <> ByteString.singleton 0))

caseUnknownConstructor :: IO ()
caseUnknownConstructor = do
  assertUnknown "outer envelope" [0xff]
  assertUnknown "client message" [0, 0xff]
  assertUnknown "server message" [1, 0xff]
  where
    assertUnknown label bytes =
      assertEqual
        label
        (Left OraclePayloadMalformed)
        (decodeOracleEnvelope (ByteString.pack bytes))

caseTruncation :: IO ()
caseTruncation = mapM_ assertEnvelope allEnvelopes
  where
    assertEnvelope envelope =
      assertBool
        ("every strict prefix fails closed: " <> show envelope)
        (all (isLeft . decodeOracleEnvelope . (`ByteString.take` body)) [0 .. ByteString.length body - 1])
      where
        body = encodeOracleEnvelope envelope

caseCanonicalDistinct :: IO ()
caseCanonicalDistinct = do
  assertBool
    "generic envelope DTO"
    ( encodeOracleEnvelope (OracleClientEnvelope (SubmitOracleCommand canonicalEnvelope))
        /= canonicalOracleEnvelopeDtoBytes canonicalEnvelope
    )
  assertBool
    "generic receipt DTO"
    (genericBytes canonicalReceipt /= canonicalOracleReceiptDtoBytes canonicalReceipt)
  assertBool
    "generic applied-entry DTO"
    (genericBytes canonicalEntry /= canonicalAppliedOracleEntryDtoBytes canonicalEntry)

genericBytes :: (Binary.Binary value) => value -> ByteString.ByteString
genericBytes = LazyByteString.toStrict . Binary.encode

assertCanonicalCoreRoundTrip ::
  (Eq dto, Show dto) =>
  (dto -> Either problem core) ->
  (core -> dto) ->
  dto ->
  IO ()
assertCanonicalCoreRoundTrip toCore fromCore dto =
  case toCore dto of
    Left _ -> assertBool "canonical DTO must decode" False
    Right core -> assertEqual "canonical core round-trip" dto (fromCore core)

roundTrip :: OracleProtocolEnvelope -> Either OraclePayloadError OracleProtocolEnvelope
roundTrip = decodeOracleEnvelope . encodeOracleEnvelope

firstEnvelope :: OracleProtocolEnvelope
firstEnvelope = case allEnvelopes of
  envelope : _ -> envelope
  [] -> error "the closed Oracle-envelope fixture catalogue is empty"
