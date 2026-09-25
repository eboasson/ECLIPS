{-# LANGUAGE OverloadedStrings #-}

module TestFixtures
  ( domainSystem,
    domainCatalogue,
    domainConfiguration,
    domainProjection,
    domainHeraldId,
    domainHeraldEpoch,
    domainMembershipGeneration,
    coreRaftNode,
    coreRaftTerm,
    coreRequestId,
    domainDecision,
    oracleEnvelopeValue,
    system,
    catalogue,
    configuration,
    projection,
    heraldId,
    heraldEpoch,
    membershipGeneration,
    raftNode,
    leaderNode,
    requestId,
    decision,
    canonicalEnvelope,
    canonicalReceipt,
    canonicalEntry,
    canonicalEntry2,
    canonicalEntry4,
    hello,
    helloContext,
    helloAccepted,
    healthReply,
    redirect,
    committedEntries,
    clientMessages,
    serverMessages,
    clientEnvelopes,
    serverEnvelopes,
    allEnvelopes,
    generatedEnvelopes,
  )
where

import Data.Binary.Put (putWord64be, runPut)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Word (Word64)
import Eclips.Domain.Identity
  ( HeraldEpoch,
    HeraldId,
    LabelDecisionId,
    SystemId,
    controlIndex,
    mkHeraldEpoch,
    mkHeraldId,
    mkLabelDecisionId,
    mkProcessEpochId,
    mkProcessId,
    mkSystemId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGenerationId,
    mkHeraldMembershipGenerationId,
  )
import Eclips.Domain.ProcessStart (ProcessStart, processStart)
import Eclips.Domain.Startup
  ( CatalogueDigest,
    ConfigurationDigest,
    InitialProjectionDigest,
    mkCatalogueDigest,
    mkConfigurationDigest,
    mkInitialProjectionDigest,
  )
import Eclips.Oracle.Command
  ( OracleEnvelope,
    oracleEnvelope,
    startProcessEpochCommand,
  )
import Eclips.Oracle.Identity
  ( OracleClientRequestId,
    oracleClientRequestId,
  )
import Eclips.Protocol.Oracle.Codec
  ( canonicalOracleEnvelopeDtoFromValue,
    catalogueDigestClaimFromDomain,
    configurationDigestClaimFromDomain,
    heraldEpochClaimFromDomain,
    heraldIdClaimFromDomain,
    heraldMembershipGenerationClaimFromDomain,
    initialProjectionDigestClaimFromDomain,
    labelDecisionIdClaimFromDomain,
    oracleClientRequestIdDtoFromCore,
    raftNodeIdClaimFromCore,
    raftTermDtoFromCore,
    systemIdClaimFromDomain,
  )
import Eclips.Protocol.Oracle.Types
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import Eclips.Raft.Configuration
import Eclips.Raft.Identity
  ( RaftNodeId,
    RaftTerm,
    mkRaftNodeId,
    raftTerm,
  )

domainSystem :: SystemId
domainSystem = mustAdmit (mkSystemId (bytes32 1))

domainCatalogue :: CatalogueDigest
domainCatalogue = mustAdmit (mkCatalogueDigest (bytes32 2))

domainConfiguration :: ConfigurationDigest
domainConfiguration = mustAdmit (mkConfigurationDigest (bytes32 8))

domainProjection :: InitialProjectionDigest
domainProjection = mustAdmit (mkInitialProjectionDigest (bytes32 3))

domainHeraldId :: HeraldId
domainHeraldId = mustAdmit (mkHeraldId (bytes32 4))

domainHeraldEpoch :: HeraldEpoch
domainHeraldEpoch = mustAdmit (mkHeraldEpoch (bytes32 5))

domainMembershipGeneration :: HeraldMembershipGenerationId
domainMembershipGeneration =
  mustAdmit (mkHeraldMembershipGenerationId (bytes32 14))

coreRaftNode :: RaftNodeId
coreRaftNode = mustAdmit (mkRaftNodeId (bytes32 6))

coreLeaderNode :: RaftNodeId
coreLeaderNode = mustAdmit (mkRaftNodeId (bytes32 7))

coreRaftTerm :: RaftTerm
coreRaftTerm = raftTerm 3

coreRequestId :: OracleClientRequestId
coreRequestId = oracleClientRequestId domainHeraldEpoch 11

domainDecision :: LabelDecisionId
domainDecision = mustAdmit (mkLabelDecisionId (bytes32 13))

oracleEnvelopeValue :: OracleEnvelope
oracleEnvelopeValue =
  oracleEnvelope
    coreRequestId
    (Just (controlIndex 0))
    domainHeraldEpoch
    (startProcessEpochCommand startProcessBootstrap)

startProcessBootstrap :: ProcessStart
startProcessBootstrap =
  processStart
    (mustAdmit (mkProcessId (bytes32 11)))
    (mustAdmit (mkProcessEpochId (bytes32 12)))
    domainHeraldEpoch

system :: SystemIdClaim
system = systemIdClaimFromDomain domainSystem

catalogue :: CatalogueDigestClaim
catalogue = catalogueDigestClaimFromDomain domainCatalogue

configuration :: ConfigurationDigestClaim
configuration = configurationDigestClaimFromDomain domainConfiguration

projection :: InitialProjectionDigestClaim
projection = initialProjectionDigestClaimFromDomain domainProjection

heraldId :: HeraldIdClaim
heraldId = heraldIdClaimFromDomain domainHeraldId

heraldEpoch :: HeraldEpochClaim
heraldEpoch = heraldEpochClaimFromDomain domainHeraldEpoch

membershipGeneration :: HeraldMembershipGenerationClaim
membershipGeneration =
  heraldMembershipGenerationClaimFromDomain domainMembershipGeneration

raftNode :: RaftNodeIdClaim
raftNode = raftNodeIdClaimFromCore coreRaftNode

leaderNode :: RaftNodeIdClaim
leaderNode = raftNodeIdClaimFromCore coreLeaderNode

requestId :: OracleClientRequestIdDto
requestId = oracleClientRequestIdDtoFromCore coreRequestId

decision :: LabelDecisionIdClaim
decision = labelDecisionIdClaimFromDomain domainDecision

canonicalEnvelope :: CanonicalOracleEnvelopeDto
canonicalEnvelope = canonicalOracleEnvelopeDtoFromValue oracleEnvelopeValue

canonicalReceipt :: CanonicalOracleReceiptDto
canonicalReceipt = canonicalOracleReceiptDto (canonicalReceiptBytes 1)

canonicalEntry :: CanonicalAppliedOracleEntryDto
canonicalEntry = canonicalAppliedOracleEntryDto (canonicalEntryBytes 1)

canonicalEntry2 :: CanonicalAppliedOracleEntryDto
canonicalEntry2 = canonicalAppliedOracleEntryDto (canonicalEntryBytes 2)

canonicalEntry4 :: CanonicalAppliedOracleEntryDto
canonicalEntry4 = canonicalAppliedOracleEntryDto (canonicalEntryBytes 4)

hello :: OracleHelloDto
hello =
  oracleHelloDto
    system
    catalogue
    configuration
    projection
    heraldId
    heraldEpoch
    (controlIndexDto 0)
    membershipGeneration

helloContext :: OracleHelloContext
helloContext = oracleHelloContext system catalogue configuration projection

helloAccepted :: OracleHelloAcceptedDto
helloAccepted =
  oracleHelloAcceptedDto
    raftNode
    (raftTermDtoFromCore coreRaftTerm)
    (controlIndexDto 5)
    (Just leaderNode)
    True

redirect :: OracleRedirectDto
redirect = oracleRedirectDto (Just leaderNode) (raftTermDtoFromCore coreRaftTerm)

committedEntries :: CommittedOracleEntriesDto
committedEntries =
  mustAdmit
    ( committedOracleEntriesAfter
        (controlIndexDto 0)
        [canonicalEntry, canonicalEntry2]
    )

clientMessages :: [OracleClientMessage]
clientMessages =
  [ OracleHello hello,
    SubmitOracleCommand canonicalEnvelope,
    WatchOracle (controlIndexDto 4),
    GetOracleRequestResult requestId,
    OracleHealthQuery 17 hello
  ]

serverMessages :: [OracleServerMessage]
serverMessages =
  [ OracleHelloAccepted helloAccepted,
    OracleRedirect redirect,
    OracleReceipt canonicalReceipt,
    OracleRequestAbsent requestId (controlIndexDto 5),
    OracleSubmissionDeferred requestId decision (controlIndexDto 5),
    CommittedOracleEntries committedEntries,
    OracleSubmissionNotReady requestId (raftTermDtoFromCore coreRaftTerm),
    OracleProgressRetired requestId (oracleProgressDto (Lifetime.receiptRetirementPrefix (Just 7)) (controlIndexDto 11)),
    OracleProgressNotReady requestId (oracleProgressDto (Lifetime.receiptRetirementPrefix (Just 7)) (controlIndexDto 11)) (raftTermDtoFromCore coreRaftTerm),
    OracleRequestRetired requestId (OracleRequestPrefixRetiredDto (oracleRequestSequenceDto 7)),
    OracleRequestRetired requestId OracleRequestHomeRetiredDto,
    OracleHealthReply healthReply
  ]

clientEnvelopes :: [OracleProtocolEnvelope]
clientEnvelopes = fmap OracleClientEnvelope clientMessages

serverEnvelopes :: [OracleProtocolEnvelope]
serverEnvelopes = fmap OracleServerEnvelope serverMessages

allEnvelopes :: [OracleProtocolEnvelope]
allEnvelopes = clientEnvelopes <> serverEnvelopes

healthReply :: OracleHealthReplyDto
healthReply =
  oracleHealthReplyDto
    17
    raftNode
    (raftTermDto 3)
    2
    genesisRaftConfigurationRef
    (stableRaftConfiguration (mustAdmit (raftVoterSet [coreRaftNode])))

-- | Exercise every current envelope arm while varying the ordinary scalar and
-- optional fields. Canonical semantic payloads remain the already admitted
-- core fixtures; their field sensitivity belongs to the core canonical suite.
generatedEnvelopes :: Word64 -> Bool -> Bool -> [OracleProtocolEnvelope]
generatedEnvelopes counter includeLeader serviceReady =
  [ OracleClientEnvelope
      ( OracleHello
          ( oracleHelloDto
              system
              catalogue
              configuration
              projection
              heraldId
              heraldEpoch
              (controlIndexDto counter)
              membershipGeneration
          )
      ),
    OracleClientEnvelope (SubmitOracleCommand canonicalEnvelope),
    OracleClientEnvelope (WatchOracle (controlIndexDto counter)),
    OracleClientEnvelope (GetOracleRequestResult generatedRequestId),
    OracleServerEnvelope
      ( OracleHelloAccepted
          ( oracleHelloAcceptedDto
              raftNode
              (raftTermDto counter)
              (controlIndexDto counter)
              leaderHint
              serviceReady
          )
      ),
    OracleServerEnvelope (OracleRedirect (oracleRedirectDto leaderHint (raftTermDto counter))),
    OracleServerEnvelope (OracleReceipt canonicalReceipt),
    OracleServerEnvelope (OracleRequestAbsent generatedRequestId (controlIndexDto counter)),
    OracleServerEnvelope (OracleSubmissionDeferred generatedRequestId decision (controlIndexDto counter)),
    OracleServerEnvelope (CommittedOracleEntries committedEntries),
    OracleServerEnvelope (OracleSubmissionNotReady generatedRequestId (raftTermDto counter)),
    OracleClientEnvelope (OracleHealthQuery counter hello),
    OracleServerEnvelope
      ( OracleHealthReply
          ( oracleHealthReplyDto
              counter
              raftNode
              (raftTermDto counter)
              counter
              genesisRaftConfigurationRef
              (stableRaftConfiguration (mustAdmit (raftVoterSet [coreRaftNode])))
          )
      ),
    OracleServerEnvelope (OracleProgressRetired generatedRequestId (oracleProgressDto (Lifetime.receiptRetirementPrefix (Just counter)) (controlIndexDto (counter `div` 2)))),
    OracleServerEnvelope (OracleProgressNotReady generatedRequestId (oracleProgressDto (Lifetime.receiptRetirementPrefix (Just counter)) (controlIndexDto (counter `div` 2))) (raftTermDto counter)),
    OracleServerEnvelope (OracleRequestRetired generatedRequestId (OracleRequestPrefixRetiredDto (oracleRequestSequenceDto counter))),
    OracleServerEnvelope (OracleRequestRetired generatedRequestId OracleRequestHomeRetiredDto)
  ]
  where
    generatedRequestId =
      oracleClientRequestIdDto heraldEpoch (oracleRequestSequenceDto counter)
    leaderHint
      | includeLeader = Just leaderNode
      | otherwise = Nothing

-- These vectors mirror the core's domain-separated cereal transcripts. They
-- deliberately carry rejected receipts and therefore an empty projection-event
-- vector, keeping the framing fixture independent of a particular command.
-- The stored diagnostic witness uses the explicit present option.
canonicalReceiptBytes :: Word64 -> ByteString
canonicalReceiptBytes index =
  encodedBytes "ECLIPS-ORACLE-RECEIPT"
    <> canonicalEntryHeraldEpochBytes
    <> encodedWord64 index
    <> canonicalEntryCommandDigest index
    <> encodedWord64 index
    <> ByteString.pack [1, 2]
    <> encodedWord64 0
    <> encodedWord64 index

canonicalEntryBytes :: Word64 -> ByteString
canonicalEntryBytes index =
  encodedBytes "ECLIPS-APPLIED-ORACLE-ENTRY"
    <> encodedWord64 index
    <> canonicalEntryHeraldEpochBytes
    <> encodedWord64 index
    <> canonicalEntryCommandDigest index
    <> encodedBytes (canonicalReceiptBytes index)
    <> encodedBytes canonicalEmptyEventVectorBytes
    <> ByteString.singleton 1
    <> canonicalEntryStateDigest index

canonicalEmptyEventVectorBytes :: ByteString
canonicalEmptyEventVectorBytes =
  encodedBytes "ECLIPS-ORACLE-PROJECTION-EVENT-VECTOR"
    <> encodedWord64 0

canonicalEntryHeraldEpochBytes :: ByteString
canonicalEntryHeraldEpochBytes = ByteString.pack [11 .. 42]

canonicalEntryCommandDigest :: Word64 -> ByteString
canonicalEntryCommandDigest 1 =
  ByteString.pack
    [ 113,
      250,
      54,
      112,
      108,
      155,
      165,
      211,
      253,
      207,
      172,
      215,
      155,
      213,
      174,
      62,
      208,
      20,
      223,
      175,
      46,
      200,
      217,
      234,
      112,
      130,
      84,
      66,
      103,
      8,
      29,
      201
    ]
canonicalEntryCommandDigest 2 =
  ByteString.pack
    [ 66,
      179,
      114,
      114,
      162,
      100,
      181,
      232,
      74,
      54,
      2,
      193,
      1,
      143,
      42,
      145,
      19,
      235,
      240,
      3,
      194,
      77,
      70,
      164,
      237,
      22,
      75,
      57,
      241,
      153,
      40,
      150
    ]
canonicalEntryCommandDigest 4 =
  ByteString.pack
    [ 179,
      254,
      37,
      84,
      17,
      12,
      62,
      186,
      227,
      22,
      176,
      53,
      122,
      205,
      27,
      25,
      223,
      148,
      73,
      152,
      177,
      136,
      107,
      193,
      57,
      48,
      68,
      118,
      223,
      67,
      2,
      125
    ]
canonicalEntryCommandDigest index = error ("missing command-digest fixture for control index " <> show index)

canonicalEntryStateDigest :: Word64 -> ByteString
canonicalEntryStateDigest 1 =
  ByteString.pack
    [ 161,
      219,
      67,
      154,
      171,
      245,
      220,
      99,
      167,
      165,
      202,
      140,
      98,
      220,
      84,
      81,
      35,
      151,
      242,
      249,
      77,
      176,
      85,
      106,
      103,
      38,
      46,
      186,
      217,
      45,
      142,
      161
    ]
canonicalEntryStateDigest 2 =
  ByteString.pack
    [ 0,
      219,
      253,
      36,
      92,
      68,
      95,
      198,
      247,
      152,
      129,
      29,
      115,
      138,
      45,
      142,
      56,
      181,
      32,
      136,
      68,
      18,
      98,
      82,
      1,
      145,
      14,
      32,
      1,
      41,
      64,
      8
    ]
canonicalEntryStateDigest 4 =
  ByteString.pack
    [ 0,
      246,
      86,
      162,
      226,
      153,
      96,
      170,
      34,
      233,
      119,
      223,
      235,
      4,
      50,
      124,
      93,
      200,
      112,
      92,
      56,
      194,
      45,
      69,
      140,
      213,
      135,
      189,
      41,
      103,
      230,
      178
    ]
canonicalEntryStateDigest index = error ("missing state-digest fixture for control index " <> show index)

encodedBytes :: ByteString -> ByteString
encodedBytes bytes = encodedWord64 (fromIntegral (ByteString.length bytes)) <> bytes

encodedWord64 :: Word64 -> ByteString
encodedWord64 = LazyByteString.toStrict . runPut . putWord64be

bytes32 :: Word -> ByteString
bytes32 value = ByteString.replicate 32 (fromIntegral value)

mustAdmit :: (Show problem) => Either problem value -> value
mustAdmit (Left problem) = error (show problem)
mustAdmit (Right value) = value
