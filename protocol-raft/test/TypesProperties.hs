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
import Data.Word (Word8)
import Eclips.Protocol.Raft.Types
import Eclips.Raft.Identity qualified as CoreIdentity
import Eclips.Raft.Input qualified as CoreInput
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck
  ( Property,
    conjoin,
    testProperty,
    (===),
  )
import TestFixtures

tests :: TestTree
tests =
  testGroup
    "Raft DTO types and adapters"
    [ testProperty "exact-width claims admit only 32 bytes" propClaimWidths,
      testCase "exact-width claims render as grouped lowercase hexadecimal" caseClaimRendering,
      testCase "claim Binary decoding repeats exact-width checks" caseClaimBinaryWidths,
      testCase "unknown nested log-entry payload tags reject" caseUnknownPayloadTag,
      testCase "hello and connection contexts require distinct endpoints" caseHelloShape,
      testCase "configuration lists require canonical nonempty sets" caseConfigurationShape,
      testCase "configuration Binary decoding repeats canonical set checks" caseConfigurationDecoder,
      testProperty "configuration adapters preserve exact opaque metadata" propConfigurationAdapter,
      testCase "configuration succession is checked before native adaptation" caseConfigurationSequence,
      testCase "vote DTOs mirror every checked core shape invariant" caseVoteShape,
      testCase "vote Binary decoding repeats every shape check" caseVoteShapeDecoder,
      testCase "transmitted log entries exclude sentinel indexes and terms" caseEntryShape,
      testCase "AppendEntries requires strict ascending entry indexes" caseAppendOrdering,
      testCase "AppendEntries Binary decoding rechecks ordering" caseAppendOrderingDecoder,
      testCase "AppendEntries mirrors every checked core shape invariant" caseAppendShape,
      testCase "AppendEntries Binary decoding repeats every shape check" caseAppendShapeDecoder,
      testCase "append response hints and match indexes are coherent" caseConflictShape,
      testCase "append-response Binary decoding rechecks optional-hint shape" caseConflictDecoder,
      testCase "checkpoint shape and snapshot Binary decoding preserve native frontiers" caseCheckpointShape,
      testCase "the fixture catalogue exhausts the current RPC kinds" caseRpcCatalogue,
      testCase "hello and RPC admission enforce physical context and phase" caseRoleAdmission,
      testCase "all source roles are checked against the established peer" caseEveryRpcSourceRole,
      testCase "core identity adapters preserve nominal values" caseCoreIdentityAdapters,
      testCase "every valid DTO round-trips through checked core RPCs" caseCoreRpcAdapters,
      testCase "request and response adapters attach the observed role" caseCoreInputAdapters
    ]

propClaimWidths :: [Word8] -> Property
propClaimWidths bytes =
  let strict = ByteString.pack bytes
      admitted = ByteString.length strict == 32
   in conjoin
        [ isRight (raftGenesisDigestDto strict) === admitted,
          isRight (raftNodeIdDto strict) === admitted
        ]

caseClaimRendering :: IO ()
caseClaimRendering =
  assertEqual "Raft node claim" (Right groupedHex) (show <$> raftNodeIdDto bytes)
  where
    bytes = ByteString.pack [0 .. 31]
    groupedHex =
      "00010203:04050607:08090a0b:0c0d0e0f:10111213:14151617:18191a1b:1c1d1e1f"

caseClaimBinaryWidths :: IO ()
caseClaimBinaryWidths = do
  let encoded = Binary.encode (ByteString.replicate 31 0)
  assertBool
    "configuration digest"
    (isLeft (decodeBinary @RaftGenesisDigestDto encoded))
  assertBool "node ID" (isLeft (decodeBinary @RaftNodeIdDto encoded))

caseUnknownPayloadTag :: IO ()
caseUnknownPayloadTag =
  assertBool
    "closed log-entry payload"
    (isLeft (decodeBinary @RaftLogEntryPayloadDto (Binary.encode (0xff :: Word8))))

caseHelloShape :: IO ()
caseHelloShape = do
  assertEqual
    "hello self endpoint"
    (Left (RaftHelloEndpointsEqual localNode))
    (raftHelloClaim genesisDigest localNode localNode)
  assertEqual
    "context self endpoint"
    (Left (RaftHelloEndpointsEqual localNode))
    (raftConnectionContext genesisDigest localNode localNode)
  assertBool
    "raw self hello"
    ( isLeft
        ( decodeBinary @RaftHelloClaim
            ( Binary.encode genesisDigest
                <> Binary.encode localNode
                <> Binary.encode localNode
            )
        )
    )

caseConfigurationShape :: IO ()
caseConfigurationShape = do
  assertEqual "empty stable" (Left RaftVotingConfigurationEmpty) (stableRaftConfigurationDto [])
  assertEqual "empty old" (Left RaftVotingConfigurationEmpty) (jointRaftConfigurationDto [] [localNode])
  assertEqual "empty new" (Left RaftVotingConfigurationEmpty) (jointRaftConfigurationDto [localNode] [])
  assertEqual "duplicate" (Left (RaftVotersNotStrictlyAscending localNode localNode)) (stableRaftConfigurationDto [localNode, localNode])
  assertEqual "reversed" (Left (RaftVotersNotStrictlyAscending remoteNode localNode)) (jointRaftConfigurationDto [localNode] [remoteNode, localNode])
  assertEqual "overlap retains two distinct sets" (JointRaftConfigurationDtoView [localNode, remoteNode] [remoteNode, otherNode]) (raftVotingConfigurationDtoView jointConfiguration)

caseConfigurationDecoder :: IO ()
caseConfigurationDecoder =
  mapM_
    rejects
    [ Binary.encode (0xff :: Word8),
      Binary.encode (0 :: Word8) <> Binary.encode ([] :: [RaftNodeIdDto]),
      Binary.encode (0 :: Word8) <> Binary.encode [localNode, localNode],
      Binary.encode (1 :: Word8) <> Binary.encode [localNode] <> Binary.encode [remoteNode, localNode],
      Binary.encode (1 :: Word8) <> Binary.encode [localNode] <> Binary.encode ([] :: [RaftNodeIdDto])
    ]
  where
    rejects bytes = assertBool "malformed nested configuration rejects" (isLeft (decodeBinary @RaftVotingConfigurationDto bytes))

propConfigurationAdapter :: [Word8] -> Bool -> Property
propConfigurationAdapter bytes joint =
  let configuration = if joint then jointConfiguration else stableConfiguration
      metadata = ByteString.pack bytes
      jointEntry = mustAdmit (raftLogEntryDto index1 term1 (ConfigurationDto jointConfiguration metadata))
      finalEntry = mustAdmit (raftLogEntryDto index2 term1 (ConfigurationDto stableConfiguration metadata))
      entries = if joint then [jointEntry] else [jointEntry, finalEntry]
      rpc = mustAdmit (appendEntriesDto term1 remoteNode index0 term0 entries (if joint then index1 else index2))
   in conjoin
        [ (raftRpcDtoToCore rpc >>= raftRpcDtoFromCore) === Right rpc,
          decodeBinary @RaftVotingConfigurationDto (Binary.encode configuration) === Right configuration,
          (raftVotingConfigurationDtoToCore configuration >>= raftVotingConfigurationDtoFromCore) === Right configuration
        ]

caseConfigurationSequence :: IO ()
caseConfigurationSequence = do
  let unchanged = mustAdmit (jointRaftConfigurationDto [remoteNode, otherNode] [remoteNode, otherNode])
      wrongFinal = mustAdmit (stableRaftConfigurationDto [localNode])
      fixtures =
        [ (index0, [stableConfiguration], RaftExpectedJointConfiguration),
          (index0, [unchanged], RaftUnchangedConfiguration),
          (index1, [stableConfiguration, stableConfiguration], RaftExpectedJointConfiguration),
          (index1, [jointConfiguration, jointConfiguration], RaftExpectedStableConfiguration),
          (index1, [jointConfiguration, wrongFinal], RaftFinalVoterSetMismatch),
          (index1, [stableConfiguration, jointConfiguration], RaftJointOldSetMismatch),
          (index1, [stableConfiguration, unchanged], RaftUnchangedConfiguration)
        ]
  mapM_
    ( \(previous, configurations, problem) -> do
        let start = raftLogIndexDtoWord64 previous
            entries = [mustAdmit (raftLogEntryDto (raftLogIndexDto index) term1 (ConfigurationDto configuration ByteString.empty)) | (index, configuration) <- zip [start + 1 ..] configurations]
            lastIndex = raftLogIndexDto (start + fromIntegral (length configurations))
            previousTerm = if previous == index0 then term0 else term1
        assertEqual "self-contained sequence contradiction" (Left (RaftAppendConfigurationSequenceInvalid lastIndex problem)) (appendEntriesDto term1 remoteNode previous previousTerm entries lastIndex)
        assertBool "canonical decode cannot bypass succession checks" (isLeft (decodeBinary @RaftRpcDto (rawAppend term1 previous previousTerm entries lastIndex)))
    )
    fixtures
  let suffix = [mustAdmit (raftLogEntryDto index2 term1 (ConfigurationDto stableConfiguration ByteString.empty))]
  assertBool "omitted prefix remains native log-matching work" (isRight (appendEntriesDto term1 remoteNode index1 term1 suffix index2))

caseVoteShape :: IO ()
caseVoteShape = do
  assertEqual
    "request term"
    (Left RaftRequestVoteTermIsZero)
    (requestVoteDto term0 remoteNode index0 term0)
  assertEqual
    "last-log sentinel coherence"
    (Left (RaftRequestVoteLastIndexTermShapeMismatch index1 term0))
    (requestVoteDto term1 remoteNode index1 term0)
  assertEqual
    "last-log term bound"
    (Left (RaftRequestVoteLastTermAfterRequestTerm term2 term1))
    (requestVoteDto term1 remoteNode index1 term2)
  assertEqual
    "response term"
    (Left RaftRequestVoteResponseTermIsZero)
    (requestVoteResponseDto term0 remoteNode False)

caseVoteShapeDecoder :: IO ()
caseVoteShapeDecoder = do
  let malformedBodies =
        [ rawRequestVote term0 index0 term0,
          rawRequestVote term1 index1 term0,
          rawRequestVote term1 index1 term2,
          rawRequestVoteResponse term0
        ]
  mapM_
    (\body -> assertBool (show body) (isLeft (decodeBinary @RaftRpcDto body)))
    malformedBodies

rawRequestVote :: RaftTermDto -> RaftLogIndexDto -> RaftTermDto -> LazyByteString.ByteString
rawRequestVote term lastIndex lastTerm =
  Binary.encode (0 :: Word8)
    <> Binary.encode term
    <> Binary.encode remoteNode
    <> Binary.encode lastIndex
    <> Binary.encode lastTerm

rawRequestVoteResponse :: RaftTermDto -> LazyByteString.ByteString
rawRequestVoteResponse term =
  Binary.encode (1 :: Word8)
    <> Binary.encode term
    <> Binary.encode remoteNode
    <> Binary.encode False

caseEntryShape :: IO ()
caseEntryShape = do
  assertEqual
    "constructor"
    (Left RaftLogEntryUsesSentinelIndex)
    (raftLogEntryDto index0 term0 LeaderNoOpDto)
  assertBool
    "decoder"
    ( isLeft
        ( decodeBinary @RaftLogEntryDto
            ( Binary.encode index0
                <> Binary.encode term0
                <> Binary.encode LeaderNoOpDto
            )
        )
    )
  assertEqual
    "constructor genesis term"
    (Left (RaftLogEntryUsesGenesisTerm index1))
    (raftLogEntryDto index1 term0 LeaderNoOpDto)
  assertBool
    "decoder genesis term"
    ( isLeft
        ( decodeBinary @RaftLogEntryDto
            ( Binary.encode index1
                <> Binary.encode term0
                <> Binary.encode LeaderNoOpDto
            )
        )
    )

caseAppendOrdering :: IO ()
caseAppendOrdering = do
  assertEqual
    "descending"
    (Left (RaftLogEntriesNotStrictlyAscending index2 index1))
    (appendEntriesDto term1 remoteNode index0 term0 [applicationEntry, noOpEntry] index2)
  assertEqual
    "duplicate"
    (Left (RaftLogEntriesNotStrictlyAscending index1 index1))
    (appendEntriesDto term1 remoteNode index0 term0 [noOpEntry, noOpEntry] index1)
  assertBool
    "empty heartbeat"
    (isRight (appendEntriesDto term1 remoteNode index0 term0 [] index0))

caseAppendOrderingDecoder :: IO ()
caseAppendOrderingDecoder =
  assertBool
    "raw descending entry list"
    ( isLeft
        ( decodeBinary @RaftRpcDto
            ( Binary.encode (2 :: Word8)
                <> Binary.encode term1
                <> Binary.encode remoteNode
                <> Binary.encode index0
                <> Binary.encode term0
                <> Binary.encode [applicationEntry, noOpEntry]
                <> Binary.encode index2
            )
        )
    )

caseAppendShape :: IO ()
caseAppendShape = do
  let index1Term2 = mustAdmit (raftLogEntryDto index1 term2 LeaderNoOpDto)
      index2Term2 = mustAdmit (raftLogEntryDto index2 term2 LeaderNoOpDto)
  mapM_
    ( \(label, expected, actual) ->
        assertEqual label (Left expected) actual
    )
    [ ( "zero request term",
        RaftAppendTermIsZero,
        appendEntriesDto term0 remoteNode index0 term0 [] index0
      ),
      ( "previous sentinel mismatch",
        RaftAppendPreviousIndexTermShapeMismatch index1 term0,
        appendEntriesDto term1 remoteNode index1 term0 [] index1
      ),
      ( "previous term after request term",
        RaftAppendPreviousTermAfterRequestTerm term2 term1,
        appendEntriesDto term1 remoteNode index1 term2 [] index1
      ),
      ( "entry gap",
        RaftAppendEntriesNotContiguous index1 index2,
        appendEntriesDto term1 remoteNode index0 term0 [applicationEntry] index2
      ),
      ( "entry term after request term",
        RaftAppendEntryTermAfterRequestTerm index1 term2 term1,
        appendEntriesDto term1 remoteNode index0 term0 [index1Term2] index1
      ),
      ( "entry term regression",
        RaftAppendEntryTermRegression index2 term2 term1,
        appendEntriesDto term2 remoteNode index1 term2 [applicationEntry] index2
      ),
      ( "leader commit after sent prefix",
        RaftAppendLeaderCommitAfterSentPrefix index1 index0,
        appendEntriesDto term1 remoteNode index0 term0 [] index1
      )
    ]
  assertEqual
    "non-regressing higher-term suffix"
    (Right (mustAdmit (appendEntriesDto term2 remoteNode index0 term0 [index1Term2, index2Term2] index2)))
    (appendEntriesDto term2 remoteNode index0 term0 [index1Term2, index2Term2] index2)

caseAppendShapeDecoder :: IO ()
caseAppendShapeDecoder = do
  let index1Term2 = mustAdmit (raftLogEntryDto index1 term2 LeaderNoOpDto)
      malformedBodies =
        [ rawAppend term0 index0 term0 [] index0,
          rawAppend term1 index1 term0 [] index1,
          rawAppend term1 index1 term2 [] index1,
          rawAppend term1 index0 term0 [applicationEntry] index2,
          rawAppend term1 index0 term0 [index1Term2] index1,
          rawAppend term2 index1 term2 [applicationEntry] index2,
          rawAppend term1 index0 term0 [] index1
        ]
  mapM_
    (\body -> assertBool (show body) (isLeft (decodeBinary @RaftRpcDto body)))
    malformedBodies

rawAppend ::
  RaftTermDto ->
  RaftLogIndexDto ->
  RaftTermDto ->
  [RaftLogEntryDto] ->
  RaftLogIndexDto ->
  LazyByteString.ByteString
rawAppend term previousIndex previousTerm entries leaderCommit =
  Binary.encode (2 :: Word8)
    <> Binary.encode term
    <> Binary.encode remoteNode
    <> Binary.encode previousIndex
    <> Binary.encode previousTerm
    <> Binary.encode entries
    <> Binary.encode leaderCommit

caseConflictShape :: IO ()
caseConflictShape = do
  let hint = ConflictingTermFromDto term1 index2
      accepted = mustAdmit (appendEntriesResponseDto term1 remoteNode True index2 index0 Nothing)
      missing =
        mustAdmit
          ( appendEntriesResponseDto
              term1
              remoteNode
              False
              index0
              index0
              (Just (MissingSuffixFromDto index1))
          )
      conflicting =
        mustAdmit
          (appendEntriesResponseDto term1 remoteNode False index1 index0 (Just hint))
  assertEqual
    "response term"
    (Left RaftAppendResponseTermIsZero)
    (appendEntriesResponseDto term0 remoteNode True index0 index0 Nothing)
  assertEqual
    "success with hint"
    (Left RaftSuccessfulAppendHasConflictHint)
    (appendEntriesResponseDto term1 remoteNode True index2 index0 (Just hint))
  assertEqual
    "failure without hint"
    (Left RaftRejectedAppendMissingConflictHint)
    (appendEntriesResponseDto term1 remoteNode False index0 index0 Nothing)
  assertEqual
    "missing-prefix index"
    (Left RaftConflictFirstIndexIsZero)
    ( appendEntriesResponseDto
        term1
        remoteNode
        False
        index0
        index0
        (Just (MissingSuffixFromDto index0))
    )
  assertEqual
    "conflict term"
    (Left RaftConflictTermIsZero)
    ( appendEntriesResponseDto
        term1
        remoteNode
        False
        index0
        index0
        (Just (ConflictingTermFromDto term0 index1))
    )
  assertEqual
    "conflict term bound"
    (Left (RaftConflictTermAfterResponseTerm term2 term1))
    ( appendEntriesResponseDto
        term1
        remoteNode
        False
        index0
        index0
        (Just (ConflictingTermFromDto term2 index1))
    )
  assertEqual
    "failure match must be the hint predecessor"
    (Left (RaftRejectedAppendMatchIndexMismatch index1 index0))
    (appendEntriesResponseDto term1 remoteNode False index0 index0 (Just hint))
  assertBool
    "coherent conflict"
    (isRight (appendEntriesResponseDto term1 remoteNode False index1 index0 (Just hint)))
  assertEqual
    "applied prefix exceeds match"
    (Left (RaftAppendResponseAppliedAfterMatch index2 index1))
    (appendEntriesResponseDto term1 remoteNode True index1 index2 Nothing)
  assertEqual
    "rejected prefix cannot attest application"
    (Left RaftRejectedAppendHasAppliedPrefix)
    (appendEntriesResponseDto term1 remoteNode False index1 index1 (Just hint))
  mapM_
    (\dto -> assertEqual (show dto) (Right dto) (decodeBinary (Binary.encode dto)))
    [accepted, missing, conflicting, mustAdmit (appendEntriesResponseDto term1 remoteNode True index2 index2 Nothing)]

caseConflictDecoder :: IO ()
caseConflictDecoder = do
  assertBool
    "zero response term"
    (isLeft (decodeBinary @RaftRpcDto (rawAppendResponse term0 True index0 Nothing Nothing)))
  assertBool
    "success with raw hint"
    (isLeft (decodeBinary @RaftRpcDto (rawAppendResponse term1 True index2 (Just term1) (Just index2))))
  assertBool
    "failure without raw hint"
    (isLeft (decodeBinary @RaftRpcDto (rawAppendResponse term1 False index0 Nothing Nothing)))
  assertBool
    "missing-prefix index zero"
    (isLeft (decodeBinary @RaftRpcDto (rawAppendResponse term1 False index0 Nothing (Just index0))))
  assertBool
    "conflict term zero"
    (isLeft (decodeBinary @RaftRpcDto (rawAppendResponse term1 False index0 (Just term0) (Just index1))))
  assertBool
    "conflict term after response"
    (isLeft (decodeBinary @RaftRpcDto (rawAppendResponse term1 False index0 (Just term2) (Just index1))))
  assertBool
    "conflict term without first index"
    (isLeft (decodeBinary @RaftRpcDto (rawAppendResponse term1 False index0 (Just term1) Nothing)))
  assertBool
    "wrong failed match index"
    (isLeft (decodeBinary @RaftRpcDto (rawAppendResponse term1 False index0 (Just term1) (Just index2))))

rawAppendResponse ::
  RaftTermDto ->
  Bool ->
  RaftLogIndexDto ->
  Maybe RaftTermDto ->
  Maybe RaftLogIndexDto ->
  LazyByteString.ByteString
rawAppendResponse responseTerm success matchIndex conflictTerm firstIndex =
  Binary.encode (3 :: Word8)
    <> Binary.encode responseTerm
    <> Binary.encode remoteNode
    <> Binary.encode success
    <> Binary.encode matchIndex
    <> Binary.encode index0
    <> Binary.encode conflictTerm
    <> Binary.encode firstIndex

caseCheckpointShape :: IO ()
caseCheckpointShape = do
  let payload = ByteString.pack [0, 1, 2]
      make index term reference = raftCheckpointDto index term reference stableConfiguration payload
      valid = mustAdmit (make index3 term2 (Just (index2, term1)))
  assertEqual "checkpoint needs a real native prefix" (Left RaftCheckpointIndexIsZero) (make index0 term1 Nothing)
  assertEqual "checkpoint term is not genesis" (Left RaftCheckpointTermIsZero) (make index1 term0 Nothing)
  mapM_ (\reference -> assertEqual "configuration must be in the checkpoint prefix" (Left RaftCheckpointConfigurationReferenceInvalid) (make index2 term1 (Just reference))) [(index0, term1), (index1, term0), (index3, term1), (index1, term2)]
  assertEqual "checkpoint fields are payload-exact" (index3, term2, Just (index2, term1), stableConfiguration, payload) (raftCheckpointDtoFields valid)
  assertEqual "checkpoint binary roundtrip" (Right valid) (decodeBinary @RaftCheckpointDto (Binary.encode valid))
  assertBool "raw configuration beyond snapshot rejects" (isLeft (decodeBinary @RaftCheckpointDto (Binary.encode index1 <> Binary.encode term1 <> Binary.encode (Just (index2, term1)) <> Binary.encode stableConfiguration <> Binary.encode payload)))
  assertEqual "snapshot request term covers its checkpoint" (Left (RaftSnapshotTermBeforeCheckpoint term1 term2)) (installSnapshotDto term1 remoteNode valid)
  assertEqual "snapshot responses name a real checkpoint" (Left RaftCheckpointIndexIsZero) (installSnapshotResponseDto term1 remoteNode index0 True)

caseRpcCatalogue :: IO ()
caseRpcCatalogue = do
  assertEqual
    "one fixture for each constructor in declaration order"
    closedRpcKindCatalogue
    (fmap raftRpcKind rpcDtos)
  assertEqual
    "RPC kind catalogue follows the complete derived enumeration"
    [minBound .. maxBound]
    closedRpcKindCatalogue
  assertEqual
    "RPC kind patterns are closed"
    [0, 1, 2, 3, 4, 5]
    (fmap raftRpcKindConstructor closedRpcKindCatalogue)
  assertEqual
    "envelope patterns are closed"
    [0, 1]
    ( fmap
        raftEnvelopeConstructor
        [RaftHello hello, RaftRpc requestVote]
    )
  assertEqual
    "log-payload patterns are closed"
    [0, 1, 2]
    ( fmap
        raftLogPayloadConstructor
        [LeaderNoOpDto, ApplicationBytesDto ByteString.empty, ConfigurationDto stableConfiguration ByteString.empty]
    )
  assertEqual
    "conflict-hint patterns are closed"
    [0, 1]
    ( fmap
        raftConflictHintConstructor
        [ MissingSuffixFromDto index1,
          ConflictingTermFromDto term1 index1
        ]
    )

closedRpcKindCatalogue :: [RaftRpcKind]
closedRpcKindCatalogue =
  [ RequestVoteKind,
    RequestVoteResponseKind,
    AppendEntriesKind,
    AppendEntriesResponseKind,
    InstallSnapshotKind,
    InstallSnapshotResponseKind
  ]

raftRpcKindConstructor :: RaftRpcKind -> Int
raftRpcKindConstructor = \case
  RequestVoteKind -> 0
  RequestVoteResponseKind -> 1
  AppendEntriesKind -> 2
  AppendEntriesResponseKind -> 3
  InstallSnapshotKind -> 4
  InstallSnapshotResponseKind -> 5

raftEnvelopeConstructor :: RaftProtocolEnvelope -> Int
raftEnvelopeConstructor = \case
  RaftHello _ -> 0
  RaftRpc _ -> 1

raftLogPayloadConstructor :: RaftLogEntryPayloadDto -> Int
raftLogPayloadConstructor = \case
  LeaderNoOpDto -> 0
  ApplicationBytesDto _ -> 1
  ConfigurationDto _ _ -> 2

raftConflictHintConstructor :: RaftConflictHintDto -> Int
raftConflictHintConstructor = \case
  MissingSuffixFromDto _ -> 0
  ConflictingTermFromDto _ _ -> 1

caseRoleAdmission :: IO ()
caseRoleAdmission = do
  assertEqual "matching hello" (Right hello) (admitRaftHello connectionContext hello)
  assertEqual
    "digest"
    (Left RaftHelloGenesisDigestMismatch)
    ( admitRaftHello
        connectionContext
        (mustAdmit (raftHelloClaim otherGenesisDigest remoteNode localNode))
    )
  assertEqual
    "source"
    (Left (RaftHelloSourceMismatch remoteNode otherNode))
    ( admitRaftHello
        connectionContext
        (mustAdmit (raftHelloClaim genesisDigest otherNode localNode))
    )
  assertEqual
    "target"
    (Left (RaftHelloTargetMismatch localNode otherNode))
    ( admitRaftHello
        connectionContext
        (mustAdmit (raftHelloClaim genesisDigest remoteNode otherNode))
    )
  assertEqual
    "RPC before hello"
    (Left RaftHelloExpected)
    (admitRaftEnvelope AwaitingRaftHello connectionContext (RaftRpc requestVote))
  assertEqual
    "hello after establishment"
    (Left RaftRpcExpected)
    (admitRaftEnvelope EstablishedRaftBinding connectionContext (RaftHello hello))

caseEveryRpcSourceRole :: IO ()
caseEveryRpcSourceRole = do
  mapM_
    (\rpc -> assertEqual (show (raftRpcKind rpc)) (Right rpc) (admitRaftRpc connectionContext rpc))
    rpcVariantDtos
  let wrongSourceDtos =
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
          mustAdmit (appendEntriesResponseDto term1 otherNode True index2 index0 Nothing),
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
    ( \rpc ->
        assertEqual
          (show (raftRpcKind rpc))
          (Left (RaftRpcSourceMismatch remoteNode otherNode))
          (admitRaftRpc connectionContext rpc)
    )
    wrongSourceDtos

caseCoreIdentityAdapters :: IO ()
caseCoreIdentityAdapters = do
  remoteCore <- mustAdmitIO (raftNodeIdDtoToCore remoteNode)
  assertEqual "node round-trip" (Right remoteNode) (raftNodeIdDtoFromCore remoteCore)
  assertEqual "term" term2 (raftTermDtoFromCore (raftTermDtoToCore term2))
  assertEqual "index" index3 (raftLogIndexDtoFromCore (raftLogIndexDtoToCore index3))

caseCoreRpcAdapters :: IO ()
caseCoreRpcAdapters =
  mapM_
    ( \dto -> do
        core <- mustAdmitIO (raftRpcDtoToCore dto)
        assertEqual (show (raftRpcKind dto)) (Right dto) (raftRpcDtoFromCore core)
    )
    rpcVariantDtos

caseCoreInputAdapters :: IO ()
caseCoreInputAdapters = do
  let generation = CoreIdentity.raftDispatchGeneration 7
      requestDtos = [requestVote, appendEntries, installSnapshot]
      responseDtos =
        [ requestVoteResponse,
          appendEntriesAcceptedResponse,
          appendEntriesMissingPrefixResponse,
          appendEntriesResponse,
          installSnapshotResponse
        ]
  mapM_
    ( \request -> do
        assertBool
          ("request arm " <> show (raftRpcKind request))
          (isRight (raftRequestDtoToCoreInput remoteNode request))
        assertEqual
          ("request rejected as response " <> show (raftRpcKind request))
          (Left (RaftAdapterInputFault CoreInput.RaftExpectedResponseObservation))
          (raftResponseDtoToCoreInput remoteNode generation request)
    )
    requestDtos
  mapM_
    ( \response -> do
        assertBool
          ("response arm " <> show response)
          (isRight (raftResponseDtoToCoreInput remoteNode generation response))
        assertEqual
          ("response rejected as request " <> show response)
          (Left (RaftAdapterInputFault CoreInput.RaftExpectedRequestObservation))
          (raftRequestDtoToCoreInput remoteNode response)
    )
    responseDtos
  mapM_ assertPhysicalSource requestDtos
  mapM_ assertPhysicalSource responseDtos
  where
    assertPhysicalSource dto =
      assertEqual
        ("observed source " <> show dto)
        ( Left
            ( RaftAdapterInputFault
                ( CoreInput.RaftRpcSourceMismatch
                    (mustAdmit (CoreIdentity.mkRaftNodeId (claimBytes 5)))
                    (mustAdmit (CoreIdentity.mkRaftNodeId (claimBytes 4)))
                )
            )
        )
        ( case raftRpcKind dto of
            RequestVoteKind -> raftRequestDtoToCoreInput otherNode dto
            AppendEntriesKind -> raftRequestDtoToCoreInput otherNode dto
            InstallSnapshotKind -> raftRequestDtoToCoreInput otherNode dto
            RequestVoteResponseKind ->
              raftResponseDtoToCoreInput otherNode (CoreIdentity.raftDispatchGeneration 7) dto
            AppendEntriesResponseKind ->
              raftResponseDtoToCoreInput otherNode (CoreIdentity.raftDispatchGeneration 7) dto
            InstallSnapshotResponseKind ->
              raftResponseDtoToCoreInput otherNode (CoreIdentity.raftDispatchGeneration 7) dto
        )

decodeBinary :: (Binary value) => LazyByteString.ByteString -> Either String value
decodeBinary bytes = case Binary.decodeOrFail bytes of
  Left (_, _, message) -> Left message
  Right (residual, _, value)
    | LazyByteString.null residual -> Right value
    | otherwise -> Left "trailing bytes"

mustAdmitIO :: (Show problem) => Either problem value -> IO value
mustAdmitIO = either (fail . show) pure
