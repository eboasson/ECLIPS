module GenesisProperties
  ( tests,
    nodeId,
    heartbeat,
    electionLower,
    electionUpper,
    checkedGenesisFor,
    checkedGenesisWithVoters,
  )
where

import Data.ByteString qualified as ByteString
import Data.Set qualified as Set
import Data.Word (Word8)
import Eclips.Raft.Genesis
  ( CheckedRaftGenesis,
    RaftGenesisFault (..),
    checkRaftGenesis,
    checkedRaftLocalNode,
    checkedRaftNativeConfiguration,
    raftGenesis,
    raftNativeElectionTimeoutLower,
    raftNativeElectionTimeoutUpper,
    raftNativeHasQuorum,
    raftNativeHeartbeatInterval,
    raftNativeVoters,
  )
import Eclips.Raft.Identity
  ( RaftDurationMicros,
    RaftIdentityFault (..),
    RaftNodeId,
    mkRaftDurationMicros,
    mkRaftNodeId,
    mkRaftProposalId,
    raftNodeIdBytes,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Property, chooseInt, conjoin, forAll, sublistOf, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "genesis"
    [ testProperty "RaftNodeId accepts exactly 32 bytes" nodeIdShapeProperty,
      testCase "RaftNodeId renders as grouped lowercase hexadecimal" nodeIdRenderingCase,
      testCase "checked native configuration retains exact normalized facts" checkedFactsCase,
      testCase "voter set must be nonempty" emptyVotersCase,
      testProperty "all nonempty cardinalities use the same voter-only majority" majorityProperty,
      testCase "voter set must be strictly ascending and unique" orderCase,
      testCase "local node must be a voter" localMembershipCase,
      testCase "election lower bound follows heartbeat" lowerBoundCase,
      testCase "election upper bound does not precede lower" upperBoundCase,
      testCase "positive proposal/timer quantities reject zero" positiveQuantityCase
    ]

nodeIdShapeProperty :: Property
nodeIdShapeProperty =
  forAll (chooseInt (0, 64)) $ \size ->
    case mkRaftNodeId (ByteString.replicate size 7) of
      Right node -> (size, ByteString.length (raftNodeIdBytes node)) === (32, 32)
      Left (WrongRaftNodeIdByteCount expected actual) ->
        (expected, actual, size == 32) === (32, size, False)
      Left problem -> error ("unexpected identity fault: " <> show problem)

nodeIdRenderingCase :: IO ()
nodeIdRenderingCase =
  show (checked "RaftNodeId" (mkRaftNodeId (ByteString.pack [0 .. 31])))
    @?= "00010203:04050607:08090a0b:0c0d0e0f:10111213:14151617:18191a1b:1c1d1e1f"

checkedFactsCase :: IO ()
checkedFactsCase = do
  let genesis = checkedGenesisFor (nodeId 2)
      configuration = checkedRaftNativeConfiguration genesis
  checkedRaftLocalNode genesis @?= nodeId 2
  raftNativeVoters configuration @?= voters
  raftNativeHeartbeatInterval configuration @?= heartbeat
  raftNativeElectionTimeoutLower configuration @?= electionLower
  raftNativeElectionTimeoutUpper configuration @?= electionUpper

emptyVotersCase :: IO ()
emptyVotersCase =
  checkRaftGenesis
    (raftGenesis (nodeId 1) [] heartbeat electionLower electionUpper)
    @?= Left RaftVotersEmpty

majorityProperty :: Property
majorityProperty =
  conjoin
    [ forAll (sublistOf (map nodeId [1 .. count + 3])) $ \acknowledgers ->
        let members = map nodeId [1 .. count]
            configuration =
              checkedRaftNativeConfiguration (checkedGenesisWithVoters (nodeId 1) members)
            counted = length (filter (`elem` members) acknowledgers)
         in (raftNativeVoters configuration, raftNativeHasQuorum configuration (Set.fromList acknowledgers))
              === (members, counted >= length members `div` 2 + 1)
    | count <- [1 .. 8]
    ]

orderCase :: IO ()
orderCase = do
  checkRaftGenesis
    (raftGenesis (nodeId 1) [nodeId 1, nodeId 3, nodeId 2] heartbeat electionLower electionUpper)
    @?= Left RaftVotersNotStrictlyAscending
  checkRaftGenesis
    (raftGenesis (nodeId 1) [nodeId 1, nodeId 1, nodeId 2] heartbeat electionLower electionUpper)
    @?= Left RaftVotersNotStrictlyAscending

localMembershipCase :: IO ()
localMembershipCase =
  checkRaftGenesis
    (raftGenesis (nodeId 4) voters heartbeat electionLower electionUpper)
    @?= Left (RaftLocalNodeNotVoter (nodeId 4))

lowerBoundCase :: IO ()
lowerBoundCase =
  checkRaftGenesis
    (raftGenesis (nodeId 1) voters heartbeat heartbeat electionUpper)
    @?= Left RaftElectionTimeoutLowerNotAfterHeartbeat

upperBoundCase :: IO ()
upperBoundCase =
  checkRaftGenesis
    (raftGenesis (nodeId 1) voters heartbeat electionUpper electionLower)
    @?= Left RaftElectionTimeoutUpperBeforeLower

positiveQuantityCase :: IO ()
positiveQuantityCase = do
  mkRaftDurationMicros 0 @?= Left ZeroRaftDuration
  mkRaftProposalId 0 @?= Left ZeroRaftProposalId
  mkRaftNodeId ByteString.empty @?= Left (WrongRaftNodeIdByteCount 32 0)

checkedGenesisFor :: RaftNodeId -> CheckedRaftGenesis
checkedGenesisFor local = checkedGenesisWithVoters local voters

checkedGenesisWithVoters :: RaftNodeId -> [RaftNodeId] -> CheckedRaftGenesis
checkedGenesisWithVoters local members =
  checked
    "Raft genesis"
    (checkRaftGenesis (raftGenesis local members heartbeat electionLower electionUpper))

voters :: [RaftNodeId]
voters = map nodeId [1, 2, 3]

nodeId :: Word8 -> RaftNodeId
nodeId byte = checked "RaftNodeId" (mkRaftNodeId (ByteString.replicate 32 byte))

heartbeat :: RaftDurationMicros
heartbeat = checked "heartbeat" (mkRaftDurationMicros 10)

electionLower :: RaftDurationMicros
electionLower = checked "election lower" (mkRaftDurationMicros 30)

electionUpper :: RaftDurationMicros
electionUpper = checked "election upper" (mkRaftDurationMicros 50)

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (error . ((label <> ": ") <>) . show) id
