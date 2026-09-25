module SmallClusterProperties
  ( tests,
  )
where

import Control.Monad (foldM, forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word64, Word8)
import Eclips.Raft.Effect
  ( RaftEffect (..),
    RaftProposalStatus (..),
    committedEntryIndex,
    committedEntryPayload,
    raftEffectBatchEffects,
  )
import Eclips.Raft.Identity
  ( RaftDispatchGeneration,
    RaftNodeId,
    RaftProposalId,
    mkRaftProposalId,
    raftLogIndex,
    raftLogIndexWord64,
    raftTerm,
  )
import Eclips.Raft.Input
  ( RaftAppendResult (..),
    RaftEntry (..),
    RaftInput,
    RaftRpc,
    RaftRpcView (..),
    acknowledgeCommittedEntries,
    appendEntriesResponse,
    fireElectionTimer,
    fireHeartbeatTimer,
    observeRaftResponse,
    proposeRaftApplication,
    raftLogEntryIndex,
    raftLogEntryPayload,
    raftLogEntryTerm,
    raftRpcView,
    requestVoteResponse,
    selectElectionTimeout,
  )
import Eclips.Raft.State
  ( RaftRole (..),
    RaftState,
    raftStateAppliedThrough,
    raftStateCommitIndex,
    raftStateElectionGeneration,
    raftStateHeartbeatGeneration,
    raftStateLastLogIndex,
    raftStateLogEntries,
    raftStatePendingProposal,
    raftStateRole,
    raftStateServiceReady,
    raftStateTerm,
    raftStateVotedFor,
    raftStateVoters,
  )
import Eclips.Raft.Transition (initialRaft, stepRaft)
import GenesisProperties (checkedGenesisWithVoters, electionLower, nodeId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    arbitrary,
    chooseInt,
    conjoin,
    forAll,
    ioProperty,
    listOf1,
    shuffle,
    testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "nonempty voter sets"
    [ testProperty "singleton commits generated opaque applications with ordered readiness" singletonProperty,
      testProperty "one through eight voters elect and commit at exactly their majority" quorumProgressProperty,
      testProperty "isolated two- and three-voter survivors cannot elect, commit, or shrink" minorityProperty
    ]

type Payload = ByteString

localNode :: RaftNodeId
localNode = nodeId 1

initialState :: [RaftNodeId] -> RaftState Payload
initialState members = initialRaft (checkedGenesisWithVoters localNode members)

singletonProperty :: Property
singletonProperty =
  forAll (listOf1 (arbitrary :: Gen [Word8])) $ \payloads -> ioProperty $ do
    (leader, electionEffects) <- campaign (initialState [localNode])
    raftStateRole leader @?= RaftLeader
    raftStateVotedFor leader @?= Just localNode
    raftStateTerm leader @?= raftTerm 1
    raftStateCommitIndex leader @?= raftLogIndex 1
    raftStateServiceReady leader @?= False
    map (\entry -> (raftLogEntryIndex entry, raftLogEntryTerm entry, raftLogEntryPayload entry)) (raftStateLogEntries leader)
      @?= [(raftLogIndex 0, raftTerm 0, LeaderNoOp), (raftLogIndex 1, raftTerm 1, LeaderNoOp)]
    assertNoRpc electionEffects
    exposedApplications electionEffects @?= []
    (ready, _) <- transition (acknowledgeCommittedEntries (raftLogIndex 1)) leader
    raftStateServiceReady ready @?= True
    final <- foldM appendAndApply ready (zip [2 ..] (map ByteString.pack payloads))
    map raftLogEntryPayload (raftStateLogEntries final)
      @?= LeaderNoOp : LeaderNoOp : map (Application . ByteString.pack) payloads
    map raftLogEntryTerm (raftStateLogEntries final)
      @?= raftTerm 0 : replicate (length payloads + 1) (raftTerm 1)
    pure True
  where
    appendAndApply state (position, payload) = do
      let index = raftLogIndex position
          proposal = proposalId position
      (committed, effects) <- transition (proposeRaftApplication proposal payload) state
      raftStateCommitIndex committed @?= index
      raftStateLastLogIndex committed @?= index
      raftStatePendingProposal committed @?= Nothing
      raftStateServiceReady committed @?= False
      assertNoRpc effects
      proposalStatuses effects @?= [(proposal, RaftProposalAppended index), (proposal, RaftProposalCommitted index)]
      case [entry | ExposeCommittedEntries entries <- effects, entry <- NonEmpty.toList entries] of
        [entry] -> do
          committedEntryIndex entry @?= index
          committedEntryPayload entry @?= Application payload
        other -> assertFailure ("expected exact committed application entry: " <> show other)
      exposedApplications effects @?= [(position, payload)]
      (blocked, blockedEffects) <- transition (proposeRaftApplication proposal payload) committed
      blocked @?= committed
      proposalStatuses blockedEffects @?= [(proposal, RaftProposalNotReady (raftTerm 1))]
      (applied, _) <- transition (acknowledgeCommittedEntries index) committed
      raftStateAppliedThrough applied @?= index
      raftStateServiceReady applied @?= True
      (repeated, repeatedEffects) <- transition (acknowledgeCommittedEntries index) applied
      repeated @?= applied
      repeatedEffects @?= []
      pure applied

quorumProgressProperty :: Property
quorumProgressProperty =
  conjoin
    [ forAll (shuffle (map nodeId [2 .. count])) $ \peers -> ioProperty $ do
        let members = map nodeId [1 .. count]
        (ready, _) <- establishLeader members peers
        let payload = ByteString.pack [count, 99]
            proposal = proposalId 1
        (appended, appendEffects) <- transition (proposeRaftApplication proposal payload) ready
        raftStateCommitIndex appended @?= raftLogIndex (if count == 1 then 2 else 1)
        (committed, _) <- acknowledgeQuorum members peers 2 (appended, appendEffects)
        raftStateCommitIndex committed @?= raftLogIndex 2
        raftStateServiceReady committed @?= False
        raftStatePendingProposal committed @?= Nothing
        (applied, _) <- transition (acknowledgeCommittedEntries (raftLogIndex 2)) committed
        raftStateServiceReady applied @?= True
        raftStateVoters applied @?= members
        pure True
    | count <- [1 .. 8]
    ]

minorityProperty :: Property
minorityProperty =
  forAll (chooseInt (1, 12)) $ \attempts -> ioProperty $ do
    forM_ [2, 3] $ \count -> do
      let members = map nodeId [1 .. count]
          peers = drop 1 members
      _ <- foldM (retryElection members) (initialState members) [1 .. attempts]
      (leader, _) <- establishLeader members peers
      (pending, _) <- transition (proposeRaftApplication (proposalId 1) (ByteString.singleton 9)) leader
      stalled <- foldM (retryHeartbeat members) pending [1 .. attempts]
      raftStateCommitIndex stalled @?= raftLogIndex 1
      raftStateLastLogIndex stalled @?= raftLogIndex 2
      raftStatePendingProposal stalled @?= Just (proposalId 1, raftLogIndex 2)
      raftStateServiceReady stalled @?= False
    pure True
  where
    retryElection members state attempt = do
      (candidate, _) <- campaign state
      raftStateRole candidate @?= RaftCandidate
      raftStateTerm candidate @?= raftTerm (fromIntegral attempt)
      raftStateVoters candidate @?= members
      raftStateCommitIndex candidate @?= raftLogIndex 0
      raftStateLastLogIndex candidate @?= raftLogIndex 0
      (redirected, _) <- transition (proposeRaftApplication (proposalId 1) ByteString.empty) candidate
      redirected @?= candidate
      pure candidate
    retryHeartbeat members state _ = do
      (successor, effects) <- transition (fireHeartbeatTimer (raftStateHeartbeatGeneration state)) state
      raftStateVoters successor @?= members
      raftStateCommitIndex successor @?= raftStateCommitIndex state
      exposedApplications effects @?= []
      pure successor

establishLeader ::
  [RaftNodeId] ->
  [RaftNodeId] ->
  IO (RaftState Payload, [RaftEffect Payload])
establishLeader members peers = do
  (candidate, voteEffects) <- campaign (initialState members)
  raftStateRole candidate @?= if length members == 1 then RaftLeader else RaftCandidate
  (leader, leaderEffects) <-
    foldM (receiveVote voteEffects) (candidate, voteEffects) (zip [1 ..] (take requiredPeers peers))
  raftStateRole leader @?= RaftLeader
  raftStateCommitIndex leader @?= raftLogIndex (if length members == 1 then 1 else 0)
  raftStateServiceReady leader @?= False
  (committed, effects) <- acknowledgeQuorum members peers 1 (leader, leaderEffects)
  (ready, _) <- transition (acknowledgeCommittedEntries (raftLogIndex 1)) committed
  pure (ready, effects)
  where
    requiredPeers = length members `div` 2
    receiveVote sent (state, _) (received, peer) = do
      (generation, _) <- requireSend peer sent
      rpc <- checked (requestVoteResponse (raftTerm 1) peer True)
      input <- checked (observeRaftResponse peer generation rpc)
      result@(successor, _) <- transition input state
      raftStateRole successor @?= if received == requiredPeers then RaftLeader else RaftCandidate
      (duplicate, duplicateEffects) <- transition input successor
      duplicate @?= successor
      duplicateEffects @?= []
      pure result

acknowledgeQuorum ::
  [RaftNodeId] ->
  [RaftNodeId] ->
  Word64 ->
  (RaftState Payload, [RaftEffect Payload]) ->
  IO (RaftState Payload, [RaftEffect Payload])
acknowledgeQuorum members peers position initial@(_, sent) =
  foldM receiveAppend initial (zip [1 ..] (take requiredPeers peers))
  where
    requiredPeers = length members `div` 2
    receiveAppend (state, _) (received, peer) = do
      (generation, request) <- requireSend peer sent
      case raftRpcView request of
        AppendEntriesView _ _ _ _ entries _ ->
          map raftLogEntryIndex entries @?= [raftLogIndex position]
        other -> assertFailure ("expected AppendEntries, got " <> show other)
      rpc <- checked (appendEntriesResponse (raftTerm 1) peer (AppendAccepted (raftLogIndex position) (raftLogIndex 0)))
      input <- checked (observeRaftResponse peer generation rpc)
      result@(successor, effects) <- transition input state
      raftStateCommitIndex successor
        @?= raftLogIndex (if received == requiredPeers then position else position - 1)
      if received < requiredPeers then exposedApplications effects @?= [] else pure ()
      pure result

campaign :: RaftState Payload -> IO (RaftState Payload, [RaftEffect Payload])
campaign state = do
  let generation = raftStateElectionGeneration state
  (armed, _) <- transition (selectElectionTimeout generation electionLower) state
  transition (fireElectionTimer generation) armed

transition :: RaftInput Payload -> RaftState Payload -> IO (RaftState Payload, [RaftEffect Payload])
transition input state = do
  (successor, batch) <- checked (stepRaft input state)
  pure (successor, raftEffectBatchEffects batch)

requireSend ::
  RaftNodeId ->
  [RaftEffect Payload] ->
  IO (RaftDispatchGeneration, RaftRpc Payload)
requireSend peer effects = case [(generation, rpc) | SendRaftRpc target generation rpc <- effects, target == peer] of
  [sent] -> pure sent
  other -> assertFailure ("expected one RPC to " <> show peer <> ", got " <> show other)

proposalStatuses :: [RaftEffect Payload] -> [(RaftProposalId, RaftProposalStatus)]
proposalStatuses effects = [(proposal, status) | ReportRaftProposal proposal status <- effects]

exposedApplications :: [RaftEffect Payload] -> [(Word64, Payload)]
exposedApplications effects =
  [ (raftLogIndexWord64 (committedEntryIndex entry), payload)
  | ExposeCommittedEntries entries <- effects,
    entry <- NonEmpty.toList entries,
    Application payload <- [committedEntryPayload entry]
  ]

assertNoRpc :: [RaftEffect Payload] -> Assertion
assertNoRpc effects = [target | SendRaftRpc target _ _ <- effects] @?= []

proposalId :: Word64 -> RaftProposalId
proposalId value = either (error . show) id (mkRaftProposalId value)

checked :: (Show fault) => Either fault value -> IO value
checked = either (assertFailure . show) pure
