{-# LANGUAGE OverloadedStrings #-}

module ConfigurationProperties (tests) where

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Eclips.Raft.Configuration
import Eclips.Raft.Effect
import Eclips.Raft.Genesis
import Eclips.Raft.Identity
import Eclips.Raft.Input
import Eclips.Raft.State
import Eclips.Raft.Transition
import GenesisProperties (checkedGenesisWithVoters, electionLower, electionUpper, heartbeat, nodeId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (forAll, shuffle, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "checked native configuration boundaries"
    [ testCase "voter sets and configuration references reject malformed shape" checkedShape,
      testProperty "only the canonical voter order is admitted" $ forAll (shuffle [nodeA, nodeB, nodeC]) $ \nodes ->
        isLeft (raftVoterSet nodes) === (nodes /= [nodeA, nodeB, nodeC]),
      testCase "learner genesis preserves immutable voters and starts without election timers" learnerGenesis,
      testCase "a learner registration can be withdrawn without permanent startup participation" removeLearner,
      testCase "a promoted and later removed learner retains only explicit learner registration" removeFormerLearner,
      testCase "RPC admission rejects malformed configuration sequences" malformedSequence,
      testCase "the matched local prefix validates the first received configuration" malformedPredecessor,
      testCase "committed no-op application and configuration require contiguous acknowledgements" orderedAcknowledgements,
      testCase "learner readiness requires exact application and matching replication evidence" exactReadiness,
      testCase "an unstarted change cancels without a configuration entry" cancelPreparation,
      testCase "leadership change invalidates captured learner readiness" staleReadiness,
      testCase "singleton heartbeats renew through a fresh complete self quorum" singletonGuard,
      testCase "a distinct outstanding response from before renewal cannot renew the guard" delayedOldGuardResponse,
      testCase "an insufficient quorum arms and expires an unprotected observation window" partialWindowEffects
    ]

nodeA, nodeB, nodeC :: RaftNodeId
nodeA = nodeId 1
nodeB = nodeId 2
nodeC = nodeId 3

voters :: [RaftNodeId] -> RaftVoterSet
voters = checked . raftVoterSet
proposal :: RaftProposalId
proposal = checked (mkRaftProposalId 10)
initial :: RaftNodeId -> [RaftNodeId] -> RaftState ByteString
initial node = initialRaft . checkedGenesisWithVoters node
learner :: RaftNodeId -> [RaftNodeId] -> RaftState ByteString
learner node nodes = initialRaft (checked (checkRaftLearnerGenesis (raftGenesis node nodes heartbeat electionLower electionUpper)))
checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
step :: RaftInput ByteString -> RaftState ByteString -> (RaftState ByteString, [RaftEffect ByteString])
step input state = let (successor, batch) = checked (stepRaft input state) in (successor, raftEffectBatchEffects batch)
request :: RaftNodeId -> RaftRpc ByteString -> RaftState ByteString -> (RaftState ByteString, [RaftEffect ByteString])
request source rpc = step (checked (observeRaftRequest source rpc))
response :: RaftNodeId -> RaftDispatchGeneration -> RaftRpc ByteString -> RaftState ByteString -> (RaftState ByteString, [RaftEffect ByteString])
response source generation rpc = step (checked (observeRaftResponse source generation rpc))
sendTo :: RaftNodeId -> [RaftEffect ByteString] -> (RaftDispatchGeneration, RaftRpc ByteString)
sendTo target effects = case [(generation, rpc) | SendRaftRpc peer generation rpc <- effects, peer == target] of
  [packet] -> packet
  other -> error ("expected one send: " <> show other)
campaign :: RaftState ByteString -> (RaftState ByteString, [RaftEffect ByteString])
campaign state = let generation = raftStateElectionGeneration state; selected = fst (step (selectElectionTimeout generation electionLower) state) in step (fireElectionTimer generation) selected
singleton :: RaftState ByteString
singleton = let (leader, _) = campaign (initial nodeA [nodeA]) in fst (step (acknowledgeCommittedEntries (raftLogIndex 1)) leader)
prepared :: (RaftState ByteString, RaftCatchUpFrontier, RaftDispatchGeneration, RaftRpc ByteString)
prepared =
  let (targeted, sends) = step (checked (updateRaftReplicationTargets [nodeB])) singleton
      (generation, rpc) = sendTo nodeB sends
      (pending, _) = step (beginRaftConfigurationChange proposal genesisRaftConfigurationRef (voters [nodeA, nodeB])) targeted
   in (pending, fromJust (raftStateCatchUpFrontier pending), generation, rpc)

checkedShape :: Assertion
checkedShape = do
  raftVoterSet [] @?= Left RaftConfigurationVotersEmpty
  raftVoterSet [nodeA, nodeA] @?= Left RaftConfigurationVotersNotStrictlyAscending
  raftVoterSet [nodeB, nodeA] @?= Left RaftConfigurationVotersNotStrictlyAscending
  raftConfigurationEntryRef (raftLogIndex 0) (raftTerm 1) @?= Left RaftConfigurationReferenceIndexIsZero
  raftConfigurationEntryRef (raftLogIndex 1) (raftTerm 0) @?= Left RaftConfigurationReferenceTermIsZero
  raftConfigurationRefView (checked (raftConfigurationEntryRef (raftLogIndex 4) (raftTerm 2))) @?= RaftConfigurationEntryRefView (raftLogIndex 4) (raftTerm 2)

learnerGenesis :: Assertion
learnerGenesis = do
  let state = learner nodeB [nodeA]
  raftStateVoters state @?= [nodeA]
  raftStateParticipation state @?= RaftLearner
  raftEffectBatchEffects (initialRaftEffects state) @?= []
  fst (campaign state) @?= state
  checkRaftLearnerGenesis (raftGenesis nodeA [nodeA] heartbeat electionLower electionUpper) @?= Left (RaftLearnerAlreadyInitialVoter nodeA)

removeLearner :: Assertion
removeLearner = do
  let state = learner nodeB [nodeA]
      (removed, _) = step (checked (updateRaftReplicationTargets [])) state
      (restored, _) = step (checked (updateRaftReplicationTargets [nodeB])) removed
  raftStateParticipation removed @?= RaftRemoved
  raftStateCanCampaign removed @?= False
  raftStateCanVote removed @?= False
  raftStateParticipation restored @?= RaftLearner

removeFormerLearner :: Assertion
removeFormerLearner = do
  let setA = voters [nodeA]
      setAB = voters [nodeA, nodeB]
      configurations = [jointRaftConfiguration setA setAB, stableRaftConfiguration setAB, jointRaftConfiguration setAB setA, stableRaftConfiguration setA]
      entries = [raftLogEntry (raftLogIndex index) (raftTerm 1) (Configuration configuration "opaque") | (index, configuration) <- zip [1 ..] configurations]
      rpc = checked (appendEntries (raftTerm 1) nodeA (raftLogIndex 0) (raftTerm 0) entries (raftLogIndex 4))
      demoted = fst (request nodeA rpc (learner nodeB [nodeA]))
      removed = fst (step (checked (updateRaftReplicationTargets [])) demoted)
  raftStateParticipation demoted @?= RaftLearner
  raftStateCanCampaign demoted @?= False
  raftStateCanVote demoted @?= False
  raftStateParticipation removed @?= RaftRemoved
  raftStateCommittedConfiguration removed @?= (checked (raftConfigurationEntryRef (raftLogIndex 4) (raftTerm 1)), stableRaftConfiguration setA)

malformedSequence :: Assertion
malformedSequence = do
  let stableA = stableRaftConfiguration (voters [nodeA])
      stableB = stableRaftConfiguration (voters [nodeB])
      jointAB = jointRaftConfiguration (voters [nodeA]) (voters [nodeA, nodeB])
      unchanged = jointRaftConfiguration (voters [nodeA]) (voters [nodeA])
      rpc :: [RaftVotingConfiguration] -> Either RaftInputFault (RaftRpc ByteString)
      rpc configurations = appendEntries (raftTerm 1) nodeA (raftLogIndex 0) (raftTerm 0) [raftLogEntry (raftLogIndex index) (raftTerm 1) (Configuration configuration "opaque") | (index, configuration) <- zip [1 ..] configurations] (raftLogIndex 0)
  mapM_ (\configurations -> assertBool "malformed transition is rejected before observation" (isLeft (rpc configurations))) [[stableA], [unchanged], [jointAB, jointAB], [jointAB, stableB]]

malformedPredecessor :: Assertion
malformedPredecessor = do
  let wrong = jointRaftConfiguration (voters [nodeA, nodeB]) (voters [nodeB])
      rpc = checked (appendEntries (raftTerm 1) nodeA (raftLogIndex 0) (raftTerm 0) [raftLogEntry (raftLogIndex 1) (raftTerm 1) (Configuration wrong "opaque")] (raftLogIndex 0))
  assertBool "self-contained RPC still needs its exact retained configuration predecessor" (isLeft (stepRaft (checked (observeRaftRequest nodeA rpc)) (initial nodeB [nodeA, nodeB, nodeC])))

orderedAcknowledgements :: Assertion
orderedAcknowledgements = do
  let joint = jointRaftConfiguration (voters [nodeA, nodeB, nodeC]) (voters [nodeA, nodeB])
      payloads = [LeaderNoOp, Application "command", Configuration joint "adapter"]
      rpc = checked (appendEntries (raftTerm 1) nodeA (raftLogIndex 0) (raftTerm 0) [raftLogEntry (raftLogIndex index) (raftTerm 1) payload | (index, payload) <- zip [1 ..] payloads] (raftLogIndex 3))
      (committed, effects) = request nodeA rpc (initial nodeB [nodeA, nodeB, nodeC])
      exposed = [entry | ExposeCommittedEntries entries <- effects, entry <- NE.toList entries]
  map committedEntryIndex exposed @?= map raftLogIndex [1, 2, 3]
  map committedEntryTerm exposed @?= replicate 3 (raftTerm 1)
  map committedEntryPayload exposed @?= payloads
  stepRaft (acknowledgeCommittedEntries (raftLogIndex 3)) committed @?= Left (RaftEntryAcknowledgementOutOfOrder (raftLogIndex 1) (raftLogIndex 3))
  stepRaft (acknowledgeCommittedEntries (raftLogIndex 4)) committed @?= Left (RaftEntryAcknowledgementAhead (raftLogIndex 4) (raftLogIndex 3))
  applied <- foldM (\state entry -> pure (fst (step (acknowledgeCommittedEntries (committedEntryIndex entry)) state))) committed exposed
  raftStateAppliedThrough applied @?= raftLogIndex 3
  step (acknowledgeCommittedEntries (raftLogIndex 3)) applied @?= (applied, [])

exactReadiness :: Assertion
exactReadiness = do
  let (pending, frontier, generation, ping) = prepared
      joined = jointRaftConfiguration (voters [nodeA]) (voters [nodeA, nodeB])
      propose = proposeRaftConfiguration proposal genesisRaftConfigurationRef joined "joint"
      prefix = checked (appendEntries (raftTerm 1) nodeA (raftLogIndex 0) (raftTerm 0) (filter ((> raftLogIndex 0) . raftLogEntryIndex) (raftStateLogEntries pending)) (raftLogIndex 1))
      blank = learner nodeB [nodeA]
      caughtUp = fst (request nodeA prefix blank)
      applied = fst (step (acknowledgeCommittedEntries (raftLogIndex 1)) caughtUp)
      token = fromJust (raftStateLearnerReadiness frontier applied)
      (reported, _) = step (observeRaftLearnerReadiness token) pending
      (_, acknowledgements) = request nodeA ping applied
      (_, ack) = sendTo nodeA acknowledgements
      (ready, _) = response nodeB generation ack reported
      (appended, _) = step propose ready
  raftStateLearnerReadiness frontier blank @?= Nothing
  raftStateLearnerReadiness frontier caughtUp @?= Nothing
  raftStateConfigurationChangeReady reported @?= False
  fst (step propose reported) @?= reported
  fst (step (proposeRaftApplication proposal "gated") reported) @?= reported
  raftStateConfigurationChangeReady ready @?= True
  raftStateLastLogIndex appended @?= raftLogIndex 2
  raftStateEffectiveConfiguration appended @?= (checked (raftConfigurationEntryRef (raftLogIndex 2) (raftTerm 1)), joined)
  raftStateCommittedConfiguration appended @?= raftStateCommittedConfiguration pending

cancelPreparation :: Assertion
cancelPreparation = do
  let (pending, _, _, _) = prepared
      (cancelled, effects) = step (cancelRaftConfigurationChange proposal) pending
  raftStateCatchUpFrontier cancelled @?= Nothing
  raftStateServiceReady cancelled @?= True
  raftStateLogEntries cancelled @?= raftStateLogEntries pending
  assertBool "exact cancellation reported" (ReportRaftProposal proposal RaftConfigurationCancelled `elem` effects)

staleReadiness :: Assertion
staleReadiness = do
  let (pending, frontier, _, _) = prepared
      token = fromJust (raftStateLearnerReadiness frontier pending)
      rpc = checked (appendEntries (raftTerm 2) nodeB (raftLogIndex 1) (raftTerm 1) [] (raftLogIndex 1))
      demoted = fst (request nodeB rpc pending)
  raftStateCatchUpFrontier demoted @?= Nothing
  fst (step (observeRaftLearnerReadiness token) demoted) @?= demoted

singletonGuard :: Assertion
singletonGuard = do
  let generation = raftStateRecentLeaderGeneration singleton
      (renewed, _) = step (fireHeartbeatTimer (raftStateHeartbeatGeneration singleton)) singleton
      (stale, _) = step (fireRecentLeaderTimer generation) renewed
  assertBool "fresh singleton self acknowledgement renews guard" (raftStateRecentLeaderGeneration renewed > generation)
  raftStateRecentLeaderGuardActive renewed @?= True
  stale @?= renewed

delayedOldGuardResponse :: Assertion
delayedOldGuardResponse = do
  let (candidate, votes) = campaign (initial nodeA [nodeA, nodeB, nodeC])
      (voteGeneration, voteRpc) = sendTo nodeB votes
      (voterB, voteEffects) = request nodeA voteRpc (initial nodeB [nodeA, nodeB, nodeC])
      (_, voteAck) = sendTo nodeA voteEffects
      (leader, appends) = response nodeB voteGeneration voteAck candidate
      (generationB, appendB) = sendTo nodeB appends
      (generationC, appendC) = sendTo nodeC appends
      (_, replyB) = request nodeA appendB voterB
      (_, replyC) = request nodeA appendC (initial nodeC [nodeA, nodeB, nodeC])
      (_, ackB) = sendTo nodeA replyB
      (_, ackC) = sendTo nodeA replyC
      (renewed, _) = response nodeB generationB ackB leader
      (delayed, _) = response nodeC generationC ackC renewed
  raftStateRecentLeaderGuardActive renewed @?= True
  raftStateRecentLeaderGeneration delayed @?= raftStateRecentLeaderGeneration renewed

partialWindowEffects :: Assertion
partialWindowEffects = do
  let nodes = [nodeA, nodeB, nodeC, nodeId 4, nodeId 5]
      (candidate, votes) = campaign (initial nodeA nodes)
      (generationB, voteB) = sendTo nodeB votes
      (generationC, voteC) = sendTo nodeC votes
      (voterB, replyB) = request nodeA voteB (initial nodeB nodes)
      (_, replyC) = request nodeA voteC (initial nodeC nodes)
      (_, ackB) = sendTo nodeA replyB
      (_, ackC) = sendTo nodeA replyC
      (candidateTwo, _) = response nodeB generationB ackB candidate
      (leader, appends) = response nodeC generationC ackC candidateTwo
      (appendGeneration, appendB) = sendTo nodeB appends
      (_, appendReply) = request nodeA appendB voterB
      (_, appendAck) = sendTo nodeA appendReply
      (partial, effects) = response nodeB appendGeneration appendAck leader
      generation = raftStateRecentLeaderGeneration partial
      (expired, expiration) = step (fireRecentLeaderTimer generation) partial
  raftStateRecentLeaderGuardActive partial @?= False
  assertBool "insufficient quorum still has a bounded lifetime" (ArmRecentLeaderTimer generation electionLower `elem` effects)
  expiration @?= [SupersedeRecentLeaderTimer generation]
  raftStateRecentLeaderGuardActive expired @?= False
  step (fireRecentLeaderTimer generation) expired @?= (expired, [])
