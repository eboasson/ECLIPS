{-# LANGUAGE OverloadedStrings #-}

module TransitionProperties
  ( tests,
  )
where

import Data.ByteString (ByteString)
import Eclips.Raft.Effect
  ( RaftEffect (..),
    RaftProposalStatus (..),
    raftEffectBatchEffects,
  )
import Eclips.Raft.Identity
  ( RaftDispatchGeneration,
    RaftElectionTimerGeneration,
    RaftHeartbeatTimerGeneration,
    RaftNodeId,
    RaftProposalId,
    RaftTerm,
    mkRaftProposalId,
    raftDispatchGeneration,
    raftElectionTimerGeneration,
    raftHeartbeatTimerGeneration,
    raftHeartbeatTimerGenerationWord64,
    raftLogIndex,
    raftTerm,
  )
import Eclips.Raft.Input
  ( RaftAppendResult (..),
    RaftEntry (..),
    RaftInput,
    RaftInputFault (..),
    RaftRpc,
    RaftRpcView (..),
    acknowledgeCommittedEntries,
    appendEntries,
    appendEntriesResponse,
    conflictingTermHint,
    fireElectionTimer,
    fireHeartbeatTimer,
    fireRecentLeaderTimer,
    observeRaftRequest,
    observeRaftResponse,
    proposeRaftApplication,
    raftLogEntry,
    raftRpcView,
    requestVote,
    requestVoteResponse,
    selectElectionTimeout,
  )
import Eclips.Raft.State
  ( RaftRole (..),
    RaftState,
    raftStateCommitIndex,
    raftStateElectionGeneration,
    raftStateHeartbeatGeneration,
    raftStateLastLogIndex,
    raftStateLeaderHint,
    raftStatePendingProposal,
    raftStateRecentLeaderGeneration,
    raftStateRole,
    raftStateTerm,
  )
import Eclips.Raft.Transition
  ( RaftFault (..),
    initialRaft,
    initialRaftEffects,
    stepRaft,
  )
import GenesisProperties
  ( checkedGenesisFor,
    electionLower,
    electionUpper,
    heartbeat,
    nodeId,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    testCase,
    (@?=),
  )

tests :: TestTree
tests =
  testGroup
    "focused transition behavior"
    [ testCase "initial and election timer generations are explicit" electionTimerCase,
      testCase "heartbeat timer generations reject stale firings" heartbeatTimerCase,
      testCase "round-trip time beyond heartbeat cadence still progresses" heartbeatResponseProgressCase,
      testCase "RPC constructors and source observations enforce shape" rpcAdmissionCase,
      testCase "a higher term steps a leader down" higherTermStepDownCase,
      testCase "denied higher-term stale votes preserve an active election deadline" deniedStaleVotePreservesDeadlineCase,
      testCase "the preserved deadline elects the up-to-date voter" preservedDeadlineElectsUpToDateVoterCase,
      testCase "denied higher-term stale votes arm a demoted leader" deniedStaleVoteArmsDemotedLeaderCase,
      testCase "newer AppendEntries replaces the leader hint" leaderHintReplacementCase,
      testCase "followers redirect and unready leaders reject transiently" proposalDispositionCase,
      testCase "only one unresolved application proposal is admitted" unresolvedProposalCase
    ]

type Payload = ByteString

nodeA, nodeB, nodeC :: RaftNodeId
nodeA = nodeId 1
nodeB = nodeId 2
nodeC = nodeId 3

initialState :: RaftNodeId -> RaftState Payload
initialState node = initialRaft (checkedGenesisFor node)

electionTimerCase :: Assertion
electionTimerCase = do
  let initial = initialState nodeA
      generation = raftStateElectionGeneration initial
      staleGeneration = raftElectionTimerGeneration 0
  raftEffectBatchEffects (initialRaftEffects initial)
    @?= [RequestElectionTimeout generation]
  (selected, selectionEffects) <- transition (selectElectionTimeout generation electionLower) initial
  selectionEffects @?= [ArmElectionTimer generation electionLower]
  (sameSelection, repeatedEffects) <-
    transition (selectElectionTimeout generation electionLower) selected
  sameSelection @?= selected
  repeatedEffects @?= []
  stepRaft (selectElectionTimeout generation electionUpper) selected
    @?= Left
      ( RaftConflictingElectionTimeoutSelection
          generation
          electionLower
          electionUpper
      )
  stepRaft (selectElectionTimeout generation heartbeat) initial
    @?= Left
      ( RaftElectionTimeoutOutsideGenesis
          heartbeat
          electionLower
          electionUpper
      )
  assertNoOp (selectElectionTimeout staleGeneration electionLower) selected
  assertNoOp (fireElectionTimer staleGeneration) selected
  assertNoOp (fireElectionTimer generation) initial
  (candidate, _) <- transition (fireElectionTimer generation) selected
  raftStateRole candidate @?= RaftCandidate
  raftStateTerm candidate @?= raftTerm 1
  assertNoOp (fireElectionTimer generation) candidate

heartbeatTimerCase :: Assertion
heartbeatTimerCase = do
  (leader, _, _) <- electLeader
  let generation = raftStateHeartbeatGeneration leader
      staleGeneration = raftHeartbeatTimerGeneration 0
  assertNoOp (fireHeartbeatTimer staleGeneration) leader
  (rearmed, effects) <- transition (fireHeartbeatTimer generation) leader
  raftStateHeartbeatGeneration rearmed @?= successorHeartbeatGeneration generation
  case reverse effects of
    ArmHeartbeatTimer armed duration : _ -> do
      armed @?= successorHeartbeatGeneration generation
      duration @?= heartbeat
    other -> assertFailure ("heartbeat was not rearmed after its RPC batch: " <> show other)

heartbeatResponseProgressCase :: Assertion
heartbeatResponseProgressCase = do
  (leader, voter, electionEffects) <- electLeader
  (originalGeneration, originalAppend) <-
    requireSend nodeB isAppendEntries electionEffects
  (afterFirstHeartbeat, firstHeartbeatEffects) <-
    transition
      (fireHeartbeatTimer (raftStateHeartbeatGeneration leader))
      leader
  (firstRetryGeneration, _) <-
    requireSend nodeB isAppendEntries firstHeartbeatEffects
  firstRetryGeneration @?= originalGeneration
  (afterSecondHeartbeat, secondHeartbeatEffects) <-
    transition
      (fireHeartbeatTimer (raftStateHeartbeatGeneration afterFirstHeartbeat))
      afterFirstHeartbeat
  (secondRetryGeneration, _) <-
    requireSend nodeB isAppendEntries secondHeartbeatEffects
  secondRetryGeneration @?= originalGeneration
  appendInput <-
    checked "delayed no-op append" (observeRaftRequest nodeA originalAppend)
  (_, responseEffects) <- transition appendInput voter
  (_, response) <- requireSend nodeA isAppendEntriesResponse responseEffects
  responseInput <-
    checked
      "delayed no-op response"
      (observeRaftResponse nodeB originalGeneration response)
  (progressed, _) <- transition responseInput afterSecondHeartbeat
  raftStateCommitIndex progressed @?= raftLogIndex 1

rpcAdmissionCase :: Assertion
rpcAdmissionCase = do
  ( requestVote (raftTerm 0) nodeA (raftLogIndex 0) (raftTerm 0) ::
      Either RaftInputFault (RaftRpc Payload)
    )
    @?= Left RaftRpcTermIsZero
  ( requestVote (raftTerm 1) nodeA (raftLogIndex 1) (raftTerm 0) ::
      Either RaftInputFault (RaftRpc Payload)
    )
    @?= Left (RaftLogIndexTermShapeMismatch (raftLogIndex 1) (raftTerm 0))
  ( requestVote (raftTerm 1) nodeA (raftLogIndex 1) (raftTerm 2) ::
      Either RaftInputFault (RaftRpc Payload)
    )
    @?= Left (RaftLastLogTermAfterRequestTerm (raftTerm 2) (raftTerm 1))
  ( appendEntries
      (raftTerm 1)
      nodeA
      (raftLogIndex 1)
      (raftTerm 2)
      []
      (raftLogIndex 0) ::
      Either RaftInputFault (RaftRpc Payload)
    )
    @?= Left (RaftPreviousLogTermAfterRequestTerm (raftTerm 2) (raftTerm 1))
  ( appendEntries
      (raftTerm 1)
      nodeA
      (raftLogIndex 0)
      (raftTerm 0)
      [raftLogEntry (raftLogIndex 1) (raftTerm 2) LeaderNoOp]
      (raftLogIndex 0) ::
      Either RaftInputFault (RaftRpc Payload)
    )
    @?= Left
      ( RaftLogEntryTermAfterRequestTerm
          (raftLogIndex 1)
          (raftTerm 2)
          (raftTerm 1)
      )
  ( appendEntries
      (raftTerm 2)
      nodeA
      (raftLogIndex 0)
      (raftTerm 0)
      [ raftLogEntry (raftLogIndex 1) (raftTerm 2) LeaderNoOp,
        raftLogEntry (raftLogIndex 2) (raftTerm 1) LeaderNoOp
      ]
      (raftLogIndex 0) ::
      Either RaftInputFault (RaftRpc Payload)
    )
    @?= Left
      ( RaftLogEntryTermRegression
          (raftLogIndex 2)
          (raftTerm 2)
          (raftTerm 1)
      )
  conflict <-
    checked
      "conflict hint"
      (conflictingTermHint (raftTerm 2) (raftLogIndex 1))
  ( appendEntriesResponse
      (raftTerm 1)
      nodeB
      (AppendRejected conflict) ::
      Either RaftInputFault (RaftRpc Payload)
    )
    @?= Left (RaftConflictTermAfterResponseTerm (raftTerm 2) (raftTerm 1))
  ( appendEntries
      (raftTerm 1)
      nodeA
      (raftLogIndex 0)
      (raftTerm 0)
      [raftLogEntry (raftLogIndex 0) (raftTerm 1) LeaderNoOp]
      (raftLogIndex 0) ::
      Either RaftInputFault (RaftRpc Payload)
    )
    @?= Left RaftLogEntryIndexIsZero
  ( appendEntries
      (raftTerm 1)
      nodeA
      (raftLogIndex 0)
      (raftTerm 0)
      [raftLogEntry (raftLogIndex 1) (raftTerm 0) LeaderNoOp]
      (raftLogIndex 0) ::
      Either RaftInputFault (RaftRpc Payload)
    )
    @?= Left (RaftLogEntryTermIsZero (raftLogIndex 1))
  ( appendEntries
      (raftTerm 1)
      nodeA
      (raftLogIndex 0)
      (raftTerm 0)
      [raftLogEntry (raftLogIndex 2) (raftTerm 1) LeaderNoOp]
      (raftLogIndex 0) ::
      Either RaftInputFault (RaftRpc Payload)
    )
    @?= Left
      (RaftLogEntriesNotContiguous (raftLogIndex 1) (raftLogIndex 2))
  ( appendEntries
      (raftTerm 1)
      nodeA
      (raftLogIndex 0)
      (raftTerm 0)
      []
      (raftLogIndex 1) ::
      Either RaftInputFault (RaftRpc Payload)
    )
    @?= Left
      (RaftLeaderCommitAfterSentPrefix (raftLogIndex 1) (raftLogIndex 0))
  request <-
    checked
      "request vote"
      (requestVote (raftTerm 1) nodeB (raftLogIndex 0) (raftTerm 0))
  observeRaftRequest nodeC (request :: RaftRpc Payload)
    @?= Left (RaftRpcSourceMismatch nodeC nodeB)
  response <- checked "vote response" (requestVoteResponse (raftTerm 1) nodeB True)
  observeRaftRequest nodeB (response :: RaftRpc Payload)
    @?= Left RaftExpectedRequestObservation
  observeRaftResponse nodeC (raftDispatchGeneration 1) response
    @?= Left (RaftRpcSourceMismatch nodeC nodeB)
  observeRaftResponse nodeB (raftDispatchGeneration 1) request
    @?= Left RaftExpectedResponseObservation
  localRequest <-
    checked
      "local request vote"
      (requestVote (raftTerm 1) nodeA (raftLogIndex 0) (raftTerm 0))
  localInput <- checked "local request observation" (observeRaftRequest nodeA localRequest)
  stepRaft localInput (initialState nodeA)
    @?= Left RaftRpcFromLocalNode
  let unconfigured = nodeId 4
  unconfiguredRequest <-
    checked
      "unconfigured request vote"
      (requestVote (raftTerm 1) unconfigured (raftLogIndex 0) (raftTerm 0))
  unconfiguredInput <-
    checked
      "unconfigured request observation"
      (observeRaftRequest unconfigured unconfiguredRequest)
  (admitted, _) <- transition unconfiguredInput (initialState nodeA)
  raftStateTerm admitted @?= raftTerm 1

higherTermStepDownCase :: Assertion
higherTermStepDownCase = do
  (leader, _, sent) <- electLeader
  (generation, _) <- requireSend nodeC isAppendEntries sent
  response <-
    checked
      "higher-term append response"
      (appendEntriesResponse (raftTerm 2) nodeC (AppendAccepted (raftLogIndex 1) (raftLogIndex 0)))
  input <-
    checked
      "higher-term response observation"
      (observeRaftResponse nodeC generation (response :: RaftRpc Payload))
  (follower, effects) <- transition input leader
  raftStateRole follower @?= RaftFollower
  raftStateTerm follower @?= raftTerm 2
  raftStateLeaderHint follower @?= Nothing
  case effects of
    SupersedeHeartbeatTimer _ : RequestElectionTimeout _ : _ -> pure ()
    other -> assertFailure ("higher-term step-down effects were incomplete: " <> show other)

deniedStaleVotePreservesDeadlineCase :: Assertion
deniedStaleVotePreservesDeadlineCase = do
  (_, follower) <- replicatedFollower
  let generation = raftStateElectionGeneration follower
  (selected, selectionEffects) <-
    transition (selectElectionTimeout generation electionUpper) follower
  selectionEffects @?= [ArmElectionTimer generation electionUpper]
  afterSecond <- denyHigherTermStaleVote generation (raftTerm 2) selected
  afterThird <- denyHigherTermStaleVote generation (raftTerm 3) afterSecond
  afterFourth <- denyHigherTermStaleVote generation (raftTerm 4) afterThird
  raftStateElectionGeneration afterFourth @?= generation

preservedDeadlineElectsUpToDateVoterCase :: Assertion
preservedDeadlineElectsUpToDateVoterCase = do
  (leaderA, followerB) <- replicatedFollower
  let preservedGeneration = raftStateElectionGeneration followerB
  (selectedB, _) <-
    transition
      (selectElectionTimeout preservedGeneration electionUpper)
      followerB
  afterSecond <- denyHigherTermStaleVote preservedGeneration (raftTerm 2) selectedB
  afterThird <- denyHigherTermStaleVote preservedGeneration (raftTerm 3) afterSecond
  afterFourth <- denyHigherTermStaleVote preservedGeneration (raftTerm 4) afterThird
  (candidateB, electionEffects) <-
    transition (fireElectionTimer preservedGeneration) afterFourth
  raftStateRole candidateB @?= RaftCandidate
  raftStateTerm candidateB @?= raftTerm 5
  (voteGeneration, voteRequest) <- requireSend nodeA isRequestVote electionEffects
  case raftRpcView voteRequest of
    RequestVoteView term candidate lastIndex lastTerm -> do
      term @?= raftTerm 5
      candidate @?= nodeB
      lastIndex @?= raftLogIndex 1
      lastTerm @?= raftTerm 1
    other -> assertFailure ("expected up-to-date vote request, got " <> show other)
  voteInput <- checked "preserved-deadline vote request" (observeRaftRequest nodeB voteRequest)
  (followerA, responseEffects) <- transition voteInput leaderA
  raftStateRole followerA @?= RaftFollower
  raftStateTerm followerA @?= raftTerm 5
  (_, voteResponse) <- requireSend nodeB isVoteResponse responseEffects
  case raftRpcView voteResponse of
    RequestVoteResponseView term voter granted -> do
      term @?= raftTerm 5
      voter @?= nodeA
      granted @?= True
    other -> assertFailure ("expected granted vote response, got " <> show other)
  responseInput <-
    checked
      "preserved-deadline vote response"
      (observeRaftResponse nodeA voteGeneration voteResponse)
  (leaderB, _) <- transition responseInput candidateB
  raftStateRole leaderB @?= RaftLeader
  raftStateTerm leaderB @?= raftTerm 5

deniedStaleVoteArmsDemotedLeaderCase :: Assertion
deniedStaleVoteArmsDemotedLeaderCase = do
  (leader, _, _) <- electLeader
  let leaderGeneration = raftStateElectionGeneration leader
  staleRequest <-
    checked
      "higher-term stale leader vote request"
      (requestVote (raftTerm 2) nodeC (raftLogIndex 0) (raftTerm 0))
  staleInput <-
    checked
      "higher-term stale leader vote observation"
      (observeRaftRequest nodeC staleRequest)
  (follower, effects) <- transition staleInput leader
  let followerGeneration = raftStateElectionGeneration follower
  raftStateRole follower @?= RaftFollower
  raftStateTerm follower @?= raftTerm 2
  (followerGeneration == leaderGeneration) @?= False
  electionTimerEffects effects @?= [RequestElectionTimeout followerGeneration]
  (_, response) <- requireSend nodeC isVoteResponse effects
  case raftRpcView response of
    RequestVoteResponseView term _ granted -> do
      term @?= raftTerm 2
      granted @?= False
    other -> assertFailure ("expected denied vote response, got " <> show other)
  (_, selectionEffects) <-
    transition
      (selectElectionTimeout followerGeneration electionLower)
      follower
  selectionEffects @?= [ArmElectionTimer followerGeneration electionLower]

leaderHintReplacementCase :: Assertion
leaderHintReplacementCase = do
  first <- heartbeatRequest (raftTerm 1) nodeB
  firstInput <- checked "first AppendEntries" (observeRaftRequest nodeB first)
  (followingB, _) <- transition firstInput (initialState nodeA)
  raftStateLeaderHint followingB @?= Just nodeB
  second <- heartbeatRequest (raftTerm 2) nodeC
  secondInput <- checked "second AppendEntries" (observeRaftRequest nodeC second)
  (followingC, _) <- transition secondInput followingB
  raftStateRole followingC @?= RaftFollower
  raftStateTerm followingC @?= raftTerm 2
  raftStateLeaderHint followingC @?= Just nodeC

proposalDispositionCase :: Assertion
proposalDispositionCase = do
  let proposal = proposalId 1
      follower = initialState nodeA
  (unchangedFollower, followerEffects) <-
    transition (proposeRaftApplication proposal "payload") follower
  unchangedFollower @?= follower
  followerEffects
    @?= [ReportRaftProposal proposal (RaftProposalRedirected Nothing (raftTerm 0))]
  (leader, _, _) <- electLeader
  (unchangedLeader, leaderEffects) <-
    transition (proposeRaftApplication proposal "payload") leader
  unchangedLeader @?= leader
  leaderEffects
    @?= [ReportRaftProposal proposal (RaftProposalNotReady (raftTerm 1))]

unresolvedProposalCase :: Assertion
unresolvedProposalCase = do
  (ready, _) <- readyLeader
  let firstProposal = proposalId 1
      secondProposal = proposalId 2
  (pending, firstEffects) <-
    transition (proposeRaftApplication firstProposal "first") ready
  raftStatePendingProposal pending @?= Just (firstProposal, raftLogIndex 2)
  raftStateLastLogIndex pending @?= raftLogIndex 2
  case firstEffects of
    ReportRaftProposal reported (RaftProposalAppended index) : _ -> do
      reported @?= firstProposal
      index @?= raftLogIndex 2
    other -> assertFailure ("first proposal was not appended before replication: " <> show other)
  (stillPending, secondEffects) <-
    transition (proposeRaftApplication secondProposal "second") pending
  stillPending @?= pending
  raftStateLastLogIndex stillPending @?= raftLogIndex 2
  secondEffects
    @?= [ReportRaftProposal secondProposal (RaftProposalNotReady (raftTerm 1))]

-- The candidate, voter, and effect batch produced on becoming leader.  Returning
-- the complete successor states keeps the test harness faithful to the runtime's
-- install-before-effect rule.
electLeader :: IO (RaftState Payload, RaftState Payload, [RaftEffect Payload])
electLeader = do
  let followerA = initialState nodeA
      followerB = initialState nodeB
      generation = raftStateElectionGeneration followerA
  (selected, _) <- transition (selectElectionTimeout generation electionLower) followerA
  (candidate, voteEffects) <- transition (fireElectionTimer generation) selected
  (voteGeneration, voteRequest) <- requireSend nodeB isRequestVote voteEffects
  voteInput <- checked "observed vote request" (observeRaftRequest nodeA voteRequest)
  (voter, responseEffects) <- transition voteInput followerB
  (_, voteResponse) <- requireSend nodeA isVoteResponse responseEffects
  responseInput <-
    checked
      "observed vote response"
      (observeRaftResponse nodeB voteGeneration voteResponse)
  (leader, leaderEffects) <- transition responseInput candidate
  raftStateRole leader @?= RaftLeader
  pure (leader, voter, leaderEffects)

readyLeader :: IO (RaftState Payload, [RaftEffect Payload])
readyLeader = do
  (leader, voter, leaderEffects) <- electLeader
  (appendGeneration, appendRequest) <- requireSend nodeB isAppendEntries leaderEffects
  appendInput <- checked "observed no-op append" (observeRaftRequest nodeA appendRequest)
  (_, responseEffects) <- transition appendInput voter
  (_, appendResponse) <- requireSend nodeA isAppendEntriesResponse responseEffects
  responseInput <-
    checked
      "observed no-op append response"
      (observeRaftResponse nodeB appendGeneration appendResponse)
  (committed, readyEffects) <- transition responseInput leader
  (ready, _) <- transition (acknowledgeCommittedEntries (raftLogIndex 1)) committed
  pure (ready, readyEffects)

replicatedFollower :: IO (RaftState Payload, RaftState Payload)
replicatedFollower = do
  (leader, voter, leaderEffects) <- electLeader
  (_, appendRequest) <- requireSend nodeB isAppendEntries leaderEffects
  appendInput <- checked "replicated follower append" (observeRaftRequest nodeA appendRequest)
  (contacted, _) <- transition appendInput voter
  (follower, _) <- transition (fireRecentLeaderTimer (raftStateRecentLeaderGeneration contacted)) contacted
  raftStateLastLogIndex follower @?= raftLogIndex 1
  pure (leader, follower)

denyHigherTermStaleVote ::
  RaftElectionTimerGeneration ->
  RaftTerm ->
  RaftState Payload ->
  IO (RaftState Payload)
denyHigherTermStaleVote expectedGeneration term state = do
  staleRequest <-
    checked
      "higher-term stale vote request"
      (requestVote term nodeC (raftLogIndex 0) (raftTerm 0))
  staleInput <-
    checked
      "higher-term stale vote observation"
      (observeRaftRequest nodeC staleRequest)
  (successor, effects) <- transition staleInput state
  raftStateRole successor @?= RaftFollower
  raftStateTerm successor @?= term
  raftStateElectionGeneration successor @?= expectedGeneration
  electionTimerEffects effects @?= []
  (_, response) <- requireSend nodeC isVoteResponse effects
  case raftRpcView response of
    RequestVoteResponseView responseTerm _ granted -> do
      responseTerm @?= term
      granted @?= False
    other -> assertFailure ("expected denied stale-log vote response, got " <> show other)
  pure successor

electionTimerEffects :: [RaftEffect bytes] -> [RaftEffect bytes]
electionTimerEffects = filter isElectionTimerEffect
  where
    isElectionTimerEffect effect = case effect of
      RequestElectionTimeout _ -> True
      ArmElectionTimer _ _ -> True
      SupersedeElectionTimer _ -> True
      _ -> False

heartbeatRequest ::
  RaftTerm ->
  RaftNodeId ->
  IO (RaftRpc Payload)
heartbeatRequest term leader =
  checked
    "heartbeat AppendEntries"
    ( appendEntries
        term
        leader
        (raftLogIndex 0)
        (raftTerm 0)
        []
        (raftLogIndex 0)
    )

transition ::
  RaftInput Payload ->
  RaftState Payload ->
  IO (RaftState Payload, [RaftEffect Payload])
transition input state = case stepRaft input state of
  Left problem -> assertFailure (show problem)
  Right (successor, batch) -> pure (successor, raftEffectBatchEffects batch)

assertNoOp :: RaftInput Payload -> RaftState Payload -> Assertion
assertNoOp input state = case stepRaft input state of
  Left problem -> assertFailure (show problem)
  Right (successor, batch) -> do
    successor @?= state
    raftEffectBatchEffects batch @?= []

requireSend ::
  RaftNodeId ->
  (RaftRpc Payload -> Bool) ->
  [RaftEffect Payload] ->
  IO (RaftDispatchGeneration, RaftRpc Payload)
requireSend target predicate effects =
  case [ (generation, rpc)
       | SendRaftRpc destination generation rpc <- effects,
         destination == target,
         predicate rpc
       ] of
    result : _ -> pure result
    [] -> assertFailure ("missing Raft RPC to " <> show target <> " in " <> show effects)

isRequestVote :: RaftRpc Payload -> Bool
isRequestVote rpc = case raftRpcView rpc of
  RequestVoteView {} -> True
  _ -> False

isVoteResponse :: RaftRpc Payload -> Bool
isVoteResponse rpc = case raftRpcView rpc of
  RequestVoteResponseView {} -> True
  _ -> False

isAppendEntries :: RaftRpc Payload -> Bool
isAppendEntries rpc = case raftRpcView rpc of
  AppendEntriesView {} -> True
  _ -> False

isAppendEntriesResponse :: RaftRpc Payload -> Bool
isAppendEntriesResponse rpc = case raftRpcView rpc of
  AppendEntriesResponseView {} -> True
  _ -> False

proposalId :: Word -> RaftProposalId
proposalId value =
  either
    (error . (("proposal ID: " <>) . show))
    id
    (mkRaftProposalId (fromIntegral value))

successorHeartbeatGeneration ::
  RaftHeartbeatTimerGeneration ->
  RaftHeartbeatTimerGeneration
successorHeartbeatGeneration generation =
  raftHeartbeatTimerGeneration
    (raftHeartbeatTimerGenerationWord64 generation + 1)

checked :: (Show problem) => String -> Either problem value -> IO value
checked label = either (assertFailure . ((label <> ": ") <>) . show) pure
