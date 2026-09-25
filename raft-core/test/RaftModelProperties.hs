{-# LANGUAGE OverloadedStrings #-}

module RaftModelProperties
  ( tests,
  )
where

import Control.Monad (foldM)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
  ( newEmptyMVar,
    newMVar,
    putMVar,
    takeMVar,
    tryReadMVar,
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Eclips.Raft.Effect
  ( RaftCommittedEntry,
    RaftEffect (..),
    RaftEffectBatch,
    RaftProposalStatus (..),
    committedEntryIndex,
    committedEntryPayload,
    raftEffectBatchEffects,
  )
import Eclips.Raft.Identity
  ( RaftDispatchGeneration,
    RaftLogIndex,
    RaftNodeId,
    RaftProposalId,
    RaftTerm,
    mkRaftProposalId,
    raftLogIndex,
    raftLogIndexWord64,
    raftTerm,
  )
import Eclips.Raft.Input
  ( RaftAppendResult (..),
    RaftConflictHintView (..),
    RaftEntry (..),
    RaftInput,
    RaftRpc,
    RaftRpcView (..),
    acknowledgeCommittedEntries,
    appendEntries,
    fireElectionTimer,
    fireRecentLeaderTimer,
    observeRaftRequest,
    observeRaftResponse,
    proposeRaftApplication,
    raftConflictHintView,
    raftLogEntry,
    raftLogEntryIndex,
    raftLogEntryPayload,
    raftLogEntryTerm,
    raftRpcView,
    requestVote,
    requestVoteResponse,
    selectElectionTimeout,
  )
import Eclips.Raft.State
  ( RaftRole (..),
    RaftState,
    raftStateAppliedThrough,
    raftStateCommitIndex,
    raftStateElectionGeneration,
    raftStateLastLogIndex,
    raftStateLogEntries,
    raftStateMatchIndex,
    raftStateRecentLeaderGeneration,
    raftStateRole,
    raftStateServiceReady,
    raftStateTerm,
    raftStateVotedFor,
  )
import Eclips.Raft.Transition
  ( RaftFault (RaftLogInvariantFault),
    initialRaft,
    stepRaft,
  )
import GenesisProperties
  ( checkedGenesisFor,
    electionLower,
    nodeId,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )
import Test.Tasty.QuickCheck
  ( Property,
    chooseInt,
    counterexample,
    forAll,
    property,
    testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "deterministic network model"
    [ testCase "1 election safety" electionSafetyCase,
      testCase "2 vote-once and log-up-to-date rules" voteAndLogRuleCase,
      testCase "3 log matching and committed-prefix immutability" committedPrefixCase,
      testCase "4 leader completeness across elections" leaderCompletenessCase,
      testCase "5 current-term commit restriction" currentTermCommitCase,
      testCase "6 uncommitted old-leader suffix is lost" oldSuffixLossCase,
      testCase "7 conflict hints direct suffix repair" conflictRepairCase,
      testCase "8 fragmented, delayed, duplicated, and reordered RPC dispatch is idempotent" duplicateRpcCase,
      testCase "9 majority progresses while a minority cannot" majorityPartitionCase,
      testCase "10 current-term no-op gates service" noOpReadinessCase,
      testProperty "11 application-byte meaning is opaque" opaquePayloadProperty,
      testCase "12 successor installation precedes batch handoff" installBeforeHandoffCase,
      testCase "13 dispatcher delay loses or duplicates no work" dispatcherDelayCase,
      testCase "14 equal committed prefixes expose equal byte order" equalPrefixExposureCase
    ]

type Payload = ByteString

data Packet = Packet
  { packetFrom :: RaftNodeId,
    packetTo :: RaftNodeId,
    packetDispatch :: RaftDispatchGeneration,
    packetResponseTo :: Maybe RaftDispatchGeneration,
    packetRpc :: RaftRpc Payload
  }
  deriving stock (Eq, Show)

data Model = Model
  { modelStates :: Map RaftNodeId (RaftState Payload),
    modelPackets :: [Packet],
    modelCommitted :: Map RaftNodeId [RaftCommittedEntry Payload],
    modelProposals :: [(RaftNodeId, RaftProposalId, RaftProposalStatus)]
  }
  deriving stock (Eq, Show)

emptyModel :: Model
emptyModel =
  Model
    { modelStates =
        Map.fromList
          [ (node, initialRaft (checkedGenesisFor node))
          | node <- clusterNodes
          ],
      modelPackets = [],
      modelCommitted = Map.empty,
      modelProposals = []
    }

clusterNodes :: [RaftNodeId]
clusterNodes = map nodeId [1, 2, 3]

nodeA, nodeB, nodeC :: RaftNodeId
nodeA = nodeId 1
nodeB = nodeId 2
nodeC = nodeId 3

stepModel :: RaftNodeId -> RaftInput Payload -> Model -> Either String Model
stepModel node = stepModelWithReply node Nothing

stepModelWithReply ::
  RaftNodeId ->
  Maybe (RaftNodeId, RaftDispatchGeneration) ->
  RaftInput Payload ->
  Model ->
  Either String Model
stepModelWithReply node replyContext input model = do
  predecessor <- lookupState node model
  (successor, batch) <- mapProblem "stepRaft" (stepRaft input predecessor)
  let installed =
        model
          { modelStates = Map.insert node successor (modelStates model)
          }
  pure
    ( foldl'
        (interpretEffect node replyContext)
        installed
        (raftEffectBatchEffects batch)
    )

interpretEffect ::
  RaftNodeId ->
  Maybe (RaftNodeId, RaftDispatchGeneration) ->
  Model ->
  RaftEffect Payload ->
  Model
interpretEffect source replyContext model effect = case effect of
  SendRaftRpc target generation rpc ->
    let correlation = case (replyContext, raftRpcView rpc) of
          (Just (requestSource, requestGeneration), RequestVoteResponseView {})
            | target == requestSource -> Just requestGeneration
          (Just (requestSource, requestGeneration), AppendEntriesResponseView {})
            | target == requestSource -> Just requestGeneration
          _ -> Nothing
        packet = Packet source target generation correlation rpc
     in model {modelPackets = modelPackets model <> [packet]}
  ReportRaftProposal proposal status ->
    model
      { modelProposals = modelProposals model <> [(source, proposal, status)]
      }
  ExposeCommittedEntries entries ->
    model
      { modelCommitted =
          Map.alter
            (Just . (<> filter isApplication (NonEmpty.toList entries)) . maybe [] id)
            source
            (modelCommitted model)
      }
  _ -> model

deliverLatest :: (Packet -> Bool) -> Model -> Either String Model
deliverLatest predicate model = do
  (packet, remaining) <- takeLatest predicate (modelPackets model)
  input <- packetInput packet
  let replyContext = case raftRpcView (packetRpc packet) of
        RequestVoteView {} -> Just (packetFrom packet, packetDispatch packet)
        AppendEntriesView {} -> Just (packetFrom packet, packetDispatch packet)
        _ -> Nothing
  stepModelWithReply
    (packetTo packet)
    replyContext
    input
    (model {modelPackets = remaining})

packetInput :: Packet -> Either String (RaftInput Payload)
packetInput packet = case raftRpcView (packetRpc packet) of
  RequestVoteView {} ->
    mapProblem "observe request vote" (observeRaftRequest (packetFrom packet) (packetRpc packet))
  AppendEntriesView {} ->
    mapProblem "observe append" (observeRaftRequest (packetFrom packet) (packetRpc packet))
  InstallSnapshotView {} ->
    mapProblem "observe snapshot" (observeRaftRequest (packetFrom packet) (packetRpc packet))
  RequestVoteResponseView {} -> responseInput
  AppendEntriesResponseView {} -> responseInput
  InstallSnapshotResponseView {} -> responseInput
  where
    responseInput = case packetResponseTo packet of
      Nothing -> Left "response packet has no matching local dispatch generation"
      Just generation ->
        mapProblem
          "observe response"
          (observeRaftResponse (packetFrom packet) generation (packetRpc packet))

takeLatest :: (value -> Bool) -> [value] -> Either String (value, [value])
takeLatest predicate values = do
  (selected, reversedRemaining) <- takeFirst predicate (reverse values)
  pure (selected, reverse reversedRemaining)

takeFirst :: (value -> Bool) -> [value] -> Either String (value, [value])
takeFirst _ [] = Left "no matching packet"
takeFirst predicate (value : remaining)
  | predicate value = Right (value, remaining)
  | otherwise = do
      (selected, rest) <- takeFirst predicate remaining
      pure (selected, value : rest)

startElection :: RaftNodeId -> Model -> Either String Model
startElection node model = do
  state <- lookupState node model
  selected <-
    stepModel
      node
      (selectElectionTimeout (raftStateElectionGeneration state) electionLower)
      model
  stepModel
    node
    (fireElectionTimer (raftStateElectionGeneration state))
    selected

electWithVote :: RaftNodeId -> RaftNodeId -> Model -> Either String Model
electWithVote candidate voter model = do
  started <- startElection candidate model
  requested <- deliverLatest (isRequestVote candidate voter) started
  deliverLatest (isVoteResponse voter candidate) requested

readyLeader :: RaftNodeId -> RaftNodeId -> Model -> Either String Model
readyLeader leader voter model = do
  elected <- electWithVote leader voter model
  appended <- deliverLatest (isAppend leader voter) elected
  committed <- deliverLatest (isAppendResponse voter leader) appended
  state <- lookupState leader committed
  acknowledgeAt leader (raftStateCommitIndex state) committed

proposeAndCommit ::
  RaftNodeId ->
  RaftNodeId ->
  RaftProposalId ->
  Payload ->
  Model ->
  Either String Model
proposeAndCommit leader voter proposal payload model = do
  proposed <- stepModel leader (proposeRaftApplication proposal payload) model
  appended <- deliverLatest (isAppend leader voter) proposed
  deliverLatest (isAppendResponse voter leader) appended

acknowledgeAt :: RaftNodeId -> RaftLogIndex -> Model -> Either String Model
acknowledgeAt node index model = do
  state <- lookupState node model
  foldM (\current next -> stepModel node (acknowledgeCommittedEntries (raftLogIndex next)) current) model [raftLogIndexWord64 (raftStateAppliedThrough state) + 1 .. raftLogIndexWord64 index]

lookupState :: RaftNodeId -> Model -> Either String (RaftState Payload)
lookupState node model = case Map.lookup node (modelStates model) of
  Nothing -> Left ("missing state for " <> show node)
  Just state -> Right state

isRequestVote :: RaftNodeId -> RaftNodeId -> Packet -> Bool
isRequestVote source target packet =
  packetFrom packet == source
    && packetTo packet == target
    && case raftRpcView (packetRpc packet) of
      RequestVoteView {} -> True
      _ -> False

isVoteResponse :: RaftNodeId -> RaftNodeId -> Packet -> Bool
isVoteResponse source target packet =
  packetFrom packet == source
    && packetTo packet == target
    && case raftRpcView (packetRpc packet) of
      RequestVoteResponseView {} -> True
      _ -> False

isAppend :: RaftNodeId -> RaftNodeId -> Packet -> Bool
isAppend source target packet =
  packetFrom packet == source
    && packetTo packet == target
    && case raftRpcView (packetRpc packet) of
      AppendEntriesView {} -> True
      _ -> False

isAppendResponse :: RaftNodeId -> RaftNodeId -> Packet -> Bool
isAppendResponse source target packet =
  packetFrom packet == source
    && packetTo packet == target
    && case raftRpcView (packetRpc packet) of
      AppendEntriesResponseView {} -> True
      _ -> False

applicationEntries :: RaftState Payload -> [(RaftLogIndex, RaftTerm, Payload)]
applicationEntries state =
  [ (raftLogEntryIndex entry, raftLogEntryTerm entry, payload)
  | entry <- raftStateLogEntries state,
    Application payload <- [raftLogEntryPayload entry]
  ]

proposalId :: Word -> RaftProposalId
proposalId value = checked "proposal id" (mkRaftProposalId (fromIntegral value))

electionSafetyCase :: Assertion
electionSafetyCase = do
  startedA <- requireModel (startElection nodeA emptyModel)
  competing <- requireModel (startElection nodeC startedA)
  votedA <- requireModel (deliverLatest (isRequestVote nodeA nodeB) competing)
  electedA <- requireModel (deliverLatest (isVoteResponse nodeB nodeA) votedA)
  deniedC <- requireModel (deliverLatest (isRequestVote nodeC nodeB) electedA)
  model <- requireModel (deliverLatest (isVoteResponse nodeB nodeC) deniedC)
  let leaders =
        [ node
        | (node, state) <- Map.toList (modelStates model),
          raftStateRole state == RaftLeader
        ]
  leaders @?= [nodeA]
  state <- requireValue (lookupState nodeA model)
  raftStateTerm state @?= raftTerm 1
  competingState <- requireValue (lookupState nodeC model)
  raftStateRole competingState @?= RaftCandidate

voteAndLogRuleCase :: Assertion
voteAndLogRuleCase = do
  started <- requireModel (startElection nodeA emptyModel)
  voted <- requireModel (deliverLatest (isRequestVote nodeA nodeB) started)
  stateAfterVote <- requireValue (lookupState nodeB voted)
  raftStateVotedFor stateAfterVote @?= Just nodeA
  competingRpc <-
    requireValue
      (requestVote (raftTerm 1) nodeC (raftLogIndex 0) (raftTerm 0))
  competingInput <- requireValue (observeRaftRequest nodeC competingRpc)
  denied <- requireModel (stepModel nodeB competingInput voted)
  denial <- requireValue (latestPacket (isVoteResponse nodeB nodeC) denied)
  case raftRpcView (packetRpc denial) of
    RequestVoteResponseView _ _ granted -> granted @?= False
    other -> assertFailure ("expected vote response, got " <> show other)
  replicated <- requireModel (readyLeader nodeA nodeB emptyModel)
  staleRpc <-
    requireValue
      (requestVote (raftTerm 2) nodeC (raftLogIndex 0) (raftTerm 0))
  staleInput <- requireValue (observeRaftRequest nodeC staleRpc)
  expired <- requireModel (expireLeaderGuard nodeB replicated)
  logDenied <- requireModel (stepModel nodeB staleInput expired)
  logDenial <- requireValue (latestPacket (isVoteResponse nodeB nodeC) logDenied)
  logAwareVoter <- requireValue (lookupState nodeB logDenied)
  raftStateVotedFor logAwareVoter @?= Nothing
  case raftRpcView (packetRpc logDenial) of
    RequestVoteResponseView term _ granted -> do
      term @?= raftTerm 2
      granted @?= False
    other -> assertFailure ("expected log-rule vote response, got " <> show other)

committedPrefixCase :: Assertion
committedPrefixCase = do
  ready <- requireModel (readyLeader nodeA nodeB emptyModel)
  replicated <- requireModel (deliverLatest (isAppend nodeA nodeB) ready)
  follower <- requireValue (lookupState nodeB replicated)
  raftStateCommitIndex follower @?= raftLogIndex 1
  exactRpc <-
    requireValue
      ( appendEntries
          (raftTerm 1)
          nodeA
          (raftLogIndex 0)
          (raftTerm 0)
          [raftLogEntry (raftLogIndex 1) (raftTerm 1) LeaderNoOp]
          (raftLogIndex 1)
      )
  exactInput <- requireValue (observeRaftRequest nodeA exactRpc)
  (matched, matchedBatch) <- requireValue (stepRaft exactInput follower)
  raftStateLogEntries matched @?= raftStateLogEntries follower
  case [ result
       | SendRaftRpc target _ rpc <- raftEffectBatchEffects matchedBatch,
         target == nodeA,
         AppendEntriesResponseView _ _ result <- [raftRpcView rpc]
       ] of
    [AppendAccepted index _] -> index @?= raftLogIndex 1
    other -> assertFailure ("exact matching entry was not acknowledged: " <> show other)
  divergentRpc <-
    requireValue
      ( appendEntries
          (raftTerm 1)
          nodeA
          (raftLogIndex 0)
          (raftTerm 0)
          [raftLogEntry (raftLogIndex 1) (raftTerm 1) (Application "different")]
          (raftLogIndex 0)
      )
  divergentInput <- requireValue (observeRaftRequest nodeA divergentRpc)
  case stepRaft divergentInput follower of
    Left (RaftLogInvariantFault _) -> pure ()
    other -> assertFailure ("equal-index/equal-term divergence was acknowledged: " <> show other)
  applicationReady <- requireModel (readyLeader nodeA nodeB emptyModel)
  applicationCommitted <-
    requireModel
      (proposeAndCommit nodeA nodeB (proposalId 1) "original" applicationReady)
  applicationAnnounced <-
    requireModel (deliverLatest (isAppend nodeA nodeB) applicationCommitted)
  applicationFollower <- requireValue (lookupState nodeB applicationAnnounced)
  samePayloadRpc <-
    requireValue
      ( appendEntries
          (raftTerm 1)
          nodeA
          (raftLogIndex 1)
          (raftTerm 1)
          [ raftLogEntry
              (raftLogIndex 2)
              (raftTerm 1)
              (Application "original")
          ]
          (raftLogIndex 2)
      )
  samePayloadInput <- requireValue (observeRaftRequest nodeA samePayloadRpc)
  (samePayloadState, _) <-
    requireValue (stepRaft samePayloadInput applicationFollower)
  raftStateLogEntries samePayloadState
    @?= raftStateLogEntries applicationFollower
  differentPayloadRpc <-
    requireValue
      ( appendEntries
          (raftTerm 1)
          nodeA
          (raftLogIndex 1)
          (raftTerm 1)
          [ raftLogEntry
              (raftLogIndex 2)
              (raftTerm 1)
              (Application "different")
          ]
          (raftLogIndex 1)
      )
  differentPayloadInput <-
    requireValue (observeRaftRequest nodeA differentPayloadRpc)
  case stepRaft differentPayloadInput applicationFollower of
    Left (RaftLogInvariantFault _) -> pure ()
    other -> assertFailure ("equal-term payload divergence was acknowledged: " <> show other)
  conflictingRpc <-
    requireValue
      ( appendEntries
          (raftTerm 2)
          nodeC
          (raftLogIndex 0)
          (raftTerm 0)
          [raftLogEntry (raftLogIndex 1) (raftTerm 2) (Application "conflict")]
          (raftLogIndex 0)
      )
  conflictingInput <- requireValue (observeRaftRequest nodeC conflictingRpc)
  case stepRaft conflictingInput follower of
    Left (RaftLogInvariantFault _) -> pure ()
    other -> assertFailure ("committed conflict was not an invariant fault: " <> show other)

leaderCompletenessCase :: Assertion
leaderCompletenessCase = do
  ready <- requireModel (readyLeader nodeA nodeB emptyModel)
  committed <-
    requireModel
      (proposeAndCommit nodeA nodeB (proposalId 1) "committed" ready)
  followerApplied <- requireModel (deliverLatest (isAppend nodeA nodeB) committed)
  elected <- requireModel (electWithVote nodeB nodeC followerApplied)
  leader <- requireValue (lookupState nodeB elected)
  assertBool
    "new leader retained committed application"
    (any (\(_, _, payload) -> payload == "committed") (applicationEntries leader))

currentTermCommitCase :: Assertion
currentTermCommitCase = do
  ready <- requireModel (readyLeader nodeA nodeB emptyModel)
  followerCommitted <- requireModel (deliverLatest (isAppend nodeA nodeB) ready)
  proposed <-
    requireModel
      (stepModel nodeA (proposeRaftApplication (proposalId 1) "old-term") followerCommitted)
  oldTermReplicated <- requireModel (deliverLatest (isAppend nodeA nodeB) proposed)
  oldLeader <- requireValue (lookupState nodeA oldTermReplicated)
  futureLeader <- requireValue (lookupState nodeB oldTermReplicated)
  oldTermEvidence <-
    requireValue (latestPacket (isAppendResponse nodeB nodeA) oldTermReplicated)
  case raftRpcView (packetRpc oldTermEvidence) of
    AppendEntriesResponseView _ _ (AppendAccepted index _) ->
      index @?= raftLogIndex 2
    other -> assertFailure ("expected old-term majority evidence, got " <> show other)
  assertBool
    "the old-term entry is physically present on a majority"
    ( all
        (any (\(_, _, payload) -> payload == "old-term") . applicationEntries)
        [oldLeader, futureLeader]
    )
  assertBool
    "the majority entry predates the next leader term"
    ( all
        (any (\(_, term, payload) -> term == raftTerm 1 && payload == "old-term") . applicationEntries)
        [oldLeader, futureLeader]
    )
  raftStateCommitIndex oldLeader @?= raftLogIndex 1
  raftStateCommitIndex futureLeader @?= raftLogIndex 1
  -- Whole-suffix AppendEntries means the new leader cannot acquire match-index
  -- evidence only through index 2: its term-2 no-op at index 3 is in every
  -- successful attempt.  The reachable public trace therefore establishes the
  -- old entry's physical majority first, observes no commit before any no-op
  -- acknowledgement, and then observes the no-op acknowledgement advance both.
  elected <- requireModel (electWithVote nodeB nodeC oldTermReplicated)
  before <- requireValue (lookupState nodeB elected)
  raftStateCommitIndex before @?= raftLogIndex 1
  raftStateMatchIndex nodeB before @?= Just (raftLogIndex 3)
  raftStateMatchIndex nodeA before @?= Just (raftLogIndex 0)
  raftStateMatchIndex nodeC before @?= Just (raftLogIndex 0)
  rejected <- requireModel (deliverLatest (isAppend nodeB nodeC) elected)
  retried <- requireModel (deliverLatest (isAppendResponse nodeC nodeB) rejected)
  retryingLeader <- requireValue (lookupState nodeB retried)
  raftStateCommitIndex retryingLeader @?= raftLogIndex 1
  replicated <- requireModel (deliverLatest (isAppend nodeB nodeC) retried)
  beforeNoOpAck <- requireValue (lookupState nodeB replicated)
  raftStateCommitIndex beforeNoOpAck @?= raftLogIndex 1
  noOpEvidence <- requireValue (latestPacket (isAppendResponse nodeC nodeB) replicated)
  case raftRpcView (packetRpc noOpEvidence) of
    AppendEntriesResponseView _ _ (AppendAccepted index _) ->
      index @?= raftLogIndex 3
    other -> assertFailure ("expected current-term no-op evidence, got " <> show other)
  after <- requireModel (deliverLatest (isAppendResponse nodeC nodeB) replicated)
  afterState <- requireValue (lookupState nodeB after)
  raftStateCommitIndex afterState @?= raftLogIndex 3

oldSuffixLossCase :: Assertion
oldSuffixLossCase = do
  ready <- requireModel (readyLeader nodeA nodeB emptyModel)
  caughtUpC <- requireModel (deliverLatest (isAppend nodeA nodeC) ready)
  oldLeader <-
    requireModel
      (stepModel nodeA (proposeRaftApplication (proposalId 1) "uncommitted") caughtUpC)
  expired <- requireModel (expireLeaderGuard nodeB oldLeader)
  electedC <- requireModel (electWithVote nodeC nodeB expired)
  replicated <- requireModel (deliverLatest (isAppend nodeC nodeB) electedC)
  committedC <- requireModel (deliverLatest (isAppendResponse nodeB nodeC) replicated)
  repairedA <- requireModel (deliverLatest (isAppend nodeC nodeA) committedC)
  stateA <- requireValue (lookupState nodeA repairedA)
  let entryTwo = filter ((== raftLogIndex 2) . raftLogEntryIndex) (raftStateLogEntries stateA)
  case entryTwo of
    [entry] -> do
      raftLogEntryTerm entry @?= raftTerm 2
      raftLogEntryPayload entry @?= LeaderNoOp
    other -> assertFailure ("unexpected repaired entry two: " <> show other)

conflictRepairCase :: Assertion
conflictRepairCase = do
  leaderHistoryRpc <-
    requireValue
      ( appendEntries
          (raftTerm 1)
          nodeA
          (raftLogIndex 0)
          (raftTerm 0)
          [ raftLogEntry (raftLogIndex 1) (raftTerm 1) LeaderNoOp,
            raftLogEntry (raftLogIndex 2) (raftTerm 1) (Application "leader-history")
          ]
          (raftLogIndex 0)
      )
  leaderHistoryInput <- requireValue (observeRaftRequest nodeA leaderHistoryRpc)
  seededB <- requireModel (stepModel nodeB leaderHistoryInput emptyModel)
  conflictHistoryRpc <-
    requireValue
      ( appendEntries
          (raftTerm 2)
          nodeA
          (raftLogIndex 0)
          (raftTerm 0)
          [ raftLogEntry (raftLogIndex 1) (raftTerm 1) LeaderNoOp,
            raftLogEntry (raftLogIndex 2) (raftTerm 2) (Application "conflicting-term")
          ]
          (raftLogIndex 0)
      )
  conflictHistoryInput <- requireValue (observeRaftRequest nodeA conflictHistoryRpc)
  seeded <- requireModel (stepModel nodeC conflictHistoryInput seededB)
  termTwoCandidate <- requireModel (startElection nodeB seeded)
  termThreeCandidate <- requireModel (startElection nodeB termTwoCandidate)
  voted <- requireModel (deliverLatest (isRequestVote nodeB nodeA) termThreeCandidate)
  electedB <- requireModel (deliverLatest (isVoteResponse nodeA nodeB) voted)
  rejected <- requireModel (deliverLatest (isAppend nodeB nodeC) electedB)
  rejection <- requireValue (latestPacket (isAppendResponse nodeC nodeB) rejected)
  case raftRpcView (packetRpc rejection) of
    AppendEntriesResponseView _ _ (AppendRejected hint) ->
      raftConflictHintView hint
        @?= ConflictingTermView (raftTerm 2) (raftLogIndex 2)
    other -> assertFailure ("expected conflicting-term rejection, got " <> show other)
  retried <- requireModel (deliverLatest (isAppendResponse nodeC nodeB) rejected)
  retryPacket <- requireValue (latestPacket (isAppend nodeB nodeC) retried)
  case raftRpcView (packetRpc retryPacket) of
    AppendEntriesView _ _ previousIndex _ entries _ -> do
      previousIndex @?= raftLogIndex 1
      map raftLogEntryIndex entries @?= [raftLogIndex 2, raftLogIndex 3]
    other -> assertFailure ("expected repaired append request, got " <> show other)
  installed <- requireModel (deliverLatest (isAppend nodeB nodeC) retried)
  stateC <- requireValue (lookupState nodeC installed)
  raftStateLastLogIndex stateC @?= raftLogIndex 3
  stateB <- requireValue (lookupState nodeB installed)
  raftStateLogEntries stateC @?= raftStateLogEntries stateB

duplicateRpcCase :: Assertion
duplicateRpcCase = do
  elected <- requireModel (electWithVote nodeA nodeB emptyModel)
  oldAppend <- requireValue (latestPacket (isAppend nodeA nodeC) elected)
  replicatedB <- requireModel (deliverLatest (isAppend nodeA nodeB) elected)
  committed <- requireModel (deliverLatest (isAppendResponse nodeB nodeA) replicatedB)
  ready <- requireModel (acknowledgeAt nodeA (raftLogIndex 1) committed)
  proposed <-
    requireModel
      (stepModel nodeA (proposeRaftApplication (proposalId 1) "new-expectation") ready)
  newAppend <- requireValue (latestPacket (isAppend nodeA nodeC) proposed)
  assertBool
    "a changed log expectation receives a distinct local generation"
    (packetDispatch oldAppend /= packetDispatch newAppend)
  assertBool
    "the multi-peer send batch remains split into independently scheduled packets"
    (any (isAppend nodeA nodeB) (modelPackets proposed))
  reordered <- requireModel (deliverSpecific oldAppend proposed)
  staleResponse <-
    requireValue (latestPacket (isAppendResponse nodeC nodeA) reordered)
  afterStale <- requireModel (deliverSpecific staleResponse reordered)
  leaderAfterStale <- requireValue (lookupState nodeA afterStale)
  raftStateMatchIndex nodeC leaderAfterStale @?= Just (raftLogIndex 0)
  installedOnce <- requireModel (deliverSpecific newAppend afterStale)
  installedTwice <-
    requireModel
      ( deliverLatest
          (== newAppend)
          (installedOnce {modelPackets = modelPackets installedOnce <> [newAppend]})
      )
  stateC <- requireValue (lookupState nodeC installedTwice)
  raftStateLastLogIndex stateC @?= raftLogIndex 2
  length (raftStateLogEntries stateC) @?= 3
  accepted <-
    requireModel (deliverLatest (isAppendResponse nodeC nodeA) installedTwice)
  leaderAfterAccepted <- requireValue (lookupState nodeA accepted)
  raftStateMatchIndex nodeC leaderAfterAccepted @?= Just (raftLogIndex 2)
  afterDuplicateResponse <-
    requireModel (deliverLatest (isAppendResponse nodeC nodeA) accepted)
  finalLeader <- requireValue (lookupState nodeA afterDuplicateResponse)
  raftStateMatchIndex nodeC finalLeader @?= Just (raftLogIndex 2)
  firstElection <- requireModel (startElection nodeA emptyModel)
  oldVoteRequest <- requireValue (latestPacket (isRequestVote nodeA nodeB) firstElection)
  secondElection <- requireModel (startElection nodeA firstElection)
  newVoteRequest <- requireValue (latestPacket (isRequestVote nodeA nodeB) secondElection)
  assertBool
    "a newer election expectation supersedes the old response generation"
    (packetDispatch oldVoteRequest /= packetDispatch newVoteRequest)
  currentTermVote <-
    requireValue (requestVoteResponse (raftTerm 2) nodeB True)
  staleVoteInput <-
    requireValue
      ( observeRaftResponse
          nodeB
          (packetDispatch oldVoteRequest)
          currentTermVote
      )
  afterStaleVote <- requireModel (stepModel nodeA staleVoteInput secondElection)
  afterStaleVote @?= secondElection
  candidateAfterStaleVote <- requireValue (lookupState nodeA afterStaleVote)
  raftStateRole candidateAfterStaleVote @?= RaftCandidate
  freshVoteInput <-
    requireValue
      ( observeRaftResponse
          nodeB
          (packetDispatch newVoteRequest)
          currentTermVote
      )
  afterFreshVote <- requireModel (stepModel nodeA freshVoteInput afterStaleVote)
  leaderAfterFreshVote <- requireValue (lookupState nodeA afterFreshVote)
  raftStateRole leaderAfterFreshVote @?= RaftLeader
  raftStateTerm leaderAfterFreshVote @?= raftTerm 2

majorityPartitionCase :: Assertion
majorityPartitionCase = do
  minority <- requireModel (startElection nodeA emptyModel)
  candidate <- requireValue (lookupState nodeA minority)
  raftStateRole candidate @?= RaftCandidate
  raftStateCommitIndex candidate @?= raftLogIndex 0
  elected <- requireModel (deliverLatest (isRequestVote nodeA nodeB) minority >>= deliverLatest (isVoteResponse nodeB nodeA))
  leader <- requireValue (lookupState nodeA elected)
  raftStateRole leader @?= RaftLeader
  raftStateCommitIndex leader @?= raftLogIndex 0
  majority <- requireModel (deliverLatest (isAppend nodeA nodeB) elected >>= deliverLatest (isAppendResponse nodeB nodeA))
  ready <- requireValue (lookupState nodeA majority)
  raftStateCommitIndex ready @?= raftLogIndex 1

noOpReadinessCase :: Assertion
noOpReadinessCase = do
  elected <- requireModel (electWithVote nodeA nodeB emptyModel)
  before <- requireValue (lookupState nodeA elected)
  raftStateServiceReady before @?= False
  rejected <-
    requireModel
      (stepModel nodeA (proposeRaftApplication (proposalId 1) "too-early") elected)
  rejectedState <- requireValue (lookupState nodeA rejected)
  raftStateLastLogIndex rejectedState @?= raftLogIndex 1
  unapplied <- requireModel (deliverLatest (isAppend nodeA nodeB) rejected >>= deliverLatest (isAppendResponse nodeB nodeA))
  unappliedState <- requireValue (lookupState nodeA unapplied)
  raftStateServiceReady unappliedState @?= False
  ready <- requireModel (acknowledgeAt nodeA (raftLogIndex 1) unapplied)
  readyState <- requireValue (lookupState nodeA ready)
  raftStateServiceReady readyState @?= True
  committed <- requireModel (proposeAndCommit nodeA nodeB (proposalId 2) "payload" ready)
  committedState <- requireValue (lookupState nodeA committed)
  raftStateServiceReady committedState @?= False
  acknowledged <- requireModel (acknowledgeAt nodeA (raftLogIndex 2) committed)
  finalState <- requireValue (lookupState nodeA acknowledged)
  raftStateAppliedThrough finalState @?= raftLogIndex 2
  raftStateServiceReady finalState @?= True

opaquePayloadProperty :: Property
opaquePayloadProperty =
  forAll (chooseInt (0, 255)) $ \leftByte ->
    forAll (chooseInt (0, 255)) $ \rightByte ->
      let leftPayload = ByteString.pack [fromIntegral leftByte]
          rightPayload = ByteString.pack [fromIntegral rightByte]
       in case (runCommittedPayload leftPayload nodeB, runCommittedPayload rightPayload nodeB) of
            (Right (leftState, leftExposed), Right (rightState, rightExposed)) ->
              counterexample
                (show (leftState, rightState, leftExposed, rightExposed))
                ( property
                    ( normalizeState leftState == normalizeState rightState
                        && map applicationPayload leftExposed == [leftPayload]
                        && map applicationPayload rightExposed == [rightPayload]
                    )
                )
            result -> counterexample (show result) False

installBeforeHandoffCase :: Assertion
installBeforeHandoffCase = do
  let initial = initialRaft (checkedGenesisFor nodeA) :: RaftState Payload
      generation = raftStateElectionGeneration initial
  (selected, _) <-
    requireValue (stepRaft (selectElectionTimeout generation electionLower) initial)
  (adopted, effects) <- blockedOwnerStep (fireElectionTimer generation) selected
  raftStateRole adopted @?= RaftCandidate
  raftStateTerm adopted @?= raftTerm 1
  let requestTerms =
        [ term
        | SendRaftRpc _ _ rpc <- effects,
          RequestVoteView term _ _ _ <- [raftRpcView rpc]
        ]
  requestTerms @?= [raftTerm 1, raftTerm 1]

dispatcherDelayCase :: Assertion
dispatcherDelayCase = do
  ready <- requireModel (readyLeader nodeA nodeB emptyModel)
  proposed <-
    requireModel
      (stepModel nodeA (proposeRaftApplication (proposalId 1) "once") ready)
  appendPacket <- requireValue (latestPacket (isAppend nodeA nodeB) proposed)
  replicated <- requireModel (deliverSpecific appendPacket proposed)
  responsePacket <-
    requireValue (latestPacket (isAppendResponse nodeB nodeA) replicated)
  responseInput <- requireValue (packetInput responsePacket)
  predecessor <- requireValue (lookupState nodeA replicated)
  (committed, effects) <- blockedOwnerStep responseInput predecessor
  raftStateCommitIndex committed @?= raftLogIndex 2
  sort [target | SendRaftRpc target _ _ <- effects] @?= [nodeB, nodeC]
  let exposed =
        concat
          [ NonEmpty.toList entries
          | ExposeCommittedEntries entries <- effects
          ]
      committedStatuses =
        [ index
        | ReportRaftProposal proposal (RaftProposalCommitted index) <- effects,
          proposal == proposalId 1
        ]
  map applicationPayload exposed @?= ["once"]
  committedStatuses @?= [raftLogIndex 2]
  case stepRaft responseInput committed of
    Left problem -> assertFailure ("duplicate response faulted: " <> show problem)
    Right (unchanged, duplicateBatch) -> do
      unchanged @?= committed
      raftEffectBatchEffects duplicateBatch @?= []

equalPrefixExposureCase :: Assertion
equalPrefixExposureCase = do
  ready <- requireModel (readyLeader nodeA nodeB emptyModel)
  caughtUpC <- requireModel (deliverLatest (isAppend nodeA nodeC) ready)
  firstCommitted <-
    requireModel
      (proposeAndCommit nodeA nodeB (proposalId 1) "first" caughtUpC)
  firstExposedB <- requireModel (deliverLatest (isAppend nodeA nodeB) firstCommitted)
  firstExposedBoth <- requireModel (deliverLatest (isAppend nodeA nodeC) firstExposedB)
  firstAcknowledged <- requireModel (acknowledgeAt nodeA (raftLogIndex 2) firstExposedBoth)
  secondCommitted <-
    requireModel
      (proposeAndCommit nodeA nodeB (proposalId 2) "second" firstAcknowledged)
  secondExposedB <- requireModel (deliverLatest (isAppend nodeA nodeB) secondCommitted)
  exposedBoth <- requireModel (deliverLatest (isAppend nodeA nodeC) secondExposedB)
  stateB <- requireValue (lookupState nodeB exposedBoth)
  stateC <- requireValue (lookupState nodeC exposedBoth)
  let committedA = Map.findWithDefault [] nodeA (modelCommitted exposedBoth)
      committedB = Map.findWithDefault [] nodeB (modelCommitted exposedBoth)
      committedC = Map.findWithDefault [] nodeC (modelCommitted exposedBoth)
  raftStateLogEntries stateB @?= raftStateLogEntries stateC
  map committedEntryIndex committedA
    @?= [raftLogIndex 2, raftLogIndex 3]
  map applicationPayload committedA @?= ["first", "second"]
  map committedEntryIndex committedA
    @?= map committedEntryIndex committedB
  map committedEntryIndex committedA
    @?= map committedEntryIndex committedC
  map applicationPayload committedA
    @?= map applicationPayload committedB
  map applicationPayload committedA
    @?= map applicationPayload committedC

runCommittedPayload ::
  Payload ->
  RaftNodeId ->
  Either String (RaftState Payload, [RaftCommittedEntry Payload])
runCommittedPayload payload follower = do
  ready <- readyLeader nodeA follower emptyModel
  committed <- proposeAndCommit nodeA follower (proposalId 1) payload ready
  state <- lookupState nodeA committed
  pure (state, Map.findWithDefault [] nodeA (modelCommitted committed))

normalizeState :: RaftState Payload -> (RaftRole, RaftTerm, RaftLogIndex, RaftLogIndex, Bool)
normalizeState state =
  ( raftStateRole state,
    raftStateTerm state,
    raftStateCommitIndex state,
    raftStateLastLogIndex state,
    raftStateServiceReady state
  )

latestPacket :: (Packet -> Bool) -> Model -> Either String Packet
latestPacket predicate model = fst <$> takeLatest predicate (modelPackets model)

deliverSpecific :: Packet -> Model -> Either String Model
deliverSpecific selected model = do
  (_, remaining) <- takeLatest (== selected) (modelPackets model)
  input <- packetInput selected
  let context = case raftRpcView (packetRpc selected) of
        RequestVoteView {} -> Just (packetFrom selected, packetDispatch selected)
        AppendEntriesView {} -> Just (packetFrom selected, packetDispatch selected)
        _ -> Nothing
  stepModelWithReply (packetTo selected) context input (model {modelPackets = remaining})

-- A one-slot dispatcher queue starts occupied, so the owner must adopt the
-- complete successor before its whole sealed batch handoff can complete.
blockedOwnerStep ::
  RaftInput Payload ->
  RaftState Payload ->
  IO (RaftState Payload, [RaftEffect Payload])
blockedOwnerStep input predecessor = case stepRaft input predecessor of
  Left problem -> assertFailure (show problem)
  Right (successor, batch) -> do
    handoff <- newMVar (Nothing :: Maybe (RaftEffectBatch Payload))
    adopted <- newEmptyMVar
    attemptingHandoff <- newEmptyMVar
    completed <- newEmptyMVar
    _ <- forkIO $ do
      putMVar adopted successor
      putMVar attemptingHandoff ()
      putMVar handoff (Just batch)
      putMVar completed ()
    installed <- takeMVar adopted
    takeMVar attemptingHandoff
    completionBeforeRelease <- tryReadMVar completed
    completionBeforeRelease @?= Nothing
    placeholder <- takeMVar handoff
    case placeholder of
      Nothing -> pure ()
      Just _ -> assertFailure "sealed batch bypassed the occupied dispatcher slot"
    delivered <- takeMVar handoff
    takeMVar completed
    installed @?= successor
    case delivered of
      Nothing -> assertFailure "dispatcher lost the sealed transition batch"
      Just deliveredBatch ->
        pure (installed, raftEffectBatchEffects deliveredBatch)

requireModel :: Either String Model -> IO Model
requireModel = either assertFailure pure

requireValue :: (Show problem) => Either problem value -> IO value
requireValue = either (assertFailure . show) pure

mapProblem :: (Show problem) => String -> Either problem value -> Either String value
mapProblem label = either (Left . ((label <> ": ") <>) . show) Right

checked :: (Show problem) => String -> Either problem value -> value
checked label = either (error . ((label <> ": ") <>) . show) id

isApplication :: RaftCommittedEntry bytes -> Bool
isApplication entry = case committedEntryPayload entry of Application _ -> True; _ -> False
applicationPayload :: RaftCommittedEntry bytes -> bytes
applicationPayload entry = case committedEntryPayload entry of Application payload -> payload; _ -> error "expected application observation"

expireLeaderGuard :: RaftNodeId -> Model -> Either String Model
expireLeaderGuard node model = do
  state <- lookupState node model
  stepModel node (fireRecentLeaderTimer (raftStateRecentLeaderGeneration state)) model
