{-# LANGUAGE OverloadedStrings #-}

-- | A finite reference machine beside the live kernel.  The reference owns its
-- own physical logs, terms, votes, match positions and commit decisions.  It
-- uses full-prefix exchanges to abstract only AppendEntries conflict retries;
-- no production quorum, membership, log-repair or election helper computes an
-- expected result.  Every macro step compares all replicas, and an independent
-- committed-entry ledger checks leader completeness and prefix immutability.
--
-- These are bounded schedules, not a proof of unbounded asynchronous liveness.
module ReconfigurationModelProperties (tests) where

import Control.Monad (foldM, unless)
import Data.ByteString (ByteString)
import Data.List (permutations, subsequences)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Raft.Configuration qualified as C
import Eclips.Raft.Effect qualified as E
import Eclips.Raft.Genesis qualified as G
import Eclips.Raft.Identity qualified as I
import Eclips.Raft.Input qualified as R
import Eclips.Raft.State qualified as S
import Eclips.Raft.Transition qualified as T
import GenesisProperties (electionLower, electionUpper, heartbeat, nodeId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "independent reconfiguration model"
    [ testCase "singleton grows through exact applied learner frontiers" singletonGrowth,
      testCase "replication replies require acknowledged application before learner promotion" appliedReplyFrontier,
      testCase "stable even sets require strict majorities in every vote ordering" evenElections,
      testCase "joint commit requires both majorities for every acknowledgement subset" jointSubsets,
      testCase "joint elections require both majorities for every vote subset" jointElectionSubsets,
      testCase "newly logged learner wins before joint commitment and commits through its no-op" newlyLoggedElection,
      testCase "a previously removed replica can campaign when a later joint includes it" recommissionedElection,
      testCase "even joint sets independently require three of four new voters" evenJointSubsets,
      testCase "old-only and new-only partitions cannot elect competing leaders" splitPartitions,
      testCase "partly learned old, joint and final groups cannot elect divergent histories" partialKnowledge,
      testCase "delayed shorter append and stale reply preserve newer configuration history" delayedPackets,
      testCase "leader loss while joint preserves committed history and finalizes" jointLeaderLoss,
      testCase "uncommitted final permits excluded previous voter to campaign without self vote" excludedCampaign,
      testCase "committed final excludes leader and future campaigns" committedRemoval,
      testCase "interrupted joint suffix rolls back to the surviving stable configuration" rollbackJoint,
      testCase "interrupted final suffix rolls back to the surviving joint configuration" rollbackFinal,
      testCase "learners cannot campaign, vote or commit a leader suffix" learnerExclusion,
      testCase "recent leader contact blocks an already inflated vote request before term handling" inflatedGuard,
      testCase "asymmetric vote traffic cannot depose a leader with recent quorum contact" asymmetricGuard,
      testCase "sends alone and duplicate acknowledgements cannot renew leader contact" guardRenewal,
      testCase "joint recent-leader renewal needs fresh majorities of both voter sets" jointGuardRenewal,
      testCase "partial quorum evidence expires while leader protection is inactive" partialGuardExpiry,
      QC.testProperty "opaque metadata and delivery permutations preserve complete growth history" growthPermutation
    ]

type Node = I.RaftNodeId

data Configuration = Stable (Set Node) | Joint (Set Node) (Set Node)
  deriving stock (Eq, Show)

data Payload = NoOp | App ByteString | Config Configuration ByteString
  deriving stock (Eq, Show)

data Entry = Entry Word64 Payload
  deriving stock (Eq, Show)

data Role = Follower | Candidate | Leader
  deriving stock (Eq, Show)

data Replica = Replica
  { term :: Word64,
    vote :: Maybe Node,
    role :: Role,
    history :: [Entry],
    committed :: Int,
    applied :: Int,
    matches :: Map Node Int,
    votes :: Set Node
  }
  deriving stock (Eq, Show)

data Reference = Reference
  { initial :: Configuration,
    replicas :: Map Node Replica,
    committedLedger :: Map Int Entry
  }
  deriving stock (Eq, Show)

data Packet = Packet Node Node I.RaftDispatchGeneration (Maybe I.RaftDispatchGeneration) (R.RaftRpc ByteString)
  deriving stock (Eq, Show)

data Network = Network
  { reference :: Reference,
    live :: Map Node (S.RaftState ByteString),
    packets :: [Packet],
    exposures :: Map Node [(Int, Entry)]
  }
  deriving stock (Eq, Show)

blank :: Replica
blank = Replica 0 Nothing Follower [] 0 0 Map.empty Set.empty

members :: Configuration -> Set Node
members (Stable ns) = ns
members (Joint before after) = Set.union before after

-- Deliberately expressed by counting list members, independently of native
-- configuration accessors and the kernel's quorum predicate.
majority :: Set Node -> Set Node -> Bool
majority voters acknowledgers =
  length [n | n <- Set.toList voters, n `Set.member` acknowledgers] >= Set.size voters `div` 2 + 1

quorum :: Configuration -> Set Node -> Bool
quorum (Stable ns) acknowledgers = majority ns acknowledgers
quorum (Joint before after) acknowledgers = majority before acknowledgers && majority after acknowledgers

configuration :: Configuration -> [Entry] -> Configuration
configuration = foldl' step
  where
    step _ (Entry _ (Config cfg _)) = cfg
    step cfg _ = cfg

at :: Node -> Reference -> Replica
at n r = replicas r Map.! n

change :: Node -> (Replica -> Replica) -> Reference -> Reference
change n f r = r {replicas = Map.adjust f n (replicas r)}

effective :: Node -> Reference -> Configuration
effective n r = configuration (initial r) (history (at n r))

committedConfiguration :: Node -> Reference -> Configuration
committedConfiguration n r = configuration (initial r) (take (committed (at n r)) (history (at n r)))

freshness :: Replica -> (Word64, Int)
freshness r = (case reverse (history r) of Entry t _ : _ -> t; [] -> 0, length (history r))

commitAt :: Node -> Int -> Reference -> Either String Reference
commitAt n position r = do
  let before = at n r
      newlyKnown = Map.fromList (zip [1 ..] (take position (history before)))
  unless
    (all (\(i, e) -> maybe True (== e) (Map.lookup i (committedLedger r))) (Map.toList newlyKnown))
    (Left "reference safety violation: different entries committed at the same index")
  let advanced = change n (\p -> p {committed = max position (committed p)}) r
      updated = advanced {committedLedger = Map.union (committedLedger r) newlyKnown}
  pure
    ( if n `Set.notMember` members (committedConfiguration n updated) && any excludesLocal (take (position - committed before) (drop (committed before) (history before)))
        then change n (\p -> p {role = Follower}) updated
        else updated
    )
  where
    excludesLocal (Entry _ (Config (Stable ns) _)) = n `Set.notMember` ns
    excludesLocal _ = False

advanceCommit :: Node -> Reference -> Either String Reference
advanceCommit n r
  | role state /= Leader = Right r
  | otherwise = commitAt n next r
  where
    state = at n r
    next = maximum (committed state : [i | (i, Entry t _) <- zip [1 ..] (history state), t == term state, quorum (effective n r) (Map.keysSet (Map.filter (>= i) (matches state)))])

appendLocal :: Node -> Payload -> Reference -> Either String Reference
appendLocal n payload r =
  advanceCommit n (change n (\p -> p {history = history p ++ [Entry (term p) payload], matches = Map.insert n (length (history p) + 1) (matches p)}) r)

elect :: Node -> Reference -> Either String Reference
elect n r
  | role p /= Candidate || not (quorum (effective n r) (votes p)) = Right r
  | otherwise = do
      unless
        (all (\(i, e) -> take 1 (drop (i - 1) (history p)) == [e]) (Map.toList (committedLedger r)))
        (Left "reference safety violation: elected leader lacks a committed entry")
      appendLocal n NoOp (change n (\s -> s {role = Leader, matches = Map.singleton n (length (history s))}) r)
  where
    p = at n r

campaignReference :: Node -> Reference -> Either String Reference
campaignReference n r
  | role (at n r) == Leader = Right r
  | n `Set.notMember` Set.union (members (effective n r)) (members (committedConfiguration n r)) = Right r
  | otherwise = elect n (change n start r)
  where
    self = n `Set.member` members (effective n r)
    start p = p {term = term p + 1, vote = if self then Just n else Nothing, role = Candidate, votes = if self then Set.singleton n else Set.empty}

voteReference :: Node -> Node -> Reference -> Either String Reference
voteReference candidate voter r
  | term asking < term voting = Right (change candidate (\p -> p {term = term voting, vote = Nothing, role = Follower}) r)
  | otherwise = elect candidate counted
  where
    asking = at candidate r
    voting = at voter r
    refreshed =
      if term asking > term voting
        then change voter (\p -> p {term = term asking, vote = Nothing, role = Follower}) r
        else r
    grants = voter `Set.member` members (effective voter refreshed) && maybe True (== candidate) (vote (at voter refreshed)) && freshness asking >= freshness (at voter refreshed)
    recorded = if grants then change voter (\p -> p {vote = Just candidate}) refreshed else refreshed
    counted =
      if grants && voter `Set.member` members (effective candidate recorded)
        then change candidate (\p -> p {votes = Set.insert voter (votes p)}) recorded
        else recorded

-- Matching shorter snapshots preserve the receiver's suffix. Only an actual
-- conflict replaces it, and a committed prefix may never be overwritten.
mergeHistory :: Int -> [Entry] -> [Entry] -> Either String [Entry]
mergeHistory committedPrefix incoming existing = go 1 incoming existing
  where
    go _ [] rest = Right rest
    go _ rest [] = Right rest
    go i (x : xs) (y : ys)
      | x == y = (x :) <$> go (i + 1) xs ys
      | i <= committedPrefix = Left "reference safety violation: replaced committed prefix"
      | otherwise = Right (x : xs)

receiveReference :: Node -> Node -> Reference -> Either String Reference
receiveReference leader follower r
  | term sending < term receiving = Right r
  | otherwise = do
      repaired <- mergeHistory (committed receiving) (history sending) (history receiving)
      let adopted = change follower (\p -> p {term = term sending, vote = if term sending > term p then Nothing else vote p, role = Follower, history = repaired}) r
      commitAt follower (min (committed sending) (length (history sending))) adopted
  where
    sending = at leader r
    receiving = at follower r

replicateReference :: Node -> Node -> Reference -> Either String Reference
replicateReference leader follower r
  | term sending < term receiving = Right (change leader (\p -> p {term = term receiving, vote = Nothing, role = Follower}) r)
  | otherwise = do
      learned <- receiveReference leader follower r
      advanceCommit leader (change leader (\p -> p {matches = Map.insert follower (length (history sending)) (matches p)}) learned)
  where
    sending = at leader r
    receiving = at follower r

nativeConfiguration :: Configuration -> C.RaftVotingConfiguration
nativeConfiguration (Stable ns) = C.stableRaftConfiguration (checked (C.raftVoterSet (Set.toList ns)))
nativeConfiguration (Joint before after) = C.jointRaftConfiguration (checked (C.raftVoterSet (Set.toList before))) (checked (C.raftVoterSet (Set.toList after)))

fromNativeConfiguration :: C.RaftVotingConfiguration -> Configuration
fromNativeConfiguration cfg = case C.raftVotingConfigurationView cfg of
  C.StableRaftConfigurationView ns -> Stable (Set.fromList ns)
  C.JointRaftConfigurationView before after -> Joint (Set.fromList before) (Set.fromList after)

fromNativePayload :: R.RaftEntry ByteString -> Payload
fromNativePayload payload = case payload of
  R.LeaderNoOp -> NoOp
  R.Application bytes -> App bytes
  R.Configuration cfg bytes -> Config (fromNativeConfiguration cfg) bytes

fromNativeEntry :: R.RaftLogEntry ByteString -> Entry
fromNativeEntry entry = Entry (I.raftTermWord64 (R.raftLogEntryTerm entry)) (fromNativePayload (R.raftLogEntryPayload entry))

referencePosition :: [Entry] -> Maybe (Int, Word64)
referencePosition entries = foldl' step Nothing (zip [1 ..] entries)
  where
    step _ (i, Entry t (Config _ _)) = Just (i, t)
    step position _ = position

nativePosition :: C.RaftConfigurationRef -> Maybe (Int, Word64)
nativePosition ref = case C.raftConfigurationRefView ref of
  C.GenesisRaftConfigurationRefView -> Nothing
  C.RaftConfigurationEntryRefView i t -> Just (fromIntegral (I.raftLogIndexWord64 i), I.raftTermWord64 t)

newNetwork :: [Node] -> [Node] -> Network
newNetwork voters participants = Network ref states [] Map.empty
  where
    ref = Reference (Stable (Set.fromList voters)) (Map.fromList [(n, blank) | n <- participants]) Map.empty
    states = Map.fromList [(n, T.initialRaft (checked ((if n `elem` voters then G.checkRaftGenesis else G.checkRaftLearnerGenesis) (G.raftGenesis n voters heartbeat electionLower electionUpper)))) | n <- participants]

stateAt :: Node -> Network -> S.RaftState ByteString
stateAt n net = live net Map.! n

checkNetwork :: String -> Network -> Either String Network
checkNetwork context net = do
  mapM_ check (Map.toList (replicas (reference net)))
  pure net
  where
    check (n, expected) = do
      let actual = stateAt n net
          actualHistory = map fromNativeEntry (filter ((> I.raftLogIndex 0) . R.raftLogEntryIndex) (S.raftStateLogEntries actual))
          actualRole = case S.raftStateRole actual of S.RaftFollower -> Follower; S.RaftCandidate -> Candidate; S.RaftLeader -> Leader
      problem (T.auditRaftState actual)
      equal "term" (term expected) (I.raftTermWord64 (S.raftStateTerm actual))
      equal "role" (role expected) actualRole
      equal "vote" (vote expected) (S.raftStateVotedFor actual)
      equal "voting eligibility" (n `Set.member` members (effective n (reference net))) (S.raftStateCanVote actual)
      equal "campaign eligibility" (n `Set.member` Set.union (members (effective n (reference net))) (members (committedConfiguration n (reference net)))) (S.raftStateCanCampaign actual)
      equal "physical log" (history expected) actualHistory
      equal "commit" (committed expected) (fromIntegral (I.raftLogIndexWord64 (S.raftStateCommitIndex actual)))
      equal "exposed exact full prefix" (zip [1 ..] (take (committed expected) (history expected))) (Map.findWithDefault [] n (exposures net))
      equal "effective configuration position" (referencePosition (history expected)) (nativePosition (fst (S.raftStateEffectiveConfiguration actual)))
      equal "committed configuration position" (referencePosition (take (committed expected) (history expected))) (nativePosition (fst (S.raftStateCommittedConfiguration actual)))
      equal "applied" (applied expected) (fromIntegral (I.raftLogIndexWord64 (S.raftStateAppliedThrough actual)))
      equal "effective configuration" (effective n (reference net)) (fromNativeConfiguration (snd (S.raftStateEffectiveConfiguration actual)))
      equal "committed configuration" (committedConfiguration n (reference net)) (fromNativeConfiguration (snd (S.raftStateCommittedConfiguration actual)))
    equal :: (Eq value, Show value) => String -> value -> value -> Either String ()
    equal label expected actual = unless (expected == actual) (Left (context <> ": " <> label <> " expected " <> show expected <> ", received " <> show actual))

stepNative :: Node -> Maybe (Node, I.RaftDispatchGeneration) -> R.RaftInput ByteString -> Network -> Either String Network
stepNative n reply input net = do
  (next, batch) <- problem (T.stepRaft input (stateAt n net))
  foldM emit (net {live = Map.insert n next (live net)}) (E.raftEffectBatchEffects batch)
  where
    emit current (E.SendRaftRpc target generation rpc) =
      let correlation = case (reply, R.raftRpcView rpc) of
            (Just (source, requestGeneration), R.RequestVoteResponseView {}) | target == source -> Just requestGeneration
            (Just (source, requestGeneration), R.AppendEntriesResponseView {}) | target == source -> Just requestGeneration
            _ -> Nothing
       in Right (current {packets = packets current ++ [Packet n target generation correlation rpc]})
    emit current (E.ExposeCommittedEntries entries) =
      let observed = [(fromIntegral (I.raftLogIndexWord64 (E.committedEntryIndex e)), Entry (I.raftTermWord64 (E.committedEntryTerm e)) (fromNativePayload (E.committedEntryPayload e))) | e <- NonEmpty.toList entries]
       in Right (current {exposures = Map.insertWith (flip (++)) n observed (exposures current)})
    emit current _ = Right current

local :: Node -> R.RaftInput ByteString -> Network -> Either String Network
local n = stepNative n Nothing

expire :: Node -> Network -> Either String Network
expire n net = local n (R.fireRecentLeaderTimer (S.raftStateRecentLeaderGeneration (stateAt n net))) net

campaign :: Node -> Network -> Either String Network
campaign n net = do
  expired <- expire n net
  let generation = S.raftStateElectionGeneration (stateAt n expired)
  selected <- local n (R.selectElectionTimeout generation electionLower) expired
  changed <- local n (R.fireElectionTimer generation) selected
  expected <- campaignReference n (reference net)
  checkNetwork "campaign" (changed {reference = expected})

data Kind = VoteRequest | VoteResponse | AppendRequest | AppendResponse
  deriving stock (Eq, Show)

packetKind :: Packet -> Kind
packetKind (Packet _ _ _ _ rpc) = case R.raftRpcView rpc of
  R.RequestVoteView {} -> VoteRequest
  R.RequestVoteResponseView {} -> VoteResponse
  R.AppendEntriesView {} -> AppendRequest
  R.AppendEntriesResponseView {} -> AppendResponse
  R.InstallSnapshotView {} -> AppendRequest
  R.InstallSnapshotResponseView {} -> AppendResponse

selectPacket :: Node -> Node -> Kind -> Network -> Either String (Packet, Network)
selectPacket source target kind net = do
  (packet, remaining) <- pick (reverse (packets net))
  pure (packet, net {packets = reverse remaining})
  where
    pick [] = Left ("no " <> show kind <> " packet from " <> show source <> " to " <> show target)
    pick (p@(Packet from to _ _ _) : rest)
      | from == source && to == target && packetKind p == kind = Right (p, rest)
      | otherwise = do (selected, remaining) <- pick rest; pure (selected, p : remaining)

deliverPacket :: Packet -> Network -> Either String Network
deliverPacket packet@(Packet source target generation correlation rpc) net = do
  (input, reply) <- case packetKind packet of
    VoteRequest -> (,Just (source, generation)) <$> problem (R.observeRaftRequest source rpc)
    AppendRequest -> (,Just (source, generation)) <$> problem (R.observeRaftRequest source rpc)
    _ -> case correlation of
      Nothing -> Left "uncorrelated response in test transport"
      Just requestGeneration -> (,Nothing) <$> problem (R.observeRaftResponse source requestGeneration rpc)
  stepNative target reply input net

deliver :: Node -> Node -> Kind -> Network -> Either String Network
deliver source target kind net = do
  (packet, remaining) <- selectPacket source target kind net
  deliverPacket packet remaining

voteFor :: Node -> Node -> Network -> Either String Network
voteFor candidate voter net = do
  expired <- expire voter net
  requested <- deliver candidate voter VoteRequest expired
  delivered <- deliver voter candidate VoteResponse requested
  expected <- voteReference candidate voter (reference net)
  checkNetwork "vote exchange" (delivered {reference = expected})

heartbeatAt :: Node -> Network -> Either String Network
heartbeatAt n net = local n (R.fireHeartbeatTimer (S.raftStateHeartbeatGeneration (stateAt n net))) net

replicateTo :: Node -> Node -> Network -> Either String Network
replicateTo leader follower net = do
  sending <- heartbeatAt leader net
  delivered <- retry (8 :: Int) sending
  expected <- replicateReference leader follower (reference net)
  checkNetwork "full prefix exchange" (delivered {reference = expected})
  where
    retry 0 _ = Left "bounded test prefix retry did not converge"
    retry fuel current = do
      received <- deliver leader follower AppendRequest current
      (reply@(Packet _ _ _ _ rpc), rest) <- selectPacket follower leader AppendResponse received
      acknowledged <- deliverPacket reply rest
      case R.raftRpcView rpc of
        R.AppendEntriesResponseView _ _ (R.AppendAccepted _ _) -> Right acknowledged
        R.AppendEntriesResponseView responseTerm _ (R.AppendRejected _)
          | responseTerm > S.raftStateTerm (stateAt leader current) -> Right acknowledged
          | otherwise -> retry (fuel - 1) acknowledged
        _ -> Left "response kind mismatch"

learnWithoutAcknowledgement :: Node -> Node -> Network -> Either String Network
learnWithoutAcknowledgement leader follower net = do
  learned <- deliver leader follower AppendRequest net
  expected <- receiveReference leader follower (reference net)
  checkNetwork "append delivered but acknowledgement withheld" (learned {reference = expected})

replicateAll :: Node -> [Node] -> Network -> Either String Network
replicateAll leader targets net = foldM (flip (replicateTo leader)) net targets

drain :: Node -> Network -> Either String Network
drain n net = do
  let expected = at n (reference net)
  appliedNet <- foldM (\current i -> local n (R.acknowledgeCommittedEntries (I.raftLogIndex (fromIntegral i))) current) net [applied expected + 1 .. committed expected]
  checkNetwork "application acknowledgement" (appliedNet {reference = change n (\p -> p {applied = committed p}) (reference net)})

drainAll :: Network -> Either String Network
drainAll net = foldM (flip drain) net (Map.keys (live net))

settle :: Node -> [Node] -> Network -> Either String Network
settle leader targets net = replicateAll leader targets net >>= replicateAll leader targets >>= drainAll

proposal :: Word64 -> I.RaftProposalId
proposal = checked . I.mkRaftProposalId

appendApplication :: Node -> Word64 -> ByteString -> Network -> Either String Network
appendApplication n ident bytes net = do
  changed <- local n (R.proposeRaftApplication (proposal ident) bytes) net
  expected <- appendLocal n (App bytes) (reference net)
  checkNetwork "application proposal" (changed {reference = expected})

prepare :: Node -> [Node] -> Word64 -> Network -> Either String Network
prepare leader after ident net = do
  let targets = filter (/= leader) (Map.keys (live net))
  routed <- problem (R.updateRaftReplicationTargets targets) >>= (\input -> local leader input net)
  local leader (R.beginRaftConfigurationChange (proposal ident) (fst (S.raftStateEffectiveConfiguration (stateAt leader routed))) (checked (C.raftVoterSet after))) routed

readyLearners :: Node -> [Node] -> Network -> Either String Network
readyLearners leader newcomers net = do
  caught <- settle leader newcomers net
  frontier <- maybe (Left "leader did not capture a configuration frontier") Right (S.raftStateCatchUpFrontier (stateAt leader caught))
  foldM
    ( \current n -> do
        token <- maybe (Left "learner did not prove exact applied catch-up") Right (S.raftStateLearnerReadiness frontier (stateAt n current))
        local leader (R.observeRaftLearnerReadiness token) current
    )
    caught
    newcomers

appendConfiguration :: Node -> Word64 -> Configuration -> ByteString -> Network -> Either String Network
appendConfiguration n ident cfg metadata net = do
  changed <- local n (R.proposeRaftConfiguration (proposal ident) (fst (S.raftStateEffectiveConfiguration (stateAt n net))) (nativeConfiguration cfg) metadata) net
  expected <- appendLocal n (Config cfg metadata) (reference net)
  checkNetwork "configuration proposal" (changed {reference = expected})

joinConfiguration :: Node -> [Node] -> Word64 -> ByteString -> Network -> Either String Network
joinConfiguration leader after ident metadata net = do
  let oldMembers = members (effective leader (reference net))
      newcomers = filter (`Set.notMember` oldMembers) after
  prepared <- prepare leader after ident net >>= readyLearners leader newcomers
  appendConfiguration leader ident (Joint oldMembers (Set.fromList after)) metadata prepared

nodeA, nodeB, nodeC, nodeD, nodeE :: Node
nodeA = nodeId 1
nodeB = nodeId 2
nodeC = nodeId 3
nodeD = nodeId 4
nodeE = nodeId 5

oldNodes, newNodes, allNodes :: [Node]
oldNodes = [nodeA, nodeB, nodeC]
newNodes = [nodeC, nodeD, nodeE]
allNodes = [nodeA, nodeB, nodeC, nodeD, nodeE]

initialLeader :: [Node] -> [Node] -> Node -> Network -> Either String Network
initialLeader voters targets leader net = do
  started <- campaign leader net
  elected <- foldM (\current voter -> if role (at leader (reference current)) == Candidate then voteFor leader voter current else Right current) started (filter (/= leader) voters)
  settle leader targets elected

oldLeader :: Either String Network
oldLeader = initialLeader oldNodes [nodeB, nodeC] nodeA (newNetwork oldNodes allNodes)

jointFixture :: Either String Network
jointFixture = oldLeader >>= joinConfiguration nodeA newNodes 10 "joint"

committedJointFixture :: Either String Network
committedJointFixture = jointFixture >>= settle nodeA [nodeB, nodeC, nodeD, nodeE]

assertRun :: Either String value -> Assertion
assertRun = either assertFailure (const (pure ()))

require :: String -> Bool -> Either String ()
require message condition = unless condition (Left message)

singletonGrowth :: Assertion
singletonGrowth = assertRun (growth [nodeB, nodeC] "opaque joint" "opaque final")

-- Delivery and adapter acknowledgement are independent schedule steps. No
-- test-only readiness token is supplied: only ordinary correlated wire replies
-- can establish the captured frontier here.
appliedReplyFrontier :: Assertion
appliedReplyFrontier = assertRun $ mapM_ one (permutations [nodeB, nodeC])
  where
    one order = do
      founder <- initialLeader [nodeA] [] nodeA (newNetwork [nodeA] [nodeA, nodeB, nodeC])
      preparing <- prepare nodeA [nodeA, nodeB, nodeC] 10 founder
      matched <- replicateAll nodeA order preparing
      require "matching without adapter application promoted learners" (not (S.raftStateConfigurationChangeReady (stateAt nodeA matched)))
      applied <- drainAll matched
      require "local adapter progress leaked to the leader without a reply" (not (S.raftStateConfigurationChangeReady (stateAt nodeA applied)))
      case order of
        [first, second] -> do
          partlyReady <- replicateTo nodeA first applied
          require "one learner attestation admitted both learners" (not (S.raftStateConfigurationChangeReady (stateAt nodeA partlyReady)))
          ready <- replicateTo nodeA second partlyReady
          require "exact applied reply frontier did not admit promotion" (S.raftStateConfigurationChangeReady (stateAt nodeA ready))
        _ -> Left "invalid finite learner schedule"

growth :: [Node] -> ByteString -> ByteString -> Either String Network
growth order jointMetadata finalMetadata = do
  founder <- initialLeader [nodeA] [] nodeA (newNetwork [nodeA] [nodeA, nodeB, nodeC])
  started <- prepare nodeA [nodeA, nodeB, nodeC] 10 founder
  blocked <- local nodeA (R.proposeRaftApplication (proposal 11) "gated") started
  _ <- checkNetwork "preparation gates proposals" blocked
  caught <- readyLearners nodeA order blocked
  joint <- appendConfiguration nodeA 10 (Joint (Set.singleton nodeA) (Set.fromList [nodeA, nodeB, nodeC])) jointMetadata caught
  require "singleton old majority cannot commit joint alone" (committed (at nodeA (reference joint)) == 1)
  committedJoint <- settle nodeA order joint
  final <- appendConfiguration nodeA 12 (Stable (Set.fromList [nodeA, nodeB, nodeC])) finalMetadata committedJoint
  completed <- settle nodeA order final
  appended <- appendApplication nodeA 13 "post-growth" completed >>= settle nodeA order
  require "all replicas replayed the same exact no-op/configuration/application history" (all (\n -> history (at n (reference appended)) == history (at nodeA (reference appended))) order)
  pure appended

depose :: Node -> Node -> Network -> Either String Network
depose target challenger net = do
  let nextTerm = term (at target (reference net)) + 1
  request <- problem (R.requestVote (I.raftTerm nextTerm) challenger (I.raftLogIndex 0) (I.raftTerm 0))
  input <- problem (R.observeRaftRequest challenger request)
  changed <- expire target net >>= local target input
  let expected = change target (\p -> p {term = nextTerm, vote = Nothing, role = Follower}) (reference net)
  checkNetwork "higher-term stale-log candidate deposes leader" (changed {reference = expected})

evenElections :: Assertion
evenElections = assertRun $ mapM_ one [ns | size <- [2, 4], let ns = take size allNodes]
  where
    one [] = Left "empty finite election fixture"
    one ns@(leader : rest) = mapM_ (run ns leader) (permutations rest)
    run ns leader order = do
      started <- campaign leader (newNetwork ns ns)
      _ <- foldM (\current voter -> if role (at leader (reference current)) == Candidate then voteFor leader voter current else Right current) started order
      pure ()

jointSubsets :: Assertion
jointSubsets = assertRun $ do
  fixture <- jointFixture
  mapM_ (one fixture) (subsequences [nodeB, nodeC, nodeD, nodeE])
  where
    one fixture delivered = do
      result <- replicateAll nodeA delivered fixture
      let shouldCommit = quorum (Joint (Set.fromList oldNodes) (Set.fromList newNodes)) (Set.fromList (nodeA : delivered))
      require "joint commit diverges from independent two-majority predicate" ((committed (at nodeA (reference result)) >= 2) == shouldCommit)

evenJointSubsets :: Assertion
evenJointSubsets = assertRun $ do
  founder <- initialLeader [nodeA, nodeB] [nodeB] nodeA (newNetwork [nodeA, nodeB] allNodes)
  fixture <- joinConfiguration nodeA [nodeA, nodeC, nodeD, nodeE] 10 "even joint" founder
  mapM_ (jointSubset fixture) (subsequences [nodeB, nodeC, nodeD, nodeE])
  committedJoint <- settle nodeA [nodeB, nodeC, nodeD, nodeE] fixture
  final <- appendConfiguration nodeA 11 (Stable (Set.fromList [nodeA, nodeC, nodeD, nodeE])) "even final" committedJoint
  mapM_ (finalSubset final) (subsequences [nodeB, nodeC, nodeD, nodeE])
  where
    jointSubset fixture delivered = do
      result <- replicateAll nodeA delivered fixture
      let expected = nodeB `elem` delivered && length (filter (`elem` [nodeC, nodeD, nodeE]) delivered) >= 2
      require "two/four joint commit admitted a half quorum" ((committed (at nodeA (reference result)) >= 2) == expected)
    finalSubset fixture delivered = do
      result <- replicateAll nodeA delivered fixture
      let expected = length (filter (`elem` [nodeC, nodeD, nodeE]) delivered) >= 2
      require "four-voter final commit counted half or an excluded old voter" ((committed (at nodeA (reference result)) >= 3) == expected)

newlyLoggedElection :: Assertion
newlyLoggedElection = assertRun $ do
  joint <- jointFixture
  learned <- foldM (flip (learnWithoutAcknowledgement nodeA)) joint [nodeB, nodeC, nodeD, nodeE]
  require "joint accidentally committed before the new learner's campaign" (all (\n -> committed (at n (reference learned)) == 1) allNodes)
  first <- campaign nodeD learned >>= voteFor nodeD nodeB >>= voteFor nodeD nodeE
  require "new learner elected without old majority" (role (at nodeD (reference first)) == Candidate)
  elected <- voteFor nodeD nodeC first
  require "committed genesis absence deposed newly included joint voter" (role (at nodeD (reference elected)) == Leader && committed (at nodeD (reference elected)) == 1)
  settled <- settle nodeD [nodeB, nodeC, nodeE] elected
  require "new leader did not commit inherited joint through its current-term no-op" (committed (at nodeD (reference settled)) == 3)

recommissionedElection :: Assertion
recommissionedElection = assertRun $ do
  joint <- committedJointFixture
  elected <- campaign nodeC joint >>= voteFor nodeC nodeB >>= voteFor nodeC nodeD
  settled <- settle nodeC [nodeA, nodeB, nodeD, nodeE] elected
  routes <- problem (R.updateRaftReplicationTargets [nodeA, nodeB, nodeD, nodeE])
  registered <- local nodeC routes settled
  final <- appendConfiguration nodeC 20 (Stable (Set.fromList newNodes)) "remove A" registered >>= settle nodeC [nodeA, nodeB, nodeD, nodeE]
  require "fixture did not commit A's removal" (nodeA `Set.notMember` members (committedConfiguration nodeA (reference final)))
  nextJoint <- joinConfiguration nodeC [nodeA, nodeC, nodeD] 30 "recommission A" final
  learned <- learnWithoutAcknowledgement nodeC nodeA nextJoint
  started <- campaign nodeA learned >>= voteFor nodeA nodeC
  electedAgain <- voteFor nodeA nodeD started
  require "historical committed exclusion overrode later logged inclusion" (role (at nodeA (reference electedAgain)) == Leader && committed (at nodeA (reference electedAgain)) == 4)
  _ <- settle nodeA [nodeC, nodeD] electedAgain
  pure ()

jointElectionSubsets :: Assertion
jointElectionSubsets = assertRun $ do
  fixture <- committedJointFixture
  mapM_ (one fixture) (subsequences [nodeA, nodeB, nodeD, nodeE])
  where
    one fixture voters = do
      started <- campaign nodeC fixture
      finished <- foldM (\current voter -> if role (at nodeC (reference current)) == Candidate then voteFor nodeC voter current else Right current) started voters
      require "joint election diverges from independent two-majority predicate" ((role (at nodeC (reference finished)) == Leader) == quorum (Joint (Set.fromList oldNodes) (Set.fromList newNodes)) (Set.fromList (nodeC : voters)))

splitPartitions :: Assertion
splitPartitions = assertRun $ do
  joint <- jointFixture
  -- C bridges both sets, while the two disjoint partitions see the logged joint.
  known <- replicateAll nodeA [nodeB, nodeD, nodeE] joint
  left <- campaign nodeB known >>= voteFor nodeB nodeA
  right <- campaign nodeD left >>= voteFor nodeD nodeE
  require "old-only partition elected" (role (at nodeB (reference right)) == Candidate)
  require "new-only partition elected" (role (at nodeD (reference right)) == Candidate)

partialKnowledge :: Assertion
partialKnowledge = assertRun $ do
  joint <- jointFixture
  committedJoint <- settle nodeA [nodeB, nodeD, nodeE] joint
  final <- appendConfiguration nodeA 20 (Stable (Set.fromList newNodes)) "partly learned final" committedJoint
  removed <- replicateTo nodeA nodeD final >>= replicateTo nodeA nodeE
  require "fixture lacks three unequal log-effective configurations" (effective nodeC (reference removed) == Stable (Set.fromList oldNodes) && effective nodeB (reference removed) == Joint (Set.fromList oldNodes) (Set.fromList newNodes) && effective nodeD (reference removed) == Stable (Set.fromList newNodes))
  oldCandidate <- campaign nodeC removed >>= voteFor nodeC nodeB
  require "stale old group elected a leader missing the committed transition" (role (at nodeC (reference oldCandidate)) == Candidate)
  newLeader <- campaign nodeD oldCandidate >>= voteFor nodeD nodeE
  require "learned final majority cannot elect" (role (at nodeD (reference newLeader)) == Leader)
  _ <- settle nodeD [nodeE] newLeader
  pure ()

delayedPackets :: Assertion
delayedPackets = assertRun $ do
  ready <- oldLeader >>= heartbeatAt nodeA
  (oldAppend, withoutAppend) <- selectPacket nodeA nodeB AppendRequest ready
  received <- deliverPacket oldAppend withoutAppend
  (oldAck, withoutAck) <- selectPacket nodeB nodeA AppendResponse received
  -- Neither delivery changes the reference's already-settled prefix.
  joint <- joinConfiguration nodeA newNodes 10 "later joint" withoutAck >>= settle nodeA [nodeB, nodeC, nodeD, nodeE]
  delayed <- deliverPacket oldAppend joint
  _ <- checkNetwork "shorter delayed append does not erase logged configuration" delayed
  replayed <- deliverPacket oldAck delayed
  _ <- checkNetwork "old correlated reply cannot change new configuration commit" replayed
  elected <- campaign nodeC replayed >>= voteFor nodeC nodeB >>= voteFor nodeC nodeD
  repaired <- settle nodeC [nodeA, nodeB, nodeD, nodeE] elected
  stale <- deliverPacket oldAck repaired
  _ <- checkNetwork "reply from an earlier term is inert after leader loss" stale
  pure ()

jointLeaderLoss :: Assertion
jointLeaderLoss = assertRun $ do
  fixture <- committedJointFixture
  elected <- campaign nodeC fixture >>= voteFor nodeC nodeB >>= voteFor nodeC nodeD
  settled <- settle nodeC [nodeB, nodeD, nodeE] elected
  final <- appendConfiguration nodeC 20 (Stable (Set.fromList newNodes)) "new leader final" settled
  _ <- settle nodeC [nodeD, nodeE] final
  pure ()

excludedCampaign :: Assertion
excludedCampaign = assertRun $ do
  joint <- committedJointFixture
  final <- appendConfiguration nodeA 20 (Stable (Set.fromList newNodes)) "uncommitted final" joint
  require "excluded leader counts itself for final commit" (committed (at nodeA (reference final)) == 2)
  learnedFinal <- replicateTo nodeA nodeC final
  deposed <- depose nodeA nodeB learnedFinal
  started <- campaign nodeA deposed
  require "uncommitted exclusion forbids previous voter campaign" (role (at nodeA (reference started)) == Candidate)
  require "excluded candidate voted for itself" (vote (at nodeA (reference started)) == Nothing)
  oneVote <- voteFor nodeA nodeC started
  require "excluded candidate counted its own vote toward new majority" (role (at nodeA (reference oneVote)) == Candidate)
  elected <- voteFor nodeA nodeD oneVote
  require "new voters refused a candidate absent from their final set" (role (at nodeA (reference elected)) == Leader)
  replicated <- replicateTo nodeA nodeC elected >>= replicateTo nodeA nodeD
  require "self-excluded leader failed to replicate and commit final plus current-term no-op" (committed (at nodeA (reference replicated)) == 4 && role (at nodeA (reference replicated)) == Follower)

committedRemoval :: Assertion
committedRemoval = assertRun $ do
  joint <- committedJointFixture
  final <- appendConfiguration nodeA 20 (Stable (Set.fromList newNodes)) "final" joint
  sent <- replicateTo nodeA nodeC final
  require "one new vote cannot commit final" (committed (at nodeA (reference sent)) == 2)
  removed <- replicateTo nodeA nodeD sent
  require "self-excluded leader remains leader after committed final" (role (at nodeA (reference removed)) == Follower)
  drained <- drain nodeA removed
  _ <- campaign nodeA drained
  pure ()

rollbackJoint :: Assertion
rollbackJoint = assertRun $ do
  joint <- jointFixture
  -- B and C retain the old prefix, so B can replace the isolated leader's
  -- uncommitted joint with a higher-term no-op.
  elected <- campaign nodeB joint >>= voteFor nodeB nodeC
  repaired <- replicateTo nodeB nodeA elected
  require "joint suffix survived conflict repair" (effective nodeA (reference repaired) == Stable (Set.fromList oldNodes))

rollbackFinal :: Assertion
rollbackFinal = assertRun $ do
  joint <- committedJointFixture
  final <- appendConfiguration nodeA 20 (Stable (Set.fromList newNodes)) "uncommitted" joint
  elected <- campaign nodeC final >>= voteFor nodeC nodeB >>= voteFor nodeC nodeD
  repaired <- replicateTo nodeC nodeA elected
  require "final suffix survived conflict repair" (effective nodeA (reference repaired) == Joint (Set.fromList oldNodes) (Set.fromList newNodes))

learnerExclusion :: Assertion
learnerExclusion = assertRun $ do
  leader <- oldLeader
  learner <- campaign nodeD leader
  require "learner campaigned" (term (at nodeD (reference learner)) == 0)
  routed <- problem (R.updateRaftReplicationTargets [nodeB, nodeC, nodeD, nodeE]) >>= (\input -> local nodeA input learner)
  proposed <- appendApplication nodeA 30 "minority suffix" routed
  acknowledged <- replicateAll nodeA [nodeD, nodeE] proposed
  require "two learners counted toward old majority" (committed (at nodeA (reference acknowledged)) == 1)
  -- A genuine candidate's request is addressed explicitly to the learner; its
  -- reply is not usable as a voter acknowledgement.
  let d = stateAt nodeD acknowledged
      oldTerm = S.raftStateTerm d
  request <- problem (R.requestVote (I.raftTerm (I.raftTermWord64 oldTerm + 1)) nodeB (S.raftStateLastLogIndex d) (S.raftStateLastLogTerm d))
  expired <- expire nodeD acknowledged
  input <- problem (R.observeRaftRequest nodeB request)
  refused <- local nodeD input expired
  require "learner granted a vote" (S.raftStateVotedFor (stateAt nodeD refused) == Nothing)

inflatedGuard :: Assertion
inflatedGuard = assertRun $ do
  ready <- oldLeader
  guarded <- replicateTo nodeA nodeB ready
  let before = stateAt nodeB guarded
      inflatedTerm = I.raftTerm (I.raftTermWord64 (S.raftStateTerm before) + 17)
  request <- problem (R.requestVote inflatedTerm nodeD (S.raftStateLastLogIndex before) (S.raftStateLastLogTerm before))
  input <- problem (R.observeRaftRequest nodeD request)
  protected <- local nodeB input guarded
  require "recent leader guard did not reject inflated term before updating term" (S.raftStateTerm (stateAt nodeB protected) == S.raftStateTerm before)
  require "guarded vote changed selected voter" (S.raftStateVotedFor (stateAt nodeB protected) == S.raftStateVotedFor before)
  expired <- expire nodeB protected
  acceptedTerm <- local nodeB input expired
  require "expired guard suppressed ordinary higher-term vote handling" (S.raftStateTerm (stateAt nodeB acceptedTerm) == inflatedTerm)

asymmetricGuard :: Assertion
asymmetricGuard = assertRun $ do
  ready <- oldLeader >>= replicateTo nodeA nodeB
  let before = stateAt nodeB ready
  -- C receives no leader traffic and repeatedly times out. Its requests can
  -- reach B, while B still has recent leader contact. No symmetric partition is
  -- assumed and C has already inflated its term before it contacts B.
  isolated <- foldM (\current _ -> campaign nodeC current) ready [1 .. 4 :: Int]
  received <- deliver nodeC nodeB VoteRequest isolated
  require "asymmetric higher-term vote disrupted healthy follower" (S.raftStateTerm (stateAt nodeB received) == S.raftStateTerm before)
  responded <- deliver nodeB nodeC VoteResponse received
  require "isolated candidate won without a voting majority" (S.raftStateRole (stateAt nodeC responded) /= S.RaftLeader)

guardRenewal :: Assertion
guardRenewal = assertRun $ do
  ready <- oldLeader
  expired <- expire nodeA ready
  sends <- foldM (\current _ -> heartbeatAt nodeA current) expired [1 .. 4 :: Int]
  require "sending heartbeats renewed recent-leader guard" (not (S.raftStateRecentLeaderGuardActive (stateAt nodeA sends)))
  received <- deliver nodeA nodeC AppendRequest sends
  (ack, rest) <- selectPacket nodeC nodeA AppendResponse received
  renewed <- deliverPacket ack rest
  require "fresh quorum response did not renew leader guard" (S.raftStateRecentLeaderGuardActive (stateAt nodeA renewed))
  expiredAgain <- expire nodeA renewed
  duplicated <- deliverPacket ack expiredAgain
  require "duplicate append acknowledgement renewed expired guard" (not (S.raftStateRecentLeaderGuardActive (stateAt nodeA duplicated)))

jointGuardRenewal :: Assertion
jointGuardRenewal = assertRun $ do
  ready <- oldLeader
  let oldTimer = S.raftStateRecentLeaderGeneration (stateAt nodeA ready)
  joint <- joinConfiguration nodeA newNodes 10 "guard configuration" ready
  oldMajority <- replicateTo nodeA nodeB joint
  require "old majority renewed a joint recent-leader guard" (not (S.raftStateRecentLeaderGuardActive (stateAt nodeA oldMajority)))
  allOld <- replicateTo nodeA nodeC oldMajority
  require "whole old set substituted for missing new majority" (not (S.raftStateRecentLeaderGuardActive (stateAt nodeA allOld)))
  both <- replicateTo nodeA nodeD allOld
  require "fresh old/new majorities did not renew joint guard" (S.raftStateRecentLeaderGuardActive (stateAt nodeA both))
  staleTimer <- local nodeA (R.fireRecentLeaderTimer oldTimer) both
  require "old configuration timer expired newer guard" (S.raftStateRecentLeaderGuardActive (stateAt nodeA staleTimer))
  expired <- expire nodeA staleTimer
  repeated <- foldM (\current _ -> replicateTo nodeA nodeB current) expired [1 .. 3 :: Int]
  require "one repeated old voter reused previous cohort's new majority" (not (S.raftStateRecentLeaderGuardActive (stateAt nodeA repeated)))

partialGuardExpiry :: Assertion
partialGuardExpiry = assertRun $ do
  ready <- initialLeader allNodes [nodeB, nodeC, nodeD, nodeE] nodeA (newNetwork allNodes allNodes)
  unguarded <- expire nodeA ready
  let expiredGeneration = S.raftStateRecentLeaderGeneration (stateAt nodeA unguarded)
  -- First retire any request issued before the preceding guard expired.
  retiredB <- replicateTo nodeA nodeB unguarded
  first <- replicateTo nodeA nodeB retiredB
  let observationGeneration = S.raftStateRecentLeaderGeneration (stateAt nodeA first)
  require "one of four remote voters incorrectly protected the five-voter leader" (not (S.raftStateRecentLeaderGuardActive (stateAt nodeA first)))
  require "partial evidence did not arm its own observation expiry" (observationGeneration > expiredGeneration)
  -- Repeated fresh replies from B and local sends cannot move the cohort's
  -- deadline. Only a complete majority starts a new protection interval.
  repeated <- replicateTo nodeA nodeB first >>= heartbeatAt nodeA >>= heartbeatAt nodeA
  require "an insufficient repeated voter extended the observation window" (S.raftStateRecentLeaderGeneration (stateAt nodeA repeated) == observationGeneration)
  expired <- expire nodeA repeated
  -- The first C reply was dispatched before expiry and is discarded for quorum
  -- freshness. A new C request still cannot combine with B's expired evidence.
  delayed <- replicateTo nodeA nodeC expired
  require "a pre-expiry request opened a fresh observation window" (S.raftStateRecentLeaderGeneration (stateAt nodeA delayed) == observationGeneration)
  second <- replicateTo nodeA nodeC delayed
  let secondGeneration = S.raftStateRecentLeaderGeneration (stateAt nodeA second)
  require "old partial B evidence combined with a much later C reply" (not (S.raftStateRecentLeaderGuardActive (stateAt nodeA second)))
  require "a new partial cohort lacks an expiry" (secondGeneration > observationGeneration)
  staleExpiry <- local nodeA (R.fireRecentLeaderTimer observationGeneration) second
  require "old window expiry changed a newer observation window" (S.raftStateRecentLeaderGeneration (stateAt nodeA staleExpiry) == secondGeneration)
  -- Consume B's pre-expiry outstanding request, then observe a new B reply in
  -- C's current window. Self, B and C now form a fresh independent majority.
  oldB <- replicateTo nodeA nodeB staleExpiry
  require "a delayed B reply completed a new cohort" (not (S.raftStateRecentLeaderGuardActive (stateAt nodeA oldB)))
  complete <- replicateTo nodeA nodeB oldB
  require "fresh B and C failed to protect the five-voter leader" (S.raftStateRecentLeaderGuardActive (stateAt nodeA complete))

growthPermutation :: QC.Property
growthPermutation = QC.forAll (QC.elements (permutations [nodeB, nodeC])) $ \order ->
  QC.forAll (QC.elements ["", "joint", "\NUL\255opaque"]) $ \metadata ->
    case growth order metadata (metadata <> " final") of
      Left err -> QC.counterexample err False
      Right _ -> QC.property True

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

problem :: (Show issue) => Either issue value -> Either String value
problem = either (Left . show) Right
