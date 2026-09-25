-- | Prospective profile-0.2 proof contracts, independent of production Raft.
--
-- This deliberately small reference machine keeps physical per-replica logs,
-- current terms, term-qualified votes, logged configurations and commit indices.
-- Its network consists of explicit full-prefix append messages and separate
-- acknowledgements: full-prefix transfer abstracts the ordinary prefix-matching
-- retry loop, but never truncates a matching longer log merely because a delayed
-- shorter message arrives. Crashes stop participation permanently; no restart,
-- persistence, clock, transport, Oracle adapter implementation or fairness proof
-- is represented. Test schedule lengths are finite examples, not resource limits.
--
-- Safety observations are independent of the quorum predicate: every elected
-- leader must contain every previously committed entry, commits must agree, and
-- append repair must not replace a locally committed entry. The bad policies
-- below are intentional mutants, each accompanied by a named counterexample.
-- Passing these contracts certifies this reference model only. Live-owner tests
-- must separately connect these obligations to the eventual implementation.
module RaftContracts (tests) where

import Data.List (find, permutations, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck qualified as QC

data Node = A | B | C | D | E
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Configuration = Stable (Set Node) | Joint (Set Node) (Set Node)
  deriving stock (Eq, Show)

data Payload = NoOp | Application Int | Configure Configuration
  deriving stock (Eq, Show)

data Entry = Entry Int Payload
  deriving stock (Eq, Show)

data Role = Follower | Candidate | Leader
  deriving stock (Eq, Show)

data Replica = Replica
  { term :: Int,
    votedFor :: Maybe Node,
    role :: Role,
    physicalLog :: [Entry],
    committedThrough :: Int,
    appliedThrough :: Int,
    matches :: Map Node Int,
    votes :: Set Node
  }
  deriving stock (Eq, Show)

data Policy
  = Correct
  | ElectionNeedsRemoteResponse
  | CommitNeedsRemoteResponse
  | UnionMajority
  | ElectionsUseGenesis
  deriving stock (Eq, Show)

data Fault
  = LeaderMissingCommitted Node Int
  | ConflictingCommit Int
  | OverwritesCommitted Node Int
  deriving stock (Eq, Show)

data Machine = Machine
  { genesis :: Configuration,
    replicas :: Map Node Replica,
    crashed :: Set Node,
    knownCommitted :: Map Int Entry,
    faults :: [Fault],
    policy :: Policy
  }
  deriving stock (Eq, Show)

-- An append snapshot retains its original term and commit watermark when delayed.
data Append = Append Node Node Int [Entry] Int
  deriving stock (Eq, Show)

data Ack = Ack Node Node Int Int
  deriving stock (Eq, Show)

old, new :: Set Node
old = Set.fromList [A, B, C]
new = Set.fromList [C, D, E]

blank :: Replica
blank = Replica 0 Nothing Follower [] 0 0 Map.empty Set.empty

replica :: Node -> Machine -> Replica
replica n m = (replicas m) Map.! n

alterReplica :: Node -> (Replica -> Replica) -> Machine -> Machine
alterReplica n f m = m {replicas = Map.adjust f n (replicas m)}

running :: Node -> Machine -> Bool
running n m = Map.member n (replicas m) && Set.notMember n (crashed m)

configurationAt :: Configuration -> [Entry] -> Configuration
configurationAt initial = foldl step initial
  where
    step _ (Entry _ (Configure c)) = c
    step c _ = c

effective :: Node -> Machine -> Configuration
effective n m = configurationAt (genesis m) (physicalLog (replica n m))

members :: Configuration -> Set Node
members (Stable ns) = ns
members (Joint before after) = Set.union before after

majority :: Set Node -> Set Node -> Bool
majority ns acknowledgers = 2 * Set.size (Set.intersection ns acknowledgers) > Set.size ns

quorum :: Policy -> Configuration -> Set Node -> Bool
quorum UnionMajority c ns = majority (members c) ns
quorum _ (Stable configured) ns = majority configured ns
quorum _ (Joint before after) ns = majority before ns && majority after ns

freshness :: Replica -> (Int, Int)
freshness r = (maybe 0 entryTerm (lastMaybe (physicalLog r)), length (physicalLog r))
  where
    entryTerm (Entry t _) = t

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (last xs)

-- Former voters excluded only by an uncommitted final configuration can still
-- campaign, without counting their own vote. Committed exclusion ends this right.
eligibleToCampaign :: Node -> Machine -> Bool
eligibleToCampaign n m =
  Set.member n (members (effective n m))
    || Set.member n (members committedConfiguration)
  where
    r = replica n m
    committedConfiguration = configurationAt (genesis m) (take (committedThrough r) (physicalLog r))

recordCommit :: Node -> Int -> Machine -> Machine
recordCommit n index m =
  updated
    { knownCommitted = Map.union (knownCommitted m) newlyCommitted,
      faults = faults m ++ disagreements
    }
  where
    updated = alterReplica n (\r -> r {committedThrough = max index (committedThrough r)}) m
    newlyCommitted = Map.fromList (zip [1 ..] (take index (physicalLog (replica n m))))
    disagreements =
      [ ConflictingCommit i
      | (i, entry) <- Map.toList newlyCommitted,
        Just previous <- [Map.lookup i (knownCommitted m)],
        entry /= previous
      ]

advanceCommit :: Node -> Machine -> Machine
advanceCommit n m
  | not (running n m) || role r /= Leader = m
  | otherwise = stepDownAfterExclusion (recordCommit n next m)
  where
    r = replica n m
    eligibleIndices =
      [ i
      | (i, Entry t _) <- zip [1 ..] (physicalLog r),
        t == term r,
        quorum (policy m) (effective n m) (Map.keysSet (Map.filter (>= i) (matches r)))
      ]
    next = maximum (committedThrough r : eligibleIndices)
    stepDownAfterExclusion state
      | Set.member n (members committedConfiguration) = state
      | otherwise = alterReplica n (\p -> p {role = Follower}) state
      where
        committedConfiguration =
          configurationAt
            (genesis state)
            (take next (physicalLog (replica n state)))

appendLocal :: Node -> Payload -> Machine -> Machine
appendLocal n payload m
  | not (running n m) || role r /= Leader = m
  | policy m == CommitNeedsRemoteResponse = appended
  | otherwise = advanceCommit n appended
  where
    r = replica n m
    appended =
      alterReplica
        n
        ( \p ->
            p
              { physicalLog = physicalLog p ++ [Entry (term p) payload],
                matches = Map.insert n (length (physicalLog p) + 1) (matches p)
              }
        )
        m

electIfQuorate :: Node -> Machine -> Machine
electIfQuorate n m
  | role r /= Candidate || not (quorum (policy m) electionConfiguration (votes r)) = m
  | otherwise = appendLocal n NoOp elected
  where
    r = replica n m
    electionConfiguration = if policy m == ElectionsUseGenesis then genesis m else effective n m
    missing =
      [ LeaderMissingCommitted n i
      | (i, entry) <- Map.toList (knownCommitted m),
        take 1 (drop (i - 1) (physicalLog r)) /= [entry]
      ]
    elected =
      ( alterReplica
          n
          ( \p ->
              p
                { role = Leader,
                  matches = Map.singleton n (length (physicalLog p))
                }
          )
          m
      )
        { faults = faults m ++ missing
        }

campaign :: Node -> Machine -> Machine
campaign n m
  | not (running n m) || not (eligibleToCampaign n m) = m
  | policy m == ElectionNeedsRemoteResponse = started
  | otherwise = electIfQuorate n started
  where
    selfVotes = if Set.member n (members (effective n m)) then Set.singleton n else Set.empty
    started =
      alterReplica
        n
        ( \r ->
            r
              { term = term r + 1,
                role = Candidate,
                votedFor = if Set.null selfVotes then Nothing else Just n,
                votes = selfVotes
              }
        )
        m

requestVote :: Node -> Node -> Machine -> Machine
requestVote candidate voter m
  | candidate == voter || not (running candidate m && running voter m) = m
  | role candidateState /= Candidate = m
  | term candidateState < term voterState =
      alterReplica
        candidate
        (\r -> r {term = term voterState, votedFor = Nothing, role = Follower})
        m
  | otherwise = if grants then electIfQuorate candidate counted else refreshed
  where
    candidateState = replica candidate m
    voterState = replica voter m
    refreshed =
      if term candidateState > term voterState
        then alterReplica voter (\r -> r {term = term candidateState, votedFor = Nothing, role = Follower}) m
        else m
    voterNow = replica voter refreshed
    grants =
      Set.member voter (members (effective voter refreshed))
        && maybe True (== candidate) (votedFor voterNow)
        && freshness candidateState >= freshness voterNow
    counted =
      alterReplica
        candidate
        (\r -> r {votes = Set.insert voter (votes r)})
        (alterReplica voter (\r -> r {votedFor = Just candidate}) refreshed)

captureAppend :: Node -> Node -> Machine -> Maybe Append
captureAppend leader follower m
  | leader /= follower && running leader m && role r == Leader = Just (Append leader follower (term r) (physicalLog r) (committedThrough r))
  | otherwise = Nothing
  where
    r = replica leader m

deliverAppend :: Append -> Machine -> (Machine, Maybe Ack)
deliverAppend (Append leader follower messageTerm incoming leaderCommit) m
  | not (running follower m) || messageTerm < term r = (m, Nothing)
  | otherwise =
      ( recordCommit follower (min leaderCommit (length incoming)) received,
        Just (Ack leader follower messageTerm (length incoming))
      )
  where
    r = replica follower m
    common = length (takeWhile id (zipWith (==) (physicalLog r) incoming))
    -- A shorter, matching message does not delete the receiver's extra suffix.
    repaired = if common == length incoming then physicalLog r else incoming
    overwritten =
      [ OverwritesCommitted follower i
      | i <- [1 .. committedThrough r],
        take 1 (drop (i - 1) repaired) /= take 1 (drop (i - 1) (physicalLog r))
      ]
    received =
      ( alterReplica
          follower
          ( \p ->
              p
                { term = messageTerm,
                  votedFor = if messageTerm > term p then Nothing else votedFor p,
                  role = Follower,
                  physicalLog = repaired
                }
          )
          m
      )
        { faults = faults m ++ overwritten
        }

deliverAck :: Ack -> Machine -> Machine
deliverAck (Ack leader follower messageTerm matched) m
  | not (running leader m) || role r /= Leader || term r /= messageTerm = m
  | otherwise =
      advanceCommit
        leader
        (alterReplica leader (\p -> p {matches = Map.insertWith max follower matched (matches p)}) m)
  where
    r = replica leader m

deliverAndAck :: Append -> Machine -> Machine
deliverAndAck message m = maybe afterAppend (`deliverAck` afterAppend) ack
  where
    (afterAppend, ack) = deliverAppend message m

replicateTo :: Node -> Node -> Machine -> Machine
replicateTo leader follower m = maybe m (`deliverAndAck` m) (captureAppend leader follower m)

replicateInOrder :: Node -> [Node] -> Machine -> Machine
replicateInOrder leader order m = foldl (flip (replicateTo leader)) m order

crash :: Node -> Machine -> Machine
crash n m = m {crashed = Set.insert n (crashed m)}

applyCommitted :: Node -> Machine -> Machine
applyCommitted n = alterReplica n (\r -> r {appliedThrough = committedThrough r})

serviceReady :: Node -> Machine -> Bool
serviceReady n m = role r == Leader && any isCurrentNoOp (take (appliedThrough r) (physicalLog r))
  where
    r = replica n m
    isCurrentNoOp (Entry t payload) = t == term r && payload == NoOp

freshSingleton :: Policy -> Machine
freshSingleton p = Machine (Stable (Set.singleton A)) (Map.singleton A blank) Set.empty Map.empty [] p

-- The five live replicas share a previously committed term-1 no-op. D and E are
-- caught-up learners: having a physical log does not grant them a genesis vote.
established :: Policy -> Machine
established p = Machine (Stable old) states Set.empty (Map.singleton 1 initial) [] p
  where
    initial = Entry 1 NoOp
    ordinary = blank {term = 1, physicalLog = [initial], committedThrough = 1, appliedThrough = 1}
    leader = ordinary {role = Leader, votedFor = Just A, matches = Map.fromList [(n, 1) | n <- [A .. E]]}
    states = Map.fromList [(n, if n == A then leader else ordinary) | n <- [A .. E]]

startJoint :: Policy -> Machine
startJoint = appendLocal A (Configure (Joint old new)) . established

leaderAmong :: [Node] -> Machine -> Maybe Node
leaderAmong ns m = find (\n -> running n m && role (replica n m) == Leader) ns

electWith :: Node -> [Node] -> Machine -> Machine
electWith n voters m = foldl (flip (requestVote n)) (campaign n m) voters

-- The scheduler tries candidates in descending physical log freshness. It makes
-- no unconditional liveness claim: partial joint dissemination can leave the
-- required voters unavailable. Safety must hold for every interrupted prefix.
recoverElection :: Machine -> Machine
recoverElection initial = foldl tryCandidate initial candidates
  where
    survivors = [B, C, D, E]
    candidates = sortOn (Down . freshness . (`replica` initial)) survivors
    tryCandidate m n = case leaderAmong survivors m of
      Just _ -> m
      Nothing -> electWith n survivors m

interruptedJoint :: [Node] -> Int -> Machine
interruptedJoint order delivered = afterDelayed
  where
    joint = startJoint Correct
    snapshots = mapMaybe (\n -> captureAppend A n joint) order
    beforeLoss = foldl (flip deliverAndAck) joint (take delivered snapshots)
    elected = recoverElection (crash A beforeLoss)
    replicated = case leaderAmong [B, C, D, E] elected of
      Nothing -> elected
      Just leader -> replicateInOrder leader [B, C, D, E] elected
    afterDelayed = foldl (\m message -> fst (deliverAppend message m)) replicated (drop delivered snapshots)

-- Counterexample: {A,D,E} is a union majority, while old majority {B,C}
-- never received Joint. After A disappears, B and C elect B with their old
-- logs. Its current-term no-op replaces index 2 at D/E. Under correct joint
-- quorums index 2 was uncommitted, so this same physical repair is safe.
unionCounterexample :: Policy -> (Machine, Machine)
unionCounterexample p = (beforeLoss, repaired)
  where
    beforeLoss = replicateInOrder A [D, E] (startJoint p)
    elected = electWith B [C] (crash A beforeLoss)
    repaired = replicateInOrder B [C, D, E] elected

-- Counterexample: everyone logs/commits Joint, then only D/E receive final
-- Stable new. Their new majority commits it, and A gives up leadership. B/C
-- retain Joint. A candidate using stale genesis voters wrongly wins with B/C;
-- D/E correctly refuse its less up-to-date log. Correct Joint requires their
-- additional new-set vote and therefore rejects this election.
staleElectionCounterexample :: Policy -> (Machine, Machine)
staleElectionCounterexample p = (finalCommitted, attempted)
  where
    jointCommitted = replicateInOrder A [B, C, D, E] (startJoint p)
    -- A second replication conveys commitment of the joint prefix to all.
    aligned = replicateInOrder A [B, C, D, E] jointCommitted
    finalAppended = appendLocal A (Configure (Stable new)) aligned
    finalCommitted = crash A (replicateInOrder A [D, E] finalAppended)
    attempted = electWith B [D, E, C] finalCommitted

tests :: TestTree
tests =
  testGroup
    "prospective singleton and joint-configuration contracts"
    [ testGroup
        "singleton local progress"
        [ QC.testProperty "self vote and local append commit every generated finite application sequence"
            $ QC.forAll (QC.resize 12 (QC.listOf (QC.chooseInt (-20, 20))))
            $ \values ->
              let elected = campaign A (freshSingleton Correct)
                  ready = applyCommitted A elected
                  completed = foldl (\m value -> applyCommitted A (appendLocal A (Application value) m)) ready values
                  applied = take (appliedThrough (replica A completed)) (physicalLog (replica A completed))
               in QC.counterexample (show completed)
                    $ role (replica A elected) == Leader
                      && committedThrough (replica A elected) == 1
                      && not (serviceReady A elected)
                      && serviceReady A ready
                      && applied == Entry 1 NoOp : map (Entry 1 . Application) values
                      && null (faults completed),
          testCase "counterexample: election victory only after a remote response stalls the singleton" $ do
            let broken = campaign A (freshSingleton ElectionNeedsRemoteResponse)
            role (replica A broken) @?= Candidate
            physicalLog (replica A broken) @?= []
            assertBool
              "the correct trace reaches readiness without receiving any RPC"
              (serviceReady A (applyCommitted A (campaign A (freshSingleton Correct)))),
          testCase "counterexample: commit advancement only after a remote response stalls readiness" $ do
            let broken = campaign A (freshSingleton CommitNeedsRemoteResponse)
            role (replica A broken) @?= Leader
            physicalLog (replica A broken) @?= [Entry 1 NoOp]
            committedThrough (replica A broken) @?= 0
            assertBool
              "applying an empty committed prefix cannot make the leader ready"
              (not (serviceReady A (applyCommitted A broken)))
        ],
      testGroup
        "joint configuration and leader loss"
        [ testCase "all 120 joint-delivery prefixes preserve commit safety across leader loss and delayed traffic" $ do
            let schedules = [(order, count) | order <- permutations [B, C, D, E], count <- [0 .. 4]]
                failures =
                  [ (order, count, faults result)
                  | (order, count) <- schedules,
                    let result = interruptedJoint order count,
                    not (null (faults result))
                  ]
            length schedules @?= 120
            failures @?= [],
          QC.testProperty "a disseminated joint configuration survives leader loss and finishes under either delivery order"
            $ QC.forAll (QC.shuffle [B, D, E])
            $ \voteOrder ->
              QC.forAll (QC.shuffle [B, D, E]) $ \replicationOrder ->
                let joint = replicateInOrder A [B, C, D, E] (startJoint Correct)
                    elected = electWith C voteOrder (crash A joint)
                    inherited = replicateInOrder C replicationOrder elected
                    finished = replicateInOrder C replicationOrder (appendLocal C (Configure (Stable new)) inherited)
                    committed = take (committedThrough (replica C finished)) (physicalLog (replica C finished))
                 in QC.counterexample (show finished)
                      $ role (replica C elected) == Leader
                        && configurationAt (genesis finished) committed == Stable new
                        && Entry 1 (Configure (Joint old new)) `elem` committed
                        && Entry 2 NoOp `elem` committed
                        && null (faults finished),
          testCase "counterexample: union-majority commit loses Joint to an elected old-configuration leader" $ do
            let (correctBefore, correctAfter) = unionCounterexample Correct
                (brokenBefore, brokenAfter) = unionCounterexample UnionMajority
            committedThrough (replica A correctBefore) @?= 1
            committedThrough (replica A brokenBefore) @?= 2
            faults correctAfter @?= []
            assertBool
              "a physically elected leader lacks the wrongly committed configuration"
              (LeaderMissingCommitted B 2 `elem` faults brokenAfter)
            take 2 (physicalLog (replica D brokenAfter)) @?= [Entry 1 NoOp, Entry 2 NoOp],
          testCase "counterexample: stale genesis election quorum loses an already committed final configuration" $ do
            let (correctBefore, correctAfter) = staleElectionCounterexample Correct
                (brokenBefore, brokenAfter) = staleElectionCounterexample ElectionsUseGenesis
            Map.lookup 3 (knownCommitted correctBefore) @?= Just (Entry 1 (Configure (Stable new)))
            Map.lookup 3 (knownCommitted brokenBefore) @?= Just (Entry 1 (Configure (Stable new)))
            role (replica A correctBefore) @?= Follower
            role (replica B correctAfter) @?= Candidate
            faults correctAfter @?= []
            role (replica B brokenAfter) @?= Leader
            assertBool
              "old-majority B/C is insufficient once their own logs say Joint"
              (LeaderMissingCommitted B 3 `elem` faults brokenAfter),
          testCase "a current-term no-op commits the inherited joint prefix; delayed old-term appends cannot repair it" $ do
            let before = startJoint Correct
                delayed = Append A C 1 (physicalLog (replica A before)) 1
                joint = replicateInOrder A [B, C, D, E] before
                elected = electWith C [B, D, E] (crash A joint)
                inheritedPrefix = take 2 (physicalLog (replica C elected))
                -- These current-term RPCs acknowledge only the previous-term
                -- prefix, before replication of the new leader's no-op. A dual
                -- majority physically has index 2; that alone cannot commit it.
                prefixOnly =
                  deliverAndAck
                    (Append C D 2 inheritedPrefix 1)
                    (deliverAndAck (Append C B 2 inheritedPrefix 1) elected)
                finished = replicateInOrder C [B, D, E] prefixOnly
                (afterDelayed, ack) = deliverAppend delayed finished
            committedThrough (replica C elected) @?= 1
            committedThrough (replica C prefixOnly) @?= 1
            committedThrough (replica C finished) @?= 3
            physicalLog (replica C finished)
              @?= [Entry 1 NoOp, Entry 1 (Configure (Joint old new)), Entry 2 NoOp]
            ack @?= Nothing
            afterDelayed @?= finished
        ]
    ]
