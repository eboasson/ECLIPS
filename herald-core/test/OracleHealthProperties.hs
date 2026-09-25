{-# LANGUAGE OverloadedRecordDot #-}

module OracleHealthProperties (tests) where

import Data.ByteString qualified as BS
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Herald.Application.Recovery (applicationRecoveryGraceMicroseconds)
import Eclips.Herald.EffectBatch (HeraldEffect (RunOracleHealthRound), effectBatchMembers)
import Eclips.Herald.FailureDetection.State qualified as Failure
import Eclips.Herald.Initialization (initialHeraldWithTiming, initialHeraldWithTimingAndIsolation)
import Eclips.Herald.Isolation (checkIsolationConfiguration)
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.OracleHealth
import Eclips.Herald.OracleHealth.State qualified as Health
import Eclips.Herald.PeerLiveness.State qualified as Peer
import Eclips.Herald.Startup.State
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Timer (TimerOutcome (TimerFired), timerSpecAbsoluteDeadline)
import Eclips.Oracle.Transition (initialOracle)
import Eclips.Oracle.Voter (oracleReplicaRegistrations, oracleVoterConfiguration)
import Eclips.Public.Types.Timing
import Eclips.Raft.Configuration
import Eclips.Raft.Identity
import GenesisFixtures qualified as F
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

nodes :: [RaftNodeId]
nodes = [checked (mkRaftNodeId (BS.replicate 32 n)) | n <- [1 .. 5]]

old, joint :: RaftVotingConfiguration
old = stableRaftConfiguration (checked (raftVoterSet (take 3 nodes)))
joint = jointRaftConfiguration (checked (raftVoterSet (take 3 nodes))) (checked (raftVoterSet (drop 2 nodes)))

jointRef :: RaftConfigurationRef
jointRef = checked (raftConfigurationEntryRef (raftLogIndex 5) (raftTerm 1))

initial :: Health.State
initial = Health.initialState 5000000 genesisRaftConfigurationRef old

observations :: Word64 -> Word64 -> RaftConfigurationRef -> RaftVotingConfiguration -> [RaftNodeId] -> [OracleHealthObservation]
observations term generation reference configuration selected = [oracleHealthObservation node (raftTerm term) generation reference configuration | node <- selected]

observe :: Word64 -> RaftConfigurationRef -> RaftVotingConfiguration -> [OracleHealthObservation] -> Health.State -> (Health.State, Bool)
observe roundNumber reference configuration replies state = case Health.observeRound roundNumber reference configuration (Set.fromList nodes) replies state of
  Nothing -> error "expected current health round"
  Just value -> value

tests :: TestTree
tests =
  testGroup
    "native Oracle health quorum"
    [ testCase "explicit isolation grace and drain override derived defaults" caseIsolationOverride,
      testCase "checked timing reaches every initial owner and emitted health round" caseTimingInitialization,
      QC.testProperty "derived health cadence survives successive fresh rounds" propDerivedCadence,
      QC.testProperty "consistent round observations are invariant under order and exact duplicates" propObservationOrder,
      QC.testProperty "every stable and joint subset follows independent strict majority counts" propQuorumSubsets,
      testCase "default cadence is one second and short policy scales with grace" $ do
        assertEqual "ordinary cadence" 1000000 (Health.roundCadence initial)
        assertEqual "short cadence" 10000 (Health.roundCadence (Health.initialState 30000 genesisRaftConfigurationRef old)),
      testCase "fresh stable strict majority includes a genuinely queried local owner" $ do
        let (_, healthy) = observe 1 genesisRaftConfigurationRef old (observations 1 0 genesisRaftConfigurationRef old (take 2 nodes)) initial
        assertBool "fresh two of three" healthy,
      testCase "joint requires both majority predicates at one exact native coordinate" $ do
        let (_, oldOnly) = observe 1 genesisRaftConfigurationRef old (observations 1 1 jointRef joint (take 3 nodes)) initial
            selected = [node | (index, node) <- zip [1 :: Int ..] nodes, index `elem` [1, 3, 4]]
            (_, both) = observe 1 genesisRaftConfigurationRef old (observations 1 1 jointRef joint selected) initial
        assertBool "old majority alone cannot confirm joint" (not oldOnly)
        assertBool "both exact joint majorities" both,
      testCase "fresh evidence expires each round and stale round cannot advance" $ do
        let (first, healthy) = observe 1 genesisRaftConfigurationRef old (observations 1 0 genesisRaftConfigurationRef old (take 2 nodes)) initial
            (_, expired) = observe 2 genesisRaftConfigurationRef old [] first
        assertBool "first confirmed" healthy
        assertBool "empty later round is unavailable" (not expired)
        assertEqual "old completion ignored" Nothing (Health.observeRound 1 genesisRaftConfigurationRef old (Set.fromList nodes) [] first),
      testCase "delayed same-term stable cannot relax a learned uncommitted joint" $ do
        let (learned, _) = observe 1 genesisRaftConfigurationRef old (observations 1 1 jointRef joint (take 1 nodes)) initial
            (_, healthy) = observe 2 genesisRaftConfigurationRef old (observations 1 0 genesisRaftConfigurationRef old (drop 1 (take 3 nodes))) learned
        assertBool "cached stable denied" (not healthy),
      testCase "higher-term truncation can restore stable at the committed floor" $ do
        let (learned, _) = observe 1 genesisRaftConfigurationRef old (observations 1 1 jointRef joint (take 1 nodes)) initial
            (_, healthy) = observe 2 genesisRaftConfigurationRef old (observations 2 2 genesisRaftConfigurationRef old (take 2 nodes)) learned
        assertBool "fresh new-term restored majority" healthy,
      testCase "higher term cannot roll back a committed joint floor" $ do
        let (_, healthy) = observe 1 jointRef joint (observations 2 2 genesisRaftConfigurationRef old (take 3 nodes)) initial
        assertBool "committed floor retained" (not healthy),
      testCase "local native generation rollback cannot count as fresh" $ do
        let (first, _) = observe 1 genesisRaftConfigurationRef old (observations 1 7 genesisRaftConfigurationRef old (take 2 nodes)) initial
            (_, healthy) = observe 2 genesisRaftConfigurationRef old (observations 2 6 genesisRaftConfigurationRef old (take 2 nodes)) first
        assertBool "obsolete local snapshots denied" (not healthy)
    ]

selectedNodes :: [Bool] -> [RaftNodeId]
selectedNodes mask = [node | (node, present) <- zip nodes (mask <> repeat False), present]

propObservationOrder :: Bool -> Bool -> [Bool] -> QC.Property
propObservationOrder useJoint duplicate mask =
  QC.forAll (QC.shuffle observed) $ \shuffled ->
    Health.observeRound 1 genesisRaftConfigurationRef old known (if duplicate then shuffled <> shuffled else shuffled) initial
      QC.=== Health.observeRound 1 genesisRaftConfigurationRef old known observed initial
  where
    known = Set.fromList nodes
    observed = observations 1 1 (if useJoint then jointRef else genesisRaftConfigurationRef) (if useJoint then joint else old) (selectedNodes mask)

propQuorumSubsets :: Bool -> [Bool] -> QC.Property
propQuorumSubsets useJoint mask =
  QC.counterexample (show (useJoint, selected)) (healthy QC.=== expected)
  where
    selected = selectedNodes mask
    configuration = if useJoint then joint else old
    reference = if useJoint then jointRef else genesisRaftConfigurationRef
    (_, healthy) = observe 1 genesisRaftConfigurationRef old (observations 1 1 reference configuration selected) initial
    majority members = 2 * length (filter (`elem` members) selected) > length members
    expected = majority (take 3 nodes) && (not useJoint || majority (drop 2 nodes))

propDerivedCadence :: QC.Positive Int -> Bool
propDerivedCadence (QC.Positive seed) =
  let target = checked (takeoverTarget (500_000 * (1 + fromIntegral (seed `mod` 100))))
      cadence = timingHealthRoundMicroseconds (deriveTimingPolicy target)
      first = Health.initialStateWithCadence cadence genesisRaftConfigurationRef old
      (second, healthy) = observe 1 genesisRaftConfigurationRef old (observations 1 0 genesisRaftConfigurationRef old (take 2 nodes)) first
      (third, unavailable) = observe 2 genesisRaftConfigurationRef old [] second
   in healthy && not unavailable && all ((== cadence) . Health.roundCadence) [first, second, third]

caseTimingInitialization :: Assertion
caseTimingInitialization = do
  let target = checked (takeoverTarget 8_000_000)
      oracle = checked (initialOracle F.fixtureCheckedOracleGenesis)
      voters = Just (oracleVoterConfiguration oracle, oracleReplicaRegistrations oracle)
      (state, effects) = checked (initialHeraldWithTiming target voters (monotonicInstant 100) F.fixtureStep14CheckedGenesis (F.fixtureCheckedInitialBootstrapsFor F.fixtureStep14CheckedGenesis) F.fixtureOracleContacts F.fixtureGeneratorSeedH1)
  startupTakeoverTarget state @?= target
  applicationRecoveryGraceMicroseconds (startupApplicationRecoveryConfiguration state) @?= 3_200_000
  (Peer.stateWitness (startupPeerLivenessState state)).graceMicroseconds @?= 3_200_000
  (Failure.stateWitness (startupFailureDetectionState state)).witnessDirectProbeDurationMicroseconds @?= 800_000
  Isolation.isolationWitnessCurrentDeadline (Isolation.stateWitness (startupIsolationState state)) @?= Just (monotonicInstant 4_800_100)
  [cadence | RunOracleHealthRound _ cadence _ _ <- effectBatchMembers effects] @?= [400_000]

caseIsolationOverride :: Assertion
caseIsolationOverride = do
  let target = checked (takeoverTarget 8_000_000)
      isolation = checked (checkIsolationConfiguration 6_000_000 750_000)
      oracle = checked (initialOracle F.fixtureCheckedOracleGenesis)
      voters = Just (oracleVoterConfiguration oracle, oracleReplicaRegistrations oracle)
      (defaultState, _) = checked (initialHeraldWithTiming target voters (monotonicInstant 100) F.fixtureStep14CheckedGenesis (F.fixtureCheckedInitialBootstrapsFor F.fixtureStep14CheckedGenesis) F.fixtureOracleContacts F.fixtureGeneratorSeedH1)
      hosts = Isolation.isolationWitnessVoterHosts (Isolation.stateWitness (startupIsolationState defaultState))
      (state, _) = checked (initialHeraldWithTimingAndIsolation (Just (hosts, isolation)) target voters (monotonicInstant 100) F.fixtureStep14CheckedGenesis (F.fixtureCheckedInitialBootstrapsFor F.fixtureStep14CheckedGenesis) F.fixtureOracleContacts F.fixtureGeneratorSeedH1)
      owner = startupIsolationState state
      witness = Isolation.stateWitness owner
  Isolation.isolationWitnessCurrentDeadline witness @?= Just (monotonicInstant 6_000_100)
  attempt <- maybe (assertFailure "missing initial grace timer") pure (Isolation.isolationWitnessCurrentTimer witness)
  prepared <- case snd (Isolation.observeGraceTimer attempt (TimerFired (monotonicInstant 6_000_100)) owner) of
    Isolation.IsolationGraceTimerSelfFenceReady value -> pure value
    _ -> assertFailure "explicit grace did not expire"
  case snd (Isolation.commitSelfFence Set.empty Isolation.IsolationDrainRequired prepared) of
    Isolation.IsolationReadOnlyDrainStarted _ _ specification _ _ -> timerSpecAbsoluteDeadline specification @?= monotonicInstant 6_750_100
    _ -> assertFailure "explicit read-only drain did not start"
