{-# LANGUAGE BangPatterns #-}

module LogWorkProperties (tests, runAllocationProbe) where

import Control.Exception (evaluate)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word (Word64)
import Eclips.Raft.Configuration
import Eclips.Raft.Effect (raftEffectBatchEffects)
import Eclips.Raft.Identity
import Eclips.Raft.Input
import Eclips.Raft.State
import Eclips.Raft.Transition
import GHC.Stats (RTSStats (allocated_bytes), getRTSStats, getRTSStatsEnabled)
import GenesisProperties (checkedGenesisWithVoters, electionLower, nodeId)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.Mem (performGC)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)
import Test.Tasty.QuickCheck (chooseInt, counterexample, forAll, testProperty)
import Text.Read (readMaybe)

tests :: TestTree
tests =
  testGroup
    "admitted log evidence and bounded idle work"
    [ testProperty "application-only histories pass exhaustive audit and retain genesis configuration"
        $ forAll (chooseInt (0, 300))
        $ \count ->
          let state = settledSingleton count
           in counterexample "application-only log audit/configuration disagrees"
                $ auditRaftState state == Right ()
                  && raftStateEffectiveConfiguration state == (genesisRaftConfigurationRef, stable [nodeA])
                  && raftStateCommittedConfiguration state == raftStateEffectiveConfiguration state,
      testProperty "indexed configurations match independent full folds through both suffix rollbacks"
        $ forAll (chooseInt (0, 80))
        $ \count ->
          case indexedRollbackHistory count of Left problem -> counterexample problem False; Right () -> counterexample "complete admitted history" True,
      testCase "heartbeat allocation does not grow with retained application history" allocationBound
    ]

nodeA, nodeB, nodeC :: RaftNodeId
nodeA = nodeId 1
nodeB = nodeId 2
nodeC = nodeId 3

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

voters :: [RaftNodeId] -> RaftVoterSet
voters = checked . raftVoterSet

stable :: [RaftNodeId] -> RaftVotingConfiguration
stable = stableRaftConfiguration . voters

-- Force the batch's diagnostic decision and the runtime readiness projection,
-- in addition to the successor itself. This is the same demand used by the
-- project-local before/after heartbeat benchmark.
strictStep :: RaftInput ByteString -> RaftState ByteString -> RaftState ByteString
strictStep input state = case checked (stepRaft input state) of
  (next, batch) ->
    let !effects = length (raftEffectBatchEffects batch)
        !generation = raftHeartbeatTimerGenerationWord64 (raftStateHeartbeatGeneration next)
        !ready = raftStateServiceReady next
     in effects `seq` generation `seq` ready `seq` next

settledSingleton :: Int -> RaftState ByteString
settledSingleton count = grow 1 count prepared
  where
    initial = initialRaft (checkedGenesisWithVoters nodeA [nodeA])
    generation = raftStateElectionGeneration initial
    selected = strictStep (selectElectionTimeout generation electionLower) initial
    elected = strictStep (fireElectionTimer generation) selected
    prepared = strictStep (acknowledgeCommittedEntries (raftStateCommitIndex elected)) elected
    grow _ 0 !state = state
    grow ident remaining !state =
      let !appended = strictStep (proposeRaftApplication (checked (mkRaftProposalId ident)) ByteString.empty) state
          !applied = strictStep (acknowledgeCommittedEntries (raftStateCommitIndex appended)) appended
       in grow (ident + 1) (remaining - 1) applied

heartbeats :: Int -> RaftState ByteString -> RaftState ByteString
heartbeats 0 !state = state
heartbeats count !state = heartbeats (count - 1) (strictStep (fireHeartbeatTimer (raftStateHeartbeatGeneration state)) state)

-- An independent fold starts from the known genesis and consumes public log
-- entries. It checks both boundaries after every append/commit/repair step.
auditProjection :: RaftState ByteString -> Either String ()
auditProjection state = do
  either (Left . show) Right (auditRaftState state)
  require "effective configuration differs from full replay" (fullAt (raftStateLastLogIndex state) == raftStateEffectiveConfiguration state)
  require "committed configuration differs from full replay" (fullAt (raftStateCommitIndex state) == raftStateCommittedConfiguration state)
  where
    fullAt limit = foldl' select (genesisRaftConfigurationRef, stable [nodeA, nodeB]) (raftStateLogEntries state)
      where
        select old entry = case raftLogEntryPayload entry of
          Configuration configuration _ | raftLogEntryIndex entry <= limit -> (checked (raftConfigurationEntryRef (raftLogEntryIndex entry) (raftLogEntryTerm entry)), configuration)
          _ -> old
    require message condition = if condition then Right () else Left message

indexedRollbackHistory :: Int -> Either String ()
indexedRollbackHistory count = do
  let initial = initialRaft (checkedGenesisWithVoters nodeB [nodeA, nodeB])
      prefix = LeaderNoOp : replicate count (Application ByteString.empty)
      end = fromIntegral (length prefix)
      joint = Configuration (jointRaftConfiguration (voters [nodeA, nodeB]) (voters [nodeB, nodeC])) ByteString.empty
      final = Configuration (stable [nodeB, nodeC]) ByteString.empty
      entry index term = raftLogEntry (raftLogIndex index) (raftTerm term)
      install term previous previousTerm entries committed state = do
        rpc <- either (Left . show) Right (appendEntries (raftTerm term) nodeA (raftLogIndex previous) (raftTerm previousTerm) entries (raftLogIndex committed))
        input <- either (Left . show) Right (observeRaftRequest nodeA rpc)
        (next, _) <- either (Left . show) Right (stepRaft input state)
        auditProjection next
        pure next
  base <- install 1 0 0 [entry index 1 payload | (index, payload) <- zip [1 ..] prefix] end initial
  uncommittedJoint <- install 1 end 1 [entry (end + 1) 1 joint] end base
  restoredStable <- install 2 end 1 [entry (end + 1) 2 LeaderNoOp] (end + 1) uncommittedJoint
  nextJoint <- install 2 (end + 1) 2 [entry (end + 2) 2 joint] (end + 1) restoredStable
  committedJoint <- install 2 (end + 2) 2 [] (end + 2) nextJoint
  uncommittedFinal <- install 2 (end + 2) 2 [entry (end + 3) 2 final] (end + 2) committedJoint
  restoredJoint <- install 3 (end + 2) 2 [entry (end + 3) 3 LeaderNoOp] (end + 3) uncommittedFinal
  nextFinal <- install 3 (end + 3) 3 [entry (end + 4) 3 final] (end + 3) restoredJoint
  finished <- install 3 (end + 4) 3 [] (end + 4) nextFinal
  auditProjection finished

-- Separate processes isolate allocation counters from concurrently running Tasty
-- cases. No elapsed-time threshold is used; construction and exhaustive audits
-- happen outside the measured heartbeat batch.
allocationBound :: Assertion
allocationBound = do
  small <- allocationFor 8
  large <- allocationFor 2048
  assertBool ("retained history grew heartbeat allocation: " <> show (small, large)) (large <= 2 * small)
  where
    allocationFor count = do
      executable <- getExecutablePath
      (status, output, errors) <- readProcessWithExitCode executable ["--raft-allocation-probe", show (count :: Int), "+RTS", "-N1", "-T", "-RTS"] ""
      case (status, readMaybe output) of
        (ExitSuccess, Just allocated) -> pure (allocated :: Word64)
        _ -> assertFailure ("allocation probe failed: " <> show status <> " " <> output <> errors) >> pure 0

runAllocationProbe :: Int -> IO ()
runAllocationProbe count = do
  enabled <- getRTSStatsEnabled
  if enabled then pure () else error "allocation probe requires RTS statistics"
  state <- evaluate (settledSingleton count)
  either (error . show) pure (auditRaftState state)
  performGC
  before <- getRTSStats
  successor <- evaluate (heartbeats 2000 state)
  _ <- evaluate (raftHeartbeatTimerGenerationWord64 (raftStateHeartbeatGeneration successor))
  performGC
  after <- getRTSStats
  either (error . show) pure (auditRaftState successor)
  print (allocated_bytes after - allocated_bytes before)
