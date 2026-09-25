{-# LANGUAGE OverloadedStrings #-}

module CheckpointProperties (tests, caseCheckpointOwnerRetention) where

import Control.Exception (bracket, evaluate)
import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as Bytes
import Data.IORef (IORef, mkWeakIORef, newIORef, readIORef)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Eclips.Raft.Checkpoint
import Eclips.Raft.Configuration
import Eclips.Raft.Effect
import Eclips.Raft.Genesis
import Eclips.Raft.Identity
import Eclips.Raft.Input
import Eclips.Raft.State
import Eclips.Raft.Transition
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import GenesisProperties (checkedGenesisWithVoters, electionLower, electionUpper, heartbeat, nodeId)
import System.Mem (performGC)
import System.Mem.Weak (Weak, deRefWeak)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Positive (..), Property, conjoin, forAll, shuffle, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "native application checkpoints"
    [ testProperty "fixed application state retains one native base over growing command history" propBoundedLog,
      testProperty "local offers report the exact retained image under reordering and duplicates" propLocalOfferReports,
      testCase "covered local offers report the original payload and empty origin" caseCoveredOfferReports,
      testCase "preparation permits its applied frontier and rejects a newer unapplied offer" casePreparationOfferReports,
      testCase "ordinary singleton owner demand releases superseded checkpoint payloads before a full audit" caseCheckpointOwnerRetention,
      testProperty "reordered and duplicate installed checkpoints retain the newest application prefix" propReorderedSnapshots,
      testCase "a lagging learner installs actual state before its snapshot acknowledgement" caseLaggingLearner,
      testCase "pending installation cannot be replaced or acknowledged out of order" casePendingInstallation,
      testCase "matching suffix survives and stale Append cannot resurrect compacted history" caseRetainedSuffix,
      testCase "snapshot metadata retains joint quorum and election safety" caseJointElection,
      testCase "a conflicting uncommitted suffix is replaced by the installed checkpoint" caseConflictingSuffix,
      testCase "a final configuration after a compacted joint base cannot roll back" caseCompactedFinal,
      testCase "local capture cannot retire an unapplied prefix" caseUnappliedCapture
    ]

nodeA, nodeB, nodeC :: RaftNodeId
nodeA = nodeId 1
nodeB = nodeId 2
nodeC = nodeId 3

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

step :: RaftInput ByteString -> RaftState ByteString -> (RaftState ByteString, [RaftEffect ByteString])
step input state = let (next, batch) = checked (stepRaft input state) in (next, raftEffectBatchEffects batch)

request :: RaftNodeId -> RaftRpc ByteString -> RaftState ByteString -> (RaftState ByteString, [RaftEffect ByteString])
request source rpc = step (checked (observeRaftRequest source rpc))

response :: RaftNodeId -> RaftDispatchGeneration -> RaftRpc ByteString -> RaftState ByteString -> (RaftState ByteString, [RaftEffect ByteString])
response source generation rpc = step (checked (observeRaftResponse source generation rpc))

sendTo :: RaftNodeId -> [RaftEffect ByteString] -> (RaftDispatchGeneration, RaftRpc ByteString)
sendTo target effects = case [(generation, rpc) | SendRaftRpc peer generation rpc <- effects, peer == target] of
  [packet] -> packet
  other -> error ("expected exactly one native packet: " <> show other)

campaign :: RaftState ByteString -> (RaftState ByteString, [RaftEffect ByteString])
campaign state =
  let expired = fst (step (fireRecentLeaderTimer (raftStateRecentLeaderGeneration state)) state)
      generation = raftStateElectionGeneration expired
      selected = fst (step (selectElectionTimeout generation electionLower) expired)
   in step (fireElectionTimer generation) selected

initial :: RaftNodeId -> [RaftNodeId] -> RaftState ByteString
initial node = initialRaft . checkedGenesisWithVoters node

singleton :: RaftState ByteString
singleton = fst (step (acknowledgeCommittedEntries (raftLogIndex 1)) (fst (campaign (initial nodeA [nodeA]))))

appendCheckpoint :: RaftState ByteString -> Word8 -> RaftState ByteString
appendCheckpoint state value =
  let index = raftLogIndex (raftLogIndexWord64 (raftStateLastLogIndex state) + 1)
      proposal = checked (mkRaftProposalId (raftLogIndexWord64 index))
      payload = Bytes.singleton value
      appended = fst (step (proposeRaftApplication proposal payload) state)
      applied = fst (step (acknowledgeCommittedEntries index) appended)
   in fst (step (checkpointRaftApplication index payload) applied)

propBoundedLog :: Positive Int -> Word8 -> Property
propBoundedLog (Positive sample) value =
  let count = 1 + sample `mod` 1000
      states = scanl appendCheckpoint singleton (replicate count value)
      retained = drop 1 states
   in conjoin
        [ conjoin [length (raftStateLogEntries state) === 1, raftStateServiceReady state === True, fmap raftCheckpointPayload (raftStateCheckpoint state) === Just (Bytes.singleton value), auditRaftState state === Right ()]
        | state <- retained
        ]

-- The reference retains only the largest offered position and the first payload
-- offered at that position. It does not inspect native successor state to decide
-- which image a reordered offer should acknowledge.
propLocalOfferReports :: Positive Word8 -> Property
propLocalOfferReports (Positive sample) =
  let count = 1 + fromIntegral (sample `mod` 20)
      configuration = stableRaftConfiguration (voters [nodeA, nodeB])
      entries = [raftLogEntry (raftLogIndex n) (raftTerm 1) (Application "command") | n <- [1 .. count]]
      committed = fst (request nodeA (checked (appendEntries (raftTerm 1) nodeA (raftLogIndex 0) (raftTerm 0) entries (raftLogIndex count))) (initial nodeB [nodeA, nodeB]))
      applied = foldl' (\state n -> fst (step (acknowledgeCommittedEntries (raftLogIndex n)) state)) committed [1 .. count]
   in forAll (shuffle ([0 .. count] <> [0, count, count `div` 2])) $ \offers ->
        let observe (state, retained, priorChecks) (ordinal, offered) =
              let payload = Bytes.pack [fromIntegral offered, ordinal]
                  previous = maybe 0 (raftLogIndexWord64 . raftCheckpointIndex) retained
                  expected = if offered > previous then Just (checked (raftCheckpoint (raftLogIndex offered) (raftTerm 1) genesisRaftConfigurationRef configuration payload)) else retained
                  (successor, effects) = step (checkpointRaftApplication (raftLogIndex offered) payload) state
                  reports = [(position, image) | ReportRaftCheckpointOffer position image <- effects]
                  check = conjoin [reports === [(raftLogIndex offered, expected)], raftStateCheckpoint successor === expected, auditRaftState successor === Right ()]
               in (successor, expected, check : priorChecks)
            (_, _, checks) = foldl' observe (applied, Nothing, []) (zip [0 ..] offers)
         in conjoin checks

caseCoveredOfferReports :: Assertion
caseCoveredOfferReports = do
  let (withoutImage, emptyReport) = step (checkpointRaftApplication (raftLogIndex 0) "ignored origin") singleton
      captured = appendCheckpoint singleton 7
      retained = raftStateCheckpoint captured
  withoutImage @?= singleton
  emptyReport @?= [ReportRaftCheckpointOffer (raftLogIndex 0) Nothing]
  mapM_
    ( \position -> do
        let (same, effects) = step (checkpointRaftApplication (raftLogIndex position) "different offered bytes") captured
        same @?= captured
        effects @?= [ReportRaftCheckpointOffer (raftLogIndex position) retained]
    )
    [0, 1, 2]
  fmap raftCheckpointPayload retained @?= Just (Bytes.singleton 7)

casePreparationOfferReports :: Assertion
casePreparationOfferReports = do
  let proposal = checked (mkRaftProposalId 1)
      targeted = fst (step (checked (updateRaftReplicationTargets [nodeB])) singleton)
      preparing = fst (step (beginRaftConfigurationChange proposal genesisRaftConfigurationRef (voters [nodeA, nodeB])) targeted)
      frontier = fromJust (raftStateCatchUpFrontier preparing)
      (captured, effects) = step (checkpointRaftApplication (raftLogIndex 1) "frontier image") preparing
      retained = raftStateCheckpoint captured
      (same, duplicateEffects) = step (checkpointRaftApplication (raftLogIndex 1) "ignored replacement") captured
  raftCatchUpFrontierIndex frontier @?= raftLogIndex 1
  effects @?= [ReportRaftCheckpointOffer (raftLogIndex 1) retained]
  fmap raftCheckpointPayload retained @?= Just "frontier image"
  same @?= captured
  duplicateEffects @?= [ReportRaftCheckpointOffer (raftLogIndex 1) retained]
  stepRaft (checkpointRaftApplication (raftLogIndex 2) "unapplied image") captured @?= Left (RaftCheckpointBeforeApplication (raftLogIndex 2) (raftLogIndex 1))
  let cancelled = fst (step (cancelRaftConfigurationChange proposal) captured)
      advanced = appendCheckpoint cancelled 9
  fmap raftCheckpointIndex (raftStateCheckpoint advanced) @?= Just (raftLogIndex 2)
  fmap raftCheckpointPayload (raftStateCheckpoint advanced) @?= Just (Bytes.singleton 9)
  auditRaftState advanced @?= Right ()

-- Each immutable cell is the opaque application image itself. Weak IORef
-- identity observes the backing cell even if GHC changes wrapper allocation.
-- This owner-shaped loop drops every predecessor and consumed effect batch;
-- unlike a full state Show/audit, its demands leave unused quorum fields cold.
{-# NOINLINE checkpointOwnerStep #-}
checkpointOwnerStep :: RaftInput (IORef ()) -> RaftState (IORef ()) -> IO (RaftState (IORef ()))
checkpointOwnerStep input state = case checked (stepRaft input state) of
  (successor, batch) -> do
    _ <- evaluate (length (raftEffectBatchEffects batch))
    evaluate successor

{-# NOINLINE checkpointOwnerHistory #-}
checkpointOwnerHistory :: Int -> IO (RaftState (IORef ()), [Weak (IORef ())])
checkpointOwnerHistory count = do
  let empty = initialRaft (checkedGenesisWithVoters nodeA [nodeA])
      generation = raftStateElectionGeneration empty
  selected <- checkpointOwnerStep (selectElectionTimeout generation electionLower) empty
  leader <- checkpointOwnerStep (fireElectionTimer generation) selected
  applied <- checkpointOwnerStep (acknowledgeCommittedEntries (raftLogIndex 1)) leader
  loop count applied []
  where
    loop 0 state witnesses = pure (state, reverse witnesses)
    loop remaining state witnesses = do
      payload <- newIORef ()
      witness <- mkWeakIORef payload (pure ())
      let index = raftLogIndex (raftLogIndexWord64 (raftStateLastLogIndex state) + 1)
          proposal = checked (mkRaftProposalId (raftLogIndexWord64 index))
      appended <- checkpointOwnerStep (proposeRaftApplication proposal payload) state
      applied <- checkpointOwnerStep (acknowledgeCommittedEntries index) appended
      captured <- checkpointOwnerStep (checkpointRaftApplication index payload) applied
      successor <- checkpointOwnerStep (fireHeartbeatTimer (raftStateHeartbeatGeneration captured)) captured
      loop (remaining - 1) successor (witness : witnesses)

caseCheckpointOwnerRetention :: Assertion
caseCheckpointOwnerRetention = do
  (state, witnesses) <- checkpointOwnerHistory 256
  bracket (newStablePtr state) freeStablePtr $ \root -> do
    -- Do not inspect the final state before GC: forcing cold maps here could
    -- erase the predecessor chain whose absence this regression must prove.
    performGC
    alive <- forM witnesses (fmap (maybe False (const True)) . deRefWeak)
    assertEqual "only the current application image remains reachable" (replicate 255 False <> [True]) alive
    retained <- deRefStablePtr root
    case raftStateCheckpoint retained of
      Nothing -> assertFailure "the rooted owner lost its current checkpoint"
      Just snapshot -> readIORef (raftCheckpointPayload snapshot) >>= assertEqual "current image remains usable" ()
    raftStateAppliedThrough retained @?= raftLogIndex 257
    raftStateServiceReady retained @?= True
    length (raftStateLogEntries retained) @?= 1
    auditRaftState retained @?= Right ()

checkpoint :: Integer -> RaftVotingConfiguration -> RaftConfigurationRef -> RaftCheckpoint ByteString
checkpoint ordinal configuration reference = checked (raftCheckpoint (raftLogIndex (fromInteger ordinal)) (raftTerm 1) reference configuration (Bytes.singleton (fromInteger ordinal)))

voters :: [RaftNodeId] -> RaftVoterSet
voters = checked . raftVoterSet

propReorderedSnapshots :: Property
propReorderedSnapshots = forAll (shuffle [1, 2, 3, 4, 5, 5, 3, 1]) $ \ordinals ->
  let configuration = stableRaftConfiguration (voters [nodeA, nodeB, nodeC])
      install state ordinal =
        let snapshot = checkpoint ordinal configuration genesisRaftConfigurationRef
            (pending, effects) = request nodeA (checked (installSnapshot (raftTerm 1) nodeA snapshot)) state
         in case [accepted | InstallRaftCheckpoint accepted <- effects] of
              [] -> pending
              [accepted] -> fst (step (acknowledgeRaftCheckpoint (raftCheckpointIndex accepted)) pending)
              _ -> error "one request exposed multiple checkpoints"
      final = foldl' install (initial nodeB [nodeA, nodeB, nodeC]) ordinals
   in conjoin [raftStateAppliedThrough final === raftLogIndex 5, fmap raftCheckpointPayload (raftStateCheckpoint final) === Just (Bytes.singleton 5), length (raftStateLogEntries final) === 1, auditRaftState final === Right ()]

caseLaggingLearner :: Assertion
caseLaggingLearner = do
  let leader = foldl' appendCheckpoint singleton [1 .. 40]
      snapshot = fromJust (raftStateCheckpoint leader)
      (targeted, probeEffects) = step (checked (updateRaftReplicationTargets [nodeB])) leader
      (probeGeneration, probe) = sendTo nodeB probeEffects
      learner = initialRaft (checked (checkRaftLearnerGenesis (raftGenesis nodeB [nodeA] heartbeat electionLower electionUpper)))
      (rejected, rejectionEffects) = request nodeA probe learner
      (_, rejection) = sendTo nodeA rejectionEffects
      (sending, snapshotEffects) = response nodeB probeGeneration rejection targeted
      (snapshotGeneration, snapshotRpc) = sendTo nodeB snapshotEffects
      (installing, installedEffects) = request nodeA snapshotRpc rejected
      appInstallations = [value | InstallRaftCheckpoint value <- installedEffects]
  appInstallations @?= [snapshot]
  raftStateAppliedThrough installing @?= raftLogIndex 0
  raftStateInstallingCheckpoint installing @?= Just (raftCheckpointIndex snapshot)
  raftStateServiceReady installing @?= False
  let applicationValue = case appInstallations of
        [value] -> raftCheckpointPayload value
        _ -> error "expected exactly one application installation"
      (installed, _) = step (acknowledgeRaftCheckpoint (raftCheckpointIndex snapshot)) installing
      (_, acknowledgement) = sendTo nodeA installedEffects
      (confirmed, _) = response nodeB snapshotGeneration acknowledgement sending
  applicationValue @?= Bytes.singleton 40
  raftStateAppliedThrough installed @?= raftCheckpointIndex snapshot
  raftStateMatchIndex nodeB confirmed @?= Just (raftCheckpointIndex snapshot)
  raftStateInstallingCheckpoint installed @?= Nothing
  length (raftStateLogEntries installed) @?= 1
  auditRaftState installed @?= Right ()

casePendingInstallation :: Assertion
casePendingInstallation = do
  let configuration = stableRaftConfiguration (voters [nodeA, nodeB])
      make n = checked (installSnapshot (raftTerm 1) nodeA (checkpoint n configuration genesisRaftConfigurationRef))
      (pending, _) = request nodeA (make 3) (initial nodeB [nodeA, nodeB])
      (stillPending, refused) = request nodeA (make 5) pending
  [value | InstallRaftCheckpoint value <- refused] @?= []
  [installed | SendRaftRpc _ _ rpc <- refused, InstallSnapshotResponseView _ _ _ installed <- [raftRpcView rpc]] @?= [False]
  raftStateInstallingCheckpoint stillPending @?= Just (raftLogIndex 3)
  stepRaft (acknowledgeRaftCheckpoint (raftLogIndex 5)) stillPending @?= Left (RaftUnexpectedCheckpointAcknowledgement (raftLogIndex 5))
  let first = fst (step (acknowledgeRaftCheckpoint (raftLogIndex 3)) stillPending)
      (next, effects) = request nodeA (make 5) first
  map raftCheckpointIndex [value | InstallRaftCheckpoint value <- effects] @?= [raftLogIndex 5]
  raftStateAppliedThrough next @?= raftLogIndex 3

caseRetainedSuffix :: Assertion
caseRetainedSuffix = do
  let configuration = stableRaftConfiguration (voters [nodeA, nodeB])
      entries = [raftLogEntry (raftLogIndex n) (raftTerm 1) (Application (Bytes.singleton (fromIntegral n))) | n <- [1 .. 5]]
      append = checked (appendEntries (raftTerm 1) nodeA (raftLogIndex 0) (raftTerm 0) entries (raftLogIndex 2))
      received = fst (request nodeA append (initial nodeB [nodeA, nodeB]))
      applied = foldl' (\state n -> fst (step (acknowledgeCommittedEntries (raftLogIndex n)) state)) received [1, 2]
      (pending, _) = request nodeA (checked (installSnapshot (raftTerm 1) nodeA (checkpoint 3 configuration genesisRaftConfigurationRef))) applied
      installed = fst (step (acknowledgeRaftCheckpoint (raftLogIndex 3)) pending)
      (afterStale, staleEffects) = request nodeA append installed
      suffix = checked (appendEntries (raftTerm 1) nodeA (raftLogIndex 3) (raftTerm 1) (drop 3 entries) (raftLogIndex 5))
      (committed, effects) = request nodeA suffix afterStale
  map raftLogEntryIndex (raftStateLogEntries installed) @?= map raftLogIndex [3, 4, 5]
  map raftLogEntryIndex (raftStateLogEntries afterStale) @?= map raftLogIndex [3, 4, 5]
  [() | ExposeCommittedEntries _ <- staleEffects] @?= []
  [committedEntryIndex entry | ExposeCommittedEntries exposed <- effects, entry <- foldr (:) [] exposed] @?= map raftLogIndex [4, 5]
  raftStateCommitIndex committed @?= raftLogIndex 5

caseJointElection :: Assertion
caseJointElection = do
  let joint = jointRaftConfiguration (voters [nodeA, nodeB, nodeC]) (voters [nodeA, nodeB])
      reference = checked (raftConfigurationEntryRef (raftLogIndex 2) (raftTerm 1))
      snapshot = checkpoint 4 joint reference
      pending = fst (request nodeA (checked (installSnapshot (raftTerm 1) nodeA snapshot)) (initial nodeB [nodeA, nodeB, nodeC]))
      applied = fst (step (acknowledgeRaftCheckpoint (raftLogIndex 4)) pending)
      (candidate, elections) = campaign applied
      (generationA, _) = sendTo nodeA elections
      (generationC, _) = sendTo nodeC elections
      term = raftStateTerm candidate
      (oldOnly, _) = response nodeC generationC (checked (requestVoteResponse term nodeC True)) candidate
      (leader, _) = response nodeA generationA (checked (requestVoteResponse term nodeA True)) oldOnly
  raftStateCommittedConfiguration applied @?= (reference, joint)
  raftStateRole oldOnly @?= RaftCandidate
  raftStateRole leader @?= RaftLeader
  raftStateLastLogIndex leader @?= raftLogIndex 5
  auditRaftState leader @?= Right ()

caseConflictingSuffix :: Assertion
caseConflictingSuffix = do
  let configuration = stableRaftConfiguration (voters [nodeA, nodeB])
      entries = [raftLogEntry (raftLogIndex n) (raftTerm 1) (Application (Bytes.singleton (fromIntegral n))) | n <- [1 .. 5]]
      append = checked (appendEntries (raftTerm 1) nodeA (raftLogIndex 0) (raftTerm 0) entries (raftLogIndex 2))
      received = fst (request nodeA append (initial nodeB [nodeA, nodeB]))
      applied = foldl' (\state n -> fst (step (acknowledgeCommittedEntries (raftLogIndex n)) state)) received [1, 2]
      snapshot = checked (raftCheckpoint (raftLogIndex 3) (raftTerm 2) genesisRaftConfigurationRef configuration "replacement-state")
      (pending, effects) = request nodeA (checked (installSnapshot (raftTerm 2) nodeA snapshot)) applied
      installed = fst (step (acknowledgeRaftCheckpoint (raftLogIndex 3)) pending)
      suffix = raftLogEntry (raftLogIndex 4) (raftTerm 2) (Application "replacement-suffix")
      (continued, appliedEffects) = request nodeA (checked (appendEntries (raftTerm 2) nodeA (raftLogIndex 3) (raftTerm 2) [suffix] (raftLogIndex 4))) installed
  [raftCheckpointPayload value | InstallRaftCheckpoint value <- effects] @?= ["replacement-state"]
  map raftLogEntryIndex (raftStateLogEntries installed) @?= [raftLogIndex 3]
  [committedEntryPayload entry | ExposeCommittedEntries exposed <- appliedEffects, entry <- foldr (:) [] exposed] @?= [Application "replacement-suffix"]
  raftStateCommitIndex continued @?= raftLogIndex 4
  auditRaftState continued @?= Right ()

caseCompactedFinal :: Assertion
caseCompactedFinal = do
  let joint = jointRaftConfiguration (voters [nodeA, nodeB, nodeC]) (voters [nodeA, nodeB])
      reference = checked (raftConfigurationEntryRef (raftLogIndex 2) (raftTerm 1))
      snapshot = checkpoint 4 joint reference
      rpc = checked (installSnapshot (raftTerm 1) nodeA snapshot)
      pending = fst (request nodeA rpc (initial nodeB [nodeA, nodeB, nodeC]))
      installed = fst (step (acknowledgeRaftCheckpoint (raftLogIndex 4)) pending)
      final = stableRaftConfiguration (voters [nodeA, nodeB])
      entry = raftLogEntry (raftLogIndex 5) (raftTerm 1) (Configuration final "final-configuration")
      committed = fst (request nodeA (checked (appendEntries (raftTerm 1) nodeA (raftLogIndex 4) (raftTerm 1) [entry] (raftLogIndex 5))) installed)
      applied = fst (step (acknowledgeCommittedEntries (raftLogIndex 5)) committed)
      compacted = fst (step (checkpointRaftApplication (raftLogIndex 5) "final-state") applied)
      (afterStale, staleEffects) = request nodeA rpc compacted
      (candidate, elections) = campaign afterStale
      (generation, _) = sendTo nodeA elections
      (leader, _) = response nodeA generation (checked (requestVoteResponse (raftStateTerm candidate) nodeA True)) candidate
      finalReference = checked (raftConfigurationEntryRef (raftLogIndex 5) (raftTerm 1))
  [value | InstallRaftCheckpoint value <- staleEffects] @?= []
  raftStateCommittedConfiguration afterStale @?= (finalReference, final)
  fmap raftCheckpointConfiguration (raftStateCheckpoint afterStale) @?= Just final
  [target | SendRaftRpc target _ _ <- elections] @?= [nodeA]
  raftStateRole leader @?= RaftLeader
  raftStateLastLogIndex leader @?= raftLogIndex 6
  auditRaftState leader @?= Right ()

caseUnappliedCapture :: Assertion
caseUnappliedCapture = do
  let leader = fst (campaign (initial nodeA [nodeA]))
  stepRaft (checkpointRaftApplication (raftLogIndex 1) "not-applied") leader @?= Left (RaftCheckpointBeforeApplication (raftLogIndex 1) (raftLogIndex 0))
