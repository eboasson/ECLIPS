{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- Real ERFT bytes connect independently initialized, privately owned kernels.
-- Scheduling retains only emitted RPCs and public observations, never RaftState.
module NativeTcpProperties (tests) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM
import Control.Exception (AsyncException, SomeException, bracket, finally, fromException, throwIO, try)
import Control.Monad (foldM, forM, forM_, unless, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as Bytes
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64, Word8)
import Eclips.Protocol.Raft.Frame
import Eclips.Protocol.Raft.Types hiding (raftLogEntryIndex)
import Eclips.Raft.Checkpoint (raftCheckpointIndex, raftCheckpointPayload)
import Eclips.Raft.Configuration
import Eclips.Raft.Effect
import Eclips.Raft.Genesis
import Eclips.Raft.Identity
import Eclips.Raft.Input
import Eclips.Raft.State
import Eclips.Raft.Transition
import Network.Socket
import Network.Socket.ByteString qualified as Socket
import System.Timeout (timeout)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.Runners (NumThreads (..))

-- Each owner keeps its own log and adapter cursor. The fixture adapter preserves
-- opaque metadata and acknowledges only the sealed contiguous exposure it saw.
data OwnerCommand
  = Apply (RaftInput ByteString) (TMVar (Either String Observation))
  | Inspect (TMVar Observation)
  | ReadReadiness RaftCatchUpFrontier (TMVar (Maybe RaftLearnerReadiness))

data Owner = Owner (TQueue OwnerCommand) ThreadId (MVar ())
type Cluster = Map RaftNodeId Owner

data Observation = Observation
  { node :: RaftNodeId,
    role :: RaftRole,
    term :: RaftTerm,
    commit :: RaftLogIndex,
    applied :: RaftLogIndex,
    lastIndex :: RaftLogIndex,
    effective :: (RaftConfigurationRef, RaftVotingConfiguration),
    election :: RaftElectionTimerGeneration,
    heartbeat :: RaftHeartbeatTimerGeneration,
    recentLeader :: RaftRecentLeaderTimerGeneration,
    frontier :: Maybe RaftCatchUpFrontier,
    entries :: [RaftLogEntry ByteString],
    exposed :: [(RaftLogIndex, RaftTerm, RaftEntry ByteString)],
    applicationImage :: ByteString,
    effects :: [RaftEffect ByteString]
  }
  deriving stock (Show)

data Dispatch = Dispatch RaftNodeId RaftNodeId RaftDispatchGeneration (RaftRpc ByteString)
  deriving stock (Show)
data Reply = Reply RaftNodeId RaftNodeId RaftDispatchGeneration (RaftRpc ByteString)
  deriving stock (Show)

tests :: TestTree
tests =
  localOption (NumThreads 1)
    $ testGroup
      "p07-erft: native configuration traffic over real ERFT"
      [ testCase "dropped joint replies cannot replace the new majority" caseDropped,
        testCase "duplicate joint and final traffic exposes each opaque entry once" caseDuplicates,
        testCase "reordered joint traffic cannot roll back a learned final configuration" caseReordered,
        testCase "removed-leader stale traffic and a new leader reach lagging registered replicas" caseRemovedLeader,
        testCase "a compacted leader installs application state over ERFT before continuing a learner suffix" caseSnapshotCatchUp
      ]

caseSnapshotCatchUp :: Assertion
caseSnapshotCatchUp = withCluster $ \cluster -> do
  _ <- campaign cluster a
  forM_ [2 .. 12] $ \position -> do
    requestId <- checked (mkRaftProposalId position)
    completed <- apply cluster a (proposeRaftApplication requestId (Bytes.singleton (fromIntegral position)))
    _ <- apply cluster a (checkpointRaftApplication completed.applied completed.applicationImage)
    pure ()
  source <- inspect cluster a
  assertEqual "the source has reclaimed its command history" 1 (length source.entries)
  targetInput <- checked (updateRaftReplicationTargets [b])
  probe <- apply cluster a targetInput
  (rejection, _) <- deliverRequest cluster (to b (requests probe))
  snapshot <- deliverReply cluster rejection
  assertEqual "lagging receiver selects InstallSnapshot" [()] [() | Dispatch _ _ _ rpc <- requests snapshot, InstallSnapshotView {} <- [raftRpcView rpc]]
  (ack, _) <- deliverRequest cluster (to b (requests snapshot))
  installed <- inspect cluster b
  assertEqual "the application installs the checkpoint bytes independently of the native log" source.applicationImage installed.applicationImage
  assertEqual "the installed native prefix is acknowledged before reply delivery" source.applied installed.applied
  assertEqual "installation does not replay the discarded application commands" [] installed.exposed
  _ <- deliverReply cluster ack
  requestId <- checked (mkRaftProposalId 13)
  next <- apply cluster a (proposeRaftApplication requestId "suffix-after-checkpoint")
  drain cluster (requests next)
  caughtUp <- inspect cluster b
  assertEqual "the learner applies the new suffix to installed application state" "suffix-after-checkpoint" caughtUp.applicationImage
  assertEqual "only the suffix needs ordinary replay" [raftLogIndex 13] [index | (index, _, Application _) <- caughtUp.exposed]

caseDropped :: Assertion
caseDropped = withCluster $ \cluster -> do
  prepareGrowth cluster [a, b, c]
  joint <- proposeJoint cluster [a, b, c]
  (replyB, _) <- deliverRequest cluster (to b (requests joint))
  (replyC, _) <- deliverRequest cluster (to c (requests joint))
  before <- inspect cluster a
  assertEqual "both configuration acknowledgements were dropped" (raftLogIndex 1) before.commit
  assertEqual "old singleton is not a joint quorum" (raftLogIndex 1) before.applied
  after <- deliverReply cluster replyB
  drain cluster (requests after)
  learned <- inspect cluster a
  assertEqual "one additional new voter completes both majorities" (raftLogIndex 2) learned.commit
  delayed <- deliverReply cluster replyC
  drain cluster (requests delayed)
  assertEqual "delayed reply preserves the same joint entry" 1 . length . configExposures =<< inspect cluster a

caseDuplicates :: Assertion
caseDuplicates = withCluster $ \cluster -> do
  prepareGrowth cluster [a, b, c]
  joint <- proposeJoint cluster [a, b, c]
  let sent = to b (requests joint)
  (firstReply, _) <- deliverRequest cluster sent
  (duplicateReply, _) <- deliverRequest cluster sent
  first <- deliverReply cluster firstReply
  duplicate <- deliverReply cluster duplicateReply
  drain cluster (requests first <> requests duplicate <> filter (not . targets b) (requests joint))
  final <- proposeFinal cluster [a, b, c]
  let finalSent = to b (requests final)
  (finalReply, _) <- deliverRequest cluster finalSent
  (duplicateFinalReply, _) <- deliverRequest cluster finalSent
  accepted <- deliverReply cluster finalReply
  repeated <- deliverReply cluster duplicateFinalReply
  drain cluster (requests accepted <> requests repeated <> filter (not . targets b) (requests final))
  heartbeatAll cluster a
  assertSameConfigurationHistory cluster [a, b, c] [a, b, c]

caseReordered :: Assertion
caseReordered = withCluster $ \cluster -> do
  prepareGrowth cluster [a, b, c]
  joint <- proposeJoint cluster [a, b, c]
  let oldRequest = to c (requests joint)
  (replyB, _) <- deliverRequest cluster (to b (requests joint))
  committedJoint <- deliverReply cluster replyB
  drainExcept cluster c (requests committedJoint)
  final <- proposeFinal cluster [a, b, c]
  (finalReplyB, _) <- deliverRequest cluster (to b (requests final))
  committedFinal <- deliverReply cluster finalReplyB
  drainExcept cluster c (requests committedFinal)
  -- C missed joint entirely; a fresh ordinary retransmission must carry it.
  heartbeatAll cluster a
  learned <- inspect cluster c
  assertEqual "lagging learner installed complete final history" (raftLogIndex 3) learned.applied
  (staleReply, _) <- deliverRequest cluster oldRequest
  staleResult <- deliverReply cluster staleReply
  drain cluster (requests staleResult)
  after <- inspect cluster c
  assertEqual "older partial history does not truncate surviving suffix" learned.entries after.entries
  assertEqual "older correlation cannot reduce committed prefix" learned.commit after.commit
  assertSameConfigurationHistory cluster [a, b, c] [a, b, c]

caseRemovedLeader :: Assertion
caseRemovedLeader = withCluster $ \cluster -> do
  prepareGrowth cluster [b, c]
  joint <- proposeJoint cluster [b, c]
  let oldRequest = to c (requests joint)
  drain cluster (requests joint)
  final <- proposeFinal cluster [b, c]
  finalBefore <- inspect cluster a
  assertEqual "self-excluded leader can finish its final entry" RaftLeader finalBefore.role
  assertEqual "self is absent from effective final voters" (StableRaftConfigurationView [b, c]) (raftVotingConfigurationView (snd finalBefore.effective))
  drain cluster (requests final)
  removed <- inspect cluster a
  assertEqual "committed exclusion relinquishes leadership" RaftFollower removed.role
  -- Final commit propagation may end with removal. Both surviving owners have
  -- the final entry in their log, and elect using that effective voter set.
  forM_ [b, c] $ \node -> do
    current <- inspect cluster node
    void (apply cluster node (fireRecentLeaderTimer current.recentLeader))
  candidate <- campaign cluster b
  drain cluster (requests candidate)
  heartbeatAll cluster b
  replacement <- inspect cluster b
  assertEqual "new leader was never an immutable genesis voter" RaftLeader replacement.role
  assertBool "ordinary election advances the term" (replacement.term > raftTerm 1)
  (staleReply, _) <- deliverRequest cluster oldRequest
  stale <- deliverReply cluster staleReply
  drain cluster (requests stale)
  stable <- inspect cluster c
  assertEqual "same-run historical source admitted without reverting configuration" (StableRaftConfigurationView [b, c]) (raftVotingConfigurationView (snd stable.effective))
  -- D starts from original genesis with no A/B/C state copied into it. B is not
  -- in that genesis, yet its bound lane delivers all native configuration bytes.
  targetUpdate <- checked (updateRaftReplicationTargets [a, c, d]) >>= apply cluster b
  drain cluster (requests targetUpdate)
  heartbeatAll cluster b
  newest <- inspect cluster b
  learner <- inspect cluster d
  assertEqual "fresh learner reconstructs every native entry" newest.entries learner.entries
  assertEqual "fresh learner applies the same opaque configuration metadata" (configExposures newest) (configExposures learner)
  let beforeTerm = learner.term
  learnerCampaign <- campaign cluster d
  assertEqual "transport admission does not enable a learner campaign" RaftFollower learnerCampaign.role
  assertEqual "learner does not advance term by timing out" beforeTerm learnerCampaign.term
  assertEqual "learner emits no vote solicitations" [] [target | Dispatch _ target _ rpc <- requests learnerCampaign, RequestVoteView {} <- [raftRpcView rpc]]

prepareGrowth :: Cluster -> [RaftNodeId] -> IO ()
prepareGrowth cluster new = do
  elected <- campaign cluster a
  assertEqual "genesis singleton elected" RaftLeader elected.role
  updated <- checked (updateRaftReplicationTargets [b, c]) >>= apply cluster a
  drain cluster (requests updated)
  heartbeatAll cluster a
  leader <- inspect cluster a
  preparing <- apply cluster a (beginRaftConfigurationChange change (fst leader.effective) (voters new))
  drain cluster (requests preparing)
  captured <- maybe (assertFailure "leader frontier unavailable") pure preparing.frontier
  forM_ [b, c] $ \node -> do
    token <- readReadiness cluster node captured >>= maybe (assertFailure "learner did not apply exact captured frontier") pure
    ready <- apply cluster a (observeRaftLearnerReadiness token)
    drain cluster (requests ready)

proposeJoint :: Cluster -> [RaftNodeId] -> IO Observation
proposeJoint cluster new = do
  before <- inspect cluster a
  result <- apply cluster a (proposeRaftConfiguration change (fst before.effective) (jointRaftConfiguration (voters [a]) (voters new)) jointMetadata)
  assertEqual "joint appended" (raftLogIndex 2) result.lastIndex
  pure result

proposeFinal :: Cluster -> [RaftNodeId] -> IO Observation
proposeFinal cluster new = do
  before <- inspect cluster a
  assertEqual "joint must commit before final proposal" (raftLogIndex 2) before.applied
  result <- apply cluster a (proposeRaftConfiguration (proposal 2) (fst before.effective) (stableRaftConfiguration (voters new)) finalMetadata)
  assertEqual "final appended" (raftLogIndex 3) result.lastIndex
  pure result

assertSameConfigurationHistory :: Cluster -> [RaftNodeId] -> [RaftNodeId] -> Assertion
assertSameConfigurationHistory cluster nodes new = forM_ nodes $ \node -> do
  observation <- inspect cluster node
  assertEqual "final committed prefix" (raftLogIndex 3) observation.applied
  assertEqual "final effective voters" (StableRaftConfigurationView new) (raftVotingConfigurationView (snd observation.effective))
  assertEqual "ordered opaque metadata exactly once" [(raftLogIndex 2, jointMetadata), (raftLogIndex 3, finalMetadata)] (configExposures observation)

configExposures :: Observation -> [(RaftLogIndex, ByteString)]
configExposures observation = [(index, metadata) | (index, _, Configuration _ metadata) <- observation.exposed]

heartbeatAll :: Cluster -> RaftNodeId -> IO ()
heartbeatAll cluster node = do
  before <- inspect cluster node
  emitted <- apply cluster node (fireHeartbeatTimer before.heartbeat)
  drain cluster (requests emitted)

campaign :: Cluster -> RaftNodeId -> IO Observation
campaign cluster node = do
  before <- inspect cluster node
  selected <- apply cluster node (selectElectionTimeout before.election (duration 10))
  apply cluster node (fireElectionTimer selected.election)

requests :: Observation -> [Dispatch]
requests observation = [Dispatch observation.node target generation rpc | SendRaftRpc target generation rpc <- observation.effects, requestRpc rpc]

requestRpc :: RaftRpc bytes -> Bool
requestRpc rpc = case raftRpcView rpc of
  RequestVoteView {} -> True
  AppendEntriesView {} -> True
  InstallSnapshotView {} -> True
  _ -> False

targets :: RaftNodeId -> Dispatch -> Bool
targets expected (Dispatch _ actual _ _) = expected == actual

to :: RaftNodeId -> [Dispatch] -> Dispatch
to target = maybe (error "expected authentic dispatch was not emitted") id . find (targets target)

drain :: Cluster -> [Dispatch] -> IO ()
drain cluster = drainWith cluster (const True)

drainExcept :: Cluster -> RaftNodeId -> [Dispatch] -> IO ()
drainExcept cluster excluded = drainWith cluster (not . targets excluded)

drainWith :: Cluster -> (Dispatch -> Bool) -> [Dispatch] -> IO ()
drainWith _ _ [] = pure ()
drainWith cluster accepted (dispatch : rest)
  | not (accepted dispatch) = drainWith cluster accepted rest
  | otherwise = do
      (reply, additional) <- deliverRequest cluster dispatch
      response <- deliverReply cluster reply
      drainWith cluster accepted (rest <> additional <> requests response)

deliverRequest :: Cluster -> Dispatch -> IO (Reply, [Dispatch])
deliverRequest cluster (Dispatch source target generation rpc) = do
  wire <- throughTcp source target rpc
  input <- checked (raftRequestDtoToCoreInput (nodeDto source) wire)
  observed <- apply cluster target input
  let responses = [reply | SendRaftRpc destination _ reply <- observed.effects, destination == source, not (requestRpc reply)]
  response <- case responses of
    [reply] -> pure reply
    _ -> assertFailure ("expected one real owner response, got " <> show observed.effects)
  pure (Reply target source generation response, requests observed)

deliverReply :: Cluster -> Reply -> IO Observation
deliverReply cluster (Reply source target generation rpc) = do
  wire <- throughTcp source target rpc
  input <- checked (raftResponseDtoToCoreInput (nodeDto source) generation wire)
  apply cluster target input

-- The registry is the fixture's explicit four replica identities, independent
-- of every owner's current native configuration. Each receiver verifies the
-- same immutable run/genesis and exact source/target before admitting an RPC.
throughTcp :: RaftNodeId -> RaftNodeId -> RaftRpc ByteString -> IO RaftRpcDto
throughTcp source target rpc = do
  assertBool "source registered" (source `elem` [a, b, c, d])
  assertBool "target registered" (target `elem` [a, b, c, d])
  dto <- checked (raftRpcDtoFromCore rpc)
  context <- checked (raftConnectionContext digest (nodeDto target) (nodeDto source))
  hello <- checked (raftHelloClaim digest (nodeDto source) (nodeDto target))
  bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
    bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
    listen listener 1
    address <- getSocketName listener
    bracket (socket AF_INET Stream defaultProtocol) close $ \sender -> do
      connect sender address
      bracket (fst <$> accept listener) close $ \receiver -> do
        Socket.sendAll sender (encodeRaftFrame (RaftHello hello) <> encodeRaftFrame (RaftRpc dto))
        receive receiver (initialRaftFrameDecoder context) []
  where
    receive receiver decoder accumulated = do
      bytes <- Socket.recv receiver 127
      unless (not (Bytes.null bytes)) (assertFailure "ERFT closed before the scheduled RPC")
      case feedRaftFrame decoder bytes of
        RaftFrameFeedResult decoded (NeedRaftFrameBytes successor) -> case accumulated <> decoded of
          [RaftHello _, RaftRpc decodedRpc] -> pure decodedRpc
          partial -> receive receiver successor partial
        other -> assertFailure ("ERFT admission failed: " <> show other)

withCluster :: (Cluster -> IO ()) -> Assertion
withCluster use = do
  result <- timeout 10_000_000 (bracket start stop use)
  case result of
    Nothing -> assertFailure "finite ERFT configuration schedule timed out"
    Just () -> pure ()
  where
    start = Map.fromList <$> forM [a, b, c, d] (\node -> (node,) <$> startOwner node)
    stop = mapM_ stopOwner . Map.elems

startOwner :: RaftNodeId -> IO Owner
startOwner node = do
  queue <- newTQueueIO
  done <- newEmptyMVar
  let genesis = raftGenesis node [a] (duration 1) (duration 10) (duration 20)
  checkedGenesis <- checked ((if node == a then checkRaftGenesis else checkRaftLearnerGenesis) genesis)
  thread <- forkIO (loop queue (initialRaft checkedGenesis) [] Bytes.empty `finally` putMVar done ())
  pure (Owner queue thread done)
  where
    loop queue state exposure applicationImage =
      atomically (readTQueue queue) >>= \case
        Inspect reply -> atomically (putTMVar reply (observeState state exposure applicationImage [])) >> loop queue state exposure applicationImage
        ReadReadiness frontier reply -> atomically (putTMVar reply (raftStateLearnerReadiness frontier state)) >> loop queue state exposure applicationImage
        Apply input reply -> do
          result <- try @SomeException (settle input state exposure applicationImage)
          case result of
            Left problem -> case (fromException problem :: Maybe AsyncException) of
              Just _ -> throwIO problem
              Nothing -> atomically (putTMVar reply (Left (show problem))) >> loop queue state exposure applicationImage
            Right (successor, entries, image, effects) -> do
              atomically (putTMVar reply (Right (observeState successor entries image effects)))
              loop queue successor entries image

    settle input state exposure applicationImage = do
      (successor, batch) <- checked (stepRaft input state)
      let effects = raftEffectBatchEffects batch
      checkpointed <- foldM installCheckpoint (successor, exposure, applicationImage, effects) [checkpoint | InstallRaftCheckpoint checkpoint <- effects]
      foldM install checkpointed [entry | ExposeCommittedEntries entries <- effects, entry <- NonEmpty.toList entries]
    installCheckpoint (state, exposure, _, effects) checkpoint = do
      let index = raftCheckpointIndex checkpoint
          image = raftCheckpointPayload checkpoint
      (successor, retained, installedImage, more) <- settle (acknowledgeRaftCheckpoint index) state (filter (\(position, _, _) -> position > index) exposure) image
      pure (successor, retained, installedImage, effects <> more)
    install (state, exposure, image, effects) entry = do
      let observed = (committedEntryIndex entry, committedEntryTerm entry, committedEntryPayload entry)
          appliedImage = case committedEntryPayload entry of
            Application bytes -> bytes
            _ -> image
      (successor, retained, installedImage, more) <- settle (acknowledgeCommittedEntries (committedEntryIndex entry)) state (exposure <> [observed]) appliedImage
      pure (successor, retained, installedImage, effects <> more)

stopOwner :: Owner -> IO ()
stopOwner (Owner _ thread done) = killThread thread >> readMVar done

observeState :: RaftState ByteString -> [(RaftLogIndex, RaftTerm, RaftEntry ByteString)] -> ByteString -> [RaftEffect ByteString] -> Observation
observeState state exposure image emitted =
  Observation
    { node = raftStateLocalNode state,
      role = raftStateRole state,
      term = raftStateTerm state,
      commit = raftStateCommitIndex state,
      applied = raftStateAppliedThrough state,
      lastIndex = raftStateLastLogIndex state,
      effective = raftStateEffectiveConfiguration state,
      election = raftStateElectionGeneration state,
      heartbeat = raftStateHeartbeatGeneration state,
      recentLeader = raftStateRecentLeaderGeneration state,
      frontier = raftStateCatchUpFrontier state,
      entries = raftStateLogEntries state,
      exposed = exposure,
      applicationImage = image,
      effects = emitted
    }

inspect :: Cluster -> RaftNodeId -> IO Observation
inspect cluster node = do
  let Owner queue _ _ = cluster Map.! node
  reply <- newEmptyTMVarIO
  atomically (writeTQueue queue (Inspect reply))
  atomically (takeTMVar reply)

apply :: Cluster -> RaftNodeId -> RaftInput ByteString -> IO Observation
apply cluster node input = do
  let Owner queue _ _ = cluster Map.! node
  reply <- newEmptyTMVarIO
  atomically (writeTQueue queue (Apply input reply))
  atomically (takeTMVar reply) >>= checked

readReadiness :: Cluster -> RaftNodeId -> RaftCatchUpFrontier -> IO (Maybe RaftLearnerReadiness)
readReadiness cluster node frontier = do
  let Owner queue _ _ = cluster Map.! node
  reply <- newEmptyTMVarIO
  atomically (writeTQueue queue (ReadReadiness frontier reply))
  atomically (takeTMVar reply)

a, b, c, d :: RaftNodeId
a = nodeId 1
b = nodeId 2
c = nodeId 3
d = nodeId 4

nodeId :: Word8 -> RaftNodeId
nodeId = either (error . show) id . mkRaftNodeId . Bytes.replicate 32
nodeDto :: RaftNodeId -> RaftNodeIdDto
nodeDto = either (error . show) id . raftNodeIdDtoFromCore
digest :: RaftGenesisDigestDto
digest = either (error . show) id (raftGenesisDigestDto (Bytes.replicate 32 99))
voters :: [RaftNodeId] -> RaftVoterSet
voters = either (error . show) id . raftVoterSet
duration :: Word64 -> RaftDurationMicros
duration = either (error . show) id . mkRaftDurationMicros
proposal :: Word64 -> RaftProposalId
proposal = either (error . show) id . mkRaftProposalId
change :: RaftProposalId
change = proposal 1
jointMetadata, finalMetadata :: ByteString
jointMetadata = Bytes.pack [0, 255, 1, 0, 2]
finalMetadata = Bytes.pack [255, 0, 254, 0, 3]
checked :: (Show problem) => Either problem value -> IO value
checked = either (assertFailure . show) pure
