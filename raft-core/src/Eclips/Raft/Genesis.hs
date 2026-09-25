-- | Checked immutable genesis for a native voter or fresh learner.
module Eclips.Raft.Genesis
  ( RaftGenesis,
    raftGenesis,
    RaftGenesisFault (..),
    CheckedRaftGenesis,
    checkRaftGenesis,
    checkRaftLearnerGenesis,
    checkedRaftLocalNode,
    checkedRaftNativeConfiguration,
    RaftNativeConfiguration,
    raftNativeVoters,
    raftNativeHasQuorum,
    raftNativeHeartbeatInterval,
    raftNativeElectionTimeoutLower,
    raftNativeElectionTimeoutUpper,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Raft.Identity
  ( RaftDurationMicros,
    RaftNodeId,
  )

-- | Unchecked startup facts.  Contact endpoints deliberately do not occur here.
data RaftGenesis
  = RaftGenesis
      RaftNodeId
      [RaftNodeId]
      RaftDurationMicros
      RaftDurationMicros
      RaftDurationMicros
  deriving stock (Eq, Show)

raftGenesis ::
  RaftNodeId ->
  [RaftNodeId] ->
  RaftDurationMicros ->
  RaftDurationMicros ->
  RaftDurationMicros ->
  RaftGenesis
raftGenesis = RaftGenesis

data RaftGenesisFault
  = RaftVotersEmpty
  | RaftVotersNotStrictlyAscending
  | RaftLocalNodeNotVoter RaftNodeId
  | RaftLearnerAlreadyInitialVoter RaftNodeId
  | RaftElectionTimeoutLowerNotAfterHeartbeat
  | RaftElectionTimeoutUpperBeforeLower
  deriving stock (Eq, Show)

-- | Domain-independent immutable initial voter and timer configuration.
--
-- Oracle/composition code may compare this projection and include it in its own
-- configuration digest, but Raft itself knows no System or Herald identity.
data RaftNativeConfiguration
  = RaftNativeConfiguration
      [RaftNodeId]
      RaftDurationMicros
      RaftDurationMicros
      RaftDurationMicros
  deriving stock (Eq, Show)

data CheckedRaftGenesis
  = CheckedRaftGenesis
      RaftNodeId
      RaftNativeConfiguration
  deriving stock (Eq, Show)

checkRaftGenesis :: RaftGenesis -> Either RaftGenesisFault CheckedRaftGenesis
checkRaftGenesis = checkGenesis False

-- | A fresh learner replays the unchanged initial configuration. Reusing an
-- initial voter's identity with an empty log is not learner initialization.
checkRaftLearnerGenesis :: RaftGenesis -> Either RaftGenesisFault CheckedRaftGenesis
checkRaftLearnerGenesis = checkGenesis True

checkGenesis :: Bool -> RaftGenesis -> Either RaftGenesisFault CheckedRaftGenesis
checkGenesis learner (RaftGenesis local voters heartbeat lower upper)
  | null voters = Left RaftVotersEmpty
  | not (strictlyAscending voters) = Left RaftVotersNotStrictlyAscending
  | not learner && local `notElem` voters = Left (RaftLocalNodeNotVoter local)
  | learner && local `elem` voters = Left (RaftLearnerAlreadyInitialVoter local)
  | lower <= heartbeat = Left RaftElectionTimeoutLowerNotAfterHeartbeat
  | upper < lower = Left RaftElectionTimeoutUpperBeforeLower
  | otherwise =
      Right
        ( CheckedRaftGenesis
            local
            (RaftNativeConfiguration voters heartbeat lower upper)
        )

strictlyAscending :: (Ord value) => [value] -> Bool
strictlyAscending values = and (zipWith (<) values (drop 1 values))

checkedRaftLocalNode :: CheckedRaftGenesis -> RaftNodeId
checkedRaftLocalNode (CheckedRaftGenesis local _) = local

checkedRaftNativeConfiguration :: CheckedRaftGenesis -> RaftNativeConfiguration
checkedRaftNativeConfiguration (CheckedRaftGenesis _ configuration) = configuration

raftNativeVoters :: RaftNativeConfiguration -> [RaftNodeId]
raftNativeVoters (RaftNativeConfiguration voters _ _ _) = voters

-- | One vote per configured voter. Unconfigured acknowledgers never count.
-- The same strict-majority predicate governs elections and log commitment.
raftNativeHasQuorum :: RaftNativeConfiguration -> Set RaftNodeId -> Bool
raftNativeHasQuorum configuration acknowledgers =
  2 * Set.size (Set.intersection voters acknowledgers) > Set.size voters
  where
    voters = Set.fromList (raftNativeVoters configuration)

raftNativeHeartbeatInterval :: RaftNativeConfiguration -> RaftDurationMicros
raftNativeHeartbeatInterval (RaftNativeConfiguration _ heartbeat _ _) = heartbeat

raftNativeElectionTimeoutLower :: RaftNativeConfiguration -> RaftDurationMicros
raftNativeElectionTimeoutLower (RaftNativeConfiguration _ _ lower _) = lower

raftNativeElectionTimeoutUpper :: RaftNativeConfiguration -> RaftDurationMicros
raftNativeElectionTimeoutUpper (RaftNativeConfiguration _ _ _ upper) = upper
