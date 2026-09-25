-- | Opaque Raft state and narrow read-only projections.
--
-- A runtime owner passes the complete state only to 'Eclips.Raft.Transition.stepRaft';
-- these projections exist for pure composition checks and property tests, not for
-- sharing state with dispatch or socket workers.
module Eclips.Raft.State
  ( RaftState,
    RaftRole (..),
    raftStateLocalNode,
    raftStateVoters,
    raftStateTerm,
    raftStateVotedFor,
    raftStateRole,
    raftStateLeaderHint,
    raftStateCommitIndex,
    raftStateExposedThrough,
    raftStateAppliedThrough,
    raftStateLastLogIndex,
    raftStateLastLogTerm,
    raftStateLogEntries,
    raftStateCheckpoint,
    raftStateInstallingCheckpoint,
    raftStateElectionGeneration,
    raftStateHeartbeatGeneration,
    raftStateMatchIndex,
    raftStatePendingProposal,
    raftStateServiceReady,
    raftStateEffectiveConfiguration,
    raftStateCommittedConfiguration,
    raftStateReplicationTargets,
    raftStateParticipation,
    raftStateCanVote,
    raftStateCanCampaign,
    raftStateCatchUpFrontier,
    raftStateLearnerReadiness,
    raftStateConfigurationChangeReady,
    raftStateRecentLeaderGeneration,
    raftStateRecentLeaderGuardActive,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Raft.Checkpoint (RaftCheckpoint)
import Eclips.Raft.Configuration
import Eclips.Raft.Genesis (checkedRaftLocalNode)
import Eclips.Raft.Identity
import Eclips.Raft.Input (RaftLogEntry)
import Eclips.Raft.Internal.Log (raftLogEntries, raftLogLastIndex, raftLogLastTerm)
import Eclips.Raft.Internal.State

raftStateLocalNode :: RaftState bytes -> RaftNodeId
raftStateLocalNode = checkedRaftLocalNode . stateGenesis

raftStateVoters :: RaftState bytes -> [RaftNodeId]
raftStateVoters =
  raftVotingConfigurationNodes . snd . effectiveConfiguration

raftStateTerm :: RaftState bytes -> RaftTerm
raftStateTerm = stateTerm

raftStateVotedFor :: RaftState bytes -> Maybe RaftNodeId
raftStateVotedFor = stateVotedFor

raftStateRole :: RaftState bytes -> RaftRole
raftStateRole = stateRole

raftStateLeaderHint :: RaftState bytes -> Maybe RaftNodeId
raftStateLeaderHint = stateLeaderHint

raftStateCommitIndex :: RaftState bytes -> RaftLogIndex
raftStateCommitIndex = stateCommitIndex

raftStateExposedThrough :: RaftState bytes -> RaftLogIndex
raftStateExposedThrough = stateExposedThrough

raftStateAppliedThrough :: RaftState bytes -> RaftLogIndex
raftStateAppliedThrough = stateAppliedThrough

raftStateLastLogIndex :: RaftState bytes -> RaftLogIndex
raftStateLastLogIndex = raftLogLastIndex . stateLog

raftStateLastLogTerm :: RaftState bytes -> RaftTerm
raftStateLastLogTerm = raftLogLastTerm . stateLog

raftStateLogEntries :: RaftState bytes -> [RaftLogEntry bytes]
raftStateLogEntries = raftLogEntries . stateLog

raftStateCheckpoint :: RaftState bytes -> Maybe (RaftCheckpoint bytes)
raftStateCheckpoint = stateCheckpoint

raftStateInstallingCheckpoint :: RaftState bytes -> Maybe RaftLogIndex
raftStateInstallingCheckpoint = stateInstallingCheckpoint

raftStateElectionGeneration :: RaftState bytes -> RaftElectionTimerGeneration
raftStateElectionGeneration = stateElectionGeneration

raftStateHeartbeatGeneration :: RaftState bytes -> RaftHeartbeatTimerGeneration
raftStateHeartbeatGeneration = stateHeartbeatGeneration

raftStateMatchIndex :: RaftNodeId -> RaftState bytes -> Maybe RaftLogIndex
raftStateMatchIndex voter = Map.lookup voter . stateMatchIndexes

raftStatePendingProposal :: RaftState bytes -> Maybe (RaftProposalId, RaftLogIndex)
raftStatePendingProposal state = case statePendingProposal state of
  Nothing -> Nothing
  Just (RaftPendingProposal proposal index) -> Just (proposal, index)

raftStateServiceReady :: RaftState bytes -> Bool
raftStateServiceReady = raftStateIsServiceReady

raftStateEffectiveConfiguration :: RaftState bytes -> (RaftConfigurationRef, RaftVotingConfiguration)
raftStateEffectiveConfiguration = effectiveConfiguration
raftStateCommittedConfiguration :: RaftState bytes -> (RaftConfigurationRef, RaftVotingConfiguration)
raftStateCommittedConfiguration = committedConfiguration
raftStateReplicationTargets :: RaftState bytes -> [RaftNodeId]
raftStateReplicationTargets = Set.toAscList . replicationTargets
raftStateParticipation :: RaftState bytes -> RaftParticipation
raftStateParticipation = localParticipation
raftStateCatchUpFrontier :: RaftState bytes -> Maybe RaftCatchUpFrontier
raftStateCatchUpFrontier state = case stateConfigurationPreparation state of
  Just (RaftConfigurationPreparation _ _ frontier _) -> Just frontier
  Nothing -> Nothing
raftStateLearnerReadiness :: RaftCatchUpFrontier -> RaftState bytes -> Maybe RaftLearnerReadiness
raftStateLearnerReadiness = learnerReadiness
raftStateConfigurationChangeReady :: RaftState bytes -> Bool
raftStateConfigurationChangeReady state = stateConfigurationPreparation state /= Nothing && null (configurationPreparationMissing state)
raftStateRecentLeaderGeneration :: RaftState bytes -> RaftRecentLeaderTimerGeneration
raftStateRecentLeaderGeneration = stateRecentLeaderGeneration
raftStateRecentLeaderGuardActive :: RaftState bytes -> Bool
raftStateRecentLeaderGuardActive = stateRecentLeaderActive

-- | The latest logged configuration controls voting immediately.
raftStateCanVote :: RaftState bytes -> Bool
raftStateCanVote = localCanVote

-- | An uncommitted final exclusion still permits campaigning to finish it.
raftStateCanCampaign :: RaftState bytes -> Bool
raftStateCanCampaign = localCanCampaign
