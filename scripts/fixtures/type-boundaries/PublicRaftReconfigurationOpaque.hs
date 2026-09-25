{-# LANGUAGE CPP #-}

module PublicRaftReconfigurationOpaque where

#if defined(RAFT_VOTER_SET_CONSTRUCTOR)
import Eclips.Raft.Configuration (RaftVoterSet (..))
forged :: RaftVoterSet
forged = RaftVoterSet []
#elif defined(RAFT_STABLE_CONSTRUCTOR)
import Eclips.Raft.Configuration (RaftVotingConfiguration (..))
forged :: RaftVotingConfiguration
forged = StableRaftConfiguration undefined
#elif defined(RAFT_JOINT_CONSTRUCTOR)
import Eclips.Raft.Configuration (RaftVotingConfiguration (..))
forged :: RaftVotingConfiguration
forged = JointRaftConfiguration undefined undefined
#elif defined(RAFT_CONFIGURATION_REF_CONSTRUCTOR)
import Eclips.Raft.Configuration (RaftConfigurationRef (..))
forged :: RaftConfigurationRef
forged = RaftConfigurationEntryRef undefined undefined
#elif defined(RAFT_FRONTIER_CONSTRUCTOR)
import Eclips.Raft.Configuration (RaftCatchUpFrontier (..))
forged :: RaftCatchUpFrontier
forged = RaftCatchUpFrontier undefined undefined undefined undefined undefined
#elif defined(RAFT_READINESS_CONSTRUCTOR)
import Eclips.Raft.Configuration (RaftLearnerReadiness (..))
forged :: RaftLearnerReadiness
forged = RaftLearnerReadiness undefined undefined
#elif defined(RAFT_COMMITTED_ENTRY_CONSTRUCTOR)
import Eclips.Raft.Effect (RaftCommittedEntry (..))
forged :: RaftCommittedEntry ()
forged = RaftCommittedEntry undefined undefined undefined
#elif defined(RAFT_CHECKPOINT_CONSTRUCTOR)
import Eclips.Raft.Checkpoint (RaftCheckpoint (..))
forged :: RaftCheckpoint ()
forged = RaftCheckpoint undefined undefined undefined undefined ()
#elif defined(RAFT_CHECKPOINT_RECORD_UPDATE)
import Eclips.Raft.Checkpoint (RaftCheckpoint, raftCheckpointIndex)
forged :: RaftCheckpoint () -> RaftCheckpoint ()
forged checkpoint = checkpoint {raftCheckpointIndex = undefined}
#elif defined(RAFT_CHECKPOINT_COERCION)
import Data.Coerce (coerce)
import Eclips.Raft.Checkpoint (RaftCheckpoint)
import Eclips.Raft.Configuration (RaftConfigurationRef, RaftVotingConfiguration)
import Eclips.Raft.Identity (RaftLogIndex, RaftTerm)
forged :: (RaftLogIndex, RaftTerm, RaftConfigurationRef, RaftVotingConfiguration, ()) -> RaftCheckpoint ()
forged = coerce
#elif defined(RAFT_FRONTIER_RECORD_UPDATE)
import Eclips.Raft.Configuration (RaftCatchUpFrontier, raftCatchUpFrontierIndex)
forged :: RaftCatchUpFrontier -> RaftCatchUpFrontier
forged frontier = frontier {raftCatchUpFrontierIndex = undefined}
#elif defined(RAFT_COMMITTED_ENTRY_RECORD_UPDATE)
import Eclips.Raft.Effect (RaftCommittedEntry, committedEntryIndex)
forged :: RaftCommittedEntry () -> RaftCommittedEntry ()
forged entry = entry {committedEntryIndex = undefined}
#elif defined(RAFT_CONFIGURATION_INTERNAL_IMPORT)
import Eclips.Raft.Internal.Configuration (RaftVoterSet)
forged :: Maybe RaftVoterSet
forged = Nothing
#elif defined(RAFT_LOG_INTERNAL_IMPORT)
import Eclips.Raft.Internal.Log (RaftCommittedEntry)
forged :: Maybe (RaftCommittedEntry ())
forged = Nothing
#elif defined(RAFT_VOTER_SET_COERCION)
import Data.Coerce (coerce)
import Eclips.Raft.Configuration (RaftVoterSet)
import Eclips.Raft.Identity (RaftNodeId)
forged :: [RaftNodeId] -> RaftVoterSet
forged = coerce
#else
#error "select one Raft reconfiguration opacity fixture"
#endif
