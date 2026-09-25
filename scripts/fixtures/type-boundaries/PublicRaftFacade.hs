module PublicRaftFacade where

import Data.ByteString (ByteString)
import Data.Set (Set)
import Eclips.Raft.Checkpoint
  ( RaftCheckpoint,
    RaftCheckpointFault,
    raftCheckpoint,
    raftCheckpointConfiguration,
    raftCheckpointConfigurationRef,
    raftCheckpointIndex,
    raftCheckpointPayload,
    raftCheckpointTerm,
  )
import Eclips.Raft.Configuration
  ( RaftConfigurationFault,
    RaftConfigurationRef,
    RaftVotingConfiguration,
    genesisRaftConfigurationRef,
    jointRaftConfiguration,
    raftConfigurationHasQuorum,
    raftVoterSet,
    stableRaftConfiguration,
  )
import Eclips.Raft.Effect
  ( RaftCommittedEntry,
    committedEntryIndex,
    committedEntryPayload,
    committedEntryTerm,
  )
import Eclips.Raft.Identity (RaftLogIndex, RaftNodeId, RaftTerm)
import Eclips.Raft.Input (RaftEntry)

configurations ::
  [RaftNodeId] ->
  [RaftNodeId] ->
  Either RaftConfigurationFault (RaftConfigurationRef, RaftVotingConfiguration, RaftVotingConfiguration)
configurations oldNodes newNodes = do
  old <- raftVoterSet oldNodes
  new <- raftVoterSet newNodes
  pure (genesisRaftConfigurationRef, jointRaftConfiguration old new, stableRaftConfiguration new)

hasQuorum :: RaftVotingConfiguration -> Set RaftNodeId -> Bool
hasQuorum = raftConfigurationHasQuorum

inspectCommitted :: RaftCommittedEntry ByteString -> (RaftLogIndex, RaftTerm, RaftEntry ByteString)
inspectCommitted entry = (committedEntryIndex entry, committedEntryTerm entry, committedEntryPayload entry)

checkpoint :: RaftLogIndex -> RaftTerm -> RaftConfigurationRef -> RaftVotingConfiguration -> ByteString -> Either RaftCheckpointFault (RaftCheckpoint ByteString)
checkpoint = raftCheckpoint

inspectCheckpoint :: RaftCheckpoint ByteString -> (RaftLogIndex, RaftTerm, RaftConfigurationRef, RaftVotingConfiguration, ByteString)
inspectCheckpoint value =
  ( raftCheckpointIndex value,
    raftCheckpointTerm value,
    raftCheckpointConfigurationRef value,
    raftCheckpointConfiguration value,
    raftCheckpointPayload value
  )
