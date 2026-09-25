-- | An application checkpoint paired with the committed native Raft prefix it
-- represents. The application bytes are opaque to the native consensus owner.
module Eclips.Raft.Checkpoint
  ( RaftCheckpoint,
    RaftCheckpointFault (..),
    raftCheckpoint,
    raftCheckpointIndex,
    raftCheckpointTerm,
    raftCheckpointConfigurationRef,
    raftCheckpointConfiguration,
    raftCheckpointPayload,
  )
where

import Eclips.Raft.Configuration
import Eclips.Raft.Identity

data RaftCheckpoint bytes = RaftCheckpoint RaftLogIndex RaftTerm RaftConfigurationRef RaftVotingConfiguration bytes
  deriving stock (Eq, Show)

data RaftCheckpointFault
  = RaftCheckpointIndexIsZero
  | RaftCheckpointTermIsZero
  | RaftCheckpointConfigurationAfterPrefix
  deriving stock (Eq, Show)

raftCheckpoint :: RaftLogIndex -> RaftTerm -> RaftConfigurationRef -> RaftVotingConfiguration -> bytes -> Either RaftCheckpointFault (RaftCheckpoint bytes)
raftCheckpoint index term reference configuration payload
  | raftLogIndexWord64 index == 0 = Left RaftCheckpointIndexIsZero
  | raftTermWord64 term == 0 = Left RaftCheckpointTermIsZero
  | otherwise = case raftConfigurationRefView reference of
      RaftConfigurationEntryRefView configurationIndex configurationTerm
        | configurationIndex > index || configurationTerm > term -> Left RaftCheckpointConfigurationAfterPrefix
      _ -> Right (RaftCheckpoint index term reference configuration payload)

raftCheckpointIndex :: RaftCheckpoint bytes -> RaftLogIndex
raftCheckpointIndex (RaftCheckpoint index _ _ _ _) = index
raftCheckpointTerm :: RaftCheckpoint bytes -> RaftTerm
raftCheckpointTerm (RaftCheckpoint _ term _ _ _) = term
raftCheckpointConfigurationRef :: RaftCheckpoint bytes -> RaftConfigurationRef
raftCheckpointConfigurationRef (RaftCheckpoint _ _ reference _ _) = reference
raftCheckpointConfiguration :: RaftCheckpoint bytes -> RaftVotingConfiguration
raftCheckpointConfiguration (RaftCheckpoint _ _ _ configuration _) = configuration
raftCheckpointPayload :: RaftCheckpoint bytes -> bytes
raftCheckpointPayload (RaftCheckpoint _ _ _ _ payload) = payload
