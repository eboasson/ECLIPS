-- | Sealed output batches returned by the pure Raft transition.
module Eclips.Raft.Effect
  ( RaftProposalStatus (..),
    RaftCommittedEntry,
    committedEntryIndex,
    committedEntryPayload,
    committedEntryTerm,
    RaftDiagnostic (..),
    RaftEffect (..),
    RaftEffectBatch,
    raftEffectBatchEffects,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Eclips.Raft.Checkpoint (RaftCheckpoint)
import Eclips.Raft.Configuration (RaftCatchUpFrontier)
import Eclips.Raft.Identity
  ( RaftDispatchGeneration,
    RaftDurationMicros,
    RaftElectionTimerGeneration,
    RaftHeartbeatTimerGeneration,
    RaftLogIndex,
    RaftNodeId,
    RaftProposalId,
    RaftRecentLeaderTimerGeneration,
    RaftTerm,
  )
import Eclips.Raft.Input (RaftRpc)
import Eclips.Raft.Internal.Log
  ( RaftCommittedEntry,
    SealedRaftBatch,
    committedEntryIndex,
    committedEntryPayload,
    committedEntryTerm,
    sealedRaftBatchEffects,
  )

data RaftProposalStatus
  = RaftProposalAppended RaftLogIndex
  | RaftProposalRedirected (Maybe RaftNodeId) RaftTerm
  | RaftProposalNotReady RaftTerm
  | RaftProposalCommitted RaftLogIndex
  | RaftConfigurationWaiting RaftCatchUpFrontier [RaftNodeId]
  | RaftConfigurationPrepared RaftCatchUpFrontier
  | RaftConfigurationRejected
  | RaftConfigurationCancelled
  deriving stock (Eq, Show)

-- | Structured diagnostics that carry no semantic decision of their own.
data RaftDiagnostic
  = RaftTermAdvanced RaftTerm RaftTerm
  | RaftCommitAdvanced RaftLogIndex RaftLogIndex
  | RaftServiceReadinessChanged Bool
  deriving stock (Eq, Show)

data RaftEffect bytes
  = SendRaftRpc
      RaftNodeId
      RaftDispatchGeneration
      (RaftRpc bytes)
  | RequestElectionTimeout RaftElectionTimerGeneration
  | ArmElectionTimer
      RaftElectionTimerGeneration
      RaftDurationMicros
  | SupersedeElectionTimer RaftElectionTimerGeneration
  | ArmHeartbeatTimer
      RaftHeartbeatTimerGeneration
      RaftDurationMicros
  | SupersedeHeartbeatTimer RaftHeartbeatTimerGeneration
  | ArmRecentLeaderTimer RaftRecentLeaderTimerGeneration RaftDurationMicros
  | SupersedeRecentLeaderTimer RaftRecentLeaderTimerGeneration
  | ReportRaftProposal RaftProposalId RaftProposalStatus
  | ExposeCommittedEntries (NonEmpty (RaftCommittedEntry bytes))
  | InstallRaftCheckpoint (RaftCheckpoint bytes)
  | -- | One acknowledgement of a successful local checkpoint offer, including
    -- ignored offers. The retained checkpoint is the actual successor value,
    -- not necessarily the offered image. Its immutable payload is shared.
    ReportRaftCheckpointOffer RaftLogIndex !(Maybe (RaftCheckpoint bytes))
  | EmitRaftDiagnostic RaftDiagnostic
  deriving stock (Eq, Show)

-- | One sealed, indivisible batch produced only with a checked successor.
--
-- Opacity prevents callers from fabricating semantic transition output.  It
-- cannot enforce scheduling by itself: the runtime owner must still adopt the
-- returned successor before handing this complete value to its dispatcher.
type RaftEffectBatch bytes = SealedRaftBatch (RaftEffect bytes)

raftEffectBatchEffects :: RaftEffectBatch bytes -> [RaftEffect bytes]
raftEffectBatchEffects = sealedRaftBatchEffects
