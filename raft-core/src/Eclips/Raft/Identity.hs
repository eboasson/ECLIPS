-- | Domain-independent identities used by the pure Raft kernel.
--
-- None of these types can be converted to an ECLIPS semantic identity.  The
-- runtime and protocol layers may carry their representations, but only this
-- package assigns Raft meaning to them.
module Eclips.Raft.Identity
  ( RaftIdentityFault (..),
    RaftNodeId,
    mkRaftNodeId,
    raftNodeIdBytes,
    RaftTerm,
    raftTerm,
    raftTermWord64,
    RaftLogIndex,
    raftLogIndex,
    raftLogIndexWord64,
    RaftProposalId,
    mkRaftProposalId,
    raftProposalIdWord64,
    RaftDurationMicros,
    mkRaftDurationMicros,
    raftDurationMicrosWord64,
    RaftElectionTimerGeneration,
    raftElectionTimerGeneration,
    raftElectionTimerGenerationWord64,
    RaftHeartbeatTimerGeneration,
    raftHeartbeatTimerGeneration,
    raftHeartbeatTimerGenerationWord64,
    RaftRecentLeaderTimerGeneration,
    raftRecentLeaderTimerGeneration,
    raftRecentLeaderTimerGenerationWord64,
    RaftDispatchGeneration,
    raftDispatchGeneration,
    raftDispatchGenerationWord64,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word (Word64)
import Eclips.Public.Types.Diagnostic (renderGroupedHex)

raftNodeIdByteCount :: Int
raftNodeIdByteCount = 32

-- | A structurally invalid Raft identity or positive quantity.
data RaftIdentityFault
  = WrongRaftNodeIdByteCount Int Int
  | ZeroRaftProposalId
  | ZeroRaftDuration
  deriving stock (Eq, Show)

-- | Opaque native replica identity, exactly 256 bits.
newtype RaftNodeId = RaftNodeId ByteString
  deriving stock (Eq, Ord)

instance Show RaftNodeId where
  show = renderGroupedHex . raftNodeIdBytes

mkRaftNodeId :: ByteString -> Either RaftIdentityFault RaftNodeId
mkRaftNodeId bytes
  | ByteString.length bytes == raftNodeIdByteCount = Right (RaftNodeId bytes)
  | otherwise =
      Left
        (WrongRaftNodeIdByteCount raftNodeIdByteCount (ByteString.length bytes))

raftNodeIdBytes :: RaftNodeId -> ByteString
raftNodeIdBytes (RaftNodeId bytes) = bytes

-- | Monotone Raft term.  Zero is the genesis term.
newtype RaftTerm = RaftTerm Word64
  deriving stock (Eq, Ord, Show)

raftTerm :: Word64 -> RaftTerm
raftTerm = RaftTerm

raftTermWord64 :: RaftTerm -> Word64
raftTermWord64 (RaftTerm value) = value

-- | Raft log position.  Zero denotes the immutable sentinel/base.
newtype RaftLogIndex = RaftLogIndex Word64
  deriving stock (Eq, Ord, Show)

raftLogIndex :: Word64 -> RaftLogIndex
raftLogIndex = RaftLogIndex

raftLogIndexWord64 :: RaftLogIndex -> Word64
raftLogIndexWord64 (RaftLogIndex value) = value

-- | Positive runtime-to-owner correlation for one opaque proposal.
newtype RaftProposalId = RaftProposalId Word64
  deriving stock (Eq, Ord, Show)

mkRaftProposalId :: Word64 -> Either RaftIdentityFault RaftProposalId
mkRaftProposalId 0 = Left ZeroRaftProposalId
mkRaftProposalId value = Right (RaftProposalId value)

raftProposalIdWord64 :: RaftProposalId -> Word64
raftProposalIdWord64 (RaftProposalId value) = value

-- | Positive native timer duration in microseconds.
newtype RaftDurationMicros = RaftDurationMicros Word64
  deriving stock (Eq, Ord, Show)

mkRaftDurationMicros :: Word64 -> Either RaftIdentityFault RaftDurationMicros
mkRaftDurationMicros 0 = Left ZeroRaftDuration
mkRaftDurationMicros value = Right (RaftDurationMicros value)

raftDurationMicrosWord64 :: RaftDurationMicros -> Word64
raftDurationMicrosWord64 (RaftDurationMicros value) = value

-- | Logical generation of an election timer intention.
newtype RaftElectionTimerGeneration = RaftElectionTimerGeneration Word64
  deriving stock (Eq, Ord, Show)

raftElectionTimerGeneration :: Word64 -> RaftElectionTimerGeneration
raftElectionTimerGeneration = RaftElectionTimerGeneration

raftElectionTimerGenerationWord64 :: RaftElectionTimerGeneration -> Word64
raftElectionTimerGenerationWord64 (RaftElectionTimerGeneration value) = value

-- | Logical generation of a heartbeat timer intention.
newtype RaftHeartbeatTimerGeneration = RaftHeartbeatTimerGeneration Word64
  deriving stock (Eq, Ord, Show)

raftHeartbeatTimerGeneration :: Word64 -> RaftHeartbeatTimerGeneration
raftHeartbeatTimerGeneration = RaftHeartbeatTimerGeneration

raftHeartbeatTimerGenerationWord64 :: RaftHeartbeatTimerGeneration -> Word64
raftHeartbeatTimerGenerationWord64 (RaftHeartbeatTimerGeneration value) = value

-- | Exact expiry generation for the minimum-election-time leader guard.
newtype RaftRecentLeaderTimerGeneration = RaftRecentLeaderTimerGeneration Word64
  deriving stock (Eq, Ord, Show)

raftRecentLeaderTimerGeneration :: Word64 -> RaftRecentLeaderTimerGeneration
raftRecentLeaderTimerGeneration = RaftRecentLeaderTimerGeneration

raftRecentLeaderTimerGenerationWord64 :: RaftRecentLeaderTimerGeneration -> Word64
raftRecentLeaderTimerGenerationWord64 (RaftRecentLeaderTimerGeneration value) = value

-- | Local correlation for one per-peer RPC response expectation.
--
-- It is never encoded in ERFT.  A connection worker attaches the generation of
-- the request whose response it observed before returning that response to the
-- kernel.  Exact retransmissions of the same outstanding logical request retain
-- their generation, so heartbeat cadence cannot invalidate every response when
-- round-trip time exceeds the heartbeat interval.
newtype RaftDispatchGeneration = RaftDispatchGeneration Word64
  deriving stock (Eq, Ord, Show)

raftDispatchGeneration :: Word64 -> RaftDispatchGeneration
raftDispatchGeneration = RaftDispatchGeneration

raftDispatchGenerationWord64 :: RaftDispatchGeneration -> Word64
raftDispatchGenerationWord64 (RaftDispatchGeneration value) = value
