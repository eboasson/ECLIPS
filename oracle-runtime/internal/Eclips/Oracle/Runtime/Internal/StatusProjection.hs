-- | Pure change guards for the independently owned runtime status fields.
module Eclips.Oracle.Runtime.Internal.StatusProjection
  ( membershipStatusProjectionChanged,
    raftStatusProjectionChanged,
    RaftStatusProjection (..),
    raftStatusProjection,
  )
where

import Data.Maybe (isJust)
import Data.Word (Word64)
import Eclips.Domain.Membership (HeraldMembershipGeneration)
import Eclips.Raft.Configuration (RaftCatchUpFrontier, RaftConfigurationRef, RaftVotingConfiguration)
import Eclips.Raft.Identity
  ( RaftLogIndex,
    RaftNodeId,
    RaftTerm,
  )
import Eclips.Raft.State
  ( RaftRole,
    RaftState,
    raftStateAppliedThrough,
    raftStateCatchUpFrontier,
    raftStateEffectiveConfiguration,
    raftStateLeaderHint,
    raftStateRole,
    raftStateServiceReady,
    raftStateTerm,
  )

-- Materialize the small native facts on their owner before publishing them.
-- In particular, a deferred generation increment would retain the predecessor
-- status and the Raft state captured by the configuration comparison.
data RaftStatusProjection
  = RaftStatusProjection
      !RaftRole
      !RaftTerm
      !(Maybe RaftNodeId)
      !Bool
      !RaftLogIndex
      !(Maybe RaftCatchUpFrontier)
      !RaftConfigurationRef
      !RaftVotingConfiguration
      !Word64

raftStatusProjection ::
  RaftState bytes -> Word64 -> (RaftConfigurationRef, RaftVotingConfiguration) -> RaftStatusProjection
raftStatusProjection state precedingGeneration precedingConfiguration =
  RaftStatusProjection
    (raftStateRole state)
    (raftStateTerm state)
    (raftStateLeaderHint state)
    (raftStateServiceReady state || isJust frontier)
    (raftStateAppliedThrough state)
    frontier
    reference
    configuration
    (precedingGeneration + if (reference, configuration) /= precedingConfiguration then 1 else 0)
  where
    frontier = raftStateCatchUpFrontier state
    (reference, configuration) = raftStateEffectiveConfiguration state

-- | Whether adopting the supplied Raft successor changes any routing or
-- readiness field published at the runtime boundary. Fields owned by the
-- Oracle and watch-ledger owners are deliberately absent from this decision.
raftStatusProjectionChanged ::
  RaftState entry ->
  RaftRole ->
  RaftTerm ->
  Maybe RaftNodeId ->
  Bool ->
  Bool
raftStatusProjectionChanged state currentRole currentTerm currentLeaderHint currentServiceReady =
  raftStateRole state /= currentRole
    || raftStateTerm state /= currentTerm
    || raftStateLeaderHint state /= currentLeaderHint
    || raftStateServiceReady state /= currentServiceReady

-- | Whether the Oracle owner's installed state changes the membership fact
-- shared with Hello and established-lane admission.
membershipStatusProjectionChanged ::
  HeraldMembershipGeneration ->
  HeraldMembershipGeneration ->
  Bool
membershipStatusProjectionChanged successor current = successor /= current
