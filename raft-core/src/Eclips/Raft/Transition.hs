-- | Pure top-level Raft initialization and transition.
module Eclips.Raft.Transition
  ( RaftFault (..),
    initialRaft,
    initialRaftEffects,
    stepRaft,
    auditRaftState,
  )
where

import Eclips.Raft.Effect
  ( RaftEffect (RequestElectionTimeout),
    RaftEffectBatch,
  )
import Eclips.Raft.Genesis (CheckedRaftGenesis)
import Eclips.Raft.Input (RaftInput)
import Eclips.Raft.Internal.Log (sealRaftBatch)
import Eclips.Raft.Internal.Prepared
  ( RaftFault (..),
    commitRaftTransition,
    prepareRaftTransition,
  )
import Eclips.Raft.Internal.State
  ( RaftState,
    initialRaftState,
    stateElectionActive,
    stateElectionGeneration,
    validateRaftState,
  )

initialRaft :: CheckedRaftGenesis -> RaftState bytes
initialRaft = initialRaftState

-- | Initial timeout-selection intention, handed off exactly once after the owner
-- adopts 'initialRaft'.
initialRaftEffects :: RaftState bytes -> RaftEffectBatch bytes
initialRaftEffects state =
  sealRaftBatch [RequestElectionTimeout (stateElectionGeneration state) | stateElectionActive state]

-- | Apply one admitted observation to an opaque state.  Equality is used only
-- to enforce Raft's equal-index/equal-term log-matching rule over otherwise
-- uninterpreted application values.
stepRaft ::
  (Eq bytes) =>
  RaftInput bytes ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, RaftEffectBatch bytes)
stepRaft input state =
  commitRaftTransition <$> prepareRaftTransition input state

-- | Explicit exhaustive diagnostic and property audit of the retained log,
-- configuration index and local state. Ordinary transitions preserve admitted
-- log evidence and do not replay this audit on timers or acknowledgements.
auditRaftState :: RaftState bytes -> Either RaftFault ()
auditRaftState state = either (Left . RaftPredecessorInvariantFault) Right (validateRaftState state)
