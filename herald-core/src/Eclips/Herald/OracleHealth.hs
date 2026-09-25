-- | Fresh immutable native-owner observations, correlated by an enclosing
-- Herald-owned round. Contacts and ordinary peer connections confer no health.
module Eclips.Herald.OracleHealth
  ( OracleHealthObservation,
    oracleHealthObservation,
    oracleHealthObservationNode,
    oracleHealthObservationTerm,
    oracleHealthObservationGeneration,
    oracleHealthObservationReference,
    oracleHealthObservationConfiguration,
  ) where

import Data.Word (Word64)
import Eclips.Raft.Configuration (RaftConfigurationRef, RaftVotingConfiguration)
import Eclips.Raft.Identity (RaftNodeId, RaftTerm)

data OracleHealthObservation = OracleHealthObservation RaftNodeId RaftTerm Word64 RaftConfigurationRef RaftVotingConfiguration
  deriving stock (Eq, Show)

oracleHealthObservation :: RaftNodeId -> RaftTerm -> Word64 -> RaftConfigurationRef -> RaftVotingConfiguration -> OracleHealthObservation
oracleHealthObservation = OracleHealthObservation

oracleHealthObservationNode :: OracleHealthObservation -> RaftNodeId
oracleHealthObservationNode (OracleHealthObservation node _ _ _ _) = node

oracleHealthObservationTerm :: OracleHealthObservation -> RaftTerm
oracleHealthObservationTerm (OracleHealthObservation _ term _ _ _) = term

oracleHealthObservationGeneration :: OracleHealthObservation -> Word64
oracleHealthObservationGeneration (OracleHealthObservation _ _ generation _ _) = generation

oracleHealthObservationReference :: OracleHealthObservation -> RaftConfigurationRef
oracleHealthObservationReference (OracleHealthObservation _ _ _ reference _) = reference

oracleHealthObservationConfiguration :: OracleHealthObservation -> RaftVotingConfiguration
oracleHealthObservationConfiguration (OracleHealthObservation _ _ _ _ configuration) = configuration
