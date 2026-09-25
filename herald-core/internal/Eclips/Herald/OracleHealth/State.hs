{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | One outstanding owner-minted round and a retained native configuration
-- floor. Fresh quorum evidence is never carried into the next round.
module Eclips.Herald.OracleHealth.State
  ( State,
    initialState,
    initialStateWithCadence,
    currentRound,
    roundCadence,
    observeRound,
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Herald.OracleHealth
import Eclips.Raft.Configuration
import Eclips.Raft.Identity (RaftNodeId, RaftTerm, raftTerm)

data Selection = Selection RaftTerm RaftConfigurationRef RaftVotingConfiguration
  deriving stock (Eq, Show)

data State = State
  { roundNumber :: Word64,
    cadence :: Word64,
    selected :: Selection,
    observations :: Map.Map RaftNodeId OracleHealthObservation
  }
  deriving stock (Eq, Show)

initialState :: Word64 -> RaftConfigurationRef -> RaftVotingConfiguration -> State
initialState grace reference configuration =
  initialStateWithCadence (min 1000000 (max 1 (grace `div` 3))) reference configuration

-- | Checked deployment timing supplies the cadence independently of fencing.
initialStateWithCadence :: Word64 -> RaftConfigurationRef -> RaftVotingConfiguration -> State
initialStateWithCadence cadence reference configuration =
  State 1 cadence (Selection (raftTerm 0) reference configuration) Map.empty

currentRound :: State -> Word64
currentRound = (.roundNumber)

roundCadence :: State -> Word64
roundCadence = (.cadence)

-- | A higher native term permits replacement of an uncommitted configuration;
-- no observation can cross the latest committed floor. Within one term a
-- delayed stable reply cannot relax a learned joint reference. Only fresh
-- replies agreeing on the complete selected native coordinate form a quorum.
observeRound :: Word64 -> RaftConfigurationRef -> RaftVotingConfiguration -> Set.Set RaftNodeId -> [OracleHealthObservation] -> State -> Maybe (State, Bool)
observeRound roundNumber floorReference floorConfiguration known replies state
  | roundNumber /= state.roundNumber = Nothing
  | otherwise = Just (state {roundNumber = roundNumber + 1, selected = chosen, observations = retained}, healthy)
  where
    aboveFloor reply =
      oracleHealthObservationReference reply > floorReference
        || (oracleHealthObservationReference reply == floorReference && oracleHealthObservationConfiguration reply == floorConfiguration)
    fresh reply = case Map.lookup (oracleHealthObservationNode reply) state.observations of
      Nothing -> True
      Just previous ->
        oracleHealthObservationTerm reply >= oracleHealthObservationTerm previous
          && oracleHealthObservationGeneration reply >= oracleHealthObservationGeneration previous
    admitted = filter (\reply -> Set.member (oracleHealthObservationNode reply) known && aboveFloor reply && fresh reply) replies
    retained = foldl' (\entries reply -> Map.insert (oracleHealthObservationNode reply) reply entries) state.observations admitted
    Selection priorTerm priorReference priorConfiguration = state.selected
    base
      | priorReference < floorReference = Selection priorTerm floorReference floorConfiguration
      | otherwise = Selection priorTerm priorReference priorConfiguration
    choose current@(Selection currentTerm currentReference _) reply
      | term > currentTerm || (term == currentTerm && reference > currentReference) = Selection term reference (oracleHealthObservationConfiguration reply)
      | otherwise = current
      where
        term = oracleHealthObservationTerm reply
        reference = oracleHealthObservationReference reply
    chosen@(Selection selectedTerm selectedReference selectedConfiguration) = foldl' choose base admitted
    matching =
      Set.fromList
        [ oracleHealthObservationNode reply
        | reply <- admitted,
          oracleHealthObservationTerm reply == selectedTerm,
          oracleHealthObservationReference reply == selectedReference,
          oracleHealthObservationConfiguration reply == selectedConfiguration
        ]
    healthy = raftConfigurationHasQuorum selectedConfiguration matching
