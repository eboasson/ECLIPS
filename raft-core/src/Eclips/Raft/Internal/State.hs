{-# LANGUAGE StrictData #-}

module Eclips.Raft.Internal.State
  ( RaftRole (..),
    RaftDispatchExpectation (..),
    RaftPendingProposal (..),
    RaftConfigurationPreparation (..),
    RaftState (..),
    RaftStateInvariantFault (..),
    initialRaftState,
    raftStateIsServiceReady,
    raftStateAdapterReady,
    validateRaftState,
    validateRaftStateFields,
    effectiveConfiguration,
    committedConfiguration,
    configurationAt,
    replicationTargets,
    localCanCampaign,
    localCanVote,
    localParticipation,
    configurationPreparationMissing,
    learnerReadiness,
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Raft.Checkpoint
import Eclips.Raft.Genesis
import Eclips.Raft.Identity
import Eclips.Raft.Input
import Eclips.Raft.Internal.Configuration
import Eclips.Raft.Internal.Log

data RaftRole = RaftFollower | RaftCandidate | RaftLeader
  deriving stock (Eq, Ord, Show)
data RaftDispatchExpectation
  = AwaitVoteResponse RaftTerm
  | AwaitAppendResponse RaftTerm RaftLogIndex RaftLogIndex
  | AwaitSnapshotResponse RaftTerm RaftLogIndex
  deriving stock (Eq, Show)
data RaftPendingProposal = RaftPendingProposal RaftProposalId RaftLogIndex
  deriving stock (Eq, Show)
data RaftConfigurationPreparation = RaftConfigurationPreparation RaftProposalId RaftVoterSet RaftCatchUpFrontier (Set RaftNodeId)
  deriving stock (Eq, Show)

-- | The serialized native owner alone retains this state. Configuration views
-- are derived from its finite log, so suffix replacement cannot leave stale sets.
-- Materialize each field when adopting a successor. In particular, singleton
-- quorum-floor updates must not leave cold projections of predecessor states
-- retaining their superseded application checkpoints.
data RaftState bytes = RaftState
  { stateGenesis :: CheckedRaftGenesis,
    stateTerm :: RaftTerm,
    stateVotedFor :: Maybe RaftNodeId,
    stateLog :: RaftLog bytes,
    stateCheckpoint :: Maybe (RaftCheckpoint bytes),
    stateInstallingCheckpoint :: Maybe RaftLogIndex,
    stateCommitIndex :: RaftLogIndex,
    stateExposedThrough :: RaftLogIndex,
    stateAppliedThrough :: RaftLogIndex,
    stateRole :: RaftRole,
    stateLeaderHint :: Maybe RaftNodeId,
    stateCandidateVotes :: Set RaftNodeId,
    stateRegisteredTargets :: Set RaftNodeId,
    stateNextIndexes :: Map RaftNodeId RaftLogIndex,
    stateMatchIndexes :: Map RaftNodeId RaftLogIndex,
    stateElectionGeneration :: RaftElectionTimerGeneration,
    stateElectionActive :: Bool,
    stateElectionDuration :: Maybe RaftDurationMicros,
    stateHeartbeatGeneration :: RaftHeartbeatTimerGeneration,
    stateHeartbeatActive :: Bool,
    stateRecentLeaderGeneration :: RaftRecentLeaderTimerGeneration,
    stateRecentLeaderActive :: Bool,
    stateRecentLeaderWindowActive :: Bool,
    stateFreshAcknowledgers :: Set RaftNodeId,
    stateQuorumDispatchFloors :: Map RaftNodeId RaftDispatchGeneration,
    stateDispatchGenerations :: Map RaftNodeId RaftDispatchGeneration,
    stateAwaitingResponses :: Map RaftNodeId (RaftDispatchGeneration, RaftDispatchExpectation),
    stateLeaderNoOpIndex :: Maybe RaftLogIndex,
    statePendingProposal :: Maybe RaftPendingProposal,
    stateConfigurationPreparation :: Maybe RaftConfigurationPreparation
  }
  deriving stock (Eq, Show)

data RaftStateInvariantFault
  = RaftStateLogNotContiguous
  | RaftStateSentinelMissing
  | RaftStateLogTermRegression RaftLogIndex RaftTerm RaftTerm
  | RaftStateLogTermAfterCurrentTerm RaftTerm RaftTerm
  | RaftStateCommitAfterLog
  | RaftStateExposureAfterCommit
  | RaftStateApplicationAckAfterExposure
  | RaftStateUnknownCandidateVote RaftNodeId
  | RaftStateCandidateDidNotVoteForSelf
  | RaftStateLeaderDoesNotNameSelf
  | RaftStateLeaderTimerShapeInvalid
  | RaftStateFollowerTimerShapeInvalid
  | RaftStateRecentLeaderTimerShapeInvalid
  | RaftStateLeaderNoOpInvalid
  | RaftStatePendingProposalInvalid RaftLogIndex
  | RaftStateUnknownProgressPeer RaftNodeId
  | RaftStateUnknownAwaitingPeer RaftNodeId
  | RaftStateConfigurationTransitionInvalid RaftLogIndex RaftConfigurationFault
  | RaftStateConfigurationIndexInvalid
  | RaftStateConfigurationPreparationInvalid
  | RaftStateCheckpointInvalid
  deriving stock (Eq, Show)

initialRaftState :: CheckedRaftGenesis -> RaftState bytes
initialRaftState genesis =
  let learner = checkedRaftLocalNode genesis `notElem` raftNativeVoters (checkedRaftNativeConfiguration genesis)
   in RaftState
        { stateGenesis = genesis,
          stateTerm = raftTerm 0,
          stateVotedFor = Nothing,
          stateLog = initialRaftLog (StableRaftConfiguration (RaftVoterSet (raftNativeVoters (checkedRaftNativeConfiguration genesis)))),
          stateCheckpoint = Nothing,
          stateInstallingCheckpoint = Nothing,
          stateCommitIndex = raftLogIndex 0,
          stateExposedThrough = raftLogIndex 0,
          stateAppliedThrough = raftLogIndex 0,
          stateRole = RaftFollower,
          stateLeaderHint = Nothing,
          stateCandidateVotes = Set.empty,
          stateRegisteredTargets = if learner then Set.singleton (checkedRaftLocalNode genesis) else Set.empty,
          stateNextIndexes = Map.empty,
          stateMatchIndexes = Map.empty,
          stateElectionGeneration = raftElectionTimerGeneration 1,
          stateElectionActive = not learner,
          stateElectionDuration = Nothing,
          stateHeartbeatGeneration = raftHeartbeatTimerGeneration 0,
          stateHeartbeatActive = False,
          stateRecentLeaderGeneration = raftRecentLeaderTimerGeneration 0,
          stateRecentLeaderActive = False,
          stateRecentLeaderWindowActive = False,
          stateFreshAcknowledgers = Set.empty,
          stateQuorumDispatchFloors = Map.empty,
          stateDispatchGenerations = Map.empty,
          stateAwaitingResponses = Map.empty,
          stateLeaderNoOpIndex = Nothing,
          statePendingProposal = Nothing,
          stateConfigurationPreparation = Nothing
        }

initialConfiguration :: RaftState bytes -> RaftVotingConfiguration
initialConfiguration = StableRaftConfiguration . RaftVoterSet . raftNativeVoters . checkedRaftNativeConfiguration . stateGenesis

configurationAt :: RaftLogIndex -> RaftState bytes -> (RaftConfigurationRef, RaftVotingConfiguration)
configurationAt limit state = raftLogConfigurationAt limit (stateLog state)

effectiveConfiguration :: RaftState bytes -> (RaftConfigurationRef, RaftVotingConfiguration)
effectiveConfiguration state = configurationAt (raftLogLastIndex (stateLog state)) state
committedConfiguration :: RaftState bytes -> (RaftConfigurationRef, RaftVotingConfiguration)
committedConfiguration state = configurationAt (stateCommitIndex state) state
replicationTargets :: RaftState bytes -> Set RaftNodeId
replicationTargets state = Set.union (stateRegisteredTargets state) (Set.fromList (raftVotingConfigurationNodes (snd (effectiveConfiguration state))))
localCanVote :: RaftState bytes -> Bool
localCanVote state = checkedRaftLocalNode (stateGenesis state) `elem` raftVotingConfigurationNodes (snd (effectiveConfiguration state))
localCanCampaign :: RaftState bytes -> Bool
localCanCampaign state = localCanVote state || checkedRaftLocalNode (stateGenesis state) `elem` raftVotingConfigurationNodes (snd (committedConfiguration state))
localParticipation :: RaftState bytes -> RaftParticipation
localParticipation state
  | localCanCampaign state = RaftVoter
  | Set.member (checkedRaftLocalNode (stateGenesis state)) (stateRegisteredTargets state) = RaftLearner
  | otherwise = RaftRemoved

raftStateAdapterReady :: RaftState bytes -> Bool
raftStateAdapterReady state = stateRole state == RaftLeader && currentNoOpCommitted && stateAppliedThrough state == stateCommitIndex state && statePendingProposal state == Nothing
  where
    currentNoOpCommitted = case stateLeaderNoOpIndex state of
      Just index -> index <= stateCommitIndex state && (raftLogTermAt index (stateLog state) == Just (stateTerm state) || index < raftLogBaseIndex (stateLog state) && raftLogTermAt (raftLogBaseIndex (stateLog state)) (stateLog state) == Just (stateTerm state))
      Nothing -> False
raftStateIsServiceReady :: RaftState bytes -> Bool
raftStateIsServiceReady state = raftStateAdapterReady state && localCanVote state && stateConfigurationPreparation state == Nothing

configurationPreparationMissing :: RaftState bytes -> [RaftNodeId]
configurationPreparationMissing state = case stateConfigurationPreparation state of
  Nothing -> []
  Just (RaftConfigurationPreparation _ desired frontier ready) ->
    [ node
    | node <- raftVoterSetNodes desired,
      node `notElem` raftVotingConfigurationNodes (snd (effectiveConfiguration state)),
      Set.notMember node ready || Map.findWithDefault (raftLogIndex 0) node (stateMatchIndexes state) < raftCatchUpFrontierIndex frontier
    ]

learnerReadiness :: RaftCatchUpFrontier -> RaftState bytes -> Maybe RaftLearnerReadiness
learnerReadiness frontier state
  | stateAppliedThrough state >= raftCatchUpFrontierIndex frontier,
    stateTerm state <= raftCatchUpFrontierLeaderTerm frontier,
    matchingFrontier =
      Just (RaftLearnerReadiness frontier (checkedRaftLocalNode (stateGenesis state)))
  | otherwise = Nothing
  where
    matchingFrontier =
      raftLogTermAt (raftCatchUpFrontierIndex frontier) (stateLog state) == Just (raftCatchUpFrontierTerm frontier)
        || ( raftCatchUpFrontierIndex frontier < raftLogBaseIndex (stateLog state)
               && raftLogTermAt (raftLogBaseIndex (stateLog state)) (stateLog state) == Just (raftCatchUpFrontierTerm frontier)
               && fst (raftLogBaseConfiguration (stateLog state)) == raftCatchUpFrontierConfiguration frontier
           )

validateRaftState :: RaftState bytes -> Either RaftStateInvariantFault ()
validateRaftState state
  | not (raftLogIsContiguous (stateLog state)) = Left RaftStateLogNotContiguous
  | raftLogBaseIndex (stateLog state) == raftLogIndex 0, raftLogTermAt (raftLogIndex 0) (stateLog state) /= Just (raftTerm 0) = Left RaftStateSentinelMissing
  | Just (index, preceding, current) <- raftLogTermRegression (stateLog state) = Left (RaftStateLogTermRegression index preceding current)
  | otherwise = do
      validateRaftStateFields state
      validateConfigurations (snd (raftLogBaseConfiguration (stateLog state))) (raftLogEntries (stateLog state))
      if raftLogConfigurationIndexValid (initialConfiguration state) (stateLog state) then Right () else Left RaftStateConfigurationIndexInvalid
  where
    validateConfigurations _ [] = Right ()
    validateConfigurations preceding (entry : rest) = case raftLogEntryPayload entry of
      Configuration configuration _ -> do
        either (Left . RaftStateConfigurationTransitionInvalid (raftLogEntryIndex entry)) Right (validateRaftConfigurationSuccessor preceding configuration)
        validateConfigurations configuration rest
      _ -> validateConfigurations preceding rest

-- Log structure and configuration-chain evidence is retained by its opaque
-- admission owner. Timers, acknowledgements and status projections only inspect
-- these local transition fields; the full audit above remains explicit.
validateRaftStateFields :: RaftState bytes -> Either RaftStateInvariantFault ()
validateRaftStateFields state
  | not validCheckpoint = Left RaftStateCheckpointInvalid
  | raftLogLastTerm (stateLog state) > stateTerm state = Left (RaftStateLogTermAfterCurrentTerm (raftLogLastTerm (stateLog state)) (stateTerm state))
  | stateCommitIndex state > raftLogLastIndex (stateLog state) = Left RaftStateCommitAfterLog
  | stateExposedThrough state > stateCommitIndex state = Left RaftStateExposureAfterCommit
  | stateAppliedThrough state > stateExposedThrough state = Left RaftStateApplicationAckAfterExposure
  | Just vote <- Set.lookupMin (Set.difference (stateCandidateVotes state) voters) = Left (RaftStateUnknownCandidateVote vote)
  | stateRole state == RaftCandidate, stateVotedFor state /= (if localCanVote state then Just local else Nothing) = Left RaftStateCandidateDidNotVoteForSelf
  | stateRole state == RaftLeader, stateLeaderHint state /= Just local = Left RaftStateLeaderDoesNotNameSelf
  | stateRole state == RaftLeader, stateElectionActive state || not (stateHeartbeatActive state) = Left RaftStateLeaderTimerShapeInvalid
  | stateRole state /= RaftLeader, stateElectionActive state /= localCanCampaign state || stateHeartbeatActive state = Left RaftStateFollowerTimerShapeInvalid
  | not (stateRecentLeaderWindowActive state), stateRecentLeaderActive state || not (Set.null (stateFreshAcknowledgers state)) = Left RaftStateRecentLeaderTimerShapeInvalid
  | not validNoOp = Left RaftStateLeaderNoOpInvalid
  | Just (RaftPendingProposal _ index) <- statePendingProposal state, index <= stateCommitIndex state || isNothing (raftLogEntryAt index (stateLog state)) = Left (RaftStatePendingProposalInvalid index)
  | Just peer <- Set.lookupMin (Set.difference (Map.keysSet (stateNextIndexes state) `Set.union` Map.keysSet (stateMatchIndexes state)) (Set.insert local targets)) = Left (RaftStateUnknownProgressPeer peer)
  | Just peer <- Set.lookupMin (Set.difference (Map.keysSet (stateAwaitingResponses state)) targets) = Left (RaftStateUnknownAwaitingPeer peer)
  | Just (RaftConfigurationPreparation _ _ frontier _) <- stateConfigurationPreparation state,
    stateRole state /= RaftLeader || raftCatchUpFrontierLeader frontier /= local || raftCatchUpFrontierLeaderTerm frontier /= stateTerm state || raftCatchUpFrontierConfiguration frontier /= fst (effectiveConfiguration state) =
      Left RaftStateConfigurationPreparationInvalid
  | otherwise = Right ()
  where
    local = checkedRaftLocalNode (stateGenesis state)
    voters = Set.fromList (raftVotingConfigurationNodes (snd (effectiveConfiguration state)))
    targets = replicationTargets state
    base = raftLogBaseIndex (stateLog state)
    validCheckpoint =
      base <= stateCommitIndex state
        && base <= stateExposedThrough state
        && ( case stateCheckpoint state of
               Nothing -> base == raftLogIndex 0
               Just checkpoint ->
                 raftCheckpointIndex checkpoint == base
                   && Just (raftCheckpointTerm checkpoint) == raftLogTermAt base (stateLog state)
                   && (raftCheckpointConfigurationRef checkpoint, raftCheckpointConfiguration checkpoint) == raftLogBaseConfiguration (stateLog state)
           )
        && ( case stateInstallingCheckpoint state of
               Nothing -> stateAppliedThrough state >= base
               Just index -> index == base && stateAppliedThrough state < index
           )
    validNoOp = case (stateRole state, stateLeaderNoOpIndex state) of
      (RaftLeader, Just index)
        | index < base -> stateAppliedThrough state >= index && raftLogTermAt base (stateLog state) == Just (stateTerm state)
        | otherwise -> raftLogTermAt index (stateLog state) == Just (stateTerm state) && case raftLogEntryPayload <$> raftLogEntryAt index (stateLog state) of Just LeaderNoOp -> True; _ -> False
      (RaftLeader, Nothing) -> False
      (_, Nothing) -> True
      _ -> False
