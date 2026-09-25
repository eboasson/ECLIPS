-- | Native configuration construction shared only by the Raft owner.
module Eclips.Raft.Internal.Configuration
  ( RaftVoterSet (..),
    RaftVotingConfiguration (..),
    RaftVotingConfigurationView (..),
    RaftConfigurationRef (..),
    RaftConfigurationRefView (..),
    RaftConfigurationFault (..),
    raftVoterSet,
    raftVoterSetNodes,
    stableRaftConfiguration,
    jointRaftConfiguration,
    raftVotingConfigurationView,
    raftVotingConfigurationNodes,
    raftConfigurationHasQuorum,
    genesisRaftConfigurationRef,
    raftConfigurationEntryRef,
    raftConfigurationRefView,
    validateRaftConfigurationSuccessor,
    RaftCatchUpFrontier (..),
    raftCatchUpFrontierLeader,
    raftCatchUpFrontierLeaderTerm,
    raftCatchUpFrontierConfiguration,
    raftCatchUpFrontierIndex,
    raftCatchUpFrontierTerm,
    RaftLearnerReadiness (..),
    raftLearnerReadinessFrontier,
    raftLearnerReadinessNode,
    RaftParticipation (..),
  ) where

import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Raft.Identity

newtype RaftVoterSet = RaftVoterSet [RaftNodeId]
  deriving stock (Eq, Ord, Show)

data RaftVotingConfiguration
  = StableRaftConfiguration RaftVoterSet
  | JointRaftConfiguration RaftVoterSet RaftVoterSet
  deriving stock (Eq, Ord, Show)

data RaftVotingConfigurationView
  = StableRaftConfigurationView [RaftNodeId]
  | JointRaftConfigurationView [RaftNodeId] [RaftNodeId]
  deriving stock (Eq, Ord, Show)

data RaftConfigurationRef
  = GenesisRaftConfigurationRef
  | RaftConfigurationEntryRef RaftLogIndex RaftTerm
  deriving stock (Eq, Ord, Show)

data RaftConfigurationRefView
  = GenesisRaftConfigurationRefView
  | RaftConfigurationEntryRefView RaftLogIndex RaftTerm
  deriving stock (Eq, Ord, Show)

data RaftConfigurationFault
  = RaftConfigurationVotersEmpty
  | RaftConfigurationVotersNotStrictlyAscending
  | RaftConfigurationReferenceIndexIsZero
  | RaftConfigurationReferenceTermIsZero
  | RaftConfigurationExpectedJoint
  | RaftConfigurationExpectedStable
  | RaftConfigurationOldSetMismatch
  | RaftConfigurationFinalSetMismatch
  | RaftConfigurationUnchanged
  deriving stock (Eq, Show)

raftVoterSet :: [RaftNodeId] -> Either RaftConfigurationFault RaftVoterSet
raftVoterSet [] = Left RaftConfigurationVotersEmpty
raftVoterSet nodes
  | and (zipWith (<) nodes (drop 1 nodes)) = Right (RaftVoterSet nodes)
  | otherwise = Left RaftConfigurationVotersNotStrictlyAscending

raftVoterSetNodes :: RaftVoterSet -> [RaftNodeId]
raftVoterSetNodes (RaftVoterSet nodes) = nodes

stableRaftConfiguration :: RaftVoterSet -> RaftVotingConfiguration
stableRaftConfiguration = StableRaftConfiguration

jointRaftConfiguration :: RaftVoterSet -> RaftVoterSet -> RaftVotingConfiguration
jointRaftConfiguration = JointRaftConfiguration

raftVotingConfigurationView :: RaftVotingConfiguration -> RaftVotingConfigurationView
raftVotingConfigurationView (StableRaftConfiguration voters) = StableRaftConfigurationView (raftVoterSetNodes voters)
raftVotingConfigurationView (JointRaftConfiguration old new) = JointRaftConfigurationView (raftVoterSetNodes old) (raftVoterSetNodes new)

raftVotingConfigurationNodes :: RaftVotingConfiguration -> [RaftNodeId]
raftVotingConfigurationNodes (StableRaftConfiguration voters) = raftVoterSetNodes voters
raftVotingConfigurationNodes (JointRaftConfiguration old new) = Set.toAscList (Set.fromList (raftVoterSetNodes old <> raftVoterSetNodes new))

raftConfigurationHasQuorum :: RaftVotingConfiguration -> Set RaftNodeId -> Bool
raftConfigurationHasQuorum configuration acknowledgers = case configuration of
  StableRaftConfiguration voters -> majority voters
  JointRaftConfiguration old new -> majority old && majority new
  where
    majority voters = let members = Set.fromList (raftVoterSetNodes voters) in 2 * Set.size (Set.intersection members acknowledgers) > Set.size members

genesisRaftConfigurationRef :: RaftConfigurationRef
genesisRaftConfigurationRef = GenesisRaftConfigurationRef

raftConfigurationEntryRef :: RaftLogIndex -> RaftTerm -> Either RaftConfigurationFault RaftConfigurationRef
raftConfigurationEntryRef index term
  | raftLogIndexWord64 index == 0 = Left RaftConfigurationReferenceIndexIsZero
  | raftTermWord64 term == 0 = Left RaftConfigurationReferenceTermIsZero
  | otherwise = Right (RaftConfigurationEntryRef index term)

raftConfigurationRefView :: RaftConfigurationRef -> RaftConfigurationRefView
raftConfigurationRefView GenesisRaftConfigurationRef = GenesisRaftConfigurationRefView
raftConfigurationRefView (RaftConfigurationEntryRef index term) = RaftConfigurationEntryRefView index term

validateRaftConfigurationSuccessor :: RaftVotingConfiguration -> RaftVotingConfiguration -> Either RaftConfigurationFault ()
validateRaftConfigurationSuccessor preceding successor = case (preceding, successor) of
  (StableRaftConfiguration old, JointRaftConfiguration expected new)
    | old /= expected -> Left RaftConfigurationOldSetMismatch
    | old == new -> Left RaftConfigurationUnchanged
    | otherwise -> Right ()
  (JointRaftConfiguration _ new, StableRaftConfiguration final)
    | new /= final -> Left RaftConfigurationFinalSetMismatch
    | otherwise -> Right ()
  (StableRaftConfiguration {}, _) -> Left RaftConfigurationExpectedJoint
  (JointRaftConfiguration {}, _) -> Left RaftConfigurationExpectedStable

-- | A settled leader's exact native log and adapter frontier. Only the owner
-- captures this token; transport endpoints are not part of its authority.
data RaftCatchUpFrontier = RaftCatchUpFrontier RaftNodeId RaftTerm RaftConfigurationRef RaftLogIndex RaftTerm
  deriving stock (Eq, Show)

raftCatchUpFrontierLeader :: RaftCatchUpFrontier -> RaftNodeId
raftCatchUpFrontierLeader (RaftCatchUpFrontier leader _ _ _ _) = leader
raftCatchUpFrontierLeaderTerm :: RaftCatchUpFrontier -> RaftTerm
raftCatchUpFrontierLeaderTerm (RaftCatchUpFrontier _ term _ _ _) = term
raftCatchUpFrontierConfiguration :: RaftCatchUpFrontier -> RaftConfigurationRef
raftCatchUpFrontierConfiguration (RaftCatchUpFrontier _ _ ref _ _) = ref
raftCatchUpFrontierIndex :: RaftCatchUpFrontier -> RaftLogIndex
raftCatchUpFrontierIndex (RaftCatchUpFrontier _ _ _ index _) = index
raftCatchUpFrontierTerm :: RaftCatchUpFrontier -> RaftTerm
raftCatchUpFrontierTerm (RaftCatchUpFrontier _ _ _ _ term) = term

-- | An exact receiving owner's checked application of the captured prefix.
data RaftLearnerReadiness = RaftLearnerReadiness RaftCatchUpFrontier RaftNodeId
  deriving stock (Eq, Show)

raftLearnerReadinessFrontier :: RaftLearnerReadiness -> RaftCatchUpFrontier
raftLearnerReadinessFrontier (RaftLearnerReadiness frontier _) = frontier
raftLearnerReadinessNode :: RaftLearnerReadiness -> RaftNodeId
raftLearnerReadinessNode (RaftLearnerReadiness _ node) = node

data RaftParticipation = RaftVoter | RaftLearner | RaftRemoved
  deriving stock (Eq, Ord, Show)
