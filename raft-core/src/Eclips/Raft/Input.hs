-- | Concrete, domain-independent inputs to the pure Raft kernel.
--
-- RPC constructors check their self-contained structural invariants.  The
-- protocol/runtime adapter additionally attaches the established source and, for
-- responses, the local dispatch generation of the matching request.  Checked
-- request histories never exceed their RPC term and entry terms do not decrease.
module Eclips.Raft.Input
  ( RaftEntry (..),
    RaftLogEntry,
    raftLogEntry,
    raftLogEntryIndex,
    raftLogEntryTerm,
    raftLogEntryPayload,
    RaftConflictHint,
    RaftConflictHintView (..),
    missingPrefixHint,
    conflictingTermHint,
    raftConflictHintView,
    RaftAppendResult (..),
    RaftRpc,
    RaftRpcView (..),
    requestVote,
    requestVoteResponse,
    appendEntries,
    appendEntriesResponse,
    installSnapshot,
    installSnapshotResponse,
    raftRpcView,
    raftRpcTerm,
    RaftInputFault (..),
    RaftInput,
    RaftInputView (..),
    observeRaftRequest,
    observeRaftResponse,
    selectElectionTimeout,
    fireElectionTimer,
    fireHeartbeatTimer,
    fireRecentLeaderTimer,
    proposeRaftApplication,
    acknowledgeCommittedEntries,
    checkpointRaftApplication,
    acknowledgeRaftCheckpoint,
    updateRaftReplicationTargets,
    beginRaftConfigurationChange,
    cancelRaftConfigurationChange,
    observeRaftLearnerReadiness,
    proposeRaftConfiguration,
    raftInputView,
  )
where

import Eclips.Raft.Checkpoint
import Eclips.Raft.Configuration
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
    raftLogIndex,
    raftLogIndexWord64,
    raftTermWord64,
  )
import Eclips.Raft.Internal.Configuration (validateRaftConfigurationSuccessor)

-- | Meaning of one non-sentinel log entry.
data RaftEntry bytes
  = LeaderNoOp
  | Application bytes
  | Configuration RaftVotingConfiguration bytes
  deriving stock (Eq, Show)

-- | One explicitly indexed log entry carried by AppendEntries.
data RaftLogEntry bytes
  = RaftLogEntry
      RaftLogIndex
      RaftTerm
      (RaftEntry bytes)
  deriving stock (Eq, Show)

raftLogEntry :: RaftLogIndex -> RaftTerm -> RaftEntry bytes -> RaftLogEntry bytes
raftLogEntry = RaftLogEntry

raftLogEntryIndex :: RaftLogEntry bytes -> RaftLogIndex
raftLogEntryIndex (RaftLogEntry index _ _) = index

raftLogEntryTerm :: RaftLogEntry bytes -> RaftTerm
raftLogEntryTerm (RaftLogEntry _ term _) = term

raftLogEntryPayload :: RaftLogEntry bytes -> RaftEntry bytes
raftLogEntryPayload (RaftLogEntry _ _ payload) = payload

-- | A structurally coherent log-conflict hint.
data RaftConflictHint
  = MissingPrefix RaftLogIndex
  | ConflictingTerm RaftTerm RaftLogIndex
  deriving stock (Eq, Show)

data RaftConflictHintView
  = MissingPrefixView RaftLogIndex
  | ConflictingTermView RaftTerm RaftLogIndex
  deriving stock (Eq, Show)

missingPrefixHint :: RaftLogIndex -> Either RaftInputFault RaftConflictHint
missingPrefixHint firstIndex
  | raftLogIndexWord64 firstIndex == 0 = Left RaftConflictFirstIndexIsZero
  | otherwise = Right (MissingPrefix firstIndex)

conflictingTermHint ::
  RaftTerm ->
  RaftLogIndex ->
  Either RaftInputFault RaftConflictHint
conflictingTermHint term firstIndex
  | raftTermWord64 term == 0 = Left RaftConflictTermIsZero
  | raftLogIndexWord64 firstIndex == 0 = Left RaftConflictFirstIndexIsZero
  | otherwise = Right (ConflictingTerm term firstIndex)

raftConflictHintView :: RaftConflictHint -> RaftConflictHintView
raftConflictHintView hint = case hint of
  MissingPrefix firstIndex -> MissingPrefixView firstIndex
  ConflictingTerm term firstIndex -> ConflictingTermView term firstIndex

-- | Result of one AppendEntries request.  Its sum shape makes success and
-- conflict fields mutually exclusive.
data RaftAppendResult
  = AppendAccepted RaftLogIndex RaftLogIndex
  | AppendRejected RaftConflictHint
  deriving stock (Eq, Show)

data RaftRpc bytes
  = RequestVoteRpc RaftTerm RaftNodeId RaftLogIndex RaftTerm
  | RequestVoteResponseRpc RaftTerm RaftNodeId Bool
  | AppendEntriesRpc
      RaftTerm
      RaftNodeId
      RaftLogIndex
      RaftTerm
      [RaftLogEntry bytes]
      RaftLogIndex
  | AppendEntriesResponseRpc RaftTerm RaftNodeId RaftAppendResult
  | InstallSnapshotRpc RaftTerm RaftNodeId (RaftCheckpoint bytes)
  | InstallSnapshotResponseRpc RaftTerm RaftNodeId RaftLogIndex Bool
  deriving stock (Eq, Show)

-- | Public read-only projection used by protocol codecs and tests.
data RaftRpcView bytes
  = RequestVoteView RaftTerm RaftNodeId RaftLogIndex RaftTerm
  | RequestVoteResponseView RaftTerm RaftNodeId Bool
  | AppendEntriesView
      RaftTerm
      RaftNodeId
      RaftLogIndex
      RaftTerm
      [RaftLogEntry bytes]
      RaftLogIndex
  | AppendEntriesResponseView RaftTerm RaftNodeId RaftAppendResult
  | InstallSnapshotView RaftTerm RaftNodeId (RaftCheckpoint bytes)
  | InstallSnapshotResponseView RaftTerm RaftNodeId RaftLogIndex Bool
  deriving stock (Eq, Show)

requestVote ::
  RaftTerm ->
  RaftNodeId ->
  RaftLogIndex ->
  RaftTerm ->
  Either RaftInputFault (RaftRpc bytes)
requestVote term candidate lastIndex lastTerm = do
  requirePositiveRpcTerm term
  requireCoherentIndexTerm lastIndex lastTerm
  if lastTerm <= term
    then pure ()
    else Left (RaftLastLogTermAfterRequestTerm lastTerm term)
  pure (RequestVoteRpc term candidate lastIndex lastTerm)

requestVoteResponse ::
  RaftTerm ->
  RaftNodeId ->
  Bool ->
  Either RaftInputFault (RaftRpc bytes)
requestVoteResponse term voter granted = do
  requirePositiveRpcTerm term
  pure (RequestVoteResponseRpc term voter granted)

appendEntries ::
  RaftTerm ->
  RaftNodeId ->
  RaftLogIndex ->
  RaftTerm ->
  [RaftLogEntry bytes] ->
  RaftLogIndex ->
  Either RaftInputFault (RaftRpc bytes)
appendEntries term leader previousIndex previousTerm entries leaderCommit = do
  requirePositiveRpcTerm term
  requireCoherentIndexTerm previousIndex previousTerm
  if previousTerm <= term
    then pure ()
    else Left (RaftPreviousLogTermAfterRequestTerm previousTerm term)
  requireContiguousEntries term previousIndex previousTerm entries
  requireConfigurationSequence previousIndex entries
  let lastSentIndex = case reverse entries of
        [] -> previousIndex
        entry : _ -> raftLogEntryIndex entry
  if leaderCommit <= lastSentIndex
    then
      pure
        ( AppendEntriesRpc
            term
            leader
            previousIndex
            previousTerm
            entries
            leaderCommit
        )
    else Left (RaftLeaderCommitAfterSentPrefix leaderCommit lastSentIndex)

appendEntriesResponse ::
  RaftTerm ->
  RaftNodeId ->
  RaftAppendResult ->
  Either RaftInputFault (RaftRpc bytes)
appendEntriesResponse term voter result = do
  requirePositiveRpcTerm term
  case result of
    AppendRejected hint -> case raftConflictHintView hint of
      ConflictingTermView conflictTerm _
        | conflictTerm > term ->
            Left (RaftConflictTermAfterResponseTerm conflictTerm term)
      _ -> pure ()
    AppendAccepted matched applied
      | applied > matched -> Left (RaftAppliedPrefixAfterMatch applied matched)
      | otherwise -> pure ()
  pure (AppendEntriesResponseRpc term voter result)

installSnapshot :: RaftTerm -> RaftNodeId -> RaftCheckpoint bytes -> Either RaftInputFault (RaftRpc bytes)
installSnapshot term leader checkpoint = do
  requirePositiveRpcTerm term
  if raftCheckpointTerm checkpoint > term
    then Left (RaftPreviousLogTermAfterRequestTerm (raftCheckpointTerm checkpoint) term)
    else Right (InstallSnapshotRpc term leader checkpoint)

installSnapshotResponse :: RaftTerm -> RaftNodeId -> RaftLogIndex -> Bool -> Either RaftInputFault (RaftRpc bytes)
installSnapshotResponse term voter index installed = do
  requirePositiveRpcTerm term
  if raftLogIndexWord64 index == 0 then Left RaftLogEntryIndexIsZero else Right (InstallSnapshotResponseRpc term voter index installed)

raftRpcView :: RaftRpc bytes -> RaftRpcView bytes
raftRpcView rpc = case rpc of
  RequestVoteRpc term candidate lastIndex lastTerm ->
    RequestVoteView term candidate lastIndex lastTerm
  RequestVoteResponseRpc term voter granted ->
    RequestVoteResponseView term voter granted
  AppendEntriesRpc term leader previousIndex previousTerm entries leaderCommit ->
    AppendEntriesView term leader previousIndex previousTerm entries leaderCommit
  AppendEntriesResponseRpc term voter result ->
    AppendEntriesResponseView term voter result
  InstallSnapshotRpc term leader checkpoint -> InstallSnapshotView term leader checkpoint
  InstallSnapshotResponseRpc term voter index installed -> InstallSnapshotResponseView term voter index installed

raftRpcTerm :: RaftRpc bytes -> RaftTerm
raftRpcTerm rpc = case rpc of
  RequestVoteRpc term _ _ _ -> term
  RequestVoteResponseRpc term _ _ -> term
  AppendEntriesRpc term _ _ _ _ _ -> term
  AppendEntriesResponseRpc term _ _ -> term
  InstallSnapshotRpc term _ _ -> term
  InstallSnapshotResponseRpc term _ _ _ -> term

data RaftInputFault
  = RaftRpcTermIsZero
  | RaftLogIndexTermShapeMismatch RaftLogIndex RaftTerm
  | RaftLastLogTermAfterRequestTerm RaftTerm RaftTerm
  | RaftPreviousLogTermAfterRequestTerm RaftTerm RaftTerm
  | RaftLogEntryIndexIsZero
  | RaftLogEntryTermIsZero RaftLogIndex
  | RaftLogEntryTermAfterRequestTerm RaftLogIndex RaftTerm RaftTerm
  | RaftLogEntryTermRegression RaftLogIndex RaftTerm RaftTerm
  | RaftLogEntriesNotContiguous RaftLogIndex RaftLogIndex
  | RaftLeaderCommitAfterSentPrefix RaftLogIndex RaftLogIndex
  | RaftConflictFirstIndexIsZero
  | RaftConflictTermIsZero
  | RaftConflictTermAfterResponseTerm RaftTerm RaftTerm
  | RaftAppliedPrefixAfterMatch RaftLogIndex RaftLogIndex
  | RaftRpcSourceMismatch RaftNodeId RaftNodeId
  | RaftExpectedRequestObservation
  | RaftExpectedResponseObservation
  | RaftReplicationTargetsNotStrictlyAscending
  | RaftConfigurationSequenceInvalid RaftLogIndex RaftConfigurationFault
  deriving stock (Eq, Show)

data RaftInput bytes
  = ObserveRequest RaftNodeId (RaftRpc bytes)
  | ObserveResponse RaftNodeId RaftDispatchGeneration (RaftRpc bytes)
  | SelectElectionTimeout
      RaftElectionTimerGeneration
      RaftDurationMicros
  | FireElectionTimer RaftElectionTimerGeneration
  | FireHeartbeatTimer RaftHeartbeatTimerGeneration
  | FireRecentLeaderTimer RaftRecentLeaderTimerGeneration
  | ProposeApplication RaftProposalId bytes
  | AcknowledgeCommittedEntries RaftLogIndex
  | CheckpointApplication RaftLogIndex bytes
  | AcknowledgeCheckpoint RaftLogIndex
  | UpdateReplicationTargets [RaftNodeId]
  | BeginConfigurationChange RaftProposalId RaftConfigurationRef RaftVoterSet
  | CancelConfigurationChange RaftProposalId
  | ObserveLearnerReadiness RaftLearnerReadiness
  | ProposeConfiguration RaftProposalId RaftConfigurationRef RaftVotingConfiguration bytes
  deriving stock (Eq, Show)

data RaftInputView bytes
  = ObserveRequestView RaftNodeId (RaftRpc bytes)
  | ObserveResponseView RaftNodeId RaftDispatchGeneration (RaftRpc bytes)
  | SelectElectionTimeoutView
      RaftElectionTimerGeneration
      RaftDurationMicros
  | FireElectionTimerView RaftElectionTimerGeneration
  | FireHeartbeatTimerView RaftHeartbeatTimerGeneration
  | FireRecentLeaderTimerView RaftRecentLeaderTimerGeneration
  | ProposeApplicationView RaftProposalId bytes
  | AcknowledgeCommittedEntriesView RaftLogIndex
  | CheckpointApplicationView RaftLogIndex bytes
  | AcknowledgeCheckpointView RaftLogIndex
  | UpdateReplicationTargetsView [RaftNodeId]
  | BeginConfigurationChangeView RaftProposalId RaftConfigurationRef RaftVoterSet
  | CancelConfigurationChangeView RaftProposalId
  | ObserveLearnerReadinessView RaftLearnerReadiness
  | ProposeConfigurationView RaftProposalId RaftConfigurationRef RaftVotingConfiguration bytes
  deriving stock (Eq, Show)

observeRaftRequest ::
  RaftNodeId ->
  RaftRpc bytes ->
  Either RaftInputFault (RaftInput bytes)
observeRaftRequest source rpc = case raftRpcView rpc of
  RequestVoteView _ candidate _ _ -> matchSource source candidate
  AppendEntriesView _ leader _ _ _ _ -> matchSource source leader
  InstallSnapshotView _ leader _ -> matchSource source leader
  RequestVoteResponseView {} -> Left RaftExpectedRequestObservation
  AppendEntriesResponseView {} -> Left RaftExpectedRequestObservation
  InstallSnapshotResponseView {} -> Left RaftExpectedRequestObservation
  where
    matchSource observed claimed
      | observed == claimed = Right (ObserveRequest observed rpc)
      | otherwise = Left (RaftRpcSourceMismatch observed claimed)

observeRaftResponse ::
  RaftNodeId ->
  RaftDispatchGeneration ->
  RaftRpc bytes ->
  Either RaftInputFault (RaftInput bytes)
observeRaftResponse source generation rpc = case raftRpcView rpc of
  RequestVoteResponseView _ voter _ -> matchSource source voter
  AppendEntriesResponseView _ voter _ -> matchSource source voter
  InstallSnapshotResponseView _ voter _ _ -> matchSource source voter
  RequestVoteView {} -> Left RaftExpectedResponseObservation
  AppendEntriesView {} -> Left RaftExpectedResponseObservation
  InstallSnapshotView {} -> Left RaftExpectedResponseObservation
  where
    matchSource observed claimed
      | observed == claimed = Right (ObserveResponse observed generation rpc)
      | otherwise = Left (RaftRpcSourceMismatch observed claimed)

selectElectionTimeout ::
  RaftElectionTimerGeneration ->
  RaftDurationMicros ->
  RaftInput bytes
selectElectionTimeout = SelectElectionTimeout

fireElectionTimer :: RaftElectionTimerGeneration -> RaftInput bytes
fireElectionTimer = FireElectionTimer

fireHeartbeatTimer :: RaftHeartbeatTimerGeneration -> RaftInput bytes
fireHeartbeatTimer = FireHeartbeatTimer

fireRecentLeaderTimer :: RaftRecentLeaderTimerGeneration -> RaftInput bytes
fireRecentLeaderTimer = FireRecentLeaderTimer

updateRaftReplicationTargets :: [RaftNodeId] -> Either RaftInputFault (RaftInput bytes)
updateRaftReplicationTargets nodes
  | and (zipWith (<) nodes (drop 1 nodes)) = Right (UpdateReplicationTargets nodes)
  | otherwise = Left RaftReplicationTargetsNotStrictlyAscending

beginRaftConfigurationChange :: RaftProposalId -> RaftConfigurationRef -> RaftVoterSet -> RaftInput bytes
beginRaftConfigurationChange = BeginConfigurationChange

cancelRaftConfigurationChange :: RaftProposalId -> RaftInput bytes
cancelRaftConfigurationChange = CancelConfigurationChange

observeRaftLearnerReadiness :: RaftLearnerReadiness -> RaftInput bytes
observeRaftLearnerReadiness = ObserveLearnerReadiness

proposeRaftConfiguration :: RaftProposalId -> RaftConfigurationRef -> RaftVotingConfiguration -> bytes -> RaftInput bytes
proposeRaftConfiguration = ProposeConfiguration

proposeRaftApplication :: RaftProposalId -> bytes -> RaftInput bytes
proposeRaftApplication = ProposeApplication

acknowledgeCommittedEntries :: RaftLogIndex -> RaftInput bytes
acknowledgeCommittedEntries = AcknowledgeCommittedEntries

-- | Capture bytes from the application's exact acknowledged prefix. Metadata
-- comes from the native log; this is neither a proposal nor an RPC observation.
-- Every successful transition reports the actual retained image through
-- 'Eclips.Raft.Effect.ReportRaftCheckpointOffer', including ignored offers.
checkpointRaftApplication :: RaftLogIndex -> bytes -> RaftInput bytes
checkpointRaftApplication = CheckpointApplication

-- | Complete an actual installation emitted by 'InstallRaftCheckpoint'.
acknowledgeRaftCheckpoint :: RaftLogIndex -> RaftInput bytes
acknowledgeRaftCheckpoint = AcknowledgeCheckpoint

raftInputView :: RaftInput bytes -> RaftInputView bytes
raftInputView input = case input of
  ObserveRequest source rpc -> ObserveRequestView source rpc
  ObserveResponse source generation rpc -> ObserveResponseView source generation rpc
  SelectElectionTimeout generation duration ->
    SelectElectionTimeoutView generation duration
  FireElectionTimer generation -> FireElectionTimerView generation
  FireHeartbeatTimer generation -> FireHeartbeatTimerView generation
  FireRecentLeaderTimer generation -> FireRecentLeaderTimerView generation
  ProposeApplication proposal payload -> ProposeApplicationView proposal payload
  AcknowledgeCommittedEntries index -> AcknowledgeCommittedEntriesView index
  CheckpointApplication index bytes -> CheckpointApplicationView index bytes
  AcknowledgeCheckpoint index -> AcknowledgeCheckpointView index
  UpdateReplicationTargets nodes -> UpdateReplicationTargetsView nodes
  BeginConfigurationChange proposal ref voters -> BeginConfigurationChangeView proposal ref voters
  CancelConfigurationChange proposal -> CancelConfigurationChangeView proposal
  ObserveLearnerReadiness readiness -> ObserveLearnerReadinessView readiness
  ProposeConfiguration proposal ref configuration metadata -> ProposeConfigurationView proposal ref configuration metadata

-- Validate every transition for which this RPC contains the predecessor. The
-- owner checks the first against its matched retained prefix before installing.
requireConfigurationSequence :: RaftLogIndex -> [RaftLogEntry bytes] -> Either RaftInputFault ()
requireConfigurationSequence previous = go Nothing
  where
    go _ [] = Right ()
    go preceding (entry : rest) = case raftLogEntryPayload entry of
      Configuration configuration _ -> do
        case preceding of
          Just old -> mapFault entry (validateRaftConfigurationSuccessor old configuration)
          Nothing -> case raftVotingConfigurationView configuration of
            StableRaftConfigurationView _ | previous == raftLogIndex 0 -> Left (RaftConfigurationSequenceInvalid (raftLogEntryIndex entry) RaftConfigurationExpectedJoint)
            JointRaftConfigurationView old new | old == new -> Left (RaftConfigurationSequenceInvalid (raftLogEntryIndex entry) RaftConfigurationUnchanged)
            _ -> Right ()
        go (Just configuration) rest
      _ -> go preceding rest
    mapFault entry = either (Left . RaftConfigurationSequenceInvalid (raftLogEntryIndex entry)) Right

requirePositiveRpcTerm :: RaftTerm -> Either RaftInputFault ()
requirePositiveRpcTerm term
  | raftTermWord64 term == 0 = Left RaftRpcTermIsZero
  | otherwise = Right ()

requireCoherentIndexTerm ::
  RaftLogIndex ->
  RaftTerm ->
  Either RaftInputFault ()
requireCoherentIndexTerm index term
  | (raftLogIndexWord64 index == 0) == (raftTermWord64 term == 0) = Right ()
  | otherwise = Left (RaftLogIndexTermShapeMismatch index term)

requireContiguousEntries ::
  RaftTerm ->
  RaftLogIndex ->
  RaftTerm ->
  [RaftLogEntry bytes] ->
  Either RaftInputFault ()
requireContiguousEntries requestTerm previousIndex previousTerm entries =
  go expected previousTerm entries
  where
    expected = raftLogIndex (raftLogIndexWord64 previousIndex + 1)

    go _ _ [] = Right ()
    go nextIndex precedingTerm (entry : remaining)
      | raftLogIndexWord64 (raftLogEntryIndex entry) == 0 =
          Left RaftLogEntryIndexIsZero
      | raftTermWord64 (raftLogEntryTerm entry) == 0 =
          Left (RaftLogEntryTermIsZero (raftLogEntryIndex entry))
      | raftLogEntryIndex entry /= nextIndex =
          Left (RaftLogEntriesNotContiguous nextIndex (raftLogEntryIndex entry))
      | raftLogEntryTerm entry > requestTerm =
          Left
            ( RaftLogEntryTermAfterRequestTerm
                (raftLogEntryIndex entry)
                (raftLogEntryTerm entry)
                requestTerm
            )
      | raftLogEntryTerm entry < precedingTerm =
          Left
            ( RaftLogEntryTermRegression
                (raftLogEntryIndex entry)
                precedingTerm
                (raftLogEntryTerm entry)
            )
      | otherwise =
          go
            (raftLogIndex (raftLogIndexWord64 nextIndex + 1))
            (raftLogEntryTerm entry)
            remaining
