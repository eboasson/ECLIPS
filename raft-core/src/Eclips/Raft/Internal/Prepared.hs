module Eclips.Raft.Internal.Prepared
  ( RaftFault (..),
    PreparedRaftTransition,
    prepareRaftTransition,
    commitRaftTransition,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Raft.Checkpoint
import Eclips.Raft.Effect
import Eclips.Raft.Genesis
import Eclips.Raft.Identity
import Eclips.Raft.Input
import Eclips.Raft.Internal.Configuration
import Eclips.Raft.Internal.Log
import Eclips.Raft.Internal.State

data RaftFault
  = RaftPredecessorInvariantFault RaftStateInvariantFault
  | RaftSuccessorInvariantFault RaftStateInvariantFault
  | RaftLogInvariantFault RaftLogFault
  | RaftInternalInputConstructionFault RaftInputFault
  | RaftUnconfiguredPeer RaftNodeId
  | RaftRpcFromLocalNode
  | RaftElectionTimeoutOutsideGenesis
      RaftDurationMicros
      RaftDurationMicros
      RaftDurationMicros
  | RaftConflictingElectionTimeoutSelection
      RaftElectionTimerGeneration
      RaftDurationMicros
      RaftDurationMicros
  | RaftAppendResponseBeyondLocalLog RaftLogIndex RaftLogIndex
  | RaftAppendResponseDoesNotMatchAttempt RaftLogIndex RaftLogIndex
  | RaftEntryAcknowledgementAhead RaftLogIndex RaftLogIndex
  | RaftEntryAcknowledgementOutOfOrder RaftLogIndex RaftLogIndex
  | RaftMissingPreviousLogTerm RaftLogIndex
  | RaftCheckpointConstructionFault RaftCheckpointFault
  | RaftCheckpointBeforeApplication RaftLogIndex RaftLogIndex
  | RaftUnexpectedCheckpointAcknowledgement RaftLogIndex
  deriving stock (Eq, Show)

data PreparedRaftTransition bytes
  = PreparedRaftTransition
      (RaftState bytes)
      (RaftEffectBatch bytes)

prepareRaftTransition ::
  (Eq bytes) =>
  RaftInput bytes ->
  RaftState bytes ->
  Either RaftFault (PreparedRaftTransition bytes)
prepareRaftTransition input predecessor = do
  mapLeft RaftPredecessorInvariantFault (validateRaftStateFields predecessor)
  (applied, initialEffects) <- applyInput (raftInputView input) predecessor
  (successor, finalEffects) <- finishCommittedExclusion applied
  let checkpointReports = case raftInputView input of
        CheckpointApplicationView offered _ -> [ReportRaftCheckpointOffer offered (stateCheckpoint successor)]
        _ -> []
      effects = initialEffects <> finalEffects <> changedPreparationEffects predecessor successor <> checkpointReports
  mapLeft RaftSuccessorInvariantFault (validateRaftStateFields successor)
  let diagnostics = transitionDiagnostics predecessor successor
  pure
    ( PreparedRaftTransition
        successor
        (sealRaftBatch (effects <> diagnostics))
    )

commitRaftTransition ::
  PreparedRaftTransition bytes ->
  (RaftState bytes, RaftEffectBatch bytes)
commitRaftTransition (PreparedRaftTransition state effects) = (state, effects)

applyInput ::
  (Eq bytes) =>
  RaftInputView bytes ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
applyInput input state = case input of
  ObserveRequestView source rpc -> handleRequest source rpc state
  ObserveResponseView source generation rpc ->
    handleResponse source generation rpc state
  SelectElectionTimeoutView generation duration ->
    handleElectionTimeoutSelection generation duration state
  FireElectionTimerView generation -> handleElectionTimer generation state
  FireHeartbeatTimerView generation -> handleHeartbeatTimer generation state
  FireRecentLeaderTimerView generation -> pure (expireRecentLeader generation state)
  UpdateReplicationTargetsView targets -> handleTargets targets state
  BeginConfigurationChangeView proposal ref desired -> handleBeginConfiguration proposal ref desired state
  CancelConfigurationChangeView proposal -> pure (handleCancelConfiguration proposal state)
  ObserveLearnerReadinessView readiness -> pure (handleLearnerReadiness readiness state)
  ProposeConfigurationView proposal ref configuration metadata -> handleConfigurationProposal proposal ref configuration metadata state
  ProposeApplicationView proposal payload -> handleProposal proposal payload state
  AcknowledgeCommittedEntriesView index ->
    handleApplicationAcknowledgement index state
  CheckpointApplicationView index payload -> handleLocalCheckpoint index payload state
  AcknowledgeCheckpointView index -> handleCheckpointAcknowledgement index state

handleElectionTimeoutSelection ::
  RaftElectionTimerGeneration ->
  RaftDurationMicros ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleElectionTimeoutSelection generation duration state
  | not (stateElectionActive state) = pure (state, [])
  | generation /= stateElectionGeneration state = pure (state, [])
  | duration < lower || duration > upper =
      Left (RaftElectionTimeoutOutsideGenesis duration lower upper)
  | otherwise = case stateElectionDuration state of
      Nothing ->
        pure
          ( state {stateElectionDuration = Just duration},
            [ArmElectionTimer generation duration]
          )
      Just selected
        | selected == duration -> pure (state, [])
        | otherwise ->
            Left
              ( RaftConflictingElectionTimeoutSelection
                  generation
                  selected
                  duration
              )
  where
    configuration = checkedRaftNativeConfiguration (stateGenesis state)
    lower = raftNativeElectionTimeoutLower configuration
    upper = raftNativeElectionTimeoutUpper configuration

handleElectionTimer ::
  RaftElectionTimerGeneration ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleElectionTimer generation state
  | not (stateElectionActive state) = pure (state, [])
  | generation /= stateElectionGeneration state = pure (state, [])
  | stateElectionDuration state == Nothing = pure (state, [])
  | not (localCanCampaign state) = pure (state, [])
  | otherwise = startElection state

startElection ::
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
startElection state = do
  let (unguarded, guardEffects) = clearRecentLeader state
      local = localNode state
      newTerm = raftTerm (raftTermWord64 (stateTerm state) + 1)
      base =
        unguarded
          { stateTerm = newTerm,
            stateVotedFor = if localCanVote state then Just local else Nothing,
            stateRole = RaftCandidate,
            stateLeaderHint = Nothing,
            stateCandidateVotes = if localCanVote state then Set.singleton local else Set.empty,
            stateNextIndexes = Map.empty,
            stateMatchIndexes = Map.empty,
            stateAwaitingResponses = Map.empty,
            stateLeaderNoOpIndex = Nothing,
            statePendingProposal = Nothing,
            stateConfigurationPreparation = Nothing,
            stateFreshAcknowledgers = Set.empty
          }
  if hasQuorum base (stateCandidateVotes base)
    then do
      (leader, effects) <- becomeLeader base
      pure (leader, guardEffects <> effects)
    else do
      let (timed, timerEffects) = resetElectionTimer base
      voteRpc <-
        mapLeft
          RaftInternalInputConstructionFault
          (requestVote newTerm local (raftLogLastIndex (stateLog timed)) (raftLogLastTerm (stateLog timed)))
      let (successor, sendEffects) =
            sendRequestsToPeers (AwaitVoteResponse newTerm) voteRpc timed
      pure (successor, guardEffects <> timerEffects <> sendEffects)

handleHeartbeatTimer ::
  RaftHeartbeatTimerGeneration ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleHeartbeatTimer generation state
  | stateRole state /= RaftLeader = pure (state, [])
  | not (stateHeartbeatActive state) = pure (state, [])
  | generation /= stateHeartbeatGeneration state = pure (state, [])
  | otherwise = do
      let nextGeneration =
            raftHeartbeatTimerGeneration
              (raftHeartbeatTimerGenerationWord64 generation + 1)
          heartbeat =
            raftNativeHeartbeatInterval
              (checkedRaftNativeConfiguration (stateGenesis state))
          rearmed = state {stateHeartbeatGeneration = nextGeneration}
      let (acknowledged, guardEffects) = recordLocalQuorumAcknowledgement rearmed
      (successor, sendEffects) <- sendAppendEntriesToAll acknowledged
      pure
        ( successor,
          SupersedeHeartbeatTimer generation
            : guardEffects
              <> sendEffects
              <> [ArmHeartbeatTimer nextGeneration heartbeat]
        )

handleProposal ::
  RaftProposalId ->
  bytes ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleProposal proposal payload state
  | stateRole state /= RaftLeader =
      pure
        ( state,
          [ ReportRaftProposal
              proposal
              (RaftProposalRedirected (stateLeaderHint state) (stateTerm state))
          ]
        )
  | not (raftStateIsServiceReady state) =
      pure
        ( state,
          [ReportRaftProposal proposal (RaftProposalNotReady (stateTerm state))]
        )
  | otherwise = do
      (newLog, entry) <- mapLogFailure (raftLogAppend (stateTerm state) (Application payload) (stateLog state))
      let index = raftLogEntryIndex entry
          local = localNode state
          appended =
            state
              { stateLog = newLog,
                stateMatchIndexes = Map.insert local index (stateMatchIndexes state),
                statePendingProposal = Just (RaftPendingProposal proposal index)
              }
      let (acknowledged, guardEffects) = recordLocalQuorumAcknowledgement appended
      (committed, commitEffects) <- advanceCommittedPrefix (leaderCommitCandidate acknowledged) acknowledged
      (successor, sendEffects) <- sendAppendEntriesToAll committed
      pure
        ( successor,
          ReportRaftProposal proposal (RaftProposalAppended index) : guardEffects <> commitEffects <> sendEffects
        )

handleApplicationAcknowledgement :: RaftLogIndex -> RaftState bytes -> Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleApplicationAcknowledgement index state
  | index <= stateAppliedThrough state = pure (state, [])
  | index > stateExposedThrough state = Left (RaftEntryAcknowledgementAhead index (stateExposedThrough state))
  | index /= successorIndex (stateAppliedThrough state) = Left (RaftEntryAcknowledgementOutOfOrder (successorIndex (stateAppliedThrough state)) index)
  | otherwise = pure (state {stateAppliedThrough = index}, [])

handleLocalCheckpoint :: RaftLogIndex -> bytes -> RaftState bytes -> Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleLocalCheckpoint index payload state
  | index <= raftLogBaseIndex (stateLog state) = pure (state, [])
  | index > stateAppliedThrough state = Left (RaftCheckpointBeforeApplication index (stateAppliedThrough state))
  | Just (RaftConfigurationPreparation _ _ frontier _) <- stateConfigurationPreparation state,
    index > raftCatchUpFrontierIndex frontier =
      pure (state, [])
  | otherwise = do
      term <- maybe (Left (RaftMissingPreviousLogTerm index)) Right (raftLogTermAt index (stateLog state))
      let (reference, configuration) = configurationAt index state
      checkpoint <- mapLeft RaftCheckpointConstructionFault (raftCheckpoint index term reference configuration payload)
      compacted <- mapLogFailure (raftLogInstallCheckpoint checkpoint (stateLog state))
      pure (state {stateLog = compacted, stateCheckpoint = Just checkpoint}, [])

handleCheckpointAcknowledgement :: RaftLogIndex -> RaftState bytes -> Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleCheckpointAcknowledgement index state
  | index <= stateAppliedThrough state = pure (state, [])
  | stateInstallingCheckpoint state == Just index = pure (state {stateAppliedThrough = index, stateInstallingCheckpoint = Nothing}, [])
  | otherwise = Left (RaftUnexpectedCheckpointAcknowledgement index)

handleRequest ::
  (Eq bytes) =>
  RaftNodeId ->
  RaftRpc bytes ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleRequest source rpc state = do
  requireRemotePeer source state
  case raftRpcView rpc of
    RequestVoteView term candidate lastIndex lastTerm ->
      handleRequestVote source term candidate lastIndex lastTerm state
    AppendEntriesView term leader previousIndex previousTerm entries leaderCommit ->
      handleAppendEntries
        source
        term
        leader
        previousIndex
        previousTerm
        entries
        leaderCommit
        state
    InstallSnapshotView term _leader checkpoint -> handleInstallSnapshot source term checkpoint state
    RequestVoteResponseView {} -> pure (state, [])
    AppendEntriesResponseView {} -> pure (state, [])
    InstallSnapshotResponseView {} -> pure (state, [])

handleResponse ::
  RaftNodeId ->
  RaftDispatchGeneration ->
  RaftRpc bytes ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleResponse source generation rpc state = do
  requireRemotePeer source state
  if not (applicableResponse source generation rpc state)
    then pure (state, [])
    else
      if rpcTerm rpc > stateTerm state
        then stepDown (rpcTerm rpc) Nothing state
        else
          if rpcTerm rpc < stateTerm state
            then pure (state, [])
            else case raftRpcView rpc of
              RequestVoteResponseView term voter granted ->
                handleRequestVoteResponse source generation term voter granted state
              AppendEntriesResponseView term voter result ->
                handleAppendEntriesResponse source generation term voter result state
              InstallSnapshotResponseView term _voter index installed -> handleInstallSnapshotResponse source generation term index installed state
              RequestVoteView {} -> pure (state, [])
              AppendEntriesView {} -> pure (state, [])
              InstallSnapshotView {} -> pure (state, [])

handleRequestVote ::
  RaftNodeId ->
  RaftTerm ->
  RaftNodeId ->
  RaftLogIndex ->
  RaftTerm ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleRequestVote source requestTerm _candidate candidateLastIndex candidateLastTerm state
  | stateRecentLeaderActive state = sendVoteResponse source False state
  | requestTerm < stateTerm state = sendVoteResponse source False state
  | otherwise = do
      let canVote =
            requestTerm > stateTerm state
              || case stateVotedFor state of
                Nothing -> True
                Just previous -> previous == source
          upToDate =
            candidateLastTerm > raftLogLastTerm (stateLog state)
              || ( candidateLastTerm == raftLogLastTerm (stateLog state)
                     && candidateLastIndex >= raftLogLastIndex (stateLog state)
                 )
          granted = localCanVote state && canVote && upToDate
      (termState, termEffects) <-
        if requestTerm > stateTerm state
          then
            if granted
              then stepDown requestTerm Nothing state
              else stepDownPreservingActiveElectionDeadline requestTerm Nothing state
          else pure (state, [])
      let
        voted =
          if granted
            then
              termState
                { stateVotedFor = Just source,
                  stateRole = RaftFollower,
                  stateLeaderHint = Nothing,
                  stateCandidateVotes = Set.empty
                }
            else termState
        (timed, timerEffects) =
          if granted && requestTerm == stateTerm state
            then resetElectionTimer voted
            else (voted, [])
      (successor, responseEffects) <- sendVoteResponse source granted timed
      pure (successor, termEffects <> timerEffects <> responseEffects)

sendVoteResponse ::
  RaftNodeId ->
  Bool ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
sendVoteResponse target granted state = do
  rpc <-
    mapLeft
      RaftInternalInputConstructionFault
      (requestVoteResponse (stateTerm state) (localNode state) granted)
  let (successor, effect) = sendRpc target Nothing rpc state
  pure (successor, [effect])

handleRequestVoteResponse ::
  RaftNodeId ->
  RaftDispatchGeneration ->
  RaftTerm ->
  RaftNodeId ->
  Bool ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleRequestVoteResponse source generation term _voter granted state =
  case Map.lookup source (stateAwaitingResponses state) of
    Just (expectedGeneration, AwaitVoteResponse expectedTerm)
      | generation == expectedGeneration,
        term == expectedTerm,
        stateRole state == RaftCandidate -> do
          let withoutAttempt =
                state
                  { stateAwaitingResponses =
                      Map.delete source (stateAwaitingResponses state)
                  }
              voted =
                if granted && source `elem` voters withoutAttempt
                  then
                    withoutAttempt
                      { stateCandidateVotes =
                          Set.insert source (stateCandidateVotes withoutAttempt)
                      }
                  else withoutAttempt
          if hasQuorum voted (stateCandidateVotes voted)
            then becomeLeader voted
            else pure (voted, [])
    _ -> pure (state, [])

becomeLeader ::
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
becomeLeader state = do
  (newLog, noOpEntry) <- mapLogFailure (raftLogAppend (stateTerm state) LeaderNoOp (stateLog state))
  let local = localNode state
      noOpIndex = raftLogEntryIndex noOpEntry
      firstNext = noOpIndex
      peerNodes = peers state
      nextIndexes = Map.fromList [(peer, firstNext) | peer <- peerNodes]
      matchIndexes =
        Map.fromList
          ((local, noOpIndex) : [(peer, raftLogIndex 0) | peer <- peerNodes])
      heartbeatGeneration =
        raftHeartbeatTimerGeneration
          (raftHeartbeatTimerGenerationWord64 (stateHeartbeatGeneration state) + 1)
      heartbeat =
        raftNativeHeartbeatInterval
          (checkedRaftNativeConfiguration (stateGenesis state))
      leader =
        state
          { stateLog = newLog,
            stateRole = RaftLeader,
            stateLeaderHint = Just local,
            stateCandidateVotes = Set.empty,
            stateNextIndexes = nextIndexes,
            stateMatchIndexes = matchIndexes,
            stateElectionActive = False,
            stateElectionDuration = Nothing,
            stateHeartbeatGeneration = heartbeatGeneration,
            stateHeartbeatActive = True,
            stateAwaitingResponses = Map.empty,
            stateLeaderNoOpIndex = Just noOpIndex,
            statePendingProposal = Nothing
          }
      timerEffects =
        [ SupersedeElectionTimer (stateElectionGeneration state),
          ArmHeartbeatTimer heartbeatGeneration heartbeat
        ]
  let (acknowledged, guardEffects) = recordLocalQuorumAcknowledgement leader
  (committed, commitEffects) <- advanceCommittedPrefix (leaderCommitCandidate acknowledged) acknowledged
  (successor, sendEffects) <- sendAppendEntriesToAll committed
  pure (successor, timerEffects <> guardEffects <> commitEffects <> sendEffects)

handleAppendEntries ::
  (Eq bytes) =>
  RaftNodeId ->
  RaftTerm ->
  RaftNodeId ->
  RaftLogIndex ->
  RaftTerm ->
  [RaftLogEntry bytes] ->
  RaftLogIndex ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleAppendEntries source requestTerm _leader previousIndex previousTerm entries leaderCommit state
  | requestTerm < stateTerm state = sendAppendRejection source state
  | otherwise = do
      (contacted, roleEffects) <- prepareFollowerForAppend source requestTerm state
      let (follower, guardEffects) = renewRecentLeader contacted
      case raftLogTermAt previousIndex (stateLog follower) of
        Nothing -> do
          hint <-
            mapLeft
              RaftInternalInputConstructionFault
              (missingPrefixHint (if previousIndex < raftLogBaseIndex (stateLog follower) then successorIndex (raftLogBaseIndex (stateLog follower)) else nextLogIndex follower))
          sendAppendResult source (AppendRejected hint) follower (roleEffects <> guardEffects)
        Just localTerm
          | localTerm /= previousTerm -> do
              firstIndex <- case raftLogFirstIndexOfTerm localTerm (stateLog follower) of
                Nothing -> Left (RaftMissingPreviousLogTerm previousIndex)
                Just index -> Right index
              hint <-
                mapLeft
                  RaftInternalInputConstructionFault
                  (conflictingTermHint localTerm firstIndex)
              sendAppendResult source (AppendRejected hint) follower (roleEffects <> guardEffects)
          | otherwise -> do
              mergedLog <-
                mapLogFailure
                  (raftLogMerge (stateCommitIndex follower) entries (stateLog follower))
              let matchedIndex = case reverse entries of
                    [] -> previousIndex
                    entry : _ -> raftLogEntryIndex entry
                  appended = follower {stateLog = mergedLog}
              (installed, configurationEffects) <- reconcileConfiguration follower appended
              let newCommit = max (stateCommitIndex installed) (min leaderCommit (raftLogLastIndex mergedLog))
              (committed, commitEffects) <- advanceCommittedPrefix newCommit installed
              (settled, participationEffects) <- reconcileParticipation committed
              sendAppendResult
                source
                (AppendAccepted matchedIndex (min matchedIndex (stateAppliedThrough settled)))
                settled
                (roleEffects <> guardEffects <> configurationEffects <> commitEffects <> participationEffects)

handleInstallSnapshot :: RaftNodeId -> RaftTerm -> RaftCheckpoint bytes -> RaftState bytes -> Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleInstallSnapshot source requestTerm checkpoint state
  | requestTerm < stateTerm state = respond False state []
  | otherwise = do
      (contacted, roleEffects) <- prepareFollowerForAppend source requestTerm state
      let (follower, guardEffects) = renewRecentLeader contacted
          preceding = roleEffects <> guardEffects
      if index <= stateCommitIndex follower
        then respond (index <= stateAppliedThrough follower) follower preceding
        else case stateInstallingCheckpoint follower of
          Just _ -> respond False follower preceding
          Nothing -> do
            installedLog <- mapLogFailure (raftLogInstallCheckpoint checkpoint (stateLog follower))
            let installing =
                  follower
                    { stateLog = installedLog,
                      stateCheckpoint = Just checkpoint,
                      stateInstallingCheckpoint = Just index,
                      stateCommitIndex = index,
                      stateExposedThrough = index
                    }
            (configured, configurationEffects) <- reconcileConfiguration follower installing
            (successor, participationEffects) <- reconcileParticipation configured
            respond True successor (preceding <> configurationEffects <> participationEffects <> [InstallRaftCheckpoint checkpoint])
  where
    index = raftCheckpointIndex checkpoint
    -- The dispatcher must finish the preceding application installation before
    -- interpreting this success. The applied cursor advances only on its ack.
    respond installed successor preceding = do
      rpc <- mapLeft RaftInternalInputConstructionFault (installSnapshotResponse (stateTerm successor) (localNode successor) index installed)
      let (sent, effect) = sendRpc source Nothing rpc successor
      pure (sent, preceding <> [effect])

handleInstallSnapshotResponse :: RaftNodeId -> RaftDispatchGeneration -> RaftTerm -> RaftLogIndex -> Bool -> RaftState bytes -> Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleInstallSnapshotResponse source generation term index installed state =
  case Map.lookup source (stateAwaitingResponses state) of
    Just (expectedGeneration, AwaitSnapshotResponse expectedTerm expectedIndex)
      | generation == expectedGeneration,
        term == expectedTerm,
        stateRole state == RaftLeader ->
          if index /= expectedIndex
            then Left (RaftAppendResponseDoesNotMatchAttempt expectedIndex index)
            else
              let withoutAttempt = state {stateAwaitingResponses = Map.delete source (stateAwaitingResponses state)}
               in if installed
                    then handleAppendAccepted source generation index index expectedIndex withoutAttempt
                    else pure (withoutAttempt, [])
    _ -> pure (state, [])

prepareFollowerForAppend ::
  RaftNodeId ->
  RaftTerm ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
prepareFollowerForAppend leader term state
  | term > stateTerm state = stepDown term (Just leader) state
  | stateRole state /= RaftFollower = stepDown term (Just leader) state
  | otherwise =
      let (timed, timerEffects) =
            resetElectionTimer (state {stateLeaderHint = Just leader})
       in pure (timed, timerEffects)

sendAppendRejection ::
  RaftNodeId ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
sendAppendRejection source state = do
  hint <-
    mapLeft
      RaftInternalInputConstructionFault
      (missingPrefixHint (nextLogIndex state))
  sendAppendResult source (AppendRejected hint) state []

sendAppendResult ::
  RaftNodeId ->
  RaftAppendResult ->
  RaftState bytes ->
  [RaftEffect bytes] ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
sendAppendResult target result state preceding = do
  rpc <-
    mapLeft
      RaftInternalInputConstructionFault
      (appendEntriesResponse (stateTerm state) (localNode state) result)
  let (successor, response) = sendRpc target Nothing rpc state
  pure (successor, preceding <> [response])

handleAppendEntriesResponse ::
  RaftNodeId ->
  RaftDispatchGeneration ->
  RaftTerm ->
  RaftNodeId ->
  RaftAppendResult ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleAppendEntriesResponse source generation term _voter result state =
  case Map.lookup source (stateAwaitingResponses state) of
    Just (expectedGeneration, AwaitAppendResponse expectedTerm _previousIndex lastSentIndex)
      | generation == expectedGeneration,
        term == expectedTerm,
        stateRole state == RaftLeader ->
          let withoutAttempt =
                state
                  { stateAwaitingResponses =
                      Map.delete source (stateAwaitingResponses state)
                  }
           in case result of
                AppendAccepted matchIndex appliedIndex ->
                  handleAppendAccepted source generation matchIndex appliedIndex lastSentIndex withoutAttempt
                AppendRejected hint ->
                  handleAppendRejected source hint withoutAttempt
    _ -> pure (state, [])

handleAppendAccepted ::
  RaftNodeId ->
  RaftDispatchGeneration ->
  RaftLogIndex ->
  RaftLogIndex ->
  RaftLogIndex ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleAppendAccepted source generation matchIndex appliedIndex lastSentIndex state
  | matchIndex > raftLogLastIndex (stateLog state) =
      Left (RaftAppendResponseBeyondLocalLog matchIndex (raftLogLastIndex (stateLog state)))
  | matchIndex /= lastSentIndex =
      Left (RaftAppendResponseDoesNotMatchAttempt lastSentIndex matchIndex)
  | otherwise = do
      let previousMatch = Map.findWithDefault (raftLogIndex 0) source (stateMatchIndexes state)
          installedMatch = max previousMatch matchIndex
          progressed =
            state
              { stateMatchIndexes =
                  Map.insert source installedMatch (stateMatchIndexes state),
                stateNextIndexes =
                  Map.insert source (successorIndex installedMatch) (stateNextIndexes state)
              }
          -- This response was admitted for the exact current-term dispatch
          -- and matched prefix. Its applied cursor came from the remote owner.
          ready = case stateConfigurationPreparation progressed of
            Just (RaftConfigurationPreparation _ _ frontier _)
              | appliedIndex >= raftCatchUpFrontierIndex frontier ->
                  fst (handleLearnerReadiness (RaftLearnerReadiness frontier source) progressed)
            _ -> progressed
          (contacted, guardEffects) = recordQuorumAcknowledgement source generation ready
          newCommit = leaderCommitCandidate contacted
      (committed, committedEffects) <- advanceCommittedPrefix newCommit contacted
      let commitEffects = guardEffects <> committedEffects
      if stateCommitIndex committed > stateCommitIndex state
        then do
          (broadcasted, sendEffects) <- sendAppendEntriesToAll committed
          pure (broadcasted, commitEffects <> sendEffects)
        else
          if Map.findWithDefault (nextLogIndex committed) source (stateNextIndexes committed)
            <= raftLogLastIndex (stateLog committed)
            then do
              (successor, sendEffect) <- sendAppendEntriesTo source committed
              pure (successor, commitEffects <> [sendEffect])
            else pure (committed, commitEffects)

handleAppendRejected ::
  RaftNodeId ->
  RaftConflictHint ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleAppendRejected source hint state = do
  let hintedIndex = case raftConflictHintView hint of
        MissingPrefixView firstIndex -> firstIndex
        ConflictingTermView conflictTerm firstIndex ->
          case raftLogLastIndexOfTerm conflictTerm (stateLog state) of
            Nothing -> firstIndex
            Just lastWithTerm -> successorIndex lastWithTerm
      boundedIndex =
        max
          (raftLogIndex 1)
          (min hintedIndex (nextLogIndex state))
      repaired =
        state
          { stateNextIndexes =
              Map.insert source boundedIndex (stateNextIndexes state)
          }
  (successor, effect) <- sendAppendEntriesTo source repaired
  pure (successor, [effect])

advanceCommittedPrefix ::
  RaftLogIndex ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
advanceCommittedPrefix requested state
  | requested <= stateCommitIndex state = pure (state, [])
  | requested > raftLogLastIndex (stateLog state) =
      Left (RaftAppendResponseBeyondLocalLog requested (raftLogLastIndex (stateLog state)))
  | otherwise =
      let applications =
            raftLogCommittedEntriesBetween
              (stateExposedThrough state)
              requested
              (stateLog state)
          committed =
            state
              { stateCommitIndex = requested,
                stateExposedThrough = requested
              }
          (settled, proposalEffects) = settleCommittedProposal committed
          exposureEffects = case NonEmpty.nonEmpty applications of
            Nothing -> []
            Just entries -> [ExposeCommittedEntries entries]
       in pure (settled, exposureEffects <> proposalEffects)

settleCommittedProposal ::
  RaftState bytes ->
  (RaftState bytes, [RaftEffect bytes])
settleCommittedProposal state = case statePendingProposal state of
  Just (RaftPendingProposal proposal index)
    | index <= stateCommitIndex state ->
        ( state {statePendingProposal = Nothing},
          [ReportRaftProposal proposal (RaftProposalCommitted index)]
        )
  _ -> (state, [])

leaderCommitCandidate :: RaftState bytes -> RaftLogIndex
leaderCommitCandidate state =
  foldl' max (stateCommitIndex state) eligible
  where
    currentTerm = stateTerm state
    currentCommit = stateCommitIndex state
    entries = raftLogEntriesFrom (successorIndex currentCommit) (stateLog state)
    eligible =
      [ index
      | entry <- entries,
        let index = raftLogEntryIndex entry,
        index > currentCommit,
        raftLogTermAt index (stateLog state) == Just currentTerm,
        hasQuorum state (replicatedVoters index state)
      ]

replicatedVoters :: RaftLogIndex -> RaftState bytes -> Set.Set RaftNodeId
replicatedVoters index state =
  Set.fromList
    [ voter
    | voter <- voters state,
      Map.findWithDefault (raftLogIndex 0) voter (stateMatchIndexes state) >= index
    ]

stepDown ::
  RaftTerm ->
  Maybe RaftNodeId ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
stepDown newTerm leader state =
  let (follower, roleEffects) = followerAfterStepDown newTerm leader state
      (timed, electionEffects) = resetElectionTimer follower
   in pure (timed, roleEffects <> electionEffects)

stepDownPreservingActiveElectionDeadline ::
  RaftTerm ->
  Maybe RaftNodeId ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
stepDownPreservingActiveElectionDeadline newTerm leader state =
  let (follower, roleEffects) = followerAfterStepDown newTerm leader state
   in if stateElectionActive state
        then pure (follower, roleEffects)
        else do
          let (timed, electionEffects) = resetElectionTimer follower
          pure (timed, roleEffects <> electionEffects)

followerAfterStepDown ::
  RaftTerm ->
  Maybe RaftNodeId ->
  RaftState bytes ->
  (RaftState bytes, [RaftEffect bytes])
followerAfterStepDown newTerm leader state =
  let termAdvanced = newTerm > stateTerm state
      pendingEffects = case statePendingProposal state of
        Nothing -> []
        Just (RaftPendingProposal proposal _) ->
          [ ReportRaftProposal
              proposal
              (RaftProposalRedirected leader newTerm)
          ]
      heartbeatEffects =
        if stateHeartbeatActive state
          then [SupersedeHeartbeatTimer (stateHeartbeatGeneration state)]
          else []
      follower =
        state
          { stateTerm = max newTerm (stateTerm state),
            stateVotedFor = if termAdvanced then Nothing else stateVotedFor state,
            stateRole = RaftFollower,
            stateLeaderHint = leader,
            stateCandidateVotes = Set.empty,
            stateNextIndexes = Map.empty,
            stateMatchIndexes = Map.empty,
            stateHeartbeatActive = False,
            stateAwaitingResponses = Map.empty,
            stateLeaderNoOpIndex = Nothing,
            statePendingProposal = Nothing,
            stateConfigurationPreparation = Nothing,
            stateFreshAcknowledgers = Set.empty
          }
      abandonedPreparationEffects = case stateConfigurationPreparation state of
        Just (RaftConfigurationPreparation proposal _ _ _) -> [ReportRaftProposal proposal (RaftProposalRedirected leader newTerm)]
        Nothing -> []
   in (follower, heartbeatEffects <> pendingEffects <> abandonedPreparationEffects)

resetElectionTimer ::
  RaftState bytes ->
  (RaftState bytes, [RaftEffect bytes])
resetElectionTimer state
  | not (localCanCampaign state) =
      ( state {stateElectionActive = False, stateElectionDuration = Nothing},
        [SupersedeElectionTimer (stateElectionGeneration state) | stateElectionActive state]
      )
  | otherwise =
      let oldGeneration = stateElectionGeneration state
          newGeneration =
            raftElectionTimerGeneration
              (raftElectionTimerGenerationWord64 oldGeneration + 1)
          supersede =
            if stateElectionActive state
              then [SupersedeElectionTimer oldGeneration]
              else []
       in ( state
              { stateElectionGeneration = newGeneration,
                stateElectionActive = True,
                stateElectionDuration = Nothing
              },
            supersede <> [RequestElectionTimeout newGeneration]
          )

sendAppendEntriesToAll ::
  RaftState bytes ->
  Either RaftFault (RaftState bytes, [RaftEffect bytes])
sendAppendEntriesToAll state =
  foldLeftEither sendOne (state, []) (peers state)
  where
    sendOne (current, effects) peer = do
      (successor, effect) <- sendAppendEntriesTo peer current
      pure (successor, effects <> [effect])

sendAppendEntriesTo ::
  RaftNodeId ->
  RaftState bytes ->
  Either RaftFault (RaftState bytes, RaftEffect bytes)
sendAppendEntriesTo peer state
  | firstIndex <= raftLogBaseIndex (stateLog state),
    Just checkpoint <- stateCheckpoint state = do
      rpc <- mapLeft RaftInternalInputConstructionFault (installSnapshot (stateTerm state) (localNode state) checkpoint)
      pure (sendRpc peer (Just (AwaitSnapshotResponse (stateTerm state) (raftCheckpointIndex checkpoint))) rpc state)
  | otherwise = sendAppend
  where
    firstIndex = Map.findWithDefault (nextLogIndex state) peer (stateNextIndexes state)
    sendAppend = do
      let previousIndex = predecessorIndex firstIndex
      previousTerm <- case raftLogTermAt previousIndex (stateLog state) of
        Nothing -> Left (RaftMissingPreviousLogTerm previousIndex)
        Just term -> Right term
      let entries = raftLogEntriesFrom firstIndex (stateLog state)
          lastSentIndex = case reverse entries of
            [] -> previousIndex
            entry : _ -> raftLogEntryIndex entry
      rpc <- mapLeft RaftInternalInputConstructionFault (appendEntries (stateTerm state) (localNode state) previousIndex previousTerm entries (stateCommitIndex state))
      pure (sendRpc peer (Just (AwaitAppendResponse (stateTerm state) previousIndex lastSentIndex)) rpc state)

sendRequestsToPeers ::
  RaftDispatchExpectation ->
  RaftRpc bytes ->
  RaftState bytes ->
  (RaftState bytes, [RaftEffect bytes])
sendRequestsToPeers expectation rpc state =
  foldl'
    ( \(current, effects) peer ->
        let (successor, effect) = sendRpc peer (Just expectation) rpc current
         in (successor, effects <> [effect])
    )
    (state, [])
    (filter (/= localNode state) (voters state))

sendRpc ::
  RaftNodeId ->
  Maybe RaftDispatchExpectation ->
  RaftRpc bytes ->
  RaftState bytes ->
  (RaftState bytes, RaftEffect bytes)
sendRpc target expectation rpc state =
  case expectation of
    Just expected
      | Just (generation, outstanding) <-
          Map.lookup target (stateAwaitingResponses state),
        expected == outstanding ->
          (state, SendRaftRpc target generation rpc)
    _ ->
      let oldGeneration =
            Map.findWithDefault
              (raftDispatchGeneration 0)
              target
              (stateDispatchGenerations state)
          generation =
            raftDispatchGeneration
              (raftDispatchGenerationWord64 oldGeneration + 1)
          awaiting = case expectation of
            Nothing -> stateAwaitingResponses state
            Just expected ->
              Map.insert target (generation, expected) (stateAwaitingResponses state)
          successor =
            state
              { stateDispatchGenerations =
                  Map.insert target generation (stateDispatchGenerations state),
                stateAwaitingResponses = awaiting
              }
       in (successor, SendRaftRpc target generation rpc)

-- Exact source binding belongs to transport admission. A lagging receiver may
-- not yet know a legal leader, and a leader can finish its own exclusion.
requireRemotePeer :: RaftNodeId -> RaftState bytes -> Either RaftFault ()
requireRemotePeer source state
  | source == localNode state = Left RaftRpcFromLocalNode
  | otherwise = Right ()

localNode :: RaftState bytes -> RaftNodeId
localNode = checkedRaftLocalNode . stateGenesis
voters :: RaftState bytes -> [RaftNodeId]
voters = raftVotingConfigurationNodes . snd . effectiveConfiguration
peers :: RaftState bytes -> [RaftNodeId]
peers state = filter (/= localNode state) (Set.toAscList (replicationTargets state))
hasQuorum :: RaftState bytes -> Set.Set RaftNodeId -> Bool
hasQuorum = raftConfigurationHasQuorum . snd . effectiveConfiguration

nextLogIndex :: RaftState bytes -> RaftLogIndex
nextLogIndex = successorIndex . raftLogLastIndex . stateLog

successorIndex :: RaftLogIndex -> RaftLogIndex
successorIndex index = raftLogIndex (raftLogIndexWord64 index + 1)

predecessorIndex :: RaftLogIndex -> RaftLogIndex
predecessorIndex index
  | raftLogIndexWord64 index == 0 = raftLogIndex 0
  | otherwise = raftLogIndex (raftLogIndexWord64 index - 1)

rpcTerm :: RaftRpc bytes -> RaftTerm
rpcTerm rpc = case raftRpcView rpc of
  RequestVoteView term _ _ _ -> term
  RequestVoteResponseView term _ _ -> term
  AppendEntriesView term _ _ _ _ _ -> term
  AppendEntriesResponseView term _ _ -> term
  InstallSnapshotView term _ _ -> term
  InstallSnapshotResponseView term _ _ _ -> term

transitionDiagnostics ::
  RaftState bytes ->
  RaftState bytes ->
  [RaftEffect bytes]
transitionDiagnostics predecessor successor =
  map
    EmitRaftDiagnostic
    ( termDiagnostic
        <> commitDiagnostic
        <> readinessDiagnostic
    )
  where
    termDiagnostic =
      [RaftTermAdvanced (stateTerm predecessor) (stateTerm successor) | stateTerm predecessor /= stateTerm successor]
    commitDiagnostic =
      [ RaftCommitAdvanced
          (stateCommitIndex predecessor)
          (stateCommitIndex successor)
      | stateCommitIndex predecessor /= stateCommitIndex successor
      ]
    readinessDiagnostic =
      [ RaftServiceReadinessChanged (raftStateIsServiceReady successor)
      | raftStateIsServiceReady predecessor /= raftStateIsServiceReady successor
      ]

foldLeftEither ::
  (accumulator -> value -> Either fault accumulator) ->
  accumulator ->
  [value] ->
  Either fault accumulator
foldLeftEither _ initial [] = Right initial
foldLeftEither step initial (value : remaining) = do
  next <- step initial value
  foldLeftEither step next remaining

mapLeft :: (left -> mapped) -> Either left right -> Either mapped right
mapLeft transform value = case value of
  Left problem -> Left (transform problem)
  Right result -> Right result

-- Suffix admission now discovers these faults before any successor is built.
-- Preserve the existing public fault classification for malformed log histories.
mapLogFailure :: Either RaftLogFault value -> Either RaftFault value
mapLogFailure = either (Left . classify) Right
  where
    classify (RaftLogTermOrderInvalid index preceding current) = RaftSuccessorInvariantFault (RaftStateLogTermRegression index preceding current)
    classify (RaftLogConfigurationTransitionInvalid index problem) = RaftSuccessorInvariantFault (RaftStateConfigurationTransitionInvalid index problem)
    classify problem = RaftLogInvariantFault problem

preparationEffects :: RaftState bytes -> [RaftEffect bytes]
preparationEffects state = case stateConfigurationPreparation state of
  Nothing -> []
  Just (RaftConfigurationPreparation proposal _ frontier _) ->
    [ReportRaftProposal proposal (case configurationPreparationMissing state of [] -> RaftConfigurationPrepared frontier; missing -> RaftConfigurationWaiting frontier missing)]

changedPreparationEffects :: RaftState bytes -> RaftState bytes -> [RaftEffect bytes]
changedPreparationEffects preceding successor
  | stateConfigurationPreparation preceding /= stateConfigurationPreparation successor
      || configurationPreparationMissing preceding /= configurationPreparationMissing successor =
      preparationEffects successor
  | otherwise = []

handleBeginConfiguration :: RaftProposalId -> RaftConfigurationRef -> RaftVoterSet -> RaftState bytes -> Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleBeginConfiguration proposal expected desired state = case stateConfigurationPreparation state of
  Just (RaftConfigurationPreparation retained target frontier _)
    | retained == proposal && target == desired && expected == raftCatchUpFrontierConfiguration frontier -> pure (state, preparationEffects state)
  Just _ -> reject
  Nothing -> case effectiveConfiguration state of
    (ref, StableRaftConfiguration old)
      | ref == expected,
        fst (committedConfiguration state) == ref,
        old /= desired,
        raftStateIsServiceReady state,
        stateCommitIndex state == raftLogLastIndex (stateLog state),
        all (`Set.member` replicationTargets state) (raftVoterSetNodes desired) ->
          let frontier = RaftCatchUpFrontier (localNode state) (stateTerm state) ref (stateCommitIndex state) (raftLogLastTerm (stateLog state))
           in pure (state {stateConfigurationPreparation = Just (RaftConfigurationPreparation proposal desired frontier Set.empty)}, [])
    _ -> reject
  where
    reject = pure (state, [ReportRaftProposal proposal RaftConfigurationRejected])

handleCancelConfiguration :: RaftProposalId -> RaftState bytes -> (RaftState bytes, [RaftEffect bytes])
handleCancelConfiguration proposal state = case stateConfigurationPreparation state of
  Just (RaftConfigurationPreparation retained _ _ _)
    | retained == proposal ->
        (state {stateConfigurationPreparation = Nothing}, [ReportRaftProposal proposal RaftConfigurationCancelled])
  _ -> (state, [ReportRaftProposal proposal RaftConfigurationRejected])

handleLearnerReadiness :: RaftLearnerReadiness -> RaftState bytes -> (RaftState bytes, [RaftEffect bytes])
handleLearnerReadiness (RaftLearnerReadiness observed node) state = case stateConfigurationPreparation state of
  Just (RaftConfigurationPreparation proposal desired frontier ready)
    | frontier == observed,
      node `elem` raftVoterSetNodes desired ->
        (state {stateConfigurationPreparation = Just (RaftConfigurationPreparation proposal desired frontier (Set.insert node ready))}, [])
  _ -> (state, [])

handleConfigurationProposal :: RaftProposalId -> RaftConfigurationRef -> RaftVotingConfiguration -> bytes -> RaftState bytes -> Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleConfigurationProposal proposal expected configuration metadata state
  | stateRole state /= RaftLeader = pure (state, [ReportRaftProposal proposal (RaftProposalRedirected (stateLeaderHint state) (stateTerm state))])
  | expected /= fst current || not (raftStateAdapterReady state) = rejected
  | Left _ <- validateRaftConfigurationSuccessor (snd current) configuration = rejected
  | not permitted = rejected
  | otherwise = do
      (newLog, entry) <- mapLogFailure (raftLogAppend (stateTerm state) (Configuration configuration metadata) (stateLog state))
      let index = raftLogEntryIndex entry
          appended = state {stateLog = newLog, stateMatchIndexes = Map.insert (localNode state) index (stateMatchIndexes state), statePendingProposal = Just (RaftPendingProposal proposal index), stateConfigurationPreparation = Nothing}
      (configured, configurationEffects) <- reconcileConfiguration state appended
      let (contacted, guardEffects) = recordLocalQuorumAcknowledgement configured
      (committed, commitEffects) <- advanceCommittedPrefix (leaderCommitCandidate contacted) contacted
      (successor, sendEffects) <- sendAppendEntriesToAll committed
      pure (successor, ReportRaftProposal proposal (RaftProposalAppended index) : configurationEffects <> guardEffects <> commitEffects <> sendEffects)
  where
    current = effectiveConfiguration state
    rejected = pure (state, [ReportRaftProposal proposal RaftConfigurationRejected])
    permitted = case configuration of
      JointRaftConfiguration _ desired -> case stateConfigurationPreparation state of
        Just (RaftConfigurationPreparation retained target _ _) -> retained == proposal && target == desired && null (configurationPreparationMissing state)
        Nothing -> False
      StableRaftConfiguration _ -> expected == fst (committedConfiguration state) && stateConfigurationPreparation state == Nothing

handleTargets :: [RaftNodeId] -> RaftState bytes -> Either RaftFault (RaftState bytes, [RaftEffect bytes])
handleTargets targets state = do
  let updated = normalizeProgress (state {stateRegisteredTargets = Set.fromList targets})
  (participating, effects) <- reconcileParticipation updated
  if stateRole participating == RaftLeader
    then do
      (successor, sends) <- sendAppendEntriesToAll participating
      pure (successor, effects <> sends)
    else pure (participating, effects)

normalizeProgress :: RaftState bytes -> RaftState bytes
normalizeProgress state =
  state
    { stateNextIndexes = Map.restrictKeys (Map.union (stateNextIndexes state) (Map.fromList [(node, nextLogIndex state) | node <- peers state])) targets,
      stateMatchIndexes = Map.restrictKeys (Map.union (stateMatchIndexes state) (Map.fromList [(node, raftLogIndex 0) | node <- peers state])) (Set.insert (localNode state) targets),
      stateAwaitingResponses = Map.restrictKeys (stateAwaitingResponses state) targets,
      stateCandidateVotes = Set.intersection (stateCandidateVotes state) (Set.fromList (voters state))
    }
  where
    targets = replicationTargets state

reconcileConfiguration :: RaftState bytes -> RaftState bytes -> Either RaftFault (RaftState bytes, [RaftEffect bytes])
reconcileConfiguration preceding installed
  | effectiveConfiguration preceding == effectiveConfiguration installed = pure (installed, [])
  | otherwise = do
      let configured =
            normalizeProgress
              installed
                { stateAwaitingResponses = Map.empty,
                  stateFreshAcknowledgers = Set.empty,
                  stateQuorumDispatchFloors = stateDispatchGenerations installed,
                  stateConfigurationPreparation = Nothing
                }
          (guarded, guardEffects) = if stateRole configured == RaftLeader then clearRecentLeader configured else (configured, [])
      (successor, participationEffects) <- reconcileParticipation guarded
      pure (successor, guardEffects <> participationEffects)

reconcileParticipation :: RaftState bytes -> Either RaftFault (RaftState bytes, [RaftEffect bytes])
reconcileParticipation state
  | stateRole state == RaftLeader = pure (state, [])
  | stateRole state == RaftCandidate && not (localCanCampaign state) = stepDown (stateTerm state) Nothing state
  | stateElectionActive state /= localCanCampaign state = pure (resetElectionTimer state)
  | otherwise = pure (state, [])

-- Final commitment is exposed before a self-excluding leader sends the final
-- committed prefix to its remaining targets and relinquishes its role.
finishCommittedExclusion :: RaftState bytes -> Either RaftFault (RaftState bytes, [RaftEffect bytes])
finishCommittedExclusion state
  | stateRole state == RaftLeader && not (localCanCampaign state) = do
      (replicated, sends) <- sendAppendEntriesToAll state
      (follower, roleEffects) <- stepDown (stateTerm state) Nothing replicated
      let (successor, guardEffects) = clearRecentLeader follower
      pure (successor, sends <> roleEffects <> guardEffects)
  | otherwise = pure (state, [])

applicableResponse :: RaftNodeId -> RaftDispatchGeneration -> RaftRpc bytes -> RaftState bytes -> Bool
applicableResponse source generation rpc state = case (Map.lookup source (stateAwaitingResponses state), raftRpcView rpc) of
  (Just (expected, AwaitVoteResponse _), RequestVoteResponseView {}) -> generation == expected
  (Just (expected, AwaitAppendResponse {}), AppendEntriesResponseView {}) -> generation == expected
  (Just (expected, AwaitSnapshotResponse {}), InstallSnapshotResponseView {}) -> generation == expected
  _ -> False

renewRecentLeader :: RaftState bytes -> (RaftState bytes, [RaftEffect bytes])
renewRecentLeader state =
  let generation = raftRecentLeaderTimerGeneration (raftRecentLeaderTimerGenerationWord64 (stateRecentLeaderGeneration state) + 1)
      duration = raftNativeElectionTimeoutLower (checkedRaftNativeConfiguration (stateGenesis state))
   in ( state {stateRecentLeaderGeneration = generation, stateRecentLeaderActive = True, stateRecentLeaderWindowActive = True, stateFreshAcknowledgers = Set.empty, stateQuorumDispatchFloors = stateDispatchGenerations state},
        [SupersedeRecentLeaderTimer (stateRecentLeaderGeneration state) | stateRecentLeaderWindowActive state] <> [ArmRecentLeaderTimer generation duration]
      )

-- Partial evidence collected without protection still needs a bounded lifetime.
-- Start its timer once, preserving the dispatch cutoff and all observations in
-- this cohort. Further insufficient replies cannot extend the observation window.
startRecentLeaderObservationWindow :: RaftState bytes -> (RaftState bytes, [RaftEffect bytes])
startRecentLeaderObservationWindow state
  | stateRecentLeaderWindowActive state = (state, [])
  | otherwise =
      let generation = raftRecentLeaderTimerGeneration (raftRecentLeaderTimerGenerationWord64 (stateRecentLeaderGeneration state) + 1)
          duration = raftNativeElectionTimeoutLower (checkedRaftNativeConfiguration (stateGenesis state))
       in (state {stateRecentLeaderGeneration = generation, stateRecentLeaderWindowActive = True}, [ArmRecentLeaderTimer generation duration])

clearRecentLeader :: RaftState bytes -> (RaftState bytes, [RaftEffect bytes])
clearRecentLeader state =
  ( state {stateRecentLeaderActive = False, stateRecentLeaderWindowActive = False, stateFreshAcknowledgers = Set.empty, stateQuorumDispatchFloors = stateDispatchGenerations state},
    [SupersedeRecentLeaderTimer (stateRecentLeaderGeneration state) | stateRecentLeaderWindowActive state]
  )

expireRecentLeader :: RaftRecentLeaderTimerGeneration -> RaftState bytes -> (RaftState bytes, [RaftEffect bytes])
expireRecentLeader generation state
  | stateRecentLeaderWindowActive state && generation == stateRecentLeaderGeneration state = clearRecentLeader state
  | otherwise = (state, [])

recordQuorumAcknowledgement :: RaftNodeId -> RaftDispatchGeneration -> RaftState bytes -> (RaftState bytes, [RaftEffect bytes])
recordQuorumAcknowledgement source generation state
  | source `notElem` voters state = (state, [])
  | generation <= Map.findWithDefault (raftDispatchGeneration 0) source (stateQuorumDispatchFloors state) = (state, [])
  | otherwise =
      let acknowledged = state {stateFreshAcknowledgers = Set.insert source (stateFreshAcknowledgers state)}
          evidence = Set.insert (localNode state) (stateFreshAcknowledgers acknowledged)
       in if hasQuorum acknowledged evidence then renewRecentLeader acknowledged else startRecentLeaderObservationWindow acknowledged

-- An explicit local acknowledgement is a fresh whole quorum only for a set
-- whose predicate self satisfies. Sending heartbeats cannot substitute for the
-- remote acknowledgements required by any other stable or joint configuration.
recordLocalQuorumAcknowledgement :: RaftState bytes -> (RaftState bytes, [RaftEffect bytes])
recordLocalQuorumAcknowledgement state
  | hasQuorum state (Set.singleton (localNode state)) = renewRecentLeader state
  | otherwise = (state, [])
