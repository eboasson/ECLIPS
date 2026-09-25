-- | The sole owner of one pure Raft state. Retained Oracle administration is a
-- narrow input projection; native transitions and proposal serialization remain
-- local to this owner.
module Eclips.Oracle.Runtime.Internal.RaftOwner (runRaftOwner) where

import Control.Concurrent.STM
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Eclips.Oracle.Genesis (raftVoterBindingNode)
import Eclips.Oracle.Runtime.Internal.Recording (recordRuntimeEvent)
import Eclips.Oracle.Runtime.Internal.Scope (signalRuntimeFailure)
import Eclips.Oracle.Runtime.Internal.StatusProjection (RaftStatusProjection (..), raftStatusProjection, raftStatusProjectionChanged)
import Eclips.Oracle.Runtime.Internal.Submission (mayCancelVoterPreparation)
import Eclips.Oracle.Runtime.Internal.Types
import Eclips.Oracle.Voter
import Eclips.Raft.Configuration
import Eclips.Raft.Effect (raftEffectBatchEffects)
import Eclips.Raft.Genesis (CheckedRaftGenesis)
import Eclips.Raft.Identity
import Eclips.Raft.Input
import Eclips.Raft.State
import Eclips.Raft.Transition (initialRaft, initialRaftEffects, stepRaft)

-- Native proposal identities are local correlations. The metadata's retained
-- change/stage identity supplies logical identity across leader replacement.

runRaftOwner ::
  CheckedRaftGenesis ->
  TQueue RaftOwnerCommand ->
  TQueue RaftDispatcherCommand ->
  TVar OracleRuntimeStatus ->
  RuntimeCoordination ->
  (OracleVoterCheckpoint -> IO ()) ->
  TQueue RecordingCommand ->
  RuntimeScope ->
  IO ()
runRaftOwner genesis commands batches status coordination checkpoint recording scope = do
  let initial = initialRaft genesis
  publishStatus initial
  handBatch Nothing (initialRaftEffects initial)
  loop initial Nothing Nothing
  where
    loop state projection prepared = do
      command <- atomically (readTQueue commands)
      case command of
        StopRaftOwner -> pure ()
        QueryRaftHealth done -> do
          atomically $ do
            current <- readTVar status
            let (reference, configuration) = raftStateEffectiveConfiguration state
            putTMVar done (OracleRuntimeHealth current.statusNode (raftStateTerm state) current.statusConfigurationGeneration reference configuration)
          loop state projection prepared
        BarrierRaftOwner done -> do
          atomically (putTMVar done ())
          loop state projection prepared
        CancelRaftPreparation requested done -> do
          -- The submitting shell already holds the shared reservation. A
          -- logged but unapplied configuration cannot be cancelled, including
          -- one inherited by a new leader while Oracle still says Preparing.
          let settled =
                raftStateEffectiveConfiguration state == raftStateCommittedConfiguration state
                  && raftStateCommitIndex state == raftStateLastLogIndex state
                  && raftStateAppliedThrough state == raftStateCommitIndex state
                  && raftStateRole state == RaftLeader
              cancellable =
                settled
                  && mayCancelVoterPreparation requested (projection >>= projectedVoterChange)
                  && maybe
                    False
                    ((== fst (raftStateEffectiveConfiguration state)) . voterConfigurationNativeRef . projectedVoterConfiguration)
                    projection
          successor <- case (cancellable, prepared, raftStateCatchUpFrontier state) of
            (True, Just (_, _, proposal), Just _) -> install (cancelRaftConfigurationChange proposal) Nothing state
            _ -> pure state
          atomically (putTMVar done (cancellable && raftStateServiceReady successor))
          loop successor projection (if cancellable then Nothing else prepared)
        UpdateOracleRaftProjection observed -> do
          let targets = observed.projectedReplicationTargets
              retired = filter (`notElem` targets) (map replicaRegistrationNode observed.projectedReplicaRegistrations)
          installed <-
            if targets == raftStateReplicationTargets state
              then pure state
              else
                install (either (error . show) id (updateRaftReplicationTargets targets)) Nothing state
          when (maybe True (\previous -> previous.projectedReplicaRegistrations /= observed.projectedReplicaRegistrations || previous.projectedReplicationTargets /= targets) projection)
            $ atomically (writeTQueue batches (UpdateRuntimeReplicaRegistry observed.projectedReplicaRegistrations retired))
          continue installed (Just observed) prepared
        ReconcileOracleVoters -> continue state projection prepared
        StepRaftOwner input replyRoute -> do
          successor <- install input replyRoute state
          continue successor projection prepared

    continue state projection prepared = do
      (successor, nextPrepared) <- reconcile state projection prepared
      loop successor projection nextPrepared

    reconcile state Nothing _ = pure (state, Nothing)
    reconcile state (Just projection) prepared =
      case (projection.projectedVoterChange, projection.projectedConfigurationProposal) of
        (Just change, Just (configuration, metadata))
          | raftStateRole state == RaftLeader,
            fst (raftStateEffectiveConfiguration state) == voterConfigurationNativeRef projection.projectedVoterConfiguration,
            raftStateAppliedThrough state == raftStateCommitIndex state,
            raftStateCommitIndex state == raftStateLastLogIndex state,
            raftStatePendingProposal state == Nothing -> do
              reserved <- atomically $ do
                busy <- readTVar coordination.coordinationSubmissionReserved
                if busy
                  then pure False
                  else do
                    writeTVar coordination.coordinationSubmissionReserved True
                    writeTVar coordination.coordinationSemanticReady False
                    pure True
              if not reserved
                then pure (state, prepared)
                else do
                  (successor, nextPrepared) <- drive state change configuration metadata prepared
                  atomically $ do
                    writeTVar coordination.coordinationSubmissionReserved False
                    writeTVar coordination.coordinationSemanticReady True
                  pure (successor, nextPrepared)
        _ -> pure (state, Nothing)

    drive state change configuration metadata prepared = do
      let matching = case prepared of
            Just (changeId, term, proposal)
              | changeId == voterChangeId change && term == raftStateTerm state -> Just proposal
            _ -> Nothing
          expected = fst (raftStateEffectiveConfiguration state)
      case voterChangePhase change of
        VoterChangePreparing -> do
          (preparing, proposal) <- case (raftStateCatchUpFrontier state, matching) of
            (Just _, Just proposal) -> pure (state, proposal)
            _
              | raftStateServiceReady state -> do
                  proposal <- allocateProposal
                  let desired =
                        either
                          (error . show)
                          id
                          (raftVoterSet (map raftVoterBindingNode (oracleVoterBindingList (voterChangeNewBindings change))))
                  successor <- install (beginRaftConfigurationChange proposal expected desired) Nothing state
                  pure (successor, proposal)
              | otherwise -> do
                  proposal <- allocateProposal
                  pure (state, proposal)
          if raftStateConfigurationChangeReady preparing
            then do
              successor <- install (proposeRaftConfiguration proposal expected configuration metadata) Nothing preparing
              pure (successor, Nothing)
            else pure (preparing, Just (voterChangeId change, raftStateTerm state, proposal))
        VoterChangeJointCommitted | raftStateServiceReady state -> do
          proposal <- allocateProposal
          successor <- install (proposeRaftConfiguration proposal expected configuration metadata) Nothing state
          pure (successor, Nothing)
        _ -> pure (state, prepared)

    allocateProposal = atomically $ do
      sequenceNumber <- stateTVar coordination.coordinationNextProposal (\current -> (current, current + 1))
      pure (either (error . show) id (mkRaftProposalId sequenceNumber))

    install input replyRoute state = case stepRaft input state of
      Left fault -> do
        signalRuntimeFailure scope (RuntimeRaftInvariantFault fault)
        pure state
      Right (successor, batch) -> do
        publishStatus successor
        case raftInputView input of
          BeginConfigurationChangeView {} -> case (raftStateCatchUpFrontier state, raftStateCatchUpFrontier successor) of
            (Nothing, Just frontier) -> checkpoint (OracleVoterPreparationCaptured frontier)
            _ -> pure ()
          ProposeConfigurationView _ _ configuration _
            | raftStateLastLogIndex successor > raftStateLastLogIndex state ->
                checkpoint (OracleVoterConfigurationAppended configuration (raftStateLastLogIndex successor))
          _ -> pure ()
        recordRuntimeEvent recording (RuntimeRaftInputInstalled input)
        handBatch replyRoute batch
        pure successor

    handBatch replyRoute batch = do
      atomically (writeTQueue batches (DispatchRaftBatch (RaftBatchEnvelope replyRoute batch)))
      recordRuntimeEvent recording (RuntimeRaftBatchHanded (length (raftEffectBatchEffects batch)))

    publishStatus :: RaftState ByteString -> IO ()
    publishStatus state = atomically $ do
      current <- readTVar status
      let effective = raftStateEffectiveConfiguration state
          configurationChanged = effective /= current.statusEffectiveConfiguration
          changed =
            raftStatusProjectionChanged
              state
              current.statusRole
              current.statusTerm
              current.statusLeaderHint
              (current.statusServiceReady && not (isJust current.statusConfigurationFrontier))
              || current.statusAppliedRaftIndex /= raftStateAppliedThrough state
              || current.statusConfigurationFrontier /= raftStateCatchUpFrontier state
              || configurationChanged
      -- Stable heartbeats do not wake parked Hello/Watch transactions.
      when changed $ case raftStatusProjection state current.statusConfigurationGeneration current.statusEffectiveConfiguration of
        RaftStatusProjection role term leader serving applied frontier reference configuration generation -> do
          let projected =
                current
                  { statusRole = role,
                    statusTerm = term,
                    statusLeaderHint = leader,
                    statusServiceReady = serving,
                    statusAppliedRaftIndex = applied,
                    statusConfigurationFrontier = frontier,
                    statusEffectiveConfiguration = (reference, configuration),
                    statusConfigurationGeneration = generation
                  }
          projected `seq` writeTVar status projected
