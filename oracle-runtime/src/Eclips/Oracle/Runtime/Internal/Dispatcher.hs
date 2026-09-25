-- | Whole-batch Raft effect dispatcher.
module Eclips.Oracle.Runtime.Internal.Dispatcher
  ( runRaftDispatcher,
  )
where

import Control.Concurrent.STM
  ( atomically,
    newEmptyTMVar,
    orElse,
    putTMVar,
    readTMVar,
    readTQueue,
    readTVar,
    writeTQueue,
    writeTVar,
  )
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Eclips.Oracle.Runtime.Internal.Checkpoint (encodeRuntimeCheckpoint, localRuntimeCheckpoint)
import Eclips.Oracle.Runtime.Internal.Connection (offerRaftResponse)
import Eclips.Oracle.Runtime.Internal.Coordination (observeProposalStatus)
import Eclips.Oracle.Runtime.Internal.Entropy (selectElectionTimeoutDuration)
import Eclips.Oracle.Runtime.Internal.Recording (recordRuntimeEvent)
import Eclips.Oracle.Runtime.Internal.Scope
  ( runtimeScopeObservedFailure,
    signalRuntimeFailure,
  )
import Eclips.Oracle.Runtime.Internal.Types
  ( AdapterCommand (ApplyCommittedRaftEntries, InstallCommittedRaftCheckpoint),
    OracleRuntime (..),
    OracleRuntimeConfiguration (..),
    OracleRuntimeEvent (RuntimeProposalObserved, RuntimeRaftCheckpointOfferObserved),
    OracleRuntimeFailure (RuntimeEssentialChildFailed),
    OracleRuntimeStatus (..),
    RaftBatchEnvelope (..),
    RaftDispatcherCommand (..),
    RaftOwnerCommand (StepRaftOwner),
    RaftReplyRoute (..),
    RuntimeCoordination (..),
    RuntimeElectionTimeoutSource (..),
    RuntimeRaftTransport (..),
    RuntimeReplicaRegistry (..),
    RuntimeTimerCommand (..),
    RuntimeTimers (..),
    WatchLedgerCommand (CaptureWatchCheckpoint),
  )
import Eclips.Raft.Checkpoint
import Eclips.Raft.Effect
  ( RaftEffect (..),
    raftEffectBatchEffects,
  )
import Eclips.Raft.Genesis (checkedRaftNativeConfiguration)
import Eclips.Raft.Identity (raftDurationMicrosWord64)
import Eclips.Raft.Input
  ( RaftInput,
    RaftRpcView (..),
    installSnapshot,
    raftRpcView,
    selectElectionTimeout,
  )

runRaftDispatcher :: OracleRuntime -> IO ()
runRaftDispatcher runtime = loop
  where
    loop = do
      command <- atomically (readTQueue runtime.runtimeRaftBatches)
      case command of
        StopRaftDispatcher -> pure ()
        UpdateRuntimeReplicaRegistry registrations retired -> do
          runRuntimeReplicaRegistry runtime.runtimeConfiguration.configuredReplicaRegistry registrations retired
          loop
        BarrierRaftDispatcher done -> atomically (putTMVar done ()) >> loop
        DispatchRaftBatch envelope -> do
          forM_ (raftEffectBatchEffects envelope.raftBatchEffects) $ \effect -> do
            healthy <- atomically ((runtimeScopeObservedFailure runtime.runtimeScope >> pure False) `orElse` pure True)
            when healthy (interpret envelope.raftBatchReplyRoute effect)
          loop

    interpret replyRoute effect = case effect of
      SendRaftRpc target generation rpc ->
        case raftRpcView rpc of
          RequestVoteResponseView {} -> sendResponse replyRoute rpc
          AppendEntriesResponseView {} -> sendResponse replyRoute rpc
          InstallSnapshotResponseView {} -> sendResponse replyRoute rpc
          RequestVoteView {} -> sendRequest target generation rpc
          AppendEntriesView {} -> sendRequest target generation rpc
          InstallSnapshotView {} -> sendRequest target generation rpc
      RequestElectionTimeout generation -> do
        sample <-
          runRuntimeElectionTimeoutSource
            runtime.runtimeConfiguration.configuredElectionTimeoutSource
        let native =
              checkedRaftNativeConfiguration
                runtime.runtimeConfiguration.configuredRaftGenesis
            duration = selectElectionTimeoutDuration native sample
        offerInput (selectElectionTimeout generation duration)
      ArmElectionTimer generation duration ->
        atomically
          ( writeTQueue
              runtime.runtimeTimers.electionTimerCommands
              (ArmRuntimeTimer generation (raftDurationMicrosWord64 duration))
          )
      SupersedeElectionTimer generation ->
        atomically
          ( writeTQueue
              runtime.runtimeTimers.electionTimerCommands
              (SupersedeRuntimeTimer generation)
          )
      ArmHeartbeatTimer generation duration ->
        atomically
          ( writeTQueue
              runtime.runtimeTimers.heartbeatTimerCommands
              (ArmRuntimeTimer generation (raftDurationMicrosWord64 duration))
          )
      SupersedeHeartbeatTimer generation ->
        atomically
          ( writeTQueue
              runtime.runtimeTimers.heartbeatTimerCommands
              (SupersedeRuntimeTimer generation)
          )
      ArmRecentLeaderTimer generation duration ->
        atomically
          ( writeTQueue
              runtime.runtimeTimers.recentLeaderTimerCommands
              (ArmRuntimeTimer generation (raftDurationMicrosWord64 duration))
          )
      SupersedeRecentLeaderTimer generation ->
        atomically
          ( writeTQueue
              runtime.runtimeTimers.recentLeaderTimerCommands
              (SupersedeRuntimeTimer generation)
          )
      ReportRaftProposal proposal status -> do
        atomically (observeProposalStatus runtime.runtimeCoordination proposal status)
        recordRuntimeEvent runtime.runtimeRecordingCommands (RuntimeProposalObserved proposal status)
      ReportRaftCheckpointOffer offered retained -> do
        -- This FIFO observation follows materialization of all earlier send
        -- recipes. Record only strict scalar coordinates, never the checkpoint
        -- payload or a suspended projection from its owning effect batch.
        let !actual = case retained of
              Nothing -> Nothing
              Just snapshot -> let !index = raftCheckpointIndex snapshot in Just index
        recordRuntimeEvent runtime.runtimeRecordingCommands (RuntimeRaftCheckpointOfferObserved offered actual)
      InstallRaftCheckpoint snapshot -> do
        done <- atomically newEmptyTMVar
        atomically $ do
          writeTVar runtime.runtimeCoordination.coordinationSemanticReady False
          writeTQueue runtime.runtimeAdapterCommands (InstallCommittedRaftCheckpoint snapshot done)
        observed <- atomically ((Left <$> runtimeScopeObservedFailure runtime.runtimeScope) `orElse` (Right <$> readTMVar done))
        case observed of
          Left _ -> pure ()
          Right (Left failure) -> signalRuntimeFailure runtime.runtimeScope failure
          Right (Right ()) -> pure ()
      ExposeCommittedEntries committed -> do
        done <- atomically newEmptyTMVar
        atomically
          ( do
              writeTVar runtime.runtimeCoordination.coordinationSemanticReady False
              writeTQueue
                runtime.runtimeAdapterCommands
                (ApplyCommittedRaftEntries committed done)
          )
        observed <-
          atomically
            ( (Left <$> runtimeScopeObservedFailure runtime.runtimeScope)
                `orElse` (Right <$> readTMVar done)
            )
        case observed of
          Left _ -> pure ()
          Right (Left failure) -> signalRuntimeFailure runtime.runtimeScope failure
          Right (Right ()) -> pure ()
      EmitRaftDiagnostic _ -> pure ()

    sendResponse Nothing _ =
      signalRuntimeFailure
        runtime.runtimeScope
        (RuntimeEssentialChildFailed "raft-dispatcher" "response effect lacked its physical request route")
    sendResponse (Just route) rpc = route.sendRaftReply rpc

    sendRequest target generation rpc = case raftRpcView rpc of
      InstallSnapshotView term leader snapshot -> case localRuntimeCheckpoint (raftCheckpointPayload snapshot) of
        Left problem -> signalRuntimeFailure runtime.runtimeScope (RuntimeEssentialChildFailed "raft-checkpoint" problem)
        Right Nothing -> dispatch rpc
        Right (Just (oracle, through)) -> do
          done <- atomically newEmptyTMVar
          atomically (writeTQueue runtime.runtimeWatchCommands (CaptureWatchCheckpoint through done))
          observed <- atomically ((Left <$> runtimeScopeObservedFailure runtime.runtimeScope) `orElse` (Right <$> readTMVar done))
          case observed of
            Left _ -> pure ()
            Right (Left failure) -> signalRuntimeFailure runtime.runtimeScope failure
            Right (Right entries) -> do
              let payload = encodeRuntimeCheckpoint oracle through entries
                  complete = either (error . show) id (raftCheckpoint (raftCheckpointIndex snapshot) (raftCheckpointTerm snapshot) (raftCheckpointConfigurationRef snapshot) (raftCheckpointConfiguration snapshot) payload)
              dispatch (either (error . show) id (installSnapshot term leader complete))
      _ -> dispatch rpc
      where
        dispatch supplied =
          runRuntimeRaftTransport
            runtime.runtimeConfiguration.configuredRaftTransport
            target
            generation
            supplied
            (offerRaftResponse runtime target generation)

    offerInput :: RaftInput ByteString -> IO ()
    offerInput input = atomically $ do
      status <- readTVar runtime.runtimeStatusCell
      if status.statusAccepting
        then
          writeTQueue
            runtime.runtimeRaftCommands
            (StepRaftOwner input Nothing)
        else pure ()
