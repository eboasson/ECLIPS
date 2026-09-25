-- | Package-private runtime vocabulary. Kernel states deliberately do not occur
-- in any queue, handle, STM cell, or public snapshot in this module.
module Eclips.Oracle.Runtime.Internal.Types
  ( RuntimeElectionTimeoutSource (..),
    RuntimeRaftTransport (..),
    RuntimeReplicaRegistry (..),
    OracleRaftProjection (..),
    RuntimeDiagnosticSink (..),
    OracleRuntimeRecordingMode (..),
    RuntimeSubmissionCheckpoint (..),
    OracleRuntimeConfiguration (..),
    OracleRuntimeStartupFault (..),
    OracleRuntimeFailure (..),
    OracleRuntimeExit (..),
    OracleRuntimeStatus (..),
    OracleRuntimeHealth (..),
    OracleSubmitResult (..),
    OracleRequestResult (..),
    OracleWatchResult (..),
    OracleRuntimeEvent (..),
    OracleVoterCheckpoint (..),
    RaftReplyRoute (..),
    RaftOwnerCommand (..),
    RaftBatchEnvelope (..),
    RaftDispatcherCommand (..),
    OracleOwnerCommand (..),
    OracleOwnerResult (..),
    OracleReceiptRetention (..),
    OracleSubmissionPreflight (..),
    AdapterCommand (..),
    WatchLedgerCommand (..),
    RecordingCommand (..),
    RuntimeTimerCommand (..),
    ProposalWaiter (..),
    RuntimeCoordination (..),
    RuntimeChild (..),
    RuntimeScope (..),
    RuntimeTimers (..),
    OracleRuntime (..),
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM
  ( TMVar,
    TQueue,
    TVar,
  )
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Word (Word64)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    HeraldId,
    LabelDecisionId,
  )
import Eclips.Domain.Membership (HeraldMembershipHistory)
import Eclips.Oracle.Admission (HeraldAdmissionManifest)
import Eclips.Oracle.Canonical (CanonicalOracleEnvelope)
import Eclips.Oracle.Command (OracleCommand)
import Eclips.Oracle.Effect
  ( OracleEffectBatch,
    OracleProtocolDisposition,
    OracleStepOutcome,
  )
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    OracleGenesisFault,
  )
import Eclips.Oracle.Identity (OracleClientRequestId)
import Eclips.Oracle.Progress (OracleProgress)
import Eclips.Oracle.Projection (AppliedOracleEntry)
import Eclips.Oracle.Receipt (OracleReceipt, OracleRequestRetirement)
import Eclips.Oracle.Runtime.Internal.CheckpointCache (CapturedOracleCheckpoint)
import Eclips.Oracle.Runtime.Internal.WatchAvailability (RuntimeDelay)
import Eclips.Oracle.State (OracleStateDigestMode)
import Eclips.Oracle.Transition (OracleInvariantFault)
import Eclips.Oracle.Voter
import Eclips.Protocol.Oracle.Types (OracleShapeError)
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)
import Eclips.Raft.Checkpoint (RaftCheckpoint)
import Eclips.Raft.Configuration
import Eclips.Raft.Effect
  ( RaftCommittedEntry,
    RaftEffectBatch,
    RaftProposalStatus,
  )
import Eclips.Raft.Genesis
  ( CheckedRaftGenesis,
    RaftGenesisFault,
  )
import Eclips.Raft.Identity
  ( RaftDispatchGeneration,
    RaftElectionTimerGeneration,
    RaftHeartbeatTimerGeneration,
    RaftLogIndex,
    RaftNodeId,
    RaftProposalId,
    RaftRecentLeaderTimerGeneration,
    RaftTerm,
  )
import Eclips.Raft.Input
  ( RaftInput,
    RaftRpc,
  )
import Eclips.Raft.State (RaftRole)
import Eclips.Raft.Transition (RaftFault)

-- | Eight-byte scheduling-jitter source. It is neither an identity generator
-- nor a security capability.
newtype RuntimeElectionTimeoutSource = RuntimeElectionTimeoutSource
  { runRuntimeElectionTimeoutSource :: IO Word64
  }

-- | Interpret one outgoing checked Raft request. The response callback must be
-- invoked only with a role-admitted response from the target and with the
-- generation supplied for the request.
newtype RuntimeRaftTransport = RuntimeRaftTransport
  { runRuntimeRaftTransport ::
      RaftNodeId ->
      RaftDispatchGeneration ->
      RaftRpc ByteString ->
      (RaftRpc ByteString -> IO ()) ->
      IO ()
  }

-- | The transport registry interprets committed dialing hints outside kernels.
newtype RuntimeReplicaRegistry = RuntimeReplicaRegistry
  { runRuntimeReplicaRegistry :: [OracleReplicaRegistration] -> [RaftNodeId] -> IO ()
  }

-- | Only current committed administration facts and prepared metadata cross
-- owner queues. Neither kernel state nor historical log snapshots escape.
data OracleRaftProjection = OracleRaftProjection
  { projectedVoterConfiguration :: VoterConfiguration,
    projectedReplicaRegistrations :: [OracleReplicaRegistration],
    projectedReplicationTargets :: [RaftNodeId],
    projectedVoterChange :: Maybe VoterChange,
    projectedConfigurationProposal :: Maybe (RaftVotingConfiguration, ByteString)
  }
  deriving stock (Eq, Show)

newtype RuntimeDiagnosticSink = RuntimeDiagnosticSink
  { runRuntimeDiagnosticSink :: OracleRuntimeEvent -> IO ()
  }

-- | Diagnostics are streamed in both modes. Exact lifetime history is an
-- explicit conformance option, not a prerequisite for running a replica.
data OracleRuntimeRecordingMode = OracleDiagnosticsOnly | OracleCaptureHistory
  deriving stock (Eq, Show)

newtype RuntimeSubmissionCheckpoint = RuntimeSubmissionCheckpoint
  { runRuntimeSubmissionCheckpoint ::
      OracleClientRequestId -> OracleCommand -> ControlIndex -> IO ()
  }

-- | Causal owner observations for finite fault schedules. Callbacks are shell
-- interpretation points and never receive a kernel state.
data OracleVoterCheckpoint
  = OracleVoterPreparationCaptured RaftCatchUpFrontier
  | OracleVoterConfigurationAppended RaftVotingConfiguration RaftLogIndex
  | OracleVoterConfigurationCommitted RaftVotingConfiguration RaftLogIndex
  | OracleApplicationCommitted OracleCommand RaftLogIndex
  | OracleApplicationApplied AppliedOracleEntry RaftLogIndex
  deriving stock (Eq, Show)

data OracleRuntimeConfiguration = OracleRuntimeConfiguration
  { configuredRaftGenesis :: CheckedRaftGenesis,
    configuredOracleGenesis :: CheckedOracleGenesis,
    configuredStateDigestMode :: OracleStateDigestMode,
    configuredLocalCohost :: HeraldEpoch,
    configuredElectionTimeoutSource :: RuntimeElectionTimeoutSource,
    configuredWatchAuthorityDelay :: RuntimeDelay,
    configuredRaftTransport :: RuntimeRaftTransport,
    configuredReplicaRegistry :: RuntimeReplicaRegistry,
    configuredReplicaResolver :: RaftNodeId -> IO (Maybe OracleReplicaRegistration),
    configuredReplicaBootstrap :: Maybe OracleReplicaRegistration,
    configuredVoterCheckpoint :: OracleVoterCheckpoint -> IO (),
    configuredDiagnosticSink :: RuntimeDiagnosticSink,
    configuredRecordingMode :: OracleRuntimeRecordingMode,
    configuredSubmissionCheckpoint :: RuntimeSubmissionCheckpoint
  }

data OracleRuntimeStartupFault
  = RuntimeRaftGenesisRejected RaftGenesisFault
  | RuntimeOracleGenesisRejected OracleGenesisFault
  | RuntimeRaftOracleConfigurationMismatch
  | RuntimeRaftVoterBindingMismatch
  | RuntimeLocalCohostBindingMissing RaftNodeId
  | RuntimeLocalCohostMismatch HeraldEpoch HeraldEpoch
  | RuntimeLocalReplicaRegistrationMismatch RaftNodeId
  deriving stock (Eq, Show)

data OracleRuntimeFailure
  = RuntimeRaftInvariantFault RaftFault
  | RuntimeOracleInvariantFault OracleInvariantFault
  | RuntimeCommittedEnvelopeInvariantFault RaftLogIndex
  | RuntimeSubmissionPreflightInvariantFault ControlIndex ControlIndex
  | RuntimeWatchLedgerGap ControlIndex ControlIndex
  | RuntimeWatchLedgerConflict ControlIndex
  | RuntimeWatchEncodingInvariantFault ControlIndex OracleShapeError
  | RuntimeEssentialChildFailed String String
  deriving stock (Eq, Show)

data OracleRuntimeExit
  = OracleRuntimeStopped
  | OracleRuntimeFailed OracleRuntimeFailure
  deriving stock (Eq, Show)

-- | Small routing/readiness projection published by the exclusive owners.
data OracleRuntimeStatus = OracleRuntimeStatus
  { statusNode :: !RaftNodeId,
    statusRole :: !RaftRole,
    statusTerm :: !RaftTerm,
    statusLeaderHint :: !(Maybe RaftNodeId),
    statusServiceReady :: !Bool,
    statusAppliedControlIndex :: !ControlIndex,
    statusLabelRetiredThrough :: !ControlIndex,
    statusCompletedWorkflowCount :: !Int,
    statusAppliedRaftIndex :: !RaftLogIndex,
    statusVoterConfiguration :: !VoterConfiguration,
    statusReplicaRegistrations :: ![OracleReplicaRegistration],
    statusPendingVoterChange :: !(Maybe VoterChange),
    statusConfigurationFrontier :: !(Maybe RaftCatchUpFrontier),
    statusEffectiveConfiguration :: !(RaftConfigurationRef, RaftVotingConfiguration),
    statusConfigurationGeneration :: !Word64,
    statusMembershipHistory :: !HeraldMembershipHistory,
    statusHeraldCatalogue :: ![HeraldAdmissionManifest],
    statusActiveHeralds :: !(Map HeraldEpoch HeraldId),
    statusAccepting :: !Bool
  }
  deriving stock (Eq, Show)

-- | A fresh queue response from the native owner, independent of leader
-- readiness and the adapter's committed control projection.
data OracleRuntimeHealth
  = OracleRuntimeHealth
      RaftNodeId
      RaftTerm
      Word64
      RaftConfigurationRef
      RaftVotingConfiguration
  deriving stock (Eq, Show)

data OracleSubmitResult
  = OracleSubmitReceipt OracleReceipt
  | OracleSubmitDeferred OracleClientRequestId LabelDecisionId ControlIndex
  | OracleSubmitConflict OracleProtocolDisposition
  | OracleSubmitRedirect (Maybe RaftNodeId) RaftTerm
  | OracleSubmitNotReady RaftTerm
  | OracleSubmitUnavailable
  | OracleSubmitProgressRetired HeraldEpoch OracleProgress
  | OracleSubmitRetired OracleRequestRetirement
  deriving stock (Eq, Show)

data OracleRequestResult
  = OracleRequestFound OracleReceipt
  | OracleRequestAbsent ControlIndex
  | OracleRequestRedirect (Maybe RaftNodeId) RaftTerm
  | OracleRequestNotReady RaftTerm
  | OracleRequestUnavailable
  | OracleRequestRetired OracleRequestRetirement
  deriving stock (Eq, Show)

data OracleWatchResult
  = OracleWatchEntries (NonEmpty AppliedOracleEntry)
  | OracleWatchRedirect (Maybe RaftNodeId) RaftTerm
  | OracleWatchNotReady RaftTerm
  | OracleWatchCursorAhead ControlIndex
  | OracleWatchUnavailable
  deriving stock (Eq, Show)

-- | Exact finite-run shell evidence. It carries typed observations and sealed
-- outcomes, never a live kernel state or mutable ledger.
data OracleRuntimeEvent
  = RuntimeRaftInputInstalled (RaftInput ByteString)
  | RuntimeRaftBatchHanded !Int
  | RuntimeOracleInputInstalled OracleClientRequestId OracleStepOutcome
  | RuntimeOracleBatchHanded !Int
  | RuntimeWatchEntryRetained ControlIndex
  | RuntimeWatchEntryPublished ControlIndex
  | RuntimeProposalObserved RaftProposalId RaftProposalStatus
  | RuntimeRaftCheckpointOfferObserved !RaftLogIndex !(Maybe RaftLogIndex)
  | RuntimeReplicaStopped
  deriving stock (Eq, Show)

newtype RaftReplyRoute = RaftReplyRoute
  { sendRaftReply :: RaftRpc ByteString -> IO ()
  }

data RaftOwnerCommand
  = StepRaftOwner
      (RaftInput ByteString)
      (Maybe RaftReplyRoute)
  | UpdateOracleRaftProjection OracleRaftProjection
  | CancelRaftPreparation VoterChangeId (TMVar Bool)
  | ReconcileOracleVoters
  | QueryRaftHealth (TMVar OracleRuntimeHealth)
  | BarrierRaftOwner (TMVar ())
  | StopRaftOwner

data RaftBatchEnvelope = RaftBatchEnvelope
  { raftBatchReplyRoute :: Maybe RaftReplyRoute,
    raftBatchEffects :: RaftEffectBatch ByteString
  }

data RaftDispatcherCommand
  = DispatchRaftBatch RaftBatchEnvelope
  | UpdateRuntimeReplicaRegistry [OracleReplicaRegistration] [RaftNodeId]
  | BarrierRaftDispatcher (TMVar ())
  | StopRaftDispatcher

data OracleOwnerCommand
  = ApplyOracleOwner
      CanonicalOracleEnvelope
      (TMVar (Either OracleInvariantFault OracleOwnerResult))
  | ApplyOracleConfigurationOwner
      (RaftCommittedEntry ByteString)
      (TMVar (Either OracleInvariantFault (AppliedOracleEntry, OracleReceiptRetention)))
  | CaptureOracleCheckpointOwner (TMVar CapturedOracleCheckpoint)
  | InstallOracleCheckpointOwner (RaftConfigurationRef, RaftVotingConfiguration) ByteString (TMVar (Either OracleRuntimeFailure (ControlIndex, [OracleReceipt], OracleReceiptRetention)))
  | QueryOracleVoterChangeOwner VoterChangeId (TMVar (Maybe VoterChange))
  | PreflightOracleOwner
      CanonicalOracleEnvelope
      (TMVar OracleSubmissionPreflight)
  | BarrierOracleOwner (TMVar ())
  | StopOracleOwner

data OracleOwnerResult = OracleOwnerResult
  { oracleOwnerOutcome :: OracleStepOutcome,
    oracleOwnerEffects :: OracleEffectBatch,
    oracleOwnerReceiptRetention :: !OracleReceiptRetention
  }

-- | Compact committed facts copied from the owner after installing a step.
-- The adapter uses the same boundary for published results and retirement
-- classification; a receipt can never be reported absent during reclamation.
data OracleReceiptRetention = OracleReceiptRetention
  { receiptRetirementFrontiers :: !(Map HeraldEpoch ReceiptRetirement),
    receiptRetiredHomes :: !(Set HeraldEpoch)
  }
  deriving stock (Eq)

data OracleSubmissionPreflight
  = OracleSubmissionMayProceed ControlIndex
  | OracleSubmissionMustDefer LabelDecisionId ControlIndex
  deriving stock (Eq, Show)

data AdapterCommand
  = ApplyCommittedRaftEntries
      (NonEmpty (RaftCommittedEntry ByteString))
      (TMVar (Either OracleRuntimeFailure ()))
  | InstallCommittedRaftCheckpoint (RaftCheckpoint ByteString) (TMVar (Either OracleRuntimeFailure ()))
  | QueryPublishedOracleResult
      OracleClientRequestId
      (TMVar (Maybe OracleReceipt, Maybe OracleRequestRetirement, OracleRuntimeStatus, Bool))
  | BarrierAdapter (TMVar ())
  | StopAdapter

data WatchLedgerCommand
  = InsertWatchEntry
      AppliedOracleEntry
      (TMVar (Either OracleRuntimeFailure ()))
  | CaptureWatchCheckpoint ControlIndex (TMVar (Either OracleRuntimeFailure [AppliedOracleEntry]))
  | InstallWatchCheckpoint ControlIndex [AppliedOracleEntry] (TMVar (Either OracleRuntimeFailure ()))
  | ReadWatchSuffix
      Word64
      ControlIndex
      (TMVar OracleWatchResult)
  | CancelWatch Word64
  | PublishWatchThrough
      ControlIndex
      (TMVar (Either OracleRuntimeFailure ()))
  | BarrierWatchLedger (TMVar ())
  | StopWatchLedger

data RecordingCommand
  = AppendRuntimeRecord OracleRuntimeEvent
  | SnapshotRuntimeRecording (TMVar [OracleRuntimeEvent])
  | StopRuntimeRecording

data RuntimeTimerCommand generation
  = ArmRuntimeTimer generation Word64
  | SupersedeRuntimeTimer generation
  | BarrierRuntimeTimer (TMVar ())
  | StopRuntimeTimer

data ProposalWaiter = ProposalWaiter
  { proposalWaiterResult :: TMVar OracleSubmitResult,
    proposalWaiterLogIndex :: Maybe RaftLogIndex
  }

data RuntimeCoordination = RuntimeCoordination
  { coordinationNextProposal :: TVar Word64,
    coordinationNextWatch :: TVar Word64,
    coordinationProposals :: TVar (Map RaftProposalId ProposalWaiter),
    coordinationProposalByIndex :: TVar (Map RaftLogIndex RaftProposalId),
    coordinationOracleControlIndex :: TVar ControlIndex,
    coordinationSubmissionReserved :: TVar Bool,
    coordinationSemanticReady :: TVar Bool
  }

data RuntimeChild = RuntimeChild
  { runtimeChildName :: String,
    runtimeChildThread :: ThreadId,
    runtimeChildDone :: MVar (Either String ())
  }

data RuntimeScope = RuntimeScope
  { runtimeScopeClosing :: TVar Bool,
    runtimeScopeFailure :: TMVar OracleRuntimeFailure,
    runtimeScopeChildren :: TVar [RuntimeChild]
  }

data RuntimeTimers = RuntimeTimers
  { electionTimerCommands :: TQueue (RuntimeTimerCommand RaftElectionTimerGeneration),
    heartbeatTimerCommands :: TQueue (RuntimeTimerCommand RaftHeartbeatTimerGeneration),
    recentLeaderTimerCommands :: TQueue (RuntimeTimerCommand RaftRecentLeaderTimerGeneration)
  }

data OracleRuntime = OracleRuntime
  { runtimeConfiguration :: OracleRuntimeConfiguration,
    runtimeRaftCommands :: TQueue RaftOwnerCommand,
    runtimeRaftBatches :: TQueue RaftDispatcherCommand,
    runtimeOracleCommands :: TQueue OracleOwnerCommand,
    runtimeAdapterCommands :: TQueue AdapterCommand,
    runtimeWatchCommands :: TQueue WatchLedgerCommand,
    runtimeRecordingCommands :: TQueue RecordingCommand,
    runtimeFinalRecording :: TMVar [OracleRuntimeEvent],
    runtimeStatusCell :: TVar OracleRuntimeStatus,
    runtimeCoordination :: RuntimeCoordination,
    runtimeScope :: RuntimeScope,
    runtimeTimers :: RuntimeTimers,
    runtimeExitCell :: TMVar OracleRuntimeExit,
    runtimeSupervisorDone :: MVar ()
  }
