-- | The sole owner of one pure Oracle state.
module Eclips.Oracle.Runtime.Internal.OracleOwner
  ( runOracleOwner,
  )
where

import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    putTMVar,
    readTQueue,
    readTVar,
    writeTQueue,
    writeTVar,
  )
import Control.Exception (IOException, finally, onException, try)
import Control.Monad (when)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Identity (ControlIndex, controlIndexWord64)
import Eclips.Oracle.Canonical
  ( canonicalOracleEnvelopeValue,
  )
import Eclips.Oracle.Command (oracleEnvelopeCommand, oracleEnvelopeRequestId)
import Eclips.Oracle.Effect (oracleEffects)
import Eclips.Oracle.Genesis (CheckedOracleGenesis)
import Eclips.Oracle.Runtime.Internal.Checkpoint (encodeLocalRuntimeCheckpoint)
import Eclips.Oracle.Runtime.Internal.CheckpointCache
  ( OracleCheckpointCaptureWork (..),
    advanceOracleCheckpointCache,
    captureOracleCheckpoint,
    capturedOracleCheckpointBytes,
    emptyOracleCheckpointCache,
  )
import Eclips.Oracle.Runtime.Internal.Hello (activeHeraldIdentities)
import Eclips.Oracle.Runtime.Internal.Recording (recordRuntimeEvent)
import Eclips.Oracle.Runtime.Internal.Scope (signalRuntimeFailure)
import Eclips.Oracle.Runtime.Internal.Types
  ( OracleOwnerCommand (..),
    OracleOwnerResult (..),
    OracleRaftProjection (..),
    OracleReceiptRetention (..),
    OracleRuntimeEvent (..),
    OracleRuntimeFailure (RuntimeEssentialChildFailed, RuntimeOracleInvariantFault),
    OracleRuntimeStatus (..),
    OracleSubmissionPreflight (..),
    RaftOwnerCommand (..),
    RecordingCommand,
    RuntimeCoordination (..),
    RuntimeScope,
  )
import Eclips.Oracle.Runtime.Internal.WorkCounts
  ( OracleWorkCounts,
    countOracleWork,
    emptyOracleWorkCounts,
    oracleCommandWorkTag,
    oracleOutcomeWorkTag,
    oracleWorkRows,
    oracleWorkSnapshotComplete,
  )
import Eclips.Oracle.State
  ( OracleState,
    OracleStateDigestMode,
    decodeOracleCheckpoint,
    oracleCheckedMembershipHistory,
    oracleCheckpointBytes,
    oracleCompletedWorkflowCount,
    oracleGreatestControlIndex,
    oracleHeraldCatalogue,
    oracleLabelRetiredThrough,
    oracleReceiptRetirementProgresses,
    oracleRequestReceipts,
    oracleRetiredHeralds,
    oracleStateDiagnosticCardinalities,
    oracleStateDiagnosticSections,
    oracleStateDigestMode,
  )
import Eclips.Oracle.Transition
  ( applyCommittedOracleConfiguration,
    initialOracleWithStateDigestMode,
    oracleSubmissionPreflightBlocker,
    stepOracle,
  )

import Eclips.Oracle.Voter
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

runOracleOwner ::
  CheckedOracleGenesis ->
  OracleStateDigestMode ->
  TQueue OracleOwnerCommand ->
  TQueue RaftOwnerCommand ->
  RuntimeCoordination ->
  TVar OracleRuntimeStatus ->
  TQueue RecordingCommand ->
  RuntimeScope ->
  IO ()
runOracleOwner genesis digestMode commands raftCommands coordination statusCell recording scope =
  case initialOracleWithStateDigestMode digestMode genesis of
    Left fault -> error ("checked Oracle genesis failed initialization: " <> show fault)
    Right initial -> withOracleWorkCounts initial (atomically (readTVar coordination.coordinationOracleControlIndex)) $ \recorder -> do
      atomically $ do
        writeTVar
          coordination.coordinationOracleControlIndex
          (oracleGreatestControlIndex initial)
        publishMembership initial
        publishVoters initial
      loop recorder initial emptyOracleCheckpointCache
  where
    loop recorder state !cachedCheckpoint = do
      command <- atomically (readTQueue commands)
      case command of
        CaptureOracleCheckpointOwner result -> do
          let through = oracleGreatestControlIndex state
              (nextCache, captured, work) =
                captureOracleCheckpoint
                  through
                  (encodeLocalRuntimeCheckpoint (oracleCheckpointBytes state) through)
                  cachedCheckpoint
          -- Materialize bytes on this owner before retaining or publishing them;
          -- the cache never keeps a serialization closure over a kernel state.
          ByteString.length (capturedOracleCheckpointBytes captured) `seq` pure ()
          recordOracleWorkBatch
            recorder
            [("checkpoint_capture", "captured"), ("checkpoint_encoding", case work of OracleCheckpointEncoded -> "encoded"; OracleCheckpointReused -> "reused")]
            state
          atomically (putTMVar result captured)
          loop recorder state nextCache
        InstallOracleCheckpointOwner expectedConfiguration bytes result -> case decodeOracleCheckpoint genesis bytes of
          Left problem -> do
            recordOracleWork recorder "checkpoint_install" "decode_rejected" state
            let failure = RuntimeEssentialChildFailed "oracle-checkpoint" (show problem)
            signalRuntimeFailure scope failure
            atomically (putTMVar result (Left failure))
          Right successor
            | oracleStateDigestMode successor /= digestMode -> do
                recordOracleWork recorder "checkpoint_install" "digest_mode_rejected" state
                let failure = RuntimeEssentialChildFailed "oracle-checkpoint" "checkpoint diagnostic digest mode differs from runtime configuration"
                signalRuntimeFailure scope failure
                atomically (putTMVar result (Left failure))
            | let configuration = oracleVoterConfiguration successor,
              expectedConfiguration /= (voterConfigurationNativeRef configuration, voterConfigurationNative configuration) -> do
                recordOracleWork recorder "checkpoint_install" "configuration_rejected" state
                let failure = RuntimeEssentialChildFailed "oracle-checkpoint" "native and Oracle checkpoint configurations differ"
                signalRuntimeFailure scope failure
                atomically (putTMVar result (Left failure))
            | otherwise -> do
                recordOracleWork recorder "checkpoint_install" "installed" successor
                let retention = receiptRetention successor
                    receipts = oracleRequestReceipts successor
                length receipts
                  `seq` retention
                  `seq` atomically
                    ( do
                        writeTVar coordination.coordinationOracleControlIndex (oracleGreatestControlIndex successor)
                        publishMembership successor
                        putTMVar result (Right (oracleGreatestControlIndex successor, receipts, retention))
                    )
                loop recorder successor emptyOracleCheckpointCache
        QueryOracleVoterChangeOwner changeId result -> do
          atomically (putTMVar result (oracleVoterChange changeId state))
          loop recorder state cachedCheckpoint
        ApplyOracleConfigurationOwner entry result ->
          case applyCommittedOracleConfiguration entry state of
            Left fault -> do
              recordOracleWork recorder "native_configuration" "invariant_fault" state
              signalRuntimeFailure scope (RuntimeOracleInvariantFault fault)
              atomically (putTMVar result (Left fault))
            Right (successor, applied) -> do
              recordOracleWorkBatch recorder [("native_configuration", "applied"), ("state_advance", "native_configuration")] successor
              let retention = receiptRetention successor
              retention `seq` pure ()
              atomically $ do
                writeTVar coordination.coordinationOracleControlIndex (oracleGreatestControlIndex successor)
                publishMembership successor
                putTMVar result (Right (applied, retention))
              loop recorder successor (advanceOracleCheckpointCache (oracleGreatestControlIndex successor) cachedCheckpoint)
        StopOracleOwner -> recordOracleWork recorder "owner" "stopped" state
        BarrierOracleOwner done -> do
          atomically (putTMVar done ())
          loop recorder state cachedCheckpoint
        PreflightOracleOwner canonical result -> do
          let envelope = canonicalOracleEnvelopeValue canonical
              observedIndex = oracleGreatestControlIndex state
              preflight =
                case oracleSubmissionPreflightBlocker envelope state of
                  Nothing -> OracleSubmissionMayProceed observedIndex
                  Just decision ->
                    OracleSubmissionMustDefer
                      decision
                      observedIndex
          recordOracleWork recorder ("preflight_" <> oracleCommandWorkTag (oracleEnvelopeCommand envelope)) (case preflight of OracleSubmissionMayProceed {} -> "may_proceed"; OracleSubmissionMustDefer {} -> "must_defer") state
          atomically (putTMVar result preflight)
          loop recorder state cachedCheckpoint
        ApplyOracleOwner canonical result -> do
          let envelope = canonicalOracleEnvelopeValue canonical
          case stepOracle envelope state of
            Left fault -> do
              recordOracleWork recorder ("apply_" <> oracleCommandWorkTag (oracleEnvelopeCommand envelope)) "invariant_fault" state
              signalRuntimeFailure scope (RuntimeOracleInvariantFault fault)
              atomically (putTMVar result (Left fault))
            Right (successor, outcome, effects) -> do
              recordOracleWorkBatch
                recorder
                ( ("apply_" <> oracleCommandWorkTag (oracleEnvelopeCommand envelope), oracleOutcomeWorkTag outcome)
                    : [("state_advance_" <> oracleCommandWorkTag (oracleEnvelopeCommand envelope), oracleOutcomeWorkTag outcome) | oracleGreatestControlIndex successor /= oracleGreatestControlIndex state]
                )
                successor
              let retention = receiptRetention successor
              retention `seq` pure ()
              -- Adopt the successor before making outcome or batch visible.
              recordRuntimeEvent
                recording
                (RuntimeOracleInputInstalled (oracleEnvelopeRequestId envelope) outcome)
              recordRuntimeEvent
                recording
                (RuntimeOracleBatchHanded (length (oracleEffects effects)))
              atomically
                ( do
                    -- Publish only the owner-issued prefix token, in the same
                    -- transaction that exposes the installed successor.
                    writeTVar
                      coordination.coordinationOracleControlIndex
                      (oracleGreatestControlIndex successor)
                    publishMembership successor
                    putTMVar
                      result
                      (Right (OracleOwnerResult outcome effects retention))
                )
              loop recorder successor (advanceOracleCheckpointCache (oracleGreatestControlIndex successor) cachedCheckpoint)
    receiptRetention state =
      OracleReceiptRetention
        { receiptRetirementFrontiers = Map.fromList (oracleReceiptRetirementProgresses state),
          receiptRetiredHomes = Set.fromList (oracleRetiredHeralds state)
        }
    publishMembership state = do
      status <- readTVar statusCell
      let history = oracleCheckedMembershipHistory state
          catalogue = oracleHeraldCatalogue state
          voterConfiguration = oracleVoterConfiguration state
          registrations = oracleReplicaRegistrations state
          pending = oraclePendingVoterChange state
          -- Force these constant-time scalar queries on this exclusive owner;
          -- the status cell must not retain a thunk closing over kernel state.
          !labelRetired = oracleLabelRetiredThrough state
          !completedCount = oracleCompletedWorkflowCount state
          labelLifetimeChanged = labelRetired /= status.statusLabelRetiredThrough || completedCount /= status.statusCompletedWorkflowCount
          membershipChanged = history /= status.statusMembershipHistory || catalogue /= status.statusHeraldCatalogue
          votersChanged =
            voterConfiguration /= status.statusVoterConfiguration
              || registrations /= status.statusReplicaRegistrations
              || pending /= status.statusPendingVoterChange
      when
        (membershipChanged || votersChanged || labelLifetimeChanged)
        ( writeTVar
            statusCell
            status
              { statusLabelRetiredThrough = labelRetired,
                statusCompletedWorkflowCount = completedCount,
                statusMembershipHistory = history,
                statusHeraldCatalogue = catalogue,
                statusActiveHeralds = if membershipChanged then activeHeraldIdentities catalogue history else status.statusActiveHeralds,
                statusVoterConfiguration = voterConfiguration,
                statusReplicaRegistrations = registrations,
                statusPendingVoterChange = pending
              }
        )
      when (votersChanged || history /= status.statusMembershipHistory) (publishVoters state)

    publishVoters state = do
      let pending = oraclePendingVoterChange state
          prepared = case pending of
            Nothing -> Nothing
            Just change -> case voterChangePhase change of
              VoterChangePreparing -> Just (prepare change JointConfigurationStage)
              VoterChangeJointCommitted -> Just (prepare change FinalConfigurationStage)
              _ -> Nothing
          prepare change stage =
            either
              (error . ("retained voter intent could not prepare metadata: " <>) . show)
              id
              (prepareOracleConfiguration (voterChangeId change) stage state)
      -- Encode metadata on this owner before handing concrete facts to Raft.
      -- A queued thunk must not defer an Oracle-state query to another owner.
      case prepared of
        Nothing -> pure ()
        Just (configuration, metadata) -> configuration `seq` metadata `seq` pure ()
      writeTQueue
        raftCommands
        ( UpdateOracleRaftProjection
            OracleRaftProjection
              { projectedVoterConfiguration = oracleVoterConfiguration state,
                projectedReplicaRegistrations = oracleReplicaRegistrations state,
                projectedReplicationTargets = oracleReplicaReplicationTargets state,
                projectedVoterChange = pending,
                projectedConfigurationProposal = prepared
              }
        )

-- No other runtime module retains the private Oracle state for diagnostics.
type Recorder = Maybe (IORef (OracleState, OracleWorkCounts))

-- The reference never leaves the owner. Its finalizer runs on that same owner,
-- including asynchronous cancellation, while the kernel remains pure. The
-- callback reads only the already-shared published control coordinate.
withOracleWorkCounts :: OracleState -> IO ControlIndex -> (Recorder -> IO value) -> IO value
withOracleWorkCounts initial publishedIndex action = do
  directory <- lookupEnv "ECLIPS_WORK_COUNTS_DIR"
  case directory of
    Nothing -> action Nothing
    Just "" -> action Nothing
    Just destination -> do
      stamp <- getMonotonicTimeNSec
      reference <- newIORef (initial, countOracleWork "owner" "initialized" emptyOracleWorkCounts)
      let markExit result = modifyIORef' reference $ \(state, counts) ->
            let updated = countOracleWork "owner_exit" result counts
             in updated `seq` (state, updated)
      ((action (Just reference) <* markExit "returned") `onException` markExit "exception")
        `finally` writeSummary (destination <> "/oracle-" <> show stamp <> ".json") publishedIndex reference

recordOracleWork :: Recorder -> String -> String -> OracleState -> IO ()
recordOracleWork recorder family result = recordOracleWorkBatch recorder [(family, result)]

recordOracleWorkBatch :: Recorder -> [(String, String)] -> OracleState -> IO ()
recordOracleWorkBatch Nothing _ _ = pure ()
recordOracleWorkBatch (Just reference) observations state =
  modifyIORef' reference $ \(_, counts) ->
    let updated = foldl' (\previous (family, result) -> countOracleWork family result previous) counts observations
     in updated `seq` (state, updated)

writeSummary :: FilePath -> IO ControlIndex -> IORef (OracleState, OracleWorkCounts) -> IO ()
writeSummary path publishedIndex reference = do
  (state, counts) <- readIORef reference
  published <- publishedIndex
  let computed = oracleGreatestControlIndex state
      complete = oracleWorkSnapshotComplete computed published counts
      sections = oracleStateDiagnosticSections state
      object pairs = "{" <> intercalate "," pairs <> "}"
      numbers values = object [show name <> ":" <> show number | (name, number) <- values]
      rows =
        [ object ["\"family\":" <> show family, "\"result\":" <> show result, "\"count\":" <> show count]
        | (family, result, count) <- oracleWorkRows counts
        ]
      output =
        object
          [ "\"owner\":\"oracle\"",
            "\"snapshot_complete\":" <> (if complete then "true" else "false"),
            "\"exit_kind\":" <> show (if ("owner", "stopped", 1) `elem` oracleWorkRows counts then "stop_command" else if ("owner_exit", "exception", 1) `elem` oracleWorkRows counts then "exception" else "returned"),
            "\"state_digest_mode\":" <> show (show (oracleStateDigestMode state)),
            "\"control_index\":" <> show (controlIndexWord64 computed),
            "\"published_control_index\":" <> show (controlIndexWord64 published),
            "\"counts\":[" <> intercalate "," rows <> "]",
            "\"cardinalities\":" <> numbers (oracleStateDiagnosticCardinalities state),
            "\"canonical_sections_bytes\":" <> numbers sections,
            "\"canonical_total_bytes\":" <> show (sum (fmap snd sections))
          ]
  written <- try (writeFile path (output <> "\n"))
  case written of
    Right () -> pure ()
    Left (exception :: IOException) -> do
      -- Diagnostic output cannot replace the owner's original exit or fault,
      -- including when stderr itself has already been closed during shutdown.
      _ <- try (hPutStrLn stderr ("Oracle work-count output failed for " <> path <> ": " <> show exception)) :: IO (Either IOException ())
      pure ()
