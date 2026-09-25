-- | Ordered committed-Raft to Oracle/ledger adapter and published-result gate.
module Eclips.Oracle.Runtime.Internal.Adapter
  ( runCommittedEntryAdapter,
  )
where

import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    newEmptyTMVar,
    orElse,
    putTMVar,
    readTMVar,
    readTQueue,
    readTVar,
    writeTQueue,
    writeTVar,
  )
import Control.Monad (foldM)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Eclips.Oracle.Canonical
  ( canonicalOracleEnvelopeValue,
    decodeCanonicalOracleEnvelope,
  )
import Eclips.Oracle.Command (oracleEnvelopeCommand)
import Eclips.Oracle.Effect
  ( OracleEffect (EmitAppliedOracleEntry),
    OracleProtocolDisposition (ConflictingOracleRequestId),
    OracleStepOutcome (..),
    oracleEffects,
  )
import Eclips.Oracle.Identity (OracleClientRequestId, oracleClientRequestHome, oracleClientRequestSequence)
import Eclips.Oracle.Projection (appliedEntryCommand, appliedEntryControlIndex, appliedEntryOracleProgress)
import Eclips.Oracle.Receipt
  ( OracleReceipt,
    OracleRequestRetirement (..),
    oracleReceiptRequestId,
  )
import Eclips.Oracle.Runtime.Internal.Checkpoint (decodeRuntimeCheckpoint)
import Eclips.Oracle.Runtime.Internal.CheckpointCache (capturedOracleCheckpointBytes, capturedOracleCheckpointIndex)
import Eclips.Oracle.Runtime.Internal.Coordination (completeProposalAt)
import Eclips.Oracle.Runtime.Internal.Scope
  ( runtimeScopeObservedFailure,
    signalRuntimeFailure,
  )
import Eclips.Oracle.Runtime.Internal.Types
  ( AdapterCommand (..),
    OracleOwnerCommand (..),
    OracleOwnerResult (..),
    OracleReceiptRetention (..),
    OracleRuntimeFailure (..),
    OracleRuntimeStatus (..),
    OracleSubmitResult (..),
    OracleVoterCheckpoint (..),
    RaftOwnerCommand (..),
    RuntimeCoordination (..),
    RuntimeScope,
    WatchLedgerCommand (..),
  )
import Eclips.Public.Types.ReceiptRetirement qualified as Lifetime
import Eclips.Raft.Checkpoint (raftCheckpointConfiguration, raftCheckpointConfigurationRef, raftCheckpointIndex, raftCheckpointPayload)
import Eclips.Raft.Effect
  ( committedEntryIndex,
    committedEntryPayload,
  )
import Eclips.Raft.Identity
  ( raftLogIndex,
    raftLogIndexWord64,
  )
import Eclips.Raft.Input
  ( RaftEntry (..),
    acknowledgeCommittedEntries,
    acknowledgeRaftCheckpoint,
    checkpointRaftApplication,
  )

runCommittedEntryAdapter ::
  (OracleVoterCheckpoint -> IO ()) ->
  TQueue AdapterCommand ->
  TQueue OracleOwnerCommand ->
  TQueue WatchLedgerCommand ->
  TQueue RaftOwnerCommand ->
  TVar OracleRuntimeStatus ->
  RuntimeCoordination ->
  RuntimeScope ->
  IO ()
runCommittedEntryAdapter checkpoint commands oracleCommands watchCommands raftCommands statusCell coordination scope =
  loop (raftLogIndex 0) Map.empty (OracleReceiptRetention Map.empty Set.empty)
  where
    -- Apply retirement and insertion before adopting the next published view.
    -- Keeping this map cold retains each previous receipt-retirement projection
    -- until a query happens to demand the accumulated publication history.
    loop appliedThrough !published retention = do
      command <- atomically (readTQueue commands)
      case command of
        InstallCommittedRaftCheckpoint snapshot done -> do
          installed <- installSnapshot snapshot
          case installed of
            Left failure -> signalRuntimeFailure scope failure >> atomically (putTMVar done (Left failure))
            Right (nextPublished, nextRetention) -> do
              atomically (putTMVar done (Right ()))
              loop (raftCheckpointIndex snapshot) nextPublished nextRetention
        StopAdapter -> pure ()
        BarrierAdapter done -> do
          atomically (putTMVar done ())
          loop appliedThrough published retention
        QueryPublishedOracleResult request result -> do
          atomically $ do
            status <- readTVar statusCell
            semanticReady <- readTVar coordination.coordinationSemanticReady
            putTMVar result (Map.lookup request published, classifyRetirement retention request, status, semanticReady)
          loop appliedThrough published retention
        ApplyCommittedRaftEntries entries done -> do
          applied <-
            foldM
              applyOne
              (Right (appliedThrough, published, retention))
              (NonEmpty.toList entries)
          case applied of
            Left failure -> do
              signalRuntimeFailure scope failure
              atomically (putTMVar done (Left failure))
            Right (successorCursor, successorPublished, successorRetention) -> do
              captureSnapshot successorCursor
              atomically (putTMVar done (Right ()))
              loop successorCursor successorPublished successorRetention

    captureSnapshot index = do
      oracleResult <- atomically newEmptyTMVar
      atomically (writeTQueue oracleCommands (CaptureOracleCheckpointOwner oracleResult))
      observed <- atomically ((Left <$> runtimeScopeObservedFailure scope) `orElse` (Right <$> readTMVar oracleResult))
      case observed of
        Left _ -> pure ()
        Right captured -> do
          let through = capturedOracleCheckpointIndex captured
          published <- atomically ((.statusAppliedControlIndex) <$> readTVar statusCell)
          if through /= published
            then signalRuntimeFailure scope (RuntimeEssentialChildFailed "oracle-checkpoint" "Oracle and watch checkpoint cuts differ")
            else atomically (writeTQueue raftCommands (StepRaftOwner (checkpointRaftApplication index (capturedOracleCheckpointBytes captured)) Nothing))

    installSnapshot snapshot = case decodeRuntimeCheckpoint (raftCheckpointPayload snapshot) of
      Left problem -> pure (Left (RuntimeEssentialChildFailed "oracle-checkpoint" problem))
      Right (oracle, through, entries) -> do
        oracleResult <- atomically newEmptyTMVar
        atomically (writeTQueue oracleCommands (InstallOracleCheckpointOwner (raftCheckpointConfigurationRef snapshot, raftCheckpointConfiguration snapshot) oracle oracleResult))
        observed <- atomically ((Left <$> runtimeScopeObservedFailure scope) `orElse` (Right <$> readTMVar oracleResult))
        case observed of
          Left failure -> pure (Left failure)
          Right (Left failure) -> pure (Left failure)
          Right (Right (oracleThrough, receipts, retention))
            | oracleThrough /= through -> pure (Left (RuntimeEssentialChildFailed "oracle-checkpoint" "Oracle and watch checkpoint cuts differ"))
            | otherwise -> do
                watchResult <- atomically newEmptyTMVar
                atomically (writeTQueue watchCommands (InstallWatchCheckpoint through entries watchResult))
                watched <- atomically ((Left <$> runtimeScopeObservedFailure scope) `orElse` (Right <$> readTMVar watchResult))
                case watched of
                  Left failure -> pure (Left failure)
                  Right (Left failure) -> pure (Left failure)
                  Right (Right ()) -> do
                    barrier <- atomically newEmptyTMVar
                    atomically $ do
                      writeTQueue raftCommands (StepRaftOwner (acknowledgeRaftCheckpoint (raftCheckpointIndex snapshot)) Nothing)
                      writeTQueue raftCommands (BarrierRaftOwner barrier)
                    acknowledged <- atomically ((Left <$> runtimeScopeObservedFailure scope) `orElse` (Right <$> readTMVar barrier))
                    case acknowledged of
                      Left failure -> pure (Left failure)
                      Right () -> do
                        atomically publishSemanticReadiness
                        pure (Right (Map.fromList [(oracleReceiptRequestId receipt, receipt) | receipt <- receipts], retention))

    applyOne (Left failure) _ = pure (Left failure)
    applyOne (Right (previous, published, retention)) entry
      | raftLogIndexWord64 index /= raftLogIndexWord64 previous + 1 =
          pure
            ( Left
                ( RuntimeEssentialChildFailed
                    "oracle-adapter"
                    "committed Raft entries did not form the next contiguous prefix"
                )
            )
      | otherwise = case committedEntryPayload entry of
          LeaderNoOp -> do
            acknowledged <- acknowledge index
            case acknowledged of
              Left failure -> pure (Left failure)
              Right () -> do
                atomically publishSemanticReadiness
                pure (Right (index, published, retention))
          Application bytes -> applyApplication index bytes published retention
          Configuration configuration _ -> do
            checkpoint (OracleVoterConfigurationCommitted configuration index)
            result <- atomically newEmptyTMVar
            atomically (writeTQueue oracleCommands (ApplyOracleConfigurationOwner entry result))
            observed <- atomically ((Left <$> runtimeScopeObservedFailure scope) `orElse` (Right <$> readTMVar result))
            case observed of
              Left failure -> pure (Left failure)
              Right (Left fault) -> pure (Left (RuntimeOracleInvariantFault fault))
              Right (Right (applied, successorRetention)) -> do
                retained <- retainEntry applied
                case retained of
                  Left failure -> pure (Left failure)
                  Right retainedIndex -> do
                    acknowledged <- acknowledge index
                    case acknowledged of
                      Left failure -> pure (Left failure)
                      Right () -> do
                        publication <- publishLedger retainedIndex
                        case publication of
                          Left failure -> pure (Left failure)
                          Right () -> do
                            atomically publishSemanticReadiness
                            pure (Right (index, reclaimPublished retention successorRetention published, successorRetention))
      where
        index = committedEntryIndex entry

    applyApplication index bytes published retention = case decodeCanonicalOracleEnvelope bytes of
      Left _ -> pure (Left (RuntimeCommittedEnvelopeInvariantFault index))
      Right canonical -> do
        checkpoint (OracleApplicationCommitted (oracleEnvelopeCommand (canonicalOracleEnvelopeValue canonical)) index)
        ownerResult <- applyOracle canonical
        case ownerResult of
          Left failure -> pure (Left failure)
          Right result -> do
            case oracleEffects result.oracleOwnerEffects of
              [EmitAppliedOracleEntry applied] -> checkpoint (OracleApplicationApplied applied index)
              _ -> pure ()
            retained <- retainEffects result
            case retained of
              Left failure -> pure (Left failure)
              Right retainedIndex -> do
                acknowledged <- acknowledge index
                case acknowledged of
                  Left failure -> pure (Left failure)
                  Right () -> do
                    publishedLedger <- publishLedger retainedIndex
                    case publishedLedger of
                      Left failure -> pure (Left failure)
                      Right () -> do
                        let outcome = result.oracleOwnerOutcome
                            successorRetention = result.oracleOwnerReceiptRetention
                            retainedPublished = reclaimPublished retention successorRetention published
                        case publishOutcome successorRetention retainedPublished outcome of
                          Left failure -> pure (Left failure)
                          Right (nextPublished, submitResult) -> do
                            atomically $ do
                              completeProposalAt coordination index submitResult
                              publishSemanticReadiness
                            pure (Right (index, nextPublished, successorRetention))

    publishSemanticReadiness = do
      reserved <- readTVar coordination.coordinationSubmissionReserved
      writeTVar coordination.coordinationSemanticReady (not reserved)

    applyOracle canonical = do
      result <- atomically newEmptyTMVar
      atomically (writeTQueue oracleCommands (ApplyOracleOwner canonical result))
      observed <- atomically ((Left <$> runtimeScopeObservedFailure scope) `orElse` (Right <$> readTMVar result))
      pure $ case observed of
        Left failure -> Left failure
        Right (Left fault) -> Left (RuntimeOracleInvariantFault fault)
        Right (Right value) -> Right value

    retainEffects result =
      case (result.oracleOwnerOutcome, oracleEffects result.oracleOwnerEffects) of
        (OracleCommitted _, [EmitAppliedOracleEntry entry]) -> retainEntry entry
        (OracleDuplicate _, []) -> pure (Right Nothing)
        (OracleDeferred {}, []) -> pure (Right Nothing)
        (OracleDuplicate _, [EmitAppliedOracleEntry entry])
          | Nothing <- appliedEntryCommand entry,
            Just _ <- appliedEntryOracleProgress entry ->
              retainEntry entry
        (OracleProtocolRejected _, []) -> pure (Right Nothing)
        (OracleProtocolRejected _, [EmitAppliedOracleEntry entry])
          | Nothing <- appliedEntryCommand entry,
            Just _ <- appliedEntryOracleProgress entry ->
              retainEntry entry
        (OracleRequestRetired _, []) -> pure (Right Nothing)
        (OracleProgressRetired _ _, [EmitAppliedOracleEntry entry]) -> retainEntry entry
        (OracleProgressRetired _ _, []) -> pure (Right Nothing)
        _ ->
          pure
            ( Left
                ( RuntimeEssentialChildFailed
                    "oracle-adapter"
                    "Oracle outcome/effect shape contradicted the closed transition"
                )
            )

    retainEntry entry = do
      retained <- atomically newEmptyTMVar
      atomically (writeTQueue watchCommands (InsertWatchEntry entry retained))
      observed <- atomically ((Left <$> runtimeScopeObservedFailure scope) `orElse` (Right <$> readTMVar retained))
      pure $ case observed of
        Left failure -> Left failure
        Right retention -> fmap (const (Just (appliedEntryControlIndex entry))) retention

    publishLedger Nothing = pure (Right ())
    publishLedger (Just index) = do
      published <- atomically newEmptyTMVar
      atomically (writeTQueue watchCommands (PublishWatchThrough index published))
      observed <- atomically ((Left <$> runtimeScopeObservedFailure scope) `orElse` (Right <$> readTMVar published))
      pure (either Left id observed)

    acknowledge index = do
      barrier <- atomically newEmptyTMVar
      atomically $ do
        writeTQueue raftCommands (StepRaftOwner (acknowledgeCommittedEntries index) Nothing)
        writeTQueue raftCommands (BarrierRaftOwner barrier)
      observed <- atomically ((Left <$> runtimeScopeObservedFailure scope) `orElse` (Right <$> readTMVar barrier))
      pure $ case observed of
        Left failure -> Left failure
        Right () -> Right ()

publishOutcome ::
  OracleReceiptRetention ->
  Map OracleClientRequestId OracleReceipt ->
  OracleStepOutcome ->
  Either OracleRuntimeFailure (Map OracleClientRequestId OracleReceipt, OracleSubmitResult)
publishOutcome retention published outcome =
  case outcome of
    OracleCommitted receipt ->
      Right
        ( if isNothing (classifyRetirement retention (oracleReceiptRequestId receipt))
            then Map.insert (oracleReceiptRequestId receipt) receipt published
            else published,
          OracleSubmitReceipt receipt
        )
    OracleDuplicate receipt ->
      case Map.lookup (oracleReceiptRequestId receipt) published of
        Just retained
          | retained == receipt -> Right (published, OracleSubmitReceipt retained)
        _ ->
          Left
            ( RuntimeEssentialChildFailed
                "oracle-adapter"
                "duplicate receipt was not previously published"
            )
    OracleDeferred request decision observedIndex ->
      Right (published, OracleSubmitDeferred request decision observedIndex)
    OracleProtocolRejected disposition ->
      case disposition of
        ConflictingOracleRequestId requestId _ _ ->
          case Map.lookup requestId published of
            Nothing ->
              Left
                ( RuntimeEssentialChildFailed
                    "oracle-adapter"
                    "conflict lacked its retained published request"
                )
            Just _ -> Right (published, OracleSubmitConflict disposition)
        _ -> Right (published, OracleSubmitConflict disposition)
    OracleRequestRetired reason -> Right (published, OracleSubmitRetired reason)
    OracleProgressRetired home frontier -> Right (published, OracleSubmitProgressRetired home frontier)

classifyRetirement :: OracleReceiptRetention -> OracleClientRequestId -> Maybe OracleRequestRetirement
classifyRetirement retention request
  | Set.member home retention.receiptRetiredHomes = Just OracleRequestHomeRetired
  | otherwise = do
      frontier <- Map.lookup home retention.receiptRetirementFrontiers
      if Lifetime.receiptIsRetired (oracleClientRequestSequence request) frontier
        then Just (OracleRequestPrefixRetired (maybe 0 id (Lifetime.receiptRetirementHighWater frontier)))
        else Nothing
  where
    home = oracleClientRequestHome request

reclaimPublished ::
  OracleReceiptRetention ->
  OracleReceiptRetention ->
  Map OracleClientRequestId OracleReceipt ->
  Map OracleClientRequestId OracleReceipt
reclaimPublished previous successor published
  | previous == successor = published
  | otherwise = Map.filterWithKey (\request _ -> isNothing (classifyRetirement successor request)) published
