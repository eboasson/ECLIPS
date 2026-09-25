-- | Exclusive owner of the finite contiguous applied-entry ledger.
module Eclips.Oracle.Runtime.Internal.WatchLedgerOwner
  ( runWatchLedgerOwner,
  )
where

import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    putTMVar,
    readTQueue,
    readTVar,
    tryPutTMVar,
    writeTVar,
  )
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Eclips.Domain.Identity
  ( ControlIndex,
    controlIndex,
    controlIndexWord64,
  )
import Eclips.Oracle.Projection
  ( AppliedOracleEntry,
    appliedEntryControlIndex,
  )
import Eclips.Oracle.Runtime.Internal.Recording (recordRuntimeEvent)
import Eclips.Oracle.Runtime.Internal.Scope (signalRuntimeFailure)
import Eclips.Oracle.Runtime.Internal.Types
  ( OracleRuntimeEvent
      ( RuntimeWatchEntryPublished,
        RuntimeWatchEntryRetained
      ),
    OracleRuntimeFailure (..),
    OracleRuntimeStatus (..),
    OracleWatchResult (..),
    RecordingCommand,
    RuntimeScope,
    WatchLedgerCommand (..),
  )
import Eclips.Oracle.Runtime.Internal.WatchServe (publishedOracleWatchRange)

runWatchLedgerOwner ::
  TQueue WatchLedgerCommand ->
  TVar OracleRuntimeStatus ->
  TQueue RecordingCommand ->
  RuntimeScope ->
  IO ()
runWatchLedgerOwner commands status recording scope =
  loop (controlIndex 0) (controlIndex 0) Map.empty Map.empty
  where
    loop retainedThrough publishedThrough entries parked = do
      command <- atomically (readTQueue commands)
      case command of
        CaptureWatchCheckpoint through result
          | through > publishedThrough -> do
              let failure = RuntimeWatchLedgerGap through publishedThrough
              signalRuntimeFailure scope failure
              atomically (putTMVar result (Left failure))
          | otherwise -> do
              let suffix = Map.elems (Map.filterWithKey (\index _ -> index <= through) entries)
              length suffix `seq` atomically (putTMVar result (Right suffix))
              loop retainedThrough publishedThrough entries parked
        InstallWatchCheckpoint through suffix result -> do
          let installed = Map.fromList [(appliedEntryControlIndex entry, entry) | entry <- suffix]
              (released, retained) = Map.partition ((< through) . fst) parked
          atomically $ do
            current <- readTVar status
            writeTVar status current {statusAppliedControlIndex = through}
            mapM_ (\(cursor, waiter) -> tryPutTMVar waiter (OracleWatchEntries (suffixThrough cursor through installed))) (Map.elems released)
            putTMVar result (Right ())
          loop through through installed retained
        StopWatchLedger -> do
          atomically
            ( mapM_
                (\(_, result) -> tryPutTMVar result OracleWatchUnavailable)
                (Map.elems parked)
            )
        BarrierWatchLedger done -> do
          atomically (putTMVar done ())
          loop retainedThrough publishedThrough entries parked
        CancelWatch watchId ->
          loop retainedThrough publishedThrough entries (Map.delete watchId parked)
        ReadWatchSuffix watchId cursor result
          | cursor > publishedThrough -> do
              atomically (putTMVar result (OracleWatchCursorAhead publishedThrough))
              loop retainedThrough publishedThrough entries parked
          | cursor < publishedThrough -> do
              atomically (putTMVar result (OracleWatchEntries (suffixThrough cursor publishedThrough entries)))
              loop retainedThrough publishedThrough entries parked
          | otherwise ->
              loop
                retainedThrough
                publishedThrough
                entries
                (Map.insert watchId (cursor, result) parked)
        PublishWatchThrough index result
          | index /= controlIndex (controlIndexWord64 publishedThrough + 1) ->
              failLedger
                (RuntimeWatchLedgerGap (controlIndex (controlIndexWord64 publishedThrough + 1)) index)
                result
          | index > retainedThrough ->
              failLedger (RuntimeWatchLedgerGap index retainedThrough) result
          | otherwise -> do
              recordRuntimeEvent recording (RuntimeWatchEntryPublished index)
              atomically $ do
                current <- readTVar status
                writeTVar status current {statusAppliedControlIndex = index}
                putTMVar result (Right ())
              let (released, retained) = Map.partition ((< index) . fst) parked
              atomically
                ( mapM_
                    (\(cursor, waiter) -> tryPutTMVar waiter (OracleWatchEntries (suffixThrough cursor index entries)))
                    (Map.elems released)
                )
              loop retainedThrough index entries retained
        InsertWatchEntry entry result -> do
          let index = appliedEntryControlIndex entry
              expected = controlIndex (controlIndexWord64 retainedThrough + 1)
          if index <= retainedThrough
            then case Map.lookup index entries of
              Just retained
                | retained == entry -> do
                    atomically (putTMVar result (Right ()))
                    loop retainedThrough publishedThrough entries parked
              _ -> failLedger (RuntimeWatchLedgerConflict index) result
            else
              if index /= expected
                then failLedger (RuntimeWatchLedgerGap expected index) result
                else do
                  let installed = Map.insert index entry entries
                  -- Retention is private until the adapter acknowledges this
                  -- exact Raft application position.
                  recordRuntimeEvent recording (RuntimeWatchEntryRetained index)
                  atomically (putTMVar result (Right ()))
                  loop index publishedThrough installed parked

    failLedger failure result = do
      signalRuntimeFailure scope failure
      atomically (putTMVar result (Left failure))

suffixThrough :: ControlIndex -> ControlIndex -> Map ControlIndex AppliedOracleEntry -> NonEmpty AppliedOracleEntry
suffixThrough cursor published entries =
  case publishedOracleWatchRange cursor published entries of
    first : rest -> first :| rest
    [] -> error "ledger suffix requested at its greatest cursor"
