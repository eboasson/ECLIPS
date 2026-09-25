-- | Private finite-run recording owner.
module Eclips.Oracle.Runtime.Internal.Recording
  ( runRecordingOwner,
    recordRuntimeEvent,
    snapshotRuntimeRecording,
  )
where

import Control.Concurrent.STM
  ( TMVar,
    TQueue,
    atomically,
    newEmptyTMVar,
    putTMVar,
    readTMVar,
    readTQueue,
    tryPutTMVar,
    writeTQueue,
  )
import Control.Exception (finally)
import Data.IORef
  ( modifyIORef',
    newIORef,
    readIORef,
  )
import Eclips.Oracle.Runtime.Internal.Types
  ( OracleRuntimeEvent,
    OracleRuntimeRecordingMode (..),
    RecordingCommand (..),
  )

runRecordingOwner ::
  OracleRuntimeRecordingMode ->
  (OracleRuntimeEvent -> IO ()) ->
  TQueue RecordingCommand ->
  TMVar [OracleRuntimeEvent] ->
  IO ()
runRecordingOwner mode sink commands finalRecording = do
  reversedRef <- case mode of
    OracleDiagnosticsOnly -> pure Nothing
    OracleCaptureHistory -> Just <$> newIORef []
  loop reversedRef
    `finally` do
      reversed <- retained reversedRef
      _ <- atomically (tryPutTMVar finalRecording (reverse reversed))
      pure ()
  where
    retained = maybe (pure []) readIORef
    loop reversedRef = do
      command <- atomically (readTQueue commands)
      case command of
        AppendRuntimeRecord event -> do
          sink event
          mapM_ (\reference -> modifyIORef' reference (event :)) reversedRef
          loop reversedRef
        SnapshotRuntimeRecording result -> do
          reversed <- retained reversedRef
          atomically (putTMVar result (reverse reversed))
          loop reversedRef
        StopRuntimeRecording -> pure ()

recordRuntimeEvent :: TQueue RecordingCommand -> OracleRuntimeEvent -> IO ()
recordRuntimeEvent commands event =
  -- In particular, force strict batch counts before queueing so recording an
  -- integer cannot retain the effect batch used to compute it.
  event `seq` atomically (writeTQueue commands (AppendRuntimeRecord event))

snapshotRuntimeRecording :: TQueue RecordingCommand -> IO [OracleRuntimeEvent]
snapshotRuntimeRecording commands = do
  result <- atomically newEmptyTMVar
  atomically (writeTQueue commands (SnapshotRuntimeRecording result))
  atomically (readTMVar result)
