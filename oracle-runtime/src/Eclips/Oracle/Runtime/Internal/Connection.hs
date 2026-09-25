-- | Narrow queue offers used by physical ERFT workers.
module Eclips.Oracle.Runtime.Internal.Connection
  ( offerRaftRequestInput,
    offerRaftResponse,
    awaitOracleRuntimeHelloStatus,
    readOracleRuntimeStatus,
    queryOracleRuntimeHealth,
  )
where

import Control.Concurrent.STM
  ( STM,
    atomically,
    check,
    newEmptyTMVarIO,
    orElse,
    readTMVar,
    readTVar,
    retry,
    writeTQueue,
  )
import Data.ByteString (ByteString)
import Eclips.Oracle.Runtime.Internal.Hello
  ( OracleHelloAvailability (..),
    classifyOracleHelloAvailability,
  )
import Eclips.Oracle.Runtime.Internal.Types
  ( OracleRuntime (..),
    OracleRuntimeHealth,
    OracleRuntimeStatus (..),
    RaftOwnerCommand (QueryRaftHealth, StepRaftOwner),
    RaftReplyRoute (..),
    RuntimeCoordination (..),
  )
import Eclips.Raft.Identity
  ( RaftDispatchGeneration,
    RaftNodeId,
  )
import Eclips.Raft.Input
  ( RaftInput,
    RaftRpc,
    observeRaftResponse,
  )

offerRaftRequestInput ::
  OracleRuntime ->
  RaftInput ByteString ->
  (RaftRpc ByteString -> IO ()) ->
  IO Bool
offerRaftRequestInput runtime input reply =
  atomically $ do
    status <- readTVar runtime.runtimeStatusCell
    if status.statusAccepting
      then do
        writeTQueue
          runtime.runtimeRaftCommands
          (StepRaftOwner input (Just (RaftReplyRoute reply)))
        pure True
      else pure False

offerRaftResponse ::
  OracleRuntime ->
  RaftNodeId ->
  RaftDispatchGeneration ->
  RaftRpc ByteString ->
  IO ()
offerRaftResponse runtime source generation rpc =
  case observeRaftResponse source generation rpc of
    Left _ -> pure ()
    Right input -> atomically $ do
      status <- readTVar runtime.runtimeStatusCell
      if status.statusAccepting
        then
          writeTQueue
            runtime.runtimeRaftCommands
            (StepRaftOwner input Nothing)
        else pure ()

readOracleRuntimeStatus :: OracleRuntime -> IO OracleRuntimeStatus
readOracleRuntimeStatus runtime = atomically (readEffectiveStatus runtime)

-- | A stopped native owner never supplies a cached health observation. The
-- stop branch also wakes callers whose request was queued just before failure.
queryOracleRuntimeHealth :: OracleRuntime -> IO (Maybe OracleRuntimeHealth)
queryOracleRuntimeHealth runtime = do
  done <- newEmptyTMVarIO
  offered <- atomically $ do
    status <- readTVar runtime.runtimeStatusCell
    if status.statusAccepting
      then writeTQueue runtime.runtimeRaftCommands (QueryRaftHealth done) >> pure True
      else pure False
  if not offered
    then pure Nothing
    else
      atomically
        $ (Just <$> readTMVar done)
          `orElse` (readTVar runtime.runtimeStatusCell >>= check . not . statusAccepting >> pure Nothing)

-- | Hold one already-authorized physical Hello until the contacted replica can
-- either serve it or name a distinct leader.  A leaderless cluster therefore
-- owns one parked connection per Herald instead of provoking a reconnect loop.
awaitOracleRuntimeHelloStatus :: OracleRuntime -> STM (Maybe OracleRuntimeStatus)
awaitOracleRuntimeHelloStatus runtime = do
  status <- readEffectiveStatus runtime
  case classifyOracleHelloAvailability
    status.statusAccepting
    status.statusServiceReady
    status.statusNode
    status.statusLeaderHint of
    OracleHelloStopped -> pure Nothing
    OracleHelloWaiting -> retry
    OracleHelloActionable -> pure (Just status)

readEffectiveStatus :: OracleRuntime -> STM OracleRuntimeStatus
readEffectiveStatus runtime = do
  status <- readTVar runtime.runtimeStatusCell
  semanticReady <- readTVar runtime.runtimeCoordination.coordinationSemanticReady
  pure
    status
      { statusServiceReady =
          status.statusAccepting && status.statusServiceReady && semanticReady
      }
