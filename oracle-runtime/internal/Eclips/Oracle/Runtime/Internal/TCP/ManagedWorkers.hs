-- | Join witnesses for the workers of currently owned resources. The caller
-- fences an owner's admission before retiring it; a finished witness is kept
-- until that join, and no per-retired-owner history remains afterwards.
module Eclips.Oracle.Runtime.Internal.TCP.ManagedWorkers
  ( ManagedWorkers,
    newManagedWorkersIO,
    managedWorkerOwners,
    startManagedWorker,
    retireManagedWorkers,
  )
where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (finally, mask_)
import Control.Monad (forM_, void, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)

data ManagedThread = ManagedThread ThreadId (MVar ())

newtype ManagedWorkers owner = ManagedWorkers (TVar (Map owner ManagedThread))

newManagedWorkersIO :: IO (ManagedWorkers owner)
newManagedWorkersIO = ManagedWorkers <$> newTVarIO Map.empty

managedWorkerOwners :: ManagedWorkers owner -> STM (Set owner)
managedWorkerOwners (ManagedWorkers workers) = Map.keysSet <$> readTVar workers

-- | Admission and registration share one transaction. The completion finalizer
-- encloses the start gate as well: retirement can interrupt the child as soon
-- as its witness is registered, before the parent has released that gate.
startManagedWorker :: (Ord owner) => ManagedWorkers owner -> owner -> STM Bool -> IO () -> IO ()
startManagedWorker (ManagedWorkers workers) owner admit action = mask_ $ do
  start <- newEmptyMVar
  done <- newEmptyMVar
  thread <- forkIOWithUnmask $ \unmask ->
    (takeMVar start >>= \permitted -> when permitted (unmask action))
      `finally` putMVar done ()
  registered <- atomically $ do
    permitted <- admit
    current <- readTVar workers
    if permitted && Map.notMember owner current
      then writeTVar workers (Map.insert owner (ManagedThread thread done) current) >> pure True
      else pure False
  putMVar start registered
  when (not registered) (void (readMVar done))

-- | The caller has already fenced every selected owner. Keep all witnesses
-- visible until their workers have joined, then release the exact witnesses.
-- Concurrent duplicate retirement is harmless and cannot remove a successor.
retireManagedWorkers :: (Ord owner) => ManagedWorkers owner -> (owner -> Bool) -> IO ()
retireManagedWorkers (ManagedWorkers workers) selected = mask_ $ do
  retiring <- atomically (Map.filterWithKey (\owner _ -> selected owner) <$> readTVar workers)
  forM_ retiring $ \(ManagedThread thread done) -> do
    killThread thread
    void (readMVar done)
  atomically $ do
    current <- readTVar workers
    let retain owner witness@(ManagedThread thread _) = case Map.lookup owner retiring of
          Just (ManagedThread retiredThread _) | thread == retiredThread -> Nothing
          _ -> Just witness
    writeTVar workers (Map.mapMaybeWithKey retain current)
