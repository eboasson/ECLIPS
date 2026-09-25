-- | One scoped worker lease for an EORC connection's uncorrelated watch lane.
module Eclips.Oracle.Runtime.Internal.WatchWorker
  ( WatchWorkerOwner,
    WatchWorkerCompletion,
    newWatchWorkerOwnerIO,
    startWatchWorkerOrReject,
    awaitWatchWorkerCompletion,
    stopWatchWorkerOwner,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIOWithUnmask,
    killThread,
  )
import Control.Concurrent.MVar
  ( MVar,
    modifyMVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
    tryReadMVar,
    withMVar,
  )
import Control.Exception
  ( finally,
    mask_,
  )
import Control.Monad (void)
import Data.Word (Word64)

-- | The uncorrelated EORC response vocabulary permits at most one outstanding
-- watch per physical connection. The state lock makes worker installation,
-- completion, and connection shutdown one explicit ownership protocol.
data WatchWorkerOwner = WatchWorkerOwner
  { responseLock :: MVar (),
    stateCell :: MVar WatchWorkerState
  }

data WatchWorkerState
  = WatchWorkerIdle Word64
  | WatchWorkerActive Word64 ThreadId (MVar ()) (MVar ())
  | WatchWorkerClosed (Maybe (MVar ()))

-- | Evidence that the accepted worker has released its ownership lease. A
-- caller may use this to sequence a later watch without polling.
newtype WatchWorkerCompletion = WatchWorkerCompletion (MVar ())

newWatchWorkerOwnerIO :: MVar () -> IO WatchWorkerOwner
newWatchWorkerOwnerIO responseLock =
  WatchWorkerOwner responseLock <$> newMVar (WatchWorkerIdle 0)

-- | Start one worker or terminate the physical lane when another watch is still
-- outstanding, the connection owner has closed, or an accepted worker exits
-- without publishing a response. The termination action must be idempotent.
-- The start gate prevents a fast worker from completing before its active lease
-- has been installed in the owner state. The worker must invoke its supplied
-- callback while holding the owner's response lock, immediately after
-- successfully publishing its reply. A sequential request which arrives as
-- that worker is winding down can then join its completion instead of being
-- mistaken for a duplicate.
startWatchWorkerOrReject ::
  WatchWorkerOwner ->
  IO () ->
  (IO () -> IO ()) ->
  IO (Maybe WatchWorkerCompletion)
startWatchWorkerOrReject owner terminateLane action =
  mask_
    ( withMVar owner.responseLock $ \() ->
        startOrJoinCompletion
    )
  where
    startOrJoinCompletion = do
      outcome <-
        modifyMVar owner.stateCell $ \case
          WatchWorkerIdle generation -> do
            start <- newEmptyMVar
            done <- newEmptyMVar
            responsePublished <- newEmptyMVar
            worker <-
              forkIOWithUnmask $ \unmask ->
                ( takeMVar start
                    >> unmask
                      ( action
                          (void (tryPutMVar responsePublished ()))
                      )
                )
                  `finally` releaseWatchWorker
                    owner
                    terminateLane
                    generation
                    done
                    responsePublished
            putMVar start ()
            pure
              ( WatchWorkerActive generation worker done responsePublished,
                WatchWorkerAccepted (WatchWorkerCompletion done)
              )
          active@(WatchWorkerActive _ _ done responsePublished) -> do
            published <- tryReadMVar responsePublished
            pure
              ( active,
                case published of
                  Just () -> WatchWorkerFinishing done
                  Nothing -> WatchWorkerRejected
              )
          closed@WatchWorkerClosed {} -> pure (closed, WatchWorkerRejected)
      case outcome of
        WatchWorkerAccepted completion -> pure (Just completion)
        WatchWorkerFinishing done -> readMVar done >> startOrJoinCompletion
        WatchWorkerRejected -> terminateLane >> pure Nothing

awaitWatchWorkerCompletion :: WatchWorkerCompletion -> IO ()
awaitWatchWorkerCompletion (WatchWorkerCompletion done) =
  void (readMVar done)

-- | Permanently close the owner, cancel its one active worker, and wait until
-- the worker's masked release path has settled. No racing start can install a
-- successor after the closed state becomes visible.
stopWatchWorkerOwner :: WatchWorkerOwner -> IO ()
stopWatchWorkerOwner owner = mask_ $ do
  disposition <-
    modifyMVar owner.stateCell $ \case
      WatchWorkerActive _ worker done _ ->
        pure (WatchWorkerClosed (Just done), CancelWatchWorker worker done)
      WatchWorkerIdle _ -> pure (WatchWorkerClosed Nothing, NoWatchWorker)
      closed@(WatchWorkerClosed done) ->
        pure (closed, maybe NoWatchWorker AwaitWatchWorker done)
  case disposition of
    NoWatchWorker -> pure ()
    CancelWatchWorker worker done -> do
      killThread worker
      void (readMVar done)
    AwaitWatchWorker done ->
      void (readMVar done)

releaseWatchWorker ::
  WatchWorkerOwner ->
  IO () ->
  Word64 ->
  MVar () ->
  MVar () ->
  IO ()
releaseWatchWorker owner terminateLane generation done responsePublished = mask_ $ do
  published <- maybe False (const True) <$> tryReadMVar responsePublished
  mustTerminate <-
    modifyMVar owner.stateCell $ \case
      WatchWorkerActive current _ _ _
        | current == generation ->
            pure
              ( if published
                  then WatchWorkerIdle (generation + 1)
                  else WatchWorkerClosed (Just done),
                not published
              )
      other -> pure (other, not published)
  (if mustTerminate then terminateLane else pure ())
    `finally` void (tryPutMVar done ())

data WatchWorkerStart
  = WatchWorkerAccepted WatchWorkerCompletion
  | WatchWorkerFinishing (MVar ())
  | WatchWorkerRejected

data WatchWorkerStop
  = NoWatchWorker
  | CancelWatchWorker ThreadId (MVar ())
  | AwaitWatchWorker (MVar ())
