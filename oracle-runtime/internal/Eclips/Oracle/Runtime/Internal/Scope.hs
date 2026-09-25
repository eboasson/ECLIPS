-- | Minimal scoped child registry used without an effects or async dependency.
module Eclips.Oracle.Runtime.Internal.Scope
  ( newRuntimeScope,
    forkRuntimeChild,
    beginRuntimeScopeClose,
    claimRuntimeScopeClose,
    abortRuntimeScope,
    joinRuntimeScope,
    runtimeScopeObservedFailure,
    signalRuntimeFailure,
  )
where

import Control.Concurrent
  ( forkIOWithUnmask,
    killThread,
  )
import Control.Concurrent.MVar
  ( newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
  )
import Control.Concurrent.STM
  ( STM,
    atomically,
    modifyTVar',
    newEmptyTMVar,
    newTVar,
    readTMVar,
    readTVar,
    tryPutTMVar,
    tryReadTMVar,
    writeTVar,
  )
import Control.Exception
  ( AsyncException,
    SomeException,
    displayException,
    fromException,
    mask_,
    try,
  )
import Control.Monad
  ( forM,
    forM_,
    unless,
    void,
  )
import Eclips.Oracle.Runtime.Internal.Types
  ( OracleRuntimeFailure (RuntimeEssentialChildFailed),
    RuntimeChild (..),
    RuntimeScope (..),
  )

newRuntimeScope :: STM RuntimeScope
newRuntimeScope = RuntimeScope <$> newTVar False <*> newEmptyTMVar <*> newTVar []

forkRuntimeChild :: RuntimeScope -> String -> IO () -> IO ()
forkRuntimeChild scope name action = mask_ $ do
  done <- newEmptyMVar
  start <- newEmptyMVar
  thread <-
    forkIOWithUnmask $ \unmask -> do
      permitted <- takeMVar start
      if not permitted
        then putMVar done (Right ())
        else do
          result <- try @SomeException (unmask action)
          normalized <- normalizeChildResult scope result
          putMVar done normalized
          case normalized of
            Right () -> pure ()
            Left message -> do
              _ <-
                atomically
                  ( tryPutTMVar
                      scope.runtimeScopeFailure
                      (RuntimeEssentialChildFailed name message)
                  )
              pure ()
  registered <- atomically $ do
    closing <- readTVar scope.runtimeScopeClosing
    if closing
      then pure False
      else do
        modifyTVar'
          scope.runtimeScopeChildren
          (RuntimeChild name thread done :)
        pure True
  putMVar start registered
  unless registered (void (readMVar done))

normalizeChildResult ::
  RuntimeScope ->
  Either SomeException () ->
  IO (Either String ())
normalizeChildResult _ (Right ()) = pure (Right ())
normalizeChildResult scope (Left exception) = do
  closing <- atomically (readTVar scope.runtimeScopeClosing)
  pure
    ( if closing && isAsyncException exception
        then Right ()
        else Left (displayException exception)
    )

isAsyncException :: SomeException -> Bool
isAsyncException exception = case fromException @AsyncException exception of
  Just _ -> True
  Nothing -> False

beginRuntimeScopeClose :: RuntimeScope -> IO ()
beginRuntimeScopeClose scope = atomically (writeTVar scope.runtimeScopeClosing True)

claimRuntimeScopeClose :: RuntimeScope -> STM Bool
claimRuntimeScopeClose scope = do
  closing <- readTVar scope.runtimeScopeClosing
  if closing
    then pure False
    else writeTVar scope.runtimeScopeClosing True >> pure True

abortRuntimeScope :: RuntimeScope -> IO (Maybe OracleRuntimeFailure)
abortRuntimeScope scope = do
  beginRuntimeScopeClose scope
  children <- atomically (readTVar scope.runtimeScopeChildren)
  forM_ children (killThread . runtimeChildThread)
  joinRuntimeChildren scope children

joinRuntimeScope :: RuntimeScope -> IO (Maybe OracleRuntimeFailure)
joinRuntimeScope scope = do
  children <- atomically (readTVar scope.runtimeScopeChildren)
  joinRuntimeChildren scope children

joinRuntimeChildren :: RuntimeScope -> [RuntimeChild] -> IO (Maybe OracleRuntimeFailure)
joinRuntimeChildren scope children = do
  outcomes <- forM children (readMVar . runtimeChildDone)
  observed <- atomically (tryReadTMVar scope.runtimeScopeFailure)
  pure
    ( case observed of
        Just failure -> Just failure
        Nothing -> firstChildFailure children outcomes
    )

firstChildFailure :: [RuntimeChild] -> [Either String ()] -> Maybe OracleRuntimeFailure
firstChildFailure children outcomes = go (zip children outcomes)
  where
    go [] = Nothing
    go ((child, outcome) : rest) = case outcome of
      Right () -> go rest
      Left message -> Just (RuntimeEssentialChildFailed child.runtimeChildName message)

runtimeScopeObservedFailure :: RuntimeScope -> STM OracleRuntimeFailure
runtimeScopeObservedFailure = readTMVar . runtimeScopeFailure

signalRuntimeFailure :: RuntimeScope -> OracleRuntimeFailure -> IO ()
signalRuntimeFailure scope failure = do
  _ <- atomically (tryPutTMVar scope.runtimeScopeFailure failure)
  pure ()
