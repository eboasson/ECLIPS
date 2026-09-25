-- | Scoped listener and connection-worker ownership.
module Eclips.Oracle.Runtime.Internal.TCP.Server
  ( TcpServer,
    startTcpServer,
    stopTcpServer,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIOWithUnmask,
    killThread,
    myThreadId,
  )
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
  )
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )
import Control.Exception
  ( IOException,
    finally,
    mask,
    mask_,
    onException,
    try,
  )
import Control.Monad
  ( forM_,
    void,
  )
import Eclips.Oracle.Runtime.Internal.TCP.Socket
  ( closeSocketQuietly,
    prepareConnectedTcpSocket,
  )
import Network.Socket
  ( Socket,
    accept,
  )

data TcpWorker = TcpWorker ThreadId (MVar ())

data TcpServer = TcpServer
  { tcpServerListener :: Socket,
    tcpServerAccepting :: TVar Bool,
    tcpServerAcceptThread :: ThreadId,
    tcpServerAcceptDone :: MVar (),
    tcpServerWorkers :: TVar [TcpWorker],
    tcpServerStopDone :: MVar ()
  }

startTcpServer :: Socket -> (IOException -> IO ()) -> (Socket -> IO ()) -> IO TcpServer
startTcpServer listener reportAcceptFailure handler = mask_ $ do
  accepting <- newTVarIO True
  workers <- newTVarIO []
  acceptDone <- newEmptyMVar
  stopDone <- newEmptyMVar
  start <- newEmptyMVar
  acceptThread <-
    forkIOWithUnmask $ \unmask -> do
      takeMVar start
      unmask (acceptLoop accepting workers)
        `finally` putMVar acceptDone ()
  let server =
        TcpServer
          { tcpServerListener = listener,
            tcpServerAccepting = accepting,
            tcpServerAcceptThread = acceptThread,
            tcpServerAcceptDone = acceptDone,
            tcpServerWorkers = workers,
            tcpServerStopDone = stopDone
          }
  putMVar start ()
  pure server
  where
    acceptLoop accepting workers = do
      keepGoing <- readTVarIO accepting
      if not keepGoing
        then pure ()
        else do
          accepted <- try @IOException (accept listener)
          case accepted of
            Left exception -> do
              stillAccepting <- readTVarIO accepting
              if stillAccepting
                then reportAcceptFailure exception
                else pure ()
            Right (connection, _) -> do
              prepared <- try @IOException (prepareConnectedTcpSocket connection)
              case prepared of
                -- Socket-option failure retires this lane, not the listener.
                Left _ -> acceptLoop accepting workers
                Right () ->
                  mask $ \_ -> do
                    done <- newEmptyMVar
                    start <- newEmptyMVar
                    let finish = do
                          closeSocketQuietly connection
                          finished <- myThreadId
                          -- Only live descriptor owners belong to the scope.
                          -- A stop which already captured this worker still
                          -- joins its completion witness; a later stop may
                          -- omit it after socket ownership has ended.
                          atomically (modifyTVar' workers (removeWorker finished))
                          putMVar done ()
                    thread <-
                      ( forkIOWithUnmask $ \unmask ->
                          ( do
                              permitted <- takeMVar start
                              if permitted
                                then void (try @IOException (unmask (handler connection)))
                                else pure ()
                          )
                            `finally` finish
                      )
                        `onException` closeSocketQuietly connection
                    registered <- atomically $ do
                      active <- readTVar accepting
                      if active
                        then modifyTVar' workers (TcpWorker thread done :) >> pure True
                        else pure False
                    putMVar start registered
                    if registered
                      then acceptLoop accepting workers
                      else void (readMVar done)

-- Force deletion throughout the small live registry, rather than retaining a
-- chain of unevaluated filters behind another connection's live head.
removeWorker :: ThreadId -> [TcpWorker] -> [TcpWorker]
removeWorker finished workers =
  let retained = filter (\(TcpWorker worker _) -> worker /= finished) workers
   in length retained `seq` retained

stopTcpServer :: TcpServer -> IO ()
stopTcpServer server = mask_ $ do
  wasAccepting <- atomically $ do
    accepting <- readTVar server.tcpServerAccepting
    writeTVar server.tcpServerAccepting False
    pure accepting
  if not wasAccepting
    then void (readMVar server.tcpServerStopDone)
    else
      ( do
          -- Interrupt and join every descriptor user before releasing the
          -- listener or accepted descriptors.  Closing a descriptor while a
          -- different thread is registering its read wait can leave a stale
          -- event-manager entry which later poisons an unrelated reused fd.
          killThread server.tcpServerAcceptThread
          void (readMVar server.tcpServerAcceptDone)
          closeSocketQuietly server.tcpServerListener
          workers <- readTVarIO server.tcpServerWorkers
          forM_ workers $ \(TcpWorker thread done) -> do
            killThread thread
            void (readMVar done)
      )
        `finally` putMVar server.tcpServerStopDone ()
