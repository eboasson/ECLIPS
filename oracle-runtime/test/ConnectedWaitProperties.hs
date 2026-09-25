{-# LANGUAGE CPP #-}

module ConnectedWaitProperties (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( TMVar,
    atomically,
    newEmptyTMVarIO,
    putTMVar,
    readTMVar,
    tryReadTMVar,
  )
#if !defined(mingw32_HOST_OS)
import Control.Concurrent.STM qualified as STM
#endif
import Control.Exception
  ( IOException,
    bracket,
    try,
  )
import Control.Monad
  ( replicateM_,
    void,
  )
#if !defined(mingw32_HOST_OS)
import Data.ByteString qualified as ByteString
#endif
import Eclips.Oracle.Runtime.Internal.ConnectedWait
  ( ConnectedWaitOutcome (..),
    awaitWhilePeerConnectedAfterRegistration,
  )
#if !defined(mingw32_HOST_OS)
import Eclips.Oracle.Runtime.Internal.TCP.Socket
  ( TcpReadProbe (..),
    probeTcpSocketReadiness,
  )
#endif
import Network.Socket
  ( Family (AF_UNIX),
    Socket,
    SocketType (Stream),
    defaultProtocol,
    socketPair,
  )
import Network.Socket qualified as Socket
#if !defined(mingw32_HOST_OS)
import Network.Socket.ByteString qualified as SocketByteString
#endif
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "connected-wait"
    ( [ testCase "an immediately ready state does not register a socket wait" caseImmediateState,
        testCase "a leaderless connected wait stays silent until readiness" caseLeaderlessUntilReady,
        testCase "shutdown unblocks a connected wait" caseShutdown,
        testCase "parked peer EOF retires every connected wait" caseDisconnectedPeers
      ]
        <> posixTests
    )

posixTests :: [TestTree]
#if !defined(mingw32_HOST_OS)
posixTests =
  [ testCase "watchdog probes before re-registering an idle socket" caseWatchdogRearmsIdleSocket,
    testCase "the POSIX probe is nonblocking and does not consume input" caseProbeDoesNotConsume
  ]
#else
posixTests = []
#endif

caseImmediateState :: IO ()
caseImmediateState = do
  (server, client) <- socketPair AF_UNIX Stream defaultProtocol
  Socket.close server
  state <- newEmptyTMVarIO
  registered <- newEmptyTMVarIO
  atomically (putTMVar state (Just (7 :: Int)))
  observed <-
    awaitWhilePeerConnectedAfterRegistration
      (atomically (putTMVar registered ()))
      (readTMVar state)
      server
  assertEqual "immediate state bypasses the already-closed socket" (ConnectedStateChanged (Just 7)) observed
  registration <- atomically (tryReadTMVar registered)
  assertEqual "the immediate state installed no peer wait" Nothing registration
  closeQuietly client

caseLeaderlessUntilReady :: IO ()
caseLeaderlessUntilReady =
  withSocketPair $ \server _client -> do
    state <- newEmptyTMVarIO
    registered <- newEmptyTMVarIO
    result <-
      startWaitWith
        (atomically (putTMVar registered ()))
        state
        server
    _ <- awaitResult "leaderless wait did not register peer readiness" registered
    pending <- atomically (tryReadTMVar result)
    assertEqual "registered leaderless wait produced no outcome" Nothing pending
    atomically (putTMVar state (Just (7 :: Int)))
    observed <- awaitResult "readiness did not release the connected wait" result
    assertEqual "readiness is the single state outcome" (ConnectedStateChanged (Just 7)) observed

caseShutdown :: IO ()
caseShutdown =
  withSocketPair $ \server _client -> do
    state <- newEmptyTMVarIO
    result <- startWait state server
    atomically (putTMVar state Nothing)
    observed <- awaitResult "shutdown did not release the connected wait" result
    assertEqual "shutdown remains distinct from peer loss" (ConnectedStateChanged Nothing) observed

caseDisconnectedPeers :: IO ()
caseDisconnectedPeers = do
  replicateM_ 32 $ do
    (server, client) <- socketPair AF_UNIX Stream defaultProtocol
    state <- newEmptyTMVarIO :: IO (TMVar (Maybe Int))
    result <- startWait state server
    Socket.close client
    observed <- awaitResult "peer EOF did not release the connected wait" result
    assertEqual "peer EOF wins the parked wait" ConnectedPeerEnded observed
    closeQuietly server

#if !defined(mingw32_HOST_OS)
caseWatchdogRearmsIdleSocket :: IO ()
caseWatchdogRearmsIdleSocket =
  withSocketPair $ \server client -> do
    registrations <- STM.newTVarIO (0 :: Int)
    state <- newEmptyTMVarIO :: IO (TMVar (Maybe Int))
    result <-
      startWaitWith
        (atomically (STM.modifyTVar' registrations (+ 1)))
        state
        server
    rearmed <-
      timeout 2_500_000
        $ atomically
        $ do
          count <- STM.readTVar registrations
          STM.check (count >= 2)
    case rearmed of
      Nothing -> assertFailure "the idle socket was not re-registered after one watchdog probe"
      Just () -> pure ()
    pending <- atomically (tryReadTMVar result)
    assertEqual "an empty direct probe does not retire the connected peer" Nothing pending
    let payload = ByteString.pack [0x45, 0x4f, 0x52, 0x43]
    SocketByteString.sendAll client payload
    observed <- awaitResult "peer input did not release the re-registered wait" result
    assertEqual "peer readability keeps its peer-first outcome" ConnectedPeerEnded observed
    received <- SocketByteString.recv server (ByteString.length payload)
    assertEqual "the rearmed connected wait consumes no protocol byte" payload received

caseProbeDoesNotConsume :: IO ()
caseProbeDoesNotConsume =
  withSocketPair $ \server client -> do
    pending <- timeout 100_000 (probeTcpSocketReadiness server)
    assertEqual "an empty socket probe returns immediately" (Just TcpReadPending) pending
    let payload = ByteString.pack [0x45, 0x4f, 0x52, 0x43]
    SocketByteString.sendAll client payload
    available <- probeTcpSocketReadiness server
    assertEqual "the direct probe detects pending protocol input" TcpReadAvailable available
    received <- SocketByteString.recv server (ByteString.length payload)
    assertEqual "the peek leaves every protocol byte for the framed reader" payload received
#endif

startWait ::
  TMVar (Maybe Int) ->
  Socket ->
  IO (TMVar (ConnectedWaitOutcome (Maybe Int)))
startWait state server = do
  startWaitWith (pure ()) state server

startWaitWith ::
  IO () ->
  TMVar (Maybe Int) ->
  Socket ->
  IO (TMVar (ConnectedWaitOutcome (Maybe Int)))
startWaitWith registered state server = do
  result <- newEmptyTMVarIO
  _ <-
    forkIO $ do
      observed <-
        awaitWhilePeerConnectedAfterRegistration
          registered
          (readTMVar state)
          server
      atomically (putTMVar result observed)
  pure result

awaitResult :: String -> TMVar value -> IO value
awaitResult failure result = do
  observed <- timeout 1_000_000 (atomically (readTMVar result))
  maybe (assertFailure failure) pure observed

withSocketPair :: (Socket -> Socket -> IO result) -> IO result
withSocketPair use =
  bracket
    (socketPair AF_UNIX Stream defaultProtocol)
    (\(left, right) -> closeQuietly left >> closeQuietly right)
    (uncurry use)

closeQuietly :: Socket -> IO ()
closeQuietly connection = void (try @IOException (Socket.close connection))
