{-# LANGUAGE OverloadedRecordDot #-}

module HeartbeatTcpProperties
  ( tests,
  )
where

import Control.Concurrent
  ( MVar,
    forkFinally,
    newEmptyMVar,
    putMVar,
    takeMVar,
    tryReadMVar,
    yield,
  )
import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    modifyTVar',
    newTQueueIO,
    newTVarIO,
    readTQueue,
    readTVar,
    tryReadTQueue,
    writeTQueue,
    writeTVar,
  )
import Control.Exception (SomeException, bracket)
import Control.Monad (replicateM_, void)
import Data.ByteString qualified as ByteString
import Data.Word (Word64)
import Eclips.Herald.Runtime.TCP
  ( HeartbeatConfiguration,
    HeraldTcpConfigurationError (..),
    heartbeatConfigurationMicroseconds,
    heartbeatIdleMicroseconds,
    heartbeatReplyTimeoutMicroseconds,
    peerDialRetryDelayMicroseconds,
  )
import Eclips.Herald.Runtime.TCP.Internal.Heartbeat
  ( HeartbeatPhase (..),
    HeartbeatState,
    discardHeartbeatAttribution,
    newHeartbeatState,
    noteHeartbeatActivity,
    recordHeartbeatPong,
    runActiveHeartbeat,
    runEstablishmentDeadline,
    runPassiveHeartbeat,
  )
import Eclips.Herald.Runtime.TCP.Internal.Scope (newTcpContext)
import Eclips.Herald.Runtime.TCP.Internal.Socket
  ( newTcpSocketWriter,
    sendTcpSocketBytesWith,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( HeartbeatLane (..),
    HeartbeatProbeEvent (..),
    HeartbeatProbePhase (..),
    HeartbeatScheduler (..),
    TcpContext (..),
  )
import Network.Socket
  ( Family (AF_UNIX),
    SocketType (Stream),
    close,
    defaultProtocol,
    socketPair,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "physical heartbeat"
    [ testCase "TCP timing constructors reject every zero interval" caseTimingConfiguration,
      testCase "active heartbeat is establishment-gated, resettable, and single-flight" caseActiveHeartbeat,
      testCase "candidate establishment has one exact-socket deadline" caseEstablishmentDeadline,
      testCase "ordinary established traffic satisfies one outstanding probe" caseActiveHeartbeatTrafficProgress,
      testCase "one missed active reply closes only its owning lane" caseActiveHeartbeatTimeout,
      testCase "passive EAPP idle grants one resettable reply window" casePassiveHeartbeat,
      testCase "one physical writer serializes semantic and heartbeat sends" caseSerializedWriter
    ]

caseTimingConfiguration :: Assertion
caseTimingConfiguration = do
  assertEqual
    "zero peer retry"
    (Left PeerDialRetryDelayMustBePositive)
    (peerDialRetryDelayMicroseconds 0)
  assertEqual
    "zero heartbeat idle"
    (Left HeartbeatIdleDelayMustBePositive)
    (heartbeatConfigurationMicroseconds 0 1)
  assertEqual
    "zero heartbeat reply timeout"
    (Left HeartbeatReplyTimeoutMustBePositive)
    (heartbeatConfigurationMicroseconds 1 0)
  let configuration = checked (heartbeatConfigurationMicroseconds 7 11)
  assertEqual "idle representation" 7 (heartbeatIdleMicroseconds configuration)
  assertEqual "reply representation" 11 (heartbeatReplyTimeoutMicroseconds configuration)

caseActiveHeartbeat :: Assertion
caseActiveHeartbeat = do
  harness <- newHeartbeatHarness HeartbeatNotEstablished
  sent <- newTVarIO []
  done <-
    startHeartbeatWorker
      ( runActiveHeartbeat
          harness.context
          fixtureHeartbeatConfiguration
          PeerHeartbeatLane
          (readTVar harness.phase)
          harness.state
          (\nonce -> atomically (modifyTVar' sent (<> [nonce])) >> pure True)
          harness.closeLane
      )
  spin
  assertScheduledEmpty "candidate phase" harness.scheduled
  atomically (writeTVar harness.phase HeartbeatEstablished)
  (idleDelay, staleIdle) <- atomically (readTQueue harness.scheduled)
  assertEqual "initial idle delay" 7 idleDelay

  replicateM_ 100 (atomically (noteHeartbeatActivity harness.state))
  spin
  assertScheduledEmpty "activity does not amplify the in-flight idle timer" harness.scheduled
  atomically (writeTVar staleIdle True)
  (replacementDelay, replacementIdle) <- atomically (readTQueue harness.scheduled)
  assertEqual "the idle boundary rearms after observed activity" 7 replacementDelay
  assertEqual "the invalidated idle deadline emits no Ping" [] =<< atomically (readTVar sent)

  atomically (writeTVar replacementIdle True)
  (replyDelay, replyElapsed) <- atomically (readTQueue harness.scheduled)
  assertEqual "active reply timeout" 11 replyDelay
  assertEqual "the first lane-local nonce" [0] =<< atomically (readTVar sent)
  spin
  assertScheduledEmpty "one outstanding Ping" harness.scheduled

  atomically (noteHeartbeatActivity harness.state)
  atomically (noteHeartbeatActivity harness.state)
  matched <- recordHeartbeatPong harness.context PeerHeartbeatLane harness.state 0
  assertEqual "the exact Pong clears the outstanding Ping" True matched
  (nextIdleDelay, _) <- atomically (readTQueue harness.scheduled)
  assertEqual "a matching Pong starts one fresh idle wait" 7 nextIdleDelay
  atomically (writeTVar replyElapsed True)
  spin
  assertEqual "the stale reply deadline cannot close the lane" 0 =<< atomically (readTVar harness.closeCount)

  atomically (writeTVar harness.phase HeartbeatClosed)
  assertWorkerSucceeded =<< takeMVar done
  events <- atomically (readTVar harness.events)
  assertBool
    "the typed probe records the exact Pong"
    (HeartbeatProbeEvent PeerHeartbeatLane (HeartbeatPongMatched 0) `elem` events)

caseEstablishmentDeadline :: Assertion
caseEstablishmentDeadline = do
  timedOut <- newHeartbeatHarness HeartbeatNotEstablished
  timeoutDone <-
    startHeartbeatWorker
      ( runEstablishmentDeadline
          timedOut.context
          fixtureHeartbeatConfiguration
          PeerHeartbeatLane
          (readTVar timedOut.phase)
          timedOut.closeLane
      )
  (delay, elapsed) <- atomically (readTQueue timedOut.scheduled)
  assertEqual "candidate uses the checked reply interval" 11 delay
  spin
  assertScheduledEmpty "one candidate owns one deadline" timedOut.scheduled
  atomically (writeTVar elapsed True)
  assertWorkerSucceeded =<< takeMVar timeoutDone
  assertEqual "candidate expiry closes only its lane" 1
    =<< atomically (readTVar timedOut.closeCount)

  established <- newHeartbeatHarness HeartbeatNotEstablished
  establishedDone <-
    startHeartbeatWorker
      ( runEstablishmentDeadline
          established.context
          fixtureHeartbeatConfiguration
          PeerHeartbeatLane
          (readTVar established.phase)
          established.closeLane
      )
  _ <- atomically (readTQueue established.scheduled)
  atomically (writeTVar established.phase HeartbeatEstablished)
  assertWorkerSucceeded =<< takeMVar establishedDone
  assertEqual "logical establishment defeats the stale candidate deadline" 0
    =<< atomically (readTVar established.closeCount)

  quiescing <- newHeartbeatHarness HeartbeatNotEstablished
  quiescingDone <-
    startHeartbeatWorker
      ( runEstablishmentDeadline
          quiescing.context
          fixtureHeartbeatConfiguration
          ApplicationHeartbeatLane
          (readTVar quiescing.phase)
          quiescing.closeLane
      )
  (applicationDelay, quiescingElapsed) <- atomically (readTQueue quiescing.scheduled)
  assertEqual "application startup honors its supplied reply allowance" 11 applicationDelay
  atomically $ do
    writeTVar quiescingElapsed True
    writeTVar quiescing.phase HeartbeatQuiescing
  assertWorkerSucceeded =<< takeMVar quiescingDone
  assertEqual "terminal quiescence defeats a crossing establishment deadline" 0
    =<< atomically (readTVar quiescing.closeCount)

caseActiveHeartbeatTimeout :: Assertion
caseActiveHeartbeatTimeout = do
  harness <- newHeartbeatHarness HeartbeatEstablished
  sent <- newTVarIO []
  done <-
    startHeartbeatWorker
      ( runActiveHeartbeat
          harness.context
          fixtureHeartbeatConfiguration
          PeerHeartbeatLane
          (readTVar harness.phase)
          harness.state
          (\nonce -> atomically (modifyTVar' sent (<> [nonce])) >> pure True)
          harness.closeLane
      )
  (_, idleElapsed) <- atomically (readTQueue harness.scheduled)
  atomically (writeTVar idleElapsed True)
  (_, replyElapsed) <- atomically (readTQueue harness.scheduled)
  assertEqual "only one Ping is written before its reply" [0] =<< atomically (readTVar sent)
  spin
  assertScheduledEmpty "held reply deadline" harness.scheduled
  atomically (writeTVar replyElapsed True)
  assertWorkerSucceeded =<< takeMVar done
  assertEqual "one exact-lane close" 1 =<< atomically (readTVar harness.closeCount)
  assertEqual "timeout does not amplify Ping work" [0] =<< atomically (readTVar sent)
  events <- atomically (readTVar harness.events)
  assertBool
    "timeout names the outstanding nonce"
    (HeartbeatProbeEvent PeerHeartbeatLane (HeartbeatTimedOut (Just 0)) `elem` events)

caseActiveHeartbeatTrafficProgress :: Assertion
caseActiveHeartbeatTrafficProgress = do
  harness <- newHeartbeatHarness HeartbeatEstablished
  sent <- newTVarIO []
  done <-
    startHeartbeatWorker
      ( runActiveHeartbeat
          harness.context
          fixtureHeartbeatConfiguration
          PeerHeartbeatLane
          (readTVar harness.phase)
          harness.state
          (\nonce -> atomically (modifyTVar' sent (<> [nonce])) >> pure True)
          harness.closeLane
      )
  (_, idleElapsed) <- atomically (readTQueue harness.scheduled)
  atomically (writeTVar idleElapsed True)
  (_, replyElapsed) <- atomically (readTQueue harness.scheduled)
  assertEqual "the probe is outstanding" [0] =<< atomically (readTVar sent)

  atomically (noteHeartbeatActivity harness.state)
  atomically (discardHeartbeatAttribution harness.state)
  atomically (noteHeartbeatActivity harness.state)
  staleMatched <- recordHeartbeatPong harness.context PeerHeartbeatLane harness.state 0
  assertEqual "a same-nonce Pong in a later chunk is stale" False staleMatched
  (_, nextIdle) <- atomically (readTQueue harness.scheduled)
  atomically (writeTVar replyElapsed True)
  spin
  assertEqual "valid stale-Pong traffic defeats the old reply deadline" 0
    =<< atomically (readTVar harness.closeCount)
  events <- atomically (readTVar harness.events)
  assertBool
    "stale Pong activity does not fabricate exact-Pong attribution"
    ( HeartbeatProbeEvent PeerHeartbeatLane (HeartbeatPongReceived 0 False) `elem` events
        && HeartbeatProbeEvent PeerHeartbeatLane (HeartbeatPongMatched 0) `notElem` events
    )

  atomically (writeTVar harness.phase HeartbeatClosed)
  atomically (writeTVar nextIdle True)
  assertWorkerSucceeded =<< takeMVar done

casePassiveHeartbeat :: Assertion
casePassiveHeartbeat = do
  harness <- newHeartbeatHarness HeartbeatNotEstablished
  done <-
    startHeartbeatWorker
      ( runPassiveHeartbeat
          harness.context
          fixtureHeartbeatConfiguration
          ApplicationHeartbeatLane
          (readTVar harness.phase)
          harness.state
          harness.closeLane
      )
  spin
  assertScheduledEmpty "candidate EAPP phase" harness.scheduled
  atomically (writeTVar harness.phase HeartbeatEstablished)
  (_, firstIdle) <- atomically (readTQueue harness.scheduled)
  atomically (writeTVar firstIdle True)
  (_, firstReply) <- atomically (readTQueue harness.scheduled)

  atomically (noteHeartbeatActivity harness.state)
  (_, replacementIdle) <- atomically (readTQueue harness.scheduled)
  atomically (writeTVar firstReply True)
  spin
  assertEqual "valid client activity defeats the stale passive deadline" 0
    =<< atomically (readTVar harness.closeCount)

  atomically (writeTVar replacementIdle True)
  (_, replacementReply) <- atomically (readTQueue harness.scheduled)
  atomically (writeTVar replacementReply True)
  assertWorkerSucceeded =<< takeMVar done
  assertEqual "one passive exact-lane close" 1 =<< atomically (readTVar harness.closeCount)
  events <- atomically (readTVar harness.events)
  assertBool
    "the passive timeout carries no invented Ping nonce"
    (HeartbeatProbeEvent ApplicationHeartbeatLane (HeartbeatTimedOut Nothing) `elem` events)

caseSerializedWriter :: Assertion
caseSerializedWriter =
  bracket
    (socketPair AF_UNIX Stream defaultProtocol)
    (mapM_ close . (\(left, right) -> [left, right]))
    $ \(socketHandle, _) -> do
      writer <- newTcpSocketWriter
      firstEntered <- newEmptyMVar
      releaseFirst <- newEmptyMVar
      secondEntered <- newEmptyMVar
      let physicalSend _ bytes
            | bytes == ByteString.singleton 1 = do
                putMVar firstEntered ()
                takeMVar releaseFirst
            | otherwise = putMVar secondEntered ()
      first <-
        startHeartbeatWorker
          (sendTcpSocketBytesWith physicalSend writer socketHandle (ByteString.singleton 1))
      takeMVar firstEntered
      second <-
        startHeartbeatWorker
          (sendTcpSocketBytesWith physicalSend writer socketHandle (ByteString.singleton 2))
      spin
      assertEqual "the second frame cannot enter the physical send" Nothing
        =<< tryReadMVar secondEntered
      putMVar releaseFirst ()
      assertWorkerSucceeded =<< takeMVar first
      assertWorkerSucceeded =<< takeMVar second
      assertEqual "the second frame enters after the first releases the writer" (Just ())
        =<< tryReadMVar secondEntered

data HeartbeatHarness = HeartbeatHarness
  { context :: TcpContext,
    phase :: TVar HeartbeatPhase,
    state :: HeartbeatState,
    scheduled :: TQueue (Word64, TVar Bool),
    events :: TVar [HeartbeatProbeEvent],
    closeCount :: TVar Int,
    closeLane :: IO ()
  }

newHeartbeatHarness :: HeartbeatPhase -> IO HeartbeatHarness
newHeartbeatHarness initialPhase = do
  context <- newTcpContext
  phase <- newTVarIO initialPhase
  state <- newHeartbeatState
  scheduled <- newTQueueIO
  events <- newTVarIO []
  closeCount <- newTVarIO 0
  let scheduler microseconds = do
        elapsed <- newTVarIO False
        atomically (writeTQueue scheduled (microseconds, elapsed))
        pure elapsed
      observe event = atomically (modifyTVar' events (<> [event]))
      closeLaneAction =
        atomically $ do
          modifyTVar' closeCount (+ 1)
          writeTVar phase HeartbeatClosed
  atomically $ do
    writeTVar context.tcpHeartbeatScheduler (HeartbeatScheduler scheduler)
    writeTVar context.tcpHeartbeatProbe (Just observe)
  pure (HeartbeatHarness context phase state scheduled events closeCount closeLaneAction)

startHeartbeatWorker :: IO () -> IO (MVar (Either SomeException ()))
startHeartbeatWorker action = do
  done <- newEmptyMVar
  _ <- forkFinally action (putMVar done)
  pure done

assertScheduledEmpty :: String -> TQueue value -> Assertion
assertScheduledEmpty label scheduled = do
  retained <- atomically (tryReadTQueue scheduled)
  assertEqual label Nothing (void retained)

assertWorkerSucceeded :: Either SomeException () -> Assertion
assertWorkerSucceeded result = case result of
  Left failure -> assertFailure ("heartbeat worker failed: " <> show failure)
  Right () -> pure ()

spin :: IO ()
spin = replicateM_ 100 yield

fixtureHeartbeatConfiguration :: HeartbeatConfiguration
fixtureHeartbeatConfiguration =
  checked (heartbeatConfigurationMicroseconds 7 11)

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
