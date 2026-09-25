{-# LANGUAGE OverloadedStrings #-}

module SocketProgressProperties (tests) where

import Control.Concurrent.STM (retry)
import Control.Monad (void)
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
  )
import Eclips.Oracle.Runtime.Internal.TCP.Socket
  ( TcpReceiveAttempt (..),
    TcpSendAttempt (..),
    receiveWithProgress,
    sendAllWithProgress,
    waitForReadinessOrWatchdogWith,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "socket progress"
    [ testCase "would-block retries retain the exact unsent suffix" caseExactSendProgress,
      testCase "would-block receive retries without inventing EOF" caseExactReceiveProgress,
      testCase "an empty received chunk remains EOF" caseReceiveEof,
      testCase "each repeated would-block installs fresh readiness" caseRepeatedReadinessRegistration,
      testCase "ready success cancels its readiness registration" caseReadyCancellation,
      testCase "watchdog expiry cancels its readiness registration" caseWatchdogCancellation
    ]

caseExactSendProgress :: IO ()
caseExactSendProgress = do
  attempts <- newIORef [TcpSendWouldBlock, TcpSentBytes 2, TcpSendWouldBlock, TcpSentBytes 3]
  offered <- newIORef []
  waits <- newIORef (0 :: Int)
  let sendAttempt remaining = do
        modifyIORef' offered (remaining :)
        popAttempt attempts TcpSendWouldBlock
      waitReady = modifyIORef' waits (+ 1)
  sendAllWithProgress waitReady sendAttempt "abcde"
  assertEqual "only the retained suffix is retried" ["abcde", "abcde", "cde", "cde"] . reverse
    =<< readIORef offered
  assertEqual "each would-block waits once" 2 =<< readIORef waits

caseExactReceiveProgress :: IO ()
caseExactReceiveProgress = do
  attempts <- newIORef [TcpReceiveWouldBlock, TcpReceivedBytes "abc"]
  requested <- newIORef []
  waits <- newIORef (0 :: Int)
  let receiveAttempt count = do
        modifyIORef' requested (count :)
        popAttempt attempts TcpReceiveWouldBlock
      waitReady = modifyIORef' waits (+ 1)
  received <- receiveWithProgress waitReady receiveAttempt 7
  assertEqual "the received bytes are returned exactly" "abc" received
  assertEqual "would-block is retried at the same bound" [7, 7] . reverse
    =<< readIORef requested
  assertEqual "would-block waits once" 1 =<< readIORef waits

caseReceiveEof :: IO ()
caseReceiveEof = do
  waits <- newIORef (0 :: Int)
  received <-
    receiveWithProgress
      (modifyIORef' waits (+ 1))
      (const (pure (TcpReceivedBytes "")))
      7
  assertEqual "empty receive is returned as EOF" "" received
  assertEqual "EOF does not install readiness" 0 =<< readIORef waits

caseRepeatedReadinessRegistration :: IO ()
caseRepeatedReadinessRegistration = do
  attempts <- newIORef [TcpSendWouldBlock, TcpSendWouldBlock, TcpSentBytes 1]
  nextRegistration <- newIORef (0 :: Int)
  registered <- newIORef []
  cancelled <- newIORef []
  let register = do
        identifier <- (+ 1) <$> readIORef nextRegistration
        modifyIORef' nextRegistration (const identifier)
        modifyIORef' registered (identifier :)
        pure (pure (), modifyIORef' cancelled (identifier :))
      waitReady = void (waitForReadinessOrWatchdogWith 1_000_000 register)
      sendAttempt _ = popAttempt attempts TcpSendWouldBlock
  sendAllWithProgress waitReady sendAttempt "x"
  assertEqual "each wait owns a distinct registration" [1, 2] . reverse
    =<< readIORef registered
  assertEqual "each distinct registration is cancelled" [1, 2] . reverse
    =<< readIORef cancelled

caseReadyCancellation :: IO ()
caseReadyCancellation = do
  cancelled <- newIORef (0 :: Int)
  observed <-
    waitForReadinessOrWatchdogWith
      1_000_000
      (pure (pure (7 :: Int), modifyIORef' cancelled (+ 1)))
  assertEqual "ready value is preserved" (Just 7) observed
  assertEqual "successful readiness registration is cancelled" 1
    =<< readIORef cancelled

caseWatchdogCancellation :: IO ()
caseWatchdogCancellation = do
  cancelled <- newIORef (0 :: Int)
  void
    $ waitForReadinessOrWatchdogWith
      0
      (pure (retry, modifyIORef' cancelled (+ 1)))
  assertEqual "timed-out readiness registration is cancelled" 1
    =<< readIORef cancelled

popAttempt :: IORef [attempt] -> attempt -> IO attempt
popAttempt attempts fallback = do
  observed <- readIORef attempts
  case observed of
    [] -> pure fallback
    next : remaining -> do
      modifyIORef' attempts (const remaining)
      pure next
