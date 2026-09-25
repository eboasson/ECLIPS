module WatchWorkerProperties (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
  ( newEmptyMVar,
    newMVar,
    putMVar,
    takeMVar,
    tryReadMVar,
    withMVar,
  )
import Control.Exception
  ( AsyncException (ThreadKilled),
    throwIO,
  )
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
  )
import Eclips.Oracle.Runtime.Internal.WatchWorker
  ( WatchWorkerCompletion,
    awaitWatchWorkerCompletion,
    newWatchWorkerOwnerIO,
    startWatchWorkerOrReject,
    stopWatchWorkerOwner,
  )
import System.Timeout (timeout)
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
    "watch worker ownership"
    [ testCase "a second outstanding watch rejects the physical lane" caseDuplicateOutstanding,
      testCase "a completed watch releases the lane for sequential reuse" caseSequentialReuse,
      testCase "an unpublished completion terminates and closes the lane" caseUnpublishedCompletion,
      testCase "a non-IOException worker failure terminates and closes the lane" caseNonIOExceptionFailure,
      testCase "a published worker hands the lane to a waiting successor" casePublishedHandoff,
      testCase "closing the owner cancels its worker and rejects a racing successor" caseCloseOwnership,
      testCase "closing after publication does not fail-close the lane" casePublishedCloseOwnership
    ]

caseDuplicateOutstanding :: Assertion
caseDuplicateOutstanding = do
  responseLock <- newMVar ()
  owner <- newWatchWorkerOwnerIO responseLock
  entered <- newEmptyMVar
  release <- newEmptyMVar
  rejected <- newIORef (0 :: Int)
  duplicateRan <- newEmptyMVar
  first <-
    requireStarted
      =<< startWatchWorkerOrReject
        owner
        (recordRejection rejected)
        (\_ -> putMVar entered () >> takeMVar release)
  takeMVar entered

  duplicate <-
    startWatchWorkerOrReject
      owner
      (recordRejection rejected)
      (\_ -> putMVar duplicateRan ())
  assertRejected "duplicate outstanding watch" duplicate
  assertEqual "the duplicate closes its lane exactly once" 1 =<< readIORef rejected
  assertEqual "the rejected worker never starts" Nothing =<< tryReadMVar duplicateRan

  putMVar release ()
  awaitCompletion first
  stopWatchWorkerOwner owner

caseSequentialReuse :: Assertion
caseSequentialReuse = do
  responseLock <- newMVar ()
  owner <- newWatchWorkerOwnerIO responseLock
  rejected <- newIORef (0 :: Int)
  runs <- newIORef (0 :: Int)
  first <-
    requireStarted
      =<< startWatchWorkerOrReject
        owner
        (recordRejection rejected)
        ( \published ->
            withMVar responseLock $ \() -> do
              modifyIORef' runs (+ 1)
              published
        )
  awaitCompletion first
  second <-
    requireStarted
      =<< startWatchWorkerOrReject
        owner
        (recordRejection rejected)
        ( \published ->
            withMVar responseLock $ \() -> do
              modifyIORef' runs (+ 1)
              published
        )
  awaitCompletion second

  assertEqual "both sequential workers execute" 2 =<< readIORef runs
  assertEqual "sequential reuse does not reject the lane" 0 =<< readIORef rejected
  stopWatchWorkerOwner owner

caseUnpublishedCompletion :: Assertion
caseUnpublishedCompletion = do
  responseLock <- newMVar ()
  owner <- newWatchWorkerOwnerIO responseLock
  terminations <- newIORef (0 :: Int)
  successorRan <- newEmptyMVar
  completion <-
    requireStarted
      =<< startWatchWorkerOrReject
        owner
        (recordRejection terminations)
        (\_ -> pure ())
  awaitCompletion completion
  assertEqual "the unpublished worker terminates its lane" 1 =<< readIORef terminations

  successor <-
    startWatchWorkerOrReject
      owner
      (recordRejection terminations)
      (\_ -> putMVar successorRan ())
  assertRejected "successor after unpublished completion" successor
  assertEqual "the closed owner terminates the successor lane" 2 =<< readIORef terminations
  assertEqual "the successor worker never starts" Nothing =<< tryReadMVar successorRan
  stopWatchWorkerOwner owner

caseNonIOExceptionFailure :: Assertion
caseNonIOExceptionFailure = do
  responseLock <- newMVar ()
  owner <- newWatchWorkerOwnerIO responseLock
  terminations <- newIORef (0 :: Int)
  successorRan <- newEmptyMVar
  completion <-
    requireStarted
      =<< startWatchWorkerOrReject
        owner
        (recordRejection terminations)
        (\_ -> throwIO ThreadKilled)
  awaitCompletion completion
  assertEqual
    "the exceptional unpublished worker terminates its lane exactly once"
    1
    =<< readIORef terminations

  successor <-
    startWatchWorkerOrReject
      owner
      (recordRejection terminations)
      (\_ -> putMVar successorRan ())
  assertRejected "successor after exceptional unpublished completion" successor
  assertEqual
    "the permanently closed owner terminates the rejected successor lane"
    2
    =<< readIORef terminations
  assertEqual "the successor worker never starts" Nothing =<< tryReadMVar successorRan
  stopWatchWorkerOwner owner

casePublishedHandoff :: Assertion
casePublishedHandoff = do
  responseLock <- newMVar ()
  owner <- newWatchWorkerOwnerIO responseLock
  terminations <- newIORef (0 :: Int)
  published <- newEmptyMVar
  release <- newEmptyMVar
  successorResult <- newEmptyMVar
  successorRan <- newEmptyMVar
  first <-
    requireStarted
      =<< startWatchWorkerOrReject
        owner
        (recordRejection terminations)
        ( \markPublished -> do
            withMVar responseLock (const markPublished)
            putMVar published ()
            takeMVar release
        )
  takeMVar published
  _ <-
    forkIO $ do
      result <-
        startWatchWorkerOrReject
          owner
          (recordRejection terminations)
          ( \markPublished ->
              withMVar responseLock $ \() -> do
                putMVar successorRan ()
                markPublished
          )
      putMVar successorResult result
  waiting <- tryReadMVar successorResult
  case waiting of
    Nothing -> pure ()
    Just _ -> assertFailure "the successor did not wait for published worker completion"

  putMVar release ()
  awaitCompletion first
  successor <- requireStarted =<< takeMVar successorResult
  takeMVar successorRan
  awaitCompletion successor
  assertEqual "published handoff does not terminate the lane" 0 =<< readIORef terminations
  stopWatchWorkerOwner owner

caseCloseOwnership :: Assertion
caseCloseOwnership = do
  responseLock <- newMVar ()
  owner <- newWatchWorkerOwnerIO responseLock
  entered <- newEmptyMVar
  never <- newEmptyMVar
  rejected <- newIORef (0 :: Int)
  active <-
    requireStarted
      =<< startWatchWorkerOrReject
        owner
        (recordRejection rejected)
        (\_ -> putMVar entered () >> takeMVar never)
  takeMVar entered
  stopWatchWorkerOwner owner
  awaitCompletion active

  successor <-
    startWatchWorkerOrReject
      owner
      (recordRejection rejected)
      (\_ -> assertFailure "a worker started after connection close")
  assertRejected "closed watch owner" successor
  assertEqual
    "the unpublished worker and rejected successor both terminate the lane"
    2
    =<< readIORef rejected

casePublishedCloseOwnership :: Assertion
casePublishedCloseOwnership = do
  responseLock <- newMVar ()
  owner <- newWatchWorkerOwnerIO responseLock
  published <- newEmptyMVar
  never <- newEmptyMVar
  terminated <- newIORef (0 :: Int)
  active <-
    requireStarted
      =<< startWatchWorkerOrReject
        owner
        (recordRejection terminated)
        ( \markPublished -> do
            withMVar responseLock (const markPublished)
            putMVar published ()
            takeMVar never
        )
  takeMVar published
  stopWatchWorkerOwner owner
  awaitCompletion active
  assertEqual
    "a fully published response does not fail-close during owner shutdown"
    0
    =<< readIORef terminated

recordRejection :: IORef Int -> IO ()
recordRejection rejected = modifyIORef' rejected (+ 1)

requireStarted :: Maybe WatchWorkerCompletion -> IO WatchWorkerCompletion
requireStarted = \case
  Just completion -> pure completion
  Nothing -> assertFailure "the watch worker was unexpectedly rejected"

assertRejected :: String -> Maybe WatchWorkerCompletion -> Assertion
assertRejected context = \case
  Nothing -> pure ()
  Just _ -> assertFailure (context <> " was unexpectedly accepted")

awaitCompletion :: WatchWorkerCompletion -> Assertion
awaitCompletion completion = do
  completed <- timeout 1_000_000 (awaitWatchWorkerCompletion completion)
  assertBool "the watch worker releases its ownership lease" (completed == Just ())
