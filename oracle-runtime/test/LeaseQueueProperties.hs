module LeaseQueueProperties (tests) where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    newEmptyMVar,
    putMVar,
    takeMVar,
    yield,
  )
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    newTVarIO,
    readTVar,
    retry,
    writeTVar,
  )
import Eclips.Oracle.Runtime.Internal.LeaseQueue
  ( LeaseClaim,
    LeaseQueue,
    claimCurrentLeaseItem,
    completeLeaseClaim,
    leaseClaimItem,
    newLeaseQueueIO,
    retireLeaseClaims,
    writeLeaseQueue,
  )
import GHC.Conc
  ( BlockReason (BlockedOnSTM),
    ThreadStatus (ThreadBlocked, ThreadDied, ThreadFinished),
    threadStatus,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "physical lease queue"
    [ testCase
        "an obsolete lease cannot consume the replacement lease's item"
        caseObsoleteLeasePreservesItem,
      testCase
        "replacement reclaims an in-flight portable item at the FIFO head"
        caseReplacementReclaimsInFlightItem,
      testCase
        "replacement discards an in-flight binding-local item"
        caseReplacementDiscardsBindingLocalItem,
      testCase
        "retirement discards pending old responses ahead of portable requests"
        caseRetirementDiscardsPendingOldResponses,
      testCase
        "the current lease claims and completes each shared item exactly once"
        caseCurrentLeaseClaimsExactlyOnce
    ]

data TestItem
  = Portable String
  | BindingLocal Int String
  deriving stock (Eq, Show)

caseObsoleteLeasePreservesItem :: IO ()
caseObsoleteLeasePreservesItem = do
  current <- newTVarIO (1 :: Int)
  queue <- newLeaseQueueIO
  staleResult <- newEmptyMVar
  staleThread <-
    forkIO
      (atomically (claimFor current 1 queue) >>= putMVar staleResult)
  blocked <- timeout 1_000_000 (awaitBlockedOnSTM staleThread)
  assertEqual
    "the obsolete lease first blocks while it still owns the empty queue"
    (Just ())
    blocked
  atomically $ do
    writeTVar current 2
    writeLeaseQueue queue (Portable "replacement")
  stale <- timeout 1_000_000 (takeMVar staleResult)
  replacement <- atomically (claimFor current 2 queue)
  assertEqual
    "the woken obsolete lease settles without an item"
    (Just Nothing)
    (fmap (fmap leaseClaimItem) stale)
  assertEqual
    "the replacement receives the item the obsolete lease could not remove"
    (Just (Portable "replacement"))
    (leaseClaimItem <$> replacement)

caseReplacementReclaimsInFlightItem :: IO ()
caseReplacementReclaimsInFlightItem = do
  current <- newTVarIO (1 :: Int)
  queue <- newLeaseQueueIO
  atomically $ do
    writeLeaseQueue queue (Portable "first")
    writeLeaseQueue queue (Portable "second")
  oldClaim <- atomically (requireClaim =<< claimFor current 1 queue)

  atomically $ do
    retireLeaseClaims bindingLocalLease 1 queue
    writeTVar current 2

  replacementClaim <- atomically (requireClaim =<< claimFor current 2 queue)
  lateOldCompletion <- atomically (completeLeaseClaim oldClaim queue)
  replacementCompletion <- atomically (completeLeaseClaim replacementClaim queue)
  secondClaim <- atomically (requireClaim =<< claimFor current 2 queue)
  assertEqual
    "the replacement reclaims the exact in-flight FIFO head"
    (Portable "first")
    (leaseClaimItem replacementClaim)
  assertEqual
    "a late old completion cannot clear the replacement claim"
    False
    lateOldCompletion
  assertEqual "the replacement completes its own claim" True replacementCompletion
  assertEqual
    "the previously pending item remains second"
    (Portable "second")
    (leaseClaimItem secondClaim)

caseReplacementDiscardsBindingLocalItem :: IO ()
caseReplacementDiscardsBindingLocalItem = do
  current <- newTVarIO (1 :: Int)
  queue <- newLeaseQueueIO
  atomically $ do
    writeLeaseQueue queue (BindingLocal 1 "old response")
    writeLeaseQueue queue (Portable "next request")
  _ <- atomically (requireClaim =<< claimFor current 1 queue)
  atomically $ do
    retireLeaseClaims bindingLocalLease 1 queue
    writeTVar current 2
  replacement <- atomically (requireClaim =<< claimFor current 2 queue)
  assertEqual
    "a response correlated on the retired binding is not handed to its replacement"
    (Portable "next request")
    (leaseClaimItem replacement)

caseRetirementDiscardsPendingOldResponses :: IO ()
caseRetirementDiscardsPendingOldResponses = do
  current <- newTVarIO (1 :: Int)
  queue <- newLeaseQueueIO
  atomically $ do
    writeLeaseQueue queue (BindingLocal 1 "old response one")
    writeLeaseQueue queue (BindingLocal 1 "old response two")
    writeLeaseQueue queue (Portable "first portable request")
    writeLeaseQueue queue (BindingLocal 2 "replacement response")
    writeLeaseQueue queue (BindingLocal 1 "old response three")
    writeLeaseQueue queue (Portable "second portable request")
    retireLeaseClaims bindingLocalLease 1 queue
    writeTVar current 2
  first <- atomically (requireClaim =<< claimFor current 2 queue)
  _ <- atomically (completeLeaseClaim first queue)
  replacementLocal <- atomically (requireClaim =<< claimFor current 2 queue)
  _ <- atomically (completeLeaseClaim replacementLocal queue)
  second <- atomically (requireClaim =<< claimFor current 2 queue)
  assertEqual
    "all pending responses for the retired lease are removed before the FIFO head"
    (Portable "first portable request")
    (leaseClaimItem first)
  assertEqual
    "a response belonging to the replacement lease is preserved in place"
    (BindingLocal 2 "replacement response")
    (leaseClaimItem replacementLocal)
  assertEqual
    "portable requests retain their exact relative FIFO order"
    (Portable "second portable request")
    (leaseClaimItem second)

awaitBlockedOnSTM :: ThreadId -> IO ()
awaitBlockedOnSTM thread = do
  observed <- threadStatus thread
  case observed of
    ThreadBlocked BlockedOnSTM -> pure ()
    ThreadFinished -> fail "the lease claim finished before blocking"
    ThreadDied -> fail "the lease claim died before blocking"
    _ -> yield >> awaitBlockedOnSTM thread

caseCurrentLeaseClaimsExactlyOnce :: IO ()
caseCurrentLeaseClaimsExactlyOnce = do
  current <- newTVarIO (7 :: Int)
  queue <- newLeaseQueueIO
  atomically $ do
    writeLeaseQueue queue (Portable "first")
    writeLeaseQueue queue (Portable "second")
  first <- atomically (requireClaim =<< claimFor current 7 queue)
  firstCompleted <- atomically (completeLeaseClaim first queue)
  second <- atomically (requireClaim =<< claimFor current 7 queue)
  assertEqual "the current lease receives the FIFO head" (Portable "first") (leaseClaimItem first)
  assertEqual "the current lease completes the FIFO head" True firstCompleted
  assertEqual "the next claim receives the next exact item" (Portable "second") (leaseClaimItem second)

claimFor ::
  TVar Int ->
  Int ->
  LeaseQueue Int TestItem ->
  STM (Maybe (LeaseClaim Int TestItem))
claimFor current expected =
  claimCurrentLeaseItem expected ((== expected) <$> readTVar current)

requireClaim :: Maybe claim -> STM claim
requireClaim = maybe retry pure

bindingLocalLease :: TestItem -> Maybe Int
bindingLocalLease = \case
  Portable {} -> Nothing
  BindingLocal lease _ -> Just lease
