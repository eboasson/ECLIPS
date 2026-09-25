module RetainedRequestProperties (tests) where

import Control.Concurrent.STM
  ( atomically,
    orElse,
  )
import Control.Monad (replicateM, when)
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
  )
import Eclips.Oracle.Runtime.Internal.RetainedRequest
  ( RetainedRequestClaim,
    RetainedRequestOffer (..),
    RetainedRequestQueue,
    acknowledgeRetainedRequest,
    claimRetainedRequest,
    completeRetainedRequestSend,
    newRetainedRequestQueueIO,
    offerRetainedRequest,
    retainedRequestClaimGeneration,
    retainedRequestClaimPayload,
    retireRetainedRequestLease,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "retained Raft request"
    [ testCase
        "a delayed response permits one retransmission per completed heartbeat send"
        caseDelayedResponsePermitsHeartbeatRetransmission,
      testCase
        "a newer vote request supersedes an obsolete queued append"
        caseNewerGenerationSupersedesQueued,
      testCase
        "an acknowledgement tombstones racing duplicates"
        caseAcknowledgementTombstonesDuplicates,
      testCase
        "connection loss reoffers a successfully sent request exactly once"
        caseReconnectReoffersOnce,
      testCase
        "an old response cannot retire a newer retained request"
        caseOldAcknowledgementPreservesNewer,
      testCase
        "lease loss while an old send is in flight reoffers only the newer request"
        caseNewerGenerationSurvivesOldLeaseRetirement
    ]

data TestRpc
  = AppendEntries Int
  | RequestVote Int
  deriving stock (Eq, Show)

type TestQueue = RetainedRequestQueue Int Int TestRpc

newTestQueue :: IO TestQueue
newTestQueue = newRetainedRequestQueueIO

caseDelayedResponsePermitsHeartbeatRetransmission :: Assertion
caseDelayedResponsePermitsHeartbeatRetransmission = do
  queue <- newTestQueue
  initial <- atomically (offerRetainedRequest 7 (AppendEntries 11) queue)
  whileQueued <-
    replicateM
      100
      (atomically (offerRetainedRequest 7 (AppendEntries 11) queue))
  initialClaim <- requireClaim "the initial append" =<< tryClaim 1 queue
  whileSending <-
    replicateM
      100
      (atomically (offerRetainedRequest 7 (AppendEntries 11) queue))
  initialSent <- atomically (completeRetainedRequestSend initialClaim queue)
  retransmission <- atomically (offerRetainedRequest 7 (AppendEntries 12) queue)
  duplicateWhileQueued <- atomically (offerRetainedRequest 7 (AppendEntries 13) queue)
  retransmissionClaim <-
    requireClaim "the retransmitted append" =<< tryClaim 1 queue
  duplicateWhileSending <- atomically (offerRetainedRequest 7 (AppendEntries 14) queue)
  retransmissionSent <- atomically (completeRetainedRequestSend retransmissionClaim queue)
  withoutFurtherHeartbeat <- tryClaim 1 queue
  callbacks <- newIORef 0
  firstResponse <- deliverResponse callbacks 7 queue
  duplicateResponse <- deliverResponse callbacks 7 queue
  afterAcknowledgement <- atomically (offerRetainedRequest 7 (AppendEntries 15) queue)
  afterAcknowledgementClaim <- tryClaim 1 queue
  callbackCount <- readIORef callbacks

  assertEqual "the first generation creates one wake" RetainedRequestEnqueued initial
  assertEqual
    "heartbeat reoffers while queued are coalesced"
    (replicate 100 RetainedRequestCoalesced)
    whileQueued
  assertClaim "the retained append" 7 (AppendEntries 11) initialClaim
  assertEqual
    "heartbeat reoffers while the physical send is in progress are coalesced"
    (replicate 100 RetainedRequestCoalesced)
    whileSending
  assertEqual "the first physical send completes" True initialSent
  assertEqual
    "the next heartbeat queues one retransmission while its response is delayed"
    RetainedRequestEnqueued
    retransmission
  assertEqual
    "another re-offer cannot duplicate the queued retransmission"
    RetainedRequestCoalesced
    duplicateWhileQueued
  assertClaim "the retransmitted append" 7 (AppendEntries 13) retransmissionClaim
  assertEqual
    "another re-offer cannot duplicate the in-progress retransmission"
    RetainedRequestCoalesced
    duplicateWhileSending
  assertEqual "the retransmission completes" True retransmissionSent
  assertNoClaim
    "one completed heartbeat re-offer creates only one retransmission"
    withoutFurtherHeartbeat
  assertEqual "the first matching response acknowledges the logical request" True firstResponse
  assertEqual "a duplicate response cannot acknowledge it again" False duplicateResponse
  assertEqual "the acknowledgement tombstone suppresses later re-offers" RetainedRequestSuppressed afterAcknowledgement
  assertNoClaim "acknowledgement removes the pending logical request" afterAcknowledgementClaim
  assertEqual "the logical response callback runs exactly once" 1 callbackCount

caseNewerGenerationSupersedesQueued :: Assertion
caseNewerGenerationSupersedesQueued = do
  queue <- newTestQueue
  appendOffer <- atomically (offerRetainedRequest 20 (AppendEntries 3) queue)
  voteOffer <- atomically (offerRetainedRequest 21 (RequestVote 9) queue)
  claim <- requireClaim "the superseding vote request" =<< tryClaim 4 queue
  sent <- atomically (completeRetainedRequestSend claim queue)
  obsolete <- tryClaim 4 queue

  assertEqual "the append initially becomes pending" RetainedRequestEnqueued appendOffer
  assertEqual "the newer vote generation becomes pending" RetainedRequestEnqueued voteOffer
  assertClaim "the superseding vote request" 21 (RequestVote 9) claim
  assertEqual "the vote request completes" True sent
  assertNoClaim "the obsolete append wake cannot become a send" obsolete

caseAcknowledgementTombstonesDuplicates :: Assertion
caseAcknowledgementTombstonesDuplicates = do
  queue <- newTestQueue
  _ <- atomically (offerRetainedRequest 30 (AppendEntries 5) queue)
  claim <- requireClaim "the acknowledged request" =<< tryClaim 6 queue
  _ <- atomically (completeRetainedRequestSend claim queue)

  justBefore <- atomically (offerRetainedRequest 30 (AppendEntries 5) queue)
  acknowledged <- atomically (acknowledgeRetainedRequest 30 queue)
  justAfter <- atomically (offerRetainedRequest 30 (AppendEntries 5) queue)
  resurrected <- tryClaim 6 queue

  assertEqual
    "a heartbeat racing just before the response queues one retransmission"
    RetainedRequestEnqueued
    justBefore
  assertEqual "the matching response retires its retained request" True acknowledged
  assertEqual
    "the tombstone suppresses a duplicate racing just after the response"
    RetainedRequestSuppressed
    justAfter
  assertNoClaim "the acknowledged generation cannot be resurrected" resurrected

caseReconnectReoffersOnce :: Assertion
caseReconnectReoffersOnce = do
  queue <- newTestQueue
  _ <- atomically (offerRetainedRequest 40 (AppendEntries 8) queue)
  original <- requireClaim "the original physical send" =<< tryClaim 10 queue
  _ <- atomically (completeRetainedRequestSend original queue)

  firstRetirement <- atomically (retireRetainedRequestLease 10 queue)
  repeatedRetirement <- atomically (retireRetainedRequestLease 10 queue)
  replacement <- requireClaim "the replacement physical send" =<< tryClaim 11 queue
  duplicateOffer <- atomically (offerRetainedRequest 40 (AppendEntries 8) queue)
  concurrentDuplicate <- tryClaim 11 queue
  resent <- atomically (completeRetainedRequestSend replacement queue)
  afterResend <- tryClaim 11 queue
  callbacks <- newIORef 0
  firstResponse <- deliverResponse callbacks 40 queue
  duplicateResponse <- deliverResponse callbacks 40 queue
  callbackCount <- readIORef callbacks

  assertEqual "connection loss rearms the sent request" True firstRetirement
  assertEqual "repeated retirement cannot enqueue it twice" False repeatedRetirement
  assertClaim "the replacement physical send" 40 (AppendEntries 8) replacement
  assertEqual
    "a same-generation offer during the replacement send is coalesced"
    RetainedRequestCoalesced
    duplicateOffer
  assertNoClaim "only one replacement send can be in flight" concurrentDuplicate
  assertEqual "the replacement send completes" True resent
  assertNoClaim "the replacement send is not offered again" afterResend
  assertEqual "the first matching response is admitted" True firstResponse
  assertEqual "a duplicate response is consumed without admission" False duplicateResponse
  assertEqual "old and replacement responses invoke the callback once" 1 callbackCount

caseOldAcknowledgementPreservesNewer :: Assertion
caseOldAcknowledgementPreservesNewer = do
  queue <- newTestQueue
  _ <- atomically (offerRetainedRequest 50 (AppendEntries 13) queue)
  oldClaim <- requireClaim "the older append" =<< tryClaim 20 queue
  _ <- atomically (completeRetainedRequestSend oldClaim queue)

  newerOffer <- atomically (offerRetainedRequest 51 (RequestVote 14) queue)
  callbacks <- newIORef 0
  oldAcknowledged <- deliverResponse callbacks 50 queue
  newerClaim <- requireClaim "the newer vote" =<< tryClaim 20 queue
  newerSent <- atomically (completeRetainedRequestSend newerClaim queue)
  newerAcknowledged <- deliverResponse callbacks 51 queue
  callbackCount <- readIORef callbacks

  assertEqual "the newer generation is retained" RetainedRequestEnqueued newerOffer
  assertEqual "the old wire response retires no current request" False oldAcknowledged
  assertClaim "the newer vote" 51 (RequestVote 14) newerClaim
  assertEqual "the newer vote is sent" True newerSent
  assertEqual "the newer response is admitted" True newerAcknowledged
  assertEqual "only the newer response invokes a callback" 1 callbackCount

caseNewerGenerationSurvivesOldLeaseRetirement :: Assertion
caseNewerGenerationSurvivesOldLeaseRetirement = do
  queue <- newTestQueue
  _ <- atomically (offerRetainedRequest 60 (AppendEntries 21) queue)
  _oldClaim <- requireClaim "the in-flight old append" =<< tryClaim 30 queue

  newerOffer <- atomically (offerRetainedRequest 61 (RequestVote 22) queue)
  oldLeaseRetired <- atomically (retireRetainedRequestLease 30 queue)
  newerClaim <- requireClaim "the replacement vote" =<< tryClaim 31 queue
  duplicate <- tryClaim 31 queue

  assertEqual "the newer vote supersedes the in-flight append" RetainedRequestEnqueued newerOffer
  assertEqual
    "retiring the old lease cannot enqueue another copy of the newer vote"
    False
    oldLeaseRetired
  assertClaim "the replacement vote" 61 (RequestVote 22) newerClaim
  assertNoClaim "only the newer vote is reoffered" duplicate

deliverResponse :: IORef Int -> Int -> TestQueue -> IO Bool
deliverResponse callbacks generation queue = do
  acknowledged <- atomically (acknowledgeRetainedRequest generation queue)
  when acknowledged (modifyIORef' callbacks (+ 1))
  pure acknowledged

tryClaim ::
  (Eq generation) =>
  lease ->
  RetainedRequestQueue generation lease payload ->
  IO (Maybe (RetainedRequestClaim generation lease payload))
tryClaim lease queue =
  atomically
    ( (Just <$> claimRetainedRequest lease queue)
        `orElse` pure Nothing
    )

requireClaim :: String -> Maybe claim -> IO claim
requireClaim context = maybe (fail (context <> " was not claimable")) pure

assertNoClaim :: String -> Maybe claim -> Assertion
assertNoClaim context = \case
  Nothing -> pure ()
  Just _ -> assertFailure context

assertClaim ::
  (Eq generation, Show generation, Eq payload, Show payload) =>
  String ->
  generation ->
  payload ->
  RetainedRequestClaim generation lease payload ->
  Assertion
assertClaim context expectedGeneration expectedPayload claim = do
  assertEqual
    (context <> " has the expected generation")
    expectedGeneration
    (retainedRequestClaimGeneration claim)
  assertEqual
    (context <> " has the expected payload")
    expectedPayload
    (retainedRequestClaimPayload claim)
