-- | One reconnect-safe logical request per remote peer.
--
-- A pure Raft owner may re-offer the same outstanding dispatch generation on
-- every heartbeat. This queue coalesces duplicate offers while a physical send
-- is queued or in progress, then permits one retransmission after that send has
-- completed. Physical connection generations are leases: losing the lease
-- makes a successfully sent logical request eligible for exactly one send on
-- its replacement connection.
module Eclips.Oracle.Runtime.Internal.RetainedRequest
  ( RetainedRequestQueue,
    RetainedRequestClaim,
    RetainedRequestOffer (..),
    newRetainedRequestQueueIO,
    offerRetainedRequest,
    claimRetainedRequest,
    retainedRequestClaimGeneration,
    retainedRequestClaimPayload,
    completeRetainedRequestSend,
    retireRetainedRequestLease,
    acknowledgeRetainedRequest,
  )
where

import Control.Concurrent.STM
  ( STM,
    TQueue,
    TVar,
    newTQueueIO,
    newTVarIO,
    readTQueue,
    readTVar,
    retry,
    writeTQueue,
    writeTVar,
  )

-- | Whether an offer created new physical work, merely refreshed the payload
-- of the retained generation, or arrived after that generation was retired.
data RetainedRequestOffer
  = RetainedRequestEnqueued
  | RetainedRequestCoalesced
  | RetainedRequestSuppressed
  deriving stock (Eq, Show)

data RetainedRequestDelivery lease
  = RetainedRequestQueued
  | RetainedRequestSending lease
  | RetainedRequestSent lease

data RetainedRequestState generation lease payload
  = RetainedRequestEmpty
  | RetainedRequestCurrent
      generation
      payload
      (RetainedRequestDelivery lease)
  | RetainedRequestAcknowledged generation

-- | The wake FIFO contains generation tokens, never copies of request
-- payloads.  Superseded tokens invalidate themselves when a writer reaches
-- them, so a newer logical request is not forced to send obsolete work first.
data RetainedRequestQueue generation lease payload = RetainedRequestQueue
  { retainedRequestState :: TVar (RetainedRequestState generation lease payload),
    retainedRequestWakes :: TQueue generation
  }

-- | One payload claimed by one physical connection lease.  Completion checks
-- both identities, making a late old-writer completion harmless.
data RetainedRequestClaim generation lease payload
  = RetainedRequestClaim generation lease payload

newRetainedRequestQueueIO ::
  IO (RetainedRequestQueue generation lease payload)
newRetainedRequestQueueIO =
  RetainedRequestQueue
    <$> newTVarIO RetainedRequestEmpty
    <*> newTQueueIO

-- | Retain the greatest offered generation. An exact-generation re-offer
-- refreshes the payload without duplicating queued or in-progress work. Once
-- the preceding physical send has completed, the next re-offer queues one
-- retransmission so a delayed response cannot suppress later heartbeats.
offerRetainedRequest ::
  (Ord generation) =>
  generation ->
  payload ->
  RetainedRequestQueue generation lease payload ->
  STM RetainedRequestOffer
offerRetainedRequest generation payload queue = do
  state <- readTVar queue.retainedRequestState
  case state of
    RetainedRequestEmpty -> enqueue
    RetainedRequestAcknowledged acknowledged
      | generation <= acknowledged ->
          pure RetainedRequestSuppressed
      | otherwise -> enqueue
    RetainedRequestCurrent current _ delivery
      | generation < current ->
          pure RetainedRequestSuppressed
      | generation == current ->
          case delivery of
            RetainedRequestSent _ -> enqueue
            _ -> do
              writeTVar
                queue.retainedRequestState
                (RetainedRequestCurrent current payload delivery)
              pure RetainedRequestCoalesced
      | otherwise -> enqueue
  where
    enqueue = do
      writeTVar
        queue.retainedRequestState
        (RetainedRequestCurrent generation payload RetainedRequestQueued)
      writeTQueue queue.retainedRequestWakes generation
      pure RetainedRequestEnqueued

-- | Wait for the current logical generation to need a send, discard any wake
-- tokens left by superseded generations, and lend the current payload to this
-- physical lease.
claimRetainedRequest ::
  (Eq generation) =>
  lease ->
  RetainedRequestQueue generation lease payload ->
  STM (RetainedRequestClaim generation lease payload)
claimRetainedRequest lease queue = do
  state <- readTVar queue.retainedRequestState
  case state of
    RetainedRequestCurrent generation payload RetainedRequestQueued -> do
      wake <- readTQueue queue.retainedRequestWakes
      if wake == generation
        then do
          writeTVar
            queue.retainedRequestState
            ( RetainedRequestCurrent
                generation
                payload
                (RetainedRequestSending lease)
            )
          pure (RetainedRequestClaim generation lease payload)
        else claimRetainedRequest lease queue
    RetainedRequestCurrent _ _ RetainedRequestSending {} -> retry
    RetainedRequestCurrent _ _ RetainedRequestSent {} -> retry
    RetainedRequestEmpty -> retry
    RetainedRequestAcknowledged {} -> retry

retainedRequestClaimGeneration ::
  RetainedRequestClaim generation lease payload ->
  generation
retainedRequestClaimGeneration (RetainedRequestClaim generation _ _) = generation

retainedRequestClaimPayload ::
  RetainedRequestClaim generation lease payload ->
  payload
retainedRequestClaimPayload (RetainedRequestClaim _ _ payload) = payload

-- | Mark a successful physical send.  A superseding offer, acknowledgement,
-- or lease retirement can win this race; in that case the late completion
-- cannot alter the newer retained state.
completeRetainedRequestSend ::
  (Eq generation, Eq lease) =>
  RetainedRequestClaim generation lease payload ->
  RetainedRequestQueue generation lease payload ->
  STM Bool
completeRetainedRequestSend
  (RetainedRequestClaim generation lease _)
  queue = do
    state <- readTVar queue.retainedRequestState
    case state of
      RetainedRequestCurrent current payload (RetainedRequestSending owner)
        | current == generation && owner == lease -> do
            writeTVar
              queue.retainedRequestState
              (RetainedRequestCurrent current payload (RetainedRequestSent lease))
            pure True
      _ -> pure False

-- | Make the request owned by a disappearing physical lease available to its
-- replacement.  Repeated retirement is idempotent and therefore writes at
-- most one wake token.
retireRetainedRequestLease ::
  (Eq lease) =>
  lease ->
  RetainedRequestQueue generation lease payload ->
  STM Bool
retireRetainedRequestLease lease queue = do
  state <- readTVar queue.retainedRequestState
  case state of
    RetainedRequestCurrent generation payload delivery
      | ownedBy lease delivery -> do
          writeTVar
            queue.retainedRequestState
            (RetainedRequestCurrent generation payload RetainedRequestQueued)
          writeTQueue queue.retainedRequestWakes generation
          pure True
    _ -> pure False
  where
    ownedBy expected = \case
      RetainedRequestSending actual -> expected == actual
      RetainedRequestSent actual -> expected == actual
      RetainedRequestQueued -> False

-- | Retire exactly the logical generation named by a valid matching response.
-- The acknowledgement remains as a tombstone until a greater generation is
-- offered, so a racing duplicate effect cannot resurrect the completed RPC.
-- A response for an older wire correlation never clears a newer request.
acknowledgeRetainedRequest ::
  (Ord generation) =>
  generation ->
  RetainedRequestQueue generation lease payload ->
  STM Bool
acknowledgeRetainedRequest generation queue = do
  state <- readTVar queue.retainedRequestState
  case state of
    RetainedRequestCurrent current _ _
      | generation == current -> acknowledge
      | otherwise -> pure False
    RetainedRequestEmpty -> acknowledge
    RetainedRequestAcknowledged current
      | generation > current -> acknowledge
      | otherwise -> pure False
  where
    acknowledge = do
      writeTVar
        queue.retainedRequestState
        (RetainedRequestAcknowledged generation)
      pure True
