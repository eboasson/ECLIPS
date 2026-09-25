-- | Atomic ownership and handoff for a queue shared across physical leases.
module Eclips.Oracle.Runtime.Internal.LeaseQueue
  ( LeaseQueue,
    LeaseClaim,
    newLeaseQueueIO,
    writeLeaseQueue,
    claimCurrentLeaseItem,
    leaseClaimItem,
    completeLeaseClaim,
    retireLeaseClaims,
  )
where

import Control.Concurrent.STM
  ( STM,
    TQueue,
    TVar,
    flushTQueue,
    newTQueueIO,
    newTVarIO,
    readTQueue,
    readTVar,
    retry,
    unGetTQueue,
    writeTQueue,
    writeTVar,
  )

-- | One pending FIFO plus the item currently owned by its physical writer.
-- There is at most one writer claim across every lease generation.
data LeaseQueue lease item = LeaseQueue
  { pendingItems :: TQueue item,
    inFlightItem :: TVar (Maybe (LeaseClaim lease item))
  }

-- | The lease generation which removed one item from the pending FIFO.
-- The item remains owned by the queue until this token is completed or the
-- lease is retired.
data LeaseClaim lease item = LeaseClaim lease item

newLeaseQueueIO :: IO (LeaseQueue lease item)
newLeaseQueueIO =
  LeaseQueue
    <$> newTQueueIO
    <*> newTVarIO Nothing

writeLeaseQueue :: LeaseQueue lease item -> item -> STM ()
writeLeaseQueue queue = writeTQueue queue.pendingItems

-- | Claim the FIFO head only while the supplied physical lease is current.
-- Keeping the ownership read, in-flight slot, and blocking queue read in one
-- transaction prevents an obsolete blocked writer from stealing a later
-- lease's item.
claimCurrentLeaseItem ::
  lease ->
  STM Bool ->
  LeaseQueue lease item ->
  STM (Maybe (LeaseClaim lease item))
claimCurrentLeaseItem lease leaseIsCurrent queue = do
  current <- leaseIsCurrent
  if not current
    then pure Nothing
    else do
      inFlight <- readTVar queue.inFlightItem
      case inFlight of
        Just _ -> retry
        Nothing -> do
          item <- readTQueue queue.pendingItems
          let claim = LeaseClaim lease item
          writeTVar queue.inFlightItem (Just claim)
          pure (Just claim)

leaseClaimItem :: LeaseClaim lease item -> item
leaseClaimItem (LeaseClaim _ item) = item

-- | Complete this exact writer claim. A late completion from a retired writer
-- cannot clear the replacement writer's claim.
completeLeaseClaim ::
  (Eq lease) =>
  LeaseClaim lease item ->
  LeaseQueue lease item ->
  STM Bool
completeLeaseClaim (LeaseClaim lease _) queue = do
  inFlight <- readTVar queue.inFlightItem
  case inFlight of
    Just (LeaseClaim currentLease _)
      | currentLease == lease -> do
          writeTVar queue.inFlightItem Nothing
          pure True
    _ -> pure False

-- | Retire one physical lease atomically with its replacement/removal. A
-- portable claimed item returns to the FIFO head; an item local to this lease
-- is discarded whether claimed or still pending. Every other pending item
-- retains its exact FIFO position.
retireLeaseClaims ::
  (Eq lease) =>
  (item -> Maybe lease) ->
  lease ->
  LeaseQueue lease item ->
  STM ()
retireLeaseClaims bindingLocalLease lease queue = do
  pending <- flushTQueue queue.pendingItems
  mapM_
    (writeTQueue queue.pendingItems)
    [ item
    | item <- pending,
      bindingLocalLease item /= Just lease
    ]
  inFlight <- readTVar queue.inFlightItem
  case inFlight of
    Just (LeaseClaim currentLease item)
      | currentLease == lease -> do
          if bindingLocalLease item /= Just lease
            then unGetTQueue queue.pendingItems item
            else pure ()
          writeTVar queue.inFlightItem Nothing
    _ -> pure ()
