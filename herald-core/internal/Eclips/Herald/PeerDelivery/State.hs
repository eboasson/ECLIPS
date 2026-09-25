-- | Per-peer delivery receipts for immutable alignment evidence. Semantic
-- evidence remains with its owner; this state retains only the unconfirmed
-- outbound tail and compact receive progress across physical bindings.
module Eclips.Herald.PeerDelivery.State
  ( State,
    DeliveryProblem (..),
    emptyState,
    offer,
    acknowledge,
    receive,
    receiptProgress,
    outstanding,
    retainedCounts,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Alignment
  ( AlignmentDeliverySequence,
    alignmentDeliverySequenceWord64,
    firstAlignmentDeliverySequence,
    nextAlignmentDeliverySequence,
  )
import Eclips.Public.Types.ReceiptRetirement
  ( ReceiptRetirement,
    emptyReceiptRetirement,
    receiptIsRetired,
    receiptRetirement,
    receiptRetirementExceptions,
    receiptRetirementHighWater,
  )

data Pending key payload = Pending !key !payload
  deriving stock (Eq, Show)

-- The key index contains exactly the outstanding records. Confirmed keys are
-- deliberately forgotten: reconnect must replay 'outstanding', not re-offer a
-- semantic history catalogue as if it consisted of new deliveries.
data State key payload
  = State
      !AlignmentDeliverySequence
      !(Map AlignmentDeliverySequence (Pending key payload))
      !(Map key AlignmentDeliverySequence)
      !ReceiptRetirement
  deriving stock (Eq, Show)

data DeliveryProblem
  = DeliveryConflictingPendingKey
  | DeliveryInvalidAcknowledgement
  | DeliveryAcknowledgementBeyondAllocation
  deriving stock (Eq, Show)

emptyState :: State key payload
emptyState = State firstAlignmentDeliverySequence Map.empty Map.empty emptyReceiptRetirement

offer ::
  (Ord key, Eq payload) =>
  key ->
  payload ->
  State key payload ->
  Either DeliveryProblem (State key payload, AlignmentDeliverySequence)
offer key payload state@(State next pending keys received) =
  case Map.lookup key keys of
    Just sequenceNumber ->
      case Map.lookup sequenceNumber pending of
        Just (Pending _ incumbent)
          | incumbent == payload -> Right (state, sequenceNumber)
          | otherwise -> Left DeliveryConflictingPendingKey
        Nothing -> error "peer delivery key index has no outstanding record"
    Nothing ->
      Right
        ( State
            (nextAlignmentDeliverySequence next)
            (Map.insert next (Pending key payload) pending)
            (Map.insert key next keys)
            received,
          next
        )

-- | A stale receipt can only release more outstanding work; it cannot recreate
-- an old key or payload. Restrict the scan to positions covered by this receipt.
acknowledge ::
  (Ord key) =>
  ReceiptRetirement ->
  State key payload ->
  Either DeliveryProblem (State key payload)
acknowledge progress state@(State next pending keys received)
  | receiptRetirementHighWater progress == Just 0
      || Set.member 0 (receiptRetirementExceptions progress) =
      Left DeliveryInvalidAcknowledgement
  | maybe False (>= alignmentDeliverySequenceWord64 next) (receiptRetirementHighWater progress) =
      Left DeliveryAcknowledgementBeyondAllocation
  | otherwise = case receiptRetirementHighWater progress of
      Nothing -> Right state
      Just high ->
        let (covered, later) = Map.spanAntitone ((<= high) . alignmentDeliverySequenceWord64) pending
            (released, unresolved) =
              Map.partitionWithKey
                (\sequenceNumber _ -> receiptIsRetired (alignmentDeliverySequenceWord64 sequenceNumber) progress)
                covered
            retainedKeys = foldl' (\index (Pending key _) -> Map.delete key index) keys released
         in Right (State next (Map.union unresolved later) retainedKeys received)

-- | Record receipt only after the semantic owner has admitted the evidence or
-- retained its dependencies. Missing positions are the only receive history;
-- filling a gap drops that exception, and an old duplicate stores nothing.
receive ::
  AlignmentDeliverySequence ->
  State key payload ->
  (State key payload, Bool)
receive sequenceNumber state@(State next pending keys received)
  | receiptIsRetired position received = (state, False)
  | otherwise =
      let oldHigh = maybe 0 id (receiptRetirementHighWater received)
          missing = receiptRetirementExceptions received
          successorMissing
            | position > oldHigh = Set.union missing (Set.fromDistinctAscList [oldHigh + 1 .. position - 1])
            | otherwise = Set.delete position missing
          successor =
            either
              (error . ("peer delivery receive invariant: " <>) . show)
              id
              (receiptRetirement (Just (max oldHigh position)) successorMissing)
       in (State next pending keys successor, True)
  where
    position = alignmentDeliverySequenceWord64 sequenceNumber

receiptProgress :: State key payload -> ReceiptRetirement
receiptProgress (State _ _ _ received) = received

outstanding :: State key payload -> [(AlignmentDeliverySequence, payload)]
outstanding (State _ pending _ _) =
  [(sequenceNumber, payload) | (sequenceNumber, Pending _ payload) <- Map.toAscList pending]

-- | Outstanding payloads, outstanding key-index entries, receive exceptions.
-- The remaining retained coordinates have constant size.
retainedCounts :: State key payload -> (Int, Int, Int)
retainedCounts (State _ pending keys received) =
  (Map.size pending, Map.size keys, Set.size (receiptRetirementExceptions received))
