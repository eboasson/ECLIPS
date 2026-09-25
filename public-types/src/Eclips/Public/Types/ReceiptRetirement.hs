-- | Monotone receipt consumption for a sequential request namespace. A high
-- water mark seals all identities below it except explicitly unresolved work.
module Eclips.Public.Types.ReceiptRetirement
  ( ReceiptRetirement,
    ReceiptRetirementError (..),
    receiptRetirement,
    receiptRetirementPrefix,
    emptyReceiptRetirement,
    receiptRetirementHighWater,
    receiptRetirementExceptions,
    receiptIsRetired,
  )
where

import Data.Binary (Binary (..))
import Data.Binary.Get (getWord8)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)

data ReceiptRetirement = ReceiptRetirement (Maybe Word64) (Set Word64)
  deriving stock (Eq, Ord, Show)

data ReceiptRetirementError = ReceiptRetirementExceptionAboveHighWater
  deriving stock (Eq, Show)

receiptRetirement :: Maybe Word64 -> Set Word64 -> Either ReceiptRetirementError ReceiptRetirement
receiptRetirement high exceptions
  | maybe (Set.null exceptions) (\through -> maybe True (<= through) (Set.lookupMax exceptions)) high =
      Right (ReceiptRetirement high exceptions)
  | otherwise = Left ReceiptRetirementExceptionAboveHighWater

receiptRetirementPrefix :: Maybe Word64 -> ReceiptRetirement
receiptRetirementPrefix high = ReceiptRetirement high Set.empty

emptyReceiptRetirement :: ReceiptRetirement
emptyReceiptRetirement = receiptRetirementPrefix Nothing

receiptRetirementHighWater :: ReceiptRetirement -> Maybe Word64
receiptRetirementHighWater (ReceiptRetirement high _) = high

receiptRetirementExceptions :: ReceiptRetirement -> Set Word64
receiptRetirementExceptions (ReceiptRetirement _ exceptions) = exceptions

receiptIsRetired :: Word64 -> ReceiptRetirement -> Bool
receiptIsRetired request (ReceiptRetirement high exceptions) =
  maybe False (request <=) high && Set.notMember request exceptions

-- Union of the released identities. An older announcement cannot reopen an
-- already released exception; neither can a newer one that repeats it.
instance Semigroup ReceiptRetirement where
  first@(ReceiptRetirement high exceptions) <> second@(ReceiptRetirement nextHigh nextExceptions) =
    ReceiptRetirement
      (max high nextHigh)
      (Set.filter (\request -> not (receiptIsRetired request first || receiptIsRetired request second)) (Set.union exceptions nextExceptions))

instance Monoid ReceiptRetirement where
  mempty = emptyReceiptRetirement

instance Binary ReceiptRetirement where
  put (ReceiptRetirement high exceptions) = put high >> put (Set.toAscList exceptions)
  get = do
    high <-
      getWord8 >>= \case
        0 -> pure Nothing
        1 -> Just <$> get
        _ -> fail "ReceiptRetirement high water must use the canonical optional tag"
    exceptions <- get
    let canonical = Set.fromList exceptions
    if Set.toAscList canonical /= exceptions
      then fail "ReceiptRetirement exceptions must be strictly ascending"
      else either (fail . show) pure (receiptRetirement high canonical)
