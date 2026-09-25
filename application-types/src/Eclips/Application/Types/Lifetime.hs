-- | Explicit consumption frontiers for one application session. These are
-- lifetime acknowledgements, independent of the identity of a semantic request.
module Eclips.Application.Types.Lifetime
  ( ApplicationReceiptRetirement,
    applicationReceiptRetirement,
    applicationReceiptRetirementWithExceptions,
    emptyApplicationReceiptRetirement,
    ordinaryReceiptRetirement,
    lifecycleReceiptRetirement,
    ordinaryReceiptRetirementThrough,
    lifecycleReceiptRetirementThrough,
  )
where

import Data.Binary (Binary (..))
import Data.Word (Word64)
import Eclips.Public.Types.ReceiptRetirement

-- | Independent high-water marks with unresolved exceptions. Zero is an
-- ordinary request identity and is never used as an empty sentinel.
data ApplicationReceiptRetirement = ApplicationReceiptRetirement ReceiptRetirement ReceiptRetirement
  deriving stock (Eq, Ord, Show)

applicationReceiptRetirement :: Maybe Word64 -> Maybe Word64 -> ApplicationReceiptRetirement
applicationReceiptRetirement ordinary lifecycle = ApplicationReceiptRetirement (receiptRetirementPrefix ordinary) (receiptRetirementPrefix lifecycle)

applicationReceiptRetirementWithExceptions :: ReceiptRetirement -> ReceiptRetirement -> ApplicationReceiptRetirement
applicationReceiptRetirementWithExceptions = ApplicationReceiptRetirement

emptyApplicationReceiptRetirement :: ApplicationReceiptRetirement
emptyApplicationReceiptRetirement = ApplicationReceiptRetirement mempty mempty

ordinaryReceiptRetirement :: ApplicationReceiptRetirement -> ReceiptRetirement
ordinaryReceiptRetirement (ApplicationReceiptRetirement ordinary _) = ordinary

lifecycleReceiptRetirement :: ApplicationReceiptRetirement -> ReceiptRetirement
lifecycleReceiptRetirement (ApplicationReceiptRetirement _ lifecycle) = lifecycle

ordinaryReceiptRetirementThrough :: ApplicationReceiptRetirement -> Maybe Word64
ordinaryReceiptRetirementThrough = receiptRetirementHighWater . ordinaryReceiptRetirement

lifecycleReceiptRetirementThrough :: ApplicationReceiptRetirement -> Maybe Word64
lifecycleReceiptRetirementThrough = receiptRetirementHighWater . lifecycleReceiptRetirement

instance Semigroup ApplicationReceiptRetirement where
  ApplicationReceiptRetirement ordinary lifecycle <> ApplicationReceiptRetirement nextOrdinary nextLifecycle =
    ApplicationReceiptRetirement (ordinary <> nextOrdinary) (lifecycle <> nextLifecycle)

instance Monoid ApplicationReceiptRetirement where
  mempty = emptyApplicationReceiptRetirement

instance Binary ApplicationReceiptRetirement where
  put (ApplicationReceiptRetirement ordinary lifecycle) = put ordinary >> put lifecycle
  get = ApplicationReceiptRetirement <$> get <*> get
