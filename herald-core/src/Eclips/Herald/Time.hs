-- | Explicit monotonic observations supplied to the pure Herald kernel.
module Eclips.Herald.Time
  ( MonotonicInstant,
    monotonicInstant,
    monotonicInstantWord64,
  )
where

import Data.Word (Word64)

-- | One runtime-supplied monotonic observation, expressed in microseconds.
--
-- Profile 0.1 assumes finite runs far below counter exhaustion.  This type does
-- not read a clock or define rollover behaviour.
newtype MonotonicInstant = MonotonicInstant Word64
  deriving stock (Eq, Ord, Show)

monotonicInstant :: Word64 -> MonotonicInstant
monotonicInstant = MonotonicInstant

monotonicInstantWord64 :: MonotonicInstant -> Word64
monotonicInstantWord64 (MonotonicInstant value) = value
