-- | Shared deployment timing derived from a takeover decision target.
--
-- The target budgets failure detection, reconnection, a fresh probe and committed
-- authority changes. It is an operating target for a responsive surviving
-- quorum, never a deadline which grants authority by itself. Durations are
-- monotonic microseconds; fractional results round down to that resolution.
module Eclips.Public.Types.Timing
  ( TimingError (..),
    TakeoverTarget,
    takeoverTarget,
    takeoverTargetMicroseconds,
    defaultTakeoverTarget,
    TimingPolicy,
    deriveTimingPolicy,
    timingHeartbeatIdleMicroseconds,
    timingHeartbeatReplyMicroseconds,
    timingPeerRecoveryGraceMicroseconds,
    timingFailureProbeMicroseconds,
    timingRaftHeartbeatMicroseconds,
    timingRaftElectionLowerMicroseconds,
    timingRaftElectionUpperMicroseconds,
    timingHealthRoundMicroseconds,
    timingHealthQueryMicroseconds,
    timingIsolationGraceMicroseconds,
    timingApplicationRecoveryGraceMicroseconds,
    timingReconnectInitialMicroseconds,
    timingSubmissionInitialMicroseconds,
    timingRetryCapMicroseconds,
    defaultApplicationReplyTimeoutMicroseconds,
    defaultReadOnlyDrainMicroseconds,
  )
where

import Data.Binary (Binary (get, put))
import Data.Word (Word64)

-- | The smallest derived duration must occupy at least one microsecond.
data TimingError = TakeoverTargetBelowMicrosecondResolution
  deriving stock (Eq, Show)

-- | Checked target; its constructor is hidden so all derived waits are positive.
newtype TakeoverTarget = TakeoverTarget Word64
  deriving stock (Eq, Ord, Show)

-- | Admit a target whose @T / 500@ initial reconnect delay is representable.
-- This is a duration-resolution constraint, not an operational latency limit.
takeoverTarget :: Word64 -> Either TimingError TakeoverTarget
takeoverTarget microseconds
  | microseconds < 500 = Left TakeoverTargetBelowMicrosecondResolution
  | otherwise = Right (TakeoverTarget microseconds)

takeoverTargetMicroseconds :: TakeoverTarget -> Word64
takeoverTargetMicroseconds (TakeoverTarget microseconds) = microseconds

-- | Five seconds for a committed takeover decision, excluding application startup.
defaultTakeoverTarget :: TakeoverTarget
defaultTakeoverTarget = TakeoverTarget 5_000_000

-- | Current-build encoding, with admission repeated during decoding.
instance Binary TakeoverTarget where
  put = put . takeoverTargetMicroseconds
  get = get >>= either (fail . show) pure . takeoverTarget

-- | Immutable derived defaults. Independent explicit overrides belong in the
-- checked configuration of the relevant runtime.
newtype TimingPolicy = TimingPolicy TakeoverTarget
  deriving stock (Eq, Show)

deriveTimingPolicy :: TakeoverTarget -> TimingPolicy
deriveTimingPolicy = TimingPolicy

timingHeartbeatIdleMicroseconds :: TimingPolicy -> Word64
timingHeartbeatIdleMicroseconds = fraction 1 20

timingHeartbeatReplyMicroseconds :: TimingPolicy -> Word64
timingHeartbeatReplyMicroseconds = fraction 1 10

timingPeerRecoveryGraceMicroseconds :: TimingPolicy -> Word64
timingPeerRecoveryGraceMicroseconds = fraction 2 5

timingFailureProbeMicroseconds :: TimingPolicy -> Word64
timingFailureProbeMicroseconds = fraction 1 10

timingRaftHeartbeatMicroseconds :: TimingPolicy -> Word64
timingRaftHeartbeatMicroseconds = fraction 1 200

timingRaftElectionLowerMicroseconds :: TimingPolicy -> Word64
timingRaftElectionLowerMicroseconds = fraction 1 20

timingRaftElectionUpperMicroseconds :: TimingPolicy -> Word64
timingRaftElectionUpperMicroseconds = fraction 1 10

timingHealthRoundMicroseconds :: TimingPolicy -> Word64
timingHealthRoundMicroseconds = fraction 1 20

timingHealthQueryMicroseconds :: TimingPolicy -> Word64
timingHealthQueryMicroseconds = fraction 1 50

timingIsolationGraceMicroseconds :: TimingPolicy -> Word64
timingIsolationGraceMicroseconds = fraction 3 5

timingApplicationRecoveryGraceMicroseconds :: TimingPolicy -> Word64
timingApplicationRecoveryGraceMicroseconds = fraction 2 5

timingReconnectInitialMicroseconds :: TimingPolicy -> Word64
timingReconnectInitialMicroseconds = fraction 1 500

timingSubmissionInitialMicroseconds :: TimingPolicy -> Word64
timingSubmissionInitialMicroseconds = fraction 1 50

timingRetryCapMicroseconds :: TimingPolicy -> Word64
timingRetryCapMicroseconds = fraction 1 20

-- | Application transport response allowance, including initial establishment,
-- heartbeat replies and the final session-close write. It is independent of
-- peer failure detection and does not impose a deadline on semantic operations.
defaultApplicationReplyTimeoutMicroseconds :: Word64
defaultApplicationReplyTimeoutMicroseconds = 10_000_000

-- | Cleanup after irreversible fencing is independent of the takeover target.
defaultReadOnlyDrainMicroseconds :: Word64
defaultReadOnlyDrainMicroseconds = 500_000

fraction :: Integer -> Integer -> TimingPolicy -> Word64
fraction numerator denominator (TimingPolicy (TakeoverTarget microseconds)) =
  fromInteger (toInteger microseconds * numerator `div` denominator)
