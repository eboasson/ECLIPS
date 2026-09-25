-- | Pure pacing for native transport reconnects. A TCP connection alone does
-- not establish a lane and cannot reset repeated rejected-Hello backoff.
module Eclips.Oracle.Runtime.Internal.TCP.Retry
  ( RaftRetryPolicy,
    raftRetryPolicy,
    RaftRetryState,
    initialRaftRetryState,
    RaftRetryOutcome (..),
    nextRaftRetry,
  ) where

import Eclips.Public.Types.Timing
  ( TakeoverTarget,
    deriveTimingPolicy,
    takeoverTargetMicroseconds,
    timingReconnectInitialMicroseconds,
    timingRetryCapMicroseconds,
  )

data RaftRetryPolicy = RaftRetryPolicy Int Int Int
  deriving stock (Eq, Show)

raftRetryPolicy :: TakeoverTarget -> RaftRetryPolicy
raftRetryPolicy target = RaftRetryPolicy initial reverseFloor cap
  where
    timing = deriveTimingPolicy target
    initial = fromIntegral (timingReconnectInitialMicroseconds timing)
    cap = fromIntegral (timingRetryCapMicroseconds timing)
    reverseFloor = min cap (fromIntegral (takeoverTargetMicroseconds target `div` 50))

newtype RaftRetryState = RaftRetryState Int
  deriving stock (Eq, Show)

data RaftRetryOutcome = RaftAttemptUnestablished | RaftEstablishedLaneEnded
  deriving stock (Eq, Show)

initialRaftRetryState :: RaftRetryPolicy -> RaftRetryState
initialRaftRetryState (RaftRetryPolicy initial _ _) = RaftRetryState initial

-- | The first ended/failed attempt uses the configured initial delay;
-- consecutive unestablished attempts double it up to the configured cap.
-- Reverse registration probes retain a slower policy-derived floor.
-- An admitted established lane resets the progression when that lane ends.
nextRaftRetry :: RaftRetryPolicy -> Bool -> RaftRetryOutcome -> RaftRetryState -> (Int, RaftRetryState)
nextRaftRetry (RaftRetryPolicy initial reverseFloor cap) reverseProbe outcome (RaftRetryState retained) =
  (max minimumDelay delay, RaftRetryState (min cap (2 * delay)))
  where
    delay = case outcome of
      RaftAttemptUnestablished -> retained
      RaftEstablishedLaneEnded -> initial
    minimumDelay = if reverseProbe then reverseFloor else initial
