{-# LANGUAGE OverloadedStrings #-}

-- | Operator-facing selection and rendering of the shared run timing policy.
module Eclips.Deployment.Timing
  ( TakeoverTarget,
    defaultTakeoverTarget,
    takeoverTarget,
    takeoverTargetMicroseconds,
    parseTakeoverTarget,
    takeoverTimingLines,
  )
where

import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Eclips.Public.Types.Timing
import Text.Read (readMaybe)

-- | Exact decimal durations, for example @5s@, @2.5s@ or @5000ms@.
-- Sub-microsecond values reject rather than silently changing the target.
parseTakeoverTarget :: Text -> Either String TakeoverTarget
parseTakeoverTarget supplied = do
  (number, multiplier) <- case () of
    _
      | Just number <- Text.stripSuffix "ms" supplied -> Right (number, 1000)
      | Just number <- Text.stripSuffix "us" supplied -> Right (number, 1)
      | Just number <- Text.stripSuffix "s" supplied -> Right (number, 1000000)
      | otherwise -> Left "takeover target requires s, ms or us, for example 5s"
  (digits, denominator) <- case Text.splitOn "." number of
    [whole] | decimal whole -> Right (whole, 1)
    [whole, fraction] | decimal whole && decimal fraction -> Right (whole <> fraction, 10 ^ Text.length fraction)
    _ -> Left "takeover target must be a positive decimal duration"
  numerator <- maybe (Left "invalid takeover target") Right (readMaybe (Text.unpack digits) :: Maybe Integer)
  let (microseconds, remainder) = (numerator * multiplier) `divMod` denominator
  if remainder /= 0 then Left "takeover target must be an exact number of microseconds" else pure ()
  value <- maybe (Left "takeover target is not representable in microseconds") Right (readMaybe (show microseconds) :: Maybe Word64)
  either (Left . show) Right (takeoverTarget value)
  where
    decimal text = not (Text.null text) && Text.all (\c -> isDigit c && c <= '9') text

-- | Report every resolved duration in microseconds, without changing authority.
takeoverTimingLines :: TakeoverTarget -> [Text]
takeoverTimingLines target =
  let timing = deriveTimingPolicy target
      line name value = "timing " <> name <> " " <> Text.pack (show value) <> "us"
   in line "takeover-target" (takeoverTargetMicroseconds target)
        : [ line name (readDuration timing)
          | (name, readDuration) <-
              [ ("heartbeat-idle", timingHeartbeatIdleMicroseconds),
                ("heartbeat-reply", timingHeartbeatReplyMicroseconds),
                ("peer-recovery", timingPeerRecoveryGraceMicroseconds),
                ("failure-probe", timingFailureProbeMicroseconds),
                ("raft-heartbeat", timingRaftHeartbeatMicroseconds),
                ("raft-election-lower", timingRaftElectionLowerMicroseconds),
                ("raft-election-upper", timingRaftElectionUpperMicroseconds),
                ("health-round", timingHealthRoundMicroseconds),
                ("health-query", timingHealthQueryMicroseconds),
                ("isolation-grace", timingIsolationGraceMicroseconds),
                ("application-recovery", timingApplicationRecoveryGraceMicroseconds),
                ("reconnect-initial", timingReconnectInitialMicroseconds),
                ("submission-initial", timingSubmissionInitialMicroseconds),
                ("retry-cap", timingRetryCapMicroseconds)
              ]
          ]
          <> [ line "application-reply" defaultApplicationReplyTimeoutMicroseconds,
               line "read-only-drain" defaultReadOnlyDrainMicroseconds
             ]
