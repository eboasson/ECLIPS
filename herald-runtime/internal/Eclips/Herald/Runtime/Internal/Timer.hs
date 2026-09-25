-- | One supervised physical interpreter for all logical Herald timers.
--
-- The manager never invents a timer identity or generation. It keys exact
-- attempts supplied by the pure kernel, waits against their absolute
-- microsecond deadlines, and returns the same attempt with a typed outcome.
module Eclips.Herald.Runtime.Internal.Timer
  ( RuntimeTimerCommand (..),
    RuntimeTimerScheduler (..),
    RuntimeTimerClock (..),
    runRuntimeTimerManager,
    systemRuntimeTimerScheduler,
    systemRuntimeTimerClock,
  )
where

import Control.Concurrent.STM
  ( STM,
    TQueue,
    atomically,
    check,
    orElse,
    readTQueue,
    readTVar,
    registerDelay,
  )
import Control.Monad (forM_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Eclips.Herald.Time
  ( MonotonicInstant,
    monotonicInstant,
    monotonicInstantWord64,
  )
import Eclips.Herald.Timer
  ( TimerAttempt,
    TimerOutcome (..),
    TimerSpec,
    timerSpecAbsoluteDeadline,
  )
import GHC.Clock (getMonotonicTimeNSec)

-- | Commands are ordered by the dispatcher. Runtime supervision cancels and
-- joins the manager; logical drain emits exact cancellation commands first.
data RuntimeTimerCommand
  = ArmRuntimeTimer TimerAttempt TimerSpec
  | CancelRuntimeTimer TimerAttempt

-- | Injectable physical delay. The returned STM action becomes ready once the
-- requested positive-or-zero number of microseconds has elapsed.
newtype RuntimeTimerScheduler
  = RuntimeTimerScheduler (Word64 -> IO (STM ()))

-- | Timer workers sample independently from the same configured monotonic
-- domain used by the serialized owner.
newtype RuntimeTimerClock
  = RuntimeTimerClock (IO MonotonicInstant)

systemRuntimeTimerScheduler :: RuntimeTimerScheduler
systemRuntimeTimerScheduler =
  RuntimeTimerScheduler $ \microseconds -> do
    elapsed <- registerDelay (fromIntegral microseconds)
    pure (readTVar elapsed >>= check)

systemRuntimeTimerClock :: RuntimeTimerClock
systemRuntimeTimerClock =
  RuntimeTimerClock
    (monotonicInstant . (`div` 1_000) <$> getMonotonicTimeNSec)

-- | Interpret a dynamically keyed set of absolute timers on one supervised
-- worker. Superseding commands remain responsive while a physical wait is
-- pending. Failure to allocate the host wait terminates this essential worker:
-- without a trustworthy recovery deadline, the runtime cannot safely infer
-- liveness, and profile resource failure ends the experimental run.
runRuntimeTimerManager ::
  RuntimeTimerScheduler ->
  RuntimeTimerClock ->
  TQueue RuntimeTimerCommand ->
  (TimerAttempt -> TimerOutcome -> IO ()) ->
  IO ()
runRuntimeTimerManager
  (RuntimeTimerScheduler schedule)
  (RuntimeTimerClock clock)
  commands
  observe = go Map.empty
    where
      go timers
        | Map.null timers = do
            command <- atomically (readTQueue commands)
            continue timers command
      go timers = do
        now <- clock
        let (due, retained) = Map.partition (deadlineReached now) timers
        if Map.null due
          then waitForCommandOrDeadline now retained
          else do
            forM_ (Map.keys due) (\attempt -> observe attempt (TimerFired now))
            go retained

      waitForCommandOrDeadline now timers = do
        case earliestTimer timers of
          Nothing -> go timers
          Just (_, spec) -> do
            let remaining = deadlineDifference now (timerSpecAbsoluteDeadline spec)
            elapsed <- schedule remaining
            next <-
              atomically
                ( (Left <$> readTQueue commands)
                    `orElse` (elapsed >> pure (Right ()))
                )
            case next of
              Left command -> continue timers command
              Right () -> go timers

      continue timers command = case command of
        ArmRuntimeTimer attempt spec -> go (Map.insert attempt spec timers)
        CancelRuntimeTimer attempt -> go (Map.delete attempt timers)

deadlineReached :: MonotonicInstant -> TimerSpec -> Bool
deadlineReached now spec = timerSpecAbsoluteDeadline spec <= now

deadlineDifference :: MonotonicInstant -> MonotonicInstant -> Word64
deadlineDifference now deadline =
  monotonicInstantWord64 deadline - monotonicInstantWord64 now

earliestTimer :: Map TimerAttempt TimerSpec -> Maybe (TimerAttempt, TimerSpec)
earliestTimer timers =
  case Map.toAscList timers of
    [] -> Nothing
    first : remaining -> Just (foldl choose first remaining)
  where
    choose retained@(_, retainedSpec) candidate@(_, candidateSpec)
      | timerSpecAbsoluteDeadline candidateSpec
          < timerSpecAbsoluteDeadline retainedSpec =
          candidate
      | otherwise = retained
