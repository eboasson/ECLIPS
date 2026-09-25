-- | Opaque logical timer attempts and runtime-supplied outcomes.
--
-- The pure owner allocates attempts. A runtime may compare and key them, arm
-- the supplied absolute specification, and return an outcome; it cannot mint a
-- semantic timer generation.
module Eclips.Herald.Timer
  ( TimerAttempt,
    TimerAttemptGeneration,
    timerAttemptGeneration,
    timerAttemptGenerationWord64,
    TimerSpec,
    timerSpecAbsoluteDeadline,
    TimerOutcome (..),
  )
where

import Eclips.Herald.Timer.Internal
  ( TimerAttempt,
    TimerAttemptGeneration,
    TimerOutcome (..),
    TimerSpec,
    timerAttemptGeneration,
    timerAttemptGenerationWord64,
    timerSpecAbsoluteDeadline,
  )
