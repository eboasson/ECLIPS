-- | Package-private representation of application-client recovery values.
module Eclips.Application.Client.Recovery.Internal
  ( ApplicationClientRecoveryConfigurationError (..),
    ApplicationClientRecoveryConfiguration (..),
    applicationClientRecoveryConfiguration,
    applicationClientRetryDelayMicroseconds,
    applicationClientRecoveryGraceMicroseconds,
    ApplicationClientMonotonicInstant (..),
    applicationClientMonotonicInstant,
    applicationClientMonotonicInstantMicroseconds,
    ApplicationSessionRecoveryGeneration (..),
    applicationSessionRecoveryGenerationWord64,
    ApplicationTransportAttemptGeneration (..),
    applicationTransportAttemptGenerationWord64,
    ApplicationClientRecoveryDeadline (..),
    applicationClientRecoveryDeadlineMicroseconds,
    ApplicationTransportRetryScope (..),
  )
where

import Data.Word (Word64)

-- | A malformed application-client recovery policy.
data ApplicationClientRecoveryConfigurationError
  = ApplicationClientRetryDelayMustBePositive
  | ApplicationClientRecoveryGraceMustBePositive
  deriving stock (Eq, Show)

-- | Positive durations interpreted against one monotonic microsecond clock.
data ApplicationClientRecoveryConfiguration
  = ApplicationClientRecoveryConfiguration Word64 Word64
  deriving stock (Eq, Show)

-- | Check the retry delay and established-session recovery grace.
--
-- Both arguments are microseconds and must be positive. Counter exhaustion and
-- monotonic-clock wrap are outside profile 0.1.
applicationClientRecoveryConfiguration ::
  Word64 ->
  Word64 ->
  Either ApplicationClientRecoveryConfigurationError ApplicationClientRecoveryConfiguration
applicationClientRecoveryConfiguration retryDelay recoveryGrace
  | retryDelay == 0 = Left ApplicationClientRetryDelayMustBePositive
  | recoveryGrace == 0 = Left ApplicationClientRecoveryGraceMustBePositive
  | otherwise = Right (ApplicationClientRecoveryConfiguration retryDelay recoveryGrace)

-- | Observe the positive delay between successive transport attempts.
applicationClientRetryDelayMicroseconds :: ApplicationClientRecoveryConfiguration -> Word64
applicationClientRetryDelayMicroseconds (ApplicationClientRecoveryConfiguration delay _) = delay

-- | Observe the positive absolute grace for an established session.
applicationClientRecoveryGraceMicroseconds :: ApplicationClientRecoveryConfiguration -> Word64
applicationClientRecoveryGraceMicroseconds (ApplicationClientRecoveryConfiguration _ grace) = grace

-- | One observation from the runtime's monotonic microsecond clock.
newtype ApplicationClientMonotonicInstant
  = ApplicationClientMonotonicInstant Word64
  deriving stock (Eq, Ord, Show)

-- | Construct a runtime clock observation. Zero is valid.
applicationClientMonotonicInstant :: Word64 -> ApplicationClientMonotonicInstant
applicationClientMonotonicInstant = ApplicationClientMonotonicInstant

-- | Observe the monotonic-clock representation.
applicationClientMonotonicInstantMicroseconds :: ApplicationClientMonotonicInstant -> Word64
applicationClientMonotonicInstantMicroseconds (ApplicationClientMonotonicInstant value) = value

-- | One uninterrupted recovery episode for an established session.
newtype ApplicationSessionRecoveryGeneration
  = ApplicationSessionRecoveryGeneration Word64
  deriving stock (Eq, Ord, Show)

-- | Observe the owner-issued recovery generation.
applicationSessionRecoveryGenerationWord64 :: ApplicationSessionRecoveryGeneration -> Word64
applicationSessionRecoveryGenerationWord64 (ApplicationSessionRecoveryGeneration value) = value

-- | One owner-issued physical transport attempt.
newtype ApplicationTransportAttemptGeneration
  = ApplicationTransportAttemptGeneration Word64
  deriving stock (Eq, Ord, Show)

-- | Observe the owner-issued transport-attempt generation.
applicationTransportAttemptGenerationWord64 :: ApplicationTransportAttemptGeneration -> Word64
applicationTransportAttemptGenerationWord64 (ApplicationTransportAttemptGeneration value) = value

-- | The fixed absolute end of one established-session recovery episode.
newtype ApplicationClientRecoveryDeadline
  = ApplicationClientRecoveryDeadline Word64
  deriving stock (Eq, Ord, Show)

-- | Observe the deadline in monotonic microseconds.
applicationClientRecoveryDeadlineMicroseconds :: ApplicationClientRecoveryDeadline -> Word64
applicationClientRecoveryDeadlineMicroseconds (ApplicationClientRecoveryDeadline value) = value

-- | The logical subject qualifying one paced transport-retry timer.
data ApplicationTransportRetryScope
  = ApplicationOpeningRetry
  | ApplicationSessionRecoveryRetry ApplicationSessionRecoveryGeneration
  deriving stock (Eq, Ord, Show)
