-- | Checked application-recovery configuration and nominal recovery identity.
--
-- The checked configuration is operational input to one Herald incarnation. It
-- is deliberately separate from canonical genesis data: changing a local grace
-- does not change membership or a protocol digest.
module Eclips.Herald.Application.Recovery.Internal
  ( ApplicationRecoveryConfiguration (..),
    ApplicationRecoveryConfigurationError (..),
    checkApplicationRecoveryConfiguration,
    applicationRecoveryGraceMicroseconds,
    ApplicationSessionRecoveryGeneration (..),
    firstApplicationSessionRecoveryGeneration,
    nextApplicationSessionRecoveryGeneration,
    applicationSessionRecoveryGenerationWord64,
    ApplicationProcessRecoveryGeneration (..),
    firstApplicationProcessRecoveryGeneration,
    nextApplicationProcessRecoveryGeneration,
    applicationProcessRecoveryGenerationWord64,
  )
where

import Data.Word (Word64)

-- | Checked positive application-session recovery grace, expressed in
-- monotonic microseconds.
newtype ApplicationRecoveryConfiguration
  = ApplicationRecoveryConfiguration Word64
  deriving stock (Eq, Show)

-- | Configuration-shape failures detected before a Herald starts.
data ApplicationRecoveryConfigurationError
  = ApplicationRecoveryGraceZero
  deriving stock (Eq, Ord, Show)

-- | Check the absolute grace used for application session and process recovery.
checkApplicationRecoveryConfiguration ::
  Word64 -> Either ApplicationRecoveryConfigurationError ApplicationRecoveryConfiguration
checkApplicationRecoveryConfiguration 0 = Left ApplicationRecoveryGraceZero
checkApplicationRecoveryConfiguration grace =
  Right (ApplicationRecoveryConfiguration grace)

-- | Observe the checked grace in monotonic microseconds.
applicationRecoveryGraceMicroseconds :: ApplicationRecoveryConfiguration -> Word64
applicationRecoveryGraceMicroseconds (ApplicationRecoveryConfiguration grace) = grace

-- | Never-reused generation of one application session's recovery episode.
newtype ApplicationSessionRecoveryGeneration
  = ApplicationSessionRecoveryGeneration Word64
  deriving stock (Eq, Ord, Show)

firstApplicationSessionRecoveryGeneration :: ApplicationSessionRecoveryGeneration
firstApplicationSessionRecoveryGeneration = ApplicationSessionRecoveryGeneration 1

-- | Advance one finite-run generation. Exhaustion is outside profile 0.1.
nextApplicationSessionRecoveryGeneration ::
  ApplicationSessionRecoveryGeneration -> ApplicationSessionRecoveryGeneration
nextApplicationSessionRecoveryGeneration
  (ApplicationSessionRecoveryGeneration generation) =
    ApplicationSessionRecoveryGeneration (generation + 1)

applicationSessionRecoveryGenerationWord64 ::
  ApplicationSessionRecoveryGeneration -> Word64
applicationSessionRecoveryGenerationWord64
  (ApplicationSessionRecoveryGeneration generation) = generation

-- | Never-reused generation of one application process's zero-session recovery
-- episode.
newtype ApplicationProcessRecoveryGeneration
  = ApplicationProcessRecoveryGeneration Word64
  deriving stock (Eq, Ord, Show)

firstApplicationProcessRecoveryGeneration :: ApplicationProcessRecoveryGeneration
firstApplicationProcessRecoveryGeneration = ApplicationProcessRecoveryGeneration 1

-- | Advance one finite-run generation. Exhaustion is outside profile 0.1.
nextApplicationProcessRecoveryGeneration ::
  ApplicationProcessRecoveryGeneration -> ApplicationProcessRecoveryGeneration
nextApplicationProcessRecoveryGeneration
  (ApplicationProcessRecoveryGeneration generation) =
    ApplicationProcessRecoveryGeneration (generation + 1)

applicationProcessRecoveryGenerationWord64 ::
  ApplicationProcessRecoveryGeneration -> Word64
applicationProcessRecoveryGenerationWord64
  (ApplicationProcessRecoveryGeneration generation) = generation
