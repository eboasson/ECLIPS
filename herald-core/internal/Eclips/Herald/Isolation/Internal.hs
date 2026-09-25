-- | Checked local-isolation configuration and nominal owner identities.
--
-- These values describe one Herald incarnation's local failure-detector
-- policy.  They are deliberately absent from canonical Oracle state: changing
-- a grace or drain duration cannot change membership truth or a protocol
-- digest.
module Eclips.Herald.Isolation.Internal
  ( IsolationConfiguration (..),
    IsolationConfigurationError (..),
    checkIsolationConfiguration,
    isolationGraceMicroseconds,
    isolationDrainMicroseconds,
    IsolationGraceGeneration (..),
    firstIsolationGraceGeneration,
    nextIsolationGraceGeneration,
    isolationGraceGenerationWord64,
    IsolationDrainId (..),
    firstIsolationDrainId,
    nextIsolationDrainId,
    isolationDrainIdWord64,
  )
where

import Data.Word (Word64)

-- | Checked positive absolute-grace and read-only-drain durations, in
-- monotonic microseconds.
data IsolationConfiguration
  = IsolationConfiguration Word64 Word64
  deriving stock (Eq, Show)

-- | Configuration-shape failures detected before a Herald starts.
data IsolationConfigurationError
  = IsolationGraceZero
  | IsolationDrainZero
  deriving stock (Eq, Ord, Show)

-- | Check the absolute isolation grace and bounded read-only drain duration.
checkIsolationConfiguration ::
  Word64 ->
  Word64 ->
  Either IsolationConfigurationError IsolationConfiguration
checkIsolationConfiguration 0 _ = Left IsolationGraceZero
checkIsolationConfiguration _ 0 = Left IsolationDrainZero
checkIsolationConfiguration grace drain =
  Right (IsolationConfiguration grace drain)

-- | Observe the checked isolation grace in monotonic microseconds.
isolationGraceMicroseconds :: IsolationConfiguration -> Word64
isolationGraceMicroseconds (IsolationConfiguration grace _) = grace

-- | Observe the checked read-only drain duration in monotonic microseconds.
isolationDrainMicroseconds :: IsolationConfiguration -> Word64
isolationDrainMicroseconds (IsolationConfiguration _ drain) = drain

-- | Never-reused generation of one below-voter-quorum grace episode.
newtype IsolationGraceGeneration
  = IsolationGraceGeneration Word64
  deriving stock (Eq, Ord, Show)

firstIsolationGraceGeneration :: IsolationGraceGeneration
firstIsolationGraceGeneration = IsolationGraceGeneration 1

-- | Advance one finite-run generation. Exhaustion is outside profile 0.1.
nextIsolationGraceGeneration ::
  IsolationGraceGeneration -> IsolationGraceGeneration
nextIsolationGraceGeneration (IsolationGraceGeneration generation) =
  IsolationGraceGeneration (generation + 1)

isolationGraceGenerationWord64 :: IsolationGraceGeneration -> Word64
isolationGraceGenerationWord64 (IsolationGraceGeneration generation) = generation

-- | Never-reused identity of one local read-only isolation drain.
newtype IsolationDrainId
  = IsolationDrainId Word64
  deriving stock (Eq, Ord, Show)

firstIsolationDrainId :: IsolationDrainId
firstIsolationDrainId = IsolationDrainId 1

-- | Advance one finite-run identity. Exhaustion is outside profile 0.1.
nextIsolationDrainId :: IsolationDrainId -> IsolationDrainId
nextIsolationDrainId (IsolationDrainId identifier) =
  IsolationDrainId (identifier + 1)

isolationDrainIdWord64 :: IsolationDrainId -> Word64
isolationDrainIdWord64 (IsolationDrainId identifier) = identifier
