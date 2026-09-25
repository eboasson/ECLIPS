-- | Checked peer-recovery configuration and nominal recovery identity.
--
-- The checked configuration is operational input to one Herald incarnation. It
-- is deliberately separate from canonical genesis data: changing a local grace
-- does not change membership or a protocol digest.
module Eclips.Herald.PeerLiveness.Internal
  ( PeerRecoveryConfiguration (..),
    PeerRecoveryConfigurationError (..),
    checkPeerRecoveryConfiguration,
    peerRecoveryGraceMicroseconds,
    PeerRecoveryGeneration (..),
    firstPeerRecoveryGeneration,
    nextPeerRecoveryGeneration,
    peerRecoveryGenerationWord64,
  )
where

import Data.Word (Word64)

-- | Checked positive peer-recovery grace, expressed in monotonic microseconds.
newtype PeerRecoveryConfiguration
  = PeerRecoveryConfiguration Word64
  deriving stock (Eq, Show)

-- | Configuration-shape failures detected before a Herald starts.
data PeerRecoveryConfigurationError
  = PeerRecoveryGraceZero
  deriving stock (Eq, Ord, Show)

-- | Check the absolute recovery grace used for every peer episode.
checkPeerRecoveryConfiguration ::
  Word64 -> Either PeerRecoveryConfigurationError PeerRecoveryConfiguration
checkPeerRecoveryConfiguration 0 = Left PeerRecoveryGraceZero
checkPeerRecoveryConfiguration grace = Right (PeerRecoveryConfiguration grace)

-- | Observe the checked grace in monotonic microseconds.
peerRecoveryGraceMicroseconds :: PeerRecoveryConfiguration -> Word64
peerRecoveryGraceMicroseconds (PeerRecoveryConfiguration grace) = grace

-- | Never-reused generation of one peer's recovery episode.
newtype PeerRecoveryGeneration = PeerRecoveryGeneration Word64
  deriving stock (Eq, Ord, Show)

firstPeerRecoveryGeneration :: PeerRecoveryGeneration
firstPeerRecoveryGeneration = PeerRecoveryGeneration 1

-- | Advance one finite-run generation. Exhaustion is outside profile 0.1.
nextPeerRecoveryGeneration :: PeerRecoveryGeneration -> PeerRecoveryGeneration
nextPeerRecoveryGeneration (PeerRecoveryGeneration generation) =
  PeerRecoveryGeneration (generation + 1)

peerRecoveryGenerationWord64 :: PeerRecoveryGeneration -> Word64
peerRecoveryGenerationWord64 (PeerRecoveryGeneration generation) = generation
