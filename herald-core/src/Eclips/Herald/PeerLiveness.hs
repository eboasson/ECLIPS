-- | Checked configuration and stable identities for pure peer recovery.
--
-- Sockets, heartbeat scheduling, and clock acquisition remain runtime-owned.
-- This module exposes only the checked startup value and opaque semantic
-- generation needed at that boundary.
module Eclips.Herald.PeerLiveness
  ( PeerRecoveryConfiguration,
    PeerRecoveryConfigurationError (..),
    checkPeerRecoveryConfiguration,
    peerRecoveryGraceMicroseconds,
    PeerRecoveryGeneration,
    peerRecoveryGenerationWord64,
  )
where

import Eclips.Herald.PeerLiveness.Internal
  ( PeerRecoveryConfiguration,
    PeerRecoveryConfigurationError (..),
    PeerRecoveryGeneration,
    checkPeerRecoveryConfiguration,
    peerRecoveryGenerationWord64,
    peerRecoveryGraceMicroseconds,
  )
