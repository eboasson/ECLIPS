-- | Checked configuration and stable identities for pure application recovery.
--
-- Sockets, heartbeat scheduling, and clock acquisition remain runtime-owned.
-- This module exposes only the checked startup value and opaque semantic
-- generations needed at that boundary.
module Eclips.Herald.Application.Recovery
  ( ApplicationRecoveryConfiguration,
    ApplicationRecoveryConfigurationError (..),
    checkApplicationRecoveryConfiguration,
    applicationRecoveryGraceMicroseconds,
    ApplicationSessionRecoveryGeneration,
    applicationSessionRecoveryGenerationWord64,
    ApplicationProcessRecoveryGeneration,
    applicationProcessRecoveryGenerationWord64,
  )
where

import Eclips.Herald.Application.Recovery.Internal
  ( ApplicationProcessRecoveryGeneration,
    ApplicationRecoveryConfiguration,
    ApplicationRecoveryConfigurationError (..),
    ApplicationSessionRecoveryGeneration,
    applicationProcessRecoveryGenerationWord64,
    applicationRecoveryGraceMicroseconds,
    applicationSessionRecoveryGenerationWord64,
    checkApplicationRecoveryConfiguration,
  )
