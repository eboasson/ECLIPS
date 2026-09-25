-- | Checked configuration and stable identities for irreversible local
-- non-voting-Herald isolation.
--
-- Sockets, monotonic clock acquisition, voter-host binding observations, and
-- timer scheduling remain runtime-owned.  Constructors stay private so only
-- the pure isolation owner can allocate generations and drain identities.
module Eclips.Herald.Isolation
  ( IsolationConfiguration,
    IsolationConfigurationError (..),
    checkIsolationConfiguration,
    isolationGraceMicroseconds,
    isolationDrainMicroseconds,
    IsolationGraceGeneration,
    isolationGraceGenerationWord64,
    IsolationDrainId,
    isolationDrainIdWord64,
  )
where

import Eclips.Herald.Isolation.Internal
  ( IsolationConfiguration,
    IsolationConfigurationError (..),
    IsolationDrainId,
    IsolationGraceGeneration,
    checkIsolationConfiguration,
    isolationDrainIdWord64,
    isolationDrainMicroseconds,
    isolationGraceGenerationWord64,
    isolationGraceMicroseconds,
  )
