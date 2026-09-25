-- | Checked timing policy and nominal correlations for pure client recovery.
module Eclips.Application.Client.Recovery
  ( ApplicationClientRecoveryConfigurationError (..),
    ApplicationClientRecoveryConfiguration,
    applicationClientRecoveryConfiguration,
    applicationClientRetryDelayMicroseconds,
    applicationClientRecoveryGraceMicroseconds,
    ApplicationClientMonotonicInstant,
    applicationClientMonotonicInstant,
    applicationClientMonotonicInstantMicroseconds,
    ApplicationSessionRecoveryGeneration,
    applicationSessionRecoveryGenerationWord64,
    ApplicationTransportAttemptGeneration,
    applicationTransportAttemptGenerationWord64,
    ApplicationClientRecoveryDeadline,
    applicationClientRecoveryDeadlineMicroseconds,
    ApplicationTransportRetryScope (..),
  )
where

import Eclips.Application.Client.Recovery.Internal
