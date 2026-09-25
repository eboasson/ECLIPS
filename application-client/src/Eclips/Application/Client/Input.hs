-- | Commands and transport observations accepted by the pure client owner.
module Eclips.Application.Client.Input
  ( ApplicationInvocationId,
    applicationInvocationId,
    applicationInvocationIdWord64,
    ApplicationUnavailableReason (..),
    ApplicationClientInput (..),
  )
where

import Data.Word (Word64)
import Eclips.Application.Client.Recovery
  ( ApplicationClientMonotonicInstant,
    ApplicationClientRecoveryDeadline,
    ApplicationSessionRecoveryGeneration,
    ApplicationTransportAttemptGeneration,
    ApplicationTransportRetryScope,
  )
import Eclips.Application.Types.Lifecycle (LifecycleCommand)
import Eclips.Application.Types.Operation (ApplicationOperation)
import Eclips.Protocol.Application.Types (ApplicationServerDto)

-- | Caller-selected correlation, local to one client owner.
--
-- Zero is valid. Fresh values must increase strictly for the lifetime of an
-- owner, across ordinary and lifecycle calls. Retired values cannot be reused.
newtype ApplicationInvocationId = ApplicationInvocationId Word64
  deriving stock (Eq, Ord, Show)

-- | Construct a caller correlation. This does not allocate a protocol request.
applicationInvocationId :: Word64 -> ApplicationInvocationId
applicationInvocationId = ApplicationInvocationId

-- | Observe the caller-selected representation.
applicationInvocationIdWord64 :: ApplicationInvocationId -> Word64
applicationInvocationIdWord64 (ApplicationInvocationId value) = value

-- | An authoritative reason why an unresolved accepted outcome cannot be
-- recovered. Loss of the established application connection ends its lifetime.
data ApplicationUnavailableReason
  = HeraldPermanentlyLost
  | HeraldIsolated
  | HeraldRetired
  | SessionNoLongerLive
  | LifecycleResultNotRetained
  deriving stock (Eq, Show)

-- | The complete input vocabulary of the pure client owner.
--
-- Physical observations echo the owner-issued attempt, retry, and recovery
-- correlations carried by effects. Their monotonic instant is when the runtime
-- observed the event, not when the owner eventually dequeues it. This lets a
-- delayed timer delivery and a late Resume be ordered against one absolute
-- deadline without reading a clock in the transition.
data ApplicationClientInput
  = Start
  | TransportAvailable
      ApplicationTransportAttemptGeneration
      ApplicationClientMonotonicInstant
  | TransportAttemptFailed
      ApplicationTransportAttemptGeneration
      ApplicationClientMonotonicInstant
  | TransportLost
      ApplicationTransportAttemptGeneration
      ApplicationClientMonotonicInstant
  | TransportRetryElapsed
      ApplicationTransportRetryScope
      ApplicationTransportAttemptGeneration
      ApplicationClientMonotonicInstant
  | RecoveryDeadlineElapsed
      ApplicationSessionRecoveryGeneration
      ApplicationClientRecoveryDeadline
      ApplicationClientMonotonicInstant
  | ServerDtoReceived
      ApplicationTransportAttemptGeneration
      ApplicationClientMonotonicInstant
      ApplicationServerDto
  | Submit ApplicationInvocationId ApplicationOperation
  | SubmitLifecycle ApplicationInvocationId LifecycleCommand
  | Cancel ApplicationInvocationId
  | ReceiptRetirementFlush ApplicationTransportAttemptGeneration
  | End
  | HeraldPermanentlyUnavailable ApplicationUnavailableReason
  deriving stock (Eq, Show)
