-- | Package-private construction of complete client effect batches.
module Eclips.Application.Client.Effect.Internal
  ( ApplicationCallOutcome (..),
    ApplicationClientEffect (..),
    ApplicationClientEffectBatch,
    applicationClientEffectBatch,
    applicationClientEffects,
  )
where

import Data.Word (Word64)
import Eclips.Application.Client.Input
  ( ApplicationInvocationId,
    ApplicationUnavailableReason,
  )
import Eclips.Application.Client.Recovery
  ( ApplicationClientMonotonicInstant,
    ApplicationClientRecoveryDeadline,
    ApplicationSessionRecoveryGeneration,
    ApplicationTransportAttemptGeneration,
    ApplicationTransportRetryScope,
  )
import Eclips.Application.Types.Access (ApplicationStartupAccess)
import Eclips.Application.Types.Lifecycle (LifecycleRequestId, LifecycleStatus, StartupError)
import Eclips.Application.Types.Rejection (ApplicationRejection)
import Eclips.Application.Types.Result (RegularCallResult)
import Eclips.Protocol.Application.Types
  ( ApplicationClientDto,
    ApplicationIsolationOverlayDto,
    ApplicationSessionErrorDto,
  )

-- | The semantic terminal result of one admitted application call.
data ApplicationCallOutcome
  = ApplicationCallSucceeded RegularCallResult
  | ApplicationCallRejected ApplicationRejection
  deriving stock (Eq, Show)

-- | One ordered instruction for the later runtime interpreter.
--
-- Retry instants and recovery deadlines are absolute monotonic microsecond
-- values. A runtime may deliver a canceled timer; it must return the exact
-- correlations so the pure owner can classify that observation as stale.
data ApplicationClientEffect
  = RequestTransport ApplicationTransportAttemptGeneration
  | ArmTransportRetry
      ApplicationTransportRetryScope
      ApplicationTransportAttemptGeneration
      ApplicationClientMonotonicInstant
  | CancelTransportRetry
      ApplicationTransportRetryScope
      ApplicationTransportAttemptGeneration
  | ArmRecoveryDeadline
      ApplicationSessionRecoveryGeneration
      ApplicationClientRecoveryDeadline
  | CancelRecoveryDeadline
      ApplicationSessionRecoveryGeneration
      ApplicationClientRecoveryDeadline
  | SessionEstablishedOnTransport ApplicationTransportAttemptGeneration
  | SendApplicationDto ApplicationClientDto
  | ScheduleReceiptRetirement ApplicationTransportAttemptGeneration Word64
  | SessionReady ApplicationStartupAccess
  | HeraldIsolationObserved ApplicationIsolationOverlayDto
  | HeraldPermanentlyUnavailableObserved ApplicationUnavailableReason
  | CallFinished ApplicationInvocationId ApplicationCallOutcome
  | CallCancelled ApplicationInvocationId
  | CallUnknown ApplicationInvocationId ApplicationUnavailableReason
  | CallClosed ApplicationInvocationId
  | SessionOpenFailed ApplicationSessionErrorDto
  | InitialClaimFailed StartupError
  | LifecycleAssigned ApplicationInvocationId LifecycleRequestId
  | LifecycleChanged ApplicationInvocationId LifecycleStatus
  | LifecycleUnknown ApplicationInvocationId ApplicationUnavailableReason
  | LifecycleClosed ApplicationInvocationId
  | ResetTransport
  | CloseTransport
  deriving stock (Eq, Show)

-- | Complete ordered effects of one successful client transition.
newtype ApplicationClientEffectBatch
  = ApplicationClientEffectBatch [ApplicationClientEffect]
  deriving stock (Eq, Show)

applicationClientEffectBatch :: [ApplicationClientEffect] -> ApplicationClientEffectBatch
applicationClientEffectBatch = ApplicationClientEffectBatch

-- | Observe effects in interpretation order.
applicationClientEffects :: ApplicationClientEffectBatch -> [ApplicationClientEffect]
applicationClientEffects (ApplicationClientEffectBatch effects) = effects
