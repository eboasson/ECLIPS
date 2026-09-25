{-# LANGUAGE NoFieldSelectors #-}

-- | Private runtime representation for the scoped application owner.
module Eclips.Application.Runtime.Internal.Types
  ( Application (..),
    ApplicationCall (..),
    ApplicationCallCompletion (..),
    ApplicationLifecycleCall (..),
    ApplicationLifecycleCompletion (..),
    applicationConfigurationEndpoint,
    applicationConfigurationAttachment,
    applicationConfigurationLiveness,
    ApplicationConfiguration (..),
    ApplicationEndpoint (..),
    ApplicationEndpointError (..),
    ApplicationLivenessConfiguration (..),
    ApplicationLivenessConfigurationError (..),
    ApplicationRuntimeFailure (..),
    ApplicationPhysicalPhase (..),
    ClientWrite (..),
    ClientConnection (..),
    EffectEnvelope (..),
    RuntimeTimerKey (..),
    RuntimeContext (..),
  )
where

import Control.Concurrent (MVar, ThreadId)
import Control.Concurrent.STM
  ( TMVar,
    TQueue,
    TVar,
  )
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word16, Word64)
import Eclips.Application.Client
  ( ApplicationClientEffect,
    ApplicationClientInput,
    ApplicationClientRecoveryConfiguration,
    ApplicationClientRecoveryDeadline,
    ApplicationInvocationId,
    ApplicationSessionRecoveryGeneration,
    ApplicationTransportAttemptGeneration,
    ApplicationTransportRetryScope,
    ApplicationUnavailableReason,
  )
import Eclips.Application.Runtime.Internal.Physical
  ( ApplicationHeartbeatActivity,
    ApplicationPhysicalPhase (..),
  )
import Eclips.Application.Types.Access (ApplicationStartupAccess)
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Application.Types.Rejection (ApplicationRejection)
import Eclips.Application.Types.Result (RegularCallResult)
import Eclips.Protocol.Application.Types
  ( ApplicationAttachmentClaim,
    ApplicationClientDto,
    ApplicationClientNonce,
    ApplicationHeartbeatNonce,
    ApplicationIsolationOverlayDto,
    ApplicationSessionErrorDto,
  )
import Eclips.Protocol.Application.Types qualified as Protocol
import Network.Socket (Socket)

-- | Checked numeric endpoint for one Herald application listener.
data ApplicationEndpoint = ApplicationEndpoint Text Word16
  deriving stock (Eq, Show)

-- | Configuration-shape errors caught before a scope starts.
data ApplicationEndpointError
  = ApplicationEndpointHostEmpty
  | ApplicationEndpointPortZero
  deriving stock (Eq, Show)

-- | Malformed application-side liveness timing configuration.
data ApplicationLivenessConfigurationError
  = ApplicationRetryDelayMustBePositive
  | ApplicationRecoveryGraceMustBePositive
  | ApplicationHeartbeatIdleMustBePositive
  | ApplicationHeartbeatReplyTimeoutMustBePositive
  deriving stock (Eq, Show)

-- | Complete checked timing policy for reconnect, recovery, and heartbeat.
-- Every duration is interpreted by the runtime's monotonic microsecond clock.
data ApplicationLivenessConfiguration
  = ApplicationLivenessConfiguration
      ApplicationClientRecoveryConfiguration
      Word64
      Word64
  deriving stock (Eq, Show)

-- | Complete immutable startup configuration.
data ApplicationConfiguration
  = ApplicationConfiguration
      ApplicationEndpoint
      ApplicationAttachmentClaim
      ApplicationClientNonce
      ApplicationLivenessConfiguration
  | PreparedApplicationConfiguration
      Lifecycle.ConnectionDescriptor
      Lifecycle.InitialClaimId
      ApplicationLivenessConfiguration

applicationConfigurationEndpoint :: ApplicationConfiguration -> ApplicationEndpoint
applicationConfigurationEndpoint = \case
  ApplicationConfiguration endpoint _ _ _ -> endpoint
  PreparedApplicationConfiguration descriptor _ _ ->
    let locator = Lifecycle.connectionDescriptorLocator descriptor
     in ApplicationEndpoint (Lifecycle.heraldLocatorHost locator) (Lifecycle.heraldLocatorPort locator)

applicationConfigurationAttachment :: ApplicationConfiguration -> ApplicationAttachmentClaim
applicationConfigurationAttachment = \case
  ApplicationConfiguration _ attachment _ _ -> attachment
  PreparedApplicationConfiguration descriptor _ _ ->
    either (error . show) id (Protocol.applicationAttachmentClaim (Lifecycle.connectionDescriptorAttachmentBytes descriptor))

applicationConfigurationLiveness :: ApplicationConfiguration -> ApplicationLivenessConfiguration
applicationConfigurationLiveness = \case
  ApplicationConfiguration _ _ _ liveness -> liveness
  PreparedApplicationConfiguration _ _ liveness -> liveness

-- | Redacted terminal failure of the application-side runtime.
data ApplicationRuntimeFailure
  = ApplicationSessionOpenRejected ApplicationSessionErrorDto
  | ApplicationInitialClaimRejected Lifecycle.StartupError
  | ApplicationSessionUnavailableBeforeStartup ApplicationUnavailableReason
  | ApplicationClientInvariantFailure
  | ApplicationTransportOwnerFailure
  deriving stock (Eq, Show)

-- | The terminal result retained by one public call.
data ApplicationCallCompletion
  = ApplicationCallSucceeded RegularCallResult
  | ApplicationCallRejected ApplicationRejection
  | ApplicationCallCancelled
  | ApplicationCallUnknown ApplicationUnavailableReason
  | ApplicationCallClosed
  deriving stock (Eq, Show)

data ApplicationLifecycleCompletion
  = ApplicationLifecycleSucceeded Lifecycle.LifecycleResult
  | ApplicationLifecycleRejected Lifecycle.LifecycleError
  | ApplicationLifecycleUnknown ApplicationUnavailableReason
  | ApplicationLifecycleClosed
  deriving stock (Eq, Show)

-- | One complete frame for the sole writer. Heartbeat frames carry a private
-- acknowledgement which becomes ready only after the progress-accounted
-- physical send completes.
data ClientWrite
  = ClientWrite ApplicationClientDto (Maybe (TMVar ()))

-- | One physical current-attempt client connection. Its sole writer queue
-- serializes semantic DTOs and heartbeat frames into one byte stream.
data ClientConnection
  = ClientConnection
      ApplicationTransportAttemptGeneration
      Socket
      (TQueue (Maybe ClientWrite))
      (TVar Bool)
      (TVar ApplicationPhysicalPhase)
      (TVar ApplicationHeartbeatActivity)
      (TVar (Maybe ApplicationHeartbeatNonce))
      (TVar Word64)

-- | One complete pure-client effect batch with its exact captured transport.
data EffectEnvelope
  = EffectEnvelope
      (Maybe ClientConnection)
      [ApplicationClientEffect]

-- | One pure-owner timer correlation currently interpreted by the runtime.
data RuntimeTimerKey
  = RuntimeReceiptRetirementTimer ApplicationTransportAttemptGeneration
  | RuntimeTransportRetryTimer
      ApplicationTransportRetryScope
      ApplicationTransportAttemptGeneration
  | RuntimeRecoveryDeadlineTimer
      ApplicationSessionRecoveryGeneration
      ApplicationClientRecoveryDeadline
  deriving stock (Eq, Ord, Show)

-- | Shared physical coordination. The pure client state never enters this
-- structure and remains a local parameter of its one owner loop.
data RuntimeContext = RuntimeContext
  { runtimeInput :: TQueue ApplicationClientInput,
    runtimeEffects :: TQueue EffectEnvelope,
    runtimeStartup :: TMVar (Either ApplicationRuntimeFailure ApplicationStartupAccess),
    runtimeIsolation :: TMVar ApplicationIsolationOverlayDto,
    runtimeUnavailable :: TMVar ApplicationUnavailableReason,
    runtimeFailure :: TMVar ApplicationRuntimeFailure,
    runtimeClosed :: TMVar (),
    runtimeStopping :: TVar Bool,
    runtimeObservationGate :: MVar (),
    runtimeRequestedAttempt :: TVar (Maybe ApplicationTransportAttemptGeneration),
    runtimeCurrent :: TVar (Maybe ClientConnection),
    runtimeTimers :: TVar (Map RuntimeTimerKey (TVar Bool)),
    runtimeCalls :: TVar (Map ApplicationInvocationId (TMVar ApplicationCallCompletion)),
    runtimeLifecycleCalls :: TVar (Map ApplicationInvocationId (TMVar ApplicationLifecycleCompletion, TVar (Maybe Lifecycle.LifecycleStatus), TMVar (Maybe Lifecycle.LifecycleRequestId))),
    runtimeNextInvocation :: TVar Word64,
    runtimeThreads :: TVar (Map ThreadId (MVar ()))
  }

-- | Opaque handle passed only after the pure owner reports session readiness.
newtype Application = Application RuntimeContext

-- | Opaque repeatable result handle for one submitted invocation.
data ApplicationCall
  = ApplicationCall
      ApplicationInvocationId
      (TMVar ApplicationCallCompletion)
      (TQueue ApplicationClientInput)

-- | One retained lifecycle request and its repeatable terminal/progress views.
data ApplicationLifecycleCall
  = ApplicationLifecycleCall
      (Maybe Lifecycle.LifecycleRequestId)
      (TMVar ApplicationLifecycleCompletion)
      (TVar (Maybe Lifecycle.LifecycleStatus))
