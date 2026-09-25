-- | Scoped callable application facade over one numeric Herald endpoint.
module Eclips.Application.Runtime
  ( Application,
    ApplicationConfiguration,
    ApplicationEndpoint,
    ApplicationEndpointError (..),
    applicationEndpoint,
    applicationEndpointHost,
    applicationEndpointPort,
    ApplicationLivenessConfiguration,
    ApplicationLivenessConfigurationError (..),
    applicationLivenessConfigurationMicroseconds,
    applicationLivenessFromTakeoverTarget,
    applicationRetryDelayMicroseconds,
    applicationRecoveryGraceMicroseconds,
    applicationHeartbeatIdleMicroseconds,
    applicationHeartbeatReplyTimeoutMicroseconds,
    ApplicationRuntimeFailure (..),
    ApplicationUnavailableReason (..),
    ApplicationCall,
    ApplicationCallCompletion (..),
    ApplicationLifecycleCall,
    ApplicationLifecycleCompletion (..),
    preparedApplicationConfiguration,
    beginChild,
    awaitPreparedChild,
    awaitChildReady,
    cancelChild,
    endProcess,
    awaitApplicationLifecycleCall,
    applicationLifecycleRequestId,
    applicationLifecycleStatus,
    recoverEndProcess,
    ApplicationIsolationOverlayDto (..),
    applicationConfiguration,
    withApplication,
    newid,
    write,
    forward,
    read,
    localTake,
    wait,
    label,
    newenv,
    awaitApplicationCall,
    cancelApplicationCall,
    awaitApplicationIsolation,
    awaitApplicationUnavailable,
  )
where

import Control.Concurrent.STM
  ( atomically,
    isEmptyTMVar,
    readTMVar,
    readTVar,
    writeTQueue,
  )
import Control.Exception (mask, onException)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word16, Word64)
import Eclips.Application.Client
  ( ApplicationClientInput (..),
    ApplicationClientRecoveryConfigurationError (..),
    ApplicationUnavailableReason (..),
    applicationClientRecoveryConfiguration,
    applicationClientRecoveryGraceMicroseconds,
    applicationClientRetryDelayMicroseconds,
    submitForward,
    submitLabel,
    submitLocalTake,
    submitNewEnvironment,
    submitNewId,
    submitRead,
    submitWait,
    submitWrite,
  )
import Eclips.Application.Runtime.Internal.Owner
  ( awaitApplicationIsolationRuntime,
    awaitApplicationUnavailableRuntime,
    recoverEndProcessRuntime,
    submitApplicationCall,
    submitApplicationLifecycleCall,
    withApplicationRuntime,
  )
import Eclips.Application.Runtime.Internal.Types
  ( Application,
    ApplicationCall (..),
    ApplicationCallCompletion (..),
    ApplicationConfiguration (..),
    ApplicationEndpoint (..),
    ApplicationEndpointError (..),
    ApplicationLifecycleCall (..),
    ApplicationLifecycleCompletion (..),
    ApplicationLivenessConfiguration (..),
    ApplicationLivenessConfigurationError (..),
    ApplicationRuntimeFailure (..),
  )
import Eclips.Application.Types.Access (ApplicationStartupAccess, PrimordialSelection)
import Eclips.Application.Types.Identity
  ( PrivateNablaId,
    PrivateObjectId,
  )
import Eclips.Application.Types.Label (ApplicationLabelTarget)
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Application.Types.NewId (NewIdTarget)
import Eclips.Application.Types.Query (ApplicationQuery)
import Eclips.Application.Types.Value (ApplicationLabel)
import Eclips.Application.Types.Write (ApplicationWriteValue)
import Eclips.Protocol.Application.Types
  ( ApplicationAttachmentClaim,
    ApplicationClientNonce,
    ApplicationIsolationOverlayDto (..),
  )
import Eclips.Public.Types.Timing qualified as Timing
import Prelude hiding (read)

-- | Check the stable endpoint shape. Numeric-host admission is completed by the
-- socket resolver with @AI_NUMERICHOST@ when the scope connects.
applicationEndpoint :: Text -> Word16 -> Either ApplicationEndpointError ApplicationEndpoint
applicationEndpoint host port
  | Text.null host = Left ApplicationEndpointHostEmpty
  | port == 0 = Left ApplicationEndpointPortZero
  | otherwise = Right (ApplicationEndpoint host port)

-- | Observe the configured numeric host text.
applicationEndpointHost :: ApplicationEndpoint -> Text
applicationEndpointHost (ApplicationEndpoint host _) = host

-- | Observe the configured nonzero TCP port.
applicationEndpointPort :: ApplicationEndpoint -> Word16
applicationEndpointPort (ApplicationEndpoint _ port) = port

-- | Check all physical application-lane timing durations.
--
-- The arguments are, in order, transport retry delay, established-session
-- recovery grace, quiet time before a heartbeat, and transport reply timeout.
-- The reply interval covers initial establishment, heartbeat replies and the
-- final session-close write; it does not time out semantic operations.
-- All four use the runtime's monotonic microsecond clock and must be positive.
applicationLivenessConfigurationMicroseconds ::
  Word64 ->
  Word64 ->
  Word64 ->
  Word64 ->
  Either ApplicationLivenessConfigurationError ApplicationLivenessConfiguration
applicationLivenessConfigurationMicroseconds retryDelay recoveryGrace heartbeatIdle heartbeatReply = do
  recovery <-
    case applicationClientRecoveryConfiguration retryDelay recoveryGrace of
      Left ApplicationClientRetryDelayMustBePositive ->
        Left ApplicationRetryDelayMustBePositive
      Left ApplicationClientRecoveryGraceMustBePositive ->
        Left ApplicationRecoveryGraceMustBePositive
      Right checkedRecovery -> Right checkedRecovery
  if heartbeatIdle == 0
    then Left ApplicationHeartbeatIdleMustBePositive
    else
      if heartbeatReply == 0
        then Left ApplicationHeartbeatReplyTimeoutMustBePositive
        else Right (ApplicationLivenessConfiguration recovery heartbeatIdle heartbeatReply)

-- | Derive application-side timing from the target carried in startup material,
-- with the independent default application transport response allowance.
-- Admission of 'Timing.TakeoverTarget' proves that every derived duration is
-- positive, so the ordinary liveness constructor cannot reject this policy.
applicationLivenessFromTakeoverTarget :: Timing.TakeoverTarget -> ApplicationLivenessConfiguration
applicationLivenessFromTakeoverTarget target =
  either
    (error . show)
    id
    ( applicationLivenessConfigurationMicroseconds
        (Timing.timingReconnectInitialMicroseconds policy)
        (Timing.timingApplicationRecoveryGraceMicroseconds policy)
        (Timing.timingHeartbeatIdleMicroseconds policy)
        Timing.defaultApplicationReplyTimeoutMicroseconds
    )
  where
    policy = Timing.deriveTimingPolicy target

-- | Observe the positive delay between physical transport attempts.
applicationRetryDelayMicroseconds :: ApplicationLivenessConfiguration -> Word64
applicationRetryDelayMicroseconds (ApplicationLivenessConfiguration recovery _ _) =
  applicationClientRetryDelayMicroseconds recovery

-- | Observe the absolute recovery grace for an established session.
applicationRecoveryGraceMicroseconds :: ApplicationLivenessConfiguration -> Word64
applicationRecoveryGraceMicroseconds (ApplicationLivenessConfiguration recovery _ _) =
  applicationClientRecoveryGraceMicroseconds recovery

-- | Observe the quiet interval before one client heartbeat is sent.
applicationHeartbeatIdleMicroseconds :: ApplicationLivenessConfiguration -> Word64
applicationHeartbeatIdleMicroseconds (ApplicationLivenessConfiguration _ idle _) = idle

-- | Observe the transport reply interval used during establishment, heartbeat
-- progress and the final session-close write.
applicationHeartbeatReplyTimeoutMicroseconds :: ApplicationLivenessConfiguration -> Word64
applicationHeartbeatReplyTimeoutMicroseconds (ApplicationLivenessConfiguration _ _ reply) = reply

-- | Construct one immutable application-side runtime configuration.
applicationConfiguration ::
  ApplicationEndpoint ->
  ApplicationAttachmentClaim ->
  ApplicationClientNonce ->
  ApplicationLivenessConfiguration ->
  ApplicationConfiguration
applicationConfiguration = ApplicationConfiguration

-- | Open one connection-scoped session, run the callback after startup is known,
-- then end and join the entire client transport scope.
withApplication ::
  ApplicationConfiguration ->
  (Application -> ApplicationStartupAccess -> IO result) ->
  IO (Either ApplicationRuntimeFailure result)
withApplication = withApplicationRuntime

-- | Submit one bare or controlled generated-identity operation.
newid ::
  Application ->
  NewIdTarget ->
  IO ApplicationCall
newid application target =
  submitApplicationCall application $ \invocation ->
    submitNewId invocation target

-- | Submit one of the current executable write payloads.
write ::
  Application ->
  PrivateNablaId ->
  ApplicationWriteValue ->
  IO ApplicationCall
write application writer value =
  submitApplicationCall application $ \invocation ->
    submitWrite invocation writer value

-- | Submit an ordinary or structural forward. The returned call remains
-- unresolved while structural stabilization is pending and exposes only the
-- operation-specific terminal result.
forward ::
  Application ->
  PrivateNablaId ->
  PrivateObjectId ->
  IO ApplicationCall
forward application writer object =
  submitApplicationCall application $ \invocation ->
    submitForward invocation writer object

-- | Submit the current read operation.
read :: Application -> ApplicationQuery -> IO ApplicationCall
read application query =
  submitApplicationCall application $ \invocation ->
    submitRead invocation query

-- | Submit the current local destructive take operation.
localTake :: Application -> ApplicationQuery -> IO ApplicationCall
localTake application query =
  submitApplicationCall application $ \invocation ->
    submitLocalTake invocation query

-- | Submit the current level-triggered wait operation.
wait :: Application -> [ApplicationQuery] -> IO ApplicationCall
wait application queries =
  submitApplicationCall application $ \invocation ->
    submitWait invocation queries

-- | Compare one private object's effective label and, when equal, submit the
-- requested label, void, or delete decision.  The comparison is value-based:
-- it carries no hidden revision, and a same-label success still performs the
-- complete visibility fence.
label ::
  Application ->
  PrivateObjectId ->
  ApplicationLabel ->
  ApplicationLabelTarget ->
  IO ApplicationCall
label application object expected target =
  submitApplicationCall application $ \invocation ->
    submitLabel invocation object expected target

-- | Create one complete private environment. The call remains unresolved while
-- the endpoint and wiring phases establish the complete 31-object topology.
newenv :: Application -> IO ApplicationCall
newenv application =
  submitApplicationCall application submitNewEnvironment

-- | Await the retained terminal result. A cancelled waiter submits the existing
-- cancellation command and exits promptly. Only Wait has semantic cancellation;
-- other accepted work continues under its original identity in the owner.
awaitApplicationCall :: ApplicationCall -> IO ApplicationCallCompletion
awaitApplicationCall call@(ApplicationCall _ completion _) =
  mask $ \restore ->
    restore (atomically (readTMVar completion))
      `onException` cancelApplicationCall call

-- | Request cancellation through the pure owner. Only a live wait has a remote
-- cancellation operation; other calls retain their ordinary terminal result.
cancelApplicationCall :: ApplicationCall -> IO ()
cancelApplicationCall (ApplicationCall invocation completion inputs) =
  atomically $ do
    pending <- isEmptyTMVar completion
    when pending (writeTQueue inputs (Cancel invocation))

-- | Wait until the connected Herald irreversibly enters its bounded local
-- read-only isolation drain. The returned closed overlay describes how local
-- reads are presented during that drain.
awaitApplicationIsolation :: Application -> IO ApplicationIsolationOverlayDto
awaitApplicationIsolation = awaitApplicationIsolationRuntime

-- | Wait for the Herald's authoritative terminal disposition. This is
-- independent of any in-flight call, so an application can observe a clean
-- isolation/retirement close even when every local read already completed.
awaitApplicationUnavailable :: Application -> IO ApplicationUnavailableReason
awaitApplicationUnavailable = awaitApplicationUnavailableRuntime

-- | Connect through one prepared descriptor with an injected stable claim ID.
-- The same configuration must be retained across initial transport retries.
preparedApplicationConfiguration :: Lifecycle.ConnectionDescriptor -> Lifecycle.InitialClaimId -> ApplicationLivenessConfiguration -> ApplicationConfiguration
preparedApplicationConfiguration = PreparedApplicationConfiguration

beginChild :: Application -> Lifecycle.HeraldLocator -> PrimordialSelection -> IO ApplicationLifecycleCall
beginChild application target selection = submitApplicationLifecycleCall application (Lifecycle.BeginChild target selection)

awaitPreparedChild :: Application -> Lifecycle.ChildPreparation -> IO ApplicationLifecycleCall
awaitPreparedChild application preparation = submitApplicationLifecycleCall application (Lifecycle.AwaitPreparedChild preparation)

awaitChildReady :: Application -> Lifecycle.ChildPreparation -> IO ApplicationLifecycleCall
awaitChildReady application preparation = submitApplicationLifecycleCall application (Lifecycle.AwaitChildReady preparation)

cancelChild :: Application -> Lifecycle.ChildPreparation -> IO ApplicationLifecycleCall
cancelChild application preparation = submitApplicationLifecycleCall application (Lifecycle.CancelChild preparation)

endProcess :: Application -> IO ApplicationLifecycleCall
endProcess application = submitApplicationLifecycleCall application Lifecycle.EndOwnProcess

-- | Awaiting cancellation does not cancel accepted lifecycle work or the child.
awaitApplicationLifecycleCall :: ApplicationLifecycleCall -> IO ApplicationLifecycleCompletion
awaitApplicationLifecycleCall (ApplicationLifecycleCall _ completion _) = atomically (readTMVar completion)

-- | The full session-qualified correlation survives process and session End.
-- No correlation exists if the local owner rejected the submission as closed.
applicationLifecycleRequestId :: ApplicationLifecycleCall -> Maybe Lifecycle.LifecycleRequestId
applicationLifecycleRequestId (ApplicationLifecycleCall request _ _) = request

applicationLifecycleStatus :: ApplicationLifecycleCall -> IO (Maybe Lifecycle.LifecycleStatus)
applicationLifecycleStatus (ApplicationLifecycleCall _ _ progress) = atomically (readTVar progress)

-- | Recover only the retained own-End result through the attachment reference.
-- This performs no session Open/Resume or new semantic End request.
recoverEndProcess :: ApplicationConfiguration -> Lifecycle.LifecycleRequestId -> IO (Either ApplicationRuntimeFailure ApplicationLifecycleCompletion)
recoverEndProcess = recoverEndProcessRuntime
