{-# LANGUAGE GADTs #-}

-- | Typed application calls and scoped ownership of the existing client runtime.
module Eclips.Application.Internal
  ( Herald,
    ConnectError (..),
    CallError (..),
    CallStatus (..),
    Operation (..),
    Call,
    LifecycleCallError (..),
    CancelResult (..),
    LifecycleOperation (..),
    LifecycleCall,
    connect,
    connectWith,
    disconnect,
    withHerald,
    startup,
    sameHerald,
    submit,
    await,
    cancel,
    status,
    submitLifecycle,
    awaitLifecycle,
    lifecycleStatus,
    lifecycleRequestId,
    recoverEndProcess,
  )
where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.STM
  ( TMVar,
    atomically,
    newEmptyTMVarIO,
    orElse,
    readTMVar,
    tryPutTMVar,
    tryReadTMVar,
    writeTQueue,
  )
import Control.Exception (IOException, SomeException, finally, mask, onException, try)
import Control.Monad (void)
import Data.ByteString qualified as Bytes
import Eclips.Application.Client qualified as Client
import Eclips.Application.Runtime qualified as Runtime
import Eclips.Application.Runtime.Internal.Types qualified as RuntimeInternal
import Eclips.Application.Types.Access (ApplicationStartupAccess, EnvironmentAccess, PrimordialSelection)
import Eclips.Application.Types.Forward (ForwardResult)
import Eclips.Application.Types.Identity (PrivateNablaId, PrivateObjectId, PrivateUniqueId)
import Eclips.Application.Types.Label (ApplicationLabelTarget, LabelResult)
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Application.Types.NewId (NewIdTarget)
import Eclips.Application.Types.Query (ApplicationQuery)
import Eclips.Application.Types.Rejection (ApplicationRejection)
import Eclips.Application.Types.Result (RegularCallResult (..), WaitResult)
import Eclips.Application.Types.Value (ApplicationLabel, ApplicationValue)
import Eclips.Application.Types.Write (ApplicationWriteValue, WriteResult)
import System.Entropy (getEntropy)

data ConnectError
  = ConnectStartupRejected Lifecycle.StartupError
  | ConnectUnavailable Client.ApplicationUnavailableReason
  | ConnectTransportFailed
  | ConnectProtocolFault
  | ConnectEntropyFailed
  deriving stock (Eq, Show)

data CallError
  = CallRejected ApplicationRejection
  | CallCancelled
  | CallUnavailable Client.ApplicationUnavailableReason
  | CallClosed
  | CallProtocolFault
  deriving stock (Eq, Show)

data CallStatus result
  = CallPending
  | CallComplete (Either CallError result)
  deriving stock (Eq, Show)

-- | Each constructor fixes its result type. This is the same eight-operation
-- vocabulary as the protocol, with no lifecycle or transport command mixed in.
data Operation result where
  NewId :: NewIdTarget -> Operation PrivateUniqueId
  Write :: PrivateNablaId -> ApplicationWriteValue -> Operation WriteResult
  Forward :: PrivateNablaId -> PrivateObjectId -> Operation ForwardResult
  Read :: ApplicationQuery -> Operation [ApplicationValue]
  LocalTake :: ApplicationQuery -> Operation [ApplicationValue]
  Wait :: [ApplicationQuery] -> Operation WaitResult
  Label :: PrivateObjectId -> ApplicationLabel -> ApplicationLabelTarget -> Operation LabelResult
  NewEnvironment :: Operation EnvironmentAccess

data Call result = Call (Operation result) Runtime.ApplicationCall

data LifecycleCallError
  = LifecycleRejected Lifecycle.LifecycleError
  | LifecycleUnavailable Client.ApplicationUnavailableReason
  | LifecycleClosed
  | LifecycleProtocolFault
  | LifecycleTransportFailed
  deriving stock (Eq, Show)

data CancelResult = Cancelled | AlreadyAttached
  deriving stock (Eq, Show)

data LifecycleOperation result where
  BeginChild :: Lifecycle.HeraldLocator -> PrimordialSelection -> LifecycleOperation Lifecycle.ChildPreparation
  AwaitPreparedChild :: Lifecycle.ChildPreparation -> LifecycleOperation Lifecycle.PreparedChild
  AwaitChildReady :: Lifecycle.ChildPreparation -> LifecycleOperation ()
  CancelChild :: Lifecycle.ChildPreparation -> LifecycleOperation CancelResult
  EndProcess :: LifecycleOperation ()

data LifecycleCall result = LifecycleCall (LifecycleOperation result) Runtime.ApplicationLifecycleCall

-- The supervisor owns the existing runtime scope. Neither the public handle
-- nor another thread can inspect or mutate the pure client state.
data Herald
  = Herald
      Runtime.Application
      ApplicationStartupAccess
      Runtime.ApplicationConfiguration
      (TMVar ())
      (TMVar (Either SomeException (Either Runtime.ApplicationRuntimeFailure ())))

startup :: Herald -> ApplicationStartupAccess
startup (Herald _ access _ _ _) = access

-- | Connection identity, independent of reusable private-name numbers.
sameHerald :: Herald -> Herald -> Bool
sameHerald (Herald _ _ _ left _) (Herald _ _ _ right _) = left == right

connect :: Lifecycle.ConnectionDescriptor -> IO (Either ConnectError Herald)
connect descriptor = do
  entropy <- try (getEntropy 32) :: IO (Either IOException Bytes.ByteString)
  case entropy of
    Left _ -> pure (Left ConnectEntropyFailed)
    Right bytes -> case Lifecycle.initialClaimId bytes of
      Right claim -> connectWith liveness claim descriptor
      Left _ -> pure (Left ConnectProtocolFault)
  where
    liveness = Runtime.applicationLivenessFromTakeoverTarget (Lifecycle.connectionDescriptorTakeoverTarget descriptor)

-- | Advanced entry point for an explicit liveness policy and retained claim ID.
connectWith :: Runtime.ApplicationLivenessConfiguration -> Lifecycle.InitialClaimId -> Lifecycle.ConnectionDescriptor -> IO (Either ConnectError Herald)
connectWith liveness claim descriptor = mask $ \restore -> do
  available <- newEmptyTMVarIO
  stopping <- newEmptyTMVarIO
  stopped <- newEmptyTMVarIO
  let configuration = Runtime.preparedApplicationConfiguration descriptor claim liveness
  supervisor <-
    forkFinally
      ( Runtime.withApplication configuration $ \application access -> do
          atomically (void (tryPutTMVar available (Herald application access configuration stopping stopped)))
          atomically (readTMVar stopping)
      )
      (atomically . void . tryPutTMVar stopped)
  let abort = killThread supervisor >> atomically (void (readTMVar stopped))
  restore
    ( atomically
        ( (Right <$> readTMVar available)
            `orElse` (Left . stoppedConnectError <$> readTMVar stopped)
        )
    )
    `onException` abort

stoppedConnectError :: Either SomeException (Either Runtime.ApplicationRuntimeFailure ()) -> ConnectError
stoppedConnectError = \case
  Left _ -> ConnectTransportFailed
  Right (Left problem) -> connectError problem
  Right (Right ()) -> ConnectTransportFailed

connectError :: Runtime.ApplicationRuntimeFailure -> ConnectError
connectError = \case
  Runtime.ApplicationInitialClaimRejected problem -> ConnectStartupRejected problem
  Runtime.ApplicationSessionUnavailableBeforeStartup reason -> ConnectUnavailable reason
  Runtime.ApplicationSessionOpenRejected _ -> ConnectProtocolFault
  Runtime.ApplicationClientInvariantFailure -> ConnectProtocolFault
  Runtime.ApplicationTransportOwnerFailure -> ConnectTransportFailed

-- | Idempotent session closure. Accepted semantic work remains Herald-owned.
disconnect :: Herald -> IO ()
disconnect (Herald _ _ _ stopping stopped) = mask $ \restore -> do
  atomically (void (tryPutTMVar stopping ()))
  restore (atomically (void (readTMVar stopped)))

withHerald :: Lifecycle.ConnectionDescriptor -> (Herald -> IO result) -> IO (Either ConnectError result)
withHerald descriptor use = mask $ \restore -> do
  -- Acquisition remains masked through cleanup registration. The connection's
  -- blocking startup wait is still interruptible and owns its abort/join path.
  connected <- connect descriptor
  case connected of
    Left problem -> pure (Left problem)
    Right herald -> (Right <$> restore (use herald)) `finally` disconnect herald

submit :: Herald -> Operation result -> IO (Call result)
submit (Herald application _ _ _ _) operation =
  Call operation <$> case operation of
    NewId target -> Runtime.newid application target
    Write writer value -> Runtime.write application writer value
    Forward writer object -> Runtime.forward application writer object
    Read query -> Runtime.read application query
    LocalTake query -> Runtime.localTake application query
    Wait queries -> Runtime.wait application queries
    Label object expected target -> Runtime.label application object expected target
    NewEnvironment -> Runtime.newenv application

-- | Await this exact invocation. Cancelling the waiting thread does not wait
-- for an accepted mutation's result or reissue it; a wait requests cancellation.
await :: Call result -> IO (Either CallError result)
await call@(Call operation (RuntimeInternal.ApplicationCall _ completion _)) =
  (decodeCompletion operation <$> atomically (readTMVar completion)) `onException` cancel call

cancel :: Call result -> IO ()
cancel (Call operation (RuntimeInternal.ApplicationCall invocation _ inputs)) = case operation of
  Wait _ -> atomically (writeTQueue inputs (Client.Cancel invocation))
  _ -> pure ()

status :: Call result -> IO (CallStatus result)
status (Call operation (RuntimeInternal.ApplicationCall _ completion _)) =
  maybe CallPending (CallComplete . decodeCompletion operation) <$> atomically (tryReadTMVar completion)

decodeCompletion :: Operation result -> Runtime.ApplicationCallCompletion -> Either CallError result
decodeCompletion operation = \case
  Runtime.ApplicationCallSucceeded result -> matchResult operation result
  Runtime.ApplicationCallRejected problem -> Left (CallRejected problem)
  Runtime.ApplicationCallCancelled -> Left CallCancelled
  Runtime.ApplicationCallUnknown reason -> Left (CallUnavailable reason)
  Runtime.ApplicationCallClosed -> Left CallClosed

-- One total typed projection, shared by every direct and advanced call. No
-- operation wrapper pattern-matches the heterogeneous result vocabulary.
matchResult :: Operation result -> RegularCallResult -> Either CallError result
matchResult operation result = case (operation, result) of
  (NewId _, NewIdCompleted value) -> Right value
  (Write _ _, WriteCompleted value) -> Right value
  (Forward _ _, ForwardCompleted value) -> Right value
  (Read _, ReadCompleted value) -> Right value
  (LocalTake _, LocalTakeCompleted value) -> Right value
  (Wait _, WaitCompleted value) -> Right value
  (Label _ _ _, LabelCompleted value) -> Right value
  (NewEnvironment, NewEnvironmentCompleted value) -> Right value
  _ -> Left CallProtocolFault

submitLifecycle :: Herald -> LifecycleOperation result -> IO (LifecycleCall result)
submitLifecycle (Herald application _ _ _ _) operation =
  LifecycleCall operation <$> case operation of
    BeginChild locator selection -> Runtime.beginChild application locator selection
    AwaitPreparedChild preparation -> Runtime.awaitPreparedChild application preparation
    AwaitChildReady preparation -> Runtime.awaitChildReady application preparation
    CancelChild preparation -> Runtime.cancelChild application preparation
    EndProcess -> Runtime.endProcess application

awaitLifecycle :: LifecycleCall result -> IO (Either LifecycleCallError result)
awaitLifecycle (LifecycleCall operation call) = decodeLifecycle operation <$> Runtime.awaitApplicationLifecycleCall call

lifecycleStatus :: LifecycleCall result -> IO (Maybe Lifecycle.LifecycleStatus)
lifecycleStatus (LifecycleCall _ call) = Runtime.applicationLifecycleStatus call

lifecycleRequestId :: LifecycleCall result -> Maybe Lifecycle.LifecycleRequestId
lifecycleRequestId (LifecycleCall _ call) = Runtime.applicationLifecycleRequestId call

decodeLifecycle :: LifecycleOperation result -> Runtime.ApplicationLifecycleCompletion -> Either LifecycleCallError result
decodeLifecycle operation = \case
  Runtime.ApplicationLifecycleSucceeded result -> case (operation, result) of
    (BeginChild _ _, Lifecycle.ChildPreparationAccepted reference) -> Right reference
    (AwaitPreparedChild expected, Lifecycle.ChildPrepared child) | Lifecycle.preparedChildPreparation child == expected -> Right child
    (AwaitChildReady _, Lifecycle.ChildReady) -> Right ()
    (CancelChild _, Lifecycle.ChildCancelled) -> Right Cancelled
    (CancelChild _, Lifecycle.ChildAlreadyAttached) -> Right AlreadyAttached
    (EndProcess, Lifecycle.ProcessEnded) -> Right ()
    _ -> Left LifecycleProtocolFault
  Runtime.ApplicationLifecycleRejected problem -> Left (LifecycleRejected problem)
  Runtime.ApplicationLifecycleUnknown reason -> Left (LifecycleUnavailable reason)
  Runtime.ApplicationLifecycleClosed -> Left LifecycleClosed

-- | Query a retained End through the original handle's connection material.
-- This remains usable after End/disconnect and cannot open a new session.
recoverEndProcess :: Herald -> Lifecycle.LifecycleRequestId -> IO (Either LifecycleCallError ())
recoverEndProcess (Herald _ _ configuration _ _) request = do
  recovered <- Runtime.recoverEndProcess configuration request
  pure $ case recovered of
    Right completion -> decodeLifecycle EndProcess completion
    Left Runtime.ApplicationClientInvariantFailure -> Left LifecycleProtocolFault
    Left _ -> Left LifecycleTransportFailed
