{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module RuntimeProperties
  ( tests,
  )
where

import Control.Concurrent
  ( MVar,
    forkFinally,
    forkIOWithUnmask,
    killThread,
    newEmptyMVar,
    putMVar,
    readMVar,
    threadDelay,
    tryReadMVar,
  )
import Control.Exception (AsyncException (ThreadKilled), IOException, SomeException, bracket, finally, fromException, throwIO, try)
import Control.Monad (replicateM_, void)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word64)
import Eclips.Application qualified as Typed
import Eclips.Application.Advanced qualified as Advanced
import Eclips.Application.Runtime
import Eclips.Application.Runtime.Internal.Physical
  ( ApplicationEstablishmentStatus (..),
    ApplicationHeartbeatWriteStatus (..),
    ApplicationPhysicalPhase (..),
    applicationEstablishmentStatus,
    applicationHandshakeTimeoutEligible,
    applicationHeartbeatBidirectionalProgress,
    applicationHeartbeatReceived,
    applicationHeartbeatWriteStatus,
    applicationHeartbeatWritten,
    applicationPhysicalEstablished,
    applicationServerDispositionQueued,
    initialApplicationHeartbeatActivity,
  )
import Eclips.Application.Typed (ApplicationSort (SortKindOf))
import Eclips.Application.Typed qualified as Payload
import Eclips.Application.Typed.Advanced qualified as PayloadAdvanced
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (SortDefinitionRole),
    ApplicationStartupAccess,
    EnvironmentAccess,
    allApplicationPredefinedSortRoles,
    applicationStartupAccess,
    environmentAccess,
    predefinedAccess,
    primordialAccessFromSelection,
    selectEnvironment,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Forward (ForwardResult (ForwardAccepted))
import Eclips.Application.Types.Identity
  ( PrivateNablaId,
    PrivateObjectId,
    PrivateUniqueId,
    SortId,
    asPrivateDeltaId,
    asPrivateNablaId,
    asPrivateObjectId,
    asPrivateProcessId,
    mkPrivateUniqueId,
    mkSortId,
    sortIdBytes,
  )
import Eclips.Application.Types.Label
  ( ApplicationLabelTarget (LabelToDelete, LabelToVoid),
    LabelResult (LabelApplied),
  )
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Application.Types.NewId (NewIdTarget (..))
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Query
  ( ApplicationQuery (ApplicationQuery),
    ApplicationQueryPredicate (QueryAlways),
  )
import Eclips.Application.Types.Query qualified as Query
import Eclips.Application.Types.Rejection (ApplicationRejection (ApplicationOperateNotPermitted))
import Eclips.Application.Types.Result
  ( OperationPendingReason (..),
    RegularCallResult (..),
    WaitResult (..),
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationSortDefinition (PredefinedSortDefinition),
  )
import Eclips.Application.Types.SortDescriptor qualified as Descriptor
import Eclips.Application.Types.Value
  ( ApplicationLabel,
    ApplicationLabelOwner (..),
    ApplicationValue (..),
  )
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (..),
    WriteResult (..),
  )
import Eclips.Protocol.Application.Frame
  ( ApplicationFrameContinuation (..),
    ApplicationFrameDecoder,
    ApplicationFrameDirection (..),
    ApplicationFrameFeedResult (..),
    encodeApplicationFrame,
    feedApplicationFrame,
    initialApplicationFrameDecoder,
  )
import Eclips.Protocol.Application.Types
  ( ApplicationAttachmentClaim,
    ApplicationClientDto (..),
    ApplicationClientNonce,
    ApplicationEnvelope (..),
    ApplicationReplyCursorClaim,
    ApplicationRequestIdClaim,
    ApplicationRequestReplyBodyDto (Cancelled, Completed, OperationAccepted, Rejected, WaitAccepted),
    ApplicationResumeTokenClaim,
    ApplicationServerDto (..),
    ApplicationSessionClaim,
    ApplicationSessionUnavailableReasonDto (HeraldIsolatedDto),
    applicationAttachmentClaim,
    applicationClientNonce,
    applicationReplyCursorClaim,
    applicationRequestIdClaim,
    applicationResumeTokenClaim,
    applicationSessionClaim,
    applicationWaitClaim,
  )
import Eclips.Public.Types.SortCatalogue qualified as Catalogue
import Eclips.Public.Types.Timing qualified as Timing
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Network.Socket
  ( Family (..),
    SockAddr (..),
    Socket,
    SocketOption (..),
    SocketType (..),
    accept,
    bind,
    close,
    defaultProtocol,
    getSocketName,
    listen,
    setSocketOption,
    socket,
    tupleToHostAddress,
  )
import Network.Socket.ByteString qualified as SocketBytes
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck qualified as QC
import Prelude hiding (read)

tests :: TestTree
tests =
  testGroup
    "scoped application runtime"
    [ testCase "typed environments expose verified wiring with ordinary operations" caseEnvironmentWiring,
      testCase "typed environments reject missing or mismatched object sort evidence" caseEnvironmentWiringEvidence,
      testCase "application payloads bind only the complete verified sort" casePayloadBinding,
      testCase "application payload reads writes and takes preserve typed results" casePayloadCalls,
      testCase "application payload wait cancellation retains the same invocation" casePayloadWaitCancellation,
      testCase "controlled payload references preserve reservation identity and operation targets" caseControlledPayloadCalls,
      testCase "controlled payload forwarding rejects objects from a different connection" caseControlledPayloadSessions,
      testCase "application payload mutation cancellation keeps the original completion cell" casePayloadMutationCancellation,
      testCase "staged topology creation retains rejected reservations and exact successful calls" caseTopologyCreationCalls,
      testCase "typed queries reject different connections with identical private names" casePayloadSessions,
      testCase "discovered endpoint binding checks library-owned read evidence" casePayloadDiscovery,
      testCase "endpoint shape is checked" caseEndpointShape,
      testCase "all liveness durations are checked and observable" caseLivenessConfiguration,
      QC.testProperty "application liveness follows the carried takeover target" propTakeoverLiveness,
      testCase "a queued handshake disposition cannot time out behind local admission" caseQueuedDispositionWinsDeadline,
      testCase "a queued heartbeat cannot start its reply timeout before wire completion" caseHeartbeatReplyStartsAfterWrite,
      QC.testProperty "one-way physical progress cannot defer the reverse heartbeat" propHeartbeatDirectionProgress,
      testCase "an unestablished binding times out and retries at a paced interval" caseUnestablishedHandshakeRetry,
      testCase "default application admission and replies tolerate delays beyond peer timing" caseSlowInitialAdmission,
      testCase "a real loopback transport opens and joins" caseLoopbackOpen,
      testCase "immediate orderly close writes a complete EndSession before EOF" caseOrderlyEndPrecedesEof,
      testCase "prepared connect retries the same initial claim over real TCP" casePreparedConnect,
      testCase "lost startup receipt barrier returns a typed startup error" casePreparedHandoffUnavailable,
      testCase "direct typed calls cover all eight operations and close idempotently" caseTypedFacadeLoopback,
      testCase "typed concurrent calls and wait cancellation keep their original identities" caseTypedConcurrentCalls,
      testCase "cancelling a mutation waiter leaves its exact result available" caseTypedMutationCancellation,
      testCase "withHerald cleans up a throwing callback" caseTypedScopeException,
      testCase "withHerald joins interrupted initial-claim acquisition" (caseTypedAcquisitionCancellation False),
      testCase "withHerald joins interrupted receipt-barrier acquisition" (caseTypedAcquisitionCancellation True),
      testCase "typed own End retains restricted recovery after close" caseTypedEndRecovery,
      testCase "own-End lost terminal and later restricted recovery use no session reopen" caseLifecycleEndRecovery,
      testCase "the eight-operation facade carries every operation and pending reason over loopback" caseCurrentFacadeLoopback,
      testCase "an established connection loss settles its unresolved call as unknown" caseLostReplyTerminal,
      testCase "newenv connection loss settles the application while semantic work belongs to Herald" caseNewEnvironmentDisconnect,
      testCase "heartbeat Ping/Pong shares the serialized writer with semantic calls" caseHeartbeatPingPong,
      testCase "receive-only application traffic still sends heartbeat to the passive server" caseReceiveOnlyHeartbeat,
      testCase "a missed heartbeat ends the application session" caseMissedHeartbeatTerminal,
      testCase "an invariant failure settles a waiting caller" caseInvariantFailureSettlesWaiter,
      testCase "terminal isolation settles admitted work but closes later calls" caseTerminalIsolationCalls,
      testCase "interrupted scope transfers pending ordinary and lifecycle results into escaped handles" caseInterruptedPendingHandles,
      testCase "a call submitted after scope exit is closed" caseEscapedHandleIsClosed
    ]

caseEndpointShape :: Assertion
caseEndpointShape = do
  assertEqual
    "empty host is rejected"
    (Left ApplicationEndpointHostEmpty)
    (applicationEndpoint "" 1)
  assertEqual
    "zero port is rejected"
    (Left ApplicationEndpointPortZero)
    (applicationEndpoint "127.0.0.1" 0)

caseLivenessConfiguration :: Assertion
caseLivenessConfiguration = do
  assertEqual
    "retry delay zero"
    (Left ApplicationRetryDelayMustBePositive)
    (applicationLivenessConfigurationMicroseconds 0 1 1 1)
  assertEqual
    "recovery grace zero"
    (Left ApplicationRecoveryGraceMustBePositive)
    (applicationLivenessConfigurationMicroseconds 1 0 1 1)
  assertEqual
    "heartbeat idle zero"
    (Left ApplicationHeartbeatIdleMustBePositive)
    (applicationLivenessConfigurationMicroseconds 1 1 0 1)
  assertEqual
    "heartbeat reply zero"
    (Left ApplicationHeartbeatReplyTimeoutMustBePositive)
    (applicationLivenessConfigurationMicroseconds 1 1 1 0)
  let configuration =
        checked (applicationLivenessConfigurationMicroseconds 7 11 13 17)
  assertEqual "retry delay" 7 (applicationRetryDelayMicroseconds configuration)
  assertEqual "recovery grace" 11 (applicationRecoveryGraceMicroseconds configuration)
  assertEqual "heartbeat idle" 13 (applicationHeartbeatIdleMicroseconds configuration)
  assertEqual "heartbeat reply" 17 (applicationHeartbeatReplyTimeoutMicroseconds configuration)

propTakeoverLiveness :: QC.Property
propTakeoverLiveness = QC.forAll (QC.choose (500, 60_000_000)) $ \microseconds ->
  let target = checked (Timing.takeoverTarget microseconds)
      configuration = applicationLivenessFromTakeoverTarget target
      policy = Timing.deriveTimingPolicy target
   in [ applicationRetryDelayMicroseconds configuration,
        applicationRecoveryGraceMicroseconds configuration,
        applicationHeartbeatIdleMicroseconds configuration,
        applicationHeartbeatReplyTimeoutMicroseconds configuration
      ]
        QC.=== [ Timing.timingReconnectInitialMicroseconds policy,
                 Timing.timingApplicationRecoveryGraceMicroseconds policy,
                 Timing.timingHeartbeatIdleMicroseconds policy,
                 Timing.defaultApplicationReplyTimeoutMicroseconds
               ]

caseQueuedDispositionWinsDeadline :: Assertion
caseQueuedDispositionWinsDeadline = do
  let queued =
        applicationServerDispositionQueued ApplicationPhysicalHandshaking
  assertEqual
    "the complete disposition stops an already elapsed handshake timer"
    ApplicationEstablishmentWaiting
    (applicationEstablishmentStatus queued True)
  assertEqual
    "the queued disposition makes the physical close claim ineligible"
    False
    (applicationHandshakeTimeoutEligible queued)
  assertEqual
    "pure admission establishes the same binding without a needless close"
    ApplicationEstablishmentReady
    ( applicationEstablishmentStatus
        (applicationPhysicalEstablished queued)
        True
    )

caseHeartbeatReplyStartsAfterWrite :: Assertion
caseHeartbeatReplyStartsAfterWrite = do
  assertEqual
    "queue admission is not a completed heartbeat write"
    ApplicationHeartbeatWriteWaiting
    (applicationHeartbeatWriteStatus ApplicationPhysicalEstablished False)
  assertEqual
    "the writer acknowledgement permits the later reply interval"
    ApplicationHeartbeatWritten
    (applicationHeartbeatWriteStatus ApplicationPhysicalEstablished True)
  assertEqual
    "physical close terminates a still-queued heartbeat"
    ApplicationHeartbeatWriteStopped
    (applicationHeartbeatWriteStatus ApplicationPhysicalClosed False)

caseUnestablishedHandshakeRetry :: Assertion
caseUnestablishedHandshakeRetry =
  bracket openListener close $ \listener -> do
    endpoint <- listenerEndpoint listener
    serverDone <- newEmptyMVar
    _ <-
      forkFinally
        (runUnestablishedHandshakeRetryServer listener)
        (putMVar serverDone)
    result <-
      within "application did not recover from an unestablished binding"
        $ withApplication (fixtureConfigurationWith endpoint fixtureHandshakeRetryLiveness)
        $ \application _ ->
          wait application [] >>= awaitApplicationCall
    assertEqual
      "fresh semantic traffic succeeds on the paced replacement binding"
      (Right fixtureWaitCompletion)
      result
    assertServerJoined serverDone

caseSlowInitialAdmission :: Assertion
caseSlowInitialAdmission =
  bracket openListener close $ \listener -> do
    endpoint <- listenerEndpoint listener
    serverDone <- newEmptyMVar
    _ <-
      forkFinally
        ( bracket (fst <$> accept listener) close $ \connection -> do
            stream <- receiveOpen connection initialClientStream
            -- The default peer reply budget is 500 ms, while application
            -- establishment and later responses share the ten-second policy.
            threadDelay 600_000
            sendServerDto connection fixtureOpened
            (request, _) <- receiveClientDto connection stream
            assertWaitCall request
            -- Also cross the peer idle-plus-reply budget after establishment.
            threadDelay 900_000
            sendServerDto connection (RequestRetained fixtureSession fixtureCursor2 fixtureRequest (Completed fixtureWaitResult))
            drainConnection connection
        )
        (putMVar serverDone)
    result <-
      within
        "delayed application admission did not complete"
        (withApplication (fixtureConfigurationWith endpoint (applicationLivenessFromTakeoverTarget Timing.defaultTakeoverTarget)) (\application access -> (access,) <$> (wait application [] >>= awaitApplicationCall)))
    assertEqual "startup and response complete on the original connection" (Right (fixtureStartup, fixtureWaitCompletion)) result
    assertServerJoined serverDone

caseLoopbackOpen :: Assertion
caseLoopbackOpen =
  bracket openListener close $ \listener -> do
    endpoint <- listenerEndpoint listener
    serverDone <- newEmptyMVar
    _ <-
      forkFinally
        (runFixtureServer listener)
        (putMVar serverDone)
    let configuration =
          fixtureConfiguration endpoint
    result <- within "application scope did not open" (withApplication configuration (\_ access -> pure access))
    assertEqual "startup access crosses TCP" (Right fixtureStartup) result
    assertServerJoined serverDone

caseCurrentFacadeLoopback :: Assertion
caseCurrentFacadeLoopback =
  bracket openListener close $ \listener -> do
    endpoint <- listenerEndpoint listener
    serverDone <- newEmptyMVar
    _ <-
      forkFinally
        (runCurrentFacadeServer listener)
        (putMVar serverDone)
    result <-
      within "current application calls did not complete"
        $ withApplication (fixtureConfiguration endpoint)
        $ \application _ -> do
          bare <- newid application BareNewId >>= awaitApplicationCall
          controlled <- newid application (ControlledNewId fixtureWriter) >>= awaitApplicationCall
          deleted <- write application fixtureWriter (DeleteReserved fixtureObject) >>= awaitApplicationCall
          published <-
            write application fixtureWriter (PublishValue (SortDefinitionValue fixtureDefinition))
              >>= awaitApplicationCall
          ordinary <-
            write application fixtureWriter (PublishValue (BoolValue True))
              >>= awaitApplicationCall
          forwarded <- forward application fixtureWriter fixtureObject >>= awaitApplicationCall
          observed <- read application fixtureQuery >>= awaitApplicationCall
          taken <- localTake application fixtureQuery >>= awaitApplicationCall
          waited <- wait application [fixtureQuery] >>= awaitApplicationCall
          labelled <-
            label application fixtureObject fixtureExpectedLabel LabelToVoid
              >>= awaitApplicationCall
          environment <- newenv application >>= awaitApplicationCall
          pure
            [ bare,
              controlled,
              deleted,
              published,
              ordinary,
              forwarded,
              observed,
              taken,
              waited,
              labelled,
              environment
            ]
    assertEqual
      "all eight operation categories retain their exact typed results"
      (Right (fmap ApplicationCallSucceeded fixtureCurrentResults))
      result
    assertServerJoined serverDone

caseLostReplyTerminal :: Assertion
caseLostReplyTerminal =
  bracket openListener close $ \listener -> do
    endpoint <- listenerEndpoint listener
    serverDone <- newEmptyMVar
    _ <-
      forkFinally
        (runLostReplyServer listener)
        (putMVar serverDone)
    result <-
      within "application scope did not settle the lost reply"
        $ withApplication (fixtureConfiguration endpoint)
        $ \application _ -> do
          call <- wait application []
          awaitApplicationCall call
    assertEqual
      "the lost session cannot determine its unresolved semantic result"
      (Right (ApplicationCallUnknown SessionNoLongerLive))
      result
    assertServerJoined serverDone

caseNewEnvironmentDisconnect :: Assertion
caseNewEnvironmentDisconnect =
  bracket openListener close $ \listener -> do
    endpoint <- listenerEndpoint listener
    serverDone <- newEmptyMVar
    _ <-
      forkFinally
        (runNewEnvironmentDisconnectServer listener)
        (putMVar serverDone)
    result <-
      within "application scope did not settle the disconnected newenv"
        $ withApplication (fixtureConfiguration endpoint)
        $ \application _ ->
          newenv application >>= awaitApplicationCall
    assertEqual
      "accepted semantic work has no application result route after disconnect"
      (Right (ApplicationCallUnknown SessionNoLongerLive))
      result
    assertServerJoined serverDone

propHeartbeatDirectionProgress :: QC.Property
propHeartbeatDirectionProgress =
  QC.forAll ((,) <$> QC.listOf QC.arbitrary <*> QC.listOf1 QC.arbitrary) $ \(prefix, progress) ->
    let advance state inbound = if inbound then applicationHeartbeatReceived state else applicationHeartbeatWritten state
        before = foldl advance initialApplicationHeartbeatActivity prefix
        receivedOnly = foldl (\state _ -> applicationHeartbeatReceived state) before progress
        writtenOnly = foldl (\state _ -> applicationHeartbeatWritten state) before progress
        both = foldl (\state inboundFirst -> advance (advance state inboundFirst) (not inboundFirst)) before progress
     in QC.conjoin
          [ QC.counterexample "server-only traffic must not suppress the client's proof of life" (not (applicationHeartbeatBidirectionalProgress before receivedOnly)),
            QC.counterexample "client-only traffic must still probe server responsiveness" (not (applicationHeartbeatBidirectionalProgress before writtenOnly)),
            QC.counterexample "traffic in both directions already proves physical activity" (applicationHeartbeatBidirectionalProgress before both)
          ]

caseReceiveOnlyHeartbeat :: Assertion
caseReceiveOnlyHeartbeat =
  bracket openListener close $ \listener -> do
    endpoint <- listenerEndpoint listener
    serverDone <- newEmptyMVar
    _ <- forkFinally (runReceiveOnlyServer listener) (putMVar serverDone)
    result <-
      within "server-only traffic lost its live application attachment"
        $ withApplication (fixtureConfigurationWith endpoint fixtureFastHeartbeat)
        $ \application _ -> newenv application >>= awaitApplicationCall
    assertEqual "the pending call completes on its original connection" (Right fixtureEnvironmentCompletion) result
    assertServerJoined serverDone

caseHeartbeatPingPong :: Assertion
caseHeartbeatPingPong =
  bracket openListener close $ \listener -> do
    endpoint <- listenerEndpoint listener
    heartbeatSeen <- newEmptyMVar
    serverDone <- newEmptyMVar
    _ <-
      forkFinally
        (runHeartbeatServer listener heartbeatSeen)
        (putMVar serverDone)
    result <-
      within "application heartbeat did not preserve the live session"
        $ withApplication (fixtureConfigurationWith endpoint fixtureFastHeartbeat)
        $ \application _ -> do
          within "server did not observe heartbeat" (readMVar heartbeatSeen)
          wait application [] >>= awaitApplicationCall
    assertEqual
      "semantic traffic follows heartbeat on the same framed writer"
      (Right fixtureWaitCompletion)
      result
    assertServerJoined serverDone

caseMissedHeartbeatTerminal :: Assertion
caseMissedHeartbeatTerminal =
  bracket openListener close $ \listener -> do
    endpoint <- listenerEndpoint listener
    heartbeatSeen <- newEmptyMVar
    serverDone <- newEmptyMVar
    _ <- forkFinally (runMissedHeartbeatServer listener heartbeatSeen) (putMVar serverDone)
    result <- within "missed heartbeat did not end the application lifetime"
      $ withApplication (fixtureConfigurationWith endpoint fixtureFastHeartbeat)
      $ \application _ -> do
        within "server did not observe heartbeat" (readMVar heartbeatSeen)
        unavailable <- awaitApplicationUnavailable application
        closed <- wait application [] >>= awaitApplicationCall
        pure (unavailable, closed)
    assertEqual
      "heartbeat loss is terminal and subsequent calls are closed"
      (Right (SessionNoLongerLive, ApplicationCallClosed))
      result
    assertServerJoined serverDone

caseInvariantFailureSettlesWaiter :: Assertion
caseInvariantFailureSettlesWaiter =
  bracket openListener close $ \listener -> do
    endpoint <- listenerEndpoint listener
    serverDone <- newEmptyMVar
    _ <-
      forkFinally
        (runInvariantFailureServer listener)
        (putMVar serverDone)
    result <-
      within "invariant failure stranded the callback"
        $ withApplication (fixtureConfiguration endpoint)
        $ \application _ -> do
          call <- wait application []
          awaitApplicationCall call
    assertEqual
      "the pure-owner contradiction is the terminal scoped failure"
      (Left ApplicationClientInvariantFailure)
      result
    assertServerJoined serverDone

caseTerminalIsolationCalls :: Assertion
caseTerminalIsolationCalls =
  bracket openListener close $ \listener -> do
    endpoint <- listenerEndpoint listener
    serverDone <- newEmptyMVar
    _ <-
      forkFinally
        ( bracket (fst <$> accept listener) close $ \connection -> do
            stream <- receiveOpen connection initialClientStream
            sendServerDto connection fixtureOpened
            (request, _) <- receiveClientDto connection stream
            assertNewEnvironmentCall request
            sendServerDto connection (HeraldPermanentlyUnavailable fixtureSession HeraldIsolatedDto)
            drainConnection connection
        )
        (putMVar serverDone)
    result <-
      within "terminal isolation stranded an application call"
        $ withApplication (fixtureConfiguration endpoint)
        $ \application _ -> do
          admitted <- newenv application
          assertEqual
            "an admitted mutation retains the authoritative loss reason"
            (ApplicationCallUnknown HeraldIsolated)
            =<< awaitApplicationCall admitted
          assertEqual
            "terminal disposition is independently observable"
            HeraldIsolated
            =<< awaitApplicationUnavailable application
          assertEqual
            "fresh work on the unavailable attachment closes locally"
            ApplicationCallClosed
            =<< (newenv application >>= awaitApplicationCall)
          assertEqual
            "rejecting later work preserves the terminal disposition"
            HeraldIsolated
            =<< awaitApplicationUnavailable application
    assertEqual "terminal isolation does not fault the scoped runtime" (Right ()) result
    assertServerJoined serverDone

caseInterruptedPendingHandles :: Assertion
caseInterruptedPendingHandles =
  bracket openListener close $ \listener -> do
    endpoint <- listenerEndpoint listener
    serverDone <- newEmptyMVar
    _ <- forkFinally (runFixtureServer listener) (putMVar serverDone)
    ready <- newEmptyMVar
    hold <- newEmptyMVar
    finished <- newEmptyMVar
    thread <-
      forkFinally
        ( withApplication (fixtureConfiguration endpoint) $ \application _ -> do
            ordinary <- wait application []
            lifecycle <- endProcess application
            putMVar ready (application, ordinary, lifecycle)
            readMVar hold
        )
        (putMVar finished)
    (application, ordinary, lifecycle) <- within "pending call handles were not assigned" (readMVar ready)
    killThread thread
    outcome <- within "interrupted application scope did not join" (readMVar finished)
    case outcome of
      Left exception | fromException exception == Just ThreadKilled -> pure ()
      other -> assertFailure ("scope interruption returned unexpectedly: " <> show (other :: Either SomeException (Either ApplicationRuntimeFailure ())))
    assertEqual "pending ordinary result belongs to its escaped handle" ApplicationCallClosed =<< awaitApplicationCall ordinary
    assertEqual "pending lifecycle result belongs to its escaped handle" ApplicationLifecycleClosed =<< awaitApplicationLifecycleCall lifecycle
    cancelApplicationCall ordinary
    cancelApplicationCall ordinary
    assertEqual "post-terminal cancellation preserves the repeatable result" ApplicationCallClosed =<< awaitApplicationCall ordinary
    assertEqual "lifecycle result remains repeatable after routing cleanup" ApplicationLifecycleClosed =<< awaitApplicationLifecycleCall lifecycle
    assertEqual "a stopped owner rejects new work locally" ApplicationCallClosed =<< (wait application [] >>= awaitApplicationCall)
    assertServerJoined serverDone

caseEscapedHandleIsClosed :: Assertion
caseEscapedHandleIsClosed =
  bracket openListener close $ \listener -> do
    endpoint <- listenerEndpoint listener
    serverDone <- newEmptyMVar
    _ <-
      forkFinally
        (runFixtureServer listener)
        (putMVar serverDone)
    opened <-
      within "application scope did not open"
        $ withApplication (fixtureConfiguration endpoint)
        $ \application _ ->
          pure application
    application <- case opened of
      Left failure -> assertFailure (show failure) >> error "unreachable"
      Right handle -> pure handle
    call <- wait application []
    completion <- within "post-scope call did not settle" (awaitApplicationCall call)
    assertEqual "the scoped handle cannot create stranded work" ApplicationCallClosed completion
    assertServerJoined serverDone

fixtureConfiguration :: ApplicationEndpoint -> ApplicationConfiguration
fixtureConfiguration endpoint =
  fixtureConfigurationWith endpoint fixtureLivenessConfiguration

fixtureConfigurationWith ::
  ApplicationEndpoint ->
  ApplicationLivenessConfiguration ->
  ApplicationConfiguration
fixtureConfigurationWith endpoint liveness =
  applicationConfiguration
    endpoint
    fixtureAttachment
    fixtureNonce
    liveness

fixtureLivenessConfiguration :: ApplicationLivenessConfiguration
fixtureLivenessConfiguration =
  checked (applicationLivenessConfigurationMicroseconds 10_000 1_000_000 200_000 200_000)

fixtureFastHeartbeat :: ApplicationLivenessConfiguration
fixtureFastHeartbeat =
  checked (applicationLivenessConfigurationMicroseconds 20_000 3_000_000 100_000 400_000)

fixtureHandshakeRetryLiveness :: ApplicationLivenessConfiguration
fixtureHandshakeRetryLiveness =
  checked (applicationLivenessConfigurationMicroseconds 400_000 3_000_000 2_000_000 100_000)

minimumObservedHandshakeRetryMicroseconds :: Word64
minimumObservedHandshakeRetryMicroseconds = 300_000

within :: String -> IO value -> IO value
within message action = do
  result <- timeout 5_000_000 action
  case result of
    Nothing -> ioError (userError message)
    Just value -> pure value

assertServerJoined :: MVar (Either SomeException ()) -> Assertion
assertServerJoined serverDone = do
  joined <- timeout 3000000 (readMVar serverDone)
  case joined of
    Nothing -> assertFailure "fixture server did not observe scoped close"
    Just (Left exception) -> assertFailure (show exception)
    Just (Right ()) -> pure ()

openListener :: IO Socket
openListener = do
  listener <- socket AF_INET Stream defaultProtocol
  setSocketOption listener ReuseAddr 1
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listener 1
  pure listener

listenerEndpoint :: Socket -> IO ApplicationEndpoint
listenerEndpoint listener = do
  address <- getSocketName listener
  case address of
    SockAddrInet port _ ->
      pure (checked (applicationEndpoint "127.0.0.1" (fromIntegral port)))
    _ -> ioError (userError "fixture listener did not bind IPv4")

caseOrderlyEndPrecedesEof :: Assertion
caseOrderlyEndPrecedesEof =
  bracket openListener close $ \listener -> do
    endpoint <- listenerEndpoint listener
    serverDone <- newEmptyMVar
    let samples = 32
    _ <-
      forkFinally
        ( replicateM_ samples
            $ bracket (fst <$> accept listener) close
            $ \connection -> do
              stream <- receiveOpen connection initialClientStream
              sendServerDto connection fixtureOpened
              (final, ClientStream _ remaining) <- receiveRawClientDto connection stream
              assertEqual "the complete final frame is the established session's End" (EndSession fixtureSession) final
              assertEqual "EndSession is the final queued semantic frame" [] remaining
              assertEqual "EOF follows the final frame" ByteString.empty =<< SocketBytes.recv connection 1
        )
        (putMVar serverDone)
    replicateM_ samples $ do
      result <- within "orderly End write did not release scope shutdown" (withApplication (fixtureConfiguration endpoint) (\_ _ -> pure ()))
      assertEqual "orderly shutdown succeeds" (Right ()) result
    assertServerJoined serverDone

runFixtureServer :: Socket -> IO ()
runFixtureServer listener =
  bracket (fst <$> accept listener) close $ \connection -> do
    _ <- receiveOpen connection initialClientStream
    sendServerDto connection fixtureOpened
    drainConnection connection

runUnestablishedHandshakeRetryServer :: Socket -> IO ()
runUnestablishedHandshakeRetryServer listener = do
  firstOpenAt <-
    bracket (fst <$> accept listener) close $ \connection -> do
      _ <- receiveOpen connection initialClientStream
      observedAt <- getMonotonicTimeNSec
      within
        "client did not close the silent unestablished binding"
        (drainConnection connection)
      pure observedAt
  bracket (fst <$> accept listener) close $ \connection -> do
    stream <- receiveOpen connection initialClientStream
    secondOpenAt <- getMonotonicTimeNSec
    let elapsedMicroseconds = (secondOpenAt - firstOpenAt) `div` 1000
    -- The explicit configured reply timeout is 100 ms and retry is 400 ms.
    -- This conservative floor leaves timer tolerance but prevents an immediate
    -- post-deadline reconnect from satisfying the assertion.
    if elapsedMicroseconds < minimumObservedHandshakeRetryMicroseconds
      then
        assertFailure
          ( "replacement Open arrived before the paced retry interval: "
              <> show elapsedMicroseconds
              <> " microseconds"
          )
      else pure ()
    sendServerDto connection fixtureOpened
    (call, _) <- receiveClientDto connection stream
    assertWaitCall call
    sendServerDto
      connection
      ( RequestRetained
          fixtureSession
          fixtureCursor2
          fixtureRequest
          (Completed fixtureWaitResult)
      )
    drainConnection connection

runCurrentFacadeServer :: Socket -> IO ()
runCurrentFacadeServer listener =
  bracket (fst <$> accept listener) close $ \connection -> do
    stream <- receiveOpen connection initialClientStream
    sendServerDto connection fixtureOpened
    _ <- serveCurrentCalls connection stream 0 2 fixtureCurrentCalls
    drainConnection connection

serveCurrentCalls ::
  Socket ->
  ClientStream ->
  Word64 ->
  Word64 ->
  [(ApplicationOperation, RegularCallResult, Maybe OperationPendingReason)] ->
  IO ClientStream
serveCurrentCalls _ stream _ _ [] = pure stream
serveCurrentCalls connection stream requestWord replyCursor ((expectedOperation, result, pendingReason) : remaining) = do
  (dto, successor) <- receiveClientDto connection stream
  let request = applicationRequestIdClaim requestWord
  case dto of
    Call session actualRequest operation -> do
      assertEqual "call session" fixtureSession session
      assertEqual "call request" request actualRequest
      assertEqual "call operation" expectedOperation operation
    other -> assertFailure ("expected current Call, got " <> show other)
  case pendingReason of
    Just reason ->
      sendServerDto
        connection
        ( RequestRetained
            fixtureSession
            (checked (applicationReplyCursorClaim replyCursor))
            request
            (OperationAccepted reason)
        )
    Nothing -> pure ()
  let terminalCursor = replyCursor + maybe 0 (const 1) pendingReason
  sendServerDto
    connection
    ( RequestRetained
        fixtureSession
        (checked (applicationReplyCursorClaim terminalCursor))
        request
        (Completed result)
    )
  serveCurrentCalls connection successor (requestWord + 1) (terminalCursor + 1) remaining

runLostReplyServer :: Socket -> IO ()
runLostReplyServer listener =
  bracket (fst <$> accept listener) close $ \connection -> do
    stream <- receiveOpen connection initialClientStream
    sendServerDto connection fixtureOpened
    (dto, _) <- receiveClientDto connection stream
    assertWaitCall dto

runNewEnvironmentDisconnectServer :: Socket -> IO ()
runNewEnvironmentDisconnectServer listener =
  bracket (fst <$> accept listener) close $ \connection -> do
    stream <- receiveOpen connection initialClientStream
    sendServerDto connection fixtureOpened
    (dto, _) <- receiveClientDto connection stream
    assertNewEnvironmentCall dto
    sendServerDto
      connection
      (RequestRetained fixtureSession fixtureCursor2 fixtureRequest (OperationAccepted EnvironmentStabilizationPending))

runReceiveOnlyServer :: Socket -> IO ()
runReceiveOnlyServer listener =
  bracket (fst <$> accept listener) close $ \connection -> do
    stream <- receiveOpen connection initialClientStream
    sendServerDto connection fixtureOpened
    (request, successor) <- receiveClientDto connection stream
    assertNewEnvironmentCall request
    received <- newEmptyMVar :: IO (MVar (Either SomeException (ApplicationClientDto, ClientStream)))
    let pending = RequestRetained fixtureSession fixtureCursor2 fixtureRequest (OperationAccepted EnvironmentStabilizationPending)
        sendUntilClientResponds =
          tryReadMVar received >>= \case
            Just result -> either throwIO pure result
            Nothing -> do
              -- Exact replay is legitimate server traffic while the admitted
              -- mutation remains pending; it supplies no client-side activity.
              sendServerDto connection pending
              threadDelay 10_000
              sendUntilClientResponds
    bracket
      (forkIOWithUnmask (\unmask -> try (unmask (receiveClientDto connection successor)) >>= putMVar received))
      (\worker -> killThread worker >> void (readMVar received))
      $ \_ -> do
        -- The passive server expects client activity within its 100 ms idle
        -- interval plus 400 ms reply window, despite sending frequent replies.
        observed <- timeout 500_000 sendUntilClientResponds
        nonce <- case observed of
          Just (Ping value, _) -> pure value
          other -> assertFailure ("receive-only client did not prove liveness to its passive server: " <> maybe "idle deadline elapsed" (show . fst) other) >> error "unreachable"
        sendServerDto connection (Pong nonce)
        sendServerDto connection (RequestRetained fixtureSession fixtureCursor3 fixtureRequest (Completed fixtureEnvironmentResult))
        drainConnection connection

runHeartbeatServer :: Socket -> MVar () -> IO ()
runHeartbeatServer listener heartbeatSeen =
  bracket (fst <$> accept listener) close $ \connection -> do
    stream <- receiveOpen connection initialClientStream
    sendServerDto connection fixtureOpened
    (heartbeat, successor) <- receiveClientDto connection stream
    nonce <- case heartbeat of
      Ping observedNonce -> pure observedNonce
      other -> assertFailure ("expected idle Ping, got " <> show other) >> error "unreachable"
    sendServerDto connection (Pong nonce)
    putMVar heartbeatSeen ()
    (call, _) <- receiveClientDto connection successor
    assertWaitCall call
    sendServerDto
      connection
      ( RequestRetained
          fixtureSession
          fixtureCursor2
          fixtureRequest
          (Completed fixtureWaitResult)
      )
    drainConnection connection

runMissedHeartbeatServer :: Socket -> MVar () -> IO ()
runMissedHeartbeatServer listener heartbeatSeen =
  bracket (fst <$> accept listener) close $ \connection -> do
    stream <- receiveOpen connection initialClientStream
    sendServerDto connection fixtureOpened
    (heartbeat, _) <- receiveClientDto connection stream
    case heartbeat of
      Ping _ -> putMVar heartbeatSeen ()
      other -> assertFailure ("expected unanswered Ping, got " <> show other)
    _ <- try (drainConnection connection) :: IO (Either IOException ())
    pure ()

runInvariantFailureServer :: Socket -> IO ()
runInvariantFailureServer listener =
  bracket (fst <$> accept listener) close $ \connection -> do
    stream <- receiveOpen connection initialClientStream
    sendServerDto connection fixtureOpened
    (dto, _) <- receiveClientDto connection stream
    assertWaitCall dto
    sendServerDto connection fixtureOpened
    drainConnection connection

data ClientStream
  = ClientStream
      ApplicationFrameDecoder
      [ApplicationClientDto]

initialClientStream :: ClientStream
initialClientStream =
  ClientStream
    (initialApplicationFrameDecoder ApplicationClientFrames)
    []

receiveOpen :: Socket -> ClientStream -> IO ClientStream
receiveOpen connection stream = do
  (dto, successor) <- receiveClientDto connection stream
  case dto of
    OpenSession attachment nonce -> do
      assertEqual "open attachment" fixtureAttachment attachment
      assertEqual "open nonce" fixtureNonce nonce
      pure successor
    other -> assertFailure ("expected OpenSession, got " <> show other) >> pure successor

-- Fake servers process lifetime metadata before exposing semantic work to
-- endpoint-specific assertions. The acknowledgement carries no reply cursor.
receiveClientDto :: Socket -> ClientStream -> IO (ApplicationClientDto, ClientStream)
receiveClientDto connection stream = do
  (dto, successor) <- receiveRawClientDto connection stream
  case dto of
    RetireReceipts session progress -> do
      assertEqual "retirement session" fixtureSession session
      sendServerDto connection (ReceiptsRetired session progress)
      receiveClientDto connection successor
    CallWithRetirement session request operation progress -> do
      sendServerDto connection (ReceiptsRetired session progress)
      pure (Call session request operation, successor)
    LifecycleCallWithRetirement session request command progress -> do
      sendServerDto connection (ReceiptsRetired session progress)
      pure (LifecycleCall session request command, successor)
    _ -> pure (dto, successor)

receiveStartupReceiptBarrier :: Socket -> ClientStream -> IO ClientStream
receiveStartupReceiptBarrier connection stream = do
  (dto, successor) <- receiveRawClientDto connection stream
  assertEqual
    "prepared startup acknowledges its session on the same connection"
    (RetireReceipts fixtureSession mempty)
    dto
  sendServerDto connection (ReceiptsRetired fixtureSession mempty)
  pure successor

receiveRawClientDto :: Socket -> ClientStream -> IO (ApplicationClientDto, ClientStream)
receiveRawClientDto _ (ClientStream decoder (dto : pending)) =
  pure (dto, ClientStream decoder pending)
receiveRawClientDto connection (ClientStream decoder []) = do
  chunk <- SocketBytes.recv connection 32768
  if ByteString.null chunk
    then ioError (userError "client closed before the expected application DTO")
    else case feedApplicationFrame decoder chunk of
      ApplicationFrameFeedResult envelopes continuation -> do
        successor <- case continuation of
          NeedApplicationFrameBytes next -> pure next
          ApplicationFrameFailed failure -> ioError (userError (show failure))
        let dtos =
              [ dto
              | ApplicationClientEnvelope dto <- envelopes
              ]
        receiveRawClientDto connection (ClientStream successor dtos)

assertWaitCall :: ApplicationClientDto -> Assertion
assertWaitCall = \case
  Call session request (WaitApplication []) -> do
    assertEqual "call session" fixtureSession session
    assertEqual "first request" fixtureRequest request
  other -> assertFailure ("expected empty-query Wait call, got " <> show other)

assertNewEnvironmentCall :: ApplicationClientDto -> Assertion
assertNewEnvironmentCall = \case
  Call session request NewEnvironmentApplication -> do
    assertEqual "newenv call session" fixtureSession session
    assertEqual "newenv first request" fixtureRequest request
  other -> assertFailure ("expected newenv Call, got " <> show other)

sendServerDto :: Socket -> ApplicationServerDto -> IO ()
sendServerDto connection =
  SocketBytes.sendAll connection
    . encodeApplicationFrame
    . ApplicationServerEnvelope

drainConnection :: Socket -> IO ()
drainConnection connection = do
  chunk <- SocketBytes.recv connection 32768
  if ByteString.null chunk then pure () else drainConnection connection

fixtureScope :: ByteString.ByteString
fixtureScope = ByteString.replicate 32 7

fixtureAttachment :: ApplicationAttachmentClaim
fixtureAttachment = checked (applicationAttachmentClaim (ByteString.replicate 32 8))

fixtureNonce :: ApplicationClientNonce
fixtureNonce = applicationClientNonce 42

fixtureSession :: ApplicationSessionClaim
fixtureSession = checked (applicationSessionClaim fixtureScope 1)

fixtureToken :: ApplicationResumeTokenClaim
fixtureToken = checked (applicationResumeTokenClaim fixtureScope 1)

fixtureCursor :: ApplicationReplyCursorClaim
fixtureCursor = checked (applicationReplyCursorClaim 1)

fixtureCursor2 :: ApplicationReplyCursorClaim
fixtureCursor2 = checked (applicationReplyCursorClaim 2)

fixtureCursor3 :: ApplicationReplyCursorClaim
fixtureCursor3 = checked (applicationReplyCursorClaim 3)

fixtureRequest :: ApplicationRequestIdClaim
fixtureRequest = applicationRequestIdClaim 0

fixtureWaitResult :: RegularCallResult
fixtureWaitResult = WaitCompleted WaitReady

fixtureWaitCompletion :: ApplicationCallCompletion
fixtureWaitCompletion = ApplicationCallSucceeded fixtureWaitResult

fixtureEnvironmentResult :: RegularCallResult
fixtureEnvironmentResult = NewEnvironmentCompleted fixtureEnvironmentAccess

fixtureEnvironmentCompletion :: ApplicationCallCompletion
fixtureEnvironmentCompletion = ApplicationCallSucceeded fixtureEnvironmentResult

fixtureOpened :: ApplicationServerDto
fixtureOpened =
  SessionOpened fixtureCursor fixtureSession fixtureToken fixtureStartup

fixtureStartup :: ApplicationStartupAccess
fixtureStartup =
  applicationStartupAccess
    (asPrivateProcessId (privateId 1))
    ( primordialAccessFromSelection
        ( selectEnvironment
            ( checked
                ( environmentAccess
                    [ predefinedAccess role (asPrivateNablaId (privateId writer)) (asPrivateDeltaId (privateId reader))
                    | (role, writer, reader) <- zip3 allApplicationPredefinedSortRoles [2 ..] [20 ..]
                    ]
                    (asPrivateObjectId (privateId 1000))
                    [asPrivateObjectId (privateId value) | value <- [1001 .. 1018]]
                )
            )
        )
    )

fixtureWriter :: PrivateNablaId
fixtureWriter = asPrivateNablaId (privateId 2)

fixtureObject :: PrivateObjectId
fixtureObject = asPrivateObjectId (privateId 40)

fixtureExpectedLabel :: ApplicationLabel
fixtureExpectedLabel = (ProcessLabel (asPrivateProcessId (privateId 1)), 7)

fixtureSortId :: SortId
fixtureSortId = checked (mkSortId (ByteString.pack [0 .. 31]))

fixtureDefinition :: ApplicationSortDefinition
fixtureDefinition = PredefinedSortDefinition SortDefinitionRole fixtureSortId

fixtureQuery :: ApplicationQuery
fixtureQuery = ApplicationQuery mempty QueryAlways

fixtureCurrentCalls :: [(ApplicationOperation, RegularCallResult, Maybe OperationPendingReason)]
fixtureCurrentCalls =
  [ (NewIdApplication BareNewId, NewIdCompleted (privateId 50), Nothing),
    (NewIdApplication (ControlledNewId fixtureWriter), NewIdCompleted (privateId 51), Nothing),
    (WriteApplication fixtureWriter (DeleteReserved fixtureObject), WriteCompleted WriteAccepted, Nothing),
    ( WriteApplication fixtureWriter (PublishValue (SortDefinitionValue fixtureDefinition)),
      WriteCompleted (SortDefinitionWritten fixtureSortId),
      Just StructuralStabilizationPending
    ),
    (WriteApplication fixtureWriter (PublishValue (BoolValue True)), WriteCompleted WriteAccepted, Nothing),
    (ForwardApplication fixtureWriter fixtureObject, ForwardCompleted ForwardAccepted, Just StructuralStabilizationPending),
    (ReadApplication fixtureQuery, ReadCompleted [], Nothing),
    (LocalTakeApplication fixtureQuery, LocalTakeCompleted [], Nothing),
    (WaitApplication [fixtureQuery], WaitCompleted WaitReady, Nothing),
    ( LabelApplication fixtureObject fixtureExpectedLabel LabelToVoid,
      LabelCompleted LabelApplied,
      Just LabelSettlementPending
    ),
    ( NewEnvironmentApplication,
      NewEnvironmentCompleted fixtureEnvironmentAccess,
      Just EnvironmentStabilizationPending
    )
  ]

fixtureCurrentResults :: [RegularCallResult]
fixtureCurrentResults = [result | (_, result, _) <- fixtureCurrentCalls]

fixtureEnvironmentAccess :: EnvironmentAccess
fixtureEnvironmentAccess =
  checked
    ( environmentAccess
        [ predefinedAccess
            role
            (asPrivateNablaId (privateId writer))
            (asPrivateDeltaId (privateId reader))
        | (role, writer, reader) <- zip3 allApplicationPredefinedSortRoles [60 ..] [70 ..]
        ]
        (asPrivateObjectId (privateId 1000))
        [asPrivateObjectId (privateId value) | value <- [1001 .. 1018]]
    )

privateId :: Word64 -> PrivateUniqueId
privateId = checked . mkPrivateUniqueId

checked :: (Show error) => Either error value -> value
checked = either (error . show) id

casePreparedConnect :: Assertion
casePreparedConnect = bracket openListener close $ \listener -> do
  endpoint <- listenerEndpoint listener
  let locator = checked (Lifecycle.heraldLocator (applicationEndpointHost endpoint) (applicationEndpointPort endpoint))
      descriptor = checked (Lifecycle.connectionDescriptor locator fixtureScope fixtureScope (ByteString.replicate 32 8))
      claim = checked (Lifecycle.initialClaimId (ByteString.replicate 32 9))
      configuration = preparedApplicationConfiguration descriptor claim fixtureLivenessConfiguration
      expected = ClaimInitial descriptor claim
  serverDone <- newEmptyMVar
  startupObserved <- newEmptyMVar
  _ <-
    forkFinally
      ( do
          bracket (fst <$> accept listener) close $ \connection -> do
            (actual, _) <- receiveClientDto connection initialClientStream
            assertEqual "first stable initial claim" expected actual
          bracket (fst <$> accept listener) close $ \connection -> do
            (actual, stream) <- receiveClientDto connection initialClientStream
            assertEqual "exact initial claim on replacement before establishment" expected actual
            sendServerDto connection (InitialClaimPending claim ["reader"])
            sendServerDto connection fixtureOpened
            (barrier, _) <- receiveRawClientDto connection stream
            assertEqual "startup confirmation uses the established connection" (RetireReceipts fixtureSession mempty) barrier
            assertEqual "startup waits for its receipt confirmation" Nothing =<< tryReadMVar startupObserved
            sendServerDto connection (ReceiptsRetired fixtureSession mempty)
            within "startup did not follow receipt confirmation" (readMVar startupObserved)
            drainConnection connection
      )
      (putMVar serverDone)
  result <- within "prepared connect did not finish its receipt confirmation" $ withApplication configuration $ \_ access -> do
    putMVar startupObserved ()
    pure access
  assertEqual "startup returned after same-connection confirmation" (Right fixtureStartup) result
  assertServerJoined serverDone

caseLifecycleEndRecovery :: Assertion
caseLifecycleEndRecovery = bracket openListener close $ \listener -> do
  endpoint <- listenerEndpoint listener
  let request = checked (Lifecycle.lifecycleRequestId fixtureScope 1 0)
      configuration = fixtureConfiguration endpoint
      expected = ApplicationLifecycleSucceeded Lifecycle.ProcessEnded
      resultReply = LifecycleReply request (Lifecycle.LifecycleCompleted Lifecycle.ProcessEnded)
      recoverOnce = bracket (fst <$> accept listener) close $ \connection -> do
        (query, _) <- receiveClientDto connection initialClientStream
        assertEqual "only attachment-scoped End result query" (RecoverLifecycleResult fixtureAttachment request) query
        sendServerDto connection resultReply
        drainConnection connection
  serverDone <- newEmptyMVar
  _ <-
    forkFinally
      ( do
          bracket (fst <$> accept listener) close $ \connection -> do
            stream <- receiveOpen connection initialClientStream
            sendServerDto connection fixtureOpened
            (command, _) <- receiveClientDto connection stream
            assertEqual "own End has a session-qualified lifecycle correlation" (LifecycleCall fixtureSession request Lifecycle.EndOwnProcess) command
            sendServerDto connection (LifecycleReply request (Lifecycle.LifecyclePending []))
          recoverOnce
          recoverOnce
      )
      (putMVar serverDone)
  result <- within "own End did not recover" $ withApplication configuration $ \application _ -> do
    call <- endProcess application
    assertEqual "public handle retains full recovery correlation" (Just request) (applicationLifecycleRequestId call)
    awaitApplicationLifecycleCall call
  assertEqual "ordered End terminal" (Right expected) result
  recovered <- within "later terminal recovery did not finish" (recoverEndProcess configuration request)
  assertEqual "separate query has same retained terminal" (Right expected) recovered
  assertServerJoined serverDone

descriptorAt :: ApplicationEndpoint -> Lifecycle.ConnectionDescriptor
descriptorAt endpoint = checked (Lifecycle.connectionDescriptor locator fixtureScope fixtureScope (ByteString.replicate 32 8))
  where
    locator = checked (Lifecycle.heraldLocator (applicationEndpointHost endpoint) (applicationEndpointPort endpoint))

withTypedFixture :: (Socket -> ClientStream -> IO ()) -> (Typed.Herald -> IO ()) -> Assertion
withTypedFixture = withTypedFixtureStartup fixtureStartup

withTypedFixtureStartup :: ApplicationStartupAccess -> (Socket -> ClientStream -> IO ()) -> (Typed.Herald -> IO ()) -> Assertion
withTypedFixtureStartup startupAccess serve use = bracket openListener close $ \listener -> do
  endpoint <- listenerEndpoint listener
  serverDone <- newEmptyMVar
  _ <- forkFinally (preparedFixtureServerStartup startupAccess listener serve) (putMVar serverDone)
  connected <- Advanced.connectWith fixtureLivenessConfiguration (checked (Lifecycle.initialClaimId fixtureScope)) (descriptorAt endpoint)
  herald <- either (assertFailure . show) pure connected
  use herald `finally` Typed.disconnect herald
  Typed.disconnect herald
  assertEqual "calls after close are typed" (Left Typed.CallClosed) =<< Typed.newid herald BareNewId
  assertServerJoined serverDone

preparedFixtureServer :: Socket -> (Socket -> ClientStream -> IO ()) -> IO ()
preparedFixtureServer = preparedFixtureServerStartup fixtureStartup

preparedFixtureServerStartup :: ApplicationStartupAccess -> Socket -> (Socket -> ClientStream -> IO ()) -> IO ()
preparedFixtureServerStartup startupAccess listener serve =
  bracket (fst <$> accept listener) close $ \connection -> do
    (claim, stream) <- receiveClientDto connection initialClientStream
    case claim of
      ClaimInitial _ _ -> pure ()
      other -> assertFailure ("expected initial claim: " <> show other)
    sendServerDto connection (SessionOpened fixtureCursor fixtureSession fixtureToken startupAccess)
    established <- receiveStartupReceiptBarrier connection stream
    serve connection established
    drainConnection connection

caseTypedEndRecovery :: Assertion
caseTypedEndRecovery = bracket openListener close $ \listener -> do
  endpoint <- listenerEndpoint listener
  serverDone <- newEmptyMVar
  let request = checked (Lifecycle.lifecycleRequestId fixtureScope 1 0)
      recoverOnce = bracket (fst <$> accept listener) close $ \connection -> do
        (query, _) <- receiveClientDto connection initialClientStream
        assertEqual "typed End recovery retains attachment and full request" (RecoverLifecycleResult fixtureAttachment request) query
        sendServerDto connection (LifecycleReply request (Lifecycle.LifecycleCompleted Lifecycle.ProcessEnded))
        drainConnection connection
  _ <-
    forkFinally
      ( do
          bracket (fst <$> accept listener) close $ \connection -> do
            (claim, stream) <- receiveClientDto connection initialClientStream
            case claim of { ClaimInitial _ _ -> pure (); other -> assertFailure (show other) }
            sendServerDto connection fixtureOpened
            established <- receiveStartupReceiptBarrier connection stream
            (command, _) <- receiveClientDto connection established
            assertEqual "typed own End" (LifecycleCall fixtureSession request Lifecycle.EndOwnProcess) command
            sendServerDto connection (LifecycleReply request (Lifecycle.LifecyclePending []))
          recoverOnce
          recoverOnce
      )
      (putMVar serverDone)
  connected <- Advanced.connectWith fixtureLivenessConfiguration (checked (Lifecycle.initialClaimId fixtureScope)) (descriptorAt endpoint)
  herald <- either (assertFailure . show) pure connected
  call <- Advanced.submitLifecycle herald Advanced.EndProcess
  assertEqual "typed End correlation" (Just request) (Advanced.lifecycleRequestId call)
  assertEqual "typed End recovers terminal loss" (Right ()) =<< within "typed End recovery" (Advanced.awaitLifecycle call)
  Typed.disconnect herald
  assertEqual "restricted recovery after disconnect" (Right ()) =<< Advanced.recoverEndProcess herald request
  assertServerJoined serverDone

caseTypedFacadeLoopback :: Assertion
caseTypedFacadeLoopback = withTypedFixture serve $ \herald -> do
  assertEqual "typed startup" fixtureStartup (Typed.startup herald)
  assertEqual "bare newid" (Right (privateId 50)) =<< Typed.newid herald BareNewId
  assertEqual "controlled newid" (Right (privateId 51)) =<< Typed.newid herald (ControlledNewId fixtureWriter)
  assertEqual "delete" (Right WriteAccepted) =<< Typed.write herald fixtureWriter (DeleteReserved fixtureObject)
  assertEqual "structural publish" (Right (SortDefinitionWritten fixtureSortId)) =<< Typed.write herald fixtureWriter (PublishValue (SortDefinitionValue fixtureDefinition))
  assertEqual "regular publish" (Right WriteAccepted) =<< Typed.write herald fixtureWriter (PublishValue (BoolValue True))
  assertEqual "forward" (Right ForwardAccepted) =<< Typed.forward herald fixtureWriter fixtureObject
  assertEqual "read" (Right []) =<< Typed.read herald fixtureQuery
  assertEqual "take" (Right []) =<< Typed.localTake herald fixtureQuery
  assertEqual "wait" (Right WaitReady) =<< Typed.wait herald [fixtureQuery]
  assertEqual "label" (Right LabelApplied) =<< Typed.label herald fixtureObject fixtureExpectedLabel LabelToVoid
  assertEqual "newenv" (Right fixtureEnvironmentAccess) =<< Typed.newenv herald
  where
    serve connection stream = void (serveCurrentCalls connection stream 0 2 fixtureCurrentCalls)

caseTypedConcurrentCalls :: Assertion
caseTypedConcurrentCalls = withTypedFixture serve $ \herald -> do
  environment <- Advanced.submit herald Advanced.NewEnvironment
  observed <- Advanced.submit herald (Advanced.Read fixtureQuery)
  assertEqual "read completes independently of pending environment" (Right []) =<< Advanced.await observed
  assertEqual "environment retains its typed eventual result" (Right fixtureEnvironmentAccess) =<< Advanced.await environment
  waiting <- Advanced.submit herald (Advanced.Wait [fixtureQuery])
  Advanced.cancel waiting
  assertEqual "wait cancellation settles retained winner" (Left Typed.CallCancelled) =<< Advanced.await waiting
  assertEqual "terminal typed status is repeatable" (Advanced.CallComplete (Right fixtureEnvironmentAccess)) =<< Advanced.status environment
  where
    serve connection stream = do
      (environment, afterEnvironment) <- receiveClientDto connection stream
      (observed, afterRead) <- receiveClientDto connection afterEnvironment
      assertEqual "original first invocation" (Call fixtureSession (applicationRequestIdClaim 0) NewEnvironmentApplication) environment
      assertEqual "independent second invocation" (Call fixtureSession (applicationRequestIdClaim 1) (ReadApplication fixtureQuery)) observed
      sendServerDto connection (RequestRetained fixtureSession fixtureCursor2 (applicationRequestIdClaim 0) (OperationAccepted EnvironmentStabilizationPending))
      sendServerDto connection (RequestRetained fixtureSession fixtureCursor3 (applicationRequestIdClaim 1) (Completed (ReadCompleted [])))
      sendServerDto connection (RequestRetained fixtureSession (cursor 4) (applicationRequestIdClaim 0) (Completed fixtureEnvironmentResult))
      (waiting, afterWait) <- receiveClientDto connection afterRead
      let request = applicationRequestIdClaim 2
          waitClaim = checked (applicationWaitClaim fixtureScope 1 request)
      assertEqual "wait invocation remains separate" (Call fixtureSession request (WaitApplication [fixtureQuery])) waiting
      sendServerDto connection (RequestRetained fixtureSession (cursor 5) request (WaitAccepted waitClaim))
      (cancelled, _) <- receiveClientDto connection afterWait
      assertEqual "existing wait correlation is cancelled" (CancelPendingWait fixtureSession request waitClaim) cancelled
      sendServerDto connection (RequestRetained fixtureSession (cursor 6) request (Cancelled waitClaim))
    cursor = checked . applicationReplyCursorClaim

caseTypedMutationCancellation :: Assertion
caseTypedMutationCancellation = do
  accepted <- newEmptyMVar
  release <- newEmptyMVar
  withTypedFixture (serve accepted release) $ \herald -> do
    call <- Advanced.submit herald Advanced.NewEnvironment
    waiterDone <- newEmptyMVar
    waiter <- forkFinally (Advanced.await call) (putMVar waiterDone)
    within "mutation acceptance missing" (readMVar accepted)
    killThread waiter
    _ <- within "cancelled mutation waiter did not exit promptly" (readMVar waiterDone)
    assertEqual "semantic result remains pending" Advanced.CallPending =<< Advanced.status call
    putMVar release ()
    assertEqual "same call receives the accepted result after waiter cancellation" (Right fixtureEnvironmentAccess) =<< Advanced.await call
  where
    serve accepted release connection stream = do
      (request, _) <- receiveClientDto connection stream
      assertEqual "one submitted mutation" (Call fixtureSession fixtureRequest NewEnvironmentApplication) request
      sendServerDto connection (RequestRetained fixtureSession fixtureCursor2 fixtureRequest (OperationAccepted EnvironmentStabilizationPending))
      putMVar accepted ()
      _ <- readMVar release
      sendServerDto connection (RequestRetained fixtureSession fixtureCursor3 fixtureRequest (Completed fixtureEnvironmentResult))

caseTypedScopeException :: Assertion
caseTypedScopeException = bracket openListener close $ \listener -> do
  endpoint <- listenerEndpoint listener
  serverDone <- newEmptyMVar
  accepted <- newEmptyMVar
  let serve connection stream = do
        (request, _) <- receiveClientDto connection stream
        assertEqual "mutation accepted before scope exception" (Call fixtureSession fixtureRequest NewEnvironmentApplication) request
        sendServerDto connection (RequestRetained fixtureSession fixtureCursor2 fixtureRequest (OperationAccepted EnvironmentStabilizationPending))
        putMVar accepted ()
  _ <- forkFinally (preparedFixtureServer listener serve) (putMVar serverDone)
  result <-
    try
      ( within
          "scope cleanup waited for pending mutation"
          ( Typed.withHerald
              (descriptorAt endpoint)
              ( \herald -> do
                  _ <- Advanced.submit herald Advanced.NewEnvironment
                  readMVar accepted
                  ioError (userError "callback failure")
              )
          )
      ) ::
      IO (Either IOException (Either Typed.ConnectError ()))
  case result of
    Left _ -> pure ()
    Right other -> assertFailure ("callback exception was lost: " <> show other)
  assertServerJoined serverDone

-- Gate cancellation on wire admission, before receipt acknowledgement makes
-- the same established socket available to the application callback.
caseTypedAcquisitionCancellation :: Bool -> Assertion
caseTypedAcquisitionCancellation duringHandoff = bracket openListener close $ \listener -> do
  endpoint <- listenerEndpoint listener
  serverDone <- newEmptyMVar
  acquisitionPending <- newEmptyMVar
  callbackEntered <- newEmptyMVar
  callerDone <- newEmptyMVar
  let serve = bracket (fst <$> accept listener) close $ \connection -> do
        (claim, stream) <- receiveClientDto connection initialClientStream
        case claim of
          ClaimInitial _ _ -> pure ()
          other -> assertFailure ("expected initial claim during acquisition: " <> show other)
        if duringHandoff
          then do
            sendServerDto connection fixtureOpened
            (barrier, _) <- receiveRawClientDto connection stream
            assertEqual "acquisition waits on same-connection receipt barrier" (RetireReceipts fixtureSession mempty) barrier
          else pure ()
        putMVar acquisitionPending ()
        drainConnection connection
      connectScope = Typed.withHerald (descriptorAt endpoint) (\_ -> putMVar callbackEntered ())
  bracket (forkFinally serve (putMVar serverDone)) killThread $ \_ ->
    bracket (forkFinally connectScope (putMVar callerDone)) killThread $ \caller -> do
      within "connection did not reach the acquisition gate" (readMVar acquisitionPending)
      outcome <- within "interrupted acquisition did not join" (killThread caller >> readMVar callerDone)
      case outcome of
        Left problem -> assertEqual "the original cancellation propagates" (Just ThreadKilled) (fromException problem)
        Right result -> assertFailure ("interrupted connection returned: " <> show result)
      assertEqual "startup cancellation never enters the callback" Nothing =<< tryReadMVar callbackEntered
      assertServerJoined serverDone

casePreparedHandoffUnavailable :: Assertion
casePreparedHandoffUnavailable = bracket openListener close $ \listener -> do
  endpoint <- listenerEndpoint listener
  serverDone <- newEmptyMVar
  _ <-
    forkFinally
      ( bracket (fst <$> accept listener) close $ \connection -> do
          (_, stream) <- receiveClientDto connection initialClientStream
          sendServerDto connection fixtureOpened
          (barrier, _) <- receiveRawClientDto connection stream
          assertEqual "startup has not yet received its receipt acknowledgement" (RetireReceipts fixtureSession mempty) barrier
      )
      (putMVar serverDone)
  let configuration = preparedApplicationConfiguration (descriptorAt endpoint) (checked (Lifecycle.initialClaimId fixtureScope)) fixtureLivenessConfiguration
  result <- within "terminal initial receipt loss left startup blocked" (withApplication configuration (\_ _ -> pure ()))
  assertEqual "startup unavailability is a terminal typed result" (Left (ApplicationSessionUnavailableBeforeStartup SessionNoLongerLive)) result
  assertServerJoined serverDone

-- The changed-policy newtype has the identical representation but a different
-- full SortId, proving that binding checks more than the payload schema.
data PayloadMessage = PayloadMessage {message :: Text}
  deriving stock (Eq, Show, Generic)
instance Payload.ValueType PayloadMessage
instance Payload.ApplicationSort PayloadMessage where
  sortPolicy = Payload.regularPolicy [Payload.key (Payload.field @"message")]
newtype RetainedMessage = RetainedMessage PayloadMessage
  deriving stock (Eq, Show, Generic)
instance Payload.ValueType RetainedMessage
instance Payload.ApplicationSort RetainedMessage where
  sortPolicy = Payload.withRetention 1 (Payload.regularPolicy [Payload.key (Payload.field @"message")])

payloadSort :: Payload.Sort PayloadMessage
payloadSort = checked (Payload.compileSort @PayloadMessage)

payloadStartup :: ApplicationStartupAccess
payloadStartup = Access.applicationStartupAccess (asPrivateProcessId (privateId 1)) (checked (Access.primordialAccessWithSorts entries Set.empty Nothing sorts objectSorts))
  where
    roots = [(role, asPrivateNablaId (privateId writer), asPrivateDeltaId (privateId reader)) | (role, writer, reader) <- zip3 allApplicationPredefinedSortRoles [2 ..] [20 ..]]
    wiring = (Access.environmentHubKey, asPrivateObjectId (privateId 1000)) : zip [Access.environmentEdgeKey role direction | role <- allApplicationPredefinedSortRoles, direction <- Access.allEnvironmentEdgeRoles] [asPrivateObjectId (privateId value) | value <- [1001 .. 1018]]
    objectSorts = Map.fromList [(name, Catalogue.predefinedSortId (if name == Access.environmentHubKey then Catalogue.NeutralVertexRole else Catalogue.EdgeRole)) | (name, _) <- wiring]
    entries =
      fmap (\(name, object) -> (name, Access.Object object)) wiring
        <> concatMap (\(role, writer, reader) -> [(Access.environmentWriterKey role, Access.Writer writer), (Access.environmentReaderKey role, Access.Reader reader)]) roots
        <> [("messages.writer", Access.Writer (asPrivateNablaId (privateId 80))), ("messages.reader", Access.Reader (asPrivateDeltaId (privateId 81)))]
    sorts =
      Map.fromList
        ( concatMap (\(role, _, _) -> [(Access.environmentWriterKey role, rootSort role), (Access.environmentReaderKey role, rootSort role)]) roots
            <> [("messages.writer", Payload.sortId payloadSort), ("messages.reader", Payload.sortId payloadSort)]
        )
    rootSort role = Catalogue.predefinedSortId (toEnum (fromEnum role))

payloadQuery :: Query.ApplicationQuery
payloadQuery = Query.ApplicationQuery (Set.singleton (asPrivateDeltaId (privateId 81))) Query.QueryAlways

caseEnvironmentWiring :: Assertion
caseEnvironmentWiring = withTypedFixtureStartup payloadStartup serve $ \herald -> do
  let environment = checked (Payload.startupEnvironment herald)
      hub = Payload.environmentHub environment
      edges = Payload.environmentEdges environment
  assertEqual "checked hub is an ordinary vertex" (privateId 1000) (Payload.vertexId hub)
  assertEqual "all supporting edges retain their private object names" [asPrivateObjectId (privateId value) | value <- [1001 .. 1018]] (fmap Payload.edgeId edges)
  assertEqual "complete selection retains thirty-one objects" 31 (Map.size (Access.selectionEntries (Payload.environmentSelection environment)))
  assertEqual "hub uses ordinary label operation" (Right LabelApplied) =<< Payload.labelVertex hub fixtureExpectedLabel LabelToVoid
  assertEqual "supporting edge uses ordinary label operation" (Right LabelApplied) =<< Payload.labelEdge (edges !! 0) fixtureExpectedLabel LabelToDelete
  assertEqual "edge forwarding retains this environment's edge writer" (Right ForwardAccepted) =<< Payload.forwardEdge (edges !! 1)
  where
    serve connection stream =
      void
        ( serveCurrentCalls
            connection
            stream
            0
            2
            [ (LabelApplication (asPrivateObjectId (privateId 1000)) fixtureExpectedLabel LabelToVoid, LabelCompleted LabelApplied, Nothing),
              (LabelApplication (asPrivateObjectId (privateId 1001)) fixtureExpectedLabel LabelToDelete, LabelCompleted LabelApplied, Nothing),
              (ForwardApplication (asPrivateNablaId (privateId 4)) (asPrivateObjectId (privateId 1002)), ForwardCompleted ForwardAccepted, Nothing)
            ]
        )

caseEnvironmentWiringEvidence :: Assertion
caseEnvironmentWiringEvidence = do
  let original = Access.startupAccessPrimordial payloadStartup
      hub = Access.environmentHubKey
      replace objectSorts = Access.applicationStartupAccess (Access.startupAccessProcess payloadStartup) (checked (Access.primordialAccessWithSorts (Map.toAscList (Access.accessEntries original)) (Access.accessRequiredEndpoints original) (Access.accessEnvironmentSources original) (Access.accessEndpointSorts original) objectSorts))
      missing = replace (Map.delete hub (Access.accessObjectSorts original))
      wrong = replace (Map.insert hub (Catalogue.predefinedSortId Catalogue.EdgeRole) (Access.accessObjectSorts original))
      check startup expected = withTypedFixtureStartup startup (\_ _ -> pure ()) $ \herald -> case Payload.startupEnvironment herald of
        Left problem -> assertEqual "object-key text is not binding evidence" expected problem
        Right _ -> assertFailure "unverified wiring became a typed environment"
  check missing (Payload.MissingObjectSort hub)
  check wrong (Payload.ObjectSortMismatch (Catalogue.predefinedSortId Catalogue.NeutralVertexRole) (Catalogue.predefinedSortId Catalogue.EdgeRole))

casePayloadBinding :: Assertion
casePayloadBinding = withTypedFixtureStartup payloadStartup (\_ _ -> pure ()) $ \herald -> do
  let writer = checked (Payload.bindNabla @PayloadMessage herald "messages.writer")
      reader = checked (Payload.bindDelta @PayloadMessage herald "messages.reader")
  assertEqual "writer private name" (asPrivateNablaId (privateId 80)) (Payload.nablaId writer)
  assertEqual "reader private name" (asPrivateDeltaId (privateId 81)) (Payload.deltaId reader)
  case Payload.bindNabla @RetainedMessage herald "messages.writer" of
    Left (Payload.EndpointSortMismatch expected actual) -> do
      assertEqual "actual complete sort" (Payload.sortId payloadSort) actual
      assertEqual "expected changed policy" (Payload.sortId (checked (Payload.compileSort @RetainedMessage))) expected
    _ -> assertFailure "same schema with different policy was bound"
  case Payload.bindDelta @PayloadMessage herald "messages.writer" of
    Left (Payload.EndpointRoleMismatch _) -> pure ()
    _ -> assertFailure "writer was bound as reader"
  case Payload.bindDelta @PayloadMessage herald "missing" of
    Left (Payload.MissingEndpoint _) -> pure ()
    _ -> assertFailure "missing reader was bound"

casePayloadCalls :: Assertion
casePayloadCalls = withTypedFixtureStartup payloadStartup serve $ \herald -> do
  let writer = checked (Payload.bindNabla @PayloadMessage herald "messages.writer")
      reader = checked (Payload.bindDelta @PayloadMessage herald "messages.reader")
      query = Payload.query reader Payload.queryAlways
      value = PayloadMessage "hello"
  assertEqual "encode only at the boundary" (Right WriteAccepted) =<< Payload.write writer value
  assertEqual "direct decoding" (Right [value]) =<< Payload.read query
  call <- PayloadAdvanced.submit (PayloadAdvanced.LocalTake query)
  assertEqual "advanced decoding" (Right [value]) =<< PayloadAdvanced.await call
  assertEqual "same completion is repeatable" (PayloadAdvanced.Complete (Right [value])) =<< PayloadAdvanced.status call
  casePayloadMalformed query
  where
    serve connection stream =
      void
        ( serveCurrentCalls
            connection
            stream
            0
            2
            [ (WriteApplication (asPrivateNablaId (privateId 80)) (PublishValue raw), WriteCompleted WriteAccepted, Nothing),
              (ReadApplication payloadQuery, ReadCompleted [raw], Nothing),
              (LocalTakeApplication payloadQuery, LocalTakeCompleted [raw], Nothing),
              (ReadApplication payloadQuery, ReadCompleted [BoolValue True], Nothing)
            ]
        )
    raw = Payload.encodeValue (PayloadMessage "hello")
    casePayloadMalformed query =
      Payload.read query >>= \case
        Left (Payload.InvalidValue _) -> pure ()
        other -> assertFailure ("malformed payload lost its decode error: " <> show other)

casePayloadWaitCancellation :: Assertion
casePayloadWaitCancellation = withTypedFixtureStartup payloadStartup serve $ \herald -> do
  let reader = checked (Payload.bindDelta @PayloadMessage herald "messages.reader")
  call <- PayloadAdvanced.submit (PayloadAdvanced.Wait (Payload.SomeQuery (Payload.query reader Payload.queryAlways) :| []))
  PayloadAdvanced.cancel call
  assertEqual "original wait is cancelled" (Left (Payload.UnderlyingCall Typed.CallCancelled)) =<< PayloadAdvanced.await call
  assertEqual "cancel result is repeatable" (PayloadAdvanced.Complete (Left (Payload.UnderlyingCall Typed.CallCancelled))) =<< PayloadAdvanced.status call
  where
    serve connection stream = do
      (request, afterCall) <- receiveClientDto connection stream
      assertEqual "typed wait lowers once" (Call fixtureSession fixtureRequest (WaitApplication [payloadQuery])) request
      let claim = checked (applicationWaitClaim fixtureScope 1 fixtureRequest)
      sendServerDto connection (RequestRetained fixtureSession fixtureCursor2 fixtureRequest (WaitAccepted claim))
      (cancelled, _) <- receiveClientDto connection afterCall
      assertEqual "same invocation cancellation" (CancelPendingWait fixtureSession fixtureRequest claim) cancelled
      sendServerDto connection (RequestRetained fixtureSession fixtureCursor3 fixtureRequest (Cancelled claim))

casePayloadSessions :: Assertion
casePayloadSessions = withTypedFixtureStartup payloadStartup idle $ \first ->
  withTypedFixtureStartup payloadStartup idle $ \second -> do
    let left = checked (Payload.bindDelta @PayloadMessage first "messages.reader")
        right = checked (Payload.bindDelta @PayloadMessage second "messages.reader")
    case Payload.queryMany (left :| [right]) Payload.queryAlways of
      Left Payload.DifferentSessions -> pure ()
      _ -> assertFailure "same private numbers allowed a cross-session query"
    call <- PayloadAdvanced.submit (PayloadAdvanced.Wait (Payload.SomeQuery (Payload.query left Payload.queryAlways) :| [Payload.SomeQuery (Payload.query right Payload.queryAlways)]))
    assertEqual "heterogeneous waits also enforce session ownership" (Left Payload.DifferentSessions) =<< PayloadAdvanced.await call
  where
    idle _ _ = pure ()

casePayloadDiscovery :: Assertion
casePayloadDiscovery = withTypedFixtureStartup payloadStartup serve $ \herald -> do
  let environment = checked (Payload.startupEnvironment herald)
  discovered <- Payload.discoverDeltas environment payloadSort
  case discovered of
    Right [reader] -> assertEqual "read-localized ID becomes handle" (asPrivateDeltaId (privateId 92)) (Payload.deltaId reader)
    Left problem -> assertFailure (show problem)
    _ -> assertFailure "expected one discovered reader"
  mismatch <- Payload.discoverDeltas environment payloadSort
  case mismatch of
    Left (Payload.EndpointSortMismatch _ _) -> pure ()
    _ -> assertFailure "unexpected sort in read response was bound"
  replicateM_ 2 (Payload.discoverDeltas environment payloadSort >>= malformed)
  writers <- Payload.discoverNablas environment payloadSort
  case writers of
    Right [writer] -> assertEqual "complete nabla shape becomes a handle" (asPrivateNablaId (privateId 92)) (Payload.nablaId writer)
    _ -> assertFailure "expected one discovered writer"
  replicateM_ 2 (Payload.discoverNablas environment payloadSort >>= malformed)
  where
    malformed (Left Payload.UnexpectedValue) = pure ()
    malformed _ = assertFailure "a malformed structural carrier became an endpoint handle"
    fields carried = Map.fromList [("object_id", UniqueIdValue (privateId 92)), ("label", LabelValue fixtureExpectedLabel), ("sort_id", BytesValue (sortIdBytes carried))]
    expected = fields (Payload.sortId payloadSort)
    raw = RecordValue . fields
    queryFor reader =
      Query.ApplicationQuery
        (Set.singleton (asPrivateDeltaId (privateId reader)))
        (Query.QueryCompare (Descriptor.ApplicationProjection ("sort_id" :| [])) Descriptor.ScalarEqual (Query.QueryBytes (sortIdBytes (Payload.sortId payloadSort))))
    serve connection stream =
      void
        ( serveCurrentCalls
            connection
            stream
            0
            2
            [ (ReadApplication (queryFor 24), ReadCompleted [raw (Payload.sortId payloadSort)], Nothing),
              (ReadApplication (queryFor 24), ReadCompleted [raw fixtureSortId], Nothing),
              (ReadApplication (queryFor 24), ReadCompleted [RecordValue (Map.delete "label" expected)], Nothing),
              (ReadApplication (queryFor 24), ReadCompleted [RecordValue (Map.insert "extra" (BoolValue True) expected)], Nothing),
              (ReadApplication (queryFor 23), ReadCompleted [RecordValue (Map.insert "sequencing_object" (OptionalUniqueIdValue Nothing) expected)], Nothing),
              (ReadApplication (queryFor 23), ReadCompleted [RecordValue expected], Nothing),
              (ReadApplication (queryFor 23), ReadCompleted [RecordValue (Map.insert "sequencing_object" (BoolValue True) expected)], Nothing)
            ]
        )

data ControlledPayload = ControlledPayload
  { identifier :: PrivateUniqueId,
    owner :: ApplicationLabel,
    content :: Text
  }
  deriving stock (Eq, Show, Generic)

instance Payload.ValueType ControlledPayload
instance Payload.ApplicationSort ControlledPayload where
  type SortKindOf ControlledPayload = 'Descriptor.ControlledSort
  sortPolicy = Payload.controlledPolicy (Payload.field @"identifier") (Just (Payload.field @"owner"))

controlledPayloadStartup :: ApplicationStartupAccess
controlledPayloadStartup =
  Access.applicationStartupAccess
    (asPrivateProcessId (privateId 1))
    (checked (Access.primordialAccessWithSorts entries Set.empty Nothing sorts objectSorts))
  where
    inherited = Access.startupAccessPrimordial payloadStartup
    objectSorts = Access.accessObjectSorts inherited
    controls = [("control.writer", Access.Writer controlledWriter), ("control.target", Access.Writer controlledTarget), ("control.reader", Access.Reader (asPrivateDeltaId (privateId 84)))]
    entries = Map.toAscList (Access.accessEntries inherited) <> controls
    definition = checked (Payload.compileSort @ControlledPayload)
    sorts = Access.accessEndpointSorts inherited <> Map.fromList [(name, Payload.sortId definition) | (name, _) <- controls]

controlledWriter, controlledTarget :: PrivateNablaId
controlledWriter = asPrivateNablaId (privateId 82)
controlledTarget = asPrivateNablaId (privateId 83)

controlledQuery :: Query.ApplicationQuery
controlledQuery = Query.ApplicationQuery (Set.singleton (asPrivateDeltaId (privateId 84))) Query.QueryAlways

controlledValue :: PrivateUniqueId -> Text -> ControlledPayload
controlledValue object text = ControlledPayload object (ProcessLabel (asPrivateProcessId (privateId 1)), 0) text

caseControlledPayloadCalls :: Assertion
caseControlledPayloadCalls = withTypedFixtureStartup controlledPayloadStartup serve $ \herald -> do
  let writer = checked (Payload.bindNabla @ControlledPayload herald "control.writer")
      target = checked (Payload.bindNabla @ControlledPayload herald "control.target")
      reader = checked (Payload.bindDelta @ControlledPayload herald "control.reader")
      query = Payload.query reader Payload.queryAlways
  reservation <- Payload.reserve writer >>= either (assertFailure . show) pure
  assertEqual "reservation retains the allocated object" (privateId 100) (Payload.reservationId reservation)
  wrongPublication <- Payload.publishReserved reservation (controlledValue (privateId 999) "wrong")
  case wrongPublication of
    Left Payload.ReservationIdentityMismatch -> pure ()
    _ -> assertFailure "a reservation published a different object identity"
  object <- Payload.publishReserved reservation original >>= either (assertFailure . show) pure
  assertEqual "successful publication keeps reservation identity" objectId (Payload.objectId object)
  assertEqual "an update cannot replace the object key" (Left Payload.ReservationIdentityMismatch)
    =<< Payload.update target object (controlledValue (privateId 999) "wrong")
  assertEqual "updates use the caller's typed target writer" (Right WriteAccepted) =<< Payload.update target object updated
  observations <- Payload.readObjects query >>= either (assertFailure . show) pure
  observed <- case observations of
    [(reference, value)] -> do
      assertEqual "read references retain the localized object identity" objectId (Payload.objectId reference)
      assertEqual "read references also retain the decoded payload" current value
      pure reference
    _ -> assertFailure "expected one controlled observation" >> error "unreachable"
  assertEqual "forward uses the selected target, not an originating writer" (Right ForwardAccepted) =<< Payload.forward target observed
  assertEqual "label passes the observed expected owner and generation" (Right LabelApplied) =<< Payload.label observed fixtureExpectedLabel LabelToVoid
  abandoned <- Payload.reserve writer >>= either (assertFailure . show) pure
  assertEqual "cancellation retains the original reservation writer" (Right WriteAccepted) =<< Payload.cancelReservation abandoned
  where
    objectId = asPrivateObjectId (privateId 100)
    original = controlledValue (privateId 100) "original"
    updated = controlledValue (privateId 100) "updated"
    current = updated {owner = fixtureExpectedLabel}
    serve connection stream =
      void
        ( serveCurrentCalls
            connection
            stream
            0
            2
            [ (NewIdApplication (ControlledNewId controlledWriter), NewIdCompleted (privateId 100), Nothing),
              (WriteApplication controlledWriter (PublishValue (Payload.encodeValue original)), WriteCompleted WriteAccepted, Nothing),
              (WriteApplication controlledTarget (PublishValue (Payload.encodeValue updated)), WriteCompleted WriteAccepted, Nothing),
              (ReadApplication controlledQuery, ReadCompleted [Payload.encodeValue current], Nothing),
              (ForwardApplication controlledTarget objectId, ForwardCompleted ForwardAccepted, Just StructuralStabilizationPending),
              (LabelApplication objectId fixtureExpectedLabel LabelToVoid, LabelCompleted LabelApplied, Just LabelSettlementPending),
              (NewIdApplication (ControlledNewId controlledWriter), NewIdCompleted (privateId 101), Nothing),
              (WriteApplication controlledWriter (DeleteReserved (asPrivateObjectId (privateId 101))), WriteCompleted WriteAccepted, Nothing)
            ]
        )

caseControlledPayloadSessions :: Assertion
caseControlledPayloadSessions = withTypedFixtureStartup controlledPayloadStartup serve $ \first -> do
  let reader = checked (Payload.bindDelta @ControlledPayload first "control.reader")
  values <- Payload.readObjects (Payload.query reader Payload.queryAlways) >>= either (assertFailure . show) pure
  object <- case values of
    [(reference, _)] -> pure reference
    _ -> assertFailure "expected one controlled observation" >> error "unreachable"
  withTypedFixtureStartup controlledPayloadStartup (\_ _ -> pure ()) $ \second -> do
    let target = checked (Payload.bindNabla @ControlledPayload second "control.target")
    assertEqual "equal private IDs do not allow cross-session forwarding" (Left Payload.DifferentSessions) =<< Payload.forward target object
    assertEqual "equal private IDs do not allow cross-session updates" (Left Payload.DifferentSessions) =<< Payload.update target object value
    let environment = checked (Payload.startupEnvironment second)
    sequenced <- Payload.createNabla environment payloadSort (Just (Payload.sequenceOn object))
    case sequenced of
      Left Payload.DifferentSessions -> pure ()
      _ -> assertFailure "a sequencing object from another connection reached endpoint creation"
  where
    value = controlledValue (privateId 100) "observed"
    serve connection stream =
      void
        ( serveCurrentCalls
            connection
            stream
            0
            2
            [(ReadApplication controlledQuery, ReadCompleted [Payload.encodeValue value], Nothing)]
        )

casePayloadMutationCancellation :: Assertion
casePayloadMutationCancellation = do
  accepted <- newEmptyMVar
  release <- newEmptyMVar
  withTypedFixtureStartup payloadStartup (serve accepted release) $ \herald -> do
    let writer = checked (Payload.bindNabla @PayloadMessage herald "messages.writer")
    call <- PayloadAdvanced.submit (PayloadAdvanced.Write writer value)
    waiterDone <- newEmptyMVar
    waiter <- forkFinally (PayloadAdvanced.await call) (putMVar waiterDone)
    within "payload mutation acceptance missing" (readMVar accepted)
    killThread waiter
    stopped <- within "cancelled payload mutation waiter did not exit" (readMVar waiterDone)
    case stopped of
      Left problem -> assertEqual "waiter cancellation propagates" (Just ThreadKilled) (fromException problem)
      Right _ -> assertFailure "cancelled waiter unexpectedly completed"
    PayloadAdvanced.cancel call
    assertEqual "the semantic mutation is still pending" PayloadAdvanced.Pending =<< PayloadAdvanced.status call
    putMVar release ()
    assertEqual "the original invocation receives its terminal result" (Right WriteAccepted) =<< PayloadAdvanced.await call
    assertEqual "a repeated await reads the same completed cell" (Right WriteAccepted) =<< PayloadAdvanced.await call
    assertEqual "terminal status preserves the same completion" (PayloadAdvanced.Complete (Right WriteAccepted)) =<< PayloadAdvanced.status call
  where
    value = PayloadMessage "retained write"
    serve accepted release connection stream = do
      (request, afterCall) <- receiveClientDto connection stream
      assertEqual "one typed write invocation" (Call fixtureSession fixtureRequest (WriteApplication (asPrivateNablaId (privateId 80)) (PublishValue (Payload.encodeValue value)))) request
      sendServerDto connection (RequestRetained fixtureSession fixtureCursor2 fixtureRequest (OperationAccepted StructuralStabilizationPending))
      putMVar accepted ()
      _ <- readMVar release
      sendServerDto connection (RequestRetained fixtureSession fixtureCursor3 fixtureRequest (Completed (WriteCompleted WriteAccepted)))
      (next, _) <- receiveClientDto connection afterCall
      assertEqual "await and cancellation do not submit another mutation" (EndSession fixtureSession) next

caseTopologyCreationCalls :: Assertion
caseTopologyCreationCalls = withTypedFixtureStartup payloadStartup serve $ \herald -> do
  let environment = checked (Payload.startupEnvironment herald)
      source = Payload.nablaVertex (checked (Payload.bindNabla @PayloadMessage herald "messages.writer"))
      destination = Payload.deltaVertex (checked (Payload.bindDelta @PayloadMessage herald "messages.reader"))
  creation <- Payload.reserveDelta environment payloadSort >>= either (assertFailure . show) pure
  assertEqual "staged delta retains its reservation" (privateId 100) (Payload.creationId creation)
  rejected <- Payload.publishCreation creation
  case rejected of
    Left (Payload.UnderlyingCall (Typed.CallRejected ApplicationOperateNotPermitted)) -> pure ()
    _ -> assertFailure "the rejected structural write lost its semantic outcome"
  assertEqual "a definite rejection leaves the original reservation cancellable" (Right WriteAccepted) =<< Payload.cancelCreation creation
  reservationCall <- PayloadAdvanced.submit (PayloadAdvanced.ReserveEdgeFor environment Payload.Preserve source destination)
  edgeCreation <- PayloadAdvanced.await reservationCall >>= either (assertFailure . show) pure
  repeated <- PayloadAdvanced.await reservationCall >>= either (assertFailure . show) pure
  assertEqual "reservation await keeps the allocated identity" (Payload.creationId edgeCreation) (Payload.creationId repeated)
  publicationCall <- PayloadAdvanced.submit (PayloadAdvanced.PublishCreation edgeCreation)
  first <- PayloadAdvanced.await publicationCall >>= either (assertFailure . show) pure
  second <- PayloadAdvanced.await publicationCall >>= either (assertFailure . show) pure
  assertEqual "publication returns the reserved edge" (asPrivateObjectId (privateId 101)) (Payload.edgeId first)
  assertEqual "publication await reads the same successful result" (Payload.edgeId first) (Payload.edgeId second)
  PayloadAdvanced.status publicationCall >>= \case
    PayloadAdvanced.Complete (Right edge) -> assertEqual "publication status uses the same result" (Payload.edgeId first) (Payload.edgeId edge)
    _ -> assertFailure "successful staged publication did not retain completion"
  where
    deltaWriter = asPrivateNablaId (privateId 6)
    edgeWriter = asPrivateNablaId (privateId 4)
    initialLabel = LabelValue (ProcessLabel (asPrivateProcessId (privateId 1)), 0)
    deltaValue = RecordValue (Map.fromList [("object_id", UniqueIdValue (privateId 100)), ("label", initialLabel), ("sort_id", BytesValue (sortIdBytes (Payload.sortId payloadSort)))])
    edgeValue = RecordValue (Map.fromList [("object_id", UniqueIdValue (privateId 101)), ("label", initialLabel), ("source_vertex", UniqueIdValue (privateId 80)), ("destination_vertex", UniqueIdValue (privateId 81)), ("strength", EnumValue "preserve")])
    serve connection stream = do
      afterReserve <- serveCurrentCalls connection stream 0 2 [(NewIdApplication (ControlledNewId deltaWriter), NewIdCompleted (privateId 100), Nothing)]
      (publication, afterPublication) <- receiveClientDto connection afterReserve
      assertEqual "staged publication uses the retained carrier and writer" (Call fixtureSession (applicationRequestIdClaim 1) (WriteApplication deltaWriter (PublishValue deltaValue))) publication
      sendServerDto connection (RequestRetained fixtureSession fixtureCursor3 (applicationRequestIdClaim 1) (Rejected ApplicationOperateNotPermitted))
      afterCalls <-
        serveCurrentCalls
          connection
          afterPublication
          2
          4
          [ (WriteApplication deltaWriter (DeleteReserved (asPrivateObjectId (privateId 100))), WriteCompleted WriteAccepted, Nothing),
            (NewIdApplication (ControlledNewId edgeWriter), NewIdCompleted (privateId 101), Nothing),
            (WriteApplication edgeWriter (PublishValue edgeValue), WriteCompleted WriteAccepted, Just StructuralStabilizationPending)
          ]
      (next, _) <- receiveClientDto connection afterCalls
      assertEqual "staged await and status never resubmit publication" (EndSession fixtureSession) next
