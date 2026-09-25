{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ApplicationTcpProperties
  ( tests,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    putMVar,
    readMVar,
    tryPutMVar,
  )
import Control.Concurrent.STM
  ( atomically,
    modifyTVar',
    newTQueueIO,
    newTVarIO,
    readTQueue,
    readTVar,
    writeTQueue,
    writeTVar,
  )
import Control.Exception (finally)
import Control.Monad (forM_, void)
import Data.ByteString qualified as ByteString
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Eclips.Application.Runtime
  ( ApplicationCallCompletion (..),
    applicationConfiguration,
    applicationEndpoint,
    applicationLivenessConfigurationMicroseconds,
    awaitApplicationCall,
    newid,
    withApplication,
    write,
  )
import Eclips.Application.Types.Access
  ( ApplicationPredefinedSortRole (NeutralVertexRole, SortDefinitionRole),
    ApplicationStartupAccess,
    PredefinedAccess,
    predefinedWriter,
  )
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity (asPrivateObjectId)
import Eclips.Application.Types.NewId (NewIdTarget (..))
import Eclips.Application.Types.Operation (ApplicationOperation (NewIdApplication))
import Eclips.Application.Types.Result
  ( RegularCallResult (NewIdCompleted, WriteCompleted),
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (RecordSchema, TextSchema),
  )
import Eclips.Application.Types.Value (ApplicationValue (SortDefinitionValue))
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (DeleteReserved, PublishValue),
    WriteResult (SortDefinitionWritten, WriteAccepted),
  )
import Eclips.Herald.Administration (adminCorrelationId)
import Eclips.Herald.Application.RPC
  ( ApplicationIngressContext (CandidateApplicationIngress),
    ApplicationOutbound (..),
    admitApplicationClientDto,
    projectApplicationEffect,
  )
import Eclips.Herald.Application.Session
  ( ApplicationSessionUnavailableReason (ApplicationSessionNoLongerLive),
    sessionAcceptanceBinding,
    sessionBindingSessionId,
  )
import Eclips.Herald.EffectBatch
  ( HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input (candidateApplicationLane, heraldInput)
import Eclips.Herald.Runtime
  ( checkApplicationRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
    heraldRuntimeConfiguration,
    runtimeGeneratorSeedSource,
    runtimePeerWorkDelayMicroseconds,
    systemRuntimeMonotonicClock,
  )
import Eclips.Herald.Runtime.Handler (heraldRuntimeHandlers)
import Eclips.Herald.Runtime.Ingress (RuntimeSubmission (Queued))
import Eclips.Herald.Runtime.Internal.Coordination
  ( BatchEnvelope,
    batchEnvelopeEffects,
  )
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    defaultRuntimeHooks,
    withHeraldRuntimeWithHooks,
  )
import Eclips.Herald.Runtime.TCP
  ( HeraldTcp,
    HeraldTcpConfiguration,
    HeraldTcpFailure (HeraldTcpRuntimeFailure),
    ResolvedTcpEndpoint,
    awaitHeraldTcpExit,
    configureHeraldTcpApplicationHeartbeat,
    configureHeraldTcpTiming,
    heartbeatConfigurationMicroseconds,
    heartbeatIdleMicroseconds,
    heartbeatReplyTimeoutMicroseconds,
    heraldTcpApplicationEndpoint,
    heraldTcpConfiguration,
    heraldTcpEndpoints,
    peerDialRetryDelayMicroseconds,
    requestHeraldTcpDrain,
    resolvedTcpHost,
    resolvedTcpPort,
    tcpListenEndpoint,
    withHeraldTcpRuntime,
  )
import Eclips.Herald.Runtime.TCP.Internal.Application
  ( ApplicationSocketPhase (..),
    applicationHeartbeatPhase,
    applicationOutboundDto,
    claimApplicationWrite,
    closeApplicationSocketUnlessTerminating,
    sendApplicationPong,
  )
import Eclips.Herald.Runtime.TCP.Internal.Facade
  ( HeraldRuntimeScope (HeraldRuntimeScope),
    withHeraldTcpRuntimeInScopeInternal,
  )
import Eclips.Herald.Runtime.TCP.Internal.Heartbeat
  ( HeartbeatPhase (HeartbeatEstablished, HeartbeatQuiescing),
    setHeartbeatSchedulerForTest,
  )
import Eclips.Herald.Runtime.TCP.Internal.Socket
  ( connectTcpEndpoint,
    newTcpSocketWriter,
    sendTcpSocketBytes,
    sendTcpSocketBytesWith,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( HeartbeatScheduler (..),
  )
import Eclips.Herald.Runtime.TCP.Internal.Types qualified as TcpInternal
import Eclips.Herald.Runtime.Trace
  ( HeraldRuntimeExit (..),
    HeraldRuntimeFailureClass (HeraldRuntimeInitializationFailure),
    heraldRuntimeFailureClass,
  )
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Transition (stepHerald)
import Eclips.Protocol.Application.Frame
  ( ApplicationFrameContinuation (..),
    ApplicationFrameDirection (ApplicationServerFrames),
    ApplicationFrameFeedResult (..),
    encodeApplicationFrame,
    feedApplicationFrame,
    initialApplicationFrameDecoder,
  )
import Eclips.Protocol.Application.Types
  ( ApplicationClientDto (..),
    ApplicationEnvelope (..),
    ApplicationRequestReplyBodyDto (Completed),
    ApplicationServerDto (..),
    ApplicationSessionErrorDto (ApplicationAttachmentNotAdmittedDto),
    ApplicationSessionUnavailableReasonDto (ApplicationSessionNoLongerLiveDto),
    applicationHeartbeatNonce,
    applicationRequestIdClaim,
  )
import Eclips.Public.Types.Timing qualified as Timing
import Network.Socket
  ( Family (AF_UNIX),
    Socket,
    SocketType (Stream),
    close,
    defaultProtocol,
    socketPair,
  )
import Network.Socket.ByteString qualified as SocketBytes
import RuntimeFixtures
  ( fixtureCheckedBootstraps,
    fixtureCheckedGenesis,
    fixtureGeneratorSeed,
    fixtureGeneratorSeedSource,
    fixtureOpenDto,
    fixtureOracleContacts,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "application TCP"
    [ QC.testProperty "application response timing is independent of peer timing and separately overridable" propApplicationTiming,
      testCase "seed failure precedes the TCP listener and callback" caseSeedFailurePrecedesTcp,
      testCase "terminal EAPP output claims one final write and closes the readable phase" caseTerminalOutputPhase,
      testCase "a terminal claim defeats an already-decided heartbeat close" caseTerminalFencesDecidedHeartbeatClose,
      testCase "pending initial claims have physical heartbeat without an application session" casePendingClaimHeartbeat,
      testCase "a crossing Ping cannot write a Pong after the terminal frame claim" caseTerminalFencesCrossingPing,
      testCase "a confirmed EAPP session dies immediately on TCP loss and cannot resurrect" caseApplicationCrash,
      testCase "server-side EAPP heartbeat is established-only and physically consumed" caseApplicationHeartbeat,
      testCase "application response override reaches establishment and established heartbeat" caseApplicationReplyOverride,
      testCase "real EAPP open, newid, reservation delete, sort write, and semantic drain cross the runtime seams" caseApplicationOpen
    ]

caseApplicationCrash :: Assertion
caseApplicationCrash = do
  sessionDisposed <- newEmptyMVar
  let listenEndpoint = checked (tcpListenEndpoint "127.0.0.1" 0)
      hooks =
        defaultRuntimeHooks
          { hookAfterDispatchBatch =
              observeApplicationCrashBatches sessionDisposed
          }
      runtimeConfiguration =
        heraldRuntimeConfiguration
          fixtureCheckedGenesis
          fixtureCheckedBootstraps
          fixtureOracleContacts
          fixtureGeneratorSeedSource
          systemRuntimeMonotonicClock
          (runtimePeerWorkDelayMicroseconds 0)
          (heraldRuntimeHandlers (const (pure ())))
          (checked (checkApplicationRecoveryConfiguration 30_000_000))
          (checked (checkPeerRecoveryConfiguration 30_000_000))
      tcpConfiguration =
        heraldTcpConfiguration
          runtimeConfiguration
          listenEndpoint
          listenEndpoint
          listenEndpoint
          []
          (checked (peerDialRetryDelayMicroseconds 1000))
          (checked (heartbeatConfigurationMicroseconds 60_000_000 5_000_000))
  observed <-
    timeout 10_000_000
      $ withHookedHeraldTcp hooks tcpConfiguration
      $ \tcp -> do
        let endpoint = heraldTcpApplicationEndpoint (heraldTcpEndpoints tcp)
        crashed <- connectTcpEndpoint endpoint
        SocketBytes.sendAll
          crashed
          (encodeApplicationFrame (ApplicationClientEnvelope fixtureOpenDto))
        openedClaim <-
          receiveApplicationServerEnvelope crashed >>= \case
            ApplicationServerEnvelope (SessionOpened _ session _ _) -> pure session
            other -> assertFailure ("unexpected crash-fixture Open response: " <> show other)
        let request = applicationRequestIdClaim 0
        SocketBytes.sendAll crashed (encodeApplicationFrame (ApplicationClientEnvelope (Call openedClaim request (NewIdApplication BareNewId))))
        confirmed <- receiveApplicationServerEnvelope crashed
        case confirmed of
          ApplicationServerEnvelope (RequestRetained actualSession _ actualRequest (Completed NewIdCompleted {})) -> do
            assertEqual "the ordinary request confirms the exact opened session" openedClaim actualSession
            assertEqual "the confirming request completed" request actualRequest
          other -> assertFailure ("unexpected confirming request reply: " <> show other)
        close crashed

        lossEffects <-
          awaitMVarWithin
            "the confirmed crashed session did not die without its 30-second opening grace"
            3_000_000
            sessionDisposed
        assertEqual "established loss arms no recovery timer" [] [() | ArmTimer {} <- lossEffects]
        terminalEffect <- case [effect | effect@DisposeApplicationSession {} <- lossEffects] of
          [one] -> pure one
          other -> assertFailure ("expected one exact session disposal, got " <> show other)
        case projectApplicationEffect terminalEffect of
          Right
            ( Just
                ( DisposeEstablishedApplicationSession
                    _
                    ( HeraldPermanentlyUnavailable
                        terminalClaim
                        ApplicationSessionNoLongerLiveDto
                      )
                  )
              ) ->
              assertEqual
                "terminal disposal names the exact crashed session"
                openedClaim
                terminalClaim
          other -> assertFailure ("unexpected terminal application projection: " <> show other)

        retry <- connectTcpEndpoint endpoint
        SocketBytes.sendAll
          retry
          (encodeApplicationFrame (ApplicationClientEnvelope fixtureOpenDto))
        rejected <- receiveApplicationServerEnvelope retry
        assertEqual
          "the sealed process rejects resurrection immediately after established loss"
          (ApplicationServerEnvelope (SessionRejected ApplicationAttachmentNotAdmittedDto))
          rejected
        eof <- timeout 1_000_000 (SocketBytes.recv retry 1)
        assertEqual "the rejected replacement lane closes" (Just ByteString.empty) eof
        close retry

        assertEqual "the permanent-loss schedule still drains cleanly" Queued
          =<< requestHeraldTcpDrain tcp (adminCorrelationId 402)
        assertEqual "the crash-test socket joins the drained scope" (Right HeraldRuntimeDrained)
          =<< awaitHeraldTcpExit tcp
  case observed of
    Nothing -> assertFailure "application crash lifetime schedule exceeded its bound"
    Just (Left failure) -> assertFailure (show failure)
    Just (Right ((), HeraldRuntimeDrained)) -> pure ()
    Just (Right ((), unexpectedExit)) ->
      assertFailure ("unexpected crash-test runtime exit: " <> show unexpectedExit)

observeApplicationCrashBatches ::
  MVar [HeraldEffect] ->
  BatchEnvelope ->
  IO ()
observeApplicationCrashBatches sessionDisposed batch =
  forM_ effects $ \effect -> case effect of
    DisposeApplicationSession {} -> void (tryPutMVar sessionDisposed effects)
    _ -> pure ()
  where
    effects = effectBatchMembers (batchEnvelopeEffects batch)

awaitMVarWithin :: String -> Int -> MVar value -> IO value
awaitMVarWithin message microseconds cell = do
  observed <- timeout microseconds (readMVar cell)
  case observed of
    Nothing -> assertFailure message
    Just value -> pure value

withHookedHeraldTcp ::
  RuntimeHooks ->
  HeraldTcpConfiguration ->
  (HeraldTcp -> IO result) ->
  IO (Either HeraldTcpFailure (result, HeraldRuntimeExit))
withHookedHeraldTcp hooks =
  withHeraldTcpRuntimeInScopeInternal
    (HeraldRuntimeScope (withHeraldRuntimeWithHooks hooks))

caseTerminalOutputPhase :: Assertion
caseTerminalOutputPhase = do
  phase <- newTVarIO EstablishedApplicationSocket
  let outbound = fixtureTerminalOutbound
  (firstClaim, observedAfterFirst, heartbeatAfterFirst, secondClaim) <-
    atomically $ do
      first <- claimApplicationWrite phase outbound
      afterFirst <- readTVar phase
      heartbeatPhase <- applicationHeartbeatPhase phase
      second <- claimApplicationWrite phase outbound
      pure (first, afterFirst, heartbeatPhase, second)
  assertEqual "the established route owns the terminal write" True firstClaim
  assertEqual "the claim quiesces reader admission" TerminatingApplicationSocket observedAfterFirst
  assertEqual "the claim quiesces every heartbeat deadline" HeartbeatQuiescing heartbeatAfterFirst
  assertEqual "a repeated terminal output cannot write twice" False secondClaim
  case applicationOutboundDto outbound of
    HeraldPermanentlyUnavailable _ ApplicationSessionNoLongerLiveDto -> pure ()
    other -> assertFailure ("terminal outbound projected the wrong DTO: " <> show other)

caseTerminalFencesDecidedHeartbeatClose :: Assertion
caseTerminalFencesDecidedHeartbeatClose = do
  phase <- newTVarIO EstablishedApplicationSocket
  physicalCloses <- newTVarIO (0 :: Int)
  terminalClaimed <- atomically (claimApplicationWrite phase fixtureTerminalOutbound)
  closeApplicationSocketUnlessTerminating
    phase
    (atomically (modifyTVar' physicalCloses (+ 1)))
  assertEqual "the terminal output claims the live socket" True terminalClaimed
  assertEqual "the delayed timeout cannot change the terminal phase" TerminatingApplicationSocket
    =<< atomically (readTVar phase)
  assertEqual "the delayed timeout cannot close the physical socket" 0
    =<< atomically (readTVar physicalCloses)

  timeoutFirstPhase <- newTVarIO EstablishedApplicationSocket
  timeoutFirstCloses <- newTVarIO (0 :: Int)
  closeApplicationSocketUnlessTerminating
    timeoutFirstPhase
    (atomically (modifyTVar' timeoutFirstCloses (+ 1)))
  lateTerminalClaim <- atomically (claimApplicationWrite timeoutFirstPhase fixtureTerminalOutbound)
  assertEqual "a timeout that owns Closing rejects the later terminal claim" False lateTerminalClaim
  assertEqual "the timeout-first schedule closes exactly once" 1
    =<< atomically (readTVar timeoutFirstCloses)

caseTerminalFencesCrossingPing :: Assertion
caseTerminalFencesCrossingPing = do
  (socketHandle, peer) <- socketPair AF_UNIX Stream defaultProtocol
  runSchedule socketHandle peer
    `finally` (close socketHandle >> close peer)
  where
    runSchedule socketHandle peer = do
      phase <- newTVarIO EstablishedApplicationSocket
      writer <- newTcpSocketWriter
      writerHeld <- newEmptyMVar
      releaseWriter <- newEmptyMVar
      blockerDone <- newEmptyMVar
      pongStarted <- newEmptyMVar
      pongDone <- newEmptyMVar
      _ <-
        forkIO $ do
          sendTcpSocketBytesWith
            (\_ _ -> putMVar writerHeld () >> readMVar releaseWriter)
            writer
            socketHandle
            ByteString.empty
          putMVar blockerDone ()
      void (awaitMVarWithin "the physical application writer was not held" 2_000_000 writerHeld)
      _ <-
        forkIO $ do
          putMVar pongStarted ()
          sent <-
            sendApplicationPong
              phase
              writer
              (pure ())
              socketHandle
              (Pong (applicationHeartbeatNonce 0x0102030405060708))
          putMVar pongDone sent
      void (awaitMVarWithin "the crossing Pong did not start" 2_000_000 pongStarted)
      terminalClaimed <- atomically (claimApplicationWrite phase fixtureTerminalOutbound)
      putMVar releaseWriter ()
      assertEqual "the terminal output wins the established phase" True terminalClaimed
      void (awaitMVarWithin "the writer blocker did not finish" 2_000_000 blockerDone)
      assertEqual
        "the Pong rechecks the terminal phase while owning the writer"
        False
        =<< awaitMVarWithin "the crossing Pong did not finish" 2_000_000 pongDone
      let terminalBytes =
            encodeApplicationFrame
              (ApplicationServerEnvelope (applicationOutboundDto fixtureTerminalOutbound))
      sendTcpSocketBytes writer socketHandle terminalBytes
      received <- timeout 2_000_000 (SocketBytes.recv peer 32768)
      assertEqual "the peer observes the terminal frame with no preceding Pong" (Just terminalBytes) received

fixtureTerminalOutbound :: ApplicationOutbound
fixtureTerminalOutbound =
  case initialHerald
    (monotonicInstant 100)
    fixtureCheckedGenesis
    fixtureCheckedBootstraps
    fixtureOracleContacts
    fixtureGeneratorSeed
    (checked (checkApplicationRecoveryConfiguration 30_000_000))
    (checked (checkPeerRecoveryConfiguration 30_000_000)) of
    Left fault -> error ("terminal fixture initialization failed: " <> show fault)
    Right (initialState, _) ->
      let body = case admitApplicationClientDto
            (CandidateApplicationIngress (candidateApplicationLane 900))
            fixtureOpenDto of
            Left problem -> error ("terminal fixture Open admission failed: " <> show problem)
            Right admitted -> admitted
       in case stepHerald (heraldInput (monotonicInstant 101) body) initialState of
            Left fault -> error ("terminal fixture Open failed: " <> show fault)
            Right (_, effects) -> case [ acceptance
                                       | SetApplicationConnectionDisposition _ acceptance <- effectBatchMembers effects
                                       ] of
              [acceptance] ->
                let session =
                      sessionBindingSessionId (sessionAcceptanceBinding acceptance)
                 in case projectApplicationEffect
                      (DisposeApplicationSession session ApplicationSessionNoLongerLive) of
                      Right (Just outbound@DisposeEstablishedApplicationSession {}) -> outbound
                      other -> error ("terminal fixture projection failed: " <> show other)
              unexpected -> error ("terminal fixture Open effects were not singular: " <> show unexpected)

caseSeedFailurePrecedesTcp :: Assertion
caseSeedFailurePrecedesTcp = do
  callbackCalled <- newIORef False
  let listenEndpoint = checked (tcpListenEndpoint "127.0.0.1" 0)
      runtimeConfiguration =
        heraldRuntimeConfiguration
          fixtureCheckedGenesis
          fixtureCheckedBootstraps
          fixtureOracleContacts
          (runtimeGeneratorSeedSource (ioError (userError "intentional TCP seed failure")))
          systemRuntimeMonotonicClock
          (runtimePeerWorkDelayMicroseconds 0)
          (heraldRuntimeHandlers (const (pure ())))
          (checked (checkApplicationRecoveryConfiguration 30_000_000))
          (checked (checkPeerRecoveryConfiguration 30_000_000))
      tcpConfiguration =
        heraldTcpConfiguration
          runtimeConfiguration
          listenEndpoint
          listenEndpoint
          listenEndpoint
          []
          (checked (peerDialRetryDelayMicroseconds 1000))
          (checked (heartbeatConfigurationMicroseconds 60_000_000 5_000_000))
  result <- withHeraldTcpRuntime tcpConfiguration (\_ -> writeIORef callbackCalled True)
  case result of
    Left (HeraldTcpRuntimeFailure failure) ->
      assertEqual
        "the ordinary source exception stays a runtime initialization failure"
        HeraldRuntimeInitializationFailure
        (heraldRuntimeFailureClass failure)
    other -> assertFailure ("seed failure unexpectedly entered the TCP scope: " <> show other)
  assertEqual "the TCP callback is not entered" False =<< readIORef callbackCalled

fixtureHeartbeatTcpConfiguration :: HeraldTcpConfiguration
fixtureHeartbeatTcpConfiguration =
  let listenEndpoint = checked (tcpListenEndpoint "127.0.0.1" 0)
      runtimeConfiguration =
        heraldRuntimeConfiguration
          fixtureCheckedGenesis
          fixtureCheckedBootstraps
          fixtureOracleContacts
          fixtureGeneratorSeedSource
          systemRuntimeMonotonicClock
          (runtimePeerWorkDelayMicroseconds 0)
          (heraldRuntimeHandlers (const (pure ())))
          (checked (checkApplicationRecoveryConfiguration 30_000_000))
          (checked (checkPeerRecoveryConfiguration 30_000_000))
   in heraldTcpConfiguration
        runtimeConfiguration
        listenEndpoint
        listenEndpoint
        listenEndpoint
        []
        (checked (peerDialRetryDelayMicroseconds 1000))
        (checked (heartbeatConfigurationMicroseconds 60_000_000 5_000_000))

propApplicationTiming :: QC.Property
propApplicationTiming = QC.forAll (QC.choose (500, 60_000_000)) $ \microseconds ->
  let target = checked (Timing.takeoverTarget microseconds)
      resolved = configureHeraldTcpTiming target fixtureHeartbeatTcpConfiguration
      TcpInternal.HeraldTcpConfiguration _ _ _ _ _ _ application peer _ _ = resolved
      explicit = checked (heartbeatConfigurationMicroseconds 13 17)
      TcpInternal.HeraldTcpConfiguration _ _ _ _ _ _ overridden retainedPeer _ _ = configureHeraldTcpApplicationHeartbeat explicit resolved
   in QC.conjoin
        [ heartbeatReplyTimeoutMicroseconds application QC.=== Timing.defaultApplicationReplyTimeoutMicroseconds,
          heartbeatReplyTimeoutMicroseconds peer QC.=== Timing.timingHeartbeatReplyMicroseconds (Timing.deriveTimingPolicy target),
          heartbeatIdleMicroseconds application QC.=== Timing.timingHeartbeatIdleMicroseconds (Timing.deriveTimingPolicy target),
          overridden QC.=== explicit,
          retainedPeer QC.=== peer
        ]

caseApplicationHeartbeat :: Assertion
caseApplicationHeartbeat = runApplicationHeartbeatCase fixtureHeartbeatTcpConfiguration 5_000_000

caseApplicationReplyOverride :: Assertion
caseApplicationReplyOverride =
  runApplicationHeartbeatCase
    ( configureHeraldTcpApplicationHeartbeat
        (checked (heartbeatConfigurationMicroseconds 60_000_000 Timing.defaultApplicationReplyTimeoutMicroseconds))
        fixtureHeartbeatTcpConfiguration
    )
    Timing.defaultApplicationReplyTimeoutMicroseconds

runApplicationHeartbeatCase :: HeraldTcpConfiguration -> Word64 -> Assertion
runApplicationHeartbeatCase tcpConfiguration expectedReply = do
  let nonce = applicationHeartbeatNonce 0x0102030405060708
  observed <-
    timeout 5000000
      $ withHeraldTcpRuntime tcpConfiguration
      $ \tcp -> do
        let endpoint = heraldTcpApplicationEndpoint (heraldTcpEndpoints tcp)
        scheduled <- newTQueueIO
        setHeartbeatSchedulerForTest tcp
          $ HeartbeatScheduler
          $ \microseconds -> do
            elapsed <- newTVarIO False
            atomically (writeTQueue scheduled (microseconds, elapsed))
            pure elapsed
        candidate <- connectTcpEndpoint endpoint
        (candidateEstablishmentDelay, _) <- atomically (readTQueue scheduled)
        assertEqual "candidate heartbeat uses the reply deadline" expectedReply candidateEstablishmentDelay
        SocketBytes.sendAll
          candidate
          (encodeApplicationFrame (ApplicationClientEnvelope (Ping nonce)))
        candidateClosed <- timeout 1000000 (SocketBytes.recv candidate 1)
        assertEqual "candidate-phase Ping closes without Pong" (Just ByteString.empty) candidateClosed
        close candidate

        established <- connectTcpEndpoint endpoint
        (establishmentDelay, _) <- atomically (readTQueue scheduled)
        assertEqual "candidate establishment uses the reply deadline" expectedReply establishmentDelay
        SocketBytes.sendAll
          established
          (encodeApplicationFrame (ApplicationClientEnvelope fixtureOpenDto))
        opened <- receiveApplicationServerEnvelope established
        case opened of
          ApplicationServerEnvelope SessionOpened {} -> pure ()
          other -> assertFailure ("unexpected open response before heartbeat: " <> show other)
        (idleDelay, idleElapsed) <- atomically (readTQueue scheduled)
        assertEqual "established EAPP heartbeat uses the idle interval" 60_000_000 idleDelay
        atomically (writeTVar idleElapsed True)
        (replyDelay, staleReplyElapsed) <- atomically (readTQueue scheduled)
        assertEqual "passive EAPP heartbeat uses the reply interval" expectedReply replyDelay

        let pingFrame = encodeApplicationFrame (ApplicationClientEnvelope (Ping nonce))
        SocketBytes.sendAll established (ByteString.take 1 pingFrame)
        (replacementIdleDelay, _) <- atomically (readTQueue scheduled)
        assertEqual
          "a nonempty established fragment is immediate liveness progress"
          60_000_000
          replacementIdleDelay
        atomically (writeTVar staleReplyElapsed True)
        SocketBytes.sendAll established (ByteString.drop 1 pingFrame)
        pong <- receiveApplicationServerEnvelope established
        assertEqual
          "the physical owner echoes the exact nonce"
          (ApplicationServerEnvelope (Pong nonce))
          pong
        close established

        assertEqual "heartbeat traffic leaves semantic drain available" Queued
          =<< requestHeraldTcpDrain tcp (adminCorrelationId 400)
        assertEqual "the heartbeat sockets join the drained scope" (Right HeraldRuntimeDrained)
          =<< awaitHeraldTcpExit tcp
  case observed of
    Nothing -> assertFailure "application heartbeat scenario timed out"
    Just (Left failure) -> assertFailure (show failure)
    Just (Right ((), HeraldRuntimeDrained)) -> pure ()
    Just (Right ((), unexpectedExit)) ->
      assertFailure ("unexpected heartbeat runtime exit: " <> show unexpectedExit)

caseApplicationOpen :: Assertion
caseApplicationOpen = do
  let listenEndpoint = checked (tcpListenEndpoint "127.0.0.1" 0)
      runtimeConfiguration =
        heraldRuntimeConfiguration
          fixtureCheckedGenesis
          fixtureCheckedBootstraps
          fixtureOracleContacts
          fixtureGeneratorSeedSource
          systemRuntimeMonotonicClock
          (runtimePeerWorkDelayMicroseconds 0)
          (heraldRuntimeHandlers (const (pure ())))
          (checked (checkApplicationRecoveryConfiguration 30_000_000))
          (checked (checkPeerRecoveryConfiguration 30_000_000))
      tcpConfiguration =
        heraldTcpConfiguration
          runtimeConfiguration
          listenEndpoint
          listenEndpoint
          listenEndpoint
          []
          (checked (peerDialRetryDelayMicroseconds 1000))
          (checked (heartbeatConfigurationMicroseconds 60_000_000 5_000_000))
  observed <-
    timeout 5000000
      $ withHeraldTcpRuntime tcpConfiguration
      $ \tcp -> do
        let resolved = heraldTcpApplicationEndpoint (heraldTcpEndpoints tcp)
            endpoint =
              checked
                ( applicationEndpoint
                    (resolvedTcpHost resolved)
                    (resolvedTcpPort resolved)
                )
            (attachment, nonce) = case fixtureOpenDto of
              OpenSession checkedAttachment checkedNonce -> (checkedAttachment, checkedNonce)
              _ -> error "fixture open DTO is not OpenSession"
            configuration =
              applicationConfiguration
                endpoint
                attachment
                nonce
                (checked (applicationLivenessConfigurationMicroseconds 1_000 2_000_000 500_000 500_000))
        sendTerminalFrame resolved (ByteString.pack [0, 0, 0, 4, 0x45, 0x50, 0x52, 0x50])
        sendTerminalFrame resolved (ByteString.pack [0, 0, 0, 3])
        applicationResult <- withApplication configuration $ \application startup -> do
          sortAccess <- predefinedAccessFor SortDefinitionRole startup
          controlledAccess <- predefinedAccessFor NeutralVertexRole startup
          bare <- newid application BareNewId >>= awaitApplicationCall
          case bare of
            ApplicationCallSucceeded NewIdCompleted {} -> pure ()
            other -> assertFailure ("unexpected bare newid completion: " <> show other)
          controlled <-
            newid application (ControlledNewId (predefinedWriter controlledAccess))
              >>= awaitApplicationCall
          controlledObject <- case controlled of
            ApplicationCallSucceeded (NewIdCompleted identifier) -> pure (asPrivateObjectId identifier)
            other -> assertFailure ("unexpected controlled newid completion: " <> show other)
          deleted <-
            write application (predefinedWriter controlledAccess) (DeleteReserved controlledObject)
              >>= awaitApplicationCall
          case deleted of
            ApplicationCallSucceeded (WriteCompleted WriteAccepted) -> pure ()
            other -> assertFailure ("unexpected reservation delete completion: " <> show other)
          call <-
            write
              application
              (predefinedWriter sortAccess)
              ( PublishValue
                  (SortDefinitionValue (DeclaredSortDefinition declaredDescriptor Nothing))
              )
          completion <- awaitApplicationCall call
          case completion of
            ApplicationCallSucceeded (WriteCompleted (SortDefinitionWritten _)) -> pure ()
            other -> assertFailure ("unexpected application call completion: " <> show other)
        case applicationResult of
          Left failure -> assertFailure (show failure)
          Right () -> pure ()
        assertEqual "the real EAPP acceptance submits semantic drain" Queued
          =<< requestHeraldTcpDrain tcp (adminCorrelationId 401)
        assertEqual "the real EAPP acceptance joins the drained TCP/runtime scope" (Right HeraldRuntimeDrained)
          =<< awaitHeraldTcpExit tcp
  case observed of
    Nothing -> assertFailure "application/TCP scope did not terminate"
    Just (Left failure) -> assertFailure (show failure)
    Just (Right ((), HeraldRuntimeDrained)) -> pure ()
    Just (Right ((), unexpectedExit)) ->
      assertFailure ("unexpected runtime exit: " <> show unexpectedExit)

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id

predefinedAccessFor :: ApplicationPredefinedSortRole -> ApplicationStartupAccess -> IO PredefinedAccess
predefinedAccessFor role startup =
  case (Map.lookup (Access.environmentWriterKey role) (Access.accessEntries (Access.startupAccessPrimordial startup)), Map.lookup (Access.environmentReaderKey role) (Access.accessEntries (Access.startupAccessPrimordial startup))) of
    (Just (Access.Writer writer), Just (Access.Reader reader)) -> pure (Access.predefinedAccess role writer reader)
    unexpected -> assertFailure ("unexpected predefined access set for " <> show role <> ": " <> show unexpected)

declaredDescriptor :: ApplicationSortDescriptor
declaredDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "key" TextSchema),
      keyProjections = [ApplicationProjection ("key" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

sendTerminalFrame :: ResolvedTcpEndpoint -> ByteString.ByteString -> IO ()
sendTerminalFrame resolved bytes = do
  connection <- connectTcpEndpoint resolved
  SocketBytes.sendAll connection bytes
  close connection

receiveApplicationServerEnvelope :: Socket -> IO ApplicationEnvelope
receiveApplicationServerEnvelope socketHandle =
  loop (initialApplicationFrameDecoder ApplicationServerFrames)
  where
    loop decoder = do
      chunk <- SocketBytes.recv socketHandle 32768
      if ByteString.null chunk
        then assertFailure "application socket closed before one complete server frame"
        else case feedApplicationFrame decoder chunk of
          ApplicationFrameFeedResult [envelope] (NeedApplicationFrameBytes _) -> pure envelope
          ApplicationFrameFeedResult [] (NeedApplicationFrameBytes successor) -> loop successor
          ApplicationFrameFeedResult envelopes continuation ->
            assertFailure
              ( "unexpected application server frame batch: "
                  <> show envelopes
                  <> ", continuation: "
                  <> show continuation
              )

casePendingClaimHeartbeat :: Assertion
casePendingClaimHeartbeat = do
  phase <- newTVarIO ClaimPendingApplicationSocket
  heartbeat <- atomically (applicationHeartbeatPhase phase)
  assertEqual "pending readiness keeps the physical connection responsive" HeartbeatEstablished heartbeat
  claimed <- atomically (claimApplicationWrite phase fixtureTerminalOutbound)
  assertEqual "a candidate has no established session to dispose" False claimed
  stillPending <- atomically (readTVar phase)
  assertEqual "no session disposition changes the pending candidate" ClaimPendingApplicationSocket stillPending
  (socketHandle, peer) <- socketPair AF_UNIX Stream defaultProtocol
  ( do
      writer <- newTcpSocketWriter
      let pong = Pong (applicationHeartbeatNonce 991)
      sent <- sendApplicationPong phase writer (pure ()) socketHandle pong
      assertEqual "pending claim answers physical heartbeat" True sent
      received <- timeout 2_000_000 (SocketBytes.recv peer 32768)
      assertEqual "only the physical Pong reaches the candidate" (Just (encodeApplicationFrame (ApplicationServerEnvelope pong))) received
    )
    `finally` (close socketHandle >> close peer)
