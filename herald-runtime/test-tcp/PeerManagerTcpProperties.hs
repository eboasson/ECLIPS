{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module PeerManagerTcpProperties
  ( tests,
  )
where

import Control.Concurrent
  ( MVar,
    ThreadId,
    forkFinally,
    killThread,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
    throwTo,
    tryPutMVar,
    tryReadMVar,
    yield,
  )
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    newEmptyTMVarIO,
    newTMVarIO,
    newTQueueIO,
    newTVarIO,
    readTMVar,
    readTQueue,
    readTVar,
    retry,
    tryReadTMVar,
    tryReadTQueue,
    writeTQueue,
    writeTVar,
  )
import Control.Exception
  ( AsyncException (ThreadKilled),
    IOException,
    SomeException,
    allowInterrupt,
    bracket,
    finally,
    fromException,
    mask,
    try,
    uninterruptibleMask_,
  )
import Control.Monad
  ( forM_,
    replicateM_,
    void,
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Set qualified as Set
import Data.Word (Word16, Word8)
import Eclips.Domain.Identity
  ( HeraldEpoch,
    HeraldId,
    mkHeraldId,
  )
import Eclips.Herald.Administration (adminCorrelationId)
import Eclips.Herald.Discovery
  ( HelloRejection (..),
    PeerAddress,
    PeerBinding,
    PeerDialClass (..),
    PeerDialIntent,
    PeerHelloDisposition (..),
    knownHerald,
    peerBindingRemoteHeraldEpoch,
    peerBindingSelectedCandidate,
    peerCandidateInitiatorHeraldEpoch,
    peerDialIntentClass,
    peerDialIntentGenerationWord64,
    peerDialIntentHeraldEpoch,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.Input (PeerControl (PeerKnownHeralds))
import Eclips.Herald.Runtime
  ( HeraldRuntime,
    HeraldRuntimeConfiguration,
    RuntimeGeneratorSeedSource,
    checkApplicationRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
    heraldRuntimeConfiguration,
    runtimePeerWorkDelayMicroseconds,
    systemRuntimeMonotonicClock,
    withHeraldRuntime,
  )
import Eclips.Herald.Runtime.Connection
  ( ConnectionRef,
    PeerPlane,
  )
import Eclips.Herald.Runtime.Handler (heraldRuntimeHandlers)
import Eclips.Herald.Runtime.Ingress
  ( RuntimeSubmission (..),
    closeRuntimeConnection,
    submitPeerControl,
  )
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    defaultRuntimeHooks,
    withHeraldRuntimeWithHooks,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( PhysicalConnectionRef (PhysicalPeerRef),
  )
import Eclips.Herald.Runtime.TCP
  ( ConfiguredPeerSeed,
    HeartbeatConfiguration,
    HeraldTcpFailure
      ( HeraldTcpPeerListenerFailure,
        HeraldTcpScopeFailure
      ),
    ResolvedTcpEndpoint,
    awaitHeraldTcpExit,
    configuredPeerSeed,
    heartbeatConfigurationMicroseconds,
    heraldTcpConfiguration,
    heraldTcpEndpoints,
    heraldTcpPeerEndpoint,
    peerDialRetryDelayMicroseconds,
    requestHeraldTcpDrain,
    resolvedTcpHost,
    resolvedTcpPort,
    tcpListenEndpoint,
    withHeraldTcpRuntime,
  )
import Eclips.Herald.Runtime.TCP.Internal.Facade
  ( closePeerTransportGate,
    heraldTcpPeerTransportGate,
    openPeerTransportGate,
    peerTransportGateIsOpen,
  )
import Eclips.Herald.Runtime.TCP.Internal.Heartbeat
  ( setHeartbeatSchedulerForTest,
  )
import Eclips.Herald.Runtime.TCP.Internal.Peer
  ( cancelPeerDial,
    closeAcceptedPeerBinding,
    closePeerOwnerOnceWithProbe,
    dialCancelled,
    fallbackPeerDialDelayMicroseconds,
    handoffConfiguredSeedForTest,
    peerAdvertisedAddress,
    peerHelloDispositionEstablishesIdentity,
    runConfiguredSeedManager,
    runPeerListener,
    setConfiguredSeedProbeForTest,
    setPeerDialProbeForTest,
    setPeerDialSchedulerForTest,
    settlePeerSocketClose,
  )
import Eclips.Herald.Runtime.TCP.Internal.Scope
  ( claimPeerTcpSocket,
    claimTcpSocket,
    newTcpContext,
    newTcpSocketKey,
    releaseTcpSocket,
    requestTcpContextStop,
    spawnTcpChild,
    spawnTcpFiniteChild,
    spawnTcpTrackedChild,
    spawnTcpTrackedFiniteChild,
    stopTcpContext,
  )
import Eclips.Herald.Runtime.TCP.Internal.Socket
  ( bindTcpListener,
    closeSocketOnce,
    closeSocketOnceWithProbe,
    connectTcpEndpoint,
    prepareConnectedTcpSocket,
    prepareConnectedTcpSocketWith,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( ConfiguredPeerTarget (..),
    ConfiguredSeedProbeEvent (..),
    HeartbeatScheduler (..),
    HeraldTcp (..),
    PeerDialProbeEvent (..),
    PeerDialProbePhase (..),
    PeerDialRetryDelay (..),
    PeerDialScheduler (..),
    PeerTransportGate (..),
    PeerTransportLease (..),
    TcpContext (..),
    TcpTrackedThread (..),
  )
import Eclips.Herald.Runtime.Trace (HeraldRuntimeExit (HeraldRuntimeDrained))
import Eclips.Protocol.Peer.Frame (encodePeerFrame)
import Eclips.Protocol.Peer.Types
  ( PeerControlDto (KnownHeraldsDto),
    PeerEnvelope (PeerControlEnvelope, PeerHeartbeatEnvelope),
    PeerHeartbeatDto (Ping),
    knownHeraldCollectionDto,
    peerHeartbeatNonce,
  )
import GHC.Conc
  ( ThreadStatus (..),
    threadStatus,
  )
import Network.Socket
  ( Family (AF_UNIX),
    ShutdownCmd (ShutdownSend),
    Socket,
    SocketOption (NoDelay),
    SocketType (Stream),
    accept,
    close,
    defaultProtocol,
    getSocketOption,
    shutdown,
    socketPair,
  )
import Network.Socket.ByteString qualified as SocketBytes
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import ThreeHeraldFixtures
  ( threeH1Bootstraps,
    threeH1GeneratorSeedSource,
    threeH1Genesis,
    threeH1HeraldEpoch,
    threeH2Bootstraps,
    threeH2GeneratorSeedSource,
    threeH2Genesis,
    threeH2HeraldEpoch,
    threeH3Bootstraps,
    threeH3GeneratorSeedSource,
    threeH3Genesis,
    threeH3HeraldEpoch,
    threeOracleContacts,
  )

tests :: TestTree
tests =
  testGroup
    "peer manager TCP"
    [ testCase "mutual learned candidates retain one live exact lease until close" caseCurrentLearnedLease,
      testCase "connected and accepted TCP sockets install NoDelay before use" caseTcpNoDelay,
      testCase "socket-option failure closes only the physical connection attempt" caseTcpOptionFailure,
      testCase "preferred reconnect cancels stale fallback generations across fast loss" casePreferredFallbackGeneration,
      testCase "blocked preferred reconnect is restored by the bounded fallback" caseBlockedPreferredFallback,
      testCase "exact owner cancellation suppresses the retained learned job" caseExactLearnedCancellation,
      testCase "fallback delay uses the bounded one-second reconnect window" caseFallbackDelay,
      testCase "an interrupted peer close still invalidates its exact owner lease" caseInterruptedPeerOwnerClose,
      testCase "an interrupted descriptor close remains retryable and settles" caseInterruptedDescriptorClose,
      testCase "silent and wrong-identity learned addresses fall through to the valid target" caseWrongIdentityFallsThrough,
      testCase "malformed and wrong-phase EPRP sockets never become current" caseInvalidPeerSockets,
      testCase "silent configured seeds time out and retry only after their paced deadline" caseConfiguredSeedRetryPaced,
      testCase "retired-Herald rejection identifies and retires a configured seed" caseRetiredConfiguredSeedIdentity,
      testCase "self Hello retires a configured seed reached through another endpoint" caseSelfConfiguredSeedIdentity,
      testCase "the peer gate joins every admitted socket worker before reopening" casePeerGateJoinsSocketWorkers,
      testCase "the peer gate waits for the runtime writer before owner close" casePeerGateJoinsRuntimeWriter,
      testCase "global stop requests peer retirement before the runtime-writer join" caseGlobalStopDefersPeerWriterJoin,
      testCase "the peer-only transport gate partitions and rejoins without stopping admission" caseReversiblePeerTransportGate,
      testCase "drain closes the late physical-install admission gate" caseDrainClosesInstallGate,
      testCase "completed short connections leave only live TCP ownership in the scope" caseCompletedConnectionsReleased,
      testCase "finite TCP children report exceptions but may return and stop normally" caseFiniteChildSupervision,
      testCase "heartbeat scheduler failure terminates the TCP scope" caseHeartbeatSchedulerFailure,
      testCase "an already-retained essential failure wins callback completion" caseFailureWinsCallback
    ]

caseRetiredConfiguredSeedIdentity :: Assertion
caseRetiredConfiguredSeedIdentity = do
  assertBool
    "a checked retired Herald establishes the configured target identity"
    (peerHelloDispositionEstablishesIdentity (PeerHelloRejected HelloRetiredHerald))
  assertBool
    "an unknown or mismatching inactive Herald cannot capture a configured seed"
    (not (peerHelloDispositionEstablishesIdentity (PeerHelloRejected HelloInactiveHerald)))

caseSelfConfiguredSeedIdentity :: Assertion
caseSelfConfiguredSeedIdentity = do
  let listener = checked (tcpListenEndpoint "127.0.0.1" 0)
      retryDelay = checked (peerDialRetryDelayMicroseconds 1000)
  observed <-
    timeout 5_000_000
      $ withHeraldTcpRuntime
        (heraldTcpConfiguration (runtimeConfiguration threeH1Genesis threeH1Bootstraps threeH1GeneratorSeedSource) listener listener listener [] retryDelay testHeartbeatConfiguration)
        ( \tcp@(HeraldTcp runtime _ _ context) -> do
            let endpoint = heraldTcpPeerEndpoint (heraldTcpEndpoints tcp)
                advertised = Set.singleton (peerAdvertisedAddress endpoint)
            withIdentityProxy endpoint endpoint $ \proxy -> do
              attempts <- newTVarIO (0 :: Int)
              scheduled <- newTQueueIO
              let observe ConfiguredSeedDialAttempted = atomically (modifyTVar' attempts (+ 1))
                  observe _ = pure ()
                  scheduler = PeerDialScheduler $ \microseconds -> do
                    atomically (writeTQueue scheduled microseconds)
                    newTVarIO False
                  cleanup = do
                    setConfiguredSeedProbeForTest tcp Nothing
                    setPeerDialSchedulerForTest tcp (PeerDialScheduler (const (newTVarIO True)))
              setConfiguredSeedProbeForTest tcp (Just observe)
              setPeerDialSchedulerForTest tcp scheduler
              flip finally cleanup $ do
                assertBool "the seed endpoint is not an advertised local address" (Set.notMember (peerAdvertisedAddress proxy.proxyEndpoint) advertised)
                putMVar proxy.proxyRelease ()
                runConfiguredSeedManager
                  (spawnTcpTrackedChild context Nothing)
                  (spawnTcpTrackedFiniteChild context HeraldTcpScopeFailure)
                  context
                  runtime
                  advertised
                  retryDelay
                  testHeartbeatConfiguration
                  (seedFor proxy.proxyEndpoint)
                assertEqual "the checked self Hello completes the manager after one dial" 1 =<< atomically (readTVar attempts)
                assertEqual "self rejection schedules no configured retry" Nothing =<< atomically (tryReadTQueue scheduled)
                configured <- atomically (readTVar context.tcpConfiguredPeerTargets)
                assertBool
                  "the exact dialled seed retains its checked local identity"
                  ( any
                      ( \(ConfiguredPeerTarget _ epoch address) ->
                          epoch == threeH1HeraldEpoch && address == peerAdvertisedAddress proxy.proxyEndpoint
                      )
                      configured
                  )
                assertEqual "the resolved seed remains handed off" True =<< handoffConfiguredSeedForTest tcp proxy.proxyEndpoint
                assertNoCurrentPeers "self Hello" tcp
                closePeerTransportGate context.tcpPeerTransportGate
                assertBool "socket retirement releases the pending candidate correlation" . null =<< atomically (readTVar context.tcpConfiguredPeerCandidates)
        )
  case observed of
    Nothing -> assertFailure "self configured-seed identity test timed out"
    Just result -> assertTcpResult "self configured-seed identity" result

caseGlobalStopDefersPeerWriterJoin :: Assertion
caseGlobalStopDefersPeerWriterJoin = do
  context <- newTcpContext
  socketKey <- newTcpSocketKey
  requested <- newEmptyMVar
  runtimeWriterDone <- newEmptyMVar
  ownerClosed <- newEmptyMVar
  let requestClose = void (tryPutMVar requested ())
      awaitOwner = do
        readMVar runtimeWriterDone
        void (tryPutMVar ownerClosed ())
      lease = PeerTransportLease requestClose awaitOwner
      cleanup = do
        void (tryPutMVar runtimeWriterDone ())
        stopTcpContext context
  flip finally cleanup $ do
    claimed <- claimPeerTcpSocket context socketKey lease
    assertBool "the accepting TCP scope claims the peer lease" claimed
    requestedStop <- timeout 1_000_000 (requestTcpContextStop context)
    case requestedStop of
      Nothing -> assertFailure "the TCP stop request waited for the held runtime writer"
      Just () -> pure ()
    readMVar requested
    assertEqual
      "the request phase leaves descriptor ownership with the held runtime writer"
      Nothing
      =<< tryReadMVar ownerClosed
    putMVar runtimeWriterDone ()
    stopTcpContext context
    readMVar ownerClosed

caseTcpNoDelay :: Assertion
caseTcpNoDelay =
  bracket
    (bindTcpListener (checked (tcpListenEndpoint "127.0.0.1" 0)))
    (ignoreIOException . close . fst)
    $ \(listener, endpoint) ->
      bracket (connectTcpEndpoint endpoint) close $ \outgoing ->
        bracket (fst <$> accept listener) close $ \incoming -> do
          outgoingNoDelay <- getSocketOption outgoing NoDelay
          assertBool
            "the connector sets NoDelay before returning the socket"
            (outgoingNoDelay /= 0)
          prepareConnectedTcpSocket incoming
          incomingNoDelay <- getSocketOption incoming NoDelay
          assertBool
            "the acceptor preparation sets NoDelay before handing off the socket"
            (incomingNoDelay /= 0)

caseTcpOptionFailure :: Assertion
caseTcpOptionFailure =
  bracket
    (socketPair AF_UNIX Stream defaultProtocol)
    ( \(socketHandle, peer) -> do
        ignoreIOException (close socketHandle)
        ignoreIOException (close peer)
    )
    $ \(socketHandle, peer) -> do
      failed <-
        try
          ( prepareConnectedTcpSocketWith
              (\_ -> ioError (userError "intentional NoDelay failure"))
              socketHandle
          ) ::
          IO (Either IOException ())
      case failed of
        Left _ -> pure ()
        Right () -> assertFailure "the injected socket-option failure was accepted"
      terminal <-
        timeout
          1000000
          (try (SocketBytes.recv peer 1) :: IO (Either IOException ByteString))
      case terminal of
        Nothing -> assertFailure "socket-option failure did not close the physical attempt"
        Just (Left failure) -> assertFailure ("failed physical-attempt close: " <> show failure)
        Just (Right bytes) ->
          assertEqual
            "socket-option failure closes only the attempted lane"
            ByteString.empty
            bytes

caseCompletedConnectionsReleased :: Assertion
caseCompletedConnectionsReleased = do
  context <- newTcpContext
  liveSocket <- newTcpSocketKey
  liveClosed <- newTVarIO (0 :: Int)
  let closeLive = do
        atomically (modifyTVar' liveClosed (+ 1))
        releaseTcpSocket context liveSocket
      liveLease = PeerTransportLease closeLive (pure ())
      PeerTransportGate _ peerLeases = context.tcpPeerTransportGate
  flip finally (stopTcpContext context) $ do
    assertBool "the live socket joins the peer scope"
      =<< claimPeerTcpSocket context liveSocket liveLease
    replicateM_ 128 $ do
      socketKey <- newTcpSocketKey
      let release = releaseTcpSocket context socketKey
      assertBool "a short connection is admitted alongside the live socket"
        =<< claimPeerTcpSocket context socketKey (PeerTransportLease release (pure ()))
      spawned <- spawnTcpTrackedFiniteChild context HeraldTcpScopeFailure (pure () `finally` release)
      case spawned of
        Nothing -> assertFailure "the accepting scope rejected its finite connection worker"
        Just (TcpTrackedThread _ done) -> readMVar done
      assertEqual "completed workers do not accumulate behind the live scope" 0
        =<< atomically (length <$> readTVar context.tcpThreads)
      assertEqual "completed socket close actions are released" 1
        =<< atomically (length <$> readTVar context.tcpSocketCloses)
      assertEqual "completed peer leases are released at the same cut" 1
        =<< atomically (length <$> readTVar peerLeases)
    assertEqual "releasing short connections never closes a different live socket" 0
      =<< atomically (readTVar liveClosed)
    stopTcpContext context
    assertEqual "shutdown still closes the retained live socket exactly once" 1
      =<< atomically (readTVar liveClosed)
    assertEqual "shutdown leaves no retained socket actions" 0
      =<< atomically (length <$> readTVar context.tcpSocketCloses)

caseFiniteChildSupervision :: Assertion
caseFiniteChildSupervision = do
  normalContext <- newTcpContext
  flip finally (stopTcpContext normalContext) $ do
    normal <-
      requireFiniteChild
        normalContext
        HeraldTcpPeerListenerFailure
        (pure ())
    awaitFinishedTcpChild "normally completed finite child" normal
    assertEqual "normal completion releases the tracked child" 0
      =<< atomically (length <$> readTVar normalContext.tcpThreads)
    assertEqual
      "normal finite-child completion does not fail the TCP scope"
      Nothing
      =<< atomically (tryReadTMVar normalContext.tcpFailure)

  failedContext <- newTcpContext
  flip finally (stopTcpContext failedContext) $ do
    failed <-
      requireFiniteChild
        failedContext
        HeraldTcpScopeFailure
        (ioError (userError "finite TCP child failed"))
    awaitFinishedTcpChild "failed finite child" failed
    assertEqual "failed completion releases the tracked child" 0
      =<< atomically (length <$> readTVar failedContext.tcpThreads)
    assertEqual
      "an ordinary finite-child exception fails the accepting TCP scope"
      (Just HeraldTcpScopeFailure)
      =<< atomically (tryReadTMVar failedContext.tcpFailure)

  stoppingContext <- newTcpContext
  flip finally (stopTcpContext stoppingContext) $ do
    entered <- newEmptyMVar
    blocked <- newEmptyMVar
    _ <-
      requireFiniteChild
        stoppingContext
        HeraldTcpScopeFailure
        (putMVar entered () >> takeMVar blocked)
    takeMVar entered
    stopTcpContext stoppingContext
    assertEqual
      "scope cancellation after the admission cut is not a child failure"
      Nothing
      =<< atomically (tryReadTMVar stoppingContext.tcpFailure)

caseHeartbeatSchedulerFailure :: Assertion
caseHeartbeatSchedulerFailure = do
  let listener = checked (tcpListenEndpoint "127.0.0.1" 0)
      retryDelay = checked (peerDialRetryDelayMicroseconds 1000)
  observed <-
    timeout 5_000_000
      $ withHeraldTcpRuntime
        (heraldTcpConfiguration (runtimeConfiguration threeH1Genesis threeH1Bootstraps threeH1GeneratorSeedSource) listener listener listener [] retryDelay testHeartbeatConfiguration)
        ( \tcp -> do
            setHeartbeatSchedulerForTest tcp
              $ HeartbeatScheduler
              $ \_ -> ioError (userError "intentional heartbeat scheduler failure")
            let endpoint = heraldTcpPeerEndpoint (heraldTcpEndpoints tcp)
            bracket (connectTcpEndpoint endpoint) (ignoreIOException . close) $ \_ -> do
              never <- newEmptyMVar :: IO (MVar ())
              takeMVar never
        )
  case observed of
    Nothing -> assertFailure "heartbeat scheduler failure did not terminate the TCP scope"
    Just (Left HeraldTcpScopeFailure) -> pure ()
    Just other -> assertFailure ("unexpected heartbeat scheduler terminal result: " <> show other)

requireFiniteChild ::
  TcpContext ->
  HeraldTcpFailure ->
  IO () ->
  IO ThreadId
requireFiniteChild context failure action = do
  spawned <- spawnTcpFiniteChild context failure action
  case spawned of
    Just thread -> pure thread
    Nothing -> assertFailure "accepting TCP scope rejected a finite child"

awaitFinishedTcpChild :: String -> ThreadId -> IO ()
awaitFinishedTcpChild description thread = do
  completed <- timeout 1_000_000 loop
  case completed of
    Just () -> pure ()
    Nothing -> assertFailure (description <> " did not finish")
  where
    loop = do
      status <- threadStatus thread
      case status of
        ThreadFinished -> pure ()
        ThreadDied -> pure ()
        _ -> yield >> loop

caseInterruptedPeerOwnerClose :: Assertion
caseInterruptedPeerOwnerClose = do
  observed <-
    timeout 10000000
      $ withThreeHeralds
      $ \h1Tcp@(HeraldTcp h1Runtime _ _ _) _h2Tcp h3Tcp -> do
        (predecessor, predecessorReference) <-
          awaitOnlyCurrent h1Tcp threeH3HeraldEpoch
        referenceCell <- newTMVarIO predecessorReference
        ownerClosed <- newTVarIO False
        ownerCloseCount <- newTVarIO (0 :: Int)
        done <- newEmptyTMVarIO
        interrupted <-
          interruptAtProbe $ \probe ->
            settlePeerSocketClose done $ do
              closePeerOwnerOnceWithProbe
                (atomically (modifyTVar' ownerCloseCount (+ 1)) >> probe)
                h1Runtime
                referenceCell
                ownerClosed
              allowInterrupt
        assertThreadKilled "peer owner close" interrupted
        atomically (readTMVar done)
        assertEqual
          "the masked claim reaches the exact runtime invalidation once"
          1
          =<< atomically (readTVar ownerCloseCount)
        assertEqual
          "the exact owner-close claim is retained after interruption"
          True
          =<< atomically (readTVar ownerClosed)
        assertEqual
          "the interrupted close already made the exact physical generation stale"
          StalePhysicalGeneration
          =<< closeRuntimeConnection h1Runtime predecessorReference
        ((replacement, _), _) <-
          awaitConvergedReplacement h1Tcp h3Tcp predecessor
        assertBool
          "the queued binding loss permits a fresh physical lease"
          (replacement /= predecessor)
  case observed of
    Nothing -> assertFailure "interrupted exact peer-owner close test timed out"
    Just result -> assertTcpResult "three-Herald interrupted owner close" result

caseInterruptedDescriptorClose :: Assertion
caseInterruptedDescriptorClose =
  bracket
    (socketPair AF_UNIX Stream defaultProtocol)
    (\(socketHandle, peer) -> ignoreIOException (close socketHandle) >> ignoreIOException (close peer))
    $ \(socketHandle, peer) -> do
      closed <- newTVarIO False
      done <- newEmptyTMVarIO
      interrupted <-
        interruptAtProbe $ \probe ->
          settlePeerSocketClose done
            $ closeSocketOnceWithProbe probe closed socketHandle
      assertThreadKilled "descriptor close" interrupted
      atomically (readTMVar done)
      assertEqual
        "interruption before the descriptor call does not publish completion"
        False
        =<< atomically (readTVar closed)

      closeSocketOnce closed socketHandle
      assertEqual
        "a retained closer publishes descriptor completion after retry"
        True
        =<< atomically (readTVar closed)
      terminal <-
        timeout
          1000000
          (try (SocketBytes.recv peer 1) :: IO (Either IOException ByteString))
      case terminal of
        Nothing -> assertFailure "retried descriptor close did not reach EOF"
        Just (Left failure) -> assertFailure ("retried descriptor close failed: " <> show failure)
        Just (Right bytes) -> assertEqual "retried descriptor close reaches EOF" ByteString.empty bytes

caseCurrentLearnedLease :: Assertion
caseCurrentLearnedLease = do
  observed <-
    timeout 10000000
      $ withThreeHeralds
      $ \h1Tcp _h2Tcp h3Tcp -> do
        (firstH1Binding, firstH1Reference) <- awaitOnlyCurrent h1Tcp threeH3HeraldEpoch
        (firstH3Binding, _) <- awaitOnlyCurrent h3Tcp threeH1HeraldEpoch
        assertEqual
          "the reciprocal sockets expose the same selected candidate"
          (peerBindingSelectedCandidate firstH1Binding)
          (peerBindingSelectedCandidate firstH3Binding)
        assertEqual
          "the deterministic mutual-dial winner is the H1-initiated candidate"
          threeH1HeraldEpoch
          (peerCandidateInitiatorHeraldEpoch (peerBindingSelectedCandidate firstH1Binding))
        replicateM_ 3 $ do
          submitted <-
            submitPeerControl
              (heraldTcpRuntime h1Tcp)
              firstH1Reference
              firstH1Binding
              (PeerKnownHeralds [])
          assertEqual "the exact current reference remains control-capable" Queued submitted
        (retainedBinding, retainedReference) <- awaitOnlyCurrent h1Tcp threeH3HeraldEpoch
        assertEqual "ordinary current control does not churn the selected lease" firstH1Binding retainedBinding
        assertEqual "ordinary current control retains its physical reference" firstH1Reference retainedReference

        closed <- closeAcceptedPeerBinding h1Tcp firstH1Binding
        assertEqual "closing the exact current lease reaches its runtime reference" (Just ConnectionClosed) closed
        ((replacement, replacementReference), _) <-
          awaitConvergedReplacement h1Tcp h3Tcp firstH1Binding
        staleClose <- closeAcceptedPeerBinding h1Tcp firstH1Binding
        assertEqual "the predecessor is absent rather than retained as history" Nothing staleClose
        assertBool "the replacement has a new logical binding" (replacement /= firstH1Binding)
        assertBool "the replacement has a new physical reference" (replacementReference /= firstH1Reference)
        replacementControl <-
          submitPeerControl
            (heraldTcpRuntime h1Tcp)
            replacementReference
            replacement
            (PeerKnownHeralds [])
        assertEqual "the replacement current lease is immediately control-capable" Queued replacementControl
  case observed of
    Nothing -> assertFailure "mutual learned-candidate current-lease test timed out"
    Just result -> assertTcpResult "three-Herald lease" result

casePreferredFallbackGeneration :: Assertion
casePreferredFallbackGeneration = do
  observed <-
    timeout 10000000
      $ withThreeHeralds
      $ \h1Tcp _h2Tcp h3Tcp -> do
        (predecessor, _) <- awaitOnlyCurrent h1Tcp threeH3HeraldEpoch
        scheduled <- newTQueueIO
        events <- newTVarIO []
        let scheduler =
              PeerDialScheduler $ \microseconds -> do
                elapsed <- newTVarIO False
                atomically (writeTQueue scheduled (microseconds, elapsed))
                pure elapsed
            observe event = atomically (modifyTVar' events (event :))
            cleanup = do
              setPeerDialProbeForTest h3Tcp Nothing
              setPeerDialSchedulerForTest
                h3Tcp
                (PeerDialScheduler (\_ -> newTVarIO True))
        setPeerDialSchedulerForTest h3Tcp scheduler
        setPeerDialProbeForTest h3Tcp (Just observe)
        flip finally cleanup $ do
          assertEqual "close the predecessor exact lease" (Just ConnectionClosed)
            =<< closeAcceptedPeerBinding h1Tcp predecessor
          (firstDelay, firstElapsed) <- atomically (readTQueue scheduled)
          assertEqual "non-preferred fallback uses the bounded window" 1_000_000 firstDelay
          firstIntent <-
            awaitPeerDialEvent
              events
              ( \(PeerDialProbeEvent intent phase) ->
                  peerDialIntentHeraldEpoch intent == threeH1HeraldEpoch
                    && phase == PeerDialFallbackScheduled
              )
          assertEqual
            "the larger H3 identity retains fallback permission"
            FallbackPeerDial
            (peerDialIntentClass firstIntent)
          let firstGeneration = peerDialIntentGenerationWord64 firstIntent

          ((replacement, _), _) <-
            awaitConvergedReplacement h1Tcp h3Tcp predecessor
          assertEqual "close the prompt preferred replacement" (Just ConnectionClosed)
            =<< closeAcceptedPeerBinding h1Tcp replacement

          secondIntent <-
            awaitPeerDialEvent
              events
              ( \(PeerDialProbeEvent intent phase) ->
                  peerDialIntentHeraldEpoch intent == threeH1HeraldEpoch
                    && phase == PeerDialFallbackScheduled
                    && peerDialIntentGenerationWord64 intent > firstGeneration
              )
          let secondGeneration = peerDialIntentGenerationWord64 secondIntent
          assertBool
            "replacement loss advances beyond the durably satisfied generation"
            (secondGeneration > firstGeneration)

          -- Exercise the physical cancellation receipt algebra with real
          -- owner-issued generations, including replay and reordering.
          cancellationContext <- newTcpContext
          atomically (cancelPeerDial cancellationContext firstIntent)
          assertEqual "an older cancellation does not revoke its successor" False
            =<< atomically (dialCancelled cancellationContext secondIntent)
          forM_ [1 .. 100 :: Int] $ \_ -> atomically $ do
            cancelPeerDial cancellationContext secondIntent
            cancelPeerDial cancellationContext firstIntent
          assertEqual "cancellation history retains one target generation" 1
            =<< atomically (length <$> readTVar cancellationContext.tcpCancelledDialGenerations)
          assertEqual "reordered old cancellation cannot reopen the old permission" True
            =<< atomically (dialCancelled cancellationContext firstIntent)
          assertEqual "the successor cancellation remains authoritative" True
            =<< atomically (dialCancelled cancellationContext secondIntent)

          _ <-
            awaitPeerDialEvent
              events
              ( \(PeerDialProbeEvent intent phase) ->
                  peerDialIntentHeraldEpoch intent == threeH1HeraldEpoch
                    && peerDialIntentGenerationWord64 intent == firstGeneration
                    && phase == PeerDialSatisfied
              )
          -- Releasing the obsolete timer after its accepted binding has
          -- already come and gone cannot resurrect generation N.
          atomically (writeTVar firstElapsed True)
          retained <- atomically (readTVar events)
          assertBool
            "the stale fallback generation never attempts a socket"
            ( not
                ( any
                    ( \(PeerDialProbeEvent intent phase) ->
                        peerDialIntentHeraldEpoch intent == threeH1HeraldEpoch
                          && peerDialIntentGenerationWord64 intent == firstGeneration
                          && phase == PeerDialAttempted
                    )
                    retained
                )
            )
  case observed of
    Nothing -> assertFailure "preferred/fallback generation test timed out"
    Just result -> assertTcpResult "three-Herald preferred fallback" result

caseFallbackDelay :: Assertion
caseFallbackDelay = do
  assertEqual
    "short configured retry receives one meaningful fallback second"
    1_000_000
    (fallbackPeerDialDelayMicroseconds (PeerDialRetryDelay 1_000))
  assertEqual
    "an explicitly slower retry is not shortened"
    2_000_000
    (fallbackPeerDialDelayMicroseconds (PeerDialRetryDelay 2_000_000))

caseExactLearnedCancellation :: Assertion
caseExactLearnedCancellation = do
  observed <-
    timeout 10000000
      $ withThreeHeralds
      $ \h1Tcp _h2Tcp h3Tcp@(HeraldTcp _ _ _ h3Context) -> do
        (predecessor, _) <- awaitOnlyCurrent h1Tcp threeH3HeraldEpoch
        scheduled <- newTQueueIO
        events <- newTVarIO []
        releasePreferred <- newEmptyMVar
        let scheduler =
              PeerDialScheduler $ \_ -> do
                elapsed <- newTVarIO False
                atomically (writeTQueue scheduled elapsed)
                pure elapsed
            observe event = atomically (modifyTVar' events (event :))
            holdPreferred (PeerDialProbeEvent intent phase)
              | peerDialIntentHeraldEpoch intent == threeH3HeraldEpoch,
                phase == PeerDialAttempted =
                  takeMVar releasePreferred
              | otherwise = pure ()
            cleanup = do
              void (tryPutMVar releasePreferred ())
              setPeerDialProbeForTest h1Tcp Nothing
              setPeerDialProbeForTest h3Tcp Nothing
              setPeerDialSchedulerForTest
                h3Tcp
                (PeerDialScheduler (\_ -> newTVarIO True))
        setPeerDialSchedulerForTest h3Tcp scheduler
        setPeerDialProbeForTest h3Tcp (Just observe)
        setPeerDialProbeForTest h1Tcp (Just holdPreferred)
        flip finally cleanup $ do
          assertEqual "close the learned predecessor lease" (Just ConnectionClosed)
            =<< closeAcceptedPeerBinding h1Tcp predecessor
          elapsed <- atomically (readTQueue scheduled)
          retained <-
            awaitPeerDialEvent
              events
              ( \(PeerDialProbeEvent intent phase) ->
                  peerDialIntentHeraldEpoch intent == threeH1HeraldEpoch
                    && phase == PeerDialFallbackScheduled
              )
          forM_ [1 .. 100 :: Int] $ \_ -> atomically (cancelPeerDial h3Context retained)
          assertEqual "duplicate exact cancellations retain one receipt" 1
            =<< atomically (length <$> readTVar h3Context.tcpCancelledDialGenerations)
          atomically (writeTVar elapsed True)
          _ <-
            awaitPeerDialEvent
              events
              ( \(PeerDialProbeEvent intent phase) ->
                  peerDialIntentHeraldEpoch intent == threeH1HeraldEpoch
                    && peerDialIntentGenerationWord64 intent
                      == peerDialIntentGenerationWord64 retained
                    && phase == PeerDialCancelled
              )
          retainedEvents <- atomically (readTVar events)
          assertBool
            "the retained learned job never attempts after exact cancellation"
            ( not
                ( any
                    ( \(PeerDialProbeEvent intent phase) ->
                        peerDialIntentHeraldEpoch intent == threeH1HeraldEpoch
                          && peerDialIntentGenerationWord64 intent
                            == peerDialIntentGenerationWord64 retained
                          && phase == PeerDialAttempted
                    )
                    retainedEvents
                )
            )
  case observed of
    Nothing -> assertFailure "exact learned cancellation test timed out"
    Just result -> assertTcpResult "exact learned cancellation" result

caseBlockedPreferredFallback :: Assertion
caseBlockedPreferredFallback = do
  observed <-
    timeout 10000000
      $ withThreeHeralds
      $ \h1Tcp _h2Tcp h3Tcp -> do
        (predecessor, _) <- awaitOnlyCurrent h1Tcp threeH3HeraldEpoch
        scheduled <- newTQueueIO
        events <- newTVarIO []
        preferredAttempted <- newEmptyMVar
        releasePreferred <- newEmptyMVar
        let scheduler =
              PeerDialScheduler $ \microseconds -> do
                elapsed <- newTVarIO False
                atomically (writeTQueue scheduled (microseconds, elapsed))
                pure elapsed
            observe event = atomically (modifyTVar' events (event :))
            holdPreferred event@(PeerDialProbeEvent intent phase)
              | peerDialIntentHeraldEpoch intent == threeH3HeraldEpoch,
                phase == PeerDialAttempted = do
                  void (tryPutMVar preferredAttempted event)
                  takeMVar releasePreferred
              | otherwise = pure ()
            cleanup = do
              void (tryPutMVar releasePreferred ())
              setPeerDialProbeForTest h1Tcp Nothing
              setPeerDialProbeForTest h3Tcp Nothing
        setPeerDialSchedulerForTest h3Tcp scheduler
        setPeerDialProbeForTest h3Tcp (Just observe)
        setPeerDialProbeForTest h1Tcp (Just holdPreferred)
        flip finally cleanup $ do
          assertEqual "close the predecessor exact lease" (Just ConnectionClosed)
            =<< closeAcceptedPeerBinding h1Tcp predecessor
          _ <- takeMVar preferredAttempted
          (fallbackDelay, elapsed) <- atomically (readTQueue scheduled)
          assertEqual "blocked-preferred fallback retains the bounded delay" 1_000_000 fallbackDelay
          scheduledIntent <-
            awaitPeerDialEvent
              events
              ( \(PeerDialProbeEvent intent phase) ->
                  peerDialIntentHeraldEpoch intent == threeH1HeraldEpoch
                    && phase == PeerDialFallbackScheduled
              )
          assertEqual
            "H3 is the non-preferred endpoint"
            FallbackPeerDial
            (peerDialIntentClass scheduledIntent)
          retainedBeforeRelease <- atomically (readTVar events)
          assertBool
            "the learned fallback cannot attempt before its injected release"
            ( not
                ( any
                    ( \(PeerDialProbeEvent intent phase) ->
                        peerDialIntentHeraldEpoch intent == threeH1HeraldEpoch
                          && peerDialIntentGenerationWord64 intent
                            == peerDialIntentGenerationWord64 scheduledIntent
                          && phase == PeerDialAttempted
                    )
                    retainedBeforeRelease
                )
            )
          atomically (writeTVar elapsed True)
          _ <-
            awaitPeerDialEvent
              events
              ( \(PeerDialProbeEvent intent phase) ->
                  peerDialIntentHeraldEpoch intent == threeH1HeraldEpoch
                    && peerDialIntentGenerationWord64 intent
                      == peerDialIntentGenerationWord64 scheduledIntent
                    && phase == PeerDialAttempted
              )
          ((replacement, _), _) <-
            awaitConvergedReplacement h1Tcp h3Tcp predecessor
          assertBool
            "the released fallback restores an exact replacement binding"
            (replacement /= predecessor)
          assertEqual
            "the blocked-preferred schedule is restored by the H3-initiated candidate"
            threeH3HeraldEpoch
            (peerCandidateInitiatorHeraldEpoch (peerBindingSelectedCandidate replacement))
  case observed of
    Nothing -> assertFailure "blocked preferred fallback test timed out"
    Just result -> assertTcpResult "three-Herald blocked preferred fallback" result

awaitPeerDialEvent ::
  TVar [PeerDialProbeEvent] ->
  (PeerDialProbeEvent -> Bool) ->
  IO PeerDialIntent
awaitPeerDialEvent events matches =
  atomically $ do
    retained <- readTVar events
    case [intent | event@(PeerDialProbeEvent intent _) <- retained, matches event] of
      intent : _ -> pure intent
      [] -> retry

caseWrongIdentityFallsThrough :: Assertion
caseWrongIdentityFallsThrough = do
  let applicationListener = checked (tcpListenEndpoint "127.0.0.1" 0)
      ordinaryPeerListener = checked (tcpListenEndpoint "127.0.0.1" 0)
      retryDelay = checked (peerDialRetryDelayMicroseconds 1000)
  observed <-
    timeout 10000000
      $ withHeraldTcpRuntime
        (heraldTcpConfiguration (runtimeConfiguration threeH3Genesis threeH3Bootstraps threeH3GeneratorSeedSource) applicationListener applicationListener ordinaryPeerListener [] retryDelay testHeartbeatConfiguration)
        ( \h3Tcp -> do
            let h3Endpoint = heraldTcpPeerEndpoint (heraldTcpEndpoints h3Tcp)
            h2Result <-
              withHeraldTcpRuntime
                (heraldTcpConfiguration (runtimeConfiguration threeH2Genesis threeH2Bootstraps threeH2GeneratorSeedSource) applicationListener applicationListener ordinaryPeerListener [] retryDelay testHeartbeatConfiguration)
                ( \h2Tcp -> do
                    let h2Endpoint = heraldTcpPeerEndpoint (heraldTcpEndpoints h2Tcp)
                    withIdentityProxy h2Endpoint h3Endpoint $ \silentProxy ->
                      withIdentityProxy h2Endpoint h3Endpoint $ \wrongIdentityProxy -> do
                        let silentAddress = peerAdvertisedAddress silentProxy.proxyEndpoint
                            wrongAddress = peerAdvertisedAddress wrongIdentityProxy.proxyEndpoint
                            validAddress = peerAdvertisedAddress h3Endpoint
                        assertBool
                          "the silent and wrong-identity addresses precede the valid address"
                          (silentAddress < wrongAddress && wrongAddress < validAddress)
                        h1Result <-
                          withHeraldTcpRuntime
                            ( heraldTcpConfiguration
                                (runtimeConfiguration threeH1Genesis threeH1Bootstraps threeH1GeneratorSeedSource)
                                applicationListener
                                applicationListener
                                ordinaryPeerListener
                                [seedFor h2Endpoint]
                                retryDelay
                                shortCandidateHeartbeatConfiguration
                            )
                            ( \h1Tcp -> do
                                h1ToH2 <- awaitOnlyCurrent h1Tcp threeH2HeraldEpoch
                                submitKnownTarget
                                  h1Tcp
                                  h1ToH2
                                  (Set.fromList [silentAddress, wrongAddress, validAddress])
                                takeMVar silentProxy.proxyAccepted
                                takeMVar wrongIdentityProxy.proxyAccepted
                                h1ToH3BeforeRelease <- currentFor h1Tcp threeH3HeraldEpoch
                                assertEqual
                                  "the silent deadline advances only to the still-blocked wrong identity"
                                  []
                                  h1ToH3BeforeRelease
                                putMVar wrongIdentityProxy.proxyRelease ()
                                (h1ToH3, _) <- awaitOnlyCurrent h1Tcp threeH3HeraldEpoch
                                (h3FromH1, _) <- awaitOnlyCurrent h3Tcp threeH1HeraldEpoch
                                assertEqual
                                  "the valid final endpoint establishes the intended exact target"
                                  (peerBindingSelectedCandidate h1ToH3)
                                  (peerBindingSelectedCandidate h3FromH1)
                            )
                        assertTcpResult "H1 learned-address fall-through" h1Result
                )
            assertTcpResult "H2 wrong-identity proxy target" h2Result
        )
  case observed of
    Nothing -> assertFailure "wrong-identity learned-address test timed out"
    Just result -> assertTcpResult "H3 intended target" result

caseInvalidPeerSockets :: Assertion
caseInvalidPeerSockets = do
  let listener = checked (tcpListenEndpoint "127.0.0.1" 0)
      retryDelay = checked (peerDialRetryDelayMicroseconds 1000)
      candidateHeartbeat =
        encodePeerFrame
          (PeerHeartbeatEnvelope (Ping (peerHeartbeatNonce 17)))
  observed <-
    timeout 5000000
      $ withHeraldTcpRuntime
        (heraldTcpConfiguration (runtimeConfiguration threeH1Genesis threeH1Bootstraps threeH1GeneratorSeedSource) listener listener listener [] retryDelay testHeartbeatConfiguration)
        ( \tcp -> do
            let endpoint = heraldTcpPeerEndpoint (heraldTcpEndpoints tcp)
            sendTerminalPeerBytes endpoint (ByteString.pack [0, 0, 0, 3])
            assertNoCurrentPeers "malformed length" tcp
            sendTerminalPeerBytes endpoint candidatePhaseControlFrame
            assertNoCurrentPeers "candidate-phase control" tcp
            sendTerminalPeerBytes endpoint candidateHeartbeat
            assertNoCurrentPeers "candidate-phase heartbeat" tcp
        )
  case observed of
    Nothing -> assertFailure "invalid EPRP socket test timed out"
    Just result -> assertTcpResult "invalid EPRP" result

caseConfiguredSeedRetryPaced :: Assertion
caseConfiguredSeedRetryPaced =
  bracket
    (bindTcpListener (checked (tcpListenEndpoint "127.0.0.1" 0)))
    (ignoreIOException . close . fst)
    $ \(seedListener, seedEndpoint) -> do
      let listener = checked (tcpListenEndpoint "127.0.0.1" 0)
          retryDelay = checked (peerDialRetryDelayMicroseconds 1000)
      observed <-
        timeout 5000000
          $ withHeraldTcpRuntime
            ( heraldTcpConfiguration
                (runtimeConfiguration threeH1Genesis threeH1Bootstraps threeH1GeneratorSeedSource)
                listener
                listener
                listener
                [seedFor seedEndpoint]
                retryDelay
                shortCandidateHeartbeatConfiguration
            )
            ( \tcp ->
                bracket (fst <$> accept seedListener) (ignoreIOException . close) $ \firstAttempt -> do
                  scheduled <- newTQueueIO
                  attempts <- newTQueueIO
                  let scheduler =
                        PeerDialScheduler $ \microseconds -> do
                          elapsed <- newTVarIO False
                          atomically (writeTQueue scheduled (microseconds, elapsed))
                          pure elapsed
                      observe event = case event of
                        ConfiguredSeedDialAttempted -> atomically (writeTQueue attempts event)
                        _ -> pure ()
                      cleanup = do
                        setConfiguredSeedProbeForTest tcp Nothing
                        setPeerDialSchedulerForTest
                          tcp
                          (PeerDialScheduler (const (newTVarIO True)))
                  setPeerDialSchedulerForTest tcp scheduler
                  setConfiguredSeedProbeForTest tcp (Just observe)
                  flip finally cleanup $ do
                    assertEqual
                      "a silent configured candidate reaches EOF at its establishment deadline"
                      (Just ())
                      =<< timeout 1_000_000 (awaitSocketEof firstAttempt)
                    (configuredDelay, release) <- atomically (readTQueue scheduled)
                    assertEqual "configured retry uses the checked physical interval" 1000 configuredDelay
                    replicateM_ 100 yield
                    assertEqual "configured retry cannot repeat before release" Nothing
                      =<< atomically (tryReadTQueue attempts)
                    atomically (writeTVar release True)
                    assertEqual "one release grants one configured retry" ConfiguredSeedDialAttempted
                      =<< atomically (readTQueue attempts)
            )
      case observed of
        Nothing -> assertFailure "configured-seed pacing test timed out"
        Just result -> assertTcpResult "configured-seed pacing" result

casePeerGateJoinsSocketWorkers :: Assertion
casePeerGateJoinsSocketWorkers = do
  observed <-
    timeout 5_000_000
      $ withHeraldRuntime
        (runtimeConfiguration threeH1Genesis threeH1Bootstraps threeH1GeneratorSeedSource)
      $ \runtime -> do
        context <- newTcpContext
        flip finally (stopTcpContext context) $ do
          (listener, endpoint) <-
            bindTcpListener (checked (tcpListenEndpoint "127.0.0.1" 0))
          atomically (modifyTVar' context.tcpListeners (listener :))
          listenerThread <-
            spawnTcpChild
              context
              (Just HeraldTcpPeerListenerFailure)
              ( runPeerListener
                  (spawnTcpTrackedChild context Nothing)
                  (spawnTcpTrackedFiniteChild context HeraldTcpScopeFailure)
                  context
                  runtime
                  (Set.singleton (peerAdvertisedAddress endpoint))
                  testHeartbeatConfiguration
                  listener
              )
          case listenerThread of
            Nothing -> assertFailure "accepting TCP scope rejected the focused peer listener"
            Just _ -> pure ()

          predecessors <- trackedTcpThreadIds context
          bracket
            (connectTcpEndpoint endpoint)
            (ignoreIOException . close)
            ( \_ -> do
                workerCompletions <- awaitNewPeerWorkerCompletions context predecessors
                closePeerTransportGate context.tcpPeerTransportGate
                assertEqual
                  "the peer cut returned before every exact worker wrapper completed"
                  (replicate 4 (Just ()))
                  =<< traverse tryReadMVar workerCompletions
            )

          openPeerTransportGate context.tcpPeerTransportGate
          sendTerminalPeerBytes endpoint candidatePhaseControlFrame
  case observed of
    Nothing -> assertFailure "peer-gate worker-join regression timed out"
    Just (Left failure) ->
      assertFailure ("focused peer runtime failed: " <> show failure)
    Just (Right _) -> pure ()

trackedTcpThreadIds :: TcpContext -> IO [ThreadId]
trackedTcpThreadIds context =
  fmap (\(TcpTrackedThread thread _) -> thread)
    <$> atomically (readTVar context.tcpThreads)

-- This focused scope contains only its peer listener. One silent accepted
-- socket adds its dedicated owner plus reader, heartbeat, and establishment
-- workers. The exact completion cells remain stable even after a worker exits.
awaitNewPeerWorkerCompletions :: TcpContext -> [ThreadId] -> IO [MVar ()]
awaitNewPeerWorkerCompletions context predecessors =
  atomically $ do
    children <- readTVar context.tcpThreads
    let fresh =
          [ child
          | child@(TcpTrackedThread thread _) <- children,
            thread `notElem` predecessors
          ]
    case fresh of
      [_, _, _, _] ->
        pure [done | TcpTrackedThread _ done <- fresh]
      _ -> retry

casePeerGateJoinsRuntimeWriter :: Assertion
casePeerGateJoinsRuntimeWriter = do
  writerDequeued <- newEmptyMVar
  releaseWriter <- newEmptyMVar
  gateDone <- newEmptyMVar
  let observeWriter physical = case physical of
        PhysicalPeerRef _ -> do
          void (tryPutMVar writerDequeued ())
          readMVar releaseWriter
        _ -> pure ()
      hooks =
        defaultRuntimeHooks
          { hookAfterWriterCommandDequeued = observeWriter
          }
      release = void (tryPutMVar releaseWriter ())
  observed <-
    timeout 5_000_000
      $ flip finally release
      $ withHeraldRuntimeWithHooks
        hooks
        (runtimeConfiguration threeH1Genesis threeH1Bootstraps threeH1GeneratorSeedSource)
      $ \runtime -> do
        context <- newTcpContext
        flip finally (stopTcpContext context) $ do
          (listener, endpoint) <-
            bindTcpListener (checked (tcpListenEndpoint "127.0.0.1" 0))
          atomically (modifyTVar' context.tcpListeners (listener :))
          listenerThread <-
            spawnTcpChild
              context
              (Just HeraldTcpPeerListenerFailure)
              ( runPeerListener
                  (spawnTcpTrackedChild context Nothing)
                  (spawnTcpTrackedFiniteChild context HeraldTcpScopeFailure)
                  context
                  runtime
                  (Set.singleton (peerAdvertisedAddress endpoint))
                  testHeartbeatConfiguration
                  listener
              )
          case listenerThread of
            Nothing -> assertFailure "accepting TCP scope rejected the writer-witness peer listener"
            Just _ -> pure ()

          predecessors <- trackedTcpThreadIds context
          bracket
            (connectTcpEndpoint endpoint)
            (ignoreIOException . close)
            ( \_ -> do
                workerCompletions <- awaitNewPeerWorkerCompletions context predecessors
                _ <-
                  forkFinally
                    (closePeerTransportGate context.tcpPeerTransportGate)
                    (const (putMVar gateDone ()))
                _ <- readMVar writerDequeued
                assertEqual
                  "the peer cut completed while the runtime writer still owned the socket"
                  Nothing
                  =<< tryReadMVar gateDone
                putMVar releaseWriter ()
                _ <- readMVar gateDone
                assertEqual
                  "runtime-writer release did not let the owner join every socket worker"
                  (replicate 4 (Just ()))
                  =<< traverse tryReadMVar workerCompletions
            )

          openPeerTransportGate context.tcpPeerTransportGate
          sendTerminalPeerBytes endpoint candidatePhaseControlFrame
  case observed of
    Nothing -> assertFailure "peer-gate runtime-writer regression timed out"
    Just (Left failure) ->
      assertFailure ("focused peer runtime failed: " <> show failure)
    Just (Right _) -> pure ()

caseReversiblePeerTransportGate :: Assertion
caseReversiblePeerTransportGate = do
  stage <- newTVarIO ("awaiting initial H1-H2 lease" :: String)
  let mark current = atomically (writeTVar stage current)
  observed <-
    timeout 10000000
      $ withThreeHeralds
      $ \h1Tcp@(HeraldTcp h1Runtime _ _ h1Context) h2Tcp _h3Tcp -> do
        (predecessor, predecessorReference) <-
          awaitOnlyCurrent h1Tcp threeH2HeraldEpoch
        let h2Address = peerAdvertisedAddress (heraldTcpPeerEndpoint (heraldTcpEndpoints h2Tcp))
            h2Endpoint = heraldTcpPeerEndpoint (heraldTcpEndpoints h2Tcp)
        -- Deterministically exercise a configured manager reaching its wait
        -- only after an accepted learned/inbound socket already owns the
        -- advertised route.  It must hand off rather than remain a second
        -- reconnect owner.
        atomically (writeTVar h1Context.tcpConfiguredPeerTargets [])
        assertEqual
          "an existing current contact causes configured-seed handoff"
          True
          =<< handoffConfiguredSeedForTest h1Tcp h2Endpoint
        configured <- atomically (readTVar h1Context.tcpConfiguredPeerTargets)
        assertBool
          "the current-contact handoff retained the exact configured route"
          ( any
              ( \(ConfiguredPeerTarget _ epoch address) ->
                  epoch == threeH2HeraldEpoch && address == h2Address
              )
              configured
          )
        mark "closing initial peer gate"
        let gate = heraldTcpPeerTransportGate h1Tcp
        initiallyOpen <- peerTransportGateIsOpen gate
        assertEqual "ordinary startup leaves the peer-only gate open" True initiallyOpen

        beforeAttempt <- newEmptyMVar
        releaseAttempt <- newEmptyMVar
        let probe event@(PeerDialProbeEvent intent phase)
              | peerDialIntentHeraldEpoch intent == threeH2HeraldEpoch,
                phase == PeerDialAttempted = do
                  void (tryPutMVar beforeAttempt event)
                  takeMVar releaseAttempt
              | otherwise = pure ()
            cleanupProbe = do
              void (tryPutMVar releaseAttempt ())
              setPeerDialProbeForTest h1Tcp Nothing
              openPeerTransportGate gate
        setPeerDialProbeForTest h1Tcp (Just probe)
        closePeerTransportGate gate
        closed <- peerTransportGateIsOpen gate
        assertEqual "closing the gate publishes the exact peer-only cut" False closed
        retained <- atomically (readTVar h1Context.tcpCurrentPeerConnections)
        assertEqual "the cut has synchronously closed every admitted peer socket" [] retained
        assertEqual
          "the peer cut also invalidates the exact owner lease before returning"
          StalePhysicalGeneration
          =<< closeRuntimeConnection h1Runtime predecessorReference
        accepting <- atomically (readTVar h1Context.tcpAccepting)
        assertEqual "the peer-only cut does not close composite admission" True accepting

        let exerciseClosedClaim = do
              mark "awaiting learned-generation attempt"
              openPeerTransportGate gate
              _ <- takeMVar beforeAttempt
              mark "rejecting learned-generation claim at closed gate"
              closePeerTransportGate gate
              putMVar releaseAttempt ()
              stillClosed <- peerTransportGateIsOpen gate
              assertEqual "the held transport cut remains closed" False stillClosed
              assertEqual
                "the generation-qualified socket cannot cross the reclosed gate"
                []
                =<< atomically (readTVar h1Context.tcpCurrentPeerConnections)
              stillAccepting <- atomically (readTVar h1Context.tcpAccepting)
              assertEqual
                "the learned generation remains supervised at the closed admission wait"
                True
                stillAccepting
        mask $ \restore -> do
          restore exerciseClosedClaim `finally` cleanupProbe

        reopened <- peerTransportGateIsOpen gate
        assertEqual "the same gate can be reopened" True reopened
        mark "awaiting replacement H1-H2 lease"
        (replacement, _) <- awaitOnlyCurrent h1Tcp threeH2HeraldEpoch
        assertBool "the retained owner generation establishes a fresh binding after reopen" (replacement /= predecessor)
  case observed of
    Nothing -> do
      current <- atomically (readTVar stage)
      assertFailure ("reversible peer-transport gate test timed out while " <> current)
    Just result -> assertTcpResult "three-Herald reversible partition" result

caseDrainClosesInstallGate :: Assertion
caseDrainClosesInstallGate = do
  let listener = checked (tcpListenEndpoint "127.0.0.1" 0)
      retryDelay = checked (peerDialRetryDelayMicroseconds 1000)
  observed <-
    timeout 5000000
      $ withHeraldTcpRuntime
        (heraldTcpConfiguration (runtimeConfiguration threeH1Genesis threeH1Bootstraps threeH1GeneratorSeedSource) listener listener listener [] retryDelay testHeartbeatConfiguration)
        ( \tcp@(HeraldTcp _ _ _ context) -> do
            submission <- requestHeraldTcpDrain tcp (adminCorrelationId 901)
            assertEqual "the semantic drain request is admitted" Queued submission
            socketKey <- newTcpSocketKey
            lateInstall <- claimTcpSocket context socketKey (pure ())
            assertEqual "an already-connected late installer loses the drain cut" False lateInstall
            accepting <- atomically (readTVar context.tcpAccepting)
            assertEqual "the physical admission fact remains closed" False accepting
            exit <- awaitHeraldTcpExit tcp
            assertEqual "the same request reaches ordinary semantic drain" (Right HeraldRuntimeDrained) exit
        )
  case observed of
    Nothing -> assertFailure "TCP drain admission-gate test timed out"
    Just (Left failure) -> assertFailure ("draining Herald TCP failed: " <> show failure)
    Just (Right ((), HeraldRuntimeDrained)) -> pure ()
    Just (Right (_, unexpectedExit)) ->
      assertFailure ("unexpected outer drain result: " <> show unexpectedExit)

caseFailureWinsCallback :: Assertion
caseFailureWinsCallback = do
  let listener = checked (tcpListenEndpoint "127.0.0.1" 0)
      retryDelay = checked (peerDialRetryDelayMicroseconds 1000)
  observed <-
    timeout 5000000
      $ withHeraldTcpRuntime
        (heraldTcpConfiguration (runtimeConfiguration threeH1Genesis threeH1Bootstraps threeH1GeneratorSeedSource) listener listener listener [] retryDelay testHeartbeatConfiguration)
        ( \(HeraldTcp _ _ _ context) -> do
            peerListener <-
              atomically $ do
                listeners <- readTVar context.tcpListeners
                case listeners of
                  current : _ -> pure current
                  [] -> retry
            close peerListener
            _ <- atomically (readTMVar context.tcpFailure)
            pure ()
        )
  case observed of
    Nothing -> assertFailure "essential TCP terminal arbitration test timed out"
    Just (Left HeraldTcpPeerListenerFailure) -> pure ()
    Just other -> assertFailure ("unexpected TCP terminal decision: " <> show other)

withThreeHeralds ::
  (HeraldTcp -> HeraldTcp -> HeraldTcp -> IO ()) ->
  IO (Either HeraldTcpFailure ())
withThreeHeralds use = do
  let listener = checked (tcpListenEndpoint "127.0.0.1" 0)
      retryDelay = checked (peerDialRetryDelayMicroseconds 1000)
  h3Result <-
    withHeraldTcpRuntime
      (heraldTcpConfiguration (runtimeConfiguration threeH3Genesis threeH3Bootstraps threeH3GeneratorSeedSource) listener listener listener [] retryDelay testHeartbeatConfiguration)
      ( \h3Tcp -> do
          let h3Endpoint = heraldTcpPeerEndpoint (heraldTcpEndpoints h3Tcp)
          h2Result <-
            withHeraldTcpRuntime
              ( heraldTcpConfiguration
                  (runtimeConfiguration threeH2Genesis threeH2Bootstraps threeH2GeneratorSeedSource)
                  listener
                  listener
                  listener
                  [seedFor h3Endpoint]
                  retryDelay
                  testHeartbeatConfiguration
              )
              ( \h2Tcp -> do
                  _ <- awaitOnlyCurrent h2Tcp threeH3HeraldEpoch
                  let h2Endpoint = heraldTcpPeerEndpoint (heraldTcpEndpoints h2Tcp)
                  h1Result <-
                    withHeraldTcpRuntime
                      ( heraldTcpConfiguration
                          (runtimeConfiguration threeH1Genesis threeH1Bootstraps threeH1GeneratorSeedSource)
                          listener
                          listener
                          listener
                          [seedFor h2Endpoint]
                          retryDelay
                          testHeartbeatConfiguration
                      )
                      (\h1Tcp -> use h1Tcp h2Tcp h3Tcp)
                  assertTcpResult "H1" h1Result
              )
          assertTcpResult "H2" h2Result
      )
  case h3Result of
    Left failure -> pure (Left failure)
    Right _ -> pure (Right ())

runtimeConfiguration ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  RuntimeGeneratorSeedSource ->
  HeraldRuntimeConfiguration
runtimeConfiguration genesis bootstraps seedSource =
  heraldRuntimeConfiguration
    genesis
    bootstraps
    threeOracleContacts
    seedSource
    systemRuntimeMonotonicClock
    (runtimePeerWorkDelayMicroseconds 0)
    (heraldRuntimeHandlers (const (pure ())))
    (checked (checkApplicationRecoveryConfiguration 30_000_000))
    (checked (checkPeerRecoveryConfiguration 30_000_000))

seedFor :: ResolvedTcpEndpoint -> ConfiguredPeerSeed
seedFor endpoint =
  checked (configuredPeerSeed (resolvedTcpHost endpoint) (resolvedTcpPort endpoint))

heraldTcpRuntime :: HeraldTcp -> HeraldRuntime
heraldTcpRuntime (HeraldTcp runtime _ _ _) = runtime

currentFor ::
  HeraldTcp ->
  HeraldEpoch ->
  IO [(PeerBinding, ConnectionRef PeerPlane)]
currentFor (HeraldTcp _ _ _ context) expectedRemote =
  filter ((== expectedRemote) . peerBindingRemoteHeraldEpoch . fst)
    <$> atomically (readTVar context.tcpCurrentPeerConnections)

awaitOnlyCurrent ::
  HeraldTcp ->
  HeraldEpoch ->
  IO (PeerBinding, ConnectionRef PeerPlane)
awaitOnlyCurrent (HeraldTcp _ _ _ context) expectedRemote =
  atomically $ do
    current <- readTVar context.tcpCurrentPeerConnections
    case filter ((== expectedRemote) . peerBindingRemoteHeraldEpoch . fst) current of
      [lease] -> pure lease
      _ -> retry

awaitConvergedReplacement ::
  HeraldTcp ->
  HeraldTcp ->
  PeerBinding ->
  IO
    ( (PeerBinding, ConnectionRef PeerPlane),
      (PeerBinding, ConnectionRef PeerPlane)
    )
awaitConvergedReplacement
  (HeraldTcp _ _ _ h1Context)
  (HeraldTcp _ _ _ h3Context)
  predecessor =
    atomically $ do
      h1Current <- readTVar h1Context.tcpCurrentPeerConnections
      h3Current <- readTVar h3Context.tcpCurrentPeerConnections
      case (filter h1Matches h1Current, filter h3Matches h3Current) of
        ([h1Lease@(h1Binding, _)], [h3Lease@(h3Binding, _)])
          | peerBindingSelectedCandidate h1Binding
              == peerBindingSelectedCandidate h3Binding ->
              pure (h1Lease, h3Lease)
        _ -> retry
    where
      h1Matches (binding, _) =
        binding /= predecessor
          && peerBindingRemoteHeraldEpoch binding == threeH3HeraldEpoch
      h3Matches (binding, _) =
        peerBindingRemoteHeraldEpoch binding == threeH1HeraldEpoch

submitKnownTarget ::
  HeraldTcp ->
  (PeerBinding, ConnectionRef PeerPlane) ->
  Set.Set PeerAddress ->
  IO ()
submitKnownTarget (HeraldTcp runtime _ _ _) (binding, reference) addresses = do
  submitted <-
    submitPeerControl
      runtime
      reference
      binding
      (PeerKnownHeralds [knownHerald threeH3HeraldId threeH3HeraldEpoch addresses])
  assertEqual "the owner accepts the injected current-binding contact update" Queued submitted

threeH3HeraldId :: HeraldId
threeH3HeraldId = checked (mkHeraldId (identifierBytes 4))

identifierBytes :: Word8 -> ByteString
identifierBytes seed =
  ByteString.pack
    [ seed + fromIntegral (index * 37 + index * index)
    | index <- [(0 :: Int) .. 31]
    ]

assertNoCurrentPeers :: String -> HeraldTcp -> Assertion
assertNoCurrentPeers context (HeraldTcp _ _ _ tcpContext) = do
  current <- atomically (readTVar tcpContext.tcpCurrentPeerConnections)
  assertEqual (context <> " did not install a current binding") [] current

sendTerminalPeerBytes :: ResolvedTcpEndpoint -> ByteString -> IO ()
sendTerminalPeerBytes endpoint bytes =
  bracket (connectTcpEndpoint endpoint) close $ \socketHandle -> do
    SocketBytes.sendAll socketHandle bytes
    ignoreIOException (shutdown socketHandle ShutdownSend)
    observed <- timeout 1000000 (try (SocketBytes.recv socketHandle 1) :: IO (Either IOException ByteString))
    case observed of
      Nothing -> assertFailure "invalid peer socket did not close"
      Just (Left _) -> pure ()
      Just (Right chunk) -> assertEqual "invalid peer socket reaches EOF" ByteString.empty chunk

candidatePhaseControlFrame :: ByteString
candidatePhaseControlFrame =
  encodePeerFrame
    ( PeerControlEnvelope
        mempty
        (KnownHeraldsDto (checked (knownHeraldCollectionDto [])))
    )

data IdentityProxy = IdentityProxy
  { proxyEndpoint :: ResolvedTcpEndpoint,
    proxyAccepted :: MVar (),
    proxyRelease :: MVar (),
    proxyListener :: Socket,
    proxyThread :: ThreadId,
    proxyDone :: MVar ()
  }

withIdentityProxy ::
  ResolvedTcpEndpoint ->
  ResolvedTcpEndpoint ->
  (IdentityProxy -> IO result) ->
  IO result
withIdentityProxy destination validEndpoint =
  bracket (startIdentityProxy destination validEndpoint) stopIdentityProxy

startIdentityProxy :: ResolvedTcpEndpoint -> ResolvedTcpEndpoint -> IO IdentityProxy
startIdentityProxy destination validEndpoint = do
  (listener, endpoint) <- bindEarlierListener (peerAdvertisedAddress validEndpoint) 10000
  accepted <- newEmptyMVar
  release <- newEmptyMVar
  done <- newEmptyMVar
  thread <-
    forkFinally
      ( runIdentityProxy listener destination accepted release
          `finally` ignoreIOException (close listener)
      )
      (const (putMVar done ()))
  pure (IdentityProxy endpoint accepted release listener thread done)

bindEarlierListener :: PeerAddress -> Word16 -> IO (Socket, ResolvedTcpEndpoint)
bindEarlierListener upperBound port = do
  let endpoint = checked (tcpListenEndpoint "127.0.0.1" port)
  bound <- try (bindTcpListener endpoint) :: IO (Either IOException (Socket, ResolvedTcpEndpoint))
  case bound of
    Left _ -> bindEarlierListener upperBound (port + 1)
    Right result@(listener, resolved)
      | peerAdvertisedAddress resolved < upperBound -> pure result
      | otherwise -> do
          ignoreIOException (close listener)
          bindEarlierListener upperBound (port + 1)

stopIdentityProxy :: IdentityProxy -> IO ()
stopIdentityProxy proxy = do
  ignoreIOException (close proxy.proxyListener)
  killThread proxy.proxyThread
  void (readMVar proxy.proxyDone)

runIdentityProxy :: Socket -> ResolvedTcpEndpoint -> MVar () -> MVar () -> IO ()
runIdentityProxy listener destination accepted release =
  bracket (fst <$> accept listener) close $ \incoming ->
    bracket (connectTcpEndpoint destination) close $ \outgoing -> do
      putMVar accepted ()
      readMVar release
      relayBothDirections incoming outgoing

relayBothDirections :: Socket -> Socket -> IO ()
relayBothDirections left right = mask $ \restore -> do
  firstDone <- newEmptyMVar
  leftDone <- newEmptyMVar
  rightDone <- newEmptyMVar
  leftThread <- forkFinally (restore (copySocket left right)) (const (putMVar leftDone () >> void (tryPutMVar firstDone ())))
  rightThread <- forkFinally (restore (copySocket right left)) (const (putMVar rightDone () >> void (tryPutMVar firstDone ())))
  let cleanup = do
        ignoreIOException (close left)
        ignoreIOException (close right)
        killThread leftThread
        killThread rightThread
        void (readMVar leftDone)
        void (readMVar rightDone)
  restore (takeMVar firstDone) `finally` cleanup

copySocket :: Socket -> Socket -> IO ()
copySocket source destination = do
  result <- try loop :: IO (Either IOException ())
  case result of
    Left _ -> pure ()
    Right () -> pure ()
  where
    loop = do
      chunk <- SocketBytes.recv source 32768
      if ByteString.null chunk
        then ignoreIOException (shutdown destination ShutdownSend)
        else SocketBytes.sendAll destination chunk >> loop

ignoreIOException :: IO () -> IO ()
ignoreIOException action = void (try action :: IO (Either IOException ()))

interruptAtProbe :: (IO () -> IO ()) -> IO (Either SomeException ())
interruptAtProbe action = do
  reached <- newEmptyMVar
  release <- newEmptyMVar
  workerDone <- newEmptyMVar
  worker <-
    forkFinally
      (action (putMVar reached () >> uninterruptibleMask_ (takeMVar release)))
      (putMVar workerDone)
  takeMVar reached
  throwerDone <- newEmptyMVar
  thrower <- forkFinally (throwTo worker ThreadKilled) (putMVar throwerDone)
  blocked <- timeout 1000000 (awaitBlockedThrow thrower)
  case blocked of
    Nothing -> assertFailure "asynchronous-exception injector did not block on the masked target"
    Just () -> pure ()
  putMVar release ()
  result <- takeMVar workerDone
  void (takeMVar throwerDone)
  pure result

awaitBlockedThrow :: ThreadId -> IO ()
awaitBlockedThrow thread = do
  status <- threadStatus thread
  case status of
    ThreadBlocked _ -> pure ()
    ThreadRunning -> yield >> awaitBlockedThrow thread
    ThreadFinished -> assertFailure "asynchronous-exception injector finished before release"
    ThreadDied -> assertFailure "asynchronous-exception injector died before release"

assertThreadKilled :: String -> Either SomeException () -> Assertion
assertThreadKilled label = \case
  Left failure -> case fromException failure :: Maybe AsyncException of
    Just ThreadKilled -> pure ()
    other -> assertFailure (label <> " raised an unexpected exception: " <> show other)
  Right () -> assertFailure (label <> " ignored the pending asynchronous exception")

assertTcpResult :: (Show failure) => String -> Either failure result -> Assertion
assertTcpResult label result = case result of
  Left failure -> assertFailure (label <> " Herald TCP failed: " <> show failure)
  Right _ -> pure ()

testHeartbeatConfiguration :: HeartbeatConfiguration
testHeartbeatConfiguration =
  checked (heartbeatConfigurationMicroseconds 60_000_000 5_000_000)

shortCandidateHeartbeatConfiguration :: HeartbeatConfiguration
shortCandidateHeartbeatConfiguration =
  checked (heartbeatConfigurationMicroseconds 60_000_000 100_000)

awaitSocketEof :: Socket -> IO ()
awaitSocketEof socketHandle = do
  chunk <- SocketBytes.recv socketHandle 32768
  if ByteString.null chunk then pure () else awaitSocketEof socketHandle

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
