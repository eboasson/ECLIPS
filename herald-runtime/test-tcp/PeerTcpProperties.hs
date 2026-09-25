{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module PeerTcpProperties
  ( tests,
  )
where

import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    newTQueueIO,
    newTVarIO,
    orElse,
    readTQueue,
    readTVar,
    retry,
    writeTQueue,
    writeTVar,
  )
import Control.Exception (bracket)
import Control.Monad (unless, when)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle (heraldLocator)
import Eclips.Domain.Disappearance qualified as Disappearance
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Topology qualified as Topology
import Eclips.Herald.Discovery
  ( PeerBinding,
    connectionNonce,
    peerBindingRemoteHeraldEpoch,
    peerHelloSystemId,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.Peer.RPC (projectPeerHello)
import Eclips.Herald.Runtime
  ( DiagnosticChecks (..),
    HeraldRuntimeConfiguration,
    RuntimeGeneratorSeedSource,
    checkApplicationRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
    configureHeraldRuntimeDiagnosticChecks,
    heraldRuntimeConfiguration,
    heraldRuntimeDiagnosticChecks,
    runtimePeerWorkDelayMicroseconds,
    systemRuntimeMonotonicClock,
  )
import Eclips.Herald.Runtime.Handler (heraldRuntimeHandlers)
import Eclips.Herald.Runtime.TCP
  ( configureHeraldTcpAdvertisedEndpoints,
    configureHeraldTcpDiscovery,
    configuredPeerSeed,
    exchangeHeraldDiscovery,
    heartbeatConfigurationMicroseconds,
    heraldTcpApplicationEndpoint,
    heraldTcpConfiguration,
    heraldTcpEndpoints,
    heraldTcpPeerEndpoint,
    peerDialRetryDelayMicroseconds,
    resolvedTcpEndpoint,
    resolvedTcpHost,
    resolvedTcpPort,
    tcpListenEndpoint,
    withHeraldTcpRuntime,
  )
import Eclips.Herald.Runtime.TCP.Internal.Facade (configureDiagnosticChecksSelection)
import Eclips.Herald.Runtime.TCP.Internal.Heartbeat
  ( setHeartbeatProbeForTest,
    setHeartbeatSchedulerForTest,
  )
import Eclips.Herald.Runtime.TCP.Internal.Socket (connectTcpEndpoint)
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( HeartbeatLane (PeerHeartbeatLane),
    HeartbeatProbeEvent (..),
    HeartbeatProbePhase (..),
    HeartbeatScheduler (..),
    HeraldTcp (..),
    TcpContext (..),
  )
import Eclips.Protocol.Peer.Frame
  ( PeerFrameContinuation (..),
    PeerFrameFeedResult (..),
    encodePeerFrame,
    feedPeerFrame,
    initialPeerFrameDecoder,
  )
import Eclips.Protocol.Peer.Types qualified as Protocol
import Eclips.Public.Types.ReceiptRetirement qualified as Receipt
import Network.Socket (Socket, close)
import Network.Socket.ByteString qualified as SocketBytes
import PairRuntimeFixtures
  ( pairLocalBootstraps,
    pairLocalGeneratorSeedSource,
    pairLocalGenesis,
    pairLocalHeraldEpoch,
    pairRemoteBootstraps,
    pairRemoteGeneratorSeedSource,
    pairRemoteGenesis,
    pairRemoteHello,
    pairRemoteHeraldEpoch,
  )
import RuntimeFixtures (fixtureOracleContacts)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "peer TCP"
    [ testCase "diagnostic option selects the runtime policy and rejects misspellings" caseDiagnosticSelection,
      testCase "configured seed establishes an admitted symmetric EPRP binding" caseConfiguredSeed,
      testCase "pre-Open ordered marker repairs once and an unknown alignment subscription rejects" caseDisappearanceMarkerReconnect,
      testCase "EPRP Hello advertises configured peer and application contacts" caseAdvertisedContacts,
      testCase "restricted discovery accepts unknown identities without creating peer bindings" caseRestrictedDiscovery
    ]

caseDiagnosticSelection :: Assertion
caseDiagnosticSelection = do
  let configuration = runtimeConfiguration pairLocalGenesis pairLocalBootstraps pairLocalGeneratorSeedSource
      selected input configured = heraldRuntimeDiagnosticChecks <$> configureDiagnosticChecksSelection input configured
  assertEqual "unset preserves checked default" (Right DiagnosticChecksEnabled) (selected Nothing configuration)
  assertEqual "explicit on" (Right DiagnosticChecksEnabled) (selected (Just "on") configuration)
  assertEqual "explicit off" (Right DiagnosticChecksDisabled) (selected (Just "off") configuration)
  assertEqual "unset preserves typed override" (Right DiagnosticChecksDisabled) (selected Nothing (configureHeraldRuntimeDiagnosticChecks DiagnosticChecksDisabled configuration))
  assertEqual "environment explicitly overrides typed policy" (Right DiagnosticChecksEnabled) (selected (Just "on") (configureHeraldRuntimeDiagnosticChecks DiagnosticChecksDisabled configuration))
  mapM_
    ( \value -> case selected (Just value) configuration of
        Left _ -> pure ()
        Right _ -> assertFailure ("invalid diagnostic option accepted: " <> show value)
    )
    ["", "false", "of", "ON"]

caseAdvertisedContacts :: Assertion
caseAdvertisedContacts = mapM_ run [(application, peer) | application <- [False, True], peer <- [False, True]]
  where
    run (advertiseApplication, advertisePeer) = do
      let listener = checked (tcpListenEndpoint "127.0.0.1" 0)
          applicationContact = if advertiseApplication then Just (checked (resolvedTcpEndpoint "application.example" 48101)) else Nothing
          peerContact = if advertisePeer then Just (checked (resolvedTcpEndpoint "peer.example" 48102)) else Nothing
          runtime = runtimeConfiguration pairLocalGenesis pairLocalBootstraps pairLocalGeneratorSeedSource
          configuration = configureHeraldTcpDiscovery (\_ _ -> pure Nothing) (configureHeraldTcpAdvertisedEndpoints applicationContact peerContact (heraldTcpConfiguration runtime listener listener listener [] (checked (peerDialRetryDelayMicroseconds 1000)) (checked (heartbeatConfigurationMicroseconds 1_000_000 5_000_000))))
          remoteHello = checked (projectPeerHello (Membership.heraldMembershipGenerationId markerMembership) (Membership.heraldMembershipGenerationActiveMemberSetDigest markerMembership) (pairRemoteHello (connectionNonce 987)))
      observed <- timeout 5_000_000 $ withHeraldTcpRuntime configuration $ \tcp ->
        bracket (connectTcpEndpoint (heraldTcpPeerEndpoint (heraldTcpEndpoints tcp))) close $ \connection -> do
          SocketBytes.sendAll connection (encodePeerFrame remoteHello)
          hello <- receiveHello connection initialPeerFrameDecoder
          let bound = heraldTcpEndpoints tcp
              expectedApplication = maybe (heraldTcpApplicationEndpoint bound) id applicationContact
              expectedPeer = maybe (heraldTcpPeerEndpoint bound) id peerContact
              expectedAddress = "tcp://" <> resolvedTcpHost expectedPeer <> ":" <> fromString (show (resolvedTcpPort expectedPeer))
          assertEqual "each peer contact uses its advertisement or its actual ephemeral bound endpoint" [Protocol.peerAddressDto expectedAddress] (Protocol.peerHelloAdvertisedAddresses hello)
          assertEqual "application contact falls back independently of the peer contact" (Just (checked (heraldLocator (resolvedTcpHost expectedApplication) (resolvedTcpPort expectedApplication)))) (Protocol.peerHelloApplicationLocator hello)
      case observed of
        Just (Right _) -> pure ()
        other -> assertFailure ("advertised endpoint scenario " <> show (advertiseApplication, advertisePeer) <> " failed: " <> show other)

    receiveHello connection decoder = do
      chunk <- SocketBytes.recv connection 32768
      if ByteString.null chunk
        then assertFailure "peer closed before Hello"
        else case feedPeerFrame decoder chunk of
          PeerFrameFeedResult frames continuation -> case [hello | Protocol.PeerHelloEnvelope hello <- frames] of
            hello : _ -> pure hello
            [] -> case continuation of
              NeedPeerFrameBytes next -> receiveHello connection next
              PeerFrameFailed failure -> assertFailure (show failure)

caseRestrictedDiscovery :: Assertion
caseRestrictedDiscovery = do
  let listener = checked (tcpListenEndpoint "127.0.0.1" 0)
      runtime = runtimeConfiguration pairLocalGenesis pairLocalBootstraps pairLocalGeneratorSeedSource
      configuration =
        configureHeraldTcpDiscovery
          (\_ bytes -> pure (Just ("reply:" <> bytes)))
          (heraldTcpConfiguration runtime listener listener listener [] (checked (peerDialRetryDelayMicroseconds 1000)) (checked (heartbeatConfigurationMicroseconds 1_000_000 5_000_000)))
  observed <- timeout 5_000_000 $ withHeraldTcpRuntime configuration $ \tcp@(HeraldTcp _ _ _ context) -> do
    let target = heraldTcpPeerEndpoint (heraldTcpEndpoints tcp)
    first <- exchangeHeraldDiscovery target "unknown-applicant"
    second <- exchangeHeraldDiscovery target "same-request"
    assertEqual "identity-free request reaches only the restricted handler" (Right "reply:unknown-applicant") first
    assertEqual "a new physical connection can retry the exact payload" (Right "reply:same-request") second
    contacts <- atomically (readTVar context.tcpCurrentPeerContacts)
    bindings <- atomically (readTVar context.tcpCurrentPeerConnections)
    assertEqual "no admitted ordinary peer identity" [] contacts
    assertEqual "no active peer binding" [] bindings
  case observed of
    Just (Right _) -> pure ()
    other -> assertFailure ("restricted discovery failed: " <> show other)

caseConfiguredSeed :: Assertion
caseConfiguredSeed = do
  let listener = checked (tcpListenEndpoint "127.0.0.1" 0)
      remoteConfiguration = runtimeConfiguration pairRemoteGenesis pairRemoteBootstraps pairRemoteGeneratorSeedSource
      retryDelay = checked (peerDialRetryDelayMicroseconds 1000)
      heartbeatConfiguration = checked (heartbeatConfigurationMicroseconds 100_000 1_000_000)
  observed <-
    timeout 5000000
      $ withHeraldTcpRuntime
        (heraldTcpConfiguration remoteConfiguration listener listener listener [] retryDelay heartbeatConfiguration)
        ( \remoteTcp -> do
            remoteHeartbeat <- newTQueueIO
            remoteDeadlines <- newTQueueIO
            let scheduler microseconds = do
                  elapsed <- newTVarIO False
                  atomically (writeTQueue remoteDeadlines (microseconds, elapsed))
                  pure elapsed
                observe event = do
                  atomically (writeTQueue remoteHeartbeat event)
            setHeartbeatProbeForTest remoteTcp (Just observe)
            setHeartbeatSchedulerForTest remoteTcp (HeartbeatScheduler scheduler)
            let remoteEndpoint = heraldTcpPeerEndpoint (heraldTcpEndpoints remoteTcp)
                seed =
                  checked
                    (configuredPeerSeed (resolvedTcpHost remoteEndpoint) (resolvedTcpPort remoteEndpoint))
                localConfiguration = runtimeConfiguration pairLocalGenesis pairLocalBootstraps pairLocalGeneratorSeedSource
            localResult <-
              withHeraldTcpRuntime
                (heraldTcpConfiguration localConfiguration listener listener listener [seed] retryDelay heartbeatConfiguration)
                ( \localTcp -> do
                    _ <- awaitAcceptedBinding localTcp pairRemoteHeraldEpoch
                    _ <- awaitAcceptedBinding remoteTcp pairLocalHeraldEpoch
                    forceHeartbeatRoundTrip remoteDeadlines remoteHeartbeat
                    pure ()
                )
            setHeartbeatProbeForTest remoteTcp Nothing
            case localResult of
              Left failure -> assertFailure ("local Herald TCP failed: " <> show failure)
              Right _ -> pure ()
        )
  case observed of
    Nothing -> assertFailure "peer TCP handshake did not terminate"
    Just (Left failure) -> assertFailure ("remote Herald TCP failed: " <> show failure)
    Just (Right _) -> pure ()

-- The producer uses the current wire DTOs; the consumer is the complete TCP
-- runtime, RPC bridge, and serialized Herald owner. The ordered marker
-- deliberately outruns Oracle Open; an alignment marker additionally needs
-- the receiver-owned subscription before its source may retain it.
caseDisappearanceMarkerReconnect :: Assertion
caseDisappearanceMarkerReconnect = do
  let listener = checked (tcpListenEndpoint "127.0.0.1" 0)
      configuration = runtimeConfiguration pairLocalGenesis pairLocalBootstraps pairLocalGeneratorSeedSource
      retryDelay = checked (peerDialRetryDelayMicroseconds 1000)
      heartbeat = checked (heartbeatConfigurationMicroseconds 1_000_000 5_000_000)
  observed <-
    timeout 10_000_000
      $ withHeraldTcpRuntime
        (heraldTcpConfiguration configuration listener listener listener [] retryDelay heartbeat)
        ( \tcp -> do
            let endpoint = heraldTcpPeerEndpoint (heraldTcpEndpoints tcp)
                sendAttempt nonce rejectUnknownSubscription = bracket (connectTcpEndpoint endpoint) close $ \socketHandle -> do
                  SocketBytes.sendAll socketHandle (encodePeerFrame (helloEnvelope nonce))
                  _ <- awaitAcceptedBinding tcp pairRemoteHeraldEpoch
                  SocketBytes.sendAll
                    socketHandle
                    (encodePeerFrame publicationMarkerEnvelope)
                  awaitPendingMarkerAcknowledgement socketHandle
                  when rejectUnknownSubscription $ do
                    SocketBytes.sendAll socketHandle (encodePeerFrame alignmentMarkerEnvelope)
                    awaitAlignmentMarkerRejection socketHandle
            sendAttempt 501 False
            awaitNoBinding tcp
            -- Same probe, direction, sequence, coordinate, and digest.
            -- Reconnection repairs transport loss without allocating new cut work.
            sendAttempt 502 True
        )
  case observed of
    Nothing -> assertFailure "disappearance marker TCP repair did not terminate"
    Just (Left failure) -> assertFailure ("disappearance marker TCP runtime failed: " <> show failure)
    Just (Right _) -> pure ()
  where
    helloEnvelope nonce =
      checked
        ( projectPeerHello
            (Membership.heraldMembershipGenerationId markerMembership)
            (Membership.heraldMembershipGenerationActiveMemberSetDigest markerMembership)
            (pairRemoteHello (connectionNonce nonce))
        )
    awaitNoBinding (HeraldTcp _ _ _ context) = atomically $ do
      bindings <- readTVar context.tcpCurrentPeerConnections
      if any ((== pairRemoteHeraldEpoch) . peerBindingRemoteHeraldEpoch . fst) bindings
        then retry
        else pure ()

awaitPendingMarkerAcknowledgement :: Socket -> IO ()
awaitPendingMarkerAcknowledgement socketHandle = go initialPeerFrameDecoder
  where
    go decoder = do
      bytes <- SocketBytes.recv socketHandle 65536
      if ByteString.null bytes
        then assertFailure "the live TCP peer rejected a pre-Open disappearance marker"
        else case feedPeerFrame decoder bytes of
          PeerFrameFeedResult frames (NeedPeerFrameBytes next) -> do
            completed <- consume frames
            if completed then pure () else go next
          other -> assertFailure ("invalid marker acknowledgement frame: " <> show other)
    consume [] = pure False
    consume (frame : rest) = case frame of
      Protocol.PeerControlEnvelope _ (Protocol.StreamReceivedDto direction _)
        | direction == markerDirection ->
            assertFailure "completion progress must not be preceded by a redundant Received"
      Protocol.PeerControlEnvelope _ (Protocol.StreamCompletedDto direction progress)
        | direction == markerDirection -> do
            let ordinal = Protocol.positiveStreamSequenceDtoWord64 markerSequence
            assertEqual
              "completion metadata covers the received marker position"
              (Just ordinal)
              (Receipt.receiptRetirementHighWater progress)
            assertEqual
              "unknown Open keeps the exact marker pending"
              (Set.singleton ordinal)
              (Receipt.receiptRetirementExceptions progress)
            assertEqual
              "unknown Open does not release the marker's semantic obligation"
              False
              (Receipt.receiptIsRetired ordinal progress)
            pure True
      Protocol.PeerHeartbeatEnvelope (Protocol.Ping nonce) -> do
        SocketBytes.sendAll
          socketHandle
          (encodePeerFrame (Protocol.PeerHeartbeatEnvelope (Protocol.Pong nonce)))
        consume rest
      _ -> consume rest

-- The envelope and probe digest are valid, but this peer has never received
-- this destination subscription. Rejection belongs to the established binding,
-- not the wire codec and not the complete runtime.
awaitAlignmentMarkerRejection :: Socket -> IO ()
awaitAlignmentMarkerRejection socketHandle = do
  bytes <- SocketBytes.recv socketHandle 65536
  unless (ByteString.null bytes) (awaitAlignmentMarkerRejection socketHandle)

markerMembership :: Membership.HeraldMembershipGeneration
markerMembership =
  checked
    ( Membership.genesisHeraldMembershipGeneration
        (peerHelloSystemId (pairRemoteHello (connectionNonce 501)))
        (pairLocalHeraldEpoch :| [pairRemoteHeraldEpoch])
    )

markerProbe :: Disappearance.DisappearanceProbeId
markerProbe = checked (Disappearance.deriveDisappearanceProbeId (Identity.controlIndex 7))

markerProbeDto :: Protocol.DisappearanceProbeIdDto
markerProbeDto =
  checked
    ( Protocol.disappearanceProbeIdDto
        (checked (Protocol.disappearanceProbeDigestClaim (Disappearance.disappearanceProbeIdBytes markerProbe)))
        (Protocol.controlIndexDto 7)
    )

markerDirection :: Protocol.StreamDirectionDto
markerDirection =
  checked
    ( Protocol.streamDirectionDto
        (checked (Protocol.heraldEpochClaim (Identity.heraldEpochBytes pairRemoteHeraldEpoch)))
        (checked (Protocol.heraldEpochClaim (Identity.heraldEpochBytes pairLocalHeraldEpoch)))
    )

markerSequence :: Protocol.PositiveStreamSequenceDto
markerSequence = checked (Protocol.positiveStreamSequenceDto 1)

publicationMarkerEnvelope :: Protocol.PeerEnvelope
publicationMarkerEnvelope =
  Protocol.PeerPublicationEnvelope
    mempty
    ( Protocol.PeerDisappearanceProbeMarkerDto
        markerDirection
        markerSequence
        (checked (Protocol.peerItemDigestClaim digest))
        (Protocol.DisappearanceProbeMarkerDto markerProbeDto coordinate markerDirection markerSequence)
    )
  where
    subject = ByteString.replicate 32 0x76
    generation =
      Membership.heraldMembershipGenerationIdBytes
        (Membership.heraldMembershipGenerationId markerMembership)
    members =
      Topology.memberSetDigestBytes
        (Membership.heraldMembershipGenerationActiveMemberSetDigest markerMembership)
    coordinate =
      Protocol.DisappearanceCoordinateDto
        (checked (Protocol.disappearanceSubjectDigestClaim subject))
        (checked (Protocol.heraldMembershipGenerationClaim generation))
        (checked (Protocol.memberSetDigestClaim members))
    domain = "ECLIPS-DISAPPEARANCE-PUBLICATION-MARKER"
    transcript =
      LazyByteString.toStrict
        ( Builder.toLazyByteString
            ( Builder.word64BE (fromIntegral (ByteString.length domain))
                <> Builder.byteString domain
                <> Builder.byteString (Disappearance.disappearanceProbeIdBytes markerProbe)
                <> foldMap
                  Builder.byteString
                  [ subject,
                    generation,
                    members,
                    Identity.heraldEpochBytes pairRemoteHeraldEpoch,
                    Identity.heraldEpochBytes pairLocalHeraldEpoch
                  ]
                <> Builder.word64BE 1
            )
        )
    digest = SHA256.hash transcript

alignmentMarkerEnvelope :: Protocol.PeerEnvelope
alignmentMarkerEnvelope =
  Protocol.PeerControlEnvelope
    mempty
    ( Protocol.AlignmentControlDto
        ( Protocol.AlignmentProbeMarkerDto
            markerProbeDto
            ( Protocol.AlignmentSubscriptionIdDto
                (checked (Protocol.heraldEpochClaim (Identity.heraldEpochBytes pairLocalHeraldEpoch)))
                (checked (Protocol.positiveAlignmentSubscriptionSequenceDto 1))
            )
            (Protocol.storeRevisionDto 0)
        )
    )

runtimeConfiguration ::
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  RuntimeGeneratorSeedSource ->
  HeraldRuntimeConfiguration
runtimeConfiguration genesis bootstraps seedSource =
  heraldRuntimeConfiguration
    genesis
    bootstraps
    fixtureOracleContacts
    seedSource
    systemRuntimeMonotonicClock
    (runtimePeerWorkDelayMicroseconds 0)
    (heraldRuntimeHandlers (const (pure ())))
    (checked (checkApplicationRecoveryConfiguration 30_000_000))
    (checked (checkPeerRecoveryConfiguration 30_000_000))

awaitAcceptedBinding :: HeraldTcp -> HeraldEpoch -> IO PeerBinding
awaitAcceptedBinding (HeraldTcp _ _ _ context) expected =
  atomically $ do
    bindings <- fmap fst <$> readTVar context.tcpCurrentPeerConnections
    case find ((== expected) . peerBindingRemoteHeraldEpoch) bindings of
      Nothing -> retry
      Just binding -> pure binding

forceHeartbeatRoundTrip :: TQueue (Word64, TVar Bool) -> TQueue HeartbeatProbeEvent -> IO ()
forceHeartbeatRoundTrip deadlines events = do
  nonce <- releaseUntilPing =<< atomically (readTQueue deadlines)
  awaitPong nonce
  where
    releaseUntilPing (delay, elapsed) = do
      if delay == 1_000_000
        then do
          -- The candidate deadline token remains intentionally inert after
          -- establishment; the finite worker has already returned.
          releaseUntilPing =<< atomically (readTQueue deadlines)
        else do
          assertEqual "the real EPRP idle uses the checked interval" 100_000 delay
          atomically (writeTVar elapsed True)
          awaitPingOrReplacement
    awaitPingOrReplacement = do
      next <-
        atomically
          ( (Left <$> readTQueue events)
              `orElse` (Right <$> readTQueue deadlines)
          )
      case next of
        Left (HeartbeatProbeEvent PeerHeartbeatLane (HeartbeatPingWritten nonce)) -> pure nonce
        Left _ -> awaitPingOrReplacement
        Right replacement -> releaseUntilPing replacement
    awaitPong expected = do
      event <- atomically (readTQueue events)
      case event of
        HeartbeatProbeEvent PeerHeartbeatLane (HeartbeatPongReceived nonce _)
          | nonce == expected -> pure ()
        _ -> awaitPong expected

checked :: (Show problem) => Either problem value -> value
checked = either (error . show) id
