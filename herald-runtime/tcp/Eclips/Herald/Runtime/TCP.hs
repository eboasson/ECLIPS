-- | Scoped application/peer TCP transport for one typed Herald runtime.
module Eclips.Herald.Runtime.TCP
  ( HeraldTcp,
    HeraldTcpConfiguration,
    HeraldTcpEndpoints,
    TcpListenEndpoint,
    TcpEndpointError (..),
    tcpListenEndpoint,
    ResolvedTcpEndpoint,
    resolvedTcpEndpoint,
    resolvedTcpHost,
    resolvedTcpPort,
    ConfiguredPeerSeed,
    configuredPeerSeed,
    configuredPeerSeedEndpoint,
    HeartbeatConfiguration,
    heartbeatConfigurationMicroseconds,
    heartbeatIdleMicroseconds,
    heartbeatReplyTimeoutMicroseconds,
    PeerDialRetryDelay,
    peerDialRetryDelayMicroseconds,
    HeraldTcpConfigurationError (..),
    HeraldTcpFailure (..),
    heraldTcpConfiguration,
    configureHeraldTcpTiming,
    configureHeraldTcpApplicationHeartbeat,
    configureHeraldTcpAdvertisedEndpoints,
    configureHeraldTcpDiscovery,
    exchangeHeraldDiscovery,
    exchangeHeraldTcpJoin,
    withHeraldTcpRuntime,
    heraldTcpEndpoints,
    heraldTcpRuntime,
    heraldTcpApplicationEndpoint,
    heraldTcpAdministrationEndpoint,
    heraldTcpPeerEndpoint,
    requestHeraldTcpDrain,
    awaitHeraldTcpExit,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word16, Word64)
import Eclips.Herald.Administration (AdminCorrelationId)
import Eclips.Herald.Runtime
  ( HeraldRuntime,
    HeraldRuntimeConfiguration,
    configureHeraldRuntimeTiming,
    exchangeHeraldJoin,
  )
import Eclips.Herald.Runtime.Ingress (RuntimeSubmission)
import Eclips.Herald.Runtime.TCP.Internal.Discovery (exchangeDiscovery)
import Eclips.Herald.Runtime.TCP.Internal.Facade
  ( awaitHeraldTcpExitInternal,
    requestHeraldTcpDrainInternal,
    withHeraldTcpRuntimeInternal,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( ConfiguredPeerSeed (..),
    HeartbeatConfiguration (..),
    HeartbeatIdleDelay (..),
    HeartbeatReplyTimeout (..),
    HeraldTcp (..),
    HeraldTcpConfiguration (..),
    HeraldTcpConfigurationError (..),
    HeraldTcpEndpoints (..),
    HeraldTcpFailure (..),
    PeerDialRetryDelay (..),
    ResolvedTcpEndpoint (..),
    TcpEndpointError (..),
    TcpListenEndpoint (..),
  )
import Eclips.Herald.Runtime.Trace (HeraldRuntimeExit)
import Eclips.Public.Types.Timing

-- | Check a numeric listener endpoint. Port zero requests an ephemeral port.
tcpListenEndpoint :: Text -> Word16 -> Either TcpEndpointError TcpListenEndpoint
tcpListenEndpoint host port
  | Text.null host = Left TcpEndpointHostEmpty
  | otherwise = Right (TcpListenEndpoint host port)

-- | Construct one nonzero numeric configured-seed endpoint.
configuredPeerSeed :: Text -> Word16 -> Either TcpEndpointError ConfiguredPeerSeed
configuredPeerSeed host port
  | Text.null host = Left TcpEndpointHostEmpty
  | port == 0 = Left TcpSeedPortZero
  | otherwise = Right (ConfiguredPeerSeed (ResolvedTcpEndpoint host port))

-- | Check a nonzero advertised endpoint; DNS resolution remains in the shell.
resolvedTcpEndpoint :: Text -> Word16 -> Either TcpEndpointError ResolvedTcpEndpoint
resolvedTcpEndpoint host port = configuredPeerSeedEndpoint <$> configuredPeerSeed host port

-- | Observe the identity-free endpoint carried by one configured seed.
configuredPeerSeedEndpoint :: ConfiguredPeerSeed -> ResolvedTcpEndpoint
configuredPeerSeedEndpoint (ConfiguredPeerSeed endpoint) = endpoint

-- | Observe the numeric host text.
resolvedTcpHost :: ResolvedTcpEndpoint -> Text
resolvedTcpHost (ResolvedTcpEndpoint host _) = host

-- | Observe the resolved nonzero port.
resolvedTcpPort :: ResolvedTcpEndpoint -> Word16
resolvedTcpPort (ResolvedTcpEndpoint _ port) = port

-- | Check the physical wait between unsuccessful peer or Oracle cycles.
peerDialRetryDelayMicroseconds ::
  Word64 -> Either HeraldTcpConfigurationError PeerDialRetryDelay
peerDialRetryDelayMicroseconds 0 = Left PeerDialRetryDelayMustBePositive
peerDialRetryDelayMicroseconds value = Right (PeerDialRetryDelay value)

-- | Check the idle and reply intervals for physical EAPP/EPRP heartbeat.
heartbeatConfigurationMicroseconds ::
  Word64 ->
  Word64 ->
  Either HeraldTcpConfigurationError HeartbeatConfiguration
heartbeatConfigurationMicroseconds 0 _ = Left HeartbeatIdleDelayMustBePositive
heartbeatConfigurationMicroseconds _ 0 = Left HeartbeatReplyTimeoutMustBePositive
heartbeatConfigurationMicroseconds idle reply =
  Right
    ( HeartbeatConfiguration
        (HeartbeatIdleDelay idle)
        (HeartbeatReplyTimeout reply)
    )

-- | Observe the checked physical-idle interval.
heartbeatIdleMicroseconds :: HeartbeatConfiguration -> Word64
heartbeatIdleMicroseconds
  (HeartbeatConfiguration (HeartbeatIdleDelay value) _) = value

-- | Observe the checked reply-timeout interval.
heartbeatReplyTimeoutMicroseconds :: HeartbeatConfiguration -> Word64
heartbeatReplyTimeoutMicroseconds
  (HeartbeatConfiguration _ (HeartbeatReplyTimeout value)) = value

-- | Construct the immutable composite startup configuration with an explicit
-- shared heartbeat policy. Application timing can then be overridden separately.
heraldTcpConfiguration ::
  HeraldRuntimeConfiguration ->
  TcpListenEndpoint ->
  TcpListenEndpoint ->
  TcpListenEndpoint ->
  [ConfiguredPeerSeed] ->
  PeerDialRetryDelay ->
  HeartbeatConfiguration ->
  HeraldTcpConfiguration
heraldTcpConfiguration runtime application administration peer seeds retry heartbeat =
  HeraldTcpConfiguration runtime application administration peer seeds retry heartbeat heartbeat (Nothing, Nothing) Nothing

-- | Resolve one takeover target into both kernel and transport timing before
-- starting any owner or socket worker. Explicit low-level constructors remain
-- available for controlled timing fixtures.
configureHeraldTcpTiming :: TakeoverTarget -> HeraldTcpConfiguration -> HeraldTcpConfiguration
configureHeraldTcpTiming target (HeraldTcpConfiguration runtime appListen adminListen peerListen seeds _ _ _ advertised discovery) =
  HeraldTcpConfiguration
    (configureHeraldRuntimeTiming target runtime)
    appListen
    adminListen
    peerListen
    seeds
    (PeerDialRetryDelay (timingReconnectInitialMicroseconds policy))
    (HeartbeatConfiguration (HeartbeatIdleDelay (timingHeartbeatIdleMicroseconds policy)) (HeartbeatReplyTimeout defaultApplicationReplyTimeoutMicroseconds))
    (HeartbeatConfiguration (HeartbeatIdleDelay (timingHeartbeatIdleMicroseconds policy)) (HeartbeatReplyTimeout (timingHeartbeatReplyMicroseconds policy)))
    advertised
    discovery
  where
    policy = deriveTimingPolicy target

-- | Override only application establishment and heartbeat timing. Apply after
-- 'configureHeraldTcpTiming' when using an explicit application-lane policy.
configureHeraldTcpApplicationHeartbeat :: HeartbeatConfiguration -> HeraldTcpConfiguration -> HeraldTcpConfiguration
configureHeraldTcpApplicationHeartbeat applicationHeartbeat (HeraldTcpConfiguration runtime appListen adminListen peerListen seeds retry _ peerHeartbeat advertised discovery) =
  HeraldTcpConfiguration runtime appListen adminListen peerListen seeds retry applicationHeartbeat peerHeartbeat advertised discovery

-- | Configure each contact independently. An omitted contact uses that
-- listener's actual bound endpoint, including its OS-assigned ephemeral port.
configureHeraldTcpAdvertisedEndpoints :: Maybe ResolvedTcpEndpoint -> Maybe ResolvedTcpEndpoint -> HeraldTcpConfiguration -> HeraldTcpConfiguration
configureHeraldTcpAdvertisedEndpoints application peer (HeraldTcpConfiguration runtime appListen adminListen peerListen seeds retry applicationHeartbeat peerHeartbeat _ discovery) =
  HeraldTcpConfiguration runtime appListen adminListen peerListen seeds retry applicationHeartbeat peerHeartbeat (application, peer) discovery

-- | Install the restricted discovery/onboarding interpreter. Payloads cross
-- the supplied owner boundary; the TCP family grants no ordinary peer binding.
configureHeraldTcpDiscovery :: (HeraldRuntime -> ByteString -> IO (Maybe ByteString)) -> HeraldTcpConfiguration -> HeraldTcpConfiguration
configureHeraldTcpDiscovery handler (HeraldTcpConfiguration runtime appListen adminListen peerListen seeds retry applicationHeartbeat peerHeartbeat advertised _) =
  HeraldTcpConfiguration runtime appListen adminListen peerListen seeds retry applicationHeartbeat peerHeartbeat advertised (Just handler)

exchangeHeraldDiscovery :: ResolvedTcpEndpoint -> ByteString -> IO (Either String ByteString)
exchangeHeraldDiscovery = exchangeDiscovery

-- | Start the Herald and its TCP children, run the callback after both listeners
-- are ready, then close and join the complete composite scope.
withHeraldTcpRuntime ::
  HeraldTcpConfiguration ->
  (HeraldTcp -> IO result) ->
  IO (Either HeraldTcpFailure (result, HeraldRuntimeExit))
withHeraldTcpRuntime = withHeraldTcpRuntimeInternal

-- | Route local onboarding through the TCP handle's single kernel owner.
exchangeHeraldTcpJoin :: HeraldTcp -> ByteString -> IO (Either RuntimeSubmission ByteString)
exchangeHeraldTcpJoin (HeraldTcp runtime _ _ _) = exchangeHeraldJoin runtime

-- | Observe both bound listener endpoints.
heraldTcpEndpoints :: HeraldTcp -> HeraldTcpEndpoints
heraldTcpEndpoints (HeraldTcp _ _ endpoints _) = endpoints

-- | Obtain the application listener endpoint.
heraldTcpApplicationEndpoint :: HeraldTcpEndpoints -> ResolvedTcpEndpoint
heraldTcpApplicationEndpoint (HeraldTcpEndpoints applicationEndpoint _ _) = applicationEndpoint

-- | Obtain the configured-process administration listener endpoint.
heraldTcpAdministrationEndpoint :: HeraldTcpEndpoints -> ResolvedTcpEndpoint
heraldTcpAdministrationEndpoint (HeraldTcpEndpoints _ administrationEndpoint _) =
  administrationEndpoint

-- | Obtain the Herald-peer listener endpoint.
heraldTcpPeerEndpoint :: HeraldTcpEndpoints -> ResolvedTcpEndpoint
heraldTcpPeerEndpoint (HeraldTcpEndpoints _ _ peerEndpoint) = peerEndpoint

-- | Submit the single existing semantic drain through the private local
-- administration lease and stop new TCP admission after it is accepted.
requestHeraldTcpDrain :: HeraldTcp -> AdminCorrelationId -> IO RuntimeSubmission
requestHeraldTcpDrain = requestHeraldTcpDrainInternal

-- | Await the Herald result and every TCP-owned child join.
awaitHeraldTcpExit :: HeraldTcp -> IO (Either HeraldTcpFailure HeraldRuntimeExit)
awaitHeraldTcpExit = awaitHeraldTcpExitInternal

-- | Scoped handle for owner-issued ingress and immutable status queries. The
-- kernel state remains private to the runtime's serialized worker.
heraldTcpRuntime :: HeraldTcp -> HeraldRuntime
heraldTcpRuntime (HeraldTcp runtime _ _ _) = runtime
