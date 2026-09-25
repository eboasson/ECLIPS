{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}

-- | Composite startup and lifecycle behind the public TCP facade.
module Eclips.Herald.Runtime.TCP.Internal.Facade
  ( HeraldRuntimeScope (..),
    PeerTransportGate,
    configureDiagnosticChecksSelection,
    withHeraldTcpRuntimeInternal,
    withHeraldTcpRuntimeInScopeInternal,
    heraldTcpPeerTransportGate,
    setHeraldTcpOracleActionProbe,
    setHeraldTcpOracleManagerProbe,
    closePeerTransportGate,
    openPeerTransportGate,
    peerTransportGateIsOpen,
    requestHeraldTcpDrainInternal,
    awaitHeraldTcpExitInternal,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkFinally,
    killThread,
  )
import Control.Concurrent.STM
  ( TMVar,
    atomically,
    modifyTVar',
    newEmptyTMVarIO,
    orElse,
    readTMVar,
    tryPutTMVar,
    writeTQueue,
    writeTVar,
  )
import Control.Exception
  ( IOException,
    SomeException,
    finally,
    mask,
    onException,
    throwIO,
    try,
  )
import Control.Monad
  ( forM_,
    void,
  )
import Data.List (intercalate)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle (heraldLocator)
import Eclips.Herald.Administration (AdminCorrelationId)
import Eclips.Herald.OracleClient (OracleClientAction)
import Eclips.Herald.Runtime
  ( DiagnosticChecks (..),
    HeraldRuntime,
    HeraldRuntimeConfiguration,
    awaitHeraldRuntimeExit,
    configureHeraldRuntimeDiagnosticChecks,
    configureHeraldRuntimeOracleActionSink,
    configureHeraldRuntimeOracleHealthSink,
    configureHeraldRuntimePeerDialCancellationSink,
    configureHeraldRuntimePeerDialSink,
    configureHeraldRuntimeWorkCountsSink,
    heraldRuntimeTakeoverTarget,
    requestHeraldRuntimeDrain,
    withHeraldRuntime,
  )
import Eclips.Herald.Runtime.Handler
  ( RuntimeLaneOffer (..),
    runtimeAdministrationConnectionHandlers,
  )
import Eclips.Herald.Runtime.Ingress
  ( RuntimeAdministrationRegistration (..),
    RuntimeSubmission (..),
    registerAdministrationConnection,
  )
import Eclips.Herald.Runtime.TCP.Internal.Administration
  ( runConfiguredAdministrationListener,
  )
import Eclips.Herald.Runtime.TCP.Internal.Application
  ( runApplicationListener,
  )
import Eclips.Herald.Runtime.TCP.Internal.Oracle (runOracleManager)
import Eclips.Herald.Runtime.TCP.Internal.OracleHealth (runOracleHealthManager)
import Eclips.Herald.Runtime.TCP.Internal.Peer
  ( cancelPeerDial,
    peerAdvertisedAddress,
    runConfiguredSeedManager,
    runLearnedDialRegistry,
    runPeerListener,
  )
import Eclips.Herald.Runtime.TCP.Internal.Scope
  ( closePeerTransportGate,
    newTcpContext,
    openPeerTransportGate,
    peerTransportGateIsOpen,
    requestTcpContextStop,
    spawnTcpChild,
    spawnTcpFiniteChild,
    spawnTcpTrackedChild,
    spawnTcpTrackedFiniteChild,
    stopTcpAdmission,
    stopTcpContext,
  )
import Eclips.Herald.Runtime.TCP.Internal.Socket (bindTcpListener)
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( ConfiguredPeerSeed (..),
    HeartbeatConfiguration,
    HeraldTcp (..),
    HeraldTcpConfiguration (..),
    HeraldTcpEndpoints (..),
    HeraldTcpFailure (..),
    OracleManagerProbeEvent,
    PeerDialRetryDelay,
    PeerTransportGate,
    ResolvedTcpEndpoint (..),
    TcpContext (..),
    TcpListenEndpoint,
  )
import Eclips.Herald.Runtime.Trace
  ( HeraldRuntimeExit,
    HeraldRuntimeFailure,
  )
import Eclips.Public.Types.Timing (deriveTimingPolicy)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket
  ( Socket,
    close,
  )
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

-- | One structured runtime scope supplied to the package-private TCP facade.
-- Its rank-2 result keeps lifecycle ownership uniform while allowing the
-- Step-14 test component to adapt private runtime hooks without making the
-- transport library depend on runtime internals.
newtype HeraldRuntimeScope
  = HeraldRuntimeScope
      ( forall result.
        HeraldRuntimeConfiguration ->
        (HeraldRuntime -> IO result) ->
        IO (Either HeraldRuntimeFailure (result, HeraldRuntimeExit))
      )

-- | Start the byte-free Herald runtime, then its application, configured
-- administration, and peer TCP listeners, and publish the composite handle
-- only after all three are ready.
withHeraldTcpRuntimeInternal ::
  HeraldTcpConfiguration ->
  (HeraldTcp -> IO result) ->
  IO (Either HeraldTcpFailure (result, HeraldRuntimeExit))
withHeraldTcpRuntimeInternal =
  withHeraldTcpRuntimeInScopeInternal (HeraldRuntimeScope withHeraldRuntime)

-- | Start the ordinary composite TCP scope through a package-private runtime
-- runner. The public facade always supplies 'withHeraldRuntime'; the Step-14
-- transport suite may supply the hook-configured equivalent.
withHeraldTcpRuntimeInScopeInternal ::
  HeraldRuntimeScope ->
  HeraldTcpConfiguration ->
  (HeraldTcp -> IO result) ->
  IO (Either HeraldTcpFailure (result, HeraldRuntimeExit))
withHeraldTcpRuntimeInScopeInternal
  (HeraldRuntimeScope runRuntime)
  (HeraldTcpConfiguration runtimeConfiguration applicationListen administrationListen peerListen seeds retryDelay applicationHeartbeat peerHeartbeat advertised discoveryHandler)
  use = do
    emptyContext <- newTcpContext
    checkedConfiguration <- configureDiagnosticChecksFromEnvironment runtimeConfiguration
    measuredConfiguration <- configureWorkCountsFromEnvironment checkedConfiguration
    let context = emptyContext {tcpDiscoveryHandler = discoveryHandler, tcpTimingPolicy = deriveTimingPolicy <$> heraldRuntimeTakeoverTarget runtimeConfiguration}
    let configuredRuntime =
          configureHeraldRuntimeOracleActionSink
            (atomically . writeTQueue context.tcpOracleActions)
            ( configureHeraldRuntimePeerDialCancellationSink
                (atomically . cancelPeerDial context)
                ( configureHeraldRuntimePeerDialSink
                    (atomically . writeTQueue context.tcpDialIntents)
                    (configureHeraldRuntimeOracleHealthSink (\roundNumber cadence claims contacts -> atomically (writeTQueue context.tcpOracleHealthRounds (roundNumber, cadence, claims, contacts))) measuredConfiguration)
                )
            )
    runtimeResult <-
      ( runRuntime configuredRuntime $ \runtime ->
          startTcpScope
            context
            runtime
            applicationListen
            administrationListen
            peerListen
            seeds
            retryDelay
            applicationHeartbeat
            peerHeartbeat
            advertised
            use
            `onException` requestTcpContextStop context
      )
        `finally` stopTcpContext context
    case runtimeResult of
      Left failure -> pure (Left (HeraldTcpRuntimeFailure failure))
      Right (tcpResult, runtimeExit) -> case tcpResult of
        Left failure -> pure (Left failure)
        Right result -> pure (Right (result, runtimeExit))

-- | Environment interpretation stays outside the pure kernel. An absent option
-- preserves the typed configuration; misspellings fail before runtime startup.
configureDiagnosticChecksFromEnvironment :: HeraldRuntimeConfiguration -> IO HeraldRuntimeConfiguration
configureDiagnosticChecksFromEnvironment configuration = do
  selected <- lookupEnv "ECLIPS_DIAGNOSTIC_CHECKS"
  either (ioError . userError) pure (configureDiagnosticChecksSelection selected configuration)

configureDiagnosticChecksSelection :: Maybe String -> HeraldRuntimeConfiguration -> Either String HeraldRuntimeConfiguration
configureDiagnosticChecksSelection selected configuration =
  case selected of
    Nothing -> Right configuration
    Just "on" -> Right (configureHeraldRuntimeDiagnosticChecks DiagnosticChecksEnabled configuration)
    Just "off" -> Right (configureHeraldRuntimeDiagnosticChecks DiagnosticChecksDisabled configuration)
    Just _ -> Left "ECLIPS_DIAGNOSTIC_CHECKS must be on or off"

-- Environment selection and filesystem output are deployment interpretation.
-- The typed runtime receives only an optional sink for one final finite census.
configureWorkCountsFromEnvironment :: HeraldRuntimeConfiguration -> IO HeraldRuntimeConfiguration
configureWorkCountsFromEnvironment configuration = do
  directory <- lookupEnv "ECLIPS_WORK_COUNTS_DIR"
  case directory of
    Just destination | not (null destination) -> do
      stamp <- getMonotonicTimeNSec
      pure (configureHeraldRuntimeWorkCountsSink (writeWorkCounts (destination <> "/herald-" <> show stamp <> ".json")) configuration)
    _ -> pure configuration

writeWorkCounts :: FilePath -> Bool -> [(String, Word64)] -> IO ()
writeWorkCounts path complete counts = do
  let document =
        "{\"owner\":\"herald\",\"complete\":"
          <> (if complete then "true" else "false")
          <> ",\"counts\":{"
          <> intercalate "," [show key <> ":" <> show count | (key, count) <- counts]
          <> "}}\n"
  written <- try (writeFile path document)
  case written of
    Right () -> pure ()
    Left (exception :: IOException) -> do
      _ <- try (hPutStrLn stderr ("Herald work-count output failed for " <> path <> ": " <> show exception)) :: IO (Either IOException ())
      pure ()

-- | Obtain the package-private reversible gate for only Herald-peer sockets.
heraldTcpPeerTransportGate :: HeraldTcp -> PeerTransportGate
heraldTcpPeerTransportGate (HeraldTcp _ _ _ context) =
  context.tcpPeerTransportGate

-- | Install or remove the package-private observation seam immediately before
-- the physical Oracle manager interprets one dequeued action. Ordinary TCP
-- runtimes leave it absent.
setHeraldTcpOracleActionProbe ::
  HeraldTcp ->
  Maybe (OracleClientAction -> IO ()) ->
  IO ()
setHeraldTcpOracleActionProbe (HeraldTcp _ _ _ context) probe =
  atomically (writeTVar context.tcpOracleActionProbe probe)

-- | Install or remove package-private Oracle pacing/progress attribution.
setHeraldTcpOracleManagerProbe ::
  HeraldTcp ->
  Maybe (OracleManagerProbeEvent -> IO ()) ->
  IO ()
setHeraldTcpOracleManagerProbe (HeraldTcp _ _ _ context) probe =
  atomically (writeTVar context.tcpOracleManagerProbe probe)

startTcpScope ::
  TcpContext ->
  HeraldRuntime ->
  TcpListenEndpoint ->
  TcpListenEndpoint ->
  TcpListenEndpoint ->
  [ConfiguredPeerSeed] ->
  PeerDialRetryDelay ->
  HeartbeatConfiguration ->
  HeartbeatConfiguration ->
  (Maybe ResolvedTcpEndpoint, Maybe ResolvedTcpEndpoint) ->
  (HeraldTcp -> IO result) ->
  IO (Either HeraldTcpFailure result)
startTcpScope context runtime applicationListen administrationListen peerListen seeds retryDelay applicationHeartbeat peerHeartbeat advertised use = do
  let configuredPeerAddresses =
        Set.fromList
          [ peerAdvertisedAddress endpoint
          | ConfiguredPeerSeed endpoint <- seeds
          ]
  applicationBound <- try (bindTcpListener applicationListen) :: IO (Either IOException (Socket, ResolvedTcpEndpoint))
  case applicationBound of
    Left _ -> pure (Left HeraldTcpApplicationListenerFailure)
    Right (applicationListener, applicationEndpoint) -> do
      administrationBound <-
        try (bindTcpListener administrationListen) ::
          IO (Either IOException (Socket, ResolvedTcpEndpoint))
      case administrationBound of
        Left _ -> do
          close applicationListener
          pure (Left HeraldTcpAdministrationListenerFailure)
        Right (administrationListener, administrationEndpoint) -> do
          peerBound <-
            try (bindTcpListener peerListen) ::
              IO (Either IOException (Socket, ResolvedTcpEndpoint))
          case peerBound of
            Left _ -> do
              close administrationListener
              close applicationListener
              pure (Left HeraldTcpPeerListenerFailure)
            Right (peerListener, peerEndpoint) -> do
              let (applicationContact@(ResolvedTcpEndpoint applicationHost applicationPort), peerContact) =
                    (maybe applicationEndpoint id (fst advertised), maybe peerEndpoint id (snd advertised))
                  configuredContext =
                    context
                      { tcpConfiguredPeerAddresses = configuredPeerAddresses,
                        tcpApplicationLocator = Just (either (error . show) id (heraldLocator applicationHost applicationPort))
                      }
              atomically
                $ modifyTVar'
                  configuredContext.tcpListeners
                  ( \listeners ->
                      peerListener
                        : administrationListener
                        : applicationListener
                        : listeners
                  )
              administration <-
                registerAdministrationConnection
                  runtime
                  (runtimeAdministrationConnectionHandlers (const (pure LaneOffered)) (pure ()))
              case administration of
                AdministrationConnectionRegistered administrationRef -> do
                  let spawn = void . spawnTcpChild configuredContext Nothing
                      spawnFinite =
                        void . spawnTcpFiniteChild configuredContext HeraldTcpScopeFailure
                      spawnPeerTracked = spawnTcpTrackedChild configuredContext Nothing
                      spawnPeerFiniteTracked =
                        spawnTcpTrackedFiniteChild configuredContext HeraldTcpScopeFailure
                      spawnDialJob =
                        spawnTcpFiniteChild configuredContext HeraldTcpScopeFailure
                      advertisedAddresses = Set.singleton (peerAdvertisedAddress peerContact)
                  void
                    ( spawnTcpChild
                        configuredContext
                        (Just HeraldTcpApplicationListenerFailure)
                        (runApplicationListener spawn spawnFinite configuredContext runtime applicationHeartbeat applicationContact applicationListener)
                    )
                  void
                    ( spawnTcpChild
                        configuredContext
                        (Just HeraldTcpAdministrationListenerFailure)
                        ( runConfiguredAdministrationListener
                            spawn
                            configuredContext
                            runtime
                            administrationListener
                        )
                    )
                  void
                    ( spawnTcpChild
                        configuredContext
                        (Just HeraldTcpPeerListenerFailure)
                        (runPeerListener spawnPeerTracked spawnPeerFiniteTracked configuredContext runtime advertisedAddresses peerHeartbeat peerListener)
                    )
                  void
                    ( spawnTcpChild
                        configuredContext
                        (Just HeraldTcpScopeFailure)
                        (runLearnedDialRegistry spawnDialJob spawnPeerTracked spawnPeerFiniteTracked configuredContext runtime advertisedAddresses retryDelay peerHeartbeat)
                    )
                  void
                    ( spawnTcpChild
                        configuredContext
                        (Just HeraldTcpScopeFailure)
                        (runOracleManager (spawnTcpChild configuredContext Nothing) configuredContext runtime retryDelay)
                    )
                  void
                    ( spawnTcpChild
                        configuredContext
                        (Just HeraldTcpScopeFailure)
                        (runOracleHealthManager configuredContext runtime)
                    )
                  forM_ seeds $ \seed ->
                    void
                      ( spawnTcpFiniteChild
                          configuredContext
                          HeraldTcpScopeFailure
                          (runConfiguredSeedManager spawnPeerTracked spawnPeerFiniteTracked configuredContext runtime advertisedAddresses retryDelay peerHeartbeat seed)
                      )
                  runTcpCallback
                    configuredContext
                    ( HeraldTcp
                        runtime
                        administrationRef
                        ( HeraldTcpEndpoints
                            applicationEndpoint
                            administrationEndpoint
                            peerEndpoint
                        )
                        configuredContext
                    )
                    use
                AdministrationRegistrationUnavailable -> do
                  requestTcpContextStop context
                  pure (Left HeraldTcpScopeFailure)
                AdministrationRegistrationStopped -> do
                  requestTcpContextStop context
                  pure (Left HeraldTcpScopeFailure)

runTcpCallback ::
  TcpContext ->
  HeraldTcp ->
  (HeraldTcp -> IO result) ->
  IO (Either HeraldTcpFailure result)
runTcpCallback context handle use = mask $ \restore -> do
  callbackResult <- newEmptyTMVarIO
  callbackThread <-
    forkFinally
      (restore (use handle))
      (atomically . void . tryPutTMVar callbackResult)
  first <-
    restore
      ( atomically
          ( (Right <$> readTMVar context.tcpFailure)
              `orElse` do
                callback <- readTMVar callbackResult
                writeTVar context.tcpAccepting False
                pure (Left callback)
          )
      )
      `onException` cancelAndStop callbackThread callbackResult context
  case first of
    Right failure -> do
      killThread callbackThread
      void (atomically (readTMVar callbackResult))
      requestTcpContextStop context
      pure (Left failure)
    Left callback -> do
      requestTcpContextStop context
      case callback of
        Left exception -> throwIO exception
        Right result -> pure (Right result)

cancelAndStop ::
  ThreadId ->
  TMVar (Either SomeException result) ->
  TcpContext ->
  IO ()
cancelAndStop callbackThread callbackResult context = do
  killThread callbackThread
  void (atomically (readTMVar callbackResult))
  requestTcpContextStop context

-- | Submit the existing administration drain and stop only new physical
-- admission once runtime coordination accepts the request.
requestHeraldTcpDrainInternal ::
  HeraldTcp ->
  AdminCorrelationId ->
  IO RuntimeSubmission
requestHeraldTcpDrainInternal (HeraldTcp runtime administration _ context) correlation = do
  result <- requestHeraldRuntimeDrain runtime administration correlation
  case result of
    Queued -> stopTcpAdmission context
    DrainRequestAlreadyPending -> stopTcpAdmission context
    ConnectionClosed -> pure ()
    StalePhysicalGeneration -> pure ()
    IngressClosedForDrain -> stopTcpAdmission context
    RuntimeStopped -> stopTcpAdmission context
  pure result

-- | Observe the Herald terminal result, then close and join every TCP-owned
-- child before exposing the composite exit.
awaitHeraldTcpExitInternal ::
  HeraldTcp ->
  IO (Either HeraldTcpFailure HeraldRuntimeExit)
awaitHeraldTcpExitInternal (HeraldTcp runtime _ _ context) = do
  result <- awaitHeraldRuntimeExit runtime
  stopTcpContext context
  pure $ case result of
    Left failure -> Left (HeraldTcpRuntimeFailure failure)
    Right runtimeExit -> Right runtimeExit
