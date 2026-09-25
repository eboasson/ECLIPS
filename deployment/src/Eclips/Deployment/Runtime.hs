{-# LANGUAGE OverloadedStrings #-}

-- | Supervised composition through the existing public runtime boundaries.
-- The deployment owns scopes and endpoint descriptions, never kernel state.
module Eclips.Deployment.Runtime
  ( DeploymentRuntime,
    withDeploymentResident,
    withDeploymentResidentOracle,
    withJoiningResident,
    withJoiningResidentOracle,
    exchangeDeploymentJoin,
    deploymentSystemIdentity,
    deploymentResident,
    deploymentBoundEndpoints,
    deploymentEndpointLines,
    deploymentRuntimeTakeoverTarget,
    deploymentLauncherConnection,
    deploymentLauncherArtifact,
    requestDeploymentDrain,
    awaitDeploymentExit,
  )
where

import Control.Concurrent (forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.STM (TMVar, atomically, newEmptyTMVarIO, newTVarIO, putTMVar, readTMVar, readTVar, writeTVar)
import Control.Exception (AsyncException, SomeException, finally, fromException, mask, try)
import Control.Monad (unless)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import Eclips.Application.Connection (encodeConnectionDescriptor)
import Eclips.Application.Types.Lifecycle (ConnectionDescriptor, connectionDescriptorWithTiming, heraldLocator)
import Eclips.Deployment.Configuration qualified as Configuration
import Eclips.Deployment.Discovery qualified as Discovery
import Eclips.Deployment.Manifest
import Eclips.Deployment.Timing (takeoverTimingLines)
import Eclips.Domain.Identity (bootstrapManifestIdBytes, heraldEpochBytes, systemIdBytes)
import Eclips.Domain.Membership (heraldMembershipGenerationId)
import Eclips.Herald.Administration (adminCorrelationId)
import Eclips.Herald.Genesis (HeraldMember (..))
import Eclips.Herald.Isolation (checkIsolationConfiguration)
import Eclips.Herald.Join qualified as Join
import Eclips.Herald.OracleClient (OracleContactSet, oracleContact, oracleContactSet, oracleNodeClaim)
import Eclips.Herald.OracleClient qualified as OracleClient
import Eclips.Herald.Runtime
  ( HeraldRuntime,
    awaitHeraldOracleReplicaRegistration,
    checkApplicationRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
    configureHeraldRuntimeIsolation,
    configureHeraldRuntimeOracleVoters,
    exchangeHeraldJoin,
    heraldRuntimeConfiguration,
    readHeraldOracleStatus,
    runtimePeerWorkDelayMicroseconds,
    systemRuntimeGeneratorSeedSource,
    systemRuntimeMonotonicClock,
  )
import Eclips.Herald.Runtime.Handler (heraldRuntimeHandlers)
import Eclips.Herald.Runtime.Ingress (RuntimeSubmission (..), configureLocalOracleReplica, submitOracleContactHints)
import Eclips.Herald.Runtime.TCP qualified as Herald
import Eclips.Herald.Runtime.Trace (HeraldRuntimeExit (HeraldRuntimeDrained))
import Eclips.Oracle.Genesis (checkedOracleRaftVoterBindings, checkedOracleSystemId, raftVoterBindingNode)
import Eclips.Oracle.Genesis qualified as OracleGenesis
import Eclips.Oracle.Runtime (oracleRuntimeServiceReady)
import Eclips.Oracle.Runtime qualified as OracleRuntime
import Eclips.Oracle.Runtime.TCP qualified as Oracle
import Eclips.Oracle.Transition (initialOracle)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Public.Types.Timing qualified as Timing
import Eclips.Raft.Genesis qualified as Raft
import Eclips.Raft.Identity (RaftNodeId, mkRaftNodeId, raftNodeIdBytes)
import System.IO (hPutStrLn, stderr)

data DeploymentRuntime = DeploymentRuntime ResidentDeployment Herald.HeraldTcp (Maybe ConnectionDescriptor) ByteString Timing.TakeoverTarget

deploymentResident :: DeploymentRuntime -> ResidentDeployment
deploymentResident (DeploymentRuntime resident _ _ _ _) = resident
deploymentBoundEndpoints :: DeploymentRuntime -> Herald.HeraldTcpEndpoints
deploymentBoundEndpoints (DeploymentRuntime _ tcp _ _ _) = Herald.heraldTcpEndpoints tcp
deploymentEndpointLines :: DeploymentRuntime -> [Text.Text]
deploymentEndpointLines running =
  let endpoints = deploymentBoundEndpoints running
      line name value = name <> " " <> Configuration.renderEndpoint (resolvedHerald value)
   in [line "application" (Herald.heraldTcpApplicationEndpoint endpoints), line "administration" (Herald.heraldTcpAdministrationEndpoint endpoints), line "peer" (Herald.heraldTcpPeerEndpoint endpoints)] <> takeoverTimingLines (deploymentRuntimeTakeoverTarget running)

deploymentLauncherConnection :: DeploymentRuntime -> Maybe ConnectionDescriptor
deploymentLauncherConnection (DeploymentRuntime _ _ descriptor _ _) = descriptor

deploymentLauncherArtifact :: DeploymentRuntime -> Maybe Text.Text
deploymentLauncherArtifact = fmap encodeConnectionDescriptor . deploymentLauncherConnection

-- | Each resident has its own OS-local scope. Only the first resident hosts the
-- singleton Oracle role and receives the one checked genesis launcher.
withDeploymentResident :: CheckedDeployment -> Word64 -> (DeploymentRuntime -> IO value) -> IO value
withDeploymentResident = withDeploymentResidentOracle id

-- | Composition/test boundary for scoped Oracle runtime interpretations. The
-- production entry point uses the identity transformer; kernels stay pure.
withDeploymentResidentOracle :: (OracleRuntime.OracleRuntimeConfiguration -> OracleRuntime.OracleRuntimeConfiguration) -> CheckedDeployment -> Word64 -> (DeploymentRuntime -> IO value) -> IO value
withDeploymentResidentOracle configureOracle deployment ordinal use = do
  resident <- maybe (ioError (userError "member ordinal is absent from the fixed deployment")) pure (find ((== ordinal) . residentOrdinal) residents)
  localPeer <- newEmptyTMVarIO
  if ordinal == residentOrdinal founder
    then do
      let planes = residentEndpoints resident
      oracleListener <- checked (oracleListen (Configuration.oracleListen planes))
      raftListener <- checked (oracleListen (Configuration.raftListen planes))
      initial <- checked (OracleRuntime.oracleRuntimeConfiguration (deploymentRaftGenesis deployment) (deploymentOracleGenesisSource deployment) (heraldMemberEpoch (residentMember resident)) OracleRuntime.systemRuntimeElectionTimeoutSource)
      base <- checked (Oracle.oracleTcpClusterConfiguration [configureOracle (OracleRuntime.configureOracleRuntimeReplicaResolver (resolveReplica localPeer) initial)])
      configured <- checked (Oracle.configureOracleTcpListeners [(node, oracleListener, raftListener)] (Oracle.configureOracleTcpTiming (deploymentTakeoverTarget deployment) base))
      started <- Oracle.withOracleTcpCluster configured $ \cluster -> do
        contacts <- case Oracle.oracleTcpOracleContacts cluster of
          [(actualNode, actual)] -> checked (contactSet actualNode (fromMaybe (resolvedOracle actual) (Configuration.oracleAdvertised planes)))
          _ -> ioError (userError "checked founder did not start one Oracle voter")
        awaitOracleReady cluster
        let actualOracle = case Oracle.oracleTcpOracleContacts cluster of [(_, endpoint)] -> resolvedOracle endpoint; _ -> error "founder Oracle endpoint missing"
            actualRaft = case Oracle.oracleTcpRaftEndpoints cluster of [(_, endpoint)] -> resolvedOracle endpoint; _ -> error "founder Raft endpoint missing"
            local = (node, fromMaybe actualOracle (Configuration.oracleAdvertised planes), fromMaybe actualRaft (Configuration.raftAdvertised planes))
        runHerald localPeer (Just local) resident contacts
      either (ioError . userError . show) pure started
    else do
      contacts <- checked (contactSet node (fromMaybe (Configuration.oracleListen (residentEndpoints founder)) (Configuration.oracleAdvertised (residentEndpoints founder))))
      runHerald localPeer Nothing resident contacts
  where
    residents = NonEmpty.toList (deploymentResidents deployment)
    founder = NonEmpty.head (deploymentResidents deployment)
    node = case checkedOracleRaftVoterBindings (deploymentCheckedOracleGenesis deployment) of
      [binding] -> raftVoterBindingNode binding
      _ -> error "checked deployment has no singleton voter"
    runHerald localPeer hosted resident contacts = do
      seeds <- traverse (checked . seedFor) [other | other <- residents, residentOrdinal other /= ordinal]
      hints <- traverse contactFor [other | other <- residents, Configuration.endpointPort (peerEndpoint other) /= 0]
      withDeploymentHerald configureOracle deployment resident contacts seeds hints (ordinal == residentOrdinal founder) localPeer hosted use
    seedFor resident = let target = peerEndpoint resident in Herald.configuredPeerSeed (Configuration.endpointHost target) (Configuration.endpointPort target)

-- | Start a pending non-voter through its checked admission configuration.
-- The callback owns onboarding and declares readiness only after activation.
withJoiningResident :: CheckedDeployment -> ResidentDeployment -> OracleContactSet -> [Discovery.DiscoveryContact] -> (DeploymentRuntime -> IO value) -> IO value
withJoiningResident = withJoiningResidentOracle id

withJoiningResidentOracle :: (OracleRuntime.OracleRuntimeConfiguration -> OracleRuntime.OracleRuntimeConfiguration) -> CheckedDeployment -> ResidentDeployment -> OracleContactSet -> [Discovery.DiscoveryContact] -> (DeploymentRuntime -> IO value) -> IO value
withJoiningResidentOracle configureOracle deployment resident contacts hints use = do
  let endpoints = Set.toAscList (Discovery.discoveryPeerSeedLocators (residentMember resident) hints)
  seeds <- traverse (\target -> checked (Herald.configuredPeerSeed (Configuration.endpointHost target) (Configuration.endpointPort target))) endpoints
  localPeer <- newEmptyTMVarIO
  withDeploymentHerald configureOracle deployment resident contacts seeds hints False localPeer Nothing use

withDeploymentHerald :: (OracleRuntime.OracleRuntimeConfiguration -> OracleRuntime.OracleRuntimeConfiguration) -> CheckedDeployment -> ResidentDeployment -> OracleContactSet -> [Herald.ConfiguredPeerSeed] -> [Discovery.DiscoveryContact] -> Bool -> TMVar Configuration.Endpoint -> Maybe (RaftNodeId, Configuration.Endpoint, Configuration.Endpoint) -> (DeploymentRuntime -> IO value) -> IO value
withDeploymentHerald configureOracle deployment resident contacts seeds hints isFounder localPeer hosted use = do
  let planes = residentEndpoints resident
      genesis = residentHeraldGenesis resident
  application <- checked (listen (Configuration.applicationListen planes))
  administration <- checked (listen (Configuration.administrationListen planes))
  peer <- checked (listen (Configuration.peerListen planes))
  let target = deploymentTakeoverTarget deployment
      timing = Timing.deriveTimingPolicy target
  applicationRecovery <- checked (checkApplicationRecoveryConfiguration (Timing.timingApplicationRecoveryGraceMicroseconds timing))
  peerRecovery <- checked (checkPeerRecoveryConfiguration (Timing.timingPeerRecoveryGraceMicroseconds timing))
  isolation <- checked (checkIsolationConfiguration (Timing.timingIsolationGraceMicroseconds timing) Timing.defaultReadOnlyDrainMicroseconds)
  retryDelay <- checked (Herald.peerDialRetryDelayMicroseconds (Timing.timingReconnectInitialMicroseconds timing))
  heartbeat <- checked (Herald.heartbeatConfigurationMicroseconds (Timing.timingHeartbeatIdleMicroseconds timing) (Timing.timingHeartbeatReplyMicroseconds timing))
  initial <- checked (initialOracle (deploymentCheckedOracleGenesis deployment))
  let runtime =
        configureHeraldRuntimeOracleVoters (Voter.oracleVoterConfiguration initial) (Voter.oracleReplicaRegistrations initial)
          $ configureHeraldRuntimeIsolation
            (Set.singleton (heraldMemberEpoch (residentMember founder)))
            isolation
            (heraldRuntimeConfiguration genesis (residentBootstraps resident) contacts systemRuntimeGeneratorSeedSource systemRuntimeMonotonicClock (runtimePeerWorkDelayMicroseconds 0) (heraldRuntimeHandlers (const (pure ()))) applicationRecovery peerRecovery)
      base = Herald.configureHeraldTcpTiming target (Herald.heraldTcpConfiguration runtime application administration peer seeds retryDelay heartbeat)
  announcedApplication <- traverse (checked . resolved) (Configuration.applicationAdvertised planes)
  announcedPeer <- traverse (checked . resolved) (Configuration.peerAdvertised planes)
  observedContacts <- newTVarIO (Set.fromList hints)
  localContact <- newEmptyTMVarIO
  oracleHints <- traverse oracleHint (OracleClient.oracleContacts contacts)
  let responder owner =
        Discovery.serveDeploymentDiscovery
          ( \observed -> do
              local <- atomically (readTMVar localContact)
              cached <- atomically $ do
                previous <- readTVar observedContacts
                let next = Set.unions [previous, Set.fromList observed, Set.singleton local]
                writeTVar observedContacts next
                pure next
              status <- readRuntimeJoinStatus owner
              oracleStatus <- readHeraldOracleStatus owner >>= checked
              registeredHints <- traverse registrationOracleHint [registration | registration <- Join.heraldOracleStatusReplicas oracleStatus, Voter.replicaRegistrationContact registration /= Nothing]
              let combined = NonEmpty.fromList (Map.elems (Map.fromList [(Discovery.discoveryOracleNode hint, hint) | hint <- NonEmpty.toList oracleHints <> registeredHints]))
              _ <- submitOracleContactHints owner (checkedOracleContacts combined)
              pure
                ( Discovery.discoverySnapshot
                    deployment
                    (heraldMembershipGenerationId (Join.joinStatusMembership status))
                    (Join.joinStatusControl status)
                    (Set.toAscList cached)
                    combined
                )
          )
          (\bytes -> either (const Nothing) Just <$> exchangeHeraldJoin owner bytes)
          ( \node -> do
              status <- readHeraldOracleStatus owner
              pure (either (const Nothing) (find ((== node) . Voter.replicaRegistrationNode) . Join.heraldOracleStatusReplicas) status)
          )
      configured =
        Herald.configureHeraldTcpDiscovery
          responder
          (Herald.configureHeraldTcpAdvertisedEndpoints announcedApplication announcedPeer base)
  result <- Herald.withHeraldTcpRuntime configured $ \tcp -> do
    let actualPeer = fromMaybe (resolvedHerald (Herald.heraldTcpPeerEndpoint (Herald.heraldTcpEndpoints tcp))) (Configuration.peerAdvertised planes)
    local <- checked (Discovery.discoveryContact (residentMember resident) (actualPeer :| []))
    atomically (putTMVar localContact local >> putTMVar localPeer actualPeer)
    let owner = Herald.heraldTcpRuntime tcp
    configuredRole <- case hosted of
      Just (node, oracle, raft) -> Just . (node,) <$> replicaContact oracle raft actualPeer
      Nothing -> do
        let oracle = fromMaybe (Configuration.oracleListen planes) (Configuration.oracleAdvertised planes)
            raft = fromMaybe (Configuration.raftListen planes) (Configuration.raftAdvertised planes)
        if Configuration.endpointPort oracle == 0 || Configuration.endpointPort raft == 0
          then pure Nothing
          else do
            node <- checked (mkRaftNodeId (SHA256.hash ("ECLIPS Oracle replica\0" <> heraldEpochBytes (heraldMemberEpoch (residentMember resident)))))
            contact <- replicaContact oracle raft actualPeer
            pure (Just (node, contact))
    case configuredRole of
      Nothing -> pure ()
      Just (node, contact) -> do
        submitted <- configureLocalOracleReplica owner node contact
        unless (submitted == Queued) (ioError (userError "local Oracle endpoints were not admitted"))
        -- FIFO owner exchange confirms endpoint ingress was applied before the
        -- deployment callback announces operator readiness.
        _ <- readHeraldOracleStatus owner >>= checked
        pure ()
    launcher <-
      if isFounder
        then do
          let actual = Herald.heraldTcpApplicationEndpoint (Herald.heraldTcpEndpoints tcp)
              contact = fromMaybe (resolvedHerald actual) (Configuration.applicationAdvertised planes)
          locator <- checked (heraldLocator (Configuration.endpointHost contact) (Configuration.endpointPort contact))
          Just <$> checked (connectionDescriptorWithTiming target locator (systemIdBytes (checkedOracleSystemId (deploymentCheckedOracleGenesis deployment))) (heraldEpochBytes (heraldMemberEpoch (residentMember resident))) (bootstrapManifestIdBytes (deploymentLauncherBootstrap deployment)))
        else pure Nothing
    let running = DeploymentRuntime resident tcp launcher (deploymentSystemBytes deployment) target
    withOptionalReplicaRole configureOracle (case hosted of Just _ -> Nothing; Nothing -> configuredRole) deployment resident localPeer owner $ do
      value <- use running
      disposition <- requestDeploymentDrain running
      case disposition of
        Queued -> awaitDeploymentExit running
        RuntimeStopped -> awaitDeploymentExit running
        DrainRequestAlreadyPending -> awaitDeploymentExit running
        IngressClosedForDrain -> awaitDeploymentExit running
        -- An operator drain can finish inside the callback. Completion
        -- revokes this scope's private administration lease before the
        -- cleanup request reaches it; the retained terminal result remains
        -- authoritative and must still be checked and joined.
        StalePhysicalGeneration -> awaitDeploymentExit running
        other -> ioError (userError ("deployment drain was not admitted: " <> show other))
      pure value
  case result of
    Right (value, HeraldRuntimeDrained) -> pure value
    Right (_, exited) -> ioError (userError ("Herald exited: " <> show exited))
    Left failure -> ioError (userError ("Herald runtime failed: " <> show failure))
  where
    founder = NonEmpty.head (deploymentResidents deployment)
    oracleHint contact = do
      node <- checked (mkRaftNodeId (OracleClient.oracleNodeClaimBytes (OracleClient.oracleContactNode contact)))
      target <- checked (Configuration.endpoint (OracleClient.oracleContactHost contact) (OracleClient.oracleContactPort contact))
      checked (Discovery.discoveryOracleContact node target)

readRuntimeJoinStatus :: HeraldRuntime -> IO Join.JoinStatus
readRuntimeJoinStatus runtime = do
  reply <- exchangeHeraldJoin runtime (Join.encodeJoinRequest Join.ReadJoinStatus) >>= checked
  decoded <- checked (Join.decodeJoinReply reply)
  case decoded of
    Join.JoinStatusReply status -> pure status
    _ -> ioError (userError "runtime did not return join status")

exchangeDeploymentJoin :: DeploymentRuntime -> Join.JoinRequest -> IO Join.JoinReply
exchangeDeploymentJoin (DeploymentRuntime _ tcp _ _ _) request = do
  reply <- Herald.exchangeHeraldTcpJoin tcp (Join.encodeJoinRequest request) >>= checked
  checked (Join.decodeJoinReply reply)

deploymentSystemIdentity :: DeploymentRuntime -> ByteString
deploymentSystemIdentity (DeploymentRuntime _ _ _ system _) = system

peerEndpoint :: ResidentDeployment -> Configuration.Endpoint
peerEndpoint resident = let planes = residentEndpoints resident in fromMaybe (Configuration.peerListen planes) (Configuration.peerAdvertised planes)

contactFor :: ResidentDeployment -> IO Discovery.DiscoveryContact
contactFor resident = checked (Discovery.discoveryContact (residentMember resident) (peerEndpoint resident :| []))

requestDeploymentDrain :: DeploymentRuntime -> IO RuntimeSubmission
requestDeploymentDrain (DeploymentRuntime _ tcp _ _ _) = Herald.requestHeraldTcpDrain tcp (adminCorrelationId 1)

awaitDeploymentExit :: DeploymentRuntime -> IO ()
awaitDeploymentExit (DeploymentRuntime _ tcp _ _ _) = do
  outcome <- Herald.awaitHeraldTcpExit tcp
  case outcome of
    Right HeraldRuntimeDrained -> pure ()
    other -> ioError (userError ("deployment resident exited: " <> show other))

awaitOracleReady :: Oracle.OracleTcpCluster -> IO ()
awaitOracleReady cluster = do
  statuses <- Oracle.oracleTcpNodeStatuses cluster
  unless (any (oracleRuntimeServiceReady . snd) statuses) (threadDelay 10_000 >> awaitOracleReady cluster)

contactSet :: RaftNodeId -> Configuration.Endpoint -> Either String OracleContactSet
contactSet node endpoint = do
  claim <- mapError (oracleNodeClaim (raftNodeIdBytes node))
  contact <- mapError (oracleContact claim (Configuration.endpointHost endpoint) (Configuration.endpointPort endpoint))
  mapError (oracleContactSet (contact :| []))
listen :: Configuration.Endpoint -> Either Herald.TcpEndpointError Herald.TcpListenEndpoint
listen endpoint = Herald.tcpListenEndpoint (Configuration.endpointHost endpoint) (Configuration.endpointPort endpoint)
resolved :: Configuration.Endpoint -> Either Herald.TcpEndpointError Herald.ResolvedTcpEndpoint
resolved endpoint = Herald.resolvedTcpEndpoint (Configuration.endpointHost endpoint) (Configuration.endpointPort endpoint)
oracleListen :: Configuration.Endpoint -> Either Oracle.OracleTcpConfigurationFault Oracle.OracleTcpListenEndpoint
oracleListen endpoint = Oracle.oracleTcpListenEndpoint (Text.unpack (Configuration.endpointHost endpoint)) (Configuration.endpointPort endpoint)
resolvedOracle :: Oracle.OracleTcpEndpoint -> Configuration.Endpoint
resolvedOracle endpoint = either error id (Configuration.endpoint (Text.pack (Oracle.oracleTcpEndpointHost endpoint)) (Oracle.oracleTcpEndpointPort endpoint))
resolvedHerald :: Herald.ResolvedTcpEndpoint -> Configuration.Endpoint
resolvedHerald endpoint = either error id (Configuration.endpoint (Herald.resolvedTcpHost endpoint) (Herald.resolvedTcpPort endpoint))

checked :: (Show problem) => Either problem value -> IO value
checked = either (ioError . userError . show) pure
mapError :: (Show problem) => Either problem value -> Either String value
mapError = either (Left . show) Right

-- | Consult the local discovery owner first, then its observed remote routes.
-- These registrations provision lanes only; the Raft/Oracle owners apply all
-- authority through their ordinary replay path.
resolveReplica :: TMVar Configuration.Endpoint -> RaftNodeId -> IO (Maybe Voter.OracleReplicaRegistration)
resolveReplica localPeer node = do
  local <- atomically (readTMVar localPeer)
  observed <- Discovery.exchangeDiscoveryRequest local (Discovery.DiscoverSystem 2 [])
  let routes = case observed of
        Right (Discovery.SystemDiscovered 2 snapshot) -> Set.toAscList (Set.unions (map Discovery.discoveryContactLocators (Set.toAscList (Discovery.discoveryContacts snapshot))))
        _ -> []
  Discovery.lookupReplicaRegistration (local : filter (/= local) routes) node

replicaContact :: Configuration.Endpoint -> Configuration.Endpoint -> Configuration.Endpoint -> IO Voter.OracleReplicaContact
replicaContact oracle raft discovery = do
  oracleEndpoint <- endpoint oracle
  raftEndpoint <- endpoint raft
  discoveryEndpoint <- endpoint discovery
  pure (Voter.oracleReplicaContact oracleEndpoint raftEndpoint discoveryEndpoint)
  where
    endpoint value = checked (Voter.oracleReplicaEndpoint (TextEncoding.encodeUtf8 (Configuration.endpointHost value)) (Configuration.endpointPort value))

registrationOracleHint :: Voter.OracleReplicaRegistration -> IO Discovery.DiscoveryOracleContact
registrationOracleHint registration = case Voter.replicaRegistrationContact registration of
  Nothing -> ioError (userError "registration has no contact")
  Just contact -> do
    target <- checked (replicaEndpoint (Voter.replicaContactOracle contact))
    checked (Discovery.discoveryOracleContact (Voter.replicaRegistrationNode registration) target)

replicaEndpoint :: Voter.OracleReplicaEndpoint -> Either String Configuration.Endpoint
replicaEndpoint endpoint = Configuration.endpoint (TextEncoding.decodeUtf8 (Voter.replicaEndpointHost endpoint)) (Voter.replicaEndpointPort endpoint)

checkedOracleContacts :: NonEmpty Discovery.DiscoveryOracleContact -> OracleContactSet
checkedOracleContacts hints = either error id $ do
  contacts <-
    traverse
      ( \hint -> do
          claim <- mapError (oracleNodeClaim (raftNodeIdBytes (Discovery.discoveryOracleNode hint)))
          let target = Discovery.discoveryOracleLocator hint
          mapError (oracleContact claim (Configuration.endpointHost target) (Configuration.endpointPort target))
      )
      hints
  mapError (oracleContactSet contacts)

-- | A prepared role starts once its exact local registration is committed.
-- Demotion changes voting authority inside Raft and leaves this learner and
-- the ordinary Herald running. A failed role is not restarted under lost state.
withOptionalReplicaRole :: (OracleRuntime.OracleRuntimeConfiguration -> OracleRuntime.OracleRuntimeConfiguration) -> Maybe (RaftNodeId, Voter.OracleReplicaContact) -> CheckedDeployment -> ResidentDeployment -> TMVar Configuration.Endpoint -> HeraldRuntime -> IO value -> IO value
withOptionalReplicaRole _ Nothing _ _ _ _ action = action
withOptionalReplicaRole configureOracle (Just (node, contact)) deployment resident localPeer owner action = mask $ \restore -> do
  done <- newEmptyTMVarIO
  worker <- forkIOWithUnmask $ \unmask ->
    ( do
        result <- try @SomeException (unmask awaitRegistration)
        case result of
          Left failure | Just _ <- (fromException failure :: Maybe AsyncException) -> pure ()
          Left failure -> hPutStrLn stderr ("Oracle replica role stopped: " <> show failure)
          Right () -> pure ()
    )
      `finally` atomically (putTMVar done ())
  restore action `finally` (killThread worker >> atomically (readTMVar done))
  where
    awaitRegistration = do
      observed <- awaitHeraldOracleReplicaRegistration owner node
      case observed of
        Left _ -> pure ()
        Right registration -> do
          unless
            (Voter.replicaRegistrationContact registration == Just contact)
            (ioError (userError "committed replica differs from configured local endpoints"))
          startRole registration
    startRole registration = do
      let native = OracleGenesis.checkedOracleRaftNativeConfiguration (deploymentCheckedOracleGenesis deployment)
          raft = Raft.raftGenesis node (Raft.raftNativeVoters native) (Raft.raftNativeHeartbeatInterval native) (Raft.raftNativeElectionTimeoutLower native) (Raft.raftNativeElectionTimeoutUpper native)
          planes = residentEndpoints resident
      configuration <- checked (OracleRuntime.oracleRuntimeLearnerConfiguration raft (deploymentOracleGenesisSource deployment) registration OracleRuntime.systemRuntimeElectionTimeoutSource)
      base <- checked (Oracle.oracleTcpClusterConfiguration [configureOracle (OracleRuntime.configureOracleRuntimeReplicaResolver (resolveReplica localPeer) configuration)])
      oracleListener <- checked (oracleListen (Configuration.oracleListen planes))
      raftListener <- checked (oracleListen (Configuration.raftListen planes))
      listening <- checked (Oracle.configureOracleTcpListeners [(node, oracleListener, raftListener)] (Oracle.configureOracleTcpTiming (deploymentTakeoverTarget deployment) base))
      -- Genesis peers have no registration contacts until their first prepare.
      -- Their immutable deployment endpoints remain valid transport hints.
      status <- readHeraldOracleStatus owner >>= checked
      genesisContacts <-
        traverse
          ( \binding -> do
              host <- maybe (ioError (userError "genesis replica host missing")) pure (find ((== OracleGenesis.raftVoterBindingHeraldEpoch binding) . heraldMemberEpoch . residentMember) (NonEmpty.toList (deploymentResidents deployment)))
              let hostPlanes = residentEndpoints host
                  initialEndpoint = fromMaybe (Configuration.raftListen hostPlanes) (Configuration.raftAdvertised hostPlanes)
                  retained = find ((== OracleGenesis.raftVoterBindingNode binding) . Voter.replicaRegistrationNode) (Join.heraldOracleStatusReplicas status)
              endpoint <- case retained >>= Voter.replicaRegistrationContact of
                Just registered -> checked (replicaEndpoint (Voter.replicaContactRaft registered))
                Nothing -> pure initialEndpoint
              listener <- checked (oracleListen endpoint)
              pure (OracleGenesis.raftVoterBindingNode binding, listener)
          )
          (OracleGenesis.checkedOracleRaftVoterBindings (deploymentCheckedOracleGenesis deployment))
      configured <- checked (Oracle.configureOracleTcpReplicaContacts genesisContacts listening)
      result <- Oracle.withOracleTcpCluster configured (\cluster -> Oracle.awaitOracleTcpNodeExit cluster node)
      case result of { Left failure -> ioError (userError (show failure)); Right exit -> hPutStrLn stderr ("Oracle replica role exited: " <> show exit) }

deploymentRuntimeTakeoverTarget :: DeploymentRuntime -> Timing.TakeoverTarget
deploymentRuntimeTakeoverTarget (DeploymentRuntime _ _ _ _ target) = target
