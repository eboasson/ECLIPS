-- | Supervised TCP composition for one or more replicas of an immutable run.
module Eclips.Oracle.Runtime.TCP
  ( OracleTcpEndpoint,
    oracleTcpEndpointHost,
    oracleTcpEndpointPort,
    OracleTcpListenEndpoint,
    oracleTcpListenEndpoint,
    configureOracleTcpListeners,
    configureOracleTcpReplicaContacts,
    configureOracleTcpTiming,
    OracleTcpConfigurationFault (..),
    OracleTcpClusterConfiguration,
    oracleTcpClusterConfiguration,
    OracleTcpStartFailure (..),
    OracleTcpCluster,
    startOracleTcpCluster,
    withOracleTcpCluster,
    stopOracleTcpCluster,
    stopOracleTcpNode,
    awaitOracleTcpNodeExit,
    oracleTcpOracleContacts,
    oracleTcpRaftEndpoints,
    oracleTcpNodeStatuses,
    oracleTcpNodeRecordings,
  )
where

import Control.Concurrent
  ( forkIOWithUnmask,
  )
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
  )
import Control.Concurrent.STM
  ( TVar,
    atomically,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )
import Control.Exception
  ( SomeException,
    displayException,
    finally,
    mask,
    mask_,
    onException,
    try,
  )
import Control.Monad
  ( forM,
    forM_,
    void,
    when,
  )
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word16)
import Eclips.Oracle.Runtime
  ( awaitOracleRuntimeExit,
    oracleRuntimeRecording,
    oracleRuntimeStatus,
    startOracleRuntime,
    stopOracleRuntime,
  )
import Eclips.Oracle.Runtime.Internal.Scope (signalRuntimeFailure)
import Eclips.Oracle.Runtime.Internal.TCP.Oracle (handleOracleTcpConnection)
import Eclips.Oracle.Runtime.Internal.TCP.Raft
  ( RaftTcpManager,
    handleRaftTcpConnection,
    newRaftTcpManager,
    raftTcpTransport,
    startRaftTcpManager,
    stopRaftTcpManager,
    updateRaftTcpRegistrations,
  )
import Eclips.Oracle.Runtime.Internal.TCP.Retry (RaftRetryPolicy, raftRetryPolicy)
import Eclips.Oracle.Runtime.Internal.TCP.Server
  ( TcpServer,
    startTcpServer,
    stopTcpServer,
  )
import Eclips.Oracle.Runtime.Internal.TCP.Socket
  ( BoundTcpListener (..),
    ResolvedTcpEndpoint (..),
    bindTcpListener,
    closeSocketQuietly,
  )
import Eclips.Oracle.Runtime.Internal.Types
  ( OracleRuntime (..),
    OracleRuntimeConfiguration (..),
    OracleRuntimeEvent,
    OracleRuntimeExit,
    OracleRuntimeFailure (..),
    OracleRuntimeStatus (..),
    RuntimeReplicaRegistry (..),
  )
import Eclips.Public.Types.Timing (TakeoverTarget, defaultTakeoverTarget)
import Eclips.Raft.Genesis
  ( checkedRaftLocalNode,
    checkedRaftNativeConfiguration,
    raftNativeVoters,
  )
import Eclips.Raft.Identity (RaftNodeId)

newtype OracleTcpEndpoint = OracleTcpEndpoint ResolvedTcpEndpoint
  deriving stock (Eq, Ord, Show)

oracleTcpEndpointHost :: OracleTcpEndpoint -> String
oracleTcpEndpointHost (OracleTcpEndpoint endpoint) = endpoint.resolvedTcpHost

oracleTcpEndpointPort :: OracleTcpEndpoint -> Word16
oracleTcpEndpointPort (OracleTcpEndpoint endpoint) = endpoint.resolvedTcpPort

data OracleTcpConfigurationFault
  = OracleTcpConfigurationHasNoVoters
  | OracleTcpConfigurationHostEmpty
  | OracleTcpConfigurationListenerSetMismatch
  | OracleTcpConfigurationListenerConflict
  | OracleTcpConfigurationReplicaContactConflict
  | OracleTcpConfigurationReplicaContactPortZero
  | OracleTcpConfigurationNodeSetMismatch [RaftNodeId] [RaftNodeId]
  | OracleTcpConfigurationNativeRaftMismatch
  | OracleTcpConfigurationOracleGenesisMismatch
  deriving stock (Eq, Show)

newtype OracleTcpListenEndpoint = OracleTcpListenEndpoint ResolvedTcpEndpoint
  deriving stock (Eq, Ord, Show)

-- | Port zero explicitly requests an ephemeral listener. Hostname resolution
-- belongs to startup, before any semantic role is declared ready.
oracleTcpListenEndpoint :: String -> Word16 -> Either OracleTcpConfigurationFault OracleTcpListenEndpoint
oracleTcpListenEndpoint "" _ = Left OracleTcpConfigurationHostEmpty
oracleTcpListenEndpoint host port = Right (OracleTcpListenEndpoint (ResolvedTcpEndpoint host port))

data OracleTcpClusterConfiguration
  = OracleTcpClusterConfiguration [OracleRuntimeConfiguration] (Map RaftNodeId (OracleTcpListenEndpoint, OracleTcpListenEndpoint)) (Map RaftNodeId ResolvedTcpEndpoint) RaftRetryPolicy

-- | Derive native transport reconnect pacing from the deployment target.
-- Every initial or subsequently discovered peer uses this same local policy.
-- Native election and heartbeat durations remain part of checked Raft genesis.
configureOracleTcpTiming :: TakeoverTarget -> OracleTcpClusterConfiguration -> OracleTcpClusterConfiguration
configureOracleTcpTiming target (OracleTcpClusterConfiguration configurations listeners contacts _) =
  OracleTcpClusterConfiguration configurations listeners contacts (raftRetryPolicy target)

-- | Select each node's Oracle and Raft listeners, in that order. Every configured
-- node must appear exactly once; fixed nonzero local bindings must be distinct.
configureOracleTcpListeners :: [(RaftNodeId, OracleTcpListenEndpoint, OracleTcpListenEndpoint)] -> OracleTcpClusterConfiguration -> Either OracleTcpConfigurationFault OracleTcpClusterConfiguration
configureOracleTcpListeners listeners (OracleTcpClusterConfiguration configurations _ contacts timing) = do
  let expected = sort (map (checkedRaftLocalNode . configuredRaftGenesis) configurations)
      actual = sort [node | (node, _, _) <- listeners]
      endpoints = [endpoint | (_, oracle, raft) <- listeners, OracleTcpListenEndpoint endpoint <- [oracle, raft], endpoint.resolvedTcpPort /= 0]
  if actual /= expected
    then Left OracleTcpConfigurationListenerSetMismatch
    else
      if length endpoints /= Map.size (Map.fromList [(endpoint, ()) | endpoint <- endpoints])
        then Left OracleTcpConfigurationListenerConflict
        else Right (OracleTcpClusterConfiguration configurations (Map.fromList [(node, (oracle, raft)) | (node, oracle, raft) <- listeners]) contacts timing)

-- | Establish additional same-run replica identity/contact bindings for ERFT.
-- These remote peers may be learners or retained transition participants. This
-- transport registry grants no votes, native replication targets, Oracle role,
-- or semantic admission; their authoritative registration belongs to the adapter.
-- Local listener bindings are supplied separately and cannot be replaced here.
configureOracleTcpReplicaContacts :: [(RaftNodeId, OracleTcpListenEndpoint)] -> OracleTcpClusterConfiguration -> Either OracleTcpConfigurationFault OracleTcpClusterConfiguration
configureOracleTcpReplicaContacts contacts (OracleTcpClusterConfiguration configurations listeners _ timing) = do
  let supplied = Map.fromList [(node, endpoint) | (node, OracleTcpListenEndpoint endpoint) <- contacts]
  if Map.size supplied /= length contacts || any (`Map.member` listeners) (Map.keys supplied)
    then Left OracleTcpConfigurationReplicaContactConflict
    else Right ()
  if any ((== 0) . resolvedTcpPort) (Map.elems supplied)
    then Left OracleTcpConfigurationReplicaContactPortZero
    else Right (OracleTcpClusterConfiguration configurations listeners supplied timing)

data OracleTcpStartFailure
  = OracleTcpRuntimeStartFailed RaftNodeId OracleRuntimeFailure
  | OracleTcpInfrastructureFailed String
  deriving stock (Eq, Show)

data OracleTcpNode = OracleTcpNode
  { tcpNodeRuntime :: OracleRuntime,
    tcpNodeRaftManager :: RaftTcpManager,
    tcpNodeRaftServer :: TcpServer,
    tcpNodeOracleServer :: TcpServer,
    tcpNodeRaftEndpoint :: OracleTcpEndpoint,
    tcpNodeOracleEndpoint :: OracleTcpEndpoint,
    tcpNodeAccepting :: TVar Bool,
    tcpNodeStopDone :: MVar (),
    tcpNodeMonitorStarted :: TVar Bool,
    tcpNodeMonitorDone :: MVar ()
  }

data OracleTcpCluster = OracleTcpCluster
  { tcpClusterNodes :: Map RaftNodeId OracleTcpNode,
    tcpClusterAccepting :: TVar Bool,
    tcpClusterStopDone :: MVar ()
  }

data PreparedNode = PreparedNode
  { preparedConfiguration :: OracleRuntimeConfiguration,
    preparedRaftListener :: BoundTcpListener,
    preparedOracleListener :: BoundTcpListener
  }

oracleTcpClusterConfiguration ::
  [OracleRuntimeConfiguration] ->
  Either OracleTcpConfigurationFault OracleTcpClusterConfiguration
oracleTcpClusterConfiguration [] = Left OracleTcpConfigurationHasNoVoters
oracleTcpClusterConfiguration configurations@(first : _) = do
  let expectedNative = checkedRaftNativeConfiguration first.configuredRaftGenesis
      expectedNodes = raftNativeVoters expectedNative
      actualNodes = sort (fmap (checkedRaftLocalNode . configuredRaftGenesis) configurations)
  if length actualNodes == Map.size (Map.fromList [(node, ()) | node <- actualNodes])
    then Right ()
    else Left (OracleTcpConfigurationNodeSetMismatch expectedNodes actualNodes)
  if all ((== expectedNative) . checkedRaftNativeConfiguration . configuredRaftGenesis) configurations
    then Right ()
    else Left OracleTcpConfigurationNativeRaftMismatch
  if all ((== first.configuredOracleGenesis) . configuredOracleGenesis) configurations
    then Right ()
    else Left OracleTcpConfigurationOracleGenesisMismatch
  let loopback = OracleTcpListenEndpoint (ResolvedTcpEndpoint "127.0.0.1" 0)
  Right (OracleTcpClusterConfiguration configurations (Map.fromList [(node, (loopback, loopback)) | node <- actualNodes]) Map.empty (raftRetryPolicy defaultTakeoverTarget))

startOracleTcpCluster ::
  OracleTcpClusterConfiguration ->
  IO (Either OracleTcpStartFailure OracleTcpCluster)
startOracleTcpCluster (OracleTcpClusterConfiguration configurations listeners contacts timing) = mask $ \restore -> do
  preparedResult <- restore (prepareNodes listeners configurations)
  case preparedResult of
    Left problem -> pure (Left (OracleTcpInfrastructureFailed problem))
    Right prepared -> do
      let raftEndpoints =
            Map.fromList
              [ (preparedNodeId node, node.preparedRaftListener.boundTcpEndpoint)
              | node <- prepared
              ]
      managerResult <-
        restore (prepareManagers timing (raftEndpoints <> contacts) prepared)
          `onException` runOwnedCleanup [closePrepared prepared]
      case managerResult of
        Left problem -> do
          runOwnedCleanup [closePrepared prepared]
          pure (Left (OracleTcpInfrastructureFailed problem))
        Right managers -> do
          runtimeResult <-
            restore (startRuntimes managers prepared)
              `onException` runOwnedCleanup
                [ stopManagers managers,
                  closePrepared prepared
                ]
          case runtimeResult of
            Left failure -> do
              runOwnedCleanup
                [ stopManagers managers,
                  closePrepared prepared
                ]
              pure (Left failure)
            Right runtimes -> do
              serverResult <-
                restore (startServers managers runtimes prepared)
                  `onException` runOwnedCleanup
                    [ stopManagers managers,
                      stopRuntimes runtimes,
                      closePrepared prepared
                    ]
              case serverResult of
                Left problem -> do
                  runOwnedCleanup
                    [ stopManagers managers,
                      stopRuntimes runtimes,
                      closePrepared prepared
                    ]
                  pure (Left (OracleTcpInfrastructureFailed problem))
                Right servers -> do
                  nodes <-
                    fmap
                      Map.fromList
                      ( forM prepared $ \node -> do
                          let nodeId = preparedNodeId node
                              runtime = runtimes Map.! nodeId
                              manager = managers Map.! nodeId
                              (raftServer, oracleServer) = servers Map.! nodeId
                          accepting <- newTVarIO True
                          stopDone <- newEmptyMVar
                          monitorStarted <- newTVarIO False
                          monitorDone <- newEmptyMVar
                          let installed =
                                OracleTcpNode
                                  { tcpNodeRuntime = runtime,
                                    tcpNodeRaftManager = manager,
                                    tcpNodeRaftServer = raftServer,
                                    tcpNodeOracleServer = oracleServer,
                                    tcpNodeRaftEndpoint = OracleTcpEndpoint node.preparedRaftListener.boundTcpEndpoint,
                                    tcpNodeOracleEndpoint = OracleTcpEndpoint node.preparedOracleListener.boundTcpEndpoint,
                                    tcpNodeAccepting = accepting,
                                    tcpNodeStopDone = stopDone,
                                    tcpNodeMonitorStarted = monitorStarted,
                                    tcpNodeMonitorDone = monitorDone
                                  }
                          pure (nodeId, installed)
                      )
                  let rollbackNodes =
                        runOwnedCleanup
                          [ stopNodes nodes,
                            closePrepared prepared
                          ]
                  healthy <-
                    restore
                      ( do
                          forM_ (Map.toAscList managers) $ \(node, manager) ->
                            startRaftTcpManager manager (runtimes Map.! node)
                          forM_ (Map.elems nodes) startNodeMonitor
                          clusterStartupHealthy nodes
                      )
                      `onException` rollbackNodes
                  if not healthy
                    then do
                      rollbackNodes
                      pure
                        ( Left
                            (OracleTcpInfrastructureFailed "a TCP replica failed during startup")
                        )
                    else do
                      accepting <- newTVarIO True
                      stopDone <- newEmptyMVar
                      pure
                        ( Right
                            OracleTcpCluster
                              { tcpClusterNodes = nodes,
                                tcpClusterAccepting = accepting,
                                tcpClusterStopDone = stopDone
                              }
                        )

withOracleTcpCluster ::
  OracleTcpClusterConfiguration ->
  (OracleTcpCluster -> IO result) ->
  IO (Either OracleTcpStartFailure result)
withOracleTcpCluster configuration use = mask $ \restore -> do
  started <- restore (startOracleTcpCluster configuration)
  case started of
    Left failure -> pure (Left failure)
    Right cluster -> do
      result <- restore (use cluster) `onException` stopOracleTcpCluster cluster
      stopOracleTcpCluster cluster
      pure (Right result)

stopOracleTcpCluster :: OracleTcpCluster -> IO ()
stopOracleTcpCluster cluster = mask_ $ do
  owner <- atomically $ do
    accepting <- readTVar cluster.tcpClusterAccepting
    writeTVar cluster.tcpClusterAccepting False
    pure accepting
  if not owner
    then void (readMVar cluster.tcpClusterStopDone)
    else do
      startOwnedCleanup
        [stopNodes cluster.tcpClusterNodes]
        cluster.tcpClusterStopDone
      void (readMVar cluster.tcpClusterStopDone)

stopOracleTcpNode :: OracleTcpCluster -> RaftNodeId -> IO Bool
stopOracleTcpNode cluster node =
  case Map.lookup node cluster.tcpClusterNodes of
    Nothing -> pure False
    Just installed -> stopNode installed >> pure True

-- | Await one supervised replica role. The deployment owner observes its exit
-- while the surrounding ordinary Herald remains active.
awaitOracleTcpNodeExit :: OracleTcpCluster -> RaftNodeId -> IO (Maybe OracleRuntimeExit)
awaitOracleTcpNodeExit cluster node = traverse (awaitOracleRuntimeExit . tcpNodeRuntime) (Map.lookup node cluster.tcpClusterNodes)

oracleTcpOracleContacts :: OracleTcpCluster -> [(RaftNodeId, OracleTcpEndpoint)]
oracleTcpOracleContacts cluster =
  fmap
    (\(node, installed) -> (node, installed.tcpNodeOracleEndpoint))
    (Map.toAscList cluster.tcpClusterNodes)

oracleTcpRaftEndpoints :: OracleTcpCluster -> [(RaftNodeId, OracleTcpEndpoint)]
oracleTcpRaftEndpoints cluster =
  fmap
    (\(node, installed) -> (node, installed.tcpNodeRaftEndpoint))
    (Map.toAscList cluster.tcpClusterNodes)

oracleTcpNodeStatuses :: OracleTcpCluster -> IO [(RaftNodeId, OracleRuntimeStatus)]
oracleTcpNodeStatuses cluster =
  traverse
    (\(node, installed) -> (node,) <$> oracleRuntimeStatus installed.tcpNodeRuntime)
    (Map.toAscList cluster.tcpClusterNodes)

oracleTcpNodeRecordings :: OracleTcpCluster -> IO [(RaftNodeId, [OracleRuntimeEvent])]
oracleTcpNodeRecordings cluster =
  traverse
    (\(node, installed) -> (node,) <$> oracleRuntimeRecording installed.tcpNodeRuntime)
    (Map.toAscList cluster.tcpClusterNodes)

prepareNodes :: Map RaftNodeId (OracleTcpListenEndpoint, OracleTcpListenEndpoint) -> [OracleRuntimeConfiguration] -> IO (Either String [PreparedNode])
prepareNodes listeners configurations = mask $ \restore -> go restore [] configurations
  where
    go _ reversed [] = pure (Right (reverse reversed))
    go restore reversed (configuration : remaining) = do
      let (OracleTcpListenEndpoint oracleEndpoint, OracleTcpListenEndpoint raftEndpoint) = listeners Map.! checkedRaftLocalNode configuration.configuredRaftGenesis
      raftResult <- try @SomeException (restore (bindTcpListener raftEndpoint))
      case raftResult of
        Left exception -> closePrepared reversed >> pure (Left (displayException exception))
        Right raftListener -> do
          oracleResult <- try @SomeException (restore (bindTcpListener oracleEndpoint))
          case oracleResult of
            Left exception -> do
              closeSocketQuietly raftListener.boundTcpSocket
              closePrepared reversed
              pure (Left (displayException exception))
            Right oracleListener ->
              go
                restore
                (PreparedNode configuration raftListener oracleListener : reversed)
                remaining

prepareManagers ::
  RaftRetryPolicy ->
  Map RaftNodeId ResolvedTcpEndpoint ->
  [PreparedNode] ->
  IO (Either String (Map RaftNodeId RaftTcpManager))
prepareManagers timing endpoints nodes = mask $ \restore -> go restore Map.empty nodes
  where
    go _ managers [] = pure (Right managers)
    go restore managers (node : remaining) = do
      let configuration = node.preparedConfiguration
      created <-
        restore
          ( newRaftTcpManager
              timing
              configuration.configuredRaftGenesis
              configuration.configuredOracleGenesis
              endpoints
              configuration.configuredReplicaResolver
              configuration.configuredReplicaBootstrap
          )
          `onException` runOwnedCleanup [stopManagers managers]
      case created of
        Left problem -> do
          runOwnedCleanup [stopManagers managers]
          pure (Left problem)
        Right manager ->
          go restore (Map.insert (preparedNodeId node) manager managers) remaining

startRuntimes ::
  Map RaftNodeId RaftTcpManager ->
  [PreparedNode] ->
  IO (Either OracleTcpStartFailure (Map RaftNodeId OracleRuntime))
startRuntimes managers nodes = mask $ \restore -> go restore Map.empty nodes
  where
    go _ runtimes [] = pure (Right runtimes)
    go restore runtimes (node : remaining) = do
      let nodeId = preparedNodeId node
          configured =
            node.preparedConfiguration
              { configuredRaftTransport = raftTcpTransport (managers Map.! nodeId),
                configuredReplicaRegistry = RuntimeReplicaRegistry (updateRaftTcpRegistrations (managers Map.! nodeId))
              }
      started <-
        restore (startOracleRuntime configured)
          `onException` runOwnedCleanup [stopRuntimes runtimes]
      case started of
        Left failure -> do
          runOwnedCleanup [stopRuntimes runtimes]
          pure (Left (OracleTcpRuntimeStartFailed nodeId failure))
        Right runtime -> go restore (Map.insert nodeId runtime runtimes) remaining

startServers ::
  Map RaftNodeId RaftTcpManager ->
  Map RaftNodeId OracleRuntime ->
  [PreparedNode] ->
  IO (Either String (Map RaftNodeId (TcpServer, TcpServer)))
startServers managers runtimes nodes = mask $ \restore -> go restore Map.empty nodes
  where
    go _ servers [] = pure (Right servers)
    go restore servers (node : remaining) = do
      let nodeId = preparedNodeId node
          runtime = runtimes Map.! nodeId
          report lane exception =
            signalRuntimeFailure
              runtime.runtimeScope
              (RuntimeEssentialChildFailed lane (displayException exception))
      raftResult <-
        try @SomeException
          ( restore
              ( startTcpServer
                  node.preparedRaftListener.boundTcpSocket
                  (report "raft-listener")
                  (handleRaftTcpConnection (managers Map.! nodeId) runtime)
              )
          )
      case raftResult of
        Left exception -> do
          runOwnedCleanup [stopServers servers]
          pure (Left (displayException exception))
        Right raftServer -> do
          oracleResult <-
            try @SomeException
              ( restore
                  ( startTcpServer
                      node.preparedOracleListener.boundTcpSocket
                      (report "oracle-listener")
                      (handleOracleTcpConnection runtime)
                  )
              )
          case oracleResult of
            Left exception -> do
              runOwnedCleanup
                [ stopTcpServer raftServer,
                  stopServers servers
                ]
              pure (Left (displayException exception))
            Right oracleServer ->
              go restore (Map.insert nodeId (raftServer, oracleServer) servers) remaining

startNodeMonitor :: OracleTcpNode -> IO ()
startNodeMonitor node = mask_ $ do
  start <- newEmptyMVar
  _ <-
    forkIOWithUnmask $ \unmask ->
      ( do
          takeMVar start
          unmask $ do
            _ <- awaitOracleRuntimeExit node.tcpNodeRuntime
            beginNodeStop node
      )
        `finally` putMVar node.tcpNodeMonitorDone ()
  atomically (writeTVar node.tcpNodeMonitorStarted True)
  putMVar start ()

stopNode :: OracleTcpNode -> IO ()
stopNode node = mask_ $ do
  beginNodeStop node
  void (readMVar node.tcpNodeStopDone)

beginNodeStop :: OracleTcpNode -> IO ()
beginNodeStop node = mask_ $ do
  owner <- atomically $ do
    accepting <- readTVar node.tcpNodeAccepting
    writeTVar node.tcpNodeAccepting False
    pure accepting
  when owner
    $ startOwnedCleanup
      [ stopTcpServer node.tcpNodeOracleServer,
        stopTcpServer node.tcpNodeRaftServer,
        stopRaftTcpManager node.tcpNodeRaftManager,
        stopOracleRuntime node.tcpNodeRuntime,
        do
          monitorStarted <- readTVarIO node.tcpNodeMonitorStarted
          when monitorStarted (void (readMVar node.tcpNodeMonitorDone))
      ]
      node.tcpNodeStopDone

clusterStartupHealthy :: Map RaftNodeId OracleTcpNode -> IO Bool
clusterStartupHealthy nodes =
  and
    <$> traverse
      ( \node -> do
          laneAccepting <- atomically (readTVar node.tcpNodeAccepting)
          runtimeStatus <- oracleRuntimeStatus node.tcpNodeRuntime
          pure (laneAccepting && runtimeStatus.statusAccepting)
      )
      (Map.elems nodes)

stopNodes :: Map RaftNodeId OracleTcpNode -> IO ()
stopNodes = bestEffort . fmap stopNode . Map.elems

stopManagers :: Map RaftNodeId RaftTcpManager -> IO ()
stopManagers = bestEffort . fmap stopRaftTcpManager . Map.elems

stopRuntimes :: Map RaftNodeId OracleRuntime -> IO ()
stopRuntimes = bestEffort . fmap stopOracleRuntime . Map.elems

stopServers :: Map RaftNodeId (TcpServer, TcpServer) -> IO ()
stopServers =
  bestEffort
    . concatMap
      (\(raftServer, oracleServer) -> [stopTcpServer oracleServer, stopTcpServer raftServer])
    . Map.elems

runOwnedCleanup :: [IO ()] -> IO ()
runOwnedCleanup actions = mask_ $ do
  done <- newEmptyMVar
  startOwnedCleanup actions done
  void (readMVar done)

startOwnedCleanup :: [IO ()] -> MVar () -> IO ()
startOwnedCleanup actions done = do
  _ <-
    forkIOWithUnmask $ \_ ->
      bestEffort actions `finally` putMVar done ()
  pure ()

bestEffort :: [IO ()] -> IO ()
bestEffort = mapM_ (void . try @SomeException)

closePrepared :: [PreparedNode] -> IO ()
closePrepared =
  mapM_ $ \node -> do
    closeSocketQuietly node.preparedRaftListener.boundTcpSocket
    closeSocketQuietly node.preparedOracleListener.boundTcpSocket

preparedNodeId :: PreparedNode -> RaftNodeId
preparedNodeId = checkedRaftLocalNode . configuredRaftGenesis . preparedConfiguration
