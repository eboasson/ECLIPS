{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Independently supervised real-TCP Heralds for Step-15 acceptance tests.
--
-- Each Herald owns a separate worker and command cell. Returning from H4's
-- callback therefore closes only H4's structured runtime/TCP scope while the
-- other three callbacks remain live.
module Step15Fixtures
  ( Step15Herald,
    step15ManagedHeraldName,
    step15ManagedHeraldTcp,
    step15ManagedHeraldWorkLedger,
    step15ManagedHeraldIsRunning,
    Step15Deployment,
    step15DeploymentOracleCluster,
    step15H1,
    step15H2,
    step15H3,
    step15H4,
    withStep15Deployment,
    withStep15H4OracleHealthCutDeployment,
    withStep15CrashDeployment,
    withRepeatedRetirementDeployment,
    withRepeatedRetirementSelfFenceDeployment,
    crashStep15Herald,
    awaitStep15PeerTopology,
    closeStep15HeraldPeerTransport,
    openStep15HeraldPeerTransport,
    withStep15ApplicationLossDeployment,
    withStep15DeploymentWithHooks,
    awaitStep15PeerMesh,
    awaitStep15SurvivorMesh,
    awaitStep15H4PeerIsolation,
    closeStep15H4PeerCut,
    openStep15H4PeerTransport,
    reopenStep15H4PeerCut,
    closeStep15H4Scope,
    crashStep15H4,
    awaitStep15H4Exit,
    awaitStep15HeraldExit,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkFinally,
    killThread,
    threadDelay,
  )
import Control.Concurrent.STM
  ( STM,
    TMVar,
    TVar,
    atomically,
    isEmptyTMVar,
    newEmptyTMVarIO,
    newTVarIO,
    orElse,
    putTMVar,
    readTMVar,
    readTVar,
    takeTMVar,
    tryPutTMVar,
    writeTVar,
  )
import Control.Exception
  ( AsyncException (ThreadKilled),
    SomeException,
    bracket,
    fromException,
    mask,
    onException,
    throwIO,
  )
import Control.Monad (unless, void, when)
import Data.List (find, sort)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Herald.Administration
  ( AdminCorrelationId,
    adminCorrelationId,
  )
import Eclips.Herald.Discovery
  ( PeerBinding,
    peerBindingRemoteHeraldEpoch,
    peerBindingSelectedCandidate,
  )
import Eclips.Herald.OracleClient (oracleContact, oracleContactNode)
import Eclips.Herald.Runtime (configureHeraldRuntimeOracleHealthSink)
import Eclips.Herald.Runtime.Ingress (RuntimeSubmission (Queued))
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks,
    defaultRuntimeHooks,
    withHeraldRuntimeWithHooks,
  )
import Eclips.Herald.Runtime.Internal.Types (HeraldRuntimeConfiguration (HeraldRuntimeConfiguration))
import Eclips.Herald.Runtime.TCP
  ( ConfiguredPeerSeed,
    HeraldTcpConfiguration,
    HeraldTcpFailure,
    awaitHeraldTcpExit,
    configuredPeerSeed,
    heraldTcpEndpoints,
    heraldTcpPeerEndpoint,
    requestHeraldTcpDrain,
    resolvedTcpHost,
    resolvedTcpPort,
    tcpListenEndpoint,
  )
import Eclips.Herald.Runtime.TCP.Internal.Facade
  ( HeraldRuntimeScope (HeraldRuntimeScope),
    PeerTransportGate,
    closePeerTransportGate,
    heraldTcpPeerTransportGate,
    openPeerTransportGate,
    withHeraldTcpRuntimeInScopeInternal,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( HeraldTcp (HeraldTcp),
    TcpContext (TcpContext, tcpCurrentPeerConnections),
  )
import Eclips.Herald.Runtime.Trace
  ( HeraldRuntimeExit (HeraldRuntimeDrained),
  )
import Eclips.Oracle.Runtime.TCP
  ( OracleTcpCluster,
    withOracleTcpCluster,
  )
import Network.Socket qualified as Socket
import Step15Configuration
  ( Step15HeraldName (..),
    Step15RuntimeProfile (..),
    awaitStep15OracleReady,
    step15HeraldEpoch,
    step15OracleContacts,
    step15OracleTcpConfigurationFor,
    step15TcpConfigurationFor,
  )
import Step15WorkCounts
  ( Step15WorkLedger,
    awaitStep15WorkLedgerReady,
    captureStep15RuntimeHooks,
    installStep15WorkProbes,
    newStep15WorkLedger,
    snapshotStep15WorkEvidence,
    step15KernelFaultEvidence,
    summarizeStep15WorkEvidence,
  )
import System.Timeout (timeout)

data Step15HeraldCommand
  = CloseStep15HeraldScope
  | DrainStep15Herald AdminCorrelationId

type Step15HeraldThreadResult =
  Either SomeException (Either HeraldTcpFailure ((), HeraldRuntimeExit))

data Step15Herald
  = Step15Herald
      Step15HeraldName
      HeraldTcp
      (TMVar Step15HeraldCommand)
      (TMVar Step15HeraldThreadResult)
      Step15WorkLedger
      (TVar Bool)
      ThreadId

step15ManagedHeraldName :: Step15Herald -> Step15HeraldName
step15ManagedHeraldName (Step15Herald name _ _ _ _ _ _) = name

step15ManagedHeraldTcp :: Step15Herald -> HeraldTcp
step15ManagedHeraldTcp (Step15Herald _ tcp _ _ _ _ _) = tcp

step15ManagedHeraldWorkLedger :: Step15Herald -> Step15WorkLedger
step15ManagedHeraldWorkLedger (Step15Herald _ _ _ _ ledger _ _) = ledger

step15ManagedHeraldCommand :: Step15Herald -> TMVar Step15HeraldCommand
step15ManagedHeraldCommand (Step15Herald _ _ command _ _ _ _) = command

step15ManagedHeraldResult :: Step15Herald -> TMVar Step15HeraldThreadResult
step15ManagedHeraldResult (Step15Herald _ _ _ result _ _ _) = result

step15ManagedHeraldExpectedCancellation :: Step15Herald -> TVar Bool
step15ManagedHeraldExpectedCancellation (Step15Herald _ _ _ _ _ expected _) = expected

step15ManagedHeraldThread :: Step15Herald -> ThreadId
step15ManagedHeraldThread (Step15Herald _ _ _ _ _ _ thread) = thread

data Step15Deployment
  = Step15Deployment
      OracleTcpCluster
      Step15Herald
      Step15Herald
      Step15Herald
      Step15Herald

step15DeploymentOracleCluster :: Step15Deployment -> OracleTcpCluster
step15DeploymentOracleCluster (Step15Deployment cluster _ _ _ _) = cluster

step15H1 :: Step15Deployment -> Step15Herald
step15H1 (Step15Deployment _ herald _ _ _) = herald

step15H2 :: Step15Deployment -> Step15Herald
step15H2 (Step15Deployment _ _ herald _ _) = herald

step15H3 :: Step15Deployment -> Step15Herald
step15H3 (Step15Deployment _ _ _ herald _) = herald

step15H4 :: Step15Deployment -> Step15Herald
step15H4 (Step15Deployment _ _ _ _ herald) = herald

step15H4PeerTransportGate :: Step15Deployment -> PeerTransportGate
step15H4PeerTransportGate =
  heraldTcpPeerTransportGate . step15ManagedHeraldTcp . step15H4

closeStep15H4PeerCut :: Step15Deployment -> IO ()
closeStep15H4PeerCut deployment = do
  closePeerTransportGate (step15H4PeerTransportGate deployment)
  awaitStep15H4PeerIsolation deployment

openStep15H4PeerTransport :: Step15Deployment -> IO ()
openStep15H4PeerTransport =
  openPeerTransportGate . step15H4PeerTransportGate

reopenStep15H4PeerCut :: Step15Deployment -> IO ()
reopenStep15H4PeerCut deployment = do
  openStep15H4PeerTransport deployment
  awaitStep15PeerMesh deployment

withStep15Deployment ::
  (Step15Deployment -> IO result) ->
  IO result
withStep15Deployment =
  withStep15DeploymentFor
    Step15StandardProfile
    (const defaultRuntimeHooks)

-- | Cut only H4's independent native-health transport. Reserve an IPv4 port
-- without listening so the ordinary health manager makes real failing TCP
-- connections; no checked health observation or kernel input is fabricated.
-- The EORC watch and the other three Heralds keep their original endpoints.
withStep15H4OracleHealthCutDeployment ::
  (Step15Deployment -> IO () -> IO () -> IO result) ->
  IO result
withStep15H4OracleHealthCutDeployment use =
  bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \unreachable -> do
    Socket.bind unreachable (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
    port <-
      Socket.getSocketName unreachable >>= \case
        Socket.SockAddrInet value _ -> pure (fromIntegral value)
        address -> ioError (userError ("Step-15 health-cut endpoint is not IPv4: " <> show address))
    available <- newTVarIO True
    let configure name configuration@(HeraldRuntimeConfiguration _ _ _ _ _ _ _ _ _ _ _ _ _ _ sink _ _ _)
          | name /= Step15H4 = configuration
          | otherwise = case sink of
              Nothing -> error "Step-15 TCP scope did not install its native-health sink"
              Just forward ->
                configureHeraldRuntimeOracleHealthSink
                  ( \roundNumber cadence claims contacts -> do
                      healthy <- atomically (readTVar available)
                      let destinations =
                            if healthy
                              then contacts
                              else [checked "Step-15 unreachable native-health contact" (oracleContact (oracleContactNode contact) "127.0.0.1" port) | contact <- contacts]
                      forward roundNumber cadence claims destinations
                  )
                  configuration
    withStep15DeploymentForRuntime configure Step15StandardProfile (const defaultRuntimeHooks) $ \deployment ->
      use deployment (atomically (writeTVar available False)) (atomically (writeTVar available True))

withStep15CrashDeployment ::
  (Step15Deployment -> IO result) ->
  IO result
withStep15CrashDeployment =
  withStep15DeploymentFor
    Step15CrashAcceptanceProfile
    (const defaultRuntimeHooks)

withRepeatedRetirementDeployment :: (Step15Deployment -> IO result) -> IO result
withRepeatedRetirementDeployment = withStep15DeploymentFor RepeatedRetirementProfile (const defaultRuntimeHooks)

withRepeatedRetirementSelfFenceDeployment :: (Step15Deployment -> IO result) -> IO result
withRepeatedRetirementSelfFenceDeployment = withStep15DeploymentFor RepeatedRetirementSelfFenceProfile (const defaultRuntimeHooks)

closeStep15HeraldPeerTransport :: Step15Herald -> IO ()
closeStep15HeraldPeerTransport = closePeerTransportGate . heraldTcpPeerTransportGate . step15ManagedHeraldTcp

openStep15HeraldPeerTransport :: Step15Herald -> IO ()
openStep15HeraldPeerTransport = openPeerTransportGate . heraldTcpPeerTransportGate . step15ManagedHeraldTcp

withStep15ApplicationLossDeployment ::
  (Step15Deployment -> IO result) ->
  IO result
withStep15ApplicationLossDeployment =
  withStep15DeploymentFor
    Step15ApplicationLossProfile
    (const defaultRuntimeHooks)

withStep15DeploymentWithHooks ::
  (Step15HeraldName -> RuntimeHooks) ->
  (Step15Deployment -> IO result) ->
  IO result
withStep15DeploymentWithHooks hooksFor use = do
  withStep15DeploymentFor Step15StandardProfile hooksFor use

withStep15DeploymentFor ::
  Step15RuntimeProfile ->
  (Step15HeraldName -> RuntimeHooks) ->
  (Step15Deployment -> IO result) ->
  IO result
withStep15DeploymentFor = withStep15DeploymentForRuntime (const id)

withStep15DeploymentForRuntime ::
  (Step15HeraldName -> HeraldRuntimeConfiguration -> HeraldRuntimeConfiguration) ->
  Step15RuntimeProfile ->
  (Step15HeraldName -> RuntimeHooks) ->
  (Step15Deployment -> IO result) ->
  IO result
withStep15DeploymentForRuntime configureRuntime profile hooksFor use = do
  oracleResult <-
    withOracleTcpCluster (step15OracleTcpConfigurationFor profile) $ \cluster -> do
      _ <- awaitStep15OracleReady cluster
      let contacts = step15OracleContacts cluster
          listener =
            checked "Step-15 listener" (tcpListenEndpoint "127.0.0.1" 0)
      withManagedStep15Herald
        Step15H1
        (hooksFor Step15H1)
        (configureRuntime Step15H1)
        (step15TcpConfigurationFor profile listener contacts Step15H1 [])
        $ \h1 -> do
          let h1Seed = peerSeedFor h1
          withManagedStep15Herald
            Step15H2
            (hooksFor Step15H2)
            (configureRuntime Step15H2)
            (step15TcpConfigurationFor profile listener contacts Step15H2 [h1Seed])
            $ \h2 -> do
              let h2Seed = peerSeedFor h2
              withManagedStep15Herald
                Step15H3
                (hooksFor Step15H3)
                (configureRuntime Step15H3)
                (step15TcpConfigurationFor profile listener contacts Step15H3 [h1Seed, h2Seed])
                $ \h3 -> do
                  let h3Seed = peerSeedFor h3
                  withManagedStep15Herald
                    Step15H4
                    (hooksFor Step15H4)
                    (configureRuntime Step15H4)
                    (step15TcpConfigurationFor profile listener contacts Step15H4 [h1Seed, h2Seed, h3Seed])
                    $ \h4 -> do
                      let deployment = Step15Deployment cluster h1 h2 h3 h4
                      result <- use deployment
                      finishStep15Deployment deployment
                      pure result
  case oracleResult of
    Left failure ->
      ioError (userError ("Step-15 Oracle TCP cluster failed: " <> show failure))
    Right result -> pure result

withManagedStep15Herald ::
  Step15HeraldName ->
  RuntimeHooks ->
  (HeraldRuntimeConfiguration -> HeraldRuntimeConfiguration) ->
  HeraldTcpConfiguration ->
  (Step15Herald -> IO result) ->
  IO result
withManagedStep15Herald name hooks configureRuntime configuration =
  bracket
    (startManagedStep15Herald name hooks configureRuntime configuration)
    closeManagedStep15Herald

startManagedStep15Herald ::
  Step15HeraldName ->
  RuntimeHooks ->
  (HeraldRuntimeConfiguration -> HeraldRuntimeConfiguration) ->
  HeraldTcpConfiguration ->
  IO Step15Herald
startManagedStep15Herald name hooks configureRuntime configuration = mask $ \restore -> do
  ready <- newEmptyTMVarIO
  command <- newEmptyTMVarIO
  result <- newEmptyTMVarIO
  workLedger <- newStep15WorkLedger
  expectedCancellation <- newTVarIO False
  let runtimeScope =
        HeraldRuntimeScope
          (withHeraldRuntimeWithHooks (captureStep15RuntimeHooks workLedger hooks) . configureRuntime)
  thread <-
    forkFinally
      ( withHeraldTcpRuntimeInScopeInternal runtimeScope configuration $ \tcp -> do
          installStep15WorkProbes workLedger tcp
          awaitStep15WorkLedgerReady workLedger
          atomically (putTMVar ready tcp)
          runStep15HeraldLifecycle tcp command
      )
      (atomically . putTMVar result)
  let stopUnpublished =
        terminateStep15HeraldThread name expectedCancellation thread result
  started <-
    restore
      ( timeout startupTimeoutMicroseconds
          $ atomically
          $ (Right <$> readTMVar ready)
            `orElse` (Left <$> readTMVar result)
      )
      `onException` stopUnpublished
  case started of
    Just (Right tcp) ->
      pure
        (Step15Herald name tcp command result workLedger expectedCancellation thread)
    Just (Left outcome) ->
      failFinishedBeforeReady name outcome
    Nothing -> do
      stopUnpublished
      ioError (userError (show name <> " did not publish its TCP handle"))

runStep15HeraldLifecycle :: HeraldTcp -> TMVar Step15HeraldCommand -> IO ()
runStep15HeraldLifecycle tcp command = mask $ \restore -> do
  exitResult <- newEmptyTMVarIO
  exitWaiter <-
    forkFinally
      (restore (awaitHeraldTcpExit tcp))
      (atomically . putTMVar exitResult)
  selected <-
    restore
      ( atomically
          ( (Left <$> takeTMVar command)
              `orElse` (Right <$> readTMVar exitResult)
          )
      )
      `onException` stopExitWaiter exitWaiter exitResult
  case selected of
    Left CloseStep15HeraldScope ->
      stopExitWaiter exitWaiter exitResult
    Left (DrainStep15Herald correlation) -> do
      submitted <- requestHeraldTcpDrain tcp correlation
      unless (submitted == Queued) $ do
        stopExitWaiter exitWaiter exitResult
        ioError
          (userError ("Step-15 semantic drain was not admitted: " <> show submitted))
      observeTcpExit Step15DrainExit =<< atomically (readTMVar exitResult)
    Right outcome -> observeTcpExit Step15AutonomousExit outcome

stopExitWaiter ::
  ThreadId ->
  TMVar (Either SomeException (Either HeraldTcpFailure HeraldRuntimeExit)) ->
  IO ()
stopExitWaiter thread result = do
  killThread thread
  void (atomically (readTMVar result))

data Step15ExitExpectation
  = Step15DrainExit
  | Step15AutonomousExit

observeTcpExit ::
  Step15ExitExpectation ->
  Either SomeException (Either HeraldTcpFailure HeraldRuntimeExit) ->
  IO ()
observeTcpExit expectation = \case
  Left exception -> throwIO exception
  Right (Left failure) ->
    ioError
      (userError ("Step-15 Herald TCP scope failed: " <> show failure))
  Right (Right HeraldRuntimeDrained)
    | Step15DrainExit <- expectation -> pure ()
  Right (Right exit)
    | Step15AutonomousExit <- expectation -> pure ()
    | otherwise ->
        ioError
          (userError ("Step-15 Herald stopped without draining: " <> show exit))

closeStep15H4Scope :: Step15Deployment -> IO HeraldRuntimeExit
closeStep15H4Scope = closeStep15HeraldScope . step15H4

-- | Model an abrupt H4 process crash. The managed worker is asynchronously
-- cancelled, and its ordinary brackets close all physical TCP sockets before
-- this function returns. Only this explicitly marked cancellation is accepted
-- by later deployment cleanup; every other worker exception remains a test
-- failure.
crashStep15H4 :: Step15Deployment -> IO ()
crashStep15H4 = crashStep15Herald . step15H4

crashStep15Herald :: Step15Herald -> IO ()
crashStep15Herald herald = mask $ \_ -> do
  atomically (writeTVar (step15ManagedHeraldExpectedCancellation herald) True)
  killThread (step15ManagedHeraldThread herald)
  outcome <- awaitStep15HeraldThreadResult herald
  case outcome of
    Left exception
      | isThreadKilled exception -> pure ()
      | otherwise -> throwIO exception
    Right result ->
      ioError
        ( userError
            ( show (step15ManagedHeraldName herald)
                <> " returned normally from an abrupt crash: "
                <> show result
            )
        )

awaitStep15HeraldExit :: Step15Herald -> IO HeraldRuntimeExit
awaitStep15HeraldExit = awaitStep15HeraldResult

awaitStep15H4Exit :: Step15Deployment -> IO HeraldRuntimeExit
awaitStep15H4Exit = awaitStep15HeraldResult . step15H4

closeStep15HeraldScope :: Step15Herald -> IO HeraldRuntimeExit
closeStep15HeraldScope herald = do
  _ <-
    atomically
      (tryPutTMVar (step15ManagedHeraldCommand herald) CloseStep15HeraldScope)
  awaitStep15HeraldResult herald

finishStep15Deployment :: Step15Deployment -> IO ()
finishStep15Deployment deployment = do
  finish (adminCorrelationId 15_004) (step15H4 deployment)
  finish (adminCorrelationId 15_003) (step15H3 deployment)
  finish (adminCorrelationId 15_002) (step15H2 deployment)
  finish (adminCorrelationId 15_001) (step15H1 deployment)
  where
    finish correlation herald = do
      running <- step15ManagedHeraldIsRunning herald
      expectedCancellation <-
        atomically (readTVar (step15ManagedHeraldExpectedCancellation herald))
      if expectedCancellation && not running
        then validateStep15HeraldThreadResult herald =<< awaitStep15HeraldThreadResult herald
        else do
          submitted <-
            if running
              then
                atomically
                  ( tryPutTMVar
                      (step15ManagedHeraldCommand herald)
                      (DrainStep15Herald correlation)
                  )
              else pure False
          exit <- awaitStep15HeraldResult herald
          when submitted
            $ unless (exit == HeraldRuntimeDrained)
            $ ioError
            $ userError
              ( show (step15ManagedHeraldName herald)
                  <> " returned an unexpected cleanup exit: "
                  <> show exit
              )

step15ManagedHeraldIsRunning :: Step15Herald -> IO Bool
step15ManagedHeraldIsRunning herald =
  atomically (isEmptyTMVar (step15ManagedHeraldResult herald))

awaitStep15HeraldResult :: Step15Herald -> IO HeraldRuntimeExit
awaitStep15HeraldResult herald = do
  outcome <- awaitStep15HeraldThreadResult herald
  case outcome of
    Left exception -> throwIO exception
    Right (Left failure) ->
      ioError
        ( userError
            (show (step15ManagedHeraldName herald) <> " TCP scope failed: " <> show failure)
        )
    Right (Right ((), exit)) -> pure exit

awaitStep15HeraldThreadResult :: Step15Herald -> IO Step15HeraldThreadResult
awaitStep15HeraldThreadResult herald = do
  observed <-
    timeout
      lifecycleTimeoutMicroseconds
      (atomically (readTMVar (step15ManagedHeraldResult herald)))
  case observed of
    Nothing ->
      ioError
        (userError (show (step15ManagedHeraldName herald) <> " did not stop"))
    Just outcome -> pure outcome

closeManagedStep15Herald :: Step15Herald -> IO ()
closeManagedStep15Herald herald = do
  running <- step15ManagedHeraldIsRunning herald
  when running
    $ void
    $ atomically
    $ tryPutTMVar
      (step15ManagedHeraldCommand herald)
      CloseStep15HeraldScope
  stopped <-
    timeout
      cleanupTimeoutMicroseconds
      (atomically (readTMVar (step15ManagedHeraldResult herald)))
  case stopped of
    Just outcome -> validateStep15HeraldThreadResult herald outcome
    Nothing ->
      terminateStep15HeraldThread
        (step15ManagedHeraldName herald)
        (step15ManagedHeraldExpectedCancellation herald)
        (step15ManagedHeraldThread herald)
        (step15ManagedHeraldResult herald)

validateStep15HeraldThreadResult ::
  Step15Herald ->
  Step15HeraldThreadResult ->
  IO ()
validateStep15HeraldThreadResult herald = \case
  Left exception -> do
    expected <-
      atomically (readTVar (step15ManagedHeraldExpectedCancellation herald))
    if expected && isThreadKilled exception
      then pure ()
      else throwIO exception
  Right (Left failure) ->
    failWithStep15HeraldEvidence herald "TCP scope failed during cleanup" failure
  Right (Right _) -> pure ()

failWithStep15HeraldEvidence ::
  (Show failure) =>
  Step15Herald ->
  String ->
  failure ->
  IO value
failWithStep15HeraldEvidence herald context failure = do
  evidence <- snapshotStep15WorkEvidence (step15ManagedHeraldWorkLedger herald)
  ioError
    ( userError
        ( show (step15ManagedHeraldName herald)
            <> " "
            <> context
            <> ": "
            <> show failure
            <> "; attributable work="
            <> show (summarizeStep15WorkEvidence evidence)
            <> "; invariant faults="
            <> show (step15KernelFaultEvidence evidence)
        )
    )

terminateStep15HeraldThread ::
  Step15HeraldName ->
  TVar Bool ->
  ThreadId ->
  TMVar Step15HeraldThreadResult ->
  IO ()
terminateStep15HeraldThread name expectedCancellation thread result = do
  atomically (writeTVar expectedCancellation True)
  killThread thread
  joined <-
    timeout lifecycleTimeoutMicroseconds (atomically (readTMVar result))
  case joined of
    Just (Left exception)
      | isThreadKilled exception -> pure ()
      | otherwise -> throwIO exception
    Just (Right (Left failure)) ->
      ioError
        (userError (show name <> " failed while joining managed cancellation: " <> show failure))
    Just (Right (Right _)) -> pure ()
    Nothing ->
      ioError
        (userError (show name <> " did not join after managed-scope cancellation"))

isThreadKilled :: SomeException -> Bool
isThreadKilled exception = fromException exception == Just ThreadKilled

awaitStep15PeerMesh :: Step15Deployment -> IO ()
awaitStep15PeerMesh deployment =
  awaitStep15PeerTopology
    [ expectedPeers Step15H1 (step15H1 deployment) allNames,
      expectedPeers Step15H2 (step15H2 deployment) allNames,
      expectedPeers Step15H3 (step15H3 deployment) allNames,
      expectedPeers Step15H4 (step15H4 deployment) allNames
    ]
  where
    allNames = [Step15H1, Step15H2, Step15H3, Step15H4]

awaitStep15SurvivorMesh :: Step15Deployment -> IO ()
awaitStep15SurvivorMesh deployment =
  awaitStep15PeerTopology
    [ expectedPeers Step15H1 (step15H1 deployment) survivors,
      expectedPeers Step15H2 (step15H2 deployment) survivors,
      expectedPeers Step15H3 (step15H3 deployment) survivors
    ]
  where
    survivors = [Step15H1, Step15H2, Step15H3]

awaitStep15H4PeerIsolation :: Step15Deployment -> IO ()
awaitStep15H4PeerIsolation deployment =
  awaitStep15PeerTopology
    [ expectedPeers Step15H1 (step15H1 deployment) survivors,
      expectedPeers Step15H2 (step15H2 deployment) survivors,
      expectedPeers Step15H3 (step15H3 deployment) survivors,
      expectedPeers Step15H4 (step15H4 deployment) []
    ]
  where
    survivors = [Step15H1, Step15H2, Step15H3]

expectedPeers ::
  Step15HeraldName ->
  Step15Herald ->
  [Step15HeraldName] ->
  (Step15Herald, [HeraldEpoch])
expectedPeers local herald members =
  ( herald,
    sort
      [ step15HeraldEpoch remote
      | remote <- members,
        remote /= local
      ]
  )

awaitStep15PeerTopology :: [(Step15Herald, [HeraldEpoch])] -> IO ()
awaitStep15PeerTopology expected = do
  observed <- timeout lifecycleTimeoutMicroseconds loop
  case observed of
    Just () -> pure ()
    Nothing -> do
      bindings <- atomically (traverse (currentPeerBindings . fst) expected)
      ioError
        ( userError
            ( "Step-15 peer topology did not converge; expected remote epochs="
                <> show (fmap snd expected)
                <> ", actual="
                <> show (fmap remoteEpochs bindings)
            )
        )
  where
    loop = do
      bindings <- atomically (traverse (currentPeerBindings . fst) expected)
      if peerTopologyMatches expected bindings
        then pure ()
        else threadDelay 10_000 >> loop

currentPeerBindings :: Step15Herald -> STM [PeerBinding]
currentPeerBindings herald =
  case step15ManagedHeraldTcp herald of
    HeraldTcp _ _ _ TcpContext {tcpCurrentPeerConnections} ->
      fmap (fmap fst) (readTVar tcpCurrentPeerConnections)

peerTopologyMatches ::
  [(Step15Herald, [HeraldEpoch])] ->
  [[PeerBinding]] ->
  Bool
peerTopologyMatches expected bindings =
  fmap remoteEpochs bindings == fmap snd expected
    && and
      [ candidatesMatch leftEpoch rightEpoch leftBindings rightBindings
      | (leftIndex, leftEpoch, leftExpected, leftBindings) <- entries,
        (rightIndex, rightEpoch, rightExpected, rightBindings) <- entries,
        leftIndex < rightIndex,
        rightEpoch `elem` leftExpected,
        leftEpoch `elem` rightExpected
      ]
  where
    entries =
      [ ( index,
          step15HeraldEpoch (step15ManagedHeraldName herald),
          expectedEpochs,
          actualBindings
        )
      | (index, ((herald, expectedEpochs), actualBindings)) <-
          zip [0 :: Int ..] (zip expected bindings)
      ]

remoteEpochs :: [PeerBinding] -> [HeraldEpoch]
remoteEpochs = sort . fmap peerBindingRemoteHeraldEpoch

candidatesMatch ::
  HeraldEpoch ->
  HeraldEpoch ->
  [PeerBinding] ->
  [PeerBinding] ->
  Bool
candidatesMatch leftEpoch rightEpoch leftBindings rightBindings =
  case ( find ((== rightEpoch) . peerBindingRemoteHeraldEpoch) leftBindings,
         find ((== leftEpoch) . peerBindingRemoteHeraldEpoch) rightBindings
       ) of
    (Just left, Just right) ->
      peerBindingSelectedCandidate left == peerBindingSelectedCandidate right
    _ -> False

peerSeedFor :: Step15Herald -> ConfiguredPeerSeed
peerSeedFor herald =
  let endpoint =
        heraldTcpPeerEndpoint (heraldTcpEndpoints (step15ManagedHeraldTcp herald))
   in checked
        "Step-15 configured peer seed"
        (configuredPeerSeed (resolvedTcpHost endpoint) (resolvedTcpPort endpoint))

failFinishedBeforeReady ::
  Step15HeraldName ->
  Step15HeraldThreadResult ->
  IO value
failFinishedBeforeReady name = \case
  Left exception -> throwIO exception
  Right (Left failure) ->
    ioError
      (userError (show name <> " failed before publishing its TCP handle: " <> show failure))
  Right (Right (_, exit)) ->
    ioError
      (userError (show name <> " stopped before publishing its TCP handle: " <> show exit))

startupTimeoutMicroseconds, lifecycleTimeoutMicroseconds, cleanupTimeoutMicroseconds :: Int
startupTimeoutMicroseconds = 10_000_000
lifecycleTimeoutMicroseconds = 10_000_000
cleanupTimeoutMicroseconds = 2_000_000

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
