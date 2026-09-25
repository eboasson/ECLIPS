{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Scoped real-transport fixtures for the Step-14 release certification.
--
-- The fixture exposes transport controls and immutable trace ledgers, never a
-- Herald kernel state. Scenario modules make progress through public EAPP,
-- EADM, EORC, and EPRP witnesses plus the explicit test latches below.
module Step14Fixtures
  ( Step14HeraldName (..),
    Step14Herald,
    step14HeraldName,
    step14HeraldTcp,
    step14HeraldTraceLedger,
    snapshotStep14HeraldTrace,
    step14HeraldTraceCursor,
    snapshotStep14HeraldTraceSince,
    semanticStep14InputBody,
    snapshotStep14HeraldPeerDials,
    snapshotStep14HeraldPeerDialsSince,
    snapshotStep14HeraldOracleManagerEvents,
    snapshotStep14HeraldOracleManagerEventsSince,
    Step14Deployment,
    step14OracleCluster,
    step14H1,
    step14H2,
    step14H3,
    step14H4,
    step14H4PeerTransportGate,
    withStep14Deployment,
    withStep14DeploymentWithOracleConfigurations,
    withStep14DeploymentWithHooks,
    step14PApplicationEndpoint,
    step14QApplicationEndpoint,
    step14PApplicationAttachment,
    step14QApplicationAttachment,
    step14ApplicationLivenessConfiguration,
    step14AdministrationEndpoint,
    awaitStep14PeerMesh,
    awaitStep14H4PeerIsolation,
    Step14PeerTraceCut (..),
    snapshotStep14PeerTraceCut,
    awaitStep14OraclePrefix,
    awaitStep14OracleExactPrefix,
    drainStep14Herald,
    newStep14Latch,
    signalStep14Latch,
    awaitStep14LatchWithin,
    newStep14Cell,
    publishStep14Cell,
    awaitStep14CellWithin,
    readStep14Cell,
    causalWaitTimeoutMicroseconds,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    readMVar,
    tryPutMVar,
    tryReadMVar,
  )
import Control.Concurrent.STM
  ( TMVar,
    TVar,
    atomically,
    modifyTVar',
    newEmptyTMVarIO,
    newTVarIO,
    readTMVar,
    readTVar,
    tryPutTMVar,
    tryReadTMVar,
  )
import Control.Exception (SomeException, onException, try)
import Control.Monad (unless, void)
import Data.List (find)
import Data.Word (Word64)
import Eclips.Application.Runtime
  ( ApplicationEndpoint,
    ApplicationLivenessConfiguration,
    applicationEndpoint,
    applicationLivenessConfigurationMicroseconds,
  )
import Eclips.Domain.Identity
  ( bootstrapManifestIdBytes,
    controlIndex,
  )
import Eclips.Herald.Administration
  ( AdminCorrelationId,
    adminCorrelationId,
  )
import Eclips.Herald.Discovery
  ( PeerBinding,
    peerBindingRemoteHeraldEpoch,
    peerBindingSelectedCandidate,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.Input
  ( ApplicationLifecycleIngress (CallApplicationLifecycle),
    ApplicationReceiptRetirementIngress (RetireApplicationReceipts),
    ApplicationRequestIngress (CallApplicationRequest),
    ApplicationRetirementWork (..),
    HeraldInput,
    HeraldInputBody (..),
    inputBody,
  )
import Eclips.Herald.OracleClient (OracleContactSet)
import Eclips.Herald.Runtime
  ( HeraldRuntimeConfiguration,
    RuntimeGeneratorSeedSource,
    checkApplicationRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
    configureHeraldRuntimeOracleVoters,
    heraldRuntimeConfiguration,
    runtimePeerWorkDelayMicroseconds,
    systemRuntimeMonotonicClock,
  )
import Eclips.Herald.Runtime.Handler (heraldRuntimeHandlers)
import Eclips.Herald.Runtime.Ingress (RuntimeSubmission (Queued))
import Eclips.Herald.Runtime.Internal.Owner
  ( RuntimeHooks (..),
    defaultRuntimeHooks,
    withHeraldRuntimeWithHooks,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    RuntimeTraceEvent (KernelEvent),
    TraceCursor,
    TraceLedger,
    snapshotTraceLedger,
    snapshotTraceLedgerSince,
    traceLedgerCursor,
  )
import Eclips.Herald.Runtime.Internal.Types (connectionRefOrdinal)
import Eclips.Herald.Runtime.TCP
  ( ConfiguredPeerSeed,
    HeraldTcpConfiguration,
    HeraldTcpFailure,
    ResolvedTcpEndpoint,
    TcpListenEndpoint,
    awaitHeraldTcpExit,
    configuredPeerSeed,
    heartbeatConfigurationMicroseconds,
    heraldTcpAdministrationEndpoint,
    heraldTcpApplicationEndpoint,
    heraldTcpConfiguration,
    heraldTcpEndpoints,
    heraldTcpPeerEndpoint,
    peerDialRetryDelayMicroseconds,
    requestHeraldTcpDrain,
    resolvedTcpHost,
    resolvedTcpPort,
    tcpListenEndpoint,
  )
import Eclips.Herald.Runtime.TCP.Internal.Facade
  ( HeraldRuntimeScope (HeraldRuntimeScope),
    heraldTcpPeerTransportGate,
    setHeraldTcpOracleManagerProbe,
    withHeraldTcpRuntimeInScopeInternal,
  )
import Eclips.Herald.Runtime.TCP.Internal.Peer
  ( setPeerDialProbeForTest,
  )
import Eclips.Herald.Runtime.TCP.Internal.Types
  ( HeraldTcp (HeraldTcp),
    OracleManagerProbeEvent,
    PeerDialProbeEvent,
    PeerTransportGate,
    TcpContext (TcpContext, tcpCurrentPeerConnections),
  )
import Eclips.Herald.Runtime.Trace
  ( HeraldRuntimeExit (HeraldRuntimeDrained),
  )
import Eclips.Oracle.Runtime
  ( OracleRuntimeConfiguration,
    oracleRuntimeAppliedControlIndex,
  )
import Eclips.Oracle.Runtime.TCP
  ( OracleTcpCluster,
    oracleTcpClusterConfiguration,
    oracleTcpNodeStatuses,
    withOracleTcpCluster,
  )
import Eclips.Oracle.Transition (initialOracle)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Protocol.Application.Types
  ( ApplicationAttachmentClaim,
    applicationAttachmentClaim,
  )
import Step12Fixtures
  ( awaitStep12ReadyLeader,
    step12CheckedRoutedOracleGenesis,
    step12H1GeneratorSeedSource,
    step12H1Genesis,
    step12H1HeraldEpoch,
    step12H1ProcessBootstrapId,
    step12H1RoutedBootstraps,
    step12H2GeneratorSeedSource,
    step12H2Genesis,
    step12H2HeraldEpoch,
    step12H2ProcessBootstrapId,
    step12H2RoutedBootstraps,
    step12H3GeneratorSeedSource,
    step12H3Genesis,
    step12H3HeraldEpoch,
    step12H3RoutedBootstraps,
    step12H4GeneratorSeedSource,
    step12H4Genesis,
    step12H4HeraldEpoch,
    step12H4RoutedBootstraps,
    step12OracleContacts,
    step12RoutedOracleRuntimeConfigurations,
  )
import System.Timeout (timeout)

-- | Observe the semantic work in a trace entry independently of its receipt
-- metadata. Standalone retirement remains visible as maintenance; this helper
-- never changes or resubmits the recorded kernel input.
semanticStep14InputBody :: HeraldInput -> HeraldInputBody
semanticStep14InputBody input = case inputBody input of
  ApplicationReceiptRetirementInput (RetireApplicationReceipts binding session _ (Just work)) -> case work of
    RetiringApplicationRequest request operation ->
      ApplicationRequestInput (CallApplicationRequest binding session request operation)
    RetiringApplicationLifecycle request locator command ->
      ApplicationLifecycleInput (CallApplicationLifecycle binding session request locator command)
  body -> body

data Step14HeraldName
  = Step14H1
  | Step14H2
  | Step14H3
  | Step14H4
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data Step14Herald
  = Step14Herald
      Step14HeraldName
      HeraldTcp
      TraceLedger
      (TVar [PeerDialProbeEvent])
      (TVar [OracleManagerProbeEvent])

step14HeraldName :: Step14Herald -> Step14HeraldName
step14HeraldName (Step14Herald name _ _ _ _) = name

step14HeraldTcp :: Step14Herald -> HeraldTcp
step14HeraldTcp (Step14Herald _ tcp _ _ _) = tcp

step14HeraldTraceLedger :: Step14Herald -> TraceLedger
step14HeraldTraceLedger (Step14Herald _ _ ledger _ _) = ledger

step14HeraldPeerDials :: Step14Herald -> TVar [PeerDialProbeEvent]
step14HeraldPeerDials (Step14Herald _ _ _ peerDials _) = peerDials

step14HeraldOracleManagerEvents ::
  Step14Herald ->
  TVar [OracleManagerProbeEvent]
step14HeraldOracleManagerEvents (Step14Herald _ _ _ _ events) = events

snapshotStep14HeraldTrace :: Step14Herald -> IO [RuntimeTraceEvent]
snapshotStep14HeraldTrace herald =
  atomically (snapshotTraceLedger (step14HeraldTraceLedger herald))

step14HeraldTraceCursor :: Step14Herald -> IO TraceCursor
step14HeraldTraceCursor herald =
  atomically (traceLedgerCursor (step14HeraldTraceLedger herald))

snapshotStep14HeraldPeerDials :: Step14Herald -> IO [PeerDialProbeEvent]
snapshotStep14HeraldPeerDials herald =
  fmap reverse (atomically (readTVar (step14HeraldPeerDials herald)))

snapshotStep14HeraldPeerDialsSince ::
  Int ->
  Step14Herald ->
  IO (Int, [PeerDialProbeEvent])
snapshotStep14HeraldPeerDialsSince cursor herald = do
  peerDials <- snapshotStep14HeraldPeerDials herald
  pure (length peerDials, drop cursor peerDials)

snapshotStep14HeraldOracleManagerEvents ::
  Step14Herald ->
  IO [OracleManagerProbeEvent]
snapshotStep14HeraldOracleManagerEvents herald =
  fmap
    reverse
    (atomically (readTVar (step14HeraldOracleManagerEvents herald)))

snapshotStep14HeraldOracleManagerEventsSince ::
  Int ->
  Step14Herald ->
  IO (Int, [OracleManagerProbeEvent])
snapshotStep14HeraldOracleManagerEventsSince cursor herald = do
  events <- snapshotStep14HeraldOracleManagerEvents herald
  pure (length events, drop cursor events)

-- | One causally coherent pre-cut observation of a Herald's transport leases,
-- trace frontier, physical-dial frontier, and Oracle-manager frontier. The
-- fixed deployment snapshot below reads all four Heralds in one STM
-- transaction, so a lease cannot disappear between the failure-evidence
-- capture and the cursors from which its loss and replacement work are
-- counted.
data Step14PeerTraceCut = Step14PeerTraceCut
  { step14PeerTraceCutLeases :: [(PeerBinding, Word64)],
    step14PeerTraceCutCursor :: TraceCursor,
    step14PeerTraceCutPeerDialCursor :: Int,
    step14PeerTraceCutOracleManagerCursor :: Int
  }

snapshotStep14HeraldTraceSince ::
  TraceCursor ->
  Step14Herald ->
  IO (TraceCursor, [RuntimeTraceEvent])
snapshotStep14HeraldTraceSince cursor herald =
  atomically
    (snapshotTraceLedgerSince cursor (step14HeraldTraceLedger herald))

data Step14Deployment
  = Step14Deployment
      OracleTcpCluster
      Step14Herald
      Step14Herald
      Step14Herald
      Step14Herald

step14OracleCluster :: Step14Deployment -> OracleTcpCluster
step14OracleCluster (Step14Deployment cluster _ _ _ _) = cluster

step14H1 :: Step14Deployment -> Step14Herald
step14H1 (Step14Deployment _ herald _ _ _) = herald

step14H2 :: Step14Deployment -> Step14Herald
step14H2 (Step14Deployment _ _ herald _ _) = herald

step14H3 :: Step14Deployment -> Step14Herald
step14H3 (Step14Deployment _ _ _ herald _) = herald

step14H4 :: Step14Deployment -> Step14Herald
step14H4 (Step14Deployment _ _ _ _ herald) = herald

step14H4PeerTransportGate :: Step14Deployment -> PeerTransportGate
step14H4PeerTransportGate =
  heraldTcpPeerTransportGate . step14HeraldTcp . step14H4

withStep14Deployment ::
  (Step14Deployment -> IO result) ->
  IO result
withStep14Deployment =
  withStep14DeploymentWithOracleConfigurations id

-- | Transform the complete raw routed voter configurations before cluster
-- startup. Tests use this only for public Oracle-runtime observation seams;
-- Raft membership, genesis, and transport remain checked by the TCP cluster.
withStep14DeploymentWithOracleConfigurations ::
  ([OracleRuntimeConfiguration] -> [OracleRuntimeConfiguration]) ->
  (Step14Deployment -> IO result) ->
  IO result
withStep14DeploymentWithOracleConfigurations transform =
  withStep14DeploymentWithOracleConfigurationsAndHooks
    transform
    (const defaultRuntimeHooks)

withStep14DeploymentWithHooks ::
  (Step14HeraldName -> RuntimeHooks) ->
  (Step14Deployment -> IO result) ->
  IO result
withStep14DeploymentWithHooks =
  withStep14DeploymentWithOracleConfigurationsAndHooks id

withStep14DeploymentWithOracleConfigurationsAndHooks ::
  ([OracleRuntimeConfiguration] -> [OracleRuntimeConfiguration]) ->
  (Step14HeraldName -> RuntimeHooks) ->
  (Step14Deployment -> IO result) ->
  IO result
withStep14DeploymentWithOracleConfigurationsAndHooks transform hooksFor use = do
  let configuration =
        checked
          "Step-14 transformed Oracle TCP cluster"
          ( oracleTcpClusterConfiguration
              (transform step12RoutedOracleRuntimeConfigurations)
          )
  oracleResult <-
    withOracleTcpCluster configuration $ \cluster -> do
      _ <- awaitStep12ReadyLeader cluster
      runFourHeralds hooksFor cluster use
  case oracleResult of
    Left failure ->
      ioError (userError ("Step-14 Oracle TCP cluster failed to start: " <> show failure))
    Right result -> pure result

runFourHeralds ::
  (Step14HeraldName -> RuntimeHooks) ->
  OracleTcpCluster ->
  (Step14Deployment -> IO result) ->
  IO result
runFourHeralds hooksFor cluster use = do
  let contacts = step12OracleContacts cluster
      listener = checked "Step-14 listener" (tcpListenEndpoint "127.0.0.1" 0)
      h1Configuration =
        step14TcpConfiguration
          listener
          contacts
          step12H1Genesis
          step12H1RoutedBootstraps
          step12H1GeneratorSeedSource
          []
  h1Result <-
    withStep14Herald Step14H1 (hooksFor Step14H1) h1Configuration $ \h1 ->
      withRequiredDrain (adminCorrelationId 14_001) h1 $ do
        let h1Seed = peerSeedFor h1
            h2Configuration =
              step14TcpConfiguration
                listener
                contacts
                step12H2Genesis
                step12H2RoutedBootstraps
                step12H2GeneratorSeedSource
                [h1Seed]
        h2Result <-
          withStep14Herald Step14H2 (hooksFor Step14H2) h2Configuration $ \h2 ->
            withRequiredDrain (adminCorrelationId 14_002) h2 $ do
              let h2Seed = peerSeedFor h2
                  h3Configuration =
                    step14TcpConfiguration
                      listener
                      contacts
                      step12H3Genesis
                      step12H3RoutedBootstraps
                      step12H3GeneratorSeedSource
                      [h1Seed, h2Seed]
              h3Result <-
                withStep14Herald Step14H3 (hooksFor Step14H3) h3Configuration $ \h3 ->
                  withRequiredDrain (adminCorrelationId 14_003) h3 $ do
                    let h3Seed = peerSeedFor h3
                        h4Configuration =
                          step14TcpConfiguration
                            listener
                            contacts
                            step12H4Genesis
                            step12H4RoutedBootstraps
                            step12H4GeneratorSeedSource
                            [h1Seed, h2Seed, h3Seed]
                    h4Result <-
                      withStep14Herald Step14H4 (hooksFor Step14H4) h4Configuration $ \h4 ->
                        withRequiredDrain (adminCorrelationId 14_004) h4 $ do
                          use (Step14Deployment cluster h1 h2 h3 h4)
                    requireDrainedScope Step14H4 h4Result
              requireDrainedScope Step14H3 h3Result
        requireDrainedScope Step14H2 h2Result
  requireDrainedScope Step14H1 h1Result

-- | Require semantic drain on the successful path. If a scenario assertion or
-- hook fails, attempt the same drain while preserving the original exception;
-- each enclosing Herald then gets the same opportunity during stack unwind.
withRequiredDrain ::
  AdminCorrelationId ->
  Step14Herald ->
  IO result ->
  IO result
withRequiredDrain correlation herald action = do
  result <- action `onException` bestEffortDrain
  drainStep14Herald correlation herald
  pure result
  where
    bestEffortDrain =
      void
        ( timeout
            exceptionDrainTimeoutMicroseconds
            ( try (drainStep14Herald correlation herald) ::
                IO (Either SomeException ())
            )
        )

withStep14Herald ::
  Step14HeraldName ->
  RuntimeHooks ->
  HeraldTcpConfiguration ->
  (Step14Herald -> IO result) ->
  IO (Either HeraldTcpFailure (result, HeraldRuntimeExit))
withStep14Herald name hooks configuration use = do
  trace <- newEmptyMVar
  peerDials <- newTVarIO []
  oracleManagerEvents <- newTVarIO []
  let capturedHooks = captureTraceLedger trace hooks
      runtimeScope =
        HeraldRuntimeScope (withHeraldRuntimeWithHooks capturedHooks)
  result <-
    withHeraldTcpRuntimeInScopeInternal runtimeScope configuration $ \tcp -> do
      setPeerDialProbeForTest
        tcp
        (Just (\event -> atomically (modifyTVar' peerDials (event :))))
      setHeraldTcpOracleManagerProbe
        tcp
        ( Just
            (\event -> atomically (modifyTVar' oracleManagerEvents (event :)))
        )
      ledger <- readMVar trace
      use (Step14Herald name tcp ledger peerDials oracleManagerEvents)
  case result of
    Left failure -> do
      initialized <- tryReadMVar trace
      case initialized of
        Nothing -> pure result
        Just ledger -> do
          events <- atomically (snapshotTraceLedger ledger)
          case [ (input, fault)
               | KernelEvent _ (KernelTraceStepped _ _ input (Left fault)) <- events
               ] of
            [] -> pure result
            faults ->
              ioError
                ( userError
                    ( show name
                        <> " kernel invariant trace: "
                        <> show (last faults)
                        <> "; TCP failure: "
                        <> show failure
                    )
                )
    Right _ -> pure result

captureTraceLedger :: MVar TraceLedger -> RuntimeHooks -> RuntimeHooks
captureTraceLedger target hooks =
  hooks
    { hookRuntimeInitialized = \instant contacts seed applicationRecovery peerRecovery initialEffects ledger -> do
        void (tryPutMVar target ledger)
        hookRuntimeInitialized hooks instant contacts seed applicationRecovery peerRecovery initialEffects ledger
    }

step14TcpConfiguration ::
  TcpListenEndpoint ->
  OracleContactSet ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  RuntimeGeneratorSeedSource ->
  [ConfiguredPeerSeed] ->
  HeraldTcpConfiguration
step14TcpConfiguration listener contacts genesis bootstraps generatorSeedSource seeds =
  heraldTcpConfiguration
    (step14RuntimeConfiguration contacts genesis bootstraps generatorSeedSource)
    listener
    listener
    listener
    seeds
    (checked "Step-14 peer retry delay" (peerDialRetryDelayMicroseconds 10_000))
    (checked "Step-14 heartbeat" (heartbeatConfigurationMicroseconds 60_000_000 5_000_000))

-- | Shared application retry, recovery, and heartbeat policy for the long
-- running Step-14 real-transport scenarios.
step14ApplicationLivenessConfiguration :: ApplicationLivenessConfiguration
step14ApplicationLivenessConfiguration =
  checked
    "Step-14 application liveness"
    (applicationLivenessConfigurationMicroseconds 1_000 120_000_000 60_000_000 5_000_000)

step14RuntimeConfiguration ::
  OracleContactSet ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  RuntimeGeneratorSeedSource ->
  HeraldRuntimeConfiguration
step14RuntimeConfiguration contacts genesis bootstraps generatorSeedSource =
  configureHeraldRuntimeOracleVoters
    (Voter.oracleVoterConfiguration initial)
    (Voter.oracleReplicaRegistrations initial)
    $ heraldRuntimeConfiguration
      genesis
      bootstraps
      contacts
      generatorSeedSource
      systemRuntimeMonotonicClock
      (runtimePeerWorkDelayMicroseconds 0)
      (heraldRuntimeHandlers (const (pure ())))
      (checked "Step-14 application recovery" (checkApplicationRecoveryConfiguration 30_000_000))
      (checked "Step-14 peer recovery" (checkPeerRecoveryConfiguration 30_000_000))
  where
    initial = checked "Step-14 initial Oracle authority" (initialOracle step12CheckedRoutedOracleGenesis)

peerSeedFor :: Step14Herald -> ConfiguredPeerSeed
peerSeedFor herald =
  let endpoint = heraldTcpPeerEndpoint (heraldTcpEndpoints (step14HeraldTcp herald))
   in checked
        "Step-14 configured peer seed"
        (configuredPeerSeed (resolvedTcpHost endpoint) (resolvedTcpPort endpoint))

step14PApplicationEndpoint :: Step14Deployment -> ApplicationEndpoint
step14PApplicationEndpoint = applicationEndpointFor . step14H1

step14QApplicationEndpoint :: Step14Deployment -> ApplicationEndpoint
step14QApplicationEndpoint = applicationEndpointFor . step14H2

applicationEndpointFor :: Step14Herald -> ApplicationEndpoint
applicationEndpointFor herald =
  let endpoint =
        heraldTcpApplicationEndpoint
          (heraldTcpEndpoints (step14HeraldTcp herald))
   in checked
        "Step-14 application endpoint"
        (applicationEndpoint (resolvedTcpHost endpoint) (resolvedTcpPort endpoint))

step14PApplicationAttachment :: ApplicationAttachmentClaim
step14PApplicationAttachment =
  checked
    "Step-14 P application attachment"
    (applicationAttachmentClaim (bootstrapManifestIdBytes step12H1ProcessBootstrapId))

step14QApplicationAttachment :: ApplicationAttachmentClaim
step14QApplicationAttachment =
  checked
    "Step-14 Q application attachment"
    (applicationAttachmentClaim (bootstrapManifestIdBytes step12H2ProcessBootstrapId))

step14AdministrationEndpoint :: Step14Herald -> ResolvedTcpEndpoint
step14AdministrationEndpoint =
  heraldTcpAdministrationEndpoint . heraldTcpEndpoints . step14HeraldTcp

-- | Wait for the fixed four-member deployment to expose one current physical
-- peer lease in each direction. Structural schedules begin only after this
-- transport-startup witness, so startup reconnect churn is not confused with
-- a semantic publication failure.
awaitStep14PeerMesh :: Step14Deployment -> IO ()
awaitStep14PeerMesh deployment = do
  observed <- timeout causalWaitTimeoutMicroseconds loop
  unless (observed == Just ()) $ do
    bindings <- traverse currentPeerBindings heralds
    ioError
      ( userError
          ( "Step-14 peer mesh did not converge; current bindings="
              <> show bindings
          )
      )
  where
    loop = do
      bindings <- traverse currentPeerBindings heralds
      if fmap length bindings == replicate 4 3
        && candidatesAgree bindings
        then pure ()
        else threadDelay 10_000 >> loop
    heralds =
      fmap
        (step14HeraldTcp . ($ deployment))
        [step14H1, step14H2, step14H3, step14H4]
    currentPeerBindings
      (HeraldTcp _ _ _ TcpContext {tcpCurrentPeerConnections}) =
        fmap (fmap fst) (atomically (readTVar tcpCurrentPeerConnections))
    candidatesAgree bindings =
      and
        [ matchingCandidate leftEpoch rightEpoch leftBindings rightBindings
        | (leftEpoch, leftBindings, leftIndex) <- indexed bindings,
          (rightEpoch, rightBindings, rightIndex) <- indexed bindings,
          leftIndex < rightIndex
        ]
    indexed bindings =
      zipWith3 (,,) heraldEpochs bindings [0 :: Int ..]
    heraldEpochs =
      [ step12H1HeraldEpoch,
        step12H2HeraldEpoch,
        step12H3HeraldEpoch,
        step12H4HeraldEpoch
      ]
    matchingCandidate leftEpoch rightEpoch leftBindings rightBindings =
      case (bindingFor rightEpoch leftBindings, bindingFor leftEpoch rightBindings) of
        (Just left, Just right) ->
          peerBindingSelectedCandidate left == peerBindingSelectedCandidate right
        _ -> False
    bindingFor remote = find ((== remote) . peerBindingRemoteHeraldEpoch)

-- | Wait until H4's reversible peer cut has removed every physical H4 lease
-- while the H1-H3 peer sub-mesh remains intact. A later mesh witness therefore
-- proves a real reconnect rather than observing lease rows whose socket close
-- is still being retired.
awaitStep14H4PeerIsolation :: Step14Deployment -> IO ()
awaitStep14H4PeerIsolation deployment = do
  observed <- timeout causalWaitTimeoutMicroseconds loop
  unless (observed == Just ())
    $ ioError (userError "Step-14 H4 peer isolation did not converge")
  where
    loop = do
      counts <- traverse currentPeerCount heralds
      if counts == [2, 2, 2, 0]
        then pure ()
        else threadDelay 10_000 >> loop
    heralds =
      fmap
        (step14HeraldTcp . ($ deployment))
        [step14H1, step14H2, step14H3, step14H4]
    currentPeerCount
      (HeraldTcp _ _ _ TcpContext {tcpCurrentPeerConnections}) =
        length <$> atomically (readTVar tcpCurrentPeerConnections)

-- | Atomically snapshot every transport projection's exact logical binding,
-- opaque owner-source ordinal, and matching trace/physical-work cursors. This
-- is failure evidence for ownership races; it does not expose or read a kernel
-- state.
snapshotStep14PeerTraceCut :: Step14Deployment -> IO [Step14PeerTraceCut]
snapshotStep14PeerTraceCut deployment =
  atomically (traverse currentPeerTraceCut heralds)
  where
    heralds =
      fmap ($ deployment) [step14H1, step14H2, step14H3, step14H4]
    currentPeerTraceCut herald = do
      leases <- currentPeerLeases (step14HeraldTcp herald)
      cursor <- traceLedgerCursor (step14HeraldTraceLedger herald)
      peerDialCursor <- length <$> readTVar (step14HeraldPeerDials herald)
      oracleManagerCursor <-
        length <$> readTVar (step14HeraldOracleManagerEvents herald)
      pure
        ( Step14PeerTraceCut
            leases
            cursor
            peerDialCursor
            oracleManagerCursor
        )
    currentPeerLeases
      (HeraldTcp _ _ _ TcpContext {tcpCurrentPeerConnections}) =
        fmap
          (fmap (\(binding, reference) -> (binding, connectionRefOrdinal reference)))
          (readTVar tcpCurrentPeerConnections)

-- | Wait until every Oracle voter has applied at least the requested prefix.
-- The deadline is only a failure bound; progress is witnessed by each public
-- runtime status.
awaitStep14OraclePrefix :: Word64 -> Step14Deployment -> IO ()
awaitStep14OraclePrefix expected deployment = do
  observed <- timeout causalWaitTimeoutMicroseconds loop
  unless (observed == Just ()) $ do
    statuses <- oracleTcpNodeStatuses (step14OracleCluster deployment)
    ioError
      ( userError
          ( "Step-14 Oracle voters did not reach prefix "
              <> show expected
              <> "; statuses: "
              <> show statuses
          )
      )
  where
    loop = do
      statuses <- fmap (fmap snd) (oracleTcpNodeStatuses (step14OracleCluster deployment))
      if not (null statuses)
        && all
          ((>= controlIndex expected) . oracleRuntimeAppliedControlIndex)
          statuses
        then pure ()
        else threadDelay 10_000 >> loop

-- | Wait until every voter exposes one exact applied prefix. This is suitable
-- only at an explicit checkpoint that prevents the next proposal; observing an
-- exact value by timing alone is not a semantic witness.
awaitStep14OracleExactPrefix :: Word64 -> Step14Deployment -> IO ()
awaitStep14OracleExactPrefix expected deployment = do
  observed <- timeout causalWaitTimeoutMicroseconds loop
  unless (observed == Just ())
    $ ioError
      (userError ("Step-14 Oracle voters did not remain exactly at prefix " <> show expected))
  where
    loop = do
      statuses <- fmap (fmap snd) (oracleTcpNodeStatuses (step14OracleCluster deployment))
      if not (null statuses)
        && all
          ((== controlIndex expected) . oracleRuntimeAppliedControlIndex)
          statuses
        then pure ()
        else threadDelay 10_000 >> loop

drainStep14Herald :: AdminCorrelationId -> Step14Herald -> IO ()
drainStep14Herald correlation herald = do
  submitted <- requestHeraldTcpDrain (step14HeraldTcp herald) correlation
  unless (submitted == Queued) $ do
    ioError
      ( userError
          ( show (step14HeraldName herald)
              <> " semantic drain was not admitted: "
              <> show submitted
          )
      )
  observed <- awaitHeraldTcpExit (step14HeraldTcp herald)
  case observed of
    Left failure ->
      ioError
        ( userError
            (show (step14HeraldName herald) <> " failed while draining: " <> show failure)
        )
    Right HeraldRuntimeDrained -> pure ()
    Right other ->
      ioError
        ( userError
            ( show (step14HeraldName herald)
                <> " stopped without semantic drain: "
                <> show other
            )
        )

requireDrainedScope ::
  Step14HeraldName ->
  Either HeraldTcpFailure (result, HeraldRuntimeExit) ->
  IO result
requireDrainedScope name = \case
  Left failure ->
    ioError (userError (show name <> " TCP scope failed: " <> show failure))
  Right (result, HeraldRuntimeDrained) -> pure result
  Right (_, other) ->
    ioError
      ( userError
          (show name <> " TCP scope returned an unexpected exit: " <> show other)
      )

newStep14Latch :: IO (MVar ())
newStep14Latch = newEmptyMVar

signalStep14Latch :: MVar () -> IO ()
signalStep14Latch latch = void (tryPutMVar latch ())

awaitStep14LatchWithin :: Int -> MVar () -> IO Bool
awaitStep14LatchWithin timeoutMicroseconds latch =
  maybe False (const True) <$> timeout timeoutMicroseconds (readMVar latch)

newStep14Cell :: IO (TMVar value)
newStep14Cell = newEmptyTMVarIO

publishStep14Cell :: TMVar value -> value -> IO Bool
publishStep14Cell cell value = atomically (tryPutTMVar cell value)

awaitStep14CellWithin :: Int -> TMVar value -> IO (Maybe value)
awaitStep14CellWithin timeoutMicroseconds cell =
  timeout timeoutMicroseconds (atomically (readTMVar cell))

readStep14Cell :: TMVar value -> IO (Maybe value)
readStep14Cell cell = atomically (tryReadTMVar cell)

-- These are failure bounds around causally witnessed state, never scheduling
-- evidence. A failed scenario gets a separate, shorter budget per Herald so
-- one stuck semantic drain cannot prevent the remaining scopes from unwinding.
causalWaitTimeoutMicroseconds, exceptionDrainTimeoutMicroseconds :: Int
causalWaitTimeoutMicroseconds = 30_000_000
exceptionDrainTimeoutMicroseconds = 10_000_000

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
