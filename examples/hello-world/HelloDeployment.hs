{-# LANGUAGE OverloadedStrings #-}

-- | Low-level runtime helpers used by the explicit founder integration tour.
module HelloDeployment
  ( tcpConfiguration,
    dynamicOracleContacts,
    awaitExactlyOneReadyLeader,
    OracleWorkObservation,
    newOracleWorkObservation,
    observeOracleWork,
    awaitOracleControlIndex,
    drainAndJoin,
    requireDrainedScope,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, throwSTM, writeTVar)
import Control.Monad (unless)
import Data.List.NonEmpty qualified as NonEmpty
import Data.String (fromString)
import Data.Word (Word64)
import Eclips.Domain.Identity (ControlIndex, controlIndex, controlIndexWord64)
import Eclips.Herald.Administration
  ( AdminCorrelationId,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
  )
import Eclips.Herald.OracleClient
  ( OracleContactSet,
    oracleContact,
    oracleContactSet,
    oracleNodeClaim,
  )
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
import Eclips.Herald.Runtime.Ingress
  ( RuntimeSubmission (Queued),
  )
import Eclips.Herald.Runtime.TCP
  ( ConfiguredPeerSeed,
    HeraldTcp,
    HeraldTcpConfiguration,
    HeraldTcpFailure,
    TcpListenEndpoint,
    awaitHeraldTcpExit,
    configureHeraldTcpTiming,
    heartbeatConfigurationMicroseconds,
    heraldTcpConfiguration,
    peerDialRetryDelayMicroseconds,
    requestHeraldTcpDrain,
  )
import Eclips.Herald.Runtime.Trace (HeraldRuntimeExit (HeraldRuntimeDrained))
import Eclips.Oracle.Genesis (CheckedOracleGenesis)
import Eclips.Oracle.Projection (appliedEntryCommand, appliedEntryControlIndex, appliedEntryProjectionEvents, appliedEntryReceiptRetirementProgress)
import Eclips.Oracle.Runtime
  ( OracleVoterCheckpoint (..),
    oracleRuntimeAppliedControlIndex,
    oracleRuntimeServiceReady,
  )
import Eclips.Oracle.Runtime.TCP
  ( OracleTcpCluster,
    oracleTcpEndpointHost,
    oracleTcpEndpointPort,
    oracleTcpNodeStatuses,
    oracleTcpOracleContacts,
  )
import Eclips.Oracle.Transition (initialOracle)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Public.Types.Timing qualified as Timing
import Eclips.Raft.Identity (raftNodeIdBytes)

tcpConfiguration ::
  TcpListenEndpoint ->
  OracleContactSet ->
  CheckedOracleGenesis ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  RuntimeGeneratorSeedSource ->
  [ConfiguredPeerSeed] ->
  HeraldTcpConfiguration
tcpConfiguration listener contacts oracleGenesis genesis bootstraps generatorSeedSource seeds =
  configureHeraldTcpTiming Timing.defaultTakeoverTarget
    $ heraldTcpConfiguration
      (runtimeConfiguration contacts oracleGenesis genesis bootstraps generatorSeedSource)
      listener
      listener
      listener
      seeds
      (checked "hello peer retry delay" (peerDialRetryDelayMicroseconds (Timing.timingReconnectInitialMicroseconds timing)))
      (checked "hello heartbeat" (heartbeatConfigurationMicroseconds (Timing.timingHeartbeatIdleMicroseconds timing) (Timing.timingHeartbeatReplyMicroseconds timing)))
  where
    timing = Timing.deriveTimingPolicy Timing.defaultTakeoverTarget

runtimeConfiguration ::
  OracleContactSet ->
  CheckedOracleGenesis ->
  CheckedHeraldGenesis ->
  CheckedInitialBootstraps ->
  RuntimeGeneratorSeedSource ->
  HeraldRuntimeConfiguration
runtimeConfiguration contacts oracleGenesis genesis bootstraps generatorSeedSource =
  configureHeraldRuntimeOracleVoters (Voter.oracleVoterConfiguration initial) (Voter.oracleReplicaRegistrations initial)
    $ heraldRuntimeConfiguration
      genesis
      bootstraps
      contacts
      generatorSeedSource
      systemRuntimeMonotonicClock
      (runtimePeerWorkDelayMicroseconds 0)
      (heraldRuntimeHandlers (const (pure ())))
      (checked "hello application recovery" (checkApplicationRecoveryConfiguration (Timing.timingApplicationRecoveryGraceMicroseconds timing)))
      (checked "hello peer recovery" (checkPeerRecoveryConfiguration (Timing.timingPeerRecoveryGraceMicroseconds timing)))
  where
    timing = Timing.deriveTimingPolicy Timing.defaultTakeoverTarget
    initial = checked "hello initial Oracle authority" (initialOracle oracleGenesis)

dynamicOracleContacts :: OracleTcpCluster -> OracleContactSet
dynamicOracleContacts cluster =
  checked
    "dynamic hello Oracle contact set"
    (oracleContactSet (NonEmpty.fromList contacts))
  where
    contacts =
      [ checked
          "dynamic hello Oracle contact"
          ( oracleContact
              (checked "dynamic hello Oracle node claim" (oracleNodeClaim (raftNodeIdBytes node)))
              (fromString (oracleTcpEndpointHost endpoint))
              (oracleTcpEndpointPort endpoint)
          )
      | (node, endpoint) <- oracleTcpOracleContacts cluster
      ]

awaitExactlyOneReadyLeader :: OracleTcpCluster -> IO ()
awaitExactlyOneReadyLeader cluster = do
  statuses <- oracleTcpNodeStatuses cluster
  case [node | (node, status) <- statuses, oracleRuntimeServiceReady status] of
    [_] -> pure ()
    _ -> threadDelay 10_000 >> awaitExactlyOneReadyLeader cluster

-- Only two counters are retained. Piggyback metadata on a semantic command
-- remains semantic work; only the explicit receipt-only origin is maintenance.
newtype OracleWorkObservation = OracleWorkObservation (TVar (ControlIndex, Word64))

newOracleWorkObservation :: IO OracleWorkObservation
newOracleWorkObservation = OracleWorkObservation <$> newTVarIO (controlIndex 0, 0)

observeOracleWork :: OracleWorkObservation -> OracleVoterCheckpoint -> IO ()
observeOracleWork (OracleWorkObservation observation) = \case
  OracleApplicationApplied entry _ -> atomically $ do
    (previous, maintenance) <- readTVar observation
    let index = appliedEntryControlIndex entry
    unless
      (controlIndexWord64 index == controlIndexWord64 previous + 1)
      (throwSTM (userError "hello Oracle observation repeated or skipped a control entry"))
    receiptOnly <- case (appliedEntryCommand entry, appliedEntryReceiptRetirementProgress entry, appliedEntryProjectionEvents entry) of
      (Nothing, Just _, []) -> pure True
      (Just _, _, _) -> pure False
      _ -> throwSTM (userError "hello Oracle observation has an unexpected control entry")
    writeTVar observation (index, maintenance + if receiptOnly then 1 else 0)
  OracleVoterConfigurationCommitted {} ->
    ioError (userError "hello workflow unexpectedly changed voter configuration")
  _ -> pure ()

-- The supplied prefix counts semantic control entries. Maintenance has its own
-- typed count, so it cannot hide a repeated or additional semantic operation.
awaitOracleControlIndex :: ControlIndex -> OracleWorkObservation -> OracleTcpCluster -> IO ()
awaitOracleControlIndex terminal observation@(OracleWorkObservation counts) cluster = do
  statuses <- oracleTcpNodeStatuses cluster
  (observedThrough, maintenance) <- readTVarIO counts
  let semantic = controlIndexWord64 observedThrough - maintenance
      converged =
        semantic == controlIndexWord64 terminal
          && all
            ((== observedThrough) . oracleRuntimeAppliedControlIndex . snd)
            statuses
  unless (semantic <= controlIndexWord64 terminal)
    $ ioError
    $ userError
    $ "hello workflow exceeded its exact semantic control count: " <> show (semantic, terminal, maintenance)
  -- The observer runs before publication of that control entry. A replica may
  -- therefore temporarily lag this observation; convergence waits for both.
  unless converged $ do
    threadDelay 10_000
    awaitOracleControlIndex terminal observation cluster

drainAndJoin :: String -> AdminCorrelationId -> HeraldTcp -> IO ()
drainAndJoin label correlation tcp = do
  submission <- requestHeraldTcpDrain tcp correlation
  unless (submission == Queued)
    $ ioError
    $ userError
    $ label <> " semantic drain was not admitted: " <> show submission
  exit <- awaitHeraldTcpExit tcp
  case exit of
    Left failure -> ioError (userError (label <> " failed while draining: " <> show failure))
    Right HeraldRuntimeDrained -> pure ()
    Right other -> ioError (userError (label <> " stopped without semantic drain: " <> show other))

requireDrainedScope :: String -> Either HeraldTcpFailure (result, HeraldRuntimeExit) -> IO result
requireDrainedScope label = \case
  Left failure -> ioError (userError (label <> " TCP scope failed: " <> show failure))
  Right (result, HeraldRuntimeDrained) -> pure result
  Right (_, other) -> ioError (userError (label <> " TCP scope returned an unexpected exit: " <> show other))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
