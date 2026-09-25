{-# LANGUAGE OverloadedStrings #-}

-- | Explicit fresh-system startup through the existing supervised TCP owners.
module FounderDeployment
  ( withFounderDeployment,
    withFounderDeploymentAtControl,
  )
where

import Eclips.Application.Types.Lifecycle (ConnectionDescriptor, connectionDescriptor, heraldLocator)
import Eclips.Domain.Identity (ControlIndex, bootstrapManifestIdBytes, controlIndex, heraldEpochBytes, systemIdBytes)
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Herald.Administration (adminCorrelationId)
import Eclips.Herald.Runtime (systemRuntimeGeneratorSeedSource)
import Eclips.Herald.Runtime.TCP (heraldTcpApplicationEndpoint, heraldTcpEndpoints, resolvedTcpHost, resolvedTcpPort, tcpListenEndpoint, withHeraldTcpRuntime)
import Eclips.Oracle.Genesis (checkedOracleActiveHeralds, checkedOracleSystemId)
import Eclips.Oracle.Runtime.TCP (withOracleTcpCluster)
import FounderConfiguration
  ( founderBootstraps,
    founderConfigurationWithCheckpoint,
    founderHeraldGenesis,
    founderLauncherBootstrap,
    founderOracleConfiguration,
    founderOracleGenesis,
  )
import HelloDeployment
  ( awaitExactlyOneReadyLeader,
    awaitOracleControlIndex,
    drainAndJoin,
    dynamicOracleContacts,
    newOracleWorkObservation,
    observeOracleWork,
    requireDrainedScope,
    tcpConfiguration,
  )
import System.Entropy (getEntropy)

-- | Create one run with one Herald, one voter and one initial launcher. The
-- callback completes the label decision and collection, then explicitly ends
-- the launcher: three semantic Oracle entries before orderly shutdown.
withFounderDeployment :: (ConnectionDescriptor -> IO result) -> IO result
withFounderDeployment = withFounderDeploymentAtControl (controlIndex 3)

-- | The integration tour supplies its exact expected lifecycle/label prefix,
-- excluding separately observed receipt-only maintenance entries.
withFounderDeploymentAtControl :: ControlIndex -> (ConnectionDescriptor -> IO result) -> IO result
withFounderDeploymentAtControl terminal use = do
  seed <- getEntropy 32
  observation <- newOracleWorkObservation
  configuration <- either (ioError . userError) pure (founderConfigurationWithCheckpoint (observeOracleWork observation) seed)
  listener <- either (ioError . userError . show) pure (tcpListenEndpoint "127.0.0.1" 0)
  result <- withOracleTcpCluster (founderOracleConfiguration configuration) $ \cluster -> do
    awaitExactlyOneReadyLeader cluster
    let herald =
          tcpConfiguration
            listener
            (dynamicOracleContacts cluster)
            (founderOracleGenesis configuration)
            (founderHeraldGenesis configuration)
            (founderBootstraps configuration)
            systemRuntimeGeneratorSeedSource
            []
    started <- withHeraldTcpRuntime herald $ \tcp -> do
      let endpoint = heraldTcpApplicationEndpoint (heraldTcpEndpoints tcp)
          oracle = founderOracleGenesis configuration
      locator <- either (ioError . userError . show) pure (heraldLocator (resolvedTcpHost endpoint) (resolvedTcpPort endpoint))
      epoch <- case checkedOracleActiveHeralds oracle of
        [member] -> pure (heraldMemberEpoch member)
        _ -> ioError (userError "founder has no unique resident")
      descriptor <- either (ioError . userError . show) pure (connectionDescriptor locator (systemIdBytes (checkedOracleSystemId oracle)) (heraldEpochBytes epoch) (bootstrapManifestIdBytes (founderLauncherBootstrap configuration)))
      value <- use descriptor
      awaitOracleControlIndex terminal observation cluster
      drainAndJoin "founder" (adminCorrelationId 1) tcp
      pure value
    requireDrainedScope "founder" started
  either (ioError . userError . ("founder Oracle TCP startup: " <>) . show) pure result
