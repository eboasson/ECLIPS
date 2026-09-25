{-# LANGUAGE OverloadedStrings #-}

-- | Pure admission of one fresh founder and its sole genesis launcher.
module FounderConfiguration
  ( FounderConfiguration,
    founderConfiguration,
    founderConfigurationWithCheckpoint,
    founderHeraldGenesis,
    founderBootstraps,
    founderOracleConfiguration,
    founderOracleGenesis,
    founderLauncherBootstrap,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    NablaSequencing (UnsequencedNabla),
    controlIndex,
    genesisAuthorityEpoch,
    mkBootstrapManifestId,
    mkDeltaId,
    mkHeraldEpoch,
    mkHeraldId,
    mkNablaId,
    mkProcessEpochId,
    mkProcessId,
    mkStoreIncarnationId,
    mkSystemId,
    nablaSequence,
  )
import Eclips.Domain.Sort.Profile
  ( predefinedCatalogueDescriptor,
    predefinedCatalogueRole,
    predefinedSortRoleTag,
    profileCatalogueDigest,
    profilePredefinedCatalogue,
  )
import Eclips.Domain.Startup
  ( checkInitialTopologyProjection,
    configuredRootBootstrap,
    deriveInitialProjectionDigest,
    genesisPredefinedOccurrenceSet,
    materializeConfiguredProcessBootstrap,
    mkConfiguredProcessBootstrap,
  )
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    ConfiguredProcessManifest (..),
    ConfiguredRootManifest (..),
    DeploymentManifest (..),
    HeraldMember (..),
    InitialTopologyManifest (..),
    OracleGenesisManifest (..),
    PredefinedCatalogueManifest (..),
    PrimordialDefinitionManifest (..),
    PrimordialProcessManifest (..),
    PrimordialSortWriterAuthority (..),
    checkHeraldGenesis,
    checkInitialBootstrapsWithTopology,
    checkedConfigurationDigest,
    checkedInitialProjectionDigest,
    profilePredefinedCatalogueManifest,
  )
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    checkOracleGenesis,
    deriveRaftConfigurationDigest,
    oracleGenesis,
    raftVoterBinding,
  )
import Eclips.Oracle.Runtime
  ( OracleVoterCheckpoint,
    configureOracleRuntimeVoterCheckpoint,
    oracleRuntimeConfiguration,
    systemRuntimeElectionTimeoutSource,
  )
import Eclips.Oracle.Runtime.TCP
  ( OracleTcpClusterConfiguration,
    oracleTcpClusterConfiguration,
  )
import Eclips.Public.Types.Timing qualified as Timing
import Eclips.Raft.Genesis
  ( checkRaftGenesis,
    checkedRaftNativeConfiguration,
    raftGenesis,
  )
import Eclips.Raft.Identity (mkRaftDurationMicros, mkRaftNodeId)

data FounderConfiguration
  = FounderConfiguration
      CheckedHeraldGenesis
      CheckedInitialBootstraps
      OracleTcpClusterConfiguration
      CheckedOracleGenesis
      BootstrapManifestId

-- | A host supplies one fresh 32-byte seed before this pure boundary. Distinct
-- domain tags derive the run, host, epoch, launcher and root identities. The
-- runtime's separate generator seed is acquired by its existing entropy owner.
founderConfiguration :: ByteString -> Either String FounderConfiguration
founderConfiguration = founderConfigurationWithCheckpoint (const (pure ()))

founderConfigurationWithCheckpoint :: (OracleVoterCheckpoint -> IO ()) -> ByteString -> Either String FounderConfiguration
founderConfigurationWithCheckpoint checkpoint seed = do
  -- Reuse the exact identity-width admission; there is no arbitrary size cap.
  _ <- check "founder entropy" (mkSystemId seed)
  system <- check "system" (mkSystemId (identity "system"))
  herald <- check "Herald" (mkHeraldId (identity "herald"))
  epoch <- check "Herald epoch" (mkHeraldEpoch (identity "herald-epoch"))
  node <- check "Raft node" (mkRaftNodeId (identity "raft-node"))
  bootstrap <- check "launcher bootstrap" (mkBootstrapManifestId (identity "launcher-bootstrap"))
  process <- check "launcher" (mkProcessId (identity "launcher"))
  processEpoch <- check "launcher epoch" (mkProcessEpochId (identity "launcher-epoch"))
  primordialWriter <- check "primordial writer" (mkNablaId (identity "primordial-writer"))
  roots <- traverse rootFor profilePredefinedCatalogue
  let member = HeraldMember herald epoch
      members = [member]
      manifest =
        ConfiguredProcessManifest bootstrap process processEpoch epoch roots
      deployment =
        DeploymentManifest
          { deploymentSystemId = system,
            deploymentLocalHeraldId = herald,
            deploymentLocalHeraldEpoch = epoch,
            deploymentActiveHeralds = members,
            deploymentPredefinedCatalogue = profilePredefinedCatalogueManifest,
            deploymentPrimordialWriterAuthority =
              PrimordialSortWriterAuthority primordialWriter genesisAuthorityEpoch epoch,
            deploymentPrimordialDefinitions =
              [ PrimordialDefinitionManifest
                  (catalogueManifestRole entry)
                  (catalogueManifestDescriptorBytes entry)
                  (nablaSequence ordinal)
              | (ordinal, entry) <- zip [1 ..] profilePredefinedCatalogueManifest
              ],
            deploymentConfiguredProcesses = [manifest],
            deploymentOracleGenesis =
              OracleGenesisManifest
                { oracleGenesisSystemId = system,
                  oracleGenesisActiveHeralds = members,
                  oracleGenesisCatalogueDigest = profileCatalogueDigest,
                  oracleGenesisControlIndex = controlIndex 0
                }
          }
      -- Paired roots and the resident system views are installed by admission.
      topologyManifest = InitialTopologyManifest []
  heraldGenesis <- check "Herald genesis" (checkHeraldGenesis deployment)
  bootstraps <-
    check
      "launcher installation"
      (checkInitialBootstrapsWithTopology heraldGenesis (PrimordialProcessManifest [bootstrap]) topologyManifest)
  configured <-
    maybe
      (Left "founder launcher roots rejected")
      Right
      ( mkConfiguredProcessBootstrap
          bootstrap
          process
          processEpoch
          epoch
          [ configuredRootBootstrap
              (configuredRootRole root)
              (configuredWriterNabla root)
              (configuredWriterSequencing root)
              (configuredReaderDelta root)
              (configuredReaderStoreIncarnation root)
          | root <- roots
          ]
      )
  applied <-
    check
      "launcher materialization"
      (materializeConfiguredProcessBootstrap (controlIndex 0) (genesisPredefinedOccurrenceSet system) configured)
  topology <- check "initial topology" (checkInitialTopologyProjection system members [applied] topologyManifest)
  let projectionDigest = deriveInitialProjectionDigest [applied] topology
  if projectionDigest == checkedInitialProjectionDigest bootstraps
    then Right ()
    else Left "founder Herald and Oracle initial projections differ"
  let timing = Timing.deriveTimingPolicy Timing.defaultTakeoverTarget
  heartbeat <- check "heartbeat" (mkRaftDurationMicros (Timing.timingRaftHeartbeatMicroseconds timing))
  electionLower <- check "election lower" (mkRaftDurationMicros (Timing.timingRaftElectionLowerMicroseconds timing))
  electionUpper <- check "election upper" (mkRaftDurationMicros (Timing.timingRaftElectionUpperMicroseconds timing))
  let raft = raftGenesis node [node] heartbeat electionLower electionUpper
  native <- checkedRaftNativeConfiguration <$> check "Raft genesis" (checkRaftGenesis raft)
  let bindings = [raftVoterBinding node epoch]
      oracle =
        oracleGenesis
          system
          members
          profileCatalogueDigest
          (fmap predefinedCatalogueDescriptor profilePredefinedCatalogue)
          [applied]
          (checkedConfigurationDigest heraldGenesis)
          topology
          projectionDigest
          bindings
          native
          (deriveRaftConfigurationDigest system bindings native)
  checkedOracle <- check "Oracle genesis" (checkOracleGenesis oracle)
  runtime <- check "Oracle runtime" (oracleRuntimeConfiguration raft oracle epoch systemRuntimeElectionTimeoutSource)
  tcp <- check "Oracle TCP" (oracleTcpClusterConfiguration [configureOracleRuntimeVoterCheckpoint checkpoint runtime])
  pure (FounderConfiguration heraldGenesis bootstraps tcp checkedOracle bootstrap)
  where
    identity tag = SHA256.hash ("ECLIPS founder identity\0" <> seed <> tag)
    rootFor entry = do
      let role = predefinedCatalogueRole entry
          tag = ByteString.singleton (predefinedSortRoleTag role)
      writer <- check "root writer" (mkNablaId (identity ("writer" <> tag)))
      reader <- check "root reader" (mkDeltaId (identity ("reader" <> tag)))
      incarnation <- check "root store" (mkStoreIncarnationId (identity ("store" <> tag)))
      pure (ConfiguredRootManifest role writer UnsequencedNabla reader incarnation)

founderHeraldGenesis :: FounderConfiguration -> CheckedHeraldGenesis
founderHeraldGenesis (FounderConfiguration value _ _ _ _) = value

founderBootstraps :: FounderConfiguration -> CheckedInitialBootstraps
founderBootstraps (FounderConfiguration _ value _ _ _) = value

founderOracleConfiguration :: FounderConfiguration -> OracleTcpClusterConfiguration
founderOracleConfiguration (FounderConfiguration _ _ value _ _) = value

founderOracleGenesis :: FounderConfiguration -> CheckedOracleGenesis
founderOracleGenesis (FounderConfiguration _ _ _ value _) = value

founderLauncherBootstrap :: FounderConfiguration -> BootstrapManifestId
founderLauncherBootstrap (FounderConfiguration _ _ _ _ value) = value

check :: (Show problem) => String -> Either problem value -> Either String value
check context = either (Left . ((context <> ": ") <>) . show) Right
