{-# LANGUAGE OverloadedStrings #-}

-- | Pure admission of one fresh run with an explicit fixed Herald membership.
module Eclips.Deployment.Manifest
  ( CheckedDeployment,
    checkDeployment,
    checkDeploymentWithTiming,
    deploymentTakeoverTarget,
    encodeDeployment,
    decodeDeployment,
    deploymentResidents,
    deploymentSystemBytes,
    deploymentOracleConfiguration,
    deploymentRaftGenesis,
    deploymentOracleGenesisSource,
    deploymentCheckedOracleGenesis,
    deploymentLauncherBootstrap,
    ResidentDeployment,
    residentOrdinal,
    residentEndpoints,
    residentMember,
    residentHeraldGenesis,
    residentBootstraps,
    joiningResidentDeployment,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Binary (decodeOrFail, encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word64)
import Eclips.Deployment.Configuration (DeploymentEndpoints)
import Eclips.Deployment.Configuration qualified as Configuration
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
    systemIdBytes,
  )
import Eclips.Domain.Sort.Profile
  ( predefinedCatalogueDescriptor,
    predefinedCatalogueRole,
    predefinedSortRoleTag,
    profileCatalogueDigest,
    profilePredefinedCatalogue,
  )
import Eclips.Domain.Startup
  ( allPredefinedSortRoles,
    checkInitialTopologyProjection,
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
    InitialTopologyEdgeManifest (InitialTopologySystemViewEdge),
    InitialTopologyManifest (..),
    OracleGenesisManifest (..),
    PredefinedCatalogueManifest (..),
    PrimordialDefinitionManifest (..),
    PrimordialProcessManifest (..),
    PrimordialSortWriterAuthority (..),
    checkHeraldGenesis,
    checkInitialBootstrapsWithTopology,
    checkJoiningHeraldGenesis,
    checkedConfigurationDigest,
    checkedInitialProjectionDigest,
    profilePredefinedCatalogueManifest,
  )
import Eclips.Oracle.Admission qualified as Admission
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    OracleGenesis,
    checkOracleGenesis,
    checkedOracleSystemId,
    deriveRaftConfigurationDigest,
    oracleGenesis,
    raftVoterBinding,
  )
import Eclips.Oracle.Runtime
  ( oracleRuntimeConfiguration,
    systemRuntimeElectionTimeoutSource,
  )
import Eclips.Oracle.Runtime.TCP
  ( OracleTcpClusterConfiguration,
    configureOracleTcpTiming,
    oracleTcpClusterConfiguration,
  )
import Eclips.Public.Types.Timing qualified as Timing
import Eclips.Raft.Genesis
  ( RaftGenesis,
    checkRaftGenesis,
    checkedRaftNativeConfiguration,
    raftGenesis,
  )
import Eclips.Raft.Identity (mkRaftDurationMicros, mkRaftNodeId)

deploymentSystemBytes :: CheckedDeployment -> ByteString
deploymentSystemBytes = systemIdBytes . checkedOracleSystemId . deploymentCheckedOracleGenesis

data ResidentDeployment = ResidentDeployment Word64 DeploymentEndpoints HeraldMember CheckedHeraldGenesis CheckedInitialBootstraps

residentOrdinal :: ResidentDeployment -> Word64
residentOrdinal (ResidentDeployment value _ _ _ _) = value
residentEndpoints :: ResidentDeployment -> DeploymentEndpoints
residentEndpoints (ResidentDeployment _ value _ _ _) = value
residentMember :: ResidentDeployment -> HeraldMember
residentMember (ResidentDeployment _ _ member _ _) = member
residentHeraldGenesis :: ResidentDeployment -> CheckedHeraldGenesis
residentHeraldGenesis (ResidentDeployment _ _ _ value _) = value
residentBootstraps :: ResidentDeployment -> CheckedInitialBootstraps
residentBootstraps (ResidentDeployment _ _ _ _ value) = value

data CheckedDeployment = CheckedDeployment ByteString (NonEmpty ResidentDeployment) OracleTcpClusterConfiguration CheckedOracleGenesis BootstrapManifestId RaftGenesis OracleGenesis Timing.TakeoverTarget

deploymentResidents :: CheckedDeployment -> NonEmpty ResidentDeployment
deploymentResidents (CheckedDeployment _ value _ _ _ _ _ _) = value
deploymentOracleConfiguration :: CheckedDeployment -> OracleTcpClusterConfiguration
deploymentOracleConfiguration (CheckedDeployment _ _ value _ _ _ _ _) = value
deploymentCheckedOracleGenesis :: CheckedDeployment -> CheckedOracleGenesis
deploymentCheckedOracleGenesis (CheckedDeployment _ _ _ value _ _ _ _) = value
deploymentLauncherBootstrap :: CheckedDeployment -> BootstrapManifestId
deploymentLauncherBootstrap (CheckedDeployment _ _ _ _ value _ _ _) = value

-- | A pending resident changes only the local admission identity. The original
-- checked bootstrap and topology are reused without adding genesis members.
joiningResidentDeployment :: CheckedDeployment -> DeploymentEndpoints -> Admission.HeraldAdmissionRecord -> Either String ResidentDeployment
joiningResidentDeployment deployment planes admission = do
  let founder = NonEmpty.head (deploymentResidents deployment)
      manifest = Admission.admissionRecordManifest admission
      member = HeraldMember (Admission.admissionManifestHeraldId manifest) (Admission.admissionManifestHeraldEpoch manifest)
  genesis <- check "joining Herald" (checkJoiningHeraldGenesis (residentHeraldGenesis founder) admission)
  pure (ResidentDeployment 0 planes member genesis (residentBootstraps founder))

-- | A fixed run carries one checked member list and exactly one genesis parent.
-- Later residents start without private application access or additional roots.
checkDeployment :: ByteString -> NonEmpty DeploymentEndpoints -> Either String CheckedDeployment
checkDeployment = checkDeploymentWithTiming Timing.defaultTakeoverTarget

-- | Select one immutable timing policy for every resident of the fresh run.
checkDeploymentWithTiming :: Timing.TakeoverTarget -> ByteString -> NonEmpty DeploymentEndpoints -> Either String CheckedDeployment
checkDeploymentWithTiming target seed endpoints = do
  -- Reuse the exact identity-width admission; there is no arbitrary size cap.
  _ <- check "founder entropy" (mkSystemId seed)
  if NonEmpty.length endpoints > 1 && any ((== 0) . Configuration.endpointPort) (concatMap (\planes -> [Configuration.peerListen planes, Configuration.oracleListen planes]) (NonEmpty.toList endpoints))
    then Left "fixed multi-Herald membership requires fixed peer and Oracle contact ports"
    else pure ()
  let listeners = concatMap (\planes -> [Configuration.peerListen planes, Configuration.applicationListen planes, Configuration.administrationListen planes]) (NonEmpty.toList endpoints) <> [Configuration.oracleListen (NonEmpty.head endpoints), Configuration.raftListen (NonEmpty.head endpoints)]
      fixed = filter ((/= 0) . Configuration.endpointPort) listeners
  if length fixed /= length (nub fixed) then Left "fixed deployment protocol listeners conflict" else pure ()
  system <- check "system" (mkSystemId (identity "system"))
  memberList <- traverse memberFor (NonEmpty.zip (1 :| [2 ..]) endpoints)
  let members = NonEmpty.toList memberList
      firstMember = NonEmpty.head memberList
      herald = heraldMemberId firstMember
      epoch = heraldMemberEpoch firstMember
  node <- check "Raft node" (mkRaftNodeId (identity "raft-node"))
  bootstrap <- check "launcher bootstrap" (mkBootstrapManifestId (identity "launcher-bootstrap"))
  process <- check "launcher" (mkProcessId (identity "launcher"))
  processEpoch <- check "launcher epoch" (mkProcessEpochId (identity "launcher-epoch"))
  primordialWriter <- check "primordial writer" (mkNablaId (identity "primordial-writer"))
  roots <- traverse rootFor profilePredefinedCatalogue
  let manifest =
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
      -- The sole launcher's ordinary declarations must reach every Herald's
      -- private system views, even before any remote child exists. Admission
      -- already supplies the founder-local routes and all paired root edges.
      topologyManifest =
        InitialTopologyManifest
          [ InitialTopologySystemViewEdge bootstrap (heraldMemberEpoch member) role
          | member <- members,
            heraldMemberEpoch member /= epoch,
            role <- allPredefinedSortRoles
          ]
  residents <-
    traverse
      ( \(ordinal, (member, planes)) -> do
          genesis <- check "Herald genesis" (checkHeraldGenesis deployment {deploymentLocalHeraldId = heraldMemberId member, deploymentLocalHeraldEpoch = heraldMemberEpoch member})
          bootstraps <- check "initial installation" (checkInitialBootstrapsWithTopology genesis (PrimordialProcessManifest [bootstrap]) topologyManifest)
          pure (ResidentDeployment ordinal planes member genesis bootstraps)
      )
      (NonEmpty.zip (1 :| [2 ..]) (NonEmpty.zip memberList endpoints))
  let founder = NonEmpty.head residents
      heraldGenesis = residentHeraldGenesis founder
      bootstraps = residentBootstraps founder
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
  let timing = Timing.deriveTimingPolicy target
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
  tcp <- check "Oracle TCP" (oracleTcpClusterConfiguration [runtime])
  pure (CheckedDeployment seed residents (configureOracleTcpTiming target tcp) checkedOracle bootstrap raft oracle target)
  where
    identity tag = SHA256.hash ("ECLIPS deployment identity\0" <> seed <> tag)
    memberFor (ordinal, _) = do
      let coordinate = Lazy.toStrict (encode (ordinal :: Word64))
      herald <- check "Herald" (mkHeraldId (identity ("herald" <> coordinate)))
      epoch <- check "Herald epoch" (mkHeraldEpoch (identity ("herald-epoch" <> coordinate)))
      pure (HeraldMember herald epoch)
    rootFor entry = do
      let role = predefinedCatalogueRole entry
          tag = ByteString.singleton (predefinedSortRoleTag role)
      writer <- check "root writer" (mkNablaId (identity ("writer" <> tag)))
      reader <- check "root reader" (mkDeltaId (identity ("reader" <> tag)))
      incarnation <- check "root store" (mkStoreIncarnationId (identity ("store" <> tag)))
      pure (ConfiguredRootManifest role writer UnsequencedNabla reader incarnation)

-- | This is an explicit fresh-run launch plan, never durable recovery state.
encodeDeployment :: CheckedDeployment -> ByteString
encodeDeployment (CheckedDeployment seed residents _ _ _ _ _ target) = Lazy.toStrict (encode (target, seed, fmap residentEndpoints residents))

decodeDeployment :: ByteString -> Either String CheckedDeployment
decodeDeployment bytes = case decodeOrFail (Lazy.fromStrict bytes) of
  Left (_, _, problem) -> Left problem
  Right (remaining, _, (target, seed, endpoints))
    | not (Lazy.null remaining) -> Left "trailing deployment manifest bytes"
    | otherwise -> do
        checked <- checkDeploymentWithTiming target seed endpoints
        if encodeDeployment checked == bytes then Right checked else Left "noncanonical deployment manifest"

check :: (Show problem) => String -> Either problem value -> Either String value
check context = either (Left . ((context <> ": ") <>) . show) Right

deploymentRaftGenesis :: CheckedDeployment -> RaftGenesis
deploymentRaftGenesis (CheckedDeployment _ _ _ _ _ genesis _ _) = genesis

deploymentOracleGenesisSource :: CheckedDeployment -> OracleGenesis
deploymentOracleGenesisSource (CheckedDeployment _ _ _ _ _ _ genesis _) = genesis

-- | The run policy also travels through restricted discovery to new residents.
deploymentTakeoverTarget :: CheckedDeployment -> Timing.TakeoverTarget
deploymentTakeoverTarget (CheckedDeployment _ _ _ _ _ _ _ target) = target
