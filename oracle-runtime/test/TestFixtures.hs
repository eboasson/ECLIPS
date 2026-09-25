{-# LANGUAGE OverloadedStrings #-}

module TestFixtures
  ( checked,
    identifierBytes,
    fixtureSystemId,
    fixtureMembers,
    fixtureHeraldId,
    fixtureHeraldEpoch,
    fixtureProcessEpoch,
    fixtureConfigurationDigest,
    fixtureRaftNodes,
    fixtureInitialLeaderNode,
    fixtureReplacementNode,
    fixtureFollowerNode,
    fixtureOracleGenesis,
    fixtureInvalidOracleGenesis,
    fixtureCheckedGenesis,
    fixtureSingletonCheckedGenesis,
    fixtureSingletonTcpConfiguration,
    fixtureRecordedSingletonTcpConfiguration,
    fixtureVoterChangeOracleGenesis,
    fixtureVoterChangeRaftGenesis,
    fixtureVoterChangeTcpConfiguration,
    fixtureVoterChangeRuntimeConfiguration,
    fixtureFirstRaftGenesis,
    fixtureInvalidRaftGenesis,
    fixtureRuntimeConfigurations,
    fixtureFirstRuntimeConfiguration,
    fixtureRuntimeConfigurationForSample,
    fixtureTcpConfiguration,
    fixtureRecordedTcpConfiguration,
    validStart,
    alternateStart,
    validOracleCommand,
    alternateOracleCommand,
    awaitReadyLeader,
    awaitAppliedPrefix,
    awaitNodesAppliedPrefix,
  )
where

import Control.Concurrent (threadDelay)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Map.Strict qualified as Map
import Data.Word
  ( Word64,
    Word8,
  )
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    DeltaId,
    HeraldEpoch,
    HeraldId,
    NablaId,
    NablaSequencing (UnsequencedNabla),
    ProcessEpochId,
    ProcessId,
    StoreIncarnationId,
    SystemId,
    controlIndex,
    mkBootstrapManifestId,
    mkDeltaId,
    mkHeraldEpoch,
    mkHeraldId,
    mkNablaId,
    mkProcessEpochId,
    mkProcessId,
    mkStoreIncarnationId,
    mkSystemId,
  )
import Eclips.Domain.ProcessStart (ProcessStart, processStart)
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
  )
import Eclips.Domain.Sort.Profile
  ( predefinedCatalogueDescriptor,
    predefinedCatalogueRole,
    profileCatalogueDigest,
    profilePredefinedCatalogue,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    CheckedInitialTopologyProjection,
    ConfigurationDigest,
    ConfiguredProcessBootstrap,
    HeraldMember (..),
    InitialTopologyManifest (..),
    checkInitialTopologyProjection,
    configuredRootBootstrap,
    deriveInitialProjectionDigest,
    genesisPredefinedOccurrenceSet,
    materializeConfiguredProcessBootstrap,
    mkConfigurationDigest,
    mkConfiguredProcessBootstrap,
    predefinedSortRoleTag,
  )
import Eclips.Oracle.Command
  ( OracleCommand,
    startProcessEpochCommand,
  )
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    OracleGenesis,
    RaftVoterBinding,
    checkOracleGenesis,
    deriveRaftConfigurationDigest,
    oracleGenesis,
    raftVoterBinding,
  )
import Eclips.Oracle.Runtime
  ( OracleRuntimeConfiguration,
    OracleRuntimeRecordingMode (OracleCaptureHistory),
    OracleRuntimeStatus,
    configureOracleRuntimeRecording,
    oracleRuntimeAppliedControlIndex,
    oracleRuntimeConfiguration,
    oracleRuntimeNode,
    oracleRuntimeServiceReady,
    runtimeElectionTimeoutSource,
  )
import Eclips.Oracle.Runtime.TCP
  ( OracleTcpCluster,
    OracleTcpClusterConfiguration,
    oracleTcpClusterConfiguration,
    oracleTcpNodeStatuses,
  )
import Eclips.Raft.Genesis
  ( CheckedRaftGenesis,
    RaftGenesis,
    RaftNativeConfiguration,
    checkRaftGenesis,
    checkedRaftNativeConfiguration,
    raftGenesis,
  )
import Eclips.Raft.Identity
  ( RaftNodeId,
    mkRaftDurationMicros,
    mkRaftNodeId,
  )
import System.Timeout (timeout)

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

identifierBytes :: Word8 -> ByteString
identifierBytes seed =
  ByteString.pack [seed + fromIntegral index | index <- [(0 :: Int) .. 31]]

fixtureSystemId :: SystemId
fixtureSystemId = checked "SystemId" (mkSystemId (identifierBytes 1))

fixtureHeraldId :: HeraldId
fixtureHeraldId = checked "HeraldId" (mkHeraldId (identifierBytes 10))

fixtureHeraldEpoch :: HeraldEpoch
fixtureHeraldEpoch = checked "HeraldEpoch" (mkHeraldEpoch (identifierBytes 11))

secondHeraldEpoch :: HeraldEpoch
secondHeraldEpoch = checked "HeraldEpoch" (mkHeraldEpoch (identifierBytes 13))

thirdHeraldEpoch :: HeraldEpoch
thirdHeraldEpoch = checked "HeraldEpoch" (mkHeraldEpoch (identifierBytes 15))

fixtureMembers :: [HeraldMember]
fixtureMembers =
  [ HeraldMember fixtureHeraldId fixtureHeraldEpoch,
    HeraldMember
      (checked "HeraldId" (mkHeraldId (identifierBytes 12)))
      secondHeraldEpoch,
    HeraldMember
      (checked "HeraldId" (mkHeraldId (identifierBytes 14)))
      thirdHeraldEpoch
  ]

fixtureProcessEpoch :: ProcessEpochId
fixtureProcessEpoch = checked "ProcessEpochId" (mkProcessEpochId (identifierBytes 31))

remoteProcessEpoch :: ProcessEpochId
remoteProcessEpoch = checked "ProcessEpochId" (mkProcessEpochId (identifierBytes 111))

fixtureBootstraps :: [AppliedProcessBootstrap]
fixtureBootstraps =
  [ materialize 20 21 fixtureProcessEpoch fixtureHeraldEpoch 40,
    materialize 100 101 remoteProcessEpoch secondHeraldEpoch 120
  ]
  where
    materialize manifestSeed processSeed epoch residence rootSeed =
      checked
        "genesis configured-process materialization"
        ( materializeConfiguredProcessBootstrap
            (controlIndex 0)
            (genesisPredefinedOccurrenceSet fixtureSystemId)
            (configuredProcess manifestSeed processSeed epoch residence rootSeed)
        )

fixtureConfigurationDigest :: ConfigurationDigest
fixtureConfigurationDigest = checked "ConfigurationDigest" (mkConfigurationDigest (identifierBytes 7))

configuredProcess ::
  Word8 ->
  Word8 ->
  ProcessEpochId ->
  HeraldEpoch ->
  Word8 ->
  ConfiguredProcessBootstrap
configuredProcess manifestSeed processSeed epoch residence rootSeed =
  maybe
    (error "complete configured process was rejected")
    id
    ( mkConfiguredProcessBootstrap
        (bootstrapManifestId manifestSeed)
        (processId processSeed)
        epoch
        residence
        [ configuredRootBootstrap
            role
            (nablaId (rootSeed + 2 * tag))
            UnsequencedNabla
            (deltaId (rootSeed + 2 * tag + 1))
            (storeIncarnationId (rootSeed + 40 + tag))
        | entry <- profilePredefinedCatalogue,
          let role = predefinedCatalogueRole entry
              tag = predefinedSortRoleTag role
        ]
    )

fixtureTopology :: CheckedInitialTopologyProjection
fixtureTopology =
  checked
    "initial topology"
    ( checkInitialTopologyProjection
        fixtureSystemId
        fixtureMembers
        fixtureBootstraps
        (InitialTopologyManifest [])
    )

fixtureDescriptors :: [CanonicalDescriptor]
fixtureDescriptors = fmap predefinedCatalogueDescriptor profilePredefinedCatalogue

fixtureRaftNodes :: [RaftNodeId]
fixtureRaftNodes = fmap raftNodeId [2, 3, 4]

fixtureFollowerNode :: RaftNodeId
fixtureFollowerNode = raftNodeId 4

fixtureInitialLeaderNode :: RaftNodeId
fixtureInitialLeaderNode = raftNodeId 2

fixtureReplacementNode :: RaftNodeId
fixtureReplacementNode = raftNodeId 3

fixtureNativeConfiguration :: RaftNativeConfiguration
fixtureNativeConfiguration =
  case fixtureRaftGenesisesChecked of
    first : _ -> checkedRaftNativeConfiguration first
    [] -> error "three-voter fixture unexpectedly had no Raft genesis"

fixtureRaftGenesises :: [RaftGenesis]
fixtureRaftGenesises =
  [ raftGenesis
      local
      fixtureRaftNodes
      heartbeat
      electionLower
      electionUpper
  | local <- fixtureRaftNodes
  ]
  where
    heartbeat = checked "heartbeat" (mkRaftDurationMicros 20000)
    electionLower = checked "election lower" (mkRaftDurationMicros 60000)
    electionUpper = checked "election upper" (mkRaftDurationMicros 120000)

fixtureRaftGenesisesChecked :: [CheckedRaftGenesis]
fixtureRaftGenesisesChecked = fmap (checked "Raft genesis" . checkRaftGenesis) fixtureRaftGenesises

fixtureBindings :: [RaftVoterBinding]
fixtureBindings =
  zipWith raftVoterBinding fixtureRaftNodes (fmap heraldMemberEpoch fixtureMembers)

fixtureOracleGenesis :: OracleGenesis
fixtureOracleGenesis =
  oracleGenesis
    fixtureSystemId
    fixtureMembers
    profileCatalogueDigest
    fixtureDescriptors
    fixtureBootstraps
    fixtureConfigurationDigest
    fixtureTopology
    (deriveInitialProjectionDigest fixtureBootstraps fixtureTopology)
    fixtureBindings
    fixtureNativeConfiguration
    (deriveRaftConfigurationDigest fixtureSystemId fixtureBindings fixtureNativeConfiguration)

fixtureInvalidOracleGenesis :: OracleGenesis
fixtureInvalidOracleGenesis =
  oracleGenesis
    fixtureSystemId
    fixtureMembers
    profileCatalogueDigest
    fixtureDescriptors
    fixtureBootstraps
    fixtureConfigurationDigest
    fixtureTopology
    (deriveInitialProjectionDigest fixtureBootstraps fixtureTopology)
    []
    fixtureNativeConfiguration
    (deriveRaftConfigurationDigest fixtureSystemId [] fixtureNativeConfiguration)

fixtureCheckedGenesis :: CheckedOracleGenesis
fixtureCheckedGenesis = checked "Oracle genesis" (checkOracleGenesis fixtureOracleGenesis)

-- One Herald owns the genesis launcher and the sole native voter.
fixtureSingletonOracleGenesis :: OracleGenesis
fixtureSingletonOracleGenesis =
  oracleGenesis
    fixtureSystemId
    members
    profileCatalogueDigest
    fixtureDescriptors
    bootstraps
    fixtureConfigurationDigest
    topology
    (deriveInitialProjectionDigest bootstraps topology)
    bindings
    native
    (deriveRaftConfigurationDigest fixtureSystemId bindings native)
  where
    members = take 1 fixtureMembers
    bootstraps = take 1 fixtureBootstraps
    bindings = take 1 fixtureBindings
    native = checkedRaftNativeConfiguration (checked "singleton Raft genesis" (checkRaftGenesis singletonRaftGenesis))
    topology = checked "singleton initial topology" (checkInitialTopologyProjection fixtureSystemId members bootstraps (InitialTopologyManifest []))

singletonRaftGenesis :: RaftGenesis
singletonRaftGenesis =
  raftGenesis
    fixtureInitialLeaderNode
    [fixtureInitialLeaderNode]
    (checked "heartbeat" (mkRaftDurationMicros 20000))
    (checked "election lower" (mkRaftDurationMicros 60000))
    (checked "election upper" (mkRaftDurationMicros 120000))

fixtureSingletonCheckedGenesis :: CheckedOracleGenesis
fixtureSingletonCheckedGenesis = checked "singleton Oracle genesis" (checkOracleGenesis fixtureSingletonOracleGenesis)

fixtureSingletonTcpConfiguration :: OracleTcpClusterConfiguration
fixtureSingletonTcpConfiguration =
  checked
    "singleton TCP cluster configuration"
    (oracleTcpClusterConfiguration [fixtureSingletonRuntimeConfiguration])

fixtureRecordedSingletonTcpConfiguration :: OracleTcpClusterConfiguration
fixtureRecordedSingletonTcpConfiguration =
  checked "recorded singleton TCP cluster configuration" (oracleTcpClusterConfiguration [configureOracleRuntimeRecording OracleCaptureHistory fixtureSingletonRuntimeConfiguration])

fixtureSingletonRuntimeConfiguration :: OracleRuntimeConfiguration
fixtureSingletonRuntimeConfiguration =
  checked "singleton runtime configuration" (oracleRuntimeConfiguration singletonRaftGenesis fixtureSingletonOracleGenesis fixtureHeraldEpoch (runtimeElectionTimeoutSource (pure 0)))

-- Three ordinary active Heralds, initially served by one native voter. Learner
-- registration and role changes occur later through production commands.
fixtureVoterChangeOracleGenesis :: OracleGenesis
fixtureVoterChangeOracleGenesis =
  oracleGenesis
    fixtureSystemId
    fixtureMembers
    profileCatalogueDigest
    fixtureDescriptors
    fixtureBootstraps
    fixtureConfigurationDigest
    fixtureTopology
    (deriveInitialProjectionDigest fixtureBootstraps fixtureTopology)
    bindings
    native
    (deriveRaftConfigurationDigest fixtureSystemId bindings native)
  where
    bindings = take 1 fixtureBindings
    native = checkedRaftNativeConfiguration (checked "initial voter genesis" (checkRaftGenesis singletonRaftGenesis))

fixtureVoterChangeRaftGenesis :: RaftNodeId -> RaftGenesis
fixtureVoterChangeRaftGenesis local =
  raftGenesis
    local
    [fixtureInitialLeaderNode]
    (checked "heartbeat" (mkRaftDurationMicros 20000))
    (checked "election lower" (mkRaftDurationMicros 60000))
    (checked "election upper" (mkRaftDurationMicros 120000))

fixtureVoterChangeTcpConfiguration :: OracleTcpClusterConfiguration
fixtureVoterChangeTcpConfiguration = checked "voter-change TCP" (oracleTcpClusterConfiguration [fixtureVoterChangeRuntimeConfiguration])

fixtureVoterChangeRuntimeConfiguration :: OracleRuntimeConfiguration
fixtureVoterChangeRuntimeConfiguration =
  checked
    "initial voter runtime"
    (oracleRuntimeConfiguration singletonRaftGenesis fixtureVoterChangeOracleGenesis fixtureHeraldEpoch (runtimeElectionTimeoutSource (pure 0)))

fixtureRuntimeConfigurations :: [OracleRuntimeConfiguration]
fixtureRuntimeConfigurations = runtimeConfigurations fixtureOracleGenesis

runtimeConfigurations :: OracleGenesis -> [OracleRuntimeConfiguration]
runtimeConfigurations genesis =
  zipWith3
    configuration
    fixtureRaftGenesises
    (fmap heraldMemberEpoch fixtureMembers)
    [0, 30000, 60000]
  where
    configuration raft cohost sample =
      checked
        "runtime configuration"
        ( oracleRuntimeConfiguration
            raft
            genesis
            cohost
            (runtimeElectionTimeoutSource (pure sample))
        )

fixtureFirstRuntimeConfiguration :: OracleRuntimeConfiguration
fixtureFirstRuntimeConfiguration =
  case fixtureRuntimeConfigurations of
    first : _ -> first
    [] -> error "three-voter fixture unexpectedly had no runtime configuration"

fixtureFirstRaftGenesis :: RaftGenesis
fixtureFirstRaftGenesis =
  case fixtureRaftGenesises of
    first : _ -> first
    [] -> error "three-voter fixture unexpectedly had no Raft genesis"

fixtureInvalidRaftGenesis :: RaftGenesis
fixtureInvalidRaftGenesis =
  raftGenesis
    (raftNodeId 99)
    fixtureRaftNodes
    (checked "heartbeat" (mkRaftDurationMicros 20000))
    (checked "election lower" (mkRaftDurationMicros 60000))
    (checked "election upper" (mkRaftDurationMicros 120000))

fixtureRuntimeConfigurationForSample :: Word64 -> OracleRuntimeConfiguration
fixtureRuntimeConfigurationForSample sample =
  case (fixtureRaftGenesises, fixtureMembers) of
    (raft : _, member : _) ->
      checked
        "runtime configuration"
        ( oracleRuntimeConfiguration
            raft
            fixtureOracleGenesis
            member.heraldMemberEpoch
            (runtimeElectionTimeoutSource (pure sample))
        )
    _ -> error "three-voter fixture unexpectedly incomplete"

fixtureTcpConfiguration :: OracleTcpClusterConfiguration
fixtureTcpConfiguration =
  checked
    "TCP cluster configuration"
    (oracleTcpClusterConfiguration fixtureRuntimeConfigurations)

fixtureRecordedTcpConfiguration :: OracleTcpClusterConfiguration
fixtureRecordedTcpConfiguration =
  checked "recorded TCP cluster configuration" (oracleTcpClusterConfiguration (map (configureOracleRuntimeRecording OracleCaptureHistory) fixtureRuntimeConfigurations))

validOracleCommand :: OracleCommand
validOracleCommand = startProcessEpochCommand validStart

alternateOracleCommand :: OracleCommand
alternateOracleCommand = startProcessEpochCommand alternateStart

validStart :: ProcessStart
validStart = processStart (processId 161) (checked "ProcessEpochId" (mkProcessEpochId (identifierBytes 171))) fixtureHeraldEpoch

alternateStart :: ProcessStart
alternateStart = processStart (processId 163) (checked "ProcessEpochId" (mkProcessEpochId (identifierBytes 172))) fixtureHeraldEpoch

awaitReadyLeader :: OracleTcpCluster -> IO RaftNodeId
awaitReadyLeader cluster = do
  found <- timeout 5000000 loop
  maybe (error "timed out waiting for a service-ready Raft leader") pure found
  where
    loop = do
      statuses <- oracleTcpNodeStatuses cluster
      case [oracleRuntimeNode status | (_, status) <- statuses, oracleRuntimeServiceReady status] of
        [node] -> pure node
        _ -> threadDelay 10000 >> loop

awaitAppliedPrefix :: OracleTcpCluster -> Word64 -> IO [OracleRuntimeStatus]
awaitAppliedPrefix cluster expected = do
  found <- timeout 5000000 loop
  maybe (error "timed out waiting for the Oracle prefix") pure found
  where
    loop = do
      statuses <- fmap (fmap snd) (oracleTcpNodeStatuses cluster)
      if any ((== controlIndex expected) . oracleRuntimeAppliedControlIndex) statuses
        then pure statuses
        else threadDelay 10000 >> loop

awaitNodesAppliedPrefix ::
  OracleTcpCluster ->
  [RaftNodeId] ->
  Word64 ->
  IO [(RaftNodeId, OracleRuntimeStatus)]
awaitNodesAppliedPrefix cluster expectedNodes expected = do
  found <- timeout 5000000 loop
  maybe (error "timed out waiting for designated Oracle prefixes") pure found
  where
    loop = do
      statuses <- oracleTcpNodeStatuses cluster
      let byNode = Map.fromList statuses
          reached node =
            maybe
              False
              ((== controlIndex expected) . oracleRuntimeAppliedControlIndex)
              (Map.lookup node byNode)
      if all reached expectedNodes
        then pure statuses
        else threadDelay 10000 >> loop

raftNodeId :: Word8 -> RaftNodeId
raftNodeId seed = checked "RaftNodeId" (mkRaftNodeId (identifierBytes seed))

nablaId :: Word8 -> NablaId
nablaId seed = checked "NablaId" (mkNablaId (identifierBytes seed))

bootstrapManifestId :: Word8 -> BootstrapManifestId
bootstrapManifestId seed = checked "BootstrapManifestId" (mkBootstrapManifestId (identifierBytes seed))

processId :: Word8 -> ProcessId
processId seed = checked "ProcessId" (mkProcessId (identifierBytes seed))

deltaId :: Word8 -> DeltaId
deltaId seed = checked "DeltaId" (mkDeltaId (identifierBytes seed))

storeIncarnationId :: Word8 -> StoreIncarnationId
storeIncarnationId seed = checked "StoreIncarnationId" (mkStoreIncarnationId (identifierBytes seed))
