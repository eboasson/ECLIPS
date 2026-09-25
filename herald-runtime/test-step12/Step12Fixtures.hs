{-# LANGUAGE OverloadedStrings #-}

module Step12Fixtures
  ( step12H1Genesis,
    step12H2Genesis,
    step12H3Genesis,
    step12H4Genesis,
    step12H1Bootstraps,
    step12H2Bootstraps,
    step12H3Bootstraps,
    step12H4Bootstraps,
    step12H1RoutedBootstraps,
    step12H2RoutedBootstraps,
    step12H3RoutedBootstraps,
    step12H4RoutedBootstraps,
    step12H1GeneratorSeedSource,
    step12H2GeneratorSeedSource,
    step12H3GeneratorSeedSource,
    step12H4GeneratorSeedSource,
    step12CheckedOracleGenesis,
    step12CheckedRoutedOracleGenesis,
    step12OracleTcpConfiguration,
    step12RecordedOracleTcpConfiguration,
    step12RoutedOracleTcpConfiguration,
    step12RoutedOracleRuntimeConfigurations,
    step12InitialLeaderNode,
    step12ReplacementLeaderNode,
    step12FirstContactFollowerNode,
    step12SubmissionFollowerNode,
    step12SubmittingHeraldId,
    step12SubmittingHeraldEpoch,
    step12H1HeraldEpoch,
    step12H2HeraldEpoch,
    step12H3HeraldEpoch,
    step12H4HeraldEpoch,
    step12SystemId,
    step12H1ProcessBootstrapId,
    step12H2ProcessBootstrapId,
    step12H1ProcessEpochId,
    step12H2ProcessEpochId,
    step12StartedProcess,
    step12ValidOracleCommand,
    step12AlternateOracleCommand,
    step12OracleContacts,
    awaitStep12ReadyLeader,
    awaitStep12AppliedPrefix,
    withReservedUnavailableOracleContacts,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.String (fromString)
import Data.Word (Word16, Word64, Word8)
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
import Eclips.Domain.ProcessStart (ProcessStart, processStart)
import Eclips.Domain.Sort.Profile
  ( predefinedCatalogueDescriptor,
    profileCatalogueDigest,
    profilePredefinedCatalogue,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    CheckedInitialTopologyProjection,
    ConfiguredProcessBootstrap,
    HeraldMember (..),
    InitialTopologyEdgeManifest (..),
    InitialTopologyManifest (..),
    PredefinedSortRole (DeltaRole, SortDefinitionRole),
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
    OracleGenesisManifest (..),
    PredefinedCatalogueManifest (..),
    PrimordialDefinitionManifest (..),
    PrimordialProcessManifest (..),
    PrimordialSortWriterAuthority (..),
    checkHeraldGenesis,
    checkInitialBootstraps,
    checkInitialBootstrapsWithTopology,
    checkedConfigurationDigest,
    profilePredefinedCatalogueManifest,
  )
import Eclips.Herald.OracleClient
  ( OracleContactSet,
    oracleContact,
    oracleContactSet,
    oracleNodeClaim,
  )
import Eclips.Herald.Runtime
  ( RuntimeGeneratorSeedSource,
    runtimeGeneratorSeedSource,
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
    oracleTcpEndpointHost,
    oracleTcpEndpointPort,
    oracleTcpNodeStatuses,
    oracleTcpOracleContacts,
  )
import Eclips.Raft.Genesis
  ( RaftNativeConfiguration,
    checkRaftGenesis,
    checkedRaftNativeConfiguration,
    raftGenesis,
  )
import Eclips.Raft.Identity
  ( RaftDurationMicros,
    RaftNodeId,
    mkRaftDurationMicros,
    mkRaftNodeId,
    raftNodeIdBytes,
  )
import Network.Socket
  ( Family (AF_INET),
    SockAddr (SockAddrInet),
    Socket,
    SocketType (Stream),
    bind,
    close,
    defaultProtocol,
    getSocketName,
    socket,
    tupleToHostAddress,
  )
import System.Timeout (timeout)

step12H1Genesis, step12H2Genesis, step12H3Genesis, step12H4Genesis :: CheckedHeraldGenesis
step12H1Genesis = checkedGenesis "H1" h1Member
step12H2Genesis = checkedGenesis "H2" h2Member
step12H3Genesis = checkedGenesis "H3" h3Member
step12H4Genesis = checkedGenesis "H4" h4Member

checkedGenesis :: String -> HeraldMember -> CheckedHeraldGenesis
checkedGenesis label local =
  checked (label <> " Herald genesis") (checkHeraldGenesis (heraldDeploymentAt local))

step12H1Bootstraps, step12H2Bootstraps, step12H3Bootstraps, step12H4Bootstraps :: CheckedInitialBootstraps
step12H1Bootstraps = checkedBootstraps "H1" step12H1Genesis
step12H2Bootstraps = checkedBootstraps "H2" step12H2Genesis
step12H3Bootstraps = checkedBootstraps "H3" step12H3Genesis
step12H4Bootstraps = checkedBootstraps "H4" step12H4Genesis

checkedBootstraps :: String -> CheckedHeraldGenesis -> CheckedInitialBootstraps
checkedBootstraps label genesis =
  checked
    (label <> " initial bootstraps")
    ( checkInitialBootstraps
        genesis
        (PrimordialProcessManifest (fmap processBootstrapId processSpecs))
    )

-- | Four-member data-space variants for Step 14. The historical Step-12
-- control fixture deliberately retains an empty topology; physical peer
-- connectivity alone is not semantic publication reachability.
step12H1RoutedBootstraps, step12H2RoutedBootstraps, step12H3RoutedBootstraps, step12H4RoutedBootstraps :: CheckedInitialBootstraps
step12H1RoutedBootstraps = checkedRoutedBootstraps "H1" step12H1Genesis
step12H2RoutedBootstraps = checkedRoutedBootstraps "H2" step12H2Genesis
step12H3RoutedBootstraps = checkedRoutedBootstraps "H3" step12H3Genesis
step12H4RoutedBootstraps = checkedRoutedBootstraps "H4" step12H4Genesis

checkedRoutedBootstraps :: String -> CheckedHeraldGenesis -> CheckedInitialBootstraps
checkedRoutedBootstraps label genesis =
  checked
    (label <> " routed initial bootstraps")
    ( checkInitialBootstrapsWithTopology
        genesis
        (PrimordialProcessManifest (fmap processBootstrapId processSpecs))
        routedInitialTopologyManifest
    )

step12H1GeneratorSeedSource, step12H2GeneratorSeedSource, step12H3GeneratorSeedSource, step12H4GeneratorSeedSource :: RuntimeGeneratorSeedSource
step12H1GeneratorSeedSource = fixedGeneratorSeedSource 0x11
step12H2GeneratorSeedSource = fixedGeneratorSeedSource 0x22
step12H3GeneratorSeedSource = fixedGeneratorSeedSource 0x33
step12H4GeneratorSeedSource = fixedGeneratorSeedSource 0x44

fixedGeneratorSeedSource :: Word8 -> RuntimeGeneratorSeedSource
fixedGeneratorSeedSource seed =
  runtimeGeneratorSeedSource (pure (ByteString.replicate 32 seed))

step12CheckedOracleGenesis :: CheckedOracleGenesis
step12CheckedOracleGenesis =
  checked "Step-13 Increment-1 Oracle genesis" (checkOracleGenesis step12OracleGenesis)

step12CheckedRoutedOracleGenesis :: CheckedOracleGenesis
step12CheckedRoutedOracleGenesis =
  checked "Step-14 routed Oracle genesis" (checkOracleGenesis step12RoutedOracleGenesis)

step12OracleTcpConfiguration :: OracleTcpClusterConfiguration
step12OracleTcpConfiguration =
  checked
    "Step-13 Increment-1 Oracle TCP cluster"
    (oracleTcpClusterConfiguration step12OracleRuntimeConfigurations)

step12RecordedOracleTcpConfiguration :: OracleTcpClusterConfiguration
step12RecordedOracleTcpConfiguration =
  checked "Step-12 recorded Oracle TCP cluster" (oracleTcpClusterConfiguration (map (configureOracleRuntimeRecording OracleCaptureHistory) step12OracleRuntimeConfigurations))

step12RoutedOracleTcpConfiguration :: OracleTcpClusterConfiguration
step12RoutedOracleTcpConfiguration =
  checked
    "Step-14 routed Oracle TCP cluster"
    (oracleTcpClusterConfiguration step12RoutedOracleRuntimeConfigurations)

step12RoutedOracleRuntimeConfigurations :: [OracleRuntimeConfiguration]
step12RoutedOracleRuntimeConfigurations =
  oracleRuntimeConfigurationsFor step12RoutedOracleGenesis

step12InitialLeaderNode :: RaftNodeId
step12InitialLeaderNode = raftNode 4

step12ReplacementLeaderNode :: RaftNodeId
step12ReplacementLeaderNode = raftNode 2

-- Node claims sort lexicographically in the Herald contact set. Giving R1 the
-- greatest claim while its injected election sample is earliest makes H4's
-- first checked contact a real follower and the redirect deterministic.
step12FirstContactFollowerNode :: RaftNodeId
step12FirstContactFollowerNode = raftNode 2

step12SubmissionFollowerNode :: RaftNodeId
step12SubmissionFollowerNode = raftNode 3

step12SubmittingHeraldId :: HeraldId
step12SubmittingHeraldId = heraldMemberId h2Member

step12SubmittingHeraldEpoch :: HeraldEpoch
step12SubmittingHeraldEpoch = heraldMemberEpoch h2Member

step12H1HeraldEpoch, step12H2HeraldEpoch, step12H3HeraldEpoch, step12H4HeraldEpoch :: HeraldEpoch
step12H1HeraldEpoch = heraldMemberEpoch h1Member
step12H2HeraldEpoch = heraldMemberEpoch h2Member
step12H3HeraldEpoch = heraldMemberEpoch h3Member
step12H4HeraldEpoch = heraldMemberEpoch h4Member

step12SystemId :: SystemId
step12SystemId = systemId

step12H1ProcessBootstrapId, step12H2ProcessBootstrapId :: BootstrapManifestId
step12H1ProcessBootstrapId = bootstrapId 10
step12H2ProcessBootstrapId = bootstrapId 11

step12H1ProcessEpochId, step12H2ProcessEpochId :: ProcessEpochId
step12H1ProcessEpochId = processEpoch 30
step12H2ProcessEpochId = processEpoch 31

step12StartedProcess :: ProcessStart
step12StartedProcess =
  dynamicProcess startedProcessSpec

step12ValidOracleCommand :: OracleCommand
step12ValidOracleCommand =
  startProcessEpochCommand step12StartedProcess

step12AlternateOracleCommand :: OracleCommand
step12AlternateOracleCommand =
  startProcessEpochCommand (dynamicProcess alternateProcessSpec)

step12OracleContacts :: OracleTcpCluster -> OracleContactSet
step12OracleContacts cluster =
  checked
    "dynamic Step-13 Increment-1 Oracle contacts"
    (oracleContactSet (NonEmpty.fromList contacts))
  where
    contacts =
      [ checked
          "dynamic Step-13 Increment-1 Oracle contact"
          ( oracleContact
              (checked "dynamic Oracle node claim" (oracleNodeClaim (raftNodeIdBytes node)))
              (fromString (oracleTcpEndpointHost endpoint))
              (oracleTcpEndpointPort endpoint)
          )
      | (node, endpoint) <- oracleTcpOracleContacts cluster
      ]

awaitStep12ReadyLeader :: OracleTcpCluster -> IO RaftNodeId
awaitStep12ReadyLeader cluster = do
  observed <- timeout 5_000_000 loop
  maybe (error "timed out waiting for the Step-13 Increment-1 Oracle leader") pure observed
  where
    loop = do
      statuses <- oracleTcpNodeStatuses cluster
      case [oracleRuntimeNode status | (_, status) <- statuses, oracleRuntimeServiceReady status] of
        [node] -> pure node
        _ -> threadDelay 10_000 >> loop

awaitStep12AppliedPrefix :: OracleTcpCluster -> Word64 -> IO ()
awaitStep12AppliedPrefix cluster expected = do
  observed <- timeout 5_000_000 loop
  maybe (error "timed out waiting for the Step-13 Increment-1 Oracle prefix") pure observed
  where
    loop = do
      statuses <- fmap (fmap snd) (oracleTcpNodeStatuses cluster)
      if any ((== controlIndex expected) . oracleRuntimeAppliedControlIndex) statuses
        then pure ()
        else threadDelay 10_000 >> loop

withReservedUnavailableOracleContacts ::
  (OracleContactSet -> IO () -> IO result) ->
  IO result
withReservedUnavailableOracleContacts use =
  bracket reserve close $ \first ->
    bracket reserve close $ \second ->
      bracket reserve close $ \third -> do
        ports <- traverse reservedPort [first, second, third]
        let contacts =
              zipWith
                ( \seed port ->
                    checked
                      "reserved unavailable Oracle contact"
                      ( oracleContact
                          (checked "reserved Oracle node claim" (oracleNodeClaim (identifierBytes seed)))
                          "127.0.0.1"
                          port
                      )
                )
                [210, 211, 212]
                ports
        use
          (checked "reserved Oracle contact set" (oracleContactSet (NonEmpty.fromList contacts)))
          (close first)
  where
    reserve = do
      reserved <- socket AF_INET Stream defaultProtocol
      bind reserved (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
      pure reserved

reservedPort :: Socket -> IO Word16
reservedPort reserved = do
  address <- getSocketName reserved
  case address of
    SockAddrInet port _ -> pure (fromIntegral port)
    other -> error ("reserved loopback socket returned an unexpected address: " <> show other)

heraldDeploymentAt :: HeraldMember -> DeploymentManifest
heraldDeploymentAt local =
  DeploymentManifest
    { deploymentSystemId = systemId,
      deploymentLocalHeraldId = heraldMemberId local,
      deploymentLocalHeraldEpoch = heraldMemberEpoch local,
      deploymentActiveHeralds = heraldMembers,
      deploymentPredefinedCatalogue = profilePredefinedCatalogueManifest,
      deploymentPrimordialWriterAuthority =
        PrimordialSortWriterAuthority
          { primordialWriterNabla = nablaId 1,
            primordialWriterAuthorityEpoch = genesisAuthorityEpoch,
            primordialWriterSourceHeraldEpoch = heraldMemberEpoch h1Member
          },
      deploymentPrimordialDefinitions =
        [ PrimordialDefinitionManifest
            { primordialManifestRole = catalogueManifestRole entry,
              primordialManifestDescriptorBytes = catalogueManifestDescriptorBytes entry,
              primordialManifestPublicationSequence = nablaSequence (fromIntegral ordinal + 1)
            }
        | (ordinal, entry) <- zip [(0 :: Int) ..] profilePredefinedCatalogueManifest
        ],
      deploymentConfiguredProcesses = fmap heraldProcess processSpecs,
      deploymentOracleGenesis =
        OracleGenesisManifest
          { oracleGenesisSystemId = systemId,
            oracleGenesisActiveHeralds = heraldMembers,
            oracleGenesisCatalogueDigest = profileCatalogueDigest,
            oracleGenesisControlIndex = controlIndex 0
          }
    }

step12OracleGenesis :: OracleGenesis
step12OracleGenesis =
  oracleGenesisFor initialTopology

step12RoutedOracleGenesis :: OracleGenesis
step12RoutedOracleGenesis =
  oracleGenesisFor routedInitialTopology

oracleGenesisFor :: CheckedInitialTopologyProjection -> OracleGenesis
oracleGenesisFor topology =
  oracleGenesis
    systemId
    heraldMembers
    profileCatalogueDigest
    (fmap predefinedCatalogueDescriptor profilePredefinedCatalogue)
    appliedBootstraps
    (checkedConfigurationDigest step12H1Genesis)
    topology
    (deriveInitialProjectionDigest appliedBootstraps topology)
    raftBindings
    raftNativeConfiguration
    (deriveRaftConfigurationDigest systemId raftBindings raftNativeConfiguration)

step12OracleRuntimeConfigurations :: [OracleRuntimeConfiguration]
step12OracleRuntimeConfigurations =
  oracleRuntimeConfigurationsFor step12OracleGenesis

oracleRuntimeConfigurationsFor :: OracleGenesis -> [OracleRuntimeConfiguration]
oracleRuntimeConfigurationsFor genesis =
  zipWith3 configured raftLocals cohosts [0, 30_000, 60_000]
  where
    raftLocals = [step12InitialLeaderNode, raftNode 2, raftNode 3]
    cohosts = fmap heraldMemberEpoch [h1Member, h2Member, h3Member]
    configured local cohost sample =
      checked
        "Step-13 Increment-1 Oracle runtime configuration"
        ( oracleRuntimeConfiguration
            (raftGenesis local raftVoters heartbeat electionLower electionUpper)
            genesis
            cohost
            (runtimeElectionTimeoutSource (pure sample))
        )

raftNativeConfiguration :: RaftNativeConfiguration
raftNativeConfiguration =
  checkedRaftNativeConfiguration
    ( checked
        "Step-13 Increment-1 Raft genesis"
        ( checkRaftGenesis
            ( raftGenesis
                step12InitialLeaderNode
                raftVoters
                heartbeat
                electionLower
                electionUpper
            )
        )
    )

raftBindings :: [RaftVoterBinding]
raftBindings =
  [ raftVoterBinding step12InitialLeaderNode (heraldMemberEpoch h1Member),
    raftVoterBinding (raftNode 2) (heraldMemberEpoch h2Member),
    raftVoterBinding (raftNode 3) (heraldMemberEpoch h3Member)
  ]

raftVoters :: [RaftNodeId]
raftVoters = sort [step12InitialLeaderNode, raftNode 2, raftNode 3]

heartbeat :: RaftDurationMicros
heartbeat = checked "heartbeat" (mkRaftDurationMicros 20_000)

electionLower :: RaftDurationMicros
electionLower = checked "election lower" (mkRaftDurationMicros 60_000)

electionUpper :: RaftDurationMicros
electionUpper = checked "election upper" (mkRaftDurationMicros 120_000)

initialTopology :: CheckedInitialTopologyProjection
initialTopology =
  checked
    "Step-13 Increment-1 initial topology"
    ( checkInitialTopologyProjection
        systemId
        heraldMembers
        appliedBootstraps
        (InitialTopologyManifest [])
    )

routedInitialTopology :: CheckedInitialTopologyProjection
routedInitialTopology =
  checked
    "Step-14 routed initial topology"
    ( checkInitialTopologyProjection
        systemId
        heraldMembers
        appliedBootstraps
        routedInitialTopologyManifest
    )

routedInitialTopologyManifest :: InitialTopologyManifest
routedInitialTopologyManifest =
  InitialTopologyManifest
    [ InitialTopologyProcessEdge
        step12H1ProcessBootstrapId
        step12H2ProcessBootstrapId
        SortDefinitionRole,
      InitialTopologySystemViewEdge
        step12H1ProcessBootstrapId
        (heraldMemberEpoch h2Member)
        SortDefinitionRole,
      InitialTopologySystemViewEdge
        step12H1ProcessBootstrapId
        (heraldMemberEpoch h3Member)
        SortDefinitionRole,
      InitialTopologySystemViewEdge
        step12H1ProcessBootstrapId
        (heraldMemberEpoch h4Member)
        SortDefinitionRole,
      InitialTopologyProcessEdge
        step12H2ProcessBootstrapId
        step12H1ProcessBootstrapId
        DeltaRole
    ]

appliedBootstraps :: [AppliedProcessBootstrap]
appliedBootstraps =
  fmap
    ( checked "configured genesis process materialization"
        . materializeConfiguredProcessBootstrap
          (controlIndex 0)
          (genesisPredefinedOccurrenceSet systemId)
        . domainProcess
    )
    processSpecs

data ProcessSpec = ProcessSpec Word8 Word8 Word8 HeraldMember Word8 Word8

processSpecs :: [ProcessSpec]
processSpecs =
  [ ProcessSpec 10 20 30 h1Member 40 100,
    ProcessSpec 11 21 31 h2Member 80 120
  ]

startedProcessSpec :: ProcessSpec
startedProcessSpec = ProcessSpec 150 151 152 h2Member 160 180

alternateProcessSpec :: ProcessSpec
alternateProcessSpec = ProcessSpec 200 201 202 h2Member 210 230

processBootstrapId :: ProcessSpec -> BootstrapManifestId
processBootstrapId (ProcessSpec manifestSeed _ _ _ _ _) = bootstrapId manifestSeed

heraldProcess :: ProcessSpec -> ConfiguredProcessManifest
heraldProcess (ProcessSpec manifestSeed processSeed epochSeed residence rootBase incarnationBase) =
  ConfiguredProcessManifest
    { configuredBootstrapManifestId = bootstrapId manifestSeed,
      configuredProcessId = processId processSeed,
      configuredProcessEpochId = processEpoch epochSeed,
      configuredProcessResidence = heraldMemberEpoch residence,
      configuredProcessRoots =
        [ ConfiguredRootManifest
            { configuredRootRole = role,
              configuredWriterNabla = nablaId (rootBase + fromIntegral (2 * ordinal)),
              configuredWriterSequencing = UnsequencedNabla,
              configuredReaderDelta = deltaId (rootBase + fromIntegral (2 * ordinal + 1)),
              configuredReaderStoreIncarnation = storeIncarnation (incarnationBase + fromIntegral ordinal)
            }
        | (ordinal, role) <- zip [(0 :: Int) ..] [minBound .. maxBound]
        ]
    }

domainProcess :: ProcessSpec -> ConfiguredProcessBootstrap
domainProcess (ProcessSpec manifestSeed processSeed epochSeed residence rootBase incarnationBase) =
  maybe
    (error "complete Step-13 Increment-1 configured process was rejected")
    id
    ( mkConfiguredProcessBootstrap
        (bootstrapId manifestSeed)
        (processId processSeed)
        (processEpoch epochSeed)
        (heraldMemberEpoch residence)
        [ configuredRootBootstrap
            role
            (nablaId (rootBase + fromIntegral (2 * ordinal)))
            UnsequencedNabla
            (deltaId (rootBase + fromIntegral (2 * ordinal + 1)))
            (storeIncarnation (incarnationBase + fromIntegral ordinal))
        | (ordinal, role) <- zip [(0 :: Int) ..] [minBound .. maxBound]
        ]
    )

heraldMembers :: [HeraldMember]
heraldMembers = [h1Member, h2Member, h3Member, h4Member]

h1Member, h2Member, h3Member, h4Member :: HeraldMember
h1Member = HeraldMember (heraldId 10) (heraldEpoch 11)
h2Member = HeraldMember (heraldId 12) (heraldEpoch 13)
h3Member = HeraldMember (heraldId 14) (heraldEpoch 15)
h4Member = HeraldMember (heraldId 16) (heraldEpoch 17)

systemId :: SystemId
systemId = checked "SystemId" (mkSystemId (identifierBytes 1))

heraldId :: Word8 -> HeraldId
heraldId seed = checked "HeraldId" (mkHeraldId (identifierBytes seed))

heraldEpoch :: Word8 -> HeraldEpoch
heraldEpoch seed = checked "HeraldEpoch" (mkHeraldEpoch (identifierBytes seed))

raftNode :: Word8 -> RaftNodeId
raftNode seed = checked "RaftNodeId" (mkRaftNodeId (identifierBytes seed))

bootstrapId :: Word8 -> BootstrapManifestId
bootstrapId seed = checked "BootstrapManifestId" (mkBootstrapManifestId (identifierBytes seed))

processId :: Word8 -> ProcessId
processId seed = checked "ProcessId" (mkProcessId (identifierBytes seed))

processEpoch :: Word8 -> ProcessEpochId
processEpoch seed = checked "ProcessEpochId" (mkProcessEpochId (identifierBytes seed))

nablaId :: Word8 -> NablaId
nablaId seed = checked "NablaId" (mkNablaId (identifierBytes seed))

deltaId :: Word8 -> DeltaId
deltaId seed = checked "DeltaId" (mkDeltaId (identifierBytes seed))

storeIncarnation :: Word8 -> StoreIncarnationId
storeIncarnation seed = checked "StoreIncarnationId" (mkStoreIncarnationId (identifierBytes seed))

identifierBytes :: Word8 -> ByteString
identifierBytes seed =
  ByteString.pack [seed + fromIntegral index | index <- [(0 :: Int) .. 31]]

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

dynamicProcess :: ProcessSpec -> ProcessStart
dynamicProcess (ProcessSpec _ processSeed epochSeed residence _ _) =
  processStart (processId processSeed) (processEpoch epochSeed) (heraldMemberEpoch residence)
