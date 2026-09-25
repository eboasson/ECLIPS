{-# LANGUAGE OverloadedStrings #-}

-- | Step-15-only checked genesis with a primordial process resident at H4.
--
-- The older Step-12/14 fixture deliberately has configured processes only at
-- H1 and H2.  Failure acceptance needs a real H4 application attachment and a
-- canonical resident-process End when H4 retires, so this fixture owns its
-- distinct configuration digest and Oracle genesis.
module Step15Genesis
  ( step15H1Genesis,
    step15H2Genesis,
    step15H3Genesis,
    step15H4Genesis,
    step15H1RoutedBootstraps,
    step15H2RoutedBootstraps,
    step15H3RoutedBootstraps,
    step15H4RoutedBootstraps,
    step15H1GeneratorSeedSource,
    step15H2GeneratorSeedSource,
    step15H3GeneratorSeedSource,
    step15H4GeneratorSeedSource,
    step15CheckedOracleGenesis,
    step15RoutedOracleTcpConfiguration,
    repeatedRetirementCheckedOracleGenesis,
    repeatedRetirementOracleTcpConfiguration,
    step15H1HeraldEpoch,
    step15H2HeraldEpoch,
    step15H3HeraldEpoch,
    step15H4HeraldEpoch,
    step15H1ProcessBootstrapId,
    step15H2ProcessBootstrapId,
    step15H4ProcessBootstrapId,
    step15H1ProcessEpochId,
    step15H4ProcessEpochId,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (sort)
import Data.Word (Word8)
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
    PredefinedSortRole (DeltaRole, NeutralVertexRole, SortDefinitionRole),
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
    checkInitialBootstrapsWithTopology,
    checkedConfigurationDigest,
    profilePredefinedCatalogueManifest,
  )
import Eclips.Herald.Runtime
  ( RuntimeGeneratorSeedSource,
    runtimeGeneratorSeedSource,
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
    oracleRuntimeConfiguration,
    runtimeElectionTimeoutSource,
  )
import Eclips.Oracle.Runtime.TCP
  ( OracleTcpClusterConfiguration,
    oracleTcpClusterConfiguration,
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
  )

step15H1Genesis, step15H2Genesis, step15H3Genesis, step15H4Genesis :: CheckedHeraldGenesis
step15H1Genesis = checkedGenesis "H1" h1Member
step15H2Genesis = checkedGenesis "H2" h2Member
step15H3Genesis = checkedGenesis "H3" h3Member
step15H4Genesis = checkedGenesis "H4" h4Member

checkedGenesis :: String -> HeraldMember -> CheckedHeraldGenesis
checkedGenesis label local =
  checked ("Step-15 " <> label <> " Herald genesis") (checkHeraldGenesis (heraldDeploymentAt local))

step15H1RoutedBootstraps, step15H2RoutedBootstraps, step15H3RoutedBootstraps, step15H4RoutedBootstraps :: CheckedInitialBootstraps
step15H1RoutedBootstraps = checkedBootstraps "H1" step15H1Genesis
step15H2RoutedBootstraps = checkedBootstraps "H2" step15H2Genesis
step15H3RoutedBootstraps = checkedBootstraps "H3" step15H3Genesis
step15H4RoutedBootstraps = checkedBootstraps "H4" step15H4Genesis

checkedBootstraps :: String -> CheckedHeraldGenesis -> CheckedInitialBootstraps
checkedBootstraps label genesis =
  checked
    ("Step-15 " <> label <> " initial bootstraps")
    ( checkInitialBootstrapsWithTopology
        genesis
        (PrimordialProcessManifest (fmap processBootstrapId processSpecs))
        routedInitialTopologyManifest
    )

step15H1GeneratorSeedSource, step15H2GeneratorSeedSource, step15H3GeneratorSeedSource, step15H4GeneratorSeedSource :: RuntimeGeneratorSeedSource
step15H1GeneratorSeedSource = fixedGeneratorSeedSource 0x11
step15H2GeneratorSeedSource = fixedGeneratorSeedSource 0x22
step15H3GeneratorSeedSource = fixedGeneratorSeedSource 0x33
step15H4GeneratorSeedSource = fixedGeneratorSeedSource 0x44

fixedGeneratorSeedSource :: Word8 -> RuntimeGeneratorSeedSource
fixedGeneratorSeedSource seed =
  runtimeGeneratorSeedSource (pure (ByteString.replicate 32 seed))

step15CheckedOracleGenesis :: CheckedOracleGenesis
step15CheckedOracleGenesis =
  checked "Step-15 Oracle genesis" (checkOracleGenesis step15OracleGenesis)

step15RoutedOracleTcpConfiguration :: OracleTcpClusterConfiguration
step15RoutedOracleTcpConfiguration =
  checked
    "Step-15 Oracle TCP cluster"
    (oracleTcpClusterConfiguration step15OracleRuntimeConfigurations)

step15OracleGenesis :: OracleGenesis
step15OracleGenesis = oracleGenesisFor raftBindings raftNativeConfiguration

oracleGenesisFor :: [RaftVoterBinding] -> RaftNativeConfiguration -> OracleGenesis
oracleGenesisFor bindings native =
  oracleGenesis
    systemId
    heraldMembers
    profileCatalogueDigest
    (fmap predefinedCatalogueDescriptor profilePredefinedCatalogue)
    appliedBootstraps
    (checkedConfigurationDigest step15H1Genesis)
    routedInitialTopology
    (deriveInitialProjectionDigest appliedBootstraps routedInitialTopology)
    bindings
    native
    (deriveRaftConfigurationDigest systemId bindings native)

-- | P05 uses the same four-Herald semantic genesis with a sole H1 voter.
-- H2 and H4 can therefore retire through the real failure protocol while the
-- Oracle voter configuration stays unchanged.
repeatedRetirementOracleTcpConfiguration :: OracleTcpClusterConfiguration
repeatedRetirementOracleTcpConfiguration =
  checked "P05 Oracle TCP cluster" (oracleTcpClusterConfiguration [runtime])
  where
    nativeGenesis = raftGenesis initialLeaderNode [initialLeaderNode] heartbeat electionLower electionUpper
    runtime = checked "P05 Oracle runtime" (oracleRuntimeConfiguration nativeGenesis repeatedRetirementOracleGenesis (heraldMemberEpoch h1Member) (runtimeElectionTimeoutSource (pure 0)))

repeatedRetirementCheckedOracleGenesis :: CheckedOracleGenesis
repeatedRetirementCheckedOracleGenesis =
  checked "P05 Oracle genesis" (checkOracleGenesis repeatedRetirementOracleGenesis)

repeatedRetirementOracleGenesis :: OracleGenesis
repeatedRetirementOracleGenesis = oracleGenesisFor bindings native
  where
    nativeGenesis = raftGenesis initialLeaderNode [initialLeaderNode] heartbeat electionLower electionUpper
    native = checkedRaftNativeConfiguration (checked "P05 Raft genesis" (checkRaftGenesis nativeGenesis))
    bindings = [raftVoterBinding initialLeaderNode (heraldMemberEpoch h1Member)]

step15OracleRuntimeConfigurations :: [OracleRuntimeConfiguration]
step15OracleRuntimeConfigurations =
  zipWith3 configured raftLocals cohosts [0, 30_000, 60_000]
  where
    raftLocals = [initialLeaderNode, raftNode 2, raftNode 3]
    cohosts = fmap heraldMemberEpoch [h1Member, h2Member, h3Member]
    configured local cohost sample =
      checked
        "Step-15 Oracle runtime configuration"
        ( oracleRuntimeConfiguration
            (raftGenesis local raftVoters heartbeat electionLower electionUpper)
            step15OracleGenesis
            cohost
            (runtimeElectionTimeoutSource (pure sample))
        )

raftNativeConfiguration :: RaftNativeConfiguration
raftNativeConfiguration =
  checkedRaftNativeConfiguration
    ( checked
        "Step-15 Raft genesis"
        ( checkRaftGenesis
            (raftGenesis initialLeaderNode raftVoters heartbeat electionLower electionUpper)
        )
    )

raftBindings :: [RaftVoterBinding]
raftBindings =
  [ raftVoterBinding initialLeaderNode (heraldMemberEpoch h1Member),
    raftVoterBinding (raftNode 2) (heraldMemberEpoch h2Member),
    raftVoterBinding (raftNode 3) (heraldMemberEpoch h3Member)
  ]

initialLeaderNode :: RaftNodeId
initialLeaderNode = raftNode 4

raftVoters :: [RaftNodeId]
raftVoters = sort [initialLeaderNode, raftNode 2, raftNode 3]

heartbeat, electionLower, electionUpper :: RaftDurationMicros
heartbeat = checked "Step-15 heartbeat" (mkRaftDurationMicros 20_000)
electionLower = checked "Step-15 election lower" (mkRaftDurationMicros 60_000)
electionUpper = checked "Step-15 election upper" (mkRaftDurationMicros 120_000)

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

routedInitialTopology :: CheckedInitialTopologyProjection
routedInitialTopology =
  checked
    "Step-15 routed initial topology"
    ( checkInitialTopologyProjection
        systemId
        heraldMembers
        appliedBootstraps
        routedInitialTopologyManifest
    )

-- The H4 NeutralVertex writer has a reader on every survivor. The terminal
-- repair tour can therefore isolate one accepted H2 copy, repair the missing
-- source history from that survivor, and prove that repair does not retarget
-- H4's lost destination-qualified publication into H1's application route.
routedInitialTopologyManifest :: InitialTopologyManifest
routedInitialTopologyManifest =
  InitialTopologyManifest
    [ InitialTopologyProcessEdge
        step15H1ProcessBootstrapId
        step15H2ProcessBootstrapId
        SortDefinitionRole,
      InitialTopologySystemViewEdge
        step15H1ProcessBootstrapId
        (heraldMemberEpoch h2Member)
        SortDefinitionRole,
      InitialTopologySystemViewEdge
        step15H1ProcessBootstrapId
        (heraldMemberEpoch h3Member)
        SortDefinitionRole,
      InitialTopologySystemViewEdge
        step15H1ProcessBootstrapId
        (heraldMemberEpoch h4Member)
        SortDefinitionRole,
      InitialTopologyProcessEdge
        step15H2ProcessBootstrapId
        step15H1ProcessBootstrapId
        DeltaRole,
      InitialTopologyProcessEdge
        step15H4ProcessBootstrapId
        step15H1ProcessBootstrapId
        NeutralVertexRole,
      InitialTopologyProcessEdge
        step15H4ProcessBootstrapId
        step15H2ProcessBootstrapId
        NeutralVertexRole,
      InitialTopologySystemViewEdge
        step15H4ProcessBootstrapId
        (heraldMemberEpoch h3Member)
        NeutralVertexRole
    ]

appliedBootstraps :: [AppliedProcessBootstrap]
appliedBootstraps =
  fmap
    ( checked "Step-15 configured genesis process materialization"
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
    ProcessSpec 11 21 31 h2Member 80 120,
    ProcessSpec 12 22 32 h4Member 140 160
  ]

step15H1ProcessBootstrapId, step15H2ProcessBootstrapId, step15H4ProcessBootstrapId :: BootstrapManifestId
step15H1ProcessBootstrapId = bootstrapId 10
step15H2ProcessBootstrapId = bootstrapId 11
step15H4ProcessBootstrapId = bootstrapId 12

step15H1ProcessEpochId, step15H4ProcessEpochId :: ProcessEpochId
step15H1ProcessEpochId = processEpoch 30
step15H4ProcessEpochId = processEpoch 32

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
    (error "complete Step-15 configured process was rejected")
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

step15H1HeraldEpoch, step15H2HeraldEpoch, step15H3HeraldEpoch, step15H4HeraldEpoch :: HeraldEpoch
step15H1HeraldEpoch = heraldMemberEpoch h1Member
step15H2HeraldEpoch = heraldMemberEpoch h2Member
step15H3HeraldEpoch = heraldMemberEpoch h3Member
step15H4HeraldEpoch = heraldMemberEpoch h4Member

systemId :: SystemId
systemId = checked "Step-15 SystemId" (mkSystemId (identifierBytes 1))

heraldId :: Word8 -> HeraldId
heraldId seed = checked "Step-15 HeraldId" (mkHeraldId (identifierBytes seed))

heraldEpoch :: Word8 -> HeraldEpoch
heraldEpoch seed = checked "Step-15 HeraldEpoch" (mkHeraldEpoch (identifierBytes seed))

raftNode :: Word8 -> RaftNodeId
raftNode seed = checked "Step-15 RaftNodeId" (mkRaftNodeId (identifierBytes seed))

bootstrapId :: Word8 -> BootstrapManifestId
bootstrapId seed = checked "Step-15 BootstrapManifestId" (mkBootstrapManifestId (identifierBytes seed))

processId :: Word8 -> ProcessId
processId seed = checked "Step-15 ProcessId" (mkProcessId (identifierBytes seed))

processEpoch :: Word8 -> ProcessEpochId
processEpoch seed = checked "Step-15 ProcessEpochId" (mkProcessEpochId (identifierBytes seed))

nablaId :: Word8 -> NablaId
nablaId seed = checked "Step-15 NablaId" (mkNablaId (identifierBytes seed))

deltaId :: Word8 -> DeltaId
deltaId seed = checked "Step-15 DeltaId" (mkDeltaId (identifierBytes seed))

storeIncarnation :: Word8 -> StoreIncarnationId
storeIncarnation seed = checked "Step-15 StoreIncarnationId" (mkStoreIncarnationId (identifierBytes seed))

identifierBytes :: Word8 -> ByteString
identifierBytes seed =
  ByteString.pack [seed + fromIntegral index | index <- [(0 :: Int) .. 31]]

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
