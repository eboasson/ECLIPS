module ThreeHeraldFixtures
  ( threeH1Genesis,
    threeH2Genesis,
    threeH3Genesis,
    threeH1Bootstraps,
    threeH2Bootstraps,
    threeH3Bootstraps,
    threeH1HeraldEpoch,
    threeH2HeraldEpoch,
    threeH3HeraldEpoch,
    threeH1BootstrapId,
    threeH2BootstrapId,
    threeH1GeneratorSeedSource,
    threeH2GeneratorSeedSource,
    threeH3GeneratorSeedSource,
    threeOracleContacts,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.String (fromString)
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
import Eclips.Domain.Startup (profileCatalogueDigest)
import Eclips.Herald.Genesis
  ( CheckedHeraldGenesis,
    CheckedInitialBootstraps,
    ConfiguredProcessManifest (..),
    ConfiguredRootManifest (..),
    DeploymentManifest (..),
    HeraldMember (..),
    InitialTopologyEdgeManifest (..),
    InitialTopologyManifest (..),
    OracleGenesisManifest (..),
    PredefinedCatalogueManifest (..),
    PredefinedSortRole (SortDefinitionRole),
    PrimordialDefinitionManifest (..),
    PrimordialProcessManifest (..),
    PrimordialSortWriterAuthority (..),
    checkHeraldGenesis,
    checkInitialBootstrapsWithTopology,
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

-- This fixture deliberately adds H3 only to active membership. H1 and H2 keep
-- the same process/data topology used by the two-Herald gate, so H3 has no
-- publication destination in the learned-contact scenario.
threeH1Genesis :: CheckedHeraldGenesis
threeH1Genesis = checked "H1 genesis" (checkHeraldGenesis (deploymentAt h1Member))

threeH2Genesis :: CheckedHeraldGenesis
threeH2Genesis = checked "H2 genesis" (checkHeraldGenesis (deploymentAt h2Member))

threeH3Genesis :: CheckedHeraldGenesis
threeH3Genesis = checked "H3 genesis" (checkHeraldGenesis (deploymentAt h3Member))

threeH1Bootstraps :: CheckedInitialBootstraps
threeH1Bootstraps = checkedBootstraps threeH1Genesis

threeH2Bootstraps :: CheckedInitialBootstraps
threeH2Bootstraps = checkedBootstraps threeH2Genesis

threeH3Bootstraps :: CheckedInitialBootstraps
threeH3Bootstraps = checkedBootstraps threeH3Genesis

threeH1HeraldEpoch :: HeraldEpoch
threeH1HeraldEpoch = heraldMemberEpoch h1Member

threeH2HeraldEpoch :: HeraldEpoch
threeH2HeraldEpoch = heraldMemberEpoch h2Member

threeH3HeraldEpoch :: HeraldEpoch
threeH3HeraldEpoch = heraldMemberEpoch h3Member

threeH1GeneratorSeedSource :: RuntimeGeneratorSeedSource
threeH1GeneratorSeedSource = runtimeGeneratorSeedSource (pure (ByteString.replicate 32 0x11))

threeH2GeneratorSeedSource :: RuntimeGeneratorSeedSource
threeH2GeneratorSeedSource = runtimeGeneratorSeedSource (pure (ByteString.replicate 32 0x22))

threeH3GeneratorSeedSource :: RuntimeGeneratorSeedSource
threeH3GeneratorSeedSource = runtimeGeneratorSeedSource (pure (ByteString.replicate 32 0x33))

threeOracleContacts :: OracleContactSet
threeOracleContacts =
  checked
    "three Oracle contacts"
    ( oracleContactSet
        ( oracle 200 61_011
            :| [oracle 201 61_012, oracle 202 61_013]
        )
    )
  where
    oracle seed port =
      checked
        "Oracle contact"
        ( oracleContact
            (checked "Oracle node claim" (oracleNodeClaim (identifierBytes seed)))
            (fromString "127.0.0.1")
            port
        )

checkedBootstraps :: CheckedHeraldGenesis -> CheckedInitialBootstraps
checkedBootstraps genesis =
  checked
    "three-Herald topology"
    ( checkInitialBootstrapsWithTopology
        genesis
        (PrimordialProcessManifest [threeH1BootstrapId, threeH2BootstrapId])
        initialTopology
    )

initialTopology :: InitialTopologyManifest
initialTopology =
  InitialTopologyManifest
    [ InitialTopologyProcessEdge
        threeH1BootstrapId
        threeH2BootstrapId
        SortDefinitionRole,
      InitialTopologySystemViewEdge
        threeH1BootstrapId
        threeH2HeraldEpoch
        SortDefinitionRole
    ]

deploymentAt :: HeraldMember -> DeploymentManifest
deploymentAt local =
  DeploymentManifest
    { deploymentSystemId = systemId,
      deploymentLocalHeraldId = heraldMemberId local,
      deploymentLocalHeraldEpoch = heraldMemberEpoch local,
      deploymentActiveHeralds = members,
      deploymentPredefinedCatalogue = profilePredefinedCatalogueManifest,
      deploymentPrimordialWriterAuthority =
        PrimordialSortWriterAuthority
          { primordialWriterNabla = nablaId 1,
            primordialWriterAuthorityEpoch = genesisAuthorityEpoch,
            primordialWriterSourceHeraldEpoch = threeH1HeraldEpoch
          },
      deploymentPrimordialDefinitions =
        [ PrimordialDefinitionManifest
            { primordialManifestRole = catalogueManifestRole entry,
              primordialManifestDescriptorBytes = catalogueManifestDescriptorBytes entry,
              primordialManifestPublicationSequence = nablaSequence (fromIntegral ordinal + 1)
            }
        | (ordinal, entry) <- zip [(0 :: Int) ..] profilePredefinedCatalogueManifest
        ],
      deploymentConfiguredProcesses = configuredProcesses,
      deploymentOracleGenesis =
        OracleGenesisManifest
          { oracleGenesisSystemId = systemId,
            oracleGenesisActiveHeralds = members,
            oracleGenesisCatalogueDigest = profileCatalogueDigest,
            oracleGenesisControlIndex = controlIndex 0
          }
    }

members :: [HeraldMember]
members = [h1Member, h2Member, h3Member]

h1Member :: HeraldMember
h1Member = HeraldMember (heraldId 2) (heraldEpoch 4)

h2Member :: HeraldMember
h2Member = HeraldMember (heraldId 3) (heraldEpoch 5)

h3Member :: HeraldMember
h3Member = HeraldMember (heraldId 4) (heraldEpoch 6)

threeH1BootstrapId :: BootstrapManifestId
threeH1BootstrapId = bootstrapManifestId 10

threeH2BootstrapId :: BootstrapManifestId
threeH2BootstrapId = bootstrapManifestId 12

configuredProcesses :: [ConfiguredProcessManifest]
configuredProcesses =
  [ configuredProcess 10 20 30 h1Member 40 100,
    configuredProcess 11 21 31 h1Member 60 110,
    configuredProcess 12 22 32 h2Member 80 120
  ]

configuredProcess ::
  Word8 ->
  Word8 ->
  Word8 ->
  HeraldMember ->
  Word8 ->
  Word8 ->
  ConfiguredProcessManifest
configuredProcess manifestByte processByte epochByte residence rootBase incarnationBase =
  ConfiguredProcessManifest
    { configuredBootstrapManifestId = bootstrapManifestId manifestByte,
      configuredProcessId = processId processByte,
      configuredProcessEpochId = processEpochId epochByte,
      configuredProcessResidence = heraldMemberEpoch residence,
      configuredProcessRoots =
        [ ConfiguredRootManifest
            { configuredRootRole = role,
              configuredWriterNabla = nablaId (rootBase + fromIntegral (2 * ordinal)),
              configuredWriterSequencing = UnsequencedNabla,
              configuredReaderDelta = deltaId (rootBase + fromIntegral (2 * ordinal + 1)),
              configuredReaderStoreIncarnation = storeIncarnationId (incarnationBase + fromIntegral ordinal)
            }
        | (ordinal, role) <- zip [(0 :: Int) ..] [minBound .. maxBound]
        ]
    }

identifierBytes :: Word8 -> ByteString
identifierBytes seed =
  ByteString.pack
    [ seed + fromIntegral (index * 37 + index * index)
    | index <- [(0 :: Int) .. 31]
    ]

systemId :: SystemId
systemId = checked "SystemId" (mkSystemId (identifierBytes 0))

heraldId :: Word8 -> HeraldId
heraldId seed = checked "HeraldId" (mkHeraldId (identifierBytes seed))

heraldEpoch :: Word8 -> HeraldEpoch
heraldEpoch seed = checked "HeraldEpoch" (mkHeraldEpoch (identifierBytes seed))

bootstrapManifestId :: Word8 -> BootstrapManifestId
bootstrapManifestId seed =
  checked "BootstrapManifestId" (mkBootstrapManifestId (identifierBytes seed))

processId :: Word8 -> ProcessId
processId seed = checked "ProcessId" (mkProcessId (identifierBytes seed))

processEpochId :: Word8 -> ProcessEpochId
processEpochId seed = checked "ProcessEpochId" (mkProcessEpochId (identifierBytes seed))

nablaId :: Word8 -> NablaId
nablaId seed = checked "NablaId" (mkNablaId (identifierBytes seed))

deltaId :: Word8 -> DeltaId
deltaId seed = checked "DeltaId" (mkDeltaId (identifierBytes seed))

storeIncarnationId :: Word8 -> StoreIncarnationId
storeIncarnationId seed =
  checked "StoreIncarnationId" (mkStoreIncarnationId (identifierBytes seed))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
