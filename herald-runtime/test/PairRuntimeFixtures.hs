module PairRuntimeFixtures
  ( pairLocalGenesis,
    pairRemoteGenesis,
    pairLocalBootstraps,
    pairRemoteBootstraps,
    pairBidirectionalLocalBootstraps,
    pairBidirectionalRemoteBootstraps,
    pairLocalBootstrapId,
    pairRemoteBootstrapId,
    pairLocalHeraldEpoch,
    pairRemoteHeraldEpoch,
    pairRemoteCandidate,
    pairRemoteHello,
    pairLocalGeneratorSeedSource,
    pairRemoteGeneratorSeedSource,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Set qualified as Set
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
import Eclips.Herald.Discovery
  ( ConnectionNonce,
    PeerCandidate,
    PeerHello,
    peerCandidate,
    peerHello,
  )
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
    checkedInitialProjectionDigest,
    profilePredefinedCatalogueManifest,
  )
import Eclips.Herald.Runtime
  ( RuntimeGeneratorSeedSource,
    runtimeGeneratorSeedSource,
  )

pairLocalGenesis :: CheckedHeraldGenesis
pairLocalGenesis = checked "local pair genesis" (checkHeraldGenesis (deploymentAt localMember))

pairRemoteGenesis :: CheckedHeraldGenesis
pairRemoteGenesis = checked "remote pair genesis" (checkHeraldGenesis (deploymentAt remoteMember))

pairLocalBootstraps :: CheckedInitialBootstraps
pairLocalBootstraps = checkedBootstraps pairLocalGenesis topology

pairRemoteBootstraps :: CheckedInitialBootstraps
pairRemoteBootstraps = checkedBootstraps pairRemoteGenesis topology

pairBidirectionalLocalBootstraps :: CheckedInitialBootstraps
pairBidirectionalLocalBootstraps = checkedBootstraps pairLocalGenesis bidirectionalTopology

pairBidirectionalRemoteBootstraps :: CheckedInitialBootstraps
pairBidirectionalRemoteBootstraps = checkedBootstraps pairRemoteGenesis bidirectionalTopology

pairLocalBootstrapId :: BootstrapManifestId
pairLocalBootstrapId = bootstrapManifestId 10

pairRemoteBootstrapId :: BootstrapManifestId
pairRemoteBootstrapId = bootstrapManifestId 12

pairLocalHeraldEpoch :: HeraldEpoch
pairLocalHeraldEpoch = heraldMemberEpoch localMember

pairRemoteHeraldEpoch :: HeraldEpoch
pairRemoteHeraldEpoch = heraldMemberEpoch remoteMember

pairRemoteCandidate :: ConnectionNonce -> PeerCandidate
pairRemoteCandidate nonce =
  peerCandidate
    (heraldMemberId remoteMember)
    (heraldMemberEpoch remoteMember)
    nonce

pairRemoteHello :: ConnectionNonce -> PeerHello
pairRemoteHello nonce =
  peerHello
    systemId
    (heraldMemberId remoteMember)
    (heraldMemberEpoch remoteMember)
    nonce
    Set.empty
    (controlIndex 0)
    profileCatalogueDigest
    (checkedInitialProjectionDigest pairLocalBootstraps)
    Nothing

pairLocalGeneratorSeedSource :: RuntimeGeneratorSeedSource
pairLocalGeneratorSeedSource = runtimeGeneratorSeedSource (pure (ByteString.replicate 32 0x11))

pairRemoteGeneratorSeedSource :: RuntimeGeneratorSeedSource
pairRemoteGeneratorSeedSource = runtimeGeneratorSeedSource (pure (ByteString.replicate 32 0x22))

checkedBootstraps :: CheckedHeraldGenesis -> InitialTopologyManifest -> CheckedInitialBootstraps
checkedBootstraps genesis initialTopology =
  checked
    "pair topology"
    ( checkInitialBootstrapsWithTopology
        genesis
        (PrimordialProcessManifest [pairLocalBootstrapId, pairRemoteBootstrapId])
        initialTopology
    )

topology :: InitialTopologyManifest
topology =
  InitialTopologyManifest
    [ InitialTopologyProcessEdge
        pairLocalBootstrapId
        pairRemoteBootstrapId
        SortDefinitionRole,
      InitialTopologySystemViewEdge
        pairLocalBootstrapId
        (heraldMemberEpoch remoteMember)
        SortDefinitionRole
    ]

bidirectionalTopology :: InitialTopologyManifest
bidirectionalTopology =
  InitialTopologyManifest
    [ InitialTopologyProcessEdge
        pairLocalBootstrapId
        pairRemoteBootstrapId
        SortDefinitionRole,
      InitialTopologyProcessEdge
        pairRemoteBootstrapId
        pairLocalBootstrapId
        SortDefinitionRole,
      InitialTopologySystemViewEdge
        pairLocalBootstrapId
        (heraldMemberEpoch remoteMember)
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
            primordialWriterSourceHeraldEpoch = heraldMemberEpoch localMember
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
members = [localMember, remoteMember]

localMember :: HeraldMember
localMember = HeraldMember (heraldId 2) (heraldEpoch 4)

remoteMember :: HeraldMember
remoteMember = HeraldMember (heraldId 3) (heraldEpoch 5)

configuredProcesses :: [ConfiguredProcessManifest]
configuredProcesses =
  [ configuredProcess 10 20 30 localMember 40 100,
    configuredProcess 11 21 31 localMember 60 110,
    configuredProcess 12 22 32 remoteMember 80 120
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
              configuredReaderStoreIncarnation =
                storeIncarnationId (incarnationBase + fromIntegral ordinal)
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
