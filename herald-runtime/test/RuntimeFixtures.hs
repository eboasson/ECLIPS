module RuntimeFixtures
  ( fixtureCheckedGenesis,
    fixtureCheckedBootstraps,
    fixtureGeneratorSeed,
    fixtureAlternateGeneratorSeed,
    fixtureGeneratorSeedBytes,
    fixtureGeneratorSeedSource,
    fixtureApplicationRecoveryConfiguration,
    fixtureAlternateApplicationRecoveryConfiguration,
    fixturePeerRecoveryConfiguration,
    fixtureAlternatePeerRecoveryConfiguration,
    fixtureOracleContacts,
    fixtureAlternateOracleContacts,
    fixtureSystemId,
    fixtureBootstrapManifestId,
    fixtureIdentifierBytes,
    fixtureOpenDto,
    fixturePeerCandidate,
    fixturePeerHello,
    fixtureMembershipGenerationId,
    fixtureMemberSetDigest,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Word (Word16, Word8)
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
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Startup (profileCatalogueDigest)
import Eclips.Domain.Topology (MemberSetDigest)
import Eclips.Herald.Discovery
  ( ConnectionNonce,
    PeerCandidate,
    PeerHello,
    connectionNonce,
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
    OracleGenesisManifest (..),
    PredefinedCatalogueManifest (..),
    PredefinedSortRole,
    PrimordialDefinitionManifest (..),
    PrimordialProcessManifest (..),
    PrimordialSortWriterAuthority (..),
    checkHeraldGenesis,
    checkInitialBootstraps,
    checkedInitialProjectionDigest,
    profilePredefinedCatalogueManifest,
  )
import Eclips.Herald.IdGenerator
  ( GeneratorSeed,
    mkGeneratorSeed,
  )
import Eclips.Herald.OracleClient
  ( OracleContactSet,
    oracleContact,
    oracleContactSet,
    oracleNodeClaim,
  )
import Eclips.Herald.Runtime
  ( ApplicationRecoveryConfiguration,
    PeerRecoveryConfiguration,
    RuntimeGeneratorSeedSource,
    checkApplicationRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
    runtimeGeneratorSeedSource,
  )
import Eclips.Protocol.Application.Types
  ( ApplicationClientDto (..),
    applicationAttachmentClaim,
    applicationClientNonce,
  )

fixtureCheckedGenesis :: CheckedHeraldGenesis
fixtureCheckedGenesis = checked "deployment genesis" (checkHeraldGenesis fixtureDeployment)

fixtureCheckedBootstraps :: CheckedInitialBootstraps
fixtureCheckedBootstraps =
  checked
    "initial bootstraps"
    (checkInitialBootstraps fixtureCheckedGenesis (PrimordialProcessManifest fixtureLocalBootstrapIds))

fixtureGeneratorSeed :: GeneratorSeed
fixtureGeneratorSeed = checked "generator seed" (mkGeneratorSeed fixtureGeneratorSeedBytes)

fixtureAlternateGeneratorSeed :: GeneratorSeed
fixtureAlternateGeneratorSeed = checked "alternate generator seed" (mkGeneratorSeed (ByteString.replicate 32 0xa5))

fixtureGeneratorSeedBytes :: ByteString
fixtureGeneratorSeedBytes = ByteString.pack [0 .. 31]

fixtureGeneratorSeedSource :: RuntimeGeneratorSeedSource
fixtureGeneratorSeedSource = runtimeGeneratorSeedSource (pure fixtureGeneratorSeedBytes)

fixtureApplicationRecoveryConfiguration :: ApplicationRecoveryConfiguration
fixtureApplicationRecoveryConfiguration =
  checked "application recovery configuration" (checkApplicationRecoveryConfiguration 60_000_000)

fixtureAlternateApplicationRecoveryConfiguration :: ApplicationRecoveryConfiguration
fixtureAlternateApplicationRecoveryConfiguration =
  checked "alternate application recovery configuration" (checkApplicationRecoveryConfiguration 30_000_000)

fixturePeerRecoveryConfiguration :: PeerRecoveryConfiguration
fixturePeerRecoveryConfiguration =
  checked "peer recovery configuration" (checkPeerRecoveryConfiguration 60_000_000)

fixtureAlternatePeerRecoveryConfiguration :: PeerRecoveryConfiguration
fixtureAlternatePeerRecoveryConfiguration =
  checked "alternate peer recovery configuration" (checkPeerRecoveryConfiguration 30_000_000)

fixtureOracleContacts :: OracleContactSet
fixtureOracleContacts =
  fixtureOracleContactsAt 61_001

fixtureAlternateOracleContacts :: OracleContactSet
fixtureAlternateOracleContacts =
  fixtureOracleContactsAt 62_001

fixtureOracleContactsAt :: Word16 -> OracleContactSet
fixtureOracleContactsAt firstPort =
  checked
    "Oracle contacts"
    ( oracleContactSet
        ( oracle 200 firstPort
            :| [oracle 201 (firstPort + 1), oracle 202 (firstPort + 2)]
        )
    )
  where
    oracle seed port =
      checked
        "Oracle contact"
        ( oracleContact
            (checked "Oracle node claim" (oracleNodeClaim (fixtureIdentifierBytes seed)))
            (fromString "127.0.0.1")
            port
        )

fixtureOpenDto :: ApplicationClientDto
fixtureOpenDto =
  OpenSession
    (checked "application attachment" (applicationAttachmentClaim (fixtureIdentifierBytes 10)))
    (applicationClientNonce 701)

fixturePeerCandidate :: PeerCandidate
fixturePeerCandidate =
  peerCandidate
    (fixtureHeraldId 3)
    (fixtureHeraldEpoch 5)
    (connectionNonce 77)

fixturePeerHello :: ConnectionNonce -> PeerHello
fixturePeerHello nonce =
  peerHello
    fixtureSystemId
    (fixtureHeraldId 3)
    (fixtureHeraldEpoch 5)
    nonce
    Set.empty
    (controlIndex 0)
    profileCatalogueDigest
    (checkedInitialProjectionDigest fixtureCheckedBootstraps)
    Nothing

fixtureMembershipGenerationId :: HeraldMembershipGenerationId
fixtureMembershipGenerationId = heraldMembershipGenerationId fixtureMembership

fixtureMemberSetDigest :: MemberSetDigest
fixtureMemberSetDigest =
  heraldMembershipGenerationActiveMemberSetDigest fixtureMembership

fixtureMembership :: HeraldMembershipGeneration
fixtureMembership =
  checked
    "fixture membership"
    ( genesisHeraldMembershipGeneration
        fixtureSystemId
        (heraldMemberEpoch fixtureLocalMember :| [heraldMemberEpoch fixtureRemoteMember])
    )

fixtureDeployment :: DeploymentManifest
fixtureDeployment =
  DeploymentManifest
    { deploymentSystemId = fixtureSystemId,
      deploymentLocalHeraldId = heraldMemberId fixtureLocalMember,
      deploymentLocalHeraldEpoch = heraldMemberEpoch fixtureLocalMember,
      deploymentActiveHeralds = fixtureMembers,
      deploymentPredefinedCatalogue = profilePredefinedCatalogueManifest,
      deploymentPrimordialWriterAuthority =
        PrimordialSortWriterAuthority
          { primordialWriterNabla = fixtureNablaId 1,
            primordialWriterAuthorityEpoch = genesisAuthorityEpoch,
            primordialWriterSourceHeraldEpoch = heraldMemberEpoch fixtureLocalMember
          },
      deploymentPrimordialDefinitions =
        [ PrimordialDefinitionManifest
            { primordialManifestRole = catalogueManifestRole entry,
              primordialManifestDescriptorBytes = catalogueManifestDescriptorBytes entry,
              primordialManifestPublicationSequence = nablaSequence (fromIntegral ordinal + 1)
            }
        | (ordinal, entry) <- zip [(0 :: Int) ..] profilePredefinedCatalogueManifest
        ],
      deploymentConfiguredProcesses = fixtureConfiguredProcesses,
      deploymentOracleGenesis =
        OracleGenesisManifest
          { oracleGenesisSystemId = fixtureSystemId,
            oracleGenesisActiveHeralds = fixtureMembers,
            oracleGenesisCatalogueDigest = profileCatalogueDigest,
            oracleGenesisControlIndex = controlIndex 0
          }
    }

fixtureMembers :: [HeraldMember]
fixtureMembers = [fixtureLocalMember, fixtureRemoteMember]

fixtureLocalMember :: HeraldMember
fixtureLocalMember = HeraldMember (fixtureHeraldId 2) (fixtureHeraldEpoch 4)

fixtureRemoteMember :: HeraldMember
fixtureRemoteMember = HeraldMember (fixtureHeraldId 3) (fixtureHeraldEpoch 5)

fixtureLocalBootstrapIds :: [BootstrapManifestId]
fixtureLocalBootstrapIds = [fixtureBootstrapManifestId 10, fixtureBootstrapManifestId 11]

fixtureConfiguredProcesses :: [ConfiguredProcessManifest]
fixtureConfiguredProcesses =
  [ configuredProcess 10 20 30 fixtureLocalMember 40 100,
    configuredProcess 11 21 31 fixtureLocalMember 60 110,
    configuredProcess 12 22 32 fixtureRemoteMember 80 120
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
    { configuredBootstrapManifestId = fixtureBootstrapManifestId manifestByte,
      configuredProcessId = fixtureProcessId processByte,
      configuredProcessEpochId = fixtureProcessEpochId epochByte,
      configuredProcessResidence = heraldMemberEpoch residence,
      configuredProcessRoots =
        [ ConfiguredRootManifest
            { configuredRootRole = role,
              configuredWriterNabla = fixtureNablaId (rootBase + fromIntegral (2 * ordinal)),
              configuredWriterSequencing = UnsequencedNabla,
              configuredReaderDelta = fixtureDeltaId (rootBase + fromIntegral (2 * ordinal + 1)),
              configuredReaderStoreIncarnation = fixtureStoreIncarnationId (incarnationBase + fromIntegral ordinal)
            }
        | (ordinal, role) <- zip [(0 :: Int) ..] allRoles
        ]
    }

allRoles :: [PredefinedSortRole]
allRoles = [minBound .. maxBound]

fixtureIdentifierBytes :: Word8 -> ByteString
fixtureIdentifierBytes seed =
  ByteString.pack
    [ seed + fromIntegral (index * 37 + index * index)
    | index <- [(0 :: Int) .. 31]
    ]

fixtureSystemId :: SystemId
fixtureSystemId = checked "SystemId" (mkSystemId (fixtureIdentifierBytes 0))

fixtureHeraldId :: Word8 -> HeraldId
fixtureHeraldId byte = checked "HeraldId" (mkHeraldId (fixtureIdentifierBytes byte))

fixtureHeraldEpoch :: Word8 -> HeraldEpoch
fixtureHeraldEpoch byte = checked "HeraldEpoch" (mkHeraldEpoch (fixtureIdentifierBytes byte))

fixtureBootstrapManifestId :: Word8 -> BootstrapManifestId
fixtureBootstrapManifestId byte = checked "BootstrapManifestId" (mkBootstrapManifestId (fixtureIdentifierBytes byte))

fixtureProcessId :: Word8 -> ProcessId
fixtureProcessId byte = checked "ProcessId" (mkProcessId (fixtureIdentifierBytes byte))

fixtureProcessEpochId :: Word8 -> ProcessEpochId
fixtureProcessEpochId byte = checked "ProcessEpochId" (mkProcessEpochId (fixtureIdentifierBytes byte))

fixtureNablaId :: Word8 -> NablaId
fixtureNablaId byte = checked "NablaId" (mkNablaId (fixtureIdentifierBytes byte))

fixtureDeltaId :: Word8 -> DeltaId
fixtureDeltaId byte = checked "DeltaId" (mkDeltaId (fixtureIdentifierBytes byte))

fixtureStoreIncarnationId :: Word8 -> StoreIncarnationId
fixtureStoreIncarnationId byte = checked "StoreIncarnationId" (mkStoreIncarnationId (fixtureIdentifierBytes byte))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
