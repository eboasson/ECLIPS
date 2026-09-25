{-# LANGUAGE OverloadedStrings #-}

module OracleFixtures
  ( checked,
    identifierBytes,
    fixtureSystemId,
    fixtureMembers,
    fixtureHeraldId,
    fixtureHeraldEpoch,
    fixtureRemoteHeraldEpoch,
    fixtureThirdHeraldEpoch,
    fixtureProcessEpoch,
    fixtureRemoteProcessEpoch,
    fixtureStartProcessEpoch,
    fixtureAlternateStartProcessEpoch,
    fixtureLocalBootstrap,
    fixtureBootstraps,
    fixtureStart,
    fixtureAlternateStart,
    fixtureThirdStart,
    fixtureRemoteStart,
    fixtureGenesisTemplate,
    fixtureConfigurationDigest,
    configuredProcess,
    fixtureTopology,
    fixtureTopologyFor,
    fixtureDescriptors,
    fixtureRaftNodes,
    fixtureRaftLocalNode,
    fixtureRaftConfiguration,
    raftConfiguration,
    raftConfigurationForVoters,
    fixtureBindings,
    fixtureOracleGenesis,
    fixtureCheckedGenesis,
    fixtureInitialState,
    validCommand,
    alternateCommand,
    validEnvelope,
    envelopeFor,
    systemId,
    heraldId,
    heraldEpoch,
    processEpochId,
    globalObjectId,
    nablaId,
    deltaId,
    storeIncarnationId,
    bootstrapManifestId,
    processId,
    raftNodeId,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    DeltaId,
    GlobalObjectId,
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
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkHeraldId,
    mkNablaId,
    mkProcessEpochId,
    mkProcessId,
    mkStoreIncarnationId,
    mkSystemId,
  )
import Eclips.Domain.ProcessStart (ProcessStart, processStart)
import Eclips.Domain.Sort.Canonical (CanonicalDescriptor)
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
    OracleEnvelope,
    oracleEnvelope,
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
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.State (OracleState, OracleStateDigestMode (OracleStateDigestEnabled))
import Eclips.Oracle.Transition (initialOracleWithStateDigestMode)
import Eclips.Raft.Genesis
  ( RaftNativeConfiguration,
    checkRaftGenesis,
    checkedRaftNativeConfiguration,
    raftGenesis,
  )
import Eclips.Raft.Identity
  ( RaftNodeId,
    mkRaftDurationMicros,
    mkRaftNodeId,
  )

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

identifierBytes :: Word8 -> ByteString
identifierBytes seed = ByteString.pack [seed + fromIntegral index | index <- [(0 :: Int) .. 31]]

fixtureSystemId :: SystemId
fixtureSystemId = systemId 1

fixtureHeraldId :: HeraldId
fixtureHeraldId = heraldId 10

fixtureHeraldEpoch :: HeraldEpoch
fixtureHeraldEpoch = heraldEpoch 11

fixtureRemoteHeraldEpoch :: HeraldEpoch
fixtureRemoteHeraldEpoch = heraldEpoch 13

fixtureThirdHeraldEpoch :: HeraldEpoch
fixtureThirdHeraldEpoch = heraldEpoch 15

fixtureMembers :: [HeraldMember]
fixtureMembers =
  [ HeraldMember fixtureHeraldId fixtureHeraldEpoch,
    HeraldMember (heraldId 12) fixtureRemoteHeraldEpoch,
    HeraldMember (heraldId 14) fixtureThirdHeraldEpoch
  ]

fixtureProcessEpoch :: ProcessEpochId
fixtureProcessEpoch = processEpochId 31

fixtureRemoteProcessEpoch :: ProcessEpochId
fixtureRemoteProcessEpoch = processEpochId 111

fixtureStartProcessEpoch :: ProcessEpochId
fixtureStartProcessEpoch = processEpochId 171

fixtureAlternateStartProcessEpoch :: ProcessEpochId
fixtureAlternateStartProcessEpoch = processEpochId 172

fixtureBootstraps :: [AppliedProcessBootstrap]
fixtureBootstraps =
  [ materialize 20 21 fixtureProcessEpoch fixtureHeraldEpoch 40,
    materialize 100 101 fixtureRemoteProcessEpoch fixtureRemoteHeraldEpoch 120
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

fixtureLocalBootstrap :: AppliedProcessBootstrap
fixtureLocalBootstrap =
  checked
    "local genesis configured-process materialization"
    ( materializeConfiguredProcessBootstrap
        (controlIndex 0)
        (genesisPredefinedOccurrenceSet fixtureSystemId)
        (configuredProcess 20 21 fixtureProcessEpoch fixtureHeraldEpoch 40)
    )

fixtureGenesisTemplate :: ConfiguredProcessBootstrap
fixtureGenesisTemplate =
  configuredProcess 160 161 fixtureStartProcessEpoch fixtureHeraldEpoch 180

fixtureConfigurationDigest :: ConfigurationDigest
fixtureConfigurationDigest =
  checked "ConfigurationDigest" (mkConfigurationDigest (identifierBytes 7))

configuredProcess :: Word8 -> Word8 -> ProcessEpochId -> HeraldEpoch -> Word8 -> ConfiguredProcessBootstrap
configuredProcess manifestSeed processSeed epoch residence rootSeed =
  maybe
    (error "complete configured-process fixture was rejected")
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
  fixtureTopologyFor fixtureSystemId fixtureMembers fixtureBootstraps

fixtureTopologyFor :: SystemId -> [HeraldMember] -> [AppliedProcessBootstrap] -> CheckedInitialTopologyProjection
fixtureTopologyFor system members bootstraps =
  checked
    "initial topology fixture"
    ( checkInitialTopologyProjection
        system
        members
        bootstraps
        (InitialTopologyManifest [])
    )

fixtureDescriptors :: [CanonicalDescriptor]
fixtureDescriptors = fmap predefinedCatalogueDescriptor profilePredefinedCatalogue

fixtureRaftNodes :: [RaftNodeId]
fixtureRaftNodes = fmap raftNodeId [2, 3, 4]

fixtureRaftLocalNode :: RaftNodeId
fixtureRaftLocalNode = raftNodeId 2

fixtureRaftConfiguration :: RaftNativeConfiguration
fixtureRaftConfiguration =
  raftConfiguration 100000 300000 500000

raftConfiguration :: Word64 -> Word64 -> Word64 -> RaftNativeConfiguration
raftConfiguration = raftConfigurationForVoters fixtureRaftNodes

raftConfigurationForVoters :: [RaftNodeId] -> Word64 -> Word64 -> Word64 -> RaftNativeConfiguration
raftConfigurationForVoters voters heartbeat lower upper =
  checkedRaftNativeConfiguration
    ( checked
        "Raft genesis fixture"
        ( checkRaftGenesis
            ( raftGenesis
                fixtureRaftLocalNode
                voters
                (checked "heartbeat" (mkRaftDurationMicros heartbeat))
                (checked "election lower" (mkRaftDurationMicros lower))
                (checked "election upper" (mkRaftDurationMicros upper))
            )
        )
    )

fixtureBindings :: [RaftVoterBinding]
fixtureBindings = zipWith raftVoterBinding fixtureRaftNodes (fmap heraldMemberEpoch fixtureMembers)

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
    fixtureRaftConfiguration
    (deriveRaftConfigurationDigest fixtureSystemId fixtureBindings fixtureRaftConfiguration)

fixtureCheckedGenesis :: CheckedOracleGenesis
fixtureCheckedGenesis = checked "Oracle genesis fixture" (checkOracleGenesis fixtureOracleGenesis)

fixtureInitialState :: OracleState
fixtureInitialState = checked "initial Oracle fixture" (initialOracleWithStateDigestMode OracleStateDigestEnabled fixtureCheckedGenesis)

validCommand :: OracleCommand
validCommand = startProcessEpochCommand fixtureStart

alternateCommand :: OracleCommand
alternateCommand = startProcessEpochCommand fixtureAlternateStart

validEnvelope :: OracleEnvelope
validEnvelope = envelopeFor 1 validCommand

envelopeFor :: Word64 -> OracleCommand -> OracleEnvelope
envelopeFor sequenceNumber command =
  oracleEnvelope
    (oracleClientRequestId fixtureHeraldEpoch sequenceNumber)
    Nothing
    fixtureHeraldEpoch
    command

systemId :: Word8 -> SystemId
systemId seed = checked "SystemId" (mkSystemId (identifierBytes seed))

heraldId :: Word8 -> HeraldId
heraldId seed = checked "HeraldId" (mkHeraldId (identifierBytes seed))

heraldEpoch :: Word8 -> HeraldEpoch
heraldEpoch seed = checked "HeraldEpoch" (mkHeraldEpoch (identifierBytes seed))

processEpochId :: Word8 -> ProcessEpochId
processEpochId seed = checked "ProcessEpochId" (mkProcessEpochId (identifierBytes seed))

globalObjectId :: Word8 -> GlobalObjectId
globalObjectId seed = checked "GlobalObjectId" (mkGlobalObjectId (identifierBytes seed))

nablaId :: Word8 -> NablaId
nablaId seed = checked "NablaId" (mkNablaId (identifierBytes seed))

deltaId :: Word8 -> DeltaId
deltaId seed = checked "DeltaId" (mkDeltaId (identifierBytes seed))

storeIncarnationId :: Word8 -> StoreIncarnationId
storeIncarnationId seed = checked "StoreIncarnationId" (mkStoreIncarnationId (identifierBytes seed))

bootstrapManifestId :: Word8 -> BootstrapManifestId
bootstrapManifestId seed = checked "BootstrapManifestId" (mkBootstrapManifestId (identifierBytes seed))

processId :: Word8 -> ProcessId
processId seed = checked "ProcessId" (mkProcessId (identifierBytes seed))

raftNodeId :: Word8 -> RaftNodeId
raftNodeId seed = checked "RaftNodeId" (mkRaftNodeId (identifierBytes seed))

-- Dynamic requests are independent of every immutable genesis root assignment.
fixtureStart, fixtureAlternateStart, fixtureThirdStart, fixtureRemoteStart :: ProcessStart
fixtureStart = processStart (processId 161) fixtureStartProcessEpoch fixtureHeraldEpoch
fixtureAlternateStart = processStart (processId 163) fixtureAlternateStartProcessEpoch fixtureHeraldEpoch
fixtureThirdStart = processStart (processId 161) (processEpochId 174) fixtureHeraldEpoch
fixtureRemoteStart = processStart (processId 165) (processEpochId 173) fixtureRemoteHeraldEpoch
