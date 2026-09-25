{-# LANGUAGE OverloadedStrings #-}

module GenesisFixtures
  ( fixtureDeploymentManifest,
    fixtureDeploymentAt,
    fixtureCheckedGenesis,
    fixtureHeraldMembershipGeneration,
    fixtureMembers,
    fixtureLocalMember,
    fixtureRemoteMember,
    fixtureLocalBootstrapIds,
    fixtureRemoteBootstrapId,
    fixtureCheckedInitialBootstraps,
    fixtureCheckedInitialBootstrapsFor,
    fixtureCheckedOracleGenesis,
    fixtureCheckedOracleGenesisFor,
    fixtureCheckedOracleGenesisWithBootstraps,
    fixtureForeignCheckedOracleGenesis,
    fixtureOracleProjectionState,
    fixtureStep14CheckedGenesis,
    fixtureStep14ThirdMember,
    fixtureConfiguredProcesses,
    fixtureIdentifierBytes,
    fixtureGeneratorSeed,
    fixtureGeneratorSeedH1,
    fixtureGeneratorSeedH2,
    fixtureGeneratorSeedH3,
    fixtureApplicationRecoveryConfiguration,
    fixturePeerRecoveryConfiguration,
    currentPeerHelloReceived,
    fixtureOracleContacts,
    fixtureHeraldRetirementReason,
    fixtureRetirementResolution,
    fixtureGenesisRetirementLineage,
    fixtureHeraldAfterProbePrefix,
    initialEffectsAreOracleWatchAndGrace,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Word (Word8)
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    ControlIndex,
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
    controlIndexWord64,
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
  ( FailureProbeResolution (RetireFailureProbeTarget),
    FailureProbeResolutionId,
    HeraldMembershipGeneration,
    deriveFailureProbeResolutionId,
    deriveHeraldFailureProbeId,
    genesisHeraldMembershipGeneration,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason,
    processEndReasonIsHeraldRetirement,
  )
import Eclips.Domain.Sort.Profile
  ( predefinedCatalogueDescriptor,
    profilePredefinedCatalogue,
  )
import Eclips.Domain.Startup (profileCatalogueDigest)
import Eclips.Herald.Application.Recovery
  ( ApplicationRecoveryConfiguration,
    checkApplicationRecoveryConfiguration,
  )
import Eclips.Herald.Discovery
  ( PeerAddress,
    PeerCandidate,
    PeerHello,
  )
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (ArmTimer, RunOracleClientAction),
    effectBatchMembers,
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
    profilePredefinedCatalogueManifest,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedActiveHeralds,
    checkedCatalogueDigest,
    checkedConfigurationDigest,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedInitialTopologyProjection,
    checkedLocalHeraldEpoch,
    checkedSystemId,
  )
import Eclips.Herald.IdGenerator (GeneratorSeed, mkGeneratorSeed)
import Eclips.Herald.Input
  ( HeraldInputBody (OracleInput),
    PeerIngress (PeerHelloReceived),
    heraldInput,
  )
import Eclips.Herald.OracleClient
  ( OracleClientAction (ConnectAndHelloOracle),
    OracleClientIngress (OracleEntriesReceived, OracleHelloReceived),
    OracleContactSet,
    checkOracleContactSet,
    oracleConnectAttemptContact,
    oracleContact,
    oracleContactNode,
    oracleHelloAcceptance,
    oracleNodeClaim,
    oracleObservedTerm,
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection qualified as OracleProjection
import Eclips.Herald.PeerLiveness
  ( PeerRecoveryConfiguration,
    checkPeerRecoveryConfiguration,
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
    startupGenesis,
    startupInitialBootstraps,
    startupLastObservedTime,
    startupOracleClientState,
    startupOracleProjectionState,
  )
import Eclips.Herald.Timer.Internal (TimerAttempt (IsolationGraceTimerAttempt))
import Eclips.Herald.Transition (stepHerald)
import Eclips.Oracle.Canonical (canonicalizeAppliedOracleEntry)
import Eclips.Oracle.Genesis
  ( CheckedOracleGenesis,
    checkOracleGenesis,
    deriveRaftConfigurationDigest,
    oracleGenesis,
    raftVoterBinding,
  )
import Eclips.Oracle.Identity (oracleClientRequestId)
import Eclips.Oracle.Label qualified as Live
import Eclips.Oracle.Voter qualified as Voter
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

fixturePeerRecoveryConfiguration :: PeerRecoveryConfiguration
fixturePeerRecoveryConfiguration =
  case checkPeerRecoveryConfiguration 1_000_000 of
    Left problem -> error ("invalid fixture peer recovery configuration: " <> show problem)
    Right configuration -> configuration

fixtureApplicationRecoveryConfiguration :: ApplicationRecoveryConfiguration
fixtureApplicationRecoveryConfiguration =
  case checkApplicationRecoveryConfiguration 1_000_000 of
    Left problem -> error ("invalid fixture application recovery configuration: " <> show problem)
    Right configuration -> configuration

fixtureHeraldRetirementReason :: ProcessEndReason
fixtureHeraldRetirementReason =
  case filter processEndReasonIsHeraldRetirement [minBound .. maxBound] of
    [reason] -> reason
    reasons ->
      error
        ( "expected exactly one Herald-retirement process-End reason, got "
            <> show reasons
        )

-- | Retain a genuine canonical no-op prefix through the ordinary Oracle watch.
-- Cancelling an unknown voter change alters no projection facts,
-- but all composed owners observe index one before detached retirement fixtures
-- exercise their membership-advance seam at index two.
fixtureHeraldAfterProbePrefix :: HeraldState -> HeraldState
fixtureHeraldAfterProbePrefix original
  | OracleProjection.oracleViewControlIndex (OracleProjection.oracleView (startupOracleProjectionState original)) /= controlIndex 0 =
      error "fixture probe prefix requires the initial control coordinate"
  | otherwise = apply (OracleEntriesReceived binding (canonical NonEmpty.:| [])) connected
  where
    genesis = startupGenesis original
    local = checkedLocalHeraldEpoch genesis
    initialOracle = Live.initialOracleState (fixtureCheckedOracleGenesisWithBootstraps genesis (startupInitialBootstraps original))
    envelope = Live.oracleEnvelope (oracleClientRequestId local 1) Nothing local (Voter.cancelVoterChangeCommand (checked "unknown voter change" (Voter.mkVoterChangeId (controlIndex 999999))))
    (_, outcome) = Live.submitOracleState envelope initialOracle
    canonical = case Live.oracleSubmissionOutcomeView outcome of
      Live.OracleSubmissionCommittedView _ entry -> canonicalizeAppliedOracleEntry entry
      other -> error ("fixture canonical probe prefix: " <> show other)
    connected = case OracleClient.oracleClientCurrentBinding (startupOracleClientState original) of
      Just _ -> original
      Nothing -> case OracleClient.oracleClientActions (startupOracleClientState original) of
        [ConnectAndHelloOracle attempt _] ->
          let node = oracleContactNode (oracleConnectAttemptContact attempt)
           in apply (OracleHelloReceived attempt (oracleHelloAcceptance node (oracleObservedTerm 1) (controlIndex 0) (Just node) True)) original
        actions -> error ("fixture Oracle binding unavailable: " <> show actions)
    binding = case OracleClient.oracleClientCurrentBinding (startupOracleClientState connected) of
      Just retained -> retained
      Nothing -> error "fixture Oracle Hello did not establish a binding"
    apply ingress state = fst (checked "apply fixture Oracle prefix" (stepHerald (heraldInput (startupLastObservedTime state) (OracleInput ingress)) state))

-- | A detached membership fixture still names a real earlier probe coordinate.
-- Fixtures applying this change must also retain the corresponding prior
-- control prefix; this helper never advances an owner or invents probe zero.
fixtureRetirementResolution :: ControlIndex -> FailureProbeResolutionId
fixtureRetirementResolution index
  | controlIndexWord64 index <= 1 = error "retirement fixture needs an earlier positive control prefix"
  | otherwise =
      deriveFailureProbeResolutionId
        (checked "fixture retirement probe" (deriveHeraldFailureProbeId (controlIndex (controlIndexWord64 index - 1))))
        RetireFailureProbeTarget

fixtureDeploymentManifest :: DeploymentManifest
fixtureDeploymentManifest = fixtureDeploymentAt fixtureLocalMember

fixtureDeploymentAt :: HeraldMember -> DeploymentManifest
fixtureDeploymentAt local =
  DeploymentManifest
    { deploymentSystemId = fixtureSystemId,
      deploymentLocalHeraldId = heraldMemberId local,
      deploymentLocalHeraldEpoch = heraldMemberEpoch local,
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

fixtureCheckedGenesis :: CheckedHeraldGenesis
fixtureCheckedGenesis =
  checked "fixture deployment genesis" (checkHeraldGenesis fixtureDeploymentManifest)

fixtureHeraldMembershipGeneration :: HeraldMembershipGeneration
fixtureHeraldMembershipGeneration =
  checked
    "fixture Herald membership"
    ( genesisHeraldMembershipGeneration
        (checkedSystemId fixtureCheckedGenesis)
        (NonEmpty.fromList (heraldMemberEpoch <$> checkedActiveHeralds fixtureCheckedGenesis))
    )

-- | Construct one successful peer-Hello ingress using the exact current
-- generation evidence owned by the predecessor Herald. Tests that exercise
-- stale or conflicting membership evidence construct 'PeerHelloReceived'
-- directly.
currentPeerHelloReceived ::
  HeraldState ->
  PeerCandidate ->
  Set PeerAddress ->
  PeerHello ->
  PeerIngress
currentPeerHelloReceived predecessor candidate addresses hello =
  PeerHelloReceived
    candidate
    addresses
    hello
    (heraldMembershipGenerationId membership)
    (heraldMembershipGenerationActiveMemberSetDigest membership)
    Nothing
  where
    membership =
      OracleProjection.oracleViewCurrentHeraldMembership
        (OracleProjection.oracleView (startupOracleProjectionState predecessor))

fixtureMembers :: [HeraldMember]
fixtureMembers = [fixtureLocalMember, fixtureRemoteMember]

fixtureLocalMember :: HeraldMember
fixtureLocalMember = HeraldMember (fixtureHeraldId 2) (fixtureHeraldEpoch 4)

fixtureRemoteMember :: HeraldMember
fixtureRemoteMember = HeraldMember (fixtureHeraldId 3) (fixtureHeraldEpoch 5)

fixtureLocalBootstrapIds :: [BootstrapManifestId]
fixtureLocalBootstrapIds = [fixtureBootstrapManifestId 10, fixtureBootstrapManifestId 11]

fixtureRemoteBootstrapId :: BootstrapManifestId
fixtureRemoteBootstrapId = fixtureBootstrapManifestId 12

-- | A shared exact Oracle/Herald genesis pair for detached Step-14 tests. One
-- local and one remote configured process are materialized at control zero;
-- the second local process remains available for exact Start fixtures.
fixtureCheckedInitialBootstraps :: CheckedInitialBootstraps
fixtureCheckedInitialBootstraps = fixtureCheckedInitialBootstrapsFor fixtureStep14CheckedGenesis

fixtureCheckedInitialBootstrapsFor :: CheckedHeraldGenesis -> CheckedInitialBootstraps
fixtureCheckedInitialBootstrapsFor genesis =
  checked
    "fixture initial bootstraps"
    ( checkInitialBootstraps
        genesis
        ( PrimordialProcessManifest
            (take 1 fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId])
        )
    )

fixtureCheckedOracleGenesis :: CheckedOracleGenesis
fixtureCheckedOracleGenesis = fixtureCheckedOracleGenesisFor fixtureStep14CheckedGenesis

fixtureCheckedOracleGenesisFor :: CheckedHeraldGenesis -> CheckedOracleGenesis
fixtureCheckedOracleGenesisFor genesis = fixtureCheckedOracleGenesisWithBootstraps genesis (fixtureCheckedInitialBootstrapsFor genesis)

fixtureCheckedOracleGenesisWithBootstraps :: CheckedHeraldGenesis -> CheckedInitialBootstraps -> CheckedOracleGenesis
fixtureCheckedOracleGenesisWithBootstraps genesis bootstraps =
  checked "fixture checked Oracle genesis" (checkOracleGenesis raw)
  where
    nodes = take (length (checkedActiveHeralds genesis)) fixtureRaftNodes
    nativeConfiguration = fixtureRaftConfigurationFor nodes
    bindings =
      zipWith
        raftVoterBinding
        nodes
        (heraldMemberEpoch <$> checkedActiveHeralds genesis)
    raw =
      oracleGenesis
        (checkedSystemId genesis)
        (checkedActiveHeralds genesis)
        (checkedCatalogueDigest genesis)
        (predefinedCatalogueDescriptor <$> profilePredefinedCatalogue)
        (checkedInitialBootstraps bootstraps)
        (checkedConfigurationDigest genesis)
        (checkedInitialTopologyProjection bootstraps)
        (checkedInitialProjectionDigest bootstraps)
        bindings
        nativeConfiguration
        ( deriveRaftConfigurationDigest
            (checkedSystemId genesis)
            bindings
            nativeConfiguration
        )

-- | A semantically projection-compatible but exactly different Oracle genesis.
-- Its distinct Raft seed/configuration makes it suitable for proving that a
-- detached Step-14 owner cannot accept evidence from another experimental run.
fixtureForeignCheckedOracleGenesis :: CheckedOracleGenesis
fixtureForeignCheckedOracleGenesis =
  checked "foreign fixture checked Oracle genesis" (checkOracleGenesis raw)
  where
    bindings =
      zipWith
        raftVoterBinding
        fixtureRaftNodes
        (heraldMemberEpoch <$> checkedActiveHeralds fixtureStep14CheckedGenesis)
    raw =
      oracleGenesis
        (checkedSystemId fixtureStep14CheckedGenesis)
        (checkedActiveHeralds fixtureStep14CheckedGenesis)
        (checkedCatalogueDigest fixtureStep14CheckedGenesis)
        (predefinedCatalogueDescriptor <$> profilePredefinedCatalogue)
        (checkedInitialBootstraps fixtureCheckedInitialBootstraps)
        (checkedConfigurationDigest fixtureStep14CheckedGenesis)
        (checkedInitialTopologyProjection fixtureCheckedInitialBootstraps)
        (checkedInitialProjectionDigest fixtureCheckedInitialBootstraps)
        bindings
        fixtureForeignRaftConfiguration
        ( deriveRaftConfigurationDigest
            (checkedSystemId fixtureStep14CheckedGenesis)
            bindings
            fixtureForeignRaftConfiguration
        )

fixtureOracleProjectionState :: OracleProjection.State
fixtureOracleProjectionState =
  OracleProjection.initialState
    fixtureStep14CheckedGenesis
    fixtureCheckedInitialBootstraps

fixtureStep14CheckedGenesis :: CheckedHeraldGenesis
fixtureStep14CheckedGenesis =
  checked
    "three-Herald Step-14 genesis"
    ( checkHeraldGenesis
        fixtureDeploymentManifest
          { deploymentActiveHeralds = step14Members,
            deploymentOracleGenesis =
              (deploymentOracleGenesis fixtureDeploymentManifest)
                { oracleGenesisActiveHeralds = step14Members
                }
          }
    )
  where
    step14Members = fixtureMembers <> [fixtureStep14ThirdMember]

fixtureStep14ThirdMember :: HeraldMember
fixtureStep14ThirdMember =
  HeraldMember
    (fixtureHeraldId 0x71)
    (fixtureHeraldEpoch 0x72)

fixtureRaftNodes :: [RaftNodeId]
fixtureRaftNodes =
  [ checked "fixture Raft node" (mkRaftNodeId (fixtureIdentifierBytes byte))
  | byte <- [0x91, 0x92, 0x93]
  ]

fixtureRaftConfigurationFor :: [RaftNodeId] -> RaftNativeConfiguration
fixtureRaftConfigurationFor nodes =
  checkedRaftNativeConfiguration
    ( checked
        "fixture Raft genesis"
        ( checkRaftGenesis
            ( raftGenesis
                localNode
                nodes
                (duration 100000)
                (duration 300000)
                (duration 500000)
            )
        )
    )
  where
    localNode = case nodes of
      first : _ -> first
      [] -> error "fixture Raft configuration requires a voter"
    duration value = checked "fixture Raft duration" (mkRaftDurationMicros value)

fixtureForeignRaftConfiguration :: RaftNativeConfiguration
fixtureForeignRaftConfiguration =
  checkedRaftNativeConfiguration
    ( checked
        "foreign fixture Raft genesis"
        ( checkRaftGenesis
            ( raftGenesis
                fixtureLastRaftNode
                fixtureRaftNodes
                (duration 110000)
                (duration 300000)
                (duration 500000)
            )
        )
    )
  where
    duration value = checked "foreign fixture Raft duration" (mkRaftDurationMicros value)

fixtureLastRaftNode :: RaftNodeId
fixtureLastRaftNode = case reverse fixtureRaftNodes of
  final : _ -> final
  [] -> error "fixture has no Raft nodes"

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
              configuredReaderStoreIncarnation =
                fixtureStoreIncarnationId (incarnationBase + fromIntegral ordinal)
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

fixtureGeneratorSeed :: GeneratorSeed
fixtureGeneratorSeed =
  checked "default GeneratorSeed" (mkGeneratorSeed (ByteString.pack [0 .. 31]))

fixtureGeneratorSeedH1 :: GeneratorSeed
fixtureGeneratorSeedH1 =
  checked "H1 GeneratorSeed" (mkGeneratorSeed (ByteString.replicate 32 0x11))

fixtureGeneratorSeedH2 :: GeneratorSeed
fixtureGeneratorSeedH2 =
  checked "H2 GeneratorSeed" (mkGeneratorSeed (ByteString.replicate 32 0x22))

fixtureGeneratorSeedH3 :: GeneratorSeed
fixtureGeneratorSeedH3 =
  checked "H3 GeneratorSeed" (mkGeneratorSeed (ByteString.replicate 32 0x33))

fixtureOracleContacts :: OracleContactSet
fixtureOracleContacts =
  checked
    "fixed Oracle contacts"
    ( checkOracleContactSet
        [ checked "Oracle contact R1" (oracleContact (node 0x91) "127.0.0.1" 29101),
          checked "Oracle contact R2" (oracleContact (node 0x92) "127.0.0.1" 29102),
          checked "Oracle contact R3" (oracleContact (node 0x93) "127.0.0.1" 29103)
        ]
    )
  where
    node byte = checked "Oracle node claim" (oracleNodeClaim (fixtureIdentifierBytes byte))

initialEffectsAreOracleWatchAndGrace :: EffectBatch -> Bool
initialEffectsAreOracleWatchAndGrace effects = case effectBatchMembers effects of
  [RunOracleClientAction (ConnectAndHelloOracle _ _), ArmTimer (IsolationGraceTimerAttempt _ _) _] -> True
  _ -> False

fixtureSystemId :: SystemId
fixtureSystemId = checked "SystemId" (mkSystemId (fixtureIdentifierBytes 0))

fixtureHeraldId :: Word8 -> HeraldId
fixtureHeraldId byte = checked "HeraldId" (mkHeraldId (fixtureIdentifierBytes byte))

fixtureHeraldEpoch :: Word8 -> HeraldEpoch
fixtureHeraldEpoch byte = checked "HeraldEpoch" (mkHeraldEpoch (fixtureIdentifierBytes byte))

fixtureBootstrapManifestId :: Word8 -> BootstrapManifestId
fixtureBootstrapManifestId byte =
  checked "BootstrapManifestId" (mkBootstrapManifestId (fixtureIdentifierBytes byte))

fixtureProcessId :: Word8 -> ProcessId
fixtureProcessId byte = checked "ProcessId" (mkProcessId (fixtureIdentifierBytes byte))

fixtureProcessEpochId :: Word8 -> ProcessEpochId
fixtureProcessEpochId byte =
  checked "ProcessEpochId" (mkProcessEpochId (fixtureIdentifierBytes byte))

fixtureNablaId :: Word8 -> NablaId
fixtureNablaId byte = checked "NablaId" (mkNablaId (fixtureIdentifierBytes byte))

fixtureDeltaId :: Word8 -> DeltaId
fixtureDeltaId byte = checked "DeltaId" (mkDeltaId (fixtureIdentifierBytes byte))

fixtureStoreIncarnationId :: Word8 -> StoreIncarnationId
fixtureStoreIncarnationId byte =
  checked "StoreIncarnationId" (mkStoreIncarnationId (fixtureIdentifierBytes byte))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

-- | Existing single-retirement fixtures still supply a checked complete
-- history; no pair of arbitrary generation values becomes an ancestry proof.
fixtureGenesisRetirementLineage :: Membership.HeraldMembershipGeneration -> Membership.HeraldMembershipGeneration -> Membership.HeraldMembershipLineage
fixtureGenesisRetirementLineage predecessor successor = either (error . show) id $ do
  history <- Membership.heraldMembershipHistory (predecessor NonEmpty.:| [successor])
  Membership.heraldMembershipLineage (Membership.heraldMembershipGenerationId predecessor) (Membership.heraldMembershipGenerationId successor) history
