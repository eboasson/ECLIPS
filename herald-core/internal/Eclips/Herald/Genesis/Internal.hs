{-# LANGUAGE OverloadedStrings #-}

-- | Pure checking of the immutable Herald startup configuration.
--
-- Raw deployment records are deliberately first-order data.  Successful
-- checking canonicalises every set and list, validates the closed profile-0.1
-- catalogue and primordial publication provenance, and hides the resulting
-- constructor.  The separately checked initial-process fixture contains only
-- references into the retained configured-bootstrap catalogue.
module Eclips.Herald.Genesis.Internal
  ( -- * Shared startup vocabulary
    PredefinedSortRole (..),
    HeraldMember (..),
    PredefinedCatalogueManifest (..),
    PrimordialSortWriterAuthority (..),
    PrimordialDefinitionManifest (..),
    ConfiguredRootManifest (..),
    ConfiguredProcessManifest (..),
    OracleGenesisManifest (..),
    DeploymentManifest (..),
    PrimordialProcessManifest (..),

    -- * Closed profile constants and derivations
    profilePredefinedCatalogue,
    profilePredefinedCatalogueManifest,
    profileCatalogueDigest,
    CatalogueDigest,
    catalogueDigestBytes,
    ConfigurationDigest,
    configurationDigestBytes,
    InitialProjectionDigest,
    initialProjectionDigestBytes,
    InitialTopologyEdgeManifest (..),
    InitialTopologyManifest (..),
    InitialTopologyError (..),
    CheckedInitialTopologyProjection,
    checkedInitialTopologyVertices,
    checkedInitialTopologyEdges,

    -- * Genesis checking
    GenesisError (..),
    CheckedHeraldGenesis,
    checkHeraldGenesis,
    checkJoiningHeraldGenesis,
    checkedStartupAdmission,
    checkedSystemId,
    checkedLocalHeraldId,
    checkedLocalHeraldEpoch,
    checkedActiveHeralds,
    checkedActiveHeraldEpochs,
    checkedCatalogueDigest,
    checkedConfigurationDigest,
    checkedPrimordialReplicas,
    checkedPredefinedOccurrenceSet,
    checkedConfiguredProcessBootstraps,
    lookupConfiguredProcessBootstrap,
    GenesisAuthority (..),
    genesisAuthorityEpoch,
    SystemBootstrapWriter,
    systemBootstrapWriterResidence,
    systemBootstrapWriterRole,
    systemBootstrapWriterNablaId,
    systemBootstrapWriterAuthority,
    checkedSystemBootstrapWriters,
    lookupSystemBootstrapWriter,
    checkedLocalSystemBootstrapWriter,
    ConfiguredProcessBootstrap,
    configuredProcessBootstrapManifestId,
    configuredEnvironmentWiringObjects,
    configuredProcessBootstrapProcessId,
    configuredProcessBootstrapProcessEpochId,
    configuredProcessBootstrapResidence,
    materializeConfiguredProcessBootstrap,
    checkedOracleControlIndex,
    CheckedCatalogueEntry,
    checkedCatalogueEntryRole,
    checkedCatalogueEntryDescriptor,
    checkedCatalogueEntrySortId,
    PrimordialDefinitionReplica,
    PredefinedOccurrenceSet,
    primordialReplicaRole,
    primordialReplicaDescriptor,
    primordialReplicaSortId,
    primordialReplicaOccurrenceId,
    primordialReplicaPublicationId,
    primordialReplicaPublication,
    primordialReplicaSourceAuthority,

    -- * Checked process bootstrap fixture
    BootstrapFault (..),
    CheckedInitialBootstraps,
    checkInitialBootstraps,
    checkInitialBootstrapsWithTopology,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedInitialTopologyProjection,
    AppliedProcessBootstrap,
    appliedBootstrapManifestId,
    appliedProcessId,
    appliedProcessEpochId,
    appliedProcessObjectId,
    appliedProcessResidence,
    appliedProcessAuthority,
    appliedProcessRoots,
    AppliedRoot,
    AppliedRootRole (..),
    appliedRootCatalogueRole,
    appliedRootSortId,
    appliedRootOccurrenceId,
    appliedRootControlPrerequisite,
    appliedRootRole,
    appliedRootObjectId,
    appliedRootAuthority,
    appliedRootPlacement,
  )
where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    BootstrapManifestId,
    ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    HeraldId,
    NablaId,
    NablaSequence,
    NablaSequencing (..),
    ProcessEpochId,
    ProcessId,
    SortId,
    StoreIncarnationId,
    SystemId,
    controlIndex,
    globalObjectIdFromDeltaId,
    globalObjectIdFromNablaId,
    globalObjectIdFromProcessEpochId,
  )
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    CanonicalDescriptorError,
    canonicalDescriptorBytes,
    validateCanonicalDescriptorIdentity,
  )
import Eclips.Domain.Sort.Descriptor
  ( DescriptorAdmission (..),
    StructuralCarrierRole (..),
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    AppliedRoot,
    AppliedRootRole (..),
    CatalogueDigest,
    CheckedInitialTopologyProjection,
    ConfigurationDigest,
    ConfiguredProcessBootstrap,
    ConfiguredRootBootstrap,
    HeraldMember (..),
    InitialProjectionDigest,
    InitialTopologyEdgeManifest (..),
    InitialTopologyError (..),
    InitialTopologyManifest (..),
    OracleGenesisManifest (..),
    PredefinedCatalogueEntry,
    PredefinedOccurrenceSet,
    PredefinedSortRole (..),
    PrimordialDefinitionReplica,
    PrimordialSortWriterAuthority (..),
    allPredefinedSortRoles,
    allStructuralCarrierRoles,
    appliedBootstrapManifestId,
    appliedProcessAuthority,
    appliedProcessEpochId,
    appliedProcessId,
    appliedProcessObjectId,
    appliedProcessResidence,
    appliedProcessRoots,
    appliedRootAuthority,
    appliedRootCatalogueRole,
    appliedRootControlPrerequisite,
    appliedRootObjectId,
    appliedRootOccurrenceId,
    appliedRootPlacement,
    appliedRootRole,
    appliedRootSortId,
    catalogueDigestBytes,
    checkInitialTopologyProjection,
    checkedInitialTopologyEdges,
    checkedInitialTopologyVertices,
    configurationDigestBytes,
    configuredEnvironmentWiringObjects,
    configuredProcessBootstrapManifestId,
    configuredProcessBootstrapProcessEpochId,
    configuredProcessBootstrapProcessId,
    configuredProcessBootstrapResidence,
    configuredRootBootstrap,
    deriveConfigurationDigest,
    deriveInitialProjectionDigest,
    deriveSystemBootstrapWriterNablaId,
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
    genesisPredefinedOccurrenceSet,
    initialProjectionDigestBytes,
    materializeConfiguredProcessBootstrap,
    mkConfiguredProcessBootstrap,
    predefinedCatalogueDescriptor,
    predefinedCatalogueRole,
    predefinedCatalogueSortId,
    primordialDefinitionReplica,
    primordialReplicaDescriptor,
    primordialReplicaOccurrenceId,
    primordialReplicaPublication,
    primordialReplicaPublicationId,
    primordialReplicaRole,
    primordialReplicaSortId,
    primordialReplicaSourceAuthority,
    profileCatalogueDigest,
    profilePredefinedCatalogue,
  )
import Eclips.Oracle.Admission qualified as Admission

-- | Raw catalogue declaration.  Both bytes and the claimed content address are
-- checked against the one closed profile catalogue.
data PredefinedCatalogueManifest = PredefinedCatalogueManifest
  { catalogueManifestRole :: PredefinedSortRole,
    catalogueManifestDescriptorBytes :: ByteString,
    catalogueManifestSortId :: SortId
  }
  deriving stock (Eq, Show)

-- | Raw ordinary sort-definition replica supplied by deployment genesis.
data PrimordialDefinitionManifest = PrimordialDefinitionManifest
  { primordialManifestRole :: PredefinedSortRole,
    primordialManifestDescriptorBytes :: ByteString,
    primordialManifestPublicationSequence :: NablaSequence
  }
  deriving stock (Eq, Show)

-- | One configured writer/reader root pair for a catalogue entry.
--
-- Sort and occurrence are deliberately absent: they are derived from the
-- checked catalogue rather than accepted as independently editable copies.
data ConfiguredRootManifest = ConfiguredRootManifest
  { configuredRootRole :: PredefinedSortRole,
    configuredWriterNabla :: NablaId,
    configuredWriterSequencing :: NablaSequencing,
    configuredReaderDelta :: DeltaId,
    configuredReaderStoreIncarnation :: StoreIncarnationId
  }
  deriving stock (Eq, Show)

data ConfiguredProcessManifest = ConfiguredProcessManifest
  { configuredBootstrapManifestId :: BootstrapManifestId,
    configuredProcessId :: ProcessId,
    configuredProcessEpochId :: ProcessEpochId,
    configuredProcessResidence :: HeraldEpoch,
    configuredProcessRoots :: [ConfiguredRootManifest]
  }
  deriving stock (Eq, Show)

data DeploymentManifest = DeploymentManifest
  { deploymentSystemId :: SystemId,
    deploymentLocalHeraldId :: HeraldId,
    deploymentLocalHeraldEpoch :: HeraldEpoch,
    deploymentActiveHeralds :: [HeraldMember],
    deploymentPredefinedCatalogue :: [PredefinedCatalogueManifest],
    deploymentPrimordialWriterAuthority :: PrimordialSortWriterAuthority,
    deploymentPrimordialDefinitions :: [PrimordialDefinitionManifest],
    deploymentConfiguredProcesses :: [ConfiguredProcessManifest],
    deploymentOracleGenesis :: OracleGenesisManifest
  }
  deriving stock (Eq, Show)

-- | Startup-only references into the checked configured-process catalogue.
newtype PrimordialProcessManifest
  = PrimordialProcessManifest [BootstrapManifestId]
  deriving stock (Eq, Show)

data GenesisError
  = GenesisAdmissionNotPending
  | GenesisAdmissionSystemMismatch
  | GenesisDuplicateHeraldId HeraldId
  | GenesisDuplicateHeraldEpoch HeraldEpoch
  | GenesisLocalMembershipMissing HeraldId HeraldEpoch
  | GenesisCatalogueRoleMissing PredefinedSortRole
  | GenesisCatalogueRoleDuplicate PredefinedSortRole
  | GenesisCatalogueDescriptorRejected PredefinedSortRole CanonicalDescriptorError
  | GenesisCatalogueDescriptorMismatch PredefinedSortRole
  | GenesisPrimordialRoleMissing PredefinedSortRole
  | GenesisPrimordialRoleDuplicate PredefinedSortRole
  | GenesisPrimordialDescriptorRejected PredefinedSortRole CanonicalDescriptorError
  | GenesisPrimordialPublicationSequenceDuplicate
  | GenesisPrimordialSourceNotActive HeraldEpoch
  | GenesisAuthorityNotAtControlIndexZero
  | GenesisConfiguredManifestIdDuplicate BootstrapManifestId
  | GenesisConfiguredProcessEpochDuplicate ProcessEpochId
  | GenesisConfiguredResidenceNotActive BootstrapManifestId HeraldEpoch
  | GenesisConfiguredRootMissing BootstrapManifestId PredefinedSortRole
  | GenesisConfiguredRootDuplicate BootstrapManifestId PredefinedSortRole
  | GenesisConfiguredGlobalIdentityDuplicate GlobalObjectId
  | GenesisConfiguredSequencingObjectUnknown NablaId GlobalObjectId
  | GenesisStoreIncarnationDuplicate StoreIncarnationId
  | GenesisOracleSystemMismatch
  | GenesisOracleMembershipMismatch
  | GenesisOracleCatalogueMismatch
  | GenesisOracleControlIndexNotZero
  deriving stock (Eq, Show)

-- | The closed profile-0.1 authority of a hidden structural bootstrap writer.
-- It is deliberately not an application-process authority or capability.
data GenesisAuthority = GenesisAuthority
  deriving stock (Eq, Ord, Show)

genesisAuthorityEpoch :: GenesisAuthority -> AuthorityEpoch
genesisAuthorityEpoch GenesisAuthority = Identity.genesisAuthorityEpoch

-- | One hidden source used only to publish structural descriptions for roots
-- resident at the indicated Herald.  Checked genesis owns the complete map.
data SystemBootstrapWriter
  = SystemBootstrapWriter
      HeraldEpoch
      StructuralCarrierRole
      NablaId
      GenesisAuthority
  deriving stock (Eq, Ord, Show)

systemBootstrapWriterResidence :: SystemBootstrapWriter -> HeraldEpoch
systemBootstrapWriterResidence (SystemBootstrapWriter residence _ _ _) = residence

systemBootstrapWriterRole :: SystemBootstrapWriter -> StructuralCarrierRole
systemBootstrapWriterRole (SystemBootstrapWriter _ role _ _) = role

systemBootstrapWriterNablaId :: SystemBootstrapWriter -> NablaId
systemBootstrapWriterNablaId (SystemBootstrapWriter _ _ nabla _) = nabla

systemBootstrapWriterAuthority :: SystemBootstrapWriter -> GenesisAuthority
systemBootstrapWriterAuthority (SystemBootstrapWriter _ _ _ authority) = authority

type CheckedCatalogueEntry = PredefinedCatalogueEntry

checkedCatalogueEntryRole :: CheckedCatalogueEntry -> PredefinedSortRole
checkedCatalogueEntryRole = predefinedCatalogueRole

checkedCatalogueEntryDescriptor :: CheckedCatalogueEntry -> CanonicalDescriptor
checkedCatalogueEntryDescriptor = predefinedCatalogueDescriptor

checkedCatalogueEntrySortId :: CheckedCatalogueEntry -> SortId
checkedCatalogueEntrySortId = predefinedCatalogueSortId

data CheckedHeraldGenesis
  = CheckedHeraldGenesis
      SystemId
      HeraldId
      HeraldEpoch
      [HeraldMember]
      [CheckedCatalogueEntry]
      CatalogueDigest
      ConfigurationDigest
      [PrimordialDefinitionReplica]
      [ConfiguredProcessBootstrap]
      (Map BootstrapManifestId ConfiguredProcessBootstrap)
      [SystemBootstrapWriter]
      (Map (HeraldEpoch, StructuralCarrierRole) SystemBootstrapWriter)
      ControlIndex
      (Maybe Admission.HeraldAdmissionRecord)
  deriving stock (Eq, Show)

-- | Reuse the immutable bootstrap for a pending local identity. The original
-- members, topology, digest and hidden writer catalogue remain unchanged.
-- This result authorizes only the separate onboarding interpreter.
checkJoiningHeraldGenesis :: CheckedHeraldGenesis -> Admission.HeraldAdmissionRecord -> Either GenesisError CheckedHeraldGenesis
checkJoiningHeraldGenesis (CheckedHeraldGenesis system _ _ members catalogue catalogueDigest configurationDigest replicas processes processMap writers writerMap index _) admission = do
  let manifest = Admission.admissionRecordManifest admission
  unless (Admission.admissionManifestSystem manifest == system) (Left GenesisAdmissionSystemMismatch)
  case Admission.admissionRecordPhase admission of
    Admission.AdmissionActivated {} -> Left GenesisAdmissionNotPending
    Admission.AdmissionCancelled {} -> Left GenesisAdmissionNotPending
    _ -> pure ()
  pure (CheckedHeraldGenesis system (Admission.admissionManifestHeraldId manifest) (Admission.admissionManifestHeraldEpoch manifest) members catalogue catalogueDigest configurationDigest replicas processes processMap writers writerMap index (Just admission))

checkedStartupAdmission :: CheckedHeraldGenesis -> Maybe Admission.HeraldAdmissionRecord
checkedStartupAdmission (CheckedHeraldGenesis _ _ _ _ _ _ _ _ _ _ _ _ _ admission) = admission

checkedSystemId :: CheckedHeraldGenesis -> SystemId
checkedSystemId (CheckedHeraldGenesis systemId _ _ _ _ _ _ _ _ _ _ _ _ _) = systemId

checkedLocalHeraldId :: CheckedHeraldGenesis -> HeraldId
checkedLocalHeraldId (CheckedHeraldGenesis _ heraldId _ _ _ _ _ _ _ _ _ _ _ _) = heraldId

checkedLocalHeraldEpoch :: CheckedHeraldGenesis -> HeraldEpoch
checkedLocalHeraldEpoch (CheckedHeraldGenesis _ _ heraldEpoch _ _ _ _ _ _ _ _ _ _ _) = heraldEpoch

checkedActiveHeralds :: CheckedHeraldGenesis -> [HeraldMember]
checkedActiveHeralds (CheckedHeraldGenesis _ _ _ members _ _ _ _ _ _ _ _ _ _) = members

checkedCatalogueDigest :: CheckedHeraldGenesis -> CatalogueDigest
checkedCatalogueDigest (CheckedHeraldGenesis _ _ _ _ _ digest _ _ _ _ _ _ _ _) = digest

checkedConfigurationDigest :: CheckedHeraldGenesis -> ConfigurationDigest
checkedConfigurationDigest (CheckedHeraldGenesis _ _ _ _ _ _ digest _ _ _ _ _ _ _) = digest

checkedPrimordialReplicas :: CheckedHeraldGenesis -> [PrimordialDefinitionReplica]
checkedPrimordialReplicas (CheckedHeraldGenesis _ _ _ _ _ _ _ replicas _ _ _ _ _ _) = replicas

checkedConfiguredProcessBootstraps :: CheckedHeraldGenesis -> [ConfiguredProcessBootstrap]
checkedConfiguredProcessBootstraps (CheckedHeraldGenesis _ _ _ _ _ _ _ _ configured _ _ _ _ _) = configured

checkedConfiguredProcessMap ::
  CheckedHeraldGenesis ->
  Map BootstrapManifestId ConfiguredProcessBootstrap
checkedConfiguredProcessMap (CheckedHeraldGenesis _ _ _ _ _ _ _ _ _ configuredMap _ _ _ _) = configuredMap

checkedSystemBootstrapWriters :: CheckedHeraldGenesis -> [SystemBootstrapWriter]
checkedSystemBootstrapWriters (CheckedHeraldGenesis _ _ _ _ _ _ _ _ _ _ writers _ _ _) = writers

checkedSystemBootstrapWriterMap ::
  CheckedHeraldGenesis ->
  Map (HeraldEpoch, StructuralCarrierRole) SystemBootstrapWriter
checkedSystemBootstrapWriterMap (CheckedHeraldGenesis _ _ _ _ _ _ _ _ _ _ _ writers _ _) = writers

lookupSystemBootstrapWriter ::
  HeraldEpoch ->
  StructuralCarrierRole ->
  CheckedHeraldGenesis ->
  Maybe SystemBootstrapWriter
lookupSystemBootstrapWriter residence role =
  Map.lookup (residence, role) . checkedSystemBootstrapWriterMap

checkedLocalSystemBootstrapWriter ::
  StructuralCarrierRole ->
  CheckedHeraldGenesis ->
  SystemBootstrapWriter
checkedLocalSystemBootstrapWriter role genesis =
  case lookupSystemBootstrapWriter (checkedLocalHeraldEpoch genesis) role genesis of
    Just writer -> writer
    Nothing -> error "checked genesis lacks its local structural bootstrap writer"

checkedOracleControlIndex :: CheckedHeraldGenesis -> ControlIndex
checkedOracleControlIndex (CheckedHeraldGenesis _ _ _ _ _ _ _ _ _ _ _ _ control _) = control

checkedActiveHeraldEpochs :: CheckedHeraldGenesis -> [HeraldEpoch]
checkedActiveHeraldEpochs = fmap heraldMemberEpoch . checkedActiveHeralds

-- | The exact index-zero occurrence set fixed by checked system identity and
-- the closed profile catalogue. Future live materialisation supplies the
-- then-current set from Oracle/SortRegistry state instead.
checkedPredefinedOccurrenceSet :: CheckedHeraldGenesis -> PredefinedOccurrenceSet
checkedPredefinedOccurrenceSet = genesisPredefinedOccurrenceSet . checkedSystemId

lookupConfiguredProcessBootstrap ::
  BootstrapManifestId ->
  CheckedHeraldGenesis ->
  Maybe ConfiguredProcessBootstrap
lookupConfiguredProcessBootstrap identifier =
  Map.lookup identifier . checkedConfiguredProcessMap

data BootstrapFault
  = UnknownBootstrapManifest BootstrapManifestId
  | DuplicateBootstrapManifest BootstrapManifestId
  | DuplicateLiveProcessId ProcessId
  | InitialTopologyRejected InitialTopologyError
  deriving stock (Eq, Show)

data CheckedInitialBootstraps
  = CheckedInitialBootstraps
      InitialProjectionDigest
      CheckedInitialTopologyProjection
      [AppliedProcessBootstrap]
  deriving stock (Eq, Show)

checkedInitialBootstraps :: CheckedInitialBootstraps -> [AppliedProcessBootstrap]
checkedInitialBootstraps (CheckedInitialBootstraps _ _ bootstraps) = bootstraps

checkedInitialProjectionDigest :: CheckedInitialBootstraps -> InitialProjectionDigest
checkedInitialProjectionDigest (CheckedInitialBootstraps digest _ _) = digest

checkedInitialTopologyProjection :: CheckedInitialBootstraps -> CheckedInitialTopologyProjection
checkedInitialTopologyProjection (CheckedInitialBootstraps _ topology _) = topology

checkInitialBootstraps ::
  CheckedHeraldGenesis ->
  PrimordialProcessManifest ->
  Either BootstrapFault CheckedInitialBootstraps
checkInitialBootstraps genesis manifest =
  checkInitialBootstrapsWithTopology genesis manifest (InitialTopologyManifest [])

checkInitialBootstrapsWithTopology ::
  CheckedHeraldGenesis ->
  PrimordialProcessManifest ->
  InitialTopologyManifest ->
  Either BootstrapFault CheckedInitialBootstraps
checkInitialBootstrapsWithTopology genesis (PrimordialProcessManifest identifiers) topologyManifest = do
  rejectDuplicate DuplicateBootstrapManifest identifiers
  configured <- traverse resolve identifiers
  rejectDuplicateOn DuplicateLiveProcessId configuredProcessBootstrapProcessId configured
  let applied = fmap materialize configured
  let canonical = sortOn appliedBootstrapManifestId applied
  topology <-
    either
      (Left . InitialTopologyRejected)
      Right
      ( checkInitialTopologyProjection
          (checkedSystemId genesis)
          (checkedActiveHeralds genesis)
          canonical
          topologyManifest
      )
  pure
    ( CheckedInitialBootstraps
        (deriveInitialProjectionDigest canonical topology)
        topology
        canonical
    )
  where
    materialize configured =
      case materializeConfiguredProcessBootstrap
        (controlIndex 0)
        (checkedPredefinedOccurrenceSet genesis)
        configured of
        Right applied -> applied
        Left problem ->
          error
            ( "index-zero configured process materialization invariant: "
                <> show problem
            )
    resolve identifier = case lookupConfiguredProcessBootstrap identifier genesis of
      Nothing -> Left (UnknownBootstrapManifest identifier)
      Just configured -> Right configured

checkHeraldGenesis ::
  DeploymentManifest ->
  Either GenesisError CheckedHeraldGenesis
checkHeraldGenesis manifest = do
  members <- checkMembership manifest
  catalogue <- checkCatalogue (deploymentPredefinedCatalogue manifest)
  let catalogueDigest = profileCatalogueDigest
      bootstrapWriters = deriveSystemBootstrapWriters (deploymentSystemId manifest) members
      bootstrapWriterMap =
        Map.fromList
          [ ((systemBootstrapWriterResidence writer, systemBootstrapWriterRole writer), writer)
          | writer <- bootstrapWriters
          ]
      infrastructureObjects =
        fmap
          (globalObjectIdFromNablaId . systemBootstrapWriterNablaId)
          bootstrapWriters
          <> [ globalObjectIdFromDeltaId
                 (deriveSystemViewDeltaId (deploymentSystemId manifest) residence role)
             | member <- members,
               let residence = heraldMemberEpoch member,
               role <- allPredefinedSortRoles
             ]
  checkOracleGenesis manifest members catalogueDigest
  primordial <- checkPrimordial manifest catalogue
  configured <-
    checkConfiguredProcesses
      (deploymentSystemId manifest)
      members
      (deploymentPrimordialWriterAuthority manifest)
      infrastructureObjects
      (deploymentConfiguredProcesses manifest)
  let configuredMap =
        Map.fromList
          [ (configuredProcessBootstrapManifestId bootstrap, bootstrap)
          | bootstrap <- configured
          ]
      configurationDigest =
        deriveConfigurationDigest
          (deploymentSystemId manifest)
          members
          catalogueDigest
          primordial
  pure
    ( CheckedHeraldGenesis
        (deploymentSystemId manifest)
        (deploymentLocalHeraldId manifest)
        (deploymentLocalHeraldEpoch manifest)
        members
        catalogue
        catalogueDigest
        configurationDigest
        primordial
        configured
        configuredMap
        bootstrapWriters
        bootstrapWriterMap
        (controlIndex 0)
        Nothing
    )

deriveSystemBootstrapWriters ::
  SystemId ->
  [HeraldMember] ->
  [SystemBootstrapWriter]
deriveSystemBootstrapWriters system members =
  [ SystemBootstrapWriter
      residence
      role
      (deriveSystemBootstrapWriterNablaId system residence role)
      GenesisAuthority
  | member <- members,
    let residence = heraldMemberEpoch member,
    role <- allStructuralCarrierRoles
  ]

checkMembership :: DeploymentManifest -> Either GenesisError [HeraldMember]
checkMembership manifest = do
  let members = sort (deploymentActiveHeralds manifest)
  rejectDuplicateOn GenesisDuplicateHeraldId heraldMemberId members
  rejectDuplicateOn GenesisDuplicateHeraldEpoch heraldMemberEpoch members
  let local =
        HeraldMember
          (deploymentLocalHeraldId manifest)
          (deploymentLocalHeraldEpoch manifest)
  unless
    (local `elem` members)
    ( Left
        ( GenesisLocalMembershipMissing
            (deploymentLocalHeraldId manifest)
            (deploymentLocalHeraldEpoch manifest)
        )
    )
  pure members

checkCatalogue ::
  [PredefinedCatalogueManifest] ->
  Either GenesisError [CheckedCatalogueEntry]
checkCatalogue manifests = do
  indexed <- exactRoleMap GenesisCatalogueRoleMissing GenesisCatalogueRoleDuplicate catalogueManifestRole manifests
  traverse (checkRole indexed) allPredefinedSortRoles
  where
    checkRole indexed role = do
      let supplied = indexed Map.! role
          expected = profileCatalogueMap Map.! role
      decoded <-
        either
          (Left . GenesisCatalogueDescriptorRejected role)
          Right
          ( validateCanonicalDescriptorIdentity
              PrimordialDescriptor
              (catalogueManifestSortId supplied)
              (catalogueManifestDescriptorBytes supplied)
          )
      unless
        (decoded == checkedCatalogueEntryDescriptor expected)
        (Left (GenesisCatalogueDescriptorMismatch role))
      pure expected

checkPrimordial ::
  DeploymentManifest ->
  [CheckedCatalogueEntry] ->
  Either GenesisError [PrimordialDefinitionReplica]
checkPrimordial manifest catalogue = do
  let authority = deploymentPrimordialWriterAuthority manifest
      source = primordialWriterSourceHeraldEpoch authority
      activeEpochs = fmap heraldMemberEpoch (deploymentActiveHeralds manifest)
  unless
    (source `elem` activeEpochs)
    (Left (GenesisPrimordialSourceNotActive source))
  unless
    (primordialWriterAuthorityEpoch authority == Identity.genesisAuthorityEpoch)
    (Left GenesisAuthorityNotAtControlIndexZero)
  indexed <-
    exactRoleMap
      GenesisPrimordialRoleMissing
      GenesisPrimordialRoleDuplicate
      primordialManifestRole
      (deploymentPrimordialDefinitions manifest)
  let sequences = fmap primordialManifestPublicationSequence (Map.elems indexed)
  when
    (Set.size (Set.fromList sequences) /= length sequences)
    (Left GenesisPrimordialPublicationSequenceDuplicate)
  let catalogueByRole = Map.fromList [(checkedCatalogueEntryRole entry, entry) | entry <- catalogue]
  traverse (checkRole authority catalogueByRole indexed) allPredefinedSortRoles
  where
    checkRole authority catalogueByRole indexed role = do
      let supplied = indexed Map.! role
          catalogueEntry = catalogueByRole Map.! role
          expectedSort = checkedCatalogueEntrySortId catalogueEntry
      _ <-
        either
          (Left . GenesisPrimordialDescriptorRejected role)
          Right
          ( validateCanonicalDescriptorIdentity
              PrimordialDescriptor
              expectedSort
              (primordialManifestDescriptorBytes supplied)
          )
      pure
        ( primordialDefinitionReplica
            (deploymentSystemId manifest)
            role
            (primordialManifestPublicationSequence supplied)
            authority
        )

checkConfiguredProcesses ::
  SystemId ->
  [HeraldMember] ->
  PrimordialSortWriterAuthority ->
  [GlobalObjectId] ->
  [ConfiguredProcessManifest] ->
  Either GenesisError [ConfiguredProcessBootstrap]
checkConfiguredProcesses systemId members primordialAuthority infrastructureObjects manifests = do
  rejectDuplicateOn GenesisConfiguredManifestIdDuplicate configuredBootstrapManifestId manifests
  rejectDuplicateOn GenesisConfiguredProcessEpochDuplicate configuredProcessEpochId manifests
  rejectDuplicateOn GenesisStoreIncarnationDuplicate id allIncarnations
  rejectDuplicateOn GenesisConfiguredGlobalIdentityDuplicate id allGlobalObjects
  traverse_
    checkSequencingObject
    [ root
    | configured <- manifests,
      root <- configuredProcessRoots configured
    ]
  checked <- traverse checkOne manifests
  pure (sortOn configuredProcessBootstrapManifestId checked)
  where
    activeEpochs = fmap heraldMemberEpoch members
    checkOne configured = do
      unless
        (configuredProcessResidence configured `elem` activeEpochs)
        ( Left
            ( GenesisConfiguredResidenceNotActive
                (configuredBootstrapManifestId configured)
                (configuredProcessResidence configured)
            )
        )
      rootsByRole <-
        exactRoleMap
          (GenesisConfiguredRootMissing (configuredBootstrapManifestId configured))
          (GenesisConfiguredRootDuplicate (configuredBootstrapManifestId configured))
          configuredRootRole
          (configuredProcessRoots configured)
      let roots = fmap (expandConfiguredRoot rootsByRole) allPredefinedSortRoles
      case mkConfiguredProcessBootstrap
        (configuredBootstrapManifestId configured)
        (configuredProcessId configured)
        (configuredProcessEpochId configured)
        (configuredProcessResidence configured)
        roots of
        Nothing -> error "checked configured process failed domain template construction"
        Just checked -> pure checked
    allIncarnations =
      [ deriveSystemViewStoreIncarnationId
          systemId
          (heraldMemberEpoch member)
          role
      | member <- members,
        role <- allPredefinedSortRoles
      ]
        <> [ incarnation
           | configured <- manifests,
             root <- configuredProcessRoots configured,
             let incarnation = configuredReaderStoreIncarnation root
           ]
    allGlobalObjects =
      infrastructureObjects
        <> ( globalObjectIdFromNablaId (primordialWriterNabla primordialAuthority)
               : concatMap configuredObjects manifests
           )
    configuredObjects configured =
      globalObjectIdFromProcessEpochId (configuredProcessEpochId configured)
        : concatMap rootObjects (configuredProcessRoots configured)
          <> configuredEnvironmentWiringObjects (configuredBootstrapManifestId configured)
    rootObjects root =
      [ globalObjectIdFromNablaId (configuredWriterNabla root),
        globalObjectIdFromDeltaId (configuredReaderDelta root)
      ]
    checkSequencingObject root = case configuredWriterSequencing root of
      UnsequencedNabla -> Right ()
      NablaSequencedBy object
        | object `elem` allGlobalObjects -> Right ()
        | otherwise ->
            Left
              ( GenesisConfiguredSequencingObjectUnknown
                  (configuredWriterNabla root)
                  object
              )

expandConfiguredRoot ::
  Map PredefinedSortRole ConfiguredRootManifest ->
  PredefinedSortRole ->
  ConfiguredRootBootstrap
expandConfiguredRoot rootsByRole role =
  let root = rootsByRole Map.! role
   in configuredRootBootstrap
        role
        (configuredWriterNabla root)
        (configuredWriterSequencing root)
        (configuredReaderDelta root)
        (configuredReaderStoreIncarnation root)

checkOracleGenesis ::
  DeploymentManifest ->
  [HeraldMember] ->
  CatalogueDigest ->
  Either GenesisError ()
checkOracleGenesis manifest members catalogueDigest = do
  let oracle = deploymentOracleGenesis manifest
  unless
    (oracleGenesisSystemId oracle == deploymentSystemId manifest)
    (Left GenesisOracleSystemMismatch)
  unless
    (sort (oracleGenesisActiveHeralds oracle) == members)
    (Left GenesisOracleMembershipMismatch)
  unless
    (oracleGenesisCatalogueDigest oracle == catalogueDigest)
    (Left GenesisOracleCatalogueMismatch)
  unless
    (oracleGenesisControlIndex oracle == controlIndex 0)
    (Left GenesisOracleControlIndexNotZero)

profilePredefinedCatalogueManifest :: [PredefinedCatalogueManifest]
profilePredefinedCatalogueManifest =
  [ PredefinedCatalogueManifest
      (checkedCatalogueEntryRole entry)
      (canonicalDescriptorBytes (checkedCatalogueEntryDescriptor entry))
      (checkedCatalogueEntrySortId entry)
  | entry <- profilePredefinedCatalogue
  ]

profileCatalogueMap :: Map PredefinedSortRole CheckedCatalogueEntry
profileCatalogueMap =
  Map.fromList [(checkedCatalogueEntryRole entry, entry) | entry <- profilePredefinedCatalogue]

exactRoleMap ::
  (PredefinedSortRole -> error) ->
  (PredefinedSortRole -> error) ->
  (value -> PredefinedSortRole) ->
  [value] ->
  Either error (Map PredefinedSortRole value)
exactRoleMap missing duplicate roleOf values = do
  rejectDuplicateOn duplicate roleOf values
  let indexed = Map.fromList [(roleOf value, value) | value <- values]
  traverse_ (\role -> unless (Map.member role indexed) (Left (missing role))) allPredefinedSortRoles
  pure indexed

rejectDuplicate ::
  (Ord value) =>
  (value -> error) ->
  [value] ->
  Either error ()
rejectDuplicate constructor = rejectDuplicateOn constructor id

rejectDuplicateOn ::
  (Ord key) =>
  (key -> error) ->
  (value -> key) ->
  [value] ->
  Either error ()
rejectDuplicateOn constructor keyOf = go Set.empty
  where
    go _ [] = Right ()
    go seen (value : remaining)
      | key `Set.member` seen = Left (constructor key)
      | otherwise = go (Set.insert key seen) remaining
      where
        key = keyOf value
