{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Semantic startup vocabulary shared by Herald and Oracle kernels.
--
-- This module contains no Herald deployment checker. It owns the closed
-- predefined catalogue, content-derived startup identities, unapplied
-- configured-process templates, the applied bootstrap shape produced at an
-- Oracle control index, and the symbolic initial-Graph projection shared by all
-- Herald perspectives.
module Eclips.Domain.Startup
  ( -- * Closed predefined catalogue
    PredefinedSortRole (..),
    allPredefinedSortRoles,
    predefinedSortRoleTag,
    PredefinedCatalogueEntry,
    predefinedCatalogueRole,
    predefinedCatalogueDescriptor,
    predefinedCatalogueSortId,
    profilePredefinedCatalogue,
    CatalogueDigest,
    catalogueDigestBytes,
    profileCatalogueDigest,
    ConfigurationDigest,
    configurationDigestBytes,
    InitialProjectionDigest,
    initialProjectionDigestBytes,
    DigestError (..),
    mkCatalogueDigest,
    mkConfigurationDigest,
    mkInitialProjectionDigest,
    HeraldMember (..),
    OracleGenesisManifest (..),
    PrimordialSortWriterAuthority (..),
    PrimordialDefinitionReplica,
    primordialDefinitionReplica,
    primordialReplicaRole,
    primordialReplicaDescriptor,
    primordialReplicaSortId,
    primordialReplicaOccurrenceId,
    primordialReplicaPublicationId,
    primordialReplicaPublication,
    primordialReplicaSourceAuthority,
    deriveConfigurationDigest,
    deriveInitialProjectionDigest,

    -- * Checked initial topology
    InitialTopologyEdgeManifest (..),
    InitialTopologyManifest (..),
    InitialTopologyError (..),
    CheckedInitialTopologyProjection,
    checkInitialTopologyProjection,
    checkedInitialTopologyVertices,
    checkedInitialTopologyEdges,

    -- * Herald-private system views
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
    deriveSystemBootstrapWriterNablaId,
    allStructuralCarrierRoles,

    -- * Current predefined occurrences
    PredefinedOccurrenceSet,
    mkPredefinedOccurrenceSet,
    genesisPredefinedOccurrenceSet,
    predefinedOccurrenceFor,
    predefinedOccurrences,

    -- * Configured process templates
    ConfiguredRootBootstrap,
    configuredRootBootstrap,
    configuredRootBootstrapCatalogueRole,
    configuredRootBootstrapWriterNabla,
    configuredRootBootstrapWriterSequencing,
    configuredRootBootstrapReaderDelta,
    configuredRootBootstrapStoreIncarnation,
    ConfiguredProcessBootstrap,
    mkConfiguredProcessBootstrap,
    configuredProcessBootstrapManifestId,
    configuredProcessBootstrapProcessId,
    configuredProcessBootstrapProcessEpochId,
    configuredProcessBootstrapResidence,
    configuredProcessBootstrapRoots,
    configuredProcessBootstrapEnvironmentHub,
    configuredProcessBootstrapEnvironmentEdges,
    configuredEnvironmentWiringObjects,
    ConfiguredProcessMaterializationError (..),
    materializeConfiguredProcessBootstrap,

    -- * Applied bootstrap shape
    AppliedProcessBootstrap,
    appliedBootstrapManifestId,
    appliedProcessId,
    appliedProcessEpochId,
    appliedProcessObjectId,
    appliedProcessResidence,
    appliedProcessAuthority,
    appliedProcessRoots,
    appliedProcessEnvironmentHub,
    appliedProcessEnvironmentEdges,
    appliedProcessEnvironmentObjects,
    appliedProcessEnvironmentPublications,
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

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Int (Int64)
import Data.List (find, sort, sortOn)
import Data.Serialize (Serialize, encode)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Environment
  ( EnvironmentEdgeDirection (..),
    environmentConnectedShapeCanonicalBytes,
    environmentEdgeOrdinals,
    environmentHubOrdinal,
  )
import Eclips.Domain.Graph
  ( EdgePayload,
    EdgeStrength (Preserve),
    VertexId (DeltaVertex, NablaVertex, NeutralVertex),
    edgeDestination,
    edgePayload,
    edgeSource,
    edgeStrength,
    edgeStrengthSymbol,
    edgeStrengthTranscriptTag,
  )
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
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
    SystemId,
    authorityEpochCanonicalBytes,
    bootstrapManifestIdBytes,
    controlIndex,
    controlIndexWord64,
    deltaIdBytes,
    genesisAuthorityEpoch,
    globalObjectIdBytes,
    globalObjectIdFromDeltaId,
    globalObjectIdFromNablaId,
    globalObjectIdFromProcessEpochId,
    globalUniqueIdFromGlobalObjectId,
    heraldEpochBytes,
    heraldIdBytes,
    mkDeltaId,
    mkGlobalObjectId,
    mkNablaId,
    mkStoreIncarnationId,
    nablaIdBytes,
    nablaSequence,
    nablaSequenceWord64,
    processEpochIdBytes,
    processIdBytes,
    publicationAuthorityEpoch,
    publicationId,
    publicationNabla,
    publicationNablaSequence,
    publicationSourceHeraldEpoch,
    sortDefinitionOccurrenceIdBytes,
    sortIdBytes,
    storeIncarnationIdBytes,
    systemIdBytes,
  )
import Eclips.Domain.Publication (CheckedPublication, mkCheckedPublication)
import Eclips.Domain.Sort.Canonical (CanonicalDescriptor)
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (..),
    structuralCarrierRoleTag,
  )
import Eclips.Domain.Sort.Profile
  ( CatalogueDigest,
    DigestError (..),
    PredefinedCatalogueEntry,
    PredefinedSortRole (..),
    allPredefinedSortRoles,
    catalogueDigestBytes,
    mkCatalogueDigest,
    predefinedCatalogueDescriptor,
    predefinedCatalogueRole,
    predefinedCatalogueSortId,
    predefinedSortRoleTag,
    profileCatalogueDigest,
    profileEntryFor,
    profilePredefinedCatalogue,
    profileSortFor,
    sortDefinitionValue,
  )
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
  )
import Eclips.Domain.Value
  ( LabelOwner (ProcessLabel),
    Value,
    bytesValue,
    enumValue,
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    optionalGlobalUniqueIdValue,
    recordValue,
  )
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import GHC.Generics (Generic)

newtype ConfigurationDigest = ConfigurationDigest ByteString
  deriving stock (Eq, Ord)

configurationDigestBytes :: ConfigurationDigest -> ByteString
configurationDigestBytes (ConfigurationDigest bytes) = bytes

-- | Canonical identity of the complete process-bootstrap projection applied at
-- index zero. Unlike the deployment configuration digest, this binds the
-- separately selected startup set.
newtype InitialProjectionDigest = InitialProjectionDigest ByteString
  deriving stock (Eq, Ord)

instance Show ConfigurationDigest where
  show = renderGroupedHex . configurationDigestBytes

instance Show InitialProjectionDigest where
  show = renderGroupedHex . initialProjectionDigestBytes

initialProjectionDigestBytes :: InitialProjectionDigest -> ByteString
initialProjectionDigestBytes (InitialProjectionDigest bytes) = bytes

data HeraldMember = HeraldMember
  { heraldMemberId :: HeraldId,
    heraldMemberEpoch :: HeraldEpoch
  }
  deriving stock (Eq, Ord, Show)

data OracleGenesisManifest = OracleGenesisManifest
  { oracleGenesisSystemId :: SystemId,
    oracleGenesisActiveHeralds :: [HeraldMember],
    oracleGenesisCatalogueDigest :: CatalogueDigest,
    oracleGenesisControlIndex :: ControlIndex
  }
  deriving stock (Eq, Show)

data PrimordialSortWriterAuthority = PrimordialSortWriterAuthority
  { primordialWriterNabla :: NablaId,
    primordialWriterAuthorityEpoch :: AuthorityEpoch,
    primordialWriterSourceHeraldEpoch :: HeraldEpoch
  }
  deriving stock (Eq, Ord, Show)

data PrimordialDefinitionReplica
  = PrimordialDefinitionReplica
      PredefinedSortRole
      CanonicalDescriptor
      SortId
      SortDefinitionOccurrenceId
      PublicationId
      CheckedPublication
      PrimordialSortWriterAuthority
  deriving stock (Eq, Show)

primordialReplicaRole :: PrimordialDefinitionReplica -> PredefinedSortRole
primordialReplicaRole (PrimordialDefinitionReplica role _ _ _ _ _ _) = role

primordialReplicaDescriptor :: PrimordialDefinitionReplica -> CanonicalDescriptor
primordialReplicaDescriptor (PrimordialDefinitionReplica _ descriptor _ _ _ _ _) = descriptor

primordialReplicaSortId :: PrimordialDefinitionReplica -> SortId
primordialReplicaSortId (PrimordialDefinitionReplica _ _ sortId _ _ _ _) = sortId

primordialReplicaOccurrenceId :: PrimordialDefinitionReplica -> SortDefinitionOccurrenceId
primordialReplicaOccurrenceId (PrimordialDefinitionReplica _ _ _ occurrence _ _ _) = occurrence

primordialReplicaPublicationId :: PrimordialDefinitionReplica -> PublicationId
primordialReplicaPublicationId (PrimordialDefinitionReplica _ _ _ _ provenance _ _) = provenance

primordialReplicaPublication :: PrimordialDefinitionReplica -> CheckedPublication
primordialReplicaPublication (PrimordialDefinitionReplica _ _ _ _ _ publication _) = publication

primordialReplicaSourceAuthority :: PrimordialDefinitionReplica -> PrimordialSortWriterAuthority
primordialReplicaSourceAuthority (PrimordialDefinitionReplica _ _ _ _ _ _ authority) = authority

primordialDefinitionReplica ::
  SystemId ->
  PredefinedSortRole ->
  NablaSequence ->
  PrimordialSortWriterAuthority ->
  PrimordialDefinitionReplica
primordialDefinitionReplica systemId role publicationSequence authority =
  PrimordialDefinitionReplica
    role
    descriptor
    sortId
    (deriveSortDefinitionOccurrenceId systemId sortId Genesis)
    provenance
    publication
    authority
  where
    entry = profileEntryFor role
    descriptor = predefinedCatalogueDescriptor entry
    sortId = predefinedCatalogueSortId entry
    provenance =
      publicationId
        (primordialWriterNabla authority)
        (primordialWriterAuthorityEpoch authority)
        (primordialWriterSourceHeraldEpoch authority)
        publicationSequence
    publication =
      profileInvariant
        "primordial definition publication"
        ( mkCheckedPublication
            (predefinedCatalogueDescriptor (profileEntryFor SortDefinitionRole))
            provenance
            (sortDefinitionValue descriptor)
        )

mkConfigurationDigest :: ByteString -> Either DigestError ConfigurationDigest
mkConfigurationDigest = fmap ConfigurationDigest . checkDigestBytes

mkInitialProjectionDigest :: ByteString -> Either DigestError InitialProjectionDigest
mkInitialProjectionDigest = fmap InitialProjectionDigest . checkDigestBytes

checkDigestBytes :: ByteString -> Either DigestError ByteString
checkDigestBytes bytes
  | ByteString.length bytes == sha256ByteCount = Right bytes
  | otherwise =
      Left
        WrongDigestByteCount
          { expectedDigestByteCount = sha256ByteCount,
            actualDigestByteCount = ByteString.length bytes
          }

sha256ByteCount :: Int
sha256ByteCount = 32

-- | One current definition occurrence for every closed predefined role.
--
-- The constructor is hidden so a template can only be materialised against a
-- complete role set.  Occurrences may change after a definition retires; they
-- therefore belong to the materialisation input rather than the configured
-- deployment template.
data PredefinedOccurrenceSet
  = PredefinedOccurrenceSet
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  deriving stock (Eq, Show)

mkPredefinedOccurrenceSet ::
  [(PredefinedSortRole, SortDefinitionOccurrenceId)] ->
  Maybe PredefinedOccurrenceSet
mkPredefinedOccurrenceSet supplied =
  case sortOn fst supplied of
    [ (SortDefinitionRole, sortDefinitionOccurrence),
      (NeutralVertexRole, neutralVertexOccurrence),
      (EdgeRole, edgeOccurrence),
      (NablaRole, nablaOccurrence),
      (DeltaRole, deltaOccurrence),
      (ProcessEpochRole, processEpochOccurrence)
      ] ->
        Just
          ( PredefinedOccurrenceSet
              sortDefinitionOccurrence
              neutralVertexOccurrence
              edgeOccurrence
              nablaOccurrence
              deltaOccurrence
              processEpochOccurrence
          )
    _ -> Nothing

-- | The fixed index-zero occurrence set derived from system identity and the
-- closed profile catalogue.  Unlike later current sets, this derivation cannot
-- be incomplete and therefore has no admission outcome.
genesisPredefinedOccurrenceSet :: SystemId -> PredefinedOccurrenceSet
genesisPredefinedOccurrenceSet systemId =
  PredefinedOccurrenceSet
    (occurrence SortDefinitionRole)
    (occurrence NeutralVertexRole)
    (occurrence EdgeRole)
    (occurrence NablaRole)
    (occurrence DeltaRole)
    (occurrence ProcessEpochRole)
  where
    occurrence role = deriveSortDefinitionOccurrenceId systemId (profileSortFor role) Genesis

predefinedOccurrenceFor ::
  PredefinedSortRole ->
  PredefinedOccurrenceSet ->
  SortDefinitionOccurrenceId
predefinedOccurrenceFor role occurrences = case (role, occurrences) of
  (SortDefinitionRole, PredefinedOccurrenceSet occurrence _ _ _ _ _) -> occurrence
  (NeutralVertexRole, PredefinedOccurrenceSet _ occurrence _ _ _ _) -> occurrence
  (EdgeRole, PredefinedOccurrenceSet _ _ occurrence _ _ _) -> occurrence
  (NablaRole, PredefinedOccurrenceSet _ _ _ occurrence _ _) -> occurrence
  (DeltaRole, PredefinedOccurrenceSet _ _ _ _ occurrence _) -> occurrence
  (ProcessEpochRole, PredefinedOccurrenceSet _ _ _ _ _ occurrence) -> occurrence

predefinedOccurrences ::
  PredefinedOccurrenceSet ->
  [(PredefinedSortRole, SortDefinitionOccurrenceId)]
predefinedOccurrences occurrences =
  [ (role, predefinedOccurrenceFor role occurrences)
  | role <- allPredefinedSortRoles
  ]

data ConfiguredRootBootstrap
  = ConfiguredRootBootstrap
      PredefinedSortRole
      NablaId
      NablaSequencing
      DeltaId
      StoreIncarnationId
  deriving stock (Eq, Show)

configuredRootBootstrapCatalogueRole :: ConfiguredRootBootstrap -> PredefinedSortRole
configuredRootBootstrapCatalogueRole (ConfiguredRootBootstrap role _ _ _ _) = role

configuredRootBootstrapWriterNabla :: ConfiguredRootBootstrap -> NablaId
configuredRootBootstrapWriterNabla (ConfiguredRootBootstrap _ nabla _ _ _) = nabla

configuredRootBootstrapWriterSequencing ::
  ConfiguredRootBootstrap -> NablaSequencing
configuredRootBootstrapWriterSequencing (ConfiguredRootBootstrap _ _ sequencing _ _) =
  sequencing

configuredRootBootstrapReaderDelta :: ConfiguredRootBootstrap -> DeltaId
configuredRootBootstrapReaderDelta (ConfiguredRootBootstrap _ _ _ delta _) = delta

configuredRootBootstrapStoreIncarnation :: ConfiguredRootBootstrap -> StoreIncarnationId
configuredRootBootstrapStoreIncarnation (ConfiguredRootBootstrap _ _ _ _ incarnation) = incarnation

configuredRootBootstrap ::
  PredefinedSortRole ->
  NablaId ->
  NablaSequencing ->
  DeltaId ->
  StoreIncarnationId ->
  ConfiguredRootBootstrap
configuredRootBootstrap = ConfiguredRootBootstrap

data ConfiguredProcessBootstrap
  = ConfiguredProcessBootstrap
      BootstrapManifestId
      ProcessId
      ProcessEpochId
      HeraldEpoch
      [ConfiguredRootBootstrap]
  deriving stock (Eq, Show)

configuredProcessBootstrapManifestId :: ConfiguredProcessBootstrap -> BootstrapManifestId
configuredProcessBootstrapManifestId (ConfiguredProcessBootstrap manifestId _ _ _ _) = manifestId

configuredProcessBootstrapProcessId :: ConfiguredProcessBootstrap -> ProcessId
configuredProcessBootstrapProcessId (ConfiguredProcessBootstrap _ processId _ _ _) = processId

configuredProcessBootstrapProcessEpochId :: ConfiguredProcessBootstrap -> ProcessEpochId
configuredProcessBootstrapProcessEpochId (ConfiguredProcessBootstrap _ _ processEpoch _ _) = processEpoch

configuredProcessBootstrapResidence :: ConfiguredProcessBootstrap -> HeraldEpoch
configuredProcessBootstrapResidence (ConfiguredProcessBootstrap _ _ _ residence _) = residence

configuredProcessBootstrapRoots :: ConfiguredProcessBootstrap -> [ConfiguredRootBootstrap]
configuredProcessBootstrapRoots (ConfiguredProcessBootstrap _ _ _ _ roots) = roots

-- | The hub and named wiring objects use the same closed manifest positions as
-- ordinary newenv. Their deterministic identities are part of checked genesis;
-- they are ordinary controlled objects after installation.
configuredProcessBootstrapEnvironmentHub :: ConfiguredProcessBootstrap -> GlobalObjectId
configuredProcessBootstrapEnvironmentHub bootstrap =
  configuredEnvironmentObject (configuredProcessBootstrapManifestId bootstrap) environmentHubOrdinal

configuredProcessBootstrapEnvironmentEdges :: ConfiguredProcessBootstrap -> [(GlobalObjectId, EdgePayload)]
configuredProcessBootstrapEnvironmentEdges bootstrap =
  environmentEdges
    (configuredProcessBootstrapManifestId bootstrap)
    (configuredProcessBootstrapEnvironmentHub bootstrap)
    [ ( configuredRootBootstrapCatalogueRole root,
        configuredRootBootstrapWriterNabla root,
        configuredRootBootstrapReaderDelta root
      )
    | root <- configuredProcessBootstrapRoots bootstrap
    ]

configuredEnvironmentObject :: BootstrapManifestId -> Word8 -> GlobalObjectId
configuredEnvironmentObject manifest ordinal =
  profileInvariant "configured environment identity"
    $ mkGlobalObjectId
    $ SHA256.hash
      ( "ECLIPS-CONFIGURED-ENVIRONMENT-OBJECT"
          <> bootstrapManifestIdBytes manifest
          <> environmentConnectedShapeCanonicalBytes
          <> ByteString.singleton ordinal
      )

configuredEnvironmentWiringObjects :: BootstrapManifestId -> [GlobalObjectId]
configuredEnvironmentWiringObjects manifest =
  map (configuredEnvironmentObject manifest) (environmentHubOrdinal : [ordinal | (_, _, ordinal) <- environmentEdgeOrdinals])

environmentEdges :: BootstrapManifestId -> GlobalObjectId -> [(PredefinedSortRole, NablaId, DeltaId)] -> [(GlobalObjectId, EdgePayload)]
environmentEdges manifest hub roots = map edgeFor environmentEdgeOrdinals
  where
    edgeFor (role, direction, ordinal) =
      case [(writer, reader) | (candidate, writer, reader) <- roots, candidate == role] of
        [(writer, reader)] ->
          ( configuredEnvironmentObject manifest ordinal,
            case direction of
              WriterToHub -> edgePayload (NablaVertex writer) (NeutralVertex hub) Preserve
              HubToReader -> edgePayload (NeutralVertex hub) (DeltaVertex reader) Preserve
              ReaderToHub -> edgePayload (DeltaVertex reader) (NeutralVertex hub) Preserve
          )
        _ -> error "checked configured environment lacks a unique root pair"

-- | Construct one opaque checked template only from a complete canonical root
-- set whose globally convertible identities are locally distinct.
mkConfiguredProcessBootstrap ::
  BootstrapManifestId ->
  ProcessId ->
  ProcessEpochId ->
  HeraldEpoch ->
  [ConfiguredRootBootstrap] ->
  Maybe ConfiguredProcessBootstrap
mkConfiguredProcessBootstrap manifestId processId processEpoch residence suppliedRoots
  | fmap configuredRootBootstrapCatalogueRole roots /= allPredefinedSortRoles = Nothing
  | hasDuplicate objectBytes = Nothing
  | hasDuplicate (fmap configuredRootBootstrapStoreIncarnation roots) = Nothing
  | otherwise =
      Just (ConfiguredProcessBootstrap manifestId processId processEpoch residence roots)
  where
    roots = sortOn configuredRootBootstrapCatalogueRole suppliedRoots
    objectBytes =
      processEpochIdBytes processEpoch
        : concatMap rootObjectBytes roots
          <> map globalObjectIdBytes (configuredEnvironmentWiringObjects manifestId)
    rootObjectBytes root =
      [ nablaIdBytes (configuredRootBootstrapWriterNabla root),
        deltaIdBytes (configuredRootBootstrapReaderDelta root)
      ]

-- | Immutable genesis binding shared by Herald and Oracle checkers.
-- Future logical processes and current membership are deliberately absent;
-- the separately checked initial projection binds exact genesis roots/topology.
-- Variable collections are normalized before cereal supplies their structural
-- encoding, so presentation order cannot affect the digest.
deriveConfigurationDigest ::
  SystemId ->
  [HeraldMember] ->
  CatalogueDigest ->
  [PrimordialDefinitionReplica] ->
  ConfigurationDigest
deriveConfigurationDigest system suppliedMembers catalogueDigest suppliedReplicas =
  profileInvariant
    "configuration digest"
    (mkConfigurationDigest (hashTranscript transcript))
  where
    members = sort suppliedMembers
    replicas = sortOn primordialReplicaRole suppliedReplicas
    transcript =
      ConfigurationDigestTranscript
        "ECLIPS-CONFIGURATION"
        (systemIdBytes system)
        (catalogueDigestBytes catalogueDigest)
        (fmap heraldMemberTranscript members)
        (fmap primordialReplicaTranscript replicas)

data ConfigurationDigestTranscript
  = ConfigurationDigestTranscript
      ByteString
      ByteString
      ByteString
      [HeraldMemberTranscript]
      [PrimordialReplicaTranscript]
  deriving stock (Generic)
  deriving anyclass (Serialize)

data HeraldMemberTranscript = HeraldMemberTranscript ByteString ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data PrimordialReplicaTranscript
  = PrimordialReplicaTranscript
      Word8
      ByteString
      PublicationTranscript
  deriving stock (Generic)
  deriving anyclass (Serialize)

data PublicationTranscript
  = PublicationTranscript
      ByteString
      ByteString
      Word64
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

heraldMemberTranscript :: HeraldMember -> HeraldMemberTranscript
heraldMemberTranscript member =
  HeraldMemberTranscript
    (heraldIdBytes (heraldMemberId member))
    (heraldEpochBytes (heraldMemberEpoch member))

primordialReplicaTranscript :: PrimordialDefinitionReplica -> PrimordialReplicaTranscript
primordialReplicaTranscript replica =
  PrimordialReplicaTranscript
    (predefinedSortRoleTag (primordialReplicaRole replica))
    (sortDefinitionOccurrenceIdBytes (primordialReplicaOccurrenceId replica))
    (publicationTranscript (primordialReplicaPublicationId replica))

publicationTranscript :: PublicationId -> PublicationTranscript
publicationTranscript identifier =
  PublicationTranscript
    (nablaIdBytes (publicationNabla identifier))
    (authorityEpochCanonicalBytes (publicationAuthorityEpoch identifier))
    (nablaSequenceWord64 (publicationNablaSequence identifier))
    (heraldEpochBytes (publicationSourceHeraldEpoch identifier))

data NablaSequencingTranscript
  = UnsequencedNablaTranscript
  | NablaSequencedByTranscript ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

nablaSequencingTranscript :: NablaSequencing -> NablaSequencingTranscript
nablaSequencingTranscript sequencing = case sequencing of
  UnsequencedNabla -> UnsequencedNablaTranscript
  NablaSequencedBy object ->
    NablaSequencedByTranscript (globalObjectIdBytes object)

-- | One startup-only universal preserving edge.  The single role structurally
-- requires source and destination sort agreement, while the constructors keep
-- raw graph and placement identities out of the trusted fixture vocabulary.
data InitialTopologyEdgeManifest
  = InitialTopologyProcessEdge
      BootstrapManifestId
      BootstrapManifestId
      PredefinedSortRole
  | InitialTopologySystemViewEdge
      BootstrapManifestId
      HeraldEpoch
      PredefinedSortRole
  deriving stock (Eq, Ord, Show)

-- | Symbolic additions to the common topology derived from checked startup.
-- Presentation order is deliberately not meaningful.
newtype InitialTopologyManifest
  = InitialTopologyManifest [InitialTopologyEdgeManifest]
  deriving stock (Eq, Show)

data InitialTopologyError
  = InitialTopologyDuplicateManifestEdge InitialTopologyEdgeManifest
  | InitialTopologySourceBootstrapNotSelected BootstrapManifestId
  | InitialTopologyDestinationBootstrapNotSelected BootstrapManifestId
  | InitialTopologyDestinationHeraldNotActive HeraldEpoch
  | InitialTopologyBootstrapResidenceNotActive BootstrapManifestId HeraldEpoch
  | InitialTopologyWriterRoleMissing BootstrapManifestId PredefinedSortRole
  | InitialTopologyReaderRoleMissing BootstrapManifestId PredefinedSortRole
  | InitialTopologyVertexCollision VertexId
  | InitialTopologyEdgeAlreadyPresent InitialTopologyEdgeManifest
  deriving stock (Eq, Show)

-- | Canonical, fully resolved Graph projection shared by every Herald
-- perspective.  Its constructor remains hidden so the projection digest can
-- only bind topology admitted from checked symbolic facts.
data CheckedInitialTopologyProjection
  = CheckedInitialTopologyProjection [VertexId] [EdgePayload]
  deriving stock (Eq, Show)

checkedInitialTopologyVertices :: CheckedInitialTopologyProjection -> [VertexId]
checkedInitialTopologyVertices (CheckedInitialTopologyProjection vertices _) = vertices

checkedInitialTopologyEdges :: CheckedInitialTopologyProjection -> [EdgePayload]
checkedInitialTopologyEdges (CheckedInitialTopologyProjection _ edges) = edges

-- | Resolve the complete common startup Graph.  The base projection contains
-- every selected root pair, six private system views and five isolated hidden
-- bootstrap writers per active Herald, and a same-role edge from each resident
-- configured writer to its Herald's private view.
-- Manifest edges can only add a selected writer to a selected reader or an
-- active Herald's derived private view; all edges preserve strength.
checkInitialTopologyProjection ::
  SystemId ->
  [HeraldMember] ->
  [AppliedProcessBootstrap] ->
  InitialTopologyManifest ->
  Either InitialTopologyError CheckedInitialTopologyProjection
checkInitialTopologyProjection system suppliedMembers suppliedBootstraps (InitialTopologyManifest suppliedEdges) = do
  maybe (Right ()) (Left . InitialTopologyDuplicateManifestEdge) (firstDuplicate manifestEdges)
  mapM_ checkResidence bootstraps
  maybe (Right ()) (Left . InitialTopologyVertexCollision) (firstDuplicate allVertices)
  resolvedManifestEdges <- traverse resolveManifestEdge manifestEdges
  mapM_ rejectExistingManifestEdge (zip manifestEdges resolvedManifestEdges)
  pure
    ( CheckedInitialTopologyProjection
        (Set.toAscList (Set.fromList allVertices))
        (Set.toAscList (Set.fromList (baseEdges <> resolvedManifestEdges)))
    )
  where
    members = sort suppliedMembers
    activeEpochs = Set.fromList (fmap heraldMemberEpoch members)
    bootstraps = sortOn appliedBootstrapManifestId suppliedBootstraps
    manifestEdges = sort suppliedEdges
    rootVertices =
      [ rootVertex root
      | bootstrap <- bootstraps,
        root <- appliedProcessRoots bootstrap
      ]
    systemViewVertices =
      [ DeltaVertex (deriveSystemViewDeltaId system herald role)
      | herald <- Set.toAscList activeEpochs,
        role <- allPredefinedSortRoles
      ]
    systemBootstrapWriterVertices =
      [ NablaVertex (deriveSystemBootstrapWriterNablaId system herald role)
      | herald <- Set.toAscList activeEpochs,
        role <- allStructuralCarrierRoles
      ]
    allVertices = rootVertices <> map (NeutralVertex . appliedProcessEnvironmentHub) bootstraps <> systemViewVertices <> systemBootstrapWriterVertices
    baseEdges = concatMap bootstrapBaseEdges bootstraps
    checkResidence bootstrap
      | Set.member (appliedProcessResidence bootstrap) activeEpochs = Right ()
      | otherwise =
          Left
            ( InitialTopologyBootstrapResidenceNotActive
                (appliedBootstrapManifestId bootstrap)
                (appliedProcessResidence bootstrap)
            )
    bootstrapBaseEdges bootstrap =
      map snd (appliedProcessEnvironmentEdges bootstrap) <> residentSystemViewEdges bootstrap
    residentSystemViewEdges bootstrap =
      [ edgePayload
          (NablaVertex nabla)
          ( DeltaVertex
              (deriveSystemViewDeltaId system (appliedProcessResidence bootstrap) (appliedRootCatalogueRole root))
          )
          Preserve
      | root <- appliedProcessRoots bootstrap,
        WriterRoot nabla _ <- [appliedRootRole root]
      ]
    resolveManifestEdge manifestEdge = case manifestEdge of
      InitialTopologyProcessEdge source destination role -> do
        sourceBootstrap <- resolveSourceBootstrap source
        destinationBootstrap <- resolveDestinationBootstrap destination
        nabla <- resolveWriter source role sourceBootstrap
        delta <- resolveReader destination role destinationBootstrap
        pure (edgePayload (NablaVertex nabla) (DeltaVertex delta) Preserve)
      InitialTopologySystemViewEdge source destinationHerald role -> do
        sourceBootstrap <- resolveSourceBootstrap source
        nabla <- resolveWriter source role sourceBootstrap
        if Set.member destinationHerald activeEpochs
          then
            pure
              ( edgePayload
                  (NablaVertex nabla)
                  (DeltaVertex (deriveSystemViewDeltaId system destinationHerald role))
                  Preserve
              )
          else Left (InitialTopologyDestinationHeraldNotActive destinationHerald)
    resolveSourceBootstrap identifier =
      maybe
        (Left (InitialTopologySourceBootstrapNotSelected identifier))
        Right
        (find ((== identifier) . appliedBootstrapManifestId) bootstraps)
    resolveDestinationBootstrap identifier =
      maybe
        (Left (InitialTopologyDestinationBootstrapNotSelected identifier))
        Right
        (find ((== identifier) . appliedBootstrapManifestId) bootstraps)
    resolveWriter identifier role bootstrap =
      maybe
        (Left (InitialTopologyWriterRoleMissing identifier role))
        Right
        ( findMap
            ( \root -> case appliedRootRole root of
                WriterRoot nabla _
                  | appliedRootCatalogueRole root == role -> Just nabla
                _ -> Nothing
            )
            (appliedProcessRoots bootstrap)
        )
    resolveReader identifier role bootstrap =
      maybe
        (Left (InitialTopologyReaderRoleMissing identifier role))
        Right
        ( findMap
            ( \root -> case appliedRootRole root of
                ReaderRoot delta
                  | appliedRootCatalogueRole root == role -> Just delta
                _ -> Nothing
            )
            (appliedProcessRoots bootstrap)
        )
    rejectExistingManifestEdge (manifestEdge, edge)
      | InitialTopologyProcessEdge source destination _ <- manifestEdge,
        source == destination =
          Left (InitialTopologyEdgeAlreadyPresent manifestEdge)
      | edge `elem` baseEdges = Left (InitialTopologyEdgeAlreadyPresent manifestEdge)
      | otherwise = Right ()

rootVertex :: AppliedRoot -> VertexId
rootVertex root = case appliedRootRole root of
  WriterRoot nabla _ -> NablaVertex nabla
  ReaderRoot delta -> DeltaVertex delta

firstDuplicate :: (Ord value) => [value] -> Maybe value
firstDuplicate = duplicateInOrder . sort
  where
    duplicateInOrder (first : second : remaining)
      | first == second = Just first
      | otherwise = duplicateInOrder (second : remaining)
    duplicateInOrder _ = Nothing

findMap :: (input -> Maybe output) -> [input] -> Maybe output
findMap _ [] = Nothing
findMap project (input : remaining) = case project input of
  Just output -> Just output
  Nothing -> findMap project remaining

-- | Bind the full applied index-zero projection and its normalized checked
-- Graph, not merely manifest references. Collection presentation is normalized
-- before encoding.
deriveInitialProjectionDigest ::
  [AppliedProcessBootstrap] ->
  CheckedInitialTopologyProjection ->
  InitialProjectionDigest
deriveInitialProjectionDigest suppliedBootstraps topology =
  profileInvariant
    "initial projection digest"
    (mkInitialProjectionDigest (hashTranscript transcript))
  where
    bootstraps =
      sortOn encode (fmap appliedProcessBootstrapTranscript suppliedBootstraps)
    transcript =
      InitialProjectionDigestTranscript
        "ECLIPS-INITIAL-PROJECTION"
        bootstraps
        (fmap initialVertexTranscript (checkedInitialTopologyVertices topology))
        (fmap initialEdgeTranscript (checkedInitialTopologyEdges topology))

data InitialProjectionDigestTranscript
  = InitialProjectionDigestTranscript
      ByteString
      [AppliedProcessBootstrapTranscript]
      [InitialVertexTranscript]
      [InitialEdgeTranscript]
  deriving stock (Generic)
  deriving anyclass (Serialize)

data InitialVertexTranscript
  = InitialNablaVertexTranscript ByteString
  | InitialDeltaVertexTranscript ByteString
  | InitialNeutralVertexTranscript ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data InitialEdgeTranscript
  = InitialEdgeTranscript
      InitialVertexTranscript
      InitialVertexTranscript
      Int64
  deriving stock (Generic)
  deriving anyclass (Serialize)

initialVertexTranscript :: VertexId -> InitialVertexTranscript
initialVertexTranscript vertex = case vertex of
  NablaVertex nabla -> InitialNablaVertexTranscript (nablaIdBytes nabla)
  DeltaVertex delta -> InitialDeltaVertexTranscript (deltaIdBytes delta)
  NeutralVertex neutral -> InitialNeutralVertexTranscript (globalObjectIdBytes neutral)

initialEdgeTranscript :: EdgePayload -> InitialEdgeTranscript
initialEdgeTranscript edge =
  InitialEdgeTranscript
    (initialVertexTranscript (edgeSource edge))
    (initialVertexTranscript (edgeDestination edge))
    (edgeStrengthTranscriptTag (edgeStrength edge))

data AppliedProcessBootstrapTranscript
  = AppliedProcessBootstrapTranscript
      ByteString
      ByteString
      ByteString
      ByteString
      ByteString
      [AppliedRootTranscript]
      ByteString
      [(ByteString, InitialEdgeTranscript)]
  deriving stock (Generic)
  deriving anyclass (Serialize)

data AppliedRootTranscript
  = AppliedRootTranscript
      Word8
      ByteString
      ByteString
      Word64
      AppliedRootRoleTranscript
      ByteString
      AppliedRootPlacementTranscript
  deriving stock (Generic)
  deriving anyclass (Serialize)

data AppliedRootRoleTranscript
  = WriterRootTranscript ByteString NablaSequencingTranscript
  | ReaderRootTranscript ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data AppliedRootPlacementTranscript
  = UnplacedRootTranscript
  | PlacedRootTranscript ByteString ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

appliedProcessBootstrapTranscript :: AppliedProcessBootstrap -> AppliedProcessBootstrapTranscript
appliedProcessBootstrapTranscript bootstrap =
  AppliedProcessBootstrapTranscript
    (bootstrapManifestIdBytes (appliedBootstrapManifestId bootstrap))
    (processIdBytes (appliedProcessId bootstrap))
    (processEpochIdBytes (appliedProcessEpochId bootstrap))
    (heraldEpochBytes (appliedProcessResidence bootstrap))
    (authorityEpochCanonicalBytes (appliedProcessAuthority bootstrap))
    (fmap appliedRootTranscript (appliedProcessRoots bootstrap))
    (globalObjectIdBytes (appliedProcessEnvironmentHub bootstrap))
    [(globalObjectIdBytes object, initialEdgeTranscript edge) | (object, edge) <- appliedProcessEnvironmentEdges bootstrap]

appliedRootTranscript :: AppliedRoot -> AppliedRootTranscript
appliedRootTranscript root =
  AppliedRootTranscript
    (predefinedSortRoleTag (appliedRootCatalogueRole root))
    (sortIdBytes (appliedRootSortId root))
    (sortDefinitionOccurrenceIdBytes (appliedRootOccurrenceId root))
    (controlIndexWord64 (appliedRootControlPrerequisite root))
    (appliedRootRoleTranscript (appliedRootRole root))
    (authorityEpochCanonicalBytes (appliedRootAuthority root))
    (appliedRootPlacementTranscript (appliedRootPlacement root))

appliedRootRoleTranscript :: AppliedRootRole -> AppliedRootRoleTranscript
appliedRootRoleTranscript role = case role of
  WriterRoot nabla sequencing ->
    WriterRootTranscript
      (nablaIdBytes nabla)
      (nablaSequencingTranscript sequencing)
  ReaderRoot delta -> ReaderRootTranscript (deltaIdBytes delta)

appliedRootPlacementTranscript ::
  Maybe (HeraldEpoch, StoreIncarnationId) ->
  AppliedRootPlacementTranscript
appliedRootPlacementTranscript placement = case placement of
  Nothing -> UnplacedRootTranscript
  Just (herald, incarnation) ->
    PlacedRootTranscript
      (heraldEpochBytes herald)
      (storeIncarnationIdBytes incarnation)

-- | Domain-separated deterministic identity for one predefined-carrier system
-- view at one Herald. Profile 0.1 relies on SHA-256 collision resistance.
deriveSystemViewDeltaId ::
  SystemId ->
  HeraldEpoch ->
  PredefinedSortRole ->
  DeltaId
deriveSystemViewDeltaId system herald role =
  profileInvariant
    "system-view delta identity"
    ( mkDeltaId
        ( hashTranscript
            (systemViewIdentityTranscript "ECLIPS-SYSTEM-VIEW-DELTA" system herald role)
        )
    )

-- | Separately domain-separated deterministic startup incarnation of that
-- private store. Its nominal type remains unrelated to 'DeltaId'.
deriveSystemViewStoreIncarnationId ::
  SystemId ->
  HeraldEpoch ->
  PredefinedSortRole ->
  StoreIncarnationId
deriveSystemViewStoreIncarnationId system herald role =
  profileInvariant
    "system-view store incarnation"
    ( mkStoreIncarnationId
        ( hashTranscript
            ( systemViewIdentityTranscript
                "ECLIPS-SYSTEM-VIEW-STORE-INCARNATION"
                system
                herald
                role
            )
        )
    )

-- | Domain-separated deterministic identity for one resident hidden source of
-- configured structural-root stages.  The explicit carrier tag is stable even
-- if constructor declaration order changes.
deriveSystemBootstrapWriterNablaId ::
  SystemId ->
  HeraldEpoch ->
  StructuralCarrierRole ->
  NablaId
deriveSystemBootstrapWriterNablaId system herald role =
  profileInvariant
    "system bootstrap-writer identity"
    ( mkNablaId
        ( hashTranscript
            ( SystemBootstrapWriterIdentityTranscript
                "ECLIPS-SYSTEM-BOOTSTRAP-WRITER"
                (systemIdBytes system)
                (heraldEpochBytes herald)
                (structuralCarrierRoleTag role)
            )
        )
    )

allStructuralCarrierRoles :: [StructuralCarrierRole]
allStructuralCarrierRoles =
  [ NeutralVertexCarrier,
    EdgeCarrier,
    NablaCarrier,
    DeltaCarrier,
    ProcessEpochCarrier
  ]

data SystemBootstrapWriterIdentityTranscript
  = SystemBootstrapWriterIdentityTranscript
      ByteString
      ByteString
      ByteString
      Word8
  deriving stock (Generic)
  deriving anyclass (Serialize)

data SystemViewIdentityTranscript
  = SystemViewIdentityTranscript
      ByteString
      ByteString
      ByteString
      Word8
  deriving stock (Generic)
  deriving anyclass (Serialize)

systemViewIdentityTranscript ::
  ByteString ->
  SystemId ->
  HeraldEpoch ->
  PredefinedSortRole ->
  SystemViewIdentityTranscript
systemViewIdentityTranscript domainTag system herald role =
  SystemViewIdentityTranscript
    domainTag
    (systemIdBytes system)
    (heraldEpochBytes herald)
    (predefinedSortRoleTag role)

data AppliedRootRole
  = WriterRoot NablaId NablaSequencing
  | ReaderRoot DeltaId
  deriving stock (Eq, Ord, Show)

data AppliedRoot
  = AppliedRoot
      PredefinedSortRole
      SortId
      SortDefinitionOccurrenceId
      ControlIndex
      AppliedRootRole
      AuthorityEpoch
      (Maybe (HeraldEpoch, StoreIncarnationId))
  deriving stock (Eq, Show)

appliedRootCatalogueRole :: AppliedRoot -> PredefinedSortRole
appliedRootCatalogueRole (AppliedRoot role _ _ _ _ _ _) = role

appliedRootSortId :: AppliedRoot -> SortId
appliedRootSortId (AppliedRoot _ sortId _ _ _ _ _) = sortId

appliedRootOccurrenceId :: AppliedRoot -> SortDefinitionOccurrenceId
appliedRootOccurrenceId (AppliedRoot _ _ occurrence _ _ _ _) = occurrence

appliedRootControlPrerequisite :: AppliedRoot -> ControlIndex
appliedRootControlPrerequisite (AppliedRoot _ _ _ prerequisite _ _ _) = prerequisite

appliedRootRole :: AppliedRoot -> AppliedRootRole
appliedRootRole (AppliedRoot _ _ _ _ role _ _) = role

appliedRootAuthority :: AppliedRoot -> AuthorityEpoch
appliedRootAuthority (AppliedRoot _ _ _ _ _ authority _) = authority

appliedRootPlacement :: AppliedRoot -> Maybe (HeraldEpoch, StoreIncarnationId)
appliedRootPlacement (AppliedRoot _ _ _ _ _ _ placement) = placement

appliedRootObjectId :: AppliedRoot -> GlobalObjectId
appliedRootObjectId root = case appliedRootRole root of
  WriterRoot identifier _ -> globalObjectIdFromNablaId identifier
  ReaderRoot identifier -> globalObjectIdFromDeltaId identifier

data AppliedProcessBootstrap
  = AppliedProcessBootstrap
      BootstrapManifestId
      ProcessId
      ProcessEpochId
      HeraldEpoch
      AuthorityEpoch
      [AppliedRoot]
  deriving stock (Eq, Show)

appliedBootstrapManifestId :: AppliedProcessBootstrap -> BootstrapManifestId
appliedBootstrapManifestId (AppliedProcessBootstrap manifestId _ _ _ _ _) = manifestId

appliedProcessId :: AppliedProcessBootstrap -> ProcessId
appliedProcessId (AppliedProcessBootstrap _ processId _ _ _ _) = processId

appliedProcessEpochId :: AppliedProcessBootstrap -> ProcessEpochId
appliedProcessEpochId (AppliedProcessBootstrap _ _ processEpoch _ _ _) = processEpoch

appliedProcessResidence :: AppliedProcessBootstrap -> HeraldEpoch
appliedProcessResidence (AppliedProcessBootstrap _ _ _ residence _ _) = residence

appliedProcessAuthority :: AppliedProcessBootstrap -> AuthorityEpoch
appliedProcessAuthority (AppliedProcessBootstrap _ _ _ _ authority _) = authority

appliedProcessRoots :: AppliedProcessBootstrap -> [AppliedRoot]
appliedProcessRoots (AppliedProcessBootstrap _ _ _ _ _ roots) = roots

appliedProcessEnvironmentHub :: AppliedProcessBootstrap -> GlobalObjectId
appliedProcessEnvironmentHub bootstrap = configuredEnvironmentObject (appliedBootstrapManifestId bootstrap) environmentHubOrdinal

appliedProcessEnvironmentEdges :: AppliedProcessBootstrap -> [(GlobalObjectId, EdgePayload)]
appliedProcessEnvironmentEdges bootstrap =
  environmentEdges
    (appliedBootstrapManifestId bootstrap)
    (appliedProcessEnvironmentHub bootstrap)
    [ (appliedRootCatalogueRole writer, nabla, delta)
    | writer <- appliedProcessRoots bootstrap,
      WriterRoot nabla _ <- [appliedRootRole writer],
      reader <- appliedProcessRoots bootstrap,
      ReaderRoot delta <- [appliedRootRole reader],
      appliedRootCatalogueRole reader == appliedRootCatalogueRole writer
    ]

appliedProcessEnvironmentObjects :: AppliedProcessBootstrap -> [GlobalObjectId]
appliedProcessEnvironmentObjects bootstrap =
  map appliedRootObjectId (appliedProcessRoots bootstrap)
    <> [appliedProcessEnvironmentHub bootstrap]
    <> map fst (appliedProcessEnvironmentEdges bootstrap)

-- | Checked ordinary descriptions supplied by genesis. The isolated private
-- bootstrap writers provide publication identities only: they grant no live
-- application writer. Canonical positions across the entire selected startup
-- set keep every source sequence distinct without consuming an application
-- writer's sequence. Each environment is seeded only into its own stores.
appliedProcessEnvironmentPublications :: SystemId -> [AppliedProcessBootstrap] -> AppliedProcessBootstrap -> [(StructuralCarrierRole, SortDefinitionOccurrenceId, CheckedPublication)]
appliedProcessEnvironmentPublications system bootstraps selected =
  [ (carrier, occurrence bootstrap role, checked bootstrap ordinal role value)
  | (ordinal, (bootstrap, carrier, role, value)) <- zip [1 ..] descriptions,
    appliedBootstrapManifestId bootstrap == appliedBootstrapManifestId selected
  ]
  where
    descriptions =
      [ (bootstrap, carrier, role, value)
      | bootstrap <- sortOn appliedBootstrapManifestId bootstraps,
        (carrier, role, value) <- environmentDescriptions bootstrap
      ]
    checked bootstrap ordinal role value =
      let descriptor = predefinedCatalogueDescriptor (profileEntryFor role)
          carrier = case role of
            NablaRole -> NablaCarrier
            DeltaRole -> DeltaCarrier
            NeutralVertexRole -> NeutralVertexCarrier
            EdgeRole -> EdgeCarrier
            _ -> error "environment description used a nonstructural sort"
          identifier =
            publicationId
              (deriveSystemBootstrapWriterNablaId system (appliedProcessResidence bootstrap) carrier)
              genesisAuthorityEpoch
              (appliedProcessResidence bootstrap)
              (nablaSequence ordinal)
       in profileInvariant "configured environment publication" (mkCheckedPublication descriptor identifier value)
    occurrence bootstrap role =
      case [appliedRootOccurrenceId root | root <- appliedProcessRoots bootstrap, appliedRootCatalogueRole root == role] of
        first : _ -> first
        [] -> error "checked configured environment lacks its carrier occurrence"

environmentDescriptions :: AppliedProcessBootstrap -> [(StructuralCarrierRole, PredefinedSortRole, Value)]
environmentDescriptions bootstrap =
  map describeRoot (appliedProcessRoots bootstrap)
    <> [(NeutralVertexCarrier, NeutralVertexRole, description (appliedProcessEnvironmentHub bootstrap) [])]
    <> [(EdgeCarrier, EdgeRole, describeEdge object edge) | (object, edge) <- appliedProcessEnvironmentEdges bootstrap]
  where
    description object fields =
      profileInvariant "configured environment description"
        $ recordValue
        $ [ (field "object_id", globalUniqueIdValue (globalUniqueIdFromGlobalObjectId object)),
            (field "label", labelValue (ProcessLabel (appliedProcessEpochId bootstrap), 0))
          ]
          <> fields
    field = profileInvariant "configured environment field" . mkFieldName
    describeRoot root = case appliedRootRole root of
      WriterRoot _ sequencing ->
        ( NablaCarrier,
          NablaRole,
          description
            (appliedRootObjectId root)
            [ (field "sort_id", bytesValue (sortIdBytes (appliedRootSortId root))),
              (field "sequencing_object", optionalGlobalUniqueIdValue (case sequencing of UnsequencedNabla -> Nothing; NablaSequencedBy object -> Just (globalUniqueIdFromGlobalObjectId object)))
            ]
        )
      ReaderRoot _ ->
        (DeltaCarrier, DeltaRole, description (appliedRootObjectId root) [(field "sort_id", bytesValue (sortIdBytes (appliedRootSortId root)))])
    describeEdge object edge =
      description
        object
        [ (field "source_vertex", globalUniqueIdValue (globalUniqueIdFromGlobalObjectId (vertexObject (edgeSource edge)))),
          (field "destination_vertex", globalUniqueIdValue (globalUniqueIdFromGlobalObjectId (vertexObject (edgeDestination edge)))),
          (field "strength", enumValue (edgeStrengthSymbol (edgeStrength edge)))
        ]
    vertexObject = \case
      NablaVertex nabla -> globalObjectIdFromNablaId nabla
      DeltaVertex delta -> globalObjectIdFromDeltaId delta
      NeutralVertex object -> object

appliedProcessObjectId :: AppliedProcessBootstrap -> GlobalObjectId
appliedProcessObjectId = globalObjectIdFromProcessEpochId . appliedProcessEpochId

data ConfiguredProcessMaterializationError
  = ConfiguredProcessMaterializationRequiresGenesis ControlIndex
  deriving stock (Eq, Show)

-- | Materialize an index-zero configured process selected into checked genesis.
--
-- Dynamic @StartProcessEpoch@ does not call this helper: it projects only the
-- started process and sends each retained root through structural publication,
-- deriving any nabla authority only after a covering topology cut is installed.
materializeConfiguredProcessBootstrap ::
  ControlIndex ->
  PredefinedOccurrenceSet ->
  ConfiguredProcessBootstrap ->
  Either ConfiguredProcessMaterializationError AppliedProcessBootstrap
materializeConfiguredProcessBootstrap appliedAt _ _
  | appliedAt /= controlIndex 0 =
      Left (ConfiguredProcessMaterializationRequiresGenesis appliedAt)
materializeConfiguredProcessBootstrap appliedAt occurrences configured =
  Right
    ( AppliedProcessBootstrap
        (configuredProcessBootstrapManifestId configured)
        (configuredProcessBootstrapProcessId configured)
        (configuredProcessBootstrapProcessEpochId configured)
        (configuredProcessBootstrapResidence configured)
        genesisAuthorityEpoch
        (concatMap materializeRoot (configuredProcessBootstrapRoots configured))
    )
  where
    materializeRoot root =
      let role = configuredRootBootstrapCatalogueRole root
          sortId = profileSortFor role
          occurrence = predefinedOccurrenceFor role occurrences
       in [ AppliedRoot
              role
              sortId
              occurrence
              appliedAt
              ( WriterRoot
                  (configuredRootBootstrapWriterNabla root)
                  (configuredRootBootstrapWriterSequencing root)
              )
              genesisAuthorityEpoch
              Nothing,
            AppliedRoot
              role
              sortId
              occurrence
              appliedAt
              (ReaderRoot (configuredRootBootstrapReaderDelta root))
              genesisAuthorityEpoch
              ( Just
                  ( configuredProcessBootstrapResidence configured,
                    configuredRootBootstrapStoreIncarnation root
                  )
              )
          ]

hasDuplicate :: (Ord value) => [value] -> Bool
hasDuplicate values = go (sortOn id values)
  where
    go (first : second : remaining) = first == second || go (second : remaining)
    go _ = False

hashTranscript :: (Serialize transcript) => transcript -> ByteString
hashTranscript = SHA256.hash . encode

profileInvariant :: (Show problem) => String -> Either problem value -> value
profileInvariant context =
  either
    (error . (("invalid closed profile " <> context <> ": ") <>) . show)
    id
