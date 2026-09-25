{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Checked immutable Oracle genesis and its Oracle-owned Raft binding.
module Eclips.Oracle.Genesis
  ( RaftVoterBinding,
    raftVoterBinding,
    raftVoterBindingNode,
    raftVoterBindingHeraldEpoch,
    OracleGenesis,
    oracleGenesis,
    OracleGenesisFault (..),
    CheckedOracleGenesis,
    checkOracleGenesis,
    checkedOracleSystemId,
    checkedOracleActiveHeralds,
    checkedOracleCatalogueDigest,
    checkedOraclePredefinedDescriptors,
    checkedOracleAppliedBootstraps,
    checkedOracleConfigurationDigest,
    checkedOracleInitialTopologyProjection,
    checkedOracleInitialProjectionDigest,
    checkedOraclePredefinedOccurrences,
    checkedOracleRaftVoterBindings,
    checkedOracleRaftNativeConfiguration,
    checkedOracleRaftConfigurationDigest,
    checkedOracleControlIndex,
    deriveRaftConfigurationDigest,
  )
where

import Data.ByteString (ByteString)
import Data.List (sort, sortOn)
import Data.Serialize (Serialize)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Graph (VertexId (..))
import Eclips.Domain.Identity
  ( AuthorityEpochView (GenesisAuthorityEpochView),
    BootstrapManifestId,
    ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    HeraldId,
    ProcessEpochId,
    ProcessId,
    SortId,
    StoreIncarnationId,
    SystemId,
    authorityEpochView,
    controlIndex,
    controlIndexWord64,
    globalObjectIdFromDeltaId,
    globalObjectIdFromNablaId,
    heraldEpochBytes,
    systemIdBytes,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalDescriptorBytes,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Profile
  ( CatalogueDigest,
    PredefinedSortRole,
    allPredefinedSortRoles,
    predefinedCatalogueDescriptor,
    profileCatalogueDigest,
    profilePredefinedCatalogue,
    profileSortFor,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    AppliedRoot,
    AppliedRootRole (..),
    CheckedInitialTopologyProjection,
    ConfigurationDigest,
    HeraldMember (..),
    InitialProjectionDigest,
    PredefinedOccurrenceSet,
    allStructuralCarrierRoles,
    appliedBootstrapManifestId,
    appliedProcessAuthority,
    appliedProcessEnvironmentHub,
    appliedProcessEnvironmentObjects,
    appliedProcessEpochId,
    appliedProcessId,
    appliedProcessObjectId,
    appliedProcessResidence,
    appliedProcessRoots,
    appliedRootAuthority,
    appliedRootCatalogueRole,
    appliedRootControlPrerequisite,
    appliedRootOccurrenceId,
    appliedRootPlacement,
    appliedRootRole,
    appliedRootSortId,
    checkedInitialTopologyVertices,
    deriveInitialProjectionDigest,
    deriveSystemBootstrapWriterNablaId,
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
    genesisPredefinedOccurrenceSet,
    predefinedOccurrenceFor,
  )
import Eclips.Oracle.Identity (RaftConfigurationDigest)
import Eclips.Oracle.Internal.Digest (checkedRaftConfigurationDigest)
import Eclips.Raft.Genesis
  ( RaftNativeConfiguration,
    raftNativeElectionTimeoutLower,
    raftNativeElectionTimeoutUpper,
    raftNativeHeartbeatInterval,
    raftNativeVoters,
  )
import Eclips.Raft.Identity
  ( RaftNodeId,
    raftDurationMicrosWord64,
    raftNodeIdBytes,
  )
import GHC.Generics (Generic)

data RaftVoterBinding = RaftVoterBinding RaftNodeId HeraldEpoch
  deriving stock (Eq, Ord, Show)

raftVoterBinding :: RaftNodeId -> HeraldEpoch -> RaftVoterBinding
raftVoterBinding = RaftVoterBinding

raftVoterBindingNode :: RaftVoterBinding -> RaftNodeId
raftVoterBindingNode (RaftVoterBinding node _) = node

raftVoterBindingHeraldEpoch :: RaftVoterBinding -> HeraldEpoch
raftVoterBindingHeraldEpoch (RaftVoterBinding _ herald) = herald

data OracleGenesis
  = OracleGenesis
      SystemId
      [HeraldMember]
      CatalogueDigest
      [CanonicalDescriptor]
      [AppliedProcessBootstrap]
      ConfigurationDigest
      CheckedInitialTopologyProjection
      InitialProjectionDigest
      [RaftVoterBinding]
      RaftNativeConfiguration
      RaftConfigurationDigest
  deriving stock (Eq, Show)

oracleGenesis ::
  SystemId ->
  [HeraldMember] ->
  CatalogueDigest ->
  [CanonicalDescriptor] ->
  [AppliedProcessBootstrap] ->
  ConfigurationDigest ->
  CheckedInitialTopologyProjection ->
  InitialProjectionDigest ->
  [RaftVoterBinding] ->
  RaftNativeConfiguration ->
  RaftConfigurationDigest ->
  OracleGenesis
oracleGenesis = OracleGenesis

data OracleGenesisFault
  = OracleGenesisHasNoActiveHeralds
  | DuplicateActiveHeraldId HeraldId
  | DuplicateActiveHeraldEpoch HeraldEpoch
  | OracleCatalogueDigestMismatch
  | OraclePredefinedCatalogueMismatch
  | DuplicateBootstrapManifestId BootstrapManifestId
  | DuplicateBootstrapProcessId ProcessId
  | DuplicateBootstrapProcessEpochId ProcessEpochId
  | DuplicateBootstrapObjectId GlobalObjectId
  | DuplicateBootstrapStoreIncarnationId StoreIncarnationId
  | BootstrapResidenceNotActive ProcessEpochId HeraldEpoch
  | BootstrapNotAtControlZero ProcessEpochId
  | BootstrapRootSetMismatch ProcessEpochId
  | BootstrapRootSortMismatch ProcessEpochId PredefinedSortRole SortId SortId
  | BootstrapRootOccurrenceMismatch ProcessEpochId PredefinedSortRole
  | InitialTopologyVertexSetMismatch
  | InitialProjectionDigestMismatch
  | DuplicateRaftVoterBinding RaftNodeId
  | DuplicateRaftCohostHeraldEpoch HeraldEpoch
  | RaftCohostHeraldNotActive HeraldEpoch
  | RaftVoterSetMismatch
  | RaftConfigurationDigestMismatch
  deriving stock (Eq, Show)

data CheckedOracleGenesis
  = CheckedOracleGenesis
      SystemId
      [HeraldMember]
      CatalogueDigest
      [CanonicalDescriptor]
      [AppliedProcessBootstrap]
      ConfigurationDigest
      CheckedInitialTopologyProjection
      InitialProjectionDigest
      PredefinedOccurrenceSet
      [RaftVoterBinding]
      RaftNativeConfiguration
      RaftConfigurationDigest
  deriving stock (Eq, Show)

checkOracleGenesis :: OracleGenesis -> Either OracleGenesisFault CheckedOracleGenesis
checkOracleGenesis
  ( OracleGenesis
      systemId
      suppliedMembers
      suppliedCatalogueDigest
      suppliedDescriptors
      suppliedBootstraps
      suppliedConfigurationDigest
      topology
      suppliedProjectionDigest
      suppliedBindings
      nativeConfiguration
      suppliedRaftDigest
    ) = do
    if null members then Left OracleGenesisHasNoActiveHeralds else Right ()
    rejectDuplicate heraldMemberId DuplicateActiveHeraldId members
    rejectDuplicate heraldMemberEpoch DuplicateActiveHeraldEpoch members
    if suppliedCatalogueDigest == profileCatalogueDigest then Right () else Left OracleCatalogueDigestMismatch
    if normalizedDescriptorClaims suppliedDescriptors == expectedDescriptorClaims
      then Right ()
      else Left OraclePredefinedCatalogueMismatch
    rejectDuplicate appliedBootstrapManifestId DuplicateBootstrapManifestId bootstraps
    rejectDuplicate appliedProcessId DuplicateBootstrapProcessId bootstraps
    rejectDuplicate appliedProcessEpochId DuplicateBootstrapProcessEpochId bootstraps
    rejectDuplicate id DuplicateBootstrapObjectId allObjectIds
    rejectDuplicate id DuplicateBootstrapObjectId (reservedInfrastructureObjectIds <> allObjectIds)
    rejectDuplicate id DuplicateBootstrapStoreIncarnationId allBootstrapStoreIds
    mapM_ checkBootstrap bootstraps
    if checkedInitialTopologyVertices topology == expectedTopologyVertices
      then Right ()
      else Left InitialTopologyVertexSetMismatch
    if suppliedProjectionDigest == deriveInitialProjectionDigest bootstraps topology
      then Right ()
      else Left InitialProjectionDigestMismatch
    rejectDuplicate raftVoterBindingNode DuplicateRaftVoterBinding bindings
    rejectDuplicate raftVoterBindingHeraldEpoch DuplicateRaftCohostHeraldEpoch bindings
    mapM_ checkActiveBinding bindings
    if fmap raftVoterBindingNode bindings == raftNativeVoters nativeConfiguration
      then Right ()
      else Left RaftVoterSetMismatch
    let derivedRaftDigest = deriveRaftConfigurationDigest systemId bindings nativeConfiguration
    if suppliedRaftDigest == derivedRaftDigest then Right () else Left RaftConfigurationDigestMismatch
    Right
      ( CheckedOracleGenesis
          systemId
          members
          profileCatalogueDigest
          descriptors
          bootstraps
          suppliedConfigurationDigest
          topology
          suppliedProjectionDigest
          occurrences
          bindings
          nativeConfiguration
          derivedRaftDigest
      )
    where
      members = sort suppliedMembers
      descriptors = sortOn descriptorSortId suppliedDescriptors
      bootstraps = sortOn appliedBootstrapManifestId suppliedBootstraps
      bindings = sortOn raftVoterBindingNode suppliedBindings
      occurrences = genesisPredefinedOccurrenceSet systemId
      activeEpochs = Set.fromList (fmap heraldMemberEpoch members)
      expectedDescriptorClaims = normalizedDescriptorClaims (fmap predefinedCatalogueDescriptor profilePredefinedCatalogue)
      allObjectIds =
        concatMap
          (\bootstrap -> appliedProcessObjectId bootstrap : appliedProcessEnvironmentObjects bootstrap)
          bootstraps
      allBootstrapStoreIds =
        [ deriveSystemViewStoreIncarnationId systemId herald role
        | herald <- Set.toAscList activeEpochs,
          role <- allPredefinedSortRoles
        ]
          <> [incarnation | bootstrap <- bootstraps, root <- appliedProcessRoots bootstrap, Just (_, incarnation) <- [appliedRootPlacement root]]
      reservedInfrastructureObjectIds =
        [ globalObjectIdFromDeltaId
            (deriveSystemViewDeltaId systemId herald role)
        | herald <- Set.toAscList activeEpochs,
          role <- allPredefinedSortRoles
        ]
          <> [ globalObjectIdFromNablaId
                 (deriveSystemBootstrapWriterNablaId systemId herald role)
             | herald <- Set.toAscList activeEpochs,
               role <- allStructuralCarrierRoles
             ]
      expectedTopologyVertices =
        Set.toAscList
          ( Set.fromList
              ( [ rootVertex root
                | bootstrap <- bootstraps,
                  root <- appliedProcessRoots bootstrap
                ]
                  <> map (NeutralVertex . appliedProcessEnvironmentHub) bootstraps
                  <> [ DeltaVertex (deriveSystemViewDeltaId systemId herald role)
                     | herald <- Set.toAscList activeEpochs,
                       role <- allPredefinedSortRoles
                     ]
                  <> [ NablaVertex
                         (deriveSystemBootstrapWriterNablaId systemId herald role)
                     | herald <- Set.toAscList activeEpochs,
                       role <- allStructuralCarrierRoles
                     ]
              )
          )
      checkActiveBinding binding
        | Set.member (raftVoterBindingHeraldEpoch binding) activeEpochs = Right ()
        | otherwise = Left (RaftCohostHeraldNotActive (raftVoterBindingHeraldEpoch binding))
      checkBootstrap bootstrap = do
        if Set.member (appliedProcessResidence bootstrap) activeEpochs
          then Right ()
          else Left (BootstrapResidenceNotActive (appliedProcessEpochId bootstrap) (appliedProcessResidence bootstrap))
        if authorityEpochView (appliedProcessAuthority bootstrap) == GenesisAuthorityEpochView
          then Right ()
          else Left (BootstrapNotAtControlZero (appliedProcessEpochId bootstrap))
        let roots = appliedProcessRoots bootstrap
            process = appliedProcessEpochId bootstrap
        if normalizedRootRoles roots == expectedRootRoles
          then Right ()
          else Left (BootstrapRootSetMismatch process)
        mapM_ (checkRoot process occurrences) roots
checkedOracleSystemId :: CheckedOracleGenesis -> SystemId
checkedOracleSystemId (CheckedOracleGenesis systemId _ _ _ _ _ _ _ _ _ _ _) = systemId

checkedOracleActiveHeralds :: CheckedOracleGenesis -> [HeraldMember]
checkedOracleActiveHeralds (CheckedOracleGenesis _ members _ _ _ _ _ _ _ _ _ _) = members

checkedOracleCatalogueDigest :: CheckedOracleGenesis -> CatalogueDigest
checkedOracleCatalogueDigest (CheckedOracleGenesis _ _ digest _ _ _ _ _ _ _ _ _) = digest

checkedOraclePredefinedDescriptors :: CheckedOracleGenesis -> [CanonicalDescriptor]
checkedOraclePredefinedDescriptors (CheckedOracleGenesis _ _ _ descriptors _ _ _ _ _ _ _ _) = descriptors

checkedOracleAppliedBootstraps :: CheckedOracleGenesis -> [AppliedProcessBootstrap]
checkedOracleAppliedBootstraps (CheckedOracleGenesis _ _ _ _ bootstraps _ _ _ _ _ _ _) = bootstraps

checkedOracleConfigurationDigest :: CheckedOracleGenesis -> ConfigurationDigest
checkedOracleConfigurationDigest (CheckedOracleGenesis _ _ _ _ _ digest _ _ _ _ _ _) = digest

checkedOracleInitialTopologyProjection :: CheckedOracleGenesis -> CheckedInitialTopologyProjection
checkedOracleInitialTopologyProjection (CheckedOracleGenesis _ _ _ _ _ _ topology _ _ _ _ _) = topology

checkedOracleInitialProjectionDigest :: CheckedOracleGenesis -> InitialProjectionDigest
checkedOracleInitialProjectionDigest (CheckedOracleGenesis _ _ _ _ _ _ _ digest _ _ _ _) = digest

checkedOraclePredefinedOccurrences :: CheckedOracleGenesis -> PredefinedOccurrenceSet
checkedOraclePredefinedOccurrences (CheckedOracleGenesis _ _ _ _ _ _ _ _ occurrences _ _ _) = occurrences

checkedOracleRaftVoterBindings :: CheckedOracleGenesis -> [RaftVoterBinding]
checkedOracleRaftVoterBindings (CheckedOracleGenesis _ _ _ _ _ _ _ _ _ bindings _ _) = bindings

checkedOracleRaftNativeConfiguration :: CheckedOracleGenesis -> RaftNativeConfiguration
checkedOracleRaftNativeConfiguration (CheckedOracleGenesis _ _ _ _ _ _ _ _ _ _ configuration _) = configuration

checkedOracleRaftConfigurationDigest :: CheckedOracleGenesis -> RaftConfigurationDigest
checkedOracleRaftConfigurationDigest (CheckedOracleGenesis _ _ _ _ _ _ _ _ _ _ _ digest) = digest

checkedOracleControlIndex :: CheckedOracleGenesis -> ControlIndex
checkedOracleControlIndex _ = controlIndex 0

deriveRaftConfigurationDigest :: SystemId -> [RaftVoterBinding] -> RaftNativeConfiguration -> RaftConfigurationDigest
deriveRaftConfigurationDigest systemId suppliedBindings nativeConfiguration =
  checkedRaftConfigurationDigest
    ( RaftConfigurationDigestTranscript
        "ECLIPS-RAFT-CONFIGURATION"
        (systemIdBytes systemId)
        (fmap bindingTranscript (sortOn raftVoterBindingNode suppliedBindings))
        (raftDurationMicrosWord64 (raftNativeHeartbeatInterval nativeConfiguration))
        (raftDurationMicrosWord64 (raftNativeElectionTimeoutLower nativeConfiguration))
        (raftDurationMicrosWord64 (raftNativeElectionTimeoutUpper nativeConfiguration))
    )

data RaftConfigurationDigestTranscript
  = RaftConfigurationDigestTranscript ByteString ByteString [RaftVoterBindingTranscript] Word64 Word64 Word64
  deriving stock (Generic)
  deriving anyclass (Serialize)

data RaftVoterBindingTranscript = RaftVoterBindingTranscript ByteString ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

bindingTranscript :: RaftVoterBinding -> RaftVoterBindingTranscript
bindingTranscript binding =
  RaftVoterBindingTranscript
    (raftNodeIdBytes (raftVoterBindingNode binding))
    (heraldEpochBytes (raftVoterBindingHeraldEpoch binding))

normalizedDescriptorClaims :: [CanonicalDescriptor] -> [(SortId, ByteString)]
normalizedDescriptorClaims = sort . fmap (\descriptor -> (descriptorSortId descriptor, canonicalDescriptorBytes descriptor))

expectedRootRoles :: [(PredefinedSortRole, Bool)]
expectedRootRoles = concatMap (\role -> [(role, False), (role, True)]) allPredefinedSortRoles

normalizedRootRoles :: [AppliedRoot] -> [(PredefinedSortRole, Bool)]
normalizedRootRoles = sort . fmap (\root -> (appliedRootCatalogueRole root, isWriter (appliedRootRole root)))
  where
    isWriter (WriterRoot _ _) = True
    isWriter (ReaderRoot _) = False

checkRoot :: ProcessEpochId -> PredefinedOccurrenceSet -> AppliedRoot -> Either OracleGenesisFault ()
checkRoot process occurrences root = do
  let role = appliedRootCatalogueRole root
      expectedSort = profileSortFor role
  if appliedRootSortId root == expectedSort
    then Right ()
    else Left (BootstrapRootSortMismatch process role (appliedRootSortId root) expectedSort)
  if appliedRootOccurrenceId root == predefinedOccurrenceFor role occurrences
    then Right ()
    else Left (BootstrapRootOccurrenceMismatch process role)
  if controlIndexWord64 (appliedRootControlPrerequisite root) == 0
    then Right ()
    else Left (BootstrapNotAtControlZero process)
  if authorityEpochView (appliedRootAuthority root) == GenesisAuthorityEpochView
    then Right ()
    else Left (BootstrapNotAtControlZero process)

rejectDuplicate :: (Ord key) => (value -> key) -> (key -> problem) -> [value] -> Either problem ()
rejectDuplicate project problem values = case firstDuplicate (sort (fmap project values)) of
  Nothing -> Right ()
  Just duplicate -> Left (problem duplicate)

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate (first : second : remaining)
  | first == second = Just first
  | otherwise = firstDuplicate (second : remaining)
firstDuplicate _ = Nothing

rootVertex :: AppliedRoot -> VertexId
rootVertex root = case appliedRootRole root of
  WriterRoot nabla _ -> NablaVertex nabla
  ReaderRoot delta -> DeltaVertex delta
