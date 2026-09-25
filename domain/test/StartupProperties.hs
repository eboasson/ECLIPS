{-# LANGUAGE OverloadedStrings #-}

module StartupProperties
  ( tests,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word8)
import DescriptorProperties (regularDescriptor)
import Eclips.Domain.Graph
  ( EdgeStrength (..),
    VertexId (DeltaVertex, NablaVertex, NeutralVertex),
    edgeDestination,
    edgePayload,
    edgeSource,
    edgeStrengthFromSymbol,
    edgeStrengthSymbol,
  )
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    DeltaId,
    GlobalUniqueId,
    HeraldEpoch,
    HeraldId,
    NablaId,
    NablaSequencing (UnsequencedNabla),
    ProcessEpochId,
    ProcessId,
    PublicationId,
    SortDefinitionOccurrenceId,
    StoreIncarnationId,
    SystemId,
    controlIndex,
    deltaIdBytes,
    genesisAuthorityEpoch,
    mkBootstrapManifestId,
    mkDeltaId,
    mkGlobalUniqueId,
    mkHeraldEpoch,
    mkHeraldId,
    mkNablaId,
    mkProcessEpochId,
    mkProcessId,
    mkStoreIncarnationId,
    mkSystemId,
    nablaIdBytes,
    nablaIdFromGlobalObjectId,
    nablaSequence,
    sortDefinitionOccurrenceIdBytes,
    sortIdBytes,
    storeIncarnationIdBytes,
  )
import Eclips.Domain.Publication
  ( PublicationError (..),
    checkedPublicationId,
    mkCheckedPublication,
  )
import Eclips.Domain.Sort.Canonical
  ( canonicalCheckedDescriptor,
    canonicalDescriptorBytes,
    canonicalizeDescriptor,
    descriptorSortId,
    validateCanonicalDescriptorIdentity,
  )
import Eclips.Domain.Sort.Descriptor
  ( ApplicationMutation (..),
    DescriptorAdmission (..),
    DescriptorSpec (..),
    SortKind (..),
    StructuralCarrierRole (..),
    ValueEvaluationError (..),
    checkedDescriptorSpec,
    controlledPolicyProjection,
    descriptorApplicationMutation,
    descriptorImmutable,
    descriptorKind,
    descriptorSchema,
    descriptorStructuralCarrierRole,
    policyApplicationMutation,
    policyHasLabelField,
    policyImmutable,
  )
import Eclips.Domain.Sort.Profile
  ( SortDefinitionIntrinsicError (..),
    profileEntryFor,
    sortDefinitionValue,
  )
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    AppliedRootRole (..),
    CheckedInitialTopologyProjection,
    ConfiguredProcessBootstrap,
    ConfiguredProcessMaterializationError (..),
    ConfiguredRootBootstrap,
    HeraldMember (..),
    InitialTopologyEdgeManifest (..),
    InitialTopologyManifest (..),
    PredefinedCatalogueEntry,
    PredefinedOccurrenceSet,
    PredefinedSortRole (..),
    PrimordialDefinitionReplica,
    PrimordialSortWriterAuthority (..),
    allPredefinedSortRoles,
    allStructuralCarrierRoles,
    appliedProcessAuthority,
    appliedProcessEnvironmentEdges,
    appliedProcessEnvironmentHub,
    appliedProcessEnvironmentObjects,
    appliedProcessEnvironmentPublications,
    appliedProcessRoots,
    appliedRootAuthority,
    appliedRootCatalogueRole,
    appliedRootControlPrerequisite,
    appliedRootOccurrenceId,
    appliedRootRole,
    appliedRootSortId,
    catalogueDigestBytes,
    checkInitialTopologyProjection,
    checkedInitialTopologyEdges,
    checkedInitialTopologyVertices,
    configurationDigestBytes,
    configuredEnvironmentWiringObjects,
    configuredProcessBootstrapRoots,
    configuredRootBootstrap,
    configuredRootBootstrapCatalogueRole,
    configuredRootBootstrapReaderDelta,
    configuredRootBootstrapStoreIncarnation,
    configuredRootBootstrapWriterNabla,
    configuredRootBootstrapWriterSequencing,
    deriveConfigurationDigest,
    deriveInitialProjectionDigest,
    deriveSystemBootstrapWriterNablaId,
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
    genesisPredefinedOccurrenceSet,
    initialProjectionDigestBytes,
    materializeConfiguredProcessBootstrap,
    mkConfiguredProcessBootstrap,
    mkPredefinedOccurrenceSet,
    predefinedCatalogueDescriptor,
    predefinedCatalogueRole,
    predefinedCatalogueSortId,
    predefinedOccurrenceFor,
    predefinedOccurrences,
    predefinedSortRoleTag,
    primordialDefinitionReplica,
    primordialReplicaPublicationId,
    profileCatalogueDigest,
    profilePredefinedCatalogue,
  )
import Eclips.Domain.Value
  ( EnumSymbol,
    FieldName,
    LabelOwner (..),
    Value,
    ValueSchema,
    bytesSchema,
    bytesValue,
    enumSchema,
    enumSymbol,
    enumValue,
    globalUniqueIdSchema,
    globalUniqueIdValue,
    labelSchema,
    labelValue,
    mkFieldName,
    optionalGlobalUniqueIdSchema,
    recordSchema,
    recordValue,
    viewSchema,
  )
import Numeric (showHex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (Property, conjoin, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "shared startup vocabulary"
    [ testCase "the closed catalogue has exactly the normative six roles" caseRoleOrder,
      testCase "predefined descriptor identities are their canonical hashes" caseDescriptorIdentities,
      testCase "only the four topology sorts have immutable controlled policy" caseCataloguePolicies,
      testCase "carrier roles and universal-edge fields are fixed" caseCarrierShape,
      testCase "edge strengths use the closed preserve/weaken enum" caseEdgeStrengthAdmission,
      testCase "catalogue digest and genesis occurrence vector are pinned" caseCatalogueAndOccurrenceGoldens,
      testCase "system-view identities and initial projection identity are pinned" caseSystemViewAndProjectionGoldens,
      testProperty "initial topology presentation order is irrelevant" propInitialTopologyOrder,
      testCase "checked initial topology resolves only symbolic endpoints" caseCheckedInitialTopology,
      testCase "current occurrence sets require and canonicalise all six roles" caseOccurrenceSet,
      testCase "sort-definition admission accepts canonical values" caseSortDefinitionIntrinsic,
      testCase "sort-definition admission rejects malformed claims" caseSortDefinitionIntrinsicRejection,
      testProperty "changing a sort definition requires its new content-addressed key" propSortDefinitionContentAddress,
      testCase "configured templates canonicalise complete roots" caseTemplateCanonicalisation,
      testCase "configured templates reject incomplete or locally conflicting roots" caseTemplateRejection,
      testCase "genesis materialisation assigns genesis authority consistently" caseMaterialisation,
      testProperty "configured environments have discoverable wiring and a shared reader SCC" propConfiguredEnvironment,
      testCase "non-genesis materialisation is rejected" caseNonGenesisMaterialisation,
      testCase "immutable genesis configuration is fixed and presentation independent" caseConfigurationGolden
    ]

caseRoleOrder :: IO ()
caseRoleOrder = do
  assertEqual "role order" expectedRoles allPredefinedSortRoles
  assertEqual "explicit stable tags" [0 .. 5] (fmap predefinedSortRoleTag allPredefinedSortRoles)
  assertEqual "catalogue role order" expectedRoles (fmap predefinedCatalogueRole profilePredefinedCatalogue)

caseDescriptorIdentities :: IO ()
caseDescriptorIdentities =
  mapM_
    ( \entry -> do
        let descriptor = predefinedCatalogueDescriptor entry
            sortId = predefinedCatalogueSortId entry
        assertEqual "descriptor exposes its hash" sortId (descriptorSortId descriptor)
        assertEqual
          "canonical bytes validate against that hash"
          (Right descriptor)
          ( validateCanonicalDescriptorIdentity
              PrimordialDescriptor
              sortId
              (canonicalDescriptorBytes descriptor)
          )
    )
    profilePredefinedCatalogue

caseCataloguePolicies :: IO ()
caseCataloguePolicies =
  mapM_
    ( \(role, expectedKind, expectedImmutable, expectedMutation) -> do
        entry <- entryFor role
        let descriptor = canonicalCheckedDescriptor (predefinedCatalogueDescriptor entry)
        assertEqual (show role <> " kind") expectedKind (descriptorKind descriptor)
        assertEqual (show role <> " immutability") expectedImmutable (descriptorImmutable descriptor)
        assertEqual (show role <> " mutation authority") expectedMutation (descriptorApplicationMutation descriptor)
        case controlledPolicyProjection descriptor of
          Nothing -> assertEqual "only sort definitions are regular" SortDefinitionRole role
          Just policy -> do
            assertBool (show role <> " retains a label field") (policyHasLabelField policy)
            assertEqual (show role <> " projected immutability") expectedImmutable (policyImmutable policy)
            assertEqual (show role <> " projected authority") expectedMutation (policyApplicationMutation policy)
    )
    [ (SortDefinitionRole, RegularSort, False, OrdinaryApplicationMutation),
      (NeutralVertexRole, ControlledSort, True, OrdinaryApplicationMutation),
      (EdgeRole, ControlledSort, True, OrdinaryApplicationMutation),
      (NablaRole, ControlledSort, True, OrdinaryApplicationMutation),
      (DeltaRole, ControlledSort, True, OrdinaryApplicationMutation),
      (ProcessEpochRole, ControlledSort, False, HeraldManagedMutation)
    ]

caseCarrierShape :: IO ()
caseCarrierShape = do
  assertEqual
    "closed carrier projection"
    [ Nothing,
      Just NeutralVertexCarrier,
      Just EdgeCarrier,
      Just NablaCarrier,
      Just DeltaCarrier,
      Just ProcessEpochCarrier
    ]
    (fmap carrier profilePredefinedCatalogue)
  edge <- entryFor EdgeRole
  assertEqual
    "universal edge has exactly the unfiltered carrier schema"
    (viewSchema universalEdgeSchema)
    ( viewSchema
        (descriptorSchema (canonicalCheckedDescriptor (predefinedCatalogueDescriptor edge)))
    )
  nabla <- entryFor NablaRole
  assertEqual
    "every Nabla explicitly carries its optional sequencing object"
    ( viewSchema
        ( checked
            "Nabla schema"
            ( recordSchema
                [ (fieldName "object_id", globalUniqueIdSchema),
                  (fieldName "label", labelSchema),
                  (fieldName "sort_id", bytesSchema),
                  (fieldName "sequencing_object", optionalGlobalUniqueIdSchema)
                ]
            )
        )
    )
    ( viewSchema
        (descriptorSchema (canonicalCheckedDescriptor (predefinedCatalogueDescriptor nabla)))
    )
  where
    carrier =
      descriptorStructuralCarrierRole
        . canonicalCheckedDescriptor
        . predefinedCatalogueDescriptor

caseEdgeStrengthAdmission :: IO ()
caseEdgeStrengthAdmission = do
  edge <- entryFor EdgeRole
  let descriptor = predefinedCatalogueDescriptor edge
      publication strength =
        mkCheckedPublication
          descriptor
          fixturePrimordialPublicationId
          (edgeValue strength)
  assertEqual "preserve symbol" (Just Preserve) (edgeStrengthFromSymbol (edgeStrengthSymbol Preserve))
  assertEqual "weaken symbol" (Just Weaken) (edgeStrengthFromSymbol (edgeStrengthSymbol Weaken))
  assertBool "preserve admitted" (either (const False) (const True) (publication (edgeStrengthSymbol Preserve)))
  assertBool "weaken admitted" (either (const False) (const True) (publication (edgeStrengthSymbol Weaken)))
  case publication (enumSymbol "amplify") of
    Left (PublicationValueRejected (ValueSchemaRejected _)) -> pure ()
    other -> assertFailure ("unknown edge enum was not rejected by its schema: " <> show other)

edgeValue :: EnumSymbol -> Value
edgeValue strength =
  checked
    "edge test value"
    ( recordValue
        [ (fieldName "object_id", globalUniqueIdValue (globalUniqueId 90)),
          (fieldName "label", labelValue (VoidLabel, 0)),
          (fieldName "source_vertex", globalUniqueIdValue (globalUniqueId 91)),
          (fieldName "destination_vertex", globalUniqueIdValue (globalUniqueId 92)),
          (fieldName "strength", enumValue strength)
        ]
    )

caseCatalogueAndOccurrenceGoldens :: IO ()
caseCatalogueAndOccurrenceGoldens = do
  assertEqual
    "catalogue digest"
    catalogueDigestGolden
    (renderHex (catalogueDigestBytes profileCatalogueDigest))
  assertEqual
    "role-ordered genesis occurrence ids"
    occurrenceGoldens
    [ renderHex
        ( sortDefinitionOccurrenceIdBytes
            ( deriveSortDefinitionOccurrenceId
                fixtureSystemId
                (predefinedCatalogueSortId entry)
                Genesis
            )
        )
    | entry <- profilePredefinedCatalogue
    ]

caseSystemViewAndProjectionGoldens :: IO ()
caseSystemViewAndProjectionGoldens = do
  assertEqual
    "role-ordered system-view delta identities"
    systemViewDeltaGoldens
    [ renderHex (deltaIdBytes (deriveSystemViewDeltaId fixtureSystemId fixtureHeraldEpoch role))
    | role <- allPredefinedSortRoles
    ]
  assertEqual
    "role-ordered system-view store incarnations"
    systemViewIncarnationGoldens
    [ renderHex
        ( storeIncarnationIdBytes
            (deriveSystemViewStoreIncarnationId fixtureSystemId fixtureHeraldEpoch role)
        )
    | role <- allPredefinedSortRoles
    ]
  assertEqual
    "explicitly carrier-ordered hidden bootstrap-writer identities"
    systemBootstrapWriterGoldens
    [ renderHex
        ( nablaIdBytes
            (deriveSystemBootstrapWriterNablaId fixtureSystemId fixtureHeraldEpoch role)
        )
    | role <- allStructuralCarrierRoles
    ]
  template <- fixtureTemplateFrom fixtureRoots
  let applied = checked "genesis materialisation" (materializeConfiguredProcessBootstrap (controlIndex 0) fixtureOccurrences template)
      topology = checkedTopology fixtureMembers [applied] (InitialTopologyManifest [])
  assertEqual
    "full applied projection identity"
    initialProjectionDigestGolden
    (renderHex (initialProjectionDigestBytes (deriveInitialProjectionDigest [applied] topology)))

propInitialTopologyOrder :: Bool -> Property
propInitialTopologyOrder reverseInput =
  checkedInitialTopologyEdges ordered === checkedInitialTopologyEdges reordered
  where
    (bootstraps, manifest) = fixtureCrossTopology
    ordered = checkedTopology fixtureMembers bootstraps manifest
    reordered = case manifest of
      InitialTopologyManifest edges ->
        checkedTopology
          (reverse fixtureMembers)
          (reverse bootstraps)
          (InitialTopologyManifest (if reverseInput then reverse edges else edges))

caseCheckedInitialTopology :: IO ()
caseCheckedInitialTopology = do
  let (bootstraps, manifest) = fixtureCrossTopology
      topology = checkedTopology fixtureMembers bootstraps manifest
      withoutManifest =
        checkedTopology fixtureMembers bootstraps (InitialTopologyManifest [])
  assertEqual
    "twenty-four roots, two hubs, twelve system views, and ten hidden writers"
    48
    (length (checkedInitialTopologyVertices topology))
  assertEqual "base edges plus two symbolic cross-Herald edges" 50 (length (checkedInitialTopologyEdges topology))
  assertEqual
    "the symbolic manifest contributes exactly two edges"
    2
    (length (checkedInitialTopologyEdges topology) - length (checkedInitialTopologyEdges withoutManifest))
  let hiddenWriters =
        [ NablaVertex
            (deriveSystemBootstrapWriterNablaId fixtureSystemId residence role)
        | member <- fixtureMembers,
          let residence = heraldMemberEpoch member,
          role <- allStructuralCarrierRoles
        ]
      topologyEdges = checkedInitialTopologyEdges topology
  assertBool
    "every checked hidden writer is an isolated initial Nabla vertex"
    ( all (`elem` checkedInitialTopologyVertices topology) hiddenWriters
        && all
          ( \writer ->
              all
                (\edge -> edgeSource edge /= writer && edgeDestination edge /= writer)
                topologyEdges
          )
          hiddenWriters
    )
  case bootstraps of
    [source, destination] -> do
      let sourceWriter = rootWriter SortDefinitionRole source
          destinationReader = rootReader SortDefinitionRole destination
          remoteView = deriveSystemViewDeltaId fixtureSystemId (heraldEpoch 5) SortDefinitionRole
      assertBool
        "process destination is resolved from its selected bootstrap"
        ( edgePayload (NablaVertex sourceWriter) (DeltaVertex destinationReader) Preserve
            `elem` checkedInitialTopologyEdges topology
        )
      assertBool
        "private destination is derived from system, active epoch, and role"
        ( edgePayload (NablaVertex sourceWriter) (DeltaVertex remoteView) Preserve
            `elem` checkedInitialTopologyEdges topology
        )
    _ -> assertFailure "cross-topology fixture does not contain exactly two bootstraps"
  assertBool
    "the checked topology changes projection identity"
    (deriveInitialProjectionDigest bootstraps topology /= deriveInitialProjectionDigest bootstraps withoutManifest)

caseOccurrenceSet :: IO ()
caseOccurrenceSet = do
  assertEqual
    "canonical role order"
    expectedRoles
    (fmap fst (predefinedOccurrences fixtureOccurrences))
  assertEqual
    "presentation order"
    (Just fixtureOccurrences)
    (mkPredefinedOccurrenceSet (reverse fixtureOccurrenceEntries))
  assertBool
    "missing role"
    (isNothing (mkPredefinedOccurrenceSet (drop 1 fixtureOccurrenceEntries)))
  case fixtureOccurrenceEntries of
    first : _second : remaining ->
      assertBool
        "duplicate role"
        (isNothing (mkPredefinedOccurrenceSet (first : first : remaining)))
    _ -> assertFailure "closed profile unexpectedly has fewer than two occurrences"

caseSortDefinitionIntrinsic :: IO ()
caseSortDefinitionIntrinsic = do
  carrier <- entryFor SortDefinitionRole
  embedded <- entryFor NeutralVertexRole
  let provenance = fixturePrimordialPublicationId
  assertBool
    "the sole publication constructor admits a canonical embedded descriptor"
    ( either
        (const False)
        (const True)
        ( mkCheckedPublication
            (predefinedCatalogueDescriptor carrier)
            provenance
            (sortDefinitionValue (predefinedCatalogueDescriptor embedded))
        )
    )
  assertBool
    "the application-descriptor branch admits an ordinary application sort"
    ( either
        (const False)
        (const True)
        ( mkCheckedPublication
            (predefinedCatalogueDescriptor carrier)
            provenance
            (sortDefinitionValue regularDescriptor)
        )
    )

caseSortDefinitionIntrinsicRejection :: IO ()
caseSortDefinitionIntrinsicRejection = do
  carrier <- entryFor SortDefinitionRole
  embedded <- entryFor NeutralVertexRole
  other <- entryFor EdgeRole
  let provenance = fixturePrimordialPublicationId
      carrierDescriptor = predefinedCatalogueDescriptor carrier
      embeddedBytes = canonicalDescriptorBytes (predefinedCatalogueDescriptor embedded)
      otherClaim = sortIdBytes (predefinedCatalogueSortId other)
      check expected value =
        assertEqual
          "typed intrinsic rejection"
          (Left (PublicationIntrinsicRejected expected))
          (mkCheckedPublication carrierDescriptor provenance value)
  check
    SortDefinitionIntrinsicClaimedSortIdWrongSize
    (sortDefinitionTestValue (ByteString.take 31 otherClaim) embeddedBytes)
  check
    SortDefinitionIntrinsicClaimedSortIdMismatch
    (sortDefinitionTestValue otherClaim embeddedBytes)
  check
    SortDefinitionIntrinsicDescriptorRejected
    ( sortDefinitionTestValue
        (sortIdBytes (predefinedCatalogueSortId embedded))
        (embeddedBytes <> ByteString.singleton 0)
    )
  let structuralSpec =
        (checkedDescriptorSpec (canonicalCheckedDescriptor (predefinedCatalogueDescriptor embedded)))
          { descriptorSpecMinimumRetentionMicros = 1
          }
      nonProfileStructural =
        checked
          "non-profile structural descriptor"
          (canonicalizeDescriptor PrimordialDescriptor structuralSpec)
  check
    SortDefinitionIntrinsicDescriptorRejected
    ( sortDefinitionTestValue
        (sortIdBytes (descriptorSortId nonProfileStructural))
        (canonicalDescriptorBytes nonProfileStructural)
    )

propSortDefinitionContentAddress :: Word8 -> Property
propSortDefinitionContentAddress retention =
  let originalSpec = checkedDescriptorSpec (canonicalCheckedDescriptor regularDescriptor)
      changedDescriptor =
        checked
          "changed application sort descriptor"
          ( canonicalizeDescriptor
              ApplicationDescriptor
              originalSpec {descriptorSpecMinimumRetentionMicros = fromIntegral retention}
          )
      carrierDescriptor = predefinedCatalogueDescriptor (profileEntryFor SortDefinitionRole)
      publish = mkCheckedPublication carrierDescriptor fixturePrimordialPublicationId
   in conjoin
        [ publish
            ( sortDefinitionTestValue
                (sortIdBytes (descriptorSortId regularDescriptor))
                (canonicalDescriptorBytes changedDescriptor)
            )
            === Left (PublicationIntrinsicRejected SortDefinitionIntrinsicClaimedSortIdMismatch),
          (either (const False) (const True) (publish (sortDefinitionValue changedDescriptor))) === True
        ]

sortDefinitionTestValue :: ByteString -> ByteString -> Value
sortDefinitionTestValue claimedSort descriptorBytes =
  checked
    "sort-definition test value"
    ( recordValue
        [ (fieldName "sort_id", bytesValue claimedSort),
          (fieldName "canonical_descriptor", bytesValue descriptorBytes)
        ]
    )

caseTemplateCanonicalisation :: IO ()
caseTemplateCanonicalisation = do
  template <- fixtureTemplateFrom (reverse fixtureRoots)
  assertEqual
    "canonical root role order"
    expectedRoles
    (fmap configuredRootBootstrapCatalogueRole (configuredProcessBootstrapRoots template))

caseTemplateRejection :: IO ()
caseTemplateRejection = do
  assertBool "missing role" (isNothing (fixtureTemplate (drop 1 fixtureRoots)))
  case fixtureRoots of
    first : second : remaining -> do
      assertBool
        "duplicate role"
        (isNothing (fixtureTemplate (first : first : remaining)))
      let duplicateWriter =
            configuredRootBootstrap
              (configuredRootBootstrapCatalogueRole second)
              (configuredRootBootstrapWriterNabla first)
              (configuredRootBootstrapWriterSequencing second)
              (configuredRootBootstrapReaderDelta second)
              (configuredRootBootstrapStoreIncarnation second)
      assertBool
        "same globally convertible root identity"
        (isNothing (fixtureTemplate (first : duplicateWriter : remaining)))
      let duplicateStore =
            configuredRootBootstrap
              (configuredRootBootstrapCatalogueRole second)
              (configuredRootBootstrapWriterNabla second)
              (configuredRootBootstrapWriterSequencing second)
              (configuredRootBootstrapReaderDelta second)
              (configuredRootBootstrapStoreIncarnation first)
      assertBool
        "same local store incarnation"
        (isNothing (fixtureTemplate (first : duplicateStore : remaining)))
      mapM_
        ( \object ->
            let collidingWriter =
                  configuredRootBootstrap
                    (configuredRootBootstrapCatalogueRole first)
                    (nablaIdFromGlobalObjectId object)
                    (configuredRootBootstrapWriterSequencing first)
                    (configuredRootBootstrapReaderDelta first)
                    (configuredRootBootstrapStoreIncarnation first)
             in assertBool
                  "configured endpoints cannot collide with the derived hub or wiring identities"
                  (isNothing (fixtureTemplate (collidingWriter : second : remaining)))
        )
        (configuredEnvironmentWiringObjects (bootstrapManifestId 10))
    _ -> assertFailure "closed profile unexpectedly has fewer than two roots"

caseMaterialisation :: IO ()
caseMaterialisation = do
  template <- fixtureTemplateFrom (reverse fixtureRoots)
  let appliedAt = controlIndex 0
      applied = checked "genesis materialisation" (materializeConfiguredProcessBootstrap appliedAt fixtureOccurrences template)
      roots = appliedProcessRoots applied
  assertEqual "process authority" genesisAuthorityEpoch (appliedProcessAuthority applied)
  assertEqual
    "writer then reader for every catalogue role"
    (concatMap (const [True, False]) expectedRoles)
    ( fmap
        ( \root -> case appliedRootRole root of
            WriterRoot _ _ -> True
            ReaderRoot _ -> False
        )
        roots
    )
  mapM_
    ( \root -> do
        assertEqual "root authority" genesisAuthorityEpoch (appliedRootAuthority root)
        assertEqual "root prerequisite" appliedAt (appliedRootControlPrerequisite root)
        entry <- entryFor (appliedRootCatalogueRole root)
        assertEqual "profile sort" (predefinedCatalogueSortId entry) (appliedRootSortId root)
        assertEqual
          "current occurrence"
          (predefinedOccurrenceFor (appliedRootCatalogueRole root) fixtureOccurrences)
          (appliedRootOccurrenceId root)
    )
    roots

caseNonGenesisMaterialisation :: IO ()
caseNonGenesisMaterialisation = do
  template <- fixtureTemplateFrom fixtureRoots
  let appliedAt = controlIndex 7
  assertEqual
    "dynamic Start cannot fabricate an authority from ControlIndex"
    (Left (ConfiguredProcessMaterializationRequiresGenesis appliedAt))
    (materializeConfiguredProcessBootstrap appliedAt fixtureOccurrences template)

propConfiguredEnvironment :: Word8 -> Property
propConfiguredEnvironment seed =
  let template = maybe (error "configured environment fixture rejected") id (mkConfiguredProcessBootstrap (bootstrapManifestId seed) (processId 11) (processEpochId 12) fixtureHeraldEpoch fixtureRoots)
      applied = checked "configured environment" (materializeConfiguredProcessBootstrap (controlIndex 0) fixtureOccurrences template)
      edges = map snd (appliedProcessEnvironmentEdges applied)
      hub = NeutralVertex (appliedProcessEnvironmentHub applied)
      readers = [DeltaVertex delta | root <- appliedProcessRoots applied, ReaderRoot delta <- [appliedRootRole root]]
      writers = [NablaVertex writer | root <- appliedProcessRoots applied, WriterRoot writer _ <- [appliedRootRole root]]
      context = Set.fromList (hub : readers)
      publications = appliedProcessEnvironmentPublications fixtureSystemId [applied] applied
      reach source = visit Set.empty [source]
        where
          visit seen [] = seen
          visit seen (vertex : pending)
            | vertex `Set.member` seen = visit seen pending
            | otherwise = visit (Set.insert vertex seen) ([edgeDestination edge | edge <- edges, edgeSource edge == vertex] <> pending)
   in conjoin
        ( [ Set.size (Set.fromList (appliedProcessEnvironmentObjects applied)) === 31,
            length edges === 18,
            length publications === 31,
            Set.size (Set.fromList [checkedPublicationId publication | (_, _, publication) <- publications]) === 31
          ]
            <> [reach vertex === context | vertex <- hub : readers]
            <> [reach writer === Set.insert writer context | writer <- writers]
        )

caseConfigurationGolden :: IO ()
caseConfigurationGolden = do
  let digest =
        deriveConfigurationDigest
          fixtureSystemId
          fixtureMembers
          profileCatalogueDigest
          fixtureReplicas
      reordered =
        deriveConfigurationDigest
          fixtureSystemId
          (reverse fixtureMembers)
          profileCatalogueDigest
          (reverse fixtureReplicas)
  assertEqual "fixed normalized binding" configurationDigestGolden (renderHex (configurationDigestBytes digest))
  assertEqual "presentation order" digest reordered

expectedRoles :: [PredefinedSortRole]
expectedRoles =
  [ SortDefinitionRole,
    NeutralVertexRole,
    EdgeRole,
    NablaRole,
    DeltaRole,
    ProcessEpochRole
  ]

fixtureMembers :: [HeraldMember]
fixtureMembers =
  [ HeraldMember fixtureHeraldId fixtureHeraldEpoch,
    HeraldMember (heraldId 4) (heraldEpoch 5)
  ]

fixtureWriterAuthority :: PrimordialSortWriterAuthority
fixtureWriterAuthority =
  PrimordialSortWriterAuthority
    { primordialWriterNabla = nablaId 6,
      primordialWriterAuthorityEpoch = genesisAuthorityEpoch,
      primordialWriterSourceHeraldEpoch = fixtureHeraldEpoch
    }

fixtureReplicas :: [PrimordialDefinitionReplica]
fixtureReplicas =
  [ primordialDefinitionReplica
      fixtureSystemId
      role
      (nablaSequence (fromIntegral (predefinedSortRoleTag role) + 1))
      fixtureWriterAuthority
  | entry <- profilePredefinedCatalogue,
    let role = predefinedCatalogueRole entry
  ]

fixturePrimordialPublicationId :: PublicationId
fixturePrimordialPublicationId =
  primordialReplicaPublicationId
    ( primordialDefinitionReplica
        fixtureSystemId
        SortDefinitionRole
        (nablaSequence 1)
        fixtureWriterAuthority
    )

fixtureRoots :: [ConfiguredRootBootstrap]
fixtureRoots =
  [ configuredRootBootstrap
      role
      (nablaId (40 + 2 * tag))
      UnsequencedNabla
      (deltaId (41 + 2 * tag))
      (storeIncarnationId (80 + tag))
  | entry <- profilePredefinedCatalogue,
    let role = predefinedCatalogueRole entry
        tag = predefinedSortRoleTag role
  ]

fixtureRemoteRoots :: [ConfiguredRootBootstrap]
fixtureRemoteRoots =
  [ configuredRootBootstrap
      role
      (nablaId (140 + 2 * tag))
      UnsequencedNabla
      (deltaId (141 + 2 * tag))
      (storeIncarnationId (180 + tag))
  | entry <- profilePredefinedCatalogue,
    let role = predefinedCatalogueRole entry
        tag = predefinedSortRoleTag role
  ]

fixtureCrossTopology :: ([AppliedProcessBootstrap], InitialTopologyManifest)
fixtureCrossTopology =
  ( [ checked "source genesis materialisation" (materializeConfiguredProcessBootstrap (controlIndex 0) fixtureOccurrences source),
      checked "destination genesis materialisation" (materializeConfiguredProcessBootstrap (controlIndex 0) fixtureOccurrences destination)
    ],
    InitialTopologyManifest
      [ InitialTopologyProcessEdge sourceId destinationId SortDefinitionRole,
        InitialTopologySystemViewEdge sourceId (heraldEpoch 5) SortDefinitionRole
      ]
  )
  where
    sourceId = bootstrapManifestId 10
    destinationId = bootstrapManifestId 13
    source = maybe (error "valid source topology template was rejected") id (fixtureTemplate fixtureRoots)
    destination =
      maybe
        (error "valid destination topology template was rejected")
        id
        ( mkConfiguredProcessBootstrap
            destinationId
            (processId 14)
            (processEpochId 15)
            (heraldEpoch 5)
            fixtureRemoteRoots
        )

checkedTopology ::
  [HeraldMember] ->
  [AppliedProcessBootstrap] ->
  InitialTopologyManifest ->
  CheckedInitialTopologyProjection
checkedTopology members bootstraps manifest =
  checked
    "initial topology"
    (checkInitialTopologyProjection fixtureSystemId members bootstraps manifest)

rootWriter :: PredefinedSortRole -> AppliedProcessBootstrap -> NablaId
rootWriter role bootstrap = case [ nabla
                                 | root <- appliedProcessRoots bootstrap,
                                   appliedRootCatalogueRole root == role,
                                   WriterRoot nabla _ <- [appliedRootRole root]
                                 ] of
  [nabla] -> nabla
  _ -> error "checked bootstrap has no unique writer for role"

rootReader :: PredefinedSortRole -> AppliedProcessBootstrap -> DeltaId
rootReader role bootstrap = case [ delta
                                 | root <- appliedProcessRoots bootstrap,
                                   appliedRootCatalogueRole root == role,
                                   ReaderRoot delta <- [appliedRootRole root]
                                 ] of
  [delta] -> delta
  _ -> error "checked bootstrap has no unique reader for role"

fixtureOccurrenceEntries :: [(PredefinedSortRole, SortDefinitionOccurrenceId)]
fixtureOccurrenceEntries =
  [ ( predefinedCatalogueRole entry,
      deriveSortDefinitionOccurrenceId
        fixtureSystemId
        (predefinedCatalogueSortId entry)
        Genesis
    )
  | entry <- profilePredefinedCatalogue
  ]

fixtureOccurrences :: PredefinedOccurrenceSet
fixtureOccurrences = genesisPredefinedOccurrenceSet fixtureSystemId

fixtureTemplate :: [ConfiguredRootBootstrap] -> Maybe ConfiguredProcessBootstrap
fixtureTemplate =
  mkConfiguredProcessBootstrap
    (bootstrapManifestId 10)
    (processId 11)
    (processEpochId 12)
    fixtureHeraldEpoch

fixtureTemplateFrom :: [ConfiguredRootBootstrap] -> IO ConfiguredProcessBootstrap
fixtureTemplateFrom = maybe (assertFailure "valid template was rejected") pure . fixtureTemplate

entryFor :: PredefinedSortRole -> IO PredefinedCatalogueEntry
entryFor role = maybe (assertFailure ("missing catalogue role " <> show role)) pure (findEntry role)

findEntry :: PredefinedSortRole -> Maybe PredefinedCatalogueEntry
findEntry role =
  case filter ((== role) . predefinedCatalogueRole) profilePredefinedCatalogue of
    [entry] -> Just entry
    _ -> Nothing

fixtureSystemId :: SystemId
fixtureSystemId = systemId 1

fixtureHeraldId :: HeraldId
fixtureHeraldId = heraldId 2

fixtureHeraldEpoch :: HeraldEpoch
fixtureHeraldEpoch = heraldEpoch 3

systemId :: Word8 -> SystemId
systemId seed = checked "SystemId" (mkSystemId (identifierBytes seed))

heraldId :: Word8 -> HeraldId
heraldId seed = checked "HeraldId" (mkHeraldId (identifierBytes seed))

heraldEpoch :: Word8 -> HeraldEpoch
heraldEpoch seed = checked "HeraldEpoch" (mkHeraldEpoch (identifierBytes seed))

bootstrapManifestId :: Word8 -> BootstrapManifestId
bootstrapManifestId seed = checked "BootstrapManifestId" (mkBootstrapManifestId (identifierBytes seed))

processId :: Word8 -> ProcessId
processId seed = checked "ProcessId" (mkProcessId (identifierBytes seed))

processEpochId :: Word8 -> ProcessEpochId
processEpochId seed = checked "ProcessEpochId" (mkProcessEpochId (identifierBytes seed))

globalUniqueId :: Word8 -> GlobalUniqueId
globalUniqueId seed = checked "GlobalUniqueId" (mkGlobalUniqueId (identifierBytes seed))

nablaId :: Word8 -> NablaId
nablaId seed = checked "NablaId" (mkNablaId (identifierBytes seed))

deltaId :: Word8 -> DeltaId
deltaId seed = checked "DeltaId" (mkDeltaId (identifierBytes seed))

storeIncarnationId :: Word8 -> StoreIncarnationId
storeIncarnationId seed = checked "StoreIncarnationId" (mkStoreIncarnationId (identifierBytes seed))

identifierBytes :: Word8 -> ByteString
identifierBytes seed =
  ByteString.pack
    [ seed + fromIntegral (index * 37 + index * index)
    | index <- [(0 :: Int) .. 31]
    ]

universalEdgeSchema :: ValueSchema
universalEdgeSchema =
  checked
    "universal edge schema"
    ( recordSchema
        [ (fieldName "object_id", globalUniqueIdSchema),
          (fieldName "label", labelSchema),
          (fieldName "source_vertex", globalUniqueIdSchema),
          (fieldName "destination_vertex", globalUniqueIdSchema),
          ( fieldName "strength",
            checked
              "edge strength enum"
              (enumSchema (edgeStrengthSymbol Preserve :| [edgeStrengthSymbol Weaken]))
          )
        ]
    )

fieldName :: Text -> FieldName
fieldName name = checked "FieldName" (mkFieldName name)

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

renderHex :: ByteString -> String
renderHex = concatMap byteHex . ByteString.unpack
  where
    byteHex byte = case showHex byte "" of
      [digit] -> ['0', digit]
      digits -> digits

catalogueDigestGolden :: String
catalogueDigestGolden = "dfe26466b566e5b6196e6b8a2e3c73dd3b87789d863c1a260a7fdbc1de6f43f3"

occurrenceGoldens :: [String]
occurrenceGoldens =
  [ "1ce11d27f317098acc46bd6f7df5d984260eb4c4b5786b6fe78e241014b5e5bd",
    "84aa636d9821210a2ccb5d83377f6843ab402d065a8bb389cd39021e204ebfb9",
    "598fe9ae1486088bffc74e32edf39157fd83cd14ecb02d7a8fd1b8bcb8cd2a2d",
    "de0639ad6fb4aadb7395fc860e6b1dab7a9ba9eef636d67c13e3196e1d1ee819",
    "d1ae41315edbd801884bb9886e7d5e936cc9af83cfd7ef857295b0680b26b080",
    "97ae57c34ece866d79fc26cf4d87028ef465364fa632dd9f0fb3f44d9e86144a"
  ]

configurationDigestGolden :: String
configurationDigestGolden = "de44372deb8ae6c8c2233097ef0aec8d53ce72eaf66a9edb99d0f3a8eb23c579"

systemViewDeltaGoldens :: [String]
systemViewDeltaGoldens =
  [ "841c7ffd51ba9cdcb4d973d16ccc8764b525afd520ec7c324f4ab967cf58b080",
    "e36a0b28ad4aeb67f7da4845b8a0bfc34b0404915106a7e44f93bde0053cce75",
    "d5c222d2e33d5b550baba6cce350812e1092acd2c08e443618b59794d653a4be",
    "f1d754028d3442111076902d788b77b30a649ec302aba4345987385c630bb6cd",
    "2efb5f3f218ea11237b6d0891896c4273f185ccdabc0a474fe47ff7cc931b203",
    "124941c8a90dc0dd13b1eb6f2164812136ffb620c192a11b7563a60bd09ae01d"
  ]

systemViewIncarnationGoldens :: [String]
systemViewIncarnationGoldens =
  [ "7a681e2a17d82d018bdb19d819bb40d8e28820c8969b7f9fcca58c526f959866",
    "5f7517e12341832237392242f4e47d29f3b67f168cec4abc84b1b271c2ed68e3",
    "6862a717c7c08b12ddfa50ea281da1fafbb56c7c61c91a0ca55217c8b3abc2be",
    "36d3b5eeb1eb09d7c69c59885c733765139ce6f069b9cbf1fee418d04457fc61",
    "57959c55df357edeb03c85c31b614c3c7680b79dd45f4b0c899d2918bedf2775",
    "ec2ab48ed1183368dc1da4f9ff05f0e05979ec0ba1459b5052e1eb6575564765"
  ]

systemBootstrapWriterGoldens :: [String]
systemBootstrapWriterGoldens =
  [ "7478455e4bfad61255c8044af2fb31886a44fcb2109c9a7a9309fde2b001cb71",
    "2d00f0716a81a5ca8988e70e038f2d4ebf55a7b32b5937987bc29486284d8e25",
    "d231cdbdc2eb7847ec1ae5806d3601f884bb4d8d2c9d53b4dfbfb28a81cffbb4",
    "1dd906a89127bc67158ee269bfc3cde451a5c6c477245a64a018eb5787cbb951",
    "dde163c4d763d9c97557213259bdc245defde1551846347750903146c3eaadf7"
  ]

initialProjectionDigestGolden :: String
initialProjectionDigestGolden = "1dcc082eb964ab572dfc04cfd2ca35fbb5a139f7fecc03f9da0f1e636ffbcccf"
