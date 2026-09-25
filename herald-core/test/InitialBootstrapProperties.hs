module InitialBootstrapProperties
  ( tests,
  )
where

import Data.List (sort)
import Eclips.Domain.Identity
  ( AuthorityEpochView (GenesisAuthorityEpochView),
    BootstrapManifestId,
    authorityEpochView,
    controlIndex,
    globalObjectIdBytes,
    mkBootstrapManifestId,
    mkHeraldEpoch,
    processEpochIdBytes,
  )
import Eclips.Domain.Startup
  ( ConfiguredProcessMaterializationError (ConfiguredProcessMaterializationRequiresGenesis),
    deriveInitialProjectionDigest,
    predefinedOccurrenceFor,
  )
import Eclips.Herald.Genesis.Internal
  ( AppliedProcessBootstrap,
    AppliedRootRole (..),
    BootstrapFault (..),
    CheckedInitialBootstraps,
    ConfiguredProcessManifest (..),
    DeploymentManifest (..),
    InitialTopologyEdgeManifest (..),
    InitialTopologyError (..),
    InitialTopologyManifest (..),
    PredefinedSortRole (..),
    PrimordialProcessManifest (..),
    appliedProcessAuthority,
    appliedProcessEpochId,
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
    checkHeraldGenesis,
    checkInitialBootstraps,
    checkInitialBootstrapsWithTopology,
    checkedConfiguredProcessBootstraps,
    checkedInitialBootstraps,
    checkedInitialProjectionDigest,
    checkedInitialTopologyEdges,
    checkedInitialTopologyProjection,
    checkedInitialTopologyVertices,
    checkedPredefinedOccurrenceSet,
    configuredProcessBootstrapManifestId,
    heraldMemberEpoch,
    lookupConfiguredProcessBootstrap,
    materializeConfiguredProcessBootstrap,
  )
import GenesisFixtures
  ( fixtureCheckedGenesis,
    fixtureConfiguredProcesses,
    fixtureDeploymentAt,
    fixtureDeploymentManifest,
    fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureLocalMember,
    fixtureRemoteBootstrapId,
    fixtureRemoteMember,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (Property, testProperty, (===))

tests :: TestTree
tests =
  testGroup
    "initial process bootstrap fixture"
    [ testCase "an empty initial fixture is valid" caseEmpty,
      testProperty "reference presentation order cannot change checked bootstraps" propReferenceOrder,
      testCase "the initial projection digest canonically binds the full applied set" caseProjectionDigest,
      testCase "symbolic topology is resolved into the common checked projection" caseSymbolicTopology,
      testProperty "topology manifest presentation order cannot change projection identity" propTopologyOrder,
      testCase "invalid symbolic topology is rejected at startup" caseTopologyRejection,
      testCase "duplicates are rejected" caseDuplicate,
      testCase "unknown references are rejected" caseUnknown,
      testCase "two simultaneous live epochs cannot share a stable process id" caseDuplicateLiveProcessId,
      testCase "the global applied fixture is equal at every Herald" caseGlobalAppliedFixture,
      testCase "configured but unapplied processes remain in genesis" caseConfiguredButUnapplied,
      testCase "configured materialization is restricted to genesis" caseGenesisMaterialization,
      testCase "process objects preserve exact process-epoch bits" caseProcessObjectBits,
      testCase "roots use catalogue then writer/reader canonical order" caseCanonicalRootOrder,
      testCase "root metadata comes from checked primordial facts" caseRootMetadata
    ]

caseEmpty :: IO ()
caseEmpty = do
  checkedSet <-
    checked
      "empty initial fixture"
      (checkInitialBootstraps fixtureCheckedGenesis (PrimordialProcessManifest []))
  assertEqual "no applied bootstrap" [] (checkedInitialBootstraps checkedSet)
  assertEqual
    "six views and five isolated bootstrap writers for each active Herald"
    22
    (length (checkedInitialTopologyVertices (checkedInitialTopologyProjection checkedSet)))
  assertEqual
    "no writer means no base edge"
    []
    (checkedInitialTopologyEdges (checkedInitialTopologyProjection checkedSet))

propReferenceOrder :: Bool -> Property
propReferenceOrder reverseInput =
  checkInitialBootstraps fixtureCheckedGenesis input
    === checkInitialBootstraps fixtureCheckedGenesis canonical
  where
    identifiers = if reverseInput then reverse fixtureLocalBootstrapIds else fixtureLocalBootstrapIds
    input = PrimordialProcessManifest identifiers
    canonical = PrimordialProcessManifest fixtureLocalBootstrapIds

caseProjectionDigest :: IO ()
caseProjectionDigest = case fixtureLocalBootstrapIds of
  first : second : _ -> do
    empty <- checkedInitialProjection []
    selected <- checkedInitialProjection [first]
    canonical <- checkedInitialProjection [first, second]
    reordered <- checkedInitialProjection [second, first]
    assertBool "different applied sets have different identities" (empty /= selected)
    assertEqual "reference order does not affect the identity" canonical reordered
    checkedSet <-
      either
        (fail . show)
        pure
        (checkInitialBootstraps fixtureCheckedGenesis (PrimordialProcessManifest [first, second]))
    assertEqual
      "the stored identity covers the full canonical applied records"
      ( deriveInitialProjectionDigest
          (checkedInitialBootstraps checkedSet)
          (checkedInitialTopologyProjection checkedSet)
      )
      (checkedInitialProjectionDigest checkedSet)
  _ -> fail "fixture has fewer than two local bootstraps"
  where
    checkedInitialProjection identifiers =
      either
        (fail . show)
        (pure . checkedInitialProjectionDigest)
        (checkInitialBootstraps fixtureCheckedGenesis (PrimordialProcessManifest identifiers))

caseSymbolicTopology :: IO ()
caseSymbolicTopology = case fixtureLocalBootstrapIds of
  source : _ -> do
    checkedSet <- checked "cross-Herald topology" (checkedCrossTopology [source, fixtureRemoteBootstrapId] crossManifest)
    let topology = checkedInitialTopologyProjection checkedSet
        withoutManifest =
          checkInitialBootstraps
            fixtureCheckedGenesis
            (PrimordialProcessManifest [source, fixtureRemoteBootstrapId])
    assertEqual
      "two environment graphs, twelve system views, and ten hidden writers"
      48
      (length (checkedInitialTopologyVertices topology))
    assertEqual "forty-eight base edges plus two cross edges" 50 (length (checkedInitialTopologyEdges topology))
    base <- checked "base topology" withoutManifest
    assertBool
      "checked manifest changes the expanded digest"
      (checkedInitialProjectionDigest checkedSet /= checkedInitialProjectionDigest base)
  [] -> fail "fixture has no source bootstrap"

propTopologyOrder :: Bool -> Property
propTopologyOrder reverseInput = case fixtureLocalBootstrapIds of
  source : _ ->
    let identifiers = [source, fixtureRemoteBootstrapId]
        manifest = if reverseInput then reverseTopologyManifest crossManifest else crossManifest
        reversedManifest = reverseTopologyManifest manifest
     in checkedCrossTopology identifiers manifest
          === checkedCrossTopology (reverse identifiers) reversedManifest
  [] -> error "fixture has no source bootstrap"

caseTopologyRejection :: IO ()
caseTopologyRejection = case fixtureLocalBootstrapIds of
  source : _ -> do
    unknown <- checked "unknown topology bootstrap" (mkBootstrapManifestId (fixtureIdentifierBytes 200))
    inactive <- checked "inactive topology Herald" (mkHeraldEpoch (fixtureIdentifierBytes 201))
    let selected = PrimordialProcessManifest [source, fixtureRemoteBootstrapId]
        duplicateEdge = InitialTopologyProcessEdge source fixtureRemoteBootstrapId SortDefinitionRole
        duplicateManifest = InitialTopologyManifest [duplicateEdge, duplicateEdge]
        baseEdge = InitialTopologyProcessEdge source source SortDefinitionRole
    assertEqual
      "duplicate symbolic edge"
      (Left (InitialTopologyRejected (InitialTopologyDuplicateManifestEdge duplicateEdge)))
      (checkInitialBootstrapsWithTopology fixtureCheckedGenesis selected duplicateManifest)
    assertEqual
      "an explicit edge cannot bypass the ordinary environment wiring"
      (Left (InitialTopologyRejected (InitialTopologyEdgeAlreadyPresent baseEdge)))
      (checkInitialBootstrapsWithTopology fixtureCheckedGenesis selected (InitialTopologyManifest [baseEdge]))
    assertEqual
      "source bootstrap must be selected"
      (Left (InitialTopologyRejected (InitialTopologySourceBootstrapNotSelected unknown)))
      ( checkInitialBootstrapsWithTopology
          fixtureCheckedGenesis
          selected
          (InitialTopologyManifest [InitialTopologyProcessEdge unknown fixtureRemoteBootstrapId SortDefinitionRole])
      )
    assertEqual
      "destination bootstrap must be selected"
      (Left (InitialTopologyRejected (InitialTopologyDestinationBootstrapNotSelected unknown)))
      ( checkInitialBootstrapsWithTopology
          fixtureCheckedGenesis
          selected
          (InitialTopologyManifest [InitialTopologyProcessEdge source unknown SortDefinitionRole])
      )
    assertEqual
      "system-view Herald must be active"
      (Left (InitialTopologyRejected (InitialTopologyDestinationHeraldNotActive inactive)))
      ( checkInitialBootstrapsWithTopology
          fixtureCheckedGenesis
          selected
          (InitialTopologyManifest [InitialTopologySystemViewEdge source inactive SortDefinitionRole])
      )
  [] -> fail "fixture has no source bootstrap"

crossManifest :: InitialTopologyManifest
crossManifest = case fixtureLocalBootstrapIds of
  source : _ ->
    InitialTopologyManifest
      [ InitialTopologyProcessEdge source fixtureRemoteBootstrapId SortDefinitionRole,
        InitialTopologySystemViewEdge
          source
          (heraldMemberEpoch fixtureRemoteMember)
          SortDefinitionRole
      ]
  [] -> error "fixture has no source bootstrap"

checkedCrossTopology ::
  [BootstrapManifestId] ->
  InitialTopologyManifest ->
  Either BootstrapFault CheckedInitialBootstraps
checkedCrossTopology identifiers =
  checkInitialBootstrapsWithTopology
    fixtureCheckedGenesis
    (PrimordialProcessManifest identifiers)

reverseTopologyManifest :: InitialTopologyManifest -> InitialTopologyManifest
reverseTopologyManifest (InitialTopologyManifest edges) = InitialTopologyManifest (reverse edges)

caseDuplicate :: IO ()
caseDuplicate = case fixtureLocalBootstrapIds of
  first : _ ->
    assertEqual
      "typed duplicate"
      (Left (DuplicateBootstrapManifest first))
      (checkInitialBootstraps fixtureCheckedGenesis (PrimordialProcessManifest [first, first]))
  [] -> fail "fixture has no local bootstrap"

caseUnknown :: IO ()
caseUnknown = do
  unknown <- checked "BootstrapManifestId" (mkBootstrapManifestId (fixtureIdentifierBytes 200))
  assertEqual
    "typed unknown reference"
    (Left (UnknownBootstrapManifest unknown))
    (checkInitialBootstraps fixtureCheckedGenesis (PrimordialProcessManifest [unknown]))

caseDuplicateLiveProcessId :: IO ()
caseDuplicateLiveProcessId = case fixtureConfiguredProcesses of
  first : second : remaining -> do
    genesis <-
      either
        (fail . show)
        pure
        ( checkHeraldGenesis
            fixtureDeploymentManifest
              { deploymentConfiguredProcesses =
                  first
                    : second {configuredProcessId = configuredProcessId first}
                    : remaining
              }
        )
    assertEqual
      "typed simultaneous-live duplicate"
      (Left (DuplicateLiveProcessId (configuredProcessId first)))
      ( checkInitialBootstraps
          genesis
          (PrimordialProcessManifest fixtureLocalBootstrapIds)
      )
  _ -> fail "fixture has fewer than two configured processes"

caseGlobalAppliedFixture :: IO ()
caseGlobalAppliedFixture = do
  remoteGenesis <- either (fail . show) pure (checkHeraldGenesis (fixtureDeploymentAt fixtureRemoteMember))
  let globalIds = fixtureRemoteBootstrapId : fixtureLocalBootstrapIds
      manifest = PrimordialProcessManifest globalIds
      localChecked = checkInitialBootstraps fixtureCheckedGenesis manifest
      remoteChecked = checkInitialBootstraps remoteGenesis manifest
  assertEqual "same applied projection" localChecked remoteChecked
  applied <- either (fail . show) (pure . checkedInitialBootstraps) localChecked
  assertBool
    "remote residence remains in the global set"
    (heraldMemberEpoch fixtureRemoteMember `elem` fmap appliedProcessResidence applied)

caseConfiguredButUnapplied :: IO ()
caseConfiguredButUnapplied = do
  let allConfigured = checkedConfiguredProcessBootstraps fixtureCheckedGenesis
  assertBool
    "remote configuration retained"
    (fixtureRemoteBootstrapId `elem` fmap configuredProcessBootstrapManifestId allConfigured)
  assertEqual
    "empty fixture still applies none"
    (Right [])
    (checkedInitialBootstraps <$> checkInitialBootstraps fixtureCheckedGenesis (PrimordialProcessManifest []))

caseGenesisMaterialization :: IO ()
caseGenesisMaterialization = case fixtureLocalBootstrapIds of
  first : _ -> case lookupConfiguredProcessBootstrap first fixtureCheckedGenesis of
    Nothing -> fail "local fixture template missing"
    Just configured -> do
      applied <-
        checked
          "configured genesis materialization"
          ( materializeConfiguredProcessBootstrap
              (controlIndex 0)
              (checkedPredefinedOccurrenceSet fixtureCheckedGenesis)
              configured
          )
      assertEqual
        "process authority"
        GenesisAuthorityEpochView
        (authorityEpochView (appliedProcessAuthority applied))
      mapM_
        ( \root -> do
            assertEqual
              "root authority"
              GenesisAuthorityEpochView
              (authorityEpochView (appliedRootAuthority root))
            assertEqual "root prerequisite" (controlIndex 0) (appliedRootControlPrerequisite root)
        )
        (appliedProcessRoots applied)
      let nonGenesisIndex = controlIndex 7
      assertEqual
        "a dynamic Start cannot fabricate authority from a control index"
        (Left (ConfiguredProcessMaterializationRequiresGenesis nonGenesisIndex))
        ( materializeConfiguredProcessBootstrap
            nonGenesisIndex
            (checkedPredefinedOccurrenceSet fixtureCheckedGenesis)
            configured
        )
  [] -> fail "fixture has no local bootstrap"

caseProcessObjectBits :: IO ()
caseProcessObjectBits = do
  bootstraps <- localBootstraps
  mapM_
    ( \bootstrap ->
        assertEqual
          "same bits"
          (processEpochIdBytes (appliedProcessEpochId bootstrap))
          (globalObjectIdBytes (appliedProcessObjectId bootstrap))
    )
    bootstraps

caseCanonicalRootOrder :: IO ()
caseCanonicalRootOrder = do
  bootstraps <- localBootstraps
  let expectedRoles = concatMap (\role -> [role, role]) allRoles
  mapM_
    ( \bootstrap -> do
        let roots = appliedProcessRoots bootstrap
        assertEqual "complete catalogue role order" expectedRoles (fmap appliedRootCatalogueRole roots)
        mapM_ assertPair (pairs roots)
    )
    bootstraps
  where
    assertPair (writer, reader) = do
      assertBool "writer role" (case appliedRootRole writer of WriterRoot _ _ -> True; ReaderRoot _ -> False)
      assertBool "reader role" (case appliedRootRole reader of ReaderRoot _ -> True; WriterRoot _ _ -> False)
      assertEqual "writer has no placement" Nothing (appliedRootPlacement writer)
      assertBool "reader has local placement" (case appliedRootPlacement reader of Just (residence, _) -> residence == heraldMemberEpoch fixtureLocalMember; Nothing -> False)

caseRootMetadata :: IO ()
caseRootMetadata = do
  bootstraps <- localBootstraps
  mapM_
    ( \bootstrap ->
        mapM_
          ( \(writer, reader) -> do
              assertEqual "paired sort" (appliedRootSortId writer) (appliedRootSortId reader)
              assertEqual "paired occurrence" (appliedRootOccurrenceId writer) (appliedRootOccurrenceId reader)
              assertEqual
                "genesis occurrence materialisation"
                ( predefinedOccurrenceFor
                    (appliedRootCatalogueRole writer)
                    (checkedPredefinedOccurrenceSet fixtureCheckedGenesis)
                )
                (appliedRootOccurrenceId writer)
              assertEqual "index zero writer prerequisite" (controlIndex 0) (appliedRootControlPrerequisite writer)
              assertEqual "index zero reader prerequisite" (controlIndex 0) (appliedRootControlPrerequisite reader)
          )
          (pairs (appliedProcessRoots bootstrap))
    )
    bootstraps

localBootstraps :: IO [AppliedProcessBootstrap]
localBootstraps =
  either
    (fail . show)
    (pure . checkedInitialBootstraps)
    ( checkInitialBootstraps
        fixtureCheckedGenesis
        (PrimordialProcessManifest fixtureLocalBootstrapIds)
    )

pairs :: [value] -> [(value, value)]
pairs [] = []
pairs (first : second : remaining) = (first, second) : pairs remaining
pairs [_] = error "checked canonical root list has odd length"

allRoles :: [PredefinedSortRole]
allRoles = sort [minBound .. maxBound]

checked :: (Show problem) => String -> Either problem value -> IO value
checked context = either (fail . ((context <> ": ") <>) . show) pure
