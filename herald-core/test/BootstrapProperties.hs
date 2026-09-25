module BootstrapProperties
  ( tests,
  )
where

import Control.Monad (foldM, forM_)
import Data.Set qualified as Set
import Eclips.Application.Types.Identity
  ( privateProcessUniqueId,
    privateUniqueIdWord64,
  )
import Eclips.Domain.Disappearance (deriveDisappearanceProbeId)
import Eclips.Domain.Identity (BootstrapManifestId, controlIndex, nablaSequence, publicationId)
import Eclips.Domain.Publication (checkedPublicationId, checkedPublicationSort, checkedPublicationValue, checkedPublicationWinnerKey, mkCheckedPublication)
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    AppliedRootRole (..),
    appliedProcessEnvironmentObjects,
    appliedProcessEnvironmentPublications,
    appliedProcessEpochId,
    appliedProcessResidence,
    appliedProcessRoots,
    appliedRootAuthority,
    appliedRootObjectId,
    appliedRootRole,
    appliedRootSortId,
    predefinedCatalogueDescriptor,
    predefinedCatalogueSortId,
    profilePredefinedCatalogue,
  )
import Eclips.Domain.Store (storedPublication, visibleInstances)
import Eclips.Domain.StructuralConsequence (predefinedDisappearanceCause)
import Eclips.Herald.Application.PrivateIdentity
  ( processIdentityWitnesses,
    witnessedBindings,
    witnessedNextPrivateUniqueId,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Bootstrap
  ( BootstrapAccess,
    BootstrapInvariant,
    BootstrapOwners,
    BootstrapViews,
    bootstrapApplicationState,
    bootstrapControlledState,
    bootstrapGraphState,
    bootstrapPlacementState,
    bootstrapStoreState,
    bootstrapViews,
    commitProcessBootstrap,
    initialBootstrapOwners,
    installSystemViewRouting,
    observeCanonicalBootstrapObjects,
    prepareProcessBootstrap,
  )
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Genesis
  ( CheckedInitialBootstraps,
    PrimordialProcessManifest (..),
    checkInitialBootstraps,
  )
import Eclips.Herald.Genesis.Internal (checkedInitialBootstraps, checkedSystemId)
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Store.State qualified as Store
import GenesisFixtures
  ( fixtureCheckedGenesis,
    fixtureLocalBootstrapIds,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (Property, property, testProperty)

tests :: TestTree
tests =
  testGroup
    "checked process bootstrap"
    [ testCase "canonical aliases and real leaves install atomically" caseCanonicalBootstrap,
      testCase "normal possession is qualified by process epoch" caseProcessScopedPossession,
      testCase "every environment contains its own thirty-one ordinary descriptions" caseEnvironmentDescriptions,
      testCase "late genesis grants merge peer updates without reviving removed objects" caseLateGenesisObservation,
      testProperty "fixture presentation order cannot change startup" propCanonicalFixtureOrder
    ]

caseCanonicalBootstrap :: IO ()
caseCanonicalBootstrap = do
  let (bootstraps, owners, accesses) = startup fixtureLocalBootstrapIds
      application = bootstrapApplicationState owners
      witnesses =
        processIdentityWitnesses (Application.applicationPrivateIdentity application)
  assertEqual "two selected processes are live" 2 (length bootstraps)
  assertEqual "one identity map per process" 2 (length witnesses)
  assertEqual
    "every map contains process plus thirty-one environment objects"
    [32, 32]
    (fmap (length . witnessedBindings) witnesses)
  assertEqual
    "each independent map starts at one and advances to thirty-three"
    [33, 33]
    (fmap (privateUniqueIdWord64 . witnessedNextPrivateUniqueId) witnesses)
  assertEqual
    "both processes independently localize their process object as one"
    [1, 1]
    ( fmap
        ( privateUniqueIdWord64
            . privateProcessUniqueId
            . Application.bootstrapAccessProcess
        )
        accesses
    )
  assertEqual
    "each process retains twelve typed root handles"
    [12, 12]
    (fmap (length . Application.bootstrapAccessRoots) accesses)
  assertEqual
    "two process graphs plus six system-view vertices"
    32
    (length (Graph.graphVertices (bootstrapGraphState owners)))
  assertEqual
    "application and system-view edges for both processes"
    48
    (length (Graph.graphEdges (bootstrapGraphState owners)))
  assertEqual
    "every reader root owns one complete placement"
    12
    (length (Placement.localPlacements (bootstrapPlacementState owners)))
  assertEqual
    "each private system view owns one placement"
    6
    (length (Placement.systemViewPlacements (bootstrapPlacementState owners)))
  assertEqual
    "six system views and every reader root own one seeded typed store"
    18
    (length (Store.storeSlots (bootstrapStoreState owners)))
  assertEqual
    "the process bootstrap added exactly its twelve application stores"
    12
    ( length
        [ ()
        | slot <- Store.storeSlots (bootstrapStoreState owners),
          Store.ApplicationReader {} <- [Store.storeSlotProvenance slot]
        ]
    )

caseProcessScopedPossession :: IO ()
caseProcessScopedPossession = do
  let (bootstraps, owners, _) = startup fixtureLocalBootstrapIds
      controlled = bootstrapControlledState owners
  case bootstraps of
    first : second : _ -> case appliedProcessRoots first of
      firstRoot : _ -> do
        let firstProcess = appliedProcessEpochId first
            secondProcess = appliedProcessEpochId second
            object = appliedRootObjectId firstRoot
        assertBool
          "the owning process normally possesses its root"
          (Controlled.controlledHasNormalPossession firstProcess object controlled)
        assertBool
          "another process does not inherit that possession"
          (not (Controlled.controlledHasNormalPossession secondProcess object controlled))
      [] -> assertFailure "fixture process did not provide canonical roots"
    _ -> assertFailure "fixture did not provide two local bootstraps"

caseEnvironmentDescriptions :: IO ()
caseEnvironmentDescriptions = do
  let (bootstraps, owners, _) = startup fixtureLocalBootstrapIds
      controlled = bootstrapControlledState owners
      stores = bootstrapStoreState owners
  assertEqual "seeded store histories replay exactly" (Right ()) (Store.validateStoreHistoryState stores)
  forM_ bootstraps $ \bootstrap -> do
    let process = appliedProcessEpochId bootstrap
        publications = appliedProcessEnvironmentPublications (checkedSystemId fixtureCheckedGenesis) bootstraps bootstrap
    forM_ (appliedProcessEnvironmentObjects bootstrap) $ \object -> do
      assertBool "each description has an ordinary controlled winner" (maybe False ((== Controlled.ControlledCurrent) . Controlled.controlledRecordLifecycle) (Controlled.controlledLocalRecord object controlled))
      assertBool "the owner normally possesses every manifest object" (Controlled.controlledHasNormalPossession process object controlled)
      forM_ [appliedProcessEpochId other | other <- bootstraps, appliedProcessEpochId other /= process] $ \other ->
        assertBool "independent environments do not inherit each other's descriptions" (not (Controlled.controlledHasNormalPossession other object controlled))
    forM_ (appliedProcessRoots bootstrap) $ \root -> case appliedRootRole root of
      WriterRoot {} -> pure ()
      ReaderRoot delta -> do
        slot <- maybe (assertFailure "environment reader store missing") pure (Store.lookupStoreSlot delta stores)
        let expected = Set.fromList [checkedPublicationId publication | (_, _, publication) <- publications, checkedPublicationSort publication == appliedRootSortId root]
            actual = Set.fromList [checkedPublicationId (storedPublication value) | (_, value) <- visibleInstances (Store.storeSlotContents slot)]
        assertBool "the environment's own matching descriptions are readable" (expected `Set.isSubsetOf` actual)

caseLateGenesisObservation :: IO ()
caseLateGenesisObservation = do
  let (bootstraps, _, _) = startup fixtureLocalBootstrapIds
      system = checkedSystemId fixtureCheckedGenesis
      cause = checked "disappearance cause" (predefinedDisappearanceCause (checked "disappearance probe" (deriveDisappearanceProbeId (controlIndex 1))) (controlIndex 2))
  forM_ bootstraps $ \bootstrap ->
    forM_ (appliedProcessEnvironmentPublications system bootstraps bootstrap) $ \(_, occurrence, genesisPublication) -> do
      let descriptor = case [predefinedCatalogueDescriptor entry | entry <- profilePredefinedCatalogue, predefinedCatalogueSortId entry == checkedPublicationSort genesisPublication] of
            entry : _ -> entry
            [] -> error "bootstrap carrier descriptor missing"
          source = case [root | root <- appliedProcessRoots bootstrap, appliedRootSortId root == checkedPublicationSort genesisPublication, WriterRoot {} <- [appliedRootRole root]] of
            root : _ -> root
            [] -> error "bootstrap carrier writer missing"
          writer = case appliedRootRole source of
            WriterRoot nabla _ -> nabla
            ReaderRoot {} -> error "selected a reader as publication source"
          updated = checked "ordinary writer update" (mkCheckedPublication descriptor (publicationId writer (appliedRootAuthority source) (appliedProcessResidence bootstrap) (nablaSequence 1)) (checkedPublicationValue genesisPublication))
          observation = checked "ordinary update observation" (Controlled.checkControlledObservation descriptor occurrence updated)
          object = Controlled.checkedControlledObservationObject observation
          peer = fst (Controlled.commitControlledPeerObservation (checked "peer update arrives first" (Controlled.prepareControlledPeerObservation observation Controlled.emptyState)))
          observe = checked "late canonical genesis observation" . observeCanonicalBootstrapObjects system bootstraps (Set.singleton object)
          merged = observe peer
          record = maybe (error "merged controlled record missing") id (Controlled.controlledLocalRecord object merged)
      assertEqual
        "ordinary winner ordering survives delayed genesis evidence"
        (max (checkedPublicationWinnerKey genesisPublication) (checkedPublicationWinnerKey updated))
        (checkedPublicationWinnerKey (Controlled.controlledRecordLatestPublication record))
      assertEqual
        "the original genesis evidence is retained"
        (Just genesisPublication)
        (Controlled.controlledRecordObservation (checkedPublicationId genesisPublication) record)
      assertBool
        "remote metadata does not grant the historical owner possession"
        (not (Controlled.controlledHasNormalPossession (appliedProcessEpochId bootstrap) object merged))
      assertBool "an exact delayed grant is idempotent" (merged == observe merged)
      let removed = Controlled.commitControlledRemoval (checked "terminal removal" (Controlled.prepareControlledRemoval cause object merged))
      assertBool "late genesis evidence cannot revive terminally removed objects" (removed == observe removed)
      assertBool "the removed bootstrap alias remains unavailable" (not (Controlled.controlledObjectCurrent object (observe removed)))

propCanonicalFixtureOrder :: Bool -> Property
propCanonicalFixtureOrder reversePresentation =
  let presented =
        if reversePresentation
          then reverse fixtureLocalBootstrapIds
          else fixtureLocalBootstrapIds
      (_, owners, accesses) = startup presented
      (_, canonicalOwners, canonicalAccesses) = startup fixtureLocalBootstrapIds
   in property ((owners, accesses) == (canonicalOwners, canonicalAccesses))

startup ::
  [BootstrapManifestId] ->
  ([AppliedProcessBootstrap], BootstrapOwners, [BootstrapAccess])
startup identifiers =
  let checkedBootstraps = checkedInitial identifiers
      bootstraps = checkedInitialBootstraps checkedBootstraps
      sortRegistry = SortRegistry.initialState fixtureCheckedGenesis
      oracleProjection =
        OracleProjection.initialState fixtureCheckedGenesis checkedBootstraps
      views =
        bootstrapViews
          (OracleProjection.oracleView oracleProjection)
          (SortRegistry.sortView sortRegistry)
      owners =
        checked
          "construct initial system-view stores"
          (initialBootstrapOwners fixtureCheckedGenesis)
      (processOwners, accesses) =
        checked
          "install checked process bootstraps"
          (foldM (installOne views) (owners, []) bootstraps)
      successor =
        checked
          "install private system-view routing"
          ( installSystemViewRouting
              (OracleProjection.oracleViewLocalHeraldEpoch (OracleProjection.oracleView oracleProjection))
              processOwners
          )
   in (bootstraps, successor, reverse accesses)

installOne ::
  BootstrapViews ->
  (BootstrapOwners, [BootstrapAccess]) ->
  AppliedProcessBootstrap ->
  Either BootstrapInvariant (BootstrapOwners, [BootstrapAccess])
installOne views (owners, accesses) bootstrap = do
  prepared <- prepareProcessBootstrap views bootstrap owners
  let (successor, access) = commitProcessBootstrap prepared
  Right (successor, access : accesses)

checkedInitial :: [BootstrapManifestId] -> CheckedInitialBootstraps
checkedInitial identifiers =
  checked
    "check initial bootstrap selection"
    (checkInitialBootstraps fixtureCheckedGenesis (PrimordialProcessManifest identifiers))

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id
