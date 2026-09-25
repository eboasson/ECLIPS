{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Atomic cross-leaf installation of one already applied process bootstrap.
module Eclips.Herald.Bootstrap
  ( BootstrapViews,
    bootstrapViews,
    BootstrapOwners,
    initialBootstrapOwners,
    bootstrapApplicationState,
    bootstrapControlledState,
    bootstrapGraphState,
    bootstrapPlacementState,
    bootstrapStoreState,
    SystemViewRoutingInvariant (..),
    installSystemViewRouting,
    processControlledFact,
    controlledRootFact,
    observeCanonicalBootstrapObjects,
    BootstrapAccess,
    BootstrapInvariant (..),
    PreparedBootstrap,
    prepareProcessBootstrap,
    commitProcessBootstrap,
  )
where

import Control.Monad (foldM, unless)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    NablaId,
    ProcessEpochId,
    SortDefinitionOccurrenceId,
    SystemId,
    globalObjectIdFromProcessEpochId,
  )
import Eclips.Domain.Publication (CheckedPublication, checkedPublicationSort)
import Eclips.Domain.Route (ReplicaStrength (Normal))
import Eclips.Domain.Sort.Descriptor (StructuralCarrierRole (..))
import Eclips.Domain.Sort.Profile (PredefinedSortRole (..), predefinedCatalogueDescriptor, profileEntryFor)
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    AppliedRoot,
    AppliedRootRole (..),
    appliedBootstrapManifestId,
    appliedProcessAuthority,
    appliedProcessEnvironmentEdges,
    appliedProcessEnvironmentHub,
    appliedProcessEnvironmentPublications,
    appliedProcessEpochId,
    appliedProcessId,
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
  )
import Eclips.Domain.Store (StoreError)
import Eclips.Herald.Application.Session.Internal qualified as ApplicationSession
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    checkedLocalHeraldEpoch,
  )
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
  )
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Store.State qualified as Store

-- | Immutable authority and effective-sort projections read by bootstrap.
data BootstrapViews = BootstrapViews
  { oracleProjection :: OracleProjection.View,
    sortRegistry :: SortRegistry.View
  }
  deriving stock (Eq)

bootstrapViews ::
  OracleProjection.View ->
  SortRegistry.View ->
  BootstrapViews
bootstrapViews = BootstrapViews

-- | Exactly the real owner states changed by process bootstrap.
--
-- Oracle projection is deliberately absent: the bootstrap must already be in
-- the applied projection supplied through 'BootstrapViews'.  Sort registry is
-- also absent because configured roots only reference effective primordial
-- definitions; they do not create definitions.
data BootstrapOwners = BootstrapOwners
  { application :: Application.State,
    controlled :: Controlled.State,
    graph :: Graph.State,
    placement :: Placement.State,
    store :: Store.State
  }
  deriving stock (Eq)

-- | Empty process-owned leaves plus the six already-seeded Herald-private
-- system-view stores constructed from checked genesis.
initialBootstrapOwners ::
  CheckedHeraldGenesis ->
  Either Store.StorePreparationError BootstrapOwners
initialBootstrapOwners genesis = do
  initialStore <- Store.initialState genesis
  pure
    BootstrapOwners
      { application = Application.emptyState,
        controlled = Controlled.emptyState,
        graph = Graph.emptyState,
        placement = Placement.initialState (checkedLocalHeraldEpoch genesis),
        store = initialStore
      }

bootstrapApplicationState :: BootstrapOwners -> Application.State
bootstrapApplicationState owners = owners.application

bootstrapControlledState :: BootstrapOwners -> Controlled.State
bootstrapControlledState owners = owners.controlled

bootstrapGraphState :: BootstrapOwners -> Graph.State
bootstrapGraphState owners = owners.graph

bootstrapPlacementState :: BootstrapOwners -> Placement.State
bootstrapPlacementState owners = owners.placement

bootstrapStoreState :: BootstrapOwners -> Store.State
bootstrapStoreState owners = owners.store

-- | Install the first-data-vertical operational projection for the six real
-- private system-view stores after every resident process bootstrap is present.
-- This adds graph and placement facts only; it does not fabricate controlled
-- ownership or application access.
installSystemViewRouting ::
  HeraldEpoch ->
  BootstrapOwners ->
  Either SystemViewRoutingInvariant BootstrapOwners
installSystemViewRouting localHerald owners = do
  graphPrepared <-
    either
      (Left . SystemViewGraphContradiction)
      Right
      ( Graph.prepareGraphSystemViews
          (fmap (Store.storeSlotDelta . snd) systemViews)
          writerViewPairs
          owners.graph
      )
  placementPrepared <-
    either
      (Left . SystemViewPlacementContradiction)
      Right
      ( Placement.prepareSystemViewPlacements
          (fmap systemViewPlacement systemViews)
          owners.placement
      )
  Right
    owners
      { graph = Graph.commitGraphSystemViews graphPrepared,
        placement = Placement.commitSystemViewPlacements placementPrepared
      }
  where
    systemViews =
      [ (role, slot)
      | slot <- Store.storeSlots owners.store,
        Store.HeraldSystemView herald role <- [Store.storeSlotProvenance slot],
        herald == localHerald
      ]
    writers =
      [ (Controlled.rootFactCatalogueRole fact, nabla)
      | fact <- Controlled.controlledRootFacts owners.controlled,
        Controlled.ControlledWriter nabla _ <- [Controlled.rootFactRole fact]
      ]
    writerViewPairs =
      [ (nabla, Store.storeSlotDelta slot)
      | (role, nabla) <- writers,
        (viewRole, slot) <- systemViews,
        role == viewRole
      ]
    systemViewPlacement (role, slot) =
      Placement.systemViewPlacement
        role
        (Store.storeSlotDelta slot)
        (Store.storeSlotSortId slot)
        (Store.storeSlotOccurrenceId slot)
        localHerald
        (Store.storeSlotIncarnation slot)

-- | The only two owner boundaries touched by private system-view projection.
data SystemViewRoutingInvariant
  = SystemViewGraphContradiction Graph.GraphBootstrapError
  | SystemViewPlacementContradiction Placement.PlacementBootstrapError
  deriving stock (Eq, Show)

type BootstrapAccess = Application.BootstrapAccess

-- | An observed contradiction between opaque checked input and one or more
-- single-owner startup projections.  There is no retry or recovery transition
-- for this impossible-state fault in profile 0.1.
data BootstrapInvariant
  = BootstrapOracleProjectionContradiction ProcessEpochId
  | BootstrapResidenceContradiction
      BootstrapManifestId
      HeraldEpoch
      HeraldEpoch
  | BootstrapSortMissing PredefinedSortRole
  | BootstrapSortContradiction PredefinedSortRole
  | BootstrapOccurrenceContradiction PredefinedSortRole
  | BootstrapControlPrerequisiteContradiction PredefinedSortRole
  | BootstrapApplicationContradiction Application.ApplicationBootstrapError
  | BootstrapControlledContradiction Controlled.ControlledBootstrapError
  | BootstrapGraphContradiction Graph.GraphBootstrapError
  | BootstrapPlacementContradiction Placement.PlacementBootstrapError
  | BootstrapStoreContradiction Store.StoreBootstrapError
  | BootstrapStoreInvariant StoreError
  | BootstrapControlledObservationContradiction Controlled.ControlledObservationError
  | BootstrapStoreObservationContradiction Store.StoreObservationProblem
  | BootstrapStoreSeedContradiction Store.StoreTransitionError
  deriving stock (Eq, Show)

-- | Complete successor product and stable application access.  Its constructor
-- stays private, so commit cannot be redirected to another predecessor.
newtype PreparedBootstrap
  = PreparedBootstrap (Prepared BootstrapOwners BootstrapAccess)

prepareProcessBootstrap ::
  BootstrapViews ->
  AppliedProcessBootstrap ->
  BootstrapOwners ->
  Either BootstrapInvariant PreparedBootstrap
prepareProcessBootstrap views bootstrap owners =
  PreparedBootstrap
    <$> prepareTransition (installProcessBootstrap views bootstrap) owners

commitProcessBootstrap ::
  PreparedBootstrap ->
  (BootstrapOwners, BootstrapAccess)
commitProcessBootstrap (PreparedBootstrap prepared) = commitPrepared prepared

installProcessBootstrap ::
  BootstrapViews ->
  AppliedProcessBootstrap ->
  BootstrapOwners ->
  Either BootstrapInvariant (BootstrapOwners, BootstrapAccess)
installProcessBootstrap views bootstrap owners = do
  validateBootstrap views bootstrap
  applicationPrepared <-
    either
      (Left . BootstrapApplicationContradiction)
      Right
      ( Application.prepareApplicationBootstrap
          ( ApplicationSession.applicationAttachmentForBootstrap
              (appliedBootstrapManifestId bootstrap)
          )
          process
          (fmap applicationRoot roots)
          (appliedProcessEnvironmentHub bootstrap)
          (appliedProcessEnvironmentEdges bootstrap)
          owners.application
      )
  controlledPrepared <-
    either
      (Left . BootstrapControlledContradiction)
      Right
      ( Controlled.prepareControlledBootstrap
          (processControlledFact bootstrap)
          (fmap (controlledRootFact process) roots)
          owners.controlled
      )
  graphPrepared <-
    either
      (Left . BootstrapGraphContradiction)
      Right
      (Graph.prepareGraphBootstrap (rootPairs roots) (appliedProcessEnvironmentHub bootstrap) (map snd (appliedProcessEnvironmentEdges bootstrap)) owners.graph)
  placementPrepared <-
    either
      (Left . BootstrapPlacementContradiction)
      Right
      ( Placement.preparePlacementBootstrap
          (mapMaybe (readerPlacement bootstrap) roots)
          owners.placement
      )
  storePrepared <-
    either
      (Left . storePreparationFault)
      Right
      ( Store.prepareStoreBootstrap
          (mapMaybe (readerStore process) roots)
          owners.store
      )
  let (applicationSuccessor, access) =
        Application.commitApplicationBootstrap applicationPrepared
      publications = appliedProcessEnvironmentPublications (OracleProjection.oracleViewSystemId views.oracleProjection) (OracleProjection.oracleViewAppliedBootstraps views.oracleProjection) bootstrap
  observations <- either (Left . BootstrapControlledObservationContradiction) Right (traverse checkedDescription publications)
  descriptionsPrepared <-
    either
      (Left . BootstrapControlledContradiction)
      Right
      (Controlled.prepareControlledBootstrapDescriptions (Just process) observations (Controlled.commitControlledBootstrap controlledPrepared))
  storeSeeds <- traverse (readerSeeds publications) [root | root <- roots, ReaderRoot {} <- [appliedRootRole root]]
  seededStores <- either (Left . BootstrapStoreSeedContradiction) Right (Store.prepareEnvironmentStoreSeed process storeSeeds (Store.commitStoreBootstrap storePrepared))
  let
    successor =
      BootstrapOwners
        { application = applicationSuccessor,
          controlled =
            Controlled.commitControlledBootstrap descriptionsPrepared,
          graph = Graph.commitGraphBootstrap graphPrepared,
          placement =
            Placement.commitPlacementBootstrap placementPrepared,
          store = Store.commitEnvironmentStoreSeed seededStores
        }
  Right (successor, access)
  where
    process = appliedProcessEpochId bootstrap
    roots = appliedProcessRoots bootstrap
    readerSeeds publications root = case (appliedRootRole root, appliedRootPlacement root) of
      (ReaderRoot delta, Just (_, incarnation)) -> do
        observations <-
          traverse
            (\(_, occurrence, publication) -> either (Left . BootstrapStoreObservationContradiction) Right (Store.retainedStoreObservation publication Normal occurrence Store.PrimordialStoreObservation))
            [entry | entry@(_, _, publication) <- publications, checkedPublicationSort publication == appliedRootSortId root]
        Right (delta, incarnation, observations)
      _ -> error "checked bootstrap reader lacks its store"

validateBootstrap ::
  BootstrapViews ->
  AppliedProcessBootstrap ->
  Either BootstrapInvariant ()
validateBootstrap views bootstrap = do
  unless
    (OracleProjection.oracleViewAuthorizesProcess bootstrap oracleView)
    (Left (BootstrapOracleProjectionContradiction process))
  unless
    (appliedProcessResidence bootstrap == OracleProjection.oracleViewLocalHeraldEpoch oracleView)
    ( Left
        ( BootstrapResidenceContradiction
            (appliedBootstrapManifestId bootstrap)
            (OracleProjection.oracleViewLocalHeraldEpoch oracleView)
            (appliedProcessResidence bootstrap)
        )
    )
  mapM_ (validateRoot views) roots
  where
    oracleView = views.oracleProjection
    process = appliedProcessEpochId bootstrap
    roots = appliedProcessRoots bootstrap

validateRoot ::
  BootstrapViews ->
  AppliedRoot ->
  Either BootstrapInvariant ()
validateRoot views root = do
  entry <-
    maybe
      (Left (BootstrapSortMissing role))
      Right
      (SortRegistry.sortViewPredefinedRole role views.sortRegistry)
  unless
    (appliedRootSortId root == SortRegistry.registryEntrySortId entry)
    (Left (BootstrapSortContradiction role))
  unless
    (appliedRootOccurrenceId root == SortRegistry.registryEntryOccurrenceId entry)
    (Left (BootstrapOccurrenceContradiction role))
  unless
    ( appliedRootControlPrerequisite root
        == OracleProjection.oracleViewControlIndex views.oracleProjection
    )
    (Left (BootstrapControlPrerequisiteContradiction role))
  where
    role = appliedRootCatalogueRole root

applicationRoot :: AppliedRoot -> Application.ApplicationRoot
applicationRoot root = case appliedRootRole root of
  WriterRoot nabla sequencing ->
    Application.ApplicationWriterRoot
      (appliedRootCatalogueRole root)
      nabla
      sequencing
  ReaderRoot delta ->
    Application.ApplicationReaderRoot (appliedRootCatalogueRole root) delta

processControlledFact :: AppliedProcessBootstrap -> Controlled.ProcessFact
processControlledFact bootstrap =
  Controlled.processFact
    (appliedProcessId bootstrap)
    (appliedProcessEpochId bootstrap)
    (appliedProcessResidence bootstrap)
    (appliedProcessAuthority bootstrap)

controlledRootFact :: ProcessEpochId -> AppliedRoot -> Controlled.RootFact
controlledRootFact process root = case appliedRootRole root of
  WriterRoot nabla sequencing ->
    Controlled.writerRootFact
      process
      (appliedRootCatalogueRole root)
      (appliedRootSortId root)
      (appliedRootOccurrenceId root)
      (appliedRootAuthority root)
      (appliedRootControlPrerequisite root)
      nabla
      sequencing
  ReaderRoot delta ->
    Controlled.readerRootFact
      process
      (appliedRootCatalogueRole root)
      (appliedRootSortId root)
      (appliedRootOccurrenceId root)
      (appliedRootAuthority root)
      (appliedRootControlPrerequisite root)
      delta

rootPairs :: [AppliedRoot] -> [(NablaId, DeltaId)]
rootPairs roots = zip writers readers
  where
    writers =
      [ nabla
      | root <- roots,
        WriterRoot nabla _ <- [appliedRootRole root]
      ]
    readers =
      [ delta
      | root <- roots,
        ReaderRoot delta <- [appliedRootRole root]
      ]

readerPlacement ::
  AppliedProcessBootstrap ->
  AppliedRoot ->
  Maybe Placement.LocalPlacement
readerPlacement bootstrap root = case (appliedRootRole root, appliedRootPlacement root) of
  (ReaderRoot delta, Just (herald, incarnation)) ->
    Just
      ( Placement.localPlacement
          delta
          (appliedRootSortId root)
          (appliedRootOccurrenceId root)
          (appliedRootObjectId root)
          (appliedProcessEpochId bootstrap)
          herald
          incarnation
          (appliedRootControlPrerequisite root)
      )
  _ -> Nothing

readerStore :: ProcessEpochId -> AppliedRoot -> Maybe Store.LocalStoreSpec
readerStore process root = case (appliedRootRole root, appliedRootPlacement root) of
  (ReaderRoot delta, Just (_, incarnation)) ->
    Just
      ( Store.localStoreSpec
          (Store.ApplicationReader process (appliedRootCatalogueRole root))
          delta
          (appliedRootSortId root)
          (appliedRootOccurrenceId root)
          incarnation
      )
  _ -> Nothing

storePreparationFault ::
  Store.StorePreparationError ->
  BootstrapInvariant
storePreparationFault = \case
  Store.StoreRejected problem -> BootstrapStoreContradiction problem
  Store.StoreInvariantContradiction problem ->
    BootstrapStoreInvariant problem

-- | Observe only the canonical genesis metadata named by a checked operation.
-- Source capabilities, private aliases and local resources are not copied.
observeCanonicalBootstrapObjects :: SystemId -> [AppliedProcessBootstrap] -> Set GlobalObjectId -> Controlled.State -> Either Controlled.ControlledBootstrapError Controlled.State
observeCanonicalBootstrapObjects system bootstraps selected initial = foldM observe initial bootstraps
  where
    observe controlled bootstrap =
      let roots = filter ((`Set.member` selected) . appliedRootObjectId) (appliedProcessRoots bootstrap)
          process = appliedProcessEpochId bootstrap
          descriptions =
            [ observation
            | entry <- appliedProcessEnvironmentPublications system bootstraps bootstrap,
              let observation = either (error . show) id (checkedDescription entry),
              Controlled.checkedControlledObservationObject observation `Set.member` selected
            ]
       in if null roots && null descriptions && globalObjectIdFromProcessEpochId process `Set.notMember` selected
            then Right controlled
            else do
              metadata <- Controlled.prepareControlledBootstrapObservation (processControlledFact bootstrap) (map (controlledRootFact process) roots) controlled
              Controlled.commitControlledBootstrap <$> Controlled.prepareControlledBootstrapDescriptions Nothing descriptions (Controlled.commitControlledBootstrap metadata)

checkedDescription :: (StructuralCarrierRole, SortDefinitionOccurrenceId, CheckedPublication) -> Either Controlled.ControlledObservationError Controlled.CheckedControlledObservation
checkedDescription (carrier, occurrence, publication) =
  Controlled.checkControlledObservation (predefinedCatalogueDescriptor (profileEntryFor role)) occurrence publication
  where
    role = case carrier of
      NablaCarrier -> NablaRole
      DeltaCarrier -> DeltaRole
      NeutralVertexCarrier -> NeutralVertexRole
      EdgeCarrier -> EdgeRole
      ProcessEpochCarrier -> ProcessEpochRole
