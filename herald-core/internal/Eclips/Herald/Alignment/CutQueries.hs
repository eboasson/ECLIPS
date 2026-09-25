{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Reusable derived cut queries. The Startup owner invalidates these views
-- when their structural or placement inputs are replaced. Cache entries never
-- capture the whole Herald state, and only compact route coordinates survive
-- a demanded historical projection.
module Eclips.Herald.Alignment.CutQueries
  ( CutQueryCache,
    CutQueryProblem (..),
    emptyCutQueryCache,
    invalidatePlacementCutQueries,
    prepareCutQueryCache,
    trimCutQueryCache,
    cutQueryRetainedKeys,
    cutQueryPositions,
    cutQueryCauseCoverage,
    cutQueriesAtPlacement,
    cutQueryMissingTopology,
    AlignmentRecoveryCutView,
    prepareAlignmentRecoveryCutView,
    alignmentFirstMatchingRecoveryCut,
    activityIsActive,
    installedDeltaRouteCoordinate,
    placementDeltaRouteCoordinate,
  )
where

import Data.IntMap.Lazy qualified as IntMap
import Data.List (sort)
import Data.Map.Lazy qualified as LazyMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Alignment
  ( PhysicalPlacementRevisionVector,
    physicalPlacementRevisionMemberSetDigest,
    physicalPlacementRevisionMembershipGenerationId,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    ProcessEpochId,
    TopologyCutId,
  )
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    structuralConsequenceCauseControlIndex,
  )
import Eclips.Domain.Topology
  ( TopologyFrontier,
    topologyCutFrontier,
    topologyFrontierMemberSetDigest,
    topologyFrontierMembershipGenerationId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Placement
  ( DeltaRoute,
    DeltaRouteView (..),
    deltaRouteOccurrenceId,
    deltaRouteSortId,
    viewDeltaRoute,
  )
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Structural.Debt (SortOccurrence, sortOccurrence)
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation

-- | The installed structural facts which an application placement route must
-- reproduce before its placement-cut-owned incarnation can participate in an
-- alignment cut. Binding the structural control prerequisite prevents a stale
-- same-host route from a passivate/reactivate predecessor being accepted.
data InstalledDeltaRouteCoordinate
  = InstalledDeltaRouteCoordinate
      !HeraldEpoch
      !DeltaId
      !SortOccurrence
      !GlobalObjectId
      !ProcessEpochId
      !ControlIndex
  deriving stock (Eq, Ord, Show)

data CutQueryProblem
  = CutQueryTopologyProblem GraphProgress.StructuralProgressProblem
  | CutQueryPlacementProblem Placement.PlacementCutProblem
  deriving stock (Eq, Show)

-- | Lazy values are essential: membership mismatches, live-incumbent bypasses
-- and suffix lower bounds must not force unreachable projection failures.
-- The derived wrapper in Startup excludes this cache from semantic equality.
data CutQueryCache = CutQueryCache
  { structural :: !(Maybe StructuralCutQueries),
    causeCoverage :: !(Map StructuralConsequenceCause (Maybe TopologyCutId)),
    cachedSorts :: !(Set SortOccurrence),
    placementQueries :: !(Map PhysicalPlacementRevisionVector PlacementCutQueries)
  }

data StructuralCutQueries = StructuralCutQueries
  { positions :: !(Map TopologyCutId Int),
    numbered :: ![(Int, TopologyCutId)],
    frontiers :: !(Map TopologyCutId (Either CutQueryProblem TopologyFrontier)),
    projectionRoutes :: !(Map TopologyCutId (Either CutQueryProblem (Map SortOccurrence (Maybe [InstalledDeltaRouteCoordinate]))))
  }

data PlacementCutQueries = PlacementCutQueries
  { actualRoutes :: Either CutQueryProblem (Map SortOccurrence [InstalledDeltaRouteCoordinate]),
    sortQueries :: !(Map SortOccurrence (AlignmentRecoveryCutView CutQueryProblem))
  }

emptyCutQueryCache :: CutQueryCache
emptyCutQueryCache = CutQueryCache Nothing Map.empty Set.empty Map.empty

-- | A placement owner replacement can make an unavailable historical vector
-- available. Drop successes and failures together while keeping structural work.
invalidatePlacementCutQueries :: CutQueryCache -> CutQueryCache
invalidatePlacementCutQueries cache = cache {cachedSorts = Set.empty, placementQueries = Map.empty}

-- | Extend only missing cause/placement/sort coordinates. The supplied views
-- contain just effective sorts and nonstructural baseline vertices; historical
-- reconstruction does not use current process liveness or possession facts.
-- Only the structural owner and these compact views enter projection thunks.
prepareCutQueryCache ::
  Reconciliation.ReconciliationViews ->
  GraphProgress.StructuralProgressState ->
  Placement.State ->
  Set StructuralConsequenceCause ->
  Set SortOccurrence ->
  Set PhysicalPlacementRevisionVector ->
  CutQueryCache ->
  CutQueryCache
prepareCutQueryCache views progress placement causes sorts vectors cache =
  case cache.structural of
    Nothing -> extend (prepareStructuralCutQueries views progress)
    Just core -> extend core
  where
    extend core = CutQueryCache (Just core) coverage allSorts placements
      where
        coverage = extendMissing causes (`GraphProgress.installedCutCoveringCause` progress) cache.causeCoverage
        newSorts = Set.difference sorts cache.cachedSorts
        allSorts = cache.cachedSorts <> sorts
        existingPlacements
          | Set.null newSorts = cache.placementQueries
          | otherwise = Map.mapWithKey extendPlacement cache.placementQueries
        -- An unqueried new vector must not allocate its per-sort map. The
        -- thunk captures selected inputs only; strict record-destructuring
        -- updates below prevent wrapper chains when existing scopes change.
        placements =
          LazyMap.union
            existingPlacements
            (LazyMap.fromSet preparePlacement (Set.difference vectors (Map.keysSet existingPlacements)))
        preparePlacement vector =
          let actual =
                mapLeft
                  CutQueryPlacementProblem
                  (applicationPlacementRoutesBySort <$> Placement.placementRoutesAtVector vector placement)
           in PlacementCutQueries actual (LazyMap.fromSet (prepareSort core vector actual) allSorts)
        extendPlacement vector (PlacementCutQueries actual queries) =
          PlacementCutQueries actual (extendMissing newSorts (prepareSort core vector actual) queries)

prepareStructuralCutQueries ::
  Reconciliation.ReconciliationViews ->
  GraphProgress.StructuralProgressState ->
  StructuralCutQueries
prepareStructuralCutQueries views progress =
  StructuralCutQueries positions numbered frontiers projectionRoutes
  where
    installed = GraphProgress.structuralInstalledCutIds progress
    numbered = zip [0 ..] installed
    positions = Map.fromList [(cut, position) | (position, cut) <- numbered]
    frontiers =
      LazyMap.fromList
        [ ( cut,
            maybe
              (Left (cutQueryMissingTopology cut))
              (Right . topologyCutFrontier . GraphProgress.installedTopologyCutCut)
              (GraphProgress.lookupInstalledTopologyCut cut progress)
          )
        | cut <- installed
        ]
    projectionRoutes =
      LazyMap.fromList
        [ ( cut,
            mapLeft
              CutQueryTopologyProblem
              (applicationProjectionRoutesBySort <$> GraphProgress.structuralProjectionAtInstalledCut views cut progress)
          )
        | cut <- installed
        ]

prepareSort ::
  StructuralCutQueries ->
  PhysicalPlacementRevisionVector ->
  Either CutQueryProblem (Map SortOccurrence [InstalledDeltaRouteCoordinate]) ->
  SortOccurrence ->
  AlignmentRecoveryCutView CutQueryProblem
prepareSort core placement actualRoutes affectedSort =
  prepareAlignmentRecoveryCutViewAt core.positions core.numbered matches
  where
    matches cut = do
      frontier <- maybe (Left (cutQueryMissingTopology cut)) id (Map.lookup cut core.frontiers)
      if topologyFrontierMembershipGenerationId frontier /= physicalPlacementRevisionMembershipGenerationId placement
        || topologyFrontierMemberSetDigest frontier /= physicalPlacementRevisionMemberSetDigest placement
        then Right False
        else do
          expected <- maybe (Left (cutQueryMissingTopology cut)) id (Map.lookup cut core.projectionRoutes)
          actual <- actualRoutes
          Right (Map.findWithDefault (Just []) affectedSort expected == Just (Map.findWithDefault [] affectedSort actual))

-- | Retained memo keys follow live protocol retention. An input-only vector is
-- discarded unless it is now current or queued; query traffic cannot grow a
-- separate, never-retired placement history.
trimCutQueryCache ::
  Set StructuralConsequenceCause ->
  Set SortOccurrence ->
  Set PhysicalPlacementRevisionVector ->
  CutQueryCache ->
  CutQueryCache
trimCutQueryCache causes sorts vectors cache =
  cache
    { causeCoverage = restrictIfNeeded cache.causeCoverage causes,
      cachedSorts = retainedSorts,
      placementQueries = trimmedPlacements
    }
  where
    retainedSorts = Set.intersection cache.cachedSorts sorts
    placements = restrictIfNeeded cache.placementQueries vectors
    trimmedPlacements
      | retainedSorts == cache.cachedSorts = placements
      | otherwise = Map.map trimPlacement placements
    trimPlacement (PlacementCutQueries actual queries) =
      PlacementCutQueries actual (restrictIfNeeded queries sorts)
    restrictIfNeeded retained keys
      | Map.keysSet retained `Set.isSubsetOf` keys = retained
      | otherwise = LazyMap.restrictKeys retained keys

cutQueryPositions :: CutQueryCache -> Map TopologyCutId Int
cutQueryPositions cache = maybe Map.empty (.positions) cache.structural

-- | Read-only retention witness for invariant and differential properties.
cutQueryRetainedKeys :: CutQueryCache -> (Set StructuralConsequenceCause, Set SortOccurrence, Set PhysicalPlacementRevisionVector)
cutQueryRetainedKeys cache = (Map.keysSet cache.causeCoverage, cache.cachedSorts, Map.keysSet cache.placementQueries)

cutQueryCauseCoverage :: StructuralConsequenceCause -> CutQueryCache -> Maybe (Maybe TopologyCutId)
cutQueryCauseCoverage cause cache = Map.lookup cause cache.causeCoverage

cutQueriesAtPlacement ::
  PhysicalPlacementRevisionVector ->
  CutQueryCache ->
  Maybe (Either CutQueryProblem (Map SortOccurrence (AlignmentRecoveryCutView CutQueryProblem)))
cutQueriesAtPlacement placement cache = fmap select (Map.lookup placement cache.placementQueries)
  where
    select retained = retained.actualRoutes >> Right retained.sortQueries

cutQueryMissingTopology :: TopologyCutId -> CutQueryProblem
cutQueryMissingTopology = CutQueryTopologyProblem . GraphProgress.StructuralProgressSourceTopologyUnavailable

extendMissing :: (Ord key) => Set key -> (key -> value) -> Map key value -> Map key value
extendMissing requested prepare retained =
  LazyMap.union retained (LazyMap.fromSet prepare (Set.difference requested (Map.keysSet retained)))

mapLeft :: (problem -> mapped) -> Either problem value -> Either mapped value
mapLeft convert = either (Left . convert) Right

-- | Shared suffix answers for related recovery queries. The map spine is
-- prepared once, but its values deliberately remain lazy: a later match or
-- projection failure must not be evaluated unless a query reaches it. Each
-- mismatch shares the following answer, so repeated and overlapping suffix
-- queries perform each candidate check at most once.
data AlignmentRecoveryCutView problem
  = AlignmentRecoveryCutView
      !(Map TopologyCutId Int)
      !(IntMap.IntMap (Either problem (Maybe TopologyCutId)))

prepareAlignmentRecoveryCutView ::
  [TopologyCutId] ->
  (TopologyCutId -> Either problem Bool) ->
  AlignmentRecoveryCutView problem
prepareAlignmentRecoveryCutView installed =
  prepareAlignmentRecoveryCutViewAt
    (Map.fromList (zip installed [0 ..]))
    (zip [0 ..] installed)

prepareAlignmentRecoveryCutViewAt ::
  Map TopologyCutId Int ->
  [(Int, TopologyCutId)] ->
  (TopologyCutId -> Either problem Bool) ->
  AlignmentRecoveryCutView problem
prepareAlignmentRecoveryCutViewAt positions installed check =
  AlignmentRecoveryCutView positions (IntMap.fromDistinctAscList answers)
  where
    (_, answers) = foldr next (Right Nothing, []) installed
    next (position, cut) (following, retained) =
      let answer = do
            matches <- check cut
            if matches then Right (Just cut) else following
       in (answer, (position, answer) : retained)

-- | Validate every incumbent before testing cause coverage, preserving the
-- original missing-incumbent error order. A live incumbent uses the original
-- coverage-only path and never demands the candidate table.
alignmentFirstMatchingRecoveryCut ::
  (TopologyCutId -> problem) ->
  Bool ->
  Maybe TopologyCutId ->
  Set TopologyCutId ->
  AlignmentRecoveryCutView problem ->
  Either problem (Maybe TopologyCutId)
alignmentFirstMatchingRecoveryCut missing liveIncumbent covering incumbents view
  | liveIncumbent = Right covering
  | otherwise = do
      let AlignmentRecoveryCutView positions answers = view
          position cut = maybe (Left (missing cut)) Right (Map.lookup cut positions)
      incumbentPositions <- traverse position (Set.toAscList incumbents)
      case covering of
        Nothing -> Right Nothing
        Just firstCovering -> do
          coveringPosition <- position firstCovering
          let lowerBound = foldl' max coveringPosition incumbentPositions
          case IntMap.lookupGE lowerBound answers of
            Nothing -> Right Nothing
            Just (_, answer) -> answer

applicationProjectionRoutesBySort ::
  Reconciliation.StructuralProjectionSnapshot -> Map SortOccurrence (Maybe [InstalledDeltaRouteCoordinate])
applicationProjectionRoutesBySort snapshot =
  Map.map
    (fmap sort)
    (Map.foldlWithKey' add Map.empty (Reconciliation.structuralProjectionSnapshotVertices snapshot))
  where
    add retained object vertex = case vertex of
      Reconciliation.DeltaVertexProjection _ _ affectedSort _ activity
        | activityIsActive activity ->
            case installedDeltaRouteCoordinate snapshot (object, vertex) of
              Nothing -> Map.insert affectedSort Nothing retained
              Just coordinate -> coordinate `seq` Map.insertWith combine affectedSort (Just [coordinate]) retained
      _ -> retained
    combine (Just incoming) (Just incumbent) = Just (incoming <> incumbent)
    combine _ _ = Nothing

applicationPlacementRoutesBySort ::
  [(HeraldEpoch, [DeltaRoute])] -> Map SortOccurrence [InstalledDeltaRouteCoordinate]
applicationPlacementRoutesBySort routes =
  Map.map sort (foldl' add Map.empty [(owner, route) | (owner, ownerRoutes) <- routes, route <- ownerRoutes])
  where
    add retained (owner, route) = case placementDeltaRouteCoordinate owner route of
      Nothing -> retained
      Just coordinate ->
        coordinate
          `seq` Map.insertWith
            (<>)
            (sortOccurrence (deltaRouteSortId route) (deltaRouteOccurrenceId route))
            [coordinate]
            retained

activityIsActive :: Reconciliation.RootActivityProjection -> Bool
activityIsActive activity = case activity of
  Reconciliation.PassiveRoot -> False
  Reconciliation.ActiveRootHere _ -> True
  Reconciliation.RemoteController _ _ -> True

installedDeltaRouteCoordinate ::
  Reconciliation.StructuralProjectionSnapshot ->
  (GlobalObjectId, Reconciliation.StructuralVertexProjection) ->
  Maybe InstalledDeltaRouteCoordinate
installedDeltaRouteCoordinate snapshot (object, projection) = do
  (delta, typedSort, process, residence, activity) <-
    case projection of
      Reconciliation.DeltaVertexProjection
        _
        delta
        typedSort
        (Reconciliation.LiveProcessController process residence)
        activity ->
          Just (delta, typedSort, process, residence, activity)
      _ -> Nothing
  case activity of
    Reconciliation.ActiveRootHere activeProcess | activeProcess == process -> Just ()
    Reconciliation.RemoteController activeProcess activeResidence
      | activeProcess == process && activeResidence == residence -> Just ()
    _ -> Nothing
  digestEntry <-
    Map.lookup object (Reconciliation.structuralProjectionSnapshotDigestEntries snapshot)
  prerequisite <-
    case Reconciliation.structuralProjectionDigestEntryControlCause digestEntry of
      Nothing ->
        Just
          (Reconciliation.structuralProjectionDigestEntryControlPrerequisite digestEntry)
      Just cause ->
        max
          (Reconciliation.structuralProjectionDigestEntryControlPrerequisite digestEntry)
          <$> structuralConsequenceCauseControlIndex cause
  Just
    ( InstalledDeltaRouteCoordinate
        residence
        delta
        typedSort
        object
        process
        prerequisite
    )

placementDeltaRouteCoordinate ::
  HeraldEpoch -> DeltaRoute -> Maybe InstalledDeltaRouteCoordinate
placementDeltaRouteCoordinate owner route = case viewDeltaRoute route of
  ApplicationDeltaRouteView
    delta
    routeSort
    occurrence
    controller
    process
    _incarnation
    prerequisite ->
      Just
        ( InstalledDeltaRouteCoordinate
            owner
            delta
            (sortOccurrence routeSort occurrence)
            controller
            process
            prerequisite
        )
  PrivateSystemViewDeltaRouteView {} -> Nothing
