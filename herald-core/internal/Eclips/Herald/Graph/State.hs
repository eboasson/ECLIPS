-- | Universal graph projection with checked bootstrap and controlled objects.
--
-- Bootstrap transitions install declared vertices and edges. Exact controlled
-- publication reconciliation may idempotently add neutral vertices without
-- changing the edge set.
module Eclips.Herald.Graph.State
  ( State,
    emptyState,
    takeAlignmentPlanChange,
    clearAlignmentPlanChange,
    initialTopologyState,
    graphVertices,
    graphEdges,
    graphBaselineVertices,
    graphBaselineEdges,
    graphNonStructuralBaselineVertices,
    graphNonStructuralBaselineEdges,
    graphHasVertex,
    graphReachableDeltas,
    graphStructuralVertexProjections,
    graphStructuralEdgeProjections,
    StructuralGraphBaselineProblem (..),
    PreparedStructuralGraphBaseline,
    prepareStructuralGraphBaseline,
    commitStructuralGraphBaseline,
    graphStateForReachabilityTest,
    PreparedNeutralVertexInsertion,
    prepareNeutralVertexInsertion,
    commitNeutralVertexInsertion,
    PreparedGraphBootstrap,
    GraphBootstrapError (..),
    prepareGraphBootstrap,
    commitGraphBootstrap,
    PreparedGraphSystemViews,
    prepareGraphSystemViews,
    commitGraphSystemViews,
    PreparedGraphMembershipAdmission,
    prepareGraphMembershipAdmission,
    commitGraphMembershipAdmission,
    graphMembershipAdmissionVerticesAt,
    StructuralGraphPatchProblem (..),
    PreparedStructuralGraphPatch,
    prepareStructuralGraphPatch,
    commitStructuralGraphPatch,
  )
where

import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void, absurd)
import Eclips.Domain.Graph
  ( EdgePayload,
    EdgeStrength (Preserve),
    VertexId (DeltaVertex, NablaVertex, NeutralVertex),
    edgeDestination,
    edgePayload,
    edgeSource,
    edgeStrength,
  )
import Eclips.Domain.Identity (ControlIndex, DeltaId, GlobalObjectId, NablaId)
import Eclips.Domain.Membership (heraldAdmissionControlIndex)
import Eclips.Domain.Route (ReplicaStrength (Normal, Weak))
import Eclips.Domain.Startup
  ( CheckedInitialTopologyProjection,
    checkedInitialTopologyEdges,
    checkedInitialTopologyVertices,
  )
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
  )
import Eclips.Herald.Join.Bootstrap qualified as JoinBootstrap
import Eclips.Herald.Structural.Reconciliation
  ( ExactChange,
    OwnedEdgeProjection,
    StructuralAppliedState,
    StructuralGraphPatch,
    StructuralVertexProjection,
    exactChangeAfter,
    exactChangeBefore,
    ownedEdgeProjectionOccurrence,
    ownedEdgeProjectionPayload,
    structuralAppliedEdgeProjections,
    structuralAppliedVertexProjections,
    structuralGraphEdgeChanges,
    structuralGraphVertexChanges,
    structuralVertexProjectionOccurrence,
    structuralVertexProjectionVertex,
  )

data State = State
  { baseVertices :: Set VertexId,
    baseEdges :: Set EdgePayload,
    structuralBaseVertices :: Set VertexId,
    structuralBaseEdges :: Set EdgePayload,
    structuralVertices :: Map.Map GlobalObjectId StructuralVertexProjection,
    structuralEdges :: Map.Map GlobalObjectId OwnedEdgeProjection,
    admissionVertices :: Map.Map JoinBootstrap.MembershipAdmissionOrigin (ControlIndex, Set VertexId),
    alignmentPlanChanged :: !Bool
  }
  deriving stock (Eq)

emptyState :: State
emptyState =
  State Set.empty Set.empty Set.empty Set.empty Map.empty Map.empty Map.empty False

-- | Install the complete system-wide index-zero Graph projection already
-- admitted by checked genesis. Remote vertices and edges are semantic routing
-- facts only; they create no local owner or Store state.
initialTopologyState :: CheckedInitialTopologyProjection -> State
initialTopologyState topology =
  State
    (Set.fromList (checkedInitialTopologyVertices topology))
    (Set.fromList (checkedInitialTopologyEdges topology))
    Set.empty
    Set.empty
    Map.empty
    Map.empty
    Map.empty
    False

-- | Coalesced facts read by alignment plan selection. Delivery/report
-- bookkeeping is intentionally absent from this independent consumer journal.
takeAlignmentPlanChange :: State -> (Bool, State)
takeAlignmentPlanChange state =
  let changed = alignmentPlanChanged state
   in changed `seq` (changed, clearAlignmentPlanChange state)

clearAlignmentPlanChange :: State -> State
clearAlignmentPlanChange state = state {alignmentPlanChanged = False}

graphVertices :: State -> [VertexId]
graphVertices = Set.toAscList . allVertices

graphEdges :: State -> [EdgePayload]
graphEdges = Set.toAscList . allEdges

-- | The non-occurrence graph projection committed by checked genesis and the
-- other explicit baseline transitions. Structural reconciliation must use
-- this view, rather than 'graphVertices', when resolving an older causal
-- frontier: the latter also contains possibly-ahead dynamic occurrence
-- projections.
--
-- Any explicit imported structural baseline is removed here once
-- 'prepareStructuralGraphBaseline' has claimed it, because that projection is
-- then reconstructed from the reconciliation ledger instead.
graphBaselineVertices :: State -> [VertexId]
graphBaselineVertices state =
  Set.toAscList
    (baseVertices state `Set.difference` structuralBaseVertices state)

graphBaselineEdges :: State -> [EdgePayload]
graphBaselineEdges state =
  Set.toAscList
    (baseEdges state `Set.difference` structuralBaseEdges state)

-- | Immutable base vertices not claimed by structural reconciliation. Once
-- the checked structural baseline has been bound, these are precisely the
-- system-view/hidden-writer vertices safe to add to a historical snapshot.
graphNonStructuralBaselineVertices :: State -> [VertexId]
graphNonStructuralBaselineVertices = graphBaselineVertices

-- | Immutable base edges not claimed by structural reconciliation. Historical
-- consumers must still filter these by the endpoints surviving their exact
-- prefix snapshot.
graphNonStructuralBaselineEdges :: State -> [EdgePayload]
graphNonStructuralBaselineEdges = graphBaselineEdges

graphHasVertex :: VertexId -> State -> Bool
graphHasVertex vertex = Set.member vertex . allVertices

graphStructuralVertexProjections ::
  State -> Map.Map GlobalObjectId StructuralVertexProjection
graphStructuralVertexProjections = structuralVertices

graphStructuralEdgeProjections ::
  State -> Map.Map GlobalObjectId OwnedEdgeProjection
graphStructuralEdgeProjections = structuralEdges

-- | Construct an arbitrary finite graph for independent reachability model
-- tests. This private-library seam deliberately performs no admission;
-- production graph states arise from checked topology projections and prepared
-- bootstrap, system-view, or controlled-neutral transitions.
graphStateForReachabilityTest :: [VertexId] -> [EdgePayload] -> State
graphStateForReachabilityTest suppliedVertices suppliedEdges =
  State
    (Set.fromList suppliedVertices)
    (Set.fromList suppliedEdges)
    Set.empty
    Set.empty
    Map.empty
    Map.empty
    Map.empty
    False

newtype PreparedNeutralVertexInsertion
  = PreparedNeutralVertexInsertion (Prepared State ())

-- | Idempotently prepare the minimal controlled neutral-object projection.
--
-- Existing topology and dynamically installed edges are immutable here.  The
-- receiver invokes this only after an exact controlled publication has applied
-- to at least one Store destination.
prepareNeutralVertexInsertion ::
  GlobalObjectId ->
  State ->
  PreparedNeutralVertexInsertion
prepareNeutralVertexInsertion object state =
  PreparedNeutralVertexInsertion
    ( either
        absurd
        id
        ( prepareTransition
            install
            state
        )
    )
  where
    install :: State -> Either Void (State, ())
    install predecessor =
      Right
        ( predecessor
            { baseVertices =
                Set.insert (NeutralVertex object) (baseVertices predecessor),
              alignmentPlanChanged = alignmentPlanChanged predecessor || Set.notMember (NeutralVertex object) (baseVertices predecessor)
            },
          ()
        )

commitNeutralVertexInsertion :: PreparedNeutralVertexInsertion -> State
commitNeutralVertexInsertion (PreparedNeutralVertexInsertion prepared) =
  fst (commitPrepared prepared)

-- | Strongest ordinary directed reachability from one writer.
--
-- Every edge is universal. A weakening edge makes that path weak, while a
-- preserving path keeps its incoming strength. The finite two-point strength
-- lattice makes this work-list computation total even in cyclic graphs.
graphReachableDeltas :: NablaId -> State -> [(DeltaId, ReplicaStrength)]
graphReachableDeltas nabla state =
  [ (delta, strength)
  | (DeltaVertex delta, strength) <- Map.toAscList reached
  ]
  where
    source = NablaVertex nabla
    reached
      | Set.notMember source (allVertices state) = Map.empty
      | otherwise = reach [(source, Normal)] (Map.singleton source Normal)
    adjacency =
      Map.fromListWith
        (<>)
        [ (edgeSource edge, [(edgeDestination edge, edgeStrength edge)])
        | edge <- Set.toAscList (allEdges state)
        ]
    reach [] known = known
    reach ((vertex, strength) : pending) known =
      let (nextPending, nextKnown) =
            foldl
              (advance strength)
              (pending, known)
              (Map.findWithDefault [] vertex adjacency)
       in reach nextPending nextKnown
    advance incoming (pending, known) (destination, edge) =
      let candidate = composeStrength incoming edge
       in case Map.lookup destination known of
            Just incumbent | incumbent >= candidate -> (pending, known)
            _ ->
              ( pending <> [(destination, candidate)],
                Map.insert destination candidate known
              )

composeStrength :: ReplicaStrength -> EdgeStrength -> ReplicaStrength
composeStrength Normal Preserve = Normal
composeStrength _ _ = Weak

data GraphBootstrapError
  = GraphVertexAlreadyInstalled VertexId
  | GraphSystemViewWriterMissing NablaId
  | GraphSystemViewDeltaAlreadyInstalled DeltaId
  | GraphSystemViewDeltaMissing DeltaId
  | GraphMembershipAdmissionCoordinateInvalid ControlIndex
  | GraphMembershipAdmissionConflict JoinBootstrap.MembershipAdmissionOrigin
  deriving stock (Eq, Show)

newtype PreparedGraphBootstrap
  = PreparedGraphBootstrap (Prepared State ())

-- | Install the checked environment vertices and its named hub wiring. The
-- structural genesis baseline subsequently claims these exact facts, so no
-- anonymous paired edge survives deletion of an application wiring object.
prepareGraphBootstrap ::
  [(NablaId, DeltaId)] ->
  GlobalObjectId ->
  [EdgePayload] ->
  State ->
  Either GraphBootstrapError PreparedGraphBootstrap
prepareGraphBootstrap pairs hub edges state =
  PreparedGraphBootstrap
    <$> prepareTransition (installGraphBootstrap pairs hub edges) state

commitGraphBootstrap :: PreparedGraphBootstrap -> State
commitGraphBootstrap (PreparedGraphBootstrap prepared) =
  fst (commitPrepared prepared)

newtype PreparedGraphSystemViews
  = PreparedGraphSystemViews (Prepared State ())

-- | Add the operational private-system-view destinations introduced by the
-- first regular-data vertical. Writers must already be checked bootstrap
-- vertices; system views add no controlled object or capability fact.
prepareGraphSystemViews ::
  [DeltaId] ->
  [(NablaId, DeltaId)] ->
  State ->
  Either GraphBootstrapError PreparedGraphSystemViews
prepareGraphSystemViews deltas pairs state =
  PreparedGraphSystemViews
    <$> prepareTransition (installGraphSystemViews deltas pairs) state

commitGraphSystemViews :: PreparedGraphSystemViews -> State
commitGraphSystemViews (PreparedGraphSystemViews prepared) =
  fst (commitPrepared prepared)

newtype PreparedGraphMembershipAdmission = PreparedGraphMembershipAdmission (Prepared State ())

-- | The private destinations of an activated Herald have a nominal admission
-- origin, and never enter the timeless genesis baseline or add exposing edges.
prepareGraphMembershipAdmission :: ControlIndex -> JoinBootstrap.HeraldBootstrapContribution -> State -> Either GraphBootstrapError PreparedGraphMembershipAdmission
prepareGraphMembershipAdmission index contribution state =
  PreparedGraphMembershipAdmission <$> prepareTransition install state
  where
    origin = JoinBootstrap.contributionOrigin contribution
    vertices = Set.fromList [DeltaVertex delta | (_, delta, _) <- JoinBootstrap.contributionViews contribution]
    install predecessor
      | index <= heraldAdmissionControlIndex (JoinBootstrap.contributionAdmission contribution) = Left (GraphMembershipAdmissionCoordinateInvalid index)
      | Just incumbent <- Map.lookup origin (admissionVertices predecessor) =
          if incumbent == (index, vertices) then Right (predecessor, ()) else Left (GraphMembershipAdmissionConflict origin)
      | Just duplicate <- Set.lookupMin (vertices `Set.intersection` allVertices predecessor) = Left (GraphVertexAlreadyInstalled duplicate)
      | otherwise = Right (predecessor {admissionVertices = Map.insert origin (index, vertices) (admissionVertices predecessor), alignmentPlanChanged = True}, ())

commitGraphMembershipAdmission :: PreparedGraphMembershipAdmission -> State
commitGraphMembershipAdmission (PreparedGraphMembershipAdmission prepared) = fst (commitPrepared prepared)

graphMembershipAdmissionVerticesAt :: ControlIndex -> State -> [VertexId]
graphMembershipAdmissionVerticesAt index state = Set.toAscList (Set.unions [vertices | (enabled, vertices) <- Map.elems (admissionVertices state), enabled <= index])

installGraphSystemViews ::
  [DeltaId] ->
  [(NablaId, DeltaId)] ->
  State ->
  Either GraphBootstrapError (State, ())
installGraphSystemViews viewDeltas pairs state = do
  mapM_ requireWriter (Set.toAscList writers)
  mapM_ requireDeclaredDelta (Set.toAscList pairedDeltas)
  mapM_ requireFreshDelta (Set.toAscList deltas)
  let successor =
        state
          { baseVertices =
              Set.union
                (baseVertices state)
                (Set.map DeltaVertex deltas),
            baseEdges =
              Set.union
                (baseEdges state)
                ( Set.fromList
                    [ edgePayload
                        (NablaVertex nabla)
                        (DeltaVertex delta)
                        Preserve
                    | (nabla, delta) <- pairs
                    ]
                ),
            alignmentPlanChanged = alignmentPlanChanged state || not (Set.null deltas)
          }
  Right (successor, ())
  where
    writers = Set.fromList (fmap fst pairs)
    deltas = Set.fromList viewDeltas
    pairedDeltas = Set.fromList (fmap snd pairs)
    requireWriter nabla
      | Set.member (NablaVertex nabla) (allVertices state) = Right ()
      | otherwise = Left (GraphSystemViewWriterMissing nabla)
    requireFreshDelta delta
      | Set.member (DeltaVertex delta) (allVertices state) =
          Left (GraphSystemViewDeltaAlreadyInstalled delta)
      | otherwise = Right ()
    requireDeclaredDelta delta
      | Set.member delta deltas = Right ()
      | otherwise = Left (GraphSystemViewDeltaMissing delta)

installGraphBootstrap ::
  [(NablaId, DeltaId)] ->
  GlobalObjectId ->
  [EdgePayload] ->
  State ->
  Either GraphBootstrapError (State, ())
installGraphBootstrap pairs hub edges state = do
  withVertices <-
    foldM
      (flip insertVertex)
      state
      (NeutralVertex hub : (pairs >>= \(nabla, delta) -> [NablaVertex nabla, DeltaVertex delta]))
  let successor =
        withVertices
          { baseEdges =
              Set.union
                (baseEdges withVertices)
                (Set.fromList edges)
          }
  Right (successor, ())

insertVertex :: VertexId -> State -> Either GraphBootstrapError State
insertVertex vertex state
  | Set.member vertex (allVertices state) =
      Left (GraphVertexAlreadyInstalled vertex)
  | otherwise =
      Right state {baseVertices = Set.insert vertex (baseVertices state), alignmentPlanChanged = True}

allVertices :: State -> Set VertexId
allVertices state =
  Set.union
    ((baseVertices state `Set.difference` structuralBaseVertices state) `Set.union` Set.unions (map snd (Map.elems (admissionVertices state))))
    ( Set.fromList
        ( structuralVertexProjectionVertex
            <$> Map.elems (structuralVertices state)
        )
    )

allEdges :: State -> Set EdgePayload
allEdges state =
  Set.filter endpointsRetained projected
  where
    vertices = allVertices state
    projected =
      Set.union
        (baseEdges state `Set.difference` structuralBaseEdges state)
        ( Set.fromList
            (ownedEdgeProjectionPayload <$> Map.elems (structuralEdges state))
        )
    endpointsRetained edge =
      Set.member (edgeSource edge) vertices
        && Set.member (edgeDestination edge) vertices

data StructuralGraphBaselineProblem
  = StructuralGraphBaselineVertexNotBaseline GlobalObjectId
  | StructuralGraphBaselineEdgeNotBaseline GlobalObjectId
  | StructuralGraphBaselineVertexMissing GlobalObjectId VertexId
  | StructuralGraphBaselineEdgeMissing GlobalObjectId EdgePayload
  | StructuralGraphBaselineConflict
  deriving stock (Eq, Show)

newtype PreparedStructuralGraphBaseline
  = PreparedStructuralGraphBaseline (Prepared State ())

-- | Bind the checked genesis controlled projections to their exact graph
-- provenance without fabricating structural dots. This may be called lazily by
-- the first Increment-4 application; an exact repeat is a no-op.
prepareStructuralGraphBaseline ::
  StructuralAppliedState ->
  State ->
  Either StructuralGraphBaselineProblem PreparedStructuralGraphBaseline
prepareStructuralGraphBaseline applied state =
  PreparedStructuralGraphBaseline <$> prepareTransition seed state
  where
    expectedVertices = structuralAppliedVertexProjections applied
    expectedEdges = structuralAppliedEdgeProjections applied

    seed predecessor
      | structuralVertices predecessor == expectedVertices
          && structuralEdges predecessor == expectedEdges =
          Right (predecessor, ())
      | not (Map.null (structuralVertices predecessor))
          || not (Map.null (structuralEdges predecessor)) =
          Left StructuralGraphBaselineConflict
      | otherwise = do
          mapM_ validateVertex (Map.toAscList expectedVertices)
          mapM_ validateEdge (Map.toAscList expectedEdges)
          let ownedVertices =
                Set.fromList
                  (structuralVertexProjectionVertex <$> Map.elems expectedVertices)
              ownedEdges =
                Set.fromList
                  (ownedEdgeProjectionPayload <$> Map.elems expectedEdges)
          Right
            ( predecessor
                { structuralBaseVertices = ownedVertices,
                  structuralBaseEdges = ownedEdges,
                  structuralVertices = expectedVertices,
                  structuralEdges = expectedEdges,
                  alignmentPlanChanged = alignmentPlanChanged predecessor || not (Set.null ownedVertices && Set.null ownedEdges)
                },
              ()
            )
      where
        validateVertex (object, projection)
          | structuralVertexProjectionOccurrence projection /= Nothing =
              Left (StructuralGraphBaselineVertexNotBaseline object)
          | Set.notMember
              (structuralVertexProjectionVertex projection)
              (baseVertices predecessor) =
              Left
                ( StructuralGraphBaselineVertexMissing
                    object
                    (structuralVertexProjectionVertex projection)
                )
          | otherwise = Right ()
        validateEdge (object, projection)
          | ownedEdgeProjectionOccurrence projection /= Nothing =
              Left (StructuralGraphBaselineEdgeNotBaseline object)
          | Set.notMember
              (ownedEdgeProjectionPayload projection)
              (baseEdges predecessor) =
              Left
                ( StructuralGraphBaselineEdgeMissing
                    object
                    (ownedEdgeProjectionPayload projection)
                )
          | otherwise = Right ()

commitStructuralGraphBaseline :: PreparedStructuralGraphBaseline -> State
commitStructuralGraphBaseline (PreparedStructuralGraphBaseline prepared) =
  fst (commitPrepared prepared)

data StructuralGraphPatchProblem
  = StructuralGraphVertexProjectionMismatch
      GlobalObjectId
      (Maybe StructuralVertexProjection)
      (Maybe StructuralVertexProjection)
  | StructuralGraphEdgeProjectionMismatch
      GlobalObjectId
      (Maybe OwnedEdgeProjection)
      (Maybe OwnedEdgeProjection)
  deriving stock (Eq, Show)

newtype PreparedStructuralGraphPatch
  = PreparedStructuralGraphPatch (Prepared State ())

-- | Exact-apply one Increment-3 structural graph patch to the live routing
-- owner. Object-qualified projection maps preserve provenance and multiplicity,
-- while public reachability observes their set union with immutable/bootstrap
-- graph facts.
prepareStructuralGraphPatch ::
  StructuralGraphPatch ->
  State ->
  Either StructuralGraphPatchProblem PreparedStructuralGraphPatch
prepareStructuralGraphPatch patch state =
  PreparedStructuralGraphPatch
    <$> prepareTransition applyPatch state
  where
    applyPatch predecessor = do
      successorVertices <-
        applyExactChanges
          StructuralGraphVertexProjectionMismatch
          (structuralGraphVertexChanges patch)
          (structuralVertices predecessor)
      successorEdges <-
        applyExactChanges
          StructuralGraphEdgeProjectionMismatch
          (structuralGraphEdgeChanges patch)
          (structuralEdges predecessor)
      Right
        ( predecessor
            { structuralVertices = successorVertices,
              structuralEdges = successorEdges
            },
          ()
        )

commitStructuralGraphPatch :: PreparedStructuralGraphPatch -> State
commitStructuralGraphPatch (PreparedStructuralGraphPatch prepared) =
  fst (commitPrepared prepared)

applyExactChanges ::
  (Ord key, Eq value) =>
  (key -> Maybe value -> Maybe value -> problem) ->
  Map.Map key (ExactChange value) ->
  Map.Map key value ->
  Either problem (Map.Map key value)
applyExactChanges mismatch changes predecessor =
  foldM applyOne predecessor (Map.toAscList changes)
  where
    applyOne current (key, change)
      | actual /= expected = Left (mismatch key expected actual)
      | otherwise =
          Right $ case exactChangeAfter change of
            Nothing -> Map.delete key current
            Just successor -> Map.insert key successor current
      where
        expected = exactChangeBefore change
        actual = Map.lookup key current
