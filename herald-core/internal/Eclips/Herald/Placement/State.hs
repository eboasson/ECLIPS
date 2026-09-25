{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure owner of local authoritative and remote projected delta placement.
module Eclips.Herald.Placement.State
  ( State,
    emptyState,
    initialState,
    LocalPlacement,
    localPlacement,
    localPlacementDelta,
    localPlacementSortId,
    localPlacementOccurrenceId,
    localPlacementControllerObject,
    localPlacementProcessEpoch,
    localPlacementHeraldEpoch,
    localPlacementStoreIncarnation,
    localPlacementControlPrerequisite,
    lookupLocalPlacement,
    takePreparationPlacementChanges,
    clearPreparationPlacementChanges,
    takeAlignmentPlanChange,
    clearAlignmentPlanChange,
    takeAlignmentLossChange,
    clearAlignmentLossChange,
    localPlacements,
    bootstrapLocalPlacements,
    SystemViewPlacement,
    systemViewPlacement,
    systemViewPlacementRole,
    systemViewPlacementDelta,
    systemViewPlacementSortId,
    systemViewPlacementOccurrenceId,
    systemViewPlacementHeraldEpoch,
    systemViewPlacementStoreIncarnation,
    lookupSystemViewPlacement,
    systemViewPlacements,
    PreparedPlacementBootstrap,
    PlacementBootstrapError (..),
    preparePlacementBootstrap,
    commitPlacementBootstrap,
    PreparedSystemViewPlacement,
    prepareSystemViewPlacements,
    commitSystemViewPlacements,
    StructuralPlacementPatchError (..),
    PreparedStructuralPlacementPatch,
    prepareStructuralPlacementPatch,
    preparedStructuralPlacementSnapshot,
    commitStructuralPlacementPatch,
    LocalSnapshotDisposition (..),
    LocalSnapshotResult,
    localSnapshotDisposition,
    localSnapshotMessage,
    PreparedLocalSnapshot,
    prepareLocalSnapshot,
    prepareLocalSnapshotWithDiagnostics,
    prepareLocalSnapshotForRoutes,
    prepareLocalSnapshotForRoutesWithDiagnostics,
    prepareRetainedLocalSnapshot,
    prepareRetainedLocalSnapshotWithDiagnostics,
    preparedLocalSnapshotResult,
    commitLocalSnapshot,
    PlacementAckDisposition (..),
    PreparedPlacementAcknowledgement,
    preparePlacementAcknowledgement,
    preparePlacementAcknowledgementWithDiagnostics,
    preparedPlacementAckDisposition,
    commitPlacementAcknowledgement,
    RemotePlacementDisposition (..),
    RemotePlacementResult,
    remotePlacementDisposition,
    remotePlacementAcknowledgement,
    PreparedRemotePlacement,
    prepareRemotePlacement,
    prepareRemotePlacementWithDiagnostics,
    preparedRemotePlacementResult,
    commitRemotePlacement,
    RetiredRemotePlacement,
    retiredRemotePlacementOwner,
    retiredRemotePlacementRoutes,
    PreparedRemotePlacementRetirement,
    prepareRemotePlacementRetirement,
    prepareRemotePlacementRetirementWithDiagnostics,
    preparedRemotePlacementRetirement,
    commitRemotePlacementRetirement,
    remotePlacementOwnerRetired,
    remotePlacementSequence,
    remotePlacementOwners,
    remotePlacementRoutes,
    lookupRemotePlacements,
    localPlacementSequence,
    placementAcknowledgedThrough,
    PlacementCutProblem (..),
    currentPhysicalPlacementRevisionVector,
    placementRoutesAtRevision,
    placementRoutesAtVector,
    retainedPlacementSnapshots,
    retainHistoricalPlacementSnapshot,
    retainedPlacementRouteMatches,
    currentPlacementRouteEntries,
    retainedPlacementRouteEntries,
    PlacementProtocolViolation (..),
    PlacementInvariantViolation (..),
    PlacementProblem (..),
    validatePlacementState,
  )
where

import Control.Monad (foldM)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Alignment
  ( AlignmentShapeError,
    PhysicalPlacementRevisionVector,
    physicalPlacementRevisionEntries,
    physicalPlacementRevisionVector,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    ProcessEpochId,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
  )
import Eclips.Domain.Startup (PredefinedSortRole)
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks (..), runDiagnosticCheck)
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
    preparedOutput,
  )
import Eclips.Herald.Placement
  ( DeltaRoute,
    PlacementAcknowledgement,
    PlacementSequence,
    PlacementShapeProblem,
    PlacementSnapshot,
    PlacementUpdate (..),
    applicationDeltaRoute,
    deltaRouteDelta,
    deltaRouteOccurrenceId,
    deltaRouteSortId,
    deltaRouteStoreIncarnation,
    firstPlacementSequence,
    nextPlacementSequence,
    placementAcknowledgement,
    placementAcknowledgementOwner,
    placementAcknowledgementSequence,
    placementSnapshot,
    placementSnapshotOwner,
    placementSnapshotRoutes,
    placementSnapshotSequence,
    privateSystemViewDeltaRoute,
  )
import Eclips.Herald.Structural.Debt
  ( sortOccurrenceDefinition,
    sortOccurrenceSortId,
  )
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation

-- | One exact local store incarnation advertised for a checked reader root.
data LocalPlacement = LocalPlacement
  { delta :: DeltaId,
    sortId :: SortId,
    occurrenceId :: SortDefinitionOccurrenceId,
    controller :: GlobalObjectId,
    processEpoch :: ProcessEpochId,
    heraldEpoch :: HeraldEpoch,
    incarnation :: StoreIncarnationId,
    controlPrerequisite :: ControlIndex
  }
  deriving stock (Eq, Show)

localPlacement ::
  DeltaId ->
  SortId ->
  SortDefinitionOccurrenceId ->
  GlobalObjectId ->
  ProcessEpochId ->
  HeraldEpoch ->
  StoreIncarnationId ->
  ControlIndex ->
  LocalPlacement
localPlacement = LocalPlacement

localPlacementDelta :: LocalPlacement -> DeltaId
localPlacementDelta placement = placement.delta

localPlacementSortId :: LocalPlacement -> SortId
localPlacementSortId placement = placement.sortId

localPlacementOccurrenceId :: LocalPlacement -> SortDefinitionOccurrenceId
localPlacementOccurrenceId placement = placement.occurrenceId

localPlacementControllerObject :: LocalPlacement -> GlobalObjectId
localPlacementControllerObject placement = placement.controller

localPlacementProcessEpoch :: LocalPlacement -> ProcessEpochId
localPlacementProcessEpoch placement = placement.processEpoch

localPlacementHeraldEpoch :: LocalPlacement -> HeraldEpoch
localPlacementHeraldEpoch placement = placement.heraldEpoch

localPlacementStoreIncarnation :: LocalPlacement -> StoreIncarnationId
localPlacementStoreIncarnation placement = placement.incarnation

localPlacementControlPrerequisite :: LocalPlacement -> ControlIndex
localPlacementControlPrerequisite placement = placement.controlPrerequisite

-- | A Herald-private operational destination, with no application owner facts.
data SystemViewPlacement = SystemViewPlacement
  { role :: PredefinedSortRole,
    delta :: DeltaId,
    sortId :: SortId,
    occurrenceId :: SortDefinitionOccurrenceId,
    heraldEpoch :: HeraldEpoch,
    incarnation :: StoreIncarnationId
  }
  deriving stock (Eq, Show)

systemViewPlacement ::
  PredefinedSortRole ->
  DeltaId ->
  SortId ->
  SortDefinitionOccurrenceId ->
  HeraldEpoch ->
  StoreIncarnationId ->
  SystemViewPlacement
systemViewPlacement = SystemViewPlacement

systemViewPlacementRole :: SystemViewPlacement -> PredefinedSortRole
systemViewPlacementRole placement = placement.role

systemViewPlacementDelta :: SystemViewPlacement -> DeltaId
systemViewPlacementDelta placement = placement.delta

systemViewPlacementSortId :: SystemViewPlacement -> SortId
systemViewPlacementSortId placement = placement.sortId

systemViewPlacementOccurrenceId :: SystemViewPlacement -> SortDefinitionOccurrenceId
systemViewPlacementOccurrenceId placement = placement.occurrenceId

systemViewPlacementHeraldEpoch :: SystemViewPlacement -> HeraldEpoch
systemViewPlacementHeraldEpoch placement = placement.heraldEpoch

systemViewPlacementStoreIncarnation :: SystemViewPlacement -> StoreIncarnationId
systemViewPlacementStoreIncarnation placement = placement.incarnation

data LocalAdvertisement = LocalAdvertisement
  { advertisedSequence :: PlacementSequence,
    advertisedRoutes :: Map DeltaId DeltaRoute
  }
  deriving stock (Eq, Show)

data RemoteProjection = RemoteProjection
  { acceptedSequence :: Maybe PlacementSequence,
    projectedRoutes :: Map DeltaId DeltaRoute,
    projectionHistory :: Map PlacementSequence (Map DeltaId DeltaRoute)
  }
  deriving stock (Eq, Show)

emptyRemoteProjection :: RemoteProjection
emptyRemoteProjection =
  RemoteProjection
    { acceptedSequence = Nothing,
      projectedRoutes = Map.empty,
      projectionHistory = Map.empty
    }

data State = State
  { applicationPlacements :: Map DeltaId LocalPlacement,
    bootstrapApplicationPlacements :: Map DeltaId LocalPlacement,
    privateSystemViews :: Map DeltaId SystemViewPlacement,
    localOwner :: Maybe HeraldEpoch,
    localAdvertisement :: Maybe LocalAdvertisement,
    localProjectionHistory :: Map PlacementSequence (Map DeltaId DeltaRoute),
    acknowledgedByPeer :: Map HeraldEpoch PlacementSequence,
    remoteProjections :: Map HeraldEpoch RemoteProjection,
    retiredRemoteOwners :: Set HeraldEpoch,
    preparationPlacementChanges :: !(Set DeltaId),
    alignmentPlanChanged :: !Bool,
    alignmentLossChanged :: !Bool
  }
  deriving stock (Eq, Show)

emptyState :: State
emptyState =
  State
    { applicationPlacements = Map.empty,
      bootstrapApplicationPlacements = Map.empty,
      privateSystemViews = Map.empty,
      localOwner = Nothing,
      localAdvertisement = Nothing,
      localProjectionHistory = Map.empty,
      acknowledgedByPeer = Map.empty,
      remoteProjections = Map.empty,
      retiredRemoteOwners = Set.empty,
      preparationPlacementChanges = Set.empty,
      alignmentPlanChanged = False,
      alignmentLossChanged = False
    }

-- | Start an owner even when its first complete route set is empty.
initialState :: HeraldEpoch -> State
initialState owner = emptyState {localOwner = Just owner}

lookupLocalPlacement :: DeltaId -> State -> Maybe LocalPlacement
lookupLocalPlacement delta state = Map.lookup delta state.applicationPlacements

-- | Coalesced changes to exactly the local identity read by child readiness.
-- Snapshot delivery and acknowledgement never change this journal.
takePreparationPlacementChanges :: State -> (Set DeltaId, State)
takePreparationPlacementChanges state =
  let changes = state.preparationPlacementChanges
   in changes `seq` (changes, clearPreparationPlacementChanges state)

clearPreparationPlacementChanges :: State -> State
clearPreparationPlacementChanges state
  | Set.null state.preparationPlacementChanges = state
  | otherwise = state {preparationPlacementChanges = Set.empty}

-- | Coalesced facts read by alignment plan selection. Delivery/report
-- bookkeeping is intentionally absent from this independent consumer journal.
takeAlignmentPlanChange :: State -> (Bool, State)
takeAlignmentPlanChange state =
  let changed = state.alignmentPlanChanged
   in changed `seq` (changed, clearAlignmentPlanChange state)

clearAlignmentPlanChange :: State -> State
clearAlignmentPlanChange state = state {alignmentPlanChanged = False}

takeAlignmentLossChange :: State -> (Bool, State)
takeAlignmentLossChange state =
  let changed = state.alignmentLossChanged
   in changed `seq` (changed, clearAlignmentLossChange state)

clearAlignmentLossChange :: State -> State
clearAlignmentLossChange state = state {alignmentLossChanged = False}

preparationPlacementIdentity :: LocalPlacement -> (SortId, SortDefinitionOccurrenceId, ProcessEpochId, HeraldEpoch, StoreIncarnationId)
preparationPlacementIdentity placement = (placement.sortId, placement.occurrenceId, placement.processEpoch, placement.heraldEpoch, placement.incarnation)

localPlacements :: State -> [LocalPlacement]
localPlacements state = Map.elems state.applicationPlacements

-- | Immutable configured reader-root basis. Dynamic structural placement may
-- advance ahead of an installed cut, so cut-qualified snapshots must start
-- from this retained basis rather than infer it from the live projection.
bootstrapLocalPlacements :: State -> [LocalPlacement]
bootstrapLocalPlacements state = Map.elems state.bootstrapApplicationPlacements

lookupSystemViewPlacement :: DeltaId -> State -> Maybe SystemViewPlacement
lookupSystemViewPlacement delta state = Map.lookup delta state.privateSystemViews

systemViewPlacements :: State -> [SystemViewPlacement]
systemViewPlacements state = Map.elems state.privateSystemViews

newtype PreparedPlacementBootstrap
  = PreparedPlacementBootstrap (Prepared State ())

data PlacementBootstrapError
  = PlacementAlreadyInstalled DeltaId
  | SystemViewPlacementAlreadyInstalled DeltaId
  | PlacementLocalOwnerConflict HeraldEpoch HeraldEpoch
  deriving stock (Eq, Show)

preparePlacementBootstrap ::
  [LocalPlacement] ->
  State ->
  Either PlacementBootstrapError PreparedPlacementBootstrap
preparePlacementBootstrap placements state =
  PreparedPlacementBootstrap
    <$> prepareTransition (installPlacements placements) state

commitPlacementBootstrap :: PreparedPlacementBootstrap -> State
commitPlacementBootstrap (PreparedPlacementBootstrap prepared) =
  fst (commitPrepared prepared)

installPlacements ::
  [LocalPlacement] ->
  State ->
  Either PlacementBootstrapError (State, ())
installPlacements placements state = do
  successor <- foldM (flip insertPlacement) state placements
  Right (successor, ())

insertPlacement ::
  LocalPlacement ->
  State ->
  Either PlacementBootstrapError State
insertPlacement placement state
  | Map.member delta state.applicationPlacements
      || Map.member delta state.privateSystemViews =
      Left (PlacementAlreadyInstalled delta)
  | otherwise = do
      withOwner <- establishOwner placement.heraldEpoch state
      Right
        withOwner
          { applicationPlacements =
              Map.insert delta placement withOwner.applicationPlacements,
            bootstrapApplicationPlacements =
              Map.insert delta placement withOwner.bootstrapApplicationPlacements,
            preparationPlacementChanges = Set.insert delta withOwner.preparationPlacementChanges,
            alignmentPlanChanged = True
          }
  where
    delta = placement.delta

newtype PreparedSystemViewPlacement
  = PreparedSystemViewPlacement (Prepared State ())

prepareSystemViewPlacements ::
  [SystemViewPlacement] ->
  State ->
  Either PlacementBootstrapError PreparedSystemViewPlacement
prepareSystemViewPlacements placements state =
  PreparedSystemViewPlacement
    <$> prepareTransition (installSystemViewPlacements placements) state

commitSystemViewPlacements :: PreparedSystemViewPlacement -> State
commitSystemViewPlacements (PreparedSystemViewPlacement prepared) =
  fst (commitPrepared prepared)

installSystemViewPlacements ::
  [SystemViewPlacement] ->
  State ->
  Either PlacementBootstrapError (State, ())
installSystemViewPlacements placements state = do
  successor <- foldM (flip insertSystemViewPlacement) state placements
  Right (successor, ())

insertSystemViewPlacement ::
  SystemViewPlacement ->
  State ->
  Either PlacementBootstrapError State
insertSystemViewPlacement placement state
  | Map.member delta state.applicationPlacements
      || Map.member delta state.privateSystemViews =
      Left (SystemViewPlacementAlreadyInstalled delta)
  | otherwise = do
      withOwner <- establishOwner placement.heraldEpoch state
      Right
        withOwner
          { privateSystemViews =
              Map.insert delta placement withOwner.privateSystemViews,
            alignmentPlanChanged = True
          }
  where
    delta = placement.delta

establishOwner ::
  HeraldEpoch ->
  State ->
  Either PlacementBootstrapError State
establishOwner owner state = case state.localOwner of
  Nothing -> Right state {localOwner = Just owner}
  Just retained
    | retained == owner -> Right state
    | otherwise -> Left (PlacementLocalOwnerConflict retained owner)

-- | A contradiction between an exact structural reconciliation patch and the
-- local Placement owner.  A ready patch changes only application placements;
-- private system-view placements remain an independently owned fixed set.
data StructuralPlacementPatchError
  = StructuralPlacementKeyMismatch DeltaId DeltaId
  | StructuralPlacementBeforeMismatch DeltaId
  | StructuralPlacementPrivateSystemViewCollision DeltaId
  | StructuralPlacementOwnerMissing
  | StructuralPlacementOwnerMismatch HeraldEpoch HeraldEpoch
  deriving stock (Eq, Show)

newtype PreparedStructuralPlacementPatch
  = PreparedStructuralPlacementPatch
      (Prepared State (Maybe PlacementSnapshot))

prepareStructuralPlacementPatch ::
  ControlIndex ->
  Reconciliation.PlacementPatch ->
  State ->
  Either StructuralPlacementPatchError PreparedStructuralPlacementPatch
prepareStructuralPlacementPatch prerequisite patch state =
  PreparedStructuralPlacementPatch
    <$> prepareTransition applyPatch state
  where
    applyPatch predecessor = do
      patched <-
        foldM
          applyStructuralChange
          predecessor
          (Map.toAscList (Reconciliation.placementPatchChanges patch))
      -- Structural application updates the live local projection, but it must
      -- not advertise that projection before an all-member topology cut has
      -- qualified the exact winning structural history.  The structural
      -- coordinator advances the retained wire snapshot explicitly from that
      -- cut projection.
      Right (patched, Nothing)

    applyStructuralChange current (delta, change) = do
      let before = Reconciliation.exactChangeBefore change
          after = Reconciliation.exactChangeAfter change
          installed = Map.lookup delta current.applicationPlacements
      requirePlacementBefore delta before installed
      case after of
        Nothing ->
          Right
            current
              { applicationPlacements =
                  Map.delete delta current.applicationPlacements,
                preparationPlacementChanges =
                  if Map.member delta current.applicationPlacements
                    then Set.insert delta current.preparationPlacementChanges
                    else current.preparationPlacementChanges
              }
        Just specification
          | Reconciliation.dynamicStoreDelta specification /= delta ->
              Left
                ( StructuralPlacementKeyMismatch
                    delta
                    (Reconciliation.dynamicStoreDelta specification)
                )
          | Map.member delta current.privateSystemViews ->
              Left (StructuralPlacementPrivateSystemViewCollision delta)
          | otherwise -> do
              requireStructuralPlacementOwner specification current
              let placement = structuralLocalPlacement prerequisite specification
              Right
                current
                  { applicationPlacements =
                      Map.insert delta placement current.applicationPlacements,
                    preparationPlacementChanges =
                      if fmap preparationPlacementIdentity installed == Just (preparationPlacementIdentity placement)
                        then current.preparationPlacementChanges
                        else Set.insert delta current.preparationPlacementChanges
                  }

preparedStructuralPlacementSnapshot ::
  PreparedStructuralPlacementPatch ->
  Maybe PlacementSnapshot
preparedStructuralPlacementSnapshot
  (PreparedStructuralPlacementPatch prepared) = preparedOutput prepared

commitStructuralPlacementPatch ::
  PreparedStructuralPlacementPatch ->
  (State, Maybe PlacementSnapshot)
commitStructuralPlacementPatch (PreparedStructuralPlacementPatch prepared) =
  commitPrepared prepared

requirePlacementBefore ::
  DeltaId ->
  Maybe Reconciliation.DynamicLocalStoreSpec ->
  Maybe LocalPlacement ->
  Either StructuralPlacementPatchError ()
requirePlacementBefore delta expected installed =
  case (expected, installed) of
    (Nothing, Nothing) -> Right ()
    (Just specification, Just placement)
      | placementMatchesDynamic specification placement -> Right ()
    _ -> Left (StructuralPlacementBeforeMismatch delta)

requireStructuralPlacementOwner ::
  Reconciliation.DynamicLocalStoreSpec ->
  State ->
  Either StructuralPlacementPatchError ()
requireStructuralPlacementOwner specification state =
  case state.localOwner of
    Just owner
      | owner == Reconciliation.dynamicStoreHerald specification -> Right ()
      | otherwise ->
          Left
            ( StructuralPlacementOwnerMismatch
                owner
                (Reconciliation.dynamicStoreHerald specification)
            )
    Nothing -> Left StructuralPlacementOwnerMissing

placementMatchesDynamic ::
  Reconciliation.DynamicLocalStoreSpec ->
  LocalPlacement ->
  Bool
placementMatchesDynamic specification placement =
  placement.delta == Reconciliation.dynamicStoreDelta specification
    && placement.sortId
      == sortOccurrenceSortId (Reconciliation.dynamicStoreSort specification)
    && placement.occurrenceId
      == sortOccurrenceDefinition (Reconciliation.dynamicStoreSort specification)
    && placement.controller == Reconciliation.dynamicStoreController specification
    && placement.processEpoch == Reconciliation.dynamicStoreProcess specification
    && placement.heraldEpoch == Reconciliation.dynamicStoreHerald specification
    && placement.incarnation == Reconciliation.dynamicStoreIncarnation specification

structuralLocalPlacement ::
  ControlIndex ->
  Reconciliation.DynamicLocalStoreSpec ->
  LocalPlacement
structuralLocalPlacement prerequisite specification =
  LocalPlacement
    { delta = Reconciliation.dynamicStoreDelta specification,
      sortId = sortOccurrenceSortId (Reconciliation.dynamicStoreSort specification),
      occurrenceId =
        sortOccurrenceDefinition (Reconciliation.dynamicStoreSort specification),
      controller = Reconciliation.dynamicStoreController specification,
      processEpoch = Reconciliation.dynamicStoreProcess specification,
      heraldEpoch = Reconciliation.dynamicStoreHerald specification,
      incarnation = Reconciliation.dynamicStoreIncarnation specification,
      controlPrerequisite = prerequisite
    }

data LocalSnapshotDisposition
  = InitialLocalSnapshot
  | ReofferedLocalSnapshot
  | AdvancedLocalSnapshot
  deriving stock (Eq, Ord, Show)

data LocalSnapshotResult = LocalSnapshotResult
  { disposition :: LocalSnapshotDisposition,
    snapshot :: PlacementSnapshot
  }
  deriving stock (Eq, Show)

localSnapshotDisposition :: LocalSnapshotResult -> LocalSnapshotDisposition
localSnapshotDisposition result = result.disposition

localSnapshotMessage :: LocalSnapshotResult -> PlacementSnapshot
localSnapshotMessage result = result.snapshot

newtype PreparedLocalSnapshot
  = PreparedLocalSnapshot (Prepared State LocalSnapshotResult)

prepareLocalSnapshot ::
  State ->
  Either PlacementProblem PreparedLocalSnapshot
prepareLocalSnapshot = prepareLocalSnapshotWithDiagnostics DiagnosticChecksEnabled

prepareLocalSnapshotWithDiagnostics ::
  DiagnosticChecks ->
  State ->
  Either PlacementProblem PreparedLocalSnapshot
prepareLocalSnapshotWithDiagnostics diagnostics state = do
  runDiagnosticCheck diagnostics (validatePlacementState state)
  PreparedLocalSnapshot <$> prepareTransition (offerLocalSnapshot diagnostics) state

-- | Advance the authoritative wire projection from an exact, externally
-- qualified route set.  Structural callers supply the projection reconstructed
-- at an installed topology cut; this is deliberately distinct from the live
-- application placement map, which may already be causally ahead.
prepareLocalSnapshotForRoutes ::
  [DeltaRoute] ->
  State ->
  Either PlacementProblem PreparedLocalSnapshot
prepareLocalSnapshotForRoutes = prepareLocalSnapshotForRoutesWithDiagnostics DiagnosticChecksEnabled

prepareLocalSnapshotForRoutesWithDiagnostics ::
  DiagnosticChecks ->
  [DeltaRoute] ->
  State ->
  Either PlacementProblem PreparedLocalSnapshot
prepareLocalSnapshotForRoutesWithDiagnostics diagnostics routes state = do
  runDiagnosticCheck diagnostics (validatePlacementState state)
  PreparedLocalSnapshot
    <$> prepareTransition (offerLocalSnapshotForRoutes diagnostics routes) state

-- | Reoffer the last qualified advertisement without consulting live routes.
-- Initialization installs revision one, so absence at a serving reconnect or
-- repair boundary is an invariant contradiction.
prepareRetainedLocalSnapshot ::
  State ->
  Either PlacementProblem PreparedLocalSnapshot
prepareRetainedLocalSnapshot = prepareRetainedLocalSnapshotWithDiagnostics DiagnosticChecksEnabled

prepareRetainedLocalSnapshotWithDiagnostics ::
  DiagnosticChecks ->
  State ->
  Either PlacementProblem PreparedLocalSnapshot
prepareRetainedLocalSnapshotWithDiagnostics diagnostics state = do
  runDiagnosticCheck diagnostics (validatePlacementState state)
  PreparedLocalSnapshot <$> prepareTransition reofferRetainedLocalSnapshot state

preparedLocalSnapshotResult :: PreparedLocalSnapshot -> LocalSnapshotResult
preparedLocalSnapshotResult (PreparedLocalSnapshot prepared) = preparedOutput prepared

commitLocalSnapshot :: PreparedLocalSnapshot -> (State, LocalSnapshotResult)
commitLocalSnapshot (PreparedLocalSnapshot prepared) = commitPrepared prepared

offerLocalSnapshot ::
  DiagnosticChecks ->
  State ->
  Either PlacementProblem (State, LocalSnapshotResult)
offerLocalSnapshot diagnostics state =
  offerLocalSnapshotForRoutes diagnostics (Map.elems (currentLocalRoutes state)) state

offerLocalSnapshotForRoutes ::
  DiagnosticChecks ->
  [DeltaRoute] ->
  State ->
  Either PlacementProblem (State, LocalSnapshotResult)
offerLocalSnapshotForRoutes diagnostics suppliedRoutes state = do
  owner <- requireLocalOwner state
  normalized <-
    mapShapeProblem
      (placementSnapshot owner firstPlacementSequence suppliedRoutes)
  let routes = routeMap (placementSnapshotRoutes normalized)
      (sequenceNumber, disposition) = case state.localAdvertisement of
        Nothing -> (firstPlacementSequence, InitialLocalSnapshot)
        Just advertised
          | advertised.advertisedRoutes == routes ->
              (advertised.advertisedSequence, ReofferedLocalSnapshot)
          | otherwise ->
              ( nextPlacementSequence advertised.advertisedSequence,
                AdvancedLocalSnapshot
              )
  snapshot <- mapShapeProblem (placementSnapshot owner sequenceNumber (Map.elems routes))
  let successor =
        state
          { localAdvertisement =
              Just
                LocalAdvertisement
                  { advertisedSequence = sequenceNumber,
                    advertisedRoutes = routes
                  },
            localProjectionHistory =
              Map.insert sequenceNumber routes state.localProjectionHistory,
            alignmentPlanChanged = state.alignmentPlanChanged || disposition /= ReofferedLocalSnapshot
          }
      result = LocalSnapshotResult {disposition, snapshot}
  runDiagnosticCheck diagnostics (validatePlacementState successor)
  Right (successor, result)

reofferRetainedLocalSnapshot ::
  State ->
  Either PlacementProblem (State, LocalSnapshotResult)
reofferRetainedLocalSnapshot state = do
  owner <- requireLocalOwner state
  advertised <-
    maybe
      (invariant PlacementLocalAdvertisementMissing)
      Right
      state.localAdvertisement
  snapshot <-
    mapShapeProblem
      ( placementSnapshot
          owner
          advertised.advertisedSequence
          (Map.elems advertised.advertisedRoutes)
      )
  Right
    ( state,
      LocalSnapshotResult
        { disposition = ReofferedLocalSnapshot,
          snapshot
        }
    )

data PlacementAckDisposition
  = PlacementAckAdvanced
  | PlacementAckStale
  deriving stock (Eq, Ord, Show)

newtype PreparedPlacementAcknowledgement
  = PreparedPlacementAcknowledgement
      (Prepared State PlacementAckDisposition)

preparePlacementAcknowledgement ::
  HeraldEpoch ->
  PlacementAcknowledgement ->
  State ->
  Either PlacementProblem PreparedPlacementAcknowledgement
preparePlacementAcknowledgement = preparePlacementAcknowledgementWithDiagnostics DiagnosticChecksEnabled

preparePlacementAcknowledgementWithDiagnostics ::
  DiagnosticChecks ->
  HeraldEpoch ->
  PlacementAcknowledgement ->
  State ->
  Either PlacementProblem PreparedPlacementAcknowledgement
preparePlacementAcknowledgementWithDiagnostics diagnostics peer acknowledgement state = do
  runDiagnosticCheck diagnostics (validatePlacementState state)
  PreparedPlacementAcknowledgement
    <$> prepareTransition (applyPlacementAcknowledgement diagnostics peer acknowledgement) state

preparedPlacementAckDisposition ::
  PreparedPlacementAcknowledgement ->
  PlacementAckDisposition
preparedPlacementAckDisposition (PreparedPlacementAcknowledgement prepared) =
  preparedOutput prepared

commitPlacementAcknowledgement ::
  PreparedPlacementAcknowledgement ->
  (State, PlacementAckDisposition)
commitPlacementAcknowledgement (PreparedPlacementAcknowledgement prepared) =
  commitPrepared prepared

applyPlacementAcknowledgement ::
  DiagnosticChecks ->
  HeraldEpoch ->
  PlacementAcknowledgement ->
  State ->
  Either PlacementProblem (State, PlacementAckDisposition)
applyPlacementAcknowledgement diagnostics peer acknowledgement state = do
  owner <- requireLocalOwner state
  if Set.member peer state.retiredRemoteOwners
    then protocol (PlacementRetiredRemoteOwner peer)
    else
      if placementAcknowledgementOwner acknowledgement /= owner
        then
          protocol
            ( PlacementAcknowledgementOwnerMismatch
                owner
                (placementAcknowledgementOwner acknowledgement)
            )
        else case state.localAdvertisement of
          Nothing -> protocol PlacementAcknowledgementBeforeAdvertisement
          Just advertised
            | placementAcknowledgementSequence acknowledgement
                > advertised.advertisedSequence ->
                protocol
                  ( PlacementAcknowledgementAhead
                      (placementAcknowledgementSequence acknowledgement)
                      advertised.advertisedSequence
                  )
            | maybe False (>= placementAcknowledgementSequence acknowledgement) previous ->
                Right (state, PlacementAckStale)
            | otherwise -> do
                let successor =
                      state
                        { acknowledgedByPeer =
                            Map.insert
                              peer
                              (placementAcknowledgementSequence acknowledgement)
                              state.acknowledgedByPeer
                        }
                runDiagnosticCheck diagnostics (validatePlacementState successor)
                Right (successor, PlacementAckAdvanced)
  where
    previous = Map.lookup peer state.acknowledgedByPeer

data RemotePlacementDisposition
  = RemotePlacementAccepted
  | RemotePlacementDuplicate
  | RemotePlacementStale
  deriving stock (Eq, Ord, Show)

data RemotePlacementResult = RemotePlacementResult
  { disposition :: RemotePlacementDisposition,
    acknowledgement :: Maybe PlacementAcknowledgement
  }
  deriving stock (Eq, Show)

remotePlacementDisposition :: RemotePlacementResult -> RemotePlacementDisposition
remotePlacementDisposition result = result.disposition

remotePlacementAcknowledgement ::
  RemotePlacementResult ->
  Maybe PlacementAcknowledgement
remotePlacementAcknowledgement result = result.acknowledgement

newtype PreparedRemotePlacement
  = PreparedRemotePlacement (Prepared State RemotePlacementResult)

prepareRemotePlacement ::
  PlacementUpdate ->
  State ->
  Either PlacementProblem PreparedRemotePlacement
prepareRemotePlacement = prepareRemotePlacementWithDiagnostics DiagnosticChecksEnabled

prepareRemotePlacementWithDiagnostics ::
  DiagnosticChecks ->
  PlacementUpdate ->
  State ->
  Either PlacementProblem PreparedRemotePlacement
prepareRemotePlacementWithDiagnostics diagnostics update state = do
  runDiagnosticCheck diagnostics (validatePlacementState state)
  PreparedRemotePlacement <$> prepareTransition (applyRemotePlacement diagnostics update) state

preparedRemotePlacementResult :: PreparedRemotePlacement -> RemotePlacementResult
preparedRemotePlacementResult (PreparedRemotePlacement prepared) =
  preparedOutput prepared

commitRemotePlacement ::
  PreparedRemotePlacement ->
  (State, RemotePlacementResult)
commitRemotePlacement (PreparedRemotePlacement prepared) = commitPrepared prepared

applyRemotePlacement ::
  DiagnosticChecks ->
  PlacementUpdate ->
  State ->
  Either PlacementProblem (State, RemotePlacementResult)
applyRemotePlacement diagnostics (FullPlacementSnapshot snapshot) state = do
  let owner = placementSnapshotOwner snapshot
      sequenceNumber = placementSnapshotSequence snapshot
  if Set.member owner state.retiredRemoteOwners
    then protocol (PlacementRetiredRemoteOwner owner)
    else case state.localOwner of
      Just local | local == owner -> protocol (PlacementRemoteOwnerIsLocal owner)
      _ -> Right ()
  let projection =
        Map.findWithDefault emptyRemoteProjection owner state.remoteProjections
  applyRemoteSnapshot diagnostics owner sequenceNumber snapshot projection state

-- | A full snapshot is the complete representation of one placement cut. An
-- older snapshot is stale; an equal current cut is a duplicate; an unequal
-- current cut is a contradiction; and a newer snapshot atomically supersedes
-- the owner's visible projection while retaining its historical cut.
applyRemoteSnapshot ::
  DiagnosticChecks ->
  HeraldEpoch ->
  PlacementSequence ->
  PlacementSnapshot ->
  RemoteProjection ->
  State ->
  Either PlacementProblem (State, RemotePlacementResult)
applyRemoteSnapshot diagnostics owner sequenceNumber snapshot projection state =
  case projection.acceptedSequence of
    Just accepted
      | sequenceNumber < accepted ->
          Right (state, remoteResult owner projection RemotePlacementStale)
      | sequenceNumber == accepted ->
          if routeMap (placementSnapshotRoutes snapshot) == projection.projectedRoutes
            then Right (state, remoteResult owner projection RemotePlacementDuplicate)
            else protocol (PlacementSequenceConflict owner sequenceNumber)
    _ -> do
      let routes = routeMap (placementSnapshotRoutes snapshot)
      case Map.lookup sequenceNumber projection.projectionHistory of
        Just retained
          | retained /= routes ->
              protocol (PlacementSequenceConflict owner sequenceNumber)
        _ -> Right ()
      let advanced =
            projection
              { acceptedSequence = Just sequenceNumber,
                projectedRoutes = routes,
                projectionHistory =
                  Map.insert sequenceNumber routes projection.projectionHistory
              }
      installRemote diagnostics owner advanced RemotePlacementAccepted state

installRemote ::
  DiagnosticChecks ->
  HeraldEpoch ->
  RemoteProjection ->
  RemotePlacementDisposition ->
  State ->
  Either PlacementProblem (State, RemotePlacementResult)
installRemote diagnostics owner projection disposition state = do
  let successor =
        state
          { remoteProjections =
              Map.insert owner projection state.remoteProjections,
            alignmentPlanChanged = True,
            alignmentLossChanged = True
          }
  runDiagnosticCheck diagnostics (validatePlacementState successor)
  Right (successor, remoteResult owner projection disposition)

-- | The exact live remote projection removed by one permanent membership
-- contraction.  Historical revisions remain available for predecessor-cut
-- validation; these routes are only the destinations which ceased to be live.
data RetiredRemotePlacement = RetiredRemotePlacement HeraldEpoch [DeltaRoute]
  deriving stock (Eq, Show)

retiredRemotePlacementOwner :: RetiredRemotePlacement -> HeraldEpoch
retiredRemotePlacementOwner (RetiredRemotePlacement owner _) = owner

retiredRemotePlacementRoutes :: RetiredRemotePlacement -> [DeltaRoute]
retiredRemotePlacementRoutes (RetiredRemotePlacement _ routes) = routes

newtype PreparedRemotePlacementRetirement
  = PreparedRemotePlacementRetirement
      (Prepared State RetiredRemotePlacement)

prepareRemotePlacementRetirement ::
  HeraldEpoch ->
  State ->
  Either PlacementProblem PreparedRemotePlacementRetirement
prepareRemotePlacementRetirement = prepareRemotePlacementRetirementWithDiagnostics DiagnosticChecksEnabled

prepareRemotePlacementRetirementWithDiagnostics ::
  DiagnosticChecks ->
  HeraldEpoch ->
  State ->
  Either PlacementProblem PreparedRemotePlacementRetirement
prepareRemotePlacementRetirementWithDiagnostics diagnostics owner state = do
  runDiagnosticCheck diagnostics (validatePlacementState state)
  PreparedRemotePlacementRetirement
    <$> prepareTransition (retireRemotePlacement diagnostics owner) state

preparedRemotePlacementRetirement ::
  PreparedRemotePlacementRetirement ->
  RetiredRemotePlacement
preparedRemotePlacementRetirement (PreparedRemotePlacementRetirement prepared) =
  preparedOutput prepared

commitRemotePlacementRetirement ::
  PreparedRemotePlacementRetirement ->
  (State, RetiredRemotePlacement)
commitRemotePlacementRetirement (PreparedRemotePlacementRetirement prepared) =
  commitPrepared prepared

retireRemotePlacement ::
  DiagnosticChecks ->
  HeraldEpoch ->
  State ->
  Either PlacementProblem (State, RetiredRemotePlacement)
retireRemotePlacement diagnostics owner state = do
  case state.localOwner of
    Just local | local == owner -> protocol (PlacementRemoteOwnerIsLocal owner)
    _ -> Right ()
  if Set.member owner state.retiredRemoteOwners
    then Right (state, RetiredRemotePlacement owner [])
    else do
      let projection = Map.findWithDefault emptyRemoteProjection owner state.remoteProjections
          retiredRoutes = Map.elems projection.projectedRoutes
          retainedProjection =
            projection
              { acceptedSequence = Nothing,
                projectedRoutes = Map.empty
              }
          successor =
            state
              { acknowledgedByPeer = Map.delete owner state.acknowledgedByPeer,
                remoteProjections = Map.insert owner retainedProjection state.remoteProjections,
                retiredRemoteOwners = Set.insert owner state.retiredRemoteOwners,
                alignmentPlanChanged = True,
                alignmentLossChanged = state.alignmentLossChanged || projection.acceptedSequence /= Nothing
              }
      runDiagnosticCheck diagnostics (validatePlacementState successor)
      Right (successor, RetiredRemotePlacement owner retiredRoutes)

remotePlacementOwnerRetired :: HeraldEpoch -> State -> Bool
remotePlacementOwnerRetired owner state = Set.member owner state.retiredRemoteOwners

remoteResult ::
  HeraldEpoch ->
  RemoteProjection ->
  RemotePlacementDisposition ->
  RemotePlacementResult
remoteResult owner projection disposition =
  RemotePlacementResult
    { disposition,
      acknowledgement =
        placementAcknowledgement owner <$> projection.acceptedSequence
    }

routeMap :: [DeltaRoute] -> Map DeltaId DeltaRoute
routeMap routes = Map.fromList [(deltaRouteDelta route, route) | route <- routes]

remotePlacementSequence :: HeraldEpoch -> State -> Maybe PlacementSequence
remotePlacementSequence owner state = do
  projection <- Map.lookup owner state.remoteProjections
  projection.acceptedSequence

remotePlacementOwners :: State -> [HeraldEpoch]
remotePlacementOwners state =
  [ owner
  | (owner, projection) <- Map.toAscList state.remoteProjections,
    Just _ <- [projection.acceptedSequence],
    Set.notMember owner state.retiredRemoteOwners
  ]

remotePlacementRoutes :: HeraldEpoch -> State -> [DeltaRoute]
remotePlacementRoutes owner state =
  maybe [] (\projection -> Map.elems projection.projectedRoutes) (Map.lookup owner state.remoteProjections)

lookupRemotePlacements :: DeltaId -> State -> [(HeraldEpoch, DeltaRoute)]
lookupRemotePlacements delta state =
  [ (owner, route)
  | (owner, projection) <- Map.toAscList state.remoteProjections,
    Just route <- [Map.lookup delta projection.projectedRoutes]
  ]

localPlacementSequence :: State -> Maybe PlacementSequence
localPlacementSequence state =
  (\advertisement -> advertisement.advertisedSequence) <$> state.localAdvertisement

placementAcknowledgedThrough ::
  HeraldEpoch ->
  State ->
  Maybe PlacementSequence
placementAcknowledgedThrough peer state =
  Map.lookup peer state.acknowledgedByPeer

-- | A complete physical-placement cut must name one retained revision for
-- every member of its exact checked generation. Missing history is a
-- dependency, not permission to substitute a newer projection.
data PlacementCutProblem
  = PlacementCutLocalOwnerMissing
  | PlacementCutRevisionUnavailable HeraldEpoch PlacementSequence
  | PlacementCutShapeRejected AlignmentShapeError
  deriving stock (Eq, Show)

currentPhysicalPlacementRevisionVector ::
  HeraldMembershipGeneration ->
  State ->
  Either PlacementCutProblem PhysicalPlacementRevisionVector
currentPhysicalPlacementRevisionVector generation state = do
  owner <- maybe (Left PlacementCutLocalOwnerMissing) Right state.localOwner
  entries <-
    traverse
      (currentRevision owner)
      ( NonEmpty.toList
          (heraldMembershipGenerationActiveHeraldEpochs generation)
      )
  either
    (Left . PlacementCutShapeRejected)
    Right
    (physicalPlacementRevisionVector generation entries)
  where
    currentRevision owner herald
      | herald == owner =
          maybe
            (Left (PlacementCutRevisionUnavailable herald firstPlacementSequence))
            (\revision -> Right (herald, revision))
            (localPlacementSequence state)
      | otherwise =
          maybe
            (Left (PlacementCutRevisionUnavailable herald firstPlacementSequence))
            (\revision -> Right (herald, revision))
            (remotePlacementSequence herald state)

placementRoutesAtRevision ::
  HeraldEpoch ->
  PlacementSequence ->
  State ->
  Maybe [DeltaRoute]
placementRoutesAtRevision owner revision state
  | Just owner == state.localOwner =
      Map.elems <$> Map.lookup revision state.localProjectionHistory
  | otherwise = do
      projection <- Map.lookup owner state.remoteProjections
      Map.elems <$> Map.lookup revision projection.projectionHistory

placementRoutesAtVector ::
  PhysicalPlacementRevisionVector ->
  State ->
  Either PlacementCutProblem [(HeraldEpoch, [DeltaRoute])]
placementRoutesAtVector vector state =
  traverse resolve (NonEmpty.toList (physicalPlacementRevisionEntries vector))
  where
    resolve (owner, revision) =
      maybe
        (Left (PlacementCutRevisionUnavailable owner revision))
        (\routes -> Right (owner, routes))
        (placementRoutesAtRevision owner revision state)

-- | Exact complete snapshots, including empty revisions and retired owners.
-- The retained maps have unique delta keys; rebuilding their checked snapshot
-- representation therefore cannot introduce a duplicate route.
retainedPlacementSnapshots :: State -> [PlacementSnapshot]
retainedPlacementSnapshots state =
  [ checkedSnapshot owner revision routes
  | (owner, history) <- Map.toAscList histories,
    (revision, routes) <- Map.toAscList history
  ]
  where
    histories =
      maybe
        id
        (\owner -> Map.insert owner state.localProjectionHistory)
        state.localOwner
        (Map.map (.projectionHistory) state.remoteProjections)
    checkedSnapshot owner revision routes =
      either
        (error . ("retained placement snapshot invariant: " <>) . show)
        id
        (placementSnapshot owner revision (Map.elems routes))

-- | Retain authenticated join-history evidence without admitting a live peer
-- advertisement. Historical coordinates never advance the current projection
-- or restore a retired owner. A local coordinate can only corroborate history
-- which this owner already produced itself.
retainHistoricalPlacementSnapshot ::
  PlacementSnapshot ->
  State ->
  Either PlacementProblem State
retainHistoricalPlacementSnapshot snapshot state = do
  validatePlacementState state
  case placementRoutesAtRevision owner revision state of
    Just retained
      | retained == placementSnapshotRoutes snapshot -> Right state
      | otherwise -> protocol (PlacementSequenceConflict owner revision)
    Nothing
      | Just owner == state.localOwner ->
          protocol (PlacementRemoteOwnerIsLocal owner)
      | otherwise -> do
          let projection =
                Map.findWithDefault emptyRemoteProjection owner state.remoteProjections
              retained =
                projection
                  { projectionHistory =
                      Map.insert
                        revision
                        (routeMap (placementSnapshotRoutes snapshot))
                        projection.projectionHistory
                  }
              successor =
                state {remoteProjections = Map.insert owner retained state.remoteProjections, alignmentPlanChanged = True}
          validatePlacementState successor
          Right successor
  where
    owner = placementSnapshotOwner snapshot
    revision = placementSnapshotSequence snapshot

-- | Whether one owner has ever advertised this exact destination identity for
-- the supplied sort occurrence.  Frozen publication routes may legitimately
-- outlive the current Store incarnation, so admission consults retained
-- cut-qualified history rather than only the live projection.
retainedPlacementRouteMatches ::
  HeraldEpoch ->
  DeltaId ->
  StoreIncarnationId ->
  SortId ->
  SortDefinitionOccurrenceId ->
  State ->
  Bool
retainedPlacementRouteMatches owner delta incarnation sortId occurrence state =
  any revisionMatches (Map.elems retainedHistory)
  where
    retainedHistory
      | Just owner == state.localOwner = state.localProjectionHistory
      | otherwise =
          maybe Map.empty (.projectionHistory) (Map.lookup owner state.remoteProjections)
    revisionMatches routes =
      case Map.lookup delta routes of
        Nothing -> False
        Just route ->
          deltaRouteStoreIncarnation route == incarnation
            && deltaRouteSortId route == sortId
            && deltaRouteOccurrenceId route == occurrence

-- | Routes in the owners' current qualified advertisements and projections.
-- Superseded revisions remain available through the retained-history accessors
-- for frozen-cut authentication, but no longer constitute live route work.
currentPlacementRouteEntries ::
  State -> [(HeraldEpoch, PlacementSequence, DeltaRoute)]
currentPlacementRouteEntries state =
  sortOn
    (\(owner, revision, route) -> (owner, revision, deltaRouteDelta route))
    (localEntries <> remoteEntries)
  where
    localEntries = case (state.localOwner, state.localAdvertisement) of
      (Just owner, Just advertisement) ->
        [ (owner, advertisement.advertisedSequence, route)
        | route <- Map.elems advertisement.advertisedRoutes
        ]
      _ -> []
    remoteEntries =
      [ (owner, revision, route)
      | (owner, projection) <- Map.toAscList state.remoteProjections,
        Just revision <- [projection.acceptedSequence],
        route <- Map.elems projection.projectedRoutes
      ]

-- | Every route retained in a local or remote placement history, ordered by
-- owner, revision, and delta. Frozen-cut admission and validation use this view
-- to authenticate routes which legitimately outlive the current projection; it
-- does not expose either projection map for mutation.
retainedPlacementRouteEntries ::
  State -> [(HeraldEpoch, PlacementSequence, DeltaRoute)]
retainedPlacementRouteEntries state =
  sortOn
    (\(owner, revision, route) -> (owner, revision, deltaRouteDelta route))
    (localEntries <> remoteEntries)
  where
    localEntries = case state.localOwner of
      Nothing -> []
      Just owner -> historyEntries owner state.localProjectionHistory
    remoteEntries =
      [ (owner, revision, route)
      | (owner, projection) <- Map.toAscList state.remoteProjections,
        (revision, routes) <- Map.toAscList projection.projectionHistory,
        route <- Map.elems routes
      ]
    historyEntries owner history =
      [ (owner, revision, route)
      | (revision, routes) <- Map.toAscList history,
        route <- Map.elems routes
      ]

currentLocalRoutes :: State -> Map DeltaId DeltaRoute
currentLocalRoutes state =
  Map.union applicationRoutes systemViewRoutes
  where
    applicationRoutes =
      Map.map
        ( \placement ->
            applicationDeltaRoute
              placement.delta
              placement.sortId
              placement.occurrenceId
              placement.controller
              placement.processEpoch
              placement.incarnation
              placement.controlPrerequisite
        )
        state.applicationPlacements
    systemViewRoutes =
      Map.map
        ( \placement ->
            privateSystemViewDeltaRoute
              placement.role
              placement.delta
              placement.sortId
              placement.occurrenceId
              placement.incarnation
        )
        state.privateSystemViews

data PlacementProtocolViolation
  = PlacementRemoteOwnerIsLocal HeraldEpoch
  | PlacementRetiredRemoteOwner HeraldEpoch
  | PlacementSequenceConflict HeraldEpoch PlacementSequence
  | PlacementAcknowledgementOwnerMismatch HeraldEpoch HeraldEpoch
  | PlacementAcknowledgementBeforeAdvertisement
  | PlacementAcknowledgementAhead PlacementSequence PlacementSequence
  deriving stock (Eq, Show)

data PlacementInvariantViolation
  = PlacementLocalOwnerMissing
  | PlacementLocalAdvertisementMissing
  | PlacementBootstrapProjectionMismatch
  | PlacementLocalRouteOwnerMismatch HeraldEpoch HeraldEpoch
  | PlacementLocalAdvertisedRevisionMissing PlacementSequence
  | PlacementLocalAdvertisedProjectionMismatch PlacementSequence
  | PlacementLocalAcknowledgementAhead HeraldEpoch PlacementSequence PlacementSequence
  | PlacementRemoteProjectionForLocalOwner HeraldEpoch
  | PlacementRemoteAcceptedProjectionMissing HeraldEpoch PlacementSequence
  | PlacementRemoteAcceptedProjectionMismatch HeraldEpoch PlacementSequence
  | PlacementRemoteRouteKeyMismatch HeraldEpoch DeltaId
  | PlacementRetiredRemoteOwnerStillLive HeraldEpoch
  | PlacementNominalShapeContradiction PlacementShapeProblem
  deriving stock (Eq, Show)

data PlacementProblem
  = PlacementProtocolProblem PlacementProtocolViolation
  | PlacementInvariantProblem PlacementInvariantViolation
  deriving stock (Eq, Show)

requireLocalOwner :: State -> Either PlacementProblem HeraldEpoch
requireLocalOwner state =
  maybe
    (invariant PlacementLocalOwnerMissing)
    Right
    state.localOwner

mapShapeProblem ::
  Either PlacementShapeProblem value ->
  Either PlacementProblem value
mapShapeProblem =
  either
    (invariant . PlacementNominalShapeContradiction)
    Right

protocol :: PlacementProtocolViolation -> Either PlacementProblem value
protocol = Left . PlacementProtocolProblem

invariant :: PlacementInvariantViolation -> Either PlacementProblem value
invariant = Left . PlacementInvariantProblem

validatePlacementState :: State -> Either PlacementProblem ()
validatePlacementState state = do
  mapM_
    validateBootstrapApplicationPlacement
    (Map.toAscList state.bootstrapApplicationPlacements)
  mapM_ validateApplicationOwner (Map.elems state.applicationPlacements)
  mapM_ validateSystemViewOwner (Map.elems state.privateSystemViews)
  case state.localAdvertisement of
    Nothing -> Right ()
    Just advertised -> do
      _ <- requireLocalOwner state
      case Map.lookup advertised.advertisedSequence state.localProjectionHistory of
        Nothing ->
          invariant
            (PlacementLocalAdvertisedRevisionMissing advertised.advertisedSequence)
        Just retained
          | retained == advertised.advertisedRoutes -> Right ()
          | otherwise ->
              invariant
                (PlacementLocalAdvertisedProjectionMismatch advertised.advertisedSequence)
      mapM_
        (validateAcknowledgement advertised.advertisedSequence)
        (Map.toAscList state.acknowledgedByPeer)
  mapM_ validateRemote (Map.toAscList state.remoteProjections)
  mapM_ validateRetiredRemoteOwner (Set.toAscList state.retiredRemoteOwners)
  where
    -- The configured-bootstrap basis is immutable history used to rebuild
    -- cut-qualified placement projections.  Process End legitimately removes
    -- its routes from the live application projection without erasing that
    -- history, so validate the retained coordinates in their own right rather
    -- than requiring them to remain live.
    validateBootstrapApplicationPlacement (delta, placement)
      | delta /= placement.delta =
          invariant PlacementBootstrapProjectionMismatch
      | otherwise = validateApplicationOwner placement

    validateApplicationOwner placement = validateOwner placement.heraldEpoch
    validateSystemViewOwner placement = validateOwner placement.heraldEpoch
    validateOwner observed = case state.localOwner of
      Nothing -> invariant PlacementLocalOwnerMissing
      Just expected
        | observed == expected -> Right ()
        | otherwise ->
            invariant (PlacementLocalRouteOwnerMismatch expected observed)

    validateAcknowledgement advertised (peer, acknowledged)
      | acknowledged <= advertised = Right ()
      | otherwise =
          invariant
            (PlacementLocalAcknowledgementAhead peer acknowledged advertised)

    validateRemote (owner, projection) = do
      case state.localOwner of
        Just local
          | local == owner ->
              invariant (PlacementRemoteProjectionForLocalOwner owner)
        _ -> Right ()
      case projection.acceptedSequence of
        Nothing -> Right ()
        Just accepted ->
          case Map.lookup accepted projection.projectionHistory of
            Nothing ->
              invariant
                (PlacementRemoteAcceptedProjectionMissing owner accepted)
            Just retained
              | retained == projection.projectedRoutes -> Right ()
              | otherwise ->
                  invariant
                    (PlacementRemoteAcceptedProjectionMismatch owner accepted)
      mapM_
        (validateRemoteRoute owner)
        (Map.toAscList projection.projectedRoutes)

    validateRemoteRoute owner (delta, route)
      | deltaRouteDelta route == delta = Right ()
      | otherwise = invariant (PlacementRemoteRouteKeyMismatch owner delta)

    validateRetiredRemoteOwner owner =
      case (Map.lookup owner state.acknowledgedByPeer, Map.lookup owner state.remoteProjections) of
        (Nothing, Nothing) -> Right ()
        (Nothing, Just projection)
          | projection.acceptedSequence == Nothing
              && Map.null projection.projectedRoutes ->
              Right ()
        _ -> invariant (PlacementRetiredRemoteOwnerStillLive owner)
