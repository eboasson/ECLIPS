{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Typed, codec-free placement vocabulary.
--
-- Application destinations and Herald-private system views are deliberately
-- different constructors. The latter cannot carry fabricated process,
-- controller, capability, or control-prerequisite facts.
module Eclips.Herald.Placement
  ( PlacementSequence,
    mkPlacementSequence,
    firstPlacementSequence,
    nextPlacementSequence,
    placementSequenceWord64,
    DeltaRoute,
    applicationDeltaRoute,
    privateSystemViewDeltaRoute,
    DeltaRouteView (..),
    viewDeltaRoute,
    deltaRouteDelta,
    deltaRouteSortId,
    deltaRouteOccurrenceId,
    deltaRouteStoreIncarnation,
    PlacementRouteProblem (..),
    validatePrivateSystemViewDeltaRoute,
    PlacementSnapshot,
    placementSnapshot,
    placementSnapshotOwner,
    placementSnapshotSequence,
    placementSnapshotRoutes,
    PlacementUpdate (..),
    PlacementAcknowledgement,
    placementAcknowledgement,
    placementAcknowledgementOwner,
    placementAcknowledgementSequence,
    PlacementShapeProblem (..),
  )
where

import Data.List (sortOn)
import Data.Word (Word64)
import Eclips.Domain.Alignment
  ( PlacementRevision,
    PlacementRevisionError (PlacementRevisionMustBePositive),
    firstPlacementRevision,
    mkPlacementRevision,
    nextPlacementRevision,
    placementRevisionWord64,
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
    SystemId,
  )
import Eclips.Domain.Sort.Profile (profileSortFor)
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
  )
import Eclips.Domain.Startup
  ( PredefinedSortRole,
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
  )

-- | The exact positive physical-placement revision owned by one hosting
-- Herald epoch.  The public placement vocabulary retains its historical name,
-- while alignment cuts consume the same nominal Domain value directly.
type PlacementSequence = PlacementRevision

data PlacementShapeProblem
  = PlacementSequenceMustBePositive
  | PlacementSnapshotDuplicateDelta DeltaId
  deriving stock (Eq, Show)

mkPlacementSequence :: Word64 -> Either PlacementShapeProblem PlacementSequence
mkPlacementSequence value = case mkPlacementRevision value of
  Left PlacementRevisionMustBePositive -> Left PlacementSequenceMustBePositive
  Right revision -> Right revision

firstPlacementSequence :: PlacementSequence
firstPlacementSequence = firstPlacementRevision

-- | Advance one finite-run sequence. Exhaustion is outside profile 0.1.
nextPlacementSequence :: PlacementSequence -> PlacementSequence
nextPlacementSequence = nextPlacementRevision

placementSequenceWord64 :: PlacementSequence -> Word64
placementSequenceWord64 = placementRevisionWord64

data DeltaRoute
  = ApplicationDeltaRoute
      DeltaId
      SortId
      SortDefinitionOccurrenceId
      GlobalObjectId
      ProcessEpochId
      StoreIncarnationId
      ControlIndex
  | PrivateSystemViewDeltaRoute
      PredefinedSortRole
      DeltaId
      SortId
      SortDefinitionOccurrenceId
      StoreIncarnationId
  deriving stock (Eq, Ord, Show)

applicationDeltaRoute ::
  DeltaId ->
  SortId ->
  SortDefinitionOccurrenceId ->
  GlobalObjectId ->
  ProcessEpochId ->
  StoreIncarnationId ->
  ControlIndex ->
  DeltaRoute
applicationDeltaRoute = ApplicationDeltaRoute

privateSystemViewDeltaRoute ::
  PredefinedSortRole ->
  DeltaId ->
  SortId ->
  SortDefinitionOccurrenceId ->
  StoreIncarnationId ->
  DeltaRoute
privateSystemViewDeltaRoute = PrivateSystemViewDeltaRoute

data DeltaRouteView
  = ApplicationDeltaRouteView
      DeltaId
      SortId
      SortDefinitionOccurrenceId
      GlobalObjectId
      ProcessEpochId
      StoreIncarnationId
      ControlIndex
  | PrivateSystemViewDeltaRouteView
      PredefinedSortRole
      DeltaId
      SortId
      SortDefinitionOccurrenceId
      StoreIncarnationId
  deriving stock (Eq, Show)

viewDeltaRoute :: DeltaRoute -> DeltaRouteView
viewDeltaRoute route = case route of
  ApplicationDeltaRoute delta sortId occurrence controller process incarnation prerequisite ->
    ApplicationDeltaRouteView
      delta
      sortId
      occurrence
      controller
      process
      incarnation
      prerequisite
  PrivateSystemViewDeltaRoute role delta sortId occurrence incarnation ->
    PrivateSystemViewDeltaRouteView
      role
      delta
      sortId
      occurrence
      incarnation

deltaRouteDelta :: DeltaRoute -> DeltaId
deltaRouteDelta route = case route of
  ApplicationDeltaRoute delta _ _ _ _ _ _ -> delta
  PrivateSystemViewDeltaRoute _ delta _ _ _ -> delta

deltaRouteSortId :: DeltaRoute -> SortId
deltaRouteSortId route = case route of
  ApplicationDeltaRoute _ sortId _ _ _ _ _ -> sortId
  PrivateSystemViewDeltaRoute _ _ sortId _ _ -> sortId

deltaRouteOccurrenceId :: DeltaRoute -> SortDefinitionOccurrenceId
deltaRouteOccurrenceId route = case route of
  ApplicationDeltaRoute _ _ occurrence _ _ _ _ -> occurrence
  PrivateSystemViewDeltaRoute _ _ _ occurrence _ -> occurrence

deltaRouteStoreIncarnation :: DeltaRoute -> StoreIncarnationId
deltaRouteStoreIncarnation route = case route of
  ApplicationDeltaRoute _ _ _ _ _ incarnation _ -> incarnation
  PrivateSystemViewDeltaRoute _ _ _ _ incarnation -> incarnation

data PlacementRouteProblem
  = ExpectedPrivateSystemViewRoute
  | PrivateSystemViewDeltaMismatch
  | PrivateSystemViewSortMismatch
  | PrivateSystemViewOccurrenceMismatch
  | PrivateSystemViewIncarnationMismatch
  deriving stock (Eq, Ord, Show)

-- | Check every derivable private-view field against its owning Herald epoch.
validatePrivateSystemViewDeltaRoute ::
  SystemId ->
  HeraldEpoch ->
  DeltaRoute ->
  Either PlacementRouteProblem ()
validatePrivateSystemViewDeltaRoute system owner route = case route of
  ApplicationDeltaRoute {} -> Left ExpectedPrivateSystemViewRoute
  PrivateSystemViewDeltaRoute role delta sortId occurrence incarnation
    | delta /= deriveSystemViewDeltaId system owner role ->
        Left PrivateSystemViewDeltaMismatch
    | sortId /= profileSortFor role -> Left PrivateSystemViewSortMismatch
    | occurrence /= deriveSortDefinitionOccurrenceId system sortId Genesis ->
        Left PrivateSystemViewOccurrenceMismatch
    | incarnation /= deriveSystemViewStoreIncarnationId system owner role ->
        Left PrivateSystemViewIncarnationMismatch
    | otherwise -> Right ()

data PlacementSnapshot = PlacementSnapshot
  { owner :: HeraldEpoch,
    sequenceNumber :: PlacementSequence,
    routes :: [DeltaRoute]
  }
  deriving stock (Eq, Show)

placementSnapshot ::
  HeraldEpoch ->
  PlacementSequence ->
  [DeltaRoute] ->
  Either PlacementShapeProblem PlacementSnapshot
placementSnapshot owner sequenceNumber supplied = do
  routes <- normalizeRoutes supplied
  Right PlacementSnapshot {owner, sequenceNumber, routes}

placementSnapshotOwner :: PlacementSnapshot -> HeraldEpoch
placementSnapshotOwner snapshot = snapshot.owner

placementSnapshotSequence :: PlacementSnapshot -> PlacementSequence
placementSnapshotSequence snapshot = snapshot.sequenceNumber

placementSnapshotRoutes :: PlacementSnapshot -> [DeltaRoute]
placementSnapshotRoutes snapshot = snapshot.routes

data PlacementUpdate
  = FullPlacementSnapshot PlacementSnapshot
  deriving stock (Eq, Show)

data PlacementAcknowledgement = PlacementAcknowledgement
  { owner :: HeraldEpoch,
    sequenceNumber :: PlacementSequence
  }
  deriving stock (Eq, Ord, Show)

placementAcknowledgement ::
  HeraldEpoch ->
  PlacementSequence ->
  PlacementAcknowledgement
placementAcknowledgement = PlacementAcknowledgement

placementAcknowledgementOwner :: PlacementAcknowledgement -> HeraldEpoch
placementAcknowledgementOwner acknowledgement = acknowledgement.owner

placementAcknowledgementSequence :: PlacementAcknowledgement -> PlacementSequence
placementAcknowledgementSequence acknowledgement = acknowledgement.sequenceNumber

normalizeRoutes ::
  [DeltaRoute] ->
  Either PlacementShapeProblem [DeltaRoute]
normalizeRoutes supplied =
  case firstDuplicate (fmap deltaRouteDelta routes) of
    Nothing -> Right routes
    Just duplicate -> Left (PlacementSnapshotDuplicateDelta duplicate)
  where
    routes = sortOn deltaRouteDelta supplied

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate (first : second : remaining)
  | first == second = Just first
  | otherwise = firstDuplicate (second : remaining)
firstDuplicate _ = Nothing
