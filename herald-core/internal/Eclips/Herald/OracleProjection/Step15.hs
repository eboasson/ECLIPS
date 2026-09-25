{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Bounded per-Herald projection used only by the detached Step-15 model.
module Eclips.Herald.OracleProjection.Step15
  ( State,
    initialState,
    ProjectionDisposition (..),
    projectEntry,
    projectedControlIndex,
    projectedMembership,
    projectedMembershipHistory,
    projectedProbeTarget,
    projectedProbeGeneration,
    mayReportProbe,
    projectedProcessEnd,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Eclips.Domain.Identity (ControlIndex, HeraldEpoch, ProcessEpochId, controlIndex, controlIndexWord64)
import Eclips.Domain.Membership
  ( HeraldFailureProbeId,
    HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    HeraldMembershipHistory,
    appendHeraldMembershipGeneration,
    heraldMembershipGenerationId,
    heraldMembershipHistory,
    heraldMembershipHistoryCurrent,
    heraldMembershipHistoryGenerations,
  )
import Eclips.Domain.ProcessLifecycle (ProcessEndReason)
import Eclips.Oracle.Step15.Reference
  ( ReferenceAppliedOracleEntry,
    ReferenceProjectionEventView (..),
    referenceAppliedEntryControlIndex,
    referenceAppliedEntryProjectionEvents,
    referenceProjectionEventView,
  )

data ProjectedProbe = ProjectedProbe
  { target :: HeraldEpoch,
    generation :: HeraldMembershipGenerationId
  }
  deriving stock (Eq, Show)

data State = State
  { controlIndex :: ControlIndex,
    appliedEntries :: Map ControlIndex ReferenceAppliedOracleEntry,
    membership :: HeraldMembershipHistory,
    openProbes :: Map HeraldFailureProbeId ProjectedProbe,
    processEnds :: Map ProcessEpochId (ControlIndex, ProcessEndReason)
  }
  deriving stock (Eq, Show)

initialState :: HeraldMembershipGeneration -> State
initialState membership =
  State
    { controlIndex = controlIndex 0,
      appliedEntries = Map.empty,
      membership = either (error . show) id (heraldMembershipHistory (NonEmpty.singleton membership)),
      openProbes = Map.empty,
      processEnds = Map.empty
    }

data ProjectionDisposition
  = ProjectionApplied
  | ProjectionDuplicate
  | ProjectionStale
  | ProjectionConflict ControlIndex
  | ProjectionGap ControlIndex ControlIndex
  deriving stock (Eq, Show)

-- | Project one exact committed entry. Rejected entries still advance the
-- projection coordinate. The finite profile retains each exact applied entry
-- so a contradictory replay at any earlier index remains distinguishable from
-- an equal duplicate.
projectEntry :: ReferenceAppliedOracleEntry -> State -> (State, ProjectionDisposition)
projectEntry entry state
  | observed <= state.controlIndex = case Map.lookup observed state.appliedEntries of
      Just retained | retained == entry -> (state, ProjectionDuplicate)
      Just _ -> (state, ProjectionConflict observed)
      Nothing -> (state, ProjectionStale)
  | controlIndexWord64 observed /= controlIndexWord64 state.controlIndex + 1 =
      (state, ProjectionGap state.controlIndex observed)
  | otherwise =
      ( foldl
          applyEvent
          state
            { controlIndex = observed,
              appliedEntries = Map.insert observed entry state.appliedEntries
            }
          eventViews,
        ProjectionApplied
      )
  where
    observed = referenceAppliedEntryControlIndex entry
    eventViews = referenceProjectionEventView <$> referenceAppliedEntryProjectionEvents entry

applyEvent :: State -> ReferenceProjectionEventView -> State
applyEvent state event = case event of
  ReferenceFailureProbeOpenedView probe target generation ->
    state {openProbes = Map.insert probe (ProjectedProbe target generation) state.openProbes}
  ReferenceFailureProbeDismissedEventView probe _ -> removeProbe probe state
  ReferenceFailureProbeRetiredEventView probe _ _ -> removeProbe probe state
  ReferenceFailureProbeSupersededEventView probe _ -> removeProbe probe state
  ReferenceHeraldMembershipAdvancedView membership ->
    state {membership = either (error . show) id (appendHeraldMembershipGeneration membership state.membership)}
  ReferenceFailureProbeReportRecordedView {} -> state
  ReferenceProcessEpochEndedView process index reason ->
    state {processEnds = Map.insert process (index, reason) state.processEnds}

removeProbe :: HeraldFailureProbeId -> State -> State
removeProbe probe state = state {openProbes = Map.delete probe state.openProbes}

projectedControlIndex :: State -> ControlIndex
projectedControlIndex state = state.controlIndex

projectedMembership :: State -> HeraldMembershipGeneration
projectedMembership state = heraldMembershipHistoryCurrent state.membership

projectedMembershipHistory :: State -> [HeraldMembershipGeneration]
projectedMembershipHistory state = NonEmpty.toList (heraldMembershipHistoryGenerations state.membership)

projectedProbeTarget :: HeraldFailureProbeId -> State -> Maybe HeraldEpoch
projectedProbeTarget probe state = (.target) <$> Map.lookup probe state.openProbes

projectedProbeGeneration :: HeraldFailureProbeId -> State -> Maybe HeraldMembershipGenerationId
projectedProbeGeneration probe state = (.generation) <$> Map.lookup probe state.openProbes

-- | A report is fresh only after this particular Herald has projected Open.
mayReportProbe :: HeraldFailureProbeId -> State -> Bool
mayReportProbe probe state =
  projectedProbeGeneration probe state == Just (heraldMembershipGenerationId (projectedMembership state))

projectedProcessEnd :: ProcessEpochId -> State -> Maybe (ControlIndex, ProcessEndReason)
projectedProcessEnd process = Map.lookup process . (.processEnds)
