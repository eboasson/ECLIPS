{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Detached composition of Step-15 projection and existing peer liveness.
module Eclips.Herald.UseCase.Step15FailureVertical
  ( State,
    initialState,
    CooldownToken,
    CooldownConfiguration,
    CooldownConfigurationProblem (..),
    checkCooldownConfiguration,
    VerticalEffect (..),
    DirectProbeAttempt,
    directProbeAttemptProbe,
    directProbeAttemptTarget,
    directProbeAttemptMembershipGeneration,
    CheckedReferenceMembershipAdvance,
    ReferenceMembershipAdvanceProblem (..),
    checkReferenceMembershipAdvance,
    prepareReferenceHeraldMembershipAdvance,
    applyReferenceWorkflowMembershipAdvance,
    observeDisconnected,
    observeConnected,
    observeRecoveryTimer,
    projectOracleEntry,
    VerticalProjectionProblem (..),
    observeCooldown,
    freshReportCommand,
    FreshReportDisposition (..),
    projectionState,
    peerLivenessState,
  )
where

import Data.List (sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity (ControlIndex, HeraldEpoch)
import Eclips.Domain.Membership
  ( HeraldFailureProbeId,
    HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationId,
    heraldMembershipGenerationRetirementControlIndex,
  )
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.OracleProjection.Step15 qualified as Projection
import Eclips.Herald.PeerLiveness.Internal (PeerRecoveryConfiguration)
import Eclips.Herald.PeerLiveness.State qualified as PeerLiveness
import Eclips.Herald.Startup.State (HeraldState)
import Eclips.Herald.Time (MonotonicInstant, monotonicInstant, monotonicInstantWord64)
import Eclips.Herald.Timer.Internal
  ( TimerAttempt,
    TimerOutcome,
    TimerSpec,
    absoluteTimerSpec,
    timerAttemptPeerRecovery,
  )
import Eclips.Herald.UseCase.Step15MembershipAdvance qualified as MembershipAdvance
import Eclips.Oracle.Step15.Reference
  ( ReferenceAppliedOracleEntry,
    ReferenceOracleCommand,
    ReferenceProbeResult,
    ReferenceProjectionEventView (..),
    referenceAppliedEntryControlIndex,
    referenceAppliedEntryProjectionEvents,
    referenceOpenHeraldFailureProbeCommand,
    referenceProjectionEventView,
    referenceReportHeraldFailureProbeCommand,
  )
import Eclips.Oracle.Step15.WorkflowReference qualified as Workflow

data LinkStatus
  = LinkConnected
  | LinkDisconnected
  | LinkCooling CooldownToken MonotonicInstant
  deriving stock (Eq, Show)

data CooldownToken = CooldownToken HeraldEpoch Word64
  deriving stock (Eq, Ord, Show)

data State = State
  { local :: HeraldEpoch,
    voterHosts :: Set HeraldEpoch,
    projection :: Projection.State,
    liveness :: PeerLiveness.State,
    links :: Map HeraldEpoch LinkStatus,
    nextCooldown :: Map HeraldEpoch Word64,
    cooldownConfiguration :: CooldownConfiguration,
    localReports :: Map HeraldFailureProbeId ReferenceProbeResult,
    directProbeAttempts :: Map HeraldFailureProbeId DirectProbeAttempt
  }
  deriving stock (Eq, Show)

newtype CooldownConfiguration = CooldownConfiguration Word64
  deriving stock (Eq, Show)

data CooldownConfigurationProblem = CooldownDurationZero
  deriving stock (Eq, Show)

checkCooldownConfiguration :: Word64 -> Either CooldownConfigurationProblem CooldownConfiguration
checkCooldownConfiguration 0 = Left CooldownDurationZero
checkCooldownConfiguration duration = Right (CooldownConfiguration duration)

initialState ::
  HeraldEpoch ->
  Set HeraldEpoch ->
  HeraldMembershipGeneration ->
  PeerRecoveryConfiguration ->
  CooldownConfiguration ->
  State
initialState local voterHosts membership recoveryConfiguration cooldownConfiguration =
  State
    { local,
      voterHosts,
      projection = Projection.initialState membership,
      liveness = PeerLiveness.initialState recoveryConfiguration,
      links = Map.empty,
      nextCooldown = Map.empty,
      cooldownConfiguration,
      localReports = Map.empty,
      directProbeAttempts = Map.empty
    }

data DirectProbeAttempt = DirectProbeAttempt HeraldFailureProbeId HeraldEpoch HeraldMembershipGenerationId
  deriving stock (Eq, Show)

directProbeAttemptProbe :: DirectProbeAttempt -> HeraldFailureProbeId
directProbeAttemptProbe (DirectProbeAttempt probe _ _) = probe

directProbeAttemptTarget :: DirectProbeAttempt -> HeraldEpoch
directProbeAttemptTarget (DirectProbeAttempt _ target _) = target

directProbeAttemptMembershipGeneration ::
  DirectProbeAttempt -> HeraldMembershipGenerationId
directProbeAttemptMembershipGeneration (DirectProbeAttempt _ _ generation) = generation

data CheckedReferenceMembershipAdvance
  = CheckedReferenceMembershipAdvance
      ControlIndex
      HeraldMembershipGeneration
      [OracleProjection.MembershipAdvanceProcessEnd]
  deriving stock (Eq, Show)

data ReferenceMembershipAdvanceProblem
  = ReferenceMembershipAdvanceAbsent
  | ReferenceMembershipAdvanceMultiple
  | ReferenceMembershipAdvanceSuccessorIndexAbsent
  | ReferenceMembershipAdvanceIndexMismatch ControlIndex ControlIndex
  | ReferenceMembershipAdvanceHeraldProblem MembershipAdvance.MembershipAdvanceProblem
  | ReferenceMembershipAdvanceWorkflowProblem Workflow.ReferenceWorkflowProblem
  deriving stock (Eq, Show)

checkReferenceMembershipAdvance ::
  ReferenceAppliedOracleEntry ->
  Either ReferenceMembershipAdvanceProblem CheckedReferenceMembershipAdvance
checkReferenceMembershipAdvance entry = case successors of
  [] -> Left ReferenceMembershipAdvanceAbsent
  [_first, _second] -> Left ReferenceMembershipAdvanceMultiple
  _ : _ : _ -> Left ReferenceMembershipAdvanceMultiple
  [successor]
    | Nothing <- heraldMembershipGenerationRetirementControlIndex successor ->
        Left ReferenceMembershipAdvanceSuccessorIndexAbsent
    | Just successorIndex <- heraldMembershipGenerationRetirementControlIndex successor,
      successorIndex /= index ->
        Left (ReferenceMembershipAdvanceIndexMismatch index successorIndex)
    | Just mismatched <- firstMismatchedEnd ->
        Left (ReferenceMembershipAdvanceIndexMismatch index mismatched)
    | otherwise -> Right (CheckedReferenceMembershipAdvance index successor (sortOn OracleProjection.membershipAdvanceProcessEndEpoch ends))
  where
    index = referenceAppliedEntryControlIndex entry
    views = referenceProjectionEventView <$> referenceAppliedEntryProjectionEvents entry
    successors = [successor | ReferenceHeraldMembershipAdvancedView successor <- views]
    ends =
      [ OracleProjection.membershipAdvanceProcessEnd process endIndex reason
      | ReferenceProcessEpochEndedView process endIndex reason <- views
      ]
    firstMismatchedEnd = case [endIndex | ReferenceProcessEpochEndedView _ endIndex _ <- views, endIndex /= index] of
      mismatched : _ -> Just mismatched
      [] -> Nothing

prepareReferenceHeraldMembershipAdvance ::
  CheckedReferenceMembershipAdvance ->
  HeraldState ->
  Either ReferenceMembershipAdvanceProblem MembershipAdvance.PreparedMembershipAdvance
prepareReferenceHeraldMembershipAdvance (CheckedReferenceMembershipAdvance index successor ends) =
  either (Left . ReferenceMembershipAdvanceHeraldProblem) Right
    . MembershipAdvance.prepareMembershipAdvance index successor ends

applyReferenceWorkflowMembershipAdvance ::
  CheckedReferenceMembershipAdvance ->
  Workflow.ReferenceWorkflowState ->
  Either ReferenceMembershipAdvanceProblem Workflow.ReferenceWorkflowState
applyReferenceWorkflowMembershipAdvance (CheckedReferenceMembershipAdvance _ successor _) =
  either (Left . ReferenceMembershipAdvanceWorkflowProblem) Right
    . Workflow.applyReferenceWorkflowRetirement successor

data VerticalEffect
  = RecoveryTimerStarted TimerAttempt TimerSpec
  | RecoveryTimerCancelled TimerAttempt
  | OpenProbeRequested ReferenceOracleCommand
  | CooldownStarted CooldownToken TimerSpec
  | CooldownObservationStale
  | CooldownObservationEarly CooldownToken TimerSpec
  | BeginFreshDirectProbe DirectProbeAttempt
  deriving stock (Eq, Show)

observeDisconnected :: MonotonicInstant -> HeraldEpoch -> State -> (State, [VerticalEffect])
observeDisconnected observedAt target state
  | not (admissibleTarget target state) = (state, [])
  | Just LinkCooling {} <- Map.lookup target state.links = (state, [])
  | otherwise = startRecovery observedAt target state {links = Map.insert target LinkDisconnected state.links}

observeConnected :: HeraldEpoch -> State -> (State, [VerticalEffect])
observeConnected target state
  | not (admissibleTarget target state) = (state, [])
  | otherwise =
      ( successor,
        maybe [] (pure . RecoveryTimerCancelled) cancellation
      )
  where
    (liveness, cancellation) = PeerLiveness.clearRecovery target state.liveness
    successor = state {liveness, links = Map.insert target LinkConnected state.links}

admissibleTarget :: HeraldEpoch -> State -> Bool
admissibleTarget target state =
  target /= state.local
    && target `elem` NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (Projection.projectedMembership state.projection))

observeRecoveryTimer ::
  TimerAttempt -> TimerOutcome -> State -> (State, [VerticalEffect])
observeRecoveryTimer attempt outcome state =
  case disposition of
    PeerLiveness.TimerObservationStale -> (successor, [])
    PeerLiveness.TimerObservationRearmed next spec ->
      (successor, [RecoveryTimerStarted next spec])
    PeerLiveness.TimerObservationExpired -> case recoveryTarget attempt of
      Nothing -> (successor, [])
      Just target ->
        ( successor,
          [ OpenProbeRequested
              ( referenceOpenHeraldFailureProbeCommand
                  target
                  (heraldMembershipGenerationId (Projection.projectedMembership state.projection))
              )
          ]
        )
  where
    (liveness, disposition) = PeerLiveness.observeTimer attempt outcome state.liveness
    successor = state {liveness}

recoveryTarget :: TimerAttempt -> Maybe HeraldEpoch
recoveryTarget attempt = case timerAttemptPeerRecovery attempt of
  Just (target, _, _) -> Just target
  Nothing -> Nothing

projectOracleEntry ::
  MonotonicInstant -> ReferenceAppliedOracleEntry -> State -> Either VerticalProjectionProblem (State, [VerticalEffect])
projectOracleEntry observedAt entry state =
  case disposition of
    Projection.ProjectionApplied -> Right (foldl applyProjectedEvent (projected, []) eventViews)
    Projection.ProjectionDuplicate -> Right (state, [])
    Projection.ProjectionStale -> Right (state, [])
    Projection.ProjectionGap retained observed -> Left (VerticalProjectionGap retained observed)
    Projection.ProjectionConflict index -> Left (VerticalProjectionConflict index)
  where
    eventViews = referenceProjectionEventView <$> referenceAppliedEntryProjectionEvents entry
    (projection, disposition) = Projection.projectEntry entry state.projection
    projected = state {projection}

    applyProjectedEvent (current, effects) event = case event of
      ReferenceFailureProbeOpenedView probe target generation
        | Set.member current.local current.voterHosts ->
            let attempt = DirectProbeAttempt probe target generation
             in ( current {directProbeAttempts = Map.insert probe attempt current.directProbeAttempts},
                  effects <> [BeginFreshDirectProbe attempt]
                )
        | otherwise -> (current, effects)
      ReferenceFailureProbeDismissedEventView probe _ ->
        case Projection.projectedProbeTarget probe state.projection of
          Nothing -> (current, effects)
          Just target ->
            let (cleared, cancellation) = PeerLiveness.clearRecovery target current.liveness
                afterClear = current {liveness = cleared, localReports = Map.delete probe current.localReports, directProbeAttempts = Map.delete probe current.directProbeAttempts}
                cancellationEffects = maybe [] (pure . RecoveryTimerCancelled) cancellation
             in case Map.lookup target current.links of
                  Just LinkDisconnected ->
                    let (cooled, cooldownEffect) = startCooldown observedAt target afterClear
                     in (cooled, effects <> cancellationEffects <> [cooldownEffect])
                  _ -> (afterClear, effects <> cancellationEffects)
      ReferenceFailureProbeRetiredEventView probe _ _ -> stopRetired probe current effects
      ReferenceFailureProbeSupersededEventView probe _ -> restartSuperseded probe current effects
      _ -> (current, effects)

    stopRetired probe current effects =
      case Projection.projectedProbeTarget probe state.projection of
        Nothing -> (current, effects)
        Just target ->
          let (cleared, cancellation) = PeerLiveness.clearRecovery target current.liveness
           in ( current {liveness = cleared, links = Map.delete target current.links, localReports = Map.delete probe current.localReports, directProbeAttempts = Map.delete probe current.directProbeAttempts},
                effects <> maybe [] (pure . RecoveryTimerCancelled) cancellation
              )

    restartSuperseded probe current effects =
      case Projection.projectedProbeTarget probe state.projection of
        Nothing -> (current, effects)
        Just target ->
          let (cleared, cancellation) = PeerLiveness.clearRecovery target current.liveness
              reset = current {liveness = cleared, localReports = Map.delete probe current.localReports, directProbeAttempts = Map.delete probe current.directProbeAttempts}
              cancellationEffects = maybe [] (pure . RecoveryTimerCancelled) cancellation
           in case Map.lookup target current.links of
                Just LinkDisconnected ->
                  let (restarted, recoveryEffects) = startRecovery observedAt target reset
                   in (restarted, effects <> cancellationEffects <> recoveryEffects)
                _ -> (reset, effects <> cancellationEffects)

data VerticalProjectionProblem
  = VerticalProjectionGap ControlIndex ControlIndex
  | VerticalProjectionConflict ControlIndex
  deriving stock (Eq, Show)

startCooldown :: MonotonicInstant -> HeraldEpoch -> State -> (State, VerticalEffect)
startCooldown observedAt target state =
  ( state
      { links = Map.insert target (LinkCooling token deadline) state.links,
        nextCooldown = Map.insert target generation state.nextCooldown
      },
    CooldownStarted token (absoluteTimerSpec deadline)
  )
  where
    generation = maybe 1 (+ 1) (Map.lookup target state.nextCooldown)
    token = CooldownToken target generation
    CooldownConfiguration duration = state.cooldownConfiguration
    deadline = monotonicInstant (monotonicInstantWord64 observedAt + duration)

observeCooldown ::
  CooldownToken -> MonotonicInstant -> State -> (State, [VerticalEffect])
observeCooldown token@(CooldownToken target _) firedAt state =
  case Map.lookup target state.links of
    Just (LinkCooling current deadline)
      | current == token,
        firedAt < deadline ->
          (state, [CooldownObservationEarly token (absoluteTimerSpec deadline)])
      | current == token ->
          startRecovery firedAt target state {links = Map.insert target LinkDisconnected state.links}
    _ -> (state, [CooldownObservationStale])

startRecovery :: MonotonicInstant -> HeraldEpoch -> State -> (State, [VerticalEffect])
startRecovery observedAt target state = case PeerLiveness.beginRecovery observedAt target state.liveness of
  (liveness, PeerLiveness.RecoveryAlreadyActive) -> (state {liveness}, [])
  (liveness, PeerLiveness.RecoveryStarted attempt spec) ->
    (state {liveness}, [RecoveryTimerStarted attempt spec])

data FreshReportDisposition
  = FreshReportCommand ReferenceOracleCommand
  | FreshReportDuplicate
  | FreshReportConflict ReferenceProbeResult ReferenceProbeResult
  | FreshReportNotProjected
  deriving stock (Eq, Show)

freshReportCommand ::
  DirectProbeAttempt -> ReferenceProbeResult -> State -> (State, FreshReportDisposition)
freshReportCommand attempt result state
  | not (Projection.mayReportProbe probe state.projection) = (state, FreshReportNotProjected)
  | Set.notMember state.local state.voterHosts = (state, FreshReportNotProjected)
  | Projection.projectedProbeGeneration probe state.projection /= Just generation = (state, FreshReportNotProjected)
  | Map.lookup probe state.directProbeAttempts /= Just attempt = (state, FreshReportNotProjected)
  | Just retained <- Map.lookup probe state.localReports =
      if retained == result
        then (state, FreshReportDuplicate)
        else (state, FreshReportConflict retained result)
  | otherwise =
      ( state {localReports = Map.insert probe result state.localReports},
        FreshReportCommand (referenceReportHeraldFailureProbeCommand probe result)
      )
  where
    probe = directProbeAttemptProbe attempt
    generation = directProbeAttemptMembershipGeneration attempt

projectionState :: State -> Projection.State
projectionState state = state.projection

peerLivenessState :: State -> PeerLiveness.State
peerLivenessState state = state.liveness
