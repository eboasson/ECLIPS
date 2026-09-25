{-# LANGUAGE OverloadedRecordDot #-}

-- | Atomic composition of the live failure-detection owner with peer control,
-- timers, the contiguous Oracle projection, and stable Oracle requests.
module Eclips.Herald.UseCase.FailureDetection
  ( observePeerRecoveryExpired,
    observeFailureProbeTimer,
    observeFailureProbeBindingAccepted,
    applyDirectFailureProbeRequest,
    applyDirectFailureProbeResponse,
    applyFailureProjectionEvent,
    settleFailureOracleRequestEntry,
    cutFailureDetectionForDrain,
    observeVoterConfiguration,
  )
where

import Control.Monad (foldM)
import Data.Set qualified as Set
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Domain.Membership
  ( HeraldFailureProbeId,
    failureProbeResolutionProbeId,
    heraldMembershipGenerationActiveHeraldEpochs,
  )
import Eclips.Herald.Discovery (PeerBinding)
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
    orderedEffectBatch,
  )
import Eclips.Herald.FailureDetection.State qualified as FailureDetection
import Eclips.Herald.Graph.Progress qualified as Progress
import Eclips.Herald.Input (PeerControl (..))
import Eclips.Herald.OracleClient
  ( OracleClientAction (SubmitOracleRequest),
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Peer.Step15
  ( DirectFailureProbeRequest,
    DirectFailureProbeResponse,
    directFailureProbeRequestGeneration,
    directFailureProbeRequestProbe,
    directFailureProbeRequestTarget,
    directFailureProbeRequestVoterConfiguration,
  )
import Eclips.Herald.PeerLiveness.Internal (PeerRecoveryGeneration)
import Eclips.Herald.PeerLiveness.State qualified as PeerLiveness
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (HeraldTransitionInvariant),
    HeraldTransitionInvariantViolation (HeraldOracleTransitionContradiction),
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupFailureDetectionState,
    replaceStartupOracleClientState,
    replaceStartupPeerLivenessState,
    startupDiscoveryState,
    startupFailureDetectionState,
    startupLastObservedTime,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPeerLivenessState,
    startupStructuralProgressState,
  )
import Eclips.Herald.Timer (TimerAttempt, TimerOutcome)
import Eclips.Oracle.Canonical
  ( CanonicalAppliedOracleEntry,
    canonicalAppliedOracleEntryValue,
  )
import Eclips.Oracle.Failure qualified as Failure
import Eclips.Oracle.Genesis (raftVoterBindingHeraldEpoch)
import Eclips.Oracle.Projection
  ( OracleProjectionEventView (..),
    appliedEntryCommand,
    appliedEntryRequestId,
  )
import Eclips.Oracle.Voter qualified as Voter

observeVoterConfiguration :: Set.Set HeraldEpoch -> Maybe (Set.Set HeraldEpoch) -> Bool -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
observeVoterConfiguration oldHosts newHosts pending state = do
  let configuration = Voter.voterConfigurationId <$> OracleProjection.oracleViewVoterConfiguration (OracleProjection.oracleView (startupOracleProjectionState state))
      previous = FailureDetection.stateWitness (startupFailureDetectionState state)
  (successor, actions) <- either (const oracleFault) Right (FailureDetection.observeVoterConfiguration oldHosts newHosts configuration pending (startupFailureDetectionState state))
  (updated, effects) <- applyOwnerTransition successor actions state
  -- A newly prepared explicit change must also reconsider a peer whose
  -- recovery already expired; no new timer will arrive for that suspicion.
  if previous.witnessVoterChangePending /= pending
    then foldM reconsider (updated, effects) [recovery | recovery <- (PeerLiveness.stateWitness (startupPeerLivenessState updated)).recoveries, recovery.status == PeerLiveness.PeerSuspectedWitness]
    else Right (updated, effects)
  where
    reconsider (current, effects) recovery = do
      (next, emitted) <- observePeerRecoveryExpired recovery.target recovery.generation current
      Right (next, effects <> emitted)

observePeerRecoveryExpired ::
  HeraldEpoch ->
  PeerRecoveryGeneration ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
observePeerRecoveryExpired target recovery state =
  case OracleProjection.oracleViewPendingVoterChange projection of
    Just change
      | Voter.voterChangePhase change == Voter.VoterChangePreparing,
        Voter.ExplicitVoterReason _ <- Voter.voterChangeReason change,
        target `elem` map raftVoterBindingHeraldEpoch (Voter.oracleVoterBindingList (Voter.voterChangeOldBindings change)) ->
          retainOracleRequest (OracleClient.prepareFailureVoterCancellationRequest (Voter.voterChangeId change)) [] state []
    _ -> applyOwnerTransition successor actions state
  where
    projection = OracleProjection.oracleView (startupOracleProjectionState state)
    membership = OracleProjection.oracleViewCurrentHeraldMembership projection
    (successor, actions) =
      FailureDetection.observeRecoveryExpired
        target
        membership
        recovery
        (startupFailureDetectionState state)

observeFailureProbeTimer ::
  TimerAttempt ->
  TimerOutcome ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
observeFailureProbeTimer attempt outcome state =
  applyOwnerTransition successor actions state
  where
    (successor, actions) =
      FailureDetection.observeProbeTimer
        attempt
        outcome
        (startupFailureDetectionState state)

observeFailureProbeBindingAccepted ::
  PeerBinding ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
observeFailureProbeBindingAccepted binding state =
  applyOwnerTransition successor actions state
  where
    (successor, actions) =
      FailureDetection.observeBindingAccepted
        binding
        (startupFailureDetectionState state)

applyDirectFailureProbeRequest ::
  PeerBinding ->
  DirectFailureProbeRequest ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyDirectFailureProbeRequest binding request state =
  applyOwnerTransition successor actions state
  where
    projectionView =
      OracleProjection.oracleView (startupOracleProjectionState state)
    membership = OracleProjection.oracleViewCurrentHeraldMembership projectionView
    retained = OracleProjection.oracleViewFailureProbe (directFailureProbeRequestProbe request) projectionView
    reporters = maybe Set.empty (configurationReporters . OracleProjection.projectedFailureProbeConfiguration) retained
    projectedOpen =
      case retained of
        Just projected ->
          OracleProjection.projectedFailureProbeTarget projected
            == directFailureProbeRequestTarget request
            && OracleProjection.projectedFailureProbeGeneration projected
              == directFailureProbeRequestGeneration request
            && Voter.voterConfigurationId (OracleProjection.projectedFailureProbeConfiguration projected) == directFailureProbeRequestVoterConfiguration request
            && OracleProjection.projectedFailureProbeTerminal projected == Nothing
        Nothing -> False
    (successor, actions) =
      FailureDetection.observeDirectProbeRequest
        binding
        membership
        projectedOpen
        reporters
        request
        (startupFailureDetectionState state)

applyDirectFailureProbeResponse ::
  PeerBinding ->
  DirectFailureProbeResponse ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyDirectFailureProbeResponse binding response state =
  applyOwnerTransition successor actions state
  where
    (successor, actions) =
      FailureDetection.observeDirectProbeResponse
        binding
        response
        (startupFailureDetectionState state)

applyFailureProjectionEvent ::
  OracleProjectionEventView ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyFailureProjectionEvent view state = case view of
  OracleReplicaRegisteredView {} -> Right (state, [])
  -- A joining observer projects the historical native exclusion but has no
  -- authority to retain a follow-up command for later activation to dispatch.
  OracleVoterChangeChangedView change
    | Voter.voterChangePhase change == Voter.VoterExcludedAwaitingHeraldRetirement,
      not (Progress.structuralProgressIsJoiningObserver (startupStructuralProgressState state)),
      Voter.AcceptedHostFailureReason resolution <- Voter.voterChangeReason change -> do
        projected <- requireProjectedProbe (failureProbeResolutionProbeId resolution) state
        case OracleProjection.projectedFailureProbeTerminal projected of
          Just (OracleProjection.ProjectedVoterHostFailureAccepted certificate)
            | Failure.acceptedVoterHostFailureChangeId certificate == Voter.voterChangeId change -> do
                (successor, effects) <- retainOracleRequest (OracleClient.prepareRetireHeraldEpochRequest resolution (Failure.acceptedVoterHostFailureTarget certificate)) [] state []
                Right (successor, effectMembers effects)
          _ -> oracleFault
    | otherwise -> Right (state, [])
  OracleVoterConfigurationChangedView {} -> Right (state, [])
  HeraldAdmissionChangedView {} -> Right (state, [])
  OracleFailureProbeOpenedView identifier target generation configuration -> do
    let membership =
          OracleProjection.oracleViewCurrentHeraldMembership
            (OracleProjection.oracleView (startupOracleProjectionState state))
        binding = Discovery.currentPeerBinding target (startupDiscoveryState state)
        failureWitness =
          FailureDetection.stateWitness (startupFailureDetectionState state)
        reporters = configurationReporters configuration
        localIsVoter = Set.member failureWitness.witnessLocal reporters
        (liveness, recoveryCancellation) =
          if localIsVoter
            then PeerLiveness.clearRecovery target (startupPeerLivenessState state)
            else (startupPeerLivenessState state, Nothing)
        withFreshReachability = replaceStartupPeerLivenessState liveness state
        (failureDetection, actions) =
          FailureDetection.observeProbeOpened
            (startupLastObservedTime state)
            membership
            binding
            identifier
            target
            generation
            (Voter.voterConfigurationId configuration)
            reporters
            (startupFailureDetectionState state)
    (successor, effects) <-
      applyOwnerTransition
        failureDetection
        actions
        withFreshReachability
    Right
      ( successor,
        maybe [] (pure . CancelTimer) recoveryCancellation
          <> effectMembers effects
      )
  OracleFailureProbeReportRecordedView identifier _ _ -> do
    projected <- requireProjectedProbe identifier state
    let (failureDetection, actions) =
          FailureDetection.observeProjectedReports
            identifier
            (OracleProjection.projectedFailureProbeTarget projected)
            (OracleProjection.projectedFailureProbeReports projected)
            (startupFailureDetectionState state)
    (successor, effects) <-
      applyOwnerTransition failureDetection actions state
    Right (successor, effectMembers effects)
  OracleVoterHostFailureAcceptedView certificate -> terminal False (failureProbeResolutionProbeId (Failure.acceptedVoterHostFailureResolution certificate)) state
  OracleFailureProbeDismissedView identifier _ -> terminal True identifier state
  OracleFailureProbeRetiredView identifier _ _ -> terminal False identifier state
  OracleFailureProbeSupersededView identifier _ -> terminal True identifier state
  HeraldMembershipAdvancedView {} -> Right (state, [])
  ProcessStartedView {} -> Right (state, [])
  ProcessEpochEndedEventView {} -> Right (state, [])
  LabelDecidedView {} -> Right (state, [])
  LabelWorkflowCompletedView {} -> Right (state, [])
  DisappearanceProbeOpenedView {} -> Right (state, [])
  PredefinedAbsenceReportedView {} -> Right (state, [])
  DisappearanceProbeInvalidatedView {} -> Right (state, [])
  DisappearanceProbeResolvedView {} -> Right (state, [])
  DisappearanceProbeAbortedView {} -> Right (state, [])
  where
    terminal beginCooldown identifier predecessor = do
      let retainedProbe =
            FailureDetection.failureProbeWitness
              identifier
              (startupFailureDetectionState predecessor)
      let (failureDetection, actions) =
            FailureDetection.observeProbeTerminal
              identifier
              (startupFailureDetectionState predecessor)
      (successor, effects) <-
        applyOwnerTransition failureDetection actions predecessor
      applyCooldown beginCooldown retainedProbe successor (effectMembers effects)

applyCooldown ::
  Bool ->
  Maybe FailureDetection.FailureProbeWitness ->
  HeraldState ->
  [HeraldEffect] ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyCooldown beginCooldown retainedProbe state effects =
  case retainedProbe of
    Just probe
      | beginCooldown,
        probeIsNonTerminal probe,
        targetIsActive probe.witnessTarget state,
        Discovery.currentPeerBinding
          probe.witnessTarget
          (startupDiscoveryState state)
          == Nothing ->
          let (peerLiveness, disposition) =
                PeerLiveness.restartRecovery
                  (startupLastObservedTime state)
                  probe.witnessTarget
                  (startupPeerLivenessState state)
              cooldownEffects = case disposition of
                PeerLiveness.RecoveryRestarted cancellation attempt specification ->
                  maybe [] (pure . CancelTimer) cancellation
                    <> [ArmTimer attempt specification]
           in Right
                ( replaceStartupPeerLivenessState peerLiveness state,
                  effects <> cooldownEffects
                )
    _ -> Right (state, effects)

probeIsNonTerminal :: FailureDetection.FailureProbeWitness -> Bool
probeIsNonTerminal probe = case probe.witnessPhase of
  FailureDetection.FailureProbeTerminalWitness -> False
  _ -> True

targetIsActive :: HeraldEpoch -> HeraldState -> Bool
targetIsActive target state =
  target
    `elem` heraldMembershipGenerationActiveHeraldEpochs
      ( OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState state))
      )

requireProjectedProbe ::
  HeraldFailureProbeId ->
  HeraldState ->
  Either HeraldInvariantFault OracleProjection.ProjectedFailureProbe
requireProjectedProbe identifier state =
  maybe
    oracleFault
    Right
    ( OracleProjection.oracleViewFailureProbe
        identifier
        (OracleProjection.oracleView (startupOracleProjectionState state))
    )

settleFailureOracleRequestEntry ::
  CanonicalAppliedOracleEntry ->
  HeraldState ->
  Either HeraldInvariantFault HeraldState
settleFailureOracleRequestEntry canonical state =
  case matching of
    [] -> Right state
    [(reference, _)] -> do
      prepared <-
        mapClientProblem
          ( OracleClient.prepareOracleResult
              reference
              canonical
              (startupOracleClientState state)
          )
      let (client, _, _) = OracleClient.commitOracleResult prepared
      Right (replaceStartupOracleClientState client state)
    _ -> oracleFault
  where
    request =
      appliedEntryRequestId <$> appliedEntryCommand (canonicalAppliedOracleEntryValue canonical)
    matching =
      [ (reference, witness)
      | (reference, witness) <-
          OracleClient.oracleClientRequestEntries (startupOracleClientState state),
        Just (OracleClient.oracleRequestRefRequestId reference) == request,
        isFailureIntent (OracleClient.oracleRequestWitnessIntent witness)
      ]

isFailureIntent :: OracleClient.OracleSemanticIntent -> Bool
isFailureIntent intent = case intent of
  OracleClient.HeraldAdmissionIntent {} -> False
  OracleClient.VoterAdministrationIntent {} -> False
  OracleClient.FailureVoterCancellationIntent {} -> True
  OracleClient.OpenHeraldFailureProbeIntent {} -> True
  OracleClient.ReportHeraldFailureProbeIntent {} -> True
  OracleClient.DismissHeraldFailureProbeIntent {} -> True
  OracleClient.AcceptVoterHostFailureIntent {} -> True
  OracleClient.RetireHeraldEpochIntent {} -> True
  OracleClient.DynamicStartIntent {} -> False
  OracleClient.ProcessEndIntent {} -> False
  OracleClient.DecideLabelIntent {} -> False
  OracleClient.CompleteLabelDecisionIntent {} -> False
  OracleClient.OpenDisappearanceProbeIntent {} -> False
  OracleClient.ReportPredefinedAbsenceIntent {} -> False
  OracleClient.InvalidateDisappearanceProbeIntent {} -> False
  OracleClient.ResolveDisappearanceProbeIntent {} -> False
  OracleClient.AbortDisappearanceProbeIntent {} -> False

cutFailureDetectionForDrain :: HeraldState -> (HeraldState, [TimerAttempt])
cutFailureDetectionForDrain state =
  (replaceStartupFailureDetectionState successor state, timers)
  where
    (successor, timers) =
      FailureDetection.cutForDrain (startupFailureDetectionState state)

applyOwnerTransition ::
  FailureDetection.State ->
  [FailureDetection.FailureDetectionAction] ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyOwnerTransition failureDetection actions state =
  foldActions
    actions
    (replaceStartupFailureDetectionState failureDetection state)
    []

foldActions ::
  [FailureDetection.FailureDetectionAction] ->
  HeraldState ->
  [HeraldEffect] ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
foldActions remaining state reverseEffects = case remaining of
  [] -> Right (state, orderedEffectBatch (reverse reverseEffects))
  action : rest -> case action of
    FailureDetection.ArmDirectFailureProbeTimer attempt specification ->
      foldActions rest state (ArmTimer attempt specification : reverseEffects)
    FailureDetection.CancelDirectFailureProbeTimer attempt ->
      foldActions rest state (CancelTimer attempt : reverseEffects)
    FailureDetection.SendDirectFailureProbe binding request ->
      foldActions
        rest
        state
        (SendPeerControl binding (PeerDirectFailureProbeRequested request) : reverseEffects)
    FailureDetection.SendDirectFailureProbeResponse binding response ->
      foldActions
        rest
        state
        (SendPeerControl binding (PeerDirectFailureProbeResponded response) : reverseEffects)
    FailureDetection.RetainOpenFailureProbe target generation recovery configuration ->
      retainOracleRequest
        ( OracleClient.prepareOpenHeraldFailureProbeRequest
            target
            generation
            recovery
            configuration
        )
        rest
        state
        reverseEffects
    FailureDetection.RetainFailureProbeReport identifier configuration result ->
      retainOracleRequest
        (OracleClient.prepareReportHeraldFailureProbeRequest identifier configuration result)
        rest
        state
        reverseEffects
    FailureDetection.RetainFailureProbeDismissal resolution ->
      retainOracleRequest
        (OracleClient.prepareDismissHeraldFailureProbeRequest resolution)
        rest
        state
        reverseEffects
    FailureDetection.RetainVoterHostFailureAcceptance resolution ->
      retainOracleRequest
        (OracleClient.prepareAcceptVoterHostFailureRequest resolution)
        rest
        state
        reverseEffects
    FailureDetection.RetainHeraldRetirement resolution target ->
      retainOracleRequest
        (OracleClient.prepareRetireHeraldEpochRequest resolution target)
        rest
        state
        reverseEffects

retainOracleRequest ::
  ( OracleClient.State ->
    Either OracleClient.OracleClientProblem OracleClient.PreparedOracleRequest
  ) ->
  [FailureDetection.FailureDetectionAction] ->
  HeraldState ->
  [HeraldEffect] ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
retainOracleRequest prepare remaining state reverseEffects = do
  prepared <- mapClientProblem (prepare (startupOracleClientState state))
  let (client, classification, reference) =
        OracleClient.commitOracleRequest prepared
      successor = replaceStartupOracleClientState client state
      newActions = case classification of
        OracleClient.OracleRequestExactRetry -> []
        OracleClient.OracleRequestFirstPrepared ->
          [ RunOracleClientAction action
          | action@(SubmitOracleRequest _ dispatch) <-
              OracleClient.oracleClientRequestActions client,
            OracleClient.oracleRequestDispatchRef dispatch == reference
          ]
  foldActions
    remaining
    successor
    (reverse newActions <> reverseEffects)

effectMembers :: EffectBatch -> [HeraldEffect]
effectMembers = effectBatchMembers

mapClientProblem ::
  Either OracleClient.OracleClientProblem value ->
  Either HeraldInvariantFault value
mapClientProblem = either (const oracleFault) Right

oracleFault :: Either HeraldInvariantFault value
oracleFault =
  Left (HeraldTransitionInvariant HeraldOracleTransitionContradiction)

configurationReporters :: Voter.VoterConfiguration -> Set.Set HeraldEpoch
configurationReporters = Set.fromList . map raftVoterBindingHeraldEpoch . Voter.voterConfigurationBindings
