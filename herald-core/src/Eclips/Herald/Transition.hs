-- | Pure deterministic Herald transition for the implemented local kernel.
module Eclips.Herald.Transition
  ( HeraldState,
    HeraldPhase (..),
    heraldPhase,
    heraldOracleReplicaProjection,
    HeraldInvariantFault (..),
    HeraldTransitionInvariantViolation (..),
    stepHerald,
  )
where

import Control.Monad (foldM, unless)
import Data.List (partition, sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Application.Types.Lifecycle qualified as Lifecycle
import Eclips.Application.Types.Operation
  ( ApplicationOperation (ReadApplication),
  )
import Eclips.Domain.Identity (ControlIndex)
import Eclips.Domain.Membership (heraldMembershipGenerationId)
import Eclips.Herald.Administration
  ( AdminCorrelationId,
    AdminReply (AdminDrainAccepted, AdministrationReceiptRetirementNotReady, AdministrationReceiptsRetired, ConflictingAdminResult, RetiredAdminResult),
    AdministrationBinding,
    adminCorrelationBelongsTo,
  )
import Eclips.Herald.Administration.State qualified as Administration
import Eclips.Herald.Application.PrivateIdentity (witnessedProcessEpoch)
import Eclips.Herald.Application.Session
  ( ApplicationSessionId,
    ApplicationSessionRejection (ApplicationSessionNotLive),
    ApplicationSessionUnavailableReason (..),
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Discovery
  ( HelloRejection (HelloInactiveHerald),
    PeerBinding,
    PeerHelloDisposition (..),
    peerBindingRemoteHeraldEpoch,
    peerCandidateOpenedConnectionNonce,
  )
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( AdministrationDispositionTarget (..),
    ApplicationDispositionTarget (..),
    EffectBatch,
    HeraldEffect (..),
    PeerProtocolDisposition (..),
    effectBatchMembers,
    emptyEffectBatch,
    orderedEffectBatch,
    singletonEffectBatch,
  )
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch)
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Input
  ( AdministrationIngress (..),
    ApplicationLifecycleIngress (..),
    ApplicationRequestIngress (..),
    ApplicationSessionIngress (..),
    CandidateApplicationLane,
    DisappearanceIngress (..),
    HeraldInput,
    HeraldInputBody (..),
    PeerControl (..),
    PeerIngress (..),
    RuntimeObservation (..),
    administrationIngressCorrelation,
    heraldInput,
    inputBody,
    inputObservedAt,
  )
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.OracleClient (OracleClientIngress (..), oracleContacts)
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection qualified as OracleProjection
import Eclips.Herald.OracleProjection.State qualified as OracleProjectionState
import Eclips.Herald.PeerDispatch.Internal (peerLogicalSequencedItem)
import Eclips.Herald.PeerLiveness.State qualified as PeerLiveness
import Eclips.Herald.PeerPayload (PeerLogicalPayload)
import Eclips.Herald.PeerStream
  ( SequencedItem,
    streamDirectionSource,
  )
import Eclips.Herald.Placement (PlacementUpdate (FullPlacementSnapshot))
import Eclips.Herald.ProcessPreparation.State qualified as PreparationState
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (..),
    HeraldTransitionInvariantViolation (..),
  )
import Eclips.Herald.Startup.State
  ( HeraldDrainWitness,
    HeraldPhase (..),
    HeraldState,
    beginHeraldDrain,
    finishHeraldDrain,
    freezeStartupSemanticControl,
    heraldDrainWitnessId,
    heraldDrainWitnessRequest,
    heraldPhase,
    replaceStartupAdministrationState,
    replaceStartupAlignmentState,
    replaceStartupApplicationState,
    replaceStartupControlledState,
    replaceStartupDisappearanceState,
    replaceStartupGraphState,
    replaceStartupIdGeneratorState,
    replaceStartupIsolationState,
    replaceStartupLabelBarrierState,
    replaceStartupLastObservedTime,
    replaceStartupPeerLivenessState,
    replaceStartupPeerStreamState,
    replaceStartupPlacementState,
    replaceStartupProcessPreparationState,
    replaceStartupPublicationState,
    replaceStartupSortRegistryState,
    replaceStartupStoreState,
    replaceStartupStructuralProgressState,
    replaceStartupTerminalSourceHoldState,
    replaceStartupWaitState,
    startupAdministrationState,
    startupAlignmentState,
    startupApplicationState,
    startupControlOracleProjectionState,
    startupControlledState,
    startupDisappearanceState,
    startupDiscoveryState,
    startupDrainWitness,
    startupGenesis,
    startupGraphState,
    startupIdGeneratorState,
    startupIsolationState,
    startupLabelBarrierState,
    startupLastObservedTime,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPeerLivenessState,
    startupPeerStreamState,
    startupPlacementState,
    startupProcessPreparationState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralProgressState,
    startupTerminalSourceHoldState,
    startupWaitState,
  )
import Eclips.Herald.TerminalSourceHold.State qualified as TerminalSourceHold
import Eclips.Herald.Time (MonotonicInstant)
import Eclips.Herald.Timer (TimerAttempt, TimerOutcome (..))
import Eclips.Herald.Timer.Internal (timerAttemptPeerRecovery)
import Eclips.Herald.UseCase.Administration qualified as AdministrationUseCase
import Eclips.Herald.UseCase.ApplicationCall
  ( ApplicationLabelWorkDisposition (..),
    advanceApplicationLabelWork,
    applyApplicationRequest,
    rejectApplicationRequestDuringIsolation,
  )
import Eclips.Herald.UseCase.ApplicationLiveness qualified as ApplicationLiveness
import Eclips.Herald.UseCase.ApplicationRetirement qualified as ApplicationRetirement
import Eclips.Herald.UseCase.DisappearanceLive qualified as DisappearanceLive
import Eclips.Herald.UseCase.FailureDetection qualified as FailureDetection
import Eclips.Herald.UseCase.Join qualified as Join
import Eclips.Herald.UseCase.OracleAdvance
  ( advanceLocalLabelWork,
    applyFencedOracleIngress,
    applyOracleIngress,
    applyResidentEndWithUnavailableReason,
    cutOracleClientForDrain,
  )
import Eclips.Herald.UseCase.PeerControl
  ( PeerControlServingDisposition (..),
    applyDrainingPeerDispatchObservation,
    applyPeerBindingLoss,
    applyPeerControlWithInstalledCuts,
    applyPeerDispatchObservation,
    applyPeerDispatchSelection,
    applyPeerHello,
    cutPeerDispatchForDrain,
    observePeerCandidateOpened,
    redriveTerminalSourceReadinessIfProgressed,
  )
import Eclips.Herald.UseCase.PeerDelivery qualified as PeerDelivery
import Eclips.Herald.UseCase.PeerInput qualified as PeerInput
import Eclips.Herald.UseCase.ProcessPreparation qualified as ProcessPreparation
import Eclips.Herald.UseCase.StructuralCoordinator qualified as StructuralCoordinator
import Eclips.Herald.UseCase.StructuralProgress qualified as StructuralProgress
import Eclips.Herald.Wait.State qualified as Wait
import Eclips.Oracle.Genesis (raftVoterBindingNode)
import Eclips.Oracle.Voter (OracleReplicaRegistration)
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)

-- | Apply one already selected and timestamped input.
--
-- A stopped Herald is total and inert, including for a timestamp lower than the
-- last retained observation. Serving and draining reject a time regression as an
-- invariant fault without exposing a successor.
stepHerald ::
  HeraldInput ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
stepHerald input predecessor = do
  (successor, effects) <- stepHeraldSelected input predecessor
  let !administration = Administration.retireClosedAdministrationReceipts (startupAdministrationState successor)
  retired <- Join.releaseFinishedJoinTransfers (replaceStartupAdministrationState administration successor)
  PeerDelivery.normalizeEffects retired effects

stepHeraldSelected :: HeraldInput -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
stepHeraldSelected input predecessor
  | heraldPhase predecessor == HeraldStopped =
      Right (predecessor, emptyEffectBatch)
  | inputObservedAt input < startupLastObservedTime predecessor =
      transitionFault HeraldObservationTimeRegressed
  | timerObservationAfterInput input =
      transitionFault HeraldTimerObservationContradiction
  | heraldPhase predecessor == HeraldDraining =
      dispatchDraining
        (inputBody input)
        (replaceStartupLastObservedTime (inputObservedAt input) predecessor)
  | RuntimeObserved (ApplicationCandidateLost lane) <- inputBody input =
      checkedSuccess
        ( replaceStartupProcessPreparationState
            (PreparationState.observeCandidateLoss lane (startupProcessPreparationState predecessor))
            (replaceStartupLastObservedTime (inputObservedAt input) predecessor)
        )
        emptyEffectBatch
  | JoinInput correlation bytes <- inputBody input = do
      (successor, effects) <- Join.applyJoinRequest correlation bytes (replaceStartupLastObservedTime (inputObservedAt input) predecessor)
      if not (localHeraldIsCurrent predecessor) && localHeraldIsCurrent successor
        then finalizeObservedControlledObsolescence predecessor (inputObservedAt input) (inputBody input) (successor, effects, PeerControlServingWorkRequired)
        else checkedSuccess successor effects
  | ApplicationReceiptRetirementInput ingress <- inputBody input = do
      (prepared, retirementEffects, work) <-
        ApplicationRetirement.applyApplicationReceiptRetirement
          ingress
          (replaceStartupLastObservedTime (inputObservedAt input) predecessor)
      case work of
        Nothing -> checkedSuccess prepared retirementEffects
        Just body -> do
          (successor, effects) <- stepHerald (heraldInput (inputObservedAt input) body) prepared
          Right (successor, retirementEffects <> effects)
  | isolationTerminal predecessor =
      dispatchIsolationTerminal
        (inputBody input)
        (replaceStartupLastObservedTime (inputObservedAt input) predecessor)
  | isolationReadOnlyDrain predecessor =
      dispatchIsolationReadOnly
        (inputBody input)
        (replaceStartupLastObservedTime (inputObservedAt input) predecessor)
  | not (localHeraldIsCurrent predecessor) =
      dispatchLocallyRetired
        (inputBody input)
        (replaceStartupLastObservedTime (inputObservedAt input) predecessor)
  | otherwise =
      let body = inputBody input
       in finalizeObservedControlledObsolescence
            predecessor
            (inputObservedAt input)
            body
            =<< dispatchWithServingDisposition
              body
              (replaceStartupLastObservedTime (inputObservedAt input) predecessor)

timerObservationAfterInput :: HeraldInput -> Bool
timerObservationAfterInput input = case inputBody input of
  RuntimeObserved (TimerObserved _ (TimerFired firedAt)) ->
    firedAt > inputObservedAt input
  _ -> False

isolationTerminal :: HeraldState -> Bool
isolationTerminal =
  (== Isolation.IsolationTerminalView)
    . Isolation.isolationWitnessPhase
    . Isolation.stateWitness
    . startupIsolationState

isolationReadOnlyDrain :: HeraldState -> Bool
isolationReadOnlyDrain =
  (== Isolation.IsolationReadOnlyDrainView)
    . Isolation.isolationWitnessPhase
    . Isolation.stateWitness
    . startupIsolationState

-- | Semantic termination retains only the control shell and typed rejection of
-- new application/peer candidates. Oracle observations cannot unseal the epoch.
dispatchIsolationTerminal :: HeraldInputBody -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchIsolationTerminal body state = case body of
  AdministrationInput ingress -> dispatchReadOnlyAdministration ingress state
  OracleInput ingress -> applyFencedOracleIngress ingress state
  ApplicationLifecycleInput ingress@RecoverApplicationLifecycleResult {} -> dispatchIsolatedLifecycle ingress state
  ApplicationSessionInput (OpenApplicationSession candidate _ _) -> rejectIsolationCandidate candidate state
  ApplicationSessionInput (ResumeApplicationSession candidate _ _ _) -> rejectIsolationCandidate candidate state
  ApplicationSessionInput (ClaimInitialApplicationSession candidate _ claim _) ->
    checkedSuccess state (singletonEffectBatch (RejectInitialClaim candidate claim Lifecycle.StartupNotLive))
  PeerInput ingress -> dispatchFencedPeer ingress state
  RuntimeObserved (AdministrationBindingLost binding) ->
    dispatchDrainingObservation (AdministrationBindingLost binding) state
  _ -> checkedSuccess state emptyEffectBatch

-- | The losing branch retains only local reads and retained-result lookup
-- during its one bounded drain. All other semantic ingress is cut locally.
dispatchIsolationReadOnly ::
  HeraldInputBody ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchIsolationReadOnly body state = case body of
  OracleInput ingress -> applyFencedOracleIngress ingress state
  PeerInput ingress -> dispatchFencedPeer ingress state
  ApplicationRequestInput request -> case request of
    CallApplicationRequest _ _ _ (ReadApplication _) ->
      applyApplicationRequest request state
    GetApplicationRequestResult {} -> applyApplicationRequest request state
    _ -> rejectApplicationRequestDuringIsolation request state
  ApplicationLifecycleInput ingress -> dispatchIsolatedLifecycle ingress state
  AdministrationInput ingress -> dispatchReadOnlyAdministration ingress state
  ApplicationSessionInput ingress -> case ingress of
    OpenApplicationSession candidate _ _ ->
      rejectIsolationCandidate candidate state
    ResumeApplicationSession candidate _ _ _ ->
      rejectIsolationCandidate candidate state
    ClaimInitialApplicationSession candidate _ claim _ ->
      checkedSuccess state (singletonEffectBatch (RejectInitialClaim candidate claim Lifecycle.StartupNotLive))
    EndApplicationSession {} -> do
      (afterEnd, effects) <- dispatchApplicationSessionAdmitted ingress state
      if null (liveApplicationSessionIds afterEnd)
        then do
          (terminal, terminalEffects) <- completeIsolationDrainEarly afterEnd
          checkedSuccess terminal (effects <> terminalEffects)
        else checkedSuccess afterEnd effects
  RuntimeObserved observation -> case observation of
    TimerObserved attempt outcome -> applyReadOnlyDrainTimer attempt outcome state
    ApplicationBindingLost binding ->
      do
        (afterLoss, _) <-
          ApplicationLiveness.applyApplicationBindingLoss
            (startupLastObservedTime state)
            binding
            state
        if null (liveApplicationSessionIds afterLoss)
          then completeIsolationDrainEarly afterLoss
          else checkedSuccess afterLoss emptyEffectBatch
    AdministrationBindingLost binding ->
      dispatchDrainingObservation (AdministrationBindingLost binding) state
    _ -> checkedSuccess state emptyEffectBatch
  _ -> checkedSuccess state emptyEffectBatch

-- Physical peer managers can finish a connect while the semantic owner is
-- fenced. Every paused candidate still needs its exact rejecting disposition;
-- established peer traffic has no remaining semantic authority.
dispatchFencedPeer :: PeerIngress -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchFencedPeer ingress state = case ingress of
  PeerCandidateOpened opened ->
    checkedSuccess state (singletonEffectBatch (RejectPeerCandidateOpened (peerCandidateOpenedConnectionNonce opened)))
  PeerHelloReceived candidate _ _ _ _ _ ->
    checkedSuccess state (singletonEffectBatch (SetPeerCandidateDisposition candidate (PeerHelloRejected HelloInactiveHerald)))
  _ -> checkedSuccess state emptyEffectBatch

-- Retained lifecycle observations remain available during the local drain.
-- New lifecycle mutations cannot be accepted on the isolated branch.
dispatchIsolatedLifecycle :: ApplicationLifecycleIngress -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchIsolatedLifecycle ingress state = case ingress of
  CallApplicationLifecycle binding _ request _ _ ->
    checkedSuccess
      state
      ( singletonEffectBatch
          ( SendApplicationLifecycleReply
              (EstablishedApplicationDisposition binding)
              (Lifecycle.LifecycleReply request (Lifecycle.LifecycleRejected Lifecycle.LifecycleTargetUnavailable))
          )
      )
  _ -> retainLocalEffects <$> ProcessPreparation.applyProcessPreparationIngress ingress state

rejectIsolationCandidate ::
  CandidateApplicationLane ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
rejectIsolationCandidate candidate state =
  checkedSuccess
    state
    ( singletonEffectBatch
        ( RejectApplicationConnection
            (CandidateApplicationDisposition candidate)
            ApplicationSessionNotLive
        )
    )

applyReadOnlyDrainTimer ::
  TimerAttempt ->
  TimerOutcome ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyReadOnlyDrainTimer attempt outcome state =
  case Isolation.observeDrainTimer attempt outcome (startupIsolationState state) of
    (isolation, Isolation.IsolationDrainTimerRearmed successor specification) ->
      checkedSuccess
        (replaceStartupIsolationState isolation state)
        (singletonEffectBatch (ArmTimer successor specification))
    (isolation, Isolation.IsolationDrainTimerExpired drain) ->
      let withIsolation = replaceStartupIsolationState isolation state
       in do
            (successor, retirementEffects) <-
              retireIsolationApplicationOwners withIsolation
            checkedSuccess
              successor
              (orderedEffectBatch (retirementEffects <> [FinishIsolation drain]))
    (_, Isolation.IsolationDrainTimerStale) ->
      checkedSuccess state emptyEffectBatch

completeIsolationDrainEarly ::
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
completeIsolationDrainEarly state =
  case Isolation.isolationWitnessCurrentDrain
    (Isolation.stateWitness (startupIsolationState state)) of
    Nothing -> checkedSuccess state emptyEffectBatch
    Just drain ->
      case Isolation.completeReadOnlyDrain drain (startupIsolationState state) of
        (isolation, Isolation.IsolationDrainCompleted _ timer) ->
          let withIsolation = replaceStartupIsolationState isolation state
           in do
                (successor, retirementEffects) <-
                  retireIsolationApplicationOwners withIsolation
                checkedSuccess
                  successor
                  ( orderedEffectBatch
                      (CancelTimer timer : retirementEffects <> [FinishIsolation drain])
                  )
        (_, Isolation.IsolationDrainCompletionStale) ->
          checkedSuccess state emptyEffectBatch

-- | Small immutable projection published by a runtime's sole kernel owner.
-- The cursor makes unchanged registration work cheap to skip.
heraldOracleReplicaProjection :: HeraldState -> (ControlIndex, [OracleReplicaRegistration])
heraldOracleReplicaProjection state =
  let view = OracleProjection.oracleView (startupControlOracleProjectionState state)
   in (OracleProjection.oracleViewControlIndex view, OracleProjection.oracleViewOracleReplicas view)

localHeraldIsCurrent :: HeraldState -> Bool
localHeraldIsCurrent =
  OracleProjection.oracleViewLocalHeraldIsCurrent
    . OracleProjection.oracleView
    . startupOracleProjectionState

dispatchLocallyRetired ::
  HeraldInputBody ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchLocallyRetired body state = case body of
  PeerInput ingress -> dispatchFencedPeer ingress state
  ApplicationRequestInput ingress@CallApplicationRequest {} ->
    case ingress of
      CallApplicationRequest _ _ _ (ReadApplication _) ->
        retainLocalEffects <$> applyApplicationRequest ingress state
      _ -> inert
  ApplicationRequestInput ingress@GetApplicationRequestResult {} ->
    retainLocalEffects <$> applyApplicationRequest ingress state
  ApplicationLifecycleInput ingress -> dispatchIsolatedLifecycle ingress state
  ApplicationSessionInput (OpenApplicationSession candidate _ _) ->
    rejectIsolationCandidate candidate state
  ApplicationSessionInput (ResumeApplicationSession candidate _ _ _) ->
    rejectIsolationCandidate candidate state
  ApplicationSessionInput (ClaimInitialApplicationSession candidate _ claim _) ->
    checkedSuccess state (singletonEffectBatch (RejectInitialClaim candidate claim Lifecycle.StartupNotLive))
  AdministrationInput ingress -> dispatchReadOnlyAdministration ingress state
  OracleInput ingress -> retainLocalEffects <$> applyOracleIngress ingress state
  _ -> inert
  where
    inert = Right (state, emptyEffectBatch)

-- | Operator connection and inspection remain local while semantic mutations
-- are fenced. Candidate admission still produces its required typed disposition.
dispatchReadOnlyAdministration :: AdministrationIngress -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchReadOnlyAdministration ingress state = case ingress of
  RetireAdministrationReceipts binding progress work -> dispatchAdministrationRetirement dispatchReadOnlyAdministration binding progress work state
  OpenAdministrationConnection {} -> dispatchAdministration ingress state
  GetAdministrationResult {} -> dispatchAdministration ingress state
  GetOracleConfiguration {} -> dispatchAdministration ingress state
  GetVoterChangeStatus {} -> dispatchAdministration ingress state
  GetHeraldStatus {} -> dispatchAdministration ingress state
  ListChildPreparations {} -> dispatchAdministration ingress state
  PrepareOracleReplica binding _ -> reject binding
  BeginVoterChange binding _ _ _ _ -> reject binding
  CancelVoterChange binding _ _ -> reject binding
  StartProcessEpoch binding _ -> reject binding
  EndProcessEpoch binding _ -> reject binding
  CancelChildPreparation binding _ _ -> reject binding
  DrainConfiguredHerald {} -> dispatchAdministration ingress state
  OrderlyHeraldShutdown {} -> dispatchAdministration ingress state
  where
    reject binding = checkedSuccess state (singletonEffectBatch (RejectAdministrationConnection (EstablishedAdministrationDisposition binding)))

retainLocalEffects :: (HeraldState, EffectBatch) -> (HeraldState, EffectBatch)
retainLocalEffects (state, effects) =
  (state, orderedEffectBatch (filter localEffect (effectBatchMembers effects)))
  where
    localEffect = \case
      SetApplicationConnectionDisposition {} -> True
      RejectApplicationConnection {} -> True
      SendApplicationReply {} -> True
      SendApplicationLifecycleReply {} -> True
      SendInitialClaimPending {} -> True
      RejectInitialClaim {} -> True
      SendAdministrationReply {} -> True
      SendApplicationWaitWake {} -> True
      SendApplicationIsolationBegun {} -> True
      DisposeApplicationSession {} -> True
      CancelTimer {} -> True
      _ -> False

-- | The input timestamp, not a later Forward attempt, is the first-observation
-- instant for every controlled winner that became obsolete in this atomic
-- transition.  This post-composition step covers local publications, incoming
-- peer publications, and work released by Oracle/control progress uniformly.
-- The connection-lifecycle fast path below cannot change a controlled winner.
finalizeObservedControlledObsolescence ::
  HeraldState ->
  MonotonicInstant ->
  HeraldInputBody ->
  (HeraldState, EffectBatch, PeerControlServingDisposition) ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
finalizeObservedControlledObsolescence predecessor observedAt _ (successor, effects, _)
  | localHeraldIsCurrent predecessor,
    not (localHeraldIsCurrent successor) =
      case Isolation.prepareLocallyLearnedRetirement
        observedAt
        ( heraldMembershipGenerationId
            ( OracleProjection.oracleViewCurrentHeraldMembership
                (OracleProjection.oracleView (startupOracleProjectionState successor))
            )
        )
        (startupIsolationState successor) of
        Nothing -> Right (successor, deduplicateOracleActions effects)
        Just prepared -> do
          (fenced, fenceEffects) <- commitIsolationFence prepared successor
          Right (fenced, deduplicateOracleActions (effects <> fenceEffects))
finalizeObservedControlledObsolescence _ _ body (successor, effects, disposition)
  | applicationReadIsLocal body =
      finalizeLocalReadControlledObservation successor effects
  | not (servingDriversNeeded body disposition) =
      case body of
        OracleInput OracleHelloReceived {} -> do
          (prepared, preparationEffects) <- advancePreparationWork successor
          Right (prepared, deduplicateOracleActions (effects <> preparationEffects))
        _ -> Right (successor, deduplicateOracleActions effects)
finalizeObservedControlledObsolescence predecessor observedAt _ (successor, effects, _) = do
  (afterAlignment, alignmentEffects) <-
    if heraldPhase successor == HeraldServing
      then advanceAlignmentAndPending predecessor successor
      else Right (successor, emptyEffectBatch)
  (afterLabel, labelEffects) <-
    if heraldPhase afterAlignment == HeraldServing
      then advanceLocalLabelWork afterAlignment
      else Right (afterAlignment, emptyEffectBatch)
  (afterApplication, applicationEffects, applicationDisposition) <-
    if heraldPhase afterLabel == HeraldServing
      then advanceApplicationLabelWork afterLabel
      else
        Right
          ( afterLabel,
            emptyEffectBatch,
            ApplicationLabelWorkUnchanged
          )
  (afterApplicationProgress, applicationProgressEffects) <-
    if heraldPhase afterApplication == HeraldServing
      && applicationDisposition == ApplicationLabelWorkChanged
      then advanceAlignmentAndPending afterLabel afterApplication
      else Right (afterApplication, emptyEffectBatch)
  -- Resumed providers or a promoted label can make the source group ready in
  -- this transition. Finish that work without waiting for unrelated ingress.
  (afterSourceLabel, sourceLabelEffects) <-
    if heraldPhase afterApplicationProgress == HeraldServing
      && applicationDisposition == ApplicationLabelWorkChanged
      then advanceLocalLabelWork afterApplicationProgress
      else Right (afterApplicationProgress, emptyEffectBatch)
  (afterDisappearance, disappearanceEffects) <-
    if heraldPhase afterSourceLabel == HeraldServing
      then DisappearanceLive.advanceDisappearanceWork afterSourceLabel
      else Right (afterSourceLabel, emptyEffectBatch)
  (afterPreparation, preparationEffects) <-
    if heraldPhase afterDisappearance == HeraldServing
      then advancePreparationWork afterDisappearance
      else Right (afterDisappearance, emptyEffectBatch)
  finalized <-
    finalizeControlledObsolescenceAt observedAt afterPreparation
  let
    finalizedEffects =
      effects
        <> alignmentEffects
        <> labelEffects
        <> applicationEffects
        <> applicationProgressEffects
        <> sourceLabelEffects
        <> disappearanceEffects
        <> preparationEffects
  normalizedEffects <-
    deduplicateStructuralReports
      predecessor
      finalized
      (deduplicateOracleActions finalizedEffects)
  Right (finalized, orderLifecycleTerminalReplies normalizedEffects)

-- | Preparation owns lifecycle progress; administration observes the resulting
-- cancellation disposition through the same retained work.
advancePreparationWork :: HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
advancePreparationWork state = do
  (afterStarts, startEffects) <- AdministrationUseCase.advanceGatedStarts state
  (afterSessions, sessionEffects) <- advanceDeferredApplicationSessions afterStarts
  (prepared, preparationEffects) <- ProcessPreparation.advanceProcessPreparations afterSessions
  (observed, administrationEffects) <- AdministrationUseCase.advancePreparationCancellations prepared
  Right (observed, startEffects <> sessionEffects <> preparationEffects <> administrationEffects)

advanceDeferredApplicationSessions :: HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
advanceDeferredApplicationSessions state
  | Application.applicationMembershipGateClosed (startupApplicationState state) = Right (state, emptyEffectBatch)
  | otherwise = foldM replay (state, emptyEffectBatch) (PreparationState.deferredSessionEntries (startupProcessPreparationState state))
  where
    replay (predecessor, effects) (lane, ingress) = do
      let ready = replaceStartupProcessPreparationState (PreparationState.removeDeferredSession lane (startupProcessPreparationState predecessor)) predecessor
      (successor, emitted) <- dispatchApplicationSessionAdmitted ingress ready
      Right (successor, effects <> emitted)

-- | Offer End's terminal result before disposing its session's delivery lane.
-- The effect owns the reply after terminal session receipts are reclaimed.
orderLifecycleTerminalReplies :: EffectBatch -> EffectBatch
orderLifecycleTerminalReplies effects =
  let (terminal, remaining) = partition isOwnEnd (effectBatchMembers effects)
   in orderedEffectBatch (terminal <> remaining)
  where
    isOwnEnd = \case
      SendApplicationLifecycleReply _ (Lifecycle.LifecycleReply _ (Lifecycle.LifecycleCompleted Lifecycle.ProcessEnded)) -> True
      _ -> False

-- A read evaluates the already serialized local Store cut. It must not become
-- an accidental scheduler tick for alignment, structural, label, publication,
-- or Oracle work that happened to be pending when the application called it.
-- Controlled-value localization can still record a local read observation, so
-- retain the ordinary observed-obsolescence finalization without running any
-- distributed-work driver.
finalizeLocalReadControlledObservation ::
  HeraldState ->
  EffectBatch ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
finalizeLocalReadControlledObservation successor effects = do
  finalized <-
    finalizeControlledObsolescenceAt
      (startupLastObservedTime successor)
      successor
  Right (finalized, deduplicateOracleActions effects)

finalizeControlledObsolescenceAt ::
  MonotonicInstant ->
  HeraldState ->
  Either HeraldInvariantFault HeraldState
finalizeControlledObsolescenceAt observedAt successor = do
  prepared <-
    either
      (const (transitionFault HeraldControlledTransitionContradiction))
      Right
      ( Controlled.prepareControlledObsoleteWinnerTimes
          observedAt
          (startupControlledState successor)
      )
  let (controlled, _) =
        Controlled.commitControlledObsoleteWinnerTimes prepared
  Right (replaceStartupControlledState controlled successor)

applicationReadIsLocal :: HeraldInputBody -> Bool
applicationReadIsLocal body = case body of
  ApplicationRequestInput
    (CallApplicationRequest _ _ _ (ReadApplication _)) -> True
  _ -> False

-- Oracle connection lifecycle and request-reoffer timing change only the
-- Oracle-client owner. The structural, label, and application drivers were at
-- their local fixed point before the input and receive no new evidence from
-- these ingress forms, so rerunning them would be pure duplicate work. Applied
-- entries and semantic deferrals do change projection/application facts and
-- retain the complete post-composition path. Runtime ownership guarantees that
-- a submission for an admitted binding is either queued on its current lane or
-- followed by typed binding loss; unrelated callbacks are not delivery retries.
servingDriversNeeded ::
  HeraldInputBody ->
  PeerControlServingDisposition ->
  Bool
servingDriversNeeded _ PeerControlGenerationEvidenceUnchanged = False
servingDriversNeeded _ PeerControlGenerationAcknowledgementHandled = False
servingDriversNeeded _ PeerControlCurrentEvidenceRejected = False
servingDriversNeeded body PeerControlServingWorkRequired = case body of
  OracleInput ingress -> case ingress of
    OracleContactsDiscovered {} -> False
    OracleLocalReplicaConfigured {} -> False
    OracleSubmissionDeferredReceived {} -> True
    OracleEntriesReceived {} -> True
    OracleConnectFailed {} -> False
    OracleHelloReceived {} -> False
    OracleRedirectReceived {} -> False
    OracleBindingLost {} -> False
    OracleRetryElapsed {} -> False
    OracleSubmissionNotReadyReceived {} -> False
    OracleProgressConfirmed {} -> False
    OracleProgressFlush {} -> False
    OracleProgressNotReadyReceived {} -> False
    OracleRequestRetiredReceived {} -> False
  RuntimeObserved (OracleHealthRoundObserved _ _) -> False
  RuntimeObserved (TimerObserved _ _) -> False
  _ -> True

-- | A direct producer and the level-triggered post-dispatch driver may both
-- offer the same retained Oracle action in one atomic transition. Preserve the
-- first semantic occurrence and every non-Oracle effect exactly; only an
-- identical later Oracle runtime action is redundant.
deduplicateOracleActions :: EffectBatch -> EffectBatch
deduplicateOracleActions = orderedEffectBatch . go [] . effectBatchMembers
  where
    go _ [] = []
    go seen (effect : remaining) = case effect of
      RunOracleClientAction action
        | action `elem` seen -> go seen remaining
        | otherwise -> effect : go (action : seen) remaining
      _ -> effect : go seen remaining

-- | Normalize every same-generation offer to the final composed cumulative
-- report at its first protocol-ordering slot, while removing later repeats.
-- A current predecessor binding has already been offered its predecessor
-- report on the reliable ordered lane; physical loss produces binding loss and
-- repair. A newly accepted physical binding defers repair until its ResumeOffer
-- is answered. Only a ResumeAccepted batch which actually carries the explicit
-- structural reconnect root resets the assumed evidence; later stream-repair
-- responses on the same binding must not manufacture another report offer.
-- A report from a predecessor generation remains byte-for-byte at its original
-- slot so terminal Established can retain its generation-changing boundary.
deduplicateStructuralReports ::
  HeraldState ->
  HeraldState ->
  EffectBatch ->
  Either HeraldInvariantFault EffectBatch
deduplicateStructuralReports predecessor successor effects =
  Right
    ( StructuralProgress.normalizeStructuralReportEffects
        initialReports
        finalBindings
        finalReport
        effects
    )
  where
    predecessorReport =
      GraphProgress.structuralLocalReport
        (startupStructuralProgressState predecessor)
    initialReports =
      Map.fromList
        [ (binding, predecessorReport)
        | binding <-
            StructuralProgress.structuralReportBindings
              (startupStructuralProgressState predecessor)
              (Discovery.currentPeerBindings (startupDiscoveryState predecessor))
        ]
    finalReport =
      GraphProgress.structuralLocalReport
        (startupStructuralProgressState successor)
    finalBindings =
      StructuralProgress.structuralReportBindings
        (startupStructuralProgressState successor)
        (Discovery.currentPeerBindings (startupDiscoveryState successor))

advanceAlignmentAndPending ::
  HeraldState ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
advanceAlignmentAndPending streamBaseline initial =
  either
    (const (transitionFault HeraldPeerTransitionContradiction))
    Right
    (StructuralCoordinator.advanceAlignmentAndPending streamBaseline initial)

dispatch ::
  HeraldInputBody ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatch body state = case heraldPhase state of
  HeraldServing -> dispatchServing body state
  HeraldDraining -> dispatchDraining body state
  HeraldStopped -> Right (state, emptyEffectBatch)

-- | Preserve the successful generation-evidence disposition across peer
-- dispatch. No input constructor alone can claim that advertised evidence was
-- unchanged: stale bindings, blocked dependencies, and protocol rejection all
-- retain the conservative full-driver path.
dispatchWithServingDisposition ::
  HeraldInputBody ->
  HeraldState ->
  Either
    HeraldInvariantFault
    (HeraldState, EffectBatch, PeerControlServingDisposition)
dispatchWithServingDisposition body state =
  case (heraldPhase state, body) of
    (HeraldServing, PeerInput (PeerControlReceived binding control)) ->
      applyPeerControlAndSettleWithDisposition binding control state
    (HeraldServing, PeerInput (PeerControlReceivedWithProgress binding progress control)) ->
      withPeerDeliveryProgress
        binding
        progress
        (applyPeerControlAndSettleWithDisposition binding control)
        state
    (HeraldServing, OracleInput _) -> do
      (projected, projectionEffects) <- dispatch body state
      (afterAheadReplay, replayEffects) <-
        replayHeldTerminalSourceControls projected
      (afterReadiness, readinessEffects, installedCuts) <-
        redriveTerminalSourceReadinessIfProgressed state afterAheadReplay
      (successor, settlementEffects) <-
        case StructuralCoordinator.settleInstalledStructuralCuts
          installedCuts
          afterReadiness of
          Left _ -> transitionFault HeraldPeerTransitionContradiction
          Right settled -> Right settled
      Right
        ( successor,
          projectionEffects
            <> replayEffects
            <> readinessEffects
            <> settlementEffects,
          PeerControlServingWorkRequired
        )
    _ -> do
      (successor, effects) <- dispatch body state
      Right (successor, effects, PeerControlServingWorkRequired)

-- | Replay only the occurrences whose own target has become authoritative.
-- Later targets remain retained, and historical readiness becomes inert.
replayHeldTerminalSourceControls ::
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
replayHeldTerminalSourceControls state
  | null held = Right (state, emptyEffectBatch)
  | not (localHeraldIsCurrent state) =
      Right
        ( replaceStartupTerminalSourceHoldState TerminalSourceHold.initialState state,
          emptyEffectBatch
        )
  | otherwise =
      case TerminalSourceHold.resolveForMembershipAdvance history holdState of
        TerminalSourceHold.TerminalSourceHoldUnchanged _ ->
          Right (state, emptyEffectBatch)
        TerminalSourceHold.TerminalSourceHoldMatched retained controls ->
          foldM
            replayOne
            (replaceStartupTerminalSourceHoldState retained state, emptyEffectBatch)
            controls
  where
    holdState = startupTerminalSourceHoldState state
    held = TerminalSourceHold.heldTerminalSourceControls holdState
    history =
      OracleProjectionState.oracleViewHeraldMembershipHistoryChecked
        (OracleProjection.oracleView (startupOracleProjectionState state))
    replayOne (predecessor, retainedEffects) (binding, control) = do
      (successor, effects, _) <-
        applyPeerControlAndSettleWithDisposition binding control predecessor
      Right (successor, retainedEffects <> effects)

dispatchServing ::
  HeraldInputBody ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchServing body state = case body of
  ApplicationReceiptRetirementInput _ -> transitionFault HeraldApplicationTransitionContradiction
  JoinInput correlation bytes -> Join.applyJoinRequest correlation bytes state
  ApplicationSessionInput ingress ->
    dispatchApplicationSession ingress state
  ApplicationRequestInput ingress ->
    applyApplicationRequest ingress state
  ApplicationLifecycleInput ingress ->
    ProcessPreparation.applyProcessPreparationIngress ingress state
  AdministrationInput ingress ->
    dispatchAdministration ingress state
  DisappearanceInput (AbortDisappearanceProbe probe) ->
    DisappearanceLive.applyDisappearanceAbort probe state
  PeerInput ingress -> dispatchPeerIngress ingress state
  OracleInput ingress -> applyOracleIngress ingress state
  RuntimeObserved observation ->
    dispatchServingObservation observation state

dispatchApplicationSession ::
  ApplicationSessionIngress ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchApplicationSession ingress state = do
  (afterRecovery, recoveryEffects) <-
    ApplicationLiveness.advanceApplicationRecoveryBeforeSessionIngress
      (startupLastObservedTime state)
      ingress
      state
  (successor, ingressEffects) <-
    dispatchApplicationSessionAdmitted ingress afterRecovery
  Right (successor, recoveryEffects <> ingressEffects)

retainGatedSession :: ApplicationSessionIngress -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
retainGatedSession ingress state = case PreparationState.retainDeferredSession ingress (startupProcessPreparationState state) of
  Left _ -> transitionFault HeraldApplicationTransitionContradiction
  Right owner -> case ingress of
    OpenApplicationSession candidate _ _ -> deferred candidate owner
    ResumeApplicationSession candidate _ _ _ -> deferred candidate owner
    _ -> transitionFault HeraldApplicationTransitionContradiction
  where
    deferred candidate owner =
      checkedSuccess
        (replaceStartupProcessPreparationState owner state)
        (singletonEffectBatch (DeferApplicationSession candidate))

dispatchApplicationSessionAdmitted ::
  ApplicationSessionIngress ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchApplicationSessionAdmitted ingress state = case ingress of
  ClaimInitialApplicationSession candidate descriptor claim locator ->
    ProcessPreparation.applyInitialProcessClaim candidate descriptor claim locator state
  OpenApplicationSession candidate attachment nonce ->
    case Application.prepareApplicationSessionOpen
      (checkedLocalHeraldEpoch (startupGenesis state))
      attachment
      nonce
      applicationState of
      Left (Application.ApplicationSessionRejected rejection) ->
        checkedSuccess
          state
          ( singletonEffectBatch
              ( RejectApplicationConnection
                  (CandidateApplicationDisposition candidate)
                  rejection
              )
          )
      Left Application.ApplicationSessionGatePending -> retainGatedSession ingress state
      Left (Application.ApplicationSessionInvariantFault _) ->
        transitionFault HeraldApplicationTransitionContradiction
      Right prepared ->
        let (successorApplication, acceptance) =
              Application.commitApplicationSessionAcceptance prepared
            cancellationEffects =
              maybe
                []
                (pure . CancelTimer)
                (Application.preparedApplicationSessionAcceptanceTimerCancellation prepared)
         in checkedSuccess
              (replaceStartupApplicationState successorApplication state)
              ( orderedEffectBatch
                  ( cancellationEffects
                      <> [SetApplicationConnectionDisposition candidate acceptance]
                  )
              )
  ResumeApplicationSession candidate session token lastObservedReply ->
    case Application.prepareApplicationSessionResume
      session
      token
      lastObservedReply
      applicationState of
      Left (Application.ApplicationSessionRejected rejection) ->
        checkedSuccess
          state
          ( singletonEffectBatch
              ( RejectApplicationConnection
                  (CandidateApplicationDisposition candidate)
                  rejection
              )
          )
      Left Application.ApplicationSessionGatePending -> retainGatedSession ingress state
      Left (Application.ApplicationSessionInvariantFault _) ->
        transitionFault HeraldApplicationTransitionContradiction
      Right prepared ->
        let (successorApplication, acceptance) =
              Application.commitApplicationSessionAcceptance prepared
            cancellationEffects =
              maybe
                []
                (pure . CancelTimer)
                (Application.preparedApplicationSessionAcceptanceTimerCancellation prepared)
         in checkedSuccess
              (replaceStartupApplicationState successorApplication state)
              ( orderedEffectBatch
                  ( cancellationEffects
                      <> [SetApplicationConnectionDisposition candidate acceptance]
                  )
              )
  EndApplicationSession binding session ->
    case Application.prepareApplicationSessionEnd
      session
      binding
      applicationState of
      Left (Application.ApplicationSessionRejected rejection) ->
        checkedSuccess
          state
          ( singletonEffectBatch
              ( RejectApplicationConnection
                  (EstablishedApplicationDisposition binding)
                  rejection
              )
          )
      Left Application.ApplicationSessionGatePending ->
        transitionFault HeraldApplicationTransitionContradiction
      Left (Application.ApplicationSessionInvariantFault _) ->
        transitionFault HeraldApplicationTransitionContradiction
      Right prepared ->
        case Wait.prepareWaitRemoval
          (Application.preparedApplicationSessionEndWaitIds prepared)
          (startupWaitState state) of
          Left _ -> transitionFault HeraldApplicationTransitionContradiction
          Right preparedWait ->
            let (waitSuccessor, _) = Wait.commitWaitRemoval preparedWait
             in checkedSuccess
                  ( replaceStartupWaitState waitSuccessor
                      . replaceStartupApplicationState
                        (Application.commitApplicationSessionEnd prepared)
                      $ state
                  )
                  emptyEffectBatch
  where
    applicationState = startupApplicationState state

dispatchAdministration ::
  AdministrationIngress ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchAdministration ingress state
  | Just (binding, correlation) <- administrationIngressCorrelation ingress,
    not (adminCorrelationBelongsTo binding correlation) =
      checkedSuccess state (singletonEffectBatch (RejectAdministrationConnection (EstablishedAdministrationDisposition binding)))
  | Just (binding, correlation) <- administrationIngressCorrelation ingress,
    Administration.administrationCorrelationRetired binding correlation (startupAdministrationState state) =
      checkedSuccess state (singletonEffectBatch (SendAdministrationReply binding (RetiredAdminResult correlation (Administration.administrationReceiptRetirement (startupAdministrationState state)))))
  | otherwise = dispatchAdministrationWork ingress state

dispatchAdministrationWork :: AdministrationIngress -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchAdministrationWork ingress state = case ingress of
  RetireAdministrationReceipts binding progress work -> dispatchAdministrationRetirement dispatchAdministration binding progress work state
  PrepareOracleReplica {} -> AdministrationUseCase.applyConfiguredAdministrationIngress ingress state
  BeginVoterChange {} -> AdministrationUseCase.applyConfiguredAdministrationIngress ingress state
  CancelVoterChange {} -> AdministrationUseCase.applyConfiguredAdministrationIngress ingress state
  GetOracleConfiguration {} -> AdministrationUseCase.applyConfiguredAdministrationIngress ingress state
  GetVoterChangeStatus {} -> AdministrationUseCase.applyConfiguredAdministrationIngress ingress state
  GetHeraldStatus {} -> AdministrationUseCase.applyConfiguredAdministrationIngress ingress state
  ListChildPreparations {} -> AdministrationUseCase.applyConfiguredAdministrationIngress ingress state
  DrainConfiguredHerald binding correlation
    | not (Administration.configuredAdministrationBindingIsCurrent binding (startupAdministrationState state)) ->
        checkedSuccess state (singletonEffectBatch (RejectAdministrationConnection (EstablishedAdministrationDisposition binding)))
    | any (\(retained, _, _) -> retained == correlation) (Administration.administrationWitnessCommands (Administration.administrationStateWitness (startupAdministrationState state))) ->
        checkedSuccess state (singletonEffectBatch (SendAdministrationReply binding (ConflictingAdminResult correlation)))
    | otherwise -> case Administration.prepareConfiguredOrderlyShutdown binding correlation (startupAdministrationState state) of
        Left _ -> transitionFault HeraldAdministrationTransitionContradiction
        Right Nothing -> checkedSuccess state (singletonEffectBatch (SendAdministrationReply binding (AdminDrainAccepted correlation)))
        Right (Just prepared) -> do
          (successor, effects) <- dispatchPreparedOrderlyShutdown prepared state
          checkedSuccess successor (singletonEffectBatch (SendAdministrationReply binding (AdminDrainAccepted correlation)) <> effects)
  OpenAdministrationConnection {} ->
    AdministrationUseCase.applyConfiguredAdministrationIngress ingress state
  StartProcessEpoch {} ->
    AdministrationUseCase.applyConfiguredAdministrationIngress ingress state
  EndProcessEpoch {} ->
    AdministrationUseCase.applyConfiguredAdministrationIngress ingress state
  CancelChildPreparation {} ->
    AdministrationUseCase.applyConfiguredAdministrationIngress ingress state
  GetAdministrationResult {} ->
    AdministrationUseCase.applyConfiguredAdministrationIngress ingress state
  OrderlyHeraldShutdown binding correlation ->
    dispatchOrderlyShutdown binding correlation state

-- Retirement and attached work share admission, while the caller preserves the
-- serving or fenced policy for the attached operation.
dispatchAdministrationRetirement :: (AdministrationIngress -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)) -> AdministrationBinding -> ReceiptRetirement -> Maybe AdministrationIngress -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchAdministrationRetirement handleAttached binding progress work state
  | not (Administration.configuredAdministrationBindingIsCurrent binding (startupAdministrationState state)) =
      checkedSuccess state (singletonEffectBatch (RejectAdministrationConnection (EstablishedAdministrationDisposition binding)))
  | maybe False (\attached -> case administrationIngressCorrelation attached of Just (owner, _) -> owner /= binding; Nothing -> True) work =
      transitionFault HeraldAdministrationTransitionContradiction
  | otherwise = case Administration.retireAdministrationReceipts binding progress (startupAdministrationState state) of
      Left () -> checkedSuccess state (singletonEffectBatch (SendAdministrationReply binding AdministrationReceiptRetirementNotReady))
      Right administration -> do
        let prepared = replaceStartupAdministrationState administration state
            acknowledged = Administration.administrationReceiptRetirement administration
            ack = singletonEffectBatch (SendAdministrationReply binding (AdministrationReceiptsRetired acknowledged))
        case work of
          Nothing -> checkedSuccess prepared ack
          Just attached -> do
            (successor, effects) <- handleAttached attached prepared
            checkedSuccess successor (ack <> effects)

dispatchOrderlyShutdown ::
  AdministrationBinding ->
  AdminCorrelationId ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchOrderlyShutdown binding correlation state =
  case Administration.prepareOrderlyShutdown
    binding
    correlation
    (startupAdministrationState state) of
    Left _ -> transitionFault HeraldAdministrationTransitionContradiction
    Right Nothing -> checkedSuccess state emptyEffectBatch
    Right (Just prepared) -> dispatchPreparedOrderlyShutdown prepared state

dispatchPreparedOrderlyShutdown :: Administration.PreparedOrderlyShutdown -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchPreparedOrderlyShutdown prepared state =
  let (successorAdministration, reference) =
        Administration.commitOrderlyShutdown prepared
      withAdministration =
        replaceStartupAdministrationState successorAdministration state
   in do
        afterOracleCut <- cutOracleClientForDrain withAdministration
        afterPeerCut <- cutPeerDispatchForDrain afterOracleCut
        let (application, applicationTimerAttempts) =
              Application.cutApplicationRecoveryForDrain
                (startupApplicationState afterPeerCut)
            afterApplicationCut =
              replaceStartupApplicationState application afterPeerCut
            (peerLiveness, peerTimerAttempts) =
              PeerLiveness.cutForDrain (startupPeerLivenessState afterPeerCut)
            afterLivenessCut =
              replaceStartupPeerLivenessState peerLiveness afterApplicationCut
            (afterFailureCut, failureTimerAttempts) =
              FailureDetection.cutFailureDetectionForDrain afterLivenessCut
        case beginHeraldDrain reference afterFailureCut of
          Nothing -> transitionFault HeraldPhaseTransitionContradiction
          Just successor ->
            checkedSuccess
              successor
              ( orderedEffectBatch
                  ( fmap
                      CancelTimer
                      ( applicationTimerAttempts
                          <> peerTimerAttempts
                          <> failureTimerAttempts
                      )
                      <> [BeginDrain (Administration.drainRequestRefDrainId reference)]
                  )
              )

dispatchPeerIngress ::
  PeerIngress ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchPeerIngress ingress state = case ingress of
  PeerCandidateOpened opened ->
    observePeerCandidateOpened opened state
  PeerHelloReceived candidate localAddresses hello generation members locator ->
    applyPeerHello candidate localAddresses hello generation members locator state
      >>= observeAcceptedVoterBinding
  PeerControlReceived binding control ->
    applyPeerControlAndSettle binding control state
  PeerControlReceivedWithProgress binding progress control -> do
    (successor, effects, _) <-
      withPeerDeliveryProgress
        binding
        progress
        (applyPeerControlAndSettleWithDisposition binding control)
        state
    Right (successor, effects)
  PeerPublicationReceivedWithProgress binding progress item -> do
    (successor, effects, _) <-
      withPeerDeliveryProgress
        binding
        progress
        ( \prepared -> do
            (successor, effects) <- applyPeerPublicationInput binding (peerLogicalSequencedItem item) prepared
            Right (successor, effects, PeerControlServingWorkRequired)
        )
        state
    Right (successor, effects)
  PeerPublicationReceived binding item ->
    applyPeerPublicationInput binding (peerLogicalSequencedItem item) state
  PeerDispatchSelected ticket binding ->
    applyPeerDispatchSelection ticket binding state

-- Header and payload share one owner transition after complete wire admission.
-- A valid receipt is an independent fact even when semantic payload admission
-- closes the peer. Preserve the semantic successor together with its effects:
-- a close can also concern older pending evidence released by this input.
withPeerDeliveryProgress ::
  PeerBinding ->
  ReceiptRetirement ->
  (HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch, PeerControlServingDisposition)) ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch, PeerControlServingDisposition)
withPeerDeliveryProgress binding progress action predecessor
  | not (bindingIsCurrent binding predecessor) = action predecessor
  | otherwise = case PeerDelivery.mergeProgress binding progress predecessor of
      Left _ -> Right (predecessor, singletonEffectBatch (RejectPeerConnection binding ClosePeerControlProtocol), PeerControlGenerationAcknowledgementHandled)
      Right prepared -> action prepared

applyPeerControlAndSettle ::
  PeerBinding ->
  PeerControl ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyPeerControlAndSettle binding control predecessor = do
  (successor, effects, _) <-
    applyPeerControlAndSettleWithDisposition binding control predecessor
  Right (successor, effects)

applyPeerControlAndSettleWithDisposition ::
  PeerBinding ->
  PeerControl ->
  HeraldState ->
  Either
    HeraldInvariantFault
    (HeraldState, EffectBatch, PeerControlServingDisposition)
applyPeerControlAndSettleWithDisposition binding control predecessor = do
  case control of
    PeerDirectFailureProbeRequested request
      | bindingIsCurrent binding predecessor -> do
          (successor, effects) <-
            FailureDetection.applyDirectFailureProbeRequest
              binding
              request
              predecessor
          Right (successor, effects, PeerControlServingWorkRequired)
      | otherwise -> unchanged
    PeerDirectFailureProbeResponded response
      | bindingIsCurrent binding predecessor -> do
          (successor, effects) <-
            FailureDetection.applyDirectFailureProbeResponse
              binding
              response
              predecessor
          Right (successor, effects, PeerControlServingWorkRequired)
      | otherwise -> unchanged
    _ -> do
      (afterControl, controlEffects, installedCuts, servingDisposition) <-
        applyPeerControlWithInstalledCuts binding control predecessor
      case installedCuts of
        [] -> Right (afterControl, controlEffects, servingDisposition)
        _ -> do
          (successor, settlementEffects) <-
            case StructuralCoordinator.settleInstalledStructuralCuts installedCuts afterControl of
              Left _ -> transitionFault HeraldPeerTransitionContradiction
              Right settled -> Right settled
          Right
            ( successor,
              controlEffects <> settlementEffects,
              servingDisposition
            )
  where
    unchanged =
      Right
        ( predecessor,
          emptyEffectBatch,
          PeerControlServingWorkRequired
        )

bindingIsCurrent :: PeerBinding -> HeraldState -> Bool
bindingIsCurrent binding state =
  Discovery.currentPeerBinding
    (peerBindingRemoteHeraldEpoch binding)
    (startupDiscoveryState state)
    == Just binding

dispatchServingObservation ::
  RuntimeObservation ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchServingObservation observation state = case observation of
  OracleHealthRoundObserved roundNumber replies ->
    case OracleProjection.oracleViewVoterConfiguration view of
      Nothing -> checkedSuccess state emptyEffectBatch
      Just configuration ->
        let known = Set.fromList (map raftVoterBindingNode (Voter.voterConfigurationBindings configuration) <> map Voter.replicaRegistrationNode (OracleProjection.oracleViewOracleReplicas view))
         in case Isolation.observeOracleHealthRound (startupLastObservedTime state) roundNumber (Voter.voterConfigurationNativeRef configuration) (Voter.voterConfigurationNative configuration) known replies (startupIsolationState state) of
              Nothing -> checkedSuccess state emptyEffectBatch
              Just (isolation, disposition) ->
                let client = startupOracleClientState state
                    rounds = [RunOracleHealthRound next cadence (OracleClient.oracleClientHelloClaims client) (NonEmpty.toList (oracleContacts (OracleClient.oracleClientContacts client))) | (next, cadence) <- maybe [] pure (Isolation.oracleHealthRound isolation)]
                 in checkedSuccess (replaceStartupIsolationState isolation state) (isolationReachabilityEffects disposition <> orderedEffectBatch rounds)
    where
      view = OracleProjection.oracleView (startupOracleProjectionState state)
  ApplicationCandidateLost _ -> checkedSuccess state emptyEffectBatch
  ApplicationBindingLost binding ->
    ApplicationLiveness.applyApplicationBindingLoss
      (startupLastObservedTime state)
      binding
      state
  AdministrationBindingLost binding ->
    checkedSuccess
      ( replaceStartupAdministrationState
          ( Administration.observeAdministrationBindingLoss
              binding
              (startupAdministrationState state)
          )
          state
      )
      emptyEffectBatch
  DrainBarrierObserved _ -> checkedSuccess state emptyEffectBatch
  PeerBindingLost binding -> do
    (successor, effects, _) <- applyPeerBindingLoss binding state
    checkedSuccess successor effects
  RetiredLocalHeraldEpochRejected reporter generation ->
    case Isolation.prepareRetiredEpochRejection
      (startupLastObservedTime state)
      reporter
      generation
      (startupIsolationState state) of
      Nothing -> checkedSuccess state emptyEffectBatch
      Just prepared -> commitIsolationFence prepared state
  PeerDispatchObserved attempt outcome ->
    applyPeerDispatchObservation True attempt outcome state
  TimerObserved attempt outcome -> applyTimerObservation attempt outcome state

applyTimerObservation ::
  TimerAttempt ->
  TimerOutcome ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyTimerObservation attempt outcome state
  | Just result <- PeerDelivery.observeTimer attempt outcome state = Right result
  | otherwise = case Isolation.observeDrainTimer attempt outcome (startupIsolationState state) of
      (isolation, Isolation.IsolationDrainTimerRearmed successor specification) ->
        checkedSuccess
          (replaceStartupIsolationState isolation state)
          (singletonEffectBatch (ArmTimer successor specification))
      (isolation, Isolation.IsolationDrainTimerExpired drain) ->
        let withIsolation = replaceStartupIsolationState isolation state
         in do
              (successor, retirementEffects) <-
                retireIsolationApplicationOwners withIsolation
              checkedSuccess
                successor
                (orderedEffectBatch (retirementEffects <> [FinishIsolation drain]))
      (_, Isolation.IsolationDrainTimerStale) ->
        observeGrace
  where
    observeGrace = case Isolation.observeGraceTimer attempt outcome (startupIsolationState state) of
      (isolation, Isolation.IsolationGraceTimerRearmed successor specification) ->
        checkedSuccess
          (replaceStartupIsolationState isolation state)
          (singletonEffectBatch (ArmTimer successor specification))
      (_, Isolation.IsolationGraceTimerSelfFenceReady prepared) ->
        commitIsolationFence prepared state
      (_, Isolation.IsolationGraceTimerStale) -> do
        (afterFailureProbe, failureProbeEffects) <-
          FailureDetection.observeFailureProbeTimer attempt outcome state
        (afterApplication, applicationEffects) <-
          ApplicationLiveness.applyApplicationRecoveryTimer
            attempt
            outcome
            afterFailureProbe
        let (peerLiveness, disposition) =
              PeerLiveness.observeTimer
                attempt
                outcome
                (startupPeerLivenessState afterApplication)
            peerEffects = case disposition of
              PeerLiveness.TimerObservationStale -> emptyEffectBatch
              PeerLiveness.TimerObservationRearmed successor spec ->
                singletonEffectBatch (ArmTimer successor spec)
              PeerLiveness.TimerObservationExpired -> emptyEffectBatch
            withPeerLiveness =
              replaceStartupPeerLivenessState peerLiveness afterApplication
        (successor, suspicionEffects) <-
          case (disposition, timerAttemptPeerRecovery attempt) of
            (PeerLiveness.TimerObservationExpired, Just (target, recovery, _)) ->
              FailureDetection.observePeerRecoveryExpired
                target
                recovery
                withPeerLiveness
            _ -> checkedSuccess withPeerLiveness emptyEffectBatch
        checkedSuccess
          successor
          ( failureProbeEffects
              <> applicationEffects
              <> peerEffects
              <> suspicionEffects
          )

observeAcceptedVoterBinding ::
  (HeraldState, EffectBatch) ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
observeAcceptedVoterBinding (state, effects) =
  case [binding | SetPeerCandidateDisposition _ (PeerHelloAccepted _ binding) <- effectBatchMembers effects] of
    [] -> checkedSuccess state effects
    bindings -> do
      (successor, failureEffects) <-
        foldM
          ( \(current, accumulated) binding -> do
              (afterBinding, emitted) <- FailureDetection.observeFailureProbeBindingAccepted binding current
              Right (afterBinding, accumulated <> emitted)
          )
          (state, emptyEffectBatch)
          bindings
      checkedSuccess successor (effects <> failureEffects)

isolationReachabilityEffects :: Isolation.IsolationReachabilityDisposition -> EffectBatch
isolationReachabilityEffects disposition = case disposition of
  Isolation.IsolationReachabilityUnchanged -> emptyEffectBatch
  Isolation.IsolationGraceCancelled attempt -> singletonEffectBatch (CancelTimer attempt)
  Isolation.IsolationGraceStarted attempt specification ->
    singletonEffectBatch (ArmTimer attempt specification)

commitIsolationFence ::
  Isolation.PreparedSelfFence ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
commitIsolationFence prepared state = do
  let (application, notices) =
        Application.beginApplicationIsolationDrain
          (startupApplicationState state)
      requirement =
        if null notices
          then Isolation.IsolationDrainNotRequired
          else Isolation.IsolationDrainRequired
      residents =
        Set.fromList
          (fmap witnessedProcessEpoch (Application.applicationProcessWitnesses application))
      (successorIsolation, disposition) =
        Isolation.commitSelfFence residents requirement prepared
      unavailableReason =
        isolationUnavailableReason
          (replaceStartupIsolationState successorIsolation state)
      noticeEffects =
        [ SendApplicationIsolationBegun
            (Application.applicationIsolationNoticeBinding notice)
            (Application.applicationIsolationNoticeCursor notice)
            (Application.applicationIsolationNoticeSession notice)
            unavailableReason
        | notice <- notices
        ]
      withFence =
        freezeStartupSemanticControl
          ( replaceStartupApplicationState
              application
              (replaceStartupIsolationState successorIsolation state)
          )
  (sealed, preparationEffects) <- advancePreparationWork withFence
  case disposition of
    Isolation.IsolationReadOnlyDrainStarted _ attempt specification cancellation _ ->
      checkedSuccess
        sealed
        ( preparationEffects
            <> orderedEffectBatch
              ( maybe [] (pure . CancelTimer) cancellation
                  <> noticeEffects
                  <> [ArmTimer attempt specification]
              )
        )
    Isolation.IsolationTerminatedWithoutDrain drain cancellation _ -> do
      (terminal, retirementEffects) <- retireIsolationApplicationOwners sealed
      checkedSuccess
        terminal
        ( preparationEffects
            <> orderedEffectBatch
              ( maybe [] (pure . CancelTimer) cancellation
                  <> retirementEffects
                  <> [FinishIsolation drain]
              )
        )

-- | Retire the application and wait owners retained across the losing branch.
-- Oracle projection and topology retirement may already have completed; the
-- isolation owner alone decides when the application-visible lane reaches its
-- irreversible terminal point.
retireIsolationApplicationOwners ::
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
retireIsolationApplicationOwners state = do
  (retired, effects) <-
    foldM
      retireOne
      (state, [])
      ( Set.toAscList
          ( Isolation.isolationWitnessResidentProcessOverlay
              (Isolation.stateWitness (startupIsolationState state))
          )
      )
  reclaimed <- ProcessPreparation.retireClosedLifecycleReceipts retired
  Right (reclaimed, effects)
  where
    reason = isolationUnavailableReason state

    retireOne (current, retainedEffects) process =
      case OracleProjection.oracleViewEndedProcess
        process
        (OracleProjection.oracleView (startupOracleProjectionState current)) of
        Just ended -> do
          (successor, effects) <-
            applyResidentEndWithUnavailableReason
              reason
              process
              (OracleProjection.projectedEndedProcessControlIndex ended)
              (OracleProjection.projectedEndedProcessReason ended)
              current
          Right (successor, retainedEffects <> effects)
        Nothing -> retireApplicationOnly current retainedEffects process

    retireApplicationOnly current retainedEffects process = do
      let preparedApplication =
            Application.prepareApplicationProcessRetirement
              process
              (startupApplicationState current)
          (application, retirement) =
            Application.commitApplicationProcessRetirement preparedApplication
      preparedWait <-
        either
          (const (transitionFault HeraldApplicationTransitionContradiction))
          Right
          ( Wait.prepareProcessWaitRemoval
              process
              (startupWaitState current)
          )
      let (waits, removedWaits) = Wait.commitWaitRemoval preparedWait
          expectedWaits =
            sort (Application.applicationProcessRetirementWaitIds retirement)
          actualWaits = sort (fmap Wait.waitRegistrationId removedWaits)
      unless (expectedWaits == actualWaits)
        $ transitionFault HeraldApplicationTransitionContradiction
      let effects =
            fmap
              CancelTimer
              (Application.applicationProcessRetirementTimerAttempts retirement)
              <> [ DisposeApplicationSession session reason
                 | session <- Application.applicationProcessRetirementSessionIds retirement
                 ]
          successor =
            replaceStartupWaitState waits
              . replaceStartupApplicationState application
              $ current
      Right (successor, retainedEffects <> effects)

isolationUnavailableReason ::
  HeraldState -> ApplicationSessionUnavailableReason
isolationUnavailableReason state =
  case Isolation.isolationWitnessSelfFenceReason
    (Isolation.stateWitness (startupIsolationState state)) of
    Just Isolation.IsolationRetirementLearned {} -> ApplicationHeraldRetired
    Just Isolation.IsolationRetiredEpochRejected {} -> ApplicationHeraldRetired
    _ -> ApplicationHeraldIsolated

liveApplicationSessionIds :: HeraldState -> [ApplicationSessionId]
liveApplicationSessionIds state =
  [ Application.applicationSessionWitnessId session
  | session <-
      Application.applicationWitnessSessions
        (Application.applicationStateWitness (startupApplicationState state)),
    Application.applicationSessionWitnessDeliveryLive session
  ]

applyPeerPublicationInput ::
  PeerBinding ->
  SequencedItem PeerLogicalPayload ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyPeerPublicationInput binding item predecessor =
  case PeerInput.applyPeerLogicalItem context binding item owners of
    Left problem -> peerInputFailure binding problem predecessor
    Right (successorOwners, result) -> do
      let rejectedPeers =
            Set.fromList
              [ streamDirectionSource
                  (PeerInput.deferredPeerRejectionDirection rejection)
              | rejection <- PeerInput.peerInputDeferredRejections result
              ]
          rejectionEffectMembers =
            [ RejectPeerConnection rejectedBinding ClosePeerPublicationProtocol
            | rejectedPeer <- Set.toAscList rejectedPeers,
              Just rejectedBinding <-
                [ Discovery.currentPeerBinding
                    rejectedPeer
                    (startupDiscoveryState successor)
                ]
            ]
          placementEffectMembers =
            [ SendPeerControl
                peer
                (PeerPlacementUpdate (FullPlacementSnapshot snapshot))
            | snapshot <- PeerInput.peerInputPlacementSnapshots result,
              peer <- Discovery.currentPeerBindings (startupDiscoveryState successor),
              peerBindingRemoteHeraldEpoch peer `Set.notMember` rejectedPeers
            ]
          acknowledgementEffectMembers =
            concatMap
              (acknowledgementEffects rejectedPeers)
              (PeerInput.peerInputAcknowledgements result)
      (afterTerminalReadiness, readinessEffects, readinessCuts) <-
        redriveTerminalSourceReadinessIfProgressed predecessor successor
      (afterReadinessSettlement, readinessSettlementEffects) <-
        case StructuralCoordinator.settleInstalledStructuralCuts
          readinessCuts
          afterTerminalReadiness of
          Left _ -> transitionFault HeraldPeerTransitionContradiction
          Right settled -> Right settled
      (structuralSuccessor, structuralEffects) <-
        case StructuralCoordinator.advanceLocalStructuralWork afterReadinessSettlement of
          Left _ -> transitionFault HeraldPeerTransitionContradiction
          Right advanced -> Right advanced
      checkedSuccess
        structuralSuccessor
        ( orderedEffectBatch
            ( rejectionEffectMembers
                <> placementEffectMembers
                <> acknowledgementEffectMembers
                <> fmap wakeEffect (PeerInput.peerInputWakes result)
            )
            <> readinessEffects
            <> readinessSettlementEffects
            <> structuralEffects
        )
      where
        successorPeerStream = PeerInput.peerInputPeerStreamState successorOwners
        successor =
          replaceStartupWaitState (PeerInput.peerInputWaitState successorOwners)
            . replaceStartupApplicationState
              (PeerInput.peerInputApplicationState successorOwners)
            . replaceStartupSortRegistryState
              (PeerInput.peerInputSortRegistryState successorOwners)
            . replaceStartupControlledState
              (PeerInput.peerInputControlledState successorOwners)
            . replaceStartupPlacementState
              (PeerInput.peerInputPlacementState successorOwners)
            . replaceStartupAlignmentState
              (PeerInput.peerInputAlignmentState successorOwners)
            . replaceStartupStructuralProgressState
              (PeerInput.peerInputStructuralProgressState successorOwners)
            . replaceStartupIdGeneratorState
              (PeerInput.peerInputIdGeneratorState successorOwners)
            . replaceStartupGraphState (PeerInput.peerInputGraphState successorOwners)
            . replaceStartupStoreState (PeerInput.peerInputStoreState successorOwners)
            . replaceStartupPeerStreamState successorPeerStream
            . replaceStartupPublicationState
              (PeerInput.peerInputPublicationState successorOwners)
            . replaceStartupDisappearanceState (PeerInput.peerInputDisappearanceState successorOwners)
            . replaceStartupLabelBarrierState
              (PeerInput.peerInputLabelBarrierState successorOwners)
            $ predecessor
  where
    context =
      PeerInput.peerInputContext
        (startupGenesis predecessor)
        (startupOracleProjectionState predecessor)
        (startupDiscoveryState predecessor)
        (startupPlacementState predecessor)
    owners =
      PeerInput.peerInputState
        (startupPublicationState predecessor)
        (startupPeerStreamState predecessor)
        (startupStoreState predecessor)
        (startupGraphState predecessor)
        (startupIdGeneratorState predecessor)
        (startupStructuralProgressState predecessor)
        (startupAlignmentState predecessor)
        (startupPlacementState predecessor)
        (startupControlledState predecessor)
        (startupSortRegistryState predecessor)
        (startupApplicationState predecessor)
        (startupWaitState predecessor)
        (startupLabelBarrierState predecessor)
        (startupDisappearanceState predecessor)

    acknowledgementEffects rejectedPeers acknowledgement =
      case acknowledgementBinding of
        Nothing -> []
        Just acknowledgementPeer ->
          ( [ SendPeerControl acknowledgementPeer (PeerStreamCompleted direction (PeerInput.peerAcknowledgementCompletion acknowledgement))
            ]
              <> maybe
                []
                ( \offer ->
                    [ SendPeerControl
                        acknowledgementPeer
                        (PeerStreamResumeOffered offer)
                    ]
                )
                (PeerInput.peerAcknowledgementRepairOffer acknowledgement)
          )
      where
        direction = PeerInput.peerAcknowledgementDirection acknowledgement
        peer = streamDirectionSource direction
        acknowledgementBinding =
          if peer `Set.member` rejectedPeers
            then Nothing
            else
              Discovery.currentPeerBinding
                peer
                (startupDiscoveryState predecessor)

    wakeEffect wake =
      SendApplicationWaitWake
        (Application.applicationWaitWakeBinding wake)
        (Application.applicationWaitWakeCursor wake)
        (Application.applicationWaitWakeRequestId wake)
        (Application.applicationWaitWakeWaitId wake)
        (Application.applicationWaitWakeResult wake)

peerInputFailure ::
  PeerBinding ->
  PeerInput.PeerInputProblem ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
peerInputFailure binding problem predecessor = case problem of
  _ -> case PeerInput.peerInputProblemDisposition problem of
    PeerInput.RejectCurrentPeerBinding -> closePeer
    PeerInput.PeerInputInvariantFault -> contradiction
  where
    closePeer =
      checkedSuccess
        predecessor
        (singletonEffectBatch (RejectPeerConnection binding ClosePeerPublicationProtocol))
    contradiction = transitionFault HeraldPeerTransitionContradiction

dispatchDraining ::
  HeraldInputBody ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchDraining body state = case body of
  ApplicationReceiptRetirementInput _ -> checkedSuccess state emptyEffectBatch
  JoinInput correlation bytes -> Join.applyJoinRequest correlation bytes state
  ApplicationSessionInput _ -> checkedSuccess state emptyEffectBatch
  ApplicationLifecycleInput _ -> checkedSuccess state emptyEffectBatch
  ApplicationRequestInput _ -> checkedSuccess state emptyEffectBatch
  AdministrationInput _ -> checkedSuccess state emptyEffectBatch
  DisappearanceInput _ -> checkedSuccess state emptyEffectBatch
  PeerInput _ -> checkedSuccess state emptyEffectBatch
  OracleInput _ -> checkedSuccess state emptyEffectBatch
  RuntimeObserved observation -> dispatchDrainingObservation observation state

dispatchDrainingObservation ::
  RuntimeObservation ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
dispatchDrainingObservation observation state = case observation of
  OracleHealthRoundObserved {} -> checkedSuccess state emptyEffectBatch
  ApplicationCandidateLost _ -> checkedSuccess state emptyEffectBatch
  ApplicationBindingLost _ -> checkedSuccess state emptyEffectBatch
  AdministrationBindingLost binding ->
    checkedSuccess
      ( replaceStartupAdministrationState
          ( Administration.observeAdministrationBindingLoss
              binding
              (startupAdministrationState state)
          )
          state
      )
      emptyEffectBatch
  DrainBarrierObserved observedDrain ->
    case startupDrainWitness state of
      Nothing -> transitionFault HeraldPhaseTransitionContradiction
      Just witness
        | observedDrain /= heraldDrainWitnessId witness ->
            checkedSuccess state emptyEffectBatch
        | otherwise -> completeDrain witness state
  PeerBindingLost _ -> checkedSuccess state emptyEffectBatch
  RetiredLocalHeraldEpochRejected _ _ -> checkedSuccess state emptyEffectBatch
  PeerDispatchObserved attempt outcome ->
    applyDrainingPeerDispatchObservation attempt outcome state
  TimerObserved _ _ -> checkedSuccess state emptyEffectBatch

completeDrain ::
  HeraldDrainWitness ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
completeDrain witness state =
  case Administration.prepareDrainCompletion
    (heraldDrainWitnessRequest witness)
    (startupAdministrationState state) of
    Left _ -> transitionFault HeraldAdministrationTransitionContradiction
    Right prepared ->
      let (successorAdministration, reply) =
            Administration.commitDrainCompletion prepared
          withAdministration =
            replaceStartupAdministrationState successorAdministration state
       in case finishHeraldDrain withAdministration of
            Nothing -> transitionFault HeraldPhaseTransitionContradiction
            Just successor ->
              checkedSuccess
                successor
                ( singletonEffectBatch
                    (FinishDrain (heraldDrainWitnessId witness) reply)
                )

checkedSuccess ::
  HeraldState ->
  EffectBatch ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
checkedSuccess state effects = Right (state, effects)

transitionFault ::
  HeraldTransitionInvariantViolation ->
  Either HeraldInvariantFault value
transitionFault = Left . HeraldTransitionInvariant
