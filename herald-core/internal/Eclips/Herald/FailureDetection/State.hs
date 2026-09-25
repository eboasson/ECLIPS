{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure owner of live Herald failure-probe attempts and intentions.
--
-- Peer recovery establishes suspicion; this owner turns a voter host's one
-- expired recovery episode into an Oracle Open intention, and only a projected
-- Open can allocate a fresh direct probe.  Every direct attempt has one fixed
-- absolute deadline.  Early runtime observations replace the physical timer
-- attempt without extending that deadline.
module Eclips.Herald.FailureDetection.State
  ( State,
    initialState,
    initialStateWithProbeDuration,
    freshJoiningReplayContext,
    observeVoterConfiguration,
    FailureDetectionAction (..),
    FailureDetectionInvariantViolation (..),
    FailureDetectionStateWitness (..),
    FailureProbeWitness (..),
    FailureProbePhaseWitness (..),
    stateWitness,
    failureProbeWitness,
    validateState,
    observeRecoveryExpired,
    observeProbeOpened,
    observeBindingAccepted,
    observeDirectProbeRequest,
    observeDirectProbeResponse,
    observeProbeTimer,
    observeProjectedReports,
    observeProbeTerminal,
    cutForDrain,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Domain.Membership
  ( FailureProbeResolution (..),
    FailureProbeResolutionId,
    HeraldFailureProbeId,
    HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    deriveFailureProbeResolutionId,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationId,
  )
import Eclips.Herald.Discovery
  ( PeerBinding,
    peerBindingRemoteHeraldEpoch,
  )
import Eclips.Herald.Peer.Step15
  ( DirectFailureProbeRequest,
    DirectFailureProbeResponse,
    DirectFailureProbeResult (..),
    directFailureProbeRequest,
    directFailureProbeRequestGeneration,
    directFailureProbeRequestProbe,
    directFailureProbeRequestTarget,
    directFailureProbeRequestVoterConfiguration,
    directFailureProbeResponse,
    directFailureProbeResponseRequest,
    directFailureProbeResponseResult,
  )
import Eclips.Herald.PeerLiveness.Internal
  ( PeerRecoveryConfiguration,
    PeerRecoveryGeneration,
    peerRecoveryGenerationWord64,
    peerRecoveryGraceMicroseconds,
  )
import Eclips.Herald.Time
  ( MonotonicInstant,
    monotonicInstant,
    monotonicInstantWord64,
  )
import Eclips.Herald.Timer.Internal
  ( TimerAttempt,
    TimerAttemptGeneration,
    TimerOutcome (..),
    TimerSpec,
    absoluteTimerSpec,
    failureProbeTimerAttempt,
    firstTimerAttemptGeneration,
    nextTimerAttemptGeneration,
    timerAttemptFailureProbe,
    timerAttemptGenerationWord64,
  )
import Eclips.Oracle.Command (ProbeResult (..))
import Eclips.Oracle.Voter (VoterConfigurationId)

data OpenKey
  = OpenKey HeraldEpoch HeraldMembershipGenerationId PeerRecoveryGeneration VoterConfigurationId
  deriving stock (Eq, Ord, Show)

data ActiveAttempt = ActiveAttempt
  { request :: DirectFailureProbeRequest,
    deadline :: MonotonicInstant,
    timerGeneration :: TimerAttemptGeneration,
    offeredBindings :: Set PeerBinding
  }
  deriving stock (Eq, Show)

data ProbePhase
  = ProbeAwaitingResponse ActiveAttempt
  | ProbeReportRetained ProbeResult
  | ProbeTerminal
  deriving stock (Eq, Show)

data LocalProbe = LocalProbe
  { target :: HeraldEpoch,
    generation :: HeraldMembershipGenerationId,
    capturedConfiguration :: VoterConfigurationId,
    capturedReporters :: Set HeraldEpoch,
    phase :: ProbePhase
  }
  deriving stock (Eq, Show)

data State = State
  { local :: HeraldEpoch,
    voterHosts :: Set HeraldEpoch,
    jointVoterHosts :: Maybe (Set HeraldEpoch, Set HeraldEpoch),
    voterConfiguration :: Maybe VoterConfigurationId,
    voterChangePending :: Bool,
    directProbeDurationMicroseconds :: Word64,
    openIntentions :: Set OpenKey,
    consumedOpenIntentions :: Set OpenKey,
    probes :: Map HeraldFailureProbeId LocalProbe,
    terminalIntentions :: Map HeraldFailureProbeId FailureProbeResolution
  }
  deriving stock (Eq, Show)

initialState ::
  HeraldEpoch ->
  Set HeraldEpoch ->
  Maybe VoterConfigurationId ->
  PeerRecoveryConfiguration ->
  State
initialState local voterHosts voterConfiguration recoveryConfiguration =
  initialStateWithProbeDuration (peerRecoveryGraceMicroseconds recoveryConfiguration) local voterHosts voterConfiguration

-- | The fresh probe is distinct from the reconnect grace. Its positive
-- duration is selected by checked startup configuration.
initialStateWithProbeDuration :: Word64 -> HeraldEpoch -> Set HeraldEpoch -> Maybe VoterConfigurationId -> State
initialStateWithProbeDuration duration local voterHosts voterConfiguration =
  State
    { local,
      voterHosts,
      jointVoterHosts = Nothing,
      voterConfiguration,
      voterChangePending = False,
      directProbeDurationMicroseconds = duration,
      openIntentions = Set.empty,
      consumedOpenIntentions = Set.empty,
      probes = Map.empty,
      terminalIntentions = Map.empty
    }

-- | Disposable private replay context retaining only identity and probe policy.
-- The live owner's attempts, timers and intentions remain independently owned.
freshJoiningReplayContext :: Set HeraldEpoch -> Maybe VoterConfigurationId -> State -> State
freshJoiningReplayContext voters configuration state =
  initialStateWithProbeDuration state.directProbeDurationMicroseconds state.local voters configuration

data FailureDetectionAction
  = RetainOpenFailureProbe
      HeraldEpoch
      HeraldMembershipGenerationId
      PeerRecoveryGeneration
      VoterConfigurationId
  | ArmDirectFailureProbeTimer TimerAttempt TimerSpec
  | CancelDirectFailureProbeTimer TimerAttempt
  | SendDirectFailureProbe PeerBinding DirectFailureProbeRequest
  | SendDirectFailureProbeResponse PeerBinding DirectFailureProbeResponse
  | RetainFailureProbeReport HeraldFailureProbeId VoterConfigurationId ProbeResult
  | RetainFailureProbeDismissal FailureProbeResolutionId
  | RetainVoterHostFailureAcceptance FailureProbeResolutionId
  | RetainHeraldRetirement FailureProbeResolutionId HeraldEpoch
  deriving stock (Eq, Show)

data FailureDetectionInvariantViolation
  = FailureDetectionVoterSetEmpty
  | FailureDetectionJointVoterHostsMismatch
  | FailureDetectionWorkDuringVoterChange
  | FailureDetectionOpenIntentionNotConsumed
  | FailureDetectionDurationZero
  | FailureDetectionLocalProbeOnNonVoter HeraldEpoch
  | FailureDetectionOpenIntentionOnNonVoter HeraldEpoch
  | FailureDetectionOpenIntentionTargetsVoter HeraldEpoch
  | FailureDetectionOpenRecoveryGenerationZero HeraldEpoch
  | FailureDetectionProbeKeyMismatch HeraldFailureProbeId
  | FailureDetectionProbeTargetsVoter HeraldFailureProbeId HeraldEpoch
  | FailureDetectionProbeGenerationMismatch HeraldFailureProbeId
  | FailureDetectionProbeConfigurationMismatch HeraldFailureProbeId
  | FailureDetectionProbeReportersEmpty HeraldFailureProbeId
  | FailureDetectionTimerGenerationZero HeraldFailureProbeId
  | FailureDetectionOfferedBindingTargetMismatch HeraldFailureProbeId PeerBinding
  | FailureDetectionTerminalIntentionWithoutProbe HeraldFailureProbeId
  deriving stock (Eq, Show)

data FailureProbePhaseWitness
  = FailureProbeAwaitingWitness
      DirectFailureProbeRequest
      MonotonicInstant
      TimerAttempt
      [PeerBinding]
  | FailureProbeReportedWitness ProbeResult
  | FailureProbeTerminalWitness
  deriving stock (Eq, Show)

data FailureProbeWitness = FailureProbeWitness
  { witnessProbe :: HeraldFailureProbeId,
    witnessTarget :: HeraldEpoch,
    witnessGeneration :: HeraldMembershipGenerationId,
    witnessConfiguration :: VoterConfigurationId,
    witnessReporters :: Set HeraldEpoch,
    witnessPhase :: FailureProbePhaseWitness
  }
  deriving stock (Eq, Show)

data FailureDetectionStateWitness = FailureDetectionStateWitness
  { witnessLocal :: HeraldEpoch,
    witnessVoterHosts :: Set HeraldEpoch,
    witnessVoterQuorums :: (Set HeraldEpoch, Maybe (Set HeraldEpoch)),
    witnessVoterChangePending :: Bool,
    witnessVoterConfiguration :: Maybe VoterConfigurationId,
    witnessDirectProbeDurationMicroseconds :: Word64,
    witnessOpenIntentions ::
      [(HeraldEpoch, HeraldMembershipGenerationId, PeerRecoveryGeneration, VoterConfigurationId)],
    witnessProbes :: [FailureProbeWitness],
    witnessTerminalIntentions ::
      [(HeraldFailureProbeId, FailureProbeResolution)]
  }
  deriving stock (Eq, Show)

stateWitness :: State -> FailureDetectionStateWitness
stateWitness state =
  FailureDetectionStateWitness
    { witnessLocal = state.local,
      witnessVoterHosts = state.voterHosts,
      witnessVoterQuorums =
        maybe
          (state.voterHosts, Nothing)
          (\(oldHosts, newHosts) -> (oldHosts, Just newHosts))
          state.jointVoterHosts,
      witnessVoterChangePending = state.voterChangePending,
      witnessVoterConfiguration = state.voterConfiguration,
      witnessDirectProbeDurationMicroseconds =
        state.directProbeDurationMicroseconds,
      witnessOpenIntentions =
        [ (target, generation, recovery, configuration)
        | OpenKey target generation recovery configuration <- Set.toAscList state.openIntentions
        ],
      witnessProbes =
        [ probeWitness identifier probe
        | (identifier, probe) <- Map.toAscList state.probes
        ],
      witnessTerminalIntentions = Map.toAscList state.terminalIntentions
    }

failureProbeWitness ::
  HeraldFailureProbeId ->
  State ->
  Maybe FailureProbeWitness
failureProbeWitness identifier state =
  probeWitness identifier <$> Map.lookup identifier state.probes

probeWitness :: HeraldFailureProbeId -> LocalProbe -> FailureProbeWitness
probeWitness identifier probe =
  FailureProbeWitness
    { witnessProbe = identifier,
      witnessTarget = probe.target,
      witnessGeneration = probe.generation,
      witnessConfiguration = probe.capturedConfiguration,
      witnessReporters = probe.capturedReporters,
      witnessPhase = case probe.phase of
        ProbeAwaitingResponse attempt ->
          FailureProbeAwaitingWitness
            attempt.request
            attempt.deadline
            ( failureProbeTimerAttempt
                identifier
                attempt.timerGeneration
            )
            (Set.toAscList attempt.offeredBindings)
        ProbeReportRetained result -> FailureProbeReportedWitness result
        ProbeTerminal -> FailureProbeTerminalWitness
    }

validateState :: State -> Either FailureDetectionInvariantViolation ()
validateState state = do
  if Set.null state.voterHosts
    then Left FailureDetectionVoterSetEmpty
    else Right ()
  case state.jointVoterHosts of
    Nothing -> Right ()
    Just (oldHosts, newHosts)
      | not (Set.null oldHosts),
        not (Set.null newHosts),
        Set.union oldHosts newHosts == state.voterHosts ->
          Right ()
      | otherwise -> Left FailureDetectionJointVoterHostsMismatch
  if state.directProbeDurationMicroseconds == 0
    then Left FailureDetectionDurationZero
    else Right ()
  if any probeActive (Map.elems state.probes) && Set.notMember state.local state.voterHosts
    then Left (FailureDetectionLocalProbeOnNonVoter state.local)
    else Right ()
  if not (Set.null state.openIntentions) && Set.notMember state.local state.voterHosts
    then Left (FailureDetectionOpenIntentionOnNonVoter state.local)
    else Right ()
  if probesSuspended state
    && ( any probeActive (Map.elems state.probes)
           || not (Set.null state.openIntentions)
           || not (Map.null state.terminalIntentions)
       )
    then Left FailureDetectionWorkDuringVoterChange
    else Right ()
  if state.openIntentions `Set.isSubsetOf` state.consumedOpenIntentions
    then Right ()
    else Left FailureDetectionOpenIntentionNotConsumed
  mapM_ validateOpen (Set.toAscList state.openIntentions)
  mapM_ validateProbe (Map.toAscList state.probes)
  mapM_ validateTerminal (Map.keys state.terminalIntentions)
  where
    validateOpen (OpenKey target _ recovery _)
      | peerRecoveryGenerationWord64 recovery == 0 =
          Left (FailureDetectionOpenRecoveryGenerationZero target)
      | otherwise = Right ()
    validateProbe (identifier, probe) = do
      if Set.null probe.capturedReporters
        then Left (FailureDetectionProbeReportersEmpty identifier)
        else Right ()
      case probe.phase of
        ProbeAwaitingResponse attempt -> do
          if directFailureProbeRequestProbe attempt.request /= identifier
            then Left (FailureDetectionProbeKeyMismatch identifier)
            else Right ()
          if directFailureProbeRequestTarget attempt.request /= probe.target
            then Left (FailureDetectionProbeKeyMismatch identifier)
            else Right ()
          if directFailureProbeRequestGeneration attempt.request /= probe.generation
            then Left (FailureDetectionProbeGenerationMismatch identifier)
            else Right ()
          if directFailureProbeRequestVoterConfiguration attempt.request /= probe.capturedConfiguration
            then Left (FailureDetectionProbeConfigurationMismatch identifier)
            else Right ()
          if timerAttemptGenerationWord64 attempt.timerGeneration == 0
            then Left (FailureDetectionTimerGenerationZero identifier)
            else Right ()
          mapM_
            ( \binding ->
                if peerBindingRemoteHeraldEpoch binding == probe.target
                  then Right ()
                  else
                    Left
                      ( FailureDetectionOfferedBindingTargetMismatch
                          identifier
                          binding
                      )
            )
            (Set.toAscList attempt.offeredBindings)
        ProbeReportRetained {} -> Right ()
        ProbeTerminal -> Right ()
    validateTerminal identifier =
      case Map.lookup identifier state.probes of
        Nothing -> Left (FailureDetectionTerminalIntentionWithoutProbe identifier)
        Just probe -> case probe.phase of
          ProbeTerminal ->
            Left (FailureDetectionTerminalIntentionWithoutProbe identifier)
          _ -> Right ()

-- | Observe committed stable/joint roles and the retained voter-change gate.
-- Configuration changes supersede local failure work instead of recounting its
-- old evidence against another denominator. Historical probe IDs and consumed
-- recovery episodes remain inert, so a later promotion or demotion cannot reuse
-- a canceled timer, report, or Open identity. Fresh work requires a fresh Open.
observeVoterConfiguration ::
  Set HeraldEpoch ->
  Maybe (Set HeraldEpoch) ->
  Maybe VoterConfigurationId ->
  Bool ->
  State ->
  Either FailureDetectionInvariantViolation (State, [FailureDetectionAction])
observeVoterConfiguration oldHosts newHosts configuration changePending state
  | Set.null oldHosts || maybe False Set.null newHosts =
      Left FailureDetectionVoterSetEmpty
  | hosts == state.voterHosts,
    joint == state.jointVoterHosts,
    configuration == state.voterConfiguration,
    changePending == state.voterChangePending =
      Right (state, [])
  | otherwise =
      let (cut, timers) = cutForDrain state
       in Right
            ( cut
                { voterHosts = hosts,
                  jointVoterHosts = joint,
                  voterConfiguration = configuration,
                  voterChangePending = changePending
                },
              fmap CancelDirectFailureProbeTimer timers
            )
  where
    hosts = maybe oldHosts (Set.union oldHosts) newHosts
    joint = fmap (oldHosts,) newHosts

probeActive :: LocalProbe -> Bool
probeActive probe = case probe.phase of
  ProbeTerminal -> False
  _ -> True

probesSuspended :: State -> Bool
probesSuspended state =
  state.voterChangePending || case state.jointVoterHosts of
    Nothing -> False
    Just _ -> True

-- | Retain exactly one stable Open intention for an expired ordinary recovery
-- episode. Only current stable voter hosts can report; pending voter changes
-- suspend probes. The exact committed configuration qualifies the Open key.
observeRecoveryExpired ::
  HeraldEpoch ->
  HeraldMembershipGeneration ->
  PeerRecoveryGeneration ->
  State ->
  (State, [FailureDetectionAction])
observeRecoveryExpired target membership recovery state
  | probesSuspended state = unchanged
  | Set.notMember state.local state.voterHosts = unchanged
  | Nothing <- state.voterConfiguration = unchanged
  | target == state.local = unchanged
  | target `notElem` NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership) = unchanged
  | any probeIsActiveForTarget (Map.elems state.probes) = unchanged
  | Just configuration <- state.voterConfiguration,
    Set.member (key configuration) state.consumedOpenIntentions =
      unchanged
  | Just configuration <- state.voterConfiguration =
      ( state
          { openIntentions = Set.insert (key configuration) state.openIntentions,
            consumedOpenIntentions = Set.insert (key configuration) state.consumedOpenIntentions
          },
        [RetainOpenFailureProbe target generation recovery configuration]
      )
  where
    generation = heraldMembershipGenerationId membership
    key configuration = OpenKey target generation recovery configuration
    probeIsActiveForTarget probe =
      probe.target == target
        && probe.generation == generation
        && case probe.phase of
          ProbeTerminal -> False
          _ -> True
    unchanged = (state, [])

-- | A canonical Open invalidates every pre-open reachability observation and
-- allocates one new bounded direct attempt at each voter host.
observeProbeOpened ::
  MonotonicInstant ->
  HeraldMembershipGeneration ->
  Maybe PeerBinding ->
  HeraldFailureProbeId ->
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  VoterConfigurationId ->
  Set HeraldEpoch ->
  State ->
  (State, [FailureDetectionAction])
observeProbeOpened observedAt membership targetBinding identifier target generation configuration reporters state
  | probesSuspended state = unchanged
  | Set.notMember state.local reporters = unchanged
  | Just configuration /= state.voterConfiguration = unchanged
  | generation /= heraldMembershipGenerationId membership = unchanged
  | target `notElem` NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership) = unchanged
  | Map.member identifier state.probes = unchanged
  | target == state.local =
      (state {probes = Map.insert identifier localProbe {phase = ProbeReportRetained ProbeReachable} state.probes}, [RetainFailureProbeReport identifier configuration ProbeReachable])
  | otherwise =
      ( state
          { probes = Map.insert identifier localProbe state.probes
          },
        ArmDirectFailureProbeTimer timer (absoluteTimerSpec deadline)
          : maybe [] (\binding -> [SendDirectFailureProbe binding request]) admittedBinding
      )
  where
    request = directFailureProbeRequest identifier target generation configuration
    deadline =
      monotonicInstant
        ( monotonicInstantWord64 observedAt
            + state.directProbeDurationMicroseconds
        )
    timerGeneration = firstTimerAttemptGeneration
    timer = failureProbeTimerAttempt identifier timerGeneration
    admittedBinding =
      targetBinding >>= \binding ->
        if peerBindingRemoteHeraldEpoch binding == target
          then Just binding
          else Nothing
    offered = maybe Set.empty Set.singleton admittedBinding
    localProbe =
      LocalProbe
        { target,
          generation,
          capturedConfiguration = configuration,
          capturedReporters = reporters,
          phase =
            ProbeAwaitingResponse
              ActiveAttempt
                { request,
                  deadline,
                  timerGeneration,
                  offeredBindings = offered
                }
        }
    unchanged = (state, [])

-- | Offer an outstanding probe once on each newly admitted logical binding.
observeBindingAccepted ::
  PeerBinding ->
  State ->
  (State, [FailureDetectionAction])
observeBindingAccepted binding state =
  (state {probes = updated}, reverse actions)
  where
    remote = peerBindingRemoteHeraldEpoch binding
    (actions, updated) =
      Map.mapAccumWithKey offer [] state.probes
    offer retained _ probe
      | probe.target /= remote = (retained, probe)
      | otherwise = case probe.phase of
          ProbeAwaitingResponse attempt
            | Set.notMember binding attempt.offeredBindings ->
                ( SendDirectFailureProbe binding attempt.request : retained,
                  probe
                    { phase =
                        ProbeAwaitingResponse
                          attempt
                            { offeredBindings =
                                Set.insert binding attempt.offeredBindings
                            }
                    }
                )
          _ -> (retained, probe)

-- | Admit a fresh request only at the named target, in the current generation,
-- from a current stable voter host, and for the exact open projection supplied by
-- the caller.
observeDirectProbeRequest ::
  PeerBinding ->
  HeraldMembershipGeneration ->
  Bool ->
  Set HeraldEpoch ->
  DirectFailureProbeRequest ->
  State ->
  (State, [FailureDetectionAction])
observeDirectProbeRequest binding membership projectedOpen capturedReporters request state
  | probesSuspended state = unchanged
  | directFailureProbeRequestTarget request /= state.local = unchanged
  | directFailureProbeRequestGeneration request
      /= heraldMembershipGenerationId membership =
      unchanged
  | Set.notMember (peerBindingRemoteHeraldEpoch binding) capturedReporters = unchanged
  | not projectedOpen = unchanged
  | otherwise =
      ( state,
        [ SendDirectFailureProbeResponse
            binding
            (directFailureProbeResponse request DirectFailureProbeReachable)
        ]
      )
  where
    unchanged = (state, [])

-- | A response can settle only the exact request offered on this logical
-- binding.  The target-authored response is necessarily Reachable; Unreachable
-- is produced solely by the reporter's own fixed deadline.
observeDirectProbeResponse ::
  PeerBinding ->
  DirectFailureProbeResponse ->
  State ->
  (State, [FailureDetectionAction])
observeDirectProbeResponse binding response state =
  case Map.lookup identifier state.probes of
    Just probe
      | ProbeAwaitingResponse attempt <- probe.phase,
        attempt.request == request,
        Set.member binding attempt.offeredBindings,
        peerBindingRemoteHeraldEpoch binding == probe.target,
        directFailureProbeResponseResult response == DirectFailureProbeReachable ->
          let timer =
                failureProbeTimerAttempt identifier attempt.timerGeneration
              successorProbe = probe {phase = ProbeReportRetained ProbeReachable}
           in ( state
                  { probes = Map.insert identifier successorProbe state.probes
                  },
                [ CancelDirectFailureProbeTimer timer,
                  RetainFailureProbeReport identifier probe.capturedConfiguration ProbeReachable
                ]
              )
    _ -> (state, [])
  where
    request = directFailureProbeResponseRequest response
    identifier = directFailureProbeRequestProbe request

-- | Observe one exact direct-probe timer.  An early observation replaces the
-- physical attempt against the unchanged deadline; expiry retains one
-- Unreachable report and leaves no live timer to spin.
observeProbeTimer ::
  TimerAttempt ->
  TimerOutcome ->
  State ->
  (State, [FailureDetectionAction])
observeProbeTimer observedAttempt outcome state =
  case timerAttemptFailureProbe observedAttempt of
    Just (identifier, observedGeneration) ->
      case Map.lookup identifier state.probes of
        Just probe
          | ProbeAwaitingResponse attempt <- probe.phase,
            attempt.timerGeneration == observedGeneration ->
              applyCurrent identifier probe attempt outcome state
        _ -> (state, [])
    Nothing -> (state, [])

applyCurrent ::
  HeraldFailureProbeId ->
  LocalProbe ->
  ActiveAttempt ->
  TimerOutcome ->
  State ->
  (State, [FailureDetectionAction])
applyCurrent identifier probe attempt (TimerFired firedAt) state
  | firedAt >= attempt.deadline =
      ( state
          { probes =
              Map.insert
                identifier
                probe {phase = ProbeReportRetained ProbeUnreachable}
                state.probes
          },
        [RetainFailureProbeReport identifier probe.capturedConfiguration ProbeUnreachable]
      )
  | otherwise =
      let generation = nextTimerAttemptGeneration attempt.timerGeneration
          successorAttempt = attempt {timerGeneration = generation}
          successorProbe = probe {phase = ProbeAwaitingResponse successorAttempt}
       in ( state
              { probes = Map.insert identifier successorProbe state.probes
              },
            [ ArmDirectFailureProbeTimer
                (failureProbeTimerAttempt identifier generation)
                (absoluteTimerSpec attempt.deadline)
            ]
          )

-- | Derive the one deterministic terminal command from stable voter-host
-- reports.  Duplicate reports and later entries cannot allocate another local
-- intention.
observeProjectedReports ::
  HeraldFailureProbeId ->
  HeraldEpoch ->
  [(HeraldEpoch, ProbeResult)] ->
  State ->
  (State, [FailureDetectionAction])
observeProjectedReports identifier target reports state
  | probesSuspended state = unchanged
  | Set.notMember state.local reporters = unchanged
  | Map.member identifier state.terminalIntentions = unchanged
  | Just retained <- Map.lookup identifier state.probes,
    retained.target /= target =
      unchanged
  | Just retained <- Map.lookup identifier state.probes,
    ProbeTerminal <- retained.phase =
      unchanged
  | Map.notMember identifier state.probes = unchanged
  | reachableMajority = retain DismissFailureProbe
  | unreachableMajority = retain RetireFailureProbeTarget
  | otherwise = unchanged
  where
    reportMap = Map.fromList reports
    reporters = maybe Set.empty (.capturedReporters) (Map.lookup identifier state.probes)
    eligibleResults = Map.restrictKeys reportMap reporters
    majority count = 2 * count > Set.size reporters
    reachableMajority =
      majority (length (filter (== ProbeReachable) (Map.elems eligibleResults)))
    unreachableMajority =
      majority (length (filter (== ProbeUnreachable) (Map.elems eligibleResults)))
    retain resolution =
      let resolutionId = deriveFailureProbeResolutionId identifier resolution
          action = case resolution of
            DismissFailureProbe -> RetainFailureProbeDismissal resolutionId
            RetireFailureProbeTarget
              | Set.member target reporters -> RetainVoterHostFailureAcceptance resolutionId
              | otherwise -> RetainHeraldRetirement resolutionId target
       in ( state
              { terminalIntentions =
                  Map.insert identifier resolution state.terminalIntentions
              },
            [action]
          )
    unchanged = (state, [])

-- | Terminal projection clears the exact attempt.  Retained probe evidence
-- remains terminal so stale responses and timer callbacks are inert, while the
-- historical recovery-qualified Open key prevents that same paced episode from
-- reopening.  A later genuine recovery generation may open a new probe after
-- Dismiss.
observeProbeTerminal ::
  HeraldFailureProbeId ->
  State ->
  (State, [FailureDetectionAction])
observeProbeTerminal identifier state =
  case Map.lookup identifier state.probes of
    Nothing -> (state, [])
    Just probe ->
      ( state
          { probes =
              Map.insert identifier probe {phase = ProbeTerminal} state.probes,
            terminalIntentions = Map.delete identifier state.terminalIntentions
          },
        case probe.phase of
          ProbeAwaitingResponse attempt ->
            [ CancelDirectFailureProbeTimer
                ( failureProbeTimerAttempt
                    identifier
                    attempt.timerGeneration
                )
            ]
          ProbeReportRetained {} -> []
          ProbeTerminal -> []
      )

-- | Cut every physical direct-probe attempt at orderly drain without
-- manufacturing a report from shutdown.
cutForDrain :: State -> (State, [TimerAttempt])
cutForDrain state =
  ( state
      { probes = Map.map terminalize state.probes,
        openIntentions = Set.empty,
        terminalIntentions = Map.empty
      },
    [ failureProbeTimerAttempt identifier attempt.timerGeneration
    | (identifier, probe) <- Map.toAscList state.probes,
      ProbeAwaitingResponse attempt <- [probe.phase]
    ]
  )
  where
    terminalize probe = probe {phase = ProbeTerminal}
