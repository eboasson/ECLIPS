{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure owner of one Herald's irreversible local isolation fence.
--
-- Fresh native-owner rounds provide the effective stable or joint quorum.
-- The owner never changes Oracle membership or stops the co-hosted Raft role:
-- it only decides that this local branch can no longer accept semantic work.
module Eclips.Herald.Isolation.State
  ( State,
    IsolationInitializationProblem (..),
    IsolationInitializationDisposition (..),
    initialState,
    freshJoiningReplayContext,
    configureOracleHealth,
    configureOracleHealthWithCadence,
    oracleHealthRound,
    observeOracleHealthRound,
    observeOracleQuorum,
    startJoiningParticipation,
    IsolationReachabilityDisposition (..),
    observeVoterConfiguration,
    observeVoterBound,
    observeVoterLost,
    IsolationSelfFenceReason (..),
    PreparedSelfFence,
    preparedSelfFenceReason,
    preparedSelfFenceTimerCancellation,
    prepareRetiredEpochRejection,
    prepareLocallyLearnedRetirement,
    IsolationGraceTimerDisposition (..),
    observeGraceTimer,
    IsolationDrainRequirement (..),
    IsolationSelfFenceDisposition (..),
    commitSelfFence,
    IsolationDrainTimerDisposition (..),
    observeDrainTimer,
    IsolationDrainCompletionDisposition (..),
    completeReadOnlyDrain,
    IsolationPhaseView (..),
    IsolationStateWitness,
    stateWitness,
    isolationWitnessLocalHerald,
    isolationWitnessVoterHosts,
    isolationWitnessVoterQuorums,
    isolationWitnessMembershipGeneration,
    isolationWitnessReachableVoterHosts,
    isolationWitnessRequiredVoterCount,
    isolationWitnessPhase,
    isolationWitnessLastGraceGeneration,
    isolationWitnessCurrentTimer,
    isolationWitnessCurrentDeadline,
    isolationWitnessCurrentDrain,
    isolationWitnessResidentProcessOverlay,
    isolationWitnessSelfFenceReason,
    IsolationInvariantViolation (..),
    validateState,
  )
where

import Control.Monad (unless)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity (HeraldEpoch, ProcessEpochId)
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Herald.Isolation.Internal
  ( IsolationConfiguration,
    IsolationDrainId,
    IsolationGraceGeneration,
    firstIsolationDrainId,
    firstIsolationGraceGeneration,
    isolationDrainIdWord64,
    isolationDrainMicroseconds,
    isolationGraceGenerationWord64,
    isolationGraceMicroseconds,
    nextIsolationDrainId,
    nextIsolationGraceGeneration,
  )
import Eclips.Herald.OracleHealth (OracleHealthObservation)
import Eclips.Herald.OracleHealth.State qualified as OracleHealth
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
    firstTimerAttemptGeneration,
    isolationDrainTimerAttempt,
    isolationGraceTimerAttempt,
    nextTimerAttemptGeneration,
    timerAttemptGenerationWord64,
    timerAttemptIsolationDrain,
    timerAttemptIsolationGrace,
  )
import Eclips.Raft.Configuration (RaftConfigurationRef, RaftVotingConfiguration)
import Eclips.Raft.Identity (RaftNodeId)

data GraceEpisode = GraceEpisode
  { graceGeneration :: IsolationGraceGeneration,
    graceDeadline :: MonotonicInstant,
    graceAttempt :: TimerAttemptGeneration
  }
  deriving stock (Eq, Show)

data DrainEpisode = DrainEpisode
  { drainIdentifier :: IsolationDrainId,
    drainDeadline :: MonotonicInstant,
    drainAttempt :: TimerAttemptGeneration,
    drainResidentProcesses :: Set ProcessEpochId,
    drainReason :: IsolationSelfFenceReason
  }
  deriving stock (Eq, Show)

data TerminalIsolation = TerminalIsolation
  { terminalIdentifier :: IsolationDrainId,
    terminalResidentProcesses :: Set ProcessEpochId,
    terminalReason :: IsolationSelfFenceReason
  }
  deriving stock (Eq, Show)

data IsolationPhase
  = VoterQuorumHealthy
  | VoterQuorumGrace GraceEpisode
  | ReadOnlyIsolationDrain DrainEpisode
  | IsolationTerminal TerminalIsolation
  deriving stock (Eq, Show)

data State = State
  { configuration :: IsolationConfiguration,
    localHerald :: HeraldEpoch,
    voterHosts :: Set HeraldEpoch,
    jointVoterHosts :: Maybe (Set HeraldEpoch, Set HeraldEpoch),
    membershipGeneration :: HeraldMembershipGenerationId,
    reachableVoterHosts :: Set HeraldEpoch,
    lastGraceGeneration :: Maybe IsolationGraceGeneration,
    nextDrain :: IsolationDrainId,
    health :: Maybe OracleHealth.State,
    quorumConfirmed :: Bool,
    phase :: IsolationPhase
  }
  deriving stock (Eq, Show)

data IsolationInitializationProblem
  = IsolationVoterHostSetEmpty
  deriving stock (Eq, Ord, Show)

data IsolationInitializationDisposition
  = InitialIsolationGraceArmed TimerAttempt TimerSpec
  deriving stock (Eq, Show)

-- | Every Herald starts with one absolute grace and no fresh native quorum.
initialState ::
  MonotonicInstant ->
  HeraldEpoch ->
  Set HeraldEpoch ->
  HeraldMembershipGenerationId ->
  IsolationConfiguration ->
  Either
    IsolationInitializationProblem
    (State, IsolationInitializationDisposition)
initialState observedAt local voterSet generation configuration
  | Set.null voterSet = Left IsolationVoterHostSetEmpty
  | otherwise =
      let grace = initialGrace observedAt configuration
          attempt = graceTimer grace
       in Right
            ( base
                { lastGraceGeneration = Just grace.graceGeneration,
                  phase = VoterQuorumGrace grace
                },
              InitialIsolationGraceArmed
                attempt
                (absoluteTimerSpec grace.graceDeadline)
            )
  where
    base =
      State
        { configuration,
          localHerald = local,
          voterHosts = voterSet,
          jointVoterHosts = Nothing,
          membershipGeneration = generation,
          reachableVoterHosts = Set.empty,
          lastGraceGeneration = Nothing,
          nextDrain = firstIsolationDrainId,
          health = Nothing,
          quorumConfirmed = False,
          phase = VoterQuorumHealthy
        }

-- | Disposable private replay context using this owner's configured policy.
-- Its provisional timer is not emitted; the live owner retains its generations,
-- native health observations and irreversible fencing state.
freshJoiningReplayContext :: MonotonicInstant -> Set HeraldEpoch -> HeraldMembershipGenerationId -> State -> Either IsolationInitializationProblem State
freshJoiningReplayContext now voters generation state =
  fst <$> initialState now state.localHerald voters generation state.configuration

-- | A pending observer has never emitted its provisional isolation timer.
-- Begin the ordinary non-voter grace when its own activation is installed.
startJoiningParticipation :: MonotonicInstant -> HeraldMembershipGenerationId -> State -> Either IsolationInitializationProblem (State, IsolationInitializationDisposition)
startJoiningParticipation now generation state =
  fmap
    (\(successor, disposition) -> (successor {jointVoterHosts = state.jointVoterHosts, health = state.health}, disposition))
    (initialState now state.localHerald state.voterHosts generation state.configuration)

data IsolationReachabilityDisposition
  = IsolationReachabilityUnchanged
  | IsolationGraceCancelled TimerAttempt
  | IsolationGraceStarted TimerAttempt TimerSpec
  deriving stock (Eq, Show)

-- | Install committed role facts. The last argument is an explicitly supplied
-- fresh quorum-member projection for detached pure tests; the production
-- composer passes empty and only native health rounds establish availability.
-- Existing grace deadlines and irreversible semantic fences remain unchanged.
observeVoterConfiguration ::
  MonotonicInstant ->
  Set HeraldEpoch ->
  Maybe (Set HeraldEpoch) ->
  Set HeraldEpoch ->
  State ->
  Either IsolationInitializationProblem (State, IsolationReachabilityDisposition)
observeVoterConfiguration observedAt oldHosts newHosts establishedHosts state
  | Set.null oldHosts || maybe False Set.null newHosts =
      Left IsolationVoterHostSetEmpty
  | otherwise = Right $ case state.phase of
      ReadOnlyIsolationDrain {} -> (withRoles, IsolationReachabilityUnchanged)
      IsolationTerminal {} -> (withRoles, IsolationReachabilityUnchanged)
      _
        | hasVoterQuorum reachable withRoles ->
            ( withRoles {reachableVoterHosts = reachable, quorumConfirmed = True, phase = VoterQuorumHealthy},
              cancellation
            )
        | VoterQuorumGrace {} <- state.phase ->
            (withRoles {reachableVoterHosts = reachable, quorumConfirmed = False}, IsolationReachabilityUnchanged)
        | otherwise ->
            let grace = nextGrace observedAt state
             in ( withRoles
                    { reachableVoterHosts = reachable,
                      quorumConfirmed = False,
                      lastGraceGeneration = Just grace.graceGeneration,
                      phase = VoterQuorumGrace grace
                    },
                  IsolationGraceStarted
                    (graceTimer grace)
                    (absoluteTimerSpec grace.graceDeadline)
                )
  where
    hosts = maybe oldHosts (Set.union oldHosts) newHosts
    reachable = Set.intersection hosts establishedHosts
    withRoles =
      state {voterHosts = hosts, jointVoterHosts = fmap (oldHosts,) newHosts}
    cancellation =
      maybe
        IsolationReachabilityUnchanged
        IsolationGraceCancelled
        (currentGraceTimer state.phase)

-- | Admit one established voter-host binding. Unrelated and duplicate peers
-- are exact no-ops. Reaching every required majority cancels the current grace;
-- connectivity after a self-fence never reactivates the branch.
observeVoterBound ::
  HeraldEpoch ->
  State ->
  (State, IsolationReachabilityDisposition)
observeVoterBound voter state
  | not (activePhase state.phase) = unchanged
  | Set.notMember voter state.voterHosts = unchanged
  | Set.member voter state.reachableVoterHosts = unchanged
  | otherwise =
      let reachable = Set.insert voter state.reachableVoterHosts
          withReachable = state {reachableVoterHosts = reachable}
       in case state.phase of
            VoterQuorumGrace grace
              | hasVoterQuorum reachable state ->
                  ( withReachable {quorumConfirmed = True, phase = VoterQuorumHealthy},
                    IsolationGraceCancelled (graceTimer grace)
                  )
            _ -> (withReachable, IsolationReachabilityUnchanged)
  where
    unchanged = (state, IsolationReachabilityUnchanged)

-- | Lose one current voter-host binding. A healthy non-voter that falls below
-- a required majority begins a fresh absolute episode. Repeated loss observations
-- never renew that episode.
observeVoterLost ::
  MonotonicInstant ->
  HeraldEpoch ->
  State ->
  (State, IsolationReachabilityDisposition)
observeVoterLost observedAt voter state
  | not (activePhase state.phase) = unchanged
  | Set.notMember voter state.reachableVoterHosts = unchanged
  | otherwise =
      let reachable = Set.delete voter state.reachableVoterHosts
          withReachable = state {reachableVoterHosts = reachable}
       in case state.phase of
            VoterQuorumHealthy
              | not (hasVoterQuorum reachable state) ->
                  let grace = nextGrace observedAt state
                   in ( withReachable
                          { quorumConfirmed = False,
                            lastGraceGeneration = Just grace.graceGeneration,
                            phase = VoterQuorumGrace grace
                          },
                        IsolationGraceStarted
                          (graceTimer grace)
                          (absoluteTimerSpec grace.graceDeadline)
                      )
            _ -> (withReachable, IsolationReachabilityUnchanged)
  where
    unchanged = (state, IsolationReachabilityUnchanged)

configureOracleHealth :: RaftConfigurationRef -> RaftVotingConfiguration -> State -> State
configureOracleHealth reference configuration state = state {health = Just (OracleHealth.initialState (isolationGraceMicroseconds state.configuration) reference configuration)}

configureOracleHealthWithCadence :: Word64 -> RaftConfigurationRef -> RaftVotingConfiguration -> State -> State
configureOracleHealthWithCadence cadence reference configuration state = state {health = Just (OracleHealth.initialStateWithCadence cadence reference configuration)}

oracleHealthRound :: State -> Maybe (Word64, Word64)
oracleHealthRound state = (\health -> (OracleHealth.currentRound health, OracleHealth.roundCadence health)) <$> state.health

observeOracleHealthRound :: MonotonicInstant -> Word64 -> RaftConfigurationRef -> RaftVotingConfiguration -> Set RaftNodeId -> [OracleHealthObservation] -> State -> Maybe (State, IsolationReachabilityDisposition)
observeOracleHealthRound now roundNumber reference configuration known replies state = do
  health <- state.health
  (successor, healthy) <- OracleHealth.observeRound roundNumber reference configuration known replies health
  Just (observeOracleQuorum now healthy state {health = Just successor})

observeOracleQuorum :: MonotonicInstant -> Bool -> State -> (State, IsolationReachabilityDisposition)
observeOracleQuorum now healthy state
  | not (activePhase state.phase) = (state, IsolationReachabilityUnchanged)
  | healthy = (state {quorumConfirmed = True, phase = VoterQuorumHealthy}, maybe IsolationReachabilityUnchanged IsolationGraceCancelled (currentGraceTimer state.phase))
  | VoterQuorumGrace {} <- state.phase = (state {quorumConfirmed = False}, IsolationReachabilityUnchanged)
  | otherwise =
      let grace = nextGrace now state
       in (state {quorumConfirmed = False, lastGraceGeneration = Just grace.graceGeneration, phase = VoterQuorumGrace grace}, IsolationGraceStarted (graceTimer grace) (absoluteTimerSpec grace.graceDeadline))

data IsolationSelfFenceReason
  = IsolationQuorumGraceExpired
  | IsolationRetiredEpochRejected
      HeraldEpoch
      HeraldMembershipGenerationId
  | IsolationRetirementLearned HeraldMembershipGenerationId
  deriving stock (Eq, Show)

-- | Opaque transaction handle. The whole-Herald coordinator must supply the
-- exact resident-process overlay and whether any live application lane needs
-- the bounded read-only drain before committing this fence.
data PreparedSelfFence = PreparedSelfFence
  { predecessor :: State,
    observedAt :: MonotonicInstant,
    reason :: IsolationSelfFenceReason,
    timerCancellation :: Maybe TimerAttempt
  }
  deriving stock (Eq, Show)

preparedSelfFenceReason :: PreparedSelfFence -> IsolationSelfFenceReason
preparedSelfFenceReason prepared = prepared.reason

preparedSelfFenceTimerCancellation :: PreparedSelfFence -> Maybe TimerAttempt
preparedSelfFenceTimerCancellation prepared = prepared.timerCancellation

-- | A current-generation retired-epoch rejection from any checked voter host
-- is stronger than waiting for the local grace. Stale generations, unrelated
-- peers and already-fenced states cannot prepare a transition.
prepareRetiredEpochRejection ::
  MonotonicInstant ->
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  State ->
  Maybe PreparedSelfFence
prepareRetiredEpochRejection observedAt reporter rejectedGeneration state
  | not (activePhase state.phase) = Nothing
  | Set.notMember reporter state.voterHosts = Nothing
  | rejectedGeneration /= state.membershipGeneration = Nothing
  | otherwise =
      Just
        PreparedSelfFence
          { predecessor = state,
            observedAt,
            reason = IsolationRetiredEpochRejected reporter rejectedGeneration,
            timerCancellation = currentGraceTimer state.phase
          }

-- | Learning a membership successor which retires this local Herald is at
-- least as strong as a remote retired-epoch rejection. It must converge through
-- the same irreversible drain/terminal cut instead of allowing the top-level
-- retired dispatch to strand the active isolation timer forever.
prepareLocallyLearnedRetirement ::
  MonotonicInstant ->
  HeraldMembershipGenerationId ->
  State ->
  Maybe PreparedSelfFence
prepareLocallyLearnedRetirement observedAt successorGeneration state
  | not (activePhase state.phase) = Nothing
  | successorGeneration == state.membershipGeneration = Nothing
  | otherwise =
      Just
        PreparedSelfFence
          { predecessor = state {membershipGeneration = successorGeneration},
            observedAt,
            reason = IsolationRetirementLearned successorGeneration,
            timerCancellation = currentGraceTimer state.phase
          }

data IsolationGraceTimerDisposition
  = IsolationGraceTimerStale
  | IsolationGraceTimerRearmed TimerAttempt TimerSpec
  | IsolationGraceTimerSelfFenceReady PreparedSelfFence
  deriving stock (Eq, Show)

-- | Apply one exact grace-timer observation. Early delivery consumes the
-- physical attempt and rearms against the unchanged absolute deadline. At or
-- after that deadline the state itself remains unchanged until the coordinator
-- atomically commits the returned opaque fence with all other owner cuts.
observeGraceTimer ::
  TimerAttempt ->
  TimerOutcome ->
  State ->
  (State, IsolationGraceTimerDisposition)
observeGraceTimer observedAttempt outcome state =
  case (state.phase, timerAttemptIsolationGrace observedAttempt) of
    (VoterQuorumGrace grace, Just (generation, attempt))
      | generation == grace.graceGeneration,
        attempt == grace.graceAttempt ->
          case outcome of
            TimerFired firedAt
              | firedAt >= grace.graceDeadline ->
                  ( state,
                    IsolationGraceTimerSelfFenceReady
                      PreparedSelfFence
                        { predecessor = state,
                          observedAt = firedAt,
                          reason = IsolationQuorumGraceExpired,
                          timerCancellation = Nothing
                        }
                  )
              | otherwise ->
                  let nextAttempt = nextTimerAttemptGeneration grace.graceAttempt
                      nextEpisode = grace {graceAttempt = nextAttempt}
                      successor = state {phase = VoterQuorumGrace nextEpisode}
                   in ( successor,
                        IsolationGraceTimerRearmed
                          (graceTimer nextEpisode)
                          (absoluteTimerSpec grace.graceDeadline)
                      )
    _ -> (state, IsolationGraceTimerStale)

data IsolationSelfFenceDisposition
  = IsolationReadOnlyDrainStarted
      IsolationDrainId
      TimerAttempt
      TimerSpec
      (Maybe TimerAttempt)
      IsolationSelfFenceReason
  | IsolationTerminatedWithoutDrain
      IsolationDrainId
      (Maybe TimerAttempt)
      IsolationSelfFenceReason
  deriving stock (Eq, Show)

-- | Whether the atomic application-owner cut found an established local lane
-- that must receive the ordered isolation notice before terminal close.
data IsolationDrainRequirement
  = IsolationDrainRequired
  | IsolationDrainNotRequired
  deriving stock (Eq, Ord, Show)

-- | Atomically install the abandoned-branch overlay and either start the one
-- bounded read-only drain or become terminal immediately when no application
-- lane is available to observe it.
commitSelfFence ::
  Set ProcessEpochId ->
  IsolationDrainRequirement ->
  PreparedSelfFence ->
  (State, IsolationSelfFenceDisposition)
commitSelfFence residentProcesses drainRequirement prepared
  | drainRequirement == IsolationDrainRequired =
      let episode =
            DrainEpisode
              { drainIdentifier = drain,
                drainDeadline = deadline,
                drainAttempt = firstTimerAttemptGeneration,
                drainResidentProcesses = residentProcesses,
                drainReason = prepared.reason
              }
          attempt = drainTimer episode
       in ( successor {phase = ReadOnlyIsolationDrain episode},
            IsolationReadOnlyDrainStarted
              drain
              attempt
              (absoluteTimerSpec deadline)
              prepared.timerCancellation
              prepared.reason
          )
  | otherwise =
      ( successor
          { phase =
              IsolationTerminal
                TerminalIsolation
                  { terminalIdentifier = drain,
                    terminalResidentProcesses = residentProcesses,
                    terminalReason = prepared.reason
                  }
          },
        IsolationTerminatedWithoutDrain
          drain
          prepared.timerCancellation
          prepared.reason
      )
  where
    predecessor = prepared.predecessor
    drain = predecessor.nextDrain
    deadline =
      monotonicInstant
        ( monotonicInstantWord64 prepared.observedAt
            + isolationDrainMicroseconds predecessor.configuration
        )
    successor =
      predecessor
        { reachableVoterHosts = Set.empty,
          nextDrain = nextIsolationDrainId drain
        }

data IsolationDrainTimerDisposition
  = IsolationDrainTimerStale
  | IsolationDrainTimerRearmed TimerAttempt TimerSpec
  | IsolationDrainTimerExpired IsolationDrainId
  deriving stock (Eq, Show)

-- | Apply one exact bounded-drain timer observation. Once terminal, every
-- later timer or reachability observation is an exact no-op.
observeDrainTimer ::
  TimerAttempt ->
  TimerOutcome ->
  State ->
  (State, IsolationDrainTimerDisposition)
observeDrainTimer observedAttempt outcome state =
  case (state.phase, timerAttemptIsolationDrain observedAttempt) of
    (ReadOnlyIsolationDrain drain, Just (identifier, attempt))
      | identifier == drain.drainIdentifier,
        attempt == drain.drainAttempt ->
          case outcome of
            TimerFired firedAt
              | firedAt >= drain.drainDeadline ->
                  ( terminalize drain state,
                    IsolationDrainTimerExpired drain.drainIdentifier
                  )
              | otherwise ->
                  let nextAttempt = nextTimerAttemptGeneration drain.drainAttempt
                      nextEpisode = drain {drainAttempt = nextAttempt}
                   in ( state {phase = ReadOnlyIsolationDrain nextEpisode},
                        IsolationDrainTimerRearmed
                          (drainTimer nextEpisode)
                          (absoluteTimerSpec drain.drainDeadline)
                      )
    _ -> (state, IsolationDrainTimerStale)

data IsolationDrainCompletionDisposition
  = IsolationDrainCompletionStale
  | IsolationDrainCompleted IsolationDrainId TimerAttempt
  deriving stock (Eq, Show)

-- | Finish early after the whole-Herald coordinator proves that no application
-- lane remains in the read-only drain. The exact physical timer is returned
-- for cancellation.
completeReadOnlyDrain ::
  IsolationDrainId ->
  State ->
  (State, IsolationDrainCompletionDisposition)
completeReadOnlyDrain identifier state = case state.phase of
  ReadOnlyIsolationDrain drain
    | drain.drainIdentifier == identifier ->
        ( terminalize drain state,
          IsolationDrainCompleted identifier (drainTimer drain)
        )
  _ -> (state, IsolationDrainCompletionStale)

data IsolationPhaseView
  = IsolationVoterQuorumHealthyView
  | IsolationVoterQuorumGraceView
  | IsolationReadOnlyDrainView
  | IsolationTerminalView
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data IsolationStateWitness = IsolationStateWitness
  { witnessLocalHerald :: HeraldEpoch,
    witnessVoterHosts :: Set HeraldEpoch,
    witnessVoterQuorums :: (Set HeraldEpoch, Maybe (Set HeraldEpoch)),
    witnessMembershipGeneration :: HeraldMembershipGenerationId,
    witnessReachableVoterHosts :: Set HeraldEpoch,
    witnessRequiredVoterCount :: Int,
    witnessPhase :: IsolationPhaseView,
    witnessLastGraceGeneration :: Maybe IsolationGraceGeneration,
    witnessCurrentTimer :: Maybe TimerAttempt,
    witnessCurrentDeadline :: Maybe MonotonicInstant,
    witnessCurrentDrain :: Maybe IsolationDrainId,
    witnessResidentProcessOverlay :: Set ProcessEpochId,
    witnessSelfFenceReason :: Maybe IsolationSelfFenceReason
  }
  deriving stock (Eq, Show)

stateWitness :: State -> IsolationStateWitness
stateWitness state =
  IsolationStateWitness
    { witnessLocalHerald = state.localHerald,
      witnessVoterHosts = state.voterHosts,
      witnessVoterQuorums = voterQuorums state,
      witnessMembershipGeneration = state.membershipGeneration,
      witnessReachableVoterHosts = state.reachableVoterHosts,
      witnessRequiredVoterCount = requiredVoterCount state,
      witnessPhase = phaseView state.phase,
      witnessLastGraceGeneration = state.lastGraceGeneration,
      witnessCurrentTimer = phaseTimer state.phase,
      witnessCurrentDeadline = phaseDeadline state.phase,
      witnessCurrentDrain = phaseDrain state.phase,
      witnessResidentProcessOverlay = phaseResidents state.phase,
      witnessSelfFenceReason = phaseReason state.phase
    }

isolationWitnessLocalHerald :: IsolationStateWitness -> HeraldEpoch
isolationWitnessLocalHerald witness = witness.witnessLocalHerald

isolationWitnessVoterHosts :: IsolationStateWitness -> Set HeraldEpoch
isolationWitnessVoterHosts witness = witness.witnessVoterHosts

isolationWitnessVoterQuorums ::
  IsolationStateWitness -> (Set HeraldEpoch, Maybe (Set HeraldEpoch))
isolationWitnessVoterQuorums witness = witness.witnessVoterQuorums

isolationWitnessMembershipGeneration ::
  IsolationStateWitness -> HeraldMembershipGenerationId
isolationWitnessMembershipGeneration witness = witness.witnessMembershipGeneration

isolationWitnessReachableVoterHosts ::
  IsolationStateWitness -> Set HeraldEpoch
isolationWitnessReachableVoterHosts witness = witness.witnessReachableVoterHosts

isolationWitnessRequiredVoterCount :: IsolationStateWitness -> Int
isolationWitnessRequiredVoterCount witness = witness.witnessRequiredVoterCount

isolationWitnessPhase :: IsolationStateWitness -> IsolationPhaseView
isolationWitnessPhase witness = witness.witnessPhase

isolationWitnessLastGraceGeneration ::
  IsolationStateWitness -> Maybe IsolationGraceGeneration
isolationWitnessLastGraceGeneration witness = witness.witnessLastGraceGeneration

isolationWitnessCurrentTimer :: IsolationStateWitness -> Maybe TimerAttempt
isolationWitnessCurrentTimer witness = witness.witnessCurrentTimer

isolationWitnessCurrentDeadline ::
  IsolationStateWitness -> Maybe MonotonicInstant
isolationWitnessCurrentDeadline witness = witness.witnessCurrentDeadline

isolationWitnessCurrentDrain :: IsolationStateWitness -> Maybe IsolationDrainId
isolationWitnessCurrentDrain witness = witness.witnessCurrentDrain

isolationWitnessResidentProcessOverlay ::
  IsolationStateWitness -> Set ProcessEpochId
isolationWitnessResidentProcessOverlay witness = witness.witnessResidentProcessOverlay

isolationWitnessSelfFenceReason ::
  IsolationStateWitness -> Maybe IsolationSelfFenceReason
isolationWitnessSelfFenceReason witness = witness.witnessSelfFenceReason

data IsolationInvariantViolation
  = IsolationInvariantVoterHostSetEmpty
  | IsolationInvariantJointVoterHostsMismatch
  | IsolationInvariantReachabilityOutsideVoterHosts
  | IsolationInvariantHealthyWithoutQuorum
  | IsolationInvariantGraceWithQuorum
  | IsolationInvariantGraceGenerationZero
  | IsolationInvariantGraceGenerationMismatch
  | IsolationInvariantTimerAttemptZero
  | IsolationInvariantFencedReachabilityRetained
  | IsolationInvariantDrainIdZero
  deriving stock (Eq, Ord, Show)

validateState :: State -> Either IsolationInvariantViolation ()
validateState state = do
  unless
    (not (Set.null state.voterHosts))
    (Left IsolationInvariantVoterHostSetEmpty)
  case state.jointVoterHosts of
    Nothing -> Right ()
    Just (oldHosts, newHosts) ->
      unless
        ( not (Set.null oldHosts)
            && not (Set.null newHosts)
            && Set.union oldHosts newHosts == state.voterHosts
        )
        (Left IsolationInvariantJointVoterHostsMismatch)
  unless
    (state.reachableVoterHosts `Set.isSubsetOf` state.voterHosts)
    (Left IsolationInvariantReachabilityOutsideVoterHosts)
  unless
    (isolationDrainIdWord64 state.nextDrain > 0)
    (Left IsolationInvariantDrainIdZero)
  validateNonVoter state

validateNonVoter :: State -> Either IsolationInvariantViolation ()
validateNonVoter state = case state.phase of
  VoterQuorumHealthy ->
    unless
      (state.quorumConfirmed)
      (Left IsolationInvariantHealthyWithoutQuorum)
  VoterQuorumGrace grace -> do
    unless
      (not (state.quorumConfirmed))
      (Left IsolationInvariantGraceWithQuorum)
    unless
      (isolationGraceGenerationWord64 grace.graceGeneration > 0)
      (Left IsolationInvariantGraceGenerationZero)
    unless
      (state.lastGraceGeneration == Just grace.graceGeneration)
      (Left IsolationInvariantGraceGenerationMismatch)
    unless
      (timerAttemptGenerationWord64 grace.graceAttempt > 0)
      (Left IsolationInvariantTimerAttemptZero)
  ReadOnlyIsolationDrain drain -> do
    unless
      (Set.null state.reachableVoterHosts)
      (Left IsolationInvariantFencedReachabilityRetained)
    unless
      (isolationDrainIdWord64 drain.drainIdentifier > 0)
      (Left IsolationInvariantDrainIdZero)
    unless
      (timerAttemptGenerationWord64 drain.drainAttempt > 0)
      (Left IsolationInvariantTimerAttemptZero)
  IsolationTerminal terminal -> do
    unless
      (Set.null state.reachableVoterHosts)
      (Left IsolationInvariantFencedReachabilityRetained)
    unless
      (isolationDrainIdWord64 terminal.terminalIdentifier > 0)
      (Left IsolationInvariantDrainIdZero)

initialGrace :: MonotonicInstant -> IsolationConfiguration -> GraceEpisode
initialGrace observedAt configuration =
  GraceEpisode
    { graceGeneration = firstIsolationGraceGeneration,
      graceDeadline =
        monotonicInstant
          ( monotonicInstantWord64 observedAt
              + isolationGraceMicroseconds configuration
          ),
      graceAttempt = firstTimerAttemptGeneration
    }

nextGrace :: MonotonicInstant -> State -> GraceEpisode
nextGrace observedAt state =
  GraceEpisode
    { graceGeneration =
        maybe
          firstIsolationGraceGeneration
          nextIsolationGraceGeneration
          state.lastGraceGeneration,
      graceDeadline =
        monotonicInstant
          ( monotonicInstantWord64 observedAt
              + isolationGraceMicroseconds state.configuration
          ),
      graceAttempt = firstTimerAttemptGeneration
    }

graceTimer :: GraceEpisode -> TimerAttempt
graceTimer grace =
  isolationGraceTimerAttempt grace.graceGeneration grace.graceAttempt

drainTimer :: DrainEpisode -> TimerAttempt
drainTimer drain =
  isolationDrainTimerAttempt drain.drainIdentifier drain.drainAttempt

currentGraceTimer :: IsolationPhase -> Maybe TimerAttempt
currentGraceTimer = \case
  VoterQuorumGrace grace -> Just (graceTimer grace)
  _ -> Nothing

activePhase :: IsolationPhase -> Bool
activePhase = \case
  VoterQuorumHealthy -> True
  VoterQuorumGrace {} -> True
  _ -> False

requiredVoterCount :: State -> Int
requiredVoterCount state = case state.jointVoterHosts of
  Nothing -> majorityCount state.voterHosts
  Just (oldHosts, newHosts) ->
    -- This is only the minimum possible cardinality, for status display. The
    -- identities must still satisfy both predicates in hasVoterQuorum.
    maximum
      [ majorityCount oldHosts,
        majorityCount newHosts,
        majorityCount oldHosts
          + majorityCount newHosts
          - Set.size (Set.intersection oldHosts newHosts)
      ]

hasVoterQuorum :: Set HeraldEpoch -> State -> Bool
hasVoterQuorum reachable state = case state.jointVoterHosts of
  Nothing -> hasMajority state.voterHosts
  Just (oldHosts, newHosts) -> hasMajority oldHosts && hasMajority newHosts
  where
    hasMajority hosts = Set.size (Set.intersection reachable hosts) >= majorityCount hosts

majorityCount :: Set HeraldEpoch -> Int
majorityCount hosts = Set.size hosts `div` 2 + 1

voterQuorums :: State -> (Set HeraldEpoch, Maybe (Set HeraldEpoch))
voterQuorums state =
  maybe
    (state.voterHosts, Nothing)
    (\(oldHosts, newHosts) -> (oldHosts, Just newHosts))
    state.jointVoterHosts

terminalize :: DrainEpisode -> State -> State
terminalize drain state =
  state
    { phase =
        IsolationTerminal
          TerminalIsolation
            { terminalIdentifier = drain.drainIdentifier,
              terminalResidentProcesses = drain.drainResidentProcesses,
              terminalReason = drain.drainReason
            }
    }

phaseView :: IsolationPhase -> IsolationPhaseView
phaseView = \case
  VoterQuorumHealthy -> IsolationVoterQuorumHealthyView
  VoterQuorumGrace {} -> IsolationVoterQuorumGraceView
  ReadOnlyIsolationDrain {} -> IsolationReadOnlyDrainView
  IsolationTerminal {} -> IsolationTerminalView

phaseTimer :: IsolationPhase -> Maybe TimerAttempt
phaseTimer = \case
  VoterQuorumGrace grace -> Just (graceTimer grace)
  ReadOnlyIsolationDrain drain -> Just (drainTimer drain)
  _ -> Nothing

phaseDeadline :: IsolationPhase -> Maybe MonotonicInstant
phaseDeadline = \case
  VoterQuorumGrace grace -> Just grace.graceDeadline
  ReadOnlyIsolationDrain drain -> Just drain.drainDeadline
  _ -> Nothing

phaseDrain :: IsolationPhase -> Maybe IsolationDrainId
phaseDrain = \case
  ReadOnlyIsolationDrain drain -> Just drain.drainIdentifier
  IsolationTerminal terminal -> Just terminal.terminalIdentifier
  _ -> Nothing

phaseResidents :: IsolationPhase -> Set ProcessEpochId
phaseResidents = \case
  ReadOnlyIsolationDrain drain -> drain.drainResidentProcesses
  IsolationTerminal terminal -> terminal.terminalResidentProcesses
  _ -> Set.empty

phaseReason :: IsolationPhase -> Maybe IsolationSelfFenceReason
phaseReason = \case
  ReadOnlyIsolationDrain drain -> Just drain.drainReason
  IsolationTerminal terminal -> Just terminal.terminalReason
  _ -> Nothing
