{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure owner of peer recovery generations, deadlines, and local suspicion.
--
-- Discovery owns bindings and addresses. This owner begins an episode when the
-- top-level coordinator proves that an active exact peer is known and unbound,
-- whether through first contact admission or loss of a current binding, and
-- clears it only when that coordinator admits a replacement binding.
module Eclips.Herald.PeerLiveness.State
  ( State,
    initialState,
    freshJoiningReplayContext,
    PeerLivenessInvariantViolation (..),
    validateState,
    PeerRecoveryStatusWitness (..),
    PeerRecoveryWitness (..),
    StateWitness (..),
    stateWitness,
    recoveryTargets,
    BeginRecoveryDisposition (..),
    beginRecovery,
    RestartRecoveryDisposition (..),
    restartRecovery,
    clearRecovery,
    retirePeer,
    TimerObservationDisposition (..),
    observeTimer,
    cutForDrain,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Herald.PeerLiveness.Internal
  ( PeerRecoveryConfiguration,
    PeerRecoveryGeneration,
    firstPeerRecoveryGeneration,
    nextPeerRecoveryGeneration,
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
    firstTimerAttemptGeneration,
    nextTimerAttemptGeneration,
    peerRecoveryTimerAttempt,
    timerAttemptGenerationWord64,
    timerAttemptPeerRecovery,
  )

data PeerRecoveryPhase
  = Recovering TimerAttemptGeneration
  | Suspected
  deriving stock (Eq, Show)

data PeerRecovery = PeerRecovery
  { generation :: PeerRecoveryGeneration,
    deadline :: MonotonicInstant,
    phase :: PeerRecoveryPhase
  }
  deriving stock (Eq, Show)

data PeerRecord = PeerRecord
  { lastGeneration :: PeerRecoveryGeneration,
    currentRecovery :: Maybe PeerRecovery
  }
  deriving stock (Eq, Show)

data State = State
  { configuration :: PeerRecoveryConfiguration,
    peers :: Map HeraldEpoch PeerRecord
  }
  deriving stock (Eq, Show)

initialState :: PeerRecoveryConfiguration -> State
initialState configuration = State {configuration, peers = Map.empty}

-- | Disposable private replay context retaining this owner's recovery policy.
-- Existing live recovery episodes and timer generations are not replaced.
freshJoiningReplayContext :: State -> State
freshJoiningReplayContext state = initialState state.configuration

data PeerLivenessInvariantViolation
  = PeerLivenessLocalTarget HeraldEpoch
  | PeerLivenessInactiveTarget HeraldEpoch
  | PeerLivenessRecoveryGenerationZero HeraldEpoch
  | PeerLivenessCurrentGenerationMismatch HeraldEpoch
  | PeerLivenessTimerAttemptGenerationZero HeraldEpoch
  deriving stock (Eq, Ord, Show)

validateState ::
  HeraldEpoch ->
  Set HeraldEpoch ->
  State ->
  Either PeerLivenessInvariantViolation ()
validateState local active state = mapM_ validatePeer (Map.toAscList state.peers)
  where
    validatePeer (peer, record)
      | peer == local = Left (PeerLivenessLocalTarget peer)
      | peer `Set.notMember` active = Left (PeerLivenessInactiveTarget peer)
      | peerRecoveryGenerationWord64 record.lastGeneration == 0 =
          Left (PeerLivenessRecoveryGenerationZero peer)
      | otherwise = case record.currentRecovery of
          Nothing -> Right ()
          Just recovery
            | recovery.generation /= record.lastGeneration ->
                Left (PeerLivenessCurrentGenerationMismatch peer)
            | otherwise -> case recovery.phase of
                Suspected -> Right ()
                Recovering attempt
                  | timerAttemptGenerationWord64 attempt == 0 ->
                      Left (PeerLivenessTimerAttemptGenerationZero peer)
                  | otherwise -> Right ()

data PeerRecoveryStatusWitness
  = PeerRecoveringWitness TimerAttempt
  | PeerSuspectedWitness
  deriving stock (Eq, Show)

data PeerRecoveryWitness = PeerRecoveryWitness
  { target :: HeraldEpoch,
    generation :: PeerRecoveryGeneration,
    deadline :: MonotonicInstant,
    status :: PeerRecoveryStatusWitness
  }
  deriving stock (Eq, Show)

data StateWitness = StateWitness
  { graceMicroseconds :: Word64,
    recoveries :: [PeerRecoveryWitness]
  }
  deriving stock (Eq, Show)

stateWitness :: State -> StateWitness
stateWitness state =
  StateWitness
    { graceMicroseconds = peerRecoveryGraceMicroseconds state.configuration,
      recoveries =
        [ recoveryWitness peer recovery
        | (peer, record) <- Map.toAscList state.peers,
          recovery <- maybe [] pure record.currentRecovery
        ]
    }

recoveryTargets :: State -> Set HeraldEpoch
recoveryTargets state =
  Map.keysSet (Map.filter (maybe False (const True) . (.currentRecovery)) state.peers)

recoveryWitness :: HeraldEpoch -> PeerRecovery -> PeerRecoveryWitness
recoveryWitness peer recovery =
  PeerRecoveryWitness
    { target = peer,
      generation = recovery.generation,
      deadline = recovery.deadline,
      status = case recovery.phase of
        Suspected -> PeerSuspectedWitness
        Recovering attempt ->
          PeerRecoveringWitness
            (peerRecoveryTimerAttempt peer recovery.generation attempt)
    }

data BeginRecoveryDisposition
  = RecoveryAlreadyActive
  | RecoveryStarted TimerAttempt TimerSpec
  deriving stock (Eq, Show)

-- | Begin one fixed-deadline episode. An already active episode is never
-- renewed by another loss observation.
beginRecovery ::
  MonotonicInstant ->
  HeraldEpoch ->
  State ->
  (State, BeginRecoveryDisposition)
beginRecovery observedAt peer state =
  case Map.lookup peer state.peers of
    Just record
      | Just _ <- record.currentRecovery ->
          (state, RecoveryAlreadyActive)
    retained ->
      let (successor, attempt, specification) =
            startRecovery observedAt peer retained state
       in (successor, RecoveryStarted attempt specification)

data RestartRecoveryDisposition
  = RecoveryRestarted (Maybe TimerAttempt) TimerAttempt TimerSpec
  deriving stock (Eq, Show)

-- | Replace any current episode with one fresh fixed-deadline generation.
-- This is deliberately distinct from repeated physical loss, which must never
-- renew an episode. A committed Dismiss or supersession consumes the probe's
-- old evidence and uses this operation to create the required paced episode.
-- A still-Recovering episode returns its exact physical timer for cancellation;
-- a terminal Suspected episode has no live timer to cancel.
restartRecovery ::
  MonotonicInstant ->
  HeraldEpoch ->
  State ->
  (State, RestartRecoveryDisposition)
restartRecovery observedAt peer state =
  let retained = Map.lookup peer state.peers
      cancellation = retained >>= (.currentRecovery) >>= currentTimer peer
      (successor, attempt, specification) =
        startRecovery observedAt peer retained state
   in ( successor,
        RecoveryRestarted cancellation attempt specification
      )

startRecovery ::
  MonotonicInstant ->
  HeraldEpoch ->
  Maybe PeerRecord ->
  State ->
  (State, TimerAttempt, TimerSpec)
startRecovery observedAt peer retained state =
  ( state {peers = Map.insert peer successorRecord state.peers},
    attempt,
    absoluteTimerSpec deadline
  )
  where
    generation = case retained of
      Nothing -> firstPeerRecoveryGeneration
      Just record -> nextPeerRecoveryGeneration record.lastGeneration
    attemptGeneration = firstTimerAttemptGeneration
    deadline =
      monotonicInstant
        ( monotonicInstantWord64 observedAt
            + peerRecoveryGraceMicroseconds state.configuration
        )
    recovery =
      PeerRecovery
        { generation,
          deadline,
          phase = Recovering attemptGeneration
        }
    successorRecord = PeerRecord generation (Just recovery)
    attempt = peerRecoveryTimerAttempt peer generation attemptGeneration

currentTimer :: HeraldEpoch -> PeerRecovery -> Maybe TimerAttempt
currentTimer peer recovery = case recovery.phase of
  Suspected -> Nothing
  Recovering attempt ->
    Just (peerRecoveryTimerAttempt peer recovery.generation attempt)

-- | Clear any uncommitted recovery/suspicion after admitting a live binding.
-- Only an outstanding physical timer needs an explicit cancellation effect.
clearRecovery :: HeraldEpoch -> State -> (State, Maybe TimerAttempt)
clearRecovery peer state = case Map.lookup peer state.peers of
  Nothing -> (state, Nothing)
  Just record -> case record.currentRecovery of
    Nothing -> (state, Nothing)
    Just recovery ->
      let cancellation = case recovery.phase of
            Suspected -> Nothing
            Recovering attempt ->
              Just (peerRecoveryTimerAttempt peer recovery.generation attempt)
          successor =
            state
              { peers =
                  Map.insert
                    peer
                    record {currentRecovery = Nothing}
                    state.peers
              }
       in (successor, cancellation)

-- | Permanently forget one peer's recovery episode after the authoritative
-- membership owner has retired that exact epoch. Admission is fenced by the
-- membership-aware coordinators; this owner only removes now-inapplicable
-- timer state and returns the exact cancellation, if any.
retirePeer :: HeraldEpoch -> State -> (State, Maybe TimerAttempt)
retirePeer peer state =
  ( state {peers = Map.delete peer state.peers},
    Map.lookup peer state.peers >>= (.currentRecovery) >>= currentTimer peer
  )

data TimerObservationDisposition
  = TimerObservationStale
  | TimerObservationRearmed TimerAttempt TimerSpec
  | TimerObservationExpired
  deriving stock (Eq, Show)

-- | Apply one exact runtime timer result. A matching early firing consumes that
-- physical attempt and allocates a replacement against the unchanged absolute
-- deadline. At or after the deadline, one suspicion is retained and the
-- attempt becomes stale. Host timer-allocation failure is a runtime resource
-- failure and never becomes a semantic input.
observeTimer ::
  TimerAttempt ->
  TimerOutcome ->
  State ->
  (State, TimerObservationDisposition)
observeTimer observedAttempt outcome state =
  case timerAttemptPeerRecovery observedAttempt of
    Just (peer, generation, attemptGeneration) ->
      case Map.lookup peer state.peers of
        Just record
          | Just recovery <- record.currentRecovery,
            recovery.generation == generation,
            Recovering currentAttempt <- recovery.phase,
            currentAttempt == attemptGeneration ->
              applyCurrent peer record recovery outcome state
        _ -> (state, TimerObservationStale)
    Nothing -> (state, TimerObservationStale)

applyCurrent ::
  HeraldEpoch ->
  PeerRecord ->
  PeerRecovery ->
  TimerOutcome ->
  State ->
  (State, TimerObservationDisposition)
applyCurrent peer record recovery outcome state = case outcome of
  TimerFired firedAt
    | firedAt >= recovery.deadline ->
        ( replaceRecovery peer record recovery {phase = Suspected} state,
          TimerObservationExpired
        )
    | otherwise -> rearm
  where
    rearm = case recovery.phase of
      Suspected -> (state, TimerObservationStale)
      Recovering attempt ->
        let successorAttempt = nextTimerAttemptGeneration attempt
            successorRecovery = recovery {phase = Recovering successorAttempt}
            successor = replaceRecovery peer record successorRecovery state
            timer =
              peerRecoveryTimerAttempt
                peer
                recovery.generation
                successorAttempt
         in ( successor,
              TimerObservationRearmed timer (absoluteTimerSpec recovery.deadline)
            )

replaceRecovery :: HeraldEpoch -> PeerRecord -> PeerRecovery -> State -> State
replaceRecovery peer record recovery state =
  state
    { peers =
        Map.insert
          peer
          record {currentRecovery = Just recovery}
          state.peers
    }

-- | Cut every current recovery episode at orderly drain. The returned attempts
-- are already in canonical target order for deterministic cancellation effects.
cutForDrain :: State -> (State, [TimerAttempt])
cutForDrain state =
  ( state
      { peers =
          Map.map
            (\record -> record {currentRecovery = Nothing})
            state.peers
      },
    [ peerRecoveryTimerAttempt peer recovery.generation attempt
    | (peer, record) <- Map.toAscList state.peers,
      recovery <- maybe [] pure record.currentRecovery,
      Recovering attempt <- [recovery.phase]
    ]
  )
