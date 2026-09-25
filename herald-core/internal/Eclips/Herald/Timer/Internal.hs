-- | Pure construction and inspection of logical timer attempts.
module Eclips.Herald.Timer.Internal
  ( TimerAttempt (..),
    TimerAttemptGeneration (..),
    firstTimerAttemptGeneration,
    nextTimerAttemptGeneration,
    peerRecoveryTimerAttempt,
    peerReceiptFlushTimerAttempt,
    failureProbeTimerAttempt,
    applicationSessionRecoveryTimerAttempt,
    isolationGraceTimerAttempt,
    isolationDrainTimerAttempt,
    timerAttemptGeneration,
    timerAttemptGenerationWord64,
    timerAttemptPeerRecovery,
    timerAttemptPeerReceiptFlush,
    timerAttemptFailureProbe,
    timerAttemptApplicationSessionRecovery,
    timerAttemptIsolationGrace,
    timerAttemptIsolationDrain,
    TimerSpec (..),
    absoluteTimerSpec,
    timerSpecAbsoluteDeadline,
    TimerOutcome (..),
  )
where

import Data.Word (Word64)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Domain.Membership (HeraldFailureProbeId)
import Eclips.Herald.Application.Recovery.Internal
  ( ApplicationSessionRecoveryGeneration,
  )
import Eclips.Herald.Application.Session.Internal (ApplicationSessionId)
import Eclips.Herald.Isolation.Internal
  ( IsolationDrainId,
    IsolationGraceGeneration,
  )
import Eclips.Herald.PeerLiveness.Internal (PeerRecoveryGeneration)
import Eclips.Herald.Time (MonotonicInstant)

-- | Owner-allocated generation of one replaceable physical timer attempt.
newtype TimerAttemptGeneration = TimerAttemptGeneration Word64
  deriving stock (Eq, Ord, Show)

firstTimerAttemptGeneration :: TimerAttemptGeneration
firstTimerAttemptGeneration = TimerAttemptGeneration 1

-- | Advance one finite-run timer attempt. Exhaustion is outside profile 0.1.
nextTimerAttemptGeneration :: TimerAttemptGeneration -> TimerAttemptGeneration
nextTimerAttemptGeneration (TimerAttemptGeneration generation) =
  TimerAttemptGeneration (generation + 1)

timerAttemptGenerationWord64 :: TimerAttemptGeneration -> Word64
timerAttemptGenerationWord64 (TimerAttemptGeneration generation) = generation

-- | One exact logical timer identity and its replaceable attempt generation.
--
-- The runtime treats this value opaquely and returns the exact value in a
-- 'TimerObserved' input. Future logical timer families extend this closed sum.
data TimerAttempt
  = PeerRecoveryTimerAttempt
      HeraldEpoch
      PeerRecoveryGeneration
      TimerAttemptGeneration
  | PeerReceiptFlushTimerAttempt
      HeraldEpoch
      TimerAttemptGeneration
  | FailureProbeTimerAttempt
      HeraldFailureProbeId
      TimerAttemptGeneration
  | ApplicationSessionRecoveryTimerAttempt
      ApplicationSessionId
      ApplicationSessionRecoveryGeneration
      TimerAttemptGeneration
  | IsolationGraceTimerAttempt
      IsolationGraceGeneration
      TimerAttemptGeneration
  | IsolationDrainTimerAttempt
      IsolationDrainId
      TimerAttemptGeneration
  deriving stock (Eq, Ord, Show)

peerRecoveryTimerAttempt ::
  HeraldEpoch ->
  PeerRecoveryGeneration ->
  TimerAttemptGeneration ->
  TimerAttempt
peerRecoveryTimerAttempt = PeerRecoveryTimerAttempt

peerReceiptFlushTimerAttempt :: HeraldEpoch -> TimerAttemptGeneration -> TimerAttempt
peerReceiptFlushTimerAttempt = PeerReceiptFlushTimerAttempt

failureProbeTimerAttempt ::
  HeraldFailureProbeId ->
  TimerAttemptGeneration ->
  TimerAttempt
failureProbeTimerAttempt = FailureProbeTimerAttempt

applicationSessionRecoveryTimerAttempt ::
  ApplicationSessionId ->
  ApplicationSessionRecoveryGeneration ->
  TimerAttemptGeneration ->
  TimerAttempt
applicationSessionRecoveryTimerAttempt = ApplicationSessionRecoveryTimerAttempt

isolationGraceTimerAttempt ::
  IsolationGraceGeneration ->
  TimerAttemptGeneration ->
  TimerAttempt
isolationGraceTimerAttempt = IsolationGraceTimerAttempt

isolationDrainTimerAttempt ::
  IsolationDrainId ->
  TimerAttemptGeneration ->
  TimerAttempt
isolationDrainTimerAttempt = IsolationDrainTimerAttempt

timerAttemptGeneration :: TimerAttempt -> TimerAttemptGeneration
timerAttemptGeneration (PeerRecoveryTimerAttempt _ _ generation) = generation
timerAttemptGeneration (PeerReceiptFlushTimerAttempt _ generation) = generation
timerAttemptGeneration (FailureProbeTimerAttempt _ generation) = generation
timerAttemptGeneration (ApplicationSessionRecoveryTimerAttempt _ _ generation) = generation
timerAttemptGeneration (IsolationGraceTimerAttempt _ generation) = generation
timerAttemptGeneration (IsolationDrainTimerAttempt _ generation) = generation

timerAttemptPeerRecovery ::
  TimerAttempt ->
  Maybe (HeraldEpoch, PeerRecoveryGeneration, TimerAttemptGeneration)
timerAttemptPeerRecovery = \case
  PeerRecoveryTimerAttempt peer recovery attempt ->
    Just (peer, recovery, attempt)
  PeerReceiptFlushTimerAttempt {} -> Nothing
  FailureProbeTimerAttempt {} -> Nothing
  ApplicationSessionRecoveryTimerAttempt {} -> Nothing
  IsolationGraceTimerAttempt {} -> Nothing
  IsolationDrainTimerAttempt {} -> Nothing

timerAttemptPeerReceiptFlush :: TimerAttempt -> Maybe (HeraldEpoch, TimerAttemptGeneration)
timerAttemptPeerReceiptFlush = \case
  PeerReceiptFlushTimerAttempt peer attempt -> Just (peer, attempt)
  _ -> Nothing

timerAttemptFailureProbe ::
  TimerAttempt ->
  Maybe (HeraldFailureProbeId, TimerAttemptGeneration)
timerAttemptFailureProbe = \case
  PeerReceiptFlushTimerAttempt {} -> Nothing
  FailureProbeTimerAttempt probe attempt -> Just (probe, attempt)
  PeerRecoveryTimerAttempt {} -> Nothing
  ApplicationSessionRecoveryTimerAttempt {} -> Nothing
  IsolationGraceTimerAttempt {} -> Nothing
  IsolationDrainTimerAttempt {} -> Nothing

timerAttemptApplicationSessionRecovery ::
  TimerAttempt ->
  Maybe
    ( ApplicationSessionId,
      ApplicationSessionRecoveryGeneration,
      TimerAttemptGeneration
    )
timerAttemptApplicationSessionRecovery = \case
  ApplicationSessionRecoveryTimerAttempt session recovery attempt ->
    Just (session, recovery, attempt)
  PeerRecoveryTimerAttempt {} -> Nothing
  PeerReceiptFlushTimerAttempt {} -> Nothing
  FailureProbeTimerAttempt {} -> Nothing
  IsolationGraceTimerAttempt {} -> Nothing
  IsolationDrainTimerAttempt {} -> Nothing

timerAttemptIsolationGrace ::
  TimerAttempt ->
  Maybe (IsolationGraceGeneration, TimerAttemptGeneration)
timerAttemptIsolationGrace = \case
  IsolationGraceTimerAttempt generation attempt ->
    Just (generation, attempt)
  PeerRecoveryTimerAttempt {} -> Nothing
  PeerReceiptFlushTimerAttempt {} -> Nothing
  FailureProbeTimerAttempt {} -> Nothing
  ApplicationSessionRecoveryTimerAttempt {} -> Nothing
  IsolationDrainTimerAttempt {} -> Nothing

timerAttemptIsolationDrain ::
  TimerAttempt ->
  Maybe (IsolationDrainId, TimerAttemptGeneration)
timerAttemptIsolationDrain = \case
  IsolationDrainTimerAttempt drain attempt ->
    Just (drain, attempt)
  PeerRecoveryTimerAttempt {} -> Nothing
  PeerReceiptFlushTimerAttempt {} -> Nothing
  FailureProbeTimerAttempt {} -> Nothing
  ApplicationSessionRecoveryTimerAttempt {} -> Nothing
  IsolationGraceTimerAttempt {} -> Nothing

-- | Absolute monotonic deadline for one logical timer attempt.
newtype TimerSpec = TimerSpec MonotonicInstant
  deriving stock (Eq, Ord, Show)

absoluteTimerSpec :: MonotonicInstant -> TimerSpec
absoluteTimerSpec = TimerSpec

timerSpecAbsoluteDeadline :: TimerSpec -> MonotonicInstant
timerSpecAbsoluteDeadline (TimerSpec deadline) = deadline

-- | Runtime observation for one exact timer attempt.
data TimerOutcome
  = TimerFired MonotonicInstant
  deriving stock (Eq, Ord, Show)
