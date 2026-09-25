{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Atomic receipt admission, piggybacking and bounded idle flushes.
module Eclips.Herald.UseCase.PeerDelivery
  ( mergeProgress,
    evidenceReceived,
    recordEvidence,
    retryReceipt,
    reconnectEvidence,
    markSeeded,
    normalizeEffects,
    observeTimer,
  )
where

import Control.Monad (foldM)
import Data.Maybe (isJust)
import Eclips.Domain.Alignment (AlignmentDeliverySequence)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Herald.Alignment.Protocol (AlignmentControl)
import Eclips.Herald.Discovery (PeerBinding, peerBindingRemoteHeraldEpoch)
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
    emptyEffectBatch,
    orderedEffectBatch,
    singletonEffectBatch,
  )
import Eclips.Herald.Input (PeerControl (..))
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerDelivery qualified as Peers
import Eclips.Herald.PeerDelivery.State qualified as Delivery
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (..),
    HeraldTransitionInvariantViolation (..),
  )
import Eclips.Herald.Startup.State
  ( HeraldPhase (..),
    HeraldState,
    heraldPhase,
    replaceStartupPeerDeliveryState,
    startupDiscoveryState,
    startupIsolationState,
    startupLastObservedTime,
    startupOracleProjectionState,
    startupPeerDeliveryState,
    startupTakeoverTarget,
  )
import Eclips.Herald.Time (monotonicInstant, monotonicInstantWord64)
import Eclips.Herald.Timer.Internal
  ( TimerAttempt,
    TimerOutcome (..),
    absoluteTimerSpec,
    nextTimerAttemptGeneration,
    peerReceiptFlushTimerAttempt,
    timerAttemptPeerReceiptFlush,
    timerSpecAbsoluteDeadline,
  )
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement, emptyReceiptRetirement)
import Eclips.Public.Types.Timing (deriveTimingPolicy, timingReconnectInitialMicroseconds)

mergeProgress :: PeerBinding -> ReceiptRetirement -> HeraldState -> Either Delivery.DeliveryProblem HeraldState
mergeProgress binding progress state
  | Discovery.currentPeerBinding remote (startupDiscoveryState state) /= Just binding = Right state
  | progress == emptyReceiptRetirement = Right state
  | otherwise = do
      delivery <- Delivery.acknowledge progress peer.delivery
      pure (install binding (peer {Peers.delivery = delivery}) state)
  where
    remote = peerBindingRemoteHeraldEpoch binding
    peer = lookupPeer binding state

evidenceReceived :: PeerBinding -> AlignmentDeliverySequence -> HeraldState -> Bool
evidenceReceived binding sequenceNumber state =
  not (snd (Delivery.receive sequenceNumber (lookupPeer binding state).delivery))

-- Duplicates request another cumulative receipt, repairing a lost flush without
-- reinstalling their old semantic payload or retaining a per-item receipt.
recordEvidence :: PeerBinding -> AlignmentDeliverySequence -> HeraldState -> HeraldState
recordEvidence binding sequenceNumber state =
  let peer = lookupPeer binding state
      (delivery, _) = Delivery.receive sequenceNumber peer.delivery
   in install binding (peer {Peers.delivery = delivery, Peers.dirty = True}) state

-- A live publication writer may defer or fail without losing its binding.
-- Reoffer cumulative progress if its only piggyback might not have been sent.
-- The caller supplies only a newly scheduled retry, so stale or repeated
-- physical outcomes cannot restart an already completed receipt flush.
retryReceipt :: HeraldEpoch -> HeraldState -> HeraldState
retryReceipt remote state
  | Delivery.receiptProgress peer.delivery == emptyReceiptRetirement || peer.dirty = state
  | otherwise = replaceStartupPeerDeliveryState (Peers.replacePeer remote (peer {Peers.dirty = True}) owner) state
  where
    owner = startupPeerDeliveryState state
    peer = Peers.peerState remote owner

reconnectEvidence :: PeerBinding -> [AlignmentControl] -> HeraldState -> [PeerControl]
reconnectEvidence binding roots state
  | peer.seeded =
      [PeerAlignmentEvidenceDelivered sequenceNumber payload | (sequenceNumber, payload) <- Delivery.outstanding peer.delivery]
  | otherwise = fmap PeerAlignmentControl roots
  where
    peer = lookupPeer binding state

markSeeded :: PeerBinding -> HeraldState -> HeraldState
markSeeded binding state = install binding ((lookupPeer binding state) {Peers.seeded = True}) state

normalizeEffects :: HeraldState -> EffectBatch -> Either HeraldInvariantFault (HeraldState, EffectBatch)
normalizeEffects initial effects = do
  (state, reversed) <- foldM normalize (initial, []) (effectBatchMembers effects)
  let (final, timers) = foldl armOrCancel (state, []) (Peers.peers (startupPeerDeliveryState state))
  pure (final, orderedEffectBatch (reverse reversed <> reverse timers))
  where
    normalize (state, accumulated) effect = case effect of
      QueuePeerAlignmentEvidence remote evidence -> do
        key <- maybe fault Right (Peers.evidenceKey evidence)
        let owner = startupPeerDeliveryState state
            peer = Peers.peerState remote owner
        (delivery, sequenceNumber) <- either (const fault) Right (Delivery.offer key evidence peer.delivery)
        let prepared = replaceStartupPeerDeliveryState (Peers.replacePeer remote (peer {Peers.delivery = delivery}) owner) state
        case Discovery.currentPeerBinding remote (startupDiscoveryState prepared) of
          Nothing -> Right (prepared, accumulated)
          Just binding -> normalize (prepared, accumulated) (SendPeerControl binding (PeerAlignmentEvidenceDelivered sequenceNumber evidence))
      SendPeerControl binding control -> do
        (prepared, delivered) <- case control of
          PeerAlignmentControl evidence | Just key <- Peers.evidenceKey evidence -> do
            let peer = lookupPeer binding state
            (delivery, sequenceNumber) <- either (const fault) Right (Delivery.offer key evidence peer.delivery)
            pure (install binding (peer {Peers.delivery = delivery}) state, PeerAlignmentEvidenceDelivered sequenceNumber evidence)
          _ -> Right (state, control)
        let force = case delivered of PeerStreamResumeOffered {} -> True; PeerStreamResumeAccepted {} -> True; _ -> False
            (successor, progress, cancelled) = advertise force binding prepared
            emitted = if progress == emptyReceiptRetirement then SendPeerControl binding delivered else SendPeerControlWithProgress binding progress delivered
        pure (successor, emitted : reverse cancelled <> accumulated)
      SendPeerItem binding attempt ->
        let (successor, progress, cancelled) = advertise False binding state
            emitted = if progress == emptyReceiptRetirement then SendPeerItem binding attempt else SendPeerItemWithProgress binding progress attempt
         in Right (successor, emitted : reverse cancelled <> accumulated)
      -- Application receipt ingress may recursively dispatch one step. Its
      -- already-normalized effects must not allocate another delivery position.
      SendPeerControlWithProgress {} -> Right (state, effect : accumulated)
      SendPeerItemWithProgress {} -> Right (state, effect : accumulated)
      CancelPeerDestination remote ->
        let owner = startupPeerDeliveryState state
            cancelled = maybe [] (\(attempt, _) -> [CancelTimer attempt]) (Peers.peerState remote owner).timer
         in Right (replaceStartupPeerDeliveryState (Peers.retirePeer remote owner) state, effect : reverse cancelled <> accumulated)
      _ -> Right (state, effect : accumulated)

    advertise force binding state =
      let peer = lookupPeer binding state
          cancelled = maybe [] (\(attempt, _) -> [CancelTimer attempt]) peer.timer
          progress = if force || peer.dirty then Delivery.receiptProgress peer.delivery else emptyReceiptRetirement
          successor = if peer.dirty || isJust peer.timer then install binding (peer {Peers.dirty = False, Peers.timer = Nothing}) state else state
       in (successor, progress, cancelled)

    armOrCancel (state, effectsSoFar) (remote, _) =
      let owner = startupPeerDeliveryState state
          peer = Peers.peerState remote owner
          current = Discovery.currentPeerBinding remote (startupDiscoveryState state)
          active = deliveryActive state && isJust current
       in if not active
            then case peer.timer of
              Nothing -> (state, effectsSoFar)
              Just (attempt, _) -> (replaceStartupPeerDeliveryState (Peers.replacePeer remote (peer {Peers.timer = Nothing}) owner) state, CancelTimer attempt : effectsSoFar)
            else case (peer.dirty, peer.timer) of
              (True, Nothing) ->
                let attempt = peerReceiptFlushTimerAttempt remote peer.nextAttempt
                    delay = timingReconnectInitialMicroseconds (deriveTimingPolicy (startupTakeoverTarget state))
                    specification = absoluteTimerSpec (monotonicInstant (monotonicInstantWord64 (startupLastObservedTime state) + delay))
                    updated = peer {Peers.timer = Just (attempt, specification), Peers.nextAttempt = nextTimerAttemptGeneration peer.nextAttempt}
                 in (replaceStartupPeerDeliveryState (Peers.replacePeer remote updated owner) state, ArmTimer attempt specification : effectsSoFar)
              _ -> (state, effectsSoFar)

observeTimer :: TimerAttempt -> TimerOutcome -> HeraldState -> Maybe (HeraldState, EffectBatch)
observeTimer attempt (TimerFired firedAt) state = do
  (remote, _) <- timerAttemptPeerReceiptFlush attempt
  let owner = startupPeerDeliveryState state
      peer = Peers.peerState remote owner
  pure $ case peer.timer of
    Just (expected, specification)
      | expected == attempt ->
          if firedAt < timerSpecAbsoluteDeadline specification
            then (state, singletonEffectBatch (ArmTimer attempt specification))
            else
              let successor = replaceStartupPeerDeliveryState (Peers.replacePeer remote (peer {Peers.timer = Nothing}) owner) state
               in case Discovery.currentPeerBinding remote (startupDiscoveryState state) of
                    Just binding | peer.dirty && deliveryActive state -> (successor, singletonEffectBatch (SendPeerControl binding PeerAlignmentDeliveryProgress))
                    _ -> (successor, emptyEffectBatch)
    _ -> (state, emptyEffectBatch)

deliveryActive :: HeraldState -> Bool
deliveryActive state =
  heraldPhase state == HeraldServing
    && OracleProjection.oracleViewLocalHeraldIsCurrent (OracleProjection.oracleView (startupOracleProjectionState state))
    && Isolation.isolationWitnessPhase (Isolation.stateWitness (startupIsolationState state))
      `notElem` [Isolation.IsolationReadOnlyDrainView, Isolation.IsolationTerminalView]

lookupPeer :: PeerBinding -> HeraldState -> Peers.PeerState
lookupPeer binding = Peers.peerState (peerBindingRemoteHeraldEpoch binding) . startupPeerDeliveryState

install :: PeerBinding -> Peers.PeerState -> HeraldState -> HeraldState
install binding peer state = replaceStartupPeerDeliveryState (Peers.replacePeer (peerBindingRemoteHeraldEpoch binding) peer (startupPeerDeliveryState state)) state

fault :: Either HeraldInvariantFault a
fault = Left (HeraldTransitionInvariant HeraldPeerTransitionContradiction)
