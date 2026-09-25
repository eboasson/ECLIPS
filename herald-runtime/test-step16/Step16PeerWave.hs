{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Test-only closure of a finite peer-work wave, observed through immutable
-- trace ledgers. A destination's Live/history result does not by itself prove
-- that all setup controls reached every participant. Closing the causal wave
-- prevents an unrelated late subscription fact from changing a disappearance
-- cut after the test removes its last carrier.
module Step16PeerWave
  ( PeerWaveCapture,
    capturePeerWave,
    awaitPeerWave,
    snapshotPendingPeerWave,
    pendingFinitePeerWave,
  ) where

import Control.Concurrent.STM (STM, atomically, check)
import Data.Map.Strict qualified as Map
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Herald.Discovery (peerBindingRemoteHeraldEpoch)
import Eclips.Herald.EffectBatch
  ( HeraldEffect (SchedulePeerDispatch),
    effectBatchMembers,
  )
import Eclips.Herald.Input
  ( HeraldInputBody (PeerInput, RuntimeObserved),
    PeerControl (PeerDirectFailureProbeRequested, PeerDirectFailureProbeResponded),
    PeerIngress (PeerDispatchSelected),
    RuntimeObservation (PeerDispatchObserved),
    inputBody,
  )
import Eclips.Herald.Peer.RPC (projectPeerControl, projectPeerLogicalAttempt)
import Eclips.Herald.PeerDispatch
  ( peerDispatchTicketDestinationHeraldEpoch,
    peerLogicalDispatchAttemptItem,
  )
import Eclips.Herald.Runtime.Internal.Trace
  ( KernelTraceStep (KernelTraceStepped),
    RuntimeTraceEvent (KernelEvent),
    TraceLedger,
    snapshotTraceLedger,
  )
import PeerEvidence (data SemanticPeerControlReceived, data SemanticPeerPublicationReceived, data SemanticSendPeerControl, data SemanticSendPeerItem)

-- Retain complete deployment ledgers, never kernel state. Mesh visibility can
-- precede routing of other effects from the same Hello transition. Starting a
-- trace suffix there would omit already-emitted, still-queued setup work and
-- could also omit earlier receipts for equal control reoffers.
newtype PeerWaveCapture = PeerWaveCapture [(HeraldEpoch, TraceLedger)]

capturePeerWave :: [(HeraldEpoch, TraceLedger)] -> PeerWaveCapture
capturePeerWave = PeerWaveCapture

-- One STM transaction reads every participant's ledger. It wakes only when
-- evidence changes; there is no polling delay or inference from elapsed quiet.
awaitPeerWave :: PeerWaveCapture -> IO ()
awaitPeerWave capture = atomically $ do
  pending <- pendingPeerWave capture
  check (null pending)

snapshotPendingPeerWave :: PeerWaveCapture -> IO [String]
snapshotPendingPeerWave = atomically . pendingPeerWave

pendingPeerWave :: PeerWaveCapture -> STM [String]
pendingPeerWave (PeerWaveCapture owners) = pendingFinitePeerWave <$> traverse snapshot owners
  where
    snapshot (epoch, ledger) = do
      events <- snapshotTraceLedger ledger
      pure (epoch, events)

-- Every announced dispatch must select; every sent item must have both its
-- source outcome and destination input; every distinct finite control fact must
-- have its destination input. Kernel effect batches include follow-up work in
-- the same receiving step before its shell outbox routes it. With stable peer
-- bindings, equal control replays are covered by the same semantic receipt.
-- Time-driven failure probes and peers outside the supplied participant set
-- are excluded. This is semantic readiness, not physical-write accounting.
pendingFinitePeerWave :: [(HeraldEpoch, [RuntimeTraceEvent])] -> [String]
pendingFinitePeerWave owners = concatMap pending owners
  where
    inputs events = [inputBody input | KernelEvent _ (KernelTraceStepped _ _ input _) <- events]
    effects events = [effect | KernelEvent _ (KernelTraceStepped _ _ _ (Right batch)) <- events, effect <- effectBatchMembers batch]
    inputTable = Map.fromList [(epoch, inputs events) | (epoch, events) <- owners]
    remoteInputs epoch = Map.findWithDefault [] epoch inputTable
    participant epoch = epoch `elem` fmap fst owners
    pending (source, events) = concatMap (pendingEffect source (inputs events)) (effects events)
    pendingEffect source localInputs = \case
      SchedulePeerDispatch ticket
        | participant (peerDispatchTicketDestinationHeraldEpoch ticket),
          ticket `notElem` [actual | PeerInput (PeerDispatchSelected actual _) <- localInputs] ->
            ["unselected dispatch " <> show (source, ticket)]
      SemanticSendPeerItem binding attempt
        | participant (peerBindingRemoteHeraldEpoch binding) ->
            [ "unreceived item " <> show (source, projectPeerLogicalAttempt attempt)
            | (source, peerLogicalDispatchAttemptItem attempt)
                `notElem` [(peerBindingRemoteHeraldEpoch receivedBinding, item) | PeerInput (SemanticPeerPublicationReceived receivedBinding item) <- remoteInputs (peerBindingRemoteHeraldEpoch binding)]
            ]
              <> [ "unobserved item outcome " <> show (source, projectPeerLogicalAttempt attempt)
                 | attempt `notElem` [actual | RuntimeObserved (PeerDispatchObserved actual _) <- localInputs]
                 ]
      SemanticSendPeerControl binding control
        | participant (peerBindingRemoteHeraldEpoch binding),
          finiteControl control,
          (source, control)
            `notElem` [(peerBindingRemoteHeraldEpoch receivedBinding, received) | PeerInput (SemanticPeerControlReceived receivedBinding received) <- remoteInputs (peerBindingRemoteHeraldEpoch binding)] ->
            ["unreceived control " <> show (source, projectPeerControl control)]
      _ -> []
    finiteControl PeerDirectFailureProbeRequested {} = False
    finiteControl PeerDirectFailureProbeResponded {} = False
    finiteControl _ = True
