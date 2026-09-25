{-# LANGUAGE CPP #-}

module PublicPeerDispatch where

#if defined(MINT_PEER_DISPATCH_TICKET)
import Eclips.Herald.PeerDispatch (PeerDispatchTicket (..))

ownerOnlyConstructor = PeerDispatchTicket
#elif defined(MINT_PEER_DISPATCH_ATTEMPT)
import Eclips.Herald.PeerDispatch (PeerDispatchAttempt (..))

ownerOnlyConstructor = PeerDispatchAttempt
#else
import Eclips.Herald.Discovery (PeerBinding)
import Eclips.Herald.EffectBatch (HeraldEffect (..))
import Eclips.Herald.Input
  ( HeraldInputBody (PeerInput, RuntimeObserved),
    PeerIngress (PeerDispatchSelected),
    RuntimeObservation (PeerDispatchObserved),
    peerPublicationReceived,
  )
import Eclips.Herald.PeerDispatch
  ( PeerDispatchOutcome (PeerDispatchWritten),
    peerDispatchAttemptItem,
    peerDispatchTicketDestinationHeraldEpoch,
  )

-- A downstream shell may return an owner-minted attempt with its own physical
-- write outcome without naming or constructing the hidden payload.
reportWritten :: HeraldEffect -> Maybe HeraldInputBody
reportWritten effect = case effect of
  SendPeerItem _ attempt ->
    Just (RuntimeObserved (PeerDispatchObserved attempt PeerDispatchWritten))
  _ -> Nothing

-- The scheduler likewise returns the opaque ticket it received together with
-- the shell's current logical binding; it never reconstructs either correlation.
selectScheduled :: PeerBinding -> HeraldEffect -> Maybe HeraldInputBody
selectScheduled binding effect = case effect of
  SchedulePeerDispatch ticket ->
    Just (PeerInput (PeerDispatchSelected ticket binding))
  _ -> Nothing

-- The scheduler can index opaque tickets by destination without learning their
-- direction or generation.
sameScheduledDestination :: HeraldEffect -> HeraldEffect -> Bool
sameScheduledDestination left right = case (left, right) of
  (SchedulePeerDispatch leftTicket, SchedulePeerDispatch rightTicket) ->
    peerDispatchTicketDestinationHeraldEpoch leftTicket
      == peerDispatchTicketDestinationHeraldEpoch rightTicket
  _ -> False

-- A typed in-memory peer lane may relay the selected publication without any
-- payload, sequence, digest, or item constructor becoming public.
relaySelected :: PeerBinding -> HeraldEffect -> Maybe HeraldInputBody
relaySelected binding effect = case effect of
  SendPeerItem _ attempt ->
    Just
      ( PeerInput
          (peerPublicationReceived binding (peerDispatchAttemptItem attempt))
      )
  _ -> Nothing
#endif
