-- | Safe runtime vocabulary for logical peer dispatch.
--
-- Tickets and attempts are opaque owner-minted correlations. A runtime may
-- retain them and return them through the matching public input, but it cannot
-- construct or alter either value. Outcomes are runtime observations and are
-- therefore the one constructible part of this facade.
module Eclips.Herald.PeerDispatch
  ( PeerDispatchTicket,
    PeerDispatchAttempt,
    PeerLogicalAttempt,
    PeerLogicalItem,
    PeerDispatchOutcome (..),
    peerDispatchTicketDestinationHeraldEpoch,
    peerDispatchAttemptItem,
    peerLogicalDispatchAttemptItem,
  )
where

import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Herald.PeerDispatch.Internal
  ( PeerLogicalItem,
    peerLogicalItem,
  )
import Eclips.Herald.PeerPayload (PeerLogicalPayload)
import Eclips.Herald.PeerStream
  ( PeerDispatchAttempt,
    PeerDispatchOutcome (..),
    PeerDispatchTicket,
  )
import Eclips.Herald.PeerStream qualified as PeerStream

type PeerLogicalAttempt = PeerDispatchAttempt PeerLogicalPayload

-- | Observe only the destination needed to route one scheduled ticket.
peerDispatchTicketDestinationHeraldEpoch :: PeerDispatchTicket -> HeraldEpoch
peerDispatchTicketDestinationHeraldEpoch =
  PeerStream.streamDirectionDestination . PeerStream.peerDispatchTicketDirection

-- | Project the exact opaque logical stream item selected by the owner.
peerDispatchAttemptItem :: PeerLogicalAttempt -> PeerLogicalItem
peerDispatchAttemptItem = peerLogicalItem . PeerStream.peerDispatchAttemptItem

peerLogicalDispatchAttemptItem :: PeerLogicalAttempt -> PeerLogicalItem
peerLogicalDispatchAttemptItem =
  peerLogicalItem . PeerStream.peerDispatchAttemptItem
