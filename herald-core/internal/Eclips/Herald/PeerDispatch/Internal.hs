-- | Private representation bridge for the public in-memory publication relay.
--
-- The safe facade exposes neither constructor nor payload projection.  The
-- input vocabulary uses this sibling module only to recover the existing
-- package-private sequenced item when constructing kernel ingress.
module Eclips.Herald.PeerDispatch.Internal
  ( PeerLogicalItem,
    peerPublicationItem,
    peerLogicalItem,
    peerLogicalSequencedItem,
  )
where

import Eclips.Herald.PeerPayload
  ( PeerLogicalPayload (..),
  )
import Eclips.Herald.PeerPublication (PeerPublication)
import Eclips.Herald.PeerStream
  ( SequencedItem,
    sequencedItem,
    sequencedItemDigest,
    sequencedItemDirection,
    sequencedItemPayload,
    sequencedItemSequence,
  )

newtype PeerLogicalItem
  = PeerLogicalItem (SequencedItem PeerLogicalPayload)
  deriving stock (Eq)

instance Show PeerLogicalItem where
  show _ = "PeerLogicalItem"

peerPublicationItem :: SequencedItem PeerPublication -> PeerLogicalItem
peerPublicationItem item =
  PeerLogicalItem
    ( sequencedItem
        (sequencedItemDirection item)
        (sequencedItemSequence item)
        (sequencedItemDigest item)
        (PeerLogicalPublication (sequencedItemPayload item))
    )

peerLogicalItem :: SequencedItem PeerLogicalPayload -> PeerLogicalItem
peerLogicalItem = PeerLogicalItem

peerLogicalSequencedItem :: PeerLogicalItem -> SequencedItem PeerLogicalPayload
peerLogicalSequencedItem (PeerLogicalItem item) = item
