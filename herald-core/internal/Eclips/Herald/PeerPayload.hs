-- | The closed payload of one logical ordered Herald-to-Herald stream.
--
-- Publications, route cutovers, and disappearance probes share
-- this stream. Each marker follows every preceding accepted publication.
module Eclips.Herald.PeerPayload
  ( PeerLogicalPayload (..),
    peerLogicalPayloadDigest,
    peerLogicalPayloadItem,
    peerLogicalPublicationItem,
    peerLogicalDisappearanceProbeMarkerItem,
    peerLogicalAssignmentReceipt,
    peerLogicalAssignmentCompleted,
  )
where

import Eclips.Domain.Alignment (routeCutoverEvidenceDigestBytes)
import Eclips.Domain.Disappearance (DisappearanceProbeId, DisappearanceSubjectMembershipCoordinate)
import Eclips.Herald.Alignment.Protocol
  ( AlignmentRouteCutoverMarker,
    alignmentRouteCutoverMarkerEvidenceDigest,
  )
import Eclips.Herald.Disappearance.Protocol qualified as Disappearance
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    peerPublicationDigest,
  )
import Eclips.Herald.PeerStream
  ( AssignmentReceipt,
    PeerItem,
    PeerItemDigest,
    SequencedItem,
    assignmentReceipt,
    mkPeerItemDigest,
    peerItem,
    sequencedItemDigest,
    sequencedItemDirection,
    sequencedItemSequence,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream

data PeerLogicalPayload
  = PeerLogicalPublication PeerPublication
  | PeerLogicalRouteCutover AlignmentRouteCutoverMarker
  | PeerLogicalDisappearanceProbeMarker Disappearance.DisappearancePublicationMarker
  deriving stock (Eq, Show)

peerLogicalPayloadDigest :: PeerLogicalPayload -> PeerItemDigest
peerLogicalPayloadDigest = \case
  PeerLogicalPublication publication -> peerPublicationDigest publication
  PeerLogicalRouteCutover marker ->
    either
      (error . ("route-cutover digest invariant: " <>) . show)
      id
      ( mkPeerItemDigest
          ( routeCutoverEvidenceDigestBytes
              (alignmentRouteCutoverMarkerEvidenceDigest marker)
          )
      )
  PeerLogicalDisappearanceProbeMarker marker -> Disappearance.disappearancePublicationMarkerDigest marker

peerLogicalPayloadItem :: PeerLogicalPayload -> PeerItem PeerLogicalPayload
peerLogicalPayloadItem payload = peerItem (peerLogicalPayloadDigest payload) payload

peerLogicalPublicationItem :: PeerPublication -> PeerItem PeerLogicalPayload
peerLogicalPublicationItem = peerLogicalPayloadItem . PeerLogicalPublication

-- | Allocate the disappearance cut in the same logical stream as publications.
peerLogicalDisappearanceProbeMarkerItem ::
  DisappearanceProbeId ->
  DisappearanceSubjectMembershipCoordinate ->
  PeerStream.SequenceBoundPeerItem PeerLogicalPayload
peerLogicalDisappearanceProbeMarkerItem probe coordinate =
  PeerStream.sequenceBoundPeerItem $ \direction sequenceNumber ->
    peerLogicalPayloadItem
      ( PeerLogicalDisappearanceProbeMarker
          (Disappearance.disappearancePublicationMarker probe coordinate direction sequenceNumber)
      )

-- | Freeze the exact transport coordinate allocated to one logical item.  A
-- composed owner retains this receipt beside semantic marker state so later
-- cumulative completion cannot be confused with an older numeric prefix.
peerLogicalAssignmentReceipt ::
  SequencedItem PeerLogicalPayload -> AssignmentReceipt
peerLogicalAssignmentReceipt item =
  assignmentReceipt
    (sequencedItemDirection item)
    (sequencedItemSequence item)
    (sequencedItemDigest item)

-- | Whether the peer completed this previously admitted immutable assignment.
-- Its semantic owner retains the checked identity; compact direction/sequence
-- completion survives payload release and reconnect for post-marker Live/Ack.
peerLogicalAssignmentCompleted ::
  AssignmentReceipt ->
  PeerStream.State PeerLogicalPayload ->
  Either PeerStream.PeerStreamProblem Bool
peerLogicalAssignmentCompleted receipt state =
  PeerStream.assignmentCompletionKnown receipt state
