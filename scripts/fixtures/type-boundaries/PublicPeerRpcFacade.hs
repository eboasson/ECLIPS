module PublicPeerRpcFacade
  ( projectCheckedHello,
  )
where

import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.Topology (MemberSetDigest)
import Eclips.Herald.Discovery (PeerHello)
import Eclips.Herald.Peer.RPC
  ( PeerRpcProjectionFault,
    projectPeerHello,
  )
import Eclips.Protocol.Peer.Types (PeerEnvelope)

projectCheckedHello ::
  HeraldMembershipGenerationId ->
  MemberSetDigest ->
  PeerHello ->
  Either PeerRpcProjectionFault PeerEnvelope
projectCheckedHello = projectPeerHello
