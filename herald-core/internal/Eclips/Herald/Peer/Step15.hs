-- | Checked semantic forms for Step-15 membership-sensitive peer exchange.
--
-- Fresh direct probes are constructors of the live peer-control path through
-- the sole EPRP adapter. The small qualified-Hello wrapper remains independent
-- evidence that Hello and probes name one exact membership generation.
module Eclips.Herald.Peer.Step15
  ( MembershipPeerHello,
    membershipPeerHello,
    membershipPeerHelloValue,
    membershipPeerHelloGeneration,
    DirectFailureProbeRequest,
    directFailureProbeRequest,
    directFailureProbeRequestProbe,
    directFailureProbeRequestTarget,
    directFailureProbeRequestGeneration,
    directFailureProbeRequestVoterConfiguration,
    DirectFailureProbeResponse,
    DirectFailureProbeResult (..),
    directFailureProbeResponse,
    directFailureProbeResponseRequest,
    directFailureProbeResponseResult,
  )
where

import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Domain.Membership
  ( HeraldFailureProbeId,
    HeraldMembershipGenerationId,
  )
import Eclips.Herald.Discovery (PeerHello)
import Eclips.Oracle.Voter (VoterConfigurationId)

data MembershipPeerHello
  = MembershipPeerHello PeerHello HeraldMembershipGenerationId
  deriving stock (Eq, Show)

membershipPeerHello ::
  PeerHello ->
  HeraldMembershipGenerationId ->
  MembershipPeerHello
membershipPeerHello = MembershipPeerHello

membershipPeerHelloValue :: MembershipPeerHello -> PeerHello
membershipPeerHelloValue (MembershipPeerHello hello _) = hello

membershipPeerHelloGeneration ::
  MembershipPeerHello ->
  HeraldMembershipGenerationId
membershipPeerHelloGeneration (MembershipPeerHello _ generation) = generation

data DirectFailureProbeRequest
  = DirectFailureProbeRequest
      HeraldFailureProbeId
      HeraldEpoch
      HeraldMembershipGenerationId
      VoterConfigurationId
  deriving stock (Eq, Ord, Show)

directFailureProbeRequest ::
  HeraldFailureProbeId ->
  HeraldEpoch ->
  HeraldMembershipGenerationId ->
  VoterConfigurationId ->
  DirectFailureProbeRequest
directFailureProbeRequest = DirectFailureProbeRequest

directFailureProbeRequestProbe ::
  DirectFailureProbeRequest ->
  HeraldFailureProbeId
directFailureProbeRequestProbe (DirectFailureProbeRequest probe _ _ _) = probe

directFailureProbeRequestTarget :: DirectFailureProbeRequest -> HeraldEpoch
directFailureProbeRequestTarget (DirectFailureProbeRequest _ target _ _) = target

directFailureProbeRequestGeneration ::
  DirectFailureProbeRequest ->
  HeraldMembershipGenerationId
directFailureProbeRequestGeneration (DirectFailureProbeRequest _ _ generation _) = generation

directFailureProbeRequestVoterConfiguration :: DirectFailureProbeRequest -> VoterConfigurationId
directFailureProbeRequestVoterConfiguration (DirectFailureProbeRequest _ _ _ configuration) = configuration

data DirectFailureProbeResult
  = DirectFailureProbeReachable
  | DirectFailureProbeUnreachable
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data DirectFailureProbeResponse
  = DirectFailureProbeResponse
      DirectFailureProbeRequest
      DirectFailureProbeResult
  deriving stock (Eq, Ord, Show)

directFailureProbeResponse ::
  DirectFailureProbeRequest ->
  DirectFailureProbeResult ->
  DirectFailureProbeResponse
directFailureProbeResponse = DirectFailureProbeResponse

directFailureProbeResponseRequest ::
  DirectFailureProbeResponse ->
  DirectFailureProbeRequest
directFailureProbeResponseRequest (DirectFailureProbeResponse request _) = request

directFailureProbeResponseResult ::
  DirectFailureProbeResponse ->
  DirectFailureProbeResult
directFailureProbeResponseResult (DirectFailureProbeResponse _ result) = result
