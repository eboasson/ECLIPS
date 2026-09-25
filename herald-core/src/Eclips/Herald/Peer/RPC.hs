-- | Pure runtime-safe admission and projection for the Herald-peer lane.
--
-- Protocol decoding establishes structural shape.  This adapter reconstructs
-- typed peer-owner values and immutable publication integrity while leaving all
-- stateful membership, binding, placement, and stream admission to
-- @stepHerald@.
module Eclips.Herald.Peer.RPC
  ( PeerRpcAdmissionError (..),
    PeerRpcProjectionFault (..),
    CandidatePeerContext (..),
    CandidatePeerInbound (..),
    EstablishedPeerInbound (..),
    admitCandidatePeerEnvelope,
    admitEstablishedPeerEnvelope,
    admitTerminalSourceUnionOccurrenceClaim,
    projectPeerHello,
    projectPeerControl,
    projectPeerControlWithProgress,
    projectPeerLogicalAttempt,
    projectPeerLogicalAttemptWithProgress,
  )
where

import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.Topology (MemberSetDigest)
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.Input (PeerControl)
import Eclips.Herald.Peer.RPC.Internal qualified as Internal
import Eclips.Herald.PeerDispatch
  ( PeerLogicalAttempt,
    PeerLogicalItem,
  )
import Eclips.Protocol.Peer.Types qualified as Protocol
import Eclips.Public.Types.ReceiptRetirement (ReceiptRetirement)

-- | A structurally decoded peer envelope cannot enter the supplied logical
-- connection phase or contradicts a refined immutable claim.
data PeerRpcAdmissionError
  = PeerRpcWrongIngressPhase
  | PeerRpcClaimContradiction
  | PeerRpcCanonicalValueContradiction
  | PeerRpcPublicationDigestMismatch
  | PeerRpcDisappearanceMarkerCoordinateMismatch
  deriving stock (Eq, Show)

-- | A checked Herald-owned value contradicted its protocol representation.
data PeerRpcProjectionFault
  = PeerRpcOwnerClaimContradiction
  | PeerRpcOwnerPublicationContradiction
  deriving stock (Eq, Show)

-- | Runtime-owned candidate context.  A remote initiation derives its complete
-- candidate tuple from the Hello; a local initiation retains the owner-minted
-- candidate and checks only its reciprocal nonce at this boundary.
data CandidatePeerContext
  = RemoteInitiatedCandidate
  | LocallyInitiatedCandidate Discovery.PeerCandidate
  deriving stock (Eq, Show)

-- | The sole candidate-phase peer input.
data CandidatePeerInbound
  = CandidatePeerHello
      Discovery.PeerCandidate
      Discovery.PeerHello
      HeraldMembershipGenerationId
      MemberSetDigest
  deriving stock (Eq, Show)

-- | The two established-phase peer inputs.
data EstablishedPeerInbound
  = EstablishedPeerControl ReceiptRetirement PeerControl
  | EstablishedPeerPublication ReceiptRetirement PeerLogicalItem
  deriving stock (Eq, Show)

-- | Admit one candidate-phase Hello.  Control and publication envelopes fail
-- closed as phase violations.
admitCandidatePeerEnvelope ::
  CandidatePeerContext ->
  Protocol.PeerEnvelope ->
  Either PeerRpcAdmissionError CandidatePeerInbound
admitCandidatePeerEnvelope context envelope = case envelope of
  Protocol.PeerHelloEnvelope dto -> do
    (hello, generation, members) <- mapAdmission (Internal.helloFromDto dto)
    candidate <- case context of
      RemoteInitiatedCandidate ->
        pure
          ( Discovery.peerCandidate
              (Discovery.peerHelloHeraldId hello)
              (Discovery.peerHelloHeraldEpoch hello)
              (Discovery.peerHelloConnectionNonce hello)
          )
      LocallyInitiatedCandidate ownerCandidate
        | Discovery.peerCandidateConnectionNonce ownerCandidate
            == Discovery.peerHelloConnectionNonce hello ->
            pure ownerCandidate
        | otherwise -> Left PeerRpcClaimContradiction
    pure (CandidatePeerHello candidate hello generation members)
  Protocol.PeerHeartbeatEnvelope _ -> Left PeerRpcWrongIngressPhase
  _ -> Left PeerRpcWrongIngressPhase

-- | Admit established control or publication traffic.  A Hello is a phase
-- violation after logical binding establishment.
admitEstablishedPeerEnvelope ::
  Protocol.PeerEnvelope ->
  Either PeerRpcAdmissionError EstablishedPeerInbound
admitEstablishedPeerEnvelope = \case
  Protocol.PeerHelloEnvelope _ -> Left PeerRpcWrongIngressPhase
  Protocol.PeerControlEnvelope progress dto ->
    EstablishedPeerControl progress <$> mapAdmission (Internal.controlFromDto dto)
  Protocol.PeerPublicationEnvelope progress dto ->
    EstablishedPeerPublication progress <$> mapAdmission (Internal.publicationFromDto dto)
  Protocol.PeerHeartbeatEnvelope _ -> Left PeerRpcWrongIngressPhase

-- | Authenticate one terminal-source union and test its exact retained
-- occurrence claim without exposing the private Graph representation. Both
-- the closed-prefix extension and the explicitly ahead set are observable
-- through this single wire-boundary query.
admitTerminalSourceUnionOccurrenceClaim ::
  Protocol.TerminalSourceUnionDto ->
  Protocol.StructuralOccurrenceIdDto ->
  Protocol.StructuralPublicationDigestClaim ->
  Either PeerRpcAdmissionError Bool
admitTerminalSourceUnionOccurrenceClaim union occurrence publicationDigest =
  mapAdmission
    ( Internal.terminalSourceUnionContainsOccurrenceClaimFromDto
        union
        occurrence
        publicationDigest
    )

-- | Project one owner-authored Hello.  Candidate correlation remains local and
-- is never serialized.
projectPeerHello ::
  HeraldMembershipGenerationId ->
  MemberSetDigest ->
  Discovery.PeerHello ->
  Either PeerRpcProjectionFault Protocol.PeerEnvelope
projectPeerHello generation members hello =
  Protocol.PeerHelloEnvelope
    <$> mapProjection (Internal.helloToDto generation members hello)

-- | Project one complete owner-authored current control arm.
projectPeerControl ::
  PeerControl ->
  Either PeerRpcProjectionFault Protocol.PeerEnvelope
projectPeerControl = projectPeerControlWithProgress mempty

projectPeerControlWithProgress :: ReceiptRetirement -> PeerControl -> Either PeerRpcProjectionFault Protocol.PeerEnvelope
projectPeerControlWithProgress progress control =
  Protocol.PeerControlEnvelope progress <$> mapProjection (Internal.controlToDto control)

-- | Project either closed logical stream payload while retaining the one
-- publication/cutover assignment order.
projectPeerLogicalAttempt ::
  PeerLogicalAttempt ->
  Either PeerRpcProjectionFault Protocol.PeerEnvelope
projectPeerLogicalAttempt = projectPeerLogicalAttemptWithProgress mempty

projectPeerLogicalAttemptWithProgress :: ReceiptRetirement -> PeerLogicalAttempt -> Either PeerRpcProjectionFault Protocol.PeerEnvelope
projectPeerLogicalAttemptWithProgress progress attempt =
  Protocol.PeerPublicationEnvelope progress
    <$> mapProjection (Internal.logicalAttemptToDto attempt)

mapAdmission ::
  Either Internal.PeerRpcBridgeError value ->
  Either PeerRpcAdmissionError value
mapAdmission = \case
  Left Internal.PeerRpcBridgeClaimContradiction ->
    Left PeerRpcClaimContradiction
  Left Internal.PeerRpcBridgeCanonicalValueContradiction ->
    Left PeerRpcCanonicalValueContradiction
  Left Internal.PeerRpcBridgePublicationDigestMismatch ->
    Left PeerRpcPublicationDigestMismatch
  Left Internal.PeerRpcBridgeDisappearanceMarkerCoordinateMismatch ->
    Left PeerRpcDisappearanceMarkerCoordinateMismatch
  Left Internal.PeerRpcBridgeOwnerPublicationContradiction ->
    Left PeerRpcClaimContradiction
  Right value -> Right value

mapProjection ::
  Either Internal.PeerRpcBridgeError value ->
  Either PeerRpcProjectionFault value
mapProjection = \case
  Left Internal.PeerRpcBridgeClaimContradiction ->
    Left PeerRpcOwnerClaimContradiction
  Left Internal.PeerRpcBridgeCanonicalValueContradiction ->
    Left PeerRpcOwnerPublicationContradiction
  Left Internal.PeerRpcBridgePublicationDigestMismatch ->
    Left PeerRpcOwnerPublicationContradiction
  Left Internal.PeerRpcBridgeDisappearanceMarkerCoordinateMismatch ->
    Left PeerRpcOwnerPublicationContradiction
  Left Internal.PeerRpcBridgeOwnerPublicationContradiction ->
    Left PeerRpcOwnerPublicationContradiction
  Right value -> Right value
