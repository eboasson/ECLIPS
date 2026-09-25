{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Package-private vocabulary for the live disappearance workflow and its
-- immutable publication-stream and alignment evidence.
module Eclips.Herald.Disappearance.Protocol
  ( DisappearanceProtocolProblem (..),
    DisappearanceCandidateObservation,
    localTakeCandidateObservation,
    disappearanceCandidateSubject,
    DisappearanceOpenContext,
    disappearanceOpenContext,
    disappearanceOpenContextMembership,
    ProjectedDisappearanceProbe,
    projectedDisappearanceProbe,
    projectedProbeId,
    projectedProbeSubject,
    projectedProbeCoordinate,
    projectedProbeMembership,
    projectedProbeMembers,
    DisappearanceOpenIntention,
    disappearanceOpenIntention,
    disappearanceOpenIntentionSubject,
    disappearanceOpenIntentionCoordinate,
    DisappearancePublicationMarker,
    disappearancePublicationMarker,
    disappearancePublicationMarkerItem,
    disappearancePublicationMarkerProbe,
    disappearancePublicationMarkerCoordinate,
    disappearancePublicationMarkerDirection,
    disappearancePublicationMarkerSequence,
    disappearancePublicationMarkerDigest,
    disappearancePublicationMarkerCanonicalBytes,
    IncomingAlignmentCut,
    incomingAlignmentCut,
    incomingAlignmentCutSubscription,
    incomingAlignmentCutSource,
    incomingAlignmentCutCanonicalBytes,
    OutgoingAlignmentCut,
    outgoingAlignmentCut,
    outgoingAlignmentCutSubscription,
    outgoingAlignmentCutThroughRevision,
    outgoingAlignmentCutCanonicalBytes,
    DisappearanceAlignmentMarker,
    disappearanceAlignmentMarker,
    disappearanceAlignmentMarkerProbe,
    disappearanceAlignmentMarkerSubscription,
    disappearanceAlignmentMarkerThroughRevision,
    disappearanceAlignmentMarkerCanonicalBytes,
    CompletedIncomingAlignmentEvidence,
    completedIncomingAlignmentEvidenceForOwner,
    completedIncomingAlignmentEvidenceProbe,
    completedIncomingAlignmentEvidenceSubscription,
    completedIncomingAlignmentEvidenceSource,
    completedIncomingAlignmentEvidenceThroughRevision,
    completedIncomingAlignmentEvidenceCanonicalBytes,
    DisappearanceBlockerClass (..),
    DisappearanceBlockerWitness,
    disappearanceBlockerWitness,
    disappearanceBlockerClass,
    disappearanceBlockerDigest,
    MatchingPublicationObservation,
    matchingPublicationObservation,
    matchingPublicationSubject,
    matchingPublicationPosition,
    matchingPublicationWitness,
    DisappearanceInvalidationReason (..),
    DisappearanceInvalidationIntention,
    disappearanceInvalidationIntention,
    disappearanceInvalidationProbe,
    disappearanceInvalidationReporter,
    disappearanceInvalidationReason,
    disappearanceInvalidationWitness,
    AbsenceAttestationClass (..),
    DisappearanceAbsenceAttestation,
    disappearanceAbsenceAttestation,
    disappearanceAbsenceAttestationSubject,
    disappearanceAbsenceAttestationEntries,
    disappearanceAbsenceAttestationCanonicalBytes,
    LocalAbsenceReport,
    localAbsenceReportForOwner,
    localAbsenceReportProbe,
    localAbsenceReportClaim,
    localAbsenceReportAttestation,
    localAbsenceReportAlignmentEvidence,
    localAbsenceReportCanonicalBytes,
    DisappearanceResolveIntention,
    disappearanceResolveIntention,
    disappearanceResolveProbe,
    disappearanceResolveClaims,
    ProjectedLabelTerminalOutcome (..),
    ProjectedLabelTerminal,
    projectedLabelTerminal,
    projectedLabelTerminalDecision,
    projectedLabelTerminalObject,
    projectedLabelTerminalOutcome,
    ProjectedInvalidationCause (..),
    ProjectedAbortCause (..),
    ProjectedDisappearanceTerminal (..),
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Alignment
  ( HeraldPublicationPosition,
    HeraldPublicationPrefix (..),
    StoreRevision,
    heraldPublicationPositionWord64,
    storeRevisionWord64,
  )
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceClaim,
    DisappearanceEvidenceDigest,
    DisappearanceProbeId,
    DisappearanceResolutionOutcome,
    DisappearanceSubject,
    DisappearanceSubjectMembershipCoordinate,
    DisappearanceSubjectView (..),
    admitDisappearanceEvidenceClaim,
    deriveDisappearanceEvidenceDigest,
    disappearanceCoordinateMemberSetDigest,
    disappearanceCoordinateMembershipGenerationId,
    disappearanceCoordinateSubjectDigest,
    disappearanceEvidenceClaimCanonicalBytes,
    disappearanceEvidenceDigestBytes,
    disappearanceProbeIdBytes,
    disappearanceSubjectCanonicalBytes,
    disappearanceSubjectDigestBytes,
    disappearanceSubjectMembershipCoordinate,
    disappearanceSubjectView,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    heraldEpochBytes,
  )
import Eclips.Domain.Label (LabelRevision)
import Eclips.Domain.MemberSet (memberSetDigestBytes)
import Eclips.Domain.Membership
  ( HeraldAdmissionId,
    HeraldMembershipGeneration,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationIdBytes,
  )
import Eclips.Herald.Alignment.Protocol
  ( AlignmentSubscriptionId,
    alignmentSubscriptionIdDestinationHerald,
    alignmentSubscriptionIdSequence,
    alignmentSubscriptionSequenceWord64,
  )
import Eclips.Herald.PeerStream
  ( PeerItemDigest,
    StreamDirection,
    StreamSequence,
    mkPeerItemDigest,
    peerItem,
    streamDirectionDestination,
    streamDirectionSource,
    streamSequenceWord64,
  )
import Eclips.Herald.PeerStream.State
  ( SequenceBoundPeerItem,
    sequenceBoundPeerItem,
  )

data DisappearanceProtocolProblem
  = DisappearanceProjectedCoordinateMismatch
      DisappearanceSubjectMembershipCoordinate
      DisappearanceSubjectMembershipCoordinate
  | DisappearanceReportConstructionInvariant
  | DisappearanceAlignmentEvidenceProbeMismatch
  | DisappearanceAlignmentEvidenceSubscriptionMismatch
  | DisappearanceAlignmentEvidenceMemberMismatch
  | DisappearanceAlignmentEvidenceIncomplete
  | DisappearanceAttestationDuplicateClass AbsenceAttestationClass
  | DisappearanceAttestationClassSetMismatch
      (Set AbsenceAttestationClass)
      (Set AbsenceAttestationClass)
  | DisappearanceAttestationSubjectMismatch
  deriving stock (Eq, Show)

-- | The sole ordinary candidate-creation witness.  Its constructor name makes
-- the call site record that an accepted application-visible local take—not an
-- empty scan, timer, or disconnect—caused eligibility.
newtype DisappearanceCandidateObservation
  = LocalTakeCandidateObservation DisappearanceSubject
  deriving stock (Eq, Show)

localTakeCandidateObservation ::
  DisappearanceSubject -> DisappearanceCandidateObservation
localTakeCandidateObservation = LocalTakeCandidateObservation

disappearanceCandidateSubject ::
  DisappearanceCandidateObservation -> DisappearanceSubject
disappearanceCandidateSubject (LocalTakeCandidateObservation subject) = subject

-- | A membership whose structural base is already established.  The use-case
-- coordinator constructs this only from the narrow Oracle/Graph readiness
-- views, so absence of the value means "retain candidate, do not Open".
newtype DisappearanceOpenContext
  = DisappearanceOpenContext HeraldMembershipGeneration
  deriving stock (Eq, Show)

disappearanceOpenContext ::
  HeraldMembershipGeneration -> DisappearanceOpenContext
disappearanceOpenContext = DisappearanceOpenContext

disappearanceOpenContextMembership ::
  DisappearanceOpenContext -> HeraldMembershipGeneration
disappearanceOpenContextMembership (DisappearanceOpenContext membership) = membership

-- | Complete checked Oracle projection supplied to every captured Herald.
data ProjectedDisappearanceProbe
  = ProjectedDisappearanceProbe
      DisappearanceProbeId
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
      HeraldMembershipGeneration
  deriving stock (Eq, Show)

projectedDisappearanceProbe ::
  DisappearanceProbeId ->
  DisappearanceSubject ->
  DisappearanceSubjectMembershipCoordinate ->
  HeraldMembershipGeneration ->
  Either DisappearanceProtocolProblem ProjectedDisappearanceProbe
projectedDisappearanceProbe probe subject suppliedCoordinate membership
  | suppliedCoordinate == expectedCoordinate =
      Right
        ( ProjectedDisappearanceProbe
            probe
            subject
            suppliedCoordinate
            membership
        )
  | otherwise =
      Left
        ( DisappearanceProjectedCoordinateMismatch
            suppliedCoordinate
            expectedCoordinate
        )
  where
    expectedCoordinate =
      disappearanceSubjectMembershipCoordinate subject membership

projectedProbeId :: ProjectedDisappearanceProbe -> DisappearanceProbeId
projectedProbeId (ProjectedDisappearanceProbe probe _ _ _) = probe

projectedProbeSubject :: ProjectedDisappearanceProbe -> DisappearanceSubject
projectedProbeSubject (ProjectedDisappearanceProbe _ subject _ _) = subject

projectedProbeCoordinate ::
  ProjectedDisappearanceProbe -> DisappearanceSubjectMembershipCoordinate
projectedProbeCoordinate (ProjectedDisappearanceProbe _ _ coordinate _) = coordinate

projectedProbeMembership ::
  ProjectedDisappearanceProbe -> HeraldMembershipGeneration
projectedProbeMembership (ProjectedDisappearanceProbe _ _ _ membership) = membership

projectedProbeMembers :: ProjectedDisappearanceProbe -> NonEmpty HeraldEpoch
projectedProbeMembers =
  heraldMembershipGenerationActiveHeraldEpochs . projectedProbeMembership

data DisappearanceOpenIntention
  = DisappearanceOpenIntention
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
  deriving stock (Eq, Show)

disappearanceOpenIntention ::
  DisappearanceSubject ->
  HeraldMembershipGeneration ->
  DisappearanceOpenIntention
disappearanceOpenIntention subject membership =
  DisappearanceOpenIntention
    subject
    (disappearanceSubjectMembershipCoordinate subject membership)

disappearanceOpenIntentionSubject ::
  DisappearanceOpenIntention -> DisappearanceSubject
disappearanceOpenIntentionSubject (DisappearanceOpenIntention subject _) = subject

disappearanceOpenIntentionCoordinate ::
  DisappearanceOpenIntention -> DisappearanceSubjectMembershipCoordinate
disappearanceOpenIntentionCoordinate (DisappearanceOpenIntention _ coordinate) = coordinate

-- | Ordered publication-stream marker.  Construction is sequence-bound, so
-- its payload and digest cannot guess or race the PeerStream allocation.
data DisappearancePublicationMarker
  = DisappearancePublicationMarker
      DisappearanceProbeId
      DisappearanceSubjectMembershipCoordinate
      StreamDirection
      StreamSequence
      PeerItemDigest
  deriving stock (Eq, Show)

-- | Construct one immutable marker at its allocated stream coordinate.  The
-- bridge uses this same transcript after checking the wire's nominal claims;
-- the disappearance owner checks them against the exact projected Open.
disappearancePublicationMarker ::
  DisappearanceProbeId ->
  DisappearanceSubjectMembershipCoordinate ->
  StreamDirection ->
  StreamSequence ->
  DisappearancePublicationMarker
disappearancePublicationMarker probe coordinate direction sequenceNumber =
  DisappearancePublicationMarker
    probe
    coordinate
    direction
    sequenceNumber
    (digestPeerItem "publication marker" (publicationMarkerBytes probe coordinate direction sequenceNumber))

disappearancePublicationMarkerItem ::
  DisappearanceProbeId ->
  DisappearanceSubjectMembershipCoordinate ->
  SequenceBoundPeerItem DisappearancePublicationMarker
disappearancePublicationMarkerItem probe coordinate =
  sequenceBoundPeerItem $ \direction sequenceNumber ->
    let marker = disappearancePublicationMarker probe coordinate direction sequenceNumber
     in peerItem (disappearancePublicationMarkerDigest marker) marker

disappearancePublicationMarkerProbe ::
  DisappearancePublicationMarker -> DisappearanceProbeId
disappearancePublicationMarkerProbe
  (DisappearancePublicationMarker probe _ _ _ _) = probe

disappearancePublicationMarkerCoordinate ::
  DisappearancePublicationMarker -> DisappearanceSubjectMembershipCoordinate
disappearancePublicationMarkerCoordinate
  (DisappearancePublicationMarker _ coordinate _ _ _) = coordinate

disappearancePublicationMarkerDirection ::
  DisappearancePublicationMarker -> StreamDirection
disappearancePublicationMarkerDirection
  (DisappearancePublicationMarker _ _ direction _ _) = direction

disappearancePublicationMarkerSequence ::
  DisappearancePublicationMarker -> StreamSequence
disappearancePublicationMarkerSequence
  (DisappearancePublicationMarker _ _ _ sequenceNumber _) = sequenceNumber

disappearancePublicationMarkerDigest ::
  DisappearancePublicationMarker -> PeerItemDigest
disappearancePublicationMarkerDigest
  (DisappearancePublicationMarker _ _ _ _ digest) = digest

disappearancePublicationMarkerCanonicalBytes ::
  DisappearancePublicationMarker -> ByteString
disappearancePublicationMarkerCanonicalBytes marker =
  publicationMarkerBytes
    (disappearancePublicationMarkerProbe marker)
    (disappearancePublicationMarkerCoordinate marker)
    (disappearancePublicationMarkerDirection marker)
    (disappearancePublicationMarkerSequence marker)

publicationMarkerBytes ::
  DisappearanceProbeId ->
  DisappearanceSubjectMembershipCoordinate ->
  StreamDirection ->
  StreamSequence ->
  ByteString
publicationMarkerBytes probe coordinate direction sequenceNumber =
  build
    ( frame publicationMarkerDomain
        <> Builder.byteString (disappearanceProbeIdBytes probe)
        <> putCoordinate coordinate
        <> Builder.byteString (heraldEpochBytes (streamDirectionSource direction))
        <> Builder.byteString (heraldEpochBytes (streamDirectionDestination direction))
        <> Builder.word64BE (streamSequenceWord64 sequenceNumber)
    )

data IncomingAlignmentCut
  = IncomingAlignmentCut AlignmentSubscriptionId HeraldEpoch
  deriving stock (Eq, Ord, Show)

incomingAlignmentCut :: AlignmentSubscriptionId -> HeraldEpoch -> IncomingAlignmentCut
incomingAlignmentCut = IncomingAlignmentCut

incomingAlignmentCutSubscription ::
  IncomingAlignmentCut -> AlignmentSubscriptionId
incomingAlignmentCutSubscription (IncomingAlignmentCut subscription _) = subscription

incomingAlignmentCutSource :: IncomingAlignmentCut -> HeraldEpoch
incomingAlignmentCutSource (IncomingAlignmentCut _ source) = source

incomingAlignmentCutCanonicalBytes :: IncomingAlignmentCut -> ByteString
incomingAlignmentCutCanonicalBytes cut =
  build
    ( frame incomingAlignmentCutDomain
        <> putSubscription (incomingAlignmentCutSubscription cut)
        <> Builder.byteString (heraldEpochBytes (incomingAlignmentCutSource cut))
    )

data OutgoingAlignmentCut
  = OutgoingAlignmentCut AlignmentSubscriptionId StoreRevision
  deriving stock (Eq, Ord, Show)

outgoingAlignmentCut ::
  AlignmentSubscriptionId -> StoreRevision -> OutgoingAlignmentCut
outgoingAlignmentCut = OutgoingAlignmentCut

outgoingAlignmentCutSubscription ::
  OutgoingAlignmentCut -> AlignmentSubscriptionId
outgoingAlignmentCutSubscription (OutgoingAlignmentCut subscription _) = subscription

outgoingAlignmentCutThroughRevision ::
  OutgoingAlignmentCut -> StoreRevision
outgoingAlignmentCutThroughRevision (OutgoingAlignmentCut _ revision) = revision

outgoingAlignmentCutCanonicalBytes :: OutgoingAlignmentCut -> ByteString
outgoingAlignmentCutCanonicalBytes cut =
  build
    ( frame outgoingAlignmentCutDomain
        <> putSubscription (outgoingAlignmentCutSubscription cut)
        <> Builder.word64BE
          (storeRevisionWord64 (outgoingAlignmentCutThroughRevision cut))
    )

-- | Immutable alignment marker.  Source identity comes from its authenticated
-- alignment association; the subscription ID already names the destination.
data DisappearanceAlignmentMarker
  = DisappearanceAlignmentMarker
      DisappearanceProbeId
      AlignmentSubscriptionId
      StoreRevision
  deriving stock (Eq, Ord, Show)

disappearanceAlignmentMarker ::
  DisappearanceProbeId -> OutgoingAlignmentCut -> DisappearanceAlignmentMarker
disappearanceAlignmentMarker probe cut =
  DisappearanceAlignmentMarker
    probe
    (outgoingAlignmentCutSubscription cut)
    (outgoingAlignmentCutThroughRevision cut)

disappearanceAlignmentMarkerProbe ::
  DisappearanceAlignmentMarker -> DisappearanceProbeId
disappearanceAlignmentMarkerProbe
  (DisappearanceAlignmentMarker probe _ _) = probe

disappearanceAlignmentMarkerSubscription ::
  DisappearanceAlignmentMarker -> AlignmentSubscriptionId
disappearanceAlignmentMarkerSubscription
  (DisappearanceAlignmentMarker _ subscription _) = subscription

disappearanceAlignmentMarkerThroughRevision ::
  DisappearanceAlignmentMarker -> StoreRevision
disappearanceAlignmentMarkerThroughRevision
  (DisappearanceAlignmentMarker _ _ revision) = revision

disappearanceAlignmentMarkerCanonicalBytes ::
  DisappearanceAlignmentMarker -> ByteString
disappearanceAlignmentMarkerCanonicalBytes marker =
  build
    ( frame alignmentMarkerDomain
        <> Builder.byteString
          (disappearanceProbeIdBytes (disappearanceAlignmentMarkerProbe marker))
        <> putSubscription
          (disappearanceAlignmentMarkerSubscription marker)
        <> Builder.word64BE
          ( storeRevisionWord64
              (disappearanceAlignmentMarkerThroughRevision marker)
          )
    )

-- | Normalized destination-side proof that one captured incoming alignment
-- subscription has applied its source marker through the required revision.
-- Unlike 'AlignmentSubscriptionId', this evidence retains the authenticated
-- source Herald explicitly.
data CompletedIncomingAlignmentEvidence
  = CompletedIncomingAlignmentEvidence
      DisappearanceProbeId
      AlignmentSubscriptionId
      HeraldEpoch
      StoreRevision
  deriving stock (Eq, Ord, Show)

completedIncomingAlignmentEvidenceForOwner ::
  ProjectedDisappearanceProbe ->
  IncomingAlignmentCut ->
  DisappearanceAlignmentMarker ->
  StoreRevision ->
  Either DisappearanceProtocolProblem CompletedIncomingAlignmentEvidence
completedIncomingAlignmentEvidenceForOwner projected capture marker appliedThrough
  | disappearanceAlignmentMarkerProbe marker /= projectedProbeId projected =
      Left DisappearanceAlignmentEvidenceProbeMismatch
  | disappearanceAlignmentMarkerSubscription marker
      /= incomingAlignmentCutSubscription capture =
      Left DisappearanceAlignmentEvidenceSubscriptionMismatch
  | appliedThrough < disappearanceAlignmentMarkerThroughRevision marker =
      Left DisappearanceAlignmentEvidenceIncomplete
  | not
      ( Set.member source members
          && Set.member destination members
      ) =
      Left DisappearanceAlignmentEvidenceMemberMismatch
  | otherwise =
      Right
        ( CompletedIncomingAlignmentEvidence
            (projectedProbeId projected)
            (incomingAlignmentCutSubscription capture)
            (incomingAlignmentCutSource capture)
            (disappearanceAlignmentMarkerThroughRevision marker)
        )
  where
    source = incomingAlignmentCutSource capture
    destination =
      alignmentSubscriptionIdDestinationHerald
        (incomingAlignmentCutSubscription capture)
    members = Set.fromList (NonEmpty.toList (projectedProbeMembers projected))

completedIncomingAlignmentEvidenceProbe ::
  CompletedIncomingAlignmentEvidence -> DisappearanceProbeId
completedIncomingAlignmentEvidenceProbe
  (CompletedIncomingAlignmentEvidence probe _ _ _) = probe

completedIncomingAlignmentEvidenceSubscription ::
  CompletedIncomingAlignmentEvidence -> AlignmentSubscriptionId
completedIncomingAlignmentEvidenceSubscription
  (CompletedIncomingAlignmentEvidence _ subscription _ _) = subscription

completedIncomingAlignmentEvidenceSource ::
  CompletedIncomingAlignmentEvidence -> HeraldEpoch
completedIncomingAlignmentEvidenceSource
  (CompletedIncomingAlignmentEvidence _ _ source _) = source

completedIncomingAlignmentEvidenceThroughRevision ::
  CompletedIncomingAlignmentEvidence -> StoreRevision
completedIncomingAlignmentEvidenceThroughRevision
  (CompletedIncomingAlignmentEvidence _ _ _ revision) = revision

completedIncomingAlignmentEvidenceCanonicalBytes ::
  CompletedIncomingAlignmentEvidence -> ByteString
completedIncomingAlignmentEvidenceCanonicalBytes evidence =
  build
    ( frame completedAlignmentEvidenceDomain
        <> Builder.byteString
          ( disappearanceProbeIdBytes
              (completedIncomingAlignmentEvidenceProbe evidence)
          )
        <> putSubscription
          (completedIncomingAlignmentEvidenceSubscription evidence)
        <> Builder.byteString
          (heraldEpochBytes (completedIncomingAlignmentEvidenceSource evidence))
        <> Builder.word64BE
          ( storeRevisionWord64
              (completedIncomingAlignmentEvidenceThroughRevision evidence)
          )
    )

-- | Closed local reasons why a negative report is not yet sound.  The exact
-- witness digest is owner-produced; this leaf never reads a sibling whole state.
data DisappearanceBlockerClass
  = VisibleApplicationCopyBlocker
  | UnsequencedPublicationBlocker
  | HeldPublicationBlocker
  | PeerInboxBlocker
  | PeerOutboxBlocker
  | RetainedStoreValueBlocker
  | CurrentControlledUseBlocker
  | ObsoleteControlledUseBlocker
  | ActiveNablaBlocker
  | PassiveNablaBlocker
  | ActiveDeltaBlocker
  | PassiveDeltaBlocker
  | UnresolvedStructuralReferenceBlocker
  | RetainedPublicationStageBlocker
  | PublicationDependencyHoldBlocker
  | RouteDependencyBlocker
  | PlacementDependencyBlocker
  | AlignmentDebtBlocker
  | AlignmentObligationBlocker
  | AlignmentAttemptBlocker
  | AlignmentSubscriptionBlocker
  | AlignmentSnapshotBlocker
  | AlignmentChangeLogBlocker
  | AlignmentCertificateBlocker
  | UncertainSemanticPayloadBlocker
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data DisappearanceBlockerWitness
  = DisappearanceBlockerWitness
      DisappearanceBlockerClass
      DisappearanceEvidenceDigest
  deriving stock (Eq, Ord, Show)

disappearanceBlockerWitness ::
  DisappearanceBlockerClass ->
  DisappearanceEvidenceDigest ->
  DisappearanceBlockerWitness
disappearanceBlockerWitness = DisappearanceBlockerWitness

disappearanceBlockerClass ::
  DisappearanceBlockerWitness -> DisappearanceBlockerClass
disappearanceBlockerClass (DisappearanceBlockerWitness blockerClass _) = blockerClass

disappearanceBlockerDigest ::
  DisappearanceBlockerWitness -> DisappearanceEvidenceDigest
disappearanceBlockerDigest (DisappearanceBlockerWitness _ digest) = digest

data MatchingPublicationObservation
  = MatchingPublicationObservation
      DisappearanceSubject
      HeraldPublicationPosition
      DisappearanceEvidenceDigest
  deriving stock (Eq, Show)

matchingPublicationObservation ::
  DisappearanceSubject ->
  HeraldPublicationPosition ->
  DisappearanceEvidenceDigest ->
  MatchingPublicationObservation
matchingPublicationObservation = MatchingPublicationObservation

matchingPublicationSubject ::
  MatchingPublicationObservation -> DisappearanceSubject
matchingPublicationSubject (MatchingPublicationObservation subject _ _) = subject

matchingPublicationPosition ::
  MatchingPublicationObservation -> HeraldPublicationPosition
matchingPublicationPosition (MatchingPublicationObservation _ position _) = position

matchingPublicationWitness ::
  MatchingPublicationObservation -> DisappearanceEvidenceDigest
matchingPublicationWitness (MatchingPublicationObservation _ _ witness) = witness

data DisappearanceInvalidationReason
  = MatchingPublicationObserved
  | LocalEvidenceContradicted
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data DisappearanceInvalidationIntention
  = DisappearanceInvalidationIntention
      DisappearanceProbeId
      HeraldEpoch
      DisappearanceInvalidationReason
      DisappearanceEvidenceDigest
  deriving stock (Eq, Show)

disappearanceInvalidationIntention ::
  DisappearanceProbeId ->
  HeraldEpoch ->
  DisappearanceInvalidationReason ->
  DisappearanceEvidenceDigest ->
  DisappearanceInvalidationIntention
disappearanceInvalidationIntention = DisappearanceInvalidationIntention

disappearanceInvalidationProbe ::
  DisappearanceInvalidationIntention -> DisappearanceProbeId
disappearanceInvalidationProbe
  (DisappearanceInvalidationIntention probe _ _ _) = probe

disappearanceInvalidationReporter ::
  DisappearanceInvalidationIntention -> HeraldEpoch
disappearanceInvalidationReporter
  (DisappearanceInvalidationIntention _ reporter _ _) = reporter

disappearanceInvalidationReason ::
  DisappearanceInvalidationIntention -> DisappearanceInvalidationReason
disappearanceInvalidationReason
  (DisappearanceInvalidationIntention _ _ reason _) = reason

disappearanceInvalidationWitness ::
  DisappearanceInvalidationIntention -> DisappearanceEvidenceDigest
disappearanceInvalidationWitness
  (DisappearanceInvalidationIntention _ _ _ witness) = witness

-- | Closed evidence categories contributed by the actual Store/publication/
-- graph owners. The attestation records normalized digests only; it does not
-- copy sibling state into the disappearance owner.
data AbsenceAttestationClass
  = ApplicationStoreAbsence
  | PublicationWorkAbsence
  | PeerStreamWorkAbsence
  | AlignmentWorkAbsence
  | GraphDependencyAbsence
  | ControlledUseAbsence
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data DisappearanceAbsenceAttestation
  = DisappearanceAbsenceAttestation
      DisappearanceSubject
      (Map AbsenceAttestationClass DisappearanceEvidenceDigest)
  deriving stock (Eq, Show)

disappearanceAbsenceAttestation ::
  DisappearanceSubject ->
  [(AbsenceAttestationClass, DisappearanceEvidenceDigest)] ->
  Either DisappearanceProtocolProblem DisappearanceAbsenceAttestation
disappearanceAbsenceAttestation subject entries = do
  normalized <- insertAttestations Map.empty entries
  let actual = Map.keysSet normalized
      expected = requiredAttestationClasses subject
  if actual == expected
    then Right (DisappearanceAbsenceAttestation subject normalized)
    else Left (DisappearanceAttestationClassSetMismatch expected actual)
  where
    insertAttestations retained [] = Right retained
    insertAttestations retained ((category, digest) : rest)
      | Map.member category retained =
          Left (DisappearanceAttestationDuplicateClass category)
      | otherwise =
          insertAttestations (Map.insert category digest retained) rest

requiredAttestationClasses ::
  DisappearanceSubject -> Set AbsenceAttestationClass
requiredAttestationClasses subject = case disappearanceSubjectView subject of
  ControlledPredefinedSubjectView {} -> common
  RegularSortDefinitionSubjectView {} ->
    Set.insert GraphDependencyAbsence (Set.insert ControlledUseAbsence common)
  where
    common =
      Set.fromList
        [ ApplicationStoreAbsence,
          PublicationWorkAbsence,
          PeerStreamWorkAbsence,
          AlignmentWorkAbsence
        ]

disappearanceAbsenceAttestationSubject ::
  DisappearanceAbsenceAttestation -> DisappearanceSubject
disappearanceAbsenceAttestationSubject
  (DisappearanceAbsenceAttestation subject _) = subject

disappearanceAbsenceAttestationEntries ::
  DisappearanceAbsenceAttestation ->
  [(AbsenceAttestationClass, DisappearanceEvidenceDigest)]
disappearanceAbsenceAttestationEntries
  (DisappearanceAbsenceAttestation _ entries) = Map.toAscList entries

disappearanceAbsenceAttestationCanonicalBytes ::
  DisappearanceAbsenceAttestation -> ByteString
disappearanceAbsenceAttestationCanonicalBytes attestation =
  build
    ( frame absenceAttestationDomain
        <> frame
          ( disappearanceSubjectCanonicalBytes
              (disappearanceAbsenceAttestationSubject attestation)
          )
        <> counted putEntry (disappearanceAbsenceAttestationEntries attestation)
    )
  where
    putEntry (category, digest) =
      Builder.word8 (attestationClassTag category)
        <> Builder.byteString (disappearanceEvidenceDigestBytes digest)

attestationClassTag :: AbsenceAttestationClass -> Word8
attestationClassTag category = case category of
  ApplicationStoreAbsence -> 1
  PublicationWorkAbsence -> 2
  PeerStreamWorkAbsence -> 3
  AlignmentWorkAbsence -> 4
  GraphDependencyAbsence -> 5
  ControlledUseAbsence -> 6

data LocalAbsenceReport
  = LocalAbsenceReport
      DisappearanceProbeId
      DisappearanceEvidenceClaim
      DisappearanceAbsenceAttestation
      [CompletedIncomingAlignmentEvidence]
      ByteString
  deriving stock (Eq, Show)

-- | Normalize complete local cut evidence and bind its digest to the exact
-- projected probe.  Only a state owner which has checked readiness should call
-- this package-private constructor.
localAbsenceReportForOwner ::
  ProjectedDisappearanceProbe ->
  HeraldEpoch ->
  HeraldPublicationPrefix ->
  [DisappearancePublicationMarker] ->
  [CompletedIncomingAlignmentEvidence] ->
  DisappearanceAbsenceAttestation ->
  Either DisappearanceProtocolProblem LocalAbsenceReport
localAbsenceReportForOwner projected reporter localCut publicationMarkers alignmentEvidence attestation
  | disappearanceAbsenceAttestationSubject attestation
      /= projectedProbeSubject projected =
      Left DisappearanceAttestationSubjectMismatch
  | any
      ((/= projectedProbeId projected) . completedIncomingAlignmentEvidenceProbe)
      alignmentEvidence =
      Left DisappearanceAlignmentEvidenceProbeMismatch
  | otherwise = case admitDisappearanceEvidenceClaim
      (projectedProbeSubject projected)
      (projectedProbeMembership projected)
      (projectedProbeId projected)
      reporter
      evidenceDigest of
      Left _ -> Left DisappearanceReportConstructionInvariant
      Right claim ->
        Right
          ( LocalAbsenceReport
              (projectedProbeId projected)
              claim
              attestation
              normalizedAlignmentEvidence
              canonical
          )
  where
    canonical =
      build
        ( frame localReportDomain
            <> Builder.byteString
              (disappearanceProbeIdBytes (projectedProbeId projected))
            <> frame
              (disappearanceSubjectCanonicalBytes (projectedProbeSubject projected))
            <> putCoordinate (projectedProbeCoordinate projected)
            <> Builder.byteString (heraldEpochBytes reporter)
            <> putPublicationPrefix localCut
            <> frame (disappearanceAbsenceAttestationCanonicalBytes attestation)
            <> counted
              (frame . disappearancePublicationMarkerCanonicalBytes)
              ( sortOn
                  ( \marker ->
                      ( disappearancePublicationMarkerDirection marker,
                        disappearancePublicationMarkerSequence marker
                      )
                  )
                  publicationMarkers
              )
            <> counted
              (frame . completedIncomingAlignmentEvidenceCanonicalBytes)
              normalizedAlignmentEvidence
        )
    evidenceDigest = deriveDisappearanceEvidenceDigest canonical
    normalizedAlignmentEvidence =
      sortOn
        ( \evidence ->
            ( completedIncomingAlignmentEvidenceSource evidence,
              completedIncomingAlignmentEvidenceSubscription evidence
            )
        )
        alignmentEvidence

localAbsenceReportProbe :: LocalAbsenceReport -> DisappearanceProbeId
localAbsenceReportProbe (LocalAbsenceReport probe _ _ _ _) = probe

localAbsenceReportClaim :: LocalAbsenceReport -> DisappearanceEvidenceClaim
localAbsenceReportClaim (LocalAbsenceReport _ claim _ _ _) = claim

localAbsenceReportAttestation ::
  LocalAbsenceReport -> DisappearanceAbsenceAttestation
localAbsenceReportAttestation (LocalAbsenceReport _ _ attestation _ _) = attestation

localAbsenceReportAlignmentEvidence ::
  LocalAbsenceReport -> [CompletedIncomingAlignmentEvidence]
localAbsenceReportAlignmentEvidence
  (LocalAbsenceReport _ _ _ alignmentEvidence _) = alignmentEvidence

localAbsenceReportCanonicalBytes :: LocalAbsenceReport -> ByteString
localAbsenceReportCanonicalBytes (LocalAbsenceReport _ _ _ _ canonical) = canonical

data DisappearanceResolveIntention
  = DisappearanceResolveIntention
      DisappearanceProbeId
      [DisappearanceEvidenceClaim]
  deriving stock (Eq, Show)

disappearanceResolveIntention ::
  DisappearanceProbeId ->
  [DisappearanceEvidenceClaim] ->
  DisappearanceResolveIntention
disappearanceResolveIntention probe =
  DisappearanceResolveIntention probe
    . sortOn disappearanceEvidenceClaimCanonicalBytes

disappearanceResolveProbe ::
  DisappearanceResolveIntention -> DisappearanceProbeId
disappearanceResolveProbe (DisappearanceResolveIntention probe _) = probe

disappearanceResolveClaims ::
  DisappearanceResolveIntention -> [DisappearanceEvidenceClaim]
disappearanceResolveClaims (DisappearanceResolveIntention _ claims) = claims

-- | The three immutable Oracle label outcomes which can follow a label-caused
-- disappearance invalidation.  A release or deletion carries its exact
-- resulting revision; NotApplied deliberately has no invented revision.
data ProjectedLabelTerminalOutcome
  = ProjectedLabelNotApplied
  | ProjectedLabelReleased LabelRevision
  | ProjectedLabelDeleted LabelRevision
  deriving stock (Eq, Show)

-- | Exact projected label terminal.  The constructor is hidden so callers must
-- name the decision, controlled object, and terminal arm explicitly rather than
-- smuggling a partially correlated tuple into the leaf transition.
data ProjectedLabelTerminal
  = ProjectedLabelTerminal
      LabelDecisionId
      GlobalObjectId
      ProjectedLabelTerminalOutcome
  deriving stock (Eq, Show)

projectedLabelTerminal ::
  LabelDecisionId ->
  GlobalObjectId ->
  ProjectedLabelTerminalOutcome ->
  ProjectedLabelTerminal
projectedLabelTerminal = ProjectedLabelTerminal

projectedLabelTerminalDecision :: ProjectedLabelTerminal -> LabelDecisionId
projectedLabelTerminalDecision (ProjectedLabelTerminal decision _ _) = decision

projectedLabelTerminalObject :: ProjectedLabelTerminal -> GlobalObjectId
projectedLabelTerminalObject (ProjectedLabelTerminal _ object _) = object

projectedLabelTerminalOutcome ::
  ProjectedLabelTerminal -> ProjectedLabelTerminalOutcome
projectedLabelTerminalOutcome (ProjectedLabelTerminal _ _ outcome) = outcome

data ProjectedInvalidationCause
  = ProjectedCommandInvalidation
      HeraldEpoch
      DisappearanceInvalidationReason
      DisappearanceEvidenceDigest
  | ProjectedLabelInvalidation LabelDecisionId GlobalObjectId
  deriving stock (Eq, Show)

data ProjectedAbortCause
  = ProjectedAuthorizedAbort
  | ProjectedMembershipSuperseded HeraldMembershipGeneration
  | ProjectedAdmissionPreparing HeraldAdmissionId
  deriving stock (Eq, Show)

data ProjectedDisappearanceTerminal
  = ProjectedDisappearanceInvalidated
      DisappearanceProbeId
      ProjectedInvalidationCause
      ControlIndex
  | ProjectedDisappearanceResolved
      DisappearanceProbeId
      DisappearanceResolutionOutcome
      ControlIndex
  | ProjectedDisappearanceAborted
      DisappearanceProbeId
      ProjectedAbortCause
      ControlIndex
  deriving stock (Eq, Show)

publicationMarkerDomain,
  incomingAlignmentCutDomain,
  outgoingAlignmentCutDomain,
  alignmentMarkerDomain,
  completedAlignmentEvidenceDomain,
  absenceAttestationDomain,
  localReportDomain ::
    ByteString
publicationMarkerDomain = "ECLIPS-DISAPPEARANCE-PUBLICATION-MARKER"
incomingAlignmentCutDomain = "ECLIPS-DISAPPEARANCE-INCOMING-ALIGNMENT-CUT"
outgoingAlignmentCutDomain = "ECLIPS-DISAPPEARANCE-OUTGOING-ALIGNMENT-CUT"
alignmentMarkerDomain = "ECLIPS-DISAPPEARANCE-ALIGNMENT-MARKER"
completedAlignmentEvidenceDomain =
  "ECLIPS-DISAPPEARANCE-COMPLETED-ALIGNMENT-EVIDENCE"
absenceAttestationDomain = "ECLIPS-DISAPPEARANCE-ABSENCE-ATTESTATION"
localReportDomain = "ECLIPS-DISAPPEARANCE-LOCAL-ABSENCE"

putCoordinate ::
  DisappearanceSubjectMembershipCoordinate -> Builder.Builder
putCoordinate coordinate =
  Builder.byteString
    ( disappearanceSubjectDigestBytes
        (disappearanceCoordinateSubjectDigest coordinate)
    )
    <> Builder.byteString
      ( heraldMembershipGenerationIdBytes
          (disappearanceCoordinateMembershipGenerationId coordinate)
      )
    <> Builder.byteString
      ( memberSetDigestBytes
          (disappearanceCoordinateMemberSetDigest coordinate)
      )

putSubscription :: AlignmentSubscriptionId -> Builder.Builder
putSubscription subscription =
  Builder.byteString
    (heraldEpochBytes (alignmentSubscriptionIdDestinationHerald subscription))
    <> Builder.word64BE
      ( alignmentSubscriptionSequenceWord64
          (alignmentSubscriptionIdSequence subscription)
      )

putPublicationPrefix :: HeraldPublicationPrefix -> Builder.Builder
putPublicationPrefix prefix = case prefix of
  EmptyHeraldPublicationPrefix -> Builder.word8 0
  HeraldPublicationPrefixThrough position ->
    Builder.word8 1
      <> Builder.word64BE (heraldPublicationPositionWord64 position)

digestPeerItem :: String -> ByteString -> PeerItemDigest
digestPeerItem label bytes =
  case mkPeerItemDigest (SHA256.hash bytes) of
    Right digest -> digest
    Left problem -> error (label <> " digest invariant: " <> show problem)

frame :: ByteString -> Builder.Builder
frame bytes =
  Builder.word64BE (fromIntegral (ByteString.length bytes) :: Word64)
    <> Builder.byteString bytes

counted :: (value -> Builder.Builder) -> [value] -> Builder.Builder
counted put values =
  Builder.word64BE (fromIntegral (length values) :: Word64)
    <> foldMap put values

build :: Builder.Builder -> ByteString
build = LazyByteString.toStrict . Builder.toLazyByteString
