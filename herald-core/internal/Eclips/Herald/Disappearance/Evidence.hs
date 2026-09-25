{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compose narrow, subject-qualified reads from the actual Herald evidence
-- owners.
--
-- This module owns no state and performs no whole-Herald validation.  It joins
-- opaque owner views only after checking their exact subject and regular-only
-- applicability, and it exposes a checked absence attestation only when every
-- relevant owner reported an empty fact set.
module Eclips.Herald.Disappearance.Evidence
  ( DisappearanceEvidenceSnapshot,
    DisappearanceEvidenceProblem (..),
    DisappearanceEvidenceOwner (..),
    disappearanceEvidenceSnapshot,
    disappearanceEvidenceSnapshotForHerald,
    regularRetirementEvidenceSnapshotForHerald,
    regularRetirementResidualBlockersForHerald,
    disappearanceEvidenceSnapshotSubject,
    disappearanceEvidenceSnapshotLocalPublicationCut,
    disappearanceEvidenceSnapshotIncomingAlignmentCuts,
    disappearanceEvidenceSnapshotOutgoingAlignmentCuts,
    disappearanceEvidenceSnapshotBlockers,
    disappearanceEvidenceSnapshotAbsenceAttestation,
    disappearanceEvidenceSnapshotMatchingPublications,
    disappearanceEvidenceSnapshotBlockerCount,
  )
where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceDigest,
    DisappearanceProbeId,
    DisappearanceSubject,
    DisappearanceSubjectView (..),
    disappearanceSubjectView,
  )
import Eclips.Domain.Identity (ControlIndex)
import Eclips.Herald.Alignment.Disappearance qualified as Alignment
import Eclips.Herald.Application.Disappearance qualified as Application
import Eclips.Herald.Controlled.Disappearance qualified as Controlled
import Eclips.Herald.Disappearance.Evidence.Internal
  ( DisappearanceEvidenceSnapshot,
    assembleDisappearanceEvidenceSnapshot,
    disappearanceEvidenceSnapshotAbsenceAttestation,
    disappearanceEvidenceSnapshotBlockerCount,
    disappearanceEvidenceSnapshotBlockers,
    disappearanceEvidenceSnapshotIncomingAlignmentCuts,
    disappearanceEvidenceSnapshotLocalPublicationCut,
    disappearanceEvidenceSnapshotMatchingPublications,
    disappearanceEvidenceSnapshotOutgoingAlignmentCuts,
    disappearanceEvidenceSnapshotSubject,
  )
import Eclips.Herald.Disappearance.OwnerEvidence
  ( combineOwnerAbsenceDigests,
  )
import Eclips.Herald.Disappearance.Protocol
  ( AbsenceAttestationClass (..),
    DisappearanceAbsenceAttestation,
    DisappearanceBlockerWitness,
    DisappearanceProtocolProblem,
    disappearanceAbsenceAttestation,
  )
import Eclips.Herald.Disappearance.Protocol qualified as Protocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.Graph.Disappearance qualified as Graph
import Eclips.Herald.PeerStream.Disappearance qualified as PeerStream
import Eclips.Herald.PeerStream.State qualified as PeerStreamState
import Eclips.Herald.Placement.Disappearance qualified as Placement
import Eclips.Herald.Publication.Disappearance qualified as Publication
import Eclips.Herald.Publication.State qualified as PublicationState
import Eclips.Herald.Startup.State
  ( HeraldState,
    startupAlignmentState,
    startupApplicationState,
    startupControlledState,
    startupDisappearanceState,
    startupGraphState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupStoreState,
  )
import Eclips.Herald.Store.Disappearance qualified as Store

data DisappearanceEvidenceOwner
  = StoreEvidenceOwner
  | ApplicationEvidenceOwner
  | PublicationEvidenceOwner
  | PeerStreamEvidenceOwner
  | AlignmentEvidenceOwner
  | ControlledEvidenceOwner
  | GraphEvidenceOwner
  | PlacementEvidenceOwner
  deriving stock (Bounded, Enum, Eq, Ord, Show)

data DisappearanceEvidenceProblem
  = DisappearanceEvidenceSubjectMismatch
      DisappearanceEvidenceOwner
      DisappearanceSubject
      DisappearanceSubject
  | DisappearanceEvidenceAttestationProblem DisappearanceProtocolProblem
  | DisappearanceEvidencePeerStreamProblem PeerStreamState.PeerStreamProblem
  deriving stock (Eq, Show)

-- Only the exact currently collecting subject cut owns a held publication.
-- Work retained by an earlier terminal probe remains evidence for a fresh cut.
subjectProbeGates :: DisappearanceSubject -> HeraldState -> Set.Set DisappearanceProbeId
subjectProbeGates subject state =
  Set.fromList
    [ Protocol.projectedProbeId witness.witnessProjectedProbe
    | witness <- Disappearance.probeWitnesses (startupDisappearanceState state),
      witness.witnessPhase == Disappearance.ProbeCollectingView,
      Protocol.projectedProbeSubject witness.witnessProjectedProbe == subject
    ]

-- | Take the simultaneous logical subject view used by an atomic whole-Herald
-- coordinator.  Each fact is still classified by its private owner adapter;
-- this convenience boundary merely prevents callers from accidentally omitting
-- one of the eight required owners during a resolve-time recheck.
disappearanceEvidenceSnapshotForHerald ::
  DisappearanceSubject ->
  HeraldState ->
  Either DisappearanceEvidenceProblem DisappearanceEvidenceSnapshot
disappearanceEvidenceSnapshotForHerald subject state = do
  peerStream <-
    either
      (Left . DisappearanceEvidencePeerStreamProblem)
      Right
      ( PeerStream.peerStreamDisappearanceViewWithGates
          (PublicationState.disappearanceGatedIncomingPublicationIds (startupPublicationState state))
          subject
          (startupPeerStreamState state)
      )
  disappearanceEvidenceSnapshot
    (Store.storeDisappearanceView subject (startupStoreState state))
    (Application.applicationDisappearanceViewWithGates (subjectProbeGates subject state) subject (startupApplicationState state))
    (Publication.publicationDisappearanceView subject (startupPublicationState state))
    peerStream
    (Alignment.alignmentDisappearanceView subject (startupAlignmentState state))
    (Controlled.controlledDisappearanceView subject (startupControlledState state))
    (Graph.graphDisappearanceView subject (startupGraphState state))
    (Placement.placementDisappearanceView subject (startupPlacementState state))

-- | Take the simultaneous, Resolve-aware view of work which still belongs to
-- one retiring regular occurrence.  The authority's immutable Open evidence
-- remains the strict historical snapshot, but a fresh commit-time recheck must
-- not let successor work accepted at or beyond Resolve block retirement, fail
-- the frozen local cut, or inflate the retired epoch's work count.
regularRetirementEvidenceSnapshotForHerald ::
  DisappearanceSubject ->
  ControlIndex ->
  HeraldState ->
  Either DisappearanceEvidenceProblem DisappearanceEvidenceSnapshot
regularRetirementEvidenceSnapshotForHerald subject resolveIndex state = do
  peerStream <-
    either
      (Left . DisappearanceEvidencePeerStreamProblem)
      Right
      ( PeerStream.peerStreamRegularRetirementDisappearanceViewWithGates
          (PublicationState.disappearanceGatedIncomingPublicationIds (startupPublicationState state))
          subject
          resolveIndex
          (startupPeerStreamState state)
      )
  let store =
        Store.storeRegularRetirementDisappearanceView
          subject
          resolveIndex
          (startupStoreState state)
      application =
        Application.applicationRegularRetirementDisappearanceViewWithGates
          (subjectProbeGates subject state)
          subject
          resolveIndex
          (startupApplicationState state)
      publication =
        Publication.publicationRegularRetirementDisappearanceView
          subject
          resolveIndex
          (startupPublicationState state)
      alignment =
        Alignment.alignmentRegularRetirementDisappearanceView
          subject
          resolveIndex
          (startupAlignmentState state)
      controlled =
        Controlled.controlledDisappearanceView subject (startupControlledState state)
      graph = Graph.graphDisappearanceView subject (startupGraphState state)
      placement =
        Placement.placementDisappearanceView subject (startupPlacementState state)
  disappearanceEvidenceSnapshot
    store
    application
    publication
    peerStream
    alignment
    controlled
    graph
    placement

-- | Report only the live residual blockers from the same Resolve-aware
-- simultaneous snapshot used by fresh regular-retirement commit rechecks.
regularRetirementResidualBlockersForHerald ::
  DisappearanceSubject ->
  ControlIndex ->
  HeraldState ->
  Either DisappearanceEvidenceProblem [DisappearanceBlockerWitness]
regularRetirementResidualBlockersForHerald subject resolveIndex state =
  disappearanceEvidenceSnapshotBlockers
    <$> regularRetirementEvidenceSnapshotForHerald
      subject
      resolveIndex
      state

-- | Join one simultaneous logical read from every owner.  The owner states do
-- not escape this boundary; only their normalized facts, cut coordinates, and
-- conditional empty-set digests are retained by the disappearance leaf.
disappearanceEvidenceSnapshot ::
  Store.StoreDisappearanceView ->
  Application.ApplicationDisappearanceView ->
  Publication.PublicationDisappearanceView ->
  PeerStream.PeerStreamDisappearanceView ->
  Alignment.AlignmentDisappearanceView ->
  Controlled.ControlledDisappearanceView ->
  Graph.GraphDisappearanceView ->
  Placement.PlacementDisappearanceView ->
  Either DisappearanceEvidenceProblem DisappearanceEvidenceSnapshot
disappearanceEvidenceSnapshot store application publication peerStream alignment controlled graph placement = do
  let subject = Store.storeDisappearanceSubject store
  checkSubject ApplicationEvidenceOwner subject (Application.applicationDisappearanceSubject application)
  checkSubject PublicationEvidenceOwner subject (Publication.publicationDisappearanceSubject publication)
  checkSubject PeerStreamEvidenceOwner subject (PeerStream.peerStreamDisappearanceSubject peerStream)
  checkSubject AlignmentEvidenceOwner subject (Alignment.alignmentDisappearanceSubject alignment)
  checkSubject ControlledEvidenceOwner subject (Controlled.controlledDisappearanceSubject controlled)
  checkSubject GraphEvidenceOwner subject (Graph.graphDisappearanceSubject graph)
  checkSubject PlacementEvidenceOwner subject (Placement.placementDisappearanceSubject placement)
  attestation <- buildAttestation subject store application publication peerStream alignment controlled graph placement
  Right
    ( assembleDisappearanceEvidenceSnapshot
        subject
        (Publication.publicationDisappearanceCurrentPrefix publication)
        (Alignment.alignmentDisappearanceIncomingCuts alignment)
        (Alignment.alignmentDisappearanceOutgoingCuts alignment)
        ( Store.storeDisappearanceBlockers store
            <> Application.applicationDisappearanceBlockers application
            <> Publication.publicationDisappearanceBlockers publication
            <> PeerStream.peerStreamDisappearanceBlockers peerStream
            <> Alignment.alignmentDisappearanceBlockers alignment
            <> Controlled.controlledDisappearanceBlockers controlled
            <> Graph.graphDisappearanceBlockers graph
            <> Placement.placementDisappearanceBlockers placement
        )
        attestation
        (Publication.publicationDisappearanceMatchingObservations publication)
    )

checkSubject ::
  DisappearanceEvidenceOwner ->
  DisappearanceSubject ->
  DisappearanceSubject ->
  Either DisappearanceEvidenceProblem ()
checkSubject owner expected actual =
  unless
    (expected == actual)
    (Left (DisappearanceEvidenceSubjectMismatch owner expected actual))

buildAttestation ::
  DisappearanceSubject ->
  Store.StoreDisappearanceView ->
  Application.ApplicationDisappearanceView ->
  Publication.PublicationDisappearanceView ->
  PeerStream.PeerStreamDisappearanceView ->
  Alignment.AlignmentDisappearanceView ->
  Controlled.ControlledDisappearanceView ->
  Graph.GraphDisappearanceView ->
  Placement.PlacementDisappearanceView ->
  Either
    DisappearanceEvidenceProblem
    (Maybe DisappearanceAbsenceAttestation)
buildAttestation subject store application publication peerStream alignment controlled graph placement =
  case commonEntries of
    Nothing -> Right Nothing
    Just common -> case disappearanceSubjectView subject of
      ControlledPredefinedSubjectView {} -> admit common
      RegularSortDefinitionSubjectView {} ->
        case regularEntries of
          Nothing -> Right Nothing
          Just regular -> admit (common <> regular)
  where
    commonEntries = do
      storeDigest <- Store.storeDisappearanceAbsenceDigest store
      applicationDigest <- Application.applicationDisappearanceAbsenceDigest application
      publicationDigest <- Publication.publicationDisappearanceAbsenceDigest publication
      peerDigest <- PeerStream.peerStreamDisappearanceAbsenceDigest peerStream
      alignmentDigest <- Alignment.alignmentDisappearanceAbsenceDigest alignment
      pure
        [ (ApplicationStoreAbsence, storeDigest),
          ( PublicationWorkAbsence,
            combinedDigest publicationWorkDomain subject (applicationDigest :| [publicationDigest])
          ),
          (PeerStreamWorkAbsence, peerDigest),
          (AlignmentWorkAbsence, alignmentDigest)
        ]
    regularEntries = do
      controlledDigest <- Controlled.controlledDisappearanceAbsenceDigest controlled
      graphDigest <- Graph.graphDisappearanceAbsenceDigest graph
      placementDigest <- Placement.placementDisappearanceAbsenceDigest placement
      pure
        [ ( GraphDependencyAbsence,
            combinedDigest graphDependencyDomain subject (graphDigest :| [placementDigest])
          ),
          (ControlledUseAbsence, controlledDigest)
        ]
    admit entries =
      either
        (Left . DisappearanceEvidenceAttestationProblem)
        (Right . Just)
        (disappearanceAbsenceAttestation subject entries)

combinedDigest ::
  ByteString ->
  DisappearanceSubject ->
  NonEmpty DisappearanceEvidenceDigest ->
  DisappearanceEvidenceDigest
combinedDigest = combineOwnerAbsenceDigests

publicationWorkDomain :: ByteString
publicationWorkDomain = "ECLIPS-DISAPPEARANCE-PUBLICATION-WORK-ABSENCE"

graphDependencyDomain :: ByteString
graphDependencyDomain = "ECLIPS-DISAPPEARANCE-GRAPH-DEPENDENCY-ABSENCE"
