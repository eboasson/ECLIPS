{-# LANGUAGE ImportQualifiedPost #-}

-- | Constructor support for the opaque owner-composed disappearance evidence
-- snapshot.
--
-- Production code constructs snapshots through
-- "Eclips.Herald.Disappearance.Evidence".  The constructor function is split
-- out solely so the detached prospective property component can inject one
-- blocker class at a time without manufacturing live Store, Graph, or
-- Alignment states.  The Step-16 architecture audit restricts imports of this
-- module to that public composer and its named fixture module.
module Eclips.Herald.Disappearance.Evidence.Internal
  ( DisappearanceEvidenceSnapshot,
    assembleDisappearanceEvidenceSnapshot,
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

import Data.List (nub, sort, sortOn)
import Data.Word (Word64)
import Eclips.Domain.Alignment (HeraldPublicationPrefix)
import Eclips.Domain.Disappearance (DisappearanceSubject)
import Eclips.Herald.Disappearance.Protocol
  ( DisappearanceAbsenceAttestation,
    DisappearanceBlockerWitness,
    IncomingAlignmentCut,
    MatchingPublicationObservation,
    OutgoingAlignmentCut,
    matchingPublicationPosition,
    matchingPublicationWitness,
  )

-- | Immutable result of reading every evidence owner for one exact subject.
-- The constructor is deliberately hidden from the ordinary module.
data DisappearanceEvidenceSnapshot
  = DisappearanceEvidenceSnapshot
      DisappearanceSubject
      HeraldPublicationPrefix
      [IncomingAlignmentCut]
      [OutgoingAlignmentCut]
      [DisappearanceBlockerWitness]
      (Maybe DisappearanceAbsenceAttestation)
      [MatchingPublicationObservation]
  deriving stock (Eq, Show)

assembleDisappearanceEvidenceSnapshot ::
  DisappearanceSubject ->
  HeraldPublicationPrefix ->
  [IncomingAlignmentCut] ->
  [OutgoingAlignmentCut] ->
  [DisappearanceBlockerWitness] ->
  Maybe DisappearanceAbsenceAttestation ->
  [MatchingPublicationObservation] ->
  DisappearanceEvidenceSnapshot
assembleDisappearanceEvidenceSnapshot subject localCut incoming outgoing blockers attestation matching =
  DisappearanceEvidenceSnapshot
    subject
    localCut
    (sort (nub incoming))
    (sort (nub outgoing))
    (sort (nub blockers))
    attestation
    ( sortOn
        (\observation -> (matchingPublicationPosition observation, matchingPublicationWitness observation))
        (nub matching)
    )

disappearanceEvidenceSnapshotSubject ::
  DisappearanceEvidenceSnapshot -> DisappearanceSubject
disappearanceEvidenceSnapshotSubject
  (DisappearanceEvidenceSnapshot subject _ _ _ _ _ _) = subject

disappearanceEvidenceSnapshotLocalPublicationCut ::
  DisappearanceEvidenceSnapshot -> HeraldPublicationPrefix
disappearanceEvidenceSnapshotLocalPublicationCut
  (DisappearanceEvidenceSnapshot _ localCut _ _ _ _ _) = localCut

disappearanceEvidenceSnapshotIncomingAlignmentCuts ::
  DisappearanceEvidenceSnapshot -> [IncomingAlignmentCut]
disappearanceEvidenceSnapshotIncomingAlignmentCuts
  (DisappearanceEvidenceSnapshot _ _ cuts _ _ _ _) = cuts

disappearanceEvidenceSnapshotOutgoingAlignmentCuts ::
  DisappearanceEvidenceSnapshot -> [OutgoingAlignmentCut]
disappearanceEvidenceSnapshotOutgoingAlignmentCuts
  (DisappearanceEvidenceSnapshot _ _ _ cuts _ _ _) = cuts

disappearanceEvidenceSnapshotBlockers ::
  DisappearanceEvidenceSnapshot -> [DisappearanceBlockerWitness]
disappearanceEvidenceSnapshotBlockers
  (DisappearanceEvidenceSnapshot _ _ _ _ blockers _ _) = blockers

disappearanceEvidenceSnapshotAbsenceAttestation ::
  DisappearanceEvidenceSnapshot -> Maybe DisappearanceAbsenceAttestation
disappearanceEvidenceSnapshotAbsenceAttestation
  (DisappearanceEvidenceSnapshot _ _ _ _ _ attestation _) = attestation

disappearanceEvidenceSnapshotMatchingPublications ::
  DisappearanceEvidenceSnapshot -> [MatchingPublicationObservation]
disappearanceEvidenceSnapshotMatchingPublications
  (DisappearanceEvidenceSnapshot _ _ _ _ _ _ matching) = matching

disappearanceEvidenceSnapshotBlockerCount ::
  DisappearanceEvidenceSnapshot -> Word64
disappearanceEvidenceSnapshotBlockerCount =
  fromIntegral . length . disappearanceEvidenceSnapshotBlockers
