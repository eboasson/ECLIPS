{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read-only, subject-qualified disappearance evidence owned by Publication.
--
-- Immutable completed publication history is deliberately not a blocker.
-- Outstanding unstamped structural work, unstabilized stamped stages, and
-- dependency-held incoming publications are.  The view separately retains all
-- owner-known local matching writes with their actual Herald publication
-- positions so the disappearance leaf can apply its opaque post-cut gate.
module Eclips.Herald.Publication.Disappearance
  ( PublicationDisappearanceView,
    publicationDisappearanceView,
    publicationRegularRetirementDisappearanceView,
    publicationDisappearanceSubject,
    publicationDisappearanceBlockers,
    publicationDisappearanceAbsenceDigest,
    publicationDisappearanceFactCount,
    publicationDisappearanceCurrentPrefix,
    publicationDisappearanceMatchingObservations,
    publicationDisappearanceMatchingObservationCount,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (sortOn)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment
  ( HeraldPublicationPosition,
    HeraldPublicationPrefix,
    heraldPublicationPositionWord64,
  )
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceDigest,
    DisappearanceSubject,
    DisappearanceSubjectView (..),
    disappearanceSubjectView,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    PublicationId,
    SortDefinitionOccurrenceId,
    heraldEpochBytes,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationCanonicalValue,
    checkedPublicationId,
    checkedPublicationSort,
  )
import Eclips.Herald.Disappearance.OwnerEvidence
  ( OwnerEvidenceSnapshot,
    canonicalPublicationIsRegularRetirementResidual,
    checkedPublicationIsSubjectRelevant,
    checkedPublicationMatchesSubjectWrite,
    deriveOwnerObservationDigest,
    ownerEvidenceFact,
    ownerEvidenceSnapshot,
    ownerEvidenceSnapshotAbsenceDigest,
    ownerEvidenceSnapshotBlockers,
    ownerEvidenceSnapshotFactCount,
    ownerEvidenceSnapshotSubject,
    peerPublicationIsRegularRetirementResidual,
    peerPublicationIsSubjectRelevant,
    publicationIdCanonicalBytes,
  )
import Eclips.Herald.Disappearance.Protocol
  ( DisappearanceBlockerClass (..),
    DisappearanceBlockerWitness,
    MatchingPublicationObservation,
    matchingPublicationObservation,
    matchingPublicationPosition,
  )
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    peerPublicationBatch,
    peerPublicationDigest,
    publicationBatchId,
  )
import Eclips.Herald.PeerStream
  ( peerItemDigestBytes,
    streamDirectionDestination,
    streamDirectionSource,
  )
import Eclips.Herald.Publication.State qualified as Publication

data PublicationDisappearanceView
  = PublicationDisappearanceView
      HeraldPublicationPrefix
      OwnerEvidenceSnapshot
      [MatchingPublicationObservation]
  deriving stock (Eq, Show)

type RetainedPublicationRelevance =
  SortDefinitionOccurrenceId ->
  ControlIndex ->
  CheckedPublication ->
  Bool

type MatchingWriteRelevance =
  SortDefinitionOccurrenceId ->
  ControlIndex ->
  CheckedPublication ->
  Bool

publicationDisappearanceView ::
  DisappearanceSubject ->
  Publication.State ->
  PublicationDisappearanceView
publicationDisappearanceView subject state =
  publicationDisappearanceViewWith
    subject
    (\occurrence _ -> checkedPublicationIsSubjectRelevant occurrence subject)
    (peerPublicationIsSubjectRelevant subject)
    (\_ _ -> checkedPublicationMatchesSubjectWrite subject)
    state

-- | Read Publication after a regular sort-definition retirement has already
-- committed.  Work naming the exact retired occurrence remains stale
-- regardless of when it was admitted.  An equal definition write or a
-- structural carrier naming the same public sort belongs to the successor
-- epoch, however, when its retained control prerequisite is at or beyond the
-- Resolve index.
publicationRegularRetirementDisappearanceView ::
  DisappearanceSubject ->
  ControlIndex ->
  Publication.State ->
  PublicationDisappearanceView
publicationRegularRetirementDisappearanceView subject resolveIndex state =
  publicationDisappearanceViewWith
    subject
    retainedIsResidual
    (peerPublicationIsRegularRetirementResidual subject resolveIndex)
    matchingWriteIsResidual
    state
  where
    retainedIsResidual occurrence controlPrerequisite checked =
      canonicalPublicationIsRegularRetirementResidual
        subject
        resolveIndex
        (checkedPublicationSort checked)
        occurrence
        (checkedPublicationCanonicalValue checked)
        (Just controlPrerequisite)
    matchingWriteIsResidual occurrence controlPrerequisite checked =
      checkedPublicationMatchesSubjectWrite subject checked
        && retainedIsResidual occurrence controlPrerequisite checked

publicationDisappearanceViewWith ::
  DisappearanceSubject ->
  RetainedPublicationRelevance ->
  (PeerPublication -> Bool) ->
  MatchingWriteRelevance ->
  Publication.State ->
  PublicationDisappearanceView
publicationDisappearanceViewWith
  subject
  retainedIsRelevant
  incomingIsRelevant
  matchingWriteIsRelevant
  state =
    PublicationDisappearanceView
      (Publication.publicationCurrentHeraldPrefix state)
      ( ownerEvidenceSnapshot
          publicationEvidenceDomain
          subject
          (unstampedFacts <> stampedFacts <> incomingFacts)
      )
      matchingObservations
    where
      unstamped = Publication.unstampedStructuralSourceStageEntries state
      unstampedFacts = do
        source <- unstamped
        let checked = Publication.structuralSourceChecked source
            occurrence = Publication.structuralSourceSortOccurrenceId source
            controlPrerequisite =
              Publication.structuralSourceControlPrerequisite source
        if retainedIsRelevant occurrence controlPrerequisite checked
          then
            pure
              ( ownerEvidenceFact
                  UnsequencedPublicationBlocker
                  ( positionedPublicationFactBytes
                      (Publication.structuralSourceHeraldPosition source)
                      checked
                  )
              )
          else []
      stampedFacts = do
        (structuralOccurrence, stage) <-
          Publication.stampedStructuralStageEntries state
        if Publication.lookupStructuralStabilization structuralOccurrence state
          == Nothing
          then pure ()
          else []
        let checked = Publication.stampedStructuralChecked stage
            occurrence = Publication.stampedStructuralSortOccurrenceId stage
            controlPrerequisite =
              Publication.stampedStructuralControlPrerequisite stage
        if retainedIsRelevant occurrence controlPrerequisite checked
          then
            pure
              ( ownerEvidenceFact
                  RetainedPublicationStageBlocker
                  ( positionedPublicationFactBytes
                      (Publication.stampedStructuralHeraldPosition stage)
                      checked
                  )
              )
          else []
      incomingFacts = do
        (identifier, incoming) <- Publication.incomingPublicationEntries state
        if Publication.incomingPublicationDisposition incoming
          == Publication.DependencyHeld
          then pure ()
          else []
        if Set.null (Publication.incomingPublicationDisappearanceGates incoming)
          then pure ()
          else []
        if incomingIsRelevant (Publication.incomingPeerPublication incoming)
          then
            pure
              ( ownerEvidenceFact
                  (incomingBlockerClass subject incoming)
                  (incomingPublicationFactBytes identifier incoming)
              )
          else []
      matchingObservations =
        sortOn
          observationPosition
          ( outgoingMatchingObservations
              subject
              matchingWriteIsRelevant
              state
              <> fmapMaybeUnstamped subject matchingWriteIsRelevant unstamped
              <> stampedMatchingObservations
                subject
                matchingWriteIsRelevant
                state
          )

publicationDisappearanceSubject ::
  PublicationDisappearanceView ->
  DisappearanceSubject
publicationDisappearanceSubject
  (PublicationDisappearanceView _ snapshot _) =
    ownerEvidenceSnapshotSubject snapshot

publicationDisappearanceBlockers ::
  PublicationDisappearanceView ->
  [DisappearanceBlockerWitness]
publicationDisappearanceBlockers
  (PublicationDisappearanceView _ snapshot _) =
    ownerEvidenceSnapshotBlockers snapshot

publicationDisappearanceAbsenceDigest ::
  PublicationDisappearanceView ->
  Maybe DisappearanceEvidenceDigest
publicationDisappearanceAbsenceDigest
  (PublicationDisappearanceView _ snapshot _) =
    ownerEvidenceSnapshotAbsenceDigest snapshot

publicationDisappearanceFactCount :: PublicationDisappearanceView -> Word64
publicationDisappearanceFactCount
  (PublicationDisappearanceView _ snapshot _) =
    ownerEvidenceSnapshotFactCount snapshot

publicationDisappearanceCurrentPrefix ::
  PublicationDisappearanceView ->
  HeraldPublicationPrefix
publicationDisappearanceCurrentPrefix
  (PublicationDisappearanceView prefix _ _) = prefix

publicationDisappearanceMatchingObservations ::
  PublicationDisappearanceView ->
  [MatchingPublicationObservation]
publicationDisappearanceMatchingObservations
  (PublicationDisappearanceView _ _ observations) = observations

publicationDisappearanceMatchingObservationCount ::
  PublicationDisappearanceView ->
  Word64
publicationDisappearanceMatchingObservationCount =
  fromIntegral . length . publicationDisappearanceMatchingObservations

incomingBlockerClass ::
  DisappearanceSubject ->
  Publication.IncomingPublicationRecord ->
  DisappearanceBlockerClass
incomingBlockerClass subject incoming = case disappearanceSubjectView subject of
  ControlledPredefinedSubjectView {} -> HeldPublicationBlocker
  RegularSortDefinitionSubjectView {}
    | any
        isStructuralDependency
        (Set.toAscList (Publication.incomingPublicationDependencies incoming)) ->
        UnresolvedStructuralReferenceBlocker
    | otherwise -> PublicationDependencyHoldBlocker

isStructuralDependency :: Publication.PublicationDependency -> Bool
isStructuralDependency dependency = case dependency of
  Publication.StructuralApplicationDependency {} -> True
  _ -> False

outgoingMatchingObservations ::
  DisappearanceSubject ->
  MatchingWriteRelevance ->
  Publication.State ->
  [MatchingPublicationObservation]
outgoingMatchingObservations subject matchingWriteIsRelevant state = do
  (_, outgoing) <- Publication.outgoingPublicationEntries state
  let checked = Publication.outgoingPublicationChecked outgoing
      occurrence = Publication.outgoingPublicationOccurrenceId outgoing
      position = Publication.outgoingPublicationHeraldPosition outgoing
      controlPrerequisite =
        Publication.outgoingPublicationControlPrerequisite outgoing
  if matchingWriteIsRelevant occurrence controlPrerequisite checked
    then pure (positionedObservation subject position checked)
    else []

fmapMaybeUnstamped ::
  DisappearanceSubject ->
  MatchingWriteRelevance ->
  [Publication.StructuralSourceStage] ->
  [MatchingPublicationObservation]
fmapMaybeUnstamped subject matchingWriteIsRelevant sources = do
  source <- sources
  let checked = Publication.structuralSourceChecked source
      occurrence = Publication.structuralSourceSortOccurrenceId source
      position = Publication.structuralSourceHeraldPosition source
      controlPrerequisite =
        Publication.structuralSourceControlPrerequisite source
  if matchingWriteIsRelevant occurrence controlPrerequisite checked
    then pure (positionedObservation subject position checked)
    else []

stampedMatchingObservations ::
  DisappearanceSubject ->
  MatchingWriteRelevance ->
  Publication.State ->
  [MatchingPublicationObservation]
stampedMatchingObservations subject matchingWriteIsRelevant state = do
  (_, stage) <- Publication.stampedStructuralStageEntries state
  let checked = Publication.stampedStructuralChecked stage
      occurrence = Publication.stampedStructuralSortOccurrenceId stage
      position = Publication.stampedStructuralHeraldPosition stage
      controlPrerequisite =
        Publication.stampedStructuralControlPrerequisite stage
  if matchingWriteIsRelevant occurrence controlPrerequisite checked
    then pure (positionedObservation subject position checked)
    else []

positionedObservation ::
  DisappearanceSubject ->
  HeraldPublicationPosition ->
  CheckedPublication ->
  MatchingPublicationObservation
positionedObservation subject position checked =
  matchingPublicationObservation
    subject
    position
    ( deriveOwnerObservationDigest
        publicationMatchingWriteDomain
        subject
        (positionedPublicationFactBytes position checked)
    )

observationPosition :: MatchingPublicationObservation -> HeraldPublicationPosition
observationPosition = matchingPublicationPosition

positionedPublicationFactBytes ::
  HeraldPublicationPosition ->
  CheckedPublication ->
  ByteString
positionedPublicationFactBytes position checked =
  build
    ( Builder.word64BE (heraldPublicationPositionWord64 position)
        <> frame
          (publicationIdCanonicalBytes (checkedPublicationId checked))
    )

incomingPublicationFactBytes ::
  PublicationId ->
  Publication.IncomingPublicationRecord ->
  ByteString
incomingPublicationFactBytes identifier incoming =
  build
    ( frame (publicationIdCanonicalBytes identifier)
        <> Builder.byteString
          ( heraldEpochBytes
              (streamDirectionSource direction)
          )
        <> Builder.byteString
          ( heraldEpochBytes
              (streamDirectionDestination direction)
          )
        <> Builder.byteString
          ( peerItemDigestBytes
              (peerPublicationDigest peerPublication)
          )
        <> frame
          ( publicationIdCanonicalBytes
              (publicationBatchId (peerPublicationBatch peerPublication))
          )
    )
  where
    direction = Publication.incomingPublicationDirection incoming
    peerPublication = Publication.incomingPeerPublication incoming

publicationEvidenceDomain, publicationMatchingWriteDomain :: ByteString
publicationEvidenceDomain = "ECLIPS-DISAPPEARANCE-PUBLICATION-WORK"
publicationMatchingWriteDomain =
  "ECLIPS-DISAPPEARANCE-PUBLICATION-MATCHING-WRITE"

frame :: ByteString -> Builder.Builder
frame bytes =
  Builder.word64BE (fromIntegral (ByteString.length bytes))
    <> Builder.byteString bytes

build :: Builder.Builder -> ByteString
build = LazyByteString.toStrict . Builder.toLazyByteString
