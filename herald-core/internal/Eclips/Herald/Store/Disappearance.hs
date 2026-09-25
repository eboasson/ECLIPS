{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read-only, subject-qualified disappearance evidence owned by Store.
--
-- The view is detached from live Store state and its constructor is hidden.
-- Hidden retained copies of a controlled object are terminal purge material,
-- not absence blockers: a controlled candidate is created by a local take,
-- which deliberately preserves that retained winner.  Regular retirement is
-- stricter and accounts for retained values typed by the exact disappearing
-- sort occurrence.
module Eclips.Herald.Store.Disappearance
  ( StoreDisappearanceView,
    storeDisappearanceView,
    storeRegularRetirementDisappearanceView,
    storeDisappearanceSubject,
    storeDisappearanceBlockers,
    storeDisappearanceAbsenceDigest,
    storeDisappearanceFactCount,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Word (Word64)
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceDigest,
    DisappearanceSubject,
    DisappearanceSubjectView (..),
    disappearanceSubjectView,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    deltaIdBytes,
    storeIncarnationIdBytes,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationId,
  )
import Eclips.Domain.Store
  ( retainedInstances,
    storedPublication,
    visibleInstances,
  )
import Eclips.Herald.Disappearance.OwnerEvidence
  ( OwnerEvidenceSnapshot,
    checkedPublicationCarriesSubjectValue,
    checkedPublicationIsRegularRetirementResidual,
    checkedPublicationIsSubjectRelevant,
    ownerEvidenceFact,
    ownerEvidenceSnapshot,
    ownerEvidenceSnapshotAbsenceDigest,
    ownerEvidenceSnapshotBlockers,
    ownerEvidenceSnapshotFactCount,
    ownerEvidenceSnapshotSubject,
    publicationIdCanonicalBytes,
  )
import Eclips.Herald.Disappearance.Protocol
  ( DisappearanceBlockerClass (..),
    DisappearanceBlockerWitness,
  )
import Eclips.Herald.Store.State
  ( State,
    StoreObservationOrigin (..),
    StoreProvenance (..),
    StoreSlot,
    retainedStoreObservationOrigin,
    retainedStoreObservationPublication,
    retainedStoreSlots,
    storeObservationEnvelopeControlPrerequisite,
    storeSlotApplicationObservations,
    storeSlotContents,
    storeSlotDelta,
    storeSlotIncarnation,
    storeSlotOccurrenceId,
    storeSlotProvenance,
    storeSlots,
  )

newtype StoreDisappearanceView
  = StoreDisappearanceView OwnerEvidenceSnapshot
  deriving stock (Eq, Show)

storeDisappearanceView ::
  DisappearanceSubject ->
  State ->
  StoreDisappearanceView
storeDisappearanceView subject =
  storeDisappearanceViewWith
    subject
    ( \slot ->
        checkedPublicationIsSubjectRelevant
          (storeSlotOccurrenceId slot)
          subject
    )

-- | Read Store after a regular sort-definition Resolve has committed.  Values
-- typed by the exact retired occurrence remain blockers.  A matching
-- definition or structural carrier in an application-visible effective Store
-- is successor work only when that same checked publication has retained
-- routed observation evidence at or beyond Resolve; primordial or missing
-- provenance remains conservatively blocking.
storeRegularRetirementDisappearanceView ::
  DisappearanceSubject ->
  ControlIndex ->
  State ->
  StoreDisappearanceView
storeRegularRetirementDisappearanceView subject resolveIndex =
  storeDisappearanceViewWith subject visiblePublicationIsResidual
  where
    visiblePublicationIsResidual slot publication =
      checkedPublicationIsRegularRetirementResidual
        (storeSlotOccurrenceId slot)
        subject
        resolveIndex
        (latestRoutedPrerequisite slot publication)
        publication

storeDisappearanceViewWith ::
  DisappearanceSubject ->
  (StoreSlot -> CheckedPublication -> Bool) ->
  State ->
  StoreDisappearanceView
storeDisappearanceViewWith subject visiblePublicationIsRelevant state =
  StoreDisappearanceView
    ( ownerEvidenceSnapshot
        storeEvidenceDomain
        subject
        (visibleFacts <> retainedFacts)
    )
  where
    visibleFacts = do
      slot <- storeSlots state
      if applicationVisible (storeSlotProvenance slot) then pure () else []
      (_, stored) <- visibleInstances (storeSlotContents slot)
      let publication = storedPublication stored
      if visiblePublicationIsRelevant slot publication
        then
          pure
            ( ownerEvidenceFact
                VisibleApplicationCopyBlocker
                (storeFactBytes slot publication)
            )
        else []
    retainedFacts = case disappearanceSubjectView subject of
      ControlledPredefinedSubjectView {} -> []
      RegularSortDefinitionSubjectView {} -> do
        slot <- retainedStoreSlots state
        (_, stored) <- retainedInstances (storeSlotContents slot)
        let publication = storedPublication stored
        if checkedPublicationCarriesSubjectValue
          (storeSlotOccurrenceId slot)
          subject
          publication
          then
            pure
              ( ownerEvidenceFact
                  RetainedStoreValueBlocker
                  (storeFactBytes slot publication)
              )
          else []

latestRoutedPrerequisite ::
  StoreSlot ->
  CheckedPublication ->
  Maybe ControlIndex
latestRoutedPrerequisite slot publication =
  foldl' retainLatest Nothing matchingPrerequisites
  where
    matchingPrerequisites = do
      observation <- storeSlotApplicationObservations slot
      if retainedStoreObservationPublication observation == publication
        then pure ()
        else []
      case retainedStoreObservationOrigin observation of
        PrimordialStoreObservation -> []
        RoutedStoreObservation envelope ->
          pure (storeObservationEnvelopeControlPrerequisite envelope)
    retainLatest Nothing prerequisite = Just prerequisite
    retainLatest (Just current) prerequisite = Just (max current prerequisite)

storeDisappearanceSubject :: StoreDisappearanceView -> DisappearanceSubject
storeDisappearanceSubject (StoreDisappearanceView snapshot) =
  ownerEvidenceSnapshotSubject snapshot

storeDisappearanceBlockers ::
  StoreDisappearanceView ->
  [DisappearanceBlockerWitness]
storeDisappearanceBlockers (StoreDisappearanceView snapshot) =
  ownerEvidenceSnapshotBlockers snapshot

storeDisappearanceAbsenceDigest ::
  StoreDisappearanceView ->
  Maybe DisappearanceEvidenceDigest
storeDisappearanceAbsenceDigest (StoreDisappearanceView snapshot) =
  ownerEvidenceSnapshotAbsenceDigest snapshot

storeDisappearanceFactCount :: StoreDisappearanceView -> Word64
storeDisappearanceFactCount (StoreDisappearanceView snapshot) =
  ownerEvidenceSnapshotFactCount snapshot

applicationVisible :: StoreProvenance -> Bool
applicationVisible provenance = case provenance of
  HeraldSystemView {} -> False
  ApplicationReader {} -> True
  StructuralBaselineReader {} -> True
  StructuralApplicationReader {} -> True
  StructuralControlReader {} -> True

storeFactBytes :: StoreSlot -> CheckedPublication -> ByteString
storeFactBytes slot publication =
  LazyByteString.toStrict
    ( Builder.toLazyByteString
        ( Builder.byteString (deltaIdBytes (storeSlotDelta slot))
            <> Builder.byteString
              (storeIncarnationIdBytes (storeSlotIncarnation slot))
            <> frame
              ( publicationIdCanonicalBytes
                  (checkedPublicationId publication)
              )
        )
    )

storeEvidenceDomain :: ByteString
storeEvidenceDomain = "ECLIPS-DISAPPEARANCE-STORE-WORK"

frame :: ByteString -> Builder.Builder
frame bytes =
  Builder.word64BE (fromIntegral (ByteString.length bytes))
    <> Builder.byteString bytes
