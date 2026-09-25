{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared canonicalization for narrow disappearance views produced by real
-- Herald state owners.
--
-- This is construction support for the owner adapters, not an alternate state
-- owner.  In particular, a disappearance coordinator consumes the opaque views
-- exported by @Store.Disappearance@, @Application.Disappearance@,
-- @Publication.Disappearance@, and @PeerStream.Disappearance@; it does not
-- construct these facts directly.
module Eclips.Herald.Disappearance.OwnerEvidence
  ( OwnerEvidenceFact,
    ownerEvidenceFact,
    OwnerEvidenceSnapshot,
    ownerEvidenceSnapshot,
    ownerEvidenceSnapshotSubject,
    ownerEvidenceSnapshotBlockers,
    ownerEvidenceSnapshotAbsenceDigest,
    ownerEvidenceSnapshotFactCount,
    combineOwnerAbsenceDigests,
    deriveOwnerObservationDigest,
    publicationIdCanonicalBytes,
    canonicalPublicationCarriesSubjectValue,
    canonicalPublicationMatchesSubjectWrite,
    canonicalPublicationReferencesSubjectSort,
    canonicalPublicationIsSubjectRelevant,
    canonicalPublicationIsRegularRetirementResidual,
    checkedPublicationCarriesSubjectValue,
    checkedPublicationMatchesSubjectWrite,
    checkedPublicationReferencesSubjectSort,
    checkedPublicationIsSubjectRelevant,
    checkedPublicationIsRegularRetirementResidual,
    peerPublicationCarriesSubjectValue,
    peerPublicationMatchesSubjectWrite,
    peerPublicationReferencesSubjectSort,
    peerPublicationIsSubjectRelevant,
    peerPublicationIsRegularRetirementResidual,
    valueReferencesSubjectSort,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (find, sort)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Disappearance
  ( CanonicalDescriptorDigest,
    DisappearanceEvidenceDigest,
    DisappearanceSubject,
    DisappearanceSubjectView (..),
    deriveCanonicalDescriptorDigest,
    deriveDisappearanceEvidenceDigest,
    disappearanceEvidenceDigestBytes,
    disappearanceSubjectCanonicalBytes,
    disappearanceSubjectView,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    GlobalObjectId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    authorityEpochCanonicalBytes,
    globalObjectIdFromGlobalUniqueId,
    heraldEpochBytes,
    mkSortId,
    nablaIdBytes,
    nablaSequenceWord64,
    publicationAuthorityEpoch,
    publicationNabla,
    publicationNablaSequence,
    publicationSourceHeraldEpoch,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationCanonicalValue,
    checkedPublicationSort,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalCheckedDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor (descriptorControlledKeyProjection)
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (..),
    decodeSortDefinitionValue,
    predefinedCatalogueDescriptor,
    profileEntryFor,
    profileSortFor,
  )
import Eclips.Domain.Value
  ( CanonicalValueBytes,
    FieldName,
    Value,
    ValueView (BytesValue, GlobalUniqueIdValue),
    canonicalValueByteString,
    decodeCanonicalValue,
    directProjection,
    mkFieldName,
    valueAt,
    viewValue,
  )
import Eclips.Herald.Disappearance.Protocol
  ( DisappearanceBlockerClass (..),
    DisappearanceBlockerWitness,
    disappearanceBlockerWitness,
  )
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    peerPublicationBatch,
    publicationBatchCanonicalValue,
    publicationBatchControlPrerequisite,
    publicationBatchOccurrenceId,
    publicationBatchSortId,
  )

-- | One exact owner-local reason why absence cannot yet be attested.
-- Constructors stay private so an adapter must first classify actual owner
-- state; the byte payload is its stable fact identity, not copied state.
data OwnerEvidenceFact
  = OwnerEvidenceFact DisappearanceBlockerClass ByteString
  deriving stock (Eq, Ord, Show)

ownerEvidenceFact ::
  DisappearanceBlockerClass ->
  ByteString ->
  OwnerEvidenceFact
ownerEvidenceFact = OwnerEvidenceFact

-- | Normalized, subject-bound result of one owner read.  It contains neither
-- the source state nor a callback into that state.
data OwnerEvidenceSnapshot
  = OwnerEvidenceSnapshot
      DisappearanceSubject
      [DisappearanceBlockerWitness]
      (Maybe DisappearanceEvidenceDigest)
      Word64
  deriving stock (Eq, Show)

-- | Bind an owner's normalized matching fact set to the full disappearance
-- subject.  An absence digest exists only for an actually empty set; callers
-- cannot accidentally turn a non-empty view into positive evidence.
ownerEvidenceSnapshot ::
  ByteString ->
  DisappearanceSubject ->
  [OwnerEvidenceFact] ->
  OwnerEvidenceSnapshot
ownerEvidenceSnapshot ownerDomain subject unnormalizedFacts =
  OwnerEvidenceSnapshot
    subject
    (fmap blockerWitness facts)
    absence
    (fromIntegral (length facts))
  where
    facts = Set.toAscList (Set.fromList unnormalizedFacts)
    blockerWitness (OwnerEvidenceFact blockerClass payload) =
      disappearanceBlockerWitness
        blockerClass
        ( deriveOwnerEvidenceDigest
            blockerDomain
            ownerDomain
            subject
            (Builder.word8 (blockerClassTag blockerClass) <> frame payload)
        )
    absence
      | null facts =
          Just
            ( deriveOwnerEvidenceDigest
                absenceDomain
                ownerDomain
                subject
                (Builder.word64BE 0)
            )
      | otherwise = Nothing

ownerEvidenceSnapshotSubject :: OwnerEvidenceSnapshot -> DisappearanceSubject
ownerEvidenceSnapshotSubject (OwnerEvidenceSnapshot subject _ _ _) = subject

ownerEvidenceSnapshotBlockers ::
  OwnerEvidenceSnapshot ->
  [DisappearanceBlockerWitness]
ownerEvidenceSnapshotBlockers (OwnerEvidenceSnapshot _ blockers _ _) = blockers

ownerEvidenceSnapshotAbsenceDigest ::
  OwnerEvidenceSnapshot ->
  Maybe DisappearanceEvidenceDigest
ownerEvidenceSnapshotAbsenceDigest
  (OwnerEvidenceSnapshot _ _ absence _) = absence

ownerEvidenceSnapshotFactCount :: OwnerEvidenceSnapshot -> Word64
ownerEvidenceSnapshotFactCount (OwnerEvidenceSnapshot _ _ _ count) = count

-- | Combine already-empty owner contributions into one attestation category.
-- The caller still proves that all views name this exact subject before calling
-- this helper.  Sorting makes sibling-owner traversal order immaterial.
combineOwnerAbsenceDigests ::
  ByteString ->
  DisappearanceSubject ->
  NonEmpty DisappearanceEvidenceDigest ->
  DisappearanceEvidenceDigest
combineOwnerAbsenceDigests categoryDomain subject digests =
  deriveOwnerEvidenceDigest
    combinedAbsenceDomain
    categoryDomain
    subject
    ( counted
        (Builder.byteString . disappearanceEvidenceDigestBytes)
        (sort (NonEmpty.toList digests))
    )

-- | Subject-bound witness for a positioned fact such as a local matching
-- publication.  The owning adapter chooses the observation domain and exact
-- fact transcript.
deriveOwnerObservationDigest ::
  ByteString ->
  DisappearanceSubject ->
  ByteString ->
  DisappearanceEvidenceDigest
deriveOwnerObservationDigest ownerDomain subject payload =
  deriveOwnerEvidenceDigest
    observationDomain
    ownerDomain
    subject
    (frame payload)

publicationIdCanonicalBytes :: PublicationId -> ByteString
publicationIdCanonicalBytes identifier =
  build
    ( Builder.byteString (nablaIdBytes (publicationNabla identifier))
        <> frame
          (authorityEpochCanonicalBytes (publicationAuthorityEpoch identifier))
        <> Builder.byteString
          (heraldEpochBytes (publicationSourceHeraldEpoch identifier))
        <> Builder.word64BE
          (nablaSequenceWord64 (publicationNablaSequence identifier))
    )

-- | Whether a publication is a retained value whose continued existence
-- depends on the disappearance subject.
--
-- For controlled subjects the globally unique object key is sufficient; its
-- structural occurrence remains bound by the enclosing subject and cannot be
-- reused after terminal suppression.  For regular subjects both sort and the
-- exact effective sort-definition occurrence must match.
checkedPublicationCarriesSubjectValue ::
  SortDefinitionOccurrenceId ->
  DisappearanceSubject ->
  CheckedPublication ->
  Bool
checkedPublicationCarriesSubjectValue occurrence subject publication =
  canonicalPublicationCarriesSubjectValue
    subject
    (checkedPublicationSort publication)
    occurrence
    (checkedPublicationCanonicalValue publication)

-- | Whether this is a write of the disappearing subject itself.  For a
-- regular subject this means an equal @sort-sort@ descriptor publication, not
-- an ordinary value merely typed by that sort.
checkedPublicationMatchesSubjectWrite ::
  DisappearanceSubject ->
  CheckedPublication ->
  Bool
checkedPublicationMatchesSubjectWrite subject publication =
  canonicalPublicationMatchesSubjectWrite
    subject
    (checkedPublicationSort publication)
    (checkedPublicationCanonicalValue publication)

-- | Whether a Nabla/Delta carrier has not yet resolved a reference to the
-- disappearing regular sort.  The publication's own occurrence belongs to
-- the predefined carrier sort, so comparing only that outer coordinate would
-- miss the actual semantic dependency carried in @sort_id@.
checkedPublicationReferencesSubjectSort ::
  DisappearanceSubject ->
  CheckedPublication ->
  Bool
checkedPublicationReferencesSubjectSort subject publication =
  canonicalPublicationReferencesSubjectSort
    subject
    (checkedPublicationSort publication)
    (checkedPublicationCanonicalValue publication)

checkedPublicationIsSubjectRelevant ::
  SortDefinitionOccurrenceId ->
  DisappearanceSubject ->
  CheckedPublication ->
  Bool
checkedPublicationIsSubjectRelevant occurrence subject publication =
  checkedPublicationCarriesSubjectValue occurrence subject publication
    || checkedPublicationMatchesSubjectWrite subject publication
    || checkedPublicationReferencesSubjectSort subject publication

-- | Classify an owner-retained canonical publication without reconstructing a
-- 'CheckedPublication'.  Alignment snapshots and change logs retain exactly
-- these three semantic coordinates.
canonicalPublicationCarriesSubjectValue ::
  DisappearanceSubject ->
  SortId ->
  SortDefinitionOccurrenceId ->
  CanonicalValueBytes ->
  Bool
canonicalPublicationCarriesSubjectValue subject sortId occurrence canonical =
  case disappearanceSubjectView subject of
    ControlledPredefinedSubjectView object _ _ ->
      controlledCanonicalObject sortId canonical == Just object
    RegularSortDefinitionSubjectView expectedSort _ expectedOccurrence ->
      sortId == expectedSort && occurrence == expectedOccurrence

canonicalPublicationMatchesSubjectWrite ::
  DisappearanceSubject ->
  SortId ->
  CanonicalValueBytes ->
  Bool
canonicalPublicationMatchesSubjectWrite subject sortId canonical =
  case disappearanceSubjectView subject of
    ControlledPredefinedSubjectView object _ _ ->
      controlledCanonicalObject sortId canonical == Just object
    RegularSortDefinitionSubjectView expectedSort descriptorDigest _ ->
      canonicalPublicationDefines
        expectedSort
        descriptorDigest
        sortId
        canonical

canonicalPublicationReferencesSubjectSort ::
  DisappearanceSubject ->
  SortId ->
  CanonicalValueBytes ->
  Bool
canonicalPublicationReferencesSubjectSort subject publicationSort canonical =
  publicationSort `elem` structuralRootCarrierSorts
    && either
      (const False)
      (valueReferencesSubjectSort subject)
      (decodeCanonicalValue (canonicalValueByteString canonical))

canonicalPublicationIsSubjectRelevant ::
  DisappearanceSubject ->
  SortId ->
  SortDefinitionOccurrenceId ->
  CanonicalValueBytes ->
  Bool
canonicalPublicationIsSubjectRelevant subject sortId occurrence canonical =
  canonicalPublicationCarriesSubjectValue subject sortId occurrence canonical
    || canonicalPublicationMatchesSubjectWrite subject sortId canonical
    || canonicalPublicationReferencesSubjectSort subject sortId canonical

-- | Whether a retained publication still contradicts one already committed
-- regular-definition retirement.  An ordinary value typed by the exact retired
-- occurrence is always stale.  Equal definition writes and structural carriers
-- naming the same public sort, however, belong to the successor epoch once their
-- retained control prerequisite reaches the Resolve index.
--
-- A missing prerequisite is deliberately conservative.  Primordial alignment
-- evidence, for example, cannot prove that it was admitted in the successor
-- epoch and therefore remains a residual blocker.
canonicalPublicationIsRegularRetirementResidual ::
  DisappearanceSubject ->
  ControlIndex ->
  SortId ->
  SortDefinitionOccurrenceId ->
  CanonicalValueBytes ->
  Maybe ControlIndex ->
  Bool
canonicalPublicationIsRegularRetirementResidual
  subject
  resolveIndex
  sortId
  occurrence
  canonical
  controlPrerequisite =
    canonicalPublicationCarriesSubjectValue
      subject
      sortId
      occurrence
      canonical
      || ( ( canonicalPublicationMatchesSubjectWrite subject sortId canonical
               || canonicalPublicationReferencesSubjectSort subject sortId canonical
           )
             && maybe True (< resolveIndex) controlPrerequisite
         )

-- | Checked-publication form of
-- 'canonicalPublicationIsRegularRetirementResidual'.  The owner supplies the
-- occurrence under which it retained the checked value and whatever ordered
-- control provenance it can still prove.
checkedPublicationIsRegularRetirementResidual ::
  SortDefinitionOccurrenceId ->
  DisappearanceSubject ->
  ControlIndex ->
  Maybe ControlIndex ->
  CheckedPublication ->
  Bool
checkedPublicationIsRegularRetirementResidual
  occurrence
  subject
  resolveIndex
  controlPrerequisite
  publication =
    canonicalPublicationIsRegularRetirementResidual
      subject
      resolveIndex
      (checkedPublicationSort publication)
      occurrence
      (checkedPublicationCanonicalValue publication)
      controlPrerequisite

peerPublicationCarriesSubjectValue ::
  DisappearanceSubject ->
  PeerPublication ->
  Bool
peerPublicationCarriesSubjectValue subject publication =
  canonicalPublicationCarriesSubjectValue
    subject
    (publicationBatchSortId batch)
    batchOccurrence
    (publicationBatchCanonicalValue batch)
  where
    batch = peerPublicationBatch publication
    batchOccurrence = publicationBatchOccurrenceId batch

peerPublicationMatchesSubjectWrite ::
  DisappearanceSubject ->
  PeerPublication ->
  Bool
peerPublicationMatchesSubjectWrite subject publication =
  canonicalPublicationMatchesSubjectWrite
    subject
    (publicationBatchSortId batch)
    (publicationBatchCanonicalValue batch)
  where
    batch = peerPublicationBatch publication

peerPublicationReferencesSubjectSort ::
  DisappearanceSubject ->
  PeerPublication ->
  Bool
peerPublicationReferencesSubjectSort subject publication =
  canonicalPublicationReferencesSubjectSort
    subject
    (publicationBatchSortId batch)
    (publicationBatchCanonicalValue batch)
  where
    batch = peerPublicationBatch publication

peerPublicationIsSubjectRelevant ::
  DisappearanceSubject ->
  PeerPublication ->
  Bool
peerPublicationIsSubjectRelevant subject publication =
  peerPublicationCarriesSubjectValue subject publication
    || peerPublicationMatchesSubjectWrite subject publication
    || peerPublicationReferencesSubjectSort subject publication

-- | Peer-publication form of
-- 'canonicalPublicationIsRegularRetirementResidual'.  Peer batches always carry
-- their exact control prerequisite, so a matching successor-epoch definition or
-- structural reference is no longer a blocker at or beyond Resolve.
peerPublicationIsRegularRetirementResidual ::
  DisappearanceSubject ->
  ControlIndex ->
  PeerPublication ->
  Bool
peerPublicationIsRegularRetirementResidual subject resolveIndex publication =
  canonicalPublicationIsRegularRetirementResidual
    subject
    resolveIndex
    (publicationBatchSortId batch)
    (publicationBatchOccurrenceId batch)
    (publicationBatchCanonicalValue batch)
    (Just (publicationBatchControlPrerequisite batch))
  where
    batch = peerPublicationBatch publication

-- | Read the carried sort reference from an already admitted structural-root
-- value.  Callers which lack the outer publication sort use this only for
-- controlled-origin fence-held work; checked/peer callers additionally verify
-- that the outer sort is the predefined Nabla or Delta carrier.
valueReferencesSubjectSort :: DisappearanceSubject -> Value -> Bool
valueReferencesSubjectSort subject value =
  case disappearanceSubjectView subject of
    ControlledPredefinedSubjectView {} -> False
    RegularSortDefinitionSubjectView expectedSort _ _ ->
      carriedSort value == Just expectedSort

carriedSort :: Value -> Maybe SortId
carriedSort value = do
  projected <- either (const Nothing) Just (valueAt (directProjection sortIdField) value)
  case viewValue projected of
    BytesValue bytes -> either (const Nothing) Just (mkSortId bytes)
    _ -> Nothing

structuralRootCarrierSorts :: [SortId]
structuralRootCarrierSorts =
  [ profileSortFor NablaRole,
    profileSortFor DeltaRole
  ]

sortIdField :: FieldName
sortIdField =
  case mkFieldName "sort_id" of
    Right field -> field
    Left problem -> error ("sort_id field invariant: " <> show problem)

canonicalPublicationDefines ::
  SortId ->
  CanonicalDescriptorDigest ->
  SortId ->
  CanonicalValueBytes ->
  Bool
canonicalPublicationDefines sortId descriptorDigest publicationSort canonical =
  publicationSort == profileSortFor SortDefinitionRole
    && case decodeCanonicalValue
      (canonicalValueByteString canonical) of
      Right value -> case decodeSortDefinitionValue value of
        Right descriptor -> descriptorMatches sortId descriptorDigest descriptor
        Left _ -> False
      Left _ -> False

descriptorMatches ::
  SortId ->
  CanonicalDescriptorDigest ->
  CanonicalDescriptor ->
  Bool
descriptorMatches sortId descriptorDigest descriptor =
  descriptorSortId descriptor == sortId
    && deriveCanonicalDescriptorDigest descriptor == descriptorDigest

controlledCanonicalObject ::
  SortId -> CanonicalValueBytes -> Maybe GlobalObjectId
controlledCanonicalObject sortId canonical = do
  descriptor <- controlledDescriptorForSort sortId
  projection <- descriptorControlledKeyProjection (canonicalCheckedDescriptor descriptor)
  value <-
    either
      (const Nothing)
      Just
      (decodeCanonicalValue (canonicalValueByteString canonical))
  projected <-
    either
      (const Nothing)
      Just
      (valueAt projection value)
  case viewValue projected of
    GlobalUniqueIdValue identifier ->
      Just (globalObjectIdFromGlobalUniqueId identifier)
    _ -> Nothing

controlledDescriptorForSort :: SortId -> Maybe CanonicalDescriptor
controlledDescriptorForSort sortId =
  predefinedCatalogueDescriptor . profileEntryFor
    <$> find ((== sortId) . profileSortFor) controlledSubjectRoles

controlledSubjectRoles :: [PredefinedSortRole]
controlledSubjectRoles = [NeutralVertexRole, EdgeRole, NablaRole, DeltaRole]

deriveOwnerEvidenceDigest ::
  ByteString ->
  ByteString ->
  DisappearanceSubject ->
  Builder.Builder ->
  DisappearanceEvidenceDigest
deriveOwnerEvidenceDigest domain ownerDomain subject body =
  deriveDisappearanceEvidenceDigest
    ( build
        ( frame domain
            <> frame ownerDomain
            <> frame (disappearanceSubjectCanonicalBytes subject)
            <> body
        )
    )

blockerClassTag :: DisappearanceBlockerClass -> Word8
blockerClassTag blockerClass = case blockerClass of
  VisibleApplicationCopyBlocker -> 0
  UnsequencedPublicationBlocker -> 1
  HeldPublicationBlocker -> 2
  PeerInboxBlocker -> 3
  PeerOutboxBlocker -> 4
  RetainedStoreValueBlocker -> 5
  CurrentControlledUseBlocker -> 6
  ObsoleteControlledUseBlocker -> 7
  ActiveNablaBlocker -> 8
  PassiveNablaBlocker -> 9
  ActiveDeltaBlocker -> 10
  PassiveDeltaBlocker -> 11
  UnresolvedStructuralReferenceBlocker -> 12
  RetainedPublicationStageBlocker -> 13
  PublicationDependencyHoldBlocker -> 14
  RouteDependencyBlocker -> 15
  PlacementDependencyBlocker -> 16
  AlignmentDebtBlocker -> 17
  AlignmentObligationBlocker -> 18
  AlignmentAttemptBlocker -> 19
  AlignmentSubscriptionBlocker -> 20
  AlignmentSnapshotBlocker -> 21
  AlignmentChangeLogBlocker -> 22
  AlignmentCertificateBlocker -> 23
  UncertainSemanticPayloadBlocker -> 24

blockerDomain, absenceDomain, combinedAbsenceDomain, observationDomain :: ByteString
blockerDomain = "ECLIPS-DISAPPEARANCE-OWNER-BLOCKER"
absenceDomain = "ECLIPS-DISAPPEARANCE-OWNER-ABSENCE"
combinedAbsenceDomain = "ECLIPS-DISAPPEARANCE-COMBINED-OWNER-ABSENCE"
observationDomain = "ECLIPS-DISAPPEARANCE-OWNER-OBSERVATION"

frame :: ByteString -> Builder.Builder
frame bytes =
  Builder.word64BE (fromIntegral (ByteString.length bytes))
    <> Builder.byteString bytes

counted :: (value -> Builder.Builder) -> [value] -> Builder.Builder
counted put values =
  Builder.word64BE (fromIntegral (length values))
    <> foldMap put values

build :: Builder.Builder -> ByteString
build = LazyByteString.toStrict . Builder.toLazyByteString
