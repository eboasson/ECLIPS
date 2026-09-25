{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read-only, subject-qualified disappearance evidence owned by Application.
--
-- Fence-held publications have not entered Publication or PeerStream yet, so
-- their accepted semantic payload and exact acceptance-time source coordinate
-- must participate in the negative-evidence cut here.
module Eclips.Herald.Application.Disappearance
  ( ApplicationDisappearanceView,
    applicationDisappearanceView,
    applicationRegularRetirementDisappearanceView,
    applicationDisappearanceViewWithGates,
    applicationRegularRetirementDisappearanceViewWithGates,
    applicationDisappearanceSubject,
    applicationDisappearanceBlockers,
    applicationDisappearanceAbsenceDigest,
    applicationDisappearanceFactCount,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceDigest,
    DisappearanceProbeId,
    DisappearanceSubject,
    DisappearanceSubjectView (..),
    deriveCanonicalDescriptorDigest,
    disappearanceSubjectView,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    GlobalObjectId,
    controlIndexWord64,
    globalObjectIdFromGlobalUniqueId,
    nablaIdBytes,
    processEpochIdBytes,
    sortDefinitionOccurrenceIdBytes,
    sortIdBytes,
  )
import Eclips.Domain.Label
  ( labelProcessAcceptancePositionOrdinal,
    labelProcessAcceptancePositionProcess,
  )
import Eclips.Domain.Sort.Canonical (descriptorSortId)
import Eclips.Domain.Value
  ( canonicalValueByteString,
    canonicalValueBytes,
  )
import Eclips.Herald.Application.PublicationEvidence qualified as Publication
import Eclips.Herald.Application.SortDefinition qualified as SortDefinition
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Disappearance.OwnerEvidence
  ( OwnerEvidenceSnapshot,
    canonicalPublicationReferencesSubjectSort,
    ownerEvidenceFact,
    ownerEvidenceSnapshot,
    ownerEvidenceSnapshotAbsenceDigest,
    ownerEvidenceSnapshotBlockers,
    ownerEvidenceSnapshotFactCount,
    ownerEvidenceSnapshotSubject,
  )
import Eclips.Herald.Disappearance.Protocol
  ( DisappearanceBlockerClass (..),
    DisappearanceBlockerWitness,
  )

newtype ApplicationDisappearanceView
  = ApplicationDisappearanceView OwnerEvidenceSnapshot
  deriving stock (Eq, Show)

applicationDisappearanceView ::
  DisappearanceSubject ->
  Application.State ->
  ApplicationDisappearanceView
applicationDisappearanceView = applicationDisappearanceViewWithGates Set.empty

applicationDisappearanceViewWithGates :: Set.Set DisappearanceProbeId -> DisappearanceSubject -> Application.State -> ApplicationDisappearanceView
applicationDisappearanceViewWithGates gates subject =
  applicationDisappearanceViewWith gates subject (heldBlockerClass subject)

-- | Read Application after a regular sort-definition retirement has already
-- committed.  Fence-held equal definition writes and structural carriers
-- naming the public sort are residual only when their immutable acceptance
-- prerequisite precedes Resolve.  Controlled-subject evidence and regular
-- work whose source occurrence Application cannot prove remain strict.
applicationRegularRetirementDisappearanceView ::
  DisappearanceSubject ->
  ControlIndex ->
  Application.State ->
  ApplicationDisappearanceView
applicationRegularRetirementDisappearanceView = applicationRegularRetirementDisappearanceViewWithGates Set.empty

applicationRegularRetirementDisappearanceViewWithGates :: Set.Set DisappearanceProbeId -> DisappearanceSubject -> ControlIndex -> Application.State -> ApplicationDisappearanceView
applicationRegularRetirementDisappearanceViewWithGates gates subject resolveIndex =
  applicationDisappearanceViewWith
    gates
    subject
    (regularRetirementHeldBlockerClass subject resolveIndex)

applicationDisappearanceViewWith ::
  Set.Set DisappearanceProbeId ->
  DisappearanceSubject ->
  (Application.ApplicationFenceHeldWork -> Maybe DisappearanceBlockerClass) ->
  Application.State ->
  ApplicationDisappearanceView
applicationDisappearanceViewWith gates subject classifyHeld state =
  ApplicationDisappearanceView
    ( ownerEvidenceSnapshot
        applicationEvidenceDomain
        subject
        facts
    )
  where
    facts = do
      held <- Application.applicationFenceHeldEntries state
      -- A disappearance-gated write has no publication effects before the
      -- terminal. Its exact gate, rather than negative-cut evidence, owns it.
      if Set.disjoint gates (Application.applicationFenceHeldDisappearanceProbes held) then pure () else []
      let work = Application.applicationFenceHeldSemanticWork held
      blockerClass <- maybe [] pure (classifyHeld work)
      pure
        ( ownerEvidenceFact
            blockerClass
            (heldFactBytes held)
        )

applicationDisappearanceSubject ::
  ApplicationDisappearanceView ->
  DisappearanceSubject
applicationDisappearanceSubject (ApplicationDisappearanceView snapshot) =
  ownerEvidenceSnapshotSubject snapshot

applicationDisappearanceBlockers ::
  ApplicationDisappearanceView ->
  [DisappearanceBlockerWitness]
applicationDisappearanceBlockers (ApplicationDisappearanceView snapshot) =
  ownerEvidenceSnapshotBlockers snapshot

applicationDisappearanceAbsenceDigest ::
  ApplicationDisappearanceView ->
  Maybe DisappearanceEvidenceDigest
applicationDisappearanceAbsenceDigest
  (ApplicationDisappearanceView snapshot) =
    ownerEvidenceSnapshotAbsenceDigest snapshot

applicationDisappearanceFactCount :: ApplicationDisappearanceView -> Word64
applicationDisappearanceFactCount (ApplicationDisappearanceView snapshot) =
  ownerEvidenceSnapshotFactCount snapshot

regularRetirementHeldBlockerClass ::
  DisappearanceSubject ->
  ControlIndex ->
  Application.ApplicationFenceHeldWork ->
  Maybe DisappearanceBlockerClass
regularRetirementHeldBlockerClass subject resolveIndex work =
  case disappearanceSubjectView subject of
    ControlledPredefinedSubjectView {} -> heldBlockerClass subject work
    RegularSortDefinitionSubjectView {}
      | heldCarriesSubjectValue subject work -> Just HeldPublicationBlocker
      | heldMatchesSubjectWrite subject work
          || heldReferencesSubjectSort subject work ->
          if Application.applicationFenceHeldWorkControlPrerequisite work
            < resolveIndex
            then Just HeldPublicationBlocker
            else Nothing
      | otherwise -> Nothing

heldBlockerClass ::
  DisappearanceSubject ->
  Application.ApplicationFenceHeldWork ->
  Maybe DisappearanceBlockerClass
heldBlockerClass subject work =
  case disappearanceSubjectView subject of
    ControlledPredefinedSubjectView object _ _
      | heldControlledObject work == Just object -> Just HeldPublicationBlocker
      | otherwise -> Nothing
    RegularSortDefinitionSubjectView {}
      | heldCarriesSubjectValue subject work
          || heldMatchesSubjectWrite subject work
          || heldReferencesSubjectSort subject work ->
          Just HeldPublicationBlocker
      | otherwise -> Nothing

heldCarriesSubjectValue ::
  DisappearanceSubject ->
  Application.ApplicationFenceHeldWork ->
  Bool
heldCarriesSubjectValue subject work =
  case disappearanceSubjectView subject of
    ControlledPredefinedSubjectView {} -> False
    RegularSortDefinitionSubjectView expectedSort _ expectedOccurrence ->
      Application.applicationFenceHeldWorkSortId work == expectedSort
        && Application.applicationFenceHeldWorkSortOccurrenceId work
          == expectedOccurrence

heldMatchesSubjectWrite ::
  DisappearanceSubject ->
  Application.ApplicationFenceHeldWork ->
  Bool
heldMatchesSubjectWrite subject work =
  case disappearanceSubjectView subject of
    ControlledPredefinedSubjectView {} -> False
    RegularSortDefinitionSubjectView sortId descriptorDigest _ ->
      case Publication.admittedApplicationPublicationSortDefinition
        (Application.applicationFenceHeldWorkAdmittedValue work) of
        Just admitted ->
          let descriptor =
                SortDefinition.admittedApplicationSortDescriptor admitted
           in descriptorSortId descriptor == sortId
                && deriveCanonicalDescriptorDigest descriptor == descriptorDigest
        Nothing -> False

heldReferencesSubjectSort ::
  DisappearanceSubject ->
  Application.ApplicationFenceHeldWork ->
  Bool
heldReferencesSubjectSort subject work =
  canonicalPublicationReferencesSubjectSort
    subject
    (Application.applicationFenceHeldWorkSortId work)
    ( canonicalValueBytes
        ( Publication.admittedApplicationPublicationValue
            (Application.applicationFenceHeldWorkAdmittedValue work)
        )
    )

heldControlledObject ::
  Application.ApplicationFenceHeldWork ->
  Maybe GlobalObjectId
heldControlledObject work =
  case Application.applicationFenceHeldWorkOrigin work of
    Publication.RegularApplicationPublicationOrigin -> Nothing
    Publication.ControlledFirstUseApplicationPublicationOrigin identifier ->
      Just (globalObjectIdFromGlobalUniqueId identifier)
    Publication.ControlledUpdateApplicationPublicationOrigin object -> Just object
    Publication.ForwardApplicationPublicationOrigin object _ -> Just object

heldFactBytes :: Application.ApplicationFenceHeldView -> ByteString
heldFactBytes held =
  build
    ( Builder.byteString
        ( processEpochIdBytes
            (labelProcessAcceptancePositionProcess position)
        )
        <> Builder.word64BE
          (labelProcessAcceptancePositionOrdinal position)
        <> Builder.byteString
          (nablaIdBytes (Application.applicationFenceHeldWorkWriter work))
        <> Builder.byteString
          (sortIdBytes (Application.applicationFenceHeldWorkSortId work))
        <> Builder.byteString
          ( sortDefinitionOccurrenceIdBytes
              (Application.applicationFenceHeldWorkSortOccurrenceId work)
          )
        <> Builder.word64BE
          ( controlIndexWord64
              (Application.applicationFenceHeldWorkControlPrerequisite work)
          )
        <> frame
          ( canonicalValueByteString
              ( canonicalValueBytes
                  ( Publication.admittedApplicationPublicationValue
                      (Application.applicationFenceHeldWorkAdmittedValue work)
                  )
              )
          )
    )
  where
    position = Application.applicationFenceHeldPosition held
    work = Application.applicationFenceHeldSemanticWork held

applicationEvidenceDomain :: ByteString
applicationEvidenceDomain = "ECLIPS-DISAPPEARANCE-APPLICATION-HELD-WORK"

frame :: ByteString -> Builder.Builder
frame bytes =
  Builder.word64BE (fromIntegral (ByteString.length bytes))
    <> Builder.byteString bytes

build :: Builder.Builder -> ByteString
build = LazyByteString.toStrict . Builder.toLazyByteString
