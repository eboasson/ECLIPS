{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Subject-qualified negative evidence owned by 'Controlled'.
--
-- The opaque view is derived from retained local records. Controlled-object
-- disappearance does not consume this regular-only category; for that arm the
-- scan is empty and the resulting digest is simply left out by composition.
module Eclips.Herald.Controlled.Disappearance
  ( ControlledDisappearanceView,
    controlledDisappearanceView,
    controlledDisappearanceSubject,
    controlledDisappearanceBlockers,
    controlledDisappearanceAbsenceDigest,
    controlledDisappearanceFactCount,
  )
where

import Data.ByteString (ByteString)
import Data.Word (Word64)
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceDigest,
    DisappearanceSubject,
    DisappearanceSubjectView (..),
    disappearanceSubjectView,
  )
import Eclips.Domain.Identity (globalObjectIdBytes)
import Eclips.Herald.Controlled.State
  ( ControlledLifecycle (..),
    State,
    controlledLocalRecords,
    controlledRecordLifecycle,
    controlledRecordObjectId,
    controlledRecordOccurrenceId,
    controlledRecordSortId,
  )
import Eclips.Herald.Disappearance.OwnerEvidence
  ( OwnerEvidenceSnapshot,
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

newtype ControlledDisappearanceView
  = ControlledDisappearanceView OwnerEvidenceSnapshot
  deriving stock (Eq, Show)

controlledDisappearanceView ::
  DisappearanceSubject ->
  State ->
  ControlledDisappearanceView
controlledDisappearanceView subject owner =
  ControlledDisappearanceView
    (ownerEvidenceSnapshot controlledEvidenceDomain subject facts)
  where
    facts = case disappearanceSubjectView subject of
      ControlledPredefinedSubjectView {} -> []
      RegularSortDefinitionSubjectView sortId _ occurrence ->
        [ ownerEvidenceFact
            ( case controlledRecordLifecycle record of
                ControlledCurrent -> CurrentControlledUseBlocker
                ControlledObsolete -> ObsoleteControlledUseBlocker
            )
            (globalObjectIdBytes (controlledRecordObjectId record))
        | record <- controlledLocalRecords owner,
          controlledRecordSortId record == sortId,
          controlledRecordOccurrenceId record == occurrence
        ]

controlledDisappearanceSubject ::
  ControlledDisappearanceView -> DisappearanceSubject
controlledDisappearanceSubject (ControlledDisappearanceView snapshot) =
  ownerEvidenceSnapshotSubject snapshot

controlledDisappearanceBlockers ::
  ControlledDisappearanceView -> [DisappearanceBlockerWitness]
controlledDisappearanceBlockers (ControlledDisappearanceView snapshot) =
  ownerEvidenceSnapshotBlockers snapshot

controlledDisappearanceAbsenceDigest ::
  ControlledDisappearanceView -> Maybe DisappearanceEvidenceDigest
controlledDisappearanceAbsenceDigest (ControlledDisappearanceView snapshot) =
  ownerEvidenceSnapshotAbsenceDigest snapshot

controlledDisappearanceFactCount :: ControlledDisappearanceView -> Word64
controlledDisappearanceFactCount (ControlledDisappearanceView snapshot) =
  ownerEvidenceSnapshotFactCount snapshot

controlledEvidenceDomain :: ByteString
controlledEvidenceDomain = "ECLIPS-DISAPPEARANCE-CONTROLLED-USE"
