{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Occurrence-qualified negative evidence owned by Placement.
--
-- The scan includes current cut-qualified advertisements and projections.
-- Superseded route history remains available to authenticate already-retained
-- frozen work, whose live owners account for it independently.
module Eclips.Herald.Placement.Disappearance
  ( PlacementDisappearanceView,
    placementDisappearanceView,
    placementDisappearanceSubject,
    placementDisappearanceBlockers,
    placementDisappearanceAbsenceDigest,
    placementDisappearanceFactCount,
  )
where

import Data.ByteString (ByteString)
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
  ( HeraldEpoch,
    deltaIdBytes,
    heraldEpochBytes,
    storeIncarnationIdBytes,
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
import Eclips.Herald.Placement
  ( DeltaRoute,
    PlacementSequence,
    deltaRouteDelta,
    deltaRouteOccurrenceId,
    deltaRouteSortId,
    deltaRouteStoreIncarnation,
    placementSequenceWord64,
  )
import Eclips.Herald.Placement.State
  ( LocalPlacement,
    State,
    SystemViewPlacement,
    currentPlacementRouteEntries,
    localPlacementDelta,
    localPlacementOccurrenceId,
    localPlacementSortId,
    localPlacementStoreIncarnation,
    localPlacements,
    systemViewPlacementDelta,
    systemViewPlacementOccurrenceId,
    systemViewPlacementSortId,
    systemViewPlacementStoreIncarnation,
    systemViewPlacements,
  )

newtype PlacementDisappearanceView
  = PlacementDisappearanceView OwnerEvidenceSnapshot
  deriving stock (Eq, Show)

placementDisappearanceView ::
  DisappearanceSubject ->
  State ->
  PlacementDisappearanceView
placementDisappearanceView subject owner =
  PlacementDisappearanceView
    (ownerEvidenceSnapshot placementEvidenceDomain subject facts)
  where
    facts = case disappearanceSubjectView subject of
      ControlledPredefinedSubjectView {} -> []
      RegularSortDefinitionSubjectView sortId _ occurrence ->
        [ ownerEvidenceFact
            PlacementDependencyBlocker
            (localPlacementFact placement)
        | placement <- localPlacements owner,
          localPlacementSortId placement == sortId,
          localPlacementOccurrenceId placement == occurrence
        ]
          <> [ ownerEvidenceFact
                 PlacementDependencyBlocker
                 (systemPlacementFact placement)
             | placement <- systemViewPlacements owner,
               systemViewPlacementSortId placement == sortId,
               systemViewPlacementOccurrenceId placement == occurrence
             ]
          <> [ ownerEvidenceFact
                 RouteDependencyBlocker
                 (routeFact ownerEpoch revision route)
             | (ownerEpoch, revision, route) <- currentPlacementRouteEntries owner,
               deltaRouteSortId route == sortId,
               deltaRouteOccurrenceId route == occurrence
             ]

placementDisappearanceSubject ::
  PlacementDisappearanceView -> DisappearanceSubject
placementDisappearanceSubject (PlacementDisappearanceView snapshot) =
  ownerEvidenceSnapshotSubject snapshot

placementDisappearanceBlockers ::
  PlacementDisappearanceView -> [DisappearanceBlockerWitness]
placementDisappearanceBlockers (PlacementDisappearanceView snapshot) =
  ownerEvidenceSnapshotBlockers snapshot

placementDisappearanceAbsenceDigest ::
  PlacementDisappearanceView -> Maybe DisappearanceEvidenceDigest
placementDisappearanceAbsenceDigest (PlacementDisappearanceView snapshot) =
  ownerEvidenceSnapshotAbsenceDigest snapshot

placementDisappearanceFactCount :: PlacementDisappearanceView -> Word64
placementDisappearanceFactCount (PlacementDisappearanceView snapshot) =
  ownerEvidenceSnapshotFactCount snapshot

localPlacementFact :: LocalPlacement -> ByteString
localPlacementFact placement =
  build
    ( Builder.word8 1
        <> Builder.byteString (deltaIdBytes (localPlacementDelta placement))
        <> Builder.byteString
          (storeIncarnationIdBytes (localPlacementStoreIncarnation placement))
    )

systemPlacementFact :: SystemViewPlacement -> ByteString
systemPlacementFact placement =
  build
    ( Builder.word8 2
        <> Builder.byteString (deltaIdBytes (systemViewPlacementDelta placement))
        <> Builder.byteString
          (storeIncarnationIdBytes (systemViewPlacementStoreIncarnation placement))
    )

routeFact :: HeraldEpoch -> PlacementSequence -> DeltaRoute -> ByteString
routeFact owner revision route =
  build
    ( Builder.byteString (heraldEpochBytes owner)
        <> Builder.word64BE (placementSequenceWord64 revision)
        <> Builder.byteString (deltaIdBytes (deltaRouteDelta route))
        <> Builder.byteString
          (storeIncarnationIdBytes (deltaRouteStoreIncarnation route))
    )

build :: Builder.Builder -> ByteString
build = LazyByteString.toStrict . Builder.toLazyByteString

placementEvidenceDomain :: ByteString
placementEvidenceDomain = "ECLIPS-DISAPPEARANCE-PLACEMENT-DEPENDENCY"
