{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Occurrence-qualified negative evidence owned by Graph.
--
-- Dynamic vertex projections retain both the exact sort occurrence and the
-- checked root-activity classification. Immutable baseline vertices are
-- predefined topology and cannot refer to an application-defined occurrence.
module Eclips.Herald.Graph.Disappearance
  ( GraphDisappearanceView,
    graphDisappearanceView,
    graphDisappearanceSubject,
    graphDisappearanceBlockers,
    graphDisappearanceAbsenceDigest,
    graphDisappearanceFactCount,
  )
where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceDigest,
    DisappearanceSubject,
    DisappearanceSubjectView (..),
    disappearanceSubjectView,
  )
import Eclips.Domain.Identity
  ( SortDefinitionOccurrenceId,
    SortId,
    globalObjectIdBytes,
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
import Eclips.Herald.Graph.State
  ( State,
    graphStructuralVertexProjections,
  )
import Eclips.Herald.Structural.Debt
  ( sortOccurrenceDefinition,
    sortOccurrenceSortId,
  )
import Eclips.Herald.Structural.Reconciliation
  ( RootActivityProjection (..),
    StructuralVertexProjection (..),
  )

newtype GraphDisappearanceView
  = GraphDisappearanceView OwnerEvidenceSnapshot
  deriving stock (Eq, Show)

graphDisappearanceView ::
  DisappearanceSubject ->
  State ->
  GraphDisappearanceView
graphDisappearanceView subject owner =
  GraphDisappearanceView
    (ownerEvidenceSnapshot graphEvidenceDomain subject facts)
  where
    facts = case disappearanceSubjectView subject of
      ControlledPredefinedSubjectView {} -> []
      RegularSortDefinitionSubjectView sortId _ occurrence ->
        [ ownerEvidenceFact blockerClass (globalObjectIdBytes object)
        | (object, projection) <-
            Map.toAscList (graphStructuralVertexProjections owner),
          projectionCarries sortId occurrence projection,
          Just blockerClass <- [projectionBlockerClass projection]
        ]

graphDisappearanceSubject :: GraphDisappearanceView -> DisappearanceSubject
graphDisappearanceSubject (GraphDisappearanceView snapshot) =
  ownerEvidenceSnapshotSubject snapshot

graphDisappearanceBlockers ::
  GraphDisappearanceView -> [DisappearanceBlockerWitness]
graphDisappearanceBlockers (GraphDisappearanceView snapshot) =
  ownerEvidenceSnapshotBlockers snapshot

graphDisappearanceAbsenceDigest ::
  GraphDisappearanceView -> Maybe DisappearanceEvidenceDigest
graphDisappearanceAbsenceDigest (GraphDisappearanceView snapshot) =
  ownerEvidenceSnapshotAbsenceDigest snapshot

graphDisappearanceFactCount :: GraphDisappearanceView -> Word64
graphDisappearanceFactCount (GraphDisappearanceView snapshot) =
  ownerEvidenceSnapshotFactCount snapshot

projectionCarries ::
  SortId ->
  SortDefinitionOccurrenceId ->
  StructuralVertexProjection ->
  Bool
projectionCarries sortId occurrence projection = case projection of
  NeutralVertexProjection {} -> False
  NablaVertexProjection _ _ retained _ _ -> matches retained
  DeltaVertexProjection _ _ retained _ _ -> matches retained
  where
    matches retained =
      sortOccurrenceSortId retained == sortId
        && sortOccurrenceDefinition retained == occurrence

projectionBlockerClass ::
  StructuralVertexProjection -> Maybe DisappearanceBlockerClass
projectionBlockerClass projection = case projection of
  NeutralVertexProjection {} -> Nothing
  NablaVertexProjection _ _ _ _ activity ->
    Just $ case activity of
      PassiveRoot -> PassiveNablaBlocker
      ActiveRootHere {} -> ActiveNablaBlocker
      RemoteController {} -> ActiveNablaBlocker
  DeltaVertexProjection _ _ _ _ activity ->
    Just $ case activity of
      PassiveRoot -> PassiveDeltaBlocker
      ActiveRootHere {} -> ActiveDeltaBlocker
      RemoteController {} -> ActiveDeltaBlocker

graphEvidenceDomain :: ByteString
graphEvidenceDomain = "ECLIPS-DISAPPEARANCE-GRAPH-DEPENDENCY"
