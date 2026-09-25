{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read-only, subject-qualified disappearance evidence owned by PeerStream.
--
-- Only active outgoing assignments and incoming publication items which have
-- not completed semantic processing are blockers.  Completed receipts and
-- retained non-publication control items are history, not live work.  Direction
-- discovery uses narrow read ports and does not invoke whole-state validation.
module Eclips.Herald.PeerStream.Disappearance
  ( PeerStreamDisappearanceView,
    peerStreamDisappearanceView,
    peerStreamDisappearanceViewWithGates,
    peerStreamRegularRetirementDisappearanceViewWithGates,
    peerStreamRegularRetirementDisappearanceView,
    peerStreamDisappearanceSubject,
    peerStreamDisappearanceBlockers,
    peerStreamDisappearanceAbsenceDigest,
    peerStreamDisappearanceFactCount,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceDigest,
    DisappearanceSubject,
  )
import Eclips.Domain.Identity (ControlIndex, PublicationId, heraldEpochBytes)
import Eclips.Herald.Disappearance.OwnerEvidence
  ( OwnerEvidenceFact,
    OwnerEvidenceSnapshot,
    ownerEvidenceFact,
    ownerEvidenceSnapshot,
    ownerEvidenceSnapshotAbsenceDigest,
    ownerEvidenceSnapshotBlockers,
    ownerEvidenceSnapshotFactCount,
    ownerEvidenceSnapshotSubject,
    peerPublicationIsRegularRetirementResidual,
    peerPublicationIsSubjectRelevant,
  )
import Eclips.Herald.Disappearance.Protocol
  ( DisappearanceBlockerClass (..),
    DisappearanceBlockerWitness,
  )
import Eclips.Herald.PeerPayload (PeerLogicalPayload (..))
import Eclips.Herald.PeerPublication (PeerPublication, peerPublicationBatch, publicationBatchId)
import Eclips.Herald.PeerStream
  ( SequencedItem,
    peerItemDigestBytes,
    sequencedItemDigest,
    sequencedItemDirection,
    sequencedItemPayload,
    sequencedItemSequence,
    streamDirectionDestination,
    streamDirectionSource,
    streamSequenceWord64,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream

newtype PeerStreamDisappearanceView
  = PeerStreamDisappearanceView OwnerEvidenceSnapshot
  deriving stock (Eq, Show)

peerStreamDisappearanceView ::
  DisappearanceSubject ->
  PeerStream.State PeerLogicalPayload ->
  Either PeerStream.PeerStreamProblem PeerStreamDisappearanceView
peerStreamDisappearanceView subject =
  peerStreamDisappearanceViewWith
    subject
    (peerPublicationIsSubjectRelevant subject)
    Set.empty

-- | Retained peer work which still belongs to an already retired regular-sort
-- epoch.  Unlike the strict resolve-time view, equal descriptor writes and
-- structural sort references at or beyond the Resolve prerequisite are valid
-- successor work and are omitted.
peerStreamRegularRetirementDisappearanceView ::
  DisappearanceSubject ->
  ControlIndex ->
  PeerStream.State PeerLogicalPayload ->
  Either PeerStream.PeerStreamProblem PeerStreamDisappearanceView
peerStreamRegularRetirementDisappearanceView subject resolveIndex =
  peerStreamDisappearanceViewWith
    subject
    (peerPublicationIsRegularRetirementResidual subject resolveIndex)
    Set.empty

-- | Only the incoming payloads backed by exact Publication gate dependencies
-- are excluded. The PeerStream transcript and sequence remain fully retained.
peerStreamDisappearanceViewWithGates :: Set PublicationId -> DisappearanceSubject -> PeerStream.State PeerLogicalPayload -> Either PeerStream.PeerStreamProblem PeerStreamDisappearanceView
peerStreamDisappearanceViewWithGates gates subject =
  peerStreamDisappearanceViewWith subject (peerPublicationIsSubjectRelevant subject) gates

peerStreamRegularRetirementDisappearanceViewWithGates :: Set PublicationId -> DisappearanceSubject -> ControlIndex -> PeerStream.State PeerLogicalPayload -> Either PeerStream.PeerStreamProblem PeerStreamDisappearanceView
peerStreamRegularRetirementDisappearanceViewWithGates gates subject index =
  peerStreamDisappearanceViewWith subject (peerPublicationIsRegularRetirementResidual subject index) gates

peerStreamDisappearanceViewWith ::
  DisappearanceSubject ->
  (PeerPublication -> Bool) ->
  Set PublicationId ->
  PeerStream.State PeerLogicalPayload ->
  Either PeerStream.PeerStreamProblem PeerStreamDisappearanceView
peerStreamDisappearanceViewWith subject isRelevant gates state = do
  outgoingDirections <- PeerStream.peerStreamOutgoingDirections state
  incomingDirections <- PeerStream.peerStreamIncomingDirections state
  outgoingItems <-
    concat
      <$> traverse
        (`PeerStream.activeOutgoingItems` state)
        outgoingDirections
  incomingItems <-
    concat
      <$> traverse
        (`PeerStream.incomingRetainedItems` state)
        incomingDirections
  let facts =
        outgoingFacts isRelevant outgoingItems
          <> incomingFacts (\publication -> publicationBatchId (peerPublicationBatch publication) `Set.notMember` gates && isRelevant publication) incomingItems
  Right
    ( PeerStreamDisappearanceView
        (ownerEvidenceSnapshot peerStreamEvidenceDomain subject facts)
    )

peerStreamDisappearanceSubject ::
  PeerStreamDisappearanceView ->
  DisappearanceSubject
peerStreamDisappearanceSubject (PeerStreamDisappearanceView snapshot) =
  ownerEvidenceSnapshotSubject snapshot

peerStreamDisappearanceBlockers ::
  PeerStreamDisappearanceView ->
  [DisappearanceBlockerWitness]
peerStreamDisappearanceBlockers (PeerStreamDisappearanceView snapshot) =
  ownerEvidenceSnapshotBlockers snapshot

peerStreamDisappearanceAbsenceDigest ::
  PeerStreamDisappearanceView ->
  Maybe DisappearanceEvidenceDigest
peerStreamDisappearanceAbsenceDigest
  (PeerStreamDisappearanceView snapshot) =
    ownerEvidenceSnapshotAbsenceDigest snapshot

peerStreamDisappearanceFactCount :: PeerStreamDisappearanceView -> Word64
peerStreamDisappearanceFactCount (PeerStreamDisappearanceView snapshot) =
  ownerEvidenceSnapshotFactCount snapshot

outgoingFacts ::
  (PeerPublication -> Bool) ->
  [SequencedItem PeerLogicalPayload] ->
  [OwnerEvidenceFact]
outgoingFacts isRelevant items = do
  item <- items
  publication <- logicalPublication (sequencedItemPayload item)
  if isRelevant publication
    then
      pure
        ( ownerEvidenceFact
            PeerOutboxBlocker
            (streamItemFactBytes item)
        )
    else []

incomingFacts ::
  (PeerPublication -> Bool) ->
  [(SequencedItem PeerLogicalPayload, PeerStream.IncomingItemStatus)] ->
  [OwnerEvidenceFact]
incomingFacts isRelevant items = do
  (item, status) <- items
  if status == PeerStream.IncomingCompleted then [] else pure ()
  publication <- logicalPublication (sequencedItemPayload item)
  if isRelevant publication
    then
      pure
        ( ownerEvidenceFact
            PeerInboxBlocker
            (streamItemFactBytes item)
        )
    else []

logicalPublication :: PeerLogicalPayload -> [PeerPublication]
logicalPublication payload = case payload of
  PeerLogicalPublication publication -> [publication]
  PeerLogicalRouteCutover {} -> []
  PeerLogicalDisappearanceProbeMarker {} -> []

streamItemFactBytes :: SequencedItem payload -> ByteString
streamItemFactBytes item =
  build
    ( Builder.byteString
        (heraldEpochBytes (streamDirectionSource direction))
        <> Builder.byteString
          (heraldEpochBytes (streamDirectionDestination direction))
        <> Builder.word64BE (streamSequenceWord64 (sequencedItemSequence item))
        <> Builder.byteString
          (peerItemDigestBytes (sequencedItemDigest item))
    )
  where
    direction = sequencedItemDirection item

peerStreamEvidenceDomain :: ByteString
peerStreamEvidenceDomain = "ECLIPS-DISAPPEARANCE-PEER-STREAM-WORK"

build :: Builder.Builder -> ByteString
build = LazyByteString.toStrict . Builder.toLazyByteString
