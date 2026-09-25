{-# LANGUAGE OverloadedRecordDot #-}

-- | Read-only matching and pure retention for post-cut publication gates.
module Eclips.Herald.Disappearance.Gate
  ( checkedPublicationGates,
    peerPublicationGates,
    canonicalPublicationGates,
    peerPublicationGateProbes,
    disappearanceProbeIsCollecting,
    retainGatedPublicationInvalidations,
  )
where

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Set qualified as Set
import Eclips.Domain.Disappearance (DisappearanceProbeId, DisappearanceSubject, deriveDisappearanceEvidenceDigest)
import Eclips.Domain.Identity (HeraldEpoch, SortDefinitionOccurrenceId, SortId)
import Eclips.Domain.Publication (CheckedPublication)
import Eclips.Domain.Value (CanonicalValueBytes)
import Eclips.Herald.Disappearance.OwnerEvidence (canonicalPublicationIsSubjectRelevant, checkedPublicationIsSubjectRelevant, peerPublicationIsSubjectRelevant)
import Eclips.Herald.Disappearance.Protocol qualified as Protocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.PeerPublication (PeerPublication)
import Eclips.Herald.PeerStream (StreamSequence, streamDirectionSource)

checkedPublicationGates :: SortDefinitionOccurrenceId -> CheckedPublication -> Disappearance.State -> [Disappearance.MatchingWriteGate]
checkedPublicationGates occurrence publication = matchingGates (\subject -> checkedPublicationIsSubjectRelevant occurrence subject publication)

canonicalPublicationGates :: SortId -> SortDefinitionOccurrenceId -> CanonicalValueBytes -> Disappearance.State -> [Disappearance.MatchingWriteGate]
canonicalPublicationGates sortId occurrence value = matchingGates (\subject -> canonicalPublicationIsSubjectRelevant subject sortId occurrence value)

peerPublicationGates :: PeerPublication -> Disappearance.State -> [Disappearance.MatchingWriteGate]
peerPublicationGates publication = matchingGates (`peerPublicationIsSubjectRelevant` publication)

-- Only work after the exact source marker is post-cut. An early marker whose
-- Oracle Open is still missing holds its complete suffix until the subject is
-- known; the prefix before that marker remains ordinary cut evidence.
peerPublicationGateProbes :: HeraldEpoch -> StreamSequence -> PeerPublication -> Disappearance.State -> Set DisappearanceProbeId
peerPublicationGateProbes source sequenceNumber publication state =
  Set.fromList
    ( [ Protocol.disappearancePublicationMarkerProbe marker
      | marker <- Disappearance.pendingPublicationMarkers state,
        streamDirectionSource (Protocol.disappearancePublicationMarkerDirection marker) == source,
        Protocol.disappearancePublicationMarkerSequence marker < sequenceNumber,
        Disappearance.probeWitness (Protocol.disappearancePublicationMarkerProbe marker) state == Nothing
      ]
        <> [ Disappearance.matchingWriteGateProbe gate
           | gate <- peerPublicationGates publication state,
             Just witness <- [Disappearance.probeWitness (Disappearance.matchingWriteGateProbe gate) state],
             Just cell <- [lookup source (witness.witnessIncomingPublicationMarkers)],
             Protocol.disappearancePublicationMarkerSequence (cell.markerCellMarker) < sequenceNumber
           ]
    )

matchingGates :: (DisappearanceSubject -> Bool) -> Disappearance.State -> [Disappearance.MatchingWriteGate]
matchingGates matches state =
  [ witness.witnessMatchingWriteGate
  | witness <- Disappearance.probeWitnesses state,
    witness.witnessPhase == Disappearance.ProbeCollectingView,
    matches (Protocol.projectedProbeSubject (witness.witnessProjectedProbe))
  ]

disappearanceProbeIsCollecting :: DisappearanceProbeId -> Disappearance.State -> Bool
disappearanceProbeIsCollecting probe state = case Disappearance.probeWitness probe state of
  Just witness -> witness.witnessPhase == Disappearance.ProbeCollectingView
  Nothing -> False

retainGatedPublicationInvalidations ::
  [Disappearance.MatchingWriteGate] ->
  ByteString ->
  Disappearance.State ->
  Either Disappearance.DisappearanceProblem Disappearance.State
retainGatedPublicationInvalidations gates bytes state = foldM retain state gates
  where
    retain current gate = do
      prepared <-
        Disappearance.prepareGatedPublicationInvalidation
          gate
          (deriveDisappearanceEvidenceDigest bytes)
          current
      pure (fst (Disappearance.commitDisappearanceTransition prepared))
