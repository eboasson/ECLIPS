{-# LANGUAGE OverloadedStrings #-}

module Step16ProspectiveFixtures
  ( fixtureSystemId,
    fixtureMembers,
    fixtureH1,
    fixtureH2,
    fixtureH3,
    fixtureH4,
    fixtureMembership,
    fixtureControlledSubject,
    fixtureRegularSubject,
    fixtureRequestId,
    fixturePublicationPrefix,
    fixtureStoreRevision,
    fixtureAlignmentSubscription,
    fixtureEvidenceDigest,
    fixtureAttestation,
    fixtureAttestationEntries,
    fixtureEvidenceSnapshot,
    fixtureStreamDirection,
    fixtureBytes,
    checked,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word64, Word8)
import Eclips.Domain.Alignment
  ( HeraldPublicationPrefix (HeraldPublicationPrefixThrough),
    StoreRevision,
    mkHeraldPublicationPosition,
    storeRevisionFromWord64,
  )
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceDigest,
    DisappearanceSubject,
    DisappearanceSubjectView (..),
    controlledPredefinedDisappearanceSubject,
    deriveDisappearanceEvidenceDigest,
    deriveRegularSortOccurrenceClaim,
    disappearanceSubjectView,
    regularSortDefinitionDisappearanceSubject,
  )
import Eclips.Domain.Identity
  ( HeraldEpoch,
    SystemId,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkStructuralSequence,
    mkSystemId,
    structuralOccurrenceId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    genesisHeraldMembershipGeneration,
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (NeutralVertexRole),
    predefinedCatalogueDescriptor,
    profileEntryFor,
  )
import Eclips.Domain.SortOccurrence (SortOccurrenceBase (Genesis))
import Eclips.Herald.Alignment.Protocol
  ( AlignmentSubscriptionId,
    alignmentSubscriptionId,
    mkAlignmentSubscriptionSequence,
  )
import Eclips.Herald.Disappearance.Evidence.Internal
  ( DisappearanceEvidenceSnapshot,
    assembleDisappearanceEvidenceSnapshot,
  )
import Eclips.Herald.Disappearance.Protocol qualified as Protocol
import Eclips.Herald.PeerStream (StreamDirection, mkStreamDirection)
import Eclips.Oracle.Identity (OracleClientRequestId, oracleClientRequestId)

fixtureSystemId :: SystemId
fixtureSystemId = checked "fixture system ID" (mkSystemId (fixtureBytes 1))

fixtureH1, fixtureH2, fixtureH3, fixtureH4 :: HeraldEpoch
fixtureH1 = checked "fixture H1" (mkHeraldEpoch (fixtureBytes 11))
fixtureH2 = checked "fixture H2" (mkHeraldEpoch (fixtureBytes 12))
fixtureH3 = checked "fixture H3" (mkHeraldEpoch (fixtureBytes 13))
fixtureH4 = checked "fixture H4" (mkHeraldEpoch (fixtureBytes 14))

fixtureMembers :: [HeraldEpoch]
fixtureMembers = [fixtureH1, fixtureH2, fixtureH3]

fixtureMembership :: HeraldMembershipGeneration
fixtureMembership =
  checked
    "fixture membership"
    ( genesisHeraldMembershipGeneration
        fixtureSystemId
        (fixtureH1 :| [fixtureH2, fixtureH3])
    )

fixtureControlledSubject :: DisappearanceSubject
fixtureControlledSubject =
  checked
    "fixture controlled disappearance subject"
    ( controlledPredefinedDisappearanceSubject
        NeutralVertexRole
        (checked "fixture controlled object" (mkGlobalObjectId (fixtureBytes 21)))
        ( structuralOccurrenceId
            fixtureH1
            (checked "fixture structural sequence" (mkStructuralSequence 1))
        )
        Nothing
    )

fixtureRegularSubject :: DisappearanceSubject
fixtureRegularSubject =
  regularSortDefinitionDisappearanceSubject
    ( deriveRegularSortOccurrenceClaim
        fixtureSystemId
        (predefinedCatalogueDescriptor (profileEntryFor NeutralVertexRole))
        Genesis
    )

fixtureRequestId :: HeraldEpoch -> Word64 -> OracleClientRequestId
fixtureRequestId = oracleClientRequestId

fixturePublicationPrefix :: HeraldPublicationPrefix
fixturePublicationPrefix =
  HeraldPublicationPrefixThrough
    (checked "fixture Herald publication position" (mkHeraldPublicationPosition 7))

fixtureStoreRevision :: Word64 -> StoreRevision
fixtureStoreRevision = storeRevisionFromWord64

fixtureAlignmentSubscription :: HeraldEpoch -> Word64 -> AlignmentSubscriptionId
fixtureAlignmentSubscription destination sequenceNumber =
  alignmentSubscriptionId
    destination
    ( checked
        "fixture alignment subscription sequence"
        (mkAlignmentSubscriptionSequence sequenceNumber)
    )

fixtureEvidenceDigest :: Word8 -> DisappearanceEvidenceDigest
fixtureEvidenceDigest byte =
  deriveDisappearanceEvidenceDigest (fixtureBytes byte)

fixtureAttestation ::
  DisappearanceSubject -> Protocol.DisappearanceAbsenceAttestation
fixtureAttestation subject =
  checked
    "fixture disappearance absence attestation"
    ( Protocol.disappearanceAbsenceAttestation
        subject
        (fixtureAttestationEntries subject)
    )

fixtureAttestationEntries ::
  DisappearanceSubject ->
  [(Protocol.AbsenceAttestationClass, DisappearanceEvidenceDigest)]
fixtureAttestationEntries subject =
  zip required (fmap fixtureEvidenceDigest [140 ..])
  where
    common =
      [ Protocol.ApplicationStoreAbsence,
        Protocol.PublicationWorkAbsence,
        Protocol.PeerStreamWorkAbsence,
        Protocol.AlignmentWorkAbsence
      ]
    required = case disappearanceSubjectView subject of
      ControlledPredefinedSubjectView {} -> common
      RegularSortDefinitionSubjectView {} ->
        common
          <> [ Protocol.GraphDependencyAbsence,
               Protocol.ControlledUseAbsence
             ]

-- | Sole direct constructor use in the prospective property component.  Live
-- code must compose actual owner views through
-- 'Eclips.Herald.Disappearance.Evidence.disappearanceEvidenceSnapshot'.
fixtureEvidenceSnapshot ::
  DisappearanceSubject ->
  HeraldPublicationPrefix ->
  [Protocol.IncomingAlignmentCut] ->
  [Protocol.OutgoingAlignmentCut] ->
  [Protocol.DisappearanceBlockerWitness] ->
  [Protocol.MatchingPublicationObservation] ->
  DisappearanceEvidenceSnapshot
fixtureEvidenceSnapshot subject localCut incoming outgoing blockers matching =
  assembleDisappearanceEvidenceSnapshot
    subject
    localCut
    incoming
    outgoing
    blockers
    (if null blockers then Just (fixtureAttestation subject) else Nothing)
    matching

fixtureStreamDirection :: HeraldEpoch -> HeraldEpoch -> StreamDirection
fixtureStreamDirection source destination =
  checked
    "fixture stream direction"
    (mkStreamDirection source destination)

fixtureBytes :: Word8 -> ByteString
fixtureBytes byte = ByteString.replicate 32 byte

checked :: (Show problem) => String -> Either problem value -> value
checked context result = case result of
  Left problem -> error (context <> ": " <> show problem)
  Right value -> value
