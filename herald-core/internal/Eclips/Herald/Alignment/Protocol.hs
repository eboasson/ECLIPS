{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Intrinsically checked context-alignment messages and private owner records.
--
-- This module deliberately contains no progress state.  It fixes immutable
-- transcripts at the alignment boundary; membership, retained-view, binding,
-- sequencing, and lifecycle admission remain responsibilities of the alignment
-- owner.
module Eclips.Herald.Alignment.Protocol
  ( AlignmentSequenceProblem (..),
    AlignmentObligationSequence,
    mkAlignmentObligationSequence,
    firstAlignmentObligationSequence,
    nextAlignmentObligationSequence,
    alignmentObligationSequenceWord64,
    AlignmentSubscriptionSequence,
    mkAlignmentSubscriptionSequence,
    firstAlignmentSubscriptionSequence,
    nextAlignmentSubscriptionSequence,
    alignmentSubscriptionSequenceWord64,
    AlignmentEvidenceSequence,
    mkAlignmentEvidenceSequence,
    firstAlignmentEvidenceSequence,
    nextAlignmentEvidenceSequence,
    alignmentEvidenceSequenceWord64,
    AlignmentObligationId,
    alignmentObligationId,
    alignmentObligationIdDestinationHerald,
    alignmentObligationIdSequence,
    AlignmentSubscriptionId,
    alignmentSubscriptionId,
    alignmentSubscriptionIdDestinationHerald,
    alignmentSubscriptionIdSequence,
    AlignmentProtocolProblem (..),
    AlignmentPlanId,
    AlignmentBindingDisposition (..),
    AlignmentPredecessorStatus (..),
    AlignmentPlanBindingClaim,
    alignmentPlanBindingClaim,
    alignmentPlanBindingClaimMembers,
    alignmentPlanBindingClaimDisposition,
    alignmentPlanBindingClaimGeneration,
    AlignmentPlanRelationClaim,
    alignmentPlanRelationClaim,
    alignmentPlanRelationClaimSource,
    alignmentPlanRelationClaimDestination,
    alignmentPlanRelationClaimStrength,
    AlignmentPlanAnnounce,
    alignmentPlanAnnounce,
    alignmentPlanAnnounceId,
    alignmentPlanAnnounceTopology,
    alignmentPlanAnnouncePredecessor,
    alignmentPlanAnnouncePredecessorStatus,
    alignmentPlanAnnounceBindings,
    alignmentPlanAnnounceRelations,
    alignmentPlanAnnounceCreatedCuts,
    AlignmentPlanAccepted,
    alignmentPlanAccepted,
    alignmentPlanAcceptedId,
    alignmentPlanAcceptedHerald,
    alignmentPlanAcceptedPublicationPrefix,
    AlignmentLostStore,
    alignmentLostStore,
    alignmentLostStoreHome,
    alignmentLostStoreDelta,
    alignmentLostStoreIncarnation,
    alignmentLostStorePlacementRevision,
    AlignmentPlanObsolete,
    alignmentPlanObsolete,
    alignmentPlanObsoleteId,
    alignmentPlanObsoleteReporter,
    alignmentPlanObsoleteLostStores,
    alignmentPlanObsoleteCanonicalBytes,
    AlignmentCutAnnounce,
    alignmentCutAnnounce,
    checkAlignmentCutAnnounce,
    alignmentCutAnnounceGeneration,
    alignmentCutAnnounceCut,
    AlignmentCutAccepted,
    alignmentCutAccepted,
    alignmentCutAcceptedGeneration,
    alignmentCutAcceptedHerald,
    alignmentCutAcceptedTopologyCut,
    alignmentCutAcceptedPlacementVector,
    alignmentCutAcceptedPublicationPrefix,
    ClassMemberReady,
    classMemberReady,
    classMemberReadyEvidenceSequence,
    classMemberReadyGeneration,
    classMemberReadyStoreIncarnation,
    classMemberReadyPredecessorAndBasePrefixDigest,
    classMemberReadyStoreRevision,
    classMemberReadyCanonicalBytes,
    classMemberReadySetDigest,
    HistoricalCertificate,
    historicalCertificate,
    historicalCertificateClassGeneration,
    historicalCertificateSourceStoreIncarnation,
    historicalCertificateExactMemberReadyDigest,
    historicalCertificateCompletedPredecessorPrefixDigest,
    historicalCertificateCertifiedStoreRevision,
    historicalCertificateCanonicalBytes,
    historicalCertificateDigest,
    DestinationStore,
    destinationStore,
    destinationStoreDelta,
    destinationStoreIncarnation,
    AlignmentObligation,
    alignmentObligation,
    alignmentObligationIdValue,
    alignmentObligationCause,
    alignmentObligationSortId,
    alignmentObligationSortDefinitionOccurrenceId,
    alignmentObligationDestinationGeneration,
    alignmentObligationDestinationStores,
    alignmentObligationSourceGeneration,
    alignmentObligationFrozenContextPathStrength,
    AlignmentAttempt,
    alignmentAttempt,
    alignmentAttemptObligation,
    alignmentAttemptObligationId,
    alignmentAttemptSubscriptionId,
    alignmentAttemptSourceHerald,
    alignmentAttemptSourceStoreIncarnation,
    alignmentAttemptHistoricalCertificate,
    alignmentAttemptHistoricalCertificateDigest,
    alignmentAttemptDestinationStores,
    alignmentAttemptFrozenContextPathStrength,
    AlignmentSourceProof (..),
    AlignmentSubscribe,
    alignmentSubscribe,
    alignmentSubscribeObligation,
    alignmentSubscribeObligationId,
    alignmentSubscribeSubscriptionId,
    alignmentSubscribeSourceStoreIncarnation,
    alignmentSubscribeSourceProof,
    alignmentSubscribeDestinationStores,
    RetainedPublicationEvidence,
    RetainedObservationOrigin (..),
    primordialRetainedPublicationEvidence,
    retainedPublicationEvidence,
    retainedPublicationEvidencePublicationId,
    retainedPublicationEvidenceSourceProcess,
    retainedPublicationEvidenceSortId,
    retainedPublicationEvidenceSortDefinitionOccurrenceId,
    retainedPublicationEvidenceCanonicalValue,
    retainedPublicationEvidenceSourceStrength,
    retainedPublicationEvidenceOrigin,
    retainedPublicationEvidenceSourceTopologyPrerequisite,
    retainedPublicationEvidenceControlPrerequisite,
    retainedPublicationEvidenceStructuralStamp,
    retainedPublicationEvidenceCanonicalBytes,
    RetainedStateEvidence,
    retainedStateEvidence,
    retainedStateEvidenceRepresentative,
    retainedStateEvidenceStrengthWitness,
    retainedStateEvidenceJoinedStrength,
    retainedStateEvidenceCanonicalBytes,
    alignmentSemanticSnapshotCanonicalBytes,
    alignmentSemanticSnapshotDigest,
    AlignmentSnapshotStart,
    alignmentSnapshotStart,
    alignmentSnapshotStartSubscriptionId,
    alignmentSnapshotStartBaseRevision,
    alignmentSnapshotStartSemanticDigest,
    alignmentSnapshotStartChunkCount,
    AlignmentSnapshotChunk,
    alignmentSnapshotChunk,
    alignmentSnapshotChunkSubscriptionId,
    alignmentSnapshotChunkNumber,
    alignmentSnapshotChunkRetainedStates,
    AlignmentSnapshotEnd,
    alignmentSnapshotEnd,
    alignmentSnapshotEndSubscriptionId,
    alignmentSnapshotEndBaseRevision,
    alignmentSnapshotEndSemanticDigest,
    AlignmentChange,
    alignmentChange,
    alignmentChangeSubscriptionId,
    alignmentChangeStoreRevision,
    alignmentChangeRetainedTransition,
    AlignmentLive,
    alignmentLive,
    alignmentLiveSubscriptionId,
    alignmentLiveThroughSourceStoreRevision,
    AlignmentAck,
    alignmentAck,
    alignmentAckSubscriptionId,
    alignmentAckAppliedSourceStoreRevision,
    AlignmentCancelReason (..),
    AlignmentCancel,
    alignmentCancel,
    alignmentCancelSubscriptionId,
    alignmentCancelReason,
    AlignmentRouteCutoverMarker,
    alignmentRouteCutoverMarker,
    alignmentRouteCutoverMarkerPlan,
    alignmentRouteCutoverMarkerGeneration,
    alignmentRouteCutoverMarkerSourceHerald,
    alignmentRouteCutoverMarkerPredecessorHerald,
    alignmentRouteCutoverMarkerCanonicalBytes,
    alignmentRouteCutoverMarkerEvidenceDigest,
    alignmentRouteCutoverSetEvidenceDigest,
    AlignmentControl (..),
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Serialize.Put qualified as Serialize
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Alignment
  ( AlignmentCut,
    AlignmentShapeError,
    AlignmentSnapshotDigest,
    BootstrapEvidenceDigest,
    ContextClassGenerationId,
    HeraldPublicationPrefix,
    HistoricalCertificateDigest,
    MemberReadyEvidenceDigest,
    PhysicalPlacementRevisionVector,
    PlacementRevision,
    RouteCutoverEvidenceDigest,
    StoreRevision,
    alignmentCutAttempt,
    alignmentCutExactMembers,
    alignmentCutFreshMemberBaseEvidence,
    alignmentCutPhysicalPlacementRevisionVector,
    alignmentCutPredecessorGenerationIds,
    alignmentCutSortDefinitionOccurrenceId,
    alignmentCutSortId,
    alignmentCutTopologyCut,
    alignmentMemberDelta,
    bootstrapEvidenceDigestBytes,
    contextClassGenerationIdBytes,
    deriveAlignmentSnapshotDigest,
    deriveContextClassGenerationId,
    deriveHistoricalCertificateDigest,
    deriveMemberReadyEvidenceDigest,
    deriveRouteCutoverEvidenceDigest,
    freshMemberBaseDelta,
    initialAlignmentPlanAttempt,
    memberReadyEvidenceDigestBytes,
    physicalPlacementRevisionEntries,
    placementRevisionWord64,
    storeRevisionWord64,
  )
import Eclips.Domain.Disappearance (DisappearanceProbeId)
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    HeraldEpoch,
    ProcessEpochId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    StoreIncarnationId,
    TopologyCutId,
    authorityEpochCanonicalBytes,
    controlIndexWord64,
    deltaIdBytes,
    heraldEpochBytes,
    nablaIdBytes,
    nablaSequenceWord64,
    processEpochIdBytes,
    publicationAuthorityEpoch,
    publicationNabla,
    publicationNablaSequence,
    publicationSourceHeraldEpoch,
    sortDefinitionOccurrenceIdBytes,
    sortIdBytes,
    storeIncarnationIdBytes,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
    structuralSequenceWord64,
    topologyCutIdBytes,
  )
import Eclips.Domain.Route (ReplicaStrength (..))
import Eclips.Domain.Sort.Descriptor (structuralCarrierRoleTag)
import Eclips.Domain.Structural
  ( StructuralPrefix,
    structuralPrefixSequence,
    structuralVersionVectorEntries,
  )
import Eclips.Domain.StructuralConsequence (StructuralConsequenceCause)
import Eclips.Domain.Topology (TopologyCut, deriveTopologyCutId)
import Eclips.Domain.Value
  ( CanonicalValueBytes,
    canonicalValueByteString,
  )
import Eclips.Herald.Alignment.Plan.Identity
import Eclips.Herald.PeerPublication
  ( StructuralOccurrenceStamp,
    structuralOccurrenceStampCarrierRole,
    structuralOccurrenceStampOccurrence,
    structuralOccurrenceStampPredecessor,
    structuralOccurrenceStampPublication,
    structuralOccurrenceStampPublicationDigest,
    structuralPublicationDigestBytes,
  )
import Eclips.Herald.Structural.Debt (sortOccurrence, sortOccurrenceDefinition, sortOccurrenceSortId)

newtype AlignmentObligationSequence = AlignmentObligationSequence Word64
  deriving stock (Eq, Ord, Show)

newtype AlignmentSubscriptionSequence = AlignmentSubscriptionSequence Word64
  deriving stock (Eq, Ord, Show)

newtype AlignmentEvidenceSequence = AlignmentEvidenceSequence Word64
  deriving stock (Eq, Ord, Show)

data AlignmentSequenceProblem
  = AlignmentObligationSequenceMustBePositive
  | AlignmentSubscriptionSequenceMustBePositive
  | AlignmentEvidenceSequenceMustBePositive
  deriving stock (Eq, Show)

mkAlignmentObligationSequence ::
  Word64 -> Either AlignmentSequenceProblem AlignmentObligationSequence
mkAlignmentObligationSequence 0 = Left AlignmentObligationSequenceMustBePositive
mkAlignmentObligationSequence value = Right (AlignmentObligationSequence value)

firstAlignmentObligationSequence :: AlignmentObligationSequence
firstAlignmentObligationSequence = AlignmentObligationSequence 1

nextAlignmentObligationSequence ::
  AlignmentObligationSequence -> AlignmentObligationSequence
nextAlignmentObligationSequence (AlignmentObligationSequence value) =
  AlignmentObligationSequence (value + 1)

alignmentObligationSequenceWord64 :: AlignmentObligationSequence -> Word64
alignmentObligationSequenceWord64 (AlignmentObligationSequence value) = value

mkAlignmentSubscriptionSequence ::
  Word64 -> Either AlignmentSequenceProblem AlignmentSubscriptionSequence
mkAlignmentSubscriptionSequence 0 = Left AlignmentSubscriptionSequenceMustBePositive
mkAlignmentSubscriptionSequence value = Right (AlignmentSubscriptionSequence value)

firstAlignmentSubscriptionSequence :: AlignmentSubscriptionSequence
firstAlignmentSubscriptionSequence = AlignmentSubscriptionSequence 1

nextAlignmentSubscriptionSequence ::
  AlignmentSubscriptionSequence -> AlignmentSubscriptionSequence
nextAlignmentSubscriptionSequence (AlignmentSubscriptionSequence value) =
  AlignmentSubscriptionSequence (value + 1)

alignmentSubscriptionSequenceWord64 :: AlignmentSubscriptionSequence -> Word64
alignmentSubscriptionSequenceWord64 (AlignmentSubscriptionSequence value) = value

mkAlignmentEvidenceSequence ::
  Word64 -> Either AlignmentSequenceProblem AlignmentEvidenceSequence
mkAlignmentEvidenceSequence 0 = Left AlignmentEvidenceSequenceMustBePositive
mkAlignmentEvidenceSequence value = Right (AlignmentEvidenceSequence value)

firstAlignmentEvidenceSequence :: AlignmentEvidenceSequence
firstAlignmentEvidenceSequence = AlignmentEvidenceSequence 1

nextAlignmentEvidenceSequence ::
  AlignmentEvidenceSequence -> AlignmentEvidenceSequence
nextAlignmentEvidenceSequence (AlignmentEvidenceSequence value) =
  AlignmentEvidenceSequence (value + 1)

alignmentEvidenceSequenceWord64 :: AlignmentEvidenceSequence -> Word64
alignmentEvidenceSequenceWord64 (AlignmentEvidenceSequence value) = value

data AlignmentObligationId
  = AlignmentObligationId HeraldEpoch AlignmentObligationSequence
  deriving stock (Eq, Ord, Show)

alignmentObligationId ::
  HeraldEpoch -> AlignmentObligationSequence -> AlignmentObligationId
alignmentObligationId = AlignmentObligationId

alignmentObligationIdDestinationHerald :: AlignmentObligationId -> HeraldEpoch
alignmentObligationIdDestinationHerald (AlignmentObligationId herald _) = herald

alignmentObligationIdSequence ::
  AlignmentObligationId -> AlignmentObligationSequence
alignmentObligationIdSequence (AlignmentObligationId _ sequenceNumber) = sequenceNumber

data AlignmentSubscriptionId
  = AlignmentSubscriptionId HeraldEpoch AlignmentSubscriptionSequence
  deriving stock (Eq, Ord, Show)

alignmentSubscriptionId ::
  HeraldEpoch -> AlignmentSubscriptionSequence -> AlignmentSubscriptionId
alignmentSubscriptionId = AlignmentSubscriptionId

alignmentSubscriptionIdDestinationHerald :: AlignmentSubscriptionId -> HeraldEpoch
alignmentSubscriptionIdDestinationHerald (AlignmentSubscriptionId herald _) = herald

alignmentSubscriptionIdSequence ::
  AlignmentSubscriptionId -> AlignmentSubscriptionSequence
alignmentSubscriptionIdSequence (AlignmentSubscriptionId _ sequenceNumber) = sequenceNumber

data AlignmentProtocolProblem
  = AlignmentCutGenerationMismatch
      ContextClassGenerationId
      ContextClassGenerationId
  | AlignmentDestinationDeltaConflict
      DeltaId
      DestinationStore
      DestinationStore
  | AlignmentDestinationStoreConflict
      StoreIncarnationId
      DestinationStore
      DestinationStore
  | AlignmentAttemptSourceGenerationMismatch
      ContextClassGenerationId
      ContextClassGenerationId
  | AlignmentAttemptCertificateStoreMismatch
      StoreIncarnationId
      StoreIncarnationId
  | AlignmentAttemptSubscriptionDestinationMismatch HeraldEpoch HeraldEpoch
  | AlignmentSubscribeDestinationMismatch HeraldEpoch HeraldEpoch
  | AlignmentSubscribeSourceProofGenerationMismatch
      ContextClassGenerationId
      ContextClassGenerationId
  | RetainedEvidenceStampPublicationMismatch PublicationId PublicationId
  | RetainedEvidenceStampSourceMismatch HeraldEpoch HeraldEpoch
  | RetainedStateEvidenceSortMismatch SortId SortId
  | RetainedStateEvidenceOccurrenceMismatch
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | RetainedStateEvidenceLogicalValueMismatch
  | RetainedStateEvidenceStrengthWitnessMismatch ReplicaStrength ReplicaStrength
  | AlignmentPlanAnnouncementShapeProblem AlignmentShapeError
  | AlignmentPlanAnnouncementCoordinateMismatch
  | AlignmentPlanAnnouncementPredecessorMismatch
  | AlignmentPlanAnnouncementDuplicateMembers
  | AlignmentPlanAnnouncementDuplicateGeneration ContextClassGenerationId
  | AlignmentPlanAnnouncementRelationInvalid
  | AlignmentPlanAnnouncementCreatedCutsMismatch
  | AlignmentPlanAcceptanceHeraldOutsidePlacement HeraldEpoch
  | AlignmentPlanObsoleteHeraldOutsidePlacement HeraldEpoch
  | AlignmentPlanAnnouncementResetShapeMismatch
  | AlignmentRouteCutoverEndpointsEqual HeraldEpoch
  deriving stock (Eq, Show)

-- | A complete-table claim has shape-checked identities but is admitted only
-- after the owner reconstructs the authoritative context and carry decisions.
data AlignmentPlanBindingClaim = AlignmentPlanBindingClaim !(NonEmpty DeltaId) !AlignmentBindingDisposition !ContextClassGenerationId
  deriving stock (Eq, Ord, Show)

alignmentPlanBindingClaim :: NonEmpty DeltaId -> AlignmentBindingDisposition -> ContextClassGenerationId -> Either AlignmentProtocolProblem AlignmentPlanBindingClaim
alignmentPlanBindingClaim members disposition generation
  | length members /= Set.size unique = Left AlignmentPlanAnnouncementDuplicateMembers
  | otherwise = case Set.toAscList unique of
      first : remaining -> Right (AlignmentPlanBindingClaim (first :| remaining) disposition generation)
      [] -> error "nonempty alignment plan class normalized to empty"
  where
    unique = Set.fromList (NonEmpty.toList members)

alignmentPlanBindingClaimMembers :: AlignmentPlanBindingClaim -> NonEmpty DeltaId
alignmentPlanBindingClaimMembers (AlignmentPlanBindingClaim members _ _) = members

alignmentPlanBindingClaimDisposition :: AlignmentPlanBindingClaim -> AlignmentBindingDisposition
alignmentPlanBindingClaimDisposition (AlignmentPlanBindingClaim _ disposition _) = disposition

alignmentPlanBindingClaimGeneration :: AlignmentPlanBindingClaim -> ContextClassGenerationId
alignmentPlanBindingClaimGeneration (AlignmentPlanBindingClaim _ _ generation) = generation

data AlignmentPlanRelationClaim = AlignmentPlanRelationClaim !ContextClassGenerationId !ContextClassGenerationId !ReplicaStrength
  deriving stock (Eq, Ord, Show)

alignmentPlanRelationClaim :: ContextClassGenerationId -> ContextClassGenerationId -> ReplicaStrength -> AlignmentPlanRelationClaim
alignmentPlanRelationClaim = AlignmentPlanRelationClaim

alignmentPlanRelationClaimSource :: AlignmentPlanRelationClaim -> ContextClassGenerationId
alignmentPlanRelationClaimSource (AlignmentPlanRelationClaim source _ _) = source

alignmentPlanRelationClaimDestination :: AlignmentPlanRelationClaim -> ContextClassGenerationId
alignmentPlanRelationClaimDestination (AlignmentPlanRelationClaim _ destination _) = destination

alignmentPlanRelationClaimStrength :: AlignmentPlanRelationClaim -> ReplicaStrength
alignmentPlanRelationClaimStrength (AlignmentPlanRelationClaim _ _ strength) = strength

data AlignmentPlanAnnounce = AlignmentPlanAnnounce !AlignmentPlanId !TopologyCut !(Maybe AlignmentPlanId) !AlignmentPredecessorStatus ![AlignmentPlanBindingClaim] ![AlignmentPlanRelationClaim] ![AlignmentCutAnnounce]
  deriving stock (Eq, Show)

alignmentPlanAnnounce :: AlignmentPlanId -> TopologyCut -> Maybe AlignmentPlanId -> AlignmentPredecessorStatus -> [AlignmentPlanBindingClaim] -> [AlignmentPlanRelationClaim] -> [AlignmentCutAnnounce] -> Either AlignmentProtocolProblem AlignmentPlanAnnounce
alignmentPlanAnnounce identifier topology predecessor status suppliedBindings suppliedRelations suppliedCuts = do
  checked <- either (Left . AlignmentPlanAnnouncementShapeProblem) Right (alignmentPlanIdAtTopologyAtAttempt (alignmentPlanIdAttempt identifier) (alignmentPlanIdSort identifier) topology (alignmentPlanIdPlacement identifier))
  if checked == identifier then Right () else Left AlignmentPlanAnnouncementCoordinateMismatch
  case predecessor of
    Just prior | prior == identifier || alignmentPlanIdSort prior /= alignmentPlanIdSort identifier -> Left AlignmentPlanAnnouncementPredecessorMismatch
    _ -> Right ()
  let allMembers = concatMap (NonEmpty.toList . alignmentPlanBindingClaimMembers) suppliedBindings
      bindings = sortOn alignmentPlanBindingClaimMembers suppliedBindings
      generations = map alignmentPlanBindingClaimGeneration bindings
      generationSet = Set.fromList generations
      repeated = [generation | (generation, count) <- Map.toAscList (Map.fromListWith (+) [(generation, 1 :: Int) | generation <- generations]), count > 1]
      relations = Set.toAscList (Set.fromList suppliedRelations)
      relationPairs = [(alignmentPlanRelationClaimSource relation, alignmentPlanRelationClaimDestination relation) | relation <- suppliedRelations]
      cuts = sortOn alignmentCutAnnounceGeneration suppliedCuts
      created = Set.fromList [alignmentPlanBindingClaimGeneration binding | binding <- bindings, alignmentPlanBindingClaimDisposition binding == AlignmentCreated]
      cutIds = map alignmentCutAnnounceGeneration cuts
  if status == AlignmentPredecessorReset
    && ( predecessor /= Nothing
           || alignmentPlanIdAttempt identifier == initialAlignmentPlanAttempt
           || any ((/= AlignmentCreated) . alignmentPlanBindingClaimDisposition) bindings
           || any (not . resetCut . alignmentCutAnnounceCut) cuts
       )
    then Left AlignmentPlanAnnouncementResetShapeMismatch
    else Right ()
  if any ((== AlignmentCarried) . alignmentPlanBindingClaimDisposition) bindings && (predecessor == Nothing || status == AlignmentPredecessorInvalidated)
    then Left AlignmentPlanAnnouncementPredecessorMismatch
    else Right ()
  if length allMembers == Set.size (Set.fromList allMembers) then Right () else Left AlignmentPlanAnnouncementDuplicateMembers
  case repeated of
    generation : _ -> Left (AlignmentPlanAnnouncementDuplicateGeneration generation)
    [] -> Right ()
  if length relationPairs == Set.size (Set.fromList relationPairs) && all (\(source, destination) -> source /= destination && Set.member source generationSet && Set.member destination generationSet) relationPairs
    then Right ()
    else Left AlignmentPlanAnnouncementRelationInvalid
  if length cutIds == Set.size created && Set.fromList cutIds == created && all (exactCut bindings) cuts
    then Right ()
    else Left AlignmentPlanAnnouncementCreatedCutsMismatch
  Right (AlignmentPlanAnnounce identifier topology predecessor status bindings relations cuts)
  where
    resetCut cut =
      null (alignmentCutPredecessorGenerationIds cut)
        && map freshMemberBaseDelta (alignmentCutFreshMemberBaseEvidence cut) == NonEmpty.toList (fmap alignmentMemberDelta (alignmentCutExactMembers cut))
    exactCut bindings announce =
      let cut = alignmentCutAnnounceCut announce
          matching = [binding | binding <- bindings, alignmentPlanBindingClaimGeneration binding == alignmentCutAnnounceGeneration announce]
       in alignmentCutAttempt cut == alignmentPlanIdAttempt identifier
            && alignmentCutSortId cut == sortOccurrenceSortId (alignmentPlanIdSort identifier)
            && alignmentCutSortDefinitionOccurrenceId cut == sortOccurrenceDefinition (alignmentPlanIdSort identifier)
            && alignmentCutTopologyCut cut == topology
            && alignmentCutPhysicalPlacementRevisionVector cut == alignmentPlanIdPlacement identifier
            && case matching of
              [binding] -> fmap alignmentMemberDelta (alignmentCutExactMembers cut) == alignmentPlanBindingClaimMembers binding
              _ -> False

alignmentPlanAnnounceId :: AlignmentPlanAnnounce -> AlignmentPlanId
alignmentPlanAnnounceId (AlignmentPlanAnnounce identifier _ _ _ _ _ _) = identifier
alignmentPlanAnnounceTopology :: AlignmentPlanAnnounce -> TopologyCut
alignmentPlanAnnounceTopology (AlignmentPlanAnnounce _ topology _ _ _ _ _) = topology
alignmentPlanAnnouncePredecessor :: AlignmentPlanAnnounce -> Maybe AlignmentPlanId
alignmentPlanAnnouncePredecessor (AlignmentPlanAnnounce _ _ predecessor _ _ _ _) = predecessor
alignmentPlanAnnouncePredecessorStatus :: AlignmentPlanAnnounce -> AlignmentPredecessorStatus
alignmentPlanAnnouncePredecessorStatus (AlignmentPlanAnnounce _ _ _ status _ _ _) = status

alignmentPlanAnnounceBindings :: AlignmentPlanAnnounce -> [AlignmentPlanBindingClaim]
alignmentPlanAnnounceBindings (AlignmentPlanAnnounce _ _ _ _ bindings _ _) = bindings
alignmentPlanAnnounceRelations :: AlignmentPlanAnnounce -> [AlignmentPlanRelationClaim]
alignmentPlanAnnounceRelations (AlignmentPlanAnnounce _ _ _ _ _ relations _) = relations
alignmentPlanAnnounceCreatedCuts :: AlignmentPlanAnnounce -> [AlignmentCutAnnounce]
alignmentPlanAnnounceCreatedCuts (AlignmentPlanAnnounce _ _ _ _ _ _ cuts) = cuts

data AlignmentPlanAccepted = AlignmentPlanAccepted !AlignmentPlanId !HeraldEpoch !HeraldPublicationPrefix
  deriving stock (Eq, Show)

alignmentPlanAccepted :: AlignmentPlanId -> HeraldEpoch -> HeraldPublicationPrefix -> Either AlignmentProtocolProblem AlignmentPlanAccepted
alignmentPlanAccepted identifier herald prefix
  | herald `elem` fmap fst (physicalPlacementRevisionEntries (alignmentPlanIdPlacement identifier)) = Right (AlignmentPlanAccepted identifier herald prefix)
  | otherwise = Left (AlignmentPlanAcceptanceHeraldOutsidePlacement herald)

alignmentPlanAcceptedId :: AlignmentPlanAccepted -> AlignmentPlanId
alignmentPlanAcceptedId (AlignmentPlanAccepted identifier _ _) = identifier
alignmentPlanAcceptedHerald :: AlignmentPlanAccepted -> HeraldEpoch
alignmentPlanAcceptedHerald (AlignmentPlanAccepted _ herald _) = herald
alignmentPlanAcceptedPublicationPrefix :: AlignmentPlanAccepted -> HeraldPublicationPrefix
alignmentPlanAcceptedPublicationPrefix (AlignmentPlanAccepted _ _ prefix) = prefix

-- | An exact incarnation no longer available at its home's witnessed placement
-- revision. The semantic owner resolves this fact against placement/membership.
data AlignmentLostStore = AlignmentLostStore !HeraldEpoch !DeltaId !StoreIncarnationId !PlacementRevision
  deriving stock (Eq, Ord, Show)

alignmentLostStore :: HeraldEpoch -> DeltaId -> StoreIncarnationId -> PlacementRevision -> AlignmentLostStore
alignmentLostStore = AlignmentLostStore

alignmentLostStoreHome :: AlignmentLostStore -> HeraldEpoch
alignmentLostStoreHome (AlignmentLostStore home _ _ _) = home
alignmentLostStoreDelta :: AlignmentLostStore -> DeltaId
alignmentLostStoreDelta (AlignmentLostStore _ delta _ _) = delta
alignmentLostStoreIncarnation :: AlignmentLostStore -> StoreIncarnationId
alignmentLostStoreIncarnation (AlignmentLostStore _ _ incarnation _) = incarnation
alignmentLostStorePlacementRevision :: AlignmentLostStore -> PlacementRevision
alignmentLostStorePlacementRevision (AlignmentLostStore _ _ _ revision) = revision

-- | The owner freezes its first report per plan/reporter so reliable delivery
-- retains one immutable payload while subsequent losses are handled by reset.
data AlignmentPlanObsolete = AlignmentPlanObsolete !AlignmentPlanId !HeraldEpoch !(NonEmpty AlignmentLostStore)
  deriving stock (Eq, Show)

alignmentPlanObsolete :: AlignmentPlanId -> HeraldEpoch -> NonEmpty AlignmentLostStore -> Either AlignmentProtocolProblem AlignmentPlanObsolete
alignmentPlanObsolete identifier reporter supplied
  | reporter `notElem` fmap fst (physicalPlacementRevisionEntries (alignmentPlanIdPlacement identifier)) = Left (AlignmentPlanObsoleteHeraldOutsidePlacement reporter)
  | otherwise = case Set.toAscList (Set.fromList (NonEmpty.toList supplied)) of
      first : rest -> Right (AlignmentPlanObsolete identifier reporter (first :| rest))
      [] -> error "nonempty lost Store facts normalized to empty"

alignmentPlanObsoleteId :: AlignmentPlanObsolete -> AlignmentPlanId
alignmentPlanObsoleteId (AlignmentPlanObsolete identifier _ _) = identifier
alignmentPlanObsoleteReporter :: AlignmentPlanObsolete -> HeraldEpoch
alignmentPlanObsoleteReporter (AlignmentPlanObsolete _ reporter _) = reporter
alignmentPlanObsoleteLostStores :: AlignmentPlanObsolete -> NonEmpty AlignmentLostStore
alignmentPlanObsoleteLostStores (AlignmentPlanObsolete _ _ facts) = facts

alignmentPlanObsoleteCanonicalBytes :: AlignmentPlanObsolete -> ByteString
alignmentPlanObsoleteCanonicalBytes (AlignmentPlanObsolete identifier reporter facts) = Serialize.runPut $ do
  putSizedBytes "ECLIPS-ALIGNMENT-PLAN-OBSOLETE"
  putSizedBytes (alignmentPlanIdCanonicalBytes identifier)
  Serialize.putByteString (heraldEpochBytes reporter)
  Serialize.putWord64be (fromIntegral (NonEmpty.length facts))
  mapM_ putLost facts
  where
    putLost (AlignmentLostStore home delta incarnation revision) = do
      Serialize.putByteString (heraldEpochBytes home)
      Serialize.putByteString (deltaIdBytes delta)
      Serialize.putByteString (storeIncarnationIdBytes incarnation)
      Serialize.putWord64be (placementRevisionWord64 revision)

data AlignmentCutAnnounce = AlignmentCutAnnounce
  { generation :: ContextClassGenerationId,
    cut :: AlignmentCut
  }
  deriving stock (Eq, Show)

alignmentCutAnnounce :: AlignmentCut -> AlignmentCutAnnounce
alignmentCutAnnounce cut =
  AlignmentCutAnnounce (deriveContextClassGenerationId cut) cut

checkAlignmentCutAnnounce ::
  ContextClassGenerationId ->
  AlignmentCut ->
  Either AlignmentProtocolProblem AlignmentCutAnnounce
checkAlignmentCutAnnounce claimed cut
  | claimed == derived = Right (AlignmentCutAnnounce claimed cut)
  | otherwise = Left (AlignmentCutGenerationMismatch derived claimed)
  where
    derived = deriveContextClassGenerationId cut

alignmentCutAnnounceGeneration :: AlignmentCutAnnounce -> ContextClassGenerationId
alignmentCutAnnounceGeneration announce = announce.generation

alignmentCutAnnounceCut :: AlignmentCutAnnounce -> AlignmentCut
alignmentCutAnnounceCut announce = announce.cut

data AlignmentCutAccepted = AlignmentCutAccepted
  { generation :: ContextClassGenerationId,
    herald :: HeraldEpoch,
    topologyCut :: TopologyCutId,
    placementVector :: PhysicalPlacementRevisionVector,
    publicationPrefix :: HeraldPublicationPrefix
  }
  deriving stock (Eq, Show)

alignmentCutAccepted ::
  ContextClassGenerationId ->
  HeraldEpoch ->
  TopologyCutId ->
  PhysicalPlacementRevisionVector ->
  HeraldPublicationPrefix ->
  AlignmentCutAccepted
alignmentCutAccepted = AlignmentCutAccepted

alignmentCutAcceptedGeneration :: AlignmentCutAccepted -> ContextClassGenerationId
alignmentCutAcceptedGeneration accepted = accepted.generation

alignmentCutAcceptedHerald :: AlignmentCutAccepted -> HeraldEpoch
alignmentCutAcceptedHerald accepted = accepted.herald

alignmentCutAcceptedTopologyCut :: AlignmentCutAccepted -> TopologyCutId
alignmentCutAcceptedTopologyCut accepted = accepted.topologyCut

alignmentCutAcceptedPlacementVector ::
  AlignmentCutAccepted -> PhysicalPlacementRevisionVector
alignmentCutAcceptedPlacementVector accepted = accepted.placementVector

alignmentCutAcceptedPublicationPrefix ::
  AlignmentCutAccepted -> HeraldPublicationPrefix
alignmentCutAcceptedPublicationPrefix accepted = accepted.publicationPrefix

data ClassMemberReady = ClassMemberReady
  { evidenceSequence :: AlignmentEvidenceSequence,
    generation :: ContextClassGenerationId,
    storeIncarnation :: StoreIncarnationId,
    predecessorAndBasePrefixDigest :: MemberReadyEvidenceDigest,
    storeRevision :: StoreRevision
  }
  deriving stock (Eq, Show)

classMemberReady ::
  AlignmentEvidenceSequence ->
  ContextClassGenerationId ->
  StoreIncarnationId ->
  MemberReadyEvidenceDigest ->
  StoreRevision ->
  ClassMemberReady
classMemberReady = ClassMemberReady

classMemberReadyEvidenceSequence :: ClassMemberReady -> AlignmentEvidenceSequence
classMemberReadyEvidenceSequence ready = ready.evidenceSequence

classMemberReadyGeneration :: ClassMemberReady -> ContextClassGenerationId
classMemberReadyGeneration ready = ready.generation

classMemberReadyStoreIncarnation :: ClassMemberReady -> StoreIncarnationId
classMemberReadyStoreIncarnation ready = ready.storeIncarnation

classMemberReadyPredecessorAndBasePrefixDigest ::
  ClassMemberReady -> MemberReadyEvidenceDigest
classMemberReadyPredecessorAndBasePrefixDigest ready =
  ready.predecessorAndBasePrefixDigest

classMemberReadyStoreRevision :: ClassMemberReady -> StoreRevision
classMemberReadyStoreRevision ready = ready.storeRevision

classMemberReadyCanonicalBytes :: ClassMemberReady -> ByteString
classMemberReadyCanonicalBytes ready = Serialize.runPut $ do
  putSizedBytes "ECLIPS-ALIGNMENT-CLASS-MEMBER-READY-V1"
  Serialize.putWord64be
    (alignmentEvidenceSequenceWord64 ready.evidenceSequence)
  Serialize.putByteString (contextClassGenerationIdBytes ready.generation)
  Serialize.putByteString (storeIncarnationIdBytes ready.storeIncarnation)
  Serialize.putByteString
    (memberReadyEvidenceDigestBytes ready.predecessorAndBasePrefixDigest)
  Serialize.putWord64be (storeRevisionWord64 ready.storeRevision)

-- | Digest the complete immutable ready-evidence set independently of arrival
-- order. Exact duplicates collapse; unequal entries remain digest-visible and
-- are rejected by the owner before certificate construction.
classMemberReadySetDigest :: [ClassMemberReady] -> MemberReadyEvidenceDigest
classMemberReadySetDigest supplied =
  deriveMemberReadyEvidenceDigest
    ( Serialize.runPut $ do
        putSizedBytes "ECLIPS-ALIGNMENT-CLASS-MEMBER-READY-SET-V1"
        let canonical =
              Set.toAscList
                (Set.fromList (fmap classMemberReadyCanonicalBytes supplied))
        Serialize.putWord64be (fromIntegral (length canonical))
        mapM_ putSizedBytes canonical
    )

data HistoricalCertificate = HistoricalCertificate
  { generation :: ContextClassGenerationId,
    sourceStoreIncarnation :: StoreIncarnationId,
    exactMemberReadyDigest :: MemberReadyEvidenceDigest,
    completedPredecessorPrefixDigest :: BootstrapEvidenceDigest,
    certifiedStoreRevision :: StoreRevision
  }
  deriving stock (Eq, Show)

historicalCertificate ::
  ContextClassGenerationId ->
  StoreIncarnationId ->
  MemberReadyEvidenceDigest ->
  BootstrapEvidenceDigest ->
  StoreRevision ->
  HistoricalCertificate
historicalCertificate = HistoricalCertificate

historicalCertificateClassGeneration ::
  HistoricalCertificate -> ContextClassGenerationId
historicalCertificateClassGeneration certificate = certificate.generation

historicalCertificateSourceStoreIncarnation ::
  HistoricalCertificate -> StoreIncarnationId
historicalCertificateSourceStoreIncarnation certificate =
  certificate.sourceStoreIncarnation

historicalCertificateExactMemberReadyDigest ::
  HistoricalCertificate -> MemberReadyEvidenceDigest
historicalCertificateExactMemberReadyDigest certificate =
  certificate.exactMemberReadyDigest

historicalCertificateCompletedPredecessorPrefixDigest ::
  HistoricalCertificate -> BootstrapEvidenceDigest
historicalCertificateCompletedPredecessorPrefixDigest certificate =
  certificate.completedPredecessorPrefixDigest

historicalCertificateCertifiedStoreRevision ::
  HistoricalCertificate -> StoreRevision
historicalCertificateCertifiedStoreRevision certificate =
  certificate.certifiedStoreRevision

historicalCertificateCanonicalBytes :: HistoricalCertificate -> ByteString
historicalCertificateCanonicalBytes certificate = Serialize.runPut $ do
  putSizedBytes "ECLIPS-HISTORICAL-CERTIFICATE-V1"
  Serialize.putByteString (contextClassGenerationIdBytes certificate.generation)
  Serialize.putByteString
    (storeIncarnationIdBytes certificate.sourceStoreIncarnation)
  Serialize.putByteString
    (memberReadyEvidenceDigestBytes certificate.exactMemberReadyDigest)
  Serialize.putByteString
    (bootstrapEvidenceDigestBytes certificate.completedPredecessorPrefixDigest)
  Serialize.putWord64be (storeRevisionWord64 certificate.certifiedStoreRevision)

historicalCertificateDigest ::
  HistoricalCertificate -> HistoricalCertificateDigest
historicalCertificateDigest =
  deriveHistoricalCertificateDigest . historicalCertificateCanonicalBytes

data DestinationStore = DestinationStore
  { delta :: DeltaId,
    storeIncarnation :: StoreIncarnationId
  }
  deriving stock (Eq, Ord, Show)

destinationStore :: DeltaId -> StoreIncarnationId -> DestinationStore
destinationStore = DestinationStore

destinationStoreDelta :: DestinationStore -> DeltaId
destinationStoreDelta store = store.delta

destinationStoreIncarnation :: DestinationStore -> StoreIncarnationId
destinationStoreIncarnation store = store.storeIncarnation

data AlignmentObligation = AlignmentObligation
  { identifier :: AlignmentObligationId,
    cause :: StructuralConsequenceCause,
    sortId :: SortId,
    sortOccurrence :: SortDefinitionOccurrenceId,
    destinationGeneration :: ContextClassGenerationId,
    destinationStores :: NonEmpty DestinationStore,
    sourceGeneration :: ContextClassGenerationId,
    frozenPathStrength :: ReplicaStrength
  }
  deriving stock (Eq, Show)

alignmentObligation ::
  AlignmentObligationId ->
  StructuralConsequenceCause ->
  SortId ->
  SortDefinitionOccurrenceId ->
  ContextClassGenerationId ->
  NonEmpty DestinationStore ->
  ContextClassGenerationId ->
  ReplicaStrength ->
  Either AlignmentProtocolProblem AlignmentObligation
alignmentObligation identifier cause sortId occurrence destinationGeneration stores sourceGeneration strength = do
  normalized <- normalizeDestinationStores stores
  Right
    AlignmentObligation
      { identifier,
        cause,
        sortId,
        sortOccurrence = occurrence,
        destinationGeneration,
        destinationStores = normalized,
        sourceGeneration,
        frozenPathStrength = strength
      }

alignmentObligationIdValue :: AlignmentObligation -> AlignmentObligationId
alignmentObligationIdValue obligation = obligation.identifier

alignmentObligationCause :: AlignmentObligation -> StructuralConsequenceCause
alignmentObligationCause obligation = obligation.cause

alignmentObligationSortId :: AlignmentObligation -> SortId
alignmentObligationSortId obligation = obligation.sortId

alignmentObligationSortDefinitionOccurrenceId ::
  AlignmentObligation -> SortDefinitionOccurrenceId
alignmentObligationSortDefinitionOccurrenceId obligation = obligation.sortOccurrence

alignmentObligationDestinationGeneration ::
  AlignmentObligation -> ContextClassGenerationId
alignmentObligationDestinationGeneration obligation =
  obligation.destinationGeneration

alignmentObligationDestinationStores ::
  AlignmentObligation -> NonEmpty DestinationStore
alignmentObligationDestinationStores obligation = obligation.destinationStores

alignmentObligationSourceGeneration ::
  AlignmentObligation -> ContextClassGenerationId
alignmentObligationSourceGeneration obligation = obligation.sourceGeneration

alignmentObligationFrozenContextPathStrength ::
  AlignmentObligation -> ReplicaStrength
alignmentObligationFrozenContextPathStrength obligation =
  obligation.frozenPathStrength

data AlignmentAttempt = AlignmentAttempt
  { obligation :: AlignmentObligation,
    subscriptionId :: AlignmentSubscriptionId,
    sourceHerald :: HeraldEpoch,
    certificate :: HistoricalCertificate
  }
  deriving stock (Eq, Show)

alignmentAttempt ::
  AlignmentObligation ->
  AlignmentSubscriptionId ->
  HeraldEpoch ->
  HistoricalCertificate ->
  Either AlignmentProtocolProblem AlignmentAttempt
alignmentAttempt obligation subscriptionId sourceHerald certificate
  | historicalCertificateClassGeneration certificate
      /= alignmentObligationSourceGeneration obligation =
      Left
        ( AlignmentAttemptSourceGenerationMismatch
            (alignmentObligationSourceGeneration obligation)
            (historicalCertificateClassGeneration certificate)
        )
  | alignmentSubscriptionIdDestinationHerald subscriptionId
      /= alignmentObligationIdDestinationHerald
        (alignmentObligationIdValue obligation) =
      Left
        ( AlignmentAttemptSubscriptionDestinationMismatch
            ( alignmentObligationIdDestinationHerald
                (alignmentObligationIdValue obligation)
            )
            (alignmentSubscriptionIdDestinationHerald subscriptionId)
        )
  | otherwise =
      Right AlignmentAttempt {obligation, subscriptionId, sourceHerald, certificate}

alignmentAttemptObligation :: AlignmentAttempt -> AlignmentObligation
alignmentAttemptObligation attempt = attempt.obligation

alignmentAttemptObligationId :: AlignmentAttempt -> AlignmentObligationId
alignmentAttemptObligationId = alignmentObligationIdValue . alignmentAttemptObligation

alignmentAttemptSubscriptionId :: AlignmentAttempt -> AlignmentSubscriptionId
alignmentAttemptSubscriptionId attempt = attempt.subscriptionId

alignmentAttemptSourceHerald :: AlignmentAttempt -> HeraldEpoch
alignmentAttemptSourceHerald attempt = attempt.sourceHerald

alignmentAttemptSourceStoreIncarnation :: AlignmentAttempt -> StoreIncarnationId
alignmentAttemptSourceStoreIncarnation =
  historicalCertificateSourceStoreIncarnation . alignmentAttemptHistoricalCertificate

alignmentAttemptHistoricalCertificate :: AlignmentAttempt -> HistoricalCertificate
alignmentAttemptHistoricalCertificate attempt = attempt.certificate

alignmentAttemptHistoricalCertificateDigest ::
  AlignmentAttempt -> HistoricalCertificateDigest
alignmentAttemptHistoricalCertificateDigest =
  historicalCertificateDigest . alignmentAttemptHistoricalCertificate

alignmentAttemptDestinationStores ::
  AlignmentAttempt -> NonEmpty DestinationStore
alignmentAttemptDestinationStores =
  alignmentObligationDestinationStores . alignmentAttemptObligation

alignmentAttemptFrozenContextPathStrength ::
  AlignmentAttempt -> ReplicaStrength
alignmentAttemptFrozenContextPathStrength =
  alignmentObligationFrozenContextPathStrength . alignmentAttemptObligation

data AlignmentSourceProof
  = FinalCertificate
      ContextClassGenerationId
      HistoricalCertificateDigest
  | BootstrapProof
      ContextClassGenerationId
      BootstrapEvidenceDigest
  deriving stock (Eq, Show)

data AlignmentSubscribe = AlignmentSubscribe
  { obligation :: AlignmentObligation,
    subscriptionId :: AlignmentSubscriptionId,
    sourceStoreIncarnation :: StoreIncarnationId,
    sourceProof :: AlignmentSourceProof
  }
  deriving stock (Eq, Show)

alignmentSubscribe ::
  AlignmentObligation ->
  AlignmentSubscriptionId ->
  StoreIncarnationId ->
  AlignmentSourceProof ->
  Either AlignmentProtocolProblem AlignmentSubscribe
alignmentSubscribe obligation subscriptionId sourceStore sourceProof = do
  let obligationId = alignmentObligationIdValue obligation
  if alignmentObligationIdDestinationHerald obligationId
    == alignmentSubscriptionIdDestinationHerald subscriptionId
    then Right ()
    else
      Left
        ( AlignmentSubscribeDestinationMismatch
            (alignmentObligationIdDestinationHerald obligationId)
            (alignmentSubscriptionIdDestinationHerald subscriptionId)
        )
  let expectedGeneration = case sourceProof of
        FinalCertificate _ _ -> alignmentObligationSourceGeneration obligation
        BootstrapProof _ _ -> alignmentObligationDestinationGeneration obligation
      actualGeneration = case sourceProof of
        FinalCertificate generation _ -> generation
        BootstrapProof generation _ -> generation
  if expectedGeneration == actualGeneration
    then Right ()
    else
      Left
        ( AlignmentSubscribeSourceProofGenerationMismatch
            expectedGeneration
            actualGeneration
        )
  Right
    AlignmentSubscribe
      { obligation,
        subscriptionId,
        sourceStoreIncarnation = sourceStore,
        sourceProof
      }

alignmentSubscribeObligation :: AlignmentSubscribe -> AlignmentObligation
alignmentSubscribeObligation subscribe = subscribe.obligation

alignmentSubscribeObligationId :: AlignmentSubscribe -> AlignmentObligationId
alignmentSubscribeObligationId =
  alignmentObligationIdValue . alignmentSubscribeObligation

alignmentSubscribeSubscriptionId :: AlignmentSubscribe -> AlignmentSubscriptionId
alignmentSubscribeSubscriptionId subscribe = subscribe.subscriptionId

alignmentSubscribeSourceStoreIncarnation ::
  AlignmentSubscribe -> StoreIncarnationId
alignmentSubscribeSourceStoreIncarnation subscribe =
  subscribe.sourceStoreIncarnation

alignmentSubscribeSourceProof :: AlignmentSubscribe -> AlignmentSourceProof
alignmentSubscribeSourceProof subscribe = subscribe.sourceProof

alignmentSubscribeDestinationStores ::
  AlignmentSubscribe -> NonEmpty DestinationStore
alignmentSubscribeDestinationStores =
  alignmentObligationDestinationStores . alignmentSubscribeObligation

data RetainedObservationOrigin
  = PrimordialRetainedObservation
  | RoutedRetainedObservation
      ProcessEpochId
      TopologyCutId
      ControlIndex
      (Maybe StructuralOccurrenceStamp)
  deriving stock (Eq, Show)

data RetainedPublicationEvidence = RetainedPublicationEvidence
  { publicationId :: PublicationId,
    sortId :: SortId,
    sortOccurrence :: SortDefinitionOccurrenceId,
    canonicalValue :: CanonicalValueBytes,
    sourceStrength :: ReplicaStrength,
    origin :: RetainedObservationOrigin
  }
  deriving stock (Eq, Show)

primordialRetainedPublicationEvidence ::
  PublicationId ->
  SortId ->
  SortDefinitionOccurrenceId ->
  CanonicalValueBytes ->
  ReplicaStrength ->
  RetainedPublicationEvidence
primordialRetainedPublicationEvidence publicationId sortId occurrence canonicalValue sourceStrength =
  RetainedPublicationEvidence
    { publicationId,
      sortId,
      sortOccurrence = occurrence,
      canonicalValue,
      sourceStrength,
      origin = PrimordialRetainedObservation
    }

retainedPublicationEvidence ::
  PublicationId ->
  ProcessEpochId ->
  SortId ->
  SortDefinitionOccurrenceId ->
  CanonicalValueBytes ->
  ReplicaStrength ->
  TopologyCutId ->
  ControlIndex ->
  Maybe StructuralOccurrenceStamp ->
  Either AlignmentProtocolProblem RetainedPublicationEvidence
retainedPublicationEvidence publicationId sourceProcess sortId occurrence canonicalValue sourceStrength topology control stamp = do
  case stamp of
    Nothing -> Right ()
    Just structural
      | structuralOccurrenceStampPublication structural /= publicationId ->
          Left
            ( RetainedEvidenceStampPublicationMismatch
                publicationId
                (structuralOccurrenceStampPublication structural)
            )
      | structuralOccurrenceSourceHeraldEpoch
          (structuralOccurrenceStampOccurrence structural)
          /= publicationSourceHeraldEpoch publicationId ->
          Left
            ( RetainedEvidenceStampSourceMismatch
                (publicationSourceHeraldEpoch publicationId)
                ( structuralOccurrenceSourceHeraldEpoch
                    (structuralOccurrenceStampOccurrence structural)
                )
            )
      | otherwise -> Right ()
  Right
    RetainedPublicationEvidence
      { publicationId,
        sortId,
        sortOccurrence = occurrence,
        canonicalValue,
        sourceStrength,
        origin = RoutedRetainedObservation sourceProcess topology control stamp
      }

retainedPublicationEvidencePublicationId ::
  RetainedPublicationEvidence -> PublicationId
retainedPublicationEvidencePublicationId evidence = evidence.publicationId

retainedPublicationEvidenceSourceProcess ::
  RetainedPublicationEvidence -> Maybe ProcessEpochId
retainedPublicationEvidenceSourceProcess evidence = case evidence.origin of
  PrimordialRetainedObservation -> Nothing
  RoutedRetainedObservation source _ _ _ -> Just source

retainedPublicationEvidenceSortId :: RetainedPublicationEvidence -> SortId
retainedPublicationEvidenceSortId evidence = evidence.sortId

retainedPublicationEvidenceSortDefinitionOccurrenceId ::
  RetainedPublicationEvidence -> SortDefinitionOccurrenceId
retainedPublicationEvidenceSortDefinitionOccurrenceId evidence =
  evidence.sortOccurrence

retainedPublicationEvidenceCanonicalValue ::
  RetainedPublicationEvidence -> CanonicalValueBytes
retainedPublicationEvidenceCanonicalValue evidence = evidence.canonicalValue

retainedPublicationEvidenceSourceStrength ::
  RetainedPublicationEvidence -> ReplicaStrength
retainedPublicationEvidenceSourceStrength evidence = evidence.sourceStrength

retainedPublicationEvidenceOrigin ::
  RetainedPublicationEvidence -> RetainedObservationOrigin
retainedPublicationEvidenceOrigin evidence = evidence.origin

retainedPublicationEvidenceSourceTopologyPrerequisite ::
  RetainedPublicationEvidence -> Maybe TopologyCutId
retainedPublicationEvidenceSourceTopologyPrerequisite evidence = case evidence.origin of
  PrimordialRetainedObservation -> Nothing
  RoutedRetainedObservation _ topology _ _ -> Just topology

retainedPublicationEvidenceControlPrerequisite ::
  RetainedPublicationEvidence -> Maybe ControlIndex
retainedPublicationEvidenceControlPrerequisite evidence = case evidence.origin of
  PrimordialRetainedObservation -> Nothing
  RoutedRetainedObservation _ _ control _ -> Just control

retainedPublicationEvidenceStructuralStamp ::
  RetainedPublicationEvidence -> Maybe StructuralOccurrenceStamp
retainedPublicationEvidenceStructuralStamp evidence = case evidence.origin of
  PrimordialRetainedObservation -> Nothing
  RoutedRetainedObservation _ _ _ stamp -> stamp

retainedPublicationEvidenceCanonicalBytes ::
  RetainedPublicationEvidence -> ByteString
retainedPublicationEvidenceCanonicalBytes evidence = Serialize.runPut $ do
  putSizedBytes "ECLIPS-ALIGNMENT-RETAINED-PUBLICATION-V1"
  putPublicationId evidence.publicationId
  Serialize.putByteString (sortIdBytes evidence.sortId)
  Serialize.putByteString (sortDefinitionOccurrenceIdBytes evidence.sortOccurrence)
  putSizedBytes (canonicalValueByteString evidence.canonicalValue)
  Serialize.putWord8 (replicaStrengthTag evidence.sourceStrength)
  case evidence.origin of
    PrimordialRetainedObservation -> Serialize.putWord8 0
    RoutedRetainedObservation source topology control stamp -> do
      Serialize.putWord8 1
      Serialize.putByteString (processEpochIdBytes source)
      Serialize.putByteString (topologyCutIdBytes topology)
      Serialize.putWord64be (controlIndexWord64 control)
      case stamp of
        Nothing -> Serialize.putWord8 0
        Just structural -> Serialize.putWord8 1 >> putStructuralStamp structural

data RetainedStateEvidence = RetainedStateEvidence
  { representative :: RetainedPublicationEvidence,
    strengthWitness :: RetainedPublicationEvidence,
    joinedStrength :: ReplicaStrength
  }
  deriving stock (Eq, Show)

retainedStateEvidence ::
  RetainedPublicationEvidence ->
  RetainedPublicationEvidence ->
  ReplicaStrength ->
  Either AlignmentProtocolProblem RetainedStateEvidence
retainedStateEvidence representative witness joined
  | representative.sortId /= witness.sortId =
      Left
        ( RetainedStateEvidenceSortMismatch
            representative.sortId
            witness.sortId
        )
  | representative.sortOccurrence /= witness.sortOccurrence =
      Left
        ( RetainedStateEvidenceOccurrenceMismatch
            representative.sortOccurrence
            witness.sortOccurrence
        )
  | representative.canonicalValue /= witness.canonicalValue =
      Left RetainedStateEvidenceLogicalValueMismatch
  | joined /= max representative.sourceStrength witness.sourceStrength =
      Left
        ( RetainedStateEvidenceStrengthWitnessMismatch
            (max representative.sourceStrength witness.sourceStrength)
            joined
        )
  | witness.sourceStrength /= joined =
      Left
        ( RetainedStateEvidenceStrengthWitnessMismatch
            joined
            witness.sourceStrength
        )
  | otherwise =
      Right
        RetainedStateEvidence
          { representative,
            strengthWitness = witness,
            joinedStrength = joined
          }

retainedStateEvidenceRepresentative ::
  RetainedStateEvidence -> RetainedPublicationEvidence
retainedStateEvidenceRepresentative fact = fact.representative

retainedStateEvidenceStrengthWitness ::
  RetainedStateEvidence -> RetainedPublicationEvidence
retainedStateEvidenceStrengthWitness fact = fact.strengthWitness

retainedStateEvidenceJoinedStrength :: RetainedStateEvidence -> ReplicaStrength
retainedStateEvidenceJoinedStrength fact = fact.joinedStrength

retainedStateEvidenceCanonicalBytes :: RetainedStateEvidence -> ByteString
retainedStateEvidenceCanonicalBytes fact = Serialize.runPut $ do
  putSizedBytes "ECLIPS-ALIGNMENT-RETAINED-STATE-V1"
  putSizedBytes
    (retainedPublicationEvidenceCanonicalBytes fact.representative)
  putSizedBytes
    (retainedPublicationEvidenceCanonicalBytes fact.strengthWitness)
  Serialize.putWord8 (replicaStrengthTag fact.joinedStrength)

alignmentSemanticSnapshotCanonicalBytes ::
  ContextClassGenerationId ->
  StoreIncarnationId ->
  StoreRevision ->
  [RetainedStateEvidence] ->
  ByteString
alignmentSemanticSnapshotCanonicalBytes generation sourceStore baseRevision retained =
  Serialize.runPut $ do
    putSizedBytes "ECLIPS-ALIGNMENT-SEMANTIC-SNAPSHOT-V1"
    Serialize.putByteString (contextClassGenerationIdBytes generation)
    Serialize.putByteString (storeIncarnationIdBytes sourceStore)
    Serialize.putWord64be (storeRevisionWord64 baseRevision)
    let canonical = sortOn retainedStateEvidenceCanonicalBytes retained
    Serialize.putWord64be (fromIntegral (length canonical))
    mapM_ (putSizedBytes . retainedStateEvidenceCanonicalBytes) canonical

alignmentSemanticSnapshotDigest ::
  ContextClassGenerationId ->
  StoreIncarnationId ->
  StoreRevision ->
  [RetainedStateEvidence] ->
  AlignmentSnapshotDigest
alignmentSemanticSnapshotDigest generation sourceStore baseRevision =
  deriveAlignmentSnapshotDigest
    . alignmentSemanticSnapshotCanonicalBytes generation sourceStore baseRevision

data AlignmentSnapshotStart = AlignmentSnapshotStart
  { subscriptionId :: AlignmentSubscriptionId,
    baseRevision :: StoreRevision,
    semanticDigest :: AlignmentSnapshotDigest,
    chunkCount :: Word64
  }
  deriving stock (Eq, Show)

alignmentSnapshotStart ::
  AlignmentSubscriptionId ->
  StoreRevision ->
  AlignmentSnapshotDigest ->
  Word64 ->
  AlignmentSnapshotStart
alignmentSnapshotStart = AlignmentSnapshotStart

alignmentSnapshotStartSubscriptionId ::
  AlignmentSnapshotStart -> AlignmentSubscriptionId
alignmentSnapshotStartSubscriptionId start = start.subscriptionId

alignmentSnapshotStartBaseRevision :: AlignmentSnapshotStart -> StoreRevision
alignmentSnapshotStartBaseRevision start = start.baseRevision

alignmentSnapshotStartSemanticDigest ::
  AlignmentSnapshotStart -> AlignmentSnapshotDigest
alignmentSnapshotStartSemanticDigest start = start.semanticDigest

alignmentSnapshotStartChunkCount :: AlignmentSnapshotStart -> Word64
alignmentSnapshotStartChunkCount start = start.chunkCount

data AlignmentSnapshotChunk = AlignmentSnapshotChunk
  { subscriptionId :: AlignmentSubscriptionId,
    chunkNumber :: Word64,
    retainedStates :: [RetainedStateEvidence]
  }
  deriving stock (Eq, Show)

alignmentSnapshotChunk ::
  AlignmentSubscriptionId ->
  Word64 ->
  [RetainedStateEvidence] ->
  AlignmentSnapshotChunk
alignmentSnapshotChunk = AlignmentSnapshotChunk

alignmentSnapshotChunkSubscriptionId ::
  AlignmentSnapshotChunk -> AlignmentSubscriptionId
alignmentSnapshotChunkSubscriptionId chunk = chunk.subscriptionId

alignmentSnapshotChunkNumber :: AlignmentSnapshotChunk -> Word64
alignmentSnapshotChunkNumber chunk = chunk.chunkNumber

alignmentSnapshotChunkRetainedStates ::
  AlignmentSnapshotChunk -> [RetainedStateEvidence]
alignmentSnapshotChunkRetainedStates chunk = chunk.retainedStates

data AlignmentSnapshotEnd = AlignmentSnapshotEnd
  { subscriptionId :: AlignmentSubscriptionId,
    baseRevision :: StoreRevision,
    semanticDigest :: AlignmentSnapshotDigest
  }
  deriving stock (Eq, Show)

alignmentSnapshotEnd ::
  AlignmentSubscriptionId ->
  StoreRevision ->
  AlignmentSnapshotDigest ->
  AlignmentSnapshotEnd
alignmentSnapshotEnd = AlignmentSnapshotEnd

alignmentSnapshotEndSubscriptionId ::
  AlignmentSnapshotEnd -> AlignmentSubscriptionId
alignmentSnapshotEndSubscriptionId end = end.subscriptionId

alignmentSnapshotEndBaseRevision :: AlignmentSnapshotEnd -> StoreRevision
alignmentSnapshotEndBaseRevision end = end.baseRevision

alignmentSnapshotEndSemanticDigest ::
  AlignmentSnapshotEnd -> AlignmentSnapshotDigest
alignmentSnapshotEndSemanticDigest end = end.semanticDigest

data AlignmentChange = AlignmentChange
  { subscriptionId :: AlignmentSubscriptionId,
    storeRevision :: StoreRevision,
    retainedTransition :: RetainedPublicationEvidence
  }
  deriving stock (Eq, Show)

alignmentChange ::
  AlignmentSubscriptionId ->
  StoreRevision ->
  RetainedPublicationEvidence ->
  AlignmentChange
alignmentChange = AlignmentChange

alignmentChangeSubscriptionId :: AlignmentChange -> AlignmentSubscriptionId
alignmentChangeSubscriptionId change = change.subscriptionId

alignmentChangeStoreRevision :: AlignmentChange -> StoreRevision
alignmentChangeStoreRevision change = change.storeRevision

alignmentChangeRetainedTransition ::
  AlignmentChange -> RetainedPublicationEvidence
alignmentChangeRetainedTransition change = change.retainedTransition

data AlignmentLive = AlignmentLive
  { subscriptionId :: AlignmentSubscriptionId,
    throughSourceStoreRevision :: StoreRevision
  }
  deriving stock (Eq, Show)

alignmentLive :: AlignmentSubscriptionId -> StoreRevision -> AlignmentLive
alignmentLive = AlignmentLive

alignmentLiveSubscriptionId :: AlignmentLive -> AlignmentSubscriptionId
alignmentLiveSubscriptionId live = live.subscriptionId

alignmentLiveThroughSourceStoreRevision :: AlignmentLive -> StoreRevision
alignmentLiveThroughSourceStoreRevision live = live.throughSourceStoreRevision

data AlignmentAck = AlignmentAck
  { subscriptionId :: AlignmentSubscriptionId,
    appliedSourceStoreRevision :: StoreRevision
  }
  deriving stock (Eq, Show)

alignmentAck :: AlignmentSubscriptionId -> StoreRevision -> AlignmentAck
alignmentAck = AlignmentAck

alignmentAckSubscriptionId :: AlignmentAck -> AlignmentSubscriptionId
alignmentAckSubscriptionId acknowledgement = acknowledgement.subscriptionId

alignmentAckAppliedSourceStoreRevision :: AlignmentAck -> StoreRevision
alignmentAckAppliedSourceStoreRevision acknowledgement =
  acknowledgement.appliedSourceStoreRevision

data AlignmentCancelReason
  = AlignmentRelationRemoved
  | AlignmentSourceIncarnationLost
  | AlignmentDestinationIncarnationLost
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data AlignmentCancel = AlignmentCancel
  { subscriptionId :: AlignmentSubscriptionId,
    reason :: AlignmentCancelReason
  }
  deriving stock (Eq, Show)

alignmentCancel :: AlignmentSubscriptionId -> AlignmentCancelReason -> AlignmentCancel
alignmentCancel = AlignmentCancel

alignmentCancelSubscriptionId :: AlignmentCancel -> AlignmentSubscriptionId
alignmentCancelSubscriptionId cancellation = cancellation.subscriptionId

alignmentCancelReason :: AlignmentCancel -> AlignmentCancelReason
alignmentCancelReason cancellation = cancellation.reason

data AlignmentRouteCutoverMarker = AlignmentRouteCutoverMarker
  { plan :: AlignmentPlanId,
    generation :: ContextClassGenerationId,
    sourceHerald :: HeraldEpoch,
    predecessorHerald :: HeraldEpoch
  }
  deriving stock (Eq, Show)

alignmentRouteCutoverMarker ::
  AlignmentPlanId ->
  ContextClassGenerationId ->
  HeraldEpoch ->
  HeraldEpoch ->
  Either AlignmentProtocolProblem AlignmentRouteCutoverMarker
alignmentRouteCutoverMarker identifier generation source predecessor
  | source == predecessor = Left (AlignmentRouteCutoverEndpointsEqual source)
  | otherwise =
      Right
        AlignmentRouteCutoverMarker
          { plan = identifier,
            generation,
            sourceHerald = source,
            predecessorHerald = predecessor
          }

alignmentRouteCutoverMarkerPlan :: AlignmentRouteCutoverMarker -> AlignmentPlanId
alignmentRouteCutoverMarkerPlan marker = marker.plan

alignmentRouteCutoverMarkerGeneration ::
  AlignmentRouteCutoverMarker -> ContextClassGenerationId
alignmentRouteCutoverMarkerGeneration marker = marker.generation

alignmentRouteCutoverMarkerSourceHerald ::
  AlignmentRouteCutoverMarker -> HeraldEpoch
alignmentRouteCutoverMarkerSourceHerald marker = marker.sourceHerald

alignmentRouteCutoverMarkerPredecessorHerald ::
  AlignmentRouteCutoverMarker -> HeraldEpoch
alignmentRouteCutoverMarkerPredecessorHerald marker = marker.predecessorHerald

alignmentRouteCutoverMarkerCanonicalBytes ::
  AlignmentRouteCutoverMarker -> ByteString
alignmentRouteCutoverMarkerCanonicalBytes marker = Serialize.runPut $ do
  putSizedBytes "ECLIPS-ALIGNMENT-ROUTE-CUTOVER-MARKER"
  putSizedBytes (alignmentPlanIdCanonicalBytes marker.plan)
  Serialize.putByteString (contextClassGenerationIdBytes marker.generation)
  Serialize.putByteString (heraldEpochBytes marker.sourceHerald)
  Serialize.putByteString (heraldEpochBytes marker.predecessorHerald)

alignmentRouteCutoverMarkerEvidenceDigest ::
  AlignmentRouteCutoverMarker -> RouteCutoverEvidenceDigest
alignmentRouteCutoverMarkerEvidenceDigest =
  deriveRouteCutoverEvidenceDigest . alignmentRouteCutoverMarkerCanonicalBytes

-- | Canonical evidence for the complete source-marker set expected by one
-- predecessor at an immutable generation cut.  The placement vector is part
-- of the generation identity; its Herald coordinates select the exact source
-- set while individual marker transcripts bind every source endpoint.
alignmentRouteCutoverSetEvidenceDigest ::
  ContextClassGenerationId ->
  HeraldEpoch ->
  AlignmentCut ->
  RouteCutoverEvidenceDigest
alignmentRouteCutoverSetEvidenceDigest generation predecessor cut =
  deriveRouteCutoverEvidenceDigest
    ( Serialize.runPut $ do
        Serialize.putByteString (contextClassGenerationIdBytes generation)
        Serialize.putByteString (heraldEpochBytes predecessor)
        let identifier = alignmentPlanIdFromClaimedCoordinatesAtAttempt (alignmentCutAttempt cut) (sortOccurrence (alignmentCutSortId cut) (alignmentCutSortDefinitionOccurrenceId cut)) (deriveTopologyCutId (alignmentCutTopologyCut cut)) (alignmentCutPhysicalPlacementRevisionVector cut)
            sources =
              filter
                (/= predecessor)
                ( fmap
                    fst
                    ( NonEmpty.toList
                        ( physicalPlacementRevisionEntries
                            (alignmentCutPhysicalPlacementRevisionVector cut)
                        )
                    )
                )
            markers =
              [ alignmentRouteCutoverMarkerCanonicalBytes marker
              | source <- sources,
                Right marker <-
                  [alignmentRouteCutoverMarker identifier generation source predecessor]
              ]
        Serialize.putWord64be (fromIntegral (length markers))
        mapM_ putSizedBytes markers
    )

data AlignmentControl
  = AlignmentPlanAnnounced AlignmentPlanAnnounce
  | AlignmentPlanAcceptanceAdvertised AlignmentPlanAccepted
  | AlignmentPlanObsoleteAdvertised AlignmentPlanObsolete
  | AlignmentCutAnnounced AlignmentCutAnnounce
  | AlignmentCutAcceptanceAdvertised AlignmentCutAccepted
  | AlignmentMemberReadyAdvertised ClassMemberReady
  | AlignmentHistoricalCertificateAdvertised HistoricalCertificate
  | AlignmentSubscribeRequested AlignmentSubscribe
  | AlignmentSnapshotStarted AlignmentSnapshotStart
  | AlignmentSnapshotChunkTransferred AlignmentSnapshotChunk
  | AlignmentSnapshotEnded AlignmentSnapshotEnd
  | AlignmentChangeTransferred AlignmentChange
  | AlignmentLiveAdvertised AlignmentLive
  | AlignmentAcknowledged AlignmentAck
  | AlignmentCancelled AlignmentCancel
  | AlignmentProbeMarker DisappearanceProbeId AlignmentSubscriptionId StoreRevision
  deriving stock (Eq, Show)

normalizeDestinationStores ::
  NonEmpty DestinationStore ->
  Either AlignmentProtocolProblem (NonEmpty DestinationStore)
normalizeDestinationStores supplied = do
  byDelta <- foldl insertDelta (Right Map.empty) (NonEmpty.toList supplied)
  _ <- foldl insertIncarnation (Right Map.empty) (Map.elems byDelta)
  case Map.elems byDelta of
    first : remaining -> Right (first :| remaining)
    [] -> error "non-empty destination stores normalized to empty"
  where
    insertDelta accumulated candidate = do
      current <- accumulated
      case Map.lookup candidate.delta current of
        Nothing -> Right (Map.insert candidate.delta candidate current)
        Just incumbent
          | incumbent == candidate -> Right current
          | otherwise ->
              Left
                ( AlignmentDestinationDeltaConflict
                    candidate.delta
                    incumbent
                    candidate
                )
    insertIncarnation accumulated candidate = do
      current <- accumulated
      case Map.lookup candidate.storeIncarnation current of
        Nothing ->
          Right (Map.insert candidate.storeIncarnation candidate current)
        Just incumbent
          | incumbent == candidate -> Right current
          | otherwise ->
              Left
                ( AlignmentDestinationStoreConflict
                    candidate.storeIncarnation
                    incumbent
                    candidate
                )

putPublicationId :: PublicationId -> Serialize.Put
putPublicationId identifier = do
  Serialize.putByteString (nablaIdBytes (publicationNabla identifier))
  putSizedBytes (authorityEpochCanonicalBytes (publicationAuthorityEpoch identifier))
  Serialize.putByteString
    (heraldEpochBytes (publicationSourceHeraldEpoch identifier))
  Serialize.putWord64be
    (nablaSequenceWord64 (publicationNablaSequence identifier))

putStructuralStamp :: StructuralOccurrenceStamp -> Serialize.Put
putStructuralStamp stamp = do
  let occurrence = structuralOccurrenceStampOccurrence stamp
  Serialize.putByteString
    (heraldEpochBytes (structuralOccurrenceSourceHeraldEpoch occurrence))
  Serialize.putWord64be
    ( structuralSequenceWord64
        (structuralOccurrenceSourceSequence occurrence)
    )
  let entries = structuralVersionVectorEntries (structuralOccurrenceStampPredecessor stamp)
  Serialize.putWord64be (fromIntegral (length entries))
  mapM_ putStructuralEntry entries
  putPublicationId (structuralOccurrenceStampPublication stamp)
  Serialize.putByteString
    ( structuralPublicationDigestBytes
        (structuralOccurrenceStampPublicationDigest stamp)
    )
  Serialize.putWord8
    (structuralCarrierRoleTag (structuralOccurrenceStampCarrierRole stamp))

putStructuralEntry :: (HeraldEpoch, StructuralPrefix) -> Serialize.Put
putStructuralEntry (herald, prefix) = do
  Serialize.putByteString (heraldEpochBytes herald)
  case structuralPrefixSequence prefix of
    Nothing -> Serialize.putWord8 0
    Just sequenceNumber -> do
      Serialize.putWord8 1
      Serialize.putWord64be (structuralSequenceWord64 sequenceNumber)

replicaStrengthTag :: ReplicaStrength -> Word8
replicaStrengthTag Weak = 0
replicaStrengthTag Normal = 1

putSizedBytes :: ByteString -> Serialize.Put
putSizedBytes bytes = do
  Serialize.putWord64be (fromIntegral (ByteString.length bytes))
  Serialize.putByteString bytes
