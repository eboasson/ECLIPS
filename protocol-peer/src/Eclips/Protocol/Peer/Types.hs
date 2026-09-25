{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Structurally checked Herald-peer claims and the complete current wire DTO
-- vocabulary.
--
-- These values establish wire shape only.  Membership, candidate phase,
-- placement authority, stream history, and publication-digest admission remain
-- responsibilities of the Herald peer RPC adapter and pure owners.
module Eclips.Protocol.Peer.Types
  ( PeerClaimError (..),
    SystemClaim,
    systemClaim,
    systemClaimBytes,
    HeraldIdClaim,
    heraldIdClaim,
    heraldIdClaimBytes,
    HeraldEpochClaim,
    heraldEpochClaim,
    heraldEpochClaimBytes,
    GlobalObjectClaim,
    globalObjectClaim,
    globalObjectClaimBytes,
    LabelDecisionClaim,
    labelDecisionClaim,
    labelDecisionClaimBytes,
    LabelOutcomeDigestClaim,
    labelOutcomeDigestClaim,
    labelOutcomeDigestClaimBytes,
    ProcessEpochClaim,
    processEpochClaim,
    processEpochClaimBytes,
    NablaClaim,
    nablaClaim,
    nablaClaimBytes,
    DeltaClaim,
    deltaClaim,
    deltaClaimBytes,
    SortOccurrenceClaim,
    sortOccurrenceClaim,
    sortOccurrenceClaimBytes,
    StoreIncarnationClaim,
    storeIncarnationClaim,
    storeIncarnationClaimBytes,
    CatalogueDigestClaim,
    catalogueDigestClaim,
    catalogueDigestClaimBytes,
    InitialProjectionDigestClaim,
    initialProjectionDigestClaim,
    initialProjectionDigestClaimBytes,
    PeerItemDigestClaim,
    peerItemDigestClaim,
    peerItemDigestClaimBytes,
    HeraldMembershipGenerationClaim,
    heraldMembershipGenerationClaim,
    heraldMembershipGenerationClaimBytes,
    OracleVoterConfigurationClaim,
    oracleVoterConfigurationClaim,
    oracleVoterConfigurationClaimBytes,
    HeraldFailureProbeDigestClaim,
    heraldFailureProbeDigestClaim,
    heraldFailureProbeDigestClaimBytes,
    MemberSetDigestClaim,
    memberSetDigestClaim,
    memberSetDigestClaimBytes,
    StructuralPublicationDigestClaim,
    structuralPublicationDigestClaim,
    structuralPublicationDigestClaimBytes,
    TopologyOccurrenceDigestClaim,
    topologyOccurrenceDigestClaim,
    topologyOccurrenceDigestClaimBytes,
    TopologyCutClaim,
    topologyCutClaim,
    topologyCutClaimBytes,
    TerminalSourceUnionDigestClaim,
    terminalSourceUnionDigestClaim,
    terminalSourceUnionDigestClaimBytes,
    TerminalSourceInventoryDigestClaim,
    terminalSourceInventoryDigestClaim,
    terminalSourceInventoryDigestClaimBytes,
    TerminalSourceAcceptanceDigestClaim,
    terminalSourceAcceptanceDigestClaim,
    terminalSourceAcceptanceDigestClaimBytes,
    ContextClassGenerationClaim,
    contextClassGenerationClaim,
    contextClassGenerationClaimBytes,
    MemberReadyEvidenceDigestClaim,
    memberReadyEvidenceDigestClaim,
    memberReadyEvidenceDigestClaimBytes,
    HistoricalCertificateDigestClaim,
    historicalCertificateDigestClaim,
    historicalCertificateDigestClaimBytes,
    AlignmentSnapshotDigestClaim,
    alignmentSnapshotDigestClaim,
    alignmentSnapshotDigestClaimBytes,
    BootstrapEvidenceDigestClaim,
    bootstrapEvidenceDigestClaim,
    bootstrapEvidenceDigestClaimBytes,
    PeerShapeError (..),
    ConnectionNonceDto,
    connectionNonceDto,
    connectionNonceDtoWord64,
    PeerHeartbeatNonce,
    peerHeartbeatNonce,
    peerHeartbeatNonceWord64,
    ControlIndexDto,
    controlIndexDto,
    controlIndexDtoWord64,
    PositiveStructuralSequenceDto,
    positiveStructuralSequenceDto,
    positiveStructuralSequenceDtoWord64,
    StructuralPrefixDto (..),
    StructuralVectorEntryDto (..),
    structuralVectorEntryHerald,
    StructuralVersionVectorDto,
    structuralVersionVectorDto,
    structuralVersionVectorMembershipGeneration,
    structuralVersionVectorMemberSetDigest,
    structuralVersionVectorEntries,
    StructuralOccurrenceIdDto (..),
    StructuralCarrierRoleDto (..),
    AuthorityEpochDto (..),
    NablaSequenceDto,
    nablaSequenceDto,
    nablaSequenceDtoWord64,
    PositivePlacementSequenceDto,
    positivePlacementSequenceDto,
    positivePlacementSequenceDtoWord64,
    PositiveStreamSequenceDto,
    positiveStreamSequenceDto,
    positiveStreamSequenceDtoWord64,
    PositiveAlignmentObligationSequenceDto,
    positiveAlignmentObligationSequenceDto,
    positiveAlignmentObligationSequenceDtoWord64,
    PositiveAlignmentSubscriptionSequenceDto,
    positiveAlignmentSubscriptionSequenceDto,
    positiveAlignmentSubscriptionSequenceDtoWord64,
    PositiveAlignmentEvidenceSequenceDto,
    positiveAlignmentEvidenceSequenceDto,
    positiveAlignmentEvidenceSequenceDtoWord64,
    PositiveAlignmentDeliverySequenceDto,
    positiveAlignmentDeliverySequenceDto,
    positiveAlignmentDeliverySequenceDtoWord64,
    AlignmentRetainedEvidenceDto (..),
    StoreRevisionDto,
    storeRevisionDto,
    storeRevisionDtoWord64,
    PositiveHeraldPublicationPositionDto,
    positiveHeraldPublicationPositionDto,
    positiveHeraldPublicationPositionDtoWord64,
    HeraldPublicationPrefixDto (..),
    PeerAddressDto,
    peerAddressDto,
    peerAddressDtoText,
    PeerHelloDto,
    peerHelloDto,
    peerHelloSystem,
    peerHelloHeraldId,
    peerHelloHeraldEpoch,
    peerHelloConnectionNonce,
    peerHelloAdvertisedAddresses,
    peerHelloAppliedControlIndex,
    peerHelloMembershipGeneration,
    peerHelloActiveMemberSetDigest,
    peerHelloCatalogueDigest,
    peerHelloInitialProjectionDigest,
    peerHelloApplicationLocator,
    KnownHeraldDto,
    knownHeraldDto,
    knownHeraldId,
    knownHeraldEpoch,
    knownHeraldAddresses,
    KnownHeraldCollectionDto,
    knownHeraldCollectionDto,
    knownHeraldCollectionEntries,
    PlacementUpdateDto (..),
    PlacementSnapshotDto,
    placementSnapshotDto,
    placementSnapshotOwner,
    placementSnapshotSequence,
    placementSnapshotRoutes,
    PlacementAcknowledgementDto (..),
    PredefinedSortRoleDto (..),
    DeltaRouteDto (..),
    deltaRouteDelta,
    StreamPrefixDto (..),
    StreamDirectionDto,
    streamDirectionDto,
    streamDirectionSource,
    streamDirectionDestination,
    GapSummaryDto,
    gapSummaryDto,
    gapSummaryDirection,
    gapSummaryReceivedPrefix,
    gapSummaryEntries,
    ResumeOfferDto,
    resumeOfferDto,
    resumeOfferWithCompletionDto,
    resumeOfferCompletion,
    resumeOfferSource,
    resumeOfferDestination,
    resumeOfferNextSourceSequence,
    resumeOfferReceivedPrefix,
    resumeOfferCompletedPrefix,
    resumeOfferGapSummary,
    ResumeResponseDto,
    resumeResponseDto,
    resumeResponseWithCompletionDto,
    resumeResponseCompletion,
    resumeResponseReceivedPrefix,
    resumeResponseCompletedPrefix,
    resumeResponseRetransmitFrom,
    PublicationIdDto (..),
    ReplicaStrengthDto (..),
    PublicationDestinationDto (..),
    publicationDestinationDelta,
    PublicationDestinationsDto,
    publicationDestinationsDto,
    publicationDestinationEntries,
    PublicationBatchDto (..),
    StructuralOccurrenceStampDto (..),
    PeerPublicationItemDto (..),
    DisappearanceProbeDigestClaim,
    disappearanceProbeDigestClaim,
    disappearanceProbeDigestClaimBytes,
    DisappearanceSubjectDigestClaim,
    disappearanceSubjectDigestClaim,
    disappearanceSubjectDigestClaimBytes,
    DisappearanceProbeIdDto,
    disappearanceProbeIdDto,
    disappearanceProbeIdDigest,
    disappearanceProbeIdControlIndex,
    DisappearanceCoordinateDto (..),
    DisappearanceProbeMarkerDto (..),
    TopologyFrontierDto (..),
    TopologyPredecessorDto (..),
    TopologyCutDto (..),
    StructuralAppliedReportDto (..),
    TopologyCutAnnounceDto (..),
    TopologyCutAcceptanceDto (..),
    topologyCutAcceptanceReporter,
    TopologyCutAcceptancesDto,
    topologyCutAcceptancesDto,
    topologyCutAcceptanceEntries,
    TopologyCutEstablishedDto (..),
    TopologyCutEstablishedAckDto (..),
    HeraldFailureProbeIdDto,
    heraldFailureProbeIdDto,
    heraldFailureProbeIdDigest,
    heraldFailureProbeIdControlIndex,
    DirectFailureProbeRequestDto (..),
    DirectFailureProbeResultDto (..),
    DirectFailureProbeResponseDto (..),
    TerminalSourceInventoryDto (..),
    TerminalSourcePayloadRequestDto (..),
    TerminalSourcePayloadRelayDto (..),
    TerminalSourceUnionDto (..),
    RetiredHeraldsDto,
    retiredHeraldsDto,
    retiredHeraldEntries,
    TerminalSourceUnionAnnounceDto (..),
    TerminalSourceUnionAcceptanceDto (..),
    terminalSourceUnionAcceptanceReporter,
    TerminalSourceUnionAcceptancesDto,
    terminalSourceUnionAcceptancesDto,
    terminalSourceUnionAcceptanceEntries,
    TerminalSourceUnionEstablishedDto (..),
    AlignmentObligationIdDto (..),
    AlignmentSubscriptionIdDto (..),
    PhysicalPlacementRevisionEntryDto (..),
    physicalPlacementRevisionEntryHerald,
    PhysicalPlacementRevisionVectorDto,
    physicalPlacementRevisionVectorDto,
    physicalPlacementRevisionVectorMembershipGeneration,
    physicalPlacementRevisionVectorMemberSetDigest,
    physicalPlacementRevisionVectorEntries,
    AlignmentMemberDto (..),
    FreshMemberBaseEvidenceDto (..),
    AlignmentCutDto (..),
    AlignmentPlanIdDto (..),
    AlignmentBindingDispositionDto (..),
    AlignmentPredecessorStatusDto (..),
    AlignmentPlanBindingClaimDto (..),
    AlignmentPlanRelationClaimDto (..),
    AlignmentPlanAnnounceDto (..),
    AlignmentPlanAcceptedDto (..),
    AlignmentLostStoreDto (..),
    AlignmentPlanObsoleteDto (..),
    AlignmentCutAnnounceDto (..),
    AlignmentCutAcceptedDto (..),
    ClassMemberReadyDto (..),
    HistoricalCertificateDto (..),
    DestinationStoreDto (..),
    StructuralConsequenceCauseDto (..),
    AlignmentObligationDto (..),
    AlignmentSourceProofDto (..),
    AlignmentSubscribeDto (..),
    RetainedObservationOriginDto (..),
    RetainedPublicationEvidenceDto (..),
    RetainedStateEvidenceDto (..),
    AlignmentSnapshotStartDto (..),
    AlignmentSnapshotChunkDto (..),
    AlignmentSnapshotEndDto (..),
    AlignmentChangeDto (..),
    AlignmentLiveDto (..),
    AlignmentAckDto (..),
    AlignmentCancelReasonDto (..),
    AlignmentCancelDto (..),
    AlignmentRouteCutoverMarkerDto (..),
    AlignmentControlDto (..),
    PeerHeartbeatDto (..),
    PreparationControlDto (..),
    RemotePreparationStatusDto (..),
    RemoteChildDto (..),
    LabelInstallationReportDto,
    labelInstallationReportDto,
    labelInstallationDecisionId,
    labelInstallationReporter,
    labelInstallationControlIndex,
    labelInstallationOutcomeDigest,
    PeerControlDto (..),
    PeerStreamItemDto (..),
    PeerEnvelope (..),
  )
where

import Control.Monad (foldM)
import Data.Binary (Binary (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle (ChildPreparation, ConnectionDescriptor, HeraldLocator, StartupError)
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import Eclips.Public.Types.ReceiptRetirement
import Eclips.Public.Types.SortId (SortId)
import GHC.Generics (Generic)

claimByteCount :: Int
claimByteCount = 32

-- | A malformed nominal 256-bit peer-protocol claim.
data PeerClaimError
  = PeerClaimWrongByteCount
  { expectedPeerClaimByteCount :: Int,
    actualPeerClaimByteCount :: Int
  }
  deriving stock (Eq, Show)

-- A nominal role parameter keeps claims with identical representations from
-- being coercible between semantic namespaces.
newtype Claim domain = Claim ByteString
  deriving stock (Eq, Ord)

instance Show (Claim domain) where
  show = renderGroupedHex . claimBytes

type role Claim nominal

data SystemClaimDomain

type SystemClaim = Claim SystemClaimDomain

data HeraldIdClaimDomain

type HeraldIdClaim = Claim HeraldIdClaimDomain

data HeraldEpochClaimDomain

type HeraldEpochClaim = Claim HeraldEpochClaimDomain

data GlobalObjectClaimDomain

type GlobalObjectClaim = Claim GlobalObjectClaimDomain

data LabelDecisionClaimDomain

type LabelDecisionClaim = Claim LabelDecisionClaimDomain

data LabelOutcomeDigestClaimDomain

type LabelOutcomeDigestClaim = Claim LabelOutcomeDigestClaimDomain

data DisappearanceProbeDigestClaimDomain

type DisappearanceProbeDigestClaim = Claim DisappearanceProbeDigestClaimDomain

data DisappearanceSubjectDigestClaimDomain

type DisappearanceSubjectDigestClaim = Claim DisappearanceSubjectDigestClaimDomain

data ProcessEpochClaimDomain

type ProcessEpochClaim = Claim ProcessEpochClaimDomain

data NablaClaimDomain

type NablaClaim = Claim NablaClaimDomain

data DeltaClaimDomain

type DeltaClaim = Claim DeltaClaimDomain

data SortOccurrenceClaimDomain

type SortOccurrenceClaim = Claim SortOccurrenceClaimDomain

data StoreIncarnationClaimDomain

type StoreIncarnationClaim = Claim StoreIncarnationClaimDomain

data CatalogueDigestClaimDomain

type CatalogueDigestClaim = Claim CatalogueDigestClaimDomain

data InitialProjectionDigestClaimDomain

type InitialProjectionDigestClaim = Claim InitialProjectionDigestClaimDomain

data PeerItemDigestClaimDomain

type PeerItemDigestClaim = Claim PeerItemDigestClaimDomain

data HeraldMembershipGenerationClaimDomain

type HeraldMembershipGenerationClaim = Claim HeraldMembershipGenerationClaimDomain

data OracleVoterConfigurationClaimDomain

type OracleVoterConfigurationClaim = Claim OracleVoterConfigurationClaimDomain

data HeraldFailureProbeDigestClaimDomain

type HeraldFailureProbeDigestClaim = Claim HeraldFailureProbeDigestClaimDomain

data MemberSetDigestClaimDomain

type MemberSetDigestClaim = Claim MemberSetDigestClaimDomain

data StructuralPublicationDigestClaimDomain

type StructuralPublicationDigestClaim = Claim StructuralPublicationDigestClaimDomain

data TopologyOccurrenceDigestClaimDomain

type TopologyOccurrenceDigestClaim = Claim TopologyOccurrenceDigestClaimDomain

data TopologyCutClaimDomain

type TopologyCutClaim = Claim TopologyCutClaimDomain

data TerminalSourceUnionDigestClaimDomain

type TerminalSourceUnionDigestClaim = Claim TerminalSourceUnionDigestClaimDomain

data TerminalSourceInventoryDigestClaimDomain

type TerminalSourceInventoryDigestClaim = Claim TerminalSourceInventoryDigestClaimDomain

data TerminalSourceAcceptanceDigestClaimDomain

type TerminalSourceAcceptanceDigestClaim = Claim TerminalSourceAcceptanceDigestClaimDomain

data ContextClassGenerationClaimDomain

type ContextClassGenerationClaim = Claim ContextClassGenerationClaimDomain

data MemberReadyEvidenceDigestClaimDomain

type MemberReadyEvidenceDigestClaim = Claim MemberReadyEvidenceDigestClaimDomain

data HistoricalCertificateDigestClaimDomain

type HistoricalCertificateDigestClaim = Claim HistoricalCertificateDigestClaimDomain

data AlignmentSnapshotDigestClaimDomain

type AlignmentSnapshotDigestClaim = Claim AlignmentSnapshotDigestClaimDomain

data BootstrapEvidenceDigestClaimDomain

type BootstrapEvidenceDigestClaim = Claim BootstrapEvidenceDigestClaimDomain

checkedClaim :: ByteString -> Either PeerClaimError (Claim domain)
checkedClaim bytes
  | ByteString.length bytes == claimByteCount = Right (Claim bytes)
  | otherwise =
      Left
        PeerClaimWrongByteCount
          { expectedPeerClaimByteCount = claimByteCount,
            actualPeerClaimByteCount = ByteString.length bytes
          }

claimBytes :: Claim domain -> ByteString
claimBytes (Claim bytes) = bytes

instance Binary (Claim domain) where
  put = put . claimBytes
  get = get >>= either (fail . show) pure . checkedClaim

systemClaim :: ByteString -> Either PeerClaimError SystemClaim
systemClaim = checkedClaim

systemClaimBytes :: SystemClaim -> ByteString
systemClaimBytes = claimBytes

heraldIdClaim :: ByteString -> Either PeerClaimError HeraldIdClaim
heraldIdClaim = checkedClaim

heraldIdClaimBytes :: HeraldIdClaim -> ByteString
heraldIdClaimBytes = claimBytes

heraldEpochClaim :: ByteString -> Either PeerClaimError HeraldEpochClaim
heraldEpochClaim = checkedClaim

heraldEpochClaimBytes :: HeraldEpochClaim -> ByteString
heraldEpochClaimBytes = claimBytes

globalObjectClaim :: ByteString -> Either PeerClaimError GlobalObjectClaim
globalObjectClaim = checkedClaim

globalObjectClaimBytes :: GlobalObjectClaim -> ByteString
globalObjectClaimBytes = claimBytes

labelDecisionClaim :: ByteString -> Either PeerClaimError LabelDecisionClaim
labelDecisionClaim = checkedClaim

labelDecisionClaimBytes :: LabelDecisionClaim -> ByteString
labelDecisionClaimBytes = claimBytes

labelOutcomeDigestClaim :: ByteString -> Either PeerClaimError LabelOutcomeDigestClaim
labelOutcomeDigestClaim = checkedClaim

labelOutcomeDigestClaimBytes :: LabelOutcomeDigestClaim -> ByteString
labelOutcomeDigestClaimBytes = claimBytes

disappearanceProbeDigestClaim :: ByteString -> Either PeerClaimError DisappearanceProbeDigestClaim
disappearanceProbeDigestClaim = checkedClaim

disappearanceProbeDigestClaimBytes :: DisappearanceProbeDigestClaim -> ByteString
disappearanceProbeDigestClaimBytes = claimBytes

disappearanceSubjectDigestClaim :: ByteString -> Either PeerClaimError DisappearanceSubjectDigestClaim
disappearanceSubjectDigestClaim = checkedClaim

disappearanceSubjectDigestClaimBytes :: DisappearanceSubjectDigestClaim -> ByteString
disappearanceSubjectDigestClaimBytes = claimBytes

processEpochClaim :: ByteString -> Either PeerClaimError ProcessEpochClaim
processEpochClaim = checkedClaim

processEpochClaimBytes :: ProcessEpochClaim -> ByteString
processEpochClaimBytes = claimBytes

nablaClaim :: ByteString -> Either PeerClaimError NablaClaim
nablaClaim = checkedClaim

nablaClaimBytes :: NablaClaim -> ByteString
nablaClaimBytes = claimBytes

deltaClaim :: ByteString -> Either PeerClaimError DeltaClaim
deltaClaim = checkedClaim

deltaClaimBytes :: DeltaClaim -> ByteString
deltaClaimBytes = claimBytes

sortOccurrenceClaim :: ByteString -> Either PeerClaimError SortOccurrenceClaim
sortOccurrenceClaim = checkedClaim

sortOccurrenceClaimBytes :: SortOccurrenceClaim -> ByteString
sortOccurrenceClaimBytes = claimBytes

storeIncarnationClaim :: ByteString -> Either PeerClaimError StoreIncarnationClaim
storeIncarnationClaim = checkedClaim

storeIncarnationClaimBytes :: StoreIncarnationClaim -> ByteString
storeIncarnationClaimBytes = claimBytes

catalogueDigestClaim :: ByteString -> Either PeerClaimError CatalogueDigestClaim
catalogueDigestClaim = checkedClaim

catalogueDigestClaimBytes :: CatalogueDigestClaim -> ByteString
catalogueDigestClaimBytes = claimBytes

initialProjectionDigestClaim :: ByteString -> Either PeerClaimError InitialProjectionDigestClaim
initialProjectionDigestClaim = checkedClaim

initialProjectionDigestClaimBytes :: InitialProjectionDigestClaim -> ByteString
initialProjectionDigestClaimBytes = claimBytes

peerItemDigestClaim :: ByteString -> Either PeerClaimError PeerItemDigestClaim
peerItemDigestClaim = checkedClaim

peerItemDigestClaimBytes :: PeerItemDigestClaim -> ByteString
peerItemDigestClaimBytes = claimBytes

heraldMembershipGenerationClaim ::
  ByteString -> Either PeerClaimError HeraldMembershipGenerationClaim
heraldMembershipGenerationClaim = checkedClaim

heraldMembershipGenerationClaimBytes ::
  HeraldMembershipGenerationClaim -> ByteString
heraldMembershipGenerationClaimBytes = claimBytes

oracleVoterConfigurationClaim :: ByteString -> Either PeerClaimError OracleVoterConfigurationClaim
oracleVoterConfigurationClaim = checkedClaim

oracleVoterConfigurationClaimBytes :: OracleVoterConfigurationClaim -> ByteString
oracleVoterConfigurationClaimBytes = claimBytes

heraldFailureProbeDigestClaim ::
  ByteString -> Either PeerClaimError HeraldFailureProbeDigestClaim
heraldFailureProbeDigestClaim = checkedClaim

heraldFailureProbeDigestClaimBytes ::
  HeraldFailureProbeDigestClaim -> ByteString
heraldFailureProbeDigestClaimBytes = claimBytes

memberSetDigestClaim :: ByteString -> Either PeerClaimError MemberSetDigestClaim
memberSetDigestClaim = checkedClaim

memberSetDigestClaimBytes :: MemberSetDigestClaim -> ByteString
memberSetDigestClaimBytes = claimBytes

structuralPublicationDigestClaim ::
  ByteString -> Either PeerClaimError StructuralPublicationDigestClaim
structuralPublicationDigestClaim = checkedClaim

structuralPublicationDigestClaimBytes ::
  StructuralPublicationDigestClaim -> ByteString
structuralPublicationDigestClaimBytes = claimBytes

topologyOccurrenceDigestClaim ::
  ByteString -> Either PeerClaimError TopologyOccurrenceDigestClaim
topologyOccurrenceDigestClaim = checkedClaim

topologyOccurrenceDigestClaimBytes ::
  TopologyOccurrenceDigestClaim -> ByteString
topologyOccurrenceDigestClaimBytes = claimBytes

topologyCutClaim :: ByteString -> Either PeerClaimError TopologyCutClaim
topologyCutClaim = checkedClaim

topologyCutClaimBytes :: TopologyCutClaim -> ByteString
topologyCutClaimBytes = claimBytes

terminalSourceUnionDigestClaim ::
  ByteString -> Either PeerClaimError TerminalSourceUnionDigestClaim
terminalSourceUnionDigestClaim = checkedClaim

terminalSourceUnionDigestClaimBytes ::
  TerminalSourceUnionDigestClaim -> ByteString
terminalSourceUnionDigestClaimBytes = claimBytes

terminalSourceInventoryDigestClaim ::
  ByteString -> Either PeerClaimError TerminalSourceInventoryDigestClaim
terminalSourceInventoryDigestClaim = checkedClaim

terminalSourceInventoryDigestClaimBytes ::
  TerminalSourceInventoryDigestClaim -> ByteString
terminalSourceInventoryDigestClaimBytes = claimBytes

terminalSourceAcceptanceDigestClaim ::
  ByteString -> Either PeerClaimError TerminalSourceAcceptanceDigestClaim
terminalSourceAcceptanceDigestClaim = checkedClaim

terminalSourceAcceptanceDigestClaimBytes ::
  TerminalSourceAcceptanceDigestClaim -> ByteString
terminalSourceAcceptanceDigestClaimBytes = claimBytes

contextClassGenerationClaim ::
  ByteString -> Either PeerClaimError ContextClassGenerationClaim
contextClassGenerationClaim = checkedClaim

contextClassGenerationClaimBytes :: ContextClassGenerationClaim -> ByteString
contextClassGenerationClaimBytes = claimBytes

memberReadyEvidenceDigestClaim ::
  ByteString -> Either PeerClaimError MemberReadyEvidenceDigestClaim
memberReadyEvidenceDigestClaim = checkedClaim

memberReadyEvidenceDigestClaimBytes ::
  MemberReadyEvidenceDigestClaim -> ByteString
memberReadyEvidenceDigestClaimBytes = claimBytes

historicalCertificateDigestClaim ::
  ByteString -> Either PeerClaimError HistoricalCertificateDigestClaim
historicalCertificateDigestClaim = checkedClaim

historicalCertificateDigestClaimBytes ::
  HistoricalCertificateDigestClaim -> ByteString
historicalCertificateDigestClaimBytes = claimBytes

alignmentSnapshotDigestClaim ::
  ByteString -> Either PeerClaimError AlignmentSnapshotDigestClaim
alignmentSnapshotDigestClaim = checkedClaim

alignmentSnapshotDigestClaimBytes :: AlignmentSnapshotDigestClaim -> ByteString
alignmentSnapshotDigestClaimBytes = claimBytes

bootstrapEvidenceDigestClaim ::
  ByteString -> Either PeerClaimError BootstrapEvidenceDigestClaim
bootstrapEvidenceDigestClaim = checkedClaim

bootstrapEvidenceDigestClaimBytes :: BootstrapEvidenceDigestClaim -> ByteString
bootstrapEvidenceDigestClaimBytes = claimBytes

-- | Structural failures in invariant-bearing peer DTOs.
data PeerShapeError
  = PlacementSequenceMustBePositive
  | StreamSequenceMustBePositive
  | StructuralSequenceMustBePositive
  | AlignmentObligationSequenceMustBePositive
  | AlignmentSubscriptionSequenceMustBePositive
  | AlignmentDeliverySequenceMustBePositive
  | AlignmentEvidenceSequenceMustBePositive
  | HeraldPublicationPositionMustBePositive
  | DisappearanceProbeControlIndexMustBePositive
  | LabelInstallationControlIndexMustBePositive
  | FailureProbeControlIndexMustBePositive
  | StructuralVectorEmpty
  | StructuralVectorDuplicateHerald HeraldEpochClaim
  | PhysicalPlacementRevisionVectorEmpty
  | PhysicalPlacementRevisionVectorDuplicateHerald HeraldEpochClaim
  | KnownHeraldConflictingDuplicate HeraldIdClaim HeraldEpochClaim
  | PlacementSnapshotDuplicateDelta DeltaClaim
  | StreamDirectionEndpointsEqual HeraldEpochClaim
  | GapEntriesNotStrictlyAscending
  | GapEntryNotBeyondFirstMissing PositiveStreamSequenceDto
  | InvalidStreamCompletionProgress
  | ResumeGapDirectionMismatch StreamDirectionDto StreamDirectionDto
  | ResumeGapPrefixMismatch StreamPrefixDto StreamPrefixDto
  | ResumeCompletedBeyondReceived StreamPrefixDto StreamPrefixDto
  | ResumeRetransmitFromMismatch PositiveStreamSequenceDto PositiveStreamSequenceDto
  | PublicationDestinationsEmpty
  | PublicationDestinationDuplicateDelta DeltaClaim
  | TopologyCutAcceptancesEmpty
  | TopologyCutAcceptanceDuplicateReporter HeraldEpochClaim
  | TerminalSourceUnionAcceptancesEmpty
  | TerminalSourceUnionAcceptanceDuplicateReporter HeraldEpochClaim
  | RetiredHeraldsEmpty
  | RetiredHeraldDuplicate HeraldEpochClaim
  deriving stock (Eq, Show)

newtype ConnectionNonceDto = ConnectionNonceDto Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

connectionNonceDto :: Word64 -> ConnectionNonceDto
connectionNonceDto = ConnectionNonceDto

connectionNonceDtoWord64 :: ConnectionNonceDto -> Word64
connectionNonceDtoWord64 (ConnectionNonceDto value) = value

-- | One lane-local EPRP heartbeat correlation.
--
-- It has no session, connection, or semantic identity. Zero is valid. Keeping
-- it nominally distinct prevents a correlation from crossing another protocol
-- family accidentally.
newtype PeerHeartbeatNonce = PeerHeartbeatNonce Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

-- | Construct a lane-local EPRP heartbeat nonce. Zero is valid.
peerHeartbeatNonce :: Word64 -> PeerHeartbeatNonce
peerHeartbeatNonce = PeerHeartbeatNonce

-- | Observe the heartbeat nonce's unsigned representation.
peerHeartbeatNonceWord64 :: PeerHeartbeatNonce -> Word64
peerHeartbeatNonceWord64 (PeerHeartbeatNonce value) = value

newtype ControlIndexDto = ControlIndexDto Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

controlIndexDto :: Word64 -> ControlIndexDto
controlIndexDto = ControlIndexDto

controlIndexDtoWord64 :: ControlIndexDto -> Word64
controlIndexDtoWord64 (ControlIndexDto value) = value

newtype PositiveStructuralSequenceDto = PositiveStructuralSequenceDto Word64
  deriving stock (Eq, Ord, Show)

positiveStructuralSequenceDto ::
  Word64 -> Either PeerShapeError PositiveStructuralSequenceDto
positiveStructuralSequenceDto 0 = Left StructuralSequenceMustBePositive
positiveStructuralSequenceDto value = Right (PositiveStructuralSequenceDto value)

positiveStructuralSequenceDtoWord64 :: PositiveStructuralSequenceDto -> Word64
positiveStructuralSequenceDtoWord64 (PositiveStructuralSequenceDto value) = value

instance Binary PositiveStructuralSequenceDto where
  put = put . positiveStructuralSequenceDtoWord64
  get = get >>= either (fail . show) pure . positiveStructuralSequenceDto

data StructuralPrefixDto
  = EmptyStructuralPrefixDto
  | StructuralPrefixThroughDto PositiveStructuralSequenceDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data StructuralVectorEntryDto
  = StructuralVectorEntryDto HeraldEpochClaim StructuralPrefixDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

structuralVectorEntryHerald :: StructuralVectorEntryDto -> HeraldEpochClaim
structuralVectorEntryHerald (StructuralVectorEntryDto herald _) = herald

data StructuralVersionVectorDto
  = StructuralVersionVectorDto
      HeraldMembershipGenerationClaim
      MemberSetDigestClaim
      (NonEmpty StructuralVectorEntryDto)
  deriving stock (Eq, Show)

structuralVersionVectorDto ::
  HeraldMembershipGenerationClaim ->
  MemberSetDigestClaim ->
  [StructuralVectorEntryDto] ->
  Either PeerShapeError StructuralVersionVectorDto
structuralVersionVectorDto generation members supplied = do
  entries <- case supplied of
    [] -> Left StructuralVectorEmpty
    _ ->
      normalizeUnique
        StructuralVectorDuplicateHerald
        structuralVectorEntryHerald
        supplied
  case entries of
    first : remaining ->
      Right
        ( StructuralVersionVectorDto
            generation
            members
            (first :| remaining)
        )
    [] -> Left StructuralVectorEmpty

structuralVersionVectorMembershipGeneration ::
  StructuralVersionVectorDto -> HeraldMembershipGenerationClaim
structuralVersionVectorMembershipGeneration
  (StructuralVersionVectorDto generation _ _) = generation

structuralVersionVectorMemberSetDigest ::
  StructuralVersionVectorDto -> MemberSetDigestClaim
structuralVersionVectorMemberSetDigest
  (StructuralVersionVectorDto _ members _) = members

structuralVersionVectorEntries ::
  StructuralVersionVectorDto -> NonEmpty StructuralVectorEntryDto
structuralVersionVectorEntries (StructuralVersionVectorDto _ _ entries) = entries

instance Binary StructuralVersionVectorDto where
  put (StructuralVersionVectorDto generation members entries) = do
    put generation
    put members
    put entries
  get = do
    generation <- get
    members <- get
    entries <- get
    either
      (fail . show)
      pure
      ( structuralVersionVectorDto
          generation
          members
          (NonEmpty.toList entries)
      )

data StructuralOccurrenceIdDto
  = StructuralOccurrenceIdDto HeraldEpochClaim PositiveStructuralSequenceDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | The Step-13 structural peer arm has exactly these four carrier roles.
-- Process epochs remain ordinary and have no dormant structural tag.
data StructuralCarrierRoleDto
  = NeutralVertexCarrierDto
  | EdgeCarrierDto
  | NablaCarrierDto
  | DeltaCarrierDto
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Binary)

data AuthorityEpochDto
  = GenesisAuthorityEpochDto
  | StructuralAuthorityEpochDto StructuralOccurrenceIdDto TopologyCutClaim
  | LabelAuthorityEpochDto ControlIndexDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

newtype NablaSequenceDto = NablaSequenceDto Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

nablaSequenceDto :: Word64 -> NablaSequenceDto
nablaSequenceDto = NablaSequenceDto

nablaSequenceDtoWord64 :: NablaSequenceDto -> Word64
nablaSequenceDtoWord64 (NablaSequenceDto value) = value

newtype PositivePlacementSequenceDto = PositivePlacementSequenceDto Word64
  deriving stock (Eq, Ord, Show)

positivePlacementSequenceDto :: Word64 -> Either PeerShapeError PositivePlacementSequenceDto
positivePlacementSequenceDto 0 = Left PlacementSequenceMustBePositive
positivePlacementSequenceDto value = Right (PositivePlacementSequenceDto value)

positivePlacementSequenceDtoWord64 :: PositivePlacementSequenceDto -> Word64
positivePlacementSequenceDtoWord64 (PositivePlacementSequenceDto value) = value

instance Binary PositivePlacementSequenceDto where
  put = put . positivePlacementSequenceDtoWord64
  get = get >>= either (fail . show) pure . positivePlacementSequenceDto

newtype PositiveStreamSequenceDto = PositiveStreamSequenceDto Word64
  deriving stock (Eq, Ord, Show)

positiveStreamSequenceDto :: Word64 -> Either PeerShapeError PositiveStreamSequenceDto
positiveStreamSequenceDto 0 = Left StreamSequenceMustBePositive
positiveStreamSequenceDto value = Right (PositiveStreamSequenceDto value)

positiveStreamSequenceDtoWord64 :: PositiveStreamSequenceDto -> Word64
positiveStreamSequenceDtoWord64 (PositiveStreamSequenceDto value) = value

instance Binary PositiveStreamSequenceDto where
  put = put . positiveStreamSequenceDtoWord64
  get = get >>= either (fail . show) pure . positiveStreamSequenceDto

newtype PositiveAlignmentObligationSequenceDto
  = PositiveAlignmentObligationSequenceDto Word64
  deriving stock (Eq, Ord, Show)

positiveAlignmentObligationSequenceDto ::
  Word64 -> Either PeerShapeError PositiveAlignmentObligationSequenceDto
positiveAlignmentObligationSequenceDto 0 =
  Left AlignmentObligationSequenceMustBePositive
positiveAlignmentObligationSequenceDto value =
  Right (PositiveAlignmentObligationSequenceDto value)

positiveAlignmentObligationSequenceDtoWord64 ::
  PositiveAlignmentObligationSequenceDto -> Word64
positiveAlignmentObligationSequenceDtoWord64
  (PositiveAlignmentObligationSequenceDto value) = value

instance Binary PositiveAlignmentObligationSequenceDto where
  put = put . positiveAlignmentObligationSequenceDtoWord64
  get =
    get
      >>= either (fail . show) pure
        . positiveAlignmentObligationSequenceDto

newtype PositiveAlignmentSubscriptionSequenceDto
  = PositiveAlignmentSubscriptionSequenceDto Word64
  deriving stock (Eq, Ord, Show)

positiveAlignmentSubscriptionSequenceDto ::
  Word64 -> Either PeerShapeError PositiveAlignmentSubscriptionSequenceDto
positiveAlignmentSubscriptionSequenceDto 0 =
  Left AlignmentSubscriptionSequenceMustBePositive
positiveAlignmentSubscriptionSequenceDto value =
  Right (PositiveAlignmentSubscriptionSequenceDto value)

positiveAlignmentSubscriptionSequenceDtoWord64 ::
  PositiveAlignmentSubscriptionSequenceDto -> Word64
positiveAlignmentSubscriptionSequenceDtoWord64
  (PositiveAlignmentSubscriptionSequenceDto value) = value

instance Binary PositiveAlignmentSubscriptionSequenceDto where
  put = put . positiveAlignmentSubscriptionSequenceDtoWord64
  get =
    get
      >>= either (fail . show) pure
        . positiveAlignmentSubscriptionSequenceDto

newtype PositiveAlignmentEvidenceSequenceDto
  = PositiveAlignmentEvidenceSequenceDto Word64
  deriving stock (Eq, Ord, Show)

positiveAlignmentEvidenceSequenceDto ::
  Word64 -> Either PeerShapeError PositiveAlignmentEvidenceSequenceDto
positiveAlignmentEvidenceSequenceDto 0 =
  Left AlignmentEvidenceSequenceMustBePositive
positiveAlignmentEvidenceSequenceDto value =
  Right (PositiveAlignmentEvidenceSequenceDto value)

positiveAlignmentEvidenceSequenceDtoWord64 ::
  PositiveAlignmentEvidenceSequenceDto -> Word64
positiveAlignmentEvidenceSequenceDtoWord64
  (PositiveAlignmentEvidenceSequenceDto value) = value

instance Binary PositiveAlignmentEvidenceSequenceDto where
  put = put . positiveAlignmentEvidenceSequenceDtoWord64
  get =
    get
      >>= either (fail . show) pure
        . positiveAlignmentEvidenceSequenceDto

newtype PositiveAlignmentDeliverySequenceDto
  = PositiveAlignmentDeliverySequenceDto Word64
  deriving stock (Eq, Ord, Show)

positiveAlignmentDeliverySequenceDto :: Word64 -> Either PeerShapeError PositiveAlignmentDeliverySequenceDto
positiveAlignmentDeliverySequenceDto 0 = Left AlignmentDeliverySequenceMustBePositive
positiveAlignmentDeliverySequenceDto sequenceNumber = Right (PositiveAlignmentDeliverySequenceDto sequenceNumber)

positiveAlignmentDeliverySequenceDtoWord64 :: PositiveAlignmentDeliverySequenceDto -> Word64
positiveAlignmentDeliverySequenceDtoWord64 (PositiveAlignmentDeliverySequenceDto sequenceNumber) = sequenceNumber

instance Binary PositiveAlignmentDeliverySequenceDto where
  put = put . positiveAlignmentDeliverySequenceDtoWord64
  get = get >>= either (fail . show) pure . positiveAlignmentDeliverySequenceDto

newtype StoreRevisionDto = StoreRevisionDto Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

storeRevisionDto :: Word64 -> StoreRevisionDto
storeRevisionDto = StoreRevisionDto

storeRevisionDtoWord64 :: StoreRevisionDto -> Word64
storeRevisionDtoWord64 (StoreRevisionDto value) = value

newtype PositiveHeraldPublicationPositionDto
  = PositiveHeraldPublicationPositionDto Word64
  deriving stock (Eq, Ord, Show)

positiveHeraldPublicationPositionDto ::
  Word64 -> Either PeerShapeError PositiveHeraldPublicationPositionDto
positiveHeraldPublicationPositionDto 0 =
  Left HeraldPublicationPositionMustBePositive
positiveHeraldPublicationPositionDto value =
  Right (PositiveHeraldPublicationPositionDto value)

positiveHeraldPublicationPositionDtoWord64 ::
  PositiveHeraldPublicationPositionDto -> Word64
positiveHeraldPublicationPositionDtoWord64
  (PositiveHeraldPublicationPositionDto value) = value

instance Binary PositiveHeraldPublicationPositionDto where
  put = put . positiveHeraldPublicationPositionDtoWord64
  get =
    get
      >>= either (fail . show) pure
        . positiveHeraldPublicationPositionDto

data HeraldPublicationPrefixDto
  = EmptyHeraldPublicationPrefixDto
  | HeraldPublicationPrefixThroughDto PositiveHeraldPublicationPositionDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | A Unicode address hint.  The 'Binary' instance for 'Text' performs UTF-8
-- validation while decoding.
newtype PeerAddressDto = PeerAddressDto Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (Binary)

peerAddressDto :: Text -> PeerAddressDto
peerAddressDto = PeerAddressDto

peerAddressDtoText :: PeerAddressDto -> Text
peerAddressDtoText (PeerAddressDto address) = address

data PeerHelloDto
  = PeerHelloDto
      SystemClaim
      HeraldIdClaim
      HeraldEpochClaim
      ConnectionNonceDto
      [PeerAddressDto]
      ControlIndexDto
      HeraldMembershipGenerationClaim
      MemberSetDigestClaim
      CatalogueDigestClaim
      InitialProjectionDigestClaim
      (Maybe HeraldLocator)
  deriving stock (Eq, Show)

peerHelloDto ::
  SystemClaim ->
  HeraldIdClaim ->
  HeraldEpochClaim ->
  ConnectionNonceDto ->
  [PeerAddressDto] ->
  ControlIndexDto ->
  HeraldMembershipGenerationClaim ->
  MemberSetDigestClaim ->
  CatalogueDigestClaim ->
  InitialProjectionDigestClaim ->
  Maybe HeraldLocator ->
  PeerHelloDto
peerHelloDto system heraldId epoch nonce addresses control generation members catalogue projection locator =
  PeerHelloDto
    system
    heraldId
    epoch
    nonce
    (normalizeAddresses addresses)
    control
    generation
    members
    catalogue
    projection
    locator

peerHelloSystem :: PeerHelloDto -> SystemClaim
peerHelloSystem (PeerHelloDto system _ _ _ _ _ _ _ _ _ _) = system

peerHelloHeraldId :: PeerHelloDto -> HeraldIdClaim
peerHelloHeraldId (PeerHelloDto _ heraldId _ _ _ _ _ _ _ _ _) = heraldId

peerHelloHeraldEpoch :: PeerHelloDto -> HeraldEpochClaim
peerHelloHeraldEpoch (PeerHelloDto _ _ epoch _ _ _ _ _ _ _ _) = epoch

peerHelloConnectionNonce :: PeerHelloDto -> ConnectionNonceDto
peerHelloConnectionNonce (PeerHelloDto _ _ _ nonce _ _ _ _ _ _ _) = nonce

peerHelloAdvertisedAddresses :: PeerHelloDto -> [PeerAddressDto]
peerHelloAdvertisedAddresses (PeerHelloDto _ _ _ _ addresses _ _ _ _ _ _) = addresses

peerHelloAppliedControlIndex :: PeerHelloDto -> ControlIndexDto
peerHelloAppliedControlIndex (PeerHelloDto _ _ _ _ _ control _ _ _ _ _) = control

peerHelloMembershipGeneration ::
  PeerHelloDto -> HeraldMembershipGenerationClaim
peerHelloMembershipGeneration
  (PeerHelloDto _ _ _ _ _ _ generation _ _ _ _) = generation

peerHelloActiveMemberSetDigest :: PeerHelloDto -> MemberSetDigestClaim
peerHelloActiveMemberSetDigest
  (PeerHelloDto _ _ _ _ _ _ _ members _ _ _) = members

peerHelloCatalogueDigest :: PeerHelloDto -> CatalogueDigestClaim
peerHelloCatalogueDigest (PeerHelloDto _ _ _ _ _ _ _ _ catalogue _ _) = catalogue

peerHelloInitialProjectionDigest :: PeerHelloDto -> InitialProjectionDigestClaim
peerHelloInitialProjectionDigest
  (PeerHelloDto _ _ _ _ _ _ _ _ _ projection _) = projection

peerHelloApplicationLocator :: PeerHelloDto -> Maybe HeraldLocator
peerHelloApplicationLocator (PeerHelloDto _ _ _ _ _ _ _ _ _ _ locator) = locator

instance Binary PeerHelloDto where
  put hello = do
    put (peerHelloSystem hello)
    put (peerHelloHeraldId hello)
    put (peerHelloHeraldEpoch hello)
    put (peerHelloConnectionNonce hello)
    put (peerHelloAdvertisedAddresses hello)
    put (peerHelloAppliedControlIndex hello)
    put (peerHelloMembershipGeneration hello)
    put (peerHelloActiveMemberSetDigest hello)
    put (peerHelloCatalogueDigest hello)
    put (peerHelloInitialProjectionDigest hello)
    put (peerHelloApplicationLocator hello)
  get =
    peerHelloDto
      <$> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> get

data KnownHeraldDto
  = KnownHeraldDto HeraldIdClaim HeraldEpochClaim [PeerAddressDto]
  deriving stock (Eq, Ord, Show)

knownHeraldDto :: HeraldIdClaim -> HeraldEpochClaim -> [PeerAddressDto] -> KnownHeraldDto
knownHeraldDto heraldId epoch addresses =
  KnownHeraldDto heraldId epoch (normalizeAddresses addresses)

knownHeraldId :: KnownHeraldDto -> HeraldIdClaim
knownHeraldId (KnownHeraldDto heraldId _ _) = heraldId

knownHeraldEpoch :: KnownHeraldDto -> HeraldEpochClaim
knownHeraldEpoch (KnownHeraldDto _ epoch _) = epoch

knownHeraldAddresses :: KnownHeraldDto -> [PeerAddressDto]
knownHeraldAddresses (KnownHeraldDto _ _ addresses) = addresses

instance Binary KnownHeraldDto where
  put known = do
    put (knownHeraldId known)
    put (knownHeraldEpoch known)
    put (knownHeraldAddresses known)
  get = knownHeraldDto <$> get <*> get <*> get

newtype KnownHeraldCollectionDto
  = KnownHeraldCollectionDto [KnownHeraldDto]
  deriving stock (Eq, Show)

knownHeraldCollectionDto :: [KnownHeraldDto] -> Either PeerShapeError KnownHeraldCollectionDto
knownHeraldCollectionDto supplied =
  KnownHeraldCollectionDto . Map.elems <$> foldM insertKnown Map.empty supplied
  where
    insertKnown ::
      Map (HeraldIdClaim, HeraldEpochClaim) KnownHeraldDto ->
      KnownHeraldDto ->
      Either PeerShapeError (Map (HeraldIdClaim, HeraldEpochClaim) KnownHeraldDto)
    insertKnown current candidate =
      case Map.lookup key current of
        Nothing -> Right (Map.insert key candidate current)
        Just incumbent
          | incumbent == candidate -> Right current
          | otherwise -> Left (KnownHeraldConflictingDuplicate (fst key) (snd key))
      where
        key = (knownHeraldId candidate, knownHeraldEpoch candidate)

knownHeraldCollectionEntries :: KnownHeraldCollectionDto -> [KnownHeraldDto]
knownHeraldCollectionEntries (KnownHeraldCollectionDto entries) = entries

instance Binary KnownHeraldCollectionDto where
  put = put . knownHeraldCollectionEntries
  get = get >>= either (fail . show) pure . knownHeraldCollectionDto

data PredefinedSortRoleDto
  = SortDefinitionRoleDto
  | NeutralVertexRoleDto
  | EdgeRoleDto
  | NablaRoleDto
  | DeltaRoleDto
  | ProcessEpochRoleDto
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Binary)

data DeltaRouteDto
  = ApplicationDeltaRouteDto
      DeltaClaim
      SortId
      SortOccurrenceClaim
      GlobalObjectClaim
      ProcessEpochClaim
      StoreIncarnationClaim
      ControlIndexDto
  | PrivateSystemViewDeltaRouteDto
      PredefinedSortRoleDto
      DeltaClaim
      SortId
      SortOccurrenceClaim
      StoreIncarnationClaim
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

deltaRouteDelta :: DeltaRouteDto -> DeltaClaim
deltaRouteDelta route = case route of
  ApplicationDeltaRouteDto delta _ _ _ _ _ _ -> delta
  PrivateSystemViewDeltaRouteDto _ delta _ _ _ -> delta

data PlacementSnapshotDto
  = PlacementSnapshotDto
      HeraldEpochClaim
      PositivePlacementSequenceDto
      [DeltaRouteDto]
  deriving stock (Eq, Show)

placementSnapshotDto ::
  HeraldEpochClaim ->
  PositivePlacementSequenceDto ->
  [DeltaRouteDto] ->
  Either PeerShapeError PlacementSnapshotDto
placementSnapshotDto owner sequenceNumber supplied = do
  routes <- normalizeUnique PlacementSnapshotDuplicateDelta deltaRouteDelta supplied
  Right (PlacementSnapshotDto owner sequenceNumber routes)

placementSnapshotOwner :: PlacementSnapshotDto -> HeraldEpochClaim
placementSnapshotOwner (PlacementSnapshotDto owner _ _) = owner

placementSnapshotSequence :: PlacementSnapshotDto -> PositivePlacementSequenceDto
placementSnapshotSequence (PlacementSnapshotDto _ sequenceNumber _) = sequenceNumber

placementSnapshotRoutes :: PlacementSnapshotDto -> [DeltaRouteDto]
placementSnapshotRoutes (PlacementSnapshotDto _ _ routes) = routes

instance Binary PlacementSnapshotDto where
  put snapshot = do
    put (placementSnapshotOwner snapshot)
    put (placementSnapshotSequence snapshot)
    put (placementSnapshotRoutes snapshot)
  get = do
    owner <- get
    sequenceNumber <- get
    routes <- get
    either (fail . show) pure (placementSnapshotDto owner sequenceNumber routes)

data PlacementUpdateDto
  = FullPlacementSnapshotDto PlacementSnapshotDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data PlacementAcknowledgementDto
  = PlacementAcknowledgementDto HeraldEpochClaim PositivePlacementSequenceDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data StreamPrefixDto
  = EmptyStreamPrefixDto
  | StreamPrefixThroughDto PositiveStreamSequenceDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data StreamDirectionDto
  = StreamDirectionDto HeraldEpochClaim HeraldEpochClaim
  deriving stock (Eq, Ord, Show)

streamDirectionDto ::
  HeraldEpochClaim ->
  HeraldEpochClaim ->
  Either PeerShapeError StreamDirectionDto
streamDirectionDto source destination
  | source == destination = Left (StreamDirectionEndpointsEqual source)
  | otherwise = Right (StreamDirectionDto source destination)

streamDirectionSource :: StreamDirectionDto -> HeraldEpochClaim
streamDirectionSource (StreamDirectionDto source _) = source

streamDirectionDestination :: StreamDirectionDto -> HeraldEpochClaim
streamDirectionDestination (StreamDirectionDto _ destination) = destination

instance Binary StreamDirectionDto where
  put direction = do
    put (streamDirectionSource direction)
    put (streamDirectionDestination direction)
  get = do
    source <- get
    destination <- get
    either (fail . show) pure (streamDirectionDto source destination)

data GapSummaryDto
  = GapSummaryDto
      StreamDirectionDto
      StreamPrefixDto
      [(PositiveStreamSequenceDto, PeerItemDigestClaim)]
  deriving stock (Eq, Show)

gapSummaryDto ::
  StreamDirectionDto ->
  StreamPrefixDto ->
  [(PositiveStreamSequenceDto, PeerItemDigestClaim)] ->
  Either PeerShapeError GapSummaryDto
gapSummaryDto direction receivedPrefix entries
  | entries /= sortOn fst entries || not (strictlyAscending (fmap fst entries)) =
      Left GapEntriesNotStrictlyAscending
  | Just sequenceNumber <- firstInvalid =
      Left (GapEntryNotBeyondFirstMissing sequenceNumber)
  | otherwise = Right (GapSummaryDto direction receivedPrefix entries)
  where
    firstMissing = nextAfterPrefix receivedPrefix
    firstInvalid = fst <$> findFirst ((<= firstMissing) . fst) entries

gapSummaryDirection :: GapSummaryDto -> StreamDirectionDto
gapSummaryDirection (GapSummaryDto direction _ _) = direction

gapSummaryReceivedPrefix :: GapSummaryDto -> StreamPrefixDto
gapSummaryReceivedPrefix (GapSummaryDto _ receivedPrefix _) = receivedPrefix

gapSummaryEntries :: GapSummaryDto -> [(PositiveStreamSequenceDto, PeerItemDigestClaim)]
gapSummaryEntries (GapSummaryDto _ _ entries) = entries

instance Binary GapSummaryDto where
  put summary = do
    put (gapSummaryDirection summary)
    put (gapSummaryReceivedPrefix summary)
    put (gapSummaryEntries summary)
  get = do
    direction <- get
    receivedPrefix <- get
    entries <- get
    either (fail . show) pure (gapSummaryDto direction receivedPrefix entries)

data ResumeOfferDto
  = ResumeOfferDto
      HeraldEpochClaim
      HeraldEpochClaim
      PositiveStreamSequenceDto
      StreamPrefixDto
      ReceiptRetirement
      GapSummaryDto
  deriving stock (Eq, Show)

resumeOfferDto ::
  HeraldEpochClaim ->
  HeraldEpochClaim ->
  PositiveStreamSequenceDto ->
  StreamPrefixDto ->
  StreamPrefixDto ->
  GapSummaryDto ->
  Either PeerShapeError ResumeOfferDto
resumeOfferDto source destination nextSource received completed = resumeOfferWithCompletionDto source destination nextSource received (prefixCompletionDto completed)

resumeOfferWithCompletionDto :: HeraldEpochClaim -> HeraldEpochClaim -> PositiveStreamSequenceDto -> StreamPrefixDto -> ReceiptRetirement -> GapSummaryDto -> Either PeerShapeError ResumeOfferDto
resumeOfferWithCompletionDto source destination nextSource received completion gap = do
  validateStreamCompletionDto received completion
  let completed = completionPrefixDto completion
  forward <- streamDirectionDto source destination
  let expectedGapDirection = reverseDirection forward
  if gapSummaryDirection gap /= expectedGapDirection
    then Left (ResumeGapDirectionMismatch expectedGapDirection (gapSummaryDirection gap))
    else
      if gapSummaryReceivedPrefix gap /= received
        then Left (ResumeGapPrefixMismatch received (gapSummaryReceivedPrefix gap))
        else
          if completed > received
            then Left (ResumeCompletedBeyondReceived completed received)
            else Right (ResumeOfferDto source destination nextSource received completion gap)

resumeOfferSource :: ResumeOfferDto -> HeraldEpochClaim
resumeOfferSource (ResumeOfferDto source _ _ _ _ _) = source

resumeOfferDestination :: ResumeOfferDto -> HeraldEpochClaim
resumeOfferDestination (ResumeOfferDto _ destination _ _ _ _) = destination

resumeOfferNextSourceSequence :: ResumeOfferDto -> PositiveStreamSequenceDto
resumeOfferNextSourceSequence (ResumeOfferDto _ _ sequenceNumber _ _ _) = sequenceNumber

resumeOfferReceivedPrefix :: ResumeOfferDto -> StreamPrefixDto
resumeOfferReceivedPrefix (ResumeOfferDto _ _ _ received _ _) = received

resumeOfferCompletedPrefix :: ResumeOfferDto -> StreamPrefixDto
resumeOfferCompletedPrefix = completionPrefixDto . resumeOfferCompletion

resumeOfferCompletion :: ResumeOfferDto -> ReceiptRetirement
resumeOfferCompletion (ResumeOfferDto _ _ _ _ completion _) = completion

resumeOfferGapSummary :: ResumeOfferDto -> GapSummaryDto
resumeOfferGapSummary (ResumeOfferDto _ _ _ _ _ gap) = gap

instance Binary ResumeOfferDto where
  put offer = do
    put (resumeOfferSource offer)
    put (resumeOfferDestination offer)
    put (resumeOfferNextSourceSequence offer)
    put (resumeOfferReceivedPrefix offer)
    put (resumeOfferCompletion offer)
    put (resumeOfferGapSummary offer)
  get = do
    source <- get
    destination <- get
    nextSource <- get
    received <- get
    completed <- get
    gap <- get
    either (fail . show) pure (resumeOfferWithCompletionDto source destination nextSource received completed gap)

data ResumeResponseDto
  = ResumeResponseDto StreamPrefixDto ReceiptRetirement PositiveStreamSequenceDto
  deriving stock (Eq, Show)

resumeResponseDto ::
  StreamPrefixDto ->
  StreamPrefixDto ->
  PositiveStreamSequenceDto ->
  Either PeerShapeError ResumeResponseDto
resumeResponseDto received completed = resumeResponseWithCompletionDto received (prefixCompletionDto completed)

resumeResponseWithCompletionDto :: StreamPrefixDto -> ReceiptRetirement -> PositiveStreamSequenceDto -> Either PeerShapeError ResumeResponseDto
resumeResponseWithCompletionDto received completion retransmitFrom
  | Left problem <- validateStreamCompletionDto received completion = Left problem
  | retransmitFrom /= expected = Left (ResumeRetransmitFromMismatch expected retransmitFrom)
  | otherwise = Right (ResumeResponseDto received completion retransmitFrom)
  where
    expected = nextAfterPrefix received

resumeResponseReceivedPrefix :: ResumeResponseDto -> StreamPrefixDto
resumeResponseReceivedPrefix (ResumeResponseDto received _ _) = received

resumeResponseCompletedPrefix :: ResumeResponseDto -> StreamPrefixDto
resumeResponseCompletedPrefix = completionPrefixDto . resumeResponseCompletion

resumeResponseCompletion :: ResumeResponseDto -> ReceiptRetirement
resumeResponseCompletion (ResumeResponseDto _ completion _) = completion

resumeResponseRetransmitFrom :: ResumeResponseDto -> PositiveStreamSequenceDto
resumeResponseRetransmitFrom (ResumeResponseDto _ _ retransmitFrom) = retransmitFrom

instance Binary ResumeResponseDto where
  put response = do
    put (resumeResponseReceivedPrefix response)
    put (resumeResponseCompletion response)
    put (resumeResponseRetransmitFrom response)
  get = do
    received <- get
    completed <- get
    retransmitFrom <- get
    either (fail . show) pure (resumeResponseWithCompletionDto received completed retransmitFrom)

prefixCompletionDto :: StreamPrefixDto -> ReceiptRetirement
prefixCompletionDto EmptyStreamPrefixDto = mempty
prefixCompletionDto (StreamPrefixThroughDto sequenceNumber) = receiptRetirementPrefix (Just (positiveStreamSequenceDtoWord64 sequenceNumber))

completionPrefixDto :: ReceiptRetirement -> StreamPrefixDto
completionPrefixDto progress = case receiptRetirementHighWater progress of
  Nothing -> EmptyStreamPrefixDto
  Just high -> through (maybe high (\first -> min high (if first == 0 then 0 else first - 1)) (Set.lookupMin (receiptRetirementExceptions progress)))
  where
    through 0 = EmptyStreamPrefixDto
    through word = StreamPrefixThroughDto (PositiveStreamSequenceDto word)

validateStreamCompletionDto :: StreamPrefixDto -> ReceiptRetirement -> Either PeerShapeError ()
validateStreamCompletionDto received progress
  | receiptRetirementHighWater progress == Just 0 || Set.member 0 (receiptRetirementExceptions progress) = Left InvalidStreamCompletionProgress
  | receiptRetirementHighWater progress > receiptRetirementHighWater (prefixCompletionDto received) = Left (ResumeCompletedBeyondReceived (completionPrefixDto (receiptRetirementPrefix (receiptRetirementHighWater progress))) received)
  | otherwise = Right ()

data PublicationIdDto
  = PublicationIdDto NablaClaim AuthorityEpochDto NablaSequenceDto HeraldEpochClaim
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data ReplicaStrengthDto
  = WeakReplicaDto
  | NormalReplicaDto
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Binary)

data PublicationDestinationDto
  = PublicationDestinationDto DeltaClaim StoreIncarnationClaim ReplicaStrengthDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

publicationDestinationDelta :: PublicationDestinationDto -> DeltaClaim
publicationDestinationDelta (PublicationDestinationDto delta _ _) = delta

newtype PublicationDestinationsDto
  = PublicationDestinationsDto (NonEmpty PublicationDestinationDto)
  deriving stock (Eq, Show)

publicationDestinationsDto ::
  [PublicationDestinationDto] ->
  Either PeerShapeError PublicationDestinationsDto
publicationDestinationsDto supplied = do
  destinations <- case supplied of
    [] -> Left PublicationDestinationsEmpty
    _ -> normalizeUnique PublicationDestinationDuplicateDelta publicationDestinationDelta supplied
  case destinations of
    first : remaining -> Right (PublicationDestinationsDto (first :| remaining))
    [] -> Left PublicationDestinationsEmpty

publicationDestinationEntries :: PublicationDestinationsDto -> NonEmpty PublicationDestinationDto
publicationDestinationEntries (PublicationDestinationsDto destinations) = destinations

instance Binary PublicationDestinationsDto where
  put = put . publicationDestinationEntries
  get =
    get
      >>= either (fail . show) pure
        . publicationDestinationsDto
        . NonEmpty.toList

data PublicationBatchDto
  = PublicationBatchDto
      PublicationIdDto
      ProcessEpochClaim
      SortId
      SortOccurrenceClaim
      ByteString
      ReplicaStrengthDto
      TopologyCutClaim
      ControlIndexDto
      PublicationDestinationsDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data StructuralOccurrenceStampDto
  = StructuralOccurrenceStampDto
      StructuralOccurrenceIdDto
      StructuralVersionVectorDto
      PublicationIdDto
      StructuralPublicationDigestClaim
      StructuralCarrierRoleDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data PeerPublicationItemDto
  = OrdinaryPublicationDto PublicationBatchDto
  | StructuralPublicationDto StructuralOccurrenceStampDto PublicationBatchDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | The committed Open coordinate of one disappearance workflow.  The bridge
-- recomputes the digest from its positive control index.
data DisappearanceProbeIdDto
  = DisappearanceProbeIdDto DisappearanceProbeDigestClaim ControlIndexDto
  deriving stock (Eq, Ord, Show)

disappearanceProbeIdDto ::
  DisappearanceProbeDigestClaim ->
  ControlIndexDto ->
  Either PeerShapeError DisappearanceProbeIdDto
disappearanceProbeIdDto digest index
  | controlIndexDtoWord64 index == 0 = Left DisappearanceProbeControlIndexMustBePositive
  | otherwise = Right (DisappearanceProbeIdDto digest index)

disappearanceProbeIdDigest :: DisappearanceProbeIdDto -> DisappearanceProbeDigestClaim
disappearanceProbeIdDigest (DisappearanceProbeIdDto digest _) = digest

disappearanceProbeIdControlIndex :: DisappearanceProbeIdDto -> ControlIndexDto
disappearanceProbeIdControlIndex (DisappearanceProbeIdDto _ index) = index

instance Binary DisappearanceProbeIdDto where
  put probe = do
    put (disappearanceProbeIdDigest probe)
    put (disappearanceProbeIdControlIndex probe)
  get = do
    digest <- get
    index <- get
    either (fail . show) pure (disappearanceProbeIdDto digest index)

-- | Immutable subject/membership identity, independently checked against Open
-- by the receiving owner after its Oracle projection catches up.
data DisappearanceCoordinateDto
  = DisappearanceCoordinateDto
      DisappearanceSubjectDigestClaim
      HeraldMembershipGenerationClaim
      MemberSetDigestClaim
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data DisappearanceProbeMarkerDto
  = DisappearanceProbeMarkerDto
      DisappearanceProbeIdDto
      DisappearanceCoordinateDto
      StreamDirectionDto
      PositiveStreamSequenceDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data PeerStreamItemDto
  = PeerPublicationDto
      StreamDirectionDto
      PositiveStreamSequenceDto
      PeerItemDigestClaim
      PeerPublicationItemDto
  | PeerRouteCutoverDto
      StreamDirectionDto
      PositiveStreamSequenceDto
      PeerItemDigestClaim
      AlignmentRouteCutoverMarkerDto
  | PeerDisappearanceProbeMarkerDto
      StreamDirectionDto
      PositiveStreamSequenceDto
      PeerItemDigestClaim
      DisappearanceProbeMarkerDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data TopologyFrontierDto
  = TopologyFrontierDto
      StructuralVersionVectorDto
      ControlIndexDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data TopologyPredecessorDto
  = SameGenerationPredecessorDto TopologyCutClaim
  | MembershipSuccessorPredecessorDto
      TopologyCutClaim
      HeraldMembershipGenerationClaim
      HeraldMembershipGenerationClaim
      StructuralVersionVectorDto
      StructuralVersionVectorDto
      TerminalSourceUnionDigestClaim
  | AdmissionTopologyPredecessorDto
      TopologyCutClaim
      HeraldMembershipGenerationClaim
      HeraldMembershipGenerationClaim
      StructuralVersionVectorDto
      StructuralVersionVectorDto
      ByteString
      ByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data TopologyCutDto
  = TopologyCutDto
      TopologyPredecessorDto
      TopologyFrontierDto
      TopologyOccurrenceDigestClaim
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data StructuralAppliedReportDto
  = StructuralAppliedReportDto
      HeraldEpochClaim
      StructuralVersionVectorDto
      ControlIndexDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data TopologyCutAnnounceDto
  = TopologyCutAnnounceDto
      HeraldEpochClaim
      TopologyCutClaim
      TopologyCutDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data TopologyCutAcceptanceDto
  = TopologyCutAcceptanceDto
      TopologyCutClaim
      HeraldEpochClaim
      StructuralVersionVectorDto
      ControlIndexDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

topologyCutAcceptanceReporter ::
  TopologyCutAcceptanceDto -> HeraldEpochClaim
topologyCutAcceptanceReporter
  (TopologyCutAcceptanceDto _ reporter _ _) = reporter

newtype TopologyCutAcceptancesDto
  = TopologyCutAcceptancesDto (NonEmpty TopologyCutAcceptanceDto)
  deriving stock (Eq, Show)

topologyCutAcceptancesDto ::
  [TopologyCutAcceptanceDto] ->
  Either PeerShapeError TopologyCutAcceptancesDto
topologyCutAcceptancesDto supplied = do
  acceptances <- case supplied of
    [] -> Left TopologyCutAcceptancesEmpty
    _ ->
      normalizeUnique
        TopologyCutAcceptanceDuplicateReporter
        topologyCutAcceptanceReporter
        supplied
  case acceptances of
    first : remaining -> Right (TopologyCutAcceptancesDto (first :| remaining))
    [] -> Left TopologyCutAcceptancesEmpty

topologyCutAcceptanceEntries ::
  TopologyCutAcceptancesDto -> NonEmpty TopologyCutAcceptanceDto
topologyCutAcceptanceEntries (TopologyCutAcceptancesDto entries) = entries

instance Binary TopologyCutAcceptancesDto where
  put = put . topologyCutAcceptanceEntries
  get =
    get
      >>= either (fail . show) pure
        . topologyCutAcceptancesDto
        . NonEmpty.toList

data TopologyCutEstablishedDto
  = TopologyCutEstablishedDto
      TopologyCutClaim
      TopologyCutDto
      TopologyCutAcceptancesDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data TopologyCutEstablishedAckDto
  = TopologyCutEstablishedAckDto
      TopologyCutClaim
      HeraldEpochClaim
      HeraldMembershipGenerationClaim
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Canonical failure-probe identity coordinates.  The digest is nominally
-- separated from other 256-bit claims and the positive Oracle control index
-- lets the semantic bridge rederive it rather than trusting an opaque digest.
data HeraldFailureProbeIdDto
  = HeraldFailureProbeIdDto
      HeraldFailureProbeDigestClaim
      ControlIndexDto
  deriving stock (Eq, Ord, Show)

heraldFailureProbeIdDto ::
  HeraldFailureProbeDigestClaim ->
  ControlIndexDto ->
  Either PeerShapeError HeraldFailureProbeIdDto
heraldFailureProbeIdDto digest index
  | controlIndexDtoWord64 index == 0 =
      Left FailureProbeControlIndexMustBePositive
  | otherwise = Right (HeraldFailureProbeIdDto digest index)

heraldFailureProbeIdDigest ::
  HeraldFailureProbeIdDto -> HeraldFailureProbeDigestClaim
heraldFailureProbeIdDigest (HeraldFailureProbeIdDto digest _) = digest

heraldFailureProbeIdControlIndex ::
  HeraldFailureProbeIdDto -> ControlIndexDto
heraldFailureProbeIdControlIndex (HeraldFailureProbeIdDto _ index) = index

instance Binary HeraldFailureProbeIdDto where
  put probe = do
    put (heraldFailureProbeIdDigest probe)
    put (heraldFailureProbeIdControlIndex probe)
  get =
    get
      >>= \digest ->
        get
          >>= either (fail . show) pure
            . heraldFailureProbeIdDto digest

-- | One fresh probe qualified by semantic membership and captured voter configuration.
data DirectFailureProbeRequestDto
  = DirectFailureProbeRequestDto
      HeraldFailureProbeIdDto
      HeraldEpochClaim
      HeraldMembershipGenerationClaim
      OracleVoterConfigurationClaim
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data DirectFailureProbeResultDto
  = DirectFailureProbeReachableDto
  | DirectFailureProbeUnreachableDto
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data DirectFailureProbeResponseDto
  = DirectFailureProbeResponseDto
      DirectFailureProbeRequestDto
      DirectFailureProbeResultDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | A sealed terminal-source inventory.  The routing coordinates and digest
-- are repeated outside the canonical semantic transcript so a receiver can
-- select the owning generation before asking the Herald kernel to materialize
-- and verify the opaque bytes.
data TerminalSourceInventoryDto
  = TerminalSourceInventoryDto
      HeraldMembershipGenerationClaim
      HeraldMembershipGenerationClaim
      HeraldEpochClaim
      HeraldEpochClaim
      StructuralPrefixDto
      TerminalSourceInventoryDigestClaim
      ByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data TerminalSourcePayloadRequestDto
  = TerminalSourcePayloadRequestDto
      HeraldMembershipGenerationClaim
      HeraldMembershipGenerationClaim
      HeraldEpochClaim
      StructuralOccurrenceIdDto
      TerminalSourceInventoryDigestClaim
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | A survivor-attributed response carrying one exact retired-source
-- occurrence.  The semantic bridge checks that the canonical occurrence bytes
-- rederive both the occurrence identity and publication digest.
data TerminalSourcePayloadRelayDto
  = TerminalSourcePayloadRelayDto
      HeraldMembershipGenerationClaim
      HeraldMembershipGenerationClaim
      HeraldEpochClaim
      HeraldEpochClaim
      TerminalSourceInventoryDigestClaim
      StructuralOccurrenceIdDto
      StructuralPublicationDigestClaim
      ByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Canonical candidate union plus the coordinates needed to select its
-- retained structural owner before materialization.
newtype RetiredHeraldsDto = RetiredHeraldsDto (NonEmpty HeraldEpochClaim)
  deriving stock (Eq, Show)

retiredHeraldsDto :: [HeraldEpochClaim] -> Either PeerShapeError RetiredHeraldsDto
retiredHeraldsDto supplied = do
  normalized <- normalizeUnique RetiredHeraldDuplicate id supplied
  maybe (Left RetiredHeraldsEmpty) (Right . RetiredHeraldsDto) (NonEmpty.nonEmpty normalized)

retiredHeraldEntries :: RetiredHeraldsDto -> NonEmpty HeraldEpochClaim
retiredHeraldEntries (RetiredHeraldsDto entries) = entries

instance Binary RetiredHeraldsDto where
  put = put . retiredHeraldEntries
  get = get >>= either (fail . show) pure . retiredHeraldsDto . NonEmpty.toList

data TerminalSourceUnionDto
  = TerminalSourceUnionDto
      HeraldMembershipGenerationClaim
      HeraldMembershipGenerationClaim
      RetiredHeraldsDto
      TerminalSourceUnionDigestClaim
      ByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data TerminalSourceUnionAnnounceDto
  = TerminalSourceUnionAnnounceDto
      HeraldEpochClaim
      TerminalSourceUnionDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data TerminalSourceUnionAcceptanceDto
  = TerminalSourceUnionAcceptanceDto
      HeraldMembershipGenerationClaim
      HeraldMembershipGenerationClaim
      RetiredHeraldsDto
      TerminalSourceUnionDigestClaim
      HeraldEpochClaim
      TerminalSourceAcceptanceDigestClaim
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

terminalSourceUnionAcceptanceReporter ::
  TerminalSourceUnionAcceptanceDto -> HeraldEpochClaim
terminalSourceUnionAcceptanceReporter
  (TerminalSourceUnionAcceptanceDto _ _ _ _ reporter _) = reporter

newtype TerminalSourceUnionAcceptancesDto
  = TerminalSourceUnionAcceptancesDto
      (NonEmpty TerminalSourceUnionAcceptanceDto)
  deriving stock (Eq, Show)

terminalSourceUnionAcceptancesDto ::
  [TerminalSourceUnionAcceptanceDto] ->
  Either PeerShapeError TerminalSourceUnionAcceptancesDto
terminalSourceUnionAcceptancesDto supplied = do
  acceptances <- case supplied of
    [] -> Left TerminalSourceUnionAcceptancesEmpty
    _ ->
      normalizeUnique
        TerminalSourceUnionAcceptanceDuplicateReporter
        terminalSourceUnionAcceptanceReporter
        supplied
  case acceptances of
    first : remaining ->
      Right (TerminalSourceUnionAcceptancesDto (first :| remaining))
    [] -> Left TerminalSourceUnionAcceptancesEmpty

terminalSourceUnionAcceptanceEntries ::
  TerminalSourceUnionAcceptancesDto ->
  NonEmpty TerminalSourceUnionAcceptanceDto
terminalSourceUnionAcceptanceEntries
  (TerminalSourceUnionAcceptancesDto entries) = entries

instance Binary TerminalSourceUnionAcceptancesDto where
  put = put . terminalSourceUnionAcceptanceEntries
  get =
    get
      >>= either (fail . show) pure
        . terminalSourceUnionAcceptancesDto
        . NonEmpty.toList

data TerminalSourceUnionEstablishedDto
  = TerminalSourceUnionEstablishedDto
      TerminalSourceUnionDto
      TerminalSourceUnionAcceptancesDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentObligationIdDto
  = AlignmentObligationIdDto
      HeraldEpochClaim
      PositiveAlignmentObligationSequenceDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data AlignmentSubscriptionIdDto
  = AlignmentSubscriptionIdDto
      HeraldEpochClaim
      PositiveAlignmentSubscriptionSequenceDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data PhysicalPlacementRevisionEntryDto
  = PhysicalPlacementRevisionEntryDto
      HeraldEpochClaim
      PositivePlacementSequenceDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

physicalPlacementRevisionEntryHerald ::
  PhysicalPlacementRevisionEntryDto -> HeraldEpochClaim
physicalPlacementRevisionEntryHerald
  (PhysicalPlacementRevisionEntryDto herald _) = herald

data PhysicalPlacementRevisionVectorDto
  = PhysicalPlacementRevisionVectorDto
      HeraldMembershipGenerationClaim
      MemberSetDigestClaim
      (NonEmpty PhysicalPlacementRevisionEntryDto)
  deriving stock (Eq, Show)

physicalPlacementRevisionVectorDto ::
  HeraldMembershipGenerationClaim ->
  MemberSetDigestClaim ->
  [PhysicalPlacementRevisionEntryDto] ->
  Either PeerShapeError PhysicalPlacementRevisionVectorDto
physicalPlacementRevisionVectorDto generation members supplied = do
  entries <- case supplied of
    [] -> Left PhysicalPlacementRevisionVectorEmpty
    _ ->
      normalizeUnique
        PhysicalPlacementRevisionVectorDuplicateHerald
        physicalPlacementRevisionEntryHerald
        supplied
  case entries of
    first : remaining ->
      Right
        ( PhysicalPlacementRevisionVectorDto
            generation
            members
            (first :| remaining)
        )
    [] -> Left PhysicalPlacementRevisionVectorEmpty

physicalPlacementRevisionVectorMembershipGeneration ::
  PhysicalPlacementRevisionVectorDto -> HeraldMembershipGenerationClaim
physicalPlacementRevisionVectorMembershipGeneration
  (PhysicalPlacementRevisionVectorDto generation _ _) = generation

physicalPlacementRevisionVectorMemberSetDigest ::
  PhysicalPlacementRevisionVectorDto -> MemberSetDigestClaim
physicalPlacementRevisionVectorMemberSetDigest
  (PhysicalPlacementRevisionVectorDto _ members _) = members

physicalPlacementRevisionVectorEntries ::
  PhysicalPlacementRevisionVectorDto -> NonEmpty PhysicalPlacementRevisionEntryDto
physicalPlacementRevisionVectorEntries
  (PhysicalPlacementRevisionVectorDto _ _ entries) = entries

instance Binary PhysicalPlacementRevisionVectorDto where
  put (PhysicalPlacementRevisionVectorDto generation members entries) = do
    put generation
    put members
    put entries
  get = do
    generation <- get
    members <- get
    entries <- get
    either
      (fail . show)
      pure
      ( physicalPlacementRevisionVectorDto
          generation
          members
          (NonEmpty.toList entries)
      )

data AlignmentMemberDto
  = AlignmentMemberDto DeltaClaim StoreIncarnationClaim HeraldEpochClaim
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data FreshMemberBaseEvidenceDto
  = FreshMemberBaseEvidenceDto
      DeltaClaim
      StoreIncarnationClaim
      StoreRevisionDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data AlignmentCutDto
  = AlignmentCutDto
      Word64
      Word64
      SortId
      SortOccurrenceClaim
      TopologyCutDto
      PhysicalPlacementRevisionVectorDto
      (NonEmpty AlignmentMemberDto)
      [ContextClassGenerationClaim]
      [FreshMemberBaseEvidenceDto]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentPlanIdDto = AlignmentPlanIdDto Word64 Word64 SortId SortOccurrenceClaim TopologyCutClaim PhysicalPlacementRevisionVectorDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentBindingDispositionDto = AlignmentCreatedDto | AlignmentCarriedDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data AlignmentPredecessorStatusDto = AlignmentPredecessorUsableDto | AlignmentPredecessorInvalidatedDto | AlignmentPredecessorResetDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentPlanBindingClaimDto = AlignmentPlanBindingClaimDto (NonEmpty DeltaClaim) AlignmentBindingDispositionDto ContextClassGenerationClaim
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data AlignmentPlanRelationClaimDto = AlignmentPlanRelationClaimDto ContextClassGenerationClaim ContextClassGenerationClaim ReplicaStrengthDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data AlignmentPlanAnnounceDto = AlignmentPlanAnnounceDto AlignmentPlanIdDto TopologyCutDto (Maybe AlignmentPlanIdDto) AlignmentPredecessorStatusDto [AlignmentPlanBindingClaimDto] [AlignmentPlanRelationClaimDto] [AlignmentCutAnnounceDto]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentPlanAcceptedDto = AlignmentPlanAcceptedDto AlignmentPlanIdDto HeraldEpochClaim HeraldPublicationPrefixDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentLostStoreDto = AlignmentLostStoreDto HeraldEpochClaim DeltaClaim StoreIncarnationClaim PositivePlacementSequenceDto
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data AlignmentPlanObsoleteDto = AlignmentPlanObsoleteDto AlignmentPlanIdDto HeraldEpochClaim (NonEmpty AlignmentLostStoreDto)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentCutAnnounceDto
  = AlignmentCutAnnounceDto ContextClassGenerationClaim AlignmentCutDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentCutAcceptedDto
  = AlignmentCutAcceptedDto
      ContextClassGenerationClaim
      HeraldEpochClaim
      TopologyCutClaim
      PhysicalPlacementRevisionVectorDto
      HeraldPublicationPrefixDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data ClassMemberReadyDto
  = ClassMemberReadyDto
      PositiveAlignmentEvidenceSequenceDto
      ContextClassGenerationClaim
      StoreIncarnationClaim
      MemberReadyEvidenceDigestClaim
      StoreRevisionDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data HistoricalCertificateDto
  = HistoricalCertificateDto
      ContextClassGenerationClaim
      StoreIncarnationClaim
      MemberReadyEvidenceDigestClaim
      BootstrapEvidenceDigestClaim
      StoreRevisionDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data DestinationStoreDto
  = DestinationStoreDto DeltaClaim StoreIncarnationClaim
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

data StructuralConsequenceCauseDto
  = StructuralOccurrenceCauseDto StructuralOccurrenceIdDto
  | LabelReleaseCauseDto LabelDecisionClaim ControlIndexDto
  | ProcessEndCauseDto ProcessEpochClaim ControlIndexDto
  | PredefinedDisappearanceCauseDto DisappearanceProbeIdDto ControlIndexDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentObligationDto
  = AlignmentObligationDto
      AlignmentObligationIdDto
      StructuralConsequenceCauseDto
      SortId
      SortOccurrenceClaim
      ContextClassGenerationClaim
      (NonEmpty DestinationStoreDto)
      ContextClassGenerationClaim
      ReplicaStrengthDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentSourceProofDto
  = FinalCertificateDto
      ContextClassGenerationClaim
      HistoricalCertificateDigestClaim
  | BootstrapProofDto
      ContextClassGenerationClaim
      BootstrapEvidenceDigestClaim
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentSubscribeDto
  = AlignmentSubscribeDto
      AlignmentObligationDto
      AlignmentSubscriptionIdDto
      StoreIncarnationClaim
      AlignmentSourceProofDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data RetainedObservationOriginDto
  = PrimordialRetainedObservationDto
  | RoutedRetainedObservationDto
      ProcessEpochClaim
      TopologyCutClaim
      ControlIndexDto
      (Maybe StructuralOccurrenceStampDto)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data RetainedPublicationEvidenceDto
  = RetainedPublicationEvidenceDto
      PublicationIdDto
      SortId
      SortOccurrenceClaim
      ByteString
      ReplicaStrengthDto
      RetainedObservationOriginDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data RetainedStateEvidenceDto
  = RetainedStateEvidenceDto
      RetainedPublicationEvidenceDto
      RetainedPublicationEvidenceDto
      ReplicaStrengthDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentSnapshotStartDto
  = AlignmentSnapshotStartDto
      AlignmentSubscriptionIdDto
      StoreRevisionDto
      AlignmentSnapshotDigestClaim
      Word64
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentSnapshotChunkDto
  = AlignmentSnapshotChunkDto
      AlignmentSubscriptionIdDto
      Word64
      [RetainedStateEvidenceDto]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentSnapshotEndDto
  = AlignmentSnapshotEndDto
      AlignmentSubscriptionIdDto
      StoreRevisionDto
      AlignmentSnapshotDigestClaim
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentChangeDto
  = AlignmentChangeDto
      AlignmentSubscriptionIdDto
      StoreRevisionDto
      RetainedPublicationEvidenceDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentLiveDto
  = AlignmentLiveDto AlignmentSubscriptionIdDto StoreRevisionDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentAckDto
  = AlignmentAckDto AlignmentSubscriptionIdDto StoreRevisionDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentCancelReasonDto
  = AlignmentRelationRemovedDto
  | AlignmentSourceIncarnationLostDto
  | AlignmentDestinationIncarnationLostDto
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Binary)

data AlignmentCancelDto
  = AlignmentCancelDto AlignmentSubscriptionIdDto AlignmentCancelReasonDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentRouteCutoverMarkerDto
  = AlignmentRouteCutoverMarkerDto
      AlignmentPlanIdDto
      ContextClassGenerationClaim
      HeraldEpochClaim
      HeraldEpochClaim
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | The only semantic controls carried by the cumulative delivery lane.
data AlignmentRetainedEvidenceDto
  = AlignmentPlanAcceptanceEvidenceDto AlignmentPlanAcceptedDto
  | AlignmentCutAcceptanceEvidenceDto AlignmentCutAcceptedDto
  | AlignmentMemberReadyEvidenceDto ClassMemberReadyDto
  | AlignmentPlanObsoleteEvidenceDto AlignmentPlanObsoleteDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data AlignmentControlDto
  = AlignmentPlanAnnouncedDto AlignmentPlanAnnounceDto
  | AlignmentCutAnnouncedDto AlignmentCutAnnounceDto
  | AlignmentHistoricalCertificateAdvertisedDto HistoricalCertificateDto
  | AlignmentSubscribeRequestedDto AlignmentSubscribeDto
  | AlignmentSnapshotStartedDto AlignmentSnapshotStartDto
  | AlignmentSnapshotChunkTransferredDto AlignmentSnapshotChunkDto
  | AlignmentSnapshotEndedDto AlignmentSnapshotEndDto
  | AlignmentChangeTransferredDto AlignmentChangeDto
  | AlignmentLiveAdvertisedDto AlignmentLiveDto
  | AlignmentAcknowledgedDto AlignmentAckDto
  | AlignmentCancelledDto AlignmentCancelDto
  | AlignmentProbeMarkerDto
      DisappearanceProbeIdDto
      AlignmentSubscriptionIdDto
      StoreRevisionDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Physical established-lane heartbeat traffic.
--
-- The connection owner consumes these messages; neither constructor is a
-- semantic 'PeerControlDto' or an ordered publication-stream item.
data PeerHeartbeatDto
  = Ping PeerHeartbeatNonce
  | Pong PeerHeartbeatNonce
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- These retain global identities only; private aliases never cross EPRP.
data RemoteChildDto = RemoteChildDto ProcessEpochClaim ControlIndexDto ConnectionDescriptor
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
data RemotePreparationStatusDto
  = RemotePreparationPendingDto [Text]
  | RemotePreparationAvailableDto RemoteChildDto (Maybe [Text])
  | RemotePreparationClaimedDto RemoteChildDto
  | RemotePreparationCancelledDto
  | RemotePreparationFailedDto StartupError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
data PreparationControlDto
  = PreparationOfferedDto ChildPreparation HeraldLocator ByteString
  | PreparationCancelledDto ChildPreparation
  | PreparationQueriedDto ChildPreparation
  | PreparationRepliedDto ChildPreparation RemotePreparationStatusDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | One direct terminal-installation report. The Herald owner checks the
-- authenticated reporter and canonical outcome after this control prerequisite
-- becomes visible in its Oracle projection.
data LabelInstallationReportDto
  = LabelInstallationReportDto
      !LabelDecisionClaim
      !HeraldEpochClaim
      !ControlIndexDto
      !LabelOutcomeDigestClaim
  deriving stock (Eq, Show)

labelInstallationReportDto ::
  LabelDecisionClaim ->
  HeraldEpochClaim ->
  ControlIndexDto ->
  LabelOutcomeDigestClaim ->
  Either PeerShapeError LabelInstallationReportDto
labelInstallationReportDto decision reporter index digest
  | controlIndexDtoWord64 index == 0 = Left LabelInstallationControlIndexMustBePositive
  | otherwise = Right (LabelInstallationReportDto decision reporter index digest)

labelInstallationDecisionId :: LabelInstallationReportDto -> LabelDecisionClaim
labelInstallationDecisionId (LabelInstallationReportDto decision _ _ _) = decision

labelInstallationReporter :: LabelInstallationReportDto -> HeraldEpochClaim
labelInstallationReporter (LabelInstallationReportDto _ reporter _ _) = reporter

labelInstallationControlIndex :: LabelInstallationReportDto -> ControlIndexDto
labelInstallationControlIndex (LabelInstallationReportDto _ _ index _) = index

labelInstallationOutcomeDigest :: LabelInstallationReportDto -> LabelOutcomeDigestClaim
labelInstallationOutcomeDigest (LabelInstallationReportDto _ _ _ digest) = digest

instance Binary LabelInstallationReportDto where
  put report = do
    put (labelInstallationDecisionId report)
    put (labelInstallationReporter report)
    put (labelInstallationControlIndex report)
    put (labelInstallationOutcomeDigest report)
  get = do
    decision <- get
    reporter <- get
    index <- get
    digest <- get
    either (fail . show) pure (labelInstallationReportDto decision reporter index digest)

data PeerControlDto
  = KnownHeraldsDto KnownHeraldCollectionDto
  | PlacementUpdateDto PlacementUpdateDto
  | PlacementAcknowledgedDto PlacementAcknowledgementDto
  | StreamReceivedDto StreamDirectionDto StreamPrefixDto
  | StreamCompletedDto StreamDirectionDto ReceiptRetirement
  | StreamFrontierAdvancedDto StreamDirectionDto PositiveStreamSequenceDto
  | StreamResumeOfferedDto ResumeOfferDto
  | StreamResumeAcceptedDto ResumeResponseDto
  | StructuralAppliedReportedDto StructuralAppliedReportDto
  | TopologyCutAnnouncedDto TopologyCutAnnounceDto
  | TopologyCutAcceptedDto TopologyCutAcceptanceDto
  | TopologyCutEstablishedControlDto TopologyCutEstablishedDto
  | TopologyCutEstablishedAcknowledgedDto TopologyCutEstablishedAckDto
  | AlignmentControlDto AlignmentControlDto
  | AlignmentEvidenceDeliveredDto PositiveAlignmentDeliverySequenceDto AlignmentRetainedEvidenceDto
  | AlignmentDeliveryProgressDto
  | PreparationControlDto PreparationControlDto
  | DirectFailureProbeRequestedDto DirectFailureProbeRequestDto
  | DirectFailureProbeRespondedDto DirectFailureProbeResponseDto
  | TerminalSourceInventoryAdvertisedDto TerminalSourceInventoryDto
  | TerminalSourcePayloadRequestedDto TerminalSourcePayloadRequestDto
  | TerminalSourcePayloadRelayedDto TerminalSourcePayloadRelayDto
  | TerminalSourceUnionAnnouncedDto TerminalSourceUnionAnnounceDto
  | TerminalSourceUnionAcceptedDto TerminalSourceUnionAcceptanceDto
  | TerminalSourceUnionEstablishedControlDto TerminalSourceUnionEstablishedDto
  | LabelInstalledDto LabelInstallationReportDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data PeerEnvelope
  = PeerHelloEnvelope PeerHelloDto
  | PeerHeartbeatEnvelope PeerHeartbeatDto
  | PeerControlEnvelope ReceiptRetirement PeerControlDto
  | PeerPublicationEnvelope ReceiptRetirement PeerStreamItemDto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

normalizeAddresses :: [PeerAddressDto] -> [PeerAddressDto]
normalizeAddresses = Set.toAscList . Set.fromList

normalizeUnique ::
  (Ord key) =>
  (key -> PeerShapeError) ->
  (value -> key) ->
  [value] ->
  Either PeerShapeError [value]
normalizeUnique duplicateError keyOf supplied =
  case firstDuplicate (fmap keyOf normalized) of
    Nothing -> Right normalized
    Just duplicate -> Left (duplicateError duplicate)
  where
    normalized = sortOn keyOf supplied

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate (first : second : remaining)
  | first == second = Just first
  | otherwise = firstDuplicate (second : remaining)
firstDuplicate _ = Nothing

strictlyAscending :: (Ord value) => [value] -> Bool
strictlyAscending [] = True
strictlyAscending [_] = True
strictlyAscending (left : right : remaining) =
  left < right && strictlyAscending (right : remaining)

findFirst :: (value -> Bool) -> [value] -> Maybe value
findFirst _ [] = Nothing
findFirst predicate (value : remaining)
  | predicate value = Just value
  | otherwise = findFirst predicate remaining

nextAfterPrefix :: StreamPrefixDto -> PositiveStreamSequenceDto
nextAfterPrefix EmptyStreamPrefixDto = PositiveStreamSequenceDto 1
nextAfterPrefix (StreamPrefixThroughDto (PositiveStreamSequenceDto value)) =
  PositiveStreamSequenceDto (value + 1)

reverseDirection :: StreamDirectionDto -> StreamDirectionDto
reverseDirection (StreamDirectionDto source destination) =
  StreamDirectionDto destination source
