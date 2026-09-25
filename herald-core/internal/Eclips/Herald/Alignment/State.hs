{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure owner of cause-qualified structural consequence debt and its
-- exact-once refinement into alignment generations, obligations, evidence,
-- and supplier-qualified attempts.
module Eclips.Herald.Alignment.State
  ( State,
    emptyState,
    currentAlignmentPlanForSort,
    liveCurrentAlignmentPlans,
    alignmentPlanEntries,
    lookupAlignmentPlan,
    alignmentPlanInvalidated,
    currentAlignmentMemberReady,
    unsettledAlignmentPlans,
    prepareAlignmentPlanPromotion,
    PreparedAlignmentPlanResetPromotion,
    prepareAlignmentPlanResetPromotion,
    preparedAlignmentPlanResetPromotionCancellations,
    preparedAlignmentPlanResetPromotionDisposition,
    commitAlignmentPlanResetPromotion,
    retainAlignmentPlanObsolete,
    alignmentPlanObsoleteEntries,
    alignmentPlanObsoleteForSort,
    alignmentResetRequiredSorts,
    retainAlignmentPlanAcceptance,
    alignmentPlanAcceptanceEntries,
    lookupAlignmentPlanAcceptance,
    generationCurrentPlanInvalidated,
    generationBirthPlanId,
    initialState,
    takePreparationReadinessChanges,
    clearPreparationReadinessChanges,
    initializeTransferMemberWork,
    transferMemberWorkHerald,
    TransferWorkStage (..),
    TransferMemberKey,
    transferWorkIds,
    pendingTransferWork,
    nextTransferWork,
    consumeTransferWork,
    clearTransferWork,
    clearDestinationProgressChanges,
    pendingTransferMemberIds,
    lookupPendingAlignmentMember,
    lookupBootstrapImport,
    bootstrapImportsAtMember,
    bootstrapFulfillmentsAtMember,
    alignmentAttemptsAtMember,
    activeAlignmentObligationCountAtMember,
    wakeDestinationProgress,
    wakeMemberStoreChanges,
    transferSchedulingValid,
    consumeLossCheck,
    wakeLossCheck,
    clearLossScheduling,
    takeRouteMarkerChanges,
    clearRouteMarkerChanges,
    PendingGenerationEvidenceKey,
    pendingGenerationEvidenceKey,
    generationEvidenceWorkIds,
    pendingGenerationEvidenceWork,
    lookupPendingGenerationEvidence,
    nextGenerationEvidenceWork,
    consumeGenerationEvidenceWork,
    takeGenerationQueryScopeChange,
    hasPendingGenerationAnnouncements,
    generationPlanWorkIds,
    pendingGenerationPlanWork,
    consumeGenerationPlanWork,
    wakeGenerationPlanInputs,
    wakeGenerationPlanSorts,
    synchronizeGenerationPlanContext,
    clearGenerationScheduling,
    generationSchedulingValid,
    liveAlignmentDebtCoordinatesAtSorts,
    alignmentStructuralDebts,
    structuralDebtsForCause,
    liveAlignmentStructuralDebtEntries,
    AlignmentPromotionKey,
    alignmentPromotionKey,
    alignmentPromotionKeyForPlan,
    alignmentPromotionKeyPlanId,
    alignmentPromotionKeyCause,
    alignmentPromotionKeySort,
    alignmentPromotionKeyTopologyCut,
    alignmentPromotionKeyPlacementVector,
    AlignmentPromotionReceipt,
    alignmentPromotionReceiptTopologyCut,
    alignmentPromotionReceiptPlacementVector,
    alignmentPromotionReceiptDebtKeys,
    alignmentPromotionReceiptGenerationIds,
    alignmentPromotionReceiptRelations,
    alignmentPromotionReceiptObligationIds,
    alignmentPromotionReceiptBootstrapImportKeys,
    alignmentPromotionEntries,
    alignmentCompletedCauseCoverage,
    alignmentCauseCoverageKeys,
    alignmentCauseCoverageKeysFor,
    alignmentGenerationEntries,
    latestAlignmentGenerationEntries,
    pendingAlignmentMemberProgress,
    pendingAlignmentRouteCutoverEntries,
    retireCompletedAlignmentRouteCutovers,
    AlignmentControlEvidence (..),
    alignmentControlEvidence,
    alignmentControlEvidenceForPlans,
    alignmentChangedControlGenerations,
    alignmentControlPlanGenerations,
    retainHistoricalAlignmentRecipient,
    historicalAlignmentRecipientGenerations,
    historicalAlignmentRecipient,
    lookupAlignmentGeneration,
    alignmentGenerationRelationEntries,
    latestAlignmentGenerationsForSort,
    alignmentObligationEntries,
    activeAlignmentObligationEntries,
    alignmentClosingObligationIds,
    alignmentObligationIsClosing,
    alignmentClosedObligationEntries,
    requiredPredecessorInputOwnersAt,
    InputClosureChanges (..),
    takeInputClosureChanges,
    clearInputClosureChanges,
    lookupAlignmentObligation,
    alignmentAttemptEntries,
    AlignmentSourceCoordinate,
    alignmentSourceCoordinate,
    alignmentSourceCoordinateHerald,
    alignmentSourceCoordinateStoreIncarnation,
    alignmentFailedSourceCoordinates,
    UnavailableAlignmentSource,
    unavailableAlignmentSource,
    unavailableAlignmentSourceHerald,
    unavailableAlignmentSourceDelta,
    unavailableAlignmentSourceStoreIncarnation,
    alignmentUnavailableSources,
    alignmentAttemptUsesUnavailableSource,
    bootstrapImportAttemptUsesUnavailableSource,
    AlignmentSourceRetentionDisposition (..),
    PreparedUnavailableAlignmentSourceRetention,
    prepareUnavailableAlignmentSourceRetention,
    preparedUnavailableAlignmentSourceRetentionDisposition,
    commitUnavailableAlignmentSourceRetention,
    AlignmentPlanCoordinate,
    alignmentPlanCoordinateId,
    alignmentPlanCoordinateSort,
    alignmentPlanCoordinateTopologyCut,
    alignmentPlanCoordinatePlacementVector,
    AlignmentPlanInvalidationCause (..),
    AlignmentPlanInvalidationReceipt,
    alignmentPlanInvalidationReceiptCoordinate,
    alignmentPlanInvalidationReceiptCauses,
    alignmentPlanInvalidationReceiptGenerations,
    alignmentPlanInvalidationReceiptObligations,
    alignmentPlanInvalidationReceiptBootstrapImports,
    alignmentPlanInvalidationEntries,
    alignmentPlanIsInvalidated,
    AlignmentAttemptInvalidationReceipt,
    alignmentAttemptInvalidationAttempt,
    alignmentAttemptInvalidationReason,
    alignmentAttemptInvalidationReasons,
    alignmentAttemptInvalidationEntries,
    PendingGenerationEvidence,
    pendingGenerationEvidenceRemoteHerald,
    pendingGenerationEvidenceControl,
    pendingGenerationEvidenceEntries,
    PendingGenerationEvidenceDisposition (..),
    PendingGenerationEvidenceProblem (..),
    PreparedPendingGenerationEvidence,
    preparePendingGenerationEvidence,
    preparePendingGenerationEvidenceRemoval,
    preparedPendingGenerationEvidenceDisposition,
    commitPendingGenerationEvidence,
    PreparedPendingGenerationEvidenceRetirement,
    preparePendingGenerationEvidenceRetirement,
    preparedPendingGenerationEvidenceRetirementRemoved,
    commitPendingGenerationEvidenceRetirement,
    BootstrapImportSource (..),
    BootstrapImportKey,
    bootstrapImportKeyGeneration,
    bootstrapImportKeyDestination,
    bootstrapImportKeySource,
    BootstrapImport,
    bootstrapImportKeyValue,
    bootstrapImportSubscribeEnvelope,
    bootstrapImportEntries,
    BootstrapImportAttempt,
    bootstrapImportAttemptImport,
    bootstrapImportAttemptSourceHerald,
    bootstrapImportAttemptSubscribe,
    bootstrapImportAttemptEntries,
    lookupBootstrapImportAttempt,
    BootstrapImportAttemptInvalidationReceipt,
    bootstrapImportAttemptInvalidationAttempt,
    bootstrapImportAttemptInvalidationReason,
    bootstrapImportAttemptInvalidationReasons,
    bootstrapImportAttemptInvalidationEntries,
    BootstrapImportFulfillmentReceipt,
    bootstrapImportFulfillmentAttempt,
    bootstrapImportFulfillmentThrough,
    bootstrapImportFulfillmentEntries,
    alignmentCutAcceptanceEntries,
    lookupAlignmentCutAcceptance,
    alignmentMemberReadinessEntries,
    lookupAlignmentMemberReadiness,
    alignmentLocalMemberReadinessEntries,
    lookupAlignmentLocalMemberReadiness,
    alignmentHistoricalCertificateEntries,
    liveAlignmentHistoricalCertificateEntries,
    lookupAlignmentHistoricalCertificate,
    alignmentTransferState,
    replaceAlignmentTransferState,
    StructuralDebtRetention (..),
    AlignmentDebtProblem (..),
    PreparedStructuralDebtRetention,
    prepareStructuralDebtRetention,
    preparedStructuralDebtRetention,
    commitStructuralDebtRetention,
    AlignmentPromotionDisposition (..),
    AlignmentPromotionProblem (..),
    PreparedAlignmentPromotion,
    prepareAlignmentPromotion,
    preparedAlignmentPromotionDisposition,
    commitAlignmentPromotion,
    retainHistoricalAlignmentPlan,
    retainHistoricalAlignmentRoutingPlan,
    retainHistoricalAlignmentPlanCoverage,
    historicalAlignmentPlanFrontier,
    adoptHistoricalAlignmentPlanFrontier,
    retainHistoricalAlignmentCoverage,
    historicalAlignmentFrontier,
    adoptHistoricalAlignmentFrontier,
    AlignmentCutEvidenceDisposition (..),
    AlignmentCutEvidenceProblem (..),
    PreparedAlignmentCutAcceptance,
    prepareAlignmentCutAcceptance,
    preparedAlignmentCutAcceptanceDisposition,
    commitAlignmentCutAcceptance,
    AlignmentReadinessDisposition (..),
    AlignmentReadinessProblem (..),
    PreparedClassMemberReady,
    prepareClassMemberReady,
    commitClassMemberReady,
    PreparedLocalClassMemberReady,
    prepareLocalClassMemberReady,
    preparedLocalClassMemberReady,
    preparedLocalClassMemberReadyDisposition,
    commitLocalClassMemberReady,
    PreparedHistoricalCertificate,
    prepareHistoricalCertificate,
    commitHistoricalCertificate,
    AlignmentAttemptDisposition (..),
    AlignmentAttemptProblem (..),
    PreparedAlignmentAttempt,
    prepareAlignmentAttempt,
    prepareAlignmentAttemptExcluding,
    preparedAlignmentAttempt,
    preparedAlignmentAttemptDisposition,
    commitAlignmentAttempt,
    AlignmentObligationClosureCompletionDisposition (..),
    AlignmentObligationClosureCompletionProblem (..),
    PreparedAlignmentObligationClosureCompletion,
    prepareAlignmentObligationClosureCompletion,
    preparedAlignmentObligationClosureCompletionCancellations,
    preparedAlignmentObligationClosureCompletionDisposition,
    commitAlignmentObligationClosureCompletion,
    AlignmentAttemptInvalidationDisposition (..),
    AlignmentAttemptInvalidationProblem (..),
    PreparedAlignmentAttemptInvalidation,
    prepareAlignmentAttemptInvalidation,
    preparedAlignmentAttemptInvalidationReceipt,
    preparedAlignmentAttemptInvalidationCancellation,
    preparedAlignmentAttemptInvalidationDisposition,
    commitAlignmentAttemptInvalidation,
    AlignmentPlanInvalidationDisposition (..),
    AlignmentPlanInvalidationProblem (..),
    PreparedAlignmentPlanInvalidation,
    prepareAlignmentPlanInvalidation,
    preparedAlignmentPlanInvalidationReceipt,
    preparedAlignmentPlanInvalidationCancellations,
    preparedAlignmentPlanInvalidationDisposition,
    commitAlignmentPlanInvalidation,
    BootstrapImportDisposition (..),
    BootstrapImportProblem (..),
    PreparedBootstrapImportAttempt,
    prepareBootstrapImportAttempt,
    prepareBootstrapImportAttemptExcluding,
    preparedBootstrapImportAttempt,
    preparedBootstrapImportSubscribe,
    preparedBootstrapImportDisposition,
    commitBootstrapImportAttempt,
    BootstrapImportAttemptInvalidationDisposition (..),
    BootstrapImportAttemptInvalidationProblem (..),
    PreparedBootstrapImportAttemptInvalidation,
    prepareBootstrapImportAttemptInvalidation,
    preparedBootstrapImportAttemptInvalidationReceipt,
    preparedBootstrapImportAttemptInvalidationCancellation,
    preparedBootstrapImportAttemptInvalidationDisposition,
    commitBootstrapImportAttemptInvalidation,
    BootstrapImportFulfillmentDisposition (..),
    BootstrapImportFulfillmentProblem (..),
    PreparedBootstrapImportFulfillment,
    prepareBootstrapImportFulfillment,
    preparedBootstrapImportFulfillmentReceipt,
    preparedBootstrapImportFulfillmentCancellations,
    preparedBootstrapImportFulfillmentDisposition,
    commitBootstrapImportFulfillment,
    AlignmentStateInvariantProblem (..),
    validateAlignmentState,
  )
where

import Control.Monad (foldM)
import Data.ByteString qualified as ByteString
import Data.List (sort, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize.Put qualified as Serialize
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Alignment
  ( AlignmentMember,
    BootstrapEvidenceDigest,
    ContextClassGenerationId,
    FreshMemberBaseEvidence,
    MemberReadyEvidenceDigest,
    PhysicalPlacementRevisionVector,
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
    alignmentMemberHerald,
    alignmentMemberStoreIncarnation,
    contextClassGenerationIdBytes,
    deriveBootstrapEvidenceDigest,
    deriveContextClassGenerationId,
    freshMemberBaseDelta,
    freshMemberBaseRevision,
    freshMemberBaseStoreIncarnation,
    memberReadyEvidenceDigestBytes,
    physicalPlacementRevisionEntries,
    storeRevisionWord64,
  )
import Eclips.Domain.Context (contextClassMembers)
import Eclips.Domain.Identity
  ( DeltaId,
    HeraldEpoch,
    StoreIncarnationId,
    TopologyCutId,
    deltaIdBytes,
    storeIncarnationIdBytes,
  )
import Eclips.Domain.Membership (HeraldMembershipGenerationId)
import Eclips.Domain.Route (ReplicaStrength (Normal))
import Eclips.Domain.StructuralConsequence (StructuralConsequenceCause)
import Eclips.Domain.Topology (deriveTopologyCutId)
import Eclips.Herald.Alignment.Generation
  ( AlignmentGeneration,
    AlignmentGenerationPlan,
    AlignmentGenerationRelation,
    alignmentGenerationAnnouncer,
    alignmentGenerationContextClass,
    alignmentGenerationCut,
    alignmentGenerationId,
    alignmentGenerationPlanGenerations,
    alignmentGenerationPlanRelations,
    alignmentGenerationRelationDestination,
    alignmentGenerationRelationSource,
    alignmentGenerationRelationStrength,
  )
import Eclips.Herald.Alignment.Plan qualified as Plan
import Eclips.Herald.Alignment.Protocol
  ( AlignmentAttempt,
    AlignmentCancel,
    AlignmentCancelReason (..),
    AlignmentControl (..),
    AlignmentCutAccepted,
    AlignmentEvidenceSequence,
    AlignmentObligation,
    AlignmentObligationId,
    AlignmentObligationSequence,
    AlignmentProtocolProblem,
    AlignmentSourceProof (BootstrapProof),
    AlignmentSubscribe,
    AlignmentSubscriptionId,
    AlignmentSubscriptionSequence,
    ClassMemberReady,
    DestinationStore,
    HistoricalCertificate,
    alignmentAttempt,
    alignmentAttemptHistoricalCertificate,
    alignmentAttemptObligation,
    alignmentAttemptObligationId,
    alignmentAttemptSourceHerald,
    alignmentAttemptSourceStoreIncarnation,
    alignmentAttemptSubscriptionId,
    alignmentCancel,
    alignmentCutAcceptedGeneration,
    alignmentCutAcceptedHerald,
    alignmentCutAcceptedPlacementVector,
    alignmentCutAcceptedTopologyCut,
    alignmentCutAnnounceGeneration,
    alignmentEvidenceSequenceWord64,
    alignmentObligation,
    alignmentObligationCause,
    alignmentObligationDestinationGeneration,
    alignmentObligationDestinationStores,
    alignmentObligationFrozenContextPathStrength,
    alignmentObligationId,
    alignmentObligationIdDestinationHerald,
    alignmentObligationIdSequence,
    alignmentObligationIdValue,
    alignmentObligationSequenceWord64,
    alignmentObligationSortDefinitionOccurrenceId,
    alignmentObligationSortId,
    alignmentObligationSourceGeneration,
    alignmentSubscribe,
    alignmentSubscribeObligation,
    alignmentSubscribeSourceProof,
    alignmentSubscribeSourceStoreIncarnation,
    alignmentSubscribeSubscriptionId,
    alignmentSubscriptionId,
    alignmentSubscriptionIdDestinationHerald,
    alignmentSubscriptionIdSequence,
    alignmentSubscriptionSequenceWord64,
    classMemberReady,
    classMemberReadyEvidenceSequence,
    classMemberReadyGeneration,
    classMemberReadyPredecessorAndBasePrefixDigest,
    classMemberReadySetDigest,
    classMemberReadyStoreIncarnation,
    classMemberReadyStoreRevision,
    destinationStore,
    destinationStoreDelta,
    destinationStoreIncarnation,
    firstAlignmentEvidenceSequence,
    firstAlignmentObligationSequence,
    firstAlignmentSubscriptionSequence,
    historicalCertificateCanonicalBytes,
    historicalCertificateCertifiedStoreRevision,
    historicalCertificateClassGeneration,
    historicalCertificateCompletedPredecessorPrefixDigest,
    historicalCertificateExactMemberReadyDigest,
    historicalCertificateSourceStoreIncarnation,
    nextAlignmentEvidenceSequence,
    nextAlignmentObligationSequence,
    nextAlignmentSubscriptionSequence,
  )
import Eclips.Herald.Alignment.Protocol qualified as Protocol
import Eclips.Herald.Alignment.Transfer qualified as Transfer
import Eclips.Herald.Internal.Prepared
  ( Prepared,
    commitPrepared,
    prepareTransition,
    preparedOutput,
  )
import Eclips.Herald.Internal.WorkIndex qualified as WorkIndex
import Eclips.Herald.Structural.Debt
  ( SortOccurrence,
    StructuralConsequenceDebt,
    StructuralDebtKey,
    StructuralDebtSet,
    emptyStructuralDebtSet,
    normalizeStructuralDebts,
    sortOccurrence,
    sortOccurrenceDefinition,
    sortOccurrenceSortId,
    structuralConsequenceDebtKey,
    structuralDebtKeyCause,
    structuralDebtKeySort,
    structuralDebtSetEntries,
  )

-- | Frozen semantic source of one pre-certificate bootstrap import.  A
-- predecessor names a certified historical class; a fresh base names the exact
-- member incarnation and captured revision embedded in the new cut.
data BootstrapImportSource
  = BootstrapFromPredecessor ContextClassGenerationId
  | BootstrapFromFreshBase FreshMemberBaseEvidence
  deriving stock (Eq, Ord, Show)

data BootstrapImportKey = BootstrapImportKey
  { generation :: ContextClassGenerationId,
    destination :: DestinationStore,
    source :: BootstrapImportSource
  }
  deriving stock (Eq, Ord, Show)

bootstrapImportKeyGeneration ::
  BootstrapImportKey -> ContextClassGenerationId
bootstrapImportKeyGeneration key = key.generation

bootstrapImportKeyDestination :: BootstrapImportKey -> DestinationStore
bootstrapImportKeyDestination key = key.destination

bootstrapImportKeySource :: BootstrapImportKey -> BootstrapImportSource
bootstrapImportKeySource key = key.source

data BootstrapImport = BootstrapImport
  { key :: BootstrapImportKey,
    subscribeEnvelope :: AlignmentObligation
  }
  deriving stock (Eq, Show)

bootstrapImportKeyValue :: BootstrapImport -> BootstrapImportKey
bootstrapImportKeyValue bootstrapImport = bootstrapImport.key

bootstrapImportSubscribeEnvelope :: BootstrapImport -> AlignmentObligation
bootstrapImportSubscribeEnvelope bootstrapImport =
  bootstrapImport.subscribeEnvelope

data BootstrapImportAttempt = BootstrapImportAttempt
  { bootstrapImport :: BootstrapImport,
    sourceHerald :: HeraldEpoch,
    subscribe :: AlignmentSubscribe
  }
  deriving stock (Eq, Show)

bootstrapImportAttemptImport :: BootstrapImportAttempt -> BootstrapImport
bootstrapImportAttemptImport attempt = attempt.bootstrapImport

bootstrapImportAttemptSourceHerald :: BootstrapImportAttempt -> HeraldEpoch
bootstrapImportAttemptSourceHerald attempt = attempt.sourceHerald

bootstrapImportAttemptSubscribe :: BootstrapImportAttempt -> AlignmentSubscribe
bootstrapImportAttemptSubscribe attempt = attempt.subscribe

data BootstrapImportAttemptInvalidationReceipt
  = BootstrapImportAttemptInvalidationReceipt
      BootstrapImportAttempt
      (Set AlignmentCancelReason)
  deriving stock (Eq, Show)

bootstrapImportAttemptInvalidationAttempt ::
  BootstrapImportAttemptInvalidationReceipt -> BootstrapImportAttempt
bootstrapImportAttemptInvalidationAttempt
  (BootstrapImportAttemptInvalidationReceipt attempt _) = attempt

bootstrapImportAttemptInvalidationReason ::
  BootstrapImportAttemptInvalidationReceipt -> AlignmentCancelReason
bootstrapImportAttemptInvalidationReason
  (BootstrapImportAttemptInvalidationReceipt _ reasons) =
    dominantAlignmentCancelReason reasons

bootstrapImportAttemptInvalidationReasons ::
  BootstrapImportAttemptInvalidationReceipt -> Set AlignmentCancelReason
bootstrapImportAttemptInvalidationReasons
  (BootstrapImportAttemptInvalidationReceipt _ reasons) = reasons

data BootstrapImportFulfillmentReceipt
  = BootstrapImportFulfillmentReceipt
      BootstrapImportAttempt
      StoreRevision
  deriving stock (Eq, Show)

bootstrapImportFulfillmentAttempt ::
  BootstrapImportFulfillmentReceipt -> BootstrapImportAttempt
bootstrapImportFulfillmentAttempt
  (BootstrapImportFulfillmentReceipt attempt _) = attempt

bootstrapImportFulfillmentThrough ::
  BootstrapImportFulfillmentReceipt -> StoreRevision
bootstrapImportFulfillmentThrough
  (BootstrapImportFulfillmentReceipt _ through) = through

-- | Dependency-ordered generation evidence whose referenced topology,
-- placement revision, generation, or member may not yet be retained locally.
-- The peer binding is kept with the exact control so admission can be retried
-- against the same authenticated source rather than weakening provenance.
data PendingGenerationEvidence = PendingGenerationEvidence
  { remoteHerald :: HeraldEpoch,
    control :: AlignmentControl
  }
  deriving stock (Eq, Show)

data PendingGenerationEvidenceKey
  = PendingPlanAnnounce Plan.AlignmentPlanId
  | PendingCutAnnounce ContextClassGenerationId
  | PendingPlanAcceptance Plan.AlignmentPlanId HeraldEpoch
  | PendingCutAcceptance ContextClassGenerationId HeraldEpoch
  | PendingMemberReady ContextClassGenerationId StoreIncarnationId
  | PendingCertificate ContextClassGenerationId StoreIncarnationId
  deriving stock (Eq, Ord, Show)

pendingGenerationEvidenceRemoteHerald ::
  PendingGenerationEvidence -> HeraldEpoch
pendingGenerationEvidenceRemoteHerald evidence = evidence.remoteHerald

pendingGenerationEvidenceControl ::
  PendingGenerationEvidence -> AlignmentControl
pendingGenerationEvidenceControl evidence = evidence.control

-- | The supplier projection retained by established attempt problems and
-- fresh-base plan receipts.  Live owner exclusion uses the delta-qualified
-- 'UnavailableAlignmentSource' below.
data AlignmentSourceCoordinate
  = AlignmentSourceCoordinate HeraldEpoch StoreIncarnationId
  deriving stock (Eq, Ord, Show)

alignmentSourceCoordinate ::
  HeraldEpoch -> StoreIncarnationId -> AlignmentSourceCoordinate
alignmentSourceCoordinate = AlignmentSourceCoordinate

alignmentSourceCoordinateHerald :: AlignmentSourceCoordinate -> HeraldEpoch
alignmentSourceCoordinateHerald (AlignmentSourceCoordinate herald _) = herald

alignmentSourceCoordinateStoreIncarnation ::
  AlignmentSourceCoordinate -> StoreIncarnationId
alignmentSourceCoordinateStoreIncarnation (AlignmentSourceCoordinate _ store) = store

-- | One exact Store incarnation that checked structural placement has made
-- unavailable as an alignment source.  Unlike an attempt-qualified terminal
-- receipt, this coordinate exists even when loss is observed before any
-- obligation has selected a representative.
data UnavailableAlignmentSource
  = UnavailableAlignmentSource HeraldEpoch DeltaId StoreIncarnationId
  deriving stock (Eq, Ord, Show)

unavailableAlignmentSource ::
  HeraldEpoch -> DeltaId -> StoreIncarnationId -> UnavailableAlignmentSource
unavailableAlignmentSource = UnavailableAlignmentSource

unavailableAlignmentSourceHerald :: UnavailableAlignmentSource -> HeraldEpoch
unavailableAlignmentSourceHerald
  (UnavailableAlignmentSource herald _ _) = herald

unavailableAlignmentSourceDelta :: UnavailableAlignmentSource -> DeltaId
unavailableAlignmentSourceDelta
  (UnavailableAlignmentSource _ delta _) = delta

unavailableAlignmentSourceStoreIncarnation ::
  UnavailableAlignmentSource -> StoreIncarnationId
unavailableAlignmentSourceStoreIncarnation
  (UnavailableAlignmentSource _ _ store) = store

-- Keep the complete immutable identity, including its recovery attempt, in
-- every coordinate index rather than reconstructing a partial identity.
newtype AlignmentPlanCoordinate = AlignmentPlanCoordinate Plan.AlignmentPlanId
  deriving stock (Eq, Ord, Show)

alignmentPlanCoordinateId :: AlignmentPlanCoordinate -> Plan.AlignmentPlanId
alignmentPlanCoordinateId (AlignmentPlanCoordinate identifier) = identifier

alignmentPlanCoordinateSort :: AlignmentPlanCoordinate -> SortOccurrence
alignmentPlanCoordinateSort = Plan.alignmentPlanIdSort . alignmentPlanCoordinateId

alignmentPlanCoordinateTopologyCut :: AlignmentPlanCoordinate -> TopologyCutId
alignmentPlanCoordinateTopologyCut = Plan.alignmentPlanIdTopology . alignmentPlanCoordinateId

alignmentPlanCoordinatePlacementVector :: AlignmentPlanCoordinate -> PhysicalPlacementRevisionVector
alignmentPlanCoordinatePlacementVector = Plan.alignmentPlanIdPlacement . alignmentPlanCoordinateId

-- | A destination loss invalidates the complete placement-owned plan. A fresh
-- base loss does the same because that exact incarnation/revision is hashed
-- into every generation cut at the coordinate. Reset supersession retires
-- executable owners without claiming that any additional Store was lost.
data AlignmentPlanInvalidationCause
  = AlignmentPlanDestinationStoreLost
      ContextClassGenerationId
      DestinationStore
  | AlignmentPlanFreshBaseStoreLost
      ContextClassGenerationId
      FreshMemberBaseEvidence
      AlignmentSourceCoordinate
  | AlignmentPlanSuperseded Plan.AlignmentPlanId Plan.AlignmentPlanId
  deriving stock (Eq, Ord, Show)

data AlignmentPlanInvalidationReceipt
  = AlignmentPlanInvalidationReceipt
      AlignmentPlanCoordinate
      (Set AlignmentPlanInvalidationCause)
      (Set ContextClassGenerationId)
      (Map AlignmentObligationId AlignmentObligation)
      (Map BootstrapImportKey BootstrapImport)
  deriving stock (Eq, Show)

alignmentPlanInvalidationReceiptCoordinate ::
  AlignmentPlanInvalidationReceipt -> AlignmentPlanCoordinate
alignmentPlanInvalidationReceiptCoordinate
  (AlignmentPlanInvalidationReceipt coordinate _ _ _ _) = coordinate

alignmentPlanInvalidationReceiptCauses ::
  AlignmentPlanInvalidationReceipt -> Set AlignmentPlanInvalidationCause
alignmentPlanInvalidationReceiptCauses
  (AlignmentPlanInvalidationReceipt _ causes _ _ _) = causes

alignmentPlanInvalidationReceiptGenerations :: AlignmentPlanInvalidationReceipt -> Set ContextClassGenerationId
alignmentPlanInvalidationReceiptGenerations (AlignmentPlanInvalidationReceipt _ _ generations _ _) = generations

alignmentPlanInvalidationReceiptObligations ::
  AlignmentPlanInvalidationReceipt ->
  [(AlignmentObligationId, AlignmentObligation)]
alignmentPlanInvalidationReceiptObligations
  (AlignmentPlanInvalidationReceipt _ _ _ obligations _) =
    Map.toAscList obligations

alignmentPlanInvalidationReceiptBootstrapImports ::
  AlignmentPlanInvalidationReceipt ->
  [(BootstrapImportKey, BootstrapImport)]
alignmentPlanInvalidationReceiptBootstrapImports
  (AlignmentPlanInvalidationReceipt _ _ _ _ bootstrapImports) =
    Map.toAscList bootstrapImports

-- | Terminal receipt for one concrete attempt. Source loss preserves the
-- unsourced obligation. Destination loss or relation removal removes that
-- logical work; a later generation plan may derive new work independently.
data AlignmentAttemptInvalidationReceipt
  = AlignmentAttemptInvalidationReceipt
      AlignmentAttempt
      (Set AlignmentCancelReason)
  deriving stock (Eq, Show)

alignmentAttemptInvalidationAttempt ::
  AlignmentAttemptInvalidationReceipt -> AlignmentAttempt
alignmentAttemptInvalidationAttempt
  (AlignmentAttemptInvalidationReceipt attempt _) = attempt

alignmentAttemptInvalidationReason ::
  AlignmentAttemptInvalidationReceipt -> AlignmentCancelReason
alignmentAttemptInvalidationReason
  (AlignmentAttemptInvalidationReceipt _ reasons) =
    dominantAlignmentCancelReason reasons

alignmentAttemptInvalidationReasons ::
  AlignmentAttemptInvalidationReceipt -> Set AlignmentCancelReason
alignmentAttemptInvalidationReasons
  (AlignmentAttemptInvalidationReceipt _ reasons) = reasons

dominantAlignmentCancelReason :: Set AlignmentCancelReason -> AlignmentCancelReason
dominantAlignmentCancelReason reasons
  | Set.member AlignmentDestinationIncarnationLost reasons =
      AlignmentDestinationIncarnationLost
  | Set.member AlignmentRelationRemoved reasons = AlignmentRelationRemoved
  | otherwise = AlignmentSourceIncarnationLost

data State = State
  { structuralDebts :: StructuralDebtSet,
    liveStructuralDebts :: Map StructuralDebtKey StructuralConsequenceDebt,
    promotions :: Map AlignmentPromotionKey AlignmentPromotionReceipt,
    planLedger :: !Plan.AlignmentPlanLedger,
    currentPlans :: !(Map SortOccurrence Plan.AlignmentPlanId),
    planAcceptances :: !(Map (Plan.AlignmentPlanId, HeraldEpoch) Protocol.AlignmentPlanAccepted),
    planObsoleteReports :: !(Map (Plan.AlignmentPlanId, HeraldEpoch) Protocol.AlignmentPlanObsolete),
    planObsoleteSorts :: !(Set SortOccurrence),
    historicalCoverage :: Map AlignmentPromotionKey [ContextClassGenerationId],
    generations :: Map ContextClassGenerationId AlignmentGeneration,
    generationRelations :: Set AlignmentGenerationRelation,
    latestBySort :: Map SortOccurrence [ContextClassGenerationId],
    historicalRecipients :: Map HeraldEpoch (Word64, Set ContextClassGenerationId),
    pendingMemberProgress ::
      Map HeraldEpoch (Map (ContextClassGenerationId, DeltaId) AlignmentMember),
    pendingRouteCutoverGenerations :: Map HeraldEpoch (Set ContextClassGenerationId),
    nextObligationSequence :: AlignmentObligationSequence,
    obligations :: Map AlignmentObligationId AlignmentObligation,
    closingObligations :: Set AlignmentObligationId,
    closedObligations :: Map AlignmentObligationId AlignmentObligation,
    expectedInputOwners :: !(Map Transfer.InputClosureTarget (Set AlignmentObligationId)),
    inputClosureChangedTargets :: !(Set Transfer.InputClosureTarget),
    inputClosureInvalidatedGenerations :: !(Set ContextClassGenerationId),
    preparationReadinessChanges :: !(Set SortOccurrence),
    generationEvidenceWork :: !(WorkIndex.WorkIndex PendingGenerationEvidenceKey GenerationWorkDependency),
    generationQueryScopeChanged :: !Bool,
    generationPlanWork :: !(WorkIndex.WorkIndex SortOccurrence ()),
    liveDebtKeysBySort :: !(Map SortOccurrence (Set StructuralDebtKey)),
    generationPlanContext :: !(Maybe (HeraldMembershipGenerationId, Bool)),
    pendingLossCheck :: !Bool,
    bootstrapAttemptWork :: !(WorkIndex.WorkIndex BootstrapImportKey TransferWorkDependency),
    ordinaryAttemptWork :: !(WorkIndex.WorkIndex AlignmentObligationId TransferWorkDependency),
    bootstrapFulfillmentWork :: !(WorkIndex.WorkIndex BootstrapImportKey TransferWorkDependency),
    memberReadinessWork :: !(Map HeraldEpoch (WorkIndex.WorkIndex TransferMemberKey TransferWorkDependency)),
    memberCertificateWork :: !(Map HeraldEpoch (WorkIndex.WorkIndex TransferMemberKey TransferWorkDependency)),
    transferLocalHerald :: !(Maybe HeraldEpoch),
    transferPendingMemberIds :: !(Map HeraldEpoch (Set TransferMemberKey)),
    transferReadyMembersByStore :: !(Map StoreIncarnationId (Set TransferMemberLocation)),
    transferReadyMembersByTarget :: !(Map (ContextClassGenerationId, StoreIncarnationId) (Set TransferMemberLocation)),
    transferCertificateMembersByGeneration :: !(Map ContextClassGenerationId (Set TransferMemberLocation)),
    bootstrapImportsByMember :: !(Map (ContextClassGenerationId, DestinationStore) (Set BootstrapImportKey)),
    bootstrapFulfillmentsByMember :: !(Map (ContextClassGenerationId, DestinationStore) (Set BootstrapImportKey)),
    routeMarkerChanges :: !(Set ContextClassGenerationId),
    nextSubscriptionSequence :: AlignmentSubscriptionSequence,
    attempts :: Map AlignmentObligationId AlignmentAttempt,
    unavailableSources :: Set UnavailableAlignmentSource,
    attemptInvalidations ::
      Map AlignmentSubscriptionId AlignmentAttemptInvalidationReceipt,
    invalidatedGenerations :: !(Set ContextClassGenerationId),
    planInvalidations ::
      Map AlignmentPlanCoordinate AlignmentPlanInvalidationReceipt,
    bootstrapImports :: Map BootstrapImportKey BootstrapImport,
    bootstrapAttempts :: Map BootstrapImportKey BootstrapImportAttempt,
    bootstrapAttemptInvalidations ::
      Map AlignmentSubscriptionId BootstrapImportAttemptInvalidationReceipt,
    bootstrapFulfillments ::
      Map BootstrapImportKey BootstrapImportFulfillmentReceipt,
    pendingGenerationEvidence ::
      Map PendingGenerationEvidenceKey PendingGenerationEvidence,
    cutAcceptances ::
      Map (ContextClassGenerationId, HeraldEpoch) AlignmentCutAccepted,
    memberReadiness ::
      Map (ContextClassGenerationId, StoreIncarnationId) ClassMemberReady,
    localMemberReadiness ::
      Map (ContextClassGenerationId, StoreIncarnationId) ClassMemberReady,
    nextEvidenceSequence :: AlignmentEvidenceSequence,
    certificates ::
      Map (ContextClassGenerationId, StoreIncarnationId) HistoricalCertificate,
    transfer :: Transfer.State
  }
  deriving stock (Eq, Show)

emptyState :: State
emptyState =
  State
    { structuralDebts = emptyStructuralDebtSet,
      liveStructuralDebts = Map.empty,
      promotions = Map.empty,
      planLedger = Plan.emptyAlignmentPlanLedger,
      currentPlans = Map.empty,
      planAcceptances = Map.empty,
      planObsoleteReports = Map.empty,
      planObsoleteSorts = Set.empty,
      historicalCoverage = Map.empty,
      generations = Map.empty,
      generationRelations = Set.empty,
      latestBySort = Map.empty,
      historicalRecipients = Map.empty,
      pendingMemberProgress = Map.empty,
      pendingRouteCutoverGenerations = Map.empty,
      nextObligationSequence = firstAlignmentObligationSequence,
      obligations = Map.empty,
      closingObligations = Set.empty,
      closedObligations = Map.empty,
      expectedInputOwners = Map.empty,
      inputClosureChangedTargets = Set.empty,
      inputClosureInvalidatedGenerations = Set.empty,
      preparationReadinessChanges = Set.empty,
      generationEvidenceWork = WorkIndex.empty,
      generationQueryScopeChanged = False,
      generationPlanWork = WorkIndex.empty,
      liveDebtKeysBySort = Map.empty,
      generationPlanContext = Nothing,
      pendingLossCheck = True,
      bootstrapAttemptWork = WorkIndex.empty,
      ordinaryAttemptWork = WorkIndex.empty,
      bootstrapFulfillmentWork = WorkIndex.empty,
      memberReadinessWork = Map.empty,
      memberCertificateWork = Map.empty,
      transferLocalHerald = Nothing,
      transferPendingMemberIds = Map.empty,
      transferReadyMembersByStore = Map.empty,
      transferReadyMembersByTarget = Map.empty,
      transferCertificateMembersByGeneration = Map.empty,
      bootstrapImportsByMember = Map.empty,
      bootstrapFulfillmentsByMember = Map.empty,
      routeMarkerChanges = Set.empty,
      nextSubscriptionSequence = firstAlignmentSubscriptionSequence,
      attempts = Map.empty,
      unavailableSources = Set.empty,
      attemptInvalidations = Map.empty,
      invalidatedGenerations = Set.empty,
      planInvalidations = Map.empty,
      bootstrapImports = Map.empty,
      bootstrapAttempts = Map.empty,
      bootstrapAttemptInvalidations = Map.empty,
      bootstrapFulfillments = Map.empty,
      pendingGenerationEvidence = Map.empty,
      cutAcceptances = Map.empty,
      memberReadiness = Map.empty,
      localMemberReadiness = Map.empty,
      nextEvidenceSequence = firstAlignmentEvidenceSequence,
      certificates = Map.empty,
      transfer = Transfer.emptyState
    }

-- | Sort-scoped semantic readiness changes, independent of transfer closure
-- notifications. Remote acknowledgements and Store content progress are inert.
takePreparationReadinessChanges :: State -> (Set SortOccurrence, State)
takePreparationReadinessChanges state =
  let changes = state.preparationReadinessChanges
   in changes `seq` (changes, clearPreparationReadinessChanges state)

clearPreparationReadinessChanges :: State -> State
clearPreparationReadinessChanges state
  | Set.null state.preparationReadinessChanges = state
  | otherwise = state {preparationReadinessChanges = Set.empty}

notifyPreparationReadiness :: SortOccurrence -> State -> State
notifyPreparationReadiness affectedSort state = state {preparationReadinessChanges = Set.insert affectedSort state.preparationReadinessChanges}

-- | Seed the empty owner's semantic membership before any closure can register.
initialState :: HeraldMembershipGenerationId -> State
initialState membership = emptyState {transfer = Transfer.seedInputClosureMembership membership Transfer.emptyState}

-- | Independent loss notification; ordinary Eq observes it.
consumeLossCheck :: State -> (Bool, State)
consumeLossCheck state = let pending = state.pendingLossCheck in pending `seq` (pending, clearLossScheduling state)

wakeLossCheck :: State -> State
wakeLossCheck state = state {pendingLossCheck = True}
clearLossScheduling :: State -> State
clearLossScheduling state = state {pendingLossCheck = False}

takeRouteMarkerChanges :: State -> (Set ContextClassGenerationId, State)
takeRouteMarkerChanges state = let changes = state.routeMarkerChanges in changes `seq` (changes, clearRouteMarkerChanges state)
clearRouteMarkerChanges :: State -> State
clearRouteMarkerChanges state = state {routeMarkerChanges = Set.empty}

data GenerationWorkDependency
  = PlanRetained !Plan.AlignmentPlanId
  | GenerationRetained !ContextClassGenerationId
  | GenerationMemberReadiness !ContextClassGenerationId
  | GenerationPlanInputs
  deriving stock (Eq, Ord, Show)

generationEvidenceDependencies :: PendingGenerationEvidenceKey -> Set GenerationWorkDependency
generationEvidenceDependencies = \case
  PendingPlanAnnounce identifier -> Set.fromList [PlanRetained identifier, GenerationPlanInputs]
  PendingPlanAcceptance identifier _ -> Set.singleton (PlanRetained identifier)
  PendingCutAnnounce generation -> Set.fromList [GenerationRetained generation, GenerationPlanInputs]
  PendingCutAcceptance generation _ -> Set.singleton (GenerationRetained generation)
  PendingMemberReady generation _ -> Set.singleton (GenerationRetained generation)
  PendingCertificate generation _ -> Set.fromList [GenerationRetained generation, GenerationMemberReadiness generation]

generationEvidenceWorkIds :: State -> Set PendingGenerationEvidenceKey
generationEvidenceWorkIds state = WorkIndex.registeredKeys state.generationEvidenceWork
pendingGenerationEvidenceWork :: State -> Set PendingGenerationEvidenceKey
pendingGenerationEvidenceWork state = WorkIndex.pendingKeys state.generationEvidenceWork
lookupPendingGenerationEvidence :: PendingGenerationEvidenceKey -> State -> Maybe PendingGenerationEvidence
lookupPendingGenerationEvidence key state = Map.lookup key state.pendingGenerationEvidence
nextGenerationEvidenceWork :: Set PendingGenerationEvidenceKey -> Maybe PendingGenerationEvidenceKey -> State -> Maybe PendingGenerationEvidenceKey
nextGenerationEvidenceWork inventory cursor state = WorkIndex.nextWork inventory cursor state.generationEvidenceWork
consumeGenerationEvidenceWork :: PendingGenerationEvidenceKey -> State -> State
consumeGenerationEvidenceWork key state = state {generationEvidenceWork = WorkIndex.consumeWork key state.generationEvidenceWork}

-- | Removing a pending announcement can release its placement-vector memo
-- even while unrelated evidence remains asleep. This event carries no semantic
-- work and is consumed after trimming; insertions already start evidence work.
takeGenerationQueryScopeChange :: State -> (Bool, State)
takeGenerationQueryScopeChange state =
  let changed = state.generationQueryScopeChanged
   in changed `seq` (changed, if changed then state {generationQueryScopeChanged = False} else state)

-- Announcement keys precede the other evidence families in canonical order.
-- The minimum primary key is therefore a complete, logarithmic presence test.
hasPendingGenerationAnnouncements :: State -> Bool
hasPendingGenerationAnnouncements state = maybe False (pendingEvidenceUsesPlacement . fst) (Map.lookupMin state.pendingGenerationEvidence)

pendingEvidenceUsesPlacement :: PendingGenerationEvidenceKey -> Bool
pendingEvidenceUsesPlacement (PendingPlanAnnounce _) = True
pendingEvidenceUsesPlacement (PendingCutAnnounce _) = True
pendingEvidenceUsesPlacement _ = False

notifyGenerationEvidence :: Set GenerationWorkDependency -> State -> State
notifyGenerationEvidence dependencies state = state {generationEvidenceWork = snd (WorkIndex.notifyDependencies dependencies state.generationEvidenceWork)}

generationPlanWorkIds :: State -> Set SortOccurrence
generationPlanWorkIds state = WorkIndex.registeredKeys state.generationPlanWork
pendingGenerationPlanWork :: State -> Set SortOccurrence
pendingGenerationPlanWork state = WorkIndex.pendingKeys state.generationPlanWork
consumeGenerationPlanWork :: Set SortOccurrence -> State -> State
consumeGenerationPlanWork sorts state = state {generationPlanWork = Set.foldl' (flip WorkIndex.consumeWork) state.generationPlanWork sorts}
wakeGenerationPlanInputs :: State -> State
wakeGenerationPlanInputs state = wakeGenerationPlanSorts (generationPlanWorkIds state) state
wakeGenerationPlanSorts :: Set SortOccurrence -> State -> State
wakeGenerationPlanSorts sorts state =
  (notifyGenerationEvidence (Set.singleton GenerationPlanInputs) state)
    { generationPlanWork = WorkIndex.markDirty sorts state.generationPlanWork
    }

-- | Update only touched live-debt coordinates after authoritative mutation.
-- New evidence on an unchanged live key cannot change group eligibility.
refreshLiveDebtScheduling :: Set StructuralDebtKey -> State -> State
refreshLiveDebtScheduling supplied state
  | Set.null changedSorts = state
  | otherwise = wakeGenerationPlanSorts changedSorts state {liveDebtKeysBySort = indexed, generationPlanWork = refreshed}
  where
    (indexed, changedSorts) = Set.foldl' update (state.liveDebtKeysBySort, Set.empty) supplied
    update (retained, changed) key =
      let affectedSort = structuralDebtKeySort key
          previous = Map.findWithDefault Set.empty affectedSort retained
          present = Map.member key state.liveStructuralDebts
       in if Set.member key previous == present
            then (retained, changed)
            else
              ( if present
                  then Map.insert affectedSort (Set.insert key previous) retained
                  else Map.update (nonEmptySet . Set.delete key) affectedSort retained,
                Set.insert affectedSort changed
              )
    refreshed = Set.foldl' refresh state.generationPlanWork changedSorts
    refresh work affectedSort
      | Map.notMember affectedSort indexed && Set.notMember affectedSort state.planObsoleteSorts = WorkIndex.removeWork affectedSort work
      | Set.member affectedSort (WorkIndex.registeredKeys work) = WorkIndex.markDirty (Set.singleton affectedSort) work
      | otherwise = WorkIndex.registerWork affectedSort Set.empty work

liveAlignmentDebtCoordinatesAtSorts :: Set SortOccurrence -> State -> [(StructuralConsequenceCause, SortOccurrence)]
liveAlignmentDebtCoordinatesAtSorts sorts state =
  Set.toAscList
    ( Set.fromList
        [ (structuralDebtKeyCause key, affectedSort)
        | affectedSort <- Set.toAscList sorts,
          key <- Set.toAscList (Map.findWithDefault Set.empty affectedSort state.liveDebtKeysBySort)
        ]
    )

-- | Context is a compact semantic stamp, not owner equality or a counter.
synchronizeGenerationPlanContext :: HeraldMembershipGenerationId -> Bool -> State -> State
synchronizeGenerationPlanContext membership handoff state
  | state.generationPlanContext == Just (membership, handoff) = state
  | otherwise = membership `seq` handoff `seq` (wakeGenerationPlanInputs state) {generationPlanContext = Just (membership, handoff)}

clearGenerationScheduling :: State -> State
clearGenerationScheduling state =
  state
    { generationEvidenceWork = WorkIndex.clearPendingWork state.generationEvidenceWork,
      generationPlanWork = WorkIndex.clearPendingWork state.generationPlanWork,
      generationPlanContext = Nothing,
      generationQueryScopeChanged = False
    }

generationSchedulingValid :: State -> Bool
generationSchedulingValid state =
  WorkIndex.valid state.generationEvidenceWork
    && WorkIndex.valid state.generationPlanWork
    && generationEvidenceWorkIds state == Map.keysSet state.pendingGenerationEvidence
    && all (\key -> WorkIndex.dependenciesFor key state.generationEvidenceWork == Just (generationEvidenceDependencies key)) (Map.keys state.pendingGenerationEvidence)
    && state.liveDebtKeysBySort == Map.fromListWith Set.union [(structuralDebtKeySort key, Set.singleton key) | key <- Map.keys state.liveStructuralDebts]
    && state.planObsoleteSorts == Set.fromList [Plan.alignmentPlanIdSort identifier | (identifier, _) <- Map.keys state.planObsoleteReports]
    && generationPlanWorkIds state == Set.union (Map.keysSet state.liveDebtKeysBySort) state.planObsoleteSorts

type TransferMemberKey = (ContextClassGenerationId, DeltaId)
type TransferMemberLocation = (HeraldEpoch, ContextClassGenerationId, DeltaId)

data TransferWorkStage key where
  BootstrapAttemptStage :: TransferWorkStage BootstrapImportKey
  OrdinaryAttemptStage :: TransferWorkStage AlignmentObligationId
  BootstrapFulfillmentStage :: TransferWorkStage BootstrapImportKey
  MemberReadinessStage :: !HeraldEpoch -> TransferWorkStage TransferMemberKey
  MemberCertificateStage :: !HeraldEpoch -> TransferWorkStage TransferMemberKey

data TransferWorkDependency
  = TransferGenerationEvidence !ContextClassGenerationId
  | TransferGenerationReadiness !ContextClassGenerationId
  | TransferSourceUnavailable !HeraldEpoch !DeltaId !StoreIncarnationId
  | TransferDestinationProgress !AlignmentSubscriptionId
  deriving stock (Eq, Ord, Show)

transferWorkIndex :: TransferWorkStage key -> State -> WorkIndex.WorkIndex key TransferWorkDependency
transferWorkIndex stage state = case stage of
  BootstrapAttemptStage -> state.bootstrapAttemptWork
  OrdinaryAttemptStage -> state.ordinaryAttemptWork
  BootstrapFulfillmentStage -> state.bootstrapFulfillmentWork
  MemberReadinessStage herald -> Map.findWithDefault WorkIndex.empty herald state.memberReadinessWork
  MemberCertificateStage herald -> Map.findWithDefault WorkIndex.empty herald state.memberCertificateWork

putTransferWorkIndex :: TransferWorkStage key -> WorkIndex.WorkIndex key TransferWorkDependency -> State -> State
putTransferWorkIndex stage index state = case stage of
  BootstrapAttemptStage -> state {bootstrapAttemptWork = index}
  OrdinaryAttemptStage -> state {ordinaryAttemptWork = index}
  BootstrapFulfillmentStage -> state {bootstrapFulfillmentWork = index}
  MemberReadinessStage herald -> state {memberReadinessWork = retainIndex herald index state.memberReadinessWork}
  MemberCertificateStage herald -> state {memberCertificateWork = retainIndex herald index state.memberCertificateWork}
  where
    retainIndex herald retained indices
      | Set.null (WorkIndex.registeredKeys retained) = Map.delete herald indices
      | otherwise = herald `seq` Map.insert herald retained indices

transferWorkIds :: TransferWorkStage key -> State -> Set key
transferWorkIds stage = WorkIndex.registeredKeys . transferWorkIndex stage
pendingTransferWork :: TransferWorkStage key -> State -> Set key
pendingTransferWork stage = WorkIndex.pendingKeys . transferWorkIndex stage
nextTransferWork :: (Ord key) => TransferWorkStage key -> Set key -> Maybe key -> State -> Maybe key
nextTransferWork stage inventory cursor = WorkIndex.nextWork inventory cursor . transferWorkIndex stage
consumeTransferWork :: (Ord key) => TransferWorkStage key -> key -> State -> State
consumeTransferWork stage key state = putTransferWorkIndex stage (WorkIndex.consumeWork key (transferWorkIndex stage state)) state

clearTransferWork :: State -> State
clearTransferWork state =
  state
    { bootstrapAttemptWork = WorkIndex.clearPendingWork state.bootstrapAttemptWork,
      ordinaryAttemptWork = WorkIndex.clearPendingWork state.ordinaryAttemptWork,
      bootstrapFulfillmentWork = WorkIndex.clearPendingWork state.bootstrapFulfillmentWork,
      memberReadinessWork = Map.map WorkIndex.clearPendingWork state.memberReadinessWork,
      memberCertificateWork = Map.map WorkIndex.clearPendingWork state.memberCertificateWork
    }

-- | One-time local ownership initialization. Startup seeds the empty owner;
-- synthetic composed fixtures may seed retained local candidates on first use.
transferMemberWorkHerald :: State -> Maybe HeraldEpoch
transferMemberWorkHerald state = state.transferLocalHerald

initializeTransferMemberWork :: HeraldEpoch -> State -> State
initializeTransferMemberWork local state = case state.transferLocalHerald of
  Just retained
    | retained == local -> state
    | otherwise -> error "Alignment transfer local owner changed"
  Nothing ->
    local
      `seq` Map.foldlWithKey'
        (\retained (generation, _) member -> synchronizeTransferMember generation member retained)
        state {transferLocalHerald = Just local}
        (Map.findWithDefault Map.empty local state.pendingMemberProgress)

pendingTransferMemberIds :: HeraldEpoch -> State -> Set TransferMemberKey
pendingTransferMemberIds herald state = Map.findWithDefault Set.empty herald state.transferPendingMemberIds

lookupPendingAlignmentMember :: HeraldEpoch -> TransferMemberKey -> State -> Maybe (AlignmentGeneration, AlignmentMember)
lookupPendingAlignmentMember herald key@(generation, _) state = do
  member <- Map.lookup herald state.pendingMemberProgress >>= Map.lookup key
  retained <- Map.lookup generation state.generations
  pure (retained, member)

lookupBootstrapImport :: BootstrapImportKey -> State -> Maybe BootstrapImport
lookupBootstrapImport key state = Map.lookup key state.bootstrapImports

bootstrapImportsAtMember :: ContextClassGenerationId -> DestinationStore -> State -> [BootstrapImport]
bootstrapImportsAtMember generation destination state =
  [retained | key <- Set.toAscList (Map.findWithDefault Set.empty (generation, destination) state.bootstrapImportsByMember), Just retained <- [Map.lookup key state.bootstrapImports]]
bootstrapFulfillmentsAtMember :: ContextClassGenerationId -> DestinationStore -> State -> [BootstrapImportFulfillmentReceipt]
bootstrapFulfillmentsAtMember generation destination state =
  [retained | key <- Set.toAscList (Map.findWithDefault Set.empty (generation, destination) state.bootstrapFulfillmentsByMember), Just retained <- [Map.lookup key state.bootstrapFulfillments]]
alignmentAttemptsAtMember :: ContextClassGenerationId -> DestinationStore -> State -> [AlignmentAttempt]
alignmentAttemptsAtMember generation destination state =
  [attempt | owner <- Set.toAscList (requiredPredecessorInputOwnersAt (generation, destinationStoreIncarnation destination) state), Just attempt <- [Map.lookup owner state.attempts], destination `elem` NonEmpty.toList (alignmentObligationDestinationStores (alignmentAttemptObligation attempt))]
activeAlignmentObligationCountAtMember :: ContextClassGenerationId -> DestinationStore -> State -> Int
activeAlignmentObligationCountAtMember generation destination state =
  length [() | owner <- Set.toAscList (requiredPredecessorInputOwnersAt (generation, destinationStoreIncarnation destination) state), Just obligation <- [Map.lookup owner state.obligations], destination `elem` NonEmpty.toList (alignmentObligationDestinationStores obligation)]

registerTransferObligation :: AlignmentObligation -> State -> State
registerTransferObligation obligation state =
  wakeTransferObligationMembers
    obligation
    state
      { ordinaryAttemptWork = WorkIndex.registerWork (alignmentObligationIdValue obligation) (ordinaryAttemptDependencies obligation state) state.ordinaryAttemptWork
      }
removeTransferObligation :: AlignmentObligation -> State -> State
removeTransferObligation obligation state =
  wakeTransferObligationMembers
    obligation
    state
      { ordinaryAttemptWork = WorkIndex.removeWork (alignmentObligationIdValue obligation) state.ordinaryAttemptWork
      }
wakeTransferObligationMembers :: AlignmentObligation -> State -> State
wakeTransferObligationMembers obligation state =
  foldl'
    (\retained destination -> wakeTransferMemberTarget (alignmentObligationDestinationGeneration obligation) (destinationStoreIncarnation destination) retained)
    state
    (NonEmpty.toList (alignmentObligationDestinationStores obligation))

registerTransferBootstrapImport :: BootstrapImportKey -> State -> State
registerTransferBootstrapImport key state =
  wakeTransferBootstrapMember
    key
    state
      { bootstrapAttemptWork = WorkIndex.registerWork key (bootstrapAttemptDependencies key state) state.bootstrapAttemptWork,
        bootstrapImportsByMember = insertTransferBucket (bootstrapMemberKey key) key state.bootstrapImportsByMember
      }
removeTransferBootstrapImport :: BootstrapImportKey -> State -> State
removeTransferBootstrapImport key state =
  wakeTransferBootstrapMember
    key
    state
      { bootstrapAttemptWork = WorkIndex.removeWork key state.bootstrapAttemptWork,
        bootstrapImportsByMember = deleteTransferBucket (bootstrapMemberKey key) key state.bootstrapImportsByMember
      }
registerTransferBootstrapAttempt :: BootstrapImportKey -> BootstrapImportAttempt -> State -> State
registerTransferBootstrapAttempt key attempt state =
  wakeTransferBootstrapMember
    key
    state
      { bootstrapFulfillmentWork = WorkIndex.registerWork key (Set.singleton (TransferDestinationProgress (alignmentSubscribeSubscriptionId attempt.subscribe))) state.bootstrapFulfillmentWork
      }
removeTransferBootstrapAttempt :: BootstrapImportKey -> State -> State
removeTransferBootstrapAttempt key state =
  wakeTransferBootstrapMember
    key
    state
      { bootstrapFulfillmentWork = WorkIndex.removeWork key state.bootstrapFulfillmentWork
      }
retainTransferBootstrapFulfillment :: BootstrapImportKey -> State -> State
retainTransferBootstrapFulfillment key state =
  wakeTransferBootstrapMember
    key
    state
      { bootstrapFulfillmentsByMember = insertTransferBucket (bootstrapMemberKey key) key state.bootstrapFulfillmentsByMember
      }
wakeTransferBootstrapMember :: BootstrapImportKey -> State -> State
wakeTransferBootstrapMember key = wakeTransferMemberTarget key.generation (destinationStoreIncarnation key.destination)
bootstrapMemberKey :: BootstrapImportKey -> (ContextClassGenerationId, DestinationStore)
bootstrapMemberKey key =
  let generation = key.generation; destination = key.destination
   in generation `seq` destination `seq` (generation, destination)

-- Notify only retained readiness candidates at the exact target; those already
-- ready leave this relation even while waiting for their own certificate.
wakeTransferMemberTarget :: ContextClassGenerationId -> StoreIncarnationId -> State -> State
wakeTransferMemberTarget generation store state =
  Set.foldl'
    (flip dirtyTransferMember)
    state
    (Map.findWithDefault Set.empty (generation, store) state.transferReadyMembersByTarget)
dirtyTransferMember :: TransferMemberLocation -> State -> State
dirtyTransferMember (herald, generation, delta) state =
  let stage = MemberReadinessStage herald
   in putTransferWorkIndex stage (WorkIndex.markDirty (Set.singleton (generation, delta)) (transferWorkIndex stage state)) state

wakeMemberStoreChanges :: Set StoreIncarnationId -> State -> State
wakeMemberStoreChanges stores state = Set.foldl' (flip dirtyTransferMember) state locations
  where
    locations = Set.foldl' (\found store -> found <> Map.findWithDefault Set.empty store state.transferReadyMembersByStore) Set.empty stores

wakeDestinationProgress :: Set AlignmentSubscriptionId -> State -> State
wakeDestinationProgress subscriptions state = Set.foldl' wakeDestination withFulfillments subscriptions
  where
    dependencies = Set.map TransferDestinationProgress subscriptions
    withFulfillments = state {bootstrapFulfillmentWork = snd (WorkIndex.notifyDependencies dependencies state.bootstrapFulfillmentWork)}
    wakeDestination retained identifier = case Transfer.lookupDestinationSubscription identifier retained.transfer of
      Nothing -> retained
      Just destination -> wakeTransferObligationMembers (alignmentSubscribeObligation (Transfer.destinationSubscriptionSubscribe destination)) retained

-- Call after the old pending-member index has been updated; its absence is the
-- authoritative old retirement predicate, including historical admission paths.
synchronizeTransferMember :: ContextClassGenerationId -> AlignmentMember -> State -> State
synchronizeTransferMember generation member state
  | state.transferLocalHerald /= Just (alignmentMemberHerald member) = state
  | otherwise = synchronizeLocalTransferMember generation member state

synchronizeLocalTransferMember :: ContextClassGenerationId -> AlignmentMember -> State -> State
synchronizeLocalTransferMember generation member state =
  let herald = alignmentMemberHerald member
      delta = alignmentMemberDelta member
      store = alignmentMemberStoreIncarnation member
      key = generation `seq` delta `seq` (generation, delta)
      location = herald `seq` generation `seq` delta `seq` (herald, generation, delta)
      target = generation `seq` store `seq` (generation, store)
      retained = Map.member key (Map.findWithDefault Map.empty herald state.pendingMemberProgress)
      needsReady = retained && Map.notMember (generation, store) state.localMemberReadiness
      needsCertificate = retained && Map.notMember (generation, store) state.certificates
      readyStage = MemberReadinessStage herald
      certificateStage = MemberCertificateStage herald
      readyIndex = transferWorkIndex readyStage state
      certificateIndex = transferWorkIndex certificateStage state
      wasReadyWork = Set.member key (WorkIndex.registeredKeys readyIndex)
      wasCertificateWork = Set.member key (WorkIndex.registeredKeys certificateIndex)
      readiness =
        if needsReady
          then if wasReadyWork then readyIndex else WorkIndex.registerWork key Set.empty readyIndex
          else WorkIndex.removeWork key readyIndex
      certificate =
        if needsCertificate
          then if wasCertificateWork then certificateIndex else WorkIndex.registerWork key (Set.singleton (TransferGenerationReadiness generation)) certificateIndex
          else WorkIndex.removeWork key certificateIndex
      indexed = putTransferWorkIndex certificateStage certificate (putTransferWorkIndex readyStage readiness state)
   in indexed
        { transferPendingMemberIds = (if retained then insertTransferBucket else deleteTransferBucket) herald key indexed.transferPendingMemberIds,
          transferReadyMembersByStore = (if needsReady then insertTransferBucket else deleteTransferBucket) store location indexed.transferReadyMembersByStore,
          transferReadyMembersByTarget = (if needsReady then insertTransferBucket else deleteTransferBucket) target location indexed.transferReadyMembersByTarget,
          transferCertificateMembersByGeneration = (if needsCertificate then insertTransferBucket else deleteTransferBucket) generation location indexed.transferCertificateMembersByGeneration
        }

notifyTransferGenerationEvidence :: ContextClassGenerationId -> State -> State
notifyTransferGenerationEvidence generation state =
  let dependencies = Set.singleton (TransferGenerationEvidence generation)
   in state
        { bootstrapAttemptWork = snd (WorkIndex.notifyDependencies dependencies state.bootstrapAttemptWork),
          ordinaryAttemptWork = snd (WorkIndex.notifyDependencies dependencies state.ordinaryAttemptWork)
        }
notifyTransferGenerationReadiness :: ContextClassGenerationId -> State -> State
notifyTransferGenerationReadiness generation state =
  Set.foldl'
    wake
    state
    (Map.findWithDefault Set.empty generation state.transferCertificateMembersByGeneration)
  where
    wake retained (herald, retainedGeneration, delta) =
      let stage = MemberCertificateStage herald
       in putTransferWorkIndex stage (WorkIndex.markDirty (Set.singleton (retainedGeneration, delta)) (transferWorkIndex stage retained)) retained
notifyTransferSourceUnavailable :: UnavailableAlignmentSource -> State -> State
notifyTransferSourceUnavailable source state =
  let dependencies = Set.singleton (sourceDependency source)
   in state
        { bootstrapAttemptWork = snd (WorkIndex.notifyDependencies dependencies state.bootstrapAttemptWork),
          ordinaryAttemptWork = snd (WorkIndex.notifyDependencies dependencies state.ordinaryAttemptWork)
        }

-- Only generation creation can expand immutable source members after initially
-- absent-generation registration. Acceptance/certificate messages need only notify.
refreshTransferGenerationDependencies :: ContextClassGenerationId -> State -> State
refreshTransferGenerationDependencies generation state =
  let dependency = Set.singleton (TransferGenerationEvidence generation)
      (ordinary, ordinaryIndex) = WorkIndex.notifyDependencies dependency state.ordinaryAttemptWork
      (bootstrap, bootstrapIndex) = WorkIndex.notifyDependencies dependency state.bootstrapAttemptWork
      refreshOrdinary index identifier = case Map.lookup identifier state.obligations of
        Nothing -> index
        Just obligation -> WorkIndex.replaceDependencies identifier (ordinaryAttemptDependencies obligation state) index
      refreshBootstrap index key = WorkIndex.replaceDependencies key (bootstrapAttemptDependencies key state) index
   in state
        { ordinaryAttemptWork = Set.foldl' refreshOrdinary ordinaryIndex ordinary,
          bootstrapAttemptWork = Set.foldl' refreshBootstrap bootstrapIndex bootstrap
        }

insertTransferBucket :: (Ord key, Ord value) => key -> value -> Map key (Set value) -> Map key (Set value)
insertTransferBucket key value retained = key `seq` value `seq` Map.insertWith Set.union key (Set.singleton value) retained
deleteTransferBucket :: (Ord key, Ord value) => key -> value -> Map key (Set value) -> Map key (Set value)
deleteTransferBucket key value = Map.update remove key
  where
    remove values = let remaining = Set.delete value values in if Set.null remaining then Nothing else Just remaining

-- Source selectors depend on immutable generation members, acceptance and
-- certificates plus each candidate's exact unavailable coordinate.
ordinaryAttemptDependencies :: AlignmentObligation -> State -> Set TransferWorkDependency
ordinaryAttemptDependencies obligation state =
  Set.fromList
    [ TransferGenerationEvidence (alignmentObligationDestinationGeneration obligation),
      TransferGenerationEvidence source
    ]
    <> sourceMemberDependencies source state
  where
    source = alignmentObligationSourceGeneration obligation

bootstrapAttemptDependencies :: BootstrapImportKey -> State -> Set TransferWorkDependency
bootstrapAttemptDependencies key state =
  Set.singleton (TransferGenerationEvidence key.generation)
    <> case key.source of
      BootstrapFromPredecessor predecessor ->
        Set.singleton (TransferGenerationEvidence predecessor)
          <> sourceMemberDependencies predecessor state
      BootstrapFromFreshBase _ ->
        case freshBaseImportUnavailableSourceCoordinate key state of
          Nothing -> Set.empty
          Just coordinate -> Set.singleton (sourceDependency coordinate)

sourceMemberDependencies :: ContextClassGenerationId -> State -> Set TransferWorkDependency
sourceMemberDependencies generation state = case Map.lookup generation state.generations of
  Nothing -> Set.empty
  Just retained ->
    Set.fromList
      [ TransferSourceUnavailable
          (alignmentMemberHerald member)
          (alignmentMemberDelta member)
          (alignmentMemberStoreIncarnation member)
      | member <- NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut retained))
      ]

sourceDependency :: UnavailableAlignmentSource -> TransferWorkDependency
sourceDependency coordinate =
  TransferSourceUnavailable
    (unavailableAlignmentSourceHerald coordinate)
    (unavailableAlignmentSourceDelta coordinate)
    (unavailableAlignmentSourceStoreIncarnation coordinate)

-- Called after the authoritative generation and candidate maps are installed.
finishGenerationCreation :: [AlignmentGeneration] -> State -> State
finishGenerationCreation supplied state = foldl' install state supplied
  where
    install retained generation =
      let identifier = alignmentGenerationId generation
       in foldl'
            (\current member -> synchronizeTransferMember identifier member current)
            (refreshTransferGenerationDependencies identifier retained)
            (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)))

finishPromotionWork :: [AlignmentGeneration] -> [AlignmentObligation] -> [BootstrapImport] -> State -> State -> State
finishPromotionWork generations owners imports before after =
  foldl'
    (\state entry -> registerTransferBootstrapImport (bootstrapImportKeyValue entry) state)
    ( foldl'
        (flip registerTransferObligation)
        (finishGenerationCreation [generation | generation <- generations, Map.notMember (alignmentGenerationId generation) before.generations] after)
        owners
    )
    imports

synchronizeMemberEvidence :: ContextClassGenerationId -> StoreIncarnationId -> State -> State
synchronizeMemberEvidence identifier store state = case Map.lookup identifier state.generations of
  Nothing -> state
  Just generation ->
    foldl'
      (\retained member -> synchronizeTransferMember identifier member retained)
      state
      [member | member <- NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)), alignmentMemberStoreIncarnation member == store]

synchronizeRemovedGenerationMembers :: Set ContextClassGenerationId -> State -> State
synchronizeRemovedGenerationMembers identifiers state = Set.foldl' update state identifiers
  where
    update retained identifier = case Map.lookup identifier retained.generations of
      Nothing -> retained
      Just generation ->
        foldl'
          (\next member -> synchronizeTransferMember identifier member next)
          retained
          (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)))

notifyNewUnavailableCandidates :: Set UnavailableAlignmentSource -> State -> State -> State
notifyNewUnavailableCandidates candidates before after = Set.foldl' notify after candidates
  where
    notify retained source
      | Set.member source after.unavailableSources && Set.notMember source before.unavailableSources = wakeLossCheck (notifyTransferSourceUnavailable source retained)
      | otherwise = retained

finishOrdinaryAttemptInvalidation :: AlignmentAttempt -> State -> State -> State
finishOrdinaryAttemptInvalidation attempt before after =
  notifyNewUnavailableCandidates (maybe Set.empty Set.singleton (alignmentAttemptUnavailableSourceCoordinate attempt before)) before
    $ if Map.member identifier after.obligations
      then wakeTransferObligationMembers obligation after {ordinaryAttemptWork = WorkIndex.markDirty (Set.singleton identifier) after.ordinaryAttemptWork}
      else wakeLossCheck (removeTransferObligation obligation after)
  where
    obligation = alignmentAttemptObligation attempt
    identifier = alignmentAttemptObligationId attempt

finishBootstrapAttemptInvalidation :: BootstrapImportAttempt -> State -> State -> State
finishBootstrapAttemptInvalidation attempt before after =
  notifyNewUnavailableCandidates (maybe Set.empty Set.singleton (bootstrapImportAttemptUnavailableSourceCoordinate attempt before)) before
    $ removeTransferBootstrapAttempt key
    $ if Map.member key after.bootstrapImports
      then after {bootstrapAttemptWork = WorkIndex.markDirty (Set.singleton key) after.bootstrapAttemptWork}
      else wakeLossCheck (removeTransferBootstrapImport key after)
  where
    key = bootstrapImportKeyValue attempt.bootstrapImport

finishPlanTransferInvalidation :: Set AlignmentPlanInvalidationCause -> [AlignmentObligation] -> [BootstrapImportKey] -> State -> State -> State
finishPlanTransferInvalidation causes owners imports before after =
  notifyNewUnavailableCandidates (retainPlanUnavailableSources causes Set.empty) before
    $ foldl'
      (\state key -> removeTransferBootstrapAttempt key (removeTransferBootstrapImport key state))
      (foldl' (flip removeTransferObligation) after owners)
      imports

-- | Independent reconstruction from authoritative owner maps. Used by the
-- invariant/property boundary, never to discover normal operational changes.
transferSchedulingValid :: State -> Bool
transferSchedulingValid state =
  and
    [ WorkIndex.valid state.bootstrapAttemptWork,
      WorkIndex.valid state.ordinaryAttemptWork,
      WorkIndex.valid state.bootstrapFulfillmentWork,
      all WorkIndex.valid (Map.elems state.memberReadinessWork),
      all WorkIndex.valid (Map.elems state.memberCertificateWork),
      WorkIndex.registeredKeys state.bootstrapAttemptWork == Map.keysSet state.bootstrapImports,
      WorkIndex.registeredKeys state.ordinaryAttemptWork == Map.keysSet state.obligations,
      WorkIndex.registeredKeys state.bootstrapFulfillmentWork == Map.keysSet state.bootstrapAttempts,
      Map.map WorkIndex.registeredKeys state.memberReadinessWork == readyKeys,
      Map.map WorkIndex.registeredKeys state.memberCertificateWork == certificateKeys,
      state.transferPendingMemberIds == pendingKeys,
      state.transferReadyMembersByStore == Map.fromListWith Set.union [(store, Set.singleton location) | (_, store, location) <- readyRows],
      state.transferReadyMembersByTarget == Map.fromListWith Set.union [((generation, store), Set.singleton location) | (generation, store, location) <- readyRows],
      state.transferCertificateMembersByGeneration == Map.fromListWith Set.union [(generation, Set.singleton location) | (generation, _, location) <- certificateRows],
      state.bootstrapImportsByMember == Map.fromListWith Set.union [(bootstrapMemberKey key, Set.singleton key) | key <- Map.keys state.bootstrapImports],
      state.bootstrapFulfillmentsByMember == Map.fromListWith Set.union [(bootstrapMemberKey key, Set.singleton key) | key <- Map.keys state.bootstrapFulfillments],
      all (\(identifier, obligation) -> WorkIndex.dependenciesFor identifier state.ordinaryAttemptWork == Just (ordinaryAttemptDependencies obligation state)) (Map.toAscList state.obligations),
      all (\key -> WorkIndex.dependenciesFor key state.bootstrapAttemptWork == Just (bootstrapAttemptDependencies key state)) (Map.keys state.bootstrapImports),
      all (\(key, attempt) -> WorkIndex.dependenciesFor key state.bootstrapFulfillmentWork == Just (Set.singleton (TransferDestinationProgress (alignmentSubscribeSubscriptionId attempt.subscribe)))) (Map.toAscList state.bootstrapAttempts)
    ]
  where
    pending =
      [ (herald, generation, delta, alignmentMemberStoreIncarnation member)
      | Just herald <- [state.transferLocalHerald],
        ((generation, delta), member) <- Map.toAscList (Map.findWithDefault Map.empty herald state.pendingMemberProgress)
      ]
    pendingKeys = Map.fromListWith Set.union [(herald, Set.singleton (generation, delta)) | (herald, generation, delta, _) <- pending]
    readyRows = [(generation, store, (herald, generation, delta)) | (herald, generation, delta, store) <- pending, Map.notMember (generation, store) state.localMemberReadiness]
    certificateRows = [(generation, store, (herald, generation, delta)) | (herald, generation, delta, store) <- pending, Map.notMember (generation, store) state.certificates]
    readyKeys = Map.fromListWith Set.union [(herald, Set.singleton (generation, delta)) | (_, _, (herald, generation, delta)) <- readyRows]
    certificateKeys = Map.fromListWith Set.union [(herald, Set.singleton (generation, delta)) | (_, _, (herald, generation, delta)) <- certificateRows]

alignmentStructuralDebts :: State -> StructuralDebtSet
alignmentStructuralDebts state = state.structuralDebts

structuralDebtsForCause ::
  StructuralConsequenceCause ->
  State ->
  [StructuralConsequenceDebt]
structuralDebtsForCause cause state =
  filter belongsToCause (structuralDebtSetEntries state.structuralDebts)
  where
    belongsToCause debt =
      structuralDebtKeyCause (structuralConsequenceDebtKey debt) == cause

-- | Structural debts remain as immutable derivation history after promotion.
-- Only a debt not covered by an exact promotion whose plan coordinate remains
-- live is executable owner work.
liveAlignmentStructuralDebtEntries :: State -> [StructuralConsequenceDebt]
liveAlignmentStructuralDebtEntries = Map.elems . (.liveStructuralDebts)

-- | Exhaustive derivation for auditing and the comparatively rare first
-- invalidation of a plan. Another valid promotion may still cover the same
-- debt, so invalidation must not blindly reactivate its receipt's keys.
reconstructLiveStructuralDebts :: StructuralDebtSet -> Map AlignmentPromotionKey AlignmentPromotionReceipt -> Map AlignmentPromotionKey [ContextClassGenerationId] -> Map AlignmentPlanCoordinate AlignmentPlanInvalidationReceipt -> Map StructuralDebtKey StructuralConsequenceDebt
reconstructLiveStructuralDebts debts promotions historical invalidations =
  Map.fromList
    [ (key, debt)
    | debt <- structuralDebtSetEntries debts,
      let key = structuralConsequenceDebtKey debt,
      Set.notMember key coveredDebtKeys
    ]
  where
    coveredDebtKeys =
      historicalCoveredDebtKeys debts historical invalidations
        <> Set.unions
          [ receipt.debtKeys
          | (key, receipt) <- Map.toAscList promotions,
            Map.notMember
              (promotionPlanCoordinate key)
              invalidations
          ]

-- Historical completion proves the cause/sort, not another owner's physical
-- debt keys. Replaying that cause into a fresh system view derives its own keys.
historicalCoveredDebtKeys :: StructuralDebtSet -> Map AlignmentPromotionKey [ContextClassGenerationId] -> Map AlignmentPlanCoordinate AlignmentPlanInvalidationReceipt -> Set StructuralDebtKey
historicalCoveredDebtKeys debts historical invalidations =
  Set.fromList
    [ key
    | debt <- structuralDebtSetEntries debts,
      let key = structuralConsequenceDebtKey debt,
      Set.member (structuralDebtKeyCause key, structuralDebtKeySort key) covered
    ]
  where
    covered =
      Set.fromList
        [ (alignmentPromotionKeyCause key, alignmentPromotionKeySort key)
        | key <- Map.keys historical,
          Map.notMember (promotionPlanCoordinate key) invalidations
        ]

-- | Exact once-only promotion group.  All debt kinds for the same structural
-- cause and affected sort are refined together into one coherent
-- topology/placement generation plan.
data AlignmentPromotionKey = AlignmentPromotionKey StructuralConsequenceCause Plan.AlignmentPlanId
  deriving stock (Eq, Ord, Show)

-- Legacy generation-only callers construct the initial plan attempt.
alignmentPromotionKey :: StructuralConsequenceCause -> SortOccurrence -> TopologyCutId -> PhysicalPlacementRevisionVector -> AlignmentPromotionKey
alignmentPromotionKey cause occurrence topology placement =
  alignmentPromotionKeyForPlan cause (Plan.alignmentPlanIdFromClaimedCoordinates occurrence topology placement)

alignmentPromotionKeyForPlan :: StructuralConsequenceCause -> Plan.AlignmentPlanId -> AlignmentPromotionKey
alignmentPromotionKeyForPlan = AlignmentPromotionKey

alignmentPromotionKeyCause :: AlignmentPromotionKey -> StructuralConsequenceCause
alignmentPromotionKeyCause (AlignmentPromotionKey cause _) = cause

alignmentPromotionKeyPlanId :: AlignmentPromotionKey -> Plan.AlignmentPlanId
alignmentPromotionKeyPlanId (AlignmentPromotionKey _ identifier) = identifier

alignmentPromotionKeySort :: AlignmentPromotionKey -> SortOccurrence
alignmentPromotionKeySort = Plan.alignmentPlanIdSort . alignmentPromotionKeyPlanId

alignmentPromotionKeyTopologyCut :: AlignmentPromotionKey -> TopologyCutId
alignmentPromotionKeyTopologyCut = Plan.alignmentPlanIdTopology . alignmentPromotionKeyPlanId

alignmentPromotionKeyPlacementVector :: AlignmentPromotionKey -> PhysicalPlacementRevisionVector
alignmentPromotionKeyPlacementVector = Plan.alignmentPlanIdPlacement . alignmentPromotionKeyPlanId

data AlignmentPromotionReceipt = AlignmentPromotionReceipt
  { topologyCut :: TopologyCutId,
    placementVector :: PhysicalPlacementRevisionVector,
    debtKeys :: Set StructuralDebtKey,
    generationIds :: [ContextClassGenerationId],
    relations :: Set AlignmentGenerationRelation,
    obligationIds :: [AlignmentObligationId],
    bootstrapImportKeys :: [BootstrapImportKey]
  }
  deriving stock (Eq, Show)

alignmentPromotionReceiptTopologyCut ::
  AlignmentPromotionReceipt -> TopologyCutId
alignmentPromotionReceiptTopologyCut receipt = receipt.topologyCut

alignmentPromotionReceiptPlacementVector ::
  AlignmentPromotionReceipt -> PhysicalPlacementRevisionVector
alignmentPromotionReceiptPlacementVector receipt = receipt.placementVector

alignmentPromotionReceiptDebtKeys ::
  AlignmentPromotionReceipt -> Set StructuralDebtKey
alignmentPromotionReceiptDebtKeys receipt = receipt.debtKeys

alignmentPromotionReceiptGenerationIds ::
  AlignmentPromotionReceipt -> [ContextClassGenerationId]
alignmentPromotionReceiptGenerationIds receipt = receipt.generationIds

alignmentPromotionReceiptRelations ::
  AlignmentPromotionReceipt -> [AlignmentGenerationRelation]
alignmentPromotionReceiptRelations receipt = Set.toAscList receipt.relations

alignmentPromotionReceiptObligationIds ::
  AlignmentPromotionReceipt -> [AlignmentObligationId]
alignmentPromotionReceiptObligationIds receipt = receipt.obligationIds

alignmentPromotionReceiptBootstrapImportKeys ::
  AlignmentPromotionReceipt -> [BootstrapImportKey]
alignmentPromotionReceiptBootstrapImportKeys receipt =
  receipt.bootstrapImportKeys

alignmentPromotionEntries ::
  State -> [(AlignmentPromotionKey, AlignmentPromotionReceipt)]
alignmentPromotionEntries state = Map.toAscList state.promotions

-- | Canonical semantic completion evidence, with every owner's private work
-- erased. Empty generation sets retain removal-only completion. Invalidation
-- reopens the same cause exactly as it does an ordinary promotion receipt.
alignmentCompletedCauseCoverage :: State -> [(AlignmentPromotionKey, [ContextClassGenerationId])]
alignmentCompletedCauseCoverage state =
  [ (key, identifiers)
  | (key, identifiers) <- Map.toAscList (Map.union (Map.map (sort . (.generationIds)) state.promotions) state.historicalCoverage),
    Map.notMember (promotionPlanCoordinate key) state.planInvalidations
  ]

-- | Include tombstoned coordinates so recovery cannot move behind an imported
-- predecessor. Callers distinguish live coverage from recovery authorization.
alignmentCauseCoverageKeys :: State -> [AlignmentPromotionKey]
alignmentCauseCoverageKeys state = Set.toAscList (Map.keysSet state.promotions <> Map.keysSet state.historicalCoverage)

-- | The cause and sort lead the key ordering, so select their contiguous
-- ranges before combining local and imported coverage. Keep tombstoned keys
-- and the complete-key ordering: recovery still needs its prior coordinates.
alignmentCauseCoverageKeysFor ::
  StructuralConsequenceCause ->
  SortOccurrence ->
  State ->
  [AlignmentPromotionKey]
alignmentCauseCoverageKeysFor cause affectedSort state =
  Set.toAscList
    ( Map.keysSet (matching state.promotions)
        <> Map.keysSet (matching state.historicalCoverage)
    )
  where
    wanted = (cause, affectedSort)
    coordinate key = (alignmentPromotionKeyCause key, alignmentPromotionKeySort key)
    matching :: Map AlignmentPromotionKey value -> Map AlignmentPromotionKey value
    matching =
      Map.takeWhileAntitone ((<= wanted) . coordinate)
        . Map.dropWhileAntitone ((< wanted) . coordinate)

alignmentGenerationEntries ::
  State -> [(ContextClassGenerationId, AlignmentGeneration)]
alignmentGenerationEntries state = Map.toAscList state.generations

-- | Current generations across all sorts, in the same identifier order as
-- filtering 'alignmentGenerationEntries' through the latest-by-sort index.
-- Superseded generations remain available through the historical accessors.
-- Invalidated latest coordinates are deliberately included: loss/recovery
-- callers decide their own disposition, as with the per-sort accessor.
latestAlignmentGenerationEntries ::
  State -> [(ContextClassGenerationId, AlignmentGeneration)]
latestAlignmentGenerationEntries state =
  [ (identifier, generation)
  | identifier <- Set.toAscList (Set.fromList (concat (Map.elems state.latestBySort))),
    Just generation <- [Map.lookup identifier state.generations]
  ]

-- | A deterministic work index. Completed local readiness/certification pairs
-- leave this index, while their immutable generation and evidence remain stored.
pendingAlignmentMemberProgress ::
  HeraldEpoch -> State -> [(AlignmentGeneration, AlignmentMember)]
pendingAlignmentMemberProgress local state =
  [ (generation, member)
  | ((identifier, _), member) <- Map.toAscList (Map.findWithDefault Map.empty local state.pendingMemberProgress),
    Just generation <- [Map.lookup identifier state.generations]
  ]

-- | Only generations which may still need this source's route settlement.
-- A completed entry may remain until the coordinator's retirement transition;
-- an unfinished eligible generation must always be represented.
pendingAlignmentRouteCutoverEntries ::
  HeraldEpoch -> State -> [(ContextClassGenerationId, AlignmentGeneration)]
pendingAlignmentRouteCutoverEntries local state =
  [ (identifier, generation)
  | identifier <- Set.toAscList (Map.findWithDefault Set.empty local state.pendingRouteCutoverGenerations),
    Just generation <- [Map.lookup identifier state.generations]
  ]

-- | Retire only completed candidates and return the exact owner-change fact.
-- Filtering can only remove identifiers, so a size decrease proves change
-- without comparing the complete owner state or historical catalogue.
retireCompletedAlignmentRouteCutovers :: HeraldEpoch -> State -> (State, Bool)
retireCompletedAlignmentRouteCutovers local state =
  let incumbent = Map.findWithDefault Set.empty local state.pendingRouteCutoverGenerations
      remaining = Set.filter remainsPending incumbent
      changed = Set.size remaining /= Set.size incumbent
   in if changed
        then
          ( state
              { pendingRouteCutoverGenerations =
                  case nonEmptySet remaining of
                    Nothing -> Map.delete local state.pendingRouteCutoverGenerations
                    Just candidates -> Map.insert local candidates state.pendingRouteCutoverGenerations
              },
            True
          )
        else (state, False)
  where
    remainsPending identifier = case Map.lookup identifier state.generations of
      Nothing -> False
      Just generation -> not (routeCutoverGenerationCompleted local generation state)

routeCutoverGenerationCompleted :: HeraldEpoch -> AlignmentGeneration -> State -> Bool
routeCutoverGenerationCompleted source generation state =
  all predecessorCompleted (alignmentCutPredecessorGenerationIds (alignmentGenerationCut generation))
  where
    identifier = alignmentGenerationId generation
    predecessorCompleted predecessorId = case Map.lookup predecessorId state.generations of
      Nothing -> False
      Just predecessor ->
        all
          (hostCompleted . alignmentMemberHerald)
          (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut predecessor)))
    hostCompleted host
      | host == source = Transfer.localRoutePrefixSettled identifier host state.transfer
      | otherwise = case Transfer.lookupRouteCutover identifier source host state.transfer of
          Nothing -> False
          Just _ -> True

indexNewAlignmentCandidates :: [AlignmentGeneration] -> State -> State
indexNewAlignmentCandidates supplied state =
  (notifyGenerationEvidence (Set.fromList (map (GenerationRetained . alignmentGenerationId) newGenerations)) state)
    { routeMarkerChanges = Set.union (Set.fromList (map alignmentGenerationId newGenerations)) state.routeMarkerChanges,
      pendingMemberProgress = Map.unionWith Map.union memberAdditions state.pendingMemberProgress,
      pendingRouteCutoverGenerations = Map.unionWith Set.union cutoverAdditions state.pendingRouteCutoverGenerations
    }
  where
    newGenerations = filter ((`Map.notMember` state.generations) . alignmentGenerationId) supplied
    memberAdditions =
      Map.fromListWith
        Map.union
        [ (alignmentMemberHerald member, Map.singleton (alignmentGenerationId generation, alignmentMemberDelta member) member)
        | generation <- newGenerations,
          member <- NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation))
        ]
    cutoverAdditions =
      Map.fromListWith
        Set.union
        [ (source, Set.singleton (alignmentGenerationId generation))
        | generation <- newGenerations,
          not (null (alignmentCutPredecessorGenerationIds (alignmentGenerationCut generation))),
          (source, _) <- NonEmpty.toList (physicalPlacementRevisionEntries (alignmentCutPhysicalPlacementRevisionVector (alignmentGenerationCut generation)))
        ]

retireCompletedAlignmentMember :: ContextClassGenerationId -> StoreIncarnationId -> State -> State
retireCompletedAlignmentMember identifier store state = synchronizeMemberEvidence identifier store (retireCompletedAlignmentMemberRaw identifier store state)

retireCompletedAlignmentMemberRaw :: ContextClassGenerationId -> StoreIncarnationId -> State -> State
retireCompletedAlignmentMemberRaw identifier store state
  | Map.member (identifier, store) state.localMemberReadiness
      && Map.member (identifier, store) state.certificates =
      case Map.lookup identifier state.generations of
        Nothing -> state
        Just generation ->
          foldl
            removeMember
            state
            [ member
            | member <- NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)),
              alignmentMemberStoreIncarnation member == store
            ]
  | otherwise = state
  where
    removeMember predecessor member =
      predecessor
        { pendingMemberProgress =
            Map.update
              (nonEmptyMap . Map.delete (identifier, alignmentMemberDelta member))
              (alignmentMemberHerald member)
              predecessor.pendingMemberProgress
        }

withoutPendingAlignmentGenerations :: Set ContextClassGenerationId -> State -> State
withoutPendingAlignmentGenerations removed state = synchronizeRemovedGenerationMembers removed (withoutPendingAlignmentGenerationsRaw removed state)

withoutPendingAlignmentGenerationsRaw :: Set ContextClassGenerationId -> State -> State
withoutPendingAlignmentGenerationsRaw removed state =
  state
    { pendingMemberProgress =
        Map.mapMaybe
          (nonEmptyMap . Map.filterWithKey (\(identifier, _) _ -> Set.notMember identifier removed))
          state.pendingMemberProgress,
      pendingRouteCutoverGenerations =
        Map.mapMaybe
          (nonEmptySet . (`Set.difference` removed))
          state.pendingRouteCutoverGenerations
    }

nonEmptyMap :: Map key value -> Maybe (Map key value)
nonEmptyMap entries = if Map.null entries then Nothing else Just entries

nonEmptySet :: Set value -> Maybe (Set value)
nonEmptySet entries = if Set.null entries then Nothing else Just entries

-- | A read-only projection, never a substitute for the checked owner state.
-- Nothing selects the complete reconnect catalogue; Just selects only keys
-- whose first component belongs to the ordinary transition candidate set.
data AlignmentControlEvidence = AlignmentControlEvidence
  { generationEntries :: [(ContextClassGenerationId, AlignmentGeneration)],
    plans :: [[AlignmentGeneration]],
    currentPlanEntries :: [(Plan.AlignmentPlanId, Plan.AlignmentPlan)],
    currentPlanAcceptances :: [((Plan.AlignmentPlanId, HeraldEpoch), Protocol.AlignmentPlanAccepted)],
    explicitBirthPlans :: Set Plan.AlignmentPlanId,
    historicalRecipientGenerations :: Map HeraldEpoch (Set ContextClassGenerationId),
    acceptances :: [((ContextClassGenerationId, HeraldEpoch), AlignmentCutAccepted)],
    readiness :: [((ContextClassGenerationId, StoreIncarnationId), ClassMemberReady)],
    certificateEntries :: [((ContextClassGenerationId, StoreIncarnationId), HistoricalCertificate)]
  }

alignmentControlEvidence :: Maybe (Set ContextClassGenerationId) -> State -> AlignmentControlEvidence
alignmentControlEvidence selected = alignmentControlEvidenceForPlans selected Nothing

-- | Ordinary transitions name their exact plan evidence separately from
-- generation evidence. A carried generation does not select every plan that
-- has ever bound it; reconnect alone requests the complete catalogue.
alignmentControlEvidenceForPlans :: Maybe (Set ContextClassGenerationId) -> Maybe (Set Plan.AlignmentPlanId) -> State -> AlignmentControlEvidence
alignmentControlEvidenceForPlans selected selectedPlanScope state =
  AlignmentControlEvidence
    { generationEntries = Map.toAscList selectedGenerations,
      currentPlanEntries = selectedPlans,
      currentPlanAcceptances = case selectedPlanScope of
        Nothing -> alignmentPlanAcceptanceEntries state
        Just identifiers -> concatMap (\identifier -> Map.toAscList (Map.takeWhileAntitone ((<= identifier) . fst) (Map.dropWhileAntitone ((< identifier) . fst) state.planAcceptances))) (Set.toAscList identifiers),
      explicitBirthPlans = Set.fromList [identifier | generation <- Map.elems selectedGenerations, let identifier = generationBirthPlanId generation, Just _ <- [lookupAlignmentPlan identifier state]],
      plans =
        [ [generation | identifier <- Set.toAscList identifiers, Just generation <- [Map.lookup identifier state.generations]]
        | identifiers <- Map.elems planIds
        ],
      historicalRecipientGenerations = Map.map snd state.historicalRecipients,
      acceptances = selectedMapEntries fst state.cutAcceptances,
      readiness = selectedMapEntries fst state.memberReadiness,
      certificateEntries = selectedMapEntries fst state.certificates
    }
  where
    selectedPlans = case selectedPlanScope of
      Nothing -> alignmentPlanEntries state
      Just identifiers -> [(identifier, plan) | identifier <- Set.toAscList identifiers, Just plan <- [lookupAlignmentPlan identifier state]]
    selectedGenerations = maybe state.generations (Map.restrictKeys state.generations) selected
    selectedCoordinates = Set.fromList (map generationPlanCoordinate (Map.elems selectedGenerations))
    planIds =
      Map.fromListWith
        Set.union
        [ (coordinate, Set.fromList receipt.generationIds)
        | (key, receipt) <- Map.toAscList state.promotions,
          let coordinate = promotionPlanCoordinate key,
          maybe True (const (Set.member coordinate selectedCoordinates)) selected
        ]
    -- Generation is the leading coordinate, so these are logarithmic range
    -- selections instead of complete-map list filtering.
    selectedMapEntries :: (key -> ContextClassGenerationId) -> Map key value -> [(key, value)]
    selectedMapEntries coordinate entries = case selected of
      Nothing -> Map.toAscList entries
      Just identifiers ->
        concatMap
          ( \identifier ->
              Map.toAscList
                ( Map.takeWhileAntitone
                    ((<= identifier) . coordinate)
                    (Map.dropWhileAntitone ((< identifier) . coordinate) entries)
                )
          )
          (Set.toAscList identifiers)

-- | Include deletions and changed same-key payloads as well as insertions:
-- callers can compare independently admitted branches, not just monotone runs.
-- A changed plan receipt or generation can also change its siblings' anchor
-- role, so expand those exact plan coordinates before projecting controls.
alignmentChangedControlGenerations :: State -> State -> Set ContextClassGenerationId
alignmentChangedControlGenerations before after =
  Set.unions
    [ changedGenerations,
      changedHistoricalRecipientGenerations before after,
      Set.unions [Set.fromList (map alignmentGenerationId (planGenerations plan)) | identifier <- Set.toList (Set.map fst (changedMapKeys before.planAcceptances after.planAcceptances)), state <- [before, after], Just plan <- [lookupAlignmentPlan identifier state]],
      Set.map fst (changedMapKeys before.cutAcceptances after.cutAcceptances),
      Set.map fst (changedMapKeys before.memberReadiness after.memberReadiness),
      Set.map fst (changedMapKeys before.certificates after.certificates),
      affectedPlanMembers before,
      affectedPlanMembers after
    ]
  where
    changedGenerations = changedMapKeys before.generations after.generations
    changedCoordinates =
      Set.map promotionPlanCoordinate (changedMapKeys before.promotions after.promotions)
        <> Set.fromList
          [ generationPlanCoordinate generation
          | state <- [before, after],
            generation <- Map.elems (Map.restrictKeys state.generations changedGenerations)
          ]
    affectedPlanMembers state
      | Set.null changedCoordinates = Set.empty
      | otherwise =
          Map.keysSet
            ( Map.filter
                ((`Set.member` changedCoordinates) . generationPlanCoordinate)
                state.generations
            )

-- | Exact plan dependencies of committed generations. A new receipt can
-- alter a sibling's anchor role without changing that sibling's payload.
-- Inputs are scoped by the coordinator to actual checked promotion results;
-- this read-only expansion makes no ancestry assumption about its snapshots.
alignmentControlPlanGenerations ::
  Set ContextClassGenerationId -> State -> State -> Set ContextClassGenerationId
alignmentControlPlanGenerations changed before after
  | Set.null selected = Set.empty
  | otherwise = selected <> members before <> members after
  where
    selected = changed <> changedHistoricalRecipientGenerations before after
    coordinates =
      Set.fromList
        [ generationPlanCoordinate generation
        | state <- [before, after],
          generation <- Map.elems (Map.restrictKeys state.generations selected)
        ]
    members state =
      Map.keysSet
        (Map.filter ((`Set.member` coordinates) . generationPlanCoordinate) state.generations)

-- | The caller supplies the current Oracle-admitted attempt and the exact
-- immutable catalogue captured or installed for this recipient. These IDs
-- express delivery interest only: a lagging old member may register them
-- before retaining the corresponding ordinary alignment plan. No generation,
-- membership, readiness, frontier, or local work is admitted by this transition.
-- Each epoch has one admission; a later attempt replaces its abandoned scope,
-- including when the replacement is empty. Same-attempt captures accumulate.
retainHistoricalAlignmentRecipient ::
  HeraldEpoch -> Word64 -> Set ContextClassGenerationId -> State -> State
retainHistoricalAlignmentRecipient recipient attempt identifiers state =
  state
    { historicalRecipients =
        Map.alter retain recipient state.historicalRecipients
    }
  where
    retain Nothing = Just (attempt, identifiers)
    retain existing@(Just (oldAttempt, oldIdentifiers))
      | attempt < oldAttempt = existing
      | attempt == oldAttempt = Just (attempt, oldIdentifiers <> identifiers)
      | otherwise = Just (attempt, identifiers)

historicalAlignmentRecipientGenerations ::
  HeraldEpoch -> State -> Set ContextClassGenerationId
historicalAlignmentRecipientGenerations recipient state =
  maybe Set.empty snd (Map.lookup recipient state.historicalRecipients)

historicalAlignmentRecipient ::
  HeraldEpoch -> ContextClassGenerationId -> State -> Bool
historicalAlignmentRecipient recipient identifier state =
  Set.member identifier (historicalAlignmentRecipientGenerations recipient state)

changedHistoricalRecipientGenerations :: State -> State -> Set ContextClassGenerationId
changedHistoricalRecipientGenerations before after =
  Set.unions
    [ identifiers
    | state <- [before, after],
      (_, identifiers) <- Map.elems (Map.restrictKeys state.historicalRecipients changedRecipients)
    ]
  where
    changedRecipients = changedMapKeys before.historicalRecipients after.historicalRecipients

promotionPlanCoordinate :: AlignmentPromotionKey -> AlignmentPlanCoordinate
promotionPlanCoordinate = planCoordinate . alignmentPromotionKeyPlanId

changedMapKeys :: (Ord key, Eq value) => Map key value -> Map key value -> Set key
changedMapKeys before after =
  Map.keysSet
    ( Map.mergeWithKey
        (\_ old new -> if old == new then Nothing else Just ())
        (Map.map (const ()))
        (Map.map (const ()))
        before
        after
    )

lookupAlignmentGeneration ::
  ContextClassGenerationId -> State -> Maybe AlignmentGeneration
lookupAlignmentGeneration identifier state = Map.lookup identifier state.generations

alignmentGenerationRelationEntries :: State -> [AlignmentGenerationRelation]
alignmentGenerationRelationEntries state = Set.toAscList state.generationRelations

latestAlignmentGenerationsForSort ::
  SortOccurrence -> State -> [AlignmentGeneration]
latestAlignmentGenerationsForSort wanted state =
  [ generation
  | identifier <- Map.findWithDefault [] wanted state.latestBySort,
    Just generation <- [Map.lookup identifier state.generations]
  ]

alignmentObligationEntries :: State -> [(AlignmentObligationId, AlignmentObligation)]
alignmentObligationEntries state = Map.toAscList state.obligations

activeAlignmentObligationEntries ::
  State -> [(AlignmentObligationId, AlignmentObligation)]
activeAlignmentObligationEntries = alignmentObligationEntries

alignmentClosingObligationIds :: State -> [AlignmentObligationId]
alignmentClosingObligationIds state = Set.toAscList state.closingObligations

alignmentObligationIsClosing :: AlignmentObligationId -> State -> Bool
alignmentObligationIsClosing identifier state = Set.member identifier state.closingObligations

alignmentClosedObligationEntries ::
  State -> [(AlignmentObligationId, AlignmentObligation)]
alignmentClosedObligationEntries state = Map.toAscList state.closedObligations

-- | Logical owners survive active -> closed movement and source-attempt loss.
-- The target names the predecessor generation and its retained destination Store.
requiredPredecessorInputOwnersAt :: Transfer.InputClosureTarget -> State -> Set AlignmentObligationId
requiredPredecessorInputOwnersAt target state = Map.findWithDefault Set.empty target state.expectedInputOwners

-- | One-consumer notifications for the predecessor-input closure stage. These
-- compact keys are accumulated by the same transitions that change their facts.
data InputClosureChanges = InputClosureChanges
  { changedTargets :: !(Set Transfer.InputClosureTarget),
    invalidatedGenerations :: !(Set ContextClassGenerationId)
  }
  deriving stock (Eq, Show)

takeInputClosureChanges :: State -> (InputClosureChanges, State)
takeInputClosureChanges state =
  ( InputClosureChanges state.inputClosureChangedTargets state.inputClosureInvalidatedGenerations,
    clearInputClosureChanges state
  )

-- | Comparison-only normalization also used by the unique journal consumer.
-- It never changes the authoritative expected-owner index.
clearInputClosureChanges :: State -> State
clearInputClosureChanges state = state {inputClosureChangedTargets = Set.empty, inputClosureInvalidatedGenerations = Set.empty}

inputClosureTargets :: AlignmentObligation -> Set Transfer.InputClosureTarget
inputClosureTargets obligation =
  Set.fromList
    [ (alignmentObligationDestinationGeneration obligation, destinationStoreIncarnation destination)
    | destination <- NonEmpty.toList (alignmentObligationDestinationStores obligation)
    ]

notifyInputClosureOwners :: [AlignmentObligation] -> State -> State
notifyInputClosureOwners owners state =
  state {inputClosureChangedTargets = foldl' (\targets owner -> Set.union targets (inputClosureTargets owner)) state.inputClosureChangedTargets owners}

insertInputClosureOwners :: [AlignmentObligation] -> State -> State
insertInputClosureOwners owners state =
  (notifyInputClosureOwners owners state)
    { expectedInputOwners = foldl' insertOwner state.expectedInputOwners owners
    }
  where
    insertOwner retained owner =
      Set.foldl'
        (\indexed target -> Map.insertWith Set.union target (Set.singleton (alignmentObligationIdValue owner)) indexed)
        retained
        (inputClosureTargets owner)

removeInputClosureOwners :: [AlignmentObligation] -> State -> State
removeInputClosureOwners owners state =
  (notifyInputClosureOwners owners state)
    { expectedInputOwners = foldl' removeOwner state.expectedInputOwners owners
    }
  where
    removeOwner retained owner =
      Set.foldl'
        (\indexed target -> Map.update (nonEmptySet . Set.delete (alignmentObligationIdValue owner)) target indexed)
        retained
        (inputClosureTargets owner)

lookupAlignmentObligation ::
  AlignmentObligationId -> State -> Maybe AlignmentObligation
lookupAlignmentObligation identifier state = Map.lookup identifier state.obligations

alignmentAttemptEntries :: State -> [(AlignmentObligationId, AlignmentAttempt)]
alignmentAttemptEntries state = Map.toAscList state.attempts

alignmentUnavailableSources :: State -> Set UnavailableAlignmentSource
alignmentUnavailableSources state = state.unavailableSources

data AlignmentSourceRetentionDisposition
  = AlignmentSourceRetainedUnavailable
  | AlignmentSourceRetentionUnchanged
  deriving stock (Eq, Ord, Show)

data PreparedUnavailableAlignmentSourceRetention
  = PreparedUnavailableAlignmentSourceRetention
      State
      AlignmentSourceRetentionDisposition

-- | Retain a checked source-incarnation loss independently of any currently
-- selected attempt.  This fact is monotone for the lifetime of the owner: an
-- incarnation cannot become available again, while a replacement incarnation
-- has a different coordinate.
prepareUnavailableAlignmentSourceRetention ::
  UnavailableAlignmentSource ->
  State ->
  PreparedUnavailableAlignmentSourceRetention
prepareUnavailableAlignmentSourceRetention source state
  | Set.member source state.unavailableSources =
      PreparedUnavailableAlignmentSourceRetention
        state
        AlignmentSourceRetentionUnchanged
  | otherwise =
      PreparedUnavailableAlignmentSourceRetention
        (notifyTransferSourceUnavailable source state)
          { unavailableSources =
              Set.insert source state.unavailableSources,
            pendingLossCheck = True
          }
        AlignmentSourceRetainedUnavailable

preparedUnavailableAlignmentSourceRetentionDisposition ::
  PreparedUnavailableAlignmentSourceRetention ->
  AlignmentSourceRetentionDisposition
preparedUnavailableAlignmentSourceRetentionDisposition
  (PreparedUnavailableAlignmentSourceRetention _ disposition) = disposition

commitUnavailableAlignmentSourceRetention ::
  PreparedUnavailableAlignmentSourceRetention ->
  (State, AlignmentSourceRetentionDisposition)
commitUnavailableAlignmentSourceRetention
  (PreparedUnavailableAlignmentSourceRetention state disposition) =
    (state, disposition)

alignmentAttemptInvalidationEntries ::
  State -> [(AlignmentSubscriptionId, AlignmentAttemptInvalidationReceipt)]
alignmentAttemptInvalidationEntries state =
  Map.toAscList state.attemptInvalidations

alignmentPlanInvalidationEntries ::
  State -> [(AlignmentPlanCoordinate, AlignmentPlanInvalidationReceipt)]
alignmentPlanInvalidationEntries state = Map.toAscList state.planInvalidations

alignmentPlanIsInvalidated ::
  SortOccurrence ->
  TopologyCutId ->
  PhysicalPlacementRevisionVector ->
  State ->
  Bool
alignmentPlanIsInvalidated affectedSort topologyCut placementVector state =
  Map.member
    (planCoordinate (Plan.alignmentPlanIdFromClaimedCoordinates affectedSort topologyCut placementVector))
    state.planInvalidations

pendingGenerationEvidenceEntries :: State -> [PendingGenerationEvidence]
pendingGenerationEvidenceEntries state =
  Map.elems state.pendingGenerationEvidence

data PendingGenerationEvidenceDisposition
  = PendingGenerationEvidenceRetained
  | PendingGenerationEvidenceRemoved
  | PendingGenerationEvidenceUnchanged
  deriving stock (Eq, Ord, Show)

data PendingGenerationEvidenceProblem
  = PendingGenerationEvidenceUnsupported
  | PendingGenerationEvidenceConflict
  deriving stock (Eq, Ord, Show)

newtype PreparedPendingGenerationEvidence
  = PreparedPendingGenerationEvidence
      (Prepared State PendingGenerationEvidenceDisposition)

-- | Retain one exact, authenticated generation-evidence message until all of
-- its semantic dependencies are available. Equal replay is idempotent;
-- unequal evidence for the same immutable semantic coordinate conflicts.
preparePendingGenerationEvidence ::
  HeraldEpoch ->
  AlignmentControl ->
  State ->
  Either
    PendingGenerationEvidenceProblem
    PreparedPendingGenerationEvidence
preparePendingGenerationEvidence remote control state = do
  key <- pendingGenerationEvidenceKey control
  let candidate = PendingGenerationEvidence remote control
  case Map.lookup key state.pendingGenerationEvidence of
    Just incumbent
      | incumbent == candidate ->
          prepare PendingGenerationEvidenceUnchanged
      | otherwise -> Left PendingGenerationEvidenceConflict
    Nothing ->
      PreparedPendingGenerationEvidence
        <$> prepareTransition
          ( \predecessor ->
              Right
                ( predecessor
                    { pendingGenerationEvidence =
                        Map.insert
                          key
                          candidate
                          predecessor.pendingGenerationEvidence,
                      generationEvidenceWork = WorkIndex.registerWork key (generationEvidenceDependencies key) predecessor.generationEvidenceWork
                    },
                  PendingGenerationEvidenceRetained
                )
          )
          state
  where
    prepare disposition =
      PreparedPendingGenerationEvidence
        <$> prepareTransition
          (\predecessor -> Right (predecessor, disposition))
          state

-- | Remove only the exact retained message that has just passed normal
-- admission. This prevents a later dependency drain from deleting conflicting
-- evidence that happened to share its key.
preparePendingGenerationEvidenceRemoval ::
  HeraldEpoch ->
  AlignmentControl ->
  State ->
  Either
    PendingGenerationEvidenceProblem
    PreparedPendingGenerationEvidence
preparePendingGenerationEvidenceRemoval remote control state = do
  key <- pendingGenerationEvidenceKey control
  let candidate = PendingGenerationEvidence remote control
  case Map.lookup key state.pendingGenerationEvidence of
    Nothing -> prepare PendingGenerationEvidenceUnchanged
    Just incumbent
      | incumbent == candidate ->
          PreparedPendingGenerationEvidence
            <$> prepareTransition
              ( \predecessor ->
                  Right
                    ( predecessor
                        { pendingGenerationEvidence =
                            Map.delete key predecessor.pendingGenerationEvidence,
                          generationEvidenceWork = WorkIndex.removeWork key predecessor.generationEvidenceWork,
                          generationQueryScopeChanged = predecessor.generationQueryScopeChanged || pendingEvidenceUsesPlacement key
                        },
                      PendingGenerationEvidenceRemoved
                    )
              )
              state
      | otherwise -> Left PendingGenerationEvidenceConflict
  where
    prepare disposition =
      PreparedPendingGenerationEvidence
        <$> prepareTransition
          (\predecessor -> Right (predecessor, disposition))
          state

preparedPendingGenerationEvidenceDisposition ::
  PreparedPendingGenerationEvidence -> PendingGenerationEvidenceDisposition
preparedPendingGenerationEvidenceDisposition
  (PreparedPendingGenerationEvidence prepared) = preparedOutput prepared

commitPendingGenerationEvidence ::
  PreparedPendingGenerationEvidence ->
  (State, PendingGenerationEvidenceDisposition)
commitPendingGenerationEvidence (PreparedPendingGenerationEvidence prepared) =
  commitPrepared prepared

data PreparedPendingGenerationEvidenceRetirement
  = PreparedPendingGenerationEvidenceRetirement
      State
      [PendingGenerationEvidence]

-- | Remove only unactioned generation evidence authenticated by an epoch that
-- has just been retired. Already-admitted generations, acceptances, readiness,
-- certificates, fulfilled imports, and evidence from surviving peers are
-- deliberately outside this cut.
preparePendingGenerationEvidenceRetirement ::
  HeraldEpoch ->
  State ->
  PreparedPendingGenerationEvidenceRetirement
preparePendingGenerationEvidenceRetirement retired predecessor =
  PreparedPendingGenerationEvidenceRetirement successor removed
  where
    (retiredEvidence, retainedEvidence) =
      Map.partition
        ((== retired) . pendingGenerationEvidenceRemoteHerald)
        predecessor.pendingGenerationEvidence
    successor =
      predecessor
        { pendingGenerationEvidence = retainedEvidence,
          generationEvidenceWork = Map.foldlWithKey' (\work key _ -> WorkIndex.removeWork key work) predecessor.generationEvidenceWork retiredEvidence,
          generationQueryScopeChanged = predecessor.generationQueryScopeChanged || any pendingEvidenceUsesPlacement (Map.keys retiredEvidence)
        }
    removed = Map.elems retiredEvidence

preparedPendingGenerationEvidenceRetirementRemoved ::
  PreparedPendingGenerationEvidenceRetirement ->
  [PendingGenerationEvidence]
preparedPendingGenerationEvidenceRetirementRemoved
  (PreparedPendingGenerationEvidenceRetirement _ removed) = removed

commitPendingGenerationEvidenceRetirement ::
  PreparedPendingGenerationEvidenceRetirement ->
  (State, [PendingGenerationEvidence])
commitPendingGenerationEvidenceRetirement
  (PreparedPendingGenerationEvidenceRetirement successor removed) =
    (successor, removed)

pendingGenerationEvidenceKey ::
  AlignmentControl ->
  Either PendingGenerationEvidenceProblem PendingGenerationEvidenceKey
pendingGenerationEvidenceKey = \case
  AlignmentPlanAnnounced announce -> Right (PendingPlanAnnounce (Protocol.alignmentPlanAnnounceId announce))
  AlignmentPlanAcceptanceAdvertised accepted -> Right (PendingPlanAcceptance (Protocol.alignmentPlanAcceptedId accepted) (Protocol.alignmentPlanAcceptedHerald accepted))
  AlignmentCutAnnounced announce ->
    Right (PendingCutAnnounce (alignmentCutAnnounceGeneration announce))
  AlignmentCutAcceptanceAdvertised accepted ->
    Right
      ( PendingCutAcceptance
          (alignmentCutAcceptedGeneration accepted)
          (alignmentCutAcceptedHerald accepted)
      )
  AlignmentMemberReadyAdvertised ready ->
    Right
      ( PendingMemberReady
          (classMemberReadyGeneration ready)
          (classMemberReadyStoreIncarnation ready)
      )
  AlignmentHistoricalCertificateAdvertised certificate ->
    Right
      ( PendingCertificate
          (historicalCertificateClassGeneration certificate)
          (historicalCertificateSourceStoreIncarnation certificate)
      )
  _ -> Left PendingGenerationEvidenceUnsupported

bootstrapImportEntries :: State -> [(BootstrapImportKey, BootstrapImport)]
bootstrapImportEntries state = Map.toAscList state.bootstrapImports

bootstrapImportAttemptEntries ::
  State -> [(BootstrapImportKey, BootstrapImportAttempt)]
bootstrapImportAttemptEntries state = Map.toAscList state.bootstrapAttempts

lookupBootstrapImportAttempt ::
  BootstrapImportKey -> State -> Maybe BootstrapImportAttempt
lookupBootstrapImportAttempt key state = Map.lookup key state.bootstrapAttempts

bootstrapImportAttemptInvalidationEntries ::
  State ->
  [(AlignmentSubscriptionId, BootstrapImportAttemptInvalidationReceipt)]
bootstrapImportAttemptInvalidationEntries state =
  Map.toAscList state.bootstrapAttemptInvalidations

bootstrapImportFulfillmentEntries ::
  State -> [(BootstrapImportKey, BootstrapImportFulfillmentReceipt)]
bootstrapImportFulfillmentEntries state = Map.toAscList state.bootstrapFulfillments

alignmentCutAcceptanceEntries ::
  State ->
  [((ContextClassGenerationId, HeraldEpoch), AlignmentCutAccepted)]
alignmentCutAcceptanceEntries state = Map.toAscList state.cutAcceptances

lookupAlignmentCutAcceptance ::
  ContextClassGenerationId ->
  HeraldEpoch ->
  State ->
  Maybe AlignmentCutAccepted
lookupAlignmentCutAcceptance generation herald state =
  Map.lookup (generation, herald) state.cutAcceptances

alignmentMemberReadinessEntries ::
  State ->
  [((ContextClassGenerationId, StoreIncarnationId), ClassMemberReady)]
alignmentMemberReadinessEntries state = Map.toAscList state.memberReadiness

lookupAlignmentMemberReadiness ::
  ContextClassGenerationId ->
  StoreIncarnationId ->
  State ->
  Maybe ClassMemberReady
lookupAlignmentMemberReadiness generation store state =
  Map.lookup (generation, store) state.memberReadiness

alignmentLocalMemberReadinessEntries ::
  State ->
  [((ContextClassGenerationId, StoreIncarnationId), ClassMemberReady)]
alignmentLocalMemberReadinessEntries state =
  Map.toAscList state.localMemberReadiness

lookupAlignmentLocalMemberReadiness ::
  ContextClassGenerationId ->
  StoreIncarnationId ->
  State ->
  Maybe ClassMemberReady
lookupAlignmentLocalMemberReadiness generation store state =
  Map.lookup (generation, store) state.localMemberReadiness

alignmentHistoricalCertificateEntries ::
  State ->
  [((ContextClassGenerationId, StoreIncarnationId), HistoricalCertificate)]
alignmentHistoricalCertificateEntries state = Map.toAscList state.certificates

-- | Certificates for the current non-invalidated generation set may still
-- authorize live alignment work. Certificates for superseded or invalidated
-- generations remain immutable ancestry only.
liveAlignmentHistoricalCertificateEntries ::
  State ->
  [((ContextClassGenerationId, StoreIncarnationId), HistoricalCertificate)]
liveAlignmentHistoricalCertificateEntries state =
  filter (generationIsLive . fst . fst) (alignmentHistoricalCertificateEntries state)
  where
    generationIsLive identifier = case Map.lookup identifier state.generations of
      Nothing -> False
      Just generation ->
        identifier `elem` Map.findWithDefault [] (alignmentPlanCoordinateSort (generationPlanCoordinate generation)) state.latestBySort
          && not (generationCurrentPlanInvalidated generation state)

lookupAlignmentHistoricalCertificate ::
  ContextClassGenerationId ->
  StoreIncarnationId ->
  State ->
  Maybe HistoricalCertificate
lookupAlignmentHistoricalCertificate generation store state =
  Map.lookup (generation, store) state.certificates

alignmentTransferState :: State -> Transfer.State
alignmentTransferState state = state.transfer

-- | Comparison/startup normalization through the Alignment owner boundary.
clearDestinationProgressChanges :: State -> State
clearDestinationProgressChanges state = state {transfer = Transfer.clearDestinationProgressChanges state.transfer}

-- | Install a successor admitted by the pure transfer owner.  The enclosing
-- coordinator prepares the Store and transfer successors first, then replaces
-- both leaf owners in one whole-Herald transition.
replaceAlignmentTransferState :: Transfer.State -> State -> State
replaceAlignmentTransferState transfer state = state {transfer}

data StructuralDebtRetention
  = StructuralDebtRetained
  | StructuralDebtRetentionUnchanged
  deriving stock (Eq, Ord, Show)

data AlignmentDebtProblem
  = StructuralDebtCauseMismatch
      StructuralConsequenceCause
      StructuralConsequenceCause
  deriving stock (Eq, Show)

newtype PreparedStructuralDebtRetention
  = PreparedStructuralDebtRetention
      (Prepared State StructuralDebtRetention)

-- | Retain one cause's complete normalized debt set.
--
-- Repeated derivations merge by the lawful evidence-set union implemented by
-- 'normalizeStructuralDebts'.  An empty set is a valid finite consequence for
-- a structurally applied non-winner and is therefore an idempotent no-op.
prepareStructuralDebtRetention ::
  StructuralConsequenceCause ->
  StructuralDebtSet ->
  State ->
  Either AlignmentDebtProblem PreparedStructuralDebtRetention
prepareStructuralDebtRetention cause supplied state = do
  mapM_ requireCause (structuralDebtSetEntries supplied)
  PreparedStructuralDebtRetention
    <$> prepareTransition retain state
  where
    requireCause debt =
      let observed =
            structuralDebtKeyCause
              (structuralConsequenceDebtKey debt)
       in if observed == cause
            then Right ()
            else Left (StructuralDebtCauseMismatch cause observed)

    retain predecessor =
      let incumbentEntries =
            structuralDebtSetEntries predecessor.structuralDebts
          merged =
            normalizeStructuralDebts
              (incumbentEntries <> structuralDebtSetEntries supplied)
          suppliedKeys = Set.fromList (map structuralConsequenceDebtKey (structuralDebtSetEntries supplied))
          incumbentKeys = Set.fromList (map structuralConsequenceDebtKey incumbentEntries)
          liveKeys =
            Set.difference
              (Map.keysSet predecessor.liveStructuralDebts <> Set.difference suppliedKeys incumbentKeys)
              (historicalCoveredDebtKeys merged predecessor.historicalCoverage predecessor.planInvalidations)
          live =
            Map.fromList
              [ (key, debt)
              | debt <- structuralDebtSetEntries merged,
                let key = structuralConsequenceDebtKey debt,
                Set.member key liveKeys
              ]
          classification =
            if merged == predecessor.structuralDebts
              then StructuralDebtRetentionUnchanged
              else StructuralDebtRetained
       in Right
            ( if classification == StructuralDebtRetentionUnchanged
                then predecessor
                else
                  refreshLiveDebtScheduling suppliedKeys
                    $ predecessor
                      { structuralDebts = merged,
                        liveStructuralDebts = live,
                        preparationReadinessChanges =
                          Set.union
                            predecessor.preparationReadinessChanges
                            (Set.map structuralDebtKeySort (Set.filter (\key -> Map.member key predecessor.liveStructuralDebts /= Set.member key liveKeys) suppliedKeys))
                      },
              classification
            )

preparedStructuralDebtRetention ::
  PreparedStructuralDebtRetention ->
  StructuralDebtRetention
preparedStructuralDebtRetention
  (PreparedStructuralDebtRetention prepared) = preparedOutput prepared

commitStructuralDebtRetention ::
  PreparedStructuralDebtRetention ->
  (State, StructuralDebtRetention)
commitStructuralDebtRetention
  (PreparedStructuralDebtRetention prepared) = commitPrepared prepared

data AlignmentPromotionDisposition
  = AlignmentPromotionCommitted
  | AlignmentPromotionUnchanged
  deriving stock (Eq, Ord, Show)

data AlignmentPromotionProblem
  = AlignmentPromotionPlanProblem Plan.AlignmentPlanProblem
  | AlignmentPromotionDebtMissing AlignmentPromotionKey
  | AlignmentPromotionTopologyCutMismatch TopologyCutId TopologyCutId
  | AlignmentPromotionPlacementVectorMismatch
  | AlignmentPromotionSortMismatch SortOccurrence SortOccurrence
  | AlignmentPromotionGenerationIdMismatch
      ContextClassGenerationId
      ContextClassGenerationId
  | AlignmentPromotionGenerationConflict ContextClassGenerationId
  | AlignmentPromotionRelationGenerationMissing ContextClassGenerationId
  | AlignmentPromotionObligationProblem AlignmentProtocolProblem
  | AlignmentPromotionConflict AlignmentPromotionKey
  | AlignmentPromotionPlanInvalidated AlignmentPlanCoordinate
  | AlignmentPromotionResetInvalid Plan.AlignmentPlanId
  | AlignmentPromotionResetInvalidationProblem AlignmentPlanInvalidationProblem
  | AlignmentHistoricalPlanConflict AlignmentPlanCoordinate
  | AlignmentHistoricalPlanLocalMember HeraldEpoch
  | AlignmentHistoricalPredecessorMissing ContextClassGenerationId
  | AlignmentHistoricalCertificateMissing ContextClassGenerationId
  | AlignmentHistoricalFrontierInvalid SortOccurrence
  | AlignmentHistoricalFrontierConflict SortOccurrence
  | AlignmentHistoricalCoverageConflict AlignmentPromotionKey
  deriving stock (Eq, Show)

newtype PreparedAlignmentPromotion
  = PreparedAlignmentPromotion
      (Prepared State AlignmentPromotionDisposition)

-- | Atomically refine one cause/sort debt group into its deterministic
-- generation plan.  Exact replay is idempotent.  The receipt is retained even
-- when the class set is empty, proving that removal-only debt was promoted and
-- preventing a later message from constructing a second plan.
prepareAlignmentPromotion ::
  HeraldEpoch ->
  StructuralConsequenceCause ->
  SortOccurrence ->
  TopologyCutId ->
  PhysicalPlacementRevisionVector ->
  AlignmentGenerationPlan ->
  State ->
  Either AlignmentPromotionProblem PreparedAlignmentPromotion
prepareAlignmentPromotion localHerald cause affectedSort topologyCut placementVector plan =
  prepareAlignmentPromotionParts
    localHerald
    cause
    affectedSort
    topologyCut
    placementVector
    Nothing
    (alignmentGenerationPlanGenerations plan)
    (alignmentGenerationPlanRelations plan)

-- | Promote checked current bindings while preserving exact live relation owners.
prepareAlignmentPlanPromotion :: HeraldEpoch -> StructuralConsequenceCause -> Plan.AlignmentPlan -> State -> Either AlignmentPromotionProblem PreparedAlignmentPromotion
prepareAlignmentPlanPromotion local cause plan state = do
  (ledger, retention) <- mapLeft AlignmentPromotionPlanProblem (Plan.retainAlignmentPlan cause plan state.planLedger)
  if alignmentPlanInvalidated identifier state then Left (AlignmentPromotionPlanInvalidated (planCoordinate identifier)) else Right ()
  case retention of
    Plan.AlignmentPlanInserted -> do
      if Plan.alignmentPlanPredecessor plan /= Map.lookup occurrence state.currentPlans
        then Left (AlignmentHistoricalFrontierConflict occurrence)
        else Right ()
      PreparedAlignmentPromotion prepared <-
        prepareAlignmentPromotionParts
          local
          cause
          occurrence
          (Plan.alignmentPlanIdTopology identifier)
          (Plan.alignmentPlanIdPlacement identifier)
          (Just plan)
          (planGenerations plan)
          (Plan.alignmentPlanRelations plan)
          state
      let (promoted, disposition) = commitPrepared prepared
          successor =
            (notifyGenerationEvidence (Set.singleton (PlanRetained identifier)) promoted)
              { planLedger = ledger,
                currentPlans = Map.insert occurrence identifier promoted.currentPlans
              }
      PreparedAlignmentPromotion <$> prepareTransition (\_ -> Right (successor, disposition)) state
    _ -> do
      let key = alignmentPromotionKeyForPlan cause identifier
          debtKeys = Set.fromList [structuralConsequenceDebtKey debt | debt <- structuralDebtSetEntries state.structuralDebts, let debtKey = structuralConsequenceDebtKey debt, structuralDebtKeyCause debtKey == cause, structuralDebtKeySort debtKey == occurrence]
          identifiers = map alignmentGenerationId (planGenerations plan)
          relations = Set.fromList (Plan.alignmentPlanRelations plan)
      if Set.null debtKeys then Left (AlignmentPromotionDebtMissing key) else Right ()
      case Map.lookup key state.promotions of
        Just retained
          | promotionReceiptCoreMatches (Plan.alignmentPlanIdTopology identifier) (Plan.alignmentPlanIdPlacement identifier) debtKeys identifiers relations retained ->
              PreparedAlignmentPromotion <$> prepareTransition (\predecessor -> Right (predecessor, AlignmentPromotionUnchanged)) state
        Just _ -> Left (AlignmentPromotionConflict key)
        Nothing -> do
          let receipt = AlignmentPromotionReceipt (Plan.alignmentPlanIdTopology identifier) (Plan.alignmentPlanIdPlacement identifier) debtKeys identifiers relations [] []
              successor =
                refreshLiveDebtScheduling debtKeys . notifyPreparationReadiness occurrence
                  $ state
                    { planLedger = ledger,
                      promotions = Map.insert key receipt state.promotions,
                      liveStructuralDebts = Map.withoutKeys state.liveStructuralDebts debtKeys
                    }
          PreparedAlignmentPromotion <$> prepareTransition (\_ -> Right (successor, AlignmentPromotionCommitted)) state
  where
    identifier = Plan.alignmentPlanId plan
    occurrence = Plan.alignmentPlanIdSort identifier

-- | Publish one fresh reset and retire the executable owners of older
-- attempts atomically. Immutable Store data, history and terminal receipts are
-- retained; reset is neither a fabricated Store loss nor historical replay.
data PreparedAlignmentPlanResetPromotion
  = PreparedAlignmentPlanResetPromotion
      (Prepared State AlignmentPromotionDisposition)
      [AlignmentCancel]

prepareAlignmentPlanResetPromotion :: HeraldEpoch -> NonEmpty.NonEmpty StructuralConsequenceCause -> Plan.AlignmentPlan -> State -> Either AlignmentPromotionProblem PreparedAlignmentPlanResetPromotion
prepareAlignmentPlanResetPromotion local causes plan state = do
  requireInvariant resetShape (AlignmentPromotionResetInvalid identifier)
  case lookupAlignmentPlan identifier state of
    Just retained -> do
      requireInvariant (retained == plan && Map.lookup occurrence state.currentPlans == Just identifier) (AlignmentPromotionResetInvalid identifier)
      (successor, disposition) <- promoteCauses state
      prepared <- prepareTransition (\_ -> Right (successor, disposition)) state
      Right (PreparedAlignmentPlanResetPromotion prepared [])
    Nothing -> do
      requireInvariant (all olderAttempt knownPlanIds) (AlignmentPromotionResetInvalid identifier)
      requireInvariant (all (\generation -> Map.notMember (alignmentGenerationId generation) state.generations) (planGenerations plan)) (AlignmentPromotionResetInvalid identifier)
      (retired, cancellations) <- foldM supersede (state, []) oldPlans
      let retiredOwners =
            Set.fromList
              [ owner
              | (coordinate, receipt) <- Map.toAscList retired.planInvalidations,
                alignmentPlanCoordinateSort coordinate == occurrence,
                olderAttempt (coordinatePlanId coordinate),
                (owner, _) <- alignmentPlanInvalidationReceiptObligations receipt
              ]
      closureRemoval <- mapLeft (AlignmentPromotionResetInvalidationProblem . AlignmentPlanInvalidationTransferProblem) (Transfer.prepareIncompletePredecessorInputClosureRemoval retiredOwners retired.transfer)
      let (transfer, _) = Transfer.commitIncompletePredecessorInputClosureRemoval closureRemoval
          ready = retireSupersededPlanEvidence identifier retired {currentPlans = Map.delete occurrence retired.currentPlans, transfer}
      (successor, _) <- promoteCauses ready
      prepared <- prepareTransition (\_ -> Right (successor, AlignmentPromotionCommitted)) state
      Right (PreparedAlignmentPlanResetPromotion prepared cancellations)
  where
    identifier = Plan.alignmentPlanId plan
    occurrence = Plan.alignmentPlanIdSort identifier
    resetShape =
      Plan.alignmentPlanPredecessorStatus plan == Plan.AlignmentPredecessorReset
        && Plan.alignmentPlanPredecessor plan == Nothing
        && all ((== Plan.AlignmentCreated) . Plan.alignmentPlanBindingDisposition . snd) (Plan.alignmentPlanBindings plan)
        && all ((== identifier) . generationBirthPlanId) (planGenerations plan)
    olderAttempt previous = Plan.alignmentPlanIdAttempt previous < Plan.alignmentPlanIdAttempt identifier
    knownPlanIds =
      Set.toAscList . Set.filter ((== occurrence) . Plan.alignmentPlanIdSort) . Set.fromList
        $ map fst (alignmentPlanEntries state)
          <> map alignmentPromotionKeyPlanId (Map.keys state.promotions <> Map.keys state.historicalCoverage)
          <> map generationBirthPlanId (Map.elems state.generations)
    oldPlans = filter (\previous -> not (alignmentPlanInvalidated previous state)) knownPlanIds
    supersede (predecessor, cancellations) previous = do
      prepared <- mapLeft AlignmentPromotionResetInvalidationProblem (prepareAlignmentPlanInvalidationAt (AlignmentPlanSuperseded previous identifier) (planCoordinate previous) predecessor)
      let (successor, _) = commitAlignmentPlanInvalidation prepared
      Right (successor, cancellations <> preparedAlignmentPlanInvalidationCancellations prepared)
    promoteCauses predecessor = foldM promote (predecessor, AlignmentPromotionUnchanged) (NonEmpty.toList causes)
    promote (predecessor, disposition) cause = do
      prepared <- prepareAlignmentPlanPromotion local cause plan predecessor
      let (successor, current) = commitAlignmentPromotion prepared
      Right (successor, if current == AlignmentPromotionCommitted then current else disposition)

preparedAlignmentPlanResetPromotionCancellations :: PreparedAlignmentPlanResetPromotion -> [AlignmentCancel]
preparedAlignmentPlanResetPromotionCancellations (PreparedAlignmentPlanResetPromotion _ cancellations) = cancellations

preparedAlignmentPlanResetPromotionDisposition :: PreparedAlignmentPlanResetPromotion -> AlignmentPromotionDisposition
preparedAlignmentPlanResetPromotionDisposition (PreparedAlignmentPlanResetPromotion prepared _) = preparedOutput prepared

commitAlignmentPlanResetPromotion :: PreparedAlignmentPlanResetPromotion -> (State, AlignmentPromotionDisposition)
commitAlignmentPlanResetPromotion (PreparedAlignmentPlanResetPromotion prepared _) = commitPrepared prepared

retireSupersededPlanEvidence :: Plan.AlignmentPlanId -> State -> State
retireSupersededPlanEvidence replacement state =
  state
    { planObsoleteReports = reports,
      planObsoleteSorts = reportSorts,
      pendingGenerationEvidence = Map.withoutKeys state.pendingGenerationEvidence removedKeys,
      generationEvidenceWork = Set.foldl' (flip WorkIndex.removeWork) state.generationEvidenceWork removedKeys,
      generationQueryScopeChanged = state.generationQueryScopeChanged || any pendingEvidenceUsesPlacement (Set.toAscList removedKeys),
      generationPlanWork =
        if Map.member occurrence state.liveDebtKeysBySort || Set.member occurrence reportSorts
          then state.generationPlanWork
          else WorkIndex.removeWork occurrence state.generationPlanWork
    }
  where
    occurrence = Plan.alignmentPlanIdSort replacement
    superseded identifier = Plan.alignmentPlanIdSort identifier == occurrence && Plan.alignmentPlanIdAttempt identifier < Plan.alignmentPlanIdAttempt replacement
    reports = Map.filterWithKey (\(identifier, _) _ -> not (superseded identifier)) state.planObsoleteReports
    reportSorts = Set.fromList [Plan.alignmentPlanIdSort identifier | (identifier, _) <- Map.keys reports]
    removedKeys = Map.keysSet (Map.filterWithKey obsolete state.pendingGenerationEvidence)
    obsolete key evidence = case key of
      PendingPlanAnnounce identifier -> superseded identifier
      PendingPlanAcceptance identifier _ -> superseded identifier
      PendingCutAnnounce _ -> case evidence.control of
        AlignmentCutAnnounced announce ->
          let cut = Protocol.alignmentCutAnnounceCut announce
           in sortOccurrence (alignmentCutSortId cut) (alignmentCutSortDefinitionOccurrenceId cut) == occurrence && alignmentCutAttempt cut < Plan.alignmentPlanIdAttempt replacement
        _ -> False
      PendingCutAcceptance generation _ -> maybe False (superseded . generationBirthPlanId) (Map.lookup generation state.generations)
      _ -> False

prepareAlignmentPromotionParts :: HeraldEpoch -> StructuralConsequenceCause -> SortOccurrence -> TopologyCutId -> PhysicalPlacementRevisionVector -> Maybe Plan.AlignmentPlan -> [AlignmentGeneration] -> [AlignmentGenerationRelation] -> State -> Either AlignmentPromotionProblem PreparedAlignmentPromotion
prepareAlignmentPromotionParts localHerald cause affectedSort topologyCut placementVector explicitPlan suppliedGenerations suppliedRelations state = do
  let planIdentifier = maybe (Plan.alignmentPlanIdFromClaimedCoordinates affectedSort topologyCut placementVector) Plan.alignmentPlanId explicitPlan
      key = alignmentPromotionKeyForPlan cause planIdentifier
      debtKeys =
        Set.fromList
          [ debtKey
          | debt <- structuralDebtSetEntries state.structuralDebts,
            let debtKey = structuralConsequenceDebtKey debt,
            structuralDebtKeyCause debtKey == cause,
            structuralDebtKeySort debtKey == affectedSort
          ]
      generationIds = fmap alignmentGenerationId suppliedGenerations
      promotedCoordinate = planCoordinate planIdentifier
  if Set.null debtKeys
    then Left (AlignmentPromotionDebtMissing key)
    else Right ()
  mapM_ (validateGeneration affectedSort topologyCut placementVector) createdGenerations
  case Map.lookup key state.promotions of
    Just incumbent
      | promotionReceiptCoreMatches
          topologyCut
          placementVector
          debtKeys
          generationIds
          (Set.fromList suppliedRelations)
          incumbent ->
          PreparedAlignmentPromotion
            <$> prepareTransition
              (\predecessor -> Right (predecessor, AlignmentPromotionUnchanged))
              state
      | otherwise -> Left (AlignmentPromotionConflict key)
    Nothing
      | Map.member promotedCoordinate state.planInvalidations ->
          Left (AlignmentPromotionPlanInvalidated promotedCoordinate)
    Nothing -> do
      mapM_ (validateGenerationInsertion state.generations) suppliedGenerations
      (newObligations, nextSequence) <-
        buildPromotionObligations
          localHerald
          cause
          affectedSort
          suppliedGenerations
          newRelations
          state.nextObligationSequence
      (newBootstrapImports, finalSequence) <-
        buildPromotionBootstrapImports
          localHerald
          cause
          affectedSort
          createdGenerations
          (retainedBootstrapImportKeys state)
          nextSequence
      let closingIdentifiers =
            supersededClosingObligations
              localHerald
              cause
              affectedSort
              suppliedGenerations
              state
          removedIdentifiers = filter (\identifier -> maybe True (not . retainedRelation) (Map.lookup identifier state.obligations)) closingIdentifiers
      let receipt =
            AlignmentPromotionReceipt
              { topologyCut,
                placementVector,
                debtKeys,
                generationIds,
                relations = Set.fromList suppliedRelations,
                obligationIds = fmap alignmentObligationIdValue newObligations,
                bootstrapImportKeys =
                  fmap bootstrapImportKeyValue newBootstrapImports
              }
      preparedState <-
        prepareTransition
          ( commitPromotion
              key
              receipt
              suppliedGenerations
              suppliedRelations
              newObligations
              newBootstrapImports
              removedIdentifiers
              finalSequence
          )
          state
      Right (PreparedAlignmentPromotion preparedState)
  where
    createdGenerations = case explicitPlan of
      Nothing -> suppliedGenerations
      Just plan -> [Plan.alignmentPlanBindingGeneration binding | (_, binding) <- Plan.alignmentPlanBindings plan, Plan.alignmentPlanBindingDisposition binding == Plan.AlignmentCreated]
    retainedRelation obligation = case explicitPlan of
      Nothing -> False
      Just _ -> any (matches obligation) suppliedRelations
    matches obligation relation =
      alignmentObligationSourceGeneration obligation == alignmentGenerationRelationSource relation
        && alignmentObligationDestinationGeneration obligation == alignmentGenerationRelationDestination relation
        && alignmentObligationFrozenContextPathStrength obligation == alignmentGenerationRelationStrength relation
    newRelations = case explicitPlan of
      Nothing -> suppliedRelations
      Just _ -> filter (\relation -> not (any (\obligation -> matches obligation relation) (Map.elems (Map.withoutKeys state.obligations state.closingObligations)))) suppliedRelations

promotionReceiptCoreMatches ::
  TopologyCutId ->
  PhysicalPlacementRevisionVector ->
  Set StructuralDebtKey ->
  [ContextClassGenerationId] ->
  Set AlignmentGenerationRelation ->
  AlignmentPromotionReceipt ->
  Bool
promotionReceiptCoreMatches topology placement debts generationIds relations receipt =
  receipt.topologyCut == topology
    && receipt.placementVector == placement
    && receipt.debtKeys == debts
    && receipt.generationIds == generationIds
    && receipt.relations == relations

buildPromotionObligations ::
  HeraldEpoch ->
  StructuralConsequenceCause ->
  SortOccurrence ->
  [AlignmentGeneration] ->
  [AlignmentGenerationRelation] ->
  AlignmentObligationSequence ->
  Either
    AlignmentPromotionProblem
    ([AlignmentObligation], AlignmentObligationSequence)
buildPromotionObligations localHerald cause affectedSort generations relations initialSequence = do
  let byId =
        Map.fromList
          [ (alignmentGenerationId generation, generation)
          | generation <- generations
          ]
  foldM (buildOne byId) ([], initialSequence) (sortOn relationKey relations)
  where
    relationKey relation =
      ( alignmentGenerationRelationDestination relation,
        alignmentGenerationRelationSource relation
      )
    buildOne byId (retained, sequenceNumber) relation = do
      destinationGeneration <-
        maybe
          ( Left
              ( AlignmentPromotionRelationGenerationMissing
                  (alignmentGenerationRelationDestination relation)
              )
          )
          Right
          (Map.lookup (alignmentGenerationRelationDestination relation) byId)
      let localStores =
            [ destinationStore
                (alignmentMemberDelta member)
                (alignmentMemberStoreIncarnation member)
            | member <-
                NonEmpty.toList
                  ( alignmentCutExactMembers
                      (alignmentGenerationCut destinationGeneration)
                  ),
              alignmentMemberHerald member == localHerald
            ]
      case NonEmpty.nonEmpty localStores of
        Nothing -> Right (retained, sequenceNumber)
        Just destinations -> do
          obligation <-
            mapLeft AlignmentPromotionObligationProblem
              $ alignmentObligation
                (alignmentObligationId localHerald sequenceNumber)
                cause
                (sortOccurrenceSortId affectedSort)
                (sortOccurrenceDefinition affectedSort)
                (alignmentGenerationRelationDestination relation)
                destinations
                (alignmentGenerationRelationSource relation)
                (alignmentGenerationRelationStrength relation)
          Right
            ( retained <> [obligation],
              nextAlignmentObligationSequence sequenceNumber
            )

buildPromotionBootstrapImports ::
  HeraldEpoch ->
  StructuralConsequenceCause ->
  SortOccurrence ->
  [AlignmentGeneration] ->
  Set BootstrapImportKey ->
  AlignmentObligationSequence ->
  Either
    AlignmentPromotionProblem
    ([BootstrapImport], AlignmentObligationSequence)
buildPromotionBootstrapImports localHerald cause affectedSort generations existingKeys initialSequence =
  foldM buildGeneration ([], initialSequence) orderedGenerations
  where
    orderedGenerations = sortOn alignmentGenerationId generations

    buildGeneration accumulated generation =
      foldM
        (buildDestination generation)
        accumulated
        localDestinations
      where
        cut = alignmentGenerationCut generation
        localDestinations =
          [ destinationStore
              (alignmentMemberDelta member)
              (alignmentMemberStoreIncarnation member)
          | member <- NonEmpty.toList (alignmentCutExactMembers cut),
            alignmentMemberHerald member == localHerald
          ]

    buildDestination generation accumulated destination =
      foldM
        (buildOne generation destination)
        accumulated
        (bootstrapSources generation)

    bootstrapSources generation =
      let cut = alignmentGenerationCut generation
       in sort
            ( fmap
                BootstrapFromPredecessor
                (alignmentCutPredecessorGenerationIds cut)
                <> fmap
                  BootstrapFromFreshBase
                  (alignmentCutFreshMemberBaseEvidence cut)
            )

    buildOne generation destination (retained, sequenceNumber) source = do
      let generationId = alignmentGenerationId generation
          sourceGeneration = case source of
            BootstrapFromPredecessor predecessor -> predecessor
            BootstrapFromFreshBase _ -> generationId
          key = BootstrapImportKey generationId destination source
          alreadyOwned =
            Set.member key existingKeys
              || any ((== key) . bootstrapImportKeyValue) retained
      if alreadyOwned
        then Right (retained, sequenceNumber)
        else do
          envelope <-
            mapLeft AlignmentPromotionObligationProblem
              $ alignmentObligation
                (alignmentObligationId localHerald sequenceNumber)
                cause
                (sortOccurrenceSortId affectedSort)
                (sortOccurrenceDefinition affectedSort)
                generationId
                (NonEmpty.singleton destination)
                sourceGeneration
                Normal
          Right
            ( retained <> [BootstrapImport key envelope],
              nextAlignmentObligationSequence sequenceNumber
            )

-- Bootstrap imports are one-time work for an immutable generation/destination/
-- source coordinate. Active, fulfilled, and terminally retained keys all
-- reserve that identity across later structural occurrences.
retainedBootstrapImportKeys :: State -> Set BootstrapImportKey
retainedBootstrapImportKeys state =
  Set.unions
    [ Map.keysSet state.bootstrapImports,
      Map.keysSet state.bootstrapFulfillments,
      Set.fromList
        [ key
        | receipt <- Map.elems state.planInvalidations,
          (key, _) <- alignmentPlanInvalidationReceiptBootstrapImports receipt
        ],
      Set.fromList
        [ bootstrapImportKeyValue
            ( bootstrapImportAttemptImport
                (bootstrapImportAttemptInvalidationAttempt receipt)
            )
        | receipt <- Map.elems state.bootstrapAttemptInvalidations
        ]
    ]

supersededClosingObligations ::
  HeraldEpoch ->
  StructuralConsequenceCause ->
  SortOccurrence ->
  [AlignmentGeneration] ->
  State ->
  [AlignmentObligationId]
supersededClosingObligations localHerald cause affectedSort generations state =
  [ identifier
  | (identifier, obligation) <- Map.toAscList state.obligations,
    alignmentObligationCause obligation /= cause,
    sortOccurrence
      (alignmentObligationSortId obligation)
      (alignmentObligationSortDefinitionOccurrenceId obligation)
      == affectedSort,
    all
      (`Set.member` retainedDestinations)
      (NonEmpty.toList (alignmentObligationDestinationStores obligation))
  ]
  where
    retainedDestinations =
      Set.fromList
        [ destinationStore
            (alignmentMemberDelta member)
            (alignmentMemberStoreIncarnation member)
        | generation <- generations,
          member <-
            NonEmpty.toList
              (alignmentCutExactMembers (alignmentGenerationCut generation)),
          alignmentMemberHerald member == localHerald
        ]

preparedAlignmentPromotionDisposition ::
  PreparedAlignmentPromotion -> AlignmentPromotionDisposition
preparedAlignmentPromotionDisposition
  (PreparedAlignmentPromotion prepared) = preparedOutput prepared

commitAlignmentPromotion ::
  PreparedAlignmentPromotion -> (State, AlignmentPromotionDisposition)
commitAlignmentPromotion
  (PreparedAlignmentPromotion prepared) = commitPrepared prepared

validateGeneration ::
  SortOccurrence ->
  TopologyCutId ->
  PhysicalPlacementRevisionVector ->
  AlignmentGeneration ->
  Either AlignmentPromotionProblem ()
validateGeneration expectedSort expectedCut expectedPlacement generation = do
  let cut = alignmentGenerationCut generation
      observedSort =
        sortOccurrence
          (alignmentCutSortId cut)
          (alignmentCutSortDefinitionOccurrenceId cut)
      observedCut = deriveTopologyCutId (alignmentCutTopologyCut cut)
      derivedGeneration = deriveContextClassGenerationId cut
  if observedSort == expectedSort
    then Right ()
    else Left (AlignmentPromotionSortMismatch expectedSort observedSort)
  if observedCut == expectedCut
    then Right ()
    else Left (AlignmentPromotionTopologyCutMismatch expectedCut observedCut)
  if alignmentCutPhysicalPlacementRevisionVector cut == expectedPlacement
    then Right ()
    else Left AlignmentPromotionPlacementVectorMismatch
  if alignmentGenerationId generation == derivedGeneration
    then Right ()
    else
      Left
        ( AlignmentPromotionGenerationIdMismatch
            derivedGeneration
            (alignmentGenerationId generation)
        )

validateGenerationInsertion ::
  Map ContextClassGenerationId AlignmentGeneration ->
  AlignmentGeneration ->
  Either AlignmentPromotionProblem ()
validateGenerationInsertion retained candidate =
  case Map.lookup (alignmentGenerationId candidate) retained of
    Nothing -> Right ()
    Just incumbent
      | incumbent == candidate -> Right ()
      | otherwise ->
          Left
            (AlignmentPromotionGenerationConflict (alignmentGenerationId candidate))

-- | Retain a complete, already reconstructed historical plan as immutable
-- evidence. The coordinator authenticates its source and exact topology and
-- placement projection before calling this leaf owner. An observer was not a
-- member of the old placement vector: importing its catalogue must not create
-- old local work, votes, or a new latest frontier. A real old member may only
-- corroborate a plan which its ordinary promotion path already installed.
retainHistoricalAlignmentPlan ::
  HeraldEpoch ->
  AlignmentGenerationPlan ->
  State ->
  Either AlignmentPromotionProblem State
retainHistoricalAlignmentPlan local plan state = case supplied of
  [] -> Right state
  first : _ -> do
    let coordinate = generationPlanCoordinate first
        affectedSort = alignmentPlanCoordinateSort coordinate
        topology = alignmentPlanCoordinateTopologyCut coordinate
        placement = alignmentPlanCoordinatePlacementVector coordinate
        incumbent =
          Map.filter ((== coordinate) . generationPlanCoordinate) state.generations
        incumbentRelations =
          Set.filter
            (\relation -> Map.member (alignmentGenerationRelationSource relation) incumbent)
            state.generationRelations
        localWasMember =
          any ((== local) . fst) (NonEmpty.toList (physicalPlacementRevisionEntries placement))
    mapM_ (validateGeneration affectedSort topology placement) supplied
    mapM_ (validateGenerationInsertion state.generations) supplied
    mapM_
      requirePredecessor
      (Set.toAscList (Set.fromList (concatMap (alignmentCutPredecessorGenerationIds . alignmentGenerationCut) supplied)))
    if not (Map.null incumbent)
      && (incumbent /= suppliedById || incumbentRelations /= suppliedRelations)
      then Left (AlignmentHistoricalPlanConflict coordinate)
      else Right ()
    if localWasMember
      then
        if incumbent == suppliedById && incumbentRelations == suppliedRelations
          then Right state
          else Left (AlignmentHistoricalPlanLocalMember local)
      else
        Right
          ( finishGenerationCreation [generation | generation <- supplied, Map.notMember (alignmentGenerationId generation) state.generations]
              $ (indexNewAlignmentCandidates supplied state)
                { generations = Map.union suppliedById state.generations,
                  generationRelations = Set.union suppliedRelations state.generationRelations
                }
          )
  where
    supplied = alignmentGenerationPlanGenerations plan
    suppliedById = Map.fromList [(alignmentGenerationId generation, generation) | generation <- supplied]
    suppliedRelations = Set.fromList (alignmentGenerationPlanRelations plan)
    requirePredecessor identifier
      | Map.notMember identifier state.generations =
          Left (AlignmentHistoricalPredecessorMissing identifier)
      | otherwise =
          case Map.lookupMin (Map.dropWhileAntitone ((< identifier) . fst) state.certificates) of
            Just ((retained, _), _) | retained == identifier -> Right ()
            _ -> Left (AlignmentHistoricalCertificateMissing identifier)

-- | Admit sealed historical completion after the coordinator has reconstructed
-- its exact plan and proved that its installed topology covers the cause. This
-- is passive semantic evidence: it allocates no obligations, attempts, votes or
-- sequence numbers, and never chooses a latest frontier. Local debt identities
-- are derived from this owner's replay, including those retained later.
retainHistoricalAlignmentCoverage ::
  HeraldEpoch ->
  AlignmentPromotionKey ->
  AlignmentGenerationPlan ->
  State ->
  Either AlignmentPromotionProblem State
retainHistoricalAlignmentCoverage local key plan state = do
  if any ((== local) . fst) (NonEmpty.toList (physicalPlacementRevisionEntries placement))
    then Left (AlignmentHistoricalPlanLocalMember local)
    else Right ()
  retained <- retainHistoricalAlignmentPlan local plan state
  let generations = alignmentGenerationPlanGenerations plan
      identifiers = sort (fmap alignmentGenerationId generations)
      complete = Map.keys (Map.filter ((== coordinate) . generationPlanCoordinate) retained.generations)
  mapM_ (validateGeneration affectedSort topology placement) generations
  if identifiers /= complete
    then Left (AlignmentHistoricalCoverageConflict key)
    else Right ()
  case Map.lookup key retained.historicalCoverage of
    Just incumbent | incumbent /= identifiers -> Left (AlignmentHistoricalCoverageConflict key)
    _ -> Right ()
  case Map.lookup key retained.promotions of
    Just incumbent | sort incumbent.generationIds /= identifiers -> Left (AlignmentHistoricalCoverageConflict key)
    _ -> Right ()
  let historical = Map.insert key identifiers retained.historicalCoverage
      coveredKeys = historicalCoveredDebtKeys retained.structuralDebts historical retained.planInvalidations
      wake = if Map.member key retained.historicalCoverage then id else wakeGenerationPlanSorts (Set.singleton affectedSort)
  Right
    $ refreshLiveDebtScheduling coveredKeys . wake
    $ retained
      { historicalCoverage = historical,
        preparationReadinessChanges =
          if Map.member key retained.historicalCoverage
            then retained.preparationReadinessChanges
            else Set.insert affectedSort retained.preparationReadinessChanges,
        liveStructuralDebts =
          Map.withoutKeys
            retained.liveStructuralDebts
            coveredKeys
      }
  where
    affectedSort = alignmentPromotionKeySort key
    topology = alignmentPromotionKeyTopologyCut key
    placement = alignmentPromotionKeyPlacementVector key
    coordinate = promotionPlanCoordinate key

-- | The semantic latest frontier, retaining explicit empty sort entries and
-- excluding tombstoned coordinates. This is captured only at the canonical
-- old anchor's checked join boundary, never merged by catalogue arrival order.
historicalAlignmentFrontier :: State -> [(SortOccurrence, [ContextClassGenerationId])]
historicalAlignmentFrontier state =
  [ (affectedSort, filter live identifiers)
  | (affectedSort, identifiers) <- Map.toAscList state.latestBySort
  ]
  where
    live identifier = case Map.lookup identifier state.generations of
      Nothing -> False
      Just generation -> not (generationCurrentPlanInvalidated generation state)

-- | Adopt the canonical predecessor anchor's captured frontier for a restricted
-- newcomer. Source authority and the admission boundary belong to the enclosing
-- coordinator. This transition admits only complete historical plan coordinates;
-- exact replay is harmless, while an existing different frontier cannot rewind.
adoptHistoricalAlignmentFrontier ::
  HeraldEpoch ->
  [(SortOccurrence, [ContextClassGenerationId])] ->
  State ->
  Either AlignmentPromotionProblem State
adoptHistoricalAlignmentFrontier local entries state = do
  _ <- foldM validateEntry Set.empty entries
  let changed = Set.fromList [affectedSort | (affectedSort, _) <- entries, Map.notMember affectedSort state.latestBySort]
      wake = if Set.null changed then id else wakeLossCheck . wakeGenerationPlanSorts changed
  Right
    $ wake
    $ state
      { latestBySort = Map.union (Map.fromList entries) state.latestBySort,
        preparationReadinessChanges = foldr (\(affectedSort, _) pending -> if Map.member affectedSort state.latestBySort then pending else Set.insert affectedSort pending) state.preparationReadinessChanges entries
      }
  where
    validateEntry seen (affectedSort, identifiers) = do
      if Set.member affectedSort seen || identifiers /= Set.toAscList (Set.fromList identifiers)
        then Left (AlignmentHistoricalFrontierInvalid affectedSort)
        else Right ()
      generations <- traverse (requireGeneration affectedSort) identifiers
      case generations of
        [] -> Right ()
        first : _ -> do
          let coordinate = generationPlanCoordinate first
              complete =
                Map.keys (Map.filter ((== coordinate) . generationPlanCoordinate) state.generations)
          if complete == identifiers
            then Right ()
            else Left (AlignmentHistoricalFrontierInvalid affectedSort)
      case Map.lookup affectedSort state.latestBySort of
        Just incumbent
          | incumbent /= identifiers ->
              Left (AlignmentHistoricalFrontierConflict affectedSort)
        _ -> Right (Set.insert affectedSort seen)
    requireGeneration affectedSort identifier = case Map.lookup identifier state.generations of
      Nothing -> Left (AlignmentHistoricalPredecessorMissing identifier)
      Just generation
        | alignmentPlanCoordinateSort (generationPlanCoordinate generation) /= affectedSort ->
            Left (AlignmentHistoricalFrontierInvalid affectedSort)
        | any
            ((== local) . fst)
            (NonEmpty.toList (physicalPlacementRevisionEntries (alignmentCutPhysicalPlacementRevisionVector (alignmentGenerationCut generation)))) ->
            Left (AlignmentHistoricalPlanLocalMember local)
        | otherwise -> Right generation

-- | Import a checked mixed-age plan as passive evidence. Only an observer may
-- introduce it; an original member can corroborate its already-owned record.
retainHistoricalAlignmentRoutingPlan :: HeraldEpoch -> Plan.AlignmentPlan -> State -> Either AlignmentPromotionProblem State
retainHistoricalAlignmentRoutingPlan local plan state = do
  ledger <- mapLeft AlignmentPromotionPlanProblem (Plan.retainHistoricalAlignmentPlan plan state.planLedger)
  mapM_ (validateGenerationInsertion state.generations) supplied
  mapM_ (validateGeneration affectedSort topology placement) created
  mapM_ requirePredecessor (Set.toAscList (Set.fromList (concatMap (alignmentCutPredecessorGenerationIds . alignmentGenerationCut) created)))
  if local `elem` fmap fst (physicalPlacementRevisionEntries placement)
    then case lookupAlignmentPlan identifier state of
      Just incumbent | incumbent == plan -> Right state
      _ -> Left (AlignmentHistoricalPlanLocalMember local)
    else
      Right
        ( finishGenerationCreation inserted
            $ (indexNewAlignmentCandidates inserted state)
              { planLedger = ledger,
                generations = Map.union suppliedById state.generations,
                generationRelations = Set.union (Set.fromList (Plan.alignmentPlanRelations plan)) state.generationRelations
              }
        )
  where
    identifier = Plan.alignmentPlanId plan
    affectedSort = Plan.alignmentPlanIdSort identifier
    topology = Plan.alignmentPlanIdTopology identifier
    placement = Plan.alignmentPlanIdPlacement identifier
    supplied = planGenerations plan
    suppliedById = Map.fromList [(alignmentGenerationId generation, generation) | generation <- supplied]
    created = [Plan.alignmentPlanBindingGeneration binding | (_, binding) <- Plan.alignmentPlanBindings plan, Plan.alignmentPlanBindingDisposition binding == Plan.AlignmentCreated]
    inserted = [generation | generation <- supplied, Map.notMember (alignmentGenerationId generation) state.generations]
    requirePredecessor generation
      | Map.notMember generation state.generations = Left (AlignmentHistoricalPredecessorMissing generation)
      | otherwise = case Map.lookupMin (Map.dropWhileAntitone ((< generation) . fst) state.certificates) of
          Just ((retained, _), _) | retained == generation -> Right ()
          _ -> Left (AlignmentHistoricalCertificateMissing generation)

-- | Cause completion refers to the complete explicit plan table, independent of
-- the birth coordinates of generations carried by that plan.
retainHistoricalAlignmentPlanCoverage :: HeraldEpoch -> AlignmentPromotionKey -> Plan.AlignmentPlan -> State -> Either AlignmentPromotionProblem State
retainHistoricalAlignmentPlanCoverage local key plan state = do
  if local `elem` fmap fst (physicalPlacementRevisionEntries placement)
    then Left (AlignmentHistoricalPlanLocalMember local)
    else Right ()
  if Plan.alignmentPlanId plan == coordinatePlanId (promotionPlanCoordinate key)
    then Right ()
    else Left (AlignmentHistoricalCoverageConflict key)
  retained <- retainHistoricalAlignmentRoutingPlan local plan state
  case Map.lookup key retained.historicalCoverage of
    Just incumbent | incumbent /= identifiers -> Left (AlignmentHistoricalCoverageConflict key)
    _ -> Right ()
  case Map.lookup key retained.promotions of
    Just incumbent | sort incumbent.generationIds /= identifiers -> Left (AlignmentHistoricalCoverageConflict key)
    _ -> Right ()
  (ledger, _) <- mapLeft AlignmentPromotionPlanProblem (Plan.retainAlignmentPlan (alignmentPromotionKeyCause key) plan retained.planLedger)
  let historical = Map.insert key identifiers retained.historicalCoverage
      coveredKeys = historicalCoveredDebtKeys retained.structuralDebts historical retained.planInvalidations
      wake = if Map.member key retained.historicalCoverage then id else wakeGenerationPlanSorts (Set.singleton affectedSort)
  Right
    $ refreshLiveDebtScheduling coveredKeys . wake
    $ retained
      { planLedger = ledger,
        historicalCoverage = historical,
        preparationReadinessChanges = if Map.member key retained.historicalCoverage then retained.preparationReadinessChanges else Set.insert affectedSort retained.preparationReadinessChanges,
        liveStructuralDebts = Map.withoutKeys retained.liveStructuralDebts coveredKeys
      }
  where
    affectedSort = alignmentPromotionKeySort key
    placement = alignmentPromotionKeyPlacementVector key
    identifiers = sort (map alignmentGenerationId (planGenerations plan))

historicalAlignmentPlanFrontier :: State -> [(SortOccurrence, Plan.AlignmentPlanId)]
historicalAlignmentPlanFrontier state = [(occurrence, identifier) | (occurrence, identifier) <- Map.toAscList state.currentPlans, not (alignmentPlanInvalidated identifier state)]

-- | Select only the canonical sealed donor's exact current routing plans.
-- Historical imports cannot choose this frontier by their arrival order.
adoptHistoricalAlignmentPlanFrontier :: HeraldEpoch -> [(SortOccurrence, Plan.AlignmentPlanId)] -> State -> Either AlignmentPromotionProblem State
adoptHistoricalAlignmentPlanFrontier local entries state = do
  _ <- foldM validateEntry Set.empty entries
  let changed = Set.fromList [occurrence | (occurrence, _) <- entries, Map.notMember occurrence state.currentPlans]
      generations = Map.fromList [(occurrence, sort (map alignmentGenerationId (planGenerations plan))) | (occurrence, identifier) <- entries, Just plan <- [lookupAlignmentPlan identifier state]]
      wake = if Set.null changed then id else wakeLossCheck . wakeGenerationPlanSorts changed
  Right
    $ wake
    $ state
      { currentPlans = Map.union (Map.fromList entries) state.currentPlans,
        latestBySort = Map.union generations state.latestBySort,
        preparationReadinessChanges = Set.union changed state.preparationReadinessChanges
      }
  where
    validateEntry seen (occurrence, identifier) = do
      if Set.member occurrence seen || Plan.alignmentPlanIdSort identifier /= occurrence || alignmentPlanInvalidated identifier state
        then Left (AlignmentHistoricalFrontierInvalid occurrence)
        else Right ()
      plan <- maybe (Left (AlignmentHistoricalFrontierInvalid occurrence)) Right (lookupAlignmentPlan identifier state)
      if local `elem` fmap fst (physicalPlacementRevisionEntries (Plan.alignmentPlanIdPlacement identifier))
        then Left (AlignmentHistoricalPlanLocalMember local)
        else Right ()
      mapM_
        ( \generation -> case Map.lookup (alignmentGenerationId generation) state.generations of
            Just retained | retained == generation -> Right ()
            _ -> Left (AlignmentHistoricalPredecessorMissing (alignmentGenerationId generation))
        )
        (planGenerations plan)
      case Map.lookup occurrence state.currentPlans of
        Just incumbent | incumbent /= identifier -> Left (AlignmentHistoricalFrontierConflict occurrence)
        _ -> Right (Set.insert occurrence seen)

commitPromotion ::
  AlignmentPromotionKey ->
  AlignmentPromotionReceipt ->
  [AlignmentGeneration] ->
  [AlignmentGenerationRelation] ->
  [AlignmentObligation] ->
  [BootstrapImport] ->
  [AlignmentObligationId] ->
  AlignmentObligationSequence ->
  State ->
  Either problem (State, AlignmentPromotionDisposition)
commitPromotion key receipt suppliedGenerations suppliedRelations newObligations newBootstrapImports closingIdentifiers nextSequence predecessor =
  Right
    ( finishPromotionWork suppliedGenerations newObligations newBootstrapImports predecessor
        . refreshLiveDebtScheduling receipt.debtKeys
        . wakeLossCheck
        . wakeGenerationPlanSorts (Set.singleton (alignmentPromotionKeySort key))
        $ ( notifyPreparationReadiness (alignmentPromotionKeySort key)
              . indexNewAlignmentCandidates suppliedGenerations
              . insertInputClosureOwners newObligations
              . notifyInputClosureOwners
                [ obligation
                | identifier <- closingIdentifiers,
                  Set.notMember identifier predecessor.closingObligations,
                  Just obligation <- [Map.lookup identifier predecessor.obligations]
                ]
              $ predecessor
          )
          { promotions = Map.insert key receipt predecessor.promotions,
            liveStructuralDebts = Map.withoutKeys predecessor.liveStructuralDebts receipt.debtKeys,
            generations =
              foldl
                (\retained generation -> Map.insert (alignmentGenerationId generation) generation retained)
                predecessor.generations
                suppliedGenerations,
            generationRelations =
              Set.union
                predecessor.generationRelations
                (Set.fromList suppliedRelations),
            latestBySort =
              Map.insert
                (alignmentPromotionKeySort key)
                (sort (fmap alignmentGenerationId suppliedGenerations))
                predecessor.latestBySort,
            nextObligationSequence = nextSequence,
            obligations =
              foldl
                (\retained obligation -> Map.insert (alignmentObligationIdValue obligation) obligation retained)
                predecessor.obligations
                newObligations,
            closingObligations =
              Set.union
                predecessor.closingObligations
                (Set.fromList closingIdentifiers),
            bootstrapImports =
              foldl
                ( \retained bootstrapImport ->
                    Map.insert
                      (bootstrapImportKeyValue bootstrapImport)
                      bootstrapImport
                      retained
                )
                predecessor.bootstrapImports
                newBootstrapImports
          },
      AlignmentPromotionCommitted
    )

mapLeft :: (left -> mapped) -> Either left value -> Either mapped value
mapLeft mapping = either (Left . mapping) Right

data AlignmentCutEvidenceDisposition
  = AlignmentCutEvidenceRetained
  | AlignmentCutEvidenceUnchanged
  deriving stock (Eq, Ord, Show)

data AlignmentCutEvidenceProblem
  = AlignmentPlanEvidenceUnknown Plan.AlignmentPlanId
  | AlignmentPlanEvidenceConflict Plan.AlignmentPlanId HeraldEpoch
  | AlignmentCutEvidenceUnknownGeneration ContextClassGenerationId
  | AlignmentCutEvidenceNotPromoted ContextClassGenerationId
  | AlignmentCutEvidenceTopologyMismatch TopologyCutId TopologyCutId
  | AlignmentCutEvidencePlacementMismatch
  | AlignmentCutEvidenceForeignAcceptor HeraldEpoch
  | AlignmentCutEvidenceConflict ContextClassGenerationId HeraldEpoch
  deriving stock (Eq, Show)

newtype PreparedAlignmentCutAcceptance
  = PreparedAlignmentCutAcceptance
      (Prepared State AlignmentCutEvidenceDisposition)

prepareAlignmentCutAcceptance ::
  AlignmentCutAccepted ->
  State ->
  Either AlignmentCutEvidenceProblem PreparedAlignmentCutAcceptance
prepareAlignmentCutAcceptance accepted state = do
  let generationId = alignmentCutAcceptedGeneration accepted
      acceptor = alignmentCutAcceptedHerald accepted
      key = (generationId, acceptor)
  generation <-
    maybe
      (Left (AlignmentCutEvidenceUnknownGeneration generationId))
      Right
      (Map.lookup generationId state.generations)
  let cut = alignmentGenerationCut generation
      expectedTopology = deriveTopologyCutId (alignmentCutTopologyCut cut)
      expectedPlacement = alignmentCutPhysicalPlacementRevisionVector cut
  if alignmentCutAcceptedTopologyCut accepted == expectedTopology
    then Right ()
    else
      Left
        ( AlignmentCutEvidenceTopologyMismatch
            expectedTopology
            (alignmentCutAcceptedTopologyCut accepted)
        )
  if alignmentCutAcceptedPlacementVector accepted == expectedPlacement
    then Right ()
    else Left AlignmentCutEvidencePlacementMismatch
  if any ((== acceptor) . fst) (NonEmpty.toList (physicalPlacementRevisionEntries expectedPlacement))
    then Right ()
    else Left (AlignmentCutEvidenceForeignAcceptor acceptor)
  if any (elem generationId . (.generationIds) . snd) (Map.toAscList state.promotions)
    then Right ()
    else Left (AlignmentCutEvidenceNotPromoted generationId)
  case Map.lookup key state.cutAcceptances of
    Just incumbent
      | incumbent == accepted ->
          PreparedAlignmentCutAcceptance
            <$> prepareTransition unchanged state
      | otherwise -> Left (AlignmentCutEvidenceConflict generationId acceptor)
    Nothing ->
      PreparedAlignmentCutAcceptance
        <$> prepareTransition
          ( \predecessor ->
              Right
                ( (notifyTransferGenerationEvidence generationId predecessor)
                    { cutAcceptances =
                        Map.insert key accepted predecessor.cutAcceptances,
                      routeMarkerChanges = Set.insert generationId predecessor.routeMarkerChanges
                    },
                  AlignmentCutEvidenceRetained
                )
          )
          state
  where
    unchanged predecessor =
      Right (predecessor, AlignmentCutEvidenceUnchanged)

preparedAlignmentCutAcceptanceDisposition ::
  PreparedAlignmentCutAcceptance -> AlignmentCutEvidenceDisposition
preparedAlignmentCutAcceptanceDisposition
  (PreparedAlignmentCutAcceptance prepared) = preparedOutput prepared

commitAlignmentCutAcceptance ::
  PreparedAlignmentCutAcceptance -> (State, AlignmentCutEvidenceDisposition)
commitAlignmentCutAcceptance (PreparedAlignmentCutAcceptance prepared) =
  commitPrepared prepared

data AlignmentReadinessDisposition
  = AlignmentReadinessRetained
  | AlignmentReadinessUnchanged
  deriving stock (Eq, Ord, Show)

data AlignmentReadinessProblem
  = AlignmentReadinessUnknownGeneration ContextClassGenerationId
  | AlignmentReadinessStoreNotMember
      ContextClassGenerationId
      StoreIncarnationId
  | AlignmentReadinessConflict ContextClassGenerationId StoreIncarnationId
  | AlignmentReadinessEvidenceMissing
      ContextClassGenerationId
      StoreIncarnationId
  | AlignmentCertificateMembersIncomplete ContextClassGenerationId
  | AlignmentCertificateReadyDigestMismatch
      MemberReadyEvidenceDigest
      MemberReadyEvidenceDigest
  | AlignmentCertificatePrefixDigestMismatch
      BootstrapEvidenceDigest
      BootstrapEvidenceDigest
  | AlignmentCertificateRevisionMismatch
  deriving stock (Eq, Show)

newtype PreparedClassMemberReady
  = PreparedClassMemberReady
      (Prepared State AlignmentReadinessDisposition)

prepareClassMemberReady ::
  ClassMemberReady ->
  State ->
  Either AlignmentReadinessProblem PreparedClassMemberReady
prepareClassMemberReady ready state = do
  let generationId = classMemberReadyGeneration ready
      store = classMemberReadyStoreIncarnation ready
      key = (generationId, store)
  generation <- requireReadinessGeneration generationId state
  requireGenerationStore generationId store generation
  case Map.lookup key state.memberReadiness of
    Just incumbent
      | incumbent == ready ->
          PreparedClassMemberReady <$> prepareTransition unchanged state
      | otherwise -> Left (AlignmentReadinessConflict generationId store)
    Nothing ->
      PreparedClassMemberReady
        <$> prepareTransition
          ( \predecessor ->
              Right
                ( (notifyTransferGenerationReadiness generationId (notifyGenerationEvidence (Set.singleton (GenerationMemberReadiness generationId)) predecessor))
                    { memberReadiness =
                        Map.insert key ready predecessor.memberReadiness
                    },
                  AlignmentReadinessRetained
                )
          )
          state
  where
    unchanged predecessor = Right (predecessor, AlignmentReadinessUnchanged)

commitClassMemberReady ::
  PreparedClassMemberReady -> (State, AlignmentReadinessDisposition)
commitClassMemberReady (PreparedClassMemberReady prepared) =
  commitPrepared prepared

data PreparedLocalClassMemberReady = PreparedLocalClassMemberReady
  { preparedState :: Prepared State AlignmentReadinessDisposition,
    ready :: ClassMemberReady
  }

-- | Allocate this Herald owner's positive evidence sequence exactly once for
-- one local generation/store coordinate. Retry returns the immutable retained
-- evidence and does not consume another sequence number.
prepareLocalClassMemberReady ::
  ContextClassGenerationId ->
  StoreIncarnationId ->
  MemberReadyEvidenceDigest ->
  StoreRevision ->
  State ->
  Either AlignmentReadinessProblem PreparedLocalClassMemberReady
prepareLocalClassMemberReady generationId store prefixDigest revision state = do
  generation <- requireReadinessGeneration generationId state
  requireGenerationStore generationId store generation
  let key = (generationId, store)
  case Map.lookup key state.localMemberReadiness of
    Just incumbent
      | localReadyCoreMatches prefixDigest revision incumbent -> do
          prepared <- prepareTransition unchanged state
          Right
            PreparedLocalClassMemberReady
              { preparedState = prepared,
                ready = incumbent
              }
      | otherwise -> Left (AlignmentReadinessConflict generationId store)
    Nothing -> case Map.lookup key state.memberReadiness of
      Just _ -> Left (AlignmentReadinessConflict generationId store)
      Nothing -> do
        let created =
              classMemberReady
                state.nextEvidenceSequence
                generationId
                store
                prefixDigest
                revision
        prepared <-
          prepareTransition
            ( \predecessor ->
                Right
                  ( (notifyTransferGenerationReadiness generationId . notifyGenerationEvidence (Set.singleton (GenerationMemberReadiness generationId)) $ (notifyPreparationReadiness (alignmentPlanCoordinateSort (generationPlanCoordinate generation)) predecessor))
                      { memberReadiness =
                          Map.insert key created predecessor.memberReadiness,
                        localMemberReadiness =
                          Map.insert key created predecessor.localMemberReadiness,
                        nextEvidenceSequence =
                          nextAlignmentEvidenceSequence predecessor.nextEvidenceSequence
                      },
                    AlignmentReadinessRetained
                  )
            )
            state
        Right
          PreparedLocalClassMemberReady
            { preparedState = prepared,
              ready = created
            }
  where
    unchanged predecessor = Right (predecessor, AlignmentReadinessUnchanged)

    localReadyCoreMatches expectedDigest expectedRevision incumbent =
      classMemberReadyGeneration incumbent == generationId
        && classMemberReadyStoreIncarnation incumbent == store
        && classMemberReadyPredecessorAndBasePrefixDigest incumbent
          == expectedDigest
        && classMemberReadyStoreRevision incumbent == expectedRevision

preparedLocalClassMemberReady ::
  PreparedLocalClassMemberReady -> ClassMemberReady
preparedLocalClassMemberReady prepared = prepared.ready

preparedLocalClassMemberReadyDisposition ::
  PreparedLocalClassMemberReady -> AlignmentReadinessDisposition
preparedLocalClassMemberReadyDisposition prepared =
  preparedOutput prepared.preparedState

commitLocalClassMemberReady ::
  PreparedLocalClassMemberReady ->
  (State, AlignmentReadinessDisposition)
commitLocalClassMemberReady prepared =
  let (successor, disposition) = commitPrepared prepared.preparedState
      ready = prepared.ready
   in ( retireCompletedAlignmentMember
          (classMemberReadyGeneration ready)
          (classMemberReadyStoreIncarnation ready)
          successor,
        disposition
      )

newtype PreparedHistoricalCertificate
  = PreparedHistoricalCertificate
      (Prepared State AlignmentReadinessDisposition)

prepareHistoricalCertificate ::
  HistoricalCertificate ->
  State ->
  Either AlignmentReadinessProblem PreparedHistoricalCertificate
prepareHistoricalCertificate certificate state = do
  let generationId = historicalCertificateClassGeneration certificate
      store = historicalCertificateSourceStoreIncarnation certificate
      key = (generationId, store)
  generation <- requireReadinessGeneration generationId state
  requireGenerationStore generationId store generation
  let memberStores =
        Set.fromList
          ( fmap
              alignmentMemberStoreIncarnation
              (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)))
          )
      ready =
        [ evidence
        | ((retainedGeneration, retainedStore), evidence) <-
            Map.toAscList state.memberReadiness,
          retainedGeneration == generationId,
          Set.member retainedStore memberStores
        ]
  if Set.fromList (fmap classMemberReadyStoreIncarnation ready) == memberStores
    then Right ()
    else Left (AlignmentCertificateMembersIncomplete generationId)
  let expectedDigest = classMemberReadySetDigest ready
      observedDigest = historicalCertificateExactMemberReadyDigest certificate
  if observedDigest == expectedDigest
    then Right ()
    else
      Left
        (AlignmentCertificateReadyDigestMismatch expectedDigest observedDigest)
  sourceReady <-
    maybe
      (Left (AlignmentReadinessEvidenceMissing generationId store))
      Right
      (Map.lookup key state.memberReadiness)
  let expectedPrefixDigest =
        deriveBootstrapEvidenceDigest
          ( memberReadyEvidenceDigestBytes
              (classMemberReadyPredecessorAndBasePrefixDigest sourceReady)
          )
      observedPrefixDigest =
        historicalCertificateCompletedPredecessorPrefixDigest certificate
  if observedPrefixDigest == expectedPrefixDigest
    then Right ()
    else
      Left
        ( AlignmentCertificatePrefixDigestMismatch
            expectedPrefixDigest
            observedPrefixDigest
        )
  if historicalCertificateCertifiedStoreRevision certificate
    == classMemberReadyStoreRevision sourceReady
    then Right ()
    else Left AlignmentCertificateRevisionMismatch
  case Map.lookup key state.certificates of
    Just incumbent
      | incumbent == certificate ->
          PreparedHistoricalCertificate <$> prepareTransition unchanged state
      | otherwise -> Left (AlignmentReadinessConflict generationId store)
    Nothing ->
      PreparedHistoricalCertificate
        <$> prepareTransition
          ( \predecessor ->
              Right
                ( retireCompletedAlignmentMember
                    generationId
                    store
                    ( notifyTransferGenerationEvidence generationId
                        . wakeGenerationPlanSorts
                          (Set.singleton (alignmentPlanCoordinateSort (generationPlanCoordinate generation)))
                        $ predecessor {certificates = Map.insert key certificate predecessor.certificates}
                    ),
                  AlignmentReadinessRetained
                )
          )
          state
  where
    unchanged predecessor = Right (predecessor, AlignmentReadinessUnchanged)

commitHistoricalCertificate ::
  PreparedHistoricalCertificate -> (State, AlignmentReadinessDisposition)
commitHistoricalCertificate (PreparedHistoricalCertificate prepared) =
  commitPrepared prepared

requireReadinessGeneration ::
  ContextClassGenerationId ->
  State ->
  Either AlignmentReadinessProblem AlignmentGeneration
requireReadinessGeneration generationId state =
  maybe
    (Left (AlignmentReadinessUnknownGeneration generationId))
    Right
    (Map.lookup generationId state.generations)

requireGenerationStore ::
  ContextClassGenerationId ->
  StoreIncarnationId ->
  AlignmentGeneration ->
  Either AlignmentReadinessProblem ()
requireGenerationStore generationId store generation
  | any
      ((== store) . alignmentMemberStoreIncarnation)
      (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation))) =
      Right ()
  | otherwise = Left (AlignmentReadinessStoreNotMember generationId store)

data AlignmentAttemptDisposition
  = AlignmentAttemptCreated
  | AlignmentAttemptUnchanged
  deriving stock (Eq, Ord, Show)

data AlignmentAttemptProblem
  = AlignmentAttemptObligationMissing AlignmentObligationId
  | AlignmentAttemptDestinationAcceptanceUnavailable AlignmentObligationId
  | AlignmentAttemptSourceEvidenceUnavailable AlignmentObligationId
  | AlignmentAttemptIncumbentSourceExcluded
      AlignmentObligationId
      AlignmentSourceCoordinate
  | AlignmentAttemptProtocolProblem AlignmentProtocolProblem
  deriving stock (Eq, Show)

data PreparedAlignmentAttempt = PreparedAlignmentAttempt
  { preparedState :: Prepared State AlignmentAttemptDisposition,
    attempt :: AlignmentAttempt
  }

prepareAlignmentAttempt ::
  AlignmentObligationId ->
  State ->
  Either AlignmentAttemptProblem PreparedAlignmentAttempt
prepareAlignmentAttempt = prepareAlignmentAttemptExcluding Set.empty

-- | Prepare an attempt while honoring the owner's durable exact source-loss
-- coordinates plus compatibility exclusions supplied by a composing
-- transition.
prepareAlignmentAttemptExcluding ::
  Set AlignmentSourceCoordinate ->
  AlignmentObligationId ->
  State ->
  Either AlignmentAttemptProblem PreparedAlignmentAttempt
prepareAlignmentAttemptExcluding additionalFailedSources identifier state = do
  obligation <-
    maybe
      (Left (AlignmentAttemptObligationMissing identifier))
      Right
      (Map.lookup identifier state.obligations)
  if Map.member
    ( alignmentObligationDestinationGeneration obligation,
      alignmentObligationIdDestinationHerald identifier
    )
    state.cutAcceptances
    then Right ()
    else Left (AlignmentAttemptDestinationAcceptanceUnavailable identifier)
  case Map.lookup identifier state.attempts of
    Just incumbent
      | alignmentAttemptSourceExcluded
          additionalFailedSources
          incumbent
          state ->
          Left
            ( AlignmentAttemptIncumbentSourceExcluded
                identifier
                (attemptSourceCoordinate incumbent)
            )
    Just incumbent ->
      PreparedAlignmentAttempt
        <$> prepareTransition unchanged state
        <*> pure incumbent
    Nothing -> do
      (sourceHerald, certificate) <-
        maybe
          (Left (AlignmentAttemptSourceEvidenceUnavailable identifier))
          Right
          (selectAlignmentSource additionalFailedSources obligation state)
      let subscription =
            alignmentSubscriptionId
              (alignmentObligationIdDestinationHerald identifier)
              state.nextSubscriptionSequence
      created <-
        mapLeft
          AlignmentAttemptProtocolProblem
          (alignmentAttempt obligation subscription sourceHerald certificate)
      prepared <-
        prepareTransition
          ( \predecessor ->
              Right
                ( wakeTransferObligationMembers obligation
                    $ predecessor
                      { attempts = Map.insert identifier created predecessor.attempts,
                        nextSubscriptionSequence =
                          nextAlignmentSubscriptionSequence predecessor.nextSubscriptionSequence
                      },
                  AlignmentAttemptCreated
                )
          )
          state
      Right PreparedAlignmentAttempt {preparedState = prepared, attempt = created}
  where
    unchanged predecessor = Right (predecessor, AlignmentAttemptUnchanged)

alignmentAttemptSourceExcluded ::
  Set AlignmentSourceCoordinate -> AlignmentAttempt -> State -> Bool
alignmentAttemptSourceExcluded additionalFailedSources attempt state =
  Set.member (attemptSourceCoordinate attempt) additionalFailedSources
    || maybe
      False
      (`Set.member` state.unavailableSources)
      (alignmentAttemptUnavailableSourceCoordinate attempt state)

preparedAlignmentAttempt :: PreparedAlignmentAttempt -> AlignmentAttempt
preparedAlignmentAttempt prepared = prepared.attempt

preparedAlignmentAttemptDisposition ::
  PreparedAlignmentAttempt -> AlignmentAttemptDisposition
preparedAlignmentAttemptDisposition prepared =
  preparedOutput prepared.preparedState

commitAlignmentAttempt ::
  PreparedAlignmentAttempt -> (State, AlignmentAttemptDisposition)
commitAlignmentAttempt prepared = commitPrepared prepared.preparedState

data AlignmentObligationClosureCompletionDisposition
  = AlignmentObligationClosureCompleted
  | AlignmentObligationClosureCompletionUnchanged
  deriving stock (Eq, Ord, Show)

data AlignmentObligationClosureCompletionProblem
  = AlignmentObligationClosureCompletionMissing AlignmentObligationId
  | AlignmentObligationClosureCompletionNotClosing AlignmentObligationId
  | AlignmentObligationClosureCompletionTransferProblem
      Transfer.AlignmentTransferProblem
  deriving stock (Eq, Show)

data PreparedAlignmentObligationClosureCompletion
  = PreparedAlignmentObligationClosureCompletion
      (Prepared State AlignmentObligationClosureCompletionDisposition)
      [AlignmentCancel]

-- | Retire one stable input-closure owner once its exact route-marker/Live
-- lower bound is complete. Every prior source attempt for the owner is
-- upgraded to RelationRemoved, so reconnect reoffers only terminal Cancels.
-- The logical tombstone exists even when no supplier attempt is current.
prepareAlignmentObligationClosureCompletion ::
  AlignmentObligationId ->
  State ->
  Either
    AlignmentObligationClosureCompletionProblem
    PreparedAlignmentObligationClosureCompletion
prepareAlignmentObligationClosureCompletion identifier state =
  case Map.lookup identifier state.closedObligations of
    Just _ -> do
      prepared <-
        prepareTransition
          ( \predecessor ->
              Right
                ( predecessor,
                  AlignmentObligationClosureCompletionUnchanged
                )
          )
          state
      Right (PreparedAlignmentObligationClosureCompletion prepared [])
    Nothing -> do
      obligation <-
        maybe
          (Left (AlignmentObligationClosureCompletionMissing identifier))
          Right
          (Map.lookup identifier state.obligations)
      if Set.member identifier state.closingObligations
        then Right ()
        else Left (AlignmentObligationClosureCompletionNotClosing identifier)
      let currentAttempt = Map.lookup identifier state.attempts
          priorReceipts =
            [ (subscription, receipt)
            | (subscription, receipt) <- Map.toAscList state.attemptInvalidations,
              alignmentAttemptObligationId
                (alignmentAttemptInvalidationAttempt receipt)
                == identifier
            ]
          successorReceipts =
            foldl
              ( \retained (subscription, receipt) ->
                  Map.insert
                    subscription
                    ( AlignmentAttemptInvalidationReceipt
                        (alignmentAttemptInvalidationAttempt receipt)
                        ( Set.insert
                            AlignmentRelationRemoved
                            (alignmentAttemptInvalidationReasons receipt)
                        )
                    )
                    retained
              )
              state.attemptInvalidations
              priorReceipts
          finalReceipts = case currentAttempt of
            Nothing -> successorReceipts
            Just attempt ->
              Map.insert
                (alignmentAttemptSubscriptionId attempt)
                ( AlignmentAttemptInvalidationReceipt
                    attempt
                    (Set.singleton AlignmentRelationRemoved)
                )
                successorReceipts
          subscriptions =
            Set.toAscList
              ( Set.fromList
                  ( fmap fst priorReceipts
                      <> maybe [] (pure . alignmentAttemptSubscriptionId) currentAttempt
                  )
              )
      (successorTransfer, cancellations) <-
        foldM retainClosureCancellation (state.transfer, []) subscriptions
      prepared <-
        prepareTransition
          ( \predecessor ->
              Right
                ( wakeLossCheck . removeTransferObligation obligation
                    $ (notifyInputClosureOwners [obligation] predecessor)
                      { obligations = Map.delete identifier predecessor.obligations,
                        closingObligations =
                          Set.delete identifier predecessor.closingObligations,
                        closedObligations =
                          Map.insert identifier obligation predecessor.closedObligations,
                        attempts = Map.delete identifier predecessor.attempts,
                        attemptInvalidations = finalReceipts,
                        transfer = successorTransfer
                      },
                  AlignmentObligationClosureCompleted
                )
          )
          state
      Right
        ( PreparedAlignmentObligationClosureCompletion
            prepared
            cancellations
        )
  where
    retainClosureCancellation (transfer, cancellations) subscription = do
      let cancellation = alignmentCancel subscription AlignmentRelationRemoved
      prepared <-
        mapLeft
          AlignmentObligationClosureCompletionTransferProblem
          (Transfer.prepareAlignmentCancellation cancellation transfer)
      let (successor, _) = Transfer.commitAlignmentCancellation prepared
      Right (successor, cancellations <> [cancellation])

preparedAlignmentObligationClosureCompletionCancellations ::
  PreparedAlignmentObligationClosureCompletion -> [AlignmentCancel]
preparedAlignmentObligationClosureCompletionCancellations
  (PreparedAlignmentObligationClosureCompletion _ cancellations) = cancellations

preparedAlignmentObligationClosureCompletionDisposition ::
  PreparedAlignmentObligationClosureCompletion ->
  AlignmentObligationClosureCompletionDisposition
preparedAlignmentObligationClosureCompletionDisposition
  (PreparedAlignmentObligationClosureCompletion prepared _) =
    preparedOutput prepared

commitAlignmentObligationClosureCompletion ::
  PreparedAlignmentObligationClosureCompletion ->
  (State, AlignmentObligationClosureCompletionDisposition)
commitAlignmentObligationClosureCompletion
  (PreparedAlignmentObligationClosureCompletion prepared _) =
    commitPrepared prepared

data AlignmentAttemptInvalidationDisposition
  = AlignmentAttemptInvalidated
  | AlignmentAttemptInvalidationUnchanged
  deriving stock (Eq, Ord, Show)

data AlignmentAttemptInvalidationProblem
  = AlignmentAttemptInvalidationMissing AlignmentObligationId
  | AlignmentAttemptInvalidationConflict AlignmentObligationId
  | AlignmentAttemptInvalidationRequiresPlanInvalidation
      ContextClassGenerationId
  | AlignmentAttemptInvalidationRequiresClosureCompletion
      AlignmentObligationId
  | AlignmentAttemptInvalidationTransferProblem Transfer.AlignmentTransferProblem
  deriving stock (Eq, Show)

data PreparedAlignmentAttemptInvalidation
  = PreparedAlignmentAttemptInvalidation
      (Prepared State AlignmentAttemptInvalidationDisposition)
      AlignmentAttemptInvalidationReceipt
      AlignmentCancel

-- | Terminalize one exact supplier-qualified attempt. Source-incarnation loss
-- preserves the unsourced obligation and globally excludes the failed
-- supplier before allocating a fresh subscription/attempt. Destination loss
-- or relation removal removes the logical obligation so installed generation
-- evidence can derive any successor work afresh. Equal replay returns the
-- retained receipt and cancellation without allocating anything.
prepareAlignmentAttemptInvalidation ::
  AlignmentAttempt ->
  AlignmentCancelReason ->
  State ->
  Either
    AlignmentAttemptInvalidationProblem
    PreparedAlignmentAttemptInvalidation
prepareAlignmentAttemptInvalidation suppliedAttempt reason state =
  if reason == AlignmentDestinationIncarnationLost
    then
      Left
        ( AlignmentAttemptInvalidationRequiresPlanInvalidation
            ( alignmentObligationDestinationGeneration
                (alignmentAttemptObligation suppliedAttempt)
            )
        )
    else
      if reason == AlignmentRelationRemoved
        then
          Left
            (AlignmentAttemptInvalidationRequiresClosureCompletion identifier)
        else case Map.lookup subscription state.attemptInvalidations of
          Just incumbent
            | alignmentAttemptInvalidationAttempt incumbent /= suppliedAttempt ->
                Left (AlignmentAttemptInvalidationConflict identifier)
            | Set.member reason (alignmentAttemptInvalidationReasons incumbent) ->
                unchanged incumbent
            | otherwise -> retain (alignmentAttemptInvalidationReasons incumbent)
          Nothing -> do
            incumbent <-
              maybe
                (Left (AlignmentAttemptInvalidationMissing identifier))
                Right
                (Map.lookup identifier state.attempts)
            if incumbent == suppliedAttempt
              then retain Set.empty
              else Left (AlignmentAttemptInvalidationConflict identifier)
  where
    identifier = alignmentAttemptObligationId suppliedAttempt
    subscription = alignmentAttemptSubscriptionId suppliedAttempt
    unchanged receipt = do
      prepared <-
        prepareTransition
          (\predecessor -> Right (predecessor, AlignmentAttemptInvalidationUnchanged))
          state
      Right
        ( PreparedAlignmentAttemptInvalidation
            prepared
            receipt
            ( alignmentCancel
                subscription
                (alignmentAttemptInvalidationReason receipt)
            )
        )
    retain retainedReasons = do
      let reasons = Set.insert reason retainedReasons
          receipt = AlignmentAttemptInvalidationReceipt suppliedAttempt reasons
          cancellation =
            alignmentCancel subscription (dominantAlignmentCancelReason reasons)
      preparedCancellation <-
        mapLeft
          AlignmentAttemptInvalidationTransferProblem
          (Transfer.prepareAlignmentCancellation cancellation state.transfer)
      let (successorTransfer, _) =
            Transfer.commitAlignmentCancellation preparedCancellation
      prepared <-
        prepareTransition
          ( \predecessor ->
              Right
                ( finishOrdinaryAttemptInvalidation suppliedAttempt predecessor
                    $ ( removeInputClosureOwners
                          [ obligation
                          | Set.member AlignmentRelationRemoved reasons,
                            Just obligation <- [Map.lookup identifier predecessor.obligations]
                          ]
                          predecessor
                      )
                      { obligations =
                          if Set.member AlignmentRelationRemoved reasons
                            then Map.delete identifier predecessor.obligations
                            else predecessor.obligations,
                        attempts = Map.delete identifier predecessor.attempts,
                        unavailableSources =
                          retainAttemptUnavailableSource
                            reasons
                            suppliedAttempt
                            predecessor,
                        closingObligations =
                          if Set.member AlignmentRelationRemoved reasons
                            then Set.delete identifier predecessor.closingObligations
                            else predecessor.closingObligations,
                        attemptInvalidations =
                          Map.insert
                            subscription
                            receipt
                            predecessor.attemptInvalidations,
                        transfer = successorTransfer
                      },
                  AlignmentAttemptInvalidated
                )
          )
          state
      Right
        ( PreparedAlignmentAttemptInvalidation
            prepared
            receipt
            cancellation
        )

retainAttemptUnavailableSource ::
  Set AlignmentCancelReason ->
  AlignmentAttempt ->
  State ->
  Set UnavailableAlignmentSource
retainAttemptUnavailableSource reasons attempt state
  | Set.member AlignmentSourceIncarnationLost reasons =
      maybe
        state.unavailableSources
        (`Set.insert` state.unavailableSources)
        (alignmentAttemptUnavailableSourceCoordinate attempt state)
  | otherwise = state.unavailableSources

preparedAlignmentAttemptInvalidationReceipt ::
  PreparedAlignmentAttemptInvalidation -> AlignmentAttemptInvalidationReceipt
preparedAlignmentAttemptInvalidationReceipt
  (PreparedAlignmentAttemptInvalidation _ receipt _) = receipt

preparedAlignmentAttemptInvalidationCancellation ::
  PreparedAlignmentAttemptInvalidation -> AlignmentCancel
preparedAlignmentAttemptInvalidationCancellation
  (PreparedAlignmentAttemptInvalidation _ _ cancellation) = cancellation

preparedAlignmentAttemptInvalidationDisposition ::
  PreparedAlignmentAttemptInvalidation -> AlignmentAttemptInvalidationDisposition
preparedAlignmentAttemptInvalidationDisposition
  (PreparedAlignmentAttemptInvalidation prepared _ _) = preparedOutput prepared

commitAlignmentAttemptInvalidation ::
  PreparedAlignmentAttemptInvalidation ->
  (State, AlignmentAttemptInvalidationDisposition)
commitAlignmentAttemptInvalidation
  (PreparedAlignmentAttemptInvalidation prepared _ _) = commitPrepared prepared

data AlignmentPlanInvalidationDisposition
  = AlignmentPlanInvalidated
  | AlignmentPlanInvalidationUnchanged
  deriving stock (Eq, Ord, Show)

data AlignmentPlanInvalidationProblem
  = AlignmentPlanInvalidationGenerationMissing ContextClassGenerationId
  | AlignmentPlanInvalidationCauseMismatch AlignmentPlanInvalidationCause
  | AlignmentPlanInvalidationTransferProblem Transfer.AlignmentTransferProblem
  deriving stock (Eq, Show)

data PreparedAlignmentPlanInvalidation
  = PreparedAlignmentPlanInvalidation
      (Prepared State AlignmentPlanInvalidationDisposition)
      AlignmentPlanInvalidationReceipt
      [AlignmentCancel]

-- | Retire every destination owner in one exact cut/placement/sort plan.
-- Generations and their evidence remain immutable ancestry for retained-cut
-- replay and audit; only executable owners disappear. Tombstoned generations
-- are ineligible as prior-latest inputs to every newly derived replacement,
-- regardless of whether their historical certificate arrives. A later
-- placement vector is a distinct promotion key and deterministically derives
-- fresh owners.
prepareAlignmentPlanInvalidation ::
  AlignmentPlanInvalidationCause ->
  State ->
  Either AlignmentPlanInvalidationProblem PreparedAlignmentPlanInvalidation
prepareAlignmentPlanInvalidation cause state = do
  targetGeneration <- maybe (Left (AlignmentPlanInvalidationCauseMismatch cause)) Right (planInvalidationCauseGeneration cause)
  generation <-
    maybe
      (Left (AlignmentPlanInvalidationGenerationMissing targetGeneration))
      Right
      (Map.lookup targetGeneration state.generations)
  validatePlanInvalidationCause cause generation
  prepareAlignmentPlanInvalidationAt cause (currentOrBirthCoordinate generation state) state

-- Supersession uses the same owner-retirement path, but only the checked atomic
-- reset transition may create that cause without an actual Store loss.
prepareAlignmentPlanInvalidationAt :: AlignmentPlanInvalidationCause -> AlignmentPlanCoordinate -> State -> Either AlignmentPlanInvalidationProblem PreparedAlignmentPlanInvalidation
prepareAlignmentPlanInvalidationAt cause coordinate state = do
  let incumbent = Map.lookup coordinate state.planInvalidations
      incumbentCauses = maybe Set.empty alignmentPlanInvalidationReceiptCauses incumbent
  if Set.member cause incumbentCauses
    then do
      retained <-
        maybe
          (Left (AlignmentPlanInvalidationCauseMismatch cause))
          Right
          incumbent
      prepared <-
        prepareTransition
          (\predecessor -> Right (predecessor, AlignmentPlanInvalidationUnchanged))
          state
      Right (PreparedAlignmentPlanInvalidation prepared retained [])
    else retainPlanInvalidation incumbentCauses incumbent
  where
    retainPlanInvalidation incumbentCauses incumbent = do
      let generationIds =
            maybe Set.empty alignmentPlanInvalidationReceiptGenerations incumbent
              <> Set.fromList
                [ identifier
                | (identifier, generation) <- Map.toAscList state.generations,
                  generationBelongsToCoordinate coordinate generation state,
                  let current = currentOrBirthCoordinate generation state,
                  current == coordinate || Map.member current state.planInvalidations
                ]
          activeObligations =
            Map.filter
              ( (`Set.member` generationIds)
                  . alignmentObligationDestinationGeneration
              )
              state.obligations
          activeBootstrapImports =
            Map.filterWithKey
              (\key _ -> Set.member key.generation generationIds)
              state.bootstrapImports
          retainedObligations =
            maybe
              activeObligations
              ( Map.union activeObligations
                  . Map.fromList
                  . alignmentPlanInvalidationReceiptObligations
              )
              incumbent
          retainedBootstrapImports =
            maybe
              activeBootstrapImports
              ( Map.union activeBootstrapImports
                  . Map.fromList
                  . alignmentPlanInvalidationReceiptBootstrapImports
              )
              incumbent
          causes = Set.insert cause incumbentCauses
          cancellationReasons = planInvalidationCancellationReasons causes
          cancellationReason = dominantAlignmentCancelReason cancellationReasons
          ordinaryReceipts =
            Map.mapWithKey
              (retainOrdinaryPlanReasons generationIds cancellationReasons)
              state.attemptInvalidations
          bootstrapReceipts =
            Map.mapWithKey
              (retainBootstrapPlanReasons generationIds cancellationReasons)
              state.bootstrapAttemptInvalidations
          activeOrdinaryAttempts =
            Map.filterWithKey
              (\identifier _ -> Map.member identifier activeObligations)
              state.attempts
          activeBootstrapAttempts =
            Map.filterWithKey
              (\key _ -> Map.member key activeBootstrapImports)
              state.bootstrapAttempts
          successorOrdinaryReceipts =
            foldl
              ( \retained attempt ->
                  Map.insert
                    (alignmentAttemptSubscriptionId attempt)
                    ( AlignmentAttemptInvalidationReceipt
                        attempt
                        cancellationReasons
                    )
                    retained
              )
              ordinaryReceipts
              (Map.elems activeOrdinaryAttempts)
          successorBootstrapReceipts =
            foldl
              ( \retained attempt ->
                  Map.insert
                    (alignmentSubscribeSubscriptionId attempt.subscribe)
                    ( BootstrapImportAttemptInvalidationReceipt
                        attempt
                        cancellationReasons
                    )
                    retained
              )
              bootstrapReceipts
              (Map.elems activeBootstrapAttempts)
          affectedSubscriptions =
            Set.toAscList
              ( Set.fromList
                  ( fmap
                      alignmentAttemptSubscriptionId
                      (Map.elems activeOrdinaryAttempts)
                      <> fmap
                        (alignmentSubscribeSubscriptionId . (.subscribe))
                        (Map.elems activeBootstrapAttempts)
                      <> [ subscription
                         | (subscription, receipt) <- Map.toAscList ordinaryReceipts,
                           ordinaryReceiptTargetsGenerations generationIds receipt
                         ]
                      <> [ subscription
                         | (subscription, receipt) <- Map.toAscList bootstrapReceipts,
                           bootstrapReceiptTargetsGenerations generationIds receipt
                         ]
                  )
              )
      (successorTransfer, cancellations) <-
        foldM
          (retainPlanCancellation cancellationReason)
          (state.transfer, [])
          affectedSubscriptions
      let receipt =
            AlignmentPlanInvalidationReceipt
              coordinate
              causes
              generationIds
              retainedObligations
              retainedBootstrapImports
      prepared <-
        prepareTransition
          ( \predecessor ->
              Right
                ( finishPlanTransferInvalidation causes (Map.elems activeObligations) (Map.keys activeBootstrapImports) predecessor
                    $ ( case incumbent of
                          Nothing -> refreshLiveDebtScheduling (Set.fromList (map structuralConsequenceDebtKey (structuralDebtSetEntries predecessor.structuralDebts)))
                          Just _ -> id
                      )
                      . wakeLossCheck
                      . wakeGenerationPlanSorts (Set.singleton (alignmentPlanCoordinateSort coordinate))
                    $ (removeInputClosureOwners (Map.elems activeObligations) (withoutPendingAlignmentGenerations generationIds predecessor))
                      { obligations =
                          Map.withoutKeys predecessor.obligations (Map.keysSet activeObligations),
                        closingObligations =
                          Set.difference
                            predecessor.closingObligations
                            (Map.keysSet activeObligations),
                        attempts =
                          Map.withoutKeys predecessor.attempts (Map.keysSet activeOrdinaryAttempts),
                        unavailableSources =
                          retainPlanUnavailableSources
                            causes
                            predecessor.unavailableSources,
                        attemptInvalidations = successorOrdinaryReceipts,
                        planInvalidations =
                          Map.insert coordinate receipt predecessor.planInvalidations,
                        invalidatedGenerations = Set.union generationIds predecessor.invalidatedGenerations,
                        inputClosureInvalidatedGenerations =
                          case incumbent of
                            Nothing -> Set.union generationIds predecessor.inputClosureInvalidatedGenerations
                            Just _ -> predecessor.inputClosureInvalidatedGenerations,
                        preparationReadinessChanges = case incumbent of
                          Nothing -> Set.insert (alignmentPlanCoordinateSort coordinate) predecessor.preparationReadinessChanges
                          Just _ -> predecessor.preparationReadinessChanges,
                        liveStructuralDebts = case incumbent of
                          Just _ -> predecessor.liveStructuralDebts
                          Nothing -> reconstructLiveStructuralDebts predecessor.structuralDebts predecessor.promotions predecessor.historicalCoverage (Map.insert coordinate receipt predecessor.planInvalidations),
                        bootstrapImports =
                          Map.withoutKeys
                            predecessor.bootstrapImports
                            (Map.keysSet activeBootstrapImports),
                        bootstrapAttempts =
                          Map.withoutKeys
                            predecessor.bootstrapAttempts
                            (Map.keysSet activeBootstrapAttempts),
                        bootstrapAttemptInvalidations = successorBootstrapReceipts,
                        transfer = successorTransfer
                      },
                  AlignmentPlanInvalidated
                )
          )
          state
      Right
        ( PreparedAlignmentPlanInvalidation
            prepared
            receipt
            cancellations
        )

    retainPlanCancellation reason (transfer, cancellations) subscription = do
      let cancellation = alignmentCancel subscription reason
      prepared <-
        mapLeft
          AlignmentPlanInvalidationTransferProblem
          (Transfer.prepareAlignmentCancellation cancellation transfer)
      let (successor, _) = Transfer.commitAlignmentCancellation prepared
      Right (successor, cancellations <> [cancellation])

preparedAlignmentPlanInvalidationReceipt ::
  PreparedAlignmentPlanInvalidation -> AlignmentPlanInvalidationReceipt
preparedAlignmentPlanInvalidationReceipt
  (PreparedAlignmentPlanInvalidation _ receipt _) = receipt

preparedAlignmentPlanInvalidationCancellations ::
  PreparedAlignmentPlanInvalidation -> [AlignmentCancel]
preparedAlignmentPlanInvalidationCancellations
  (PreparedAlignmentPlanInvalidation _ _ cancellations) = cancellations

preparedAlignmentPlanInvalidationDisposition ::
  PreparedAlignmentPlanInvalidation -> AlignmentPlanInvalidationDisposition
preparedAlignmentPlanInvalidationDisposition
  (PreparedAlignmentPlanInvalidation prepared _ _) = preparedOutput prepared

commitAlignmentPlanInvalidation ::
  PreparedAlignmentPlanInvalidation ->
  (State, AlignmentPlanInvalidationDisposition)
commitAlignmentPlanInvalidation
  (PreparedAlignmentPlanInvalidation prepared _ _) = commitPrepared prepared

planInvalidationCauseGeneration ::
  AlignmentPlanInvalidationCause -> Maybe ContextClassGenerationId
planInvalidationCauseGeneration = \case
  AlignmentPlanDestinationStoreLost generation _ -> Just generation
  AlignmentPlanFreshBaseStoreLost generation _ _ -> Just generation
  AlignmentPlanSuperseded _ _ -> Nothing

validatePlanInvalidationCause ::
  AlignmentPlanInvalidationCause ->
  AlignmentGeneration ->
  Either AlignmentPlanInvalidationProblem ()
validatePlanInvalidationCause cause generation =
  if valid then Right () else Left (AlignmentPlanInvalidationCauseMismatch cause)
  where
    cut = alignmentGenerationCut generation
    members = NonEmpty.toList (alignmentCutExactMembers cut)
    valid = case cause of
      AlignmentPlanSuperseded _ _ -> False
      AlignmentPlanDestinationStoreLost generationId destination ->
        generationId == alignmentGenerationId generation
          && any
            ( \member ->
                alignmentMemberDelta member == destinationStoreDelta destination
                  && alignmentMemberStoreIncarnation member
                    == destinationStoreIncarnation destination
            )
            members
      AlignmentPlanFreshBaseStoreLost generationId fresh source ->
        generationId == alignmentGenerationId generation
          && fresh `elem` alignmentCutFreshMemberBaseEvidence cut
          && any
            ( \member ->
                alignmentMemberDelta member == freshMemberBaseDelta fresh
                  && alignmentMemberStoreIncarnation member
                    == freshMemberBaseStoreIncarnation fresh
                  && alignmentMemberHerald member
                    == alignmentSourceCoordinateHerald source
                  && alignmentMemberStoreIncarnation member
                    == alignmentSourceCoordinateStoreIncarnation source
            )
            members

generationPlanCoordinate :: AlignmentGeneration -> AlignmentPlanCoordinate
generationPlanCoordinate = planCoordinate . Plan.alignmentGenerationBirthPlanId

planInvalidationCancellationReasons ::
  Set AlignmentPlanInvalidationCause -> Set AlignmentCancelReason
planInvalidationCancellationReasons causes =
  Set.fromList
    ( [ AlignmentDestinationIncarnationLost
      | any isDestinationLoss (Set.toAscList causes)
      ]
        <> [ AlignmentRelationRemoved
           | any isFreshBaseLoss (Set.toAscList causes)
           ]
    )
  where
    isDestinationLoss = \case
      AlignmentPlanDestinationStoreLost _ _ -> True
      AlignmentPlanFreshBaseStoreLost _ _ _ -> False
      AlignmentPlanSuperseded _ _ -> False
    isFreshBaseLoss = \case
      AlignmentPlanDestinationStoreLost _ _ -> False
      AlignmentPlanFreshBaseStoreLost _ _ _ -> True
      AlignmentPlanSuperseded _ _ -> True

retainPlanUnavailableSources ::
  Set AlignmentPlanInvalidationCause ->
  Set UnavailableAlignmentSource ->
  Set UnavailableAlignmentSource
retainPlanUnavailableSources causes incumbent =
  foldr retain incumbent (Set.toAscList causes)
  where
    retain cause retained = case cause of
      AlignmentPlanSuperseded _ _ -> retained
      AlignmentPlanDestinationStoreLost _ _ -> retained
      AlignmentPlanFreshBaseStoreLost _ fresh source ->
        Set.insert
          ( UnavailableAlignmentSource
              (alignmentSourceCoordinateHerald source)
              (freshMemberBaseDelta fresh)
              (alignmentSourceCoordinateStoreIncarnation source)
          )
          retained

retainOrdinaryPlanReasons ::
  Set ContextClassGenerationId ->
  Set AlignmentCancelReason ->
  AlignmentSubscriptionId ->
  AlignmentAttemptInvalidationReceipt ->
  AlignmentAttemptInvalidationReceipt
retainOrdinaryPlanReasons generations reasons _ receipt
  | ordinaryReceiptTargetsGenerations generations receipt =
      AlignmentAttemptInvalidationReceipt
        (alignmentAttemptInvalidationAttempt receipt)
        (Set.union reasons (alignmentAttemptInvalidationReasons receipt))
  | otherwise = receipt

ordinaryReceiptTargetsGenerations ::
  Set ContextClassGenerationId -> AlignmentAttemptInvalidationReceipt -> Bool
ordinaryReceiptTargetsGenerations generations receipt =
  Set.member
    ( alignmentObligationDestinationGeneration
        ( alignmentAttemptObligation
            (alignmentAttemptInvalidationAttempt receipt)
        )
    )
    generations

retainBootstrapPlanReasons ::
  Set ContextClassGenerationId ->
  Set AlignmentCancelReason ->
  AlignmentSubscriptionId ->
  BootstrapImportAttemptInvalidationReceipt ->
  BootstrapImportAttemptInvalidationReceipt
retainBootstrapPlanReasons generations reasons _ receipt
  | bootstrapReceiptTargetsGenerations generations receipt =
      BootstrapImportAttemptInvalidationReceipt
        (bootstrapImportAttemptInvalidationAttempt receipt)
        (Set.union reasons (bootstrapImportAttemptInvalidationReasons receipt))
  | otherwise = receipt

bootstrapReceiptTargetsGenerations ::
  Set ContextClassGenerationId ->
  BootstrapImportAttemptInvalidationReceipt ->
  Bool
bootstrapReceiptTargetsGenerations generations receipt =
  Set.member
    ( bootstrapImportKeyGeneration
        ( bootstrapImportKeyValue
            ( bootstrapImportAttemptImport
                (bootstrapImportAttemptInvalidationAttempt receipt)
            )
        )
    )
    generations

attemptSourceCoordinate :: AlignmentAttempt -> AlignmentSourceCoordinate
attemptSourceCoordinate attempt =
  AlignmentSourceCoordinate
    (alignmentAttemptSourceHerald attempt)
    (alignmentAttemptSourceStoreIncarnation attempt)

alignmentAttemptUnavailableSourceCoordinate ::
  AlignmentAttempt -> State -> Maybe UnavailableAlignmentSource
alignmentAttemptUnavailableSourceCoordinate attempt =
  resolveGenerationSourceCoordinate
    ( alignmentObligationSourceGeneration
        (alignmentAttemptObligation attempt)
    )
    (alignmentAttemptSourceHerald attempt)
    (alignmentAttemptSourceStoreIncarnation attempt)

alignmentAttemptUsesUnavailableSource ::
  UnavailableAlignmentSource -> AlignmentAttempt -> State -> Bool
alignmentAttemptUsesUnavailableSource source attempt state =
  alignmentAttemptUnavailableSourceCoordinate attempt state == Just source

bootstrapImportAttemptUnavailableSourceCoordinate ::
  BootstrapImportAttempt -> State -> Maybe UnavailableAlignmentSource
bootstrapImportAttemptUnavailableSourceCoordinate attempt =
  resolveGenerationSourceCoordinate
    sourceGeneration
    attempt.sourceHerald
    (alignmentSubscribeSourceStoreIncarnation attempt.subscribe)
  where
    key = bootstrapImportKeyValue attempt.bootstrapImport
    sourceGeneration = case key.source of
      BootstrapFromPredecessor predecessor -> predecessor
      BootstrapFromFreshBase _ -> key.generation

bootstrapImportAttemptUsesUnavailableSource ::
  UnavailableAlignmentSource -> BootstrapImportAttempt -> State -> Bool
bootstrapImportAttemptUsesUnavailableSource source attempt state =
  bootstrapImportAttemptUnavailableSourceCoordinate attempt state == Just source

resolveGenerationSourceCoordinate ::
  ContextClassGenerationId ->
  HeraldEpoch ->
  StoreIncarnationId ->
  State ->
  Maybe UnavailableAlignmentSource
resolveGenerationSourceCoordinate generationId herald store state = do
  generation <- Map.lookup generationId state.generations
  case [ UnavailableAlignmentSource
           (alignmentMemberHerald member)
           (alignmentMemberDelta member)
           (alignmentMemberStoreIncarnation member)
       | member <-
           NonEmpty.toList
             (alignmentCutExactMembers (alignmentGenerationCut generation)),
         alignmentMemberHerald member == herald,
         alignmentMemberStoreIncarnation member == store
       ] of
    [source] -> Just source
    _ -> Nothing

alignmentFailedSourceCoordinates :: State -> Set AlignmentSourceCoordinate
alignmentFailedSourceCoordinates state =
  Set.fromList
    ( [ AlignmentSourceCoordinate
          (unavailableAlignmentSourceHerald source)
          (unavailableAlignmentSourceStoreIncarnation source)
      | source <- Set.toAscList state.unavailableSources
      ]
        <> [ attemptSourceCoordinate
               (alignmentAttemptInvalidationAttempt receipt)
           | receipt <- Map.elems state.attemptInvalidations,
             Set.member
               AlignmentSourceIncarnationLost
               (alignmentAttemptInvalidationReasons receipt)
           ]
        <> [ bootstrapAttemptSourceCoordinate
               (bootstrapImportAttemptInvalidationAttempt receipt)
           | receipt <- Map.elems state.bootstrapAttemptInvalidations,
             Set.member
               AlignmentSourceIncarnationLost
               (bootstrapImportAttemptInvalidationReasons receipt)
           ]
        <> [ source
           | receipt <- Map.elems state.planInvalidations,
             AlignmentPlanFreshBaseStoreLost _ _ source <-
               Set.toAscList (alignmentPlanInvalidationReceiptCauses receipt)
           ]
    )

selectAlignmentSource ::
  Set AlignmentSourceCoordinate ->
  AlignmentObligation ->
  State ->
  Maybe (HeraldEpoch, HistoricalCertificate)
selectAlignmentSource additionalFailedSources obligation state =
  case sortOn sourceKey candidates of
    [] -> Nothing
    (sourceHerald, _, _, certificate) : _ -> Just (sourceHerald, certificate)
  where
    generationId = alignmentObligationSourceGeneration obligation
    members = case Map.lookup generationId state.generations of
      Nothing -> []
      Just retained ->
        NonEmpty.toList
          (alignmentCutExactMembers (alignmentGenerationCut retained))
    candidates =
      [ (sourceHerald, sourceDelta, sourceStore, certificate)
      | ((retainedGeneration, sourceStore), certificate) <-
          Map.toAscList state.certificates,
        retainedGeneration == generationId,
        member <- members,
        alignmentMemberStoreIncarnation member == sourceStore,
        let sourceHerald = alignmentMemberHerald member,
        let sourceDelta = alignmentMemberDelta member,
        Map.member (generationId, sourceHerald) state.cutAcceptances,
        Map.member (alignmentObligationDestinationGeneration obligation, sourceHerald) state.cutAcceptances,
        sourceIsAvailable
          additionalFailedSources
          (UnavailableAlignmentSource sourceHerald sourceDelta sourceStore)
          state
      ]
    sourceKey (sourceHerald, _, sourceStore, _) =
      (sourceHerald, sourceStore)

sourceIsAvailable ::
  Set AlignmentSourceCoordinate ->
  UnavailableAlignmentSource ->
  State ->
  Bool
sourceIsAvailable additionalFailedSources source state =
  Set.notMember source state.unavailableSources
    && Set.notMember
      ( AlignmentSourceCoordinate
          (unavailableAlignmentSourceHerald source)
          (unavailableAlignmentSourceStoreIncarnation source)
      )
      additionalFailedSources

data BootstrapImportDisposition
  = BootstrapImportAttemptCreated
  | BootstrapImportAttemptUnchanged
  deriving stock (Eq, Ord, Show)

data BootstrapImportProblem
  = BootstrapImportMissing BootstrapImportKey
  | BootstrapImportDestinationAcceptanceUnavailable BootstrapImportKey
  | BootstrapImportSourceEvidenceUnavailable BootstrapImportKey
  | BootstrapImportFreshBaseSourceFailed
      BootstrapImportKey
      AlignmentSourceCoordinate
  | BootstrapImportIncumbentSourceExcluded
      BootstrapImportKey
      AlignmentSourceCoordinate
  | BootstrapImportProtocolProblem AlignmentProtocolProblem
  | BootstrapImportTransferProblem Transfer.AlignmentTransferProblem
  deriving stock (Eq, Show)

data PreparedBootstrapImportAttempt = PreparedBootstrapImportAttempt
  { preparedState :: Prepared State BootstrapImportDisposition,
    attempt :: BootstrapImportAttempt
  }

prepareBootstrapImportAttempt ::
  BootstrapImportKey ->
  State ->
  Either BootstrapImportProblem PreparedBootstrapImportAttempt
prepareBootstrapImportAttempt =
  prepareBootstrapImportAttemptExcluding Set.empty

-- | Bootstrap counterpart of 'prepareAlignmentAttemptExcluding'.
prepareBootstrapImportAttemptExcluding ::
  Set AlignmentSourceCoordinate ->
  BootstrapImportKey ->
  State ->
  Either BootstrapImportProblem PreparedBootstrapImportAttempt
prepareBootstrapImportAttemptExcluding additionalFailedSources key state = do
  bootstrapImport <-
    maybe
      (Left (BootstrapImportMissing key))
      Right
      (Map.lookup key state.bootstrapImports)
  let destinationHerald =
        alignmentObligationIdDestinationHerald
          ( alignmentObligationIdValue
              bootstrapImport.subscribeEnvelope
          )
  if Map.member (key.generation, destinationHerald) state.cutAcceptances
    then Right ()
    else Left (BootstrapImportDestinationAcceptanceUnavailable key)
  case freshBaseImportUnavailableSourceCoordinate key state of
    Just source
      | not (sourceIsAvailable additionalFailedSources source state) ->
          Left
            ( BootstrapImportFreshBaseSourceFailed
                key
                (legacySourceCoordinate source)
            )
    _ -> Right ()
  case Map.lookup key state.bootstrapAttempts of
    Just incumbent
      | bootstrapImportAttemptSourceExcluded
          additionalFailedSources
          incumbent
          state ->
          Left
            ( BootstrapImportIncumbentSourceExcluded
                key
                (bootstrapAttemptSourceCoordinate incumbent)
            )
    Just incumbent ->
      PreparedBootstrapImportAttempt
        <$> prepareTransition
          (\predecessor -> Right (predecessor, BootstrapImportAttemptUnchanged))
          state
        <*> pure incumbent
    Nothing -> do
      (sourceHerald, sourceStore, evidenceDigest) <-
        maybe
          (Left (BootstrapImportSourceEvidenceUnavailable key))
          Right
          (selectBootstrapImportSource additionalFailedSources bootstrapImport state)
      let subscription =
            alignmentSubscriptionId
              ( alignmentObligationIdDestinationHerald
                  (alignmentObligationIdValue bootstrapImport.subscribeEnvelope)
              )
              state.nextSubscriptionSequence
          generation = key.generation
      subscribe <-
        mapLeft
          BootstrapImportProtocolProblem
          ( alignmentSubscribe
              bootstrapImport.subscribeEnvelope
              subscription
              sourceStore
              (BootstrapProof generation evidenceDigest)
          )
      preparedTransfer <-
        mapLeft
          BootstrapImportTransferProblem
          ( Transfer.prepareDestinationBootstrap
              sourceHerald
              subscribe
              state.transfer
          )
      let (successorTransfer, _) =
            Transfer.commitDestinationBootstrap preparedTransfer
          created =
            BootstrapImportAttempt
              { bootstrapImport,
                sourceHerald,
                subscribe
              }
      preparedState <-
        prepareTransition
          ( \predecessor ->
              Right
                ( registerTransferBootstrapAttempt key created
                    $ predecessor
                      { bootstrapAttempts =
                          Map.insert key created predecessor.bootstrapAttempts,
                        nextSubscriptionSequence =
                          nextAlignmentSubscriptionSequence
                            predecessor.nextSubscriptionSequence,
                        transfer = successorTransfer
                      },
                  BootstrapImportAttemptCreated
                )
          )
          state
      Right PreparedBootstrapImportAttempt {preparedState, attempt = created}

preparedBootstrapImportAttempt ::
  PreparedBootstrapImportAttempt -> BootstrapImportAttempt
preparedBootstrapImportAttempt prepared = prepared.attempt

preparedBootstrapImportSubscribe ::
  PreparedBootstrapImportAttempt -> AlignmentSubscribe
preparedBootstrapImportSubscribe =
  bootstrapImportAttemptSubscribe . preparedBootstrapImportAttempt

preparedBootstrapImportDisposition ::
  PreparedBootstrapImportAttempt -> BootstrapImportDisposition
preparedBootstrapImportDisposition prepared =
  preparedOutput prepared.preparedState

commitBootstrapImportAttempt ::
  PreparedBootstrapImportAttempt -> (State, BootstrapImportDisposition)
commitBootstrapImportAttempt prepared = commitPrepared prepared.preparedState

data BootstrapImportAttemptInvalidationDisposition
  = BootstrapImportAttemptInvalidated
  | BootstrapImportAttemptInvalidationUnchanged
  deriving stock (Eq, Ord, Show)

data BootstrapImportAttemptInvalidationProblem
  = BootstrapImportAttemptInvalidationMissing AlignmentObligationId
  | BootstrapImportAttemptInvalidationConflict AlignmentObligationId
  | BootstrapImportAttemptInvalidationRequiresPlanInvalidation
      ContextClassGenerationId
  | BootstrapImportAttemptInvalidationTransferProblem
      Transfer.AlignmentTransferProblem
  deriving stock (Eq, Show)

data PreparedBootstrapImportAttemptInvalidation
  = PreparedBootstrapImportAttemptInvalidation
      (Prepared State BootstrapImportAttemptInvalidationDisposition)
      BootstrapImportAttemptInvalidationReceipt
      AlignmentCancel

prepareBootstrapImportAttemptInvalidation ::
  BootstrapImportAttempt ->
  AlignmentCancelReason ->
  State ->
  Either
    BootstrapImportAttemptInvalidationProblem
    PreparedBootstrapImportAttemptInvalidation
prepareBootstrapImportAttemptInvalidation suppliedAttempt reason state =
  case Map.lookup subscription state.bootstrapAttemptInvalidations of
    Just incumbent
      | bootstrapImportAttemptInvalidationAttempt incumbent /= suppliedAttempt ->
          Left (BootstrapImportAttemptInvalidationConflict identifier)
      | Set.member reason (bootstrapImportAttemptInvalidationReasons incumbent) ->
          unchanged incumbent
      | otherwise -> retain (bootstrapImportAttemptInvalidationReasons incumbent)
    Nothing
      | bootstrapInvalidationRequiresPlan reason key.source ->
          Left
            ( BootstrapImportAttemptInvalidationRequiresPlanInvalidation
                key.generation
            )
      | otherwise -> do
          incumbent <-
            maybe
              (Left (BootstrapImportAttemptInvalidationMissing identifier))
              Right
              (Map.lookup key state.bootstrapAttempts)
          if incumbent == suppliedAttempt
            then Right ()
            else Left (BootstrapImportAttemptInvalidationConflict identifier)
          currentImport <-
            maybe
              (Left (BootstrapImportAttemptInvalidationMissing identifier))
              Right
              (Map.lookup key state.bootstrapImports)
          if currentImport == suppliedAttempt.bootstrapImport
            then retain Set.empty
            else Left (BootstrapImportAttemptInvalidationConflict identifier)
  where
    bootstrapImport = suppliedAttempt.bootstrapImport
    key = bootstrapImport.key
    identifier =
      alignmentObligationIdValue bootstrapImport.subscribeEnvelope
    subscription = alignmentSubscribeSubscriptionId suppliedAttempt.subscribe
    unchanged receipt = do
      prepared <-
        prepareTransition
          ( \predecessor ->
              Right
                ( predecessor,
                  BootstrapImportAttemptInvalidationUnchanged
                )
          )
          state
      Right
        ( PreparedBootstrapImportAttemptInvalidation
            prepared
            receipt
            ( alignmentCancel
                subscription
                (bootstrapImportAttemptInvalidationReason receipt)
            )
        )
    retain retainedReasons = do
      let reasons = Set.insert reason retainedReasons
          receipt =
            BootstrapImportAttemptInvalidationReceipt suppliedAttempt reasons
          cancellation =
            alignmentCancel subscription (dominantAlignmentCancelReason reasons)
      preparedCancellation <-
        mapLeft
          BootstrapImportAttemptInvalidationTransferProblem
          (Transfer.prepareAlignmentCancellation cancellation state.transfer)
      let (successorTransfer, _) =
            Transfer.commitAlignmentCancellation preparedCancellation
      prepared <-
        prepareTransition
          ( \predecessor ->
              Right
                ( finishBootstrapAttemptInvalidation suppliedAttempt predecessor
                    $ predecessor
                      { bootstrapImports =
                          if Set.member AlignmentRelationRemoved reasons
                            then Map.delete key predecessor.bootstrapImports
                            else predecessor.bootstrapImports,
                        bootstrapAttempts =
                          Map.delete key predecessor.bootstrapAttempts,
                        unavailableSources =
                          retainBootstrapAttemptUnavailableSource
                            reasons
                            suppliedAttempt
                            predecessor,
                        bootstrapAttemptInvalidations =
                          Map.insert
                            subscription
                            receipt
                            predecessor.bootstrapAttemptInvalidations,
                        transfer = successorTransfer
                      },
                  BootstrapImportAttemptInvalidated
                )
          )
          state
      Right
        ( PreparedBootstrapImportAttemptInvalidation
            prepared
            receipt
            cancellation
        )

retainBootstrapAttemptUnavailableSource ::
  Set AlignmentCancelReason ->
  BootstrapImportAttempt ->
  State ->
  Set UnavailableAlignmentSource
retainBootstrapAttemptUnavailableSource reasons attempt state
  | Set.member AlignmentSourceIncarnationLost reasons =
      maybe
        state.unavailableSources
        (`Set.insert` state.unavailableSources)
        (bootstrapImportAttemptUnavailableSourceCoordinate attempt state)
  | otherwise = state.unavailableSources

bootstrapInvalidationRequiresPlan ::
  AlignmentCancelReason -> BootstrapImportSource -> Bool
bootstrapInvalidationRequiresPlan reason source =
  case (reason, source) of
    (AlignmentDestinationIncarnationLost, _) -> True
    (AlignmentSourceIncarnationLost, BootstrapFromFreshBase _) -> True
    _ -> False

bootstrapAttemptSourceCoordinate ::
  BootstrapImportAttempt -> AlignmentSourceCoordinate
bootstrapAttemptSourceCoordinate attempt =
  AlignmentSourceCoordinate
    attempt.sourceHerald
    (alignmentSubscribeSourceStoreIncarnation attempt.subscribe)

bootstrapImportAttemptSourceExcluded ::
  Set AlignmentSourceCoordinate -> BootstrapImportAttempt -> State -> Bool
bootstrapImportAttemptSourceExcluded additionalFailedSources attempt state =
  Set.member
    (bootstrapAttemptSourceCoordinate attempt)
    additionalFailedSources
    || maybe
      False
      (`Set.member` state.unavailableSources)
      (bootstrapImportAttemptUnavailableSourceCoordinate attempt state)

preparedBootstrapImportAttemptInvalidationReceipt ::
  PreparedBootstrapImportAttemptInvalidation ->
  BootstrapImportAttemptInvalidationReceipt
preparedBootstrapImportAttemptInvalidationReceipt
  (PreparedBootstrapImportAttemptInvalidation _ receipt _) = receipt

preparedBootstrapImportAttemptInvalidationCancellation ::
  PreparedBootstrapImportAttemptInvalidation -> AlignmentCancel
preparedBootstrapImportAttemptInvalidationCancellation
  (PreparedBootstrapImportAttemptInvalidation _ _ cancellation) = cancellation

preparedBootstrapImportAttemptInvalidationDisposition ::
  PreparedBootstrapImportAttemptInvalidation ->
  BootstrapImportAttemptInvalidationDisposition
preparedBootstrapImportAttemptInvalidationDisposition
  (PreparedBootstrapImportAttemptInvalidation prepared _ _) = preparedOutput prepared

commitBootstrapImportAttemptInvalidation ::
  PreparedBootstrapImportAttemptInvalidation ->
  (State, BootstrapImportAttemptInvalidationDisposition)
commitBootstrapImportAttemptInvalidation
  (PreparedBootstrapImportAttemptInvalidation prepared _ _) =
    commitPrepared prepared

data BootstrapImportFulfillmentDisposition
  = BootstrapImportFulfilled
  | BootstrapImportFulfillmentUnchanged
  deriving stock (Eq, Ord, Show)

data BootstrapImportFulfillmentProblem
  = BootstrapImportFulfillmentMissing BootstrapImportKey
  | BootstrapImportFulfillmentConflict BootstrapImportKey
  | BootstrapImportFulfillmentTranscriptIncomplete BootstrapImportKey
  | BootstrapImportFulfillmentTransferProblem Transfer.AlignmentTransferProblem
  deriving stock (Eq, Show)

data PreparedBootstrapImportFulfillment
  = PreparedBootstrapImportFulfillment
      (Prepared State BootstrapImportFulfillmentDisposition)
      BootstrapImportFulfillmentReceipt
      [AlignmentCancel]

prepareBootstrapImportFulfillment ::
  BootstrapImportAttempt ->
  StoreRevision ->
  State ->
  Either BootstrapImportFulfillmentProblem PreparedBootstrapImportFulfillment
prepareBootstrapImportFulfillment suppliedAttempt through state =
  case Map.lookup key state.bootstrapFulfillments of
    Just receipt
      | receipt == candidate -> do
          prepared <-
            prepareTransition
              (\predecessor -> Right (predecessor, BootstrapImportFulfillmentUnchanged))
              state
          Right (PreparedBootstrapImportFulfillment prepared receipt [])
      | otherwise -> Left (BootstrapImportFulfillmentConflict key)
    Nothing -> do
      bootstrapImport <-
        maybe
          (Left (BootstrapImportFulfillmentMissing key))
          Right
          (Map.lookup key state.bootstrapImports)
      attempt <-
        maybe
          (Left (BootstrapImportFulfillmentMissing key))
          Right
          (Map.lookup key state.bootstrapAttempts)
      if bootstrapImport == suppliedAttempt.bootstrapImport
        && attempt == suppliedAttempt
        then Right ()
        else Left (BootstrapImportFulfillmentConflict key)
      destination <-
        case [ retained
             | (identifier, retained) <- Transfer.destinationSubscriptionEntries state.transfer,
               identifier == subscription
             ] of
          [retained] -> Right retained
          _ -> Left (BootstrapImportFulfillmentTranscriptIncomplete key)
      if Transfer.destinationSubscriptionSubscribe destination == suppliedAttempt.subscribe
        && Transfer.destinationSubscriptionBootstrapSourceHerald destination
          == Just suppliedAttempt.sourceHerald
        && Transfer.destinationSubscriptionAppliedThrough destination == Just through
        && Transfer.destinationSubscriptionLiveThrough destination == Just through
        && Transfer.destinationSubscriptionAcknowledgedThrough destination == Just through
        then Right ()
        else Left (BootstrapImportFulfillmentTranscriptIncomplete key)
      let priorReceipts =
            [ (retainedSubscription, receipt)
            | (retainedSubscription, receipt) <-
                Map.toAscList state.bootstrapAttemptInvalidations,
              bootstrapImportKeyValue
                ( bootstrapImportAttemptImport
                    (bootstrapImportAttemptInvalidationAttempt receipt)
                )
                == key
            ]
          successorReceipts =
            foldl
              ( \retained (retainedSubscription, receipt) ->
                  Map.insert
                    retainedSubscription
                    ( BootstrapImportAttemptInvalidationReceipt
                        (bootstrapImportAttemptInvalidationAttempt receipt)
                        ( Set.insert
                            AlignmentRelationRemoved
                            (bootstrapImportAttemptInvalidationReasons receipt)
                        )
                    )
                    retained
              )
              state.bootstrapAttemptInvalidations
              priorReceipts
          finalReceipts =
            Map.insert
              subscription
              ( BootstrapImportAttemptInvalidationReceipt
                  suppliedAttempt
                  (Set.singleton AlignmentRelationRemoved)
              )
              successorReceipts
          subscriptions =
            Set.toAscList
              (Set.insert subscription (Set.fromList (fmap fst priorReceipts)))
      (successorTransfer, cancellations) <-
        foldM retainFulfillmentCancellation (state.transfer, []) subscriptions
      prepared <-
        prepareTransition
          ( \predecessor ->
              Right
                ( wakeLossCheck . retainTransferBootstrapFulfillment key . removeTransferBootstrapAttempt key . removeTransferBootstrapImport key
                    $ predecessor
                      { bootstrapImports = Map.delete key predecessor.bootstrapImports,
                        bootstrapAttempts = Map.delete key predecessor.bootstrapAttempts,
                        bootstrapAttemptInvalidations = finalReceipts,
                        bootstrapFulfillments =
                          Map.insert key candidate predecessor.bootstrapFulfillments,
                        transfer = successorTransfer
                      },
                  BootstrapImportFulfilled
                )
          )
          state
      Right
        ( PreparedBootstrapImportFulfillment
            prepared
            candidate
            cancellations
        )
  where
    key = bootstrapImportKeyValue suppliedAttempt.bootstrapImport
    subscription = alignmentSubscribeSubscriptionId suppliedAttempt.subscribe
    candidate = BootstrapImportFulfillmentReceipt suppliedAttempt through
    retainFulfillmentCancellation (transfer, cancellations) identifier = do
      let cancellation = alignmentCancel identifier AlignmentRelationRemoved
      prepared <-
        mapLeft
          BootstrapImportFulfillmentTransferProblem
          (Transfer.prepareAlignmentCancellation cancellation transfer)
      let (successor, _) = Transfer.commitAlignmentCancellation prepared
      Right (successor, cancellations <> [cancellation])

preparedBootstrapImportFulfillmentReceipt ::
  PreparedBootstrapImportFulfillment -> BootstrapImportFulfillmentReceipt
preparedBootstrapImportFulfillmentReceipt
  (PreparedBootstrapImportFulfillment _ receipt _) = receipt

preparedBootstrapImportFulfillmentCancellations ::
  PreparedBootstrapImportFulfillment -> [AlignmentCancel]
preparedBootstrapImportFulfillmentCancellations
  (PreparedBootstrapImportFulfillment _ _ cancellations) = cancellations

preparedBootstrapImportFulfillmentDisposition ::
  PreparedBootstrapImportFulfillment -> BootstrapImportFulfillmentDisposition
preparedBootstrapImportFulfillmentDisposition
  (PreparedBootstrapImportFulfillment prepared _ _) = preparedOutput prepared

commitBootstrapImportFulfillment ::
  PreparedBootstrapImportFulfillment ->
  (State, BootstrapImportFulfillmentDisposition)
commitBootstrapImportFulfillment
  (PreparedBootstrapImportFulfillment prepared _ _) = commitPrepared prepared

selectBootstrapImportSource ::
  Set AlignmentSourceCoordinate ->
  BootstrapImport ->
  State ->
  Maybe (HeraldEpoch, StoreIncarnationId, BootstrapEvidenceDigest)
selectBootstrapImportSource additionalFailedSources bootstrapImport state =
  case bootstrapImport.key.source of
    BootstrapFromPredecessor predecessorGeneration ->
      selectPredecessor predecessorGeneration
    BootstrapFromFreshBase fresh -> selectFresh fresh
  where
    newGeneration = bootstrapImport.key.generation

    selectPredecessor predecessorGeneration = do
      predecessor <- Map.lookup predecessorGeneration state.generations
      case sortOn sourceKey (predecessorCandidates predecessorGeneration predecessor) of
        [] -> Nothing
        (sourceHerald, sourceStore, certificate) : _ ->
          Just
            ( sourceHerald,
              sourceStore,
              predecessorBootstrapEvidenceDigest newGeneration certificate
            )

    predecessorCandidates predecessorGeneration predecessor =
      [ (sourceHerald, sourceStore, certificate)
      | ((retainedGeneration, sourceStore), certificate) <-
          Map.toAscList state.certificates,
        retainedGeneration == predecessorGeneration,
        member <-
          NonEmpty.toList
            (alignmentCutExactMembers (alignmentGenerationCut predecessor)),
        alignmentMemberStoreIncarnation member == sourceStore,
        let sourceHerald = alignmentMemberHerald member,
        let sourceDelta = alignmentMemberDelta member,
        Map.member (predecessorGeneration, sourceHerald) state.cutAcceptances,
        Map.member (newGeneration, sourceHerald) state.cutAcceptances,
        sourceIsAvailable
          additionalFailedSources
          (UnavailableAlignmentSource sourceHerald sourceDelta sourceStore)
          state
      ]

    selectFresh fresh = do
      generation <- Map.lookup newGeneration state.generations
      if fresh `elem` alignmentCutFreshMemberBaseEvidence (alignmentGenerationCut generation)
        then Just ()
        else Nothing
      member <-
        findGenerationMember
          (freshMemberBaseDelta fresh)
          (freshMemberBaseStoreIncarnation fresh)
          generation
      let sourceHerald = alignmentMemberHerald member
          source =
            UnavailableAlignmentSource
              sourceHerald
              (freshMemberBaseDelta fresh)
              (freshMemberBaseStoreIncarnation fresh)
      if Map.member (newGeneration, sourceHerald) state.cutAcceptances
        && sourceIsAvailable additionalFailedSources source state
        then
          Just
            ( sourceHerald,
              freshMemberBaseStoreIncarnation fresh,
              freshBootstrapEvidenceDigest newGeneration fresh
            )
        else Nothing

    sourceKey (sourceHerald, sourceStore, _) = (sourceHerald, sourceStore)

freshBaseImportUnavailableSourceCoordinate ::
  BootstrapImportKey -> State -> Maybe UnavailableAlignmentSource
freshBaseImportUnavailableSourceCoordinate key state = case key.source of
  BootstrapFromPredecessor _ -> Nothing
  BootstrapFromFreshBase fresh -> do
    generation <- Map.lookup key.generation state.generations
    member <-
      findGenerationMember
        (freshMemberBaseDelta fresh)
        (freshMemberBaseStoreIncarnation fresh)
        generation
    Just
      ( UnavailableAlignmentSource
          (alignmentMemberHerald member)
          (freshMemberBaseDelta fresh)
          (freshMemberBaseStoreIncarnation fresh)
      )

legacySourceCoordinate ::
  UnavailableAlignmentSource -> AlignmentSourceCoordinate
legacySourceCoordinate source =
  AlignmentSourceCoordinate
    (unavailableAlignmentSourceHerald source)
    (unavailableAlignmentSourceStoreIncarnation source)

findGenerationMember ::
  DeltaId ->
  StoreIncarnationId ->
  AlignmentGeneration ->
  Maybe AlignmentMember
findGenerationMember delta store generation =
  case [ member
       | member <-
           NonEmpty.toList
             (alignmentCutExactMembers (alignmentGenerationCut generation)),
         alignmentMemberDelta member == delta,
         alignmentMemberStoreIncarnation member == store
       ] of
    [member] -> Just member
    _ -> Nothing

predecessorBootstrapEvidenceDigest ::
  ContextClassGenerationId ->
  HistoricalCertificate ->
  BootstrapEvidenceDigest
predecessorBootstrapEvidenceDigest newGeneration certificate =
  deriveBootstrapEvidenceDigest
    ( Serialize.runPut $ do
        Serialize.putWord8 0
        Serialize.putByteString (contextClassGenerationIdBytes newGeneration)
        putSizedBytes (historicalCertificateCanonicalBytes certificate)
    )

freshBootstrapEvidenceDigest ::
  ContextClassGenerationId ->
  FreshMemberBaseEvidence ->
  BootstrapEvidenceDigest
freshBootstrapEvidenceDigest newGeneration fresh =
  deriveBootstrapEvidenceDigest
    ( Serialize.runPut $ do
        Serialize.putWord8 1
        Serialize.putByteString (contextClassGenerationIdBytes newGeneration)
        Serialize.putByteString (deltaIdBytes (freshMemberBaseDelta fresh))
        Serialize.putByteString
          (storeIncarnationIdBytes (freshMemberBaseStoreIncarnation fresh))
        Serialize.putWord64be (storeRevisionWord64 (freshMemberBaseRevision fresh))
    )

putSizedBytes :: ByteString.ByteString -> Serialize.Put
putSizedBytes bytes = do
  Serialize.putWord64be (fromIntegral (ByteString.length bytes))
  Serialize.putByteString bytes

-- | Closed owner-local contradiction categories. Cross-owner topology,
-- placement, and Store-history relationships are checked by Startup.Invariant
-- after this projection has validated itself.
data AlignmentStateInvariantProblem
  = AlignmentStateSchedulingInvariant
  | AlignmentStatePlanInvariant
  | AlignmentStatePromotionInvariant
  | AlignmentStateLiveDebtInvariant
  | AlignmentStateGenerationInvariant
  | AlignmentStateRelationInvariant
  | AlignmentStateObligationInvariant
  | AlignmentStateBootstrapImportInvariant
  | AlignmentStateObligationSequenceInvariant
  | AlignmentStateCutAcceptanceInvariant
  | AlignmentStatePendingEvidenceInvariant
  | AlignmentStateReadinessInvariant
  | AlignmentStateEvidenceSequenceInvariant
  | AlignmentStateCertificateInvariant
  | AlignmentStateAttemptInvariant
  | AlignmentStateAttemptInvalidationInvariant
  | AlignmentStatePlanInvalidationInvariant
  | AlignmentStateBootstrapAttemptInvalidationInvariant
  | AlignmentStateBootstrapFulfillmentInvariant
  | AlignmentStateSubscriptionSequenceInvariant
  | AlignmentStateTransferInvariant
  deriving stock (Eq, Ord, Show, Enum, Bounded)

validateAlignmentState ::
  State -> Either AlignmentStateInvariantProblem ()
validateAlignmentState state = do
  requireInvariant (generationSchedulingValid state && transferSchedulingValid state) AlignmentStateSchedulingInvariant
  mapM_ validatePlanEntry (alignmentPlanEntries state)
  mapM_ validateCurrentPlan (Map.toAscList state.currentPlans)
  mapM_ validatePlanAcceptance (Map.toAscList state.planAcceptances)
  requireInvariant
    (all (\((identifier, reporter), report) -> identifier == Protocol.alignmentPlanObsoleteId report && reporter == Protocol.alignmentPlanObsoleteReporter report && not (obsoletePlanAttempt identifier state)) (Map.toAscList state.planObsoleteReports))
    AlignmentStatePlanInvariant
  requireInvariant
    (state.invalidatedGenerations == Set.unions (map alignmentPlanInvalidationReceiptGenerations (Map.elems state.planInvalidations)))
    AlignmentStatePlanInvalidationInvariant
  requireInvariant
    ( Set.fromList (Plan.alignmentPlanCoverage state.planLedger)
        == Set.fromList
          [ (alignmentPromotionKeyCause key, identifier)
          | key <- Map.keys state.promotions <> Map.keys state.historicalCoverage,
            let identifier = coordinatePlanId (promotionPlanCoordinate key),
            Just _ <- [lookupAlignmentPlan identifier state]
          ]
    )
    AlignmentStatePlanInvariant
  requireInvariant
    (state.liveStructuralDebts == reconstructLiveStructuralDebts state.structuralDebts state.promotions state.historicalCoverage state.planInvalidations)
    AlignmentStateLiveDebtInvariant
  mapM_ validatePromotion (Map.toAscList state.promotions)
  mapM_ validateHistoricalCoverage (Map.toAscList state.historicalCoverage)
  mapM_ validateGenerationEntry (Map.toAscList state.generations)
  mapM_ validateLatestEntry (Map.toAscList state.latestBySort)
  requireInvariant
    (state.pendingMemberProgress == expectedPendingMembers)
    AlignmentStateGenerationInvariant
  requireInvariant
    ( all (not . Set.null) (Map.elems state.pendingRouteCutoverGenerations)
        && routeCandidatePairs `Set.isSubsetOf` allRouteCandidatePairs
        && requiredRouteCandidatePairs `Set.isSubsetOf` routeCandidatePairs
    )
    AlignmentStateGenerationInvariant
  mapM_ validateRelation (Set.toAscList state.generationRelations)
  mapM_ validateObligationEntry (Map.toAscList state.obligations)
  mapM_ validateClosedObligation (Map.toAscList state.closedObligations)
  requireInvariant
    ( state.expectedInputOwners
        == Map.fromListWith
          Set.union
          [ (target, Set.singleton identifier)
          | (identifier, obligation) <- Map.toAscList state.obligations <> Map.toAscList state.closedObligations,
            target <- Set.toAscList (inputClosureTargets obligation)
          ]
    )
    AlignmentStateObligationInvariant
  requireInvariant
    ( state.closingObligations `Set.isSubsetOf` Map.keysSet state.obligations
        && Set.null
          (Map.keysSet state.obligations `Set.intersection` Map.keysSet state.closedObligations)
    )
    AlignmentStateObligationInvariant
  mapM_ validateBootstrapImportEntry (Map.toAscList state.bootstrapImports)
  requireInvariant
    ( sort obligationSequences
        == take (length obligationSequences) [1 ..]
        && alignmentObligationSequenceWord64 state.nextObligationSequence
          == fromIntegral (length obligationSequences + 1)
    )
    AlignmentStateObligationSequenceInvariant
  mapM_ validateCutAcceptance (Map.toAscList state.cutAcceptances)
  mapM_
    validatePendingGenerationEvidence
    (Map.toAscList state.pendingGenerationEvidence)
  mapM_ validateReady (Map.toAscList state.memberReadiness)
  mapM_ validateLocalReady (Map.toAscList state.localMemberReadiness)
  requireInvariant
    ( sort localEvidenceSequences
        == take (length localEvidenceSequences) [1 ..]
        && alignmentEvidenceSequenceWord64 state.nextEvidenceSequence
          == fromIntegral (length localEvidenceSequences + 1)
    )
    AlignmentStateEvidenceSequenceInvariant
  mapM_ validateCertificate (Map.toAscList state.certificates)
  mapM_ validateAttempt (Map.toAscList state.attempts)
  mapM_
    validateAttemptInvalidation
    (Map.toAscList state.attemptInvalidations)
  mapM_
    validatePlanInvalidation
    (Map.toAscList state.planInvalidations)
  mapM_ validateBootstrapAttempt (Map.toAscList state.bootstrapAttempts)
  mapM_
    validateBootstrapAttemptInvalidation
    (Map.toAscList state.bootstrapAttemptInvalidations)
  mapM_
    validateBootstrapFulfillment
    (Map.toAscList state.bootstrapFulfillments)
  requireInvariant
    ( length subscriptionIds == Set.size (Set.fromList subscriptionIds)
        && sort subscriptionSequences
          == take (length subscriptionSequences) [1 ..]
        && alignmentSubscriptionSequenceWord64 state.nextSubscriptionSequence
          == fromIntegral (length subscriptionSequences + 1)
    )
    AlignmentStateSubscriptionSequenceInvariant
  mapLeft
    (const AlignmentStateTransferInvariant)
    (Transfer.validateAlignmentTransferState state.transfer)
  where
    validatePlanEntry (identifier, plan) =
      requireInvariant
        ( identifier == Plan.alignmentPlanId plan
            && all
              ( \(_, binding) ->
                  let generation = Plan.alignmentPlanBindingGeneration binding
                   in Map.lookup (alignmentGenerationId generation) state.generations == Just generation
                        && case Plan.alignmentPlanBindingDisposition binding of
                          Plan.AlignmentCreated -> generationBirthPlanId generation == identifier
                          Plan.AlignmentCarried ->
                            case Plan.alignmentPlanPredecessor plan >>= (`lookupAlignmentPlan` state) of
                              Nothing -> False
                              Just previous -> generation `elem` planGenerations previous
              )
              (Plan.alignmentPlanBindings plan)
            && all (`Set.member` state.generationRelations) (Plan.alignmentPlanRelations plan)
            && maybe True (\previous -> lookupAlignmentPlan previous state /= Nothing) (Plan.alignmentPlanPredecessor plan)
        )
        AlignmentStatePlanInvariant

    validateCurrentPlan (occurrence, identifier) =
      requireInvariant
        ( occurrence == Plan.alignmentPlanIdSort identifier
            && case lookupAlignmentPlan identifier state of
              Nothing -> False
              Just plan -> Map.lookup occurrence state.latestBySort == Just (sort (map alignmentGenerationId (planGenerations plan)))
        )
        AlignmentStatePlanInvariant

    validatePlanAcceptance ((identifier, herald), accepted) =
      requireInvariant
        ( Protocol.alignmentPlanAcceptedId accepted == identifier
            && Protocol.alignmentPlanAcceptedHerald accepted == herald
            && herald `elem` map fst (NonEmpty.toList (physicalPlacementRevisionEntries (Plan.alignmentPlanIdPlacement identifier)))
            && case lookupAlignmentPlan identifier state of
              Nothing -> False
              Just plan ->
                all
                  ( \(_, binding) ->
                      Plan.alignmentPlanBindingDisposition binding == Plan.AlignmentCarried
                        || Map.lookup (alignmentGenerationId (Plan.alignmentPlanBindingGeneration binding), herald) state.cutAcceptances
                          == Just (Protocol.alignmentCutAccepted (alignmentGenerationId (Plan.alignmentPlanBindingGeneration binding)) herald (Plan.alignmentPlanIdTopology identifier) (Plan.alignmentPlanIdPlacement identifier) (Protocol.alignmentPlanAcceptedPublicationPrefix accepted))
                  )
                  (Plan.alignmentPlanBindings plan)
        )
        AlignmentStatePlanInvariant

    obligationIdentifiers =
      Set.fromList
        ( Map.keys state.obligations
            <> Map.keys state.closedObligations
            <> fmap
              ( alignmentObligationIdValue
                  . bootstrapImportSubscribeEnvelope
                  . snd
              )
              (Map.toAscList state.bootstrapImports)
            <> fmap
              ( alignmentAttemptObligationId
                  . alignmentAttemptInvalidationAttempt
              )
              (Map.elems state.attemptInvalidations)
            <> fmap
              ( alignmentObligationIdValue
                  . bootstrapImportSubscribeEnvelope
                  . bootstrapImportAttemptImport
                  . bootstrapImportAttemptInvalidationAttempt
              )
              (Map.elems state.bootstrapAttemptInvalidations)
            <> fmap
              ( alignmentObligationIdValue
                  . bootstrapImportSubscribeEnvelope
                  . bootstrapImportAttemptImport
                  . bootstrapImportFulfillmentAttempt
              )
              (Map.elems state.bootstrapFulfillments)
            <> concatMap
              ( fmap fst
                  . alignmentPlanInvalidationReceiptObligations
              )
              (Map.elems state.planInvalidations)
            <> concatMap
              ( fmap
                  ( alignmentObligationIdValue
                      . bootstrapImportSubscribeEnvelope
                      . snd
                  )
                  . alignmentPlanInvalidationReceiptBootstrapImports
              )
              (Map.elems state.planInvalidations)
        )
    obligationSequences =
      [ alignmentObligationSequenceWord64
          (alignmentObligationIdSequence identifier)
      | identifier <- Set.toAscList obligationIdentifiers
      ]
    subscriptionIds =
      fmap (alignmentAttemptSubscriptionId . snd) (Map.toAscList state.attempts)
        <> fmap
          ( alignmentAttemptSubscriptionId
              . alignmentAttemptInvalidationAttempt
              . snd
          )
          (Map.toAscList state.attemptInvalidations)
        <> fmap
          (alignmentSubscribeSubscriptionId . bootstrapImportAttemptSubscribe . snd)
          (Map.toAscList state.bootstrapAttempts)
        <> fmap
          ( alignmentSubscribeSubscriptionId
              . bootstrapImportAttemptSubscribe
              . bootstrapImportAttemptInvalidationAttempt
              . snd
          )
          (Map.toAscList state.bootstrapAttemptInvalidations)
    subscriptionSequences =
      [ alignmentSubscriptionSequenceWord64
          (alignmentSubscriptionIdSequence identifier)
      | identifier <- subscriptionIds
      ]
    localEvidenceSequences =
      [ alignmentEvidenceSequenceWord64
          (classMemberReadyEvidenceSequence ready)
      | ready <- Map.elems state.localMemberReadiness
      ]

    validateHistoricalCoverage (key, identifiers) =
      requireInvariant
        ( identifiers == sort [alignmentGenerationId generation | generation <- Map.elems state.generations, generationBelongsToCoordinate (promotionPlanCoordinate key) generation state]
            && maybe True ((== identifiers) . sort . (.generationIds)) (Map.lookup key state.promotions)
        )
        AlignmentStatePromotionInvariant

    validatePromotion (key, receipt) = do
      let expectedDebtKeys =
            Set.fromList
              [ debtKey
              | debt <- structuralDebtSetEntries state.structuralDebts,
                let debtKey = structuralConsequenceDebtKey debt,
                structuralDebtKeyCause debtKey
                  == alignmentPromotionKeyCause key,
                structuralDebtKeySort debtKey == alignmentPromotionKeySort key
              ]
          retainedGenerationIds = Map.keysSet state.generations
          retainedObligationIds =
            Set.fromList
              [ identifier
              | receiptIdentifier <- fmap (.obligationIds) (Map.elems state.promotions),
                identifier <- receiptIdentifier,
                retainedObligation identifier /= Nothing
              ]
          retainedBootstrapKeys =
            Set.fromList
              [ bootstrapKey
              | receiptKey <- fmap (.bootstrapImportKeys) (Map.elems state.promotions),
                bootstrapKey <- receiptKey,
                retainedBootstrapImport bootstrapKey /= Nothing
              ]
      requireInvariant
        ( not (Set.null expectedDebtKeys)
            && receipt.topologyCut == alignmentPromotionKeyTopologyCut key
            && receipt.placementVector == alignmentPromotionKeyPlacementVector key
            && receipt.debtKeys == expectedDebtKeys
            && Set.fromList receipt.generationIds
              `Set.isSubsetOf` retainedGenerationIds
            && receipt.relations
              `Set.isSubsetOf` state.generationRelations
            && all
              ( \relation ->
                  alignmentGenerationRelationSource relation
                    `elem` receipt.generationIds
                    && alignmentGenerationRelationDestination relation
                      `elem` receipt.generationIds
              )
              receipt.relations
            && all
              ( generationIdHasCoordinate
                  (promotionPlanCoordinate key)
              )
              receipt.generationIds
            && Set.fromList receipt.obligationIds
              `Set.isSubsetOf` retainedObligationIds
            && Set.fromList receipt.bootstrapImportKeys
              `Set.isSubsetOf` retainedBootstrapKeys
            && all (obligationMatchesPromotion key receipt) receipt.obligationIds
            && all
              (bootstrapMatchesPromotion key receipt)
              receipt.bootstrapImportKeys
            && case lookupAlignmentPlan (coordinatePlanId (promotionPlanCoordinate key)) state of
              Nothing -> True
              Just plan ->
                sort receipt.generationIds == sort (map alignmentGenerationId (planGenerations plan))
                  && receipt.relations == Set.fromList (Plan.alignmentPlanRelations plan)
        )
        AlignmentStatePromotionInvariant

    obligationMatchesPromotion key receipt identifier =
      case retainedObligation identifier of
        Nothing -> False
        Just obligation ->
          alignmentObligationCause obligation
            == alignmentPromotionKeyCause key
            && sortOccurrence
              (alignmentObligationSortId obligation)
              (alignmentObligationSortDefinitionOccurrenceId obligation)
              == alignmentPromotionKeySort key
            && alignmentObligationDestinationGeneration obligation
              `elem` receipt.generationIds
            && alignmentObligationSourceGeneration obligation
              `elem` receipt.generationIds

    bootstrapMatchesPromotion key receipt bootstrapKey =
      case retainedBootstrapImport bootstrapKey of
        Nothing -> False
        Just bootstrapImport ->
          let envelope = bootstrapImport.subscribeEnvelope
           in alignmentObligationCause envelope
                == alignmentPromotionKeyCause key
                && sortOccurrence
                  (alignmentObligationSortId envelope)
                  (alignmentObligationSortDefinitionOccurrenceId envelope)
                  == alignmentPromotionKeySort key
                && bootstrapKey.generation `elem` receipt.generationIds

    retainedObligation identifier =
      exactRetainedValue
        ( maybe [] pure (Map.lookup identifier state.obligations)
            <> maybe [] pure (Map.lookup identifier state.closedObligations)
            <> [ obligation
               | invalidation <- Map.elems state.planInvalidations,
                 (retainedIdentifier, obligation) <-
                   alignmentPlanInvalidationReceiptObligations invalidation,
                 retainedIdentifier == identifier
               ]
            <> [ alignmentAttemptObligation attempt
               | invalidation <- Map.elems state.attemptInvalidations,
                 let attempt = alignmentAttemptInvalidationAttempt invalidation,
                 alignmentAttemptObligationId attempt == identifier,
                 Set.member
                   AlignmentRelationRemoved
                   (alignmentAttemptInvalidationReasons invalidation)
               ]
        )

    retainedBootstrapImport key =
      exactRetainedValue
        ( maybe [] pure (Map.lookup key state.bootstrapImports)
            <> [ bootstrapImportAttemptImport
                   (bootstrapImportFulfillmentAttempt fulfillment)
               | fulfillment <- Map.elems state.bootstrapFulfillments,
                 bootstrapImportKeyValue
                   ( bootstrapImportAttemptImport
                       (bootstrapImportFulfillmentAttempt fulfillment)
                   )
                   == key
               ]
            <> [ bootstrapImport
               | invalidation <- Map.elems state.planInvalidations,
                 (retainedKey, bootstrapImport) <-
                   alignmentPlanInvalidationReceiptBootstrapImports invalidation,
                 retainedKey == key
               ]
            <> [ bootstrapImportAttemptImport attempt
               | invalidation <- Map.elems state.bootstrapAttemptInvalidations,
                 let attempt = bootstrapImportAttemptInvalidationAttempt invalidation,
                 bootstrapImportKeyValue (bootstrapImportAttemptImport attempt) == key,
                 Set.member
                   AlignmentRelationRemoved
                   (bootstrapImportAttemptInvalidationReasons invalidation)
               ]
        )

    exactRetainedValue [] = Nothing
    exactRetainedValue (candidate : remaining)
      | all (== candidate) remaining = Just candidate
      | otherwise = Nothing

    validateGenerationEntry (identifier, generation) = do
      let cut = alignmentGenerationCut generation
          members = NonEmpty.toList (alignmentCutExactMembers cut)
          memberDeltas =
            fmap alignmentMemberDelta members
          contextDeltas =
            NonEmpty.toList
              (contextClassMembers (alignmentGenerationContextClass generation))
          expectedAnnouncer = alignmentMemberHerald (minimum members)
      requireInvariant
        ( identifier == alignmentGenerationId generation
            && identifier == deriveContextClassGenerationId cut
            && memberDeltas == contextDeltas
            && alignmentGenerationAnnouncer generation == expectedAnnouncer
            && all (`Map.member` state.generations) (alignmentCutPredecessorGenerationIds cut)
            && all predecessorHasCertificate (alignmentCutPredecessorGenerationIds cut)
        )
        AlignmentStateGenerationInvariant

    predecessorHasCertificate identifier =
      any
        ((== identifier) . fst . fst)
        (Map.toAscList state.certificates)

    nonInvalidatedGenerations =
      [ generation
      | generation <- Map.elems state.generations,
        not (generationCurrentPlanInvalidated generation state)
      ]
    expectedPendingMembers =
      Map.fromListWith
        Map.union
        [ (alignmentMemberHerald member, Map.singleton (identifier, alignmentMemberDelta member) member)
        | generation <- nonInvalidatedGenerations,
          let identifier = alignmentGenerationId generation,
          member <- NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)),
          let key = (identifier, alignmentMemberStoreIncarnation member),
          not (Map.member key state.localMemberReadiness && Map.member key state.certificates)
        ]
    allRouteCandidatePairs =
      Set.fromList
        [ (source, alignmentGenerationId generation)
        | generation <- nonInvalidatedGenerations,
          not (null (alignmentCutPredecessorGenerationIds (alignmentGenerationCut generation))),
          (source, _) <- NonEmpty.toList (physicalPlacementRevisionEntries (alignmentCutPhysicalPlacementRevisionVector (alignmentGenerationCut generation)))
        ]
    routeCandidatePairs =
      Set.fromList
        [ (source, identifier)
        | (source, identifiers) <- Map.toAscList state.pendingRouteCutoverGenerations,
          identifier <- Set.toAscList identifiers
        ]
    requiredRouteCandidatePairs =
      Set.filter
        ( \(source, identifier) -> case Map.lookup identifier state.generations of
            Nothing -> False
            Just generation -> not (routeCutoverGenerationCompleted source generation state)
        )
        allRouteCandidatePairs

    validateLatestEntry (typedSort, identifiers) =
      requireInvariant
        ( identifiers == Set.toAscList (Set.fromList identifiers)
            && all (generationMatchesSort typedSort) identifiers
        )
        AlignmentStateGenerationInvariant

    generationMatchesSort typedSort identifier =
      case Map.lookup identifier state.generations of
        Nothing -> False
        Just generation ->
          let cut = alignmentGenerationCut generation
           in typedSort
                == sortOccurrence
                  (alignmentCutSortId cut)
                  (alignmentCutSortDefinitionOccurrenceId cut)

    validateRelation relation =
      requireInvariant
        ( case ( Map.lookup
                   (alignmentGenerationRelationSource relation)
                   state.generations,
                 Map.lookup
                   (alignmentGenerationRelationDestination relation)
                   state.generations
               ) of
            (Just source, Just destination) ->
              any (elem relation . Plan.alignmentPlanRelations . snd) (alignmentPlanEntries state)
                || generationCutCoordinates source == generationCutCoordinates destination
            _ -> False
        )
        AlignmentStateRelationInvariant

    generationCutCoordinates = generationBirthPlanId

    validateObligationEntry (identifier, obligation) =
      requireInvariant
        ( identifier == alignmentObligationIdValue obligation
            && alignmentObligationIdDestinationHerald identifier
              == destinationHerald obligation
            && relationMatchesObligation obligation
            && ( any
                   (elem identifier . (.obligationIds) . snd)
                   (Map.toAscList state.promotions)
               )
        )
        AlignmentStateObligationInvariant

    validateClosedObligation (identifier, obligation) =
      requireInvariant
        ( identifier == alignmentObligationIdValue obligation
            && alignmentObligationIdDestinationHerald identifier
              == destinationHerald obligation
            && relationMatchesObligation obligation
            && retainedObligation identifier == Just obligation
            && Map.notMember identifier state.obligations
            && Set.notMember identifier state.closingObligations
            && Map.notMember identifier state.attempts
            && any
              (elem identifier . (.obligationIds) . snd)
              (Map.toAscList state.promotions)
        )
        AlignmentStateObligationInvariant

    validateBootstrapImportEntry (key, bootstrapImport) =
      requireInvariant
        (bootstrapImportInvariant key bootstrapImport)
        AlignmentStateBootstrapImportInvariant

    bootstrapImportInvariant key bootstrapImport =
      let envelope = bootstrapImport.subscribeEnvelope
          identifier = alignmentObligationIdValue envelope
          expectedSourceGeneration = case key.source of
            BootstrapFromPredecessor predecessor -> predecessor
            BootstrapFromFreshBase _ -> key.generation
       in bootstrapImport.key == key
            && alignmentObligationDestinationGeneration envelope
              == key.generation
            && alignmentObligationSourceGeneration envelope
              == expectedSourceGeneration
            && alignmentObligationFrozenContextPathStrength envelope == Normal
            && NonEmpty.toList
              (alignmentObligationDestinationStores envelope)
              == [key.destination]
            && Just (alignmentObligationIdDestinationHerald identifier)
              == bootstrapDestinationHerald key
            && bootstrapSourceBelongsToCut key
            && any
              (elem key . (.bootstrapImportKeys) . snd)
              (Map.toAscList state.promotions)

    bootstrapDestinationHerald key =
      case Map.lookup key.generation state.generations of
        Nothing -> Nothing
        Just generation ->
          case [ alignmentMemberHerald member
               | member <-
                   NonEmpty.toList
                     (alignmentCutExactMembers (alignmentGenerationCut generation)),
                 alignmentMemberDelta member == destinationStoreDelta key.destination,
                 alignmentMemberStoreIncarnation member
                   == destinationStoreIncarnation key.destination
               ] of
            [herald] -> Just herald
            _ -> Nothing

    bootstrapSourceBelongsToCut key =
      case Map.lookup key.generation state.generations of
        Nothing -> False
        Just generation ->
          let cut = alignmentGenerationCut generation
           in case key.source of
                BootstrapFromPredecessor predecessor ->
                  predecessor `elem` alignmentCutPredecessorGenerationIds cut
                BootstrapFromFreshBase fresh ->
                  fresh `elem` alignmentCutFreshMemberBaseEvidence cut

    destinationHerald obligation =
      alignmentObligationIdDestinationHerald
        (alignmentObligationIdValue obligation)

    relationMatchesObligation obligation =
      let source = alignmentObligationSourceGeneration obligation
          destination = alignmentObligationDestinationGeneration obligation
          relationRetained =
            any
              ( \relation ->
                  alignmentGenerationRelationSource relation == source
                    && alignmentGenerationRelationDestination relation == destination
                    && alignmentGenerationRelationStrength relation
                      == alignmentObligationFrozenContextPathStrength obligation
              )
              (Set.toAscList state.generationRelations)
       in relationRetained
            && case Map.lookup destination state.generations of
              Nothing -> False
              Just generation ->
                let localMembers =
                      [ ( alignmentMemberDelta member,
                          alignmentMemberStoreIncarnation member
                        )
                      | member <-
                          NonEmpty.toList
                            (alignmentCutExactMembers (alignmentGenerationCut generation)),
                        alignmentMemberHerald member == destinationHerald obligation
                      ]
                    destinations =
                      [ (destinationStoreDelta store, destinationStoreIncarnation store)
                      | store <-
                          NonEmpty.toList
                            (alignmentObligationDestinationStores obligation)
                      ]
                 in destinations == localMembers

    validateCutAcceptance ((generationId, acceptor), accepted) =
      requireInvariant
        ( alignmentCutAcceptedGeneration accepted == generationId
            && alignmentCutAcceptedHerald accepted == acceptor
            && case Map.lookup generationId state.generations of
              Nothing -> False
              Just generation ->
                let cut = alignmentGenerationCut generation
                 in alignmentCutAcceptedTopologyCut accepted
                      == deriveTopologyCutId (alignmentCutTopologyCut cut)
                      && alignmentCutAcceptedPlacementVector accepted
                        == alignmentCutPhysicalPlacementRevisionVector cut
                      && any
                        ((== acceptor) . fst)
                        ( NonEmpty.toList
                            ( physicalPlacementRevisionEntries
                                (alignmentCutPhysicalPlacementRevisionVector cut)
                            )
                        )
        )
        AlignmentStateCutAcceptanceInvariant

    validatePendingGenerationEvidence (key, evidence) =
      requireInvariant
        (pendingGenerationEvidenceKey evidence.control == Right key)
        AlignmentStatePendingEvidenceInvariant

    validateReady ((generationId, store), ready) =
      requireInvariant
        ( classMemberReadyGeneration ready == generationId
            && classMemberReadyStoreIncarnation ready == store
            && generationHasStore generationId store
        )
        AlignmentStateReadinessInvariant

    validateLocalReady (key, ready) =
      requireInvariant
        (Map.lookup key state.memberReadiness == Just ready)
        AlignmentStateEvidenceSequenceInvariant

    validateCertificate ((generationId, store), certificate) =
      let memberStores = generationStores generationId
          ready =
            [ evidence
            | ((retainedGeneration, retainedStore), evidence) <-
                Map.toAscList state.memberReadiness,
              retainedGeneration == generationId,
              Set.member retainedStore memberStores
            ]
       in requireInvariant
            ( historicalCertificateClassGeneration certificate == generationId
                && historicalCertificateSourceStoreIncarnation certificate == store
                && Set.fromList (fmap classMemberReadyStoreIncarnation ready)
                  == memberStores
                && historicalCertificateExactMemberReadyDigest certificate
                  == classMemberReadySetDigest ready
                && case Map.lookup (generationId, store) state.memberReadiness of
                  Nothing -> False
                  Just sourceReady ->
                    historicalCertificateCertifiedStoreRevision certificate
                      == classMemberReadyStoreRevision sourceReady
                      && historicalCertificateCompletedPredecessorPrefixDigest certificate
                        == deriveBootstrapEvidenceDigest
                          ( memberReadyEvidenceDigestBytes
                              ( classMemberReadyPredecessorAndBasePrefixDigest
                                  sourceReady
                              )
                          )
            )
            AlignmentStateCertificateInvariant

    generationStores generationId =
      case Map.lookup generationId state.generations of
        Nothing -> Set.empty
        Just generation ->
          Set.fromList
            ( fmap
                alignmentMemberStoreIncarnation
                ( NonEmpty.toList
                    (alignmentCutExactMembers (alignmentGenerationCut generation))
                )
            )

    generationHasStore generationId store =
      Set.member store (generationStores generationId)

    validateAttempt (identifier, attempt) =
      requireInvariant
        ( attemptCoreInvariant identifier attempt
            && not (alignmentAttemptSourceExcluded Set.empty attempt state)
            && Map.lookup identifier state.obligations
              == Just (alignmentAttemptObligation attempt)
            && Map.notMember
              (alignmentAttemptSubscriptionId attempt)
              state.attemptInvalidations
        )
        AlignmentStateAttemptInvariant

    validateAttemptInvalidation (subscription, receipt) =
      let attempt = alignmentAttemptInvalidationAttempt receipt
          identifier = alignmentAttemptObligationId attempt
          reasons = alignmentAttemptInvalidationReasons receipt
          terminal =
            Set.member AlignmentDestinationIncarnationLost reasons
              || Set.member AlignmentRelationRemoved reasons
          ownerRetention =
            if terminal
              then
                Map.notMember identifier state.obligations
                  && retainedObligation identifier
                    == Just (alignmentAttemptObligation attempt)
              else
                Map.lookup identifier state.obligations
                  == Just (alignmentAttemptObligation attempt)
       in requireInvariant
            ( subscription == alignmentAttemptSubscriptionId attempt
                && not (Set.null reasons)
                && attemptCoreInvariant identifier attempt
                && ( not (Set.member AlignmentSourceIncarnationLost reasons)
                       || maybe
                         False
                         (`Set.member` state.unavailableSources)
                         (alignmentAttemptUnavailableSourceCoordinate attempt state)
                   )
                && ownerRetention
                && all
                  ((/= subscription) . alignmentAttemptSubscriptionId)
                  (Map.elems state.attempts)
                && terminalDestinationCancellationIsExact
                  subscription
                  (dominantAlignmentCancelReason reasons)
            )
            AlignmentStateAttemptInvalidationInvariant

    attemptCoreInvariant identifier attempt =
      identifier == alignmentAttemptObligationId attempt
        && relationMatchesObligation (alignmentAttemptObligation attempt)
        && any
          (elem identifier . (.obligationIds) . snd)
          (Map.toAscList state.promotions)
        && alignmentSubscriptionIdDestinationHerald
          (alignmentAttemptSubscriptionId attempt)
          == alignmentObligationIdDestinationHerald identifier
        && Map.lookup
          ( alignmentObligationSourceGeneration
              (alignmentAttemptObligation attempt),
            alignmentAttemptSourceStoreIncarnation attempt
          )
          state.certificates
          == Just (alignmentAttemptHistoricalCertificate attempt)
        && Map.member
          ( alignmentObligationSourceGeneration
              (alignmentAttemptObligation attempt),
            alignmentAttemptSourceHerald attempt
          )
          state.cutAcceptances
        && Map.member
          ( alignmentObligationDestinationGeneration
              (alignmentAttemptObligation attempt),
            alignmentObligationIdDestinationHerald identifier
          )
          state.cutAcceptances
        && Map.member
          ( alignmentObligationDestinationGeneration
              (alignmentAttemptObligation attempt),
            alignmentAttemptSourceHerald attempt
          )
          state.cutAcceptances

    terminalDestinationCancellationIsExact subscription reason =
      any
        ( \(identifier, destination) ->
            identifier == subscription
              && Transfer.destinationSubscriptionCancellation destination
                == Just (alignmentCancel subscription reason)
        )
        (Transfer.destinationSubscriptionEntries state.transfer)

    validatePlanInvalidation (coordinate, receipt) =
      requireInvariant
        ( coordinate == alignmentPlanInvalidationReceiptCoordinate receipt
            && all (generationIdHasCoordinate coordinate) (Set.toAscList (alignmentPlanInvalidationReceiptGenerations receipt))
            && not
              (Set.null (alignmentPlanInvalidationReceiptCauses receipt))
            && all
              (planCauseMatchesCoordinate coordinate)
              (Set.toAscList (alignmentPlanInvalidationReceiptCauses receipt))
            && all
              planCauseRetainsUnavailableSource
              (Set.toAscList (alignmentPlanInvalidationReceiptCauses receipt))
            && all
              ( \(identifier, obligation) ->
                  identifier == alignmentObligationIdValue obligation
                    && generationIdHasCoordinate
                      coordinate
                      (alignmentObligationDestinationGeneration obligation)
                    && Set.member (alignmentObligationDestinationGeneration obligation) (alignmentPlanInvalidationReceiptGenerations receipt)
                    && Map.notMember identifier state.obligations
                    && retainedObligation identifier == Just obligation
              )
              (alignmentPlanInvalidationReceiptObligations receipt)
            && all
              ( \(key, bootstrapImport) ->
                  key == bootstrapImportKeyValue bootstrapImport
                    && generationIdHasCoordinate coordinate key.generation
                    && Set.member key.generation (alignmentPlanInvalidationReceiptGenerations receipt)
                    && Map.notMember key state.bootstrapImports
                    && retainedBootstrapImport key == Just bootstrapImport
              )
              (alignmentPlanInvalidationReceiptBootstrapImports receipt)
            && all
              ( not
                  . generationIdAffectedByInvalidation coordinate
                  . alignmentObligationDestinationGeneration
              )
              (Map.elems state.obligations)
            && all
              ( \key ->
                  not (generationIdAffectedByInvalidation coordinate key.generation)
              )
              (Map.keys state.bootstrapImports)
            && all
              ( not
                  . generationIdAffectedByInvalidation coordinate
                  . alignmentObligationDestinationGeneration
                  . alignmentAttemptObligation
              )
              (Map.elems state.attempts)
            && all
              ( \attempt ->
                  not
                    ( generationIdAffectedByInvalidation
                        coordinate
                        ( bootstrapImportKeyGeneration
                            ( bootstrapImportKeyValue
                                (bootstrapImportAttemptImport attempt)
                            )
                        )
                    )
              )
              (Map.elems state.bootstrapAttempts)
        )
        AlignmentStatePlanInvalidationInvariant

    generationIdAffectedByInvalidation coordinate identifier =
      maybe False (Set.member identifier . alignmentPlanInvalidationReceiptGenerations) (Map.lookup coordinate state.planInvalidations)

    planCauseMatchesCoordinate coordinate cause = case cause of
      AlignmentPlanSuperseded previous replacement ->
        coordinatePlanId coordinate == previous
          && Plan.alignmentPlanIdSort previous == Plan.alignmentPlanIdSort replacement
          && Plan.alignmentPlanIdAttempt previous < Plan.alignmentPlanIdAttempt replacement
          && maybe False ((== Plan.AlignmentPredecessorReset) . Plan.alignmentPlanPredecessorStatus) (lookupAlignmentPlan replacement state)
      _ -> case planInvalidationCauseGeneration cause >>= (`Map.lookup` state.generations) of
        Nothing -> False
        Just generation ->
          generationBelongsToCoordinate coordinate generation state
            && case validatePlanInvalidationCause cause generation of
              Left _ -> False
              Right () -> True

    planCauseRetainsUnavailableSource cause = case cause of
      AlignmentPlanSuperseded _ _ -> True
      AlignmentPlanDestinationStoreLost _ _ -> True
      AlignmentPlanFreshBaseStoreLost _ fresh source ->
        Set.member
          ( UnavailableAlignmentSource
              (alignmentSourceCoordinateHerald source)
              (freshMemberBaseDelta fresh)
              (alignmentSourceCoordinateStoreIncarnation source)
          )
          state.unavailableSources

    generationIdHasCoordinate coordinate identifier =
      maybe
        False
        (\generation -> generationBelongsToCoordinate coordinate generation state)
        (Map.lookup identifier state.generations)

    validateBootstrapAttempt (key, attempt) =
      requireInvariant
        ( case Map.lookup key state.bootstrapImports of
            Just bootstrapImport ->
              attempt.bootstrapImport == bootstrapImport
                && Map.notMember
                  (alignmentSubscribeSubscriptionId attempt.subscribe)
                  state.bootstrapAttemptInvalidations
                && alignmentSubscribeObligation attempt.subscribe
                  == bootstrapImport.subscribeEnvelope
                && alignmentSubscriptionIdDestinationHerald
                  (alignmentSubscribeSubscriptionId attempt.subscribe)
                  == alignmentObligationIdDestinationHerald
                    (alignmentObligationIdValue bootstrapImport.subscribeEnvelope)
                && Map.member
                  ( key.generation,
                    alignmentObligationIdDestinationHerald
                      (alignmentObligationIdValue bootstrapImport.subscribeEnvelope)
                  )
                  state.cutAcceptances
                && bootstrapAttemptSourceIsRetained key attempt
                && not
                  ( bootstrapImportAttemptSourceExcluded
                      Set.empty
                      attempt
                      state
                  )
                && any
                  ( \(identifier, destination) ->
                      identifier
                        == alignmentSubscribeSubscriptionId attempt.subscribe
                        && Transfer.destinationSubscriptionSubscribe destination
                          == attempt.subscribe
                        && Transfer.destinationSubscriptionBootstrapSourceHerald destination
                          == Just attempt.sourceHerald
                  )
                  (Transfer.destinationSubscriptionEntries state.transfer)
            Nothing -> False
        )
        AlignmentStateBootstrapImportInvariant

    validateBootstrapAttemptInvalidation (subscription, receipt) =
      let attempt = bootstrapImportAttemptInvalidationAttempt receipt
          bootstrapImport = attempt.bootstrapImport
          key = bootstrapImport.key
          identifier =
            alignmentObligationIdValue bootstrapImport.subscribeEnvelope
          reasons = bootstrapImportAttemptInvalidationReasons receipt
          sourceOnlyPredecessor =
            reasons == Set.singleton AlignmentSourceIncarnationLost
              && case key.source of
                BootstrapFromPredecessor _ -> True
                BootstrapFromFreshBase _ -> False
          ownerRetention =
            if sourceOnlyPredecessor
              then Map.lookup key state.bootstrapImports == Just bootstrapImport
              else
                Map.notMember key state.bootstrapImports
                  && retainedBootstrapImport key == Just bootstrapImport
       in requireInvariant
            ( subscription == alignmentSubscribeSubscriptionId attempt.subscribe
                && not (Set.null reasons)
                && bootstrapImportInvariant key bootstrapImport
                && alignmentSubscribeObligation attempt.subscribe
                  == bootstrapImport.subscribeEnvelope
                && alignmentSubscriptionIdDestinationHerald subscription
                  == alignmentObligationIdDestinationHerald identifier
                && bootstrapAttemptSourceIsRetained key attempt
                && ( not (Set.member AlignmentSourceIncarnationLost reasons)
                       || maybe
                         False
                         (`Set.member` state.unavailableSources)
                         ( bootstrapImportAttemptUnavailableSourceCoordinate
                             attempt
                             state
                         )
                   )
                && all
                  ( (/= subscription)
                      . alignmentSubscribeSubscriptionId
                      . bootstrapImportAttemptSubscribe
                  )
                  (Map.elems state.bootstrapAttempts)
                && ownerRetention
                && terminalDestinationCancellationIsExact
                  subscription
                  (dominantAlignmentCancelReason reasons)
            )
            AlignmentStateBootstrapAttemptInvalidationInvariant

    validateBootstrapFulfillment (key, receipt) =
      let attempt = bootstrapImportFulfillmentAttempt receipt
          bootstrapImport = bootstrapImportAttemptImport attempt
          subscription = alignmentSubscribeSubscriptionId attempt.subscribe
          through = bootstrapImportFulfillmentThrough receipt
          retainedInvalidation =
            Map.lookup subscription state.bootstrapAttemptInvalidations
          retainedDestination =
            [ destination
            | (identifier, destination) <-
                Transfer.destinationSubscriptionEntries state.transfer,
              identifier == subscription
            ]
       in requireInvariant
            ( key == bootstrapImportKeyValue bootstrapImport
                && bootstrapImportInvariant key bootstrapImport
                && Map.notMember key state.bootstrapImports
                && Map.notMember key state.bootstrapAttempts
                && case (retainedInvalidation, retainedDestination) of
                  (Just invalidation, [destination]) ->
                    let reasons =
                          bootstrapImportAttemptInvalidationReasons invalidation
                     in bootstrapImportAttemptInvalidationAttempt invalidation == attempt
                          && Set.member AlignmentRelationRemoved reasons
                          && Transfer.destinationSubscriptionSubscribe destination
                            == attempt.subscribe
                          && Transfer.destinationSubscriptionBootstrapSourceHerald destination
                            == Just attempt.sourceHerald
                          && Transfer.destinationSubscriptionAppliedThrough destination
                            == Just through
                          && Transfer.destinationSubscriptionLiveThrough destination
                            == Just through
                          && Transfer.destinationSubscriptionAcknowledgedThrough destination
                            == Just through
                          && Transfer.destinationSubscriptionCancellation destination
                            == Just
                              ( alignmentCancel
                                  subscription
                                  (dominantAlignmentCancelReason reasons)
                              )
                  _ -> False
            )
            AlignmentStateBootstrapFulfillmentInvariant

    bootstrapAttemptSourceIsRetained key attempt =
      let sourceStore =
            alignmentSubscribeSourceStoreIncarnation attempt.subscribe
          accepted =
            Map.member
              (key.generation, attempt.sourceHerald)
              state.cutAcceptances
       in accepted && case key.source of
            BootstrapFromPredecessor predecessorGeneration ->
              case ( Map.lookup predecessorGeneration state.generations,
                     Map.lookup
                       (predecessorGeneration, sourceStore)
                       state.certificates
                   ) of
                (Just predecessor, Just certificate) ->
                  generationMemberHostedBy
                    sourceStore
                    attempt.sourceHerald
                    predecessor
                    && Map.member
                      (predecessorGeneration, attempt.sourceHerald)
                      state.cutAcceptances
                    && alignmentSubscribeSourceProof attempt.subscribe
                      == BootstrapProof
                        key.generation
                        ( predecessorBootstrapEvidenceDigest
                            key.generation
                            certificate
                        )
                _ -> False
            BootstrapFromFreshBase fresh ->
              sourceStore == freshMemberBaseStoreIncarnation fresh
                && case Map.lookup key.generation state.generations of
                  Nothing -> False
                  Just generation ->
                    generationMemberHostedBy
                      sourceStore
                      attempt.sourceHerald
                      generation
                      && alignmentSubscribeSourceProof attempt.subscribe
                        == BootstrapProof
                          key.generation
                          (freshBootstrapEvidenceDigest key.generation fresh)

    generationMemberHostedBy sourceStore sourceHerald generation =
      any
        ( \member ->
            alignmentMemberStoreIncarnation member == sourceStore
              && alignmentMemberHerald member == sourceHerald
        )
        (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)))

requireInvariant ::
  Bool -> problem -> Either problem ()
requireInvariant condition problem
  | condition = Right ()
  | otherwise = Left problem

-- Explicit current plans retain immutable birth generations by reference.
planGenerations :: Plan.AlignmentPlan -> [AlignmentGeneration]
planGenerations = map (Plan.alignmentPlanBindingGeneration . snd) . Plan.alignmentPlanBindings

alignmentPlanEntries :: State -> [(Plan.AlignmentPlanId, Plan.AlignmentPlan)]
alignmentPlanEntries state = Plan.alignmentPlanEntries state.planLedger

lookupAlignmentPlan :: Plan.AlignmentPlanId -> State -> Maybe Plan.AlignmentPlan
lookupAlignmentPlan identifier state = Plan.lookupAlignmentPlan identifier state.planLedger

currentAlignmentPlanForSort :: SortOccurrence -> State -> Maybe Plan.AlignmentPlan
currentAlignmentPlanForSort occurrence state = Map.lookup occurrence state.currentPlans >>= (`lookupAlignmentPlan` state)

liveCurrentAlignmentPlans :: State -> [Plan.AlignmentPlan]
liveCurrentAlignmentPlans state = [plan | identifier <- Map.elems state.currentPlans, not (alignmentPlanInvalidated identifier state), Just plan <- [lookupAlignmentPlan identifier state]]

planCoordinate :: Plan.AlignmentPlanId -> AlignmentPlanCoordinate
planCoordinate = AlignmentPlanCoordinate

coordinatePlanId :: AlignmentPlanCoordinate -> Plan.AlignmentPlanId
coordinatePlanId = alignmentPlanCoordinateId

generationBirthPlanId :: AlignmentGeneration -> Plan.AlignmentPlanId
generationBirthPlanId = coordinatePlanId . generationPlanCoordinate

alignmentPlanInvalidated :: Plan.AlignmentPlanId -> State -> Bool
alignmentPlanInvalidated identifier state = Map.member (planCoordinate identifier) state.planInvalidations

currentOrBirthCoordinate :: AlignmentGeneration -> State -> AlignmentPlanCoordinate
currentOrBirthCoordinate generation state =
  case currentAlignmentPlanForSort (alignmentPlanCoordinateSort (generationPlanCoordinate generation)) state of
    Just plan | alignmentGenerationId generation `elem` map alignmentGenerationId (planGenerations plan) -> planCoordinate (Plan.alignmentPlanId plan)
    _ -> generationPlanCoordinate generation

generationCurrentPlanInvalidated :: AlignmentGeneration -> State -> Bool
generationCurrentPlanInvalidated generation state = Set.member (alignmentGenerationId generation) state.invalidatedGenerations

generationBelongsToCoordinate :: AlignmentPlanCoordinate -> AlignmentGeneration -> State -> Bool
generationBelongsToCoordinate coordinate generation state = case lookupAlignmentPlan (coordinatePlanId coordinate) state of
  Just plan -> alignmentGenerationId generation `elem` map alignmentGenerationId (planGenerations plan)
  Nothing -> generationPlanCoordinate generation == coordinate

alignmentPlanAcceptanceEntries :: State -> [((Plan.AlignmentPlanId, HeraldEpoch), Protocol.AlignmentPlanAccepted)]
alignmentPlanAcceptanceEntries state = Map.toAscList state.planAcceptances

lookupAlignmentPlanAcceptance :: Plan.AlignmentPlanId -> HeraldEpoch -> State -> Maybe Protocol.AlignmentPlanAccepted
lookupAlignmentPlanAcceptance identifier herald state = Map.lookup (identifier, herald) state.planAcceptances

-- | Retain the first exact cause report, including a rejected plan that was
-- never adopted here. A completed reset makes older attempt reports inert.
retainAlignmentPlanObsolete :: Protocol.AlignmentPlanObsolete -> State -> Either AlignmentCutEvidenceProblem (State, AlignmentCutEvidenceDisposition)
retainAlignmentPlanObsolete report state
  | obsoletePlanAttempt identifier state || Map.member key state.planObsoleteReports = Right (state, AlignmentCutEvidenceUnchanged)
  | otherwise =
      Right
        ( wakeGenerationPlanSorts
            (Set.singleton occurrence)
            state
              { planObsoleteReports = Map.insert key report state.planObsoleteReports,
                planObsoleteSorts = Set.insert occurrence state.planObsoleteSorts,
                generationPlanWork = WorkIndex.registerWork occurrence Set.empty state.generationPlanWork
              },
          AlignmentCutEvidenceRetained
        )
  where
    identifier = Protocol.alignmentPlanObsoleteId report
    occurrence = Plan.alignmentPlanIdSort identifier
    key = (identifier, Protocol.alignmentPlanObsoleteReporter report)

alignmentPlanObsoleteEntries :: State -> [Protocol.AlignmentPlanObsolete]
alignmentPlanObsoleteEntries state = Map.elems state.planObsoleteReports

alignmentPlanObsoleteForSort :: SortOccurrence -> State -> [Protocol.AlignmentPlanObsolete]
alignmentPlanObsoleteForSort occurrence = filter ((== occurrence) . Plan.alignmentPlanIdSort . Protocol.alignmentPlanObsoleteId) . alignmentPlanObsoleteEntries

alignmentResetRequiredSorts :: State -> Set SortOccurrence
alignmentResetRequiredSorts state = state.planObsoleteSorts

obsoletePlanAttempt :: Plan.AlignmentPlanId -> State -> Bool
obsoletePlanAttempt identifier state =
  maybe False ((> Plan.alignmentPlanIdAttempt identifier) . Plan.alignmentPlanIdAttempt) (Map.lookup (Plan.alignmentPlanIdSort identifier) state.currentPlans)

-- A created generation's immutable cut acceptance is the projection of its
-- creation-plan acceptance. Carried generations keep the old prefix unchanged.
retainAlignmentPlanAcceptance :: Protocol.AlignmentPlanAccepted -> State -> Either AlignmentCutEvidenceProblem (State, AlignmentCutEvidenceDisposition)
retainAlignmentPlanAcceptance accepted state
  | obsoletePlanAttempt (Protocol.alignmentPlanAcceptedId accepted) state = Right (state, AlignmentCutEvidenceUnchanged)
  | otherwise = retainCurrentAlignmentPlanAcceptance accepted state

retainCurrentAlignmentPlanAcceptance :: Protocol.AlignmentPlanAccepted -> State -> Either AlignmentCutEvidenceProblem (State, AlignmentCutEvidenceDisposition)
retainCurrentAlignmentPlanAcceptance accepted state = do
  plan <- maybe (Left (AlignmentPlanEvidenceUnknown identifier)) Right (lookupAlignmentPlan identifier state)
  case Map.lookup (identifier, herald) state.planAcceptances of
    Just retained | retained == accepted -> Right (state, AlignmentCutEvidenceUnchanged)
    Just _ -> Left (AlignmentPlanEvidenceConflict identifier herald)
    Nothing -> do
      let created = [Plan.alignmentPlanBindingGeneration binding | (_, binding) <- Plan.alignmentPlanBindings plan, Plan.alignmentPlanBindingDisposition binding == Plan.AlignmentCreated]
          retainCut predecessor generation = do
            let projected = Protocol.alignmentCutAccepted (alignmentGenerationId generation) herald (Plan.alignmentPlanIdTopology identifier) (Plan.alignmentPlanIdPlacement identifier) (Protocol.alignmentPlanAcceptedPublicationPrefix accepted)
            case Map.lookup (alignmentGenerationId generation, herald) predecessor.cutAcceptances of
              Just existing | existing /= projected -> Left (AlignmentCutEvidenceConflict (alignmentGenerationId generation) herald)
              _ ->
                Right
                  (notifyTransferGenerationEvidence (alignmentGenerationId generation) predecessor)
                    { cutAcceptances = Map.insert (alignmentGenerationId generation, herald) projected predecessor.cutAcceptances,
                      routeMarkerChanges = Set.insert (alignmentGenerationId generation) predecessor.routeMarkerChanges
                    }
      withCuts <- foldM retainCut state created
      Right
        ( (notifyPreparationReadiness (Plan.alignmentPlanIdSort identifier) withCuts)
            { planAcceptances = Map.insert (identifier, herald) accepted withCuts.planAcceptances
            },
          AlignmentCutEvidenceRetained
        )
  where
    identifier = Protocol.alignmentPlanAcceptedId accepted
    herald = Protocol.alignmentPlanAcceptedHerald accepted

-- Carry preserves exact incoming streams: there is no new historical prefix
-- scope for that member. Created members retain their checked closure-backed
-- local readiness. Both require acceptance of the current routing coordinate.
currentAlignmentMemberReady :: HeraldEpoch -> ContextClassGenerationId -> StoreIncarnationId -> State -> Bool
currentAlignmentMemberReady local identifier store state =
  Map.member (identifier, store) state.localMemberReadiness
    && case Map.lookup identifier state.generations of
      Nothing -> False
      Just generation -> case currentAlignmentPlanForSort (alignmentPlanCoordinateSort (generationPlanCoordinate generation)) state of
        Nothing -> False
        Just plan ->
          not (alignmentPlanInvalidated (Plan.alignmentPlanId plan) state)
            && identifier `elem` map alignmentGenerationId (planGenerations plan)
            && Map.member (Plan.alignmentPlanId plan, local) state.planAcceptances
            && any (\member -> alignmentMemberHerald member == local && alignmentMemberStoreIncarnation member == store) (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)))

unsettledAlignmentPlans :: State -> [Plan.AlignmentPlan]
unsettledAlignmentPlans state =
  [ plan
  | plan <- liveCurrentAlignmentPlans state,
    case state.transferLocalHerald of
      Nothing -> True
      Just local ->
        Map.notMember (Plan.alignmentPlanId plan, local) state.planAcceptances
          || any
            ( \generation ->
                any
                  ( \member ->
                      alignmentMemberHerald member == local
                        && not (currentAlignmentMemberReady local (alignmentGenerationId generation) (alignmentMemberStoreIncarnation member) state)
                  )
                  (NonEmpty.toList (alignmentCutExactMembers (alignmentGenerationCut generation)))
            )
            (planGenerations plan)
  ]
