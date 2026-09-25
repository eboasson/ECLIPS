{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Pure owner of structural application progress and the retained topology-cut
-- chain.
--
-- The current routing graph remains a separate live owner. A prepared
-- structural application commits both successors together, making it impossible
-- for a caller to advance the version vector without exact-applying the
-- Increment-3 graph patch.
module Eclips.Herald.Graph.Progress
  ( StructuralProgressState,
    StructuralProgressProblem (..),
    initialStructuralProgressState,
    initialJoiningStructuralProgressState,
    StructuralProgressBase,
    captureStructuralProgressBase,
    structuralProgressBaseCarrierBase,
    StructuralProgressBaseEvidence,
    StructuralProgressBaseClaims,
    StructuralProgressBaseAdmissionContext,
    AdmittedStructuralProgressBaseEvidence,
    StructuralProgressBaseProblem (..),
    captureStructuralProgressBaseEvidence,
    encodeStructuralProgressBaseEvidence,
    decodeStructuralProgressBaseEvidence,
    structuralProgressBaseAdmissionContext,
    admitStructuralProgressBaseEvidence,
    admittedStructuralProgressMembership,
    admittedStructuralProgressMembershipHistory,
    admittedStructuralProgressRetirementBases,
    admittedStructuralProgressAdmissions,
    prepareStructuralProgressBaseImportFromEvidence,
    validateStructuralProgressOriginalOccurrences,
    StructuralProgressCertificateEvidence,
    StructuralProgressCertificateClaims,
    StructuralProgressCertificateProblem (..),
    captureStructuralProgressCertificates,
    encodeStructuralProgressCertificates,
    decodeStructuralProgressCertificates,
    prepareStructuralProgressBaseImportWithCertificates,
    PreparedStructuralProgressBaseImport,
    prepareStructuralProgressBaseImport,
    commitStructuralProgressBaseImport,
    takePreparationReadinessChange,
    clearPreparationReadinessChange,
    takeAlignmentPlanChange,
    notifyAlignmentMembershipChange,
    clearAlignmentPlanChange,
    structuralProgressIsJoiningObserver,
    structuralProgressLocalHerald,
    replaceStructuralProgressLocalHeraldForInvariantTest,
    structuralProgressMembers,
    structuralProgressMembershipGenerationId,
    structuralProgressMembershipHistory,
    StructuralDescendantSettlement,
    structuralDescendantSettlement,
    structuralDescendantSettlementLineage,
    carryDescendantStampedSurvivorOccurrence,
    structuralProgressMemberSetDigest,
    structuralProgressReconciliation,
    restoreStructuralLocalStoreForInvariantTest,
    structuralAppliedVector,
    structuralAppliedControlPrefix,
    structuralLocalReport,
    structuralReportEntries,
    structuralStableFrontier,
    structuralGenesisCutId,
    structuralLastInstalledCutId,
    structuralLastInstalledCutControlPrefix,
    structuralPublicationControlPrerequisite,
    structuralInstalledCutEntries,
    structuralInstalledCutIds,
    structuralInstalledEstablishedChain,
    structuralInstalledSuccessorBases,
    structuralOpenCut,
    structuralOpenLocalAcceptance,
    structuralOpenEstablished,
    structuralPendingAnnounce,
    structuralPendingEstablished,
    structuralPendingEstablishedChain,
    PreparedMembershipSuccessorBaseInstallation,
    prepareMembershipSuccessorBaseInstallation,
    preparedMembershipSuccessorBaseCut,
    commitMembershipSuccessorBaseInstallation,
    structuralSuccessorBaseInstalled,
    PreparedMembershipAdmissionBaseInstallation,
    PreparedHeraldJoinBaseCandidate,
    prepareHeraldJoinBaseCandidate,
    preparedHeraldJoinBaseCandidateDigest,
    prepareMembershipAdmissionBaseInstallation,
    preparedMembershipAdmissionBaseCut,
    commitMembershipAdmissionBaseInstallation,
    StructuralProgressInvariantProblem (..),
    validateStructuralProgressState,
    AppliedStructuralOccurrence,
    appliedStructuralOccurrenceStamp,
    appliedStructuralOccurrenceSourceTopologyPrerequisite,
    appliedStructuralOccurrenceCarrierSort,
    appliedStructuralOccurrenceControlPrerequisite,
    lookupAppliedStructuralOccurrence,
    StructuralControlAdvance,
    structuralControlAdvanceOccurrence,
    structuralControlAdvancePredecessor,
    structuralControlAdvanceSuccessor,
    PreparedStructuralApplication,
    prepareStructuralApplication,
    prepareCarriedStructuralApplication,
    preparedStructuralControlAdvance,
    commitStructuralApplication,
    PreparedStructuralProjectionRefresh,
    prepareStructuralProjectionRefresh,
    commitStructuralProjectionRefresh,
    StructuralControlProgress (..),
    PreparedStructuralControlProgress,
    prepareStructuralControlProgress,
    commitStructuralControlProgress,
    StructuralReportClassification (..),
    PreparedStructuralReport,
    prepareStructuralReport,
    commitStructuralReport,
    DynamicVertexInstallation,
    dynamicVertexInstallationVertex,
    dynamicVertexInstallationOccurrence,
    dynamicVertexInstallationPublication,
    dynamicVertexInstallationController,
    dynamicVertexInstallationSort,
    dynamicVertexInstallationEarliestCut,
    dynamicVertexInstallationCurrentCut,
    PreparedStructuralQueries,
    prepareStructuralQueries,
    preparedStructuralProgress,
    lookupPreparedDynamicNabla,
    lookupPreparedDynamicDelta,
    lookupPreparedCurrentNablaProjection,
    lookupInstalledDynamicNabla,
    lookupInstalledDynamicDelta,
    structuralNablaProjectionAtCoordinate,
    installedCutCoveringCause,
    installedCutCoversCause,
    currentInstalledCutCoveringCause,
    installedCutCoveringOccurrence,
    currentInstalledCutCoveringOccurrence,
    InstalledTopologyCut,
    installedTopologyCutEstablished,
    installedTopologyCutOwnAcceptance,
    installedTopologyCutOwnAcknowledgement,
    installedTopologyCutJoinSealAdmission,
    installedTopologyCutNewlyCoveredOccurrences,
    installedTopologyCutCut,
    lookupInstalledTopologyCut,
    structuralProjectionAtInstalledCut,
    structuralTopologyOccurrenceDigestAt,
    structuralAdmissionAnchorClaim,
    topologyCutNewlyCoveredOccurrences,
    TopologyControlCheckpoint,
    topologyControlCheckpoint,
    topologyControlCheckpointWithConsequence,
    topologyJoinSealCheckpoint,
    topologyControlCheckpointIndex,
    topologyControlCheckpointAffectsTopology,
    topologyControlCheckpointCanonicalBytes,
    TopologyCutProposalPreparation (..),
    PreparedTopologyCutProposal,
    validateCandidateOccurrences,
    validateCandidateOccurrencesExhaustive,
    prepareTopologyCutProposal,
    TopologyProjectionWork (..),
    prepareTopologyCutProposalCached,
    preparedTopologyCutProposalAnnounce,
    preparedTopologyCutProposalLocalAcceptance,
    commitTopologyCutProposal,
    TopologyCutAnnounceClassification (..),
    PreparedTopologyCutAnnounce,
    prepareTopologyCutAnnounce,
    preparedTopologyCutAnnounceClassification,
    commitTopologyCutAnnounce,
    TopologyCutAcceptanceClassification (..),
    PreparedTopologyCutAcceptance,
    prepareTopologyCutAcceptance,
    preparedTopologyCutAcceptanceClassification,
    commitTopologyCutAcceptance,
    PreparedTopologyCutEstablishment,
    prepareTopologyCutEstablishment,
    preparedTopologyCutEstablished,
    preparedTopologyCutEstablishedAck,
    commitTopologyCutEstablishment,
    TopologyCutEstablishedClassification (..),
    PreparedTopologyCutEstablished,
    prepareTopologyCutEstablished,
    preparedTopologyCutEstablishedClassification,
    commitTopologyCutEstablished,
    TopologyCutAckClassification (..),
    PreparedTopologyCutAck,
    prepareTopologyCutEstablishedAck,
    preparedTopologyCutAckClassification,
    commitTopologyCutEstablishedAck,
  )
where

import Control.Monad (foldM, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize qualified as Codec
import Data.Serialize.Put qualified as Serialize
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Graph (VertexId (..))
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    GlobalObjectId,
    HeraldEpoch,
    NablaId,
    PublicationId,
    StructuralOccurrenceId,
    SystemId,
    TopologyCutId,
    controlIndex,
    globalObjectIdFromDeltaId,
    globalObjectIdFromNablaId,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
    structuralSequenceWord64,
  )
import Eclips.Domain.Identity qualified as Identity
import Eclips.Domain.MemberSet qualified as MemberSet
import Eclips.Domain.Membership
  ( HeraldAdmissionId,
    HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    HeraldMembershipLineage,
    decodeHeraldAdmissionIdCanonicalBytes,
    heraldAdmissionControlIndex,
    heraldAdmissionIdCanonicalBytes,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationChangeControlIndex,
    heraldMembershipGenerationId,
    heraldMembershipGenerationPredecessor,
    heraldMembershipHistory,
    heraldMembershipLineage,
    heraldMembershipLineageGenerations,
    heraldMembershipLineageOrigin,
    heraldMembershipLineageTarget,
  )
import Eclips.Domain.Membership qualified as Membership
import Eclips.Domain.Publication (checkedPublicationId)
import Eclips.Domain.Sort.Descriptor qualified as Descriptor
import Eclips.Domain.Startup
  ( InitialProjectionDigest,
    initialProjectionDigestBytes,
  )
import Eclips.Domain.Startup qualified as Startup
import Eclips.Domain.Structural
  ( StructuralPrefix,
    StructuralVectorProblem,
    StructuralVersionVector,
    emptyStructuralVersionVector,
    mkStructuralVersionVector,
    nextAfterStructuralPrefix,
    structuralPrefixSequence,
    structuralPrefixThrough,
    structuralVersionVectorComponent,
    structuralVersionVectorCovers,
    structuralVersionVectorEntries,
    structuralVersionVectorMemberSetDigest,
    structuralVersionVectorMembershipGenerationId,
  )
import Eclips.Domain.Structural qualified as Structural
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    StructuralConsequenceCauseView (..),
    structuralConsequenceCauseView,
    structuralOccurrenceCause,
  )
import Eclips.Domain.Topology
  ( MemberSetDigest,
    TopologyCut,
    TopologyFrontier,
    TopologyOccurrenceDigest,
    TopologyPredecessorView (..),
    TopologyShapeProblem,
    deriveGenesisTopologyCutId,
    deriveTopologyCutId,
    deriveTopologyOccurrenceDigest,
    sameGenerationPredecessor,
    topologyCut,
    topologyCutFrontier,
    topologyCutOccurrenceDigest,
    topologyCutPredecessor,
    topologyFrontier,
    topologyFrontierAppliedControlPrefix,
    topologyFrontierMemberSetDigest,
    topologyFrontierStructuralVersionVector,
    topologyPredecessorCutId,
    topologyPredecessorView,
  )
import Eclips.Domain.Topology qualified as Topology
import Eclips.Herald.Graph.Protocol
  ( StructuralAppliedReport,
    TopologyCutAcceptance,
    TopologyCutAnnounce,
    TopologyCutEstablished,
    TopologyCutEstablishedAck,
    structuralAppliedReport,
    structuralAppliedReportControlPrefix,
    structuralAppliedReportMemberSetDigest,
    structuralAppliedReportMembershipGenerationId,
    structuralAppliedReportReporter,
    structuralAppliedReportVersionVector,
    topologyCutAcceptance,
    topologyCutAcceptanceId,
    topologyCutAcceptanceReport,
    topologyCutAcceptanceReporter,
    topologyCutAnnounce,
    topologyCutAnnounceAnnouncer,
    topologyCutAnnounceCut,
    topologyCutAnnounceId,
    topologyCutEstablished,
    topologyCutEstablishedAcceptances,
    topologyCutEstablishedAck,
    topologyCutEstablishedAckId,
    topologyCutEstablishedAckMembershipGenerationId,
    topologyCutEstablishedAckReporter,
    topologyCutEstablishedCut,
    topologyCutEstablishedId,
  )
import Eclips.Herald.Graph.Protocol qualified as Protocol
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource
  ( CarriedStampedSurvivor,
    SuccessorStructuralBase,
    TerminalSourceProblem,
    allocateSuccessorStructuralOccurrence,
    carriedStampedSurvivorFromAdvance,
    carriedStampedSurvivorPredecessor,
    carriedStampedSurvivorStamp,
    carriedStampedSurvivorVector,
    establishedSuccessorStructuralBase,
    projectSurvivorLiveAheadVector,
    successorStructuralBaseCutId,
    successorStructuralBaseEstablishedUnion,
    successorStructuralBaseGenerationId,
    successorStructuralBaseLineage,
    terminalSourceUnionAcceptanceReporter,
    terminalSourceUnionEstablishedAcceptances,
    terminalSourceUnionEstablishedUnion,
    terminalSourceUnionSuccessorInitialVector,
    terminalSourceUnionTerminalControlPrefix,
    terminalSourceUnionTerminalPredecessorVector,
  )
import Eclips.Herald.Graph.TerminalSource qualified as Terminal
import Eclips.Herald.Internal.Derived (Derived, derive, derivedValue)
import Eclips.Herald.Join.Bootstrap qualified as JoinBootstrap
import Eclips.Herald.PeerPublication
  ( StructuralOccurrenceStamp,
    structuralOccurrenceStampCarrierRole,
    structuralOccurrenceStampOccurrence,
    structuralOccurrenceStampPredecessor,
    structuralOccurrenceStampPublication,
  )
import Eclips.Herald.PeerPublication qualified as Peer
import Eclips.Herald.Structural.Debt (SortOccurrence)
import Eclips.Herald.Structural.Debt qualified as Debt
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Oracle.Admission qualified as Admission

data AppliedStructuralOccurrence = AppliedStructuralOccurrence
  { stamp :: StructuralOccurrenceStamp,
    sourceTopologyPrerequisite :: TopologyCutId,
    carrierSort :: SortOccurrence,
    controlPrerequisite :: ControlIndex
  }
  deriving stock (Eq, Show)

appliedStructuralOccurrenceStamp ::
  AppliedStructuralOccurrence -> StructuralOccurrenceStamp
appliedStructuralOccurrenceStamp occurrence = occurrence.stamp

appliedStructuralOccurrenceSourceTopologyPrerequisite ::
  AppliedStructuralOccurrence -> TopologyCutId
appliedStructuralOccurrenceSourceTopologyPrerequisite occurrence =
  occurrence.sourceTopologyPrerequisite

appliedStructuralOccurrenceCarrierSort ::
  AppliedStructuralOccurrence -> SortOccurrence
appliedStructuralOccurrenceCarrierSort occurrence = occurrence.carrierSort

appliedStructuralOccurrenceControlPrerequisite ::
  AppliedStructuralOccurrence -> ControlIndex
appliedStructuralOccurrenceControlPrerequisite occurrence =
  occurrence.controlPrerequisite

data InstalledTopologyCut = InstalledTopologyCut
  { installedEstablished :: TopologyCutEstablished,
    ownAcceptance :: Maybe TopologyCutAcceptance,
    ownAcknowledgement :: Maybe TopologyCutEstablishedAck,
    newlyCoveredOccurrences :: [StructuralOccurrenceId],
    installedAcknowledgements :: Map HeraldEpoch TopologyCutEstablishedAck,
    membershipSuccessorBase :: Maybe SuccessorStructuralBase,
    membershipAdmissionBase :: Maybe (Topology.HeraldJoinBaseRecipe, Admission.HeraldAdmissionRecord),
    joinSealAdmission :: Maybe HeraldAdmissionId
  }
  deriving stock (Eq, Show)

installedTopologyCutEstablished :: InstalledTopologyCut -> TopologyCutEstablished
installedTopologyCutEstablished installed = installed.installedEstablished

installedTopologyCutOwnAcceptance ::
  InstalledTopologyCut -> Maybe TopologyCutAcceptance
installedTopologyCutOwnAcceptance installed = installed.ownAcceptance

installedTopologyCutOwnAcknowledgement ::
  InstalledTopologyCut -> Maybe TopologyCutEstablishedAck
installedTopologyCutOwnAcknowledgement installed = installed.ownAcknowledgement

installedTopologyCutJoinSealAdmission :: InstalledTopologyCut -> Maybe HeraldAdmissionId
installedTopologyCutJoinSealAdmission installed = installed.joinSealAdmission

installedTopologyCutNewlyCoveredOccurrences ::
  InstalledTopologyCut -> [StructuralOccurrenceId]
installedTopologyCutNewlyCoveredOccurrences installed =
  installed.newlyCoveredOccurrences

installedTopologyCutCut :: InstalledTopologyCut -> TopologyCut
installedTopologyCutCut =
  topologyCutEstablishedCut . installedTopologyCutEstablished

data OpenTopologyCut = OpenTopologyCut
  { announce :: TopologyCutAnnounce,
    acceptances :: Map HeraldEpoch TopologyCutAcceptance,
    openEstablished :: Maybe TopologyCutEstablished,
    openAcknowledgements :: Map HeraldEpoch TopologyCutEstablishedAck,
    joinSealAdmission :: Maybe HeraldAdmissionId
  }
  deriving stock (Eq, Show)

-- | Exact evidence for an ordinary predecessor-generation cut that was open
-- when an atomic membership-successor base superseded it.  Retaining the
-- checked announce keeps the cut identity derivable; naming the superseding
-- base lets the invariant and late-message admission recover the terminal
-- coordinate that abandoned the attempt without discarding its retained work.
data SupersededOpenCut = SupersededOpenCut
  { announce :: TopologyCutAnnounce,
    supersededBy :: TopologyCutId
  }
  deriving stock (Eq, Show)

-- | Append-only coverage index. For each monotonically advanced
-- prefix, retain the earliest installed cut which reaches it. A private source
-- map deliberately retains retired epochs, unlike a canonical live-member
-- StructuralVersionVector. Installed order makes arbitrary-cut queries a
-- comparison with the earliest covering cut, without walking predecessors.
data InstalledCoverageIndex
  = InstalledCoverageIndex
      !(Map HeraldEpoch (Map StructuralPrefix TopologyCutId))
      !(Map ControlIndex TopologyCutId)
      !(Map TopologyCutId Int)
  deriving stock (Eq, Show)

emptyInstalledCoverageIndex :: InstalledCoverageIndex
emptyInstalledCoverageIndex = InstalledCoverageIndex Map.empty Map.empty Map.empty

extendInstalledCoverageIndex ::
  TopologyCutId -> TopologyCut -> InstalledCoverageIndex -> InstalledCoverageIndex
extendInstalledCoverageIndex cutId cut (InstalledCoverageIndex bySource byControl positions) =
  InstalledCoverageIndex
    (Map.foldlWithKey' extendSource bySource directlyCoveredPrefixes)
    (retainCoverageAdvance (topologyFrontierAppliedControlPrefix frontier) cutId byControl)
    (Map.insert cutId (Map.size positions) positions)
  where
    frontier = topologyCutFrontier cut
    directlyCoveredPrefixes =
      Map.fromListWith
        max
        ( structuralVersionVectorEntries (topologyFrontierStructuralVersionVector frontier)
            <> case topologyPredecessorView (topologyCutPredecessor cut) of
              MembershipSuccessorPredecessorView _ _ _ terminalVector _ _ ->
                structuralVersionVectorEntries terminalVector
              _ -> []
        )
    extendSource retained source through =
      Map.alter
        (Just . retainCoverageAdvance through cutId . maybe Map.empty id)
        source
        retained

retainCoverageAdvance ::
  (Ord prefix) => prefix -> TopologyCutId -> Map prefix TopologyCutId -> Map prefix TopologyCutId
retainCoverageAdvance through cutId retained =
  case Map.lookupMax retained of
    Just (alreadyCovered, _) | through <= alreadyCovered -> retained
    _ -> Map.insert through cutId retained

indexedInstalledCutCoveringCause ::
  StructuralConsequenceCause -> InstalledCoverageIndex -> Maybe TopologyCutId
indexedInstalledCutCoveringCause cause (InstalledCoverageIndex bySource byControl _) =
  case structuralConsequenceCauseView cause of
    StructuralOccurrenceCauseView occurrence -> do
      prefixes <- Map.lookup (structuralOccurrenceSourceHeraldEpoch occurrence) bySource
      snd <$> Map.lookupGE (structuralPrefixThrough (structuralOccurrenceSourceSequence occurrence)) prefixes
    LabelReleaseCauseView _ index -> snd <$> Map.lookupGE index byControl
    ProcessEndCauseView _ index -> snd <$> Map.lookupGE index byControl
    PredefinedDisappearanceCauseView _ index -> snd <$> Map.lookupGE index byControl

indexedInstalledCutCoversCause ::
  TopologyCutId -> StructuralConsequenceCause -> InstalledCoverageIndex -> Bool
indexedInstalledCutCoversCause cutId cause index@(InstalledCoverageIndex _ _ positions) =
  case (Map.lookup cutId positions, indexedInstalledCutCoveringCause cause index >>= (`Map.lookup` positions)) of
    (Just target, Just earliest) -> earliest <= target
    _ -> False

-- The exhaustive owner audit verifies that every installation updated the
-- derived index. Runtime queries never reconstruct this reference value.
rebuildInstalledCoverageIndex :: StructuralProgressState -> InstalledCoverageIndex
rebuildInstalledCoverageIndex state =
  foldl' install emptyInstalledCoverageIndex state.installedChain
  where
    install retained cutId =
      case Map.lookup cutId state.installedCuts of
        Nothing -> retained
        Just installed -> extendInstalledCoverageIndex cutId (installedTopologyCutCut installed) retained

data StructuralProgressState = StructuralProgressState
  { localHerald :: HeraldEpoch,
    joiningObserver :: Bool,
    currentMembership :: HeraldMembershipGeneration,
    membershipHistory :: Map HeraldMembershipGenerationId HeraldMembershipGeneration,
    initialProjection :: InitialProjectionDigest,
    reconciliation :: Reconciliation.StructuralAppliedState,
    appliedVector :: StructuralVersionVector,
    appliedControlPrefix :: ControlIndex,
    currentNablaProjections :: Derived (Either StructuralProgressProblem Reconciliation.PreparedNablaProjections),
    proposalProjection :: !(Derived (Maybe CachedTopologyProjection)),
    history :: Map StructuralOccurrenceId AppliedStructuralOccurrence,
    reports :: Map HeraldEpoch StructuralAppliedReport,
    genesisCutId :: TopologyCutId,
    lastInstalledCutId :: TopologyCutId,
    installedCuts :: Map TopologyCutId InstalledTopologyCut,
    installedChain :: [TopologyCutId],
    installedCoverageIndex :: InstalledCoverageIndex,
    supersededOpenCuts :: Map TopologyCutId SupersededOpenCut,
    openCut :: Maybe OpenTopologyCut,
    pendingAnnounce :: Maybe TopologyCutAnnounce,
    pendingEstablished :: [TopologyCutEstablished],
    preparationReadinessChanged :: !Bool,
    alignmentPlanChanged :: !Bool
  }
  deriving stock (Eq, Show)

-- | Immutable applied evidence paired with its carrier/control history. Reports,
-- acknowledgements and in-flight rounds belong to the source participant and
-- are deliberately absent. Strict portable records detach every field from the
-- source owner before the capture is retained.
data StructuralProgressBase
  = StructuralProgressBase
      !HeraldMembershipGeneration
      !(Map HeraldMembershipGenerationId HeraldMembershipGeneration)
      !InitialProjectionDigest
      !TopologyCutId
      !Reconciliation.StructuralCarrierBase
      !StructuralVersionVector
      !ControlIndex
      !(Map StructuralOccurrenceId StructuralProgressOccurrenceBase)
      !(Map Int StructuralProgressCutBase)
  deriving stock (Eq, Show)

data StructuralProgressOccurrenceBase
  = StructuralProgressOccurrenceBase
      !StructuralOccurrenceStamp
      !TopologyCutId
      !SortOccurrence
      !ControlIndex
  deriving stock (Eq, Show)

data StructuralProgressCutBase
  = StructuralProgressCutBase
      !TopologyCutEstablished
      !(Maybe SuccessorStructuralBase)
      !(Maybe StructuralProgressAdmissionBase)
      !(Maybe HeraldAdmissionId)
  deriving stock (Eq, Show)

data StructuralProgressAdmissionBase
  = StructuralProgressAdmissionBase
      !Topology.HeraldJoinBaseRecipe
      !Admission.HeraldAdmissionRecord
  deriving stock (Eq, Show)

-- | The ordered, portable certificate portion of an admitted Progress owner.
-- Participant reports outside these certificates, acknowledgements and open
-- rounds are deliberately absent. This leaf is independent of carrier bytes.
newtype StructuralProgressCertificateEvidence
  = StructuralProgressCertificateEvidence (Map Int StructuralProgressCutBase)
  deriving stock (Eq, Show)

-- | Canonical shape is not installed-cut authority. These claims must still
-- be admitted against the paired membership/carrier base and receiver genesis.
newtype StructuralProgressCertificateClaims
  = StructuralProgressCertificateClaims (Map Int StructuralProgressCutClaim)
  deriving stock (Eq, Show)

data StructuralProgressCutClaim
  = StructuralProgressCutClaim
      !TopologyCutEstablished
      !(Maybe Terminal.TerminalSourceUnionEstablished)
      !(Maybe StructuralProgressAdmissionBase)
      !(Maybe HeraldAdmissionId)
  deriving stock (Eq, Show)

data StructuralProgressCertificateProblem
  = StructuralProgressCertificateMalformed
  | StructuralProgressCertificateNotCanonical
  | StructuralProgressCertificateMembershipMismatch
  | StructuralProgressCertificateKindMismatch TopologyCutId
  | StructuralProgressCertificateMetadataMismatch TopologyCutId
  | StructuralProgressCertificateAnchorMismatch
  | StructuralProgressCertificateSuccessorProblem TerminalSourceProblem
  | StructuralProgressCertificateImportProblem StructuralProgressProblem
  deriving stock (Eq, Show)

type RawStructuralProgressCut =
  (ByteString, Maybe ByteString, Maybe (ByteString, ByteString), Maybe ByteString)

captureStructuralProgressCertificates :: StructuralProgressState -> StructuralProgressCertificateEvidence
captureStructuralProgressCertificates state =
  StructuralProgressCertificateEvidence
    (Map.fromList (zip [0 ..] [captureProgressCut (state.installedCuts Map.! cut) | cut <- state.installedChain]))

captureProgressCut :: InstalledTopologyCut -> StructuralProgressCutBase
captureProgressCut installed =
  let successor = installed.membershipSuccessorBase
      admission = case installed.membershipAdmissionBase of
        Nothing -> Nothing
        Just (recipe, record) ->
          let retained = StructuralProgressAdmissionBase recipe record
           in retained `seq` Just retained
      joinSeal = installed.joinSealAdmission
   in forceMaybe successor
        `seq` forceMaybe admission
        `seq` forceMaybe joinSeal
        `seq` StructuralProgressCutBase installed.installedEstablished successor admission joinSeal
  where
    forceMaybe Nothing = ()
    forceMaybe (Just value) = value `seq` ()

encodeStructuralProgressCertificates :: StructuralProgressCertificateEvidence -> ByteString
encodeStructuralProgressCertificates (StructuralProgressCertificateEvidence cuts) =
  encodeProgressCertificateClaims
    ( StructuralProgressCertificateClaims
        ( Map.map
            (\(StructuralProgressCutBase established successor admission seal) -> StructuralProgressCutClaim established (successorStructuralBaseEstablishedUnion <$> successor) admission seal)
            cuts
        )
    )

encodeProgressCertificateClaims :: StructuralProgressCertificateClaims -> ByteString
encodeProgressCertificateClaims (StructuralProgressCertificateClaims cuts) =
  Codec.encode ("ECLIPS-STRUCTURAL-PROGRESS-CERTIFICATES" :: ByteString, map rawCut (Map.elems cuts))
  where
    rawCut (StructuralProgressCutClaim established successor admission seal) =
      ( Protocol.topologyCutEstablishedCanonicalBytes established,
        Terminal.terminalSourceUnionEstablishedCanonicalBytes <$> successor,
        (\(StructuralProgressAdmissionBase recipe record) -> (Topology.heraldJoinBaseRecipeCanonicalBytes recipe, Admission.encodeAdmissionRecord record)) <$> admission,
        heraldAdmissionIdCanonicalBytes <$> seal
      )

-- | Decode each nested frame through its nominal codec. Copying the frames
-- prevents one surviving certificate from retaining the entire input buffer.
-- Membership, chain continuity and frontier coverage are checked at import.
decodeStructuralProgressCertificates :: ByteString -> Either StructuralProgressCertificateProblem StructuralProgressCertificateClaims
decodeStructuralProgressCertificates bytes = do
  (domain, supplied) <- shape (Codec.decode bytes :: Either String (ByteString, [RawStructuralProgressCut]))
  unless (domain == "ECLIPS-STRUCTURAL-PROGRESS-CERTIFICATES") (Left StructuralProgressCertificateMalformed)
  cuts <- traverse decodeCut supplied
  let !claims = StructuralProgressCertificateClaims (Map.fromList (zip [0 ..] cuts))
  unless (encodeProgressCertificateClaims claims == bytes) (Left StructuralProgressCertificateNotCanonical)
  pure claims
  where
    shape = either (const (Left StructuralProgressCertificateMalformed)) Right
    decodeCut (established, successor, admission, seal) =
      StructuralProgressCutClaim
        <$> shape (Protocol.decodeTopologyCutEstablishedCanonicalBytes (ByteString.copy established))
        <*> traverse (shape . Terminal.decodeTerminalSourceUnionEstablishedCanonicalBytes . ByteString.copy) successor
        <*> traverse decodeAdmission admission
        <*> traverse (shape . decodeHeraldAdmissionIdCanonicalBytes . ByteString.copy) seal
    decodeAdmission (recipe, record) =
      StructuralProgressAdmissionBase
        <$> shape (Topology.decodeHeraldJoinBaseRecipeCanonicalBytes (ByteString.copy recipe))
        <*> shape (Admission.decodeAdmissionRecord (ByteString.copy record))

-- | Admit decoded certificate claims without exposing a raw constructor for a
-- checked carrier/progress base. The still-in-memory base supplies immutable
-- membership, applied occurrences and the required final cut; the ordinary
-- import audit checks the complete reconstructed chain and source-cut links.
-- Unhashed admission/seal metadata must match the source's admitted metadata.
-- Full wire composition must replace that source dependence with checked Oracle
-- proof admission, together with the separate byte-to-carrier boundary.
prepareStructuralProgressBaseImportWithCertificates ::
  StructuralProgressCertificateClaims ->
  StructuralProgressBase ->
  Reconciliation.StructuralAppliedState ->
  StructuralProgressState ->
  Either StructuralProgressCertificateProblem PreparedStructuralProgressBaseImport
prepareStructuralProgressBaseImportWithCertificates
  (StructuralProgressCertificateClaims claims)
  (StructuralProgressBase membership memberships initialProjection genesis carriers vector control occurrences originalCuts)
  reconciliation
  receiver = do
    generations <- maybe (Left StructuralProgressCertificateMembershipMismatch) Right (NonEmpty.nonEmpty (sortOn (maybe (controlIndex 0) id . heraldMembershipGenerationChangeControlIndex) (Map.elems memberships)))
    history <- mapLeft (const StructuralProgressCertificateMembershipMismatch) (heraldMembershipHistory generations)
    cuts <- traverse (admitCut history) claims
    unless (lastCut cuts == lastCut originalCuts) (Left StructuralProgressCertificateAnchorMismatch)
    mapLeft
      StructuralProgressCertificateImportProblem
      ( prepareStructuralProgressBaseImport
          (StructuralProgressBase membership memberships initialProjection genesis carriers vector control occurrences cuts)
          reconciliation
          receiver
      )
    where
      admittedMetadata =
        Map.fromList
          [ (topologyCutEstablishedId established, (successor, admission, seal))
          | StructuralProgressCutBase established successor admission seal <- Map.elems originalCuts
          ]
      lastCut cuts = case Map.lookupMax cuts of
        Nothing -> genesis
        Just (_, StructuralProgressCutBase established _ _ _) -> topologyCutEstablishedId established
      admitCut history (StructuralProgressCutClaim established successor admission seal) = do
        let cut = topologyCutEstablishedCut established
            mismatch = StructuralProgressCertificateKindMismatch (topologyCutEstablishedId established)
        base <- case (topologyPredecessorView (topologyCutPredecessor cut), successor, admission, seal) of
          (SameGenerationPredecessorView {}, Nothing, Nothing, _) -> do
            unless
              (maybe True ((<= topologyFrontierAppliedControlPrefix (topologyCutFrontier cut)) . heraldAdmissionControlIndex) seal)
              (Left mismatch)
            pure Nothing
          (MembershipSuccessorPredecessorView _ origin target _ _ _, Just union, Nothing, Nothing) -> do
            lineage <- mapLeft (const StructuralProgressCertificateMembershipMismatch) (heraldMembershipLineage origin target history)
            Just <$> mapLeft StructuralProgressCertificateSuccessorProblem (establishedSuccessorStructuralBase lineage union cut)
          (AdmissionTopologyPredecessorView {}, Nothing, Just _, Nothing) -> pure Nothing
          _ -> Left mismatch
        unless
          (Map.lookup (topologyCutEstablishedId established) admittedMetadata == Just (base, admission, seal))
          (Left (StructuralProgressCertificateMetadataMismatch (topologyCutEstablishedId established)))
        pure (StructuralProgressCutBase established base admission seal)

-- | Progress-owned immutable facts. The paired carrier is deliberately absent
-- from this wire representation; neither nominal decoding nor certificate
-- admission installs a live owner or authorizes a Join source.
newtype StructuralProgressBaseEvidence
  = StructuralProgressBaseEvidence StructuralProgressBaseClaims
  deriving stock (Eq, Show)

data StructuralProgressBaseClaims
  = StructuralProgressBaseClaims
      !HeraldMembershipGeneration
      !Membership.HeraldMembershipHistory
      !InitialProjectionDigest
      !TopologyCutId
      !StructuralVersionVector
      !ControlIndex
      !(Map StructuralOccurrenceId StructuralProgressOccurrenceBase)
      !StructuralProgressCertificateClaims
  deriving stock (Eq, Show)

-- The context contains concrete already-admitted Projection facts and the
-- receiver's immutable genesis. It cannot retain either whole owner State.
data StructuralProgressBaseAdmissionContext
  = StructuralProgressBaseAdmissionContext
      !Membership.HeraldMembershipHistory
      !(Map HeraldAdmissionId Admission.HeraldAdmissionRecord)
      !ControlIndex
      !TopologyCutId
      !HeraldMembershipGeneration
      !InitialProjectionDigest
      !TopologyCutId
  deriving stock (Eq, Show)

data AdmittedStructuralProgressBaseEvidence
  = AdmittedStructuralProgressBaseEvidence
      !StructuralProgressBaseClaims
      !(Map Int StructuralProgressCutBase)
      ![(HeraldMembershipLineage, Topology.HeraldJoinBaseRecipe, Admission.HeraldAdmissionRecord)]
  deriving stock (Eq, Show)

data StructuralProgressBaseProblem
  = StructuralProgressBaseMalformed String
  | StructuralProgressBaseInconsistent String
  | StructuralProgressBaseCertificateProblem StructuralProgressCertificateProblem
  | StructuralProgressBaseImportProblem StructuralProgressProblem
  deriving stock (Eq, Show)

type RawProgressVector = (ByteString, ByteString, [(ByteString, Word64)])
type RawProgressStamp =
  ((ByteString, Word64), RawProgressVector, (ByteString, ByteString, ByteString, Word64), ByteString, Word8)
type RawProgressOccurrence = (RawProgressStamp, ByteString, (ByteString, ByteString), Word64)
type RawProgressBase =
  (ByteString, (ByteString, ByteString, ByteString, ByteString), (RawProgressVector, Word64), [RawProgressOccurrence], ByteString)

captureStructuralProgressBaseEvidence :: StructuralProgressBase -> StructuralProgressBaseEvidence
captureStructuralProgressBaseEvidence (StructuralProgressBase membership memberships initialProjection genesis _ vector control occurrences cuts) =
  StructuralProgressBaseEvidence
    (StructuralProgressBaseClaims membership history initialProjection genesis vector control occurrences claims)
  where
    history = case heraldMembershipHistory (NonEmpty.fromList (sortOn generationCoordinate (Map.elems memberships))) of
      Right checked -> checked
      Left problem -> error ("checked structural progress membership history: " <> show problem)
    claims = StructuralProgressCertificateClaims (Map.map cutClaim cuts)
    cutClaim (StructuralProgressCutBase established successor admission seal) =
      StructuralProgressCutClaim established (successorStructuralBaseEstablishedUnion <$> successor) admission seal

encodeStructuralProgressBaseEvidence :: StructuralProgressBaseEvidence -> ByteString
encodeStructuralProgressBaseEvidence (StructuralProgressBaseEvidence claims) = encodeProgressBaseClaims claims

encodeProgressBaseClaims :: StructuralProgressBaseClaims -> ByteString
encodeProgressBaseClaims (StructuralProgressBaseClaims membership history initialProjection genesis vector control occurrences certificates) =
  Codec.encode transcript
  where
    transcript :: RawProgressBase
    transcript =
      ( "ECLIPS-STRUCTURAL-PROGRESS-BASE",
        ( Membership.heraldMembershipGenerationIdBytes (heraldMembershipGenerationId membership),
          Membership.heraldMembershipHistoryCanonicalBytes history,
          initialProjectionDigestBytes initialProjection,
          Identity.topologyCutIdBytes genesis
        ),
        (rawVector vector, Identity.controlIndexWord64 control),
        map rawOccurrence (Map.elems occurrences),
        encodeProgressCertificateClaims certificates
      )
    rawOccurrence (StructuralProgressOccurrenceBase stamp sourceTopology carrierSort prerequisite) =
      (rawStamp stamp, Identity.topologyCutIdBytes sourceTopology, (Identity.sortIdBytes (Debt.sortOccurrenceSortId carrierSort), Identity.sortDefinitionOccurrenceIdBytes (Debt.sortOccurrenceDefinition carrierSort)), Identity.controlIndexWord64 prerequisite)
    rawStamp stamp =
      let occurrence = structuralOccurrenceStampOccurrence stamp
          publication = structuralOccurrenceStampPublication stamp
       in ( (Identity.heraldEpochBytes (structuralOccurrenceSourceHeraldEpoch occurrence), structuralSequenceWord64 (structuralOccurrenceSourceSequence occurrence)),
            rawVector (structuralOccurrenceStampPredecessor stamp),
            (Identity.nablaIdBytes (Identity.publicationNabla publication), Identity.authorityEpochCanonicalBytes (Identity.publicationAuthorityEpoch publication), Identity.heraldEpochBytes (Identity.publicationSourceHeraldEpoch publication), Identity.nablaSequenceWord64 (Identity.publicationNablaSequence publication)),
            Peer.structuralPublicationDigestBytes (Peer.structuralOccurrenceStampPublicationDigest stamp),
            Descriptor.structuralCarrierRoleTag (structuralOccurrenceStampCarrierRole stamp)
          )
    rawVector value =
      ( Membership.heraldMembershipGenerationIdBytes (structuralVersionVectorMembershipGenerationId value),
        MemberSet.memberSetDigestBytes (structuralVersionVectorMemberSetDigest value),
        [(Identity.heraldEpochBytes herald, maybe 0 structuralSequenceWord64 (structuralPrefixSequence prefix)) | (herald, prefix) <- structuralVersionVectorEntries value]
      )

decodeStructuralProgressBaseEvidence :: ByteString -> Either StructuralProgressBaseProblem StructuralProgressBaseClaims
decodeStructuralProgressBaseEvidence bytes = do
  (domain, (rawMembership, rawHistory, rawInitial, rawGenesis), (rawApplied, rawControl), rawOccurrences, rawCertificates) <- progressBaseClaim (Codec.decode bytes :: Either String RawProgressBase)
  progressBaseRequire (domain == "ECLIPS-STRUCTURAL-PROGRESS-BASE") "wrong Progress base domain"
  history <- progressBaseClaim (Membership.decodeHeraldMembershipHistoryCanonicalBytes (ByteString.copy rawHistory))
  generation <- progressBaseClaim (Membership.mkHeraldMembershipGenerationId (ByteString.copy rawMembership))
  membership <- maybe (Left (StructuralProgressBaseInconsistent "unknown current Progress membership")) Right (Membership.lookupHeraldMembershipGeneration generation history)
  initialProjection <- progressBaseClaim (Startup.mkInitialProjectionDigest (ByteString.copy rawInitial))
  genesis <- progressBaseClaim (Identity.mkTopologyCutId (ByteString.copy rawGenesis))
  vector <- decodeVector rawApplied
  occurrences <- traverse decodeOccurrence rawOccurrences
  certificates <- mapLeft StructuralProgressBaseCertificateProblem (decodeStructuralProgressCertificates rawCertificates)
  let claims = StructuralProgressBaseClaims membership history initialProjection genesis vector (controlIndex rawControl) (Map.fromList occurrences) certificates
  progressBaseRequire (encodeProgressBaseClaims claims == bytes) "noncanonical Progress base"
  pure claims
  where
    decodeOccurrence (rawStamp, rawTopology, (rawSort, rawDefinition), rawControl) = do
      stamp <- decodeStamp rawStamp
      topology <- progressBaseClaim (Identity.mkTopologyCutId (ByteString.copy rawTopology))
      sortId <- progressBaseClaim (Identity.mkSortId (ByteString.copy rawSort))
      definition <- progressBaseClaim (Identity.mkSortDefinitionOccurrenceId (ByteString.copy rawDefinition))
      pure (structuralOccurrenceStampOccurrence stamp, StructuralProgressOccurrenceBase stamp topology (Debt.sortOccurrence sortId definition) (controlIndex rawControl))
    decodeStamp ((rawHerald, rawSequence), rawVector, (rawNabla, rawAuthority, rawSource, rawNablaSequence), rawDigest, rawRole) = do
      herald <- progressBaseClaim (Identity.mkHeraldEpoch (ByteString.copy rawHerald))
      sequenceNumber <- progressBaseClaim (Identity.mkStructuralSequence rawSequence)
      vector <- decodeVector rawVector
      nabla <- progressBaseClaim (Identity.mkNablaId (ByteString.copy rawNabla))
      authority <- progressBaseClaim (Identity.decodeAuthorityEpochCanonicalBytes (ByteString.copy rawAuthority))
      source <- progressBaseClaim (Identity.mkHeraldEpoch (ByteString.copy rawSource))
      let nablaSequence = Identity.nablaSequence rawNablaSequence
      digest <- progressBaseClaim (Peer.mkStructuralPublicationDigest (ByteString.copy rawDigest))
      role <- case rawRole of
        0 -> pure Descriptor.NeutralVertexCarrier
        1 -> pure Descriptor.EdgeCarrier
        2 -> pure Descriptor.NablaCarrier
        3 -> pure Descriptor.DeltaCarrier
        _ -> Left (StructuralProgressBaseMalformed "unsupported structural carrier role")
      progressBaseClaim (Peer.mkStructuralOccurrenceStamp (Identity.structuralOccurrenceId herald sequenceNumber) vector (Identity.publicationId nabla authority source nablaSequence) digest role)
    decodeVector (rawGeneration, rawMembers, rawEntries) = do
      generation <- progressBaseClaim (Membership.mkHeraldMembershipGenerationId (ByteString.copy rawGeneration))
      members <- progressBaseClaim (MemberSet.mkMemberSetDigest (ByteString.copy rawMembers))
      entries <- traverse decodeComponent rawEntries
      nonempty <- maybe (Left (StructuralProgressBaseMalformed "empty structural vector")) Right (NonEmpty.nonEmpty entries)
      progressBaseClaim (Structural.structuralVersionVectorFromClaimedCoordinates generation members nonempty)
    decodeComponent (rawHerald, rawPrefix) = do
      herald <- progressBaseClaim (Identity.mkHeraldEpoch (ByteString.copy rawHerald))
      prefix <- if rawPrefix == 0 then pure Structural.emptyStructuralPrefix else structuralPrefixThrough <$> progressBaseClaim (Identity.mkStructuralSequence rawPrefix)
      pure (herald, prefix)

progressBaseClaim :: (Show problem) => Either problem value -> Either StructuralProgressBaseProblem value
progressBaseClaim = either (Left . StructuralProgressBaseMalformed . show) Right

progressBaseRequire :: Bool -> String -> Either StructuralProgressBaseProblem ()
progressBaseRequire condition detail = unless condition (Left (StructuralProgressBaseInconsistent detail))

generationCoordinate :: HeraldMembershipGeneration -> ControlIndex
generationCoordinate = maybe (controlIndex 0) id . heraldMembershipGenerationChangeControlIndex

-- | Bind concrete facts supplied by the checked Projection and Join envelope.
-- A Progress owner can lag a newer Oracle membership during base establishment;
-- admission resolves its exact historical prefix instead of requiring today's
-- final Projection generation.
structuralProgressBaseAdmissionContext ::
  Membership.HeraldMembershipHistory ->
  [Admission.HeraldAdmissionRecord] ->
  ControlIndex ->
  TopologyCutId ->
  StructuralProgressState ->
  Either StructuralProgressBaseProblem StructuralProgressBaseAdmissionContext
structuralProgressBaseAdmissionContext history records committed requiredFinal receiver = do
  progressBaseRequire
    ( receiver.joiningObserver
        && Map.null receiver.history
        && receiver.appliedControlPrefix == controlIndex 0
        && receiver.appliedVector == emptyStructuralVersionVector receiver.currentMembership
        && Map.null receiver.installedCuts
        && null receiver.installedChain
        && receiver.lastInstalledCutId == receiver.genesisCutId
        && Map.null receiver.reports
        && Map.null receiver.supersededOpenCuts
        && receiver.openCut == Nothing
        && receiver.pendingAnnounce == Nothing
        && null receiver.pendingEstablished
    )
    "Progress admission receiver is not fresh"
  let generations = Membership.heraldMembershipHistoryGenerations history
      admissions = Map.fromList [(Admission.admissionRecordId record, record) | record <- records]
  progressBaseRequire
    ( NonEmpty.head generations == receiver.currentMembership
        && all ((<= committed) . generationCoordinate) generations
        && length records == Map.size admissions
        && all ((<= committed) . Admission.admissionRecordChangedIndex) records
    )
    "Progress admission context differs from genesis/control bound"
  pure (StructuralProgressBaseAdmissionContext history admissions committed requiredFinal receiver.currentMembership receiver.initialProjection receiver.genesisCutId)

admitStructuralProgressBaseEvidence ::
  StructuralProgressBaseAdmissionContext ->
  StructuralProgressBaseClaims ->
  Either StructuralProgressBaseProblem AdmittedStructuralProgressBaseEvidence
admitStructuralProgressBaseEvidence
  (StructuralProgressBaseAdmissionContext oracleHistory records committed requiredFinal receiverMembership receiverInitial receiverGenesis)
  claims@(StructuralProgressBaseClaims membership history initialProjection genesis vector control occurrences (StructuralProgressCertificateClaims certificateClaims)) = do
    lineage <- progressBaseClaim (heraldMembershipLineage (heraldMembershipGenerationId receiverMembership) (heraldMembershipGenerationId membership) oracleHistory)
    progressBaseRequire
      ( Membership.heraldMembershipHistoryGenerations history == heraldMembershipLineageGenerations lineage
          && Membership.heraldMembershipHistoryCurrent history == membership
          && receiverInitial == initialProjection
          && receiverGenesis == genesis
          && control <= committed
          && all ((<= control) . generationCoordinate) (Membership.heraldMembershipHistoryGenerations history)
      )
      "Progress genesis, membership prefix or control mismatch"
    checkVector vector
    progressBaseRequire (structuralVersionVectorMembershipGenerationId vector == heraldMembershipGenerationId membership) "applied vector uses another generation"
    cuts <- traverse admitCut certificateClaims
    let cutIds = [topologyCutEstablishedId established | StructuralProgressCutBase established _ _ _ <- Map.elems cuts]
        final = case reverse cutIds of [] -> genesis; latest : _ -> latest
        installed = Map.fromList [(topologyCutEstablishedId established, topologyCutEstablishedCut established) | StructuralProgressCutBase established _ _ _ <- Map.elems cuts]
    progressBaseRequire (length cutIds == Map.size installed && final == requiredFinal && Map.notMember genesis installed) "Progress installed cut anchor/identity mismatch"
    (_, finalFrontier, _) <- foldM checkChain (genesis, topologyFrontier (emptyStructuralVersionVector receiverMembership) (controlIndex 0), Nothing) (Map.elems cuts)
    progressBaseRequire (structuralVersionVectorMembershipGenerationId (topologyFrontierStructuralVersionVector finalFrontier) == heraldMembershipGenerationId membership) "installed chain does not reach current Progress membership"
    mapM_ (checkOccurrence installed cuts) (Map.toAscList occurrences)
    mapM_ checkComponent (structuralVersionVectorEntries vector)
    admittedAdmissions <- traverse admissionLineage [base | StructuralProgressCutBase _ _ (Just base) _ <- Map.elems cuts]
    pure (AdmittedStructuralProgressBaseEvidence claims cuts admittedAdmissions)
    where
      memberships = Map.fromList [(heraldMembershipGenerationId generation, generation) | generation <- NonEmpty.toList (Membership.heraldMembershipHistoryGenerations history)]
      lookupMembership ident = maybe (Left (StructuralProgressBaseInconsistent "unknown Progress generation")) Right (Map.lookup ident memberships)
      checkVector supplied = do
        generation <- lookupMembership (structuralVersionVectorMembershipGenerationId supplied)
        progressBaseRequire (mkStructuralVersionVector generation (structuralVersionVectorEntries supplied) == Right supplied) "vector shape differs from membership"
      checkFrontier frontier = do
        checkVector (topologyFrontierStructuralVersionVector frontier)
        progressBaseRequire
          ( topologyFrontierAppliedControlPrefix frontier <= control
              && (structuralVersionVectorMembershipGenerationId (topologyFrontierStructuralVersionVector frontier) /= heraldMembershipGenerationId membership || structuralVersionVectorCovers vector (topologyFrontierStructuralVersionVector frontier))
          )
          "installed frontier exceeds applied progress"
      admitCut (StructuralProgressCutClaim established successor admission seal) = do
        let cut = topologyCutEstablishedCut established
            frontier = topologyCutFrontier cut
            generationId = structuralVersionVectorMembershipGenerationId (topologyFrontierStructuralVersionVector frontier)
        generation <- lookupMembership generationId
        checkFrontier frontier
        let acceptances = topologyCutEstablishedAcceptances established
        progressBaseRequire
          ( topologyCutEstablishedId established == deriveTopologyCutId cut
              && map topologyCutAcceptanceReporter acceptances == NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs generation)
          )
          "established cut reporters differ from captured membership"
        mapM_ (checkAcceptance established frontier generationId) acceptances
        successorBase <- case (topologyPredecessorView (topologyCutPredecessor cut), successor, admission, seal) of
          (SameGenerationPredecessorView {}, Nothing, Nothing, marker) -> do
            mapM_ (checkJoinMarker generation frontier) marker
            pure Nothing
          (MembershipSuccessorPredecessorView _ origin target _ _ _, Just union, Nothing, Nothing) -> do
            cutLineage <- progressBaseClaim (heraldMembershipLineage origin target history)
            Just <$> progressBaseClaim (establishedSuccessorStructuralBase cutLineage union cut)
          (AdmissionTopologyPredecessorView {}, Nothing, Just (StructuralProgressAdmissionBase recipe record), Nothing) -> do
            progressBaseRequire
              (Map.lookup (Admission.admissionRecordId record) records == Just record && admissionCertificateMatches recipe record)
              "installed admission certificate differs from Projection"
            pure Nothing
          _ -> Left (StructuralProgressBaseInconsistent "installed cut certificate kind mismatch")
        pure (StructuralProgressCutBase established successorBase admission seal)
      checkAcceptance established frontier generationId acceptance = do
        let report = topologyCutAcceptanceReport acceptance
        checkVector (structuralAppliedReportVersionVector report)
        progressBaseRequire
          ( topologyCutAcceptanceId acceptance == topologyCutEstablishedId established
              && structuralAppliedReportMembershipGenerationId report == generationId
              && reportCoversFrontier report frontier
          )
          "installed acceptance differs from cut"
      checkJoinMarker generation frontier ident = do
        record <- maybe (Left (StructuralProgressBaseInconsistent "unknown Join cut admission")) Right (Map.lookup ident records)
        let at = topologyFrontierAppliedControlPrefix frontier
            terminalBound = case Admission.admissionRecordPhase record of
              Admission.AdmissionActivated index _ -> at < index
              Admission.AdmissionCancelled index -> at < index
              _ -> True
        progressBaseRequire
          (Admission.admissionRecordPredecessor record == generation && heraldAdmissionControlIndex ident <= at && terminalBound)
          "Join cut admission generation/coordinate mismatch"
      checkChain (previous, previousFrontier, previousDigest) (StructuralProgressCutBase established successor admission seal) = do
        let cut = topologyCutEstablishedCut established
            frontier = topologyCutFrontier cut
            digest = topologyCutOccurrenceDigest cut
            previousVector = topologyFrontierStructuralVersionVector previousFrontier
            candidateVector = topologyFrontierStructuralVersionVector frontier
            previousControl = topologyFrontierAppliedControlPrefix previousFrontier
            candidateControl = topologyFrontierAppliedControlPrefix frontier
        progressBaseRequire (topologyPredecessorCutId (topologyCutPredecessor cut) == previous) "installed cuts are not one predecessor chain"
        case topologyPredecessorView (topologyCutPredecessor cut) of
          SameGenerationPredecessorView {} ->
            progressBaseRequire
              ( structuralVersionVectorCovers candidateVector previousVector
                  && candidateControl >= previousControl
                  && (candidateVector /= previousVector || (candidateControl > previousControl && (maybe True (/= digest) previousDigest || seal /= Nothing)))
              )
              "ordinary installed cut does not advance its predecessor"
          MembershipSuccessorPredecessorView _ origin _ _ _ _ -> case successor of
            Just base -> do
              let union = terminalSourceUnionEstablishedUnion (successorStructuralBaseEstablishedUnion base)
              progressBaseRequire
                ( structuralVersionVectorMembershipGenerationId previousVector == origin
                    && structuralVersionVectorCovers (terminalSourceUnionTerminalPredecessorVector union) previousVector
                    && terminalSourceUnionTerminalControlPrefix union >= previousControl
                )
                "retirement base does not cover predecessor"
            Nothing -> Left (StructuralProgressBaseInconsistent "missing retirement base")
          AdmissionTopologyPredecessorView _ origin target _ _ ident digestClaim -> case admission of
            Just (StructuralProgressAdmissionBase recipe record) ->
              progressBaseRequire
                ( Topology.heraldJoinBaseRecipeAdmissionId recipe == ident
                    && Topology.heraldJoinBaseRecipeDigest recipe == digestClaim
                    && heraldMembershipGenerationId (Topology.heraldJoinBaseRecipeGeneration recipe) == origin
                    && lookupClaimedCut previous == Just (Topology.heraldJoinBaseRecipeEstablishedCut recipe)
                    && case Admission.admissionRecordPhase record of
                      Admission.AdmissionActivated index generation -> heraldMembershipGenerationId generation == target && Topology.activateHeraldJoinBase index digest recipe == Right (generation, cut)
                      _ -> False
                )
                "admission base differs from predecessor/activation"
            Nothing -> Left (StructuralProgressBaseInconsistent "missing admission base")
        pure (topologyCutEstablishedId established, frontier, Just digest)
      lookupClaimedCut ident = case [topologyCutEstablishedCut established | StructuralProgressCutClaim established _ _ _ <- Map.elems certificateClaims, topologyCutEstablishedId established == ident] of
        cut : _ -> Just cut
        [] -> Nothing
      checkOccurrence installed cuts (ident, StructuralProgressOccurrenceBase stamp sourceTopology _ prerequisite) = do
        checkVector (structuralOccurrenceStampPredecessor stamp)
        let bases = [base | StructuralProgressCutBase _ (Just base) _ _ <- Map.elems cuts]
            covered (source, prefix) = case structuralVersionVectorComponent source vector of
              Just current -> current >= prefix
              Nothing -> any (\base -> maybe False (>= prefix) (structuralVersionVectorComponent source (terminalSourceUnionTerminalPredecessorVector (terminalSourceUnionEstablishedUnion (successorStructuralBaseEstablishedUnion base))))) bases
        progressBaseRequire
          ( ident == structuralOccurrenceStampOccurrence stamp
              && prerequisite <= control
              && (sourceTopology == genesis || Map.member sourceTopology installed)
              && (vectorCoversStructuralOccurrence ident vector || any (topologyCutCoversOccurrence ident) (Map.elems installed))
              && all covered (structuralVersionVectorEntries (structuralOccurrenceStampPredecessor stamp))
          )
          "Progress occurrence outside applied/installed evidence"
      checkComponent (source, prefix) =
        let retained = [structuralSequenceWord64 (structuralOccurrenceSourceSequence ident) | ident <- Map.keys occurrences, structuralOccurrenceSourceHeraldEpoch ident == source]
            expected = maybe 0 structuralSequenceWord64 (structuralPrefixSequence prefix)
         in progressBaseRequire (toInteger (length retained) == toInteger expected && (null retained || (minimum retained == 1 && maximum retained == expected))) "applied source prefix differs from occurrence history"
      admissionLineage (StructuralProgressAdmissionBase recipe record) = case Admission.admissionRecordPhase record of
        Admission.AdmissionActivated _ generation -> do
          checked <- progressBaseClaim (heraldMembershipLineage (heraldMembershipGenerationId (Topology.heraldJoinBaseRecipeGeneration recipe)) (heraldMembershipGenerationId generation) history)
          pure (checked, recipe, record)
        _ -> Left (StructuralProgressBaseInconsistent "installed admission is not activated")

admittedStructuralProgressMembership :: AdmittedStructuralProgressBaseEvidence -> HeraldMembershipGeneration
admittedStructuralProgressMembership (AdmittedStructuralProgressBaseEvidence (StructuralProgressBaseClaims membership _ _ _ _ _ _ _) _ _) = membership

admittedStructuralProgressMembershipHistory :: AdmittedStructuralProgressBaseEvidence -> Membership.HeraldMembershipHistory
admittedStructuralProgressMembershipHistory (AdmittedStructuralProgressBaseEvidence (StructuralProgressBaseClaims _ history _ _ _ _ _ _) _ _) = history

admittedStructuralProgressRetirementBases :: AdmittedStructuralProgressBaseEvidence -> [SuccessorStructuralBase]
admittedStructuralProgressRetirementBases (AdmittedStructuralProgressBaseEvidence _ cuts _) = [base | StructuralProgressCutBase _ (Just base) _ _ <- Map.elems cuts]

admittedStructuralProgressAdmissions :: AdmittedStructuralProgressBaseEvidence -> [(HeraldMembershipLineage, Topology.HeraldJoinBaseRecipe, Admission.HeraldAdmissionRecord)]
admittedStructuralProgressAdmissions (AdmittedStructuralProgressBaseEvidence _ _ admissions) = admissions

-- | Complete the independent evidence admission with the already checked
-- paired carrier. No arbitrary byte-decoded carrier is promoted by this bridge.
-- The carrier wire path also checks 'validateStructuralProgressOriginalOccurrences'
-- against the exact original archive before pairing the reconstructed owner.
prepareStructuralProgressBaseImportFromEvidence ::
  AdmittedStructuralProgressBaseEvidence ->
  Reconciliation.StructuralCarrierBase ->
  Reconciliation.StructuralAppliedState ->
  StructuralProgressState ->
  Either StructuralProgressBaseProblem PreparedStructuralProgressBaseImport
prepareStructuralProgressBaseImportFromEvidence
  (AdmittedStructuralProgressBaseEvidence (StructuralProgressBaseClaims membership history initialProjection genesis vector control occurrences _) cuts _)
  carriers
  reconciliation
  receiver = do
    let applications = Reconciliation.structuralCarrierBaseApplications carriers
    progressBaseRequire (map Reconciliation.structuralApplicationOccurrence applications == Map.keys occurrences) "paired carrier occurrence set differs"
    mapM_ matchOccurrence applications
    mapLeft
      StructuralProgressBaseImportProblem
      (prepareStructuralProgressBaseImport (StructuralProgressBase membership memberships initialProjection genesis carriers vector control occurrences cuts) reconciliation receiver)
    where
      memberships = Map.fromList [(heraldMembershipGenerationId generation, generation) | generation <- NonEmpty.toList (Membership.heraldMembershipHistoryGenerations history)]
      matchOccurrence application = case Map.lookup (Reconciliation.structuralApplicationOccurrence application) occurrences of
        Nothing -> Left (StructuralProgressBaseInconsistent "paired carrier occurrence missing")
        Just (StructuralProgressOccurrenceBase stamp _ carrierSort prerequisite) ->
          progressBaseRequire
            ( structuralOccurrenceStampPredecessor stamp == Reconciliation.structuralApplicationPredecessor application
                && structuralOccurrenceStampPublication stamp == checkedPublicationId (Reconciliation.structuralApplicationPublication application)
                && carrierSort == Reconciliation.structuralApplicationCarrierSort application
                && prerequisite == Reconciliation.structuralApplicationControlPrerequisite application
                && Just (structuralOccurrenceStampCarrierRole stamp)
                  == (Reconciliation.appliedControlledProjectionRole <$> Reconciliation.structuralAppliedControlledProjectionAtOccurrence (Reconciliation.structuralApplicationOccurrence application) reconciliation)
            )
            "paired carrier occurrence facts differ"

-- | Bind the Progress-owned stamp and source coordinates to complete original
-- payloads. The archive may also retain payloads for other consumers; only
-- occurrences represented by this base must have matching evidence here.
-- Carrier decoding independently admits the publication and its original meaning.
validateStructuralProgressOriginalOccurrences ::
  AdmittedStructuralProgressBaseEvidence ->
  Map StructuralOccurrenceId Terminal.TerminalStructuralOccurrence ->
  Either StructuralProgressBaseProblem ()
validateStructuralProgressOriginalOccurrences
  (AdmittedStructuralProgressBaseEvidence (StructuralProgressBaseClaims _ _ _ _ _ _ occurrences _) _ _)
  originals = mapM_ matchOriginal (Map.toAscList occurrences)
    where
      matchOriginal (ident, StructuralProgressOccurrenceBase stamp sourceTopology carrierSort prerequisite) = do
        original <- maybe (Left (StructuralProgressBaseInconsistent "missing original Progress occurrence payload")) Right (Map.lookup ident originals)
        progressBaseRequire
          (Terminal.terminalStructuralOccurrenceId original == ident && Terminal.terminalStructuralOccurrenceStamp original == stamp)
          "original payload stamp differs from Progress"
        semantics <- progressBaseClaim (Peer.decodeStructuralPublicationCanonicalBytes (Terminal.terminalStructuralOccurrencePayloadBytes original))
        progressBaseRequire
          ( Peer.structuralPublicationSemanticsPublicationId semantics == structuralOccurrenceStampPublication stamp
              && Peer.structuralPublicationSemanticsCarrierRole semantics == structuralOccurrenceStampCarrierRole stamp
              && Peer.structuralPublicationSemanticsSourceTopologyPrerequisite semantics == sourceTopology
              && Peer.structuralPublicationSemanticsControlPrerequisite semantics == prerequisite
              && Debt.sortOccurrence (Peer.structuralPublicationSemanticsSortId semantics) (Peer.structuralPublicationSemanticsOccurrenceId semantics) == carrierSort
          )
          "original payload semantics differ from Progress"

captureStructuralProgressBase :: StructuralProgressState -> StructuralProgressBase
captureStructuralProgressBase state =
  StructuralProgressBase
    state.currentMembership
    state.membershipHistory
    state.initialProjection
    state.genesisCutId
    (Reconciliation.captureStructuralCarrierBase state.reconciliation)
    state.appliedVector
    state.appliedControlPrefix
    (Map.map captureOccurrence state.history)
    (case captureStructuralProgressCertificates state of StructuralProgressCertificateEvidence cuts -> cuts)
  where
    captureOccurrence occurrence =
      StructuralProgressOccurrenceBase
        occurrence.stamp
        occurrence.sourceTopologyPrerequisite
        occurrence.carrierSort
        occurrence.controlPrerequisite

structuralProgressBaseCarrierBase :: StructuralProgressBase -> Reconciliation.StructuralCarrierBase
structuralProgressBaseCarrierBase (StructuralProgressBase _ _ _ _ carriers _ _ _ _) = carriers

newtype PreparedStructuralProgressBaseImport
  = PreparedStructuralProgressBaseImport StructuralProgressState

-- | Attach already admitted carrier/control facts to a fresh observer without
-- replaying old applications under current labels or sort definitions. The
-- caller atomically applies the paired carrier import's Graph patch as well.
prepareStructuralProgressBaseImport ::
  StructuralProgressBase ->
  Reconciliation.StructuralAppliedState ->
  StructuralProgressState ->
  Either StructuralProgressProblem PreparedStructuralProgressBaseImport
prepareStructuralProgressBaseImport
  (StructuralProgressBase membership memberships initialProjection genesis carriers vector control occurrences cuts)
  reconciliation
  receiver = do
    unless
      ( receiver.joiningObserver
          && Map.null receiver.history
          && receiver.appliedControlPrefix == controlIndex 0
          && receiver.appliedVector == emptyStructuralVersionVector receiver.currentMembership
          && Map.null receiver.installedCuts
          && null receiver.installedChain
          && receiver.lastInstalledCutId == receiver.genesisCutId
          && Map.null receiver.reports
          && Map.null receiver.supersededOpenCuts
          && receiver.openCut == Nothing
          && receiver.pendingAnnounce == Nothing
          && null receiver.pendingEstablished
          && all (Set.notMember receiver.localHerald . membershipSetFor) (Map.elems memberships)
      )
      (Left StructuralProgressBaseImportRequiresFreshObserver)
    unless
      ( receiver.genesisCutId == genesis
          && receiver.initialProjection == initialProjection
          && receiver.currentMembership == genesisMembershipFromHistoryInvariant memberships
      )
      (Left StructuralProgressBaseGenesisMismatch)
    unless
      (Reconciliation.captureStructuralCarrierBase reconciliation == carriers)
      (Left StructuralProgressBaseCarrierMismatch)
    let history = Map.map restoreOccurrence occurrences
        genesisFrontier = topologyFrontier (emptyStructuralVersionVector receiver.currentMembership) (controlIndex 0)
        (installed, reverseChain, coverage, _) =
          Map.foldl' (installCut history) (Map.empty, [], emptyInstalledCoverageIndex, genesisFrontier) cuts
        chain = reverse reverseChain
        latest = case reverseChain of
          cut : _ -> cut
          [] -> genesis
        successor =
          refreshCurrentNablaProjections
            receiver
              { currentMembership = membership,
                membershipHistory = memberships,
                reconciliation,
                appliedVector = vector,
                appliedControlPrefix = control,
                history,
                reports = Map.empty,
                lastInstalledCutId = latest,
                installedCuts = installed,
                installedChain = chain,
                installedCoverageIndex = coverage,
                supersededOpenCuts = Map.empty,
                openCut = Nothing,
                pendingAnnounce = Nothing,
                pendingEstablished = []
              }
    mapLeft StructuralProgressInvariantFault (validateStructuralProgressState successor)
    pure (PreparedStructuralProgressBaseImport successor)
    where
      restoreOccurrence (StructuralProgressOccurrenceBase stamp sourceTopology carrierSort prerequisite) =
        AppliedStructuralOccurrence stamp sourceTopology carrierSort prerequisite
      installCut history (installed, reverseChain, coverage, predecessorFrontier) (StructuralProgressCutBase established successorBase admissionBase joinSeal) =
        let cutId = topologyCutEstablishedId established
            cut = topologyCutEstablishedCut established
            retained =
              InstalledTopologyCut
                { installedEstablished = established,
                  ownAcceptance = find ((== receiver.localHerald) . topologyCutAcceptanceReporter) (topologyCutEstablishedAcceptances established),
                  ownAcknowledgement = Nothing,
                  newlyCoveredOccurrences =
                    [ occurrence
                    | occurrence <- Map.keys history,
                      topologyCutCoversOccurrence occurrence cut,
                      not (frontierCoversOccurrence occurrence predecessorFrontier)
                    ],
                  installedAcknowledgements = Map.empty,
                  membershipSuccessorBase = successorBase,
                  membershipAdmissionBase = case admissionBase of
                    Nothing -> Nothing
                    Just (StructuralProgressAdmissionBase recipe record) -> Just (recipe, record),
                  joinSealAdmission = joinSeal
                }
         in (Map.insert cutId retained installed, cutId : reverseChain, extendInstalledCoverageIndex cutId cut coverage, topologyCutFrontier cut)

commitStructuralProgressBaseImport :: PreparedStructuralProgressBaseImport -> StructuralProgressState
commitStructuralProgressBaseImport (PreparedStructuralProgressBaseImport successor) = successor

data StructuralProgressProblem
  = StructuralProgressVectorProblem StructuralVectorProblem
  | StructuralProgressTopologyShapeProblem TopologyShapeProblem
  | StructuralProgressLocalHeraldNotMember HeraldEpoch
  | StructuralProgressReconciliationMembershipMismatch
  | StructuralProgressInitialHistoryNotEmpty
  | StructuralProgressBaseImportRequiresFreshObserver
  | StructuralProgressBaseGenesisMismatch
  | StructuralProgressBaseCarrierMismatch
  | StructuralProgressPreparedAgainstDifferentState
  | StructuralProgressPreparedWithoutApplication
  | StructuralProgressRefreshCannotAdvanceVector
  | StructuralProgressOccurrenceMismatch
      StructuralOccurrenceId
      StructuralOccurrenceId
  | StructuralProgressPredecessorMismatch
  | StructuralProgressPublicationMismatch PublicationId PublicationId
  | StructuralProgressCarrierRoleMismatch
  | StructuralProgressSourceTopologyUnavailable TopologyCutId
  | StructuralProgressPredecessorNotCovered
  | StructuralProgressControlPrerequisiteNotCovered ControlIndex ControlIndex
  | StructuralProgressTopologyControlCoordinateMismatch
      TopologyCutId
      ControlIndex
      ControlIndex
  | StructuralProgressSourceComponentMissing HeraldEpoch
  | StructuralProgressSourceGap
  | StructuralProgressOccurrenceConflict StructuralOccurrenceId
  | StructuralProgressGraphBaselineProblem Graph.StructuralGraphBaselineProblem
  | StructuralProgressGraphProblem Graph.StructuralGraphPatchProblem
  | StructuralProgressForeignReporter HeraldEpoch
  | StructuralProgressWrongMembershipGeneration
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | StructuralProgressWrongMemberSet MemberSetDigest MemberSetDigest
  | StructuralProgressReportVectorMembershipMismatch HeraldEpoch
  | StructuralProgressIncomparableReport HeraldEpoch
  | StructuralProgressControlRegression ControlIndex ControlIndex
  | StructuralProgressNotAnnouncer HeraldEpoch HeraldEpoch
  | StructuralProgressNoCompleteReportMatrix
  | StructuralProgressOpenCutExists TopologyCutId
  | StructuralProgressNoOpenCut
  | StructuralProgressCutIdMismatch TopologyCutId TopologyCutId
  | StructuralProgressWrongCutAnnouncer HeraldEpoch HeraldEpoch
  | StructuralProgressCutPredecessorMismatch TopologyCutId TopologyCutId
  | StructuralProgressOrdinaryCutRequiresSameGenerationPredecessor TopologyCutId
  | StructuralProgressUnknownCutPredecessor TopologyCutId
  | StructuralProgressCutFrontierRegressed
  | StructuralProgressCutDoesNotAdvance
  | StructuralProgressCutFrontierAhead
  | StructuralProgressTopologyDigestMismatch
  | StructuralProgressTopologyProjectionHeld (Set Reconciliation.StructuralDependency)
  | StructuralProgressReconciliationProblem Reconciliation.StructuralReconciliationProblem
  | StructuralProgressWrongAcceptanceCut TopologyCutId TopologyCutId
  | StructuralProgressAcceptanceDoesNotCover HeraldEpoch
  | StructuralProgressAcceptanceConflict HeraldEpoch
  | StructuralProgressAcceptanceForUnknownCut TopologyCutId
  | StructuralProgressEstablishedEvidenceProblem
  | StructuralProgressIncompleteEstablishedEvidence
  | StructuralProgressEstablishedConflict TopologyCutId
  | StructuralProgressAckForUnknownCut TopologyCutId
  | StructuralProgressAckConflict HeraldEpoch
  | StructuralProgressTerminalSourceProblem TerminalSourceProblem
  | StructuralProgressSuccessorBaseMismatch
  | StructuralProgressSuccessorBaseAhead
  | StructuralProgressSuccessorBaseControlAhead ControlIndex ControlIndex
  | StructuralProgressMembershipHistoryConflict HeraldMembershipGenerationId
  | StructuralProgressAdmissionCertificateMismatch
  | StructuralProgressAdmissionGraphProblem Graph.GraphBootstrapError
  | StructuralProgressObserverCannotContribute
  | StructuralProgressInvariantFault StructuralProgressInvariantProblem
  deriving stock (Eq, Show)

initialStructuralProgressState ::
  SystemId ->
  HeraldEpoch ->
  HeraldMembershipGeneration ->
  InitialProjectionDigest ->
  Reconciliation.StructuralAppliedState ->
  Either StructuralProgressProblem StructuralProgressState
initialStructuralProgressState system local membership initialProjection applied = do
  initialProgressState False system local membership initialProjection applied

-- | An applicant reconstructs original genesis and admitted history under its
-- own identity. It has no report, acceptance, acknowledgement, or active role
-- until a checked activation installs its admission base.
initialJoiningStructuralProgressState :: SystemId -> HeraldEpoch -> HeraldMembershipGeneration -> InitialProjectionDigest -> Reconciliation.StructuralAppliedState -> Either StructuralProgressProblem StructuralProgressState
initialJoiningStructuralProgressState = initialProgressState True

structuralProgressIsJoiningObserver :: StructuralProgressState -> Bool
structuralProgressIsJoiningObserver state = state.joiningObserver

initialProgressState :: Bool -> SystemId -> HeraldEpoch -> HeraldMembershipGeneration -> InitialProjectionDigest -> Reconciliation.StructuralAppliedState -> Either StructuralProgressProblem StructuralProgressState
initialProgressState observer system local membership initialProjection applied = do
  let emptyVector = emptyStructuralVersionVector membership
      normalizedMembers = heraldMembershipGenerationActiveHeraldEpochs membership
      memberDigest = heraldMembershipGenerationActiveMemberSetDigest membership
      generationId = heraldMembershipGenerationId membership
      membershipSet = Set.fromList (NonEmpty.toList normalizedMembers)
  if Set.member local membershipSet /= observer
    then Right ()
    else Left (StructuralProgressLocalHeraldNotMember local)
  if Reconciliation.structuralAppliedMembershipGenerationId applied == generationId
    && Reconciliation.structuralAppliedMemberSetDigest applied == memberDigest
    && Set.fromList (Reconciliation.structuralAppliedFixedMembership applied) == membershipSet
    then Right ()
    else Left StructuralProgressReconciliationMembershipMismatch
  if null (Reconciliation.structuralAppliedHistoryOccurrences applied)
    then Right ()
    else Left StructuralProgressInitialHistoryNotEmpty
  genesis <-
    mapLeft
      StructuralProgressTopologyShapeProblem
      (deriveGenesisTopologyCutId system membership initialProjection)
  let zero = controlIndex 0
      localReport = structuralAppliedReport local emptyVector zero
  Right
    StructuralProgressState
      { localHerald = local,
        joiningObserver = observer,
        currentMembership = membership,
        membershipHistory = Map.singleton generationId membership,
        initialProjection,
        reconciliation = applied,
        appliedVector = emptyVector,
        appliedControlPrefix = zero,
        currentNablaProjections =
          deriveCurrentNablaProjections
            (CurrentNablaQueryInputs genesis zero applied (Right (CurrentNablaFrontier emptyVector zero))),
        proposalProjection = derive Nothing,
        history = Map.empty,
        reports = if observer then Map.empty else Map.singleton local localReport,
        genesisCutId = genesis,
        lastInstalledCutId = genesis,
        installedCuts = Map.empty,
        installedChain = [],
        installedCoverageIndex = emptyInstalledCoverageIndex,
        supersededOpenCuts = Map.empty,
        openCut = Nothing,
        pendingAnnounce = Nothing,
        pendingEstablished = [],
        preparationReadinessChanged = False,
        alignmentPlanChanged = False
      }

-- | One coalesced preparation notification for actual structural authority
-- changes. Report/acceptance/acknowledgement bookkeeping leaves it unchanged.
takePreparationReadinessChange :: StructuralProgressState -> (Bool, StructuralProgressState)
takePreparationReadinessChange state =
  let changed = state.preparationReadinessChanged
   in changed `seq` (changed, clearPreparationReadinessChange state)

-- | Initial assembly and explicit semantic comparisons only; the preparation
-- consumer must take the notification before clearing it.
clearPreparationReadinessChange :: StructuralProgressState -> StructuralProgressState
clearPreparationReadinessChange state = state {preparationReadinessChanged = False}

-- | Coalesced facts read by alignment plan selection. Delivery/report
-- bookkeeping is intentionally absent from this independent consumer journal.
takeAlignmentPlanChange :: StructuralProgressState -> (Bool, StructuralProgressState)
takeAlignmentPlanChange state =
  let changed = state.alignmentPlanChanged
   in changed `seq` (changed, clearAlignmentPlanChange state)

clearAlignmentPlanChange :: StructuralProgressState -> StructuralProgressState
clearAlignmentPlanChange state = state {alignmentPlanChanged = False}

-- | Oracle membership can change before its successor structural base is
-- installed. Wake membership-sensitive plan selection independently of the
-- ordinary control cursor, whose progress alone does not change plan inputs.
notifyAlignmentMembershipChange :: StructuralProgressState -> StructuralProgressState
notifyAlignmentMembershipChange state = state {alignmentPlanChanged = True}

structuralProgressLocalHerald :: StructuralProgressState -> HeraldEpoch
structuralProgressLocalHerald state = state.localHerald

-- | Narrow contradiction fixture for dynamic-writer residence checks.
replaceStructuralProgressLocalHeraldForInvariantTest ::
  HeraldEpoch ->
  StructuralProgressState ->
  StructuralProgressState
replaceStructuralProgressLocalHeraldForInvariantTest replacement state =
  state {localHerald = replacement}

currentMembers :: StructuralProgressState -> NonEmpty HeraldEpoch
currentMembers =
  heraldMembershipGenerationActiveHeraldEpochs . (.currentMembership)

currentMemberSetDigest :: StructuralProgressState -> MemberSetDigest
currentMemberSetDigest =
  heraldMembershipGenerationActiveMemberSetDigest . (.currentMembership)

currentMembershipGenerationId ::
  StructuralProgressState -> HeraldMembershipGenerationId
currentMembershipGenerationId =
  heraldMembershipGenerationId . (.currentMembership)

structuralProgressMembers :: StructuralProgressState -> NonEmpty HeraldEpoch
structuralProgressMembers = currentMembers

structuralProgressMembershipGenerationId ::
  StructuralProgressState -> HeraldMembershipGenerationId
structuralProgressMembershipGenerationId = currentMembershipGenerationId

structuralProgressMembershipHistory ::
  StructuralProgressState ->
  [(HeraldMembershipGenerationId, HeraldMembershipGeneration)]
structuralProgressMembershipHistory = Map.toAscList . (.membershipHistory)

structuralProgressMemberSetDigest :: StructuralProgressState -> MemberSetDigest
structuralProgressMemberSetDigest = currentMemberSetDigest

structuralProgressReconciliation ::
  StructuralProgressState -> Reconciliation.StructuralAppliedState
structuralProgressReconciliation state = state.reconciliation

-- | Narrow whole-state contradiction fixture: restore one live structural
-- resource while leaving retained projection/control history untouched.
restoreStructuralLocalStoreForInvariantTest ::
  Reconciliation.DynamicLocalStoreSpec ->
  StructuralProgressState ->
  StructuralProgressState
restoreStructuralLocalStoreForInvariantTest resource state =
  refreshCurrentNablaProjections
    $ state
      { reconciliation =
          Reconciliation.restoreStructuralLocalStoreForInvariantTest
            resource
            state.reconciliation
      }

structuralAppliedVector :: StructuralProgressState -> StructuralVersionVector
structuralAppliedVector state = state.appliedVector

structuralAppliedControlPrefix :: StructuralProgressState -> ControlIndex
structuralAppliedControlPrefix state = state.appliedControlPrefix

structuralLocalReport :: StructuralProgressState -> StructuralAppliedReport
structuralLocalReport state =
  case Map.lookup state.localHerald state.reports of
    Just report -> report
    Nothing -> error "structural progress lost its seeded local report"

structuralReportEntries ::
  StructuralProgressState -> [(HeraldEpoch, StructuralAppliedReport)]
structuralReportEntries state = Map.toAscList state.reports

structuralGenesisCutId :: StructuralProgressState -> TopologyCutId
structuralGenesisCutId state = state.genesisCutId

structuralLastInstalledCutId :: StructuralProgressState -> TopologyCutId
structuralLastInstalledCutId state = state.lastInstalledCutId

-- | The control frontier authenticated by the exact cut returned by
-- 'structuralLastInstalledCutId'.  Applied control may already be ahead while
-- a successor cut remains open, so publication coordinates must use this
-- frontier rather than the live applied prefix.
structuralLastInstalledCutControlPrefix :: StructuralProgressState -> ControlIndex
structuralLastInstalledCutControlPrefix state
  | state.lastInstalledCutId == state.genesisCutId = controlIndex 0
  | otherwise =
      topologyFrontierAppliedControlPrefix
        . topologyCutFrontier
        . installedTopologyCutCut
        $ case Map.lookup state.lastInstalledCutId state.installedCuts of
          Just installed -> installed
          Nothing -> error "structural progress lost its last installed cut"

-- | Stamp a publication at a control coordinate coherent with its selected
-- source-topology prerequisite.  The writer tenure supplies an authority
-- minimum, while the installed cut may require a later exact control frontier.
structuralPublicationControlPrerequisite ::
  ControlIndex -> StructuralProgressState -> ControlIndex
structuralPublicationControlPrerequisite authorityMinimum state =
  max authorityMinimum (structuralLastInstalledCutControlPrefix state)

structuralInstalledCutEntries ::
  StructuralProgressState -> [(TopologyCutId, InstalledTopologyCut)]
structuralInstalledCutEntries state = Map.toAscList state.installedCuts

structuralInstalledCutIds :: StructuralProgressState -> [TopologyCutId]
structuralInstalledCutIds state = state.installedChain

-- | The retained established transcript in predecessor order. This is the
-- repair carrier used by the deterministic announcer after reconnect; map-key
-- order is deliberately not a substitute for chain order.
structuralInstalledEstablishedChain ::
  StructuralProgressState -> [TopologyCutEstablished]
structuralInstalledEstablishedChain state =
  fmap establishedFor state.installedChain
  where
    establishedFor cutId = case Map.lookup cutId state.installedCuts of
      Just installed -> installed.installedEstablished
      Nothing -> error "structural progress installed chain lost its cut"

structuralInstalledSuccessorBases :: StructuralProgressState -> [SuccessorStructuralBase]
structuralInstalledSuccessorBases state = [base | cutId <- state.installedChain, Just installed <- [Map.lookup cutId state.installedCuts], Just base <- [installed.membershipSuccessorBase]]

structuralOpenCut :: StructuralProgressState -> Maybe TopologyCutAnnounce
structuralOpenCut state = (.announce) <$> state.openCut

structuralOpenLocalAcceptance ::
  StructuralProgressState -> Maybe TopologyCutAcceptance
structuralOpenLocalAcceptance state = do
  open <- state.openCut
  Map.lookup state.localHerald open.acceptances

structuralOpenEstablished ::
  StructuralProgressState -> Maybe TopologyCutEstablished
structuralOpenEstablished state = state.openCut >>= (.openEstablished)

structuralPendingAnnounce ::
  StructuralProgressState -> Maybe TopologyCutAnnounce
structuralPendingAnnounce state = state.pendingAnnounce

structuralPendingEstablished ::
  StructuralProgressState -> Maybe TopologyCutEstablished
structuralPendingEstablished state = case state.pendingEstablished of
  established : _ -> Just established
  [] -> Nothing

-- | Dependency-held Established evidence in exact predecessor order.  A
-- reconnect repair can reoffer more than one installed cut before the local
-- structural owner reaches the first frontier, so retaining only the latest
-- fact would lose the dependency needed to install the rest of the chain.
structuralPendingEstablishedChain ::
  StructuralProgressState -> [TopologyCutEstablished]
structuralPendingEstablishedChain state = state.pendingEstablished

-- | A checked first-cut installation for one membership contraction.  Keeping
-- the preparation opaque makes the membership switch, vector projection,
-- reconciliation requalification, report reset, and cut-chain append one
-- indivisible pure owner transition.
data PreparedMembershipSuccessorBaseInstallation
  = PreparedMembershipSuccessorBaseInstallation
      StructuralProgressState
      TopologyCut

prepareMembershipSuccessorBaseInstallation ::
  HeraldMembershipGeneration ->
  SuccessorStructuralBase ->
  TopologyCut ->
  StructuralProgressState ->
  Either StructuralProgressProblem PreparedMembershipSuccessorBaseInstallation
prepareMembershipSuccessorBaseInstallation successorMembership base cut state = do
  mapLeft StructuralProgressInvariantFault (validateStructuralProgressState state)
  let establishedUnion = successorStructuralBaseEstablishedUnion base
      union = terminalSourceUnionEstablishedUnion establishedUnion
      terminalVector = terminalSourceUnionTerminalPredecessorVector union
      initialVector = terminalSourceUnionSuccessorInitialVector union
      terminalControl = terminalSourceUnionTerminalControlPrefix union
      cutId = deriveTopologyCutId cut
      successorGenerationId = heraldMembershipGenerationId successorMembership
  checkedBase <-
    mapLeft
      StructuralProgressTerminalSourceProblem
      ( establishedSuccessorStructuralBase
          (successorStructuralBaseLineage base)
          establishedUnion
          cut
      )
  unless
    ( checkedBase == base
        && successorStructuralBaseCutId base == cutId
        && heraldMembershipLineageOrigin (successorStructuralBaseLineage base) == state.currentMembership
        && heraldMembershipLineageTarget (successorStructuralBaseLineage base) == successorMembership
    )
    (Left StructuralProgressSuccessorBaseMismatch)
  unless
    (topologyPredecessorCutId (topologyCutPredecessor cut) == state.lastInstalledCutId)
    ( Left
        ( StructuralProgressCutPredecessorMismatch
            state.lastInstalledCutId
            (topologyPredecessorCutId (topologyCutPredecessor cut))
        )
    )
  unless
    (state.joiningObserver || Set.member state.localHerald (membershipSetFor successorMembership))
    (Left (StructuralProgressLocalHeraldNotMember state.localHerald))
  case Map.lookup successorGenerationId state.membershipHistory of
    Nothing -> Right ()
    Just incumbent ->
      unless
        (incumbent == successorMembership)
        (Left (StructuralProgressMembershipHistoryConflict successorGenerationId))
  -- A terminal union may be agreed before this Herald has materialized every
  -- occurrence it names.  Installing the cut must not invent that application:
  -- wait until the predecessor-generation applied owner covers the exact
  -- terminal vector and retirement control coordinate.
  unless
    (structuralVersionVectorCovers state.appliedVector terminalVector)
    (Left StructuralProgressSuccessorBaseAhead)
  unless
    (state.appliedControlPrefix >= terminalControl)
    ( Left
        ( StructuralProgressSuccessorBaseControlAhead
            state.appliedControlPrefix
            terminalControl
        )
    )
  projectedVector <-
    mapLeft
      StructuralProgressTerminalSourceProblem
      (projectSurvivorLiveAheadVector state.appliedVector base)
  reconciled <- mapLeft StructuralProgressReconciliationProblem (Reconciliation.establishStructuralAppliedMembership base projectedVector state.reconciliation)
  let acceptanceReports =
        [ structuralAppliedReport reporter initialVector terminalControl
        | terminalAcceptance <- terminalSourceUnionEstablishedAcceptances establishedUnion,
          let reporter = terminalSourceUnionAcceptanceReporter terminalAcceptance
        ]
      acceptances = fmap (topologyCutAcceptance cutId) acceptanceReports
  established <-
    mapLeft
      (const StructuralProgressEstablishedEvidenceProblem)
      (topologyCutEstablished cutId cut acceptances)
  let ownAcceptance = find ((== state.localHerald) . topologyCutAcceptanceReporter) acceptances
  let acknowledgement =
        topologyCutEstablishedAck cutId state.localHerald successorGenerationId
      predecessorFrontier = currentInstalledFrontier state
      newlyCovered =
        [ occurrence
        | occurrence <- Map.keys state.history,
          topologyCutCoversOccurrence occurrence cut,
          not (frontierCoversOccurrence occurrence predecessorFrontier)
        ]
      installed =
        InstalledTopologyCut
          { installedEstablished = established,
            ownAcceptance,
            ownAcknowledgement = if state.joiningObserver then Nothing else Just acknowledgement,
            newlyCoveredOccurrences = newlyCovered,
            installedAcknowledgements = if state.joiningObserver then Map.empty else Map.singleton state.localHerald acknowledgement,
            membershipSuccessorBase = Just base,
            membershipAdmissionBase = Nothing,
            joinSealAdmission = Nothing
          }
      rebased =
        state
          { currentMembership = successorMembership,
            membershipHistory =
              Map.union
                (Map.fromList [(heraldMembershipGenerationId generation, generation) | generation <- NonEmpty.toList (heraldMembershipLineageGenerations (successorStructuralBaseLineage base))])
                state.membershipHistory,
            reconciliation = reconciled,
            appliedVector = projectedVector,
            reports = Map.empty,
            lastInstalledCutId = cutId,
            installedCuts = Map.insert cutId installed state.installedCuts,
            installedChain = state.installedChain <> [cutId],
            installedCoverageIndex = extendInstalledCoverageIndex cutId cut state.installedCoverageIndex,
            supersededOpenCuts = Map.union (supersededOpenCutTombstone cutId state.openCut) state.supersededOpenCuts,
            openCut = Nothing,
            pendingAnnounce = Nothing,
            pendingEstablished = []
          }
      successor = updateLocalReport (refreshCurrentNablaProjections rebased)
  mapLeft StructuralProgressInvariantFault (validateStructuralProgressState successor)
  pure (PreparedMembershipSuccessorBaseInstallation successor cut)

-- Only an unestablished open cut is actually abandoned by the atomic
-- membership transition.  An established open cut is already retained in the
-- installed chain, which is the stronger evidence for its late acceptances and
-- acknowledgements. Later membership transitions retain earlier tombstones so
-- exact delayed acceptances remain recognizable throughout the installed chain.
supersededOpenCutTombstone ::
  TopologyCutId -> Maybe OpenTopologyCut -> Map TopologyCutId SupersededOpenCut
supersededOpenCutTombstone supersedingCut = \case
  Just open
    | Nothing <- open.openEstablished ->
        let announce = open.announce
         in Map.singleton
              (topologyCutAnnounceId announce)
              SupersededOpenCut {announce, supersededBy = supersedingCut}
  _ -> Map.empty

preparedMembershipSuccessorBaseCut ::
  PreparedMembershipSuccessorBaseInstallation -> TopologyCut
preparedMembershipSuccessorBaseCut
  (PreparedMembershipSuccessorBaseInstallation _ cut) = cut

commitMembershipSuccessorBaseInstallation ::
  PreparedMembershipSuccessorBaseInstallation -> StructuralProgressState
commitMembershipSuccessorBaseInstallation
  (PreparedMembershipSuccessorBaseInstallation successor _) = successor

-- | Whether this exact membership-successor base remains in the installed cut
-- chain.  Installation is monotone: later same-generation cuts do not revoke
-- the lineage witness which admitted successor-generation work.
structuralSuccessorBaseInstalled ::
  SuccessorStructuralBase -> StructuralProgressState -> Bool
structuralSuccessorBaseInstalled base state =
  maybe
    False
    ((== Just base) . (.membershipSuccessorBase))
    (Map.lookup (successorStructuralBaseCutId base) state.installedCuts)

data PreparedMembershipAdmissionBaseInstallation = PreparedMembershipAdmissionBaseInstallation Graph.State StructuralProgressState TopologyCut

-- | A reporter's own verified retained predecessor bytes and exact manifest.
-- There is no successor generation or guessed activation index in readiness.
data PreparedHeraldJoinBaseCandidate = PreparedHeraldJoinBaseCandidate Topology.HeraldJoinBaseRecipe JoinBootstrap.HeraldBootstrapContribution

preparedHeraldJoinBaseCandidateDigest :: PreparedHeraldJoinBaseCandidate -> ByteString
preparedHeraldJoinBaseCandidateDigest (PreparedHeraldJoinBaseCandidate recipe _) = Topology.heraldJoinBaseRecipeDigestBytes recipe

prepareHeraldJoinBaseCandidate :: Admission.HeraldAdmissionRecord -> Topology.HeraldJoinBaseRecipe -> JoinBootstrap.HeraldBootstrapContribution -> Reconciliation.ReconciliationViews -> Graph.State -> StructuralProgressState -> Either StructuralProgressProblem PreparedHeraldJoinBaseCandidate
prepareHeraldJoinBaseCandidate record recipe contribution views graph state = do
  mapLeft StructuralProgressInvariantFault (validateStructuralProgressState state)
  seal <- maybe (Left StructuralProgressAdmissionCertificateMismatch) Right (Admission.admissionRecordSeal record)
  let oldCut = Topology.heraldJoinBaseRecipeEstablishedCut recipe
      frontier = topologyCutFrontier oldCut
      historicalActivation = case Admission.admissionRecordPhase record of
        Admission.AdmissionActivated {} -> True
        _ -> False
      localEligible = Set.member state.localHerald (membershipSetFor state.currentMembership) || (state.joiningObserver && (state.localHerald == Topology.heraldJoinBaseRecipeApplicant recipe || historicalActivation))
      phaseEligible = case Admission.admissionRecordPhase record of
        Admission.AdmissionSealed -> True
        Admission.AdmissionReady -> True
        Admission.AdmissionActivated {} -> True
        _ -> False
      openSettled = maybe True ((/= Nothing) . (.openEstablished)) state.openCut
  unless
    ( phaseEligible
        && localEligible
        && openSettled
        && Admission.admissionRecordId record == Topology.heraldJoinBaseRecipeAdmissionId recipe
        && Admission.admissionRecordPredecessor record == Topology.heraldJoinBaseRecipeGeneration recipe
        && state.currentMembership == Topology.heraldJoinBaseRecipeGeneration recipe
        && Admission.joinSealTopologyCut seal == oldCut
        && Admission.heraldJoinDigestBytes (Admission.joinSealRecipeDigest seal) == Topology.heraldJoinBaseRecipeDigestBytes recipe
        && Admission.heraldJoinDigestBytes (Admission.joinSealContributionDigest seal) == Topology.topologyOccurrenceDigestBytes (JoinBootstrap.contributionDigest contribution)
        && JoinBootstrap.contributionDigest contribution == Topology.heraldJoinBaseRecipeContributionDigest recipe
        && JoinBootstrap.contributionAdmission contribution == Topology.heraldJoinBaseRecipeAdmissionId recipe
        && JoinBootstrap.contributionHerald contribution == Topology.heraldJoinBaseRecipeApplicant recipe
        && JoinBootstrap.contributionSystem contribution == Admission.admissionManifestSystem (Admission.admissionRecordManifest record)
        && state.lastInstalledCutId == deriveTopologyCutId oldCut
        && maybe False ((== oldCut) . installedTopologyCutCut) (Map.lookup state.lastInstalledCutId state.installedCuts)
        && state.appliedVector == topologyFrontierStructuralVersionVector frontier
        && state.appliedControlPrefix >= Admission.joinSealControlPrefix seal
        && all (\(_, delta, _) -> not (Graph.graphHasVertex (DeltaVertex delta) graph)) (JoinBootstrap.contributionViews contribution)
    )
    (Left StructuralProgressAdmissionCertificateMismatch)
  reconstructed <- projectionAtFrontier views frontier state
  digest <- either (Left . StructuralProgressTopologyProjectionHeld) Right reconstructed
  unless (digest == topologyCutOccurrenceDigest oldCut) (Left StructuralProgressTopologyDigestMismatch)
  Right (PreparedHeraldJoinBaseCandidate recipe contribution)

preparedMembershipAdmissionBaseCut :: PreparedMembershipAdmissionBaseInstallation -> TopologyCut
preparedMembershipAdmissionBaseCut (PreparedMembershipAdmissionBaseInstallation _ _ cut) = cut

commitMembershipAdmissionBaseInstallation :: PreparedMembershipAdmissionBaseInstallation -> (Graph.State, StructuralProgressState)
commitMembershipAdmissionBaseInstallation (PreparedMembershipAdmissionBaseInstallation graph progress _) = (graph, progress)

-- | Applying the Oracle activation certificate installs the already prepared
-- recipe atomically. Readiness proves every old member and the applicant hold
-- the exact bytes; no new successor-generation cut exchange is needed.
prepareMembershipAdmissionBaseInstallation :: Admission.HeraldAdmissionRecord -> Topology.HeraldJoinBaseRecipe -> JoinBootstrap.HeraldBootstrapContribution -> Reconciliation.ReconciliationViews -> Graph.State -> StructuralProgressState -> Either StructuralProgressProblem PreparedMembershipAdmissionBaseInstallation
prepareMembershipAdmissionBaseInstallation certificate recipe contribution views graph state = do
  mapLeft StructuralProgressInvariantFault (validateStructuralProgressState state)
  unless (admissionCertificateMatches recipe certificate) (Left StructuralProgressAdmissionCertificateMismatch)
  (index, membership) <- case Admission.admissionRecordPhase certificate of
    Admission.AdmissionActivated coordinate admitted -> Right (coordinate, admitted)
    _ -> Left StructuralProgressAdmissionCertificateMismatch
  unless
    ( JoinBootstrap.contributionAdmission contribution == Topology.heraldJoinBaseRecipeAdmissionId recipe
        && JoinBootstrap.contributionHerald contribution == Topology.heraldJoinBaseRecipeApplicant recipe
        && JoinBootstrap.contributionSystem contribution == Admission.admissionManifestSystem (Admission.admissionRecordManifest certificate)
        && JoinBootstrap.contributionDigest contribution == Topology.heraldJoinBaseRecipeContributionDigest recipe
    )
    (Left StructuralProgressAdmissionCertificateMismatch)
  graphPrepared <- mapLeft StructuralProgressAdmissionGraphProblem (Graph.prepareGraphMembershipAdmission index contribution graph)
  case find ((== Just (recipe, certificate)) . (.membershipAdmissionBase)) (Map.elems state.installedCuts) of
    Just installed -> Right (PreparedMembershipAdmissionBaseInstallation (Graph.commitGraphMembershipAdmission graphPrepared) state (installedTopologyCutCut installed))
    Nothing -> do
      _ <- prepareHeraldJoinBaseCandidate certificate recipe contribution views graph state
      let oldCut = Topology.heraldJoinBaseRecipeEstablishedCut recipe
          oldFrontier = topologyCutFrontier oldCut
          oldVector = topologyFrontierStructuralVersionVector oldFrontier
          oldControl = topologyFrontierAppliedControlPrefix oldFrontier
      unless
        ( state.currentMembership == Topology.heraldJoinBaseRecipeGeneration recipe
            && state.lastInstalledCutId == deriveTopologyCutId oldCut
            && maybe False ((== oldCut) . installedTopologyCutCut) (Map.lookup state.lastInstalledCutId state.installedCuts)
            && (state.joiningObserver || Set.member state.localHerald (membershipSetFor membership))
            && state.appliedVector == oldVector
            && state.appliedControlPrefix >= oldControl
            && maybe True ((/= Nothing) . (.openEstablished)) state.openCut
        )
        (Left StructuralProgressSuccessorBaseMismatch)
      history <- either (const (Left StructuralProgressAdmissionCertificateMismatch)) Right (heraldMembershipHistory (NonEmpty.fromList (sortOn (maybe (controlIndex 0) id . heraldMembershipGenerationChangeControlIndex) (membership : Map.elems state.membershipHistory))))
      lineage <- either (const (Left StructuralProgressAdmissionCertificateMismatch)) Right (heraldMembershipLineage (currentMembershipGenerationId state) (heraldMembershipGenerationId membership) history)
      (_, provisionalCut) <- mapLeft StructuralProgressTopologyShapeProblem (Topology.activateHeraldJoinBase index (topologyCutOccurrenceDigest oldCut) recipe)
      let initial = topologyFrontierStructuralVersionVector (topologyCutFrontier provisionalCut)
      reconciliation <- mapLeft StructuralProgressReconciliationProblem (Reconciliation.establishStructuralAdmissionMembership lineage recipe contribution provisionalCut initial state.reconciliation)
      let provisional =
            state
              { currentMembership = membership,
                membershipHistory = Map.insert (heraldMembershipGenerationId membership) membership state.membershipHistory,
                reconciliation,
                appliedVector = initial,
                appliedControlPrefix = max index state.appliedControlPrefix,
                joiningObserver = Set.notMember state.localHerald (membershipSetFor membership)
              }
      occurrenceResult <- projectionAtFrontier views (topologyFrontier initial index) provisional
      occurrence <- either (Left . StructuralProgressTopologyProjectionHeld) Right occurrenceResult
      (_, cut) <- mapLeft StructuralProgressTopologyShapeProblem (Topology.activateHeraldJoinBase index occurrence recipe)
      let cutId = deriveTopologyCutId cut
          generationId = heraldMembershipGenerationId membership
          reports = [structuralAppliedReport reporter initial index | reporter <- NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership)]
          acceptances = map (topologyCutAcceptance cutId) reports
          acknowledgement = topologyCutEstablishedAck cutId state.localHerald generationId
      established <- mapLeft (const StructuralProgressEstablishedEvidenceProblem) (topologyCutEstablished cutId cut acceptances)
      let installed =
            InstalledTopologyCut
              { installedEstablished = established,
                ownAcceptance = find ((== state.localHerald) . topologyCutAcceptanceReporter) acceptances,
                ownAcknowledgement = if provisional.joiningObserver then Nothing else Just acknowledgement,
                newlyCoveredOccurrences = [],
                installedAcknowledgements = if provisional.joiningObserver then Map.empty else Map.singleton state.localHerald acknowledgement,
                membershipSuccessorBase = Nothing,
                membershipAdmissionBase = Just (recipe, certificate),
                joinSealAdmission = Nothing
              }
          successor =
            updateLocalReport
              ( refreshCurrentNablaProjections
                  provisional
                    { reports = Map.empty,
                      lastInstalledCutId = cutId,
                      installedCuts = Map.insert cutId installed state.installedCuts,
                      installedChain = state.installedChain <> [cutId],
                      installedCoverageIndex = extendInstalledCoverageIndex cutId cut state.installedCoverageIndex,
                      openCut = Nothing,
                      pendingAnnounce = Nothing,
                      pendingEstablished = []
                    }
              )
      mapLeft StructuralProgressInvariantFault (validateStructuralProgressState successor)
      Right (PreparedMembershipAdmissionBaseInstallation (Graph.commitGraphMembershipAdmission graphPrepared) successor cut)

admissionCertificateMatches :: Topology.HeraldJoinBaseRecipe -> Admission.HeraldAdmissionRecord -> Bool
admissionCertificateMatches recipe record =
  Admission.admissionRecordId record == Topology.heraldJoinBaseRecipeAdmissionId recipe
    && Admission.admissionRecordPredecessor record == Topology.heraldJoinBaseRecipeGeneration recipe
    && Admission.admissionManifestHeraldEpoch (Admission.admissionRecordManifest record) == Topology.heraldJoinBaseRecipeApplicant recipe
    && Admission.admissionRecordSealAcceptors record == members
    && Admission.admissionRecordBaseReporters record == members
    && maybe False ((== Topology.heraldJoinBaseRecipeApplicant recipe) . Admission.joinReadyReporter) (Admission.admissionRecordNewcomerReport record)
    && case Admission.admissionRecordSeal record of
      Nothing -> False
      Just seal ->
        Admission.joinSealTopologyCut seal == Topology.heraldJoinBaseRecipeEstablishedCut recipe
          && Admission.heraldJoinDigestBytes (Admission.joinSealRecipeDigest seal) == Topology.heraldJoinBaseRecipeDigestBytes recipe
          && Admission.heraldJoinDigestBytes (Admission.joinSealContributionDigest seal) == Topology.topologyOccurrenceDigestBytes (Topology.heraldJoinBaseRecipeContributionDigest recipe)
          && case Admission.admissionRecordPhase record of
            Admission.AdmissionActivated index membership ->
              fmap fst (Topology.activateHeraldJoinBase index (topologyCutOccurrenceDigest (Topology.heraldJoinBaseRecipeEstablishedCut recipe)) recipe) == Right membership
            _ -> False
  where
    members = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (Topology.heraldJoinBaseRecipeGeneration recipe))

lookupAppliedStructuralOccurrence ::
  StructuralOccurrenceId ->
  StructuralProgressState ->
  Maybe AppliedStructuralOccurrence
lookupAppliedStructuralOccurrence occurrence state = Map.lookup occurrence state.history

lookupInstalledTopologyCut ::
  TopologyCutId -> StructuralProgressState -> Maybe InstalledTopologyCut
lookupInstalledTopologyCut cut state = Map.lookup cut state.installedCuts

-- | Reconstruct the exact dynamic structural projection covered by one
-- installed all-member cut.  The current reconciliation cache may be ahead;
-- alignment/context derivation must therefore use this historical projection
-- rather than the live Graph leaves.
structuralProjectionAtInstalledCut ::
  Reconciliation.ReconciliationViews ->
  TopologyCutId ->
  StructuralProgressState ->
  Either StructuralProgressProblem Reconciliation.StructuralProjectionSnapshot
structuralProjectionAtInstalledCut views cutId state = do
  installed <-
    maybe
      (Left (StructuralProgressSourceTopologyUnavailable cutId))
      Right
      (lookupInstalledTopologyCut cutId state)
  let frontier = topologyCutFrontier (installedTopologyCutCut installed)
      vector = topologyFrontierStructuralVersionVector frontier
      reconciliationAtCut =
        Reconciliation.rebaseStructuralAppliedMembership
          vector
          state.reconciliation
  projected <-
    mapLeft
      StructuralProgressReconciliationProblem
      ( Reconciliation.prepareStructuralProjectionAtAdmittedVector
          views
          vector
          (topologyFrontierAppliedControlPrefix frontier)
          reconciliationAtCut
      )
  case projected of
    Left dependencies ->
      Left (StructuralProgressTopologyProjectionHeld dependencies)
    Right snapshot -> Right snapshot

topologyCutNewlyCoveredOccurrences ::
  TopologyCutId -> StructuralProgressState -> [StructuralOccurrenceId]
topologyCutNewlyCoveredOccurrences cut state =
  maybe [] (.newlyCoveredOccurrences) (Map.lookup cut state.installedCuts)

data StructuralControlAdvance = StructuralControlAdvance
  { occurrence :: StructuralOccurrenceId,
    predecessor :: StructuralVersionVector,
    successor :: StructuralVersionVector
  }
  deriving stock (Eq, Show)

structuralControlAdvanceOccurrence ::
  StructuralControlAdvance -> StructuralOccurrenceId
structuralControlAdvanceOccurrence advance = advance.occurrence

structuralControlAdvancePredecessor ::
  StructuralControlAdvance -> StructuralVersionVector
structuralControlAdvancePredecessor advance = advance.predecessor

structuralControlAdvanceSuccessor ::
  StructuralControlAdvance -> StructuralVersionVector
structuralControlAdvanceSuccessor advance = advance.successor

data PreparedStructuralApplication = PreparedStructuralApplication
  { graphSuccessor :: Graph.State,
    progressSuccessor :: StructuralProgressState,
    advance :: StructuralControlAdvance
  }

prepareStructuralApplication ::
  StructuralOccurrenceStamp ->
  TopologyCutId ->
  Reconciliation.PreparedStructuralReconciliation ->
  Graph.State ->
  StructuralProgressState ->
  Either StructuralProgressProblem PreparedStructuralApplication
prepareStructuralApplication stamp sourceTopology prepared graph state = do
  if Reconciliation.preparedStructuralPredecessor prepared == state.reconciliation
    then Right ()
    else Left StructuralProgressPreparedAgainstDifferentState
  application <-
    maybe
      (Left StructuralProgressPreparedWithoutApplication)
      Right
      (Reconciliation.preparedStructuralApplication prepared)
  role <-
    maybe
      (Left StructuralProgressPreparedWithoutApplication)
      Right
      (Reconciliation.preparedStructuralObservedCarrierRole prepared)
  let occurrence = structuralOccurrenceStampOccurrence stamp
      applicationOccurrence =
        Reconciliation.structuralApplicationOccurrence application
      predecessor = structuralOccurrenceStampPredecessor stamp
      publication = structuralOccurrenceStampPublication stamp
      applicationPublication =
        checkedPublicationId
          (Reconciliation.structuralApplicationPublication application)
      retained =
        AppliedStructuralOccurrence
          { stamp,
            sourceTopologyPrerequisite = sourceTopology,
            carrierSort = Reconciliation.structuralApplicationCarrierSort application,
            controlPrerequisite =
              Reconciliation.structuralApplicationControlPrerequisite application
          }
  if occurrence == applicationOccurrence
    then Right ()
    else Left (StructuralProgressOccurrenceMismatch applicationOccurrence occurrence)
  if predecessor == Reconciliation.structuralApplicationPredecessor application
    then Right ()
    else Left StructuralProgressPredecessorMismatch
  if publication == applicationPublication
    then Right ()
    else Left (StructuralProgressPublicationMismatch applicationPublication publication)
  if structuralOccurrenceStampCarrierRole stamp == role
    then Right ()
    else Left StructuralProgressCarrierRoleMismatch
  if isInstalledCut sourceTopology state
    then Right ()
    else Left (StructuralProgressSourceTopologyUnavailable sourceTopology)
  if state.appliedControlPrefix >= retained.controlPrerequisite
    then Right ()
    else
      Left
        ( StructuralProgressControlPrerequisiteNotCovered
            state.appliedControlPrefix
            retained.controlPrerequisite
        )
  validateRetirementSuppressionControl occurrence prepared state
  case Map.lookup occurrence state.history of
    Just incumbent
      | incumbent == retained -> do
          baselinePrepared <-
            mapLeft
              StructuralProgressGraphBaselineProblem
              (Graph.prepareStructuralGraphBaseline state.reconciliation graph)
          let seededGraph = Graph.commitStructuralGraphBaseline baselinePrepared
          graphPrepared <-
            mapLeft
              StructuralProgressGraphProblem
              ( Graph.prepareStructuralGraphPatch
                  (Reconciliation.preparedStructuralGraphPatch prepared)
                  seededGraph
              )
          let graphSuccessor = Graph.commitStructuralGraphPatch graphPrepared
              advance = StructuralControlAdvance occurrence state.appliedVector state.appliedVector
          Right (PreparedStructuralApplication graphSuccessor state advance)
      | otherwise -> Left (StructuralProgressOccurrenceConflict occurrence)
    Nothing -> do
      if Reconciliation.preparedStructuralClassification prepared
        == Reconciliation.StructuralProjectionRefresh
        then Left StructuralProgressRefreshCannotAdvanceVector
        else Right ()
      if structuralVersionVectorCovers state.appliedVector predecessor
        then Right ()
        else Left StructuralProgressPredecessorNotCovered
      sourcePrefix <-
        maybe
          (Left (StructuralProgressSourceComponentMissing source))
          Right
          (structuralVersionVectorComponent source state.appliedVector)
      if nextAfterStructuralPrefix sourcePrefix
        == structuralOccurrenceSourceSequence occurrence
        then Right ()
        else Left StructuralProgressSourceGap
      successorVector <- advanceOccurrence occurrence state
      baselinePrepared <-
        mapLeft
          StructuralProgressGraphBaselineProblem
          (Graph.prepareStructuralGraphBaseline state.reconciliation graph)
      let seededGraph = Graph.commitStructuralGraphBaseline baselinePrepared
      graphPrepared <-
        mapLeft
          StructuralProgressGraphProblem
          ( Graph.prepareStructuralGraphPatch
              (Reconciliation.preparedStructuralGraphPatch prepared)
              seededGraph
          )
      let graphSuccessor = Graph.commitStructuralGraphPatch graphPrepared
          successor =
            updateLocalReport
              ( refreshCurrentNablaProjections
                  state
                    { reconciliation = Reconciliation.preparedStructuralSuccessor prepared,
                      appliedVector = successorVector,
                      history = Map.insert occurrence retained state.history
                    }
              )
          advance = StructuralControlAdvance occurrence state.appliedVector successorVector
      Right (PreparedStructuralApplication graphSuccessor successor advance)
  where
    source =
      structuralOccurrenceSourceHeraldEpoch
        (structuralOccurrenceStampOccurrence stamp)

-- | Commit one immutable predecessor-generation survivor stamp against the
-- successor-generation live-ahead cursor authenticated by terminal-source
-- lineage. The retained occurrence keeps the old stamp byte-for-byte; only the
-- applied progress coordinate advances in the successor generation.
prepareCarriedStructuralApplication ::
  CarriedStampedSurvivor ->
  TopologyCutId ->
  Reconciliation.PreparedStructuralReconciliation ->
  Graph.State ->
  StructuralProgressState ->
  Either StructuralProgressProblem PreparedStructuralApplication
prepareCarriedStructuralApplication carried sourceTopology prepared graph state = do
  if Reconciliation.preparedStructuralPredecessor prepared == state.reconciliation
    then Right ()
    else Left StructuralProgressPreparedAgainstDifferentState
  application <-
    maybe
      (Left StructuralProgressPreparedWithoutApplication)
      Right
      (Reconciliation.preparedStructuralApplication prepared)
  role <-
    maybe
      (Left StructuralProgressPreparedWithoutApplication)
      Right
      (Reconciliation.preparedStructuralObservedCarrierRole prepared)
  let stamp = carriedStampedSurvivorStamp carried
      occurrence = structuralOccurrenceStampOccurrence stamp
      applicationOccurrence =
        Reconciliation.structuralApplicationOccurrence application
      historicalPredecessor = structuralOccurrenceStampPredecessor stamp
      applicationPublication =
        checkedPublicationId
          (Reconciliation.structuralApplicationPublication application)
      retained =
        AppliedStructuralOccurrence
          { stamp,
            sourceTopologyPrerequisite = sourceTopology,
            carrierSort = Reconciliation.structuralApplicationCarrierSort application,
            controlPrerequisite =
              Reconciliation.structuralApplicationControlPrerequisite application
          }
  if occurrence == applicationOccurrence
    then Right ()
    else Left (StructuralProgressOccurrenceMismatch applicationOccurrence occurrence)
  if historicalPredecessor
    == Reconciliation.structuralApplicationPredecessor application
    then Right ()
    else Left StructuralProgressPredecessorMismatch
  if structuralOccurrenceStampPublication stamp == applicationPublication
    then Right ()
    else
      Left
        ( StructuralProgressPublicationMismatch
            applicationPublication
            (structuralOccurrenceStampPublication stamp)
        )
  if structuralOccurrenceStampCarrierRole stamp == role
    then Right ()
    else Left StructuralProgressCarrierRoleMismatch
  if isInstalledCut sourceTopology state
    then Right ()
    else Left (StructuralProgressSourceTopologyUnavailable sourceTopology)
  if state.appliedControlPrefix >= retained.controlPrerequisite
    then Right ()
    else
      Left
        ( StructuralProgressControlPrerequisiteNotCovered
            state.appliedControlPrefix
            retained.controlPrerequisite
        )
  validateRetirementSuppressionControl occurrence prepared state
  if carriedStampedSurvivorPredecessor carried == state.appliedVector
    then Right ()
    else Left StructuralProgressPredecessorMismatch
  case Map.lookup occurrence state.history of
    Just incumbent
      | incumbent == retained -> do
          baselinePrepared <-
            mapLeft
              StructuralProgressGraphBaselineProblem
              (Graph.prepareStructuralGraphBaseline state.reconciliation graph)
          let seededGraph = Graph.commitStructuralGraphBaseline baselinePrepared
          graphPrepared <-
            mapLeft
              StructuralProgressGraphProblem
              ( Graph.prepareStructuralGraphPatch
                  (Reconciliation.preparedStructuralGraphPatch prepared)
                  seededGraph
              )
          let graphSuccessor = Graph.commitStructuralGraphPatch graphPrepared
              advance =
                StructuralControlAdvance occurrence state.appliedVector state.appliedVector
          Right (PreparedStructuralApplication graphSuccessor state advance)
      | otherwise -> Left (StructuralProgressOccurrenceConflict occurrence)
    Nothing -> do
      if Reconciliation.preparedStructuralClassification prepared
        == Reconciliation.StructuralProjectionRefresh
        then Left StructuralProgressRefreshCannotAdvanceVector
        else Right ()
      baselinePrepared <-
        mapLeft
          StructuralProgressGraphBaselineProblem
          (Graph.prepareStructuralGraphBaseline state.reconciliation graph)
      let seededGraph = Graph.commitStructuralGraphBaseline baselinePrepared
      graphPrepared <-
        mapLeft
          StructuralProgressGraphProblem
          ( Graph.prepareStructuralGraphPatch
              (Reconciliation.preparedStructuralGraphPatch prepared)
              seededGraph
          )
      let successorVector = carriedStampedSurvivorVector carried
          graphSuccessor = Graph.commitStructuralGraphPatch graphPrepared
          successor =
            updateLocalReport
              ( refreshCurrentNablaProjections
                  state
                    { reconciliation = Reconciliation.preparedStructuralSuccessor prepared,
                      appliedVector = successorVector,
                      history = Map.insert occurrence retained state.history
                    }
              )
          advance = StructuralControlAdvance occurrence state.appliedVector successorVector
      mapLeft StructuralProgressInvariantFault (validateStructuralProgressState successor)
      Right (PreparedStructuralApplication graphSuccessor successor advance)

preparedStructuralControlAdvance ::
  PreparedStructuralApplication -> StructuralControlAdvance
preparedStructuralControlAdvance prepared = prepared.advance

commitStructuralApplication ::
  PreparedStructuralApplication ->
  (Graph.State, StructuralProgressState, StructuralControlAdvance)
commitStructuralApplication prepared =
  (prepared.graphSuccessor, prepared.progressSuccessor, prepared.advance)

validateRetirementSuppressionControl ::
  StructuralOccurrenceId ->
  Reconciliation.PreparedStructuralReconciliation ->
  StructuralProgressState ->
  Either StructuralProgressProblem ()
validateRetirementSuppressionControl occurrence prepared state =
  case Reconciliation.structuralAppliedRetirementSuppressionAt
    occurrence
    (Reconciliation.preparedStructuralSuccessor prepared) of
    Nothing -> Right ()
    Just suppression
      | floorIndex <= state.appliedControlPrefix -> Right ()
      | otherwise ->
          Left
            ( StructuralProgressControlPrerequisiteNotCovered
                state.appliedControlPrefix
                floorIndex
            )
      where
        floorIndex =
          Reconciliation.structuralRetirementSuppressionControlIndex suppression

data PreparedStructuralProjectionRefresh
  = PreparedStructuralProjectionRefresh Graph.State StructuralProgressState

-- | Exact-apply a control-qualified reconciliation refresh without inventing
-- a structural dot or changing the applied vector.  The Graph and
-- reconciliation successors are prepared and committed as one owner cut.
prepareStructuralProjectionRefresh ::
  Reconciliation.PreparedStructuralReconciliation ->
  Graph.State ->
  StructuralProgressState ->
  Either StructuralProgressProblem PreparedStructuralProjectionRefresh
prepareStructuralProjectionRefresh prepared graph state = do
  if Reconciliation.preparedStructuralPredecessor prepared == state.reconciliation
    then Right ()
    else Left StructuralProgressPreparedAgainstDifferentState
  if Reconciliation.preparedStructuralClassification prepared
    == Reconciliation.StructuralProjectionRefresh
    then Right ()
    else Left StructuralProgressRefreshCannotAdvanceVector
  baselinePrepared <-
    mapLeft
      StructuralProgressGraphBaselineProblem
      (Graph.prepareStructuralGraphBaseline state.reconciliation graph)
  graphPrepared <-
    mapLeft
      StructuralProgressGraphProblem
      ( Graph.prepareStructuralGraphPatch
          (Reconciliation.preparedStructuralGraphPatch prepared)
          (Graph.commitStructuralGraphBaseline baselinePrepared)
      )
  let graphSuccessor = Graph.commitStructuralGraphPatch graphPrepared
      progressSuccessor
        | Reconciliation.preparedStructuralChangesState prepared =
            updateLocalReport
              ( refreshCurrentNablaProjections
                  state
                    { reconciliation = Reconciliation.preparedStructuralSuccessor prepared
                    }
              )
        | otherwise = state
  Right (PreparedStructuralProjectionRefresh graphSuccessor progressSuccessor)

commitStructuralProjectionRefresh ::
  PreparedStructuralProjectionRefresh -> (Graph.State, StructuralProgressState)
commitStructuralProjectionRefresh
  (PreparedStructuralProjectionRefresh graph progress) = (graph, progress)

data StructuralControlProgress
  = StructuralControlAdvanced
  | StructuralControlUnchanged
  deriving stock (Eq, Ord, Show)

data PreparedStructuralControlProgress = PreparedStructuralControlProgress
  { successor :: StructuralProgressState,
    classification :: StructuralControlProgress
  }

prepareStructuralControlProgress ::
  ControlIndex ->
  StructuralProgressState ->
  Either StructuralProgressProblem PreparedStructuralControlProgress
prepareStructuralControlProgress prefix state
  | prefix < state.appliedControlPrefix =
      Left
        ( StructuralProgressControlRegression
            state.appliedControlPrefix
            prefix
        )
  | prefix == state.appliedControlPrefix =
      Right (PreparedStructuralControlProgress state StructuralControlUnchanged)
  | otherwise =
      Right
        ( PreparedStructuralControlProgress
            (updateLocalReport advanced {proposalProjection = state.proposalProjection})
            StructuralControlAdvanced
        )
  where
    -- A cursor advance releases control waiters, but changes projection queries
    -- only when it crosses a retained structural consequence. Reconciliation
    -- and the installed frontier are identical on this transition.
    advanced
      | Reconciliation.structuralProjectionControlCoordinate prefix state.reconciliation
          == Reconciliation.structuralProjectionControlCoordinate state.appliedControlPrefix state.reconciliation =
          state {appliedControlPrefix = prefix, preparationReadinessChanged = True}
      | otherwise = refreshCurrentNablaProjections state {appliedControlPrefix = prefix}

commitStructuralControlProgress ::
  PreparedStructuralControlProgress ->
  (StructuralProgressState, StructuralControlProgress)
commitStructuralControlProgress prepared =
  (prepared.successor, prepared.classification)

data StructuralReportClassification
  = StructuralReportAdvanced
  | StructuralReportDuplicate
  | StructuralReportStale
  | StructuralReportHistoricalStale
  deriving stock (Eq, Ord, Show)

data PreparedStructuralReport = PreparedStructuralReport
  { successor :: StructuralProgressState,
    classification :: StructuralReportClassification
  }

prepareStructuralReport ::
  StructuralAppliedReport ->
  StructuralProgressState ->
  Either StructuralProgressProblem PreparedStructuralReport
prepareStructuralReport report state
  | exactActivePredecessorReport report state =
      Right
        ( PreparedStructuralReport
            state
            StructuralReportHistoricalStale
        )
  | otherwise = do
      validateReportShape report state
      let reporter = structuralAppliedReportReporter report
      if reporter == state.localHerald
        then
          if report == structuralLocalReport state
            then Right (PreparedStructuralReport state StructuralReportDuplicate)
            else Left (StructuralProgressIncomparableReport reporter)
        else case Map.lookup reporter state.reports of
          Nothing ->
            Right
              ( PreparedStructuralReport
                  state {reports = Map.insert reporter report state.reports}
                  StructuralReportAdvanced
              )
          Just incumbent -> compareWith incumbent
  where
    compareWith incumbent
      | report == incumbent =
          Right (PreparedStructuralReport state StructuralReportDuplicate)
      | reportCovers report incumbent =
          Right
            ( PreparedStructuralReport
                state
                  { reports =
                      Map.insert
                        (structuralAppliedReportReporter report)
                        report
                        state.reports
                  }
                StructuralReportAdvanced
            )
      | reportCovers incumbent report =
          Right (PreparedStructuralReport state StructuralReportStale)
      | otherwise =
          Left
            ( StructuralProgressIncomparableReport
                (structuralAppliedReportReporter report)
            )

commitStructuralReport ::
  PreparedStructuralReport ->
  (StructuralProgressState, StructuralReportClassification)
commitStructuralReport prepared = (prepared.successor, prepared.classification)

structuralStableFrontier :: StructuralProgressState -> Maybe TopologyFrontier
structuralStableFrontier state = do
  reports <-
    traverse
      (`Map.lookup` state.reports)
      (NonEmpty.toList (currentMembers state))
  vector <-
    componentwiseMinimum
      state.currentMembership
      (fmap structuralAppliedReportVersionVector reports)
  let controlPrefix = minimum (fmap structuralAppliedReportControlPrefix reports)
  pure (topologyFrontier vector controlPrefix)

updateLocalReport :: StructuralProgressState -> StructuralProgressState
updateLocalReport state =
  if state.joiningObserver
    then state
    else
      state
        { reports =
            Map.insert
              state.localHerald
              ( structuralAppliedReport
                  state.localHerald
                  state.appliedVector
                  state.appliedControlPrefix
              )
              state.reports
        }

advanceOccurrence ::
  StructuralOccurrenceId ->
  StructuralProgressState ->
  Either StructuralProgressProblem StructuralVersionVector
advanceOccurrence occurrence state =
  mapLeft
    StructuralProgressVectorProblem
    ( mkStructuralVersionVector
        state.currentMembership
        [ if herald == source
            then (herald, structuralPrefixThrough sequenceNumber)
            else (herald, prefix)
        | (herald, prefix) <- structuralVersionVectorEntries state.appliedVector
        ]
    )
  where
    source = structuralOccurrenceSourceHeraldEpoch occurrence
    sequenceNumber = structuralOccurrenceSourceSequence occurrence

validateReportShape ::
  StructuralAppliedReport ->
  StructuralProgressState ->
  Either StructuralProgressProblem ()
validateReportShape report state = do
  let reporter = structuralAppliedReportReporter report
  if Set.member reporter (memberSet state)
    then Right ()
    else Left (StructuralProgressForeignReporter reporter)
  if structuralAppliedReportMembershipGenerationId report
    == currentMembershipGenerationId state
    then Right ()
    else
      Left
        ( StructuralProgressWrongMembershipGeneration
            (currentMembershipGenerationId state)
            (structuralAppliedReportMembershipGenerationId report)
        )
  if structuralAppliedReportMemberSetDigest report == currentMemberSetDigest state
    then Right ()
    else
      Left
        ( StructuralProgressWrongMemberSet
            (currentMemberSetDigest state)
            (structuralAppliedReportMemberSetDigest report)
        )
  if vectorMembers (structuralAppliedReportVersionVector report) == memberSet state
    then Right ()
    else Left (StructuralProgressReportVectorMembershipMismatch reporter)

-- | Admit a well-shaped report from a current survivor at a retained ancestor
-- connected to the current generation by actually installed membership bases.
-- A delayed reconnect catalogue can cross several retirements; ancestry alone
-- and retired reporters remain insufficient. Do not compare the report to a
-- union's least terminal vector/control coordinate: included/ahead claims and
-- later Oracle application can legitimately advance a survivor still reporting
-- an older generation. The accepted observation grants no authority and is
-- discarded.
exactActivePredecessorReport ::
  StructuralAppliedReport -> StructuralProgressState -> Bool
exactActivePredecessorReport report state =
  case exactPredecessorMembership claimed state of
    Nothing -> False
    Just predecessor ->
      Set.member reporter (memberSet state)
        && reportShapeValidFor predecessor report
  where
    claimed = structuralAppliedReportMembershipGenerationId report
    reporter = structuralAppliedReportReporter report

-- | Acceptances carry a cut identity in addition to their report.  A late
-- predecessor-generation acceptance is stale only when that identity is
-- backed by exact retained evidence: either the acceptance itself belongs to
-- an installed predecessor cut, or its cut is the checked open-cut tombstone
-- recorded by the atomic membership transition.
exactActivePredecessorAcceptance ::
  TopologyCutAcceptance -> StructuralProgressState -> Bool
exactActivePredecessorAcceptance acceptance state =
  exactActivePredecessorReport report state
    && ( exactRetainedPredecessorAcceptance acceptance state
           || exactSupersededPredecessorAcceptance acceptance state
       )
  where
    report = topologyCutAcceptanceReport acceptance

exactRetainedPredecessorAcceptance ::
  TopologyCutAcceptance -> StructuralProgressState -> Bool
exactRetainedPredecessorAcceptance acceptance state =
  case Map.lookup cutId state.installedCuts of
    Nothing -> False
    Just installed ->
      cutGenerationId installed == claimed
        && find
          ( (== reporter)
              . topologyCutAcceptanceReporter
          )
          (topologyCutEstablishedAcceptances installed.installedEstablished)
          == Just acceptance
  where
    cutId = topologyCutAcceptanceId acceptance
    report = topologyCutAcceptanceReport acceptance
    claimed = structuralAppliedReportMembershipGenerationId report
    reporter = topologyCutAcceptanceReporter acceptance

-- The report of an acceptance that was genuinely in flight need not already
-- be present in the abandoned open cut.  Its admissible coordinate is still
-- exact: it must cover that cut's retained frontier.  Like a plain predecessor
-- report, its vector/control may legitimately run ahead while the sender is
-- still awaiting its membership rebase.  The tombstoned cut identity is the
-- authority boundary, and the accepted message is owner-inert.
exactSupersededPredecessorAcceptance ::
  TopologyCutAcceptance -> StructuralProgressState -> Bool
exactSupersededPredecessorAcceptance acceptance state =
  case Map.lookup cutId state.supersededOpenCuts of
    Nothing -> False
    Just tombstone ->
      let frontier = topologyCutFrontier (topologyCutAnnounceCut tombstone.announce)
       in topologyCutAcceptanceId acceptance
            == topologyCutAnnounceId tombstone.announce
            && reportCoversFrontier report frontier
  where
    cutId = topologyCutAcceptanceId acceptance
    report = topologyCutAcceptanceReport acceptance

cutGenerationId :: InstalledTopologyCut -> HeraldMembershipGenerationId
cutGenerationId installed =
  structuralVersionVectorMembershipGenerationId
    ( topologyFrontierStructuralVersionVector
        (topologyCutFrontier (installedTopologyCutCut installed))
    )

exactInstalledPredecessorAcknowledgement ::
  TopologyCutEstablishedAck -> StructuralProgressState -> Bool
exactInstalledPredecessorAcknowledgement acknowledgement state =
  case exactPredecessorMembership claimed state of
    Nothing -> False
    Just predecessor ->
      Set.member reporter (memberSet state)
        && acknowledgement
          == topologyCutEstablishedAck cutId reporter (heraldMembershipGenerationId predecessor)
        && case Map.lookup cutId state.installedCuts of
          Nothing -> False
          Just installed ->
            structuralVersionVectorMembershipGenerationId
              ( topologyFrontierStructuralVersionVector
                  ( topologyCutFrontier
                      (topologyCutEstablishedCut installed.installedEstablished)
                  )
              )
              == claimed
  where
    claimed = topologyCutEstablishedAckMembershipGenerationId acknowledgement
    reporter = topologyCutEstablishedAckReporter acknowledgement
    cutId = topologyCutEstablishedAckId acknowledgement

-- | A membership base is installed atomically from terminal-union or admission
-- evidence and therefore has no ordinary acknowledgement collection phase. A
-- survivor that learns the exact installed base during reconnect nevertheless
-- returns the deterministic acknowledgement.  Recognise that acknowledgement
-- as stale evidence rather than treating the intentionally absent retained ack
-- as a conflict.
exactInstalledSuccessorBaseAcknowledgement ::
  TopologyCutEstablishedAck -> StructuralProgressState -> Bool
exactInstalledSuccessorBaseAcknowledgement acknowledgement state =
  claimed == currentMembershipGenerationId state
    && Set.member reporter (memberSet state)
    && acknowledgement == topologyCutEstablishedAck cutId reporter claimed
    && case Map.lookup cutId state.installedCuts of
      Just installed ->
        case (installed.membershipSuccessorBase, installed.membershipAdmissionBase) of
          (Just base, _) -> successorStructuralBaseGenerationId base == claimed
          (_, Just (_, record)) -> case Admission.admissionRecordPhase record of
            Admission.AdmissionActivated _ generation -> heraldMembershipGenerationId generation == claimed
            _ -> False
          _ -> False
      Nothing -> False
  where
    claimed = topologyCutEstablishedAckMembershipGenerationId acknowledgement
    reporter = topologyCutEstablishedAckReporter acknowledgement
    cutId = topologyCutEstablishedAckId acknowledgement

exactPredecessorMembership ::
  HeraldMembershipGenerationId ->
  StructuralProgressState ->
  Maybe HeraldMembershipGeneration
exactPredecessorMembership claimed state = do
  _ <- structuralDescendantSettlement claimed (currentMembershipGenerationId state) state
  if claimed /= currentMembershipGenerationId state then Map.lookup claimed state.membershipHistory else Nothing

reportShapeValidFor ::
  HeraldMembershipGeneration -> StructuralAppliedReport -> Bool
reportShapeValidFor membership report =
  let expectedMembers = membershipSetFor membership
      expectedGeneration = heraldMembershipGenerationId membership
      expectedDigest =
        heraldMembershipGenerationActiveMemberSetDigest membership
   in Set.member (structuralAppliedReportReporter report) expectedMembers
        && structuralAppliedReportMembershipGenerationId report
          == expectedGeneration
        && structuralAppliedReportMemberSetDigest report == expectedDigest
        && vectorMembers (structuralAppliedReportVersionVector report) == expectedMembers

reportCovers :: StructuralAppliedReport -> StructuralAppliedReport -> Bool
reportCovers covering required =
  structuralVersionVectorCovers
    (structuralAppliedReportVersionVector covering)
    (structuralAppliedReportVersionVector required)
    && structuralAppliedReportControlPrefix covering
      >= structuralAppliedReportControlPrefix required

componentwiseMinimum ::
  HeraldMembershipGeneration ->
  [StructuralVersionVector] ->
  Maybe StructuralVersionVector
componentwiseMinimum _ [] = Nothing
componentwiseMinimum membership vectors =
  either
    (const Nothing)
    Just
    ( mkStructuralVersionVector
        membership
        [ (member, minimum prefixes)
        | member <-
            NonEmpty.toList
              (heraldMembershipGenerationActiveHeraldEpochs membership),
          let prefixes =
                [ prefix
                | vector <- vectors,
                  Just prefix <- [structuralVersionVectorComponent member vector]
                ],
          length prefixes == length vectors
        ]
    )

vectorMembers :: StructuralVersionVector -> Set HeraldEpoch
vectorMembers = Set.fromList . fmap fst . structuralVersionVectorEntries

vectorComponentMatchesHistory :: StructuralProgressState -> HeraldEpoch -> Bool
vectorComponentMatchesHistory state source =
  case structuralVersionVectorComponent source state.appliedVector of
    Nothing -> False
    Just prefix ->
      let expected =
            maybe 0 structuralSequenceWord64 (structuralPrefixSequence prefix)
          retained =
            [ structuralSequenceWord64
                (structuralOccurrenceSourceSequence occurrence)
            | occurrence <- Map.keys state.history,
              structuralOccurrenceSourceHeraldEpoch occurrence == source
            ]
       in fromIntegral (length retained) == expected
            && case retained of
              [] -> expected == 0
              present -> minimum present == 1 && maximum present == expected

memberSet :: StructuralProgressState -> Set HeraldEpoch
memberSet = membershipSetFor . (.currentMembership)

membershipSetFor :: HeraldMembershipGeneration -> Set HeraldEpoch
membershipSetFor =
  Set.fromList
    . NonEmpty.toList
    . heraldMembershipGenerationActiveHeraldEpochs

isInstalledCut :: TopologyCutId -> StructuralProgressState -> Bool
isInstalledCut cut state =
  cut == state.genesisCutId || Map.member cut state.installedCuts

mapLeft :: (problem -> other) -> Either problem value -> Either other value
mapLeft f = either (Left . f) Right

data DynamicVertexInstallation = DynamicVertexInstallation
  { vertex :: VertexId,
    occurrence :: StructuralOccurrenceId,
    publication :: PublicationId,
    controller :: Reconciliation.ControllerProjection,
    sort :: SortOccurrence,
    earliestCut :: TopologyCutId,
    currentCut :: TopologyCutId
  }
  deriving stock (Eq, Show)

dynamicVertexInstallationVertex :: DynamicVertexInstallation -> VertexId
dynamicVertexInstallationVertex installation = installation.vertex

dynamicVertexInstallationOccurrence ::
  DynamicVertexInstallation -> StructuralOccurrenceId
dynamicVertexInstallationOccurrence installation = installation.occurrence

dynamicVertexInstallationPublication ::
  DynamicVertexInstallation -> PublicationId
dynamicVertexInstallationPublication installation = installation.publication

dynamicVertexInstallationController ::
  DynamicVertexInstallation -> Reconciliation.ControllerProjection
dynamicVertexInstallationController installation = installation.controller

dynamicVertexInstallationSort :: DynamicVertexInstallation -> SortOccurrence
dynamicVertexInstallationSort installation = installation.sort

dynamicVertexInstallationEarliestCut ::
  DynamicVertexInstallation -> TopologyCutId
dynamicVertexInstallationEarliestCut installation = installation.earliestCut

dynamicVertexInstallationCurrentCut ::
  DynamicVertexInstallation -> TopologyCutId
dynamicVertexInstallationCurrentCut installation = installation.currentCut

-- | A batch binds the current graph and progress owners. The graph already owns
-- the exact current vertex index; historical authority preparation is shared by
-- the progress owner until its structural or control coordinate changes.
data PreparedStructuralQueries
  = PreparedStructuralQueries
      StructuralProgressState
      (Map GlobalObjectId Reconciliation.StructuralVertexProjection)
      (Either StructuralProgressProblem Reconciliation.PreparedNablaProjections)

prepareStructuralQueries :: Graph.State -> StructuralProgressState -> PreparedStructuralQueries
prepareStructuralQueries graph state =
  PreparedStructuralQueries
    state
    (Graph.graphStructuralVertexProjections graph)
    (derivedValue state.currentNablaProjections)

preparedStructuralProgress :: PreparedStructuralQueries -> StructuralProgressState
preparedStructuralProgress (PreparedStructuralQueries state _ _) = state

lookupPreparedDynamicNabla ::
  NablaId -> PreparedStructuralQueries -> Maybe DynamicVertexInstallation
lookupPreparedDynamicNabla nabla (PreparedStructuralQueries state vertices _) = do
  projection <- Map.lookup (globalObjectIdFromNablaId nabla) vertices
  unless (Reconciliation.structuralVertexProjectionVertex projection == NablaVertex nabla) Nothing
  installedDynamicVertexFromProjection state projection

lookupPreparedDynamicDelta ::
  DeltaId -> PreparedStructuralQueries -> Maybe DynamicVertexInstallation
lookupPreparedDynamicDelta delta (PreparedStructuralQueries state vertices _) = do
  projection <- Map.lookup (globalObjectIdFromDeltaId delta) vertices
  unless (Reconciliation.structuralVertexProjectionVertex projection == DeltaVertex delta) Nothing
  installedDynamicVertexFromProjection state projection

lookupPreparedCurrentNablaProjection ::
  NablaId ->
  PreparedStructuralQueries ->
  Either StructuralProgressProblem (Maybe Reconciliation.HistoricalNablaProjection)
lookupPreparedCurrentNablaProjection nabla (PreparedStructuralQueries _ _ prepared) = do
  projections <- prepared
  mapLeft StructuralProgressReconciliationProblem (Reconciliation.lookupPreparedNablaProjection nabla projections)

lookupInstalledDynamicNabla ::
  NablaId -> StructuralProgressState -> Maybe DynamicVertexInstallation
lookupInstalledDynamicNabla nabla =
  lookupInstalledDynamicVertex (NablaVertex nabla)

lookupInstalledDynamicDelta ::
  DeltaId -> StructuralProgressState -> Maybe DynamicVertexInstallation
lookupInstalledDynamicDelta delta =
  lookupInstalledDynamicVertex (DeltaVertex delta)

-- | Bind the authority-relevant retained Nabla projection to an exact
-- installed topology/control coordinate.  A control prerequisite may be
-- newer than the topology cut when an authority-only label release leaves the
-- topology bytes unchanged, but it may never precede the cut's own control
-- frontier.
structuralNablaProjectionAtCoordinate ::
  NablaId ->
  TopologyCutId ->
  ControlIndex ->
  StructuralProgressState ->
  Either StructuralProgressProblem (Maybe Reconciliation.HistoricalNablaProjection)
structuralNablaProjectionAtCoordinate nabla cut controlPrefix state = do
  if controlPrefix <= state.appliedControlPrefix
    then Right ()
    else
      Left
        ( StructuralProgressControlPrerequisiteNotCovered
            state.appliedControlPrefix
            controlPrefix
        )
  frontier <-
    if cut == state.genesisCutId
      then
        Right
          ( topologyFrontier
              (emptyStructuralVersionVector (genesisMembershipInvariant state))
              (controlIndex 0)
          )
      else
        maybe
          (Left (StructuralProgressSourceTopologyUnavailable cut))
          (Right . topologyCutFrontier . installedTopologyCutCut)
          (Map.lookup cut state.installedCuts)
  let cutControl = topologyFrontierAppliedControlPrefix frontier
  if controlPrefix >= cutControl
    then Right ()
    else
      Left
        ( StructuralProgressTopologyControlCoordinateMismatch
            cut
            cutControl
            controlPrefix
        )
  let vector = topologyFrontierStructuralVersionVector frontier
      reconciliationAtCut =
        Reconciliation.rebaseStructuralAppliedMembership
          vector
          state.reconciliation
  mapLeft
    StructuralProgressReconciliationProblem
    ( Reconciliation.structuralNablaProjectionAt
        nabla
        vector
        controlPrefix
        reconciliationAtCut
    )

-- These strict input records deliberately exclude the progress owner and its
-- previous derived value. A deferred reconstruction retains only the current
-- reconciliation owner and one selected frontier, never a chain of predecessor
-- progress records. The expensive reconstruction itself remains lazy.
data CurrentNablaFrontier = CurrentNablaFrontier !StructuralVersionVector !ControlIndex

data CurrentNablaQueryInputs
  = CurrentNablaQueryInputs
      !TopologyCutId
      !ControlIndex
      !Reconciliation.StructuralAppliedState
      !(Either StructuralProgressProblem CurrentNablaFrontier)

deriveCurrentNablaProjections ::
  CurrentNablaQueryInputs ->
  Derived (Either StructuralProgressProblem Reconciliation.PreparedNablaProjections)
deriveCurrentNablaProjections inputs =
  inputs `seq` derive (prepareCurrentNablaProjections inputs)

-- Rebuild after reconciliation, a control advance across a retained consequence,
-- or the latest installed frontier changes. Reports and pending/open-cut
-- bookkeeping preserve this field. In particular an acknowledgement update to
-- an installed cut cannot change its immutable frontier.
refreshCurrentNablaProjections :: StructuralProgressState -> StructuralProgressState
refreshCurrentNablaProjections state@StructuralProgressState {reconciliation, appliedControlPrefix, lastInstalledCutId, genesisCutId, membershipHistory, installedCuts} =
  let frontier =
        if lastInstalledCutId == genesisCutId
          then
            let selected = CurrentNablaFrontier (emptyStructuralVersionVector (genesisMembershipFromHistoryInvariant membershipHistory)) (controlIndex 0)
             in selected `seq` Right selected
          else case Map.lookup lastInstalledCutId installedCuts of
            Nothing -> Left (StructuralProgressSourceTopologyUnavailable lastInstalledCutId)
            Just installed ->
              let retained = topologyCutFrontier (installedTopologyCutCut installed)
                  selected = CurrentNablaFrontier (topologyFrontierStructuralVersionVector retained) (topologyFrontierAppliedControlPrefix retained)
               in selected `seq` Right selected
      inputs = CurrentNablaQueryInputs lastInstalledCutId appliedControlPrefix reconciliation frontier
   in inputs `seq` state {currentNablaProjections = deriveCurrentNablaProjections inputs, proposalProjection = derive Nothing, preparationReadinessChanged = True, alignmentPlanChanged = True}

prepareCurrentNablaProjections ::
  CurrentNablaQueryInputs ->
  Either StructuralProgressProblem Reconciliation.PreparedNablaProjections
prepareCurrentNablaProjections (CurrentNablaQueryInputs cut controlPrefix reconciliation selected) = do
  CurrentNablaFrontier vector cutControl <- selected
  if controlPrefix >= cutControl
    then Right ()
    else Left (StructuralProgressTopologyControlCoordinateMismatch cut cutControl controlPrefix)
  let reconciliationAtCut = Reconciliation.rebaseStructuralAppliedMembership vector reconciliation
  mapLeft
    StructuralProgressReconciliationProblem
    (Reconciliation.prepareNablaProjectionsAt vector controlPrefix reconciliationAtCut)

lookupInstalledDynamicVertex ::
  VertexId -> StructuralProgressState -> Maybe DynamicVertexInstallation
lookupInstalledDynamicVertex wanted state = do
  projection <-
    find
      ((== wanted) . Reconciliation.structuralVertexProjectionVertex)
      (Map.elems (Reconciliation.structuralAppliedVertexProjections state.reconciliation))
  installedDynamicVertexFromProjection state projection

installedDynamicVertexFromProjection ::
  StructuralProgressState -> Reconciliation.StructuralVertexProjection -> Maybe DynamicVertexInstallation
installedDynamicVertexFromProjection state projection = do
  occurrence <- Reconciliation.structuralVertexProjectionOccurrence projection
  publication <- Reconciliation.structuralVertexProjectionPublication projection
  earliest <- installedCutCoveringOccurrence occurrence state
  current <- currentInstalledCutCoveringOccurrence occurrence state
  case projection of
    Reconciliation.NablaVertexProjection _ nabla typedSort controller _ ->
      pure
        DynamicVertexInstallation
          { vertex = NablaVertex nabla,
            occurrence,
            publication,
            controller,
            sort = typedSort,
            earliestCut = earliest,
            currentCut = current
          }
    Reconciliation.DeltaVertexProjection _ delta typedSort controller _ ->
      pure
        DynamicVertexInstallation
          { vertex = DeltaVertex delta,
            occurrence,
            publication,
            controller,
            sort = typedSort,
            earliestCut = earliest,
            currentCut = current
          }
    Reconciliation.NeutralVertexProjection {} -> Nothing

installedCutCoveringOccurrence ::
  StructuralOccurrenceId ->
  StructuralProgressState ->
  Maybe TopologyCutId
installedCutCoveringOccurrence occurrence =
  installedCutCoveringCause (structuralOccurrenceCause occurrence)

installedCutCoveringCause ::
  StructuralConsequenceCause ->
  StructuralProgressState ->
  Maybe TopologyCutId
installedCutCoveringCause cause state =
  indexedInstalledCutCoveringCause cause state.installedCoverageIndex

currentInstalledCutCoveringOccurrence ::
  StructuralOccurrenceId ->
  StructuralProgressState ->
  Maybe TopologyCutId
currentInstalledCutCoveringOccurrence occurrence =
  currentInstalledCutCoveringCause (structuralOccurrenceCause occurrence)

currentInstalledCutCoveringCause ::
  StructuralConsequenceCause ->
  StructuralProgressState ->
  Maybe TopologyCutId
currentInstalledCutCoveringCause cause state
  | state.lastInstalledCutId == state.genesisCutId = Nothing
  | installedCutCoversCause state.lastInstalledCutId cause state =
      Just state.lastInstalledCutId
  | otherwise = Nothing

installedCutCoversCause ::
  TopologyCutId ->
  StructuralConsequenceCause ->
  StructuralProgressState ->
  Bool
installedCutCoversCause cutId cause state =
  indexedInstalledCutCoversCause cutId cause state.installedCoverageIndex

installedCutFrontier :: InstalledTopologyCut -> TopologyFrontier
installedCutFrontier =
  topologyCutFrontier
    . topologyCutEstablishedCut
    . installedTopologyCutEstablished

frontierCoversOccurrence :: StructuralOccurrenceId -> TopologyFrontier -> Bool
frontierCoversOccurrence occurrence =
  frontierCoversCause (structuralOccurrenceCause occurrence)

topologyCutCoversOccurrence :: StructuralOccurrenceId -> TopologyCut -> Bool
topologyCutCoversOccurrence occurrence =
  topologyCutCoversCause (structuralOccurrenceCause occurrence)

-- | Membership-successor lineage closes the terminal predecessor vector as
-- well as exposing the successor frontier.  In particular, a retired-source
-- occurrence remains covered even though its component is intentionally absent
-- from every successor-generation frontier.
topologyCutCoversCause :: StructuralConsequenceCause -> TopologyCut -> Bool
topologyCutCoversCause cause cut =
  frontierCoversCause cause (topologyCutFrontier cut)
    || case (structuralConsequenceCauseView cause, topologyPredecessorView (topologyCutPredecessor cut)) of
      ( StructuralOccurrenceCauseView occurrence,
        MembershipSuccessorPredecessorView _ _ _ terminalVector _ _
        ) ->
          vectorCoversStructuralOccurrence occurrence terminalVector
      _ -> False

frontierCoversCause :: StructuralConsequenceCause -> TopologyFrontier -> Bool
frontierCoversCause cause frontier =
  case structuralConsequenceCauseView cause of
    StructuralOccurrenceCauseView occurrence ->
      frontierCoversStructuralOccurrence occurrence frontier
    LabelReleaseCauseView _ index ->
      topologyFrontierAppliedControlPrefix frontier >= index
    ProcessEndCauseView _ index ->
      topologyFrontierAppliedControlPrefix frontier >= index
    PredefinedDisappearanceCauseView _ index ->
      topologyFrontierAppliedControlPrefix frontier >= index

frontierCoversStructuralOccurrence ::
  StructuralOccurrenceId -> TopologyFrontier -> Bool
frontierCoversStructuralOccurrence occurrence frontier =
  vectorCoversStructuralOccurrence
    occurrence
    (topologyFrontierStructuralVersionVector frontier)

vectorCoversStructuralOccurrence ::
  StructuralOccurrenceId -> StructuralVersionVector -> Bool
vectorCoversStructuralOccurrence occurrence vector =
  maybe
    False
    (>= structuralPrefixThrough sequenceNumber)
    (structuralVersionVectorComponent source vector)
  where
    source = structuralOccurrenceSourceHeraldEpoch occurrence
    sequenceNumber = structuralOccurrenceSourceSequence occurrence

data TopologyControlCheckpoint = TopologyControlCheckpoint
  { index :: ControlIndex,
    affectsTopology :: Bool,
    canonicalBytes :: ByteString,
    joinSealAdmission :: Maybe HeraldAdmissionId
  }
  deriving stock (Eq, Show)

topologyControlCheckpoint :: ControlIndex -> ByteString -> TopologyControlCheckpoint
topologyControlCheckpoint index bytes =
  TopologyControlCheckpoint index False bytes Nothing

topologyControlCheckpointWithConsequence ::
  ControlIndex -> Bool -> ByteString -> TopologyControlCheckpoint
topologyControlCheckpointWithConsequence index affects bytes = TopologyControlCheckpoint index affects bytes Nothing

-- | A join may establish an unchanged graph at a later barrier control prefix.
-- The ordinary exact old-member cut ceremony remains mandatory; this typed
-- checkpoint records why the finite join boundary itself requires a new cut.
topologyJoinSealCheckpoint :: HeraldAdmissionId -> ControlIndex -> ByteString -> TopologyControlCheckpoint
topologyJoinSealCheckpoint admission index bytes = TopologyControlCheckpoint index False bytes (Just admission)

joinCheckpointAdvances :: TopologyControlCheckpoint -> Bool
joinCheckpointAdvances checkpoint = maybe False ((<= checkpoint.index) . heraldAdmissionControlIndex) checkpoint.joinSealAdmission

topologyControlCheckpointIndex :: TopologyControlCheckpoint -> ControlIndex
topologyControlCheckpointIndex checkpoint = checkpoint.index

topologyControlCheckpointAffectsTopology :: TopologyControlCheckpoint -> Bool
topologyControlCheckpointAffectsTopology checkpoint = checkpoint.affectsTopology

topologyControlCheckpointCanonicalBytes ::
  TopologyControlCheckpoint -> ByteString
topologyControlCheckpointCanonicalBytes checkpoint = checkpoint.canonicalBytes

data TopologyCutProposalPreparation
  = TopologyCutProposalUnavailable
  | TopologyCutProposalHeld (Set Reconciliation.StructuralDependency)
  | TopologyCutProposalReady PreparedTopologyCutProposal

data PreparedTopologyCutProposal = PreparedTopologyCutProposal
  { successor :: StructuralProgressState,
    announce :: TopologyCutAnnounce,
    localAcceptance :: TopologyCutAcceptance
  }

-- One bounded memo retains fully evaluated output and compact dependency
-- coordinates, never a reconciliation/progress owner or a deferred reconstruction.
-- Every reconciliation refresh invalidates it; a pure Oracle cursor advance
-- preserves it and the effective control coordinate below selects any newly
-- covered consequence. Derived equality deliberately ignores memo population.
data TopologyProjectionKey
  = TopologyProjectionKey
      !StructuralVersionVector
      !ControlIndex
      !Reconciliation.StructuralProjectionInputs
  deriving stock (Eq)

data CachedTopologyProjection
  = CachedTopologyProjection
      !TopologyProjectionKey
      !ControlIndex
      !TopologyOccurrenceDigest

data TopologyProjectionWork
  = TopologyProjectionReused
  | TopologyProjectionReconstructed
  deriving stock (Eq, Show)

prepareTopologyCutProposal ::
  Reconciliation.ReconciliationViews ->
  TopologyControlCheckpoint ->
  StructuralProgressState ->
  Either StructuralProgressProblem TopologyCutProposalPreparation
prepareTopologyCutProposal views checkpoint state = do
  (_, _, preparation) <- prepareTopologyCutProposalUsing False views checkpoint state
  pure preparation

-- | Retain the memo even when no new cut is required. The work witness permits
-- direct operation counts without instrumenting the expensive reconstruction.
prepareTopologyCutProposalCached ::
  Reconciliation.ReconciliationViews ->
  TopologyControlCheckpoint ->
  StructuralProgressState ->
  Either StructuralProgressProblem (TopologyProjectionWork, StructuralProgressState, TopologyCutProposalPreparation)
prepareTopologyCutProposalCached = prepareTopologyCutProposalUsing True

prepareTopologyCutProposalUsing ::
  Bool ->
  Reconciliation.ReconciliationViews ->
  TopologyControlCheckpoint ->
  StructuralProgressState ->
  Either StructuralProgressProblem (TopologyProjectionWork, StructuralProgressState, TopologyCutProposalPreparation)
prepareTopologyCutProposalUsing reuse views checkpoint state = do
  when state.joiningObserver (Left StructuralProgressObserverCannotContribute)
  let expectedAnnouncer = NonEmpty.head (currentMembers state)
  if state.localHerald == expectedAnnouncer
    then Right ()
    else Left (StructuralProgressNotAnnouncer expectedAnnouncer state.localHerald)
  case state.openCut of
    Just open -> Left (StructuralProgressOpenCutExists (topologyCutAnnounceId open.announce))
    Nothing -> Right ()
  frontier <-
    maybe
      (Left StructuralProgressNoCompleteReportMatrix)
      Right
      (structuralStableFrontier state)
  validateCheckpoint frontier checkpoint
  validateSuccessorFrontier frontier state
  (work, retained, projection) <- prepareProposalProjection reuse views frontier state
  case projection of
    Left dependencies -> Right (work, retained, TopologyCutProposalHeld dependencies)
    Right digest -> do
      let predecessorFrontier = currentInstalledFrontier state
          vectorAdvanced =
            topologyFrontierStructuralVersionVector frontier
              /= topologyFrontierStructuralVersionVector predecessorFrontier
          controlAdvanced =
            topologyFrontierAppliedControlPrefix frontier
              > topologyFrontierAppliedControlPrefix predecessorFrontier
          topologyChanged = case currentInstalledOccurrenceDigest state of
            -- The distinguished genesis predecessor commits the complete
            -- checked InitialProjectionDigest, not a fabricated structural
            -- occurrence snapshot. An ordered label/End consequence can still
            -- change the induced checked-genesis projection before any dynamic
            -- structural dot exists; the typed checkpoint marks that case.
            Nothing -> checkpoint.affectsTopology
            Just incumbent -> digest /= incumbent
          cutAdvances = vectorAdvanced || (controlAdvanced && (topologyChanged || joinCheckpointAdvances checkpoint))
      if not cutAdvances
        then Right (work, retained, TopologyCutProposalUnavailable)
        else do
          cut <-
            mapLeft
              StructuralProgressTopologyShapeProblem
              (topologyCut (sameGenerationPredecessor state.lastInstalledCutId) frontier digest)
          let
            announce = topologyCutAnnounce state.localHerald cut
            acceptance =
              topologyCutAcceptance
                (topologyCutAnnounceId announce)
                (structuralLocalReport state)
            open =
              OpenTopologyCut
                { announce,
                  acceptances = Map.singleton state.localHerald acceptance,
                  openEstablished = Nothing,
                  openAcknowledgements = Map.empty,
                  joinSealAdmission = checkpoint.joinSealAdmission
                }
            successor = retained {openCut = Just open}
          Right
            ( work,
              retained,
              TopologyCutProposalReady
                (PreparedTopologyCutProposal successor announce acceptance)
            )

prepareProposalProjection ::
  Bool ->
  Reconciliation.ReconciliationViews ->
  TopologyFrontier ->
  StructuralProgressState ->
  Either StructuralProgressProblem (TopologyProjectionWork, StructuralProgressState, Either (Set Reconciliation.StructuralDependency) TopologyOccurrenceDigest)
prepareProposalProjection reuse views frontier state = do
  inputs <- mapLeft StructuralProgressReconciliationProblem (Reconciliation.prepareStructuralProjectionInputs views)
  let !key =
        TopologyProjectionKey
          (topologyFrontierStructuralVersionVector frontier)
          (Reconciliation.structuralProjectionControlCoordinate prefix state.reconciliation)
          inputs
      prefix = topologyFrontierAppliedControlPrefix frontier
  case if reuse then derivedValue state.proposalProjection else Nothing of
    Just (CachedTopologyProjection previous validatedThrough digest)
      | previous == key && validatedThrough <= prefix ->
          Right (TopologyProjectionReused, state, Right digest)
    _ -> do
      validateCandidateOccurrences frontier state
      projection <- projectionAtAdmittedFrontier views frontier state
      case projection of
        Left dependencies -> Right (TopologyProjectionReconstructed, state, Left dependencies)
        Right digest ->
          let !cached = CachedTopologyProjection key prefix digest
              !memo = Just cached
              retained = state {proposalProjection = derive memo}
           in Right (TopologyProjectionReconstructed, retained, Right digest)

preparedTopologyCutProposalAnnounce ::
  PreparedTopologyCutProposal -> TopologyCutAnnounce
preparedTopologyCutProposalAnnounce prepared = prepared.announce

preparedTopologyCutProposalLocalAcceptance ::
  PreparedTopologyCutProposal -> TopologyCutAcceptance
preparedTopologyCutProposalLocalAcceptance prepared = prepared.localAcceptance

commitTopologyCutProposal ::
  PreparedTopologyCutProposal -> StructuralProgressState
commitTopologyCutProposal prepared = prepared.successor

data TopologyCutAnnounceClassification
  = TopologyCutAnnounceAccepted TopologyCutAcceptance
  | TopologyCutAnnounceDuplicate TopologyCutAcceptance
  | TopologyCutAnnounceHeld
  | TopologyCutAnnounceStale TopologyCutEstablished
  deriving stock (Eq, Show)

data PreparedTopologyCutAnnounce = PreparedTopologyCutAnnounce
  { successor :: StructuralProgressState,
    classification :: TopologyCutAnnounceClassification
  }

prepareTopologyCutAnnounce ::
  Reconciliation.ReconciliationViews ->
  TopologyControlCheckpoint ->
  TopologyCutAnnounce ->
  StructuralProgressState ->
  Either StructuralProgressProblem PreparedTopologyCutAnnounce
prepareTopologyCutAnnounce views checkpoint announce state = do
  when state.joiningObserver (Left StructuralProgressObserverCannotContribute)
  validateAnnounceIdentity announce state
  validateOrdinaryCutPredecessor (topologyCutAnnounceCut announce)
  case state.openCut of
    Just open
      | open.announce == announce ->
          case Map.lookup state.localHerald open.acceptances of
            Just acceptance ->
              Right
                ( PreparedTopologyCutAnnounce
                    (clearMatchingPendingAnnounce announce state)
                    (TopologyCutAnnounceDuplicate acceptance)
                )
            Nothing -> admitCurrentPredecessor
      | topologyCutPredecessor (topologyCutAnnounceCut open.announce)
          == topologyCutPredecessor (topologyCutAnnounceCut announce) ->
          Left
            ( StructuralProgressEstablishedConflict
                (topologyCutAnnounceId announce)
            )
      | otherwise -> admitByPredecessor
    Nothing -> admitByPredecessor
  where
    admitByPredecessor
      | topologyPredecessorCutId (topologyCutPredecessor cut) == state.lastInstalledCutId =
          admitCurrentPredecessor
      | isInstalledCut (topologyPredecessorCutId (topologyCutPredecessor cut)) state =
          case currentEstablished state of
            Just established ->
              Right
                ( PreparedTopologyCutAnnounce
                    (clearMatchingPendingAnnounce announce state)
                    (TopologyCutAnnounceStale established)
                )
            Nothing ->
              Left
                ( StructuralProgressCutPredecessorMismatch
                    state.lastInstalledCutId
                    (topologyPredecessorCutId (topologyCutPredecessor cut))
                )
      | otherwise =
          Right
            ( PreparedTopologyCutAnnounce
                state {pendingAnnounce = Just announce}
                TopologyCutAnnounceHeld
            )

    admitCurrentPredecessor
      | not localCovers =
          Right
            ( PreparedTopologyCutAnnounce
                state {pendingAnnounce = Just announce}
                TopologyCutAnnounceHeld
            )
      | otherwise = do
          validateCheckpoint frontier checkpoint
          validateSuccessorFrontier frontier state
          validateCutAdvances checkpoint cut state
          validateCandidateOccurrences frontier state
          projection <- projectionAtAdmittedFrontier views frontier state
          case projection of
            Left _ ->
              Right
                ( PreparedTopologyCutAnnounce
                    state {pendingAnnounce = Just announce}
                    TopologyCutAnnounceHeld
                )
            Right digest
              | digest /= topologyCutOccurrenceDigest cut ->
                  Left StructuralProgressTopologyDigestMismatch
              | otherwise ->
                  let acceptance =
                        topologyCutAcceptance
                          (topologyCutAnnounceId announce)
                          (structuralLocalReport state)
                      open =
                        OpenTopologyCut
                          { announce,
                            acceptances = Map.singleton state.localHerald acceptance,
                            openEstablished = Nothing,
                            openAcknowledgements = Map.empty,
                            joinSealAdmission = checkpoint.joinSealAdmission
                          }
                   in Right
                        ( PreparedTopologyCutAnnounce
                            (clearMatchingPendingAnnounce announce state)
                              { openCut = Just open
                              }
                            (TopologyCutAnnounceAccepted acceptance)
                        )

    cut = topologyCutAnnounceCut announce
    frontier = topologyCutFrontier cut
    localCovers =
      structuralVersionVectorCovers
        state.appliedVector
        (topologyFrontierStructuralVersionVector frontier)
        && state.appliedControlPrefix
          >= topologyFrontierAppliedControlPrefix frontier

clearMatchingPendingAnnounce ::
  TopologyCutAnnounce ->
  StructuralProgressState ->
  StructuralProgressState
clearMatchingPendingAnnounce announce state =
  case state.pendingAnnounce of
    Just retained
      | retained == announce -> state {pendingAnnounce = Nothing}
    _ -> state

preparedTopologyCutAnnounceClassification ::
  PreparedTopologyCutAnnounce -> TopologyCutAnnounceClassification
preparedTopologyCutAnnounceClassification prepared = prepared.classification

commitTopologyCutAnnounce ::
  PreparedTopologyCutAnnounce -> StructuralProgressState
commitTopologyCutAnnounce prepared = prepared.successor

data TopologyCutAcceptanceClassification
  = TopologyCutAcceptanceRetained
  | TopologyCutAcceptanceDuplicate
  | TopologyCutAcceptanceCompletesSet
  | TopologyCutAcceptanceReoffersEstablished TopologyCutEstablished
  | TopologyCutAcceptanceHistoricalStale
  deriving stock (Eq, Show)

data PreparedTopologyCutAcceptance = PreparedTopologyCutAcceptance
  { successor :: StructuralProgressState,
    classification :: TopologyCutAcceptanceClassification
  }

prepareTopologyCutAcceptance ::
  TopologyCutAcceptance ->
  StructuralProgressState ->
  Either StructuralProgressProblem PreparedTopologyCutAcceptance
prepareTopologyCutAcceptance acceptance state
  | exactActivePredecessorAcceptance acceptance state =
      Right
        ( PreparedTopologyCutAcceptance
            state
            TopologyCutAcceptanceHistoricalStale
        )
  | otherwise = do
      let expectedAnnouncer = NonEmpty.head (currentMembers state)
      if state.localHerald == expectedAnnouncer
        then Right ()
        else Left (StructuralProgressNotAnnouncer expectedAnnouncer state.localHerald)
      validateReportShape (topologyCutAcceptanceReport acceptance) state
      case state.openCut of
        Nothing -> staleInstalledAcceptance
        Just open
          | topologyCutAcceptanceId acceptance /= topologyCutAnnounceId open.announce ->
              staleInstalledAcceptance
          | otherwise -> admitOpenAcceptance open
  where
    admitOpenAcceptance open = do
      let reporter = topologyCutAcceptanceReporter acceptance
          frontier = topologyCutFrontier (topologyCutAnnounceCut open.announce)
      if reportCoversFrontier (topologyCutAcceptanceReport acceptance) frontier
        then Right ()
        else Left (StructuralProgressAcceptanceDoesNotCover reporter)
      case Map.lookup reporter open.acceptances of
        Just incumbent
          | incumbent == acceptance ->
              Right
                ( PreparedTopologyCutAcceptance
                    state
                    ( case open.openEstablished of
                        Just established ->
                          TopologyCutAcceptanceReoffersEstablished established
                        Nothing -> TopologyCutAcceptanceDuplicate
                    )
                )
          | otherwise -> Left (StructuralProgressAcceptanceConflict reporter)
        Nothing ->
          let acceptances = Map.insert reporter acceptance open.acceptances
              complete = Map.keysSet acceptances == memberSet state
              successor = state {openCut = Just open {acceptances}}
           in Right
                ( PreparedTopologyCutAcceptance
                    successor
                    ( if complete
                        then TopologyCutAcceptanceCompletesSet
                        else TopologyCutAcceptanceRetained
                    )
                )

    staleInstalledAcceptance =
      case Map.lookup (topologyCutAcceptanceId acceptance) state.installedCuts of
        Just installed ->
          case find
            ( (== topologyCutAcceptanceReporter acceptance)
                . topologyCutAcceptanceReporter
            )
            (topologyCutEstablishedAcceptances installed.installedEstablished) of
            Just incumbent
              | incumbent == acceptance ->
                  Right
                    ( PreparedTopologyCutAcceptance
                        state
                        (TopologyCutAcceptanceReoffersEstablished installed.installedEstablished)
                    )
              | otherwise -> acceptanceConflict
            Nothing -> acceptanceConflict
        Nothing ->
          Left
            (StructuralProgressAcceptanceForUnknownCut (topologyCutAcceptanceId acceptance))

    acceptanceConflict =
      Left
        ( StructuralProgressAcceptanceConflict
            (topologyCutAcceptanceReporter acceptance)
        )

preparedTopologyCutAcceptanceClassification ::
  PreparedTopologyCutAcceptance -> TopologyCutAcceptanceClassification
preparedTopologyCutAcceptanceClassification prepared = prepared.classification

commitTopologyCutAcceptance ::
  PreparedTopologyCutAcceptance -> StructuralProgressState
commitTopologyCutAcceptance prepared = prepared.successor

data PreparedTopologyCutEstablishment = PreparedTopologyCutEstablishment
  { successor :: StructuralProgressState,
    preparedEstablished :: TopologyCutEstablished,
    preparedAcknowledgement :: TopologyCutEstablishedAck
  }

prepareTopologyCutEstablishment ::
  StructuralProgressState ->
  Either StructuralProgressProblem PreparedTopologyCutEstablishment
prepareTopologyCutEstablishment state = do
  let expectedAnnouncer = NonEmpty.head (currentMembers state)
  if state.localHerald == expectedAnnouncer
    then Right ()
    else Left (StructuralProgressNotAnnouncer expectedAnnouncer state.localHerald)
  open <- maybe (Left StructuralProgressNoOpenCut) Right state.openCut
  if Map.keysSet open.acceptances == memberSet state
    then Right ()
    else Left StructuralProgressIncompleteEstablishedEvidence
  established <-
    mapLeft
      (const StructuralProgressEstablishedEvidenceProblem)
      ( topologyCutEstablished
          (topologyCutAnnounceId open.announce)
          (topologyCutAnnounceCut open.announce)
          (Map.elems open.acceptances)
      )
  let acknowledgement =
        topologyCutEstablishedAck
          (topologyCutEstablishedId established)
          state.localHerald
          (currentMembershipGenerationId state)
  successor <- installEstablished open.joinSealAdmission established acknowledgement state
  let retainedOpen =
        open
          { openEstablished = Just established,
            openAcknowledgements = Map.singleton state.localHerald acknowledgement
          }
      acknowledgementComplete =
        Set.singleton state.localHerald == memberSet state
  Right
    PreparedTopologyCutEstablishment
      { successor =
          successor
            { openCut =
                if acknowledgementComplete
                  then Nothing
                  else Just retainedOpen
            },
        preparedEstablished = established,
        preparedAcknowledgement = acknowledgement
      }

preparedTopologyCutEstablished ::
  PreparedTopologyCutEstablishment -> TopologyCutEstablished
preparedTopologyCutEstablished prepared = prepared.preparedEstablished

preparedTopologyCutEstablishedAck ::
  PreparedTopologyCutEstablishment -> TopologyCutEstablishedAck
preparedTopologyCutEstablishedAck prepared = prepared.preparedAcknowledgement

commitTopologyCutEstablishment ::
  PreparedTopologyCutEstablishment -> StructuralProgressState
commitTopologyCutEstablishment prepared = prepared.successor

data TopologyCutEstablishedClassification
  = TopologyCutEstablishedInstalled TopologyCutEstablishedAck
  | TopologyCutEstablishedDuplicate TopologyCutEstablishedAck
  | TopologyCutEstablishedHeld
  | TopologyCutEstablishedImported
  deriving stock (Eq, Show)

data PreparedTopologyCutEstablished = PreparedTopologyCutEstablished
  { successor :: StructuralProgressState,
    classification :: TopologyCutEstablishedClassification
  }

prepareTopologyCutEstablished ::
  Reconciliation.ReconciliationViews ->
  TopologyControlCheckpoint ->
  TopologyCutEstablished ->
  StructuralProgressState ->
  Either StructuralProgressProblem PreparedTopologyCutEstablished
prepareTopologyCutEstablished views checkpoint established state = do
  let cutId = topologyCutEstablishedId established
      cut = topologyCutEstablishedCut established
  case Map.lookup cutId state.installedCuts of
    Just installed
      | installed.installedEstablished == established ->
          Right
            ( PreparedTopologyCutEstablished
                (clearMatchingPendingEstablished established state)
                (maybe TopologyCutEstablishedImported TopologyCutEstablishedDuplicate installed.ownAcknowledgement)
            )
      | otherwise -> Left (StructuralProgressEstablishedConflict cutId)
    Nothing -> do
      validateOrdinaryCutPredecessor cut
      case break ((== cutId) . topologyCutEstablishedId) state.pendingEstablished of
        ([], retained : _)
          | retained == established ->
              if topologyPredecessorCutId (topologyCutPredecessor cut) == state.lastInstalledCutId
                then admitCurrentPredecessor True cutId cut
                else
                  Left
                    ( StructuralProgressCutPredecessorMismatch
                        state.lastInstalledCutId
                        (topologyPredecessorCutId (topologyCutPredecessor cut))
                    )
          | otherwise -> Left (StructuralProgressEstablishedConflict cutId)
        (_ : _, retained : _)
          | retained == established -> retainHeld state
          | otherwise -> Left (StructuralProgressEstablishedConflict cutId)
        (_, []) -> do
          -- Reconnect repair deliberately reoffers the complete retained cut
          -- chain.  Once terminal Established has installed a membership-successor
          -- base, an exact ordinary predecessor-generation Established is still a
          -- deterministic duplicate even though it no longer has the current
          -- membership shape.  Exact retained equality is checked before current-
          -- generation admission; any changed transcript for the same identity was
          -- rejected above and any unknown historical identity remains subject to
          -- the ordinary current-membership checks below.
          validateEstablishedEvidence established state
          case reverse state.pendingEstablished of
            retainedPredecessor : _ ->
              let expected = topologyCutEstablishedId retainedPredecessor
                  observed = topologyPredecessorCutId (topologyCutPredecessor cut)
               in if observed == expected
                    then do
                      validateRetainedEstablishedSuccessor retainedPredecessor established
                      retainHeld
                        state
                          { pendingEstablished =
                              state.pendingEstablished <> [established]
                          }
                    else
                      Left
                        ( StructuralProgressCutPredecessorMismatch
                            expected
                            observed
                        )
            [] ->
              if topologyPredecessorCutId (topologyCutPredecessor cut) == state.lastInstalledCutId
                then admitCurrentPredecessor False cutId cut
                else
                  Left
                    ( StructuralProgressCutPredecessorMismatch
                        state.lastInstalledCutId
                        (topologyPredecessorCutId (topologyCutPredecessor cut))
                    )
  where
    admitCurrentPredecessor alreadyRetained cutId cut
      | not localCovers = held
      | otherwise = do
          validateCheckpoint frontier checkpoint
          validateSuccessorFrontier frontier state
          validateCutAdvances checkpoint cut state
          validateCandidateOccurrences frontier state
          projection <- projectionAtAdmittedFrontier views frontier state
          case projection of
            Left _ -> held
            Right digest
              | digest /= topologyCutOccurrenceDigest cut ->
                  Left StructuralProgressTopologyDigestMismatch
              | otherwise -> do
                  let acknowledgement =
                        topologyCutEstablishedAck
                          cutId
                          state.localHerald
                          (currentMembershipGenerationId state)
                  successor <- installEstablished checkpoint.joinSealAdmission established acknowledgement state
                  let successorOpen = case state.openCut of
                        Just open
                          | topologyCutAnnounceId open.announce == cutId -> Nothing
                        other -> other
                  Right
                    ( PreparedTopologyCutEstablished
                        (clearMatchingPendingEstablished established successor)
                          { openCut = successorOpen
                          }
                        (if state.joiningObserver then TopologyCutEstablishedImported else TopologyCutEstablishedInstalled acknowledgement)
                    )
      where
        frontier = topologyCutFrontier cut
        localCovers =
          structuralVersionVectorCovers
            state.appliedVector
            (topologyFrontierStructuralVersionVector frontier)
            && state.appliedControlPrefix
              >= topologyFrontierAppliedControlPrefix frontier
        held =
          retainHeld
            ( if alreadyRetained
                then state
                else
                  state
                    { pendingEstablished =
                        state.pendingEstablished <> [established]
                    }
            )

    retainHeld successor =
      Right
        ( PreparedTopologyCutEstablished
            successor
            TopologyCutEstablishedHeld
        )

validateRetainedEstablishedSuccessor ::
  TopologyCutEstablished ->
  TopologyCutEstablished ->
  Either StructuralProgressProblem ()
validateRetainedEstablishedSuccessor predecessor established = do
  let predecessorId = topologyCutEstablishedId predecessor
      predecessorCut = topologyCutEstablishedCut predecessor
      predecessorFrontier = topologyCutFrontier predecessorCut
      cut = topologyCutEstablishedCut established
      frontier = topologyCutFrontier cut
      observedPredecessor =
        topologyPredecessorCutId (topologyCutPredecessor cut)
  validateOrdinaryCutPredecessor cut
  if observedPredecessor == predecessorId
    then Right ()
    else
      Left
        ( StructuralProgressCutPredecessorMismatch
            predecessorId
            observedPredecessor
        )
  if structuralVersionVectorCovers
    (topologyFrontierStructuralVersionVector frontier)
    (topologyFrontierStructuralVersionVector predecessorFrontier)
    && topologyFrontierAppliedControlPrefix frontier
      >= topologyFrontierAppliedControlPrefix predecessorFrontier
    then Right ()
    else Left StructuralProgressCutFrontierRegressed
  let vectorAdvanced =
        topologyFrontierStructuralVersionVector frontier
          /= topologyFrontierStructuralVersionVector predecessorFrontier
      controlAdvanced =
        topologyFrontierAppliedControlPrefix frontier
          > topologyFrontierAppliedControlPrefix predecessorFrontier
  -- Pending certificates remain claims. Their join checkpoint is checked
  -- against the retained Oracle history when they reach local installation.
  if vectorAdvanced || controlAdvanced
    then Right ()
    else Left StructuralProgressCutDoesNotAdvance

clearMatchingPendingEstablished ::
  TopologyCutEstablished ->
  StructuralProgressState ->
  StructuralProgressState
clearMatchingPendingEstablished established state =
  case state.pendingEstablished of
    retained : remaining
      | retained == established -> state {pendingEstablished = remaining}
    _ -> state

preparedTopologyCutEstablishedClassification ::
  PreparedTopologyCutEstablished -> TopologyCutEstablishedClassification
preparedTopologyCutEstablishedClassification prepared = prepared.classification

commitTopologyCutEstablished ::
  PreparedTopologyCutEstablished -> StructuralProgressState
commitTopologyCutEstablished prepared = prepared.successor

data TopologyCutAckClassification
  = TopologyCutAckRetained
  | TopologyCutAckDuplicate
  | TopologyCutAckAllMembers
  | TopologyCutAckStale
  deriving stock (Eq, Ord, Show)

data PreparedTopologyCutAck = PreparedTopologyCutAck
  { successor :: StructuralProgressState,
    classification :: TopologyCutAckClassification
  }

prepareTopologyCutEstablishedAck ::
  TopologyCutEstablishedAck ->
  StructuralProgressState ->
  Either StructuralProgressProblem PreparedTopologyCutAck
prepareTopologyCutEstablishedAck acknowledgement state
  | exactInstalledPredecessorAcknowledgement acknowledgement state
      || exactInstalledSuccessorBaseAcknowledgement acknowledgement state =
      Right (PreparedTopologyCutAck state TopologyCutAckStale)
  | otherwise = do
      let expectedAnnouncer = NonEmpty.head (currentMembers state)
          reporter = topologyCutEstablishedAckReporter acknowledgement
          cutId = topologyCutEstablishedAckId acknowledgement
      if state.localHerald == expectedAnnouncer
        then Right ()
        else Left (StructuralProgressNotAnnouncer expectedAnnouncer state.localHerald)
      if Set.member reporter (memberSet state)
        then Right ()
        else Left (StructuralProgressForeignReporter reporter)
      if topologyCutEstablishedAckMembershipGenerationId acknowledgement
        == currentMembershipGenerationId state
        then Right ()
        else
          Left
            ( StructuralProgressWrongMembershipGeneration
                (currentMembershipGenerationId state)
                (topologyCutEstablishedAckMembershipGenerationId acknowledgement)
            )
      case state.openCut of
        Nothing -> staleInstalledAck cutId reporter
        Just open
          | topologyCutAnnounceId open.announce /= cutId ->
              staleInstalledAck cutId reporter
          | otherwise ->
              case (open.openEstablished, Map.lookup cutId state.installedCuts) of
                (Just established, Just installed)
                  | installed.installedEstablished == established ->
                      retainAck cutId reporter open
                _ -> Left (StructuralProgressAckForUnknownCut cutId)
  where
    retainAck cutId reporter open = case Map.lookup reporter open.openAcknowledgements of
      Just incumbent
        | incumbent == acknowledgement ->
            Right (PreparedTopologyCutAck state TopologyCutAckDuplicate)
        | otherwise -> Left (StructuralProgressAckConflict reporter)
      Nothing -> do
        let acknowledgements =
              Map.insert reporter acknowledgement open.openAcknowledgements
            complete = Map.keysSet acknowledgements == memberSet state
            openSuccessor = open {openAcknowledgements = acknowledgements}
            installedSuccessor =
              Map.adjust
                ( \installed ->
                    installed {installedAcknowledgements = acknowledgements}
                )
                cutId
                state.installedCuts
            successor =
              state
                { installedCuts = installedSuccessor,
                  openCut = if complete then Nothing else Just openSuccessor
                }
        Right
          ( PreparedTopologyCutAck
              successor
              (if complete then TopologyCutAckAllMembers else TopologyCutAckRetained)
          )

    staleInstalledAck cutId reporter =
      case Map.lookup cutId state.installedCuts of
        Nothing -> Left (StructuralProgressAckForUnknownCut cutId)
        Just installed ->
          case Map.lookup reporter installed.installedAcknowledgements of
            Just incumbent
              | incumbent == acknowledgement ->
                  Right (PreparedTopologyCutAck state TopologyCutAckStale)
              | otherwise -> Left (StructuralProgressAckConflict reporter)
            Nothing -> Left (StructuralProgressAckConflict reporter)

preparedTopologyCutAckClassification ::
  PreparedTopologyCutAck -> TopologyCutAckClassification
preparedTopologyCutAckClassification prepared = prepared.classification

commitTopologyCutEstablishedAck ::
  PreparedTopologyCutAck -> StructuralProgressState
commitTopologyCutEstablishedAck prepared = prepared.successor

validateAnnounceIdentity ::
  TopologyCutAnnounce ->
  StructuralProgressState ->
  Either StructuralProgressProblem ()
validateAnnounceIdentity announce state = do
  let expectedAnnouncer = NonEmpty.head (currentMembers state)
      observedAnnouncer = topologyCutAnnounceAnnouncer announce
      claimed = topologyCutAnnounceId announce
      derived = deriveTopologyCutId (topologyCutAnnounceCut announce)
      frontier = topologyCutFrontier (topologyCutAnnounceCut announce)
  if observedAnnouncer == expectedAnnouncer
    then Right ()
    else Left (StructuralProgressWrongCutAnnouncer expectedAnnouncer observedAnnouncer)
  if claimed == derived
    then Right ()
    else Left (StructuralProgressCutIdMismatch derived claimed)
  if topologyFrontierMemberSetDigest frontier == currentMemberSetDigest state
    then Right ()
    else
      Left
        ( StructuralProgressWrongMemberSet
            (currentMemberSetDigest state)
            (topologyFrontierMemberSetDigest frontier)
        )
  if structuralVersionVectorMembershipGenerationId
    (topologyFrontierStructuralVersionVector frontier)
    == currentMembershipGenerationId state
    then Right ()
    else
      Left
        ( StructuralProgressWrongMembershipGeneration
            (currentMembershipGenerationId state)
            ( structuralVersionVectorMembershipGenerationId
                (topologyFrontierStructuralVersionVector frontier)
            )
        )
  if vectorMembers (topologyFrontierStructuralVersionVector frontier) == memberSet state
    then Right ()
    else Left (StructuralProgressReportVectorMembershipMismatch observedAnnouncer)

-- | The membership-successor predecessor transcript carries terminal-union
-- authority and is admitted only by the dedicated atomic successor-base
-- transition.  Every ordinary cut message must name the simple predecessor
-- form, even when a malformed transcript embeds the same cut id.
validateOrdinaryCutPredecessor ::
  TopologyCut -> Either StructuralProgressProblem ()
validateOrdinaryCutPredecessor cut =
  if ordinaryCutPredecessorValid cut
    then Right ()
    else
      Left
        ( StructuralProgressOrdinaryCutRequiresSameGenerationPredecessor
            (topologyPredecessorCutId (topologyCutPredecessor cut))
        )

ordinaryCutPredecessorValid :: TopologyCut -> Bool
ordinaryCutPredecessorValid cut =
  case topologyPredecessorView (topologyCutPredecessor cut) of
    SameGenerationPredecessorView _ -> True
    MembershipSuccessorPredecessorView {} -> False
    AdmissionTopologyPredecessorView {} -> False

validateCheckpoint ::
  TopologyFrontier ->
  TopologyControlCheckpoint ->
  Either StructuralProgressProblem ()
validateCheckpoint frontier checkpoint
  | topologyFrontierAppliedControlPrefix frontier == checkpoint.index = Right ()
  | otherwise =
      Left
        ( StructuralProgressControlPrerequisiteNotCovered
            (topologyFrontierAppliedControlPrefix frontier)
            checkpoint.index
        )

validateSuccessorFrontier ::
  TopologyFrontier ->
  StructuralProgressState ->
  Either StructuralProgressProblem ()
validateSuccessorFrontier candidate state =
  let predecessor = currentInstalledFrontier state
   in if structuralVersionVectorCovers
        (topologyFrontierStructuralVersionVector candidate)
        (topologyFrontierStructuralVersionVector predecessor)
        && topologyFrontierAppliedControlPrefix candidate
          >= topologyFrontierAppliedControlPrefix predecessor
        then Right ()
        else Left StructuralProgressCutFrontierRegressed

validateCutAdvances ::
  TopologyControlCheckpoint ->
  TopologyCut ->
  StructuralProgressState ->
  Either StructuralProgressProblem ()
validateCutAdvances checkpoint cut state =
  let candidate = topologyCutFrontier cut
      predecessor = currentInstalledFrontier state
      vectorAdvanced =
        topologyFrontierStructuralVersionVector candidate
          /= topologyFrontierStructuralVersionVector predecessor
      controlAdvanced =
        topologyFrontierAppliedControlPrefix candidate
          > topologyFrontierAppliedControlPrefix predecessor
      digestChanged = case currentInstalledOccurrenceDigest state of
        Nothing -> checkpoint.affectsTopology
        Just incumbent -> topologyCutOccurrenceDigest cut /= incumbent
   in if vectorAdvanced || (controlAdvanced && (digestChanged || joinCheckpointAdvances checkpoint))
        then Right ()
        else Left StructuralProgressCutDoesNotAdvance

validateCandidateOccurrences ::
  TopologyFrontier ->
  StructuralProgressState ->
  Either StructuralProgressProblem ()
validateCandidateOccurrences frontier state =
  case Reconciliation.prepareStructuralFrontierSummary vector state.reconciliation of
    Right summary
      | topologyFrontierAppliedControlPrefix frontier >= Reconciliation.structuralFrontierControlPrerequisite summary,
        all (candidateVectorCoversPredecessor state vector . snd) (Reconciliation.structuralFrontierPredecessors summary) ->
          Right ()
    -- Preserve the exact first closure/control error of the original scan.
    -- Ordinary candidate admission has already established local coverage.
    _ -> validateCandidateOccurrencesExhaustive frontier state
  where
    vector = topologyFrontierStructuralVersionVector frontier

validateCandidateOccurrencesExhaustive ::
  TopologyFrontier -> StructuralProgressState -> Either StructuralProgressProblem ()
validateCandidateOccurrencesExhaustive frontier state =
  mapM_ validate covered
  where
    vector = topologyFrontierStructuralVersionVector frontier
    controlPrefix = topologyFrontierAppliedControlPrefix frontier
    covered =
      [ retained
      | (occurrence, retained) <- Map.toAscList state.history,
        frontierCoversOccurrence occurrence frontier
      ]
    validate retained
      | not
          ( candidateVectorCoversPredecessor
              state
              vector
              ( structuralOccurrenceStampPredecessor
                  (appliedStructuralOccurrenceStamp retained)
              )
          ) =
          Left StructuralProgressPredecessorNotCovered
      | controlPrefix < retained.controlPrerequisite =
          Left
            ( StructuralProgressControlPrerequisiteNotCovered
                controlPrefix
                retained.controlPrerequisite
            )
      | otherwise = Right ()

projectionAtFrontier ::
  Reconciliation.ReconciliationViews ->
  TopologyFrontier ->
  StructuralProgressState ->
  Either
    StructuralProgressProblem
    (Either (Set Reconciliation.StructuralDependency) TopologyOccurrenceDigest)
projectionAtFrontier =
  projectionAtFrontierUsing Reconciliation.prepareStructuralProjectionAtVector

-- The caller has already established local coverage and causal closure, or
-- selected an installed cut. Repeating those proofs is an optional audit.
projectionAtAdmittedFrontier ::
  Reconciliation.ReconciliationViews ->
  TopologyFrontier ->
  StructuralProgressState ->
  Either
    StructuralProgressProblem
    (Either (Set Reconciliation.StructuralDependency) TopologyOccurrenceDigest)
projectionAtAdmittedFrontier =
  projectionAtFrontierUsing Reconciliation.prepareStructuralProjectionAtAdmittedVector

projectionAtFrontierUsing ::
  (Reconciliation.ReconciliationViews -> StructuralVersionVector -> ControlIndex -> Reconciliation.StructuralAppliedState -> Either Reconciliation.StructuralReconciliationProblem (Either (Set Reconciliation.StructuralDependency) Reconciliation.StructuralProjectionSnapshot)) ->
  Reconciliation.ReconciliationViews ->
  TopologyFrontier ->
  StructuralProgressState ->
  Either
    StructuralProgressProblem
    (Either (Set Reconciliation.StructuralDependency) TopologyOccurrenceDigest)
projectionAtFrontierUsing prepareProjection views frontier state = do
  projection <-
    mapLeft
      StructuralProgressReconciliationProblem
      ( prepareProjection
          views
          (topologyFrontierStructuralVersionVector frontier)
          (topologyFrontierAppliedControlPrefix frontier)
          state.reconciliation
      )
  pure
    $ fmap
      (deriveTopologyOccurrenceDigest . canonicalTopologyProjection state.initialProjection)
      projection

-- | Reconstruct the topology digest at one exact retained structural/control
-- coordinate without exposing the reconciliation owner.  Terminal-source
-- establishment uses this checked seam for its derived frontier; dependency
-- incompleteness remains explicit rather than being converted into a guessed
-- digest.
structuralTopologyOccurrenceDigestAt ::
  Reconciliation.ReconciliationViews ->
  StructuralVersionVector ->
  ControlIndex ->
  StructuralProgressState ->
  Either
    StructuralProgressProblem
    (Either (Set Reconciliation.StructuralDependency) TopologyOccurrenceDigest)
structuralTopologyOccurrenceDigestAt views vector controlPrefix =
  projectionAtFrontier views (topologyFrontier vector controlPrefix)

-- | The Begin command may describe checked genesis before any dynamic cut has
-- been materialized. This claim grants no installed-cut authority; a join seal
-- subsequently establishes its real old-member cut through the ordinary
-- report/acceptance ceremony.
structuralAdmissionAnchorClaim :: Reconciliation.ReconciliationViews -> StructuralProgressState -> Either StructuralProgressProblem TopologyCut
structuralAdmissionAnchorClaim views state = case Map.lookup state.lastInstalledCutId state.installedCuts of
  Just installed -> Right (installedTopologyCutCut installed)
  Nothing -> do
    let frontier = currentInstalledFrontier state
    result <- projectionAtFrontier views frontier state
    digest <- either (Left . StructuralProgressTopologyProjectionHeld) Right result
    mapLeft StructuralProgressTopologyShapeProblem (topologyCut (sameGenerationPredecessor state.genesisCutId) frontier digest)

canonicalTopologyProjection ::
  InitialProjectionDigest -> Reconciliation.StructuralProjectionSnapshot -> ByteString
canonicalTopologyProjection initialProjection snapshot =
  Serialize.runPut $ do
    putSizedBytes (ByteString.Char8.pack "ECLIPS-TOPOLOGY-PROJECTION")
    putSizedBytes (initialProjectionDigestBytes initialProjection)
    putSizedBytes (Reconciliation.structuralProjectionSnapshotCanonicalBytes snapshot)
  where
    putSizedBytes bytes = do
      Serialize.putWord64be (fromIntegral (ByteString.length bytes))
      Serialize.putByteString bytes

currentInstalledFrontier :: StructuralProgressState -> TopologyFrontier
currentInstalledFrontier state =
  case Map.lookup state.lastInstalledCutId state.installedCuts of
    Just installed -> installedCutFrontier installed
    Nothing ->
      topologyFrontier
        (emptyStructuralVersionVector state.currentMembership)
        (controlIndex 0)

currentInstalledOccurrenceDigest ::
  StructuralProgressState -> Maybe TopologyOccurrenceDigest
currentInstalledOccurrenceDigest state =
  topologyCutOccurrenceDigest
    . topologyCutEstablishedCut
    . installedTopologyCutEstablished
    <$> Map.lookup state.lastInstalledCutId state.installedCuts

currentEstablished :: StructuralProgressState -> Maybe TopologyCutEstablished
currentEstablished state =
  installedTopologyCutEstablished
    <$> Map.lookup state.lastInstalledCutId state.installedCuts

reportCoversFrontier :: StructuralAppliedReport -> TopologyFrontier -> Bool
reportCoversFrontier report frontier =
  structuralAppliedReportMemberSetDigest report
    == topologyFrontierMemberSetDigest frontier
    && structuralVersionVectorCovers
      (structuralAppliedReportVersionVector report)
      (topologyFrontierStructuralVersionVector frontier)
    && structuralAppliedReportControlPrefix report
      >= topologyFrontierAppliedControlPrefix frontier

validateEstablishedEvidence ::
  TopologyCutEstablished ->
  StructuralProgressState ->
  Either StructuralProgressProblem ()
validateEstablishedEvidence established state = do
  let cutId = topologyCutEstablishedId established
      cut = topologyCutEstablishedCut established
      derived = deriveTopologyCutId cut
      acceptances = topologyCutEstablishedAcceptances established
      acceptanceMap = Map.fromList [(topologyCutAcceptanceReporter acceptance, acceptance) | acceptance <- acceptances]
  if cutId == derived
    then Right ()
    else Left (StructuralProgressCutIdMismatch derived cutId)
  if topologyFrontierMemberSetDigest (topologyCutFrontier cut) == currentMemberSetDigest state
    then Right ()
    else
      Left
        ( StructuralProgressWrongMemberSet
            (currentMemberSetDigest state)
            (topologyFrontierMemberSetDigest (topologyCutFrontier cut))
        )
  if structuralVersionVectorMembershipGenerationId
    (topologyFrontierStructuralVersionVector (topologyCutFrontier cut))
    == currentMembershipGenerationId state
    then Right ()
    else
      Left
        ( StructuralProgressWrongMembershipGeneration
            (currentMembershipGenerationId state)
            ( structuralVersionVectorMembershipGenerationId
                (topologyFrontierStructuralVersionVector (topologyCutFrontier cut))
            )
        )
  if length acceptances == Map.size acceptanceMap
    && Map.keysSet acceptanceMap == memberSet state
    then Right ()
    else Left StructuralProgressIncompleteEstablishedEvidence
  mapM_ validateAcceptance acceptances
  where
    frontier = topologyCutFrontier (topologyCutEstablishedCut established)
    validateAcceptance acceptance = do
      if topologyCutAcceptanceId acceptance == topologyCutEstablishedId established
        then Right ()
        else
          Left
            ( StructuralProgressWrongAcceptanceCut
                (topologyCutEstablishedId established)
                (topologyCutAcceptanceId acceptance)
            )
      validateReportShape (topologyCutAcceptanceReport acceptance) state
      if reportCoversFrontier (topologyCutAcceptanceReport acceptance) frontier
        then Right ()
        else
          Left
            ( StructuralProgressAcceptanceDoesNotCover
                (topologyCutAcceptanceReporter acceptance)
            )

installEstablished ::
  Maybe HeraldAdmissionId ->
  TopologyCutEstablished ->
  TopologyCutEstablishedAck ->
  StructuralProgressState ->
  Either StructuralProgressProblem StructuralProgressState
installEstablished admission established acknowledgement state = do
  validateEstablishedEvidence established state
  let cutId = topologyCutEstablishedId established
      cut = topologyCutEstablishedCut established
  validateOrdinaryCutPredecessor cut
  if topologyPredecessorCutId (topologyCutPredecessor cut) == state.lastInstalledCutId
    then Right ()
    else
      Left
        ( StructuralProgressCutPredecessorMismatch
            state.lastInstalledCutId
            (topologyPredecessorCutId (topologyCutPredecessor cut))
        )
  let ownAcceptance =
        find
          ((== state.localHerald) . topologyCutAcceptanceReporter)
          (topologyCutEstablishedAcceptances established)
  let predecessorFrontier = currentInstalledFrontier state
      frontier = topologyCutFrontier cut
      newlyCovered =
        [ occurrence
        | occurrence <- Map.keys state.history,
          frontierCoversOccurrence occurrence frontier,
          not (frontierCoversOccurrence occurrence predecessorFrontier)
        ]
      installed =
        InstalledTopologyCut
          { installedEstablished = established,
            ownAcceptance,
            ownAcknowledgement = if state.joiningObserver then Nothing else Just acknowledgement,
            newlyCoveredOccurrences = newlyCovered,
            installedAcknowledgements =
              if state.joiningObserver then Map.empty else Map.singleton state.localHerald acknowledgement,
            membershipSuccessorBase = Nothing,
            membershipAdmissionBase = Nothing,
            joinSealAdmission = admission
          }
  Right
    ( refreshCurrentNablaProjections
        state
          { lastInstalledCutId = cutId,
            installedCuts = Map.insert cutId installed state.installedCuts,
            installedChain = state.installedChain <> [cutId],
            installedCoverageIndex = extendInstalledCoverageIndex cutId cut state.installedCoverageIndex
          }
    )

data StructuralProgressInvariantProblem
  = StructuralProgressInvariantMembership
  | StructuralProgressInvariantReconciliationMembership
  | StructuralProgressInvariantReconciliation
      Reconciliation.StructuralAppliedInvariantProblem
  | StructuralProgressInvariantAppliedVector
  | StructuralProgressInvariantHistory StructuralOccurrenceId
  | StructuralProgressInvariantReport HeraldEpoch
  | StructuralProgressInvariantInstalledChain TopologyCutId
  | StructuralProgressInvariantLastInstalled TopologyCutId
  | StructuralProgressInvariantCoverageIndex
  | StructuralProgressInvariantSupersededOpenCut TopologyCutId
  | StructuralProgressInvariantOpenCut TopologyCutId
  | StructuralProgressInvariantPendingAnnounce TopologyCutId
  | StructuralProgressInvariantPendingEstablished TopologyCutId
  deriving stock (Eq, Show)

-- | Validate the complete pure owner witness consumed by Startup.Invariant.
-- This is intentionally a contradiction detector, not a recovery path.
validateStructuralProgressState ::
  StructuralProgressState ->
  Either StructuralProgressInvariantProblem ()
validateStructuralProgressState state = do
  let members = memberSet state
      normalizedMembers = Set.toAscList members
  if NonEmpty.toList (currentMembers state) == normalizedMembers
    && Set.member state.localHerald members /= state.joiningObserver
    && heraldMembershipGenerationActiveMemberSetDigest state.currentMembership
      == currentMemberSetDigest state
    && Map.lookup
      (currentMembershipGenerationId state)
      state.membershipHistory
      == Just state.currentMembership
    && all
      (\(generationId, membership) -> heraldMembershipGenerationId membership == generationId)
      (Map.toAscList state.membershipHistory)
    then Right ()
    else Left StructuralProgressInvariantMembership
  if Reconciliation.structuralAppliedMembershipGenerationId state.reconciliation
    == currentMembershipGenerationId state
    && Reconciliation.structuralAppliedMemberSetDigest state.reconciliation
      == currentMemberSetDigest state
    && Set.fromList (Reconciliation.structuralAppliedFixedMembership state.reconciliation)
      == members
    then Right ()
    else Left StructuralProgressInvariantReconciliationMembership
  case Reconciliation.validateStructuralAppliedState state.reconciliation of
    Right () -> Right ()
    Left problem -> Left (StructuralProgressInvariantReconciliation problem)
  if structuralVersionVectorMembershipGenerationId state.appliedVector
    == currentMembershipGenerationId state
    && structuralVersionVectorMemberSetDigest state.appliedVector
      == currentMemberSetDigest state
    && vectorMembers state.appliedVector == members
    && Set.fromList (Map.keys state.history)
      == Set.fromList
        (Reconciliation.structuralAppliedHistoryOccurrences state.reconciliation)
    && all (vectorComponentMatchesHistory state) normalizedMembers
    then Right ()
    else Left StructuralProgressInvariantAppliedVector
  mapM_ validateHistory (Map.toAscList state.history)
  mapM_ validateReportEntry (Map.toAscList state.reports)
  if Map.lookup state.localHerald state.reports
    == ( if state.joiningObserver
           then Nothing
           else
             Just
               ( structuralAppliedReport
                   state.localHerald
                   state.appliedVector
                   state.appliedControlPrefix
               )
       )
    then Right ()
    else Left (StructuralProgressInvariantReport state.localHerald)
  genesis <-
    maybe
      (Left StructuralProgressInvariantMembership)
      Right
      (genesisMembership state)
  validateChain
    state.genesisCutId
    (topologyFrontier (emptyStructuralVersionVector genesis) (controlIndex 0))
    Nothing
    state.installedChain
  if Map.keysSet state.installedCuts == Set.fromList state.installedChain
    && Map.size state.installedCuts == length state.installedChain
    then Right ()
    else
      Left
        ( StructuralProgressInvariantInstalledChain
            state.lastInstalledCutId
        )
  if state.installedCoverageIndex == rebuildInstalledCoverageIndex state
    then Right ()
    else Left StructuralProgressInvariantCoverageIndex
  let expectedLast = case reverse state.installedChain of
        cut : _ -> cut
        [] -> state.genesisCutId
  if state.lastInstalledCutId == expectedLast
    then Right ()
    else Left (StructuralProgressInvariantLastInstalled state.lastInstalledCutId)
  mapM_ validateSupersededOpenCut (Map.toAscList state.supersededOpenCuts)
  case state.openCut of
    Nothing -> Right ()
    Just open -> validateOpen open
  case state.pendingAnnounce of
    Nothing -> Right ()
    Just announce ->
      if announceIdentityValid announce
        then Right ()
        else
          Left
            (StructuralProgressInvariantPendingAnnounce (topologyCutAnnounceId announce))
  validatePendingEstablishedChain
    Set.empty
    state.lastInstalledCutId
    (currentInstalledFrontier state)
    (currentInstalledOccurrenceDigest state)
    state.pendingEstablished
  where
    validateHistory (occurrence, retained)
      | occurrence /= structuralOccurrenceStampOccurrence retained.stamp =
          Left (StructuralProgressInvariantHistory occurrence)
      | not (progressCoversOccurrence occurrence) =
          Left (StructuralProgressInvariantHistory occurrence)
      | not (progressCoversPredecessor (structuralOccurrenceStampPredecessor retained.stamp)) =
          Left (StructuralProgressInvariantHistory occurrence)
      | retained.controlPrerequisite > state.appliedControlPrefix =
          Left (StructuralProgressInvariantHistory occurrence)
      | maybe
          False
          ( (> state.appliedControlPrefix)
              . Reconciliation.structuralRetirementSuppressionControlIndex
          )
          ( Reconciliation.structuralAppliedRetirementSuppressionAt
              occurrence
              state.reconciliation
          ) =
          Left (StructuralProgressInvariantHistory occurrence)
      | not (isInstalledCut retained.sourceTopologyPrerequisite state) =
          Left (StructuralProgressInvariantHistory occurrence)
      | otherwise = Right ()

    validatePendingEstablishedChain _ _ _ _ [] = Right ()
    validatePendingEstablishedChain seen predecessor predecessorFrontier predecessorDigest (established : remaining) =
      let cutId = topologyCutEstablishedId established
          cut = topologyCutEstablishedCut established
          frontier = topologyCutFrontier cut
          valid =
            Set.notMember cutId seen
              && Map.notMember cutId state.installedCuts
              && ordinaryCutPredecessorValid cut
              && topologyPredecessorCutId (topologyCutPredecessor cut) == predecessor
              && establishedEvidenceValidFor state.currentMembership established
              && (cutAdvancesFrom predecessorFrontier predecessorDigest cut || topologyFrontierAppliedControlPrefix frontier > topologyFrontierAppliedControlPrefix predecessorFrontier)
       in if valid
            then
              validatePendingEstablishedChain
                (Set.insert cutId seen)
                cutId
                frontier
                (Just (topologyCutOccurrenceDigest cut))
                remaining
            else Left (StructuralProgressInvariantPendingEstablished cutId)

    progressCoversOccurrence occurrence =
      vectorCoversStructuralOccurrence occurrence state.appliedVector
        || any
          (topologyCutCoversOccurrence occurrence . installedTopologyCutCut)
          (Map.elems state.installedCuts)

    progressCoversPredecessor =
      candidateVectorCoversPredecessor state state.appliedVector

    validateReportEntry (reporter, report)
      | reporter /= structuralAppliedReportReporter report
          || not (reportShapeValid report) =
          Left (StructuralProgressInvariantReport reporter)
      | otherwise = Right ()

    validateSupersededOpenCut (cutId, tombstone)
      | supersededOpenCutValid cutId tombstone = Right ()
      | otherwise = Left (StructuralProgressInvariantSupersededOpenCut cutId)

    supersededOpenCutValid cutId tombstone =
      case Map.lookup tombstone.supersededBy state.installedCuts of
        Just supersedingInstalled
          | Just base <- supersedingInstalled.membershipSuccessorBase,
            let predecessorMembership = heraldMembershipLineageOrigin (successorStructuralBaseLineage base),
            let predecessorId = heraldMembershipGenerationId predecessorMembership,
            MembershipSuccessorPredecessorView
              namedPredecessor
              namedPredecessorGeneration
              namedSuccessorGeneration
              terminalVector
              _
              _ <-
              topologyPredecessorView
                (topologyCutPredecessor (installedTopologyCutCut supersedingInstalled)),
            SameGenerationPredecessorView openPredecessor <-
              topologyPredecessorView (topologyCutPredecessor openCut) ->
              let union =
                    terminalSourceUnionEstablishedUnion
                      (successorStructuralBaseEstablishedUnion base)
                  checkedTerminalVector =
                    terminalSourceUnionTerminalPredecessorVector union
                  terminalControl = terminalSourceUnionTerminalControlPrefix union
               in cutId == topologyCutAnnounceId tombstone.announce
                    && cutId == deriveTopologyCutId openCut
                    && Map.notMember cutId state.installedCuts
                    && tombstone.supersededBy
                      == successorStructuralBaseCutId base
                    && namedPredecessorGeneration == predecessorId
                    && namedSuccessorGeneration == successorStructuralBaseGenerationId base
                    && namedPredecessor == openPredecessor
                    && terminalVector == checkedTerminalVector
                    && topologyCutAnnounceAnnouncer tombstone.announce
                      == NonEmpty.head
                        (heraldMembershipGenerationActiveHeraldEpochs predecessorMembership)
                    && frontierShapeValidFor predecessorMembership openFrontier
                    -- The abandoned proposal can contain survivor work above
                    -- the least terminal closure. Its exact announce remains
                    -- stale evidence; the base does not certify that work.
                    && terminalControl
                      >= topologyFrontierAppliedControlPrefix openFrontier
        _ -> False
      where
        openCut = topologyCutAnnounceCut tombstone.announce
        openFrontier = topologyCutFrontier openCut

    validateChain _ _ _ [] = Right ()
    validateChain predecessor predecessorFrontier predecessorDigest (cutId : remaining) = do
      installed <-
        maybe
          (Left (StructuralProgressInvariantInstalledChain cutId))
          Right
          (Map.lookup cutId state.installedCuts)
      let established = installed.installedEstablished
          cut = topologyCutEstablishedCut established
          frontier = topologyCutFrontier cut
          occurrenceDigest = topologyCutOccurrenceDigest cut
          acceptanceMap =
            Map.fromList
              [ (topologyCutAcceptanceReporter acceptance, acceptance)
              | acceptance <- topologyCutEstablishedAcceptances established
              ]
          expectedNew =
            [ occurrence
            | occurrence <- Map.keys state.history,
              topologyCutCoversOccurrence occurrence cut,
              not (frontierCoversOccurrence occurrence predecessorFrontier)
            ]
          installedGenerationId =
            structuralVersionVectorMembershipGenerationId
              (topologyFrontierStructuralVersionVector frontier)
      cutMembership <-
        maybe
          (Left (StructuralProgressInvariantInstalledChain cutId))
          Right
          (Map.lookup installedGenerationId state.membershipHistory)
      if topologyCutEstablishedId established == cutId
        && deriveTopologyCutId cut == cutId
        && topologyPredecessorCutId (topologyCutPredecessor cut) == predecessor
        && establishedEvidenceValidFor cutMembership established
        && frontierWithinOwner cutMembership frontier
        && cutLineageValid
          installed
          predecessor
          predecessorFrontier
          predecessorDigest
          cut
        && installed.newlyCoveredOccurrences == expectedNew
        && Map.lookup state.localHerald acceptanceMap == installed.ownAcceptance
        && installed.ownAcknowledgement
          == ( if Set.member state.localHerald (membershipSetFor cutMembership)
                 then
                   Just
                     ( topologyCutEstablishedAck
                         cutId
                         state.localHerald
                         installedGenerationId
                     )
                 else Nothing
             )
        && acknowledgementMapValidFor
          cutMembership
          cutId
          installed.installedAcknowledgements
        && Map.lookup state.localHerald installed.installedAcknowledgements
          == installed.ownAcknowledgement
        && installedAcknowledgementsCompleteWhenRequired
          cutMembership
          cutId
          installed
        then validateChain cutId frontier (Just occurrenceDigest) remaining
        else Left (StructuralProgressInvariantInstalledChain cutId)

    validateOpen open =
      let cutId = topologyCutAnnounceId open.announce
          cut = topologyCutAnnounceCut open.announce
          reporters = Map.keysSet open.acceptances
          acceptancesValid =
            and
              [ reporter == topologyCutAcceptanceReporter acceptance
                  && acceptanceEvidenceValidFor
                    state.currentMembership
                    cutId
                    (topologyCutFrontier cut)
                    acceptance
              | (reporter, acceptance) <- Map.toAscList open.acceptances
              ]
          localAccepted = Map.member state.localHerald open.acceptances
          phaseValid = case open.openEstablished of
            Nothing ->
              topologyCutPredecessor cut
                == sameGenerationPredecessor state.lastInstalledCutId
                && Map.null open.openAcknowledgements
                && Map.notMember cutId state.installedCuts
                && ( cutAdvancesFrom
                       (currentInstalledFrontier state)
                       (currentInstalledOccurrenceDigest state)
                       cut
                       || joinCutAdvances open.joinSealAdmission (currentInstalledFrontier state) cut
                   )
            Just established ->
              Just established
                == installedTopologyCutEstablishedFor cutId
                && topologyCutEstablishedId established == cutId
                && topologyCutEstablishedCut established == cut
                && topologyCutEstablishedAcceptances established
                  == Map.elems open.acceptances
                && establishedEvidenceValidFor state.currentMembership established
                && state.lastInstalledCutId == cutId
                && not
                  (Map.keysSet open.openAcknowledgements == memberSet state)
                && case Map.lookup cutId state.installedCuts of
                  Nothing -> False
                  Just installed ->
                    open.openAcknowledgements == installed.installedAcknowledgements
       in if announceIdentityValid open.announce
            && frontierWithinOwner state.currentMembership (topologyCutFrontier cut)
            && Set.isSubsetOf reporters (memberSet state)
            && localAccepted
            && acceptancesValid
            && acknowledgementMapValidFor
              state.currentMembership
              cutId
              open.openAcknowledgements
            && phaseValid
            then Right ()
            else Left (StructuralProgressInvariantOpenCut cutId)

    reportShapeValid = reportShapeValidFor state.currentMembership

    frontierShapeValidFor membership frontier =
      structuralVersionVectorMembershipGenerationId
        (topologyFrontierStructuralVersionVector frontier)
        == heraldMembershipGenerationId membership
        && topologyFrontierMemberSetDigest frontier
          == heraldMembershipGenerationActiveMemberSetDigest membership
        && vectorMembers (topologyFrontierStructuralVersionVector frontier)
          == membershipSetFor membership

    frontierShapeValid = frontierShapeValidFor state.currentMembership

    frontierWithinOwner membership frontier =
      frontierShapeValidFor membership frontier
        && ( heraldMembershipGenerationId membership
               /= currentMembershipGenerationId state
               || structuralVersionVectorCovers
                 state.appliedVector
                 (topologyFrontierStructuralVersionVector frontier)
           )
        && state.appliedControlPrefix
          >= topologyFrontierAppliedControlPrefix frontier

    acceptanceEvidenceValidFor membership cutId frontier acceptance =
      topologyCutAcceptanceId acceptance == cutId
        && reportShapeValidFor membership (topologyCutAcceptanceReport acceptance)
        && reportCoversFrontier (topologyCutAcceptanceReport acceptance) frontier

    establishedEvidenceValidFor membership established =
      let cutId = topologyCutEstablishedId established
          cut = topologyCutEstablishedCut established
          frontier = topologyCutFrontier cut
          acceptances = topologyCutEstablishedAcceptances established
          acceptanceMap =
            Map.fromList
              [ (topologyCutAcceptanceReporter acceptance, acceptance)
              | acceptance <- acceptances
              ]
       in deriveTopologyCutId cut == cutId
            && frontierShapeValidFor membership frontier
            && length acceptances == Map.size acceptanceMap
            && Map.keysSet acceptanceMap == membershipSetFor membership
            && all
              (acceptanceEvidenceValidFor membership cutId frontier)
              acceptances

    acknowledgementMapValidFor membership cutId acknowledgements =
      Set.isSubsetOf
        (Map.keysSet acknowledgements)
        (membershipSetFor membership)
        && and
          [ reporter == topologyCutEstablishedAckReporter acknowledgement
              && acknowledgement
                == topologyCutEstablishedAck
                  cutId
                  reporter
                  (heraldMembershipGenerationId membership)
          | (reporter, acknowledgement) <- Map.toAscList acknowledgements
          ]

    installedAcknowledgementsCompleteWhenRequired membership cutId installed
      | Set.notMember state.localHerald (membershipSetFor membership) = Map.null installed.installedAcknowledgements
      | heraldMembershipGenerationId membership
          /= currentMembershipGenerationId state =
          True
      | Just _ <- installed.membershipSuccessorBase =
          Map.keysSet installed.installedAcknowledgements
            == Set.singleton state.localHerald
      | Just _ <- installed.membershipAdmissionBase =
          Map.keysSet installed.installedAcknowledgements == Set.singleton state.localHerald
      | state.localHerald
          /= NonEmpty.head (heraldMembershipGenerationActiveHeraldEpochs membership) =
          Map.keysSet installed.installedAcknowledgements
            == Set.singleton state.localHerald
      | Map.keysSet installed.installedAcknowledgements
          == membershipSetFor membership =
          True
      | otherwise = case state.openCut of
          Just open ->
            topologyCutAnnounceId open.announce == cutId
              && open.openEstablished == Just installed.installedEstablished
              && open.openAcknowledgements == installed.installedAcknowledgements
          Nothing -> False

    announceIdentityValid announce =
      topologyCutAnnounceAnnouncer announce == NonEmpty.head (currentMembers state)
        && topologyCutAnnounceId announce
          == deriveTopologyCutId (topologyCutAnnounceCut announce)
        && ordinaryCutPredecessorValid (topologyCutAnnounceCut announce)
        && frontierShapeValid
          (topologyCutFrontier (topologyCutAnnounceCut announce))

    installedTopologyCutEstablishedFor cutId =
      installedTopologyCutEstablished
        <$> Map.lookup cutId state.installedCuts

    cutLineageValid installed predecessor predecessorFrontier predecessorDigest cut =
      case topologyPredecessorView (topologyCutPredecessor cut) of
        SameGenerationPredecessorView namedPredecessor ->
          namedPredecessor == predecessor
            && installed.membershipSuccessorBase == Nothing
            && installed.membershipAdmissionBase == Nothing
            && (cutAdvancesFrom predecessorFrontier predecessorDigest cut || joinCutAdvances installed.joinSealAdmission predecessorFrontier cut)
        MembershipSuccessorPredecessorView _ predecessorId successorId _ _ _ ->
          case installed.membershipSuccessorBase of
            Nothing -> False
            Just base ->
              case ( Map.lookup predecessorId state.membershipHistory,
                     Map.lookup successorId state.membershipHistory
                   ) of
                (Just predecessorMembership, Just successorMembership) ->
                  let establishedUnion = successorStructuralBaseEstablishedUnion base
                      union = terminalSourceUnionEstablishedUnion establishedUnion
                      terminalVector =
                        terminalSourceUnionTerminalPredecessorVector union
                      terminalControl = terminalSourceUnionTerminalControlPrefix union
                   in structuralVersionVectorMembershipGenerationId
                        (topologyFrontierStructuralVersionVector predecessorFrontier)
                        == predecessorId
                        && structuralVersionVectorCovers
                          terminalVector
                          (topologyFrontierStructuralVersionVector predecessorFrontier)
                        && terminalControl
                          >= topologyFrontierAppliedControlPrefix predecessorFrontier
                        && heraldMembershipLineageOrigin (successorStructuralBaseLineage base) == predecessorMembership
                        && heraldMembershipLineageTarget (successorStructuralBaseLineage base) == successorMembership
                        && establishedSuccessorStructuralBase
                          (successorStructuralBaseLineage base)
                          establishedUnion
                          cut
                          == Right base
                _ -> False
        AdmissionTopologyPredecessorView namedPredecessor predecessorId successorId _ _ admission digest ->
          case installed.membershipAdmissionBase of
            Nothing -> False
            Just (recipe, record) ->
              namedPredecessor == predecessor
                && Topology.heraldJoinBaseRecipeAdmissionId recipe == admission
                && Topology.heraldJoinBaseRecipeDigest recipe == digest
                && heraldMembershipGenerationId (Topology.heraldJoinBaseRecipeGeneration recipe) == predecessorId
                && admissionCertificateMatches recipe record
                && case Admission.admissionRecordPhase record of
                  Admission.AdmissionActivated index membership ->
                    heraldMembershipGenerationId membership == successorId
                      && Topology.activateHeraldJoinBase index (topologyCutOccurrenceDigest cut) recipe == Right (membership, cut)
                  _ -> False

    cutAdvancesFrom predecessorFrontier predecessorDigest cut =
      let candidate = topologyCutFrontier cut
          candidateVector = topologyFrontierStructuralVersionVector candidate
          predecessorVector =
            topologyFrontierStructuralVersionVector predecessorFrontier
          vectorCovers =
            structuralVersionVectorCovers candidateVector predecessorVector
          vectorAdvanced = candidateVector /= predecessorVector
          candidateControl = topologyFrontierAppliedControlPrefix candidate
          predecessorControl =
            topologyFrontierAppliedControlPrefix predecessorFrontier
          controlCovers = candidateControl >= predecessorControl
          controlAdvanced = candidateControl > predecessorControl
          digestChanged = case predecessorDigest of
            -- Genesis commits an InitialProjectionDigest rather than a
            -- TopologyOccurrenceDigest, so there is no like-typed incumbent
            -- to compare.  The checked proposal/receive transition admits
            -- this control-only first cut only with explicit
            -- topology-consequence evidence; retained opaque state therefore
            -- records the resulting dynamic digest as changed from genesis.
            Nothing -> True
            Just digest -> topologyCutOccurrenceDigest cut /= digest
       in vectorCovers
            && controlCovers
            && (vectorAdvanced || (controlAdvanced && digestChanged))

    -- The checked join checkpoint permits the same control-only advance
    -- before establishment and after it enters the installed chain.
    joinCutAdvances admission predecessorFrontier cut =
      let frontier = topologyCutFrontier cut
          index = topologyFrontierAppliedControlPrefix frontier
       in maybe False ((<= index) . heraldAdmissionControlIndex) admission
            && index > topologyFrontierAppliedControlPrefix predecessorFrontier
            && structuralVersionVectorCovers (topologyFrontierStructuralVersionVector frontier) (topologyFrontierStructuralVersionVector predecessorFrontier)

genesisMembership ::
  StructuralProgressState -> Maybe HeraldMembershipGeneration
genesisMembership state = genesisMembershipFromHistory state.membershipHistory

genesisMembershipFromHistory ::
  Map HeraldMembershipGenerationId HeraldMembershipGeneration -> Maybe HeraldMembershipGeneration
genesisMembershipFromHistory history =
  case [ membership
       | membership <- Map.elems history,
         heraldMembershipGenerationPredecessor membership == Nothing
       ] of
    [membership] -> Just membership
    _ -> Nothing

genesisMembershipInvariant ::
  StructuralProgressState -> HeraldMembershipGeneration
genesisMembershipInvariant state = genesisMembershipFromHistoryInvariant state.membershipHistory

genesisMembershipFromHistoryInvariant ::
  Map HeraldMembershipGenerationId HeraldMembershipGeneration -> HeraldMembershipGeneration
genesisMembershipFromHistoryInvariant history =
  case genesisMembershipFromHistory history of
    Just membership -> membership
    Nothing -> error "structural progress lost its unique genesis membership"

-- | A candidate frontier normally covers a retained predecessor in its own
-- membership generation.  Immediately after a contraction, however, a
-- predecessor-generation occurrence remains legitimate through the exact
-- installed successor base: retired components are sealed by the terminal
-- vector while surviving components may advance in the candidate vector.
--
-- Use the candidate vector rather than the owner's live applied vector.  A
-- stable report matrix must establish the coverage it claims; local progress
-- beyond that matrix cannot make a cut valid.
candidateVectorCoversPredecessor ::
  StructuralProgressState ->
  StructuralVersionVector ->
  StructuralVersionVector ->
  Bool
candidateVectorCoversPredecessor state candidate required =
  structuralVersionVectorCovers candidate required
    || case structuralDescendantSettlement requiredGeneration candidateGeneration state of
      Nothing -> False
      Just witness ->
        all (covered witness) (structuralVersionVectorEntries required)
  where
    requiredGeneration = structuralVersionVectorMembershipGenerationId required
    candidateGeneration = structuralVersionVectorMembershipGenerationId candidate
    covered witness (origin, prefix) =
      case structuralVersionVectorComponent origin candidate of
        Just current -> current >= prefix
        Nothing -> any (terminalCovers origin prefix) witness.bases
    terminalCovers origin prefix base =
      let union = terminalSourceUnionEstablishedUnion (successorStructuralBaseEstablishedUnion base)
       in maybe False (>= prefix) (structuralVersionVectorComponent origin (terminalSourceUnionTerminalPredecessorVector union))

-- | A proof assembled only from actually installed membership bases on the
-- unique established cut chain. An origin may lie inside a compound base;
-- history alone never supplies its settlement evidence.
data StructuralDescendantSettlement = StructuralDescendantSettlement
  { lineage :: HeraldMembershipLineage,
    bases :: [SuccessorStructuralBase]
  }
  deriving stock (Eq, Show)

structuralDescendantSettlementLineage :: StructuralDescendantSettlement -> HeraldMembershipLineage
structuralDescendantSettlementLineage witness = witness.lineage

structuralDescendantSettlement ::
  HeraldMembershipGenerationId -> HeraldMembershipGenerationId -> StructuralProgressState -> Maybe StructuralDescendantSettlement
structuralDescendantSettlement origin target state = do
  history <-
    either (const Nothing) Just . heraldMembershipHistory
      =<< NonEmpty.nonEmpty
        (sortOn (maybe (controlIndex 0) id . heraldMembershipGenerationChangeControlIndex) (Map.elems state.membershipHistory))
  lineage <- either (const Nothing) Just (heraldMembershipLineage origin target history)
  let generations = fmap heraldMembershipGenerationId (NonEmpty.toList (heraldMembershipLineageGenerations lineage))
      pairs = zip generations (drop 1 generations)
      installedBases =
        [ base
        | cutId <- state.installedChain,
          Just installed <- [Map.lookup cutId state.installedCuts],
          Just base <- [installed.membershipSuccessorBase],
          let generation = successorStructuralBaseGenerationId base,
          generation `elem` generations
        ]
      covers (before, after) base =
        let baseGenerations = fmap heraldMembershipGenerationId (NonEmpty.toList (heraldMembershipLineageGenerations (successorStructuralBaseLineage base)))
         in (before, after) `elem` zip baseGenerations (drop 1 baseGenerations)
      admissionPairs =
        [ (heraldMembershipGenerationId (Topology.heraldJoinBaseRecipeGeneration recipe), heraldMembershipGenerationId membership)
        | installed <- Map.elems state.installedCuts,
          Just (recipe, record) <- [installed.membershipAdmissionBase],
          Admission.AdmissionActivated _ membership <- [Admission.admissionRecordPhase record]
        ]
  let targetInstalled =
        target == heraldMembershipGenerationId (genesisMembershipInvariant state)
          || any ((== target) . structuralVersionVectorMembershipGenerationId . topologyFrontierStructuralVersionVector . topologyCutFrontier . installedTopologyCutCut) (Map.elems state.installedCuts)
  if targetInstalled && all (\pair -> pair `elem` admissionPairs || any (covers pair) installedBases) pairs
    then Just StructuralDescendantSettlement {lineage, bases = installedBases}
    else Nothing

-- | Admit immutable survivor work through every installed base separating its
-- origin from the current vector. Current authority is checked at ingress;
-- this proof concerns only preserved historical structural dependencies.
carryDescendantStampedSurvivorOccurrence :: StructuralOccurrenceStamp -> StructuralVersionVector -> StructuralProgressState -> Either StructuralProgressProblem CarriedStampedSurvivor
carryDescendantStampedSurvivorOccurrence stamp current state = do
  let predecessor = structuralOccurrenceStampPredecessor stamp
      origin = structuralVersionVectorMembershipGenerationId predecessor
      target = structuralVersionVectorMembershipGenerationId current
      source = structuralOccurrenceSourceHeraldEpoch (structuralOccurrenceStampOccurrence stamp)
  witness <- maybe (Left StructuralProgressPredecessorNotCovered) Right (structuralDescendantSettlement origin target state)
  unless
    (vectorMatches (heraldMembershipLineageOrigin witness.lineage) predecessor && vectorMatches (heraldMembershipLineageTarget witness.lineage) current)
    (Left StructuralProgressPredecessorMismatch)
  unless (candidateVectorCoversPredecessor state current predecessor) (Left StructuralProgressPredecessorNotCovered)
  base <- maybe (Left StructuralProgressSuccessorBaseMismatch) Right (find ((== target) . successorStructuralBaseGenerationId) witness.bases)
  advance <- mapLeft StructuralProgressTerminalSourceProblem (allocateSuccessorStructuralOccurrence source current base)
  mapLeft StructuralProgressTerminalSourceProblem (carriedStampedSurvivorFromAdvance stamp advance)
  where
    vectorMatches membership vector =
      structuralVersionVectorMembershipGenerationId vector == heraldMembershipGenerationId membership
        && structuralVersionVectorMemberSetDigest vector == heraldMembershipGenerationActiveMemberSetDigest membership
        && vectorMembers vector == membershipSetFor membership
