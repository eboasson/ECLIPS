{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Canonical semantic onboarding material. A bundle contains immutable
-- occurrences, private-system-view observations, historical placement and
-- alignment facts, never an owner's private representation, application store,
-- possession map, or physical connection.
module Eclips.Herald.Join.History
  ( JoinHistory,
    JoinHistoryProblem (..),
    captureJoinHistory,
    captureJoinHistoryWithDiagnostics,
    encodeJoinHistory,
    decodeJoinHistory,
    JoinSourceHistory,
    decodeJoinSourceHistory,
    encodeJoinSourceHistory,
    sourceJoinHistory,
    joinSourceMemberCut,
    joinHistorySystem,
    joinHistoryAdmission,
    joinHistoryAttempt,
    joinHistorySource,
    joinHistoryControlPrefix,
    joinHistoryControlAnchor,
    joinHistoryTopologyCut,
    joinHistoryControlEntries,
    joinHistoryControlSuffixAfter,
    joinHistoryTopologyCertificates,
    joinHistoryTerminalCertificates,
    joinHistoryOccurrences,
    joinHistorySealCheckpoints,
    joinHistoryPlacementSnapshots,
    joinHistoryAlignmentHistory,
    alignmentFrontierAdmitted,
    adoptJoinAlignmentFrontier,
    advanceJoinHistoryData,
    advanceJoinHistoriesData,
    installJoinHistory,
    validateJoinHistoryContext,
    joinHistoryInstalled,
  ) where

import Control.Monad (foldM, unless)
import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Serialize (Serialize)
import Data.Serialize qualified as S
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Alignment
import Eclips.Domain.Identity
import Eclips.Domain.Membership
import Eclips.Domain.Publication
import Eclips.Domain.Route (ReplicaStrength (Normal))
import Eclips.Domain.Sort.Profile
import Eclips.Domain.Startup (appliedProcessEnvironmentPublications)
import Eclips.Domain.Structural
import Eclips.Domain.Topology
import Eclips.Domain.Value
import Eclips.Herald.Alignment.History qualified as AlignmentHistory
import Eclips.Herald.Alignment.Plan qualified as AlignmentPlan
import Eclips.Herald.Alignment.Protocol qualified as Evidence
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.DiagnosticChecks (DiagnosticChecks (..), runDiagnosticCheck)
import Eclips.Herald.EffectBatch (EffectBatch)
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.Progress qualified as Progress
import Eclips.Herald.Graph.Protocol qualified as GraphProtocol
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource qualified as Terminal
import Eclips.Herald.Join.Replay qualified as Replay
import Eclips.Herald.Join.SourceBundle qualified as SourceBundle
import Eclips.Herald.Join.State qualified as Join
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Peer.RPC.Internal qualified as Bridge
import Eclips.Herald.PeerPublication qualified as Peer
import Eclips.Herald.Placement qualified as Placement
import Eclips.Herald.Placement.State qualified as PlacementState
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as Registry
import Eclips.Herald.Startup.State
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt (sortOccurrence)
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.UseCase.AlignmentHistory qualified as AlignmentReplay
import Eclips.Herald.UseCase.PeerInput qualified as PeerInput
import Eclips.Herald.UseCase.Step15StructuralBase qualified as Base
import Eclips.Herald.UseCase.StructuralProgress qualified as StructuralWork
import Eclips.Oracle.Admission
import Eclips.Oracle.Canonical
import Eclips.Oracle.Projection (appliedEntryControlIndex)
import Eclips.Protocol.Peer.Types qualified as Wire
import GHC.Generics (Generic)

-- System-view Store revisions are source coordinates only. Installation replays
-- their observations into the receiver's fresh incarnation/revision history;
-- historical placement revisions retain their original owner coordinates.
data SystemViewHistory = SystemViewHistory PredefinedSortRole Word64 [Evidence.RetainedPublicationEvidence]
  deriving stock (Eq, Show)

-- A covered prefix comes from the independently admitted Projection base.
-- Only the contiguous suffix is replay material; sparse exact request evidence
-- belongs to that base's archive, not to this transcript.
data JoinControlHistory = JoinControlHistory !ControlIndex ![CanonicalAppliedOracleEntry]
  deriving stock (Eq, Show)
data JoinHistory = JoinHistory SystemId HeraldAdmissionId Word64 HeraldEpoch ControlIndex TopologyCut Word64 Word64 JoinControlHistory [(Maybe HeraldAdmissionId, GraphProtocol.TopologyCutEstablished)] [Terminal.TerminalSourceUnionEstablished] [Terminal.TerminalStructuralOccurrence] [SystemViewHistory] [Placement.PlacementSnapshot] AlignmentHistory.AlignmentHistory
  deriving stock (Eq, Show)
data JoinHistoryProblem
  = JoinHistoryMalformed
  | JoinHistoryWrongAdmission
  | JoinHistoryWrongSystem
  | JoinHistorySourceNotOldMember
  | JoinHistoryCanonicalControlGap
  | JoinHistoryOccurrenceConflict StructuralOccurrenceId
  | JoinHistorySourceMaterialMissing
  | JoinHistoryOwnerProblem String
  deriving stock (Eq, Show)

joinHistorySystem :: JoinHistory -> SystemId
joinHistorySystem (JoinHistory x _ _ _ _ _ _ _ _ _ _ _ _ _ _) = x
joinHistoryAdmission :: JoinHistory -> HeraldAdmissionId
joinHistoryAdmission (JoinHistory _ x _ _ _ _ _ _ _ _ _ _ _ _ _) = x
joinHistoryAttempt :: JoinHistory -> Word64
joinHistoryAttempt (JoinHistory _ _ x _ _ _ _ _ _ _ _ _ _ _ _) = x
joinHistorySource :: JoinHistory -> HeraldEpoch
joinHistorySource (JoinHistory _ _ _ x _ _ _ _ _ _ _ _ _ _ _) = x
joinHistoryControlPrefix :: JoinHistory -> ControlIndex
joinHistoryControlPrefix (JoinHistory _ _ _ _ x _ _ _ _ _ _ _ _ _ _) = x
joinHistoryTopologyCut :: JoinHistory -> TopologyCut
joinHistoryTopologyCut (JoinHistory _ _ _ _ _ x _ _ _ _ _ _ _ _ _) = x
joinHistoryControlEntries :: JoinHistory -> [CanonicalAppliedOracleEntry]
joinHistoryControlEntries (JoinHistory _ _ _ _ _ _ _ _ (JoinControlHistory _ entries) _ _ _ _ _ _) = entries
joinHistoryControlAnchor :: JoinHistory -> ControlIndex
joinHistoryControlAnchor (JoinHistory _ _ _ _ _ _ _ _ (JoinControlHistory anchor _) _ _ _ _ _ _) = anchor

-- The checked transcript owns canonical entry coordinates. A caller selects
-- a covered receiver cursor without interpreting Oracle semantic entries.
joinHistoryControlSuffixAfter :: ControlIndex -> JoinHistory -> Maybe [CanonicalAppliedOracleEntry]
joinHistoryControlSuffixAfter after history
  | after < joinHistoryControlAnchor history || after > joinHistoryControlPrefix history = Nothing
  | otherwise = Just (dropWhile ((<= after) . appliedEntryControlIndex . canonicalAppliedOracleEntryValue) (joinHistoryControlEntries history))
joinHistoryTopologyCertificates :: JoinHistory -> [GraphProtocol.TopologyCutEstablished]
joinHistoryTopologyCertificates (JoinHistory _ _ _ _ _ _ _ _ _ x _ _ _ _ _) = map snd x
joinHistorySealCheckpoints :: JoinHistory -> [(TopologyCutId, HeraldAdmissionId)]
joinHistorySealCheckpoints (JoinHistory _ _ _ _ _ _ _ _ _ cuts _ _ _ _ _) = [(GraphProtocol.topologyCutEstablishedId established, admission) | (Just admission, established) <- cuts]
joinHistoryTerminalCertificates :: JoinHistory -> [Terminal.TerminalSourceUnionEstablished]
joinHistoryTerminalCertificates (JoinHistory _ _ _ _ _ _ _ _ _ _ x _ _ _ _) = x
joinHistoryOccurrences :: JoinHistory -> [Terminal.TerminalStructuralOccurrence]
joinHistoryOccurrences (JoinHistory _ _ _ _ _ _ _ _ _ _ _ x _ _ _) = x
joinHistoryViews :: JoinHistory -> [SystemViewHistory]
joinHistoryViews (JoinHistory _ _ _ _ _ _ _ _ _ _ _ _ x _ _) = x

-- | Canonical source envelope and its checked nested History. The other owner
-- frames remain opaque here; portable snapshot admission belongs to ControlBase.
-- The immutable aggregate bytes, not only History, determine the sealed receipt.
data JoinSourceHistory = JoinSourceHistory !ByteString !JoinHistory !HeraldJoinMemberCut
  deriving stock (Eq, Show)

decodeJoinSourceHistory :: ByteString -> Either JoinHistoryProblem JoinSourceHistory
decodeJoinSourceHistory bytes = do
  bundle <- owner (SourceBundle.decodeJoinSourceBundle bytes)
  history@(JoinHistory _ _ _ _ _ _ stamped input _ _ _ _ _ _ _) <- decodeJoinHistory (SourceBundle.joinSourceBundleHistoryFrame bundle)
  let !retained = BS.copy bytes
      !memberCut = heraldJoinMemberCut stamped input (deriveHeraldJoinDigest retained)
  pure (JoinSourceHistory retained history memberCut)

encodeJoinSourceHistory :: JoinSourceHistory -> ByteString
encodeJoinSourceHistory (JoinSourceHistory bytes _ _) = bytes

sourceJoinHistory :: JoinSourceHistory -> JoinHistory
sourceJoinHistory (JoinSourceHistory _ history _) = history

joinSourceMemberCut :: JoinSourceHistory -> HeraldJoinMemberCut
joinSourceMemberCut (JoinSourceHistory _ _ memberCut) = memberCut

joinHistoryPlacementSnapshots :: JoinHistory -> [Placement.PlacementSnapshot]
joinHistoryPlacementSnapshots (JoinHistory _ _ _ _ _ _ _ _ _ _ _ _ _ snapshots _) = snapshots

joinHistoryAlignmentHistory :: JoinHistory -> AlignmentHistory.AlignmentHistory
joinHistoryAlignmentHistory (JoinHistory _ _ _ _ _ _ _ _ _ _ _ _ _ _ history) = history

-- | Capture only after the finite ready source work has drained and a current
-- installed K covers every already allocated source occurrence. A held
-- unsequenced stage/unknown future definition is not a drain obligation.
captureJoinHistory :: HeraldAdmissionRecord -> HeraldState -> Either JoinHistoryProblem (Maybe JoinHistory)
captureJoinHistory = captureJoinHistoryWithDiagnostics DiagnosticChecksEnabled

-- | Omit only the completed local transcript audit when requested. Source
-- eligibility, pending work and frontier readiness still determine capture.
captureJoinHistoryWithDiagnostics :: DiagnosticChecks -> HeraldAdmissionRecord -> HeraldState -> Either JoinHistoryProblem (Maybe JoinHistory)
captureJoinHistoryWithDiagnostics checks admission state = do
  require (admissionManifestSystem (admissionRecordManifest admission) == system) JoinHistoryWrongSystem
  require (source `elem` oldMembers) JoinHistorySourceNotOldMember
  require (Projection.oracleViewHeraldAdmission (admissionRecordId admission) view == Just admission) JoinHistoryWrongAdmission
  require (Projection.oracleViewCurrentHeraldMembership view == admissionRecordPredecessor admission) JoinHistoryWrongAdmission
  readySource <- owner (StructuralWork.advanceNextReadyApplicationStructuralStage state)
  case readySource of
    Just _ -> pure Nothing
    Nothing | any (\(decision, _) -> not (Projection.oracleViewLabelWorkflowCompleted decision view)) (Projection.projectedLabelWorkflows (startupOracleProjectionState state)) -> pure Nothing
    Nothing -> case lookup (Progress.structuralLastInstalledCutId progress) (Progress.structuralInstalledCutEntries progress) of
      Nothing -> pure Nothing
      Just installed -> do
        let cut = Progress.installedTopologyCutCut installed
            frontier = topologyCutFrontier cut
        if topologyFrontierMembershipGenerationId frontier /= heraldMembershipGenerationId (admissionRecordPredecessor admission)
          || topologyFrontierAppliedControlPrefix frontier < admissionRecordSemanticToken admission
          || topologyFrontierStructuralVersionVector frontier /= Progress.structuralAppliedVector progress
          || stamped > component source (topologyFrontierStructuralVersionVector frontier)
          then pure Nothing
          else do
            imported <- traverse (fmap sourceJoinHistory . decodeJoinSourceHistory . snd) (Join.installedHistories (startupJoinState state))
            alignment <-
              owner
                ( AlignmentHistory.withAlignmentHistoryPlanAcceptances
                    (Join.importedPlanAcceptances (startupJoinState state) <> concatMap (AlignmentHistory.alignmentHistoryPlanAcceptances . joinHistoryAlignmentHistory) imported)
                    (AlignmentHistory.captureAlignmentHistory (startupAlignmentState state))
                )
            if source == minimum oldMembers && not (alignmentFrontierAdmitted oldMembers alignment)
              then pure Nothing
              else do
                entries <- traverse controlEntry [controlIndexWord64 anchor + 1 .. controlIndexWord64 control]
                let controls = JoinControlHistory anchor entries
                occurrences <- captureOccurrences imported state
                snapshots <- traverse captureView systemSlots
                let history = JoinHistory system (admissionRecordId admission) (admissionRecordAttempt admission) source control cut stamped input controls [(Progress.installedTopologyCutJoinSealAdmission retainedCut, established) | established <- Progress.structuralInstalledEstablishedChain progress, Just retainedCut <- [lookup (GraphProtocol.topologyCutEstablishedId established) (Progress.structuralInstalledCutEntries progress)]] (map Terminal.successorStructuralBaseEstablishedUnion (Progress.structuralInstalledSuccessorBases progress)) occurrences snapshots (PlacementState.retainedPlacementSnapshots (startupPlacementState state)) alignment
                runDiagnosticCheck checks (validateHistory history)
                pure (Just history)
  where
    source = Genesis.checkedLocalHeraldEpoch (startupGenesis state)
    system = Genesis.checkedSystemId (startupGenesis state)
    view = Projection.oracleView (startupOracleProjectionState state)
    progress = startupStructuralProgressState state
    publication = startupPublicationState state
    control = Projection.oracleViewControlIndex view
    anchor = Projection.projectionCanonicalCoveredThrough (startupOracleProjectionState state)
    oldMembers = NE.toList (heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor admission))
    stamped = maximum (0 : [structuralSequenceWord64 (structuralOccurrenceSourceSequence (Peer.structuralOccurrenceStampOccurrence (Publication.stampedStructuralStamp stage))) | (_, stage) <- Publication.stampedStructuralStageEntries publication])
    input = minimum (publishedPrefix : [Publication.heraldPublicationPositionWord64 (Publication.structuralSourceHeraldPosition stage) - 1 | stage <- Publication.unstampedStructuralSourceStageEntries publication])
    publishedPrefix = case Publication.publicationCurrentHeraldPrefix publication of EmptyHeraldPublicationPrefix -> 0; HeraldPublicationPrefixThrough position -> heraldPublicationPositionWord64 position
    systemSlots = sortOn (predefinedSortRoleTag . fst) [(role, slot) | slot <- Store.storeSlots (startupStoreState state), Store.HeraldSystemView ownerEpoch role <- [Store.storeSlotProvenance slot], ownerEpoch == source]
    controlEntry index = maybe (Left JoinHistoryCanonicalControlGap) Right (Projection.appliedEntryEvidence (controlIndex index) (startupOracleProjectionState state))
    captureView (role, slot) = do
      observations <- traverse observationEvidence (Store.storeSlotApplicationObservations slot)
      -- An accepted ordinary system-view publication may still be in transit
      -- to every destination. Its source owns the immutable semantic bytes,
      -- so include that ready input in the source cut independently of delivery.
      -- Unsequenced structural stages and ordinary application delta sorts do
      -- not enter this mandatory six-view transfer.
      emitted <- traverse outgoingEvidence [record | (_, record) <- Publication.outgoingPublicationEntries publication, checkedPublicationSort (Publication.outgoingPublicationChecked record) == profileSortFor role]
      let normalized = Map.elems (Map.fromList [(Evidence.retainedPublicationEvidenceCanonicalBytes observation, observation) | observation <- observations <> emitted])
      pure (SystemViewHistory role (storeRevisionWord64 (Store.storeSlotRevision slot)) normalized)
    outgoingEvidence record =
      let value = Publication.outgoingPublicationChecked record
       in owner (Evidence.retainedPublicationEvidence (checkedPublicationId value) (Publication.outgoingPublicationSourceProcess record) (checkedPublicationSort value) (Publication.outgoingPublicationOccurrenceId record) (checkedPublicationCanonicalValue value) (Publication.outgoingPublicationSourceStrength record) (Publication.outgoingPublicationSourceTopologyPrerequisite record) (Publication.outgoingPublicationControlPrerequisite record) Nothing)

-- | A canonical frontier is handed off only after its still-current original
-- fixed members admitted the exact plans. This waits for cut acceptance, not
-- Store readiness or input closure. Earlier source captures do not block old
-- anchor replay. Empty current plans require the same exact coordinate proof;
-- superseded historical plans are not prerequisites.
-- Inherited acceptance facts remain passive proof so a former newcomer can
-- become the next least source without impersonating an old fixed member.
alignmentFrontierAdmitted :: [HeraldEpoch] -> AlignmentHistory.AlignmentHistory -> Bool
alignmentFrontierAdmitted currentMembers history =
  all admitted (map snd (AlignmentHistory.alignmentHistoryFrontier history))
  where
    plans = Map.fromList [(Evidence.alignmentPlanAnnounceId plan, plan) | plan <- AlignmentHistory.alignmentHistoryPlans history]
    acceptances = Set.fromList [(Evidence.alignmentPlanAcceptedId accepted, Evidence.alignmentPlanAcceptedHerald accepted) | accepted <- AlignmentHistory.alignmentHistoryPlanAcceptances history]
    current = Set.fromList currentMembers
    admitted identifier = case Map.lookup identifier plans of
      Nothing -> False
      Just _ -> all (\member -> Set.member (identifier, member) acceptances) (Set.toAscList (Set.intersection current (Set.fromList (map fst (NE.toList (physicalPlacementRevisionEntries (AlignmentPlan.alignmentPlanIdPlacement identifier)))))))

captureOccurrences :: [JoinHistory] -> HeraldState -> Either JoinHistoryProblem [Terminal.TerminalStructuralOccurrence]
captureOccurrences imported state = do
  local <- traverse sourceOccurrence (Publication.stampedStructuralStageEntries publication)
  incoming <- traverse incomingOccurrence [peer | (_, record) <- Publication.incomingPublicationEntries publication, Publication.incomingPublicationSemanticallyAuthenticated record, let peer = Publication.incomingPeerPublication record, isJust (Peer.peerPublicationStructuralStamp peer)]
  let terminal = maybe [] (map snd . Base.structuralBasePayloadEntries) (startupStructuralBaseCoordinator state)
      inherited = Join.importedOccurrences (startupJoinState state) <> concatMap joinHistoryOccurrences imported
  Map.elems <$> foldM insert Map.empty (local <> incoming <> terminal <> inherited)
  where
    publication = startupPublicationState state
    sourceOccurrence (_, stage) = do
      entry <- maybe (Left JoinHistorySourceMaterialMissing) Right (Registry.lookupEffectiveSort (checkedPublicationSort (Publication.stampedStructuralChecked stage)) (startupSortRegistryState state))
      bytes <- owner (Peer.structuralPublicationCanonicalBytesForSemantics (Registry.registryEntryDescriptor entry) (Publication.stampedStructuralSortOccurrenceId stage) (Publication.stampedStructuralChecked stage) (Publication.stampedStructuralSourceProcess stage) (Publication.stampedStructuralSourceStrength stage) (Publication.stampedStructuralSourceTopologyPrerequisite stage) (Publication.stampedStructuralControlPrerequisite stage))
      owner (Terminal.terminalStructuralOccurrence (Publication.stampedStructuralStamp stage) bytes)
    incomingOccurrence peer = case (Peer.peerPublicationStructuralStamp peer, Peer.peerPublicationStructuralCanonicalBytes peer) of
      (Just stamp, Just bytes) -> owner (Terminal.terminalStructuralOccurrence stamp bytes)
      _ -> Left JoinHistorySourceMaterialMissing
    insert retained occurrence = case Map.lookup identifier retained of
      Nothing -> pure (Map.insert identifier occurrence retained)
      Just old | old == occurrence -> pure retained
      _ -> Left (JoinHistoryOccurrenceConflict identifier)
      where
        identifier = Terminal.terminalStructuralOccurrenceId occurrence

observationEvidence :: Store.RetainedStoreObservation -> Either JoinHistoryProblem Evidence.RetainedPublicationEvidence
observationEvidence observation = case Store.retainedStoreObservationOrigin observation of
  Store.PrimordialStoreObservation -> pure (Evidence.primordialRetainedPublicationEvidence identifier (checkedPublicationSort publication) occurrence (checkedPublicationCanonicalValue publication) strength)
  Store.RoutedStoreObservation envelope -> owner (Evidence.retainedPublicationEvidence identifier (Store.storeObservationEnvelopeSourceProcess envelope) (checkedPublicationSort publication) occurrence (checkedPublicationCanonicalValue publication) strength (Store.storeObservationEnvelopeSourceTopology envelope) (Store.storeObservationEnvelopeControlPrerequisite envelope) (Store.storeObservationEnvelopeStructuralStamp envelope))
  where
    publication = Store.retainedStoreObservationPublication observation
    identifier = checkedPublicationId publication
    occurrence = Store.retainedStoreObservationSortOccurrence observation
    strength = Store.retainedStoreObservationIncomingStrength observation

-- | Imported data is re-admitted to locally owned system views. The coordinator
-- projects contiguous control and installs the structural history/certificates
-- before invoking this final data-cut installation. Ordinary application delta
-- stores are never transfer destinations.
installJoinHistory :: JoinHistory -> HeraldState -> Either JoinHistoryProblem (HeraldState, EffectBatch)
installJoinHistory history initial = do
  validateJoinHistoryContext history initial
  advanceJoinHistoryData history initial

-- | Check the admission and canonical control overlap without repeating data
-- import. A multi-source transaction shares that import's dependency fixed point.
validateJoinHistoryContext :: JoinHistory -> HeraldState -> Either JoinHistoryProblem ()
validateJoinHistoryContext history initial = do
  require (joinHistorySystem history == Genesis.checkedSystemId (startupGenesis initial)) JoinHistoryWrongSystem
  let view = Projection.oracleView (startupOracleProjectionState initial)
  record <- maybe (Left JoinHistoryWrongAdmission) Right (Projection.oracleViewHeraldAdmission (joinHistoryAdmission history) view)
  require (admissionRecordAttempt record == joinHistoryAttempt history && case admissionRecordPhase record of AdmissionCancelled {} -> False; _ -> True) JoinHistoryWrongAdmission
  require (joinHistorySource history `elem` NE.toList (heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor record))) JoinHistorySourceNotOldMember
  require (historyControlsInstalled history (startupOracleProjectionState initial)) JoinHistoryCanonicalControlGap

-- Both application and readiness interpret the same exact nominal evidence.
-- Authority admission remains in installObservation; the readiness fallback
-- requires its previously retained canonical Join receipt before this decoder.
checkedHistoryObservation :: Registry.RegistryEntry -> Evidence.RetainedPublicationEvidence -> Either JoinHistoryProblem Store.RetainedStoreObservation
checkedHistoryObservation entry evidence = do
  require (Evidence.retainedPublicationEvidenceSortId evidence == Registry.registryEntrySortId entry && Evidence.retainedPublicationEvidenceSortDefinitionOccurrenceId evidence == Registry.registryEntryOccurrenceId entry) JoinHistoryMalformed
  value <- owner (decodeCanonicalValue (canonicalValueByteString (Evidence.retainedPublicationEvidenceCanonicalValue evidence)))
  checked <- owner (mkCheckedPublication (Registry.registryEntryDescriptor entry) (Evidence.retainedPublicationEvidencePublicationId evidence) value)
  require (checkedPublicationCanonicalValue checked == Evidence.retainedPublicationEvidenceCanonicalValue evidence) JoinHistoryMalformed
  let occurrence = Evidence.retainedPublicationEvidenceSortDefinitionOccurrenceId evidence
      strength = Evidence.retainedPublicationEvidenceSourceStrength evidence
      origin = case Evidence.retainedPublicationEvidenceOrigin evidence of
        Evidence.PrimordialRetainedObservation -> Store.PrimordialStoreObservation
        Evidence.RoutedRetainedObservation source topology control stamp -> Store.RoutedStoreObservation (Store.storeObservationEnvelope source occurrence topology control stamp)
  owner (Store.retainedStoreObservation checked strength occurrence origin)

installObservation :: PredefinedSortRole -> HeraldState -> Evidence.RetainedPublicationEvidence -> Either JoinHistoryProblem HeraldState
installObservation role state evidence = do
  let registry = startupSortRegistryState state
      local = Genesis.checkedLocalHeraldEpoch (startupGenesis state)
  entry <- maybe (Left JoinHistorySourceMaterialMissing) Right (Registry.lookupPredefinedRole role registry)
  observation <- checkedHistoryObservation entry evidence
  let checked = Store.retainedStoreObservationPublication observation
      occurrence = Store.retainedStoreObservationSortOccurrence observation
      strength = Store.retainedStoreObservationIncomingStrength observation
  case Evidence.retainedPublicationEvidenceOrigin evidence of
    Evidence.PrimordialRetainedObservation -> do
      let bootstraps = Projection.oracleViewAppliedBootstraps (Projection.oracleView (startupOracleProjectionState state))
          canonicalEnvironment = [publication | bootstrap <- bootstraps, (_, _, publication) <- appliedProcessEnvironmentPublications (Genesis.checkedSystemId (startupGenesis state)) bootstraps bootstrap]
      require ((strength == Normal && checked `elem` Store.primordialSeedPublications (startupStoreState state)) || checked `elem` canonicalEnvironment) JoinHistoryMalformed
    Evidence.RoutedRetainedObservation source topology control stamp -> owner (PeerInput.validateRetainedStorePublication (PeerInput.routedPublicationAuthorityClaim (Registry.registryEntryDescriptor entry) occurrence checked source strength topology control stamp))
  slot <- maybe (Left JoinHistorySourceMaterialMissing) Right (find (\candidate -> Store.storeSlotProvenance candidate == Store.HeraldSystemView local role) (Store.storeSlots (startupStoreState state)))
  prepared <- owner (Store.prepareAlignmentStoreApplication (Store.peerStoreDestination (Store.storeSlotDelta slot) (Store.storeSlotIncarnation slot) strength :| []) observation (startupStoreState state))
  let (store, _) = Store.commitPeerStoreApplication prepared
      retained = Join.retainImportedObservation role evidence (startupJoinState state)
      updated = retained `seq` replaceStartupJoinState retained (replaceStartupStoreState store state)
  if role == SortDefinitionRole
    then installDefinition evidence checked updated
    else pure updated

installDefinition :: Evidence.RetainedPublicationEvidence -> CheckedPublication -> HeraldState -> Either JoinHistoryProblem HeraldState
installDefinition evidence publication state = do
  descriptor <- owner (decodeSortDefinitionValue (checkedPublicationValue publication))
  plan <- owner (Registry.planSortInduction (Genesis.checkedSystemId (startupGenesis state)) descriptor (startupSortRegistryState state))
  -- Old definition observations stay retained but do not reintroduce a retired
  -- definition occurrence; only a publication after its exact release may do so.
  if fromMaybe (controlIndex 0) (Evidence.retainedPublicationEvidenceControlPrerequisite evidence) < Registry.sortInductionPlanControlPrerequisite plan
    then pure state
    else do
      prepared <- owner (Registry.prepareSortInduction descriptor (Registry.sortInductionPlanOccurrenceId plan) publication (startupSortRegistryState state))
      pure (replaceStartupSortRegistryState (Registry.commitSortInduction prepared) state)

-- Every admitted byte and exact seal coordinate must already be held locally.
-- The caller retains the canonical bundle as the system-view cut receipt.
joinHistoryInstalled :: JoinHistory -> HeraldState -> Bool
joinHistoryInstalled history state =
  -- Capture/import checks the immutable control transcript once. Readiness
  -- needs only its semantic cursor; later reclamation cannot change that prefix.
  Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState state)) >= joinHistoryControlPrefix history
    && deriveTopologyCutId (joinHistoryTopologyCut history) `elem` Progress.structuralInstalledCutIds progress
    && all
      ( \occurrence ->
          not (historyOccurrenceRequired history occurrence) || case Progress.lookupAppliedStructuralOccurrence (Terminal.terminalStructuralOccurrenceId occurrence) progress of
            Just applied -> Progress.appliedStructuralOccurrenceStamp applied == Terminal.terminalStructuralOccurrenceStamp occurrence
            Nothing -> False
      )
      (joinHistoryOccurrences history)
    && all viewInstalled (joinHistoryViews history)
    && AlignmentReplay.alignmentHistoryInstalled (joinHistoryAlignmentHistory history) state
    && all placementInstalled (joinHistoryPlacementSnapshots history)
  where
    progress = startupStructuralProgressState state
    local = Genesis.checkedLocalHeraldEpoch (startupGenesis state)
    placementInstalled snapshot =
      PlacementState.placementRoutesAtRevision (Placement.placementSnapshotOwner snapshot) (Placement.placementSnapshotSequence snapshot) (startupPlacementState state) == Just (Placement.placementSnapshotRoutes snapshot)
    viewInstalled (SystemViewHistory role _ observations) = case find (\slot -> Store.storeSlotProvenance slot == Store.HeraldSystemView local role) (Store.storeSlots (startupStoreState state)) of
      Nothing -> False
      Just slot -> all (observationInstalled role slot) observations
    observationInstalled role slot evidence =
      any (\observation -> observationEvidence observation == Right evidence) (Store.storeSlotApplicationObservations slot)
        || ( Join.hasImportedObservation role evidence (startupJoinState state)
               && case Registry.lookupPredefinedRole role (startupSortRegistryState state) of
                 Nothing -> False
                 Just entry -> case checkedHistoryObservation entry evidence of
                   Left _ -> False
                   Right observation -> Store.currentStoreObservationSuppressed (Store.storeSlotDelta slot) (Store.storeSlotIncarnation slot) observation (startupStoreState state)
           )

-- Exact retained bytes take precedence over covered duplicates. The cursor
-- proves the anchor was actually installed; decoding an anchor alone grants
-- no permission to skip controls or issue an installed-history receipt.
historyControlsInstalled :: JoinHistory -> Projection.State -> Bool
historyControlsInstalled history projection =
  Projection.oracleViewControlIndex (Projection.oracleView projection) >= joinHistoryControlPrefix history
    && all installed (joinHistoryControlEntries history)
  where
    installed entry = case Projection.classifyAppliedEntry entry projection of
      Projection.AppliedEntryExactDuplicate _ -> True
      Projection.AppliedEntryCoveredByBase _ -> True
      _ -> False

-- Canonical outer transcript; embedded owner DTOs are decoded and checked by
-- the existing nominal bridge. No generic owner state is serializable.
data RawView = RawView Word8 Word64 [ByteString]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
data RawControlHistory = RawControlHistory Word64 [ByteString]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
data RawHistory = RawHistory ByteString ByteString Word64 ByteString Word64 ByteString Word64 Word64 RawControlHistory [(Maybe ByteString, ByteString)] [ByteString] [ByteString] [RawView] [ByteString] ByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
rawHistory :: JoinHistory -> RawHistory
rawHistory (JoinHistory system admission attempt source control cut stamped input (JoinControlHistory anchor entries) cuts terminals occurrences views placements alignment) = RawHistory (systemIdBytes system) (heraldAdmissionIdCanonicalBytes admission) attempt (heraldEpochBytes source) (controlIndexWord64 control) (topologyCutCanonicalBytes cut) stamped input (RawControlHistory (controlIndexWord64 anchor) (map canonicalAppliedOracleEntryBytes entries)) [(heraldAdmissionIdCanonicalBytes <$> cutAdmission, GraphProtocol.topologyCutEstablishedCanonicalBytes established) | (cutAdmission, established) <- cuts] (map Terminal.terminalSourceUnionEstablishedCanonicalBytes terminals) (map Terminal.terminalStructuralOccurrenceCanonicalBytes occurrences) [RawView (predefinedSortRoleTag role) revision (map encodeEvidence observations) | SystemViewHistory role revision observations <- views] (map encodePlacementSnapshot placements) (AlignmentHistory.encodeAlignmentHistory alignment)
encodeJoinHistory :: JoinHistory -> ByteString
encodeJoinHistory history = S.encode ("ECLIPS-HERALD-JOIN-HISTORY" :: ByteString, rawHistory history)
decodeJoinHistory :: ByteString -> Either JoinHistoryProblem JoinHistory
decodeJoinHistory bytes = do
  (domain, RawHistory system admission attempt source control cut stamped input (RawControlHistory anchor entries) cuts terminals occurrences views placements alignment) <- shape (S.decode bytes)
  require (domain == ("ECLIPS-HERALD-JOIN-HISTORY" :: ByteString)) JoinHistoryMalformed
  history <- JoinHistory <$> shape (mkSystemId system) <*> shape (decodeHeraldAdmissionIdCanonicalBytes admission) <*> pure attempt <*> shape (mkHeraldEpoch source) <*> pure (controlIndex control) <*> shape (decodeTopologyCutCanonicalBytes cut) <*> pure stamped <*> pure input <*> (JoinControlHistory (controlIndex anchor) <$> traverse (shape . decodeCanonicalAppliedOracleEntry) entries) <*> traverse (\(cutAdmission, established) -> (,) <$> traverse (shape . decodeHeraldAdmissionIdCanonicalBytes) cutAdmission <*> shape (GraphProtocol.decodeTopologyCutEstablishedCanonicalBytes established)) cuts <*> traverse (shape . Terminal.decodeTerminalSourceUnionEstablishedCanonicalBytes) terminals <*> traverse (shape . Terminal.decodeTerminalStructuralOccurrenceCanonicalBytes) occurrences <*> traverse decodeView views <*> traverse decodePlacementSnapshot placements <*> shape (AlignmentHistory.decodeAlignmentHistory alignment)
  validateHistory history
  require (encodeJoinHistory history == bytes) JoinHistoryMalformed
  pure history
  where
    decodeView (RawView tag revision observations) = do
      role <- maybe (Left JoinHistoryMalformed) Right (find ((== tag) . predefinedSortRoleTag) allPredefinedSortRoles)
      SystemViewHistory role revision <$> traverse decodeEvidence observations
encodeEvidence :: Evidence.RetainedPublicationEvidence -> ByteString
encodeEvidence evidence = case Bridge.retainedPublicationEvidenceToDto evidence of
  Left problem -> error ("checked join evidence encoding invariant: " <> show problem)
  Right dto -> BL.toStrict (Binary.encode dto)
decodeEvidence :: ByteString -> Either JoinHistoryProblem Evidence.RetainedPublicationEvidence
decodeEvidence bytes = do
  dto <- case Binary.decodeOrFail (BL.fromStrict bytes) of
    Left _ -> Left JoinHistoryMalformed
    Right (rest, _, value) | BL.null rest -> Right (value :: Wire.RetainedPublicationEvidenceDto)
    _ -> Left JoinHistoryMalformed
  evidence <- shape (Bridge.retainedPublicationEvidenceFromDto dto)
  require (encodeEvidence evidence == bytes) JoinHistoryMalformed
  pure evidence
encodePlacementSnapshot :: Placement.PlacementSnapshot -> ByteString
encodePlacementSnapshot snapshot = case Bridge.placementSnapshotToDto snapshot of
  Left problem -> error ("checked join placement encoding invariant: " <> show problem)
  Right dto -> BL.toStrict (Binary.encode dto)
decodePlacementSnapshot :: ByteString -> Either JoinHistoryProblem Placement.PlacementSnapshot
decodePlacementSnapshot bytes = do
  dto <- case Binary.decodeOrFail (BL.fromStrict bytes) of
    Left _ -> Left JoinHistoryMalformed
    Right (rest, _, value) | BL.null rest -> Right (value :: Wire.PlacementSnapshotDto)
    _ -> Left JoinHistoryMalformed
  snapshot <- shape (Bridge.placementSnapshotFromDto dto)
  require (encodePlacementSnapshot snapshot == bytes) JoinHistoryMalformed
  pure snapshot
validateHistory :: JoinHistory -> Either JoinHistoryProblem ()
validateHistory (JoinHistory _ _ attempt source control cut stamped _ (JoinControlHistory anchor entries) cuts _ occurrences views placements _) = do
  require (attempt > 0) JoinHistoryMalformed
  require (map placementCoordinate placements == Set.toAscList (Set.fromList (map placementCoordinate placements))) JoinHistoryMalformed
  require (anchor <= control) JoinHistoryCanonicalControlGap
  final <- foldM contiguous anchor entries
  require (final == control) JoinHistoryCanonicalControlGap
  require (source `elem` map fst (structuralVersionVectorEntries vector) && stamped <= component source vector) JoinHistorySourceNotOldMember
  require (topologyFrontierAppliedControlPrefix (topologyCutFrontier cut) <= control) JoinHistoryMalformed
  require (map (\(SystemViewHistory role _ _) -> predefinedSortRoleTag role) views == map predefinedSortRoleTag allPredefinedSortRoles) JoinHistoryMalformed
  require (and (zipWith (<) occurrenceIds (drop 1 occurrenceIds))) JoinHistoryMalformed
  require (coversSourcePrefix 1 occurrenceIds) JoinHistorySourceMaterialMissing
  require (deriveTopologyCutId cut `elem` map (GraphProtocol.topologyCutEstablishedId . snd) cuts) JoinHistorySourceMaterialMissing
  require (all (\(SystemViewHistory _ _ evidence) -> map Evidence.retainedPublicationEvidenceCanonicalBytes evidence == Set.toAscList (Set.fromList (map Evidence.retainedPublicationEvidenceCanonicalBytes evidence))) views) JoinHistoryMalformed
  where
    occurrenceIds = map Terminal.terminalStructuralOccurrenceId occurrences
    -- Canonical ordering groups each source's strictly increasing occurrences.
    -- Consume its required prefix once; retained terminal tails above the stamp
    -- need not be contiguous and other sources have their own readiness checks.
    coversSourcePrefix expected remaining
      | expected > stamped = True
      | otherwise = case remaining of
          [] -> False
          identifier : rest
            | structuralOccurrenceSourceHeraldEpoch identifier < source -> coversSourcePrefix expected rest
            | structuralOccurrenceSourceHeraldEpoch identifier == source,
              structuralSequenceWord64 (structuralOccurrenceSourceSequence identifier) == expected ->
                coversSourcePrefix (expected + 1) rest
            | otherwise -> False
    contiguous previous entry = do
      let index = appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry)
      require (controlIndexWord64 index == controlIndexWord64 previous + 1 && index <= control) JoinHistoryCanonicalControlGap
      pure index
    vector = topologyFrontierStructuralVersionVector (topologyCutFrontier cut)
    placementCoordinate snapshot = (Placement.placementSnapshotOwner snapshot, Placement.placementSnapshotSequence snapshot)
component :: HeraldEpoch -> StructuralVersionVector -> Word64
component source vector = maybe 0 (maybe 0 structuralSequenceWord64 . structuralPrefixSequence) (structuralVersionVectorComponent source vector)
owner :: (Show problem) => Either problem value -> Either JoinHistoryProblem value
owner = either (Left . JoinHistoryOwnerProblem . show) Right
shape :: Either problem value -> Either JoinHistoryProblem value
shape = either (const (Left JoinHistoryMalformed)) Right
require :: Bool -> JoinHistoryProblem -> Either JoinHistoryProblem ()
require condition problem = unless condition (Left problem)

-- | Replay every currently ready retained occurrence/certificate to a finite
-- fixed point. A caller may project the next canonical control entry between
-- passes; an incomplete pass is never an installed-history receipt.
advanceJoinHistoryData :: JoinHistory -> HeraldState -> Either JoinHistoryProblem (HeraldState, EffectBatch)
advanceJoinHistoryData history = advanceJoinHistoriesData [history]

-- | A donor may supply a predecessor plan, certificate or observation needed
-- by an earlier donor. Reach a shared fixed point before reporting completion;
-- arrival order is not a dependency ordering.
advanceJoinHistoriesData :: [JoinHistory] -> HeraldState -> Either JoinHistoryProblem (HeraldState, EffectBatch)
advanceJoinHistoriesData histories initial = do
  require (all ((== Genesis.checkedSystemId (startupGenesis initial)) . joinHistorySystem) histories) JoinHistoryWrongSystem
  loop initial mempty
  where
    loop state effects = do
      (successor, emitted) <- foldM advance (state, mempty) histories
      if fingerprint successor == fingerprint state then pure (successor, effects <> emitted) else loop successor (effects <> emitted)
    advance (state, effects) history = do
      placement <- owner (foldM (flip PlacementState.retainHistoricalPlacementSnapshot) (startupPlacementState state) (joinHistoryPlacementSnapshots history))
      let withPlacement = replaceStartupPlacementState placement state
      withViews <- foldM installReadyView withPlacement (joinHistoryViews history)
      occurred <- foldM (replay history) withViews (joinHistoryOccurrences history)
      cutApplied <- foldM (installCut history) occurred (joinHistoryTopologyCertificates history)
      (aligned, emitted) <- owner (AlignmentReplay.advanceAlignmentHistory (joinHistoryAlignmentHistory history) cutApplied)
      pure (aligned, effects <> emitted)
    fingerprint state =
      ( Progress.structuralAppliedVector (startupStructuralProgressState state),
        Progress.structuralInstalledCutIds (startupStructuralProgressState state),
        [(Store.storeSlotDelta slot, Store.storeSlotRevision slot) | slot <- Store.storeSlots (startupStoreState state)],
        PlacementState.retainedPlacementSnapshots (startupPlacementState state),
        AlignmentReplay.alignmentHistoryFingerprint state,
        Join.importedSystemViewObservationCount (startupJoinState state)
      )
    installReadyView state (SystemViewHistory role _ evidence) = foldM (installReadyObservation role) state evidence
    installReadyObservation role state evidence
      -- A canonical receipt belongs to this owner's permanent system view,
      -- independent of the donor or current membership cut. Exact replay cannot
      -- change its Store. Definition observations still revisit induction: a
      -- later control entry can change the effective definition occurrence.
      | role /= SortDefinitionRole,
        Join.hasImportedObservation role evidence (startupJoinState state) =
          pure state
      | evidenceReady state evidence = installObservation role state evidence
      | otherwise = pure state
    replay history state occurrence
      | not (historyOccurrenceRequired history occurrence) = pure state
      | Progress.lookupAppliedStructuralOccurrence (Terminal.terminalStructuralOccurrenceId occurrence) (startupStructuralProgressState state) /= Nothing = pure state
      | otherwise = fromMaybe state <$> owner (Replay.replayJoinStructuralOccurrence occurrence state)
    installCut history state established
      | GraphProtocol.topologyCutEstablishedId established `elem` Progress.structuralInstalledCutIds progress = pure state
      | topologyFrontierAppliedControlPrefix frontier > Projection.oracleViewControlIndex oracle = pure state
      | topologyPredecessorCutId (topologyCutPredecessor cut) /= Progress.structuralLastInstalledCutId progress = pure state
      | otherwise = case topologyPredecessorView (topologyCutPredecessor cut) of
          SameGenerationPredecessorView _ | topologyFrontierMembershipGenerationId frontier == Progress.structuralProgressMembershipGenerationId progress -> do
            let prefix = topologyFrontierAppliedControlPrefix frontier
                (changed, bytes) = Reconciliation.structuralControlCheckpointAt prefix (Progress.structuralProgressReconciliation progress)
                checkpoint = case lookup (GraphProtocol.topologyCutEstablishedId established) (joinHistorySealCheckpoints history) of
                  Just admission -> Progress.topologyJoinSealCheckpoint admission prefix bytes
                  Nothing -> Progress.topologyControlCheckpointWithConsequence prefix changed bytes
            prepared <- owner (Progress.prepareTopologyCutEstablished (historyViews state) checkpoint established progress)
            pure (replaceStartupStructuralProgressState (Progress.commitTopologyCutEstablished prepared) state)
          MembershipSuccessorPredecessorView _ predecessor successor _ _ _ ->
            case find ((== successor) . Terminal.terminalSourceUnionSuccessorGenerationId . Terminal.terminalSourceUnionEstablishedUnion) (joinHistoryTerminalCertificates history) of
              Nothing -> pure state
              Just establishedUnion -> do
                lineage <- owner (heraldMembershipLineage predecessor successor (Projection.oracleViewHeraldMembershipHistoryChecked oracle))
                base <- owner (Terminal.establishedSuccessorStructuralBase lineage establishedUnion cut)
                prepared <- owner (Progress.prepareMembershipSuccessorBaseInstallation (heraldMembershipLineageTarget lineage) base cut progress)
                let applied = Progress.commitMembershipSuccessorBaseInstallation prepared
                pure (replaceStartupStructuralProgressState applied state)
          _ -> pure state
      where
        progress = startupStructuralProgressState state
        oracle = Projection.oracleView (startupOracleProjectionState state)
        cut = GraphProtocol.topologyCutEstablishedCut established
        frontier = topologyCutFrontier cut

-- | Adopt the immutable authoritative frontier only immediately before the
-- checked Activate entry. Preparing/Sealed attempts may be invalidated and
-- recopied; their catalogues are passive until this one-way admission boundary.
adoptJoinAlignmentFrontier :: JoinSourceHistory -> HeraldState -> Either JoinHistoryProblem HeraldState
adoptJoinAlignmentFrontier source state = do
  let history = sourceJoinHistory source
  require (Progress.structuralProgressIsJoiningObserver (startupStructuralProgressState state)) JoinHistoryWrongAdmission
  record <- maybe (Left JoinHistoryWrongAdmission) Right (Projection.oracleViewHeraldAdmission (joinHistoryAdmission history) (Projection.oracleView (startupOracleProjectionState state)))
  seal <- maybe (Left JoinHistoryWrongAdmission) Right (admissionRecordSeal record)
  require (admissionRecordAttempt record == joinHistoryAttempt history) JoinHistoryWrongAdmission
  require (joinHistorySource history == minimum (NE.toList (heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor record)))) JoinHistorySourceNotOldMember
  require (lookup (joinHistorySource history) (joinSealMemberCuts seal) == Just (joinSourceMemberCut source)) JoinHistoryWrongAdmission
  require (joinHistoryInstalled history state) JoinHistorySourceMaterialMissing
  require (alignmentFrontierAdmitted (NE.toList (heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor record))) (joinHistoryAlignmentHistory history)) JoinHistorySourceMaterialMissing
  covered <- owner (AlignmentReplay.adoptAlignmentHistoryCoverage (joinHistoryAlignmentHistory history) state)
  alignment <- owner (Alignment.adoptHistoricalAlignmentPlanFrontier (Genesis.checkedLocalHeraldEpoch (startupGenesis covered)) (AlignmentHistory.alignmentHistoryFrontier (joinHistoryAlignmentHistory history)) (startupAlignmentState covered))
  pure (replaceStartupAlignmentState alignment covered)

historyViews :: HeraldState -> Reconciliation.ReconciliationViews
historyViews state =
  Reconciliation.withReconciliationDiagnosticChecks (startupDiagnosticChecks state)
    $ Reconciliation.reconciliationViewsWithEndedProcesses
      ( Map.fromList
          [ (process, (residence, Projection.projectedEndedProcessControlIndex ended))
          | (process, ended) <- Projection.projectedEndedProcesses projection,
            Just residence <- [Projection.oracleViewProcessResidence process oracle]
          ]
      )
    $ Reconciliation.reconciliationViews
      (Genesis.checkedLocalHeraldEpoch (startupGenesis state))
      (Map.fromList [(Registry.registryEntrySortId entry, sortOccurrence (Registry.registryEntrySortId entry) (Registry.registryEntryOccurrenceId entry)) | entry <- Registry.registryEntries (startupSortRegistryState state)])
      (Set.fromList (Graph.graphNonStructuralBaselineVertices (startupGraphState state)))
      (Map.fromList [(process, residence) | process <- Projection.projectedProcessEpochs projection, Projection.oracleViewProcessIsLive process oracle, Just residence <- [Projection.oracleViewProcessResidence process oracle]])
      (Set.fromList (Controlled.controlledNormalPossessionEntries (startupControlledState state)))
  where
    projection = startupOracleProjectionState state
    oracle = Projection.oracleView projection

-- Retained terminal tails above K remain bytes in the bundle, never invented
-- applied prefixes. Only K-covered occurrences are prerequisites for readiness.
historyOccurrenceRequired :: JoinHistory -> Terminal.TerminalStructuralOccurrence -> Bool
historyOccurrenceRequired history occurrence =
  any (\vector -> sequenceNumber <= component source vector) vectors
  where
    identifier = Terminal.terminalStructuralOccurrenceId occurrence
    source = structuralOccurrenceSourceHeraldEpoch identifier
    sequenceNumber = structuralSequenceWord64 (structuralOccurrenceSourceSequence identifier)
    vectors =
      topologyFrontierStructuralVersionVector (topologyCutFrontier (joinHistoryTopologyCut history))
        : [Terminal.terminalSourceUnionTerminalPredecessorVector (Terminal.terminalSourceUnionEstablishedUnion established) | established <- joinHistoryTerminalCertificates history]

evidenceReady :: HeraldState -> Evidence.RetainedPublicationEvidence -> Bool
evidenceReady state evidence = case Evidence.retainedPublicationEvidenceOrigin evidence of
  Evidence.PrimordialRetainedObservation -> True
  Evidence.RoutedRetainedObservation _ topology control stamp ->
    control <= Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState state))
      && (topology == Progress.structuralGenesisCutId progress || topology `elem` Progress.structuralInstalledCutIds progress)
      && maybe True (\s -> isJust (Progress.lookupAppliedStructuralOccurrence (Peer.structuralOccurrenceStampOccurrence s) progress)) stamp
  where
    progress = startupStructuralProgressState state
