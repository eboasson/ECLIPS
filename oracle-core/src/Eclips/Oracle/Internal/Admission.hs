{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure ownership of the preemptible admission barrier and its exact evidence.
-- The Oracle retains only manifests and normalized cut reports. Graph/store
-- payload admission and interpretation belong to each Herald's own join owner.
module Eclips.Oracle.Internal.Admission
  ( HeraldAdmissionManifest,
    heraldAdmissionManifest,
    admissionManifestSystem,
    admissionManifestHeraldId,
    admissionManifestHeraldEpoch,
    HeraldJoinDigest,
    mkHeraldJoinDigest,
    heraldJoinDigestBytes,
    deriveHeraldJoinDigest,
    HeraldJoinMemberCut,
    heraldJoinMemberCut,
    joinMemberStampedPrefix,
    joinMemberInputPrefix,
    joinMemberStoreDigest,
    HeraldJoinSeal,
    heraldJoinSeal,
    joinSealAdmissionId,
    joinSealAttempt,
    joinSealTopologyCut,
    joinSealControlPrefix,
    joinSealMemberCuts,
    joinSealContributionDigest,
    joinSealRecipeDigest,
    joinSealDigest,
    HeraldJoinReadyReport,
    heraldJoinReadyReport,
    joinReadyAdmissionId,
    joinReadyAttempt,
    joinReadyReporter,
    joinReadySealDigest,
    joinReadyControlPrefix,
    joinReadyRecipeDigest,
    HeraldAdmissionPhase (..),
    HeraldAdmissionRecord,
    admissionRecordId,
    admissionRecordManifest,
    admissionRecordPredecessor,
    admissionRecordBeginIndex,
    admissionRecordChangedIndex,
    admissionRecordSemanticToken,
    admissionRecordAttempt,
    admissionRecordPhase,
    admissionRecordSeal,
    admissionRecordSealAcceptors,
    admissionRecordBaseReporters,
    admissionRecordBaseReports,
    admissionRecordNewcomerReport,
    HeraldAdmissionProblem (..),
    HeraldAdmissionCommand (..),
    AdmissionState,
    initialAdmissionState,
    admissionCatalogue,
    admissionRecords,
    pendingAdmission,
    lookupAdmission,
    applyAdmissionCommand,
    invalidateAdmission,
    cancelPendingAdmission,
    admissionStateCanonicalBytes,
    encodeAdmissionCheckpoint,
    decodeAdmissionCheckpoint,
    encodeAdmissionCommand,
    decodeAdmissionCommand,
    encodeAdmissionRecord,
    decodeAdmissionRecord,
    encodeAdmissionProblem,
    decodeAdmissionProblem,
  ) where

import Control.Monad (unless)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Serialize qualified as S
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
import Eclips.Domain.Membership
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Domain.Structural
import Eclips.Domain.Topology
import Eclips.Oracle.Genesis (CheckedOracleGenesis, checkedOracleActiveHeralds, checkedOracleSystemId)
import GHC.Generics (Generic)

-- | Immutable identity manifest; locators remain discovery hints.
data HeraldAdmissionManifest = HeraldAdmissionManifest SystemId HeraldId HeraldEpoch
  deriving stock (Eq, Ord, Show)

heraldAdmissionManifest :: SystemId -> HeraldId -> HeraldEpoch -> HeraldAdmissionManifest
heraldAdmissionManifest = HeraldAdmissionManifest
admissionManifestSystem :: HeraldAdmissionManifest -> SystemId
admissionManifestSystem (HeraldAdmissionManifest x _ _) = x
admissionManifestHeraldId :: HeraldAdmissionManifest -> HeraldId
admissionManifestHeraldId (HeraldAdmissionManifest _ x _) = x
admissionManifestHeraldEpoch :: HeraldAdmissionManifest -> HeraldEpoch
admissionManifestHeraldEpoch (HeraldAdmissionManifest _ _ x) = x

newtype HeraldJoinDigest = HeraldJoinDigest ByteString deriving stock (Eq, Ord, Show)
mkHeraldJoinDigest :: ByteString -> Either HeraldAdmissionProblem HeraldJoinDigest
mkHeraldJoinDigest bytes
  | BS.length bytes == 32 = Right (HeraldJoinDigest bytes)
  | otherwise = Left MalformedAdmissionClaim
heraldJoinDigestBytes :: HeraldJoinDigest -> ByteString
heraldJoinDigestBytes (HeraldJoinDigest bytes) = bytes
deriveHeraldJoinDigest :: ByteString -> HeraldJoinDigest
deriveHeraldJoinDigest = HeraldJoinDigest . SHA256.hash . ("ECLIPS-HERALD-JOIN-DIGEST" <>)

data HeraldJoinMemberCut = HeraldJoinMemberCut Word64 Word64 HeraldJoinDigest
  deriving stock (Eq, Show)
heraldJoinMemberCut :: Word64 -> Word64 -> HeraldJoinDigest -> HeraldJoinMemberCut
heraldJoinMemberCut = HeraldJoinMemberCut
joinMemberStampedPrefix :: HeraldJoinMemberCut -> Word64
joinMemberStampedPrefix (HeraldJoinMemberCut x _ _) = x
joinMemberInputPrefix :: HeraldJoinMemberCut -> Word64
joinMemberInputPrefix (HeraldJoinMemberCut _ x _) = x
joinMemberStoreDigest :: HeraldJoinMemberCut -> HeraldJoinDigest
joinMemberStoreDigest (HeraldJoinMemberCut _ _ x) = x

data HeraldJoinSeal = HeraldJoinSeal HeraldAdmissionId Word64 TopologyCut ControlIndex (Map HeraldEpoch HeraldJoinMemberCut) HeraldJoinDigest HeraldJoinDigest
  deriving stock (Eq, Show)
heraldJoinSeal :: HeraldAdmissionId -> Word64 -> TopologyCut -> ControlIndex -> [(HeraldEpoch, HeraldJoinMemberCut)] -> HeraldJoinDigest -> HeraldJoinDigest -> Either HeraldAdmissionProblem HeraldJoinSeal
heraldJoinSeal ident attempt cut prefix cuts contribution recipe = do
  require (attempt > 0) MalformedAdmissionClaim
  require (Map.size normalized == length cuts) JoinCutReporterSetMismatch
  require (Map.keys normalized == fmap fst components) JoinCutReporterSetMismatch
  require (all covered components) JoinStampedPrefixNotCovered
  require (topologyFrontierAppliedControlPrefix frontier <= prefix) JoinControlPrefixMismatch
  pure (HeraldJoinSeal ident attempt cut prefix normalized contribution recipe)
  where
    normalized = Map.fromList cuts
    frontier = topologyCutFrontier cut
    components = structuralVersionVectorEntries (topologyFrontierStructuralVersionVector frontier)
    covered (epoch, p) = maybe False ((<= prefixNumber p) . joinMemberStampedPrefix) (Map.lookup epoch normalized)
    prefixNumber = maybe 0 structuralSequenceWord64 . structuralPrefixSequence
joinSealAdmissionId :: HeraldJoinSeal -> HeraldAdmissionId
joinSealAdmissionId (HeraldJoinSeal x _ _ _ _ _ _) = x
joinSealAttempt :: HeraldJoinSeal -> Word64
joinSealAttempt (HeraldJoinSeal _ x _ _ _ _ _) = x
joinSealTopologyCut :: HeraldJoinSeal -> TopologyCut
joinSealTopologyCut (HeraldJoinSeal _ _ x _ _ _ _) = x
joinSealControlPrefix :: HeraldJoinSeal -> ControlIndex
joinSealControlPrefix (HeraldJoinSeal _ _ _ x _ _ _) = x
joinSealMemberCuts :: HeraldJoinSeal -> [(HeraldEpoch, HeraldJoinMemberCut)]
joinSealMemberCuts (HeraldJoinSeal _ _ _ _ x _ _) = Map.toAscList x
joinSealContributionDigest :: HeraldJoinSeal -> HeraldJoinDigest
joinSealContributionDigest (HeraldJoinSeal _ _ _ _ _ x _) = x
joinSealRecipeDigest :: HeraldJoinSeal -> HeraldJoinDigest
joinSealRecipeDigest (HeraldJoinSeal _ _ _ _ _ _ x) = x
joinSealDigest :: HeraldJoinSeal -> HeraldJoinDigest
joinSealDigest = deriveHeraldJoinDigest . S.encode . rawSeal

data HeraldJoinReadyReport = HeraldJoinReadyReport HeraldAdmissionId Word64 HeraldEpoch HeraldJoinDigest ControlIndex HeraldJoinDigest
  deriving stock (Eq, Show)
heraldJoinReadyReport :: HeraldAdmissionId -> Word64 -> HeraldEpoch -> HeraldJoinDigest -> ControlIndex -> HeraldJoinDigest -> HeraldJoinReadyReport
heraldJoinReadyReport = HeraldJoinReadyReport
joinReadyAdmissionId :: HeraldJoinReadyReport -> HeraldAdmissionId
joinReadyAdmissionId (HeraldJoinReadyReport x _ _ _ _ _) = x
joinReadyAttempt :: HeraldJoinReadyReport -> Word64
joinReadyAttempt (HeraldJoinReadyReport _ x _ _ _ _) = x
joinReadyReporter :: HeraldJoinReadyReport -> HeraldEpoch
joinReadyReporter (HeraldJoinReadyReport _ _ x _ _ _) = x
joinReadySealDigest :: HeraldJoinReadyReport -> HeraldJoinDigest
joinReadySealDigest (HeraldJoinReadyReport _ _ _ x _ _) = x
joinReadyControlPrefix :: HeraldJoinReadyReport -> ControlIndex
joinReadyControlPrefix (HeraldJoinReadyReport _ _ _ _ x _) = x
joinReadyRecipeDigest :: HeraldJoinReadyReport -> HeraldJoinDigest
joinReadyRecipeDigest (HeraldJoinReadyReport _ _ _ _ _ x) = x

data HeraldAdmissionPhase = AdmissionPreparing | AdmissionSealed | AdmissionReady | AdmissionActivated ControlIndex HeraldMembershipGeneration | AdmissionCancelled ControlIndex
  deriving stock (Eq, Show)

-- Private fields never provide a public record-update path around admission.
data HeraldAdmissionRecord = HeraldAdmissionRecord
  { recordId :: HeraldAdmissionId,
    recordManifest :: HeraldAdmissionManifest,
    recordPredecessor :: HeraldMembershipGeneration,
    recordBeginIndex :: ControlIndex,
    recordChangedIndex :: ControlIndex,
    recordAttempt :: Word64,
    recordSemanticToken :: ControlIndex,
    recordPhase :: HeraldAdmissionPhase,
    recordSeal :: Maybe HeraldJoinSeal,
    recordAcceptors :: Set HeraldEpoch,
    recordBaseReports :: Map HeraldEpoch HeraldJoinReadyReport,
    recordNewcomerReport :: Maybe HeraldJoinReadyReport
  }
  deriving stock (Eq, Show)
admissionRecordId :: HeraldAdmissionRecord -> HeraldAdmissionId
admissionRecordId = recordId
admissionRecordManifest :: HeraldAdmissionRecord -> HeraldAdmissionManifest
admissionRecordManifest = recordManifest
admissionRecordPredecessor :: HeraldAdmissionRecord -> HeraldMembershipGeneration
admissionRecordPredecessor = recordPredecessor
admissionRecordBeginIndex :: HeraldAdmissionRecord -> ControlIndex
admissionRecordBeginIndex = recordBeginIndex
admissionRecordChangedIndex :: HeraldAdmissionRecord -> ControlIndex
admissionRecordChangedIndex = recordChangedIndex
admissionRecordSemanticToken :: HeraldAdmissionRecord -> ControlIndex
admissionRecordSemanticToken = recordSemanticToken
admissionRecordAttempt :: HeraldAdmissionRecord -> Word64
admissionRecordAttempt = recordAttempt
admissionRecordPhase :: HeraldAdmissionRecord -> HeraldAdmissionPhase
admissionRecordPhase = recordPhase
admissionRecordSeal :: HeraldAdmissionRecord -> Maybe HeraldJoinSeal
admissionRecordSeal = recordSeal
admissionRecordSealAcceptors :: HeraldAdmissionRecord -> [HeraldEpoch]
admissionRecordSealAcceptors = Set.toAscList . recordAcceptors
admissionRecordBaseReporters :: HeraldAdmissionRecord -> [HeraldEpoch]
admissionRecordBaseReporters = Map.keys . recordBaseReports
admissionRecordBaseReports :: HeraldAdmissionRecord -> [HeraldJoinReadyReport]
admissionRecordBaseReports = Map.elems . recordBaseReports
admissionRecordNewcomerReport :: HeraldAdmissionRecord -> Maybe HeraldJoinReadyReport
admissionRecordNewcomerReport = recordNewcomerReport

data HeraldAdmissionProblem
  = MalformedAdmissionClaim
  | AdmissionWrongSystem
  | AdmissionSlotOccupied
  | AdmissionEpochAlreadyKnown
  | AdmissionHeraldIdOccupied
  | AdmissionBaseNotCurrent
  | UnknownHeraldAdmission
  | HeraldAdmissionTerminal
  | JoinSealWrongCoordinator
  | JoinSealAttemptMismatch
  | JoinSealConflict
  | JoinCutReporterSetMismatch
  | JoinStampedPrefixNotCovered
  | JoinControlPrefixMismatch
  | JoinSealNotCollected
  | JoinReporterMismatch
  | JoinReadinessMismatch
  | JoinReadinessIncomplete
  | JoinOpenLabelWorkflow
  | AdmissionApplicationGateClosed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data HeraldAdmissionCommand
  = BeginHeraldAdmission HeraldAdmissionManifest TopologyCut
  | SealHeraldAdmission HeraldJoinSeal
  | AcceptHeraldJoinSeal HeraldAdmissionId Word64 HeraldJoinDigest
  | HeraldJoinBaseReady HeraldJoinReadyReport
  | HeraldJoinReady HeraldJoinReadyReport
  | ActivateHerald HeraldAdmissionId
  | CancelHeraldAdmission HeraldAdmissionId
  deriving stock (Eq, Show)

data AdmissionState = AdmissionState (Map HeraldEpoch HeraldAdmissionManifest) (Map HeraldAdmissionId HeraldAdmissionRecord) (Maybe HeraldAdmissionId)
  deriving stock (Eq, Show)
initialAdmissionState :: SystemId -> [HeraldMember] -> AdmissionState
initialAdmissionState system members = AdmissionState (Map.fromList [(heraldMemberEpoch m, HeraldAdmissionManifest system (heraldMemberId m) (heraldMemberEpoch m)) | m <- members]) Map.empty Nothing
admissionCatalogue :: AdmissionState -> [HeraldAdmissionManifest]
admissionCatalogue (AdmissionState catalogue _ _) = Map.elems catalogue
admissionRecords :: AdmissionState -> [HeraldAdmissionRecord]
admissionRecords (AdmissionState _ records _) = Map.elems records
pendingAdmission :: AdmissionState -> Maybe HeraldAdmissionRecord
pendingAdmission (AdmissionState _ records ident) = ident >>= (`Map.lookup` records)
lookupAdmission :: HeraldAdmissionId -> AdmissionState -> Maybe HeraldAdmissionRecord
lookupAdmission ident (AdmissionState _ records _) = Map.lookup ident records

applyAdmissionCommand :: SystemId -> HeraldMembershipGeneration -> Bool -> ControlIndex -> HeraldEpoch -> HeraldAdmissionCommand -> AdmissionState -> Either HeraldAdmissionProblem (AdmissionState, HeraldAdmissionRecord, Maybe HeraldMembershipGeneration)
applyAdmissionCommand system current labelOpen index home command (AdmissionState catalogue records pending) = case command of
  BeginHeraldAdmission manifest anchor -> do
    require (pending == Nothing) AdmissionSlotOccupied
    require (admissionManifestSystem manifest == system) AdmissionWrongSystem
    require (Map.notMember applicant catalogue) AdmissionEpochAlreadyKnown
    require (not (any occupied (Map.elems catalogue))) AdmissionHeraldIdOccupied
    require (cutCurrent anchor && topologyFrontierAppliedControlPrefix (topologyCutFrontier anchor) < index) AdmissionBaseNotCurrent
    ident <- either (const (Left MalformedAdmissionClaim)) Right (deriveHeraldAdmissionId index)
    let record = HeraldAdmissionRecord ident manifest current index index 1 index AdmissionPreparing Nothing Set.empty Map.empty Nothing
    pure (AdmissionState (Map.insert applicant manifest catalogue) (Map.insert ident record records) (Just ident), record, Nothing)
    where
      applicant = admissionManifestHeraldEpoch manifest
      occupied m = admissionManifestHeraldId m == admissionManifestHeraldId manifest && Set.member (admissionManifestHeraldEpoch m) active
  SealHeraldAdmission seal -> update (joinSealAdmissionId seal) $ \r -> do
    require (home == Set.findMin (oldMembers r)) JoinSealWrongCoordinator
    require (not labelOpen) JoinOpenLabelWorkflow
    require (joinSealAttempt seal == recordAttempt r) JoinSealAttemptMismatch
    require (cutCurrent (joinSealTopologyCut seal)) AdmissionBaseNotCurrent
    require (fmap fst (joinSealMemberCuts seal) == Set.toAscList (oldMembers r)) JoinCutReporterSetMismatch
    require (joinSealControlPrefix seal >= recordSemanticToken r && joinSealControlPrefix seal < index) JoinControlPrefixMismatch
    require (maybe True (== seal) (recordSeal r)) JoinSealConflict
    checkSealRecipe (recordId r) (admissionManifestHeraldEpoch (recordManifest r)) current seal
    pure (if recordSeal r == Just seal then r else r {recordSeal = Just seal, recordPhase = AdmissionSealed}, Nothing)
  AcceptHeraldJoinSeal ident attempt digest -> update ident $ \r -> do
    seal <- getSeal r
    require (attempt == recordAttempt r && digest == joinSealDigest seal) JoinSealAttemptMismatch
    require (Set.member home (oldMembers r)) JoinReporterMismatch
    pure (r {recordAcceptors = Set.insert home (recordAcceptors r)}, Nothing)
  HeraldJoinBaseReady report -> ready False report
  HeraldJoinReady report -> ready True report
  ActivateHerald ident -> update ident $ \r -> do
    require (not labelOpen) JoinOpenLabelWorkflow
    require (recordPhase r == AdmissionReady && complete r) JoinReadinessIncomplete
    generation <- either (const (Left AdmissionBaseNotCurrent)) Right (admitHeraldMembershipGeneration index ident (admissionManifestHeraldEpoch (recordManifest r)) current)
    pure (r {recordPhase = AdmissionActivated index generation}, Just generation)
  CancelHeraldAdmission ident -> update ident $ \r -> pure (r {recordPhase = AdmissionCancelled index}, Nothing)
  where
    active = Set.fromList (NE.toList (heraldMembershipGenerationActiveHeraldEpochs current))
    cutCurrent = (== heraldMembershipGenerationId current) . topologyFrontierMembershipGenerationId . topologyCutFrontier
    update ident f = do
      r <- maybe (Left UnknownHeraldAdmission) Right (Map.lookup ident records)
      require (pending == Just ident) HeraldAdmissionTerminal
      require (recordPredecessor r == current) AdmissionBaseNotCurrent
      (changed, generation) <- f r
      let r' = changed {recordChangedIndex = index}
          pending' = if terminal (recordPhase r') then Nothing else pending
      pure (AdmissionState catalogue (Map.insert ident r' records) pending', r', generation)
    ready newcomer report = update (joinReadyAdmissionId report) $ \r -> do
      seal <- getSeal r
      require (recordAcceptors r == oldMembers r) JoinSealNotCollected
      require (joinReadyAttempt report == recordAttempt r && joinReadySealDigest report == joinSealDigest seal && joinReadyRecipeDigest report == joinSealRecipeDigest seal) JoinReadinessMismatch
      require (joinReadyControlPrefix report >= joinSealControlPrefix seal && joinReadyControlPrefix report < index) JoinControlPrefixMismatch
      if newcomer
        then require (joinReadyReporter report == admissionManifestHeraldEpoch (recordManifest r)) JoinReporterMismatch
        else require (joinReadyReporter report == home && Set.member home (oldMembers r)) JoinReporterMismatch
      let prior = if newcomer then recordNewcomerReport r else Map.lookup home (recordBaseReports r)
      require (maybe True (== report) prior) JoinReadinessMismatch
      let changed = if newcomer then r {recordNewcomerReport = Just report} else r {recordBaseReports = Map.insert home report (recordBaseReports r)}
      pure (changed {recordPhase = if complete changed then AdmissionReady else AdmissionSealed}, Nothing)

oldMembers :: HeraldAdmissionRecord -> Set HeraldEpoch
oldMembers = Set.fromList . NE.toList . heraldMembershipGenerationActiveHeraldEpochs . recordPredecessor
complete :: HeraldAdmissionRecord -> Bool
complete r = recordAcceptors r == oldMembers r && Map.keysSet (recordBaseReports r) == oldMembers r && maybe False (const True) (recordNewcomerReport r)
terminal :: HeraldAdmissionPhase -> Bool
terminal AdmissionActivated {} = True
terminal AdmissionCancelled {} = True
terminal _ = False
getSeal :: HeraldAdmissionRecord -> Either HeraldAdmissionProblem HeraldJoinSeal
getSeal = maybe (Left JoinSealNotCollected) Right . recordSeal

-- The record codec and the live command bind the same immutable contribution
-- to the predecessor cut and applicant; a matching set of report bytes alone
-- cannot establish that binding.
checkSealRecipe :: HeraldAdmissionId -> HeraldEpoch -> HeraldMembershipGeneration -> HeraldJoinSeal -> Either HeraldAdmissionProblem ()
checkSealRecipe ident applicant predecessor seal = do
  contribution <- claim (mkTopologyOccurrenceDigest (heraldJoinDigestBytes (joinSealContributionDigest seal)))
  recipe <- claim (heraldJoinBaseRecipe ident applicant predecessor (joinSealTopologyCut seal) contribution)
  require (heraldJoinDigestBytes (joinSealRecipeDigest seal) == heraldJoinBaseRecipeDigestBytes recipe) JoinReadinessMismatch

require :: Bool -> HeraldAdmissionProblem -> Either HeraldAdmissionProblem ()
require True _ = Right ()
require False problem = Left problem

invalidateAdmission :: ControlIndex -> AdmissionState -> (AdmissionState, [HeraldAdmissionRecord])
invalidateAdmission index state@(AdmissionState catalogue records pending) = case pendingAdmission state of
  Nothing -> (state, [])
  Just r ->
    let r' = r {recordChangedIndex = index, recordSemanticToken = index, recordAttempt = recordAttempt r + 1, recordPhase = AdmissionPreparing, recordSeal = Nothing, recordAcceptors = Set.empty, recordBaseReports = Map.empty, recordNewcomerReport = Nothing}
     in (AdmissionState catalogue (Map.insert (recordId r) r' records) pending, [r'])
cancelPendingAdmission :: ControlIndex -> AdmissionState -> (AdmissionState, [HeraldAdmissionRecord])
cancelPendingAdmission index state@(AdmissionState catalogue records _) = case pendingAdmission state of
  Nothing -> (state, [])
  Just r ->
    let r' = r {recordChangedIndex = index, recordPhase = AdmissionCancelled index}
     in (AdmissionState catalogue (Map.insert (recordId r) r' records) Nothing, [r'])

-- Canonical claims. Every decoder checks nested domain claims and re-encoding;
-- decoded snapshots remain projection claims until contiguous replay admits them.
data RawManifest = RawManifest ByteString ByteString ByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
data RawCut = RawCut ByteString Word64 Word64 ByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
data RawSeal = RawSeal ByteString Word64 ByteString Word64 [RawCut] ByteString ByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
data RawReady = RawReady ByteString Word64 ByteString ByteString Word64 ByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
data RawCommand = RawBegin RawManifest ByteString | RawSealCommand RawSeal | RawAccept ByteString Word64 ByteString | RawOldReady RawReady | RawNewReady RawReady | RawActivate ByteString | RawCancel ByteString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
data RawPhase = RawPreparing | RawSealed | RawReadyPhase | RawActivated Word64 ByteString | RawCancelled Word64
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
data RawRecord = RawRecord ByteString RawManifest ByteString Word64 Word64 Word64 Word64 RawPhase (Maybe RawSeal) [ByteString] [RawReady] (Maybe RawReady)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialize)
rawManifest :: HeraldAdmissionManifest -> RawManifest
rawManifest (HeraldAdmissionManifest s h e) = RawManifest (systemIdBytes s) (heraldIdBytes h) (heraldEpochBytes e)
admitManifest :: RawManifest -> Either HeraldAdmissionProblem HeraldAdmissionManifest
admitManifest (RawManifest s h e) = HeraldAdmissionManifest <$> claim (mkSystemId s) <*> claim (mkHeraldId h) <*> claim (mkHeraldEpoch e)
rawSeal :: HeraldJoinSeal -> RawSeal
rawSeal (HeraldJoinSeal ident attempt cut prefix cuts contribution recipe) = RawSeal (heraldAdmissionIdCanonicalBytes ident) attempt (topologyCutCanonicalBytes cut) (controlIndexWord64 prefix) [RawCut (heraldEpochBytes e) stamped input (heraldJoinDigestBytes digest) | (e, HeraldJoinMemberCut stamped input digest) <- Map.toAscList cuts] (heraldJoinDigestBytes contribution) (heraldJoinDigestBytes recipe)
admitSeal :: RawSeal -> Either HeraldAdmissionProblem HeraldJoinSeal
admitSeal (RawSeal ident attempt cut prefix cuts contribution recipe) = do
  i <- claim (decodeHeraldAdmissionIdCanonicalBytes ident)
  k <- claim (decodeTopologyCutCanonicalBytes cut)
  c <- traverse (\(RawCut e stamped input digest) -> (,) <$> claim (mkHeraldEpoch e) <*> (HeraldJoinMemberCut stamped input <$> mkHeraldJoinDigest digest)) cuts
  d <- mkHeraldJoinDigest contribution
  b <- mkHeraldJoinDigest recipe
  heraldJoinSeal i attempt k (controlIndex prefix) c d b
rawReady :: HeraldJoinReadyReport -> RawReady
rawReady (HeraldJoinReadyReport i a h s p b) = RawReady (heraldAdmissionIdCanonicalBytes i) a (heraldEpochBytes h) (heraldJoinDigestBytes s) (controlIndexWord64 p) (heraldJoinDigestBytes b)
admitReady :: RawReady -> Either HeraldAdmissionProblem HeraldJoinReadyReport
admitReady (RawReady i a h s p b) = do
  require (a > 0) MalformedAdmissionClaim
  HeraldJoinReadyReport <$> claim (decodeHeraldAdmissionIdCanonicalBytes i) <*> pure a <*> claim (mkHeraldEpoch h) <*> mkHeraldJoinDigest s <*> pure (controlIndex p) <*> mkHeraldJoinDigest b
rawCommand :: HeraldAdmissionCommand -> RawCommand
rawCommand command = case command of
  BeginHeraldAdmission m k -> RawBegin (rawManifest m) (topologyCutCanonicalBytes k)
  SealHeraldAdmission s -> RawSealCommand (rawSeal s)
  AcceptHeraldJoinSeal i a d -> RawAccept (heraldAdmissionIdCanonicalBytes i) a (heraldJoinDigestBytes d)
  HeraldJoinBaseReady r -> RawOldReady (rawReady r)
  HeraldJoinReady r -> RawNewReady (rawReady r)
  ActivateHerald i -> RawActivate (heraldAdmissionIdCanonicalBytes i)
  CancelHeraldAdmission i -> RawCancel (heraldAdmissionIdCanonicalBytes i)
admitCommand :: RawCommand -> Either HeraldAdmissionProblem HeraldAdmissionCommand
admitCommand command = case command of
  RawBegin m k -> BeginHeraldAdmission <$> admitManifest m <*> claim (decodeTopologyCutCanonicalBytes k)
  RawSealCommand s -> SealHeraldAdmission <$> admitSeal s
  RawAccept i a d -> do
    require (a > 0) MalformedAdmissionClaim
    AcceptHeraldJoinSeal <$> claim (decodeHeraldAdmissionIdCanonicalBytes i) <*> pure a <*> mkHeraldJoinDigest d
  RawOldReady r -> HeraldJoinBaseReady <$> admitReady r
  RawNewReady r -> HeraldJoinReady <$> admitReady r
  RawActivate i -> ActivateHerald <$> claim (decodeHeraldAdmissionIdCanonicalBytes i)
  RawCancel i -> CancelHeraldAdmission <$> claim (decodeHeraldAdmissionIdCanonicalBytes i)
rawPhase :: HeraldAdmissionPhase -> RawPhase
rawPhase phase = case phase of
  AdmissionPreparing -> RawPreparing
  AdmissionSealed -> RawSealed
  AdmissionReady -> RawReadyPhase
  AdmissionActivated i g -> RawActivated (controlIndexWord64 i) (heraldMembershipGenerationCanonicalBytes g)
  AdmissionCancelled i -> RawCancelled (controlIndexWord64 i)
admitPhase :: RawPhase -> Either HeraldAdmissionProblem HeraldAdmissionPhase
admitPhase phase = case phase of
  RawPreparing -> pure AdmissionPreparing
  RawSealed -> pure AdmissionSealed
  RawReadyPhase -> pure AdmissionReady
  RawActivated i g -> AdmissionActivated (controlIndex i) <$> claim (decodeHeraldMembershipGenerationCanonicalBytes g)
  RawCancelled i -> pure (AdmissionCancelled (controlIndex i))
rawRecord :: HeraldAdmissionRecord -> RawRecord
rawRecord r = RawRecord (heraldAdmissionIdCanonicalBytes (recordId r)) (rawManifest (recordManifest r)) (heraldMembershipGenerationCanonicalBytes (recordPredecessor r)) (controlIndexWord64 (recordBeginIndex r)) (controlIndexWord64 (recordChangedIndex r)) (recordAttempt r) (controlIndexWord64 (recordSemanticToken r)) (rawPhase (recordPhase r)) (rawSeal <$> recordSeal r) (heraldEpochBytes <$> Set.toAscList (recordAcceptors r)) (rawReady <$> Map.elems (recordBaseReports r)) (rawReady <$> recordNewcomerReport r)
admitRecord :: RawRecord -> Either HeraldAdmissionProblem HeraldAdmissionRecord
admitRecord (RawRecord i m g begin changed attempt token phase seal acceptors reports newcomer) = do
  ident <- claim (decodeHeraldAdmissionIdCanonicalBytes i)
  manifest <- admitManifest m
  predecessor <- claim (decodeHeraldMembershipGenerationCanonicalBytes g)
  p <- admitPhase phase
  k <- traverse admitSeal seal
  accepted <- traverse (claim . mkHeraldEpoch) acceptors
  old <- traverse admitReady reports
  new <- traverse admitReady newcomer
  let record = HeraldAdmissionRecord ident manifest predecessor (controlIndex begin) (controlIndex changed) attempt (controlIndex token) p k (Set.fromList accepted) (Map.fromList [(joinReadyReporter r, r) | r <- old]) new
  require (begin > 0 && changed >= begin && token >= begin && token <= changed && attempt > 0) MalformedAdmissionClaim
  expected <- claim (deriveHeraldAdmissionId (controlIndex begin))
  require (ident == expected && Set.fromList accepted `Set.isSubsetOf` oldMembers record && Map.keysSet (recordBaseReports record) `Set.isSubsetOf` oldMembers record) MalformedAdmissionClaim
  require (all ((== ident) . joinReadyAdmissionId) old && maybe True ((== ident) . joinReadyAdmissionId) new) MalformedAdmissionClaim
  require (maybe True (\s -> joinSealAdmissionId s == ident && joinSealAttempt s == attempt && fmap fst (joinSealMemberCuts s) == Set.toAscList (oldMembers record)) k) MalformedAdmissionClaim
  require (case p of AdmissionPreparing -> k == Nothing && null accepted && null old && new == Nothing; AdmissionSealed -> k /= Nothing; AdmissionReady -> k /= Nothing && complete record; AdmissionActivated ix successor -> ix == controlIndex changed && complete record && either (const False) (== successor) (admitHeraldMembershipGeneration ix ident (admissionManifestHeraldEpoch manifest) predecessor); AdmissionCancelled ix -> ix == controlIndex changed) MalformedAdmissionClaim
  require (maybe True (\s -> topologyFrontierMembershipGenerationId (topologyCutFrontier (joinSealTopologyCut s)) == heraldMembershipGenerationId predecessor && joinSealControlPrefix s >= controlIndex token && joinSealControlPrefix s <= controlIndex changed && all (matchesReport s) (old <> maybe [] pure new)) k) MalformedAdmissionClaim
  require (maybe True ((== admissionManifestHeraldEpoch manifest) . joinReadyReporter) new) MalformedAdmissionClaim
  -- Cancellation retains its prior seal and evidence. An unsealed cancelled
  -- record can only come from Preparing, which has no collected evidence.
  require (k /= Nothing || (null accepted && null old && new == Nothing)) MalformedAdmissionClaim
  require ((null old && new == Nothing) || Set.fromList accepted == oldMembers record) MalformedAdmissionClaim
  mapM_ (checkSealRecipe ident (admissionManifestHeraldEpoch manifest) predecessor) k
  pure record
  where
    matchesReport s r = joinReadyAttempt r == attempt && joinReadySealDigest r == joinSealDigest s && joinReadyRecipeDigest r == joinSealRecipeDigest s && joinReadyControlPrefix r >= joinSealControlPrefix s && joinReadyControlPrefix r <= controlIndex changed
claim :: Either problem value -> Either HeraldAdmissionProblem value
claim = either (const (Left MalformedAdmissionClaim)) Right
canonicalDecode :: (Eq raw, Serialize raw) => ByteString -> (raw -> Either HeraldAdmissionProblem value) -> (value -> raw) -> ByteString -> Either HeraldAdmissionProblem value
canonicalDecode domain admit raw bytes = do
  (tag, value) <- claim (S.decode bytes)
  require (tag == domain) MalformedAdmissionClaim
  result <- admit value
  require (value == raw result && bytes == S.encode (domain, raw result)) MalformedAdmissionClaim
  pure result
encodeAdmissionCommand :: HeraldAdmissionCommand -> ByteString
encodeAdmissionCommand command = S.encode ("ECLIPS-HERALD-ADMISSION-COMMAND" :: ByteString, rawCommand command)
decodeAdmissionCommand :: ByteString -> Either HeraldAdmissionProblem HeraldAdmissionCommand
decodeAdmissionCommand = canonicalDecode "ECLIPS-HERALD-ADMISSION-COMMAND" admitCommand rawCommand
encodeAdmissionRecord :: HeraldAdmissionRecord -> ByteString
encodeAdmissionRecord record = S.encode ("ECLIPS-HERALD-ADMISSION-RECORD" :: ByteString, rawRecord record)
decodeAdmissionRecord :: ByteString -> Either HeraldAdmissionProblem HeraldAdmissionRecord
decodeAdmissionRecord = canonicalDecode "ECLIPS-HERALD-ADMISSION-RECORD" admitRecord rawRecord
encodeAdmissionProblem :: HeraldAdmissionProblem -> ByteString
encodeAdmissionProblem = S.encode . (fromIntegral :: Int -> Word8) . fromEnum
decodeAdmissionProblem :: ByteString -> Either HeraldAdmissionProblem HeraldAdmissionProblem
decodeAdmissionProblem bytes = do
  tag <- claim (S.decode bytes)
  require (tag <= fromIntegral (fromEnum (maxBound :: HeraldAdmissionProblem))) MalformedAdmissionClaim
  pure (toEnum (fromIntegral (tag :: Word8)))
admissionStateCanonicalBytes :: AdmissionState -> ByteString
admissionStateCanonicalBytes (AdmissionState catalogue records pending) = S.encode ("ECLIPS-HERALD-ADMISSION-STATE" :: ByteString, rawManifest <$> Map.elems catalogue, rawRecord <$> Map.elems records, heraldAdmissionIdCanonicalBytes <$> pending)

encodeAdmissionCheckpoint :: AdmissionState -> ByteString
encodeAdmissionCheckpoint = admissionStateCanonicalBytes

-- Restore the current owner, including its pending slot, through the same
-- checked records used by projection admission. No command replay is needed.
decodeAdmissionCheckpoint :: CheckedOracleGenesis -> ByteString -> Either String AdmissionState
decodeAdmissionCheckpoint genesis bytes = do
  (domain, rawCatalogue, rawRecords, rawPending) <- S.decode bytes
  unless (domain == ("ECLIPS-HERALD-ADMISSION-STATE" :: ByteString)) (Left "wrong admission checkpoint domain")
  catalogue <- traverse (checkpointClaim . admitManifest) rawCatalogue
  records <- traverse (checkpointClaim . admitRecord) rawRecords
  pending <- traverse (checkpointClaim . decodeHeraldAdmissionIdCanonicalBytes) rawPending
  let byEpoch = Map.fromList [(admissionManifestHeraldEpoch manifest, manifest) | manifest <- catalogue]
      byId = Map.fromList [(admissionRecordId record, record) | record <- records]
      state = AdmissionState byEpoch byId pending
      initial = initialAdmissionState (checkedOracleSystemId genesis) (checkedOracleActiveHeralds genesis)
      known = Map.fromList [(admissionManifestHeraldEpoch manifest, manifest) | manifest <- admissionCatalogue initial <> map admissionRecordManifest records]
      live = [admissionRecordId record | record <- records, case admissionRecordPhase record of AdmissionActivated {} -> False; AdmissionCancelled {} -> False; _ -> True]
  unless
    ( byEpoch == known
        && all ((== checkedOracleSystemId genesis) . admissionManifestSystem) catalogue
        && live == maybe [] pure pending
        && encodeAdmissionCheckpoint state == bytes
    )
    (Left "inconsistent admission checkpoint")
  pure state
  where
    checkpointClaim :: (Show problem) => Either problem value -> Either String value
    checkpointClaim = either (Left . show) Right
