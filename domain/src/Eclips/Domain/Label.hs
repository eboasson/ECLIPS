{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Constructor-checked shared vocabulary for the Slice-4 label protocol.
--
-- This module fixes low semantic shapes used by both the Oracle and Herald.  It
-- owns no Oracle request identity, workflow phase, wire DTO, controlled payload,
-- or live authority-tag extension.  In particular, construction of a
-- 'LabelDecisionId' from an Oracle request belongs above the Domain/Oracle
-- dependency wall.
module Eclips.Domain.Label
  ( -- * Requested and released labels
    LabelTarget,
    LabelTargetView (..),
    targetProcess,
    targetVoid,
    targetDelete,
    labelTargetView,
    ReleasedLabelState,
    ReleasedLabelStateView (..),
    releasedLabel,
    releasedDeleted,
    releasedLabelStateView,
    releasedLabelStateGeneration,
    releasedLabelStateIsDeleted,
    LabelTransitionError (..),
    LabelTransitionOutcome (..),
    labelTransition,

    -- * Independent first-overlay evidence
    InitialLabelEvidence,
    InitialLabelEvidenceSourceView (..),
    initialBootstrapLabelEvidence,
    initialPublicationLabelEvidence,
    initialLabelEvidenceObject,
    initialLabelEvidenceLabel,
    initialLabelEvidenceSource,

    -- * Released records and authority preparation
    LabelRevision,
    LabelRevisionError (..),
    mkLabelRevision,
    labelRevisionControlIndex,
    LabelRecord,
    LabelRecordError (..),
    mkLabelRecord,
    labelRecordObjectId,
    labelRecordReleasedState,
    labelRecordRevision,
    labelRecordRetainedAuthority,
    labelRecordApplicableAuthority,
    PriorAuthorityJustification,
    PriorAuthorityJustificationView (..),
    existingReleasedAuthority,
    checkedGenesisAuthority,
    establishedStructuralAuthority,
    priorAuthorityJustificationView,
    PreparedAuthorityDisposition,
    PreparedAuthorityDispositionView (..),
    noTargetAuthority,
    retainPriorAuthority,
    deriveAuthorityAtRelease,
    retirePriorAuthority,
    preparedAuthorityDispositionView,
    preparedAuthorityDisposition,

    -- * Home acceptance cut
    LabelProcessAcceptancePosition,
    LabelProcessAcceptancePositionError (..),
    mkLabelProcessAcceptancePosition,
    firstLabelProcessAcceptancePosition,
    nextLabelProcessAcceptancePosition,
    labelProcessAcceptancePositionProcess,
    labelProcessAcceptancePositionOrdinal,
    HomeLabelAcceptanceCut,
    homeLabelAcceptanceCut,
    homeLabelAcceptanceCutCallerProcessEpoch,
    homeLabelAcceptanceCutProcessPosition,
    homeLabelAcceptanceCutPublicationPrefix,
    HomeLabelAcceptanceCutCanonicalProblem (..),
    homeLabelAcceptanceCutCanonicalBytes,
    decodeHomeLabelAcceptanceCutCanonicalBytes,

    -- * Readiness and digest evidence
    LabelDigestError (..),
    PreparedLabelDigest,
    mkPreparedLabelDigest,
    preparedLabelDigestBytes,
    LabelOutcomeDigest,
    mkLabelOutcomeDigest,
    labelOutcomeDigestBytes,
    LabelInstallationReport,
    labelInstallationReport,
    labelInstallationDecisionId,
    labelInstallationReporter,
    labelInstallationControlIndex,
    labelInstallationOutcomeDigest,
    FenceReadyReport,
    mkFenceReadyReport,
    fenceReadyReportDecisionId,
    fenceReadyReportReporter,
    fenceReadyReportMemberSetDigest,
    FenceReadyReportCanonicalProblem (..),
    fenceReadyReportCanonicalBytes,
    decodeFenceReadyReportCanonicalBytes,
    GenerationQualifiedFenceReadyReport,
    GenerationQualifiedLabelEvidenceProblem (..),
    qualifyFenceReadyReport,
    generationQualifiedFenceReadyReportGeneration,
    generationQualifiedFenceReadyReportValue,
    generationQualifiedFenceReadyReportCanonicalBytes,

    -- * Normalized preparation evidence
    PreparedLabelFacts,
    PreparedLabelFactsError (..),
    mkPreparedLabelFacts,
    preparedLabelFactsDecisionId,
    preparedLabelFactsResolveIndex,
    preparedLabelFactsObjectId,
    preparedLabelFactsProposedOutcome,
    preparedLabelFactsExpectedPriorState,
    preparedLabelFactsExpectedPriorRevision,
    preparedLabelFactsCatalogueDigest,
    preparedLabelFactsExpectedPriorAuthority,
    preparedLabelFactsPriorAuthorityJustification,
    preparedLabelFactsAuthorityDisposition,
    preparedLabelFactsMemberSetDigest,
    PreparedLabelFactsCanonicalProblem (..),
    preparedLabelFactsCanonicalBytes,
    decodePreparedLabelFactsCanonicalBytes,
    derivePreparedLabelDigest,
    GenerationQualifiedPreparedLabelFacts,
    qualifyPreparedLabelFacts,
    generationQualifiedPreparedLabelFactsGeneration,
    generationQualifiedPreparedLabelFactsValue,
    generationQualifiedPreparedLabelFactsCanonicalBytes,
    PreparedLabelReport,
    PreparedLabelReportError (..),
    preparedLabelReport,
    admitPreparedLabelReport,
    preparedLabelReportReporter,
    preparedLabelReportFacts,
    preparedLabelReportDigest,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Serialize (Serialize)
import Data.Serialize qualified as Serialize
import Data.Word (Word64, Word8)
import Eclips.Domain.Alignment
  ( HeraldPublicationPositionError,
    HeraldPublicationPrefix (..),
    heraldPublicationPositionWord64,
    heraldPublicationPrefixPosition,
    mkHeraldPublicationPosition,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    AuthorityEpochCanonicalProblem,
    ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    IdentityError,
    LabelDecisionId,
    ProcessEpochId,
    PublicationId,
    StructuralOccurrenceId,
    StructuralSequenceError,
    TopologyCutId,
    authorityEpochCanonicalBytes,
    controlIndex,
    controlIndexWord64,
    decodeAuthorityEpochCanonicalBytes,
    genesisAuthorityEpoch,
    globalObjectIdBytes,
    heraldEpochBytes,
    labelDecisionIdBytes,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkLabelDecisionId,
    mkProcessEpochId,
    mkStructuralSequence,
    mkTopologyCutId,
    processEpochIdBytes,
    structuralAuthorityEpoch,
    structuralOccurrenceId,
    structuralOccurrenceSourceHeraldEpoch,
    structuralOccurrenceSourceSequence,
    structuralSequenceWord64,
    topologyCutIdBytes,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
  )
import Eclips.Domain.Sort.Profile
  ( CatalogueDigest,
    catalogueDigestBytes,
    mkCatalogueDigest,
  )
import Eclips.Domain.Sort.Profile qualified as SortProfile
import Eclips.Domain.Startup
  ( InitialProjectionDigest,
    initialProjectionDigestBytes,
    mkInitialProjectionDigest,
  )
import Eclips.Domain.Startup qualified as Startup
import Eclips.Domain.Topology
  ( MemberSetDigest,
    TopologyDigestError,
    memberSetDigestBytes,
    mkMemberSetDigest,
  )
import Eclips.Domain.Value
  ( Label,
    LabelOwner (..),
    ValueError,
    ValueView (..),
    canonicalValueByteString,
    canonicalValueBytes,
    decodeCanonicalValue,
    labelValue,
    viewValue,
  )
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import GHC.Generics (Generic)

-- | A checked Herald's observation before the object's first Oracle overlay.
-- The source binds this small fact to its bootstrap root or immutable
-- publication; no publication payload or requested CAS value is retained.
-- Authority provenance is carried separately by the label protocol.
data InitialLabelEvidence
  = InitialLabelEvidence
      !GlobalObjectId
      !Label
      !InitialLabelEvidenceSourceView
  deriving stock (Eq, Show)

data InitialLabelEvidenceSourceView
  = InitialBootstrapLabelEvidenceView !ProcessEpochId
  | InitialPublicationLabelEvidenceView !PublicationId
  deriving stock (Eq, Show)

initialBootstrapLabelEvidence ::
  GlobalObjectId -> ProcessEpochId -> InitialLabelEvidence
initialBootstrapLabelEvidence object process =
  InitialLabelEvidence object (ProcessLabel process, 0) (InitialBootstrapLabelEvidenceView process)

-- | A later observation with a nonzero generation cannot establish initial
-- state. It can still support a request whose Oracle record already exists.
-- A raw zombie observation is retained: the Oracle checks its canonical End
-- knowledge rather than letting this constructor assert process lifecycle.
initialPublicationLabelEvidence ::
  GlobalObjectId -> Label -> PublicationId -> Maybe InitialLabelEvidence
initialPublicationLabelEvidence object label@(_, generation) publication
  | generation == 0 = Just (InitialLabelEvidence object label (InitialPublicationLabelEvidenceView publication))
  | otherwise = Nothing

initialLabelEvidenceObject :: InitialLabelEvidence -> GlobalObjectId
initialLabelEvidenceObject (InitialLabelEvidence object _ _) = object

initialLabelEvidenceLabel :: InitialLabelEvidence -> Label
initialLabelEvidenceLabel (InitialLabelEvidence _ label _) = label

initialLabelEvidenceSource :: InitialLabelEvidence -> InitialLabelEvidenceSourceView
initialLabelEvidenceSource (InitialLabelEvidence _ _ source) = source

-- | Requested target of one label operation.  Zombie is deliberately absent.
data LabelTarget
  = TargetProcess ProcessEpochId
  | TargetVoid
  | TargetDelete
  deriving stock (Eq, Ord, Show)

data LabelTargetView
  = TargetProcessView ProcessEpochId
  | TargetVoidView
  | TargetDeleteView
  deriving stock (Eq, Ord, Show)

targetProcess :: ProcessEpochId -> LabelTarget
targetProcess = TargetProcess

targetVoid :: LabelTarget
targetVoid = TargetVoid

targetDelete :: LabelTarget
targetDelete = TargetDelete

labelTargetView :: LabelTarget -> LabelTargetView
labelTargetView target = case target of
  TargetProcess process -> TargetProcessView process
  TargetVoid -> TargetVoidView
  TargetDelete -> TargetDeleteView

-- | The canonical Oracle overlay: either a regular semantic label or terminal
-- deletion.  Delete is not a 'Label' and therefore cannot enter a value/query.
data ReleasedLabelState
  = ReleasedLabel Label
  | ReleasedDeleted Word64
  deriving stock (Eq, Ord, Show)

data ReleasedLabelStateView
  = ReleasedLabelView Label
  | ReleasedDeletedView Word64
  deriving stock (Eq, Ord, Show)

releasedLabel :: Label -> ReleasedLabelState
releasedLabel = ReleasedLabel

releasedDeleted :: Word64 -> ReleasedLabelState
releasedDeleted = ReleasedDeleted

releasedLabelStateView :: ReleasedLabelState -> ReleasedLabelStateView
releasedLabelStateView state = case state of
  ReleasedLabel label -> ReleasedLabelView label
  ReleasedDeleted generation -> ReleasedDeletedView generation

-- | The only generation retained for this state, including terminal deletion.
releasedLabelStateGeneration :: ReleasedLabelState -> Word64
releasedLabelStateGeneration state = case state of
  ReleasedLabel (_, generation) -> generation
  ReleasedDeleted generation -> generation

releasedLabelStateIsDeleted :: ReleasedLabelState -> Bool
releasedLabelStateIsDeleted state = case state of
  ReleasedLabel _ -> False
  ReleasedDeleted _ -> True

data LabelTransitionError
  = LabelTransitionFromDeleted
  | LabelTransitionCallerMismatch ProcessEpochId ProcessEpochId
  | LabelTransitionTargetMustBeCaller ProcessEpochId ProcessEpochId
  deriving stock (Eq, Show)

data LabelTransitionOutcome
  = LabelExpectedMismatch
  | LabelTransitionApplied ReleasedLabelState
  deriving stock (Eq, Show)

-- | Apply the value-CAS and the closed caller/target algebra.
--
-- Process liveness, nameability, possession, and target existence are owned by
-- the surrounding transition and must be checked separately.
labelTransition ::
  ProcessEpochId ->
  Label ->
  ReleasedLabelState ->
  LabelTarget ->
  Either LabelTransitionError LabelTransitionOutcome
labelTransition caller expected prior target = case prior of
  ReleasedDeleted _ -> Left LabelTransitionFromDeleted
  ReleasedLabel actual
    | actual /= expected -> Right LabelExpectedMismatch
  ReleasedLabel (ProcessLabel owner, generation)
    | caller /= owner -> Left (LabelTransitionCallerMismatch owner caller)
    | otherwise -> Right (LabelTransitionApplied (targetReleasedState (generation + 1) target))
  ReleasedLabel (VoidLabel, generation) -> transitionFromVoidLike caller (generation + 1) target
  ReleasedLabel (ZombieLabel _, generation) -> transitionFromVoidLike caller (generation + 1) target

transitionFromVoidLike ::
  ProcessEpochId ->
  Word64 ->
  LabelTarget ->
  Either LabelTransitionError LabelTransitionOutcome
transitionFromVoidLike caller successor target = case target of
  TargetProcess process
    | process /= caller ->
        Left (LabelTransitionTargetMustBeCaller caller process)
  _ -> Right (LabelTransitionApplied (targetReleasedState successor target))

-- The successor is proposed from the captured prior. It becomes authoritative
-- only at Release; replay installs this exact recorded state rather than taking
-- another successor. Counter exhaustion is outside the finite-run model.
targetReleasedState :: Word64 -> LabelTarget -> ReleasedLabelState
targetReleasedState generation target = case target of
  TargetProcess process -> ReleasedLabel (ProcessLabel process, generation)
  TargetVoid -> ReleasedLabel (VoidLabel, generation)
  TargetDelete -> ReleasedDeleted generation

-- | Positive release control index of one successful label decision.
newtype LabelRevision = LabelRevision ControlIndex
  deriving stock (Eq, Ord, Show)

data LabelRevisionError
  = LabelRevisionMustBePositive
  deriving stock (Eq, Show)

mkLabelRevision :: ControlIndex -> Either LabelRevisionError LabelRevision
mkLabelRevision index
  | controlIndexWord64 index == 0 = Left LabelRevisionMustBePositive
  | otherwise = Right (LabelRevision index)

labelRevisionControlIndex :: LabelRevision -> ControlIndex
labelRevisionControlIndex (LabelRevision index) = index

-- | One released Oracle label overlay.
--
-- The last authority may remain on a deleted record as historical evidence;
-- 'labelRecordApplicableAuthority' makes the terminal non-applicability total.
data LabelRecord
  = LabelRecord
      GlobalObjectId
      ReleasedLabelState
      LabelRevision
      (Maybe AuthorityEpoch)
  deriving stock (Eq, Show)

data LabelRecordError
  = LabelRecordStoredZombieNotPermitted ProcessEpochId
  deriving stock (Eq, Show)

-- | Construct a stored Oracle overlay.
--
-- End never rewrites the stored label to zombie; zombie is derived only by an
-- effective-label projection.  Rejecting it here prevents a caller from
-- confusing projected and retained state.
mkLabelRecord ::
  GlobalObjectId ->
  ReleasedLabelState ->
  LabelRevision ->
  Maybe AuthorityEpoch ->
  Either LabelRecordError LabelRecord
mkLabelRecord object state revision authority = case state of
  ReleasedLabel (ZombieLabel process, _) ->
    Left (LabelRecordStoredZombieNotPermitted process)
  _ -> Right (LabelRecord object state revision authority)

labelRecordObjectId :: LabelRecord -> GlobalObjectId
labelRecordObjectId (LabelRecord object _ _ _) = object

labelRecordReleasedState :: LabelRecord -> ReleasedLabelState
labelRecordReleasedState (LabelRecord _ state _ _) = state

labelRecordRevision :: LabelRecord -> LabelRevision
labelRecordRevision (LabelRecord _ _ revision _) = revision

labelRecordRetainedAuthority :: LabelRecord -> Maybe AuthorityEpoch
labelRecordRetainedAuthority (LabelRecord _ _ _ authority) = authority

labelRecordApplicableAuthority ::
  (ProcessEpochId -> Bool) ->
  LabelRecord ->
  Maybe AuthorityEpoch
labelRecordApplicableAuthority isEnded (LabelRecord _ state _ authority) =
  case state of
    ReleasedLabel (ProcessLabel process, _)
      | not (isEnded process) -> authority
    ReleasedLabel _ -> Nothing
    ReleasedDeleted _ -> Nothing

data PriorAuthorityJustification
  = ExistingReleasedAuthority LabelRevision
  | CheckedGenesisAuthority InitialProjectionDigest
  | EstablishedStructuralAuthority StructuralOccurrenceId TopologyCutId
  deriving stock (Eq, Ord, Show)

data PriorAuthorityJustificationView
  = ExistingReleasedAuthorityView LabelRevision
  | CheckedGenesisAuthorityView InitialProjectionDigest
  | EstablishedStructuralAuthorityView StructuralOccurrenceId TopologyCutId
  deriving stock (Eq, Ord, Show)

existingReleasedAuthority :: LabelRevision -> PriorAuthorityJustification
existingReleasedAuthority = ExistingReleasedAuthority

checkedGenesisAuthority :: InitialProjectionDigest -> PriorAuthorityJustification
checkedGenesisAuthority = CheckedGenesisAuthority

establishedStructuralAuthority ::
  StructuralOccurrenceId ->
  TopologyCutId ->
  PriorAuthorityJustification
establishedStructuralAuthority = EstablishedStructuralAuthority

priorAuthorityJustificationView ::
  PriorAuthorityJustification ->
  PriorAuthorityJustificationView
priorAuthorityJustificationView justification = case justification of
  ExistingReleasedAuthority revision -> ExistingReleasedAuthorityView revision
  CheckedGenesisAuthority digest -> CheckedGenesisAuthorityView digest
  EstablishedStructuralAuthority occurrence cut ->
    EstablishedStructuralAuthorityView occurrence cut

data PreparedAuthorityDisposition
  = NoTargetAuthority
  | RetainPriorAuthority AuthorityEpoch
  | DeriveAuthorityAtRelease
  | RetirePriorAuthority AuthorityEpoch
  deriving stock (Eq, Ord, Show)

data PreparedAuthorityDispositionView
  = NoTargetAuthorityView
  | RetainPriorAuthorityView AuthorityEpoch
  | DeriveAuthorityAtReleaseView
  | RetirePriorAuthorityView AuthorityEpoch
  deriving stock (Eq, Ord, Show)

noTargetAuthority :: PreparedAuthorityDisposition
noTargetAuthority = NoTargetAuthority

retainPriorAuthority :: AuthorityEpoch -> PreparedAuthorityDisposition
retainPriorAuthority = RetainPriorAuthority

deriveAuthorityAtRelease :: PreparedAuthorityDisposition
deriveAuthorityAtRelease = DeriveAuthorityAtRelease

retirePriorAuthority :: AuthorityEpoch -> PreparedAuthorityDisposition
retirePriorAuthority = RetirePriorAuthority

preparedAuthorityDispositionView ::
  PreparedAuthorityDisposition ->
  PreparedAuthorityDispositionView
preparedAuthorityDispositionView disposition = case disposition of
  NoTargetAuthority -> NoTargetAuthorityView
  RetainPriorAuthority authority -> RetainPriorAuthorityView authority
  DeriveAuthorityAtRelease -> DeriveAuthorityAtReleaseView
  RetirePriorAuthority authority -> RetirePriorAuthorityView authority

-- | Determine the release-time authority recipe for an already admitted
-- effective prior label and proposed outcome.
preparedAuthorityDisposition ::
  Maybe AuthorityEpoch ->
  Label ->
  ReleasedLabelState ->
  PreparedAuthorityDisposition
preparedAuthorityDisposition priorAuthority priorLabel proposed =
  case priorAuthority of
    Nothing -> NoTargetAuthority
    Just authority -> case proposed of
      ReleasedDeleted _ -> RetirePriorAuthority authority
      ReleasedLabel (ProcessLabel nextProcess, _)
        | fst priorLabel /= ProcessLabel nextProcess -> DeriveAuthorityAtRelease
      ReleasedLabel _ -> RetainPriorAuthority authority

-- | Positive accepted-call position scoped to one process epoch.
data LabelProcessAcceptancePosition
  = LabelProcessAcceptancePosition ProcessEpochId Word64
  deriving stock (Eq, Ord, Show)

data LabelProcessAcceptancePositionError
  = LabelProcessAcceptancePositionMustBePositive
  deriving stock (Eq, Show)

mkLabelProcessAcceptancePosition ::
  ProcessEpochId ->
  Word64 ->
  Either LabelProcessAcceptancePositionError LabelProcessAcceptancePosition
mkLabelProcessAcceptancePosition _ 0 =
  Left LabelProcessAcceptancePositionMustBePositive
mkLabelProcessAcceptancePosition process ordinal =
  Right (LabelProcessAcceptancePosition process ordinal)

firstLabelProcessAcceptancePosition ::
  ProcessEpochId ->
  LabelProcessAcceptancePosition
firstLabelProcessAcceptancePosition process =
  LabelProcessAcceptancePosition process 1

-- | Advance under the profile-wide finite-run no-wrap premise.
nextLabelProcessAcceptancePosition ::
  LabelProcessAcceptancePosition ->
  LabelProcessAcceptancePosition
nextLabelProcessAcceptancePosition
  (LabelProcessAcceptancePosition process ordinal) =
    LabelProcessAcceptancePosition process (ordinal + 1)

labelProcessAcceptancePositionProcess ::
  LabelProcessAcceptancePosition ->
  ProcessEpochId
labelProcessAcceptancePositionProcess
  (LabelProcessAcceptancePosition process _) = process

labelProcessAcceptancePositionOrdinal ::
  LabelProcessAcceptancePosition ->
  Word64
labelProcessAcceptancePositionOrdinal
  (LabelProcessAcceptancePosition _ ordinal) = ordinal

-- | Exact home cut captured atomically with label semantic acceptance.
-- The caller is derived from the checked process-scoped position, so the two
-- facts cannot disagree.
data HomeLabelAcceptanceCut
  = HomeLabelAcceptanceCut
      LabelProcessAcceptancePosition
      HeraldPublicationPrefix
  deriving stock (Eq, Ord, Show)

homeLabelAcceptanceCut ::
  LabelProcessAcceptancePosition ->
  HeraldPublicationPrefix ->
  HomeLabelAcceptanceCut
homeLabelAcceptanceCut = HomeLabelAcceptanceCut

homeLabelAcceptanceCutCallerProcessEpoch ::
  HomeLabelAcceptanceCut ->
  ProcessEpochId
homeLabelAcceptanceCutCallerProcessEpoch
  (HomeLabelAcceptanceCut position _) =
    labelProcessAcceptancePositionProcess position

homeLabelAcceptanceCutProcessPosition ::
  HomeLabelAcceptanceCut ->
  LabelProcessAcceptancePosition
homeLabelAcceptanceCutProcessPosition (HomeLabelAcceptanceCut position _) =
  position

homeLabelAcceptanceCutPublicationPrefix ::
  HomeLabelAcceptanceCut ->
  HeraldPublicationPrefix
homeLabelAcceptanceCutPublicationPrefix (HomeLabelAcceptanceCut _ prefix) =
  prefix

-- | Canonical checked home-cut evidence for enclosing protocol transcripts.
homeLabelAcceptanceCutCanonicalBytes :: HomeLabelAcceptanceCut -> ByteString
homeLabelAcceptanceCutCanonicalBytes (HomeLabelAcceptanceCut position prefix) =
  Serialize.encode
    ( HomeLabelAcceptanceCutTranscript
        homeLabelAcceptanceCutDomain
        (processEpochIdBytes (labelProcessAcceptancePositionProcess position))
        (labelProcessAcceptancePositionOrdinal position)
        (heraldPublicationPositionWord64 <$> heraldPublicationPrefixPosition prefix)
    )

data HomeLabelAcceptanceCutCanonicalProblem
  = HomeLabelAcceptanceCutCanonicalDecodeFailed String
  | HomeLabelAcceptanceCutCanonicalWrongDomain ByteString
  | HomeLabelAcceptanceCutCanonicalInvalidProcessEpoch IdentityError
  | HomeLabelAcceptanceCutCanonicalInvalidProcessPosition
      LabelProcessAcceptancePositionError
  | HomeLabelAcceptanceCutCanonicalInvalidPublicationPosition
      HeraldPublicationPositionError
  | HomeLabelAcceptanceCutCanonicalNonCanonical
  deriving stock (Eq, Show)

-- | Checked inverse of 'homeLabelAcceptanceCutCanonicalBytes'.
decodeHomeLabelAcceptanceCutCanonicalBytes ::
  ByteString ->
  Either HomeLabelAcceptanceCutCanonicalProblem HomeLabelAcceptanceCut
decodeHomeLabelAcceptanceCutCanonicalBytes bytes = do
  HomeLabelAcceptanceCutTranscript domain processBytes ordinal prefixPosition <-
    case Serialize.decode bytes of
      Left problem ->
        Left (HomeLabelAcceptanceCutCanonicalDecodeFailed problem)
      Right transcript -> Right transcript
  if domain == homeLabelAcceptanceCutDomain
    then Right ()
    else Left (HomeLabelAcceptanceCutCanonicalWrongDomain domain)
  process <-
    either
      (Left . HomeLabelAcceptanceCutCanonicalInvalidProcessEpoch)
      Right
      (mkProcessEpochId processBytes)
  position <-
    either
      (Left . HomeLabelAcceptanceCutCanonicalInvalidProcessPosition)
      Right
      (mkLabelProcessAcceptancePosition process ordinal)
  prefix <- case prefixPosition of
    Nothing -> Right EmptyHeraldPublicationPrefix
    Just rawPosition -> do
      checkedPosition <-
        either
          (Left . HomeLabelAcceptanceCutCanonicalInvalidPublicationPosition)
          Right
          (mkHeraldPublicationPosition rawPosition)
      Right (HeraldPublicationPrefixThrough checkedPosition)
  let cut = homeLabelAcceptanceCut position prefix
  if homeLabelAcceptanceCutCanonicalBytes cut == bytes
    then Right cut
    else Left HomeLabelAcceptanceCutCanonicalNonCanonical

homeLabelAcceptanceCutDomain :: ByteString
homeLabelAcceptanceCutDomain = "ECLIPS-HOME-LABEL-ACCEPTANCE-CUT"

data HomeLabelAcceptanceCutTranscript
  = HomeLabelAcceptanceCutTranscript
      ByteString
      ByteString
      Word64
      (Maybe Word64)
  deriving stock (Generic)
  deriving anyclass (Serialize)

sha256ByteCount :: Int
sha256ByteCount = 32

data LabelDigestError = LabelDigestWrongByteCount
  { expectedLabelDigestByteCount :: Int,
    actualLabelDigestByteCount :: Int
  }
  deriving stock (Eq, Show)

newtype PreparedLabelDigest = PreparedLabelDigest ByteString
  deriving stock (Eq, Ord)

newtype LabelOutcomeDigest = LabelOutcomeDigest ByteString
  deriving stock (Eq, Ord)

instance Show PreparedLabelDigest where
  show = renderGroupedHex . preparedLabelDigestBytes

instance Show LabelOutcomeDigest where
  show = renderGroupedHex . labelOutcomeDigestBytes

mkPreparedLabelDigest :: ByteString -> Either LabelDigestError PreparedLabelDigest
mkPreparedLabelDigest = fmap PreparedLabelDigest . checkDigestBytes

preparedLabelDigestBytes :: PreparedLabelDigest -> ByteString
preparedLabelDigestBytes (PreparedLabelDigest bytes) = bytes

mkLabelOutcomeDigest :: ByteString -> Either LabelDigestError LabelOutcomeDigest
mkLabelOutcomeDigest = fmap LabelOutcomeDigest . checkDigestBytes

labelOutcomeDigestBytes :: LabelOutcomeDigest -> ByteString
labelOutcomeDigestBytes (LabelOutcomeDigest bytes) = bytes

-- | Compact terminal-installation evidence sent directly to the current
-- completion collector. The canonical terminal coordinate and digest bind the
-- report to one immutable outcome; membership and sender admission belong to
-- the receiving Herald owner.
data LabelInstallationReport
  = LabelInstallationReport
      !LabelDecisionId
      !HeraldEpoch
      !ControlIndex
      !LabelOutcomeDigest
  deriving stock (Eq, Ord, Show)

labelInstallationReport ::
  LabelDecisionId ->
  HeraldEpoch ->
  ControlIndex ->
  LabelOutcomeDigest ->
  LabelInstallationReport
labelInstallationReport = LabelInstallationReport

labelInstallationDecisionId :: LabelInstallationReport -> LabelDecisionId
labelInstallationDecisionId (LabelInstallationReport decision _ _ _) = decision

labelInstallationReporter :: LabelInstallationReport -> HeraldEpoch
labelInstallationReporter (LabelInstallationReport _ reporter _ _) = reporter

labelInstallationControlIndex :: LabelInstallationReport -> ControlIndex
labelInstallationControlIndex (LabelInstallationReport _ _ index _) = index

labelInstallationOutcomeDigest :: LabelInstallationReport -> LabelOutcomeDigest
labelInstallationOutcomeDigest (LabelInstallationReport _ _ _ digest) = digest

checkDigestBytes :: ByteString -> Either LabelDigestError ByteString
checkDigestBytes bytes
  | ByteString.length bytes == sha256ByteCount = Right bytes
  | otherwise =
      Left
        LabelDigestWrongByteCount
          { expectedLabelDigestByteCount = sha256ByteCount,
            actualLabelDigestByteCount = ByteString.length bytes
          }

-- | One captured participant's readiness claim. The source has already settled
-- its selected publication group before requesting Open. Membership and reporter
-- admission require the captured workflow or generation, not self-contained rows
-- duplicated inside every report.
data FenceReadyReport
  = FenceReadyReport LabelDecisionId HeraldEpoch MemberSetDigest
  deriving stock (Eq, Show)

mkFenceReadyReport ::
  LabelDecisionId -> HeraldEpoch -> MemberSetDigest -> FenceReadyReport
mkFenceReadyReport = FenceReadyReport

fenceReadyReportDecisionId :: FenceReadyReport -> LabelDecisionId
fenceReadyReportDecisionId (FenceReadyReport decision _ _) = decision

fenceReadyReportReporter :: FenceReadyReport -> HeraldEpoch
fenceReadyReportReporter (FenceReadyReport _ reporter _) = reporter

fenceReadyReportMemberSetDigest :: FenceReadyReport -> MemberSetDigest
fenceReadyReportMemberSetDigest (FenceReadyReport _ _ digest) = digest

-- | Compact canonical Ready claim for enclosing Oracle transcripts.
fenceReadyReportCanonicalBytes :: FenceReadyReport -> ByteString
fenceReadyReportCanonicalBytes report =
  Serialize.encode
    ( FenceReadyReportTranscript
        fenceReadyReportDomain
        (labelDecisionIdBytes (fenceReadyReportDecisionId report))
        (heraldEpochBytes (fenceReadyReportReporter report))
        (memberSetDigestBytes (fenceReadyReportMemberSetDigest report))
    )

data FenceReadyReportTranscript
  = FenceReadyReportTranscript ByteString ByteString ByteString ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data FenceReadyReportCanonicalProblem
  = FenceReadyReportCanonicalDecodeFailed String
  | FenceReadyReportCanonicalWrongDomain ByteString
  | FenceReadyReportCanonicalInvalidIdentity IdentityError
  | FenceReadyReportCanonicalInvalidMemberSetDigest TopologyDigestError
  | FenceReadyReportCanonicalNonCanonical
  deriving stock (Eq, Show)

-- | Check the canonical shape and exact nominal field widths. The enclosing
-- Oracle workflow authenticates the reporter and captured membership digest;
-- 'qualifyFenceReadyReport' performs the corresponding generation check.
decodeFenceReadyReportCanonicalBytes ::
  ByteString -> Either FenceReadyReportCanonicalProblem FenceReadyReport
decodeFenceReadyReportCanonicalBytes bytes = do
  FenceReadyReportTranscript domain decisionBytes reporterBytes membersBytes <-
    case Serialize.decode bytes of
      Left problem -> Left (FenceReadyReportCanonicalDecodeFailed problem)
      Right transcript -> Right transcript
  if domain == fenceReadyReportDomain
    then Right ()
    else Left (FenceReadyReportCanonicalWrongDomain domain)
  decision <- decodeFenceIdentity mkLabelDecisionId decisionBytes
  reporter <- decodeFenceIdentity mkHeraldEpoch reporterBytes
  membersDigest <-
    either
      (Left . FenceReadyReportCanonicalInvalidMemberSetDigest)
      Right
      (mkMemberSetDigest membersBytes)
  let report = mkFenceReadyReport decision reporter membersDigest
  if fenceReadyReportCanonicalBytes report == bytes
    then Right report
    else Left FenceReadyReportCanonicalNonCanonical

fenceReadyReportDomain :: ByteString
fenceReadyReportDomain = "ECLIPS-LABEL-FENCE-READY"

decodeFenceIdentity ::
  (ByteString -> Either IdentityError identity) ->
  ByteString ->
  Either FenceReadyReportCanonicalProblem identity
decodeFenceIdentity constructor bytes =
  either
    (Left . FenceReadyReportCanonicalInvalidIdentity)
    Right
    (constructor bytes)

-- | A label evidence row qualified by the exact membership generation under
-- which it was captured.  The current live Oracle schema still carries the
-- nested report alone; this wrapper is the checked semantic form used by the
-- Step-15 structural/membership composition until that schema changes
-- atomically.
data GenerationQualifiedFenceReadyReport
  = GenerationQualifiedFenceReadyReport
      HeraldMembershipGenerationId
      FenceReadyReport
  deriving stock (Eq, Show)

data GenerationQualifiedLabelEvidenceProblem
  = GenerationQualifiedReadyMemberSetMismatch
      MemberSetDigest
      MemberSetDigest
  | GenerationQualifiedReadyReporterNotMember HeraldEpoch
  | GenerationQualifiedPreparedMemberSetMismatch
      MemberSetDigest
      MemberSetDigest
  deriving stock (Eq, Show)

qualifyFenceReadyReport ::
  HeraldMembershipGeneration ->
  FenceReadyReport ->
  Either
    GenerationQualifiedLabelEvidenceProblem
    GenerationQualifiedFenceReadyReport
qualifyFenceReadyReport generation report
  | observed /= expected =
      Left (GenerationQualifiedReadyMemberSetMismatch expected observed)
  | fenceReadyReportReporter report `notElem` heraldMembershipGenerationActiveHeraldEpochs generation =
      Left (GenerationQualifiedReadyReporterNotMember (fenceReadyReportReporter report))
  | otherwise =
      Right
        ( GenerationQualifiedFenceReadyReport
            (heraldMembershipGenerationId generation)
            report
        )
  where
    expected = heraldMembershipGenerationActiveMemberSetDigest generation
    observed = fenceReadyReportMemberSetDigest report

generationQualifiedFenceReadyReportGeneration ::
  GenerationQualifiedFenceReadyReport ->
  HeraldMembershipGenerationId
generationQualifiedFenceReadyReportGeneration
  (GenerationQualifiedFenceReadyReport generation _) = generation

generationQualifiedFenceReadyReportValue ::
  GenerationQualifiedFenceReadyReport ->
  FenceReadyReport
generationQualifiedFenceReadyReportValue
  (GenerationQualifiedFenceReadyReport _ report) = report

generationQualifiedFenceReadyReportCanonicalBytes ::
  GenerationQualifiedFenceReadyReport ->
  ByteString
generationQualifiedFenceReadyReportCanonicalBytes qualified =
  Serialize.encode
    ( GenerationQualifiedLabelEvidenceTranscript
        "ECLIPS-GENERATION-QUALIFIED-FENCE-READY"
        ( heraldMembershipGenerationIdBytes
            (generationQualifiedFenceReadyReportGeneration qualified)
        )
        ( fenceReadyReportCanonicalBytes
            (generationQualifiedFenceReadyReportValue qualified)
        )
    )

data GenerationQualifiedLabelEvidenceTranscript
  = GenerationQualifiedLabelEvidenceTranscript
      ByteString
      ByteString
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | Common preparation facts whose equality is required from every captured
-- Herald.  Local patch/Store/Graph details are deliberately absent.
data PreparedLabelFacts
  = PreparedLabelFacts
      LabelDecisionId
      ControlIndex
      GlobalObjectId
      ReleasedLabelState
      ReleasedLabelState
      (Maybe LabelRevision)
      CatalogueDigest
      (Maybe AuthorityEpoch)
      (Maybe PriorAuthorityJustification)
      PreparedAuthorityDisposition
      MemberSetDigest
  deriving stock (Eq, Show)

data PreparedLabelFactsError
  = PreparedLabelFactsResolveIndexMustBePositive
  | PreparedLabelFactsExpectedPriorDeleted
  | PreparedLabelFactsMissingAuthorityJustification AuthorityEpoch
  | PreparedLabelFactsUnexpectedAuthorityJustification
      PriorAuthorityJustification
  | PreparedLabelFactsRevisionJustificationMismatch
      (Maybe LabelRevision)
      PriorAuthorityJustification
  | PreparedLabelFactsAuthorityJustificationMismatch
      AuthorityEpoch
      PriorAuthorityJustification
  | PreparedLabelFactsAuthorityDispositionMismatch
      PreparedAuthorityDisposition
      PreparedAuthorityDisposition
  | PreparedLabelFactsZombieOutcomeNotPermitted ProcessEpochId
  | PreparedLabelFactsGenerationMismatch Word64 Word64
  deriving stock (Eq, Show)

mkPreparedLabelFacts ::
  LabelDecisionId ->
  ControlIndex ->
  GlobalObjectId ->
  ReleasedLabelState ->
  ReleasedLabelState ->
  Maybe LabelRevision ->
  CatalogueDigest ->
  Maybe AuthorityEpoch ->
  Maybe PriorAuthorityJustification ->
  PreparedAuthorityDisposition ->
  MemberSetDigest ->
  Either PreparedLabelFactsError PreparedLabelFacts
mkPreparedLabelFacts decision resolveIndex object proposed expectedPrior expectedRevision catalogue expectedAuthority justification disposition members = do
  if controlIndexWord64 resolveIndex == 0
    then Left PreparedLabelFactsResolveIndexMustBePositive
    else Right ()
  priorLabel <- case expectedPrior of
    ReleasedDeleted _ -> Left PreparedLabelFactsExpectedPriorDeleted
    ReleasedLabel label -> Right label
  case proposed of
    ReleasedLabel (ZombieLabel process, _) ->
      Left (PreparedLabelFactsZombieOutcomeNotPermitted process)
    _ -> Right ()
  validatePreparedAuthority expectedRevision expectedAuthority justification
  let expectedDisposition =
        preparedAuthorityDisposition expectedAuthority priorLabel proposed
  if disposition == expectedDisposition
    then Right ()
    else
      Left
        ( PreparedLabelFactsAuthorityDispositionMismatch
            expectedDisposition
            disposition
        )
  let expectedGeneration = snd priorLabel + 1
      proposedGeneration = releasedLabelStateGeneration proposed
  if proposedGeneration == expectedGeneration
    then Right ()
    else Left (PreparedLabelFactsGenerationMismatch expectedGeneration proposedGeneration)
  Right
    ( PreparedLabelFacts
        decision
        resolveIndex
        object
        proposed
        expectedPrior
        expectedRevision
        catalogue
        expectedAuthority
        justification
        disposition
        members
    )

validatePreparedAuthority ::
  Maybe LabelRevision ->
  Maybe AuthorityEpoch ->
  Maybe PriorAuthorityJustification ->
  Either PreparedLabelFactsError ()
validatePreparedAuthority expectedRevision expectedAuthority justification =
  case (expectedAuthority, justification) of
    (Nothing, Just supplied) ->
      Left (PreparedLabelFactsUnexpectedAuthorityJustification supplied)
    (Just expected, Nothing) ->
      Left (PreparedLabelFactsMissingAuthorityJustification expected)
    (Nothing, Nothing) -> Right ()
    (Just authority, Just supplied) -> do
      validatePreparedRevision expectedRevision supplied
      if justificationSupportsAuthority authority supplied
        then Right ()
        else
          Left
            (PreparedLabelFactsAuthorityJustificationMismatch authority supplied)

validatePreparedRevision ::
  Maybe LabelRevision ->
  PriorAuthorityJustification ->
  Either PreparedLabelFactsError ()
validatePreparedRevision expectedRevision justification =
  case justification of
    ExistingReleasedAuthority revision
      | expectedRevision == Just revision -> Right ()
    CheckedGenesisAuthority _
      | expectedRevision == Nothing -> Right ()
    EstablishedStructuralAuthority _ _
      | expectedRevision == Nothing -> Right ()
    _ ->
      Left
        ( PreparedLabelFactsRevisionJustificationMismatch
            expectedRevision
            justification
        )

justificationSupportsAuthority ::
  AuthorityEpoch ->
  PriorAuthorityJustification ->
  Bool
justificationSupportsAuthority _ (ExistingReleasedAuthority _) = True
justificationSupportsAuthority authority (CheckedGenesisAuthority _) =
  authority == genesisAuthorityEpoch
justificationSupportsAuthority authority (EstablishedStructuralAuthority occurrence cut) =
  authority == structuralAuthorityEpoch occurrence cut

preparedLabelFactsDecisionId :: PreparedLabelFacts -> LabelDecisionId
preparedLabelFactsDecisionId (PreparedLabelFacts decision _ _ _ _ _ _ _ _ _ _) =
  decision

preparedLabelFactsResolveIndex :: PreparedLabelFacts -> ControlIndex
preparedLabelFactsResolveIndex (PreparedLabelFacts _ index _ _ _ _ _ _ _ _ _) =
  index

preparedLabelFactsObjectId :: PreparedLabelFacts -> GlobalObjectId
preparedLabelFactsObjectId (PreparedLabelFacts _ _ object _ _ _ _ _ _ _ _) =
  object

preparedLabelFactsProposedOutcome :: PreparedLabelFacts -> ReleasedLabelState
preparedLabelFactsProposedOutcome (PreparedLabelFacts _ _ _ proposed _ _ _ _ _ _ _) =
  proposed

preparedLabelFactsExpectedPriorState ::
  PreparedLabelFacts ->
  ReleasedLabelState
preparedLabelFactsExpectedPriorState
  (PreparedLabelFacts _ _ _ _ expectedPrior _ _ _ _ _ _) = expectedPrior

preparedLabelFactsExpectedPriorRevision ::
  PreparedLabelFacts ->
  Maybe LabelRevision
preparedLabelFactsExpectedPriorRevision
  (PreparedLabelFacts _ _ _ _ _ expectedRevision _ _ _ _ _) = expectedRevision

preparedLabelFactsCatalogueDigest :: PreparedLabelFacts -> CatalogueDigest
preparedLabelFactsCatalogueDigest (PreparedLabelFacts _ _ _ _ _ _ catalogue _ _ _ _) =
  catalogue

preparedLabelFactsExpectedPriorAuthority ::
  PreparedLabelFacts ->
  Maybe AuthorityEpoch
preparedLabelFactsExpectedPriorAuthority
  (PreparedLabelFacts _ _ _ _ _ _ _ authority _ _ _) = authority

preparedLabelFactsPriorAuthorityJustification ::
  PreparedLabelFacts ->
  Maybe PriorAuthorityJustification
preparedLabelFactsPriorAuthorityJustification
  (PreparedLabelFacts _ _ _ _ _ _ _ _ justification _ _) = justification

preparedLabelFactsAuthorityDisposition ::
  PreparedLabelFacts ->
  PreparedAuthorityDisposition
preparedLabelFactsAuthorityDisposition
  (PreparedLabelFacts _ _ _ _ _ _ _ _ _ disposition _) = disposition

preparedLabelFactsMemberSetDigest :: PreparedLabelFacts -> MemberSetDigest
preparedLabelFactsMemberSetDigest
  (PreparedLabelFacts _ _ _ _ _ _ _ _ _ _ members) = members

derivePreparedLabelDigest :: PreparedLabelFacts -> PreparedLabelDigest
derivePreparedLabelDigest facts =
  digestInvariant
    "prepared label digest"
    mkPreparedLabelDigest
    (SHA256.hash (preparedLabelFactsCanonicalBytes facts))

-- | Canonical common preparation evidence embedded in Oracle transcripts.
preparedLabelFactsCanonicalBytes :: PreparedLabelFacts -> ByteString
preparedLabelFactsCanonicalBytes = Serialize.encode . preparedLabelFactsTranscript

data PreparedLabelFactsTranscript
  = PreparedLabelFactsTranscript
      ByteString
      ByteString
      Word64
      ByteString
      ReleasedLabelStateTranscript
      ReleasedLabelStateTranscript
      (Maybe Word64)
      ByteString
      (Maybe ByteString)
      (Maybe PriorAuthorityJustificationTranscript)
      PreparedAuthorityDispositionTranscript
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReleasedLabelStateTranscript
  = ReleasedLabelStateTranscript Word8 ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data PriorAuthorityJustificationTranscript
  = PriorAuthorityJustificationTranscript
      Word8
      Word64
      ByteString
      Word64
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data PreparedAuthorityDispositionTranscript
  = PreparedAuthorityDispositionTranscript Word8 ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data PreparedLabelFactsCanonicalProblem
  = PreparedLabelFactsCanonicalDecodeFailed String
  | PreparedLabelFactsCanonicalWrongDomain ByteString
  | PreparedLabelFactsCanonicalInvalidIdentity IdentityError
  | PreparedLabelFactsCanonicalUnknownReleasedStateTag Word8
  | PreparedLabelFactsCanonicalInvalidReleasedLabel ValueError
  | PreparedLabelFactsCanonicalReleasedValueIsNotLabel
  | PreparedLabelFactsCanonicalInvalidDeletedGeneration String
  | PreparedLabelFactsCanonicalInvalidRevision LabelRevisionError
  | PreparedLabelFactsCanonicalInvalidCatalogueDigest SortProfile.DigestError
  | PreparedLabelFactsCanonicalInvalidAuthority
      AuthorityEpochCanonicalProblem
  | PreparedLabelFactsCanonicalUnknownJustificationTag Word8
  | PreparedLabelFactsCanonicalInvalidInitialProjectionDigest
      Startup.DigestError
  | PreparedLabelFactsCanonicalInvalidStructuralSequence
      StructuralSequenceError
  | PreparedLabelFactsCanonicalUnknownDispositionTag Word8
  | PreparedLabelFactsCanonicalInvalidMemberSetDigest TopologyDigestError
  | PreparedLabelFactsCanonicalAdmissionFailed PreparedLabelFactsError
  | PreparedLabelFactsCanonicalNonCanonical
  deriving stock (Eq, Show)

-- | Checked inverse of 'preparedLabelFactsCanonicalBytes'.
--
-- Nested values, identities, digests, authority transcripts, and positive
-- positions are admitted by their owning constructors before the complete
-- cross-field preparation invariant is checked by 'mkPreparedLabelFacts'.
decodePreparedLabelFactsCanonicalBytes ::
  ByteString -> Either PreparedLabelFactsCanonicalProblem PreparedLabelFacts
decodePreparedLabelFactsCanonicalBytes bytes = do
  PreparedLabelFactsTranscript
    domain
    decisionBytes
    resolveIndex
    objectBytes
    proposedTranscript
    expectedPriorTranscript
    expectedRevision
    catalogueBytes
    expectedAuthorityBytes
    justificationValue
    dispositionValue
    memberSetBytes <-
    case Serialize.decode bytes of
      Left problem -> Left (PreparedLabelFactsCanonicalDecodeFailed problem)
      Right transcript -> Right transcript
  if domain == preparedLabelFactsDomain
    then Right ()
    else Left (PreparedLabelFactsCanonicalWrongDomain domain)
  decision <- decodePreparedIdentity mkLabelDecisionId decisionBytes
  object <- decodePreparedIdentity mkGlobalObjectId objectBytes
  proposed <- decodePreparedReleasedState proposedTranscript
  expectedPrior <- decodePreparedReleasedState expectedPriorTranscript
  revision <- traverse decodePreparedRevision expectedRevision
  catalogue <-
    either
      (Left . PreparedLabelFactsCanonicalInvalidCatalogueDigest)
      Right
      (mkCatalogueDigest catalogueBytes)
  expectedAuthority <-
    traverse decodePreparedAuthority expectedAuthorityBytes
  justification <-
    traverse decodePreparedJustification justificationValue
  disposition <- decodePreparedDisposition dispositionValue
  members <-
    either
      (Left . PreparedLabelFactsCanonicalInvalidMemberSetDigest)
      Right
      (mkMemberSetDigest memberSetBytes)
  facts <-
    either
      (Left . PreparedLabelFactsCanonicalAdmissionFailed)
      Right
      ( mkPreparedLabelFacts
          decision
          (controlIndex resolveIndex)
          object
          proposed
          expectedPrior
          revision
          catalogue
          expectedAuthority
          justification
          disposition
          members
      )
  if preparedLabelFactsCanonicalBytes facts == bytes
    then Right facts
    else Left PreparedLabelFactsCanonicalNonCanonical

preparedLabelFactsDomain :: ByteString
preparedLabelFactsDomain = "ECLIPS-PREPARED-LABEL-FACTS"

decodePreparedIdentity ::
  (ByteString -> Either IdentityError identity) ->
  ByteString ->
  Either PreparedLabelFactsCanonicalProblem identity
decodePreparedIdentity constructor bytes =
  either
    (Left . PreparedLabelFactsCanonicalInvalidIdentity)
    Right
    (constructor bytes)

decodePreparedReleasedState ::
  ReleasedLabelStateTranscript ->
  Either PreparedLabelFactsCanonicalProblem ReleasedLabelState
decodePreparedReleasedState (ReleasedLabelStateTranscript tag payload) =
  case tag of
    0 -> do
      value <-
        either
          (Left . PreparedLabelFactsCanonicalInvalidReleasedLabel)
          Right
          (decodeCanonicalValue payload)
      case viewValue value of
        LabelValue label -> Right (releasedLabel label)
        _ -> Left PreparedLabelFactsCanonicalReleasedValueIsNotLabel
    1 -> case Serialize.decode payload of
      Left problem -> Left (PreparedLabelFactsCanonicalInvalidDeletedGeneration problem)
      Right generation -> Right (releasedDeleted generation)
    _ -> Left (PreparedLabelFactsCanonicalUnknownReleasedStateTag tag)

decodePreparedRevision ::
  Word64 -> Either PreparedLabelFactsCanonicalProblem LabelRevision
decodePreparedRevision rawRevision =
  either
    (Left . PreparedLabelFactsCanonicalInvalidRevision)
    Right
    (mkLabelRevision (controlIndex rawRevision))

decodePreparedAuthority ::
  ByteString -> Either PreparedLabelFactsCanonicalProblem AuthorityEpoch
decodePreparedAuthority =
  either
    (Left . PreparedLabelFactsCanonicalInvalidAuthority)
    Right
    . decodeAuthorityEpochCanonicalBytes

decodePreparedJustification ::
  PriorAuthorityJustificationTranscript ->
  Either PreparedLabelFactsCanonicalProblem PriorAuthorityJustification
decodePreparedJustification
  (PriorAuthorityJustificationTranscript tag revision bytes sequenceNumber cutBytes) =
    case tag of
      0 -> existingReleasedAuthority <$> decodePreparedRevision revision
      1 -> do
        digest <-
          either
            (Left . PreparedLabelFactsCanonicalInvalidInitialProjectionDigest)
            Right
            (mkInitialProjectionDigest bytes)
        Right (checkedGenesisAuthority digest)
      2 -> do
        source <- decodePreparedIdentity mkHeraldEpoch bytes
        checkedSequence <-
          either
            (Left . PreparedLabelFactsCanonicalInvalidStructuralSequence)
            Right
            (mkStructuralSequence sequenceNumber)
        cut <- decodePreparedIdentity mkTopologyCutId cutBytes
        Right
          ( establishedStructuralAuthority
              (structuralOccurrenceId source checkedSequence)
              cut
          )
      _ -> Left (PreparedLabelFactsCanonicalUnknownJustificationTag tag)

decodePreparedDisposition ::
  PreparedAuthorityDispositionTranscript ->
  Either PreparedLabelFactsCanonicalProblem PreparedAuthorityDisposition
decodePreparedDisposition (PreparedAuthorityDispositionTranscript tag bytes) =
  case tag of
    0 -> Right noTargetAuthority
    1 -> retainPriorAuthority <$> decodePreparedAuthority bytes
    2 -> Right deriveAuthorityAtRelease
    3 -> retirePriorAuthority <$> decodePreparedAuthority bytes
    _ -> Left (PreparedLabelFactsCanonicalUnknownDispositionTag tag)

preparedLabelFactsTranscript ::
  PreparedLabelFacts ->
  PreparedLabelFactsTranscript
preparedLabelFactsTranscript facts =
  PreparedLabelFactsTranscript
    preparedLabelFactsDomain
    (labelDecisionIdBytes (preparedLabelFactsDecisionId facts))
    (controlIndexWord64 (preparedLabelFactsResolveIndex facts))
    (globalObjectIdBytes (preparedLabelFactsObjectId facts))
    (releasedStateTranscript (preparedLabelFactsProposedOutcome facts))
    (releasedStateTranscript (preparedLabelFactsExpectedPriorState facts))
    ( controlIndexWord64 . labelRevisionControlIndex
        <$> preparedLabelFactsExpectedPriorRevision facts
    )
    (catalogueDigestBytes (preparedLabelFactsCatalogueDigest facts))
    (authorityEpochCanonicalBytes <$> preparedLabelFactsExpectedPriorAuthority facts)
    (justificationTranscript <$> preparedLabelFactsPriorAuthorityJustification facts)
    (dispositionTranscript (preparedLabelFactsAuthorityDisposition facts))
    (memberSetDigestBytes (preparedLabelFactsMemberSetDigest facts))

releasedStateTranscript :: ReleasedLabelState -> ReleasedLabelStateTranscript
releasedStateTranscript state = case state of
  ReleasedLabel label ->
    ReleasedLabelStateTranscript 0 (canonicalLabelBytes label)
  ReleasedDeleted generation -> ReleasedLabelStateTranscript 1 (Serialize.encode generation)

canonicalLabelBytes :: Label -> ByteString
canonicalLabelBytes = canonicalValueByteString . canonicalValueBytes . labelValue

justificationTranscript ::
  PriorAuthorityJustification ->
  PriorAuthorityJustificationTranscript
justificationTranscript justification = case justification of
  ExistingReleasedAuthority revision ->
    PriorAuthorityJustificationTranscript
      0
      (controlIndexWord64 (labelRevisionControlIndex revision))
      ByteString.empty
      0
      ByteString.empty
  CheckedGenesisAuthority digest ->
    PriorAuthorityJustificationTranscript
      1
      0
      (initialProjectionDigestBytes digest)
      0
      ByteString.empty
  EstablishedStructuralAuthority occurrence cut ->
    PriorAuthorityJustificationTranscript
      2
      0
      (heraldEpochBytes (structuralOccurrenceSourceHeraldEpoch occurrence))
      ( structuralSequenceWord64
          (structuralOccurrenceSourceSequence occurrence)
      )
      (topologyCutIdBytes cut)

dispositionTranscript ::
  PreparedAuthorityDisposition ->
  PreparedAuthorityDispositionTranscript
dispositionTranscript disposition = case disposition of
  NoTargetAuthority ->
    PreparedAuthorityDispositionTranscript 0 ByteString.empty
  RetainPriorAuthority authority ->
    PreparedAuthorityDispositionTranscript
      1
      (authorityEpochCanonicalBytes authority)
  DeriveAuthorityAtRelease ->
    PreparedAuthorityDispositionTranscript 2 ByteString.empty
  RetirePriorAuthority authority ->
    PreparedAuthorityDispositionTranscript
      3
      (authorityEpochCanonicalBytes authority)

-- | Common prepared facts bound to the membership generation whose captured
-- reporters produced them.  Keeping this wrapper outside the current Oracle
-- transcript lets the Herald-side Step-15 composition become generation-safe
-- without partially activating the later EORC cutover.
data GenerationQualifiedPreparedLabelFacts
  = GenerationQualifiedPreparedLabelFacts
      HeraldMembershipGenerationId
      PreparedLabelFacts
  deriving stock (Eq, Show)

qualifyPreparedLabelFacts ::
  HeraldMembershipGeneration ->
  PreparedLabelFacts ->
  Either
    GenerationQualifiedLabelEvidenceProblem
    GenerationQualifiedPreparedLabelFacts
qualifyPreparedLabelFacts generation facts
  | observed == expected =
      Right
        ( GenerationQualifiedPreparedLabelFacts
            (heraldMembershipGenerationId generation)
            facts
        )
  | otherwise =
      Left (GenerationQualifiedPreparedMemberSetMismatch expected observed)
  where
    expected = heraldMembershipGenerationActiveMemberSetDigest generation
    observed = preparedLabelFactsMemberSetDigest facts

generationQualifiedPreparedLabelFactsGeneration ::
  GenerationQualifiedPreparedLabelFacts ->
  HeraldMembershipGenerationId
generationQualifiedPreparedLabelFactsGeneration
  (GenerationQualifiedPreparedLabelFacts generation _) = generation

generationQualifiedPreparedLabelFactsValue ::
  GenerationQualifiedPreparedLabelFacts ->
  PreparedLabelFacts
generationQualifiedPreparedLabelFactsValue
  (GenerationQualifiedPreparedLabelFacts _ facts) = facts

generationQualifiedPreparedLabelFactsCanonicalBytes ::
  GenerationQualifiedPreparedLabelFacts ->
  ByteString
generationQualifiedPreparedLabelFactsCanonicalBytes qualified =
  Serialize.encode
    ( GenerationQualifiedLabelEvidenceTranscript
        "ECLIPS-GENERATION-QUALIFIED-PREPARED-LABEL"
        ( heraldMembershipGenerationIdBytes
            (generationQualifiedPreparedLabelFactsGeneration qualified)
        )
        ( preparedLabelFactsCanonicalBytes
            (generationQualifiedPreparedLabelFactsValue qualified)
        )
    )

data PreparedLabelReport
  = PreparedLabelReport HeraldEpoch PreparedLabelFacts PreparedLabelDigest
  deriving stock (Eq, Show)

data PreparedLabelReportError
  = PreparedLabelReportDigestMismatch PreparedLabelDigest PreparedLabelDigest
  deriving stock (Eq, Show)

preparedLabelReport :: HeraldEpoch -> PreparedLabelFacts -> PreparedLabelReport
preparedLabelReport reporter facts =
  PreparedLabelReport reporter facts (derivePreparedLabelDigest facts)

-- | Admit an untrusted claimed digest by recomputing it from the full typed
-- facts.  Equal typed facts therefore cannot carry unequal checked evidence.
admitPreparedLabelReport ::
  HeraldEpoch ->
  PreparedLabelFacts ->
  PreparedLabelDigest ->
  Either PreparedLabelReportError PreparedLabelReport
admitPreparedLabelReport reporter facts supplied =
  let expected = derivePreparedLabelDigest facts
   in if supplied == expected
        then Right (PreparedLabelReport reporter facts supplied)
        else Left (PreparedLabelReportDigestMismatch expected supplied)

preparedLabelReportReporter :: PreparedLabelReport -> HeraldEpoch
preparedLabelReportReporter (PreparedLabelReport reporter _ _) = reporter

preparedLabelReportFacts :: PreparedLabelReport -> PreparedLabelFacts
preparedLabelReportFacts (PreparedLabelReport _ facts _) = facts

preparedLabelReportDigest :: PreparedLabelReport -> PreparedLabelDigest
preparedLabelReportDigest (PreparedLabelReport _ _ digest) = digest

digestInvariant ::
  String ->
  (ByteString -> Either LabelDigestError digest) ->
  ByteString ->
  digest
digestInvariant label constructor bytes =
  either (error . ((label <> " invariant: ") <>) . show) id (constructor bytes)
