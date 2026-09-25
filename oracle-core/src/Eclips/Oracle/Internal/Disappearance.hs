{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure disappearance leaf of the sole Oracle kernel. The enclosing Oracle
-- owns request idempotence, replicated control indexes, labels, and membership.
module Eclips.Oracle.Internal.Disappearance
  ( DisappearanceInvalidationReason (..),
    DisappearanceAbortReason (..),
    CompleteDisappearanceEvidenceDigest (..),
    DisappearanceCommand (..),
    DisappearanceProbeInvalidationView (..),
    DisappearanceProbeAbortReasonView (..),
    DisappearanceProbePhaseView (..),
    DisappearanceProbeView (..),
    DisappearanceCommandResult (..),
    DisappearanceRejection (..),
    ProjectedDisappearanceProbeHeader (..),
    DisappearanceProjectionEvent (..),
    DisappearanceContext (..),
    DisappearanceState,
    authorizedDisappearanceAbortReason,
    completeDisappearanceEvidenceDigest,
    completeDisappearanceEvidenceDigestBytes,
    admitProjectedDisappearanceProbeHeader,
    projectedDisappearanceProbeHeaderId,
    projectedDisappearanceProbeHeaderSubject,
    projectedDisappearanceProbeHeaderCoordinate,
    projectedDisappearanceProbeHeaderMembership,
    initialDisappearanceState,
    disappearanceProbe,
    disappearanceProbes,
    disappearanceRegularRetirementIndex,
    disappearanceControlledSubjectDeleted,
    applyDisappearanceCommand,
    invalidateDisappearanceForLabel,
    abortDisappearanceForMembership,
    abortDisappearanceForAdmission,
    disappearanceStateCanonicalBytes,
    encodeDisappearanceCheckpoint,
    decodeDisappearanceCheckpoint,
  ) where

import Control.Monad (unless)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize qualified as S
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Disappearance
  ( DisappearanceEvidenceClaim,
    DisappearanceEvidenceDigest,
    DisappearanceOpenResult,
    DisappearanceProbeId,
    DisappearanceProbeIdentityProblem,
    DisappearanceResolutionOutcome,
    DisappearanceSubject,
    DisappearanceSubjectMembershipCoordinate,
    DisappearanceSubjectView (..),
    aliasedDisappearanceProbe,
    decodeDisappearanceEvidenceClaimCanonicalBytes,
    decodeDisappearanceResolutionOutcomeCanonicalBytes,
    decodeDisappearanceSubjectCanonicalBytes,
    deriveDisappearanceProbeId,
    disappearanceCoordinateMemberSetDigest,
    disappearanceCoordinateMembershipGenerationId,
    disappearanceCoordinateSubjectDigest,
    disappearanceEvidenceClaimCanonicalBytes,
    disappearanceEvidenceClaimCoordinate,
    disappearanceEvidenceClaimProbeId,
    disappearanceEvidenceClaimReporter,
    disappearanceEvidenceDigestBytes,
    disappearanceProbeIdBytes,
    disappearanceResolutionControlIndex,
    disappearanceResolutionOutcomeCanonicalBytes,
    disappearanceResolutionSubject,
    disappearanceSubjectCanonicalBytes,
    disappearanceSubjectDigestBytes,
    disappearanceSubjectMembershipCoordinate,
    disappearanceSubjectView,
    mkDisappearanceEvidenceDigest,
    openedDisappearanceProbe,
    resolveDisappearanceSubject,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    SortDefinitionOccurrenceId,
    SortId,
    SystemId,
    controlIndex,
    controlIndexWord64,
    globalObjectIdBytes,
    heraldEpochBytes,
    labelDecisionIdBytes,
    mkGlobalObjectId,
    mkHeraldEpoch,
    mkLabelDecisionId,
    mkSortId,
    sortIdBytes,
  )
import Eclips.Domain.Label (LabelRevision)
import Eclips.Domain.MemberSet (memberSetDigestBytes)
import Eclips.Domain.Membership
  ( HeraldAdmissionId,
    HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    decodeHeraldAdmissionIdCanonicalBytes,
    decodeHeraldMembershipGenerationCanonicalBytes,
    heraldAdmissionControlIndex,
    heraldAdmissionIdCanonicalBytes,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationCanonicalBytes,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
    mkHeraldMembershipGenerationId,
  )
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
    resolvedRetirementOccurrenceBase,
  )
import Eclips.Public.Types.Diagnostic (renderGroupedHex)
import GHC.Generics (Generic)

data DisappearanceInvalidationReason
  = MatchingPublicationObserved
  | EvidenceContradicted
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | The sole caller-constructible abort authority in the live command family.
-- Membership supersession and admission preparation are Oracle context
-- transitions, not this command.
data DisappearanceAbortReason
  = AuthorizedDisappearanceAbort
  deriving stock (Eq, Ord, Show)

authorizedDisappearanceAbortReason :: DisappearanceAbortReason
authorizedDisappearanceAbortReason = AuthorizedDisappearanceAbort

newtype CompleteDisappearanceEvidenceDigest
  = CompleteDisappearanceEvidenceDigest ByteString
  deriving stock (Eq, Ord)

instance Show CompleteDisappearanceEvidenceDigest where
  show = renderGroupedHex . completeDisappearanceEvidenceDigestBytes

completeDisappearanceEvidenceDigest ::
  [DisappearanceEvidenceClaim] -> CompleteDisappearanceEvidenceDigest
completeDisappearanceEvidenceDigest claims =
  CompleteDisappearanceEvidenceDigest
    ( SHA256.hash
        ( build
            ( frame completeEvidenceDomain
                <> counted
                  (frame . disappearanceEvidenceClaimCanonicalBytes)
                  ( sortOn
                      ( \claim ->
                          ( disappearanceEvidenceClaimReporter claim,
                            disappearanceEvidenceClaimCanonicalBytes claim
                          )
                      )
                      claims
                  )
            )
        )
    )

completeDisappearanceEvidenceDigestBytes ::
  CompleteDisappearanceEvidenceDigest -> ByteString
completeDisappearanceEvidenceDigestBytes (CompleteDisappearanceEvidenceDigest bytes) = bytes

data DisappearanceCommand
  = DisappearanceOpen
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
  | DisappearanceReport DisappearanceProbeId DisappearanceEvidenceClaim
  | DisappearanceInvalidate
      DisappearanceProbeId
      HeraldEpoch
      DisappearanceInvalidationReason
      DisappearanceEvidenceDigest
  | DisappearanceResolve DisappearanceProbeId CompleteDisappearanceEvidenceDigest
  | DisappearanceAbort DisappearanceProbeId DisappearanceAbortReason
  deriving stock (Eq, Show)

data DisappearanceProbeInvalidationView
  = CommandDisappearanceInvalidation
      HeraldEpoch
      DisappearanceInvalidationReason
      DisappearanceEvidenceDigest
  | LabelOpenDisappearanceInvalidation LabelDecisionId
  deriving stock (Eq, Show)

data DisappearanceProbeAbortReasonView
  = ExplicitDisappearanceAbortReasonView DisappearanceAbortReason
  | MembershipSupersededDisappearanceAbortView HeraldMembershipGenerationId
  | AdmissionPreparingDisappearanceAbortView HeraldAdmissionId
  deriving stock (Eq, Show)

data DisappearanceProbePhase
  = DisappearanceCollecting
  | DisappearanceInvalidated DisappearanceProbeInvalidationView ControlIndex
  | DisappearanceResolved DisappearanceResolutionOutcome ControlIndex
  | DisappearanceAbortedPhase DisappearanceAbortReason ControlIndex
  | DisappearanceMembershipSuperseded HeraldMembershipGenerationId ControlIndex
  | DisappearanceAdmissionPreparingPhase HeraldAdmissionId ControlIndex
  deriving stock (Eq, Show)

data DisappearanceProbePhaseView
  = DisappearanceCollectingView
  | DisappearanceInvalidatedView DisappearanceProbeInvalidationView ControlIndex
  | DisappearanceResolvedView DisappearanceResolutionOutcome ControlIndex
  | DisappearanceAbortedView DisappearanceAbortReason ControlIndex
  | DisappearanceMembershipSupersededView HeraldMembershipGenerationId ControlIndex
  | DisappearanceAdmissionPreparingView HeraldAdmissionId ControlIndex
  deriving stock (Eq, Show)

data DisappearanceProbeRecord
  = DisappearanceProbeRecord
      DisappearanceProbeId
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
      HeraldMembershipGeneration
      [HeraldEpoch]
      ControlIndex
      (Map HeraldEpoch DisappearanceEvidenceClaim)
      DisappearanceProbePhase
  deriving stock (Eq, Show)

data DisappearanceProbeView
  = DisappearanceProbeView
      DisappearanceProbeId
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
      HeraldMembershipGenerationId
      [HeraldEpoch]
      ControlIndex
      [(HeraldEpoch, DisappearanceEvidenceClaim)]
      DisappearanceProbePhaseView
  deriving stock (Eq, Show)

data DisappearanceCommandResult
  = DisappearanceOpenAccepted DisappearanceOpenResult
  | DisappearanceReportAccepted DisappearanceProbeId HeraldEpoch
  | DisappearanceInvalidateAccepted DisappearanceProbeId
  | DisappearanceResolveAccepted DisappearanceResolutionOutcome
  | DisappearanceAbortAccepted DisappearanceProbeId
  deriving stock (Eq, Show)

data DisappearanceRejection
  = DisappearanceAdmissionPreparing HeraldAdmissionId
  | DisappearanceSubjectMembershipCoordinateMismatch
      DisappearanceSubjectMembershipCoordinate
      DisappearanceSubjectMembershipCoordinate
  | DisappearanceControlledRevisionMismatch
      GlobalObjectId
      (Maybe LabelRevision)
      (Maybe LabelRevision)
  | DisappearanceControlledSubjectDeleted GlobalObjectId
  | DisappearanceLabelWorkflowInProgress GlobalObjectId LabelDecisionId
  | DisappearanceStaleRegularOccurrence
      SortId
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | DisappearanceUnknownProbe DisappearanceProbeId
  | DisappearanceProbeAlreadyTerminal DisappearanceProbeId DisappearanceProbePhaseView
  | DisappearanceReporterHomeMismatch HeraldEpoch HeraldEpoch
  | DisappearanceReporterNotCaptured DisappearanceProbeId HeraldEpoch
  | DisappearanceEvidenceProbeMismatch DisappearanceProbeId DisappearanceProbeId
  | DisappearanceEvidenceCoordinateMismatch
      DisappearanceSubjectMembershipCoordinate
      DisappearanceSubjectMembershipCoordinate
  | DisappearanceConflictingReport
      DisappearanceProbeId
      HeraldEpoch
      DisappearanceEvidenceClaim
      DisappearanceEvidenceClaim
  | DisappearanceIncompleteEvidence DisappearanceProbeId [HeraldEpoch]
  | DisappearanceCompleteEvidenceMismatch
      DisappearanceProbeId
      CompleteDisappearanceEvidenceDigest
      CompleteDisappearanceEvidenceDigest
  deriving stock (Eq, Show)

data ProjectedDisappearanceProbeHeader
  = ProjectedDisappearanceProbeHeader
      DisappearanceProbeId
      DisappearanceSubject
      DisappearanceSubjectMembershipCoordinate
      HeraldMembershipGeneration
  deriving stock (Eq, Show)

-- | Assemble nominal portable header facts without inventing an Open event.
-- Membership/cut authority belongs to the receiving projection; the derived
-- identity and coordinate cannot disagree with the supplied checked leaves.
admitProjectedDisappearanceProbeHeader ::
  ControlIndex ->
  DisappearanceSubject ->
  HeraldMembershipGeneration ->
  Either DisappearanceProbeIdentityProblem ProjectedDisappearanceProbeHeader
admitProjectedDisappearanceProbeHeader opened subject membership = do
  probe <- deriveDisappearanceProbeId opened
  pure (ProjectedDisappearanceProbeHeader probe subject (disappearanceSubjectMembershipCoordinate subject membership) membership)

projectedDisappearanceProbeHeaderId :: ProjectedDisappearanceProbeHeader -> DisappearanceProbeId
projectedDisappearanceProbeHeaderId (ProjectedDisappearanceProbeHeader probe _ _ _) = probe

projectedDisappearanceProbeHeaderSubject :: ProjectedDisappearanceProbeHeader -> DisappearanceSubject
projectedDisappearanceProbeHeaderSubject (ProjectedDisappearanceProbeHeader _ subject _ _) = subject

projectedDisappearanceProbeHeaderCoordinate ::
  ProjectedDisappearanceProbeHeader -> DisappearanceSubjectMembershipCoordinate
projectedDisappearanceProbeHeaderCoordinate (ProjectedDisappearanceProbeHeader _ _ coordinate _) = coordinate

projectedDisappearanceProbeHeaderMembership ::
  ProjectedDisappearanceProbeHeader -> HeraldMembershipGeneration
projectedDisappearanceProbeHeaderMembership (ProjectedDisappearanceProbeHeader _ _ _ membership) = membership

data DisappearanceProjectionEvent
  = DisappearanceOpenProjected ProjectedDisappearanceProbeHeader DisappearanceOpenResult
  | DisappearanceReportProjected DisappearanceEvidenceClaim
  | DisappearanceInvalidatedProjected
      DisappearanceProbeId
      DisappearanceProbeInvalidationView
  | DisappearanceResolvedProjected
      DisappearanceProbeId
      DisappearanceResolutionOutcome
  | DisappearanceAbortedProjected
      DisappearanceProbeId
      DisappearanceProbeAbortReasonView
  deriving stock (Eq, Show)

data DisappearanceContext = DisappearanceContext
  { contextSystem :: SystemId,
    contextMembership :: HeraldMembershipGeneration,
    contextLabelRevisions :: Map GlobalObjectId (Maybe LabelRevision),
    contextLabels :: Map GlobalObjectId LabelDecisionId,
    contextDeleted :: Set.Set GlobalObjectId
  }

data DisappearanceState = DisappearanceState
  { stateActive :: Map DisappearanceSubjectMembershipCoordinate DisappearanceProbeId,
    stateProbes :: Map DisappearanceProbeId DisappearanceProbeRecord,
    stateActiveControlled :: !(Map GlobalObjectId (Set.Set DisappearanceProbeId)),
    stateRetirements :: Map SortId ControlIndex,
    stateDeleted :: Set.Set GlobalObjectId
  }
  deriving stock (Eq, Show)

initialDisappearanceState :: DisappearanceState
initialDisappearanceState = DisappearanceState Map.empty Map.empty Map.empty Map.empty Set.empty

setActive :: Map DisappearanceSubjectMembershipCoordinate DisappearanceProbeId -> DisappearanceState -> DisappearanceState
setActive value state = state {stateActive = value}
setProbes :: Map DisappearanceProbeId DisappearanceProbeRecord -> DisappearanceState -> DisappearanceState
setProbes value state = state {stateProbes = value}
setRetirements :: Map SortId ControlIndex -> DisappearanceState -> DisappearanceState
setRetirements value state = state {stateRetirements = value}

disappearanceProbe :: DisappearanceProbeId -> DisappearanceState -> Maybe DisappearanceProbeView
disappearanceProbe probe state = probeView <$> Map.lookup probe (stateProbes state)
disappearanceProbes :: DisappearanceState -> [DisappearanceProbeView]
disappearanceProbes = fmap probeView . Map.elems . stateProbes
disappearanceRegularRetirementIndex :: SortId -> DisappearanceState -> Maybe ControlIndex
disappearanceRegularRetirementIndex sort = Map.lookup sort . stateRetirements
disappearanceControlledSubjectDeleted :: GlobalObjectId -> DisappearanceState -> Bool
disappearanceControlledSubjectDeleted object = Set.member object . stateDeleted

applyDisappearanceCommand :: DisappearanceContext -> ControlIndex -> HeraldEpoch -> DisappearanceCommand -> DisappearanceState -> Either DisappearanceRejection (DisappearanceCommandResult, DisappearanceState, [DisappearanceProjectionEvent])
applyDisappearanceCommand context index home command state = case command of
  DisappearanceOpen subject coordinate -> applyOpen context index subject coordinate state
  DisappearanceReport probe claim -> applyReport home probe claim state
  DisappearanceInvalidate probe reporter reason witness -> applyInvalidate index home probe reporter reason witness state
  DisappearanceResolve probe evidence -> applyResolve context index home probe evidence state
  DisappearanceAbort probe reason -> applyAbort index home probe reason state

invalidateDisappearanceForLabel :: ControlIndex -> GlobalObjectId -> LabelDecisionId -> DisappearanceState -> (DisappearanceState, [DisappearanceProjectionEvent])
invalidateDisappearanceForLabel index object decision state =
  let affected = [activeProbe ident state | ident <- Set.toAscList (Map.findWithDefault Set.empty object (stateActiveControlled state))]
      reason = LabelOpenDisappearanceInvalidation decision
   in ( foldl (\updated record -> terminalize (probeIdentifier record) (DisappearanceInvalidated reason index) updated) state affected,
        fmap (\record -> DisappearanceInvalidatedProjected (probeIdentifier record) reason) affected
      )

abortDisappearanceForMembership :: ControlIndex -> HeraldMembershipGenerationId -> DisappearanceState -> (DisappearanceState, [DisappearanceProjectionEvent])
abortDisappearanceForMembership index successor state =
  let affected = sortOn probeIdentifier $ filter (\record -> disappearanceCoordinateMembershipGenerationId (probeCoordinate record) /= successor) (fmap (`activeProbe` state) (Map.elems (stateActive state)))
   in ( foldl (\updated record -> terminalize (probeIdentifier record) (DisappearanceMembershipSuperseded successor index) updated) state affected,
        fmap (\record -> DisappearanceAbortedProjected (probeIdentifier record) (MembershipSupersededDisappearanceAbortView successor)) affected
      )

-- | Beginning admission closes the old membership's disappearance work without
-- advancing its generation. Retain the admission cause independently from a
-- true membership supersession so terminal evidence keeps its original meaning.
abortDisappearanceForAdmission :: ControlIndex -> HeraldAdmissionId -> DisappearanceState -> (DisappearanceState, [DisappearanceProjectionEvent])
abortDisappearanceForAdmission index admission state =
  let affected = sortOn probeIdentifier (fmap (`activeProbe` state) (Map.elems (stateActive state)))
   in ( foldl (\updated record -> terminalize (probeIdentifier record) (DisappearanceAdmissionPreparingPhase admission index) updated) state affected,
        fmap (\record -> DisappearanceAbortedProjected (probeIdentifier record) (AdmissionPreparingDisappearanceAbortView admission)) affected
      )

applyOpen ::
  DisappearanceContext ->
  ControlIndex ->
  DisappearanceSubject ->
  DisappearanceSubjectMembershipCoordinate ->
  DisappearanceState ->
  Either
    DisappearanceRejection
    (DisappearanceCommandResult, DisappearanceState, [DisappearanceProjectionEvent])
applyOpen context index subject suppliedCoordinate state = do
  let membership = contextMembership context
      coordinate = disappearanceSubjectMembershipCoordinate subject membership
  if suppliedCoordinate == coordinate
    then Right ()
    else
      Left
        ( DisappearanceSubjectMembershipCoordinateMismatch
            suppliedCoordinate
            coordinate
        )
  validateSubjectCurrent context subject state
  case Map.lookup coordinate (stateActive state) of
    Just existing ->
      let record = case Map.lookup existing (stateProbes state) of
            Nothing -> error "active disappearance probe missing from retained history"
            Just retained -> retained
          result = aliasedDisappearanceProbe existing
          successor = state
       in Right
            ( DisappearanceOpenAccepted result,
              successor,
              [DisappearanceOpenProjected (projectedDisappearanceProbeHeader record) result]
            )
    Nothing -> do
      let probe = probeAt index
          members = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership)
          record =
            DisappearanceProbeRecord
              probe
              subject
              coordinate
              membership
              members
              index
              Map.empty
              DisappearanceCollecting
          result = openedDisappearanceProbe probe
          successor =
            insertActiveControlled record
              . setProbes (Map.insert probe record (stateProbes state))
              . setActive (Map.insert coordinate probe (stateActive state))
              $ state
      Right
        ( DisappearanceOpenAccepted result,
          successor,
          [DisappearanceOpenProjected (projectedDisappearanceProbeHeader record) result]
        )

validateSubjectCurrent ::
  DisappearanceContext -> DisappearanceSubject -> DisappearanceState -> Either DisappearanceRejection ()
validateSubjectCurrent context subject state = case disappearanceSubjectView subject of
  ControlledPredefinedSubjectView object _ suppliedRevision -> do
    case if object `Set.member` stateDeleted state || object `Set.member` contextDeleted context then Just () else Nothing of
      Just () ->
        Left (DisappearanceControlledSubjectDeleted object)
      _ -> Right ()
    case Map.lookup object (contextLabels context) of
      Nothing -> Right ()
      Just decision -> Left (DisappearanceLabelWorkflowInProgress object decision)
    let retainedRevision = Map.findWithDefault Nothing object (contextLabelRevisions context)
    if suppliedRevision == retainedRevision
      then Right ()
      else
        Left
          ( DisappearanceControlledRevisionMismatch
              object
              suppliedRevision
              retainedRevision
          )
  RegularSortDefinitionSubjectView sortId _ suppliedOccurrence ->
    let expectedOccurrence = expectedRegularOccurrence context sortId state
     in if suppliedOccurrence == expectedOccurrence
          then Right ()
          else
            Left
              ( DisappearanceStaleRegularOccurrence
                  sortId
                  suppliedOccurrence
                  expectedOccurrence
              )

applyReport ::
  HeraldEpoch ->
  DisappearanceProbeId ->
  DisappearanceEvidenceClaim ->
  DisappearanceState ->
  Either
    DisappearanceRejection
    (DisappearanceCommandResult, DisappearanceState, [DisappearanceProjectionEvent])
applyReport home probe claim state = do
  let reporter = disappearanceEvidenceClaimReporter claim
  record <- collectingProbe probe state
  if home == reporter
    then Right ()
    else Left (DisappearanceReporterHomeMismatch home reporter)
  validateReporter probe reporter record
  if disappearanceEvidenceClaimProbeId claim == probeIdentifier record
    then Right ()
    else
      Left
        ( DisappearanceEvidenceProbeMismatch
            (disappearanceEvidenceClaimProbeId claim)
            (probeIdentifier record)
        )
  if disappearanceEvidenceClaimCoordinate claim == probeCoordinate record
    then Right ()
    else
      Left
        ( DisappearanceEvidenceCoordinateMismatch
            (disappearanceEvidenceClaimCoordinate claim)
            (probeCoordinate record)
        )
  case Map.lookup reporter (probeReports record) of
    Just retained
      | retained == claim ->
          Right (DisappearanceReportAccepted probe reporter, state, [])
      | otherwise ->
          Left (DisappearanceConflictingReport probe reporter retained claim)
    Nothing ->
      let updated = setProbeReports (Map.insert reporter claim (probeReports record)) record
          successor = setProbes (Map.insert probe updated (stateProbes state)) state
       in Right
            ( DisappearanceReportAccepted probe reporter,
              successor,
              [DisappearanceReportProjected claim]
            )

applyInvalidate ::
  ControlIndex ->
  HeraldEpoch ->
  DisappearanceProbeId ->
  HeraldEpoch ->
  DisappearanceInvalidationReason ->
  DisappearanceEvidenceDigest ->
  DisappearanceState ->
  Either
    DisappearanceRejection
    (DisappearanceCommandResult, DisappearanceState, [DisappearanceProjectionEvent])
applyInvalidate index home probe reporter reason witness state = do
  record <- lookupProbeRecord probe state
  if home == reporter
    then Right ()
    else Left (DisappearanceReporterHomeMismatch home reporter)
  let cause = CommandDisappearanceInvalidation reporter reason witness
  case probePhase record of
    DisappearanceInvalidated retained _
      | retained == cause ->
          Right (DisappearanceInvalidateAccepted probe, state, [])
    DisappearanceCollecting -> do
      validateReporter probe reporter record
      let successor = terminalize probe (DisappearanceInvalidated cause index) state
      Right
        ( DisappearanceInvalidateAccepted probe,
          successor,
          [DisappearanceInvalidatedProjected probe cause]
        )
    terminal ->
      Left (DisappearanceProbeAlreadyTerminal probe (phaseView terminal))

applyResolve ::
  DisappearanceContext ->
  ControlIndex ->
  HeraldEpoch ->
  DisappearanceProbeId ->
  CompleteDisappearanceEvidenceDigest ->
  DisappearanceState ->
  Either
    DisappearanceRejection
    (DisappearanceCommandResult, DisappearanceState, [DisappearanceProjectionEvent])
applyResolve context index home probe suppliedDigest state = do
  record <- lookupProbeRecord probe state
  let expectedDigest = completeDisappearanceEvidenceDigest (Map.elems (probeReports record))
  case probePhase record of
    DisappearanceResolved retained _
      | suppliedDigest == expectedDigest ->
          Right (DisappearanceResolveAccepted retained, state, [])
    DisappearanceCollecting -> do
      validateReporter probe home record
      validateSubjectCurrent context (probeSubject record) state
      let missing = filter (`Map.notMember` probeReports record) (probeMembers record)
      if null missing
        then Right ()
        else Left (DisappearanceIncompleteEvidence probe missing)
      if suppliedDigest == expectedDigest
        then Right ()
        else Left (DisappearanceCompleteEvidenceMismatch probe suppliedDigest expectedDigest)
      outcome <-
        either
          (error . ("positive disappearance resolve index rejected: " <>) . show)
          Right
          (resolveDisappearanceSubject (contextSystem context) index (probeSubject record))
      let resolved = terminalize probe (DisappearanceResolved outcome index) state
          successor = applyResolutionOutcome outcome resolved
      Right
        ( DisappearanceResolveAccepted outcome,
          successor,
          [DisappearanceResolvedProjected probe outcome]
        )
    terminal ->
      Left (DisappearanceProbeAlreadyTerminal probe (phaseView terminal))

applyAbort ::
  ControlIndex ->
  HeraldEpoch ->
  DisappearanceProbeId ->
  DisappearanceAbortReason ->
  DisappearanceState ->
  Either
    DisappearanceRejection
    (DisappearanceCommandResult, DisappearanceState, [DisappearanceProjectionEvent])
applyAbort index home probe reason state = do
  record <- lookupProbeRecord probe state
  case probePhase record of
    DisappearanceAbortedPhase retained _
      | retained == reason ->
          Right (DisappearanceAbortAccepted probe, state, [])
    DisappearanceCollecting -> do
      validateReporter probe home record
      let successor = terminalize probe (DisappearanceAbortedPhase reason index) state
      Right
        ( DisappearanceAbortAccepted probe,
          successor,
          [DisappearanceAbortedProjected probe (ExplicitDisappearanceAbortReasonView reason)]
        )
    terminal ->
      Left (DisappearanceProbeAlreadyTerminal probe (phaseView terminal))

probeView :: DisappearanceProbeRecord -> DisappearanceProbeView
probeView record =
  DisappearanceProbeView
    (probeIdentifier record)
    (probeSubject record)
    (probeCoordinate record)
    (heraldMembershipGenerationId (probeMembership record))
    (probeMembers record)
    (probeOpenedAt record)
    (Map.toAscList (probeReports record))
    (phaseView (probePhase record))

phaseView :: DisappearanceProbePhase -> DisappearanceProbePhaseView
phaseView phase = case phase of
  DisappearanceCollecting -> DisappearanceCollectingView
  DisappearanceInvalidated reason index -> DisappearanceInvalidatedView reason index
  DisappearanceResolved outcome index -> DisappearanceResolvedView outcome index
  DisappearanceAbortedPhase reason index -> DisappearanceAbortedView reason index
  DisappearanceMembershipSuperseded generation index ->
    DisappearanceMembershipSupersededView generation index
  DisappearanceAdmissionPreparingPhase admission index ->
    DisappearanceAdmissionPreparingView admission index

collectingProbe ::
  DisappearanceProbeId ->
  DisappearanceState ->
  Either DisappearanceRejection DisappearanceProbeRecord
collectingProbe probe state = do
  record <- lookupProbeRecord probe state
  case probePhase record of
    DisappearanceCollecting -> Right record
    terminal -> Left (DisappearanceProbeAlreadyTerminal probe (phaseView terminal))

lookupProbeRecord ::
  DisappearanceProbeId ->
  DisappearanceState ->
  Either DisappearanceRejection DisappearanceProbeRecord
lookupProbeRecord probe state =
  maybe (Left (DisappearanceUnknownProbe probe)) Right (Map.lookup probe (stateProbes state))

validateReporter ::
  DisappearanceProbeId ->
  HeraldEpoch ->
  DisappearanceProbeRecord ->
  Either DisappearanceRejection ()
validateReporter probe reporter record
  | reporter `elem` probeMembers record = Right ()
  | otherwise = Left (DisappearanceReporterNotCaptured probe reporter)

terminalize ::
  DisappearanceProbeId -> DisappearanceProbePhase -> DisappearanceState -> DisappearanceState
terminalize probe phase state = case Map.lookup probe (stateProbes state) of
  Nothing -> state
  Just record ->
    deleteActiveControlled record
      . setProbes
        (Map.insert probe (setProbePhase phase record) (stateProbes state))
      . setActive
        ( Map.update
            (\retained -> if retained == probe then Nothing else Just retained)
            (probeCoordinate record)
            (stateActive state)
        )
      $ state

-- The active indices exclude terminal probes. Label decisions never traverse
-- the retained disappearance transcript when invalidating one object's proof.
activeProbe :: DisappearanceProbeId -> DisappearanceState -> DisappearanceProbeRecord
activeProbe ident state = case Map.lookup ident (stateProbes state) of
  Just record -> record
  Nothing -> error "active disappearance index references absent probe"

insertActiveControlled :: DisappearanceProbeRecord -> DisappearanceState -> DisappearanceState
insertActiveControlled record state = case controlledObject (probeSubject record) of
  Nothing -> state
  Just object -> state {stateActiveControlled = Map.insertWith Set.union object (Set.singleton (probeIdentifier record)) (stateActiveControlled state)}

deleteActiveControlled :: DisappearanceProbeRecord -> DisappearanceState -> DisappearanceState
deleteActiveControlled record state = case controlledObject (probeSubject record) of
  Nothing -> state
  Just object -> state {stateActiveControlled = Map.update remove object (stateActiveControlled state)}
  where
    remove identifiers = let remaining = Set.delete (probeIdentifier record) identifiers in if Set.null remaining then Nothing else Just remaining

applyResolutionOutcome ::
  DisappearanceResolutionOutcome -> DisappearanceState -> DisappearanceState
applyResolutionOutcome outcome state =
  case disappearanceSubjectView (resolutionSubject outcome) of
    ControlledPredefinedSubjectView object _ _ ->
      state {stateDeleted = Set.insert object (stateDeleted state)}
    RegularSortDefinitionSubjectView sortId _ _ ->
      setRetirements
        (Map.insert sortId (resolutionIndex outcome) (stateRetirements state))
        state

resolutionSubject :: DisappearanceResolutionOutcome -> DisappearanceSubject
resolutionSubject = disappearanceResolutionSubject

resolutionIndex :: DisappearanceResolutionOutcome -> ControlIndex
resolutionIndex = disappearanceResolutionControlIndex

expectedRegularOccurrence :: DisappearanceContext -> SortId -> DisappearanceState -> SortDefinitionOccurrenceId
expectedRegularOccurrence context sortId state =
  deriveSortDefinitionOccurrenceId (contextSystem context) sortId base
  where
    base = case Map.lookup sortId (stateRetirements state) of
      Nothing -> Genesis
      Just retirement ->
        either
          (error . ("positive retirement index rejected: " <>) . show)
          id
          (resolvedRetirementOccurrenceBase retirement)

controlledObject :: DisappearanceSubject -> Maybe GlobalObjectId
controlledObject subject = case disappearanceSubjectView subject of
  ControlledPredefinedSubjectView object _ _ -> Just object
  RegularSortDefinitionSubjectView {} -> Nothing

probeAt :: ControlIndex -> DisappearanceProbeId
probeAt =
  either
    (error . ("positive disappearance Open index rejected: " <>) . show)
    id
    . deriveDisappearanceProbeId

probeIdentifier :: DisappearanceProbeRecord -> DisappearanceProbeId
probeIdentifier (DisappearanceProbeRecord probe _ _ _ _ _ _ _) = probe

probeSubject :: DisappearanceProbeRecord -> DisappearanceSubject
probeSubject (DisappearanceProbeRecord _ subject _ _ _ _ _ _) = subject

probeCoordinate :: DisappearanceProbeRecord -> DisappearanceSubjectMembershipCoordinate
probeCoordinate (DisappearanceProbeRecord _ _ coordinate _ _ _ _ _) = coordinate

probeMembership :: DisappearanceProbeRecord -> HeraldMembershipGeneration
probeMembership (DisappearanceProbeRecord _ _ _ membership _ _ _ _) = membership

projectedDisappearanceProbeHeader :: DisappearanceProbeRecord -> ProjectedDisappearanceProbeHeader
projectedDisappearanceProbeHeader record =
  ProjectedDisappearanceProbeHeader
    (probeIdentifier record)
    (probeSubject record)
    (probeCoordinate record)
    (probeMembership record)

probeMembers :: DisappearanceProbeRecord -> [HeraldEpoch]
probeMembers (DisappearanceProbeRecord _ _ _ _ members _ _ _) = members

probeOpenedAt :: DisappearanceProbeRecord -> ControlIndex
probeOpenedAt (DisappearanceProbeRecord _ _ _ _ _ opened _ _) = opened

probeReports :: DisappearanceProbeRecord -> Map HeraldEpoch DisappearanceEvidenceClaim
probeReports (DisappearanceProbeRecord _ _ _ _ _ _ reports _) = reports

probePhase :: DisappearanceProbeRecord -> DisappearanceProbePhase
probePhase (DisappearanceProbeRecord _ _ _ _ _ _ _ phase) = phase

setProbeReports ::
  Map HeraldEpoch DisappearanceEvidenceClaim -> DisappearanceProbeRecord -> DisappearanceProbeRecord
setProbeReports value (DisappearanceProbeRecord a b c d e f _ h) =
  DisappearanceProbeRecord a b c d e f value h

setProbePhase :: DisappearanceProbePhase -> DisappearanceProbeRecord -> DisappearanceProbeRecord
setProbePhase value (DisappearanceProbeRecord a b c d e f g _) =
  DisappearanceProbeRecord a b c d e f g value
completeEvidenceDomain :: ByteString
completeEvidenceDomain = "ECLIPS-DISAPPEARANCE-COMPLETE-EVIDENCE"
putControlIndex :: ControlIndex -> Builder.Builder
putControlIndex = Builder.word64BE . controlIndexWord64
putHeraldList :: [HeraldEpoch] -> Builder.Builder
putHeraldList = counted (Builder.byteString . heraldEpochBytes)
putProbe :: DisappearanceProbeId -> Builder.Builder
putProbe = Builder.byteString . disappearanceProbeIdBytes

putCoordinate :: DisappearanceSubjectMembershipCoordinate -> Builder.Builder
putCoordinate coordinate =
  -- The checked evidence transcript already fixes the coordinate encoding.  A
  -- synthetic claim is neither needed nor constructible here, so use its three
  -- public fixed-width leaves.
  Builder.byteString
    ( disappearanceSubjectDigestBytes
        (disappearanceCoordinateSubjectDigest coordinate)
    )
    <> Builder.byteString
      ( heraldMembershipGenerationIdBytes
          (disappearanceCoordinateMembershipGenerationId coordinate)
      )
    <> Builder.byteString
      ( memberSetDigestBytes
          (disappearanceCoordinateMemberSetDigest coordinate)
      )

putInvalidationView :: DisappearanceProbeInvalidationView -> Builder.Builder
putInvalidationView cause = case cause of
  CommandDisappearanceInvalidation reporter reason witness ->
    Builder.word8 0
      <> Builder.byteString (heraldEpochBytes reporter)
      <> Builder.word8 (fromIntegral (fromEnum reason))
      <> Builder.byteString (disappearanceEvidenceDigestBytes witness)
  LabelOpenDisappearanceInvalidation decision ->
    Builder.word8 1 <> Builder.byteString (labelDecisionIdBytes decision)

putAbortReasonView :: DisappearanceProbeAbortReasonView -> Builder.Builder
putAbortReasonView reason = case reason of
  ExplicitDisappearanceAbortReasonView authorized ->
    Builder.word8 0 <> Builder.word8 (disappearanceAbortReasonTag authorized)
  MembershipSupersededDisappearanceAbortView generation ->
    Builder.word8 1
      <> Builder.byteString (heraldMembershipGenerationIdBytes generation)
  AdmissionPreparingDisappearanceAbortView admission ->
    Builder.word8 2 <> frame (heraldAdmissionIdCanonicalBytes admission)

disappearanceAbortReasonTag :: DisappearanceAbortReason -> Word8
disappearanceAbortReasonTag AuthorizedDisappearanceAbort = 0

putPhaseView :: DisappearanceProbePhaseView -> Builder.Builder
putPhaseView phase = case phase of
  DisappearanceCollectingView -> Builder.word8 0 <> frame ByteString.empty
  DisappearanceInvalidatedView reason index ->
    Builder.word8 1
      <> frame (build (putInvalidationView reason <> putControlIndex index))
  DisappearanceResolvedView outcome index ->
    Builder.word8 2
      <> frame
        ( build
            ( putControlIndex index
                <> frame (disappearanceResolutionOutcomeCanonicalBytes outcome)
            )
        )
  DisappearanceAbortedView reason index ->
    Builder.word8 3
      <> frame
        ( build
            ( putAbortReasonView (ExplicitDisappearanceAbortReasonView reason)
                <> putControlIndex index
            )
        )
  DisappearanceMembershipSupersededView generation index ->
    Builder.word8 3
      <> frame
        ( build
            ( putAbortReasonView
                (MembershipSupersededDisappearanceAbortView generation)
                <> putControlIndex index
            )
        )
  DisappearanceAdmissionPreparingView admission index ->
    Builder.word8 3
      <> frame
        ( build
            ( putAbortReasonView (AdmissionPreparingDisappearanceAbortView admission)
                <> putControlIndex index
            )
        )

putProbeRecord :: DisappearanceProbeRecord -> Builder.Builder
putProbeRecord record =
  putProbe (probeIdentifier record)
    <> frame (disappearanceSubjectCanonicalBytes (probeSubject record))
    <> putCoordinate (probeCoordinate record)
    <> putControlIndex (probeOpenedAt record)
    <> putHeraldList (probeMembers record)
    <> countedFramed
      putReport
      (Map.toAscList (probeReports record))
    <> putPhaseView (phaseView (probePhase record))
  where
    putReport (reporter, claim) =
      Builder.byteString (heraldEpochBytes reporter)
        <> frame (disappearanceEvidenceClaimCanonicalBytes claim)

putRetirement :: (SortId, ControlIndex) -> Builder.Builder
putRetirement (sortId, index) =
  Builder.byteString (sortIdBytes sortId) <> putControlIndex index

build :: Builder.Builder -> ByteString
build = LazyByteString.toStrict . Builder.toLazyByteString

-- Positional state/probe updates keep constructors private without record-update
-- escape hatches.

disappearanceStateCanonicalBytes :: DisappearanceState -> ByteString
disappearanceStateCanonicalBytes state =
  build
    ( frame "ECLIPS-DISAPPEARANCE-STATE"
        <> countedFramed (putProbeRecord . snd) (Map.toAscList (stateProbes state))
        <> countedFramed putRetirement (Map.toAscList (stateRetirements state))
        <> counted (Builder.byteString . globalObjectIdBytes) (Set.toAscList (stateDeleted state))
    )

-- Captured membership is part of a live probe's authority. Keep its checked
-- generation in the checkpoint, rather than reconstructing it from today's
-- membership or the projection's member list.
data CheckpointDisappearanceProbe = CheckpointDisappearanceProbe ByteString ByteString Word64 [ByteString] CheckpointDisappearancePhase
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

data CheckpointDisappearancePhase
  = CheckpointDisappearanceCollecting
  | CheckpointDisappearanceCommandInvalidation ByteString Word8 ByteString Word64
  | CheckpointDisappearanceLabelInvalidation ByteString Word64
  | CheckpointDisappearanceResolved ByteString Word64
  | CheckpointDisappearanceAborted Word64
  | CheckpointDisappearanceSuperseded ByteString Word64
  | CheckpointDisappearanceAdmissionPreparing ByteString Word64
  deriving stock (Generic)
  deriving anyclass (S.Serialize)

encodeDisappearanceCheckpoint :: DisappearanceState -> ByteString
encodeDisappearanceCheckpoint state =
  S.encode
    ( fmap encodeProbe (Map.elems (stateProbes state)),
      fmap (\(sort, index) -> (sortIdBytes sort, controlIndexWord64 index)) (Map.toAscList (stateRetirements state)),
      fmap globalObjectIdBytes (Set.toAscList (stateDeleted state))
    )
  where
    encodeProbe record =
      CheckpointDisappearanceProbe
        (disappearanceSubjectCanonicalBytes (probeSubject record))
        (heraldMembershipGenerationCanonicalBytes (probeMembership record))
        (controlIndexWord64 (probeOpenedAt record))
        (fmap disappearanceEvidenceClaimCanonicalBytes (Map.elems (probeReports record)))
        (encodePhase (probePhase record))
    encodePhase phase = case phase of
      DisappearanceCollecting -> CheckpointDisappearanceCollecting
      DisappearanceInvalidated reason index -> case reason of
        CommandDisappearanceInvalidation reporter cause witness -> CheckpointDisappearanceCommandInvalidation (heraldEpochBytes reporter) (fromIntegral (fromEnum cause)) (disappearanceEvidenceDigestBytes witness) (controlIndexWord64 index)
        LabelOpenDisappearanceInvalidation decision -> CheckpointDisappearanceLabelInvalidation (labelDecisionIdBytes decision) (controlIndexWord64 index)
      DisappearanceResolved outcome index -> CheckpointDisappearanceResolved (disappearanceResolutionOutcomeCanonicalBytes outcome) (controlIndexWord64 index)
      DisappearanceAbortedPhase AuthorizedDisappearanceAbort index -> CheckpointDisappearanceAborted (controlIndexWord64 index)
      DisappearanceMembershipSuperseded generation index -> CheckpointDisappearanceSuperseded (heraldMembershipGenerationIdBytes generation) (controlIndexWord64 index)
      DisappearanceAdmissionPreparingPhase admission index -> CheckpointDisappearanceAdmissionPreparing (heraldAdmissionIdCanonicalBytes admission) (controlIndexWord64 index)

decodeDisappearanceCheckpoint :: ByteString -> Either String DisappearanceState
decodeDisappearanceCheckpoint bytes = do
  (rawProbes, rawRetirements, rawDeleted) <- S.decode bytes
  probes <- traverse decodeProbe rawProbes
  retirements <- traverse (\(sort, index) -> (,) <$> admit (mkSortId sort) <*> pure (controlIndex index)) rawRetirements
  deleted <- traverse (admit . mkGlobalObjectId) rawDeleted
  let live = [(probeCoordinate probe, probeIdentifier probe) | probe <- probes, probePhase probe == DisappearanceCollecting]
      state = foldl (flip insertActiveControlled) (DisappearanceState (Map.fromList live) (Map.fromList [(probeIdentifier probe, probe) | probe <- probes]) Map.empty (Map.fromList retirements) (Set.fromList deleted)) [probe | probe <- probes, probePhase probe == DisappearanceCollecting]
      outcomes = [outcome | probe <- probes, DisappearanceResolved outcome _ <- [probePhase probe]]
      resolved = foldl (flip applyResolutionOutcome) initialDisappearanceState (sortOn disappearanceResolutionControlIndex outcomes)
  unless
    ( Map.size (stateActive state) == length live
        && stateRetirements state == stateRetirements resolved
        && stateDeleted state == stateDeleted resolved
        && encodeDisappearanceCheckpoint state == bytes
    )
    (Left "invalid disappearance checkpoint")
  pure state
  where
    admit :: (Show problem) => Either problem value -> Either String value
    admit = either (Left . show) Right
    decodeProbe (CheckpointDisappearanceProbe rawSubject rawMembership rawOpened rawReports rawPhase) = do
      let opened = controlIndex rawOpened
      identifier <- admit (deriveDisappearanceProbeId opened)
      subject <- decodeDisappearanceSubjectCanonicalBytes rawSubject
      membership <- admit (decodeHeraldMembershipGenerationCanonicalBytes rawMembership)
      reports <- traverse (decodeDisappearanceEvidenceClaimCanonicalBytes opened) rawReports
      phase <- decodePhase rawPhase
      let coordinate = disappearanceSubjectMembershipCoordinate subject membership
          members = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs membership)
          byReporter = Map.fromList [(disappearanceEvidenceClaimReporter claim, claim) | claim <- reports]
          reportValid claim =
            disappearanceEvidenceClaimProbeId claim == identifier
              && disappearanceEvidenceClaimCoordinate claim == coordinate
              && disappearanceEvidenceClaimReporter claim `elem` members
          phaseValid = case phase of
            DisappearanceCollecting -> True
            DisappearanceInvalidated reason index ->
              opened < index && case reason of
                CommandDisappearanceInvalidation reporter _ _ -> reporter `elem` members
                LabelOpenDisappearanceInvalidation _ -> controlledObject subject /= Nothing
            DisappearanceResolved outcome index ->
              opened < index
                && disappearanceResolutionControlIndex outcome == index
                && disappearanceResolutionSubject outcome == subject
                && Map.keys byReporter == members
            DisappearanceAbortedPhase _ index -> opened < index
            DisappearanceMembershipSuperseded generation index -> opened < index && generation /= heraldMembershipGenerationId membership
            DisappearanceAdmissionPreparingPhase admission index -> opened < index && heraldAdmissionControlIndex admission == index
      unless (all reportValid reports && phaseValid) (Left "invalid disappearance checkpoint probe")
      pure (DisappearanceProbeRecord identifier subject coordinate membership members opened byReporter phase)
    decodePhase phase = case phase of
      CheckpointDisappearanceCollecting -> Right DisappearanceCollecting
      CheckpointDisappearanceCommandInvalidation rawReporter rawReason rawWitness index -> do
        reporter <- admit (mkHeraldEpoch rawReporter)
        reason <- case rawReason of
          0 -> Right MatchingPublicationObserved
          1 -> Right EvidenceContradicted
          _ -> Left "invalid disappearance checkpoint invalidation reason"
        witness <- admit (mkDisappearanceEvidenceDigest rawWitness)
        pure (DisappearanceInvalidated (CommandDisappearanceInvalidation reporter reason witness) (controlIndex index))
      CheckpointDisappearanceLabelInvalidation decision index -> DisappearanceInvalidated <$> (LabelOpenDisappearanceInvalidation <$> admit (mkLabelDecisionId decision)) <*> pure (controlIndex index)
      CheckpointDisappearanceResolved outcome index -> DisappearanceResolved <$> decodeDisappearanceResolutionOutcomeCanonicalBytes outcome <*> pure (controlIndex index)
      CheckpointDisappearanceAborted index -> Right (DisappearanceAbortedPhase AuthorizedDisappearanceAbort (controlIndex index))
      CheckpointDisappearanceSuperseded generation index -> DisappearanceMembershipSuperseded <$> admit (mkHeraldMembershipGenerationId generation) <*> pure (controlIndex index)
      CheckpointDisappearanceAdmissionPreparing admission index -> DisappearanceAdmissionPreparingPhase <$> admit (decodeHeraldAdmissionIdCanonicalBytes (ByteString.copy admission)) <*> pure (controlIndex index)

frame :: ByteString -> Builder.Builder
frame bytes = Builder.word64BE (fromIntegral (ByteString.length bytes)) <> Builder.byteString bytes
counted :: (a -> Builder.Builder) -> [a] -> Builder.Builder
counted encode values = Builder.word64BE (fromIntegral (length values)) <> foldMap encode values
countedFramed :: (a -> Builder.Builder) -> [a] -> Builder.Builder
countedFramed encode = counted (frame . build . encode)
