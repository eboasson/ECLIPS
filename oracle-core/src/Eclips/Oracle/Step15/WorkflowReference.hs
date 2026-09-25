{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Detached reference model for Step-15 membership-sensitive label workflow
-- consequences.  This module deliberately does not extend the live Oracle
-- command sum, EORC schema, or live canonical state.
module Eclips.Oracle.Step15.WorkflowReference
  ( ReferenceWorkflowState,
    initialReferenceWorkflowState,
    ReferenceWorkflowProblem (..),
    decideReferenceWorkflow,
    decideReferenceWorkflowAt,
    applyReferenceWorkflowRetirement,
    ReferenceWorkflowPhaseView (..),
    ReferenceWorkflowTerminalView (..),
    referenceWorkflowPhase,
    referenceWorkflowCapturedGeneration,
    referenceWorkflowCapturedMembers,
    referenceWorkflowCapturedMemberSetDigest,
    referenceWorkflowApplicableGeneration,
    referenceWorkflowApplicableReporters,
    referenceWorkflowCollector,
    referenceWorkflowTerminal,
    referenceWorkflowCurrentMembership,
    ReferenceWorkflowCompletion,
    referenceWorkflowCompletion,
    ReferenceWorkflowReportResult (..),
    ReferenceWorkflowReportProtocolProblem (..),
    ReferenceWorkflowReportSubmission (..),
    submitReferenceWorkflowCompletion,
    referenceWorkflowStateCanonicalBytes,
  )
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Serialize (Serialize)
import Data.Serialize qualified as Serialize
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64, Word8)
import Eclips.Domain.Identity
  ( ControlIndex,
    HeraldEpoch,
    LabelDecisionId,
    controlIndexWord64,
    heraldEpochBytes,
    labelDecisionIdBytes,
  )
import Eclips.Domain.Label
  ( LabelOutcomeDigest,
    labelOutcomeDigestBytes,
    mkLabelOutcomeDigest,
  )
import Eclips.Domain.MemberSet (MemberSetDigest, memberSetDigestBytes)
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    HeraldMembershipGenerationId,
    heraldMembershipGenerationActiveHeraldEpochs,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationCanonicalBytes,
    heraldMembershipGenerationId,
    heraldMembershipGenerationIdBytes,
    heraldMembershipGenerationPredecessor,
    heraldMembershipGenerationRetiredHeraldEpoch,
    heraldMembershipGenerationRetirementControlIndex,
    heraldMembershipGenerationRetirementId,
    retireHeraldMembershipGeneration,
  )
import Eclips.Oracle.Identity
  ( OracleClientRequestId,
    oracleClientRequestHome,
    oracleClientRequestSequence,
  )
import GHC.Generics (Generic)

data ReferenceWorkflowPhase
  = ReferenceWorkflowTerminal
      ReferenceWorkflowTerminal
      Bool
  deriving stock (Eq, Show)

data ReferenceWorkflowTerminal
  = ReferenceWorkflowOtherNotApplied
      ControlIndex
      ByteString
      LabelOutcomeDigest
  | ReferenceWorkflowReleased
      ControlIndex
      ByteString
      LabelOutcomeDigest
  deriving stock (Eq, Show)

data ReferenceWorkflow
  = ReferenceWorkflow
      LabelDecisionId
      HeraldMembershipGenerationId
      [HeraldEpoch]
      MemberSetDigest
      HeraldMembershipGenerationId
      [HeraldEpoch]
      HeraldEpoch
      ReferenceWorkflowPhase
  deriving stock (Eq, Show)

data ReferenceWorkflowCompletion
  = ReferenceWorkflowCompletion
      OracleClientRequestId
      HeraldEpoch
      LabelDecisionId
      HeraldMembershipGenerationId
      LabelOutcomeDigest
  deriving stock (Eq, Show)

data ReferenceWorkflowReportResult
  = ReferenceWorkflowReportAccepted
  | ReferenceWorkflowReportRejected ReferenceWorkflowProblem
  deriving stock (Eq, Show)

data ReferenceWorkflowRequestRecord
  = ReferenceWorkflowRequestRecord
      ReferenceWorkflowCompletion
      ReferenceWorkflowReportResult
  deriving stock (Eq, Show)

data ReferenceWorkflowState
  = ReferenceWorkflowState
      (Map HeraldMembershipGenerationId HeraldMembershipGeneration)
      HeraldMembershipGeneration
      (Maybe ReferenceWorkflow)
      (Map OracleClientRequestId ReferenceWorkflowRequestRecord)
  deriving stock (Eq, Show)

data ReferenceWorkflowProblem
  = ReferenceWorkflowInitialGenerationNotGenesis HeraldMembershipGenerationId
  | ReferenceWorkflowAlreadyOpen LabelDecisionId
  | ReferenceWorkflowUnknown
  | ReferenceWorkflowDecisionMismatch LabelDecisionId LabelDecisionId
  | ReferenceWorkflowStaleMembershipGeneration
      HeraldMembershipGenerationId
      HeraldMembershipGenerationId
  | ReferenceWorkflowWrongPhase ReferenceWorkflowPhaseView
  | ReferenceWorkflowControlIndexZero
  | ReferenceWorkflowRetirementMissingPredecessor
  | ReferenceWorkflowRetirementMalformed
  | ReferenceWorkflowReportHomeMismatch HeraldEpoch HeraldEpoch
  | ReferenceWorkflowReporterInactive HeraldEpoch
  | ReferenceWorkflowReporterNotApplicable HeraldEpoch
  | ReferenceWorkflowReportDigestMismatch LabelOutcomeDigest LabelOutcomeDigest
  | ReferenceWorkflowCollectorMismatch HeraldEpoch HeraldEpoch
  deriving stock (Eq, Show)

data ReferenceWorkflowReportProtocolProblem
  = ReferenceWorkflowConflictingRequestId OracleClientRequestId
  deriving stock (Eq, Show)

data ReferenceWorkflowReportSubmission
  = ReferenceWorkflowReportCommitted ReferenceWorkflowReportResult
  | ReferenceWorkflowReportDuplicate ReferenceWorkflowReportResult
  | ReferenceWorkflowReportProtocolRejected ReferenceWorkflowReportProtocolProblem
  deriving stock (Eq, Show)

data ReferenceWorkflowPhaseView
  = ReferenceWorkflowReleasedView
  | ReferenceWorkflowCompletedNotAppliedView
  | ReferenceWorkflowCompletedReleasedView
  deriving stock (Eq, Ord, Show)

data ReferenceWorkflowTerminalView
  = ReferenceWorkflowOtherNotAppliedView
      ControlIndex
      ByteString
      LabelOutcomeDigest
  | ReferenceWorkflowReleasedValueView
      ControlIndex
      ByteString
      LabelOutcomeDigest
  deriving stock (Eq, Show)

initialReferenceWorkflowState ::
  HeraldMembershipGeneration ->
  Either ReferenceWorkflowProblem ReferenceWorkflowState
initialReferenceWorkflowState generation =
  case heraldMembershipGenerationPredecessor generation of
    Nothing ->
      Right
        ( ReferenceWorkflowState
            (Map.singleton (heraldMembershipGenerationId generation) generation)
            generation
            Nothing
            Map.empty
        )
    Just _ ->
      Left
        ( ReferenceWorkflowInitialGenerationNotGenesis
            (heraldMembershipGenerationId generation)
        )

-- | One canonical decision fixes either failure or the value requiring local
-- installation. The model never admits an intermediate global phase. Left is
-- NotApplied and has no installation collection; Right is Applied.
decideReferenceWorkflow ::
  LabelDecisionId ->
  HeraldMembershipGenerationId ->
  ControlIndex ->
  Either ByteString ByteString ->
  ReferenceWorkflowState ->
  Either ReferenceWorkflowProblem ReferenceWorkflowState
decideReferenceWorkflow decision supplied index outcome state@(ReferenceWorkflowState _ current _ _) =
  decideReferenceWorkflowAt (NonEmpty.head (heraldMembershipGenerationActiveHeraldEpochs current)) decision supplied index outcome state

decideReferenceWorkflowAt ::
  HeraldEpoch ->
  LabelDecisionId ->
  HeraldMembershipGenerationId ->
  ControlIndex ->
  Either ByteString ByteString ->
  ReferenceWorkflowState ->
  Either ReferenceWorkflowProblem ReferenceWorkflowState
decideReferenceWorkflowAt home decision supplied index outcome (ReferenceWorkflowState history current workflow requests) = do
  requirePositive index
  case workflow of
    Nothing -> Right ()
    Just retained -> case workflowPhase retained of
      ReferenceWorkflowTerminal _ True -> Right ()
      _ -> Left (ReferenceWorkflowAlreadyOpen (workflowDecision retained))
  let expected = heraldMembershipGenerationId current
  if supplied == expected
    then Right ()
    else Left (ReferenceWorkflowStaleMembershipGeneration supplied expected)
  let members = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs current)
  if home `elem` members
    then Right ()
    else Left (ReferenceWorkflowReporterInactive home)
  let (terminal, completed) = case outcome of
        Left reason -> (ReferenceWorkflowOtherNotApplied index reason (terminalDigest decision supplied 1 index reason), True)
        Right value -> (ReferenceWorkflowReleased index value (terminalDigest decision supplied 2 index value), False)
      decided =
        ReferenceWorkflow
          decision
          supplied
          members
          (heraldMembershipGenerationActiveMemberSetDigest current)
          supplied
          members
          home
          (ReferenceWorkflowTerminal terminal completed)
  Right (ReferenceWorkflowState history current (Just decided) requests)

applyReferenceWorkflowRetirement ::
  HeraldMembershipGeneration ->
  ReferenceWorkflowState ->
  Either ReferenceWorkflowProblem ReferenceWorkflowState
applyReferenceWorkflowRetirement successor state@(ReferenceWorkflowState history current workflow requests)
  | Map.lookup (heraldMembershipGenerationId successor) history == Just successor,
    heraldMembershipGenerationPredecessor successor /= Nothing =
      Right state
  | otherwise = do
      predecessor <-
        maybe
          (Left ReferenceWorkflowRetirementMissingPredecessor)
          Right
          (heraldMembershipGenerationPredecessor successor)
      if predecessor == heraldMembershipGenerationId current
        then Right ()
        else Left ReferenceWorkflowRetirementMalformed
      index <-
        maybe
          (Left ReferenceWorkflowRetirementMalformed)
          Right
          (heraldMembershipGenerationRetirementControlIndex successor)
      target <-
        maybe
          (Left ReferenceWorkflowRetirementMalformed)
          Right
          (heraldMembershipGenerationRetiredHeraldEpoch successor)
      retirement <-
        maybe
          (Left ReferenceWorkflowRetirementMalformed)
          Right
          (heraldMembershipGenerationRetirementId successor)
      expected <-
        either
          (const (Left ReferenceWorkflowRetirementMalformed))
          Right
          (retireHeraldMembershipGeneration index retirement target current)
      if expected == successor
        then Right ()
        else Left ReferenceWorkflowRetirementMalformed
      let successorId = heraldMembershipGenerationId successor
          successorMembers = NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs successor)
          updatedWorkflow = fmap (applyRetirementToWorkflow successorId successorMembers) workflow
      Right
        ( ReferenceWorkflowState
            (Map.insert successorId successor history)
            successor
            updatedWorkflow
            requests
        )

applyRetirementToWorkflow ::
  HeraldMembershipGenerationId -> [HeraldEpoch] -> ReferenceWorkflow -> ReferenceWorkflow
applyRetirementToWorkflow successor members =
  completeIfCovered . setWorkflowApplicable successor members

referenceWorkflowCompletion ::
  OracleClientRequestId ->
  HeraldEpoch ->
  LabelDecisionId ->
  HeraldMembershipGenerationId ->
  LabelOutcomeDigest ->
  ReferenceWorkflowCompletion
referenceWorkflowCompletion = ReferenceWorkflowCompletion

submitReferenceWorkflowCompletion ::
  ReferenceWorkflowCompletion ->
  ReferenceWorkflowState ->
  (ReferenceWorkflowState, ReferenceWorkflowReportSubmission)
submitReferenceWorkflowCompletion report state@(ReferenceWorkflowState history current workflow requests) =
  case Map.lookup request requests of
    Just (ReferenceWorkflowRequestRecord retained result)
      | retained == report -> (state, ReferenceWorkflowReportDuplicate result)
      | otherwise ->
          ( state,
            ReferenceWorkflowReportProtocolRejected
              (ReferenceWorkflowConflictingRequestId request)
          )
    Nothing ->
      let (updatedWorkflow, result) = applyFreshReport report current workflow
          successor =
            ReferenceWorkflowState
              history
              current
              updatedWorkflow
              (Map.insert request (ReferenceWorkflowRequestRecord report result) requests)
       in (successor, ReferenceWorkflowReportCommitted result)
  where
    request = reportRequestId report

applyFreshReport ::
  ReferenceWorkflowCompletion ->
  HeraldMembershipGeneration ->
  Maybe ReferenceWorkflow ->
  (Maybe ReferenceWorkflow, ReferenceWorkflowReportResult)
applyFreshReport report current workflow = case validateFreshReport report current workflow of
  Left problem -> (workflow, ReferenceWorkflowReportRejected problem)
  Right retained ->
    let updated = case workflowPhase retained of
          ReferenceWorkflowTerminal terminal _ -> setWorkflowPhase (ReferenceWorkflowTerminal terminal True) retained
     in (Just updated, ReferenceWorkflowReportAccepted)

validateFreshReport ::
  ReferenceWorkflowCompletion ->
  HeraldMembershipGeneration ->
  Maybe ReferenceWorkflow ->
  Either ReferenceWorkflowProblem ReferenceWorkflow
validateFreshReport report current maybeWorkflow = do
  workflow <- maybe (Left ReferenceWorkflowUnknown) Right maybeWorkflow
  let requestHome = oracleClientRequestHome (reportRequestId report)
      reporter = reportReporter report
  if requestHome == reporter
    then Right ()
    else Left (ReferenceWorkflowReportHomeMismatch requestHome reporter)
  if reporter `Set.member` activeMembers current
    then Right ()
    else Left (ReferenceWorkflowReporterInactive reporter)
  if reportDecision report == workflowDecision workflow
    then Right ()
    else Left (ReferenceWorkflowDecisionMismatch (workflowDecision workflow) (reportDecision report))
  if reporter `elem` workflowCapturedMembers workflow
    then Right ()
    else Left (ReferenceWorkflowReporterNotApplicable reporter)
  (terminal, complete) <- case workflowPhase workflow of
    ReferenceWorkflowTerminal retained complete -> Right (retained, complete)
  let expected = terminalOutcomeDigest terminal
  if reportDigest report == expected
    then Right ()
    else Left (ReferenceWorkflowReportDigestMismatch expected (reportDigest report))
  if complete
    then Right workflow
    else do
      if reportGeneration report == workflowApplicableGeneration workflow
        then Right ()
        else Left (ReferenceWorkflowStaleMembershipGeneration (reportGeneration report) (workflowApplicableGeneration workflow))
      collector <- maybe (Left ReferenceWorkflowUnknown) Right (workflowCollector workflow)
      if reporter == collector
        then Right workflow
        else Left (ReferenceWorkflowCollectorMismatch collector reporter)

referenceWorkflowPhase :: ReferenceWorkflowState -> Maybe ReferenceWorkflowPhaseView
referenceWorkflowPhase = fmap (phaseView . workflowPhase) . stateWorkflow

referenceWorkflowCapturedGeneration :: ReferenceWorkflowState -> Maybe HeraldMembershipGenerationId
referenceWorkflowCapturedGeneration = fmap workflowCapturedGeneration . stateWorkflow

referenceWorkflowCapturedMembers :: ReferenceWorkflowState -> [HeraldEpoch]
referenceWorkflowCapturedMembers = maybe [] workflowCapturedMembers . stateWorkflow

referenceWorkflowCapturedMemberSetDigest :: ReferenceWorkflowState -> Maybe MemberSetDigest
referenceWorkflowCapturedMemberSetDigest = fmap workflowCapturedDigest . stateWorkflow

referenceWorkflowApplicableGeneration :: ReferenceWorkflowState -> Maybe HeraldMembershipGenerationId
referenceWorkflowApplicableGeneration = fmap workflowApplicableGeneration . stateWorkflow

referenceWorkflowApplicableReporters :: ReferenceWorkflowState -> [HeraldEpoch]
referenceWorkflowApplicableReporters = maybe [] workflowApplicableMembers . stateWorkflow

referenceWorkflowCollector :: ReferenceWorkflowState -> Maybe HeraldEpoch
referenceWorkflowCollector state = stateWorkflow state >>= workflowCollector

workflowCollector :: ReferenceWorkflow -> Maybe HeraldEpoch
workflowCollector workflow
  | workflowHome workflow `elem` survivors = Just (workflowHome workflow)
  | otherwise = case survivors of [] -> Nothing; _ -> Just (minimum survivors)
  where
    survivors = workflowApplicableMembers workflow

workflowHome :: ReferenceWorkflow -> HeraldEpoch
workflowHome (ReferenceWorkflow _ _ _ _ _ _ home _) = home

referenceWorkflowTerminal :: ReferenceWorkflowState -> Maybe ReferenceWorkflowTerminalView
referenceWorkflowTerminal state = do
  workflow <- stateWorkflow state
  case workflowPhase workflow of
    ReferenceWorkflowTerminal terminal _ -> Just (terminalView terminal)

referenceWorkflowCurrentMembership :: ReferenceWorkflowState -> HeraldMembershipGeneration
referenceWorkflowCurrentMembership (ReferenceWorkflowState _ current _ _) = current

referenceWorkflowStateCanonicalBytes :: ReferenceWorkflowState -> ByteString
referenceWorkflowStateCanonicalBytes (ReferenceWorkflowState history current workflow requests) =
  Serialize.encode
    ( ReferenceWorkflowStateTranscript
        "ECLIPS-STEP15-WORKFLOW-REFERENCE-STATE"
        (heraldMembershipGenerationCanonicalBytes <$> membershipLineage history current)
        (heraldMembershipGenerationIdBytes (heraldMembershipGenerationId current))
        (workflowTranscript <$> workflow)
        (requestTranscript <$> Map.elems requests)
    )

completeIfCovered :: ReferenceWorkflow -> ReferenceWorkflow
completeIfCovered workflow = case workflowPhase workflow of
  ReferenceWorkflowTerminal terminal complete ->
    setWorkflowPhase (ReferenceWorkflowTerminal terminal (complete || null (workflowApplicableMembers workflow))) workflow

phaseView :: ReferenceWorkflowPhase -> ReferenceWorkflowPhaseView
phaseView phase = case phase of
  ReferenceWorkflowTerminal terminal completed -> case (terminal, completed) of
    (ReferenceWorkflowReleased {}, False) -> ReferenceWorkflowReleasedView
    (ReferenceWorkflowReleased {}, True) -> ReferenceWorkflowCompletedReleasedView
    (_, False) -> error "NotApplied has no installation collection"
    (_, True) -> ReferenceWorkflowCompletedNotAppliedView

terminalView :: ReferenceWorkflowTerminal -> ReferenceWorkflowTerminalView
terminalView terminal = case terminal of
  ReferenceWorkflowOtherNotApplied index reason digest ->
    ReferenceWorkflowOtherNotAppliedView index reason digest
  ReferenceWorkflowReleased index value digest ->
    ReferenceWorkflowReleasedValueView index value digest

terminalOutcomeDigest :: ReferenceWorkflowTerminal -> LabelOutcomeDigest
terminalOutcomeDigest terminal = case terminal of
  ReferenceWorkflowOtherNotApplied _ _ digest -> digest
  ReferenceWorkflowReleased _ _ digest -> digest

terminalDigest ::
  LabelDecisionId ->
  HeraldMembershipGenerationId ->
  Word8 ->
  ControlIndex ->
  ByteString ->
  LabelOutcomeDigest
terminalDigest decision captured tag index payload =
  either (error . ("workflow terminal digest invariant: " <>) . show) id
    . mkLabelOutcomeDigest
    . SHA256.hash
    . Serialize.encode
    $ ReferenceWorkflowTerminalDigestTranscript
      "ECLIPS-STEP15-WORKFLOW-TERMINAL"
      (labelDecisionIdBytes decision)
      (heraldMembershipGenerationIdBytes captured)
      tag
      (controlIndexWord64 index)
      payload

requirePositive :: ControlIndex -> Either ReferenceWorkflowProblem ()
requirePositive index
  | controlIndexWord64 index == 0 = Left ReferenceWorkflowControlIndexZero
  | otherwise = Right ()

activeMembers :: HeraldMembershipGeneration -> Set HeraldEpoch
activeMembers = Set.fromList . NonEmpty.toList . heraldMembershipGenerationActiveHeraldEpochs

workflowDecision :: ReferenceWorkflow -> LabelDecisionId
workflowDecision (ReferenceWorkflow decision _ _ _ _ _ _ _) = decision

workflowCapturedGeneration :: ReferenceWorkflow -> HeraldMembershipGenerationId
workflowCapturedGeneration (ReferenceWorkflow _ generation _ _ _ _ _ _) = generation

workflowCapturedMembers :: ReferenceWorkflow -> [HeraldEpoch]
workflowCapturedMembers (ReferenceWorkflow _ _ members _ _ _ _ _) = members

workflowCapturedDigest :: ReferenceWorkflow -> MemberSetDigest
workflowCapturedDigest (ReferenceWorkflow _ _ _ digest _ _ _ _) = digest

workflowApplicableGeneration :: ReferenceWorkflow -> HeraldMembershipGenerationId
workflowApplicableGeneration (ReferenceWorkflow _ _ _ _ generation _ _ _) = generation

workflowApplicableMembers :: ReferenceWorkflow -> [HeraldEpoch]
workflowApplicableMembers (ReferenceWorkflow _ _ _ _ _ members _ _) = members

workflowPhase :: ReferenceWorkflow -> ReferenceWorkflowPhase
workflowPhase (ReferenceWorkflow _ _ _ _ _ _ _ phase) = phase

setWorkflowPhase :: ReferenceWorkflowPhase -> ReferenceWorkflow -> ReferenceWorkflow
setWorkflowPhase phase (ReferenceWorkflow decision captured members digest applicable applicableMembers home _) =
  ReferenceWorkflow decision captured members digest applicable applicableMembers home phase

setWorkflowApplicable ::
  HeraldMembershipGenerationId -> [HeraldEpoch] -> ReferenceWorkflow -> ReferenceWorkflow
setWorkflowApplicable generation applicableMembers (ReferenceWorkflow decision captured members digest _ _ home phase) =
  ReferenceWorkflow decision captured members digest generation (filter (`elem` members) applicableMembers) home phase

stateWorkflow :: ReferenceWorkflowState -> Maybe ReferenceWorkflow
stateWorkflow (ReferenceWorkflowState _ _ workflow _) = workflow

reportRequestId :: ReferenceWorkflowCompletion -> OracleClientRequestId
reportRequestId (ReferenceWorkflowCompletion request _ _ _ _) = request

reportReporter :: ReferenceWorkflowCompletion -> HeraldEpoch
reportReporter (ReferenceWorkflowCompletion _ reporter _ _ _) = reporter

reportDecision :: ReferenceWorkflowCompletion -> LabelDecisionId
reportDecision (ReferenceWorkflowCompletion _ _ decision _ _) = decision

reportGeneration :: ReferenceWorkflowCompletion -> HeraldMembershipGenerationId
reportGeneration (ReferenceWorkflowCompletion _ _ _ generation _) = generation

reportDigest :: ReferenceWorkflowCompletion -> LabelOutcomeDigest
reportDigest (ReferenceWorkflowCompletion _ _ _ _ digest) = digest

workflowTranscript :: ReferenceWorkflow -> ReferenceWorkflowTranscript
workflowTranscript workflow =
  ReferenceWorkflowTranscript
    (labelDecisionIdBytes (workflowDecision workflow))
    (heraldMembershipGenerationIdBytes (workflowCapturedGeneration workflow))
    (heraldEpochBytes <$> workflowCapturedMembers workflow)
    (memberSetDigestBytes (workflowCapturedDigest workflow))
    (heraldMembershipGenerationIdBytes (workflowApplicableGeneration workflow))
    (heraldEpochBytes <$> workflowApplicableMembers workflow)
    (heraldEpochBytes (workflowHome workflow))
    (phaseTranscript (workflowPhase workflow))

phaseTranscript :: ReferenceWorkflowPhase -> ReferenceWorkflowPhaseTranscript
phaseTranscript phase = case phase of
  ReferenceWorkflowTerminal terminal completed ->
    ReferenceWorkflowPhaseTranscript
      2
      (Just (terminalTranscript terminal))
      []
      completed

terminalTranscript :: ReferenceWorkflowTerminal -> ReferenceWorkflowTerminalTranscript
terminalTranscript terminal = case terminal of
  ReferenceWorkflowOtherNotApplied index reason digest ->
    ReferenceWorkflowTerminalTranscript 1 (controlIndexWord64 index) reason (labelOutcomeDigestBytes digest)
  ReferenceWorkflowReleased index value digest ->
    ReferenceWorkflowTerminalTranscript 2 (controlIndexWord64 index) value (labelOutcomeDigestBytes digest)

requestTranscript :: ReferenceWorkflowRequestRecord -> ReferenceWorkflowRequestTranscript
requestTranscript (ReferenceWorkflowRequestRecord report result) =
  ReferenceWorkflowRequestTranscript (reportTranscript report) (reportResultTranscript result)

reportTranscript :: ReferenceWorkflowCompletion -> ReferenceWorkflowReportTranscript
reportTranscript report =
  ReferenceWorkflowReportTranscript
    (heraldEpochBytes (oracleClientRequestHome (reportRequestId report)))
    (oracleClientRequestSequence (reportRequestId report))
    (heraldEpochBytes (reportReporter report))
    (labelDecisionIdBytes (reportDecision report))
    (heraldMembershipGenerationIdBytes (reportGeneration report))
    (labelOutcomeDigestBytes (reportDigest report))

reportResultTranscript :: ReferenceWorkflowReportResult -> ReferenceWorkflowReportResultTranscript
reportResultTranscript result = case result of
  ReferenceWorkflowReportAccepted -> ReferenceWorkflowReportResultTranscript 0 ByteString.empty
  ReferenceWorkflowReportRejected problem ->
    ReferenceWorkflowReportResultTranscript 1 (Serialize.encode (problemTranscript problem))

problemTranscript :: ReferenceWorkflowProblem -> ReferenceWorkflowProblemTranscript
problemTranscript problem = case problem of
  ReferenceWorkflowInitialGenerationNotGenesis generation ->
    transcript 0 [heraldMembershipGenerationIdBytes generation]
  ReferenceWorkflowAlreadyOpen decision -> transcript 1 [labelDecisionIdBytes decision]
  ReferenceWorkflowUnknown -> transcript 2 []
  ReferenceWorkflowDecisionMismatch expected actual ->
    transcript 3 [labelDecisionIdBytes expected, labelDecisionIdBytes actual]
  ReferenceWorkflowStaleMembershipGeneration supplied expected ->
    transcript
      4
      [ heraldMembershipGenerationIdBytes supplied,
        heraldMembershipGenerationIdBytes expected
      ]
  ReferenceWorkflowWrongPhase phase -> transcript 5 [ByteString.singleton (phaseViewTag phase)]
  ReferenceWorkflowControlIndexZero -> transcript 6 []
  ReferenceWorkflowRetirementMissingPredecessor -> transcript 7 []
  ReferenceWorkflowRetirementMalformed -> transcript 8 []
  ReferenceWorkflowReportHomeMismatch home reporter ->
    transcript 9 [heraldEpochBytes home, heraldEpochBytes reporter]
  ReferenceWorkflowReporterInactive reporter -> transcript 10 [heraldEpochBytes reporter]
  ReferenceWorkflowReporterNotApplicable reporter -> transcript 11 [heraldEpochBytes reporter]
  ReferenceWorkflowReportDigestMismatch expected actual ->
    transcript 12 [labelOutcomeDigestBytes expected, labelOutcomeDigestBytes actual]
  ReferenceWorkflowCollectorMismatch expected supplied -> transcript 13 [heraldEpochBytes expected, heraldEpochBytes supplied]
  where
    transcript = ReferenceWorkflowProblemTranscript

phaseViewTag :: ReferenceWorkflowPhaseView -> Word8
phaseViewTag phase = case phase of
  ReferenceWorkflowReleasedView -> 3
  ReferenceWorkflowCompletedNotAppliedView -> 4
  ReferenceWorkflowCompletedReleasedView -> 5

membershipLineage ::
  Map HeraldMembershipGenerationId HeraldMembershipGeneration ->
  HeraldMembershipGeneration ->
  [HeraldMembershipGeneration]
membershipLineage history = reverse . go
  where
    go generation =
      generation : case heraldMembershipGenerationPredecessor generation of
        Nothing -> []
        Just predecessor ->
          case Map.lookup predecessor history of
            Just previous -> go previous
            Nothing -> error "workflow reference membership-history invariant"

data ReferenceWorkflowStateTranscript
  = ReferenceWorkflowStateTranscript
      ByteString
      [ByteString]
      ByteString
      (Maybe ReferenceWorkflowTranscript)
      [ReferenceWorkflowRequestTranscript]
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceWorkflowTranscript
  = ReferenceWorkflowTranscript
      ByteString
      ByteString
      [ByteString]
      ByteString
      ByteString
      [ByteString]
      ByteString
      ReferenceWorkflowPhaseTranscript
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceWorkflowPhaseTranscript
  = ReferenceWorkflowPhaseTranscript
      Word8
      (Maybe ReferenceWorkflowTerminalTranscript)
      [ReferenceWorkflowReportTranscript]
      Bool
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceWorkflowTerminalTranscript
  = ReferenceWorkflowTerminalTranscript
      Word8
      Word64
      ByteString
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceWorkflowRequestTranscript
  = ReferenceWorkflowRequestTranscript
      ReferenceWorkflowReportTranscript
      ReferenceWorkflowReportResultTranscript
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceWorkflowReportTranscript
  = ReferenceWorkflowReportTranscript
      ByteString
      Word64
      ByteString
      ByteString
      ByteString
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceWorkflowReportResultTranscript
  = ReferenceWorkflowReportResultTranscript
      Word8
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceWorkflowProblemTranscript
  = ReferenceWorkflowProblemTranscript Word8 [ByteString]
  deriving stock (Generic)
  deriving anyclass (Serialize)

data ReferenceWorkflowTerminalDigestTranscript
  = ReferenceWorkflowTerminalDigestTranscript
      ByteString
      ByteString
      ByteString
      Word8
      Word64
      ByteString
  deriving stock (Generic)
  deriving anyclass (Serialize)
