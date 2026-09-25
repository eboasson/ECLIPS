{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Historical installation evidence for each pending label decision.
-- Application admission belongs to the selected source group and endpoint hold;
-- this owner contains no global observation gate or preparation phase.
module Eclips.Herald.LabelBarrier.State
  ( State,
    initialState,
    barrierLocalHerald,
    barrierActiveDecisions,
    TerminalOutcomeEvidence,
    terminalOutcomeEvidence,
    terminalOutcomeDecisionId,
    terminalOutcomeControlIndex,
    terminalOutcomeDigest,
    terminalOutcomeCatalogueDigest,
    terminalOutcomeMemberSetDigest,
    barrierTerminalEvidence,
    barrierTerminalEvidences,
    barrierCollectionState,
    replaceBarrierCollectionState,
    BarrierProblem (..),
    PreparedTerminalApplication,
    prepareTerminalApplication,
    commitTerminalApplication,
    PreparedWorkflowCompletion,
    prepareWorkflowCompletion,
    commitWorkflowCompletion,
    BarrierInvariantViolation (..),
    validateBarrierState,
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Eclips.Domain.Identity (ControlIndex, HeraldEpoch, LabelDecisionId, controlIndexWord64)
import Eclips.Domain.Label (LabelOutcomeDigest, labelInstallationControlIndex, labelInstallationDecisionId, labelInstallationOutcomeDigest, labelInstallationReporter)
import Eclips.Domain.Sort.Profile (CatalogueDigest)
import Eclips.Domain.Topology (MemberSetDigest)
import Eclips.Herald.Label.Collection qualified as Collection

data TerminalOutcomeEvidence = TerminalOutcomeEvidence
  { decisionId :: !LabelDecisionId,
    controlIndex :: !ControlIndex,
    digest :: !LabelOutcomeDigest,
    catalogueDigest :: !CatalogueDigest,
    memberSetDigest :: !MemberSetDigest
  }
  deriving stock (Eq, Show)

data State = State
  { localHerald :: !HeraldEpoch,
    terminal :: !(Map LabelDecisionId TerminalOutcomeEvidence),
    collection :: !Collection.State
  }
  deriving stock (Eq, Show)

initialState :: HeraldEpoch -> State
initialState localHerald = State localHerald Map.empty Collection.emptyState

barrierLocalHerald :: State -> HeraldEpoch
barrierLocalHerald state = state.localHerald

barrierActiveDecisions :: State -> Set LabelDecisionId
barrierActiveDecisions state = Map.keysSet state.terminal

terminalOutcomeEvidence :: LabelDecisionId -> ControlIndex -> LabelOutcomeDigest -> CatalogueDigest -> MemberSetDigest -> TerminalOutcomeEvidence
terminalOutcomeEvidence = TerminalOutcomeEvidence

terminalOutcomeDecisionId :: TerminalOutcomeEvidence -> LabelDecisionId
terminalOutcomeDecisionId evidence = evidence.decisionId

terminalOutcomeControlIndex :: TerminalOutcomeEvidence -> ControlIndex
terminalOutcomeControlIndex evidence = evidence.controlIndex

terminalOutcomeDigest :: TerminalOutcomeEvidence -> LabelOutcomeDigest
terminalOutcomeDigest evidence = evidence.digest

terminalOutcomeCatalogueDigest :: TerminalOutcomeEvidence -> CatalogueDigest
terminalOutcomeCatalogueDigest evidence = evidence.catalogueDigest

terminalOutcomeMemberSetDigest :: TerminalOutcomeEvidence -> MemberSetDigest
terminalOutcomeMemberSetDigest evidence = evidence.memberSetDigest

barrierTerminalEvidence :: LabelDecisionId -> State -> Maybe TerminalOutcomeEvidence
barrierTerminalEvidence decision state = Map.lookup decision state.terminal

barrierTerminalEvidences :: State -> [TerminalOutcomeEvidence]
barrierTerminalEvidences state = Map.elems state.terminal

barrierCollectionState :: State -> Collection.State
barrierCollectionState state = state.collection

replaceBarrierCollectionState :: Collection.State -> State -> State
replaceBarrierCollectionState collection state = state {collection}

data BarrierProblem
  = BarrierWorkflowAlreadyActive LabelDecisionId
  | BarrierTerminalIndexMustBePositive ControlIndex
  | BarrierTerminalEvidenceMissing LabelDecisionId
  deriving stock (Eq, Show)

newtype PreparedTerminalApplication = PreparedTerminalApplication State

-- Installation is recorded only after direct effects are applied. It remains a
-- historical fact even if a later End supersedes the effective label.
prepareTerminalApplication :: TerminalOutcomeEvidence -> State -> Either BarrierProblem PreparedTerminalApplication
prepareTerminalApplication evidence state
  | controlIndexWord64 evidence.controlIndex == 0 = Left (BarrierTerminalIndexMustBePositive evidence.controlIndex)
  | Just retained <- Map.lookup evidence.decisionId state.terminal, retained /= evidence = Left (BarrierWorkflowAlreadyActive retained.decisionId)
  | otherwise = Right (PreparedTerminalApplication state {terminal = Map.insert evidence.decisionId evidence state.terminal})

commitTerminalApplication :: PreparedTerminalApplication -> State
commitTerminalApplication (PreparedTerminalApplication state) = state

newtype PreparedWorkflowCompletion = PreparedWorkflowCompletion State

prepareWorkflowCompletion :: LabelDecisionId -> State -> Either BarrierProblem PreparedWorkflowCompletion
prepareWorkflowCompletion decision state
  | Map.member decision state.terminal = Right (PreparedWorkflowCompletion state {terminal = Map.delete decision state.terminal, collection = Collection.complete decision state.collection})
  | otherwise = Left (BarrierTerminalEvidenceMissing decision)

commitWorkflowCompletion :: PreparedWorkflowCompletion -> State
commitWorkflowCompletion (PreparedWorkflowCompletion state) = state

data BarrierInvariantViolation = BarrierInvariantTerminalEvidenceMismatch
  deriving stock (Eq, Show)

validateBarrierState :: State -> Either BarrierInvariantViolation ()
validateBarrierState state
  | Collection.valid state.collection,
    all (\(decision, evidence) -> decision == evidence.decisionId && controlIndexWord64 evidence.controlIndex > 0) (Map.toList state.terminal),
    all reportMatches (Collection.reports state.collection) =
      Right ()
  | otherwise = Left BarrierInvariantTerminalEvidenceMismatch
  where
    reportMatches report = case Map.lookup (labelInstallationDecisionId report) state.terminal of
      Nothing -> False
      Just evidence ->
        labelInstallationControlIndex report == evidence.controlIndex
          && labelInstallationOutcomeDigest report == evidence.digest
          && labelInstallationReporter report == state.localHerald
