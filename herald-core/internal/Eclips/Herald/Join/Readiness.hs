-- | Exact source-cut and readiness checks shared by Join requests and private
-- joining-state replacement. Reading these facts grants no admission authority.
module Eclips.Herald.Join.Readiness (histories, localReadyReport) where

import Control.Monad (unless)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Eclips.Domain.Membership (heraldMembershipGenerationActiveHeraldEpochs)
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.Progress qualified as Graph
import Eclips.Herald.Join.Base (checkedJoinBase)
import Eclips.Herald.Join.History qualified as History
import Eclips.Herald.Join.State qualified as Join
import Eclips.Herald.Startup.State
import Eclips.Herald.UseCase.OracleAdvance (structuralReconciliationViews)
import Eclips.Oracle.Admission

histories :: HeraldAdmissionRecord -> HeraldState -> [History.JoinSourceHistory]
histories record state =
  [history | ((admission, attempt, _), bytes) <- Join.installedHistories (startupJoinState state), admission == admissionRecordId record, attempt == admissionRecordAttempt record, Right history <- [History.decodeJoinSourceHistory bytes]]

localReadyReport :: HeraldAdmissionRecord -> HeraldState -> Either String HeraldJoinReadyReport
localReadyReport record state = do
  seal <- maybe (Left "no join seal") Right (admissionRecordSeal record)
  let retained = histories record state
      bySource = Map.fromList [(History.joinHistorySource (History.sourceJoinHistory source), source) | source <- retained]
      expected = NE.toList (heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor record))
  unless (Map.keys bySource == expected && all (\(source, memberCut) -> maybe False (\sourceHistory -> History.joinSourceMemberCut sourceHistory == memberCut && History.joinHistoryInstalled (History.sourceJoinHistory sourceHistory) state) (Map.lookup source bySource)) (joinSealMemberCuts seal)) (Left "sealed input cut is incomplete")
  (contribution, recipe) <- checkedJoinBase record state
  _ <- either (Left . show) Right (Graph.prepareHeraldJoinBaseCandidate record recipe contribution (structuralReconciliationViews state) (startupGraphState state) (startupStructuralProgressState state))
  pure (heraldJoinReadyReport (admissionRecordId record) (admissionRecordAttempt record) (Genesis.checkedLocalHeraldEpoch (startupGenesis state)) (joinSealDigest seal) (joinSealControlPrefix seal) (joinSealRecipeDigest seal))
