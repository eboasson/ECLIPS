-- | Interleave contiguous control replay with dependency-ready retained data.
-- In particular, the old cut is installed before its later Activate entry;
-- control-first copying of an owner's latest projection is never used.
module Eclips.Herald.UseCase.JoinHistory
  ( JoinHistoryReplayProblem (..),
    replayJoinHistory,
    replayJoinControlHistory,
  ) where

import Control.Monad (foldM, unless)
import Data.List.NonEmpty qualified as NonEmpty
import Eclips.Domain.Membership (heraldMembershipGenerationActiveHeraldEpochs)
import Eclips.Herald.EffectBatch (EffectBatch)
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.Progress qualified as Progress
import Eclips.Herald.Join.History qualified as History
import Eclips.Herald.Join.State qualified as Join
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Startup.State
import Eclips.Herald.UseCase.OracleAdvance qualified as OracleAdvance
import Eclips.Oracle.Admission
  ( HeraldAdmissionPhase (AdmissionActivated),
    admissionRecordAttempt,
    admissionRecordId,
    admissionRecordPhase,
    admissionRecordPredecessor,
  )
import Eclips.Oracle.Canonical (CanonicalAppliedOracleEntry, canonicalAppliedOracleEntryValue)
import Eclips.Oracle.Projection
  ( OracleProjectionEventView (HeraldAdmissionChangedView),
    appliedEntryControlIndex,
    appliedEntryProjectionEvents,
    oracleProjectionEventView,
  )

data JoinHistoryReplayProblem
  = JoinHistoryReplayDataProblem History.JoinHistoryProblem
  | JoinHistoryReplayControlProblem
  deriving stock (Eq, Show)

-- | The Boolean is a checked full-cut observation, not a receipt for merely
-- receiving a bundle. Retry unions additional donors' retained facts through
-- the same owned state; the root Join owner retains all canonical bundle bytes.
replayJoinHistory :: History.JoinHistory -> HeraldState -> Either JoinHistoryReplayProblem (HeraldState, EffectBatch, Bool)
replayJoinHistory history initial = do
  -- An omitted prefix is justified only by this receiver's installed semantic
  -- state. Reject before even passive data import; a wire anchor is not a base.
  unless
    (Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState initial)) >= History.joinHistoryControlAnchor history)
    (Left (JoinHistoryReplayDataProblem History.JoinHistoryCanonicalControlGap))
  (replayed, effects) <-
    if not (Progress.structuralProgressIsJoiningObserver (startupStructuralProgressState initial))
      then dataPass initial
      else foldM replayControl (initial, mempty) (History.joinHistoryControlEntries history)
  if Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState replayed)) < History.joinHistoryControlPrefix history
    then pure (replayed, effects, False)
    else do
      (final, emitted) <- mapData (History.installJoinHistory history replayed)
      pure (final, effects <> emitted, History.joinHistoryInstalled history final)
  where
    dataPass = mapData . History.advanceJoinHistoryData history
    replayControl (state, effects) entry
      -- Exact old controls still require canonical/cursor validation, but
      -- cannot release data dependencies. The first new entry's before-pass,
      -- or final installation for an all-old prefix, admits this donor's data.
      | appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry)
          <= Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState state)) = do
          (projected, emitted) <- replayJoinControlHistory [entry] state
          pure (projected, effects <> emitted)
      | otherwise = do
          (before, beforeEffects) <- dataPass state
          (projected, emitted) <- replayJoinControlHistory [entry] before
          (after, afterEffects) <- dataPass projected
          pure (after, effects <> beforeEffects <> emitted <> afterEffects)
    mapData = either (Left . JoinHistoryReplayDataProblem) Right

-- | Adopt the sealed predecessor frontier in the same atomic transition as
-- this applicant's activation. Merely receiving a preparing or sealed attempt
-- cannot install a live frontier: a later invalidation may replace that attempt.
-- Subsequent exact history replay cannot rewind an active owner's latest plans.
replayJoinControlHistory ::
  [CanonicalAppliedOracleEntry] ->
  HeraldState ->
  Either JoinHistoryReplayProblem (HeraldState, EffectBatch)
replayJoinControlHistory entries initial = foldM replay (initial, mempty) entries
  where
    replay (state, effects) entry = do
      prepared <- prepareActivation entry state
      (successor, emitted) <-
        either
          (const (Left JoinHistoryReplayControlProblem))
          Right
          (OracleAdvance.applyJoinControlHistory [entry] prepared)
      pure (successor, effects <> emitted)

    prepareActivation entry state
      | not (Progress.structuralProgressIsJoiningObserver (startupStructuralProgressState state)) = Right state
      | otherwise = case Genesis.checkedStartupAdmission (startupGenesis state) of
          Nothing -> Right state
          Just startupAdmission
            | any (activates (admissionRecordId startupAdmission)) (appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue entry)) -> do
                record <-
                  maybe
                    (Left JoinHistoryReplayControlProblem)
                    Right
                    (Projection.oracleViewHeraldAdmission (admissionRecordId startupAdmission) (Projection.oracleView (startupOracleProjectionState state)))
                let source = minimum (NonEmpty.toList (heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor record)))
                    key = (admissionRecordId record, admissionRecordAttempt record, source)
                bytes <- maybe (Left JoinHistoryReplayControlProblem) Right (lookup key (Join.installedHistories (startupJoinState state)))
                history <- mapData (History.decodeJoinSourceHistory bytes)
                mapData (History.adoptJoinAlignmentFrontier history state)
            | otherwise -> Right state
    activates admission event = case oracleProjectionEventView event of
      HeraldAdmissionChangedView record -> admissionRecordId record == admission && case admissionRecordPhase record of AdmissionActivated {} -> True; _ -> False
      _ -> False
    mapData = either (Left . JoinHistoryReplayDataProblem) Right
