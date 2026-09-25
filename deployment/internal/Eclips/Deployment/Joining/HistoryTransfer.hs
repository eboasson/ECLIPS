-- | The finite history-transfer workflow with its transport and pacing
-- boundaries supplied by the runtime shell. Every wait observes current
-- admission authority, including when the selected old source is unavailable.
module Eclips.Deployment.Joining.HistoryTransfer
  ( HistoryTransferResult (..),
    HistoryAdmissionObservation (..),
    transferJoinHistories,
    ControlHistoryResult (..),
    controlHistoryLocalResult,
    awaitJoinControlHistory,
  ) where

import Data.ByteString (ByteString)
import Data.List (find, maximumBy)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Eclips.Domain.Identity (HeraldEpoch)
import Eclips.Domain.Membership (HeraldAdmissionId, heraldMembershipGenerationActiveHeraldEpochs)
import Eclips.Herald.Join (JoinReply (..), JoinRequest (InstallJoinControlHistory, InstallJoinHistory, ReadJoinStatus), JoinStatus (..))
import Eclips.Oracle.Admission
import Eclips.Oracle.Canonical (CanonicalAppliedOracleEntry)

data ControlHistoryResult
  = ControlHistorySynchronized
  | ControlHistoryCancelled
  | ControlHistoryRejected
  deriving stock (Eq, Show)

-- Activation ends the onboarding callback's authority even when the supplied
-- batch has a later suffix. The caller separately checks its expected epoch
-- and membership before treating the full admission as complete.
controlHistoryLocalResult :: HeraldAdmissionId -> JoinReply -> Maybe ControlHistoryResult
controlHistoryLocalResult admission (JoinStatusReply status)
  | joinStatusServing status = Just ControlHistorySynchronized
  | Just record <- find ((== admission) . admissionRecordId) (joinStatusAdmissions status),
    AdmissionCancelled {} <- admissionRecordPhase record =
      Just ControlHistoryCancelled
controlHistoryLocalResult _ _ = Nothing

-- Only incomplete installation needs an additional remote authority read.
-- Keep the current attempt's control replay separate from the caller's decision
-- to recapture source histories after an attempt change.
awaitJoinControlHistory ::
  (Monad m) =>
  m (Maybe HeraldAdmissionRecord) ->
  (JoinRequest -> m JoinReply) ->
  m () ->
  HeraldAdmissionId ->
  [CanonicalAppliedOracleEntry] ->
  m ControlHistoryResult
awaitJoinControlHistory latest local pace admission entries = wait
  where
    wait = do
      before <- controlHistoryLocalResult admission <$> local ReadJoinStatus
      case before of
        Just result -> pure result
        Nothing -> do
          reply <- local (InstallJoinControlHistory entries)
          after <- controlHistoryLocalResult admission <$> local ReadJoinStatus
          case after of
            Just result -> pure result
            Nothing | reply == JoinHistoryInstalled -> pure ControlHistorySynchronized
            Nothing -> do
              observed <- latest
              case observed of
                Just record
                  | admissionRecordId record == admission,
                    AdmissionCancelled {} <- admissionRecordPhase record ->
                      pure ControlHistoryCancelled
                _ | reply == JoinRejected -> pure ControlHistoryRejected
                _ -> pace >> wait

data HistoryTransferResult
  = HistoriesTransferred
  | HistoryTransferRestart HeraldAdmissionRecord
  | HistoryTransferCancelled
  | HistoryTransferRejected
  | HistoryTransferConflict
  deriving stock (Eq, Show)

-- Best available projection, then the exact receiving owner's projection.
-- Absence of a newer global observation does not establish receiver freshness.
data HistoryAdmissionObservation = HistoryAdmissionObservation (Maybe HeraldAdmissionRecord) (Maybe HeraldAdmissionRecord)

transferJoinHistories ::
  (Monad m) =>
  (Maybe HeraldEpoch -> m HistoryAdmissionObservation) ->
  (HeraldEpoch -> JoinRequest -> m (Maybe JoinReply)) ->
  (JoinRequest -> m JoinReply) ->
  m () ->
  HeraldAdmissionRecord ->
  [ByteString] ->
  m HistoryTransferResult
transferJoinHistories latest remote local pace expected bundles = do
  staged <- run False transfers
  case staged of
    HistoriesTransferred -> run True transfers
    other -> pure other
  where
    members = NE.toList (heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor expected))
    transfers = [(Just member, bundle) | member <- members, bundle <- bundles] <> [(Nothing, bundle) | bundle <- bundles]
    run _ [] = pure HistoriesTransferred
    run waiting remaining@((destination, bundle) : rest) = do
      (before, _) <- changedAttempt destination
      case before of
        Just result -> pure result
        Nothing -> do
          reply <- case destination of
            Just member -> remote member (InstallJoinHistory bundle)
            Nothing -> Just <$> local (InstallJoinHistory bundle)
          -- An End or label release can invalidate the attempt after the
          -- precheck and before this reply. Refresh before classifying it.
          (after, receiverCurrent) <- changedAttempt destination
          case after of
            Just result -> pure result
            Nothing -> case reply of
              Just (JoinTransferRetired admission attempt)
                | admission == admissionRecordId expected,
                  attempt == admissionRecordAttempt expected ->
                    awaitRetired destination
              Just JoinHistoryInstalled -> run waiting rest
              Just JoinRetry | not waiting -> run waiting rest
              Just JoinRetry -> pace >> run waiting remaining
              Nothing -> pace >> run waiting remaining
              Just JoinRequestConflict | receiverCurrent -> pure HistoryTransferConflict
              _ | receiverCurrent -> pure HistoryTransferRejected
              _ -> pace >> run waiting remaining
    -- Retirement is definitive for these exact transfer bytes, even when the
    -- following status exchange is unavailable. Only semantic admission status
    -- chooses restart or cancellation; never resend the discarded envelope.
    awaitRetired destination = do
      pace
      (changed, _) <- changedAttempt destination
      maybe (awaitRetired destination) pure changed
    changedAttempt destination = do
      HistoryAdmissionObservation available receiver <- latest destination
      let observations = filter ((== admissionRecordId expected) . admissionRecordId) (catMaybes [available, receiver])
          current = case observations of [] -> Nothing; records -> Just (maximumBy (comparing admissionRecordChangedIndex) records)
          receiverCurrent = maybe False (\record -> admissionRecordId record == admissionRecordId expected && admissionRecordAttempt record == admissionRecordAttempt expected && admissionRecordChangedIndex record >= admissionRecordChangedIndex expected) receiver
      pure
        ( case current of
            Just record
              | admissionRecordId record == admissionRecordId expected,
                admissionRecordChangedIndex record >= admissionRecordChangedIndex expected ->
                  case admissionRecordPhase record of
                    AdmissionCancelled {} -> Just HistoryTransferCancelled
                    AdmissionActivated {} -> Just (HistoryTransferRestart record)
                    _ | admissionRecordAttempt record > admissionRecordAttempt expected -> Just (HistoryTransferRestart record)
                    _ -> Nothing
            _ -> Nothing,
          receiverCurrent
        )
