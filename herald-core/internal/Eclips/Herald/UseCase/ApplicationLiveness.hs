{-# LANGUAGE OverloadedStrings #-}

-- | Atomic composition for application recovery, wait reachability, and the
-- one automatic process-End intention produced by final unexpected loss.
module Eclips.Herald.UseCase.ApplicationLiveness
  ( advanceApplicationRecoveryBeforeSessionIngress,
    applyApplicationBindingLoss,
    applyApplicationRecoveryTimer,
  )
where

import Data.ByteString (ByteString)
import Data.Serialize.Put qualified as Serialize
import Eclips.Application.Types.Lifecycle (connectionDescriptorAttachmentBytes)
import Eclips.Domain.Identity (ProcessEpochId, processEpochIdBytes)
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason (ApplicationPermanentlyLost),
  )
import Eclips.Herald.Application.Session
  ( ApplicationSessionBinding,
    ApplicationSessionUnavailableReason (ApplicationSessionNoLongerLive),
  )
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    emptyEffectBatch,
    orderedEffectBatch,
  )
import Eclips.Herald.Input
  ( ApplicationSessionIngress (..),
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (HeraldTransitionInvariant),
    HeraldTransitionInvariantViolation (..),
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupApplicationState,
    replaceStartupOracleClientState,
    replaceStartupWaitState,
    startupApplicationState,
    startupOracleClientState,
    startupWaitState,
  )
import Eclips.Herald.Time (MonotonicInstant)
import Eclips.Herald.Timer (TimerAttempt, TimerOutcome)
import Eclips.Herald.UseCase.ProcessPreparation qualified as Preparation
import Eclips.Herald.Wait.State qualified as Wait

advanceApplicationRecoveryBeforeSessionIngress ::
  MonotonicInstant ->
  ApplicationSessionIngress ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
advanceApplicationRecoveryBeforeSessionIngress observedAt ingress predecessor =
  case ingressProcess ingress (startupApplicationState predecessor) of
    Nothing -> Right (predecessor, emptyEffectBatch)
    Just process -> do
      prepared <-
        mapApplicationProblem
          ( Application.prepareApplicationRecoverySweep
              observedAt
              process
              (startupApplicationState predecessor)
          )
      settleExpiration
        (Application.commitApplicationRecoverySweep prepared)
        (Application.preparedApplicationRecoverySweepExpiration prepared)
        predecessor

applyApplicationBindingLoss ::
  MonotonicInstant ->
  ApplicationSessionBinding ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyApplicationBindingLoss _ binding predecessor = do
  prepared <-
    mapApplicationProblem
      (Application.prepareApplicationBindingTermination binding (startupApplicationState predecessor))
  settleExpiration
    (Application.commitApplicationRecoverySweep prepared)
    (Application.preparedApplicationRecoverySweepExpiration prepared)
    predecessor

applyApplicationRecoveryTimer ::
  TimerAttempt ->
  TimerOutcome ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyApplicationRecoveryTimer attempt outcome predecessor = do
  prepared <-
    mapApplicationProblem
      ( Application.prepareApplicationRecoveryTimerObservation
          attempt
          outcome
          (startupApplicationState predecessor)
      )
  let application = Application.commitApplicationRecoveryTimerObservation prepared
  case Application.preparedApplicationRecoveryTimerDisposition prepared of
    Application.ApplicationRecoveryTimerStale ->
      Right
        (replaceStartupApplicationState application predecessor, emptyEffectBatch)
    Application.ApplicationRecoveryTimerRearmed successor spec ->
      Right
        ( replaceStartupApplicationState application predecessor,
          orderedEffectBatch [ArmTimer successor spec]
        )
    Application.ApplicationRecoveryTimerExpired expiration ->
      settleExpiration application expiration predecessor

settleExpiration ::
  Application.State ->
  Application.ApplicationRecoveryExpiration ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
settleExpiration application expiration predecessor = do
  preparedWait <-
    mapWaitProblem
      ( Wait.prepareWaitRemoval
          (Application.applicationRecoveryExpiredWaitIds expiration)
          (startupWaitState predecessor)
      )
  let (waits, _) = Wait.commitWaitRemoval preparedWait
      withReachability =
        replaceStartupWaitState waits
          . replaceStartupApplicationState application
          $ predecessor
  (successor, oracleEffects) <- case Application.applicationRecoverySealedProcess expiration of
    Nothing -> Right (withReachability, [])
    Just process -> retainAutomaticEnd process withReachability
  reclaimed <-
    if null (Application.applicationRecoveryExpiredSessionIds expiration)
      then Right successor
      else Preparation.retireClosedLifecycleReceipts successor
  let terminalEffects =
        [ DisposeApplicationSession session ApplicationSessionNoLongerLive
        | session <- Application.applicationRecoveryExpiredSessionIds expiration
        ]
      cancellationEffects =
        fmap CancelTimer (Application.applicationRecoveryCancelledTimers expiration)
  Right
    ( reclaimed,
      orderedEffectBatch (cancellationEffects <> terminalEffects <> oracleEffects)
    )

retainAutomaticEnd ::
  ProcessEpochId ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
retainAutomaticEnd process predecessor = do
  prepared <-
    mapOracleProblem
      ( OracleClient.prepareEndProcessEpochRequest
          (automaticApplicationLossSemanticKey process)
          process
          ApplicationPermanentlyLost
          (startupOracleClientState predecessor)
      )
  let (oracleClient, classification, _) = OracleClient.commitOracleRequest prepared
  case classification of
    OracleClient.OracleRequestExactRetry ->
      Left (HeraldTransitionInvariant HeraldApplicationTransitionContradiction)
    OracleClient.OracleRequestFirstPrepared ->
      Right
        ( replaceStartupOracleClientState oracleClient predecessor,
          fmap
            RunOracleClientAction
            (OracleClient.oracleClientRequestActions oracleClient)
        )

automaticApplicationLossSemanticKey :: ProcessEpochId -> ByteString
automaticApplicationLossSemanticKey process =
  Serialize.runPut $ do
    Serialize.putByteString "ECLIPS-AUTOMATIC-APPLICATION-LOSS-END"
    Serialize.putByteString (processEpochIdBytes process)

ingressProcess ::
  ApplicationSessionIngress ->
  Application.State ->
  Maybe ProcessEpochId
ingressProcess ingress application = case ingress of
  ClaimInitialApplicationSession _ descriptor _ _ -> do
    attachment <- either (const Nothing) Just (Session.applicationAttachmentFromClaimBytes (connectionDescriptorAttachmentBytes descriptor))
    Application.applicationAttachmentProcess attachment application
  OpenApplicationSession _ attachment _ ->
    Application.applicationAttachmentProcess attachment application
  ResumeApplicationSession _ session _ _ ->
    Application.applicationSessionProcess session application
  EndApplicationSession _ session ->
    Application.applicationSessionProcess session application

mapApplicationProblem ::
  Either Application.ApplicationSessionTransitionError value ->
  Either HeraldInvariantFault value
mapApplicationProblem = either (const applicationFault) Right

mapWaitProblem :: Either problem value -> Either HeraldInvariantFault value
mapWaitProblem = either (const applicationFault) Right

mapOracleProblem :: Either problem value -> Either HeraldInvariantFault value
mapOracleProblem = either (const applicationFault) Right

applicationFault :: Either HeraldInvariantFault value
applicationFault =
  Left (HeraldTransitionInvariant HeraldApplicationTransitionContradiction)
