{-# LANGUAGE OverloadedStrings #-}

-- | Restricted onboarding interpreted by the same serialized Herald owner.
module Eclips.Herald.UseCase.Join (applyJoinRequest, releaseFinishedJoinTransfers) where

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Domain.Membership
import Eclips.Domain.Startup (HeraldMember (..))
import Eclips.Herald.Alignment.History qualified as AlignmentHistory
import Eclips.Herald.Alignment.Protocol qualified as AlignmentProtocol
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.EffectBatch
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.Progress qualified as Graph
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.Join
import Eclips.Herald.Join.Base (checkedJoinBase)
import Eclips.Herald.Join.History qualified as History
import Eclips.Herald.Join.Readiness (histories, localReadyReport)
import Eclips.Herald.Join.State qualified as Join
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Startup.Invariant
import Eclips.Herald.Startup.State
import Eclips.Herald.UseCase.ControlBase qualified as ControlBase
import Eclips.Herald.UseCase.ControlReclamation qualified as Reclamation
import Eclips.Herald.UseCase.JoinHistory qualified as Replay
import Eclips.Herald.UseCase.OracleAdvance (structuralReconciliationViews)
import Eclips.Herald.UseCase.Step15StructuralBase qualified as StructuralBase
import Eclips.Herald.UseCase.StructuralCoordinator qualified as Structural
import Eclips.Oracle.Admission
import Eclips.Oracle.Failure qualified as Failure

applyJoinRequest :: Word64 -> ByteString -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
applyJoinRequest correlation bytes predecessor
  | heraldPhase predecessor /= HeraldServing = reply predecessor mempty JoinRejected
  | otherwise = case decodeJoinRequest bytes of
      Left _ -> reply predecessor mempty JoinRejected
      Right request
        | Isolation.isolationWitnessPhase (Isolation.stateWitness (startupIsolationState predecessor)) `elem` [Isolation.IsolationReadOnlyDrainView, Isolation.IsolationTerminalView],
          not (inspectionRequest request) ->
            reply predecessor mempty JoinRejected
      Right request -> dispatch request
  where
    inspectionRequest = \case
      ReadJoinStatus -> True
      ReadOracleStatus -> True
      ReadOracleVoterChange {} -> True
      ReadOracleVoterHostFailure {} -> True
      ReadJoinCommand {} -> True
      ReadJoinCommandWithRetirement {} -> True
      RetireJoinReceipts {} -> True
      _ -> False
    dispatch request = case request of
      SubmitJoinCommandWithRetirement token command progress -> retire (joinRequestOwner token) progress (Just (SubmitJoinCommand token command))
      ReadJoinCommandWithRetirement token progress -> retire (joinRequestOwner token) progress (Just (ReadJoinCommand token))
      RetireJoinReceipts owner progress -> retire owner progress Nothing
      ReadJoinStatus -> reply predecessor mempty (JoinStatusReply (status predecessor))
      ReadOracleStatus ->
        let current = controlView predecessor
         in reply
              predecessor
              mempty
              ( OracleStatusReply
                  ( HeraldOracleStatus
                      (Projection.oracleViewControlIndex current)
                      (Projection.oracleViewVoterConfiguration current)
                      (Projection.oracleViewOracleReplicas current)
                      (Projection.oracleViewPendingVoterChange current)
                  )
              )
      ReadOracleVoterChange identifier ->
        reply predecessor mempty (OracleVoterChangeReply (Projection.oracleViewVoterChange identifier (controlView predecessor)))
      ReadOracleVoterHostFailure resolution ->
        let certificate = case Projection.projectedFailureProbeTerminal =<< Projection.oracleViewFailureProbe (failureProbeResolutionProbeId resolution) (controlView predecessor) of
              Just (Projection.ProjectedVoterHostFailureAccepted accepted)
                | Failure.acceptedVoterHostFailureResolution accepted == resolution -> Just accepted
              _ -> Nothing
         in reply predecessor mempty (OracleVoterHostFailureReply certificate)
      ReadJoinControlHistory after ->
        -- A checked semantic base can advance while an applicant still owns
        -- an earlier contiguous tail. Sparse result pins alone are insufficient.
        case Projection.retainedControlSuffixAfter after (startupOracleProjectionState predecessor) of
          Nothing -> reply predecessor mempty JoinRejected
          Just entries -> reply predecessor mempty (JoinControlHistoryReply entries)
      InstallJoinControlHistory entries
        | Genesis.checkedStartupAdmission (startupGenesis predecessor) == Nothing -> reply predecessor mempty JoinRejected
        | otherwise -> case Replay.replayJoinControlHistory entries predecessor of
            Left _ -> reply predecessor mempty JoinRejected
            Right (successor, effects) -> do
              -- Scratch replay also uses the entry fold, so only the complete
              -- externally published transaction may discard its input tail.
              reclaimed <-
                if Projection.oracleViewControlIndex (view successor) > Projection.oracleViewControlIndex (view predecessor)
                  || Client.canonicalEvidenceReleased (startupOracleClientState predecessor) (startupOracleClientState successor)
                  then Reclamation.reclaimControlPrefixOrFault successor
                  else pure successor
              reply
                reclaimed
                effects
                ( if all
                    ( \entry -> case Projection.classifyAppliedEntry entry (startupControlOracleProjectionState reclaimed) of
                        Projection.AppliedEntryExactDuplicate _ -> True
                        Projection.AppliedEntryCoveredByBase _ -> True
                        _ -> False
                    )
                    entries
                    then JoinHistoryInstalled
                    else JoinRetry
                )
      ReadJoinReady admission -> case Projection.oracleViewHeraldAdmission admission (view predecessor) of
        Just record | transferReleased predecessor admission (admissionRecordAttempt record) -> reply predecessor mempty (JoinTransferRetired admission (admissionRecordAttempt record))
        Just record | Right report <- localReadyReport record predecessor -> reply predecessor mempty (JoinReadyReply report)
        _ -> reply predecessor mempty JoinRetry
      ReadJoinCommand token -> commandResult token predecessor
      SubmitJoinCommand token command ->
        case Join.lookupRequest token (startupJoinState predecessor) of
          Just (retained, _) | retained /= command -> reply predecessor mempty JoinRequestConflict
          Just _ -> commandResult token predecessor
          Nothing
            | Join.requestIsRetired token (startupJoinState predecessor) -> reply predecessor mempty (JoinRequestRetired token (Join.requestReceiptRetirement (joinRequestOwner token) (startupJoinState predecessor)))
            | not (Projection.oracleViewLocalHeraldIsCurrent (view predecessor)) -> reply predecessor mempty JoinRejected
            | not (commandLocallyAdmitted command predecessor) -> reply predecessor mempty JoinRetry
            | otherwise -> case Client.prepareHeraldAdmissionRequest command (startupOracleClientState predecessor) of
                Left _ -> reply predecessor mempty JoinRetry
                Right prepared ->
                  let (client, _, reference) = Client.commitOracleRequest prepared
                      successor =
                        replaceStartupOracleClientState client
                          . replaceStartupJoinState (Join.retainRequest token command reference (startupJoinState predecessor))
                          $ predecessor
                   in reply successor (orderedEffectBatch (map RunOracleClientAction (Client.oracleClientActions client))) JoinCommandPending
      CaptureJoinHistory admission -> case Projection.oracleViewHeraldAdmission admission (view predecessor) of
        -- Begin may already be committed at the coordinator while this old
        -- member is still catching up through its ordinary Oracle watch.
        Nothing -> reply predecessor mempty JoinRetry
        Just record | transferReleased predecessor admission (admissionRecordAttempt record) -> reply predecessor mempty (JoinTransferRetired admission (admissionRecordAttempt record))
        Just record -> case Join.lookupCapture admission (admissionRecordAttempt record) (startupJoinState predecessor) of
          Just captured -> reply predecessor mempty (JoinHistoryReply captured)
          Nothing -> case Structural.advanceLocalStructuralWork predecessor of
            Left _ -> fault
            Right (drained, effects) -> case ControlBase.captureJoiningBase drained of
              Left _ -> reply drained effects JoinRejected
              Right Nothing -> reply drained effects JoinRetry
              Right (Just base) ->
                let history = ControlBase.joiningBaseHistory base
                    captured = ControlBase.encodeJoiningBase base
                    retained =
                      Join.retainInstalledHistory admission (admissionRecordAttempt record) (History.joinHistorySource history) captured
                        . Join.retainCapture admission (admissionRecordAttempt record) captured
                        $ startupJoinState drained
                 in do
                      evidenced <- retainHistoryProvenance history (replaceStartupJoinState retained drained)
                      reply (retainHistoryRecipient history evidenced) effects (JoinHistoryReply captured)
      InstallJoinHistory captured -> case History.sourceJoinHistory <$> History.decodeJoinSourceHistory captured of
        Left _ -> reply predecessor mempty JoinRejected
        Right history
          | History.joinHistorySystem history /= Genesis.checkedSystemId (startupGenesis predecessor) -> reply predecessor mempty JoinRejected
          | transferReleased predecessor (History.joinHistoryAdmission history) (History.joinHistoryAttempt history) -> reply predecessor mempty (JoinTransferRetired (History.joinHistoryAdmission history) (History.joinHistoryAttempt history))
          | Graph.structuralProgressIsJoiningObserver (startupStructuralProgressState predecessor) -> installApplicantHistory history captured
          | otherwise ->
              let key = (History.joinHistoryAdmission history, History.joinHistoryAttempt history, History.joinHistorySource history)
                  retained = startupJoinState predecessor
               in case lookup key (Join.installedHistories retained) of
                    Just prior | prior /= captured -> reply predecessor mempty JoinRequestConflict
                    _ ->
                      let staged = replaceStartupJoinState (Join.retainInstalledHistory (History.joinHistoryAdmission history) (History.joinHistoryAttempt history) (History.joinHistorySource history) captured retained) predecessor
                          bundles = [payload | ((admission, attempt, _), payload) <- Join.installedHistories (startupJoinState staged), admission == History.joinHistoryAdmission history, attempt == History.joinHistoryAttempt history]
                       in case foldM install (staged, mempty) bundles of
                            Left _ -> reply predecessor mempty JoinRejected
                            Right (successor, effects) -> reply successor effects (if History.joinHistoryInstalled history successor then JoinHistoryInstalled else JoinRetry)
    -- A pending applicant is unobservable. Buffer a complete predecessor set
    -- before reconstructing its coherent base; active source members below keep
    -- their monotone data replay and cannot use this replacement boundary.
    installApplicantHistory history captured
      | attempt < newestAttempt admission = reply predecessor mempty (JoinTransferRetired admission attempt)
      | otherwise = case lookup key (Join.installedHistories retained) of
          Just prior
            | prior /= captured -> reply predecessor mempty JoinRequestConflict
            | otherwise ->
                reply predecessor mempty
                  $ if maybe False (\record -> admissionRecordAttempt record == attempt && completeSourceSet record retained && History.joinHistoryInstalled history predecessor) (Projection.oracleViewHeraldAdmission admission (view predecessor))
                    then JoinHistoryInstalled
                    else JoinRetry
          Nothing -> case ControlBase.sourceAdmission source captured predecessor of
            Left _ -> reply predecessor mempty JoinRejected
            Right record ->
              let currentAttempt = Join.releaseTransferHistories (\identifier ordinal -> identifier == admission && ordinal < attempt) retained
                  buffered = Join.retainInstalledHistory admission attempt source captured currentAttempt
                  staged = replaceStartupJoinState buffered predecessor
               in if not (completeSourceSet record buffered)
                    then reply staged mempty JoinRetry
                    else case ControlBase.replacePortableJoiningBases (sourceSet record buffered) staged of
                      Left _ -> reply predecessor mempty JoinRejected
                      Right successor -> reply successor mempty JoinHistoryInstalled
      where
        admission = History.joinHistoryAdmission history
        attempt = History.joinHistoryAttempt history
        source = History.joinHistorySource history
        key = (admission, attempt, source)
        retained = startupJoinState predecessor
    -- Retained group keys have already passed sourceAdmission. They remember a
    -- checked invalidation while the old semantic staging stays unpublished;
    -- adoption later records that attempt canonically before releasing them.
    newestAttempt admission =
      maximum
        $ 0
          : [admissionRecordAttempt record | Just record <- [Genesis.checkedStartupAdmission (startupGenesis predecessor)], admissionRecordId record == admission]
            <> [admissionRecordAttempt record | Just record <- [Projection.oracleViewHeraldAdmission admission (view predecessor)]]
            <> [attempt | ((identifier, attempt, _), _) <- Join.installedHistories (startupJoinState predecessor), identifier == admission]
    sourceSet record retained =
      [(source, payload) | ((admission, attempt, source), payload) <- Join.installedHistories retained, admission == admissionRecordId record, attempt == admissionRecordAttempt record]
    completeSourceSet record retained =
      Set.fromList (map fst (sourceSet record retained)) == Set.fromList (NE.toList (heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor record)))
    install (state, effects) payload = do
      history <- either (Left . show) (Right . History.sourceJoinHistory) (History.decodeJoinSourceHistory payload)
      (successor, emitted, _) <- either (Left . show) Right (Replay.replayJoinHistory history state)
      evidenced <- either (Left . show) Right (retainHistoryProvenance history successor)
      pure (retainHistoryRecipient history evidenced, effects <> emitted)
    reply state effects result = Right (state, effects <> singletonEffectBatch (SendJoinReply correlation (encodeJoinReply result)))
    fault = Left (HeraldTransitionInvariant HeraldOracleTransitionContradiction)
    commandResult token state = case Join.lookupRequest token (startupJoinState state) of
      Nothing | Join.requestIsRetired token (startupJoinState state) -> reply state mempty (JoinRequestRetired token (Join.requestReceiptRetirement (joinRequestOwner token) (startupJoinState state)))
      Nothing -> reply state mempty JoinRejected
      Just (_, reference) -> case Client.oracleClientRequestEvidence reference (startupOracleClientState state) of
        Nothing -> reply state mempty JoinCommandPending
        Just entry -> case Client.prepareOracleResult reference entry (startupOracleClientState state) of
          Left _ -> fault
          Right prepared ->
            let (client, _, result) = Client.commitOracleResult prepared
                successor = replaceStartupOracleClientState client state
             in reply successor mempty $ case result of
                  Client.OracleRequestHeraldAdmissionChanged record -> JoinCommandComplete [record]
                  _ -> JoinRejected

    retire owner progress work =
      case Join.retireRequestReceipts owner progress settled (startupJoinState predecessor) of
        Left () -> reply predecessor mempty JoinRetirementNotReady
        Right retained ->
          let successor = replaceStartupJoinState retained predecessor
           in case work of
                Nothing -> reply successor mempty (JoinReceiptsRetired owner (Join.requestReceiptRetirement owner retained))
                Just attached -> applyJoinRequest correlation (encodeJoinRequest attached) successor
      where
        settled reference = case Client.lookupOracleRequest reference (startupOracleClientState predecessor) of
          Just witness -> case Client.oracleRequestWitnessStatus witness of Client.OracleRequestProjected {} -> True; _ -> False
          Nothing -> False

retainHistoryProvenance :: History.JoinHistory -> HeraldState -> Either HeraldInvariantFault HeraldState
retainHistoryProvenance history state = do
  retained <-
    either
      (const (Left (HeraldTransitionInvariant HeraldOracleTransitionContradiction)))
      Right
      (Join.retainImportedProvenance (AlignmentHistory.alignmentHistoryPlanAcceptances (History.joinHistoryAlignmentHistory history)) (History.joinHistoryOccurrences history) (startupJoinState state))
  pure (replaceStartupJoinState retained state)

-- The activated applicant's frontier adoption runs before its activation is
-- exposed as a serving membership. Imported semantic evidence is already owned
-- separately; only transfer envelopes and captures are released here.
releaseFinishedJoinTransfers :: HeraldState -> Either HeraldInvariantFault HeraldState
releaseFinishedJoinTransfers state = do
  let terminalOwners =
        Set.fromList
          [ admissionManifestHeraldEpoch (admissionRecordManifest record)
          | record <- Projection.projectedHeraldAdmissions (startupOracleProjectionState state),
            case admissionRecordPhase record of AdmissionActivated {} -> True; AdmissionCancelled {} -> True; _ -> False
          ]
      settled reference = case Client.lookupOracleRequest reference (startupOracleClientState state) of
        Just witness -> case Client.oracleRequestWitnessStatus witness of Client.OracleRequestProjected {} -> True; _ -> False
        Nothing -> False
  retained <-
    either
      (const (Left (HeraldTransitionInvariant HeraldOracleTransitionContradiction)))
      Right
      (Join.retireClosedRequestReceipts (`Set.member` terminalOwners) settled (startupJoinState state))
  let !released = Join.releaseTransferHistories (transferReleased state) retained
  pure (replaceStartupJoinState released state)

transferReleased :: HeraldState -> HeraldAdmissionId -> Word64 -> Bool
transferReleased state admission attempt = case Projection.oracleViewHeraldAdmission admission (view state) of
  Nothing -> False
  Just record
    | attempt < admissionRecordAttempt record -> True
    | otherwise -> case admissionRecordPhase record of
        AdmissionCancelled {} -> True
        AdmissionActivated {} -> not (Graph.structuralProgressIsJoiningObserver (startupStructuralProgressState state))
        _ -> False

-- | The applicant receives these exact passive generation IDs before it can
-- establish an ordinary peer binding. Retain delivery interest on old sources
-- so readiness/certificates produced after sealing reach it without sending
-- unrelated old history. An invalidated attempt replaces this finite scope.
retainHistoryRecipient :: History.JoinHistory -> HeraldState -> HeraldState
retainHistoryRecipient history state =
  case Projection.oracleViewHeraldAdmission (History.joinHistoryAdmission history) (Projection.oracleView (startupOracleProjectionState state)) of
    Just record
      | admissionRecordAttempt record == History.joinHistoryAttempt history,
        Genesis.checkedLocalHeraldEpoch (startupGenesis state) `elem` NE.toList (heraldMembershipGenerationActiveHeraldEpochs (admissionRecordPredecessor record)),
        case admissionRecordPhase record of AdmissionCancelled {} -> False; _ -> True ->
          replaceStartupAlignmentState
            ( Alignment.retainHistoricalAlignmentRecipient
                (admissionManifestHeraldEpoch (admissionRecordManifest record))
                (admissionRecordAttempt record)
                identifiers
                (startupAlignmentState state)
            )
            state
    _ -> state
  where
    identifiers = Set.fromList [AlignmentProtocol.alignmentPlanBindingClaimGeneration binding | plan <- AlignmentHistory.alignmentHistoryPlans (History.joinHistoryAlignmentHistory history), binding <- AlignmentProtocol.alignmentPlanAnnounceBindings plan]

commandLocallyAdmitted :: HeraldAdmissionCommand -> HeraldState -> Bool
commandLocallyAdmitted command state = case command of
  BeginHeraldAdmission _ anchor -> case Graph.structuralAdmissionAnchorClaim (structuralReconciliationViews state) progress of
    Right installed ->
      installed == anchor
        && predecessorBaseSettled
        && Graph.structuralProgressMembershipGenerationId progress == Projection.oracleViewCurrentHeraldMembershipId (view state)
    Left _ -> False
  CancelHeraldAdmission _ -> True
  SealHeraldAdmission seal -> case recordFor (joinSealAdmissionId seal) of
    Nothing -> False
    Just record -> prepareJoinSeal record (histories record state) == Right seal
  AcceptHeraldJoinSeal admission attempt digest -> case recordFor admission of
    Just record -> case admissionRecordSeal record of
      Just seal ->
        admissionRecordAttempt record == attempt
          && joinSealDigest seal == digest
          && prepareJoinSeal record (histories record state) == Right seal
          && lookup local (joinSealMemberCuts seal) == (History.joinSourceMemberCut <$> ownHistory record)
          && either (const False) (const True) (checkedJoinBase record state)
      _ -> False
    _ -> False
  HeraldJoinBaseReady report -> case recordFor (joinReadyAdmissionId report) of
    Just record -> localReadyReport record state == Right report
    _ -> False
  HeraldJoinReady report -> case recordFor (joinReadyAdmissionId report) of
    Just record ->
      joinReadyReporter report == admissionManifestHeraldEpoch (admissionRecordManifest record)
        && maybe False (reportMatches record report) (admissionRecordSeal record)
    _ -> False
  ActivateHerald admission -> case recordFor admission of
    Just record ->
      admissionRecordPhase record == AdmissionReady
        && either (const False) (const True) (localReadyReport record state)
    _ -> False
  where
    local = Genesis.checkedLocalHeraldEpoch (startupGenesis state)
    progress = startupStructuralProgressState state
    predecessorBaseSettled = case startupStructuralBaseCoordinator state of
      Nothing -> True
      Just coordinator -> maybe False (`Graph.structuralSuccessorBaseInstalled` progress) (StructuralBase.structuralBaseEvidence coordinator)
    recordFor admission = Projection.oracleViewHeraldAdmission admission (view state)
    ownHistory record = do
      captured <- Join.lookupCapture (admissionRecordId record) (admissionRecordAttempt record) (startupJoinState state)
      either (const Nothing) Just (History.decodeJoinSourceHistory captured)

reportMatches :: HeraldAdmissionRecord -> HeraldJoinReadyReport -> HeraldJoinSeal -> Bool
reportMatches record report seal = joinReadyAdmissionId report == admissionRecordId record && joinReadyAttempt report == admissionRecordAttempt record && joinReadySealDigest report == joinSealDigest seal && joinReadyControlPrefix report == joinSealControlPrefix seal && joinReadyRecipeDigest report == joinSealRecipeDigest seal

status :: HeraldState -> JoinStatus
status state =
  JoinStatus
    (Genesis.checkedSystemId genesis)
    (HeraldMember (Genesis.checkedLocalHeraldId genesis) (Genesis.checkedLocalHeraldEpoch genesis))
    (Projection.oracleViewCurrentHeraldMembership (controlView state))
    (Projection.oracleViewControlIndex (controlView state))
    (Projection.oracleViewControlIndex (view state))
    (Projection.projectedHeraldAdmissions (startupControlOracleProjectionState state))
    (if fenced then Nothing else either (const Nothing) Just (Graph.structuralAdmissionAnchorClaim (structuralReconciliationViews state) (startupStructuralProgressState state)))
    (not fenced && Projection.oracleViewLocalHeraldIsCurrent (controlView state) && not (Graph.structuralProgressIsJoiningObserver (startupStructuralProgressState state)))
  where
    genesis = startupGenesis state
    fenced = Isolation.isolationWitnessPhase (Isolation.stateWitness (startupIsolationState state)) `elem` [Isolation.IsolationReadOnlyDrainView, Isolation.IsolationTerminalView]

view :: HeraldState -> Projection.View
view = Projection.oracleView . startupOracleProjectionState

controlView :: HeraldState -> Projection.View
controlView = Projection.oracleView . startupControlOracleProjectionState
