-- | Atomic coordination of the Oracle client and contiguous projection.
--
-- Every fresh projection entry also releases the exact receiver prerequisite
-- in the same whole-Herald successor.
module Eclips.Herald.UseCase.OracleAdvance
  ( applyOracleIngress,
    applyFencedOracleIngress,
    structuralReconciliationViews,
    refreshOracleRoutingFromRegistrations,
    applyJoinControlHistory,
    advanceLocalLabelWork,
    applyResidentEnd,
    applyResidentEndWithUnavailableReason,
    preserveResidentEndForIsolation,
    releaseControlIndexDependencies,
    cutOracleClientForDrain,
  )
where

import Control.Monad (foldM, unless)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding (decodeUtf8)
import Eclips.Application.Types.Label (LabelResult (..))
import Eclips.Domain.Disappearance (DisappearanceProbeId, DisappearanceSubjectView (..), disappearanceSubjectView)
import Eclips.Domain.Identity
  ( ControlIndex,
    GlobalObjectId,
    HeraldEpoch,
    LabelDecisionId,
    ProcessEpochId,
    controlIndex,
    controlIndexWord64,
    labelAuthorityEpoch,
  )
import Eclips.Domain.Label
  ( LabelOutcomeDigest,
    PreparedLabelDigest,
    PreparedLabelFacts,
    ReleasedLabelState,
    ReleasedLabelStateView (ReleasedDeletedView, ReleasedLabelView),
    labelRecordReleasedState,
    labelRecordRetainedAuthority,
    labelRecordRevision,
    preparedLabelFactsObjectId,
    releasedLabelStateView,
  )
import Eclips.Domain.Membership
  ( HeraldAdmissionId,
    HeraldMembershipGeneration,
    heraldMembershipGenerationAdmissionId,
    heraldMembershipGenerationId,
    heraldMembershipGenerationRetiredHeraldEpoch,
  )
import Eclips.Domain.ProcessLifecycle
  ( ProcessEndReason,
    processEndReasonIsHeraldRetirement,
  )
import Eclips.Domain.ProcessStart (processStartProcessEpochId)
import Eclips.Domain.Sort.Profile (profileCatalogueDigest)
import Eclips.Domain.StructuralConsequence
  ( StructuralConsequenceCause,
    labelReleaseCause,
    processEndCause,
  )
import Eclips.Domain.Value (LabelOwner (ProcessLabel))
import Eclips.Herald.Administration (AdminReply (RetainedAdminResult))
import Eclips.Herald.Administration qualified as AdministrationValue
import Eclips.Herald.Administration.State qualified as Administration
import Eclips.Herald.Alignment.Loss qualified as AlignmentLoss
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Application.Request
  ( ApplicationRequestReply (RetainedRequestReply),
    RetainedRequestReplyBody (Completed),
  )
import Eclips.Herald.Application.Session
  ( ApplicationSessionUnavailableReason (ApplicationSessionNoLongerLive),
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Authority qualified as Authority
import Eclips.Herald.Bootstrap qualified as Bootstrap
import Eclips.Herald.ConfiguredProcess.Start qualified as ConfiguredStart
import Eclips.Herald.ConfiguredProcess.State qualified as Configured
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Disappearance.Protocol qualified as DisappearanceProtocol
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.Discovery
  ( peerBindingRemoteHeraldEpoch,
    peerDialIntentHeraldEpoch,
  )
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    PeerProtocolDisposition (ClosePeerPublicationProtocol),
    effectBatchMembers,
    orderedEffectBatch,
  )
import Eclips.Herald.Genesis.Internal (checkedLocalHeraldEpoch, checkedStartupAdmission)
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.Graph.TerminalSource qualified as TerminalSource
import Eclips.Herald.Input
  ( PeerControl (..),
  )
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.Join.Base (checkedJoinBase)
import Eclips.Herald.LabelBarrier.State qualified as LabelBarrier
import Eclips.Herald.OracleClient
  ( OracleBinding,
    OracleClientAction
      ( AssociateOracleSubmissionRetry,
        CancelOracleRetry,
        ReleaseDeferredOracleSubmission,
        ReportOracleClientDiagnostic,
        ScheduleOracleProgress,
        ScheduleOracleRetry,
        SubmitOracleProgress,
        SubmitOracleRequest
      ),
    OracleClientDiagnostic (StaleOracleClientIngress),
    OracleClientIngress (..),
    OracleRetryPurpose (OracleSubmissionRetry),
  )
import Eclips.Herald.OracleClient qualified as OracleContact
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.PeerLiveness.State qualified as PeerLiveness
import Eclips.Herald.PeerStream
  ( streamDirectionDestination,
    streamDirectionSource,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement
  ( PlacementUpdate (FullPlacementSnapshot),
    deltaRouteDelta,
    deltaRouteStoreIncarnation,
  )
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.ProcessPreparation.State qualified as ProcessPreparation
import Eclips.Herald.Publication.Groups qualified as PublicationGroups
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (HeraldTransitionInvariant),
    HeraldTransitionInvariantViolation (HeraldOracleTransitionContradiction),
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
    advanceStartupStructuralControlProgress,
    replaceStartupAdministrationState,
    replaceStartupAlignmentCutQueryCache,
    replaceStartupAlignmentState,
    replaceStartupApplicationState,
    replaceStartupConfiguredProcessState,
    replaceStartupControlledState,
    replaceStartupDisappearanceState,
    replaceStartupDiscoveryState,
    replaceStartupGraphState,
    replaceStartupIdGeneratorState,
    replaceStartupIsolationState,
    replaceStartupLabelBarrierState,
    replaceStartupLabelPatchState,
    replaceStartupOracleClientState,
    replaceStartupOracleProjectionState,
    replaceStartupPeerLivenessState,
    replaceStartupPeerStreamState,
    replaceStartupPlacementState,
    replaceStartupProcessPreparationState,
    replaceStartupPublicationState,
    replaceStartupSortRegistryState,
    replaceStartupStoreState,
    replaceStartupStructuralBaseCoordinator,
    replaceStartupStructuralProgressState,
    replaceStartupWaitState,
    startupAdministrationState,
    startupAlignmentCutQueryCache,
    startupAlignmentState,
    startupApplicationState,
    startupConfiguredProcessState,
    startupControlOracleProjectionState,
    startupControlledState,
    startupDiagnosticChecks,
    startupDisappearanceState,
    startupDiscoveryState,
    startupGenesis,
    startupGraphState,
    startupIdGeneratorState,
    startupIsolationState,
    startupLabelBarrierState,
    startupLabelPatchState,
    startupLastObservedTime,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPeerLivenessState,
    startupPeerStreamState,
    startupPlacementState,
    startupProcessPreparationState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralProgressState,
    startupWaitState,
  )
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Debt (sortOccurrence)
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.UseCase.AlignmentTransfer qualified as AlignmentTransfer
import Eclips.Herald.UseCase.ControlReclamation qualified as ControlReclamation
import Eclips.Herald.UseCase.ControlledRemoval qualified as ControlledRemoval
import Eclips.Herald.UseCase.Disappearance qualified as DisappearanceReplay
import Eclips.Herald.UseCase.DisappearanceLive qualified as DisappearanceLive
import Eclips.Herald.UseCase.FailureDetection qualified as FailureDetection
import Eclips.Herald.UseCase.JoinControlTails qualified as JoinControlTails
import Eclips.Herald.UseCase.LabelCollection qualified as LabelCollection
import Eclips.Herald.UseCase.LabelPatch qualified as LabelPatch
import Eclips.Herald.UseCase.PeerInput qualified as PeerInput
import Eclips.Herald.UseCase.StructuralCoordinator qualified as StructuralCoordinator
import Eclips.Herald.UseCase.StructuralSettlement qualified as StructuralSettlement
import Eclips.Herald.UseCase.TerminalStructuralArchive qualified as TerminalStructuralArchive
import Eclips.Herald.UseCase.TerminalStructuralStart qualified as TerminalStructuralStart
import Eclips.Herald.Wait.State qualified as Wait
import Eclips.Oracle.Canonical
  ( CanonicalAppliedOracleEntry,
    canonicalAppliedOracleEntryValue,
    canonicalizeOracleReceipt,
  )
import Eclips.Oracle.Disappearance qualified as OracleDisappearance
import Eclips.Oracle.Genesis (raftVoterBindingHeraldEpoch, raftVoterBindingNode)
import Eclips.Oracle.Identity (OracleClientRequestId)
import Eclips.Oracle.Label
  ( LiveTerminalOutcomeView (..),
    ReleasedLabelOverlay,
    labelCompletionDecisionId,
    liveDecisionId,
    liveDecisionMemberSetDigest,
    liveDecisionObject,
    liveDecisionRequestControlIndex,
    liveTerminalOutcomeView,
    releasedLabelOverlayRetainedAuthority,
    releasedLabelOverlayRevision,
    releasedLabelOverlayState,
  )
import Eclips.Oracle.Projection
  ( OracleProjectionEvent,
    OracleProjectionEventView (..),
    ProcessOriginView (DynamicStartView),
    appliedEntryCommand,
    appliedEntryControlIndex,
    appliedEntryProjectionEvents,
    appliedEntryReceipt,
    appliedEntryRequestId,
    oracleProjectionEventView,
    processRecordOrigin,
    processRecordProcessEpoch,
    processRecordProcessId,
    processRecordResidence,
  )
import Eclips.Oracle.Voter qualified as Voter
import Eclips.Raft.Identity (raftNodeIdBytes)

applyOracleIngress ::
  OracleClientIngress ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyOracleIngress ingress predecessor = case ingress of
  OracleEntriesReceived binding entries ->
    applyEntries binding entries predecessor
  OracleSubmissionDeferredReceived binding request decision observedIndex ->
    applySubmissionDeferral binding request decision observedIndex predecessor
  _ ->
    case OracleClient.prepareClientIngress ingress client of
      Left _ -> oracleFault
      Right prepared ->
        checkedOracleSuccess
          ( replaceStartupOracleClientState
              (OracleClient.commitClientIngress prepared)
              predecessor
          )
          (actionBatch (OracleClient.preparedClientIngressActions prepared))
  where
    client = startupOracleClientState predecessor

-- | A fenced owner advances its immutable control cache and client evidence.
-- The semantic projection remains frozen at the fence; no application, graph,
-- failure, label, membership, or publication driver runs on this path.
applyFencedOracleIngress :: OracleClientIngress -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
applyFencedOracleIngress ingress predecessor = case ingress of
  OracleEntriesReceived binding entries
    | not (OracleClient.oracleClientBindingIsCurrent binding initialClient) -> Right (predecessor, actionBatch [ReportOracleClientDiagnostic StaleOracleClientIngress])
    | not (batchIsContiguous entries) -> oracleFault
    | otherwise -> do
        result <- foldControlEntries (NonEmpty.toList entries) predecessor
        case result of
          Nothing -> do
            prepared <- mapClientProblem (OracleClient.prepareRewatch initialClient)
            Right (predecessor, actionBatch (controlActions (OracleClient.preparedClientIngressActions prepared)))
          Just successor -> do
            progress <- mapClientProblem (OracleClient.prepareCommittedPrefixProgress (OracleClient.oracleClientAppliedCursor initialClient) (startupOracleClientState successor))
            client <- mapClientProblem (compactCompletedClient (OracleClient.commitClientIngress progress))
            Right (replaceStartupOracleClientState client successor, actionBatch (controlActions (OracleClient.preparedClientIngressActions progress <> OracleClient.oracleClientActions client)))
  OracleSubmissionDeferredReceived binding request decision observedIndex -> do
    (successor, effects) <- applySubmissionDeferral binding request decision observedIndex predecessor
    Right (successor, orderedEffectBatch [effect | effect@(RunOracleClientAction action) <- effectBatchMembers effects, not (null (controlActions [action]))])
  _ -> do
    prepared <- mapClientProblem (OracleClient.prepareClientIngress ingress initialClient)
    Right (replaceStartupOracleClientState (OracleClient.commitClientIngress prepared) predecessor, actionBatch (controlActions (OracleClient.preparedClientIngressActions prepared)))
  where
    initialClient = startupOracleClientState predecessor
    controlActions = filter $ \case
      SubmitOracleRequest {} -> False
      SubmitOracleProgress {} -> False
      ScheduleOracleProgress {} -> False
      AssociateOracleSubmissionRetry {} -> False
      ScheduleOracleRetry OracleSubmissionRetry _ -> False
      _ -> True
    foldControlEntries [] state = Right (Just state)
    foldControlEntries (canonical : remaining) state = do
      prepared <- either (const oracleFault) Right (OracleProjection.prepareAppliedEntry canonical (startupControlOracleProjectionState state))
      membership <- membershipProjectionGeneration canonical
      let client = startupOracleClientState state
      case OracleProjection.preparedAppliedEntryClassification prepared of
        OracleProjection.AppliedEntryGap {} -> Right Nothing
        OracleProjection.AppliedEntryUnequalConflict {} -> oracleFault
        OracleProjection.AppliedEntryExactDuplicate index
          | cursorEvidenceMatches index canonical membership client -> foldControlEntries remaining state
          | otherwise -> oracleFault
        OracleProjection.AppliedEntryCoveredByBase index
          | coveredCursorEvidenceMatches index canonical client -> foldControlEntries remaining state
          | otherwise -> oracleFault
        OracleProjection.AppliedEntryNext _ -> do
          advancedClient <- case membership of
            Nothing -> OracleClient.commitCursorAdvance <$> mapClientProblem (OracleClient.prepareCursorAdvance canonical client)
            Just generation -> OracleClient.commitMembershipCursorAdvance <$> mapClientProblem (OracleClient.prepareCanonicalMembershipCursorAdvance canonical (heraldMembershipGenerationId generation) client)
          let projection = OracleProjection.commitAppliedEntry prepared
          retainedClient <-
            if OracleProjection.oracleViewLocalHeraldIsCurrent (OracleProjection.oracleView projection)
              then Right advancedClient
              else mapClientProblem (OracleClient.prepareDrain advancedClient)
          routedClient <- refreshOracleRouting (appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical)) (OracleProjection.oracleView projection) retainedClient
          let successor =
                foldl'
                  releaseControlTail
                  (replaceStartupOracleClientState routedClient . replaceStartupOracleProjectionState projection $ state)
                  (appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical))
          foldControlEntries remaining successor
    releaseControlTail state event = case oracleProjectionEventView event of
      HeraldAdmissionChangedView record -> JoinControlTails.releaseAdmission record state
      HeraldMembershipAdvancedView generation -> maybe state (`JoinControlTails.releaseRetired` state) (heraldMembershipGenerationRetiredHeraldEpoch generation)
      _ -> state

applySubmissionDeferral ::
  OracleBinding ->
  OracleClientRequestId ->
  LabelDecisionId ->
  ControlIndex ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applySubmissionDeferral binding request decision observedIndex predecessor
  | not (OracleClient.oracleClientBindingIsCurrent binding client) =
      checkedOracleSuccess
        predecessor
        (actionBatch [ReportOracleClientDiagnostic StaleOracleClientIngress])
  | otherwise = do
      prepared <-
        mapClientProblem
          ( OracleClient.prepareOracleSubmissionDeferral
              request
              decision
              observedIndex
              client
          )
      released <-
        releaseCompletedDeferrals
          (OracleClient.commitOracleSubmissionDeferral prepared)
          (startupOracleProjectionState predecessor)
      let successor = replaceStartupOracleClientState released predecessor
      checkedOracleSuccess
        successor
        ( actionBatch
            ( ReleaseDeferredOracleSubmission binding request
                : OracleClient.oracleClientRequestActions released
            )
        )
  where
    client = startupOracleClientState predecessor

applyEntries ::
  OracleBinding ->
  NonEmpty CanonicalAppliedOracleEntry ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyEntries binding entries predecessor
  | not (OracleClient.oracleClientBindingIsCurrent binding initialClient) =
      checkedOracleSuccess
        predecessor
        (actionBatch [ReportOracleClientDiagnostic StaleOracleClientIngress])
  | not (batchIsContiguous entries) = oracleFault
  | otherwise = do
      result <- foldEntries (NonEmpty.toList entries) predecessor []
      case result of
        EntryGap -> do
          prepared <- mapClientProblem (OracleClient.prepareRewatch initialClient)
          checkedOracleSuccess
            predecessor
            (actionBatch (OracleClient.preparedClientIngressActions prepared))
        EntryApplied successor releaseEffects -> do
          preparedProgress <-
            mapClientProblem
              ( OracleClient.prepareCommittedPrefixProgress
                  (OracleClient.oracleClientAppliedCursor initialClient)
                  (startupOracleClientState successor)
              )
          (progressClient, releasedEvidence) <- mapClientProblem (OracleClient.compactCompletedBatch (OracleClient.commitClientIngress preparedProgress))
          let withProgress =
                replaceStartupOracleClientState progressClient successor
              cancellationEffects =
                fmap
                  RunOracleClientAction
                  (OracleClient.preparedClientIngressActions preparedProgress)
          reclaimed <-
            if OracleClient.oracleClientAppliedCursor initialClient /= OracleClient.oracleClientAppliedCursor progressClient
              || OracleClient.oracleClientCanonicalCoveredThrough initialClient /= OracleClient.oracleClientCanonicalCoveredThrough progressClient
              || releasedEvidence
              then ControlReclamation.reclaimControlPrefixOrFault withProgress
              else pure withProgress
          let retainedActions =
                fmap
                  RunOracleClientAction
                  (OracleClient.oracleClientActions (startupOracleClientState reclaimed))
          checkedOracleSuccess
            reclaimed
            ( orderedEffectBatch
                (cancellationEffects <> releaseEffects <> retainedActions)
            )
  where
    initialClient = startupOracleClientState predecessor

-- | The pending local identity receives canonical control history on its
-- restricted onboarding lane. No fictitious active Oracle binding is minted.
applyJoinControlHistory :: [CanonicalAppliedOracleEntry] -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
applyJoinControlHistory entries state
  | checkedStartupAdmission (startupGenesis state) == Nothing = oracleFault
  | otherwise = replay entries state []
  where
    replay [] successor effects = complete successor effects
    replay (entry : rest) predecessor effects
      | GraphProgress.structuralProgressIsJoiningObserver (startupStructuralProgressState predecessor)
          || OracleProjection.appliedEntryEvidence (appliedEntryControlIndex (canonicalAppliedOracleEntryValue entry)) (startupControlOracleProjectionState predecessor) == Just entry = do
          result <- foldEntries [entry] predecessor effects
          case result of
            EntryGap -> oracleFault
            EntryApplied successor emitted -> replay rest successor emitted
      -- Activation ends the callback's authority in this very input. Retain
      -- its complete successor/effects and let ordinary Oracle catch-up own a
      -- later unknown suffix; exact retries remain harmless and observable.
      | otherwise = complete predecessor effects
    complete successor effects = do
      client <- mapClientProblem (compactCompletedClient (startupOracleClientState successor))
      Right (replaceStartupOracleClientState client successor, orderedEffectBatch effects)

-- All local consumers and projected request results have finished this batch.
-- The label frontier releases request witnesses independently of the retained
-- exact control archive; receipt acknowledgement alone cannot release them.
compactCompletedClient :: OracleClient.State -> Either OracleClient.OracleClientProblem OracleClient.State
compactCompletedClient = fmap fst . OracleClient.compactCompletedBatch

data EntryFold
  = EntryGap
  | EntryApplied HeraldState [HeraldEffect]

foldEntries ::
  [CanonicalAppliedOracleEntry] ->
  HeraldState ->
  [HeraldEffect] ->
  Either HeraldInvariantFault EntryFold
foldEntries [] state releaseEffects = Right (EntryApplied state releaseEffects)
foldEntries (canonical : remaining) state releaseEffects =
  case OracleProjection.prepareAppliedEntry canonical projection of
    Left _ -> oracleFault
    Right prepared ->
      case OracleProjection.preparedAppliedEntryClassification prepared of
        OracleProjection.AppliedEntryGap {} -> Right EntryGap
        OracleProjection.AppliedEntryUnequalConflict {} -> oracleFault
        OracleProjection.AppliedEntryExactDuplicate index -> do
          membership <- membershipProjectionGeneration canonical
          if cursorEvidenceMatches index canonical membership client
            then foldEntries remaining state releaseEffects
            else oracleFault
        OracleProjection.AppliedEntryCoveredByBase index ->
          if coveredCursorEvidenceMatches index canonical client
            then foldEntries remaining state releaseEffects
            else oracleFault
        OracleProjection.AppliedEntryNext _ -> do
          -- Preserve the source-before-control order captured when an
          -- application call was accepted.  In particular, Process End must
          -- not retire the source authority before already-ready stages with
          -- the preceding control prerequisite have entered structural
          -- history.  Their ordinary End refresh below then projects Zombie
          -- with the exact control cause, independent of runtime scheduling.
          (withPriorStructural, priorStructuralEffects) <-
            if entryEndsProcess canonical
              then
                mapStructuralProblem
                  (StructuralCoordinator.advanceLocalStructuralWork state)
              else Right (state, mempty)
          membership <- membershipProjectionGeneration canonical
          advancedClient <- case membership of
            Nothing ->
              OracleClient.commitCursorAdvance
                <$> mapClientProblem
                  (OracleClient.prepareCursorAdvance canonical client)
            Just generation ->
              OracleClient.commitMembershipCursorAdvance
                <$> mapClientProblem
                  ( OracleClient.prepareCanonicalMembershipCursorAdvance
                      canonical
                      (heraldMembershipGenerationId generation)
                      client
                  )
          let advancedProjection = OracleProjection.commitAppliedEntry prepared
          releasedClient <-
            releaseCompletedDeferrals
              advancedClient
              advancedProjection
          let withOracle =
                replaceStartupApplicationState
                  ( Application.setApplicationMembershipGate
                      ( OracleProjection.oracleViewPendingHeraldAdmission (OracleProjection.oracleView advancedProjection) /= Nothing
                          || ( checkedStartupAdmission (startupGenesis withPriorStructural) /= Nothing
                                 && not (OracleProjection.oracleViewLocalHeraldIsCurrent (OracleProjection.oracleView advancedProjection))
                             )
                      )
                      (startupApplicationState withPriorStructural)
                  )
                  . replaceStartupOracleClientState releasedClient
                  . replaceStartupOracleProjectionState advancedProjection
                  $ withPriorStructural
          (withMembership, membershipEffects) <-
            applyMembershipProjectionEvents
              (appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical))
              (notifyProjectedPreparationProcesses (appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical)) withOracle)
          (withVoters, voterEffects) <-
            applyOracleVoterProjectionEvents
              (appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical))
              withMembership
          withConfiguredStart <- settleConfiguredStartEntry canonical withVoters
          withAdministrationEnd <- settleAdministrationEndEntry canonical withConfiguredStart
          withSettledLabelRequest <-
            settleLocalOracleRequestEntry canonical withAdministrationEnd
          withSettledFailureRequest <-
            FailureDetection.settleFailureOracleRequestEntry
              canonical
              withSettledLabelRequest
          (withLabelProjection, labelProjectionEffects) <-
            applyLabelProjectionEvents
              (appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical))
              withSettledFailureRequest
          (withRetirement, retirementEffects) <-
            applyResidentEndEvents
              (appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical))
              withLabelProjection
          withReadyProcesses <-
            either
              (const oracleFault)
              Right
              (StructuralSettlement.settleReadyProcessStarts withRetirement)
          withControlProgress <-
            mapProgressProblem
              ( advanceStartupStructuralControlProgress
                  (appliedEntryControlIndex (canonicalAppliedOracleEntryValue canonical))
                  withReadyProcesses
              )
          let administrationEffects =
                administrationResultChangeEffects withPriorStructural withControlProgress
          (withReleasedDependencies, dependencyEffects) <-
            releaseControlIndexDependencies withControlProgress
          (successor, structuralBaseEffects) <-
            beginTerminalStructuralAfterMembership
              membership
              withPriorStructural
              withReleasedDependencies
          foldEntries
            remaining
            successor
            ( releaseEffects
                <> effectBatchMembers priorStructuralEffects
                <> administrationEffects
                <> labelProjectionEffects
                <> membershipEffects
                <> voterEffects
                <> retirementEffects
                <> effectBatchMembers dependencyEffects
                <> effectBatchMembers structuralBaseEffects
            )
  where
    projection = startupOracleProjectionState state
    client = startupOracleClientState state

-- | Refresh current role/routing projections only for a new registration,
-- intention, or configuration event. Retained watch history is never rescanned.
applyOracleVoterProjectionEvents :: [OracleProjectionEvent] -> HeraldState -> Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyOracleVoterProjectionEvents events predecessor
  | not (any isVoterEvent events) = Right (predecessor, [])
  | otherwise = do
      let view = OracleProjection.oracleView (startupOracleProjectionState predecessor)
          configuration = OracleProjection.oracleViewVoterConfiguration view
          priorQuorums = Isolation.isolationWitnessVoterQuorums (Isolation.stateWitness (startupIsolationState predecessor))
          quorums = maybe priorQuorums configurationHosts configuration
          (oldHosts, newHosts) = quorums
          pending = OracleProjection.oracleViewPendingVoterChange view /= Nothing
      preferred <- refreshOracleRouting events view (startupOracleClientState predecessor)
      let established = Set.empty
      (isolation, isolationDisposition) <-
        either
          (const oracleFault)
          Right
          (Isolation.observeVoterConfiguration (startupLastObservedTime predecessor) oldHosts newHosts established (startupIsolationState predecessor))
      (successor, failureEffects) <-
        FailureDetection.observeVoterConfiguration
          oldHosts
          newHosts
          pending
          (replaceStartupOracleClientState preferred . replaceStartupIsolationState isolation $ predecessor)
      let isolationEffects
            | GraphProgress.structuralProgressIsJoiningObserver (startupStructuralProgressState predecessor) = []
            | otherwise = case isolationDisposition of
                Isolation.IsolationReachabilityUnchanged -> []
                Isolation.IsolationGraceCancelled attempt -> [CancelTimer attempt]
                Isolation.IsolationGraceStarted attempt specification -> [ArmTimer attempt specification]
      Right (successor, isolationEffects <> effectBatchMembers failureEffects)
  where
    isVoterEvent event = case oracleProjectionEventView event of
      OracleReplicaRegisteredView {} -> True
      OracleVoterChangeChangedView {} -> True
      OracleVoterConfigurationChangedView {} -> True
      _ -> False
    hosts = Set.fromList . map raftVoterBindingHeraldEpoch . Voter.oracleVoterBindingList
    configurationHosts configuration = case Voter.voterConfigurationView configuration of
      Voter.StableVoterConfigurationView bindings -> (hosts bindings, Nothing)
      Voter.JointVoterConfigurationView old new -> (hosts old, Just (hosts new))

-- | Routing is a projection-only operation, also usable by a fenced control
-- shell without updating any semantic owner or starting a semantic driver.
refreshOracleRouting :: [OracleProjectionEvent] -> OracleProjection.View -> OracleClient.State -> Either HeraldInvariantFault OracleClient.State
refreshOracleRouting events = refreshOracleRoutingFromRegistrations [registration | event <- events, OracleReplicaRegisteredView registration <- [oracleProjectionEventView event]]

-- | The same routing projection admits a checked base's current registration
-- catalogue without manufacturing historical projection events.
refreshOracleRoutingFromRegistrations :: [Voter.OracleReplicaRegistration] -> OracleProjection.View -> OracleClient.State -> Either HeraldInvariantFault OracleClient.State
refreshOracleRoutingFromRegistrations registrations view initial = do
  hinted <- foldM addRegistrationHints initial registrations
  case OracleProjection.oracleViewVoterConfiguration view of
    Nothing -> Right hinted
    Just configuration -> do
      nodes <- traverse (either (const oracleFault) Right . OracleContact.oracleNodeClaim . raftNodeIdBytes . raftVoterBindingNode) (Voter.voterConfigurationBindings configuration)
      Right (OracleClient.preferOracleVoters (Set.fromList nodes) hinted)
  where
    addRegistrationHints client registration = case Voter.replicaRegistrationContact registration of
      Nothing -> Right client
      Just contact -> do
        node <- either (const oracleFault) Right (OracleContact.oracleNodeClaim (raftNodeIdBytes (Voter.replicaRegistrationNode registration)))
        let endpoint = Voter.replicaContactOracle contact
        route <- either (const oracleFault) Right (OracleContact.oracleContact node (decodeUtf8 (Voter.replicaEndpointHost endpoint)) (Voter.replicaEndpointPort endpoint))
        contacts <- either (const oracleFault) Right (OracleContact.oracleContactSet (route :| []))
        Right (OracleClient.observeOracleContactHints contacts client)

membershipProjectionGeneration ::
  CanonicalAppliedOracleEntry ->
  Either HeraldInvariantFault (Maybe HeraldMembershipGeneration)
membershipProjectionGeneration canonical =
  case [ generation
       | event <-
           appliedEntryProjectionEvents
             (canonicalAppliedOracleEntryValue canonical),
         HeraldMembershipAdvancedView generation <- [oracleProjectionEventView event]
       ] of
    [] -> Right Nothing
    [generation] -> Right (Just generation)
    _ -> oracleFault

entryEndsProcess :: CanonicalAppliedOracleEntry -> Bool
entryEndsProcess canonical =
  any
    ( \event -> case oracleProjectionEventView event of
        ProcessEpochEndedEventView {} -> True
        _ -> False
    )
    (appliedEntryProjectionEvents (canonicalAppliedOracleEntryValue canonical))

cursorEvidenceMatches ::
  ControlIndex ->
  CanonicalAppliedOracleEntry ->
  Maybe HeraldMembershipGeneration ->
  OracleClient.State ->
  Bool
cursorEvidenceMatches index canonical membership client =
  case membership of
    Nothing -> OracleClient.oracleClientMatchesProjectedEntry canonical client
    Just generation ->
      OracleClient.oracleClientMatchesProjectedEntry canonical client
        && OracleClient.oracleClientMembershipAdvance index client
          == Just (heraldMembershipGenerationId generation)

-- Covered traffic is a no-op, not exact receipt or result evidence. Both owners
-- must cover the coordinate, and any still-pinned Client bytes must agree.
coveredCursorEvidenceMatches :: ControlIndex -> CanonicalAppliedOracleEntry -> OracleClient.State -> Bool
coveredCursorEvidenceMatches index canonical client =
  index <= OracleClient.oracleClientCanonicalCoveredThrough client
    && OracleClient.oracleClientMatchesCoveredEntry canonical client

-- | Release work whose sole missing prerequisite was the contiguous Oracle
-- control prefix.  Route-cutover markers are included because a membership
-- contraction can make a retired source terminal without producing any later
-- alignment input to drive them again.
releaseControlIndexDependencies ::
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
releaseControlIndexDependencies withControlProgress = do
  (disappearanceOwners, disappearanceRelease) <-
    mapPeerProblem
      (PeerInput.releasePendingDisappearanceProbeMarkers (peerInputContextFor withControlProgress) (peerInputStateFor withControlProgress))
  -- Marker release owns only these two leaves. Reinstalling untouched graph,
  -- sort, placement and progress owners would discard their derived cut views.
  let withDisappearanceMarkers =
        replaceStartupDisappearanceState (PeerInput.peerInputDisappearanceState disappearanceOwners)
          . replaceStartupPeerStreamState (PeerInput.peerInputPeerStreamState disappearanceOwners)
          $ withControlProgress
      disappearanceMarkerEffects = peerDependencyReleaseEffects withDisappearanceMarkers disappearanceRelease
  (releasedOwners, released) <-
    mapPeerProblem
      ( PeerInput.releasePeerControl
          (peerInputContextFor withDisappearanceMarkers)
          (peerInputStateFor withDisappearanceMarkers)
      )
  let installedOwners = installPeerInputState releasedOwners withDisappearanceMarkers
      -- Every resolved publication (including a held reclassification) enters
      -- the release result. With none, release can change receipt, wait and
      -- disappearance bookkeeping only, never cut-query inputs.
      releasedState
        | null (PeerInput.peerControlReleasedPublications released) =
            let cache = startupAlignmentCutQueryCache withDisappearanceMarkers
             in cache `seq` replaceStartupAlignmentCutQueryCache cache installedOwners
        | otherwise = installedOwners
      peerEffects = peerReleaseEffects releasedState released
  (successor, structuralEffects) <-
    mapStructuralProblem
      (StructuralCoordinator.advanceLocalStructuralWork releasedState)
  (withRouteCutovers, routeCutoverEffects) <-
    mapStructuralProblem
      (StructuralCoordinator.releasePendingAlignmentRouteCutovers successor)
  Right
    ( withRouteCutovers,
      orderedEffectBatch
        ( disappearanceMarkerEffects
            <> peerEffects
            <> effectBatchMembers structuralEffects
            <> effectBatchMembers routeCutoverEffects
        )
    )

releaseCompletedDeferrals ::
  OracleClient.State ->
  OracleProjection.State ->
  Either HeraldInvariantFault OracleClient.State
releaseCompletedDeferrals client projection =
  mapClientProblem
    ( OracleClient.releaseCompletedOracleDeferrals
        ( \decision ->
            OracleProjection.oracleViewLabelWorkflowCompleted
              decision
              (OracleProjection.oracleView projection)
        )
        client
    )

settleConfiguredStartEntry ::
  CanonicalAppliedOracleEntry ->
  HeraldState ->
  Either HeraldInvariantFault HeraldState
settleConfiguredStartEntry canonical state =
  case matchingCorrelations of
    [] -> Right state
    [correlation] -> do
      prepared <-
        mapConfiguredStartProblem
          ( ConfiguredStart.prepareConfiguredStartOracleResult
              (startupGenesis state)
              correlation
              owners
          )
      let (successorOwners, outcome) =
            ConfiguredStart.commitConfiguredStartOracleResult prepared
          successor = installConfiguredStartOwners successorOwners state
      case outcome of
        ConfiguredStart.ConfiguredStartOracleRejected {} -> Right successor
        ConfiguredStart.ConfiguredStartProcessInstalled {} -> Right successor
        ConfiguredStart.ConfiguredStartOracleExactRetry {} -> Right successor
    _ -> oracleFault
  where
    request = appliedEntryRequestId <$> appliedEntryCommand (canonicalAppliedOracleEntryValue canonical)
    matchingCorrelations =
      [ correlation
      | (correlation, _, _, status) <-
          Administration.configuredStartEntries (startupAdministrationState state),
        Just intention <- [configuredStartIntention status],
        Just
          ( OracleClient.oracleRequestRefRequestId
              (Administration.startProcessIntentionOracleRequest intention)
          )
          == request
      ]
    owners =
      ConfiguredStart.configuredStartOwners
        (startupAdministrationState state)
        (startupOracleClientState state)
        (startupOracleProjectionState state)
        (startupConfiguredProcessState state)
        (startupIdGeneratorState state)

configuredStartIntention ::
  Administration.ConfiguredStartStatus ->
  Maybe Administration.StartProcessEpochIntention
configuredStartIntention = \case
  Administration.ConfiguredStartGated -> Nothing
  Administration.ConfiguredStartAwaitingOracle intention -> Just intention
  Administration.ConfiguredStartOracleRejected intention _ _ -> Just intention
  Administration.ConfiguredStartProcessInstalled intention _ _ -> Just intention
  Administration.ConfiguredStartReady intention _ _ _ -> Just intention

settleAdministrationEndEntry ::
  CanonicalAppliedOracleEntry ->
  HeraldState ->
  Either HeraldInvariantFault HeraldState
settleAdministrationEndEntry canonical state = case matchingCorrelations of
  [] -> Right state
  [(correlation, intention)] -> do
    preparedClient <-
      mapClientProblem
        ( OracleClient.prepareOracleResult
            (Administration.endProcessIntentionOracleRequest intention)
            canonical
            (startupOracleClientState state)
        )
    let (oracleClient, _, result) =
          OracleClient.commitOracleResult preparedClient
        index = appliedEntryControlIndex (canonicalAppliedOracleEntryValue canonical)
    settlement <- case result of
      OracleClient.OracleRequestRejected rejection ->
        Right
          ( Administration.EndProcessOracleWasRejected
              (Administration.endProcessIntentionOracleRequest intention)
              index
              rejection
          )
      OracleClient.OracleRequestEnded process endIndex reason ->
        Right
          ( Administration.EndProcessOracleWasAccepted
              (Administration.endProcessIntentionOracleRequest intention)
              endIndex
              process
              reason
          )
      _ -> oracleFault
    preparedAdministration <-
      either
        (const oracleFault)
        Right
        ( Administration.prepareEndProcessSettlement
            correlation
            settlement
            (startupAdministrationState state)
        )
    let (administration, _) =
          Administration.commitEndProcessSettlement preparedAdministration
    Right
      ( replaceStartupOracleClientState oracleClient
          . replaceStartupAdministrationState administration
          $ state
      )
  _ -> oracleFault
  where
    request = appliedEntryRequestId <$> appliedEntryCommand (canonicalAppliedOracleEntryValue canonical)
    matchingCorrelations =
      [ (correlation, intention)
      | (correlation, _, status) <-
          Administration.endProcessEpochEntries (startupAdministrationState state),
        Just intention <- [endProcessIntention status],
        Just
          ( OracleClient.oracleRequestRefRequestId
              (Administration.endProcessIntentionOracleRequest intention)
          )
          == request
      ]

endProcessIntention ::
  Administration.EndProcessEpochStatus ->
  Maybe Administration.EndProcessEpochIntention
endProcessIntention = \case
  Administration.EndProcessAwaitingOracle intention -> Just intention
  Administration.EndProcessOracleRejected intention _ _ -> Just intention
  Administration.EndProcessAwaitingRetirement intention _ _ _ -> Just intention
  Administration.EndProcessCompleted intention _ _ _ -> Just intention

-- | Settle the exact local request named by this entry and notify its
-- preparation consumers. Their phase/observation work remains at its original
-- later stage; projection alone does not retire those consumers.
settleLocalOracleRequestEntry ::
  CanonicalAppliedOracleEntry ->
  HeraldState ->
  Either HeraldInvariantFault HeraldState
settleLocalOracleRequestEntry canonical state =
  case request >>= (\identifier -> OracleClient.lookupOracleRequestById identifier (startupOracleClientState state)) of
    Nothing -> Right state
    Just witness -> do
      let reference = OracleClient.oracleRequestWitnessRef witness
      prepared <-
        mapClientProblem
          ( OracleClient.prepareOracleResult
              reference
              canonical
              (startupOracleClientState state)
          )
      let (client, classification, result) = OracleClient.commitOracleResult prepared
          withClient =
            replaceStartupOracleClientState
              client
              ( if classification == OracleClient.OracleResultFirstRecorded
                  then
                    replaceStartupProcessPreparationState
                      (ProcessPreparation.notifyOracleResults (Set.singleton reference) (startupProcessPreparationState state))
                      state
                  else state
              )
      case (OracleClient.oracleRequestWitnessIntent witness, result) of
        (OracleClient.VoterAdministrationIntent {}, _) -> do
          command <- maybe oracleFault Right (appliedEntryCommand (canonicalAppliedOracleEntryValue canonical))
          retained <- either (const oracleFault) Right (Administration.settleVoterAdministration reference (canonicalizeOracleReceipt (appliedEntryReceipt command)) (startupAdministrationState withClient))
          Right (replaceStartupAdministrationState retained withClient)
        (OracleClient.DecideLabelIntent decision _ _ _ _ _ _ _ _, OracleClient.OracleRequestRejected _) ->
          rejectLocalOpen decision withClient
        (OracleClient.CompleteLabelDecisionIntent attestation, _) ->
          Right (LabelCollection.wakeInstallationCollection (labelCompletionDecisionId attestation) withClient)
        _ -> Right withClient
  where
    request = appliedEntryRequestId <$> appliedEntryCommand (canonicalAppliedOracleEntryValue canonical)

-- Remote Start facts also enable an outgoing preparation without any local
-- request witness. This runs only for fresh semantic entries, never duplicates
-- or the control-only continuation after fencing.
notifyProjectedPreparationProcesses :: [OracleProjectionEvent] -> HeraldState -> HeraldState
notifyProjectedPreparationProcesses events state
  | Set.null changed = state
  | otherwise =
      replaceStartupProcessPreparationState
        (ProcessPreparation.notifyPreparationDependencies changed (startupProcessPreparationState state))
        state
  where
    changed =
      Set.fromList
        [ ProcessPreparation.ProcessChanged process
        | event <- events,
          process <- case oracleProjectionEventView event of
            ProcessStartedView record -> [processRecordProcessEpoch record]
            ProcessEpochEndedEventView ended _ _ -> [ended]
            _ -> []
        ]

rejectLocalOpen ::
  LabelDecisionId ->
  HeraldState ->
  Either HeraldInvariantFault HeraldState
rejectLocalOpen decision state = do
  preparedTerminal <-
    mapApplicationProblem
      ( Application.prepareApplicationLabelTerminal
          decision
          LabelNotApplied
          (startupApplicationState state)
      )
  let applicationWithTerminal =
        Application.commitApplicationLabelTerminal preparedTerminal
  preparedCompletion <-
    mapApplicationProblem
      ( Application.prepareApplicationLabelWorkflowCompletion
          decision
          applicationWithTerminal
      )
  let (application, _) =
        Application.commitApplicationLabelWorkflowCompletion preparedCompletion
  Right
    (replaceStartupApplicationState application state)

applyLabelProjectionEvents ::
  [OracleProjectionEvent] ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyLabelProjectionEvents events initial =
  foldl applyOne (Right (initial, [])) events
  where
    applyOne accumulated event = do
      (state, effects) <- accumulated
      observed <- case oracleProjectionEventView event of
        LabelDecidedView decision _ _ ->
          replaceStartupOracleClientState
            <$> mapClientProblem
              ( OracleClient.observeLabelDecision
                  (liveDecisionId decision)
                  (liveDecisionRequestControlIndex decision)
                  (startupOracleClientState state)
              )
            <*> pure state
        _ -> Right state
      (successor, eventEffects) <-
        applyLabelProjectionEvent (oracleProjectionEventView event) observed
      withCandidates <- applyDisappearanceLabelTerminal (oracleProjectionEventView event) successor
      consumed <- case oracleProjectionEventView event of
        LabelDecidedView decision terminal _ -> case liveTerminalOutcomeView terminal of
          LiveNotAppliedOutcomeView {} -> consume (liveDecisionId decision) withCandidates
          LiveReleasedOutcomeView {} -> Right withCandidates
        LabelWorkflowCompletedView decision _ -> consume decision withCandidates
        _ -> Right withCandidates
      Right (consumed, effects <> eventEffects)
    -- The promise follows every local semantic consumer, including application
    -- completion and disappearance work. An old semantic completion replay is
    -- harmless; its independent outstanding request pins remain client-owned.
    consume decision state = do
      client <- mapClientProblem (OracleClient.consumeLabelDecision decision (startupOracleClientState state))
      Right (replaceStartupOracleClientState client state)

applyDisappearanceLabelTerminal :: OracleProjectionEventView -> HeraldState -> Either HeraldInvariantFault HeraldState
applyDisappearanceLabelTerminal _ state | GraphProgress.structuralProgressIsJoiningObserver (startupStructuralProgressState state) = Right state
applyDisappearanceLabelTerminal event state = case event of
  LabelDecidedView decided outcome _ -> case liveTerminalOutcomeView outcome of
    LiveNotAppliedOutcomeView {} -> apply (liveDecisionId decided) DisappearanceProtocol.ProjectedLabelNotApplied
    LiveReleasedOutcomeView {} -> Right state
  LabelWorkflowCompletedView decision _ -> do
    workflow <- requireProjectedLabelWorkflow decision state
    case OracleProjection.projectedLabelWorkflowTerminal workflow of
      Just (OracleProjection.ProjectedLabelReleased _ overlay _) ->
        apply
          decision
          ( case releasedLabelStateView (releasedLabelOverlayState overlay) of
              ReleasedDeletedView {} -> DisappearanceProtocol.ProjectedLabelDeleted (releasedLabelOverlayRevision overlay)
              _ -> DisappearanceProtocol.ProjectedLabelReleased (releasedLabelOverlayRevision overlay)
          )
      _ -> oracleFault
  _ -> Right state
  where
    apply decision outcome = do
      workflow <- requireProjectedLabelWorkflow decision state
      let object = liveDecisionObject (OracleProjection.projectedLabelWorkflowDecision workflow)
      prepared <- either (const oracleFault) Right (Disappearance.prepareProjectedLabelTerminal (DisappearanceProtocol.projectedLabelTerminal decision object outcome) (startupDisappearanceState state))
      let (leaf, _) = Disappearance.commitDisappearanceTransition prepared
      Right (replaceStartupDisappearanceState leaf state)

-- | Exhaustive eliminator for the live Oracle event vocabulary. Process
-- lifecycle effects remain in their existing owners; every label arm is
-- applied here in the exact order in which Oracle emitted it.
applyLabelProjectionEvent ::
  OracleProjectionEventView ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyLabelProjectionEvent eventView state
  | GraphProgress.structuralProgressIsJoiningObserver (startupStructuralProgressState state),
    observerDisappearanceEvent eventView =
      case eventView of
        DisappearanceProbeResolvedView probe _ -> do
          projected <- maybe oracleFault Right (OracleProjection.oracleViewDisappearanceProbe probe (OracleProjection.oracleView (startupOracleProjectionState state)))
          (successor, _) <- either (const oracleFault) Right (DisappearanceReplay.replayProjectedDisappearanceResolution projected state)
          Right (successor, [])
        _ -> Right (state, [])
applyLabelProjectionEvent eventView state = case eventView of
  OracleReplicaRegisteredView {} -> Right (state, [])
  view@OracleVoterChangeChangedView {} -> FailureDetection.applyFailureProjectionEvent view state
  OracleVoterConfigurationChangedView {} -> Right (state, [])
  HeraldAdmissionChangedView {} -> Right (state, [])
  DisappearanceProbeOpenedView header _ -> do
    projected <-
      either
        (const oracleFault)
        Right
        ( DisappearanceProtocol.projectedDisappearanceProbe
            (OracleDisappearance.projectedDisappearanceProbeHeaderId header)
            (OracleDisappearance.projectedDisappearanceProbeHeaderSubject header)
            (OracleDisappearance.projectedDisappearanceProbeHeaderCoordinate header)
            (OracleDisappearance.projectedDisappearanceProbeHeaderMembership header)
        )
    (successor, effects) <- DisappearanceLive.applyDisappearanceOpen projected state
    Right (successor, effectBatchMembers effects)
  PredefinedAbsenceReportedView claim -> do
    successor <- DisappearanceLive.applyDisappearanceReport claim state
    Right (successor, [])
  DisappearanceProbeInvalidatedView probe _ -> applyProjectedDisappearanceTerminal probe state
  DisappearanceProbeResolvedView probe _ -> applyProjectedDisappearanceTerminal probe state
  DisappearanceProbeAbortedView probe _ -> applyProjectedDisappearanceTerminal probe state
  ProcessStartedView record -> do
    -- Start establishes the initial process label at this Oracle control cut.
    -- The same projected process fact is available at every active Herald;
    -- no writer or reader roots are installed by a dynamic Start.
    startIndex <- case processRecordOrigin record of
      DynamicStartView index _ -> Right index
      _ -> oracleFault
    prepared <-
      either
        (const oracleFault)
        Right
        ( Controlled.prepareControlledBootstrap
            ( Controlled.processFact
                (processRecordProcessId record)
                (processRecordProcessEpoch record)
                (processRecordResidence record)
                (labelAuthorityEpoch startIndex)
            )
            []
            (startupControlledState state)
        )
    Right (replaceStartupControlledState (Controlled.commitControlledBootstrap prepared) state, [])
  ProcessEpochEndedEventView {} -> Right (state, [])
  LabelDecidedView decided outcome digest -> case liveTerminalOutcomeView outcome of
    LiveNotAppliedOutcomeView decision _ _ _ _ _ -> do
      application <- terminalApplicationIfOwned decision LabelNotApplied state
      completeLocalApplication decision (replaceStartupApplicationState application state)
    LiveReleasedOutcomeView facts preparedDigest index overlay ->
      applyReleasedLabel (liveDecisionId decided) facts preparedDigest index overlay digest state
  LabelWorkflowCompletedView decision _ ->
    applyLabelWorkflowCompletion decision state
  view@OracleVoterHostFailureAcceptedView {} -> FailureDetection.applyFailureProjectionEvent view state
  view@OracleFailureProbeOpenedView {} ->
    FailureDetection.applyFailureProjectionEvent view state
  view@OracleFailureProbeReportRecordedView {} ->
    FailureDetection.applyFailureProjectionEvent view state
  view@OracleFailureProbeDismissedView {} ->
    FailureDetection.applyFailureProjectionEvent view state
  view@HeraldMembershipAdvancedView {} ->
    FailureDetection.applyFailureProjectionEvent view state
  view@OracleFailureProbeRetiredView {} ->
    FailureDetection.applyFailureProjectionEvent view state
  view@OracleFailureProbeSupersededView {} ->
    FailureDetection.applyFailureProjectionEvent view state

-- Historical evidence remains in the canonical Oracle projection. A joining
-- observer never acquires a captured-member write gate or emits probe reports.
observerDisappearanceEvent :: OracleProjectionEventView -> Bool
observerDisappearanceEvent event = case event of
  DisappearanceProbeOpenedView {} -> True
  PredefinedAbsenceReportedView {} -> True
  DisappearanceProbeInvalidatedView {} -> True
  DisappearanceProbeResolvedView {} -> True
  DisappearanceProbeAbortedView {} -> True
  _ -> False

-- The canonical Oracle projection is installed first, while this composition
-- applies the exact terminal to the local semantic owners before control-held
-- work is released or the successor is exposed.
applyProjectedDisappearanceTerminal :: DisappearanceProbeId -> HeraldState -> Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyProjectedDisappearanceTerminal probe state = do
  projected <- maybe oracleFault Right (OracleProjection.oracleViewDisappearanceProbe probe view)
  terminal <- maybe oracleFault Right (OracleProjection.projectedDisappearanceTerminal projected)
  translated <- case terminal of
    OracleProjection.ProjectedDisappearanceResolved outcome index ->
      Right (DisappearanceProtocol.ProjectedDisappearanceResolved probe outcome index)
    OracleProjection.ProjectedDisappearanceInvalidated cause index -> do
      invalidation <- case cause of
        OracleDisappearance.CommandDisappearanceInvalidation reporter reason witness ->
          Right
            ( DisappearanceProtocol.ProjectedCommandInvalidation
                reporter
                ( case reason of
                    OracleDisappearance.MatchingPublicationObserved -> DisappearanceProtocol.MatchingPublicationObserved
                    OracleDisappearance.EvidenceContradicted -> DisappearanceProtocol.LocalEvidenceContradicted
                )
                witness
            )
        OracleDisappearance.LabelOpenDisappearanceInvalidation decision ->
          case disappearanceSubjectView (OracleDisappearance.projectedDisappearanceProbeHeaderSubject (OracleProjection.projectedDisappearanceHeader projected)) of
            ControlledPredefinedSubjectView object _ _ -> Right (DisappearanceProtocol.ProjectedLabelInvalidation decision object)
            _ -> oracleFault
      Right (DisappearanceProtocol.ProjectedDisappearanceInvalidated probe invalidation index)
    OracleProjection.ProjectedDisappearanceAborted cause index ->
      Right
        ( DisappearanceProtocol.ProjectedDisappearanceAborted
            probe
            ( case cause of
                OracleDisappearance.ExplicitDisappearanceAbortReasonView _ -> DisappearanceProtocol.ProjectedAuthorizedAbort
                OracleDisappearance.AdmissionPreparingDisappearanceAbortView admission -> DisappearanceProtocol.ProjectedAdmissionPreparing admission
                OracleDisappearance.MembershipSupersededDisappearanceAbortView _ -> DisappearanceProtocol.ProjectedMembershipSuperseded (OracleProjection.oracleViewCurrentHeraldMembership view)
            )
            index
        )
  (successor, effects) <- DisappearanceLive.applyDisappearanceTerminal translated state
  Right (successor, effectBatchMembers effects)
  where
    view = OracleProjection.oracleView (startupOracleProjectionState state)

applyReleasedLabel ::
  LabelDecisionId ->
  PreparedLabelFacts ->
  PreparedLabelDigest ->
  ControlIndex ->
  ReleasedLabelOverlay ->
  LabelOutcomeDigest ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyReleasedLabel decision facts preparedDigest releaseIndex released outcomeDigest predecessor = do
  observed <-
    either
      (const oracleFault)
      Right
      ( Bootstrap.observeCanonicalBootstrapObjects
          (OracleProjection.oracleViewSystemId (OracleProjection.oracleView (startupOracleProjectionState predecessor)))
          (map snd (OracleProjection.projectedBootstraps (startupOracleProjectionState predecessor)))
          (Set.singleton (preparedLabelFactsObjectId facts))
          (startupControlledState predecessor)
      )
  let state = replaceStartupControlledState observed predecessor
      local = LabelBarrier.barrierLocalHerald (startupLabelBarrierState state)
  specification <-
    mapPatchContextProblem
      ( LabelPatch.checkedPatchSpecification
          local
          facts
          preparedDigest
          (startupStructuralProgressState state)
          (startupControlledState state)
          (startupSortRegistryState state)
          (startupPlacementState state)
          (startupStoreState state)
          (startupOracleProjectionState state)
      )
  preparedLocal <- mapPatchProblem (LabelPatch.prepareLabelPatch specification (startupIdGeneratorState state) (startupLabelPatchState state))
  let (preparedPatches, preparedGenerator) = LabelPatch.commitLabelPatch preparedLocal
  preparedPatch <-
    mapPatchProblem
      ( LabelPatch.preparePatchRelease
          specification
          releaseIndex
          preparedGenerator
          preparedPatches
      )
  recipe <- maybe oracleFault Right (LabelPatch.preparedReleasedPatchRecipe preparedPatch)
  let object = LabelPatch.labelPatchRecipeObject recipe
  firstAuthority <-
    case LabelPatch.targetRoleView (LabelPatch.labelPatchRecipeRole recipe) of
      LabelPatch.NablaTargetView _ _
        | Just _ <- Controlled.controlledReleasedLabelRecord object (startupControlledState state) -> Right Nothing
      LabelPatch.NablaTargetView nabla _ ->
        Just
          <$> either
            (const oracleFault)
            Right
            ( Authority.resolveInitialNablaAuthority
                nabla
                (startupStructuralProgressState state)
                (startupControlledState state)
            )
      _ -> Right Nothing
  preparedControlled <-
    mapControlledReleaseProblem
      ( Controlled.prepareControlledLabelReleaseWithAuthority
          ( OracleProjection.oracleViewInitialProjectionDigest
              (OracleProjection.oracleView (startupOracleProjectionState state))
          )
          firstAuthority
          facts
          releaseIndex
          (startupControlledState state)
      )
  let controlledWithRelease =
        Controlled.commitControlledLabelRelease preparedControlled
  controlledRecord <-
    maybe
      oracleFault
      Right
      (Controlled.controlledReleasedLabelRecord object controlledWithRelease)
  unless
    ( labelRecordReleasedState controlledRecord == releasedLabelOverlayState released
        && labelRecordRevision controlledRecord == releasedLabelOverlayRevision released
        && labelRecordRetainedAuthority controlledRecord
          == releasedLabelOverlayRetainedAuthority released
    )
    oracleFault
  let releasedView = releasedLabelStateView (releasedLabelOverlayState released)
      controlled =
        case (releasedView, LabelPatch.targetRoleView (LabelPatch.labelPatchRecipeRole recipe)) of
          -- Terminal deletion delegates reservation retirement to the shared
          -- controlled-removal transaction so the work is applied and counted
          -- exactly once.  Nonterminal handoff/void release keeps its existing
          -- early passivation step.
          (ReleasedDeletedView _, _) -> controlledWithRelease
          (_, LabelPatch.NablaTargetView nabla _) ->
            fst
              ( Controlled.commitControlledNablaReservationRetirement
                  ( Controlled.prepareControlledNablaReservationRetirement
                      nabla
                      controlledWithRelease
                  )
              )
          _ -> controlledWithRelease
  let (patches, generator) = LabelPatch.commitPatchRelease preparedPatch
      withRelease =
        replaceStartupIdGeneratorState generator
          . replaceStartupLabelPatchState patches
          . replaceStartupControlledState controlled
          $ state
  (withTopology, topologyEffects) <-
    case releasedView of
      ReleasedDeletedView {} -> do
        preparedRemoval <-
          mapControlledRemovalProblem
            ( ControlledRemoval.prepareLabelControlledRemoval
                releaseIndex
                recipe
                withRelease
            )
        let (successor, effects, _) =
              ControlledRemoval.commitControlledRemoval preparedRemoval
        Right (successor, effects)
      _ ->
        applyLabelReleaseTopology
          releaseIndex
          recipe
          (releasedLabelOverlayState released)
          withRelease
  (successor, terminalEffects) <-
    applyTerminalLabelOutcome
      decision
      releaseIndex
      outcomeDigest
      LabelApplied
      withTopology
  Right
    ( successor,
      effectBatchMembers topologyEffects <> terminalEffects
    )

applyLabelReleaseTopology ::
  ControlIndex ->
  LabelPatch.LabelPatchRecipe ->
  ReleasedLabelState ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyLabelReleaseTopology releaseIndex recipe released state =
  case LabelPatch.targetRoleView (LabelPatch.labelPatchRecipeRole recipe) of
    LabelPatch.AbsentOrdinaryTargetView -> Right (state, mempty)
    LabelPatch.OrdinaryControlledTargetView -> Right (state, mempty)
    _ -> do
      cause <- mapConsequenceProblem (labelReleaseCause decision releaseIndex)
      suppliedFresh <- labelPatchFreshStoreEvidence recipe
      unchanged <-
        mapReconciliationProblem
          ( Reconciliation.prepareUnchangedStructuralLabelControlRefresh
              (labelStructuralReconciliationViews object released state)
              cause
              object
              released
              suppliedFresh
              reconciliation
          )
      case unchanged of
        Just _ ->
          Right
            ( recordStructuralWork (LabelPatch.noLabelStructuralWork {LabelPatch.labelWorkProjectionReuses = 1}) state,
              mempty
            )
        Nothing -> do
          preparation <-
            mapReconciliationProblem
              ( Reconciliation.prepareStructuralLabelControlRefresh
                  (structuralReconciliationViews state)
                  cause
                  object
                  released
                  suppliedFresh
                  reconciliation
              )
          case preparation of
            Reconciliation.StructuralHeld _ -> oracleFault
            Reconciliation.StructuralReady prepared -> do
              let counted = recordStructuralWork (LabelPatch.structuralLabelPreparationWork False prepared) state
              -- Empty patches need no cause receipts or loss delivery. The
              -- Oracle entry loop still advances control-dependent readiness.
              if not (Reconciliation.preparedStructuralChangesState prepared)
                then Right (counted, mempty)
                else do
                  successor <- applyPreparedStructuralControlRefresh cause releaseIndex prepared counted
                  mapAlignmentTransferProblem (AlignmentTransfer.deliverAlignmentLossTransferDelta counted successor)
  where
    decision = LabelPatch.labelPatchRecipeDecision recipe
    object = LabelPatch.labelPatchRecipeObject recipe
    reconciliation = GraphProgress.structuralProgressReconciliation (startupStructuralProgressState state)
    recordStructuralWork work current =
      replaceStartupLabelPatchState
        (LabelPatch.recordLabelStructuralWork work (startupLabelPatchState current))
        current

-- | The unchanged-projection proof reads only this target's proposed process
-- and Normal possession. It never needs the global sort, endpoint or process
-- inventories used by full reconciliation.
labelStructuralReconciliationViews :: GlobalObjectId -> ReleasedLabelState -> HeraldState -> Reconciliation.ReconciliationViews
labelStructuralReconciliationViews object released state =
  Reconciliation.withReconciliationDiagnosticChecks (startupDiagnosticChecks state)
    $ Reconciliation.reconciliationViewsWithEndedProcesses ended
    $ Reconciliation.reconciliationViews local Map.empty Set.empty residences possessions
  where
    view = OracleProjection.oracleView (startupOracleProjectionState state)
    local = OracleProjection.oracleViewLocalHeraldEpoch view
    process = case releasedLabelStateView released of
      ReleasedLabelView (ProcessLabel owner, _) -> Just owner
      _ -> Nothing
    residence = do
      owner <- process
      host <- OracleProjection.oracleViewProcessResidence owner view
      pure (owner, host)
    residences = case residence of
      Just (owner, host) | OracleProjection.oracleViewProcessIsLive owner view -> Map.singleton owner host
      _ -> Map.empty
    ended = case residence of
      Just (owner, host)
        | Just end <- OracleProjection.oracleViewEndedProcess owner view ->
            Map.singleton owner (host, OracleProjection.projectedEndedProcessControlIndex end)
      _ -> Map.empty
    possessions = case process of
      Just owner | Controlled.controlledHasNormalPossession owner object (startupControlledState state) -> Set.singleton (owner, object)
      _ -> Set.empty

labelPatchFreshStoreEvidence ::
  LabelPatch.LabelPatchRecipe ->
  Either HeraldInvariantFault (Maybe Reconciliation.FreshStoreIncarnationEvidence)
labelPatchFreshStoreEvidence recipe =
  case Map.toAscList (LabelPatch.labelPatchRecipePreallocatedStores recipe) of
    [] -> Right Nothing
    [(activation, incarnation)] ->
      Right
        ( Just
            ( Reconciliation.freshStoreIncarnationEvidence
                Reconciliation.GeneratedStoreIncarnation
                (LabelPatch.storeActivationDelta activation)
                incarnation
            )
        )
    _ -> oracleFault

applyPreparedStructuralControlRefresh ::
  StructuralConsequenceCause ->
  ControlIndex ->
  Reconciliation.PreparedStructuralReconciliation ->
  HeraldState ->
  Either HeraldInvariantFault HeraldState
applyPreparedStructuralControlRefresh cause prerequisite prepared state = do
  preparedStore <-
    mapStorePatchProblem
      ( Store.prepareStructuralStorePatch
          cause
          (Reconciliation.preparedStructuralStorePatch prepared)
          (startupStoreState state)
      )
  preparedPlacement <-
    mapPlacementPatchProblem
      ( Placement.prepareStructuralPlacementPatch
          prerequisite
          (Reconciliation.preparedStructuralPlacementPatch prepared)
          (startupPlacementState state)
      )
  preparedProgress <-
    mapProgressProblem
      ( GraphProgress.prepareStructuralProjectionRefresh
          prepared
          (startupGraphState state)
          (startupStructuralProgressState state)
      )
  preparedAlignment <-
    mapAlignmentProblem
      ( Alignment.prepareStructuralDebtRetention
          cause
          (Reconciliation.preparedStructuralDebts prepared)
          (startupAlignmentState state)
      )
  let store = Store.commitStructuralStorePatch preparedStore
      (placement, _) = Placement.commitStructuralPlacementPatch preparedPlacement
      (graph, progress) =
        GraphProgress.commitStructuralProjectionRefresh preparedProgress
      (alignment, _) = Alignment.commitStructuralDebtRetention preparedAlignment
      successor =
        replaceStartupAlignmentState alignment
          . replaceStartupStructuralProgressState progress
          . replaceStartupGraphState graph
          . replaceStartupPlacementState placement
          . replaceStartupStoreState store
          $ state
  retainVanishedLocalPlacementLosses cause state successor

-- | A local structural control patch changes the live placement projection
-- before a later installed cut advertises it.  Observe exact physical
-- coordinates at that atomic patch boundary so an old destination plan cannot
-- be re-driven first and a selected source cannot be chosen again in between.
retainVanishedLocalPlacementLosses ::
  StructuralConsequenceCause ->
  HeraldState ->
  HeraldState ->
  Either HeraldInvariantFault HeraldState
retainVanishedLocalPlacementLosses cause predecessor successor = do
  coordinator <-
    mapAlignmentLossInvariant
      ( AlignmentLoss.alignmentLossCoordinatorState
          local
          (startupAlignmentState successor)
      )
  afterLoss <- foldM applyOne coordinator (Set.toAscList vanished)
  Right
    ( replaceStartupAlignmentState
        (AlignmentLoss.alignmentLossCoordinatorOwner afterLoss)
        successor
    )
  where
    local = checkedLocalHeraldEpoch (startupGenesis successor)
    vanished =
      localPlacementStoreCoordinates local (startupPlacementState predecessor)
        Set.\\ localPlacementStoreCoordinates local (startupPlacementState successor)
    applyOne current lost = do
      prepared <-
        mapAlignmentLossProblem
          (AlignmentLoss.prepareAlignmentLoss cause lost current)
      Right (AlignmentLoss.commitAlignmentLoss prepared)

localPlacementStoreCoordinates ::
  HeraldEpoch ->
  Placement.State ->
  Set.Set AlignmentLoss.QualifiedStoreCoordinate
localPlacementStoreCoordinates local placement =
  Set.fromList
    [ AlignmentLoss.qualifiedStoreCoordinate
        local
        (Placement.localPlacementDelta retained)
        (Placement.localPlacementStoreIncarnation retained)
    | retained <- Placement.localPlacements placement
    ]

structuralReconciliationViews :: HeraldState -> Reconciliation.ReconciliationViews
structuralReconciliationViews state =
  Reconciliation.withReconciliationDiagnosticChecks (startupDiagnosticChecks state)
    $ Reconciliation.reconciliationViewsWithEndedProcesses
      ( Map.fromList
          [ (process, (residence, OracleProjection.projectedEndedProcessControlIndex ended))
          | (process, ended) <- OracleProjection.projectedEndedProcesses oracle,
            Just residence <- [OracleProjection.oracleViewProcessResidence process oracleView]
          ]
      )
    $ Reconciliation.reconciliationViews
      (OracleProjection.oracleViewLocalHeraldEpoch oracleView)
      ( Map.fromList
          [ ( SortRegistry.registryEntrySortId entry,
              sortOccurrence
                (SortRegistry.registryEntrySortId entry)
                (SortRegistry.registryEntryOccurrenceId entry)
            )
          | entry <- SortRegistry.registryEntries (startupSortRegistryState state)
          ]
      )
      (Set.fromList (Graph.graphNonStructuralBaselineVertices (startupGraphState state)))
      ( Map.fromList
          [ (process, residence)
          | process <- OracleProjection.projectedProcessEpochs oracle,
            OracleProjection.oracleViewProcessIsLive process oracleView,
            Just residence <- [OracleProjection.oracleViewProcessResidence process oracleView]
          ]
      )
      ( Set.fromList
          ( Controlled.controlledNormalPossessionEntries
              (startupControlledState state)
          )
      )
  where
    oracle = startupOracleProjectionState state
    oracleView = OracleProjection.oracleView oracle

applyTerminalLabelOutcome ::
  LabelDecisionId ->
  ControlIndex ->
  LabelOutcomeDigest ->
  LabelResult ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyTerminalLabelOutcome _ _ _ _ state | GraphProgress.structuralProgressIsJoiningObserver (startupStructuralProgressState state) = Right (state, [])
applyTerminalLabelOutcome decision index digest result state = do
  workflow <- requireProjectedLabelWorkflow decision state
  let opened = OracleProjection.projectedLabelWorkflowDecision workflow
      evidence =
        LabelBarrier.terminalOutcomeEvidence
          decision
          index
          digest
          profileCatalogueDigest
          (liveDecisionMemberSetDigest opened)
  preparedBarrier <-
    mapBarrierProblem
      ( LabelBarrier.prepareTerminalApplication
          evidence
          (startupLabelBarrierState state)
      )
  applicationWithTerminal <-
    terminalApplicationIfOwned decision result state
  installed <-
    LabelCollection.beginInstallationCollection
      decision
      ( replaceStartupApplicationState applicationWithTerminal
          . replaceStartupLabelBarrierState
            (LabelBarrier.commitTerminalApplication preparedBarrier)
          $ state
      )
  Right (installed, [])

terminalApplicationIfOwned ::
  LabelDecisionId ->
  LabelResult ->
  HeraldState ->
  Either HeraldInvariantFault Application.State
terminalApplicationIfOwned decision result state =
  case Application.applicationActiveLabelForDecision decision application of
    Nothing -> Right application
    Just _ -> do
      prepared <-
        mapApplicationProblem
          (Application.prepareApplicationLabelTerminal decision result application)
      Right (Application.commitApplicationLabelTerminal prepared)
  where
    application = startupApplicationState state

-- A local invocation may wait for its own publication group while this Herald
-- participates in a different canonical workflow. An unsent decision request
-- owns no Oracle outcome and can be ended locally before submission.
labelApplicationIsDraining :: Application.ActiveLabelApplicationView -> HeraldState -> Bool
labelApplicationIsDraining active state =
  case activeLabelRequest active state of
    Just witness ->
      OracleClient.oracleRequestWitnessStatus witness == OracleClient.OracleRequestAwaitingSourceDrain
    Nothing -> False

activeLabelRequest :: Application.ActiveLabelApplicationView -> HeraldState -> Maybe OracleClient.OracleRequestWitness
activeLabelRequest active state =
  OracleClient.lookupOracleRequestBySemanticKey
    (Application.applicationRequestKeyCanonicalBytes (Application.activeLabelApplicationKey active))
    (startupOracleClientState state)

applyLabelWorkflowCompletion ::
  LabelDecisionId ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyLabelWorkflowCompletion _ state | GraphProgress.structuralProgressIsJoiningObserver (startupStructuralProgressState state) = Right (state, [])
applyLabelWorkflowCompletion decision state
  | LabelBarrier.barrierTerminalEvidence decision (startupLabelBarrierState state) == Nothing = Right (state, [])
  | otherwise = do
      preparedBarrier <-
        mapBarrierProblem
          ( LabelBarrier.prepareWorkflowCompletion
              decision
              (startupLabelBarrierState state)
          )
      completeLocalApplication
        decision
        (replaceStartupLabelBarrierState (LabelBarrier.commitWorkflowCompletion preparedBarrier) state)

-- A failed immutable comparison finishes here without a global installation
-- collector. Successful decisions call this only at canonical completion.
completeLocalApplication :: LabelDecisionId -> HeraldState -> Either HeraldInvariantFault (HeraldState, [HeraldEffect])
completeLocalApplication decision state = case Application.applicationActiveLabelForDecision decision (startupApplicationState state) of
  Just _ -> do
    prepared <- mapApplicationProblem (Application.prepareApplicationLabelWorkflowCompletion decision (startupApplicationState state))
    let (application, outcome) = Application.commitApplicationLabelWorkflowCompletion prepared
    Right (replaceStartupApplicationState application state, applicationCompletionEffects outcome)
  _ -> Right (state, [])

applicationCompletionEffects ::
  Application.ApplicationOperationCompletionOutcome -> [HeraldEffect]
applicationCompletionEffects outcome = case outcome of
  Application.ApplicationOperationNoLiveRequest _ -> []
  Application.ApplicationOperationCompleted Nothing -> []
  Application.ApplicationOperationCompleted (Just observation) ->
    [ SendApplicationReply
        (Application.applicationOperationCompletionBinding observation)
        ( RetainedRequestReply
            (Application.applicationOperationCompletionCursor observation)
            ( Completed
                (Application.applicationOperationCompletionRequestId observation)
                (Application.applicationOperationCompletionResult observation)
            )
        )
    ]

requireProjectedLabelWorkflow ::
  LabelDecisionId ->
  HeraldState ->
  Either HeraldInvariantFault OracleProjection.ProjectedLabelWorkflow
requireProjectedLabelWorkflow decision state =
  maybe
    oracleFault
    Right
    ( OracleProjection.oracleViewLabelWorkflow
        decision
        (OracleProjection.oracleView (startupOracleProjectionState state))
    )

-- | Advance the selected source drain and installation collector after serving
-- transitions. Each owner retains its progress, so an unchanged pass emits no
-- new request or peer report.
advanceLocalLabelWork ::
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
advanceLocalLabelWork initial | GraphProgress.structuralProgressIsJoiningObserver (startupStructuralProgressState initial) = Right (initial, mempty)
advanceLocalLabelWork initial = do
  -- The serving driver derives new intentions; it is not a delivery retry.
  -- Anything already present at this serialized cut was offered when first
  -- retained. Binding replacement, explicit retry, and recovery entry paths
  -- separately reoffer the complete retained request set.
  let previouslyOffered =
        OracleClient.oracleClientLabelRequestActions
          (startupOracleClientState initial)
  withSource <- submitDrainedLabel initial
  (successor, collectionEffects) <- LabelCollection.advanceInstallationCollection withSource
  let oracleEffects =
        fmap
          RunOracleClientAction
          ( filter
              (`notElem` previouslyOffered)
              ( OracleClient.oracleClientLabelRequestActions
                  (startupOracleClientState successor)
              )
          )
      progressEffects
        | OracleClient.oracleClientProgressReady (startupOracleClientState initial)
            /= OracleClient.oracleClientProgressReady (startupOracleClientState successor) =
            fmap RunOracleClientAction (OracleClient.oracleClientProgressActions (startupOracleClientState successor))
        | otherwise = []
  Right (successor, orderedEffectBatch (collectionEffects <> oracleEffects <> progressEffects))

-- | Reserve the distributed workflow only when the exact caller/object group
-- has settled. Empty groups take the same transition as application acceptance.
-- Membership installation can keep an intention locally pending. The Oracle
-- independently excludes competing decisions only for the same object.
submitDrainedLabel :: HeraldState -> Either HeraldInvariantFault HeraldState
submitDrainedLabel state =
  foldM submitOne state (Application.applicationActiveLabels (startupApplicationState state))
  where
    submitOne current active
      | labelApplicationIsDraining active current,
        PublicationGroups.groupReady
          (Application.sequencingSelectionGroup (Application.activeLabelApplicationSelection active))
          (Publication.publicationGroups (startupPublicationState current)),
        not (Application.applicationMembershipGateClosed (startupApplicationState current)),
        OracleProjection.oracleViewCurrentHeraldMembershipId
          (OracleProjection.oracleView (startupOracleProjectionState current))
          == GraphProgress.structuralProgressMembershipGenerationId (startupStructuralProgressState current) = do
          witness <- maybe oracleFault Right (activeLabelRequest active current)
          preparedClient <-
            mapClientProblem
              ( OracleClient.prepareDrainedLabelSubmission
                  (OracleClient.oracleRequestWitnessRef witness)
                  (startupOracleClientState current)
              )
          let (client, _, _) = OracleClient.commitOracleRequest preparedClient
          Right (replaceStartupOracleClientState client current)
      | otherwise = Right current

-- No request was emitted, so canonical caller End can finish this invocation
-- locally. Accepted publications keep their own group obligations. A submitted
-- request is deliberately excluded and retains its original Oracle outcome.
endUnsentLabel :: ProcessEpochId -> ControlIndex -> HeraldState -> Either HeraldInvariantFault (HeraldState, [HeraldEffect])
endUnsentLabel process index state =
  foldM endOne (state, []) (Application.applicationActiveLabels (startupApplicationState state))
  where
    endOne (current, effects) active
      | labelApplicationIsDraining active current = do
          witness <- maybe oracleFault Right (activeLabelRequest active current)
          case OracleClient.oracleRequestWitnessIntent witness of
            OracleClient.DecideLabelIntent decision caller _ _ _ _ _ _ _
              | caller == process -> do
                  preparedClient <-
                    mapClientProblem
                      (OracleClient.prepareUnsentLabelEnd (OracleClient.oracleRequestWitnessRef witness) process index (startupOracleClientState current))
                  preparedTerminal <-
                    mapApplicationProblem
                      (Application.prepareApplicationLabelTerminal decision LabelNotApplied (startupApplicationState current))
                  preparedCompletion <-
                    mapApplicationProblem
                      (Application.prepareApplicationLabelWorkflowCompletion decision (Application.commitApplicationLabelTerminal preparedTerminal))
                  let (client, _, _) = OracleClient.commitOracleRequest preparedClient
                      (application, outcome) = Application.commitApplicationLabelWorkflowCompletion preparedCompletion
                  Right
                    ( replaceStartupOracleClientState client . replaceStartupApplicationState application $ current,
                      effects <> applicationCompletionEffects outcome
                    )
            _ -> Right (current, effects)
      | otherwise = Right (current, effects)

-- | Apply the non-projection owners affected by the one canonical live
-- membership contraction. OracleProjection has already validated the exact
-- successor and resident-End batch, and OracleClient has already crossed its
-- membership-aware cursor seam at the same control coordinate. This
-- coordinator therefore neither invents nor stores a second control-stream
-- fact.
applyMembershipProjectionEvents ::
  [OracleProjectionEvent] ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyMembershipProjectionEvents events state = do
  withAdmissions <-
    foldM
      retainAdmission
      state
      [record | event <- events, HeraldAdmissionChangedView record <- [oracleProjectionEventView event]]
  case [ generation
       | event <- events,
         HeraldMembershipAdvancedView generation <- [oracleProjectionEventView event]
       ] of
    [] -> Right (withAdmissions, [])
    [generation] -> case heraldMembershipGenerationAdmissionId generation of
      Just admission -> applyLiveHeraldAdmission admission generation withAdmissions
      Nothing -> applyLiveMembershipAdvance generation retirementEnds withAdmissions
    _ -> oracleFault
  where
    retainAdmission current record = do
      prepared <- either (const oracleFault) Right (Discovery.prepareHeraldAdmission record (startupDiscoveryState current))
      JoinControlTails.observeAdmission record (replaceStartupDiscoveryState (Discovery.commitHeraldAdmission prepared) current)
    retirementEnds =
      [ (process, index, reason)
      | event <- events,
        ProcessEpochEndedEventView process index reason <- [oracleProjectionEventView event],
        processEndReasonIsHeraldRetirement reason
      ]

-- The same whole-owner successor installs the certified base and updates
-- discovery; only the applicant's own activation starts ordinary runtime work.
applyLiveHeraldAdmission :: HeraldAdmissionId -> HeraldMembershipGeneration -> HeraldState -> Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyLiveHeraldAdmission admission membership predecessor = do
  record <- maybe oracleFault Right (OracleProjection.oracleViewHeraldAdmission admission (OracleProjection.oracleView (startupOracleProjectionState predecessor)))
  (contribution, recipe) <- either (const oracleFault) Right (checkedJoinBase record predecessor)
  preparedBase <- mapProgressProblem (GraphProgress.prepareMembershipAdmissionBaseInstallation record recipe contribution (structuralReconciliationViews predecessor) (startupGraphState predecessor) (startupStructuralProgressState predecessor))
  preparedDiscovery <- either (const oracleFault) Right (Discovery.prepareMembershipAdvance membership (startupDiscoveryState predecessor))
  let (graph, progress) = GraphProgress.commitMembershipAdmissionBaseInstallation preparedBase
      (discovery, _) = Discovery.commitMembershipAdvance preparedDiscovery
      installed =
        replaceStartupGraphState graph
          . replaceStartupStructuralProgressState progress
          . replaceStartupDiscoveryState discovery
          $ predecessor
      selfActivated = GraphProgress.structuralProgressIsJoiningObserver (startupStructuralProgressState predecessor) && not (GraphProgress.structuralProgressIsJoiningObserver progress)
  (settled, settledEffects) <-
    if GraphProgress.structuralProgressIsJoiningObserver progress
      then Right (installed, mempty)
      else mapStructuralProblem (StructuralCoordinator.settleInstalledStructuralCuts [GraphProgress.structuralLastInstalledCutId progress] installed)
  if selfActivated
    then do
      activeClient <- mapClientProblem (OracleClient.startJoiningParticipation (startupOracleClientState settled))
      (isolation, disposition) <- either (const oracleFault) Right (Isolation.startJoiningParticipation (startupLastObservedTime predecessor) (heraldMembershipGenerationId membership) (startupIsolationState predecessor))
      let timers = case disposition of
            Isolation.InitialIsolationGraceArmed attempt specification -> [ArmTimer attempt specification]
      Right (JoinControlTails.releaseOwnActivation record . replaceStartupOracleClientState activeClient . replaceStartupIsolationState isolation $ settled, effectBatchMembers settledEffects <> timers <> [RunOracleHealthRound roundNumber cadence (OracleClient.oracleClientHelloClaims activeClient) (NonEmpty.toList (OracleContact.oracleContacts (OracleClient.oracleClientContacts activeClient))) | (roundNumber, cadence) <- maybe [] pure (Isolation.oracleHealthRound isolation)] <> map RunOracleClientAction (OracleClient.oracleClientActions activeClient))
    else Right (settled, effectBatchMembers settledEffects)

applyLiveMembershipAdvance ::
  HeraldMembershipGeneration ->
  [(ProcessEpochId, ControlIndex, ProcessEndReason)] ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyLiveMembershipAdvance membership processEnds predecessor = do
  retired <-
    maybe
      oracleFault
      Right
      (heraldMembershipGenerationRetiredHeraldEpoch membership)
  let discoveryBefore = startupDiscoveryState predecessor
      pendingDials = Discovery.peerDialIntents discoveryBefore
  preparedDiscovery <-
    either
      (const oracleFault)
      Right
      (Discovery.prepareMembershipAdvance membership discoveryBefore)
  let retiredBindings =
        Discovery.preparedMembershipRetiredBindings preparedDiscovery
      localRetired =
        Discovery.preparedMembershipLocalRetired preparedDiscovery
      dials
        | localRetired = pendingDials
        | otherwise =
            filter ((== retired) . peerDialIntentHeraldEpoch) pendingDials
      (discovery, _) = Discovery.commitMembershipAdvance preparedDiscovery
      (peerLiveness, recoveryTimers)
        | localRetired = PeerLiveness.cutForDrain (startupPeerLivenessState predecessor)
        | otherwise =
            let (successor, cancellation) =
                  PeerLiveness.retirePeer retired (startupPeerLivenessState predecessor)
             in (successor, maybe [] pure cancellation)
  oracleClient <-
    if localRetired
      then
        either
          (const oracleFault)
          Right
          (OracleClient.prepareDrain (startupOracleClientState predecessor))
      else Right (startupOracleClientState predecessor)
  (peerStream, placement, retiredPlacement, cancelledDestinations) <-
    if localRetired
      then
        Right
          ( startupPeerStreamState predecessor,
            startupPlacementState predecessor,
            Nothing,
            []
          )
      else do
        preparedPeerStream <-
          either
            (const oracleFault)
            Right
            ( PeerStream.preparePeerRetirement
                retired
                (startupPeerStreamState predecessor)
            )
        preparedPlacement <-
          either
            (const oracleFault)
            Right
            ( Placement.prepareRemotePlacementRetirementWithDiagnostics
                (startupDiagnosticChecks predecessor)
                retired
                (startupPlacementState predecessor)
            )
        let (nextPeerStream, _) = PeerStream.commitPeerRetirement preparedPeerStream
            (nextPlacement, retiredProjection) =
              Placement.commitRemotePlacementRetirement preparedPlacement
        Right
          ( nextPeerStream,
            nextPlacement,
            Just retiredProjection,
            [retired]
          )
  let withMembership =
        JoinControlTails.releaseRetired retired
          . replaceStartupPublicationState
            (Publication.retirePublicationGroupPeer retired (startupPublicationState predecessor))
          . replaceStartupPlacementState placement
          . replaceStartupStructuralProgressState
            (GraphProgress.notifyAlignmentMembershipChange (startupStructuralProgressState predecessor))
          . replaceStartupPeerStreamState peerStream
          . replaceStartupPeerLivenessState peerLiveness
          . replaceStartupDiscoveryState discovery
          . replaceStartupOracleClientState oracleClient
          $ predecessor
  (withEnds, endEffects) <-
    foldM
      ( \(current, retained) (process, index, reason) -> do
          (successor, emitted) <-
            applyResidentEndWith
              ( if localRetired
                  then PreserveResidentBranchForIsolation
                  else RetireResidentApplication ApplicationSessionNoLongerLive
              )
              process
              index
              reason
              current
          Right (successor, retained <> emitted)
      )
      (withMembership, [])
      processEnds
  (withFinalPeerCut, finalCancelledDestinations) <-
    if localRetired
      then do
        preparedPeerStream <-
          either
            (const oracleFault)
            Right
            (PeerStream.prepareLocalRetirementCut (startupPeerStreamState withEnds))
        let (nextPeerStream, directions) =
              PeerStream.commitLocalRetirementCut preparedPeerStream
            destinations = fmap streamDirectionDestination directions
            publication =
              foldr
                Publication.retirePublicationGroupPeer
                (startupPublicationState withEnds)
                destinations
        Right
          ( replaceStartupPublicationState publication
              . replaceStartupPeerStreamState nextPeerStream
              $ withEnds,
            destinations
          )
      else Right (withEnds, cancelledDestinations)
  let preparedEvidenceRetirement =
        Alignment.preparePendingGenerationEvidenceRetirement
          retired
          (startupAlignmentState withFinalPeerCut)
      (alignmentAfterEvidenceRetirement, _) =
        Alignment.commitPendingGenerationEvidenceRetirement
          preparedEvidenceRetirement
      withRetiredGenerationEvidence =
        replaceStartupAlignmentState
          alignmentAfterEvidenceRetirement
          withFinalPeerCut
  afterLoss <- case retiredPlacement of
    Nothing -> Right withRetiredGenerationEvidence
    Just retiredProjection -> do
      alignment <-
        applyLiveRetiredPlacementLosses
          (checkedLocalHeraldEpoch (startupGenesis predecessor))
          (liveRetiredPlacementCoordinates retiredProjection)
          (startupAlignmentState withRetiredGenerationEvidence)
      Right
        ( replaceStartupAlignmentState
            alignment
            withRetiredGenerationEvidence
        )
  (successor, alignmentLossEffects) <-
    either
      (const oracleFault)
      Right
      ( AlignmentTransfer.deliverAlignmentLossTransferDelta
          withRetiredGenerationEvidence
          afterLoss
      )
  let effects =
        endEffects
          <> fmap CancelTimer recoveryTimers
          <> fmap CancelPeerDial dials
          <> fmap ClosePeerBinding retiredBindings
          <> fmap CancelPeerDestination finalCancelledDestinations
          <> effectBatchMembers alignmentLossEffects
  Right
    ( successor,
      if localRetired then filter localRetirementCleanupEffect effects else effects
    )

-- | Freeze the retired-source inventory only after this Oracle coordinate has
-- released every pre-existing control waiter.  A shape-valid structural item
-- can be held until this very membership entry and then fail semantic
-- authentication; advertising the earlier raw bytes would let that rejected
-- item poison another survivor through terminal-source relay.
beginTerminalStructuralAfterMembership ::
  Maybe HeraldMembershipGeneration ->
  HeraldState ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
beginTerminalStructuralAfterMembership Nothing _ successor =
  Right (successor, mempty)
beginTerminalStructuralAfterMembership (Just membership) _ successor
  | heraldMembershipGenerationAdmissionId membership /= Nothing = Right (successor, mempty)
  | GraphProgress.structuralProgressIsJoiningObserver (startupStructuralProgressState successor) = Right (successor, mempty)
beginTerminalStructuralAfterMembership (Just membership) oraclePredecessor successor = do
  retired <-
    maybe
      oracleFault
      Right
      (heraldMembershipGenerationRetiredHeraldEpoch membership)
  let local = checkedLocalHeraldEpoch (startupGenesis successor)
  if retired == local
    then Right (successor, mempty)
    else do
      retained <-
        either
          (const oracleFault)
          Right
          ( TerminalStructuralArchive.retainedRetiredSourceOccurrences
              retired
              (startupPublicationState successor)
          )
      coordinator <-
        either
          (const oracleFault)
          Right
          ( TerminalStructuralStart.beginAfterAppliedMembershipAdvance
              retained
              oraclePredecessor
              successor
          )
      let withCoordinator =
            replaceStartupStructuralBaseCoordinator (Just coordinator) successor
          inventories = TerminalSource.structuralBaseCoordinatorLocalInventories coordinator
          broadcasts =
            [ SendPeerControl
                binding
                (PeerTerminalSourceInventoryAdvertised inventory)
            | inventory <- inventories,
              binding <-
                Discovery.currentPeerBindings
                  (startupDiscoveryState withCoordinator)
            ]
      (withStructuralWork, structuralEffects) <-
        mapStructuralProblem
          (StructuralCoordinator.advanceLocalStructuralWork withCoordinator)
      Right
        ( withStructuralWork,
          orderedEffectBatch broadcasts <> structuralEffects
        )

liveRetiredPlacementCoordinates ::
  Placement.RetiredRemotePlacement ->
  [AlignmentLoss.QualifiedStoreCoordinate]
liveRetiredPlacementCoordinates retiredPlacement =
  Set.toAscList
    ( Set.fromList
        [ AlignmentLoss.qualifiedStoreCoordinate
            owner
            (deltaRouteDelta route)
            (deltaRouteStoreIncarnation route)
        | route <- Placement.retiredRemotePlacementRoutes retiredPlacement
        ]
    )
  where
    owner = Placement.retiredRemotePlacementOwner retiredPlacement

applyLiveRetiredPlacementLosses ::
  HeraldEpoch ->
  [AlignmentLoss.QualifiedStoreCoordinate] ->
  Alignment.State ->
  Either HeraldInvariantFault Alignment.State
applyLiveRetiredPlacementLosses local lostStores predecessor =
  fst <$> foldM applyCoordinate (predecessor, []) lostStores
  where
    applyCoordinate (owner, retainedPlans) lost =
      case Set.toAscList (AlignmentLoss.alignmentLossRelevantCauses lost owner) of
        [] ->
          let unavailable =
                Alignment.unavailableAlignmentSource
                  (AlignmentLoss.qualifiedStoreCoordinateHerald lost)
                  (AlignmentLoss.qualifiedStoreCoordinateDelta lost)
                  (AlignmentLoss.qualifiedStoreCoordinateIncarnation lost)
              (successor, _) =
                Alignment.commitUnavailableAlignmentSourceRetention
                  (Alignment.prepareUnavailableAlignmentSourceRetention unavailable owner)
           in Right (successor, retainedPlans)
        causes -> do
          coordinator <-
            either
              (const oracleFault)
              Right
              (AlignmentLoss.alignmentLossCoordinatorState local owner)
          (successorCoordinator, plans) <-
            foldM (applyCause lost) (coordinator, []) causes
          Right
            ( AlignmentLoss.alignmentLossCoordinatorOwner successorCoordinator,
              retainedPlans <> plans
            )

    applyCause lost (coordinator, plans) cause = do
      prepared <-
        either
          (const oracleFault)
          Right
          ( AlignmentLoss.prepareAlignmentLossAfterMembershipRetirement
              cause
              lost
              coordinator
          )
      Right
        ( AlignmentLoss.commitAlignmentLoss prepared,
          plans <> [AlignmentLoss.preparedAlignmentLossPlan prepared]
        )

localRetirementCleanupEffect :: HeraldEffect -> Bool
localRetirementCleanupEffect = \case
  SendApplicationIsolationBegun {} -> True
  DisposeApplicationSession {} -> True
  CancelTimer {} -> True
  CancelPeerDial {} -> True
  ClosePeerBinding {} -> True
  CancelPeerDestination {} -> True
  RunOracleClientAction (CancelOracleRetry _) -> True
  _ -> False

applyResidentEndEvents ::
  [OracleProjectionEvent] ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyResidentEndEvents events initial = foldM applyOne (initial, []) events
  where
    applyOne (state, effects) event =
      case oracleProjectionEventView event of
        ProcessEpochEndedEventView _ _ reason
          | processEndReasonIsHeraldRetirement reason ->
              Right (state, effects)
        ProcessEpochEndedEventView process index reason -> do
          (successor, retirementEffects) <-
            applyResidentEnd process index reason state
          Right (successor, effects <> retirementEffects)
        _ -> Right (state, effects)

applyResidentEnd ::
  ProcessEpochId ->
  ControlIndex ->
  ProcessEndReason ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyResidentEnd =
  applyResidentEndWith
    (RetireResidentApplication ApplicationSessionNoLongerLive)

applyResidentEndWithUnavailableReason ::
  ApplicationSessionUnavailableReason ->
  ProcessEpochId ->
  ControlIndex ->
  ProcessEndReason ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyResidentEndWithUnavailableReason =
  applyResidentEndWith . RetireResidentApplication

preserveResidentEndForIsolation ::
  ProcessEpochId ->
  ControlIndex ->
  ProcessEndReason ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
preserveResidentEndForIsolation =
  applyResidentEndWith PreserveResidentBranchForIsolation

data ResidentApplicationEndDisposition
  = RetireResidentApplication ApplicationSessionUnavailableReason
  | PreserveResidentBranchForIsolation

applyResidentEndWith ::
  ResidentApplicationEndDisposition ->
  ProcessEpochId ->
  ControlIndex ->
  ProcessEndReason ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyResidentEndWith applicationDisposition process endIndex reason state = do
  let projection = startupOracleProjectionState state
      view = OracleProjection.oracleView projection
  residence <-
    maybe
      oracleFault
      Right
      (OracleProjection.oracleViewProcessResidence process view)
  case applicationDisposition of
    PreserveResidentBranchForIsolation
      | residence == OracleProjection.oracleViewLocalHeraldEpoch view ->
          Right (state, [])
    _ ->
      applyResidentEndNow
        applicationDisposition
        process
        endIndex
        reason
        residence
        view
        state

applyResidentEndNow ::
  ResidentApplicationEndDisposition ->
  ProcessEpochId ->
  ControlIndex ->
  ProcessEndReason ->
  HeraldEpoch ->
  OracleProjection.View ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, [HeraldEffect])
applyResidentEndNow applicationDisposition process endIndex reason residence view state = do
  let
    universallyRetired =
      replaceStartupControlledState
        ( Controlled.commitControlledProcessRetirement
            ( Controlled.prepareControlledProcessRetirement
                process
                (startupControlledState state)
            )
        )
        state
  (withTopology, topologyEffects) <-
    applyProcessEndTopology process endIndex universallyRetired
  if residence /= OracleProjection.oracleViewLocalHeraldEpoch view
    then Right (withTopology, effectBatchMembers topologyEffects)
    else do
      validateResidentEndOrigin process withTopology
      preparedConfigured <-
        either
          (const oracleFault)
          Right
          ( ConfiguredStart.prepareConfiguredStartProcessEnd
              process
              endIndex
              reason
              ( ConfiguredStart.configuredProcessEndOwners
                  (startupAdministrationState withTopology)
                  (startupConfiguredProcessState withTopology)
              )
          )
      case ConfiguredStart.preparedConfiguredStartProcessEndOutcome preparedConfigured of
        ConfiguredStart.ConfiguredStartProcessEndExactRetry -> oracleFault
        _ -> Right ()
      let (configuredOwners, _) =
            ConfiguredStart.commitConfiguredStartProcessEnd preparedConfigured
          administrationAfterStart =
            ConfiguredStart.configuredProcessEndAdministration configuredOwners
          configuredProcesses =
            ConfiguredStart.configuredProcessEndConfiguredProcesses configuredOwners
      (application, waits, retirementEffects) <-
        case applicationDisposition of
          PreserveResidentBranchForIsolation -> oracleFault
          RetireResidentApplication unavailableReason -> do
            let preparedApplication =
                  Application.prepareApplicationProcessRetirement
                    process
                    (startupApplicationState withTopology)
                (successorApplication, retirement) =
                  Application.commitApplicationProcessRetirement preparedApplication
            preparedWait <-
              either
                (const oracleFault)
                Right
                ( Wait.prepareProcessWaitRemoval
                    process
                    (startupWaitState withTopology)
                )
            let (successorWaits, removedWaits) = Wait.commitWaitRemoval preparedWait
                expectedWaits =
                  sort (Application.applicationProcessRetirementWaitIds retirement)
                actualWaits = sort (fmap Wait.waitRegistrationId removedWaits)
            unless (expectedWaits == actualWaits) oracleFault
            Right
              ( successorApplication,
                successorWaits,
                fmap
                  CancelTimer
                  (Application.applicationProcessRetirementTimerAttempts retirement)
                  <> [ DisposeApplicationSession session unavailableReason
                     | session <- Application.applicationProcessRetirementSessionIds retirement
                     ]
              )
      preparedCompletion <-
        either
          (const oracleFault)
          Right
          ( Administration.prepareEndProcessCompletion
              process
              endIndex
              reason
              administrationAfterStart
          )
      let (administration, _) =
            Administration.commitEndProcessCompletion preparedCompletion
      (withEndedLabel, labelEffects) <-
        endUnsentLabel
          process
          endIndex
          ( replaceStartupWaitState waits
              . replaceStartupApplicationState application
              . replaceStartupConfiguredProcessState configuredProcesses
              . replaceStartupAdministrationState administration
              $ withTopology
          )
      Right (withEndedLabel, retirementEffects <> effectBatchMembers topologyEffects <> labelEffects)

applyProcessEndTopology ::
  ProcessEpochId ->
  ControlIndex ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyProcessEndTopology process endIndex state = do
  cause <- mapConsequenceProblem (processEndCause process endIndex)
  prepared <-
    mapReconciliationProblem
      ( Reconciliation.prepareStructuralProcessEndRefreshes
          (structuralReconciliationViews state)
          cause
          process
          ( GraphProgress.structuralProgressReconciliation
              (startupStructuralProgressState state)
          )
      )
  successor <-
    foldM
      ( \current (_, refresh) ->
          applyPreparedStructuralControlRefresh
            cause
            endIndex
            refresh
            current
      )
      state
      prepared
  mapAlignmentTransferProblem
    ( AlignmentTransfer.deliverAlignmentLossTransferDelta
        state
        successor
    )

-- | Local dynamic processes have exactly one retained owner: administration or
-- prepared-child lifecycle.  The canonical Start and its request witness remain
-- the origin even when cancellation prevented local application installation.
validateResidentEndOrigin ::
  ProcessEpochId ->
  HeraldState ->
  Either HeraldInvariantFault ()
validateResidentEndOrigin process state =
  case OracleProjection.oracleViewStartedProcess process view of
    Nothing ->
      unless
        ( isGenesisProcess
            && scaffoldOrigin == Nothing
            && null administrationOrigins
            && null preparationOrigins
        )
        oracleFault
    Just started ->
      let index = OracleProjection.projectedStartedProcessControlIndex started
          request = OracleProjection.projectedStartedProcessRequest started
          start = OracleProjection.projectedStartedProcessBootstrap started
          administrationOrigin =
            scaffoldOrigin == Just index
              && map (\(originIndex, reference) -> (originIndex, OracleClient.oracleRequestRefRequestId reference)) administrationOrigins == [(index, request)]
              && null preparationOrigins
          preparationOrigin =
            scaffoldOrigin == Nothing
              && null administrationOrigins
              && case preparationOrigins of
                [preparation] ->
                  ProcessPreparation.preparationStart preparation == start
                    && maybe True (== index) (ProcessPreparation.preparationStartControl preparation)
                    && case ProcessPreparation.preparationStartReference preparation of
                      Nothing -> False
                      Just reference ->
                        OracleClient.oracleRequestRefRequestId reference == request
                          && case OracleClient.lookupOracleRequest reference (startupOracleClientState state) of
                            Nothing -> False
                            Just witness ->
                              OracleClient.oracleRequestWitnessBootstrap witness == Just start
                                && OracleClient.oracleRequestWitnessStatus witness == OracleClient.OracleRequestProjected index
                _ -> False
       in unless (administrationOrigin || preparationOrigin) oracleFault
  where
    projection = startupOracleProjectionState state
    view = OracleProjection.oracleView projection
    isGenesisProcess =
      any ((== process) . fst) (OracleProjection.projectedBootstraps projection)
    scaffoldOrigin = do
      scaffold <-
        Configured.lookupLiveProcessScaffold
          process
          (startupConfiguredProcessState state)
      pure (Configured.liveProcessScaffoldControlIndex scaffold)
    administrationOrigins =
      Administration.configuredStartProcessOrigins
        process
        (startupAdministrationState state)
    preparationOrigins =
      [ preparation
      | (_, preparation) <- ProcessPreparation.preparationEntries (startupProcessPreparationState state),
        processStartProcessEpochId (ProcessPreparation.preparationStart preparation) == process
      ]

administrationResultChangeEffects ::
  HeraldState ->
  HeraldState ->
  [HeraldEffect]
administrationResultChangeEffects predecessor successor =
  case Administration.currentConfiguredAdministrationBinding after of
    Nothing -> []
    Just binding ->
      [ SendAdministrationReply binding (RetainedAdminResult correlation status)
      | (correlation, _, _) <-
          Administration.administrationWitnessCommands
            (Administration.administrationStateWitness after),
        AdministrationValue.adminCorrelationBelongsTo binding correlation,
        Just status <- [Administration.lookupAdministrationResultStatus correlation after],
        Administration.lookupAdministrationResultStatus correlation before /= Just status
      ]
  where
    before = startupAdministrationState predecessor
    after = startupAdministrationState successor

installConfiguredStartOwners ::
  ConfiguredStart.Owners ->
  HeraldState ->
  HeraldState
installConfiguredStartOwners owners =
  replaceStartupIdGeneratorState
    (ConfiguredStart.configuredStartIdGenerator owners)
    . replaceStartupConfiguredProcessState
      (ConfiguredStart.configuredStartConfiguredProcesses owners)
    . replaceStartupAdministrationState
      (ConfiguredStart.configuredStartAdministration owners)
    . replaceStartupOracleProjectionState
      (ConfiguredStart.configuredStartOracleProjection owners)
    . replaceStartupOracleClientState
      (ConfiguredStart.configuredStartOracleClient owners)

peerInputContextFor :: HeraldState -> PeerInput.PeerInputContext
peerInputContextFor state =
  PeerInput.peerInputContext
    (startupGenesis state)
    (startupOracleProjectionState state)
    (startupDiscoveryState state)
    (startupPlacementState state)

peerInputStateFor :: HeraldState -> PeerInput.PeerInputState
peerInputStateFor state =
  PeerInput.peerInputState
    (startupPublicationState state)
    (startupPeerStreamState state)
    (startupStoreState state)
    (startupGraphState state)
    (startupIdGeneratorState state)
    (startupStructuralProgressState state)
    (startupAlignmentState state)
    (startupPlacementState state)
    (startupControlledState state)
    (startupSortRegistryState state)
    (startupApplicationState state)
    (startupWaitState state)
    (startupLabelBarrierState state)
    (startupDisappearanceState state)

installPeerInputState :: PeerInput.PeerInputState -> HeraldState -> HeraldState
installPeerInputState owners =
  replaceStartupDisappearanceState (PeerInput.peerInputDisappearanceState owners)
    . replaceStartupLabelBarrierState (PeerInput.peerInputLabelBarrierState owners)
    . replaceStartupWaitState (PeerInput.peerInputWaitState owners)
    . replaceStartupApplicationState (PeerInput.peerInputApplicationState owners)
    . replaceStartupSortRegistryState (PeerInput.peerInputSortRegistryState owners)
    . replaceStartupControlledState (PeerInput.peerInputControlledState owners)
    . replaceStartupPlacementState (PeerInput.peerInputPlacementState owners)
    . replaceStartupAlignmentState (PeerInput.peerInputAlignmentState owners)
    . replaceStartupStructuralProgressState
      (PeerInput.peerInputStructuralProgressState owners)
    . replaceStartupIdGeneratorState (PeerInput.peerInputIdGeneratorState owners)
    . replaceStartupGraphState (PeerInput.peerInputGraphState owners)
    . replaceStartupStoreState (PeerInput.peerInputStoreState owners)
    . replaceStartupPeerStreamState (PeerInput.peerInputPeerStreamState owners)
    . replaceStartupPublicationState (PeerInput.peerInputPublicationState owners)

peerReleaseEffects ::
  HeraldState ->
  PeerInput.PeerControlReleaseResult ->
  [HeraldEffect]
peerReleaseEffects state result =
  rejectionEffects
    <> [ SendPeerControl
           binding
           (PeerPlacementUpdate (FullPlacementSnapshot snapshot))
       | snapshot <- PeerInput.peerControlPlacementSnapshots result,
         binding <- Discovery.currentPeerBindings (startupDiscoveryState state),
         peerBindingRemoteHeraldEpoch binding `Set.notMember` rejectedPeers
       ]
    <> concatMap acknowledgementEffects (PeerInput.peerControlAcknowledgements result)
    <> fmap wakeEffect (PeerInput.peerControlWakes result)
  where
    rejectedPeers =
      Set.fromList
        [ streamDirectionSource (PeerInput.deferredPeerRejectionDirection rejection)
        | rejection <- PeerInput.peerControlDeferredRejections result
        ]
    rejectionEffects =
      [ RejectPeerConnection binding ClosePeerPublicationProtocol
      | peer <- Set.toAscList rejectedPeers,
        Just binding <- [Discovery.currentPeerBinding peer (startupDiscoveryState state)]
      ]
    acknowledgementEffects acknowledgement =
      case if peer `Set.member` rejectedPeers
        then Nothing
        else Discovery.currentPeerBinding peer (startupDiscoveryState state) of
        Nothing -> []
        Just binding ->
          [SendPeerControl binding (PeerStreamCompleted direction (PeerInput.peerAcknowledgementCompletion acknowledgement))]
            <> maybe
              []
              (\offer -> [SendPeerControl binding (PeerStreamResumeOffered offer)])
              (PeerInput.peerAcknowledgementRepairOffer acknowledgement)
      where
        direction = PeerInput.peerAcknowledgementDirection acknowledgement
        peer = streamDirectionSource direction

    wakeEffect wake =
      SendApplicationWaitWake
        (Application.applicationWaitWakeBinding wake)
        (Application.applicationWaitWakeCursor wake)
        (Application.applicationWaitWakeRequestId wake)
        (Application.applicationWaitWakeWaitId wake)
        (Application.applicationWaitWakeResult wake)

peerDependencyReleaseEffects ::
  HeraldState ->
  PeerInput.PeerDependencyReleaseResult ->
  [HeraldEffect]
peerDependencyReleaseEffects state result =
  rejectionEffects
    <> [ SendPeerControl
           binding
           (PeerPlacementUpdate (FullPlacementSnapshot snapshot))
       | snapshot <- PeerInput.peerDependencyPlacementSnapshots result,
         binding <- Discovery.currentPeerBindings (startupDiscoveryState state),
         peerBindingRemoteHeraldEpoch binding `Set.notMember` rejectedPeers
       ]
    <> concatMap
      acknowledgementEffects
      (PeerInput.peerDependencyAcknowledgements result)
    <> fmap wakeEffect (PeerInput.peerDependencyWakes result)
  where
    rejectedPeers =
      Set.fromList
        [ streamDirectionSource (PeerInput.deferredPeerRejectionDirection rejection)
        | rejection <- PeerInput.peerDependencyDeferredRejections result
        ]
    rejectionEffects =
      [ RejectPeerConnection binding ClosePeerPublicationProtocol
      | peer <- Set.toAscList rejectedPeers,
        Just binding <- [Discovery.currentPeerBinding peer (startupDiscoveryState state)]
      ]
    acknowledgementEffects acknowledgement =
      case if peer `Set.member` rejectedPeers
        then Nothing
        else Discovery.currentPeerBinding peer (startupDiscoveryState state) of
        Nothing -> []
        Just binding ->
          [SendPeerControl binding (PeerStreamCompleted direction (PeerInput.peerAcknowledgementCompletion acknowledgement))]
            <> maybe
              []
              (\offer -> [SendPeerControl binding (PeerStreamResumeOffered offer)])
              (PeerInput.peerAcknowledgementRepairOffer acknowledgement)
      where
        direction = PeerInput.peerAcknowledgementDirection acknowledgement
        peer = streamDirectionSource direction

    wakeEffect wake =
      SendApplicationWaitWake
        (Application.applicationWaitWakeBinding wake)
        (Application.applicationWaitWakeCursor wake)
        (Application.applicationWaitWakeRequestId wake)
        (Application.applicationWaitWakeWaitId wake)
        (Application.applicationWaitWakeResult wake)

batchIsContiguous :: NonEmpty CanonicalAppliedOracleEntry -> Bool
batchIsContiguous entries =
  and (zipWith consecutive indices (drop 1 indices))
  where
    indices = fmap entryIndex (NonEmpty.toList entries)
    entryIndex = appliedEntryControlIndex . canonicalAppliedOracleEntryValue
    consecutive earlier later = succControlIndex earlier == later

succControlIndex :: ControlIndex -> ControlIndex
succControlIndex index =
  controlIndex (controlIndexWord64 index + 1)

-- | Retire retry/watch intention before crossing the Herald drain phase cut.
cutOracleClientForDrain ::
  HeraldState ->
  Either HeraldInvariantFault HeraldState
cutOracleClientForDrain predecessor = do
  successorClient <- mapClientProblem (OracleClient.prepareDrain (startupOracleClientState predecessor))
  Right (replaceStartupOracleClientState successorClient predecessor)

actionBatch :: [OracleClientAction] -> EffectBatch
actionBatch = orderedEffectBatch . fmap RunOracleClientAction

checkedOracleSuccess ::
  HeraldState ->
  EffectBatch ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
checkedOracleSuccess state effects = Right (state, effects)

mapClientProblem ::
  Either OracleClient.OracleClientProblem value ->
  Either HeraldInvariantFault value
mapClientProblem = either (const oracleFault) Right

mapPeerProblem ::
  Either PeerInput.PeerInputProblem value ->
  Either HeraldInvariantFault value
mapPeerProblem = either (const oracleFault) Right

mapProgressProblem ::
  Either GraphProgress.StructuralProgressProblem value ->
  Either HeraldInvariantFault value
mapProgressProblem = either (const oracleFault) Right

mapStructuralProblem ::
  Either StructuralCoordinator.StructuralCoordinationProblem value ->
  Either HeraldInvariantFault value
mapStructuralProblem = either (const oracleFault) Right

mapConfiguredStartProblem ::
  Either ConfiguredStart.ConfiguredStartProblem value ->
  Either HeraldInvariantFault value
mapConfiguredStartProblem = either (const oracleFault) Right

mapBarrierProblem ::
  Either LabelBarrier.BarrierProblem value ->
  Either HeraldInvariantFault value
mapBarrierProblem = either (const oracleFault) Right

mapApplicationProblem ::
  Either Application.ApplicationSessionTransitionError value ->
  Either HeraldInvariantFault value
mapApplicationProblem = either (const oracleFault) Right

mapPatchContextProblem ::
  Either LabelPatch.PatchContextProblem value ->
  Either HeraldInvariantFault value
mapPatchContextProblem = either (const oracleFault) Right

mapPatchProblem ::
  Either LabelPatch.PatchProblem value ->
  Either HeraldInvariantFault value
mapPatchProblem = either (const oracleFault) Right

mapControlledReleaseProblem ::
  Either Controlled.ControlledLabelReleaseError value ->
  Either HeraldInvariantFault value
mapControlledReleaseProblem = either (const oracleFault) Right

mapControlledRemovalProblem ::
  Either ControlledRemoval.ControlledRemovalProblem value ->
  Either HeraldInvariantFault value
mapControlledRemovalProblem = either (const oracleFault) Right

mapReconciliationProblem ::
  Either Reconciliation.StructuralReconciliationProblem value ->
  Either HeraldInvariantFault value
mapReconciliationProblem = either (const oracleFault) Right

mapStorePatchProblem ::
  Either Store.StructuralStorePatchError value ->
  Either HeraldInvariantFault value
mapStorePatchProblem = either (const oracleFault) Right

mapPlacementPatchProblem ::
  Either Placement.StructuralPlacementPatchError value ->
  Either HeraldInvariantFault value
mapPlacementPatchProblem = either (const oracleFault) Right

mapAlignmentProblem ::
  Either Alignment.AlignmentDebtProblem value ->
  Either HeraldInvariantFault value
mapAlignmentProblem = either (const oracleFault) Right

mapAlignmentLossProblem ::
  Either AlignmentLoss.AlignmentLossProblem value ->
  Either HeraldInvariantFault value
mapAlignmentLossProblem = either (const oracleFault) Right

mapAlignmentLossInvariant ::
  Either AlignmentLoss.AlignmentLossInvariantProblem value ->
  Either HeraldInvariantFault value
mapAlignmentLossInvariant = either (const oracleFault) Right

mapAlignmentTransferProblem ::
  Either AlignmentTransfer.AlignmentTransferCoordinatorProblem value ->
  Either HeraldInvariantFault value
mapAlignmentTransferProblem = either (const oracleFault) Right

mapConsequenceProblem :: Either problem value -> Either HeraldInvariantFault value
mapConsequenceProblem = either (const oracleFault) Right

oracleFault :: Either HeraldInvariantFault value
oracleFault = Left (HeraldTransitionInvariant HeraldOracleTransitionContradiction)
