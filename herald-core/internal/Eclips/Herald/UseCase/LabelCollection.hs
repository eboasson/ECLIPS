-- | Direct terminal-report routing and the one membership-qualified Oracle
-- completion. These effects run at installation, report, membership and binding
-- changes; retained dispatch stamps make unchanged driver passes inert.
module Eclips.Herald.UseCase.LabelCollection
  ( beginInstallationCollection,
    wakeInstallationCollection,
    advanceInstallationCollection,
    receiveInstallationReport,
  ) where

import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Domain.Identity (LabelDecisionId)
import Eclips.Domain.Label
  ( LabelInstallationReport,
    labelInstallationControlIndex,
    labelInstallationDecisionId,
    labelInstallationOutcomeDigest,
    labelInstallationReport,
    labelInstallationReporter,
  )
import Eclips.Domain.Membership (heraldMembershipGenerationActiveHeraldEpochs, heraldMembershipGenerationId)
import Eclips.Herald.Discovery (PeerBinding, peerBindingGeneration, peerBindingGenerationWord64, peerBindingRemoteHeraldEpoch)
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch (HeraldEffect (..), PeerProtocolDisposition (ClosePeerControlProtocol))
import Eclips.Herald.Input (PeerControl (PeerLabelInstalled))
import Eclips.Herald.Label.Collection qualified as Collection
import Eclips.Herald.LabelBarrier.State qualified as Barrier
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Startup.Invariant (HeraldInvariantFault (..), HeraldTransitionInvariantViolation (..))
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupLabelBarrierState,
    replaceStartupOracleClientState,
    startupDiscoveryState,
    startupLabelBarrierState,
    startupOracleClientState,
    startupOracleProjectionState,
  )
import Eclips.Oracle.Label (labelCompletionAttestation, liveDecisionCapturedHeralds, liveDecisionHome)

collection :: HeraldState -> Collection.State
collection = Barrier.barrierCollectionState . startupLabelBarrierState

withCollection :: Collection.State -> HeraldState -> HeraldState
withCollection updated state = replaceStartupLabelBarrierState (Barrier.replaceBarrierCollectionState updated (startupLabelBarrierState state)) state

-- | A previously offered completion has acquired its immutable Oracle result.
-- A newer membership-qualified offer can be prepared on the next driver pass.
wakeInstallationCollection :: LabelDecisionId -> HeraldState -> HeraldState
wakeInstallationCollection decision state = withCollection (Collection.wakeDecision decision (collection state)) state

invariant :: Either error a -> Either HeraldInvariantFault a
invariant = either (const (Left (HeraldTransitionInvariant HeraldOracleTransitionContradiction))) Right

-- The local fact exists only after direct installation. Only the decision
-- installed by this transition is examined; unrelated collections are untouched.
beginInstallationCollection :: LabelDecisionId -> HeraldState -> Either HeraldInvariantFault HeraldState
beginInstallationCollection decision state = case (Collection.currentReport decision (collection state), Barrier.barrierTerminalEvidence decision barrier) of
  (Nothing, Just terminal) -> do
    workflow <- maybe (invariant (Left "missing projected label")) Right (Projection.oracleViewLabelWorkflow decision view)
    let opened = Projection.projectedLabelWorkflowDecision workflow
        report = labelInstallationReport decision (Barrier.barrierLocalHerald barrier) (Barrier.terminalOutcomeControlIndex terminal) (Barrier.terminalOutcomeDigest terminal)
    captured <- maybe (invariant (Left "missing captured membership")) Right (liveDecisionCapturedHeralds (Projection.oracleViewHeraldMembershipHistoryChecked view) opened)
    begun <- invariant (Collection.begin report (liveDecisionHome opened) (Set.fromList captured) (heraldMembershipGenerationId membership) (Set.fromList (toList (heraldMembershipGenerationActiveHeraldEpochs membership))) (collection state))
    Right (withCollection begun state)
  _ -> Right state
  where
    barrier = startupLabelBarrierState state
    view = Projection.oracleView (startupOracleProjectionState state)
    membership = Projection.oracleViewCurrentHeraldMembership view

receiveInstallationReport :: PeerBinding -> LabelInstallationReport -> HeraldState -> Either HeraldInvariantFault (HeraldState, [HeraldEffect])
receiveInstallationReport binding report state
  | labelInstallationReporter report /= peerBindingRemoteHeraldEpoch binding = Right (state, [RejectPeerConnection binding ClosePeerControlProtocol])
  | otherwise = do
      if labelInstallationControlIndex report > Projection.oracleViewControlIndex (Projection.oracleView (startupOracleProjectionState state))
        then case Collection.stage report (collection state) of
          Left _ -> Right (state, [RejectPeerConnection binding ClosePeerControlProtocol])
          Right updated -> Right (withCollection updated state, [])
        else admitReport report state

-- Already-completed reports can cross canonical completion in transit.
admitReport :: LabelInstallationReport -> HeraldState -> Either HeraldInvariantFault (HeraldState, [HeraldEffect])
admitReport report state
  | Projection.oracleViewLabelWorkflowCompleted (labelInstallationDecisionId report) view = Right (state, [])
  | otherwise = case Collection.observe report (collection state) of
      Right updated -> Right (withCollection updated state, [])
      Left _ -> Right (state, maybe [] (\binding -> [RejectPeerConnection binding ClosePeerControlProtocol]) (Discovery.currentPeerBinding (labelInstallationReporter report) (startupDiscoveryState state)))
  where
    view = Projection.oracleView (startupOracleProjectionState state)

advanceInstallationCollection :: HeraldState -> Either HeraldInvariantFault (HeraldState, [HeraldEffect])
advanceInstallationCollection initial = do
  let view = Projection.oracleView (startupOracleProjectionState initial)
      membership = Projection.oracleViewCurrentHeraldMembership view
      refreshed = Collection.refreshMembership (heraldMembershipGenerationId membership) (Set.fromList (toList (heraldMembershipGenerationActiveHeraldEpochs membership))) (collection initial)
      bindings = Map.fromList [(peerBindingRemoteHeraldEpoch binding, peerBindingGenerationWord64 (peerBindingGeneration binding)) | binding <- Discovery.currentPeerBindings (startupDiscoveryState initial)]
      rebound = Collection.refreshBindings bindings refreshed
      (withoutReady, reports) = Collection.takeReady (Projection.oracleViewControlIndex view) rebound
  (observed, reportEffects) <- foldM (\(state, effects) report -> do (next, emitted) <- admitReport report state; Right (next, effects <> emitted)) (withCollection withoutReady initial, []) reports
  foldM advanceOne (observed, reportEffects) (Set.toAscList (Collection.pendingWork (collection observed)))
  where
    advanceOne (state, effects) decision = do
      let consumed = withCollection (Collection.consumeWork decision (collection state)) state
          (dispatched, dispatchEffects) = dispatchReport decision consumed
      completed <- prepareCompletion decision dispatched
      Right (completed, effects <> dispatchEffects)

dispatchReport :: LabelDecisionId -> HeraldState -> (HeraldState, [HeraldEffect])
dispatchReport decision state = case (Collection.currentReport decision current, Collection.collector decision current) of
  (Just report, Just target)
    | Just binding <- Discovery.currentPeerBinding target (startupDiscoveryState state),
      let generation = peerBindingGenerationWord64 (peerBindingGeneration binding),
      Collection.reportNeedsDispatch decision target generation current ->
        (withCollection (Collection.markReportDispatched decision target generation current) state, [SendPeerControl binding (PeerLabelInstalled report)])
  _ -> (state, [])
  where
    current = collection state

prepareCompletion :: LabelDecisionId -> HeraldState -> Either HeraldInvariantFault HeraldState
prepareCompletion decision state = case (Collection.currentReport decision current, Collection.completionReady decision current) of
  (Just report, Just generation)
    | previousSettled report -> do
        let claim = claimFor report generation
        prepared <- either (const (invariant (Left "completion request contradiction"))) Right (Client.prepareLabelCompletionRequest claim client)
        let (nextClient, _, _) = Client.commitOracleRequest prepared
        Right (withCollection (Collection.markCompletionOffered decision current) (replaceStartupOracleClientState nextClient state))
  _ -> Right state
  where
    current = collection state
    client = startupOracleClientState state
    claimFor report generation = labelCompletionAttestation (labelInstallationDecisionId report) (labelInstallationControlIndex report) (labelInstallationOutcomeDigest report) (labelInstallationReporter report) generation
    previousSettled report = case Collection.offeredGeneration decision current of
      Nothing -> True
      Just generation -> case Client.lookupOracleRequestBySemanticKey (Client.labelCompletionSemanticKey (claimFor report generation)) client of
        Just witness -> case Client.oracleRequestWitnessStatus witness of
          Client.OracleRequestProjected _ -> True
          _ -> False
        Nothing -> False
