{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Atomic local and remote selected-child lifecycle. Oracle intent, grants, namespaces,
-- claim ownership, and requester results are installed before effects escape.
module Eclips.Herald.UseCase.ProcessPreparation
  ( applyProcessPreparationIngress,
    advanceProcessPreparations,
    advanceProcessPreparationsExhaustive,
    harvestPreparationChanges,
    sameProcessPreparationWorkState,
    applyInitialProcessClaim,
    applyPeerPreparationControl,
    childReadiness,
    requestPreparationCancellation,
    preparationCancellationStatus,
    retireClosedLifecycleReceipts,
  ) where

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Serialize.Put qualified as Put
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word64)
import Eclips.Application.Types.Lifecycle
import Eclips.Application.Types.Lifetime (lifecycleReceiptRetirement, lifecycleReceiptRetirementThrough)
import Eclips.Domain.Identity
import Eclips.Domain.ProcessLifecycle (ProcessEndReason (ExplicitAdministrativeEnd))
import Eclips.Domain.ProcessStart
import Eclips.Herald.Alignment.State qualified as Alignment
import Eclips.Herald.Application.Environment qualified as Environment
import Eclips.Herald.Application.Primordial qualified as Primordial
import Eclips.Herald.Application.Primordial.Transfer qualified as Transfer
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Discovery qualified as Discovery
import Eclips.Herald.Discovery.State qualified as DiscoveryState
import Eclips.Herald.EffectBatch
import Eclips.Herald.Genesis.Internal qualified as Genesis
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.IdGenerator.State qualified as Generator
import Eclips.Herald.Input
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.OracleClient.State qualified as Client
import Eclips.Herald.OracleProjection.State qualified as Projection
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.ProcessPreparation.Protocol
import Eclips.Herald.ProcessPreparation.Readiness qualified as Readiness
import Eclips.Herald.ProcessPreparation.State qualified as Preparation
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (HeraldTransitionInvariant),
    HeraldTransitionInvariantViolation (HeraldProcessPreparationTransitionContradiction),
  )
import Eclips.Herald.Startup.State
import Eclips.Herald.Store.State qualified as Store
import Eclips.Public.Types.ReceiptRetirement qualified as Retirement

applyProcessPreparationIngress :: ApplicationLifecycleIngress -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
applyProcessPreparationIngress ingress state = case ingress of
  RecoverApplicationLifecycleResult lane attachment request ->
    let reply = maybe (LifecycleAbsent request) (LifecycleReply request) (Preparation.ownEndRecovery attachment request owner)
     in Right (state, singletonEffectBatch (SendApplicationLifecycleReply (CandidateApplicationDisposition lane) reply))
  GetApplicationLifecycleResult binding session request -> case admittedSession binding session state of
    Nothing -> rejected binding request LifecycleRequestNotAdmitted state
    Just _
      | requestBelongsToSession request session,
        Just through <- retiredLifecycleRequest session request state ->
          retiredReply binding request through state
    Just process | requestBelongsToSession request session -> replyFor binding (Preparation.lifecycleKey process request) state
    Just _ -> rejected binding request LifecycleRequestNotAdmitted state
  CallApplicationLifecycle binding session request locator command -> case admittedSession binding session state of
    Nothing -> rejected binding request LifecycleRequestNotAdmitted state
    Just _ | not (requestBelongsToSession request session) -> rejected binding request LifecycleRequestNotAdmitted state
    Just _ | Just through <- retiredLifecycleRequest session request state -> retiredReply binding request through state
    Just process -> do
      let key = Preparation.lifecycleKey process request
      case Preparation.lookupRequest key owner of
        Just record
          | Preparation.requestCommand record /= command ->
              Right (state, singletonEffectBatch (SendApplicationLifecycleReply (EstablishedApplicationDisposition binding) (LifecycleConflict request)))
        Just _ -> replyFor binding key state
        Nothing -> do
          attachment <- require (Application.applicationSessionAttachment session (startupApplicationState state))
          let retain status predecessor = do
                retained <- checked (Preparation.retainRequest key command status session binding attachment (startupProcessPreparationState predecessor))
                pure (replaceStartupProcessPreparationState retained predecessor)
          accepted <- case command of
            BeginChild {} -> do
              pending <- retain (LifecyclePending []) state
              updateOwner (Preparation.retainDeferredBegin key locator) pending
            EndOwnProcess
              | not (locallyActive state) -> retain (LifecycleRejected LifecycleTargetUnavailable) state
              | not (Projection.oracleViewProcessIsLive process (oracleView state)) -> retain (LifecycleRejected LifecycleOwnProcessNotLive) state
              | otherwise -> retain (LifecyclePending []) state
            _
              | Just reference <- commandPreparation command,
                Just outgoing <- Preparation.lookupOutgoing reference owner ->
                  if Preparation.outgoingParent outgoing /= process
                    then retain (LifecycleRejected LifecycleNotPreparingParent) state
                    else do
                      initial <- retain (LifecyclePending []) state
                      if command == CancelChild reference
                        then fst <$> requestPreparationCancellation reference initial
                        else pure initial
            _ -> case commandPreparation command >>= (`Preparation.lookupPreparation` owner) of
              Nothing -> retain (LifecycleRejected LifecycleUnknownPreparation) state
              Just preparation
                | Preparation.preparationParent preparation /= process -> retain (LifecycleRejected LifecycleNotPreparingParent) state
                | otherwise -> do
                    initial <- retain (LifecyclePending []) state
                    case command of
                      CancelChild reference -> case Preparation.preparationPhase preparation of
                        Preparation.Attached -> settle key (LifecycleCompleted ChildAlreadyAttached) initial
                        Preparation.Terminal | Preparation.preparationCancelled preparation && Preparation.preparationFailure preparation == Nothing -> settle key (LifecycleCompleted ChildCancelled) initial
                        Preparation.Terminal -> settle key (LifecycleRejected LifecycleAlreadyTerminal) initial
                        _ -> do
                          sealed <- checked (Preparation.markPreparationCancelling reference (startupProcessPreparationState initial))
                          pure (replaceStartupProcessPreparationState sealed initial)
                      _ -> Right initial
          (advanced, effects) <- advanceProcessPreparations accepted
          (final, reply) <- replyFor binding key advanced
          pure (final, reply <> withoutReply request effects <> newRequestActions state accepted)
  where
    owner = startupProcessPreparationState state

retiredLifecycleRequest :: Session.ApplicationSessionId -> LifecycleRequestId -> HeraldState -> Maybe Word64
retiredLifecycleRequest session request state = do
  progress <- Application.applicationReceiptRetirementProgress session (startupApplicationState state)
  through <- lifecycleReceiptRetirementThrough progress
  if Retirement.receiptIsRetired (lifecycleRequestIdWord64 request) (lifecycleReceiptRetirement progress) then Just through else Nothing

retiredReply :: Session.ApplicationSessionBinding -> LifecycleRequestId -> Word64 -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
retiredReply binding request through state =
  Right (state, singletonEffectBatch (SendApplicationLifecycleReply (EstablishedApplicationDisposition binding) (LifecycleRetired request through)))

admittedSession :: Session.ApplicationSessionBinding -> Session.ApplicationSessionId -> HeraldState -> Maybe ProcessEpochId
admittedSession binding session state = do
  let application = startupApplicationState state
  current <- Application.applicationSessionCurrentBinding session application
  if current /= binding || Application.applicationSessionDeliveryLive session application /= Just True
    then Nothing
    else Application.applicationSessionProcess session application

replyFor :: Session.ApplicationSessionBinding -> Preparation.LifecycleKey -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
replyFor binding key state = do
  let request = Preparation.lifecycleKeyRequest key
  case Preparation.lookupRequest key (startupProcessPreparationState state) of
    Nothing -> Right (state, singletonEffectBatch (SendApplicationLifecycleReply (EstablishedApplicationDisposition binding) (LifecycleAbsent request)))
    Just record -> do
      successor <- updateOwner (Preparation.replaceRequestBinding key binding) state
      pure (successor, singletonEffectBatch (SendApplicationLifecycleReply (EstablishedApplicationDisposition binding) (LifecycleReply request (Preparation.requestStatus record))))

rejected :: Session.ApplicationSessionBinding -> LifecycleRequestId -> LifecycleError -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
rejected binding request problem state =
  Right
    ( state,
      singletonEffectBatch
        (SendApplicationLifecycleReply (EstablishedApplicationDisposition binding) (LifecycleReply request (LifecycleRejected problem)))
    )

commandPreparation :: LifecycleCommand -> Maybe ChildPreparation
commandPreparation = \case
  AwaitPreparedChild reference -> Just reference
  AwaitChildReady reference -> Just reference
  CancelChild reference -> Just reference
  _ -> Nothing

-- | Drive only already accepted lifecycle obligations. Readiness observations are
-- refreshed by control/structural progress, with no application call prerequisite.
data PreparationDriver = SelectivePreparationDriver | ExhaustivePreparationDriver
  deriving stock (Eq)

advanceProcessPreparations :: HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
advanceProcessPreparations = advanceProcessPreparationsWith SelectivePreparationDriver

-- | Independent original map inventories and uncached readiness/result queries.
-- Atomic transitions, effect assembly and lifecycle cleanup are shared.
advanceProcessPreparationsExhaustive :: HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
advanceProcessPreparationsExhaustive = advanceProcessPreparationsWith ExhaustivePreparationDriver

advanceProcessPreparationsWith :: PreparationDriver -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
advanceProcessPreparationsWith driver initial = do
  afterBegins <- foldM advanceDeferredBegin initial (Preparation.deferredBeginEntries (startupProcessPreparationState initial))
  afterOffers <- foldM advanceIncoming afterBegins (map fst (Preparation.incomingEntries (startupProcessPreparationState afterBegins)))
  (afterStarts, _) <- drivePreparationStage driver Preparation.PreparationStage (stageInventory driver Preparation.PreparationStage (map fst . Preparation.preparationEntries) afterOffers) (withoutEffects (advancePreparationWith driver)) afterOffers
  let requests = stageInventory driver Preparation.RequestStage (map fst . Preparation.requestEntries) afterStarts
  (afterOutgoing, _) <- drivePreparationStage driver Preparation.OutgoingStage (stageInventory driver Preparation.OutgoingStage (map fst . Preparation.outgoingEntries) afterStarts) (withoutEffects (advanceOutgoingWith driver)) afterStarts
  (afterRequests, _) <- drivePreparationStage driver Preparation.RequestStage requests (withoutEffects (advanceRequestWith driver (Readiness.prepareReadiness afterOutgoing))) afterOutgoing
  (afterClaims, claimEffects) <- drivePreparationStage driver Preparation.ClaimStage (stageInventory driver Preparation.ClaimStage (map fst . Preparation.pendingClaimEntries) afterRequests) (redriveClaim (Readiness.prepareReadiness afterRequests)) afterRequests
  (afterDelivery, remoteEffects) <- offerRemotePreparationsWith driver afterClaims
  -- Preserve own-End's final reply before releasing its closed-session receipt.
  let (changed, cleared) = Preparation.takeChangedRequestKeys (startupProcessPreparationState afterDelivery)
      effects = changedRepliesFor driver changed initial afterDelivery <> claimEffects <> remoteEffects <> newRequestActions initial afterDelivery
      withReplies = replaceStartupProcessPreparationState cleared afterDelivery
  reclaimed <- retireClosedLifecycleReceiptsWith driver withReplies
  pure (reclaimed, effects)
  where
    withoutEffects action state key = (,mempty) <$> action state key
    redriveClaim queries state (attachment, claim) = case Preparation.lookupPendingClaim attachment claim (startupProcessPreparationState state) of
      Nothing -> pure (state, mempty)
      Just lane -> do
        (next, emitted) <- claimAttachmentWithQueries driver queries lane attachment claim state
        let terminal = orderedEffectBatch [effect | effect <- effectBatchMembers emitted, case effect of SendInitialClaimPending {} -> False; _ -> True]
        pure (next, terminal)

stageInventory :: (Ord key) => PreparationDriver -> Preparation.WorkStage key -> (Preparation.State -> [key]) -> HeraldState -> Set.Set key
stageInventory SelectivePreparationDriver stage _ = Preparation.workIds stage . startupProcessPreparationState
stageInventory ExhaustivePreparationDriver _ entries = Set.fromList . entries . startupProcessPreparationState

-- Consume before the handler: self/backward wakeups survive for the next
-- original invocation; existing later keys are eligible in this frozen snapshot.
drivePreparationStage :: (Ord key) => PreparationDriver -> Preparation.WorkStage key -> Set.Set key -> (HeraldState -> key -> Either HeraldInvariantFault (HeraldState, EffectBatch)) -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
drivePreparationStage driver stage inventory action = go Nothing mempty
  where
    go cursor effects predecessor =
      let state = harvestPreparationChanges predecessor
          owner = startupProcessPreparationState state
          selected = case driver of
            SelectivePreparationDriver -> Preparation.nextWork stage inventory cursor owner
            ExhaustivePreparationDriver -> maybe (Set.lookupMin inventory) (`Set.lookupGT` inventory) cursor
       in case selected of
            Nothing -> pure (state, effects)
            Just key -> do
              let consumed = replaceStartupProcessPreparationState (Preparation.consumeWork stage key owner) state
              (successor, emitted) <- action consumed key
              registered <- refreshPreparationWorkDependencies stage key successor
              go (Just key) (effects <> emitted) registered

refreshPreparationWorkDependencies :: (Ord key) => Preparation.WorkStage key -> key -> HeraldState -> Either HeraldInvariantFault HeraldState
refreshPreparationWorkDependencies stage key state
  | Set.notMember key (Preparation.workIds stage owner) = pure state
  | otherwise = updateOwner (Preparation.replaceWorkDependencies stage key (dependencies stage key)) state
  where
    owner = startupProcessPreparationState state
    dependencies :: Preparation.WorkStage item -> item -> Set.Set Preparation.PreparationDependency
    dependencies Preparation.PreparationStage reference = case Preparation.lookupPreparation reference owner of
      Nothing -> Set.empty
      Just preparation ->
        Set.fromList [Preparation.PreparationChanged reference, Preparation.ActivityChanged, Preparation.OracleBindingChanged, Preparation.ProcessChanged (processStartProcessEpochId (Preparation.preparationStart preparation))]
          <> if Preparation.preparationPhase preparation == Preparation.Preparing && Preparation.preparationStartControl preparation /= Nothing
            then
              Readiness.preparationInstallationDependencies preparation
                <> Set.singleton (Preparation.ProcessChanged (Preparation.preparationParent preparation))
            else Set.empty
    dependencies Preparation.OutgoingStage reference = case Preparation.lookupOutgoing reference owner of
      Nothing -> Set.empty
      Just outgoing ->
        Set.fromList
          ( [Preparation.PreparationChanged reference, Preparation.ActivityChanged, Preparation.MembershipChanged, Preparation.ProcessChanged (Preparation.outgoingParent outgoing), Preparation.AttachmentChanged (Session.applicationAttachmentForProcess (Preparation.outgoingParent outgoing))] <> case Preparation.outgoingChild outgoing of
              Nothing -> []
              Just (RemoteChild process _ _) -> [Preparation.ProcessChanged process, Preparation.ObjectChanged (globalObjectIdFromProcessEpochId process), Preparation.PossessionChanged process (globalObjectIdFromProcessEpochId process)]
          )
    dependencies Preparation.RequestStage requestKey = case Preparation.lookupRequest requestKey owner of
      Nothing -> Set.empty
      Just record ->
        Set.fromList
          ( Preparation.SessionChanged (Preparation.requestSession record) : case Preparation.requestCommand record of
              EndOwnProcess
                | Preparation.requestEndReference record == Nothing ->
                    [Preparation.ProcessChanged (Preparation.lifecycleKeyProcess requestKey), Preparation.StructuralReadinessChanged]
              command -> case commandPreparation command of
                Nothing -> []
                Just reference -> [Preparation.PreparationChanged reference, Preparation.ReadinessChanged reference]
          )
    dependencies Preparation.ClaimStage (attachment, _) =
      Set.fromList [Preparation.AttachmentChanged attachment, Preparation.ActivityChanged, Preparation.GateChanged]
        <> case Preparation.preparationForAttachment attachment owner of
          Nothing -> maybe Set.empty (Set.singleton . Preparation.ProcessChanged) (soleGenesisClaimProcess attachment state)
          Just preparation -> Set.fromList [Preparation.PreparationChanged (Preparation.preparationReference preparation), Preparation.ReadinessChanged (Preparation.preparationReference preparation), Preparation.ProcessChanged (processStartProcessEpochId (Preparation.preparationStart preparation))]
    dependencies Preparation.OutgoingDeliveryStage reference = case Preparation.lookupOutgoing reference owner of
      Nothing -> Set.empty
      Just outgoing -> Set.fromList [Preparation.PreparationChanged reference, Preparation.ActivityChanged, Preparation.PeerBindingChanged (Preparation.outgoingTarget outgoing)]
    dependencies Preparation.IncomingDeliveryStage reference = case Preparation.lookupIncoming reference owner of
      Nothing -> Set.empty
      Just incoming -> Set.fromList [Preparation.PreparationChanged reference, Preparation.ReadinessChanged reference, Preparation.PeerBindingChanged (Preparation.incomingSource incoming)]

harvestPreparationChanges :: HeraldState -> HeraldState
harvestPreparationChanges predecessor
  | Set.null dependencies && Preparation.preparationContext owner == Just context = predecessor
  | otherwise =
      let queries = startupAlignmentCutQueryCache predecessor
       in queries
            `seq` replaceStartupAlignmentCutQueryCache
              queries
              ( replaceStartupProcessPreparationState
                  (Preparation.synchronizePreparationContext context (Preparation.notifyPreparationDependencies dependencies owner))
                  drained
              )
  where
    owner = startupProcessPreparationState predecessor
    (controlledChanges, controlled) = Controlled.takePreparationChanges (startupControlledState predecessor)
    (structuralChanged, progress) = GraphProgress.takePreparationReadinessChange (startupStructuralProgressState predecessor)
    (slots, stores) = Store.takePreparationSlotChanges (startupStoreState predecessor)
    (placed, placements) = Placement.takePreparationPlacementChanges (startupPlacementState predecessor)
    (sorts, alignment) = Alignment.takePreparationReadinessChanges (startupAlignmentState predecessor)
    (bindings, discovery) = DiscoveryState.takePreparationBindingChanges (startupDiscoveryState predecessor)
    (sessions, application) = Application.takePreparationSessionChanges (startupApplicationState predecessor)
    controlledChanged = not (Set.null controlledChanges.changedObjects && Set.null controlledChanges.changedProcesses && Set.null controlledChanges.changedPossessions)
    dependencies =
      Set.unions
        [ Set.map Preparation.ObjectChanged controlledChanges.changedObjects,
          Set.map Preparation.ProcessChanged controlledChanges.changedProcesses,
          Set.map (uncurry Preparation.PossessionChanged) controlledChanges.changedPossessions,
          if structuralChanged then Set.singleton Preparation.StructuralReadinessChanged else Set.empty,
          Set.map Preparation.DeltaChanged (slots <> placed),
          Set.map Preparation.SortReadinessChanged sorts,
          Set.map Preparation.PeerBindingChanged bindings,
          Set.map Preparation.SessionChanged sessions
        ]
    context = Preparation.PreparationContext (Projection.oracleViewCurrentHeraldMembershipId (oracleView predecessor)) (locallyActive predecessor) (Application.applicationGatePhase (startupApplicationState predecessor)) (Application.applicationMembershipGateClosed (startupApplicationState predecessor)) (Client.oracleClientCurrentBinding (startupOracleClientState predecessor))
    install whenChanged replace replacement state = if whenChanged then replace replacement state else state
    drained =
      install controlledChanged replaceStartupControlledState controlled
        . install structuralChanged replaceStartupStructuralProgressState progress
        . install (not (Set.null slots)) replaceStartupStoreState stores
        . install (not (Set.null placed)) replaceStartupPlacementState placements
        . install (not (Set.null sorts)) replaceStartupAlignmentState alignment
        . install (not (Set.null bindings)) replaceStartupDiscoveryState discovery
        . install (not (Set.null sessions)) replaceStartupApplicationState application
        $ predecessor

-- | Comparison-only erasure of schedules/caches/journals, never semantic joins,
-- result observation facts or authoritative state. Ordinary Eq stays complete.
sameProcessPreparationWorkState :: HeraldState -> HeraldState -> Bool
sameProcessPreparationWorkState left right = clearStartupPreparationScheduling left == clearStartupPreparationScheduling right

-- | Expired sessions cannot retry or consume observation waits. Keep pending
-- mutations until the semantic driver settles them, then release their terminal
-- receipts on its next pass. Session-loss handling may invoke this immediately
-- after expiration; the driver above invokes it after constructing final replies.
retireClosedLifecycleReceipts :: HeraldState -> Either HeraldInvariantFault HeraldState
retireClosedLifecycleReceipts = retireClosedLifecycleReceiptsWith SelectivePreparationDriver

retireClosedLifecycleReceiptsWith :: PreparationDriver -> HeraldState -> Either HeraldInvariantFault HeraldState
retireClosedLifecycleReceiptsWith driver predecessor
  | driver == SelectivePreparationDriver && Set.null candidates = pure state
  | otherwise =
      foldM
        (\successor session -> updateOwner (Preparation.retireClosedSessionLifecycleReceipts session) successor)
        (replaceStartupProcessPreparationState consumed state)
        (Set.toAscList closed)
  where
    state = harvestPreparationChanges predecessor
    owner = startupProcessPreparationState state
    (candidates, consumed) = Preparation.takeLifecycleRetirementSessions owner
    inventory = case driver of
      SelectivePreparationDriver -> candidates
      ExhaustivePreparationDriver -> Set.fromList [Preparation.requestSession record | (_, record) <- Preparation.requestEntries owner]
    application = startupApplicationState state
    closed = Set.filter (\session -> Application.applicationSessionProcess session application == Nothing) inventory

-- A gated Begin retains its call and return locator only. Selection admission,
-- source capture, child entropy, and Start intent all happen after reopening.
advanceDeferredBegin :: HeraldState -> (Preparation.LifecycleKey, HeraldLocator) -> Either HeraldInvariantFault HeraldState
advanceDeferredBegin state (key, locator)
  | Application.applicationMembershipGateClosed (startupApplicationState state) = Right state
  | otherwise = do
      record <- require (Preparation.lookupRequest key (startupProcessPreparationState state))
      case Preparation.requestCommand record of
        BeginChild target selection -> do
          let process = Preparation.lifecycleKeyProcess key
              request = Preparation.lifecycleKeyRequest key
              initial = replaceStartupProcessPreparationState (Preparation.removeDeferredBegin key (startupProcessPreparationState state)) state
              reject problem = settle key (LifecycleRejected problem) initial
          if target /= locator && DiscoveryState.resolveApplicationLocator target (startupDiscoveryState initial) == Nothing
            then reject LifecycleUnknownTarget
            else
              if not (locallyActive initial)
                then reject LifecycleTargetUnavailable
                else
                  if not (Projection.oracleViewProcessIsLive process (oracleView initial))
                    then reject LifecycleOwnProcessNotLive
                    else case Primordial.checkPrimordialSelection process selection (Application.applicationPrivateIdentity (startupApplicationState initial)) (startupControlledState initial) (startupStructuralProgressState initial) of
                      Left _ -> reject LifecycleSelectionNotAdmitted
                      Right grant -> do
                        reference <- checked (childPreparation (lifecycleRequestIdScopeBytes request) (lifecycleRequestIdSessionOrdinal request) (lifecycleRequestIdWord64 request))
                        if target == locator
                          then do
                            (start, generated) <- generateStart initial
                            preparations <- checked (Preparation.retainPreparation reference process locator grant start (startupProcessPreparationState initial))
                            settle key (LifecycleCompleted (ChildPreparationAccepted reference)) (replaceStartupProcessPreparationState preparations . replaceStartupIdGeneratorState generated $ initial)
                          else do
                            epoch <- require (DiscoveryState.resolveApplicationLocator target (startupDiscoveryState initial))
                            case Transfer.captureStartupTransfer process (Genesis.checkedLocalHeraldEpoch (startupGenesis initial)) grant initial of
                              Left _ -> reject LifecycleSelectionNotAdmitted
                              Right transfer -> do
                                let outgoing = Preparation.OutgoingPreparation process epoch target (Transfer.encodeStartupTransfer transfer) (RemotePreparationPending []) Nothing Nothing False Nothing Nothing
                                accepted <- updateOwner (Preparation.retainOutgoing reference outgoing) initial
                                settle key (LifecycleCompleted (ChildPreparationAccepted reference)) accepted
        _ -> contradiction

advancePreparationWith :: PreparationDriver -> HeraldState -> ChildPreparation -> Either HeraldInvariantFault HeraldState
advancePreparationWith driver initial reference = do
  initialPreparation <- require (Preparation.lookupPreparation reference (startupProcessPreparationState initial))
  state <- case (Preparation.preparationPhase initialPreparation, Preparation.preparationStartReference initialPreparation) of
    (Preparation.Cancelling, Nothing) -> updateOwner (Preparation.markPreparationTerminal reference) initial
    (Preparation.Preparing, Nothing) | locallyActive initial && isJust (Client.oracleClientCurrentBinding (startupOracleClientState initial)) -> do
      prepared <-
        checked
          ( Client.prepareStartProcessEpochRequest
              (preparationSemanticKey "ECLIPS-CHILD-START" reference)
              (Preparation.preparationStart initialPreparation)
              (startupOracleClientState initial)
          )
      let (client, _, oracleReference) = Client.commitOracleRequest prepared
      updateOwner
        (Preparation.markPreparationStartReference reference oracleReference)
        (replaceStartupOracleClientState client initial)
    _ -> pure initial
  preparation <- require (Preparation.lookupPreparation reference (startupProcessPreparationState state))
  let process = processStartProcessEpochId (Preparation.preparationStart preparation)
  observedState <- foldM (consumeObservedWith driver) state (Preparation.unobservedPreparationResults reference (startupProcessPreparationState state))
  if Preparation.preparationPhase preparation == Preparation.Terminal
    then pure observedState
    else
      if isJust (Projection.oracleViewEndedProcess process (oracleView state))
        then updateOwner (Preparation.markPreparationTerminal reference) observedState
        else
          if not (locallyActive state)
            then updateOwner (Preparation.markPreparationFailed reference StartupNotLive) observedState
            else do
              withStart <- case Preparation.preparationStartControl preparation of
                Just _ -> pure observedState
                Nothing -> do
                  observed <- case Preparation.preparationStartReference preparation of
                    Nothing -> pure Nothing
                    Just referenceValue -> observedResultWith driver referenceValue observedState
                  case observed of
                    Nothing -> pure observedState
                    Just (successor, Client.OracleRequestStarted start control)
                      | start == Preparation.preparationStart preparation -> updateOwner (Preparation.markPreparationStarted reference control) successor
                    Just (successor, Client.OracleRequestRejected _) -> updateOwner (if Preparation.preparationCancelled preparation then Preparation.markPreparationTerminal reference else Preparation.markPreparationFailed reference StartupNotLive) successor
                    _ -> contradiction
              retained <- require (Preparation.lookupPreparation reference (startupProcessPreparationState withStart))
              case (Preparation.preparationPhase retained, Preparation.preparationStartControl retained) of
                (Preparation.Preparing, Just _) -> installPreparationWith driver retained withStart
                (Preparation.Cancelling, Just _) -> case Preparation.preparationEndReference retained of
                  Nothing -> do
                    prepared <-
                      checked
                        ( Client.prepareEndProcessEpochRequest
                            (preparationSemanticKey "ECLIPS-CHILD-CANCEL" reference)
                            process
                            ExplicitAdministrativeEnd
                            (startupOracleClientState withStart)
                        )
                    let (client, _, endReference) = Client.commitOracleRequest prepared
                    updateOwner (Preparation.markPreparationEndReference reference endReference) (replaceStartupOracleClientState client withStart)
                  Just endReference -> do
                    observed <- observedResultWith driver endReference withStart
                    case observed of
                      Nothing -> pure withStart
                      Just (successor, Client.OracleRequestEnded _ _ _) -> updateOwner (Preparation.markPreparationTerminal reference) successor
                      Just (successor, Client.OracleRequestRejected _) -> updateOwner (Preparation.markPreparationFailed reference StartupNotLive) successor
                      _ -> contradiction
                _ -> pure withStart

installPreparationWith :: PreparationDriver -> Preparation.Preparation -> HeraldState -> Either HeraldInvariantFault HeraldState
installPreparationWith driver preparation state = case unavailable of
  key : _ -> do
    failed <- updateOwner (Preparation.markPreparationInstallationFailed (Preparation.preparationReference preparation) (StartupRequiredObjectUnavailable key)) state
    advancePreparationWith driver failed (Preparation.preparationReference preparation)
  [] -> installCurrentPreparation preparation state
  where
    unavailable =
      [ key
      | (key, entry) <- Map.toAscList (Primordial.primordialGrantEntries (Preparation.preparationGrant preparation)),
        case entry of Primordial.GlobalIdentity _ -> False; _ -> not (Controlled.controlledObjectCurrent (globalObjectIdFromGlobalUniqueId (Primordial.globalPrimordialIdentity entry)) (startupControlledState state))
      ]

installCurrentPreparation :: Preparation.Preparation -> HeraldState -> Either HeraldInvariantFault HeraldState
installCurrentPreparation preparation state = do
  let process = processStartProcessEpochId (Preparation.preparationStart preparation)
      parent = Preparation.preparationParent preparation
      reference = Preparation.preparationReference preparation
      attachment = Session.applicationAttachmentForProcess process
  controlledGrants <- checked (Controlled.prepareControlledGrants process (Primordial.primordialGrantPossessions (Preparation.preparationGrant preparation)) (startupControlledState state))
  applicationBootstrap <- checked (Application.prepareApplicationChildBootstrap attachment process (Preparation.preparationGrant preparation) (startupApplicationState state))
  let controlled = Controlled.commitControlledGrants controlledGrants
      (application, _) = Application.commitApplicationBootstrap applicationBootstrap
      childInstalled = replaceStartupControlledState controlled . replaceStartupApplicationState application $ state
  (installed, result) <-
    if Preparation.preparationSource preparation == Nothing
      && Projection.oracleViewProcessIsLive parent (oracleView state)
      && Application.applicationBootstrapAccess parent application /= Nothing
      then do
        nameGrant <- checked (Controlled.checkControlledGrant process (globalObjectIdFromProcessEpochId process) controlled)
        parentGrant <- checked (Controlled.prepareControlledGrants parent [nameGrant] controlled)
        alias <- checked (Application.prepareApplicationProcessAlias parent process application)
        let (parentApplication, privateProcess) = Application.commitApplicationProcessAlias alias
        descriptor <-
          checked
            ( connectionDescriptorWithTiming
                (startupTakeoverTarget state)
                (Preparation.preparationLocator preparation)
                (systemIdBytes (Genesis.checkedSystemId (startupGenesis state)))
                (heraldEpochBytes (Genesis.checkedLocalHeraldEpoch (startupGenesis state)))
                (processEpochIdBytes process)
            )
        pure
          ( replaceStartupApplicationState parentApplication . replaceStartupControlledState (Controlled.commitControlledGrants parentGrant) $ childInstalled,
            Just (preparedChild reference privateProcess descriptor)
          )
      else pure (childInstalled, Nothing)
  bootstrapped <- updateOwner (Right . Preparation.notifyPreparationDependencies (Set.singleton (Preparation.ProcessChanged process))) installed
  updateOwner (Preparation.markPreparationInstalled reference result) bootstrapped

-- | An accepted environment may still need its newly established writers to
-- publish the rest of its graph. Keep explicit own-End local until that work
-- completes, so the Oracle cannot retire those writers between its phases.
processHasPendingEnvironment :: ProcessEpochId -> HeraldState -> Bool
processHasPendingEnvironment process =
  any ((== process) . Environment.environmentManifestProcess . Environment.pendingEnvironmentManifest . snd)
    . Application.applicationPendingEnvironmentEntries
    . startupApplicationState

retainOwnEndRequest :: Preparation.LifecycleKey -> HeraldState -> Either HeraldInvariantFault HeraldState
retainOwnEndRequest key state = do
  let process = Preparation.lifecycleKeyProcess key
      request = Preparation.lifecycleKeyRequest key
  prepared <- checked (Client.prepareEndProcessEpochRequest (semanticKey "ECLIPS-OWN-END" process request) process ExplicitAdministrativeEnd (startupOracleClientState state))
  let (client, _, reference) = Client.commitOracleRequest prepared
  updateOwner
    (Preparation.retainOwnEndReference key reference)
    (replaceStartupOracleClientState client state)

advanceRequestWith :: PreparationDriver -> Readiness.PreparedReadiness -> HeraldState -> Preparation.LifecycleKey -> Either HeraldInvariantFault HeraldState
advanceRequestWith driver queries state key = do
  record <- require (Preparation.lookupRequest key (startupProcessPreparationState state))
  case Preparation.requestStatus record of
    LifecyclePending _ -> case Preparation.requestCommand record of
      BeginChild {} -> pure state
      EndOwnProcess -> case Preparation.requestEndReference record of
        Nothing
          | isJust (Projection.oracleViewEndedProcess (Preparation.lifecycleKeyProcess key) (oracleView state)) -> settle key (LifecycleCompleted ProcessEnded) state
          | processHasPendingEnvironment (Preparation.lifecycleKeyProcess key) state -> pure state
          | otherwise -> retainOwnEndRequest key state
        Just reference -> do
          observed <- observedResultWith driver reference state
          case observed of
            Nothing -> pure state
            Just (successor, Client.OracleRequestEnded _ _ _) -> updateOwner (Preparation.markResultObserved (Preparation.OwnEndResult key)) successor >>= settle key (LifecycleCompleted ProcessEnded)
            Just (successor, Client.OracleRequestRejected _) -> updateOwner (Preparation.markResultObserved (Preparation.OwnEndResult key)) successor >>= settle key (LifecycleRejected LifecycleOracleRejected)
            _ -> contradiction
      command
        | Just reference <- commandPreparation command,
          Just outgoing <- Preparation.lookupOutgoing reference (startupProcessPreparationState state) ->
            advanceRemoteRequest key command reference outgoing state
      command -> do
        reference <- require (commandPreparation command)
        preparation <- require (Preparation.lookupPreparation reference (startupProcessPreparationState state))
        let terminal = Preparation.preparationPhase preparation == Preparation.Terminal
        case command of
          AwaitPreparedChild _ | Just failure <- Preparation.preparationFailure preparation -> settle key (LifecycleRejected (LifecycleStartupFailed failure)) state
          AwaitPreparedChild _ -> case Preparation.preparationResult preparation of
            Just result -> settle key (LifecycleCompleted (ChildPrepared result)) state
            Nothing | terminal -> settle key (LifecycleRejected LifecycleAlreadyTerminal) state
            _ -> pure state
          AwaitChildReady _ -> do
            (observed, readiness) <- queryChildReadiness driver queries reference state
            case readiness of
              Left failure -> settle key (LifecycleRejected (LifecycleStartupFailed failure)) observed
              Right Nothing -> settle key (LifecycleCompleted ChildReady) observed
              Right (Just pending) -> settle key (LifecyclePending pending) observed
          CancelChild _ | terminal -> settle key (maybe (LifecycleCompleted ChildCancelled) (LifecycleRejected . LifecycleStartupFailed) (Preparation.preparationFailure preparation)) state
          CancelChild _ -> pure state
    _ -> pure state

observedResultWith :: PreparationDriver -> Client.OracleRequestRef -> HeraldState -> Either HeraldInvariantFault (Maybe (HeraldState, Client.OracleRequestResult))
observedResultWith driver reference state = do
  evidence <- case driver of
    ExhaustivePreparationDriver -> pure (Client.oracleClientRequestEvidence reference (startupOracleClientState state))
    SelectivePreparationDriver -> do
      witness <- require (Client.lookupOracleRequest reference (startupOracleClientState state))
      case Client.oracleRequestWitnessStatus witness of
        Client.OracleRequestAwaitingSourceDrain -> contradiction
        Client.OracleRequestEndedBeforeSubmission {} -> contradiction
        Client.OracleRequestAwaitingProjection -> pure Nothing
        Client.OracleRequestDeferred _ _ -> pure Nothing
        Client.OracleRequestProjected _ -> Just <$> require (Client.oracleClientRequestEvidence reference (startupOracleClientState state))
  case evidence of
    Nothing -> pure Nothing
    Just entry -> do
      prepared <- checked (Client.prepareOracleResult reference entry (startupOracleClientState state))
      let (client, _, result) = Client.commitOracleResult prepared
      pure (Just (replaceStartupOracleClientState client state, result))

queryChildReadiness :: PreparationDriver -> Readiness.PreparedReadiness -> ChildPreparation -> HeraldState -> Either HeraldInvariantFault (HeraldState, Either StartupError (Maybe [Text]))
queryChildReadiness driver queries reference predecessor =
  case (driver, Preparation.lookupReadiness reference (startupProcessPreparationState state)) of
    (SelectivePreparationDriver, Just readiness) -> pure (state, readiness)
    _ -> do
      preparation <- require (Preparation.lookupPreparation reference (startupProcessPreparationState state))
      let readiness = childReadinessWith queries reference state
          dependencies = Readiness.preparationReadinessDependencies preparation state
      successor <- updateOwner (Preparation.retainReadiness reference dependencies readiness) state
      pure (successor, readiness)
  where
    -- Every caller harvested at its stage/claim boundary; no owner read by the
    -- prepared query changes while this compact answer is being installed.
    state = predecessor

childReadiness :: ChildPreparation -> HeraldState -> Either StartupError (Maybe [Text])
childReadiness reference state = childReadinessWith (Readiness.prepareReadiness state) reference state

childReadinessWith :: Readiness.PreparedReadiness -> ChildPreparation -> HeraldState -> Either StartupError (Maybe [Text])
childReadinessWith queries reference state = do
  preparation <- maybe (Left StartupUnknownAttachment) Right (Preparation.lookupPreparation reference (startupProcessPreparationState state))
  let process = processStartProcessEpochId (Preparation.preparationStart preparation)
      grant = Preparation.preparationGrant preparation
  if Preparation.preparationCancelled preparation then Left StartupCancelled else pure ()
  case Preparation.preparationFailure preparation of { Just failure -> Left failure; Nothing -> pure () }
  if Preparation.preparationPhase preparation == Preparation.Terminal || not (locallyActive state) then Left StartupNotLive else pure ()
  let required = Set.toAscList (Primordial.primordialGrantRequiredEndpoints grant)
  if Preparation.preparationPhase preparation == Preparation.Preparing
    then readyResult (Just required)
    else do
      if not (Projection.oracleViewProcessIsLive process (oracleView state)) then Left StartupNotLive else pure ()
      let gate = case Application.applicationGatePhase (startupApplicationState state) of
            Application.ApplicationGateClosed _ -> Application.applicationMembershipGateClosed (startupApplicationState state) || not (null required)
            _ -> False
      pending <- traverse (readyEntry gate process grant) required
      let missing = [key | (key, False) <- pending]
      readyResult (if gate || not (null missing) then Just (if gate then required else missing) else Nothing)
  where
    -- Replies enter retained preparation bookkeeping and effects. Consume all
    -- query results here so neither can keep this batch or its predecessor.
    readyResult readiness = forceReadiness readiness `seq` Right readiness
    forceReadiness Nothing = ()
    forceReadiness (Just keys) = foldr (\key rest -> key `seq` rest) () keys
    readyEntry gate process grant key = do
      entry <- maybe (Left (StartupRequiredObjectUnavailable key)) Right (Map.lookup key (Primordial.primordialGrantEntries grant))
      let object = globalObjectIdFromGlobalUniqueId (Primordial.globalPrimordialIdentity entry)
          controlled = startupControlledState state
      if not (Controlled.controlledObjectCurrent object controlled) then Left (StartupRequiredObjectUnavailable key) else pure ()
      -- A closed gate still validates every required object, as before, but
      -- never demands its operation predicate: the reply contains all keys.
      let ready =
            not gate && case entry of
              Primordial.GlobalWriter writer _ -> Readiness.preparedRequiredWriterReady process writer queries
              Primordial.GlobalReader delta _ -> Readiness.preparedRequiredDeltaReady process delta queries
              _ -> False
      ready `seq` pure (key, ready)

applyInitialProcessClaim :: CandidateApplicationLane -> ConnectionDescriptor -> InitialClaimId -> Maybe HeraldLocator -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
applyInitialProcessClaim lane descriptor claim actualLocator state
  | connectionDescriptorLineageBytes descriptor /= systemIdBytes (Genesis.checkedSystemId (startupGenesis state)) = claimRejected StartupWrongLineage
  | connectionDescriptorEpochBytes descriptor /= heraldEpochBytes (Genesis.checkedLocalHeraldEpoch (startupGenesis state)) = claimRejected StartupWrongTarget
  | maybe False (/= connectionDescriptorLocator descriptor) actualLocator = claimRejected StartupWrongTarget
  | otherwise = do
      attachment <- checked (Session.applicationAttachmentFromClaimBytes (connectionDescriptorAttachmentBytes descriptor))
      case Preparation.preparationForAttachment attachment (startupProcessPreparationState state) of
        Just preparation | Preparation.preparationLocator preparation /= connectionDescriptorLocator descriptor -> claimRejected StartupWrongTarget
        Nothing | isJust (soleGenesisClaimProcess attachment state) && actualLocator /= Just (connectionDescriptorLocator descriptor) -> claimRejected StartupWrongTarget
        _ -> claimAttachment lane attachment claim state
  where
    claimRejected errorValue = Right (state, singletonEffectBatch (RejectInitialClaim lane claim errorValue))

claimAttachment :: CandidateApplicationLane -> Session.ApplicationAttachment -> InitialClaimId -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
claimAttachment = claimAttachmentWith SelectivePreparationDriver

claimAttachmentWith :: PreparationDriver -> CandidateApplicationLane -> Session.ApplicationAttachment -> InitialClaimId -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
claimAttachmentWith driver lane attachment claim predecessor = claimAttachmentWithQueries driver (Readiness.prepareReadiness predecessor) lane attachment claim predecessor

-- Claims change Application and preparation facts, but none of the owners held
-- by PreparedReadiness. The ordered claim stage shares this lazy view while
-- childReadinessWith still reads each current Application gate from its state.
claimAttachmentWithQueries :: PreparationDriver -> Readiness.PreparedReadiness -> CandidateApplicationLane -> Session.ApplicationAttachment -> InitialClaimId -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
claimAttachmentWithQueries driver queries lane attachment claim predecessor = do
  let state = harvestPreparationChanges predecessor
  (observed, readiness) <- case Preparation.preparationForAttachment attachment (startupProcessPreparationState state) of
    Just preparation | Preparation.preparationPhase preparation /= Preparation.Attached -> do
      (successor, answer) <- queryChildReadiness driver queries (Preparation.preparationReference preparation) state
      pure (successor, Just answer)
    _ -> pure (state, Nothing)
  claimAttachmentObserved lane attachment claim readiness observed

claimAttachmentObserved :: CandidateApplicationLane -> Session.ApplicationAttachment -> InitialClaimId -> Maybe (Either StartupError (Maybe [Text])) -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
claimAttachmentObserved lane attachment claim observedReadiness state = case Preparation.preparationForAttachment attachment owner of
  Nothing -> case soleGenesisClaimProcess attachment state of
    Nothing -> rejectedClaim StartupUnknownAttachment
    Just process
      | not (locallyActive state) || not (Projection.oracleViewProcessIsLive process (oracleView state)) -> rejectedClaim StartupNotLive
      | otherwise -> case Application.prepareApplicationGenesisInitialClaim
          (startupLastObservedTime state)
          (Genesis.checkedLocalHeraldEpoch (startupGenesis state))
          attachment
          process
          claim
          (startupApplicationState state) of
          Left (Application.ApplicationInitialClaimRejected problem) -> rejectedClaim problem
          Left _ -> contradiction
          Right prepared -> case Application.commitApplicationInitialClaim prepared of
            (application, Application.ApplicationInitialClaimOpened acceptance) ->
              pure
                ( replaceStartupProcessPreparationState (notifyAttachmentClaimed (Preparation.removePendingClaim attachment claim owner)) (replaceStartupApplicationState application state),
                  orderedEffectBatch (maybe [] (pure . CancelTimer) (Application.preparedApplicationInitialClaimTimerCancellation prepared) <> [SetApplicationConnectionDisposition lane acceptance])
                )
            (application, Application.ApplicationInitialClaimPending) ->
              pure
                ( replaceStartupProcessPreparationState (Preparation.retainPendingClaim attachment claim lane owner) (replaceStartupApplicationState application state),
                  singletonEffectBatch (SendInitialClaimPending lane claim [])
                )
  Just preparation -> case claimReadiness preparation of
    Left problem -> rejectedClaim problem
    Right readiness -> do
      let ready = readiness == Nothing
          pending = maybe [] id readiness
      if Preparation.preparationPhase preparation == Preparation.Preparing
        then pure (replaceStartupProcessPreparationState (Preparation.retainPendingClaim attachment claim lane owner) state, singletonEffectBatch (SendInitialClaimPending lane claim pending))
        else case Application.prepareApplicationInitialClaim
          (startupLastObservedTime state)
          (Genesis.checkedLocalHeraldEpoch (startupGenesis state))
          attachment
          claim
          ready
          (startupApplicationState state) of
          Left (Application.ApplicationInitialClaimRejected problem) -> rejectedClaim problem
          Left _ -> contradiction
          Right prepared -> do
            let (application, outcome) = Application.commitApplicationInitialClaim prepared
                withApplication = replaceStartupApplicationState application state
            case outcome of
              Application.ApplicationInitialClaimPending ->
                let retained = Preparation.retainPendingClaim attachment claim lane owner
                 in Right
                      ( replaceStartupProcessPreparationState retained withApplication,
                        singletonEffectBatch (SendInitialClaimPending lane claim pending)
                      )
              Application.ApplicationInitialClaimOpened acceptance -> do
                attached <- checked (Preparation.markPreparationAttached (Preparation.preparationReference preparation) owner)
                let settled = notifyAttachmentClaimed (Preparation.removePendingClaim attachment claim attached)
                    cancellations = maybe [] (pure . CancelTimer) (Application.preparedApplicationInitialClaimTimerCancellation prepared)
                pure
                  ( replaceStartupProcessPreparationState settled withApplication,
                    orderedEffectBatch (cancellations <> [SetApplicationConnectionDisposition lane acceptance])
                  )
  where
    owner = startupProcessPreparationState state
    notifyAttachmentClaimed
      | Application.applicationInitialClaimIsClaimed attachment (startupApplicationState state) = id
      | otherwise = Preparation.notifyPreparationDependencies (Set.singleton (Preparation.AttachmentChanged attachment))
    claimReadiness preparation
      | Preparation.preparationPhase preparation == Preparation.Attached =
          if locallyActive state && Projection.oracleViewProcessIsLive (processStartProcessEpochId (Preparation.preparationStart preparation)) (oracleView state)
            then Right Nothing
            else Left StartupNotLive
      | otherwise = case observedReadiness of Just answer -> answer; Nothing -> Left StartupUnknownAttachment
    rejectedClaim problem =
      Right
        ( replaceStartupProcessPreparationState (Preparation.removePendingClaim attachment claim owner) state,
          singletonEffectBatch (RejectInitialClaim lane claim problem)
        )

soleGenesisClaimProcess :: Session.ApplicationAttachment -> HeraldState -> Maybe ProcessEpochId
soleGenesisClaimProcess attachment state = case Projection.projectedBootstraps (startupOracleProjectionState state) of
  [(process, bootstrap)]
    | Genesis.appliedProcessResidence bootstrap == Genesis.checkedLocalHeraldEpoch (startupGenesis state),
      attachment == Session.applicationAttachmentForBootstrap (Genesis.appliedBootstrapManifestId bootstrap) ->
        Just process
  _ -> Nothing

changedRepliesFor :: PreparationDriver -> Set.Set Preparation.LifecycleKey -> HeraldState -> HeraldState -> EffectBatch
changedRepliesFor driver changed before after =
  orderedEffectBatch
    [ SendApplicationLifecycleReply
        (EstablishedApplicationDisposition (Preparation.requestBinding record))
        (LifecycleReply (Preparation.lifecycleKeyRequest key) (Preparation.requestStatus record))
    | key <- case driver of SelectivePreparationDriver -> Set.toAscList changed; ExhaustivePreparationDriver -> map fst (Preparation.requestEntries (startupProcessPreparationState after)),
      Just record <- [Preparation.lookupRequest key (startupProcessPreparationState after)],
      Just old <- [Preparation.lookupRequest key (startupProcessPreparationState before)],
      Preparation.requestStatus old /= Preparation.requestStatus record,
      Preparation.requestCommand record == EndOwnProcess
        || admittedSession (Preparation.requestBinding record) (Preparation.requestSession record) after == Just (Preparation.lifecycleKeyProcess key)
    ]

locallyActive :: HeraldState -> Bool
locallyActive state =
  Projection.oracleViewLocalHeraldIsCurrent (oracleView state)
    && Isolation.isolationWitnessPhase (Isolation.stateWitness (startupIsolationState state)) `notElem` [Isolation.IsolationReadOnlyDrainView, Isolation.IsolationTerminalView]
oracleView :: HeraldState -> Projection.View
oracleView = Projection.oracleView . startupOracleProjectionState
requestActions :: HeraldState -> EffectBatch
requestActions = orderedEffectBatch . fmap RunOracleClientAction . Client.oracleClientRequestActions . startupOracleClientState
newRequestActions :: HeraldState -> HeraldState -> EffectBatch
newRequestActions before after
  | Client.oracleClientNextRequestSequence (startupOracleClientState before) /= Client.oracleClientNextRequestSequence (startupOracleClientState after) = requestActions after
  | otherwise = mempty
semanticKey :: ByteString -> ProcessEpochId -> LifecycleRequestId -> ByteString
semanticKey tag process request = tag <> processEpochIdBytes process <> lifecycleRequestIdScopeBytes request <> Put.runPut (Put.putWord64be (lifecycleRequestIdSessionOrdinal request) >> Put.putWord64be (lifecycleRequestIdWord64 request))
preparationSemanticKey :: ByteString -> ChildPreparation -> ByteString
preparationSemanticKey tag reference = tag <> childPreparationScopeBytes reference <> Put.runPut (Put.putWord64be (childPreparationSessionOrdinal reference) >> Put.putWord64be (childPreparationOrdinal reference))
generateStart :: HeraldState -> Either HeraldInvariantFault (ProcessStart, Generator.State)
generateStart state = do
  first <- checked (Generator.prepareGeneratedId (startupIdGeneratorState state))
  second <- checked (Generator.prepareGeneratedId (Generator.commitGeneratedId first))
  process <- checked (mkProcessId (globalUniqueIdBytes (Generator.preparedGlobalUniqueId first)))
  epoch <- checked (mkProcessEpochId (globalUniqueIdBytes (Generator.preparedGlobalUniqueId second)))
  pure (processStart process epoch (Genesis.checkedLocalHeraldEpoch (startupGenesis state)), Generator.commitGeneratedId second)
settle :: Preparation.LifecycleKey -> LifecycleStatus -> HeraldState -> Either HeraldInvariantFault HeraldState
settle key status = updateOwner (Preparation.settleRequest key status)
updateOwner :: (Preparation.State -> Either problem Preparation.State) -> HeraldState -> Either HeraldInvariantFault HeraldState
updateOwner transition state = do
  successor <- checked (transition (startupProcessPreparationState state))
  pure (replaceStartupProcessPreparationState successor state)
checked :: Either problem value -> Either HeraldInvariantFault value
checked = either (const contradiction) Right
require :: Maybe value -> Either HeraldInvariantFault value
require = maybe contradiction Right
contradiction :: Either HeraldInvariantFault value
contradiction = Left (HeraldTransitionInvariant HeraldProcessPreparationTransitionContradiction)

withoutReply :: LifecycleRequestId -> EffectBatch -> EffectBatch
withoutReply request = orderedEffectBatch . filter keep . effectBatchMembers
  where
    keep (SendApplicationLifecycleReply _ (LifecycleReply observed _)) = observed /= request
    keep _ = True

requestBelongsToSession :: LifecycleRequestId -> Session.ApplicationSessionId -> Bool
requestBelongsToSession request session =
  lifecycleRequestIdScopeBytes request == heraldEpochBytes (Session.applicationSessionIdHeraldEpoch session)
    && lifecycleRequestIdSessionOrdinal request == Session.applicationSessionIdOrdinal session
consumeObservedWith :: PreparationDriver -> HeraldState -> (Preparation.PreparationResultConsumer, Client.OracleRequestRef) -> Either HeraldInvariantFault HeraldState
consumeObservedWith driver state (consumer, reference) = do
  observed <- observedResultWith driver reference state
  case observed of
    Nothing -> pure state
    Just (successor, _) -> updateOwner (Preparation.markResultObserved consumer) successor

-- | The administration adapter shares the same serialized seal and claim winner.
-- Its own request owner retains administrative correlation and observes this
-- status after the ordinary lifecycle driver advances ordered Start/End work.
requestPreparationCancellation :: ChildPreparation -> HeraldState -> Either HeraldInvariantFault (HeraldState, LifecycleStatus)
requestPreparationCancellation reference state
  | Just outgoing <- Preparation.lookupOutgoing reference (startupProcessPreparationState state) = do
      let retryCleanup = case Preparation.outgoingStatus outgoing of
            RemotePreparationFailed (StartupRequiredObjectUnavailable _) -> Projection.oracleViewIsActiveHerald (Preparation.outgoingTarget outgoing) (oracleView state)
            _ -> False
      successor <- updateOwner (Preparation.cancelOutgoing reference (if retryCleanup then Preparation.RetryRemoteCleanup else Preparation.OrdinaryCancellation)) state
      pure (successor, preparationCancellationStatus reference successor)
  | Just incoming <- Preparation.lookupIncoming reference (startupProcessPreparationState state),
    Preparation.lookupPreparation reference (startupProcessPreparationState state) == Nothing = do
      successor <- updateOwner (Preparation.sealIncomingCancellation reference (Preparation.incomingSource incoming)) state
      pure (successor, LifecycleCompleted ChildCancelled)
  | otherwise = case Preparation.lookupPreparation reference (startupProcessPreparationState state) of
      Just preparation | Preparation.preparationPhase preparation `elem` [Preparation.Preparing, Preparation.Prepared, Preparation.Cancelling] -> do
        successor <- updateOwner (Preparation.markPreparationCancelling reference) state
        pure (successor, preparationCancellationStatus reference successor)
      _ -> pure (state, preparationCancellationStatus reference state)

preparationCancellationStatus :: ChildPreparation -> HeraldState -> LifecycleStatus
preparationCancellationStatus reference state
  | Just outgoing <- Preparation.lookupOutgoing reference (startupProcessPreparationState state) = remoteCancellationStatus (Preparation.outgoingStatus outgoing)
  | Just incoming <- Preparation.lookupIncoming reference (startupProcessPreparationState state),
    Preparation.lookupPreparation reference (startupProcessPreparationState state) == Nothing =
      if Preparation.incomingCancelled incoming then LifecycleCompleted ChildCancelled else maybe (LifecyclePending []) (LifecycleRejected . LifecycleStartupFailed) (Preparation.incomingFailure incoming)
  | otherwise = case Preparation.lookupPreparation reference (startupProcessPreparationState state) of
      Nothing -> LifecycleRejected LifecycleUnknownPreparation
      Just preparation -> case Preparation.preparationPhase preparation of
        Preparation.Attached -> LifecycleCompleted ChildAlreadyAttached
        Preparation.Terminal -> case Preparation.preparationFailure preparation of
          Just failure -> LifecycleRejected (LifecycleStartupFailed failure)
          Nothing | Preparation.preparationCancelled preparation -> LifecycleCompleted ChildCancelled
          _ -> LifecycleRejected LifecycleAlreadyTerminal
        _ -> LifecyclePending []

-- | An admitted peer lane supplies the source/target epoch. The payload itself
-- carries no authority to create a private alias or a process fact.
applyPeerPreparationControl :: Discovery.PeerBinding -> PreparationControl -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
applyPeerPreparationControl binding control state
  | DiscoveryState.currentPeerBinding peer (startupDiscoveryState state) /= Just binding = pure (state, mempty)
  | not (Projection.oracleViewIsActiveHerald peer (oracleView state)) = pure (state, mempty)
  | otherwise = case control of
      PreparationReplied reference status -> case Preparation.lookupOutgoing reference owner of
        Nothing -> pure (state, mempty)
        Just outgoing | Preparation.outgoingTarget outgoing /= peer -> pure (state, mempty)
        Just outgoing -> do
          valid <- validateRemoteReply outgoing status state
          if not valid || remoteTerminal (Preparation.outgoingStatus outgoing)
            then pure (state, mempty)
            else do
              retained <- updateOwner (Preparation.receiveOutgoingStatus reference status) state
              advanceProcessPreparations retained
      _ | childPreparationScopeBytes (controlReference control) /= heraldEpochBytes peer -> pure (state, mempty)
      PreparationOffered reference locator bytes -> case Transfer.decodeStartupTransfer bytes of
        Left _ -> reply reference (RemotePreparationFailed StartupUnknownAttachment) state
        Right transfer | Transfer.transferSourceHerald transfer /= peer -> reply reference (RemotePreparationFailed StartupWrongTarget) state
        Right _ -> do
          case Preparation.lookupIncoming reference owner of
            Just incoming | Preparation.incomingSource incoming /= peer -> reply reference (RemotePreparationFailed StartupWrongTarget) state
            Just incoming | Just previous <- Preparation.incomingOffer incoming, previous /= (locator, bytes) -> reply reference (RemotePreparationFailed StartupAlreadyClaimed) state
            _ -> do
              retained <- updateOwner (Preparation.retainIncomingOffer reference peer locator bytes) state
              advanceProcessPreparations retained
      PreparationCancelled reference -> do
        retained <- updateOwner (Preparation.sealIncomingCancellation reference peer) state
        (sealed, _) <- requestPreparationCancellation reference retained
        advanceProcessPreparations sealed
      PreparationQueried reference -> case Preparation.lookupIncoming reference owner of
        Nothing -> reply reference (RemotePreparationFailed StartupUnknownAttachment) state
        Just _ -> do
          retained <- updateOwner (Preparation.requestIncomingReply reference) state
          advanceProcessPreparations retained
  where
    owner = startupProcessPreparationState state
    peer = Discovery.peerBindingRemoteHeraldEpoch binding
    reply reference status successor = pure (successor, singletonEffectBatch (SendPeerControl binding (PeerPreparationControl (PreparationReplied reference status))))

controlReference :: PreparationControl -> ChildPreparation
controlReference = \case
  PreparationOffered reference _ _ -> reference
  PreparationCancelled reference -> reference
  PreparationQueried reference -> reference
  PreparationReplied reference _ -> reference

advanceIncoming :: HeraldState -> ChildPreparation -> Either HeraldInvariantFault HeraldState
advanceIncoming state reference = do
  incoming <- require (Preparation.lookupIncoming reference (startupProcessPreparationState state))
  case Preparation.lookupPreparation reference (startupProcessPreparationState state) of
    Just preparation
      | Preparation.incomingCancelled incoming && Preparation.preparationPhase preparation `elem` [Preparation.Preparing, Preparation.Prepared] ->
          updateOwner (Preparation.markPreparationCancelling reference) state
    Just _ -> pure state
    Nothing
      | Preparation.incomingCancelled incoming || isJust (Preparation.incomingFailure incoming) -> pure state
      | not (locallyActive state) -> failIncoming StartupNotLive
      | otherwise -> case Preparation.incomingOffer incoming of
          Nothing -> pure state
          Just (locator, bytes) -> case DiscoveryState.applicationLocatorFor local (startupDiscoveryState state) of
            Nothing -> pure state
            Just actual | actual /= locator -> failIncoming StartupWrongTarget
            Just _ -> do
              transfer <- checked (Transfer.decodeStartupTransfer bytes)
              case Transfer.admitStartupTransfer (Preparation.incomingSource incoming) transfer state of
                Left _ -> failIncoming StartupUnknownAttachment
                Right Nothing -> pure state
                Right (Just (grant, admitted)) -> do
                  (start, generated) <- generateStart admitted
                  updateOwner
                    (Preparation.retainRemotePreparation (Preparation.incomingSource incoming) reference (Transfer.transferParent transfer) locator grant start)
                    (replaceStartupIdGeneratorState generated admitted)
  where
    local = Genesis.checkedLocalHeraldEpoch (startupGenesis state)
    failIncoming failure = updateOwner (Preparation.failIncoming reference failure) state

advanceOutgoingWith :: PreparationDriver -> HeraldState -> ChildPreparation -> Either HeraldInvariantFault HeraldState
advanceOutgoingWith driver state reference = do
  outgoing <- require (Preparation.lookupOutgoing reference (startupProcessPreparationState state))
  let status = Preparation.outgoingStatus outgoing
      replace statusValue = updateOwner (Preparation.receiveOutgoingStatus reference statusValue) state
  if remoteTerminal status
    then pure state
    else
      if not (locallyActive state) || not (Projection.oracleViewIsActiveHerald (Preparation.outgoingTarget outgoing) (oracleView state))
        then replace (RemotePreparationFailed StartupNotLive)
        else
          if Preparation.outgoingCancelled outgoing && Preparation.outgoingOfferBinding outgoing == Nothing
            then replace RemotePreparationCancelled
            else case Preparation.outgoingChild outgoing of
              Nothing -> pure state
              Just child@(RemoteChild process control descriptor)
                | isJust (Projection.oracleViewEndedProcess process (oracleView state)) -> replace (RemotePreparationFailed StartupNotLive)
                | Preparation.outgoingResult outgoing /= Nothing -> pure state
                | otherwise -> case (case driver of SelectivePreparationDriver -> Projection.oracleViewStartedProcess process (oracleView state); ExhaustivePreparationDriver -> lookup process (Projection.projectedStartedProcesses (startupOracleProjectionState state))) of
                    Nothing -> pure state
                    Just started
                      | Projection.projectedStartedProcessControlIndex started /= control
                          || processStartResidence (Projection.projectedStartedProcessBootstrap started) /= Preparation.outgoingTarget outgoing ->
                          contradiction
                      | not (Projection.oracleViewProcessIsLive parent (oracleView state)) || Application.applicationBootstrapAccess parent application == Nothing -> pure state
                      | otherwise -> do
                          valid <- validateRemoteChild outgoing child state
                          if not valid then contradiction else pure ()
                          nameGrant <- checked (Controlled.checkControlledGrant process (globalObjectIdFromProcessEpochId process) (startupControlledState state))
                          parentGrant <- checked (Controlled.prepareControlledGrants parent [nameGrant] (startupControlledState state))
                          alias <- checked (Application.prepareApplicationProcessAlias parent process application)
                          let (installed, privateProcess) = Application.commitApplicationProcessAlias alias
                          updateOwner
                            (Preparation.retainOutgoingResult reference (preparedChild reference privateProcess descriptor))
                            (replaceStartupControlledState (Controlled.commitControlledGrants parentGrant) . replaceStartupApplicationState installed $ state)
                      where
                        parent = Preparation.outgoingParent outgoing
                        application = startupApplicationState state

advanceRemoteRequest :: Preparation.LifecycleKey -> LifecycleCommand -> ChildPreparation -> Preparation.OutgoingPreparation -> HeraldState -> Either HeraldInvariantFault HeraldState
advanceRemoteRequest key command _ outgoing state = case command of
  CancelChild _ -> settle key (remoteCancellationStatus status) state
  AwaitPreparedChild _ -> case status of
    RemotePreparationFailed failure -> settle key (LifecycleRejected (LifecycleStartupFailed failure)) state
    RemotePreparationCancelled -> settle key (LifecycleRejected (LifecycleStartupFailed StartupCancelled)) state
    _ -> case Preparation.outgoingResult outgoing of
      Just result -> settle key (LifecycleCompleted (ChildPrepared result)) state
      Nothing -> pure state
  AwaitChildReady _ -> case status of
    RemotePreparationAvailable _ Nothing | Preparation.outgoingResult outgoing /= Nothing -> settle key (LifecycleCompleted ChildReady) state
    RemotePreparationClaimed _ | Preparation.outgoingResult outgoing /= Nothing -> settle key (LifecycleCompleted ChildReady) state
    RemotePreparationAvailable _ (Just keys) -> settle key (LifecyclePending keys) state
    RemotePreparationPending keys -> settle key (LifecyclePending keys) state
    RemotePreparationFailed failure -> settle key (LifecycleRejected (LifecycleStartupFailed failure)) state
    RemotePreparationCancelled -> settle key (LifecycleRejected (LifecycleStartupFailed StartupCancelled)) state
    _ -> pure state
  _ -> contradiction
  where
    status = Preparation.outgoingStatus outgoing

remoteCancellationStatus :: RemotePreparationStatus -> LifecycleStatus
remoteCancellationStatus = \case
  RemotePreparationClaimed _ -> LifecycleCompleted ChildAlreadyAttached
  RemotePreparationCancelled -> LifecycleCompleted ChildCancelled
  RemotePreparationFailed failure -> LifecycleRejected (LifecycleStartupFailed failure)
  _ -> LifecyclePending []
remoteTerminal :: RemotePreparationStatus -> Bool
remoteTerminal = \case
  RemotePreparationCancelled -> True
  RemotePreparationFailed _ -> True
  _ -> False
remoteStatusChild :: RemotePreparationStatus -> Maybe RemoteChild
remoteStatusChild = \case
  RemotePreparationAvailable child _ -> Just child
  RemotePreparationClaimed child -> Just child
  _ -> Nothing

validateRemoteReply :: Preparation.OutgoingPreparation -> RemotePreparationStatus -> HeraldState -> Either HeraldInvariantFault Bool
validateRemoteReply outgoing status state = do
  valid <- maybe (pure True) (\child -> validateRemoteChild outgoing child state) (remoteStatusChild status)
  let same = case (Preparation.outgoingChild outgoing, remoteStatusChild status) of
        (Just previous, Just observed) -> previous == observed
        (Just _, Nothing) -> remoteTerminal status
        _ -> True
      monotoneClaim = case Preparation.outgoingStatus outgoing of
        RemotePreparationClaimed _ -> case status of RemotePreparationClaimed _ -> True; _ -> remoteTerminal status
        _ -> True
  pure (valid && same && monotoneClaim)
validateRemoteChild :: Preparation.OutgoingPreparation -> RemoteChild -> HeraldState -> Either HeraldInvariantFault Bool
validateRemoteChild outgoing (RemoteChild process _ descriptor) state =
  pure
    ( connectionDescriptorLocator descriptor == Preparation.outgoingLocator outgoing
        && connectionDescriptorEpochBytes descriptor == heraldEpochBytes (Preparation.outgoingTarget outgoing)
        && connectionDescriptorLineageBytes descriptor == systemIdBytes (Genesis.checkedSystemId (startupGenesis state))
        && connectionDescriptorAttachmentBytes descriptor == processEpochIdBytes process
    )

-- Each delivery stage preserves its old entry snapshot. Incoming delivery first
-- tests transport availability; a disconnected dirty cache remains unevaluated.
offerRemotePreparationsWith :: PreparationDriver -> HeraldState -> Either HeraldInvariantFault (HeraldState, EffectBatch)
offerRemotePreparationsWith driver initial = do
  (afterOutgoing, offers) <- drivePreparationStage driver Preparation.OutgoingDeliveryStage (stageInventory driver Preparation.OutgoingDeliveryStage (map fst . Preparation.outgoingEntries) initial) offerOutgoing initial
  let queries = Readiness.prepareReadiness afterOutgoing
  (successor, replies) <- drivePreparationStage driver Preparation.IncomingDeliveryStage (stageInventory driver Preparation.IncomingDeliveryStage (map fst . Preparation.incomingEntries) afterOutgoing) (offerIncoming queries) afterOutgoing
  pure (successor, offers <> replies)
  where
    offerOutgoing state reference = do
      outgoing <- require (Preparation.lookupOutgoing reference (startupProcessPreparationState state))
      if not (locallyActive state) || remoteTerminal (Preparation.outgoingStatus outgoing)
        then pure (state, mempty)
        else case DiscoveryState.currentPeerBinding (Preparation.outgoingTarget outgoing) (startupDiscoveryState state) of
          Nothing -> pure (state, mempty)
          Just binding -> do
            let offerNeeded = Preparation.outgoingOfferBinding outgoing /= Just binding
                cancelNeeded = Preparation.outgoingCancelled outgoing && Preparation.outgoingCancelBinding outgoing /= Just binding
                offer = case Preparation.outgoingChild outgoing of
                  Nothing -> PreparationOffered reference (Preparation.outgoingLocator outgoing) (Preparation.outgoingTransfer outgoing)
                  Just _ -> PreparationQueried reference
                controls = [offer | offerNeeded] <> [PreparationCancelled reference | cancelNeeded]
            successor <- updateOwner (Preparation.recordOutgoingDelivery reference binding cancelNeeded) state
            pure (successor, orderedEffectBatch [SendPeerControl binding (PeerPreparationControl control) | control <- controls])
    offerIncoming queries state reference = do
      incoming <- require (Preparation.lookupIncoming reference (startupProcessPreparationState state))
      case DiscoveryState.currentPeerBinding (Preparation.incomingSource incoming) (startupDiscoveryState state) of
        Nothing -> pure (state, mempty)
        Just binding -> do
          (observed, status) <- incomingStatusWith driver queries reference incoming state
          if Preparation.incomingLastReply incoming == Just (binding, status)
            then pure (observed, mempty)
            else do
              successor <- updateOwner (Preparation.recordIncomingReply reference binding status) observed
              pure (successor, singletonEffectBatch (SendPeerControl binding (PeerPreparationControl (PreparationReplied reference status))))

incomingStatusWith :: PreparationDriver -> Readiness.PreparedReadiness -> ChildPreparation -> Preparation.IncomingPreparation -> HeraldState -> Either HeraldInvariantFault (HeraldState, RemotePreparationStatus)
incomingStatusWith driver queries reference incoming state = case Preparation.lookupPreparation reference (startupProcessPreparationState state) of
  Nothing
    | Just failure <- Preparation.incomingFailure incoming -> unchanged (RemotePreparationFailed failure)
    | Preparation.incomingCancelled incoming -> unchanged RemotePreparationCancelled
    | otherwise -> unchanged (RemotePreparationPending [])
  Just preparation -> case Preparation.preparationPhase preparation of
    Preparation.Terminal -> unchanged $ case Preparation.preparationFailure preparation of
      Just failure -> RemotePreparationFailed failure
      Nothing | Preparation.preparationCancelled preparation -> RemotePreparationCancelled
      _ -> RemotePreparationFailed StartupNotLive
    Preparation.Preparing -> unchanged (RemotePreparationPending (Set.toAscList (Primordial.primordialGrantRequiredEndpoints (Preparation.preparationGrant preparation))))
    Preparation.Cancelling -> unchanged (RemotePreparationPending [])
    phase -> do
      let process = processStartProcessEpochId (Preparation.preparationStart preparation)
      control <- require (Preparation.preparationStartControl preparation)
      descriptor <- checked (connectionDescriptorWithTiming (startupTakeoverTarget state) (Preparation.preparationLocator preparation) (systemIdBytes (Genesis.checkedSystemId (startupGenesis state))) (heraldEpochBytes (Genesis.checkedLocalHeraldEpoch (startupGenesis state))) (processEpochIdBytes process))
      let child = RemoteChild process control descriptor
      if phase == Preparation.Attached
        then unchanged (RemotePreparationClaimed child)
        else do
          (observed, readiness) <- queryChildReadiness driver queries reference state
          pure (observed, case readiness of Left failure -> RemotePreparationFailed failure; Right pending -> RemotePreparationAvailable child pending)
  where
    unchanged status = pure (state, status)
