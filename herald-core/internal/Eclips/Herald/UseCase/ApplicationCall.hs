{-# LANGUAGE OverloadedStrings #-}

-- | Atomic coordination of the regular application calls executable by the
-- current Herald kernel.
module Eclips.Herald.UseCase.ApplicationCall
  ( ApplicationLabelWorkDisposition (..),
    applyApplicationRequest,
    rejectApplicationRequestDuringIsolation,
    advanceApplicationLabelWork,
  )
where

import Control.Monad (foldM)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Eclips.Application.Types.Identity
  ( PrivateNablaId,
    PrivateObjectId,
    PrivateProcessId,
    asPrivateObjectId,
    privateNablaUniqueId,
    privateObjectUniqueId,
    privateProcessUniqueId,
  )
import Eclips.Application.Types.Label
  ( ApplicationLabelTarget (..),
  )
import Eclips.Application.Types.NewId (NewIdTarget (..))
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Query (ApplicationQuery)
import Eclips.Application.Types.Rejection (ApplicationRejection (..))
import Eclips.Application.Types.Result
  ( RegularCallResult (..),
    WaitResult (WaitReady),
  )
import Eclips.Application.Types.Value (ApplicationLabel, ApplicationValue)
import Eclips.Application.Types.Value qualified as ApplicationValue
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (..),
    WriteResult (WriteAccepted),
  )
import Eclips.Domain.Identity
  ( GlobalObjectId,
    GlobalUniqueId,
    LabelDecisionId,
    ProcessEpochId,
    globalObjectIdFromDeltaId,
    globalObjectIdFromGlobalUniqueId,
    nablaIdFromGlobalObjectId,
    processEpochIdFromGlobalObjectId,
    publicationNabla,
  )
import Eclips.Domain.Label
  ( LabelTarget,
    LabelTargetView (..),
    ReleasedLabelStateView (..),
    homeLabelAcceptanceCut,
    labelTargetView,
    releasedLabelStateView,
    targetDelete,
    targetProcess,
    targetVoid,
  )
import Eclips.Domain.Membership (HeraldMembershipGeneration)
import Eclips.Domain.Publication
  ( checkedPublicationId,
    checkedPublicationSort,
  )
import Eclips.Domain.Query
  ( QueryAdmissionError (..),
    checkQueryPredicate,
  )
import Eclips.Domain.Sort.Canonical (canonicalCheckedDescriptor)
import Eclips.Domain.Sort.Descriptor
  ( SortKind (ControlledSort),
    StructuralCarrierRole (NablaCarrier),
    descriptorKind,
    descriptorSchema,
  )
import Eclips.Domain.Startup (PredefinedSortRole (SortDefinitionRole))
import Eclips.Domain.Value (Label)
import Eclips.Domain.Value qualified as DisappearanceValue
import Eclips.Herald.Application.Forward qualified as ApplicationForward
import Eclips.Herald.Application.PrivateIdentity qualified as PrivateIdentity
import Eclips.Herald.Application.Publication qualified as ApplicationPublication
import Eclips.Herald.Application.Query
  ( ApplicationQueryGlobalizationError (..),
    GlobalizedApplicationQuery,
    globalizeApplicationQuery,
    globalizedQueryApplicationProjection,
    globalizedQueryComparisons,
    globalizedQueryDeltas,
    globalizedQueryPredicate,
  )
import Eclips.Herald.Application.Request.Internal
  ( ApplicationRequestReply,
    RequestId,
    WaitId,
  )
import Eclips.Herald.Application.Session
  ( ApplicationSessionBinding,
    ApplicationSessionId,
  )
import Eclips.Herald.Application.SortDefinition
  ( ApplicationSortDefinitionRejection (..),
    presentApplicationSortDefinition,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Authority qualified as Authority
import Eclips.Herald.Controlled.Operate qualified as ControlledOperate
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Disappearance.Gate qualified as DisappearanceGate
import Eclips.Herald.Disappearance.State qualified as Disappearance
import Eclips.Herald.Discovery.State qualified as Discovery
import Eclips.Herald.EffectBatch
  ( ApplicationDispositionTarget (EstablishedApplicationDisposition),
    EffectBatch,
    HeraldEffect (..),
    orderedEffectBatch,
    singletonEffectBatch,
  )
import Eclips.Herald.EffectivePublication
  ( checkEffectivePublicationContext,
    interpretEffectivePublication,
    projectEffectivePublicationZombieOverlay,
  )
import Eclips.Herald.Genesis.Internal (checkedInitialProjectionDigest)
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Input
  ( ApplicationRequestIngress (..),
    PeerControl (PeerStreamFrontierAdvanced),
  )
import Eclips.Herald.Isolation.State qualified as Isolation
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.Groups qualified as PublicationGroups
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.Query
  ( ResolvedQuery,
    resolvedQuery,
    resolvedQueryBranch,
  )
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.Invariant
  ( HeraldInvariantFault (HeraldTransitionInvariant),
    HeraldTransitionInvariantViolation (HeraldApplicationTransitionContradiction),
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupApplicationState,
    replaceStartupControlledState,
    replaceStartupDisappearanceState,
    replaceStartupOracleClientState,
    replaceStartupPeerStreamState,
    replaceStartupPublicationState,
    replaceStartupSortRegistryState,
    replaceStartupStoreState,
    replaceStartupWaitState,
    startupApplicationState,
    startupControlledState,
    startupDisappearanceState,
    startupDiscoveryState,
    startupGenesis,
    startupGraphState,
    startupInitialBootstraps,
    startupIsolationState,
    startupLastObservedTime,
    startupOracleClientState,
    startupOracleProjectionState,
    startupPeerStreamState,
    startupPlacementState,
    startupPublicationState,
    startupSortRegistryState,
    startupStoreState,
    startupStructuralProgressState,
    startupWaitState,
  )
import Eclips.Herald.Store.Observation qualified as StoreObservation
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.UseCase.DisappearanceLive qualified as DisappearanceLive
import Eclips.Herald.UseCase.NewEnvironment qualified as NewEnvironment
import Eclips.Herald.UseCase.NewId
  ( NewIdFailure (..),
    operableNablaAuthority,
    operableNablaId,
    planNewId,
    resolveOperableNabla,
  )
import Eclips.Herald.UseCase.StructuralCoordinator qualified as StructuralCoordinator
import Eclips.Herald.Wait.State qualified as Wait

data CallFailure
  = CallRejected ApplicationRejection
  | CallRejectedAfterControlledObservation Controlled.State ApplicationRejection
  | CallContradiction

type CallPlan value = Either CallFailure value

-- | Exact evidence that one application-label fallout pass changed a Herald
-- owner.  The top-level transition uses this instead of comparing the whole
-- Herald state before deciding whether newly released application work needs
-- another alignment pass.
data ApplicationLabelWorkDisposition
  = ApplicationLabelWorkUnchanged
  | ApplicationLabelWorkChanged
  deriving stock (Eq, Ord, Show)

instance Semigroup ApplicationLabelWorkDisposition where
  ApplicationLabelWorkChanged <> _ = ApplicationLabelWorkChanged
  _ <> ApplicationLabelWorkChanged = ApplicationLabelWorkChanged
  ApplicationLabelWorkUnchanged <> ApplicationLabelWorkUnchanged =
    ApplicationLabelWorkUnchanged

instance Monoid ApplicationLabelWorkDisposition where
  mempty = ApplicationLabelWorkUnchanged

applyApplicationRequest ::
  ApplicationRequestIngress ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyApplicationRequest ingress predecessor = case ingress of
  CallApplicationRequest binding session request call ->
    case Application.classifyApplicationRequest
      session
      binding
      request
      call
      (startupApplicationState predecessor) of
      Left problem -> requestSessionFailure binding problem predecessor
      Right classification -> case classification of
        Application.FirstApplicationRequest candidate ->
          case Application.applicationGatePhase
            (startupApplicationState predecessor) of
            Application.ApplicationGateClosed _ ->
              case Application.prepareApplicationGatedIngress candidate of
                Left _ -> invariantFault
                Right prepared ->
                  checkedResult
                    ( replaceStartupApplicationState
                        (Application.commitApplicationDetachedRequest prepared)
                        predecessor
                    )
                    (orderedEffectBatch [])
            Application.ApplicationGateOpen _ ->
              finishPlan
                candidate
                predecessor
                (planApplicationIngress candidate call predecessor)
        Application.RetainedApplicationRequest reply ->
          checkedResult predecessor (replyEffect binding reply)
        Application.AttachedApplicationRequest _ ->
          checkedResult predecessor (orderedEffectBatch [])
        Application.ConflictingApplicationRequest reply ->
          checkedResult predecessor (replyEffect binding reply)
  GetApplicationRequestResult binding session request ->
    case Application.applicationRequestResultDisposition
      session
      binding
      request
      (startupApplicationState predecessor) of
      Left problem -> requestSessionFailure binding problem predecessor
      Right Nothing -> checkedResult predecessor (orderedEffectBatch [])
      Right (Just reply) -> checkedResult predecessor (replyEffect binding reply)
  CancelApplicationPendingWait binding session request wait ->
    applyCancellation binding session request wait predecessor

-- | During the losing branch's bounded drain, a new mutating call still owns
-- ordinary request identity and therefore receives one retained, cursor-ordered
-- rejection. It cannot enter any semantic planner or distributed work owner.
-- Local reads and retained-result lookup use 'applyApplicationRequest' instead.
rejectApplicationRequestDuringIsolation ::
  ApplicationRequestIngress ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
rejectApplicationRequestDuringIsolation ingress predecessor = case ingress of
  CallApplicationRequest binding session request call ->
    case Application.classifyApplicationRequest
      session
      binding
      request
      call
      (startupApplicationState predecessor) of
      Left problem -> requestSessionFailure binding problem predecessor
      Right classification -> case classification of
        Application.FirstApplicationRequest candidate ->
          finishPlan
            candidate
            predecessor
            (Left (CallRejected ApplicationOperateNotPermitted))
        Application.RetainedApplicationRequest reply ->
          checkedResult predecessor (replyEffect binding reply)
        Application.AttachedApplicationRequest _ ->
          checkedResult predecessor (orderedEffectBatch [])
        Application.ConflictingApplicationRequest reply ->
          checkedResult predecessor (replyEffect binding reply)
  GetApplicationRequestResult {} -> applyApplicationRequest ingress predecessor
  CancelApplicationPendingWait {} ->
    checkedResult predecessor (orderedEffectBatch [])

planCall ::
  Application.ApplicationRequestCandidate ->
  ApplicationOperation ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch)
planCall candidate call predecessor = case call of
  NewIdApplication target ->
    mapNewIdPlan (planNewId candidate target predecessor)
  WriteApplication privateNabla payload -> case payload of
    PublishValue value ->
      planPublicationWrite candidate privateNabla value predecessor
    DeleteReserved privateObject ->
      planDeleteReserved candidate privateNabla privateObject predecessor
  ForwardApplication privateNabla privateObject ->
    planForward candidate privateNabla privateObject predecessor
  ReadApplication query -> planQuery False candidate query predecessor
  LocalTakeApplication query -> planQuery True candidate query predecessor
  WaitApplication queries -> planWait candidate queries predecessor
  LabelApplication object expected target ->
    planLabelApplication candidate object expected target predecessor
  NewEnvironmentApplication ->
    planNewEnvironmentApplication candidate predecessor

planNewEnvironmentApplication ::
  Application.ApplicationRequestCandidate ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch)
planNewEnvironmentApplication candidate predecessor =
  case NewEnvironment.planApplicationNewEnvironment candidate predecessor of
    Left (NewEnvironment.NewEnvironmentRejected rejection) ->
      Left (CallRejected rejection)
    Left _ -> Left CallContradiction
    Right (successor, NewEnvironment.ApplicationNewEnvironmentDeferred) ->
      Right (successor, orderedEffectBatch [])
    Right
      ( successor,
        NewEnvironment.ApplicationNewEnvironmentAccepted _ reply
        ) -> do
        (structuralSuccessor, structuralEffects) <-
          mapContradiction
            (StructuralCoordinator.advanceLocalStructuralWork successor)
        Right
          ( structuralSuccessor,
            candidateReplyEffect candidate reply <> structuralEffects
          )

-- | Publications in the active label's exact sequencing scope are held
-- through its result.  They are fully validated
-- and globalized first, but none of the planned Application, Controlled,
-- Publication, Store, structural, or peer successors becomes visible.  The
-- Application owner retains only the self-contained semantic work and its
-- already allocated process position.
planApplicationIngress ::
  Application.ApplicationRequestCandidate ->
  ApplicationOperation ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch)
planApplicationIngress candidate call predecessor =
  case heldEndpointDecision (Application.applicationRequestCandidateProcess candidate) call predecessor of
    Just decision -> do
      prepared <- mapContradiction (Application.prepareApplicationEndpointHeldIngress decision candidate)
      Right
        ( replaceStartupApplicationState (Application.commitApplicationDetachedRequest prepared) predecessor,
          orderedEffectBatch []
        )
    Nothing -> gateApplicationPlan candidate call predecessor (planApplicationLabelIngress candidate call predecessor)

heldEndpointDecision :: ProcessEpochId -> ApplicationOperation -> HeraldState -> Maybe LabelDecisionId
heldEndpointDecision process call predecessor = do
  privateNabla <- case call of
    NewIdApplication (ControlledNewId nabla) -> Just nabla
    WriteApplication nabla _ -> Just nabla
    ForwardApplication nabla _ -> Just nabla
    _ -> Nothing
  global <- either (const Nothing) Just (Application.resolveApplicationPrivateUniqueId process (privateNablaUniqueId privateNabla) application)
  Application.applicationEndpointAdmissionHoldFor (nablaIdFromGlobalObjectId (globalObjectIdFromGlobalUniqueId global)) application
  where
    application = startupApplicationState predecessor

-- Every path which can release positioned application work shares the same
-- post-cut gate, including work originally held by an unrelated label.
gateApplicationPlan ::
  Application.ApplicationRequestCandidate ->
  ApplicationOperation ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch) ->
  CallPlan (HeraldState, EffectBatch)
gateApplicationPlan candidate call predecessor plannedCall = do
  planned@(plannedState, _) <- plannedCall
  case call of
    WriteApplication _ (PublishValue _) -> gatePlannedPublication planned plannedState
    ForwardApplication {} -> gatePlannedPublication planned plannedState
    _ -> Right planned
  where
    gatePlannedPublication planned plannedState =
      let position = Application.applicationRequestCandidatePosition candidate
          plannedWork = case Publication.lookupApplicationPublication position (startupPublicationState plannedState) of
            Just record -> Just (heldWorkForRecord record)
            Nothing -> case [ Application.applicationFenceHeldSemanticWork held
                            | held <- Application.applicationFenceHeldEntries (startupApplicationState plannedState),
                              Application.applicationFenceHeldPosition held == position
                            ] of
              work : _ -> Just work
              [] -> Nothing
       in case plannedWork of
            Nothing -> Right planned
            Just work ->
              let canonical = DisappearanceValue.canonicalValueBytes (Publication.admittedApplicationPublicationValue (Application.applicationFenceHeldWorkAdmittedValue work))
                  gates =
                    DisappearanceGate.canonicalPublicationGates
                      (Application.applicationFenceHeldWorkSortId work)
                      (Application.applicationFenceHeldWorkSortOccurrenceId work)
                      canonical
                      (startupDisappearanceState predecessor)
               in if null gates
                    then Right planned
                    else do
                      held <-
                        mapContradiction
                          ( Application.prepareApplicationDisappearanceHeld
                              (Set.fromList (fmap Disappearance.matchingWriteGateProbe gates))
                              candidate
                              work
                          )
                      leaf <-
                        mapContradiction
                          ( DisappearanceGate.retainGatedPublicationInvalidations
                              gates
                              (DisappearanceValue.canonicalValueByteString canonical)
                              (startupDisappearanceState predecessor)
                          )
                      Right
                        ( replaceStartupDisappearanceState leaf
                            . replaceStartupApplicationState (Application.commitApplicationDetachedRequest held)
                            $ predecessor,
                          orderedEffectBatch []
                        )

planApplicationLabelIngress ::
  Application.ApplicationRequestCandidate ->
  ApplicationOperation ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch)
planApplicationLabelIngress candidate call predecessor =
  case call of
    WriteApplication _ (PublishValue _) -> gatePlannedLabelPublication candidate predecessor (planCall candidate call predecessor)
    ForwardApplication {} -> gatePlannedLabelPublication candidate predecessor (planCall candidate call predecessor)
    _ -> planCall candidate call predecessor

gatePlannedLabelPublication ::
  Application.ApplicationRequestCandidate ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch) ->
  CallPlan (HeraldState, EffectBatch)
gatePlannedLabelPublication candidate predecessor plannedCall
  | not (Application.applicationHasActiveLabels (startupApplicationState predecessor)) = plannedCall
  | otherwise = do
      planned@(plannedState, _) <- plannedCall
      record <-
        maybe
          (Left CallContradiction)
          Right
          ( Publication.lookupApplicationPublication
              (Application.applicationRequestCandidatePosition candidate)
              (startupPublicationState plannedState)
          )
      if publicationMembershipHeld (Publication.applicationPublicationGroupMembership record) (startupApplicationState predecessor)
        then do
          prepared <-
            mapContradiction
              ( Application.prepareApplicationFenceHeld
                  candidate
                  (heldWorkForRecord record)
              )
          Right
            ( replaceStartupApplicationState
                (Application.commitApplicationDetachedRequest prepared)
                predecessor,
              orderedEffectBatch []
            )
        else Right planned

heldWorkForRecord :: Publication.ApplicationPublicationRecord -> Application.ApplicationFenceHeldWork
heldWorkForRecord record =
  Application.applicationFenceHeldWork
    (publicationNabla (Publication.applicationPublicationId record))
    (checkedPublicationSort (Publication.applicationPublicationChecked record))
    (Publication.applicationPublicationSortOccurrenceId record)
    (Publication.applicationPublicationAdmittedValue record)
    (Publication.applicationPublicationOrigin record)
    (Publication.applicationPublicationSourceStrength record)
    (Publication.applicationPublicationControlPrerequisite record)
    (Publication.applicationPublicationGroupMembership record)

-- A publication belongs to at most two groups. Query their object indexes
-- instead of scanning unrelated active labels for every admitted publication.
publicationMembershipHeld :: PublicationGroups.Membership -> Application.State -> Bool
publicationMembershipHeld members application =
  any held (PublicationGroups.membershipKeys members)
  where
    held group = case Application.applicationActiveLabelForGroup group application of
      Just active -> Application.activeLabelApplicationTerminalResult active == Nothing
      Nothing -> False

-- | Drive all application-owned label fallout after any Serving transition.
-- Once the global gate is open, reconsider raw ingress even while a local
-- invocation drains: unrelated providers can proceed, while selected later
-- publications remain held. Positioned selected work is released only once
-- its own invocation is terminal. Finally reconsider provider-blocked label
-- candidates, admitting independent objects in the same owner pass.
advanceApplicationLabelWork ::
  HeraldState ->
  Either
    HeraldInvariantFault
    (HeraldState, EffectBatch, ApplicationLabelWorkDisposition)
advanceApplicationLabelWork predecessor =
  case Application.applicationGatePhase application of
    Application.ApplicationGateOpen _ -> do
      (afterHeld, heldEffects) <-
        foldM redriveHeld (predecessor, orderedEffectBatch []) heldSnapshot
      let gatedWork = gatedSnapshot afterHeld
      (afterGated, gatedEffects) <-
        foldM redriveGated (afterHeld, orderedEffectBatch []) gatedWork
      (afterCandidates, candidateEffects, candidateDisposition) <- reconsiderCandidates afterGated
      (successor, environmentEffects, environmentDisposition) <-
        reconsiderEnvironmentCandidates afterCandidates
      Right
        ( successor,
          heldEffects <> gatedEffects <> candidateEffects <> environmentEffects,
          workPresenceDisposition heldSnapshot
            <> workPresenceDisposition gatedWork
            <> candidateDisposition
            <> environmentDisposition
        )
    _ -> Right (predecessor, orderedEffectBatch [], ApplicationLabelWorkUnchanged)
  where
    application = startupApplicationState predecessor
    heldSnapshot =
      sortOn
        Application.applicationFenceHeldPosition
        [ held
        | held <- Application.applicationFenceHeldEntries application,
          not (selectedHeldWork held),
          not
            ( any
                ( \probe ->
                    DisappearanceGate.disappearanceProbeIsCollecting
                      probe
                      (startupDisappearanceState predecessor)
                )
                (Set.toList (Application.applicationFenceHeldDisappearanceProbes held))
            )
        ]
    selectedHeldWork held =
      publicationMembershipHeld
        (Application.applicationFenceHeldWorkGroupMembership (Application.applicationFenceHeldSemanticWork held))
        application

    redriveHeld (current, effects) view = do
      (candidate, work) <-
        case Application.applicationFenceHeldForRedrive
          (Application.applicationFenceHeldPosition view)
          (startupApplicationState current) of
          Left _ -> invariantFault
          Right prepared -> Right prepared
      let replayPredecessor =
            replaceStartupApplicationState
              (Application.applicationRequestCandidateState candidate)
              current
      (successor, replayEffects) <-
        finishPlan
          candidate
          replayPredecessor
          ( gateApplicationPlan
              candidate
              (Application.applicationRequestCandidateCall candidate)
              replayPredecessor
              (gatePlannedLabelPublication candidate replayPredecessor (planFenceHeldWork candidate work replayPredecessor))
          )
      Right (successor, effects <> replayEffects)

    gatedSnapshot state =
      sortOn
        ( \entry ->
            ( Application.applicationGatedIngressProcess entry,
              Application.applicationGatedIngressKey entry
            )
        )
        [ entry
        | entry <- Application.applicationGatedIngressEntries (startupApplicationState state),
          Application.applicationGatedIngressReady entry (startupApplicationState state),
          heldEndpointDecision (Application.applicationGatedIngressProcess entry) (Application.applicationGatedIngressOperation entry) state == Nothing
        ]

    redriveGated (current, effects) entry =
      case Application.applicationGatedIngressOperation entry of
        LabelApplication {} -> do
          applicationSuccessor <-
            case Application.moveGatedLabelToCandidate
              (Application.applicationGatedIngressKey entry)
              (startupApplicationState current) of
              Left _ -> invariantFault
              Right moved -> Right moved
          Right
            ( replaceStartupApplicationState applicationSuccessor current,
              effects
            )
        operation -> do
          candidate <-
            case Application.applicationGatedIngressForRedrive
              (Application.applicationGatedIngressKey entry)
              (startupApplicationState current) of
              Left _ -> invariantFault
              Right prepared -> Right prepared
          let replayPredecessor =
                replaceStartupApplicationState
                  (Application.applicationRequestCandidateState candidate)
                  current
          (successor, replayEffects) <-
            finishPlan
              candidate
              replayPredecessor
              (planApplicationIngress candidate operation replayPredecessor)
          Right (successor, effects <> replayEffects)

reconsiderCandidates ::
  HeraldState ->
  Either
    HeraldInvariantFault
    (HeraldState, EffectBatch, ApplicationLabelWorkDisposition)
reconsiderCandidates predecessor =
  visit
    predecessor
    (orderedEffectBatch [])
    ApplicationLabelWorkUnchanged
    snapshot
  where
    snapshot =
      sortOn
        ( \candidate ->
            ( Application.applicationLabelCandidateProcess candidate,
              Application.applicationLabelCandidateKey candidate
            )
        )
        ( Application.applicationLabelCandidateEntries
            (startupApplicationState predecessor)
        )

    visit current effects disposition [] = Right (current, effects, disposition)
    visit current effects disposition (view : remaining) = do
      candidate <-
        case Application.applicationLabelCandidateForReconsideration
          (Application.applicationLabelCandidateKey view)
          (startupApplicationState current) of
          Left _ -> invariantFault
          Right prepared -> Right prepared
      (object, expected, target) <- case Application.applicationRequestCandidateCall candidate of
        LabelApplication suppliedObject suppliedExpected suppliedTarget ->
          Right (suppliedObject, suppliedExpected, suppliedTarget)
        _ -> invariantFault
      case checkLabelAdmission candidate object expected target current of
        Left CallContradiction -> invariantFault
        Left (CallRejected rejection) -> do
          (successor, rejectionEffects) <-
            rejectRetainedLabelCandidate
              candidate
              rejection
              (startupControlledState current)
              current
          visit
            successor
            (effects <> rejectionEffects)
            ApplicationLabelWorkChanged
            remaining
        Left (CallRejectedAfterControlledObservation controlled rejection) -> do
          (successor, rejectionEffects) <-
            rejectRetainedLabelCandidate candidate rejection controlled current
          visit
            successor
            (effects <> rejectionEffects)
            ApplicationLabelWorkChanged
            remaining
        Right CheckedLabelWaitingForProvider -> visit current effects disposition remaining
        Right admission@CheckedLabelAdmission {} -> do
          (successor, acceptanceEffects) <-
            finishPlan
              candidate
              current
              ( acceptRetainedLabelApplication
                  (Application.applicationLabelCandidateKey view)
                  candidate
                  admission
                  current
              )
          visit successor (effects <> acceptanceEffects) ApplicationLabelWorkChanged remaining

-- A successful pass consumes every member of both frozen owner snapshots.
-- Held entries are first removed by 'applicationFenceHeldForRedrive'; gated
-- entries are either moved to the candidate owner or removed by
-- 'applicationGatedIngressForRedrive'.  Thus snapshot presence is exact change
-- evidence: a failed preparation returns an invariant fault and no disposition,
-- while every successfully visited entry changes the Application owner even if
-- its semantic replay emits no effect.
workPresenceDisposition :: [value] -> ApplicationLabelWorkDisposition
workPresenceDisposition [] = ApplicationLabelWorkUnchanged
workPresenceDisposition (_ : _) = ApplicationLabelWorkChanged

-- | Re-drive every cursorless @newenv@ candidate after a serving transition.
-- A still-blocked candidate is removed and reinserted state-identically and is
-- therefore not reported as progress to the outer fixed point.
reconsiderEnvironmentCandidates ::
  HeraldState ->
  Either
    HeraldInvariantFault
    (HeraldState, EffectBatch, ApplicationLabelWorkDisposition)
reconsiderEnvironmentCandidates predecessor =
  foldM visit initial (Application.applicationEnvironmentCandidateKeys application)
  where
    application = startupApplicationState predecessor
    initial =
      ( predecessor,
        orderedEffectBatch [],
        ApplicationLabelWorkUnchanged
      )

    visit (current, effects, disposition) key = do
      candidate <-
        case Application.applicationEnvironmentCandidateForRedrive
          key
          (startupApplicationState current) of
          Left _ -> invariantFault
          Right value -> Right value
      let replayPredecessor =
            replaceStartupApplicationState
              (Application.applicationRequestCandidateState candidate)
              current
      (successor, replayEffects) <-
        finishPlan
          candidate
          replayPredecessor
          (planNewEnvironmentApplication candidate replayPredecessor)
      let changed =
            startupApplicationState successor
              /= startupApplicationState current
          nextDisposition
            | changed = ApplicationLabelWorkChanged
            | otherwise = disposition
      Right
        ( successor,
          effects <> replayEffects,
          nextDisposition
        )

rejectRetainedLabelCandidate ::
  Application.ApplicationRequestCandidate ->
  ApplicationRejection ->
  Controlled.State ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
rejectRetainedLabelCandidate candidate rejection controlled predecessor = do
  prepared <-
    case Application.prepareApplicationLabelCandidateRejection
      key
      rejection
      (startupApplicationState predecessor) of
      Left _ -> invariantFault
      Right value -> Right value
  let (applicationSuccessor, reply) =
        Application.commitApplicationLabelCandidateRejection prepared
      successor =
        replaceStartupControlledState controlled
          . replaceStartupApplicationState applicationSuccessor
          $ predecessor
  checkedResult successor (candidateReplyEffect candidate reply)
  where
    key =
      Application.applicationRequestKey
        (Application.applicationRequestCandidateSession candidate)
        (Application.applicationRequestCandidateId candidate)

planFenceHeldWork ::
  Application.ApplicationRequestCandidate ->
  Application.ApplicationFenceHeldWork ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch)
planFenceHeldWork candidate work predecessor =
  case Application.applicationRequestCandidateCall candidate of
    WriteApplication privateNabla (PublishValue _) -> do
      prepared <-
        case ApplicationPublication.prepareFenceHeldApplicationPublication
          (startupGenesis predecessor)
          (currentHeraldMembership predecessor)
          candidate
          work
          (startupControlledState predecessor)
          (startupSortRegistryState predecessor)
          (startupGraphState predecessor)
          (startupStructuralProgressState predecessor)
          (startupPlacementState predecessor)
          (startupPublicationState predecessor)
          (startupStoreState predecessor)
          (startupPeerStreamState predecessor) of
          Left problem ->
            Left
              ( heldApplicationPublicationFailure
                  candidate
                  privateNabla
                  problem
              )
          Right accepted -> Right accepted
      finishPreparedPublicationWrite candidate predecessor prepared
    ForwardApplication privateNabla _ -> do
      prepared <-
        case ApplicationForward.prepareFenceHeldApplicationForward
          (startupGenesis predecessor)
          (currentHeraldMembership predecessor)
          (startupLastObservedTime predecessor)
          candidate
          work
          (startupControlledState predecessor)
          (startupSortRegistryState predecessor)
          (startupGraphState predecessor)
          (startupStructuralProgressState predecessor)
          (startupPlacementState predecessor)
          (startupPublicationState predecessor)
          (startupStoreState predecessor)
          (startupPeerStreamState predecessor) of
          Left problem ->
            Left
              ( heldApplicationForwardFailure
                  candidate
                  privateNabla
                  problem
              )
          Right accepted -> Right accepted
      finishPreparedForward candidate predecessor prepared
    _ -> Left CallContradiction

data CheckedLabelAdmission
  = CheckedLabelAdmission
      GlobalObjectId
      Label
      LabelTarget
      Application.SequencingSelection
      Controlled.ControlledLabelPrior
  | CheckedLabelWaitingForProvider

-- | Live label admission. Provider-blocked work remains an unpositioned
-- candidate; an eligible call atomically claims its process position, pending
-- EAPP reply, selected publication fence, and nondispatchable Oracle intention.
-- The source driver submits the decision only after that selected group drains.
planLabelApplication ::
  Application.ApplicationRequestCandidate ->
  PrivateObjectId ->
  ApplicationLabel ->
  ApplicationLabelTarget ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch)
planLabelApplication candidate object expected target predecessor = do
  admission <- checkLabelAdmission candidate object expected target predecessor
  case admission of
    CheckedLabelWaitingForProvider -> do
      prepared <-
        mapContradiction
          (Application.prepareApplicationLabelCandidate candidate)
      let successor =
            replaceStartupApplicationState
              (Application.commitApplicationDetachedRequest prepared)
              predecessor
      Right (successor, orderedEffectBatch [])
    CheckedLabelAdmission {} ->
      acceptFreshLabelApplication candidate admission predecessor

acceptFreshLabelApplication ::
  Application.ApplicationRequestCandidate ->
  CheckedLabelAdmission ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch)
acceptFreshLabelApplication
  candidate
  (CheckedLabelAdmission object expected target selection priorFacts)
  predecessor = do
    let process = Application.applicationRequestCandidateProcess candidate
        key =
          Application.applicationRequestKey
            (Application.applicationRequestCandidateSession candidate)
            (Application.applicationRequestCandidateId candidate)
        position = Application.applicationRequestCandidatePosition candidate
        acceptanceCut =
          homeLabelAcceptanceCut
            position
            ( Publication.publicationCurrentHeraldPrefix
                (startupPublicationState predecessor)
            )
    preparedOracle <-
      mapContradiction
        ( OracleClient.prepareDrainingLabelDecisionRequest
            (Application.applicationRequestKeyCanonicalBytes key)
            process
            object
            expected
            (Controlled.controlledLabelPriorInitialEvidence priorFacts)
            (Controlled.controlledLabelPriorAuthority priorFacts)
            (Controlled.controlledLabelPriorAuthorityJustification priorFacts)
            target
            acceptanceCut
            (startupOracleClientState predecessor)
        )
    if OracleClient.preparedLabelDecisionRequestClassification preparedOracle
      == OracleClient.OracleRequestFirstPrepared
      then Right ()
      else Left CallContradiction
    let decision = OracleClient.preparedLabelDecisionId preparedOracle
    preparedApplication <-
      mapContradiction
        ( Application.prepareApplicationLabelAcceptance
            decision
            selection
            candidate
        )
    withEndpointHold <- retainLabelEndpointHold candidate object target priorFacts predecessor preparedApplication
    let (application, reply) =
          Application.commitApplicationLabelAcceptance withEndpointHold
        (oracleClient, _, retainedDecision, _requestRef) =
          OracleClient.commitLabelDecisionRequest preparedOracle
    if retainedDecision == decision
      then Right ()
      else Left CallContradiction
    let successor =
          replaceStartupOracleClientState oracleClient
            . replaceStartupApplicationState application
            $ predecessor
        effects =
          orderedEffectBatch
            ( SendApplicationReply
                (Application.applicationRequestCandidateBinding candidate)
                reply
                : fmap
                  RunOracleClientAction
                  (OracleClient.oracleClientRequestActions oracleClient)
            )
    Right (successor, effects)
acceptFreshLabelApplication _ _ _ = Left CallContradiction

-- | Promote the exact retained pre-acceptance candidate.  Its request key is
-- stable, but its process position, acceptance cut, prior label evidence, and
-- Oracle decision are all derived in this one current-state transaction.
acceptRetainedLabelApplication ::
  Application.ApplicationRequestKey ->
  Application.ApplicationRequestCandidate ->
  CheckedLabelAdmission ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch)
acceptRetainedLabelApplication
  key
  candidate
  (CheckedLabelAdmission object expected target selection priorFacts)
  predecessor = do
    let process = Application.applicationRequestCandidateProcess candidate
        position = Application.applicationRequestCandidatePosition candidate
        acceptanceCut =
          homeLabelAcceptanceCut
            position
            ( Publication.publicationCurrentHeraldPrefix
                (startupPublicationState predecessor)
            )
    preparedOracle <-
      mapContradiction
        ( OracleClient.prepareDrainingLabelDecisionRequest
            (Application.applicationRequestKeyCanonicalBytes key)
            process
            object
            expected
            (Controlled.controlledLabelPriorInitialEvidence priorFacts)
            (Controlled.controlledLabelPriorAuthority priorFacts)
            (Controlled.controlledLabelPriorAuthorityJustification priorFacts)
            target
            acceptanceCut
            (startupOracleClientState predecessor)
        )
    if OracleClient.preparedLabelDecisionRequestClassification preparedOracle
      == OracleClient.OracleRequestFirstPrepared
      then Right ()
      else Left CallContradiction
    let decision = OracleClient.preparedLabelDecisionId preparedOracle
    preparedApplication <-
      mapContradiction
        ( Application.prepareRetainedApplicationLabelAcceptance
            decision
            selection
            key
            (startupApplicationState predecessor)
        )
    withEndpointHold <- retainLabelEndpointHold candidate object target priorFacts predecessor preparedApplication
    let (application, reply) =
          Application.commitApplicationLabelAcceptance withEndpointHold
        (oracleClient, _, retainedDecision, _requestRef) =
          OracleClient.commitLabelDecisionRequest preparedOracle
    if retainedDecision == decision
      then Right ()
      else Left CallContradiction
    let successor =
          replaceStartupOracleClientState oracleClient
            . replaceStartupApplicationState application
            $ predecessor
        effects =
          orderedEffectBatch
            ( candidateReplyEffects candidate reply
                <> fmap
                  RunOracleClientAction
                  (OracleClient.oracleClientRequestActions oracleClient)
            )
    Right (successor, effects)
acceptRetainedLabelApplication _ _ _ _ = Left CallContradiction

retainLabelEndpointHold ::
  Application.ApplicationRequestCandidate ->
  GlobalObjectId ->
  LabelTarget ->
  Controlled.ControlledLabelPrior ->
  HeraldState ->
  Application.PreparedApplicationLabelAcceptance ->
  CallPlan Application.PreparedApplicationLabelAcceptance
retainLabelEndpointHold candidate object target prior predecessor prepared =
  case (Controlled.controlledCurrentStructuralRole object (startupControlledState predecessor), releasedLabelStateView (Controlled.controlledLabelPriorEffectiveState prior)) of
    (Just NablaCarrier, ReleasedLabelView (DisappearanceValue.ProcessLabel owner, _))
      | owner == Application.applicationRequestCandidateProcess candidate,
        labelTargetView target /= TargetProcessView owner ->
          mapContradiction (Application.retainApplicationLabelEndpointHold (nablaIdFromGlobalObjectId object) prepared)
    _ -> Right prepared

checkLabelAdmission ::
  Application.ApplicationRequestCandidate ->
  PrivateObjectId ->
  ApplicationLabel ->
  ApplicationLabelTarget ->
  HeraldState ->
  CallPlan CheckedLabelAdmission
checkLabelAdmission candidate privateObject privateExpected privateTarget predecessor = do
  object <- resolveLabelObject
  if Controlled.controlledObjectLabelable object controlled
    then do
      if Controlled.controlledHasNormalPossession process object controlled
        then Right ()
        else Left transitionNotPermitted
      target <- resolveLabelTarget privateTarget
      expected <-
        case Application.globalizeApplicationLabel processRoles process privateExpected application of
          Left problem -> Left (applicationValueGlobalizationFailure problem)
          Right value -> Right value
      let selection = Application.sequencingSelection process object
          blocked =
            Application.applicationActiveLabelForObject object application /= Nothing
              || labelSelectionHasPendingPublication selection predecessor
              || labelMembershipTransitionPending predecessor
              || labelInitialNablaProviderPending object predecessor
      if blocked
        then Right CheckedLabelWaitingForProvider
        else do
          firstAuthority <-
            case (Controlled.controlledReleasedLabelRecord object controlled, Controlled.controlledCurrentStructuralRole object controlled) of
              (Nothing, Just NablaCarrier) ->
                Just
                  <$> mapContradiction
                    ( Authority.resolveInitialNablaAuthority
                        (nablaIdFromGlobalObjectId object)
                        (startupStructuralProgressState predecessor)
                        controlled
                    )
              _ -> Right Nothing
          priorFacts <-
            case Controlled.checkControlledLabelPriorWithAuthority
              (checkedInitialProjectionDigest (startupInitialBootstraps predecessor))
              firstAuthority
              object
              controlled of
              Left (Controlled.ControlledLabelPriorUnbackedLabelAuthority _ _) ->
                Left CallContradiction
              Left _ -> Left objectNotLabelable
              Right checked -> Right checked
          Right
            ( CheckedLabelAdmission
                object
                expected
                target
                selection
                priorFacts
            )
    else Left objectNotLabelable
  where
    process = Application.applicationRequestCandidateProcess candidate
    application = startupApplicationState predecessor
    controlled = startupControlledState predecessor
    roleEpochs =
      OracleProjection.projectedProcessEpochs
        (startupOracleProjectionState predecessor)
    processRoles = Application.processRoleView roleEpochs
    objectNotLabelable =
      CallRejected (ApplicationObjectNotLabelable privateObject)
    transitionNotPermitted =
      CallRejected ApplicationLabelTransitionNotPermitted

    resolveLabelObject =
      case Application.resolveApplicationPrivateUniqueId
        process
        (privateObjectUniqueId privateObject)
        application of
        Right global -> Right (globalObjectIdFromGlobalUniqueId global)
        Left _ -> Left objectNotLabelable

    resolveLabelTarget supplied = case supplied of
      LabelToVoid -> Right targetVoid
      LabelToDelete -> Right targetDelete
      LabelToProcess privateProcess -> do
        targetProcessEpoch <- resolveTargetProcess privateProcess
        if targetProcessEpoch `elem` roleEpochs
          then Right (targetProcess targetProcessEpoch)
          else Left (targetNotNameable privateProcess)

    resolveTargetProcess privateProcess =
      case Application.resolveApplicationPrivateUniqueId
        process
        (privateProcessUniqueId privateProcess)
        application of
        Right global ->
          Right
            ( processEpochIdFromGlobalObjectId
                (globalObjectIdFromGlobalUniqueId global)
            )
        Left _ -> Left (targetNotNameable privateProcess)

    targetNotNameable :: PrivateProcessId -> CallFailure
    targetNotNameable privateProcess =
      CallRejected (ApplicationLabelTargetNotNameable privateProcess)

-- A locally accepted Nabla publication is visible to Controlled before its
-- structural occurrence has an installed cut. Only that exact first-publication
-- provider can defer initial authority evidence; an absent unexplained tenure
-- remains an invariant fault. This does not add another publication drain.
labelInitialNablaProviderPending :: GlobalObjectId -> HeraldState -> Bool
labelInitialNablaProviderPending object state =
  case (Controlled.controlledReleasedLabelRecord object controlled, Controlled.controlledCurrentStructuralRole object controlled, Controlled.controlledLocalRecord object controlled) of
    (Nothing, Just NablaCarrier, Just record) ->
      let firstPublication = checkedPublicationId (Controlled.controlledRecordFirstPublication record)
       in any
            ((== firstPublication) . checkedPublicationId . Publication.unsequencedStructuralChecked . snd)
            (Publication.unstampedApplicationStructuralStageEntries publication)
            || any
              ( \(occurrence, stage) ->
                  checkedPublicationId (Publication.stampedStructuralChecked stage) == firstPublication
                    && GraphProgress.installedCutCoveringOccurrence occurrence progress == Nothing
              )
              (Publication.stampedStructuralStageEntries publication)
    _ -> False
  where
    controlled = startupControlledState state
    publication = startupPublicationState state
    progress = startupStructuralProgressState state

-- | Oracle projects retirement before the Graph owner can install the exact
-- membership-successor cut.  A label request must not capture coordinates from
-- opposite sides of that interval.  Retaining the checked application
-- candidate uses the ordinary pre-acceptance owner: it allocates no process
-- position, opens no fence, and creates no Oracle intention.  The Graph
-- membership changes only as part of successor-base installation, so equality
-- is the exact release condition rather than a separately mutable runtime flag.
labelMembershipTransitionPending :: HeraldState -> Bool
labelMembershipTransitionPending state =
  OracleProjection.oracleViewCurrentHeraldMembershipId
    (OracleProjection.oracleView (startupOracleProjectionState state))
    /= GraphProgress.structuralProgressMembershipGenerationId
      (startupStructuralProgressState state)

labelSelectionHasPendingPublication ::
  Application.SequencingSelection ->
  HeraldState ->
  Bool
labelSelectionHasPendingPublication selection state =
  PublicationGroups.groupUnstampedCount
    group
    (Publication.publicationGroups (startupPublicationState state))
    > 0
    -- Positioned work held by an older label or disappearance probe has not
    -- entered publication-group accounting yet. Let it resume before accepting
    -- another matching boundary; unrelated held work must not block this call.
    || any
      ( Set.member group
          . PublicationGroups.membershipKeys
          . Application.applicationFenceHeldWorkGroupMembership
          . Application.applicationFenceHeldSemanticWork
      )
      (Application.applicationFenceHeldEntries (startupApplicationState state))
  where
    group = Application.sequencingSelectionGroup selection

planDeleteReserved ::
  Application.ApplicationRequestCandidate ->
  PrivateNablaId ->
  PrivateObjectId ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch)
planDeleteReserved candidate privateNabla privateObject predecessor = do
  generated <-
    case Application.resolveApplicationPrivateUniqueId
      process
      (privateObjectUniqueId privateObject)
      (startupApplicationState predecessor) of
      Left (Application.ApplicationValueRejected _) ->
        Left
          ( CallRejected
              (ApplicationUnknownPrivateIdentity (privateObjectUniqueId privateObject))
          )
      Left (Application.ApplicationValueGlobalizationInvariant _) ->
        Left CallContradiction
      Right identity -> Right identity
  operable <-
    mapNewIdPlan
      (resolveOperableNabla candidate privateNabla predecessor)
  preparedDelete <-
    case Controlled.prepareControlledReservationDelete
      process
      (operableNablaId operable)
      (operableNablaAuthority operable)
      generated
      (startupControlledState predecessor) of
      Left Controlled.ControlledReservationUnavailable ->
        Left
          ( CallRejected
              (ApplicationReservationUnavailable privateNabla privateObject)
          )
      Left Controlled.ControlledReservationAuthorityContradiction ->
        Left CallContradiction
      Right prepared -> Right prepared
  let preparedRequest =
        Application.prepareApplicationRequestCompletion
          candidate
          (WriteCompleted WriteAccepted)
      (applicationSuccessor, reply) =
        Application.commitApplicationRequest preparedRequest
      successor =
        replaceStartupControlledState
          (Controlled.commitControlledReservationDelete preparedDelete)
          . replaceStartupApplicationState applicationSuccessor
          $ predecessor
  Right
    ( successor,
      candidateReplyEffect candidate reply
    )
  where
    process = Application.applicationRequestCandidateProcess candidate

planPublicationWrite ::
  Application.ApplicationRequestCandidate ->
  PrivateNablaId ->
  ApplicationValue ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch)
planPublicationWrite candidate privateNabla value predecessor = do
  prepared <-
    case ApplicationPublication.prepareLocalApplicationPublication
      (startupGenesis predecessor)
      (currentHeraldMembership predecessor)
      roles
      candidate
      privateNabla
      value
      (startupControlledState predecessor)
      (startupSortRegistryState predecessor)
      (startupGraphState predecessor)
      (startupStructuralProgressState predecessor)
      (startupPlacementState predecessor)
      (startupPublicationState predecessor)
      (startupStoreState predecessor)
      (startupPeerStreamState predecessor) of
      Left problem ->
        Left
          ( applicationPublicationFailure
              candidate
              privateNabla
              problem
          )
      Right accepted -> Right accepted
  finishPreparedPublicationWrite candidate predecessor prepared
  where
    roles =
      Application.processRoleView
        ( OracleProjection.projectedProcessEpochs
            (startupOracleProjectionState predecessor)
        )

finishPreparedPublicationWrite ::
  Application.ApplicationRequestCandidate ->
  HeraldState ->
  ApplicationPublication.PreparedLocalApplicationPublication ->
  CallPlan (HeraldState, EffectBatch)
finishPreparedPublicationWrite candidate predecessor prepared = do
  let ( applicationAfterPublication,
        controlledSuccessor,
        registrySuccessor,
        publicationSuccessor,
        storeSuccessor,
        peerStreamSuccessor,
        outcome
        ) = ApplicationPublication.commitLocalApplicationPublication prepared
      publicationEffects =
        ApplicationPublication.localApplicationPublicationEffects outcome
      callerReply =
        ApplicationPublication.localApplicationPublicationReply outcome
  ready <- case publicationEffects of
    ApplicationPublication.OrdinaryApplicationPublicationEffects {} ->
      readyWaitRegistrations
        applicationAfterPublication
        controlledSuccessor
        registrySuccessor
        storeSuccessor
        (startupWaitState predecessor)
    ApplicationPublication.UnsequencedStructuralApplicationPublicationEffects ->
      Right []
  preparedWaitRemoval <-
    mapContradiction
      ( Wait.prepareWaitRemoval
          (fmap Wait.waitRegistrationId ready)
          (startupWaitState predecessor)
      )
  preparedWaitCompletions <-
    mapContradiction
      ( Application.prepareApplicationWaitCompletions
          (fmap Wait.waitRegistrationId ready)
          applicationAfterPublication
      )
  let (applicationSuccessor, wakes) =
        Application.commitApplicationWaitCompletions preparedWaitCompletions
      (waitSuccessor, _) = Wait.commitWaitRemoval preparedWaitRemoval
      successor =
        replaceStartupWaitState waitSuccessor
          . replaceStartupSortRegistryState
            registrySuccessor
          . replaceStartupStoreState storeSuccessor
          . replaceStartupPublicationState publicationSuccessor
          . replaceStartupPeerStreamState peerStreamSuccessor
          . replaceStartupControlledState controlledSuccessor
          . replaceStartupApplicationState applicationSuccessor
          $ predecessor
      effects =
        orderedEffectBatch
          ( candidateReplyEffects candidate callerReply
              <> fmap wakeEffect wakes
              <> applicationPublicationPeerEffects
                predecessor
                publicationEffects
          )
  case publicationEffects of
    ApplicationPublication.OrdinaryApplicationPublicationEffects {} ->
      Right (successor, effects)
    ApplicationPublication.UnsequencedStructuralApplicationPublicationEffects -> do
      (structuralSuccessor, structuralEffects) <-
        mapContradiction
          (StructuralCoordinator.advanceLocalStructuralWork successor)
      Right (structuralSuccessor, effects <> structuralEffects)

planForward ::
  Application.ApplicationRequestCandidate ->
  PrivateNablaId ->
  PrivateObjectId ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch)
planForward candidate privateNabla privateObject predecessor = do
  prepared <-
    case ApplicationForward.prepareLocalApplicationForward
      (startupGenesis predecessor)
      (currentHeraldMembership predecessor)
      (startupLastObservedTime predecessor)
      candidate
      privateNabla
      privateObject
      (startupControlledState predecessor)
      (startupSortRegistryState predecessor)
      (startupGraphState predecessor)
      (startupStructuralProgressState predecessor)
      (startupPlacementState predecessor)
      (startupPublicationState predecessor)
      (startupStoreState predecessor)
      (startupPeerStreamState predecessor) of
      Left problem -> Left (applicationForwardFailure candidate privateNabla problem)
      Right accepted -> Right accepted
  finishPreparedForward candidate predecessor prepared

finishPreparedForward ::
  Application.ApplicationRequestCandidate ->
  HeraldState ->
  ApplicationForward.PreparedLocalApplicationForward ->
  CallPlan (HeraldState, EffectBatch)
finishPreparedForward candidate predecessor prepared = do
  let ( applicationAfterForward,
        controlledSuccessor,
        publicationSuccessor,
        storeSuccessor,
        peerStreamSuccessor,
        outcome
        ) = ApplicationForward.commitLocalApplicationForward prepared
      publicationEffects = ApplicationForward.localApplicationForwardEffects outcome
      callerReply = ApplicationForward.localApplicationForwardReply outcome
  ready <- case publicationEffects of
    ApplicationPublication.OrdinaryApplicationPublicationEffects {} ->
      readyWaitRegistrations
        applicationAfterForward
        controlledSuccessor
        (startupSortRegistryState predecessor)
        storeSuccessor
        (startupWaitState predecessor)
    ApplicationPublication.UnsequencedStructuralApplicationPublicationEffects ->
      Right []
  preparedWaitRemoval <-
    mapContradiction
      ( Wait.prepareWaitRemoval
          (fmap Wait.waitRegistrationId ready)
          (startupWaitState predecessor)
      )
  preparedWaitCompletions <-
    mapContradiction
      ( Application.prepareApplicationWaitCompletions
          (fmap Wait.waitRegistrationId ready)
          applicationAfterForward
      )
  let (applicationSuccessor, wakes) =
        Application.commitApplicationWaitCompletions preparedWaitCompletions
      (waitSuccessor, _) = Wait.commitWaitRemoval preparedWaitRemoval
      successor =
        replaceStartupWaitState waitSuccessor
          . replaceStartupStoreState storeSuccessor
          . replaceStartupPublicationState publicationSuccessor
          . replaceStartupPeerStreamState peerStreamSuccessor
          . replaceStartupControlledState controlledSuccessor
          . replaceStartupApplicationState applicationSuccessor
          $ predecessor
      effects =
        orderedEffectBatch
          ( candidateReplyEffects candidate callerReply
              <> fmap wakeEffect wakes
              <> applicationPublicationPeerEffects predecessor publicationEffects
          )
  case publicationEffects of
    ApplicationPublication.OrdinaryApplicationPublicationEffects {} ->
      Right (successor, effects)
    ApplicationPublication.UnsequencedStructuralApplicationPublicationEffects -> do
      (structuralSuccessor, structuralEffects) <-
        mapContradiction
          (StructuralCoordinator.advanceLocalStructuralWork successor)
      Right (structuralSuccessor, effects <> structuralEffects)

applicationPublicationPeerEffects ::
  HeraldState ->
  ApplicationPublication.LocalApplicationPublicationEffects ->
  [HeraldEffect]
applicationPublicationPeerEffects predecessor = \case
  ApplicationPublication.OrdinaryApplicationPublicationEffects
    _
    _
    frontierAdvances
    dispatchTickets ->
      [ SendPeerControl binding (PeerStreamFrontierAdvanced direction nextSequence)
      | (peer, (direction, nextSequence)) <- Map.toAscList frontierAdvances,
        Just binding <-
          [ Discovery.currentPeerBinding
              peer
              (startupDiscoveryState predecessor)
          ]
      ]
        <> fmap SchedulePeerDispatch dispatchTickets
  ApplicationPublication.UnsequencedStructuralApplicationPublicationEffects -> []

planQuery ::
  Bool ->
  Application.ApplicationRequestCandidate ->
  ApplicationQuery ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch)
planQuery takeMatches candidate query predecessor = do
  admitted <- resolveQuery candidate query predecessor
  (cut, evidence) <- effectiveQueryCut admitted predecessor
  if takeMatches
    then do
      takePlan <-
        mapContradiction
          (StoreObservation.prepareEffectiveLocalTakePlan evidence cut)
      preparedTake <-
        mapContradiction
          ( Store.prepareExactLocalTake
              (StoreObservation.effectiveLocalTakeKeys takePlan)
              (startupStoreState predecessor)
          )
      let (storeSuccessor, _) = Store.commitExactLocalTake preparedTake
      completeQuery
        candidate
        True
        (StoreObservation.effectiveLocalTakeMatches takePlan)
        storeSuccessor
        predecessor
    else do
      readPlan <-
        mapContradiction
          (StoreObservation.prepareEffectiveReadPlan evidence cut)
      completeQuery
        candidate
        False
        (StoreObservation.effectiveReadMatches readPlan)
        (startupStoreState predecessor)
        predecessor

completeQuery ::
  Application.ApplicationRequestCandidate ->
  Bool ->
  [StoreObservation.EffectiveStoreMatch] ->
  Store.State ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch)
completeQuery candidate wasTake matches storeSuccessor predecessor = do
  (localizedApplication, localizedControlled, values) <-
    localizeMatches
      (Application.applicationRequestCandidateProcess candidate)
      wasTake
      matches
      predecessor
  let result
        | wasTake = LocalTakeCompleted values
        | otherwise = ReadCompleted values
  preparedRequest <-
    mapContradiction
      ( Application.prepareApplicationRequestCompletionFromLocalizedState
          candidate
          localizedApplication
          result
      )
  let (applicationSuccessor, reply) =
        Application.commitApplicationRequest preparedRequest
      successor =
        replaceStartupStoreState storeSuccessor
          . replaceStartupControlledState localizedControlled
          . replaceStartupApplicationState applicationSuccessor
          $ predecessor
  withCandidates <-
    if wasTake
      then mapContradiction (DisappearanceLive.observeTakenPublications (fmap StoreObservation.effectiveStoreMatchRawPublication matches) successor)
      else Right successor
  Right
    ( withCandidates,
      candidateReplyEffect candidate reply
    )

planWait ::
  Application.ApplicationRequestCandidate ->
  [ApplicationQuery] ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch)
planWait candidate queries predecessor = do
  admitted <- traverse (\query -> resolveQuery candidate query predecessor) queries
  observed <-
    traverse
      (\query -> effectiveWaitMatches query predecessor)
      admitted
  if any (not . null) observed
    then
      Right
        ( completeSimpleState candidate (WaitCompleted WaitReady) predecessor,
          completeSimpleEffect candidate (WaitCompleted WaitReady)
        )
    else do
      let registration =
            Wait.waitRegistration
              (Application.applicationRequestCandidateWaitId candidate)
              (Application.applicationRequestCandidateSession candidate)
              (Application.applicationRequestCandidateId candidate)
              (Application.applicationRequestCandidateProcess candidate)
              (Application.applicationRequestCandidatePosition candidate)
              admitted
      preparedWait <-
        mapContradiction
          (Wait.prepareWaitRegistration registration (startupWaitState predecessor))
      let preparedRequest = Application.prepareApplicationWaitAcceptance candidate
          (applicationSuccessor, reply) =
            Application.commitApplicationRequest preparedRequest
          successor =
            replaceStartupWaitState (Wait.commitWaitRegistration preparedWait)
              . replaceStartupApplicationState applicationSuccessor
              $ predecessor
      Right
        ( successor,
          candidateReplyEffect candidate reply
        )
completeSimpleState ::
  Application.ApplicationRequestCandidate ->
  RegularCallResult ->
  HeraldState ->
  HeraldState
completeSimpleState candidate result predecessor =
  replaceStartupApplicationState applicationSuccessor predecessor
  where
    (applicationSuccessor, _) =
      Application.commitApplicationRequest
        (Application.prepareApplicationRequestCompletion candidate result)

completeSimpleEffect ::
  Application.ApplicationRequestCandidate ->
  RegularCallResult ->
  EffectBatch
completeSimpleEffect candidate result =
  candidateReplyEffect candidate reply
  where
    (_, reply) =
      Application.commitApplicationRequest
        (Application.prepareApplicationRequestCompletion candidate result)

resolveQuery ::
  Application.ApplicationRequestCandidate ->
  ApplicationQuery ->
  HeraldState ->
  CallPlan ResolvedQuery
resolveQuery candidate query predecessor = do
  globalized <-
    case globalizeApplicationQuery roles process query applicationState of
      Left problem -> Left (queryGlobalizationFailure problem)
      Right value -> Right value
  branches <- traverse (resolveDelta globalized) (globalizedQueryDeltas globalized)
  Right (resolvedQuery branches)
  where
    process = Application.applicationRequestCandidateProcess candidate
    applicationState = startupApplicationState predecessor
    controlled = startupControlledState predecessor
    placement = startupPlacementState predecessor
    store = startupStoreState predecessor
    registry = startupSortRegistryState predecessor
    progress = startupStructuralProgressState predecessor
    roles =
      Application.processRoleView
        ( OracleProjection.projectedProcessEpochs
            (startupOracleProjectionState predecessor)
        )
    resolveDelta globalized (privateDelta, delta) = do
      authority <- resolveReaderAuthority privateDelta delta
      let sortId = ControlledOperate.controlledReadSortId authority
          occurrenceId = ControlledOperate.controlledReadOccurrenceId authority
      localPlacement <-
        maybe
          (Left (CallRejected (ApplicationDeltaNotLocal privateDelta)))
          Right
          (Placement.lookupLocalPlacement delta placement)
      slot <-
        maybe
          (Left (CallRejected (ApplicationDeltaStoreUnavailable privateDelta)))
          Right
          (Store.lookupStoreSlot delta store)
      entry <-
        requireChecked
          (SortRegistry.lookupEffectiveSort sortId registry)
      if Placement.localPlacementSortId localPlacement /= sortId
        || Placement.localPlacementOccurrenceId localPlacement /= occurrenceId
        || Placement.localPlacementControllerObject localPlacement
          /= globalObjectIdFromDeltaId delta
        || Placement.localPlacementProcessEpoch localPlacement /= process
        || Placement.localPlacementHeraldEpoch localPlacement
          /= ControlledOperate.controlledReadHeraldEpoch authority
        || Store.storeSlotSortId slot /= sortId
        || Store.storeSlotOccurrenceId slot /= occurrenceId
        || Placement.localPlacementStoreIncarnation localPlacement
          /= Store.storeSlotIncarnation slot
        || SortRegistry.registryEntryOccurrenceId entry /= occurrenceId
        then Left CallContradiction
        else Right ()
      case (SortRegistry.registryEntryPredefinedRole entry, globalizedQueryComparisons globalized) of
        (Just SortDefinitionRole, projection : _) ->
          Left
            ( CallRejected
                (ApplicationSortDefinitionProjectionUnsupported projection)
            )
        _ -> Right ()
      checkedPredicate <-
        case checkQueryPredicate
          ( descriptorSchema
              ( canonicalCheckedDescriptor
                  (SortRegistry.registryEntryDescriptor entry)
              )
          )
          (globalizedQueryPredicate globalized) of
          Left problem -> Left (queryAdmissionFailure globalized problem)
          Right checked -> Right checked
      Right
        ( resolvedQueryBranch
            delta
            (Store.storeSlotIncarnation slot)
            checkedPredicate
        )

    -- Bootstrap and structurally installed readers are disjoint authority
    -- namespaces. Select by the exact Delta rather than by whether its owning
    -- process happens to have bootstrap facts: a bootstrap application may
    -- publish and subsequently query through a dynamic Delta.
    resolveReaderAuthority privateDelta delta =
      case ControlledOperate.checkControlledRead process delta controlled progress of
        Left (ControlledOperate.ControlledReadRoleMismatch _) ->
          Left (CallRejected (ApplicationDeltaRoleMismatch privateDelta))
        Left (ControlledOperate.ControlledReadOperateNotPermitted _ _) ->
          Left (CallRejected ApplicationOperateNotPermitted)
        Left (ControlledOperate.ControlledReadInvariantContradiction _) ->
          Left CallContradiction
        Right authority -> Right authority

effectiveQueryCut ::
  ResolvedQuery ->
  HeraldState ->
  CallPlan
    ( StoreObservation.EffectiveQueryCut,
      Map.Map
        StoreObservation.EffectiveCandidateKey
        StoreObservation.EffectiveStoreEvidence
    )
effectiveQueryCut query state =
  effectiveQueryCutForWithZombieOverlay
    ( Isolation.isolationWitnessResidentProcessOverlay
        (Isolation.stateWitness (startupIsolationState state))
    )
    (startupControlledState state)
    (startupSortRegistryState state)
    (startupStoreState state)
    query

effectiveQueryCutFor ::
  Controlled.State ->
  SortRegistry.State ->
  Store.State ->
  ResolvedQuery ->
  CallPlan
    ( StoreObservation.EffectiveQueryCut,
      Map.Map
        StoreObservation.EffectiveCandidateKey
        StoreObservation.EffectiveStoreEvidence
    )
effectiveQueryCutFor controlled registry store query = do
  effectiveQueryCutForWithZombieOverlay Set.empty controlled registry store query

effectiveQueryCutForWithZombieOverlay ::
  Set.Set ProcessEpochId ->
  Controlled.State ->
  SortRegistry.State ->
  Store.State ->
  ResolvedQuery ->
  CallPlan
    ( StoreObservation.EffectiveQueryCut,
      Map.Map
        StoreObservation.EffectiveCandidateKey
        StoreObservation.EffectiveStoreEvidence
    )
effectiveQueryCutForWithZombieOverlay zombies controlled registry store query = do
  cut <- mapContradiction (StoreObservation.captureEffectiveQueryCut query store)
  evidence <-
    Map.fromList
      <$> traverse bindCandidate (StoreObservation.effectiveQueryCutRawCandidates cut)
  Right (cut, evidence)
  where
    bindCandidate (key, publication) = do
      entry <-
        maybe
          (Left CallContradiction)
          Right
          (SortRegistry.lookupEffectiveSort (checkedPublicationSort publication) registry)
      context <-
        mapContradiction
          ( checkEffectivePublicationContext
              (SortRegistry.registryEntryDescriptor entry)
              controlled
              publication
          )
      bound <-
        mapContradiction
          ( StoreObservation.effectiveStoreEvidence
              publication
              ( projectEffectivePublicationZombieOverlay
                  zombies
                  (interpretEffectivePublication context)
              )
          )
      Right (key, bound)

effectiveWaitMatches ::
  ResolvedQuery ->
  HeraldState ->
  CallPlan [StoreObservation.EffectiveStoreMatch]
effectiveWaitMatches query state =
  effectiveWaitMatchesFor
    (startupControlledState state)
    (startupSortRegistryState state)
    (startupStoreState state)
    query

effectiveWaitMatchesFor ::
  Controlled.State ->
  SortRegistry.State ->
  Store.State ->
  ResolvedQuery ->
  CallPlan [StoreObservation.EffectiveStoreMatch]
effectiveWaitMatchesFor controlled registry store query = do
  (cut, evidence) <- effectiveQueryCutFor controlled registry store query
  plan <-
    mapContradiction
      (StoreObservation.prepareEffectiveWaitPlan evidence cut)
  Right (StoreObservation.effectiveWaitMatches plan)

localizeMatches ::
  ProcessEpochId ->
  Bool ->
  [StoreObservation.EffectiveStoreMatch] ->
  HeraldState ->
  CallPlan (Application.State, Controlled.State, [ApplicationValue])
localizeMatches process wasTake matches predecessor =
  foldM
    localizeOne
    ( startupApplicationState predecessor,
      startupControlledState predecessor,
      []
    )
    matches
  where
    roles =
      Application.processRoleView
        ( OracleProjection.projectedProcessEpochs
            (startupOracleProjectionState predecessor)
        )
    carrierSort =
      SortRegistry.registryEntrySortId
        <$> SortRegistry.lookupPredefinedRole
          SortDefinitionRole
          (startupSortRegistryState predecessor)
    registry = startupSortRegistryState predecessor
    localizeOne (application, controlled, values) match
      | Just (checkedPublicationSort publication) == carrierSort = do
          definition <-
            mapContradiction
              (presentApplicationSortDefinition effectiveValue)
          Right
            ( application,
              controlled,
              values <> [ApplicationValue.SortDefinitionValue definition]
            )
      | otherwise = do
          controlledSuccessor <- localizeControlled controlled match publication
          prepared <-
            mapContradiction
              ( Application.prepareOrdinaryApplicationValueLocalizationWithZombieOverlay
                  ( Isolation.isolationWitnessResidentProcessOverlay
                      (Isolation.stateWitness (startupIsolationState predecessor))
                  )
                  roles
                  process
                  effectiveValue
                  application
              )
          let (applicationSuccessor, value) =
                Application.commitOrdinaryApplicationValueLocalization prepared
          Right
            ( applicationSuccessor,
              controlledSuccessor,
              values <> [value]
            )
      where
        publication = StoreObservation.effectiveStoreMatchRawPublication match
        effectiveValue = StoreObservation.effectiveStoreMatchValue match

    localizeControlled controlled match publication = do
      entry <-
        maybe
          (Left CallContradiction)
          Right
          (SortRegistry.lookupEffectiveSort (checkedPublicationSort publication) registry)
      if descriptorKind
        (canonicalCheckedDescriptor (SortRegistry.registryEntryDescriptor entry))
        /= ControlledSort
        then Right controlled
        else do
          observation <-
            mapContradiction
              ( Controlled.checkControlledObservation
                  (SortRegistry.registryEntryDescriptor entry)
                  (SortRegistry.registryEntryOccurrenceId entry)
                  publication
              )
          prepared <-
            mapContradiction
              ( (if wasTake then Controlled.prepareControlledStoreLocalTake else Controlled.prepareControlledStoreRead)
                  process
                  ( StoreObservation.effectiveCandidateKeyDelta
                      (StoreObservation.effectiveStoreMatchKey match)
                  )
                  (StoreObservation.effectiveStoreMatchStrength match)
                  observation
                  controlled
              )
          Right (Controlled.commitControlledStoreObservation prepared)

readyWaitRegistrations ::
  Application.State ->
  Controlled.State ->
  SortRegistry.State ->
  Store.State ->
  Wait.State ->
  CallPlan [Wait.WaitRegistration]
readyWaitRegistrations application controlled registry store waitState =
  case Application.applicationGatePhase application of
    Application.ApplicationGateClosed _ -> Right []
    Application.ApplicationGateOpen _ -> filterM registrationReady (Wait.waitRegistrations waitState)
  where
    registrationReady registration =
      or <$> traverse queryReady (Wait.waitRegistrationQueries registration)
    queryReady query =
      not . null <$> effectiveWaitMatchesFor controlled registry store query

applyCancellation ::
  ApplicationSessionBinding ->
  ApplicationSessionId ->
  RequestId ->
  WaitId ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
applyCancellation binding session request wait predecessor =
  case Application.prepareApplicationWaitCancellation
    session
    binding
    request
    wait
    (startupApplicationState predecessor) of
    Left problem -> requestSessionFailure binding problem predecessor
    Right outcome -> case outcome of
      Application.ApplicationWaitCancellationReply reply ->
        checkedResult predecessor (replyEffect binding reply)
      Application.ExactApplicationWaitCancellation prepared ->
        case Wait.prepareWaitRemoval [wait] (startupWaitState predecessor) of
          Left _ -> invariantFault
          Right waitPrepared ->
            let (applicationSuccessor, reply) =
                  Application.commitApplicationWaitCancellation prepared
                (waitSuccessor, _) = Wait.commitWaitRemoval waitPrepared
                successor =
                  replaceStartupWaitState waitSuccessor
                    . replaceStartupApplicationState applicationSuccessor
                    $ predecessor
             in checkedResult successor (replyEffect binding reply)

finishPlan ::
  Application.ApplicationRequestCandidate ->
  HeraldState ->
  CallPlan (HeraldState, EffectBatch) ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
finishPlan candidate predecessor = \case
  Left (CallRejected rejection) ->
    let prepared = Application.prepareApplicationRequestRejection candidate rejection
        (applicationSuccessor, reply) =
          Application.commitApplicationRequest prepared
        successor =
          replaceStartupApplicationState applicationSuccessor predecessor
     in checkedResult
          successor
          (candidateReplyEffect candidate reply)
  Left (CallRejectedAfterControlledObservation controlled rejection) ->
    let prepared = Application.prepareApplicationRequestRejection candidate rejection
        (applicationSuccessor, reply) =
          Application.commitApplicationRequest prepared
        successor =
          replaceStartupControlledState controlled
            . replaceStartupApplicationState applicationSuccessor
            $ predecessor
     in checkedResult
          successor
          (candidateReplyEffect candidate reply)
  Left CallContradiction -> invariantFault
  Right (successor, effects) -> checkedResult successor effects

requestSessionFailure ::
  ApplicationSessionBinding ->
  Application.ApplicationSessionTransitionError ->
  HeraldState ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
requestSessionFailure binding problem predecessor = case problem of
  Application.ApplicationSessionRejected rejection ->
    checkedResult
      predecessor
      ( singletonEffectBatch
          ( RejectApplicationConnection
              (EstablishedApplicationDisposition binding)
              rejection
          )
      )
  Application.ApplicationSessionInvariantFault _ -> invariantFault
  Application.ApplicationSessionGatePending -> invariantFault

queryGlobalizationFailure :: ApplicationQueryGlobalizationError -> CallFailure
queryGlobalizationFailure problem = case problem of
  ApplicationQueryInvalidFieldName field _ ->
    CallRejected (ApplicationInvalidFieldName field)
  ApplicationQueryIdentityError valueProblem -> case valueProblem of
    Application.ApplicationValueRejected rejection -> case rejection of
      Application.ApplicationValueUnknownPrivateId privateIdentity ->
        CallRejected (ApplicationUnknownPrivateIdentity privateIdentity)
      Application.ApplicationValueInvalidFieldName field _ ->
        CallRejected (ApplicationInvalidFieldName field)
      Application.ApplicationValueProcessRoleMismatch privateProcess ->
        CallRejected (ApplicationProcessRoleMismatch privateProcess)
      Application.ApplicationValueMisplacedSortDefinition -> CallContradiction
    Application.ApplicationValueGlobalizationInvariant _ -> CallContradiction

queryAdmissionFailure ::
  GlobalizedApplicationQuery ->
  QueryAdmissionError ->
  CallFailure
queryAdmissionFailure globalized = \case
  QueryProjectionInvalid projection _ -> invalidProjection projection
  QueryProjectionMustBeScalar projection -> invalidProjection projection
  QueryLiteralTypeMismatch _ _ _ ->
    CallRejected ApplicationQueryPredicateMismatch
  where
    invalidProjection projection =
      maybe
        CallContradiction
        (CallRejected . ApplicationInvalidQueryProjection)
        (globalizedQueryApplicationProjection projection globalized)

applicationPublicationFailure ::
  Application.ApplicationRequestCandidate ->
  PrivateNablaId ->
  ApplicationPublication.LocalApplicationPublicationError ->
  CallFailure
applicationPublicationFailure candidate privateNabla = \case
  ApplicationPublication.LocalApplicationPublicationValueError problem ->
    applicationPublicationValueFailure problem
  ApplicationPublication.LocalApplicationPublicationAccessError problem ->
    applicationPublicationAccessFailure candidate privateNabla problem
  ApplicationPublication.LocalApplicationPublicationOwnerError problem ->
    case problem of
      Publication.PublicationApplicationValueRejected _ ->
        CallRejected ApplicationSortDescriptorRejected
      _ -> CallContradiction
  ApplicationPublication.LocalApplicationPublicationObservationError _ ->
    CallContradiction
  ApplicationPublication.LocalApplicationPublicationFirstUseError problem ->
    controlledFirstUseFailure candidate privateNabla problem
  ApplicationPublication.LocalApplicationPublicationUpdateError problem ->
    controlledUpdateFailure privateNabla problem
  ApplicationPublication.LocalApplicationPublicationSortRegistryError _ ->
    CallContradiction
  ApplicationPublication.LocalApplicationPublicationRouteError _ ->
    CallContradiction
  ApplicationPublication.LocalApplicationPublicationStoreError _ ->
    CallContradiction
  ApplicationPublication.LocalApplicationPublicationPeerPublicationError _ ->
    CallContradiction
  ApplicationPublication.LocalApplicationPublicationPeerStreamError _ ->
    CallContradiction
  ApplicationPublication.LocalApplicationPublicationOwnerEpochMismatch _ _ ->
    CallContradiction
  ApplicationPublication.LocalApplicationPublicationDispositionContradiction ->
    CallContradiction

heldApplicationPublicationFailure ::
  Application.ApplicationRequestCandidate ->
  PrivateNablaId ->
  ApplicationPublication.LocalApplicationPublicationError ->
  CallFailure
heldApplicationPublicationFailure candidate privateNabla problem =
  case problem of
    ApplicationPublication.LocalApplicationPublicationAccessError accessProblem ->
      heldApplicationPublicationAccessFailure candidate privateNabla accessProblem
    ApplicationPublication.LocalApplicationPublicationFirstUseError firstUseProblem ->
      case firstUseProblem of
        Controlled.ControlledFirstUseAuthorityContradiction {} -> rejected
        Controlled.ControlledFirstUseObjectConflict _ -> rejected
        _ -> applicationPublicationFailure candidate privateNabla problem
    ApplicationPublication.LocalApplicationPublicationSortRegistryError _ ->
      CallRejected ApplicationSortDescriptorRejected
    ApplicationPublication.LocalApplicationPublicationRouteError _ -> rejected
    _ -> applicationPublicationFailure candidate privateNabla problem
  where
    rejected = CallRejected ApplicationOperateNotPermitted

heldApplicationPublicationAccessFailure ::
  Application.ApplicationRequestCandidate ->
  PrivateNablaId ->
  ApplicationPublication.ApplicationPublicationAccessError ->
  CallFailure
heldApplicationPublicationAccessFailure candidate privateNabla problem =
  case problem of
    ApplicationPublication.ApplicationPublicationEffectiveSortUnavailable _ -> rejected
    ApplicationPublication.ApplicationPublicationWriterSortMismatch _ _ -> rejected
    ApplicationPublication.ApplicationPublicationWriterOccurrenceMismatch _ _ -> rejected
    ApplicationPublication.ApplicationPublicationReservationAuthorityMismatch {} -> rejected
    ApplicationPublication.ApplicationPublicationControlledRecordUnavailable _ -> rejected
    ApplicationPublication.ApplicationPublicationControlledLabelContradiction _ -> rejected
    ApplicationPublication.ApplicationPublicationEdgeValueContradiction -> rejected
    ApplicationPublication.ApplicationPublicationSortDefinitionValueContradiction -> rejected
    _ -> applicationPublicationAccessFailure candidate privateNabla problem
  where
    rejected = CallRejected ApplicationOperateNotPermitted

applicationForwardFailure ::
  Application.ApplicationRequestCandidate ->
  PrivateNablaId ->
  ApplicationForward.LocalApplicationForwardError ->
  CallFailure
applicationForwardFailure candidate privateNabla = \case
  ApplicationForward.LocalApplicationForwardDispositionContradiction ->
    CallContradiction
  ApplicationForward.LocalApplicationForwardWriterIdentityError problem ->
    applicationValueGlobalizationFailure problem
  ApplicationForward.LocalApplicationForwardTargetIdentityError problem ->
    applicationValueGlobalizationFailure problem
  ApplicationForward.LocalApplicationForwardAccessError problem ->
    applicationPublicationAccessFailure candidate privateNabla problem
  ApplicationForward.LocalApplicationForwardTargetUnavailable _ ->
    CallRejected ApplicationOperateNotPermitted
  ApplicationForward.LocalApplicationForwardTargetNotControlled _ ->
    CallRejected ApplicationOperateNotPermitted
  ApplicationForward.LocalApplicationForwardSortMismatch {} ->
    CallRejected ApplicationOperateNotPermitted
  ApplicationForward.LocalApplicationForwardOccurrenceMismatch {} ->
    CallContradiction
  ApplicationForward.LocalApplicationForwardPossessionUnavailable _ ->
    CallRejected ApplicationOperateNotPermitted
  ApplicationForward.LocalApplicationForwardObservationError _ ->
    CallContradiction
  ApplicationForward.LocalApplicationForwardObsoleteObservationError _ ->
    CallContradiction
  ApplicationForward.LocalApplicationForwardRetentionExpired controlled _ _ _ _ ->
    CallRejectedAfterControlledObservation controlled ApplicationOperateNotPermitted
  ApplicationForward.LocalApplicationForwardOwnerError _ ->
    CallContradiction
  ApplicationForward.LocalApplicationForwardControlledError _ ->
    CallContradiction
  ApplicationForward.LocalApplicationForwardRouteError _ ->
    CallContradiction
  ApplicationForward.LocalApplicationForwardStoreError _ ->
    CallContradiction
  ApplicationForward.LocalApplicationForwardPeerPublicationError _ ->
    CallContradiction
  ApplicationForward.LocalApplicationForwardPeerStreamError _ ->
    CallContradiction
  ApplicationForward.LocalApplicationForwardOwnerEpochMismatch {} ->
    CallContradiction

heldApplicationForwardFailure ::
  Application.ApplicationRequestCandidate ->
  PrivateNablaId ->
  ApplicationForward.LocalApplicationForwardError ->
  CallFailure
heldApplicationForwardFailure candidate privateNabla problem =
  case problem of
    ApplicationForward.LocalApplicationForwardAccessError accessProblem ->
      heldApplicationPublicationAccessFailure candidate privateNabla accessProblem
    ApplicationForward.LocalApplicationForwardOccurrenceMismatch {} -> rejected
    ApplicationForward.LocalApplicationForwardObservationError _ -> rejected
    ApplicationForward.LocalApplicationForwardControlledError _ -> rejected
    ApplicationForward.LocalApplicationForwardRouteError _ -> rejected
    _ -> applicationForwardFailure candidate privateNabla problem
  where
    rejected = CallRejected ApplicationOperateNotPermitted

applicationPublicationValueFailure ::
  ApplicationPublication.ApplicationPublicationValueError ->
  CallFailure
applicationPublicationValueFailure = \case
  ApplicationPublication.ApplicationPublicationOrdinaryValueRejected problem ->
    applicationValueGlobalizationFailure problem
  ApplicationPublication.ApplicationPublicationSortDefinitionRejected problem ->
    CallRejected (sortDefinitionRejection problem)
  ApplicationPublication.ApplicationPublicationSortDefinitionCarrierMismatch _ _ ->
    CallRejected ApplicationSortDescriptorRejected
  ApplicationPublication.ApplicationPublicationSortDefinitionCarrierRequiresStructuredValue _ ->
    CallRejected ApplicationSortDescriptorRejected

applicationValueGlobalizationFailure ::
  Application.ApplicationValueGlobalizationError ->
  CallFailure
applicationValueGlobalizationFailure = \case
  Application.ApplicationValueRejected rejection -> case rejection of
    Application.ApplicationValueUnknownPrivateId privateIdentity ->
      CallRejected (ApplicationUnknownPrivateIdentity privateIdentity)
    Application.ApplicationValueInvalidFieldName field _ ->
      CallRejected (ApplicationInvalidFieldName field)
    Application.ApplicationValueProcessRoleMismatch privateProcess ->
      CallRejected (ApplicationProcessRoleMismatch privateProcess)
    Application.ApplicationValueMisplacedSortDefinition ->
      CallRejected ApplicationSortDescriptorRejected
  Application.ApplicationValueGlobalizationInvariant _ -> CallContradiction

applicationPublicationAccessFailure ::
  Application.ApplicationRequestCandidate ->
  PrivateNablaId ->
  ApplicationPublication.ApplicationPublicationAccessError ->
  CallFailure
applicationPublicationAccessFailure candidate privateNabla = \case
  ApplicationPublication.ApplicationPublicationWriterIdentityError problem ->
    applicationValueGlobalizationFailure problem
  ApplicationPublication.ApplicationPublicationBootstrapOperateError problem ->
    controlledBootstrapOperateFailure privateNabla problem
  ApplicationPublication.ApplicationPublicationDynamicOperateError problem ->
    controlledDynamicOperateFailure privateNabla problem
  ApplicationPublication.ApplicationPublicationEffectiveSortUnavailable _ ->
    CallContradiction
  ApplicationPublication.ApplicationPublicationWriterSortMismatch _ _ ->
    CallContradiction
  ApplicationPublication.ApplicationPublicationWriterOccurrenceMismatch _ _ ->
    CallContradiction
  ApplicationPublication.ApplicationPublicationMutationUnavailable _ ->
    CallRejected ApplicationOperateNotPermitted
  ApplicationPublication.ApplicationPublicationImmutableUpdate _ ->
    CallRejected ApplicationOperateNotPermitted
  ApplicationPublication.ApplicationPublicationReservationUnavailable generated ->
    applicationReservationFailure candidate privateNabla generated
  ApplicationPublication.ApplicationPublicationReservationBindingMismatch generated _ _ ->
    applicationReservationFailure candidate privateNabla generated
  ApplicationPublication.ApplicationPublicationReservationAuthorityMismatch {} ->
    CallContradiction
  ApplicationPublication.ApplicationPublicationControlledRecordUnavailable _ ->
    CallContradiction
  ApplicationPublication.ApplicationPublicationControlledLabelContradiction _ ->
    CallContradiction
  ApplicationPublication.ApplicationPublicationControlledValueShapeContradiction ->
    CallRejected ApplicationSortDescriptorRejected
  ApplicationPublication.ApplicationPublicationStructuralCarrierReferenceError _ ->
    CallContradiction
  ApplicationPublication.ApplicationPublicationStructuralReferenceNotPossessed {} ->
    CallRejected ApplicationOperateNotPermitted
  ApplicationPublication.ApplicationPublicationStructuralReferenceNotCurrentVertex _ ->
    CallRejected ApplicationOperateNotPermitted
  ApplicationPublication.ApplicationPublicationEdgeValueContradiction ->
    CallContradiction
  ApplicationPublication.ApplicationPublicationSortDefinitionValueContradiction ->
    CallContradiction

controlledBootstrapOperateFailure ::
  PrivateNablaId ->
  Controlled.ControlledBootstrapOperateError ->
  CallFailure
controlledBootstrapOperateFailure privateNabla = \case
  Controlled.ControlledBootstrapOperateProcessUnavailable _ -> CallContradiction
  Controlled.ControlledBootstrapOperateWriterUnavailable _ ->
    CallRejected (ApplicationNablaRoleMismatch privateNabla)
  Controlled.ControlledBootstrapOperateWriterProcessMismatch {} ->
    CallRejected (ApplicationNablaRoleMismatch privateNabla)
  Controlled.ControlledBootstrapOperateProcessRoleContradiction _ ->
    CallContradiction
  Controlled.ControlledBootstrapOperateWriterRoleContradiction _ ->
    CallContradiction
  Controlled.ControlledBootstrapOperateProcessNotPossessed _ ->
    CallRejected ApplicationOperateNotPermitted
  Controlled.ControlledBootstrapOperateWriterNotPossessed {} ->
    CallRejected ApplicationOperateNotPermitted

controlledDynamicOperateFailure ::
  PrivateNablaId ->
  ControlledOperate.ControlledOperateError ->
  CallFailure
controlledDynamicOperateFailure privateNabla = \case
  ControlledOperate.ControlledOperateBootstrapError problem ->
    controlledBootstrapOperateFailure privateNabla problem
  ControlledOperate.ControlledOperateDynamicWriterUnavailable _ ->
    CallRejected (ApplicationNablaRoleMismatch privateNabla)
  ControlledOperate.ControlledOperateDynamicControllerMismatch {} ->
    CallRejected ApplicationOperateNotPermitted
  ControlledOperate.ControlledOperateDynamicResidenceMismatch ->
    CallRejected ApplicationOperateNotPermitted
  ControlledOperate.ControlledOperateDynamicWriterNotPossessed {} ->
    CallRejected ApplicationOperateNotPermitted
  ControlledOperate.ControlledOperateDynamicRecordNotCurrent _ ->
    CallRejected ApplicationOperateNotPermitted
  ControlledOperate.ControlledOperateDynamicRecordLabelMismatch _ ->
    CallRejected ApplicationOperateNotPermitted
  _ -> CallContradiction

controlledFirstUseFailure ::
  Application.ApplicationRequestCandidate ->
  PrivateNablaId ->
  Controlled.ControlledFirstUseError ->
  CallFailure
controlledFirstUseFailure candidate privateNabla = \case
  Controlled.ControlledFirstUseReservationUnavailable generated ->
    applicationReservationFailure candidate privateNabla generated
  Controlled.ControlledFirstUseWriterUnavailable _ ->
    CallRejected (ApplicationNablaRoleMismatch privateNabla)
  Controlled.ControlledFirstUseWriterNotPossessed {} ->
    CallRejected ApplicationOperateNotPermitted
  Controlled.ControlledFirstUseInitialLabelUnavailable _ ->
    CallRejected ApplicationOperateNotPermitted
  Controlled.ControlledFirstUseMustBeCurrent ->
    CallRejected ApplicationSortDescriptorRejected
  Controlled.ControlledFirstUseAuthorityContradiction {} -> CallContradiction
  Controlled.ControlledFirstUsePublicationNablaMismatch {} -> CallContradiction
  Controlled.ControlledFirstUsePublicationAuthorityMismatch {} -> CallContradiction
  Controlled.ControlledFirstUseSortMismatch {} -> CallContradiction
  Controlled.ControlledFirstUseOccurrenceMismatch {} -> CallContradiction
  Controlled.ControlledFirstUseObjectConflict _ -> CallContradiction
  Controlled.ControlledFirstUseConflict {} -> CallContradiction

controlledUpdateFailure ::
  PrivateNablaId ->
  Controlled.ControlledUpdateError ->
  CallFailure
controlledUpdateFailure privateNabla = \case
  Controlled.ControlledUpdateObjectUnavailable _ ->
    CallRejected ApplicationOperateNotPermitted
  Controlled.ControlledUpdateNotCurrent {} ->
    CallRejected ApplicationOperateNotPermitted
  Controlled.ControlledUpdateWriterUnavailable _ ->
    CallRejected (ApplicationNablaRoleMismatch privateNabla)
  Controlled.ControlledUpdateWriterNotPossessed {} ->
    CallRejected ApplicationOperateNotPermitted
  Controlled.ControlledUpdateObjectNotPossessed {} ->
    CallRejected ApplicationOperateNotPermitted
  Controlled.ControlledUpdateSortMismatch {} ->
    CallRejected ApplicationOperateNotPermitted
  Controlled.ControlledUpdateOccurrenceMismatch {} ->
    CallRejected ApplicationOperateNotPermitted
  Controlled.ControlledUpdateStructuralRoleMismatch {} ->
    CallRejected ApplicationOperateNotPermitted
  Controlled.ControlledUpdateLatestLabelUnavailable {} ->
    CallRejected ApplicationOperateNotPermitted
  Controlled.ControlledUpdateLabelMismatch {} ->
    CallRejected ApplicationOperateNotPermitted
  Controlled.ControlledUpdatePublicationNablaMismatch {} -> CallContradiction
  Controlled.ControlledUpdatePublicationAuthorityMismatch {} -> CallContradiction
  Controlled.ControlledUpdatePublicationConflict _ -> CallContradiction

applicationReservationFailure ::
  Application.ApplicationRequestCandidate ->
  PrivateNablaId ->
  GlobalUniqueId ->
  CallFailure
applicationReservationFailure candidate privateNabla generated =
  if Application.applicationRequestCandidateReplyOwned candidate
    then case PrivateIdentity.lookupPrivateUniqueId
      (Application.applicationRequestCandidateProcess candidate)
      generated
      ( Application.applicationPrivateIdentity
          (Application.applicationRequestCandidateState candidate)
      ) of
      Right (Just privateObject) ->
        CallRejected
          ( ApplicationReservationUnavailable
              privateNabla
              (asPrivateObjectId privateObject)
          )
      _ -> CallContradiction
    else CallRejected ApplicationOperateNotPermitted

sortDefinitionRejection ::
  ApplicationSortDefinitionRejection -> ApplicationRejection
sortDefinitionRejection problem = case problem of
  ApplicationDescriptorInvalidFieldName field _ -> ApplicationInvalidFieldName field
  ApplicationDescriptorInvalidSchema _ -> ApplicationSortDescriptorRejected
  ApplicationDescriptorRejected _ -> ApplicationSortDescriptorRejected
  ApplicationDescriptorClaimedSortIdMismatch claimed computed ->
    ApplicationSortIdClaimMismatch claimed computed

replyEffect ::
  ApplicationSessionBinding -> ApplicationRequestReply -> EffectBatch
replyEffect binding reply =
  singletonEffectBatch (SendApplicationReply binding reply)

candidateReplyEffects ::
  Application.ApplicationRequestCandidate ->
  ApplicationRequestReply ->
  [HeraldEffect]
candidateReplyEffects candidate reply
  | Application.applicationRequestCandidateReplyDeliverable candidate =
      [ SendApplicationReply
          (Application.applicationRequestCandidateBinding candidate)
          reply
      ]
  | otherwise = []

candidateReplyEffect ::
  Application.ApplicationRequestCandidate ->
  ApplicationRequestReply ->
  EffectBatch
candidateReplyEffect candidate =
  orderedEffectBatch . candidateReplyEffects candidate

wakeEffect :: Application.ApplicationWaitWake -> HeraldEffect
wakeEffect wake =
  SendApplicationWaitWake
    (Application.applicationWaitWakeBinding wake)
    (Application.applicationWaitWakeCursor wake)
    (Application.applicationWaitWakeRequestId wake)
    (Application.applicationWaitWakeWaitId wake)
    (Application.applicationWaitWakeResult wake)

checkedResult ::
  HeraldState ->
  EffectBatch ->
  Either HeraldInvariantFault (HeraldState, EffectBatch)
checkedResult state effects = Right (state, effects)

currentHeraldMembership :: HeraldState -> HeraldMembershipGeneration
currentHeraldMembership =
  OracleProjection.oracleViewCurrentHeraldMembership
    . OracleProjection.oracleView
    . startupOracleProjectionState

invariantFault :: Either HeraldInvariantFault value
invariantFault =
  Left
    ( HeraldTransitionInvariant
        HeraldApplicationTransitionContradiction
    )

requireChecked :: Maybe value -> CallPlan value
requireChecked = maybe (Left CallContradiction) Right

mapContradiction :: Either error value -> CallPlan value
mapContradiction = either (const (Left CallContradiction)) Right

mapNewIdPlan :: Either NewIdFailure value -> CallPlan value
mapNewIdPlan = \case
  Left (NewIdRejected rejection) -> Left (CallRejected rejection)
  Left NewIdContradiction -> Left CallContradiction
  Right value -> Right value

filterM :: (value -> Either error Bool) -> [value] -> Either error [value]
filterM action = foldM one []
  where
    one retained value = do
      keep <- action value
      Right (if keep then retained <> [value] else retained)
