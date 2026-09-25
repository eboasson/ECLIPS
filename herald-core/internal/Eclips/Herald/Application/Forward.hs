{-# LANGUAGE OverloadedRecordDot #-}

-- | Atomic local preparation of application @forward@.
--
-- A forward copies one retained controlled winner into a fresh publication
-- identity owned by the caller's current nabla.  It does not reinterpret the
-- value as an application update and does not grant new possession: the
-- caller's already effective Weak/Normal possession is fixed as the source
-- strength and attenuates the frozen route.
module Eclips.Herald.Application.Forward
  ( LocalApplicationForwardError (..),
    LocalApplicationForwardOutcome,
    localApplicationForwardRecord,
    localApplicationForwardTarget,
    localApplicationForwardSourceStrength,
    localApplicationForwardEffects,
    localApplicationForwardReply,
    obsoleteForwardRetentionOpen,
    PreparedLocalApplicationForward,
    prepareLocalApplicationForward,
    prepareFenceHeldApplicationForward,
    preparedLocalApplicationForwardOutcome,
    commitLocalApplicationForward,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Eclips.Application.Types.Forward (ForwardResult (ForwardAccepted))
import Eclips.Application.Types.Identity
  ( PrivateNablaId,
    PrivateObjectId,
    privateNablaUniqueId,
    privateObjectUniqueId,
  )
import Eclips.Application.Types.Operation (ApplicationOperation (ForwardApplication))
import Eclips.Application.Types.Result
  ( OperationPendingReason (StructuralStabilizationPending),
    RegularCallResult (ForwardCompleted),
  )
import Eclips.Domain.Identity
  ( GlobalObjectId,
    HeraldEpoch,
    SortDefinitionOccurrenceId,
    SortId,
    globalObjectIdFromGlobalUniqueId,
    nablaIdFromGlobalObjectId,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationLifecycle,
    checkedPublicationValue,
  )
import Eclips.Domain.Route
  ( FrozenRoute,
    ReplicaStrength,
    attenuateFrozenRoute,
    partitionFrozenRoute,
    routePartitionLocal,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalCheckedDescriptor,
  )
import Eclips.Domain.Sort.Descriptor
  ( Lifecycle (Current, Obsolete),
    SortKind (ControlledSort),
    descriptorKind,
    descriptorMinimumRetentionMicros,
  )
import Eclips.Herald.Application.Publication
  ( ApplicationPublicationAccessError,
    ApplicationPublicationRouteError,
    LocalApplicationPublicationEffects (..),
  )
import Eclips.Herald.Application.Publication qualified as ApplicationPublication
import Eclips.Herald.Application.Request.Internal (ApplicationRequestReply)
import Eclips.Herald.Application.State
  ( ApplicationFenceHeldWork,
    ApplicationRequestCandidate,
    ApplicationValueGlobalizationError,
    State,
    applicationFenceHeldWorkAdmittedValue,
    applicationFenceHeldWorkControlPrerequisite,
    applicationFenceHeldWorkOrigin,
    applicationFenceHeldWorkSortId,
    applicationFenceHeldWorkSortOccurrenceId,
    applicationFenceHeldWorkSourceStrength,
    applicationFenceHeldWorkWriter,
    applicationRequestCandidateCall,
    applicationRequestCandidatePosition,
    applicationRequestCandidateProcess,
    applicationRequestCandidateState,
    commitApplicationRequest,
    prepareApplicationOperationAcceptance,
    prepareApplicationRequestCompletion,
    resolveApplicationPrivateUniqueId,
  )
import Eclips.Herald.Controlled.Operate qualified as ControlledOperate
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Genesis.Internal (CheckedHeraldGenesis)
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.PeerPayload
  ( PeerLogicalPayload,
    peerLogicalAssignmentReceipt,
    peerLogicalPublicationItem,
  )
import Eclips.Herald.PeerPublication
  ( PeerPublicationProblem,
    mkOrdinaryPeerPublication,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Time
  ( MonotonicInstant,
    monotonicInstantWord64,
  )

data LocalApplicationForwardError
  = LocalApplicationForwardDispositionContradiction
  | LocalApplicationForwardWriterIdentityError ApplicationValueGlobalizationError
  | LocalApplicationForwardTargetIdentityError ApplicationValueGlobalizationError
  | LocalApplicationForwardAccessError ApplicationPublicationAccessError
  | LocalApplicationForwardTargetUnavailable GlobalObjectId
  | LocalApplicationForwardTargetNotControlled SortId
  | LocalApplicationForwardSortMismatch SortId SortId
  | LocalApplicationForwardOccurrenceMismatch
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | LocalApplicationForwardPossessionUnavailable GlobalObjectId
  | LocalApplicationForwardObservationError Controlled.ControlledObservationError
  | LocalApplicationForwardObsoleteObservationError
      Controlled.ControlledObsoleteObservationTimeError
  | LocalApplicationForwardRetentionExpired
      Controlled.State
      GlobalObjectId
      MonotonicInstant
      Word64
      MonotonicInstant
  | LocalApplicationForwardOwnerError Publication.PublicationPreparationError
  | LocalApplicationForwardControlledError Controlled.ControlledPeerObservationError
  | LocalApplicationForwardRouteError ApplicationPublicationRouteError
  | LocalApplicationForwardStoreError Store.StoreTransitionError
  | LocalApplicationForwardPeerPublicationError PeerPublicationProblem
  | LocalApplicationForwardPeerStreamError PeerStream.PeerStreamProblem
  | LocalApplicationForwardOwnerEpochMismatch HeraldEpoch HeraldEpoch

data LocalApplicationForwardOutcome
  = LocalApplicationForwardOutcome
      Publication.ApplicationPublicationRecord
      CheckedPublication
      ReplicaStrength
      LocalApplicationPublicationEffects
      ApplicationRequestReply

localApplicationForwardRecord ::
  LocalApplicationForwardOutcome -> Publication.ApplicationPublicationRecord
localApplicationForwardRecord
  (LocalApplicationForwardOutcome record _ _ _ _) = record

localApplicationForwardTarget ::
  LocalApplicationForwardOutcome -> CheckedPublication
localApplicationForwardTarget
  (LocalApplicationForwardOutcome _ target _ _ _) = target

localApplicationForwardSourceStrength ::
  LocalApplicationForwardOutcome -> ReplicaStrength
localApplicationForwardSourceStrength
  (LocalApplicationForwardOutcome _ _ strength _ _) = strength

localApplicationForwardEffects ::
  LocalApplicationForwardOutcome -> LocalApplicationPublicationEffects
localApplicationForwardEffects
  (LocalApplicationForwardOutcome _ _ _ effects _) = effects

localApplicationForwardReply ::
  LocalApplicationForwardOutcome -> ApplicationRequestReply
localApplicationForwardReply
  (LocalApplicationForwardOutcome _ _ _ _ reply) = reply

data PreparedLocalApplicationForward
  = PreparedLocalApplicationForward
      State
      Controlled.State
      Publication.State
      Store.State
      (PeerStream.State PeerLogicalPayload)
      LocalApplicationForwardOutcome

prepareLocalApplicationForward ::
  CheckedHeraldGenesis ->
  HeraldMembershipGeneration ->
  MonotonicInstant ->
  ApplicationRequestCandidate ->
  PrivateNablaId ->
  PrivateObjectId ->
  Controlled.State ->
  SortRegistry.State ->
  Graph.State ->
  GraphProgress.StructuralProgressState ->
  Placement.State ->
  Publication.State ->
  Store.State ->
  PeerStream.State PeerLogicalPayload ->
  Either LocalApplicationForwardError PreparedLocalApplicationForward
prepareLocalApplicationForward
  genesis
  membership
  observedAt
  candidate
  privateNabla
  privateObject
  controlledState
  registryState
  graphState
  structuralProgressState
  placementState
  publicationState
  storeState
  peerStreamState = do
    require
      ( applicationRequestCandidateCall candidate
          == ForwardApplication privateNabla privateObject
      )
      LocalApplicationForwardDispositionContradiction
    writerIdentity <-
      mapLeft
        LocalApplicationForwardWriterIdentityError
        ( resolveApplicationPrivateUniqueId
            process
            (privateNablaUniqueId privateNabla)
            applicationState
        )
    let writer =
          nablaIdFromGlobalObjectId
            (globalObjectIdFromGlobalUniqueId writerIdentity)
    targetIdentity <-
      mapLeft
        LocalApplicationForwardTargetIdentityError
        ( resolveApplicationPrivateUniqueId
            process
            (privateObjectUniqueId privateObject)
            applicationState
        )
    let target = globalObjectIdFromGlobalUniqueId targetIdentity
    operate <-
      mapLeft
        (LocalApplicationForwardAccessError . operateAccessError)
        ( ControlledOperate.checkControlledOperate
            process
            writer
            controlledState
            structuralProgressState
        )
    effective <-
      mapLeft
        LocalApplicationForwardAccessError
        ( resolveApplicationForwardSource
            operate
            registryState
        )
    let descriptor = SortRegistry.registryEntryDescriptor effective
        checkedDescriptor = canonicalCheckedDescriptor descriptor
        writerSort = SortRegistry.registryEntrySortId effective
        writerOccurrence = SortRegistry.registryEntryOccurrenceId effective
    require
      (descriptorKind checkedDescriptor == ControlledSort)
      (LocalApplicationForwardTargetNotControlled writerSort)
    targetRecord <-
      maybe
        (Left (LocalApplicationForwardTargetUnavailable target))
        Right
        (Controlled.controlledLocalRecord target controlledState)
    require
      (Controlled.controlledRecordSortId targetRecord == writerSort)
      ( LocalApplicationForwardSortMismatch
          writerSort
          (Controlled.controlledRecordSortId targetRecord)
      )
    require
      (Controlled.controlledRecordOccurrenceId targetRecord == writerOccurrence)
      ( LocalApplicationForwardOccurrenceMismatch
          writerOccurrence
          (Controlled.controlledRecordOccurrenceId targetRecord)
      )
    sourceStrength <-
      maybe
        (Left (LocalApplicationForwardPossessionUnavailable target))
        Right
        (Controlled.controlledEffectivePossessionStrength process target controlledState)
    let retainedTarget = Controlled.controlledRecordLatestPublication targetRecord
    (controlledAfterTime, observedTargetRecord) <-
      prepareObsoleteRetention
        observedAt
        descriptor
        writerOccurrence
        retainedTarget
        controlledState
    admitted <-
      mapLeft
        LocalApplicationForwardAccessError
        ( ApplicationPublication.revalidateFenceHeldForwardValue
            descriptor
            target
            ( Publication.admittedOrdinaryApplicationPublicationValue
                (checkedPublicationValue retainedTarget)
            )
            controlledAfterTime
        )
    rawRoute <-
      mapLeft
        LocalApplicationForwardRouteError
        ( ApplicationPublication.deriveApplicationPublicationRoute
            genesis
            membership
            descriptor
            writer
            graphState
            placementState
        )
    referenceControlPrerequisite <-
      mapLeft
        LocalApplicationForwardAccessError
        ( ApplicationPublication.applicationPublicationStructuralReferenceControlPrerequisite
            descriptor
            admitted
            registryState
        )
    let route = attenuateFrozenRoute sourceStrength rawRoute
        authority = ControlledOperate.controlledOperateAuthority operate
        sourceTopologyPrerequisite =
          GraphProgress.structuralLastInstalledCutId structuralProgressState
        controlPrerequisite =
          max
            referenceControlPrerequisite
            ( GraphProgress.structuralPublicationControlPrerequisite
                (ControlledOperate.controlledOperateControlPrerequisite operate)
                structuralProgressState
            )
        occurrence = ControlledOperate.controlledOperateOccurrenceId operate
        publicationEpoch =
          Publication.publicationWitnessHeraldEpoch
            (Publication.publicationStateWitness publicationState)
        streamEpoch = PeerStream.peerStreamLocalEpoch peerStreamState
    require
      (publicationEpoch == streamEpoch)
      (LocalApplicationForwardOwnerEpochMismatch publicationEpoch streamEpoch)
    preparedOwner <-
      mapLeft
        LocalApplicationForwardOwnerError
        ( Publication.prepareApplicationPublication
            writer
            (ControlledOperate.controlledOperateSequencingObject operate)
            authority
            descriptor
            admitted
            (Publication.ForwardApplicationPublicationOrigin target sourceStrength)
            (heraldMembershipGenerationId membership)
            process
            (applicationRequestCandidatePosition candidate)
            occurrence
            sourceStrength
            sourceTopologyPrerequisite
            controlPrerequisite
            route
            publicationState
        )
    let (publicationSuccessor, _, record) =
          Publication.commitApplicationPublication preparedOwner
        forwarded = Publication.applicationPublicationChecked record
    observation <-
      mapLeft
        LocalApplicationForwardObservationError
        (Controlled.checkControlledObservation descriptor occurrence forwarded)
    require
      (Controlled.checkedControlledObservationObject observation == target)
      LocalApplicationForwardDispositionContradiction
    preparedControlled <-
      mapLeft
        LocalApplicationForwardControlledError
        ( Controlled.prepareControlledPeerObservation
            observation
            controlledAfterTime
        )
    let (controlledSuccessor, _) =
          Controlled.commitControlledPeerObservation preparedControlled
        preparedRequest =
          case Publication.applicationPublicationUnsequencedStage record of
            Nothing ->
              prepareApplicationRequestCompletion
                candidate
                (ForwardCompleted ForwardAccepted)
            Just _ ->
              prepareApplicationOperationAcceptance
                candidate
                StructuralStabilizationPending
        (applicationSuccessor, reply) =
          commitApplicationRequest preparedRequest
    finishForward
      applicationSuccessor
      publicationEpoch
      route
      controlledSuccessor
      publicationSuccessor
      storeState
      peerStreamState
      descriptor
      record
      observedTargetRecord
      sourceStrength
      reply
    where
      process = applicationRequestCandidateProcess candidate
      applicationState = applicationRequestCandidateState candidate

-- | Revalidate an already positioned forward after its label fence releases.
-- The captured admitted value and global writer/target survive reply-owner
-- retirement; the current authority, process liveness, possession, released
-- target state, route, and topology cut do not.  The acceptance-time control
-- prerequisite survives as a lower bound on the rederived release value.
prepareFenceHeldApplicationForward ::
  CheckedHeraldGenesis ->
  HeraldMembershipGeneration ->
  MonotonicInstant ->
  ApplicationRequestCandidate ->
  ApplicationFenceHeldWork ->
  Controlled.State ->
  SortRegistry.State ->
  Graph.State ->
  GraphProgress.StructuralProgressState ->
  Placement.State ->
  Publication.State ->
  Store.State ->
  PeerStream.State PeerLogicalPayload ->
  Either LocalApplicationForwardError PreparedLocalApplicationForward
prepareFenceHeldApplicationForward
  genesis
  membership
  observedAt
  candidate
  held
  controlledState
  registryState
  graphState
  structuralProgressState
  placementState
  publicationState
  storeState
  peerStreamState = do
    case applicationRequestCandidateCall candidate of
      ForwardApplication _ _ -> Right ()
      _ -> Left LocalApplicationForwardDispositionContradiction
    (target, retainedStrength) <-
      case applicationFenceHeldWorkOrigin held of
        Publication.ForwardApplicationPublicationOrigin object strength ->
          Right (object, strength)
        _ -> Left LocalApplicationForwardDispositionContradiction
    let process = applicationRequestCandidateProcess candidate
        writer = applicationFenceHeldWorkWriter held
        admitted = applicationFenceHeldWorkAdmittedValue held
        retainedSort = applicationFenceHeldWorkSortId held
        retainedOccurrence = applicationFenceHeldWorkSortOccurrenceId held
        retainedControlPrerequisite =
          applicationFenceHeldWorkControlPrerequisite held
    if applicationFenceHeldWorkSourceStrength held == retainedStrength
      then Right ()
      else Left LocalApplicationForwardDispositionContradiction
    operate <-
      mapLeft
        (LocalApplicationForwardAccessError . operateAccessError)
        ( ControlledOperate.checkControlledOperate
            process
            writer
            controlledState
            structuralProgressState
        )
    effective <-
      mapLeft
        LocalApplicationForwardAccessError
        (resolveApplicationForwardSource operate registryState)
    let descriptor = SortRegistry.registryEntryDescriptor effective
        checkedDescriptor = canonicalCheckedDescriptor descriptor
        writerSort = SortRegistry.registryEntrySortId effective
        writerOccurrence = SortRegistry.registryEntryOccurrenceId effective
    require
      (descriptorKind checkedDescriptor == ControlledSort)
      (LocalApplicationForwardTargetNotControlled writerSort)
    require
      (retainedSort == writerSort)
      (LocalApplicationForwardSortMismatch retainedSort writerSort)
    require
      (retainedOccurrence == writerOccurrence)
      ( LocalApplicationForwardOccurrenceMismatch
          retainedOccurrence
          writerOccurrence
      )
    targetRecord <-
      maybe
        (Left (LocalApplicationForwardTargetUnavailable target))
        Right
        (Controlled.controlledLocalRecord target controlledState)
    require
      (Controlled.controlledRecordSortId targetRecord == writerSort)
      ( LocalApplicationForwardSortMismatch
          writerSort
          (Controlled.controlledRecordSortId targetRecord)
      )
    require
      (Controlled.controlledRecordOccurrenceId targetRecord == writerOccurrence)
      ( LocalApplicationForwardOccurrenceMismatch
          writerOccurrence
          (Controlled.controlledRecordOccurrenceId targetRecord)
      )
    currentStrength <-
      maybe
        (Left (LocalApplicationForwardPossessionUnavailable target))
        Right
        (Controlled.controlledEffectivePossessionStrength process target controlledState)
    require
      (currentStrength == retainedStrength)
      (LocalApplicationForwardPossessionUnavailable target)
    let retainedTarget = Controlled.controlledRecordLatestPublication targetRecord
    (controlledAfterTime, observedTargetRecord) <-
      prepareObsoleteRetention
        observedAt
        descriptor
        writerOccurrence
        retainedTarget
        controlledState
    rawRoute <-
      mapLeft
        LocalApplicationForwardRouteError
        ( ApplicationPublication.deriveApplicationPublicationRoute
            genesis
            membership
            descriptor
            writer
            graphState
            placementState
        )
    releasedAdmitted <-
      mapLeft
        LocalApplicationForwardAccessError
        ( ApplicationPublication.revalidateFenceHeldForwardValue
            descriptor
            target
            admitted
            controlledAfterTime
        )
    referenceControlPrerequisite <-
      mapLeft
        LocalApplicationForwardAccessError
        ( ApplicationPublication.applicationPublicationStructuralReferenceControlPrerequisite
            descriptor
            releasedAdmitted
            registryState
        )
    let route = attenuateFrozenRoute retainedStrength rawRoute
        authority = ControlledOperate.controlledOperateAuthority operate
        sourceTopologyPrerequisite =
          GraphProgress.structuralLastInstalledCutId structuralProgressState
        controlPrerequisite =
          max
            retainedControlPrerequisite
            ( max
                referenceControlPrerequisite
                ( GraphProgress.structuralPublicationControlPrerequisite
                    (ControlledOperate.controlledOperateControlPrerequisite operate)
                    structuralProgressState
                )
            )
        occurrence = ControlledOperate.controlledOperateOccurrenceId operate
        publicationEpoch =
          Publication.publicationWitnessHeraldEpoch
            (Publication.publicationStateWitness publicationState)
        streamEpoch = PeerStream.peerStreamLocalEpoch peerStreamState
        origin =
          Publication.ForwardApplicationPublicationOrigin target retainedStrength
    require
      (publicationEpoch == streamEpoch)
      (LocalApplicationForwardOwnerEpochMismatch publicationEpoch streamEpoch)
    preparedOwner <-
      mapLeft
        LocalApplicationForwardOwnerError
        ( Publication.prepareApplicationPublication
            writer
            (ControlledOperate.controlledOperateSequencingObject operate)
            authority
            descriptor
            releasedAdmitted
            origin
            (heraldMembershipGenerationId membership)
            process
            (applicationRequestCandidatePosition candidate)
            occurrence
            retainedStrength
            sourceTopologyPrerequisite
            controlPrerequisite
            route
            publicationState
        )
    let (publicationSuccessor, _, record) =
          Publication.commitApplicationPublication preparedOwner
        forwarded = Publication.applicationPublicationChecked record
    observation <-
      mapLeft
        LocalApplicationForwardObservationError
        (Controlled.checkControlledObservation descriptor occurrence forwarded)
    require
      (Controlled.checkedControlledObservationObject observation == target)
      LocalApplicationForwardDispositionContradiction
    preparedControlled <-
      mapLeft
        LocalApplicationForwardControlledError
        ( Controlled.prepareControlledPeerObservation
            observation
            controlledAfterTime
        )
    let (controlledSuccessor, _) =
          Controlled.commitControlledPeerObservation preparedControlled
        preparedRequest =
          case Publication.applicationPublicationUnsequencedStage record of
            Nothing ->
              prepareApplicationRequestCompletion
                candidate
                (ForwardCompleted ForwardAccepted)
            Just _ ->
              prepareApplicationOperationAcceptance
                candidate
                StructuralStabilizationPending
        (applicationSuccessor, reply) =
          commitApplicationRequest preparedRequest
    finishForward
      applicationSuccessor
      publicationEpoch
      route
      controlledSuccessor
      publicationSuccessor
      storeState
      peerStreamState
      descriptor
      record
      observedTargetRecord
      retainedStrength
      reply

preparedLocalApplicationForwardOutcome ::
  PreparedLocalApplicationForward -> LocalApplicationForwardOutcome
preparedLocalApplicationForwardOutcome
  (PreparedLocalApplicationForward _ _ _ _ _ outcome) = outcome

commitLocalApplicationForward ::
  PreparedLocalApplicationForward ->
  ( State,
    Controlled.State,
    Publication.State,
    Store.State,
    PeerStream.State PeerLogicalPayload,
    LocalApplicationForwardOutcome
  )
commitLocalApplicationForward
  ( PreparedLocalApplicationForward
      applicationState
      controlledState
      publicationState
      storeState
      peerStreamState
      outcome
    ) =
    ( applicationState,
      controlledState,
      publicationState,
      storeState,
      peerStreamState,
      outcome
    )

prepareObsoleteRetention ::
  MonotonicInstant ->
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  CheckedPublication ->
  Controlled.State ->
  Either
    LocalApplicationForwardError
    (Controlled.State, Controlled.ControlledLocalRecord)
prepareObsoleteRetention observedAt descriptor occurrence targetPublication state = do
  observation <-
    mapLeft
      LocalApplicationForwardObservationError
      (Controlled.checkControlledObservation descriptor occurrence targetPublication)
  let object = Controlled.checkedControlledObservationObject observation
  case checkedPublicationLifecycle targetPublication of
    Current -> do
      record <-
        maybe
          (Left (LocalApplicationForwardTargetUnavailable object))
          Right
          (Controlled.controlledLocalRecord object state)
      Right (state, record)
    Obsolete -> do
      prepared <-
        mapLeft
          LocalApplicationForwardObsoleteObservationError
          ( Controlled.prepareControlledObsoleteObservationTime
              observedAt
              observation
              state
          )
      let (successor, _) =
            Controlled.commitControlledObsoleteObservationTime prepared
      record <-
        maybe
          (Left (LocalApplicationForwardTargetUnavailable object))
          Right
          (Controlled.controlledLocalRecord object successor)
      firstObserved <-
        maybe
          (Left LocalApplicationForwardDispositionContradiction)
          Right
          (Controlled.controlledRecordObsoleteObservedAt record)
      let retention =
            descriptorMinimumRetentionMicros
              (canonicalCheckedDescriptor descriptor)
      if obsoleteForwardRetentionOpen firstObserved retention observedAt
        then Right (successor, record)
        else
          Left
            ( LocalApplicationForwardRetentionExpired
                successor
                object
                firstObserved
                retention
                observedAt
            )

-- | The exact half-open obsolete-forward authorization interval.  The first
-- observation is admitted; the deadline itself is rejected.  Finite profile
-- runs are assumed far below unsigned-counter exhaustion.
obsoleteForwardRetentionOpen ::
  MonotonicInstant -> Word64 -> MonotonicInstant -> Bool
obsoleteForwardRetentionOpen firstObserved minimumRetention observedAt =
  monotonicInstantWord64 observedAt
    < monotonicInstantWord64 firstObserved + minimumRetention

-- Forward is intentionally available for Herald-managed controlled values such
-- as process epochs.  It authenticates the writer's exact effective sort
-- occurrence but, unlike application write, does not require application
-- mutation authority because it copies the retained canonical state unchanged.
resolveApplicationForwardSource ::
  ControlledOperate.CheckedControlledOperate ->
  SortRegistry.State ->
  Either ApplicationPublicationAccessError SortRegistry.RegistryEntry
resolveApplicationForwardSource operate registryState = do
  effective <-
    maybe
      ( Left
          ( ApplicationPublication.ApplicationPublicationEffectiveSortUnavailable
              (ControlledOperate.controlledOperateSortId operate)
          )
      )
      Right
      ( SortRegistry.lookupEffectiveSort
          (ControlledOperate.controlledOperateSortId operate)
          registryState
      )
  require
    ( ControlledOperate.controlledOperateSortId operate
        == SortRegistry.registryEntrySortId effective
    )
    ( ApplicationPublication.ApplicationPublicationWriterSortMismatch
        (ControlledOperate.controlledOperateSortId operate)
        (SortRegistry.registryEntrySortId effective)
    )
  require
    ( ControlledOperate.controlledOperateOccurrenceId operate
        == SortRegistry.registryEntryOccurrenceId effective
    )
    ( ApplicationPublication.ApplicationPublicationWriterOccurrenceMismatch
        (ControlledOperate.controlledOperateOccurrenceId operate)
        (SortRegistry.registryEntryOccurrenceId effective)
    )
  Right effective

operateAccessError ::
  ControlledOperate.ControlledOperateError ->
  ApplicationPublicationAccessError
operateAccessError = \case
  ControlledOperate.ControlledOperateBootstrapError problem ->
    ApplicationPublication.ApplicationPublicationBootstrapOperateError problem
  problem ->
    ApplicationPublication.ApplicationPublicationDynamicOperateError problem

finishForward ::
  State ->
  HeraldEpoch ->
  FrozenRoute ->
  Controlled.State ->
  Publication.State ->
  Store.State ->
  PeerStream.State PeerLogicalPayload ->
  CanonicalDescriptor ->
  Publication.ApplicationPublicationRecord ->
  Controlled.ControlledLocalRecord ->
  ReplicaStrength ->
  ApplicationRequestReply ->
  Either LocalApplicationForwardError PreparedLocalApplicationForward
finishForward
  applicationState
  publicationEpoch
  route
  controlledState
  publicationState
  storeState
  peerStreamState
  descriptor
  record
  target
  sourceStrength
  reply =
    case Publication.applicationPublicationOutgoingRecord record of
      Just outgoing -> do
        let localRoute =
              routePartitionLocal
                (partitionFrozenRoute publicationEpoch route)
            remoteBatches =
              Publication.outgoingPublicationRemoteBatches outgoing
        remotePeerPublications <-
          mapLeft
            LocalApplicationForwardPeerPublicationError
            ( traverse
                ( mkOrdinaryPeerPublication
                    descriptor
                    (Publication.applicationPublicationSortOccurrenceId record)
                    (Publication.applicationPublicationChecked record)
                )
                remoteBatches
            )
        let peerRequests =
              Map.map
                (\publication -> peerLogicalPublicationItem publication :| [])
                remotePeerPublications
        preparedStore <-
          mapLeft
            LocalApplicationForwardStoreError
            ( Store.prepareStoreApplication
                ( Store.storeObservationEnvelope
                    (Publication.applicationPublicationSourceProcess record)
                    (Publication.applicationPublicationSortOccurrenceId record)
                    (Publication.applicationPublicationSourceTopologyPrerequisite record)
                    (Publication.applicationPublicationControlPrerequisite record)
                    Nothing
                )
                localRoute
                (Publication.applicationPublicationChecked record)
                storeState
            )
        preparedPeerStream <-
          mapLeft
            LocalApplicationForwardPeerStreamError
            (PeerStream.prepareEnqueue peerRequests peerStreamState)
        let storeSuccessor = Store.commitStoreApplication preparedStore
            (peerStreamSuccessor, assignments) =
              PeerStream.commitEnqueue preparedPeerStream
            effects =
              OrdinaryApplicationPublicationEffects
                remotePeerPublications
                assignments
                (PeerStream.preparedEnqueueFrontierAdvances preparedPeerStream)
                (PeerStream.preparedEnqueueDispatchTickets preparedPeerStream)
        certifiedPublication <-
          either
            (Left . LocalApplicationForwardOwnerError)
            Right
            ( Publication.certifyOutgoingPublicationDispatch
                (Publication.applicationPublicationId record)
                (fmap (fmap peerLogicalAssignmentReceipt) assignments)
                publicationState
            )
        certifiedRecord <-
          maybe
            (Left LocalApplicationForwardDispositionContradiction)
            Right
            (Publication.lookupApplicationPublication (Publication.applicationPublicationAcceptancePosition record) certifiedPublication)
        Right
          ( PreparedLocalApplicationForward
              applicationState
              controlledState
              certifiedPublication
              storeSuccessor
              peerStreamSuccessor
              ( LocalApplicationForwardOutcome
                  certifiedRecord
                  (Controlled.controlledRecordLatestPublication target)
                  sourceStrength
                  effects
                  reply
              )
          )
      Nothing -> case Publication.applicationPublicationUnsequencedStage record of
        Just _ ->
          Right
            ( PreparedLocalApplicationForward
                applicationState
                controlledState
                publicationState
                storeState
                peerStreamState
                ( LocalApplicationForwardOutcome
                    record
                    (Controlled.controlledRecordLatestPublication target)
                    sourceStrength
                    UnsequencedStructuralApplicationPublicationEffects
                    reply
                )
            )
        Nothing -> Left LocalApplicationForwardDispositionContradiction

mapLeft :: (left -> mapped) -> Either left right -> Either mapped right
mapLeft convert = either (Left . convert) Right

require :: Bool -> problem -> Either problem ()
require condition problem
  | condition = Right ()
  | otherwise = Left problem
