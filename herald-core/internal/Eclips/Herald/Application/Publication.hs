{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Package-internal preparation for the sole application publication schema.
-- The public EAPP adapter reaches this owner only through the surrounding Herald
-- transition, which composes value admission, publication identity/retention,
-- ordinary Store application, structural staging, and direct peer-stream enqueue
-- before installing every successor atomically.
module Eclips.Herald.Application.Publication
  ( AdmittedApplicationPublicationValue,
    admittedApplicationPublicationValue,
    admittedApplicationPublicationSortDefinition,
    ApplicationPublicationValueError (..),
    admitApplicationPublicationValue,
    applicationPublicationRequestMatchesRecord,
    ApplicationPublicationAccessError (..),
    ApplicationPublicationRouteError (..),
    resolveApplicationPublicationSource,
    deriveApplicationPublicationRoute,
    applicationPublicationStructuralReferenceControlPrerequisite,
    revalidateFenceHeldForwardValue,
    LocalControlledPublicationOutcome (..),
    LocalApplicationPublicationEffects (..),
    LocalApplicationPublicationOutcome,
    localApplicationPublicationClassification,
    localApplicationPublicationRecord,
    localApplicationPublicationAdmittedValue,
    localApplicationPublicationControlledOutcome,
    localApplicationPublicationEffects,
    localApplicationPublicationReply,
    LocalApplicationPublicationError (..),
    PreparedLocalApplicationPublication,
    prepareLocalApplicationPublication,
    prepareFenceHeldApplicationPublication,
    preparedLocalApplicationPublicationOutcome,
    commitLocalApplicationPublication,
  )
where

import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Eclips.Application.Types.Identity
  ( PrivateNablaId,
    privateNablaUniqueId,
  )
import Eclips.Application.Types.Operation (ApplicationOperation (..))
import Eclips.Application.Types.Result
  ( OperationPendingReason (StructuralStabilizationPending),
    RegularCallResult (WriteCompleted),
  )
import Eclips.Application.Types.Value
  ( ApplicationValue (..),
  )
import Eclips.Application.Types.Value qualified as Application
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (WriteAccepted),
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    ControlIndex,
    GlobalObjectId,
    GlobalUniqueId,
    HeraldEpoch,
    NablaId,
    ProcessEpochId,
    SortDefinitionOccurrenceId,
    SortId,
    controlIndex,
    globalObjectIdFromGlobalUniqueId,
    globalUniqueIdFromGlobalObjectId,
    nablaIdFromGlobalObjectId,
    publicationNabla,
  )
import Eclips.Domain.Label
  ( ReleasedLabelStateView (..),
    releasedLabelStateView,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationValue,
  )
import Eclips.Domain.Route
  ( FrozenRoute,
    ReplicaStrength (Normal),
    RouteError,
    partitionFrozenRoute,
    routePartitionLocal,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalCheckedDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Descriptor
  ( ApplicationMutation (OrdinaryApplicationMutation),
    CheckedDescriptor,
    SortKind (..),
    StructuralCarrierRole (..),
    descriptorApplicationMutation,
    descriptorControlledKeyProjection,
    descriptorImmutable,
    descriptorKind,
    descriptorLabelField,
    descriptorStructuralCarrierRole,
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (EdgeRole, SortDefinitionRole),
    StructuralCarrierReferenceError,
    predefinedCatalogueDescriptor,
    profileEntryFor,
    structuralCarrierSortReferences,
  )
import Eclips.Domain.Value
  ( Value,
    ValueView (..),
    directProjection,
    fieldNameText,
    labelValue,
    mkFieldName,
    recordValueMap,
    valueAt,
    viewValue,
  )
import Eclips.Domain.Value qualified as Domain
import Eclips.Herald.Application.Request (ApplicationRequestReply)
import Eclips.Herald.Application.SortDefinition
  ( ApplicationSortDefinitionRejection,
    admitApplicationSortDefinition,
    admittedApplicationSortDescriptor,
    admittedApplicationSortId,
    admittedApplicationSortValue,
    admittedApplicationSortWriteResult,
  )
import Eclips.Herald.Application.State
  ( ApplicationFenceHeldWork,
    ApplicationPublicationRequestWitness,
    ApplicationRequestCandidate,
    ApplicationValueGlobalizationError,
    ProcessRoleView,
    State,
    applicationFenceHeldWorkAdmittedValue,
    applicationFenceHeldWorkControlPrerequisite,
    applicationFenceHeldWorkOrigin,
    applicationFenceHeldWorkSortId,
    applicationFenceHeldWorkSortOccurrenceId,
    applicationFenceHeldWorkSourceStrength,
    applicationFenceHeldWorkWriter,
    applicationPublicationRequestWitnessPosition,
    applicationPublicationRequestWitnessPrivateNabla,
    applicationPublicationRequestWitnessProcess,
    applicationPublicationRequestWitnessValue,
    applicationRequestCandidateCall,
    applicationRequestCandidatePosition,
    applicationRequestCandidateProcess,
    applicationRequestCandidateState,
    commitApplicationRequest,
    globalizeOrdinaryApplicationValue,
    prepareApplicationOperationAcceptance,
    prepareApplicationRequestCompletion,
    resolveApplicationPrivateUniqueId,
  )
import Eclips.Herald.Controlled.Operate qualified as ControlledOperate
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.PeerPayload
  ( PeerLogicalPayload,
    peerLogicalAssignmentReceipt,
    peerLogicalPublicationItem,
  )
import Eclips.Herald.PeerPublication
  ( PeerPublication,
    PeerPublicationProblem,
    mkOrdinaryPeerPublication,
  )
import Eclips.Herald.PeerStream
  ( PeerDispatchTicket,
    SequencedItem,
    StreamDirection,
    StreamSequence,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.Route
  ( WriterRouteError,
  )
import Eclips.Herald.Publication.Route qualified as PublicationRoute
import Eclips.Herald.Publication.State
  ( AdmittedApplicationPublicationValue,
    ApplicationPublicationClassification (..),
    ApplicationPublicationOrigin (..),
    ApplicationPublicationRecord,
    PublicationPreparationError,
    admittedApplicationPublicationSortDefinition,
    admittedApplicationPublicationValue,
    admittedOrdinaryApplicationPublicationValue,
    admittedSortDefinitionApplicationPublicationValue,
  )
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Store.State qualified as Store

data ApplicationPublicationValueError
  = ApplicationPublicationOrdinaryValueRejected
      ApplicationValueGlobalizationError
  | ApplicationPublicationSortDefinitionRejected
      ApplicationSortDefinitionRejection
  | ApplicationPublicationSortDefinitionCarrierMismatch SortId SortId
  | ApplicationPublicationSortDefinitionCarrierRequiresStructuredValue
      SortId
  deriving stock (Eq, Show)

-- | Admit exactly one top-level application value for an effective writer
-- descriptor.  Structured sort definitions use their dedicated adapter and can
-- only be values of the predefined sort-definition carrier.  Conversely, an
-- ordinary record cannot impersonate that carrier's structured application arm.
admitApplicationPublicationValue ::
  ProcessRoleView ->
  ProcessEpochId ->
  CanonicalDescriptor ->
  ApplicationValue ->
  State ->
  Either ApplicationPublicationValueError AdmittedApplicationPublicationValue
admitApplicationPublicationValue roles process effectiveDescriptor applicationValue applicationState =
  case applicationValue of
    SortDefinitionValue definition -> do
      requireSortDefinitionCarrier effectiveDescriptor
      admitted <-
        either
          (Left . ApplicationPublicationSortDefinitionRejected)
          Right
          (admitApplicationSortDefinition definition)
      Right
        (admittedSortDefinitionApplicationPublicationValue admitted)
    ordinary
      | effectiveDescriptor == sortDefinitionCarrierDescriptor ->
          Left
            ( ApplicationPublicationSortDefinitionCarrierRequiresStructuredValue
                (descriptorSortId effectiveDescriptor)
            )
      | otherwise ->
          admittedOrdinaryApplicationPublicationValue
            <$> either
              (Left . ApplicationPublicationOrdinaryValueRejected)
              Right
              ( globalizeOrdinaryApplicationValue
                  roles
                  process
                  ordinary
                  applicationState
              )

-- | Re-derive the semantic value and writer of one live package-internal raw
-- request and compare them with the immutable retained publication.  This is
-- deliberately a one-way whole-state witness: ending the application session
-- removes the raw request while the self-contained semantic publication
-- remains valid.
--
-- Existing controlled-object updates ignore the application's label field.
-- Both sides are therefore compared with that field masked, rather than using
-- the controlled owner's current label (which may advance after acceptance).
applicationPublicationRequestMatchesRecord ::
  ProcessRoleView ->
  ApplicationPublicationRequestWitness ->
  CanonicalDescriptor ->
  State ->
  ApplicationPublicationRecord ->
  Bool
applicationPublicationRequestMatchesRecord roles witness descriptor applicationState record =
  requestShapeMatches
    && writerMatches
    && valueMatches
  where
    process = applicationPublicationRequestWitnessProcess witness
    retained = Publication.applicationPublicationAdmittedValue record
    origin = Publication.applicationPublicationOrigin record
    checkedDescriptor = canonicalCheckedDescriptor descriptor
    requestShapeMatches =
      process == Publication.applicationPublicationSourceProcess record
        && applicationPublicationRequestWitnessPosition witness
          == Publication.applicationPublicationAcceptancePosition record
    writerMatches =
      case resolveApplicationPrivateUniqueId
        process
        ( privateNablaUniqueId
            (applicationPublicationRequestWitnessPrivateNabla witness)
        )
        applicationState of
        Left _ -> False
        Right writer ->
          nablaIdFromGlobalObjectId (globalObjectIdFromGlobalUniqueId writer)
            == publicationNabla (Publication.applicationPublicationId record)
    valueMatches = case origin of
      RegularApplicationPublicationOrigin ->
        descriptorKind checkedDescriptor == RegularSort
          && directAdmission == Right retained
      ControlledFirstUseApplicationPublicationOrigin generated ->
        descriptorKind checkedDescriptor == ControlledSort
          && directAdmission == Right retained
          && retainedObject == Just (globalObjectIdFromGlobalUniqueId generated)
      ControlledUpdateApplicationPublicationOrigin object ->
        descriptorKind checkedDescriptor == ControlledSort
          && maskedValuesMatch
          && retainedObject == Just object
      ForwardApplicationPublicationOrigin {} -> False
    directAdmission =
      admitApplicationPublicationValue
        roles
        process
        descriptor
        (applicationPublicationRequestWitnessValue witness)
        applicationState
    maskedAdmission =
      either
        (const Nothing)
        ( either (const Nothing) Just
            . maskAdmittedControlledLabel descriptor
        )
        ( admitApplicationPublicationValue
            roles
            process
            descriptor
            ( maskControlledLabel
                descriptor
                (applicationPublicationRequestWitnessValue witness)
            )
            applicationState
        )
    maskedRetained =
      either (const Nothing) Just (maskAdmittedControlledLabel descriptor retained)
    maskedValuesMatch = case (maskedAdmission, maskedRetained) of
      (Just supplied, Just semantic) -> supplied == semantic
      _ -> False
    retainedObject =
      either
        (const Nothing)
        Just
        ( controlledObjectFromValue
            checkedDescriptor
            (admittedApplicationPublicationValue retained)
        )

data ApplicationPublicationAccessError
  = ApplicationPublicationWriterIdentityError ApplicationValueGlobalizationError
  | ApplicationPublicationBootstrapOperateError
      Controlled.ControlledBootstrapOperateError
  | ApplicationPublicationDynamicOperateError
      ControlledOperate.ControlledOperateError
  | ApplicationPublicationEffectiveSortUnavailable SortId
  | ApplicationPublicationWriterSortMismatch SortId SortId
  | ApplicationPublicationWriterOccurrenceMismatch
      SortDefinitionOccurrenceId
      SortDefinitionOccurrenceId
  | ApplicationPublicationMutationUnavailable SortId
  | ApplicationPublicationImmutableUpdate SortId
  | ApplicationPublicationReservationUnavailable GlobalUniqueId
  | ApplicationPublicationReservationBindingMismatch
      GlobalUniqueId
      ProcessEpochId
      NablaId
  | ApplicationPublicationReservationAuthorityMismatch
      GlobalUniqueId
      AuthorityEpoch
      AuthorityEpoch
  | ApplicationPublicationControlledRecordUnavailable GlobalObjectId
  | ApplicationPublicationControlledLabelContradiction GlobalObjectId
  | ApplicationPublicationControlledValueShapeContradiction
  | ApplicationPublicationStructuralReferenceNotPossessed
      ProcessEpochId
      GlobalObjectId
  | ApplicationPublicationStructuralReferenceNotCurrentVertex GlobalObjectId
  | ApplicationPublicationEdgeValueContradiction
  | ApplicationPublicationStructuralCarrierReferenceError
      StructuralCarrierReferenceError
  | ApplicationPublicationSortDefinitionValueContradiction
  deriving stock (Eq, Show)

data ApplicationPublicationRouteError
  = ApplicationPublicationOrdinaryRouteError WriterRouteError
  | ApplicationPublicationStructuralReaderRouteError WriterRouteError
  | ApplicationPublicationStructuralRouteShapeError RouteError
  deriving stock (Eq, Show)

data LocalControlledPublicationOutcome
  = RegularLocalPublication
  | ControlledLocalFirstUse Controlled.ControlledFirstUseResult
  | ControlledLocalUpdate Controlled.ControlledUpdateResult
  deriving stock (Eq, Show)

-- | Effects made available by one package-internal preparation.  The
-- structural constructor has no Store or peer payload by construction, and an
-- exact retry has no newly prepared effects at all.
data LocalApplicationPublicationEffects
  = OrdinaryApplicationPublicationEffects
      (Map HeraldEpoch PeerPublication)
      (Map HeraldEpoch (NonEmpty (SequencedItem PeerLogicalPayload)))
      (Map HeraldEpoch (StreamDirection, StreamSequence))
      [PeerDispatchTicket]
  | UnsequencedStructuralApplicationPublicationEffects
  deriving stock (Eq, Show)

data LocalApplicationPublicationOutcome
  = LocalApplicationPublicationOutcome
      ApplicationPublicationClassification
      ApplicationPublicationRecord
      AdmittedApplicationPublicationValue
      LocalControlledPublicationOutcome
      LocalApplicationPublicationEffects
      ApplicationRequestReply
  deriving stock (Eq, Show)

localApplicationPublicationClassification ::
  LocalApplicationPublicationOutcome ->
  ApplicationPublicationClassification
localApplicationPublicationClassification
  (LocalApplicationPublicationOutcome classification _ _ _ _ _) = classification

localApplicationPublicationRecord ::
  LocalApplicationPublicationOutcome -> ApplicationPublicationRecord
localApplicationPublicationRecord
  (LocalApplicationPublicationOutcome _ record _ _ _ _) = record

-- | Preserve the dedicated structured-sort admission result through the whole
-- composite preparation so the eventual same-transition SortRegistry induction
-- need not re-admit or reconstruct it.
localApplicationPublicationAdmittedValue ::
  LocalApplicationPublicationOutcome -> AdmittedApplicationPublicationValue
localApplicationPublicationAdmittedValue
  (LocalApplicationPublicationOutcome _ _ admitted _ _ _) = admitted

localApplicationPublicationControlledOutcome ::
  LocalApplicationPublicationOutcome -> LocalControlledPublicationOutcome
localApplicationPublicationControlledOutcome
  (LocalApplicationPublicationOutcome _ _ _ controlled _ _) = controlled

localApplicationPublicationEffects ::
  LocalApplicationPublicationOutcome -> LocalApplicationPublicationEffects
localApplicationPublicationEffects
  (LocalApplicationPublicationOutcome _ _ _ _ effects _) = effects

localApplicationPublicationReply ::
  LocalApplicationPublicationOutcome -> ApplicationRequestReply
localApplicationPublicationReply
  (LocalApplicationPublicationOutcome _ _ _ _ _ reply) = reply

data LocalApplicationPublicationError
  = LocalApplicationPublicationValueError ApplicationPublicationValueError
  | LocalApplicationPublicationAccessError ApplicationPublicationAccessError
  | LocalApplicationPublicationOwnerError PublicationPreparationError
  | LocalApplicationPublicationObservationError
      Controlled.ControlledObservationError
  | LocalApplicationPublicationFirstUseError
      Controlled.ControlledFirstUseError
  | LocalApplicationPublicationUpdateError Controlled.ControlledUpdateError
  | LocalApplicationPublicationSortRegistryError SortRegistry.SortInductionError
  | LocalApplicationPublicationRouteError ApplicationPublicationRouteError
  | LocalApplicationPublicationStoreError Store.StoreTransitionError
  | LocalApplicationPublicationPeerPublicationError PeerPublicationProblem
  | LocalApplicationPublicationPeerStreamError PeerStream.PeerStreamProblem
  | LocalApplicationPublicationOwnerEpochMismatch HeraldEpoch HeraldEpoch
  | LocalApplicationPublicationDispositionContradiction
  deriving stock (Eq, Show)

-- | Opaque complete successors for every owner touched by a generic local
-- publication, plus the exact retry/effect result.  No successor is observable
-- unless controlled admission and every applicable ordinary preparation
-- succeed.
data PreparedLocalApplicationPublication
  = PreparedLocalApplicationPublication
      State
      Controlled.State
      SortRegistry.State
      Publication.State
      Store.State
      (PeerStream.State PeerLogicalPayload)
      LocalApplicationPublicationOutcome

-- | Prepare one first-seen sole-schema publication. Exact retries are already
-- classified by the Application owner and reoffer its retained reply without
-- re-entering this semantic path. The new request authenticates the current writer and its
-- effective SortRegistry occurrence, derives its control prerequisite, checks
-- possession capabilities, and prepares every owner successor atomically.
prepareLocalApplicationPublication ::
  CheckedHeraldGenesis ->
  HeraldMembershipGeneration ->
  ProcessRoleView ->
  ApplicationRequestCandidate ->
  PrivateNablaId ->
  Application.ApplicationValue ->
  Controlled.State ->
  SortRegistry.State ->
  Graph.State ->
  GraphProgress.StructuralProgressState ->
  Placement.State ->
  Publication.State ->
  Store.State ->
  PeerStream.State PeerLogicalPayload ->
  Either LocalApplicationPublicationError PreparedLocalApplicationPublication
prepareLocalApplicationPublication
  genesis
  membership
  roles
  candidate
  privateNabla
  applicationValue
  controlledState
  registryState
  graphState
  structuralProgressState
  placementState
  publicationState
  storeState
  peerStreamState = do
    let process = applicationRequestCandidateProcess candidate
        acceptancePosition = applicationRequestCandidatePosition candidate
        applicationState = applicationRequestCandidateState candidate
    require
      ( applicationRequestCandidateCall candidate
          == WriteApplication privateNabla (PublishValue applicationValue)
      )
      LocalApplicationPublicationDispositionContradiction
    resolvedWriter <-
      either
        ( Left
            . LocalApplicationPublicationAccessError
            . ApplicationPublicationWriterIdentityError
        )
        Right
        ( resolveApplicationPrivateUniqueId
            process
            ( privateNablaUniqueId
                privateNabla
            )
            applicationState
        )
    let nabla =
          nablaIdFromGlobalObjectId
            (globalObjectIdFromGlobalUniqueId resolvedWriter)
    operate <-
      either
        (Left . LocalApplicationPublicationAccessError . operateAccessError)
        Right
        ( ControlledOperate.checkControlledOperate
            process
            nabla
            controlledState
            structuralProgressState
        )
    effective <-
      either
        (Left . LocalApplicationPublicationAccessError)
        Right
        ( resolveApplicationPublicationSource
            operate
            registryState
        )
    let authority = ControlledOperate.controlledOperateAuthority operate
        descriptor = SortRegistry.registryEntryDescriptor effective
        occurrence =
          ControlledOperate.controlledOperateOccurrenceId operate
        writerControlPrerequisite =
          GraphProgress.structuralPublicationControlPrerequisite
            (ControlledOperate.controlledOperateControlPrerequisite operate)
            structuralProgressState
        publicationEpoch =
          Publication.publicationWitnessHeraldEpoch
            (Publication.publicationStateWitness publicationState)
        streamEpoch = PeerStream.peerStreamLocalEpoch peerStreamState
    require
      (publicationEpoch == streamEpoch)
      (LocalApplicationPublicationOwnerEpochMismatch publicationEpoch streamEpoch)
    route <-
      either
        (Left . LocalApplicationPublicationRouteError)
        Right
        ( deriveApplicationPublicationRoute
            genesis
            membership
            descriptor
            nabla
            graphState
            placementState
        )
    (admittedWithRetainedLabel, origin) <-
      admitAndClassifyApplicationPublicationValue
        roles
        process
        nabla
        authority
        descriptor
        applicationValue
        applicationState
        controlledState
    inductionPlan <-
      prepareApplicationSortInductionPlan
        genesis
        admittedWithRetainedLabel
        registryState
    referenceControlPrerequisite <-
      either
        (Left . LocalApplicationPublicationAccessError)
        Right
        ( applicationPublicationStructuralReferenceControlPrerequisite
            descriptor
            admittedWithRetainedLabel
            registryState
        )
    let controlPrerequisite =
          max
            writerControlPrerequisite
            ( max
                referenceControlPrerequisite
                ( maybe
                    writerControlPrerequisite
                    SortRegistry.sortInductionPlanControlPrerequisite
                    inductionPlan
                )
            )
    preparedOwner <-
      either
        (Left . LocalApplicationPublicationOwnerError)
        Right
        ( Publication.prepareApplicationPublication
            nabla
            (ControlledOperate.controlledOperateSequencingObject operate)
            authority
            descriptor
            admittedWithRetainedLabel
            origin
            (heraldMembershipGenerationId membership)
            process
            acceptancePosition
            occurrence
            Normal
            (GraphProgress.structuralLastInstalledCutId structuralProgressState)
            controlPrerequisite
            route
            publicationState
        )
    let classification =
          Publication.preparedApplicationPublicationClassification preparedOwner
        record = Publication.preparedApplicationPublicationRecord preparedOwner
        (publicationSuccessor, _, _) =
          Publication.commitApplicationPublication preparedOwner
        checked = Publication.applicationPublicationChecked record
        preparedRequest =
          case Publication.applicationPublicationUnsequencedStage record of
            Nothing ->
              prepareApplicationRequestCompletion
                candidate
                (WriteCompleted writeResult)
            Just _ ->
              prepareApplicationOperationAcceptance
                candidate
                StructuralStabilizationPending
        (applicationSuccessor, reply) =
          commitApplicationRequest preparedRequest
        writeResult =
          maybe
            WriteAccepted
            admittedApplicationSortWriteResult
            (admittedApplicationPublicationSortDefinition admittedWithRetainedLabel)
    either
      (Left . LocalApplicationPublicationAccessError)
      Right
      ( validateStructuralNamePossession
          process
          descriptor
          checked
          controlledState
      )
    (controlledSuccessor, controlledOutcome) <-
      prepareControlledPublication
        descriptor
        (ControlledOperate.controlledOperateWriterBinding operate)
        origin
        checked
        classification
        controlledState
    registrySuccessor <-
      prepareApplicationSortRegistry
        admittedWithRetainedLabel
        inductionPlan
        checked
        registryState
    finishFirstApplicationPublication
      applicationSuccessor
      publicationEpoch
      route
      controlledSuccessor
      registrySuccessor
      publicationSuccessor
      storeState
      peerStreamState
      descriptor
      classification
      record
      admittedWithRetainedLabel
      controlledOutcome
      reply

-- | Revalidate and interpret one already positioned application publication
-- after its local label fence has released.  The held value is already fully
-- globalized, so this path remains executable after session/process reply
-- retirement.  Authority, effective occurrence, route, released label,
-- possession, liveness, and deletion are all derived again from the current
-- whole-state predecessor before any ordinary owner becomes visible.
prepareFenceHeldApplicationPublication ::
  CheckedHeraldGenesis ->
  HeraldMembershipGeneration ->
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
  Either LocalApplicationPublicationError PreparedLocalApplicationPublication
prepareFenceHeldApplicationPublication
  genesis
  membership
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
      WriteApplication _ (PublishValue _) -> Right ()
      _ -> Left LocalApplicationPublicationDispositionContradiction
    let process = applicationRequestCandidateProcess candidate
        acceptancePosition = applicationRequestCandidatePosition candidate
        writer = applicationFenceHeldWorkWriter held
        retainedOrigin = applicationFenceHeldWorkOrigin held
        retainedAdmitted = applicationFenceHeldWorkAdmittedValue held
        retainedSort = applicationFenceHeldWorkSortId held
        retainedOccurrence = applicationFenceHeldWorkSortOccurrenceId held
        retainedControlPrerequisite =
          applicationFenceHeldWorkControlPrerequisite held
    case retainedOrigin of
      ForwardApplicationPublicationOrigin {} ->
        Left LocalApplicationPublicationDispositionContradiction
      _ -> Right ()
    operate <-
      either
        (Left . LocalApplicationPublicationAccessError . operateAccessError)
        Right
        ( ControlledOperate.checkControlledOperate
            process
            writer
            controlledState
            structuralProgressState
        )
    effective <-
      either
        (Left . LocalApplicationPublicationAccessError)
        Right
        (resolveApplicationPublicationSource operate registryState)
    let authority = ControlledOperate.controlledOperateAuthority operate
        descriptor = SortRegistry.registryEntryDescriptor effective
        effectiveSort = SortRegistry.registryEntrySortId effective
        occurrence = ControlledOperate.controlledOperateOccurrenceId operate
        writerControlPrerequisite =
          GraphProgress.structuralPublicationControlPrerequisite
            (ControlledOperate.controlledOperateControlPrerequisite operate)
            structuralProgressState
        publicationEpoch =
          Publication.publicationWitnessHeraldEpoch
            (Publication.publicationStateWitness publicationState)
        streamEpoch = PeerStream.peerStreamLocalEpoch peerStreamState
    require
      (retainedSort == effectiveSort)
      ( LocalApplicationPublicationAccessError
          (ApplicationPublicationWriterSortMismatch retainedSort effectiveSort)
      )
    require
      (retainedOccurrence == occurrence)
      ( LocalApplicationPublicationAccessError
          ( ApplicationPublicationWriterOccurrenceMismatch
              retainedOccurrence
              occurrence
          )
      )
    require
      (applicationFenceHeldWorkSourceStrength held == Normal)
      LocalApplicationPublicationDispositionContradiction
    require
      (publicationEpoch == streamEpoch)
      (LocalApplicationPublicationOwnerEpochMismatch publicationEpoch streamEpoch)
    admitted <-
      revalidateHeldApplicationPublicationValue
        descriptor
        retainedOrigin
        retainedAdmitted
        controlledState
    inductionPlan <-
      prepareApplicationSortInductionPlan genesis admitted registryState
    referenceControlPrerequisite <-
      either
        (Left . LocalApplicationPublicationAccessError)
        Right
        ( applicationPublicationStructuralReferenceControlPrerequisite
            descriptor
            admitted
            registryState
        )
    let controlPrerequisite =
          max
            retainedControlPrerequisite
            ( max
                writerControlPrerequisite
                ( max
                    referenceControlPrerequisite
                    ( maybe
                        writerControlPrerequisite
                        SortRegistry.sortInductionPlanControlPrerequisite
                        inductionPlan
                    )
                )
            )
    route <-
      either
        (Left . LocalApplicationPublicationRouteError)
        Right
        ( deriveApplicationPublicationRoute
            genesis
            membership
            descriptor
            writer
            graphState
            placementState
        )
    preparedOwner <-
      either
        (Left . LocalApplicationPublicationOwnerError)
        Right
        ( Publication.prepareApplicationPublication
            writer
            (ControlledOperate.controlledOperateSequencingObject operate)
            authority
            descriptor
            admitted
            retainedOrigin
            (heraldMembershipGenerationId membership)
            process
            acceptancePosition
            occurrence
            Normal
            (GraphProgress.structuralLastInstalledCutId structuralProgressState)
            controlPrerequisite
            route
            publicationState
        )
    let classification =
          Publication.preparedApplicationPublicationClassification preparedOwner
        record = Publication.preparedApplicationPublicationRecord preparedOwner
        (publicationSuccessor, _, _) =
          Publication.commitApplicationPublication preparedOwner
        checked = Publication.applicationPublicationChecked record
        preparedRequest =
          case Publication.applicationPublicationUnsequencedStage record of
            Nothing ->
              prepareApplicationRequestCompletion
                candidate
                (WriteCompleted writeResult)
            Just _ ->
              prepareApplicationOperationAcceptance
                candidate
                StructuralStabilizationPending
        (applicationSuccessor, reply) =
          commitApplicationRequest preparedRequest
        writeResult =
          maybe
            WriteAccepted
            admittedApplicationSortWriteResult
            (admittedApplicationPublicationSortDefinition admitted)
    either
      (Left . LocalApplicationPublicationAccessError)
      Right
      (validateStructuralNamePossession process descriptor checked controlledState)
    (controlledSuccessor, controlledOutcome) <-
      prepareControlledPublication
        descriptor
        (ControlledOperate.controlledOperateWriterBinding operate)
        retainedOrigin
        checked
        classification
        controlledState
    registrySuccessor <-
      prepareApplicationSortRegistry admitted inductionPlan checked registryState
    finishFirstApplicationPublication
      applicationSuccessor
      publicationEpoch
      route
      controlledSuccessor
      registrySuccessor
      publicationSuccessor
      storeState
      peerStreamState
      descriptor
      classification
      record
      admitted
      controlledOutcome
      reply

revalidateHeldApplicationPublicationValue ::
  CanonicalDescriptor ->
  ApplicationPublicationOrigin ->
  AdmittedApplicationPublicationValue ->
  Controlled.State ->
  Either LocalApplicationPublicationError AdmittedApplicationPublicationValue
revalidateHeldApplicationPublicationValue descriptor origin admitted controlledState =
  case origin of
    RegularApplicationPublicationOrigin -> do
      requireKind RegularSort
      Right admitted
    ControlledFirstUseApplicationPublicationOrigin _ -> do
      requireKind ControlledSort
      Right admitted
    ControlledUpdateApplicationPublicationOrigin object -> do
      requireKind ControlledSort
      masked <-
        either
          (Left . LocalApplicationPublicationAccessError)
          Right
          (maskAdmittedControlledLabel descriptor admitted)
      either
        (Left . LocalApplicationPublicationAccessError)
        Right
        (overlayControlledUpdateLabel descriptor object masked controlledState)
    ForwardApplicationPublicationOrigin {} ->
      Left LocalApplicationPublicationDispositionContradiction
  where
    requireKind expected =
      require
        (descriptorKind (canonicalCheckedDescriptor descriptor) == expected)
        LocalApplicationPublicationDispositionContradiction

-- | Apply the current released label to an acceptance-time forward payload.
-- The object/value identity remains the one captured by the semantic hold;
-- only the authoritative controlled label is refreshed before publication.
revalidateFenceHeldForwardValue ::
  CanonicalDescriptor ->
  GlobalObjectId ->
  AdmittedApplicationPublicationValue ->
  Controlled.State ->
  Either ApplicationPublicationAccessError AdmittedApplicationPublicationValue
revalidateFenceHeldForwardValue descriptor object admitted controlledState = do
  require
    (descriptorKind (canonicalCheckedDescriptor descriptor) == ControlledSort)
    ApplicationPublicationControlledValueShapeContradiction
  masked <- maskAdmittedControlledLabel descriptor admitted
  overlayControlledUpdateLabel descriptor object masked controlledState

preparedLocalApplicationPublicationOutcome ::
  PreparedLocalApplicationPublication -> LocalApplicationPublicationOutcome
preparedLocalApplicationPublicationOutcome
  (PreparedLocalApplicationPublication _ _ _ _ _ _ outcome) = outcome

commitLocalApplicationPublication ::
  PreparedLocalApplicationPublication ->
  ( State,
    Controlled.State,
    SortRegistry.State,
    Publication.State,
    Store.State,
    PeerStream.State PeerLogicalPayload,
    LocalApplicationPublicationOutcome
  )
commitLocalApplicationPublication
  ( PreparedLocalApplicationPublication
      applicationState
      controlledState
      registryState
      publicationState
      storeState
      peerStreamState
      outcome
    ) =
    ( applicationState,
      controlledState,
      registryState,
      publicationState,
      storeState,
      peerStreamState,
      outcome
    )

resolveApplicationPublicationSource ::
  ControlledOperate.CheckedControlledOperate ->
  SortRegistry.State ->
  Either
    ApplicationPublicationAccessError
    SortRegistry.RegistryEntry
resolveApplicationPublicationSource operate registryState = do
  effective <-
    maybe
      ( Left
          ( ApplicationPublicationEffectiveSortUnavailable
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
    ( ApplicationPublicationWriterSortMismatch
        (ControlledOperate.controlledOperateSortId operate)
        (SortRegistry.registryEntrySortId effective)
    )
  require
    ( ControlledOperate.controlledOperateOccurrenceId operate
        == SortRegistry.registryEntryOccurrenceId effective
    )
    ( ApplicationPublicationWriterOccurrenceMismatch
        (ControlledOperate.controlledOperateOccurrenceId operate)
        (SortRegistry.registryEntryOccurrenceId effective)
    )
  require
    ( descriptorApplicationMutation
        (canonicalCheckedDescriptor (SortRegistry.registryEntryDescriptor effective))
        == OrdinaryApplicationMutation
    )
    (ApplicationPublicationMutationUnavailable (SortRegistry.registryEntrySortId effective))
  Right effective

operateAccessError ::
  ControlledOperate.ControlledOperateError ->
  ApplicationPublicationAccessError
operateAccessError = \case
  ControlledOperate.ControlledOperateBootstrapError problem ->
    ApplicationPublicationBootstrapOperateError problem
  problem -> ApplicationPublicationDynamicOperateError problem

deriveApplicationPublicationRoute ::
  CheckedHeraldGenesis ->
  HeraldMembershipGeneration ->
  CanonicalDescriptor ->
  NablaId ->
  Graph.State ->
  Placement.State ->
  Either ApplicationPublicationRouteError FrozenRoute
deriveApplicationPublicationRoute genesis membership descriptor nabla graph placement =
  case descriptorStructuralCarrierRole (canonicalCheckedDescriptor descriptor) of
    Nothing ->
      do
        readerRoute <-
          either
            (Left . ApplicationPublicationOrdinaryRouteError)
            Right
            ( PublicationRoute.freezeWriterRoute
                nabla
                (descriptorSortId descriptor)
                graph
                placement
            )
        if descriptor == sortDefinitionCarrierDescriptor
          then
            either
              (Left . ApplicationPublicationStructuralRouteShapeError)
              Right
              (PublicationRoute.extendPredefinedSystemViews system membership SortDefinitionRole readerRoute)
          else Right readerRoute
    Just role ->
      do
        readerRoute <-
          either
            (Left . ApplicationPublicationStructuralReaderRouteError)
            Right
            ( PublicationRoute.freezeWriterRoute
                nabla
                (descriptorSortId descriptor)
                graph
                placement
            )
        either
          (Left . ApplicationPublicationStructuralRouteShapeError)
          Right
          (PublicationRoute.extendStructuralSystemViews system membership role readerRoute)
  where
    system = checkedSystemId genesis

admitAndClassifyApplicationPublicationValue ::
  ProcessRoleView ->
  ProcessEpochId ->
  NablaId ->
  AuthorityEpoch ->
  CanonicalDescriptor ->
  ApplicationValue ->
  State ->
  Controlled.State ->
  Either
    LocalApplicationPublicationError
    (AdmittedApplicationPublicationValue, ApplicationPublicationOrigin)
admitAndClassifyApplicationPublicationValue
  roles
  process
  writer
  authority
  descriptor
  applicationValue
  applicationState
  controlledState =
    case descriptorKind checkedDescriptor of
      RegularSort -> do
        admitted <- admit applicationValue
        Right (admitted, RegularApplicationPublicationOrigin)
      ControlledSort -> do
        probe <- admit (maskControlledLabel descriptor applicationValue)
        object <- access (controlledObjectFromValue checkedDescriptor (admittedApplicationPublicationValue probe))
        case Controlled.controlledLocalRecord object controlledState of
          Just _ -> do
            requireAccess
              (not (descriptorImmutable checkedDescriptor))
              (ApplicationPublicationImmutableUpdate (descriptorSortId descriptor))
            admitted <- access (overlayControlledUpdateLabel descriptor object probe controlledState)
            Right
              ( admitted,
                ControlledUpdateApplicationPublicationOrigin object
              )
          Nothing -> do
            let generated = globalUniqueIdFromGlobalObjectId object
            reservation <-
              maybe
                (accessFailure (ApplicationPublicationReservationUnavailable generated))
                Right
                (Controlled.controlledReservationWitness generated controlledState)
            requireAccess
              (Controlled.reservationWitnessPhase reservation == Controlled.Reserved)
              (ApplicationPublicationReservationUnavailable generated)
            requireAccess
              ( Controlled.reservationWitnessProcess reservation == process
                  && Controlled.reservationWitnessNabla reservation == writer
              )
              (ApplicationPublicationReservationBindingMismatch generated process writer)
            requireAccess
              (Controlled.reservationWitnessAuthority reservation == authority)
              ( ApplicationPublicationReservationAuthorityMismatch
                  generated
                  (Controlled.reservationWitnessAuthority reservation)
                  authority
              )
            admitted <- admit applicationValue
            admittedObject <-
              access
                ( controlledObjectFromValue
                    checkedDescriptor
                    (admittedApplicationPublicationValue admitted)
                )
            requireAccess
              (admittedObject == object)
              ApplicationPublicationControlledValueShapeContradiction
            Right
              ( admitted,
                ControlledFirstUseApplicationPublicationOrigin generated
              )
    where
      checkedDescriptor = canonicalCheckedDescriptor descriptor
      admit value =
        either
          (Left . LocalApplicationPublicationValueError)
          Right
          ( admitApplicationPublicationValue
              roles
              process
              descriptor
              value
              applicationState
          )
      access = either (Left . LocalApplicationPublicationAccessError) Right
      accessFailure = Left . LocalApplicationPublicationAccessError
      requireAccess condition problem =
        access (require condition problem)

validateStructuralNamePossession ::
  ProcessEpochId ->
  CanonicalDescriptor ->
  CheckedPublication ->
  Controlled.State ->
  Either ApplicationPublicationAccessError ()
validateStructuralNamePossession process descriptor checked controlledState = do
  references <- checkedStructuralReferences descriptor checked
  traverse_ requirePossession references
  where
    requirePossession object = do
      require
        (Controlled.controlledHasNormalPossession process object controlledState)
        (ApplicationPublicationStructuralReferenceNotPossessed process object)
      require
        ( case Controlled.controlledCurrentStructuralRole object controlledState of
            Just NeutralVertexCarrier -> True
            Just NablaCarrier -> True
            Just DeltaCarrier -> True
            _ -> False
        )
        (ApplicationPublicationStructuralReferenceNotCurrentVertex object)

-- | Decode reference-bearing values only after the exact predefined descriptor
-- has admitted them as a 'CheckedPublication'.  Missing/wrong fields are an
-- explicit contradiction; they can no longer silently erase a required name
-- capability check.
checkedStructuralReferences ::
  CanonicalDescriptor ->
  CheckedPublication ->
  Either ApplicationPublicationAccessError [GlobalObjectId]
checkedStructuralReferences descriptor checked =
  case descriptorStructuralCarrierRole (canonicalCheckedDescriptor descriptor) of
    Just EdgeCarrier -> do
      require
        (descriptor == predefinedCatalogueDescriptor (profileEntryFor EdgeRole))
        ApplicationPublicationEdgeValueContradiction
      traverse
        checkedEdgeEndpoint
        ["source_vertex", "destination_vertex"]
    _ -> Right []
  where
    checkedEdgeEndpoint name = do
      field <-
        either
          (const (Left ApplicationPublicationEdgeValueContradiction))
          Right
          (mkFieldName name)
      projected <-
        either
          (const (Left ApplicationPublicationEdgeValueContradiction))
          Right
          ( valueAt
              (directProjection field)
              (checkedPublicationValue checked)
          )
      case viewValue projected of
        GlobalUniqueIdValue identifier ->
          Right (globalObjectIdFromGlobalUniqueId identifier)
        _ -> Left ApplicationPublicationEdgeValueContradiction

-- | The current retirement epoch of every public sort embedded by a structural
-- root is part of the publication's causal input. Without this floor an old
-- Nabla/Delta value could arrive after Resolve and be rebound to the successor
-- occurrence solely because its payload carries the stable SortId.
applicationPublicationStructuralReferenceControlPrerequisite ::
  CanonicalDescriptor ->
  AdmittedApplicationPublicationValue ->
  SortRegistry.State ->
  Either ApplicationPublicationAccessError ControlIndex
applicationPublicationStructuralReferenceControlPrerequisite descriptor admitted registryState = do
  references <-
    either
      (Left . ApplicationPublicationStructuralCarrierReferenceError)
      Right
      ( structuralCarrierSortReferences
          descriptor
          (admittedApplicationPublicationValue admitted)
      )
  Right
    ( foldl
        max
        (controlIndex 0)
        ( fmap
            (`SortRegistry.regularSortReferenceControlPrerequisite` registryState)
            references
        )
    )

prepareApplicationSortInductionPlan ::
  CheckedHeraldGenesis ->
  AdmittedApplicationPublicationValue ->
  SortRegistry.State ->
  Either
    LocalApplicationPublicationError
    (Maybe SortRegistry.SortInductionPlan)
prepareApplicationSortInductionPlan genesis admitted registryState =
  case admittedApplicationPublicationSortDefinition admitted of
    Nothing -> Right Nothing
    Just definition -> do
      require
        ( admittedApplicationPublicationValue admitted
            == admittedApplicationSortValue definition
        )
        ( LocalApplicationPublicationAccessError
            ApplicationPublicationSortDefinitionValueContradiction
        )
      Just
        <$> either
          (Left . LocalApplicationPublicationSortRegistryError)
          Right
          ( SortRegistry.planSortInduction
              (checkedSystemId genesis)
              (admittedApplicationSortDescriptor definition)
              registryState
          )

prepareApplicationSortRegistry ::
  AdmittedApplicationPublicationValue ->
  Maybe SortRegistry.SortInductionPlan ->
  CheckedPublication ->
  SortRegistry.State ->
  Either LocalApplicationPublicationError SortRegistry.State
prepareApplicationSortRegistry admitted inductionPlan checked registryState =
  case (admittedApplicationPublicationSortDefinition admitted, inductionPlan) of
    (Nothing, Nothing) -> Right registryState
    (Just definition, Just plan) -> do
      require
        ( SortRegistry.sortInductionPlanSortId plan
            == admittedApplicationSortId definition
        )
        LocalApplicationPublicationDispositionContradiction
      prepared <-
        either
          (Left . LocalApplicationPublicationSortRegistryError)
          Right
          ( SortRegistry.prepareSortInduction
              (admittedApplicationSortDescriptor definition)
              (SortRegistry.sortInductionPlanOccurrenceId plan)
              checked
              registryState
          )
      Right (SortRegistry.commitSortInduction prepared)
    _ -> Left LocalApplicationPublicationDispositionContradiction

finishFirstApplicationPublication ::
  State ->
  HeraldEpoch ->
  FrozenRoute ->
  Controlled.State ->
  SortRegistry.State ->
  Publication.State ->
  Store.State ->
  PeerStream.State PeerLogicalPayload ->
  CanonicalDescriptor ->
  ApplicationPublicationClassification ->
  ApplicationPublicationRecord ->
  AdmittedApplicationPublicationValue ->
  LocalControlledPublicationOutcome ->
  ApplicationRequestReply ->
  Either LocalApplicationPublicationError PreparedLocalApplicationPublication
finishFirstApplicationPublication
  applicationState
  publicationEpoch
  route
  controlledState
  registryState
  publicationState
  storeState
  peerStreamState
  descriptor
  classification
  record
  admitted
  controlledOutcome
  reply =
    case Publication.applicationPublicationOutgoingRecord record of
      Just outgoing -> do
        let localRoute =
              routePartitionLocal
                (partitionFrozenRoute publicationEpoch route)
            remoteBatches =
              Publication.outgoingPublicationRemoteBatches outgoing
        remotePeerPublications <-
          either
            (Left . LocalApplicationPublicationPeerPublicationError)
            Right
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
          either
            (Left . LocalApplicationPublicationStoreError)
            Right
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
          either
            (Left . LocalApplicationPublicationPeerStreamError)
            Right
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
            (Left . LocalApplicationPublicationOwnerError)
            Right
            ( Publication.certifyOutgoingPublicationDispatch
                (Publication.applicationPublicationId record)
                (fmap (fmap peerLogicalAssignmentReceipt) assignments)
                publicationState
            )
        certifiedRecord <-
          maybe
            (Left LocalApplicationPublicationDispositionContradiction)
            Right
            (Publication.lookupApplicationPublication (Publication.applicationPublicationAcceptancePosition record) certifiedPublication)
        Right
          ( PreparedLocalApplicationPublication
              applicationState
              controlledState
              registryState
              certifiedPublication
              storeSuccessor
              peerStreamSuccessor
              ( LocalApplicationPublicationOutcome
                  classification
                  certifiedRecord
                  admitted
                  controlledOutcome
                  effects
                  reply
              )
          )
      Nothing -> case Publication.applicationPublicationUnsequencedStage record of
        Just _ ->
          Right
            ( PreparedLocalApplicationPublication
                applicationState
                controlledState
                registryState
                publicationState
                storeState
                peerStreamState
                ( LocalApplicationPublicationOutcome
                    classification
                    record
                    admitted
                    controlledOutcome
                    UnsequencedStructuralApplicationPublicationEffects
                    reply
                )
            )
        Nothing -> Left LocalApplicationPublicationDispositionContradiction

-- The supplied label bytes of an existing-object update are semantically
-- ignored.  Replacing the direct application field before recursive
-- globalization also prevents an irrelevant stale private process alias from
-- causing rejection.
maskControlledLabel ::
  CanonicalDescriptor ->
  ApplicationValue ->
  ApplicationValue
maskControlledLabel descriptor applicationValue
  | descriptorKind checkedDescriptor /= ControlledSort = applicationValue
  | Just labelField <- descriptorLabelField checkedDescriptor =
      case applicationValue of
        Application.RecordValue fields
          | Map.member (fieldNameText labelField) fields ->
              Application.RecordValue
                ( Map.insert
                    (fieldNameText labelField)
                    (Application.LabelValue (Application.VoidLabel, 0))
                    fields
                )
        _ -> applicationValue
  | otherwise = applicationValue
  where
    checkedDescriptor = canonicalCheckedDescriptor descriptor

maskAdmittedControlledLabel ::
  CanonicalDescriptor ->
  AdmittedApplicationPublicationValue ->
  Either ApplicationPublicationAccessError AdmittedApplicationPublicationValue
maskAdmittedControlledLabel descriptor admitted
  | descriptorKind checkedDescriptor /= ControlledSort = Right admitted
  | Just labelField <- descriptorLabelField checkedDescriptor =
      case viewValue (admittedApplicationPublicationValue admitted) of
        Domain.RecordValue fields
          | Map.member labelField fields ->
              replaceAdmittedApplicationPublicationValue
                (recordValueMap (Map.insert labelField (labelValue (Domain.VoidLabel, 0)) fields))
                admitted
        _ -> Left ApplicationPublicationControlledValueShapeContradiction
  | otherwise = Right admitted
  where
    checkedDescriptor = canonicalCheckedDescriptor descriptor

overlayControlledUpdateLabel ::
  CanonicalDescriptor ->
  GlobalObjectId ->
  AdmittedApplicationPublicationValue ->
  Controlled.State ->
  Either
    ApplicationPublicationAccessError
    AdmittedApplicationPublicationValue
overlayControlledUpdateLabel descriptor object admitted controlledState
  | descriptorKind checkedDescriptor /= ControlledSort = Right admitted
  | otherwise = do
      admittedObject <- controlledObjectFromValue checkedDescriptor value
      require
        (admittedObject == object)
        ApplicationPublicationControlledValueShapeContradiction
      _ <-
        maybe
          (Left (ApplicationPublicationControlledRecordUnavailable object))
          Right
          (Controlled.controlledLocalRecord object controlledState)
      overlaid <- case descriptorLabelField checkedDescriptor of
        Nothing -> Right value
        Just labelField -> do
          effectiveState <-
            maybe
              (Left (ApplicationPublicationControlledLabelContradiction object))
              Right
              (Controlled.controlledEffectiveLabelState object controlledState)
          effectiveLabel <- case releasedLabelStateView effectiveState of
            ReleasedLabelView retainedLabel -> Right retainedLabel
            ReleasedDeletedView {} ->
              Left (ApplicationPublicationControlledLabelContradiction object)
          case viewValue value of
            Domain.RecordValue fields
              | Map.member labelField fields ->
                  Right
                    ( recordValueMap
                        (Map.insert labelField (labelValue effectiveLabel) fields)
                    )
            _ -> Left ApplicationPublicationControlledValueShapeContradiction
      replaceAdmittedApplicationPublicationValue overlaid admitted
  where
    checkedDescriptor = canonicalCheckedDescriptor descriptor
    value = admittedApplicationPublicationValue admitted

replaceAdmittedApplicationPublicationValue ::
  Value ->
  AdmittedApplicationPublicationValue ->
  Either ApplicationPublicationAccessError AdmittedApplicationPublicationValue
replaceAdmittedApplicationPublicationValue replacement admitted =
  case admittedApplicationPublicationSortDefinition admitted of
    Nothing ->
      Right (admittedOrdinaryApplicationPublicationValue replacement)
    Just admittedDefinition
      | replacement == admittedApplicationSortValue admittedDefinition ->
          Right
            ( admittedSortDefinitionApplicationPublicationValue
                admittedDefinition
            )
      | otherwise ->
          Left ApplicationPublicationSortDefinitionValueContradiction

controlledObjectFromValue ::
  CheckedDescriptor ->
  Value ->
  Either ApplicationPublicationAccessError GlobalObjectId
controlledObjectFromValue descriptor value =
  case descriptorControlledKeyProjection descriptor of
    Nothing -> Left ApplicationPublicationControlledValueShapeContradiction
    Just projection -> case valueAt projection value of
      Right projected -> case viewValue projected of
        GlobalUniqueIdValue identifier ->
          Right (globalObjectIdFromGlobalUniqueId identifier)
        _ -> Left ApplicationPublicationControlledValueShapeContradiction
      Left _ -> Left ApplicationPublicationControlledValueShapeContradiction

prepareControlledPublication ::
  CanonicalDescriptor ->
  Controlled.ControlledWriterBinding ->
  ApplicationPublicationOrigin ->
  CheckedPublication ->
  ApplicationPublicationClassification ->
  Controlled.State ->
  Either
    LocalApplicationPublicationError
    (Controlled.State, LocalControlledPublicationOutcome)
prepareControlledPublication
  descriptor
  binding
  origin
  checked
  classification
  controlledState =
    case descriptorKind (canonicalCheckedDescriptor descriptor) of
      RegularSort -> case origin of
        RegularApplicationPublicationOrigin ->
          Right (controlledState, RegularLocalPublication)
        _ -> Left LocalApplicationPublicationDispositionContradiction
      ControlledSort -> do
        observation <-
          either
            (Left . LocalApplicationPublicationObservationError)
            Right
            ( Controlled.checkControlledObservation
                descriptor
                (Controlled.controlledWriterBindingOccurrenceId binding)
                checked
            )
        case origin of
          ControlledFirstUseApplicationPublicationOrigin target -> do
            prepared <-
              either
                (Left . LocalApplicationPublicationFirstUseError)
                Right
                ( Controlled.prepareControlledFirstUseWithBinding
                    binding
                    target
                    observation
                    controlledState
                )
            let (successor, result) =
                  Controlled.commitControlledFirstUse prepared
            requireControlledAgreement
              (controlledFirstUseAgrees classification result)
            Right (successor, ControlledLocalFirstUse result)
          ControlledUpdateApplicationPublicationOrigin object -> do
            requireControlledAgreement
              (Controlled.checkedControlledObservationObject observation == object)
            prepared <-
              either
                (Left . LocalApplicationPublicationUpdateError)
                Right
                ( Controlled.prepareControlledUpdateWithBinding
                    binding
                    observation
                    controlledState
                )
            let (successor, result) = Controlled.commitControlledUpdate prepared
            requireControlledAgreement
              (controlledUpdateAgrees classification result)
            Right (successor, ControlledLocalUpdate result)
          ForwardApplicationPublicationOrigin {} ->
            Left LocalApplicationPublicationDispositionContradiction
          RegularApplicationPublicationOrigin ->
            Left LocalApplicationPublicationDispositionContradiction

controlledFirstUseAgrees ::
  ApplicationPublicationClassification ->
  Controlled.ControlledFirstUseResult ->
  Bool
controlledFirstUseAgrees classification result = case (classification, result) of
  (ApplicationPublicationFirstAccepted, Controlled.ControlledFirstUseWon) -> True
  (ApplicationPublicationExactRetry, Controlled.ControlledFirstUseRetry) -> True
  _ -> False

controlledUpdateAgrees ::
  ApplicationPublicationClassification ->
  Controlled.ControlledUpdateResult ->
  Bool
controlledUpdateAgrees classification result = case (classification, result) of
  (ApplicationPublicationFirstAccepted, Controlled.ControlledUpdateWinnerAdvanced) -> True
  (ApplicationPublicationFirstAccepted, Controlled.ControlledUpdateWinnerRetained) -> True
  (ApplicationPublicationExactRetry, Controlled.ControlledUpdateRetry) -> True
  _ -> False

requireControlledAgreement ::
  Bool -> Either LocalApplicationPublicationError ()
requireControlledAgreement condition =
  require condition LocalApplicationPublicationDispositionContradiction

requireSortDefinitionCarrier ::
  CanonicalDescriptor -> Either ApplicationPublicationValueError ()
requireSortDefinitionCarrier effectiveDescriptor =
  require
    (effectiveDescriptor == sortDefinitionCarrierDescriptor)
    ( ApplicationPublicationSortDefinitionCarrierMismatch
        (descriptorSortId sortDefinitionCarrierDescriptor)
        (descriptorSortId effectiveDescriptor)
    )

sortDefinitionCarrierDescriptor :: CanonicalDescriptor
sortDefinitionCarrierDescriptor =
  predefinedCatalogueDescriptor (profileEntryFor SortDefinitionRole)

require :: Bool -> problem -> Either problem ()
require condition problem
  | condition = Right ()
  | otherwise = Left problem
