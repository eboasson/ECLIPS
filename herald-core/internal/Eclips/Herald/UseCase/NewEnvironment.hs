{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Atomic public @newenv@ acceptance coordination.
module Eclips.Herald.UseCase.NewEnvironment
  ( NewEnvironmentFailure (..),
    NewEnvironmentOutcome (..),
    ApplicationNewEnvironmentOutcome (..),
    planNewEnvironment,
    planApplicationNewEnvironment,
    expandNewEnvironment,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Eclips.Application.Types.Identity (privateNablaUniqueId)
import Eclips.Application.Types.Rejection
  ( ApplicationRejection (ApplicationOperateNotPermitted, EnvironmentSourcesUnavailable),
  )
import Eclips.Domain.Environment
  ( EnvironmentEdgeDirection (..),
    EnvironmentRootClaimView (..),
    EnvironmentRootSlot,
    environmentConnectedObjectCount,
    environmentHubOrdinal,
    environmentManifestRootCount,
    environmentManifestShapeRoots,
    environmentRootClaimView,
    environmentRootSlotClaim,
    environmentRootSlotStructuralCarrierRole,
    environmentWiringSlots,
    profileEnvironmentManifestShape,
  )
import Eclips.Domain.Graph (EdgeStrength (Preserve), edgeStrengthSymbol)
import Eclips.Domain.Identity
  ( GlobalObjectId,
    GlobalUniqueId,
    NablaId,
    ProcessEpochId,
    globalObjectIdFromGlobalUniqueId,
    nablaIdFromGlobalObjectId,
    sortIdBytes,
  )
import Eclips.Domain.Membership
  ( HeraldMembershipGeneration,
    heraldMembershipGenerationActiveMemberSetDigest,
    heraldMembershipGenerationId,
  )
import Eclips.Domain.Route
  ( FrozenRoute,
    ReplicaStrength (Normal),
    freezeRoute,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    canonicalCheckedDescriptor,
  )
import Eclips.Domain.Sort.Descriptor
  ( StructuralCarrierRole (DeltaCarrier, EdgeCarrier, NablaCarrier, NeutralVertexCarrier),
    descriptorStructuralCarrierRole,
  )
import Eclips.Domain.Sort.Profile
  ( PredefinedSortRole (..),
    allPredefinedSortRoles,
    profileSortFor,
  )
import Eclips.Domain.Value
  ( FieldName,
    LabelOwner (ProcessLabel),
    Value,
    ValueError,
    bytesValue,
    enumValue,
    globalUniqueIdValue,
    labelValue,
    mkFieldName,
    optionalGlobalUniqueIdValue,
    recordValue,
  )
import Eclips.Herald.Application.Environment
  ( EnvironmentManifest,
    EnvironmentManifestKey,
    EnvironmentManifestProblem,
    EnvironmentRootPlan,
    PositionedEnvironmentRoot,
    completedEnvironmentManifest,
    environmentManifestGeneratedIds,
    environmentManifestKey,
    environmentManifestProcess,
    environmentManifestRoots,
    environmentRootPlan,
    environmentRootPlanDescriptor,
    environmentRootPlanGeneratedId,
    environmentRootPlanOccurrenceId,
    environmentRootPlanSlot,
    pendingEnvironmentManifest,
    positionedEnvironmentRootChecked,
    positionedEnvironmentRootPlan,
  )
import Eclips.Herald.Application.Publication qualified as ApplicationPublication
import Eclips.Herald.Application.Request (ApplicationRequestReply, RequestId)
import Eclips.Herald.Application.Session
  ( ApplicationSessionBinding,
    ApplicationSessionId,
  )
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.Controlled.Operate qualified as ControlledOperate
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Genesis.Internal (checkedSystemId)
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.IdGenerator.State qualified as IdGenerator
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Publication.Route qualified as PublicationRoute
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupApplicationState,
    replaceStartupControlledState,
    replaceStartupIdGeneratorState,
    replaceStartupPublicationState,
    startupApplicationState,
    startupControlledState,
    startupGenesis,
    startupIdGeneratorState,
    startupOracleProjectionState,
    startupPublicationState,
    startupSortRegistryState,
    startupStructuralBaseCoordinator,
    startupStructuralProgressState,
  )
import Eclips.Herald.UseCase.Step15StructuralBase qualified as StructuralBase

-- | Coordination failures. Ordinary operate rejection remains distinct from
-- an internal owner contradiction so the EAPP caller can retain it normally.
data NewEnvironmentFailure
  = NewEnvironmentSessionFailure Application.ApplicationSessionTransitionError
  | NewEnvironmentRequestConflict RequestId
  | NewEnvironmentRejected ApplicationRejection
  | NewEnvironmentInvariantFault
  deriving stock (Eq, Show)

data NewEnvironmentOutcome
  = NewEnvironmentDeferred
  | NewEnvironmentAccepted EnvironmentManifest
  | NewEnvironmentRetried EnvironmentManifest
  deriving stock (Eq, Show)

data ApplicationNewEnvironmentOutcome
  = ApplicationNewEnvironmentDeferred
  | ApplicationNewEnvironmentAccepted EnvironmentManifest ApplicationRequestReply
  deriving stock (Eq, Show)

data EnvironmentSource = EnvironmentSource
  { writer :: NablaId,
    sequencingObject :: Maybe GlobalObjectId,
    binding :: Controlled.ControlledWriterBinding,
    descriptor :: CanonicalDescriptor,
    route :: FrozenRoute
  }

-- | Classify, preflight, prepare, and atomically commit the four owners of one
-- package-private @newenv@ acceptance. Exact retry consults only retained
-- provenance; it never regenerates roots or rebuilds routes against a later
-- membership generation.
planApplicationNewEnvironment ::
  Application.ApplicationRequestCandidate ->
  HeraldState ->
  Either NewEnvironmentFailure (HeraldState, ApplicationNewEnvironmentOutcome)
planApplicationNewEnvironment initialCandidate predecessor =
  firstOrDeferred initialCandidate
  where
    firstOrDeferred candidate = do
      readiness <- acceptanceReadiness predecessor
      case readiness of
        EnvironmentAcceptanceDeferred -> do
          prepared <-
            mapInvariant
              (Application.prepareApplicationEnvironmentCandidate candidate)
          let successor =
                replaceStartupApplicationState
                  (Application.commitEnvironmentRequestCandidate prepared)
                  predecessor
          pure (successor, ApplicationNewEnvironmentDeferred)
        EnvironmentAcceptanceReady membership ->
          acceptFirst membership candidate

    acceptFirst membership candidate = do
      let process = Application.applicationRequestCandidateProcess candidate
          progress = startupStructuralProgressState predecessor
      key <- mapInvariant (environmentManifestKeyFor candidate)
      access <- checkedEnvironmentBootstrapAccess process predecessor
      nablaSource <-
        resolveEnvironmentSource process access NablaCarrier predecessor
      deltaSource <-
        resolveEnvironmentSource process access DeltaCarrier predecessor
      requireInvariant (nablaSource.writer /= deltaSource.writer)
      count <-
        mapInvariant
          (IdGenerator.mkPositiveCount (fromIntegral environmentConnectedObjectCount))
      generatedPreparation <-
        mapInvariant
          ( IdGenerator.prepareGeneratedIdRange
              count
              (startupIdGeneratorState predecessor)
          )
      plans <-
        mapInvariant
          ( buildEnvironmentPlans
              process
              progress
              nablaSource
              deltaSource
              (IdGenerator.preparedGlobalUniqueIds generatedPreparation)
          )
      preparedPublication <-
        mapInvariant
          ( Publication.prepareEnvironmentRootRange
              key
              membership
              (IdGenerator.preparedGlobalUniqueIds generatedPreparation)
              plans
              (startupPublicationState predecessor)
          )
      requireInvariant
        ( Publication.preparedEnvironmentRootRangeClassification preparedPublication
            == Publication.EnvironmentRootRangeFirstPositioned
        )
      let manifest = Publication.preparedEnvironmentManifest preparedPublication
      controlledRoots <-
        mapInvariant
          ( traverse
              (checkedControlledEnvironmentRoot process nablaSource deltaSource)
              (environmentManifestRoots manifest)
          )
      preparedControlled <-
        mapInvariant
          ( Controlled.prepareControlledEnvironmentRoots
              process
              controlledRoots
              (startupControlledState predecessor)
          )
      requireInvariant
        ( Controlled.preparedControlledEnvironmentRootsClassification preparedControlled
            == Controlled.ControlledEnvironmentRootsEstablished
        )
      preparedApplication <-
        mapInvariant
          (Application.prepareApplicationEnvironmentAcceptance candidate manifest)
      let (publicationSuccessor, _, committedManifest) =
            Publication.commitEnvironmentRootRange preparedPublication
          (controlledSuccessor, _) =
            Controlled.commitControlledEnvironmentRoots preparedControlled
          successor =
            replaceStartupApplicationState
              (Application.commitEnvironmentRequestAcceptance preparedApplication)
              . replaceStartupControlledState controlledSuccessor
              . replaceStartupPublicationState publicationSuccessor
              . replaceStartupIdGeneratorState
                (IdGenerator.commitGeneratedIdRange generatedPreparation)
              $ predecessor
      requireInvariant (committedManifest == manifest)
      pure
        ( successor,
          ApplicationNewEnvironmentAccepted
            manifest
            (Application.preparedEnvironmentRequestAcceptanceReply preparedApplication)
        )

-- | Package-private direct adapter retained for semantic owner properties. The
-- live EAPP path classifies through the ordinary request owner and calls
-- 'planApplicationNewEnvironment'; this adapter additionally proves the
-- environment range's exact retained retry relation.
planNewEnvironment ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  RequestId ->
  Application.NewEnvironmentCoordinatorInput ->
  HeraldState ->
  Either NewEnvironmentFailure (HeraldState, NewEnvironmentOutcome)
planNewEnvironment session binding request input predecessor = do
  classification <-
    mapSession
      ( Application.classifyEnvironmentRequest
          session
          binding
          request
          input
          (startupApplicationState predecessor)
      )
  case classification of
    Application.ConflictingEnvironmentRequest conflict ->
      Left (NewEnvironmentRequestConflict conflict)
    Application.RetainedEnvironmentRequest retained pending ->
      retainedOutcome retained (pendingEnvironmentManifest pending)
    Application.RetainedCompletedEnvironment retained completed ->
      retainedOutcome retained (completedEnvironmentManifest completed)
    Application.FirstEnvironmentRequest retained -> drive retained
    Application.RetainedEnvironmentCandidate retained -> drive retained
  where
    drive retained = do
      candidate <- mapSession (Application.environmentRequestApplicationCandidate retained)
      (successor, outcome) <- planApplicationNewEnvironment candidate predecessor
      pure
        ( successor,
          case outcome of
            ApplicationNewEnvironmentDeferred -> NewEnvironmentDeferred
            ApplicationNewEnvironmentAccepted manifest _ ->
              NewEnvironmentAccepted manifest
        )

    retainedOutcome retained manifest = do
      key <-
        mapInvariant
          ( environmentManifestKey
              (Application.environmentRequestProcess retained)
              (Application.environmentRequestPosition retained)
          )
      preparedPublication <-
        mapInvariant
          ( Publication.prepareRetainedEnvironmentRootRange
              key
              (startupPublicationState predecessor)
          )
      requireInvariant
        ( Publication.preparedEnvironmentRootRangeClassification preparedPublication
            == Publication.EnvironmentRootRangeExactRetry
            && Publication.preparedEnvironmentManifest preparedPublication == manifest
        )
      pure (predecessor, NewEnvironmentRetried manifest)

data EnvironmentAcceptanceReadiness
  = EnvironmentAcceptanceDeferred
  | EnvironmentAcceptanceReady HeraldMembershipGeneration

acceptanceReadiness ::
  HeraldState -> Either NewEnvironmentFailure EnvironmentAcceptanceReadiness
acceptanceReadiness state = do
  membershipReady <- environmentMembershipReady state membership progress
  if applicationLabelWorkActive (startupApplicationState state) || not membershipReady
    then Right EnvironmentAcceptanceDeferred
    else Right (EnvironmentAcceptanceReady membership)
  where
    membership =
      OracleProjection.oracleViewCurrentHeraldMembership
        (OracleProjection.oracleView (startupOracleProjectionState state))
    progress = startupStructuralProgressState state

-- | The evolving closure proves the checked installed-ancestor/current-target
-- interval while acceptance waits. Its anchor may span several retirements; an
-- unrelated generation mismatch remains a composed-state contradiction.
environmentMembershipReady ::
  HeraldState ->
  HeraldMembershipGeneration ->
  GraphProgress.StructuralProgressState ->
  Either NewEnvironmentFailure Bool
environmentMembershipReady state membership progress
  | oracleGeneration == graphGeneration = do
      requireInvariant (oracleDigest == graphDigest)
      Right True
  | otherwise = do
      coordinator <-
        maybe
          (Left NewEnvironmentInvariantFault)
          Right
          (startupStructuralBaseCoordinator state)
      let predecessorMembership =
            StructuralBase.structuralBasePredecessorMembership coordinator
          successorMembership =
            StructuralBase.structuralBaseSuccessorMembership coordinator
      requireInvariant
        ( heraldMembershipGenerationId predecessorMembership == graphGeneration
            && heraldMembershipGenerationActiveMemberSetDigest predecessorMembership
              == graphDigest
            && successorMembership == membership
        )
      Right False
  where
    oracleGeneration = heraldMembershipGenerationId membership
    oracleDigest = heraldMembershipGenerationActiveMemberSetDigest membership
    graphGeneration = GraphProgress.structuralProgressMembershipGenerationId progress
    graphDigest = GraphProgress.structuralProgressMemberSetDigest progress

applicationLabelWorkActive :: Application.State -> Bool
applicationLabelWorkActive state =
  case Application.applicationGatePhase state of
    Application.ApplicationGateClosed {} -> True
    Application.ApplicationGateOpen {} ->
      Application.applicationHasActiveLabels state

environmentManifestKeyFor ::
  Application.ApplicationRequestCandidate ->
  Either EnvironmentManifestProblem EnvironmentManifestKey
environmentManifestKeyFor request =
  environmentManifestKey
    (Application.applicationRequestCandidateProcess request)
    (Application.applicationRequestCandidatePosition request)

resolveEnvironmentSource ::
  ProcessEpochId ->
  Application.BootstrapAccess ->
  StructuralCarrierRole ->
  HeraldState ->
  Either NewEnvironmentFailure EnvironmentSource
resolveEnvironmentSource process access carrier state = do
  (nablaSource, deltaSource) <-
    maybe
      (Left (NewEnvironmentRejected EnvironmentSourcesUnavailable))
      Right
      (Application.bootstrapAccessEnvironmentSources access)
  let privateSource = case carrier of
        NablaCarrier -> nablaSource
        DeltaCarrier -> deltaSource
        _ -> error "private environment requested a non-root carrier"
  globalSource <-
    mapInvariant
      ( Application.resolveApplicationPrivateUniqueId
          process
          (privateNablaUniqueId privateSource)
          (startupApplicationState state)
      )
  resolveGlobalEnvironmentSource process carrier globalSource state

resolveGlobalEnvironmentSource ::
  ProcessEpochId ->
  StructuralCarrierRole ->
  GlobalUniqueId ->
  HeraldState ->
  Either NewEnvironmentFailure EnvironmentSource
resolveGlobalEnvironmentSource process carrier globalSource state = do
  let object = globalObjectIdFromGlobalUniqueId globalSource
      source = nablaIdFromGlobalObjectId object
  if Controlled.controlledObjectCurrent object (startupControlledState state)
    && Controlled.controlledHasNormalPossession process object (startupControlledState state)
    then pure ()
    else Left (NewEnvironmentRejected ApplicationOperateNotPermitted)
  operate <-
    either
      environmentOperateFailure
      Right
      ( ControlledOperate.checkControlledOperate
          process
          source
          (startupControlledState state)
          (startupStructuralProgressState state)
      )
  effective <-
    mapInvariant
      ( ApplicationPublication.resolveApplicationPublicationSource
          operate
          (startupSortRegistryState state)
      )
  let descriptor = SortRegistry.registryEntryDescriptor effective
  requireInvariant
    ( descriptorStructuralCarrierRole (canonicalCheckedDescriptor descriptor)
        == Just carrier
    )
  -- Existing writers supply authority and publication provenance, not an
  -- application-visible disclosure path. Until completion only Herald system
  -- views receive the private construction; the final step seeds its own stores.
  emptyRoute <- mapInvariant (freezeRoute [])
  route <-
    mapInvariant
      ( PublicationRoute.extendStructuralSystemViews
          (checkedSystemId (startupGenesis state))
          ( OracleProjection.oracleViewCurrentHeraldMembership
              (OracleProjection.oracleView (startupOracleProjectionState state))
          )
          carrier
          emptyRoute
      )
  pure
    EnvironmentSource
      { writer = source,
        sequencingObject = ControlledOperate.controlledOperateSequencingObject operate,
        binding = ControlledOperate.controlledOperateWriterBinding operate,
        descriptor,
        route
      }

-- | Read the exact retained startup sources. Ordinary operation is rechecked
-- independently for both sources at acceptance; no other writers are substituted.
checkedEnvironmentBootstrapAccess ::
  ProcessEpochId ->
  HeraldState ->
  Either NewEnvironmentFailure Application.BootstrapAccess
checkedEnvironmentBootstrapAccess process state = do
  access <-
    maybe
      (Left NewEnvironmentInvariantFault)
      Right
      (Application.applicationBootstrapAccess process (startupApplicationState state))
  _ <- mapInvariant (Application.applicationStartupAccessForBootstrap access)
  Right access

-- | Authority/possession loss is an ordinary admission failure. Shape,
-- occurrence, publication, and owner disagreements remain invariant faults.
environmentOperateFailure ::
  ControlledOperate.ControlledOperateError ->
  Either NewEnvironmentFailure value
environmentOperateFailure = \case
  ControlledOperate.ControlledOperateBootstrapError problem ->
    case problem of
      Controlled.ControlledBootstrapOperateWriterProcessMismatch {} -> rejected
      Controlled.ControlledBootstrapOperateProcessNotPossessed {} -> rejected
      Controlled.ControlledBootstrapOperateWriterNotPossessed {} -> rejected
      _ -> Left NewEnvironmentInvariantFault
  ControlledOperate.ControlledOperateDynamicControllerMismatch {} -> rejected
  ControlledOperate.ControlledOperateDynamicResidenceMismatch -> rejected
  ControlledOperate.ControlledOperateDynamicWriterNotPossessed {} -> rejected
  ControlledOperate.ControlledOperateDynamicRecordNotCurrent {} -> rejected
  ControlledOperate.ControlledOperateDynamicRecordLabelMismatch {} -> rejected
  _ -> Left NewEnvironmentInvariantFault
  where
    rejected = Left (NewEnvironmentRejected ApplicationOperateNotPermitted)

buildEnvironmentPlans ::
  ProcessEpochId ->
  GraphProgress.StructuralProgressState ->
  EnvironmentSource ->
  EnvironmentSource ->
  NonEmpty GlobalUniqueId ->
  Either () (NonEmpty EnvironmentRootPlan)
buildEnvironmentPlans process progress nablaSource deltaSource generated =
  buildPlans
    process
    progress
    nablaSource
    deltaSource
    generated
    (environmentManifestShapeRoots profileEnvironmentManifestShape)
    generated

buildPlans ::
  ProcessEpochId ->
  GraphProgress.StructuralProgressState ->
  EnvironmentSource ->
  EnvironmentSource ->
  NonEmpty GlobalUniqueId ->
  NonEmpty EnvironmentRootSlot ->
  NonEmpty GlobalUniqueId ->
  Either () (NonEmpty EnvironmentRootPlan)
buildPlans process progress nablaSource deltaSource allGenerated slots generated =
  traverse buildOne (NonEmpty.zip slots generated)
  where
    buildOne (slot, generatedId) = do
      value <- mapLeftUnit (environmentRootValue process allGenerated slot generatedId)
      let source = sourceForSlot nablaSource deltaSource slot
          binding = source.binding
      Right
        ( environmentRootPlan
            slot
            generatedId
            source.writer
            source.sequencingObject
            (Controlled.controlledWriterBindingAuthority binding)
            source.descriptor
            value
            (Controlled.controlledWriterBindingOccurrenceId binding)
            Normal
            (GraphProgress.structuralLastInstalledCutId progress)
            ( GraphProgress.structuralPublicationControlPrerequisite
                (Controlled.controlledWriterBindingControlPrerequisite binding)
                progress
            )
            source.route
        )

checkedControlledEnvironmentRoot ::
  ProcessEpochId ->
  EnvironmentSource ->
  EnvironmentSource ->
  PositionedEnvironmentRoot ->
  Either () Controlled.ControlledEnvironmentRoot
checkedControlledEnvironmentRoot process nablaSource deltaSource positioned = do
  let plan = positionedEnvironmentRootPlan positioned
      source = sourceForSlot nablaSource deltaSource (environmentRootPlanSlot plan)
  observation <-
    mapLeftUnit
      ( Controlled.checkControlledObservation
          (environmentRootPlanDescriptor plan)
          (environmentRootPlanOccurrenceId plan)
          (positionedEnvironmentRootChecked positioned)
      )
  mapLeftUnit
    ( Controlled.checkControlledEnvironmentRoot
        process
        (environmentRootPlanSlot plan)
        (environmentRootPlanGeneratedId plan)
        source.binding
        observation
    )

sourceForSlot ::
  EnvironmentSource ->
  EnvironmentSource ->
  EnvironmentRootSlot ->
  EnvironmentSource
sourceForSlot nablaSource deltaSource slot =
  case environmentRootSlotStructuralCarrierRole slot of
    NablaCarrier -> nablaSource
    DeltaCarrier -> deltaSource
    NeutralVertexCarrier -> nablaSource
    EdgeCarrier -> deltaSource
    _ -> error "checked environment slot acquired an unsupported carrier"

environmentRootValue ::
  ProcessEpochId ->
  NonEmpty GlobalUniqueId ->
  EnvironmentRootSlot ->
  GlobalUniqueId ->
  Either ValueError Value
environmentRootValue process generated slot object =
  recordValue
    ( [ (objectIdField, globalUniqueIdValue object),
        (labelField, labelValue (ProcessLabel process, 0))
      ]
        <> fields
    )
  where
    fields = case environmentRootClaimView (environmentRootSlotClaim slot) of
      EnvironmentWriterRootClaimView role _ ->
        [ (sortIdField, bytesValue (sortIdBytes (profileSortFor role))),
          (sequencingObjectField, optionalGlobalUniqueIdValue Nothing)
        ]
      EnvironmentReaderRootClaimView role ->
        [(sortIdField, bytesValue (sortIdBytes (profileSortFor role)))]
      EnvironmentHubRootClaimView -> []
      EnvironmentEdgeRootClaimView role direction ->
        let (writer, reader) = endpointIds generated role
            hub = NonEmpty.toList generated !! fromIntegral environmentHubOrdinal
            (source, destination) = case direction of
              WriterToHub -> (writer, hub)
              HubToReader -> (hub, reader)
              ReaderToHub -> (reader, hub)
         in [ (checkedFieldName "source_vertex", globalUniqueIdValue source),
              (checkedFieldName "destination_vertex", globalUniqueIdValue destination),
              (checkedFieldName "strength", enumValue (edgeStrengthSymbol Preserve))
            ]

endpointIds :: NonEmpty GlobalUniqueId -> PredefinedSortRole -> (GlobalUniqueId, GlobalUniqueId)
endpointIds generated role =
  case lookup role (zip allPredefinedSortRoles (pairs (take (fromIntegral environmentManifestRootCount) (NonEmpty.toList generated)))) of
    Just result -> result
    Nothing -> error "checked environment lacks its endpoint pair"
  where
    pairs (writer : reader : rest) = (writer, reader) : pairs rest
    pairs [] = []
    pairs _ = error "checked environment has an incomplete endpoint pair"

-- | Finish construction with the new environment's own ordinary writers. Their
-- bindings are resolved only after the coordinator installs the endpoint cut.
expandNewEnvironment :: EnvironmentManifest -> HeraldState -> Either NewEnvironmentFailure HeraldState
expandNewEnvironment prior state = do
  let process = environmentManifestProcess prior
      generated = environmentManifestGeneratedIds prior
      membership =
        OracleProjection.oracleViewCurrentHeraldMembership
          (OracleProjection.oracleView (startupOracleProjectionState state))
  vertexSource <- resolveGlobalEnvironmentSource process NeutralVertexCarrier (fst (endpointIds generated NeutralVertexRole)) state
  edgeSource <- resolveGlobalEnvironmentSource process EdgeCarrier (fst (endpointIds generated EdgeRole)) state
  suffix <- maybe (Left NewEnvironmentInvariantFault) Right (NonEmpty.nonEmpty (drop (fromIntegral environmentManifestRootCount) (NonEmpty.toList generated)))
  plans <- mapInvariant (buildPlans process (startupStructuralProgressState state) vertexSource edgeSource generated environmentWiringSlots suffix)
  preparedPublication <- mapInvariant (Publication.prepareEnvironmentWiringRange prior membership plans (startupPublicationState state))
  let manifest = Publication.preparedEnvironmentManifest preparedPublication
  positioned <- maybe (Left NewEnvironmentInvariantFault) Right (NonEmpty.nonEmpty (drop (fromIntegral environmentManifestRootCount) (NonEmpty.toList (environmentManifestRoots manifest))))
  controlled <- mapInvariant (traverse (checkedControlledEnvironmentRoot process vertexSource edgeSource) positioned)
  preparedControlled <- mapInvariant (Controlled.prepareControlledEnvironmentWiring process controlled (startupControlledState state))
  application <- mapInvariant (Application.expandPendingEnvironment manifest (startupApplicationState state))
  let (publication, _, _) = Publication.commitEnvironmentRootRange preparedPublication
      (control, _) = Controlled.commitControlledEnvironmentRoots preparedControlled
  pure (replaceStartupApplicationState application . replaceStartupControlledState control . replaceStartupPublicationState publication $ state)

objectIdField, labelField, sortIdField, sequencingObjectField :: FieldName
objectIdField = checkedFieldName "object_id"
labelField = checkedFieldName "label"
sortIdField = checkedFieldName "sort_id"
sequencingObjectField = checkedFieldName "sequencing_object"

checkedFieldName :: Text -> FieldName
checkedFieldName name =
  case mkFieldName name of
    Right field -> field
    Left _ -> error "invalid closed private-environment field name"

mapInvariant :: Either problem value -> Either NewEnvironmentFailure value
mapInvariant = either (const (Left NewEnvironmentInvariantFault)) Right

mapSession ::
  Either Application.ApplicationSessionTransitionError value ->
  Either NewEnvironmentFailure value
mapSession = either (Left . NewEnvironmentSessionFailure) Right

mapLeftUnit :: Either problem value -> Either () value
mapLeftUnit = either (const (Left ())) Right

requireInvariant :: Bool -> Either NewEnvironmentFailure ()
requireInvariant True = Right ()
requireInvariant False = Left NewEnvironmentInvariantFault
