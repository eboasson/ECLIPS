{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module ForwardProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List (find, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Word (Word64, Word8)
import Eclips.Application.Types.Forward (ForwardResult (ForwardAccepted))
import Eclips.Application.Types.Identity
  ( PrivateNablaId,
    PrivateObjectId,
    asPrivateObjectId,
  )
import Eclips.Application.Types.Operation
  ( ApplicationOperation (ForwardApplication),
  )
import Eclips.Application.Types.Result
  ( OperationPendingReason (StructuralStabilizationPending),
    RegularCallResult (ForwardCompleted),
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, CompareField, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationScalarComparison (ScalarEqual),
    ApplicationScalarLiteral (LiteralBool),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (ApplicationSortDescriptor),
    ApplicationSortKind (ControlledSort, RegularSort),
    ApplicationValueSchema (BoolSchema, LabelSchema, RecordSchema, UniqueIdSchema),
  )
import Eclips.Application.Types.SortDescriptor qualified as SortSyntax
import Eclips.Application.Types.Value qualified as Application
import Eclips.Domain.Graph
  ( EdgeStrength (Preserve, Weaken),
    VertexId (DeltaVertex, NablaVertex),
    edgePayload,
  )
import Eclips.Domain.Identity
  ( ControlIndex,
    DeltaId,
    GlobalObjectId,
    GlobalUniqueId,
    HeraldEpoch,
    NablaId,
    NablaSequencing (UnsequencedNabla),
    ProcessEpochId,
    SortDefinitionOccurrenceId,
    SortId,
    controlIndex,
    genesisAuthorityEpoch,
    globalObjectIdFromGlobalUniqueId,
    globalObjectIdFromNablaId,
    mkBootstrapManifestId,
    mkDeltaId,
    mkGlobalUniqueId,
    mkHeraldEpoch,
    mkNablaId,
    mkProcessEpochId,
    mkProcessId,
    mkStoreIncarnationId,
    nablaSequence,
    processEpochIdBytes,
    publicationId,
    sortIdBytes,
  )
import Eclips.Domain.Membership (genesisHeraldMembershipGeneration)
import Eclips.Domain.Publication
  ( CheckedPublication,
    checkedPublicationSort,
    checkedPublicationValue,
    mkCheckedPublication,
  )
import Eclips.Domain.Route
  ( ReplicaStrength (Normal, Weak),
    destinationStrength,
    routeDestinations,
  )
import Eclips.Domain.Sort.Canonical
  ( CanonicalDescriptor,
    descriptorSortId,
  )
import Eclips.Domain.Sort.Profile (sortDefinitionValue)
import Eclips.Domain.SortOccurrence
  ( SortOccurrenceBase (Genesis),
    deriveSortDefinitionOccurrenceId,
    resolvedRetirementOccurrenceBase,
  )
import Eclips.Domain.Startup
  ( PredefinedSortRole (DeltaRole, NablaRole, NeutralVertexRole, SortDefinitionRole),
    allPredefinedSortRoles,
    mkInitialProjectionDigest,
    primordialReplicaDescriptor,
    primordialReplicaOccurrenceId,
    primordialReplicaPublication,
    primordialReplicaRole,
  )
import Eclips.Domain.Structural (emptyStructuralVersionVector)
import Eclips.Domain.Value qualified as Domain
import Eclips.Herald.Application.Forward
  ( LocalApplicationForwardError (LocalApplicationForwardRetentionExpired, LocalApplicationForwardStoreError),
    PreparedLocalApplicationForward,
    commitLocalApplicationForward,
    localApplicationForwardEffects,
    localApplicationForwardRecord,
    localApplicationForwardReply,
    localApplicationForwardSourceStrength,
    localApplicationForwardTarget,
    obsoleteForwardRetentionOpen,
    prepareFenceHeldApplicationForward,
    prepareLocalApplicationForward,
    preparedLocalApplicationForwardOutcome,
  )
import Eclips.Herald.Application.Publication qualified as ApplicationPublication
import Eclips.Herald.Application.Request.Internal
  ( ApplicationRequestReply (RequestConflict, RetainedRequestReply),
    RequestId,
    RetainedRequestReplyBody (Completed, OperationAccepted),
    requestId,
  )
import Eclips.Herald.Application.Session.Internal
  ( ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionReply (SessionOpened),
    applicationAttachmentForBootstrap,
    clientNonce,
    sessionAcceptanceBinding,
    sessionAcceptanceReply,
  )
import Eclips.Herald.Application.SortDefinition
  ( admitApplicationSortDefinition,
    admittedApplicationSortDescriptor,
  )
import Eclips.Herald.Application.State qualified as ApplicationState
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Genesis.Internal
  ( CheckedHeraldGenesis,
    PrimordialDefinitionReplica,
    checkedActiveHeraldEpochs,
    checkedLocalHeraldEpoch,
    checkedPrimordialReplicas,
    checkedSystemId,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.PeerPayload (PeerLogicalPayload)
import Eclips.Herald.PeerPublication
  ( peerPublicationBatch,
    publicationBatchDestinations,
    publicationDestinationStrength,
  )
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement qualified as PlacementMessage
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.State
  ( ApplicationPublicationClassification (ApplicationPublicationExactRetry),
    ApplicationPublicationOrigin (ForwardApplicationPublicationOrigin),
  )
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
import Eclips.Herald.Time (MonotonicInstant, monotonicInstant)
import GenesisFixtures (fixtureCheckedGenesis, fixtureHeraldMembershipGeneration, fixtureIdentifierBytes)
import PrimordialTestAccess (fixtureEnvironmentEdges, fixtureEnvironmentHub)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "application forward"
    [ testCase "Normal possession preserves the frozen route strengths" (caseOrdinaryAttenuation Normal [Weak, Normal]),
      testCase "Weak possession attenuates every frozen route destination" (caseOrdinaryAttenuation Weak [Weak, Weak]),
      testCase "obsolete authorization is half-open from the earliest top-level observation" caseObsoleteRetention,
      testCase "structural forward stays pending and retains its Forward terminal" caseStructuralPending,
      testCase
        "first and fence-held structural forwards carry their referenced-sort retirement floor"
        caseStructuralReferenceControlFloor,
      testCase "exact retry, request conflict, and late failure are atomic" caseRetryConflictRollback
    ]

caseOrdinaryAttenuation :: ReplicaStrength -> [ReplicaStrength] -> Assertion
caseOrdinaryAttenuation sourceStrength expectedStrengths = do
  fixture <- ordinaryFixture sourceStrength False 20 False
  candidate <- firstForwardCandidate fixture fixture.applicationState fixture.request fixture.privateTarget
  prepared <- checkedForward "ordinary forward" (prepareForwardAt fixture 10 candidate fixture.controlled fixture.publications fixture.stores fixture.stream)
  let outcome = preparedLocalApplicationForwardOutcome prepared
      record = localApplicationForwardRecord outcome
      routeStrengths = sort (fmap destinationStrength (routeDestinations (Publication.applicationPublicationRoute record)))
  assertEqual "exact source possession strength" sourceStrength (localApplicationForwardSourceStrength outcome)
  assertEqual
    "forward origin fixes target and source strength"
    (ForwardApplicationPublicationOrigin fixture.target sourceStrength)
    (Publication.applicationPublicationOrigin record)
  assertEqual "retained target winner" fixture.targetPublication (localApplicationForwardTarget outcome)
  assertEqual "attenuated frozen route" expectedStrengths routeStrengths
  case localApplicationForwardEffects outcome of
    ApplicationPublication.OrdinaryApplicationPublicationEffects peerPublications _ _ _ -> do
      let peerStrengths =
            sort
              [ publicationDestinationStrength destination
              | peerPublication <- Map.elems peerPublications,
                destination <- NonEmpty.toList (publicationBatchDestinations (peerPublicationBatch peerPublication))
              ]
      assertEqual "remote batches retain the attenuated route" expectedStrengths peerStrengths
    other -> assertFailure ("expected ordinary forward effects, got " <> show other)
  assertForwardCompleted fixture.request (localApplicationForwardReply outcome)

caseObsoleteRetention :: Assertion
caseObsoleteRetention = do
  fixture <- ordinaryFixture Normal True 20 False
  firstCandidate <- firstForwardCandidate fixture fixture.applicationState fixture.request fixture.privateTarget
  firstPrepared <- checkedForward "first obsolete forward" (prepareForwardAt fixture 50 firstCandidate fixture.controlled fixture.publications fixture.stores fixture.stream)
  let (applicationFirst, controlledFirst, publicationsFirst, storesFirst, streamFirst, _) =
        commitLocalApplicationForward firstPrepared
  assertEqual "first observation retained" (Just (monotonicInstant 50)) (obsoleteObservedAt fixture.target controlledFirst)

  secondCandidate <- firstForwardCandidate fixture applicationFirst (requestId 2) fixture.privateTarget
  secondPrepared <- checkedForward "second obsolete forward" (prepareForwardAt fixture 69 secondCandidate controlledFirst publicationsFirst storesFirst streamFirst)
  let (applicationSecond, controlledSecond, publicationsSecond, storesSecond, streamSecond, _) =
        commitLocalApplicationForward secondPrepared
  assertEqual "later admissible forward cannot restart retention" (Just (monotonicInstant 50)) (obsoleteObservedAt fixture.target controlledSecond)
  assertBool "first instant is inside the interval" (obsoleteForwardRetentionOpen (monotonicInstant 50) 20 (monotonicInstant 50))
  assertBool "last instant before deadline is inside" (obsoleteForwardRetentionOpen (monotonicInstant 50) 20 (monotonicInstant 69))
  assertBool "deadline is outside" (not (obsoleteForwardRetentionOpen (monotonicInstant 50) 20 (monotonicInstant 70)))

  deadlineCandidate <- firstForwardCandidate fixture applicationSecond (requestId 3) fixture.privateTarget
  case prepareForwardAt fixture 70 deadlineCandidate controlledSecond publicationsSecond storesSecond streamSecond of
    Left (LocalApplicationForwardRetentionExpired controlledAfter object firstObserved retention observedAt) -> do
      assertEqual "rejected object" fixture.target object
      assertEqual "rejection uses earliest observation" (monotonicInstant 50) firstObserved
      assertEqual "retention" 20 retention
      assertEqual "top-level observation" (monotonicInstant 70) observedAt
      assertEqual "rejection successor preserves earliest observation" (Just (monotonicInstant 50)) (obsoleteObservedAt fixture.target controlledAfter)
    Left _ -> assertFailure "unexpected obsolete-forward rejection"
    Right _ -> assertFailure "deadline forward unexpectedly succeeded"
  assertFirstForwardRequest fixture applicationSecond (requestId 3) fixture.privateTarget

caseStructuralPending :: Assertion
caseStructuralPending = do
  fixture <- structuralFixture
  candidate <- firstForwardCandidate fixture fixture.applicationState fixture.request fixture.privateTarget
  prepared <- checkedForward "structural forward" (prepareForwardAt fixture 10 candidate fixture.controlled fixture.publications fixture.stores fixture.stream)
  let (applicationAfter, _, publicationsAfter, storesAfter, streamAfter, outcome) =
        commitLocalApplicationForward prepared
      record = localApplicationForwardRecord outcome
  assertEqual
    "structural origin"
    (ForwardApplicationPublicationOrigin fixture.target Normal)
    (Publication.applicationPublicationOrigin record)
  assertBool "structural stage retained" (Publication.applicationPublicationUnsequencedStage record /= Nothing)
  assertEqual
    "forward-specific terminal accessor"
    (ForwardCompleted ForwardAccepted)
    (Publication.applicationPublicationTerminalResult record)
  assertEqual
    "structural effects are deliberately batch-free"
    ApplicationPublication.UnsequencedStructuralApplicationPublicationEffects
    (localApplicationForwardEffects outcome)
  assertStructuralPending fixture.request (localApplicationForwardReply outcome)
  assertBool "Store unchanged before structural settlement" (fixture.stores == storesAfter)
  assertEqual "PeerStream unchanged before structural sequencing" fixture.stream streamAfter

  completion <-
    checkedIO
      "structural terminal completion"
      ( ApplicationState.prepareApplicationOperationCompletion
          (Publication.applicationPublicationAcceptancePosition record)
          (Publication.applicationPublicationTerminalResult record)
          applicationAfter
      )
  let (applicationCompleted, _) = ApplicationState.commitApplicationOperationCompletion completion
  completedReply <-
    checkedIO
      "completed structural forward lookup"
      (ApplicationState.applicationRequestResult fixture.session fixture.binding fixture.request applicationCompleted)
  assertForwardCompleted fixture.request completedReply
  assertEqual
    "semantic publication remains retained"
    (Just record)
    ( Publication.lookupApplicationPublication
        (Publication.applicationPublicationAcceptancePosition record)
        publicationsAfter
    )

caseStructuralReferenceControlFloor :: Assertion
caseStructuralReferenceControlFloor =
  mapM_ exercise [NablaRole, DeltaRole]
  where
    resolveIndex = controlIndex 11
    exercise role = do
      fixture <- structuralReferenceForwardFixture role resolveIndex
      candidate <-
        firstForwardCandidate
          fixture
          fixture.applicationState
          fixture.request
          fixture.privateTarget
      first <-
        checkedForward
          (show role <> " first structural-reference forward")
          ( prepareForwardAt
              fixture
              10
              candidate
              fixture.controlled
              fixture.publications
              fixture.stores
              fixture.stream
          )
      assertForwardFloor
        (show role <> " first forward")
        resolveIndex
        (localApplicationForwardRecord (preparedLocalApplicationForwardOutcome first))

      let target = fixture.targetPublication
          admitted =
            Publication.admittedOrdinaryApplicationPublicationValue
              (checkedPublicationValue target)
          held =
            ApplicationState.applicationFenceHeldWork
              fixture.writer
              (checkedPublicationSort target)
              (Controlled.controlledRecordOccurrenceId (controlledRecord fixture.target fixture.controlled))
              admitted
              (ForwardApplicationPublicationOrigin fixture.target Normal)
              Normal
              (controlIndex 3)
              (Publication.applicationPublicationGroupMembership (localApplicationForwardRecord (preparedLocalApplicationForwardOutcome first)))
      redriven <-
        checkedForward
          (show role <> " fence-held structural-reference forward")
          ( prepareFenceHeldApplicationForward
              fixtureCheckedGenesis
              fixtureHeraldMembershipGeneration
              (monotonicInstant 10)
              candidate
              held
              fixture.controlled
              fixture.registry
              fixture.graph
              fixture.progress
              fixture.placement
              fixture.publications
              fixture.stores
              fixture.stream
          )
      assertForwardFloor
        (show role <> " fence-held redrive")
        resolveIndex
        (localApplicationForwardRecord (preparedLocalApplicationForwardOutcome redriven))

assertForwardFloor :: String -> ControlIndex -> Publication.ApplicationPublicationRecord -> Assertion
assertForwardFloor context expected record = do
  assertEqual
    (context <> " joins the embedded-sort retirement floor")
    expected
    (Publication.applicationPublicationControlPrerequisite record)
  stage <-
    maybe
      (assertFailure (context <> " did not retain an unsequenced structural stage"))
      pure
      (Publication.applicationPublicationUnsequencedStage record)
  assertEqual
    (context <> " carries the floor into structural sequencing")
    expected
    (Publication.unsequencedStructuralControlPrerequisite stage)

caseRetryConflictRollback :: Assertion
caseRetryConflictRollback = do
  fixture <- ordinaryFixture Normal False 20 False
  candidate <- firstForwardCandidate fixture fixture.applicationState fixture.request fixture.privateTarget
  prepared <- checkedForward "first forward" (prepareForwardAt fixture 10 candidate fixture.controlled fixture.publications fixture.stores fixture.stream)
  let (applicationAfter, _, publicationsAfter, _, _, outcome) = commitLocalApplicationForward prepared
      record = localApplicationForwardRecord outcome
      expectedReply = localApplicationForwardReply outcome
      exactCall = ForwardApplication fixture.privateWriter fixture.privateTarget
  case ApplicationState.classifyApplicationRequest fixture.session fixture.binding fixture.request exactCall applicationAfter of
    Right (ApplicationState.RetainedApplicationRequest reply) ->
      assertEqual "exact request retry reoffers one reply" expectedReply reply
    _ -> assertFailure "exact request retry was not retained"
  retained <-
    checkedIO
      "publication exact retry"
      ( Publication.prepareRetainedApplicationPublication
          (Publication.applicationPublicationAcceptancePosition record)
          publicationsAfter
      )
  let (publicationsRetried, classification, retriedRecord) =
        Publication.commitApplicationPublication retained
  assertEqual "publication retry classification" ApplicationPublicationExactRetry classification
  assertEqual "publication retry record" record retriedRecord
  assertBool "publication retry does not advance its owner" (publicationsAfter == publicationsRetried)
  case ApplicationState.classifyApplicationRequest
    fixture.session
    fixture.binding
    fixture.request
    (ForwardApplication fixture.privateWriter fixture.otherPrivateTarget)
    applicationAfter of
    Right (ApplicationState.ConflictingApplicationRequest reply) ->
      assertEqual "changed target conflicts" (RequestConflict fixture.request) reply
    _ -> assertFailure "changed target did not conflict"

  rollback <- ordinaryFixture Normal False 20 True
  rollbackCandidate <- firstForwardCandidate rollback rollback.applicationState rollback.request rollback.privateTarget
  case prepareForwardAt rollback 10 rollbackCandidate rollback.controlled rollback.publications rollback.stores rollback.stream of
    Left LocalApplicationForwardStoreError {} -> pure ()
    Left _ -> assertFailure "unexpected late rollback failure"
    Right _ -> assertFailure "missing local Store slot unexpectedly accepted"
  assertFirstForwardRequest rollback rollback.applicationState rollback.request rollback.privateTarget
  assertEqual "Publication owner unchanged after failed preparation" [] (Publication.applicationPublicationEntries rollback.publications)
  assertEqual
    "Controlled winner unchanged after failed preparation"
    rollback.targetPublication
    (Controlled.controlledRecordLatestPublication (controlledRecord rollback.target rollback.controlled))
  assertEqual "PeerStream unchanged after failed preparation" (PeerStream.initialState rollback.localHerald) rollback.stream

data ForwardFixture = ForwardFixture
  { localHerald :: HeraldEpoch,
    writer :: NablaId,
    target :: GlobalObjectId,
    targetPublication :: CheckedPublication,
    privateWriter :: PrivateNablaId,
    privateTarget :: PrivateObjectId,
    otherPrivateTarget :: PrivateObjectId,
    session :: ApplicationSessionId,
    binding :: ApplicationSessionBinding,
    request :: RequestId,
    applicationState :: ApplicationState.State,
    controlled :: Controlled.State,
    registry :: SortRegistry.State,
    graph :: Graph.State,
    progress :: GraphProgress.StructuralProgressState,
    placement :: Placement.State,
    publications :: Publication.State,
    stores :: Store.State,
    stream :: PeerStream.State PeerLogicalPayload
  }

ordinaryFixture ::
  ReplicaStrength ->
  Bool ->
  Word64 ->
  Bool ->
  IO ForwardFixture
ordinaryFixture sourceStrength obsolete retention includeUnstoredLocal = do
  let descriptor = ordinaryControlledDescriptor retention
      occurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId fixtureCheckedGenesis)
          (descriptorSortId descriptor)
          Genesis
      registry =
        SortRegistry.commitSortInduction
          ( checkedPure
              "ordinary controlled sort induction"
              ( SortRegistry.prepareSortInduction
                  descriptor
                  occurrence
                  (primordialReplicaPublication (carrierFor SortDefinitionRole))
                  (SortRegistry.initialState fixtureCheckedGenesis)
              )
          )
      localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis
      remoteHerald = fixtureIdentity "forward remote Herald" mkHeraldEpoch 0x81
      writer = fixtureIdentity "forward writer" mkNablaId 0x82
      process = fixtureIdentity "forward process" mkProcessEpochId 0xa1
      controller = globalObjectIdFromNablaId writer
      normalDelta = fixtureIdentity "normal route Delta" mkDeltaId 0x83
      weakDelta = fixtureIdentity "weak route Delta" mkDeltaId 0x84
      possessionDelta = fixtureIdentity "possession Delta" mkDeltaId 0x85
      localDelta = fixtureIdentity "unstored local Delta" mkDeltaId 0x86
      remoteNormalIncarnation = fixtureIdentity "normal route incarnation" mkStoreIncarnationId 0x87
      remoteWeakIncarnation = fixtureIdentity "weak route incarnation" mkStoreIncarnationId 0x88
      localIncarnation = fixtureIdentity "unstored local incarnation" mkStoreIncarnationId 0x89
      vertices =
        [NablaVertex writer, DeltaVertex normalDelta, DeltaVertex weakDelta]
          <> [DeltaVertex localDelta | includeUnstoredLocal]
      edges =
        [ edgePayload (NablaVertex writer) (DeltaVertex normalDelta) Preserve,
          edgePayload (NablaVertex writer) (DeltaVertex weakDelta) Weaken
        ]
          <> [edgePayload (NablaVertex writer) (DeltaVertex localDelta) Preserve | includeUnstoredLocal]
      graph = Graph.graphStateForReachabilityTest vertices edges
      localPlacement =
        if includeUnstoredLocal
          then
            Placement.commitPlacementBootstrap
              ( checkedPure
                  "unstored local placement"
                  ( Placement.preparePlacementBootstrap
                      [ Placement.localPlacement
                          localDelta
                          (descriptorSortId descriptor)
                          occurrence
                          controller
                          process
                          localHerald
                          localIncarnation
                          (controlIndex 3)
                      ]
                      (Placement.initialState localHerald)
                  )
              )
          else Placement.initialState localHerald
      remoteRoutes =
        [ PlacementMessage.applicationDeltaRoute
            normalDelta
            (descriptorSortId descriptor)
            occurrence
            controller
            process
            remoteNormalIncarnation
            (controlIndex 3),
          PlacementMessage.applicationDeltaRoute
            weakDelta
            (descriptorSortId descriptor)
            occurrence
            controller
            process
            remoteWeakIncarnation
            (controlIndex 3)
        ]
      snapshot =
        checkedPure
          "forward remote placement snapshot"
          (PlacementMessage.placementSnapshot remoteHerald PlacementMessage.firstPlacementSequence remoteRoutes)
      (placement, _) =
        Placement.commitRemotePlacement
          ( checkedPure
              "forward remote placement"
              ( Placement.prepareRemotePlacement
                  (PlacementMessage.FullPlacementSnapshot snapshot)
                  localPlacement
              )
          )
  makeFixture
    descriptor
    occurrence
    registry
    NeutralVertexRole
    writer
    possessionDelta
    sourceStrength
    (`targetValue` obsolete)
    graph
    placement

structuralFixture :: IO ForwardFixture
structuralFixture = do
  let registry = SortRegistry.initialState fixtureCheckedGenesis
      entry =
        maybe
          (error "neutral structural sort missing")
          id
          (SortRegistry.lookupPredefinedRole NeutralVertexRole registry)
      descriptor = SortRegistry.registryEntryDescriptor entry
      occurrence = SortRegistry.registryEntryOccurrenceId entry
      writer = fixtureIdentity "structural forward writer" mkNablaId 0x91
      possessionDelta = fixtureIdentity "structural forward possession Delta" mkDeltaId 0x92
      localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis
  makeFixture
    descriptor
    occurrence
    registry
    NeutralVertexRole
    writer
    possessionDelta
    Normal
    structuralTargetValue
    Graph.emptyState
    (Placement.initialState localHerald)

structuralReferenceForwardFixture ::
  PredefinedSortRole -> ControlIndex -> IO ForwardFixture
structuralReferenceForwardFixture role resolveIndex = do
  let (referencedSort, retiredRegistry) =
        retiredReferenceRegistry resolveIndex
      carrier = carrierFor role
      descriptor = primordialReplicaDescriptor carrier
      occurrence = primordialReplicaOccurrenceId carrier
      writer =
        fixtureIdentity
          (show role <> " forward reference writer")
          mkNablaId
          roleByte
      possessionDelta =
        fixtureIdentity
          (show role <> " forward reference possession Delta")
          mkDeltaId
          (roleByte + 1)
      localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis
      initialProgress = testStructuralProgress fixtureCheckedGenesis
      (progressAtResolve, _) =
        GraphProgress.commitStructuralControlProgress
          ( checkedPure
              (show role <> " forward reference control progress")
              ( GraphProgress.prepareStructuralControlProgress
                  resolveIndex
                  initialProgress
              )
          )
  fixture <-
    makeFixture
      descriptor
      occurrence
      retiredRegistry
      role
      writer
      possessionDelta
      Normal
      (structuralReferenceTargetValue role referencedSort)
      Graph.emptyState
      (Placement.initialState localHerald)
  pure fixture {progress = progressAtResolve}
  where
    roleByte = case role of
      NablaRole -> 0x93
      DeltaRole -> 0x95
      _ -> error "structural-reference forward requires NablaRole or DeltaRole"

retiredReferenceRegistry ::
  ControlIndex -> (SortId, SortRegistry.State)
retiredReferenceRegistry resolveIndex =
  (descriptorSortId descriptor, retired)
  where
    descriptor = regularReferenceDescriptor
    initial = SortRegistry.initialState fixtureCheckedGenesis
    plan =
      checkedPure
        "forward reference induction plan"
        ( SortRegistry.planSortInduction
            (checkedSystemId fixtureCheckedGenesis)
            descriptor
            initial
        )
    occurrence = SortRegistry.sortInductionPlanOccurrenceId plan
    definitionCarrier = carrierFor SortDefinitionRole
    definitionPublication =
      checkedPure
        "forward reference definition publication"
        ( mkCheckedPublication
            (primordialReplicaDescriptor definitionCarrier)
            ( publicationId
                (fixtureIdentity "forward reference definition writer" mkNablaId 0x97)
                genesisAuthorityEpoch
                (checkedLocalHeraldEpoch fixtureCheckedGenesis)
                (nablaSequence 1)
            )
            (sortDefinitionValue descriptor)
        )
    defined =
      SortRegistry.commitSortInduction
        ( checkedPure
            "forward reference definition induction"
            ( SortRegistry.prepareSortInduction
                descriptor
                occurrence
                definitionPublication
                initial
            )
        )
    entry =
      maybe
        (error "forward reference definition missing after induction")
        id
        (SortRegistry.lookupEffectiveSort (descriptorSortId descriptor) defined)
    successorOccurrence =
      deriveSortDefinitionOccurrenceId
        (checkedSystemId fixtureCheckedGenesis)
        (descriptorSortId descriptor)
        ( checkedPure
            "forward reference Resolve occurrence"
            (resolvedRetirementOccurrenceBase resolveIndex)
        )
    retired =
      SortRegistry.commitRegularSortRetirement
        ( checkedPure
            "forward reference definition retirement"
            ( SortRegistry.prepareExactRegularSortRetirement
                (checkedSystemId fixtureCheckedGenesis)
                entry
                resolveIndex
                successorOccurrence
                defined
            )
        )

makeFixture ::
  CanonicalDescriptor ->
  SortDefinitionOccurrenceId ->
  SortRegistry.State ->
  PredefinedSortRole ->
  NablaId ->
  DeltaId ->
  ReplicaStrength ->
  (GlobalUniqueId -> Domain.Value) ->
  Graph.State ->
  Placement.State ->
  IO ForwardFixture
makeFixture descriptor occurrence registry role writer possessionDelta sourceStrength makeTargetValue graph placement = do
  let localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis
      process = fixtureIdentity "forward process" mkProcessEpochId 0xa1
      processId = fixtureIdentity "forward process identity" mkProcessId 0xa2
      targetIdentity = fixtureIdentity "forward target" mkGlobalUniqueId 0xa3
      otherTargetIdentity = fixtureIdentity "other forward target" mkGlobalUniqueId 0xa4
      target = globalObjectIdFromGlobalUniqueId targetIdentity
      authority = genesisAuthorityEpoch
      targetSource = fixtureIdentity "forward target publication source" mkNablaId 0xa5
      targetPublication =
        checkedPure
          "forward target publication"
          ( mkCheckedPublication
              descriptor
              (publicationId targetSource authority localHerald (nablaSequence 99))
              (makeTargetValue targetIdentity)
          )
      processRoot = Controlled.processFact processId process localHerald authority
      writerRoot =
        Controlled.writerRootFact
          process
          role
          (descriptorSortId descriptor)
          occurrence
          authority
          (controlIndex 3)
          writer
          UnsequencedNabla
      readerRoot =
        Controlled.readerRootFact
          process
          role
          (descriptorSortId descriptor)
          occurrence
          authority
          (controlIndex 3)
          possessionDelta
      bootstrapped =
        Controlled.commitControlledBootstrap
          ( checkedPure
              "forward controlled bootstrap"
              (Controlled.prepareControlledBootstrap processRoot [writerRoot, readerRoot] Controlled.emptyState)
          )
      observation =
        checkedPure
          "forward target observation"
          (Controlled.checkControlledObservation descriptor occurrence targetPublication)
      (observed, _) =
        Controlled.commitControlledPeerObservation
          ( checkedPure
              "forward target record"
              (Controlled.prepareControlledPeerObservation observation bootstrapped)
          )
      controlled =
        Controlled.commitControlledStoreObservation
          ( checkedPure
              "forward target possession"
              ( Controlled.prepareControlledStoreRead
                  process
                  possessionDelta
                  sourceStrength
                  observation
                  observed
              )
          )
      (applicationBase, privateWriter, session, binding) =
        forwardApplication localHerald process role writer possessionDelta
      (applicationWithTarget, privateTarget) =
        localizeObject process targetIdentity applicationBase
      (applicationState, otherPrivateTarget) =
        localizeObject process otherTargetIdentity applicationWithTarget
  pure
    ForwardFixture
      { localHerald,
        writer,
        target,
        targetPublication,
        privateWriter,
        privateTarget,
        otherPrivateTarget,
        session,
        binding,
        request = requestId 1,
        applicationState,
        controlled,
        registry,
        graph,
        progress = testStructuralProgress fixtureCheckedGenesis,
        placement,
        publications = Publication.initialState localHerald,
        stores = checkedPure "forward initial Store" (Store.initialState fixtureCheckedGenesis),
        stream = PeerStream.initialState localHerald
      }

prepareForwardAt ::
  ForwardFixture ->
  Word64 ->
  ApplicationState.ApplicationRequestCandidate ->
  Controlled.State ->
  Publication.State ->
  Store.State ->
  PeerStream.State PeerLogicalPayload ->
  Either LocalApplicationForwardError PreparedLocalApplicationForward
prepareForwardAt fixture observedAt candidate controlled publications stores stream =
  prepareLocalApplicationForward
    fixtureCheckedGenesis
    fixtureHeraldMembershipGeneration
    (monotonicInstant observedAt)
    candidate
    fixture.privateWriter
    privateTarget
    controlled
    fixture.registry
    fixture.graph
    fixture.progress
    fixture.placement
    publications
    stores
    stream
  where
    privateTarget = case ApplicationState.applicationRequestCandidateCall candidate of
      ForwardApplication _ target -> target
      _ -> fixture.privateTarget

firstForwardCandidate ::
  ForwardFixture ->
  ApplicationState.State ->
  RequestId ->
  PrivateObjectId ->
  IO ApplicationState.ApplicationRequestCandidate
firstForwardCandidate fixture state request privateTarget =
  case ApplicationState.classifyApplicationRequest
    fixture.session
    fixture.binding
    request
    (ForwardApplication fixture.privateWriter privateTarget)
    state of
    Left problem -> assertFailure ("forward request classification failed: " <> show problem)
    Right (ApplicationState.FirstApplicationRequest candidate) -> pure candidate
    Right _ -> assertFailure "expected first forward request"

assertFirstForwardRequest ::
  ForwardFixture ->
  ApplicationState.State ->
  RequestId ->
  PrivateObjectId ->
  Assertion
assertFirstForwardRequest fixture state request privateTarget =
  case ApplicationState.classifyApplicationRequest
    fixture.session
    fixture.binding
    request
    (ForwardApplication fixture.privateWriter privateTarget)
    state of
    Right ApplicationState.FirstApplicationRequest {} -> pure ()
    _ -> assertFailure "forward request was consumed by failed preparation"

assertForwardCompleted :: RequestId -> ApplicationRequestReply -> Assertion
assertForwardCompleted request = \case
  RetainedRequestReply _ (Completed retainedRequest (ForwardCompleted ForwardAccepted)) ->
    assertEqual "completed request identity" request retainedRequest
  reply -> assertFailure ("expected ForwardAccepted, got " <> show reply)

assertStructuralPending :: RequestId -> ApplicationRequestReply -> Assertion
assertStructuralPending request = \case
  RetainedRequestReply _ (OperationAccepted retainedRequest StructuralStabilizationPending) ->
    assertEqual "pending request identity" request retainedRequest
  reply -> assertFailure ("expected structural pending reply, got " <> show reply)

obsoleteObservedAt :: GlobalObjectId -> Controlled.State -> Maybe MonotonicInstant
obsoleteObservedAt target =
  Controlled.controlledRecordObsoleteObservedAt . controlledRecord target

controlledRecord :: GlobalObjectId -> Controlled.State -> Controlled.ControlledLocalRecord
controlledRecord target state =
  maybe (error "forward target record missing") id (Controlled.controlledLocalRecord target state)

forwardApplication ::
  HeraldEpoch ->
  ProcessEpochId ->
  PredefinedSortRole ->
  NablaId ->
  DeltaId ->
  ( ApplicationState.State,
    PrivateNablaId,
    ApplicationSessionId,
    ApplicationSessionBinding
  )
forwardApplication herald process selectedRole selectedWriter selectedReader =
  (openedState, privateWriter, session, binding)
  where
    attachment =
      applicationAttachmentForBootstrap
        (checkedPure "forward bootstrap manifest" (mkBootstrapManifestId (processEpochIdBytes process)))
    roots =
      concat
        [ [ ApplicationState.ApplicationWriterRoot
              role
              ( if role == selectedRole
                  then selectedWriter
                  else fixtureIdentity "forward application writer root" mkNablaId (0xb0 + ordinal)
              )
              UnsequencedNabla,
            ApplicationState.ApplicationReaderRoot
              role
              ( if role == selectedRole
                  then selectedReader
                  else fixtureIdentity "forward application reader root" mkDeltaId (0xc0 + ordinal)
              )
          ]
        | (ordinal, role) <- zip [0 ..] allPredefinedSortRoles
        ]
    (bootstrapped, access) =
      ApplicationState.commitApplicationBootstrap
        ( checkedPure
            "forward application bootstrap"
            (ApplicationState.prepareApplicationBootstrap attachment process roots fixtureEnvironmentHub (fixtureEnvironmentEdges roots) ApplicationState.emptyState)
        )
    privateWriter =
      case [ candidate
           | ApplicationState.WriterAccess role candidate _ <- ApplicationState.bootstrapAccessRoots access,
             role == selectedRole
           ] of
        [candidate] -> candidate
        _ -> error "forward writer was not localized exactly once"
    (openedState, acceptance) =
      ApplicationState.commitApplicationSessionAcceptance
        ( checkedPure
            "forward application session"
            (ApplicationState.prepareApplicationSessionOpen herald attachment (clientNonce 1) bootstrapped)
        )
    binding = sessionAcceptanceBinding acceptance
    session = case sessionAcceptanceReply acceptance of
      SessionOpened opened _ _ -> opened
      reply -> error ("expected opened forward session, got " <> show reply)

localizeObject ::
  ProcessEpochId ->
  GlobalUniqueId ->
  ApplicationState.State ->
  (ApplicationState.State, PrivateObjectId)
localizeObject process identifier state =
  case ApplicationState.commitOrdinaryApplicationValueLocalization
    ( checkedPure
        "forward object localization"
        ( ApplicationState.prepareOrdinaryApplicationValueLocalization
            (ApplicationState.processRoleView [process])
            process
            (Domain.globalUniqueIdValue identifier)
            state
        )
    ) of
    (successor, Application.UniqueIdValue privateIdentity) ->
      (successor, asPrivateObjectId privateIdentity)
    _ -> error "forward object localization changed value kind"

ordinaryControlledDescriptor :: Word64 -> CanonicalDescriptor
ordinaryControlledDescriptor minimumRetention =
  admittedApplicationSortDescriptor
    ( checkedPure
        "ordinary forward descriptor"
        (admitApplicationSortDefinition definition)
    )
  where
    definition =
      DeclaredSortDefinition
        ApplicationSortDescriptor
          { SortSyntax.sortKind = ControlledSort,
            SortSyntax.valueSchema =
              RecordSchema
                ( Map.fromList
                    [ ("label", LabelSchema),
                      ("object_id", UniqueIdSchema),
                      ("obsolete", BoolSchema)
                    ]
                ),
            SortSyntax.keyProjections =
              [ApplicationProjection ("object_id" :| [])],
            SortSyntax.validityPredicate = AlwaysPredicate,
            SortSyntax.obsolescencePredicate =
              CompareField
                (ApplicationProjection ("obsolete" :| []))
                ScalarEqual
                (LiteralBool True),
            SortSyntax.rankTerms =
              RankApplicationValue Ascending :| [],
            SortSyntax.minimumRetentionMicros = minimumRetention,
            SortSyntax.isImmutable = False,
            SortSyntax.labelField = Just "label"
          }
        Nothing

regularReferenceDescriptor :: CanonicalDescriptor
regularReferenceDescriptor =
  admittedApplicationSortDescriptor
    ( checkedPure
        "forward referenced regular descriptor"
        ( admitApplicationSortDefinition
            ( DeclaredSortDefinition
                ApplicationSortDescriptor
                  { SortSyntax.sortKind = RegularSort,
                    SortSyntax.valueSchema =
                      RecordSchema (Map.singleton "key" BoolSchema),
                    SortSyntax.keyProjections =
                      [ApplicationProjection ("key" :| [])],
                    SortSyntax.validityPredicate = AlwaysPredicate,
                    SortSyntax.obsolescencePredicate = NeverPredicate,
                    SortSyntax.rankTerms =
                      RankApplicationValue Ascending :| [],
                    SortSyntax.minimumRetentionMicros = 0,
                    SortSyntax.isImmutable = False,
                    SortSyntax.labelField = Nothing
                  }
                Nothing
            )
        )
    )

targetValue :: GlobalUniqueId -> Bool -> Domain.Value
targetValue target obsolete =
  checkedPure
    "forward target value"
    ( Domain.recordValue
        [ (checkedPure "forward label field" (Domain.mkFieldName "label"), Domain.labelValue (Domain.VoidLabel, 0)),
          (checkedPure "forward object field" (Domain.mkFieldName "object_id"), Domain.globalUniqueIdValue target),
          (checkedPure "forward obsolete field" (Domain.mkFieldName "obsolete"), Domain.boolValue obsolete)
        ]
    )

structuralTargetValue :: GlobalUniqueId -> Domain.Value
structuralTargetValue target =
  checkedPure
    "structural forward target value"
    ( Domain.recordValue
        [ (checkedPure "structural forward label field" (Domain.mkFieldName "label"), Domain.labelValue (Domain.VoidLabel, 0)),
          (checkedPure "structural forward object field" (Domain.mkFieldName "object_id"), Domain.globalUniqueIdValue target)
        ]
    )

structuralReferenceTargetValue ::
  PredefinedSortRole -> SortId -> GlobalUniqueId -> Domain.Value
structuralReferenceTargetValue role referencedSort target =
  checkedPure
    (show role <> " forward structural-reference value")
    (Domain.recordValue fields)
  where
    field name = checkedPure "forward structural-reference field" (Domain.mkFieldName name)
    common =
      [ (field "label", Domain.labelValue (Domain.VoidLabel, 0)),
        (field "object_id", Domain.globalUniqueIdValue target),
        (field "sort_id", Domain.bytesValue (sortIdBytes referencedSort))
      ]
    fields = case role of
      NablaRole ->
        (field "sequencing_object", Domain.optionalGlobalUniqueIdValue Nothing)
          : common
      DeltaRole -> common
      _ -> error "structural-reference value requires NablaRole or DeltaRole"

testStructuralProgress :: CheckedHeraldGenesis -> GraphProgress.StructuralProgressState
testStructuralProgress genesis =
  checkedPure
    "forward structural progress"
    ( GraphProgress.initialStructuralProgressState
        (checkedSystemId genesis)
        (checkedLocalHeraldEpoch genesis)
        membership
        projectionDigest
        (Reconciliation.emptyStructuralAppliedState membershipVector)
    )
  where
    members = case checkedActiveHeraldEpochs genesis of
      first : remaining -> first :| remaining
      [] -> error "checked genesis has no active Heralds"
    membership =
      checkedPure
        "forward structural membership"
        (genesisHeraldMembershipGeneration (checkedSystemId genesis) members)
    membershipVector = emptyStructuralVersionVector membership
    projectionDigest =
      checkedPure
        "forward initial projection digest"
        (mkInitialProjectionDigest (ByteString.replicate 32 0xd1))

carrierFor :: PredefinedSortRole -> PrimordialDefinitionReplica
carrierFor role =
  maybe
    (error ("missing primordial carrier " <> show role))
    id
    (find ((== role) . primordialReplicaRole) (checkedPrimordialReplicas fixtureCheckedGenesis))

fixtureIdentity ::
  (Show problem) =>
  String ->
  (ByteString.ByteString -> Either problem identity) ->
  Word8 ->
  identity
fixtureIdentity description constructor seed =
  checkedPure description (constructor (fixtureIdentifierBytes seed))

checkedPure :: (Show problem) => String -> Either problem value -> value
checkedPure description = either (error . ((description <> ": ") <>) . show) id

checkedIO :: (Show problem) => String -> Either problem value -> IO value
checkedIO description = either (assertFailure . ((description <> ": ") <>) . show) pure

checkedForward :: String -> Either LocalApplicationForwardError value -> IO value
checkedForward description =
  either (const (assertFailure (description <> ": forward preparation failed"))) pure
