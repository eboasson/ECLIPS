{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module ApplicationPublicationProperties (tests) where

import GenesisFixtures (fixtureRetirementResolution)

import Control.Monad (foldM)
import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Word (Word8)
import Eclips.Application.Types.Identity
  ( PrivateNablaId,
    PrivateUniqueId,
    asPrivateNablaId,
    mkPrivateUniqueId,
    privateNablaUniqueId,
  )
import Eclips.Application.Types.NewId (NewIdTarget (BareNewId))
import Eclips.Application.Types.Operation
  ( ApplicationOperation (NewIdApplication, WriteApplication),
  )
import Eclips.Application.Types.Result
  ( RegularCallResult (NewIdCompleted),
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, CompareField, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationScalarComparison (ScalarEqual),
    ApplicationScalarLiteral (LiteralBool),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (ControlledSort, RegularSort),
    ApplicationValueSchema (BoolSchema, LabelSchema, RecordSchema, UniqueIdSchema),
  )
import Eclips.Application.Types.SortDescriptor qualified as SortSyntax
import Eclips.Application.Types.Value qualified as Application
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (SortDefinitionWritten),
  )
import Eclips.Domain.Alignment
  ( HeraldPublicationPrefix (HeraldPublicationPrefixThrough),
  )
import Eclips.Domain.Disappearance
  ( deriveRegularSortOccurrenceClaim,
    regularSortDefinitionDisappearanceSubject,
  )
import Eclips.Domain.Graph
  ( EdgeStrength (Preserve, Weaken),
    VertexId (DeltaVertex, NablaVertex),
    edgePayload,
  )
import Eclips.Domain.Identity
  ( AuthorityEpoch,
    ControlIndex,
    DeltaId,
    GlobalObjectId,
    GlobalUniqueId,
    HeraldEpoch,
    NablaId,
    NablaSequencing (UnsequencedNabla),
    ProcessEpochId,
    ProcessId,
    PublicationId,
    SortDefinitionOccurrenceId,
    SortId,
    controlIndex,
    firstStructuralSequence,
    genesisAuthorityEpoch,
    globalObjectIdFromDeltaId,
    globalObjectIdFromGlobalUniqueId,
    globalObjectIdFromNablaId,
    globalObjectIdFromProcessEpochId,
    globalUniqueIdFromGlobalObjectId,
    mkBootstrapManifestId,
    mkDeltaId,
    mkGlobalUniqueId,
    mkHeraldEpoch,
    mkNablaId,
    mkProcessEpochId,
    mkProcessId,
    mkSortDefinitionOccurrenceId,
    mkSortId,
    mkStoreIncarnationId,
    mkTopologyCutId,
    nablaSequence,
    nablaSequenceWord64,
    processEpochIdBytes,
    publicationId,
    publicationNablaSequence,
    sortIdBytes,
    structuralAuthorityEpoch,
    structuralOccurrenceId,
  )
import Eclips.Domain.Membership
  ( admitHeraldMembershipGeneration,
    deriveHeraldAdmissionId,
    genesisHeraldMembershipGeneration,
    retireHeraldMembershipGeneration,
  )
import Eclips.Domain.Publication
  ( checkedPublicationKey,
    checkedPublicationSort,
    mkCheckedPublication,
  )
import Eclips.Domain.Route
  ( FrozenRoute,
    ReplicaStrength (Normal, Weak),
    destinationHerald,
    freezeRoute,
    partitionFrozenRoute,
    routeDestination,
    routeDestinations,
    routePartitionRemote,
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
  ( PredefinedSortRole (DeltaRole, EdgeRole, NablaRole, NeutralVertexRole, ProcessEpochRole, SortDefinitionRole),
    allPredefinedSortRoles,
    deriveSystemViewDeltaId,
    deriveSystemViewStoreIncarnationId,
    mkInitialProjectionDigest,
    primordialReplicaOccurrenceId,
    primordialReplicaPublication,
    primordialReplicaRole,
    primordialReplicaSortId,
  )
import Eclips.Domain.Store qualified as DomainStore
import Eclips.Domain.Structural (emptyStructuralVersionVector)
import Eclips.Domain.Value qualified as Domain
import Eclips.Herald.Application.Publication
  ( ApplicationPublicationAccessError (..),
    LocalApplicationPublicationEffects (..),
    LocalApplicationPublicationError (..),
    LocalControlledPublicationOutcome (..),
    PreparedLocalApplicationPublication,
    admittedApplicationPublicationSortDefinition,
    commitLocalApplicationPublication,
    localApplicationPublicationAdmittedValue,
    localApplicationPublicationClassification,
    localApplicationPublicationControlledOutcome,
    localApplicationPublicationEffects,
    localApplicationPublicationRecord,
    localApplicationPublicationReply,
    preparedLocalApplicationPublicationOutcome,
  )
import Eclips.Herald.Application.Publication qualified as ApplicationPublication
import Eclips.Herald.Application.Request.Internal
  ( ProcessAcceptancePosition,
    RequestId,
    processAcceptancePosition,
    requestId,
  )
import Eclips.Herald.Application.Session.Internal
  ( ApplicationAttachment,
    ApplicationSessionBinding,
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
    admittedApplicationSortId,
    admittedApplicationSortWriteResult,
  )
import Eclips.Herald.Application.State qualified as ApplicationState
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.Genesis (CheckedHeraldGenesis)
import Eclips.Herald.Genesis.Internal
  ( PrimordialDefinitionReplica,
    checkedActiveHeraldEpochs,
    checkedLocalHeraldEpoch,
    checkedPrimordialReplicas,
    checkedSystemId,
    primordialReplicaDescriptor,
  )
import Eclips.Herald.Graph.Progress qualified as GraphProgress
import Eclips.Herald.Graph.State qualified as Graph
import Eclips.Herald.PeerPayload
  ( PeerLogicalPayload (..),
  )
import Eclips.Herald.PeerPublication qualified as PeerPublication
import Eclips.Herald.PeerStream (mkStreamDirection, sequencedItemPayload)
import Eclips.Herald.PeerStream.State qualified as PeerStream
import Eclips.Herald.Placement qualified as PlacementMessage
import Eclips.Herald.Placement.State qualified as Placement
import Eclips.Herald.Publication.State
  ( ApplicationPublicationClassification (..),
    ApplicationPublicationOrigin (..),
  )
import Eclips.Herald.Publication.State qualified as Publication
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Structural.Reconciliation qualified as Reconciliation
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
import Test.Tasty.QuickCheck
  ( Property,
    counterexample,
    ioProperty,
    testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "generic application publication preparation"
    [ testCase
        "ordinary preparation derives route and commits Store, outbox, and sort induction"
        caseOrdinaryComposite,
      testCase
        "equal definition after retirement uses the Resolve-derived inner occurrence"
        caseEqualDefinitionAfterRetirement,
      testCase
        "exact retry precedes mutable revalidation and changed raw input conflicts"
        caseExactRetry,
      testCase
        "request ownership is session-scoped and shares the live request namespace"
        caseSessionScopedRequestOwnership,
      testCase
        "structural first-use retains the all-member unsequenced stage before reader routing"
        caseStructuralStage,
      testCase
        "structural definitions admit first publication and retry but reject later writes"
        caseImmutableStructuralDefinitions,
      testCase
        "Nabla and Delta publications carry the retirement floor of their referenced sort"
        caseStructuralReferenceControlFloor,
      testCase
        "structural routing retains mandatory system views and configured readers"
        caseStructuralReaderRoute,
      testCase
        "structural routing partitions a remote reader with its mandatory system view"
        caseStructuralRemoteReaderRoute,
      testCase
        "structural routing follows the authoritative successor membership"
        caseStructuralSuccessorMembership,
      testCase
        "new sort definitions reach admitted private views without application graph edges"
        caseSortDefinitionAdmissionMembership,
      testCase
        "controlled current and obsolete raw requests retain exact semantic linkage"
        caseControlledLifecycle,
      testCase
        "source, reservation, update, and edge failures leave every owner unchanged"
        caseNegativeRollbackMatrix,
      testProperty
        "mixed accept/retry traces keep positions and writer sequences gap-free"
        propMixedOrdering
    ]

caseOrdinaryComposite :: Assertion
caseOrdinaryComposite = do
  fixture <- ordinaryFixture
  applicationRequest <-
    checked
      "ordinary request"
      ( firstPublicationRequest
          fixture.applicationState
          fixture.session
          fixture.binding
          fixture.request
          fixture.privateNabla
          fixture.value
      )
  prepared <- checked "ordinary prepare" (prepareOrdinary fixture applicationRequest fixture.controlled fixture.registry fixture.publications fixture.stores fixture.stream)
  let outcome = preparedLocalApplicationPublicationOutcome prepared
      (applicationAfter, controlledAfter, registryAfter, publicationsAfter, storesAfter, streamAfter, committedOutcome) =
        commitLocalApplicationPublication prepared
      record = localApplicationPublicationRecord outcome
      identifier = Publication.applicationPublicationId record
  assertEqual "commit outcome" outcome committedOutcome
  assertBool
    "accepted request retained by Application"
    (not (null (ApplicationState.applicationRequestEntries fixture.session applicationAfter)))
  assertBool "regular leaves Controlled unchanged" (controlledAfter == fixture.controlled)
  assertEqual "regular owner outcome" RegularLocalPublication (localApplicationPublicationControlledOutcome outcome)
  assertEqual "first classification" ApplicationPublicationFirstAccepted (localApplicationPublicationClassification outcome)
  assertEqual "derived frozen route" fixture.route (Publication.applicationPublicationRoute record)
  assertEqual "first Herald position" 1 (Publication.heraldPublicationPositionWord64 (Publication.applicationPublicationHeraldPosition record))
  assertEqual "first writer sequence" 0 (nablaSequenceWord64 (publicationNablaSequence identifier))
  assertEqual
    "ordinary record installed"
    (Just (Publication.applicationPublicationChecked record))
    (Publication.outgoingPublicationChecked <$> Publication.lookupOutgoingPublication identifier publicationsAfter)
  definition <-
    maybe
      (fail "sort-definition evidence missing")
      pure
      (admittedApplicationPublicationSortDefinition (localApplicationPublicationAdmittedValue outcome))
  assertBool
    "sort is induced in the same prepared successor"
    (SortRegistry.lookupEffectiveSort (admittedApplicationSortId definition) registryAfter /= Nothing)
  assertStoreReceipt fixture.localSlot identifier storesAfter
  case localApplicationPublicationEffects outcome of
    OrdinaryApplicationPublicationEffects batches assignments offers tickets -> do
      batch <- requireMapEntry "remote batch" fixture.remoteHerald batches
      assigned <- requireMapEntry "remote assignment" fixture.remoteHerald assignments
      assertEqual
        "exact peer publication assigned"
        (PeerLogicalPublication batch)
        (sequencedItemPayload (NonEmpty.head assigned))
      assertEqual "remote frontier offered" [fixture.remoteHerald] (Map.keys offers)
      assertEqual "no live binding has no dispatch ticket" [] tickets
      direction <- checked "stream direction" (mkStreamDirection fixture.localHerald fixture.remoteHerald)
      active <- checked "active outbox" (PeerStream.activeOutgoingItems direction streamAfter)
      assertEqual "assigned item retained" (NonEmpty.toList assigned) active
    other -> assertFailure ("expected ordinary effects, got " <> show other)

caseEqualDefinitionAfterRetirement :: Assertion
caseEqualDefinitionAfterRetirement = do
  fixture <- ordinaryFixture
  firstRequest <-
    checked
      "first definition request"
      ( firstPublicationRequest
          fixture.applicationState
          fixture.session
          fixture.binding
          fixture.request
          fixture.privateNabla
          fixture.value
      )
  firstPrepared <-
    checked
      "first definition preparation"
      ( prepareOrdinary
          fixture
          firstRequest
          fixture.controlled
          fixture.registry
          fixture.publications
          fixture.stores
          fixture.stream
      )
  let (applicationAfterFirst, controlledAfterFirst, registryAfterFirst, publicationsAfterFirst, storesAfterFirst, streamAfterFirst, firstOutcome) =
        commitLocalApplicationPublication firstPrepared
      firstRecord = localApplicationPublicationRecord firstOutcome
  firstDefinition <-
    maybe
      (assertFailure "first admitted definition missing")
      pure
      ( admittedApplicationPublicationSortDefinition
          (localApplicationPublicationAdmittedValue firstOutcome)
      )
  let descriptor = admittedApplicationSortDescriptor firstDefinition
      sortId = admittedApplicationSortId firstDefinition
      carrier = carrierFor SortDefinitionRole
      carrierSortId = primordialReplicaSortId carrier
      carrierOccurrence = primordialReplicaOccurrenceId carrier
      oldEntry =
        maybe
          (error "first dynamic sort registry entry missing")
          id
          (SortRegistry.lookupEffectiveSort sortId registryAfterFirst)
      oldOccurrence = SortRegistry.registryEntryOccurrenceId oldEntry
      resolveIndex = controlIndex 11
      successorOccurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId fixtureCheckedGenesis)
          sortId
          ( checkedPure
              "Resolve-derived sort occurrence base"
              (resolvedRetirementOccurrenceBase resolveIndex)
          )
      subject =
        regularSortDefinitionDisappearanceSubject
          ( deriveRegularSortOccurrenceClaim
              (checkedSystemId fixtureCheckedGenesis)
              descriptor
              Genesis
          )
      registryRetired =
        SortRegistry.commitRegularSortRetirement
          ( checkedPure
              "retire first dynamic registry occurrence"
              ( SortRegistry.prepareExactRegularSortRetirement
                  (checkedSystemId fixtureCheckedGenesis)
                  oldEntry
                  resolveIndex
                  successorOccurrence
                  registryAfterFirst
              )
          )
      (storesRetired, storeRetirementSummary) =
        Store.commitRegularDefinitionRetirement
          ( checkedPure
              "retire first dynamic Store projection"
              ( Store.prepareRegularDefinitionRetirement
                  subject
                  ( HeraldPublicationPrefixThrough
                      (Publication.applicationPublicationHeraldPosition firstRecord)
                  )
                  resolveIndex
                  storesAfterFirst
              )
          )
  assertEqual
    "the first application result exposes the content-addressed SortId"
    (SortDefinitionWritten sortId)
    (admittedApplicationSortWriteResult firstDefinition)
  assertEqual
    "the first contained definition starts at Genesis"
    ( deriveSortDefinitionOccurrenceId
        (checkedSystemId fixtureCheckedGenesis)
        sortId
        Genesis
    )
    oldOccurrence
  assertEqual
    "retirement removes the old effective registry entry"
    Nothing
    (SortRegistry.lookupEffectiveSort sortId registryRetired)
  assertEqual
    "the Store projection retirement was applied"
    Store.RegularDefinitionRetirementApplied
    ( Store.regularDefinitionRetirementSummaryDisposition
        storeRetirementSummary
    )

  let (structuralProgressAtResolve, structuralAdvance) =
        GraphProgress.commitStructuralControlProgress
          ( checkedPure
              "advance structural control through regular Resolve"
              ( GraphProgress.prepareStructuralControlProgress
                  resolveIndex
                  (testStructuralProgress fixtureCheckedGenesis)
              )
          )
  assertEqual
    "the disappearance Resolve advances structural control"
    GraphProgress.StructuralControlAdvanced
    structuralAdvance
  assertEqual
    "the second publication predecessor covers the Resolve index"
    resolveIndex
    (GraphProgress.structuralAppliedControlPrefix structuralProgressAtResolve)

  secondRequest <-
    checked
      "equal redefinition request"
      ( firstPublicationRequest
          applicationAfterFirst
          fixture.session
          fixture.binding
          (requestId 2)
          fixture.privateNabla
          fixture.value
      )
  secondPrepared <-
    checked
      "equal redefinition preparation"
      ( prepareOrdinaryWithStructuralProgress
          fixture
          structuralProgressAtResolve
          secondRequest
          controlledAfterFirst
          registryRetired
          publicationsAfterFirst
          storesRetired
          streamAfterFirst
      )
  let (_, _, registryAfterSecond, _, storesAfterSecond, _, secondOutcome) =
        commitLocalApplicationPublication secondPrepared
      secondRecord = localApplicationPublicationRecord secondOutcome
      secondChecked = Publication.applicationPublicationChecked secondRecord
  secondDefinition <-
    maybe
      (assertFailure "second admitted definition missing")
      pure
      ( admittedApplicationPublicationSortDefinition
          (localApplicationPublicationAdmittedValue secondOutcome)
      )
  effectiveAfterSecond <-
    maybe
      (assertFailure "equal redefinition was not induced")
      pure
      (SortRegistry.lookupEffectiveSort sortId registryAfterSecond)
  outgoing <-
    maybe
      (assertFailure "equal redefinition has no outgoing record")
      pure
      (Publication.applicationPublicationOutgoingRecord secondRecord)
  storeSlot <-
    maybe
      (assertFailure "sort-definition Store disappeared")
      pure
      ( Store.lookupStoreSlot
          (Store.storeSlotDelta fixture.localSlot)
          storesAfterSecond
      )
  storedDefinition <-
    maybe
      (assertFailure "equal redefinition is not visible in the local Store")
      pure
      ( DomainStore.lookupVisible
          (checkedPublicationKey secondChecked)
          (Store.storeSlotContents storeSlot)
      )

  assertEqual
    "the equal definition returns the same public SortId"
    sortId
    (admittedApplicationSortId secondDefinition)
  assertEqual
    "the second application result carries that same public SortId"
    (SortDefinitionWritten sortId)
    (admittedApplicationSortWriteResult secondDefinition)
  assertEqual
    "the contained definition uses the Resolve-derived occurrence"
    successorOccurrence
    (SortRegistry.registryEntryOccurrenceId effectiveAfterSecond)
  assertBool
    "the stale Genesis occurrence is no longer effective"
    (SortRegistry.registryEntryOccurrenceId effectiveAfterSecond /= oldOccurrence)
  assertBool
    "the stale occurrence remains classified as retired rather than effective"
    ( SortRegistry.lookupRegularSortRetirement
        sortId
        oldOccurrence
        registryAfterSecond
        /= Nothing
    )
  assertEqual
    "the enclosing checked publication remains typed by sort-sort"
    carrierSortId
    (checkedPublicationSort secondChecked)
  assertEqual
    "the enclosing publication retains the primordial sort-sort occurrence"
    carrierOccurrence
    (Publication.applicationPublicationSortOccurrenceId secondRecord)
  assertEqual
    "the retained outgoing record carries the primordial occurrence"
    carrierOccurrence
    (Publication.outgoingPublicationOccurrenceId outgoing)
  assertEqual
    "the Resolve index becomes the exact publication prerequisite"
    resolveIndex
    (Publication.applicationPublicationControlPrerequisite secondRecord)
  assertEqual
    "the retained outgoing publication keeps that Resolve prerequisite"
    resolveIndex
    (Publication.outgoingPublicationControlPrerequisite outgoing)
  assertEqual
    "the frozen outer route remains the original sort-sort route"
    fixture.route
    (Publication.applicationPublicationRoute secondRecord)
  assertBool
    "the remote sort-sort batch remains present"
    (not (Map.null (Publication.outgoingPublicationRemoteBatches outgoing)))
  mapM_
    ( \batch -> do
        assertEqual
          "remote batch retains the sort-sort carrier SortId"
          carrierSortId
          (PeerPublication.publicationBatchSortId batch)
        assertEqual
          "remote batch retains the primordial sort-sort occurrence"
          carrierOccurrence
          (PeerPublication.publicationBatchOccurrenceId batch)
        assertEqual
          "remote batch carries the Resolve prerequisite"
          resolveIndex
          (PeerPublication.publicationBatchControlPrerequisite batch)
    )
    (Map.elems (Publication.outgoingPublicationRemoteBatches outgoing))
  assertEqual
    "the local Store remains a sort-sort carrier"
    (carrierSortId, carrierOccurrence)
    (Store.storeSlotSortId storeSlot, Store.storeSlotOccurrenceId storeSlot)
  assertEqual
    "the local Store exposes the new enclosing publication"
    secondChecked
    (DomainStore.storedPublication storedDefinition)

caseExactRetry :: Assertion
caseExactRetry = do
  fixture <- ordinaryFixture
  firstRequest <-
    checked
      "first request"
      ( firstPublicationRequest
          fixture.applicationState
          fixture.session
          fixture.binding
          fixture.request
          fixture.privateNabla
          fixture.value
      )
  first <- checked "first" (prepareOrdinary fixture firstRequest fixture.controlled fixture.registry fixture.publications fixture.stores fixture.stream)
  let (applicationAfter, controlledAfter, registryAfter, publicationsAfter, storesAfter, streamAfter, firstOutcome) = commitLocalApplicationPublication first
      firstRecord = localApplicationPublicationRecord firstOutcome
      retryClassification =
        ApplicationState.classifyApplicationRequest
          fixture.session
          fixture.binding
          fixture.request
          (WriteApplication fixture.privateNabla (PublishValue fixture.value))
          applicationAfter
  case retryClassification of
    Right (ApplicationState.RetainedApplicationRequest reply) ->
      assertEqual "exact retry reoffers the retained reply" (localApplicationPublicationReply firstOutcome) reply
    Left problem -> assertFailure ("exact retry classifier failed: " <> show problem)
    Right _ -> assertFailure "exact retry did not classify as retained"
  assertEqual
    "semantic record remains stable"
    (Just firstRecord)
    (Publication.lookupApplicationPublication fixture.position publicationsAfter)
  assertBool "Controlled remains the committed first successor" (controlledAfter == fixture.controlled)
  assertBool "SortRegistry retains the committed first successor" (registryAfter /= fixture.registry)
  assertBool "Store retains the committed first successor" (storesAfter /= fixture.stores)
  assertBool "PeerStream retains the committed first successor" (streamAfter /= fixture.stream)
  assertRequestConflict
    "changed value"
    ( ApplicationState.classifyApplicationRequest
        fixture.session
        fixture.binding
        fixture.request
        (WriteApplication fixture.privateNabla (PublishValue (Application.BoolValue True)))
        applicationAfter
    )

caseSessionScopedRequestOwnership :: Assertion
caseSessionScopedRequestOwnership = do
  fixture <- ordinaryFixture
  firstRequest <-
    checked
      "session-owned first request"
      ( firstPublicationRequest
          fixture.applicationState
          fixture.session
          fixture.binding
          fixture.request
          fixture.privateNabla
          fixture.value
      )
  first <-
    checked
      "session-owned first publication"
      ( prepareOrdinary
          fixture
          firstRequest
          fixture.controlled
          fixture.registry
          fixture.publications
          fixture.stores
          fixture.stream
      )
  let (applicationAfter, _, _, publicationsAfter, _, _, outcome) =
        commitLocalApplicationPublication first
      record = localApplicationPublicationRecord outcome
      retained =
        ApplicationState.applicationRequestEntries
          fixture.session
          applicationAfter
  case retained of
    [(retainedId, witness)] -> do
      assertEqual "retained request key" fixture.request retainedId
      assertEqual
        "retained exact typed publication call"
        (WriteApplication fixture.privateNabla (PublishValue fixture.value))
        (ApplicationState.applicationRequestWitnessCall witness)
      assertEqual
        "retained process position"
        (Just fixture.position)
        (ApplicationState.applicationRequestWitnessPosition witness)
    other -> assertFailure ("expected one retained generic request, got " <> show (length other))
  case ApplicationState.classifyApplicationRequest
    fixture.session
    fixture.binding
    fixture.request
    (NewIdApplication BareNewId)
    applicationAfter of
    Right (ApplicationState.ConflictingApplicationRequest _) -> pure ()
    other ->
      assertFailure
        ( "generic request did not reserve the live request namespace: "
            <> either show (const "non-conflict") other
        )
  ended <-
    checked
      "end generic publication session"
      ( ApplicationState.prepareApplicationSessionEnd
          fixture.session
          fixture.binding
          applicationAfter
      )
  let applicationEnded = ApplicationState.commitApplicationSessionEnd ended
  assertEqual
    "session end removes the generic request owner"
    []
    ( ApplicationState.applicationRequestEntries
        fixture.session
        applicationEnded
    )
  assertEqual
    "semantic publication survives session end"
    (Just record)
    (Publication.lookupApplicationPublication fixture.position publicationsAfter)
  case ApplicationState.applicationRequestResult
    fixture.session
    fixture.binding
    fixture.request
    applicationEnded of
    Left _ -> pure ()
    Right _ -> assertFailure "ended session unexpectedly retained retry reachability"
  let attachment = applicationAttachment fixture.process
      (applicationReopened, acceptance) =
        ApplicationState.commitApplicationSessionAcceptance
          ( checkedPure
              "open replacement session"
              ( ApplicationState.prepareApplicationSessionOpen
                  fixture.localHerald
                  attachment
                  (clientNonce 2)
                  applicationEnded
              )
          )
      replacementBinding = sessionAcceptanceBinding acceptance
      replacementSession = case sessionAcceptanceReply acceptance of
        SessionOpened session _ _ -> session
        reply -> error ("expected replacement session, got " <> show reply)
  replacement <-
    checked
      "new session same request id"
      ( firstPublicationRequest
          applicationReopened
          replacementSession
          replacementBinding
          fixture.request
          fixture.privateNabla
          fixture.value
      )
  assertEqual
    "new session uses next process-wide position"
    (processAcceptancePosition fixture.process 2)
    (ApplicationState.applicationRequestCandidatePosition replacement)

  -- The reverse namespace direction is equally closed: a currently retained
  -- live request ID cannot be repurposed as the package-private call.
  ordinaryCandidate <-
    case ApplicationState.classifyApplicationRequest
      fixture.session
      fixture.binding
      fixture.request
      (NewIdApplication BareNewId)
      fixture.applicationState of
      Right (ApplicationState.FirstApplicationRequest candidate) -> pure candidate
      other ->
        fail
          ( "ordinary request did not classify first: "
              <> either show (const "unexpected classification") other
          )
  let (ordinaryRetained, _) =
        ApplicationState.commitApplicationRequest
          ( ApplicationState.prepareApplicationRequestCompletion
              ordinaryCandidate
              ( NewIdCompleted
                  (checkedPure "ordinary result private id" (mkPrivateUniqueId 77))
              )
          )
  assertRequestConflict
    "live request namespace"
    ( ApplicationState.classifyApplicationRequest
        fixture.session
        fixture.binding
        fixture.request
        (WriteApplication fixture.privateNabla (PublishValue fixture.value))
        ordinaryRetained
    )
  let otherPrivateNabla =
        asPrivateNablaId
          (checkedPure "different private nabla" (mkPrivateUniqueId 999))
  assertRequestConflict
    "changed private nabla"
    ( ApplicationState.classifyApplicationRequest
        fixture.session
        fixture.binding
        fixture.request
        (WriteApplication otherPrivateNabla (PublishValue fixture.value))
        applicationAfter
    )

caseStructuralStage :: Assertion
caseStructuralStage = do
  fixture <- structuralFixture
  applicationRequest <-
    checked
      "structural request"
      ( firstPublicationRequest
          fixture.applicationState
          fixture.session
          fixture.binding
          fixture.request
          fixture.privateNabla
          fixture.value
      )
  prepared <- checked "structural" (prepareStructural fixture applicationRequest)
  let (_, controlledAfter, registryAfter, publicationsAfter, storesAfter, streamAfter, outcome) =
        commitLocalApplicationPublication prepared
      record = localApplicationPublicationRecord outcome
      identifier = Publication.applicationPublicationId record
  assertEqual "first-use winner" (ControlledLocalFirstUse Controlled.ControlledFirstUseWon) (localApplicationPublicationControlledOutcome outcome)
  assertEqual "no ordinary plan" UnsequencedStructuralApplicationPublicationEffects (localApplicationPublicationEffects outcome)
  assertBool "possession installed" (Controlled.controlledHasNormalPossession fixture.process fixture.objectId controlledAfter)
  assertBool "registry unchanged" (registryAfter == fixture.registry)
  assertBool "Store unchanged" (storesAfter == fixture.stores)
  assertEqual "stream unchanged" fixture.stream streamAfter
  assertEqual "not ordinary outgoing" Nothing (Publication.lookupOutgoingPublication identifier publicationsAfter)
  stage <- maybe (fail "unsequenced stage missing") pure (Publication.applicationPublicationUnsequencedStage record)
  assertEqual "genesis all-member route" fixture.route (Publication.unsequencedStructuralRoute stage)
  assertEqual "source process" fixture.process (Publication.unsequencedStructuralSourceProcess stage)
  assertEqual "acceptance position" fixture.position (Publication.unsequencedStructuralAcceptancePosition stage)
  assertEqual "effective occurrence" fixture.occurrence (Publication.unsequencedStructuralSortOccurrenceId stage)
  assertEqual "control prerequisite" fixture.controlPrerequisite (Publication.unsequencedStructuralControlPrerequisite stage)
  assertEqual "ordinary arm remains distinct" Nothing (admittedApplicationPublicationSortDefinition (localApplicationPublicationAdmittedValue outcome))

caseImmutableStructuralDefinitions :: Assertion
caseImmutableStructuralDefinitions =
  mapM_ exercise [NeutralVertexRole, EdgeRole, NablaRole, DeltaRole]
  where
    exercise :: PredefinedSortRole -> Assertion
    exercise role = do
      fixture <-
        structuralRoleFixture
          role
          (primordialReplicaSortId (carrierFor NeutralVertexRole))
          (SortRegistry.initialState fixtureCheckedGenesis)
      firstRequest <-
        checked
          (show role <> " first request")
          ( firstPublicationRequest
              fixture.applicationState
              fixture.session
              fixture.binding
              fixture.request
              fixture.privateNabla
              fixture.value
          )
      first <- checked (show role <> " first publication") (prepareStructural fixture firstRequest)
      let (applicationAfter, controlledAfter, registryAfter, publicationsAfter, storesAfter, streamAfter, outcome) =
            commitLocalApplicationPublication first
          committed :: StructuralFixture
          committed = case fixture of
            StructuralFixture {..} ->
              StructuralFixture
                { applicationState = applicationAfter,
                  controlled = controlledAfter,
                  registry = registryAfter,
                  publications = publicationsAfter,
                  stores = storesAfter,
                  stream = streamAfter,
                  ..
                }
      assertEqual
        (show role <> " first publication establishes the definition")
        (ControlledLocalFirstUse Controlled.ControlledFirstUseWon)
        (localApplicationPublicationControlledOutcome outcome)
      case ApplicationState.classifyApplicationRequest
        fixture.session
        fixture.binding
        fixture.request
        (WriteApplication fixture.privateNabla (PublishValue fixture.value))
        applicationAfter of
        Right (ApplicationState.RetainedApplicationRequest reply) ->
          assertEqual
            (show role <> " exact retry retains the successful first reply")
            (localApplicationPublicationReply outcome)
            reply
        Left problem -> assertFailure (show role <> " retry failed: " <> show problem)
        Right _ -> assertFailure (show role <> " exact retry was not retained")
      mapM_ (rejectRewrite role committed) (rewriteCandidates role committed)

    rejectRewrite :: PredefinedSortRole -> StructuralFixture -> (String, Application.ApplicationValue) -> Assertion
    rejectRewrite role fixture (description, candidateValue) = do
      candidate <-
        checked
          (show role <> " " <> description <> " request")
          ( firstPublicationRequest
              fixture.applicationState
              fixture.session
              fixture.binding
              (requestId 2)
              fixture.privateNabla
              candidateValue
          )
      assertLocalPublicationFailure
        (show role <> " " <> description)
        ( \case
            LocalApplicationPublicationAccessError (ApplicationPublicationImmutableUpdate sortId) ->
              sortId == primordialReplicaSortId (carrierFor role)
            _ -> False
        )
        ( prepareStructural
            ( case fixture of
                StructuralFixture {..} -> StructuralFixture {value = candidateValue, ..}
            )
            candidate
        )
      assertStillFirst
        (show role <> " rejected rewrite")
        fixture.applicationState
        fixture.session
        fixture.binding
        (requestId 2)
        fixture.privateNabla
        candidateValue

    rewriteCandidates :: PredefinedSortRole -> StructuralFixture -> [(String, Application.ApplicationValue)]
    rewriteCandidates role fixture =
      ("same definition under a new request", fixture.value)
        : case role of
          NeutralVertexRole -> []
          EdgeRole ->
            [("changed edge strength", replaceField "strength" (Application.EnumValue "weaken") fixture.value)]
          NablaRole ->
            [ ("changed carried sort", changedSort fixture.value),
              ( "changed sequencing object",
                replaceField
                  "sequencing_object"
                  (Application.OptionalUniqueIdValue (Just (privateNablaUniqueId fixture.privateNabla)))
                  fixture.value
              )
            ]
          DeltaRole -> [("changed carried sort", changedSort fixture.value)]
          _ -> error "immutable structural fixture requires a structural carrier"

    changedSort =
      replaceField
        "sort_id"
        (Application.BytesValue (sortIdBytes (primordialReplicaSortId (carrierFor EdgeRole))))

    replaceField name replacement (Application.RecordValue fields) =
      Application.RecordValue (Map.insert name replacement fields)
    replaceField _ _ _ = error "structural fixture must have a record payload"

caseStructuralReferenceControlFloor :: Assertion
caseStructuralReferenceControlFloor = do
  let resolveIndex = controlIndex 11
  (referencedSort, retiredRegistry) <-
    retiredStructuralReferenceRegistry resolveIndex
  mapM_
    (exercise referencedSort retiredRegistry resolveIndex)
    [NablaRole, DeltaRole]
  where
    exercise referencedSort retiredRegistry resolveIndex role = do
      fixture <-
        structuralRoleFixture role referencedSort retiredRegistry
      applicationRequest <-
        checked
          (show role <> " structural reference request")
          ( firstPublicationRequest
              fixture.applicationState
              fixture.session
              fixture.binding
              fixture.request
              fixture.privateNabla
              fixture.value
          )
      let (progressAtResolve, _) =
            GraphProgress.commitStructuralControlProgress
              ( checkedPure
                  (show role <> " structural reference control progress")
                  ( GraphProgress.prepareStructuralControlProgress
                      resolveIndex
                      (testStructuralProgress fixtureCheckedGenesis)
                  )
              )
      prepared <-
        checked
          (show role <> " structural reference publication")
          ( prepareStructuralWithProgress
              progressAtResolve
              fixture
              applicationRequest
          )
      let (_, _, _, _, _, _, outcome) =
            commitLocalApplicationPublication prepared
          record = localApplicationPublicationRecord outcome
      stage <-
        maybe
          (assertFailure (show role <> " publication has no unsequenced stage"))
          pure
          (Publication.applicationPublicationUnsequencedStage record)
      assertEqual
        (show role <> " carries the referenced sort retirement Resolve")
        resolveIndex
        (Publication.unsequencedStructuralControlPrerequisite stage)
      assertEqual
        (show role <> " retains its immutable predefined carrier occurrence")
        fixture.occurrence
        (Publication.unsequencedStructuralSortOccurrenceId stage)

caseStructuralReaderRoute :: Assertion
caseStructuralReaderRoute = do
  let local = checkedLocalHeraldEpoch fixtureCheckedGenesis
      carrier = carrierFor NeutralVertexRole
      descriptor = primordialReplicaDescriptor carrier
      sortId = primordialReplicaSortId carrier
      occurrence = primordialReplicaOccurrenceId carrier
      writer = fixtureIdentity "structural reader-route writer" mkNablaId 0x91
      reader = fixtureIdentity "structural reader-route Delta" mkDeltaId 0x92
      incarnation = fixtureIdentity "structural reader-route incarnation" mkStoreIncarnationId 0x93
      process = fixtureIdentity "structural reader-route process" mkProcessEpochId 0x94
      localSystemView =
        deriveSystemViewDeltaId
          (checkedSystemId fixtureCheckedGenesis)
          local
          NeutralVertexRole
      localSystemViewIncarnation =
        deriveSystemViewStoreIncarnationId
          (checkedSystemId fixtureCheckedGenesis)
          local
          NeutralVertexRole
      graph =
        Graph.graphStateForReachabilityTest
          [ NablaVertex writer,
            DeltaVertex reader,
            DeltaVertex localSystemView
          ]
          [ edgePayload (NablaVertex writer) (DeltaVertex reader) Weaken,
            edgePayload (NablaVertex writer) (DeltaVertex localSystemView) Weaken
          ]
      readerPlacement =
        Placement.localPlacement
          reader
          sortId
          occurrence
          (globalObjectIdFromDeltaId reader)
          process
          local
          incarnation
          (controlIndex 0)
      placement =
        Placement.commitPlacementBootstrap
          ( checkedPure
              "structural configured-reader placement"
              ( Placement.preparePlacementBootstrap
                  [readerPlacement]
                  ( Placement.commitSystemViewPlacements
                      ( checkedPure
                          "structural mandatory system-view placement"
                          ( Placement.prepareSystemViewPlacements
                              [ Placement.systemViewPlacement
                                  NeutralVertexRole
                                  localSystemView
                                  sortId
                                  occurrence
                                  local
                                  localSystemViewIncarnation
                              ]
                              (Placement.initialState local)
                          )
                      )
                  )
              )
          )
      expected =
        checkedPure
          "structural mandatory-plus-reader route"
          ( freezeRoute
              ( [ routeDestination
                    (deriveSystemViewDeltaId (checkedSystemId fixtureCheckedGenesis) herald NeutralVertexRole)
                    (deriveSystemViewStoreIncarnationId (checkedSystemId fixtureCheckedGenesis) herald NeutralVertexRole)
                    herald
                    Normal
                | herald <- checkedActiveHeraldEpochs fixtureCheckedGenesis
                ]
                  <> [routeDestination reader incarnation local Weak]
              )
          )
      actual =
        ApplicationPublication.deriveApplicationPublicationRoute
          fixtureCheckedGenesis
          fixtureHeraldMembershipGeneration
          descriptor
          writer
          graph
          placement
  assertEqual
    "the structural cut unions configured readers and lets mandatory Normal replication dominate a Weak graph path"
    (Right expected)
    actual

caseStructuralRemoteReaderRoute :: Assertion
caseStructuralRemoteReaderRoute = do
  let local = checkedLocalHeraldEpoch fixtureCheckedGenesis
      remote =
        case filter (/= local) (checkedActiveHeraldEpochs fixtureCheckedGenesis) of
          remoteHerald : _ -> remoteHerald
          [] -> error "structural remote-reader fixture requires a remote active Herald"
      carrier = carrierFor NeutralVertexRole
      descriptor = primordialReplicaDescriptor carrier
      sortId = primordialReplicaSortId carrier
      occurrence = primordialReplicaOccurrenceId carrier
      writer = fixtureIdentity "structural remote-reader writer" mkNablaId 0x95
      reader = fixtureIdentity "structural remote-reader Delta" mkDeltaId 0x96
      incarnation = fixtureIdentity "structural remote-reader incarnation" mkStoreIncarnationId 0x97
      process = fixtureIdentity "structural remote-reader process" mkProcessEpochId 0x98
      graph =
        Graph.graphStateForReachabilityTest
          [NablaVertex writer, DeltaVertex reader]
          [edgePayload (NablaVertex writer) (DeltaVertex reader) Weaken]
      remoteReaderRoute =
        PlacementMessage.applicationDeltaRoute
          reader
          sortId
          occurrence
          (globalObjectIdFromDeltaId reader)
          process
          incarnation
          (controlIndex 0)
      remoteSnapshot =
        checkedPure
          "structural remote-reader snapshot"
          ( PlacementMessage.placementSnapshot
              remote
              PlacementMessage.firstPlacementSequence
              [remoteReaderRoute]
          )
      (placement, _) =
        Placement.commitRemotePlacement
          ( checkedPure
              "structural remote-reader placement"
              ( Placement.prepareRemotePlacement
                  (PlacementMessage.FullPlacementSnapshot remoteSnapshot)
                  (Placement.initialState local)
              )
          )
      mandatoryDestination herald =
        routeDestination
          (deriveSystemViewDeltaId (checkedSystemId fixtureCheckedGenesis) herald NeutralVertexRole)
          (deriveSystemViewStoreIncarnationId (checkedSystemId fixtureCheckedGenesis) herald NeutralVertexRole)
          herald
          Normal
      configuredReaderDestination =
        routeDestination reader incarnation remote Weak
      expected =
        checkedPure
          "structural remote mandatory-plus-reader route"
          ( freezeRoute
              ( configuredReaderDestination
                  : fmap mandatoryDestination (checkedActiveHeraldEpochs fixtureCheckedGenesis)
              )
          )
      expectedRemoteCut =
        checkedPure
          "structural remote partition"
          (freezeRoute [configuredReaderDestination, mandatoryDestination remote])
  actual <-
    checked
      "structural remote route derivation"
      ( ApplicationPublication.deriveApplicationPublicationRoute
          fixtureCheckedGenesis
          fixtureHeraldMembershipGeneration
          descriptor
          writer
          graph
          placement
      )
  assertEqual
    "the structural cut contains the remote Weak reader and every mandatory Normal system view"
    expected
    actual
  remoteCut <-
    requireMapEntry
      "remote structural route partition"
      remote
      (routePartitionRemote (partitionFrozenRoute local actual))
  assertEqual
    "the selected remote Herald receives its configured reader and mandatory system view in one peer cut"
    (routeDestinations expectedRemoteCut)
    (NonEmpty.toList remoteCut)

caseStructuralSuccessorMembership :: Assertion
caseStructuralSuccessorMembership = do
  let local = checkedLocalHeraldEpoch fixtureCheckedGenesis
      remote =
        case filter (/= local) (checkedActiveHeraldEpochs fixtureCheckedGenesis) of
          remoteHerald : _ -> remoteHerald
          [] -> error "successor-membership fixture requires a remote Herald"
      successor =
        checkedPure
          "retired-member successor"
          (retireHeraldMembershipGeneration (controlIndex 17) (fixtureRetirementResolution (controlIndex 17)) remote fixtureHeraldMembershipGeneration)
      carrier = carrierFor NeutralVertexRole
      descriptor = primordialReplicaDescriptor carrier
      writer = fixtureIdentity "successor route writer" mkNablaId 0x99
      route =
        checkedPure
          "successor structural route"
          ( ApplicationPublication.deriveApplicationPublicationRoute
              fixtureCheckedGenesis
              successor
              descriptor
              writer
              (Graph.graphStateForReachabilityTest [NablaVertex writer] [])
              (Placement.initialState local)
          )
      routedHeralds = destinationHerald <$> routeDestinations route
  assertBool "retired member is absent" (remote `notElem` routedHeralds)
  assertBool "surviving member remains" (local `elem` routedHeralds)

caseSortDefinitionAdmissionMembership :: Assertion
caseSortDefinitionAdmissionMembership = do
  let local = checkedLocalHeraldEpoch fixtureCheckedGenesis
      newcomer = fixtureIdentity "definition admission newcomer" mkHeraldEpoch 0xb9
      writer = fixtureIdentity "definition admission writer" mkNablaId 0xba
      carrier = carrierFor SortDefinitionRole
      descriptor = primordialReplicaDescriptor carrier
      graph = Graph.graphStateForReachabilityTest [NablaVertex writer] []
  admission <- checked "definition admission id" (deriveHeraldAdmissionId (controlIndex 1))
  successor <- checked "definition admission generation" (admitHeraldMembershipGeneration (controlIndex 2) admission newcomer fixtureHeraldMembershipGeneration)
  route <-
    checked
      "definition route after admission"
      (ApplicationPublication.deriveApplicationPublicationRoute fixtureCheckedGenesis successor descriptor writer graph (Placement.initialState local))
  let expected =
        routeDestination
          (deriveSystemViewDeltaId (checkedSystemId fixtureCheckedGenesis) newcomer SortDefinitionRole)
          (deriveSystemViewStoreIncarnationId (checkedSystemId fixtureCheckedGenesis) newcomer SortDefinitionRole)
          newcomer
          Normal
  assertBool "new private view is a mandatory destination" (expected `elem` routeDestinations route)
  assertEqual "private delivery does not rely on an exposing graph edge" [] (Graph.graphReachableDeltas writer graph)

caseControlledLifecycle :: Assertion
caseControlledLifecycle = do
  fixture <- controlledFixture
  firstRequest <-
    checked
      "controlled first request"
      ( firstPublicationRequest
          fixture.applicationState
          fixture.session
          fixture.binding
          fixture.request
          fixture.privateNabla
          fixture.currentValue
      )
  first <- checked "controlled first" (prepareControlled fixture firstRequest fixture.controlled fixture.publications fixture.stores fixture.stream)
  let (applicationFirst, controlledFirst, registryFirst, publicationsFirst, storesFirst, streamFirst, firstOutcome) =
        commitLocalApplicationPublication first
      firstRecord = localApplicationPublicationRecord firstOutcome
  assertEqual
    "reservation derives first-use"
    (ControlledFirstUseApplicationPublicationOrigin fixture.generated)
    (Publication.applicationPublicationOrigin firstRecord)
  assertBool "first-use possession" (Controlled.controlledHasNormalPossession fixture.process fixture.objectId controlledFirst)
  firstWitness <-
    requirePublicationRequestWitness
      fixture.request
      fixture.session
      applicationFirst
  assertBool
    "whole-state request position links the controlled-current first use"
    ( ApplicationState.applicationRequestWitnessPosition firstWitness
        == Just (Publication.applicationPublicationAcceptancePosition firstRecord)
    )
  assertEqual
    "whole-state request retains the exact controlled-current call"
    (WriteApplication fixture.privateNabla (PublishValue fixture.currentValue))
    (ApplicationState.applicationRequestWitnessCall firstWitness)
  updateRequest <-
    checked
      "controlled update request"
      ( firstPublicationRequest
          applicationFirst
          fixture.session
          fixture.binding
          (requestId 2)
          fixture.privateNabla
          fixture.obsoleteValue
      )
  update <- checked "controlled update" (prepareControlled fixture updateRequest controlledFirst publicationsFirst storesFirst streamFirst)
  let (applicationUpdate, controlledUpdate, registryUpdate, publicationsUpdate, _storesUpdate, _streamUpdate, updateOutcome) =
        commitLocalApplicationPublication update
      updateRecord = localApplicationPublicationRecord updateOutcome
      observation =
        checkedPure
          "update observation"
          (Controlled.checkControlledObservation fixture.descriptor fixture.occurrence (Publication.applicationPublicationChecked updateRecord))
  assertEqual
    "local record derives update"
    (ControlledUpdateApplicationPublicationOrigin fixture.objectId)
    (Publication.applicationPublicationOrigin updateRecord)
  assertEqual
    "retained canonical pair replaces supplied Void owner and nonzero generation"
    (Just ((Domain.ProcessLabel fixture.process, 0)))
    (Controlled.checkedControlledObservationLabel observation)
  assertEqual
    "obsolete retention names suppression"
    (Just fixture.objectId)
    (Publication.applicationPublicationSuppression (Publication.applicationPublicationRetention updateRecord))
  controlledRecord <- requireControlledRecord fixture.objectId controlledUpdate
  assertBool "suppression already installed" (Controlled.controlledRecordSuppression controlledRecord /= Controlled.ControlledUnsuppressed)
  assertBool "ordinary controlled value leaves registry" (registryUpdate == registryFirst)
  updateWitness <-
    requirePublicationRequestWitness
      (requestId 2)
      fixture.session
      applicationUpdate
  assertBool
    "whole-state request position links the controlled-obsolete update"
    ( ApplicationState.applicationRequestWitnessPosition updateWitness
        == Just (Publication.applicationPublicationAcceptancePosition updateRecord)
    )
  assertEqual
    "whole-state request retains the exact controlled-obsolete call"
    (WriteApplication fixture.privateNabla (PublishValue fixture.obsoleteValue))
    (ApplicationState.applicationRequestWitnessCall updateWitness)
  case ApplicationState.classifyApplicationRequest
    fixture.session
    fixture.binding
    (requestId 2)
    (WriteApplication fixture.privateNabla (PublishValue fixture.obsoleteValue))
    applicationUpdate of
    Right (ApplicationState.RetainedApplicationRequest _) -> pure ()
    Left problem -> assertFailure ("controlled retry classification failed: " <> show problem)
    Right _ -> assertFailure "controlled retry did not classify as retained"
  assertEqual
    "controlled retry leaves the committed semantic record reachable"
    (Just updateRecord)
    ( Publication.lookupApplicationPublication
        (Publication.applicationPublicationAcceptancePosition updateRecord)
        publicationsUpdate
    )

caseNegativeRollbackMatrix :: Assertion
caseNegativeRollbackMatrix = do
  structural <- structuralFixture
  sourceRequest <-
    checked
      "source failure request"
      ( firstPublicationRequest
          structural.applicationState
          structural.session
          structural.binding
          structural.request
          structural.privateNabla
          structural.value
      )
  assertLocalPublicationFailure
    "missing bootstrap operate source"
    ( \case
        LocalApplicationPublicationAccessError
          ( ApplicationPublicationBootstrapOperateError
              (Controlled.ControlledBootstrapOperateProcessUnavailable process)
            ) -> process == structural.process
        _ -> False
    )
    ( prepareLocalApplicationPublication
        fixtureCheckedGenesis
        (ApplicationState.processRoleView [structural.process])
        sourceRequest
        Controlled.emptyState
        structural.registry
        Graph.emptyState
        (Placement.initialState structural.localHerald)
        structural.publications
        structural.stores
        structural.stream
    )
  assertStillFirst
    "source failure"
    structural.applicationState
    structural.session
    structural.binding
    structural.request
    structural.privateNabla
    structural.value

  let unknownSort =
        fixtureIdentity "unregistered effective writer sort" mkSortId 0xd9
      wrongOccurrence =
        fixtureIdentity
          "wrong effective writer occurrence"
          mkSortDefinitionOccurrenceId
          0xda
      sourceWith sortId occurrence =
        controlledSourceState
          structural.process
          (fixtureIdentity "effective-source process id" mkProcessId 0xdb)
          structural.localHerald
          NeutralVertexRole
          sortId
          occurrence
          structural.nabla
          structural.authority
          structural.controlPrerequisite
      effectiveSourceRows =
        [ ( "writer sort is not effective",
            sourceWith unknownSort structural.occurrence,
            \case
              LocalApplicationPublicationAccessError
                (ApplicationPublicationEffectiveSortUnavailable sortId) ->
                  sortId == unknownSort
              _ -> False
          ),
          ( "writer occurrence differs from the effective occurrence",
            sourceWith
              (primordialReplicaSortId (carrierFor NeutralVertexRole))
              wrongOccurrence,
            \case
              LocalApplicationPublicationAccessError
                (ApplicationPublicationWriterOccurrenceMismatch supplied effective) ->
                  supplied == wrongOccurrence
                    && effective == structural.occurrence
              _ -> False
          )
        ]
  mapM_
    ( \(description, badControlled, matches) -> do
        assertLocalPublicationFailure
          description
          matches
          ( prepareLocalApplicationPublication
              fixtureCheckedGenesis
              (ApplicationState.processRoleView [structural.process])
              sourceRequest
              badControlled
              structural.registry
              Graph.emptyState
              (Placement.initialState structural.localHerald)
              structural.publications
              structural.stores
              structural.stream
          )
        assertStillFirst
          description
          structural.applicationState
          structural.session
          structural.binding
          structural.request
          structural.privateNabla
          structural.value
    )
    effectiveSourceRows

  let managedCarrier = carrierFor ProcessEpochRole
      managedWriter =
        fixtureIdentity "Herald-managed application writer" mkNablaId 0xdc
      managedOccurrence = primordialReplicaOccurrenceId managedCarrier
      managedSort = primordialReplicaSortId managedCarrier
      (managedApplication, managedPrivateWriter, managedSession, managedBinding) =
        publicationApplication
          structural.localHerald
          structural.process
          ProcessEpochRole
          managedWriter
      managedSource =
        controlledSourceState
          structural.process
          (fixtureIdentity "Herald-managed source process id" mkProcessId 0xdd)
          structural.localHerald
          ProcessEpochRole
          managedSort
          managedOccurrence
          managedWriter
          structural.authority
          structural.controlPrerequisite
  managedRequest <-
    checked
      "Herald-managed mutation request"
      ( firstPublicationRequest
          managedApplication
          managedSession
          managedBinding
          structural.request
          managedPrivateWriter
          structural.value
      )
  assertLocalPublicationFailure
    "Herald-managed writer mutation policy"
    ( \case
        LocalApplicationPublicationAccessError
          (ApplicationPublicationMutationUnavailable sortId) ->
            sortId == managedSort
        _ -> False
    )
    ( prepareLocalApplicationPublication
        fixtureCheckedGenesis
        (ApplicationState.processRoleView [structural.process])
        managedRequest
        managedSource
        structural.registry
        Graph.emptyState
        (Placement.initialState structural.localHerald)
        structural.publications
        structural.stores
        structural.stream
    )
  assertStillFirst
    "Herald-managed mutation failure"
    managedApplication
    managedSession
    managedBinding
    structural.request
    managedPrivateWriter
    structural.value

  let hiddenWriter = fixtureIdentity "hidden application writer" mkNablaId 0xd0
      hiddenPrivateNabla =
        asPrivateNablaId
          (checkedPure "hidden private writer" (mkPrivateUniqueId 999))
      (hiddenApplication, _, hiddenSession, hiddenBinding) =
        publicationApplication
          structural.localHerald
          structural.process
          NeutralVertexRole
          hiddenWriter
  hiddenWriterRequest <-
    checked
      "hidden writer request"
      ( firstPublicationRequest
          hiddenApplication
          hiddenSession
          hiddenBinding
          structural.request
          hiddenPrivateNabla
          structural.value
      )
  assertLocalPublicationFailure
    "application cannot name a hidden bootstrap writer"
    ( \case
        LocalApplicationPublicationAccessError
          (ApplicationPublicationWriterIdentityError _) -> True
        _ -> False
    )
    ( prepareLocalApplicationPublication
        fixtureCheckedGenesis
        (ApplicationState.processRoleView [structural.process])
        hiddenWriterRequest
        structural.controlled
        structural.registry
        Graph.emptyState
        (Placement.initialState structural.localHerald)
        structural.publications
        structural.stores
        structural.stream
    )
  assertStillFirst
    "hidden writer failure"
    hiddenApplication
    hiddenSession
    hiddenBinding
    structural.request
    hiddenPrivateNabla
    structural.value

  let sourceWithoutReservation =
        controlledSourceState
          structural.process
          (fixtureIdentity "reservation failure process id" mkProcessId 0xd1)
          structural.localHerald
          NeutralVertexRole
          (primordialReplicaSortId (carrierFor NeutralVertexRole))
          structural.occurrence
          structural.nabla
          structural.authority
          structural.controlPrerequisite
  assertLocalPublicationFailure
    "missing first-use reservation"
    ( \case
        LocalApplicationPublicationAccessError
          (ApplicationPublicationReservationUnavailable generated) ->
            generated == structural.generated
        _ -> False
    )
    ( prepareLocalApplicationPublication
        fixtureCheckedGenesis
        (ApplicationState.processRoleView [structural.process])
        sourceRequest
        sourceWithoutReservation
        structural.registry
        Graph.emptyState
        (Placement.initialState structural.localHerald)
        structural.publications
        structural.stores
        structural.stream
    )
  assertStillFirst
    "reservation failure"
    structural.applicationState
    structural.session
    structural.binding
    structural.request
    structural.privateNabla
    structural.value

  -- Bootstrap role admission and normal possession are atomic and no profile
  -- 0.1 transition removes that possession.  Consequently the explicit
  -- NotPossessed guard is an invariant-fault guard; the reachable writer
  -- authorization failure is a writer owned by another process.
  let foreignProcess =
        fixtureIdentity "foreign writer process" mkProcessEpochId 0xde
      foreignWriter = fixtureIdentity "foreign writer" mkNablaId 0xdf
      foreignWriterSource =
        Controlled.commitControlledBootstrap
          ( checkedPure
              "foreign writer source"
              ( Controlled.prepareControlledBootstrap
                  ( Controlled.processFact
                      (fixtureIdentity "foreign writer process id" mkProcessId 0xe1)
                      foreignProcess
                      structural.localHerald
                      structural.authority
                  )
                  [ Controlled.writerRootFact
                      foreignProcess
                      NeutralVertexRole
                      (primordialReplicaSortId (carrierFor NeutralVertexRole))
                      structural.occurrence
                      structural.authority
                      structural.controlPrerequisite
                      foreignWriter
                      UnsequencedNabla
                  ]
                  sourceWithoutReservation
              )
          )
      (foreignWriterApplication, foreignPrivateUnique) =
        localizeUniqueId
          structural.process
          ( globalUniqueIdFromGlobalObjectId
              (globalObjectIdFromNablaId foreignWriter)
          )
          structural.applicationState
      foreignPrivateWriter = asPrivateNablaId foreignPrivateUnique
  foreignWriterRequest <-
    checked
      "foreign writer request"
      ( firstPublicationRequest
          foreignWriterApplication
          structural.session
          structural.binding
          structural.request
          foreignPrivateWriter
          structural.value
      )
  assertLocalPublicationFailure
    "writer belongs to another process"
    ( \case
        LocalApplicationPublicationAccessError
          ( ApplicationPublicationBootstrapOperateError
              ( Controlled.ControlledBootstrapOperateWriterProcessMismatch
                  requested
                  owner
                )
            ) ->
            requested == structural.process && owner == foreignProcess
        _ -> False
    )
    ( prepareLocalApplicationPublication
        fixtureCheckedGenesis
        (ApplicationState.processRoleView [structural.process])
        foreignWriterRequest
        foreignWriterSource
        structural.registry
        Graph.emptyState
        (Placement.initialState structural.localHerald)
        structural.publications
        structural.stores
        structural.stream
    )
  assertStillFirst
    "foreign writer failure"
    foreignWriterApplication
    structural.session
    structural.binding
    structural.request
    foreignPrivateWriter
    structural.value

  let wrongReservationProcess =
        fixtureIdentity "wrong reservation process" mkProcessEpochId 0xd7
      wrongReservationNabla =
        fixtureIdentity "wrong reservation writer" mkNablaId 0xd8
      wrongReservationAuthority = testStructuralAuthority
      reservedWith owner writer retainedAuthority =
        Controlled.commitControlledReservation
          ( Controlled.prepareControlledReservation
              owner
              writer
              (primordialReplicaSortId (carrierFor NeutralVertexRole))
              retainedAuthority
              structural.generated
              sourceWithoutReservation
          )
      reservationRows =
        [ ( "reservation owned by another process",
            reservedWith
              wrongReservationProcess
              structural.nabla
              structural.authority,
            \case
              LocalApplicationPublicationAccessError
                (ApplicationPublicationReservationBindingMismatch generated process writer) ->
                  generated == structural.generated
                    && process == structural.process
                    && writer == structural.nabla
              _ -> False
          ),
          ( "reservation bound to another writer",
            reservedWith
              structural.process
              wrongReservationNabla
              structural.authority,
            \case
              LocalApplicationPublicationAccessError
                (ApplicationPublicationReservationBindingMismatch generated process writer) ->
                  generated == structural.generated
                    && process == structural.process
                    && writer == structural.nabla
              _ -> False
          ),
          ( "reservation carries another authority",
            reservedWith
              structural.process
              structural.nabla
              wrongReservationAuthority,
            \case
              LocalApplicationPublicationAccessError
                (ApplicationPublicationReservationAuthorityMismatch generated retained required) ->
                  generated == structural.generated
                    && retained == wrongReservationAuthority
                    && required == structural.authority
              _ -> False
          )
        ]
  mapM_
    ( \(description, badControlled, matches) -> do
        let before =
              localPublicationOwnerWitness
                structural.applicationState
                badControlled
                structural.registry
                structural.publications
                structural.stores
                structural.stream
        assertLocalPublicationFailure
          description
          matches
          ( prepareLocalApplicationPublication
              fixtureCheckedGenesis
              (ApplicationState.processRoleView [structural.process])
              sourceRequest
              badControlled
              structural.registry
              Graph.emptyState
              (Placement.initialState structural.localHerald)
              structural.publications
              structural.stores
              structural.stream
          )
        assertBool
          (description <> ": input owners changed")
          ( before
              == localPublicationOwnerWitness
                structural.applicationState
                badControlled
                structural.registry
                structural.publications
                structural.stores
                structural.stream
          )
        assertStillFirst
          description
          structural.applicationState
          structural.session
          structural.binding
          structural.request
          structural.privateNabla
          structural.value
    )
    reservationRows

  immutable <- controlledFixtureFor immutableControlledSortDefinition
  immutableFirstRequest <-
    checked
      "immutable first request"
      ( firstPublicationRequest
          immutable.applicationState
          immutable.session
          immutable.binding
          immutable.request
          immutable.privateNabla
          immutable.currentValue
      )
  immutableFirst <-
    checked
      "immutable first publication"
      ( prepareControlled
          immutable
          immutableFirstRequest
          immutable.controlled
          immutable.publications
          immutable.stores
          immutable.stream
      )
  let (immutableApplication, immutableControlled, _, immutablePublications, immutableStores, immutableStream, _) =
        commitLocalApplicationPublication immutableFirst
  immutableUpdateRequest <-
    checked
      "immutable update request"
      ( firstPublicationRequest
          immutableApplication
          immutable.session
          immutable.binding
          (requestId 2)
          immutable.privateNabla
          immutable.currentValue
      )
  assertLocalPublicationFailure
    "immutable controlled update"
    ( \case
        LocalApplicationPublicationAccessError
          (ApplicationPublicationImmutableUpdate sortId) ->
            sortId == descriptorSortId immutable.descriptor
        _ -> False
    )
    ( prepareControlled
        immutable
        immutableUpdateRequest
        immutableControlled
        immutablePublications
        immutableStores
        immutableStream
    )
  assertStillFirst
    "immutable update failure"
    immutableApplication
    immutable.session
    immutable.binding
    (requestId 2)
    immutable.privateNabla
    immutable.currentValue

  mutable <- controlledFixture
  -- An existing object cannot be rewritten through an immutable structural
  -- carrier, even when its retained sort differs from that carrier.  The
  -- immutable-write boundary rejects this before controlled update admission.
  let differentClassBase =
        peerControlledRecord mutable ((Domain.ProcessLabel mutable.process, 0))
      differentClassControlled =
        Controlled.commitControlledBootstrap
          ( checkedPure
              "different structural class writer"
              ( Controlled.prepareControlledBootstrap
                  ( Controlled.processFact
                      (fixtureIdentity "different-class process id" mkProcessId 0xe2)
                      structural.process
                      structural.localHerald
                      structural.authority
                  )
                  [ Controlled.writerRootFact
                      structural.process
                      NeutralVertexRole
                      (primordialReplicaSortId (carrierFor NeutralVertexRole))
                      structural.occurrence
                      structural.authority
                      structural.controlPrerequisite
                      structural.nabla
                      UnsequencedNabla
                  ]
                  differentClassBase
              )
          )
      (differentClassApplication, differentClassPrivateObject) =
        localizeUniqueId
          structural.process
          mutable.generated
          structural.applicationState
      differentClassValue =
        Application.RecordValue
          ( Map.fromList
              [ ("label", Application.LabelValue (Application.VoidLabel, 0)),
                ( "object_id",
                  Application.UniqueIdValue differentClassPrivateObject
                )
              ]
          )
  differentClassRequest <-
    checked
      "different structural class request"
      ( firstPublicationRequest
          differentClassApplication
          structural.session
          structural.binding
          structural.request
          structural.privateNabla
          differentClassValue
      )
  assertLocalPublicationFailure
    "existing object belongs to a different structural class"
    ( \case
        LocalApplicationPublicationAccessError
          (ApplicationPublicationImmutableUpdate observed) ->
            observed == primordialReplicaSortId (carrierFor NeutralVertexRole)
        _ -> False
    )
    ( prepareLocalApplicationPublication
        fixtureCheckedGenesis
        (ApplicationState.processRoleView [structural.process])
        differentClassRequest
        differentClassControlled
        structural.registry
        Graph.emptyState
        (Placement.initialState structural.localHerald)
        structural.publications
        structural.stores
        structural.stream
    )
  assertStillFirst
    "different structural class failure"
    differentClassApplication
    structural.session
    structural.binding
    structural.request
    structural.privateNabla
    differentClassValue

  let edgeReferenceWriter =
        fixtureIdentity "obsolete-reference edge writer" mkNablaId 0xe3
      edgeCarrier = carrierFor EdgeRole
      mutableSourceWithEdge =
        Controlled.commitControlledBootstrap
          ( checkedPure
              "controlled source with edge writer"
              ( Controlled.prepareControlledBootstrap
                  ( Controlled.processFact
                      (fixtureIdentity "controlled edge process id" mkProcessId 0xe4)
                      mutable.process
                      mutable.localHerald
                      mutable.authority
                  )
                  [ Controlled.writerRootFact
                      mutable.process
                      NeutralVertexRole
                      (descriptorSortId mutable.descriptor)
                      mutable.occurrence
                      mutable.authority
                      (controlIndex 6)
                      mutable.nabla
                      UnsequencedNabla,
                    Controlled.writerRootFact
                      mutable.process
                      EdgeRole
                      (primordialReplicaSortId edgeCarrier)
                      (primordialReplicaOccurrenceId edgeCarrier)
                      mutable.authority
                      (controlIndex 6)
                      edgeReferenceWriter
                      UnsequencedNabla
                  ]
                  Controlled.emptyState
              )
          )
      mutableControlledWithEdge =
        Controlled.commitControlledReservation
          ( Controlled.prepareControlledReservation
              mutable.process
              mutable.nabla
              (descriptorSortId mutable.descriptor)
              mutable.authority
              mutable.generated
              mutableSourceWithEdge
          )

  currentRequest <-
    checked
      "current lifecycle request"
      ( firstPublicationRequest
          mutable.applicationState
          mutable.session
          mutable.binding
          mutable.request
          mutable.privateNabla
          mutable.currentValue
      )
  current <-
    checked
      "current lifecycle publication"
      ( prepareControlled
          mutable
          currentRequest
          mutableControlledWithEdge
          mutable.publications
          mutable.stores
          mutable.stream
      )
  let (currentApplication, currentControlled, _, currentPublications, currentStores, currentStream, _) =
        commitLocalApplicationPublication current
  obsoleteRequest <-
    checked
      "obsolete lifecycle request"
      ( firstPublicationRequest
          currentApplication
          mutable.session
          mutable.binding
          (requestId 2)
          mutable.privateNabla
          mutable.obsoleteValue
      )
  obsolete <-
    checked
      "obsolete lifecycle publication"
      ( prepareControlled
          mutable
          obsoleteRequest
          currentControlled
          currentPublications
          currentStores
          currentStream
      )
  let (obsoleteApplication, obsoleteControlled, _, obsoletePublications, obsoleteStores, obsoleteStream, _) =
        commitLocalApplicationPublication obsolete
  resurrectionRequest <-
    checked
      "current-after-obsolete request"
      ( firstPublicationRequest
          obsoleteApplication
          mutable.session
          mutable.binding
          (requestId 3)
          mutable.privateNabla
          mutable.currentValue
      )
  let obsoleteOwners =
        localPublicationOwnerWitness
          obsoleteApplication
          obsoleteControlled
          mutable.registry
          obsoletePublications
          obsoleteStores
          obsoleteStream
  assertLocalPublicationFailure
    "current update after obsolete"
    ( \case
        LocalApplicationPublicationUpdateError
          (Controlled.ControlledUpdateNotCurrent object Controlled.ControlledObsolete) ->
            object == mutable.objectId
        _ -> False
    )
    ( prepareControlled
        mutable
        resurrectionRequest
        obsoleteControlled
        obsoletePublications
        obsoleteStores
        obsoleteStream
    )
  assertBool
    "current-after-obsolete changed an input owner"
    ( obsoleteOwners
        == localPublicationOwnerWitness
          obsoleteApplication
          obsoleteControlled
          mutable.registry
          obsoletePublications
          obsoleteStores
          obsoleteStream
    )
  assertStillFirst
    "current-after-obsolete failure"
    obsoleteApplication
    mutable.session
    mutable.binding
    (requestId 3)
    mutable.privateNabla
    mutable.currentValue

  let edgeGenerated =
        fixtureIdentity "obsolete-reference edge object" mkGlobalUniqueId 0xe5
      (edgeWriterApplication, edgePrivateWriterUnique) =
        localizeUniqueId
          mutable.process
          ( globalUniqueIdFromGlobalObjectId
              (globalObjectIdFromNablaId edgeReferenceWriter)
          )
          obsoleteApplication
      edgePrivateWriter = asPrivateNablaId edgePrivateWriterUnique
      (edgeObjectApplication, edgePrivateObject) =
        localizeUniqueId mutable.process edgeGenerated edgeWriterApplication
      (obsoleteReferenceApplication, privateObsoleteReference) =
        localizeUniqueId
          mutable.process
          mutable.generated
          edgeObjectApplication
      obsoleteReferenceValue =
        Application.RecordValue
          ( Map.fromList
              [ ( "destination_vertex",
                  Application.UniqueIdValue privateObsoleteReference
                ),
                ("label", Application.LabelValue (Application.VoidLabel, 0)),
                ("object_id", Application.UniqueIdValue edgePrivateObject),
                ( "source_vertex",
                  Application.UniqueIdValue
                    (privateNablaUniqueId edgePrivateWriter)
                ),
                ("strength", Application.EnumValue "preserve")
              ]
          )
      obsoleteWithEdgeReservation =
        Controlled.commitControlledReservation
          ( Controlled.prepareControlledReservation
              mutable.process
              edgeReferenceWriter
              (primordialReplicaSortId edgeCarrier)
              mutable.authority
              edgeGenerated
              obsoleteControlled
          )
  obsoleteReferenceRequest <-
    checked
      "obsolete structural reference request"
      ( firstPublicationRequest
          obsoleteReferenceApplication
          mutable.session
          mutable.binding
          (requestId 4)
          edgePrivateWriter
          obsoleteReferenceValue
      )
  assertBool
    "obsolete reference retains normal name possession"
    ( Controlled.controlledHasNormalPossession
        mutable.process
        mutable.objectId
        obsoleteWithEdgeReservation
    )
  assertLocalPublicationFailure
    "possessed obsolete object is no longer a current edge vertex"
    ( \case
        LocalApplicationPublicationAccessError
          (ApplicationPublicationStructuralReferenceNotCurrentVertex object) ->
            object == mutable.objectId
        _ -> False
    )
    ( prepareLocalApplicationPublication
        fixtureCheckedGenesis
        (ApplicationState.processRoleView [mutable.process])
        obsoleteReferenceRequest
        obsoleteWithEdgeReservation
        mutable.registry
        Graph.emptyState
        (Placement.initialState mutable.localHerald)
        obsoletePublications
        obsoleteStores
        obsoleteStream
    )
  assertStillFirst
    "obsolete structural reference failure"
    obsoleteReferenceApplication
    mutable.session
    mutable.binding
    (requestId 4)
    edgePrivateWriter
    obsoleteReferenceValue

  let peerOnlyControlled =
        peerControlledRecord
          mutable
          ((Domain.ProcessLabel mutable.process, 0))
      peerOwnerBefore =
        localPublicationOwnerWitness
          mutable.applicationState
          peerOnlyControlled
          mutable.registry
          mutable.publications
          mutable.stores
          mutable.stream
  unpossessedUpdate <-
    checked
      "unpossessed update request"
      ( firstPublicationRequest
          mutable.applicationState
          mutable.session
          mutable.binding
          mutable.request
          mutable.privateNabla
          mutable.currentValue
      )
  assertLocalPublicationFailure
    "controlled update without normal target possession"
    ( \case
        LocalApplicationPublicationUpdateError
          (Controlled.ControlledUpdateObjectNotPossessed process object) ->
            process == mutable.process && object == mutable.objectId
        _ -> False
    )
    ( prepareControlled
        mutable
        unpossessedUpdate
        peerOnlyControlled
        mutable.publications
        mutable.stores
        mutable.stream
    )
  assertBool
    "unpossessed update changed an input owner"
    ( peerOwnerBefore
        == localPublicationOwnerWitness
          mutable.applicationState
          peerOnlyControlled
          mutable.registry
          mutable.publications
          mutable.stores
          mutable.stream
    )
  assertStillFirst
    "unpossessed update failure"
    mutable.applicationState
    mutable.session
    mutable.binding
    mutable.request
    mutable.privateNabla
    mutable.currentValue

  let unavailableLabelProcess =
        fixtureIdentity "unavailable retained-label process" mkProcessEpochId 0xc6
      unavailableLabelControlled =
        possessedPeerControlledRecord
          mutable
          ((Domain.ProcessLabel unavailableLabelProcess, 0))
      unavailableLabelOwners =
        localPublicationOwnerWitness
          mutable.applicationState
          unavailableLabelControlled
          mutable.registry
          mutable.publications
          mutable.stores
          mutable.stream
  unavailableLabelUpdate <-
    checked
      "unavailable retained-label update request"
      ( firstPublicationRequest
          mutable.applicationState
          mutable.session
          mutable.binding
          mutable.request
          mutable.privateNabla
          mutable.currentValue
      )
  assertLocalPublicationFailure
    "controlled update whose latest label is not operable"
    ( \case
        LocalApplicationPublicationUpdateError
          (Controlled.ControlledUpdateLatestLabelUnavailable retained process) ->
            retained == Just ((Domain.ProcessLabel unavailableLabelProcess, 0))
              && process == mutable.process
        _ -> False
    )
    ( prepareControlled
        mutable
        unavailableLabelUpdate
        unavailableLabelControlled
        mutable.publications
        mutable.stores
        mutable.stream
    )
  assertBool
    "unavailable-label update changed an input owner"
    ( unavailableLabelOwners
        == localPublicationOwnerWitness
          mutable.applicationState
          unavailableLabelControlled
          mutable.registry
          mutable.publications
          mutable.stores
          mutable.stream
    )
  assertStillFirst
    "unavailable-label update failure"
    mutable.applicationState
    mutable.session
    mutable.binding
    mutable.request
    mutable.privateNabla
    mutable.currentValue

  edge <- edgeFailureFixture
  edgeRequest <-
    checked
      "edge failure request"
      ( firstPublicationRequest
          edge.applicationState
          edge.session
          edge.binding
          edge.request
          edge.privateNabla
          edge.value
      )
  assertLocalPublicationFailure
    "edge endpoint without name possession"
    ( \case
        LocalApplicationPublicationAccessError
          (ApplicationPublicationStructuralReferenceNotPossessed process object) ->
            process == edge.process && object == edge.missingEndpoint
        _ -> False
    )
    ( prepareLocalApplicationPublication
        fixtureCheckedGenesis
        (ApplicationState.processRoleView [edge.process])
        edgeRequest
        edge.controlled
        edge.registry
        Graph.emptyState
        (Placement.initialState edge.localHerald)
        edge.publications
        edge.stores
        edge.stream
    )
  assertStillFirst
    "edge name failure"
    edge.applicationState
    edge.session
    edge.binding
    edge.request
    edge.privateNabla
    edge.value

  let processObject = globalObjectIdFromProcessEpochId edge.process
      (possessedNonVertexApplication, privateProcessObject) =
        localizeUniqueId
          edge.process
          (globalUniqueIdFromGlobalObjectId processObject)
          edge.applicationState
      possessedNonVertexValue = case edge.value of
        Application.RecordValue fields ->
          Application.RecordValue
            ( Map.insert
                "destination_vertex"
                (Application.UniqueIdValue privateProcessObject)
                fields
            )
        _ -> error "edge fixture value is not a record"
  possessedNonVertexRequest <-
    checked
      "possessed non-vertex edge request"
      ( firstPublicationRequest
          possessedNonVertexApplication
          edge.session
          edge.binding
          edge.request
          edge.privateNabla
          possessedNonVertexValue
      )
  assertBool
    "the rejected process endpoint is normally possessed"
    (Controlled.controlledHasNormalPossession edge.process processObject edge.controlled)
  assertLocalPublicationFailure
    "possessed process object is not an admissible edge vertex"
    ( \case
        LocalApplicationPublicationAccessError
          (ApplicationPublicationStructuralReferenceNotCurrentVertex object) ->
            object == processObject
        _ -> False
    )
    ( prepareLocalApplicationPublication
        fixtureCheckedGenesis
        (ApplicationState.processRoleView [edge.process])
        possessedNonVertexRequest
        edge.controlled
        edge.registry
        Graph.emptyState
        (Placement.initialState edge.localHerald)
        edge.publications
        edge.stores
        edge.stream
    )
  assertStillFirst
    "possessed non-vertex edge failure"
    possessedNonVertexApplication
    edge.session
    edge.binding
    edge.request
    edge.privateNabla
    possessedNonVertexValue

propMixedOrdering :: [Bool] -> Property
propMixedOrdering choices = ioProperty $ do
  fixture <- ordinaryFixture
  case runTrace fixture choices of
    Left problem -> pure (counterexample problem False)
    Right (publications, accepted) -> do
      let records = fmap snd (Publication.applicationPublicationEntries publications)
          positions = fmap (Publication.heraldPublicationPositionWord64 . Publication.applicationPublicationHeraldPosition) records
          sequences = fmap (nablaSequenceWord64 . publicationNablaSequence . Publication.applicationPublicationId) records
          nextPosition =
            Publication.heraldPublicationPositionWord64
              (Publication.publicationWitnessNextHeraldPosition (Publication.publicationStateWitness publications))
          expectedPositions = [1 .. fromIntegral accepted]
          expectedSequences = [0 .. fromIntegral accepted - 1]
          valid = positions == expectedPositions && sequences == expectedSequences && nextPosition == fromIntegral accepted + 1
      pure (counterexample (show (positions, sequences, nextPosition)) valid)

data OrdinaryFixture = OrdinaryFixture
  { localHerald :: HeraldEpoch,
    remoteHerald :: HeraldEpoch,
    process :: ProcessEpochId,
    nabla :: NablaId,
    authority :: AuthorityEpoch,
    position :: ProcessAcceptancePosition,
    session :: ApplicationSessionId,
    binding :: ApplicationSessionBinding,
    privateNabla :: PrivateNablaId,
    request :: RequestId,
    occurrence :: SortDefinitionOccurrenceId,
    controlPrerequisite :: ControlIndex,
    value :: Application.ApplicationValue,
    route :: FrozenRoute,
    localSlot :: Store.StoreSlot,
    applicationState :: ApplicationState.State,
    controlled :: Controlled.State,
    registry :: SortRegistry.State,
    graph :: Graph.State,
    placement :: Placement.State,
    publications :: Publication.State,
    stores :: Store.State,
    stream :: PeerStream.State PeerLogicalPayload
  }

ordinaryFixture :: IO OrdinaryFixture
ordinaryFixture = do
  let localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis
      remoteHerald = case filter (/= localHerald) (checkedActiveHeraldEpochs fixtureCheckedGenesis) of
        remote : _ -> remote
        [] -> error "ordinary publication fixture requires a remote Herald"
      process = fixtureIdentity "ordinary process" mkProcessEpochId 0xa2
      nabla = fixtureIdentity "ordinary writer" mkNablaId 0xa3
      authority = genesisAuthorityEpoch
      position = processAcceptancePosition process 1
      request = requestId 1
      controlPrerequisite = controlIndex 4
      carrier = carrierFor SortDefinitionRole
      occurrence = primordialReplicaOccurrenceId carrier
      stores = checkedPure "initial Store" (Store.initialState fixtureCheckedGenesis)
      localSlot = storeSlotFor carrier stores
      remoteDelta = fixtureIdentity "remote Delta" mkDeltaId 0xa4
      remoteIncarnation = fixtureIdentity "remote incarnation" mkStoreIncarnationId 0xa5
      graph =
        Graph.graphStateForReachabilityTest
          [NablaVertex nabla, DeltaVertex (Store.storeSlotDelta localSlot), DeltaVertex remoteDelta]
          [ edgePayload (NablaVertex nabla) (DeltaVertex (Store.storeSlotDelta localSlot)) Preserve,
            edgePayload (NablaVertex nabla) (DeltaVertex remoteDelta) Weaken
          ]
      localPlacement =
        Placement.commitSystemViewPlacements
          ( checkedPure
              "local placement"
              ( Placement.prepareSystemViewPlacements
                  [ Placement.systemViewPlacement
                      SortDefinitionRole
                      (Store.storeSlotDelta localSlot)
                      (Store.storeSlotSortId localSlot)
                      occurrence
                      localHerald
                      (Store.storeSlotIncarnation localSlot)
                  ]
                  (Placement.initialState localHerald)
              )
          )
      remoteRoute =
        PlacementMessage.privateSystemViewDeltaRoute
          SortDefinitionRole
          remoteDelta
          (primordialReplicaSortId carrier)
          occurrence
          remoteIncarnation
      snapshot = checkedPure "remote snapshot" (PlacementMessage.placementSnapshot remoteHerald PlacementMessage.firstPlacementSequence [remoteRoute])
      (placement, _) =
        Placement.commitRemotePlacement
          ( checkedPure
              "remote placement"
              (Placement.prepareRemotePlacement (PlacementMessage.FullPlacementSnapshot snapshot) localPlacement)
          )
      controlled =
        controlledSourceState
          process
          (fixtureIdentity "ordinary process id" mkProcessId 0xa6)
          localHerald
          SortDefinitionRole
          (primordialReplicaSortId carrier)
          occurrence
          nabla
          authority
          controlPrerequisite
      route =
        checkedPure
          "derived route"
          (ApplicationPublication.deriveApplicationPublicationRoute fixtureCheckedGenesis fixtureHeraldMembershipGeneration (primordialReplicaDescriptor carrier) nabla graph placement)
      (applicationState, privateNabla, session, binding) =
        publicationApplication
          localHerald
          process
          SortDefinitionRole
          nabla
  pure
    OrdinaryFixture
      { localHerald,
        remoteHerald,
        process,
        nabla,
        authority,
        position,
        session,
        binding,
        privateNabla,
        request,
        occurrence,
        controlPrerequisite,
        value = Application.SortDefinitionValue declaredSortDefinition,
        route,
        localSlot,
        applicationState,
        controlled,
        registry = SortRegistry.initialState fixtureCheckedGenesis,
        graph,
        placement,
        publications = Publication.initialState localHerald,
        stores,
        stream = PeerStream.initialState localHerald
      }

prepareLocalApplicationPublication ::
  CheckedHeraldGenesis ->
  ApplicationState.ProcessRoleView ->
  ApplicationState.ApplicationRequestCandidate ->
  Controlled.State ->
  SortRegistry.State ->
  Graph.State ->
  Placement.State ->
  Publication.State ->
  Store.State ->
  PeerStream.State PeerLogicalPayload ->
  Either LocalApplicationPublicationError PreparedLocalApplicationPublication
prepareLocalApplicationPublication
  genesis
  roles
  candidate
  controlled
  registry
  graph
  placement
  publications
  stores
  stream =
    case ApplicationState.applicationRequestCandidateCall candidate of
      WriteApplication privateNabla (PublishValue value) ->
        ApplicationPublication.prepareLocalApplicationPublication
          genesis
          fixtureHeraldMembershipGeneration
          roles
          candidate
          privateNabla
          value
          controlled
          registry
          graph
          (testStructuralProgress genesis)
          placement
          publications
          stores
          stream
      _ -> Left LocalApplicationPublicationDispositionContradiction

testStructuralProgress ::
  CheckedHeraldGenesis -> GraphProgress.StructuralProgressState
testStructuralProgress genesis =
  checkedPure
    "test structural progress"
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
        "test structural membership"
        (genesisHeraldMembershipGeneration (checkedSystemId genesis) members)
    membershipVector = emptyStructuralVersionVector membership
    projectionDigest =
      checkedPure
        "test initial projection digest"
        (mkInitialProjectionDigest (ByteString.replicate 32 0xf8))

testStructuralAuthority :: AuthorityEpoch
testStructuralAuthority =
  structuralAuthorityEpoch
    ( structuralOccurrenceId
        (fixtureIdentity "test authority source" mkHeraldEpoch 0xf6)
        firstStructuralSequence
    )
    (fixtureIdentity "test authority cut" mkTopologyCutId 0xf7)

prepareOrdinary ::
  OrdinaryFixture ->
  ApplicationState.ApplicationRequestCandidate ->
  Controlled.State ->
  SortRegistry.State ->
  Publication.State ->
  Store.State ->
  PeerStream.State PeerLogicalPayload ->
  Either LocalApplicationPublicationError PreparedLocalApplicationPublication
prepareOrdinary fixture applicationRequest controlled registry publications stores stream =
  prepareOrdinaryWithStructuralProgress
    fixture
    (testStructuralProgress fixtureCheckedGenesis)
    applicationRequest
    controlled
    registry
    publications
    stores
    stream

prepareOrdinaryWithStructuralProgress ::
  OrdinaryFixture ->
  GraphProgress.StructuralProgressState ->
  ApplicationState.ApplicationRequestCandidate ->
  Controlled.State ->
  SortRegistry.State ->
  Publication.State ->
  Store.State ->
  PeerStream.State PeerLogicalPayload ->
  Either LocalApplicationPublicationError PreparedLocalApplicationPublication
prepareOrdinaryWithStructuralProgress fixture structuralProgress applicationRequest controlled registry publications stores stream =
  ApplicationPublication.prepareLocalApplicationPublication
    fixtureCheckedGenesis
    fixtureHeraldMembershipGeneration
    (ApplicationState.processRoleView [])
    applicationRequest
    fixture.privateNabla
    fixture.value
    controlled
    registry
    fixture.graph
    structuralProgress
    fixture.placement
    publications
    stores
    stream

data StructuralFixture = StructuralFixture
  { localHerald :: HeraldEpoch,
    process :: ProcessEpochId,
    generated :: GlobalUniqueId,
    objectId :: GlobalObjectId,
    nabla :: NablaId,
    authority :: AuthorityEpoch,
    position :: ProcessAcceptancePosition,
    session :: ApplicationSessionId,
    binding :: ApplicationSessionBinding,
    privateNabla :: PrivateNablaId,
    request :: RequestId,
    occurrence :: SortDefinitionOccurrenceId,
    controlPrerequisite :: ControlIndex,
    value :: Application.ApplicationValue,
    route :: FrozenRoute,
    applicationState :: ApplicationState.State,
    controlled :: Controlled.State,
    registry :: SortRegistry.State,
    publications :: Publication.State,
    stores :: Store.State,
    stream :: PeerStream.State PeerLogicalPayload
  }

structuralFixture :: IO StructuralFixture
structuralFixture = do
  let localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis
      process = fixtureIdentity "structural process" mkProcessEpochId 0xb1
      generated = fixtureIdentity "structural object" mkGlobalUniqueId 0xb2
      objectId = globalObjectIdFromGlobalUniqueId generated
      nabla = fixtureIdentity "structural writer" mkNablaId 0xb3
      authority = genesisAuthorityEpoch
      position = processAcceptancePosition process 1
      request = requestId 1
      controlPrerequisite = controlIndex 5
      carrier = carrierFor NeutralVertexRole
      occurrence = primordialReplicaOccurrenceId carrier
      (openedApplication, privateNabla, session, binding) =
        publicationApplication
          localHerald
          process
          NeutralVertexRole
          nabla
      (applicationState, privateObject) =
        localizeUniqueId process generated openedApplication
      value = Application.RecordValue (Map.fromList [("label", Application.LabelValue (Application.VoidLabel, 0)), ("object_id", Application.UniqueIdValue privateObject)])
      source =
        controlledSourceState
          process
          (fixtureIdentity "structural process id" mkProcessId 0xb4)
          localHerald
          NeutralVertexRole
          (primordialReplicaSortId carrier)
          occurrence
          nabla
          authority
          controlPrerequisite
      controlled =
        Controlled.commitControlledReservation
          ( Controlled.prepareControlledReservation
              process
              nabla
              (primordialReplicaSortId carrier)
              authority
              generated
              source
          )
      route =
        checkedPure
          "structural route"
          ( freezeRoute
              [ routeDestination
                  (deriveSystemViewDeltaId (checkedSystemId fixtureCheckedGenesis) herald NeutralVertexRole)
                  (deriveSystemViewStoreIncarnationId (checkedSystemId fixtureCheckedGenesis) herald NeutralVertexRole)
                  herald
                  Normal
              | herald <- checkedActiveHeraldEpochs fixtureCheckedGenesis
              ]
          )
  pure
    StructuralFixture
      { localHerald,
        process,
        generated,
        objectId,
        nabla,
        authority,
        position,
        session,
        binding,
        privateNabla,
        request,
        occurrence,
        controlPrerequisite,
        value,
        route,
        applicationState,
        controlled,
        registry = SortRegistry.initialState fixtureCheckedGenesis,
        publications = Publication.initialState localHerald,
        stores = checkedPure "structural Store" (Store.initialState fixtureCheckedGenesis),
        stream = PeerStream.initialState localHerald
      }

retiredStructuralReferenceRegistry ::
  ControlIndex ->
  IO (SortId, SortRegistry.State)
retiredStructuralReferenceRegistry resolveIndex = do
  let descriptor =
        admittedApplicationSortDescriptor
          ( checkedPure
              "structural reference sort definition"
              (admitApplicationSortDefinition declaredSortDefinition)
          )
      sortId = descriptorSortId descriptor
      initialRegistry = SortRegistry.initialState fixtureCheckedGenesis
      plan =
        checkedPure
          "initial structural reference induction plan"
          ( SortRegistry.planSortInduction
              (checkedSystemId fixtureCheckedGenesis)
              descriptor
              initialRegistry
          )
      occurrence = SortRegistry.sortInductionPlanOccurrenceId plan
      carrier = carrierFor SortDefinitionRole
      definitionWriter =
        fixtureIdentity "structural reference definition writer" mkNablaId 0xbc
      identifier =
        publicationId
          definitionWriter
          genesisAuthorityEpoch
          (checkedLocalHeraldEpoch fixtureCheckedGenesis)
          (nablaSequence 1)
  publication <-
    checked
      "structural reference definition publication"
      ( mkCheckedPublication
          (primordialReplicaDescriptor carrier)
          identifier
          (sortDefinitionValue descriptor)
      )
  let definedRegistry =
        SortRegistry.commitSortInduction
          ( checkedPure
              "induce structural reference sort"
              ( SortRegistry.prepareSortInduction
                  descriptor
                  occurrence
                  publication
                  initialRegistry
              )
          )
      entry =
        maybe
          (error "induced structural reference sort missing")
          id
          (SortRegistry.lookupEffectiveSort sortId definedRegistry)
      successorOccurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId fixtureCheckedGenesis)
          sortId
          ( checkedPure
              "structural reference Resolve occurrence base"
              (resolvedRetirementOccurrenceBase resolveIndex)
          )
      retiredRegistry =
        SortRegistry.commitRegularSortRetirement
          ( checkedPure
              "retire structural reference sort"
              ( SortRegistry.prepareExactRegularSortRetirement
                  (checkedSystemId fixtureCheckedGenesis)
                  entry
                  resolveIndex
                  successorOccurrence
                  definedRegistry
              )
          )
  pure (sortId, retiredRegistry)

structuralRoleFixture ::
  PredefinedSortRole ->
  SortId ->
  SortRegistry.State ->
  IO StructuralFixture
structuralRoleFixture role referencedSort registry = do
  let localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis
      process = fixtureIdentity (show role <> " reference process") mkProcessEpochId roleByte
      generated =
        fixtureIdentity
          (show role <> " reference object")
          mkGlobalUniqueId
          (roleByte + 1)
      objectId = globalObjectIdFromGlobalUniqueId generated
      nabla =
        fixtureIdentity
          (show role <> " reference writer")
          mkNablaId
          (roleByte + 2)
      authority = genesisAuthorityEpoch
      position = processAcceptancePosition process 1
      request = requestId 1
      controlPrerequisite = controlIndex 5
      carrier = carrierFor role
      occurrence = primordialReplicaOccurrenceId carrier
      (openedApplication, privateNabla, session, binding) =
        publicationApplication localHerald process role nabla
      (applicationState, privateObject) =
        localizeUniqueId process generated openedApplication
      commonFields =
        [ ("label", Application.LabelValue label),
          ("object_id", Application.UniqueIdValue privateObject)
        ]
      sortField = ("sort_id", Application.BytesValue (sortIdBytes referencedSort))
      label = (Application.VoidLabel, 0)
      value =
        Application.RecordValue
          ( Map.fromList
              ( case role of
                  NeutralVertexRole -> commonFields
                  EdgeRole ->
                    [ ("source_vertex", Application.UniqueIdValue (privateNablaUniqueId privateNabla)),
                      ("destination_vertex", Application.UniqueIdValue (privateNablaUniqueId privateNabla)),
                      ("strength", Application.EnumValue "preserve")
                    ]
                      <> commonFields
                  NablaRole ->
                    ( "sequencing_object",
                      Application.OptionalUniqueIdValue Nothing
                    )
                      : sortField
                      : commonFields
                  DeltaRole -> sortField : commonFields
                  _ -> error "structural fixture requires a structural carrier"
              )
          )
      source =
        controlledSourceState
          process
          ( fixtureIdentity
              (show role <> " reference process id")
              mkProcessId
              (roleByte + 3)
          )
          localHerald
          role
          (primordialReplicaSortId carrier)
          occurrence
          nabla
          authority
          controlPrerequisite
      controlled =
        Controlled.commitControlledReservation
          ( Controlled.prepareControlledReservation
              process
              nabla
              (primordialReplicaSortId carrier)
              authority
              generated
              source
          )
      route =
        checkedPure
          (show role <> " structural reference route")
          ( freezeRoute
              [ routeDestination
                  ( deriveSystemViewDeltaId
                      (checkedSystemId fixtureCheckedGenesis)
                      herald
                      role
                  )
                  ( deriveSystemViewStoreIncarnationId
                      (checkedSystemId fixtureCheckedGenesis)
                      herald
                      role
                  )
                  herald
                  Normal
              | herald <- checkedActiveHeraldEpochs fixtureCheckedGenesis
              ]
          )
  pure
    StructuralFixture
      { localHerald,
        process,
        generated,
        objectId,
        nabla,
        authority,
        position,
        session,
        binding,
        privateNabla,
        request,
        occurrence,
        controlPrerequisite,
        value,
        route,
        applicationState,
        controlled,
        registry,
        publications = Publication.initialState localHerald,
        stores = checkedPure "structural reference Store" (Store.initialState fixtureCheckedGenesis),
        stream = PeerStream.initialState localHerald
      }
  where
    roleByte = case role of
      NeutralVertexRole -> 0xb0
      EdgeRole -> 0xe0
      NablaRole -> 0xc0
      DeltaRole -> 0xd0
      _ -> error "structural fixture requires a structural carrier"

prepareStructural ::
  StructuralFixture ->
  ApplicationState.ApplicationRequestCandidate ->
  Either LocalApplicationPublicationError PreparedLocalApplicationPublication
prepareStructural fixture applicationRequest =
  prepareStructuralWithProgress
    (testStructuralProgress fixtureCheckedGenesis)
    fixture
    applicationRequest

prepareStructuralWithProgress ::
  GraphProgress.StructuralProgressState ->
  StructuralFixture ->
  ApplicationState.ApplicationRequestCandidate ->
  Either LocalApplicationPublicationError PreparedLocalApplicationPublication
prepareStructuralWithProgress structuralProgress fixture applicationRequest =
  ApplicationPublication.prepareLocalApplicationPublication
    fixtureCheckedGenesis
    fixtureHeraldMembershipGeneration
    (ApplicationState.processRoleView [fixture.process])
    applicationRequest
    fixture.privateNabla
    fixture.value
    fixture.controlled
    fixture.registry
    Graph.emptyState
    structuralProgress
    (Placement.initialState fixture.localHerald)
    fixture.publications
    fixture.stores
    fixture.stream

data EdgeFailureFixture = EdgeFailureFixture
  { localHerald :: HeraldEpoch,
    process :: ProcessEpochId,
    missingEndpoint :: GlobalObjectId,
    privateNabla :: PrivateNablaId,
    session :: ApplicationSessionId,
    binding :: ApplicationSessionBinding,
    request :: RequestId,
    value :: Application.ApplicationValue,
    applicationState :: ApplicationState.State,
    controlled :: Controlled.State,
    registry :: SortRegistry.State,
    publications :: Publication.State,
    stores :: Store.State,
    stream :: PeerStream.State PeerLogicalPayload
  }

edgeFailureFixture :: IO EdgeFailureFixture
edgeFailureFixture = do
  let localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis
      process = fixtureIdentity "edge process" mkProcessEpochId 0xd2
      nabla = fixtureIdentity "edge writer" mkNablaId 0xd3
      authority = genesisAuthorityEpoch
      generated = fixtureIdentity "edge object" mkGlobalUniqueId 0xd4
      missingGlobal = fixtureIdentity "missing edge endpoint" mkGlobalUniqueId 0xd5
      missingEndpoint = globalObjectIdFromGlobalUniqueId missingGlobal
      carrier = carrierFor EdgeRole
      occurrence = primordialReplicaOccurrenceId carrier
      (openedApplication, privateNabla, session, binding) =
        publicationApplication localHerald process EdgeRole nabla
      (withObject, privateObject) =
        localizeUniqueId process generated openedApplication
      (applicationState, privateMissing) =
        localizeUniqueId process missingGlobal withObject
      value =
        Application.RecordValue
          ( Map.fromList
              [ ("destination_vertex", Application.UniqueIdValue privateMissing),
                ("label", Application.LabelValue (Application.VoidLabel, 0)),
                ("object_id", Application.UniqueIdValue privateObject),
                ( "source_vertex",
                  Application.UniqueIdValue
                    (privateNablaUniqueId privateNabla)
                ),
                ("strength", Application.EnumValue "preserve")
              ]
          )
      source =
        controlledSourceState
          process
          (fixtureIdentity "edge process id" mkProcessId 0xd6)
          localHerald
          EdgeRole
          (primordialReplicaSortId carrier)
          occurrence
          nabla
          authority
          (controlIndex 7)
      controlled =
        Controlled.commitControlledReservation
          ( Controlled.prepareControlledReservation
              process
              nabla
              (primordialReplicaSortId carrier)
              authority
              generated
              source
          )
  pure
    EdgeFailureFixture
      { localHerald,
        process,
        missingEndpoint,
        privateNabla,
        session,
        binding,
        request = requestId 1,
        value,
        applicationState,
        controlled,
        registry = SortRegistry.initialState fixtureCheckedGenesis,
        publications = Publication.initialState localHerald,
        stores = checkedPure "edge Store" (Store.initialState fixtureCheckedGenesis),
        stream = PeerStream.initialState localHerald
      }

data ControlledFixture = ControlledFixture
  { localHerald :: HeraldEpoch,
    process :: ProcessEpochId,
    generated :: GlobalUniqueId,
    objectId :: GlobalObjectId,
    nabla :: NablaId,
    authority :: AuthorityEpoch,
    position :: ProcessAcceptancePosition,
    session :: ApplicationSessionId,
    binding :: ApplicationSessionBinding,
    privateNabla :: PrivateNablaId,
    request :: RequestId,
    occurrence :: SortDefinitionOccurrenceId,
    descriptor :: CanonicalDescriptor,
    currentValue :: Application.ApplicationValue,
    obsoleteValue :: Application.ApplicationValue,
    applicationState :: ApplicationState.State,
    controlled :: Controlled.State,
    registry :: SortRegistry.State,
    publications :: Publication.State,
    stores :: Store.State,
    stream :: PeerStream.State PeerLogicalPayload
  }

controlledFixture :: IO ControlledFixture
controlledFixture = controlledFixtureFor obsoleteControlledSortDefinition

controlledFixtureFor :: ApplicationSortDefinition -> IO ControlledFixture
controlledFixtureFor sortDefinition = do
  let localHerald = checkedLocalHeraldEpoch fixtureCheckedGenesis
      process = fixtureIdentity "controlled process" mkProcessEpochId 0xc1
      generated = fixtureIdentity "controlled object" mkGlobalUniqueId 0xc2
      objectId = globalObjectIdFromGlobalUniqueId generated
      nabla = fixtureIdentity "controlled writer" mkNablaId 0xc3
      authority = genesisAuthorityEpoch
      position = processAcceptancePosition process 1
      request = requestId 1
      descriptor = admittedApplicationSortDescriptor (checkedPure "controlled descriptor" (admitApplicationSortDefinition sortDefinition))
      occurrence =
        deriveSortDefinitionOccurrenceId
          (checkedSystemId fixtureCheckedGenesis)
          (descriptorSortId descriptor)
          Genesis
      (openedApplication, privateNabla, session, binding, access) =
        publicationApplicationWithAccess
          localHerald
          process
          NeutralVertexRole
          nabla
      (applicationState, privateObject) =
        localizeUniqueId process generated openedApplication
      currentValue = controlledValue privateObject ((Application.ProcessLabel (ApplicationState.bootstrapAccessProcess access), 0)) False
      obsoleteValue = controlledValue privateObject (Application.VoidLabel, 37) True
      source =
        controlledSourceState
          process
          (fixtureIdentity "controlled process id" mkProcessId 0xc4)
          localHerald
          NeutralVertexRole
          (descriptorSortId descriptor)
          occurrence
          nabla
          authority
          (controlIndex 6)
      controlled =
        Controlled.commitControlledReservation
          ( Controlled.prepareControlledReservation
              process
              nabla
              (descriptorSortId descriptor)
              authority
              generated
              source
          )
      registry =
        SortRegistry.commitSortInduction
          ( checkedPure
              "controlled registry"
              ( SortRegistry.prepareSortInduction
                  descriptor
                  occurrence
                  (primordialReplicaPublication (carrierFor SortDefinitionRole))
                  (SortRegistry.initialState fixtureCheckedGenesis)
              )
          )
  pure
    ControlledFixture
      { localHerald,
        process,
        generated,
        objectId,
        nabla,
        authority,
        position,
        session,
        binding,
        privateNabla,
        request,
        occurrence,
        descriptor,
        currentValue,
        obsoleteValue,
        applicationState,
        controlled,
        registry,
        publications = Publication.initialState localHerald,
        stores = checkedPure "controlled Store" (Store.initialState fixtureCheckedGenesis),
        stream = PeerStream.initialState localHerald
      }

prepareControlled ::
  ControlledFixture ->
  ApplicationState.ApplicationRequestCandidate ->
  Controlled.State ->
  Publication.State ->
  Store.State ->
  PeerStream.State PeerLogicalPayload ->
  Either LocalApplicationPublicationError PreparedLocalApplicationPublication
prepareControlled fixture applicationRequest controlled publications stores stream =
  prepareLocalApplicationPublication
    fixtureCheckedGenesis
    (ApplicationState.processRoleView [fixture.process])
    applicationRequest
    controlled
    fixture.registry
    Graph.emptyState
    (Placement.initialState fixture.localHerald)
    publications
    stores
    stream

peerControlledRecord ::
  ControlledFixture ->
  Domain.Label ->
  Controlled.State
peerControlledRecord fixture retainedLabel =
  fst
    ( Controlled.commitControlledPeerObservation
        ( checkedPure
            "peer controlled record"
            (Controlled.prepareControlledPeerObservation observation source)
        )
    )
  where
    source =
      controlledSourceState
        fixture.process
        (fixtureIdentity "peer-record process id" mkProcessId 0xc5)
        fixture.localHerald
        NeutralVertexRole
        (descriptorSortId fixture.descriptor)
        fixture.occurrence
        fixture.nabla
        fixture.authority
        (controlIndex 6)
    publication =
      checkedPure
        "peer-record checked publication"
        ( mkCheckedPublication
            fixture.descriptor
            ( publicationId
                fixture.nabla
                fixture.authority
                fixture.localHerald
                (nablaSequence 99)
            )
            ( checkedPure
                "peer-record value"
                ( Domain.recordValue
                    [ ( checkedPure "label field" (Domain.mkFieldName "label"),
                        Domain.labelValue retainedLabel
                      ),
                      ( checkedPure "object field" (Domain.mkFieldName "object_id"),
                        Domain.globalUniqueIdValue fixture.generated
                      ),
                      ( checkedPure "obsolete field" (Domain.mkFieldName "obsolete"),
                        Domain.boolValue False
                      )
                    ]
                )
            )
        )
    observation =
      checkedPure
        "peer-record controlled observation"
        ( Controlled.checkControlledObservation
            fixture.descriptor
            fixture.occurrence
            publication
        )

-- The unavailable-label row must retain Normal target possession so label
-- operability, rather than the earlier effective-possession guard, is the sole
-- failing premise.
possessedPeerControlledRecord ::
  ControlledFixture ->
  Domain.Label ->
  Controlled.State
possessedPeerControlledRecord fixture retainedLabel =
  Controlled.commitControlledStoreObservation
    ( checkedPure
        "possessed peer controlled record"
        ( Controlled.prepareControlledStoreRead
            fixture.process
            reader
            Normal
            observation
            recorded
        )
    )
  where
    reader = fixtureIdentity "peer-record reader" mkDeltaId 0xc7
    source =
      controlledSourceStateWithReader
        fixture.process
        (fixtureIdentity "possessed peer-record process id" mkProcessId 0xc8)
        fixture.localHerald
        NeutralVertexRole
        (descriptorSortId fixture.descriptor)
        fixture.occurrence
        fixture.nabla
        reader
        fixture.authority
        (controlIndex 6)
    publication =
      checkedPure
        "possessed peer-record checked publication"
        ( mkCheckedPublication
            fixture.descriptor
            ( publicationId
                fixture.nabla
                fixture.authority
                fixture.localHerald
                (nablaSequence 99)
            )
            ( checkedPure
                "possessed peer-record value"
                ( Domain.recordValue
                    [ ( checkedPure "label field" (Domain.mkFieldName "label"),
                        Domain.labelValue retainedLabel
                      ),
                      ( checkedPure "object field" (Domain.mkFieldName "object_id"),
                        Domain.globalUniqueIdValue fixture.generated
                      ),
                      ( checkedPure "obsolete field" (Domain.mkFieldName "obsolete"),
                        Domain.boolValue False
                      )
                    ]
                )
            )
        )
    observation =
      checkedPure
        "possessed peer-record controlled observation"
        ( Controlled.checkControlledObservation
            fixture.descriptor
            fixture.occurrence
            publication
        )
    recorded =
      fst
        ( Controlled.commitControlledPeerObservation
            ( checkedPure
                "install possessed peer controlled record"
                (Controlled.prepareControlledPeerObservation observation source)
            )
        )

runTrace :: OrdinaryFixture -> [Bool] -> Either String (Publication.State, Int)
runTrace fixture choices = do
  firstRequest <-
    mapLeftShow
      ( firstPublicationRequest
          fixture.applicationState
          fixture.session
          fixture.binding
          fixture.request
          fixture.privateNabla
          fixture.value
      )
  first <- mapLeftShow (prepareOrdinary fixture firstRequest fixture.controlled fixture.registry fixture.publications fixture.stores fixture.stream)
  let (application, controlled, registry, publications, stores, stream, _) = commitLocalApplicationPublication first
  (_, _, _, finalPublications, _, _, accepted, _) <-
    foldM step (application, controlled, registry, publications, stores, stream, 1, fixture.request) choices
  Right (finalPublications, accepted)
  where
    step (application, controlled, registry, publications, stores, stream, accepted, latest) acceptNew = do
      let nextAccepted = accepted + 1
          nextRequest = if acceptNew then requestId (fromIntegral nextAccepted) else latest
      if acceptNew
        then do
          applicationRequest <-
            mapLeftShow
              ( firstPublicationRequest
                  application
                  fixture.session
                  fixture.binding
                  nextRequest
                  fixture.privateNabla
                  fixture.value
              )
          prepared <-
            mapLeftShow
              (prepareOrdinary fixture applicationRequest controlled registry publications stores stream)
          let (applicationAfter, controlledAfter, registryAfter, publicationsAfter, storesAfter, streamAfter, outcome) =
                commitLocalApplicationPublication prepared
          if localApplicationPublicationClassification outcome /= ApplicationPublicationFirstAccepted
            then Left "first-seen classification mismatch"
            else
              Right
                ( applicationAfter,
                  controlledAfter,
                  registryAfter,
                  publicationsAfter,
                  storesAfter,
                  streamAfter,
                  nextAccepted,
                  nextRequest
                )
        else case ApplicationState.classifyApplicationRequest
          fixture.session
          fixture.binding
          nextRequest
          (WriteApplication fixture.privateNabla (PublishValue fixture.value))
          application of
          Right (ApplicationState.RetainedApplicationRequest _) ->
            Right
              ( application,
                controlled,
                registry,
                publications,
                stores,
                stream,
                accepted,
                nextRequest
              )
          Left problem -> Left (show problem)
          Right _ -> Left "retry classification mismatch"

controlledSourceState ::
  ProcessEpochId ->
  ProcessId ->
  HeraldEpoch ->
  PredefinedSortRole ->
  SortId ->
  SortDefinitionOccurrenceId ->
  NablaId ->
  AuthorityEpoch ->
  ControlIndex ->
  Controlled.State
controlledSourceState process processId herald role sortId occurrence writer authority prerequisite =
  Controlled.commitControlledBootstrap
    ( checkedPure
        "controlled source"
        ( Controlled.prepareControlledBootstrap
            (Controlled.processFact processId process herald authority)
            [ Controlled.writerRootFact
                process
                role
                sortId
                occurrence
                authority
                prerequisite
                writer
                UnsequencedNabla
            ]
            Controlled.emptyState
        )
    )

controlledSourceStateWithReader ::
  ProcessEpochId ->
  ProcessId ->
  HeraldEpoch ->
  PredefinedSortRole ->
  SortId ->
  SortDefinitionOccurrenceId ->
  NablaId ->
  DeltaId ->
  AuthorityEpoch ->
  ControlIndex ->
  Controlled.State
controlledSourceStateWithReader process processId herald role sortId occurrence writer reader authority prerequisite =
  Controlled.commitControlledBootstrap
    ( checkedPure
        "controlled source with reader"
        ( Controlled.prepareControlledBootstrap
            (Controlled.processFact processId process herald authority)
            [ Controlled.writerRootFact
                process
                role
                sortId
                occurrence
                authority
                prerequisite
                writer
                UnsequencedNabla,
              Controlled.readerRootFact
                process
                role
                sortId
                occurrence
                authority
                prerequisite
                reader
            ]
            Controlled.emptyState
        )
    )

publicationApplication ::
  HeraldEpoch ->
  ProcessEpochId ->
  PredefinedSortRole ->
  NablaId ->
  ( ApplicationState.State,
    PrivateNablaId,
    ApplicationSessionId,
    ApplicationSessionBinding
  )
publicationApplication herald process role nabla =
  let (state, privateNabla, session, binding, _) =
        publicationApplicationWithAccess herald process role nabla
   in (state, privateNabla, session, binding)

publicationApplicationWithAccess ::
  HeraldEpoch ->
  ProcessEpochId ->
  PredefinedSortRole ->
  NablaId ->
  ( ApplicationState.State,
    PrivateNablaId,
    ApplicationSessionId,
    ApplicationSessionBinding,
    ApplicationState.BootstrapAccess
  )
publicationApplicationWithAccess herald process role nabla =
  (openedState, privateNabla, session, binding, access)
  where
    attachment = applicationAttachment process
    (bootstrapped, access) =
      ApplicationState.commitApplicationBootstrap
        ( checkedPure
            "application bootstrap"
            ( ApplicationState.prepareApplicationBootstrap
                attachment
                process
                roots
                fixtureEnvironmentHub
                (fixtureEnvironmentEdges roots)
                ApplicationState.emptyState
            )
        )
    privateNabla =
      case [ candidate
           | ApplicationState.WriterAccess candidateRole candidate _ <-
               ApplicationState.bootstrapAccessRoots access,
             candidateRole == role
           ] of
        [candidate] -> candidate
        _ -> error "application writer root was not localized exactly once"
    roots =
      concat
        [ [ ApplicationState.ApplicationWriterRoot
              candidateRole
              ( if candidateRole == role
                  then nabla
                  else
                    fixtureIdentity
                      "application writer root"
                      mkNablaId
                      (0xe0 + ordinal)
              )
              UnsequencedNabla,
            ApplicationState.ApplicationReaderRoot
              candidateRole
              ( fixtureIdentity
                  "application reader root"
                  mkDeltaId
                  (0xf0 + ordinal)
              )
          ]
        | (ordinal, candidateRole) <- zip [0 ..] allPredefinedSortRoles
        ]
    (openedState, acceptance) =
      ApplicationState.commitApplicationSessionAcceptance
        ( checkedPure
            "application session open"
            ( ApplicationState.prepareApplicationSessionOpen
                herald
                attachment
                (clientNonce 1)
                bootstrapped
            )
        )
    binding = sessionAcceptanceBinding acceptance
    session = case sessionAcceptanceReply acceptance of
      SessionOpened opened _ _ -> opened
      reply -> error ("expected opened session, got " <> show reply)

applicationAttachment :: ProcessEpochId -> ApplicationAttachment
applicationAttachment process =
  applicationAttachmentForBootstrap
    ( checkedPure
        "bootstrap manifest"
        (mkBootstrapManifestId (processEpochIdBytes process))
    )

firstPublicationRequest ::
  ApplicationState.State ->
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  RequestId ->
  PrivateNablaId ->
  Application.ApplicationValue ->
  Either String ApplicationState.ApplicationRequestCandidate
firstPublicationRequest state session binding request privateNabla value =
  case ApplicationState.classifyApplicationRequest
    session
    binding
    request
    (WriteApplication privateNabla (PublishValue value))
    state of
    Left problem -> Left (show problem)
    Right (ApplicationState.FirstApplicationRequest candidate) ->
      Right candidate
    Right ApplicationState.RetainedApplicationRequest {} ->
      Left "expected first generic publication request, got retry"
    Right ApplicationState.AttachedApplicationRequest {} ->
      Left "expected first generic publication request, got attached request"
    Right ApplicationState.ConflictingApplicationRequest {} ->
      Left "expected first generic publication request, got conflict"

assertRequestConflict ::
  String ->
  Either
    ApplicationState.ApplicationSessionTransitionError
    ApplicationState.ApplicationRequestClassification ->
  Assertion
assertRequestConflict description classified =
  case classified of
    Right ApplicationState.ConflictingApplicationRequest {} -> pure ()
    Left problem ->
      assertFailure (description <> ": classifier failed: " <> show problem)
    Right _ ->
      assertFailure (description <> ": expected request conflict")

assertStillFirst ::
  String ->
  ApplicationState.State ->
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  RequestId ->
  PrivateNablaId ->
  Application.ApplicationValue ->
  Assertion
assertStillFirst description state session binding request privateNabla value =
  case ApplicationState.classifyApplicationRequest
    session
    binding
    request
    (WriteApplication privateNabla (PublishValue value))
    state of
    Right ApplicationState.FirstApplicationRequest {} -> pure ()
    Left problem ->
      assertFailure (description <> ": retry classification failed: " <> show problem)
    Right _ ->
      assertFailure (description <> ": failed preparation consumed request identity")

assertLocalPublicationFailure ::
  String ->
  (LocalApplicationPublicationError -> Bool) ->
  Either LocalApplicationPublicationError PreparedLocalApplicationPublication ->
  Assertion
assertLocalPublicationFailure description matches result =
  case result of
    Left problem
      | matches problem -> pure ()
      | otherwise ->
          assertFailure
            (description <> ": unexpected failure " <> show problem)
    Right _ ->
      assertFailure (description <> ": preparation unexpectedly succeeded")

data LocalPublicationOwnerWitness
  = LocalPublicationOwnerWitness
      ApplicationState.State
      Controlled.State
      SortRegistry.State
      Publication.State
      Store.State
      (PeerStream.State PeerLogicalPayload)
  deriving stock (Eq)

localPublicationOwnerWitness ::
  ApplicationState.State ->
  Controlled.State ->
  SortRegistry.State ->
  Publication.State ->
  Store.State ->
  PeerStream.State PeerLogicalPayload ->
  LocalPublicationOwnerWitness
localPublicationOwnerWitness = LocalPublicationOwnerWitness

localizeUniqueId :: ProcessEpochId -> GlobalUniqueId -> ApplicationState.State -> (ApplicationState.State, PrivateUniqueId)
localizeUniqueId process identifier state =
  case ApplicationState.commitOrdinaryApplicationValueLocalization
    ( checkedPure
        "identity localization"
        ( ApplicationState.prepareOrdinaryApplicationValueLocalization
            (ApplicationState.processRoleView [process])
            process
            (Domain.globalUniqueIdValue identifier)
            state
        )
    ) of
    (successor, Application.UniqueIdValue privateIdentity) -> (successor, privateIdentity)
    _ -> error "identity localization changed value kind"

controlledValue :: PrivateUniqueId -> Application.ApplicationLabel -> Bool -> Application.ApplicationValue
controlledValue object label obsolete =
  Application.RecordValue
    (Map.fromList [("label", Application.LabelValue label), ("object_id", Application.UniqueIdValue object), ("obsolete", Application.BoolValue obsolete)])

declaredSortDefinition :: ApplicationSortDefinition
declaredSortDefinition =
  DeclaredSortDefinition
    ApplicationSortDescriptor
      { SortSyntax.sortKind = RegularSort,
        SortSyntax.valueSchema = RecordSchema (Map.singleton "key" BoolSchema),
        SortSyntax.keyProjections = [ApplicationProjection ("key" :| [])],
        SortSyntax.validityPredicate = AlwaysPredicate,
        SortSyntax.obsolescencePredicate = NeverPredicate,
        SortSyntax.rankTerms = RankApplicationValue Ascending :| [],
        SortSyntax.minimumRetentionMicros = 0,
        SortSyntax.isImmutable = False,
        SortSyntax.labelField = Nothing
      }
    Nothing

obsoleteControlledSortDefinition :: ApplicationSortDefinition
obsoleteControlledSortDefinition =
  DeclaredSortDefinition
    ApplicationSortDescriptor
      { SortSyntax.sortKind = ControlledSort,
        SortSyntax.valueSchema = RecordSchema (Map.fromList [("label", LabelSchema), ("object_id", UniqueIdSchema), ("obsolete", BoolSchema)]),
        SortSyntax.keyProjections = [ApplicationProjection ("object_id" :| [])],
        SortSyntax.validityPredicate = AlwaysPredicate,
        SortSyntax.obsolescencePredicate = CompareField (ApplicationProjection ("obsolete" :| [])) ScalarEqual (LiteralBool True),
        SortSyntax.rankTerms = RankApplicationValue Ascending :| [],
        SortSyntax.minimumRetentionMicros = 0,
        SortSyntax.isImmutable = False,
        SortSyntax.labelField = Just "label"
      }
    Nothing

immutableControlledSortDefinition :: ApplicationSortDefinition
immutableControlledSortDefinition =
  DeclaredSortDefinition
    ApplicationSortDescriptor
      { SortSyntax.sortKind = ControlledSort,
        SortSyntax.valueSchema = RecordSchema (Map.fromList [("label", LabelSchema), ("object_id", UniqueIdSchema), ("obsolete", BoolSchema)]),
        SortSyntax.keyProjections = [ApplicationProjection ("object_id" :| [])],
        SortSyntax.validityPredicate = AlwaysPredicate,
        SortSyntax.obsolescencePredicate = NeverPredicate,
        SortSyntax.rankTerms = RankApplicationValue Ascending :| [],
        SortSyntax.minimumRetentionMicros = 0,
        SortSyntax.isImmutable = True,
        SortSyntax.labelField = Just "label"
      }
    Nothing

carrierFor :: PredefinedSortRole -> PrimordialDefinitionReplica
carrierFor role =
  maybe (error ("missing carrier " <> show role)) id (find ((== role) . primordialReplicaRole) (checkedPrimordialReplicas fixtureCheckedGenesis))

storeSlotFor :: PrimordialDefinitionReplica -> Store.State -> Store.StoreSlot
storeSlotFor carrier state =
  maybe (error "missing carrier Store slot") id (find ((== primordialReplicaSortId carrier) . Store.storeSlotSortId) (Store.storeSlots state))

assertStoreReceipt :: Store.StoreSlot -> PublicationId -> Store.State -> Assertion
assertStoreReceipt original identifier state = do
  successor <- maybe (fail "Store slot disappeared") pure (Store.lookupStoreSlot (Store.storeSlotDelta original) state)
  assertBool "local receipt retained" (identifier `elem` Store.storeSlotAppliedPublicationIds successor)

requireControlledRecord :: GlobalObjectId -> Controlled.State -> IO Controlled.ControlledLocalRecord
requireControlledRecord object state = maybe (fail "controlled record missing") pure (Controlled.controlledLocalRecord object state)

requirePublicationRequestWitness ::
  RequestId ->
  ApplicationSessionId ->
  ApplicationState.State ->
  IO ApplicationState.ApplicationRequestWitness
requirePublicationRequestWitness request session state =
  case [ witness
       | (retainedRequest, witness) <-
           ApplicationState.applicationRequestEntries session state,
         retainedRequest == request
       ] of
    [witness] -> pure witness
    witnesses ->
      fail
        ( "expected one retained generic publication request witness, got "
            <> show (length witnesses)
        )

requireMapEntry :: (Ord key, Show key) => String -> key -> Map.Map key value -> IO value
requireMapEntry description key entries = maybe (fail (description <> " missing for " <> show key)) pure (Map.lookup key entries)

fixtureIdentity :: (Show problem) => String -> (ByteString.ByteString -> Either problem value) -> Word8 -> value
fixtureIdentity description constructor seed = checkedPure description (constructor (fixtureIdentifierBytes seed))

checked :: (Show problem) => String -> Either problem value -> IO value
checked description = either (fail . ((description <> ": ") <>) . show) pure

checkedPure :: (Show problem) => String -> Either problem value -> value
checkedPure description = either (error . ((description <> ": ") <>) . show) id

mapLeftShow :: (Show problem) => Either problem value -> Either String value
mapLeftShow = either (Left . show) Right
