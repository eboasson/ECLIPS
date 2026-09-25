{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Step5SemanticProperties
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.List (find, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity
  ( privateDeltaUniqueId,
    privateProcessUniqueId,
  )
import Eclips.Application.Types.Query qualified as Application
import Eclips.Application.Types.Result
  ( RegularCallResult (..),
    WaitResult (WaitReady),
  )
import Eclips.Application.Types.SortDescriptor
  ( ApplicationPredicateExpression (AlwaysPredicate, NeverPredicate),
    ApplicationProjection (ApplicationProjection),
    ApplicationRankDirection (Ascending),
    ApplicationRankTerm (RankApplicationValue),
    ApplicationSortDefinition (DeclaredSortDefinition),
    ApplicationSortDescriptor (..),
    ApplicationSortKind (RegularSort),
    ApplicationValueSchema (RecordSchema, TextSchema),
  )
import Eclips.Application.Types.Value qualified as ApplicationValue
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (SortDefinitionWritten),
  )
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    DeltaId,
    ProcessEpochId,
    SortId,
    StoreIncarnationId,
    deltaIdFromGlobalObjectId,
    globalObjectIdFromGlobalUniqueId,
    globalObjectIdFromProcessEpochId,
    globalUniqueIdFromGlobalObjectId,
    mkDeltaId,
    mkProcessEpochId,
    mkStoreIncarnationId,
  )
import Eclips.Domain.Publication (checkedPublicationId)
import Eclips.Domain.Query qualified as DomainQuery
import Eclips.Domain.Sort.Canonical (canonicalCheckedDescriptor)
import Eclips.Domain.Sort.Descriptor (descriptorSchema)
import Eclips.Domain.Startup
  ( PredefinedSortRole (SortDefinitionRole),
  )
import Eclips.Domain.Value qualified as DomainValue
import Eclips.Herald.Application.Query
  ( globalizeApplicationQuery,
    globalizedQueryDeltas,
    globalizedQueryPredicate,
  )
import Eclips.Herald.Application.Request
  ( ApplicationOperation (..),
    ApplicationRejection (..),
    ApplicationRequestReply (..),
    RetainedRequestReplyBody (..),
    WaitId,
    requestId,
  )
import Eclips.Herald.Application.Session
  ( ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionReply (SessionOpened),
    primordialApplicationAttachment,
    sessionAcceptanceBinding,
    sessionAcceptanceReply,
  )
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.SortDefinition
  ( admitApplicationSortDefinition,
    admittedApplicationSortId,
  )
import Eclips.Herald.Application.State qualified as ApplicationState
import Eclips.Herald.Controlled.State qualified as Controlled
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.EffectivePublication
  ( checkEffectivePublicationContext,
    interpretEffectivePublication,
  )
import Eclips.Herald.Genesis
  ( CheckedInitialBootstraps,
    PrimordialProcessManifest (..),
    checkInitialBootstraps,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedPrimordialReplicas,
    primordialReplicaDescriptor,
    primordialReplicaOccurrenceId,
    primordialReplicaRole,
    primordialReplicaSortId,
  )
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( ApplicationRequestIngress (..),
    ApplicationSessionIngress (..),
    HeraldInputBody (..),
    candidateApplicationLane,
    heraldInput,
  )
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.Query
  ( resolvedQuery,
    resolvedQueryBranch,
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupLastObservedTime,
    startupApplicationState,
    startupLastObservedTime,
    startupOracleProjectionState,
    startupWaitState,
  )
import Eclips.Herald.Store.Observation qualified as StoreObservation
import Eclips.Herald.Store.State qualified as Store
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Wait.State qualified as Wait
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureGeneratorSeed,
    fixtureIdentifierBytes,
    fixtureLocalBootstrapIds,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
  )
import PrimordialTestAccess (conventionalStartupPairs)
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
    testProperty,
  )
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "Step-5 cross-owner semantics"
    [ testCase "query globalization covers unique-ID, label, and enum arms" caseQueryIdentityLiterals,
      testCase "query admission retains the precise typed rejection" caseTypedQueryRejections,
      testProperty "store read/take is canonical and preserves cross-store duplicates" propStoreReadTake,
      testCase "immediate and empty-disjunction waits have distinct levels" caseWaitLevels,
      testCase "a wake wins before a later cancellation" caseWakeBeforeCancel
    ]

caseQueryIdentityLiterals :: Assertion
caseQueryIdentityLiterals = do
  let opened = openedFixture
      processPrivate = Access.startupAccessProcess opened.access
      privateIdentity = privateProcessUniqueId processPrivate
      reader = Access.predefinedReader (accessFor Access.ProcessEpochRole opened.access)
      projection = ApplicationProjection ("identity" :| [])
      applicationPredicate =
        Application.QueryAll
          ( Application.QueryCompare
              projection
              Application.ScalarEqual
              (Application.QueryUniqueId privateIdentity)
              :| [ Application.QueryCompare
                     projection
                     Application.ScalarEqual
                     (Application.QueryLabel (ApplicationValue.VoidLabel, 0)),
                   Application.QueryCompare
                     projection
                     Application.ScalarEqual
                     (Application.QueryLabel ((ApplicationValue.ProcessLabel processPrivate, 0))),
                   Application.QueryCompare
                     projection
                     Application.ScalarEqual
                     (Application.QueryLabel ((ApplicationValue.ZombieLabel processPrivate, 0))),
                   Application.QueryCompare
                     projection
                     Application.ScalarEqual
                     (Application.QueryEnum "preserve")
                 ]
          )
      query = Application.ApplicationQuery (Set.singleton reader) applicationPredicate
      roles =
        ApplicationState.processRoleView
          ( OracleProjection.projectedProcessEpochs
              (startupOracleProjectionState opened.state)
          )
      globalized =
        checked
          "globalize application query"
          ( globalizeApplicationQuery
              roles
              opened.process
              query
              (startupApplicationState opened.state)
          )
      globalIdentity =
        globalUniqueIdFromGlobalObjectId
          (globalObjectIdFromProcessEpochId opened.process)
      expectedPredicate =
        DomainQuery.QueryAll
          ( DomainQuery.QueryCompare
              expectedProjection
              DomainQuery.ScalarEqual
              (DomainQuery.QueryGlobalUniqueId globalIdentity)
              :| [ DomainQuery.QueryCompare
                     expectedProjection
                     DomainQuery.ScalarEqual
                     (DomainQuery.QueryLabel (DomainValue.VoidLabel, 0)),
                   DomainQuery.QueryCompare
                     expectedProjection
                     DomainQuery.ScalarEqual
                     (DomainQuery.QueryLabel ((DomainValue.ProcessLabel opened.process, 0))),
                   DomainQuery.QueryCompare
                     expectedProjection
                     DomainQuery.ScalarEqual
                     (DomainQuery.QueryLabel ((DomainValue.ZombieLabel opened.process, 0))),
                   DomainQuery.QueryCompare
                     expectedProjection
                     DomainQuery.ScalarEqual
                     (DomainQuery.QueryEnum (DomainValue.enumSymbol "preserve"))
                 ]
          )
      expectedDelta =
        deltaIdFromGlobalObjectId
          . globalObjectIdFromGlobalUniqueId
          $ checked
            "resolve private reader"
            ( ApplicationState.resolveApplicationPrivateUniqueId
                opened.process
                (privateDeltaUniqueId reader)
                (startupApplicationState opened.state)
            )
  assertEqual "the private reader resolves in the caller namespace" [(reader, expectedDelta)] (globalizedQueryDeltas globalized)
  assertEqual "all identity-bearing literals globalize by semantic role" expectedPredicate (globalizedQueryPredicate globalized)
  where
    expectedProjection =
      DomainValue.mkProjection
        (checked "query field" (DomainValue.mkFieldName "identity") :| [])

caseTypedQueryRejections :: Assertion
caseTypedQueryRejections = do
  let opened = openedFixture
      neutralReader = Access.predefinedReader (accessFor Access.NeutralVertexRole opened.access)
      processReader = Access.predefinedReader (accessFor Access.ProcessEpochRole opened.access)
      sortReader = Access.predefinedReader (accessFor Access.SortDefinitionRole opened.access)
      invalidField = ApplicationProjection ("not-valid" :| [])
      absentProjection = ApplicationProjection ("absent" :| [])
      liveProjection = ApplicationProjection ("live" :| [])
      sortProjection = ApplicationProjection ("sort_id" :| [])
      query reader projection literal =
        Application.ApplicationQuery
          (Set.singleton reader)
          (Application.QueryCompare projection Application.ScalarEqual literal)
      cases =
        [ ( ApplicationInvalidFieldName "not-valid",
            ReadApplication (query neutralReader invalidField (Application.QueryBool True))
          ),
          ( ApplicationInvalidQueryProjection absentProjection,
            ReadApplication (query neutralReader absentProjection (Application.QueryBool True))
          ),
          ( ApplicationQueryPredicateMismatch,
            ReadApplication (query processReader liveProjection (Application.QueryText "not a bool"))
          ),
          ( ApplicationSortDefinitionProjectionUnsupported sortProjection,
            ReadApplication (query sortReader sortProjection (Application.QueryBytes ByteString.empty))
          )
        ]
  foldCases opened.state 1 cases
  where
    foldCases _ _ [] = pure ()
    foldCases state ordinal ((expected, call) : rest) = do
      let opened = openedFixture
          (successor, effects) = runCall (20 + ordinal) opened.session opened.binding ordinal call state
      assertRejected ordinal expected effects
      foldCases successor (ordinal + 1) rest

propStoreReadTake :: Bool -> Property
propStoreReadTake reversePresentation =
  case storeReadTakeTrace reversePresentation of
    Left problem -> counterexample problem False
    Right (matches, taken, hidden) ->
      let coordinates =
            fmap
              ( \match ->
                  let key = StoreObservation.effectiveStoreMatchKey match
                   in ( StoreObservation.effectiveCandidateKeyDelta key,
                        StoreObservation.effectiveCandidateKeyObjectKey key
                      )
              )
              matches
          publicationCounts =
            Map.fromListWith
              (+)
              [ ( checkedPublicationId
                    (StoreObservation.effectiveStoreMatchRawPublication match),
                  1 :: Int
                )
              | match <- matches
              ]
       in counterexample
            (show coordinates)
            ( coordinates == sort coordinates
                && length matches == 12
                && Map.size publicationCounts == 6
                && all (== 2) publicationCounts
                && taken == matches
                && null hidden
            )

storeReadTakeTrace ::
  Bool ->
  Either
    String
    ( [StoreObservation.EffectiveStoreMatch],
      [StoreObservation.EffectiveStoreMatch],
      [StoreObservation.EffectiveStoreMatch]
    )
storeReadTakeTrace reversePresentation = do
  replica <-
    maybe
      (Left "missing sort-definition primordial replica")
      Right
      ( find
          ((== SortDefinitionRole) . primordialReplicaRole)
          (checkedPrimordialReplicas fixtureCheckedGenesis)
      )
  initial <- mapLeft show (Store.initialState fixtureCheckedGenesis)
  preparedBootstrap <-
    mapLeft
      show
      (Store.prepareStoreBootstrap specifications initial)
  checkedPredicate <-
    mapLeft
      show
      ( DomainQuery.checkQueryPredicate
          (descriptorSchema (canonicalCheckedDescriptor (primordialReplicaDescriptor replica)))
          DomainQuery.QueryAlways
      )
  let bootstrapped = Store.commitStoreBootstrap preparedBootstrap
      descriptor = primordialReplicaDescriptor replica
      branches =
        [ resolvedQueryBranch deltaA incarnationA checkedPredicate,
          resolvedQueryBranch deltaB incarnationB checkedPredicate
        ]
      query = resolvedQuery (if reversePresentation then reverse branches else branches)
  cut <- mapLeft show (StoreObservation.captureEffectiveQueryCut query bootstrapped)
  evidence <- effectiveEvidence descriptor cut
  readPlan <- mapLeft show (StoreObservation.prepareEffectiveReadPlan evidence cut)
  takePlan <- mapLeft show (StoreObservation.prepareEffectiveLocalTakePlan evidence cut)
  preparedTake <-
    mapLeft
      show
      ( Store.prepareExactLocalTake
          (StoreObservation.effectiveLocalTakeKeys takePlan)
          bootstrapped
      )
  let (successor, _) = Store.commitExactLocalTake preparedTake
      matches = StoreObservation.effectiveReadMatches readPlan
      taken = StoreObservation.effectiveLocalTakeMatches takePlan
  hiddenCut <- mapLeft show (StoreObservation.captureEffectiveQueryCut query successor)
  hiddenEvidence <- effectiveEvidence descriptor hiddenCut
  hiddenPlan <-
    mapLeft
      show
      (StoreObservation.prepareEffectiveReadPlan hiddenEvidence hiddenCut)
  Right (matches, taken, StoreObservation.effectiveReadMatches hiddenPlan)
  where
    effectiveEvidence descriptor cut =
      Map.fromList
        <$> traverse
          ( \(key, publication) -> do
              context <-
                mapLeft
                  show
                  ( checkEffectivePublicationContext
                      descriptor
                      Controlled.emptyState
                      publication
                  )
              evidence <-
                mapLeft
                  show
                  ( StoreObservation.effectiveStoreEvidence
                      publication
                      (interpretEffectivePublication context)
                  )
              Right (key, evidence)
          )
          (StoreObservation.effectiveQueryCutRawCandidates cut)
    specifications =
      presentation
        [ Store.localStoreSpec
            (Store.ApplicationReader storeProcess SortDefinitionRole)
            deltaA
            carrierSort
            carrierOccurrence
            incarnationA,
          Store.localStoreSpec
            (Store.ApplicationReader storeProcess SortDefinitionRole)
            deltaB
            carrierSort
            carrierOccurrence
            incarnationB
        ]
    presentation = if reversePresentation then reverse else id
    carrierReplica =
      checkedMaybe
        "sort-definition replica"
        ( find
            ((== SortDefinitionRole) . primordialReplicaRole)
            (checkedPrimordialReplicas fixtureCheckedGenesis)
        )
    carrierSort = primordialReplicaSortId carrierReplica
    carrierOccurrence = primordialReplicaOccurrenceId carrierReplica

caseWaitLevels :: Assertion
caseWaitLevels = do
  let opened = openedFixture
      query = alwaysQuery Access.SortDefinitionRole opened.access
      (afterImmediate, immediateEffects) =
        runCall 30 opened.session opened.binding 1 (WaitApplication [query]) opened.state
  assertCompletedWait 1 immediateEffects
  assertEqual "an immediate wait installs no registration" [] (Wait.waitRegistrations (startupWaitState afterImmediate))

  let (afterFirstPending, firstEffects) =
        runCall 31 opened.session opened.binding 2 (WaitApplication []) afterImmediate
  firstWait <- acceptedWait 2 firstEffects
  let firstRegistration = Wait.lookupWaitRegistration firstWait (startupWaitState afterFirstPending)
  assertEqual "the empty disjunction is retained exactly" (Just []) (Wait.waitRegistrationQueries <$> firstRegistration)

  let (afterSecondPending, secondEffects) =
        runCall 32 opened.session opened.binding 3 (WaitApplication []) afterFirstPending
  secondWait <- acceptedWait 3 secondEffects
  let (afterWrongCancel, wrongEffects) =
        runRequest
          33
          (CancelApplicationPendingWait opened.binding opened.session (requestId 2) secondWait)
          afterSecondPending
  assertEqual "a wrong wait occurrence is a request conflict" [SendApplicationReply opened.binding (RequestConflict (requestId 2))] (effectBatchMembers wrongEffects)
  assertOnlyTimeAdvanced afterSecondPending afterWrongCancel
  assertBool "both pending registrations survive the wrong cancellation" (all (\wait -> Wait.lookupWaitRegistration wait (startupWaitState afterWrongCancel) /= Nothing) [firstWait, secondWait])

caseWakeBeforeCancel :: Assertion
caseWakeBeforeCancel = do
  let opened = openedFixture
      query = alwaysQuery Access.SortDefinitionRole opened.access
      (afterTake, _) =
        runCall 40 opened.session opened.binding 1 (LocalTakeApplication query) opened.state
      (afterWait, waitEffects) =
        runCall 41 opened.session opened.binding 2 (WaitApplication [query]) afterTake
  wait <- acceptedWait 2 waitEffects
  let writer = Access.predefinedWriter (accessFor Access.SortDefinitionRole opened.access)
      write =
        WriteApplication
          writer
          ( PublishValue
              (ApplicationValue.SortDefinitionValue (DeclaredSortDefinition declaredDescriptor Nothing))
          )
      (afterWake, wakeEffects) =
        runCall 42 opened.session opened.binding 3 write afterWait
  assertWriteThenWake 3 2 wait wakeEffects
  assertEqual "the wake removes the registration atomically" Nothing (Wait.lookupWaitRegistration wait (startupWaitState afterWake))

  let (afterLateCancel, lateCancelEffects) =
        runRequest
          43
          (CancelApplicationPendingWait opened.binding opened.session (requestId 2) wait)
          afterWake
  assertCompletedWait 2 lateCancelEffects
  assertOnlyTimeAdvanced afterWake afterLateCancel

data OpenedFixture = OpenedFixture
  { state :: HeraldState,
    session :: ApplicationSessionId,
    binding :: ApplicationSessionBinding,
    process :: ProcessEpochId,
    access :: Access.ApplicationStartupAccess
  }

openedFixture :: OpenedFixture
openedFixture =
  OpenedFixture
    { state = afterOpen,
      session = session,
      binding = sessionAcceptanceBinding acceptance,
      process = process,
      access = access
    }
  where
    initial =
      fst
        ( checked
            "initial Herald"
            ( initialHerald
                (monotonicInstant 10)
                fixtureCheckedGenesis
                (checkedInitial fixtureLocalBootstrapIds)
                fixtureOracleContacts
                fixtureGeneratorSeed
                fixtureApplicationRecoveryConfiguration
                fixturePeerRecoveryConfiguration
            )
        )
    attachment =
      checkedMaybe
        "primordial attachment"
        ( primordialApplicationAttachment
            fixtureCheckedGenesis
            (checkedInitial fixtureLocalBootstrapIds)
            firstLocalBootstrapId
        )
    process =
      checkedMaybe
        "attachment process"
        (ApplicationState.applicationAttachmentProcess attachment (startupApplicationState initial))
    (afterOpen, effects) =
      runStep
        11
        ( ApplicationSessionInput
            ( OpenApplicationSession
                (candidateApplicationLane 81)
                attachment
                (Session.clientNonce 1)
            )
        )
        initial
    acceptance = case effectBatchMembers effects of
      [SetApplicationConnectionDisposition _ accepted] -> accepted
      actual -> error ("unexpected open effects: " <> show actual)
    (session, access) = case sessionAcceptanceReply acceptance of
      SessionOpened openedSession _ startupAccess -> (openedSession, startupAccess)
      other -> error ("unexpected open reply: " <> show other)

alwaysQuery :: Access.ApplicationPredefinedSortRole -> Access.ApplicationStartupAccess -> Application.ApplicationQuery
alwaysQuery role access =
  Application.ApplicationQuery
    (Set.singleton (Access.predefinedReader (accessFor role access)))
    Application.QueryAlways

accessFor :: Access.ApplicationPredefinedSortRole -> Access.ApplicationStartupAccess -> Access.PredefinedAccess
accessFor role access =
  checkedMaybe
    ("predefined access " <> show role)
    (find ((== role) . Access.predefinedAccessRole) (conventionalStartupPairs access))

declaredDescriptor :: ApplicationSortDescriptor
declaredDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "key" TextSchema),
      keyProjections = [ApplicationProjection ("key" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

runCall ::
  Word64 ->
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  Word64 ->
  ApplicationOperation ->
  HeraldState ->
  (HeraldState, EffectBatch)
runCall observed session binding request call =
  runRequest
    observed
    (CallApplicationRequest binding session (requestId request) call)

runRequest :: Word64 -> ApplicationRequestIngress -> HeraldState -> (HeraldState, EffectBatch)
runRequest observed request =
  runStep observed (ApplicationRequestInput request)

runStep :: Word64 -> HeraldInputBody -> HeraldState -> (HeraldState, EffectBatch)
runStep observed body predecessor =
  checked
    "Herald step"
    (verifiedStepHerald (heraldInput (monotonicInstant observed) body) predecessor)

assertRejected :: Word64 -> ApplicationRejection -> EffectBatch -> Assertion
assertRejected request expected effects = case effectBatchMembers effects of
  [SendApplicationReply _ (RetainedRequestReply _ (Rejected actual rejection))] -> do
    assertEqual "rejected request" (requestId request) actual
    assertEqual "typed application rejection" expected rejection
  actual -> assertFailure ("unexpected rejection effects: " <> show actual)

acceptedWait :: Word64 -> EffectBatch -> IO WaitId
acceptedWait request effects = case effectBatchMembers effects of
  [SendApplicationReply _ (RetainedRequestReply _ (WaitAccepted actual wait))]
    | actual == requestId request -> pure wait
  actual -> assertFailure ("unexpected wait acceptance: " <> show actual)

assertCompletedWait :: Word64 -> EffectBatch -> Assertion
assertCompletedWait request effects = case effectBatchMembers effects of
  [ SendApplicationReply
      _
      (RetainedRequestReply _ (Completed actual (WaitCompleted WaitReady)))
    ] -> assertEqual "completed wait request" (requestId request) actual
  actual -> assertFailure ("unexpected completed-wait effects: " <> show actual)

assertWriteThenWake :: Word64 -> Word64 -> WaitId -> EffectBatch -> Assertion
assertWriteThenWake writeRequest waitRequest wait effects =
  case effectBatchMembers effects of
    [ SendApplicationReply
        _
        ( RetainedRequestReply
            _
            (Completed actualWrite (WriteCompleted (SortDefinitionWritten actualSort)))
          ),
      SendApplicationWaitWake _ _ actualWaitRequest actualWait WaitReady
      ] -> do
        assertEqual "write reply precedes wake" (requestId writeRequest) actualWrite
        assertEqual "write reports the declared SortId" declaredSortId actualSort
        assertEqual "wake request" (requestId waitRequest) actualWaitRequest
        assertEqual "wake occurrence" wait actualWait
    actual -> assertFailure ("unexpected write/wake effects: " <> show actual)

assertOnlyTimeAdvanced :: HeraldState -> HeraldState -> Assertion
assertOnlyTimeAdvanced predecessor successor =
  assertBool
    "only observation time advanced"
    ( predecessor
        == replaceStartupLastObservedTime
          (startupLastObservedTime predecessor)
          successor
    )

checkedInitial :: [BootstrapManifestId] -> CheckedInitialBootstraps
checkedInitial selected =
  checked
    "checked initial bootstraps"
    ( checkInitialBootstraps
        fixtureCheckedGenesis
        (PrimordialProcessManifest selected)
    )

firstLocalBootstrapId :: BootstrapManifestId
firstLocalBootstrapId = case fixtureLocalBootstrapIds of
  first : _ -> first
  [] -> error "fixture has no local bootstrap"

storeProcess :: ProcessEpochId
storeProcess =
  checked "store process" (mkProcessEpochId (fixtureIdentifierBytes 231))

deltaA, deltaB :: DeltaId
deltaA = checked "first store delta" (mkDeltaId (fixtureIdentifierBytes 232))
deltaB = checked "second store delta" (mkDeltaId (fixtureIdentifierBytes 233))

incarnationA, incarnationB :: StoreIncarnationId
incarnationA = checked "first store incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 234))
incarnationB = checked "second store incarnation" (mkStoreIncarnationId (fixtureIdentifierBytes 235))

mapLeft :: (problem -> String) -> Either problem value -> Either String value
mapLeft describe = either (Left . describe) Right

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

checkedMaybe :: String -> Maybe value -> value
checkedMaybe context = maybe (error (context <> ": missing")) id

declaredSortId :: SortId
declaredSortId =
  admittedApplicationSortId
    ( checked
        "admitted wait-triggering sort"
        ( admitApplicationSortDefinition
            (DeclaredSortDefinition declaredDescriptor Nothing)
        )
    )
