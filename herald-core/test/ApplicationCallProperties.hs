{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ApplicationCallProperties
  ( tests,
  )
where

import Control.Monad (void)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word16, Word64)
import Eclips.Application.Types.Access qualified as Access
import Eclips.Application.Types.Identity (PrivateNablaId, SortId)
import Eclips.Application.Types.Query
  ( ApplicationQuery (..),
    ApplicationQueryPredicate (QueryAlways),
  )
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
import Eclips.Application.Types.Value (ApplicationValue (SortDefinitionValue))
import Eclips.Application.Types.Write
  ( ApplicationWriteValue (PublishValue),
    WriteResult (SortDefinitionWritten),
  )
import Eclips.Domain.Identity (BootstrapManifestId)
import Eclips.Herald.Application.Request
  ( ApplicationOperation (..),
    ApplicationReplyCursor,
    ApplicationRequestReply (..),
    RetainedRequestReplyBody (..),
    WaitId,
    requestId,
  )
import Eclips.Herald.Application.Session
  ( ApplicationAttachment,
    ApplicationResumeToken,
    ApplicationSessionAcceptance,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionReply (SessionOpened),
    primordialApplicationAttachment,
    sessionAcceptanceBinding,
    sessionAcceptanceCursor,
    sessionAcceptanceReply,
  )
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.SortDefinition
  ( admitApplicationSortDefinition,
    admittedApplicationSortId,
  )
import Eclips.Herald.EffectBatch
  ( EffectBatch,
    HeraldEffect (..),
    effectBatchMembers,
  )
import Eclips.Herald.Genesis
  ( CheckedInitialBootstraps,
    PrimordialProcessManifest (..),
    checkInitialBootstraps,
  )
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( ApplicationRequestIngress (..),
    ApplicationSessionIngress (..),
    HeraldInputBody (..),
    RuntimeObservation (ApplicationBindingLost),
    candidateApplicationLane,
    heraldInput,
  )
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupApplicationState,
    replaceStartupControlledState,
    replaceStartupLastObservedTime,
    startupApplicationState,
    startupControlledState,
    startupLastObservedTime,
    startupStoreState,
    startupVisibilityState,
  )
import Eclips.Herald.Time (monotonicInstant)
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureGeneratorSeed,
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
  ( Positive (..),
    Property,
    counterexample,
    ioProperty,
    testProperty,
  )
import VerifiedHeraldTransition (verifiedStepHerald)

tests :: TestTree
tests =
  testGroup
    "Step-5 application-call composition"
    [ testCase "the complete local write/read/take/wait/cancel vertical is atomic" caseWorkedVertical,
      testCase "read uses one local Store visibility cut and emits only its reply" caseReadIsEntirelyLocal,
      testProperty "generated complete traces preserve all session namespaces" propGeneratedWorkedVertical,
      testCase "conflicting request reuse changes no semantic owner" caseConflictingReuse,
      testProperty "the worked pure trace is deterministic" propDeterministicTrace
    ]

caseWorkedVertical :: Assertion
caseWorkedVertical = void (workedVerticalAt 10 0)

caseReadIsEntirelyLocal :: Assertion
caseReadIsEntirelyLocal = do
  let opened = openedFixture
      definition = opened.definition
      visibleDefinition =
        DeclaredSortDefinition declaredDescriptor (Just (sortIdFor definition))
      writeCall =
        WriteApplication
          opened.writer
          (PublishValue (SortDefinitionValue definition))
      (afterWrite, writeEffects) =
        callStep 12 opened.session opened.binding 1 writeCall opened.state
  assertCompletedWrite (sortIdFor definition) 1 writeEffects

  let (afterVisibleRead, visibleReadEffects) =
        callStep
          13
          opened.session
          opened.binding
          2
          (ReadApplication opened.query)
          afterWrite
  visibleValues <- completedValues "visible local read" 2 visibleReadEffects
  assertBool
    "the local Store cut contains the value published before the read"
    (SortDefinitionValue visibleDefinition `elem` visibleValues)
  assertEntirelyLocalRead afterWrite afterVisibleRead visibleReadEffects

  let (afterTake, takeEffects) =
        callStep
          14
          opened.session
          opened.binding
          3
          (LocalTakeApplication opened.query)
          afterVisibleRead
  _ <- completedValues "local visibility-cut change" 3 takeEffects
  assertBool
    "local-take changes the Store visibility cut used by the next read"
    (startupStoreState afterTake /= startupStoreState afterVisibleRead)
  let (afterHiddenRead, hiddenReadEffects) =
        callStep
          15
          opened.session
          opened.binding
          4
          (ReadApplication opened.query)
          afterTake
  assertEqual "the next local Store cut is no longer visible" []
    =<< completedValues "hidden local read" 4 hiddenReadEffects
  assertEntirelyLocalRead afterTake afterHiddenRead hiddenReadEffects

propGeneratedWorkedVertical :: Positive Word16 -> Word16 -> Property
propGeneratedWorkedVertical (Positive timeOffset) requestOffset =
  ioProperty $ do
    void
      ( workedVerticalAt
          (100 + fromIntegral timeOffset)
          (fromIntegral requestOffset * 16)
      )
    pure True

workedVerticalAt :: Word64 -> Word64 -> IO (HeraldState, [EffectBatch])
workedVerticalAt initialTime requestBase = do
  let opened = openedFixtureAt initialTime
      request ordinal = requestBase + ordinal
      observed offset = initialTime + offset
      checkedBootstraps = checkedInitial fixtureLocalBootstrapIds
      secondAttachment =
        checkedMaybe
          "second primordial attachment"
          ( primordialApplicationAttachment
              fixtureCheckedGenesis
              checkedBootstraps
              secondLocalBootstrapId
          )
      (afterSecondOpen, secondAcceptance, secondOpenEffects) =
        openSession
          (observed 2)
          92
          2
          opened.attachment
          opened.state
      (afterThirdOpen, thirdAcceptance, thirdOpenEffects) =
        openSession
          (observed 3)
          93
          1
          secondAttachment
          afterSecondOpen
      (secondSession, secondBinding) = sessionFromAcceptance secondAcceptance
      (thirdSession, thirdBinding) = sessionFromAcceptance thirdAcceptance
      writer = opened.writer
      query = opened.query
      definition = opened.definition
      writeCall = WriteApplication writer (PublishValue (SortDefinitionValue definition))
      (afterWrite, writeEffects) =
        callStep (observed 4) opened.session opened.binding (request 1) writeCall afterThirdOpen
  assertCompletedWrite (sortIdFor definition) (request 1) writeEffects

  let (afterRetry, retryEffects) =
        callStep (observed 5) opened.session opened.binding (request 1) writeCall afterWrite
  assertEqual "exact retry reoffers the same reply" writeEffects retryEffects
  assertOnlyTimeAdvanced afterWrite afterRetry

  let (afterRead, readEffects) =
        callStep (observed 6) opened.session opened.binding (request 2) (ReadApplication query) afterRetry
  readValues <- completedValues "read" (request 2) readEffects
  assertEqual "the six primordial and one declared definitions are visible" 7 (length readValues)
  assertBool "every sort-sort value uses the structured inverse" (all isSortDefinition readValues)

  let (afterTake, takeEffects) =
        callStep (observed 7) opened.session opened.binding (request 3) (LocalTakeApplication query) afterRead
  takenValues <- completedValues "take" (request 3) takeEffects
  assertEqual "take observes the same canonical cut" readValues takenValues

  let (afterOldWriteRetry, oldWriteRetryEffects) =
        callStep (observed 8) opened.session opened.binding (request 1) writeCall afterTake
  assertEqual "retry after take reoffers the old write reply" writeEffects oldWriteRetryEffects
  assertOnlyTimeAdvanced afterTake afterOldWriteRetry

  let (afterHiddenRead, hiddenEffects) =
        callStep
          (observed 9)
          opened.session
          opened.binding
          (request 4)
          (ReadApplication query)
          afterOldWriteRetry
  assertEqual "the taken cut is hidden" []
    =<< completedValues "hidden read" (request 4) hiddenEffects

  let waitCall = WaitApplication [query]
      (afterPendingWait, waitEffects) =
        callStep (observed 10) opened.session opened.binding (request 5) waitCall afterHiddenRead
  firstWait <- acceptedWait (request 5) waitEffects
  let (afterWakeWrite, wakeEffects) =
        callStep (observed 11) opened.session opened.binding (request 6) writeCall afterPendingWait
  wakeCursor <-
    writeAndWakeCursor
      (sortIdFor definition)
      (request 6)
      (request 5)
      firstWait
      wakeEffects

  let (afterWakeRecovery, wakeRecoveryEffects) =
        stepResult
          (observed 12)
          opened.session
          opened.binding
          (request 5)
          afterWakeWrite
  assertCompletedWait (request 5) wakeCursor wakeRecoveryEffects

  let (afterSecondTake, secondTakeEffects) =
        callStep
          (observed 13)
          opened.session
          opened.binding
          (request 7)
          (LocalTakeApplication query)
          afterWakeRecovery
      (afterSecondWait, secondWaitEffects) =
        callStep (observed 14) opened.session opened.binding (request 8) waitCall afterSecondTake
  secondWait <- acceptedWait (request 8) secondWaitEffects
  let (afterCancel, cancelEffects) =
        step
          (observed 15)
          ( ApplicationRequestInput
              ( CancelApplicationPendingWait
                  opened.binding
                  opened.session
                  (requestId (request 8))
                  secondWait
              )
          )
          afterSecondWait
  assertCancelled (request 8) secondWait cancelEffects

  let (afterPostCancelWrite, postCancelWriteEffects) =
        callStep
          (observed 16)
          opened.session
          opened.binding
          (request 9)
          writeCall
          afterCancel
  assertCompletedWrite (sortIdFor definition) (request 9) postCancelWriteEffects

  let (afterLoss, lossEffects) =
        step
          (observed 17)
          (RuntimeObserved (ApplicationBindingLost opened.binding))
          afterPostCancelWrite
  assertEqual
    "binding loss immediately removes only the lost session"
    [DisposeApplicationSession opened.session Session.ApplicationSessionNoLongerLive]
    (effectBatchMembers lossEffects)
  let (afterResume, resumeEffects) =
        step
          (observed 18)
          (ApplicationSessionInput (ResumeApplicationSession (candidateApplicationLane 94) opened.session opened.resumeToken opened.openCursor))
          afterLoss
  assertSessionNotLive "the lost session cannot resume" resumeEffects
  let (afterResult, resultEffects) = stepResult (observed 19) opened.session opened.binding (request 8) afterResume
  assertSessionNotLive "retained results die with the application session" resultEffects
  let (afterEnd, endEffects) =
        step
          (observed 20)
          (ApplicationSessionInput (EndApplicationSession opened.binding opened.session))
          afterResult
  assertSessionNotLive "the ended session has no remaining orderly-close owner" endEffects
  let (afterRejectedResult, resultAfterEnd) = stepResult (observed 21) opened.session opened.binding (request 8) afterEnd
  assertSessionNotLive "the ended session remains unqueryable" resultAfterEnd

  let emptyQuery = ApplicationQuery Set.empty QueryAlways
      (afterSameProcessRead, sameProcessEffects) =
        callStep
          (observed 22)
          secondSession
          secondBinding
          (request 20)
          (ReadApplication emptyQuery)
          afterRejectedResult
      (finalState, otherProcessEffects) =
        callStep
          (observed 23)
          thirdSession
          thirdBinding
          (request 20)
          (ReadApplication emptyQuery)
          afterSameProcessRead
  assertEqual "the other same-process session survives" []
    =<< completedValues "same-process session" (request 20) sameProcessEffects
  assertEqual "the other process session remains isolated" []
    =<< completedValues "other-process session" (request 20) otherProcessEffects
  pure
    ( finalState,
      [ secondOpenEffects,
        thirdOpenEffects,
        writeEffects,
        retryEffects,
        readEffects,
        takeEffects,
        oldWriteRetryEffects,
        hiddenEffects,
        waitEffects,
        wakeEffects,
        wakeRecoveryEffects,
        secondTakeEffects,
        secondWaitEffects,
        cancelEffects,
        postCancelWriteEffects,
        lossEffects,
        resumeEffects,
        resultEffects,
        endEffects,
        resultAfterEnd,
        sameProcessEffects,
        otherProcessEffects
      ]
    )

assertSessionNotLive :: String -> EffectBatch -> Assertion
assertSessionNotLive context effects = case effectBatchMembers effects of
  [RejectApplicationConnection _ Session.ApplicationSessionNotLive] -> pure ()
  actual -> assertFailure (context <> ": " <> show actual)

caseConflictingReuse :: Assertion
caseConflictingReuse = do
  let opened = openedFixture
      original = ReadApplication opened.query
      conflict = LocalTakeApplication opened.query
      (afterOriginal, _) = callStep 12 opened.session opened.binding 9 original opened.state
      (afterConflict, effects) = callStep 13 opened.session opened.binding 9 conflict afterOriginal
  case effectBatchMembers effects of
    [SendApplicationReply binding (RequestConflict request)] -> do
      assertEqual "conflict targets the current binding" opened.binding binding
      assertEqual "conflict retains the request ID" (requestId 9) request
    actual -> assertFailure ("unexpected conflict effects: " <> show actual)
  assertOnlyTimeAdvanced afterOriginal afterConflict

propDeterministicTrace :: Positive Word16 -> Word16 -> Property
propDeterministicTrace (Positive timeOffset) requestOffset =
  ioProperty $ do
    let observed = 100 + fromIntegral timeOffset
        requestBase = fromIntegral requestOffset * 16
    left <- workedVerticalAt observed requestBase
    right <- workedVerticalAt observed requestBase
    pure
      ( counterexample
          "equal initialized state/input trace diverged"
          (left == right)
      )

data OpenedFixture = OpenedFixture
  { state :: HeraldState,
    session :: ApplicationSessionId,
    binding :: ApplicationSessionBinding,
    attachment :: ApplicationAttachment,
    resumeToken :: ApplicationResumeToken,
    openCursor :: ApplicationReplyCursor,
    writer :: PrivateNablaId,
    query :: ApplicationQuery,
    definition :: ApplicationSortDefinition
  }

openedFixture :: OpenedFixture
openedFixture = openedFixtureAt 10

openedFixtureAt :: Word64 -> OpenedFixture
openedFixtureAt initialTime =
  OpenedFixture
    { state = afterOpen,
      session = session,
      binding = sessionAcceptanceBinding acceptance,
      attachment = attachment,
      resumeToken = resumeToken,
      openCursor = sessionAcceptanceCursor acceptance,
      writer = Access.predefinedWriter sortAccess,
      query =
        ApplicationQuery
          { applicationQueryDeltas = Set.singleton (Access.predefinedReader sortAccess),
            applicationQueryPredicate = QueryAlways
          },
      definition =
        DeclaredSortDefinition declaredDescriptor Nothing
    }
  where
    checkedBootstraps = checkedInitial fixtureLocalBootstrapIds
    initial =
      fst
        ( checked
            "initial Herald"
            ( initialHerald
                (monotonicInstant initialTime)
                fixtureCheckedGenesis
                checkedBootstraps
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
            checkedBootstraps
            firstLocalBootstrapId
        )
    (afterOpen, openEffects) =
      step
        (initialTime + 1)
        ( ApplicationSessionInput
            ( OpenApplicationSession
                (candidateApplicationLane 91)
                attachment
                (Session.clientNonce 1)
            )
        )
        initial
    acceptance = case effectBatchMembers openEffects of
      [SetApplicationConnectionDisposition _ accepted] -> accepted
      actual -> error ("unexpected open effects: " <> show actual)
    (session, resumeToken, startupAccess) = case sessionAcceptanceReply acceptance of
      SessionOpened openedSession token access -> (openedSession, token, access)
      other -> error ("unexpected open reply: " <> show other)
    sortAccess =
      checkedMaybe
        "sort-definition startup access"
        ( find
            ((== Access.SortDefinitionRole) . Access.predefinedAccessRole)
            (conventionalStartupPairs startupAccess)
        )

declaredDescriptor :: ApplicationSortDescriptor
declaredDescriptor =
  ApplicationSortDescriptor
    { sortKind = RegularSort,
      valueSchema = RecordSchema (Map.singleton "key" TextSchema),
      keyProjections =
        [ApplicationProjection ("key" :| [])],
      validityPredicate = AlwaysPredicate,
      obsolescencePredicate = NeverPredicate,
      rankTerms = RankApplicationValue Ascending :| [],
      minimumRetentionMicros = 0,
      isImmutable = False,
      labelField = Nothing
    }

openSession ::
  Word64 ->
  Word64 ->
  Word64 ->
  ApplicationAttachment ->
  HeraldState ->
  (HeraldState, ApplicationSessionAcceptance, EffectBatch)
openSession observed lane nonce attachment predecessor =
  (successor, acceptance, effects)
  where
    (successor, effects) =
      step
        observed
        ( ApplicationSessionInput
            ( OpenApplicationSession
                (candidateApplicationLane lane)
                attachment
                (Session.clientNonce nonce)
            )
        )
        predecessor
    acceptance = case effectBatchMembers effects of
      [SetApplicationConnectionDisposition _ accepted] -> accepted
      actual -> error ("unexpected open effects: " <> show actual)

sessionFromAcceptance ::
  ApplicationSessionAcceptance ->
  (ApplicationSessionId, ApplicationSessionBinding)
sessionFromAcceptance acceptance =
  case sessionAcceptanceReply acceptance of
    SessionOpened session _ _ -> (session, sessionAcceptanceBinding acceptance)
    other -> error ("unexpected session acceptance: " <> show other)

callStep ::
  Word64 ->
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  Word64 ->
  ApplicationOperation ->
  HeraldState ->
  (HeraldState, EffectBatch)
callStep observed session binding request call =
  step
    observed
    ( ApplicationRequestInput
        ( CallApplicationRequest
            binding
            session
            (requestId request)
            call
        )
    )

stepResult ::
  Word64 ->
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  Word64 ->
  HeraldState ->
  (HeraldState, EffectBatch)
stepResult observed session binding request =
  step
    observed
    ( ApplicationRequestInput
        (GetApplicationRequestResult binding session (requestId request))
    )

step :: Word64 -> HeraldInputBody -> HeraldState -> (HeraldState, EffectBatch)
step observed body state =
  checked
    "Herald step"
    (verifiedStepHerald (heraldInput (monotonicInstant observed) body) state)

assertCompletedWrite :: SortId -> Word64 -> EffectBatch -> Assertion
assertCompletedWrite expectedSort request effects = case effectBatchMembers effects of
  [ SendApplicationReply
      _
      ( RetainedRequestReply
          _
          (Completed actual (WriteCompleted (SortDefinitionWritten actualSort)))
        )
    ] -> do
      assertEqual "write request" (requestId request) actual
      assertEqual "written sort" expectedSort actualSort
  actual -> assertFailure ("unexpected write effects: " <> show actual)

completedValues :: String -> Word64 -> EffectBatch -> IO [ApplicationValue]
completedValues context request effects = case effectBatchMembers effects of
  [SendApplicationReply _ (RetainedRequestReply _ (Completed actual result))]
    | actual == requestId request -> case result of
        ReadCompleted values -> pure values
        LocalTakeCompleted values -> pure values
        other -> assertFailure (context <> ": unexpected result " <> show other)
  actual -> assertFailure (context <> ": unexpected effects " <> show actual)

acceptedWait :: Word64 -> EffectBatch -> IO WaitId
acceptedWait request effects = case effectBatchMembers effects of
  [SendApplicationReply _ (RetainedRequestReply _ (WaitAccepted actual wait))]
    | actual == requestId request -> pure wait
  actual -> assertFailure ("unexpected wait acceptance: " <> show actual)

writeAndWakeCursor ::
  SortId ->
  Word64 ->
  Word64 ->
  WaitId ->
  EffectBatch ->
  IO ApplicationReplyCursor
writeAndWakeCursor expectedSort writeRequest waitRequest wait effects =
  case effectBatchMembers effects of
    [ SendApplicationReply
        _
        ( RetainedRequestReply
            _
            (Completed actualWrite (WriteCompleted (SortDefinitionWritten actualSort)))
          ),
      SendApplicationWaitWake _ cursor actualWaitRequest actualWait WaitReady
      ] -> do
        assertEqual "triggering write first" (requestId writeRequest) actualWrite
        assertEqual "triggering written sort" expectedSort actualSort
        assertEqual "wake request" (requestId waitRequest) actualWaitRequest
        assertEqual "wake occurrence" wait actualWait
        pure cursor
    actual -> fail ("unexpected write/wake effects: " <> show actual)

assertCompletedWait ::
  Word64 ->
  ApplicationReplyCursor ->
  EffectBatch ->
  Assertion
assertCompletedWait request expectedCursor effects =
  case effectBatchMembers effects of
    [ SendApplicationReply
        _
        ( RetainedRequestReply
            cursor
            (Completed actualRequest (WaitCompleted WaitReady))
          )
      ] -> do
        assertEqual "recovered wait request" (requestId request) actualRequest
        assertEqual "the wake and cached result share one cursor" expectedCursor cursor
    actual -> assertFailure ("unexpected recovered wait effects: " <> show actual)

assertCancelled :: Word64 -> WaitId -> EffectBatch -> Assertion
assertCancelled request wait effects = case effectBatchMembers effects of
  [SendApplicationReply _ (RetainedRequestReply _ (Cancelled actualRequest actualWait))] -> do
    assertEqual "cancelled request" (requestId request) actualRequest
    assertEqual "cancelled wait" wait actualWait
  actual -> assertFailure ("unexpected cancellation effects: " <> show actual)

isSortDefinition :: ApplicationValue -> Bool
isSortDefinition (SortDefinitionValue _) = True
isSortDefinition _ = False

sortIdFor :: ApplicationSortDefinition -> SortId
sortIdFor definition =
  admittedApplicationSortId
    (checked "admitted declared sort" (admitApplicationSortDefinition definition))

assertOnlyTimeAdvanced :: HeraldState -> HeraldState -> Assertion
assertOnlyTimeAdvanced predecessor successor =
  assertBool
    "only observation time changed"
    ( predecessor
        == replaceStartupLastObservedTime
          (startupLastObservedTime predecessor)
          successor
    )

assertEntirelyLocalRead :: HeraldState -> HeraldState -> EffectBatch -> Assertion
assertEntirelyLocalRead predecessor successor effects = do
  case effectBatchMembers effects of
    [SendApplicationReply _ (RetainedRequestReply _ (Completed _ (ReadCompleted _)))] ->
      pure ()
    actual ->
      assertFailure
        ( "read emitted work other than its application reply: "
            <> show actual
        )
  assertBool
    "read preserves the exact local Store visibility cut"
    (startupStoreState successor == startupStoreState predecessor)
  assertBool
    "read preserves the visibility-work owner"
    (startupVisibilityState successor == startupVisibilityState predecessor)
  assertBool
    "read changes only time and local application/controlled bookkeeping"
    ( predecessor
        == replaceStartupLastObservedTime
          (startupLastObservedTime predecessor)
          ( replaceStartupApplicationState
              (startupApplicationState predecessor)
              ( replaceStartupControlledState
                  (startupControlledState predecessor)
                  successor
              )
          )
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

secondLocalBootstrapId :: BootstrapManifestId
secondLocalBootstrapId = case fixtureLocalBootstrapIds of
  _ : second : _ -> second
  _ -> error "fixture has no second local bootstrap"

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

checkedMaybe :: String -> Maybe value -> value
checkedMaybe context = maybe (error (context <> ": missing")) id
