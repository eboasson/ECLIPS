{-# LANGUAGE OverloadedRecordDot #-}

module ApplicationSessionProperties
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.Set qualified as Set
import Data.Word (Word32, Word64)
import Eclips.Application.Types.Access
  ( ApplicationStartupAccess,
    allApplicationPredefinedSortRoles,
    predefinedAccessRole,
    predefinedReader,
    predefinedWriter,
    startupAccessProcess,
  )
import Eclips.Application.Types.Identity
  ( privateDeltaUniqueId,
    privateNablaUniqueId,
    privateProcessUniqueId,
    privateUniqueIdWord64,
  )
import Eclips.Application.Types.Value qualified as Application
import Eclips.Domain.Identity
  ( BootstrapManifestId,
    HeraldEpoch,
    ProcessEpochId,
    mkBootstrapManifestId,
    mkGlobalUniqueId,
  )
import Eclips.Domain.Startup
  ( AppliedProcessBootstrap,
    appliedBootstrapManifestId,
    appliedProcessEpochId,
  )
import Eclips.Domain.Value qualified as Domain
import Eclips.Herald.Application.Request.Internal
  ( ApplicationReplyCursor,
    nextApplicationReplyCursor,
  )
import Eclips.Herald.Application.Session.Internal
  ( ApplicationAttachment,
    ApplicationResumeToken,
    ApplicationSessionAcceptance,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionRejection (..),
    ApplicationSessionReply (..),
    applicationAttachmentForBootstrap,
    applicationResumeTokenOrdinal,
    applicationSessionIdOrdinal,
    clientNonce,
    primordialApplicationAttachment,
    sessionAcceptanceBinding,
    sessionAcceptanceCursor,
    sessionAcceptanceReply,
    sessionBindingGeneration,
    sessionBindingGenerationWord64,
  )
import Eclips.Herald.Application.State
  ( ApplicationSessionTransitionError (..),
    BootstrapAccess,
    RootAccess (..),
    State,
    applicationAttachmentProcess,
    applicationBootstrapAccess,
    applicationPrivateIdentity,
    applicationProcessRecoveryEntries,
    applicationSessionRecoveryEntries,
    applicationSessionWitnessId,
    applicationSessionWitnessStartupAccess,
    applicationStateWitness,
    applicationWitnessSessions,
    bootstrapAccessProcess,
    bootstrapAccessRoots,
    commitApplicationBindingLoss,
    commitApplicationSessionAcceptance,
    commitApplicationSessionEnd,
    commitOrdinaryApplicationValueLocalization,
    globalizeOrdinaryApplicationValue,
    prepareApplicationBindingLoss,
    prepareApplicationSessionEnd,
    prepareApplicationSessionOpen,
    prepareApplicationSessionResume,
    prepareOrdinaryApplicationValueLocalization,
    processRoleView,
  )
import Eclips.Herald.Application.State qualified as ApplicationState
import Eclips.Herald.Bootstrap
  ( BootstrapInvariant,
    BootstrapOwners,
    BootstrapViews,
    bootstrapApplicationState,
    bootstrapViews,
    commitProcessBootstrap,
    initialBootstrapOwners,
    prepareProcessBootstrap,
  )
import Eclips.Herald.Genesis
  ( CheckedInitialBootstraps,
    PrimordialProcessManifest (..),
    checkInitialBootstraps,
  )
import Eclips.Herald.Genesis.Internal
  ( checkedInitialBootstraps,
    checkedLocalHeraldEpoch,
  )
import Eclips.Herald.Input (ApplicationSessionIngress (..), candidateApplicationLane)
import Eclips.Herald.OracleProjection.State qualified as OracleProjection
import Eclips.Herald.ProcessPreparation.State qualified as Preparation
import Eclips.Herald.SortRegistry.State qualified as SortRegistry
import Eclips.Herald.Time (monotonicInstant)
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureLocalBootstrapIds,
    fixtureRemoteBootstrapId,
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

tests :: TestTree
tests =
  testGroup
    "application attachments and sessions"
    [ testCase "primordial attachments exist exactly for selected local processes" caseAttachmentFixture,
      testCase "open returns the canonical process-private startup bundle" caseStartupAccess,
      testCase "same-process sessions share access but isolate session identities" caseSameProcessSessions,
      testCase "the same nonce is independent in another process" caseProcessNonceIsolation,
      testProperty "a live nonce retry is allocation-idempotent" propOpenRetry,
      testProperty "preparation session changes name only actual removals and coalesce until consumed" propPreparationSessionChanges,
      testCase "an open retry after binding loss restores the original delivery" caseOpenRetryAfterLoss,
      testCase "end releases a nonce and preserves the process owner" caseEndAndReopen,
      testCase "resume advances one binding generation and rejects the old binding" caseResume,
      testCase "an exact resume retry after lost acceptance reoffers without advancing" caseResumeRetryAfterLoss,
      testCase "wrong session inputs reject without changing the owner" caseWrongInputs,
      testProperty "membership admission defers grants and preserves exact live retries" propMembershipSessionGate,
      testCase "binding loss preserves the logical session and permits resume" caseBindingLoss,
      testCase "stale and duplicate binding-loss observations are no-ops" caseStaleBindingLoss,
      testCase "ending one session leaves another session and its process map usable" caseEndIsolation
    ]

propMembershipSessionGate :: Word32 -> Property
propMembershipSessionGate nonceValue =
  counterexample "membership admission allocated or recovered a binding while closed"
    $ pending (prepareApplicationSessionOpen fixture.herald fixture.firstAttachment (clientNonce nonce) gatedInitial)
      && pending (prepareApplicationSessionResume session token originalCursor gatedOpen)
      && retainedOpen == accepted
      && retainedResume == resumed
      && pending (prepareApplicationSessionOpen fixture.herald fixture.firstAttachment (clientNonce nonce) gatedLoss)
      && pending (prepareApplicationSessionResume session token originalCursor gatedLoss)
      && recovered == resumed
      && Preparation.deferredSessionEntries correlations == [(lane, request)]
      && null (Preparation.deferredSessionEntries (Preparation.removeDeferredSession lane correlations))
      && null (Preparation.deferredSessionEntries (Preparation.observeCandidateLoss lane correlations))
      && Preparation.deferredSessionEntries reopenedCorrelation == [(replacementLane, replacementRequest)]
      && case prepareApplicationSessionEnd session (sessionAcceptanceBinding resumed) gatedResumed of Right _ -> True; _ -> False
  where
    fixture = sessionFixture
    nonce = fromIntegral nonceValue
    gatedInitial = ApplicationState.setApplicationMembershipGate True fixture.state
    (afterOpen, accepted) = open fixture.herald fixture.firstAttachment nonce fixture.state
    (session, token, _) = opened accepted
    originalCursor = sessionAcceptanceCursor accepted
    gatedOpen = ApplicationState.setApplicationMembershipGate True afterOpen
    (_, retainedOpen) = open fixture.herald fixture.firstAttachment nonce gatedOpen
    (afterResume, resumed) = resume session token originalCursor afterOpen
    gatedResumed = ApplicationState.setApplicationMembershipGate True afterResume
    (_, retainedResume) = resume session token originalCursor gatedResumed
    afterLoss = commitApplicationBindingLoss (prepareApplicationBindingLoss fixtureApplicationRecoveryConfiguration (monotonicInstant 0) (sessionAcceptanceBinding resumed) afterResume)
    gatedLoss = ApplicationState.setApplicationMembershipGate True afterLoss
    (_, recovered) = resume session token originalCursor (ApplicationState.setApplicationMembershipGate False gatedLoss)
    pending (Left ApplicationSessionGatePending) = True
    pending _ = False
    lane = candidateApplicationLane nonce
    request = ResumeApplicationSession lane session token originalCursor
    correlations = checkedPure "retain pending session correlation" (Preparation.retainDeferredSession request Preparation.emptyState >>= Preparation.retainDeferredSession request)
    replacementLane = candidateApplicationLane (nonce + 1)
    replacementRequest = OpenApplicationSession replacementLane fixture.firstAttachment (clientNonce nonce)
    reopenedCorrelation =
      checkedPure
        "live replacement after candidate close"
        (Preparation.retainDeferredSession replacementRequest (Preparation.observeCandidateLoss lane correlations))

caseAttachmentFixture :: Assertion
caseAttachmentFixture = do
  let selected = checkedBootstraps fixtureLocalBootstrapIds
      (bootstraps, state) = bootstrapped selected
  mapM_
    ( \bootstrap -> do
        let manifest = appliedBootstrapManifestId bootstrap
            process = appliedProcessEpochId bootstrap
        attachment <-
          maybe
            (assertFailure "selected local manifest did not yield an attachment")
            pure
            ( primordialApplicationAttachment
                fixtureCheckedGenesis
                selected
                manifest
            )
        assertEqual
          "attachment resolves to the installed local process"
          (Just process)
          (applicationAttachmentProcess attachment state)
    )
    bootstraps
  let onlyFirst = checkedBootstraps (take 1 fixtureLocalBootstrapIds)
  case drop 1 fixtureLocalBootstrapIds of
    unselected : _ ->
      assertEqual
        "known but unselected local manifest has no attachment"
        Nothing
        ( primordialApplicationAttachment
            fixtureCheckedGenesis
            onlyFirst
            unselected
        )
    [] -> assertFailure "fixture has fewer than two local bootstraps"
  let includingRemote =
        checkedBootstraps
          (fixtureLocalBootstrapIds <> [fixtureRemoteBootstrapId])
  assertEqual
    "selected nonresident manifest has no local attachment"
    Nothing
    ( primordialApplicationAttachment
        fixtureCheckedGenesis
        includingRemote
        fixtureRemoteBootstrapId
    )

caseStartupAccess :: Assertion
caseStartupAccess = do
  let fixture = sessionFixture
      (successor, accepted) =
        open fixture.herald fixture.firstAttachment 3 fixture.state
      (_, _, startup) = opened accepted
      access = bootstrapAccess fixture.firstProcess fixture.state
  assertEqual
    "startup process handle comes from the installed bootstrap access"
    (bootstrapAccessProcess access)
    (startupAccessProcess startup)
  assertEqual
    "startup catalogue is the complete canonical six-role sequence"
    allApplicationPredefinedSortRoles
    (fmap predefinedAccessRole (conventionalStartupPairs startup))
  assertEqual
    "startup writer/reader handles are the exact internal canonical roots"
    (fmap rootPrivateIdentity (bootstrapAccessRoots access))
    ( concatMap
        ( \entry ->
            [ privateNablaUniqueId (predefinedWriter entry),
              privateDeltaUniqueId (predefinedReader entry)
            ]
        )
        (conventionalStartupPairs startup)
    )
  assertBool "open produces a successor" (successor /= fixture.state)
  where
    rootPrivateIdentity = \case
      WriterAccess _ writer _ -> privateNablaUniqueId writer
      ReaderAccess _ reader -> privateDeltaUniqueId reader

caseSameProcessSessions :: Assertion
caseSameProcessSessions = do
  let fixture = sessionFixture
      (afterFirst, first) =
        open fixture.herald fixture.firstAttachment 10 fixture.state
      (afterSecond, second) =
        open fixture.herald fixture.firstAttachment 11 afterFirst
      (firstSession, firstToken, firstAccess) = opened first
      (secondSession, secondToken, secondAccess) = opened second
  assertEqual "sessions share exact startup access" firstAccess secondAccess
  assertBool "session IDs are isolated" (firstSession /= secondSession)
  assertBool "resume tokens are isolated" (firstToken /= secondToken)
  assertEqual
    "the token uses the same allocation ordinal as its session"
    (applicationSessionIdOrdinal firstSession)
    (applicationResumeTokenOrdinal firstToken)
  assertBool
    "logical bindings are isolated"
    (sessionAcceptanceBinding first /= sessionAcceptanceBinding second)
  assertBool "two opens changed state" (afterSecond /= fixture.state)

caseProcessNonceIsolation :: Assertion
caseProcessNonceIsolation = do
  let fixture = sessionFixture
      (afterFirst, first) =
        open fixture.herald fixture.firstAttachment 17 fixture.state
      (afterSecond, second) =
        open fixture.herald fixture.secondAttachment 17 afterFirst
      (firstSession, firstToken, firstAccess) = opened first
      (secondSession, secondToken, secondAccess) = opened second
  assertBool "same nonce gives distinct sessions across processes" (firstSession /= secondSession)
  assertBool "same nonce gives distinct tokens across processes" (firstToken /= secondToken)
  assertEqual
    "each fresh process namespace begins with the same private process bits"
    ( privateUniqueIdWord64
        (privateProcessUniqueId (startupAccessProcess firstAccess))
    )
    ( privateUniqueIdWord64
        (privateProcessUniqueId (startupAccessProcess secondAccess))
    )
  let globalized process startup =
        globalizeOrdinaryApplicationValue
          (processRoleView [])
          process
          ( Application.UniqueIdValue
              (privateProcessUniqueId (startupAccessProcess startup))
          )
          afterSecond
  assertBool
    "equal private bits resolve only in their session process namespace"
    ( globalized fixture.firstProcess firstAccess
        /= globalized fixture.secondProcess secondAccess
    )

propOpenRetry :: Word32 -> Property
propOpenRetry generatedNonce =
  let fixture = sessionFixture
      nonceWord = fromIntegral generatedNonce
      (afterFirst, first) =
        openWithNonce fixture.herald fixture.firstAttachment nonceWord fixture.state
      (afterRetry, retried) =
        openWithNonce fixture.herald fixture.firstAttachment nonceWord afterFirst
      (afterNext, next) =
        openWithNonce fixture.herald fixture.firstAttachment (nonceWord + 1) afterRetry
      (firstSession, firstToken, _) = opened first
      (retrySession, retryToken, _) = opened retried
      (nextSession, _, _) = opened next
   in counterexample
        "retry changed identity or consumed an allocation"
        ( firstSession == retrySession
            && firstToken == retryToken
            && sessionAcceptanceBinding first == sessionAcceptanceBinding retried
            && applicationSessionIdOrdinal nextSession
              == applicationSessionIdOrdinal firstSession + 1
            && afterRetry == afterFirst
            && afterNext /= afterRetry
        )

propPreparationSessionChanges :: Word32 -> Property
propPreparationSessionChanges generatedNonce =
  let fixture = sessionFixture
      nonceWord = fromIntegral generatedNonce
      base = ApplicationState.clearPreparationSessionChanges fixture.state
      (afterFirst, first) = openWithNonce fixture.herald fixture.firstAttachment nonceWord base
      (afterSecond, second) = openWithNonce fixture.herald fixture.firstAttachment (nonceWord + 1) afterFirst
      (firstSession, _, _) = opened first
      (secondSession, _, _) = opened second
      ended = end firstSession (sessionAcceptanceBinding first) afterSecond
      (endedChanges, drained) = ApplicationState.takePreparationSessionChanges ended
      (retired, _) = ApplicationState.commitApplicationProcessRetirement (ApplicationState.prepareApplicationProcessRetirement fixture.firstProcess ended)
      (retiredChanges, retiredDrained) = ApplicationState.takePreparationSessionChanges retired
      (retirementReplay, _) = ApplicationState.commitApplicationProcessRetirement (ApplicationState.prepareApplicationProcessRetirement fixture.firstProcess retiredDrained)
      terminated = ApplicationState.commitApplicationRecoverySweep (checkedPure "exact binding termination" (ApplicationState.prepareApplicationBindingTermination (sessionAcceptanceBinding first) afterSecond))
      (terminationChanges, terminationDrained) = ApplicationState.takePreparationSessionChanges terminated
      terminationReplay = ApplicationState.commitApplicationRecoverySweep (checkedPure "stale binding termination" (ApplicationState.prepareApplicationBindingTermination (sessionAcceptanceBinding first) terminationDrained))
      recoverableLoss = commitApplicationBindingLoss (prepareApplicationBindingLoss fixtureApplicationRecoveryConfiguration (monotonicInstant 0) (sessionAcceptanceBinding first) afterSecond)
      changes = fst . ApplicationState.takePreparationSessionChanges
   in counterexample
        "session removal notifications differed from the checked lifecycle changes"
        ( Set.null (changes afterSecond)
            && Set.null (changes recoverableLoss)
            && endedChanges == Set.singleton firstSession
            && Set.null (changes drained)
            && retiredChanges == Set.fromList [firstSession, secondSession]
            && Set.null (changes retirementReplay)
            && terminationChanges == Set.singleton firstSession
            && Set.null (changes terminationReplay)
            && ApplicationState.applicationSessionProcess secondSession terminated == Just fixture.firstProcess
        )

caseOpenRetryAfterLoss :: Assertion
caseOpenRetryAfterLoss = do
  let fixture = sessionFixture
      (afterOpen, accepted) =
        open fixture.herald fixture.firstAttachment 19 fixture.state
      binding = sessionAcceptanceBinding accepted
      (session, _, _) = opened accepted
      afterLoss =
        commitApplicationBindingLoss
          (prepareApplicationBindingLoss fixtureApplicationRecoveryConfiguration (monotonicInstant 0) binding afterOpen)
      (afterRetry, retried) =
        open fixture.herald fixture.firstAttachment 19 afterLoss
      (_, next) =
        open fixture.herald fixture.firstAttachment 20 afterRetry
      (nextSession, _, _) = opened next
  assertEqual
    "retry restores the original session, token, startup access, and binding"
    accepted
    retried
  assertBool
    "reoffering restores the pre-loss semantic session owner"
    (applicationStateWitness afterRetry == applicationStateWitness afterOpen)
  assertEqual
    "reoffering clears the active session recovery"
    []
    (applicationSessionRecoveryEntries afterRetry)
  assertBool
    "reoffering retains generation history outside the semantic witness"
    ( applicationProcessRecoveryEntries afterRetry
        /= applicationProcessRecoveryEntries afterOpen
    )
  assertEqual
    "retry after loss does not consume a session allocation"
    (applicationSessionIdOrdinal session + 1)
    (applicationSessionIdOrdinal nextSession)

caseEndAndReopen :: Assertion
caseEndAndReopen = do
  let fixture = sessionFixture
      (afterOpen, first) =
        open fixture.herald fixture.firstAttachment 21 fixture.state
      (firstSession, _, _) = opened first
      afterEnd =
        end
          firstSession
          (sessionAcceptanceBinding first)
          afterOpen
      (afterReopen, second) =
        open fixture.herald fixture.firstAttachment 21 afterEnd
      (secondSession, _, _) = opened second
  assertBool "reopen after end allocates a fresh session" (secondSession /= firstSession)
  assertEqual
    "the process-scoped bootstrap access survives session end"
    (applicationBootstrapAccess fixture.firstProcess fixture.state)
    (applicationBootstrapAccess fixture.firstProcess afterReopen)
  assertBool
    "the process-scoped identity map survives session end"
    (applicationPrivateIdentity fixture.state == applicationPrivateIdentity afterEnd)

caseResume :: Assertion
caseResume = do
  let fixture = sessionFixture
      (afterOpen, accepted) =
        open fixture.herald fixture.firstAttachment 24 fixture.state
      (session, token, _) = opened accepted
      previousBinding = sessionAcceptanceBinding accepted
      lastObserved = sessionAcceptanceCursor accepted
      (afterResume, resumed) =
        resume session token lastObserved afterOpen
      currentBinding = sessionAcceptanceBinding resumed
  assertEqual
    "resume advances exactly one generation"
    (sessionBindingGenerationWord64 (sessionBindingGeneration previousBinding) + 1)
    (sessionBindingGenerationWord64 (sessionBindingGeneration currentBinding))
  assertBool "resume commits the new logical binding" (afterResume /= afterOpen)
  case sessionAcceptanceReply resumed of
    SessionResumed resumedSession _ ->
      assertEqual "resume acknowledgement names the same session" session resumedSession
    reply -> assertFailure ("expected resume acknowledgement, got " <> show reply)

caseResumeRetryAfterLoss :: Assertion
caseResumeRetryAfterLoss = do
  let fixture = sessionFixture
      (afterOpen, openedAcceptance) =
        open fixture.herald fixture.firstAttachment 27 fixture.state
      (session, token, _) = opened openedAcceptance
      originalCursor = sessionAcceptanceCursor openedAcceptance
      (afterResume, resumed) =
        resume session token originalCursor afterOpen
      currentBinding = sessionAcceptanceBinding resumed
      currentCursor = sessionAcceptanceCursor resumed
      afterLoss =
        commitApplicationBindingLoss
          (prepareApplicationBindingLoss fixtureApplicationRecoveryConfiguration (monotonicInstant 0) currentBinding afterResume)
      (afterRetry, retried) =
        resume session token originalCursor afterLoss
      (afterNextResume, nextResume) =
        resume session token currentCursor afterRetry
      (_, nextSessionAcceptance) =
        open fixture.herald fixture.firstAttachment 28 afterRetry
      (nextSession, _, _) = opened nextSessionAcceptance
  assertEqual
    "exact retry reoffers the committed current binding and acknowledgement"
    resumed
    retried
  assertBool
    "exact retry restores the pre-loss semantic session owner"
    (applicationStateWitness afterRetry == applicationStateWitness afterResume)
  assertEqual
    "exact retry clears the active session recovery"
    []
    (applicationSessionRecoveryEntries afterRetry)
  assertEqual
    "a later new resume advances from the reoffered current binding"
    (sessionBindingGenerationWord64 (sessionBindingGeneration currentBinding) + 1)
    ( sessionBindingGenerationWord64
        (sessionBindingGeneration (sessionAcceptanceBinding nextResume))
    )
  assertRejectedUnchanged
    "an unissued future cursor is rejected"
    ApplicationReplyCursorNotIssued
    afterNextResume
    ( prepareApplicationSessionResume
        session
        token
        (nextApplicationReplyCursor (sessionAcceptanceCursor nextResume))
        afterNextResume
    )
  assertEqual
    "resume retry consumes no session allocation"
    (applicationSessionIdOrdinal session + 1)
    (applicationSessionIdOrdinal nextSession)

caseWrongInputs :: Assertion
caseWrongInputs = do
  let fixture = sessionFixture
      unknownAttachment =
        applicationAttachmentForBootstrap
          ( checkedPure
              "unknown bootstrap manifest"
              (mkBootstrapManifestId (ByteString.replicate 32 0xfa))
          )
  assertRejectedUnchanged
    "unknown attachment"
    ApplicationAttachmentNotAdmitted
    fixture.state
    ( prepareApplicationSessionOpen
        fixture.herald
        unknownAttachment
        (clientNonce 30)
        fixture.state
    )
  let (afterFirst, first) =
        open fixture.herald fixture.firstAttachment 31 fixture.state
      (afterSecond, second) =
        open fixture.herald fixture.firstAttachment 32 afterFirst
      (firstSession, _, _) = opened first
      (secondSession, secondToken, _) = opened second
      secondBinding = sessionAcceptanceBinding second
      firstCursor = sessionAcceptanceCursor first
      secondCursor = sessionAcceptanceCursor second
  assertRejectedUnchanged
    "wrong token"
    ApplicationResumeTokenMismatch
    afterSecond
    ( prepareApplicationSessionResume
        firstSession
        secondToken
        firstCursor
        afterSecond
    )
  assertRejectedUnchanged
    "end binding from another session"
    ApplicationEndSessionBindingMismatch
    afterSecond
    (prepareApplicationSessionEnd firstSession secondBinding afterSecond)
  let afterSecondEnd = end secondSession secondBinding afterSecond
  assertRejectedUnchanged
    "ended session is no longer live"
    ApplicationSessionNotLive
    afterSecondEnd
    ( prepareApplicationSessionResume
        secondSession
        secondToken
        secondCursor
        afterSecondEnd
    )

caseBindingLoss :: Assertion
caseBindingLoss = do
  let fixture = sessionFixture
      (afterOpen, accepted) =
        open fixture.herald fixture.firstAttachment 41 fixture.state
      (session, token, startup) = opened accepted
      binding = sessionAcceptanceBinding accepted
      lastObserved = sessionAcceptanceCursor accepted
      afterLoss =
        commitApplicationBindingLoss
          (prepareApplicationBindingLoss fixtureApplicationRecoveryConfiguration (monotonicInstant 0) binding afterOpen)
      (_, resumed) = resume session token lastObserved afterLoss
  assertBool "matching loss changes only delivery reachability" (afterLoss /= afterOpen)
  case sessionAcceptanceReply resumed of
    SessionResumed resumedSession _ -> assertEqual "resume retains session" session resumedSession
    reply -> assertFailure ("expected resume acknowledgement, got " <> show reply)
  assertEqual
    "loss and resume preserve the retained startup bundle"
    startup
    (startupFor session afterLoss)

caseStaleBindingLoss :: Assertion
caseStaleBindingLoss = do
  let fixture = sessionFixture
      (afterOpen, accepted) =
        open fixture.herald fixture.firstAttachment 43 fixture.state
      (session, token, _) = opened accepted
      originalBinding = sessionAcceptanceBinding accepted
      originalCursor = sessionAcceptanceCursor accepted
      afterLoss =
        commitApplicationBindingLoss
          (prepareApplicationBindingLoss fixtureApplicationRecoveryConfiguration (monotonicInstant 0) originalBinding afterOpen)
      afterDuplicate =
        commitApplicationBindingLoss
          (prepareApplicationBindingLoss fixtureApplicationRecoveryConfiguration (monotonicInstant 0) originalBinding afterLoss)
      (afterResume, _) = resume session token originalCursor afterLoss
      afterStale =
        commitApplicationBindingLoss
          (prepareApplicationBindingLoss fixtureApplicationRecoveryConfiguration (monotonicInstant 0) originalBinding afterResume)
  assertBool "duplicate loss is a no-op" (afterDuplicate == afterLoss)
  assertBool "old-generation loss after resume is a no-op" (afterStale == afterResume)

caseEndIsolation :: Assertion
caseEndIsolation = do
  let fixture = sessionFixture
      (afterFirst, first) =
        open fixture.herald fixture.firstAttachment 51 fixture.state
      (afterSecond, second) =
        open fixture.herald fixture.firstAttachment 52 afterFirst
      (firstSession, _, _) = opened first
      internalValue =
        Domain.globalUniqueIdValue
          ( checkedPure
              "later shared global identity"
              (mkGlobalUniqueId (ByteString.replicate 32 0xde))
          )
      preparedLocalization =
        checkedPure
          "later process-scoped localization"
          ( prepareOrdinaryApplicationValueLocalization
              (processRoleView [])
              fixture.firstProcess
              internalValue
              afterSecond
          )
      (afterLocalization, localized) =
        commitOrdinaryApplicationValueLocalization preparedLocalization
      afterEnd =
        end firstSession (sessionAcceptanceBinding first) afterLocalization
      (_, retriedSecond) =
        open fixture.herald fixture.firstAttachment 52 afterEnd
  assertEqual
    "the other live session remains the exact nonce retry"
    second
    retriedSecond
  assertBool
    "ending one session preserves the process identity map"
    (applicationPrivateIdentity afterLocalization == applicationPrivateIdentity afterEnd)
  assertEqual
    "the other session uses the process localization installed after both opens"
    (Right internalValue)
    ( globalizeOrdinaryApplicationValue
        (processRoleView [])
        fixture.firstProcess
        localized
        afterEnd
    )

data SessionFixture = SessionFixture
  { herald :: HeraldEpoch,
    state :: State,
    firstProcess :: ProcessEpochId,
    secondProcess :: ProcessEpochId,
    firstAttachment :: ApplicationAttachment,
    secondAttachment :: ApplicationAttachment
  }

sessionFixture :: SessionFixture
sessionFixture =
  case bootstrapped (checkedBootstraps fixtureLocalBootstrapIds) of
    (first : second : _, state) ->
      SessionFixture
        { herald = checkedLocalHeraldEpoch fixtureCheckedGenesis,
          state = state,
          firstProcess = appliedProcessEpochId first,
          secondProcess = appliedProcessEpochId second,
          firstAttachment =
            attachmentFor (appliedBootstrapManifestId first),
          secondAttachment =
            attachmentFor (appliedBootstrapManifestId second)
        }
    _ -> error "session fixture has fewer than two resident bootstraps"
  where
    attachmentFor manifest =
      applicationAttachmentForBootstrap manifest

checkedBootstraps :: [BootstrapManifestId] -> CheckedInitialBootstraps
checkedBootstraps manifests =
  checkedPure
    "checked initial bootstraps"
    ( checkInitialBootstraps
        fixtureCheckedGenesis
        (PrimordialProcessManifest manifests)
    )

bootstrapped :: CheckedInitialBootstraps -> ([AppliedProcessBootstrap], State)
bootstrapped checkedInitial =
  (resident, bootstrapApplicationState owners)
  where
    resident =
      filter
        (\bootstrap -> appliedBootstrapManifestId bootstrap `elem` fixtureLocalBootstrapIds)
        (checkedInitialBootstraps checkedInitial)
    oracle = OracleProjection.initialState fixtureCheckedGenesis checkedInitial
    sorts = SortRegistry.initialState fixtureCheckedGenesis
    views =
      bootstrapViews
        (OracleProjection.oracleView oracle)
        (SortRegistry.sortView sorts)
    initialOwners =
      checkedPure
        "initial bootstrap owners"
        (initialBootstrapOwners fixtureCheckedGenesis)
    owners = checkedPure "resident process bootstrap" (foldM (install views) initialOwners resident)

install ::
  BootstrapViews ->
  BootstrapOwners ->
  AppliedProcessBootstrap ->
  Either BootstrapInvariant BootstrapOwners
install views owners bootstrap = do
  prepared <- prepareProcessBootstrap views bootstrap owners
  Right (fst (commitProcessBootstrap prepared))

open ::
  HeraldEpoch ->
  ApplicationAttachment ->
  Word64 ->
  State ->
  (State, ApplicationSessionAcceptance)
open herald attachment nonceWord =
  openWithNonce herald attachment nonceWord

openWithNonce ::
  HeraldEpoch ->
  ApplicationAttachment ->
  Word64 ->
  State ->
  (State, ApplicationSessionAcceptance)
openWithNonce herald attachment nonceWord state =
  commitApplicationSessionAcceptance
    ( checkedPure
        "prepare application session open"
        ( prepareApplicationSessionOpen
            herald
            attachment
            (clientNonce nonceWord)
            state
        )
    )

resume ::
  ApplicationSessionId ->
  ApplicationResumeToken ->
  ApplicationReplyCursor ->
  State ->
  (State, ApplicationSessionAcceptance)
resume session token lastObserved state =
  commitApplicationSessionAcceptance
    ( checkedPure
        "prepare application session resume"
        (prepareApplicationSessionResume session token lastObserved state)
    )

end ::
  ApplicationSessionId ->
  ApplicationSessionBinding ->
  State ->
  State
end session binding state =
  commitApplicationSessionEnd
    ( checkedPure
        "prepare application session end"
        (prepareApplicationSessionEnd session binding state)
    )

opened ::
  ApplicationSessionAcceptance ->
  (ApplicationSessionId, ApplicationResumeToken, ApplicationStartupAccess)
opened acceptance = case sessionAcceptanceReply acceptance of
  SessionOpened session token startup -> (session, token, startup)
  reply -> error ("expected opened acknowledgement, got " <> show reply)

bootstrapAccess :: ProcessEpochId -> State -> BootstrapAccess
bootstrapAccess process state =
  case applicationBootstrapAccess process state of
    Just access -> access
    Nothing -> error "session fixture is missing bootstrap access"

startupFor :: ApplicationSessionId -> State -> ApplicationStartupAccess
startupFor session state =
  case find ((== session) . applicationSessionWitnessId) witnesses of
    Just witness -> applicationSessionWitnessStartupAccess witness
    Nothing -> error "session witness is missing"
  where
    witnesses =
      applicationWitnessSessions (applicationStateWitness state)

assertRejectedUnchanged ::
  String ->
  ApplicationSessionRejection ->
  State ->
  Either ApplicationSessionTransitionError prepared ->
  Assertion
assertRejectedUnchanged description expected _predecessor result = case result of
  Left (ApplicationSessionRejected actual) ->
    assertEqual description expected actual
  Left problem -> assertFailure (description <> ": wrong fault " <> show problem)
  Right _ -> assertFailure (description <> ": transition unexpectedly succeeded")

checkedPure :: (Show problem) => String -> Either problem value -> value
checkedPure context = either (error . ((context <> ": ") <>) . show) id
