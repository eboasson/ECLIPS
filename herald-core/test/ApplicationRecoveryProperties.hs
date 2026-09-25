{-# LANGUAGE OverloadedRecordDot #-}

module ApplicationRecoveryProperties (tests) where

import Data.Word (Word64)
import Eclips.Application.Types.Lifetime (applicationReceiptRetirement, emptyApplicationReceiptRetirement)
import Eclips.Application.Types.Operation (ApplicationOperation (NewEnvironmentApplication))
import Eclips.Application.Types.Rejection (ApplicationRejection (ApplicationOperateNotPermitted))
import Eclips.Domain.Identity (BootstrapManifestId, ProcessEpochId)
import Eclips.Herald.Application.Recovery
  ( ApplicationRecoveryConfigurationError (ApplicationRecoveryGraceZero),
    applicationProcessRecoveryGenerationWord64,
    applicationRecoveryGraceMicroseconds,
    applicationSessionRecoveryGenerationWord64,
    checkApplicationRecoveryConfiguration,
  )
import Eclips.Herald.Application.Recovery.Internal
  ( firstApplicationProcessRecoveryGeneration,
    firstApplicationSessionRecoveryGeneration,
    nextApplicationProcessRecoveryGeneration,
    nextApplicationSessionRecoveryGeneration,
  )
import Eclips.Herald.Application.Request (ApplicationReplyCursor, ApplicationRequestReply (..), requestId)
import Eclips.Herald.Application.Session
  ( ApplicationAttachment,
    ApplicationResumeToken,
    ApplicationSessionBinding,
    ApplicationSessionId,
    ApplicationSessionRejection (ApplicationAttachmentNotAdmitted, ApplicationSessionNotLive),
    ApplicationSessionReply (SessionOpened),
    ApplicationSessionUnavailableReason (ApplicationSessionNoLongerLive),
    primordialApplicationAttachment,
    sessionAcceptanceBinding,
    sessionAcceptanceCursor,
    sessionAcceptanceReply,
  )
import Eclips.Herald.Application.Session.Internal qualified as Session
import Eclips.Herald.Application.State qualified as Application
import Eclips.Herald.EffectBatch
  ( ApplicationDispositionTarget (CandidateApplicationDisposition),
    EffectBatch,
    HeraldEffect (..),
    effectBatchIsEmpty,
    effectBatchMembers,
  )
import Eclips.Herald.Genesis (CheckedInitialBootstraps, PrimordialProcessManifest (..), checkInitialBootstraps)
import Eclips.Herald.Initialization (initialHerald)
import Eclips.Herald.Input
  ( ApplicationReceiptRetirementIngress (..),
    ApplicationRetirementWork (..),
    ApplicationSessionIngress (..),
    HeraldInputBody (..),
    RuntimeObservation (..),
    candidateApplicationLane,
    heraldInput,
  )
import Eclips.Herald.OracleClient.State qualified as OracleClient
import Eclips.Herald.Startup.Invariant (validateHeraldState)
import Eclips.Herald.Startup.State
  ( HeraldState,
    replaceStartupApplicationState,
    replaceStartupLastObservedTime,
    startupApplicationState,
    startupLastObservedTime,
    startupOracleClientState,
  )
import Eclips.Herald.Time (monotonicInstant)
import Eclips.Herald.Transition (stepHerald)
import GenesisFixtures
  ( fixtureApplicationRecoveryConfiguration,
    fixtureCheckedGenesis,
    fixtureGeneratorSeed,
    fixtureLocalBootstrapIds,
    fixtureOracleContacts,
    fixturePeerRecoveryConfiguration,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (Positive (Positive), Property, counterexample, testProperty)

tests :: TestTree
tests =
  testGroup
    "application connection lifetime"
    [ testCase "piggyback commits retirement before a late retry and standalone confirms empty progress" caseRetirementIngress,
      testCase "retired reply cursor remains valid after its final receipt is removed" caseRetiredCursorInvariant,
      testCase "current binding loss immediately removes the session and ends its final process once" caseImmediateLoss,
      testCase "a replaced binding loss is stale" caseStaleBindingLoss,
      testCase "one sibling loss keeps the process live until the final sibling is lost" caseSiblingLoss,
      testCase "a dead session cannot resume and a sealed process cannot reopen" caseNoRecovery,
      testCase "a never-attached process is not inferred dead" caseNeverAttached,
      testCase "orderly final session end does not imply process death" caseOrderlyEnd,
      testProperty "legacy recovery configuration remains structurally checked" propCheckedPositiveGrace,
      testProperty "session generation identities remain monotonic" propSessionGeneration,
      testProperty "process generation identities remain monotonic" propProcessGeneration
    ]

caseRetirementIngress :: Assertion
caseRetirementIngress = do
  let opened = openedFixture 0 8 80
      progress = applicationReceiptRetirement (Just 0) Nothing
      (confirmed, confirmation) = stepBody 2 (ApplicationReceiptRetirementInput (RetireApplicationReceipts opened.binding opened.session emptyApplicationReceiptRetirement Nothing)) opened.state
      (retired, effects) = stepBody 3 (ApplicationReceiptRetirementInput (RetireApplicationReceipts opened.binding opened.session progress (Just (RetiringApplicationRequest (requestId 0) NewEnvironmentApplication)))) confirmed
  assertEqual
    "same-socket startup has an explicit confirmation"
    [SendApplicationReply opened.binding (ApplicationReceiptsRetired emptyApplicationReceiptRetirement)]
    (effectBatchMembers confirmation)
  assertEqual
    "retirement precedes the typed late-retry result"
    [SendApplicationReply opened.binding (ApplicationReceiptsRetired progress), SendApplicationReply opened.binding (RequestRetired (requestId 0) 0)]
    (effectBatchMembers effects)
  assertEqual
    "sealed request cannot execute"
    []
    (concatMap Application.applicationRequestOwnerRequests (Application.applicationRequestOwnerWitnesses (Application.applicationRequestStateWitness (startupApplicationState retired))))
  assertEqual "the composed successor validates" (Right ()) (validateHeraldState retired)

caseRetiredCursorInvariant :: Assertion
caseRetiredCursorInvariant = do
  let opened = openedFixture 0 9 90
      application = startupApplicationState opened.state
      candidate = case Application.classifyApplicationRequest opened.session opened.binding (requestId 0) NewEnvironmentApplication application of
        Right (Application.FirstApplicationRequest result) -> result
        _ -> error "expected fresh rejection candidate"
      (rejected, _) =
        Application.commitApplicationRequest
          (Application.prepareApplicationRequestRejection candidate ApplicationOperateNotPermitted)
      retired =
        Application.commitApplicationReceiptRetirement
          ( checked
              "retire final retained cursor"
              ( Application.prepareApplicationReceiptRetirement
                  opened.binding
                  opened.session
                  (applicationReceiptRetirement (Just 0) Nothing)
                  rejected
              )
          )
  assertEqual
    "full owner invariant accepts a discarded last reply"
    (Right ())
    (validateHeraldState (replaceStartupApplicationState retired opened.state))

caseImmediateLoss :: Assertion
caseImmediateLoss = do
  let opened = openedFixture 0 10 100
      process = sessionProcess opened.session opened.state
      (lost, effects) = stepBody 2 (RuntimeObserved (ApplicationBindingLost opened.binding)) opened.state
      (again, duplicateEffects) = stepBody 3 (RuntimeObserved (ApplicationBindingLost opened.binding)) lost
  assertEqual "session ownership is gone at loss observation" [] (sessions lost)
  assertEqual "process seals immediately" [process] (sealedProcesses lost)
  assertEqual "one automatic process End is retained" 1 (length (processEndIntents lost))
  assertEqual "the lost session is disposed" [opened.session] (disposedSessions effects)
  assertEqual "no recovery timer exists" [] (Application.applicationSessionRecoveryEntries (startupApplicationState lost))
  assertBool "no recovery timer is armed" (all (\case ArmTimer {} -> False; _ -> True) (effectBatchMembers effects))
  assertBool "duplicate loss emits no effects" (effectBatchIsEmpty duplicateEffects)
  assertOnlyTimeAdvanced lost again

caseStaleBindingLoss :: Assertion
caseStaleBindingLoss = do
  let opened = openedFixture 0 20 200
      (resumed, _) =
        stepBody
          2
          (ApplicationSessionInput (ResumeApplicationSession (candidateApplicationLane 201) opened.session opened.token opened.cursor))
          opened.state
      (observed, effects) = stepBody 3 (RuntimeObserved (ApplicationBindingLost opened.binding)) resumed
  assertBool "old physical binding loss does not kill replacement" (effectBatchIsEmpty effects)
  assertOnlyTimeAdvanced resumed observed
  assertEqual "replacement session survives" [opened.session] (fmap sessionId (sessions observed))

caseSiblingLoss :: Assertion
caseSiblingLoss = do
  let first = openedFixture 0 30 300
      second = openSession 2 301 31 first.attachment first.state
      process = sessionProcess first.session second.state
      (oneLost, effects) = stepBody 3 (RuntimeObserved (ApplicationBindingLost first.binding)) second.state
      (bothLost, lastEffects) = stepBody 4 (RuntimeObserved (ApplicationBindingLost second.binding)) oneLost
  assertEqual "only the lost sibling is disposed" [first.session] (disposedSessions effects)
  assertEqual "the attached sibling remains" [second.session] (fmap sessionId (sessions oneLost))
  assertEqual "one attached sibling prevents process sealing" [] (sealedProcesses oneLost)
  assertEqual "one attached sibling prevents automatic End" [] (processEndIntents oneLost)
  assertEqual "final loss disposes its own session" [second.session] (disposedSessions lastEffects)
  assertEqual "final loss seals once" [process] (sealedProcesses bothLost)
  assertEqual "final loss retains one End" 1 (length (processEndIntents bothLost))

caseNoRecovery :: Assertion
caseNoRecovery = do
  let opened = openedFixture 0 40 400
      (lost, _) = stepBody 2 (RuntimeObserved (ApplicationBindingLost opened.binding)) opened.state
      resumeLane = candidateApplicationLane 401
      openLane = candidateApplicationLane 402
      (afterResume, resumeEffects) =
        stepBody
          3
          (ApplicationSessionInput (ResumeApplicationSession resumeLane opened.session opened.token opened.cursor))
          lost
      (_, openEffects) =
        stepBody
          4
          (ApplicationSessionInput (OpenApplicationSession openLane opened.attachment (Session.clientNonce 41)))
          afterResume
  assertEqual
    "Resume rejects an expired session immediately"
    [RejectApplicationConnection (CandidateApplicationDisposition resumeLane) ApplicationSessionNotLive]
    (effectBatchMembers resumeEffects)
  assertEqual
    "fresh Open cannot resurrect a sealed process"
    [RejectApplicationConnection (CandidateApplicationDisposition openLane) ApplicationAttachmentNotAdmitted]
    (effectBatchMembers openEffects)

caseNeverAttached :: Assertion
caseNeverAttached = do
  let opened = openedFixture 0 50 500
      initial = initialFixture 0
      (successor, effects) = stepBody 1 (RuntimeObserved (ApplicationBindingLost opened.binding)) initial
  assertBool "unknown loss is inert" (effectBatchIsEmpty effects)
  assertEqual "never-attached process has no automatic End" [] (processEndIntents successor)
  assertOnlyTimeAdvanced initial successor

caseOrderlyEnd :: Assertion
caseOrderlyEnd = do
  let opened = openedFixture 0 60 600
      (ended, effects) = stepBody 2 (ApplicationSessionInput (EndApplicationSession opened.binding opened.session)) opened.state
  assertEqual "orderly end forgets the session" [] (sessions ended)
  assertEqual "orderly end does not seal the process" [] (sealedProcesses ended)
  assertEqual "orderly end has no automatic process End" [] (processEndIntents ended)
  assertBool "orderly EndSession is a silent kernel transition" (effectBatchIsEmpty effects)

propCheckedPositiveGrace :: Positive Word64 -> Property
propCheckedPositiveGrace (Positive grace) =
  counterexample ("positive grace: " <> show grace)
    $ checkApplicationRecoveryConfiguration 0 == Left ApplicationRecoveryGraceZero
      && fmap applicationRecoveryGraceMicroseconds (checkApplicationRecoveryConfiguration grace)
        == Right grace

propSessionGeneration :: Positive Word64 -> Property
propSessionGeneration (Positive advances) =
  let generation =
        iterate nextApplicationSessionRecoveryGeneration firstApplicationSessionRecoveryGeneration
          !! fromIntegral (advances `mod` 10_000)
   in counterexample ("advances: " <> show advances)
        $ applicationSessionRecoveryGenerationWord64 generation
          == 1 + advances `mod` 10_000

propProcessGeneration :: Positive Word64 -> Property
propProcessGeneration (Positive advances) =
  let generation =
        iterate nextApplicationProcessRecoveryGeneration firstApplicationProcessRecoveryGeneration
          !! fromIntegral (advances `mod` 10_000)
   in counterexample ("advances: " <> show advances)
        $ applicationProcessRecoveryGenerationWord64 generation
          == 1 + advances `mod` 10_000

data Opened = Opened
  { state :: HeraldState,
    attachment :: ApplicationAttachment,
    session :: ApplicationSessionId,
    token :: ApplicationResumeToken,
    binding :: ApplicationSessionBinding,
    cursor :: ApplicationReplyCursor
  }

openedFixture :: Word64 -> Word64 -> Word64 -> Opened
openedFixture initialObserved nonce lane =
  openSession
    (initialObserved + 1)
    lane
    nonce
    attachment
    (initialFixture initialObserved)
  where
    attachment =
      checkedMaybe
        "primordial application attachment"
        ( primordialApplicationAttachment
            fixtureCheckedGenesis
            checkedBootstraps
            firstLocalBootstrapId
        )

initialFixture :: Word64 -> HeraldState
initialFixture initialObserved =
  fst
    ( checked
        "initial Herald"
        ( initialHerald
            (monotonicInstant initialObserved)
            fixtureCheckedGenesis
            checkedBootstraps
            fixtureOracleContacts
            fixtureGeneratorSeed
            fixtureApplicationRecoveryConfiguration
            fixturePeerRecoveryConfiguration
        )
    )

openSession :: Word64 -> Word64 -> Word64 -> ApplicationAttachment -> HeraldState -> Opened
openSession observed lane nonce attachment predecessor =
  case effectBatchMembers effects of
    [SetApplicationConnectionDisposition actualLane acceptance]
      | actualLane == candidate -> opened acceptance
    actual -> error ("unexpected application-open effects: " <> show actual)
  where
    candidate = candidateApplicationLane lane
    (successor, effects) =
      stepBody
        observed
        ( ApplicationSessionInput
            (OpenApplicationSession candidate attachment (Session.clientNonce nonce))
        )
        predecessor
    opened acceptance = case sessionAcceptanceReply acceptance of
      SessionOpened session token _ ->
        Opened
          { state = successor,
            attachment,
            session,
            token,
            binding = sessionAcceptanceBinding acceptance,
            cursor = sessionAcceptanceCursor acceptance
          }
      reply -> error ("application open returned a non-open reply: " <> show reply)

checkedBootstraps :: CheckedInitialBootstraps
checkedBootstraps =
  checked
    "checked initial bootstraps"
    ( checkInitialBootstraps
        fixtureCheckedGenesis
        (PrimordialProcessManifest fixtureLocalBootstrapIds)
    )

firstLocalBootstrapId :: BootstrapManifestId
firstLocalBootstrapId = case fixtureLocalBootstrapIds of
  first : _ -> first
  [] -> error "fixture has no local bootstrap"

stepBody :: Word64 -> HeraldInputBody -> HeraldState -> (HeraldState, EffectBatch)
stepBody observed body predecessor =
  checked "Herald transition" $ do
    result@(successor, _) <-
      stepHerald (heraldInput (monotonicInstant observed) body) predecessor
    validateHeraldState successor
    Right result

sessions :: HeraldState -> [Application.ApplicationSessionWitness]
sessions =
  Application.applicationWitnessSessions
    . Application.applicationStateWitness
    . startupApplicationState

sessionId :: Application.ApplicationSessionWitness -> ApplicationSessionId
sessionId = Application.applicationSessionWitnessId

sessionProcess :: ApplicationSessionId -> HeraldState -> ProcessEpochId
sessionProcess session state =
  checkedMaybe
    "application session process"
    (Application.applicationSessionProcess session (startupApplicationState state))

sealedProcesses :: HeraldState -> [ProcessEpochId]
sealedProcesses state =
  [ process
  | Application.ApplicationProcessRecoveryWitness
      process
      _
      (Application.ApplicationProcessSealedWitness _) <-
      Application.applicationProcessRecoveryEntries
        (startupApplicationState state)
  ]

processEndIntents :: HeraldState -> [OracleClient.OracleSemanticIntent]
processEndIntents state =
  [ intent
  | (_, witness) <-
      OracleClient.oracleClientRequestEntries (startupOracleClientState state),
    let intent = OracleClient.oracleRequestWitnessIntent witness,
    OracleClient.ProcessEndIntent {} <- [intent]
  ]

disposedSessions :: EffectBatch -> [ApplicationSessionId]
disposedSessions effects =
  [ session
  | DisposeApplicationSession session ApplicationSessionNoLongerLive <- effectBatchMembers effects
  ]

assertOnlyTimeAdvanced :: HeraldState -> HeraldState -> Assertion
assertOnlyTimeAdvanced predecessor successor = do
  assertBool "the observation time advances" (startupLastObservedTime successor > startupLastObservedTime predecessor)
  assertBool
    "only the observation time changes"
    ( predecessor
        == replaceStartupLastObservedTime
          (startupLastObservedTime predecessor)
          successor
    )

checked :: (Show problem) => String -> Either problem value -> value
checked context = either (error . ((context <> ": ") <>) . show) id

checkedMaybe :: String -> Maybe value -> value
checkedMaybe context = maybe (error (context <> ": missing")) id
